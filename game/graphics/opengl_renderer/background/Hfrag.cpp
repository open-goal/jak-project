#include "Hfrag.h"

#include "common/log/log.h"

#include "third-party/imgui/imgui.h"

Hfrag::Hfrag(const std::string& name, int my_id) : BucketRenderer(name, my_id) {
  // generate shared index buffer
  int vi = 0;
  std::vector<u32> indices;
  for (int bucket_idx = 0; bucket_idx < kNumBuckets; bucket_idx++) {
    for (int tile_idx = 0; tile_idx < kNumMontageTiles; tile_idx++) {
      indices.push_back(vi++);
      indices.push_back(vi++);
      indices.push_back(vi++);
      indices.push_back(vi++);
      indices.push_back(UINT32_MAX);
    }
  }

  glGenBuffers(1, &m_montage_indices);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_montage_indices);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(u32) * indices.size(), indices.data(),
               GL_STATIC_DRAW);
  ASSERT(indices.size() == kIndsPerTile * kNumMontageTiles * kNumBuckets);
}

void Hfrag::draw_debug_window() {
  for (auto& level : m_levels) {
    if (!level.in_use) {
      ImGui::Text("Inactive");
    } else {
      ImGui::Text("Level %s", level.name.c_str());
      ImGui::Text(" total corners:   %d", level.stats.total_corners);
      ImGui::Text(" in view corners: %d", level.stats.corners_in_view);
      ImGui::Text(" in view and not occluded corners: %d",
                  level.stats.corners_in_view_and_not_occluded);
      ImGui::Text(" buckets used: %d", level.stats.buckets_used);
    }
  }
}

void Hfrag::init_shaders(ShaderLibrary&) {}

bool is_dma_nop(const DmaTransfer& xfer) {
  return xfer.size_bytes == 0 && xfer.vifcode0().kind == VifCode::Kind::NOP &&
         xfer.vifcode1().kind == VifCode::Kind::NOP;
}

bool is_dma_init_nop(const DmaTransfer& xfer) {
  return xfer.size_bytes == 0 &&
         (xfer.vifcode0().kind == VifCode::Kind::NOP ||
          xfer.vifcode0().kind == VifCode::Kind::MARK) &&
         xfer.vifcode1().kind == VifCode::Kind::NOP;
}

void Hfrag::render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) {
  // dma.read_and_advance();  // link to bucket data
  {
    auto nop1 = dma.read_and_advance();
    ASSERT(is_dma_init_nop(nop1));
    if (dma.current_tag_offset() == render_state->next_bucket) {
      return;
    }
  }

  // first transfer is always FLUSHA, DIRECT to set up the bucket
  {
    auto default_init = dma.read_and_advance();
    ASSERT(default_init.size_bytes == 10 * 16);
    ASSERT(default_init.vifcode0().kind == VifCode::Kind::FLUSHA);
    ASSERT(default_init.vifcode1().kind == VifCode::Kind::DIRECT);
  }

  {
    auto first_nop = dma.read_and_advance();
    ASSERT(is_dma_nop(first_nop));
  }

  auto xfer = dma.read_and_advance();
  while (!is_dma_nop(xfer)) {
    // grab the data
    ASSERT(xfer.size_bytes == sizeof(TfragPcPortData));
    TfragPcPortData pc_port_data;
    int level_idx = xfer.vifcode1().immediate;
    memcpy(&pc_port_data, xfer.data, sizeof(TfragPcPortData));
    pc_port_data.level_name[11] = '\0';

    const u8* occlusion_data = nullptr;
    if (render_state->occlusion_vis[level_idx].valid) {
      occlusion_data = render_state->occlusion_vis[level_idx].data;
    }

    // ugh
    if (std::string("desert-vis") == pc_port_data.level_name) {
      strcpy(pc_port_data.level_name, "dst");
    }

    // try to get a hfrag level
    auto* hfrag_level = get_hfrag_level(pc_port_data.level_name, render_state);
    if (hfrag_level) {
      hfrag_level->last_used_frame = render_state->frame_idx;
      if (occlusion_data) {
        occlusion_data += hfrag_level->hfrag->occlusion_offset;
      }
      render_hfrag_level(hfrag_level, render_state, prof, pc_port_data, occlusion_data);
    }

    xfer = dma.read_and_advance();
  }

  // ending
  {
    auto default_init = dma.read_and_advance();
    ASSERT(default_init.size_bytes == 10 * 16);
    ASSERT(default_init.vifcode0().kind == VifCode::Kind::FLUSHA);
    ASSERT(default_init.vifcode1().kind == VifCode::Kind::DIRECT);
  }

  {
    auto end_nop = dma.read_and_advance();
    ASSERT(is_dma_nop(end_nop));
  }
  ASSERT(render_state->next_bucket == dma.current_tag_offset());
}

Hfrag::HfragLevel* Hfrag::get_hfrag_level(const std::string& name,
                                          SharedRenderState* render_state) {
  // first, see if this level is loaded. If not, there's nothing we can do.
  auto lev_data = render_state->loader->get_tfrag3_level(name);
  if (!lev_data) {
    // printf("[hfrag] can't display %s because it's not loaded.\n", name.c_str());
    // render_state->loader->debug_print_loaded_levels();
    return nullptr;
  }

  // check if the level load work is already done and we can reuse.
  for (auto& lev : m_levels) {
    if (lev.in_use && lev.name == name && lev.load_id == lev_data->load_id) {
      // we can reuse it!
      return &lev;
    }
  }

  // prefer to load over a stale copy of this same level
  for (auto& lev : m_levels) {
    if (lev.in_use && lev.name == name) {
      // unload the previous copy and reload
      unload_hfrag_level(&lev);
      load_hfrag_level(name, &lev, lev_data);
      return &lev;
    }
  }

  // prefer to load in an unused slot
  for (auto& lev : m_levels) {
    if (!lev.in_use) {
      load_hfrag_level(name, &lev, lev_data);
      return &lev;
    }
  }

  // find the least recently used level
  u64 oldest_frame = UINT64_MAX;
  HfragLevel* oldest_lev = nullptr;
  for (auto& lev : m_levels) {
    if (lev.in_use) {
      if (lev.last_used_frame < oldest_frame) {
        lg::warn("hfrag discarding level {} for level {}, age {} frames", lev.name, name,
                 render_state->frame_idx - lev.last_used_frame);
        oldest_frame = lev.last_used_frame;
        oldest_lev = &lev;
      }
    }
  }

  ASSERT(oldest_lev);
  unload_hfrag_level(oldest_lev);
  load_hfrag_level(name, oldest_lev, lev_data);
  return oldest_lev;
}

void Hfrag::unload_hfrag_level(Hfrag::HfragLevel* lev) {
  ASSERT(lev->in_use);
  // delete OpenGL resources we created.
  glBindTexture(GL_TEXTURE_1D, lev->time_of_day_texture);
  glDeleteTextures(1, &lev->time_of_day_texture);
  glDeleteVertexArrays(1, &lev->vao);

  glDeleteVertexArrays(1, &lev->montage_vao);
  glDeleteBuffers(1, &lev->montage_vertices);

  lev->in_use = false;
  lev->name.clear();
  lev->last_used_frame = 0;
  lev->hfrag = nullptr;
}

void Hfrag::load_hfrag_level(const std::string& load_name,
                             Hfrag::HfragLevel* lev,
                             const LevelData* data) {
  ASSERT(!lev->in_use);

  lev->in_use = true;
  lev->name = load_name;
  lev->load_id = data->load_id;
  lev->vertex_buffer = data->hfrag_vertices;
  lev->index_buffer = data->hfrag_indices;
  lev->num_colors = data->level->hfrag.time_of_day_colors.color_count;
  lev->hfrag = &data->level->hfrag;
  lev->wang_texture = data->textures.at(data->level->hfrag.wang_tree_tex_id[0]);

  if (m_color_result.size() < lev->num_colors) {
    m_color_result.resize(lev->num_colors);
  }

  ASSERT(lev->hfrag->buckets.size() == kNumBuckets);
  ASSERT(lev->hfrag->corners.size() == kNumCorners);
  ASSERT(lev->num_colors <= TIME_OF_DAY_COLOR_COUNT);

  // normal drawing opengl
  glGenVertexArrays(1, &lev->vao);
  glBindVertexArray(lev->vao);
  glBindBuffer(GL_ARRAY_BUFFER, lev->vertex_buffer);
  glEnableVertexAttribArray(0);
  glEnableVertexAttribArray(1);
  glEnableVertexAttribArray(2);
  glEnableVertexAttribArray(3);
  glEnableVertexAttribArray(4);

  glVertexAttribPointer(0,                                // location 0 in the shader
                        3,                                // 3 values per vert
                        GL_FLOAT,                         // floats
                        GL_FALSE,                         // normalized (ignored)
                        sizeof(tfrag3::HfragmentVertex),  // stride
                        (void*)offsetof(tfrag3::HfragmentVertex, height)  // offset
  );

  glVertexAttribIPointer(1,                                // location 1 in the shader
                         1,                                // 1 values per vert
                         GL_UNSIGNED_SHORT,                // u16
                         sizeof(tfrag3::HfragmentVertex),  // stride
                         (void*)offsetof(tfrag3::HfragmentVertex, color_index)  // offset (0)
  );
  glVertexAttribIPointer(2,                                           // location 1 in the shader
                         2,                                           // 2 values per vert
                         GL_UNSIGNED_BYTE,                            // u8
                         sizeof(tfrag3::HfragmentVertex),             // stride
                         (void*)offsetof(tfrag3::HfragmentVertex, u)  // offset (0)
  );
  glVertexAttribIPointer(3,                                            // location 1 in the shader
                         1,                                            // 2 values per vert
                         GL_UNSIGNED_INT,                              // u32
                         sizeof(tfrag3::HfragmentVertex),              // stride
                         (void*)offsetof(tfrag3::HfragmentVertex, vi)  // offset (0)
  );
  glActiveTexture(GL_TEXTURE10);
  glGenTextures(1, &lev->time_of_day_texture);
  glBindTexture(GL_TEXTURE_1D, lev->time_of_day_texture);
  glTexImage1D(GL_TEXTURE_1D, 0, GL_RGBA, TIME_OF_DAY_COLOR_COUNT, 0, GL_RGBA,
               GL_UNSIGNED_INT_8_8_8_8, nullptr);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glBindVertexArray(0);

  // montage

  struct MontageVertex {
    float x, y, u, v;
  };
  struct MontageTile {
    MontageVertex verts[4];
  };
  struct MontageBucket {
    MontageTile tiles[kNumMontageTiles];
  };
  MontageBucket montage[kNumBuckets];

  // generate montage vertices
  for (int bucket_idx = 0; bucket_idx < kNumBuckets; bucket_idx++) {
    auto& bucket = montage[bucket_idx];
    for (int tile_idx = 0; tile_idx < kNumMontageTiles; tile_idx++) {
      auto& tile = bucket.tiles[tile_idx];
      // the xy is the same for all buckets
      const int tx = tile_idx % 4;
      const int ty = tile_idx / 4;

      tile.verts[0].x = tx * 0.25f;
      tile.verts[1].x = tx * 0.25f + 0.25f;
      tile.verts[2].x = tx * 0.25f;
      tile.verts[3].x = tx * 0.25f + 0.25f;

      tile.verts[0].y = ty * 0.25f;
      tile.verts[1].y = ty * 0.25f;
      tile.verts[2].y = ty * 0.25f + 0.25f;
      tile.verts[3].y = ty * 0.25f + 0.25f;

      // use the lookup table from the game
      const int montage_idx = lev->hfrag->buckets[bucket_idx].montage_table[tile_idx];
      const int mx = montage_idx & 15;
      const int my = montage_idx / 16;
      ASSERT(montage_idx < 8 * 16);

      tile.verts[0].u = mx * 0.0625f;
      tile.verts[1].u = mx * 0.0625f + 0.0625f;
      tile.verts[2].u = mx * 0.0625f;
      tile.verts[3].u = mx * 0.0625f + 0.0625f;

      tile.verts[0].v = my * 0.125f;
      tile.verts[1].v = my * 0.125f;
      tile.verts[2].v = my * 0.125f + 0.125f;
      tile.verts[3].v = my * 0.125f + 0.125f;
    }
  }

  glGenVertexArrays(1, &lev->montage_vao);
  glBindVertexArray(lev->montage_vao);
  glGenBuffers(1, &lev->montage_vertices);
  glBindBuffer(GL_ARRAY_BUFFER, lev->montage_vertices);
  glBufferData(GL_ARRAY_BUFFER, sizeof(MontageBucket) * kNumBuckets, montage, GL_STATIC_DRAW);
  glActiveTexture(GL_TEXTURE0);

  glEnableVertexAttribArray(0);
  glEnableVertexAttribArray(1);

  glVertexAttribPointer(0,                                 // location 0 in the shader
                        2,                                 // 3 values per vert
                        GL_FLOAT,                          // floats
                        GL_FALSE,                          // normalized (ignored)
                        sizeof(MontageVertex),             // stride
                        (void*)offsetof(MontageVertex, x)  // offset
  );
  glVertexAttribPointer(1,                                 // location 0 in the shader
                        2,                                 // 3 values per vert
                        GL_FLOAT,                          // floats
                        GL_FALSE,                          // normalized (ignored)
                        sizeof(MontageVertex),             // stride
                        (void*)offsetof(MontageVertex, u)  // offset
  );
}

void Hfrag::render_hfrag_level(Hfrag::HfragLevel* lev,
                               SharedRenderState* render_state,
                               ScopedProfilerNode& prof,
                               const TfragPcPortData& pc_data,
                               const u8* occlusion_data) {
  // first pass, determine visibility and which buckets we need to generate textures for
  for (auto& b : m_bucket_used) {
    b = false;
  }
  lev->stats = {};

  // bucket 0 is not drawn, although there is data there.
  for (u32 bucket_idx = 1; bucket_idx < lev->hfrag->buckets.size(); bucket_idx++) {
    const auto& bucket = lev->hfrag->buckets[bucket_idx];
    for (u32 corner_idx : bucket.corners) {
      const auto& corner = lev->hfrag->corners[corner_idx];
      lev->stats.total_corners++;
      bool draw = true;

      if (sphere_in_view_ref(corner.bsphere, pc_data.camera.planes)) {
        lev->stats.corners_in_view++;
      } else {
        draw = false;
      }

      if (draw && occlusion_data) {  // only check vis bit if frustum culling passes
        int occlusion_byte = corner.vis_id / 8;
        int occlusion_bit = corner.vis_id & 7;
        if ((occlusion_data[occlusion_byte] & (1 << (7 - occlusion_bit)))) {
          lev->stats.corners_in_view_and_not_occluded++;
        } else {
          draw = false;
        }
      }

      m_corner_vis[corner_idx] = draw;
      if (draw) {
        m_bucket_used[bucket_idx] = true;
      }
    }
    if (m_bucket_used[bucket_idx]) {
      lev->stats.buckets_used++;
    }
  }

  // textures
  render_hfrag_montage_textures(lev, render_state, prof);

  // generate time of day texture
  interp_time_of_day(pc_data.camera.itimes, lev->hfrag->time_of_day_colors, m_color_result.data());
  glActiveTexture(GL_TEXTURE10);
  glBindTexture(GL_TEXTURE_1D, lev->time_of_day_texture);
  glTexSubImage1D(GL_TEXTURE_1D, 0, 0, lev->num_colors, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
                  m_color_result.data());

  // initialize data
  glBindVertexArray(lev->vao);
  glBindBuffer(GL_ARRAY_BUFFER, lev->vertex_buffer);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, lev->index_buffer);
  glActiveTexture(GL_TEXTURE0);
  glEnable(GL_PRIMITIVE_RESTART);
  glPrimitiveRestartIndex(UINT32_MAX);

  // set up shader
  first_tfrag_draw_setup(pc_data.camera, render_state, ShaderId::HFRAG);
  setup_opengl_from_draw_mode(lev->hfrag->draw_mode, GL_TEXTURE0, false);

  glActiveTexture(GL_TEXTURE0);
  // glBindTexture(GL_TEXTURE_2D, lev->hfrag->wang_tree_tex_id[0]);

  // draw pass
  for (u32 bucket_idx = 0; bucket_idx < lev->hfrag->buckets.size(); bucket_idx++) {
    if (!m_bucket_used[bucket_idx]) {
      continue;  // no need to bind texture.
    }
    glBindTexture(GL_TEXTURE_2D, lev->montage_texture[bucket_idx].fb.texture());
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);  // HACK rm

    const auto& bucket = lev->hfrag->buckets[bucket_idx];
    for (u32 corner_idx : bucket.corners) {
      const auto& corner = lev->hfrag->corners[corner_idx];
      if (m_corner_vis[corner_idx]) {
        glDrawElements(GL_TRIANGLE_STRIP, corner.index_length, GL_UNSIGNED_INT,
                       (void*)(corner.index_start * sizeof(u32)));
        prof.add_draw_call(1);
        prof.add_tri(corner.num_tris);
      }
    }
  }
}

void Hfrag::render_hfrag_montage_textures(Hfrag::HfragLevel* lev,
                                          SharedRenderState* render_state,
                                          ScopedProfilerNode& prof) {
  glBindVertexArray(lev->montage_vao);
  glBindBuffer(GL_ARRAY_BUFFER, lev->montage_vertices);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_montage_indices);
  glEnable(GL_PRIMITIVE_RESTART);
  glPrimitiveRestartIndex(UINT32_MAX);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_BLEND);

  const auto& sh = render_state->shaders[ShaderId::HFRAG_MONTAGE];
  sh.activate();
  glUniform1i(glGetUniformLocation(sh.id(), "tex_T0"), 0);

  for (int bi = 0; bi < kNumBuckets; bi++) {
    if (!m_bucket_used[bi]) {
      continue;  // no need to generate textures
    }

    FramebufferTexturePairContext ctxt(lev->montage_texture[bi].fb);  // render to texture
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, lev->wang_texture);
    constexpr int index_stride = kIndsPerTile * kNumMontageTiles;
    const int offset = bi * index_stride;
    glDrawElements(GL_TRIANGLE_STRIP, index_stride, GL_UNSIGNED_INT, (void*)(offset * sizeof(u32)));
    prof.add_draw_call();
    prof.add_tri(32);
  }

  glEnable(GL_DEPTH_TEST);
}