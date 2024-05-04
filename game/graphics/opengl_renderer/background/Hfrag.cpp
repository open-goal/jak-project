#include "Hfrag.h"

#include "common/log/log.h"

Hfrag::Hfrag(const std::string& name, int my_id) : BucketRenderer(name, my_id) {}

void Hfrag::draw_debug_window() {}

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
    memcpy(&pc_port_data, xfer.data, sizeof(TfragPcPortData));
    pc_port_data.level_name[11] = '\0';

    // ugh
    if (std::string("desert-vis") == pc_port_data.level_name) {
      strcpy(pc_port_data.level_name, "dst");
    }

    // try to get a hfrag level
    auto* hfrag_level = get_hfrag_level(pc_port_data.level_name, render_state);
    if (hfrag_level) {
      hfrag_level->last_used_frame = render_state->frame_idx;
      render_hfrag_level(hfrag_level, render_state, prof, pc_port_data);
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
    printf("[hfrag] can't display %s because it's not loaded.\n", name.c_str());
    render_state->loader->debug_print_loaded_levels();
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
  lev->in_use = false;
  lev->name.clear();
  lev->last_used_frame = 0;
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
  lev->corners = &data->level->hfrag.corners;
  lev->buckets = &data->level->hfrag.buckets;
  glGenVertexArrays(1, &lev->vao);
  glBindVertexArray(lev->vao);
  lev->tod_cache = swizzle_time_of_day(data->level->hfrag.time_of_day_colors);
  lev->num_colors = data->level->hfrag.time_of_day_colors.size();
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

  glVertexAttribIPointer(3,                                // location 1 in the shader
                         1,                                // 3 values per vert
                         GL_UNSIGNED_SHORT,                // u16
                         sizeof(tfrag3::HfragmentVertex),  // stride
                         (void*)offsetof(tfrag3::HfragmentVertex, color_index)  // offset (0)
  );

  glActiveTexture(GL_TEXTURE10);
  glGenTextures(1, &lev->time_of_day_texture);
  glBindTexture(GL_TEXTURE_1D, lev->time_of_day_texture);
  glTexImage1D(GL_TEXTURE_1D, 0, GL_RGBA, TIME_OF_DAY_COLOR_COUNT, 0, GL_RGBA,
               GL_UNSIGNED_INT_8_8_8_8, nullptr);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

  glBindVertexArray(0);
  ASSERT(lev->num_colors <= TIME_OF_DAY_COLOR_COUNT);

  if (m_color_result.size() < lev->num_colors) {
    m_color_result.resize(lev->num_colors);
  }
}

void Hfrag::render_hfrag_level(Hfrag::HfragLevel* lev,
                               SharedRenderState* render_state,
                               ScopedProfilerNode& prof,
                               const TfragPcPortData& pc_data) {
  // generate time of day texture
  interp_time_of_day_fast(pc_data.camera.itimes, lev->tod_cache, m_color_result.data());
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

  // for now, very simple rendering.
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_GREATER);
  glDisable(GL_BLEND);
  glDepthMask(GL_TRUE);

  glDrawElements(GL_TRIANGLE_STRIP, 506880, GL_UNSIGNED_INT, nullptr);
  //  for (u32 ci = 0; ci < lev->corners->size(); ci++) {
  //    // const auto& corner = (*lev->corners)[ci];
  //
  //  }
}