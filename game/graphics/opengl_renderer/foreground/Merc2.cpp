#include "Merc2.h"
#include "third-party/imgui/imgui.h"
#include "game/graphics/opengl_renderer/background/background_common.h"

Merc2::Merc2(const std::string& name, BucketId my_id) : BucketRenderer(name, my_id) {
  glGenVertexArrays(1, &m_vao);
  glBindVertexArray(m_vao);

  glGenBuffers(1, &m_bones_buffer);
  glBindBuffer(GL_UNIFORM_BUFFER, m_bones_buffer);
  glBufferData(GL_UNIFORM_BUFFER, MAX_SHADER_BONES * sizeof(MercMat), nullptr, GL_DYNAMIC_DRAW);
  glBindBuffer(GL_UNIFORM_BUFFER, 0);

  for (int i = 0; i < MAX_LEVELS; i++) {
    auto& draws = m_level_draw_buckets.emplace_back();
    draws.draws.resize(MAX_DRAWS_PER_LEVEL);
  }
}

/*!
 * Handle the merc renderer switching to a different model.
 */
void Merc2::init_pc_model(const DmaTransfer& setup, SharedRenderState* render_state) {
  // determine the name. We've packed this in a separate PC-port specific packet.
  char name[128];
  strcpy(name, (const char*)setup.data);

  // get the model from the loader
  m_current_model = render_state->loader->get_merc_model(name);

  // update stats
  m_stats.num_models++;
  if (m_current_model) {
    for (const auto& effect : m_current_model->model->effects) {
      m_stats.num_effects++;
      m_stats.num_predicted_draws += effect.draws.size();
      for (const auto& draw : effect.draws) {
        m_stats.num_predicted_tris += draw.num_triangles;
      }
    }
  }
}

/*!
 * Once-per-frame initialization
 */
void Merc2::init_for_frame(SharedRenderState* render_state) {
  // reset state
  m_current_model = std::nullopt;
  m_stats = {};

  // activate the merc shader used for all draws
  render_state->shaders[ShaderId::MERC2].activate();

  // set uniforms that we know from render_state
  glUniform4f(m_uniforms.fog_color, render_state->fog_color[0], render_state->fog_color[1],
              render_state->fog_color[2], render_state->fog_intensity);
}

void Merc2::draw_debug_window() {
  ImGui::Text("Models   : %d", m_stats.num_models);
  ImGui::Text("Effects  : %d", m_stats.num_effects);
  ImGui::Text("Draws (p): %d", m_stats.num_predicted_draws);
  ImGui::Text("Tris  (p): %d", m_stats.num_predicted_tris);
  ImGui::Text("Bones    : %d", m_stats.num_bones_uploaded);
  ImGui::Text("Lights   : %d", m_stats.num_lights);
  ImGui::Text("Dflush   : %d", m_stats.num_draw_flush);
}

void Merc2::init_shaders(ShaderLibrary& shaders) {
  shaders[ShaderId::MERC2].activate();
  m_uniforms.light_direction[0] = glGetUniformLocation(shaders[ShaderId::MERC2].id(), "light_dir0");
  m_uniforms.light_direction[1] = glGetUniformLocation(shaders[ShaderId::MERC2].id(), "light_dir1");
  m_uniforms.light_direction[2] = glGetUniformLocation(shaders[ShaderId::MERC2].id(), "light_dir2");
  m_uniforms.light_color[0] = glGetUniformLocation(shaders[ShaderId::MERC2].id(), "light_col0");
  m_uniforms.light_color[1] = glGetUniformLocation(shaders[ShaderId::MERC2].id(), "light_col1");
  m_uniforms.light_color[2] = glGetUniformLocation(shaders[ShaderId::MERC2].id(), "light_col2");
  m_uniforms.light_ambient = glGetUniformLocation(shaders[ShaderId::MERC2].id(), "light_ambient");

  m_uniforms.hvdf_offset = glGetUniformLocation(shaders[ShaderId::MERC2].id(), "hvdf_offset");
  m_uniforms.perspective[0] = glGetUniformLocation(shaders[ShaderId::MERC2].id(), "perspective0");
  m_uniforms.perspective[1] = glGetUniformLocation(shaders[ShaderId::MERC2].id(), "perspective1");
  m_uniforms.perspective[2] = glGetUniformLocation(shaders[ShaderId::MERC2].id(), "perspective2");
  m_uniforms.perspective[3] = glGetUniformLocation(shaders[ShaderId::MERC2].id(), "perspective3");

  m_uniforms.fog = glGetUniformLocation(shaders[ShaderId::MERC2].id(), "fog_constants");
  m_uniforms.decal = glGetUniformLocation(shaders[ShaderId::MERC2].id(), "decal_enable");

  m_uniforms.fog_color = glGetUniformLocation(shaders[ShaderId::MERC2].id(), "fog_color");
  m_uniforms.perspective_matrix =
      glGetUniformLocation(shaders[ShaderId::MERC2].id(), "perspective_matrix");
  m_uniforms.ignore_alpha = glGetUniformLocation(shaders[ShaderId::MERC2].id(), "ignore_alpha");
}

/*!
 * Main merc2 rendering.
 */
void Merc2::render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) {
  // skip if disabled
  if (!m_enabled) {
    while (dma.current_tag_offset() != render_state->next_bucket) {
      dma.read_and_advance();
    }
    return;
  }

  init_for_frame(render_state);

  // iterate through the dma chain, filling buckets
  handle_all_dma(dma, render_state, prof);

  // flush model data to buckets
  flush_pending_model(render_state, prof);
  // flush buckets to draws
  flush_draw_buckets(render_state, prof);
}

/*!
 * Queue up some bones to be included in the bone buffer.
 * Returns the index of the first bone.
 */
u32 Merc2::alloc_bones(int count, float scale) {
  ASSERT(count + m_next_free_bone <= MAX_SHADER_BONES);

  u32 first_bone = m_next_free_bone;

  // model should have under 128 bones.
  ASSERT(count <= MAX_SKEL_BONES);

  // iterate over each bone we need
  for (int i = 0; i < count; i++) {
    auto& skel_mat = m_skel_matrix_buffer[i];
    auto& shader_mat = m_shader_matrix_buffer[m_next_free_bone++];

    // scale the transformation matrix (todo: can we move this to the extraction)
    // and copy to the large bone buffer.
    for (int j = 0; j < 3; j++) {
      shader_mat.tmat[j] = skel_mat.tmat[j] * scale;
    }
    shader_mat.tmat[3] = skel_mat.tmat[3];

    for (int j = 0; j < 3; j++) {
      shader_mat.nmat[j] = skel_mat.nmat[j];
    }

    // we could include the effect of the perspective matrix here.
    //        for (int j = 0; j < 3; j++) {
    //          tbone_buffer[i][j] = vf15.elementwise_multiply(bone_mat[j]);
    //          tbone_buffer[i][j].w() += p.w() * bone_mat[j].z();
    //          tbone_buffer[i][j] *= scale;
    //        }
    //
    //        tbone_buffer[i][3] = vf15.elementwise_multiply(bone_mat[3]) +
    //        m_low_memory.perspective[3]; tbone_buffer[i][3].w() += p.w() * bone_mat[3].z();
  }

  return first_bone;
}

u32 Merc2::alloc_lights(const VuLights& lights) {
  ASSERT(m_next_free_light < MAX_LIGHTS);
  m_stats.num_lights++;
  u32 light_idx = m_next_free_light;
  m_lights_buffer[m_next_free_light++] = lights;
  static_assert(sizeof(VuLights) == 7 * 16);
  return light_idx;
}

/*!
 * Store light values
 */
void Merc2::set_lights(const DmaTransfer& dma) {
  memcpy(&m_current_lights, dma.data, sizeof(VuLights));
}

void Merc2::handle_matrix_dma(const DmaTransfer& dma) {
  int slot = dma.vif0() & 0xff;
  ASSERT(slot < MAX_SKEL_BONES);
  memcpy(&m_skel_matrix_buffer[slot], dma.data, sizeof(MercMat));
}

/*!
 * Main MERC2 function to handle DMA
 */
void Merc2::handle_all_dma(DmaFollower& dma,
                           SharedRenderState* render_state,
                           ScopedProfilerNode& prof) {
  // process the first tag. this is just jumping to the merc-specific dma.
  auto data0 = dma.read_and_advance();
  ASSERT(data0.vif1() == 0);
  ASSERT(data0.vif0() == 0);
  ASSERT(data0.size_bytes == 0);
  if (dma.current_tag().kind == DmaTag::Kind::CALL) {
    // renderer didn't run, let's just get out of here.
    for (int i = 0; i < 4; i++) {
      dma.read_and_advance();
    }
    ASSERT(dma.current_tag_offset() == render_state->next_bucket);
    return;
  }
  ASSERT(data0.size_bytes == 0);
  ASSERT(data0.vif0() == 0);
  ASSERT(data0.vif1() == 0);

  // if we reach here, there's stuff to draw
  // this handles merc-specific setup DMA
  handle_setup_dma(dma);

  // handle each merc transfer
  while (dma.current_tag_offset() != render_state->next_bucket) {
    handle_merc_chain(dma, render_state, prof);
  }
  ASSERT(dma.current_tag_offset() == render_state->next_bucket);
}

namespace {
void set_uniform(GLuint uniform, const math::Vector3f& val) {
  glUniform3f(uniform, val.x(), val.y(), val.z());
}
void set_uniform(GLuint uniform, const math::Vector4f& val) {
  glUniform4f(uniform, val.x(), val.y(), val.z(), val.w());
}
}  // namespace

void Merc2::handle_setup_dma(DmaFollower& dma) {
  auto first = dma.read_and_advance();

  // 10 quadword setup packet
  ASSERT(first.size_bytes == 10 * 16);
  // m_stats.str += fmt::format("Setup 0: {} {} {}", first.size_bytes / 16,
  // first.vifcode0().print(), first.vifcode1().print());

  // transferred vifcodes
  {
    auto vif0 = first.vifcode0();
    auto vif1 = first.vifcode1();
    // STCYCL 4, 4
    ASSERT(vif0.kind == VifCode::Kind::STCYCL);
    auto vif0_st = VifCodeStcycl(vif0);
    ASSERT(vif0_st.cl == 4 && vif0_st.wl == 4);
    // STMOD
    ASSERT(vif1.kind == VifCode::Kind::STMOD);
    ASSERT(vif1.immediate == 0);
  }

  // 1 qw with 4 vifcodes.
  u32 vifcode_data[4];
  memcpy(vifcode_data, first.data, 16);
  {
    auto vif0 = VifCode(vifcode_data[0]);
    ASSERT(vif0.kind == VifCode::Kind::BASE);
    ASSERT(vif0.immediate == MercDataMemory::BUFFER_BASE);
    auto vif1 = VifCode(vifcode_data[1]);
    ASSERT(vif1.kind == VifCode::Kind::OFFSET);
    ASSERT((s16)vif1.immediate == MercDataMemory::BUFFER_OFFSET);
    auto vif2 = VifCode(vifcode_data[2]);
    ASSERT(vif2.kind == VifCode::Kind::NOP);
    auto vif3 = VifCode(vifcode_data[3]);
    ASSERT(vif3.kind == VifCode::Kind::UNPACK_V4_32);
    VifCodeUnpack up(vif3);
    ASSERT(up.addr_qw == MercDataMemory::LOW_MEMORY);
    ASSERT(!up.use_tops_flag);
    ASSERT(vif3.num == 8);
  }

  // 8 qw's of low memory data
  memcpy(&m_low_memory, first.data + 16, sizeof(LowMemory));
  set_uniform(m_uniforms.hvdf_offset, m_low_memory.hvdf_offset);
  set_uniform(m_uniforms.fog, m_low_memory.fog);
  for (int i = 0; i < 4; i++) {
    set_uniform(m_uniforms.perspective[i], m_low_memory.perspective[i]);
  }
  // todo rm.
  glUniformMatrix4fv(m_uniforms.perspective_matrix, 1, GL_FALSE, &m_low_memory.perspective[0].x());

  // 1 qw with another 4 vifcodes.
  u32 vifcode_final_data[4];
  memcpy(vifcode_final_data, first.data + 16 + sizeof(LowMemory), 16);
  {
    ASSERT(VifCode(vifcode_final_data[0]).kind == VifCode::Kind::FLUSHE);
    ASSERT(vifcode_final_data[1] == 0);
    ASSERT(vifcode_final_data[2] == 0);
    VifCode mscal(vifcode_final_data[3]);
    ASSERT(mscal.kind == VifCode::Kind::MSCAL);
    ASSERT(mscal.immediate == 0);
  }

  // TODO: process low memory initialization

  auto second = dma.read_and_advance();
  ASSERT(second.size_bytes == 32);  // setting up test register.
  auto nothing = dma.read_and_advance();
  ASSERT(nothing.size_bytes == 0);
  ASSERT(nothing.vif0() == 0);
  ASSERT(nothing.vif1() == 0);
}

namespace {
bool tag_is_nothing_next(const DmaFollower& dma) {
  return dma.current_tag().kind == DmaTag::Kind::NEXT && dma.current_tag().qwc == 0 &&
         dma.current_tag_vif0() == 0 && dma.current_tag_vif1() == 0;
}
bool tag_is_nothing_cnt(const DmaFollower& dma) {
  return dma.current_tag().kind == DmaTag::Kind::CNT && dma.current_tag().qwc == 0 &&
         dma.current_tag_vif0() == 0 && dma.current_tag_vif1() == 0;
}
}  // namespace

void Merc2::handle_merc_chain(DmaFollower& dma,
                              SharedRenderState* render_state,
                              ScopedProfilerNode& prof) {
  while (tag_is_nothing_next(dma)) {
    auto nothing = dma.read_and_advance();
    ASSERT(nothing.size_bytes == 0);
  }
  if (dma.current_tag().kind == DmaTag::Kind::CALL) {
    for (int i = 0; i < 4; i++) {
      dma.read_and_advance();
    }
    return;
  }

  auto init = dma.read_and_advance();

  if (init.vifcode1().kind == VifCode::Kind::PC_PORT) {
    // we got a PC PORT packet. this contains some extra data to set up the model
    flush_pending_model(render_state, prof);
    init_pc_model(init, render_state);
    ASSERT(tag_is_nothing_cnt(dma));
    init = dma.read_and_advance();  // dummy tag in pc port
    init = dma.read_and_advance();
  }

  // row stuff.
  ASSERT(init.vifcode0().kind == VifCode::Kind::STROW);
  ASSERT(init.size_bytes == 16);
  // m_vif.row[0] = init.vif1();
  // memcpy(m_vif.row + 1, init.data, 12);
  u32 extra;
  memcpy(&extra, init.data + 12, 4);
  // ASSERT(extra == 0);
  m_current_effect_enable_bits = extra;
  m_current_ignore_alpha_bits = extra >> 16;
  DmaTransfer next;

  bool setting_up = true;
  u32 mscal_addr = -1;
  while (setting_up) {
    next = dma.read_and_advance();
    // fmt::print("next: {}", dma.current_tag().print());
    u32 offset_in_data = 0;
    //    fmt::print("START {} : {} {}\n", next.size_bytes, next.vifcode0().print(),
    //               next.vifcode1().print());
    auto vif0 = next.vifcode0();
    switch (vif0.kind) {
      case VifCode::Kind::NOP:
      case VifCode::Kind::FLUSHE:
        break;
      case VifCode::Kind::STMOD:
        ASSERT(vif0.immediate == 0 || vif0.immediate == 1);
        // m_vif.stmod = vif0.immediate;
        break;
      default:
        ASSERT(false);
    }

    auto vif1 = next.vifcode1();
    switch (vif1.kind) {
      case VifCode::Kind::UNPACK_V4_8: {
        VifCodeUnpack up(vif1);
        offset_in_data += 4 * vif1.num;
      } break;
      case VifCode::Kind::UNPACK_V4_32: {
        VifCodeUnpack up(vif1);
        if (up.addr_qw == 132 && vif1.num == 8) {
          set_lights(next);
        } else if (vif1.num == 7) {
          handle_matrix_dma(next);
        }
        offset_in_data += 16 * vif1.num;
      } break;
      case VifCode::Kind::MSCAL:
        // fmt::print("cal\n");
        mscal_addr = vif1.immediate;
        ASSERT(next.size_bytes == 0);
        setting_up = false;
        break;
      default:
        ASSERT(false);
    }

    ASSERT(offset_in_data <= next.size_bytes);
    if (offset_in_data < next.size_bytes) {
      ASSERT((offset_in_data % 4) == 0);
      u32 leftover = next.size_bytes - offset_in_data;
      if (leftover < 16) {
        for (u32 i = 0; i < leftover; i++) {
          ASSERT(next.data[offset_in_data + i] == 0);
        }
      } else {
        ASSERT(false);
      }
    }
  }
}

/*!
 * Flush a model to draw buckets
 */
void Merc2::flush_pending_model(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  if (!m_current_model) {
    return;
  }

  const LevelData* lev = m_current_model->level;
  const tfrag3::MercModel* model = m_current_model->model;

  int bone_count = (model->max_bones + 31) & (~31);  // todo
  // int bone_count = 128;

  if (m_next_free_light >= MAX_LIGHTS) {
    fmt::print("MERC2 out of lights, consider increasing MAX_LIGHTS\n");
    flush_draw_buckets(render_state, prof);
  }

  if (m_next_free_bone + bone_count > MAX_SHADER_BONES) {
    fmt::print("MERC2 out of bones, consider increasing MAX_SHADER_BONES\n");
    flush_draw_buckets(render_state, prof);
  }

  // find a level bucket
  LevelDrawBucket* lev_bucket = nullptr;
  for (u32 i = 0; i < m_next_free_level_bucket; i++) {
    if (m_level_draw_buckets[i].level == lev) {
      lev_bucket = &m_level_draw_buckets[i];
      break;
    }
  }

  if (!lev_bucket) {
    // no existing bucket
    if (m_next_free_level_bucket >= m_level_draw_buckets.size()) {
      // out of room, flush
      fmt::print("MERC2 out of levels, consider increasing MAX_LEVELS\n");
      flush_draw_buckets(render_state, prof);
      // and retry the whole thing.
      flush_pending_model(render_state, prof);
      return;
    }
    // alloc a new one
    lev_bucket = &m_level_draw_buckets[m_next_free_level_bucket++];
    lev_bucket->reset();
    lev_bucket->level = lev;
  }

  if (lev_bucket->next_free_draw + model->max_draws >= lev_bucket->draws.size()) {
    // out of room, flush
    fmt::print("MERC2 out of draws, consider increasing MAX_DRAWS_PER_LEVEL\n");
    flush_draw_buckets(render_state, prof);
    // and retry the whole thing.
    flush_pending_model(render_state, prof);
    return;
  }

  u32 first_bone = alloc_bones(bone_count, model->scale_xyz);

  // allocate lights
  u32 lights = alloc_lights(m_current_lights);
  //
  for (size_t ei = 0; ei < model->effects.size(); ei++) {
    if (!(m_current_effect_enable_bits & (1 << ei))) {
      continue;
    }

    u8 ignore_alpha = (m_current_ignore_alpha_bits & (1 << ei));
    auto& effect = model->effects[ei];
    for (auto& mdraw : effect.draws) {
      Draw* draw = &lev_bucket->draws[lev_bucket->next_free_draw++];
      draw->first_index = mdraw.first_index;
      draw->index_count = mdraw.index_count;
      draw->mode = mdraw.mode;
      draw->texture = mdraw.tree_tex_id;
      draw->first_bone = first_bone;
      draw->light_idx = lights;
      draw->num_triangles = mdraw.num_triangles;
      draw->ignore_alpha = ignore_alpha;
    }
  }

  m_current_model = std::nullopt;
}

void Merc2::flush_draw_buckets(SharedRenderState* /*render_state*/, ScopedProfilerNode& prof) {
  m_stats.num_draw_flush++;

  for (u32 li = 0; li < m_next_free_level_bucket; li++) {
    const auto& lev_bucket = m_level_draw_buckets[li];
    const auto* lev = lev_bucket.level;
    glBindVertexArray(m_vao);
    glBindBuffer(GL_ARRAY_BUFFER, lev->merc_vertices);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, lev->merc_indices);

    glEnable(GL_PRIMITIVE_RESTART);
    glPrimitiveRestartIndex(UINT32_MAX);
    glEnableVertexAttribArray(0);
    glEnableVertexAttribArray(1);
    glEnableVertexAttribArray(2);
    glEnableVertexAttribArray(3);
    glEnableVertexAttribArray(4);
    glEnableVertexAttribArray(5);
    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_GEQUAL);

    glVertexAttribPointer(0,                                        // location 0 in the shader
                          3,                                        // 3 values per vert
                          GL_FLOAT,                                 // floats
                          GL_FALSE,                                 // normalized
                          sizeof(tfrag3::MercVertex),               // stride
                          (void*)offsetof(tfrag3::MercVertex, pos)  // offset (0)
    );

    glVertexAttribPointer(1,                                              // location 1 in the
                          3,                                              // 3 values per vert
                          GL_FLOAT,                                       // floats
                          GL_FALSE,                                       // normalized
                          sizeof(tfrag3::MercVertex),                     // stride
                          (void*)offsetof(tfrag3::MercVertex, normal[0])  // offset (0)
    );

    glVertexAttribPointer(2,                                               // location 1 in the
                          3,                                               // 3 values per vert
                          GL_FLOAT,                                        // floats
                          GL_FALSE,                                        // normalized
                          sizeof(tfrag3::MercVertex),                      // stride
                          (void*)offsetof(tfrag3::MercVertex, weights[0])  // offset (0)
    );

    glVertexAttribPointer(3,                                          // location 1 in the shader
                          2,                                          // 3 values per vert
                          GL_FLOAT,                                   // floats
                          GL_FALSE,                                   // normalized
                          sizeof(tfrag3::MercVertex),                 // stride
                          (void*)offsetof(tfrag3::MercVertex, st[0])  // offset (0)
    );

    glVertexAttribPointer(4,                                            // location 1 in the shader
                          3,                                            // 3 values per vert
                          GL_UNSIGNED_BYTE,                             // floats
                          GL_TRUE,                                      // normalized
                          sizeof(tfrag3::MercVertex),                   // stride
                          (void*)offsetof(tfrag3::MercVertex, rgba[0])  // offset (0)
    );

    glVertexAttribIPointer(5,                                            // location 0 in the
                           3,                                            // 3 floats per vert
                           GL_UNSIGNED_BYTE,                             // u8's
                           sizeof(tfrag3::MercVertex),                   //
                           (void*)offsetof(tfrag3::MercVertex, mats[0])  // offset in array
    );

    int last_tex = -1;
    int last_light = -1;
    m_stats.num_bones_uploaded += m_next_free_bone;

    glBindBuffer(GL_UNIFORM_BUFFER, m_bones_buffer);
    glBufferSubData(GL_UNIFORM_BUFFER, 0, m_next_free_bone * sizeof(MercMat),
                    m_shader_matrix_buffer);
    glBindBuffer(GL_UNIFORM_BUFFER, 0);

    for (u32 di = 0; di < lev_bucket.next_free_draw; di++) {
      auto& draw = lev_bucket.draws[di];
      glUniform1i(m_uniforms.ignore_alpha, draw.ignore_alpha);
      if ((int)draw.texture != last_tex) {
        glBindTexture(GL_TEXTURE_2D, lev->textures.at(draw.texture));
        last_tex = draw.texture;
      }

      if ((int)draw.light_idx != last_light) {
        set_uniform(m_uniforms.light_direction[0], m_lights_buffer[draw.light_idx].direction0);
        set_uniform(m_uniforms.light_direction[1], m_lights_buffer[draw.light_idx].direction1);
        set_uniform(m_uniforms.light_direction[2], m_lights_buffer[draw.light_idx].direction2);
        set_uniform(m_uniforms.light_color[0], m_lights_buffer[draw.light_idx].color0);
        set_uniform(m_uniforms.light_color[1], m_lights_buffer[draw.light_idx].color1);
        set_uniform(m_uniforms.light_color[2], m_lights_buffer[draw.light_idx].color2);
        set_uniform(m_uniforms.light_ambient, m_lights_buffer[draw.light_idx].ambient);
        last_light = draw.light_idx;
      }
      setup_opengl_from_draw_mode(draw.mode, GL_TEXTURE0, true);

      glUniform1i(m_uniforms.decal, draw.mode.get_decal());

      prof.add_draw_call();
      prof.add_tri(draw.num_triangles);
      glBindBufferRange(GL_UNIFORM_BUFFER, 1, m_bones_buffer, sizeof(MercMat) * draw.first_bone,
                        128 * sizeof(MercMat));
      glDrawElements(GL_TRIANGLE_STRIP, draw.index_count, GL_UNSIGNED_INT,
                     (void*)(sizeof(u32) * draw.first_index));
    }
  }

  m_next_free_light = 0;
  m_next_free_bone = 0;
  m_next_free_level_bucket = 0;
}
