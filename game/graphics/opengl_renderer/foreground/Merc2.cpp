#include "Merc2.h"

#include "common/global_profiler/GlobalProfiler.h"

#include "game/graphics/opengl_renderer/background/background_common.h"

#include "third-party/imgui/imgui.h"

Merc2::Merc2(const std::string& name, int my_id) : BucketRenderer(name, my_id) {
  glGenVertexArrays(1, &m_vao);
  glBindVertexArray(m_vao);

  glGenBuffers(1, &m_bones_buffer);
  glBindBuffer(GL_UNIFORM_BUFFER, m_bones_buffer);
  std::vector<u8> temp(MAX_SHADER_BONE_VECTORS * sizeof(math::Vector4f));
  glBufferData(GL_UNIFORM_BUFFER, MAX_SHADER_BONE_VECTORS * sizeof(math::Vector4f), temp.data(),
               GL_DYNAMIC_DRAW);
  glBindBuffer(GL_UNIFORM_BUFFER, 0);

  GLint val;
  glGetIntegerv(GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT, &val);
  if (val <= 16) {
    // somehow doubt this can happen, but just in case
    m_opengl_buffer_alignment = 1;
  } else {
    m_opengl_buffer_alignment = val / 16;  // number of bone vectors
    if (m_opengl_buffer_alignment * 16 != (u32)val) {
      ASSERT_MSG(false,
                 fmt::format("opengl uniform buffer alignment is {}, which is strange\n", val));
    }
  }

  for (int i = 0; i < MAX_LEVELS; i++) {
    auto& draws = m_level_draw_buckets.emplace_back();
    draws.draws.resize(MAX_DRAWS_PER_LEVEL);
    draws.envmap_draws.resize(MAX_ENVMAP_DRAWS_PER_LEVEL);
  }

  m_mod_vtx_temp.resize(MAX_MOD_VTX);
  m_mod_vtx_pos_unpack_temp.resize(MAX_MOD_VTX * 2);
  m_mod_vtx_nrm_unpack_temp.resize(MAX_MOD_VTX * 2);
}

Merc2::~Merc2() {
  for (auto& x : m_mod_vtx_buffers) {
    glDeleteBuffers(1, &x.vertex);
    glDeleteVertexArrays(1, &x.vao);
  }
}

/*!
 * Handle the merc renderer switching to a different model.
 */
void Merc2::init_pc_model(const DmaTransfer& setup,
                          SharedRenderState* render_state,
                          ScopedProfilerNode& proff) {
  auto p = scoped_prof("init-pc");
  //  ;; name   (128 char, 8 qw)
  //  ;; lights (7 qw x 1)
  //  ;; matrix slot string (128 char, 8 qw)
  //  ;; matrices (7 qw x N)
  //  ;; flags    (num-effects, effect-alpha-ignore, effect-disable)
  //  ;; fades    (u32 x N), padding to qw aligned

  // Part 1: name
  const u8* input_data = setup.data;
  ASSERT(strlen((const char*)input_data) < 127);
  char name[128];
  strcpy(name, (const char*)setup.data);
  m_current_model = render_state->loader->get_merc_model(name);
  if (!m_current_model) {
    m_current_model_updates_verts = false;
    m_stats.num_missing_models++;
    return;
  }
  input_data += 128;

  // Part 2: lights
  memcpy(&m_current_lights, input_data, sizeof(VuLights));
  input_data += sizeof(VuLights);

  // Part 3: matrix slot string
  auto* matrix_array = (const u32*)(input_data + 128);
  int i;
  for (i = 0; i < 128; i++) {
    if (input_data[i] == 0xff) {
      break;
    }
    u32 addr;
    memcpy(&addr, &matrix_array[i * 4], 4);
    const u8* real_addr = setup.data - setup.data_offset + addr;
    ASSERT(input_data[i] < MAX_SKEL_BONES);
    memcpy(&m_skel_matrix_buffer[input_data[i]], real_addr, sizeof(MercMat));
  }
  input_data += 128 + 16 * i;

  // Part 4: flags
  auto* flags = (const u32*)input_data;
  int num_effects = flags[0];
  ASSERT(num_effects < kMaxEffect);
  m_current_ignore_alpha_bits = flags[1];
  m_current_effect_enable_bits = flags[2];
  m_current_model_updates_verts = flags[3];
  input_data += 16;

  // Part 5: fades
  for (int ei = 0; ei < num_effects; ei++) {
    for (int j = 0; j < 4; j++) {
      m_fade_buffer[ei * 4 + j] = input_data[ei * 4 + j];
    }
  }
  input_data += (((num_effects * 4) + 15) / 16) * 16;

  // Part 6: pointers
  if (m_current_model_updates_verts) {
    auto p = scoped_prof("update-verts");
    for (int ei = 0; ei < num_effects; ei++) {
      const auto& effect = m_current_model->model->effects[ei];
      if (effect.mod.mod_draw.empty()) {
        continue;
      }

      prof().begin_event("start1");
      auto opengl_buffers = alloc_mod_vtx_buffer(m_current_model->level);
      m_mod_opengl_buffers[ei] = opengl_buffers;
      if (effect.mod.vertices.size() > MAX_MOD_VTX) {
        fmt::print("More mod vertices than MAX_MOD_VTX. {} > {}\n", effect.mod.vertices.size(),
                   MAX_MOD_VTX);
        ASSERT_NOT_REACHED();
      }
      memcpy(m_mod_vtx_temp.data(), effect.mod.vertices.data(),
             sizeof(tfrag3::MercVertex) * effect.mod.vertices.size());

      u32 goal_addr;
      memcpy(&goal_addr, input_data + 4 * ei, 4);
      const u8* ee0 = setup.data - setup.data_offset;
      const u8* merc_effect = ee0 + goal_addr;
      u16 frag_cnt;
      memcpy(&frag_cnt, merc_effect + 18, 2);
      u32 frag_goal;
      memcpy(&frag_goal, merc_effect, 4);
      u32 frag_ctrl_goal;
      memcpy(&frag_ctrl_goal, merc_effect + 4, 4);
      const u8* frag = ee0 + frag_goal;
      const u8* frag_ctrl = ee0 + frag_ctrl_goal;

      // loop over frags
      u32 vidx = 0;
      u32 st_vif_add = m_current_model->model->st_vif_add;
      float xyz_scale = m_current_model->model->xyz_scale;
      prof().end_event();
      {
        auto p = scoped_prof("vert-math");
        for (int fi = 0; fi < frag_cnt; fi++) {
          u8 unsigned_four_count = frag_ctrl[0];
          u8 lump_four_count = frag_ctrl[1];
          u8 fp_qwc = frag_ctrl[2];
          u8 mat_xfer_count = frag_ctrl[3];
          u32 mm_qwc_off = frag[10];
          float float_offsets[3];
          memcpy(float_offsets, &frag[mm_qwc_off * 16], 12);

          u32 my_u4_count = ((unsigned_four_count + 3) / 4) * 16;
          u32 my_l4_count = my_u4_count + ((lump_four_count + 3) / 4) * 16;
          int kk = 0;
          for (u32 w = my_u4_count / 4; w < (my_l4_count / 4) - 2; w += 3) {
            // just want positions for now.
            u32 q0w = 0x4b010000 + frag[w * 4 + (0 * 4) + 3];
            u32 q1w = 0x4b010000 + frag[w * 4 + (1 * 4) + 3];
            u32 q2w = 0x4b010000 + frag[w * 4 + (2 * 4) + 3];

            // and maybe normals
            u32 q0z = 0x47800000 + frag[w * 4 + (0 * 4) + 2];
            u32 q1z = 0x47800000 + frag[w * 4 + (1 * 4) + 2];
            u32 q2z = 0x47800000 + frag[w * 4 + (2 * 4) + 2];

            auto* pos_array = m_mod_vtx_pos_unpack_temp[vidx].data();
            memcpy(&pos_array[0], &q0w, 4);
            memcpy(&pos_array[1], &q1w, 4);
            memcpy(&pos_array[2], &q2w, 4);
            pos_array[0] += float_offsets[0];
            pos_array[1] += float_offsets[1];
            pos_array[2] += float_offsets[2];
            pos_array[0] *= xyz_scale;
            pos_array[1] *= xyz_scale;
            pos_array[2] *= xyz_scale;

            auto* nrm_array = m_mod_vtx_nrm_unpack_temp[vidx].data();
            memcpy(&nrm_array[0], &q0z, 4);
            memcpy(&nrm_array[1], &q1z, 4);
            memcpy(&nrm_array[2], &q2z, 4);
            nrm_array[0] += -65537;
            nrm_array[1] += -65537;
            nrm_array[2] += -65537;
            vidx++;
          }

          // next control
          frag_ctrl += 4 + 2 * mat_xfer_count;

          // next frag
          u32 mm_qwc_count = frag[11];
          frag += mm_qwc_count * 16;
        }
      }

      {
        scoped_prof("copy");
        for (u32 i = 0; i < effect.mod.vertices.size(); i++) {
          int addr = effect.mod.vertex_lump4_addr.at(i);
          if (addr < vidx) {
            memcpy(m_mod_vtx_temp.at(i).pos, m_mod_vtx_pos_unpack_temp.at(addr).data(), 12);
            memcpy(m_mod_vtx_temp.at(i).normal, m_mod_vtx_nrm_unpack_temp.at(addr).data(), 12);
          }
        }
      }

      m_stats.num_uploads++;
      m_stats.num_upload_bytes += effect.mod.vertices.size() * sizeof(tfrag3::MercVertex);
      {
        auto pp = scoped_prof("update-verts-upload");
        glBindBuffer(GL_ARRAY_BUFFER, opengl_buffers.vertex);
        glBufferData(GL_ARRAY_BUFFER, effect.mod.vertices.size() * sizeof(tfrag3::MercVertex),
                     m_mod_vtx_temp.data(), GL_DYNAMIC_DRAW);
      }
    }
  }

  if (m_current_model) {
    m_stats.num_models++;
    for (const auto& effect : m_current_model->model->effects) {
      bool envmap = effect.has_envmap;
      m_stats.num_effects++;
      m_stats.num_predicted_draws += effect.all_draws.size();
      if (envmap) {
        m_stats.num_envmap_effects++;
        m_stats.num_predicted_draws += effect.all_draws.size();
      }
      for (const auto& draw : effect.all_draws) {
        m_stats.num_predicted_tris += draw.num_triangles;
        if (envmap) {
          m_stats.num_predicted_tris += draw.num_triangles;
        }
      }
    }
  } else {
    m_stats.num_missing_models++;
  }
}

void Merc2::draw_debug_window() {
  ImGui::Text("Models   : %d", m_stats.num_models);
  ImGui::Text("Effects  : %d", m_stats.num_effects);
  ImGui::Text("Draws (p): %d", m_stats.num_predicted_draws);
  ImGui::Text("Tris  (p): %d", m_stats.num_predicted_tris);
  ImGui::Text("Bones    : %d", m_stats.num_bones_uploaded);
  ImGui::Text("Lights   : %d", m_stats.num_lights);
  ImGui::Text("Dflush   : %d", m_stats.num_draw_flush);

  ImGui::Text("EEffects : %d", m_stats.num_envmap_effects);
  ImGui::Text("ETris    : %d", m_stats.num_envmap_tris);

  ImGui::Text("Uploads  : %d", m_stats.num_uploads);
  ImGui::Text("Upload kB: %d", m_stats.num_upload_bytes / 1024);

  ImGui::Checkbox("Debug", &m_debug_mode);
  if (m_debug_mode) {
    for (const auto& model : m_debug.model_list) {
      if (ImGui::TreeNode(model.name.c_str())) {
        for (const auto& e : model.effects) {
          for (const auto& d : e.draws) {
            ImGui::Text("%s", d.mode.to_string().c_str());
          }
          ImGui::Separator();
        }
        ImGui::TreePop();
      }
    }
  }
}

void Merc2::init_shaders(ShaderLibrary& shaders) {
  init_shader_common(shaders[ShaderId::MERC2], &m_merc_uniforms, true);
  init_shader_common(shaders[ShaderId::EMERC], &m_emerc_uniforms, false);
  m_emerc_uniforms.fade = glGetUniformLocation(shaders[ShaderId::EMERC].id(), "fade");
}

void Merc2::init_shader_common(Shader& shader, Uniforms* uniforms, bool include_lights) {
  auto id = shader.id();
  shader.activate();
  if (include_lights) {
    uniforms->light_direction[0] = glGetUniformLocation(id, "light_dir0");
    uniforms->light_direction[1] = glGetUniformLocation(id, "light_dir1");
    uniforms->light_direction[2] = glGetUniformLocation(id, "light_dir2");
    uniforms->light_color[0] = glGetUniformLocation(id, "light_col0");
    uniforms->light_color[1] = glGetUniformLocation(id, "light_col1");
    uniforms->light_color[2] = glGetUniformLocation(id, "light_col2");
    uniforms->light_ambient = glGetUniformLocation(id, "light_ambient");
  }

  uniforms->hvdf_offset = glGetUniformLocation(id, "hvdf_offset");

  uniforms->fog = glGetUniformLocation(id, "fog_constants");
  uniforms->decal = glGetUniformLocation(id, "decal_enable");

  uniforms->fog_color = glGetUniformLocation(id, "fog_color");
  uniforms->perspective_matrix = glGetUniformLocation(id, "perspective_matrix");
  uniforms->ignore_alpha = glGetUniformLocation(id, "ignore_alpha");

  uniforms->gfx_hack_no_tex = glGetUniformLocation(id, "gfx_hack_no_tex");
}

void Merc2::switch_to_merc2(SharedRenderState* render_state) {
  render_state->shaders[ShaderId::MERC2].activate();

  // set uniforms that we know from render_state
  glUniform4f(m_merc_uniforms.fog_color, render_state->fog_color[0] / 255.f,
              render_state->fog_color[1] / 255.f, render_state->fog_color[2] / 255.f,
              render_state->fog_intensity / 255);
  glUniform1i(m_merc_uniforms.gfx_hack_no_tex, Gfx::g_global_settings.hack_no_tex);
}

void Merc2::switch_to_emerc(SharedRenderState* render_state) {
  render_state->shaders[ShaderId::EMERC].activate();
  // set uniforms that we know from render_state
  glUniform4f(m_emerc_uniforms.fog_color, render_state->fog_color[0] / 255.f,
              render_state->fog_color[1] / 255.f, render_state->fog_color[2] / 255.f,
              render_state->fog_intensity / 255);
  glUniform1i(m_emerc_uniforms.gfx_hack_no_tex, Gfx::g_global_settings.hack_no_tex);
}

/*!
 * Main merc2 rendering.
 */
void Merc2::render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) {
  m_stats = {};
  if (m_debug_mode) {
    m_debug = {};
  }

  // skip if disabled
  if (!m_enabled) {
    while (dma.current_tag_offset() != render_state->next_bucket) {
      dma.read_and_advance();
    }
    return;
  }
  m_current_model = std::nullopt;
  switch_to_merc2(render_state);

  {
    auto pp = scoped_prof("handle-all-dma");
    // iterate through the dma chain, filling buckets
    handle_all_dma(dma, render_state, prof);
  }

  {
    auto pp = scoped_prof("last-flush-model");
    // flush model data to buckets
    flush_pending_model(render_state, prof);
  }

  {
    auto pp = scoped_prof("flush-buckets");
    // flush buckets to draws
    flush_draw_buckets(render_state, prof);
  }
}

u32 Merc2::alloc_lights(const VuLights& lights) {
  ASSERT(m_next_free_light < MAX_LIGHTS);
  m_stats.num_lights++;
  u32 light_idx = m_next_free_light;
  m_lights_buffer[m_next_free_light++] = lights;
  static_assert(sizeof(VuLights) == 7 * 16);
  return light_idx;
}

std::string Merc2::ShaderMercMat::to_string() const {
  return fmt::format("tmat:\n{}\n{}\n{}\n{}\n", tmat[0].to_string_aligned(),
                     tmat[1].to_string_aligned(), tmat[2].to_string_aligned(),
                     tmat[3].to_string_aligned());
}

/*!
 * Main MERC2 function to handle DMA
 */
void Merc2::handle_all_dma(DmaFollower& dma,
                           SharedRenderState* render_state,
                           ScopedProfilerNode& prof) {
  // process the first tag. this is just jumping to the merc-specific dma.
  auto data0 = dma.read_and_advance();
  ASSERT(data0.vif1() == 0 || data0.vifcode1().kind == VifCode::Kind::NOP);
  ASSERT(data0.vif0() == 0 || data0.vifcode0().kind == VifCode::Kind::NOP ||
         data0.vifcode0().kind == VifCode::Kind::MARK);
  ASSERT(data0.size_bytes == 0);
  if (dma.current_tag().kind == DmaTag::Kind::CALL) {
    // renderer didn't run, let's just get out of here.
    for (int i = 0; i < 4; i++) {
      dma.read_and_advance();
    }
    ASSERT(dma.current_tag_offset() == render_state->next_bucket);
    return;
  }

  if (dma.current_tag_offset() == render_state->next_bucket) {
    return;
  }
  // if we reach here, there's stuff to draw
  // this handles merc-specific setup DMA
  handle_setup_dma(dma, render_state);

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

void Merc2::handle_setup_dma(DmaFollower& dma, SharedRenderState* render_state) {
  auto first = dma.read_and_advance();

  // 10 quadword setup packet
  ASSERT(first.size_bytes == 10 * 16);

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

  switch_to_merc2(render_state);
  set_uniform(m_merc_uniforms.hvdf_offset, m_low_memory.hvdf_offset);
  set_uniform(m_merc_uniforms.fog, m_low_memory.fog);
  glUniformMatrix4fv(m_merc_uniforms.perspective_matrix, 1, GL_FALSE,
                     &m_low_memory.perspective[0].x());
  switch_to_emerc(render_state);
  set_uniform(m_emerc_uniforms.hvdf_offset, m_low_memory.hvdf_offset);
  set_uniform(m_emerc_uniforms.fog, m_low_memory.fog);
  glUniformMatrix4fv(m_emerc_uniforms.perspective_matrix, 1, GL_FALSE,
                     &m_low_memory.perspective[0].x());

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

  if (render_state->version == GameVersion::Jak1) {
    auto second = dma.read_and_advance();
    ASSERT(second.size_bytes == 32);  // setting up test register.
    auto nothing = dma.read_and_advance();
    ASSERT(nothing.size_bytes == 0);
    ASSERT(nothing.vif0() == 0);
    ASSERT(nothing.vif1() == 0);
  } else {
    auto second = dma.read_and_advance();
    ASSERT(second.size_bytes == 48);  // setting up test/zbuf register.
    // todo z write mask stuff.
    auto nothing = dma.read_and_advance();
    ASSERT(nothing.size_bytes == 0);
    ASSERT(nothing.vif0() == 0);
    ASSERT(nothing.vif1() == 0);
  }
}

namespace {
bool tag_is_nothing_next(const DmaFollower& dma) {
  return dma.current_tag().kind == DmaTag::Kind::NEXT && dma.current_tag().qwc == 0 &&
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
  int skip_count = 2;
  if (render_state->version == GameVersion::Jak2) {
    skip_count = 1;
  }

  while (init.vifcode1().kind == VifCode::Kind::PC_PORT) {
    flush_pending_model(render_state, prof);
    init_pc_model(init, render_state, prof);
    for (int i = 0; i < skip_count; i++) {
      auto link = dma.read_and_advance();
      ASSERT(link.vifcode0().kind == VifCode::Kind::NOP);
      ASSERT(link.vifcode1().kind == VifCode::Kind::NOP);
      ASSERT(link.size_bytes == 0);
    }
    init = dma.read_and_advance();
  }

  if (init.vifcode0().kind == VifCode::Kind::FLUSHA) {
    int num_skipped = 0;
    while (dma.current_tag_offset() != render_state->next_bucket) {
      dma.read_and_advance();
      num_skipped++;
    }
    ASSERT(num_skipped < 4);
    return;
  }
}

/*!
 * Queue up some bones to be included in the bone buffer.
 * Returns the index of the first bone vector.
 */
u32 Merc2::alloc_bones(int count) {
  u32 first_bone_vector = m_next_free_bone_vector;
  ASSERT(count * 8 + first_bone_vector <= MAX_SHADER_BONE_VECTORS);

  // model should have under 128 bones.
  ASSERT(count <= MAX_SKEL_BONES);

  // iterate over each bone we need
  for (int i = 0; i < count; i++) {
    auto& skel_mat = m_skel_matrix_buffer[i];
    auto* shader_mat = &m_shader_bone_vector_buffer[m_next_free_bone_vector];
    int bv = 0;

    // and copy to the large bone buffer.
    for (int j = 0; j < 4; j++) {
      shader_mat[bv++] = skel_mat.tmat[j];
    }

    for (int j = 0; j < 3; j++) {
      shader_mat[bv++] = skel_mat.nmat[j];
    }

    m_next_free_bone_vector += 8;
  }

  auto b0 = m_next_free_bone_vector;
  m_next_free_bone_vector += m_opengl_buffer_alignment - 1;
  m_next_free_bone_vector /= m_opengl_buffer_alignment;
  m_next_free_bone_vector *= m_opengl_buffer_alignment;
  ASSERT(b0 <= m_next_free_bone_vector);
  ASSERT(first_bone_vector + count * 8 <= m_next_free_bone_vector);
  return first_bone_vector;
}

Merc2::ModBuffers Merc2::alloc_mod_vtx_buffer(const LevelData* lev) {
  if (m_next_mod_vtx_buffer >= m_mod_vtx_buffers.size()) {
    GLuint b;
    glGenBuffers(1, &b);
    GLuint vao;
    glGenVertexArrays(1, &vao);
    glBindVertexArray(vao);
    glBindBuffer(GL_ARRAY_BUFFER, b);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, lev->merc_indices);
    setup_merc_vao();
    m_mod_vtx_buffers.push_back({vao, b});
  }
  return m_mod_vtx_buffers[m_next_mod_vtx_buffer++];
}

/*!
 * Flush a model to draw buckets
 */
void Merc2::flush_pending_model(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  if (!m_current_model) {
    return;
  }

  if (m_debug_mode) {
    auto& d = m_debug.model_list.emplace_back();
    d.name = m_current_model->model->name;
    for (auto& e : m_current_model->model->effects) {
      auto& de = d.effects.emplace_back();
      de.envmap = e.has_envmap;
      de.envmap_mode = e.envmap_mode;
      for (auto& draw : e.all_draws) {
        auto& dd = de.draws.emplace_back();
        dd.mode = draw.mode;
        dd.num_tris = draw.num_triangles;
      }
    }
  }

  const LevelData* lev = m_current_model->level;
  const tfrag3::MercModel* model = m_current_model->model;

  int bone_count = model->max_bones + 1;

  if (m_next_free_light >= MAX_LIGHTS) {
    fmt::print("MERC2 out of lights, consider increasing MAX_LIGHTS\n");
    flush_draw_buckets(render_state, prof);
  }

  if (m_next_free_bone_vector + m_opengl_buffer_alignment + bone_count * 8 >
      MAX_SHADER_BONE_VECTORS) {
    fmt::print("MERC2 out of bones, consider increasing MAX_SHADER_BONE_VECTORS\n");
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
      // fmt::print("MERC2 out of levels, consider increasing MAX_LEVELS\n");
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

  if (lev_bucket->next_free_envmap_draw + model->max_draws >= lev_bucket->envmap_draws.size()) {
    // out of room, flush
    fmt::print("MERC2 out of envmap draws, consider increasing MAX_ENVMAP_DRAWS_PER_LEVEL\n");
    // or, use a more accurate max_draws for envmap.
    flush_draw_buckets(render_state, prof);
    // and retry the whole thing.
    flush_pending_model(render_state, prof);
    return;
  }

  u32 first_bone = alloc_bones(bone_count);

  // allocate lights
  u32 lights = alloc_lights(m_current_lights);
  //
  for (size_t ei = 0; ei < model->effects.size(); ei++) {
    if (!(m_current_effect_enable_bits & (1 << ei))) {
      continue;
    }

    u8 ignore_alpha = (m_current_ignore_alpha_bits & (1 << ei));
    auto& effect = model->effects[ei];
    if (effect.has_envmap) {
      bool nonzero_fade = false;
      for (int i = 0; i < 4; i++) {
        if (m_fade_buffer[4 * ei + i]) {
          nonzero_fade = true;
          break;
        }
      }
      if (nonzero_fade) {
        for (auto& mdraw : effect.all_draws) {
          Draw* draw = &lev_bucket->envmap_draws[lev_bucket->next_free_envmap_draw++];
          draw->flags = 0;
          draw->first_index = mdraw.first_index;
          draw->index_count = mdraw.index_count;
          draw->mode = effect.envmap_mode;
          draw->texture = effect.envmap_texture;
          draw->first_bone = first_bone;
          draw->light_idx = lights;
          draw->num_triangles = mdraw.num_triangles;
          for (int i = 0; i < 4; i++) {
            draw->fade[i] = m_fade_buffer[4 * ei + i];
          }
        }
      }
    }

    if (m_current_model_updates_verts) {
      for (auto& mdraw : effect.mod.fix_draw) {
        alloc_normal_draw(mdraw, ignore_alpha, lev_bucket, first_bone, lights);
      }

      for (auto& mdraw : effect.mod.mod_draw) {
        auto* draw = alloc_normal_draw(mdraw, ignore_alpha, lev_bucket, first_bone, lights);
        draw->flags |= MOD_VTX;
        draw->mod_vtx_buffer = m_mod_opengl_buffers[ei];
      }
    } else {
      for (auto& draw : effect.all_draws) {
        alloc_normal_draw(draw, ignore_alpha, lev_bucket, first_bone, lights);
      }
    }
  }

  m_current_model = std::nullopt;
}

Merc2::Draw* Merc2::alloc_normal_draw(const tfrag3::MercDraw& mdraw,
                                      bool ignore_alpha,
                                      LevelDrawBucket* lev_bucket,
                                      u32 first_bone,
                                      u32 lights) {
  Draw* draw = &lev_bucket->draws[lev_bucket->next_free_draw++];
  draw->flags = 0;
  draw->first_index = mdraw.first_index;
  draw->index_count = mdraw.index_count;
  draw->mode = mdraw.mode;
  draw->texture = mdraw.tree_tex_id;
  draw->first_bone = first_bone;
  draw->light_idx = lights;
  draw->num_triangles = mdraw.num_triangles;
  if (ignore_alpha) {
    draw->flags |= IGNORE_ALPHA;
  }
  for (int i = 0; i < 4; i++) {
    draw->fade[i] = 0;
  }
  return draw;
}

void Merc2::setup_merc_vao() {
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
                        4,                                            // 3 values per vert
                        GL_UNSIGNED_BYTE,                             // floats
                        GL_TRUE,                                      // normalized
                        sizeof(tfrag3::MercVertex),                   // stride
                        (void*)offsetof(tfrag3::MercVertex, rgba[0])  // offset (0)
  );

  glVertexAttribIPointer(5,                                            // location 0 in the
                         4,                                            // 3 floats per vert
                         GL_UNSIGNED_BYTE,                             // u8's
                         sizeof(tfrag3::MercVertex),                   //
                         (void*)offsetof(tfrag3::MercVertex, mats[0])  // offset in array
  );
}

void Merc2::flush_draw_buckets(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  m_stats.num_draw_flush++;
  for (u32 li = 0; li < m_next_free_level_bucket; li++) {
    const auto& lev_bucket = m_level_draw_buckets[li];
    const auto* lev = lev_bucket.level;
    glBindVertexArray(m_vao);
    glBindBuffer(GL_ARRAY_BUFFER, lev->merc_vertices);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, lev->merc_indices);
    setup_merc_vao();
    m_stats.num_bones_uploaded += m_next_free_bone_vector;

    glBindBuffer(GL_UNIFORM_BUFFER, m_bones_buffer);
    glBufferSubData(GL_UNIFORM_BUFFER, 0, m_next_free_bone_vector * sizeof(math::Vector4f),
                    m_shader_bone_vector_buffer);
    glBindBuffer(GL_UNIFORM_BUFFER, 0);

    switch_to_merc2(render_state);
    do_draws(lev_bucket.draws.data(), lev, lev_bucket.next_free_draw, m_merc_uniforms, prof, false,
             render_state);
    if (lev_bucket.next_free_envmap_draw) {
      switch_to_emerc(render_state);
      do_draws(lev_bucket.envmap_draws.data(), lev, lev_bucket.next_free_envmap_draw,
               m_emerc_uniforms, prof, true, render_state);
    }
  }

  m_next_free_light = 0;
  m_next_free_bone_vector = 0;
  m_next_free_level_bucket = 0;
  m_next_mod_vtx_buffer = 0;
}

void Merc2::do_draws(const Draw* draw_array,
                     const LevelData* lev,
                     u32 num_draws,
                     const Uniforms& uniforms,
                     ScopedProfilerNode& prof,
                     bool set_fade,
                     SharedRenderState* render_state) {
  int last_tex = -1;
  int last_light = -1;
  bool normal_vtx_buffer_bound = true;
  for (u32 di = 0; di < num_draws; di++) {
    auto& draw = draw_array[di];
    if (draw.flags & MOD_VTX) {
      glBindVertexArray(draw.mod_vtx_buffer.vao);
      normal_vtx_buffer_bound = false;
    } else {
      if (!normal_vtx_buffer_bound) {
        glBindVertexArray(m_vao);
        normal_vtx_buffer_bound = true;
      }
    }
    glUniform1i(uniforms.ignore_alpha, draw.flags & DrawFlags::IGNORE_ALPHA);
    if ((int)draw.texture != last_tex) {
      if (draw.texture < lev->textures.size()) {
        glBindTexture(GL_TEXTURE_2D, lev->textures.at(draw.texture));
      } else {
        fmt::print("Invalid draw.texture is {}, would have crashed.\n", draw.texture);
      }
      last_tex = draw.texture;
    }

    if ((int)draw.light_idx != last_light && !set_fade) {
      set_uniform(uniforms.light_direction[0], m_lights_buffer[draw.light_idx].direction0);
      set_uniform(uniforms.light_direction[1], m_lights_buffer[draw.light_idx].direction1);
      set_uniform(uniforms.light_direction[2], m_lights_buffer[draw.light_idx].direction2);
      set_uniform(uniforms.light_color[0], m_lights_buffer[draw.light_idx].color0);
      set_uniform(uniforms.light_color[1], m_lights_buffer[draw.light_idx].color1);
      set_uniform(uniforms.light_color[2], m_lights_buffer[draw.light_idx].color2);
      set_uniform(uniforms.light_ambient, m_lights_buffer[draw.light_idx].ambient);
      last_light = draw.light_idx;
    }
    setup_opengl_from_draw_mode(draw.mode, GL_TEXTURE0, true);

    glUniform1i(uniforms.decal, draw.mode.get_decal());

    if (set_fade) {
      math::Vector4f fade =
          math::Vector4f(draw.fade[0], draw.fade[1], draw.fade[2], draw.fade[3]) / 255.f;
      set_uniform(uniforms.fade, fade);
      ASSERT(draw.mode.get_alpha_blend() == DrawMode::AlphaBlend::SRC_0_DST_DST);
      // glBindTexture(GL_TEXTURE_2D, render_state->texture_pool->get_placeholder_texture());
    }

    prof.add_draw_call();
    prof.add_tri(draw.num_triangles);
    glBindBufferRange(GL_UNIFORM_BUFFER, 1, m_bones_buffer,
                      sizeof(math::Vector4f) * draw.first_bone, 128 * sizeof(ShaderMercMat));
    glDrawElements(GL_TRIANGLE_STRIP, draw.index_count, GL_UNSIGNED_INT,
                   (void*)(sizeof(u32) * draw.first_index));
  }

  if (!normal_vtx_buffer_bound) {
    glBindVertexArray(m_vao);
  }
}