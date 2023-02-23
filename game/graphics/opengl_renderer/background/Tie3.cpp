#include "Tie3.h"

#include "common/global_profiler/GlobalProfiler.h"
#include "common/log/log.h"
#include "common/util/Assert.h"

#include "third-party/imgui/imgui.h"

Tie3::Tie3(const std::string& name, int my_id, int level_id)
    : BucketRenderer(name, my_id), m_level_id(level_id) {
  // regardless of how many we use some fixed max
  // we won't actually interp or upload to gpu the unused ones, but we need a fixed maximum so
  // indexing works properly.
  m_color_result.resize(TIME_OF_DAY_COLOR_COUNT);

  m_wind_data.paused = 0;
  math::Vector4f ones(1, 1, 1, 1);
  m_wind_data.wind_normal = ones;
  m_wind_data.wind_temp = ones;
  for (auto& wv : m_wind_data.wind_array) {
    wv = ones;
  }
  for (auto& wf : m_wind_data.wind_force) {
    wf = 1.f;
  }
}

Tie3::~Tie3() {
  discard_tree_cache();
}

void Tie3::update_load(const LevelData* loader_data) {
  auto ul = scoped_prof("update-load");
  const tfrag3::Level* lev_data = loader_data->level.get();
  m_wind_vectors.clear();
  // We changed level!
  discard_tree_cache();
  for (int geo = 0; geo < 4; ++geo) {
    m_trees[geo].resize(lev_data->tie_trees[geo].size());
  }

  size_t vis_temp_len = 0;
  size_t max_draws = 0;
  size_t max_num_grps = 0;
  u16 max_wind_idx = 0;
  size_t time_of_day_count = 0;
  size_t max_inds = 0;
  for (u32 l_geo = 0; l_geo < tfrag3::TIE_GEOS; l_geo++) {
    for (u32 l_tree = 0; l_tree < lev_data->tie_trees[l_geo].size(); l_tree++) {
      auto ul = scoped_prof("load-tree");
      size_t wind_idx_buffer_len = 0;
      size_t num_grps = 0;
      const auto& tree = lev_data->tie_trees[l_geo][l_tree];
      max_draws = std::max(tree.static_draws.size(), max_draws);
      for (auto& draw : tree.static_draws) {
        num_grps += draw.vis_groups.size();
      }
      max_num_grps = std::max(max_num_grps, num_grps);
      for (auto& draw : tree.instanced_wind_draws) {
        wind_idx_buffer_len += draw.vertex_index_stream.size();
      }
      for (auto& inst : tree.wind_instance_info) {
        max_wind_idx = std::max(max_wind_idx, inst.wind_idx);
      }
      time_of_day_count = std::max(tree.colors.size(), time_of_day_count);
      max_inds = std::max(tree.unpacked.indices.size(), max_inds);
      u32 verts = tree.packed_vertices.color_indices.size();
      auto& lod_tree = m_trees.at(l_geo);
      glGenVertexArrays(1, &lod_tree[l_tree].vao);
      glBindVertexArray(lod_tree[l_tree].vao);
      lod_tree[l_tree].vertex_buffer = loader_data->tie_data[l_geo][l_tree].vertex_buffer;
      lod_tree[l_tree].vert_count = verts;
      lod_tree[l_tree].draws = &tree.static_draws;
      lod_tree[l_tree].colors = &tree.colors;
      lod_tree[l_tree].vis = &tree.bvh;
      lod_tree[l_tree].index_data = tree.unpacked.indices.data();
      lod_tree[l_tree].instance_info = &tree.wind_instance_info;
      lod_tree[l_tree].wind_draws = &tree.instanced_wind_draws;
      vis_temp_len = std::max(vis_temp_len, tree.bvh.vis_nodes.size());
      lod_tree[l_tree].tod_cache = swizzle_time_of_day(tree.colors);
      glBindBuffer(GL_ARRAY_BUFFER, lod_tree[l_tree].vertex_buffer);
      glEnableVertexAttribArray(0);
      glEnableVertexAttribArray(1);
      glEnableVertexAttribArray(2);

      glVertexAttribPointer(0,                                           // location 0 in the shader
                            3,                                           // 3 values per vert
                            GL_FLOAT,                                    // floats
                            GL_FALSE,                                    // normalized
                            sizeof(tfrag3::PreloadedVertex),             // stride
                            (void*)offsetof(tfrag3::PreloadedVertex, x)  // offset (0)
      );

      glVertexAttribPointer(1,                                           // location 1 in the shader
                            3,                                           // 3 values per vert
                            GL_FLOAT,                                    // floats
                            GL_FALSE,                                    // normalized
                            sizeof(tfrag3::PreloadedVertex),             // stride
                            (void*)offsetof(tfrag3::PreloadedVertex, s)  // offset (0)
      );

      glVertexAttribIPointer(2,                                // location 2 in the shader
                             1,                                // 1 values per vert
                             GL_UNSIGNED_SHORT,                // u16
                             sizeof(tfrag3::PreloadedVertex),  // stride
                             (void*)offsetof(tfrag3::PreloadedVertex, color_index)  // offset (0)
      );

      glGenBuffers(1, &lod_tree[l_tree].single_draw_index_buffer);
      lod_tree[l_tree].index_buffer = loader_data->tie_data[l_geo][l_tree].index_buffer;

      if (wind_idx_buffer_len > 0) {
        lod_tree[l_tree].wind_matrix_cache.resize(tree.wind_instance_info.size());
        lod_tree[l_tree].has_wind = true;
        lod_tree[l_tree].wind_vertex_index_buffer =
            loader_data->tie_data[l_geo][l_tree].wind_indices;
        u32 off = 0;
        for (auto& draw : tree.instanced_wind_draws) {
          lod_tree[l_tree].wind_vertex_index_offsets.push_back(off);
          off += draw.vertex_index_stream.size();
        }
      }

      lod_tree[l_tree].has_proto_visibility = tree.has_per_proto_visibility_toggle;
      if (tree.has_per_proto_visibility_toggle) {
        lod_tree[l_tree].proto_visibility.init(tree.proto_names);
      }

      glActiveTexture(GL_TEXTURE10);
      glGenTextures(1, &lod_tree[l_tree].time_of_day_texture);
      glBindTexture(GL_TEXTURE_1D, lod_tree[l_tree].time_of_day_texture);
      glTexImage1D(GL_TEXTURE_1D, 0, GL_RGBA, TIME_OF_DAY_COLOR_COUNT, 0, GL_RGBA,
                   GL_UNSIGNED_INT_8_8_8_8, nullptr);
      glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

      glBindVertexArray(0);
    }
  }

  m_cache.vis_temp.resize(vis_temp_len);
  m_cache.multidraw_offset_per_stripdraw.resize(max_draws);
  m_cache.multidraw_count_buffer.resize(max_num_grps);
  m_cache.multidraw_index_offset_buffer.resize(max_num_grps);
  m_wind_vectors.resize(4 * max_wind_idx + 4);  // 4x u32's per wind.
  m_cache.draw_idx_temp.resize(max_draws);
  m_cache.index_temp.resize(max_inds);

  ASSERT(time_of_day_count <= TIME_OF_DAY_COLOR_COUNT);
}

/*!
 * Set up all OpenGL and temporary buffers for a given level name.
 * The level name should be the 3 character short name.
 */
bool Tie3::setup_for_level(const std::string& level, SharedRenderState* render_state) {
  // make sure we have the level data.
  Timer tfrag3_setup_timer;
  auto lev_data = render_state->loader->get_tfrag3_level(level);
  if (!lev_data || (m_has_level && lev_data->load_id != m_load_id)) {
    m_has_level = false;
    m_textures = nullptr;
    m_level_name = "";
    discard_tree_cache();
    return false;
  }
  m_textures = &lev_data->textures;
  m_load_id = lev_data->load_id;

  if (m_level_name != level) {
    update_load(lev_data);
    m_has_level = true;
    m_level_name = level;
  } else {
    m_has_level = true;
  }

  if (tfrag3_setup_timer.getMs() > 5) {
    lg::info("TIE setup: {:.1f}ms", tfrag3_setup_timer.getMs());
  }

  return m_has_level;
}

void vector_min_in_place(math::Vector4f& v, float val) {
  for (int i = 0; i < 4; i++) {
    if (v[i] > val) {
      v[i] = val;
    }
  }
}

math::Vector4f vector_max(const math::Vector4f& v, float val) {
  math::Vector4f result;
  for (int i = 0; i < 4; i++) {
    result[i] = std::max(val, v[i]);
  }
  return result;
}

void do_wind_math(u16 wind_idx,
                  float* wind_vector_data,
                  const Tie3::WindWork& wind_work,
                  float stiffness,
                  std::array<math::Vector4f, 4>& mat) {
  float* my_vector = wind_vector_data + (4 * wind_idx);
  const auto& work_vector = wind_work.wind_array[(wind_work.wind_time + wind_idx) & 63];
  constexpr float cx = 0.5;
  constexpr float cy = 100.0;
  constexpr float cz = 0.0166;
  constexpr float cw = -1.0;

  // ld s1, 8(s5)                    # load wind vector 1
  // pextlw s1, r0, s1               # convert to 2x 64 bits, by shifting left
  // qmtc2.i vf18, s1                # put in vf
  float vf18_x = my_vector[2];
  float vf18_z = my_vector[3];

  // ld s2, 0(s5)                    # load wind vector 0
  // pextlw s3, r0, s2               # convert to 2x 64 bits, by shifting left
  // qmtc2.i vf17, s3                # put in vf
  float vf17_x = my_vector[0];
  float vf17_z = my_vector[1];

  // lqc2 vf16, 12(s3)               # load wind vector
  math::Vector4f vf16 = work_vector;

  // vmula.xyzw acc, vf16, vf1       # acc = vf16
  // vmsubax.xyzw acc, vf18, vf19    # acc = vf16 - vf18 * wind_const.x
  // vmsuby.xyzw vf16, vf17, vf19
  // # vf16 -= (vf18 * wind_const.x) + (vf17 * wind_const.y)
  vf16.x() -= cx * vf18_x + cy * vf17_x;
  vf16.z() -= cx * vf18_z + cy * vf17_z;

  // vmulaz.xyzw acc, vf16, vf19     # acc = vf16 * wind_const.z
  // vmadd.xyzw vf18, vf1, vf18
  // # vf18 += vf16 * wind_const.z
  math::Vector4f vf18(vf18_x, 0.f, vf18_z, 0.f);
  vf18 += vf16 * cz;

  // vmulaz.xyzw acc, vf18, vf19    # acc = vf18 * wind_const.z
  // vmadd.xyzw vf17, vf17, vf1
  // # vf17 += vf18 * wind_const.z
  math::Vector4f vf17(vf17_x, 0.f, vf17_z, 0.f);
  vf17 += vf18 * cz;

  // vitof12.xyzw vf11, vf11 # normal convert
  // vitof12.xyzw vf12, vf12 # normal convert

  // vminiw.xyzw vf17, vf17, vf0
  vector_min_in_place(vf17, 1.f);

  // qmfc2.i s3, vf18
  // ppacw s3, r0, s3

  // vmaxw.xyzw vf27, vf17, vf19
  auto vf27 = vector_max(vf17, cw);

  // vmulw.xyzw vf27, vf27, vf15
  vf27 *= stiffness;

  // vmulax.yw acc, vf0, vf0
  // vmulay.xz acc, vf27, vf10
  // vmadd.xyzw vf10, vf1, vf10
  mat[0].x() += vf27.x() * mat[0].y();
  mat[0].z() += vf27.z() * mat[0].y();

  // qmfc2.i s2, vf27
  if (!wind_work.paused) {
    my_vector[0] = vf27.x();
    my_vector[1] = vf27.z();
    my_vector[2] = vf18.x();
    my_vector[3] = vf18.z();
  }

  // vmulax.yw acc, vf0, vf0
  // vmulay.xz acc, vf27, vf11
  // vmadd.xyzw vf11, vf1, vf11
  mat[1].x() += vf27.x() * mat[1].y();
  mat[1].z() += vf27.z() * mat[1].y();

  // ppacw s2, r0, s2
  // vmulax.yw acc, vf0, vf0
  // vmulay.xz acc, vf27, vf12
  // vmadd.xyzw vf12, vf1, vf12
  mat[2].x() += vf27.x() * mat[2].y();
  mat[2].z() += vf27.z() * mat[2].y();

  //
  // if not paused
  // sd s3, 8(s5)
  // sd s2, 0(s5)
}

void Tie3::discard_tree_cache() {
  for (int geo = 0; geo < 4; ++geo) {
    for (auto& tree : m_trees[geo]) {
      glBindTexture(GL_TEXTURE_1D, tree.time_of_day_texture);
      glDeleteTextures(1, &tree.time_of_day_texture);
      // glDeleteBuffers(1, &tree.index_buffer);
      glDeleteBuffers(1, &tree.single_draw_index_buffer);
      glDeleteVertexArrays(1, &tree.vao);
    }

    m_trees[geo].clear();
  }
}

void Tie3::render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) {
  if (!m_enabled) {
    while (dma.current_tag_offset() != render_state->next_bucket) {
      dma.read_and_advance();
    }
    return;
  }

  if (m_override_level && m_pending_user_level) {
    m_has_level = setup_for_level(*m_pending_user_level, render_state);
    m_pending_user_level = {};
  }

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

  auto gs_test = dma.read_and_advance();
  if (gs_test.size_bytes == 160) {
  } else {
    ASSERT(gs_test.size_bytes == 32);

    auto tie_consts = dma.read_and_advance();
    ASSERT(tie_consts.size_bytes == 9 * 16);
  }

  auto mscalf = dma.read_and_advance();
  ASSERT(mscalf.size_bytes == 0);

  auto row = dma.read_and_advance();
  ASSERT(row.size_bytes == 32);

  auto next = dma.read_and_advance();
  if (next.size_bytes == 32) {
    next = dma.read_and_advance();
  }
  ASSERT(next.size_bytes == 0);

  auto pc_port_data = dma.read_and_advance();
  ASSERT(pc_port_data.size_bytes == sizeof(TfragPcPortData));
  memcpy(&m_pc_port_data, pc_port_data.data, sizeof(TfragPcPortData));
  m_pc_port_data.level_name[11] = '\0';

  if (render_state->version == GameVersion::Jak1) {
    auto wind_data = dma.read_and_advance();
    ASSERT(wind_data.size_bytes == sizeof(WindWork));
    memcpy(&m_wind_data, wind_data.data, sizeof(WindWork));
  }

  const u8* proto_vis_data = nullptr;
  size_t proto_vis_data_size = 0;
  if (render_state->version == GameVersion::Jak2) {
    auto proto_mask_data = dma.read_and_advance();
    proto_vis_data = proto_mask_data.data;
    proto_vis_data_size = proto_mask_data.size_bytes;
  }

  while (dma.current_tag_offset() != render_state->next_bucket) {
    dma.read_and_advance();
  }

  TfragRenderSettings settings;
  settings.hvdf_offset = m_pc_port_data.hvdf_off;
  settings.fog = m_pc_port_data.fog;

  memcpy(settings.math_camera.data(), m_pc_port_data.camera[0].data(), 64);
  settings.tree_idx = 0;

  if (render_state->occlusion_vis[m_level_id].valid) {
    settings.occlusion_culling = render_state->occlusion_vis[m_level_id].data;
  }

  update_render_state_from_pc_settings(render_state, m_pc_port_data);

  for (int i = 0; i < 4; i++) {
    settings.planes[i] = m_pc_port_data.planes[i];
    settings.itimes[i] = m_pc_port_data.itimes[i];
  }

  if (!m_override_level) {
    m_has_level = setup_for_level(m_pc_port_data.level_name, render_state);
  }

  render_all_trees(lod(), settings, render_state, prof, proto_vis_data, proto_vis_data_size);
}

void Tie3::render_all_trees(int geom,
                            const TfragRenderSettings& settings,
                            SharedRenderState* render_state,
                            ScopedProfilerNode& prof,
                            const u8* proto_vis_data,
                            size_t proto_vis_data_size) {
  Timer all_tree_timer;
  if (m_override_level && m_pending_user_level) {
    m_has_level = setup_for_level(*m_pending_user_level, render_state);
    m_pending_user_level = {};
  }
  for (u32 i = 0; i < m_trees[geom].size(); i++) {
    render_tree(i, geom, settings, render_state, prof, proto_vis_data, proto_vis_data_size);
  }
  m_all_tree_time.add(all_tree_timer.getSeconds());
}

void Tie3::render_tree_wind(int idx,
                            int geom,
                            const TfragRenderSettings& settings,
                            SharedRenderState* render_state,
                            ScopedProfilerNode& prof) {
  auto& tree = m_trees.at(geom).at(idx);
  if (tree.wind_draws->empty()) {
    return;
  }

  // note: this isn't the most efficient because we might compute wind matrices for invisible
  // instances. TODO: add vis ids to the instance info to avoid this
  memset(tree.wind_matrix_cache.data(), 0, sizeof(float) * 16 * tree.wind_matrix_cache.size());
  auto& cam_bad = settings.math_camera;
  std::array<math::Vector4f, 4> cam;
  for (int i = 0; i < 4; i++) {
    for (int j = 0; j < 4; j++) {
      cam[i][j] = cam_bad.data()[i * 4 + j];
    }
  }

  for (size_t inst_id = 0; inst_id < tree.instance_info->size(); inst_id++) {
    auto& info = tree.instance_info->operator[](inst_id);
    auto& out = tree.wind_matrix_cache[inst_id];
    // auto& mat = tree.instance_info->operator[](inst_id).matrix;
    auto mat = info.matrix;

    ASSERT(info.wind_idx * 4 <= m_wind_vectors.size());
    do_wind_math(info.wind_idx, m_wind_vectors.data(), m_wind_data,
                 info.stiffness * m_wind_multiplier, mat);

    // vmulax.xyzw acc, vf20, vf10
    // vmadday.xyzw acc, vf21, vf10
    // vmaddz.xyzw vf10, vf22, vf10
    out[0] = cam[0] * mat[0].x() + cam[1] * mat[0].y() + cam[2] * mat[0].z();

    // vmulax.xyzw acc, vf20, vf11
    // vmadday.xyzw acc, vf21, vf11
    // vmaddz.xyzw vf11, vf22, vf11
    out[1] = cam[0] * mat[1].x() + cam[1] * mat[1].y() + cam[2] * mat[1].z();

    // vmulax.xyzw acc, vf20, vf12
    // vmadday.xyzw acc, vf21, vf12
    // vmaddz.xyzw vf12, vf22, vf12
    out[2] = cam[0] * mat[2].x() + cam[1] * mat[2].y() + cam[2] * mat[2].z();

    // vmulax.xyzw acc, vf20, vf13
    // vmadday.xyzw acc, vf21, vf13
    // vmaddaz.xyzw acc, vf22, vf13
    // vmaddw.xyzw vf13, vf23, vf0
    out[3] = cam[0] * mat[3].x() + cam[1] * mat[3].y() + cam[2] * mat[3].z() + cam[3];
  }

  int last_texture = -1;
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, tree.wind_vertex_index_buffer);

  for (size_t draw_idx = 0; draw_idx < tree.wind_draws->size(); draw_idx++) {
    const auto& draw = tree.wind_draws->operator[](draw_idx);

    if ((int)draw.tree_tex_id != last_texture) {
      glBindTexture(GL_TEXTURE_2D, m_textures->at(draw.tree_tex_id));
      last_texture = draw.tree_tex_id;
    }
    auto double_draw = setup_tfrag_shader(render_state, draw.mode, ShaderId::TFRAG3);

    int off = 0;
    for (auto& grp : draw.instance_groups) {
      if (!m_debug_all_visible && !m_cache.vis_temp.at(grp.vis_idx)) {
        off += grp.num;
        continue;  // invisible, skip.
      }

      glUniformMatrix4fv(
          glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "camera"), 1, GL_FALSE,
          tree.wind_matrix_cache.at(grp.instance_idx)[0].data());

      prof.add_draw_call();
      prof.add_tri(grp.num);

      tree.perf.draws++;
      tree.perf.wind_draws++;

      glDrawElements(GL_TRIANGLE_STRIP, grp.num, GL_UNSIGNED_INT,
                     (void*)((off + tree.wind_vertex_index_offsets.at(draw_idx)) * sizeof(u32)));
      off += grp.num;

      switch (double_draw.kind) {
        case DoubleDrawKind::NONE:
          break;
        case DoubleDrawKind::AFAIL_NO_DEPTH_WRITE:
          tree.perf.draws++;
          tree.perf.wind_draws++;
          prof.add_draw_call();
          prof.add_tri(grp.num);
          glUniform1f(
              glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "alpha_min"),
              -10.f);
          glUniform1f(
              glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "alpha_max"),
              double_draw.aref_second);
          glDepthMask(GL_FALSE);
          glDrawElements(GL_TRIANGLE_STRIP, draw.vertex_index_stream.size(), GL_UNSIGNED_INT,
                         (void*)0);
          break;
        default:
          ASSERT(false);
      }
    }
  }
}

void Tie3::render_tree(int idx,
                       int geom,
                       const TfragRenderSettings& settings,
                       SharedRenderState* render_state,
                       ScopedProfilerNode& prof,
                       const u8* proto_vis_data,
                       size_t proto_vis_data_size) {
  // reset perf
  Timer tree_timer;
  auto& tree = m_trees.at(geom).at(idx);
  tree.perf.draws = 0;
  tree.perf.wind_draws = 0;

  // don't render if we haven't loaded
  if (!m_has_level) {
    return;
  }

  // update time of day
  if (m_color_result.size() < tree.colors->size()) {
    m_color_result.resize(tree.colors->size());
  }

  // update proto vis mask
  if (proto_vis_data) {
    tree.proto_visibility.update(proto_vis_data, proto_vis_data_size);
  }

  Timer interp_timer;
  if (m_use_fast_time_of_day) {
    interp_time_of_day_fast(settings.itimes, tree.tod_cache, m_color_result.data());
  } else {
    interp_time_of_day_slow(settings.itimes, *tree.colors, m_color_result.data());
  }
  tree.perf.tod_time.add(interp_timer.getSeconds());

  Timer setup_timer;
  glActiveTexture(GL_TEXTURE10);
  glBindTexture(GL_TEXTURE_1D, tree.time_of_day_texture);
  glTexSubImage1D(GL_TEXTURE_1D, 0, 0, tree.colors->size(), GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
                  m_color_result.data());

  // setup OpenGL shader
  first_tfrag_draw_setup(settings, render_state, ShaderId::TFRAG3);

  glBindVertexArray(tree.vao);
  glBindBuffer(GL_ARRAY_BUFFER, tree.vertex_buffer);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,
               render_state->no_multidraw ? tree.single_draw_index_buffer : tree.index_buffer);
  glActiveTexture(GL_TEXTURE0);
  glEnable(GL_PRIMITIVE_RESTART);
  glPrimitiveRestartIndex(UINT32_MAX);
  tree.perf.tod_time.add(setup_timer.getSeconds());

  int last_texture = -1;

  if (!m_debug_all_visible) {
    // need culling data
    Timer cull_timer;
    cull_check_all_slow(settings.planes, tree.vis->vis_nodes, settings.occlusion_culling,
                        m_cache.vis_temp.data());
    tree.perf.cull_time.add(cull_timer.getSeconds());
  } else {
    // no culling.
    tree.perf.cull_time.add(0);
  }

  u32 num_tris;
  if (render_state->no_multidraw) {
    Timer index_timer;
    u32 idx_buffer_size;
    if (m_debug_all_visible) {
      idx_buffer_size =
          make_all_visible_index_list(m_cache.draw_idx_temp.data(), m_cache.index_temp.data(),
                                      *tree.draws, tree.index_data, &num_tris);
    } else {
      if (tree.has_proto_visibility) {
        idx_buffer_size = make_index_list_from_vis_and_proto_string(
            m_cache.draw_idx_temp.data(), m_cache.index_temp.data(), *tree.draws, m_cache.vis_temp,
            tree.proto_visibility.vis_flags, tree.index_data, &num_tris);
      } else {
        idx_buffer_size = make_index_list_from_vis_string(
            m_cache.draw_idx_temp.data(), m_cache.index_temp.data(), *tree.draws, m_cache.vis_temp,
            tree.index_data, &num_tris);
      }
    }

    glBufferData(GL_ELEMENT_ARRAY_BUFFER, idx_buffer_size * sizeof(u32), m_cache.index_temp.data(),
                 GL_STREAM_DRAW);
    tree.perf.index_time.add(index_timer.getSeconds());

  } else {
    if (m_debug_all_visible) {
      Timer index_timer;
      num_tris = make_all_visible_multidraws(
          m_cache.multidraw_offset_per_stripdraw.data(), m_cache.multidraw_count_buffer.data(),
          m_cache.multidraw_index_offset_buffer.data(), *tree.draws);
      tree.perf.index_time.add(index_timer.getSeconds());
    } else {
      Timer index_timer;
      if (tree.has_proto_visibility) {
        num_tris = make_multidraws_from_vis_and_proto_string(
            m_cache.multidraw_offset_per_stripdraw.data(), m_cache.multidraw_count_buffer.data(),
            m_cache.multidraw_index_offset_buffer.data(), *tree.draws, m_cache.vis_temp,
            tree.proto_visibility.vis_flags);
      } else {
        num_tris = make_multidraws_from_vis_string(
            m_cache.multidraw_offset_per_stripdraw.data(), m_cache.multidraw_count_buffer.data(),
            m_cache.multidraw_index_offset_buffer.data(), *tree.draws, m_cache.vis_temp);
      }

      tree.perf.index_time.add(index_timer.getSeconds());
    }
  }

  Timer draw_timer;
  prof.add_tri(num_tris);

  for (size_t draw_idx = 0; draw_idx < tree.draws->size(); draw_idx++) {
    const auto& draw = tree.draws->operator[](draw_idx);
    const auto& multidraw_indices = m_cache.multidraw_offset_per_stripdraw[draw_idx];
    const auto& singledraw_indices = m_cache.draw_idx_temp[draw_idx];

    if (render_state->no_multidraw) {
      if (singledraw_indices.second == 0) {
        continue;
      }
    } else {
      if (multidraw_indices.second == 0) {
        continue;
      }
    }

    if ((int)draw.tree_tex_id != last_texture) {
      glBindTexture(GL_TEXTURE_2D, m_textures->at(draw.tree_tex_id));
      last_texture = draw.tree_tex_id;
    }

    auto double_draw = setup_tfrag_shader(render_state, draw.mode, ShaderId::TFRAG3);

    prof.add_draw_call();

    tree.perf.draws++;

    if (render_state->no_multidraw) {
      glDrawElements(GL_TRIANGLE_STRIP, singledraw_indices.second, GL_UNSIGNED_INT,
                     (void*)(singledraw_indices.first * sizeof(u32)));
    } else {
      glMultiDrawElements(GL_TRIANGLE_STRIP,
                          &m_cache.multidraw_count_buffer[multidraw_indices.first], GL_UNSIGNED_INT,
                          &m_cache.multidraw_index_offset_buffer[multidraw_indices.first],
                          multidraw_indices.second);
    }

    switch (double_draw.kind) {
      case DoubleDrawKind::NONE:
        break;
      case DoubleDrawKind::AFAIL_NO_DEPTH_WRITE:
        tree.perf.draws++;
        prof.add_draw_call();
        glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "alpha_min"),
                    -10.f);
        glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "alpha_max"),
                    double_draw.aref_second);
        glDepthMask(GL_FALSE);
        if (render_state->no_multidraw) {
          glDrawElements(GL_TRIANGLE_STRIP, singledraw_indices.second, GL_UNSIGNED_INT,
                         (void*)(singledraw_indices.first * sizeof(u32)));
        } else {
          glMultiDrawElements(
              GL_TRIANGLE_STRIP, &m_cache.multidraw_count_buffer[multidraw_indices.first],
              GL_UNSIGNED_INT, &m_cache.multidraw_index_offset_buffer[multidraw_indices.first],
              multidraw_indices.second);
        }
        break;
      default:
        ASSERT(false);
    }

    if (m_debug_wireframe && !render_state->no_multidraw) {
      render_state->shaders[ShaderId::TFRAG3_NO_TEX].activate();
      glUniformMatrix4fv(
          glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3_NO_TEX].id(), "camera"), 1,
          GL_FALSE, settings.math_camera.data());
      glUniform4f(
          glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3_NO_TEX].id(), "hvdf_offset"),
          settings.hvdf_offset[0], settings.hvdf_offset[1], settings.hvdf_offset[2],
          settings.hvdf_offset[3]);
      glUniform1f(
          glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3_NO_TEX].id(), "fog_constant"),
          settings.fog.x());
      glDisable(GL_BLEND);
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
      glMultiDrawElements(GL_TRIANGLE_STRIP,
                          &m_cache.multidraw_count_buffer[multidraw_indices.first], GL_UNSIGNED_INT,
                          &m_cache.multidraw_index_offset_buffer[multidraw_indices.first],
                          multidraw_indices.second);
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
      prof.add_draw_call();
      render_state->shaders[ShaderId::TFRAG3].activate();
    }
  }

  if (!m_hide_wind) {
    auto wind_prof = prof.make_scoped_child("wind");
    render_tree_wind(idx, geom, settings, render_state, wind_prof);
  }

  glBindVertexArray(0);
  tree.perf.draw_time.add(draw_timer.getSeconds());
  tree.perf.tree_time.add(tree_timer.getSeconds());
}

void Tie3::draw_debug_window() {
  ImGui::InputText("Custom Level", m_user_level, sizeof(m_user_level));
  if (ImGui::Button("Go!")) {
    m_pending_user_level = m_user_level;
  }
  ImGui::Checkbox("Override level", &m_override_level);
  ImGui::Checkbox("Fast ToD", &m_use_fast_time_of_day);
  ImGui::Checkbox("Wireframe", &m_debug_wireframe);
  ImGui::SameLine();
  ImGui::Checkbox("All Visible", &m_debug_all_visible);
  ImGui::Checkbox("Hide Wind", &m_hide_wind);
  ImGui::SliderFloat("Wind Multiplier", &m_wind_multiplier, 0., 40.f);
  ImGui::Separator();
  for (u32 i = 0; i < m_trees[lod()].size(); i++) {
    auto& perf = m_trees[lod()][i].perf;
    ImGui::Text("Tree: %d", i);
    ImGui::Text("time of days: %d", (int)m_trees[lod()][i].colors->size());
    ImGui::Text("draw: %d", perf.draws);
    ImGui::Text("wind draw: %d", perf.wind_draws);
    ImGui::Text("total: %.2f", perf.tree_time.get());
    ImGui::Text("proto vis: %.2f", perf.proto_vis_time.get() * 1000.f);
    ImGui::Text("cull: %.2f index: %.2f tod: %.2f setup: %.2f draw: %.2f",
                perf.cull_time.get() * 1000.f, perf.index_time.get() * 1000.f,
                perf.tod_time.get() * 1000.f, perf.setup_time.get() * 1000.f,
                perf.draw_time.get() * 1000.f);
    ImGui::Separator();
  }
  ImGui::Text("All trees: %.2f", 1000.f * m_all_tree_time.get());
}

void TieProtoVisibility::init(const std::vector<std::string>& names) {
  vis_flags.resize(names.size());
  for (auto& x : vis_flags) {
    x = 1;
  }
  all_visible = true;
  name_to_idx.clear();
  size_t i = 0;
  for (auto& name : names) {
    name_to_idx[name].push_back(i++);
  }
}

void TieProtoVisibility::update(const u8* data, size_t size) {
  char name_buffer[256];  // ??

  if (!all_visible) {
    for (auto& x : vis_flags) {
      x = 1;
    }
    all_visible = true;
  }

  const u8* end = data + size;

  while (true) {
    int name_idx = 0;
    while (*data) {
      name_buffer[name_idx++] = *data;
      data++;
    }
    if (name_idx) {
      ASSERT(name_idx < 254);
      name_buffer[name_idx] = '\0';
      const auto& it = name_to_idx.find(name_buffer);
      if (it != name_to_idx.end()) {
        all_visible = false;
        for (auto x : name_to_idx.at(name_buffer)) {
          vis_flags[x] = 0;
        }
      }
    }

    while (*data == 0) {
      if (data >= end) {
        return;
      }
      data++;
    }
  }
}