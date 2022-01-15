#include "Tie3.h"

#include "third-party/imgui/imgui.h"

Tie3::Tie3(const std::string& name, BucketId my_id) : BucketRenderer(name, my_id) {
  // regardless of how many we use some fixed max
  // we won't actually interp or upload to gpu the unused ones, but we need a fixed maximum so
  // indexing works properly.
  m_color_result.resize(TIME_OF_DAY_COLOR_COUNT);
}

Tie3::~Tie3() {
  discard_tree_cache();
}

bool Tie3::update_load(const tfrag3::Level* lev_data) {
  switch (m_load_state.state) {
    case DISCARD_TREE:
      m_wind_vectors.clear();
      // We changed level!
      discard_tree_cache();
      m_trees.resize(lev_data->tie_trees.size());
      m_load_state.state = INIT_NEW_TREES;
      break;
    case INIT_NEW_TREES: {
      size_t time_of_day_count = 0;
      size_t vis_temp_len = 0;
      size_t max_draw = 0;
      size_t max_idx_per_draw = 0;
      u16 max_wind_idx = 0;

      // set up each tree
      for (size_t tree_idx = 0; tree_idx < lev_data->tie_trees.size(); tree_idx++) {
        size_t idx_buffer_len = 0;
        size_t wind_idx_buffer_len = 0;
        const auto& tree = lev_data->tie_trees[tree_idx];
        max_draw = std::max(tree.static_draws.size(), max_draw);
        for (auto& draw : tree.static_draws) {
          idx_buffer_len += draw.vertex_index_stream.size();
          max_idx_per_draw = std::max(max_idx_per_draw, draw.vertex_index_stream.size());
        }
        for (auto& draw : tree.instanced_wind_draws) {
          wind_idx_buffer_len += draw.vertex_index_stream.size();
          max_idx_per_draw = std::max(max_idx_per_draw, draw.vertex_index_stream.size());
        }
        for (auto& inst : tree.instance_info) {
          max_wind_idx = std::max(max_wind_idx, inst.wind_idx);
        }
        time_of_day_count = std::max(tree.colors.size(), time_of_day_count);
        u32 verts = tree.vertices.size();
        fmt::print("  tree {} has {} verts ({} kB) and {} draws\n", tree_idx, verts,
                   verts * sizeof(tfrag3::PreloadedVertex) / 1024.f, tree.static_draws.size());
        glGenVertexArrays(1, &m_trees[tree_idx].vao);
        glBindVertexArray(m_trees[tree_idx].vao);
        glGenBuffers(1, &m_trees[tree_idx].vertex_buffer);
        m_trees[tree_idx].vert_count = verts;
        m_trees[tree_idx].draws = &tree.static_draws;  // todo - should we just copy this?
        m_trees[tree_idx].colors = &tree.colors;
        m_trees[tree_idx].vis = &tree.bvh;
        m_trees[tree_idx].instance_info = &tree.instance_info;
        m_trees[tree_idx].wind_draws = &tree.instanced_wind_draws;
        vis_temp_len = std::max(vis_temp_len, tree.bvh.vis_nodes.size());
        m_trees[tree_idx].tod_cache = swizzle_time_of_day(tree.colors);
        glBindBuffer(GL_ARRAY_BUFFER, m_trees[tree_idx].vertex_buffer);
        glBufferData(GL_ARRAY_BUFFER, verts * sizeof(tfrag3::PreloadedVertex), nullptr,
                     GL_STATIC_DRAW);
        glEnableVertexAttribArray(0);
        glEnableVertexAttribArray(1);
        glEnableVertexAttribArray(2);

        //        glBufferSubData(GL_ARRAY_BUFFER, 0, verts * sizeof(tfrag3::PreloadedVertex),
        //                        tree.vertices.data());

        glVertexAttribPointer(0,                                // location 0 in the shader
                              3,                                // 3 values per vert
                              GL_FLOAT,                         // floats
                              GL_FALSE,                         // normalized
                              sizeof(tfrag3::PreloadedVertex),  // stride
                              (void*)offsetof(tfrag3::PreloadedVertex, x)  // offset (0)
        );

        glVertexAttribPointer(1,                                // location 1 in the shader
                              3,                                // 3 values per vert
                              GL_FLOAT,                         // floats
                              GL_FALSE,                         // normalized
                              sizeof(tfrag3::PreloadedVertex),  // stride
                              (void*)offsetof(tfrag3::PreloadedVertex, s)  // offset (0)
        );

        glVertexAttribIPointer(2,                                // location 2 in the shader
                               1,                                // 1 values per vert
                               GL_UNSIGNED_SHORT,                // u16
                               sizeof(tfrag3::PreloadedVertex),  // stride
                               (void*)offsetof(tfrag3::PreloadedVertex, color_index)  // offset (0)
        );

        glGenBuffers(1, &m_trees[tree_idx].index_buffer);
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_trees[tree_idx].index_buffer);
        glBufferData(GL_ELEMENT_ARRAY_BUFFER, idx_buffer_len * sizeof(u32), nullptr,
                     GL_STREAM_DRAW);
        m_trees[tree_idx].index_list.resize(idx_buffer_len);

        if (wind_idx_buffer_len > 0) {
          m_trees[tree_idx].wind_matrix_cache.resize(tree.instance_info.size());
          m_trees[tree_idx].has_wind = true;
          glGenBuffers(1, &m_trees[tree_idx].wind_vertex_index_buffer);
          glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_trees[tree_idx].wind_vertex_index_buffer);
          std::vector<u32> temp;
          temp.resize(wind_idx_buffer_len);
          u32 off = 0;
          for (auto& draw : tree.instanced_wind_draws) {
            m_trees[tree_idx].wind_vertex_index_offsets.push_back(off);
            memcpy(temp.data() + off, draw.vertex_index_stream.data(),
                   draw.vertex_index_stream.size() * sizeof(u32));
            off += draw.vertex_index_stream.size();
          }

          glBufferData(GL_ELEMENT_ARRAY_BUFFER, wind_idx_buffer_len * sizeof(u32), temp.data(),
                       GL_STATIC_DRAW);
        }

        glActiveTexture(GL_TEXTURE10);
        glGenTextures(1, &m_trees[tree_idx].time_of_day_texture);
        glBindTexture(GL_TEXTURE_1D, m_trees[tree_idx].time_of_day_texture);
        // just fill with zeros. this lets use use the faster texsubimage later
        glTexImage1D(GL_TEXTURE_1D, 0, GL_RGBA, TIME_OF_DAY_COLOR_COUNT, 0, GL_RGBA,
                     GL_UNSIGNED_INT_8_8_8_8, m_color_result.data());
        glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

        glBindVertexArray(0);
      }

      fmt::print("TIE temporary vis output size: {}\n", vis_temp_len);
      m_cache.vis_temp.resize(vis_temp_len);
      fmt::print("TIE max draws/tree: {}\n", max_draw);
      m_cache.draw_idx_temp.resize(max_draw);
      fmt::print("TIE draw with the most verts: {}\n", max_idx_per_draw);
      fmt::print("wind: {}\n", max_wind_idx);
      m_wind_vectors.resize(4 * max_wind_idx + 4);  // 4x u32's per wind.
      fmt::print("level max time of day: {}\n", time_of_day_count);
      assert(time_of_day_count <= TIME_OF_DAY_COLOR_COUNT);
    }
      m_load_state.state = UPLOAD_VERTS;
      m_load_state.vert = 0;
      break;

    case State::UPLOAD_VERTS: {
      constexpr u32 MAX_VERTS = 40000;
      bool remaining = false;
      for (size_t tree_idx = 0; tree_idx < lev_data->tie_trees.size(); tree_idx++) {
        const auto& tree = lev_data->tie_trees[tree_idx];
        u32 verts = tree.vertices.size();
        u32 start_vert = (m_load_state.vert) * MAX_VERTS;
        u32 end_vert = std::min(verts, (m_load_state.vert + 1) * MAX_VERTS);
        if (end_vert > start_vert) {
          glBindVertexArray(m_trees[tree_idx].vao);
          glBindBuffer(GL_ARRAY_BUFFER, m_trees[tree_idx].vertex_buffer);
          glBufferSubData(GL_ARRAY_BUFFER, start_vert * sizeof(tfrag3::PreloadedVertex),
                          (end_vert - start_vert) * sizeof(tfrag3::PreloadedVertex),
                          tree.vertices.data() + start_vert);
          if (end_vert < verts) {
            remaining = true;
          }
        }
      }
      m_load_state.vert++;
      if (!remaining) {
        m_load_state.state = INIT_TEX;
        m_load_state.tex = 0;
      }
    } break;

    case State::INIT_TEX:
      for (size_t max_tex = std::min((size_t)m_load_state.tex + 3, lev_data->textures.size());
           m_load_state.tex < max_tex; m_load_state.tex++) {
        auto& tex = lev_data->textures[m_load_state.tex];
        GLuint gl_tex;
        glGenTextures(1, &gl_tex);
        glBindTexture(GL_TEXTURE_2D, gl_tex);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tex.w, tex.h, 0, GL_RGBA,
                     GL_UNSIGNED_INT_8_8_8_8_REV, tex.data.data());
        glBindTexture(GL_TEXTURE_2D, 0);
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D, gl_tex);
        glGenerateMipmap(GL_TEXTURE_2D);

        float aniso = 0.0f;
        glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY, &aniso);
        glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY, aniso);
        m_textures.push_back(gl_tex);
      }
      return m_load_state.tex == lev_data->textures.size();
      break;
    default:
      assert(false);
  }

  return false;
}
/*!
 * Set up all OpenGL and temporary buffers for a given level name.
 * The level name should be the 3 character short name.
 */
bool Tie3::setup_for_level(const std::string& level, SharedRenderState* render_state) {
  // make sure we have the level data.
  // TODO: right now this will wait to load from disk and unpack it.
  Timer tfrag3_setup_timer;
  auto lev_data = render_state->loader.get_tfrag3_level(level);
  if (!lev_data) {
    return false;
  }
  int init_load_state = m_load_state.state;

  if (m_level_name != level) {
    m_has_level = false;
    if (!m_load_state.loading) {
      m_load_state.loading = true;
      m_load_state.state = State::FIRST;
    }
    if (update_load(lev_data)) {
      m_has_level = true;
      m_level_name = level;
      m_load_state.loading = false;
    }
  } else {
    m_has_level = true;
  }

  if (tfrag3_setup_timer.getMs() > 5) {
    fmt::print("TIE setup: {:.1f}ms s {}\n", tfrag3_setup_timer.getMs(), init_load_state);
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
  //# vf16 -= (vf18 * wind_const.x) + (vf17 * wind_const.y)
  vf16.x() -= cx * vf18_x + cy * vf17_x;
  vf16.z() -= cx * vf18_z + cy * vf17_z;

  // vmulaz.xyzw acc, vf16, vf19     # acc = vf16 * wind_const.z
  // vmadd.xyzw vf18, vf1, vf18
  //# vf18 += vf16 * wind_const.z
  math::Vector4f vf18(vf18_x, 0.f, vf18_z, 0.f);
  vf18 += vf16 * cz;

  // vmulaz.xyzw acc, vf18, vf19    # acc = vf18 * wind_const.z
  // vmadd.xyzw vf17, vf17, vf1
  //# vf17 += vf18 * wind_const.z
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
  for (auto tex : m_textures) {
    glBindTexture(GL_TEXTURE_2D, tex);
    glDeleteTextures(1, &tex);
  }
  m_textures.clear();

  for (auto& tree : m_trees) {
    glBindTexture(GL_TEXTURE_1D, tree.time_of_day_texture);
    glDeleteTextures(1, &tree.time_of_day_texture);
    glDeleteBuffers(1, &tree.vertex_buffer);
    glDeleteBuffers(1, &tree.index_buffer);
    glDeleteVertexArrays(1, &tree.vao);
    if (tree.has_wind) {
      glDeleteBuffers(1, &tree.wind_vertex_index_buffer);
    }
  }

  m_trees.clear();
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
  assert(data0.vif1() == 0);
  assert(data0.vif0() == 0);
  assert(data0.size_bytes == 0);

  if (dma.current_tag().kind == DmaTag::Kind::CALL) {
    // renderer didn't run, let's just get out of here.
    for (int i = 0; i < 4; i++) {
      dma.read_and_advance();
    }
    assert(dma.current_tag_offset() == render_state->next_bucket);
    return;
  }

  auto gs_test = dma.read_and_advance();
  assert(gs_test.size_bytes == 32);

  auto tie_consts = dma.read_and_advance();
  assert(tie_consts.size_bytes == 9 * 16);

  auto mscalf = dma.read_and_advance();
  assert(mscalf.size_bytes == 0);

  auto row = dma.read_and_advance();
  assert(row.size_bytes == 32);

  auto next = dma.read_and_advance();
  assert(next.size_bytes == 0);

  auto pc_port_data = dma.read_and_advance();
  assert(pc_port_data.size_bytes == sizeof(TfragPcPortData));
  memcpy(&m_pc_port_data, pc_port_data.data, sizeof(TfragPcPortData));
  m_pc_port_data.level_name[11] = '\0';

  auto wind_data = dma.read_and_advance();
  assert(wind_data.size_bytes == sizeof(WindWork));
  memcpy(&m_wind_data, wind_data.data, sizeof(WindWork));

  while (dma.current_tag_offset() != render_state->next_bucket) {
    dma.read_and_advance();
  }

  TfragRenderSettings settings;
  settings.hvdf_offset = m_pc_port_data.hvdf_off;
  settings.fog_x = m_pc_port_data.fogx;

  memcpy(settings.math_camera.data(), m_pc_port_data.camera[0].data(), 64);
  settings.tree_idx = 0;

  for (int i = 0; i < 4; i++) {
    settings.planes[i] = m_pc_port_data.planes[i];
  }

  if (false) {
    //    for (int i = 0; i < 8; i++) {
    //      settings.time_of_day_weights[i] = m_time_of_days[i];
    //    }
  } else {
    for (int i = 0; i < 8; i++) {
      settings.time_of_day_weights[i] =
          2 * (0xff & m_pc_port_data.itimes[i / 2].data()[2 * (i % 2)]) / 127.f;
    }
  }
  if (!m_override_level) {
    m_has_level = setup_for_level(m_pc_port_data.level_name, render_state);
  }
  render_all_trees(settings, render_state, prof);
  // todo render all...
}

void Tie3::render_all_trees(const TfragRenderSettings& settings,
                            SharedRenderState* render_state,
                            ScopedProfilerNode& prof) {
  Timer all_tree_timer;
  if (m_override_level && m_pending_user_level) {
    m_has_level = setup_for_level(*m_pending_user_level, render_state);
    m_pending_user_level = {};
  }
  for (u32 i = 0; i < m_trees.size(); i++) {
    render_tree(i, settings, render_state, prof);
  }
  m_all_tree_time.add(all_tree_timer.getSeconds());
}

void Tie3::render_tree_wind(int idx,
                            const TfragRenderSettings& settings,
                            SharedRenderState* render_state,
                            ScopedProfilerNode& prof) {
  auto& tree = m_trees.at(idx);
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

    assert(info.wind_idx * 4 <= m_wind_vectors.size());
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
      glBindTexture(GL_TEXTURE_2D, m_textures.at(draw.tree_tex_id));
      last_texture = draw.tree_tex_id;
    }
    auto double_draw = setup_tfrag_shader(settings, render_state, draw.mode);

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
      tree.perf.verts += grp.num;

      glDrawElements(GL_TRIANGLE_STRIP, grp.num, GL_UNSIGNED_INT,
                     (void*)((off + tree.wind_vertex_index_offsets.at(draw_idx)) * sizeof(u32)));
      off += grp.num;

      switch (double_draw.kind) {
        case DoubleDrawKind::NONE:
          break;
        case DoubleDrawKind::AFAIL_NO_DEPTH_WRITE:
          tree.perf.draws++;
          tree.perf.wind_draws++;
          tree.perf.verts += grp.num;
          prof.add_draw_call();
          prof.add_tri(grp.num);
          glUniform1f(
              glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "alpha_min"),
              -10.f);
          glUniform1f(
              glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "alpha_max"),
              double_draw.aref);
          glDepthMask(GL_FALSE);
          glDrawElements(GL_TRIANGLE_STRIP, draw.vertex_index_stream.size(), GL_UNSIGNED_INT,
                         (void*)0);
          break;
        default:
          assert(false);
      }
    }
  }
}

void Tie3::render_tree(int idx,
                       const TfragRenderSettings& settings,
                       SharedRenderState* render_state,
                       ScopedProfilerNode& prof) {
  Timer tree_timer;
  auto& tree = m_trees.at(idx);
  tree.perf.draws = 0;
  tree.perf.verts = 0;
  tree.perf.full_draws = 0;
  tree.perf.wind_draws = 0;
  if (!m_has_level) {
    return;
  }

  if (m_color_result.size() < tree.colors->size()) {
    m_color_result.resize(tree.colors->size());
  }

  Timer interp_timer;
  if (m_use_fast_time_of_day) {
    interp_time_of_day_fast(settings.time_of_day_weights, tree.tod_cache, m_color_result.data());
  } else {
    interp_time_of_day_slow(settings.time_of_day_weights, *tree.colors, m_color_result.data());
  }
  tree.perf.tod_time.add(interp_timer.getSeconds());

  Timer setup_timer;
  glActiveTexture(GL_TEXTURE10);
  glBindTexture(GL_TEXTURE_1D, tree.time_of_day_texture);
  glTexSubImage1D(GL_TEXTURE_1D, 0, 0, tree.colors->size(), GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
                  m_color_result.data());

  first_tfrag_draw_setup(settings, render_state);

  glBindVertexArray(tree.vao);
  glBindBuffer(GL_ARRAY_BUFFER, tree.vertex_buffer);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, tree.index_buffer);
  glActiveTexture(GL_TEXTURE0);
  glEnable(GL_PRIMITIVE_RESTART);
  glPrimitiveRestartIndex(UINT32_MAX);
  tree.perf.tod_time.add(setup_timer.getSeconds());

  int last_texture = -1;
  u32 idx_buffer_ptr = 0;

  if (m_debug_all_visible) {
    tree.perf.cull_time.add(0);
    Timer index_timer;
    idx_buffer_ptr = make_all_visible_index_list(m_cache.draw_idx_temp.data(),
                                                 tree.index_list.data(), *tree.draws);
    tree.perf.index_time.add(index_timer.getSeconds());
    tree.perf.index_upload = sizeof(u32) * idx_buffer_ptr;
  } else {
    Timer cull_timer;
    cull_check_all_slow(settings.planes, tree.vis->vis_nodes, m_cache.vis_temp.data());
    tree.perf.cull_time.add(cull_timer.getSeconds());

    Timer index_timer;
    idx_buffer_ptr = make_index_list_from_vis_string(
        m_cache.draw_idx_temp.data(), tree.index_list.data(), *tree.draws, m_cache.vis_temp);
    tree.perf.index_time.add(index_timer.getSeconds());
    tree.perf.index_upload = sizeof(u32) * idx_buffer_ptr;
  }

  Timer draw_timer;
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, idx_buffer_ptr * sizeof(u32), tree.index_list.data(),
               GL_STREAM_DRAW);

  for (size_t draw_idx = 0; draw_idx < tree.draws->size(); draw_idx++) {
    const auto& draw = tree.draws->operator[](draw_idx);
    const auto& indices = m_cache.draw_idx_temp[draw_idx];

    if (indices.second <= indices.first) {
      continue;
    }

    if ((int)draw.tree_tex_id != last_texture) {
      glBindTexture(GL_TEXTURE_2D, m_textures.at(draw.tree_tex_id));
      last_texture = draw.tree_tex_id;
    }

    auto double_draw = setup_tfrag_shader(settings, render_state, draw.mode);
    int draw_size = indices.second - indices.first;
    void* offset = (void*)(indices.first * sizeof(u32));

    prof.add_draw_call();
    prof.add_tri(draw.num_triangles * (float)draw_size / draw.vertex_index_stream.size());

    bool is_full = draw_size == (int)draw.vertex_index_stream.size();

    tree.perf.draws++;
    if (is_full) {
      tree.perf.full_draws++;
    }
    tree.perf.verts += draw_size;

    glDrawElements(GL_TRIANGLE_STRIP, draw_size, GL_UNSIGNED_INT, (void*)offset);

    switch (double_draw.kind) {
      case DoubleDrawKind::NONE:
        break;
      case DoubleDrawKind::AFAIL_NO_DEPTH_WRITE:
        tree.perf.draws++;
        tree.perf.verts += draw_size;
        if (is_full) {
          tree.perf.full_draws++;
        }
        prof.add_draw_call();
        prof.add_tri(draw_size);
        glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "alpha_min"),
                    -10.f);
        glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "alpha_max"),
                    double_draw.aref);
        glDepthMask(GL_FALSE);
        glDrawElements(GL_TRIANGLE_STRIP, draw_size, GL_UNSIGNED_INT, (void*)offset);
        break;
      default:
        assert(false);
    }

    if (m_debug_wireframe) {
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
          settings.fog_x);
      glDisable(GL_BLEND);
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
      glDrawElements(GL_TRIANGLE_STRIP, draw_size, GL_UNSIGNED_INT, (void*)offset);
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
      prof.add_draw_call();
      prof.add_tri(draw_size);
      render_state->shaders[ShaderId::TFRAG3].activate();
    }
  }

  if (!m_hide_wind) {
    auto wind_prof = prof.make_scoped_child("wind");
    render_tree_wind(idx, settings, render_state, wind_prof);
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
  for (u32 i = 0; i < m_trees.size(); i++) {
    auto& perf = m_trees[i].perf;
    ImGui::Text("Tree: %d", i);
    ImGui::Text("index data bytes: %d", perf.index_upload);
    ImGui::Text("time of days: %d", (int)m_trees[i].colors->size());
    ImGui::Text("draw: %d, full: %d, verts: %d", perf.draws, perf.full_draws, perf.verts);
    ImGui::Text("wind draw: %d", perf.wind_draws);
    ImGui::Text("total: %.2f", perf.tree_time.get());
    ImGui::Text("cull: %.2f index: %.2f tod: %.2f setup: %.2f draw: %.2f",
                perf.cull_time.get() * 1000.f, perf.index_time.get() * 1000.f,
                perf.tod_time.get() * 1000.f, perf.setup_time.get() * 1000.f,
                perf.draw_time.get() * 1000.f);
    ImGui::Separator();
  }
  ImGui::Text("All trees: %.2f", 1000.f * m_all_tree_time.get());
}
