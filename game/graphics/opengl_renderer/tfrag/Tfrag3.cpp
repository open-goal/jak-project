#include "Tfrag3.h"

Tfrag3::Tfrag3() {
  // glGenVertexArrays(1, &m_vao);
}

Tfrag3::~Tfrag3() {
  discard_tree_cache();
  // glDeleteVertexArrays(1, &m_vao);
}

void Tfrag3::setup_for_level(const std::string& level, SharedRenderState* render_state) {
  // make sure we have the level data.
  auto lev_data = render_state->loader.get_tfrag3_level(level);
  if (m_level_name != level) {
    fmt::print("new level for tfrag3: {} -> {}\n", m_level_name, level);
    fmt::print("discarding old stuff\n");
    discard_tree_cache();
    fmt::print("level has {} trees\n", lev_data->trees.size());
    m_cached_trees.resize(lev_data->trees.size());

    size_t idx_buffer_len = 0;
    size_t time_of_day_count = 0;

    for (size_t tree_idx = 0; tree_idx < lev_data->trees.size(); tree_idx++) {
      const auto& tree = lev_data->trees[tree_idx];
      m_cached_trees[tree_idx].kind = tree.kind;
      if (tree.kind != tfrag3::TFragmentTreeKind::INVALID) {
        for (auto& draw : tree.draws) {
          idx_buffer_len = std::max(idx_buffer_len, draw.vertex_index_stream.size());
        }
        time_of_day_count = std::max(tree.colors.size(), time_of_day_count);
        u32 verts = tree.vertices.size();
        fmt::print("  tree {} has {} verts ({} kB) and {} draws\n", tree_idx, verts,
                   verts * sizeof(tfrag3::PreloadedVertex) / 1024.f, tree.draws.size());
        glGenVertexArrays(1, &m_cached_trees[tree_idx].vao);
        glBindVertexArray(m_cached_trees[tree_idx].vao);
        glGenBuffers(1, &m_cached_trees[tree_idx].vertex_buffer);
        m_cached_trees[tree_idx].vert_count = verts;
        m_cached_trees[tree_idx].draws = &tree.draws;  // todo - should we just copy this?
        m_cached_trees[tree_idx].colors = &tree.colors;
        glBindBuffer(GL_ARRAY_BUFFER, m_cached_trees[tree_idx].vertex_buffer);
        glBufferData(GL_ARRAY_BUFFER, verts * sizeof(tfrag3::PreloadedVertex), nullptr,
                     GL_DYNAMIC_DRAW);
        glEnableVertexAttribArray(0);
        glEnableVertexAttribArray(1);
        glEnableVertexAttribArray(2);

        glBufferSubData(GL_ARRAY_BUFFER, 0, verts * sizeof(tfrag3::PreloadedVertex),
                        tree.vertices.data());

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

        glVertexAttribPointer(2,                                // location 2 in the shader
                              1,                                // 1 values per vert
                              GL_UNSIGNED_SHORT,                // u16
                              GL_FALSE,                         // don't normalize
                              sizeof(tfrag3::PreloadedVertex),  // stride
                              (void*)offsetof(tfrag3::PreloadedVertex, color_index)  // offset (0)
        );
        glBindVertexArray(0);
      }
    }

    fmt::print("level has {} textures\n", lev_data->textures.size());
    for (auto& tex : lev_data->textures) {
      GLuint gl_tex;
      // fmt::print("  tex: {} x {} {} {}\n", tex.w, tex.h, tex.debug_name, tex.debug_tpage_name);
      glGenTextures(1, &gl_tex);
      glBindTexture(GL_TEXTURE_2D, gl_tex);
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tex.w, tex.h, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
                   tex.data.data());
      glBindTexture(GL_TEXTURE_2D, 0);
      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_2D, gl_tex);
      glGenerateMipmap(GL_TEXTURE_2D);

      float aniso = 0.0f;
      glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY, &aniso);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY, aniso);
      m_textures.push_back(gl_tex);
    }

    fmt::print("level max index stream: {}\n", idx_buffer_len);
    m_has_index_buffer = true;
    glGenBuffers(1, &m_index_buffer);
    glActiveTexture(GL_TEXTURE1);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_index_buffer);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, idx_buffer_len * sizeof(u32), nullptr, GL_DYNAMIC_DRAW);

    fmt::print("level max time of day: {}\n", time_of_day_count);
    assert(time_of_day_count <= TIME_OF_DAY_COLOR_COUNT);
    // regardless of how many we use some fixed max
    // we won't actually interp or upload to gpu the unused ones, but we need a fixed maximum so
    // indexing works properly.
    m_color_result.resize(TIME_OF_DAY_COLOR_COUNT);
    glGenTextures(1, &m_time_of_day_texture);
    m_has_time_of_day_texture = true;
    glBindTexture(GL_TEXTURE_1D, m_time_of_day_texture);
    // just fill with zeros. this lets use use the faster texsubimage later
    glTexImage1D(GL_TEXTURE_1D, 0, GL_RGBA, TIME_OF_DAY_COLOR_COUNT, 0, GL_RGBA,
                 GL_UNSIGNED_INT_8_8_8_8, m_color_result.data());
    glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

    m_level_name = level;
  }
}

void Tfrag3::first_draw_setup(const RenderSettings& settings, SharedRenderState* render_state) {
  render_state->shaders[ShaderId::TFRAG3].activate();
  glUniform1i(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "tex_T0"), 0);
  glUniform1i(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "tex_T1"), 1);
  glUniformMatrix4fv(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "camera"),
                     1, GL_FALSE, settings.math_camera.data());
  glUniform4f(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "hvdf_offset"),
              settings.hvdf_offset[0], settings.hvdf_offset[1], settings.hvdf_offset[2],
              settings.hvdf_offset[3]);
  glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "fog_constant"),
              settings.fog_x);
}

Tfrag3::DoubleDraw Tfrag3::setup_shader(const RenderSettings& /*settings*/,
                                        SharedRenderState* render_state,
                                        DrawMode mode) {
  glActiveTexture(GL_TEXTURE0);

  if (mode.get_zt_enable()) {
    glEnable(GL_DEPTH_TEST);
    switch (mode.get_depth_test()) {
      case GsTest::ZTest::NEVER:
        glDepthFunc(GL_NEVER);
        break;
      case GsTest::ZTest::ALWAYS:
        glDepthFunc(GL_ALWAYS);
        break;
      case GsTest::ZTest::GEQUAL:
        glDepthFunc(GL_GEQUAL);
        break;
      case GsTest::ZTest::GREATER:
        glDepthFunc(GL_GREATER);
        break;
      default:
        assert(false);
    }
  } else {
    glDisable(GL_DEPTH_TEST);
  }

  if (mode.get_ab_enable() && mode.get_alpha_blend() != DrawMode::AlphaBlend::DISABLED) {
    glEnable(GL_BLEND);
    switch (mode.get_alpha_blend()) {
      case DrawMode::AlphaBlend::SRC_DST_SRC_DST:
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        break;
      case DrawMode::AlphaBlend::SRC_0_SRC_DST:
        glBlendFunc(GL_SRC_ALPHA, GL_ONE);
        break;
      default:
        assert(false);
    }
  } else {
    glDisable(GL_BLEND);
  }

  if (mode.get_clamp_s_enable()) {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  } else {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  }

  if (mode.get_clamp_t_enable()) {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  } else {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  }

  if (mode.get_filt_enable()) {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  } else {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  }

  // for some reason, they set atest NEVER + FB_ONLY to disable depth writes
  bool alpha_hack_to_disable_z_write = false;
  DoubleDraw double_draw;

  float alpha_min = 0.;
  if (mode.get_at_enable()) {
    switch (mode.get_alpha_test()) {
      case DrawMode::AlphaTest::ALWAYS:
        break;
      case DrawMode::AlphaTest::GEQUAL:
        alpha_min = mode.get_aref() / 127.f;
        switch (mode.get_alpha_fail()) {
          case GsTest::AlphaFail::KEEP:
            // ok, no need for double draw
            break;
          case GsTest::AlphaFail::FB_ONLY:
            // darn, we need to draw twice
            double_draw.kind = DoubleDrawKind::AFAIL_NO_DEPTH_WRITE;
            double_draw.aref = alpha_min;
            break;
          default:
            assert(false);
        }
        break;
      case DrawMode::AlphaTest::NEVER:
        if (mode.get_alpha_fail() == GsTest::AlphaFail::FB_ONLY) {
          alpha_hack_to_disable_z_write = true;
        } else {
          assert(false);
        }
        break;
      default:
        assert(false);
    }
  }

  if (mode.get_depth_write_enable()) {
    glDepthMask(GL_TRUE);
  } else {
    glDepthMask(GL_FALSE);
  }

  glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "alpha_min"),
              alpha_min);
  glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "alpha_max"),
              10.f);

  return double_draw;
}

void Tfrag3::render_tree(const RenderSettings& settings,
                         SharedRenderState* render_state,
                         ScopedProfilerNode& prof) {
  auto& tree = m_cached_trees.at(settings.tree_idx);
  assert(tree.kind != tfrag3::TFragmentTreeKind::INVALID);

  if (m_color_result.size() < tree.colors->size()) {
    m_color_result.resize(tree.colors->size());
  }
  interp_time_of_day_slow(settings.time_of_day_weights, *tree.colors, m_color_result.data());
  glActiveTexture(GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_1D, m_time_of_day_texture);
  glTexSubImage1D(GL_TEXTURE_1D, 0, 0, tree.colors->size(), GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
                  m_color_result.data());

  first_draw_setup(settings, render_state);

  glBindVertexArray(tree.vao);
  glBindBuffer(GL_ARRAY_BUFFER, tree.vertex_buffer);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_index_buffer);
  glActiveTexture(GL_TEXTURE0);
  glEnable(GL_PRIMITIVE_RESTART);
  glPrimitiveRestartIndex(UINT32_MAX);

  for (const auto& draw : *tree.draws) {
    glBindTexture(GL_TEXTURE_2D, m_textures.at(draw.tree_tex_id));
    auto double_draw = setup_shader(settings, render_state, draw.mode);
    prof.add_draw_call();
    prof.add_tri(draw.num_triangles);
    glBufferSubData(GL_ELEMENT_ARRAY_BUFFER, 0, draw.vertex_index_stream.size() * sizeof(u32),
                    draw.vertex_index_stream.data());

    glDrawElements(GL_TRIANGLE_STRIP, draw.vertex_index_stream.size(), GL_UNSIGNED_INT, (void*)0);

    switch (double_draw.kind) {
      case DoubleDrawKind::NONE:
        break;
      case DoubleDrawKind::AFAIL_NO_DEPTH_WRITE:
        prof.add_draw_call();
        prof.add_tri(draw.num_triangles);
        glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "alpha_min"),
                    -10.f);
        glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "alpha_max"),
                    double_draw.aref);
        glDepthMask(GL_FALSE);
        glDrawElements(GL_TRIANGLE_STRIP, draw.vertex_index_stream.size(), GL_UNSIGNED_INT,
                       (void*)0);
        break;
      default:
        assert(false);
    }
  }
  glBindVertexArray(0);
}

/*!
 * Render all trees with settings for the given tree.
 * This is intended to be used only for debugging when we can't easily get commands for all trees
 * working.
 */
void Tfrag3::debug_render_all_trees(const RenderSettings& settings,
                                    SharedRenderState* render_state,
                                    ScopedProfilerNode& prof) {
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClearDepth(0.0);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  RenderSettings settings_copy = settings;
  for (size_t i = 0; i < m_cached_trees.size(); i++) {
    if (m_cached_trees[i].kind != tfrag3::TFragmentTreeKind::INVALID) {
      settings_copy.tree_idx = i;
      render_tree(settings_copy, render_state, prof);
    }
  }
}

void Tfrag3::debug_render_all_trees_nolores(const RenderSettings& settings,
                                            SharedRenderState* render_state,
                                            ScopedProfilerNode& prof) {
  RenderSettings settings_copy = settings;
  for (size_t i = 0; i < m_cached_trees.size(); i++) {
    if (m_cached_trees[i].kind != tfrag3::TFragmentTreeKind::INVALID &&
        m_cached_trees[i].kind != tfrag3::TFragmentTreeKind::LOWRES_TRANS &&
        m_cached_trees[i].kind != tfrag3::TFragmentTreeKind::LOWRES) {
      settings_copy.tree_idx = i;
      render_tree(settings_copy, render_state, prof);
    }
  }
}

void Tfrag3::discard_tree_cache() {
  for (auto tex : m_textures) {
    glBindTexture(GL_TEXTURE_2D, tex);
    glDeleteTextures(1, &tex);
  }
  m_textures.clear();

  for (auto& tree : m_cached_trees) {
    if (tree.kind != tfrag3::TFragmentTreeKind::INVALID) {
      glDeleteBuffers(1, &tree.vertex_buffer);
      glDeleteVertexArrays(1, &tree.vao);
    }
  }

  if (m_has_index_buffer) {
    glDeleteBuffers(1, &m_index_buffer);
    m_has_index_buffer = false;
  }

  if (m_has_time_of_day_texture) {
    glBindTexture(GL_TEXTURE_1D, m_time_of_day_texture);
    glDeleteTextures(1, &m_time_of_day_texture);
    m_has_time_of_day_texture = false;
  }

  // delete textures and stuff.
  m_cached_trees.clear();
}

void Tfrag3::interp_time_of_day_slow(const float weights[8],
                                     const std::vector<tfrag3::TimeOfDayColor>& in,
                                     math::Vector<u8, 4>* out) {
  // Timer interp_timer;
  for (size_t color = 0; color < in.size(); color++) {
    math::Vector4f result = math::Vector4f::zero();
    for (int component = 0; component < 8; component++) {
      result += in[color].rgba[component].cast<float>() * weights[component];
    }
    result[0] = std::min(result[0], 255.f);
    result[1] = std::min(result[1], 255.f);
    result[2] = std::min(result[2], 255.f);
    result[3] = std::min(result[3], 128.f);  // note: different for alpha!
    out[color] = result.cast<u8>();
  }
  // about 70 us, not bad.
  // fmt::print("interp {} colors {:.2f} ms\n", in.size(), interp_timer.getMs());
}