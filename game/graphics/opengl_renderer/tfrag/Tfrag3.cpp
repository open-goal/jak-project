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

    for (size_t tree_idx = 0; tree_idx < lev_data->trees.size(); tree_idx++) {
      const auto& tree = lev_data->trees[tree_idx];
      m_cached_trees[tree_idx].kind = tree.kind;
      if (tree.kind != tfrag3::TFragmentTreeKind::INVALID) {
        for (auto& draw : tree.draws) {
          idx_buffer_len = std::max(idx_buffer_len, draw.vertex_index_stream.size());
        }
        u32 verts = tree.vertices.size();
        fmt::print("  tree {} has {} verts ({} kB) and {} draws\n", tree_idx, verts,
                   verts * sizeof(tfrag3::PreloadedVertex) / 1024.f, tree.draws.size());
        glGenVertexArrays(1, &m_cached_trees[tree_idx].vao);
        glBindVertexArray(m_cached_trees[tree_idx].vao);
        glGenBuffers(1, &m_cached_trees[tree_idx].vertex_buffer);
        m_cached_trees[tree_idx].vert_count = verts;
        m_cached_trees[tree_idx].draws = &tree.draws;  // todo - should we just copy this?
        glBindBuffer(GL_ARRAY_BUFFER, m_cached_trees[tree_idx].vertex_buffer);
        glBufferData(GL_ARRAY_BUFFER, verts * sizeof(tfrag3::PreloadedVertex), nullptr,
                     GL_DYNAMIC_DRAW);
        glEnableVertexAttribArray(0);
        glEnableVertexAttribArray(1);

        glBufferSubData(GL_ARRAY_BUFFER, 0, verts * sizeof(tfrag3::PreloadedVertex),
                        tree.vertices.data());

        glVertexAttribPointer(0,                                // location 0 in the shader
                              3,                                // 3 values per vert
                              GL_FLOAT,                         // floats
                              GL_FALSE,                         // normalized
                              sizeof(tfrag3::PreloadedVertex),  // stride
                              (void*)offsetof(tfrag3::PreloadedVertex, x)  // offset (0)
        );

        glVertexAttribPointer(1,                                // location 2 in the shader
                              3,                                // 3 values per vert
                              GL_FLOAT,                         // floats
                              GL_FALSE,                         // normalized
                              sizeof(tfrag3::PreloadedVertex),  // stride
                              (void*)offsetof(tfrag3::PreloadedVertex, s)  // offset (0)
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
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_index_buffer);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, idx_buffer_len * sizeof(u32), nullptr, GL_DYNAMIC_DRAW);

    m_level_name = level;
  }
}

void Tfrag3::first_draw_setup(const RenderSettings& settings, SharedRenderState* render_state) {
  render_state->shaders[ShaderId::TFRAG3].activate();
  glUniform1i(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "T0"), 0);
  glUniformMatrix4fv(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "camera"),
                     1, GL_FALSE, settings.math_camera.data());
  glUniform4f(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "hvdf_offset"),
              settings.hvdf_offset[0], settings.hvdf_offset[1], settings.hvdf_offset[2],
              settings.hvdf_offset[3]);
  glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "fog_constant"),
              settings.fog_x);
}

void Tfrag3::setup_shader(const RenderSettings& /*settings*/,
                          SharedRenderState* render_state,
                          DrawMode mode) {
  if (mode.get_depth_write_enable()) {
    glDepthMask(GL_TRUE);
  } else {
    glDepthMask(GL_FALSE);
  }

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

  if (mode.get_clamp_enable()) {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  } else {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  }

  if (mode.get_filt_enable()) {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  } else {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  }

  float alpha_reject = 0.;
  if (mode.get_at_enable()) {
    switch (mode.get_alpha_test()) {
      case DrawMode::AlphaTest::ALWAYS:
        break;
      case DrawMode::AlphaTest::GEQUAL:
        alpha_reject = mode.get_aref() / 127.f;
        break;
      case DrawMode::AlphaTest::NEVER:
        break;
      default:
        assert(false);
    }
  }

  glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "alpha_reject"),
              alpha_reject);
}

void Tfrag3::render_tree(const RenderSettings& settings,
                         SharedRenderState* render_state,
                         ScopedProfilerNode& prof) {
  auto& tree = m_cached_trees.at(settings.tree_idx);
  assert(tree.kind != tfrag3::TFragmentTreeKind::INVALID);

  first_draw_setup(settings, render_state);

  glBindVertexArray(tree.vao);
  glBindBuffer(GL_ARRAY_BUFFER, tree.vertex_buffer);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_index_buffer);
  glActiveTexture(GL_TEXTURE0);
  glEnable(GL_PRIMITIVE_RESTART);
  glPrimitiveRestartIndex(UINT32_MAX);

  for (const auto& draw : *tree.draws) {
    glBindTexture(GL_TEXTURE_2D, m_textures.at(draw.tree_tex_id));
    setup_shader(settings, render_state, draw.mode);
    prof.add_draw_call();
    prof.add_tri(draw.num_triangles);
    glBufferSubData(GL_ELEMENT_ARRAY_BUFFER, 0, draw.vertex_index_stream.size() * sizeof(u32),
                    draw.vertex_index_stream.data());

    // hack:
    //    glDisable(GL_DEPTH_TEST);
    //    glDepthFunc(GL_ALWAYS);
    //    // glDisable(GL_ALPHA_TEST);
    //    glDisable(GL_BLEND);

    glDrawElements(GL_TRIANGLE_STRIP, draw.vertex_index_stream.size(), GL_UNSIGNED_INT, (void*)0);
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

  // delete textures and stuff.
  m_cached_trees.clear();
}