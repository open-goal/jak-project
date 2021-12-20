#include "Tie3.h"

#include "third-party/imgui/imgui.h"

Tie3::Tie3(const std::string& name, BucketId my_id) : BucketRenderer(name, my_id) {}

Tie3::~Tie3() {
  discard_tree_cache();
}

void Tie3::setup_for_level(const std::string& level, SharedRenderState* render_state) {
  // make sure we have the level data.
  auto lev_data = render_state->loader.get_tfrag3_level(level);
  if (m_level_name != level) {
    fmt::print("new level for tie3: {} -> {}\n", m_level_name, level);
    fmt::print("discarding old stuff\n");
    discard_tree_cache();
    fmt::print("level has {} tie trees\n", lev_data->tie_trees.size());
    m_trees.resize(lev_data->tie_trees.size());

    size_t idx_buffer_len = 0;
    size_t time_of_day_count = 0;

    for (size_t tree_idx = 0; tree_idx < lev_data->tie_trees.size(); tree_idx++) {
      const auto& tree = lev_data->tie_trees[tree_idx];
      for (auto& draw : tree.static_draws) {
        idx_buffer_len = std::max(idx_buffer_len, draw.vertex_index_stream.size());
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
      m_trees[tree_idx].vis_temp.resize(tree.bvh.vis_nodes.size());
      m_trees[tree_idx].culled_indices.resize(idx_buffer_len);
      glBindBuffer(GL_ARRAY_BUFFER, m_trees[tree_idx].vertex_buffer);
      glBufferData(GL_ARRAY_BUFFER, verts * sizeof(tfrag3::PreloadedVertex), nullptr,
                   GL_DYNAMIC_DRAW);
      glEnableVertexAttribArray(0);
      glEnableVertexAttribArray(1);
      glEnableVertexAttribArray(2);

      glBufferSubData(GL_ARRAY_BUFFER, 0, verts * sizeof(tfrag3::PreloadedVertex),
                      tree.vertices.data());

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

      glVertexAttribPointer(2,                                // location 2 in the shader
                            1,                                // 1 values per vert
                            GL_UNSIGNED_SHORT,                // u16
                            GL_FALSE,                         // don't normalize
                            sizeof(tfrag3::PreloadedVertex),  // stride
                            (void*)offsetof(tfrag3::PreloadedVertex, color_index)  // offset (0)
      );
      glBindVertexArray(0);
    }

    // todo share textures
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

void Tie3::discard_tree_cache() {
  for (auto tex : m_textures) {
    glBindTexture(GL_TEXTURE_2D, tex);
    glDeleteTextures(1, &tex);
  }
  m_textures.clear();

  for (auto& tree : m_trees) {
    glDeleteBuffers(1, &tree.vertex_buffer);
    glDeleteVertexArrays(1, &tree.vao);
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

  m_trees.clear();
}

void Tie3::render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) {
  if (m_override_level && m_pending_user_level) {
    setup_for_level(*m_pending_user_level, render_state);
    m_pending_user_level = {};
  }

  // todo render all...
}

void Tie3::render_all_trees(const TfragRenderSettings& settings,
                            SharedRenderState* render_state,
                            ScopedProfilerNode& prof) {
  if (m_override_level && m_pending_user_level) {
    setup_for_level(*m_pending_user_level, render_state);
    m_pending_user_level = {};
  }
  for (u32 i = 0; i < m_trees.size(); i++) {
    render_tree(i, settings, render_state, prof);
  }
}

void Tie3::render_tree(int idx,
                       const TfragRenderSettings& settings,
                       SharedRenderState* render_state,
                       ScopedProfilerNode& prof) {
  auto& tree = m_trees.at(idx);

  if (m_color_result.size() < tree.colors->size()) {
    m_color_result.resize(tree.colors->size());
  }
  interp_time_of_day_slow(settings.time_of_day_weights, *tree.colors, m_color_result.data());
  glActiveTexture(GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_1D, m_time_of_day_texture);
  glTexSubImage1D(GL_TEXTURE_1D, 0, 0, tree.colors->size(), GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
                  m_color_result.data());

  first_tfrag_draw_setup(settings, render_state);

  glBindVertexArray(tree.vao);
  glBindBuffer(GL_ARRAY_BUFFER, tree.vertex_buffer);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_index_buffer);
  glActiveTexture(GL_TEXTURE0);
  glEnable(GL_PRIMITIVE_RESTART);
  glPrimitiveRestartIndex(UINT32_MAX);

  for (const auto& draw : *tree.draws) {
    glBindTexture(GL_TEXTURE_2D, m_textures.at(draw.tree_tex_id));
    auto double_draw = setup_tfrag_shader(settings, render_state, draw.mode);

    int draw_size = draw.vertex_index_stream.size();
    if (false) {
      int vtx_idx = 0;
      int out_idx = 0;
      for (auto& grp : draw.vis_groups) {
        if (grp.vis_idx == 0xffffffff || tree.vis_temp.at(grp.vis_idx)) {
          memcpy(&tree.culled_indices[out_idx], &draw.vertex_index_stream[vtx_idx],
                 grp.num * sizeof(u32));
          out_idx += grp.num;
        }

        vtx_idx += grp.num;
      }

      draw_size = out_idx;
      if (draw_size == 0) {
        continue;
      }

      prof.add_draw_call();
      prof.add_tri(draw.num_triangles * (float)out_idx / draw.vertex_index_stream.size());

      glBufferSubData(GL_ELEMENT_ARRAY_BUFFER, 0, out_idx * sizeof(u32),
                      tree.culled_indices.data());
    } else {
      glBufferSubData(GL_ELEMENT_ARRAY_BUFFER, 0, draw.vertex_index_stream.size() * sizeof(u32),
                      draw.vertex_index_stream.data());
      prof.add_draw_call();
      prof.add_tri(draw.num_triangles);
    }

    glDrawElements(GL_TRIANGLE_STRIP, draw_size, GL_UNSIGNED_INT, (void*)0);

    switch (double_draw.kind) {
      case DoubleDrawKind::NONE:
        break;
      case DoubleDrawKind::AFAIL_NO_DEPTH_WRITE:
        prof.add_draw_call();
        prof.add_tri(draw_size);
        glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "alpha_min"),
                    -10.f);
        glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::TFRAG3].id(), "alpha_max"),
                    double_draw.aref);
        glDepthMask(GL_FALSE);
        glDrawElements(GL_TRIANGLE_STRIP, draw_size, GL_UNSIGNED_INT, (void*)0);
        break;
      default:
        assert(false);
    }
  }
  glBindVertexArray(0);
}

void Tie3::draw_debug_window() {
  ImGui::InputText("Custom Level", m_user_level, sizeof(m_user_level));
  if (ImGui::Button("Go!")) {
    m_pending_user_level = m_user_level;
  }
  ImGui::Checkbox("Override level", &m_override_level);
}