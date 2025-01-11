#include "BspRenderer.h"

#include "common/log/log.h"

#include <third-party/imgui/imgui.h>

BspRenderer::BspRenderer(GameVersion version) {
  glGenVertexArrays(1, &m_vao);
}

void BspRenderer::render(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  // can't render
  if (!render_state->has_pc_data) {
    return;
  }

  // check loaded levels
  auto levels = render_state->loader->get_in_use_levels();
  if (levels.empty()) {
    return;
  }

  glBindVertexArray(m_vao);

  // see if we need to load meshes for any
  for (const auto& level : levels) {
    const auto& cached = m_level_cache.find(level->load_id);
    if (cached == m_level_cache.end()) {
      lg::info("BspRenderer loading for {}", level->level->level_name);
      unload_cached_for_name(level->level->level_name);
      load_level(level, &m_level_cache[level->load_id]);
    }
    render_level(render_state, prof, &m_level_cache.at(level->load_id));
  }
}

void BspRenderer::load_level(LevelData* level, LevelCache* lc) {
  glGenBuffers(1, &lc->vertex_buffer);
  glGenBuffers(1, &lc->index_buffer);
  glBindBuffer(GL_ARRAY_BUFFER, lc->vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER,
               level->level->debug_data.bsp_cell_vertices.size() * sizeof(tfrag3::BspVisVertex),
               level->level->debug_data.bsp_cell_vertices.data(), GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, lc->index_buffer);
  glBufferData(GL_ARRAY_BUFFER, level->level->debug_data.bsp_cell_indices.size() * sizeof(u32),
               level->level->debug_data.bsp_cell_indices.data(), GL_STATIC_DRAW);
  lc->index_count = level->level->debug_data.bsp_cell_indices.size();
}

void BspRenderer::unload_cached_for_name(const std::string& name) {
  for (auto it = m_level_cache.begin(); it != m_level_cache.end();) {
    if (it->second.name == name) {
      lg::info("BspRenderer first removing loaded {}", name);
      unload_level(&it->second);
      it = m_level_cache.erase(it);
    } else {
      ++it;
    }
  }
}

void BspRenderer::unload_level(LevelCache* lc) {
  glDeleteBuffers(1, &lc->index_buffer);
  glDeleteBuffers(1, &lc->vertex_buffer);
}

BspRenderer::~BspRenderer() {
  glDeleteVertexArrays(1, &m_vao);
}

void BspRenderer::draw_debug_window() {
  ImGui::InputInt("min-leaf", &min_leaf);
  ImGui::InputInt("max-leaf", &max_leaf);
}

void BspRenderer::render_level(SharedRenderState* render_state,
                               ScopedProfilerNode& prof,
                               LevelCache* lc) {
  auto shader = render_state->shaders[ShaderId::BSP].id();
  render_state->shaders[ShaderId::BSP].activate();
  glUniformMatrix4fv(glGetUniformLocation(shader, "camera"), 1, GL_FALSE,
                     render_state->camera_matrix[0].data());
  glUniform4f(glGetUniformLocation(shader, "hvdf_offset"), render_state->camera_hvdf_off[0],
              render_state->camera_hvdf_off[1], render_state->camera_hvdf_off[2],
              render_state->camera_hvdf_off[3]);
  const auto& trans = render_state->camera_pos;
  glUniform4f(glGetUniformLocation(shader, "camera_position"), trans[0], trans[1], trans[2],
              trans[3]);
  glUniform1f(glGetUniformLocation(shader, "fog_constant"), render_state->camera_fog.x());
  glUniform1f(glGetUniformLocation(shader, "fog_min"), render_state->camera_fog.y());
  glUniform1f(glGetUniformLocation(shader, "fog_max"), render_state->camera_fog.z());
  glUniform1i(glGetUniformLocation(shader, "min_leaf"), (GLint)min_leaf);
  glUniform1i(glGetUniformLocation(shader, "max_leaf"), (GLint)max_leaf);

  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_GEQUAL);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);  // ?
  glDepthMask(GL_TRUE);

  glBindBuffer(GL_ARRAY_BUFFER, lc->vertex_buffer);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, lc->index_buffer);
  glEnableVertexAttribArray(0);
  glEnableVertexAttribArray(1);
  glEnableVertexAttribArray(2);
  glEnableVertexAttribArray(3);
  glVertexAttribPointer(0,                             // location 0 in the shader
                        3,                             // 3 values per vert
                        GL_FLOAT,                      // floats
                        GL_FALSE,                      // normalized
                        sizeof(tfrag3::BspVisVertex),  // stride
                        0                              // offset (0)
  );
  glVertexAttribIPointer(1,                             // location 1 in the shader
                         1,                             // 3 values per vert
                         GL_UNSIGNED_SHORT,             // u16
                         sizeof(tfrag3::BspVisVertex),  // stride
                         (void*)offsetof(tfrag3::BspVisVertex, bsp_cell)  // offset
  );
  glEnable(GL_PRIMITIVE_RESTART);
  glPrimitiveRestartIndex(UINT32_MAX);
  glUniform1i(glGetUniformLocation(shader, "wireframe"), 0);
  // glDrawElements(GL_TRIANGLE_FAN, lc->index_count, GL_UNSIGNED_INT, nullptr);

  if (true) {
    glUniform1i(glGetUniformLocation(shader, "wireframe"), 1);
    glDisable(GL_BLEND);
    glDepthMask(GL_FALSE);
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    glLineWidth(3.0);
    glDrawElements(GL_TRIANGLE_FAN, lc->index_count, GL_UNSIGNED_INT, nullptr);
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    glEnable(GL_BLEND);
    glDepthMask(GL_TRUE);
  }

  prof.add_draw_call();
  prof.add_tri(lc->index_count);  // not exactly, but who cares.
}