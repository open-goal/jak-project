#include "CollideMeshRenderer.h"

#include "game/graphics/opengl_renderer/background/background_common.h"

CollideMeshRenderer::CollideMeshRenderer() {
  glGenVertexArrays(1, &m_vao);
}

CollideMeshRenderer::~CollideMeshRenderer() {
  glDeleteVertexArrays(1, &m_vao);
}

void CollideMeshRenderer::render(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  if (!render_state->has_pc_data) {
    return;
  }

  auto levels = render_state->loader->get_in_use_levels();
  if (levels.empty()) {
    return;
  }
  render_state->shaders[ShaderId::COLLISION].activate();

  glBindVertexArray(m_vao);
  TfragRenderSettings settings;
  memcpy(settings.math_camera.data(), render_state->camera_matrix[0].data(), 64);
  settings.hvdf_offset = render_state->camera_hvdf_off;
  settings.fog = render_state->camera_fog;
  settings.tree_idx = 0;
  for (int i = 0; i < 4; i++) {
    settings.planes[i] = render_state->camera_planes[i];
  }
  auto shader = render_state->shaders[ShaderId::COLLISION].id();
  glUniformMatrix4fv(glGetUniformLocation(shader, "camera"), 1, GL_FALSE,
                     settings.math_camera.data());
  glUniform4f(glGetUniformLocation(shader, "hvdf_offset"), settings.hvdf_offset[0],
              settings.hvdf_offset[1], settings.hvdf_offset[2], settings.hvdf_offset[3]);
  const auto& trans = render_state->camera_pos;
  glUniform4f(glGetUniformLocation(shader, "camera_position"), trans[0], trans[1], trans[2],
              trans[3]);
  glUniform1f(glGetUniformLocation(shader, "fog_constant"), settings.fog.x());
  glUniform1f(glGetUniformLocation(shader, "fog_min"), settings.fog.y());
  glUniform1f(glGetUniformLocation(shader, "fog_max"), settings.fog.z());
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_GEQUAL);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);  // ?
  glDepthMask(GL_TRUE);

  for (auto lev : levels) {
    glBindBuffer(GL_ARRAY_BUFFER, lev->collide_vertices);
    glEnableVertexAttribArray(0);
    glEnableVertexAttribArray(1);
    glEnableVertexAttribArray(2);
    glEnableVertexAttribArray(3);
    glVertexAttribPointer(0,                                      // location 0 in the shader
                          3,                                      // 3 values per vert
                          GL_FLOAT,                               // floats
                          GL_FALSE,                               // normalized
                          sizeof(tfrag3::CollisionMesh::Vertex),  // stride
                          0                                       // offset (0)
    );
    glVertexAttribIPointer(1,                                      // location 1 in the shader
                           1,                                      // 3 values per vert
                           GL_UNSIGNED_INT,                        // u32
                           sizeof(tfrag3::CollisionMesh::Vertex),  // stride
                           (void*)offsetof(tfrag3::CollisionMesh::Vertex, flags)  // offset
    );
    glVertexAttribPointer(2,                                      // location 2 in the shader
                          3,                                      // 3 values per vert
                          GL_SHORT,                               // floats
                          GL_TRUE,                                // normalized
                          sizeof(tfrag3::CollisionMesh::Vertex),  // stride
                          (void*)offsetof(tfrag3::CollisionMesh::Vertex, nx)  // offset (0)
    );
    glVertexAttribIPointer(3,                                      // location 3 in the shader
                           1,                                      // 3 values per vert
                           GL_UNSIGNED_INT,                        // u32
                           sizeof(tfrag3::CollisionMesh::Vertex),  // stride
                           (void*)offsetof(tfrag3::CollisionMesh::Vertex, pat)  // offset (0)
    );
    glUniform1i(glGetUniformLocation(shader, "wireframe"), 0);
    glUniform1uiv(glGetUniformLocation(shader, "collision_mode_mask"),
                  Gfx::g_global_settings.collision_mode_mask.size(),
                  Gfx::g_global_settings.collision_mode_mask.data());
    glUniform1uiv(glGetUniformLocation(shader, "collision_event_mask"),
                  Gfx::g_global_settings.collision_event_mask.size(),
                  Gfx::g_global_settings.collision_event_mask.data());
    glUniform1uiv(glGetUniformLocation(shader, "collision_material_mask"),
                  Gfx::g_global_settings.collision_material_mask.size(),
                  Gfx::g_global_settings.collision_material_mask.data());
    glUniform1ui(glGetUniformLocation(shader, "collision_skip_mask"),
                 Gfx::g_global_settings.collision_skip_mask);
    glUniform1i(glGetUniformLocation(shader, "mode"), Gfx::g_global_settings.collision_mode);
    glDrawArrays(GL_TRIANGLES, 0, lev->level->collision.vertices.size());

    if (Gfx::g_global_settings.collision_wireframe) {
      glUniform1i(glGetUniformLocation(shader, "wireframe"), 1);
      glDisable(GL_BLEND);
      glDepthMask(GL_FALSE);
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
      glDrawArrays(GL_TRIANGLES, 0, lev->level->collision.vertices.size());
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
      glEnable(GL_BLEND);
      glDepthMask(GL_TRUE);
    }

    prof.add_draw_call();
    prof.add_tri(lev->level->collision.vertices.size() / 3);
  }
}
