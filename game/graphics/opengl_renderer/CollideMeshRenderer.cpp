#include "CollideMeshRenderer.h"

#include "game/graphics/opengl_renderer/background/background_common.h"

CollideMeshRenderer::CollideMeshRenderer() {
  glGenVertexArrays(1, &m_vao);
  glBindVertexArray(m_vao);

  glEnableVertexAttribArray(0);
  glEnableVertexAttribArray(1);

  //  glVertexAttribPointer(0,                                      // location 0 in the shader
  //                        3,                                      // 3 values per vert
  //                        GL_FLOAT,                               // floats
  //                        GL_FALSE,                               // normalized
  //                        sizeof(tfrag3::CollisionMesh::Vertex),  // stride
  //                        (void*)offsetof(tfrag3::CollisionMesh::Vertex, x)  // offset
  //  );

  glVertexAttribPointer(0,                                          // location 0 in the shader
                        3,                                          // 3 values per vert
                        GL_FLOAT,                                   // floats
                        GL_FALSE,                                   // normalized
                        sizeof(tfrag3::ShrubGpuVertex),             // stride
                        (void*)offsetof(tfrag3::ShrubGpuVertex, x)  // offset (0)
  );

  //  glVertexAttribIPointer(1,                                      // location 1 in the shader
  //                         1,                                      // 3 values per vert
  //                         GL_UNSIGNED_INT,                        // u32
  //                         sizeof(tfrag3::CollisionMesh::Vertex),  // stride
  //                         (void*)offsetof(tfrag3::CollisionMesh::Vertex, flags)  // offset
  //  );
  glBindVertexArray(0);
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
  glEnableVertexAttribArray(0);
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(0,                                      // location 0 in the shader
                        3,                                      // 3 values per vert
                        GL_FLOAT,                               // floats
                        GL_FALSE,                               // normalized
                        sizeof(tfrag3::CollisionMesh::Vertex),  // stride
                        (void*)offsetof(tfrag3::CollisionMesh::Vertex, x)  // offset
  );

  glVertexAttribIPointer(1,                                      // location 1 in the shader
                         1,                                      // 3 values per vert
                         GL_UNSIGNED_INT,                        // u32
                         sizeof(tfrag3::CollisionMesh::Vertex),  // stride
                         (void*)offsetof(tfrag3::CollisionMesh::Vertex, flags)  // offset
  );
  TfragRenderSettings settings;
  memcpy(settings.math_camera.data(), render_state->camera_matrix[0].data(), 64);
  settings.hvdf_offset = render_state->camera_hvdf_off;
  settings.fog = render_state->camera_fog;
  settings.tree_idx = 0;
  for (int i = 0; i < 4; i++) {
    settings.planes[i] = render_state->camera_planes[i];
  }
  glUniformMatrix4fv(
      glGetUniformLocation(render_state->shaders[ShaderId::COLLISION].id(), "camera"), 1, GL_FALSE,
      settings.math_camera.data());
  glUniform4f(glGetUniformLocation(render_state->shaders[ShaderId::COLLISION].id(), "hvdf_offset"),
              settings.hvdf_offset[0], settings.hvdf_offset[1], settings.hvdf_offset[2],
              settings.hvdf_offset[3]);
  glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::COLLISION].id(), "fog_constant"),
              settings.fog.x());
  glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::COLLISION].id(), "fog_min"),
              settings.fog.y());
  glUniform1f(glGetUniformLocation(render_state->shaders[ShaderId::COLLISION].id(), "fog_max"),
              settings.fog.z());
  glUniform1i(glGetUniformLocation(render_state->shaders[ShaderId::COLLISION].id(), "mode"), 0);
  //  glDisable(GL_DEPTH_TEST);
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_GEQUAL);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);  // ?
  glDepthMask(GL_TRUE);

  for (auto lev : levels) {
    static_assert(16 == sizeof(tfrag3::CollisionMesh::Vertex));
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, lev->collide_indices);
    glBindBuffer(GL_ARRAY_BUFFER, lev->collide_vertices);
    glVertexAttribPointer(0,         // location 0 in the shader
                          3,         // 3 values per vert
                          GL_FLOAT,  // floats
                          GL_FALSE,  // normalized
                          16,        // stride
                          0          // offset (0)
    );

    glUniform1i(glGetUniformLocation(render_state->shaders[ShaderId::COLLISION].id(), "mode"), 0);
    glDrawElements(GL_TRIANGLES, lev->level->collision.indices.size(), GL_UNSIGNED_INT, (void*)0);

    glUniform1i(glGetUniformLocation(render_state->shaders[ShaderId::COLLISION].id(), "mode"), 1);
    glDisable(GL_BLEND);
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    glDrawElements(GL_TRIANGLES, lev->level->collision.indices.size(), GL_UNSIGNED_INT, (void*)0);
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    glEnable(GL_BLEND);


    prof.add_draw_call();
    prof.add_tri(lev->level->collision.indices.size() / 3);
  }
}