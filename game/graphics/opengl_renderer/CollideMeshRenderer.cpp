#include "CollideMeshRenderer.h"

#include "game/graphics/opengl_renderer/background/background_common.h"

float material_colors_jak1[23 * 3] = {
    1.0f,  0.7f,  1.0f,   // 0, stone
    0.1f,  2.0f,  2.0f,   // 1, ice
    0.75f, 0.25f, 0.1f,   // 2, quicksand
    0.1f,  0.25f, 0.75f,  // 3, waterbottom
    0.5f,  0.15f, 0.1f,   // 4, tar
    2.0f,  1.5f,  0.5f,   // 5, sand
    1.5f,  0.75f, 0.1f,   // 6, wood
    0.1f,  1.35f, 0.1f,   // 7, grass
    1.7f,  1.3f,  0.1f,   // 8, pcmetal
    1.8f,  1.8f,  1.8f,   // 9, snow
    1.5f,  0.2f,  1.0f,   // 10, deepsnow
    1.2f,  0.5f,  0.3f,   // 11, hotcoals
    1.4f,  0.1f,  0.1f,   // 12, lava
    0.8f,  0.3f,  0.1f,   // 13, crwood
    1.0f,  0.4f,  1.0f,   // 14, gravel
    1.5f,  0.5f,  0.15f,  // 15, dirt
    0.7f,  0.7f,  1.0f,   // 16, metal
    0.1f,  0.1f,  1.2f,   // 17, straw
    0.75f, 1.75f, 0.75f,  // 18, tube
    0.4f,  0.1f,  0.8f,   // 19, swamp
    0.1f,  0.4f,  0.8f,   // 20, stopproj
    1.9f,  0.1f,  1.9f,   // 21, rotate
    1.0f,  1.0f,  1.0f,   // 22, neutral
};

float event_colors_jak1[7 * 3] = {
    1.0f, 1.0f, 1.0f,  // 0, none
    0.2f, 1.0f, 1.0f,  // 1, deadly
    0.1f, 1.0f, 0.1f,  // 2, endlessfall
    1.0f, 1.0f, 0.1f,  // 3, burn
    0.1f, 0.1f, 1.0f,  // 4, deadlyup
    1.0f, 0.1f, 0.5f,  // 5, burnup
    1.0f, 0.1f, 0.1f,  // 6, melt
};

float mode_colors_jak1[3 * 3] = {
    1.25f, 0.1f, 0.1f,  // 0, ground
    0.1f,  0.1f, 1.0f,  // 1, wall
    1.0f,  0.1f, 1.0f,  // 2, obstacle
};

CollideMeshRenderer::CollideMeshRenderer(GameVersion version) {
  glGenVertexArrays(1, &m_vao);
  glGenBuffers(1, &m_ubo);

  init_pat_colors(version);

  glBindBuffer(GL_UNIFORM_BUFFER, m_ubo);
  glBufferData(GL_UNIFORM_BUFFER, sizeof(m_colors), &m_colors, GL_DYNAMIC_DRAW);
  glBindBuffer(GL_UNIFORM_BUFFER, 0);
}

CollideMeshRenderer::~CollideMeshRenderer() {
  glDeleteVertexArrays(1, &m_vao);
  glDeleteBuffers(1, &m_ubo);
}

void CollideMeshRenderer::init_pat_colors(GameVersion version) {
  for (int i = 0; i < 0x8; ++i) {
    m_colors.pat_mode_colors[i].x() = -1.f;
    m_colors.pat_mode_colors[i].y() = -1.f;
    m_colors.pat_mode_colors[i].z() = -1.f;
  }
  for (int i = 0; i < 0x40; ++i) {
    m_colors.pat_material_colors[i].x() = -1.f;
    m_colors.pat_material_colors[i].y() = -1.f;
    m_colors.pat_material_colors[i].z() = -1.f;
  }
  for (int i = 0; i < 0x40; ++i) {
    m_colors.pat_event_colors[i].x() = -1.f;
    m_colors.pat_event_colors[i].y() = -1.f;
    m_colors.pat_event_colors[i].z() = -1.f;
  }

  switch (version) {
    case GameVersion::Jak1:
      for (int i = 0; i < 23 * 3; ++i) {
        m_colors.pat_material_colors[i / 3].data()[i % 3] = material_colors_jak1[i];
      }
      for (int i = 0; i < 7 * 3; ++i) {
        m_colors.pat_event_colors[i / 3].data()[i % 3] = event_colors_jak1[i];
      }
      for (int i = 0; i < 3 * 3; ++i) {
        m_colors.pat_mode_colors[i / 3].data()[i % 3] = mode_colors_jak1[i];
      }
      break;
    case GameVersion::Jak2:
      break;
  }
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
  auto shader = render_state->shaders[ShaderId::COLLISION].id();
  glUniformBlockBinding(shader, glGetUniformBlockIndex(shader, "PatColors"), 0);
  glBindBufferBase(GL_UNIFORM_BUFFER, 0, m_ubo);
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
  glUniform1i(glGetUniformLocation(shader, "version"), (GLint)render_state->version);
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
