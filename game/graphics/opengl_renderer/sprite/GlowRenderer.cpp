#include "GlowRenderer.h"

GlowRenderer::GlowRenderer() {
  m_vertex_buffer.resize(kMaxVertices);
  m_sprite_data_buffer.resize(kMaxSprites);
  m_index_buffer.resize(kMaxIndices);

  glGenBuffers(1, &m_ogl.vertex_buffer);
  glGenVertexArrays(1, &m_ogl.vao);
  glBindVertexArray(m_ogl.vao);
  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.vertex_buffer);
  auto bytes = kMaxVertices * sizeof(Vertex);
  glBufferData(GL_ARRAY_BUFFER, bytes, nullptr, GL_STREAM_DRAW);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0,                          // location 0 in the shader
                        4,                          // 4 floats per vert
                        GL_FLOAT,                   // floats
                        GL_TRUE,                    // normalized, ignored,
                        sizeof(Vertex),             //
                        (void*)offsetof(Vertex, x)  // offset in array
  );

  glEnableVertexAttribArray(1);
  glVertexAttribPointer(1,                          // location 1 in the shader
                        4,                          // 4 color components
                        GL_FLOAT,                   // floats
                        GL_TRUE,                    // normalized, ignored,
                        sizeof(Vertex),             //
                        (void*)offsetof(Vertex, r)  // offset in array
  );

  glEnableVertexAttribArray(2);
  glVertexAttribPointer(2,                          // location 2 in the shader
                        3,                          // 4 color components
                        GL_FLOAT,                   // floats
                        GL_TRUE,                    // normalized, ignored,
                        sizeof(Vertex),             //
                        (void*)offsetof(Vertex, u)  // offset in array (why is this a pointer...)
  );

  glBindBuffer(GL_ARRAY_BUFFER, 0);

  glGenBuffers(1, &m_ogl.index_buffer);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_ogl.index_buffer);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, kMaxIndices * sizeof(u32), nullptr, GL_STREAM_DRAW);

  glBindVertexArray(0);
}

namespace {
void copy_to_vertex(GlowRenderer::Vertex* vtx, const Vector4f& xyzw) {
  vtx->x = xyzw.x();
  vtx->y = xyzw.y();
  vtx->z = xyzw.z();
  vtx->w = xyzw.w();
}
}  // namespace

SpriteGlowOutput* GlowRenderer::alloc_sprite() {
  ASSERT(m_next_sprite < m_sprite_data_buffer.size());
  return &m_sprite_data_buffer[m_next_sprite++];
}

void GlowRenderer::cancel_sprite() {
  ASSERT(m_next_sprite);
  m_next_sprite--;
}

void GlowRenderer::add_sprite_pass_1(const SpriteGlowOutput& data) {
  {  // first draw is a GS sprite to clear the alpha. Need to convert to triangle strip:
    u32 idx_start = m_next_vertex;
    Vertex* vtx = alloc_vtx(4);
    for (int i = 0; i < 4; i++) {
      vtx[i].r = 1.f;  // debug
      vtx[i].g = 0.f;
      vtx[i].b = 0.f;
      vtx[i].a = 0.f;
    }
    copy_to_vertex(vtx, data.first_clear_pos[0]);
    copy_to_vertex(vtx + 1, data.first_clear_pos[0]);
    vtx[1].x = data.first_clear_pos[1].x();
    copy_to_vertex(vtx + 2, data.first_clear_pos[0]);
    vtx[2].y = data.first_clear_pos[1].y();
    copy_to_vertex(vtx + 3, data.first_clear_pos[1]);

    u32* idx = alloc_index(5);
    idx[0] = idx_start;
    idx[1] = idx_start + 1;
    idx[2] = idx_start + 2;
    idx[3] = idx_start + 3;
    idx[4] = UINT32_MAX;
  }
}

void GlowRenderer::flush(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  if (!m_next_sprite) {
    // no sprites submitted.
    return;
  }

  u32 probe_idx_start = m_next_index;
  for (u32 sidx = 0; sidx < m_next_sprite; sidx++) {
    add_sprite_pass_1(m_sprite_data_buffer[sidx]);
  }
  prof.add_tri(m_next_sprite * 4);
  u32 copy_idx_start = m_next_index;

  glBindVertexArray(m_ogl.vao);
  glEnable(GL_PRIMITIVE_RESTART);
  glPrimitiveRestartIndex(UINT32_MAX);
  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, m_next_vertex * sizeof(Vertex), m_vertex_buffer.data(),
               GL_STREAM_DRAW);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_ogl.index_buffer);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, m_next_index * sizeof(u32), m_index_buffer.data(),
               GL_STREAM_DRAW);

  // do probes
  render_state->shaders[ShaderId::GLOW_PROBE].activate();
  prof.add_draw_call();
  glDisable(GL_BLEND);
  glDisable(GL_DEPTH_TEST);
  glDrawElements(GL_TRIANGLE_STRIP, copy_idx_start - probe_idx_start, GL_UNSIGNED_INT,
                 (void*)(probe_idx_start * sizeof(u32)));

  // Draw 1 and 2

  m_next_vertex = 0;
  m_next_index = 0;
  m_next_sprite = 0;
}

GlowRenderer::Vertex* GlowRenderer::alloc_vtx(int num) {
  ASSERT(m_next_vertex + num <= m_vertex_buffer.size());
  auto* result = &m_vertex_buffer[m_next_vertex];
  m_next_vertex += num;
  return result;
}

u32* GlowRenderer::alloc_index(int num) {
  ASSERT(m_next_index + num <= m_index_buffer.size());
  auto* result = &m_index_buffer[m_next_index];
  m_next_index += num;
  return result;
}