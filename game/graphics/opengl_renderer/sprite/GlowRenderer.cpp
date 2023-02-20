#include "GlowRenderer.h"

#include "third-party/imgui/imgui.h"

GlowRenderer::GlowRenderer() {
  m_vertex_buffer.resize(kMaxVertices);
  m_sprite_data_buffer.resize(kMaxSprites);
  m_index_buffer.resize(kMaxIndices);

  // buffer setup
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

  // probe fbo setup
  glGenFramebuffers(1, &m_ogl.probe_fbo);
  glBindFramebuffer(GL_FRAMEBUFFER, m_ogl.probe_fbo);
  glGenTextures(1, &m_ogl.probe_fbo_rgba_tex);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, m_ogl.probe_fbo_rgba_tex);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, m_ogl.probe_fbo_w, m_ogl.probe_fbo_h, 0, GL_RGBA,
               GL_UNSIGNED_BYTE, nullptr);
  glGenRenderbuffers(1, &m_ogl.probe_fbo_zbuf_rb);
  glBindRenderbuffer(GL_RENDERBUFFER, m_ogl.probe_fbo_zbuf_rb);
  glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT24, m_ogl.probe_fbo_w,
                        m_ogl.probe_fbo_h);
  glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER,
                            m_ogl.probe_fbo_zbuf_rb);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D,
                         m_ogl.probe_fbo_rgba_tex, 0);
  GLenum render_targets[1] = {GL_COLOR_ATTACHMENT0};
  glDrawBuffers(1, render_targets);
  auto status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
  ASSERT(status == GL_FRAMEBUFFER_COMPLETE);

  // downsample fbo setup
  glGenFramebuffers(1, &m_ogl.downsample_fbo);
  glBindFramebuffer(GL_FRAMEBUFFER, m_ogl.downsample_fbo);
  glGenTextures(1, &m_ogl.downsample_fbo_tex);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, m_ogl.downsample_fbo_tex);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, 32, 32, 0, GL_RGBA, GL_UNSIGNED_BYTE, nullptr);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D,
                         m_ogl.downsample_fbo_tex, 0);
  glDrawBuffers(1, render_targets);
  status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
  ASSERT(status == GL_FRAMEBUFFER_COMPLETE);

  glBindFramebuffer(GL_FRAMEBUFFER, 0);
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

  {  // second draw is a GS sprite to clear the alpha. Need to convert to triangle strip:
    u32 idx_start = m_next_vertex;
    Vertex* vtx = alloc_vtx(4);
    for (int i = 0; i < 4; i++) {
      vtx[i].r = 0.f;  // debug
      vtx[i].g = 1.f;
      vtx[i].b = 0.f;
      vtx[i].a = 1.f;
    }
    copy_to_vertex(vtx, data.second_clear_pos[0]);
    copy_to_vertex(vtx + 1, data.second_clear_pos[0]);
    vtx[1].x = data.second_clear_pos[1].x();
    copy_to_vertex(vtx + 2, data.second_clear_pos[0]);
    vtx[2].y = data.second_clear_pos[1].y();
    copy_to_vertex(vtx + 3, data.second_clear_pos[1]);

    u32* idx = alloc_index(5);
    idx[0] = idx_start;
    idx[1] = idx_start + 1;
    idx[2] = idx_start + 2;
    idx[3] = idx_start + 3;
    idx[4] = UINT32_MAX;
  }
}

void GlowRenderer::add_sprite_pass_2(const SpriteGlowOutput& data) {
  //  auto& bottom_corner = data.offscreen_uv[0];
  //  auto dim = data.offscreen_uv[1] - bottom_corner;

  u32 idx_start = m_next_vertex;
  Vertex* vtx = alloc_vtx(4);
  for (int i = 0; i < 4; i++) {
    vtx[i].r = 1.f;  // debug
    vtx[i].g = 0.f;
    vtx[i].b = 0.f;
    vtx[i].a = 0.f;
    vtx[i].x = -1;
    vtx[i].y = -1;
    vtx[i].z = 0;
    vtx[i].w = 0;
  }
  vtx[1].x = 1;
  vtx[2].y = 1;
  vtx[3].x = 1;
  vtx[3].y = 1;

  vtx[0].u = data.offscreen_uv[0][0];
  vtx[0].v = data.offscreen_uv[0][1];
  vtx[1].u = data.offscreen_uv[1][0];
  vtx[1].v = data.offscreen_uv[0][1];
  vtx[2].u = data.offscreen_uv[0][0];
  vtx[2].v = data.offscreen_uv[1][1];
  vtx[3].u = data.offscreen_uv[1][0];
  vtx[3].v = data.offscreen_uv[1][1];

  u32* idx = alloc_index(5);
  idx[0] = idx_start;
  idx[1] = idx_start + 1;
  idx[2] = idx_start + 2;
  idx[3] = idx_start + 3;
  idx[4] = UINT32_MAX;
}

void GlowRenderer::blit_depth(SharedRenderState* render_state) {
  if (m_ogl.probe_fbo_w != render_state->render_fb_w ||
      m_ogl.probe_fbo_h != render_state->render_fb_h) {
    m_ogl.probe_fbo_w = render_state->render_fb_w;
    m_ogl.probe_fbo_h = render_state->render_fb_h;

    glBindTexture(GL_TEXTURE_2D, m_ogl.probe_fbo_rgba_tex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, m_ogl.probe_fbo_w, m_ogl.probe_fbo_h, 0, GL_RGBA,
                 GL_UNSIGNED_BYTE, NULL);
    glBindTexture(GL_TEXTURE_2D, 0);

    glBindRenderbuffer(GL_RENDERBUFFER, m_ogl.probe_fbo_zbuf_rb);
    glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT24, m_ogl.probe_fbo_w,
                          m_ogl.probe_fbo_h);
  }

  glBindFramebuffer(GL_READ_FRAMEBUFFER, render_state->render_fb);
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, m_ogl.probe_fbo);

  glBlitFramebuffer(render_state->render_fb_x,                              // srcX0
                    render_state->render_fb_y,                              // srcY0
                    render_state->render_fb_x + render_state->render_fb_w,  // srcX1
                    render_state->render_fb_y + render_state->render_fb_h,  // srcY1
                    0,                                                      // dstX0
                    0,                                                      // dstY0
                    m_ogl.probe_fbo_w,                                      // dstX1
                    m_ogl.probe_fbo_h,                                      // dstY1
                    GL_DEPTH_BUFFER_BIT,                                    // mask
                    GL_NEAREST                                              // filter
  );
}

void GlowRenderer::draw_debug_window() {
  ImGui::Checkbox("Show Probes", &m_debug.show_probes);
  ImGui::Text("Count: %d", m_debug.num_sprites);
  //  ImGui::InputFloat("hack", &m_debug.hack);
  //  ImGui::InputFloat("hack2", &m_debug.hack2);
}

void GlowRenderer::flush(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  m_debug.num_sprites = m_next_sprite;
  if (!m_next_sprite) {
    // no sprites submitted.
    return;
  }
  blit_depth(render_state);
  glBindFramebuffer(GL_FRAMEBUFFER, m_ogl.probe_fbo);

  u32 probe_idx_start = m_next_index;
  for (u32 sidx = 0; sidx < m_next_sprite; sidx++) {
    add_sprite_pass_1(m_sprite_data_buffer[sidx]);
  }

  u32 copy_idx_start = m_next_index;
  for (u32 sidx = 0; sidx < m_next_sprite; sidx++) {
    add_sprite_pass_2(m_sprite_data_buffer[sidx]);
  }

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
  prof.add_tri(m_next_sprite * 4);
  glDisable(GL_BLEND);
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_GEQUAL);
  glDrawElements(GL_TRIANGLE_STRIP, copy_idx_start - probe_idx_start, GL_UNSIGNED_INT,
                 (void*)(probe_idx_start * sizeof(u32)));

  // optionally debug draw the probes
  if (m_debug.show_probes) {
    prof.add_draw_call();
    prof.add_tri(m_next_sprite * 4);
    glBindFramebuffer(GL_FRAMEBUFFER, render_state->render_fb);
    glDrawElements(GL_TRIANGLE_STRIP, copy_idx_start - probe_idx_start, GL_UNSIGNED_INT,
                   (void*)(probe_idx_start * sizeof(u32)));
  }

  render_state->shaders[ShaderId::GLOW_PROBE_READ].activate();
  glBindFramebuffer(GL_FRAMEBUFFER, m_ogl.downsample_fbo);
  glBindTexture(GL_TEXTURE_2D, m_ogl.probe_fbo_rgba_tex);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glViewport(0, 0, 32, 32);
  u32 idx = copy_idx_start;
  for (u32 sidx = 0; sidx < m_next_sprite; sidx++) {
    glDrawElements(GL_TRIANGLE_STRIP, 5, GL_UNSIGNED_INT, (void*)(idx * sizeof(u32)));
    idx += 5;
  }
  glViewport(render_state->draw_offset_x, render_state->draw_offset_y,
             render_state->draw_region_w, render_state->draw_region_h);

  if (m_debug.show_probes) {
    render_state->shaders[ShaderId::GLOW_PROBE_READ_DEBUG].activate();
    glBindFramebuffer(GL_FRAMEBUFFER, render_state->render_fb);
    u32 idx = copy_idx_start;
    for (u32 sidx = 0; sidx < m_next_sprite; sidx++) {
      glDrawElements(GL_TRIANGLE_STRIP, 5, GL_UNSIGNED_INT, (void*)(idx * sizeof(u32)));
      idx += 5;
    }
  }

  m_next_vertex = 0;
  m_next_index = 0;
  m_next_sprite = 0;
  glBindFramebuffer(GL_FRAMEBUFFER, render_state->render_fb);
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