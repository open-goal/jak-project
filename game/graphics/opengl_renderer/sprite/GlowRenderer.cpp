#include "GlowRenderer.h"

#include "third-party/imgui/imgui.h"

GlowRenderer::GlowRenderer() {
  m_vertex_buffer.resize(kMaxVertices);
  m_sprite_data_buffer.resize(kMaxSprites);
  m_index_buffer.resize(kMaxIndices);

  // dynamic buffer
  {
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
                          2,                          // 2 uv
                          GL_FLOAT,                   // floats
                          GL_TRUE,                    // normalized, ignored,
                          sizeof(Vertex),             //
                          (void*)offsetof(Vertex, u)  // offset in array (why is this a pointer...)
    );

    glEnableVertexAttribArray(3);
    glVertexAttribPointer(3,                           // location 2 in the shader
                          2,                           // 2 uv
                          GL_FLOAT,                    // floats
                          GL_TRUE,                     // normalized, ignored,
                          sizeof(Vertex),              //
                          (void*)offsetof(Vertex, uu)  // offset in array (why is this a pointer...)
    );

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glGenBuffers(1, &m_ogl.index_buffer);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_ogl.index_buffer);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, kMaxIndices * sizeof(u32), nullptr, GL_STREAM_DRAW);
    glBindVertexArray(0);
  }

  // static buffer
  {
    glGenBuffers(1, &m_ogl_downsampler.vertex_buffer);
    glGenVertexArrays(1, &m_ogl_downsampler.vao);
    glBindVertexArray(m_ogl_downsampler.vao);
    glBindBuffer(GL_ARRAY_BUFFER, m_ogl_downsampler.vertex_buffer);
    std::vector<Vertex> vertices;
    std::vector<u32> indices;
    vertices.resize(kMaxSprites * 4);
    indices.resize(kMaxSprites * 5);
    for (int i = 0; i < kMaxSprites; i++) {
      int x = i / kDownsampleBatchWidth;
      int y = i % kDownsampleBatchWidth;
      float step = 1.f / kDownsampleBatchWidth;
      Vertex* vtx = &vertices.at(i * 4);
      for (int j = 0; j < 4; j++) {
        vtx[j].r = 0.f;  // debug
        vtx[j].g = 0.f;
        vtx[j].b = 0.f;
        vtx[j].a = 0.f;
        vtx[j].x = x * step;
        vtx[j].y = y * step;
        vtx[j].z = 0;
        vtx[j].w = 0;
      }
      vtx[1].x += step;
      vtx[2].y += step;
      vtx[3].x += step;
      vtx[3].y += step;
      indices.at(i * 5 + 0) = i * 4;
      indices.at(i * 5 + 1) = i * 4 + 1;
      indices.at(i * 5 + 2) = i * 4 + 2;
      indices.at(i * 5 + 3) = i * 4 + 3;
      indices.at(i * 5 + 4) = UINT32_MAX;
    }

    glBufferData(GL_ARRAY_BUFFER, vertices.size() * sizeof(Vertex), vertices.data(),
                 GL_STATIC_DRAW);
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
    glGenBuffers(1, &m_ogl_downsampler.index_buffer);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_ogl_downsampler.index_buffer);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, indices.size() * sizeof(u32), indices.data(),
                 GL_STATIC_DRAW);
    glBindVertexArray(0);
  }

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
  int ds_size = kFirstDownsampleSize;
  for (int i = 0; i < kDownsampleIterations; i++) {
    m_ogl.downsample_fbos[i].size = ds_size * kDownsampleBatchWidth;
    glGenFramebuffers(1, &m_ogl.downsample_fbos[i].fbo);
    glBindFramebuffer(GL_FRAMEBUFFER, m_ogl.downsample_fbos[i].fbo);
    glGenTextures(1, &m_ogl.downsample_fbos[i].tex);
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, m_ogl.downsample_fbos[i].tex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, ds_size * kDownsampleBatchWidth,
                 ds_size * kDownsampleBatchWidth, 0, GL_RGBA, GL_UNSIGNED_BYTE, nullptr);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D,
                           m_ogl.downsample_fbos[i].tex, 0);
    glDrawBuffers(1, render_targets);
    status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    ASSERT(status == GL_FRAMEBUFFER_COMPLETE);
    ds_size /= 2;
  }

  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  // from the giftag
  m_default_draw_mode.set_ab(true);

  // leftovers
  //  ;; (new 'static 'gs-test :ate 1 :afail 1 :zte 1 :ztst 2)
  //  (new 'static 'gs-adcmd :cmds (gs-reg64 test-1) :x #x51001)
  m_default_draw_mode.set_at(true);
  m_default_draw_mode.set_alpha_fail(GsTest::AlphaFail::FB_ONLY);
  m_default_draw_mode.set_zt(true);
  m_default_draw_mode.set_depth_test(GsTest::ZTest::GEQUAL);
  m_default_draw_mode.set_alpha_test(DrawMode::AlphaTest::NEVER);

  //  ;; (new 'static 'gs-zbuf :zbp 304 :psm 1 :zmsk 1)
  //  (new 'static 'gs-adcmd :cmds (gs-reg64 zbuf-1) :x #x1000130 :y #x1)
  m_default_draw_mode.disable_depth_write();
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

void GlowRenderer::add_sprite_pass_2(const SpriteGlowOutput& data, int sprite_idx) {
  // output is a grid of kBatchWidth * kBatchWidth.
  // for simplicity, we'll map to (0, 1) here, and the shader will convert to (-1, 1) for opengl.
  int x = sprite_idx / kDownsampleBatchWidth;
  int y = sprite_idx % kDownsampleBatchWidth;
  float step = 1.f / kDownsampleBatchWidth;

  u32 idx_start = m_next_vertex;
  Vertex* vtx = alloc_vtx(4);
  for (int i = 0; i < 4; i++) {
    vtx[i].r = 1.f;  // debug
    vtx[i].g = 0.f;
    vtx[i].b = 0.f;
    vtx[i].a = 0.f;
    vtx[i].x = x * step;
    vtx[i].y = y * step;
    vtx[i].z = 0;
    vtx[i].w = 0;
  }
  vtx[1].x += step;
  vtx[2].y += step;
  vtx[3].x += step;
  vtx[3].y += step;

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

void GlowRenderer::add_sprite_pass_3(const SpriteGlowOutput& data, int sprite_idx) {
  // output is a grid of kBatchWidth * kBatchWidth.
  // for simplicity, we'll map to (0, 1) here, and the shader will convert to (-1, 1) for opengl.
  int x = sprite_idx / kDownsampleBatchWidth;
  int y = sprite_idx % kDownsampleBatchWidth;
  float step = 1.f / kDownsampleBatchWidth;

  u32 idx_start = m_next_vertex;
  Vertex* vtx = alloc_vtx(4);
  for (int i = 0; i < 4; i++) {
    vtx[i].r = data.flare_draw_color[0];
    vtx[i].g = data.flare_draw_color[1];
    vtx[i].b = data.flare_draw_color[2];
    vtx[i].a = data.flare_draw_color[3];
    copy_to_vertex(&vtx[i], data.flare_xyzw[i]);
    vtx[i].u = 0;
    vtx[i].v = 0;
    vtx[i].uu = x * step + step / 2;
    vtx[i].vv = y * step + step / 2;
  }
  vtx[1].u = 1;
  vtx[3].v = 1;
  vtx[2].u = 1;
  vtx[2].v = 1;

  auto& record = m_sprite_records[sprite_idx];
  record.draw_mode = m_default_draw_mode;
  record.tbp = 0;
  record.idx = m_next_index;

  u32* idx = alloc_index(5);
  // flip first two - fan -> strip
  idx[0] = idx_start + 1;
  idx[1] = idx_start + 0;
  idx[2] = idx_start + 2;
  idx[3] = idx_start + 3;
  idx[4] = UINT32_MAX;

  // handle adgif stuff
  {
    ASSERT(data.adgif.tex0_addr == (u32)GsRegisterAddress::TEX0_1);
    GsTex0 reg(data.adgif.tex0_data);
    record.tbp = reg.tbp0();
    record.draw_mode.set_tcc(reg.tcc());
    // shader is hardcoded for this right now.
    ASSERT(reg.tcc() == 1);
    ASSERT(reg.tfx() == GsTex0::TextureFunction::MODULATE);
  }

  {
    ASSERT((u8)data.adgif.tex1_addr == (u8)GsRegisterAddress::TEX1_1);
    GsTex1 reg(data.adgif.tex1_data);
    record.draw_mode.set_filt_enable(reg.mmag());
  }

  {
    ASSERT(data.adgif.mip_addr == (u32)GsRegisterAddress::MIPTBP1_1);
    // ignore
  }

  // clamp or zbuf
  if (GsRegisterAddress(data.adgif.clamp_addr) == GsRegisterAddress::ZBUF_1) {
    GsZbuf x(data.adgif.clamp_data);
    record.draw_mode.set_depth_write_enable(!x.zmsk());
  } else if (GsRegisterAddress(data.adgif.clamp_addr) == GsRegisterAddress::CLAMP_1) {
    u32 val = data.adgif.clamp_data;
    if (!(val == 0b101 || val == 0 || val == 1 || val == 0b100)) {
      ASSERT_MSG(false, fmt::format("clamp: 0x{:x}", val));
    }
    record.draw_mode.set_clamp_s_enable(val & 0b001);
    record.draw_mode.set_clamp_t_enable(val & 0b100);
  } else {
    ASSERT(false);
  }

  // alpha
  ASSERT(data.adgif.alpha_addr == (u32)GsRegisterAddress::ALPHA_1);  // ??

  // ;; a = 0, b = 2, c = 1, d = 1
  // Cv = (Cs - 0) * Ad + D
  // leaving out the multiply by Ad.
  record.draw_mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_0_FIX_DST);
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
  ImGui::Checkbox("Show Copy", &m_debug.show_probe_copies);

  ImGui::Text("Count: %d", m_debug.num_sprites);
}

void GlowRenderer::downsample_chain(SharedRenderState* render_state,
                                    ScopedProfilerNode& prof,
                                    u32 num_sprites) {
  glBindVertexArray(m_ogl_downsampler.vao);
  glEnable(GL_PRIMITIVE_RESTART);
  glPrimitiveRestartIndex(UINT32_MAX);
  render_state->shaders[ShaderId::GLOW_PROBE_DOWNSAMPLE].activate();
  for (int i = 0; i < kDownsampleIterations - 1; i++) {
    auto* source = &m_ogl.downsample_fbos[i];
    auto* dest = &m_ogl.downsample_fbos[i + 1];
    glBindTexture(GL_TEXTURE_2D, source->tex);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glBindFramebuffer(GL_FRAMEBUFFER, dest->fbo);
    glViewport(0, 0, dest->size, dest->size);
    prof.add_draw_call();
    prof.add_tri(num_sprites * 4);
    glDrawElements(GL_TRIANGLE_STRIP, num_sprites * 5, GL_UNSIGNED_INT, nullptr);
  }
  glViewport(render_state->draw_offset_x, render_state->draw_offset_y, render_state->draw_region_w,
             render_state->draw_region_h);
}

void GlowRenderer::draw_probes(SharedRenderState* render_state,
                               ScopedProfilerNode& prof,
                               u32 idx_start,
                               u32 idx_end) {
  glBindFramebuffer(GL_FRAMEBUFFER, m_ogl.probe_fbo);
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
  glDrawElements(GL_TRIANGLE_STRIP, idx_end - idx_start, GL_UNSIGNED_INT,
                 (void*)(idx_start * sizeof(u32)));
}

void GlowRenderer::debug_draw_probes(SharedRenderState* render_state,
                                     ScopedProfilerNode& prof,
                                     u32 idx_start,
                                     u32 idx_end) {
  prof.add_draw_call();
  prof.add_tri(m_next_sprite * 4);
  glBindFramebuffer(GL_FRAMEBUFFER, render_state->render_fb);
  glDrawElements(GL_TRIANGLE_STRIP, idx_end - idx_start, GL_UNSIGNED_INT,
                 (void*)(idx_start * sizeof(u32)));
}

void GlowRenderer::draw_probe_copies(SharedRenderState* render_state,
                                     ScopedProfilerNode& prof,
                                     u32 idx_start,
                                     u32 idx_end) {
  // read probe from probe fbo, write it to the first downsample fbo
  render_state->shaders[ShaderId::GLOW_PROBE_READ].activate();
  glBindFramebuffer(GL_FRAMEBUFFER, m_ogl.downsample_fbos[0].fbo);
  glBindTexture(GL_TEXTURE_2D, m_ogl.probe_fbo_rgba_tex);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glViewport(0, 0, m_ogl.downsample_fbos[0].size, m_ogl.downsample_fbos[0].size);
  prof.add_draw_call();
  prof.add_tri(m_next_sprite * 2);
  glDrawElements(GL_TRIANGLE_STRIP, idx_end - idx_start, GL_UNSIGNED_INT,
                 (void*)(idx_start * sizeof(u32)));
  glViewport(render_state->draw_offset_x, render_state->draw_offset_y, render_state->draw_region_w,
             render_state->draw_region_h);
}

void GlowRenderer::debug_draw_probe_copies(SharedRenderState* render_state,
                                           ScopedProfilerNode& prof,
                                           u32 idx_start,
                                           u32 idx_end) {
  render_state->shaders[ShaderId::GLOW_PROBE_READ_DEBUG].activate();
  prof.add_draw_call();
  prof.add_tri(m_next_sprite * 2);
  glBindFramebuffer(GL_FRAMEBUFFER, render_state->render_fb);
  glDrawElements(GL_TRIANGLE_STRIP, idx_end - idx_start, GL_UNSIGNED_INT,
                 (void*)(idx_start * sizeof(u32)));
}

void GlowRenderer::flush(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  m_debug.num_sprites = m_next_sprite;
  if (!m_next_sprite) {
    // no sprites submitted.
    return;
  }

  // copy depth from framebuffer to a temporary buffer
  // (this is a bit wasteful)
  blit_depth(render_state);

  // generate vertex/index data for probes
  u32 probe_idx_start = m_next_index;
  for (u32 sidx = 0; sidx < m_next_sprite; sidx++) {
    add_sprite_pass_1(m_sprite_data_buffer[sidx]);
  }

  // generate vertex/index data for copy to downsample buffer
  u32 copy_idx_start = m_next_index;
  for (u32 sidx = 0; sidx < m_next_sprite; sidx++) {
    add_sprite_pass_2(m_sprite_data_buffer[sidx], sidx);
  }
  u32 copy_idx_end = m_next_index;

  // generate vertex/index data for framebuffer draws
  u32 main_draw_idx_start = m_next_index;
  for (u32 sidx = 0; sidx < m_next_sprite; sidx++) {
    add_sprite_pass_3(m_sprite_data_buffer[sidx], sidx);
  }
  u32 main_draw_idx_end = m_next_index;

  // draw probes
  draw_probes(render_state, prof, probe_idx_start, copy_idx_start);
  if (m_debug.show_probes) {
    debug_draw_probes(render_state, prof, probe_idx_start, copy_idx_start);
  }

  // copy probes
  draw_probe_copies(render_state, prof, copy_idx_start, copy_idx_end);
  if (m_debug.show_probe_copies) {
    debug_draw_probe_copies(render_state, prof, copy_idx_start, copy_idx_end);
  }

  // downsample probes.
  downsample_chain(render_state, prof, m_next_sprite);

  draw_sprites(render_state, prof, main_draw_idx_start, main_draw_idx_end);

  m_next_vertex = 0;
  m_next_index = 0;
  m_next_sprite = 0;
  glBindFramebuffer(GL_FRAMEBUFFER, render_state->render_fb);
}

void GlowRenderer::draw_sprites(SharedRenderState* render_state,
                                ScopedProfilerNode& prof,
                                u32 idx_start,
                                u32 idx_end) {
  glBindFramebuffer(GL_FRAMEBUFFER, render_state->render_fb);
  glBindVertexArray(m_ogl.vao);
  glEnable(GL_PRIMITIVE_RESTART);
  glPrimitiveRestartIndex(UINT32_MAX);

  glActiveTexture(GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_2D, m_ogl.downsample_fbos[kDownsampleIterations - 1].tex);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  render_state->shaders[ShaderId::GLOW_DRAW].activate();
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_GEQUAL);
  glEnable(GL_BLEND);
  // Cv = (Cs - 0) * Ad + D
  glBlendFunc(GL_ONE, GL_ONE);
  glBlendEquation(GL_FUNC_ADD);

  glDepthMask(GL_FALSE);

  for (u32 i = 0; i < m_next_sprite; i++) {
    const auto& record = m_sprite_records[i];
    auto tex = render_state->texture_pool->lookup(record.tbp);
    if (!tex) {
      fmt::print("Failed to find texture at {}, using random", record.tbp);
      tex = render_state->texture_pool->get_placeholder_texture();
    }
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, *tex);
    if (record.draw_mode.get_clamp_s_enable()) {
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    } else {
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    }

    if (record.draw_mode.get_clamp_t_enable()) {
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    } else {
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    }

    if (record.draw_mode.get_filt_enable()) {
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    } else {
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    }

    prof.add_draw_call();
    prof.add_tri(2);
    glDrawElements(GL_TRIANGLE_STRIP, 5, GL_UNSIGNED_INT, (void*)(record.idx * sizeof(u32)));
  }

  /*
  prof.add_draw_call();
  prof.add_tri(m_next_sprite * 4);
  glDisable(GL_BLEND);
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_GEQUAL);
  glDrawElements(GL_TRIANGLE_STRIP, idx_end - idx_start, GL_UNSIGNED_INT,
                 (void*)(idx_start * sizeof(u32)));
                 */
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