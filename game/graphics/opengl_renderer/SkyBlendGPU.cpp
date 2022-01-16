#include "SkyBlendGPU.h"

#include "common/log/log.h"
#include "game/graphics/opengl_renderer/AdgifHandler.h"

SkyBlendGPU::SkyBlendGPU() {
  // generate textures for sky blending
  glGenFramebuffers(2, m_framebuffers);
  glGenTextures(2, m_textures);

  GLint old_framebuffer;
  glGetIntegerv(GL_FRAMEBUFFER_BINDING, &old_framebuffer);

  // setup the framebuffers
  for (int i = 0; i < 2; i++) {
    glBindFramebuffer(GL_FRAMEBUFFER, m_framebuffers[i]);
    glBindTexture(GL_TEXTURE_2D, m_textures[i]);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, m_sizes[i], m_sizes[i], 0, GL_RGBA,
                 GL_UNSIGNED_INT_8_8_8_8_REV, 0);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, m_textures[i], 0);
    GLenum draw_buffers[1] = {GL_COLOR_ATTACHMENT0};
    glDrawBuffers(1, draw_buffers);
    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
      lg::error("SkyTextureHandler setup failed.");
    }
  }
  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  glGenBuffers(1, &m_gl_vertex_buffer);
  glBindBuffer(GL_ARRAY_BUFFER, m_gl_vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, sizeof(Vertex) * 6, nullptr, GL_DYNAMIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, old_framebuffer);

  // we only draw squares
  m_vertex_data[0].x = 0;
  m_vertex_data[0].y = 0;

  m_vertex_data[1].x = 1;
  m_vertex_data[1].y = 0;

  m_vertex_data[2].x = 0;
  m_vertex_data[2].y = 1;

  m_vertex_data[3].x = 1;
  m_vertex_data[3].y = 0;

  m_vertex_data[4].x = 0;
  m_vertex_data[4].y = 1;

  m_vertex_data[5].x = 1;
  m_vertex_data[5].y = 1;
}

SkyBlendGPU::~SkyBlendGPU() {
  glDeleteFramebuffers(2, m_framebuffers);
  glDeleteBuffers(1, &m_gl_vertex_buffer);
  glDeleteTextures(2, m_textures);
}

SkyBlendStats SkyBlendGPU::do_sky_blends(DmaFollower& dma,
                                         SharedRenderState* render_state,
                                         ScopedProfilerNode& prof) {
  SkyBlendStats stats;
  GLuint vao;
  glGenVertexArrays(1, &vao);
  glBindVertexArray(vao);

  GLint old_viewport[4];
  glGetIntegerv(GL_VIEWPORT, old_viewport);

  GLint old_framebuffer;
  glGetIntegerv(GL_FRAMEBUFFER_BINDING, &old_framebuffer);

  while (dma.current_tag().qwc == 6) {
    // assuming that the vif and gif-tag is correct
    auto setup_data = dma.read_and_advance();
    if (render_state->dump_playback) {
      // continue;
    }

    // first is an adgif
    AdgifHelper adgif(setup_data.data + 16);
    assert(adgif.is_normal_adgif());
    assert(adgif.alpha().data == 0x8000000068);  // Cs + Cd

    // next is the actual draw
    auto draw_data = dma.read_and_advance();
    assert(draw_data.size_bytes == 6 * 16);

    GifTag draw_or_blend_tag(draw_data.data);

    // the first draw overwrites the previous frame's draw by disabling alpha blend (ABE = 0)
    bool is_first_draw = !GsPrim(draw_or_blend_tag.prim()).abe();

    // here's we're relying on the format of the drawing to get the alpha/offset.
    u32 coord;
    u32 intensity;
    memcpy(&coord, draw_data.data + (5 * 16), 4);
    memcpy(&intensity, draw_data.data + 16, 4);

    // we didn't parse the render-to-texture setup earlier, so we need a way to tell sky from
    // clouds. we can look at the drawing coordinates to tell - the sky is smaller than the clouds.
    int buffer_idx = 0;
    if (coord == 0x200) {
      // sky
      buffer_idx = 0;
    } else if (coord == 0x400) {
      buffer_idx = 1;
    } else {
      assert(false);  // bad data
    }

    // look up the source texture
    auto tex = render_state->texture_pool->lookup(adgif.tex0().tbp0());
    assert(tex);

    if (!tex->on_gpu) {
      render_state->texture_pool->upload_to_gpu(tex);
    }

    // setup for rendering!
    glBindFramebuffer(GL_FRAMEBUFFER, m_framebuffers[buffer_idx]);
    glViewport(0, 0, m_sizes[buffer_idx], m_sizes[buffer_idx]);
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, m_textures[buffer_idx], 0);
    render_state->shaders[ShaderId::SKY_BLEND].activate();

    // if the first is set, it disables alpha. we can just clear here, so it's easier to find
    // in renderdoc.
    if (is_first_draw) {
      float clear[4] = {0, 0, 0, 0};
      glClearBufferfv(GL_COLOR, 0, clear);
    }

    // intensities should be 0-128 (maybe higher is okay, but I don't see how this could be
    // generated with the GOAL code.)
    assert(intensity <= 128);

    // todo - could do this on the GPU, but probably not worth it for <20 triangles...
    float intensity_float = intensity / 128.f;
    for (auto& vert : m_vertex_data) {
      vert.intensity = intensity_float;
    }

    glDisable(GL_DEPTH_TEST);
    glEnable(GL_BLEND);

    // will add.
    glBlendFunc(GL_ONE, GL_ONE);

    // setup draw data
    glBindBuffer(GL_ARRAY_BUFFER, m_gl_vertex_buffer);
    glBufferData(GL_ARRAY_BUFFER, sizeof(Vertex) * 6, m_vertex_data, GL_STREAM_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0,         // location 0 in the shader
                          3,         // 3 floats per vert
                          GL_FLOAT,  // floats
                          GL_TRUE,   // normalized, ignored,
                          0,         // tightly packed
                          0

    );
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, tex->gpu_texture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    // Draw a sqaure
    glDrawArrays(GL_TRIANGLES, 0, 6);

    // 1 draw, 2 triangles
    prof.add_draw_call(1);
    prof.add_tri(2);

    if (buffer_idx == 0) {
      if (is_first_draw) {
        stats.sky_draws++;
      } else {
        stats.sky_blends++;
      }
    } else {
      if (is_first_draw) {
        stats.cloud_draws++;
      } else {
        stats.cloud_blends++;
      }
    }
  }

  // put in pool.
  for (int i = 0; i < 2; i++) {
    // todo - these are hardcoded and rely on the vram layout.
    u32 tbp = i == 0 ? 8064 : 8096;

    // lookup existing, or create a new entry
    TextureRecord* tex = render_state->texture_pool->lookup(tbp);
    if (!tex) {
      auto tsp = std::make_shared<TextureRecord>();
      render_state->texture_pool->set_texture(tbp, tsp);
      tex = tsp.get();
    }

    // update it
    tex->gpu_texture = m_textures[i];
    tex->on_gpu = true;
    tex->only_on_gpu = true;
    tex->do_gc = false;
    tex->w = m_sizes[i];
    tex->h = m_sizes[i];
    tex->name = fmt::format("PC-SKY-{}", i);
  }

  glViewport(old_viewport[0], old_viewport[1], old_viewport[2], old_viewport[3]);
  glBindFramebuffer(GL_FRAMEBUFFER, old_framebuffer);
  glBindVertexArray(0);
  glDeleteVertexArrays(1, &vao);

  return stats;
}
