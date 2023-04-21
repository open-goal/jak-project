#include "opengl_utils.h"

#include <array>
#include <cstdio>

#include "common/util/Assert.h"

#include "game/graphics/opengl_renderer/BucketRenderer.h"

FramebufferTexturePair::FramebufferTexturePair(int w, int h, u64 texture_format, int num_levels)
    : m_w(w), m_h(h) {
  m_framebuffers.resize(num_levels);
  glGenFramebuffers(num_levels, m_framebuffers.data());
  glGenTextures(1, &m_texture);

  GLint old_framebuffer;
  glGetIntegerv(GL_FRAMEBUFFER_BINDING, &old_framebuffer);

  for (int i = 0; i < num_levels; i++) {
    glBindTexture(GL_TEXTURE_2D, m_texture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, num_levels);
    glTexImage2D(GL_TEXTURE_2D, i, GL_RGBA, w >> i, h >> i, 0, GL_RGBA, texture_format, nullptr);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  }

  for (int i = 0; i < num_levels; i++) {
    glBindTexture(GL_TEXTURE_2D, 0);
    glBindFramebuffer(GL_FRAMEBUFFER, m_framebuffers[i]);
    glBindTexture(GL_TEXTURE_2D, m_texture);
    // I don't know if we really need to do this. whatever uses this texture should figure it out.

    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + i, m_texture, i);
    GLenum draw_buffers[1] = {GLenum(GL_COLOR_ATTACHMENT0 + i)};
    glDrawBuffers(1, draw_buffers);
    auto status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
      switch (status) {
        case GL_FRAMEBUFFER_UNDEFINED:
          printf("GL_FRAMEBUFFER_UNDEFINED\n");
          break;
        case GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT:
          printf("GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT\n");
          break;
        case GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT:
          printf("GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT\n");
          break;
        case GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER:
          printf("GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER\n");
          break;
        case GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER:
          printf("GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER\n");
          break;
        case GL_FRAMEBUFFER_UNSUPPORTED:
          printf("GL_FRAMEBUFFER_UNSUPPORTED\n");
          break;
        case GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE:
          printf("GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE\n");
          break;
        case GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS:
          printf("GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS\n");
          break;
      }

      ASSERT(false);
    }
    glBindFramebuffer(GL_FRAMEBUFFER, old_framebuffer);
  }

  glBindFramebuffer(GL_FRAMEBUFFER, old_framebuffer);
}

FramebufferTexturePair::~FramebufferTexturePair() {
  glDeleteFramebuffers(m_framebuffers.size(), m_framebuffers.data());
  glDeleteTextures(1, &m_texture);
}

FramebufferTexturePairContext::FramebufferTexturePairContext(FramebufferTexturePair& fb, int level)
    : m_fb(&fb) {
  glGetIntegerv(GL_VIEWPORT, m_old_viewport);
  glGetIntegerv(GL_FRAMEBUFFER_BINDING, &m_old_framebuffer);
  glBindFramebuffer(GL_FRAMEBUFFER, m_fb->m_framebuffers[level]);
  glViewport(0, 0, m_fb->m_w, m_fb->m_h);
  glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, m_fb->m_texture, level);
}

void FramebufferTexturePairContext::switch_to(FramebufferTexturePair& fb) {
  if (&fb != m_fb) {
    m_fb = &fb;
    glBindFramebuffer(GL_FRAMEBUFFER, m_fb->m_framebuffers[0]);
    glViewport(0, 0, m_fb->m_w, m_fb->m_h);
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, m_fb->m_texture, 0);
  }
}

FramebufferTexturePairContext::~FramebufferTexturePairContext() {
  glViewport(m_old_viewport[0], m_old_viewport[1], m_old_viewport[2], m_old_viewport[3]);
  glBindFramebuffer(GL_FRAMEBUFFER, m_old_framebuffer);
}

FullScreenDraw::FullScreenDraw() {
  glGenVertexArrays(1, &m_vao);
  glGenBuffers(1, &m_vertex_buffer);
  glBindVertexArray(m_vao);

  struct Vertex {
    float x, y;
  };

  std::array<Vertex, 4> vertices = {
      Vertex{-1, -1},
      Vertex{-1, 1},
      Vertex{1, -1},
      Vertex{1, 1},
  };

  glBindBuffer(GL_ARRAY_BUFFER, m_vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, sizeof(Vertex) * 4, vertices.data(), GL_STATIC_DRAW);

  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0,               // location 0 in the shader
                        2,               // 2 floats per vert
                        GL_FLOAT,        // floats
                        GL_TRUE,         // normalized, ignored,
                        sizeof(Vertex),  //
                        nullptr          //
  );

  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
}

FullScreenDraw::~FullScreenDraw() {
  glDeleteVertexArrays(1, &m_vao);
  glDeleteBuffers(1, &m_vertex_buffer);
}

void FullScreenDraw::draw(const math::Vector4f& color,
                          SharedRenderState* render_state,
                          ScopedProfilerNode& prof) {
  glBindVertexArray(m_vao);
  glBindBuffer(GL_ARRAY_BUFFER, m_vertex_buffer);
  auto& shader = render_state->shaders[ShaderId::SOLID_COLOR];
  shader.activate();
  glUniform4f(glGetUniformLocation(shader.id(), "fragment_color"), color[0], color[1], color[2],
              color[3]);

  prof.add_tri(2);
  prof.add_draw_call();
  glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
}

FramebufferCopier::FramebufferCopier() {
  glGenFramebuffers(1, &m_fbo);
  glBindFramebuffer(GL_FRAMEBUFFER, m_fbo);

  glGenTextures(1, &m_fbo_texture);
  glBindTexture(GL_TEXTURE_2D, m_fbo_texture);

  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, m_fbo_width, m_fbo_height, 0, GL_RGB, GL_UNSIGNED_BYTE,
               NULL);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, m_fbo_texture, 0);

  ASSERT(glCheckFramebufferStatus(GL_FRAMEBUFFER) == GL_FRAMEBUFFER_COMPLETE);

  glBindTexture(GL_TEXTURE_2D, 0);
}

FramebufferCopier::~FramebufferCopier() {
  glDeleteTextures(1, &m_fbo_texture);
  glDeleteFramebuffers(1, &m_fbo);
}

void FramebufferCopier::copy_now(int render_fb_w,
                                 int render_fb_h,
                                 int render_fb_x,
                                 int render_fb_y,
                                 GLuint render_fb) {
  if (m_fbo_width != render_fb_w || m_fbo_height != render_fb_h) {
    m_fbo_width = render_fb_w;
    m_fbo_height = render_fb_h;

    glBindTexture(GL_TEXTURE_2D, m_fbo_texture);

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, m_fbo_width, m_fbo_height, 0, GL_RGB, GL_UNSIGNED_BYTE,
                 NULL);

    glBindTexture(GL_TEXTURE_2D, 0);
  }

  glBindFramebuffer(GL_READ_FRAMEBUFFER, render_fb);
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, m_fbo);

  glBlitFramebuffer(render_fb_x,                // srcX0
                    render_fb_y,                // srcY0
                    render_fb_x + render_fb_w,  // srcX1
                    render_fb_y + render_fb_h,  // srcY1
                    0,                          // dstX0
                    0,                          // dstY0
                    m_fbo_width,                // dstX1
                    m_fbo_height,               // dstY1
                    GL_COLOR_BUFFER_BIT,        // mask
                    GL_NEAREST                  // filter
  );

  glBindFramebuffer(GL_FRAMEBUFFER, render_fb);
}