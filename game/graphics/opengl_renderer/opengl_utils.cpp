#include "opengl_utils.h"

#include "common/util/Assert.h"
#include <cstdio>

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