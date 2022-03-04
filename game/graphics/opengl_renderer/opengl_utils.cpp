#include "opengl_utils.h"

#include "common/util/Assert.h"

FramebufferTexturePair::FramebufferTexturePair(int w, int h, u64 texture_format) : m_w(w), m_h(h) {
  glGenFramebuffers(1, &m_framebuffer);
  glGenTextures(1, &m_texture);

  GLint old_framebuffer;
  glGetIntegerv(GL_FRAMEBUFFER_BINDING, &old_framebuffer);

  glBindFramebuffer(GL_FRAMEBUFFER, m_framebuffer);
  glBindTexture(GL_TEXTURE_2D, m_texture);

  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_RGBA, texture_format, nullptr);

  // I don't know if we really need to do this. whatever uses this texture should figure it out.
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

  glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, m_texture, 0);
  GLenum draw_buffers[1] = {GL_COLOR_ATTACHMENT0};
  glDrawBuffers(1, draw_buffers);
  if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE) {
    ASSERT(false);
  }

  glBindFramebuffer(GL_FRAMEBUFFER, old_framebuffer);
}

FramebufferTexturePair::~FramebufferTexturePair() {
  glDeleteFramebuffers(1, &m_framebuffer);
  glDeleteTextures(1, &m_texture);
}

FramebufferTexturePairContext::FramebufferTexturePairContext(FramebufferTexturePair& fb)
    : m_fb(&fb) {
  glGetIntegerv(GL_VIEWPORT, m_old_viewport);
  glGetIntegerv(GL_FRAMEBUFFER_BINDING, &m_old_framebuffer);
  glBindFramebuffer(GL_FRAMEBUFFER, m_fb->m_framebuffer);
  glViewport(0, 0, m_fb->m_w, m_fb->m_h);
  glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, m_fb->m_texture, 0);
}

void FramebufferTexturePairContext::switch_to(FramebufferTexturePair& fb) {
  if (&fb != m_fb) {
    m_fb = &fb;
    glBindFramebuffer(GL_FRAMEBUFFER, m_fb->m_framebuffer);
    glViewport(0, 0, m_fb->m_w, m_fb->m_h);
    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, m_fb->m_texture, 0);
  }
}

FramebufferTexturePairContext::~FramebufferTexturePairContext() {
  glViewport(m_old_viewport[0], m_old_viewport[1], m_old_viewport[2], m_old_viewport[3]);
  glBindFramebuffer(GL_FRAMEBUFFER, m_old_framebuffer);
}