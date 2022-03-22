#pragma once

#include "game/graphics/pipelines/opengl.h"

/*!
 * This is a wrapper around a framebuffer and texture to make it easier to render to a texture.
 */
class FramebufferTexturePair {
 public:
  FramebufferTexturePair(int w, int h, u64 texture_format, int num_levels = 1);
  ~FramebufferTexturePair();

  GLuint texture() const { return m_texture; }

  FramebufferTexturePair(const FramebufferTexturePair&) = delete;
  FramebufferTexturePair& operator=(const FramebufferTexturePair&) = delete;

 private:
  friend class FramebufferTexturePairContext;
  std::vector<GLuint> m_framebuffers;
  GLuint m_texture;
  int m_w, m_h;
};

class FramebufferTexturePairContext {
 public:
  FramebufferTexturePairContext(FramebufferTexturePair& fb, int level = 0);
  ~FramebufferTexturePairContext();

  void switch_to(FramebufferTexturePair& fb);

  FramebufferTexturePairContext(const FramebufferTexturePairContext&) = delete;
  FramebufferTexturePairContext& operator=(const FramebufferTexturePairContext&) = delete;

 private:
  FramebufferTexturePair* m_fb;
  GLint m_old_viewport[4];
  GLint m_old_framebuffer;
};