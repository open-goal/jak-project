#pragma once

#include "common/math/Vector.h"

#include "game/graphics/pipelines/opengl.h"

struct SharedRenderState;
class ScopedProfilerNode;

/*!
 * This is a wrapper around a framebuffer and texture to make it easier to render to a texture.
 */
class FramebufferTexturePair {
 public:
  FramebufferTexturePair(int w, int h, u64 texture_format, int num_levels = 1);
  ~FramebufferTexturePair();

  GLuint texture() const { return m_texture; }

  void update_texture_size(int w, int h) {
    m_w = w;
    m_h = h;
  }

  void update_texture_unsafe(GLuint texture) { m_texture = texture; }

  FramebufferTexturePair(const FramebufferTexturePair&) = delete;
  FramebufferTexturePair& operator=(const FramebufferTexturePair&) = delete;
  FramebufferTexturePair(FramebufferTexturePair&& other) {
    if (this == &other) {
      return;
    }
    ASSERT(!m_moved_from && !other.m_moved_from);
    other.m_moved_from = true;
    m_w = other.m_w;
    m_h = other.m_h;
    m_texture = other.m_texture;
    m_framebuffers = std::move(other.m_framebuffers);
  }
  int width() const { return m_w; }
  int height() const { return m_h; }

 private:
  friend class FramebufferTexturePairContext;
  std::vector<GLuint> m_framebuffers;
  GLuint m_texture;
  int m_w, m_h;
  bool m_moved_from = false;
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

// draw over the full screen.
// you must set alpha/ztest/etc.
class FullScreenDraw {
 public:
  FullScreenDraw();
  ~FullScreenDraw();
  FullScreenDraw(const FullScreenDraw&) = delete;
  FullScreenDraw& operator=(const FullScreenDraw&) = delete;
  void draw(const math::Vector4f& color, SharedRenderState* render_state, ScopedProfilerNode& prof);

 private:
  GLuint m_vao;
  GLuint m_vertex_buffer;
};

class FramebufferCopier {
 public:
  FramebufferCopier();
  ~FramebufferCopier();
  FramebufferCopier(const FramebufferCopier&) = delete;
  FramebufferCopier& operator=(const FramebufferCopier&) = delete;
  void copy_now(int render_fb_w, int render_fb_h, GLuint render_fb);
  void copy_back_now(int render_fb_w, int render_fb_h, GLuint render_fb);
  u64 texture() const { return m_fbo_texture; }
  int width() const { return m_fbo_width; }
  int height() const { return m_fbo_height; }

 private:
  GLuint m_fbo = 0, m_fbo_texture = 0;
  int m_fbo_width = 640, m_fbo_height = 480;
};