#pragma once

#include <optional>

#include "game/graphics/pipelines/opengl.h"

struct Fbo {
  bool valid = false;  // do we have an OpenGL fbo_id?
  GLuint fbo_id = -1;

  // optional rgba/zbuffer/stencil data.
  std::optional<GLuint> tex_id;
  std::optional<GLuint> zbuf_stencil_id;

  bool multisampled = false;
  int multisample_count = 0;  // Should be 1 if multisampled is disabled

  bool is_window = false;
  int width = 640;
  int height = 480;

  // Does this fbo match the given format? MSAA = 1 will accept a normal buffer, or a multisample 1x
  bool matches(int w, int h, int msaa) const {
    int effective_msaa = multisampled ? multisample_count : 1;
    return valid && width == w && height == h && effective_msaa == msaa;
  }

  bool matches(const Fbo& other) const {
    return matches(other.width, other.height, other.multisample_count);
  }

  // Free opengl resources, if we have any.
  void clear() {
    if (valid) {
      glDeleteFramebuffers(1, &fbo_id);
      fbo_id = -1;

      if (tex_id) {
        glDeleteTextures(1, &tex_id.value());
        tex_id.reset();
      }

      if (zbuf_stencil_id) {
        glDeleteRenderbuffers(1, &zbuf_stencil_id.value());
        zbuf_stencil_id.reset();
      }

      valid = false;
    }
  }
};