#pragma once

#include "game/graphics/dma/dma_chain_read.h"
#include "game/graphics/opengl_renderer/Shader.h"

class OpenGLRenderer {
 public:
  OpenGLRenderer();
  void render(DmaFollower dma, int window_width, int window_height);

 private:
  void draw_test_triangle();

  ShaderLibrary m_shaders;
};

