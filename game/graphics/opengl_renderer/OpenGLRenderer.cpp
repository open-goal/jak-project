#include "OpenGLRenderer.h"

#include "common/log/log.h"
#include "game/graphics/opengl.h"

void GLAPIENTRY opengl_error_callback(GLenum /*source*/,
                                      GLenum /*type*/,
                                      GLuint /*id*/,
                                      GLenum severity,
                                      GLsizei /*length*/,
                                      const GLchar* message,
                                      const void* /*userParam*/) {
  if (severity == GL_DEBUG_SEVERITY_NOTIFICATION) {
    return;
  }
  lg::error("OpenGL error: {}", message);
}

OpenGLRenderer::OpenGLRenderer() {
  // setup OpenGL errors
  glEnable(GL_DEBUG_OUTPUT);
  glDebugMessageCallback(opengl_error_callback, nullptr);
}

void OpenGLRenderer::render(DmaFollower dma, int window_width, int window_height) {
  (void)dma;
  // glClearColor(0.5, 0.5, 0.5, 1.0);
  glViewport(0, 0, window_width, window_height);
  glClear(GL_COLOR_BUFFER_BIT);

  draw_test_triangle();
}

void OpenGLRenderer::draw_test_triangle() {
  // just remembering how to use opengl here.

  //////////
  // Setup
  //////////

  // create "buffer object names"
  GLuint vertex_buffer, color_buffer;
  glGenBuffers(1, &vertex_buffer);
  glGenBuffers(1, &color_buffer);

  // set vertex data
  glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
  const float verts[9] = {0.0, 0.5, 0, -0.5, -0.5 * .866, 0, 0.5, -0.5 * .866, 0};
  glBufferData(GL_ARRAY_BUFFER, 9 * sizeof(float), verts, GL_STATIC_DRAW);

  // set color data
  glBindBuffer(GL_ARRAY_BUFFER, color_buffer);
  const float colors[12] = {1., 0, 0., 1., 0., 1., 0., 1., 0., 0., 1., 1.};
  glBufferData(GL_ARRAY_BUFFER, 12 * sizeof(float), colors, GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);

  //////////
  // Draw!
  //////////
  m_shaders[ShaderId::TEST_SHADER].activate();

  // location 0: the vertices
  glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(
      0,  // attribute 0. No particular reason for 0, but must match the layout in the shader.
      3,  // size
      GL_FLOAT,  // type
      GL_FALSE,  // normalized?
      0,         // stride
      (void*)0   // array buffer offset
  );

  glBindBuffer(GL_ARRAY_BUFFER, color_buffer);
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(
      1,  // attribute 0. No particular reason for 0, but must match the layout in the shader.
      4,  // size
      GL_FLOAT,  // type
      GL_FALSE,  // normalized?
      0,         // stride
      (void*)0   // array buffer offset
  );

  glDrawArrays(GL_TRIANGLES, 0, 3);

  ////////////
  // Clean Up
  ////////////
  // delete buffer
  glDeleteBuffers(1, &color_buffer);
  glDeleteBuffers(1, &vertex_buffer);
}