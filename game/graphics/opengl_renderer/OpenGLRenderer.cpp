#include "OpenGLRenderer.h"

#include "common/log/log.h"
#include "game/graphics/pipelines/opengl.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"
#include "third-party/imgui/imgui.h"

// for the vif callback
#include "game/kernel/kmachine.h"

/*!
 * OpenGL Error callback. If we do something invalid, this will be called.
 */
void GLAPIENTRY opengl_error_callback(GLenum source,
                                      GLenum type,
                                      GLuint id,
                                      GLenum severity,
                                      GLsizei /*length*/,
                                      const GLchar* message,
                                      const void* /*userParam*/) {
  if (severity == GL_DEBUG_SEVERITY_NOTIFICATION) {
    return;
  } else if (severity == GL_DEBUG_SEVERITY_LOW) {
    lg::info("OpenGL message 0x{:X} S{:X} T{:X}: {}", id, source, type, message);
  } else if (severity == GL_DEBUG_SEVERITY_MEDIUM) {
    lg::warn("OpenGL warn 0x{:X} S{:X} T{:X}: {}", id, source, type, message);
  } else if (severity == GL_DEBUG_SEVERITY_HIGH) {
    lg::error("OpenGL error 0x{:X} S{:X} T{:X}: {}", id, source, type, message);
  }
}

OpenGLRenderer::OpenGLRenderer(std::shared_ptr<TexturePool> texture_pool)
    : m_render_state(texture_pool) {
  // setup OpenGL errors

  // disable specific errors
  const GLuint l_gl_error_ignores[1] = {
      0x64  // [API-PERFORMANCE] glDrawArrays uses non-native input attribute type
  };
  glEnable(GL_DEBUG_OUTPUT);
  glDebugMessageCallback(opengl_error_callback, nullptr);
  // filter
  glDebugMessageControl(GL_DEBUG_SOURCE_API, GL_DEBUG_TYPE_PERFORMANCE, GL_DONT_CARE, 1,
                        &l_gl_error_ignores[0], GL_FALSE);

  // initialize all renderers
  init_bucket_renderers();
}

/*!
 * Construct bucket renderers.  We can specify different renderers for different buckets
 */
void OpenGLRenderer::init_bucket_renderers() {
  // For example, set up bucket 0:
  init_bucket_renderer<EmptyBucketRenderer>("bucket0", BucketId::BUCKET0);

  // TODO what the heck is drawing to debug-draw-0 on init?
  init_bucket_renderer<DirectRenderer>("sprite", BucketId::SPRITE, 102);
  init_bucket_renderer<DirectRenderer>("debug-draw-0", BucketId::DEBUG_DRAW_0, 102);
  init_bucket_renderer<DirectRenderer>("debug-draw-1", BucketId::DEBUG_DRAW_1, 102);

  // for now, for any unset renderers, just set them to an EmptyBucketRenderer.
  for (size_t i = 0; i < m_bucket_renderers.size(); i++) {
    if (!m_bucket_renderers[i]) {
      init_bucket_renderer<EmptyBucketRenderer>(fmt::format("bucket{}", i), (BucketId)i);
    }
  }
}

/*!
 * Main render function. This is called from the gfx loop with the chain passed from the game.
 */
void OpenGLRenderer::render(DmaFollower dma, int window_width_px, int window_height_px) {
  setup_frame(window_width_px, window_height_px);
  // draw_test_triangle();
  // render the buckets!
  dispatch_buckets(dma);

  draw_renderer_selection_window();
  // add a profile bar for the imgui stuff
  vif_interrupt_callback();
}

void OpenGLRenderer::draw_renderer_selection_window() {
  ImGui::Begin("Renderer Debug");
  for (size_t i = 0; i < m_bucket_renderers.size(); i++) {
    auto renderer = m_bucket_renderers[i].get();
    if (renderer && !renderer->empty()) {
      ImGui::PushID(i);
      if (ImGui::CollapsingHeader(renderer->name_and_id().c_str())) {
        ImGui::Checkbox("Enable", &renderer->enabled());
        renderer->draw_debug_window();
      }
      ImGui::PopID();
    }
  }
  if (ImGui::CollapsingHeader("Texture Pool")) {
    m_render_state.texture_pool->draw_debug_window();
  }
  ImGui::End();
}

/*!
 * Pre-render frame setup.
 */
void OpenGLRenderer::setup_frame(int window_width_px, int window_height_px) {
  glViewport(0, 0, window_width_px, window_height_px);
  glClearColor(0.5, 0.5, 0.5, 0.0);
  glClear(GL_COLOR_BUFFER_BIT);
  glDisable(GL_BLEND);
}

/*!
 * This function finds buckets and dispatches them to the appropriate part.
 */
void OpenGLRenderer::dispatch_buckets(DmaFollower dma) {
  // The first thing the DMA chain should be a call to a common default-registers chain.
  // this chain resets the state of the GS. After this is buckets

  m_render_state.buckets_base =
      dma.current_tag_offset() + 16;  // offset by 1 qw for the initial call
  m_render_state.next_bucket = m_render_state.buckets_base;

  // Find the default regs buffer
  auto initial_call_tag = dma.current_tag();
  assert(initial_call_tag.kind == DmaTag::Kind::CALL);
  auto initial_call_default_regs = dma.read_and_advance();
  assert(initial_call_default_regs.transferred_tag == 0);  // should be a nop.
  m_render_state.default_regs_buffer = dma.current_tag_offset();
  auto default_regs_tag = dma.current_tag();
  assert(default_regs_tag.kind == DmaTag::Kind::CNT);
  assert(default_regs_tag.qwc == 10);
  // TODO verify data in here.
  dma.read_and_advance();
  auto default_ret_tag = dma.current_tag();
  assert(default_ret_tag.qwc == 0);
  assert(default_ret_tag.kind == DmaTag::Kind::RET);
  dma.read_and_advance();

  // now we should point to the first bucket!
  assert(dma.current_tag_offset() == m_render_state.next_bucket);
  m_render_state.next_bucket += 16;

  // loop over the buckets!
  for (int bucket_id = 0; bucket_id < (int)BucketId::MAX_BUCKETS; bucket_id++) {
    auto& renderer = m_bucket_renderers[bucket_id];
    //    fmt::print("render bucket {} with {}\n", bucket_id, renderer->name_and_id());
    renderer->render(dma, &m_render_state);
    // should have ended at the start of the next chain
    assert(dma.current_tag_offset() == m_render_state.next_bucket);
    m_render_state.next_bucket += 16;
    vif_interrupt_callback();
  }

  // TODO ending data.
}

void OpenGLRenderer::draw_test_triangle() {
  // just remembering how to use opengl here.

  //////////
  // Setup
  //////////

  // create "buffer object names"
  GLuint vertex_buffer, color_buffer, vao;
  glGenBuffers(1, &vertex_buffer);
  glGenBuffers(1, &color_buffer);

  glGenVertexArrays(1, &vao);
  glBindVertexArray(vao);

  // set vertex data
  glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
  const float verts[9] = {0.0, 0.8, 0, -0.5, -0.5 * .866, 0, 0.5, -0.5 * .866, 0};
  glBufferData(GL_ARRAY_BUFFER, 9 * sizeof(float), verts, GL_STATIC_DRAW);

  // set color data
  glBindBuffer(GL_ARRAY_BUFFER, color_buffer);
  const float colors[12] = {1., 0, 0., 1., 0., 1., 0., 1., 0., 0., 1., 1.};
  glBufferData(GL_ARRAY_BUFFER, 12 * sizeof(float), colors, GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);

  //////////
  // Draw!
  //////////
  m_render_state.shaders[ShaderId::TEST_SHADER].activate();

  // location 0: the vertices
  glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0,         // location 0 in the shader
                        3,         // 3 floats per vert
                        GL_FLOAT,  // floats
                        GL_FALSE,  // normalized, ignored,
                        0,         // tightly packed
                        0          // offset in array (why is is this a pointer...)
  );

  glBindBuffer(GL_ARRAY_BUFFER, color_buffer);
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, 0, (void*)0);

  glDrawArrays(GL_TRIANGLES, 0, 3);
  glBindVertexArray(0);

  ////////////
  // Clean Up
  ////////////
  // delete buffer
  glDeleteBuffers(1, &color_buffer);
  glDeleteBuffers(1, &vertex_buffer);
  glDeleteVertexArrays(1, &vao);
}
