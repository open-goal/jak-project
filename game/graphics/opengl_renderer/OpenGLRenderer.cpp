#include "OpenGLRenderer.h"

#include "common/log/log.h"
#include "game/graphics/pipelines/opengl.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"
#include "game/graphics/opengl_renderer/SpriteRenderer.h"
#include "game/graphics/opengl_renderer/TextureUploadHandler.h"
#include "third-party/imgui/imgui.h"
#include "common/util/FileUtil.h"
#include "game/graphics/opengl_renderer/SkyRenderer.h"
#include "game/graphics/opengl_renderer/tfrag/TFragment.h"
#include "game/graphics/opengl_renderer/tfrag/Tie3.h"

// for the vif callback
#include "game/kernel/kmachine.h"
namespace {
std::string g_current_render;

}

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
    lg::debug("OpenGL notification 0x{:X} S{:X} T{:X}: {}", id, source, type, message);
  } else if (severity == GL_DEBUG_SEVERITY_LOW) {
    lg::info("[{}] OpenGL message 0x{:X} S{:X} T{:X}: {}", g_current_render, id, source, type,
             message);
  } else if (severity == GL_DEBUG_SEVERITY_MEDIUM) {
    lg::warn("[{}] OpenGL warn 0x{:X} S{:X} T{:X}: {}", g_current_render, id, source, type,
             message);
  } else if (severity == GL_DEBUG_SEVERITY_HIGH) {
    lg::error("[{}] OpenGL error 0x{:X} S{:X} T{:X}: {}", g_current_render, id, source, type,
              message);
  }
}

OpenGLRenderer::OpenGLRenderer(std::shared_ptr<TexturePool> texture_pool)
    : m_render_state(texture_pool) {
  // setup OpenGL errors

  glEnable(GL_DEBUG_OUTPUT);
  glDebugMessageCallback(opengl_error_callback, nullptr);
  // disable specific errors
  // const GLuint gl_error_ignores_api_perf[1] = {};
  // glDebugMessageControl(GL_DEBUG_SOURCE_API, GL_DEBUG_TYPE_PERFORMANCE, GL_DONT_CARE, 0,
  // &gl_error_ignores_api_perf[0], GL_FALSE);

  lg::debug("OpenGL context information: {}", (const char*)glGetString(GL_VERSION));

  // initialize all renderers
  init_bucket_renderers();
}

/*!
 * Construct bucket renderers.  We can specify different renderers for different buckets
 */
void OpenGLRenderer::init_bucket_renderers() {
  std::vector<tfrag3::TFragmentTreeKind> normal_tfrags = {tfrag3::TFragmentTreeKind::NORMAL,
                                                          tfrag3::TFragmentTreeKind::LOWRES};
  std::vector<tfrag3::TFragmentTreeKind> dirt_tfrags = {tfrag3::TFragmentTreeKind::DIRT};
  std::vector<tfrag3::TFragmentTreeKind> ice_tfrags = {tfrag3::TFragmentTreeKind::ICE};

  init_bucket_renderer<EmptyBucketRenderer>("bucket0", BucketId::BUCKET0);
  init_bucket_renderer<SkyRenderer>("sky", BucketId::SKY_DRAW);

  init_bucket_renderer<TextureUploadHandler>("tfrag-tex-0", BucketId::TFRAG_TEX_LEVEL0);
  init_bucket_renderer<TFragment>("tfrag-0", BucketId::TFRAG_LEVEL0, normal_tfrags, false);
  init_bucket_renderer<Tie3>("tie-0", BucketId::TIE_LEVEL0);
  init_bucket_renderer<TextureUploadHandler>("tfrag-tex-1", BucketId::TFRAG_TEX_LEVEL1);
  init_bucket_renderer<TFragment>("tfrag-1", BucketId::TFRAG_LEVEL1, normal_tfrags, false);
  init_bucket_renderer<Tie3>("tie-1", BucketId::TIE_LEVEL1);
  init_bucket_renderer<TextureUploadHandler>("shrub-tex-0", BucketId::SHRUB_TEX_LEVEL0);
  init_bucket_renderer<TextureUploadHandler>("shrub-tex-1", BucketId::SHRUB_TEX_LEVEL1);
  init_bucket_renderer<TextureUploadHandler>("alpha-tex-0", BucketId::ALPHA_TEX_LEVEL0);
  init_bucket_renderer<TextureUploadHandler>("alpha-tex-1", BucketId::ALPHA_TEX_LEVEL1);
  auto sky_gpu_blender = std::make_shared<SkyBlendGPU>();
  auto sky_cpu_blender = std::make_shared<SkyBlendCPU>();
  init_bucket_renderer<SkyBlendHandler>("sky-blend-and-tfrag-trans-0",
                                        BucketId::TFRAG_TRANS0_AND_SKY_BLEND_LEVEL0,
                                        sky_gpu_blender, sky_cpu_blender);
  init_bucket_renderer<TFragment>("tfrag-dirt-0", BucketId::TFRAG_DIRT_LEVEL0, dirt_tfrags, false);
  init_bucket_renderer<TFragment>("tfrag-ice-0", BucketId::TFRAG_ICE_LEVEL0, ice_tfrags, false);
  init_bucket_renderer<SkyBlendHandler>("sky-blend-and-tfrag-trans-1",
                                        BucketId::TFRAG_TRANS1_AND_SKY_BLEND_LEVEL1,
                                        sky_gpu_blender, sky_cpu_blender);
  init_bucket_renderer<TFragment>("tfrag-dirt-1", BucketId::TFRAG_DIRT_LEVEL1, dirt_tfrags, false);
  init_bucket_renderer<TFragment>("tfrag-ice-1", BucketId::TFRAG_ICE_LEVEL1, ice_tfrags, false);
  init_bucket_renderer<TextureUploadHandler>("pris-tex-0", BucketId::PRIS_TEX_LEVEL0);
  init_bucket_renderer<TextureUploadHandler>("pris-tex-1", BucketId::PRIS_TEX_LEVEL1);
  init_bucket_renderer<TextureUploadHandler>("water-tex-0", BucketId::WATER_TEX_LEVEL0);
  init_bucket_renderer<TextureUploadHandler>("water-tex-1", BucketId::WATER_TEX_LEVEL1);
  init_bucket_renderer<TextureUploadHandler>("pre-sprite-tex", BucketId::PRE_SPRITE_TEX);
  init_bucket_renderer<SpriteRenderer>("sprite", BucketId::SPRITE);
  init_bucket_renderer<DirectRenderer>("debug-draw-0", BucketId::DEBUG_DRAW_0, 0x8000,
                                       DirectRenderer::Mode::NORMAL);
  init_bucket_renderer<DirectRenderer>("debug-draw-1", BucketId::DEBUG_DRAW_1, 0x8000,
                                       DirectRenderer::Mode::NORMAL);

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
void OpenGLRenderer::render(DmaFollower dma, const RenderOptions& settings) {
  m_profiler.clear();
  m_render_state.dump_playback = settings.playing_from_dump;
  m_render_state.ee_main_memory = settings.playing_from_dump ? nullptr : g_ee_main_mem;
  m_render_state.offset_of_s7 = offset_of_s7();

  {
    auto prof = m_profiler.root()->make_scoped_child("frame-setup");
    setup_frame(settings.window_width_px, settings.window_height_px, settings.lbox_width_px,
                settings.lbox_height_px);
  }
  {
    auto prof = m_profiler.root()->make_scoped_child("texture-gc");
    m_render_state.texture_pool->remove_garbage_textures();
  }

  // draw_test_triangle();
  // render the buckets!
  {
    auto prof = m_profiler.root()->make_scoped_child("buckets");
    dispatch_buckets(dma, prof);
  }

  if (settings.draw_render_debug_window) {
    auto prof = m_profiler.root()->make_scoped_child("render-window");
    draw_renderer_selection_window();
    // add a profile bar for the imgui stuff
    if (!m_render_state.dump_playback) {
      vif_interrupt_callback();
    }
  }

  m_profiler.finish();
  if (settings.draw_profiler_window) {
    m_profiler.draw();
  }

  if (settings.save_screenshot) {
    finish_screenshot(settings.screenshot_path, settings.window_width_px, settings.window_height_px,
                      settings.lbox_width_px, settings.lbox_height_px);
  }
}

void OpenGLRenderer::serialize(Serializer& ser) {
  m_render_state.texture_pool->serialize(ser);
  for (auto& renderer : m_bucket_renderers) {
    renderer->serialize(ser);
  }
}

/*!
 * Draw the per-renderer debug window
 */
void OpenGLRenderer::draw_renderer_selection_window() {
  ImGui::Begin("Renderer Debug");

  ImGui::Checkbox("Sky CPU", &m_render_state.use_sky_cpu);

  for (size_t i = 0; i < m_bucket_renderers.size(); i++) {
    auto renderer = m_bucket_renderers[i].get();
    if (renderer && !renderer->empty()) {
      ImGui::PushID(i);
      if (ImGui::TreeNode(renderer->name_and_id().c_str())) {
        ImGui::Checkbox("Enable", &renderer->enabled());
        renderer->draw_debug_window();
        ImGui::TreePop();
      }
      ImGui::PopID();
    }
  }
  if (ImGui::TreeNode("Texture Pool")) {
    m_render_state.texture_pool->draw_debug_window();
    ImGui::TreePop();
  }
  ImGui::End();
}

/*!
 * Pre-render frame setup.
 */
void OpenGLRenderer::setup_frame(int window_width_px,
                                 int window_height_px,
                                 int offset_x,
                                 int offset_y) {
  glViewport(offset_x, offset_y, window_width_px, window_height_px);
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClearDepth(0.0);
  glDepthMask(GL_TRUE);
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glDisable(GL_BLEND);
}

/*!
 * This function finds buckets and dispatches them to the appropriate part.
 */
void OpenGLRenderer::dispatch_buckets(DmaFollower dma, ScopedProfilerNode& prof) {
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
    auto bucket_prof = prof.make_scoped_child(renderer->name_and_id());
    // lg::info("Render: {} start\n", renderer->name_and_id());
    g_current_render = renderer->name_and_id();
    renderer->render(dma, &m_render_state, bucket_prof);
    // lg::info("Render: {} end\n", renderer->name_and_id());
    //  should have ended at the start of the next chain
    assert(dma.current_tag_offset() == m_render_state.next_bucket);
    m_render_state.next_bucket += 16;

    if (!m_render_state.dump_playback) {
      vif_interrupt_callback();
    }
  }
  g_current_render = "";

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

/*!
 * Take a screenshot!
 */
void OpenGLRenderer::finish_screenshot(const std::string& output_name,
                                       int width,
                                       int height,
                                       int x,
                                       int y) {
  std::vector<u32> buffer(width * height);
  glPixelStorei(GL_PACK_ALIGNMENT, 1);
  glReadBuffer(GL_BACK);
  glReadPixels(x, y, width, height, GL_RGBA, GL_UNSIGNED_BYTE, buffer.data());
  // flip upside down in place
  for (int h = 0; h < height / 2; h++) {
    for (int w = 0; w < width; w++) {
      std::swap(buffer[h * width + w], buffer[(height - h - 1) * width + w]);
    }
  }

  // set alpha. For some reason, image viewers do weird stuff with alpha.
  for (auto& px : buffer) {
    px |= 0xff000000;
  }
  file_util::write_rgba_png(output_name, buffer.data(), width, height);
}
