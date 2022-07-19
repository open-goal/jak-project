#include "OpenGLRenderer.h"

#include "common/log/log.h"
#include "common/util/FileUtil.h"

#include "game/graphics/opengl_renderer/DepthCue.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"
#include "game/graphics/opengl_renderer/EyeRenderer.h"
#include "game/graphics/opengl_renderer/ShadowRenderer.h"
#include "game/graphics/opengl_renderer/SkyRenderer.h"
#include "game/graphics/opengl_renderer/Sprite3.h"
#include "game/graphics/opengl_renderer/SpriteRenderer.h"
#include "game/graphics/opengl_renderer/TextureUploadHandler.h"
#include "game/graphics/opengl_renderer/background/Shrub.h"
#include "game/graphics/opengl_renderer/background/TFragment.h"
#include "game/graphics/opengl_renderer/background/Tie3.h"
#include "game/graphics/opengl_renderer/foreground/Generic2.h"
#include "game/graphics/opengl_renderer/foreground/Merc2.h"
#include "game/graphics/opengl_renderer/ocean/OceanMidAndFar.h"
#include "game/graphics/opengl_renderer/ocean/OceanNear.h"
#include "game/graphics/pipelines/opengl.h"

#include "third-party/imgui/imgui.h"

// for the vif callback
#include "game/kernel/common/kmachine.h"
#include "game/runtime.h"

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
    // ASSERT(false);
  }
}

OpenGLRenderer::OpenGLRenderer(std::shared_ptr<TexturePool> texture_pool,
                               std::shared_ptr<Loader> loader)
    : m_render_state(texture_pool, loader) {
  // setup OpenGL errors
  glEnable(GL_DEBUG_OUTPUT);
  glDebugMessageCallback(opengl_error_callback, nullptr);
  // disable specific errors
  const GLuint gl_error_ignores_api_other[1] = {0x20071};
  glDebugMessageControl(GL_DEBUG_SOURCE_API, GL_DEBUG_TYPE_OTHER, GL_DONT_CARE, 1,
                        &gl_error_ignores_api_other[0], GL_FALSE);

  lg::debug("OpenGL context information: {}", (const char*)glGetString(GL_VERSION));

  // initialize all renderers
  init_bucket_renderers();
}

/*!
 * Construct bucket renderers.  We can specify different renderers for different buckets
 */
void OpenGLRenderer::init_bucket_renderers() {
  m_bucket_categories.fill(BucketCategory::OTHER);
  std::vector<tfrag3::TFragmentTreeKind> normal_tfrags = {tfrag3::TFragmentTreeKind::NORMAL,
                                                          tfrag3::TFragmentTreeKind::LOWRES};
  std::vector<tfrag3::TFragmentTreeKind> dirt_tfrags = {tfrag3::TFragmentTreeKind::DIRT};
  std::vector<tfrag3::TFragmentTreeKind> ice_tfrags = {tfrag3::TFragmentTreeKind::ICE};
  auto sky_gpu_blender = std::make_shared<SkyBlendGPU>();
  auto sky_cpu_blender = std::make_shared<SkyBlendCPU>();

  //-------------
  // PRE TEXTURE
  //-------------
  // 0 : ??
  // 1 : ??
  // 2 : ??
  // 3 : SKY_DRAW
  init_bucket_renderer<SkyRenderer>("sky", BucketCategory::OTHER, BucketId::SKY_DRAW);
  // 4 : OCEAN_MID_AND_FAR
  init_bucket_renderer<OceanMidAndFar>("ocean-mid-far", BucketCategory::OCEAN,
                                       BucketId::OCEAN_MID_AND_FAR);

  //-----------------------
  // LEVEL 0 tfrag texture
  //-----------------------
  // 5 : TFRAG_TEX_LEVEL0
  init_bucket_renderer<TextureUploadHandler>("l0-tfrag-tex", BucketCategory::TEX,
                                             BucketId::TFRAG_TEX_LEVEL0);
  // 6 : TFRAG_LEVEL0
  init_bucket_renderer<TFragment>("l0-tfrag-tfrag", BucketCategory::TFRAG, BucketId::TFRAG_LEVEL0,
                                  normal_tfrags, false, 0);
  // 7 : TFRAG_NEAR_LEVEL0
  // 8 : TIE_NEAR_LEVEL0
  // 9 : TIE_LEVEL0
  init_bucket_renderer<Tie3>("l0-tfrag-tie", BucketCategory::TIE, BucketId::TIE_LEVEL0, 0);
  // 10 : MERC_TFRAG_TEX_LEVEL0
  init_bucket_renderer<Merc2>("l0-tfrag-merc", BucketCategory::MERC,
                              BucketId::MERC_TFRAG_TEX_LEVEL0);
  // 11 : GMERC_TFRAG_TEX_LEVEL0
  init_bucket_renderer<Generic2>("l0-tfrag-generic", BucketCategory::GENERIC,
                                 BucketId::GENERIC_TFRAG_TEX_LEVEL0, 1500000, 10000, 10000, 800);

  //-----------------------
  // LEVEL 1 tfrag texture
  //-----------------------
  // 12 : TFRAG_TEX_LEVEL1
  init_bucket_renderer<TextureUploadHandler>("l1-tfrag-tex", BucketCategory::TEX,
                                             BucketId::TFRAG_TEX_LEVEL1);
  // 13 : TFRAG_LEVEL1
  init_bucket_renderer<TFragment>("l1-tfrag-tfrag", BucketCategory::TFRAG, BucketId::TFRAG_LEVEL1,
                                  normal_tfrags, false, 1);
  // 14 : TFRAG_NEAR_LEVEL1
  // 15 : TIE_NEAR_LEVEL1
  // 16 : TIE_LEVEL1
  init_bucket_renderer<Tie3>("l1-tfrag-tie", BucketCategory::TIE, BucketId::TIE_LEVEL1, 1);
  // 17 : MERC_TFRAG_TEX_LEVEL1
  init_bucket_renderer<Merc2>("l1-tfrag-merc", BucketCategory::MERC,
                              BucketId::MERC_TFRAG_TEX_LEVEL1);
  // 18 : GMERC_TFRAG_TEX_LEVEL1
  init_bucket_renderer<Generic2>("l1-tfrag-generic", BucketCategory::GENERIC,
                                 BucketId::GENERIC_TFRAG_TEX_LEVEL1, 1500000, 10000, 10000, 800);

  //-----------------------
  // LEVEL 0 shrub texture
  //-----------------------
  // 19 : SHRUB_TEX_LEVEL0
  init_bucket_renderer<TextureUploadHandler>("l0-shrub-tex", BucketCategory::TEX,
                                             BucketId::SHRUB_TEX_LEVEL0);
  // 20 : SHRUB_NORMAL_LEVEL0
  init_bucket_renderer<Shrub>("l0-shrub", BucketCategory::SHRUB, BucketId::SHRUB_NORMAL_LEVEL0);
  // 21 : ???
  // 22 : SHRUB_BILLBOARD_LEVEL0
  // 23 : SHRUB_TRANS_LEVEL0
  // 24 : SHRUB_GENERIC_LEVEL0

  //-----------------------
  // LEVEL 1 shrub texture
  //-----------------------
  // 25 : SHRUB_TEX_LEVEL1
  init_bucket_renderer<TextureUploadHandler>("l1-shrub-tex", BucketCategory::TEX,
                                             BucketId::SHRUB_TEX_LEVEL1);
  // 26 : SHRUB_NORMAL_LEVEL1
  init_bucket_renderer<Shrub>("l1-shrub", BucketCategory::SHRUB, BucketId::SHRUB_NORMAL_LEVEL1);
  // 27 : ???
  // 28 : SHRUB_BILLBOARD_LEVEL1
  // 29 : SHRUB_TRANS_LEVEL1
  // 30 : SHRUB_GENERIC_LEVEL1
  init_bucket_renderer<Generic2>("mystery-generic", BucketCategory::GENERIC,
                                 BucketId::SHRUB_GENERIC_LEVEL1);

  //-----------------------
  // LEVEL 0 alpha texture
  //-----------------------
  init_bucket_renderer<TextureUploadHandler>("l0-alpha-tex", BucketCategory::TEX,
                                             BucketId::ALPHA_TEX_LEVEL0);  // 31
  init_bucket_renderer<SkyBlendHandler>("l0-alpha-sky-blend-and-tfrag-trans", BucketCategory::OTHER,
                                        BucketId::TFRAG_TRANS0_AND_SKY_BLEND_LEVEL0, 0,
                                        sky_gpu_blender, sky_cpu_blender);  // 32
  // 33
  init_bucket_renderer<TFragment>("l0-alpha-tfrag", BucketCategory::TFRAG,
                                  BucketId::TFRAG_DIRT_LEVEL0, dirt_tfrags, false,
                                  0);  // 34
  // 35
  init_bucket_renderer<TFragment>("l0-alpha-tfrag-ice", BucketCategory::TFRAG,
                                  BucketId::TFRAG_ICE_LEVEL0, ice_tfrags, false, 0);
  // 37

  //-----------------------
  // LEVEL 1 alpha texture
  //-----------------------
  init_bucket_renderer<TextureUploadHandler>("l1-alpha-tex", BucketCategory::TEX,
                                             BucketId::ALPHA_TEX_LEVEL1);  // 38
  init_bucket_renderer<SkyBlendHandler>("l1-alpha-sky-blend-and-tfrag-trans", BucketCategory::OTHER,
                                        BucketId::TFRAG_TRANS1_AND_SKY_BLEND_LEVEL1, 1,
                                        sky_gpu_blender, sky_cpu_blender);  // 39
  // 40
  init_bucket_renderer<TFragment>("l1-alpha-tfrag-dirt", BucketCategory::TFRAG,
                                  BucketId::TFRAG_DIRT_LEVEL1, dirt_tfrags, false,
                                  1);  // 41
  // 42
  init_bucket_renderer<TFragment>("l1-alpha-tfrag-ice", BucketCategory::TFRAG,
                                  BucketId::TFRAG_ICE_LEVEL1, ice_tfrags, false, 1);
  // 44

  init_bucket_renderer<Merc2>("common-alpha-merc", BucketCategory::MERC,
                              BucketId::MERC_AFTER_ALPHA);

  init_bucket_renderer<Generic2>("common-alpha-generic", BucketCategory::GENERIC,
                                 BucketId::GENERIC_ALPHA);                                  // 46
  init_bucket_renderer<ShadowRenderer>("shadow", BucketCategory::OTHER, BucketId::SHADOW);  // 47

  //-----------------------
  // LEVEL 0 pris texture
  //-----------------------
  init_bucket_renderer<TextureUploadHandler>("l0-pris-tex", BucketCategory::TEX,
                                             BucketId::PRIS_TEX_LEVEL0);  // 48
  init_bucket_renderer<Merc2>("l0-pris-merc", BucketCategory::MERC,
                              BucketId::MERC_PRIS_LEVEL0);  // 49
  init_bucket_renderer<Generic2>("l0-pris-generic", BucketCategory::GENERIC,
                                 BucketId::GENERIC_PRIS_LEVEL0);  // 50

  //-----------------------
  // LEVEL 1 pris texture
  //-----------------------
  init_bucket_renderer<TextureUploadHandler>("l1-pris-tex", BucketCategory::TEX,
                                             BucketId::PRIS_TEX_LEVEL1);  // 51
  init_bucket_renderer<Merc2>("l1-pris-merc", BucketCategory::MERC,
                              BucketId::MERC_PRIS_LEVEL1);  // 52
  init_bucket_renderer<Generic2>("l1-pris-generic", BucketCategory::GENERIC,
                                 BucketId::GENERIC_PRIS_LEVEL1);  // 53

  // other renderers may output to the eye renderer
  m_render_state.eye_renderer = init_bucket_renderer<EyeRenderer>(
      "common-pris-eyes", BucketCategory::OTHER, BucketId::MERC_EYES_AFTER_PRIS);  // 54

  // hack: set to merc2 for debugging
  init_bucket_renderer<Merc2>("common-pris-merc", BucketCategory::MERC,
                              BucketId::MERC_AFTER_PRIS);  // 55
  init_bucket_renderer<Generic2>("common-pris-generic", BucketCategory::GENERIC,
                                 BucketId::GENERIC_PRIS);  // 56

  //-----------------------
  // LEVEL 0 water texture
  //-----------------------
  init_bucket_renderer<TextureUploadHandler>("l0-water-tex", BucketCategory::TEX,
                                             BucketId::WATER_TEX_LEVEL0);  // 57
  init_bucket_renderer<Merc2>("l0-water-merc", BucketCategory::MERC,
                              BucketId::MERC_WATER_LEVEL0);  // 58
  init_bucket_renderer<Generic2>("l0-water-generic", BucketCategory::GENERIC,
                                 BucketId::GENERIC_WATER_LEVEL0);  // 59

  //-----------------------
  // LEVEL 1 water texture
  //-----------------------
  init_bucket_renderer<TextureUploadHandler>("l1-water-tex", BucketCategory::TEX,
                                             BucketId::WATER_TEX_LEVEL1);  // 60
  init_bucket_renderer<Merc2>("l1-water-merc", BucketCategory::MERC,
                              BucketId::MERC_WATER_LEVEL1);  // 61
  init_bucket_renderer<Generic2>("l1-water-generic", BucketCategory::GENERIC,
                                 BucketId::GENERIC_WATER_LEVEL1);  // 62

  init_bucket_renderer<OceanNear>("ocean-near", BucketCategory::OCEAN, BucketId::OCEAN_NEAR);  // 63

  //-----------------------
  // DEPTH CUE
  //-----------------------
  init_bucket_renderer<DepthCue>("depth-cue", BucketCategory::OTHER, BucketId::DEPTH_CUE);  // 64

  //-----------------------
  // COMMON texture
  //-----------------------
  init_bucket_renderer<TextureUploadHandler>("common-tex", BucketCategory::TEX,
                                             BucketId::PRE_SPRITE_TEX);  // 65

  std::vector<std::unique_ptr<BucketRenderer>> sprite_renderers;
  // the first renderer added will be the default for sprite.
  sprite_renderers.push_back(std::make_unique<Sprite3>("sprite-3", BucketId::SPRITE));
  sprite_renderers.push_back(std::make_unique<SpriteRenderer>("sprite-renderer", BucketId::SPRITE));
  init_bucket_renderer<RenderMux>("sprite", BucketCategory::SPRITE, BucketId::SPRITE,
                                  std::move(sprite_renderers));  // 66

  init_bucket_renderer<DirectRenderer>("debug", BucketCategory::OTHER, BucketId::DEBUG, 0x20000);
  init_bucket_renderer<DirectRenderer>("debug-no-zbuf", BucketCategory::OTHER,
                                       BucketId::DEBUG_NO_ZBUF, 0x8000);
  init_bucket_renderer<DirectRenderer>("subtitle", BucketCategory::OTHER, BucketId::SUBTITLE, 2000);

  // for now, for any unset renderers, just set them to an EmptyBucketRenderer.
  for (size_t i = 0; i < m_bucket_renderers.size(); i++) {
    if (!m_bucket_renderers[i]) {
      init_bucket_renderer<EmptyBucketRenderer>(fmt::format("bucket{}", i), BucketCategory::OTHER,
                                                (BucketId)i);
    }

    m_bucket_renderers[i]->init_shaders(m_render_state.shaders);
    m_bucket_renderers[i]->init_textures(*m_render_state.texture_pool);
  }
  sky_cpu_blender->init_textures(*m_render_state.texture_pool);
  sky_gpu_blender->init_textures(*m_render_state.texture_pool);
  m_render_state.loader->load_common(*m_render_state.texture_pool, "GAME");
}

/*!
 * Main render function. This is called from the gfx loop with the chain passed from the game.
 */
void OpenGLRenderer::render(DmaFollower dma, const RenderOptions& settings) {
  m_profiler.clear();
  m_render_state.reset();
  m_render_state.ee_main_memory = g_ee_main_mem;
  m_render_state.offset_of_s7 = offset_of_s7();

  {
    auto prof = m_profiler.root()->make_scoped_child("frame-setup");
    setup_frame(settings);
    if (settings.gpu_sync) {
      glFinish();
    }
  }

  {
    auto prof = m_profiler.root()->make_scoped_child("loader");
    if (m_last_pmode_alp == 0 && settings.pmode_alp_register != 0 && m_enable_fast_blackout_loads) {
      // blackout, load everything and don't worry about frame rate
      m_render_state.loader->update_blocking(*m_render_state.texture_pool);

    } else {
      m_render_state.loader->update(*m_render_state.texture_pool);
    }
  }

  // render the buckets!
  {
    auto prof = m_profiler.root()->make_scoped_child("buckets");
    dispatch_buckets(dma, prof, settings.gpu_sync);
  }

  // apply effects done with PCRTC registers
  {
    auto prof = m_profiler.root()->make_scoped_child("pcrtc");
    do_pcrtc_effects(settings.pmode_alp_register, &m_render_state, prof);
    if (settings.gpu_sync) {
      glFinish();
    }
  }

  if (settings.draw_render_debug_window) {
    auto prof = m_profiler.root()->make_scoped_child("render-window");
    draw_renderer_selection_window();
    // add a profile bar for the imgui stuff
    vif_interrupt_callback();
    if (settings.gpu_sync) {
      glFinish();
    }
  }

  m_last_pmode_alp = settings.pmode_alp_register;

  m_profiler.finish();
  if (settings.draw_profiler_window) {
    m_profiler.draw();
  }

  //  if (m_profiler.root_time() > 0.018) {
  //    fmt::print("Slow frame: {:.2f} ms\n", m_profiler.root_time() * 1000);
  //    fmt::print("{}\n", m_profiler.to_string());
  //  }

  if (settings.draw_small_profiler_window) {
    SmallProfilerStats stats;
    stats.draw_calls = m_profiler.root()->stats().draw_calls;
    stats.triangles = m_profiler.root()->stats().triangles;
    for (int i = 0; i < (int)BucketCategory::MAX_CATEGORIES; i++) {
      stats.time_per_category[i] = m_category_times[i];
    }
    m_small_profiler.draw(m_render_state.load_status_debug, stats);
  }

  if (settings.draw_subtitle_editor_window) {
    m_subtitle_editor.draw_window();
  }

  if (settings.save_screenshot) {
    Fbo* screenshot_src;

    // can't screenshot from a multisampled buffer directly -
    if (m_fbo_state.resources.resolve_buffer.valid) {
      screenshot_src = &m_fbo_state.resources.resolve_buffer;
    } else {
      screenshot_src = m_fbo_state.render_fbo;
    }
    finish_screenshot(settings.screenshot_path, screenshot_src->width, screenshot_src->height, 0, 0,
                      screenshot_src->fbo_id);
  }
  if (settings.gpu_sync) {
    glFinish();
  }
}

/*!
 * Draw the per-renderer debug window
 */
void OpenGLRenderer::draw_renderer_selection_window() {
  ImGui::Begin("Renderer Debug");

  ImGui::Checkbox("Use old single-draw", &m_render_state.no_multidraw);
  ImGui::SliderFloat("Fog Adjust", &m_render_state.fog_intensity, 0, 10);
  ImGui::Checkbox("Sky CPU", &m_render_state.use_sky_cpu);
  ImGui::Checkbox("Occlusion Cull", &m_render_state.use_occlusion_culling);
  ImGui::Checkbox("Merc XGKICK", &m_render_state.enable_merc_xgkick);
  ImGui::Checkbox("Blackout Loads", &m_enable_fast_blackout_loads);

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

namespace {
Fbo make_fbo(int w, int h, int msaa, bool make_zbuf_and_stencil) {
  Fbo result;
  bool use_multisample = msaa > 1;

  // make framebuffer object
  glGenFramebuffers(1, &result.fbo_id);
  glBindFramebuffer(GL_FRAMEBUFFER, result.fbo_id);
  result.valid = true;

  // make texture that will hold the colors of the framebuffer
  GLuint tex;
  glGenTextures(1, &tex);
  result.tex_id = tex;
  glActiveTexture(GL_TEXTURE0);
  if (use_multisample) {
    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, tex);
    glTexImage2DMultisample(GL_TEXTURE_2D_MULTISAMPLE, msaa, GL_RGBA8, w, h, GL_TRUE);
  } else {
    glBindTexture(GL_TEXTURE_2D, tex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, nullptr);
  }
  // make depth and stencil buffers that will hold the... depth and stencil buffers
  if (make_zbuf_and_stencil) {
    GLuint zbuf;
    glGenRenderbuffers(1, &zbuf);
    result.zbuf_stencil_id = zbuf;
    glBindRenderbuffer(GL_RENDERBUFFER, zbuf);
    if (use_multisample) {
      glRenderbufferStorageMultisample(GL_RENDERBUFFER, msaa, GL_DEPTH24_STENCIL8, w, h);
    } else {
      glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH24_STENCIL8, w, h);
    }
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_RENDERBUFFER, zbuf);
  }
  // attach texture to framebuffer as target for colors

  if (use_multisample) {
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D_MULTISAMPLE, tex, 0);
  } else {
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, tex, 0);
  }

  GLenum render_targets[1] = {GL_COLOR_ATTACHMENT0};
  glDrawBuffers(1, render_targets);
  auto status = glCheckFramebufferStatus(GL_FRAMEBUFFER);

  if (status != GL_FRAMEBUFFER_COMPLETE) {
    lg::error("Failed to setup framebuffer: {} {} {} {}\n", w, h, msaa, make_zbuf_and_stencil);
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

  result.multisample_count = msaa;
  result.multisampled = use_multisample;
  result.is_window = false;
  result.width = w;
  result.height = h;

  return result;
}
}  // namespace

/*!
 * Pre-render frame setup.
 */
void OpenGLRenderer::setup_frame(const RenderOptions& settings) {
  // glfw controls the window framebuffer, so we just update the size:
  auto& window_fb = m_fbo_state.resources.window;
  bool window_resized = window_fb.width != settings.window_framebuffer_width ||
                        window_fb.height != settings.window_framebuffer_height;
  window_fb.valid = true;
  window_fb.is_window = true;
  window_fb.fbo_id = 0;
  window_fb.width = settings.window_framebuffer_width;
  window_fb.height = settings.window_framebuffer_height;
  window_fb.multisample_count = 1;
  window_fb.multisampled = false;

  // see if the render FBO is still applicable
  if (!m_fbo_state.render_fbo || window_resized ||
      !m_fbo_state.render_fbo->matches(settings.game_res_w, settings.game_res_h,
                                       settings.msaa_samples)) {
    // doesn't match, set up a new one for these settings
    lg::info("FBO Setup: requested {}x{}, msaa {}", settings.game_res_w, settings.game_res_h,
             settings.msaa_samples);

    // clear old framebuffers
    m_fbo_state.resources.render_buffer.clear();
    m_fbo_state.resources.resolve_buffer.clear();

    // first, see if we can just render straight to the display framebuffer.
    if (window_fb.matches(settings.game_res_w, settings.game_res_h, settings.msaa_samples)) {
      // it matches - no need for extra framebuffers.
      lg::info("FBO Setup: rendering directly to window framebuffer");
      m_fbo_state.render_fbo = &m_fbo_state.resources.window;
    } else {
      lg::info("FBO Setup: window didn't match: {} {}", window_fb.width, window_fb.height);

      // create a fbo to render to, with the desired settings
      m_fbo_state.resources.render_buffer =
          make_fbo(settings.game_res_w, settings.game_res_h, settings.msaa_samples, true);
      m_fbo_state.render_fbo = &m_fbo_state.resources.render_buffer;

      bool resolution_matches =
          window_fb.width == settings.game_res_w && window_fb.height == settings.game_res_h;
      bool msaa_matches = window_fb.multisample_count == settings.msaa_samples;

      if (!resolution_matches && !msaa_matches) {
        lg::info("FBO Setup: using second temporary buffer: res: {}x{} {}x{}", window_fb.width,
                 window_fb.height, settings.game_res_w, settings.game_res_h);

        // we'll need a temporary fbo to do the msaa resolve step
        // non-multisampled, and doesn't need z/stencil
        m_fbo_state.resources.resolve_buffer =
            make_fbo(settings.game_res_w, settings.game_res_h, 1, false);
      } else {
        lg::info("FBO Setup: not using second temporary buffer");
      }
    }
  }

  glBindFramebuffer(GL_FRAMEBUFFER, m_fbo_state.render_fbo->fbo_id);
  ASSERT_MSG(settings.game_res_w > 0 && settings.game_res_h > 0,
             fmt::format("Bad viewport size from game_res: {}x{}\n", settings.game_res_w,
                         settings.game_res_h));
  glViewport(0, 0, settings.game_res_w, settings.game_res_h);
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClearDepth(0.0);
  glClearStencil(0);
  glDepthMask(GL_TRUE);
  // Note: could rely on sky renderer to clear depth and color, but this causes problems with
  // letterboxing
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);
  glDisable(GL_BLEND);

  m_render_state.render_fb_w = settings.game_res_w;
  m_render_state.render_fb_h = settings.game_res_h;

  // compare the aspect ratio of the frame buffer to the requested draw area.
  float fb_aspect =
      (float)settings.window_framebuffer_width / (float)settings.window_framebuffer_height;
  float draw_aspect = (float)settings.draw_region_width / (float)settings.draw_region_height;
  float squash = fb_aspect / draw_aspect;

  // start with the full area
  m_render_state.draw_region_w = m_render_state.render_fb_w;
  m_render_state.draw_region_h = m_render_state.render_fb_h;

  // and letterbox as needed
  if (squash > 1) {
    m_render_state.draw_region_w = ((float)m_render_state.draw_region_w / squash) + 0.1;
  } else {
    m_render_state.draw_region_h = ((float)m_render_state.draw_region_h * squash) + 0.1;
  }

  // center the letterbox
  m_render_state.draw_offset_x = (settings.game_res_w - m_render_state.draw_region_w) / 2;
  m_render_state.draw_offset_y = (settings.game_res_h - m_render_state.draw_region_h) / 2;

  if (settings.borderless_windows_hacks) {
    // pretend the framebuffer is 1 pixel shorter on borderless. fullscreen issues!
    // add one pixel of vertical letterbox on borderless to make up for extra line
    m_render_state.draw_region_h--;
    m_render_state.draw_offset_y++;
  }

  m_render_state.render_fb = m_fbo_state.render_fbo->fbo_id;

  if (m_render_state.draw_region_w <= 0 || m_render_state.draw_region_h <= 0) {
    // trying to draw to 0 size region... opengl doesn't like this.
    m_render_state.draw_region_w = 640;
    m_render_state.draw_region_h = 480;
  }
  glViewport(m_render_state.draw_offset_x, m_render_state.draw_offset_y,
             m_render_state.draw_region_w, m_render_state.draw_region_h);
}

/*!
 * This function finds buckets and dispatches them to the appropriate part.
 */
void OpenGLRenderer::dispatch_buckets(DmaFollower dma,
                                      ScopedProfilerNode& prof,
                                      bool sync_after_buckets) {
  // The first thing the DMA chain should be a call to a common default-registers chain.
  // this chain resets the state of the GS. After this is buckets
  m_category_times.fill(0);

  m_render_state.buckets_base =
      dma.current_tag_offset() + 16;  // offset by 1 qw for the initial call
  m_render_state.next_bucket = m_render_state.buckets_base;

  // Find the default regs buffer
  auto initial_call_tag = dma.current_tag();
  ASSERT(initial_call_tag.kind == DmaTag::Kind::CALL);
  auto initial_call_default_regs = dma.read_and_advance();
  ASSERT(initial_call_default_regs.transferred_tag == 0);  // should be a nop.
  m_render_state.default_regs_buffer = dma.current_tag_offset();
  auto default_regs_tag = dma.current_tag();
  ASSERT(default_regs_tag.kind == DmaTag::Kind::CNT);
  ASSERT(default_regs_tag.qwc == 10);
  // TODO verify data in here.
  auto default_data = dma.read_and_advance();
  ASSERT(default_data.size_bytes > 148);
  memcpy(m_render_state.fog_color.data(), default_data.data + 144, 4);
  auto default_ret_tag = dma.current_tag();
  ASSERT(default_ret_tag.qwc == 0);
  ASSERT(default_ret_tag.kind == DmaTag::Kind::RET);
  dma.read_and_advance();

  // now we should point to the first bucket!
  ASSERT(dma.current_tag_offset() == m_render_state.next_bucket);
  m_render_state.next_bucket += 16;

  // loop over the buckets!
  for (int bucket_id = 0; bucket_id < (int)BucketId::MAX_BUCKETS; bucket_id++) {
    auto& renderer = m_bucket_renderers[bucket_id];
    auto bucket_prof = prof.make_scoped_child(renderer->name_and_id());
    g_current_render = renderer->name_and_id();
    // lg::info("Render: {} start", g_current_render);
    renderer->render(dma, &m_render_state, bucket_prof);
    if (sync_after_buckets) {
      auto pp = scoped_prof("finish");
      glFinish();
    }

    // lg::info("Render: {} end", g_current_render);
    //  should have ended at the start of the next chain
    ASSERT(dma.current_tag_offset() == m_render_state.next_bucket);
    m_render_state.next_bucket += 16;
    vif_interrupt_callback();
    m_category_times[(int)m_bucket_categories[bucket_id]] += bucket_prof.get_elapsed_time();

    // hack to draw the collision mesh in the middle the drawing
    if (bucket_id == (int)BucketId::ALPHA_TEX_LEVEL0 - 1 &&
        Gfx::g_global_settings.collision_enable) {
      auto p = prof.make_scoped_child("collision-draw");
      m_collide_renderer.render(&m_render_state, p);
    }
  }
  g_current_render = "";

  // TODO ending data.
}

/*!
 * Take a screenshot!
 */
void OpenGLRenderer::finish_screenshot(const std::string& output_name,
                                       int width,
                                       int height,
                                       int x,
                                       int y,
                                       GLuint fbo) {
  std::vector<u32> buffer(width * height);
  glPixelStorei(GL_PACK_ALIGNMENT, 1);
  GLint oldbuf;
  glGetIntegerv(GL_READ_FRAMEBUFFER_BINDING, &oldbuf);
  glBindFramebuffer(GL_READ_FRAMEBUFFER, fbo);
  glReadBuffer(GL_COLOR_ATTACHMENT0);
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
  glBindFramebuffer(GL_READ_FRAMEBUFFER, oldbuf);
}

void OpenGLRenderer::do_pcrtc_effects(float alp,
                                      SharedRenderState* render_state,
                                      ScopedProfilerNode& prof) {
  if (m_fbo_state.render_fbo->is_window) {
    // nothing to do!
  } else {
    Fbo* window_blit_src = nullptr;
    const Fbo& window_fbo = m_fbo_state.resources.window;
    if (m_fbo_state.resources.resolve_buffer.valid) {
      glBindFramebuffer(GL_READ_FRAMEBUFFER, m_fbo_state.render_fbo->fbo_id);
      glBindFramebuffer(GL_DRAW_FRAMEBUFFER, m_fbo_state.resources.resolve_buffer.fbo_id);
      glBlitFramebuffer(0,                                            // srcX0
                        0,                                            // srcY0
                        m_fbo_state.render_fbo->width,                // srcX1
                        m_fbo_state.render_fbo->height,               // srcY1
                        0,                                            // dstX0
                        0,                                            // dstY0
                        m_fbo_state.resources.resolve_buffer.width,   // dstX1
                        m_fbo_state.resources.resolve_buffer.height,  // dstY1
                        GL_COLOR_BUFFER_BIT,                          // mask
                        GL_LINEAR                                     // filter
      );
      window_blit_src = &m_fbo_state.resources.resolve_buffer;
    } else {
      window_blit_src = &m_fbo_state.resources.render_buffer;
    }

    glBindFramebuffer(GL_READ_FRAMEBUFFER, window_blit_src->fbo_id);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
    glBlitFramebuffer(0,                        // srcX0
                      0,                        // srcY0
                      window_blit_src->width,   // srcX1
                      window_blit_src->height,  // srcY1
                      0,                        // dstX0
                      0,                        // dstY0
                      window_fbo.width,         // dstX1
                      window_fbo.height,        // dstY1
                      GL_COLOR_BUFFER_BIT,      // mask
                      GL_LINEAR                 // filter
    );
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
  }
  if (alp < 1) {
    glDisable(GL_DEPTH_TEST);
    glEnable(GL_BLEND);
    glBlendFuncSeparate(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA, GL_ONE, GL_ZERO);
    glBlendEquation(GL_FUNC_ADD);
    glViewport(0, 0, m_fbo_state.resources.window.width, m_fbo_state.resources.window.height);

    m_blackout_renderer.draw(Vector4f(0, 0, 0, 1.f - alp), render_state, prof);

    glEnable(GL_DEPTH_TEST);
  }
}
