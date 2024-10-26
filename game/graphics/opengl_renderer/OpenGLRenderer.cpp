#include "OpenGLRenderer.h"

#include "common/goal_constants.h"
#include "common/log/log.h"
#include "common/util/FileUtil.h"

#include "game/graphics/opengl_renderer/BlitDisplays.h"
#include "game/graphics/opengl_renderer/DepthCue.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"
#include "game/graphics/opengl_renderer/EyeRenderer.h"
#include "game/graphics/opengl_renderer/ProgressRenderer.h"
#include "game/graphics/opengl_renderer/ShadowRenderer.h"
#include "game/graphics/opengl_renderer/SkyRenderer.h"
#include "game/graphics/opengl_renderer/TextureUploadHandler.h"
#include "game/graphics/opengl_renderer/VisDataHandler.h"
#include "game/graphics/opengl_renderer/Warp.h"
#include "game/graphics/opengl_renderer/background/Hfrag.h"
#include "game/graphics/opengl_renderer/background/Shrub.h"
#include "game/graphics/opengl_renderer/background/TFragment.h"
#include "game/graphics/opengl_renderer/background/Tie3.h"
#include "game/graphics/opengl_renderer/foreground/Generic2.h"
#include "game/graphics/opengl_renderer/foreground/Generic2BucketRenderer.h"
#include "game/graphics/opengl_renderer/foreground/Merc2BucketRenderer.h"
#include "game/graphics/opengl_renderer/foreground/Shadow2.h"
#include "game/graphics/opengl_renderer/ocean/OceanMidAndFar.h"
#include "game/graphics/opengl_renderer/ocean/OceanNear.h"
#include "game/graphics/opengl_renderer/sprite/Sprite3.h"
#include "game/graphics/pipelines/opengl.h"

#include "third-party/imgui/imgui.h"
#include "third-party/imgui/imgui_stdlib.h"

// for the vif callback
#include "game/kernel/common/kmachine.h"
#include "game/runtime.h"

#ifdef _WIN32
#include <Windows.h>
#endif
#include "common/util/string_util.h"

namespace {
std::string g_current_renderer;
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
    lg::debug("[{}] OpenGL notification 0x{:X} S{:X} T{:X}: {}", g_current_renderer, id, source,
              type, message);
  } else if (severity == GL_DEBUG_SEVERITY_LOW) {
    lg::info("[{}] OpenGL message 0x{:X} S{:X} T{:X}: {}", g_current_renderer, id, source, type,
             message);
  } else if (severity == GL_DEBUG_SEVERITY_MEDIUM) {
    lg::warn("[{}] OpenGL warn 0x{:X} S{:X} T{:X}: {}", g_current_renderer, id, source, type,
             message);
  } else if (severity == GL_DEBUG_SEVERITY_HIGH) {
    lg::error("[{}] OpenGL error 0x{:X} S{:X} T{:X}: {}", g_current_renderer, id, source, type,
              message);
    // ASSERT(false);
  }
}

OpenGLRenderer::OpenGLRenderer(std::shared_ptr<TexturePool> texture_pool,
                               std::shared_ptr<Loader> loader,
                               GameVersion version)
    : m_render_state(texture_pool, loader, version),
      m_collide_renderer(version),
      m_version(version) {
  // requires OpenGL 4.3
#ifndef __APPLE__
  // setup OpenGL errors
  glEnable(GL_DEBUG_OUTPUT);
  glDebugMessageCallback(opengl_error_callback, nullptr);
  // disable specific errors
  const GLuint gl_error_ignores_api_other[1] = {0x20071};
  glDebugMessageControl(GL_DEBUG_SOURCE_API, GL_DEBUG_TYPE_OTHER, GL_DONT_CARE, 1,
                        &gl_error_ignores_api_other[0], GL_FALSE);
#endif

  lg::info("OpenGL context version: {}", (const char*)glGetString(GL_VERSION));
  lg::info("OpenGL context renderer: {}", (const char*)glGetString(GL_RENDERER));
  lg::info("OpenGL context vendor: {}", (const char*)glGetString(GL_VENDOR));
  lg::info("OpenGL context shading language version: {}",
           (const char*)glGetString(GL_SHADING_LANGUAGE_VERSION));

  const tfrag3::Level* common_level = nullptr;
  {
    auto p = scoped_prof("load-common");
    common_level = &m_render_state.loader->load_common(*m_render_state.texture_pool, "GAME");
  }

  // initialize all renderers
  switch (m_version) {
    case GameVersion::Jak1:
      break;
    case GameVersion::Jak2:
    case GameVersion::Jak3:
      m_texture_animator =
          std::make_shared<TextureAnimator>(m_render_state.shaders, common_level, m_version);
      break;
    default:
      ASSERT(false);
  }

  m_merc2 = std::make_shared<Merc2>(m_render_state.shaders, anim_slot_array());
  m_generic2 = std::make_shared<Generic2>(m_render_state.shaders);

  // initialize all renderers
  auto p = scoped_prof("init-bucket-renderers");
  switch (m_version) {
    case GameVersion::Jak1:
      init_bucket_renderers_jak1();
      break;
    case GameVersion::Jak2:
      init_bucket_renderers_jak2();
      break;
    case GameVersion::Jak3:
      init_bucket_renderers_jak3();
      break;
    default:
      ASSERT(false);
  }
}

void OpenGLRenderer::init_bucket_renderers_jak3() {
  using namespace jak3;
  m_bucket_renderers.resize((int)BucketId::MAX_BUCKETS);
  m_bucket_categories.resize((int)BucketId::MAX_BUCKETS, BucketCategory::OTHER);
  {
    auto p = scoped_prof("render-inits");

    m_blit_displays =
        init_bucket_renderer<BlitDisplays>("blit", BucketCategory::OTHER, BucketId::BLIT_START);
    init_bucket_renderer<VisDataHandler>("vis", BucketCategory::OTHER, BucketId::BUCKET_2);

    // 4
    init_bucket_renderer<TextureUploadHandler>("tex-lcom-sky-pre", BucketCategory::TEX,
                                               BucketId::TEX_LCOM_SKY_PRE, m_texture_animator);
    init_bucket_renderer<DirectRenderer>("sky", BucketCategory::OTHER, BucketId::SKY, 1024 * 8);

    init_bucket_renderer<OceanMidAndFar>("ocean-mid-far", BucketCategory::OCEAN,
                                         BucketId::OCEAN_MID_FAR);

    // 8 (in tfrag category for now, just for stat reporting.)
    init_bucket_renderer<Hfrag>("hfrag", BucketCategory::TFRAG, BucketId::HFRAG);

    // 10
    for (int i = 0; i < LEVEL_MAX; i++) {
#define GET_BUCKET_ID_FOR_LIST(bkt1, bkt2, idx) ((int)(bkt1) + ((int)(bkt2) - (int)(bkt1)) * (idx))
      // 10
      init_bucket_renderer<TextureUploadHandler>(
          fmt::format("tex-l{}-tfrag", i), BucketCategory::TEX,
          GET_BUCKET_ID_FOR_LIST(BucketId::TEX_L0_TFRAG, BucketId::TEX_L1_TFRAG, i),
          m_texture_animator);
      // 11
      init_bucket_renderer<TFragment>(
          fmt::format("tfrag-l{}-tfrag", i), BucketCategory::TFRAG,
          GET_BUCKET_ID_FOR_LIST(BucketId::TFRAG_L0_TFRAG, BucketId::TFRAG_L1_TFRAG, i),
          std::vector{tfrag3::TFragmentTreeKind::NORMAL}, false, i, anim_slot_array());
      Tie3* tie = init_bucket_renderer<Tie3>(
          fmt::format("tie-l{}-tfrag", i), BucketCategory::TIE,
          GET_BUCKET_ID_FOR_LIST(BucketId::TIE_L0_TFRAG, BucketId::TIE_L1_TFRAG, i), i,
          anim_slot_array());
      init_bucket_renderer<Tie3AnotherCategory>(
          fmt::format("etie-l{}-tfrag", i), BucketCategory::TIE,
          GET_BUCKET_ID_FOR_LIST(BucketId::ETIE_L0_TFRAG, BucketId::ETIE_L1_TFRAG, i), tie,
          tfrag3::TieCategory::NORMAL_ENVMAP);

      // 17
      init_bucket_renderer<Merc2BucketRenderer>(
          fmt::format("merc-l{}-tfrag", i), BucketCategory::MERC,
          GET_BUCKET_ID_FOR_LIST(BucketId::MERC_L0_TFRAG, BucketId::MERC_L1_TFRAG, i), m_merc2);
      init_bucket_renderer<Generic2BucketRenderer>(
          fmt::format("gmerc-l{}-tfrag", i), BucketCategory::MERC,
          GET_BUCKET_ID_FOR_LIST(BucketId::GMERC_L0_TFRAG, BucketId::GMERC_L1_TFRAG, i), m_generic2,
          Generic2::Mode::NORMAL);

      init_bucket_renderer<TextureUploadHandler>(
          fmt::format("tex-l{}-shrub", i), BucketCategory::TEX,
          GET_BUCKET_ID_FOR_LIST(BucketId::TEX_L0_SHRUB, BucketId::TEX_L1_SHRUB, i),
          m_texture_animator);
      init_bucket_renderer<Shrub>(
          fmt::format("shrub-l{}-shrub", i), BucketCategory::SHRUB,
          GET_BUCKET_ID_FOR_LIST(BucketId::SHRUB_L0_SHRUB, BucketId::SHRUB_L1_SHRUB, i));
      init_bucket_renderer<Merc2BucketRenderer>(
          fmt::format("merc-l{}-shrub", i), BucketCategory::MERC,
          GET_BUCKET_ID_FOR_LIST(BucketId::MERC_L0_SHRUB, BucketId::MERC_L1_SHRUB, i), m_merc2);
      init_bucket_renderer<Generic2BucketRenderer>(
          fmt::format("gmerc-l{}-shrub", i), BucketCategory::MERC,
          GET_BUCKET_ID_FOR_LIST(BucketId::GMERC_L0_SHRUB, BucketId::GMERC_L1_SHRUB, i), m_generic2,
          Generic2::Mode::NORMAL);

      // 230
      init_bucket_renderer<TextureUploadHandler>(
          fmt::format("tex-l{}-alpha", i), BucketCategory::TEX,
          GET_BUCKET_ID_FOR_LIST(BucketId::TEX_L0_ALPHA, BucketId::TEX_L1_ALPHA, i),
          m_texture_animator);
      init_bucket_renderer<TFragment>(
          fmt::format("tfrag-t-l{}-alpha", i), BucketCategory::TFRAG,
          GET_BUCKET_ID_FOR_LIST(BucketId::TFRAG_L0_ALPHA, BucketId::TFRAG_L1_ALPHA, i),
          std::vector{tfrag3::TFragmentTreeKind::TRANS}, false, i, anim_slot_array());
      init_bucket_renderer<Tie3AnotherCategory>(
          fmt::format("tie-t-l{}-alpha", i), BucketCategory::TIE,
          GET_BUCKET_ID_FOR_LIST(BucketId::TIE_L0_ALPHA, BucketId::TIE_L1_ALPHA, i), tie,
          tfrag3::TieCategory::TRANS);
      init_bucket_renderer<Tie3AnotherCategory>(
          fmt::format("etie-l{}-alpha", i), BucketCategory::TIE,
          GET_BUCKET_ID_FOR_LIST(BucketId::ETIE_L0_ALPHA, BucketId::ETIE_L1_ALPHA, i), tie,
          tfrag3::TieCategory::TRANS_ENVMAP);
      init_bucket_renderer<Merc2BucketRenderer>(
          fmt::format("merc-l{}-alpha", i), BucketCategory::MERC,
          GET_BUCKET_ID_FOR_LIST(BucketId::MERC_L0_ALPHA, BucketId::MERC_L1_ALPHA, i), m_merc2);
      init_bucket_renderer<Generic2BucketRenderer>(
          fmt::format("gmerc-l{}-alpha", i), BucketCategory::GENERIC,
          GET_BUCKET_ID_FOR_LIST(BucketId::GMERC_L0_ALPHA, BucketId::GMERC_L1_ALPHA, i), m_generic2,
          Generic2::Mode::NORMAL);
      init_bucket_renderer<Tie3AnotherCategory>(
          fmt::format("tie-w-l{}-water", i), BucketCategory::TIE,
          GET_BUCKET_ID_FOR_LIST(BucketId::TIE_L0_WATER, BucketId::TIE_L1_WATER, i), tie,
          tfrag3::TieCategory::WATER);
      init_bucket_renderer<Tie3AnotherCategory>(
          fmt::format("etie-l{}-water", i), BucketCategory::TIE,
          GET_BUCKET_ID_FOR_LIST(BucketId::ETIE_L0_WATER, BucketId::ETIE_L1_WATER, i), tie,
          tfrag3::TieCategory::WATER_ENVMAP);
    }

    // 340
    init_bucket_renderer<TextureUploadHandler>("tex-lcom-tfrag", BucketCategory::TEX,
                                               BucketId::TEX_LCOM_TFRAG, m_texture_animator);
    init_bucket_renderer<Merc2BucketRenderer>("merc-lcom-tfrag", BucketCategory::MERC,
                                              BucketId::MERC_LCOM_TFRAG, m_merc2);
    init_bucket_renderer<Generic2BucketRenderer>("gmerc-lcom-tfrag", BucketCategory::GENERIC,
                                                 BucketId::GMERC_LCOM_TFRAG, m_generic2,
                                                 Generic2::Mode::NORMAL);
    // 345
    init_bucket_renderer<TextureUploadHandler>("tex-lcom-shrub", BucketCategory::TEX,
                                               BucketId::TEX_LCOM_SHRUB, m_texture_animator);
    init_bucket_renderer<Merc2BucketRenderer>("merc-lcom-shrub", BucketCategory::MERC,
                                              BucketId::MERC_LCOM_SHRUB, m_merc2);

    // 350
    init_bucket_renderer<Shadow2>("shadow", BucketCategory::OTHER, BucketId::SHADOW);

    // 351
    for (int i = 0; i < LEVEL_MAX; i++) {
#define GET_BUCKET_ID_FOR_LIST(bkt1, bkt2, idx) ((int)(bkt1) + ((int)(bkt2) - (int)(bkt1)) * (idx))
      init_bucket_renderer<TextureUploadHandler>(
          fmt::format("tex-l{}-pris", i), BucketCategory::TEX,
          GET_BUCKET_ID_FOR_LIST(BucketId::TEX_L0_PRIS, BucketId::TEX_L1_PRIS, i),
          m_texture_animator);
      init_bucket_renderer<TextureUploadHandler>(
          fmt::format("tex-l{}-pris2", i), BucketCategory::TEX,
          GET_BUCKET_ID_FOR_LIST(BucketId::TEX_L0_PRIS2, BucketId::TEX_L1_PRIS2, i),
          m_texture_animator);

      init_bucket_renderer<Merc2BucketRenderer>(
          fmt::format("merc-l{}-pris", i), BucketCategory::MERC,
          GET_BUCKET_ID_FOR_LIST(BucketId::MERC_L0_PRIS, BucketId::MERC_L1_PRIS, i), m_merc2);
      init_bucket_renderer<Generic2BucketRenderer>(
          fmt::format("gmerc-l{}-pris", i), BucketCategory::GENERIC,
          GET_BUCKET_ID_FOR_LIST(BucketId::GMERC_L0_PRIS, BucketId::GMERC_L1_PRIS, i), m_generic2,
          Generic2::Mode::NORMAL);

      init_bucket_renderer<Generic2BucketRenderer>(
          fmt::format("gmerc2-l{}-pris", i), BucketCategory::GENERIC,
          GET_BUCKET_ID_FOR_LIST(BucketId::GMERC2_L0_PRIS, BucketId::GMERC2_L1_PRIS, i), m_generic2,
          Generic2::Mode::PRIM);

      init_bucket_renderer<Merc2BucketRenderer>(
          fmt::format("merc-l{}-pris2", i), BucketCategory::MERC,
          GET_BUCKET_ID_FOR_LIST(BucketId::MERC_L0_PRIS2, BucketId::MERC_L1_PRIS2, i), m_merc2);
      init_bucket_renderer<Generic2BucketRenderer>(
          fmt::format("gmerc-l{}-pris2", i), BucketCategory::GENERIC,
          GET_BUCKET_ID_FOR_LIST(BucketId::GMERC_L0_PRIS2, BucketId::GMERC_L1_PRIS2, i), m_generic2,
          Generic2::Mode::NORMAL);
      init_bucket_renderer<Generic2BucketRenderer>(
          fmt::format("gmerc2-l{}-pris2", i), BucketCategory::GENERIC,
          GET_BUCKET_ID_FOR_LIST(BucketId::GMERC2_L0_PRIS2, BucketId::GMERC2_L1_PRIS2, i),
          m_generic2, Generic2::Mode::PRIM);
    }

    // 401
    init_bucket_renderer<TextureUploadHandler>("tex-lcom-pris", BucketCategory::TEX,
                                               BucketId::TEX_LCOM_PRIS, m_texture_animator);
    init_bucket_renderer<Merc2BucketRenderer>("merc-lcom-pris", BucketCategory::MERC,
                                              BucketId::MERC_LCOM_PRIS, m_merc2);
    init_bucket_renderer<Generic2BucketRenderer>("gmerc-lcom-pris", BucketCategory::GENERIC,
                                                 BucketId::GMERC_LCOM_PRIS, m_generic2,
                                                 Generic2::Mode::NORMAL);

    init_bucket_renderer<Generic2BucketRenderer>("gmerc2-lcom-pris", BucketCategory::GENERIC,
                                                 BucketId::GMERC2_LCOM_PRIS, m_generic2,
                                                 Generic2::Mode::PRIM);

    // 461
    init_bucket_renderer<TextureUploadHandler>("tex-lcom-sky-post", BucketCategory::TEX,
                                               BucketId::TEX_LCOM_SKY_POST, m_texture_animator);
    init_bucket_renderer<OceanNear>("ocean-near", BucketCategory::OCEAN, BucketId::OCEAN_NEAR);

    // 463
    for (int i = 0; i < LEVEL_MAX; i++) {
#define GET_BUCKET_ID_FOR_LIST(bkt1, bkt2, idx) ((int)(bkt1) + ((int)(bkt2) - (int)(bkt1)) * (idx))
      // 463
      init_bucket_renderer<TextureUploadHandler>(
          fmt::format("tex-l{}-water", i), BucketCategory::TEX,
          GET_BUCKET_ID_FOR_LIST(BucketId::TEX_L0_WATER, BucketId::TEX_L1_WATER, i),
          m_texture_animator);
      init_bucket_renderer<Merc2BucketRenderer>(
          fmt::format("merc-l{}-water", i), BucketCategory::MERC,
          GET_BUCKET_ID_FOR_LIST(BucketId::MERC_L0_WATER, BucketId::MERC_L1_WATER, i), m_merc2);
      init_bucket_renderer<Generic2BucketRenderer>(
          fmt::format("gmerc-l{}-water", i), BucketCategory::GENERIC,
          GET_BUCKET_ID_FOR_LIST(BucketId::GMERC_L0_WATER, BucketId::GMERC_L1_WATER, i), m_generic2,
          Generic2::Mode::NORMAL);
      // 466
      init_bucket_renderer<TFragment>(
          fmt::format("tfrag-l{}-water", i), BucketCategory::TFRAG,
          GET_BUCKET_ID_FOR_LIST(BucketId::TFRAG_L0_WATER, BucketId::TFRAG_L1_WATER, i),
          std::vector{tfrag3::TFragmentTreeKind::WATER}, false, i, anim_slot_array());
      init_bucket_renderer<Generic2BucketRenderer>(
          fmt::format("gmerc2-l{}-water", i), BucketCategory::GENERIC,
          GET_BUCKET_ID_FOR_LIST(BucketId::GMERC2_L0_WATER, BucketId::GMERC2_L1_WATER, i),
          m_generic2, Generic2::Mode::PRIM);
    }

    // 563
    init_bucket_renderer<TextureUploadHandler>("tex-lcom-water", BucketCategory::TEX,
                                               BucketId::TEX_LCOM_WATER, m_texture_animator);
    init_bucket_renderer<Merc2BucketRenderer>("merc-lcom-water", BucketCategory::MERC,
                                              BucketId::MERC_LCOM_WATER, m_merc2);
    init_bucket_renderer<Generic2BucketRenderer>("generic2-lcom-water", BucketCategory::GENERIC,
                                                 BucketId::GMERC2_LCOM_WATER, m_generic2,
                                                 Generic2::Mode::PRIM);

    // 568
    init_bucket_renderer<TextureUploadHandler>("tex-sprite", BucketCategory::TEX,
                                               BucketId::TEX_SPRITE, m_texture_animator);
    init_bucket_renderer<Generic2BucketRenderer>("generic-sprite-1", BucketCategory::GENERIC,
                                                 BucketId::GENERIC_SPRITE_1, m_generic2,
                                                 Generic2::Mode::PRIM);
    init_bucket_renderer<Sprite3>("particles", BucketCategory::SPRITE, BucketId::PARTICLES);
    init_bucket_renderer<Generic2BucketRenderer>("generic-sprite-2", BucketCategory::GENERIC,
                                                 BucketId::GENERIC_SPRITE_2, m_generic2,
                                                 Generic2::Mode::PRIM);
    init_bucket_renderer<Generic2BucketRenderer>("generic-sprite-3", BucketCategory::OTHER,
                                                 BucketId::GENERIC_SPRITE_3, m_generic2,
                                                 Generic2::Mode::LIGHTNING);

    init_bucket_renderer<Shadow2>("shadow2", BucketCategory::OTHER, BucketId::SHADOW2);
    init_bucket_renderer<Shadow2>("shadow3", BucketCategory::OTHER, BucketId::SHADOW3);
    // 575
    init_bucket_renderer<TextureUploadHandler>("tex-warp", BucketCategory::TEX, BucketId::TEX_WARP,
                                               m_texture_animator);
    init_bucket_renderer<Warp>("generic-warp", BucketCategory::GENERIC, BucketId::GENERIC_WARP,
                               m_generic2);

    init_bucket_renderer<TextureUploadHandler>("debug-no-zbuf1", BucketCategory::OTHER,
                                               BucketId::DEBUG_NO_ZBUF1, m_texture_animator, true);
    // 578
    init_bucket_renderer<TextureUploadHandler>("tex-hud-hud-alpha", BucketCategory::TEX,
                                               BucketId::TEX_HUD_HUD_ALPHA, m_texture_animator,
                                               true);
    init_bucket_renderer<ProgressRenderer>("hud-draw-hud-alpha", BucketCategory::OTHER,
                                           BucketId::HUD_DRAW_HUD_ALPHA, 0x8000);
    init_bucket_renderer<TextureUploadHandler>("tex-hud-pris2", BucketCategory::TEX,
                                               BucketId::TEX_HUD_PRIS2, m_texture_animator);
    init_bucket_renderer<ProgressRenderer>("hud-draw-pris2", BucketCategory::OTHER,
                                           BucketId::HUD_DRAW_PRIS2, 0x8000);
    init_bucket_renderer<ProgressRenderer>("progress", BucketCategory::OTHER, BucketId::BUCKET582,
                                           0x8000);

    // 583
    init_bucket_renderer<DirectRenderer>("debug", BucketCategory::OTHER, BucketId::DEBUG, 0x8000);
    // 584
    init_bucket_renderer<DirectRenderer>("debug-no-zbuf2", BucketCategory::OTHER,
                                         BucketId::DEBUG_NO_ZBUF2, 0x8000);
    init_bucket_renderer<DirectRenderer>("debug-menu", BucketCategory::OTHER, BucketId::DEBUG_MENU,
                                         0x8000);

    auto eye_renderer = std::make_unique<EyeRenderer>("eyes", 0);
    m_render_state.eye_renderer = eye_renderer.get();
    m_jak3_eye_renderer = std::move(eye_renderer);

    // for any unset renderers, just set them to an EmptyBucketRenderer.
    for (size_t i = 0; i < m_bucket_renderers.size(); i++) {
      if (!m_bucket_renderers[i]) {
        init_bucket_renderer<EmptyBucketRenderer>(fmt::format("bucket-{}", i),
                                                  BucketCategory::OTHER, i);
      }

      m_bucket_renderers[i]->init_shaders(m_render_state.shaders);
      m_bucket_renderers[i]->init_textures(*m_render_state.texture_pool, GameVersion::Jak3);
    }
    m_jak3_eye_renderer->init_shaders(m_render_state.shaders);
    m_jak3_eye_renderer->init_textures(*m_render_state.texture_pool, GameVersion::Jak3);
  }
}

void OpenGLRenderer::init_bucket_renderers_jak2() {
  using namespace jak2;
  m_bucket_renderers.resize((int)BucketId::MAX_BUCKETS);
  m_bucket_categories.resize((int)BucketId::MAX_BUCKETS, BucketCategory::OTHER);

  // 0
  init_bucket_renderer<VisDataHandler>("vis", BucketCategory::OTHER, BucketId::BUCKET_2);
  m_blit_displays =
      init_bucket_renderer<BlitDisplays>("blit", BucketCategory::OTHER, BucketId::BUCKET_3);
  init_bucket_renderer<TextureUploadHandler>("tex-lcom-sky-pre", BucketCategory::TEX,
                                             BucketId::TEX_LCOM_SKY_PRE, m_texture_animator);
  init_bucket_renderer<DirectRenderer>("sky-draw", BucketCategory::OTHER, BucketId::SKY_DRAW, 1024);
  init_bucket_renderer<OceanMidAndFar>("ocean-mid-far", BucketCategory::OCEAN,
                                       BucketId::OCEAN_MID_FAR);

  for (int i = 0; i < LEVEL_MAX; ++i) {
#define GET_BUCKET_ID_FOR_LIST(bkt1, bkt2, idx) ((int)(bkt1) + ((int)(bkt2) - (int)(bkt1)) * (idx))
    init_bucket_renderer<TextureUploadHandler>(
        fmt::format("tex-l{}-tfrag", i), BucketCategory::TEX,
        GET_BUCKET_ID_FOR_LIST(BucketId::TEX_L0_TFRAG, BucketId::TEX_L1_TFRAG, i),
        m_texture_animator);
    init_bucket_renderer<TFragment>(
        fmt::format("tfrag-l{}-tfrag", i), BucketCategory::TFRAG,
        GET_BUCKET_ID_FOR_LIST(BucketId::TFRAG_L0_TFRAG, BucketId::TFRAG_L1_TFRAG, i),
        std::vector{tfrag3::TFragmentTreeKind::NORMAL}, false, i, anim_slot_array());
    Tie3* tie = init_bucket_renderer<Tie3>(
        fmt::format("tie-l{}-tfrag", i), BucketCategory::TIE,
        GET_BUCKET_ID_FOR_LIST(BucketId::TIE_L0_TFRAG, BucketId::TIE_L1_TFRAG, i), i,
        anim_slot_array());
    init_bucket_renderer<Tie3AnotherCategory>(
        fmt::format("etie-l{}-tfrag", i), BucketCategory::TIE,
        GET_BUCKET_ID_FOR_LIST(BucketId::ETIE_L0_TFRAG, BucketId::ETIE_L1_TFRAG, i), tie,
        tfrag3::TieCategory::NORMAL_ENVMAP);
    init_bucket_renderer<Merc2BucketRenderer>(
        fmt::format("merc-l{}-tfrag", i), BucketCategory::MERC,
        GET_BUCKET_ID_FOR_LIST(BucketId::MERC_L0_TFRAG, BucketId::MERC_L1_TFRAG, i), m_merc2);
    init_bucket_renderer<Generic2BucketRenderer>(
        fmt::format("gmerc-l{}-tfrag", i), BucketCategory::MERC,
        GET_BUCKET_ID_FOR_LIST(BucketId::GMERC_L0_TFRAG, BucketId::GMERC_L1_TFRAG, i), m_generic2,
        Generic2::Mode::NORMAL);

    init_bucket_renderer<TextureUploadHandler>(
        fmt::format("tex-l{}-shrub", i), BucketCategory::TEX,
        GET_BUCKET_ID_FOR_LIST(BucketId::TEX_L0_SHRUB, BucketId::TEX_L1_SHRUB, i),
        m_texture_animator);
    init_bucket_renderer<Shrub>(
        fmt::format("shrub-l{}-shrub", i), BucketCategory::SHRUB,
        GET_BUCKET_ID_FOR_LIST(BucketId::SHRUB_L0_SHRUB, BucketId::SHRUB_L1_SHRUB, i));
    init_bucket_renderer<Merc2BucketRenderer>(
        fmt::format("merc-l{}-shrub", i), BucketCategory::MERC,
        GET_BUCKET_ID_FOR_LIST(BucketId::MERC_L0_SHRUB, BucketId::MERC_L1_SHRUB, i), m_merc2);
    init_bucket_renderer<Generic2BucketRenderer>(
        fmt::format("gmerc-l{}-shrub", i), BucketCategory::MERC,
        GET_BUCKET_ID_FOR_LIST(BucketId::GMERC_L0_SHRUB, BucketId::GMERC_L1_SHRUB, i), m_generic2,
        Generic2::Mode::NORMAL);

    init_bucket_renderer<TextureUploadHandler>(
        fmt::format("tex-l{}-alpha", i), BucketCategory::TEX,
        GET_BUCKET_ID_FOR_LIST(BucketId::TEX_L0_ALPHA, BucketId::TEX_L1_ALPHA, i),
        m_texture_animator);
    init_bucket_renderer<TFragment>(
        fmt::format("tfrag-t-l{}-alpha", i), BucketCategory::TFRAG,
        GET_BUCKET_ID_FOR_LIST(BucketId::TFRAG_T_L0_ALPHA, BucketId::TFRAG_T_L1_ALPHA, i),
        std::vector{tfrag3::TFragmentTreeKind::TRANS}, false, i, anim_slot_array());
    init_bucket_renderer<Tie3AnotherCategory>(
        fmt::format("tie-t-l{}-alpha", i), BucketCategory::TIE,
        GET_BUCKET_ID_FOR_LIST(BucketId::TIE_T_L0_ALPHA, BucketId::TIE_T_L1_ALPHA, i), tie,
        tfrag3::TieCategory::TRANS);
    init_bucket_renderer<Tie3AnotherCategory>(
        fmt::format("etie-t-l{}-alpha", i), BucketCategory::TIE,
        GET_BUCKET_ID_FOR_LIST(BucketId::ETIE_T_L0_ALPHA, BucketId::ETIE_T_L1_ALPHA, i), tie,
        tfrag3::TieCategory::TRANS_ENVMAP);
    init_bucket_renderer<Merc2BucketRenderer>(
        fmt::format("merc-l{}-alpha", i), BucketCategory::MERC,
        GET_BUCKET_ID_FOR_LIST(BucketId::MERC_L0_ALPHA, BucketId::MERC_L1_ALPHA, i), m_merc2);
    init_bucket_renderer<Generic2BucketRenderer>(
        fmt::format("gmerc-l{}-alpha", i), BucketCategory::GENERIC,
        GET_BUCKET_ID_FOR_LIST(BucketId::GMERC_L0_ALPHA, BucketId::GMERC_L1_ALPHA, i), m_generic2,
        Generic2::Mode::NORMAL);

    init_bucket_renderer<TextureUploadHandler>(
        fmt::format("tex-l{}-pris", i), BucketCategory::TEX,
        GET_BUCKET_ID_FOR_LIST(BucketId::TEX_L0_PRIS, BucketId::TEX_L1_PRIS, i),
        m_texture_animator);
    init_bucket_renderer<Merc2BucketRenderer>(
        fmt::format("merc-l{}-pris", i), BucketCategory::MERC,
        GET_BUCKET_ID_FOR_LIST(BucketId::MERC_L0_PRIS, BucketId::MERC_L1_PRIS, i), m_merc2);
    init_bucket_renderer<Generic2BucketRenderer>(
        fmt::format("gmerc-l{}-pris", i), BucketCategory::GENERIC,
        GET_BUCKET_ID_FOR_LIST(BucketId::GMERC_L0_PRIS, BucketId::GMERC_L1_PRIS, i), m_generic2,
        Generic2::Mode::NORMAL);

    init_bucket_renderer<TextureUploadHandler>(
        fmt::format("tex-l{}-pris2", i), BucketCategory::TEX,
        GET_BUCKET_ID_FOR_LIST(BucketId::TEX_L0_PRIS2, BucketId::TEX_L1_PRIS2, i),
        m_texture_animator);
    init_bucket_renderer<Merc2BucketRenderer>(
        fmt::format("merc-l{}-pris2", i), BucketCategory::MERC,
        GET_BUCKET_ID_FOR_LIST(BucketId::MERC_L0_PRIS2, BucketId::MERC_L1_PRIS2, i), m_merc2);
    init_bucket_renderer<Generic2BucketRenderer>(
        fmt::format("gmerc-l{}-pris2", i), BucketCategory::GENERIC,
        GET_BUCKET_ID_FOR_LIST(BucketId::GMERC_L0_PRIS2, BucketId::GMERC_L1_PRIS2, i), m_generic2,
        Generic2::Mode::NORMAL);

    init_bucket_renderer<TextureUploadHandler>(
        fmt::format("tex-l{}-water", i), BucketCategory::TEX,
        GET_BUCKET_ID_FOR_LIST(BucketId::TEX_L0_WATER, BucketId::TEX_L1_WATER, i),
        m_texture_animator);
    init_bucket_renderer<Merc2BucketRenderer>(
        fmt::format("merc-l{}-water", i), BucketCategory::MERC,
        GET_BUCKET_ID_FOR_LIST(BucketId::MERC_L0_WATER, BucketId::MERC_L1_WATER, i), m_merc2);
    init_bucket_renderer<Generic2BucketRenderer>(
        fmt::format("gmerc-l{}-water", i), BucketCategory::GENERIC,
        GET_BUCKET_ID_FOR_LIST(BucketId::GMERC_L0_WATER, BucketId::GMERC_L1_WATER, i), m_generic2,
        Generic2::Mode::NORMAL);
    init_bucket_renderer<TFragment>(
        fmt::format("tfrag-w-l{}-alpha", i), BucketCategory::TFRAG,
        GET_BUCKET_ID_FOR_LIST(BucketId::TFRAG_W_L0_WATER, BucketId::TFRAG_W_L1_WATER, i),
        std::vector{tfrag3::TFragmentTreeKind::WATER}, false, i, anim_slot_array());
    init_bucket_renderer<Tie3AnotherCategory>(
        fmt::format("tie-w-l{}-water", i), BucketCategory::TIE,
        GET_BUCKET_ID_FOR_LIST(BucketId::TIE_W_L0_WATER, BucketId::TIE_W_L1_WATER, i), tie,
        tfrag3::TieCategory::WATER);
    init_bucket_renderer<Tie3AnotherCategory>(
        fmt::format("etie-w-l{}-water", i), BucketCategory::TIE,
        GET_BUCKET_ID_FOR_LIST(BucketId::ETIE_W_L0_WATER, BucketId::ETIE_W_L1_WATER, i), tie,
        tfrag3::TieCategory::WATER_ENVMAP);
#undef GET_BUCKET_ID_FOR_LIST
  }
  // 180
  init_bucket_renderer<TextureUploadHandler>("tex-lcom-tfrag", BucketCategory::TEX,
                                             BucketId::TEX_LCOM_TFRAG, m_texture_animator);
  init_bucket_renderer<Merc2BucketRenderer>("merc-lcom-tfrag", BucketCategory::MERC,
                                            BucketId::MERC_LCOM_TFRAG, m_merc2);
  // 190
  init_bucket_renderer<TextureUploadHandler>("tex-lcom-shrub", BucketCategory::TEX,
                                             BucketId::TEX_LCOM_SHRUB, m_texture_animator);
  init_bucket_renderer<Merc2BucketRenderer>("merc-lcom-shrub", BucketCategory::MERC,
                                            BucketId::MERC_LCOM_SHRUB, m_merc2);
  init_bucket_renderer<Generic2BucketRenderer>("gmerc-lcom-tfrag", BucketCategory::GENERIC,
                                               BucketId::GMERC_LCOM_TFRAG, m_generic2,
                                               Generic2::Mode::NORMAL);
  init_bucket_renderer<Shadow2>("shadow", BucketCategory::OTHER, BucketId::SHADOW);
  // 220
  init_bucket_renderer<TextureUploadHandler>("tex-lcom-pris", BucketCategory::TEX,
                                             BucketId::TEX_LCOM_PRIS, m_texture_animator);
  init_bucket_renderer<Merc2BucketRenderer>("merc-lcom-pris", BucketCategory::MERC,
                                            BucketId::MERC_LCOM_PRIS, m_merc2);
  init_bucket_renderer<Generic2BucketRenderer>("gmerc-lcom-pris", BucketCategory::GENERIC,
                                               BucketId::GMERC_LCOM_PRIS, m_generic2,
                                               Generic2::Mode::NORMAL);
  init_bucket_renderer<TextureUploadHandler>("tex-lcom-water", BucketCategory::TEX,
                                             BucketId::TEX_LCOM_WATER, m_texture_animator);
  init_bucket_renderer<Merc2BucketRenderer>("merc-lcom-water", BucketCategory::MERC,
                                            BucketId::MERC_LCOM_WATER, m_merc2);
  init_bucket_renderer<TextureUploadHandler>("tex-lcom-sky-post", BucketCategory::TEX,
                                             BucketId::TEX_LCOM_SKY_POST, m_texture_animator);
  // 310
  init_bucket_renderer<OceanNear>("ocean-near", BucketCategory::OCEAN, BucketId::OCEAN_NEAR);
  init_bucket_renderer<TextureUploadHandler>("tex-all-sprite", BucketCategory::TEX,
                                             BucketId::TEX_ALL_SPRITE, m_texture_animator);
  init_bucket_renderer<Sprite3>("particles", BucketCategory::SPRITE, BucketId::PARTICLES);
  init_bucket_renderer<Shadow2>("shadow2", BucketCategory::OTHER, BucketId::SHADOW2);
  init_bucket_renderer<Generic2BucketRenderer>("effects", BucketCategory::OTHER, BucketId::EFFECTS,
                                               m_generic2, Generic2::Mode::LIGHTNING);
  init_bucket_renderer<TextureUploadHandler>("tex-all-warp", BucketCategory::TEX,
                                             BucketId::TEX_ALL_WARP, m_texture_animator);
  init_bucket_renderer<Warp>("warp", BucketCategory::GENERIC, BucketId::GMERC_WARP, m_generic2);
  init_bucket_renderer<TextureUploadHandler>("debug-no-zbuf1", BucketCategory::OTHER,
                                             BucketId::DEBUG_NO_ZBUF1, m_texture_animator, true);
  init_bucket_renderer<TextureUploadHandler>("tex-all-map", BucketCategory::TEX,
                                             BucketId::TEX_ALL_MAP, m_texture_animator, true);
  // 320
  init_bucket_renderer<ProgressRenderer>("progress", BucketCategory::OTHER, BucketId::PROGRESS,
                                         0x1000);
  init_bucket_renderer<DirectRenderer>("screen-filter", BucketCategory::OTHER,
                                       BucketId::SCREEN_FILTER, 256);
  init_bucket_renderer<TextureUploadHandler>("subtitle", BucketCategory::OTHER, BucketId::SUBTITLE,
                                             m_texture_animator, true);
  init_bucket_renderer<DirectRenderer>("debug2", BucketCategory::OTHER, BucketId::DEBUG2, 0x8000);
  init_bucket_renderer<DirectRenderer>("debug-no-zbuf2", BucketCategory::OTHER,
                                       BucketId::DEBUG_NO_ZBUF2, 0x8000);
  init_bucket_renderer<DirectRenderer>("debug3", BucketCategory::OTHER, BucketId::DEBUG3, 0x2000);

  auto eye_renderer = std::make_unique<EyeRenderer>("eyes", 0);
  m_render_state.eye_renderer = eye_renderer.get();
  m_jak2_eye_renderer = std::move(eye_renderer);

  {
    auto p = scoped_prof("render-inits");
    // for now, for any unset renderers, just set them to an EmptyBucketRenderer.
    for (size_t i = 0; i < m_bucket_renderers.size(); i++) {
      if (!m_bucket_renderers[i]) {
        init_bucket_renderer<EmptyBucketRenderer>(fmt::format("bucket-{}", i),
                                                  BucketCategory::OTHER, i);
      }

      m_bucket_renderers[i]->init_shaders(m_render_state.shaders);
      m_bucket_renderers[i]->init_textures(*m_render_state.texture_pool, GameVersion::Jak2);
    }
    m_jak2_eye_renderer->init_shaders(m_render_state.shaders);
    m_jak2_eye_renderer->init_textures(*m_render_state.texture_pool, GameVersion::Jak2);
  }
}

/*!
 * Construct bucket renderers.  We can specify different renderers for different buckets
 */
void OpenGLRenderer::init_bucket_renderers_jak1() {
  using namespace jak1;
  m_bucket_renderers.resize((int)BucketId::MAX_BUCKETS);
  m_bucket_categories.resize((int)BucketId::MAX_BUCKETS, BucketCategory::OTHER);

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
                                             BucketId::TFRAG_TEX_LEVEL0, m_texture_animator);
  // 6 : TFRAG_LEVEL0
  init_bucket_renderer<TFragment>("l0-tfrag-tfrag", BucketCategory::TFRAG, BucketId::TFRAG_LEVEL0,
                                  normal_tfrags, false, 0, anim_slot_array());
  // 7 : TFRAG_NEAR_LEVEL0
  // 8 : TIE_NEAR_LEVEL0
  // 9 : TIE_LEVEL0
  init_bucket_renderer<Tie3WithEnvmapJak1>("l0-tfrag-tie", BucketCategory::TIE,
                                           BucketId::TIE_LEVEL0, 0);
  // 10 : MERC_TFRAG_TEX_LEVEL0
  init_bucket_renderer<Merc2BucketRenderer>("l0-tfrag-merc", BucketCategory::MERC,
                                            BucketId::MERC_TFRAG_TEX_LEVEL0, m_merc2);
  // 11 : GMERC_TFRAG_TEX_LEVEL0
  init_bucket_renderer<Generic2BucketRenderer>("l0-tfrag-generic", BucketCategory::GENERIC,
                                               BucketId::GENERIC_TFRAG_TEX_LEVEL0, m_generic2,
                                               Generic2::Mode::NORMAL);

  //-----------------------
  // LEVEL 1 tfrag texture
  //-----------------------
  // 12 : TFRAG_TEX_LEVEL1
  init_bucket_renderer<TextureUploadHandler>("l1-tfrag-tex", BucketCategory::TEX,
                                             BucketId::TFRAG_TEX_LEVEL1, m_texture_animator);
  // 13 : TFRAG_LEVEL1
  init_bucket_renderer<TFragment>("l1-tfrag-tfrag", BucketCategory::TFRAG, BucketId::TFRAG_LEVEL1,
                                  normal_tfrags, false, 1, anim_slot_array());
  // 14 : TFRAG_NEAR_LEVEL1
  // 15 : TIE_NEAR_LEVEL1
  // 16 : TIE_LEVEL1
  init_bucket_renderer<Tie3WithEnvmapJak1>("l1-tfrag-tie", BucketCategory::TIE,
                                           BucketId::TIE_LEVEL1, 1);
  // 17 : MERC_TFRAG_TEX_LEVEL1
  init_bucket_renderer<Merc2BucketRenderer>("l1-tfrag-merc", BucketCategory::MERC,
                                            BucketId::MERC_TFRAG_TEX_LEVEL1, m_merc2);
  // 18 : GMERC_TFRAG_TEX_LEVEL1
  init_bucket_renderer<Generic2BucketRenderer>("l1-tfrag-generic", BucketCategory::GENERIC,
                                               BucketId::GENERIC_TFRAG_TEX_LEVEL1, m_generic2,
                                               Generic2::Mode::NORMAL);

  //-----------------------
  // LEVEL 0 shrub texture
  //-----------------------
  // 19 : SHRUB_TEX_LEVEL0
  init_bucket_renderer<TextureUploadHandler>("l0-shrub-tex", BucketCategory::TEX,
                                             BucketId::SHRUB_TEX_LEVEL0, m_texture_animator);
  // 20 : SHRUB_NORMAL_LEVEL0
  init_bucket_renderer<Shrub>("l0-shrub", BucketCategory::SHRUB, BucketId::SHRUB_NORMAL_LEVEL0);
  // 21 : ???
  // 22 : SHRUB_BILLBOARD_LEVEL0
  // 23 : SHRUB_TRANS_LEVEL0
  // 24 : SHRUB_GENERIC_LEVEL0
  init_bucket_renderer<Generic2BucketRenderer>("l0-shrub-generic", BucketCategory::GENERIC,
                                               BucketId::SHRUB_GENERIC_LEVEL0, m_generic2,
                                               Generic2::Mode::NORMAL);

  //-----------------------
  // LEVEL 1 shrub texture
  //-----------------------
  // 25 : SHRUB_TEX_LEVEL1
  init_bucket_renderer<TextureUploadHandler>("l1-shrub-tex", BucketCategory::TEX,
                                             BucketId::SHRUB_TEX_LEVEL1, m_texture_animator);
  // 26 : SHRUB_NORMAL_LEVEL1
  init_bucket_renderer<Shrub>("l1-shrub", BucketCategory::SHRUB, BucketId::SHRUB_NORMAL_LEVEL1);
  // 27 : ???
  // 28 : SHRUB_BILLBOARD_LEVEL1
  // 29 : SHRUB_TRANS_LEVEL1
  // 30 : SHRUB_GENERIC_LEVEL1
  init_bucket_renderer<Generic2BucketRenderer>("l1-shrub-generic", BucketCategory::GENERIC,
                                               BucketId::SHRUB_GENERIC_LEVEL1, m_generic2,
                                               Generic2::Mode::NORMAL);

  //-----------------------
  // LEVEL 0 alpha texture
  //-----------------------
  init_bucket_renderer<TextureUploadHandler>("l0-alpha-tex", BucketCategory::TEX,
                                             BucketId::ALPHA_TEX_LEVEL0, m_texture_animator);  // 31
  init_bucket_renderer<SkyBlendHandler>("l0-alpha-sky-blend-and-tfrag-trans", BucketCategory::OTHER,
                                        BucketId::TFRAG_TRANS0_AND_SKY_BLEND_LEVEL0, 0,
                                        sky_gpu_blender, sky_cpu_blender, anim_slot_array());  // 32
  // 33
  init_bucket_renderer<TFragment>("l0-alpha-tfrag", BucketCategory::TFRAG,
                                  BucketId::TFRAG_DIRT_LEVEL0, dirt_tfrags, false, 0,
                                  anim_slot_array());  // 34
  // 35
  init_bucket_renderer<TFragment>("l0-alpha-tfrag-ice", BucketCategory::TFRAG,
                                  BucketId::TFRAG_ICE_LEVEL0, ice_tfrags, false, 0,
                                  anim_slot_array());
  // 37

  //-----------------------
  // LEVEL 1 alpha texture
  //-----------------------
  init_bucket_renderer<TextureUploadHandler>("l1-alpha-tex", BucketCategory::TEX,
                                             BucketId::ALPHA_TEX_LEVEL1, m_texture_animator);  // 38
  init_bucket_renderer<SkyBlendHandler>("l1-alpha-sky-blend-and-tfrag-trans", BucketCategory::OTHER,
                                        BucketId::TFRAG_TRANS1_AND_SKY_BLEND_LEVEL1, 1,
                                        sky_gpu_blender, sky_cpu_blender, anim_slot_array());  // 39
  // 40
  init_bucket_renderer<TFragment>("l1-alpha-tfrag-dirt", BucketCategory::TFRAG,
                                  BucketId::TFRAG_DIRT_LEVEL1, dirt_tfrags, false, 1,
                                  anim_slot_array());  // 41
  // 42
  init_bucket_renderer<TFragment>("l1-alpha-tfrag-ice", BucketCategory::TFRAG,
                                  BucketId::TFRAG_ICE_LEVEL1, ice_tfrags, false, 1,
                                  anim_slot_array());
  // 44

  init_bucket_renderer<Merc2BucketRenderer>("common-alpha-merc", BucketCategory::MERC,
                                            BucketId::MERC_AFTER_ALPHA, m_merc2);

  init_bucket_renderer<Generic2BucketRenderer>("common-alpha-generic", BucketCategory::GENERIC,
                                               BucketId::GENERIC_ALPHA, m_generic2,
                                               Generic2::Mode::NORMAL);                     // 46
  init_bucket_renderer<ShadowRenderer>("shadow", BucketCategory::OTHER, BucketId::SHADOW);  // 47

  //-----------------------
  // LEVEL 0 pris texture
  //-----------------------
  init_bucket_renderer<TextureUploadHandler>("l0-pris-tex", BucketCategory::TEX,
                                             BucketId::PRIS_TEX_LEVEL0, m_texture_animator);  // 48
  init_bucket_renderer<Merc2BucketRenderer>("l0-pris-merc", BucketCategory::MERC,
                                            BucketId::MERC_PRIS_LEVEL0, m_merc2);  // 49
  init_bucket_renderer<Generic2BucketRenderer>("l0-pris-generic", BucketCategory::GENERIC,
                                               BucketId::GENERIC_PRIS_LEVEL0, m_generic2,
                                               Generic2::Mode::NORMAL);  // 50

  //-----------------------
  // LEVEL 1 pris texture
  //-----------------------
  init_bucket_renderer<TextureUploadHandler>("l1-pris-tex", BucketCategory::TEX,
                                             BucketId::PRIS_TEX_LEVEL1, m_texture_animator);  // 51
  init_bucket_renderer<Merc2BucketRenderer>("l1-pris-merc", BucketCategory::MERC,
                                            BucketId::MERC_PRIS_LEVEL1, m_merc2);  // 52
  init_bucket_renderer<Generic2BucketRenderer>("l1-pris-generic", BucketCategory::GENERIC,
                                               BucketId::GENERIC_PRIS_LEVEL1, m_generic2,
                                               Generic2::Mode::NORMAL);  // 53

  // other renderers may output to the eye renderer
  m_render_state.eye_renderer = init_bucket_renderer<EyeRenderer>(
      "common-pris-eyes", BucketCategory::OTHER, BucketId::MERC_EYES_AFTER_PRIS);  // 54

  // hack: set to merc2 for debugging
  init_bucket_renderer<Merc2BucketRenderer>("common-pris-merc", BucketCategory::MERC,
                                            BucketId::MERC_AFTER_PRIS, m_merc2);  // 55
  init_bucket_renderer<Generic2BucketRenderer>("common-pris-generic", BucketCategory::GENERIC,
                                               BucketId::GENERIC_PRIS, m_generic2,
                                               Generic2::Mode::NORMAL);  // 56

  //-----------------------
  // LEVEL 0 water texture
  //-----------------------
  init_bucket_renderer<TextureUploadHandler>("l0-water-tex", BucketCategory::TEX,
                                             BucketId::WATER_TEX_LEVEL0, m_texture_animator);  // 57
  init_bucket_renderer<Merc2BucketRenderer>("l0-water-merc", BucketCategory::MERC,
                                            BucketId::MERC_WATER_LEVEL0, m_merc2);  // 58
  init_bucket_renderer<Generic2BucketRenderer>("l0-water-generic", BucketCategory::GENERIC,
                                               BucketId::GENERIC_WATER_LEVEL0, m_generic2,
                                               Generic2::Mode::NORMAL);  // 59

  //-----------------------
  // LEVEL 1 water texture
  //-----------------------
  init_bucket_renderer<TextureUploadHandler>("l1-water-tex", BucketCategory::TEX,
                                             BucketId::WATER_TEX_LEVEL1, m_texture_animator);  // 60
  init_bucket_renderer<Merc2BucketRenderer>("l1-water-merc", BucketCategory::MERC,
                                            BucketId::MERC_WATER_LEVEL1, m_merc2);  // 61
  init_bucket_renderer<Generic2BucketRenderer>("l1-water-generic", BucketCategory::GENERIC,
                                               BucketId::GENERIC_WATER_LEVEL1, m_generic2,
                                               Generic2::Mode::NORMAL);  // 62

  init_bucket_renderer<OceanNear>("ocean-near", BucketCategory::OCEAN, BucketId::OCEAN_NEAR);  // 63

  //-----------------------
  // DEPTH CUE
  //-----------------------
  init_bucket_renderer<DepthCue>("depth-cue", BucketCategory::OTHER, BucketId::DEPTH_CUE);  // 64

  //-----------------------
  // COMMON texture
  //-----------------------
  init_bucket_renderer<TextureUploadHandler>("common-tex", BucketCategory::TEX,
                                             BucketId::PRE_SPRITE_TEX, m_texture_animator);  // 65

  init_bucket_renderer<Sprite3>("sprite", BucketCategory::SPRITE, BucketId::SPRITE);  // 66

  init_bucket_renderer<DirectRenderer>("debug", BucketCategory::OTHER, BucketId::DEBUG, 0x20000);
  init_bucket_renderer<DirectRenderer>("debug-no-zbuf", BucketCategory::OTHER,
                                       BucketId::DEBUG_NO_ZBUF, 0x8000);
  // an extra custom bucket!
  init_bucket_renderer<DirectRenderer>("subtitle", BucketCategory::OTHER, BucketId::SUBTITLE, 6000);

  // for now, for any unset renderers, just set them to an EmptyBucketRenderer.
  for (size_t i = 0; i < m_bucket_renderers.size(); i++) {
    if (!m_bucket_renderers[i]) {
      init_bucket_renderer<EmptyBucketRenderer>(fmt::format("bucket-{}", i), BucketCategory::OTHER,
                                                (BucketId)i);
    }

    m_bucket_renderers[i]->init_shaders(m_render_state.shaders);
    m_bucket_renderers[i]->init_textures(*m_render_state.texture_pool, GameVersion::Jak1);
  }
  sky_cpu_blender->init_textures(*m_render_state.texture_pool, m_version);
  sky_gpu_blender->init_textures(*m_render_state.texture_pool, m_version);
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
    lg::error("Failed to setup framebuffer: {} {} {} {} ", w, h, msaa, make_zbuf_and_stencil);
    switch (status) {
      case GL_FRAMEBUFFER_UNDEFINED:
        lg::print("GL_FRAMEBUFFER_UNDEFINED\n");
        break;
      case GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT:
        lg::print("GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT\n");
        break;
      case GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT:
        lg::print("GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT\n");
        break;
      case GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER:
        lg::print("GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER\n");
        break;
      case GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER:
        lg::print("GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER\n");
        break;
      case GL_FRAMEBUFFER_UNSUPPORTED:
        lg::print("GL_FRAMEBUFFER_UNSUPPORTED\n");
        break;
      case GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE:
        lg::print("GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE\n");
        break;
      case GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS:
        lg::print("GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS\n");
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

void OpenGLRenderer::blit_display() {
  if (m_blit_displays) {
    m_blit_displays->do_copy_back(&m_render_state);
  }
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
    g_current_renderer = "frame-setup";
    auto prof = m_profiler.root()->make_scoped_child("frame-setup");
    setup_frame(settings);
    if (settings.gpu_sync) {
      glFinish();
    }
  }

  {
    g_current_renderer = "loader";
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
    if (m_texture_animator) {
      // if animation requests weren't made, assume the level is unloaded and the textures should
      // reset.
      m_texture_animator->clear_stale_textures(m_render_state.frame_idx);
    }
  }

  // blit framebuffer so that it can be used as a texture by the game later
  {
    g_current_renderer = "blit-display";
    auto prof = m_profiler.root()->make_scoped_child("blit-display");
    blit_display();
  }

  // apply effects done with PCRTC registers
  {
    g_current_renderer = "pcrtc";
    auto prof = m_profiler.root()->make_scoped_child("pcrtc");
    do_pcrtc_effects(settings.pmode_alp_register, &m_render_state, prof);
    if (settings.gpu_sync) {
      glFinish();
    }
  }

  m_last_pmode_alp = settings.pmode_alp_register;

  if (settings.save_screenshot) {
    g_current_renderer = "screenshot";
    auto prof = m_profiler.root()->make_scoped_child("screenshot");
    int read_buffer;
    int x, y, w, h, fbo_id;

    if (settings.internal_res_screenshot) {
      Fbo* screenshot_src;
      // can't screenshot from a multisampled buffer directly -
      if (m_fbo_state.resources.resolve_buffer.valid) {
        screenshot_src = &m_fbo_state.resources.resolve_buffer;
        read_buffer = GL_COLOR_ATTACHMENT0;
      } else {
        screenshot_src = m_fbo_state.render_fbo;
        read_buffer = GL_FRONT;
      }
      w = screenshot_src->width;
      h = screenshot_src->height;
      x = 0;
      y = 0;
      fbo_id = screenshot_src->fbo_id;
    } else {
      read_buffer = GL_BACK;
      w = settings.draw_region_width;
      h = settings.draw_region_height;
      x = m_render_state.draw_offset_x;
      y = m_render_state.draw_offset_y;
      fbo_id = 0;  // window
    }
    finish_screenshot(settings.screenshot_path, w, h, x, y, fbo_id, read_buffer,
                      settings.quick_screenshot);
  }

  if (settings.draw_render_debug_window) {
    g_current_renderer = "render-window";
    auto prof = m_profiler.root()->make_scoped_child("render-window");
    draw_renderer_selection_window();
    // add a profile bar for the imgui stuff
    // vif_interrupt_callback(0);
    if (settings.gpu_sync) {
      glFinish();
    }
  }

  if (settings.draw_loader_window) {
    g_current_renderer = "loader-window";
    m_render_state.loader->draw_debug_window();
  }

  m_profiler.finish();
  //  if (m_profiler.root_time() > 0.018) {
  //    fmt::print("Slow frame: {:.2f} ms\n", m_profiler.root_time() * 1000);
  //    fmt::print("{}\n", m_profiler.to_string());
  //  }

  if (settings.draw_profiler_window) {
    g_current_renderer = "profiler-window";
    m_profiler.draw();
  }

  if (settings.draw_small_profiler_window) {
    g_current_renderer = "small-profiler-window";
    SmallProfilerStats stats;
    stats.draw_calls = m_profiler.root()->stats().draw_calls;
    stats.triangles = m_profiler.root()->stats().triangles;
    for (int i = 0; i < (int)BucketCategory::MAX_CATEGORIES; i++) {
      stats.time_per_category[i] = m_category_times[i];
    }
    m_small_profiler.draw(m_render_state.load_status_debug, stats);
  }

  if (settings.draw_subtitle_editor_window) {
    g_current_renderer = "subtitle-editor-window";
    if (m_subtitle_editor == nullptr) {
      m_subtitle_editor = new SubtitleEditor();
    }
    m_subtitle_editor->draw_window();
  }

  if (settings.draw_filters_window) {
    g_current_renderer = "filters-window";
    m_filters_menu.draw_window();
  }

  if (settings.gpu_sync) {
    g_current_renderer = "gpu-sync";
    glFinish();
  }

  g_current_renderer = "end";
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
  ImGui::Checkbox("Blackout Loads", &m_enable_fast_blackout_loads);

  if (m_texture_animator && ImGui::TreeNode("Texture Animator")) {
    m_texture_animator->draw_debug_window();
    ImGui::TreePop();
  }

  ImGui::InputText("Renderer Filter", &m_renderer_filter);

  for (size_t i = 0; i < m_bucket_renderers.size(); i++) {
    auto renderer = m_bucket_renderers[i].get();
    if (!m_renderer_filter.empty() && !str_util::contains(renderer->name(), m_renderer_filter)) {
      continue;
    }
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
  if (m_jak2_eye_renderer) {
    if (ImGui::TreeNode("Eyes")) {
      m_jak2_eye_renderer->draw_debug_window();
      ImGui::TreePop();
    }
  }
  if (m_jak3_eye_renderer) {
    if (ImGui::TreeNode("Eyes")) {
      m_jak3_eye_renderer->draw_debug_window();
      ImGui::TreePop();
    }
  }
  ImGui::End();
}

/*!
 * Pre-render frame setup.
 */
void OpenGLRenderer::setup_frame(const RenderOptions& settings) {
  // SDL controls the window framebuffer, so we just update the size:
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
  if (settings.save_screenshot || window_resized || !m_fbo_state.render_fbo ||
      !m_fbo_state.render_fbo->matches(settings.game_res_w, settings.game_res_h,
                                       settings.msaa_samples)) {
    // doesn't match, set up a new one for these settings
    lg::info("FBO Setup: requested {}x{}, msaa {}", settings.game_res_w, settings.game_res_h,
             settings.msaa_samples);

    // clear old framebuffers
    m_fbo_state.resources.render_buffer.clear();
    m_fbo_state.resources.resolve_buffer.clear();

    // NOTE: we will ALWAYS render the game to a separate framebuffer instead of directly to the
    // window framebuffer.

    // create a fbo to render to, with the desired settings
    m_fbo_state.resources.render_buffer =
        make_fbo(settings.game_res_w, settings.game_res_h, settings.msaa_samples, true);
    m_fbo_state.render_fbo = &m_fbo_state.resources.render_buffer;

    if (settings.msaa_samples != 1) {
      lg::info("FBO Setup: using second temporary buffer: res: {}x{}", settings.game_res_w,
               settings.game_res_h);

      // we'll need a temporary fbo to do the msaa resolve step
      // non-multisampled, and doesn't need z/stencil
      m_fbo_state.resources.resolve_buffer =
          make_fbo(settings.game_res_w, settings.game_res_h, 1, false);
    } else {
      lg::info("FBO Setup: not using second temporary buffer");
    }
  }

  ASSERT_MSG(settings.game_res_w > 0 && settings.game_res_h > 0,
             fmt::format("Bad viewport size from game_res: {}x{}\n", settings.game_res_w,
                         settings.game_res_h));

  ASSERT_MSG(!m_fbo_state.render_fbo->is_window, "window fbo");

  if (m_version == GameVersion::Jak1) {
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
    glViewport(0, 0, m_fbo_state.resources.window.width, m_fbo_state.resources.window.height);
    glClearColor(0.0, 0.0, 0.0, 0.0);
    glClearDepth(0.0);
    glDepthMask(GL_TRUE);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);
    glDisable(GL_BLEND);

    glBindFramebuffer(GL_FRAMEBUFFER, m_fbo_state.render_fbo->fbo_id);
    glClearColor(0.0, 0.0, 0.0, 0.0);
    glClearDepth(0.0);
    glClearStencil(0);
    glDepthMask(GL_TRUE);
    // Note: could rely on sky renderer to clear depth and color, but this causes problems with
    // letterboxing
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);
    glDisable(GL_BLEND);
    m_render_state.stencil_dirty = false;
  }
  // jak 2 does the clear in BlitDisplays.cpp

  // setup the draw region to letterbox later
  m_render_state.draw_region_w = settings.draw_region_width;
  m_render_state.draw_region_h = settings.draw_region_height;

  // center the letterbox
  m_render_state.draw_offset_x =
      (settings.window_framebuffer_width - m_render_state.draw_region_w) / 2;
  m_render_state.draw_offset_y =
      (settings.window_framebuffer_height - m_render_state.draw_region_h) / 2;

  m_render_state.render_fb = m_fbo_state.render_fbo->fbo_id;

  if (m_render_state.draw_region_w <= 0 || m_render_state.draw_region_h <= 0) {
    // trying to draw to 0 size region... opengl doesn't like this.
    m_render_state.draw_region_w = 320;
    m_render_state.draw_region_h = 240;
  }

  m_render_state.render_fb_x = 0;
  m_render_state.render_fb_y = 0;
  m_render_state.render_fb_w = settings.game_res_w;
  m_render_state.render_fb_h = settings.game_res_h;
  glViewport(0, 0, settings.game_res_w, settings.game_res_h);
}

void OpenGLRenderer::dispatch_buckets_jak1(DmaFollower dma,
                                           ScopedProfilerNode& prof,
                                           bool sync_after_buckets) {
  // The first thing the DMA chain should be a call to a common default-registers chain.
  // this chain resets the state of the GS. After this is buckets
  m_category_times.fill(0);

  m_render_state.buckets_base =
      dma.current_tag_offset() + 16;  // offset by 1 qw for the initial call
  m_render_state.next_bucket = m_render_state.buckets_base;
  m_render_state.bucket_for_vis_copy = (int)jak1::BucketId::TFRAG_LEVEL0;
  m_render_state.num_vis_to_copy = jak1::LEVEL_MAX;

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
  for (size_t bucket_id = 0; bucket_id < m_bucket_renderers.size(); bucket_id++) {
    auto& renderer = m_bucket_renderers[bucket_id];
    auto bucket_prof = prof.make_scoped_child(renderer->name_and_id());
    g_current_renderer = renderer->name_and_id();
    // lg::info("Render: {} start", g_current_renderer);
    renderer->render(dma, &m_render_state, bucket_prof);
    if (sync_after_buckets) {
      auto pp = scoped_prof("finish");
      glFinish();
    }

    // lg::info("Render: {} end", g_current_renderer);
    //  should have ended at the start of the next chain
    ASSERT(dma.current_tag_offset() == m_render_state.next_bucket);
    m_render_state.next_bucket += 16;
    vif_interrupt_callback(bucket_id);
    m_category_times[(int)m_bucket_categories[bucket_id]] += bucket_prof.get_elapsed_time();

    // hack to draw the collision mesh in the middle the drawing
    if (bucket_id == 31 - 1 && Gfx::g_global_settings.collision_enable) {
      auto p = prof.make_scoped_child("collision-draw");
      m_collide_renderer.render(&m_render_state, p);
    }
  }

  // TODO ending data.
}

void OpenGLRenderer::dispatch_buckets_jak2(DmaFollower dma,
                                           ScopedProfilerNode& prof,
                                           bool sync_after_buckets) {
  // The first thing the DMA chain should be a call to a common default-registers chain.
  // this chain resets the state of the GS. After this is buckets
  m_category_times.fill(0);

  m_render_state.buckets_base = dma.current_tag_offset();  // starts at 0 in jak 2
  m_render_state.next_bucket = m_render_state.buckets_base + 16;
  m_render_state.bucket_for_vis_copy = (int)jak2::BucketId::BUCKET_2;
  m_render_state.num_vis_to_copy = jak2::LEVEL_MAX;

  for (size_t bucket_id = 0; bucket_id < m_bucket_renderers.size(); bucket_id++) {
    auto& renderer = m_bucket_renderers[bucket_id];
    auto bucket_prof = prof.make_scoped_child(renderer->name_and_id());
    g_current_renderer = renderer->name_and_id();
    // lg::info("Render: {} start", g_current_renderer);
    renderer->render(dma, &m_render_state, bucket_prof);
    if (sync_after_buckets) {
      auto pp = scoped_prof("finish");
      glFinish();
    }

    // lg::info("Render: {} end", g_current_renderer);
    //  should have ended at the start of the next chain
    ASSERT(dma.current_tag_offset() == m_render_state.next_bucket);
    m_render_state.next_bucket += 16;
    vif_interrupt_callback(bucket_id + 1);
    m_category_times[(int)m_bucket_categories[bucket_id]] += bucket_prof.get_elapsed_time();

    // hack to draw the collision mesh in the middle the drawing
    if (bucket_id + 1 == (int)jak2::BucketId::TEX_L0_ALPHA &&
        Gfx::g_global_settings.collision_enable) {
      auto p = prof.make_scoped_child("collision-draw");
      m_collide_renderer.render(&m_render_state, p);
    }
  }
  vif_interrupt_callback(m_bucket_renderers.size());

  // TODO ending data.
}

void OpenGLRenderer::dispatch_buckets_jak3(DmaFollower dma,
                                           ScopedProfilerNode& prof,
                                           bool sync_after_buckets) {
  // The first thing the DMA chain should be a call to a common default-registers chain.
  // this chain resets the state of the GS. After this is buckets
  m_category_times.fill(0);

  m_render_state.buckets_base = dma.current_tag_offset();  // starts at 0 in jak 2
  m_render_state.next_bucket = m_render_state.buckets_base + 16;
  m_render_state.bucket_for_vis_copy = (int)jak3::BucketId::BUCKET_2;
  m_render_state.num_vis_to_copy = jak3::LEVEL_MAX;

  for (size_t bucket_id = 0; bucket_id < m_bucket_renderers.size(); bucket_id++) {
    auto& renderer = m_bucket_renderers[bucket_id];
    auto bucket_prof = prof.make_scoped_child(renderer->name_and_id());
    g_current_renderer = renderer->name_and_id();
    // lg::info("Render: {} start", g_current_renderer);
    renderer->render(dma, &m_render_state, bucket_prof);
    if (sync_after_buckets) {
      auto pp = scoped_prof("finish");
      glFinish();
    }

    // lg::info("Render: {} end", g_current_renderer);
    //  should have ended at the start of the next chain
    ASSERT(dma.current_tag_offset() == m_render_state.next_bucket);
    m_render_state.next_bucket += 16;
    vif_interrupt_callback(bucket_id + 1);
    m_category_times[(int)m_bucket_categories[bucket_id]] += bucket_prof.get_elapsed_time();

    // hack to draw the collision mesh in the middle the drawing
    if (bucket_id + 1 == (int)jak3::BucketId::TEX_L0_ALPHA &&
        Gfx::g_global_settings.collision_enable) {
      auto p = prof.make_scoped_child("collision-draw");
      m_collide_renderer.render(&m_render_state, p);
    }
  }
  vif_interrupt_callback(m_bucket_renderers.size());

  // TODO ending data.
}

/*!
 * This function finds buckets and dispatches them to the appropriate part.
 */
void OpenGLRenderer::dispatch_buckets(DmaFollower dma,
                                      ScopedProfilerNode& prof,
                                      bool sync_after_buckets) {
  g_current_renderer = "dispatch-buckets pre";

  m_render_state.version = m_version;
  m_render_state.frame_idx++;
  switch (m_version) {
    case GameVersion::Jak1:
      dispatch_buckets_jak1(dma, prof, sync_after_buckets);
      break;
    case GameVersion::Jak2:
      dispatch_buckets_jak2(dma, prof, sync_after_buckets);
      break;
    case GameVersion::Jak3:
      dispatch_buckets_jak3(dma, prof, sync_after_buckets);
      break;
    default:
      ASSERT(false);
  }

  g_current_renderer = "dispatch-buckets post";
}

#ifdef _WIN32
void win_print_last_error(const std::string& msg) {
  LPSTR lpMsgBuf;

  FormatMessage(
      FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_IGNORE_INSERTS,
      NULL, GetLastError(), MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPSTR)&lpMsgBuf, 0, NULL);

  lg::error("[OpenGLRenderer] {} Win Err: {}", msg, lpMsgBuf);
}

void copy_texture_to_clipboard(int width, int height, const std::vector<u32>& texture_data) {
  std::vector<u32> data(texture_data);

  // BGR -> RGB
  for (auto& px : data) {
    u8 r = px >> 0;
    u8 g = px >> 8;
    u8 b = px >> 16;
    u8 a = px >> 24;
    px = (a << 24) | (r << 16) | (g << 8) | (b << 0);
  }

  // Calculate the total size of the image data
  size_t image_size = data.size() * sizeof(u32);

  // BMP/DIB file header
  BITMAPINFOHEADER header;
  header.biSize = sizeof(header);
  header.biWidth = width;
  header.biHeight = height;
  header.biPlanes = 1;
  header.biBitCount = 32;
  header.biCompression = BI_RGB;
  header.biSizeImage = 0;
  header.biXPelsPerMeter = 0;
  header.biYPelsPerMeter = 0;
  header.biClrUsed = 0;
  header.biClrImportant = 0;

  // Open the clipboard
  if (!OpenClipboard(NULL)) {
    win_print_last_error("Failed to open the clipboard.");
    return;
  }

  // Empty the clipboard
  if (!EmptyClipboard()) {
    win_print_last_error("Failed to empty the clipboard.");
    CloseClipboard();
    return;
  }

  // Create a global memory object to hold the image data
  HGLOBAL hClipboardData = GlobalAlloc(GMEM_MOVEABLE, sizeof(header) + image_size);
  if (hClipboardData == NULL) {
    win_print_last_error("Failed to allocate memory for clipboard data.");
    CloseClipboard();
    return;
  }

  // Get a pointer to the global memory object
  void* pData = GlobalLock(hClipboardData);
  if (pData == NULL) {
    win_print_last_error("Failed to lock clipboard memory.");
    CloseClipboard();
    GlobalFree(hClipboardData);
    return;
  }

  // Copy the image data into the global memory object
  memcpy(pData, &header, sizeof(header));
  memcpy((char*)pData + sizeof(header), data.data(), image_size);

  // Unlock the global memory object
  if (!GlobalUnlock(hClipboardData) && GetLastError() != NO_ERROR) {
    win_print_last_error("Failed to unlock memory.");
    CloseClipboard();
    GlobalFree(hClipboardData);
    return;
  }

  // Set the image data to clipboard
  if (!SetClipboardData(CF_DIB, hClipboardData)) {
    win_print_last_error("Failed to set clipboard data.");
    CloseClipboard();
    GlobalFree(hClipboardData);
    return;
  }

  // Close the clipboard
  CloseClipboard();
  GlobalFree(hClipboardData);

  lg::info("Image data copied to clipboard successfully!");
}
#endif

/*!
 * Take a screenshot!
 */
void OpenGLRenderer::finish_screenshot(const std::string& output_name,
                                       int width,
                                       int height,
                                       int x,
                                       int y,
                                       GLuint fbo,
                                       int read_buffer,
                                       bool quick_screenshot) {
  std::vector<u32> buffer(width * height);
  glPixelStorei(GL_PACK_ALIGNMENT, 1);
  GLint oldbuf, oldreadbuf;
  glGetIntegerv(GL_READ_FRAMEBUFFER_BINDING, &oldbuf);
  glGetIntegerv(GL_READ_BUFFER, &oldreadbuf);
  glBindFramebuffer(GL_READ_FRAMEBUFFER, fbo);
  glReadBuffer(read_buffer);
  glReadPixels(x, y, width, height, GL_RGBA, GL_UNSIGNED_BYTE, buffer.data());

  // set alpha. our renderers mess this up in a way that isn't relevant to the final framebuffer.
  for (auto& px : buffer) {
    px |= 0xff000000;
  }

#ifdef _WIN32
  if (quick_screenshot) {
    // copy to clipboard (windows only)
    copy_texture_to_clipboard(width, height, buffer);
  }
#else
  (void)quick_screenshot;
  lg::error("screenshot to clipboard NYI non-Windows");
#endif

  // flip upside down in place
  for (int h = 0; h < height / 2; h++) {
    for (int w = 0; w < width; w++) {
      std::swap(buffer[h * width + w], buffer[(height - h - 1) * width + w]);
    }
  }

  file_util::write_rgba_png(output_name, buffer.data(), width, height);
  glReadBuffer(oldreadbuf);
  glBindFramebuffer(GL_READ_FRAMEBUFFER, oldbuf);
}

void OpenGLRenderer::do_pcrtc_effects(float alp,
                                      SharedRenderState* render_state,
                                      ScopedProfilerNode& prof) {
  Fbo* window_blit_src = nullptr;
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
  glBlitFramebuffer(0,                                                          // srcX0
                    0,                                                          // srcY0
                    window_blit_src->width,                                     // srcX1
                    window_blit_src->height,                                    // srcY1
                    render_state->draw_offset_x,                                // dstX0
                    render_state->draw_offset_y,                                // dstY0
                    render_state->draw_offset_x + render_state->draw_region_w,  // dstX1
                    render_state->draw_offset_y + render_state->draw_region_h,  // dstY1
                    GL_COLOR_BUFFER_BIT,                                        // mask
                    GL_LINEAR                                                   // filter
  );
  glBindFramebuffer(GL_FRAMEBUFFER, 0);

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
