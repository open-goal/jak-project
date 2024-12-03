/*!
 * @file opengl.cpp
 * Lower-level OpenGL interface. No actual rendering is performed here!
 */

#include "opengl.h"

#include <condition_variable>
#include <memory>
#include <mutex>
#include <sstream>

#include "common/dma/dma_copy.h"
#include "common/global_profiler/GlobalProfiler.h"
#include "common/goal_constants.h"
#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/FrameLimiter.h"
#include "common/util/Timer.h"
#include "common/util/compress.h"

#include "game/graphics/display.h"
#include "game/graphics/gfx.h"
#include "game/graphics/opengl_renderer/OpenGLRenderer.h"
#include "game/graphics/opengl_renderer/debug_gui.h"
#include "game/graphics/screenshot.h"
#include "game/graphics/texture/TexturePool.h"
#include "game/runtime.h"
#include "game/sce/libscf.h"
#include "game/system/hid/input_manager.h"
#include "game/system/hid/sdl_util.h"

#include "fmt/core.h"
#include "third-party/SDL/include/SDL.h"
#include "third-party/imgui/imgui.h"
#include "third-party/imgui/imgui_impl_opengl3.h"
#include "third-party/imgui/imgui_impl_sdl.h"
#include "third-party/imgui/imgui_style.h"
#define STBI_WINDOWS_UTF8
#include "common/util/dialogs.h"
#include "common/util/string_util.h"

#include "third-party/stb_image/stb_image.h"

constexpr bool run_dma_copy = false;

constexpr PerGameVersion<int> fr3_level_count(jak1::LEVEL_TOTAL,
                                              jak2::LEVEL_TOTAL,
                                              jak3::LEVEL_TOTAL);

struct GraphicsData {
  // vsync
  std::mutex sync_mutex;
  std::condition_variable sync_cv;

  // dma chain transfer
  std::mutex dma_mutex;
  std::condition_variable dma_cv;
  u64 frame_idx = 0;
  u64 frame_idx_of_input_data = 0;
  bool has_data_to_render = false;
  FixedChunkDmaCopier dma_copier;

  // texture pool
  std::shared_ptr<TexturePool> texture_pool;

  std::shared_ptr<Loader> loader;

  // temporary opengl renderer
  OpenGLRenderer ogl_renderer;

  OpenGlDebugGui debug_gui;

  FrameLimiter frame_limiter;
  Timer engine_timer;
  double last_engine_time = 1. / 60.;
  float pmode_alp = 1.f;

  std::string imgui_log_filename, imgui_filename;
  GameVersion version;

  GraphicsData(GameVersion version)
      : dma_copier(EE_MAIN_MEM_SIZE),
        texture_pool(std::make_shared<TexturePool>(version)),
        loader(std::make_shared<Loader>(
            file_util::get_jak_project_dir() / "out" / game_version_names[version] / "fr3",
            fr3_level_count[version])),
        ogl_renderer(texture_pool, loader, version),
        debug_gui(),
        version(version) {}
};

std::unique_ptr<GraphicsData> g_gfx_data;

static bool gl_inited = false;
static int gl_init(GfxGlobalSettings& settings) {
  prof().instant_event("ROOT");
  Timer gl_init_timer;
  // Initialize SDL
  {
    auto p = scoped_prof("startup::sdl::init_sdl");
    // remove SDL garbage from hooking signal handler.
    SDL_SetHint(SDL_HINT_NO_SIGNAL_HANDLERS, "1");
    if (SDL_Init(SDL_INIT_VIDEO) != 0) {
      sdl_util::log_error("Could not initialize SDL, exiting");
      dialogs::create_error_message_dialog("Critical Error Encountered",
                                           "Could not initialize SDL, exiting");
      return 1;
    }
  }

  {
    auto p = scoped_prof("startup::sdl::get_version_info");
    SDL_version compiled;
    SDL_VERSION(&compiled);
    SDL_version linked;
    SDL_GetVersion(&linked);
    lg::info("SDL Initialized, compiled with version - {}.{}.{} | linked with version - {}.{}.{}",
             compiled.major, compiled.minor, compiled.patch, linked.major, linked.minor,
             linked.patch);
  }

  {
    auto p = scoped_prof("startup::sdl::set_gl_attributes");

    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
    if (settings.debug) {
      SDL_GL_SetAttribute(SDL_GL_CONTEXT_FLAGS, SDL_GL_CONTEXT_DEBUG_FLAG);
    } else {
      SDL_GL_SetAttribute(SDL_GL_CONTEXT_FLAGS, 0);
    }
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 4);
#ifndef __APPLE__
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);
#else
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 1);
#endif
    SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 24);
    SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 8);
    SDL_GL_SetAttribute(SDL_GL_ALPHA_SIZE, 8);
  }
  lg::info("gl init took {:.3f}s\n", gl_init_timer.getSeconds());
  return 0;
}

static void gl_exit() {
  g_gfx_data.reset();
  gl_inited = false;
}

static void init_imgui(SDL_Window* window,
                       SDL_GLContext gl_context,
                       const std::string& glsl_version) {
  // check that version of the library is okay
  IMGUI_CHECKVERSION();

  // this does initialization for stuff like the font data
  ImGui::CreateContext();

  // Init ImGui settings
  g_gfx_data->imgui_filename = file_util::get_file_path({"imgui.ini"});
  g_gfx_data->imgui_log_filename = file_util::get_file_path({"imgui_log.txt"});
  ImGuiIO& io = ImGui::GetIO();
  io.ConfigFlags |= ImGuiConfigFlags_NoMouseCursorChange;  // We manage the mouse cursor!
  if (!Gfx::g_debug_settings.monospaced_font) {
    // TODO - add or switch to Noto since it supports the entire unicode range
    std::string font_path =
        (file_util::get_jak_project_dir() / "game" / "assets" / "fonts" / "NotoSansJP-Medium.ttf")
            .string();
    if (file_util::file_exists(font_path)) {
      static const ImWchar ranges[] = {
          0x0020, 0x00FF,  // Basic Latin + Latin Supplement
          0x0400, 0x052F,  // Cyrillic + Cyrillic Supplement
          0x2000, 0x206F,  // General Punctuation
          0x2DE0, 0x2DFF,  // Cyrillic Extended-A
          0x3000, 0x30FF,  // CJK Symbols and Punctuations, Hiragana, Katakana
          0x3131, 0x3163,  // Korean alphabets
          0x31F0, 0x31FF,  // Katakana Phonetic Extensions
          0x4E00, 0x9FAF,  // CJK Ideograms
          0xA640, 0xA69F,  // Cyrillic Extended-B
          0xAC00, 0xD7A3,  // Korean characters
          0xFF00, 0xFFEF,  // Half-width characters
          0xFFFD, 0xFFFD,  // Invalid
          0,
      };
      io.Fonts->AddFontFromFileTTF(font_path.c_str(), Gfx::g_debug_settings.imgui_font_size,
                                   nullptr, ranges);
    }
  }

  io.IniFilename = g_gfx_data->imgui_filename.c_str();
  io.LogFilename = g_gfx_data->imgui_log_filename.c_str();

  if (Gfx::g_debug_settings.alternate_style) {
    ImGui::applyAlternateStyle();
  }

  // set up to get inputs for this window
  ImGui_ImplSDL2_InitForOpenGL(window, gl_context);

  // NOTE: imgui's setup calls functions that may fail intentionally, and attempts to disable error
  // reporting so these errors are invisible. But it does not work, and some weird X11 default
  // cursor error is set here that we clear.
  SDL_ClearError();

  // set up the renderer
  ImGui_ImplOpenGL3_Init(glsl_version.c_str());
}

static std::shared_ptr<GfxDisplay> gl_make_display(int width,
                                                   int height,
                                                   const char* title,
                                                   GfxGlobalSettings& /*settings*/,
                                                   GameVersion game_version,
                                                   bool is_main) {
  // Setup the window
  prof().instant_event("ROOT");
  prof().begin_event("startup::sdl::create_window");
  // TODO - SDL2 doesn't seem to support HDR (and neither does windows)
  //   Related -
  //   https://answers.microsoft.com/en-us/windows/forum/all/hdr-monitor-low-brightness-after-exiting-full/999f7ee9-7ba3-4f9c-b812-bbeb9ff8dcc1
  SDL_Window* window = SDL_CreateWindow(title, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                                        width, height, SDL_WINDOW_OPENGL | SDL_WINDOW_RESIZABLE);
  // TODO - rendering code on hiDPI/Retina displays is not adequate, solve it properly so that
  // `SDL_WINDOW_ALLOW_HIGHDPI` can be added back to the window flags.
  prof().end_event();
  if (!window) {
    sdl_util::log_error("gl_make_display failed - Could not create display window");
    dialogs::create_error_message_dialog(
        "Critical Error Encountered",
        "Unable to create OpenGL window.\nOpenGOAL requires OpenGL 4.3.\nEnsure your GPU "
        "supports this and your drivers are up to date.");
    return NULL;
  }

  // Make an OpenGL Context
  prof().begin_event("startup::sdl::create_context");
  SDL_GLContext gl_context = SDL_GL_CreateContext(window);
  prof().end_event();
  if (!gl_context) {
    sdl_util::log_error("gl_make_display failed - Could not create OpenGL Context");
    dialogs::create_error_message_dialog(
        "Critical Error Encountered",
        "Unable to create OpenGL context.\nOpenGOAL requires OpenGL 4.3.\nEnsure your GPU "
        "supports this and your drivers are up to date.");
    return NULL;
  }

  {
    auto p = scoped_prof("startup::sdl::assign_context");
    if (SDL_GL_MakeCurrent(window, gl_context) != 0) {
      sdl_util::log_error("gl_make_display failed - Could not associated context with window");
      dialogs::create_error_message_dialog("Critical Error Encountered",
                                           "Unable to create OpenGL window with context.\nOpenGOAL "
                                           "requires OpenGL 4.3.\nEnsure your GPU "
                                           "supports this and your drivers are up to date.");
      return NULL;
    }
  }

  if (!gl_inited) {
    {
      auto p = scoped_prof("startup::sdl::glad_init");
      gladLoadGLLoader((GLADloadproc)SDL_GL_GetProcAddress);
      if (!gladLoadGL()) {
        lg::error("GL init fail");
        dialogs::create_error_message_dialog("Critical Error Encountered",
                                             "Unable to initialize OpenGL API.\nOpenGOAL requires "
                                             "OpenGL 4.3.\nEnsure your GPU "
                                             "supports this and your drivers are up to date.");
        return NULL;
      }
    }
    {
      auto p = scoped_prof("startup::sdl::gfx_data_init");
      g_gfx_data = std::make_unique<GraphicsData>(game_version);
    }
    gl_inited = true;
    const char* gl_version = (const char*)glGetString(GL_VERSION);
    lg::info("OpenGL initialized - v{}.{} | Renderer: {}", GLVersion.major, GLVersion.minor,
             gl_version);
  }

  {
    auto p = scoped_prof("startup::sdl::window_extras");
    float dpi = 1.0f;
    int window_display_idx = SDL_GetWindowDisplayIndex(window);
    if (window_display_idx >= 0) {
      SDL_GetDisplayDPI(window_display_idx, &dpi, NULL, NULL);
      dpi /= 96.0f;

      if (dpi <= 0.0f) {
        dpi = 1.0f;
      }
    }

    // Setup Window Icon
    const auto image_path = file_util::get_jak_project_dir() / "game" / "assets" /
                            version_to_game_name(game_version) /
                            (dpi == 1.0f ? "app64.png" : "app256.png");
    if (fs::exists(image_path)) {
      int icon_width;
      int icon_height;

      auto icon_data = stbi_load(image_path.string().c_str(), &icon_width, &icon_height, nullptr,
                                 STBI_rgb_alpha);
      if (icon_data) {
        SDL_Surface* icon_surf = SDL_CreateRGBSurfaceWithFormatFrom(
            (void*)icon_data, icon_width, icon_height, 32, 4 * icon_width, SDL_PIXELFORMAT_RGBA32);
        SDL_SetWindowIcon(window, icon_surf);
        SDL_FreeSurface(icon_surf);
        stbi_image_free(icon_data);
      } else {
        lg::error("Could not load icon for OpenGL window, couldn't load image data");
      }
    } else {
      lg::error("Could not load icon for OpenGL window, {} does not exist", image_path.string());
    }
  }

  prof().begin_event("startup::sdl::create_GLDisplay");
  auto display = std::make_shared<GLDisplay>(window, gl_context, is_main);
  display->set_imgui_visible(Gfx::g_debug_settings.show_imgui);
  prof().end_event();

  {
    auto p = scoped_prof("startup::sdl::init_imgui");
    // setup imgui
#ifdef __APPLE__
    init_imgui(window, gl_context, "#version 410");
#else
    init_imgui(window, gl_context, "#version 430");
#endif
  }

  return std::static_pointer_cast<GfxDisplay>(display);
}

GLDisplay::GLDisplay(SDL_Window* window, SDL_GLContext gl_context, bool is_main)
    : m_window(window),
      m_gl_context(gl_context),
      m_display_manager(std::make_shared<DisplayManager>(window)),
      m_input_manager(std::make_shared<InputManager>()) {
  m_main = is_main;
  m_display_manager->set_input_manager(m_input_manager);
  // Register commands
  m_input_manager->register_command(CommandBinding::Source::KEYBOARD,
                                    CommandBinding(Gfx::g_debug_settings.hide_imgui_key, [&]() {
                                      if (!Gfx::g_debug_settings.ignore_hide_imgui) {
                                        set_imgui_visible(!is_imgui_visible());
                                      }
                                    }));
  m_input_manager->register_command(
      CommandBinding::Source::KEYBOARD,
      CommandBinding(SDLK_F2, [&]() { m_take_screenshot_next_frame = true; }));
}

GLDisplay::~GLDisplay() {
  // Cleanup ImGUI
  ImGuiIO& io = ImGui::GetIO();
  io.IniFilename = nullptr;
  io.LogFilename = nullptr;
  ImGui_ImplOpenGL3_Shutdown();
  ImGui_ImplSDL2_Shutdown();
  ImGui::DestroyContext();
  // Cleanup SDL
  SDL_GL_DeleteContext(m_gl_context);
  SDL_DestroyWindow(m_window);
  SDL_Quit();
  if (m_main) {
    gl_exit();
  }
}

void render_game_frame(int game_width,
                       int game_height,
                       int window_fb_width,
                       int window_fb_height,
                       int draw_region_width,
                       int draw_region_height,
                       int msaa_samples,
                       bool take_screenshot) {
  // wait for a copied chain.
  bool got_chain = false;
  {
    auto p = scoped_prof("wait-for-dma");
    std::unique_lock<std::mutex> lock(g_gfx_data->dma_mutex);
    // there's a timeout here, so imgui can still be responsive even if we don't render anything
    got_chain = g_gfx_data->dma_cv.wait_for(lock, std::chrono::milliseconds(40),
                                            [=] { return g_gfx_data->has_data_to_render; });
  }
  // render that chain.
  if (got_chain) {
    g_gfx_data->frame_idx_of_input_data = g_gfx_data->frame_idx;
    RenderOptions options;
    options.game_res_w = game_width;
    options.game_res_h = game_height;
    options.window_framebuffer_width = window_fb_width;
    options.window_framebuffer_height = window_fb_height;
    options.draw_region_width = draw_region_width;
    options.draw_region_height = draw_region_height;
    options.msaa_samples = msaa_samples;
    options.draw_render_debug_window = g_gfx_data->debug_gui.should_draw_render_debug();
    options.draw_profiler_window = g_gfx_data->debug_gui.should_draw_profiler();
    options.draw_loader_window = g_gfx_data->debug_gui.should_draw_loader_menu();
    options.draw_subtitle_editor_window = g_gfx_data->debug_gui.should_draw_subtitle_editor();
    options.draw_filters_window = g_gfx_data->debug_gui.should_draw_filters_menu();
    options.save_screenshot = false;
    options.quick_screenshot = false;
    options.internal_res_screenshot = false;
    options.gpu_sync = g_gfx_data->debug_gui.should_gl_finish();

    if (take_screenshot) {
      options.save_screenshot = true;
      options.quick_screenshot = true;
      options.screenshot_path = file_util::make_screenshot_filepath(g_game_version);
    }
    // note : it's important we call get_screenshot_flag first because it modifies state
    if (g_gfx_data->debug_gui.get_screenshot_flag() || g_want_screenshot) {
      g_want_screenshot = false;
      options.save_screenshot = true;
      options.internal_res_screenshot = true;
      options.game_res_w = g_screen_shot_settings->width;
      options.game_res_h = g_screen_shot_settings->height;
      options.window_framebuffer_width = options.game_res_w;
      options.window_framebuffer_height = options.game_res_h;
      options.draw_region_width = options.game_res_w;
      options.draw_region_height = options.game_res_h;
      options.msaa_samples = g_screen_shot_settings->msaa;
      options.screenshot_path =
          file_util::make_screenshot_filepath(g_game_version, get_screen_shot_name());
    }

    options.draw_small_profiler_window =
        g_gfx_data->debug_gui.master_enable && g_gfx_data->debug_gui.small_profiler;
    options.pmode_alp_register = g_gfx_data->pmode_alp;

    GLint msaa_max;
    glGetIntegerv(GL_MAX_SAMPLES, &msaa_max);
    if (options.msaa_samples > msaa_max) {
      options.msaa_samples = msaa_max;
    }

    if constexpr (run_dma_copy) {
      auto& chain = g_gfx_data->dma_copier.get_last_result();
      g_gfx_data->ogl_renderer.render(DmaFollower(chain.data.data(), chain.start_offset), options);
    } else {
      auto p = scoped_prof("ogl-render");
      g_gfx_data->ogl_renderer.render(DmaFollower(g_gfx_data->dma_copier.get_last_input_data(),
                                                  g_gfx_data->dma_copier.get_last_input_offset()),
                                      options);
    }
  }

  // before vsync, mark the chain as rendered.
  {
    // should be fine to remove this mutex if the game actually waits for vsync to call
    // send_chain again. but let's be safe for now.
    std::unique_lock<std::mutex> lock(g_gfx_data->dma_mutex);
    g_gfx_data->engine_timer.start();
    g_gfx_data->has_data_to_render = false;
    g_gfx_data->sync_cv.notify_all();
  }
}

void GLDisplay::process_sdl_events() {
  SDL_Event evt;
  while (SDL_PollEvent(&evt) != 0) {
    if (evt.type == SDL_QUIT) {
      m_should_quit = true;
    }
    {
      auto p = scoped_prof("sdl-display-manager");
      m_display_manager->process_sdl_event(evt);
    }
    if (!m_should_quit) {
      {
        auto p = scoped_prof("imgui-sdl-process");
        ImGui_ImplSDL2_ProcessEvent(&evt);
      }
    }
    {
      auto p = scoped_prof("sdl-input-monitor-process-event");
      m_input_manager->process_sdl_event(evt);
    }
  }
}

/*!
 * Main function called to render graphics frames. This is called in a loop.
 */
void GLDisplay::render() {
  // Before we process the current frames SDL events we for keyboard/mouse button inputs.
  //
  // This technically means that keyboard/mouse button inputs will be a frame behind but the
  // event-based code is limiting (there aren't enough events to achieve a totally stateless
  // approach). Binding handling is still taken care of by the event code though.
  {
    auto p = scoped_prof("sdl-input-monitor-poll-for-kb-mouse");
    ImGuiIO& io = ImGui::GetIO();
    if (io.WantCaptureKeyboard) {
      m_input_manager->clear_keyboard_actions();
    } else {
      m_input_manager->poll_keyboard_data();
    }
    if (io.WantCaptureMouse) {
      m_input_manager->clear_mouse_actions();
    } else {
      m_input_manager->poll_mouse_data();
    }
    m_input_manager->finish_polling();
  }
  // Now process SDL Events
  process_sdl_events();
  // Also process any display related events received from the EE (the game)
  // this is done here so they run from the perspective of the graphics thread
  {
    auto p = scoped_prof("display-manager-ee-events");
    m_display_manager->process_ee_events();
  }
  {
    auto p = scoped_prof("input-manager-ee-events");
    m_input_manager->process_ee_events();
  }

  // imgui start of frame
  {
    auto p = scoped_prof("imgui-new-frame");
    ImGui_ImplOpenGL3_NewFrame();
    ImGui_ImplSDL2_NewFrame();
    ImGui::NewFrame();
  }

  // framebuffer size
  int fbuf_w, fbuf_h;
  SDL_GL_GetDrawableSize(m_window, &fbuf_w, &fbuf_h);

  // render game!
  g_gfx_data->debug_gui.master_enable = is_imgui_visible();
  if (g_gfx_data->debug_gui.should_advance_frame()) {
    auto p = scoped_prof("game-render");
    int game_res_w = Gfx::g_global_settings.game_res_w;
    int game_res_h = Gfx::g_global_settings.game_res_h;
    if (game_res_w <= 0 || game_res_h <= 0) {
      // if the window size reports 0, the game will ask for a 0 sized window, and nothing likes
      // that.
      game_res_w = 640;
      game_res_h = 480;
    }
    // set the size of the visible/playable portion of the game in the window
    get_display_manager()->set_game_size(Gfx::g_global_settings.lbox_w,
                                         Gfx::g_global_settings.lbox_h);
    render_game_frame(
        game_res_w, game_res_h, fbuf_w, fbuf_h, Gfx::g_global_settings.lbox_w,
        Gfx::g_global_settings.lbox_h, Gfx::g_global_settings.msaa_samples,
        m_take_screenshot_next_frame && g_gfx_data->debug_gui.screenshot_hotkey_enabled);
    // If we took a screenshot, stop taking them now!
    if (m_take_screenshot_next_frame) {
      m_take_screenshot_next_frame = false;
    }
  }

  // render debug
  if (is_imgui_visible()) {
    auto p = scoped_prof("debug-gui");
    g_gfx_data->debug_gui.draw(g_gfx_data->dma_copier.get_last_result().stats);
  }
  {
    auto p = scoped_prof("imgui-render");
    ImGui::Render();
    ImGui_ImplOpenGL3_RenderDrawData(ImGui::GetDrawData());
  }

  // actual vsync
  g_gfx_data->debug_gui.finish_frame();
  if (Gfx::g_global_settings.framelimiter) {
    auto p = scoped_prof("frame-limiter");
    g_gfx_data->frame_limiter.run(
        Gfx::g_global_settings.target_fps, Gfx::g_global_settings.experimental_accurate_lag,
        Gfx::g_global_settings.sleep_in_frame_limiter, g_gfx_data->last_engine_time);
  }

  {
    auto p = scoped_prof("swap-buffers");
    SDL_GL_SwapWindow(m_window);
  }

  // actually wait for vsync
  if (g_gfx_data->debug_gui.should_gl_finish()) {
    glFinish();
  }

  // switch vsync modes, if requested
  if (Gfx::g_global_settings.vsync != Gfx::g_global_settings.old_vsync) {
    Gfx::g_global_settings.old_vsync = Gfx::g_global_settings.vsync;
    // NOTE - -1 can be used for adaptive vsync, maybe useful for Jak 2+?
    // https://wiki.libsdl.org/SDL2/SDL_GL_SetSwapInterval
    SDL_GL_SetSwapInterval(Gfx::g_global_settings.vsync);
  }

  // Start timing for the next frame.
  g_gfx_data->debug_gui.start_frame();
  prof().instant_event("ROOT");

  // toggle even odd and wake up engine waiting on vsync.
  // TODO: we could play with moving this earlier, right after the final bucket renderer.
  //       it breaks the VIF-interrupt profiling though.
  {
    prof().instant_event("engine-notify");
    std::unique_lock<std::mutex> lock(g_gfx_data->sync_mutex);
    g_gfx_data->frame_idx++;
    g_gfx_data->sync_cv.notify_all();
  }

  // reboot whole game, if requested
  if (g_gfx_data->debug_gui.want_reboot_in_debug) {
    g_gfx_data->debug_gui.want_reboot_in_debug = false;
    MasterExit = RuntimeExitStatus::RESTART_IN_DEBUG;
  }

  {
    auto p = scoped_prof("check-close-window");
    // exit if display window was closed
    if (m_should_quit) {
      std::unique_lock<std::mutex> lock(g_gfx_data->sync_mutex);
      MasterExit = RuntimeExitStatus::EXIT;
      g_gfx_data->sync_cv.notify_all();
    }
  }
}

/*!
 * Wait for the next vsync. Returns 0 or 1 depending on if frame is even or odd.
 * Called from the game thread, on a GOAL stack.
 */
u32 gl_vsync() {
  if (!g_gfx_data) {
    return 0;
  }
  std::unique_lock<std::mutex> lock(g_gfx_data->sync_mutex);
  auto init_frame = g_gfx_data->frame_idx_of_input_data;
  g_gfx_data->sync_cv.wait(lock, [=] {
    return (MasterExit != RuntimeExitStatus::RUNNING) || g_gfx_data->frame_idx > init_frame;
  });
  return g_gfx_data->frame_idx & 1;
}

u32 gl_sync_path() {
  if (!g_gfx_data) {
    return 0;
  }
  std::unique_lock<std::mutex> lock(g_gfx_data->sync_mutex);
  g_gfx_data->last_engine_time = g_gfx_data->engine_timer.getSeconds();
  if (!g_gfx_data->has_data_to_render) {
    return 0;
  }
  g_gfx_data->sync_cv.wait(lock, [=] { return !g_gfx_data->has_data_to_render; });
  return 0;
}

/*!
 * Send DMA to the renderer.
 * Called from the game thread, on a GOAL stack.
 */
void gl_send_chain(const void* data, u32 offset) {
  if (g_gfx_data) {
    std::unique_lock<std::mutex> lock(g_gfx_data->dma_mutex);
    if (g_gfx_data->has_data_to_render) {
      lg::error(
          "Gfx::send_chain called when the graphics renderer has pending data. Was this called "
          "multiple times per frame?");
      return;
    }

    // we copy the dma data and give a copy of it to the render.
    // the copy has a few advantages:
    // - if the game code has a bug and corrupts the DMA buffer, the renderer won't see it.
    // - the copied DMA is much smaller than the entire game memory, so it can be dumped to a
    // file
    //    separate of the entire RAM.
    // - it verifies the DMA data is valid early on.
    // but it may also be pretty expensive. Both the renderer and the game wait on this to
    // complete.

    // The renderers should just operate on DMA chains, so eliminating this step in the future
    // may be easy.

    g_gfx_data->dma_copier.set_input_data(data, offset, run_dma_copy);

    g_gfx_data->has_data_to_render = true;
    g_gfx_data->dma_cv.notify_all();
  }
}

/*!
 * Upload texture outside of main DMA chain.
 * We trust the game to not remove textures that are currently being used, but if the game is messed
 * up, there is a possible race to updating this texture.
 */
void gl_texture_upload_now(const u8* tpage, int mode, u32 s7_ptr) {
  // block
  if (g_gfx_data) {
    // just pass it to the texture pool.
    // the texture pool will take care of locking.
    // we don't want to lock here for the entire duration of the conversion.
    g_gfx_data->texture_pool->handle_upload_now(tpage, mode, g_ee_main_mem, s7_ptr, false);
  }
}

/*!
 * Handle a local->local texture copy. The texture pool can just update texture pointers.
 * This is called from the main thread and the texture pool itself will handle locking.
 */
void gl_texture_relocate(u32 destination, u32 source, u32 format) {
  if (g_gfx_data) {
    g_gfx_data->texture_pool->relocate(destination, source, format);
  }
}

void gl_set_levels(const std::vector<std::string>& levels) {
  g_gfx_data->loader->set_want_levels(levels);
}

void gl_set_active_levels(const std::vector<std::string>& levels) {
  g_gfx_data->loader->set_active_levels(levels);
}

void gl_set_pmode_alp(float val) {
  g_gfx_data->pmode_alp = val;
}

const GfxRendererModule gRendererOpenGL = {
    gl_init,                // init
    gl_make_display,        // make_display
    gl_exit,                // exit
    gl_vsync,               // vsync
    gl_sync_path,           // sync_path
    gl_send_chain,          // send_chain
    gl_texture_upload_now,  // texture_upload_now
    gl_texture_relocate,    // texture_relocate
    gl_set_levels,          // set_levels
    gl_set_active_levels,   // set_active_levels
    gl_set_pmode_alp,       // set_pmode_alp
    GfxPipeline::OpenGL,    // pipeline
    "OpenGL 4.3"            // name
};
