/*!
 * @file opengl.cpp
 * Lower-level OpenGL interface. No actual rendering is performed here!
 */

#include "opengl.h"

#include <condition_variable>
#include <memory>
#include <mutex>

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
#include "game/graphics/texture/TexturePool.h"
#include "game/runtime.h"
#include "game/sce/libscf.h"
#include "game/system/newpad.h"

#include "third-party/SDL/include/SDL.h"
#include "third-party/fmt/core.h"
#include "third-party/imgui/imgui.h"
#include "third-party/imgui/imgui_impl_opengl3.h"
#include "third-party/imgui/imgui_impl_sdl.h"
#include "third-party/imgui/imgui_style.h"
#define STBI_WINDOWS_UTF8
#include "third-party/stb_image/stb_image.h"

namespace {

constexpr bool run_dma_copy = false;

constexpr PerGameVersion<int> fr3_level_count(3, 7);

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
  float pmode_alp = 0.f;

  std::string imgui_log_filename, imgui_filename;
  GameVersion version;

  GraphicsData(GameVersion version)
      : dma_copier(EE_MAIN_MEM_SIZE),
        texture_pool(std::make_shared<TexturePool>(version)),
        loader(std::make_shared<Loader>(
            file_util::get_jak_project_dir() / "out" / game_version_names[version] / "fr3",
            fr3_level_count[version])),
        ogl_renderer(texture_pool, loader, version),
        version(version) {}
};

std::unique_ptr<GraphicsData> g_gfx_data;

static bool want_hotkey_screenshot = false;

struct {
  bool callbacks_registered = false;
  // GLFWmonitor** monitors;
  int monitor_count;
} g_glfw_state;

// void SetGlobalGLFWCallbacks() {
//   if (g_glfw_state.callbacks_registered) {
//     lg::warn("Global GLFW callbacks were already registered!");
//   }
//
//   // Get initial state
//   g_glfw_state.monitors = glfwGetMonitors(&g_glfw_state.monitor_count);
//
//   // Listen for events
//   glfwSetMonitorCallback([](GLFWmonitor* /*monitor*/, int /*event*/) {
//     // Reload monitor list
//     g_glfw_state.monitors = glfwGetMonitors(&g_glfw_state.monitor_count);
//   });
//
//   g_glfw_state.callbacks_registered = true;
// }

void ErrorCallback(int err, const char* msg) {
  lg::error("GLFW ERR {}: {}", err, std::string(msg));
}

// bool HasError() {
//   const char* ptr;
//   if (glfwGetError(&ptr) != GLFW_NO_ERROR) {
//     lg::error("glfw error: {}", ptr);
//     return true;
//   } else {
//     return false;
//   }
// }

}  // namespace

static bool gl_inited = false;
static int gl_init(GfxSettings& settings) {
  // Initialize SDL
  if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_GAMECONTROLLER | SDL_INIT_HAPTIC) != 0) {
    lg::error("Could not initialize SDL, exiting - {}", SDL_GetError());
    return NULL;
  }

  // Log some info about SDL, potentially useful
  SDL_version compiled;
  SDL_VERSION(&compiled);
  SDL_version linked;
  SDL_GetVersion(&linked);
  lg::info("SDL Initialized, compiled with version - {}.{}.{} | linked with version - {}.{}.{}",
           compiled.major, compiled.minor, compiled.patch, linked.major, linked.minor,
           linked.patch);

  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
  if (settings.debug) {
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_FLAGS, SDL_GL_CONTEXT_DEBUG_FLAG);
  } else {
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_FLAGS, 0);
  }
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 4);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);

  return 0;
}

static void gl_exit() {
  // TODO - cleanup SDL callbacks
  g_gfx_data.reset();
  gl_inited = false;
}

static void init_imgui(SDL_Window* window, SDL_GLContext gl_context, std::string glsl_version) {
  // check that version of the library is okay
  IMGUI_CHECKVERSION();

  // this does initialization for stuff like the font data
  ImGui::CreateContext();

  // Init ImGui settings
  g_gfx_data->imgui_filename = file_util::get_file_path({"imgui.ini"});
  g_gfx_data->imgui_log_filename = file_util::get_file_path({"imgui_log.txt"});
  ImGuiIO& io = ImGui::GetIO();
  // if the font doesn't exist, use the default
  std::string font_path =
      (file_util::get_jak_project_dir() / "game" / "assets" / "fonts" / "Roboto-Medium.ttf").string();
  if (file_util::file_exists(font_path)) {
    io.Fonts->AddFontFromFileTTF(font_path.c_str(), 12); // TODO - make the font size configurable
  }

  io.IniFilename = g_gfx_data->imgui_filename.c_str();
  io.LogFilename = g_gfx_data->imgui_log_filename.c_str();

  setImGuiStyle();

  // set up to get inputs for this window
  auto test = SDL_GetCurrentVideoDriver();
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
                                                   GfxSettings& settings,
                                                   GameVersion game_version,
                                                   bool is_main) {
  // Setup the window
  // TODO - dont center unless edge-case
  SDL_Window* window =
      SDL_CreateWindow(title, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, width, height,
                       SDL_WINDOW_OPENGL | SDL_WINDOW_RESIZABLE | SDL_WINDOW_ALLOW_HIGHDPI);
  if (!window) {
    lg::error("gl_make_display failed - Could not create display window - {}", SDL_GetError());
    return NULL;
  }

  // Make an OpenGL Context
  SDL_GLContext gl_context = SDL_GL_CreateContext(window);
  if (!gl_context) {
    lg::error("gl_make_display failed - Could not create OpenGL Context - {}", SDL_GetError());
    return NULL;
  }
  if (SDL_GL_MakeCurrent(window, gl_context) != 0) {
    lg::error("gl_make_display failed - Could not associated context with window - {}",
              SDL_GetError());
    return NULL;
  }

  if (!gl_inited) {
    gladLoadGLLoader((GLADloadproc)SDL_GL_GetProcAddress);
    if (!gladLoadGL()) {
      lg::error("GL init fail");
      return NULL;
    }
    g_gfx_data = std::make_unique<GraphicsData>(game_version);
    gl_inited = true;
    const char* gl_version = (const char*)glGetString(GL_VERSION);
    lg::info("OpenGL initialized - v{}.{} | Renderer: {}", GLVersion.major, GLVersion.minor,
             gl_version);
  }

  // Setup Window Icon
  // TODO - hiDPI icon
  // https://sourcegraph.com/github.com/dfranx/SHADERed/-/blob/main.cpp?L422:24&subtree=true
  int icon_width;
  int icon_height;
  std::string image_path =
      (file_util::get_jak_project_dir() / "game" / "assets" / "appicon.png").string();
  auto icon_data =
      stbi_load(image_path.c_str(), &icon_width, &icon_height, nullptr, STBI_rgb_alpha);
  if (icon_data) {
    SDL_Surface* icon_surf = SDL_CreateRGBSurfaceWithFormatFrom(
        (void*)icon_data, icon_width, icon_height, 32, 4 * icon_width, SDL_PIXELFORMAT_RGBA32);
    SDL_SetWindowIcon(window, icon_surf);
    SDL_FreeSurface(icon_surf);
    stbi_image_free(icon_data);
  } else {
    lg::error("Could not load icon for OpenGL window");
  }

  // TODO - should this actually be here, or should it be in GLDisplay?
  // SetGlobalGLFWCallbacks();

  auto display = std::make_shared<GLDisplay>(window, is_main);
  display->set_imgui_visible(Gfx::g_debug_settings.show_imgui);

  // setup imgui
  init_imgui(window, gl_context, "#version 430");

  return std::static_pointer_cast<GfxDisplay>(display);
}

GLDisplay::GLDisplay(SDL_Window* window, bool is_main) : m_window(window) {
  m_main = is_main;
  m_input_monitor = std::make_unique<Pad::InputMonitor>();

  // Get initial state
  // TODO - a mess
  // get_position(&m_last_windowed_xpos, &m_last_windowed_ypos);
  // get_size(&m_last_windowed_width, &m_last_windowed_height);

  // Register SDL Callbacks

  // update_cursor_visibility(is_imgui_visible());

  //// Listen for window-specific GLFW events
  // glfwSetWindowUserPointer(window, reinterpret_cast<void*>(this));

  // glfwSetKeyCallback(window, [](GLFWwindow* window, int key, int scancode, int action, int mods)
  // {
  //   GLDisplay* display = reinterpret_cast<GLDisplay*>(glfwGetWindowUserPointer(window));
  //   display->on_key(window, key, scancode, action, mods);
  // });

  // glfwSetMouseButtonCallback(window, [](GLFWwindow* window, int button, int action, int mode) {
  //   GLDisplay* display = reinterpret_cast<GLDisplay*>(glfwGetWindowUserPointer(window));
  //   display->on_mouse_key(window, button, action, mode);
  // });

  // glfwSetCursorPosCallback(window, [](GLFWwindow* window, double xposition, double yposition) {
  //   GLDisplay* display = reinterpret_cast<GLDisplay*>(glfwGetWindowUserPointer(window));
  //   display->on_cursor_position(window, xposition, yposition);
  // });

  // glfwSetWindowPosCallback(window, [](GLFWwindow* window, int xpos, int ypos) {
  //   GLDisplay* display = reinterpret_cast<GLDisplay*>(glfwGetWindowUserPointer(window));
  //   display->on_window_pos(window, xpos, ypos);
  // });

  // glfwSetWindowSizeCallback(window, [](GLFWwindow* window, int width, int height) {
  //   GLDisplay* display = reinterpret_cast<GLDisplay*>(glfwGetWindowUserPointer(window));
  //   display->on_window_size(window, width, height);
  // });

  // glfwSetWindowIconifyCallback(window, [](GLFWwindow* window, int iconified) {
  //   GLDisplay* display = reinterpret_cast<GLDisplay*>(glfwGetWindowUserPointer(window));
  //   display->on_iconify(window, iconified);
  // });
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
  // TODO - we should probably cleanup the context explicitly too, but we don't store it
  // SDL_GL_DeleteContext(m_gl_context);
  SDL_DestroyWindow(m_window);
  SDL_Quit();
  if (m_main) {
    gl_exit();
  }
}

void GLDisplay::update_cursor_visibility(bool is_visible) {
  /*if (Gfx::get_button_mapping().use_mouse) {
    auto cursor_mode = is_visible ? GLFW_CURSOR_NORMAL : GLFW_CURSOR_DISABLED;
    glfwSetInputMode(window, GLFW_CURSOR, cursor_mode);
  }*/
}

void GLDisplay::on_key(int key, int /*scancode*/, int action, int /*mods*/) {
  // if (action == GlfwKeyAction::Press) {
  //   // lg::debug("KEY PRESS:   key: {} scancode: {} mods: {:X}", key, scancode, mods);
  //   Pad::OnKeyPress(key);
  // } else if (action == GlfwKeyAction::Release) {
  //   // lg::debug("KEY RELEASE: key: {} scancode: {} mods: {:X}", key, scancode, mods);
  //   Pad::OnKeyRelease(key);
  //   // Debug keys input mapping TODO add remapping
  //   /*switch (key) {
  //     case GLFW_KEY_LEFT_ALT:
  //     case GLFW_KEY_RIGHT_ALT:
  //       if (glfwGetWindowAttrib(window, GLFW_FOCUSED) &&
  //           !Gfx::g_debug_settings.ignore_imgui_hide_keybind) {
  //         set_imgui_visible(!is_imgui_visible());
  //         update_cursor_visibility(window, is_imgui_visible());
  //       }
  //       break;
  //     case GLFW_KEY_F2:
  //       want_hotkey_screenshot = true;
  //       break;
  //   }*/
  // }
}

void GLDisplay::on_mouse_key(int button, int action, int /*mode*/) {
  // int key =
  //     button + GLFW_KEY_LAST;  // Mouse button index are appended after initial GLFW keys in
  //     newpad

  // if (button == GLFW_MOUSE_BUTTON_LEFT &&
  //     is_imgui_visible()) {  // Are there any other mouse buttons we don't want to use?
  //   Pad::ClearKey(key);
  //   return;
  // }

  // if (action == GlfwKeyAction::Press) {
  //   Pad::OnKeyPress(key);
  // } else if (action == GlfwKeyAction::Release) {
  //   Pad::OnKeyRelease(key);
  // }
}

void GLDisplay::on_cursor_position(double xposition, double yposition) {
  return;
  // double xoffset = xposition - last_cursor_x_position;
  // double yoffset = yposition - last_cursor_y_position;

  // last_cursor_x_position = xposition;
  // last_cursor_y_position = yposition;
  // Pad::MappingInfo mapping_info = Gfx::get_button_mapping();
  // if (is_imgui_visible() || !mapping_info.use_mouse) {
  //   if (is_cursor_position_valid == true) {
  //     /*Pad::ClearAnalogAxisValue(mapping_info, GlfwKeyCustomAxis::CURSOR_X_AXIS);
  //     Pad::ClearAnalogAxisValue(mapping_info, GlfwKeyCustomAxis::CURSOR_Y_AXIS);*/
  //     is_cursor_position_valid = false;
  //   }
  //   return;
  // }

  // if (is_cursor_position_valid == false) {
  //   is_cursor_position_valid = true;
  //   return;
  // }

  /*Pad::SetAnalogAxisValue(mapping_info, GlfwKeyCustomAxis::CURSOR_X_AXIS, xoffset);
  Pad::SetAnalogAxisValue(mapping_info, GlfwKeyCustomAxis::CURSOR_Y_AXIS, yoffset);*/
}

void GLDisplay::on_window_pos(int xpos, int ypos) {
  // only change them on a legit change, not on the initial update
  if (m_fullscreen_mode != GfxDisplayMode::ForceUpdate &&
      m_fullscreen_target_mode == GfxDisplayMode::Windowed) {
    m_last_windowed_xpos = xpos;
    m_last_windowed_ypos = ypos;
  }
}

void GLDisplay::on_window_size(int width, int height) {
  // only change them on a legit change, not on the initial update
  if (m_fullscreen_mode != GfxDisplayMode::ForceUpdate &&
      m_fullscreen_target_mode == GfxDisplayMode::Windowed) {
    m_last_windowed_width = width;
    m_last_windowed_height = height;
  }
}

void GLDisplay::on_iconify(int iconified) {
  // m_minimized = iconified == GLFW_TRUE;
}

namespace {
std::string make_full_screenshot_output_file_path(const std::string& file_name) {
  file_util::create_dir_if_needed(file_util::get_file_path({"screenshots"}));
  return file_util::get_file_path({"screenshots", file_name});
}
}  // namespace

static std::string get_current_timestamp() {
  auto current_time = std::time(0);
  auto local_current_time = *std::localtime(&current_time);
  // Remember to increase size of result if the date format is changed
  char result[20];
  std::strftime(result, sizeof(result), "%Y_%m_%d_%H_%M_%S", &local_current_time);
  return std::string(result);
}

static std::string make_hotkey_screenshot_file_name() {
  return version_to_game_name(g_game_version) + "_" + get_current_timestamp() + ".png";
}

static bool endsWith(std::string_view str, std::string_view suffix) {
  return str.size() >= suffix.size() &&
         0 == str.compare(str.size() - suffix.size(), suffix.size(), suffix);
}

void render_game_frame(int game_width,
                       int game_height,
                       int window_fb_width,
                       int window_fb_height,
                       int draw_region_width,
                       int draw_region_height,
                       int msaa_samples,
                       bool windows_borderless_hack) {
  // wait for a copied chain.
  bool got_chain = false;
  {
    auto p = scoped_prof("wait-for-dma");
    std::unique_lock<std::mutex> lock(g_gfx_data->dma_mutex);
    // note: there's a timeout here. If the engine is messed up and not sending us frames,
    // we still want to run the glfw loop.
    got_chain = g_gfx_data->dma_cv.wait_for(lock, std::chrono::milliseconds(50),
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
    options.draw_subtitle_editor_window = g_gfx_data->debug_gui.should_draw_subtitle_editor();
    options.draw_filters_window = g_gfx_data->debug_gui.should_draw_filters_menu();
    options.save_screenshot = false;
    options.gpu_sync = g_gfx_data->debug_gui.should_gl_finish();
    options.borderless_windows_hacks = windows_borderless_hack;

    // hack for jak 2 resize
    if (g_game_version == GameVersion::Jak2) {
      float ratio = 0.75 * (float)window_fb_width / (float)window_fb_height;
      if (ratio > 1) {
        window_fb_width /= ratio;
      } else {
        window_fb_height *= ratio;
      }
      options.game_res_w = window_fb_width;
      options.game_res_h = window_fb_height;
      options.window_framebuffer_width = window_fb_width;
      options.window_framebuffer_height = window_fb_height;
      options.draw_region_width = window_fb_width;
      options.draw_region_height = window_fb_height;
    }

    if (want_hotkey_screenshot && g_gfx_data->debug_gui.screenshot_hotkey_enabled) {
      options.save_screenshot = true;
      std::string screenshot_file_name = make_hotkey_screenshot_file_name();
      options.screenshot_path = make_full_screenshot_output_file_path(screenshot_file_name);
    }
    if (g_gfx_data->debug_gui.get_screenshot_flag()) {
      options.save_screenshot = true;
      options.game_res_w = g_gfx_data->debug_gui.screenshot_width;
      options.game_res_h = g_gfx_data->debug_gui.screenshot_height;
      options.draw_region_width = options.game_res_w;
      options.draw_region_height = options.game_res_h;
      options.msaa_samples = g_gfx_data->debug_gui.screenshot_samples;
      std::string screenshot_file_name = g_gfx_data->debug_gui.screenshot_name();
      if (!endsWith(screenshot_file_name, ".png")) {
        screenshot_file_name += ".png";
      }
      options.screenshot_path = make_full_screenshot_output_file_path(screenshot_file_name);
    }
    want_hotkey_screenshot = false;

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

void GLDisplay::get_position(int* x, int* y) {
  std::lock_guard<std::mutex> lk(m_lock);
  if (x) {
    *x = m_display_state.window_pos_x;
  }
  if (y) {
    *y = m_display_state.window_pos_y;
  }
}

void GLDisplay::get_size(int* width, int* height) {
  std::lock_guard<std::mutex> lk(m_lock);
  if (width) {
    *width = m_display_state.window_size_width;
  }
  if (height) {
    *height = m_display_state.window_size_height;
  }
}

void GLDisplay::get_scale(float* xs, float* ys) {
  std::lock_guard<std::mutex> lk(m_lock);
  if (xs) {
    *xs = m_display_state.window_scale_x;
  }
  if (ys) {
    *ys = m_display_state.window_scale_y;
  }
}

void GLDisplay::set_size(int width, int height) {
  // glfwSetWindowSize(m_window, width, height);
  m_pending_size.width = width;
  m_pending_size.height = height;
  m_pending_size.pending = true;

  if (windowed()) {
    m_last_windowed_width = width;
    m_last_windowed_height = height;
  }
}

void GLDisplay::update_fullscreen(GfxDisplayMode mode, int screen) {
  //  GLFWmonitor* monitor = get_monitor(screen);
  //
  //  switch (mode) {
  //    case GfxDisplayMode::Windowed: {
  //      // windowed
  //      // TODO - display mode doesn't re-position the window
  //      int x, y, width, height;
  //
  //      if (m_last_fullscreen_mode == GfxDisplayMode::Windowed) {
  //        // windowed -> windowed, keep position and size
  //        width = m_last_windowed_width;
  //        height = m_last_windowed_height;
  //        x = m_last_windowed_xpos;
  //        y = m_last_windowed_ypos;
  //        lg::debug("Windowed -> Windowed - x:{} | y:{}", x, y);
  //      } else {
  //        // fullscreen -> windowed, use last windowed size but on the monitor previously
  //        fullscreened
  //        //
  //        // glfwGetMonitorWorkarea will return the width/height of the scaled fullscreen window
  //        // - for example, you full screened a 1280x720 game on a 4K monitor -- you won't get the
  //        4k
  //        // resolution!
  //        //
  //        // Additionally, the coordinates for the top left seem very weird in stacked displays
  //        (you
  //        // get a negative Y coordinate)
  //        int monitorX, monitorY, monitorWidth, monitorHeight;
  //        glfwGetMonitorWorkarea(monitor, &monitorX, &monitorY, &monitorWidth, &monitorHeight);
  //
  //        width = m_last_windowed_width;
  //        height = m_last_windowed_height;
  //        if (monitorX < 0) {
  //          x = monitorX - 50;
  //        } else {
  //          x = monitorX + 50;
  //        }
  //        if (monitorY < 0) {
  //          y = monitorY - 50;
  //        } else {
  //          y = monitorY + 50;
  //        }
  //        lg::debug("FS -> Windowed screen: {} - x:{}:{}/{} | y:{}:{}/{}", screen, monitorX, x,
  //        width,
  //                  monitorY, y, height);
  //      }
  //
  //      glfwSetWindowAttrib(m_window, GLFW_DECORATED, GLFW_TRUE);
  //      glfwSetWindowFocusCallback(m_window, NULL);
  //      glfwSetWindowAttrib(m_window, GLFW_FLOATING, GLFW_FALSE);
  //      glfwSetWindowMonitor(m_window, NULL, x, y, width, height, GLFW_DONT_CARE);
  //    } break;
  //    case GfxDisplayMode::Fullscreen: {
  //      // TODO - when transitioning from fullscreen to windowed, it will use the old primary
  //      display
  //      // which is to say, dragging the window to a different monitor won't update the used
  //      display
  //      // fullscreen
  //      const GLFWvidmode* vmode = glfwGetVideoMode(monitor);
  //      glfwSetWindowAttrib(m_window, GLFW_DECORATED, GLFW_FALSE);
  //      glfwSetWindowFocusCallback(m_window, NULL);
  //      glfwSetWindowAttrib(m_window, GLFW_FLOATING, GLFW_FALSE);
  //      glfwSetWindowMonitor(m_window, monitor, 0, 0, vmode->width, vmode->height,
  //      GLFW_DONT_CARE);
  //    } break;
  //    case GfxDisplayMode::Borderless: {
  //      // TODO - when transitioning from fullscreen to windowed, it will use the old primary
  //      display
  //      // which is to say, dragging the window to a different monitor won't update the used
  //      display
  //      // borderless fullscreen
  //      int x, y;
  //      glfwGetMonitorPos(monitor, &x, &y);
  //      const GLFWvidmode* vmode = glfwGetVideoMode(monitor);
  //      glfwSetWindowAttrib(m_window, GLFW_DECORATED, GLFW_FALSE);
  //      // glfwSetWindowAttrib(m_window, GLFW_FLOATING, GLFW_TRUE);
  //      // glfwSetWindowFocusCallback(m_window, FocusCallback);
  // #ifdef _WIN32
  //      glfwSetWindowMonitor(m_window, NULL, x, y, vmode->width, vmode->height + 1,
  //      GLFW_DONT_CARE);
  // #else
  //      glfwSetWindowMonitor(m_window, NULL, x, y, vmode->width, vmode->height, GLFW_DONT_CARE);
  // #endif
  //    } break;
  //    default: {
  //      break;
  //    }
  //  }
}

int GLDisplay::get_screen_vmode_count() {
  std::lock_guard<std::mutex> lk(m_lock);
  return m_display_state.num_vmodes;
}

void GLDisplay::get_screen_size(int vmode_idx, s32* w_out, s32* h_out) {
  std::lock_guard<std::mutex> lk(m_lock);
  if (vmode_idx >= 0 && vmode_idx < MAX_VMODES) {
    if (w_out) {
      *w_out = m_display_state.vmodes[vmode_idx].width;
    }
    if (h_out) {
      *h_out = m_display_state.vmodes[vmode_idx].height;
    }
  } else if (fullscreen_mode() == Fullscreen) {
    if (w_out) {
      *w_out = m_display_state.largest_vmode_width;
    }
    if (h_out) {
      *h_out = m_display_state.largest_vmode_height;
    }
  } else {
    if (w_out) {
      *w_out = m_display_state.current_vmode.width;
    }
    if (h_out) {
      *h_out = m_display_state.current_vmode.height;
    }
  }
}

int GLDisplay::get_screen_rate(int vmode_idx) {
  std::lock_guard<std::mutex> lk(m_lock);
  if (vmode_idx >= 0 && vmode_idx < MAX_VMODES) {
    return m_display_state.vmodes[vmode_idx].refresh_rate;
  } else if (fullscreen_mode() == GfxDisplayMode::Fullscreen) {
    return m_display_state.largest_vmode_refresh_rate;
  } else {
    return m_display_state.current_vmode.refresh_rate;
  }
}

// GLFWmonitor* GLDisplay::get_monitor(int index) {
//   if (index < 0 || index >= g_glfw_state.monitor_count) {
//     // out of bounds, default to primary monitor
//     index = 0;
//   }
//
//   return g_glfw_state.monitors[index];
// }

int GLDisplay::get_monitor_count() {
  return g_glfw_state.monitor_count;
}

std::tuple<double, double> GLDisplay::get_mouse_pos() {
  return {last_cursor_x_position, last_cursor_y_position};
}

bool GLDisplay::minimized() {
  return m_minimized;
}

void GLDisplay::set_lock(bool lock) {
  // glfwSetWindowAttrib(m_window, GLFW_RESIZABLE, lock ? GLFW_TRUE : GLFW_FALSE);
}

bool GLDisplay::fullscreen_pending() {
  return false;
  /*GLFWmonitor* monitor;
  {
    auto _ = scoped_prof("get_monitor");
    monitor = get_monitor(fullscreen_screen());
  }

  const GLFWvidmode* vmode;
  {
    auto _ = scoped_prof("get-video-mode");
    vmode = glfwGetVideoMode(monitor);
  }

  return GfxDisplay::fullscreen_pending() ||
         (vmode->width != m_last_video_mode.width || vmode->height != m_last_video_mode.height ||
          vmode->refreshRate != m_last_video_mode.refreshRate);*/
}

void GLDisplay::fullscreen_flush() {
  GfxDisplay::fullscreen_flush();

  /*GLFWmonitor* monitor = get_monitor(fullscreen_screen());
  auto vmode = glfwGetVideoMode(monitor);*/

  // m_last_video_mode = *vmode;
}

void update_global_profiler() {
  if (g_gfx_data->debug_gui.dump_events) {
    prof().set_enable(false);
    g_gfx_data->debug_gui.dump_events = false;
    prof().dump_to_json((file_util::get_jak_project_dir() / "prof.json").string());
  }
  prof().set_enable(g_gfx_data->debug_gui.record_events);
}

// void GLDisplay::VMode::set(const GLFWvidmode* vmode) {
//   width = vmode->width;
//   height = vmode->height;
//   refresh_rate = vmode->refreshRate;
// }

// void GLDisplay::update_glfw() {
//   auto p = scoped_prof("update_glfw");
//
//   glfwPollEvents();
//   glfwMakeContextCurrent(m_window);
//   auto& mapping_info = Gfx::get_button_mapping();
//   Pad::update_gamepads(mapping_info);
//
//   glfwGetFramebufferSize(m_window, &m_display_state_copy.window_size_width,
//                          &m_display_state_copy.window_size_height);
//
//   glfwGetWindowContentScale(m_window, &m_display_state_copy.window_scale_x,
//                             &m_display_state_copy.window_scale_y);
//
//   glfwGetWindowPos(m_window, &m_display_state_copy.window_pos_x,
//                    &m_display_state_copy.window_pos_y);
//
//   GLFWmonitor* monitor = get_monitor(fullscreen_screen());
//   auto current_vmode = glfwGetVideoMode(monitor);
//   if (current_vmode) {
//     m_display_state_copy.current_vmode.set(current_vmode);
//   }
//
//   int count = 0;
//   auto vmodes = glfwGetVideoModes(monitor, &count);
//
//   if (count > MAX_VMODES) {
//     fmt::print("got too many vmodes: {}\n", count);
//     count = MAX_VMODES;
//   }
//
//   m_display_state_copy.num_vmodes = count;
//
//   m_display_state_copy.largest_vmode_width = 1;
//   m_display_state_copy.largest_vmode_refresh_rate = 1;
//   for (int i = 0; i < count; i++) {
//     if (vmodes[i].width > m_display_state_copy.largest_vmode_width) {
//       m_display_state_copy.largest_vmode_height = vmodes[i].height;
//       m_display_state_copy.largest_vmode_width = vmodes[i].width;
//     }
//
//     if (vmodes[i].refreshRate > m_display_state_copy.largest_vmode_refresh_rate) {
//       m_display_state_copy.largest_vmode_refresh_rate = vmodes[i].refreshRate;
//     }
//     m_display_state_copy.vmodes[i].set(&vmodes[i]);
//   }
//
//   if (m_pending_size.pending) {
//     glfwSetWindowSize(m_window, m_pending_size.width, m_pending_size.height);
//     m_pending_size.pending = false;
//   }
//
//   std::lock_guard<std::mutex> lk(m_lock);
//   m_display_state = m_display_state_copy;
// }

void GLDisplay::process_sdl_events() {
  SDL_Event evt;
  while (SDL_PollEvent(&evt) != 0) {
    if (evt.type == SDL_QUIT) {
      m_should_quit = true;
    }

    if (evt.type == SDL_CONTROLLERAXISMOTION) {
      lg::info("AHHH");
    }

    if (evt.type == SDL_KEYUP) {
      if (evt.key.keysym.sym == SDLK_LALT) {
        set_imgui_visible(!is_imgui_visible());
      }
    }

    if (!m_should_quit) {
      ImGui_ImplSDL2_ProcessEvent(&evt);
    }
  }
}

/*!
 * Main function called to render graphics frames. This is called in a loop.
 */
void GLDisplay::render() {
  // Process SDL Events
  process_sdl_events();
  // TODO - update SDL window

  // imgui start of frame
  {
    auto p = scoped_prof("imgui-init");
    ImGui_ImplOpenGL3_NewFrame();
    ImGui_ImplSDL2_NewFrame();
    ImGui::NewFrame();
  }

  // framebuffer size
  int fbuf_w, fbuf_h;
  SDL_GL_GetDrawableSize(m_window, &fbuf_w, &fbuf_h);
  bool windows_borderless_hacks = false;
#ifdef _WIN32
  if (last_fullscreen_mode() == GfxDisplayMode::Borderless) {
    windows_borderless_hacks = true;
  }
#endif

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
    render_game_frame(game_res_w, game_res_h, fbuf_w, fbuf_h, Gfx::g_global_settings.lbox_w,
                      Gfx::g_global_settings.lbox_h, Gfx::g_global_settings.msaa_samples,
                      windows_borderless_hacks);
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

  // update fullscreen mode, if requested
  {
    auto p = scoped_prof("fullscreen-update");
    update_last_fullscreen_mode();

    if (fullscreen_pending() && !minimized()) {
      fullscreen_flush();
    }
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
    SDL_GL_SetSwapInterval(Gfx::g_global_settings.vsync);
  }

  // Start timing for the next frame.
  g_gfx_data->debug_gui.start_frame();
  prof().instant_event("ROOT");
  update_global_profiler();

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
    g_gfx_data->texture_pool->handle_upload_now(tpage, mode, g_ee_main_mem, s7_ptr);
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

void gl_poll_events() {
  // TODO
  // glfwPollEvents();
}

void gl_set_levels(const std::vector<std::string>& levels) {
  g_gfx_data->loader->set_want_levels(levels);
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
    gl_poll_events,         // poll_events
    gl_set_levels,          // set_levels
    gl_set_pmode_alp,       // set_pmode_alp
    GfxPipeline::OpenGL,    // pipeline
    "OpenGL 4.3"            // name
};
