/*!
 * @file display.cpp
 * Display for graphics. This is the game window, distinct from the runtime console.
 */

#include "display.h"
#include "gfx.h"

#include "common/log/log.h"

/* ****************************** */
/* Internal functions  */
/* ****************************** */

namespace {

bool renderer_is_correct(const GfxRendererModule* renderer, GfxPipeline pipeline) {
  return renderer->pipeline == pipeline;
}

void set_main_display(std::shared_ptr<GfxDisplay> display) {
  if (Display::g_displays.size() > 0) {
    Display::g_displays[0] = display;
  } else {
    Display::g_displays.push_back(display);
  }
}

}  // namespace

/* ****************************** */
/* GfxDisplay  */
/* ****************************** */

GfxDisplay::GfxDisplay(GLFWwindow* a_window) {
  set_renderer(GfxPipeline::OpenGL);
  set_window(a_window);
}

GfxDisplay::~GfxDisplay() {
  m_renderer->kill_display(this);
  // window_generic_ptr = nullptr;
}

void GfxDisplay::set_renderer(GfxPipeline pipeline) {
  if (is_active()) {
    lg::error("Can't change display's renderer while window exists.");
    return;
  }
  if (m_renderer != nullptr) {
    lg::error("A display changed renderer unexpectedly.");
    return;
  }

  m_renderer = Gfx::GetRenderer(pipeline);
}

void GfxDisplay::set_window(GLFWwindow* window) {
  if (!renderer_is_correct(m_renderer, GfxPipeline::OpenGL)) {
    lg::error("Can't set OpenGL window when using {}", m_renderer->name);
    return;
  }
  if (is_active()) {
    lg::error("Already have a window. Close window first.");
    return;
  }

  this->window_glfw = window;
}

void GfxDisplay::set_title(const char* title) {
  if (!is_active()) {
    lg::error("No window to set title `{}`.", title);
    return;
  }

  m_title = title;
}

void GfxDisplay::render_graphics() {
  m_renderer->render_display(this);
}

/* ****************************** */
/* DISPLAY  */
/* ****************************** */

namespace Display {

std::vector<std::shared_ptr<GfxDisplay>> g_displays;
std::shared_ptr<GfxDisplay> GetMainDisplay() {
  if (g_displays.size() == 0)
    return NULL;
  return g_displays.front()->is_active() ? g_displays.front() : NULL;
}

int InitMainDisplay(int width, int height, const char* title, GfxSettings& settings) {
  if (GetMainDisplay() != NULL) {
    lg::warn("InitMainDisplay called when main display already exists.");
    return 1;
  }

  auto display = settings.renderer->make_main_display(width, height, title, settings);
  if (display == NULL) {
    lg::error("Failed to make main display.");
    return 1;
  }
  set_main_display(display);
  return 0;
}

void KillMainDisplay() {
  KillDisplay(GetMainDisplay());
}

void KillDisplay(std::shared_ptr<GfxDisplay> display) {
  // lg::debug("kill display #x{:x}", (uintptr_t)display);
  if (!display->is_active()) {
    lg::warn("display #x{:x} cant be killed because it is not active");
    return;
  }

  if (GetMainDisplay() == display) {
    // killing the main display, kill all children displays too!
    while (g_displays.size() > 1) {
      KillDisplay(g_displays.at(1));
    }
  }

  // find this display in the list and remove it from there
  // if everything went right the smart pointer should delete the display.
  auto this_disp = std::find(g_displays.begin(), g_displays.end(), display);
  g_displays.erase(this_disp);
}

}  // namespace Display
