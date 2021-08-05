/*!
 * @file display.cpp
 * Display for graphics. This is the game window, distinct from the runtime console.
 */

#include "display.h"
#include "gfx.h"

#include "common/log/log.h"


namespace {

bool renderer_is_correct(const GfxRendererModule* renderer, GfxPipeline pipeline) {
  return renderer->pipeline == pipeline;
}

void set_main_display(GfxDisplay* display) {
  if (Display::displays.size() > 0) {
    Display::displays[0] = display;
  } else {
    Display::displays.push_back(display);
  }
}

}


GfxDisplay::GfxDisplay(GLFWwindow* a_window) {
  set_renderer(GfxPipeline::OpenGL);
  set_window(a_window);
}

GfxDisplay::~GfxDisplay() {
  renderer->kill_display(this);
  // window_generic_ptr = nullptr;
}

void GfxDisplay::set_renderer(GfxPipeline pipeline) {
  if (is_active()) {
    lg::error("Can't change display's renderer while window exists.");
    return;
  }
  if (renderer != nullptr) {
    lg::error("A display changed renderer unexpectedly.");
    return;
  }

  renderer = Gfx::GetRenderer(pipeline);
}

void GfxDisplay::set_window(GLFWwindow* window) {
  if (!renderer_is_correct(renderer, GfxPipeline::OpenGL)) {
    lg::error("Can't set OpenGL window when using {}", renderer->name);
    return;
  }
  if (is_active()) {
    lg::error("Already have a window. Close window first.");
    return;
  }

  this->window_glfw = window;
}

void GfxDisplay::set_title(const char* title)
{
  if (!is_active()) {
    lg::error("No window to set title `{}`.", title);
    return;
  }

  this->title = title;
}

void GfxDisplay::render_graphics() {
  renderer->render_display(this);
}


namespace Display {

std::vector<GfxDisplay*> displays;
GfxDisplay* GetMainDisplay() {
  if (displays.size() == 0)
    return NULL;
  return displays.front()->is_active() ? displays.front() : NULL;
}


int InitMainDisplay(int width, int height, const char* title, GfxSettings& settings) {
  if (GetMainDisplay() != NULL) {
    lg::warn("InitMainDisplay called when main display already exists.");
    return 1;
  }

  set_main_display(settings.renderer->make_main_display(width, height, title, settings));
}

void KillDisplay(GfxDisplay* display) {
  lg::debug("kill display #x{:x}", (uintptr_t)display);
  if (!display->is_active()) {
    lg::warn("display #x{:x} cant be killed because it is not active");
    return;
  }

  if (GetMainDisplay() == display) {
    // killing the main display, kill all children displays too!
    for (auto child_disp : displays) {
      if (child_disp == display)
        continue;
      KillDisplay(child_disp);
    }
  }

  // find this display in the list and remove it from there
  auto this_disp = std::find(displays.begin(), displays.end(), display);
  displays.erase(this_disp);

  // delete the display. the destructor does the rest.
  delete display;
}

}  // namespace Display
