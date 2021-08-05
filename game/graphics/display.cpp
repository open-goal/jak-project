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

void set_main_display(std::shared_ptr<GfxDisplay>& display) {
  if (Display::displays.size() > 0) {
    Display::displays[0] = std::move(display);
  } else {
    Display::displays.push_back(std::move(display));
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

std::vector<std::shared_ptr<GfxDisplay>> displays;
std::shared_ptr<GfxDisplay> GetMainDisplay() {
  if (displays.size() == 0)
    return NULL;
  if (displays.front()->is_active()) {
    return displays.front();
  } else {
    return NULL;
  }
  // return displays.front()->is_active() ? std::move(displays.front()) : NULL;
}


int InitMainDisplay(int width, int height, const char* title, GfxSettings& settings) {
  if (GetMainDisplay() != NULL) {
    lg::warn("InitMainDisplay called when main display already exists.");
    return 1;
  }

  set_main_display(settings.renderer->make_main_display(width, height, title, settings));
}

void KillDisplay(std::shared_ptr<GfxDisplay>& display) {
  // lg::debug("kill display #x{:x}", (uintptr_t)display);
  if (!display->is_active()) {
    lg::warn("display #x{:x} cant be killed because it is not active");
    return;
  }

  if (GetMainDisplay() == display) {
    // killing the main display, kill all children displays too!
    for (int i = 1; i < displays.size(); ++i) {
      KillDisplay(std::move(displays.at(i)));
    }
  }

  // find this display in the list and remove it from there
  // if everything went right the smart pointer should delete the display.
  auto this_disp = std::find(displays.begin(), displays.end(), display);
  displays.erase(this_disp);
}

}  // namespace Display
