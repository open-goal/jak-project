#pragma once

/*!
 * @file gfx.h
 * Graphics component for the runtime. Handles some low-level routines.
 */

#ifndef RUNTIME_GFX_H
#define RUNTIME_GFX_H

#include "common/common_types.h"
#include "display.h"
#include "game/kernel/kboot.h"

namespace Gfx {

u32 Init();
u32 Exit();

template <typename T>
void Loop(T f) {
  while (f()) {
    // run display-specific things
    if (Display::display) {
      // lg::debug("run display");
      glfwMakeContextCurrent(Display::display);

      // render graphics
      glClear(GL_COLOR_BUFFER_BIT);

      glfwSwapBuffers(Display::display);

      // poll events TODO integrate input with cpad
      glfwPollEvents();

      // exit if display window was closed
      if (glfwWindowShouldClose(Display::display)) {
        // Display::KillDisplay(Display::display);
        MasterExit = 1;
      }
    }
  }
}

}  // namespace Gfx

#endif  // RUNTIME_GFX_H
