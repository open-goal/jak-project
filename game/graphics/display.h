#pragma once

/*!
 * @file display.h
 * Display for graphics. This is the game window, distinct from the runtime console.
 */

#ifndef RUNTIME_DISPLAY_H
#define RUNTIME_DISPLAY_H

#include "opengl.h"

namespace Display {

// TODO - eventually we might actually want to support having multiple windows and viewpoints
// so it would be nice if we didn't end up designing this system such that this MUST be
// a single window.
extern GLFWwindow* display;

void InitDisplay(int width, int height, const char* title, GLFWwindow*& d);
void KillDisplay(GLFWwindow*& d);

}  // namespace Display

#endif  // RUNTIME_DISPLAY_H
