#pragma once

/*!
 * @file opengl.h
 * OpenGL includes.
 */

#define GLFW_INCLUDE_NONE
#include "third-party/glad/include/glad/glad.h"
#include "third-party/glfw/include/GLFW/glfw3.h"

#include "game/graphics/gfx.h"

enum GlfwKeyAction {
  Release = GLFW_RELEASE,  // falling edge of key press
  Press = GLFW_PRESS,      // rising edge of key press
  Repeat = GLFW_REPEAT     // repeated input on hold e.g. when typing something
};

extern const GfxRendererModule moduleOpenGL;
