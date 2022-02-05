//========================================================================
// GLFW 3.4 - www.glfw.org
//------------------------------------------------------------------------
// Copyright (c) 2016 Google Inc.
// Copyright (c) 2016-2019 Camilla Löwy <elmindreda@glfw.org>
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
//    claim that you wrote the original software. If you use this software
//    in a product, an acknowledgment in the product documentation would
//    be appreciated but is not required.
//
// 2. Altered source versions must be plainly marked as such, and must not
//    be misrepresented as being the original software.
//
// 3. This notice may not be removed or altered from any source
//    distribution.
//
//========================================================================
// It is fine to use C99 in this file because it will not be built with VS
//========================================================================

#include "internal.h"

#include <stdlib.h>

static void applySizeLimits(_GLFWwindow* window, int* width, int* height)
{
    if (window->numer != GLFW_DONT_CARE && window->denom != GLFW_DONT_CARE)
    {
        const float ratio = (float) window->numer / (float) window->denom;
        *height = (int) (*width / ratio);
    }

    if (window->minwidth != GLFW_DONT_CARE && *width < window->minwidth)
        *width = window->minwidth;
    else if (window->maxwidth != GLFW_DONT_CARE && *width > window->maxwidth)
        *width = window->maxwidth;

    if (window->minheight != GLFW_DONT_CARE && *height < window->minheight)
        *height = window->minheight;
    else if (window->maxheight != GLFW_DONT_CARE && *height > window->maxheight)
        *height = window->maxheight;
}

static void fitToMonitor(_GLFWwindow* window)
{
    GLFWvidmode mode;
    _glfwGetVideoModeNull(window->monitor, &mode);
    _glfwGetMonitorPosNull(window->monitor,
                           &window->null.xpos,
                           &window->null.ypos);
    window->null.width = mode.width;
    window->null.height = mode.height;
}

static void acquireMonitor(_GLFWwindow* window)
{
    _glfwInputMonitorWindow(window->monitor, window);
}

static void releaseMonitor(_GLFWwindow* window)
{
    if (window->monitor->window != window)
        return;

    _glfwInputMonitorWindow(window->monitor, NULL);
}

static int createNativeWindow(_GLFWwindow* window,
                              const _GLFWwndconfig* wndconfig,
                              const _GLFWfbconfig* fbconfig)
{
    if (window->monitor)
        fitToMonitor(window);
    else
    {
        window->null.xpos = 17;
        window->null.ypos = 17;
        window->null.width = wndconfig->width;
        window->null.height = wndconfig->height;
    }

    window->null.visible = wndconfig->visible;
    window->null.decorated = wndconfig->decorated;
    window->null.maximized = wndconfig->maximized;
    window->null.floating = wndconfig->floating;
    window->null.transparent = fbconfig->transparent;
    window->null.opacity = 1.f;

    return GLFW_TRUE;
}


//////////////////////////////////////////////////////////////////////////
//////                       GLFW platform API                      //////
//////////////////////////////////////////////////////////////////////////

int _glfwCreateWindowNull(_GLFWwindow* window,
                          const _GLFWwndconfig* wndconfig,
                          const _GLFWctxconfig* ctxconfig,
                          const _GLFWfbconfig* fbconfig)
{
    if (!createNativeWindow(window, wndconfig, fbconfig))
        return GLFW_FALSE;

    if (ctxconfig->client != GLFW_NO_API)
    {
        if (ctxconfig->source == GLFW_NATIVE_CONTEXT_API ||
            ctxconfig->source == GLFW_OSMESA_CONTEXT_API)
        {
            if (!_glfwInitOSMesa())
                return GLFW_FALSE;
            if (!_glfwCreateContextOSMesa(window, ctxconfig, fbconfig))
                return GLFW_FALSE;
        }
        else if (ctxconfig->source == GLFW_EGL_CONTEXT_API)
        {
            if (!_glfwInitEGL())
                return GLFW_FALSE;
            if (!_glfwCreateContextEGL(window, ctxconfig, fbconfig))
                return GLFW_FALSE;
        }
    }

    if (window->monitor)
    {
        _glfwShowWindowNull(window);
        _glfwFocusWindowNull(window);
        acquireMonitor(window);
    }

    return GLFW_TRUE;
}

void _glfwDestroyWindowNull(_GLFWwindow* window)
{
    if (window->monitor)
        releaseMonitor(window);

    if (_glfw.null.focusedWindow == window)
        _glfw.null.focusedWindow = NULL;

    if (window->context.destroy)
        window->context.destroy(window);
}

void _glfwSetWindowTitleNull(_GLFWwindow* window, const char* title)
{
}

void _glfwSetWindowIconNull(_GLFWwindow* window, int count, const GLFWimage* images)
{
}

void _glfwSetWindowMonitorNull(_GLFWwindow* window,
                               _GLFWmonitor* monitor,
                               int xpos, int ypos,
                               int width, int height,
                               int refreshRate)
{
    if (window->monitor == monitor)
    {
        if (!monitor)
        {
            _glfwSetWindowPosNull(window, xpos, ypos);
            _glfwSetWindowSizeNull(window, width, height);
        }

        return;
    }

    if (window->monitor)
        releaseMonitor(window);

    _glfwInputWindowMonitor(window, monitor);

    if (window->monitor)
    {
        window->null.visible = GLFW_TRUE;
        acquireMonitor(window);
        fitToMonitor(window);
    }
    else
    {
        _glfwSetWindowPosNull(window, xpos, ypos);
        _glfwSetWindowSizeNull(window, width, height);
    }
}

void _glfwGetWindowPosNull(_GLFWwindow* window, int* xpos, int* ypos)
{
    if (xpos)
        *xpos = window->null.xpos;
    if (ypos)
        *ypos = window->null.ypos;
}

void _glfwSetWindowPosNull(_GLFWwindow* window, int xpos, int ypos)
{
    if (window->monitor)
        return;

    if (window->null.xpos != xpos || window->null.ypos != ypos)
    {
        window->null.xpos = xpos;
        window->null.ypos = ypos;
        _glfwInputWindowPos(window, xpos, ypos);
    }
}

void _glfwGetWindowSizeNull(_GLFWwindow* window, int* width, int* height)
{
    if (width)
        *width = window->null.width;
    if (height)
        *height = window->null.height;
}

void _glfwSetWindowSizeNull(_GLFWwindow* window, int width, int height)
{
    if (window->monitor)
        return;

    if (window->null.width != width || window->null.height != height)
    {
        window->null.width = width;
        window->null.height = height;
        _glfwInputWindowSize(window, width, height);
        _glfwInputFramebufferSize(window, width, height);
    }
}

void _glfwSetWindowSizeLimitsNull(_GLFWwindow* window,
                                  int minwidth, int minheight,
                                  int maxwidth, int maxheight)
{
    int width = window->null.width;
    int height = window->null.height;
    applySizeLimits(window, &width, &height);
    _glfwSetWindowSizeNull(window, width, height);
}

void _glfwSetWindowAspectRatioNull(_GLFWwindow* window, int n, int d)
{
    int width = window->null.width;
    int height = window->null.height;
    applySizeLimits(window, &width, &height);
    _glfwSetWindowSizeNull(window, width, height);
}

void _glfwGetFramebufferSizeNull(_GLFWwindow* window, int* width, int* height)
{
    if (width)
        *width = window->null.width;
    if (height)
        *height = window->null.height;
}

void _glfwGetWindowFrameSizeNull(_GLFWwindow* window,
                                 int* left, int* top,
                                 int* right, int* bottom)
{
    if (window->null.decorated && !window->monitor)
    {
        if (left)
            *left = 1;
        if (top)
            *top = 10;
        if (right)
            *right = 1;
        if (bottom)
            *bottom = 1;
    }
    else
    {
        if (left)
            *left = 0;
        if (top)
            *top = 0;
        if (right)
            *right = 0;
        if (bottom)
            *bottom = 0;
    }
}

void _glfwGetWindowContentScaleNull(_GLFWwindow* window, float* xscale, float* yscale)
{
    if (xscale)
        *xscale = 1.f;
    if (yscale)
        *yscale = 1.f;
}

void _glfwIconifyWindowNull(_GLFWwindow* window)
{
    if (_glfw.null.focusedWindow == window)
    {
        _glfw.null.focusedWindow = NULL;
        _glfwInputWindowFocus(window, GLFW_FALSE);
    }

    if (!window->null.iconified)
    {
        window->null.iconified = GLFW_TRUE;
        _glfwInputWindowIconify(window, GLFW_TRUE);

        if (window->monitor)
            releaseMonitor(window);
    }
}

void _glfwRestoreWindowNull(_GLFWwindow* window)
{
    if (window->null.iconified)
    {
        window->null.iconified = GLFW_FALSE;
        _glfwInputWindowIconify(window, GLFW_FALSE);

        if (window->monitor)
            acquireMonitor(window);
    }
    else if (window->null.maximized)
    {
        window->null.maximized = GLFW_FALSE;
        _glfwInputWindowMaximize(window, GLFW_FALSE);
    }
}

void _glfwMaximizeWindowNull(_GLFWwindow* window)
{
    if (!window->null.maximized)
    {
        window->null.maximized = GLFW_TRUE;
        _glfwInputWindowMaximize(window, GLFW_TRUE);
    }
}

int _glfwWindowMaximizedNull(_GLFWwindow* window)
{
    return window->null.maximized;
}

int _glfwWindowHoveredNull(_GLFWwindow* window)
{
    return _glfw.null.xcursor >= window->null.xpos &&
           _glfw.null.ycursor >= window->null.ypos &&
           _glfw.null.xcursor <= window->null.xpos + window->null.width - 1 &&
           _glfw.null.ycursor <= window->null.ypos + window->null.height - 1;
}

int _glfwFramebufferTransparentNull(_GLFWwindow* window)
{
    return window->null.transparent;
}

void _glfwSetWindowResizableNull(_GLFWwindow* window, GLFWbool enabled)
{
    window->null.resizable = enabled;
}

void _glfwSetWindowDecoratedNull(_GLFWwindow* window, GLFWbool enabled)
{
    window->null.decorated = enabled;
}

void _glfwSetWindowFloatingNull(_GLFWwindow* window, GLFWbool enabled)
{
    window->null.floating = enabled;
}

void _glfwSetWindowMousePassthroughNull(_GLFWwindow* window, GLFWbool enabled)
{
}

float _glfwGetWindowOpacityNull(_GLFWwindow* window)
{
    return window->null.opacity;
}

void _glfwSetWindowOpacityNull(_GLFWwindow* window, float opacity)
{
    window->null.opacity = opacity;
}

void _glfwSetRawMouseMotionNull(_GLFWwindow *window, GLFWbool enabled)
{
}

GLFWbool _glfwRawMouseMotionSupportedNull(void)
{
    return GLFW_TRUE;
}

void _glfwShowWindowNull(_GLFWwindow* window)
{
    window->null.visible = GLFW_TRUE;
}

void _glfwRequestWindowAttentionNull(_GLFWwindow* window)
{
}

void _glfwHideWindowNull(_GLFWwindow* window)
{
    if (_glfw.null.focusedWindow == window)
    {
        _glfw.null.focusedWindow = NULL;
        _glfwInputWindowFocus(window, GLFW_FALSE);
    }

    window->null.visible = GLFW_FALSE;
}

void _glfwFocusWindowNull(_GLFWwindow* window)
{
    _GLFWwindow* previous;

    if (_glfw.null.focusedWindow == window)
        return;

    if (!window->null.visible)
        return;

    previous = _glfw.null.focusedWindow;
    _glfw.null.focusedWindow = window;

    if (previous)
    {
        _glfwInputWindowFocus(previous, GLFW_FALSE);
        if (previous->monitor && previous->autoIconify)
            _glfwIconifyWindowNull(previous);
    }

    _glfwInputWindowFocus(window, GLFW_TRUE);
}

int _glfwWindowFocusedNull(_GLFWwindow* window)
{
    return _glfw.null.focusedWindow == window;
}

int _glfwWindowIconifiedNull(_GLFWwindow* window)
{
    return window->null.iconified;
}

int _glfwWindowVisibleNull(_GLFWwindow* window)
{
    return window->null.visible;
}

void _glfwPollEventsNull(void)
{
}

void _glfwWaitEventsNull(void)
{
}

void _glfwWaitEventsTimeoutNull(double timeout)
{
}

void _glfwPostEmptyEventNull(void)
{
}

void _glfwGetCursorPosNull(_GLFWwindow* window, double* xpos, double* ypos)
{
    if (xpos)
        *xpos = _glfw.null.xcursor - window->null.xpos;
    if (ypos)
        *ypos = _glfw.null.ycursor - window->null.ypos;
}

void _glfwSetCursorPosNull(_GLFWwindow* window, double x, double y)
{
    _glfw.null.xcursor = window->null.xpos + (int) x;
    _glfw.null.ycursor = window->null.ypos + (int) y;
}

void _glfwSetCursorModeNull(_GLFWwindow* window, int mode)
{
}

int _glfwCreateCursorNull(_GLFWcursor* cursor,
                          const GLFWimage* image,
                          int xhot, int yhot)
{
    return GLFW_TRUE;
}

int _glfwCreateStandardCursorNull(_GLFWcursor* cursor, int shape)
{
    return GLFW_TRUE;
}

void _glfwDestroyCursorNull(_GLFWcursor* cursor)
{
}

void _glfwSetCursorNull(_GLFWwindow* window, _GLFWcursor* cursor)
{
}

void _glfwSetClipboardStringNull(const char* string)
{
    char* copy = _glfw_strdup(string);
    _glfw_free(_glfw.null.clipboardString);
    _glfw.null.clipboardString = copy;
}

const char* _glfwGetClipboardStringNull(void)
{
    return _glfw.null.clipboardString;
}

EGLenum _glfwGetEGLPlatformNull(EGLint** attribs)
{
    return 0;
}

EGLNativeDisplayType _glfwGetEGLNativeDisplayNull(void)
{
    return 0;
}

EGLNativeWindowType _glfwGetEGLNativeWindowNull(_GLFWwindow* window)
{
    return 0;
}

const char* _glfwGetScancodeNameNull(int scancode)
{
    if (scancode < GLFW_KEY_SPACE || scancode > GLFW_KEY_LAST)
    {
        _glfwInputError(GLFW_INVALID_VALUE, "Invalid scancode %i", scancode);
        return NULL;
    }

    switch (scancode)
    {
        case GLFW_KEY_APOSTROPHE:
            return "'";
        case GLFW_KEY_COMMA:
            return ",";
        case GLFW_KEY_MINUS:
        case GLFW_KEY_KP_SUBTRACT:
            return "-";
        case GLFW_KEY_PERIOD:
        case GLFW_KEY_KP_DECIMAL:
            return ".";
        case GLFW_KEY_SLASH:
        case GLFW_KEY_KP_DIVIDE:
            return "/";
        case GLFW_KEY_SEMICOLON:
            return ";";
        case GLFW_KEY_EQUAL:
        case GLFW_KEY_KP_EQUAL:
            return "=";
        case GLFW_KEY_LEFT_BRACKET:
            return "[";
        case GLFW_KEY_RIGHT_BRACKET:
            return "]";
        case GLFW_KEY_KP_MULTIPLY:
            return "*";
        case GLFW_KEY_KP_ADD:
            return "+";
        case GLFW_KEY_BACKSLASH:
        case GLFW_KEY_WORLD_1:
        case GLFW_KEY_WORLD_2:
            return "\\";
        case GLFW_KEY_0:
        case GLFW_KEY_KP_0:
            return "0";
        case GLFW_KEY_1:
        case GLFW_KEY_KP_1:
            return "1";
        case GLFW_KEY_2:
        case GLFW_KEY_KP_2:
            return "2";
        case GLFW_KEY_3:
        case GLFW_KEY_KP_3:
            return "3";
        case GLFW_KEY_4:
        case GLFW_KEY_KP_4:
            return "4";
        case GLFW_KEY_5:
        case GLFW_KEY_KP_5:
            return "5";
        case GLFW_KEY_6:
        case GLFW_KEY_KP_6:
            return "6";
        case GLFW_KEY_7:
        case GLFW_KEY_KP_7:
            return "7";
        case GLFW_KEY_8:
        case GLFW_KEY_KP_8:
            return "8";
        case GLFW_KEY_9:
        case GLFW_KEY_KP_9:
            return "9";
        case GLFW_KEY_A:
            return "a";
        case GLFW_KEY_B:
            return "b";
        case GLFW_KEY_C:
            return "c";
        case GLFW_KEY_D:
            return "d";
        case GLFW_KEY_E:
            return "e";
        case GLFW_KEY_F:
            return "f";
        case GLFW_KEY_G:
            return "g";
        case GLFW_KEY_H:
            return "h";
        case GLFW_KEY_I:
            return "i";
        case GLFW_KEY_J:
            return "j";
        case GLFW_KEY_K:
            return "k";
        case GLFW_KEY_L:
            return "l";
        case GLFW_KEY_M:
            return "m";
        case GLFW_KEY_N:
            return "n";
        case GLFW_KEY_O:
            return "o";
        case GLFW_KEY_P:
            return "p";
        case GLFW_KEY_Q:
            return "q";
        case GLFW_KEY_R:
            return "r";
        case GLFW_KEY_S:
            return "s";
        case GLFW_KEY_T:
            return "t";
        case GLFW_KEY_U:
            return "u";
        case GLFW_KEY_V:
            return "v";
        case GLFW_KEY_W:
            return "w";
        case GLFW_KEY_X:
            return "x";
        case GLFW_KEY_Y:
            return "y";
        case GLFW_KEY_Z:
            return "z";
    }

    return NULL;
}

int _glfwGetKeyScancodeNull(int key)
{
    return key;
}

void _glfwGetRequiredInstanceExtensionsNull(char** extensions)
{
}

int _glfwGetPhysicalDevicePresentationSupportNull(VkInstance instance,
                                                  VkPhysicalDevice device,
                                                  uint32_t queuefamily)
{
    return GLFW_FALSE;
}

VkResult _glfwCreateWindowSurfaceNull(VkInstance instance,
                                      _GLFWwindow* window,
                                      const VkAllocationCallbacks* allocator,
                                      VkSurfaceKHR* surface)
{
    // This seems like the most appropriate error to return here
    return VK_ERROR_EXTENSION_NOT_PRESENT;
}

