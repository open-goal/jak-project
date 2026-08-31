#pragma once

/*!
 * @file sdl3_compat.h
 * jak-project's game/ code is written against SDL3's API. devkitPro only ships SDL2 for
 * Switch (no SDL3 port exists), and the two APIs differ in real ways beyond naming: SDL3
 * flattened/renamed event fields, added a generic Properties system with no SDL2 equivalent,
 * and changed several return types from int to bool. This header is NOT a general-purpose
 * SDL3-on-SDL2 emulation layer -- it covers exactly the ~69 SDL3 calls game/ actually makes,
 * backed by devkitPro's real SDL2 for everything that maps closely (window/GL context/audio),
 * with a real translation layer only where the shapes genuinely differ (events, properties).
 *
 * Included instead of third-party/SDL/include/SDL3/SDL.h when building for Switch (see the
 * #if defined(__SWITCH__) branches at each of that header's include sites in game/).
 */

// Token-rename trick used for libnx's u128 (see CodeTester.h), for the ONE thing here that's
// genuinely just a type with no linker symbol: SDL_Event. (The functions below that also
// collide with SDL2 can't use this trick -- renaming a *declaration* doesn't rename what the
// prebuilt libSDL2.a actually exports, so the linker ends up looking for a symbol like
// "SDL_Init_real" that doesn't exist anywhere. Those are handled further down by defining a
// same-behavior wrapper under a distinct name -- which calls the real, correctly-linked
// function while its true name is still unclaimed -- and only then #define-redirecting the SDL3
// name to it, so every reference in game/ code (included after this header) resolves to the
// wrapper without ever renaming anything SDL2 itself exports.)
#define SDL_Event SDL_Event_real
#include <SDL2/SDL.h>
#undef SDL_Event

#include <cstdint>
#include <cstring>

// ============================================================================================
// Types SDL3 renamed outright (same shape, different name)
// ============================================================================================
using SDL_GUID = SDL_JoystickGUID;                    // both are `{ Uint8 data[16]; }`
using SDL_Gamepad = SDL_GameController;
using SDL_GamepadButton = SDL_GameControllerButton;
using SDL_GamepadAxis = SDL_GameControllerAxis;
using SDL_GamepadType = SDL_GameControllerType;
using SDL_DisplayID = int;      // SDL2 identifies displays by plain int index
using SDL_PropertiesID = Uint64;

// ============================================================================================
// bool-returning SDL3 functions that were int/SDL_bool-returning in SDL2. SDL2's SDL_bool
// already converts cleanly to bool in `if (!SDL_SetHint(...))`-style checks, so only the
// genuinely int-returning ones (0 == success, not "0 == false") need wrapping.
// ============================================================================================
inline bool sdl3compat_Init(Uint32 flags) {
  return SDL_Init(flags) == 0;
}
inline bool sdl3compat_InitSubSystem(Uint32 flags) {
  return SDL_InitSubSystem(flags) == 0;
}
inline bool SDL_HideCursor() {
  return SDL_ShowCursor(SDL_DISABLE) >= 0;
}
inline bool sdl3compat_ShowCursor() {
  return SDL_ShowCursor(SDL_ENABLE) >= 0;
}
inline bool sdl3compat_GL_MakeCurrent(SDL_Window* window, SDL_GLContext context) {
  return SDL_GL_MakeCurrent(window, context) == 0;
}

// ============================================================================================
// Gamepad/joystick: SDL3 renamed "GameController" -> "Gamepad" and face buttons from
// Xbox-letter names (A/B/X/Y) to layout-agnostic ones (SOUTH/EAST/WEST/NORTH). Values match;
// only the names differ.
// ============================================================================================
#define SDL_INIT_GAMEPAD SDL_INIT_GAMECONTROLLER
#define SDL_OpenGamepad SDL_GameControllerOpen
#define SDL_CloseGamepad SDL_GameControllerClose
#define SDL_GetGamepadJoystick SDL_GameControllerGetJoystick
#define SDL_GetGamepadName SDL_GameControllerName
#define SDL_GetGamepadType SDL_GameControllerGetType
#define SDL_IsGamepad SDL_IsGameController
#define SDL_AddGamepadMappingsFromFile SDL_GameControllerAddMappingsFromFile
#define SDL_GetGamepadStringForButton SDL_GameControllerGetStringForButton
#define SDL_GetGamepadStringForAxis SDL_GameControllerGetStringForAxis
#define SDL_RumbleGamepad SDL_GameControllerRumble
#define SDL_RumbleGamepadTriggers SDL_GameControllerRumbleTriggers
#define SDL_SetGamepadLED SDL_GameControllerSetLED
#define SDL_SendGamepadEffect SDL_GameControllerSendEffect

#define SDL_GAMEPAD_TYPE_PS3 SDL_CONTROLLER_TYPE_PS3
#define SDL_GAMEPAD_TYPE_PS5 SDL_CONTROLLER_TYPE_PS5

#define SDL_GAMEPAD_BUTTON_INVALID SDL_CONTROLLER_BUTTON_INVALID
#define SDL_GAMEPAD_BUTTON_COUNT SDL_CONTROLLER_BUTTON_MAX
#define SDL_GAMEPAD_BUTTON_SOUTH SDL_CONTROLLER_BUTTON_A
#define SDL_GAMEPAD_BUTTON_EAST SDL_CONTROLLER_BUTTON_B
#define SDL_GAMEPAD_BUTTON_WEST SDL_CONTROLLER_BUTTON_X
#define SDL_GAMEPAD_BUTTON_NORTH SDL_CONTROLLER_BUTTON_Y
#define SDL_GAMEPAD_BUTTON_LEFT_SHOULDER SDL_CONTROLLER_BUTTON_LEFTSHOULDER
#define SDL_GAMEPAD_BUTTON_RIGHT_SHOULDER SDL_CONTROLLER_BUTTON_RIGHTSHOULDER
#define SDL_GAMEPAD_BUTTON_DPAD_UP SDL_CONTROLLER_BUTTON_DPAD_UP
#define SDL_GAMEPAD_BUTTON_DPAD_DOWN SDL_CONTROLLER_BUTTON_DPAD_DOWN
#define SDL_GAMEPAD_BUTTON_DPAD_LEFT SDL_CONTROLLER_BUTTON_DPAD_LEFT
#define SDL_GAMEPAD_BUTTON_DPAD_RIGHT SDL_CONTROLLER_BUTTON_DPAD_RIGHT
#define SDL_GAMEPAD_BUTTON_BACK SDL_CONTROLLER_BUTTON_BACK
#define SDL_GAMEPAD_BUTTON_START SDL_CONTROLLER_BUTTON_START
#define SDL_GAMEPAD_BUTTON_LEFT_STICK SDL_CONTROLLER_BUTTON_LEFTSTICK
#define SDL_GAMEPAD_BUTTON_RIGHT_STICK SDL_CONTROLLER_BUTTON_RIGHTSTICK

#define SDL_GAMEPAD_AXIS_INVALID SDL_CONTROLLER_AXIS_INVALID
#define SDL_GAMEPAD_AXIS_COUNT SDL_CONTROLLER_AXIS_MAX
#define SDL_GAMEPAD_AXIS_LEFTX SDL_CONTROLLER_AXIS_LEFTX
#define SDL_GAMEPAD_AXIS_LEFTY SDL_CONTROLLER_AXIS_LEFTY
#define SDL_GAMEPAD_AXIS_RIGHTX SDL_CONTROLLER_AXIS_RIGHTX
#define SDL_GAMEPAD_AXIS_RIGHTY SDL_CONTROLLER_AXIS_RIGHTY
#define SDL_GAMEPAD_AXIS_LEFT_TRIGGER SDL_CONTROLLER_AXIS_TRIGGERLEFT
#define SDL_GAMEPAD_AXIS_RIGHT_TRIGGER SDL_CONTROLLER_AXIS_TRIGGERRIGHT

inline int SDL_GetNumJoystickAxes(SDL_Joystick* joy) {
  return SDL_JoystickNumAxes(joy);
}
inline SDL_JoystickID SDL_GetJoystickID(SDL_Joystick* joy) {
  // SDL3 reserves 0 as "no joystick" and game_controller.cpp rejects a zero instance id, but
  // SDL2 numbers instance ids from 0 -- so the first pad was always refused. Offset by one here
  // and in the event `which` fields below so both sides stay consistent.
  return SDL_JoystickInstanceID(joy) + 1;
}
inline SDL_GUID SDL_GetJoystickGUID(SDL_Joystick* joy) {
  return SDL_JoystickGetGUID(joy);
}
inline void SDL_GUIDToString(SDL_GUID guid, char* dst, int dst_size) {
  SDL_JoystickGetGUIDString(guid, dst, dst_size);
}
inline Sint16 SDL_GetJoystickAxis(SDL_Joystick* joy, int axis) {
  return SDL_JoystickGetAxis(joy, axis);
}

// SDL3 replaced index-based enumeration (SDL_NumJoysticks + linear index) with an ID array.
// game/ only ever iterates this once at startup and opens gamepads by index via
// SDL_OpenGamepad(index) - which, confusingly, SDL3's docs say takes a *joystick instance ID*,
// but jak-project's own call sites pass the loop index from SDL_GetJoysticks either way, and
// SDL_GameControllerOpen also wants an index, so returning plain 0..N-1 indices here (not real
// instance IDs) keeps both call sites internally consistent.
inline SDL_JoystickID* SDL_GetJoysticks(int* count) {
  int n = SDL_NumJoysticks();
  static SDL_JoystickID ids[32];
  n = n > 32 ? 32 : n;
  for (int i = 0; i < n; i++) {
    ids[i] = i;
  }
  if (count) {
    *count = n;
  }
  return ids;
}

// ============================================================================================
// SDL3's Properties system has no SDL2 equivalent at all. game/ only ever queries three known
// boolean capability flags on a gamepad, so fake a PropertiesID as the SDL_Gamepad pointer
// itself and special-case exactly those three string keys against SDL2's individual
// capability-query functions, instead of building a real generic property store.
// ============================================================================================
#define SDL_PROP_GAMEPAD_CAP_RGB_LED_BOOLEAN "sdl3compat.gamepad.cap.rgb_led"
#define SDL_PROP_GAMEPAD_CAP_RUMBLE_BOOLEAN "sdl3compat.gamepad.cap.rumble"
#define SDL_PROP_GAMEPAD_CAP_TRIGGER_RUMBLE_BOOLEAN "sdl3compat.gamepad.cap.trigger_rumble"

inline SDL_PropertiesID SDL_GetGamepadProperties(SDL_Gamepad* gamepad) {
  return reinterpret_cast<SDL_PropertiesID>(gamepad);
}
inline bool SDL_GetBooleanProperty(SDL_PropertiesID props, const char* name, bool default_value) {
  auto* gamepad = reinterpret_cast<SDL_Gamepad*>(props);
  if (!gamepad) {
    return default_value;
  }
  if (std::strcmp(name, SDL_PROP_GAMEPAD_CAP_RGB_LED_BOOLEAN) == 0) {
    return SDL_GameControllerHasLED(gamepad);
  }
  if (std::strcmp(name, SDL_PROP_GAMEPAD_CAP_RUMBLE_BOOLEAN) == 0) {
    return SDL_GameControllerHasRumble(gamepad);
  }
  if (std::strcmp(name, SDL_PROP_GAMEPAD_CAP_TRIGGER_RUMBLE_BOOLEAN) == 0) {
    return SDL_GameControllerHasRumbleTriggers(gamepad);
  }
  return default_value;
}

// ============================================================================================
// Mouse: SDL3 uses float coordinates and a per-window relative-mouse-mode call; SDL2 uses int
// coordinates and a single global relative-mouse-mode call.
// ============================================================================================
inline Uint32 SDL_GetMouseState(float* x, float* y) {
  int ix = 0, iy = 0;
  Uint32 state = SDL_GetMouseState(&ix, &iy);
  if (x) {
    *x = static_cast<float>(ix);
  }
  if (y) {
    *y = static_cast<float>(iy);
  }
  return state;
}
inline Uint32 SDL_GetRelativeMouseState(float* x, float* y) {
  int ix = 0, iy = 0;
  Uint32 state = SDL_GetRelativeMouseState(&ix, &iy);
  if (x) {
    *x = static_cast<float>(ix);
  }
  if (y) {
    *y = static_cast<float>(iy);
  }
  return state;
}
inline bool SDL_SetWindowRelativeMouseMode(SDL_Window* window, bool enabled) {
  (void)window;
  return SDL_SetRelativeMouseMode(enabled ? SDL_TRUE : SDL_FALSE) == 0;
}
#define SDL_BUTTON_MASK SDL_BUTTON
#define SDL_KMOD_SHIFT KMOD_SHIFT
#define SDL_KMOD_CTRL KMOD_CTRL
#define SDL_KMOD_ALT KMOD_ALT
#define SDL_KMOD_GUI KMOD_GUI

// SDL3's keycodes for printable characters are real Unicode codepoints, so shifted/uppercase
// letters are distinct constants from their lowercase SDLK_x (unlike SDL2, which only has
// lowercase). ASCII value == codepoint here, so these can just be the literal characters.
#define SDLK_A 'A'
#define SDLK_D 'D'
#define SDLK_E 'E'
#define SDLK_F 'F'
#define SDLK_I 'I'
#define SDLK_J 'J'
#define SDLK_K 'K'
#define SDLK_L 'L'
#define SDLK_O 'O'
#define SDLK_P 'P'
#define SDLK_Q 'Q'
#define SDLK_R 'R'
#define SDLK_S 'S'
#define SDLK_W 'W'
#define SDLK_APOSTROPHE SDLK_QUOTE

// Hints are just string constants; define any this SDL2 build doesn't already have as harmless
// no-op fallbacks (the corresponding hidapi/background-events behavior they'd toggle either
// isn't relevant on Switch or is already SDL2's default).
#ifndef SDL_HINT_JOYSTICK_HIDAPI_PS3_SIXAXIS_DRIVER
#define SDL_HINT_JOYSTICK_HIDAPI_PS3_SIXAXIS_DRIVER "SDL_JOYSTICK_HIDAPI_PS3_SIXAXIS_DRIVER"
#endif
#ifndef SDL_HINT_JOYSTICK_HIDAPI_PS3
#define SDL_HINT_JOYSTICK_HIDAPI_PS3 "SDL_JOYSTICK_HIDAPI_PS3"
#endif
#ifndef SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS
#define SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS "SDL_JOYSTICK_ALLOW_BACKGROUND_EVENTS"
#endif
#ifndef SDL_HINT_NO_SIGNAL_HANDLERS
#define SDL_HINT_NO_SIGNAL_HANDLERS "SDL_NO_SIGNAL_HANDLERS"
#endif

// SDL2's SDL_GetScancodeFromKey doesn't take a modstate out-param; game/ only ever discards it.
inline SDL_Scancode SDL_GetScancodeFromKey(SDL_Keycode key, SDL_Keymod* /*out_mod*/) {
  return SDL_GetScancodeFromKey(key);
}

// ============================================================================================
// Window / display. Switch has exactly one fixed display, no repositioning, no multi-monitor
// enumeration -- these are honest single-display implementations, not faithful ports of SDL3's
// multi-monitor API.
// ============================================================================================
#define SDL_WINDOW_HIGH_PIXEL_DENSITY 0

inline SDL_Window* SDL_CreateWindow(const char* title, int w, int h, Uint32 flags) {
  return SDL_CreateWindow(title, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, w, h, flags);
}
inline float SDL_GetWindowDisplayScale(SDL_Window* /*window*/) {
  return 1.0f;
}
inline void SDL_GetWindowSizeInPixels(SDL_Window* window, int* w, int* h) {
  SDL_GL_GetDrawableSize(window, w, h);
}
inline bool sdl3compat_SetWindowFullscreen(SDL_Window* window, bool fullscreen) {
  return SDL_SetWindowFullscreen(window, fullscreen ? SDL_WINDOW_FULLSCREEN_DESKTOP : 0) == 0;
}
inline bool SDL_SetWindowFullscreenMode(SDL_Window* /*window*/, const SDL_DisplayMode* /*mode*/) {
  return true;  // Switch has one fixed mode; nothing to switch to.
}
inline bool SDL_SyncWindow(SDL_Window* /*window*/) {
  return true;
}
// SDL3 treats display ID 0 as "invalid"; real IDs start at 1. Returning 0 here made
// display_manager's "could not retrieve current window's sdl display id" path fire and left the
// display list empty. Every display function below ignores the ID anyway -- there is one display.
inline SDL_DisplayID SDL_GetDisplayForWindow(SDL_Window* /*window*/) {
  return 1;
}
inline SDL_DisplayID* SDL_GetDisplays(int* count) {
  static SDL_DisplayID id = 1;
  if (count) {
    *count = 1;
  }
  return &id;
}
// Query SDL2 for the real display size rather than hardcoding. Citron reports a 1920x1080
// drawable in docked mode and 1280x720 in handheld; hardcoding one of them made the display
// manager's resolution list disagree with the actual framebuffer, so the game sized its render
// target for a display it wasn't drawing to. SDL2's overload takes (int, SDL_DisplayMode*), so
// it resolves separately from these SDL3-shaped shims.
inline const SDL_DisplayMode* sdl3compat_RealDisplayMode() {
  static SDL_DisplayMode mode{};
  static bool init = false;
  if (!init) {
    init = true;
    SDL_DisplayMode real{};
    if (SDL_GetDesktopDisplayMode(0, &real) == 0 && real.w > 0 && real.h > 0) {
      mode = real;
    } else {
      mode.w = 1280;
      mode.h = 720;
    }
    if (mode.refresh_rate <= 0) {
      mode.refresh_rate = 60;
    }
  }
  return &mode;
}
inline bool sdl3compat_GetDisplayBounds(SDL_DisplayID /*display*/, SDL_Rect* rect) {
  if (rect) {
    const auto* m = sdl3compat_RealDisplayMode();
    *rect = SDL_Rect{0, 0, m->w, m->h};
  }
  return true;
}
inline const SDL_DisplayMode* SDL_GetCurrentDisplayMode(SDL_DisplayID /*display*/) {
  return sdl3compat_RealDisplayMode();
}
inline const SDL_DisplayMode* SDL_GetDesktopDisplayMode(SDL_DisplayID display) {
  return SDL_GetCurrentDisplayMode(display);
}
inline SDL_DisplayMode** SDL_GetFullscreenDisplayModes(SDL_DisplayID /*display*/, int* count) {
  // An empty list left display_manager with no supported resolutions, so the window size
  // resolved to 0,0. Report the one mode the display actually presents; the refresh rate has to
  // match SDL_GetCurrentDisplayMode's or display_manager files it as windowed-only.
  static SDL_DisplayMode* modes[] = {const_cast<SDL_DisplayMode*>(sdl3compat_RealDisplayMode()),
                                     nullptr};
  if (count) {
    *count = 1;
  }
  return modes;
}
inline const char* SDL_GetDisplayName(SDL_DisplayID /*display*/) {
  return "Switch";
}

// SDL_DisplayOrientation and SDL_ORIENTATION_* already exist in SDL2 (added 2.0.9) with the
// same names SDL3 uses -- no redeclaration needed.
inline SDL_DisplayOrientation SDL_GetCurrentDisplayOrientation(SDL_DisplayID /*display*/) {
  return SDL_ORIENTATION_LANDSCAPE;
}

inline SDL_Surface* SDL_CreateSurfaceFrom(int width,
                                          int height,
                                          Uint32 format,
                                          void* pixels,
                                          int pitch) {
  int bpp;
  Uint32 rmask, gmask, bmask, amask;
  SDL_PixelFormatEnumToMasks(format, &bpp, &rmask, &gmask, &bmask, &amask);
  return SDL_CreateRGBSurfaceFrom(pixels, width, height, bpp, pitch, rmask, gmask, bmask, amask);
}
inline void SDL_DestroySurface(SDL_Surface* surface) {
  SDL_FreeSurface(surface);
}

inline void SDL_GL_DestroyContext(SDL_GLContext ctx) {
  SDL_GL_DeleteContext(ctx);
}
inline bool sdl3compat_SetWindowPosition(SDL_Window* window, int x, int y) {
  SDL_SetWindowPosition(window, x, y);
  return true;
}
inline bool sdl3compat_SetWindowSize(SDL_Window* window, int w, int h) {
  SDL_SetWindowSize(window, w, h);
  return true;
}

// ============================================================================================
// Version query: only used for a debug print. Encode as SDL3's (major*1000000 + minor*1000 +
// patch) scheme so the printed number at least looks sane.
// ============================================================================================
#define SDL_VERSION (SDL_MAJOR_VERSION * 1000000 + SDL_MINOR_VERSION * 1000 + SDL_PATCHLEVEL)
inline int SDL_GetVersion() {
  SDL_version v;
  SDL_GetVersion(&v);
  return v.major * 1000000 + v.minor * 1000 + v.patch;
}

// ============================================================================================
// Events. SDL3 flattened event field layout (event.key.key vs SDL2's event.key.keysym.sym) and
// renamed the controller union members (gaxis/gbutton vs SDL2's caxis/cbutton) and split
// SDL_WINDOWEVENT's sub-events into distinct top-level types. This is a real, purpose-built
// translation, not an alias -- SDL_PollEvent below pulls a real SDL2 event and converts it.
// ============================================================================================
enum {
  SDL_EVENT_QUIT = 0x40000000,
  SDL_EVENT_KEY_DOWN,
  SDL_EVENT_KEY_UP,
  SDL_EVENT_MOUSE_MOTION,
  SDL_EVENT_MOUSE_BUTTON_DOWN,
  SDL_EVENT_MOUSE_BUTTON_UP,
  SDL_EVENT_GAMEPAD_ADDED,
  SDL_EVENT_GAMEPAD_REMOVED,
  SDL_EVENT_GAMEPAD_AXIS_MOTION,
  SDL_EVENT_GAMEPAD_BUTTON_DOWN,
  SDL_EVENT_GAMEPAD_BUTTON_UP,
  SDL_EVENT_JOYSTICK_AXIS_MOTION,
  SDL_EVENT_WINDOW_FIRST,
  SDL_EVENT_WINDOW_MINIMIZED = SDL_EVENT_WINDOW_FIRST,
  SDL_EVENT_WINDOW_MAXIMIZED,
  SDL_EVENT_WINDOW_RESTORED,
  SDL_EVENT_WINDOW_MOVED,
  SDL_EVENT_WINDOW_RESIZED,
  SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED,
  SDL_EVENT_WINDOW_DISPLAY_CHANGED,
  SDL_EVENT_WINDOW_MOUSE_ENTER,
  SDL_EVENT_WINDOW_MOUSE_LEAVE,
  SDL_EVENT_WINDOW_LAST = SDL_EVENT_WINDOW_MOUSE_LEAVE,
  SDL_EVENT_DISPLAY_FIRST,
  SDL_EVENT_DISPLAY_ADDED = SDL_EVENT_DISPLAY_FIRST,
  SDL_EVENT_DISPLAY_REMOVED,
  SDL_EVENT_DISPLAY_ORIENTATION,
  SDL_EVENT_DISPLAY_LAST = SDL_EVENT_DISPLAY_ORIENTATION,
};

struct SDL_KeyboardEventCompat {
  Uint32 type;
  Uint32 windowID;
  SDL_Keycode key;
  SDL_Scancode scancode;
  Uint16 mod;
  Uint8 repeat;
};
struct SDL_MouseMotionEventCompat {
  Uint32 type;
  Uint32 windowID;
  Uint32 state;
  float x, y, xrel, yrel;
};
struct SDL_MouseButtonEventCompat {
  Uint32 type;
  Uint32 windowID;
  Uint8 button;
  Uint8 clicks;
  float x, y;
};
struct SDL_GamepadAxisEventCompat {
  Uint32 type;
  SDL_JoystickID which;
  Uint8 axis;
  Sint16 value;
};
struct SDL_GamepadButtonEventCompat {
  Uint32 type;
  SDL_JoystickID which;
  Uint8 button;
};
struct SDL_JoyAxisEventCompat {
  Uint32 type;
  SDL_JoystickID which;
  Uint8 axis;
  Sint16 value;
};
struct SDL_GamepadDeviceEventCompat {
  Uint32 type;
  SDL_JoystickID which;
};
struct SDL_WindowEventCompat {
  Uint32 type;
  Uint32 windowID;
  Sint32 data1, data2;
};
struct SDL_DisplayEventCompat {
  Uint32 type;
  SDL_DisplayID displayID;
  Sint32 data1;
};

union SDL_Event {
  Uint32 type;
  SDL_KeyboardEventCompat key;
  SDL_MouseMotionEventCompat motion;
  SDL_MouseButtonEventCompat button;
  SDL_GamepadAxisEventCompat gaxis;
  SDL_GamepadButtonEventCompat gbutton;
  SDL_JoyAxisEventCompat jaxis;
  SDL_GamepadDeviceEventCompat gdevice;
  SDL_WindowEventCompat window;
  SDL_DisplayEventCompat display;
};

inline int sdl3compat_PollEvent(SDL_Event* out) {
  SDL_Event_real real_evt;
  while (SDL_PollEvent(&real_evt)) {
    switch (real_evt.type) {
      case SDL_QUIT:
        out->type = SDL_EVENT_QUIT;
        return 1;
      case SDL_KEYDOWN:
      case SDL_KEYUP:
        out->key.type = real_evt.type == SDL_KEYDOWN ? SDL_EVENT_KEY_DOWN : SDL_EVENT_KEY_UP;
        out->key.windowID = real_evt.key.windowID;
        out->key.key = real_evt.key.keysym.sym;
        out->key.scancode = real_evt.key.keysym.scancode;
        out->key.mod = real_evt.key.keysym.mod;
        out->key.repeat = real_evt.key.repeat;
        return 1;
      case SDL_MOUSEMOTION:
        out->motion.type = SDL_EVENT_MOUSE_MOTION;
        out->motion.windowID = real_evt.motion.windowID;
        out->motion.state = real_evt.motion.state;
        out->motion.x = static_cast<float>(real_evt.motion.x);
        out->motion.y = static_cast<float>(real_evt.motion.y);
        out->motion.xrel = static_cast<float>(real_evt.motion.xrel);
        out->motion.yrel = static_cast<float>(real_evt.motion.yrel);
        return 1;
      case SDL_MOUSEBUTTONDOWN:
      case SDL_MOUSEBUTTONUP:
        out->button.type =
            real_evt.type == SDL_MOUSEBUTTONDOWN ? SDL_EVENT_MOUSE_BUTTON_DOWN : SDL_EVENT_MOUSE_BUTTON_UP;
        out->button.windowID = real_evt.button.windowID;
        out->button.button = real_evt.button.button;
        out->button.clicks = real_evt.button.clicks;
        out->button.x = static_cast<float>(real_evt.button.x);
        out->button.y = static_cast<float>(real_evt.button.y);
        return 1;
      case SDL_CONTROLLERDEVICEADDED:
      case SDL_CONTROLLERDEVICEREMOVED:
        out->gdevice.type = real_evt.type == SDL_CONTROLLERDEVICEADDED ? SDL_EVENT_GAMEPAD_ADDED
                                                                       : SDL_EVENT_GAMEPAD_REMOVED;
        out->gdevice.which = real_evt.cdevice.which;
        return 1;
      case SDL_CONTROLLERAXISMOTION:
        out->gaxis.type = SDL_EVENT_GAMEPAD_AXIS_MOTION;
        out->gaxis.which = real_evt.caxis.which + 1;
        out->gaxis.axis = real_evt.caxis.axis;
        out->gaxis.value = real_evt.caxis.value;
        return 1;
      case SDL_CONTROLLERBUTTONDOWN:
      case SDL_CONTROLLERBUTTONUP:
        out->gbutton.type = real_evt.type == SDL_CONTROLLERBUTTONDOWN ? SDL_EVENT_GAMEPAD_BUTTON_DOWN
                                                                      : SDL_EVENT_GAMEPAD_BUTTON_UP;
        out->gbutton.which = real_evt.cbutton.which + 1;
        out->gbutton.button = real_evt.cbutton.button;
        return 1;
      case SDL_JOYAXISMOTION:
        out->jaxis.type = SDL_EVENT_JOYSTICK_AXIS_MOTION;
        out->jaxis.which = real_evt.jaxis.which + 1;
        out->jaxis.axis = real_evt.jaxis.axis;
        out->jaxis.value = real_evt.jaxis.value;
        return 1;
      case SDL_WINDOWEVENT: {
        Uint32 mapped;
        switch (real_evt.window.event) {
          case SDL_WINDOWEVENT_MINIMIZED:
            mapped = SDL_EVENT_WINDOW_MINIMIZED;
            break;
          case SDL_WINDOWEVENT_MAXIMIZED:
            mapped = SDL_EVENT_WINDOW_MAXIMIZED;
            break;
          case SDL_WINDOWEVENT_RESTORED:
            mapped = SDL_EVENT_WINDOW_RESTORED;
            break;
          case SDL_WINDOWEVENT_MOVED:
            mapped = SDL_EVENT_WINDOW_MOVED;
            break;
          case SDL_WINDOWEVENT_RESIZED:
            mapped = SDL_EVENT_WINDOW_RESIZED;
            break;
          case SDL_WINDOWEVENT_SIZE_CHANGED:
            mapped = SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED;
            break;
          case SDL_WINDOWEVENT_ENTER:
            mapped = SDL_EVENT_WINDOW_MOUSE_ENTER;
            break;
          case SDL_WINDOWEVENT_LEAVE:
            mapped = SDL_EVENT_WINDOW_MOUSE_LEAVE;
            break;
          default:
            continue;  // not one we translate; skip to the next real event
        }
        out->window.type = mapped;
        out->window.windowID = real_evt.window.windowID;
        out->window.data1 = real_evt.window.data1;
        out->window.data2 = real_evt.window.data2;
        return 1;
      }
      case SDL_DISPLAYEVENT: {
        Uint32 mapped;
        switch (real_evt.display.event) {
          case SDL_DISPLAYEVENT_CONNECTED:
            mapped = SDL_EVENT_DISPLAY_ADDED;
            break;
          case SDL_DISPLAYEVENT_DISCONNECTED:
            mapped = SDL_EVENT_DISPLAY_REMOVED;
            break;
          case SDL_DISPLAYEVENT_ORIENTATION:
            mapped = SDL_EVENT_DISPLAY_ORIENTATION;
            break;
          default:
            continue;  // not one we translate; skip to the next real event
        }
        out->display.type = mapped;
        out->display.displayID = real_evt.display.display;
        out->display.data1 = real_evt.display.data1;
        return 1;
      }
      default:
        continue;  // not one we translate; skip to the next real event
    }
  }
  return 0;
}

// Redirect these 9 SDL3 names to the wrappers above, for everything textually after this point
// (i.e. the game/ source file that included this header) -- deliberately placed at the very end
// so nothing earlier in this header (which needs the real, unrenamed SDL2 functions) is affected.
#define SDL_Init sdl3compat_Init
#define SDL_InitSubSystem sdl3compat_InitSubSystem
#define SDL_ShowCursor sdl3compat_ShowCursor
#define SDL_PollEvent sdl3compat_PollEvent
#define SDL_SetWindowFullscreen sdl3compat_SetWindowFullscreen
#define SDL_GetDisplayBounds sdl3compat_GetDisplayBounds
#define SDL_SetWindowPosition sdl3compat_SetWindowPosition
#define SDL_SetWindowSize sdl3compat_SetWindowSize
#define SDL_GL_MakeCurrent sdl3compat_GL_MakeCurrent
