#pragma once

/*!
 * @file newpad.h
 * PC-port specific cpad implementation on the C kernel. Monitors button inputs.
 * Actual input detection is done through window events and is gfx pipeline-dependent.
 */

/* NOTE ABOUT KEY VALUES!
 * I am using the renderer-dependent key value macros here (at least for now). This means that the
 * button mapping may be renderer-dependent. When changing renderers, make sure to backup the
 * original button mapping or something so that the user can reset it afterwards. Eventually we
 * should fix this (or maybe it's not even a problem).
 */

#include <unordered_map>

#include "common/common_types.h"

namespace Pad {

static constexpr int CONTROLLER_COUNT = 2;  // support 2 controllers.

enum class Analog {
  Left_X = 0,
  Left_Y,
  Right_X,
  Right_Y,

  Max
};

// mirrors goal enum pad-buttons. used as indices to an array!
enum class Button {
  Select = 0,
  L3 = 1,
  R3 = 2,
  Start = 3,

  Up = 4,
  Right = 5,
  Down = 6,
  Left = 7,

  L2 = 8,
  R2 = 9,
  L1 = 10,
  R1 = 11,

  Triangle = 12,
  Circle = 13,
  X = 14,
  Square = 15,

  Max = 16,

  // aliases
  Ecks = X,
  Cross = X,
  O = Circle
};

// glfw keycode to string mapping
// https://gist.github.com/0xD34DC0DE/910855d41786b962127ae401da2a3441
// Key code from the USB HID Usage Tables v1.12(p. 53-60) but re-arranged to map to 7-bit ASCII for
// printable keys
enum KeyCode : int32_t {
  Unknown = -1,
  Space = 32,
  Apostrophe = 39,
  Comma = 44,
  Minus = 45,
  Period = 46,
  Slash = 47,
  Num0 = 48,
  Num1 = 49,
  Num2 = 50,
  Num3 = 51,
  Num4 = 52,
  Num5 = 53,
  Num6 = 54,
  Num7 = 55,
  Num8 = 56,
  Num9 = 57,
  Semicolon = 59,
  Equal = 61,
  A = 65,
  B = 66,
  C = 67,
  D = 68,
  E = 69,
  F = 70,
  G = 71,
  H = 72,
  I = 73,
  J = 74,
  K = 75,
  L = 76,
  M = 77,
  N = 78,
  O = 79,
  P = 80,
  Q = 81,
  R = 82,
  S = 83,
  T = 84,
  U = 85,
  V = 86,
  W = 87,
  X = 88,
  Y = 89,
  Z = 90,
  LeftBracket = 91,
  Backslash = 92,
  RightBracket = 93,
  GraveAccent = 96,
  World1 = 161,
  World2 = 162,
  Escape = 256,
  Enter = 257,
  Tab = 258,
  Backspace = 259,
  Insert = 260,
  Delete = 261,
  Right = 262,
  Left = 263,
  Down = 264,
  Up = 265,
  PageUp = 266,
  PageDown = 267,
  Home = 268,
  End = 269,
  CapsLock = 280,
  ScrollLock = 281,
  NumLock = 282,
  PrintScreen = 283,
  Pause = 284,
  F1 = 290,
  F2 = 291,
  F3 = 292,
  F4 = 293,
  F5 = 294,
  F6 = 295,
  F7 = 296,
  F8 = 297,
  F9 = 298,
  F10 = 299,
  F11 = 300,
  F12 = 301,
  F13 = 302,
  F14 = 303,
  F15 = 304,
  F16 = 305,
  F17 = 306,
  F18 = 307,
  F19 = 308,
  F20 = 309,
  F21 = 310,
  F22 = 311,
  F23 = 312,
  F24 = 313,
  F25 = 314,
  Keypad0 = 320,
  Keypad1 = 321,
  Keypad2 = 322,
  Keypad3 = 323,
  Keypad4 = 324,
  Keypad5 = 325,
  Keypad6 = 326,
  Keypad7 = 327,
  Keypad8 = 328,
  Keypad9 = 329,
  KeypadDecimal = 330,
  KeypadDivide = 331,
  KeypadMultiply = 332,
  KeypadSubtract = 333,
  KeypadAdd = 334,
  KeypadEnter = 335,
  KeypadEqual = 336,
  LeftShift = 340,
  LeftControl = 341,
  LeftAlt = 342,
  LeftSuper = 343,
  RightShift = 344,
  RightControl = 345,
  RightAlt = 346,
  RightSuper = 347,
  Menu = 348
};

constexpr const char* const LUT44To96[]{
    "COMMA",     "MINUS",   "PERIOD",      "SLASH",     "NUM0",
    "NUM1",      "NUM2",    "NUM3",        "NUM4",      "NUM5",
    "NUM6",      "NUM7",    "NUM8",        "NUM9",      "INVALID",
    "SEMICOLON", "INVALID", "EQUAL",       "INVALID",   "INVALID",
    "INVALID",   "A",       "B",           "C",         "D",
    "E",         "F",       "G",           "H",         "I",
    "J",         "K",       "L",           "M",         "N",
    "O",         "P",       "Q",           "R",         "S",
    "T",         "U",       "V",           "W",         "X",
    "Y",         "Z",       "LEFTBRACKET", "BACKSLASH", "RIGHTBRACKET",
    "INVALID",   "INVALID", "GRAVEACCENT"};

constexpr const char* const LUT256To348[]{"ESCAPE",
                                          "ENTER",
                                          "TAB",
                                          "BACKSPACE",
                                          "INSERT",
                                          "DELETE",
                                          "RIGHT",
                                          "LEFT",
                                          "DOWN",
                                          "UP",
                                          "PAGEUP",
                                          "PAGEDOWN",
                                          "HOME",
                                          "END",
                                          "INVALID",
                                          "INVALID",
                                          "INVALID",
                                          "INVALID",
                                          "INVALID",
                                          "INVALID",
                                          "INVALID",
                                          "INVALID",
                                          "INVALID",
                                          "INVALID",
                                          "CAPSLOCK",
                                          "SCROLLLOCK",
                                          "NUMLOCK",
                                          "PRINTSCREEN",
                                          "PAUSE",
                                          "INVALID",
                                          "INVALID",
                                          "INVALID",
                                          "INVALID",
                                          "INVALID",
                                          "F1",
                                          "F2",
                                          "F3",
                                          "F4",
                                          "F5",
                                          "F6",
                                          "F7",
                                          "F8",
                                          "F9",
                                          "F10",
                                          "F11",
                                          "F12",
                                          "F13",
                                          "F14",
                                          "F15",
                                          "F16",
                                          "F17",
                                          "F18",
                                          "F19",
                                          "F20",
                                          "F21",
                                          "F22",
                                          "F23",
                                          "F24",
                                          "F25",
                                          "INVALID",
                                          "INVALID",
                                          "INVALID",
                                          "INVALID",
                                          "INVALID",
                                          "KEYPAD0",
                                          "KEYPAD1",
                                          "KEYPAD2",
                                          "KEYPAD3",
                                          "KEYPAD4",
                                          "KEYPAD5",
                                          "KEYPAD6",
                                          "KEYPAD7",
                                          "KEYPAD8",
                                          "KEYPAD9",
                                          "KEYPADDECIMAL",
                                          "KEYPADDIVIDE",
                                          "KEYPADMULTIPLY",
                                          "KEYPADSUBTRACT",
                                          "KEYPADADD",
                                          "KEYPADENTER",
                                          "KEYPADEQUAL",
                                          "INVALID",
                                          "INVALID",
                                          "INVALID",
                                          "LEFTSHIFT",
                                          "LEFTCONTROL",
                                          "LEFTALT",
                                          "LEFTSUPER",
                                          "RIGHTSHIFT",
                                          "RIGHTCONTROL",
                                          "RIGHTALT",
                                          "RIGHTSUPER",
                                          "MENU"};

constexpr const char* KeyCodeToString(KeyCode keycode) noexcept {
  if (keycode == 32) {
    return "SPACE";
  }  // common key, don't treat as an unlikely scenario

  if (keycode >= 44 && keycode <= 96)
      [[likely]] { return LUT44To96[keycode - 44]; }

  if (keycode >= 256 && keycode <= 348)
      [[likely]] { return LUT256To348[keycode - 256]; }

  // unlikely scenario where the keycode didn't fall inside one of the two lookup tables
  switch (keycode) {
    case 39:
      return "APOSTROPHE";
    case 161:
      return "WORLD1";
    case 162:
      return "WORLD2";
    default:
      return "UNKNOWN";
  }
}

struct MappingInfo {
  bool debug = true;        // debug mode
  bool buffer_mode = true;  // use buffered inputs

  int joy_pad_mapping[CONTROLLER_COUNT][(int)Pad::Button::Max];
  // int kb_pad_mapping_analog[CONTROLLER_COUNT][(int)Pad::Button::Max];
  int kb_pad_mapping[CONTROLLER_COUNT][(int)Pad::Button::Max];  // controller button mapping
  // TODO complex button mapping & key macros (e.g. shift+x for l2+r2 press etc.)
};

void OnKeyPress(int key, int button = -1);
void OnKeyRelease(int key);
void ForceClearKeys();
void ClearKeys();

void DefaultMapping(MappingInfo& mapping);
int IsPressed(MappingInfo& mapping, Button button, int pad);
int AnalogValue(MappingInfo& mapping, Analog analog, int pad);
void MapButton(MappingInfo& mapping, Button button, int pad, int key);

// this enum is also in pc-pad-utils.gc and progress-pc.gc
enum class InputModeStatus { Disabled, Enabled, Canceled, Progress_KB, Progress_JOY };

extern MappingInfo g_input_mode_mapping;
void EnterInputMode(int mode = -1);
void ExitInputMode(bool);
u64 input_mode_get();
u64 input_mode_get_key();
void input_mode_set_key(s64 key);
u64 input_mode_get_index();
const char* joy_name_get(int pad);
void kb_map_set(int pad, int button);
void joy_map_set(int pad, int button, s64 code);
void keyboard_enable(int enable);
void joy_enable(int enable);
void input_mode_pad_set(s64);
u64 input_mode_get_joy();
void input_mode_set_joy(s64 button);
void joy_map_reset(MappingInfo& mapping);

void initialize();
void read_pad_state();
void update_gamepads();
int rumble(int pad, float slow_motor, float fast_motor);

}  // namespace Pad
