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

static constexpr int CONTROLLER_COUNT = 4;  // support 4 controllers.

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

enum class AnalogMappingMode { DigitalInput = 0, AnalogInput = 1 };

// AnalogMappingInfo allows either button or axes to control analog input(s).
//  * In Digital Input Mode, uses both positive_key and negative_key as button indices.
//  * In Analog Input mode, only the positive_key is used.
//    - The positive_key in Analog Input mode represents an analog axis (i.e
//    GLFW_GAMEPAD_AXIS_RIGHT_Y)
struct AnalogMappingInfo {
  AnalogMappingMode mode = AnalogMappingMode::DigitalInput;
  int axis_id = -1;
  int positive_key = -1;
  int negative_key = -1;
};

struct MappingInfo {
  bool debug = true;  // debug mode
  bool use_mouse = false;
  bool buffer_mode = true;  // use buffered inputs

  int controller_button_mapping[CONTROLLER_COUNT][(int)Pad::Button::Max];
  AnalogMappingInfo controller_analog_mapping[CONTROLLER_COUNT][(int)Pad::Analog::Max];

  int keyboard_button_mapping[CONTROLLER_COUNT][(
      int)Pad::Button::Max];  // Back up in case controller gets disconnected
  AnalogMappingInfo keyboard_analog_mapping[CONTROLLER_COUNT][(int)Pad::Analog::Max];
  double mouse_x_axis_sensitivities[CONTROLLER_COUNT];
  double mouse_y_axis_sensitivities[CONTROLLER_COUNT];
  // TODO complex button mapping & key macros (e.g. shift+x for l2+r2 press etc.)
};

void OnKeyPress(int key);
void OnKeyRelease(int key);
void ClearKey(int key);
void ForceClearKeys();
void ClearKeys();

void DefaultMapping(MappingInfo& mapping);
int IsPressed(MappingInfo& mapping, Button button, int pad);
int GetAnalogValue(MappingInfo& mapping, Analog analog, int pad);
void MapButton(MappingInfo& mapping, Button button, int pad, int key);
void MapAnalog(MappingInfo& mapping, Button button, int pad, AnalogMappingInfo& analogMapping);
void SetAnalogAxisValue(MappingInfo& mapping, int axis, double value);
void ClearAnalogAxisValue(MappingInfo& mapping, int axis);

// this enum is also in pc-pad-utils.gc
enum class InputModeStatus { Disabled, Enabled, Canceled };

extern MappingInfo g_input_mode_mapping;
void EnterInputMode();
void ExitInputMode(bool);
u64 input_mode_get();
u64 input_mode_get_key();
u64 input_mode_get_index();
void input_mode_pad_set(s64);

void initialize();
void update_gamepads(MappingInfo& mapping_info);
int rumble(int pad, float slow_motor, float fast_motor);
int GetGamepadState(int pad);
void ForceClearAnalogValue();
void clear_pad(int pad);

void UpdateAxisValue(MappingInfo& mapping_info);
void SetGamepadState(int pad, int pad_index);
bool* GetKeyboardInputBuffer();
bool* GetKeyboardBufferedInputBuffer();
float* GetKeyboardInputAnalogBuffer(int pad);
bool* GetControllerInputBuffer(int pad);
float* GetControllerAnalogInputBuffer(int pad);

}  // namespace Pad
