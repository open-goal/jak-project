/*!
 * @file newpad.cpp
 * PC-port specific cpad implementation on the C kernel. Monitors button inputs.
 * Actual input detection is done through window events and is gfx pipeline-dependent.
 */

#include "newpad.h"

#include <atomic>
#include <cmath>

#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/util/FileUtil.h"

#include "game/graphics/pipelines/opengl.h"  // for GLFW macros

#include "third-party/imgui/imgui.h"

namespace Pad {

/*
********************************
* Key checking
********************************
*/

// key-down status of any detected key.
bool g_key_status[glfw::NUM_KEYS] = {0};
// key-down status of any detected key. this is buffered for the remainder of a frame.
bool g_buffered_key_status[glfw::NUM_KEYS] = {0};

float g_key_analogs[CONTROLLER_COUNT][(int)Analog::Max] = {{0}};

bool g_gamepad_buttons[CONTROLLER_COUNT][(int)Button::Max] = {{0}};
float g_gamepad_analogs[CONTROLLER_COUNT][(int)Analog::Max] = {{0}};

struct GamepadState {
  int gamepad_idx[CONTROLLER_COUNT] = {-1, -1, -1, -1};
  bool glfw_joystick_used[GLFW_JOYSTICK_LAST + 1] = {false};
} g_gamepads;

// input mode for controller mapping
InputModeStatus input_mode = InputModeStatus::Disabled;
int input_mode_pad = 0;
u64 input_mode_key = -1;
u64 input_mode_mod = 0;
u64 input_mode_index = 0;
MappingInfo g_input_mode_mapping;

void ClearKey(int key) {
  if (key < 0 || key > glfw::NUM_KEYS) {
    lg::warn("ClearKey failed: Attempted to clear invalid key {}", key);
    return;
  }

  g_key_status[key] = false;
  g_buffered_key_status[key] = false;
}

void ClearAnalogAxisValue(MappingInfo& mapping_info, int axis) {
  for (int pad = 0; pad < CONTROLLER_COUNT; ++pad) {
    for (int analog = 0; analog < (int)Analog::Max; ++analog) {
      if (mapping_info.keyboard_analog_mapping[pad][analog].axis_id == axis &&
          mapping_info.keyboard_analog_mapping[pad][analog].mode ==
              AnalogMappingMode::AnalogInput) {
        g_key_analogs[pad][analog] = 0.0f;
      }
    }
  }
}

void ForceClearAnalogValue() {
  for (int pad = 0; pad < CONTROLLER_COUNT; ++pad) {
    for (int analog = 0; analog < (int)Analog::Max; ++analog) {
      g_key_analogs[pad][analog] = 0.0f;
    }
  }
}

void ForceClearKeys() {
  for (auto& key : g_key_status) {
    key = false;
  }
  for (auto& key : g_buffered_key_status) {
    key = false;
  }
}

void ClearKeys() {
  for (int key = 0; key < glfw::NUM_KEYS; key++) {
    g_buffered_key_status[key] = g_key_status[key];
  }
}

void OnKeyPress(int key) {
  if (ImGui::IsAnyItemActive()) {
    return;
  }

  if (input_mode == InputModeStatus::Enabled) {
    if (key == GLFW_KEY_ESCAPE) {
      ExitInputMode(true);
      return;
    }
    input_mode_key = key;
    MapButton(g_input_mode_mapping, (Button)(input_mode_index++), input_mode_pad, key);
    if (input_mode_index >= (u64)Button::Max) {
      ExitInputMode(false);
    }
    return;
  }
  // set absolute key status
  ASSERT(key < glfw::NUM_KEYS);
  g_key_status[key] = true;
  // set buffered key status
  g_buffered_key_status[key] = true;
}

void OnKeyRelease(int key) {
  if (input_mode == InputModeStatus::Enabled) {
    return;
  }
  ASSERT(key < glfw::NUM_KEYS);
  g_key_status[key] = false;
}

/*
********************************
* Pad checking
********************************
*/

static int CheckPadIdx(int pad) {
  if (pad < 0 || pad >= CONTROLLER_COUNT) {
    lg::error("Invalid pad {}", pad);
    return -1;
  }
  return pad;
}

// returns 1 if either keyboard or controller button is pressed. Controller button has priority.
// returns 0 if invalid or not pressed.
int IsPressed(MappingInfo& mapping, Button button, int pad = 0) {
  if (CheckPadIdx(pad) == -1) {
    return 0;
  }

  if (g_gamepad_buttons[pad][(int)button]) {
    return 1;
  }

  int key = mapping.keyboard_button_mapping[pad][(int)button];
  if (key == -1)
    return 0;
  auto& keymap = mapping.buffer_mode ? g_buffered_key_status : g_key_status;
  ASSERT(key < glfw::NUM_KEYS);
  return keymap[key];
}

void SetAnalogAxisValue(MappingInfo& mapping_info, int axis, double value) {
  const double sensitivity_numerator = Gfx::g_global_settings.target_fps;
  const double minimum_sensitivity = 1e-4;

  for (int pad = 0; pad < CONTROLLER_COUNT; ++pad) {
    for (int analog = 0; analog < (int)Analog::Max; ++analog) {
      if (mapping_info.keyboard_analog_mapping[pad][analog].axis_id == axis) {
        double newValue = value;
        if (axis == GlfwKeyCustomAxis::CURSOR_X_AXIS) {
          if (mapping_info.mouse_x_axis_sensitivities[pad] < minimum_sensitivity) {
            mapping_info.mouse_x_axis_sensitivities[pad] = minimum_sensitivity;
          }
          newValue /= (sensitivity_numerator / mapping_info.mouse_x_axis_sensitivities[pad]);
        } else if (axis == GlfwKeyCustomAxis::CURSOR_Y_AXIS) {
          if (mapping_info.mouse_y_axis_sensitivities[pad] < minimum_sensitivity) {
            mapping_info.mouse_y_axis_sensitivities[pad] = minimum_sensitivity;
          }
          newValue /= (sensitivity_numerator / mapping_info.mouse_y_axis_sensitivities[pad]);
        }

        if (newValue > 1.0) {
          g_key_analogs[pad][analog] = 1.0;
        } else if (newValue < -1.0) {
          g_key_analogs[pad][analog] = -1.0;
        } else if (std::isnan(newValue)) {
          g_key_analogs[pad][analog] = 0.0;
        } else {
          g_key_analogs[pad][analog] = newValue;
        }

        // Invert logic used here. Left Y axis movement is based on towrds the camera.
        // In game forward is treated as going away from the camera and backwards is headed towards
        // the camera.
        if (axis == GlfwKeyCustomAxis::CURSOR_Y_AXIS) {
          g_key_analogs[pad][analog] *= -1;
        }
      }
    }
  }
}

void UpdateAxisValue(MappingInfo& mapping_info) {
  for (int pad = 0; pad < CONTROLLER_COUNT; ++pad) {
    for (int analog = 0; analog < (int)Analog::Max; ++analog) {
      if (mapping_info.keyboard_analog_mapping[pad][analog].mode ==
          AnalogMappingMode::AnalogInput) {
        continue;  // Assumed Set Axis set value already
      }

      // Invert logic used here. Left Y axis movement is based on towrds the camera.
      // In game forward is treated as going away from the camera and backwards is headed towards
      // the camera.
      double input = 0.0f;
      if (mapping_info.keyboard_analog_mapping[pad][analog].positive_key > -1 &&
          mapping_info.keyboard_analog_mapping[pad][analog].positive_key < glfw::NUM_KEYS) {
        if (analog == static_cast<int>(Analog::Left_Y) ||
            analog == static_cast<int>(Analog::Right_Y)) {
          input -=
              g_buffered_key_status[mapping_info.keyboard_analog_mapping[pad][analog].positive_key];
        } else {
          input +=
              g_buffered_key_status[mapping_info.keyboard_analog_mapping[pad][analog].positive_key];
        }
      }
      if (mapping_info.keyboard_analog_mapping[pad][analog].negative_key > -1 &&
          mapping_info.keyboard_analog_mapping[pad][analog].negative_key < glfw::NUM_KEYS) {
        if (analog == static_cast<int>(Analog::Left_Y) ||
            analog == static_cast<int>(Analog::Right_Y)) {
          input +=
              g_buffered_key_status[mapping_info.keyboard_analog_mapping[pad][analog].negative_key];
        } else {
          input -=
              g_buffered_key_status[mapping_info.keyboard_analog_mapping[pad][analog].negative_key];
        }
      }
      g_key_analogs[pad][analog] = input;
    }
  }
}

// returns the value of the analog axis (in the future, likely pressure sensitive if we support it?)
// if invalid or otherwise -- returns 127 (analog stick neutral position)
int GetAnalogValue(MappingInfo& /*mapping*/, Analog analog, int pad = 0) {
  float input = 0.0f;
  if (CheckPadIdx(pad) == -1) {
    // Pad out of range, return a stable value
    return 127;
  }

  float controller_input = 0.0f;
  if (g_gamepads.gamepad_idx[pad] > -1) {
    controller_input = g_gamepad_analogs[pad][(int)analog];
  }

  float keyboard_input = g_key_analogs[pad][(int)analog];
  // Hack. Clearing the buffer immediately can lead to inconsistencies on analog input.
  // If a mouse is disconnected or can't calculate a new delta it would stay stuck at 1.
  // Decreasing the values gradually seems like a good comprise.
  g_key_analogs[pad][(int)analog] *= 0.95;

  if (fabs(controller_input) > fabs(keyboard_input)) {
    input = controller_input;
  } else {
    input = keyboard_input;
  }

  // GLFW provides float in range -1 to 1, caller expects 0-255
  const float input_low = -1.0f;
  const float input_high = 1.0f;
  const int output_low = 0;
  const int output_high = 255;

  // Map from input to output range
  return int((input - input_low) * (output_high - output_low) / (input_high - input_low) +
             output_low);
}

// map a button on a pad to a key
void MapButton(MappingInfo& mapping, Button button, int pad, int key) {
  // check if pad is valid. dont map buttons with invalid pads.
  if (CheckPadIdx(pad) == -1) {
    return;
  }

  if (g_gamepads.gamepad_idx[pad] == -1) {
    // TODO: Check if other pad is keyboard and if key is already bound
    mapping.keyboard_button_mapping[pad][(int)button] = key;
  } else {
    mapping.controller_button_mapping[pad][(int)button] = key;
  }
}

void MapAnalog(MappingInfo& mapping, Analog button, int pad, AnalogMappingInfo& analomapping_info) {
  // check if pad is valid. dont map buttons with invalid pads.
  if (CheckPadIdx(pad) == -1) {
    return;
  }

  if (g_gamepads.gamepad_idx[pad] == -1) {
    // TODO: Check if other pad is keyboard and if key is already bound
    mapping.keyboard_analog_mapping[pad][(int)button] = analomapping_info;
  } else {
    mapping.controller_analog_mapping[pad][(int)button] = analomapping_info;
  }
}

// reset button mappings
void DefaultMapping(MappingInfo& mapping) {
  // make every button invalid
  for (int32_t pad = 0; pad < CONTROLLER_COUNT; ++pad) {
    for (int32_t button = 0; button < (int)Pad::Button::Max; ++button) {
      mapping.controller_button_mapping[pad][button] = -1;
      mapping.keyboard_button_mapping[pad][button] = -1;
    }

    for (int32_t analog = 0; analog < (int)Pad::Analog::Max; ++analog) {
      mapping.controller_analog_mapping[pad][analog] = AnalogMappingInfo();
      mapping.keyboard_analog_mapping[pad][analog] = AnalogMappingInfo();
    }
  }

  constexpr std::pair<Button, int> gamepad_map[] = {
      {Button::Select, GLFW_GAMEPAD_BUTTON_BACK},
      {Button::L3, GLFW_GAMEPAD_BUTTON_LEFT_THUMB},
      {Button::R3, GLFW_GAMEPAD_BUTTON_RIGHT_THUMB},
      {Button::Start, GLFW_GAMEPAD_BUTTON_START},
      {Button::Up, GLFW_GAMEPAD_BUTTON_DPAD_UP},
      {Button::Right, GLFW_GAMEPAD_BUTTON_DPAD_RIGHT},
      {Button::Down, GLFW_GAMEPAD_BUTTON_DPAD_DOWN},
      {Button::Left, GLFW_GAMEPAD_BUTTON_DPAD_LEFT},
      {Button::L1, GLFW_GAMEPAD_BUTTON_LEFT_BUMPER},
      {Button::R1, GLFW_GAMEPAD_BUTTON_RIGHT_BUMPER},
      {Button::Triangle, GLFW_GAMEPAD_BUTTON_TRIANGLE},
      {Button::Circle, GLFW_GAMEPAD_BUTTON_CIRCLE},
      {Button::X, GLFW_GAMEPAD_BUTTON_CROSS},
      {Button::Square, GLFW_GAMEPAD_BUTTON_SQUARE}};

  for (int32_t pad = 0; pad < CONTROLLER_COUNT; ++pad) {
    for (const auto& [button, value] : gamepad_map) {
      mapping.controller_button_mapping[pad][(int)button] = value;
    }
  }

  // TODO - these are different from the analog bindings above and cause
  // the keyboard to be bound to controls regardless
  //
  // Need someway to toggle off -- where do we have access to the game's settings?

  // TODO - What should the second pc default controls be?

  // R1 / L1
  MapButton(mapping, Button::L1, 0, GLFW_KEY_Q);
  MapButton(mapping, Button::R1, 0, GLFW_KEY_O);

  // R2 / L2
  MapButton(mapping, Button::L2, 0, GLFW_KEY_1);
  MapButton(mapping, Button::R2, 0, GLFW_KEY_P);

  // face buttons
  MapButton(mapping, Button::Ecks, 0, GLFW_KEY_SPACE);
  MapButton(mapping, Button::Square, 0, GLFW_KEY_F);
  MapButton(mapping, Button::Triangle, 0, GLFW_KEY_R);
  MapButton(mapping, Button::Circle, 0, GLFW_KEY_E);

  // dpad
  MapButton(mapping, Button::Up, 0, GLFW_KEY_UP);
  MapButton(mapping, Button::Right, 0, GLFW_KEY_RIGHT);
  MapButton(mapping, Button::Down, 0, GLFW_KEY_DOWN);
  MapButton(mapping, Button::Left, 0, GLFW_KEY_LEFT);

  // start for progress
  MapButton(mapping, Button::Start, 0, GLFW_KEY_ENTER);

  // l3/r3 for menu
  MapButton(mapping, Button::L3, 0, GLFW_KEY_COMMA);
  MapButton(mapping, Button::R3, 0, GLFW_KEY_PERIOD);

  AnalogMappingInfo analomapping_info;

  analomapping_info.positive_key = GLFW_KEY_D;
  analomapping_info.negative_key = GLFW_KEY_A;
  MapAnalog(mapping, Analog::Left_X, 0, analomapping_info);

  analomapping_info.positive_key = GLFW_KEY_W;
  analomapping_info.negative_key = GLFW_KEY_S;
  MapAnalog(mapping, Analog::Left_Y, 0, analomapping_info);

  analomapping_info.mode = AnalogMappingMode::AnalogInput;
  analomapping_info.axis_id = GlfwKeyCustomAxis::CURSOR_X_AXIS;
  MapAnalog(mapping, Analog::Right_X, 0, analomapping_info);

  analomapping_info.axis_id = GlfwKeyCustomAxis::CURSOR_Y_AXIS;
  MapAnalog(mapping, Analog::Right_Y, 0, analomapping_info);

  const double default_mouse_x_sensitivity = 5.0f;
  const double default_mouse_y_sensitivity = 2.0f;

  for (int pad = 0; pad < CONTROLLER_COUNT; ++pad) {
    mapping.mouse_x_axis_sensitivities[pad] = default_mouse_x_sensitivity;
    mapping.mouse_y_axis_sensitivities[pad] = default_mouse_y_sensitivity;
  }
}

void EnterInputMode() {
  input_mode = InputModeStatus::Enabled;
  input_mode_index = 0;
  input_mode_pad = 0;
}

void ExitInputMode(bool canceled) {
  input_mode = canceled ? InputModeStatus::Canceled : InputModeStatus::Disabled;
}

u64 input_mode_get() {
  return (u64)input_mode;
}

u64 input_mode_get_key() {
  return input_mode_key;
}

u64 input_mode_get_index() {
  return input_mode_index;
}

void input_mode_pad_set(s64 idx) {
  input_mode_pad = idx;
}

/*
********************************
* Gamepad Support
********************************
*/

void check_gamepads() {
  auto check_pad = [](int pad) {  // -> bool
    if (g_gamepads.gamepad_idx[pad] == -1) {
      for (int i = GLFW_JOYSTICK_1; i <= GLFW_JOYSTICK_LAST; i++) {
        if (g_gamepads.glfw_joystick_used[i]) {
          continue;
        }
        if (glfwJoystickPresent(i) && glfwJoystickIsGamepad(i)) {
          g_gamepads.gamepad_idx[pad] = i;
          g_gamepads.glfw_joystick_used[i] = true;
          lg::info("Using joystick {} for pad {}: {}, {}", i, pad, glfwGetJoystickName(i),
                   glfwGetGamepadName(i));
          break;
        }
      }
    } else if (!glfwJoystickPresent(g_gamepads.gamepad_idx[pad])) {
      lg::info("Pad {} / joystick {} has been disconnected", pad, g_gamepads.gamepad_idx[pad]);
      g_gamepads.glfw_joystick_used[g_gamepads.gamepad_idx[pad]] = false;
      g_gamepads.gamepad_idx[pad] = -1;
      return false;
    }
    return true;  // pad already exists or was created
  };

  for (int i = 0; i < CONTROLLER_COUNT; i++) {
    check_pad(i);
  }
}

void initialize() {
  std::string mapping_path =
      (file_util::get_jak_project_dir() / "game" / "assets" / "sdl_controller_db.txt").string();
  glfwUpdateGamepadMappings(file_util::read_text_file(mapping_path).c_str());
  check_gamepads();
  if (g_gamepads.gamepad_idx[0] == -1) {
    lg::info("No joysticks found.");
  }
}

void clear_pad(int pad) {
  for (int i = 0; i < (int)Button::Max; ++i) {
    g_gamepad_buttons[pad][i] = false;
  }
  for (int i = 0; i < (int)Analog::Max; ++i) {
    g_gamepad_analogs[pad][i] = 0;
  }
}

void update_gamepads(MappingInfo& mapping_info) {
  check_gamepads();
  UpdateAxisValue(mapping_info);

  if (g_gamepads.gamepad_idx[0] == -1) {
    for (int pad = 0; pad < CONTROLLER_COUNT; ++pad) {
      clear_pad(pad);
    }
    return;
  }

  constexpr std::pair<Analog, int> gamepad_analog_map[] = {
      {Analog::Left_X, GLFW_GAMEPAD_AXIS_LEFT_X},
      {Analog::Left_Y, GLFW_GAMEPAD_AXIS_LEFT_Y},
      {Analog::Right_X, GLFW_GAMEPAD_AXIS_RIGHT_X},
      {Analog::Right_Y, GLFW_GAMEPAD_AXIS_RIGHT_Y}};

  auto read_pad_state = [gamepad_analog_map, mapping_info](int pad) {
    GLFWgamepadstate state;
    glfwGetGamepadState(g_gamepads.gamepad_idx[pad], &state);

    for (int32_t button = 0; button < (int)Pad::Button::Max; ++button) {
      int key = mapping_info.controller_button_mapping[pad][button];
      g_gamepad_buttons[pad][(int)button] = state.buttons[key];
    }

    g_gamepad_buttons[pad][(int)Button::L2] = state.axes[GLFW_GAMEPAD_AXIS_LEFT_TRIGGER] > 0;
    g_gamepad_buttons[pad][(int)Button::R2] = state.axes[GLFW_GAMEPAD_AXIS_RIGHT_TRIGGER] > 0;

    for (const auto& [analog_vector, idx] : gamepad_analog_map) {
      g_gamepad_analogs[pad][(int)analog_vector] = state.axes[idx];
    }
  };

  for (int i = 0; i < CONTROLLER_COUNT; i++) {
    if (g_gamepads.gamepad_idx[i] != -1)
      read_pad_state(i);
    else
      clear_pad(i);
  }
}

int rumble(int pad, float slow_motor, float fast_motor) {
  if (g_gamepads.gamepad_idx[pad] != -1 &&
      glfwSetJoystickRumble(g_gamepads.gamepad_idx[pad], slow_motor, fast_motor)) {
    return 1;
  }
  return 0;
}

int GetGamepadState(int pad) {
  return g_gamepads.gamepad_idx[pad];
}

// The following setters/getters are mainly used for unit tests
void SetGamepadState(int pad, int pad_index) {
  if (CheckPadIdx(pad) != -1) {
    if (pad_index <= GLFW_JOYSTICK_LAST) {
      g_gamepads.gamepad_idx[pad] = pad_index;
    }
  }
}

bool* GetKeyboardInputBuffer() {
  return g_key_status;
}
// key-down status of any detected key. this is buffered for the remainder of a frame.
bool* GetKeyboardBufferedInputBuffer() {
  return g_buffered_key_status;
}

float* GetKeyboardInputAnalogBuffer(int pad) {
  return g_key_analogs[pad];
}

bool* GetControllerInputBuffer(int pad) {
  return g_gamepad_buttons[pad];
}

float* GetControllerAnalogInputBuffer(int pad) {
  return g_gamepad_analogs[pad];
}

};  // namespace Pad
