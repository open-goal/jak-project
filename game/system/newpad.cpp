/*!
 * @file newpad.cpp
 * PC-port specific cpad implementation on the C kernel. Monitors button inputs.
 * Actual input detection is done through window events and is gfx pipeline-dependent.
 */

#include "newpad.h"

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

constexpr int NUM_KEYS = GLFW_KEY_LAST + 1;
// key-down status of any detected key.
bool g_key_status[NUM_KEYS] = {0};
// key-down status of any detected key. this is buffered for the remainder of a frame.
bool g_buffered_key_status[NUM_KEYS] = {0};

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

void ForceClearKeys() {
  for (auto& key : g_key_status) {
    key = false;
  }
  for (auto& key : g_buffered_key_status) {
    key = false;
  }
}

void ClearKeys() {
  for (int key = 0; key < NUM_KEYS; key++) {
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
  ASSERT(key < NUM_KEYS);
  g_key_status[key] = true;
  // set buffered key status
  g_buffered_key_status[key] = true;
}

void OnKeyRelease(int key) {
  if (input_mode == InputModeStatus::Enabled) {
    return;
  }
  ASSERT(key < NUM_KEYS);
  g_key_status[key] = false;
}

/*
********************************
* Pad checking
********************************
*/

static int CheckPadIdx(int pad) {
  if (pad < 0 || pad > CONTROLLER_COUNT) {
    lg::error("Invalid pad {}", pad);
    return -1;
  }
  return pad;
}

// returns 1 if button is pressed. returns 0 if invalid or not pressed.
int IsPressed(MappingInfo& mapping, Button button, int pad = 0) {
  if (CheckPadIdx(pad) == -1) {
    return 0;
  }

  if (g_gamepad_buttons[pad][(int)button]) {
    return 1;
  }
  auto key = mapping.pad_mapping[pad][(int)button];
  if (key == -1)
    return 0;
  auto& keymap = mapping.buffer_mode ? g_buffered_key_status : g_key_status;
  ASSERT(key < NUM_KEYS);
  return keymap[key];
}

// returns the value of the analog axis (in the future, likely pressure sensitive if we support it?)
// if invalid or otherwise -- returns 127 (analog stick neutral position)
int AnalogValue(MappingInfo& /*mapping*/, Analog analog, int pad = 0) {
  if (CheckPadIdx(pad) == -1) {
    // Pad out of range, return a stable value
    return 127;
  }

  float input = 0.0f;
  if (pad == 0) {
    // Movement controls mapped to WASD keys
    if (g_buffered_key_status[GLFW_KEY_W] && analog == Analog::Left_Y)
      input += -1.0f;
    if (g_buffered_key_status[GLFW_KEY_S] && analog == Analog::Left_Y)
      input += 1.0f;
    if (g_buffered_key_status[GLFW_KEY_A] && analog == Analog::Left_X)
      input += -1.0f;
    if (g_buffered_key_status[GLFW_KEY_D] && analog == Analog::Left_X)
      input += 1.0f;

    // Camera controls mapped to IJKL keys
    if (g_buffered_key_status[GLFW_KEY_I] && analog == Analog::Right_Y)
      input += -1.0f;
    if (g_buffered_key_status[GLFW_KEY_K] && analog == Analog::Right_Y)
      input += 1.0f;
    if (g_buffered_key_status[GLFW_KEY_J] && analog == Analog::Right_X)
      input += -1.0f;
    if (g_buffered_key_status[GLFW_KEY_L] && analog == Analog::Right_X)
      input += 1.0f;
  } else if (pad == 1) {
    // these bindings are not sane
    if (g_buffered_key_status[GLFW_KEY_KP_5] && analog == Analog::Left_Y)
      input += -1.0f;
    if (g_buffered_key_status[GLFW_KEY_KP_2] && analog == Analog::Left_Y)
      input += 1.0f;
    if (g_buffered_key_status[GLFW_KEY_KP_1] && analog == Analog::Left_X)
      input += -1.0f;
    if (g_buffered_key_status[GLFW_KEY_KP_3] && analog == Analog::Left_X)
      input += 1.0f;

    // these bindings are not sane
    if (g_buffered_key_status[GLFW_KEY_KP_DIVIDE] && analog == Analog::Right_Y)
      input += -1.0f;
    if (g_buffered_key_status[GLFW_KEY_KP_8] && analog == Analog::Right_Y)
      input += 1.0f;
    if (g_buffered_key_status[GLFW_KEY_KP_7] && analog == Analog::Right_X)
      input += -1.0f;
    if (g_buffered_key_status[GLFW_KEY_KP_9] && analog == Analog::Right_X)
      input += 1.0f;
  }

  if (input == 0) {
    input = g_gamepad_analogs[pad][(int)analog];
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

  mapping.pad_mapping[pad][(int)button] = key;
}

// reset button mappings
void DefaultMapping(MappingInfo& mapping) {
  // make every button invalid
  for (int p = 0; p < CONTROLLER_COUNT; ++p) {
    for (int i = 0; i < (int)Button::Max; ++i) {
      MapButton(mapping, (Button)i, p, -1);
    }
  }

  // TODO - these are different from the analog bindings above and cause
  // the keyboard to be bound to controls regardless
  //
  // Need someway to toggle off -- where do we have access to the game's settings?

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
  for (int i = 0; i < 4; ++i) {
    g_gamepad_analogs[pad][i] = 0;
  }
}

void update_gamepads() {
  check_gamepads();

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

  constexpr std::pair<Analog, int> gamepad_analog_map[] = {
      {Analog::Left_X, GLFW_GAMEPAD_AXIS_LEFT_X},
      {Analog::Left_Y, GLFW_GAMEPAD_AXIS_LEFT_Y},
      {Analog::Right_X, GLFW_GAMEPAD_AXIS_RIGHT_X},
      {Analog::Right_Y, GLFW_GAMEPAD_AXIS_RIGHT_Y}};

  auto read_pad_state = [gamepad_map, gamepad_analog_map](int pad) {
    GLFWgamepadstate state;
    glfwGetGamepadState(g_gamepads.gamepad_idx[pad], &state);

    for (const auto& [button, idx] : gamepad_map) {
      g_gamepad_buttons[pad][(int)button] = state.buttons[idx];
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

};  // namespace Pad
