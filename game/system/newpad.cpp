/*!
 * @file newpad.cpp
 * PC-port specific cpad implementation on the C kernel. Monitors button inputs.
 * Actual input detection is done through window events and is gfx pipeline-dependent.
 */

#include "newpad.h"
#include "common/log/log.h"

#include "game/graphics/pipelines/opengl.h"  // for GLFW macros
#include "game/kernel/kscheme.h"

namespace Pad {

/*
********************************
* Key checking
********************************
*/

std::unordered_map<int, int> g_key_status;
std::unordered_map<int, int> g_buffered_key_status;

// input mode for controller mapping
InputModeStatus input_mode = InputModeStatus::Disabled;
u64 input_mode_pad = 0;
u64 input_mode_key = -1;
u64 input_mode_mod = 0;
u64 input_mode_index = 0;
MappingInfo g_input_mode_mapping;

void ForceClearKeys() {
  g_key_status.clear();
  g_buffered_key_status.clear();
}

void ClearKeys() {
  g_buffered_key_status.clear();
  for (auto& key : g_key_status) {
    g_buffered_key_status.insert(std::make_pair(key.first, key.second));
  }
}

void OnKeyPress(int key) {
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
  if (g_key_status.find(key) == g_key_status.end()) {
    g_key_status.insert(std::make_pair(key, 1));
  } else {
    g_key_status.at(key) = 1;
  }

  // set buffered key status
  if (g_buffered_key_status.find(key) == g_buffered_key_status.end()) {
    g_buffered_key_status.insert(std::make_pair(key, 1));
  } else {
    g_buffered_key_status.at(key) = 1;
  }
}

void OnKeyRelease(int key) {
  if (input_mode == InputModeStatus::Enabled) {
    return;
  }

  // if we come out of input mode, the key wont be found.
  if (g_key_status.find(key) == g_key_status.end()) {
    return;
  }
  // set absolute key status
  g_key_status.at(key) = 0;
}

/*
********************************
* Pad checking
********************************
*/

static int CheckPadIdx(int pad) {
  if (pad < 0 || pad > CONTROLLER_COUNT) {
    lg::error("Invalid pad {}, returning pad 0", pad);
  }
  return 0;
}

// returns 1 if button is pressed. returns 0 if invalid or not pressed.
int IsPressed(MappingInfo& mapping, Button button, int pad = 0) {
  auto key = mapping.pad_mapping[CheckPadIdx(pad)][(int)button];
  if (key == -1)
    return 0;
  auto& keymap = mapping.buffer_mode ? g_buffered_key_status : g_key_status;
  if (keymap.find(key) == keymap.end())
    return 0;
  return keymap.at(key);
}

// map a button on a pad to a key
void MapButton(MappingInfo& mapping, Button button, int pad, int key) {
  // check if pad is valid. dont map buttons with invalid pads.
  if (CheckPadIdx(pad) != pad)
    return;

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

  // face buttons
  MapButton(mapping, Button::Ecks, 0, GLFW_KEY_Z);
  MapButton(mapping, Button::Square, 0, GLFW_KEY_X);
  MapButton(mapping, Button::Triangle, 0, GLFW_KEY_S);
  MapButton(mapping, Button::Circle, 0, GLFW_KEY_A);

  // dpad
  MapButton(mapping, Button::Up, 0, GLFW_KEY_UP);
  MapButton(mapping, Button::Right, 0, GLFW_KEY_RIGHT);
  MapButton(mapping, Button::Down, 0, GLFW_KEY_DOWN);
  MapButton(mapping, Button::Left, 0, GLFW_KEY_LEFT);

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

};  // namespace Pad
