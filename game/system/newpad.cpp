/*!
 * @file newpad.cpp
 * PC-port specific cpad implementation on the C kernel. Monitors button inputs.
 * Actual input detection is done through window events and is gfx pipeline-dependent.
 */

#include "newpad.h"
#include "common/log/log.h"

#include "game/graphics/pipelines/opengl.h"  // for GLFW macros

namespace Pad {

/*
********************************
* Key checking
********************************
*/

std::unordered_map<int, int> g_key_status;
std::unordered_map<int, int> g_buffered_key_status;

void ForceClearKeys() {
  g_key_status.clear();
  g_buffered_key_status.clear();
}

void ClearKeys() {
  for (auto& key : g_key_status) {
    key.second = false;
  }
  for (auto& key : g_buffered_key_status) {
    key.second = false;
  }
}

void OnKeyPress(int key) {
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
  // set absolute key status
  // no bounds checking for now in order to catch bugs
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
}

};  // namespace Pad
