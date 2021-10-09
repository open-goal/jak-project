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

struct MappingInfo {
  bool debug = true;        // debug mode
  bool buffer_mode = true;  // use buffered inputs

  int pad_mapping[CONTROLLER_COUNT][(int)Pad::Button::Max];  // controller button mapping
  // TODO complex button mapping & key macros (e.g. shift+x for l2+r2 press etc.)
};

// key-down status of any detected key.
extern std::unordered_map<int, int> g_key_status;
// key-down status of any detected key. this is buffered for the remainder of a frame.
extern std::unordered_map<int, int> g_buffered_key_status;

void OnKeyPress(int key);
void OnKeyRelease(int key);
void ForceClearKeys();
void ClearKeys();

void DefaultMapping(MappingInfo& mapping);
int IsPressed(MappingInfo& mapping, Button button, int pad);
void MapButton(MappingInfo& mapping, Button button, int pad, int key);

// this enum is also in pc-pad-utils.gc
enum class InputModeStatus { Disabled, Enabled, Canceled };

extern MappingInfo g_input_mode_mapping;
void EnterInputMode();
void ExitInputMode(bool);
u64 input_mode_get();
u64 input_mode_get_key();
u64 input_mode_get_index();

void initialize();
void update_gamepads();

}  // namespace Pad
