#pragma once

/*!
 * @file newpad.h
 * PC-port specific cpad implementation on the C kernel. Monitors button inputs.
 * Actual input detection is done through window events and is gfx pipeline-dependent.
 */

#include <unordered_map>
#include <optional>
#include "common/common_types.h"

#include "third-party/SDL/include/SDL.h"

namespace Pad {

struct PadData {
  // Analog Values
  std::pair<u8, u8> analog_x;
  std::pair<u8, u8> analog_y;

  // Normal Buttons
  bool select;
  bool l3;
  bool r3;
  bool start;

  // Pressure Buttons
  std::pair<bool, u8> dpad_up;
  std::pair<bool, u8> dpad_right;
  std::pair<bool, u8> dpad_down;
  std::pair<bool, u8> dpad_left;

  std::pair<bool, u8> l2;
  std::pair<bool, u8> r2;
  std::pair<bool, u8> l1;
  std::pair<bool, u8> r1;

  std::pair<bool, u8> triangle;
  std::pair<bool, u8> circle;
  std::pair<bool, u8> cross;
  std::pair<bool, u8> square;
};

// Each binding has a combination of SDL identifiers / modifiers
// and maps to a combination on the original PS2
//
// In most cases, this would be a 1-1 combination, but you could imagine
// binding 1 key to 2 PS2 buttons or some-such.
struct InputBinding {
  std::vector<int> m_sdl_bind;
  std::vector<int> m_ps2_bind;
};

// A distinct input device.  Only those devices that are "active" should be read
class InputDevice {
  enum class Type { JOYSTICK, KEYBOARD, MOUSE };

 public:
  // TODO - do devices have a decent identifier (guid?)
  InputDevice(int sdl_device_index,
              Type type = Type::JOYSTICK,
              int analog_dead_zone = 8000,
              bool buffer_inputs = false);
  void process_event(const SDL_Event& event, PadData& data);
  void close_device();

 private:
  int m_device_index;
  SDL_Joystick* m_device_handle;
  bool m_is_active;
  std::string m_name;
  Type m_type;
  std::vector<InputBinding> m_binds;
  int m_analog_dead_zone;
  // TODO - mouse? (sensitivities)
  // TODO - positive/negative keys
  // TODO - digital vs analog
  bool m_buffer_inputs;  // TODO - not entirely sure what this means, test with old code
};

// Central class that:
// - keeps track of available input devices
// - polls data from the input devices considered active
// - fetches said data to be sent to the game
class InputMonitor {
 public:
  InputMonitor();
  ~InputMonitor();

  // Propagate and handle the SDL event, ignored it if it's not relevant
  void process_sdl_event(const SDL_Event& event);
  void refresh_device_list();
  std::vector<InputDevice> get_available_device_info() const;
  // Polls the current active input device for it's data and update `m_data`
  
  PadData get_current_data() const;
  void change_active_device(int device_id);
  // TODO - remapping support

 private:
  std::vector<InputDevice> m_available_devices;
  std::optional<InputDevice> m_active_device;
  PadData m_data;
};

// void OnKeyPress(int key);
// void OnKeyRelease(int key);
// void ClearKey(int key);
// void ForceClearKeys();
// void ClearKeys();
//
// void DefaultMapping(MappingInfo& mapping);
// int IsPressed(MappingInfo& mapping, Button button, int pad);
// int GetAnalogValue(MappingInfo& mapping, Analog analog, int pad);
// void MapButton(MappingInfo& mapping, Button button, int pad, int key);
// void MapAnalog(MappingInfo& mapping, Analog button, int pad, AnalogMappingInfo& analogMapping);
// void SetAnalogAxisValue(MappingInfo& mapping, int axis, double value);
// void ClearAnalogAxisValue(MappingInfo& mapping, int axis);
//
// extern MappingInfo g_input_mode_mapping;
// void EnterInputMode();
// void ExitInputMode(bool);
// u64 input_mode_get();
// u64 input_mode_get_key();
// u64 input_mode_get_index();
// void input_mode_pad_set(s64);
//
// void initialize();
// void update_gamepads(MappingInfo& mapping_info);
// int rumble(int pad, float slow_motor, float fast_motor);
// int GetGamepadState(int pad);
// void ForceClearAnalogValue();
// void clear_pad(int pad);
//
// void UpdateAxisValue(MappingInfo& mapping_info);
// void SetGamepadState(int pad, int pad_index);
// bool* GetKeyboardInputBuffer();
// bool* GetKeyboardBufferedInputBuffer();
// float* GetKeyboardInputAnalogBuffer(int pad);
// bool* GetControllerInputBuffer(int pad);
// float* GetControllerAnalogInputBuffer(int pad);

}  // namespace Pad
