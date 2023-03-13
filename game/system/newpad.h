#pragma once

/*!
 * @file newpad.h
 * PC-port specific cpad implementation on the C kernel. Monitors button inputs.
 * Actual input detection is done through window events and is gfx pipeline-dependent.
 */

#include <array>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>

#include "common/common_types.h"

#include "third-party/SDL/include/SDL.h"

namespace Pad {

static const u8 ANALOG_NEUTRAL = 127;

enum PadButtonIndex {
  SELECT = 0,
  L3,
  R3,
  START,
  DPAD_UP,
  DPAD_RIGHT,
  DPAD_DOWN,
  DPAD_LEFT,
  L2,
  R2,
  L1,
  R1,
  TRIANGLE,
  CIRCLE,
  CROSS,
  SQUARE = 15
};

struct PadData {
  // Analog Values
  std::array<u8, 4> analog_data = {ANALOG_NEUTRAL, ANALOG_NEUTRAL, ANALOG_NEUTRAL, ANALOG_NEUTRAL};

  std::pair<u8, u8> analog_left() const { return {analog_data.at(0), analog_data.at(1)}; }
  std::pair<u8, u8> analog_right() const { return {analog_data.at(2), analog_data.at(3)}; }

  // NOTE - pressure is always 255 (max) at this time
  std::array<bool, 16> button_data = {};

  // Normal Buttons
  bool select() const { return button_data.at(PadButtonIndex::SELECT); };
  bool l3() const { return button_data.at(PadButtonIndex::L3); };
  bool r3() const { return button_data.at(PadButtonIndex::R3); };
  bool start() const { return button_data.at(PadButtonIndex::START); };

  // Pressure Buttons
  std::pair<bool, u8> dpad_up() const { return {button_data.at(PadButtonIndex::DPAD_UP), 255}; };
  std::pair<bool, u8> dpad_right() const {
    return {button_data.at(PadButtonIndex::DPAD_RIGHT), 255};
  };
  std::pair<bool, u8> dpad_down() const {
    return {button_data.at(PadButtonIndex::DPAD_DOWN), 255};
  };
  std::pair<bool, u8> dpad_left() const {
    return {button_data.at(PadButtonIndex::DPAD_LEFT), 255};
  };

  std::pair<bool, u8> l2() const { return {button_data.at(PadButtonIndex::L2), 255}; };
  std::pair<bool, u8> r2() const { return {button_data.at(PadButtonIndex::R2), 255}; };
  std::pair<bool, u8> l1() const { return {button_data.at(PadButtonIndex::L1), 255}; };
  std::pair<bool, u8> r1() const { return {button_data.at(PadButtonIndex::R1), 255}; };

  std::pair<bool, u8> triangle() const { return {button_data.at(PadButtonIndex::TRIANGLE), 255}; };
  std::pair<bool, u8> circle() const { return {button_data.at(PadButtonIndex::CIRCLE), 255}; };
  std::pair<bool, u8> cross() const { return {button_data.at(PadButtonIndex::CROSS), 255}; };
  std::pair<bool, u8> square() const { return {button_data.at(PadButtonIndex::SQUARE), 255}; };
};

static const std::unordered_map<u8, std::vector<PadButtonIndex>> s_default_controller_button_binds =
    {{SDL_CONTROLLER_BUTTON_A, {PadButtonIndex::CROSS}},
     {SDL_CONTROLLER_BUTTON_B, {PadButtonIndex::CIRCLE}},
     {SDL_CONTROLLER_BUTTON_X, {PadButtonIndex::SQUARE}},
     {SDL_CONTROLLER_BUTTON_Y, {PadButtonIndex::TRIANGLE}},
     {SDL_CONTROLLER_BUTTON_X, {PadButtonIndex::SQUARE}},
     {SDL_CONTROLLER_BUTTON_LEFTSTICK, {PadButtonIndex::L3}},
     {SDL_CONTROLLER_BUTTON_RIGHTSTICK, {PadButtonIndex::R3}},
     {SDL_CONTROLLER_BUTTON_BACK, {PadButtonIndex::SELECT}},
     {SDL_CONTROLLER_BUTTON_START, {PadButtonIndex::START}},
     {SDL_CONTROLLER_BUTTON_LEFTSHOULDER, {PadButtonIndex::L1}},
     {SDL_CONTROLLER_BUTTON_RIGHTSHOULDER, {PadButtonIndex::R1}},
     {SDL_CONTROLLER_BUTTON_DPAD_UP, {PadButtonIndex::DPAD_UP}},
     {SDL_CONTROLLER_BUTTON_DPAD_DOWN, {PadButtonIndex::DPAD_DOWN}},
     {SDL_CONTROLLER_BUTTON_DPAD_LEFT, {PadButtonIndex::DPAD_LEFT}},
     {SDL_CONTROLLER_BUTTON_DPAD_RIGHT, {PadButtonIndex::DPAD_RIGHT}}};

static const std::unordered_map<u8, std::vector<PadButtonIndex>> s_default_controller_axis_binds = {
    {SDL_CONTROLLER_AXIS_TRIGGERLEFT, {PadButtonIndex::L2}},
    {SDL_CONTROLLER_AXIS_TRIGGERRIGHT, {PadButtonIndex::R2}}};

// A distinct input device.  Only those devices that are "active" should be read
class InputDevice {
 protected:
  bool m_loaded = false;
  std::string m_device_name;

  // TODO - ports!

  // Each binding is a mapping from an SDL input to one or more
  // PS2 inputs
  //
  // TODO - support modifiers (keyboards), not sure how SDL represents those yet
  std::unordered_map<u8, std::vector<PadButtonIndex>> m_button_binds = s_default_controller_button_binds;
  std::unordered_map<u8, std::vector<PadButtonIndex>> m_axis_binds = s_default_controller_axis_binds;
  // TODO - mouse? (sensitivities)
  // TODO - positive/negative keys
  // TODO - digital vs analog
  // bool m_buffer_inputs;  // TODO - not entirely sure what this means, test with old code (hold
  // onto inputs for a frame before firing?)

 public:
  virtual ~InputDevice(){};

  virtual void process_event(const SDL_Event& event, std::shared_ptr<Pad::PadData> data) = 0;
  virtual void close_device() = 0;

  bool is_loaded() const { return m_loaded; };
};

// https://wiki.libsdl.org/SDL2/CategoryGameController
class GameController : public InputDevice {
 public:
  GameController(int sdl_device_id, int dead_zone = 0);
  ~GameController() { close_device(); }

  void process_event(const SDL_Event& event, std::shared_ptr<Pad::PadData> data) override;
  void close_device() override;

 private:
  int m_sdl_instance_id = -1;
  SDL_GameController* m_device_handle;
  int m_analog_dead_zone = 0;
};

// TODO - Keyboard and Mouse (mouse doesn't process events though!)

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

  std::shared_ptr<Pad::PadData> get_current_data() const;
  void change_active_device(int device_id);
  // TODO - remapping support

 private:
  std::vector<std::shared_ptr<InputDevice>> m_available_devices;
  std::shared_ptr<InputDevice> m_active_device;
  std::shared_ptr<Pad::PadData> m_data;
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
