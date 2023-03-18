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

/*
TODO:
  - Ports
  - Hotswapping
  - Register functions (ie. screenshots / hiding imgui)
  - Controller Selection
  - Custom Binds from JSON
  - PS4/PS5 color customizing
*/

struct PadData {
  enum AnalogIndex { LEFT_X = 0, LEFT_Y, RIGHT_X, RIGHT_Y = 3 };

  enum ButtonIndex {
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

  static const u8 ANALOG_NEUTRAL = 127;

  // Analog Values
  std::array<u8, 4> analog_data = {ANALOG_NEUTRAL, ANALOG_NEUTRAL, ANALOG_NEUTRAL, ANALOG_NEUTRAL};

  std::pair<u8, u8> analog_left() const {
    return {analog_data.at(AnalogIndex::LEFT_X), analog_data.at(AnalogIndex::LEFT_Y)};
  }
  std::pair<u8, u8> analog_right() const {
    return {analog_data.at(AnalogIndex::RIGHT_X), analog_data.at(AnalogIndex::RIGHT_Y)};
  }

  // NOTE - pressure is always 255 (max)
  std::array<bool, 16> button_data = {};

  // Normal Buttons
  bool select() const { return button_data.at(ButtonIndex::SELECT); };
  bool l3() const { return button_data.at(ButtonIndex::L3); };
  bool r3() const { return button_data.at(ButtonIndex::R3); };
  bool start() const { return button_data.at(ButtonIndex::START); };

  // Pressure Buttons
  std::pair<bool, u8> dpad_up() const { return {button_data.at(ButtonIndex::DPAD_UP), 255}; };
  std::pair<bool, u8> dpad_right() const { return {button_data.at(ButtonIndex::DPAD_RIGHT), 255}; };
  std::pair<bool, u8> dpad_down() const { return {button_data.at(ButtonIndex::DPAD_DOWN), 255}; };
  std::pair<bool, u8> dpad_left() const { return {button_data.at(ButtonIndex::DPAD_LEFT), 255}; };

  std::pair<bool, u8> l2() const { return {button_data.at(ButtonIndex::L2), 255}; };
  std::pair<bool, u8> r2() const { return {button_data.at(ButtonIndex::R2), 255}; };
  std::pair<bool, u8> l1() const { return {button_data.at(ButtonIndex::L1), 255}; };
  std::pair<bool, u8> r1() const { return {button_data.at(ButtonIndex::R1), 255}; };

  std::pair<bool, u8> triangle() const { return {button_data.at(ButtonIndex::TRIANGLE), 255}; };
  std::pair<bool, u8> circle() const { return {button_data.at(ButtonIndex::CIRCLE), 255}; };
  std::pair<bool, u8> cross() const { return {button_data.at(ButtonIndex::CROSS), 255}; };
  std::pair<bool, u8> square() const { return {button_data.at(ButtonIndex::SQUARE), 255}; };
};

/// Contains all information needed when processing a host input
/// For example -- for a keyboard binding it informs us what modifiers need to be hit, etc
///
/// All bindings _must_ provide the PS2 button/analog index they map to
struct InputBinding {
  InputBinding(int index) : pad_data_index(index){};
  InputBinding(int index, bool _inverse_val) : pad_data_index(index), inverse_val(_inverse_val){};

  /// Corresponds to PadData::AnalogIndex or PadData::ButtonIndex
  int pad_data_index;
  // Keyboard Stipulations
  /// If considered pressed, it will invert the value (ie, left/right on an analog stick)
  bool inverse_val = false;
  // https://wiki.libsdl.org/SDL2/SDL_Keymod
  bool need_shift = false;
  bool need_ctrl = false;
  bool need_meta = false;  // aka GUI / windows key
  bool need_alt = false;
};

struct InputBindingGroups {
  std::unordered_map<uint8_t, std::vector<InputBinding>> analog_axii;
  std::unordered_map<uint8_t, std::vector<InputBinding>> button_axii;
  std::unordered_map<uint8_t, std::vector<InputBinding>> buttons;
};

// A distinct input device.  Only those devices that are "active" should be read
class InputDevice {
 protected:
  bool m_loaded = false;
  std::string m_device_name;

  // TODO - ports!

  // Each binding is a mapping from an SDL input to one or more InputBindings
  InputBindingGroups m_binds;

  // TODO - mouse? (sensitivities)
  // TODO - positive/negative keys
  // TODO - digital vs analog
  // bool m_buffer_inputs;  // TODO - not entirely sure what this means, test with old code (hold
  // onto inputs for a frame before firing?)

 public:
  virtual ~InputDevice(){};

  virtual void process_event(const SDL_Event& event, std::shared_ptr<PadData> data) = 0;
  virtual void close_device() = 0;
  virtual int update_rumble(const int port, const u8 low_rumble, const u8 high_rumble) = 0;

  bool is_loaded() const { return m_loaded; };
};

// TODO - move to it's own header file to keep this clean?
/// https://wiki.libsdl.org/SDL2/SDL_GameControllerButton
static const InputBindingGroups s_default_controller_binds = {
    {{SDL_CONTROLLER_AXIS_LEFTX, {InputBinding(PadData::AnalogIndex::LEFT_X)}},
     {SDL_CONTROLLER_AXIS_LEFTY, {InputBinding(PadData::AnalogIndex::LEFT_Y)}},
     {SDL_CONTROLLER_AXIS_RIGHTX, {InputBinding(PadData::AnalogIndex::RIGHT_X)}},
     {SDL_CONTROLLER_AXIS_RIGHTY, {InputBinding(PadData::AnalogIndex::RIGHT_Y)}}},
    {
        {SDL_CONTROLLER_AXIS_TRIGGERLEFT, {InputBinding(PadData::ButtonIndex::L2)}},
        {SDL_CONTROLLER_AXIS_TRIGGERRIGHT, {InputBinding(PadData::ButtonIndex::R2)}},
    },
    {{SDL_CONTROLLER_BUTTON_A, {InputBinding(PadData::ButtonIndex::CROSS)}},
     {SDL_CONTROLLER_BUTTON_B, {InputBinding(PadData::ButtonIndex::CIRCLE)}},
     {SDL_CONTROLLER_BUTTON_X, {InputBinding(PadData::ButtonIndex::SQUARE)}},
     {SDL_CONTROLLER_BUTTON_Y, {InputBinding(PadData::ButtonIndex::TRIANGLE)}},
     {SDL_CONTROLLER_BUTTON_LEFTSTICK, {InputBinding(PadData::ButtonIndex::L3)}},
     {SDL_CONTROLLER_BUTTON_RIGHTSTICK, {InputBinding(PadData::ButtonIndex::R3)}},
     {SDL_CONTROLLER_BUTTON_BACK, {InputBinding(PadData::ButtonIndex::SELECT)}},
     {SDL_CONTROLLER_BUTTON_START, {InputBinding(PadData::ButtonIndex::START)}},
     {SDL_CONTROLLER_BUTTON_LEFTSHOULDER, {InputBinding(PadData::ButtonIndex::L1)}},
     {SDL_CONTROLLER_BUTTON_RIGHTSHOULDER, {InputBinding(PadData::ButtonIndex::R1)}},
     {SDL_CONTROLLER_BUTTON_DPAD_UP, {InputBinding(PadData::ButtonIndex::DPAD_UP)}},
     {SDL_CONTROLLER_BUTTON_DPAD_DOWN, {InputBinding(PadData::ButtonIndex::DPAD_DOWN)}},
     {SDL_CONTROLLER_BUTTON_DPAD_LEFT, {InputBinding(PadData::ButtonIndex::DPAD_LEFT)}},
     {SDL_CONTROLLER_BUTTON_DPAD_RIGHT, {InputBinding(PadData::ButtonIndex::DPAD_RIGHT)}}}};

// https://wiki.libsdl.org/SDL2/CategoryGameController
class GameController : public InputDevice {
 public:
  GameController(int sdl_device_id, float analog_dead_zone = 0.3);
  ~GameController() { close_device(); }

  void process_event(const SDL_Event& event, std::shared_ptr<PadData> data) override;
  void close_device() override;
  int update_rumble(const int port, const u8 low_rumble, const u8 high_rumble) override;

 private:
  int m_sdl_instance_id = -1;
  SDL_GameController* m_device_handle;
  float m_analog_dead_zone;
};

// TODO - move to it's own header file to keep this clean?
/// https://wiki.libsdl.org/SDL2/SDL_Keycode
static const InputBindingGroups s_default_keyboard_binds = {
    {{SDLK_a, {InputBinding(PadData::AnalogIndex::LEFT_X, true)}},
     {SDLK_d, {InputBinding(PadData::AnalogIndex::LEFT_X)}},
     {SDLK_s, {InputBinding(PadData::AnalogIndex::LEFT_Y)}},
     {SDLK_w, {InputBinding(PadData::AnalogIndex::LEFT_Y, true)}},
     {SDLK_l, {InputBinding(PadData::AnalogIndex::RIGHT_X, true)}},
     {SDLK_j, {InputBinding(PadData::AnalogIndex::RIGHT_X)}},
     {SDLK_k, {InputBinding(PadData::AnalogIndex::RIGHT_Y)}},
     {SDLK_i, {InputBinding(PadData::AnalogIndex::RIGHT_Y, true)}}},
    {},
    {{SDLK_SPACE, {InputBinding(PadData::ButtonIndex::CROSS)}},
     {SDLK_f, {InputBinding(PadData::ButtonIndex::CIRCLE)}},
     {SDLK_e, {InputBinding(PadData::ButtonIndex::SQUARE)}},
     {SDLK_r, {InputBinding(PadData::ButtonIndex::TRIANGLE)}},
     {SDLK_COMMA, {InputBinding(PadData::ButtonIndex::L3)}},
     {SDLK_PERIOD, {InputBinding(PadData::ButtonIndex::R3)}},
     {SDLK_QUOTE, {InputBinding(PadData::ButtonIndex::SELECT)}},
     {SDLK_RETURN, {InputBinding(PadData::ButtonIndex::START)}},
     {SDLK_o, {InputBinding(PadData::ButtonIndex::L1)}},
     {SDLK_q, {InputBinding(PadData::ButtonIndex::R1)}},
     {SDLK_1, {InputBinding(PadData::ButtonIndex::L2)}},
     {SDLK_p, {InputBinding(PadData::ButtonIndex::R2)}},
     {SDLK_UP, {InputBinding(PadData::ButtonIndex::DPAD_UP)}},
     {SDLK_DOWN, {InputBinding(PadData::ButtonIndex::DPAD_DOWN)}},
     {SDLK_LEFT, {InputBinding(PadData::ButtonIndex::DPAD_LEFT)}},
     {SDLK_RIGHT, {InputBinding(PadData::ButtonIndex::DPAD_RIGHT)}}}};

class KeyboardDevice : public InputDevice {
 public:
  // TODO - load user binds
  KeyboardDevice() { m_binds = s_default_keyboard_binds; };
  ~KeyboardDevice() {}

  void process_event(const SDL_Event& event, std::shared_ptr<PadData> data) override;
  void close_device() override{
      // there is nothing to close
  };
  int update_rumble(const int port, const u8 low_rumble, const u8 high_rumble) override {
    return 0;
  };
};

static const InputBindingGroups s_default_mouse_binds = {{}, {}, {}};

class MouseDevice : public InputDevice {
 public:
  // TODO - load user binds
  MouseDevice() { m_binds = s_default_mouse_binds; };
  ~MouseDevice() {}

  void process_event(const SDL_Event& event, std::shared_ptr<PadData> data) override;
  void close_device() override{
      // there is nothing to close
  };
  int update_rumble(const int port, const u8 low_rumble, const u8 high_rumble) override {
    return 0;
  };

  std::pair<int, int> get_mouse_pos() { return {m_xcoord, m_ycoord}; }

 private:
  int m_xcoord = 0;
  int m_ycoord = 0;
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

  std::shared_ptr<PadData> get_current_data() const;
  int update_rumble(int port, u8 low_intensity, u8 high_intensity);
  void change_active_device(int device_id);
  std::pair<int, int> get_mouse_pos() { return m_mouse.get_mouse_pos(); }

 private:
  // TODO - actually need to be shared_ptrs? references should be fine right?
  std::vector<std::shared_ptr<InputDevice>> m_available_devices;
  std::shared_ptr<InputDevice> m_active_device;
  KeyboardDevice m_keyboard;
  MouseDevice m_mouse;
  std::shared_ptr<PadData> m_data;
};
