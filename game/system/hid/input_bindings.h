#pragma once

#include <array>
#include <functional>
#include <unordered_map>
#include <utility>

#include <common/common_types.h>

/// <summary>
/// A simple abstraction around the PS2 controller data with some convenience functions for
/// pulling specific data out if it's useful.
///
/// Pressure is always assumed to be the max, barely any input library handles pressure properly
/// and the VAST majority of controllers don't even support it either.
/// </summary>
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

// order of pressure sensitive buttons in memory (not the same as their bit order...).
extern const std::vector<PadData::ButtonIndex> PAD_DATA_PRESSURE_INDEX_ORDER;

/// <summary>
/// Contains all information needed when processing a host input (ie. from SDL)
/// For example -- for a keyboard binding it informs us what modifiers need to be hit at the same
/// time, etc <para> All bindings _must_ provide the PS2 button/analog index they map to
/// </para>
/// <para>
/// There is also a special case for binary inputs mapping to the analog sticks.  In such a
/// situation both keys will be modifying the same underlying value, but one will mutate the value
/// to the minimum of the range and the other to the maximum.  The key intended to map to the
/// minimum should specify `true` for `inverse_val`. Additionally, in order to handle the scenario
/// where both binds are triggered at the same time you must also provide the opposing bind -- it
/// will be simultaneously tested for and handled accordingly.
///
/// For example, pressing both W and S should result in Jak not moving. And then letting go of W
/// should make him move towards the camera.
/// </para>
/// </summary>
struct InputBinding {
  InputBinding(int index) : pad_data_index(index){};
  InputBinding(int index, bool _inverse_val) : pad_data_index(index), inverse_val(_inverse_val){};

  /// Corresponds to PadData::AnalogIndex or PadData::ButtonIndex
  int pad_data_index;

  /// If considered pressed, it will invert the value (ie, left/right on an analog stick)
  bool inverse_val = false;
  // https://wiki.libsdl.org/SDL2/SDL_Keymod
  bool need_shift = false;
  bool need_ctrl = false;
  bool need_meta = false;  // aka GUI / windows key
  bool need_alt = false;
};

struct InputBindingGroups {
  std::unordered_map<u8, std::vector<InputBinding>> analog_axii;
  std::unordered_map<u8, std::vector<InputBinding>> button_axii;
  std::unordered_map<u8, std::vector<InputBinding>> buttons;
};

/// https://wiki.libsdl.org/SDL2/SDL_GameControllerButton
extern const InputBindingGroups DEFAULT_CONTROLLER_BINDS;
/// https://wiki.libsdl.org/SDL2/SDL_Keycode
extern const InputBindingGroups DEFAULT_KEYBOARD_BINDS;
/// https://wiki.libsdl.org/SDL2/SDL_MouseButtonEvent
extern const InputBindingGroups DEFAULT_MOUSE_BINDS;

/// <summary>
/// A CommandBinding by contrast is a way to map some arbitrary runtime command to
/// a user initiated input.
///
/// These are not used to mutate the state of a PadData object and instead run
/// an arbitrary lambda (with no return value) when triggered.
///
/// An example of these would be taking a screenshot or save-state actions
/// </summary>
struct CommandBinding {
  enum Source { CONTROLLER, KEYBOARD, MOUSE };

  CommandBinding(const u8 _host_key, std::function<void()> _command)
      : host_key(_host_key), command(_command){};
  u8 host_key;
  std::function<void()> command;
  bool need_shift = false;
  bool need_ctrl = false;
  bool need_meta = false;  // aka GUI / windows key
  bool need_alt = false;
};

struct CommandBindingGroups {
  std::unordered_map<u8, std::vector<CommandBinding>> controller_binds;
  std::unordered_map<u8, std::vector<CommandBinding>> keyboard_binds;
  std::unordered_map<u8, std::vector<CommandBinding>> mouse_binds;
};
