#pragma once

#include <array>
#include <functional>
#include <optional>
#include <string>
#include <unordered_map>
#include <utility>

#include "common/common_types.h"
#include "common/log/log.h"
#include "common/util/json_util.h"

/// A simple abstraction around the PS2 controller data with some convenience functions for
/// pulling specific data out if it's useful.
///
/// Pressure is always assumed to be the max, barely any input library handles pressure properly
/// and the VAST majority of controllers don't even support it either.
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

  static const int ANALOG_NEUTRAL = 127;

  // NOTE - store analog values as larger signed integers and then clamp them to their 0-255
  // u8 range. This is to make it easier to properly handle multiple non-analog sources attempting
  // to simulate analog sticks
  //
  // Imagine you have 100 keys bound to moving forward and back, and all those key up / key down
  // events are encountered asynchrously, only when their state changes
  //
  // There are a lot of strategies to handle this:
  // - you can delay applying the input by one frame, so you have a wholistic viewpoint.  But this
  // delay is undesirable and doesn't work elegantly with multiple input sources
  // - you can have a bunch of complicated state overseeing it all
  // - or (with this approach) you can process the events asynchronously, adding and subtracting
  // from the aggregate amount and organically end up with the correct value
  //
  // For a tangible example, imagine you have `W` bound to move forward, as well as left click
  // - subtract `127` when `W` is pressed = 0 (127 is neutral)
  // - subtract `127` when `Mouse1` is pressed = -127 (clamp to 0 for the game)
  // - add `127` when `W` is released = 0
  // - add `127` when `Mouse1` is released = 127 (back to neutral)
  //
  // Because keys that are pressed, must eventually be released -- this has a fairly strong
  // guarantee.  About the only hole is if you decided to unplug your keyboard while holding a key,
  // which is something we can distinctly detect and handle with a decent compromise (reset the
  // inputs)
  std::array<int, 4> analog_data = {ANALOG_NEUTRAL, ANALOG_NEUTRAL, ANALOG_NEUTRAL, ANALOG_NEUTRAL};

  std::pair<u8, u8> analog_left() const {
    return {std::clamp(analog_data.at(AnalogIndex::LEFT_X), 0, 255),
            std::clamp(analog_data.at(AnalogIndex::LEFT_Y), 0, 255)};
  }
  std::pair<u8, u8> analog_right() const {
    return {std::clamp(analog_data.at(AnalogIndex::RIGHT_X), 0, 255),
            std::clamp(analog_data.at(AnalogIndex::RIGHT_Y), 0, 255)};
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

  // Analog Simulation Tracking
  // There exists a flaw with the described analog tracking approach described above, and that has
  // to do with imprecise analog drift / fluctuating numbers.
  //
  // If you switch between a keyboard and a controller, sometimes the controllers analog stick can
  // oscillate between values triggering frivolous events.  This impacts the usage of the
  // alternative input device such as the keyboard or mouse
  //
  // To solve this, we keep track of if the user is actively using the keyboard or mouse to
  // manipulate the analog values and if they are, we can ignore controller analog inputs.  Once all
  // inputs are released the controller input can resume.
 private:
  int analog_sim_tracker = 0;

 public:
  bool analogs_being_simulated() { return analog_sim_tracker > 0; }
  void update_analog_sim_tracker(bool released) {
    if (released) {
      analog_sim_tracker--;
    } else {
      analog_sim_tracker++;
    }
    if (analog_sim_tracker < 0) {
      analog_sim_tracker = 0;
    }
  }

  void clear() {
    for (auto& x : button_data) {
      x = 0;
    }
    clear_analogs();
  }

  void clear_analogs() {
    for (auto& x : analog_data) {
      x = ANALOG_NEUTRAL;
    }
    analog_sim_tracker = 0;
  }
};

// order of pressure sensitive buttons in memory (not the same as their bit order...).
extern const std::vector<PadData::ButtonIndex> PAD_DATA_PRESSURE_INDEX_ORDER;

// https://wiki.libsdl.org/SDL2/SDL_Keymod
struct InputModifiers {
  InputModifiers() = default;
  InputModifiers(const u16 sdl_mod_state);

  bool need_shift = false;
  bool need_ctrl = false;
  bool need_meta = false;  // aka GUI / windows key
  bool need_alt = false;

  bool has_necessary_modifiers(const u16 key_modifiers) const;

  bool operator==(const InputModifiers& other) const {
    if (need_shift == other.need_shift && need_ctrl == other.need_ctrl &&
        need_meta == other.need_meta && need_alt == other.need_alt) {
      return true;
    }
    return false;
  }
};

void to_json(json& j, const InputModifiers& obj);
void from_json(const json& j, InputModifiers& obj);

/// Contains all information needed when processing a host input (ie. from SDL)
/// For example -- for a keyboard binding it informs us what modifiers need to be hit at the same
/// time, etc
///
/// All bindings _must_ provide the PS2 button/analog index they map to
///
/// There is also a special case for binary inputs mapping to the analog sticks.  In such a
/// situation both keys will be modifying the same underlying value, but one will mutate the value
/// to the minimum of the range and the other to the maximum.  The key intended to map to the
/// minimum should specify `true` for `minimum_in_range`.
///
/// For example, pressing both W and S should result in Jak not moving. And then letting go of W
/// should make him move towards the camera.
struct InputBinding {
  InputBinding() = default;
  InputBinding(int index) : pad_data_index(index){};
  InputBinding(int index, const std::optional<InputModifiers> _modifiers) : pad_data_index(index) {
    if (_modifiers) {
      modifiers = _modifiers.value();
    }
  };
  InputBinding(int index, bool _minimum_in_range)
      : pad_data_index(index), minimum_in_range(_minimum_in_range){};
  InputBinding(int index, bool _minimum_in_range, const std::optional<InputModifiers> _modifiers)
      : pad_data_index(index), minimum_in_range(_minimum_in_range) {
    if (_modifiers) {
      modifiers = _modifiers.value();
    }
  };

  /// Corresponds to PadData::AnalogIndex or PadData::ButtonIndex
  int pad_data_index;

  /// If considered pressed, it will invert the value (ie, left/right on an analog stick)
  bool minimum_in_range = false;
  InputModifiers modifiers;
};

void to_json(json& j, const InputBinding& obj);
void from_json(const json& j, InputBinding& obj);

enum InputDeviceType { CONTROLLER = 0, KEYBOARD = 1, MOUSE = 2 };

struct InputBindingInfo {
  s32 sdl_idx;
  u32 pad_idx;
  std::string host_name;
  bool analog_button;
  InputModifiers modifiers;

  InputBindingInfo() = default;
  InputBindingInfo(const InputBinding bind,
                   const InputDeviceType device_type,
                   const s32 sdl_code,
                   const bool analog_button);
};

// Contains all info related to the current binding we are waiting for
// so the relevant device can successfully apply it
struct InputBindAssignmentMeta {
  InputDeviceType device_type = InputDeviceType::CONTROLLER;
  int pad_idx;
  bool for_analog = false;
  bool for_analog_minimum = false;

  // For only some rebindings, we have to know what keys were originally bound to the
  // confirmation buttons this is because the user has to hit said key to initiate waiting for the
  // new assignment
  //
  // For most input sources this doesn't matter because we listen for the DOWN event, but in order
  // to allow modifiers as binds (ie. Shift for X) we have to listen to UP events as well (only for
  // the modifiers).  This is also relevant for analog rebinds as the transition from fully pressed
  // to unpressed triggers another press.
  //
  // TLDR - we ignore the first UP event if it was bound to a confirmation key.  Additionally, this
  // depends on the game as Jak 1 treats X or O as a confirm key...
  std::vector<InputBindingInfo> keyboard_confirmation_binds = {};
  bool seen_keyboard_confirm_up = false;
  std::vector<InputBindingInfo> controller_confirmation_binds = {};
  bool seen_controller_confirm_neutral = false;

  // Indicates the binding has been received, assigned, and we can proceed.
  bool assigned = false;
};

struct InputBindingGroups {
  InputBindingGroups() = default;
  InputBindingGroups(const InputDeviceType _device_type,
                     std::unordered_map<u32, std::vector<InputBinding>> _analog_axii,
                     std::unordered_map<u32, std::vector<InputBinding>> _button_axii,
                     std::unordered_map<u32, std::vector<InputBinding>> _buttons)
      : device_type(_device_type),
        analog_axii(_analog_axii),
        button_axii(_button_axii),
        buttons(_buttons){};

  // TODO - make these private
  InputDeviceType device_type;
  std::unordered_map<u32, std::vector<InputBinding>> analog_axii;
  std::unordered_map<u32, std::vector<InputBinding>> button_axii;
  std::unordered_map<u32, std::vector<InputBinding>> buttons;

  std::vector<InputBindingInfo> lookup_analog_binds(PadData::AnalogIndex idx,
                                                    bool only_minimum_binds = false);
  std::vector<InputBindingInfo> lookup_button_binds(PadData::ButtonIndex idx);

  void assign_analog_bind(u32 sdl_idx,
                          InputBindAssignmentMeta& bind_meta,
                          const std::optional<InputModifiers> modifiers = {});
  void assign_button_bind(u32 sdl_idx,
                          InputBindAssignmentMeta& bind_meta,
                          const bool analog_button = false,
                          const std::optional<InputModifiers> modifiers = {});

  void set_bindings(const InputBindingGroups& binds);

 private:
  typedef std::pair<int, bool> BindCacheKey;

  struct hash_name {
    size_t operator()(const BindCacheKey& key) const {
      return std::hash<int>()(key.first) ^ std::hash<bool>()(key.second);
    }
  };

  // These are caches for reverse-lookups (from the mapped bind instead of the host bind)
  // for reading inputs we keep things fast -- we start with an SDL host value and map it to the
  // required binds
  //
  // However there are some situations where we want to the reverse -- find out what binds
  // correspond with the PS2 value. Such as when remapping a key so you can unbind overlapping binds
  std::unordered_map<BindCacheKey, std::vector<InputBindingInfo>, hash_name> m_analog_lookup;
  std::unordered_map<BindCacheKey, std::vector<InputBindingInfo>, hash_name> m_button_lookup;
  // The underlying data structures support multiple binds for the same input, but the UX doesn't
  // so we have to wipe out any shared bindings after an assignment
  void remove_multiple_binds(u32 sdl_idx,
                             InputBindAssignmentMeta& bind_meta,
                             std::unordered_map<u32, std::vector<InputBinding>>& bind_map);
  std::optional<std::pair<InputBinding, bool>> find_button_bind_from_sdl_idx(
      u32 sdl_idx,
      const std::optional<InputModifiers> modifiers);
  std::optional<std::pair<InputBinding, bool>> find_analog_bind_from_sdl_idx(
      u32 sdl_idx,
      const std::optional<InputModifiers> modifiers);
};

void to_json(json& j, const InputBindingGroups& obj);
void from_json(const json& j, InputBindingGroups& obj);

/// https://wiki.libsdl.org/SDL2/SDL_GameControllerButton
extern const InputBindingGroups DEFAULT_CONTROLLER_BINDS;
/// https://wiki.libsdl.org/SDL2/SDL_Keycode
extern const InputBindingGroups DEFAULT_KEYBOARD_BINDS;
/// https://wiki.libsdl.org/SDL2/SDL_MouseButtonEvent
extern const InputBindingGroups DEFAULT_MOUSE_BINDS;

/// A CommandBinding by contrast is a way to map some arbitrary runtime command to
/// a user initiated input.
///
/// These are not used to mutate the state of a PadData object and instead run
/// an arbitrary lambda (with no return value) when triggered.
///
/// An example of these would be taking a screenshot or save-state actions
// TODO - there is currently a bad UX if commands overlap with user bindings.  For example if "F2"
// is for screenshots and the user binds that to "X" it will work, but you're going to take a
// screenshot everytime you jump.
//
// We probably don't want that but fundamentally this is a problem because the commands are
// hard-coded and not customizable so even if we prevented such binds -- there would not be a good
// user-facing reason why the bind failed to take.
//
// So there are some potential solutions but this doesn't feel high priority and this was always an
// issue.
struct CommandBinding {
  enum Source { CONTROLLER, KEYBOARD, MOUSE };

  CommandBinding(const u32 _host_key, std::function<void()> _command)
      : host_key(_host_key), command(_command){};
  u32 host_key;
  std::function<void()> command;
  InputModifiers modifiers;
};

struct CommandBindingGroups {
  std::unordered_map<u32, std::vector<CommandBinding>> controller_binds;
  std::unordered_map<u32, std::vector<CommandBinding>> keyboard_binds;
  std::unordered_map<u32, std::vector<CommandBinding>> mouse_binds;
};
