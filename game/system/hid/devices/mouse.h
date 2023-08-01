#pragma once

#include "input_device.h"

// See ActiveKeyboardAction, same idea slightly different data
struct ActiveMouseAction {
  u32 sdl_mouse_button;
  InputBinding binding;
  bool player_movement =
      false;  // A special one for the mouse, no related binding, hard-coded behaviour!
  std::function<void(std::shared_ptr<PadData>, InputBinding bind)> revert_action;
};

class MouseDevice : public InputDevice {
 public:
  struct MouseButtonStatus {
    bool left = false;
    bool right = false;
    bool middle = false;
    bool mouse4 = false;
    bool mouse5 = false;
  };

  MouseDevice(){};
  MouseDevice(std::shared_ptr<game_settings::InputSettings> settings);
  ~MouseDevice() {}

  void poll_state(std::shared_ptr<PadData> data);
  void clear_actions(std::shared_ptr<PadData> data);
  void process_event(const SDL_Event& event,
                     const CommandBindingGroups& commands,
                     std::shared_ptr<PadData> data,
                     std::optional<InputBindAssignmentMeta>& bind_assignment) override;
  void close_device() override{
      // there is nothing to close
  };

  void enable_relative_mode(const bool enable);
  void enable_camera_control(const bool enable);
  void enable_movement_control(const bool enable) { m_control_movement = enable; }
  std::pair<int, int> get_mouse_pos() const { return {m_xcoord, m_ycoord}; }
  MouseButtonStatus get_mouse_button_status() const { return m_button_status; }
  void set_camera_sens(const float xsens, const float ysens);
  bool is_camera_being_controlled() { return m_control_camera; }

 private:
  std::vector<ActiveMouseAction> m_active_actions = {};

  // Track the state of mouse for Game reasons
  int m_xcoord = 0;
  int m_ycoord = 0;
  MouseButtonStatus m_button_status;

  bool m_control_camera = false;
  bool m_control_movement = false;
  float m_xsens = -15.0;
  float m_ysens = 10.0;

  bool is_action_already_active(const u32 sdl_keycode, const bool player_movement);
};
