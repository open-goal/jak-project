#pragma once

#include "input_device.h"

class MouseDevice : public InputDevice {
 public:
  struct MouseButtonStatus {
    bool left;
    bool right;
    bool middle;
    bool mouse4;
    bool mouse5;
  };

  MouseDevice(){};
  MouseDevice(std::shared_ptr<game_settings::InputSettings> settings);
  ~MouseDevice() {}

  void process_event(const SDL_Event& event,
                     const CommandBindingGroups& commands,
                     std::shared_ptr<PadData> data,
                     std::optional<InputBindAssignmentMeta>& bind_assignment,
                     bool ignore_inputs = false) override;
  void close_device() override{
      // there is nothing to close
  };

  void enable_relative_mode(const bool enable);
  void enable_camera_control(const bool enable);
  void enable_movement_control(const bool enable) { m_control_movement = enable; }
  std::pair<int, int> get_mouse_pos() const { return {m_xcoord, m_ycoord}; }
  MouseButtonStatus get_mouse_button_status() const { return m_button_status; }
  void set_camera_sens(const float xsens, const float ysens);

 private:
  int m_xcoord = 0;
  int m_ycoord = 0;
  MouseButtonStatus m_button_status;

  bool m_control_camera = false;
  bool m_control_movement = false;
  bool m_was_moving_with_mouse = false;
  float m_xsens = -15.0;
  float m_ysens = 10.0;
};
