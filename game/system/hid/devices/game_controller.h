#pragma once

#include "input_device.h"

// TODO:
//- new options:
//  - enable/disable pressure sensitivity (all games, disable if not supported)
//  - swap r1/r2 when using gun (jak 2)
//  - trigger effects (disable if not using a ps5/xbox1) (jak 2)
//- pressure sensitivity if PS3 controller and supported
//  - gotta use SXS (unknown for Linux, needs testing)
//- trigger effects:
//  - xbox1:
//    - small vibrate when collecting dark eco
//    - big vibrate when changing to dark jak
//    - vibrate when shooting gun, proportional to gun type
//  - ps5:
//    - resistance when changing hover zones (if gun isn't out or if swap disabled)
//    - resistance when changing to dark jak
//    - different gun shooting effects
//- test all the other things (fullscreening, etc)

// https://wiki.libsdl.org/SDL2/CategoryGameController
class GameController : public InputDevice {
 public:
  GameController(int sdl_device_id, std::shared_ptr<game_settings::InputSettings> settings);
  ~GameController() { close_device(); }

  void process_event(const SDL_Event& event,
                     const CommandBindingGroups& commands,
                     std::shared_ptr<PadData> data,
                     std::optional<InputBindAssignmentMeta>& bind_assignment) override;
  void close_device() override;
  int update_rumble(const u8 low_rumble, const u8 high_rumble);
  std::string get_name() const { return m_device_name; }
  bool has_led() { return m_has_led; }
  bool has_rumble() { return m_has_rumble; }
  void set_led(const u8 red, const u8 green, const u8 blue);
  std::string get_guid() { return m_guid; }
  bool is_dualsense() { return m_is_dualsense; }
  bool has_trigger_rumble() { return m_has_trigger_rumble; }

 private:
  int m_sdl_instance_id = -1;
  SDL_Gamepad* m_device_handle;
  SDL_Joystick* m_low_device_handle;
  std::string m_device_name = "";
  bool m_has_led;
  bool m_has_rumble;
  std::string m_guid = "";
  bool m_has_pressure_sensitive_buttons = false;
  bool m_is_dualsense;
  bool m_has_trigger_rumble;
};
