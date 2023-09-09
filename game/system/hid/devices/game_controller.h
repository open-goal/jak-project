#pragma once

#include "input_device.h"

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

 private:
  int m_sdl_instance_id = -1;
  SDL_GameController* m_device_handle;
  std::string m_device_name = "";
  bool m_has_led;
  bool m_has_rumble;
  std::string m_guid = "";
};
