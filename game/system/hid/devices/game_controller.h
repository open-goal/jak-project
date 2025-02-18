#pragma once

#include "dualsense_effects.h"
#include "input_device.h"

// https://wiki.libsdl.org/SDL3/CategoryGamepad
class GameController : public InputDevice {
 public:
  GameController(int sdl_device_id, std::shared_ptr<game_settings::InputSettings> settings);
  ~GameController() { close_device(); }

  void process_event(const SDL_Event& event,
                     const CommandBindingGroups& commands,
                     std::shared_ptr<PadData> data,
                     std::optional<InputBindAssignmentMeta>& bind_assignment) override;
  void close_device() override;
  int send_rumble(const u8 low_rumble, const u8 high_rumble);
  void send_trigger_rumble(const u16 left_rumble,
                           const u16 right_rumble,
                           const u32 duration_ms = 100);
  void clear_trigger_effect(dualsense_effects::TriggerEffectOption option);
  void send_trigger_effect_feedback(dualsense_effects::TriggerEffectOption option,
                                    u8 position,
                                    u8 strength);
  void send_trigger_effect_vibrate(dualsense_effects::TriggerEffectOption option,
                                   u8 position,
                                   u8 amplitude,
                                   u8 frequency);
  void send_trigger_effect_weapon(dualsense_effects::TriggerEffectOption option,
                                  u8 start_position,
                                  u8 end_position,
                                  u8 strength);
  std::string get_name() const { return m_device_name; }
  bool has_led() { return m_has_led; }
  bool has_rumble() { return m_has_rumble; }
  void set_led(const u8 red, const u8 green, const u8 blue);
  std::string get_guid() { return m_guid; }
  bool is_dualsense() { return m_is_dualsense; }
  bool has_trigger_rumble() { return m_has_trigger_rumble; }
  bool has_trigger_effect_support() { return has_trigger_rumble() || is_dualsense(); }
  bool has_pressure_sensitivity_support() { return m_has_pressure_sensitive_buttons; }

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
