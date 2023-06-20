#pragma once

#include "input_device.h"

class KeyboardDevice : public InputDevice {
 public:
  KeyboardDevice(){};
  KeyboardDevice(std::shared_ptr<game_settings::InputSettings> settings);
  ~KeyboardDevice() {}

  void process_event(const SDL_Event& event,
                     const CommandBindingGroups& commands,
                     std::shared_ptr<PadData> data,
                     std::optional<InputBindAssignmentMeta>& bind_assignment,
                     bool ignore_inputs = false) override;
  void close_device() override{
      // there is nothing to close
  };

  // When we assign a bind on a keydown, we have to ignore it's keyup event
  // for analog binds, or it will adjust the position
  std::optional<u32> m_ignore_key_on_keyup;
};
