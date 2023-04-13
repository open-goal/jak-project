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
                     std::optional<InputBindAssignmentMeta>& bind_assignment) override;
  void close_device() override{
      // there is nothing to close
  };
};
