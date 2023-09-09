#pragma once

#include <optional>

#include "game/settings/settings.h"
#include "game/system/hid/input_bindings.h"

#include "third-party/SDL/include/SDL.h"

// A distinct input device.  Only those devices that are "active" should be read
class InputDevice {
 protected:
  bool m_loaded = false;
  std::shared_ptr<game_settings::InputSettings> m_settings;

 public:
  virtual ~InputDevice(){};

  virtual void process_event(const SDL_Event& event,
                             const CommandBindingGroups& commands,
                             std::shared_ptr<PadData> data,
                             std::optional<InputBindAssignmentMeta>& bind_assignment) = 0;
  virtual void close_device() = 0;
  bool is_loaded() const { return m_loaded; };
};
