#include "input_manager.h"

#include <atomic>
#include <cmath>

#include "input_manager.h"
#include "sdl_util.h"

#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/util/FileUtil.h"

#include "game/graphics/pipelines/opengl.h"
#include "game/runtime.h"

#include "third-party/imgui/imgui.h"

InputManager::InputManager()
    // Load user settings
    : m_settings(std::make_shared<game_settings::InputSettings>(game_settings::InputSettings())) {
  // Update to latest controller DB file
  std::string mapping_path =
      (file_util::get_jak_project_dir() / "game" / "assets" / "sdl_controller_db.txt").string();
  if (file_util::file_exists(mapping_path)) {
    SDL_GameControllerAddMappingsFromFile(mapping_path.c_str());
  } else {
    lg::error("Could not find SDL Controller DB at path `{}`", mapping_path);
  }
  // Initialize atleast 2 ports, because that's normal for Jak
  // more will be allocated if more controllers are found
  m_data[0] = std::make_shared<PadData>();
  m_data[1] = std::make_shared<PadData>();
  m_keyboard = KeyboardDevice(m_settings);
  m_mouse = MouseDevice(m_settings);

  if (m_data.find(m_keyboard_and_mouse_port) == m_data.end()) {
    m_data[m_keyboard_and_mouse_port] = std::make_shared<PadData>();
  }
  m_command_binds = CommandBindingGroups();
  refresh_device_list();
  ignore_background_controller_events(false);
  hide_cursor(m_auto_hide_mouse);
}

InputManager::~InputManager() {
  for (auto& device : m_available_controllers) {
    device->close_device();
  }
  m_settings->save_settings();
}

void InputManager::refresh_device_list() {
  m_available_controllers.clear();
  m_controller_port_mapping.clear();
  // Enumerate devices
  // TODO - if this was done on a separate thread, there would be no hitch in the game thread
  // but of course, that presents other synchronization challenges.
  const auto num_joysticks = SDL_NumJoysticks();
  if (num_joysticks > 0) {
    for (int i = 0; i < num_joysticks; i++) {
      if (!SDL_IsGameController(i)) {
        lg::error("Controller with device id {} is not avaiable via the GameController API", i);
        continue;
      }
      auto controller = std::make_shared<GameController>(i, m_settings);
      if (!controller->is_loaded()) {
        lg::error("Unable to successfully connect to GameController with id {}, skipping", i);
        continue;
      }
      m_available_controllers.push_back(controller);
      // By default, controller port mapping is on a first-come-first-served basis
      //
      // However, we will use previously saved controller port mappings to take precedence
      // For example, if you previous set your PS5 controller to be port 0, then even
      // if another controller is detected first, the PS5 controller should be assigned as expected.
      if (m_settings->controller_port_mapping.find(controller->get_guid()) !=
          m_settings->controller_port_mapping.end()) {
        // Though it's possible for a user to assign multiple controllers to the same port, so the
        // last one wins
        m_controller_port_mapping[m_settings->controller_port_mapping.at(controller->get_guid())] =
            i;
      } else {
        m_controller_port_mapping[m_available_controllers.size() - 1] = i;
        m_settings->controller_port_mapping[controller->get_guid()] =
            m_available_controllers.size() - 1;
      }
      // Allocate a PadData if this is a new port
      if (m_data.find(i) == m_data.end()) {
        m_data[i] = std::make_shared<PadData>();
      }
    }
    // If the controller that was last selected to be port 0 is around, prioritize it
    if (!m_settings->last_selected_controller_guid.empty()) {
      for (size_t i = 0; i < m_available_controllers.size(); i++) {
        const auto& controller_guid = m_available_controllers.at(i)->get_guid();
        if (controller_guid == m_settings->last_selected_controller_guid) {
          m_controller_port_mapping[0] = i;
          m_settings->controller_port_mapping[controller_guid] = 0;
          break;
        }
      }
    }
  }
  if (m_available_controllers.empty()) {
    lg::warn(
        "No active game controllers could be found or loaded successfully - inputs will not work!");
  } else {
    lg::info("Found {} controllers", m_available_controllers.size());
  }
}

void InputManager::enqueue_ignore_background_controller_events(const bool ignore) {
  const std::lock_guard<std::mutex> lock(m_event_queue_mtx);
  ee_event_queue.push({EEInputEventType::IGNORE_BACKGROUND_CONTROLLER_EVENTS, ignore, {}, {}, {}});
}

void InputManager::ignore_background_controller_events(const bool ignore) {
  m_ignore_background_controller_events = ignore;
  // TODO - ignoring return value (atleast log it)
  SDL_SetHint(SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS, ignore ? "0" : "1");
}

void InputManager::hide_cursor(const bool hide_cursor) {
  if (hide_cursor == m_mouse_currently_hidden) {
    return;
  }
  // NOTE - seems like an SDL bug, but the cursor will be visible / locked to the center of the
  // screen if you use the 'start menu' to exit the window / return to it (atleast in windowed mode)
  auto ok = SDL_ShowCursor(hide_cursor ? SDL_DISABLE : SDL_ENABLE);
  if (ok < 0) {
    sdl_util::log_error("Unable to show/hide mouse cursor");
  } else {
    m_mouse_currently_hidden = hide_cursor;
  }
}

void InputManager::process_sdl_event(const SDL_Event& event) {
  // Detect controller connections and disconnects
  if (sdl_util::is_any_event_type(event.type,
                                  {SDL_CONTROLLERDEVICEADDED, SDL_CONTROLLERDEVICEREMOVED})) {
    lg::info("Controller added or removed. refreshing controller device list");
    refresh_device_list();
  }

  if (m_data.find(m_keyboard_and_mouse_port) != m_data.end()) {
    m_keyboard.process_event(event, m_command_binds, m_data.at(m_keyboard_and_mouse_port),
                             m_waiting_for_bind);
    m_mouse.process_event(event, m_command_binds, m_data.at(m_keyboard_and_mouse_port),
                          m_waiting_for_bind);
  }

  // Send event to active controller device
  // This goes last so it takes precedence
  for (const auto& [port, controller_idx] : m_controller_port_mapping) {
    if (m_data.find(port) != m_data.end() && (int)m_available_controllers.size() > controller_idx) {
      m_available_controllers.at(controller_idx)
          ->process_event(event, m_command_binds, m_data.at(port), m_waiting_for_bind);
    }
  }

  // Clear the binding assignment if we got one
  if (m_waiting_for_bind && m_waiting_for_bind->assigned) {
    stop_waiting_for_bind();
    // NOTE - this is a total hack, but it's to prevent immediately re-assigning the "confirmation"
    // bind if you use a source that is polled
    // TODO: There's a correct way to do this....figure it out eventually
    m_skip_polling_for_n_frames = 60;
  }

  // Adjust mouse cursor visibility
  if (m_auto_hide_mouse) {
    if (event.type == SDL_MOUSEMOTION && !m_mouse.is_camera_being_controlled()) {
      hide_cursor(false);
    } else if (event.type == SDL_KEYDOWN || event.type == SDL_CONTROLLERBUTTONDOWN) {
      hide_cursor(true);
    }
  }
}

void InputManager::poll_keyboard_data() {
  if (m_keyboard_enabled && m_skip_polling_for_n_frames <= 0 && !m_waiting_for_bind) {
    if (m_data.find(m_keyboard_and_mouse_port) != m_data.end()) {
      m_keyboard.poll_state(m_data.at(m_keyboard_and_mouse_port));
    }
  }
}

void InputManager::clear_keyboard_actions() {
  if (m_keyboard_enabled) {
    if (m_data.find(m_keyboard_and_mouse_port) != m_data.end()) {
      m_keyboard.clear_actions(m_data.at(m_keyboard_and_mouse_port));
    }
  }
}

void InputManager::poll_mouse_data() {
  if (m_mouse_enabled && m_skip_polling_for_n_frames <= 0 && !m_waiting_for_bind) {
    if (m_data.find(m_keyboard_and_mouse_port) != m_data.end()) {
      m_mouse.poll_state(m_data.at(m_keyboard_and_mouse_port));
    }
  }
}

void InputManager::clear_mouse_actions() {
  if (m_mouse_enabled && !m_waiting_for_bind) {
    if (m_data.find(m_keyboard_and_mouse_port) != m_data.end()) {
      m_mouse.clear_actions(m_data.at(m_keyboard_and_mouse_port));
    }
  }
}

void InputManager::finish_polling() {
  m_skip_polling_for_n_frames--;
  if (m_skip_polling_for_n_frames < 0) {
    m_skip_polling_for_n_frames = 0;
  }
}

void InputManager::process_ee_events() {
  const std::lock_guard<std::mutex> lock(m_event_queue_mtx);
  // Fully process any events from the EE
  while (!ee_event_queue.empty()) {
    const auto& evt = ee_event_queue.front();
    switch (evt.type) {
      case EEInputEventType::IGNORE_BACKGROUND_CONTROLLER_EVENTS:
        ignore_background_controller_events(std::get<bool>(evt.param1));
        break;
      case EEInputEventType::UPDATE_RUMBLE:
        update_rumble(std::get<int>(evt.param1), std::get<u8>(evt.param2),
                      std::get<u8>(evt.param3));
        break;
      case EEInputEventType::SET_CONTROLLER_LED:
        set_controller_led(std::get<int>(evt.param1), std::get<u8>(evt.param2),
                           std::get<u8>(evt.param3), std::get<u8>(evt.param4));
        break;
      case EEInputEventType::UPDATE_MOUSE_OPTIONS:
        update_mouse_options(std::get<bool>(evt.param1), std::get<bool>(evt.param2),
                             std::get<bool>(evt.param3));
        break;
      case EEInputEventType::SET_AUTO_HIDE_MOUSE:
        set_auto_hide_mouse(std::get<bool>(evt.param1));
        break;
    }
    ee_event_queue.pop();
  }
}

void InputManager::register_command(const CommandBinding::Source source,
                                    const CommandBinding bind) {
  switch (source) {
    case CommandBinding::Source::CONTROLLER:
      if (m_command_binds.controller_binds.find(bind.host_key) ==
          m_command_binds.controller_binds.end()) {
        m_command_binds.controller_binds[bind.host_key] = {};
      }
      m_command_binds.controller_binds[bind.host_key].push_back(bind);
      break;
    case CommandBinding::Source::KEYBOARD:
      if (m_command_binds.keyboard_binds.find(bind.host_key) ==
          m_command_binds.keyboard_binds.end()) {
        m_command_binds.keyboard_binds[bind.host_key] = {};
      }
      m_command_binds.keyboard_binds[bind.host_key].push_back(bind);
      break;
    case CommandBinding::Source::MOUSE:
      if (m_command_binds.mouse_binds.find(bind.host_key) == m_command_binds.mouse_binds.end()) {
        m_command_binds.mouse_binds[bind.host_key] = {};
      }
      m_command_binds.mouse_binds[bind.host_key].push_back(bind);
      break;
  }
}

std::optional<std::shared_ptr<PadData>> InputManager::get_current_data(const int port) const {
  if (m_data.find(port) == m_data.end()) {
    return {};
  }
  return m_data.at(port);
}

std::string InputManager::get_controller_name(const int controller_id) {
  if ((size_t)controller_id >= m_available_controllers.size()) {
    return "";
  }
  return m_available_controllers.at(controller_id)->get_name();
}

std::string InputManager::get_current_bind(const int port,
                                           const InputDeviceType device_type,
                                           const bool buttons,
                                           const int input_idx,
                                           const bool analog_for_minimum) {
  std::vector<InputBindingInfo> binding_info;
  switch (device_type) {
    case InputDeviceType::CONTROLLER:
      if (m_controller_port_mapping.find(port) != m_controller_port_mapping.end() &&
          m_controller_port_mapping.at(port) < (int)m_available_controllers.size() &&
          m_settings->controller_binds.find(
              m_available_controllers.at(m_controller_port_mapping.at(port))->get_guid()) !=
              m_settings->controller_binds.end()) {
        binding_info =
            m_settings->controller_binds
                .at(m_available_controllers.at(m_controller_port_mapping.at(port))->get_guid())
                .lookup_button_binds((PadData::ButtonIndex)input_idx);
      }
      break;
    case InputDeviceType::KEYBOARD:
      if (!buttons) {
        binding_info = m_settings->keyboard_binds.lookup_analog_binds(
            (PadData::AnalogIndex)input_idx, analog_for_minimum);
      } else {
        binding_info =
            m_settings->keyboard_binds.lookup_button_binds((PadData::ButtonIndex)input_idx);
      }
      break;
    case InputDeviceType::MOUSE:
      binding_info = m_settings->mouse_binds.lookup_button_binds((PadData::ButtonIndex)input_idx);
      break;
  }
  if (binding_info.empty()) {
    return "";
  }
  return binding_info.front().host_name;
}

void InputManager::set_controller_for_port(const int controller_id, const int port) {
  if (controller_id < (int)m_available_controllers.size()) {
    // Reset inputs as this device won't be able to be read from again!
    clear_inputs();
    auto& controller = m_available_controllers.at(controller_id);
    m_controller_port_mapping[port] = controller_id;
    m_settings->controller_port_mapping[controller->get_guid()] = port;
    // NOTE - only tracking port 0 for now
    if (port == 0) {
      m_settings->last_selected_controller_guid = controller->get_guid();
    }
    m_settings->save_settings();
  }
}

bool InputManager::controller_has_led(const int port) {
  if (m_controller_port_mapping.find(port) == m_controller_port_mapping.end()) {
    return false;
  }
  const auto id = m_controller_port_mapping.at(port);
  if (id >= (int)m_available_controllers.size()) {
    return false;
  }
  return m_available_controllers.at(id)->has_led();
}

bool InputManager::controller_has_rumble(const int port) {
  if (m_controller_port_mapping.find(port) == m_controller_port_mapping.end()) {
    return false;
  }
  const auto id = m_controller_port_mapping.at(port);
  if (id >= (int)m_available_controllers.size()) {
    return false;
  }
  return m_available_controllers.at(id)->has_rumble();
}

void InputManager::enqueue_set_controller_led(const int port,
                                              const u8 red,
                                              const u8 green,
                                              const u8 blue) {
  const std::lock_guard<std::mutex> lock(m_event_queue_mtx);
  ee_event_queue.push({EEInputEventType::SET_CONTROLLER_LED, port, red, green, blue});
}

void InputManager::set_controller_led(const int port, const u8 red, const u8 green, const u8 blue) {
  if (m_controller_port_mapping.find(port) == m_controller_port_mapping.end()) {
    return;
  }
  const auto id = m_controller_port_mapping.at(port);
  if (id >= (int)m_available_controllers.size()) {
    return;
  }
  m_available_controllers.at(id)->set_led(red, green, blue);
}

void InputManager::enqueue_update_rumble(const int port,
                                         const u8 low_intensity,
                                         const u8 high_intensity) {
  const std::lock_guard<std::mutex> lock(m_event_queue_mtx);
  ee_event_queue.push({EEInputEventType::UPDATE_RUMBLE, port, low_intensity, high_intensity, {}});
}

int InputManager::update_rumble(int port, u8 low_intensity, u8 high_intensity) {
  if (m_controller_port_mapping.find(port) == m_controller_port_mapping.end()) {
    return 0;
  }
  return m_available_controllers.at(m_controller_port_mapping.at(port))
      ->update_rumble(low_intensity, high_intensity);
}

void InputManager::enable_keyboard(const bool enabled) {
  m_keyboard_enabled = enabled;
  if (!m_keyboard_enabled) {
    // Reset inputs as this device won't be able to be read from again!
    clear_inputs();
  }
}

void InputManager::enqueue_update_mouse_options(const bool enabled,
                                                const bool control_camera,
                                                const bool control_movement) {
  const std::lock_guard<std::mutex> lock(m_event_queue_mtx);
  ee_event_queue.push(
      {EEInputEventType::UPDATE_MOUSE_OPTIONS, enabled, control_camera, control_movement, {}});
}

void InputManager::update_mouse_options(const bool enabled,
                                        const bool control_camera,
                                        const bool control_movement) {
  m_mouse_enabled = enabled;
  if (!m_mouse_enabled) {
    // Reset inputs as this device won't be able to be read from again!
    clear_inputs();
  }
  // Switch relevant flags over related to the mouse
  m_mouse.enable_camera_control(enabled && control_camera);
  m_mouse.enable_movement_control(enabled && control_movement);
}

void InputManager::set_wait_for_bind(const InputDeviceType device_type,
                                     const bool for_analog,
                                     const bool for_minimum_analog,
                                     const int input_idx) {
  m_waiting_for_bind = InputBindAssignmentMeta();
  m_waiting_for_bind->device_type = device_type;
  m_waiting_for_bind->pad_idx = input_idx;
  m_waiting_for_bind->for_analog = for_analog;
  m_waiting_for_bind->for_analog_minimum = for_minimum_analog;
  m_waiting_for_bind->keyboard_confirmation_binds =
      m_settings->keyboard_binds.lookup_button_binds(PadData::CROSS);
  if (g_game_version == GameVersion::Jak1) {
    auto circle_binds = m_settings->keyboard_binds.lookup_button_binds(PadData::CIRCLE);
    m_waiting_for_bind->keyboard_confirmation_binds.insert(
        m_waiting_for_bind->keyboard_confirmation_binds.end(), circle_binds.begin(),
        circle_binds.end());
  }
}

void InputManager::set_camera_sens(const float xsens, const float ysens) {
  m_mouse.set_camera_sens(xsens, ysens);
}

void InputManager::reset_input_bindings_to_defaults(const int port,
                                                    const InputDeviceType device_type) {
  switch (device_type) {
    case InputDeviceType::CONTROLLER:
      if (m_controller_port_mapping.find(port) != m_controller_port_mapping.end() &&
          m_controller_port_mapping.at(port) < (int)m_available_controllers.size() &&
          m_settings->controller_binds.find(
              m_available_controllers.at(m_controller_port_mapping.at(port))->get_guid()) !=
              m_settings->controller_binds.end()) {
        m_settings->controller_binds
            .at(m_available_controllers.at(m_controller_port_mapping.at(port))->get_guid())
            .set_bindings(DEFAULT_CONTROLLER_BINDS);
      }
      break;
    case InputDeviceType::KEYBOARD:
      m_settings->keyboard_binds.set_bindings(DEFAULT_KEYBOARD_BINDS);
      break;
    case InputDeviceType::MOUSE:
      m_settings->mouse_binds.set_bindings(DEFAULT_MOUSE_BINDS);
      break;
  }
}

void InputManager::enqueue_set_auto_hide_mouse(const bool auto_hide_mouse) {
  const std::lock_guard<std::mutex> lock(m_event_queue_mtx);
  ee_event_queue.push({EEInputEventType::SET_AUTO_HIDE_MOUSE, auto_hide_mouse, {}, {}, {}});
}

void InputManager::set_auto_hide_mouse(const bool auto_hide_mouse) {
  m_auto_hide_mouse = auto_hide_mouse;
  if (!auto_hide_mouse) {
    hide_cursor(false);
  }
}

void InputManager::clear_inputs() {
  // Reset inputs as this device won't be able to be read from again!
  for (auto& [port, data] : m_data) {
    data->clear();
  }
}
