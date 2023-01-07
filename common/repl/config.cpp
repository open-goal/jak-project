#include "config.h"

#include "third-party/fmt/core.h"

namespace REPL {
void to_json(json& j, const Config& obj) {
  j = json{
      {"numConnectToTargetAttempts", obj.target_connect_attempts},
      {"asmFileSearchDirs", obj.asm_file_search_dirs},
      {"keybinds", obj.keybinds},
  };
  // Add game specific config if it exists
  for (const auto& [game_name, cfg] : obj.game_specific_cfg) {
    j[version_to_game_name(game_name)] = cfg;
  }
}

void from_json(const json& j, Config& obj) {
  if (j.contains("numConnectToTargetAttempts")) {
    j.at("numConnectToTargetAttempts").get_to(obj.target_connect_attempts);
  }
  if (j.contains("asmFileSearchDirs")) {
    j.at("asmFileSearchDirs").get_to(obj.asm_file_search_dirs);
  }
  if (j.contains("appendKeybinds")) {
    j.at("appendKeybinds").get_to(obj.append_keybinds);
  }
  if (j.contains("keybinds")) {
    std::vector<KeyBind> keybinds = j.at("keybinds");
    if (!obj.append_keybinds) {
      obj.keybinds = keybinds;
    } else {
      // append the keybinds
      // - start with the provided ones
      // - skip ones from the default set if they have the same key + modifier combination
      for (const auto& default_bind : obj.keybinds) {
        // check if it's a duplicate bind
        bool duplicate = false;
        for (const auto& new_bind : keybinds) {
          if (new_bind.key == default_bind.key && new_bind.modifier == default_bind.modifier) {
            duplicate = true;
            break;
          }
        }
        if (!duplicate) {
          keybinds.push_back(default_bind);
        }
      }
      obj.keybinds = keybinds;
    }
  }
  for (const auto& name : valid_game_version_names()) {
    if (j.contains(name)) {
      obj.game_specific_cfg.emplace(game_name_to_version(name), j.at(name).get<Config>());
    }
  }
}

std::string KeyBind::string() const {
  switch (modifier) {
    case KeyBind::Modifier::CTRL:
      return fmt::format("CTRL-{}", key);
    case KeyBind::Modifier::SHIFT:
      return fmt::format("SHIFT-{}", key);
    case KeyBind::Modifier::META:
      return fmt::format("META-{}", key);
  }
}

void to_json(json& j, const KeyBind& obj) {
  j = json{{"description", obj.description}, {"command", obj.command}, {"key", obj.key}};
  switch (obj.modifier) {
    case KeyBind::Modifier::CTRL:
      j["modifier"] = "ctrl";
      break;
    case KeyBind::Modifier::SHIFT:
      j["modifier"] = "shift";
      break;
    case KeyBind::Modifier::META:
      j["modifier"] = "meta";
      break;
  }
}

void from_json(const json& j, KeyBind& obj) {
  j.at("description").get_to(obj.description);
  j.at("command").get_to(obj.command);
  j.at("key").get_to(obj.key);
  auto modString = j.at("modifier").get<std::string>();
  if (modString == "ctrl") {
    obj.modifier = KeyBind::Modifier::CTRL;
  } else if (modString == "shift") {
    obj.modifier = KeyBind::Modifier::SHIFT;
  } else if (modString == "meta") {
    obj.modifier = KeyBind::Modifier::META;
  }
}
}  // namespace REPL
