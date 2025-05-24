#include "config.h"

#include "common/versions/versions.h"

#include "fmt/core.h"

namespace REPL {
void to_json(json& j, const Config& obj) {
  j = json{
      {"nreplPort", obj.nrepl_port},
      {"gameVersionFolder", obj.game_version_folder},
      {"numConnectToTargetAttempts", obj.target_connect_attempts},
      {"asmFileSearchDirs", obj.asm_file_search_dirs},
      {"keybinds", obj.keybinds},
      {"perGameHistory", obj.per_game_history},
      {"permissiveRedefinitions", obj.permissive_redefinitions},
  };
}

void from_json(const json& j, Config& obj) {
  // TODO - make a camelCase variant of json_serialize/deserialize macros
  if (j.contains("nreplPort")) {
    j.at("nreplPort").get_to(obj.nrepl_port);
  }
  if (j.contains("gameVersionFolder")) {
    j.at("gameVersionFolder").get_to(obj.game_version_folder);
  }
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
  if (j.contains("perGameHistory")) {
    j.at("perGameHistory").get_to(obj.per_game_history);
  }
  if (j.contains("permissiveRedefinitions")) {
    j.at("permissiveRedefinitions").get_to(obj.permissive_redefinitions);
  }
  // if there is game specific configuration, override any values we just set
  if (j.contains(version_to_game_name(obj.game_version))) {
    from_json(j.at(version_to_game_name(obj.game_version)), obj);
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
