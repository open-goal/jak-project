#include "subtitle_editor_db.h"

#include "common/util/FileUtil.h"
#include "common/util/json_util.h"

#include "game/runtime.h"

#include "third-party/fmt/core.h"

void Jak1SubtitleEditorDB::update() {
  if (g_game_version != GameVersion::Jak1) {
    return;
  }
  std::string db_path =
      (file_util::get_jak_project_dir() / "game" / "assets" / "jak1" / "subtitle-editor-db.json")
          .string();
  auto config_str = file_util::read_text_file(db_path);
  auto db_data = parse_commented_json(config_str, db_path);

  for (const auto& [key, val] : db_data.items()) {
    auto new_entry = Entry();
    try {
      new_entry.entity_type = val.at("entity_type").get<std::string>();
      new_entry.process_name = val.at("process_name").get<std::string>();
      new_entry.continue_name = val.at("continue_name").get<std::string>();
      new_entry.move_to = val.at("move_to").get<std::vector<double>>();
      if (val.contains("delay")) {
        new_entry.delay_frames = val.at("delay").get<int>();
      } else {
        new_entry.delay_frames = 0;
      }
      if (val.contains("move_first")) {
        new_entry.move_first = val.at("move_first").get<bool>();
      } else {
        new_entry.move_first = false;
      }
      if (new_entry.move_to.size() != 0 && new_entry.move_to.size() != 3) {
        fmt::print("Bad subtitle db entry, provide 0 or 3 coordinates for 'move_to' - {}", key);
        continue;
      }
      new_entry.execute_code = val.at("execute_code").get<std::string>();
      new_entry.requirements = val.at("requirements").get<std::vector<std::string>>();
      if (m_db.count(key) == 0) {
        m_db.emplace(key, new_entry);
      } else {
        m_db[key] = new_entry;
      }

    } catch (std::exception& ex) {
      fmt::print("Bad subtitle db entry - {} - {}", key, ex.what());
    }
  }
}
