#include "subtitles2_deser.h"

#include "common/log/log.h"
#include "common/util/FileUtil.h"

#include "third-party/json.hpp"

const std::vector<std::string> locale_lookup = {"en-US", "fr-FR", "de-DE", "es-ES",
                                                "it-IT", "jp-JP", "ko-KR", "en-GB"};

bool write_subtitle_db_to_files(const GameSubtitle2DB& db, const GameVersion game_version) {
  try {
    for (const auto& [language_id, bank] : db.m_banks) {
      json data;
      to_json(data, *bank);
      std::string dump_path = (file_util::get_jak_project_dir() / "game" / "assets" /
                               version_to_game_name(game_version) / "subtitle" /
                               fmt::format("subtitle_{}.json", locale_lookup.at(language_id)))
                                  .string();
      file_util::write_text_file(dump_path, data.dump(2));
    }
  } catch (std::exception& ex) {
    lg::error(ex.what());
    return false;
  }
  return true;
}
