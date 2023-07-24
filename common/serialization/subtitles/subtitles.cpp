#include "subtitles.h"

#include "common/goos/ParseHelpers.h"
#include "common/goos/Reader.h"
#include "common/util/FileUtil.h"
#include "common/util/json_util.h"

#include "third-party/fmt/core.h"

void open_subtitle_project(const std::string& project_kind,
                           const std::string& file_path,
                           std::vector<GameSubtitleDefinitionFile>& subtitle_files) {
  goos::Reader reader;
  auto& proj = reader.read_from_file({file_path}).as_pair()->cdr.as_pair()->car;
  if (!proj.is_pair() || !proj.as_pair()->car.is_symbol() ||
      proj.as_pair()->car.as_symbol()->name != project_kind) {
    throw std::runtime_error(fmt::format("invalid project '{}'", project_kind));
  }

  goos::for_each_in_list(proj.as_pair()->cdr, [&](const goos::Object& o) {
    if (o.is_pair() && o.as_pair()->cdr.is_pair()) {
      auto args = o.as_pair();
      auto& action = args->car.as_symbol()->name;
      args = args->cdr.as_pair();

      if (action == "file-json") {
        auto new_file = GameSubtitleDefinitionFile();
        while (true) {
          const auto& kwarg = args->car.as_symbol()->name;
          args = args->cdr.as_pair();
          if (kwarg == ":language-id") {
            new_file.language_id = args->car.as_int();
          } else if (kwarg == ":text-version") {
            new_file.text_version = args->car.as_string()->data;
          } else if (kwarg == ":lines") {
            new_file.lines_path = args->car.as_string()->data;
          } else if (kwarg == ":meta") {
            new_file.meta_path = args->car.as_string()->data;
          } else if (kwarg == ":lines-base") {
            new_file.lines_base_path = args->car.as_string()->data;
          } else if (kwarg == ":meta-base") {
            new_file.meta_base_path = args->car.as_string()->data;
          }
          if (args->cdr.is_empty_list()) {
            break;
          }
          args = args->cdr.as_pair();
        }
        subtitle_files.push_back(new_file);
      } else {
        throw std::runtime_error(
            fmt::format("unknown action {} in {} project", action, project_kind));
      }
    } else {
      throw std::runtime_error(fmt::format("invalid entry in {} project", project_kind));
    }
  });
}

const std::unordered_map<GameVersion, std::vector<std::string>> locale_lookup = {
    {GameVersion::Jak1,
     {"en-US", "fr-FR", "de-DE", "es-ES", "it-IT", "jp-JP", "en-GB", "pt-PT", "fi-FI", "sv-SE",
      "da-DK", "no-NO", "nl-NL", "pt-BR", "hu-HU", "ca-ES", "is-IS"}},
    {GameVersion::Jak2, {"en-US", "fr-FR", "de-DE", "es-ES", "it-IT", "jp-JP", "ko-KR", "en-GB"}}};

std::string lookup_locale_code(const GameVersion game_version, const int language_id) {
  if (locale_lookup.find(game_version) == locale_lookup.end() ||
      (int)locale_lookup.at(game_version).size() < language_id) {
    return "";
  }
  return locale_lookup.at(game_version).at(language_id);
}

const std::unordered_map<GameVersion, std::vector<int>> language_ids_with_audio = {
    {GameVersion::Jak1, {0, 1, 2, 3, 4, 5, 6}},
    {GameVersion::Jak2, {0, 1, 2, 3, 4, 5, 6, 7}}};

bool dump_language_with_duplicates_from_base(const GameVersion game_version,
                                             const int language_id) {
  if (language_ids_with_audio.find(game_version) == language_ids_with_audio.end()) {
    return true;
  }
  if (std::find(language_ids_with_audio.at(game_version).begin(),
                language_ids_with_audio.at(game_version).end(),
                language_id) == language_ids_with_audio.at(game_version).end()) {
    return true;
  }
  return false;
}
