#pragma once

#include <optional>
#include <string>
#include <vector>

#include "common/versions/versions.h"

struct GameSubtitleDefinitionFile {
  int language_id = -1;
  std::string text_version = "jak1-v2";
  std::string lines_path = "";
  std::optional<std::string> lines_base_path = std::nullopt;
  std::string meta_path = "";
  std::optional<std::string> meta_base_path = std::nullopt;
};

void open_subtitle_project(const std::string& project_kind,
                           const std::string& file_path,
                           std::vector<GameSubtitleDefinitionFile>& inputs);
std::string lookup_locale_code(const GameVersion game_version, const int language_id);
// Languages that have audio tracks are not translated via Crowdin, therefore they must have
// a copy of the scenes from their base english counterpart no matter what (so it can be
// translated!)
//
// In contrast, languages like english-UK vs english-GB are almost entirely identical and cannot be
// translated via Crowdin (it has an audio track) so it makes sense to minimize the noise in the
// file by eliding the duplicated scenes that have no changes.
bool dump_language_with_duplicates_from_base(const GameVersion game_version, const int language_id);
