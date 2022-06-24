#pragma once
#include <map>
#include <memory>
#include <string>
#include <unordered_map>
#include <unordered_set>

#include "common/serialization/subtitles/subtitles.h"
#include "common/util/Assert.h"
#include "common/util/FontUtils.h"

void compile_game_text(const std::vector<std::string>& filenames,
                       GameTextVersion text_ver,
                       GameTextDB& db);
void compile_game_subtitle(const std::vector<std::string>& filenames,
                           GameTextVersion text_ver,
                           GameSubtitleDB& db);

void open_text_project(const std::string& kind,
                       const std::string& filename,
                       std::unordered_map<GameTextVersion, std::vector<std::string>>& inputs);
