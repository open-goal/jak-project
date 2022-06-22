#pragma once
#include "common/util/FontUtils.h"
#include "common/util/Assert.h"
#include <string>
#include <map>
#include <unordered_map>
#include <unordered_set>
#include <memory>
#include "common/serialization/subtitles/subtitles.h"

void compile_game_text(const std::vector<std::string>& filenames,
                       GameTextVersion text_ver,
                       GameTextDB& db);
void compile_game_subtitle(const std::vector<std::string>& filenames,
                           GameTextVersion text_ver,
                           GameSubtitleDB& db);

void open_text_project(const std::string& kind,
                       const std::string& filename,
                       std::unordered_map<GameTextVersion, std::vector<std::string>>& inputs);
