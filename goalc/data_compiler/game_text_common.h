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
                       GameTextDB& db,
                       const std::string& output_prefix);
void compile_game_subtitle(const std::vector<std::string>& filenames,
                           GameSubtitleDB& db,
                           const std::string& output_prefix);
