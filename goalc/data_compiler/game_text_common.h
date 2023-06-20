#pragma once
#include <map>
#include <memory>
#include <string>
#include <unordered_map>
#include <unordered_set>

#include "common/serialization/subtitles/subtitles_ser.h"
#include "common/serialization/subtitles2/subtitles2_ser.h"
#include "common/util/Assert.h"
#include "common/util/FontUtils.h"

void compile_game_text(const std::vector<GameTextDefinitionFile>& filenames,
                       GameTextDB& db,
                       const std::string& output_prefix);
void compile_game_subtitle(const std::vector<GameSubtitleDefinitionFile>& filenames,
                           GameSubtitleDB& db,
                           const std::string& output_prefix);
void compile_game_subtitle2(const std::vector<GameSubtitle2DefinitionFile>& filenames,
                            GameSubtitle2DB& db,
                            const std::string& output_prefix);
