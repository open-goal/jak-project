#pragma once
#include <map>
#include <memory>
#include <string>
#include <unordered_map>
#include <unordered_set>

#include "common/util/Assert.h"
#include "common/util/FontUtils.h"
#include <common/serialization/subtitles/subtitles.h>
#include <common/serialization/subtitles/subtitles_v1.h>
#include <common/serialization/subtitles/subtitles_v2.h>
#include <common/serialization/text/text_ser.h>

void compile_game_text(const std::vector<GameTextDefinitionFile>& filenames,
                       GameTextDB& db,
                       const std::string& output_prefix);
void compile_game_subtitles(const std::vector<GameSubtitleDefinitionFile>& filenames,
                           GameSubtitleDBV1& db,
                           const std::string& output_prefix);
void compile_game_subtitles_v2(const std::vector<GameSubtitleDefinitionFile>& filenames,
                            GameSubtitleDBV2& db,
                            const std::string& output_prefix);
