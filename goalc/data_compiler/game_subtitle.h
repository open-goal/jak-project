#pragma once

#include <vector>

#include "common/serialization/subtitles/subtitles.h"

void compile_game_subtitle(const std::vector<std::string>& filenames, GameTextVersion text_ver);
