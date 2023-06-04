#pragma once

#include "common/serialization/subtitles/subtitles_ser.h"

bool write_subtitle_db_to_files(const GameSubtitleDB& db, const GameVersion game_version);
