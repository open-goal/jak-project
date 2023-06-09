#pragma once

#include "common/serialization/subtitles2/subtitles2_ser.h"

bool write_subtitle_db_to_files(const GameSubtitle2DB& db, const GameVersion game_version);
