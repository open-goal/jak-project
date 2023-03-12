#pragma once

#include "common/versions.h"

//! Supported languages.
enum class Language {
  English = 0,
  French = 1,
  German = 2,
  Spanish = 3,
  Italian = 4,
  Japanese = 5,
  UK_English = 6,
  // uk english?
};

struct GameLaunchOptions {
  GameVersion game_version = GameVersion::Jak1;
  bool disable_display = false;
  bool disable_debug_vm = false;
};
