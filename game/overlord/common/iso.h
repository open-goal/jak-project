#pragma once

#include "common/common_types.h"
#include "common/versions.h"

#include "game/overlord/common/isocommon.h"

struct VagDirEntry {
  char name[8];
  u32 offset;
};

static constexpr int VAG_COUNT_JAK1 = 868;
struct VagDirJak1 {
  u32 count;
  VagDirEntry vag[VAG_COUNT_JAK1];
};

static constexpr int VAG_COUNT_JAK2 = 2728;
struct VagDirJak2 {
  u32 count;
  VagDirEntry vag[VAG_COUNT_JAK2];
};

// use the larger VagDir and it will work for both 1 and 2.
using VagDir = VagDirJak2;

constexpr PerGameVersion<size_t> VAG_DIR_FILE_SIZE = {sizeof(VagDirJak1), sizeof(VagDirJak2)};

void iso_init_globals();
