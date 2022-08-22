#pragma once

#include "common/common_types.h"
#include "common/versions.h"

// Separate constants for the decompiler, so we can make changes to the game without breaking
// decompilation.

constexpr s32 DECOMP_SYM_INFO_OFFSET = 8167 * 8 - 4;

constexpr PerGameVersion<int> OFFSET_OF_NEXT_STATE_STORE = {72, 64};