#pragma once

#include "common/common_types.h"
#include "common/versions/versions.h"

constexpr s32 BINTEGER_OFFSET = 0;
constexpr s32 PAIR_OFFSET = 2;
constexpr int POINTER_SIZE = 4;
constexpr int BASIC_OFFSET = 4;
constexpr int STRUCTURE_ALIGNMENT = 16;
constexpr int ARRAY_DATA_OFFSET = 12;  // not including type tag

/*!
 * Here you can change the size of the symbol table!
 * Make sure to also edit the constant in gcommon.gc
 */
// constexpr s32 GOAL_MAX_SYMBOLS = 8192;  // this MUST be a multiple of 2!!
// constexpr s32 SYM_INFO_OFFSET = 8167 * 8 - 4;

namespace jak1 {
constexpr s32 ORIGINAL_MAX_GOAL_SYMBOLS = 8192;
constexpr s32 ORIGINAL_SYM_TO_STRING_OFFSET = 0xff38;

constexpr s32 GOAL_MAX_SYMBOLS = 16384;
constexpr s32 SYM_INFO_OFFSET = GOAL_MAX_SYMBOLS * 8 - 4;

constexpr s32 SYM_TABLE_MEM_SIZE = GOAL_MAX_SYMBOLS * 8 * 2;

constexpr int bits_for_sym() {
  int b = -1;
  for (int i = 0; i < 32; ++i) {
    if ((GOAL_MAX_SYMBOLS & (1 << i)) != 0) {
      if (b != -1) {
        // already got a set bit... not a multiple of 2!
        // throw 0;
        return -1;
      }
      b = i;
    }
  }
  return b + 1;
}
static_assert(bits_for_sym() != -1, "symbol table invalid length");

// amount of levels in level heap
constexpr int LEVEL_MAX = 2;
// total amount of levels, including ones outside level heap (default-level)
constexpr int LEVEL_TOTAL = LEVEL_MAX + 1;
}  // namespace jak1

namespace jak2 {
// for now, we don't have the ability to extend the size of the symbol table
constexpr s32 GOAL_MAX_SYMBOLS = 0x4000;
constexpr s32 SYM_TABLE_MEM_SIZE = 0x30000;
// from the "off-by-one" symbol pointer
constexpr int SYM_TO_STRING_OFFSET = 0xff37;
constexpr int SYM_TO_HASH_OFFSET = 0x1fe6f;

// amount of levels in level heap
constexpr int LEVEL_MAX = 6;
// total amount of levels, including ones outside level heap (default-level)
constexpr int LEVEL_TOTAL = LEVEL_MAX + 1;
}  // namespace jak2

namespace jak3 {
// for now, we don't have the ability to extend the size of the symbol table
constexpr s32 GOAL_MAX_SYMBOLS = 0x4000;
// amount of levels in level heap
constexpr int LEVEL_MAX = 10;
// total amount of levels, including ones outside level heap (default-level)
constexpr int LEVEL_TOTAL = LEVEL_MAX + 1;
}  // namespace jak3

constexpr s32 max_symbols(GameVersion version) {
  switch (version) {
    case GameVersion::Jak1:
      return jak1::GOAL_MAX_SYMBOLS;
    case GameVersion::Jak2:
      return jak2::GOAL_MAX_SYMBOLS;
    case GameVersion::Jak3:
      return jak3::GOAL_MAX_SYMBOLS;
  }
}

enum class RegClass : u8 { GPR_64, FLOAT, INT_128, VECTOR_FLOAT, INVALID };

constexpr u32 GOAL_NEW_METHOD = 0;       // method ID of GOAL new
constexpr u32 GOAL_DEL_METHOD = 1;       // method ID of GOAL delete
constexpr u32 GOAL_PRINT_METHOD = 2;     // method ID of GOAL print
constexpr u32 GOAL_INSPECT_METHOD = 3;   // method ID of GOAL inspect
constexpr u32 GOAL_LENGTH_METHOD = 4;    // method ID of GOAL length
constexpr u32 GOAL_ASIZE_METHOD = 5;     // method ID of GOAL size
constexpr u32 GOAL_COPY_METHOD = 6;      // method ID of GOAL copy
constexpr u32 GOAL_RELOC_METHOD = 7;     // method ID of GOAL relocate
constexpr u32 GOAL_MEMUSAGE_METHOD = 8;  // method ID of GOAL mem-usage

constexpr int EE_MAIN_MEM_LOW_PROTECT = 512 * 1024;
constexpr int EE_MAIN_MEM_SIZE = 128 * (1 << 20);  // 128 MB, same as PS2 TOOL
constexpr u64 EE_MAIN_MEM_MAP = 0x2123000000;      // intentionally > 32-bit to catch pointer bugs

// when true, attempt to map the EE memory in the low 2 GB of RAM
// this allows us to use EE pointers as real pointers.  However, this might not always work,
// so this should be used only for debugging.
constexpr bool EE_MEM_LOW_MAP = false;

constexpr double METER_LENGTH = 4096.0;
constexpr double DEGREES_PER_ROT = 65536.0;
constexpr double DEGREES_LENGTH = DEGREES_PER_ROT / 360.0;
constexpr u64 TICKS_PER_SECOND = 300.0;

constexpr float DEFAULT_RES_TIME = -1000000000.0;
