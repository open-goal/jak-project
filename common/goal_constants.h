#pragma once

#ifndef JAK_GOAL_CONSTANTS_H
#define JAK_GOAL_CONSTANTS_H

#include "common_types.h"

constexpr s32 BINTEGER_OFFSET = 0;
constexpr s32 PAIR_OFFSET = 2;
constexpr int POINTER_SIZE = 4;
constexpr int BASIC_OFFSET = 4;
constexpr int STRUCTURE_ALIGNMENT = 16;

enum class RegKind { GPR_64, FLOAT, INT_128, FLOAT_4X, INVALID };

constexpr u32 GOAL_NEW_METHOD = 0;       // method ID of GOAL new
constexpr u32 GOAL_DEL_METHOD = 1;       // method ID of GOAL delete
constexpr u32 GOAL_PRINT_METHOD = 2;     // method ID of GOAL print
constexpr u32 GOAL_INSPECT_METHOD = 3;   // method ID of GOAL inspect
constexpr u32 GOAL_LENGTH_METHOD = 4;    // method ID of GOAL length
constexpr u32 GOAL_ASIZE_METHOD = 5;     // method ID of GOAL size
constexpr u32 GOAL_COPY_METHOD = 6;      // method ID of GOAL copy
constexpr u32 GOAL_RELOC_METHOD = 7;     // method ID of GOAL relocate
constexpr u32 GOAL_MEMUSAGE_METHOD = 8;  // method ID of GOAL mem-usage

constexpr int EE_MAIN_MEM_LOW_PROTECT = 1024 * 1024;
constexpr int EE_MAIN_MEM_SIZE = 128 * (1 << 20);  // 128 MB, same as PS2 TOOL
constexpr u64 EE_MAIN_MEM_MAP = 0x2000000000;      // intentionally > 32-bit to catch pointer bugs

// when true, attempt to map the EE memory in the low 2 GB of RAM
// this allows us to use EE pointers as real pointers.  However, this might not always work,
// so this should be used only for debugging.
constexpr bool EE_MEM_LOW_MAP = false;

#endif  // JAK_GOAL_CONSTANTS_H
