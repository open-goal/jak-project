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

#endif  // JAK_GOAL_CONSTANTS_H
