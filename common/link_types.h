#pragma once

/*!
 * @file link_types.h
 * Types used in the linking data, shared between the object file generator and the kernel's linker.
 */

#include "common_types.h"

enum LinkKind {
  LINK_TABLE_END = 0,                 //! no more linking data
  LINK_SYMBOL_OFFSET = 1,             //! link a symbol (pointer to symbol table entry)
  LINK_TYPE_PTR = 2,                  //! link a pointer to a type.
  LINK_DISTANCE_TO_OTHER_SEG_64 = 3,  //! link to another segment
  LINK_DISTANCE_TO_OTHER_SEG_32 = 4,  //! link to another segment
  LINK_PTR = 5,                       //! link a pointer within this segment
};

enum SegmentTypes { MAIN_SEGMENT = 0, DEBUG_SEGMENT = 1, TOP_LEVEL_SEGMENT = 2 };

constexpr const char* SEGMENT_NAMES[3] = {"main", "debug", "top-level"};

constexpr int N_SEG = 3;

/*!
 * Data at the front of the DGO.
 */
struct DgoHeader {
  u32 object_count;
  char name[60];
};

/*!
 * Data at the front of each OBJ.
 */
struct ObjectHeader {
  u32 size;
  char name[60];
};

// Header for link data used for V2 linking data
// used in GOAL and OpenGOAL
struct LinkHeaderV2 {
  uint32_t type_tag;  // always -1
  uint32_t length;    // length of link data
  uint32_t version;   // always 2
};

// Header for link data used for V4
struct LinkHeaderV4 {
  uint32_t type_tag;   // always -1
  uint32_t length;     // length of V2 link data found after object.
  uint32_t version;    // always 4
  uint32_t code_size;  // length of object data before link data starts
};

struct LinkHeaderV5Core {
  uint32_t length_to_get_to_code;  // 4 length.. of link data?
  uint16_t version;                // 8
  uint16_t unknown;                // 10
  uint32_t length_to_get_to_link;  // 12
  uint32_t link_length;            // 16
  uint8_t n_segments;              // 20
  char name[59];                   // 21 (really??)
};

struct LinkHeaderV5 {
  uint32_t type_tag;  // 0 always 0?
  LinkHeaderV5Core core;
};

// when a u32/s32 symbol link contains this value, (s7 + <val>) should be a 4-byte aligned address,
// not including the 1 byte symbol offset. (no effect in jak 1).
constexpr u32 LINK_SYM_NO_OFFSET_FLAG = 0xbadbeef;