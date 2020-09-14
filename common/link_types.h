#pragma once

/*!
 * @file link_types.h
 * Types used in the linking data, shared between the object file generator and the kernel's linker.
 */

#ifndef JAK1_LINK_TYPES_H
#define JAK1_LINK_TYPES_H

enum LinkKind {
  LINK_TABLE_END = 0,                 //! no more linking data
  LINK_SYMBOL_OFFSET = 1,             //! link a symbol (pointer to symbol table entry)
  LINK_TYPE_PTR = 2,                  //! link a pointer to a type.
  LINK_DISTANCE_TO_OTHER_SEG_64 = 3,  //! link to another segment
  LINK_DISTANCE_TO_OTHER_SEG_32 = 4,  //! link to another segment
};

enum SegmentTypes { MAIN_SEGMENT = 0, DEBUG_SEGMENT = 1, TOP_LEVEL_SEGMENT = 2 };

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

#endif  // JAK1_LINK_TYPES_H
