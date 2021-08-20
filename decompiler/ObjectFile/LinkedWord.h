#pragma once

/*!
 * @file LinkedWord.h
 * A word (4 bytes), possibly with some linking info.
 */

#include <cstdint>
#include <string>
#include "common/util/assert.h"

#include "common/common_types.h"

namespace decompiler {
class LinkedWord {
 public:
  explicit LinkedWord(uint32_t _data) : data(_data) {}

  enum Kind {
    PLAIN_DATA,  // just plain data
    PTR,         // pointer to a location
    HI_PTR,      // lower 16-bits of this data are the upper 16 bits of a pointer
    LO_PTR,      // lower 16-bits of this data are the lower 16 bits of a pointer
    SYM_PTR,     // this is a pointer to a symbol
    EMPTY_PTR,   // this is a pointer to the empty list
    SYM_OFFSET,  // this is an offset of a symbol in the symbol table
    TYPE_PTR     // this is a pointer to a type
  } kind = PLAIN_DATA;

  uint32_t data = 0;

  int label_id = -1;
  std::string symbol_name;

  u8 get_byte(int idx) const {
    assert(kind == PLAIN_DATA);
    switch (idx) {
      case 0:
        return data & 0xff;
      case 1:
        return (data >> 8) & 0xff;
      case 2:
        return (data >> 16) & 0xff;
      case 3:
        return (data >> 24) & 0xff;
      default:
        assert(false);
        return 0;
    }
  }
};
}  // namespace decompiler
