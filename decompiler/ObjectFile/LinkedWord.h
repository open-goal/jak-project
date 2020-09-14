#pragma once

/*!
 * @file LinkedWord.h
 * A word (4 bytes), possibly with some linking info.
 */

#ifndef JAK2_DISASSEMBLER_LINKEDWORD_H
#define JAK2_DISASSEMBLER_LINKEDWORD_H

#include <cstdint>
#include <string>

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
};

#endif  // JAK2_DISASSEMBLER_LINKEDWORD_H
