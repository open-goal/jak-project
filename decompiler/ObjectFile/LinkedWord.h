#pragma once
// clang-format off
// TODO - clang formatting is off in this file due to 
// differences in newer versions of clang-format which we have not updates to yet

#if defined(__GNUC__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#elif defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif

/*!
 * @file LinkedWord.h
 * A word (4 bytes), possibly with some linking info.
 */

#include <cstdint>
#include <cstring>
#include <string>

#include "common/common_types.h"
#include "common/util/Assert.h"

namespace decompiler {
class LinkedWord {
 public:
  enum Kind : u8 {
    PLAIN_DATA,      // just plain data
    PTR,             // pointer to a location
    HI_PTR,          // lower 16-bits of this data are the upper 16 bits of a pointer
    LO_PTR,          // lower 16-bits of this data are the lower 16 bits of a pointer
    SYM_PTR,         // this is a pointer to a symbol
    EMPTY_PTR,       // this is a pointer to the empty list
    SYM_OFFSET,      // this is an offset of a symbol in the symbol table
    SYM_VAL_OFFSET,  // offset to the value of the symbol (different in jak 2)
    TYPE_PTR         // this is a pointer to a type
  };

 private:
  uintptr_t m_data_ptr = 0;  // 8 bytes
 public:
  u32 data;

 private:
  Kind m_kind = PLAIN_DATA;

 public:
  explicit LinkedWord(uint32_t _data) : data(_data) {}

  bool holds_string() const {
    return m_kind == SYM_PTR || m_kind == SYM_OFFSET || m_kind == TYPE_PTR ||
           m_kind == SYM_VAL_OFFSET;
  }

  LinkedWord(const LinkedWord& other) {
    if (&other != this) {
      data = other.data;
      m_kind = other.m_kind;
      if (holds_string()) {
        const char* other_str = (const char*)(other.m_data_ptr);
        char* str = new char[strlen(other_str) + 1];
        strcpy(str, other_str);
        m_data_ptr = (uintptr_t)str;
      } else {
        m_data_ptr = other.m_data_ptr;
      }
    }
  }

  LinkedWord& operator=(const LinkedWord& other) {
    if (&other != this) {
      data = other.data;
      if (holds_string()) {
        delete[]((char*)m_data_ptr);
      }
      m_kind = other.m_kind;

      if (holds_string()) {
        const char* other_str = (const char*)(other.m_data_ptr);
        char* str = new char[strlen(other_str) + 1];
        strcpy(str, other_str);
        m_data_ptr = (uintptr_t)str;
      } else {
        m_data_ptr = other.m_data_ptr;
      }
    }
    return *this;
  }

  ~LinkedWord() {
    if (holds_string()) {
      delete[]((char*)m_data_ptr);
    }
  }

  void set_to_empty_ptr() {
    if (holds_string()) {
      delete[]((char*)m_data_ptr);
    }
    m_kind = EMPTY_PTR;
  }

  void set_to_symbol(Kind kind, const std::string& name) {
    if (holds_string()) {
      delete[]((char*)m_data_ptr);
    }
    m_kind = kind;
    char* str = new char[name.size() + 1];
    strcpy(str, name.c_str());
    m_data_ptr = (uintptr_t)str;
  }

  void set_to_pointer(Kind kind, u32 label_id) {
    if (holds_string()) {
      delete[]((char*)m_data_ptr);
    }
    m_data_ptr = label_id;
    m_kind = kind;
  }

  void set_to_plain_data() {
    if (holds_string()) {
      delete[]((char*)m_data_ptr);
    }
    m_kind = PLAIN_DATA;
  }

  u8 get_byte(int idx) const {
    ASSERT(kind() == PLAIN_DATA);
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
        ASSERT(false);
        return 0;
    }
  }

  // kind, label_id, symbol_name
  Kind kind() const { return m_kind; }

  u32 label_id() const {
    ASSERT(m_kind == PTR || m_kind == LO_PTR || m_kind == HI_PTR);
    return m_data_ptr;
  }

  std::string symbol_name() const {
    ASSERT(holds_string());
    return (const char*)(m_data_ptr);
  }

 private:
  // if plain data, this is empty.
  // otherwise:
  //  u8 (kind)
  //  u32 label id | actual string.
};

}  // namespace decompiler

#if defined(__GNUC__)
#pragma GCC diagnostic pop
#elif defined(__clang__)
#pragma clang diagnostic pop
#endif
// clang-format on