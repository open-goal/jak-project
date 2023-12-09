#pragma once

#include "game/kernel/common/kmalloc.h"
#include "game/kernel/common/Ptr.h"
#include "game/kernel/common/Symbol4.h"
#include "game/kernel/common/kmalloc.h"
#include "game/kernel/common/kscheme.h"

namespace jak3 {
void kscheme_init_globals();
constexpr s32 SYMBOL_OFFSET = 1;

/*!
 * GOAL Type
 */
struct Type {
  Ptr<Symbol4<Ptr<Type>>> symbol;  //! The type's symbol             0x0
  Ptr<Type> parent;                //! The type's parent             0x4
  u16 allocated_size;              //! The type's size in memory     0x8
  u16 padded_size;                 //! The type's size, when padded? 0xa

  u16 heap_base;    //! relative location of heap     0xc
  u16 num_methods;  //! allocated-length field        0xe - 0xf

  Ptr<Function> new_method;       // 16     0
  Ptr<Function> delete_method;    // 20     1
  Ptr<Function> print_method;     // 24     2
  Ptr<Function> inspect_method;   // 28     3
  Ptr<Function> length_method;    // 32     4
  Ptr<Function> asize_of_method;  // 36     5
  Ptr<Function> copy_method;      // 40     6
  Ptr<Function> relocate_method;  // 44     7
  Ptr<Function> memusage_method;  // 48     8

  Ptr<Function>& get_method(u32 i) {
    Ptr<Function>* f = &new_method;
    return f[i];
  }
};

s64 load_and_link(const char* filename, char* decode_name, kheapinfo* heap, u32 flags);
}