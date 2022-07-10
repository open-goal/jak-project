#pragma once

#include "common/goal_constants.h"

#include "game/kernel/common/kmalloc.h"
#include "game/kernel/common/kscheme.h"

namespace jak1 {

struct SymInfo {
  u32 hash;
  Ptr<String> str;
};

struct Symbol {
  u32 value;
};

inline Ptr<SymInfo> info(Ptr<Symbol> s) {
  return s.cast<SymInfo>() + jak1::SYM_INFO_OFFSET;
}

constexpr u32 DEFAULT_METHOD_COUNT = 12;
constexpr u32 FALLBACK_UNKNOWN_METHOD_COUNT = 44;
constexpr u32 SYM_TABLE_END = jak1::GOAL_MAX_SYMBOLS - 32;

/*!
 * GOAL Type
 */
struct Type {
  Ptr<Symbol> symbol;  //! The type's symbol             0x0
  Ptr<Type> parent;    //! The type's parent             0x4
  u16 allocated_size;  //! The type's size in memory     0x8
  u16 padded_size;     //! The type's size, when padded? 0xa

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

void kscheme_init_globals();
Ptr<Symbol> intern_from_c(const char* name);
u64 load(u32 file_name_in, u32 heap_in);
u64 loado(u32 file_name_in, u32 heap_in);
u64 unload(u32 name);
s64 load_and_link(const char* filename, char* decode_name, kheapinfo* heap, u32 flags);
u64 make_string_from_c(const char* c_str);
u64 new_pair(u32 heap, u32 type, u32 car, u32 cdr);
u64 inspect_object(u32 obj);
u64 print_object(u32 obj);
Ptr<Symbol> find_symbol_from_c(const char* name);
u64 call_method_of_type(u64 arg, Ptr<Type> type, u32 method_id);
Ptr<Type> intern_type_from_c(const char* name, u64 methods);
u64 call_method_of_type_arg2(u32 arg, Ptr<Type> type, u32 method_id, u32 a1, u32 a2);
u64 alloc_heap_object(u32 heap, u32 type, u32 size, u32 pp);
Ptr<Function> make_function_symbol_from_c(const char* name, void* f);
Ptr<Function> make_stack_arg_function_symbol_from_c(const char* name, void* f);
s32 InitHeapAndSymbol();
u64 call_goal_function_by_name(const char* name);
Ptr<Type> alloc_and_init_type(Ptr<Symbol> sym, u32 method_count);
Ptr<Symbol> set_fixed_symbol(u32 offset, const char* name, u32 value);
}  // namespace jak1
