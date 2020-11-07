#pragma once

/*!
 * @file kscheme.h
 * Implementation of GOAL runtime.
 */

#ifndef JAK_KSCHEME_H
#define JAK_KSCHEME_H

#include "common/common_types.h"
#include "common/goal_constants.h"
#include "kmachine.h"
#include "kmalloc.h"

extern u32 FastLink;
extern s32 NumSymbols;
extern Ptr<u32> EnableMethodSet;
extern Ptr<u32> s7;
extern Ptr<u32> SymbolTable2;
extern Ptr<u32> LastSymbol;

constexpr u32 EMPTY_HASH = 0x8454B6E6;
constexpr u32 OFFSET_MASK = 7;
constexpr u32 CRC_POLY = 0x04c11db7;

constexpr u32 DEFAULT_METHOD_COUNT = 12;
constexpr u32 FALLBACK_UNKNOWN_METHOD_COUNT = 44;

struct String {
  u32 len;
  char* data() { return ((char*)this) + sizeof(String); }
};

struct SymInfo {
  u32 hash;
  Ptr<String> str;
};

struct Symbol {
  u32 value;
};

inline Ptr<SymInfo> info(Ptr<Symbol> s) {
  return s.cast<SymInfo>() + SYM_INFO_OFFSET;
}

struct Function {};

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

u32 crc32(const u8* data, s32 size);
void kscheme_init_globals();
void init_crc();
u64 alloc_from_heap(u32 heapSymbol, u32 type, s32 size);
Ptr<Symbol> intern_from_c(const char* name);
Ptr<Type> intern_type_from_c(const char* name, u64 methods);
Ptr<Type> set_type_values(Ptr<Type> type, Ptr<Type> parent, u64 flags);
u64 print_object(u32 obj);
u64 print_pair(u32 obj);
u64 print_binteger(u64 obj);
u64 inspect_pair(u32 obj);
u64 inspect_binteger(u64 obj);
s32 InitHeapAndSymbol();
u64 call_goal(Ptr<Function> f, u64 a, u64 b, u64 c, u64 st, void* offset);
u64 call_goal_on_stack(Ptr<Function> f, u64 rsp, u64 st, void* offset);
void print_symbol_table();
u64 make_string_from_c(const char* c_str);
Ptr<Symbol> find_symbol_from_c(const char* name);
u64 call_method_of_type(u32 arg, Ptr<Type> type, u32 method_id);
u64 inspect_object(u32 obj);
u64 new_pair(u32 heap, u32 type, u32 car, u32 cdr);
s64 load_and_link(const char* filename, char* decode_name, kheapinfo* heap, u32 flags);
u64 load(u32 file_name_in, u32 heap_in);
u64 loado(u32 file_name_in, u32 heap_in);
u64 unload(u32 name);
Ptr<Function> make_function_symbol_from_c(const char* name, void* f);
u64 call_goal_function_by_name(const char* name);
Ptr<Type> alloc_and_init_type(Ptr<Symbol> sym, u32 method_count);
Ptr<Symbol> set_fixed_symbol(u32 offset, const char* name, u32 value);

#endif  // JAK_KSCHEME_H
