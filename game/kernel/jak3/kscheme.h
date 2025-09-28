#pragma once

#include "game/kernel/common/Ptr.h"
#include "game/kernel/common/Symbol4.h"
#include "game/kernel/common/kmalloc.h"
#include "game/kernel/common/kscheme.h"

namespace jak3 {
void kscheme_init_globals();
constexpr s32 SYMBOL_OFFSET = 1;
extern Ptr<u32> SymbolString;
extern bool DebugSymbols;

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
u32 u32_in_fixed_sym(u32 offset);
Ptr<Symbol4<u32>> intern_from_c(int sym_id, int flags, const char* name);
u64 load(u32 /*file_name_in*/, u32 /*heap_in*/);
u64 loadb(u32 /*file_name_in*/, u32 /*heap_in*/, u32 /*param3*/);
u64 loadc(const char* /*file_name*/, kheapinfo* /*heap*/, u32 /*flags*/);
u64 loado(u32 file_name_in, u32 heap_in);
u64 unload(u32 name);
Ptr<Function> make_function_symbol_from_c(const char* name, void* f);
Ptr<Function> make_stack_arg_function_symbol_from_c(const char* name, void* f);
u64 print_object(u32 obj);
u64 inspect_object(u32 obj);
Ptr<Symbol4<u32>> find_symbol_from_c(uint16_t sym_id, const char* name);
u64 make_string_from_c(const char* c_str);
u64 call_method_of_type(u32 arg, Ptr<Type> type, u32 method_id);
u64 new_pair(u32 heap, u32 type, u32 car, u32 cdr);
u64 call_goal_function_by_name(const char* name);
Ptr<Type> intern_type_from_c(int a, int b, const char* name, u64 methods);
u64 alloc_heap_object(u32 heap, u32 type, u32 size, u32 pp);
int InitHeapAndSymbol();
u64 call_method_of_type_arg2(u32 arg, Ptr<Type> type, u32 method_id, u32 a1, u32 a2);
u64 alloc_heap_memory(u32 heap, u32 size);
template <typename T>
Ptr<Ptr<String>> sym_to_string_ptr(Ptr<Symbol4<T>> in) {
  return Ptr<Ptr<String>>(SymbolString.offset + in.offset - s7.offset);
}
template <typename T>
Ptr<String> sym_to_string(Ptr<Symbol4<T>> in) {
  return *sym_to_string_ptr(in);
}

template <typename T>
const char* sym_to_cstring(Ptr<Symbol4<T>> in) {
  return sym_to_string(in)->data();
}

}  // namespace jak3