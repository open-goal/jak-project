#pragma once
#include "common/common_types.h"

#include "game/kernel/common/Ptr.h"
#include "game/kernel/common/Symbol4.h"
#include "game/kernel/common/kscheme.h"

namespace jak2 {

constexpr s32 SYMBOL_OFFSET = 1;

extern Ptr<Symbol4<u32>> SqlResult;

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

void kscheme_init_globals();
Ptr<Symbol4<u32>> intern_from_c(const char* name);
Ptr<Type> intern_type_from_c(const char* name, u64 methods);
u64 call_method_of_type_arg2(u32 arg, Ptr<Type> type, u32 method_id, u32 a1, u32 a2);
Ptr<Symbol4<u32>> find_symbol_from_c(const char* name);
u64 make_string_from_c(const char* c_str);
u64 make_debug_string_from_c(const char* c_str);
u64 new_pair(u32 heap, u32 type, u32 car, u32 cdr);
u64 inspect_object(u32 obj);
u64 print_object(u32 obj);
Ptr<Function> make_function_symbol_from_c(const char* name, void* f);
Ptr<Function> make_stack_arg_function_symbol_from_c(const char* name, void* f);
int InitHeapAndSymbol();
u64 load(u32 file_name_in, u32 heap_in);
u64 loadb(u32 file_name_in, u32 heap_in, u32 param3);
u64 loado(u32 file_name_in, u32 heap_in);
u64 unload(u32 name);
u64 call_method_of_type(u32 arg, Ptr<Type> type, u32 method_id);
u64 call_goal_function_by_name(const char* name);
u64 alloc_heap_memory(u32 heap, u32 size);
u64 alloc_heap_object(u32 heap, u32 type, u32 size, u32 pp);
u32 u32_in_fixed_sym(u32 offset);
}  // namespace jak2
