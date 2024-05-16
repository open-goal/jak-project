
#include "kscheme.h"

#include <cstdio>
#include <cstring>

#include "fileio.h"

#include "common/common_types.h"
#include "common/global_profiler/GlobalProfiler.h"
#include "common/goal_constants.h"
#include "common/log/log.h"
#include "common/symbols.h"

#include "game/kernel/common/fileio.h"
#include "game/kernel/common/kdsnetm.h"
#include "game/kernel/common/klink.h"
#include "game/kernel/common/kmemcard.h"
#include "game/kernel/common/kprint.h"
#include "game/kernel/jak2/kdgo.h"
#include "game/kernel/jak2/klink.h"
#include "game/kernel/jak2/klisten.h"
#include "game/kernel/jak2/kmachine.h"
#include "game/kernel/jak2/kmalloc.h"
#include "game/kernel/jak2/kprint.h"

namespace jak2 {
using namespace jak2_symbols;
// where to put a new symbol for the most recently searched for symbol that wasn't found
u32 symbol_slot;

Ptr<Symbol4<u32>> LevelTypeList;
Ptr<Symbol4<u32>> CollapseQuote;
Ptr<Symbol4<u32>> SqlResult;
Ptr<u32> KernelDebug;

void kscheme_init_globals() {
  symbol_slot = 0;
  LevelTypeList.offset = 0;
  CollapseQuote.offset = 0;
  SqlResult.offset = 0;
  KernelDebug.offset = 0;
}

/*!
 * New method for types which cannot have "new" used on them.
 * Prints an error to stdout and returns false.
 */
u64 new_illegal(u32 allocation, u32 type) {
  (void)allocation;
  MsgErr("dkernel: illegal attempt to call new method of static object type %s\n",
         symbol_name_cstr(*Ptr<Type>(type)->symbol));
  return s7.offset;
}

u32 u32_in_fixed_sym(u32 offset) {
  return Ptr<Symbol4<u32>>(s7.offset + offset)->value();
}

namespace {
template <typename T>
Ptr<Ptr<String>> sym_to_string_ptr(Ptr<Symbol4<T>> in) {
  return Ptr<Ptr<String>>(in.offset + SYM_TO_STRING_OFFSET);
}

template <typename T>
Ptr<String> sym_to_string(Ptr<Symbol4<T>> in) {
  return *sym_to_string_ptr(in);
}

template <typename T>
Ptr<u32> sym_to_hash(Ptr<Symbol4<T>> in) {
  return Ptr<u32>(in.offset + SYM_TO_HASH_OFFSET);
}

void fixed_sym_set(u32 offset, u32 value) {
  Ptr<Symbol4<u32>>(s7.offset + offset)->value() = value;
}
}  // namespace

u64 alloc_from_heap(u32 heap_symbol, u32 type, s32 size, u32 pp) {
  using namespace jak2_symbols;
  auto heap_ptr = Ptr<Symbol4<Ptr<kheapinfo>>>(heap_symbol)->value();

  s32 aligned_size = ((size + 0xf) / 0x10) * 0x10;
  if ((heap_symbol == s7.offset + FIX_SYM_GLOBAL_HEAP) ||
      (heap_symbol == s7.offset + FIX_SYM_DEBUG) ||
      (heap_symbol == s7.offset + FIX_SYM_LOADING_LEVEL) ||
      (heap_symbol == s7.offset + FIX_SYM_PROCESS_LEVEL_HEAP)) {
    if (!type) {  // no type given, just call it a global-object
      return kmalloc(heap_ptr, size, KMALLOC_MEMSET, "global-object").offset;
    }

    Ptr<Type> typ(type);
    if (!typ->symbol.offset) {  // type doesn't have a symbol, just call it a global-object
      return kmalloc(heap_ptr, size, KMALLOC_MEMSET, "global-object").offset;
    }

    Ptr<String> gstr = sym_to_string(typ->symbol);
    if (!gstr->len) {  // string has nothing in it.
      return kmalloc(heap_ptr, size, KMALLOC_MEMSET, "global-object").offset;
    }

    return kmalloc(heap_ptr, size, KMALLOC_MEMSET, gstr->data()).offset;
  } else if (heap_symbol == s7.offset + FIX_SYM_PROCESS_TYPE) {
    u32 start = *Ptr<u32>(pp + 0x64);
    u32 heapEnd = *Ptr<u32>(pp + 0x60);
    u32 allocEnd = start + aligned_size;

    if (allocEnd < heapEnd) {
      *Ptr<u32>(pp + 0x64) = allocEnd;
      memset(Ptr<u8>(start).c(), 0, aligned_size);
      return start;
    } else {
      MsgErr("kmalloc: !alloc mem in heap for #<process @ #x%x> (%d bytes)\n", pp, aligned_size);
      return 0;
    }
  } else if (heap_symbol == s7.offset + FIX_SYM_SCRATCH) {
    ASSERT(false);  // nyi, I think unused.
    return 0;
  } else {
    memset(Ptr<u8>(heap_symbol).c(), 0, aligned_size);  // treat it as a stack address
    return heap_symbol;
  }
}

/*!
 * Allocate untyped memory.
 */
u64 alloc_heap_memory(u32 heap, u32 size) {
  // should never happen on process heap
  return alloc_from_heap(heap, 0, size, UNKNOWN_PP);
}

/*!
 * Allocate memory and add type tag for an object.
 * For allocating basics.
 * Called from GOAL.
 */
u64 alloc_heap_object(u32 heap, u32 type, u32 size, u32 pp) {
  auto mem = alloc_from_heap(heap, type, size, pp);
  if (!mem) {
    return 0;
  }

  *Ptr<u32>(mem) = type;
  return mem + BASIC_OFFSET;
}

/*!
 * Allocate a structure and get the structure size from the type.
 */
u64 new_structure(u32 heap, u32 type) {
  // should never happen on process heap
  return alloc_from_heap(heap, type, Ptr<Type>(type)->allocated_size, UNKNOWN_PP);
}

/*!
 * Allocate a structure with a dynamic size
 */
u64 new_dynamic_structure(u32 heap_symbol, u32 type, u32 size) {
  // should never happen on process heap
  return alloc_from_heap(heap_symbol, type, size, UNKNOWN_PP);
}

/*!
 * Delete a structure.  Not supported, as it uses kfree, which doesn't do anything.
 */
void delete_structure(u32 s) {
  kfree(Ptr<u8>(s));
}

/*!
 * Allocate a basic of fixed size.
 */
u64 new_basic(u32 heap, u32 type, u32 /*size*/, u32 pp) {
  return alloc_heap_object(heap, type, Ptr<Type>(type)->allocated_size, pp);
}

/*!
 * Delete a basic.  Not supported, as it uses kfree.
 */
void delete_basic(u32 s) {
  // note that the game has a bug here and has s as a uint* and does -4 which is actually a
  // 16-byte offset. Luckily kfree does nothing so there's no harm done.  But it's a good indication
  // that the "freeing memory" feature never made it very far in development. This bug exists in
  // Jak 3 as well.
  kfree(Ptr<u8>(s - BASIC_OFFSET * 4));  // replicate the bug
}

/*!
 * Allocate a new pair and set its car and cdr.
 */
u64 new_pair(u32 heap, u32 type, u32 car, u32 cdr) {
  auto mem = alloc_from_heap(heap, type, Ptr<Type>(type)->allocated_size, UNKNOWN_PP);
  if (!mem) {
    return 0;
  }

  u32* m = Ptr<u32>(mem).c();
  m[0] = car;
  m[1] = cdr;
  return mem + PAIR_OFFSET;
}

/*!
 * Delete a pair.  BUG
 */
void delete_pair(u32 s) {
  // the -8 should be a -2, but s is likely a u32* in the code.
  kfree(Ptr<u8>(s - 8));
}

/*!
 * Make an empty string of given size.
 * Allocates from the global heap.
 */
u64 make_string(u32 size) {
  using namespace jak2_symbols;
  auto mem_size = size + 1;  // null
  if (mem_size < 8) {
    mem_size = 8;  // min size of string
  }

  // total size is mem_size (chars + null term), plus basic_offset (type tag) + 4 (string size)
  auto mem =
      alloc_heap_object((s7 + FIX_SYM_GLOBAL_HEAP).offset, u32_in_fixed_sym(FIX_SYM_STRING_TYPE),
                        mem_size + BASIC_OFFSET + sizeof(uint32_t), UNKNOWN_PP);

  // set the string size field.
  if (mem) {
    *Ptr<u32>(mem) = size;
  }
  return mem;
}

/*!
 * Convert a C string to a GOAL string.
 * Allocates from the global heap and copies the string data.
 */
u64 make_string_from_c(const char* c_str) {
  auto str_size = strlen(c_str);
  auto mem_size = str_size + 1;
  if (mem_size < 8) {
    mem_size = 8;
  }

  auto mem =
      alloc_heap_object((s7 + FIX_SYM_GLOBAL_HEAP).offset, u32_in_fixed_sym(FIX_SYM_STRING_TYPE),
                        mem_size + BASIC_OFFSET + 4, UNKNOWN_PP);
  // there's no check for failed allocation here!

  // string size field
  *Ptr<u32>(mem) = str_size;

  // rest is chars
  kstrcpy(Ptr<char>(mem + 4).c(), c_str);
  return mem;
}

u64 make_debug_string_from_c(const char* c_str) {
  auto str_size = strlen(c_str);
  auto mem_size = str_size + 1;
  if (mem_size < 8) {
    mem_size = 8;
  }

  auto mem = alloc_heap_object((s7 + FIX_SYM_DEBUG).offset, u32_in_fixed_sym(FIX_SYM_STRING_TYPE),
                               mem_size + BASIC_OFFSET + 4, UNKNOWN_PP);
  // there's no check for failed allocation here!

  // string size field
  *Ptr<u32>(mem) = str_size;

  // rest is chars
  kstrcpy(Ptr<char>(mem + 4).c(), c_str);
  return mem;
}

extern "C" {
#ifndef __aarch64__
#ifdef __APPLE__
void _arg_call_systemv() asm("_arg_call_systemv");
void _stack_call_systemv() asm("_stack_call_systemv");
void _stack_call_win32() asm("_stack_call_win32");
#else
void _arg_call_systemv();
void _stack_call_systemv();
void _stack_call_win32();
#endif
#else
#if defined(__APPLE__)
void _arg_call_arm64() asm("_arg_call_arm64");
void _stack_call_arm64() asm("_stack_call_arm64");
#else
void _arg_call_arm64();
void _stack_call_arm64();
#endif
#endif
}

/*!
 * This creates an OpenGOAL function from a C++ function. Only 6 arguments can be accepted.
 * But calling this function is fast. It used to be really fast but wrong.
 */
Ptr<Function> make_function_from_c_systemv(void* func, bool arg3_is_pp) {
  auto mem = Ptr<u8>(alloc_heap_object(s7.offset + FIX_SYM_GLOBAL_HEAP,
                                       u32_in_fixed_sym(FIX_SYM_FUNCTION_TYPE), 0x40, UNKNOWN_PP));
  auto f = (uint64_t)func;
  auto target_function = (u8*)&f;
#ifndef __aarch64__
  auto trampoline_function_addr = _arg_call_systemv;
#else
  auto trampoline_function_addr = _arg_call_arm64;
#endif
  auto trampoline = (u8*)&trampoline_function_addr;
  // TODO - x86 code still being emitted below

  // movabs rax, target_function
  int offset = 0;
  mem.c()[offset++] = 0x48;
  mem.c()[offset++] = 0xb8;
  for (int i = 0; i < 8; i++) {
    mem.c()[offset++] = target_function[i];
  }

  // push rax
  mem.c()[offset++] = 0x50;

  // movabs rax, trampoline
  mem.c()[offset++] = 0x48;
  mem.c()[offset++] = 0xb8;
  for (int i = 0; i < 8; i++) {
    mem.c()[offset++] = trampoline[i];
  }

  if (arg3_is_pp) {
    // mov rcx, r13. Puts pp in the third argument.
    mem.c()[offset++] = 0x4c;
    mem.c()[offset++] = 0x89;
    mem.c()[offset++] = 0xe9;
  }

  // jmp rax
  mem.c()[offset++] = 0xff;
  mem.c()[offset++] = 0xe0;
  // the asm function's ret will return to the caller of this (GOAL code) directlyz.

  // CacheFlush(mem, 0x34);

  return mem.cast<Function>();
}

/*!
 * Create a GOAL function from a C function. This doesn't export it as a global function, it just
 * creates a function object on the global heap.
 *
 * This creates a simple trampoline function which jumps to the C function and reorders the
 * arguments to be correct for Windows.
 */
Ptr<Function> make_function_from_c_win32(void* func, bool arg3_is_pp) {
  // allocate a function object on the global heap
  auto mem = Ptr<u8>(alloc_heap_object(s7.offset + FIX_SYM_GLOBAL_HEAP,
                                       u32_in_fixed_sym(FIX_SYM_FUNCTION_TYPE), 0x80, UNKNOWN_PP));
  auto f = (uint64_t)func;
  auto fp = (u8*)&f;

  int i = 0;
  // we will put the function address in RAX with a movabs rax, imm8
  mem.c()[i++] = 0x48;
  mem.c()[i++] = 0xb8;
  for (int j = 0; j < 8; j++) {
    mem.c()[i++] = fp[j];
  }

  /*
    push rdi
    push rsi
    push rdx
    push rcx
    pop r9
    pop r8
    pop rdx
    pop rcx
    push r10
    push r11
    sub rsp, 40
   */
  for (auto x : {0x57, 0x56, 0x52, 0x51, 0x41, 0x59, 0x41, 0x58, 0x5A, 0x59, 0x41, 0x52, 0x41, 0x53,
                 0x48, 0x83, 0xEC, 0x28}) {
    mem.c()[i++] = x;
  }

  if (arg3_is_pp) {
    // mov r9, r13. Puts pp in the third argument.
    mem.c()[i++] = 0x4d;
    mem.c()[i++] = 0x89;
    mem.c()[i++] = 0xe9;
  }

  /*
    call rax
    add rsp, 40
    pop r11
    pop r10
    ret
   */
  for (auto x : {0xFF, 0xD0, 0x48, 0x83, 0xC4, 0x28, 0x41, 0x5B, 0x41, 0x5A, 0xC3}) {
    mem.c()[i++] = x;
  }

  // CacheFlush(mem, 0x34);

  return mem.cast<Function>();
}

Ptr<Function> make_stack_arg_function_from_c_systemv(void* func) {
  // allocate a function object on the global heap
  auto mem = Ptr<u8>(alloc_heap_object(s7.offset + FIX_SYM_GLOBAL_HEAP,
                                       u32_in_fixed_sym(FIX_SYM_FUNCTION_TYPE), 0x40, UNKNOWN_PP));
  auto f = (uint64_t)func;
  auto target_function = (u8*)&f;
#ifndef __aarch64__
  auto trampoline_function_addr = _stack_call_systemv;
#else
  auto trampoline_function_addr = _stack_call_arm64;
#endif
  auto trampoline = (u8*)&trampoline_function_addr;

  // movabs rax, target_function
  int offset = 0;
  mem.c()[offset++] = 0x48;
  mem.c()[offset++] = 0xb8;
  for (int i = 0; i < 8; i++) {
    mem.c()[offset++] = target_function[i];
  }

  // push rax
  mem.c()[offset++] = 0x50;

  // movabs rax, trampoline
  mem.c()[offset++] = 0x48;
  mem.c()[offset++] = 0xb8;
  for (int i = 0; i < 8; i++) {
    mem.c()[offset++] = trampoline[i];
  }

  // jmp rax
  mem.c()[offset++] = 0xff;
  mem.c()[offset++] = 0xe0;

  // CacheFlush(mem, 0x34);

  return mem.cast<Function>();
}

#ifdef _WIN32
/*!
 * Create a GOAL function from a C function.  This calls a windows function, but doesn't scramble
 * the argument order.  It's supposed to be used with _format_win32 which assumes GOAL order.
 */
Ptr<Function> make_stack_arg_function_from_c_win32(void* func) {
  // allocate a function object on the global heap
  auto mem = Ptr<u8>(alloc_heap_object(s7.offset + FIX_SYM_GLOBAL_HEAP,
                                       u32_in_fixed_sym(FIX_SYM_FUNCTION_TYPE), 0x80, UNKNOWN_PP));
  auto f = (uint64_t)func;
  auto fp = (u8*)&f;
  auto trampoline_function_addr = _stack_call_win32;
  auto trampoline = (u8*)&trampoline_function_addr;

  int i = 0;
  // we will put the function address in RAX with a movabs rax, imm8
  mem.c()[i++] = 0x48;
  mem.c()[i++] = 0xb8;
  for (int j = 0; j < 8; j++) {
    mem.c()[i++] = fp[j];
  }

  // push rax
  mem.c()[i++] = 0x50;

  // we will put the function address in RAX with a movabs rax, imm8
  mem.c()[i++] = 0x48;
  mem.c()[i++] = 0xb8;
  for (int j = 0; j < 8; j++) {
    mem.c()[i++] = trampoline[j];
  }

  /*
   * jmp rax
   */
  for (auto x : {0xFF, 0xE0}) {
    mem.c()[i++] = x;
  }

  return mem.cast<Function>();
}
#endif

/*!
 * Create a GOAL function from a C function. This doesn't export it as a global function, it just
 * creates a function object on the global heap.
 *
 * The implementation is to create a simple trampoline function which jumps to the C function.
 */
Ptr<Function> make_function_from_c(void* func, bool arg3_is_pp = false) {
#ifdef __linux__
  return make_function_from_c_systemv(func, arg3_is_pp);
#elif __APPLE__
  return make_function_from_c_systemv(func, arg3_is_pp);
#elif _WIN32
  return make_function_from_c_win32(func, arg3_is_pp);
#endif
}

Ptr<Function> make_stack_arg_function_from_c(void* func) {
#ifdef __linux__
  return make_stack_arg_function_from_c_systemv(func);
#elif __APPLE__
  return make_stack_arg_function_from_c_systemv(func);
#elif _WIN32
  return make_stack_arg_function_from_c_win32(func);
#endif
}

/*!
 * Create a GOAL function which does nothing and immediately returns.
 */
Ptr<Function> make_nothing_func() {
  auto mem = Ptr<u8>(alloc_heap_object(s7.offset + FIX_SYM_GLOBAL_HEAP,
                                       u32_in_fixed_sym(FIX_SYM_FUNCTION_TYPE), 0x14, UNKNOWN_PP));

  // a single x86-64 ret.
  mem.c()[0] = 0xc3;
  // CacheFlush(mem, 8);
  return mem.cast<Function>();
}

/*!
 * Create a GOAL function which returns 0.
 */
Ptr<Function> make_zero_func() {
  auto mem = Ptr<u8>(alloc_heap_object(s7.offset + FIX_SYM_GLOBAL_HEAP,
                                       u32_in_fixed_sym(FIX_SYM_FUNCTION_TYPE), 0x14, UNKNOWN_PP));
  // xor eax, eax
  mem.c()[0] = 0x31;
  mem.c()[1] = 0xc0;
  // ret
  mem.c()[2] = 0xc3;
  // CacheFlush(mem, 8);
  return mem.cast<Function>();
}

/*!
 * Given a C function and a name, create a GOAL function and store it in the symbol with the given
 * name. This effectively creates a global GOAL function with the given name which calls the given C
 * function.
 *
 * This work on both Linux and Windows, but only supports up to 6 arguments.
 */
Ptr<Function> make_function_symbol_from_c(const char* name, void* f) {
  auto sym = intern_from_c(name);
  auto func = make_function_from_c(f);
  sym->value() = func.offset;
  return func;
}

/*!
 * Like make_function_symbol_from_c, but all 8 GOAL arguments are put into an array on the stack.
 * The address of this array is passed as the first and only argument to f.
 */
Ptr<Function> make_stack_arg_function_symbol_from_c(const char* name, void* f) {
  auto sym = intern_from_c(name);
  auto func = make_stack_arg_function_from_c(f);
  sym->value() = func.offset;
  return func;
}

/*!
 * Set the named symbol to the value. This isn't specific to functions.
 */
u32 make_raw_function_symbol_from_c(const char* name, u32 value) {
  intern_from_c(name)->value() = value;
  return value;
}

/*!
 * Configure a "fixed" symbol to have a given name and value.  The "fixed" symbols are symbols
 * which have their location in the symbol table determined ahead of time and not looked up by the
 * hash function.
 */
Ptr<Symbol4<u32>> set_fixed_symbol(u32 offset, const char* name, u32 value) {
  // grab the symbol directly
  Ptr<Symbol4<u32>> sym(s7.offset + offset);
  ASSERT((sym.offset & 3) == 1);  //
  sym->value() = value;

  // set name of the symbol
  *sym_to_string_ptr(sym).c() = Ptr<String>(make_string_from_c(name));

  // set hash of the symbol
  *sym_to_hash(sym).c() = crc32((const u8*)name, strlen(name));

  NumSymbols++;
  return sym;
}

/*!
 * Search the hash table's fixed area for a symbol.
 * Returns null if we didn't find it.
 */
Ptr<Symbol4<u32>> find_symbol_in_fixed_area(u32 hash, const char* name) {
  for (u32 i = s7.offset; i < s7.offset + FIX_FIXED_SYM_END_OFFSET; i += 4) {
    auto sym = Ptr<Symbol4<u32>>(i);
    if (*sym_to_hash(sym) == hash) {
      if (!strcmp(sym_to_string(sym)->data(), name)) {
        return sym;
      }
    }
  }
  return Ptr<Symbol4<u32>>(0);
}

/*!
 * Do a linear probe from start to end to find a symbol (or a slot for a new symbol).
 * If we run into the end without finding anything, returns 1 to indicate the linear probe needs to
 * wrap around. If we run into a blank space, mark that as the slot. Also search the fixed area for
 * the symbol. If we fail to find it without wrapping, and it's not in the fixed area, return 0.
 */
Ptr<Symbol4<u32>> find_symbol_in_area(u32 hash, const char* name, u32 start, u32 end) {
  for (u32 i = start; i < end; i += 4) {
    auto sym = Ptr<Symbol4<u32>>(i);

    // note - this may break if any symbols hash to zero!
    if (*sym_to_hash(sym) == hash) {
      if (!strcmp(sym_to_string(sym)->data(), name)) {
        return sym;
      }
    }

    if (!(*sym_to_hash(sym))) {
      // open slot!
      // means we don't need to wrap.
      symbol_slot = i;

      // check the fixed area, in case it's not dynamically placed.
      return find_symbol_in_fixed_area(hash, name);
    }
  }

  // we got to the end without finding the symbol or an empty slot.  Return 1 to indicate this.
  return Ptr<Symbol4<u32>>(1);
}

/*!
 * Searches the table for a symbol.  If the symbol is found, returns it.
 * If not, returns 0, but symbol_slot will contain the slot for the symbol.
 * If both are 0, the symbol table is full and you are sad.
 * Also allows you to find the empty pair by searching for _empty_
 */
Ptr<Symbol4<u32>> find_symbol_from_c(const char* name) {
  symbol_slot = 0;  // nowhere to put the symbol yet, clear any old symbol_slot result.
  u32 hash = crc32((const u8*)name, (int)strlen(name));

  // check if we've got the empty pair.
  if (hash == EMPTY_HASH) {
    if (!strcmp(name, "_empty_")) {
      return (s7 + S7_OFF_FIX_SYM_EMPTY_PAIR).cast<Symbol4<u32>>();
    }
  }

  s32 sh1 = hash << 0x13;
  s32 sh2 = sh1 >> 0x10;
  // will be signed, bottom 3 bits 0 (for alignment, symbols are every 8 bytes)
  // upper 16 bits are the same, so we will reach +/- 8 kb around 0.

  if (sh2 > 0) {
    // upper table first.
    auto probe = find_symbol_in_area(hash, name, s7.offset + sh2, LastSymbol.offset);
    if (probe.offset != 1) {
      return probe;
    }

    // overflow!
    probe = find_symbol_in_area(hash, name, SymbolTable2.offset, s7.offset - 0x10);
    if (probe.offset == 1) {
      // uh oh, both overflowed!
      printf("[BIG WARNING] symbol table probe double overflow!\n");
      return find_symbol_in_fixed_area(hash, name);
    } else {
      return probe;
    }

  } else {
    // lower table first
    auto probe = find_symbol_in_area(hash, name, s7.offset + sh2, s7.offset - 0x10);
    if (probe.offset != 1) {
      return probe;
    }

    // overflow!
    probe =
        find_symbol_in_area(hash, name, s7.offset + FIX_FIXED_SYM_END_OFFSET, LastSymbol.offset);
    if (probe.offset == 1) {
      printf("[BIG WARNING] symbol table probe double overflow!\n");
      return find_symbol_in_fixed_area(hash, name);
    } else {
      return probe;
    }
  }
}

/*!
 * Returns a symbol with the given name.  If this is the first time, make a new symbol, otherwise it
 * returns the old one. Basically a LISP symbol intern
 */
Ptr<Symbol4<u32>> intern_from_c(const char* name) {
  auto symbol = find_symbol_from_c(name);
  if (symbol.offset) {
    // already exists, return it!
    return symbol;
  }

  // otherwise, a new symbol!
  symbol = Ptr<Symbol4<u32>>(symbol_slot);

  u32 hash = crc32((const u8*)name, (int)strlen(name));
  auto str = make_string_from_c(name);
  *sym_to_string_ptr(symbol) = Ptr<String>(str);
  *sym_to_hash(symbol) = hash;

  NumSymbols++;
  return symbol;
}

/*!
 * GOAL intern function.
 */
u64 intern(u32 name) {
  return intern_from_c(Ptr<String>(name)->data()).offset;
}

namespace {
u32 size_of_type(u32 method_count) {
  return (4 * method_count + 0x23) & 0xfffffff0;
}
}  // namespace

/*!
 * Given a symbol for the type name, allocate memory for a type and add it to the symbol table.
 * New: in Jak 2, there's a level type list
 */
Ptr<Type> alloc_and_init_type(Ptr<Symbol4<Ptr<Type>>> sym,
                              u32 method_count,
                              bool force_global_type) {
  // number of bytes for this type
  u32 type_size = size_of_type(method_count);
  u32 type_mem = 0;
  ASSERT(sym.offset & 1);

  if (!force_global_type &&
      u32_in_fixed_sym(FIX_SYM_LOADING_LEVEL) != u32_in_fixed_sym(FIX_SYM_GLOBAL_HEAP)) {
    u32 type_list_ptr = LevelTypeList->value();
    if (type_list_ptr == 0) {
      // we don't have a type-list... just alloc on global
      MsgErr("dkernel: trying to init loading level type \'%s\' while type-list is undefined\n",
             sym_to_string(sym)->data());
      type_mem = alloc_heap_object(s7.offset + FIX_SYM_GLOBAL_HEAP,
                                   u32_in_fixed_sym(FIX_SYM_TYPE_TYPE), type_size, UNKNOWN_PP);
    } else {
      // we do have a type list! allocate on the level heap
      type_mem = alloc_heap_object(s7.offset + FIX_SYM_LOADING_LEVEL,
                                   u32_in_fixed_sym(FIX_SYM_TYPE_TYPE), type_size, UNKNOWN_PP);
      // link us!
      u32 old_head = *Ptr<u32>(type_list_ptr);
      *Ptr<u32>(type_list_ptr) = type_mem;
      // I guess we hide this in the memusage method.
      Ptr<Type>(type_mem)->memusage_method.offset = old_head;
    }
  } else {
    // normal global type
    type_mem = alloc_heap_object(s7.offset + FIX_SYM_GLOBAL_HEAP,
                                 u32_in_fixed_sym(FIX_SYM_TYPE_TYPE), type_size, UNKNOWN_PP);
  }

  Ptr<Type> the_type(type_mem);
  sym->value() = the_type;
  the_type->allocated_size = type_size;
  the_type->padded_size = ((type_size + 0xf) & 0xfff0);
  return the_type;
}

/*!
 * Like intern, but returns a type instead of a symbol. If the type doesn't exist, a new one is
 * allocated.
 */
Ptr<Type> intern_type_from_c(const char* name, u64 methods) {
  // there's a weird flag system used here.
  // if methods is a number that's not 0 or 1, its used as the desired number of methods.
  // If method is 0, and a new type needs to be created, it uses 12 methods
  // If method is 1, and a new type needs to be created, it uses 44 methods
  // If method is 0 or 1 and no new type needs to be created, there is no error.
  // Requesting a type to have fewer methods than the existing type has is ok.
  // Requesting a type to have more methods than the existing type is not ok and prints an error.

  auto symbol = intern_from_c(name);
  u32 sym_value = symbol->value();

  if (!sym_value) {
    // new type
    int n_methods = methods;

    if (methods == 0) {
      // some stupid types like user-defined children of integers have "0" as the method count
      n_methods = 0xc;
    } else if (methods == 1) {
      // whatever builds the v2/v4 object files (level data) doesn't actually know method counts.
      // so it just puts a 1.  In this case, we should put lots of methods, just in case.
      // I guess 44 was the number they picked.
      n_methods = 0x2c;
    }

    // create the type.
    auto casted_sym = symbol.cast<Symbol4<Ptr<Type>>>();
    auto type = alloc_and_init_type(casted_sym, n_methods, 0);  // allow level types
    type->symbol = casted_sym;
    type->num_methods = n_methods;
    return type;
  } else {
    // the type exists.
    auto type = Ptr<Type>(sym_value);
    // note - flags of 0 or 1 will pass through here without triggering the error.
    if (size_of_type(type->num_methods) < size_of_type(methods)) {
      MsgErr(
          "dkernel: trying to redefine a type '%s' with %d methods when it had %d, try "
          "restarting\n",
          name, (u32)methods, type->num_methods);
      ASSERT(false);
    }
    return type;
  }
}

/*!
 * Wrapper of intern_type_from_c to use with GOAL. It accepts a gstring as a name.
 */
u64 intern_type(u32 name, u64 methods) {
  return intern_type_from_c(Ptr<String>(name)->data(), methods).offset;
}

/*!
 * Configure a type.
 */
Ptr<Type> set_type_values(Ptr<Type> type, Ptr<Type> parent, u64 flags) {
  type->parent = parent;
  type->allocated_size = (flags & 0xffff);
  type->heap_base = (flags >> 16) & 0xffff;
  type->padded_size = ((type->allocated_size + 0xf) & 0xfff0);

  u16 new_methods = (flags >> 32) & 0xffff;  // i think this accidentally uses jak1 style flags.
  if (type->num_methods < new_methods) {
    type->num_methods = new_methods;
  }

  return type;
}

/*!
 * Setup a type which is located in a fixed spot of the symbol table.
 */
Ptr<Type> set_fixed_type(u32 offset,
                         const char* name,
                         Ptr<Symbol4<Ptr<Type>>> parent_symbol,
                         u64 flags,
                         u32 print,
                         u32 inspect) {
  Ptr<Symbol4<Ptr<Type>>> type_symbol(s7.offset + offset);
  Ptr<Type> symbol_value = type_symbol->value();

  // set the symbol's name and hash
  *sym_to_string_ptr(type_symbol) = Ptr<String>(make_string_from_c(name));
  *sym_to_hash(type_symbol) = crc32((const u8*)name, strlen(name));
  NumSymbols++;

  if (symbol_value.offset == 0) {
    // no type memory exists, let's allocate it. force it global
    // the flag logic here multiplies the method count 2, hopefully
    // this will set up the symbol
    symbol_value = alloc_and_init_type(type_symbol, (flags >> 0x1f) & 0xffff, 1);
  }

  // remember our symbol
  symbol_value->symbol = type_symbol;
  // make our type a type (we're a basic)
  u32 type_of_type = u32_in_fixed_sym(FIX_SYM_TYPE_TYPE);
  *Ptr<u32>(symbol_value.offset - 4) = type_of_type;

  Ptr<Type> parent_type = parent_symbol->value();
  set_type_values(symbol_value, parent_type, flags);

  symbol_value->new_method = parent_type->new_method;
  symbol_value->delete_method = parent_type->delete_method;

  if (!print) {
    symbol_value->print_method = parent_type->print_method;
  } else {
    symbol_value->print_method.offset = print;
  }

  if (!inspect) {
    symbol_value->inspect_method = parent_type->inspect_method;
  } else {
    symbol_value->inspect_method.offset = inspect;
  }

  symbol_value->length_method.offset = u32_in_fixed_sym(FIX_SYM_ZERO_FUNC);
  symbol_value->asize_of_method = parent_type->asize_of_method;
  symbol_value->copy_method = parent_type->copy_method;
  return symbol_value;
}

static bool in_valid_memory_for_new_type(u32 addr) {
  if (SymbolTable2.offset <= addr && addr < 0x8000000) {
    return true;
  }

  if (addr < 0x100000 && addr >= 0x84000) {
    return true;
  }
  return false;
}

static bool is_valid_type(u32 addr) {
  if ((addr & 7) != 4) {
    return false;
  }

  if (*Ptr<u32>(addr - 4) != u32_in_fixed_sym(FIX_SYM_TYPE_TYPE)) {
    return false;
  }

  return true;
}
/*!
 * New method of type. A GOAL (deftype) will end up calling this method.
 * Internally does an intern.
 */
u64 new_type(u32 symbol, u32 parent, u64 flags) {
  u32 n_methods = (flags >> 32) & 0xffff;
  if (n_methods == 0) {
    // 12 methods used as default, if the user has not provided us with a number
    n_methods = 12;
  }

  // copy methods
  u32 parent_num_methods = Ptr<Type>(parent)->num_methods;
  auto new_type_obj =
      Ptr<Type>(intern_type(sym_to_string(Ptr<Symbol4<u32>>(symbol)).offset, n_methods));
  u32 original_type_list_value = new_type_obj->memusage_method.offset;
  Ptr<Function>* child_slots = &(new_type_obj->new_method);
  Ptr<Function>* parent_slots = &(Ptr<Type>(parent)->new_method);
  for (u32 i = 0; i < n_methods; i++) {
    if (i < parent_num_methods) {  // bug fix from jak 1
      child_slots[i] = parent_slots[i];
    } else {
      child_slots[i].offset = 0;
    }
  }

  // deal with loading-level types
  if (u32_in_fixed_sym(FIX_SYM_LOADING_LEVEL) == u32_in_fixed_sym(FIX_SYM_GLOBAL_HEAP)) {
    // not loading a level

    // we'll consider a type list if it's #f or a valid type
    if (original_type_list_value && (original_type_list_value == s7.offset ||
                                     (in_valid_memory_for_new_type(original_type_list_value) &&
                                      is_valid_type(original_type_list_value)))) {
      printf("case 1 for new_type level types\n");
      new_type_obj->memusage_method.offset = original_type_list_value;
    }
  } else {
    if (original_type_list_value == 0) {
      // loading a level, but the type is global
      MsgWarn("dkernel: loading-level init of type %s, but was interned global (this is okay)\n",
              sym_to_string(new_type_obj->symbol)->data());
    } else {
      new_type_obj->memusage_method.offset = original_type_list_value;
    }
  }

  return set_type_values(new_type_obj, Ptr<Type>(parent), flags).offset;
}

/*!
 * Is t1 a t2?
 */
u64 type_typep(Ptr<Type> t1, Ptr<Type> t2) {
  if (t1 == t2) {
    return (s7 + FIX_SYM_TRUE).offset;
  }

  do {
    t1 = t1->parent;
    if (t1 == t2) {
      return (s7 + FIX_SYM_TRUE).offset;
    }
  } while (t1.offset && t1.offset != u32_in_fixed_sym(FIX_SYM_OBJECT_TYPE));
  return s7.offset;
}

u64 method_set(u32 type_, u32 method_id, u32 method) {
  Ptr<Type> type(type_);
  if (method_id > 255)
    printf("[METHOD SET ERROR] tried to set method %d\n", method_id);

  auto existing_method = type->get_method(method_id).offset;

  if (method == 1) {
    method = 0;
  } else if (method == 0) {
    return 0;
  } else if (method == 2) {
    method = type->parent->get_method(method_id).offset;
    printf("[Method Set] got 2, inheriting\n");
  }

  // do the set
  type->get_method(method_id).offset = method;

  // now, propagate to children
  // we don't track children directly, so we end up having to iterate the whole symbol to find all
  // types. This is slow, so we only do it in some cases

  // the condition is either setting *enable-method-set* in GOAL, or if we're debugging without the
  // disk boot. The point of doing this in debug is just to print warning messages.
  if (*EnableMethodSet || (!FastLink && MasterDebug && !DiskBoot)) {
    auto sym = Ptr<Symbol4<Ptr<Type>>>(s7.offset);
    for (; sym.offset < LastSymbol.offset; sym.offset += 4) {
      auto sym_value = sym->value();
      if (in_valid_memory_for_new_type(sym_value.offset) && (sym_value.offset & 7) == 4 &&
          *Ptr<u32>(sym_value.offset - 4) == u32_in_fixed_sym(FIX_SYM_TYPE_TYPE) &&
          method_id < sym_value->num_methods &&
          sym_value->get_method(method_id).offset == existing_method &&
          type_typep(sym_value, type) != s7.offset) {
        if (FastLink != 0) {
          printf("************ WARNING **************\n");
          printf("method %d of %s redefined - you must define class heirarchies in order now\n",
                 method_id, sym_to_string(sym)->data());
          printf("***********************************\n");
        }
        sym_value->get_method(method_id).offset = method;
      }
    }

    sym = Ptr<Symbol4<Ptr<Type>>>(SymbolTable2.offset);
    for (; sym.offset < s7.offset; sym.offset += 4) {
      auto sym_value = sym->value();
      if (in_valid_memory_for_new_type(sym_value.offset) && (sym_value.offset & 7) == 4 &&
          *Ptr<u32>(sym_value.offset - 4) == u32_in_fixed_sym(FIX_SYM_TYPE_TYPE) &&
          method_id < sym_value->num_methods &&
          sym_value->get_method(method_id).offset == existing_method &&
          type_typep(sym_value, type) != s7.offset) {
        if (FastLink != 0) {
          printf("************ WARNING **************\n");
          printf("method %d of %s redefined - you must define class heirarchies in order now\n",
                 method_id, sym_to_string(sym)->data());
          printf("***********************************\n");
        }
        sym_value->get_method(method_id).offset = method;
      }
    }
  }

  return method;
}

/*!
 * Call a GOAL method of a given type.
 */
u64 call_method_of_type(u32 arg, Ptr<Type> type, u32 method_id) {
  if (((type.offset < SymbolTable2.offset || 0x7ffffff < type.offset) &&  // not in normal memory
       (type.offset < 0x84000 || 0x100000 <= type.offset))                // not in kernel memory
      || ((type.offset & OFFSET_MASK) != BASIC_OFFSET)) {                 // invalid type
    cprintf("#<#%x has invalid type ptr #x%x>\n", arg, type.offset);
  } else {
    auto type_tag = Ptr<Ptr<Type>>(type.offset - 4);
    if ((*type_tag).offset == u32_in_fixed_sym(FIX_SYM_TYPE_TYPE)) {
      auto f = type->get_method(method_id);
      return call_goal(f, arg, 0, 0, s7.offset, g_ee_main_mem);
    } else {
      cprintf("#<#x%x has invalid type ptr #x%x, bad type #x%x>\n", arg, type.offset,
              (*type_tag).offset);
    }
  }
  printf("[ERROR] call_method_of_type failed!\n");
  return arg;
}

/*!
 * Call a GOAL function with 2 arguments.
 */
u64 call_goal_function_arg2(Ptr<Function> func, u64 a, u64 b) {
  return call_goal(func, a, b, 0, s7.offset, g_ee_main_mem);
}

/*!
 * Call a global GOAL function by name.
 */
u64 call_goal_function_by_name(const char* name) {
  return call_goal_function(Ptr<Function>(intern_from_c(name)->value()));
}

/*!
 * Like call_method_of_type, but has two arguments. Used to "relocate" v2/s4 loads.
 */
u64 call_method_of_type_arg2(u32 arg, Ptr<Type> type, u32 method_id, u32 a1, u32 a2) {
  if (((type.offset < SymbolTable2.offset || 0x7ffffff < type.offset) &&  // not in normal memory
       (type.offset < 0x84000 || 0x100000 <= type.offset))                // not in kernel memory
      || ((type.offset & OFFSET_MASK) != BASIC_OFFSET)) {                 // invalid type
    cprintf("#<#%x has invalid type ptr #x%x>\n", arg, type.offset);
  } else {
    auto type_tag = Ptr<Ptr<Type>>(type.offset - 4);
    if ((*type_tag).offset == u32_in_fixed_sym(FIX_SYM_TYPE_TYPE)) {
      // return type->get_method(method_id).cast<u64 (u32,u32,u32)>().c()(arg,a1,a2);
      return call_goal(type->get_method(method_id), arg, a1, a2, s7.offset, g_ee_main_mem);
    } else {
      cprintf("#<#x%x has invalid type ptr #x%x, bad type #x%x>\n", arg, type.offset,
              (*type_tag).offset);
    }
  }
  ASSERT_MSG(false, "[ERROR] call_method_of_type_arg2 failed!");
  return arg;
}

u64 print_object(u32 obj);
u64 print_pair(u32 obj);
u64 print_symbol(u32 obj);

/*!
 * Print an object with a newline after it to the GOAL PrintBuffer (not stdout)
 */
u64 sprint(u32 obj) {
  auto rv = print_object(obj);
  cprintf("\n");
  return rv;
}

/*!
 * Most generic printing method.
 * Does not correctly handle 64 bit boxed integers or object64's correctly.
 * It is important that no objects of type object actually exist or this will loop!
 */
u64 print_object(u32 obj) {
  if ((obj & OFFSET_MASK) == BINTEGER_OFFSET) {
    return print_binteger(s64(s32(obj)));
  } else {
    if ((obj < SymbolTable2.offset || 0x7ffffff < obj) &&  // not in normal memory
        (obj < 0x84000 || 0x100000 <= obj)) {              // not in kernel memory
      cprintf("#<invalid object #x%x>", obj);
    } else if ((obj & OFFSET_MASK) == PAIR_OFFSET) {
      return print_pair(obj);
    } else if ((obj & 1) == SYMBOL_OFFSET && obj >= SymbolTable2.offset &&
               obj < LastSymbol.offset) {
      return print_symbol(obj);
    } else if ((obj & OFFSET_MASK) == BASIC_OFFSET) {
      return call_method_of_type(obj, Ptr<Type>(*Ptr<u32>(obj - 4)), GOAL_PRINT_METHOD);
    } else {
      cprintf("#<unknown type %d @ #x%x>", obj & OFFSET_MASK, obj);
    }
  }
  return obj;
}

/*!
 * Default print method a basic.
 * Confirms basic is valid and prints the type name.
 */
u64 print_basic(u32 obj) {
  if (((obj < SymbolTable2.offset || 0x7ffffff < obj) &&  // not in normal memory
       (obj < 0x84000 || 0x100000 <= obj))                // not in kernel memory
      || ((obj & OFFSET_MASK) != BASIC_OFFSET)) {
    cprintf("#<invalid basic #x%x>", obj);
  } else {
    cprintf("#<%s @ #x%x>", sym_to_string(Ptr<Type>(*Ptr<u32>(obj - 4))->symbol)->data(), obj);
  }
  return obj;
}

/*!
 * Print a pair as a LISP list.  Don't try to print circular lists or it will get stuck
 * Can print improper lists
 */
u64 print_pair(u32 obj) {
  if (obj == s7.offset + S7_OFF_FIX_SYM_EMPTY_PAIR) {
    cprintf("()");
  } else {
    // clang-format off
    // we want to treat ('quote <foo>) as just '<foo> unless
    if (CollapseQuote->value() == s7.offset   // CollapseQuote is enabled
        || ((obj & 7) != 2)                   // this object isn't a pair
        || *Ptr<u32>(obj - 2) != s7.offset + FIX_SYM_QUOTE // the car isn't 'quote
        || (*Ptr<u32>(obj + 2) & 7) != 2                   // the cdr isn't a pair
        || *Ptr<u32>(*Ptr<u32>(obj + 2) + 2) != s7.offset + S7_OFF_FIX_SYM_EMPTY_PAIR  // the cddr isn't '()
        ) {
      // clang-format on
      cprintf("(");
      auto toPrint = obj;
      for (;;) {
        if ((toPrint & OFFSET_MASK) == PAIR_OFFSET) {
          // print CAR
          print_object(*Ptr<u32>(toPrint - 2));

          // load up CDR
          auto cdr = *Ptr<u32>(toPrint + 2);
          toPrint = cdr;
          if (cdr == s7.offset + S7_OFF_FIX_SYM_EMPTY_PAIR) {  // end of proper list
            cprintf(")");
            return obj;
          } else {  // continue list
            cprintf(" ");
          }
        } else {  // improper list
          cprintf(". ");
          print_object(toPrint);
          cprintf(")");
          return obj;
        }
      }
    } else {
      cprintf("'");
      print_object(*Ptr<u32>(*Ptr<u32>(obj + 2) - 2));
    }
  }
  return obj;
}

/*!
 * Print method for symbol.  Just prints the name without quotes or anything fancy.
 */
u64 print_symbol(u32 obj) {
  if (((obj < SymbolTable2.offset || 0x7ffffff < obj) &&  // not in normal memory
       (obj < 0x84000 || 0x100000 <= obj))                // not in kernel memory
      || ((obj & 1) != 1) || obj < SymbolTable2.offset || obj >= LastSymbol.offset) {
    cprintf("#<invalid symbol #x%x>", obj);
  } else {
    char* str = sym_to_string(Ptr<Symbol4<u32>>(obj))->data();
    cprintf("%s", str);
  }
  return obj;
}

/*!
 * Print method for type.  Just prints the name without quotes
 */
u64 print_type(u32 obj) {
  if (((obj < SymbolTable2.offset || 0x7ffffff < obj) &&  // not in normal memory
       (obj < 0x84000 || 0x100000 <= obj))                // not in kernel memory
      || ((obj & OFFSET_MASK) != BASIC_OFFSET) ||
      *Ptr<u32>(obj - 4) != u32_in_fixed_sym(FIX_SYM_TYPE_TYPE)) {
    cprintf("#<invalid type #x%x>", obj);
  } else {
    cprintf("%s", sym_to_string(Ptr<Type>(obj)->symbol)->data());
  }
  return obj;
}

/*!
 * Print method for string.  Prints the string in quotes.
 */
u64 print_string(u32 obj) {
  if (((obj < SymbolTable2.offset || 0x7ffffff < obj) &&  // not in normal memory
       (obj < 0x84000 || 0x100000 <= obj))                // not in kernel memory
      || ((obj & OFFSET_MASK) != BASIC_OFFSET) ||
      *Ptr<u32>(obj - 4) != u32_in_fixed_sym(FIX_SYM_STRING_TYPE)) {
    if (obj == s7.offset) {
      cprintf("#f");  // new in jak 2.

    } else {
      cprintf("#<invalid string #x%x>", obj);
    }
  } else {
    cprintf("\"%s\"", Ptr<String>(obj)->data());
  }
  return obj;
}

/*!
 * Print method for function. Just prints the address because functions can't identify themselves.
 */
u64 print_function(u32 obj) {
  cprintf("#<compiled %s @ #x%x>", sym_to_string(Ptr<Type>(*Ptr<u32>(obj - 4))->symbol)->data(),
          obj);
  return obj;
}

/*!
 * Get the allocated size field of a basic.  By default we grab this from the type struct.
 * Dynamically sized basics should override this method.
 */
u64 asize_of_basic(u32 it) {
  return Ptr<Type>(*Ptr<u32>(it - BASIC_OFFSET))->allocated_size;
}

/*!
 * Create a copy of a basic.  If the destination isn't identified as a symbol, treat it as an
 * address. This seems a little bit unsafe to me, as it reads the 4-bytes before the given address
 * and checks it against the symbol type pointer to see if its a symbol. It seems possible to have a
 * false positive for this check.
 */
u64 copy_basic(u32 obj, u32 heap, u32 /*unused*/, u32 pp) {
  // determine size of basic. We call a method instead of using asize_of_basic in case the type has
  // overridden the default asize_of method.
  u32 size = call_method_of_type(obj, Ptr<Type>(*Ptr<u32>(obj - BASIC_OFFSET)), GOAL_ASIZE_METHOD);
  u32 result;

  if ((heap & 1) == 1) {
    // we think we're creating a new copy on a heap.  First allocate memory...
    result = alloc_heap_object(heap, *Ptr<u32>(obj - BASIC_OFFSET), size, pp);
    // then copy! (minus the type tag, alloc_heap_object already did it for us)
    memcpy(Ptr<u32>(result).c(), Ptr<u32>(obj).c(), size - BASIC_OFFSET);
  } else {
    printf("DANGER COPY BASIC!\n");
    // copy directly (including type tag)
    memcpy(Ptr<u32>(heap - BASIC_OFFSET).c(), Ptr<u32>(obj - BASIC_OFFSET).c(), size);
    result = heap;
  }
  return result;
}

u64 inspect_pair(u32 obj);
u64 inspect_symbol(u32 obj);
/*!
 * Highest level inspect method. Won't inspect 64-bit bintegers correctly.
 */
u64 inspect_object(u32 obj) {
  if ((obj & OFFSET_MASK) == BINTEGER_OFFSET) {
    return inspect_binteger(obj);
  } else {
    if ((obj < SymbolTable2.offset || 0x7ffffff < obj) &&  // not in normal memory
        (obj < 0x84000 || 0x100000 <= obj)) {              // not in kernel memory
      cprintf("#<invalid object #x%x>", obj);
    } else if ((obj & OFFSET_MASK) == PAIR_OFFSET) {
      return inspect_pair(obj);
    } else if ((obj & 1) == SYMBOL_OFFSET && obj >= SymbolTable2.offset &&
               obj < LastSymbol.offset) {
      return inspect_symbol(obj);
    } else if ((obj & OFFSET_MASK) == BASIC_OFFSET) {
      return call_method_of_type(obj, Ptr<Type>(*Ptr<u32>(obj - BASIC_OFFSET)),
                                 GOAL_INSPECT_METHOD);
    } else {
      cprintf("#<unknown type %d @ #x%x>", obj & OFFSET_MASK, obj);
    }
  }
  return obj;
}

/*!
 * Inspect a pair.
 */
u64 inspect_pair(u32 obj) {
  cprintf("[%8x] pair ", obj);
  print_pair(obj);
  cprintf("\n");
  return obj;
}

/*!
 * Inspect a string. There's a typo in allocated_length (has underscore instead of dash).
 * This typo is fixed in later games.
 */
u64 inspect_string(u32 obj) {
  if (((obj < SymbolTable2.offset || 0x7ffffff < obj) &&  // not in normal memory
       (obj < 0x84000 || 0x100000 <= obj))                // not in kernel memory
      || ((obj & OFFSET_MASK) != BASIC_OFFSET) ||
      *Ptr<u32>(obj - 4) != u32_in_fixed_sym(FIX_SYM_STRING_TYPE)) {
    cprintf("#<invalid string #x%x>\n", obj);
  } else {
    auto str = Ptr<String>(obj);
    cprintf("[%8x] string\n\tallocated_length: %d\n\tdata: \"%s\"\n", obj, str->len, str->data());
  }
  return obj;
}

/*!
 * Inspect a symbol.
 */
u64 inspect_symbol(u32 obj) {
  if (((obj < SymbolTable2.offset || 0x7ffffff < obj) &&  // not in normal memory
       (obj < 0x84000 || 0x100000 <= obj))                // not in kernel memory
      || ((obj & 1) != 1) || obj < SymbolTable2.offset || obj >= LastSymbol.offset) {
    cprintf("#<invalid symbol #x%x>", obj);
  } else {
    auto sym = Ptr<Symbol4<u32>>(obj);
    cprintf("[%8x] symbol\n\tname: %s\n\thash: #x%x\n\tvalue: ", obj, sym_to_string(sym)->data(),
            *sym_to_hash(sym));
    print_object(sym->value());
    cprintf("\n");
  }
  return obj;
}

/*!
 * Inspect a type.
 */
u64 inspect_type(u32 obj) {
  if (((obj < SymbolTable2.offset || 0x7ffffff < obj) &&  // not in normal memory
       (obj < 0x84000 || 0x100000 <= obj))                // not in kernel memory
      || ((obj & OFFSET_MASK) != BASIC_OFFSET) ||
      *Ptr<u32>(obj - 4) != u32_in_fixed_sym(FIX_SYM_TYPE_TYPE)) {
    cprintf("#<invalid type #x%x>\n", obj);
  } else {
    auto typ = Ptr<Type>(obj);
    auto sym = typ->symbol;

    cprintf("[%8x] type\n\tname: %s\n\tparent: ", obj, sym_to_string(sym)->data());
    print_object(typ->parent.offset);
    cprintf("\n\tsize: %d/%d\n\theap-base: %d\n\tallocated-length: %d\n\tprint: ",
            typ->allocated_size, typ->padded_size, typ->heap_base, typ->num_methods);
    print_object(typ->print_method.offset);
    cprintf("\n\tinspect: ");
    print_object(typ->inspect_method.offset);
    cprintf("\n");
  }
  return obj;
}

/*!
 * Inspect a basic. This is just a fallback for basics which don't know how to inspect themselves.
 * We just use print_object.
 */
u64 inspect_basic(u32 obj) {
  if (((obj < SymbolTable2.offset || 0x7ffffff < obj) &&  // not in normal memory
       (obj < 0x84000 || 0x100000 <= obj))                // not in kernel memory
      || ((obj & OFFSET_MASK) != BASIC_OFFSET)) {
    if (obj == s7.offset) {
      // added in jak2
      return inspect_symbol(obj);
    } else {
      cprintf("#<invalid basic #x%x>\n", obj);
    }
  } else {
    cprintf("[%8x] ", obj);
    print_object(*Ptr<u32>(obj - 4));
    cprintf("\n");
  }
  return obj;
}

/*!
 * Inspect a link block. This link block doesn't seem to be used at all.
 */
u64 inspect_link_block(u32 ob) {
  struct LinkBlock {
    u32 length;
    u32 version;
  };

  auto lb = Ptr<LinkBlock>(ob);
  cprintf("[%8x] link-block\n\tallocated_length: %d\n\tversion: %d\n\tfunction: ", ob, lb->length,
          lb->version);
  print_object(ob + lb->length);
  cprintf("\n");
  return ob;
}

namespace {
Ptr<Symbol4<Ptr<Type>>> get_fixed_type_symbol(u32 offset) {
  return (s7 + offset).cast<Symbol4<Ptr<Type>>>();
}

u64 pack_type_flag(u64 methods, u64 heap_base, u64 size) {
  return (methods << 32) + (heap_base << 16) + (size);
}
}  // namespace

int InitHeapAndSymbol() {
  // allocate memory for all 3 tables
  Ptr<u32> symbol_table =
      kmalloc(kglobalheap, jak2::SYM_TABLE_MEM_SIZE, KMALLOC_MEMSET, "symbol-table").cast<u32>();
  // setup table pointers
  LastSymbol = symbol_table + 0xff00;
  SymbolTable2 = symbol_table + 5;
  s7 = symbol_table + 0x8001;
  NumSymbols = 0;

  // inform compiler of s7
  reset_output();

  // empty pair (this is extra confusing).
  *Ptr<u32>(s7.offset + FIX_SYM_EMPTY_CAR - 1) = s7.offset + S7_OFF_FIX_SYM_EMPTY_PAIR;
  *Ptr<u32>(s7.offset + FIX_SYM_EMPTY_CDR - 1) = s7.offset + S7_OFF_FIX_SYM_EMPTY_PAIR;

  // 'global
  fixed_sym_set(FIX_SYM_GLOBAL_HEAP, kglobalheap.offset);

  // basic types required to actually set up symbols
  alloc_and_init_type((s7 + FIX_SYM_TYPE_TYPE).cast<Symbol4<Ptr<Type>>>(), 9, true);
  alloc_and_init_type((s7 + FIX_SYM_SYMBOL_TYPE).cast<Symbol4<Ptr<Type>>>(), 9, true);
  alloc_and_init_type((s7 + FIX_SYM_STRING_TYPE).cast<Symbol4<Ptr<Type>>>(), 9, true);
  alloc_and_init_type((s7 + FIX_SYM_FUNCTION_TYPE).cast<Symbol4<Ptr<Type>>>(), 9, true);

  // set up symbols!
  set_fixed_symbol(FIX_SYM_FALSE, "#f", s7.offset + FIX_SYM_FALSE);
  set_fixed_symbol(FIX_SYM_TRUE, "#t", s7.offset + FIX_SYM_TRUE);
  set_fixed_symbol(FIX_SYM_NOTHING_FUNC, "nothing", make_nothing_func().offset);
  set_fixed_symbol(FIX_SYM_ZERO_FUNC, "zero-func", make_zero_func().offset);
  set_fixed_symbol(FIX_SYM_ASIZE_OF_BASIC_FUNC, "asize-of-basic-func",
                   make_function_from_c((void*)asize_of_basic).offset);
  set_fixed_symbol(FIX_SYM_COPY_BASIC_FUNC, "asize-of-basic-func",
                   make_function_from_c((void*)copy_basic, true).offset);
  set_fixed_symbol(FIX_SYM_DELETE_BASIC, "delete-basic",
                   make_function_from_c((void*)delete_basic).offset);
  set_fixed_symbol(FIX_SYM_GLOBAL_HEAP, "global", kglobalheap.offset);
  set_fixed_symbol(FIX_SYM_DEBUG, "debug", kdebugheap.offset);
  set_fixed_symbol(FIX_SYM_STATIC, "static", s7.offset + FIX_SYM_STATIC);
  set_fixed_symbol(FIX_SYM_LOADING_LEVEL, "loading-level", kglobalheap.offset);
  set_fixed_symbol(FIX_SYM_LOADING_PACKAGE, "loading-package", kglobalheap.offset);
  set_fixed_symbol(FIX_SYM_PROCESS_LEVEL_HEAP, "process-level-heap", kglobalheap.offset);
  set_fixed_symbol(FIX_SYM_STACK, "stack", s7.offset + FIX_SYM_STACK);
  set_fixed_symbol(FIX_SYM_SCRATCH, "scratch", s7.offset + FIX_SYM_SCRATCH);
  set_fixed_symbol(FIX_SYM_SCRATCH_TOP, "*scratch-top*", 0x70000000);
  set_fixed_symbol(FIX_SYM_LEVEL, "level", 0);
  set_fixed_symbol(FIX_SYM_ART_GROUP, "art-group", 0);
  set_fixed_symbol(FIX_SYM_TEXTURE_PAGE_DIR, "texture-page-dir", 0);
  set_fixed_symbol(FIX_SYM_TEXTURE_PAGE, "texture-page", 0);
  set_fixed_symbol(FIX_SYM_SOUND, "sound", 0);
  set_fixed_symbol(FIX_SYM_DGO, "dgo", 0);
  set_fixed_symbol(FIX_SYM_TOP_LEVEL, "top-level", u32_in_fixed_sym(FIX_SYM_NOTHING_FUNC));
  set_fixed_symbol(FIX_SYM_QUOTE, "quote", s7.offset + FIX_SYM_QUOTE);

  // set up types
  auto new_illegal_func = make_function_from_c((void*)new_illegal);
  auto delete_illegal_func = make_function_from_c((void*)delete_illegal);
  auto print_object_func = make_function_from_c((void*)print_object);
  auto inspect_object_func = make_function_from_c((void*)inspect_object);

  // OBJECT
  set_fixed_type(FIX_SYM_OBJECT_TYPE, "object", get_fixed_type_symbol(FIX_SYM_OBJECT_TYPE),
                 pack_type_flag(9, 0, 4), print_object_func.offset, inspect_object_func.offset);
  auto object_type = Ptr<Type>(u32_in_fixed_sym(FIX_SYM_OBJECT_TYPE));
  object_type->new_method = Ptr<Function>(u32_in_fixed_sym(FIX_SYM_NOTHING_FUNC));  // new for jak 2
  object_type->delete_method = delete_illegal_func;
  object_type->asize_of_method = Ptr<Function>(u32_in_fixed_sym(FIX_SYM_NOTHING_FUNC));
  object_type->copy_method = make_function_from_c((void*)copy_fixed);

  // STRUCTURE
  auto structure_type =
      set_fixed_type(FIX_SYM_STRUCTURE, "structure", get_fixed_type_symbol(FIX_SYM_OBJECT_TYPE),
                     pack_type_flag(9, 0, 4), make_function_from_c((void*)print_structure).offset,
                     make_function_from_c((void*)inspect_structure).offset);
  structure_type->new_method = make_function_from_c((void*)new_structure);
  structure_type->delete_method = make_function_from_c((void*)delete_structure);

  // BASIC
  auto basic_type =
      set_fixed_type(FIX_SYM_BASIC, "basic", get_fixed_type_symbol(FIX_SYM_STRUCTURE),
                     pack_type_flag(9, 0, 4), make_function_from_c((void*)print_basic).offset,
                     make_function_from_c((void*)inspect_basic).offset);
  basic_type->new_method = make_function_from_c((void*)new_basic, true);
  basic_type->delete_method = Ptr<Function>(u32_in_fixed_sym(FIX_SYM_DELETE_BASIC));
  basic_type->asize_of_method = Ptr<Function>(u32_in_fixed_sym(FIX_SYM_ASIZE_OF_BASIC_FUNC));
  basic_type->copy_method = Ptr<Function>(u32_in_fixed_sym(FIX_SYM_COPY_BASIC_FUNC));

  // SYMBOL
  set_fixed_type(FIX_SYM_SYMBOL_TYPE, "symbol", get_fixed_type_symbol(FIX_SYM_OBJECT_TYPE),
                 pack_type_flag(9, 0, 4), make_function_from_c((void*)print_symbol).offset,
                 make_function_from_c((void*)inspect_symbol).offset);
  auto sym_type = Ptr<Type>(u32_in_fixed_sym(FIX_SYM_SYMBOL_TYPE));
  sym_type->new_method = new_illegal_func;
  sym_type->delete_method = delete_illegal_func;

  // TYPE
  set_fixed_type(FIX_SYM_TYPE_TYPE, "type", get_fixed_type_symbol(FIX_SYM_BASIC),
                 pack_type_flag(9, 0, 0x38), make_function_from_c((void*)print_type).offset,
                 make_function_from_c((void*)inspect_type).offset);
  auto type_type = Ptr<Type>(u32_in_fixed_sym(FIX_SYM_TYPE_TYPE));
  type_type->new_method = make_function_from_c((void*)new_type);
  type_type->delete_method = delete_illegal_func;

  // STRING
  set_fixed_type(FIX_SYM_STRING_TYPE, "string", get_fixed_type_symbol(FIX_SYM_BASIC),
                 pack_type_flag(9, 0, 8), make_function_from_c((void*)print_string).offset,
                 make_function_from_c((void*)inspect_string).offset);

  // FUNCTION
  set_fixed_type(FIX_SYM_FUNCTION_TYPE, "function", get_fixed_type_symbol(FIX_SYM_BASIC),
                 pack_type_flag(9, 0, 4), make_function_from_c((void*)print_function).offset, 0);
  auto function_type = Ptr<Type>(u32_in_fixed_sym(FIX_SYM_FUNCTION_TYPE));
  function_type->new_method = new_illegal_func;
  function_type->delete_method = delete_illegal_func;

  // VU FUNCTION
  set_fixed_type(FIX_SYM_VU_FUNCTION, "vu-function", get_fixed_type_symbol(FIX_SYM_STRUCTURE),
                 pack_type_flag(9, 0, 0x16), make_function_from_c((void*)print_vu_function).offset,
                 make_function_from_c((void*)inspect_vu_function).offset);
  Ptr<Type>(u32_in_fixed_sym(FIX_SYM_VU_FUNCTION))->delete_method = delete_illegal_func;

  // LINK BLOCK
  set_fixed_type(FIX_SYM_LINK_BLOCK, "link-block", get_fixed_type_symbol(FIX_SYM_BASIC),
                 pack_type_flag(9, 0, 0xc), 0,
                 make_function_from_c((void*)inspect_link_block).offset);
  auto link_block_type = Ptr<Type>(u32_in_fixed_sym(FIX_SYM_LINK_BLOCK));
  link_block_type->new_method = new_illegal_func;
  link_block_type->delete_method = delete_illegal_func;

  // KHEAP
  set_fixed_type(FIX_SYM_HEAP, "kheap", get_fixed_type_symbol(FIX_SYM_STRUCTURE),
                 pack_type_flag(9, 0, 0x10), 0, make_function_from_c((void*)inspect_kheap).offset);

  // ARRAY
  set_fixed_type(FIX_SYM_ARRAY, "array", get_fixed_type_symbol(FIX_SYM_BASIC),
                 pack_type_flag(9, 0, 0x10), 0, 0);

  // PAIR
  set_fixed_type(FIX_SYM_PAIR_TYPE, "pair", get_fixed_type_symbol(FIX_SYM_OBJECT_TYPE),
                 pack_type_flag(9, 0, 8), make_function_from_c((void*)print_pair).offset,
                 make_function_from_c((void*)inspect_pair).offset);
  Ptr<Type>(u32_in_fixed_sym(FIX_SYM_PAIR_TYPE))->new_method =
      make_function_from_c((void*)new_pair);
  Ptr<Type>(u32_in_fixed_sym(FIX_SYM_PAIR_TYPE))->delete_method =
      make_function_from_c((void*)delete_pair);

  // KERNEL
  set_fixed_type(FIX_SYM_PROCESS_TREE, "process-tree", get_fixed_type_symbol(FIX_SYM_BASIC),
                 pack_type_flag(0xe, 0, 0x24), 0, 0);
  set_fixed_type(FIX_SYM_PROCESS_TYPE, "process", get_fixed_type_symbol(FIX_SYM_PROCESS_TREE),
                 pack_type_flag(0xe, 0, 0x80), 0, 0);
  set_fixed_type(FIX_SYM_THREAD, "thread", get_fixed_type_symbol(FIX_SYM_BASIC),
                 pack_type_flag(0xc, 0, 0x28), 0, 0);
  set_fixed_type(FIX_SYM_CONNECTABLE, "connectable", get_fixed_type_symbol(FIX_SYM_STRUCTURE),
                 pack_type_flag(9, 0, 0x10), 0, 0);
  set_fixed_type(FIX_SYM_STACK_FRAME, "stack-frame", get_fixed_type_symbol(FIX_SYM_BASIC),
                 pack_type_flag(9, 0, 0xc), 0, 0);
  set_fixed_type(FIX_SYM_FILE_STREAM, "file-stream", get_fixed_type_symbol(FIX_SYM_BASIC),
                 pack_type_flag(9, 0, 0x14), 0, 0);

  // POINTER
  set_fixed_type(FIX_SYM_POINTER, "pointer", get_fixed_type_symbol(FIX_SYM_OBJECT_TYPE),
                 pack_type_flag(9, 0, 4), 0, 0);
  Ptr<Type>(u32_in_fixed_sym(FIX_SYM_POINTER))->new_method = new_illegal_func;

  // NUMERIC
  set_fixed_type(FIX_SYM_NUMBER, "number", get_fixed_type_symbol(FIX_SYM_OBJECT_TYPE),
                 pack_type_flag(9, 0, 8), make_function_from_c((void*)print_integer).offset,
                 make_function_from_c((void*)inspect_integer).offset);
  Ptr<Type>(u32_in_fixed_sym(FIX_SYM_NUMBER))->new_method = new_illegal_func;

  set_fixed_type(FIX_SYM_FLOAT, "float", get_fixed_type_symbol(FIX_SYM_NUMBER),
                 pack_type_flag(9, 0, 4), make_function_from_c((void*)print_float).offset,
                 make_function_from_c((void*)inspect_float).offset);

  set_fixed_type(FIX_SYM_INTEGER, "integer", get_fixed_type_symbol(FIX_SYM_NUMBER),
                 pack_type_flag(9, 0, 8), 0, 0);

  set_fixed_type(FIX_SYM_BINTEGER, "binteger", get_fixed_type_symbol(FIX_SYM_INTEGER),
                 pack_type_flag(9, 0, 8), make_function_from_c((void*)print_binteger).offset,
                 make_function_from_c((void*)inspect_binteger).offset);

  set_fixed_type(FIX_SYM_SINTEGER, "sinteger", get_fixed_type_symbol(FIX_SYM_INTEGER),
                 pack_type_flag(9, 0, 8), 0, 0);
  set_fixed_type(FIX_SYM_INT8, "int8", get_fixed_type_symbol(FIX_SYM_SINTEGER),
                 pack_type_flag(9, 0, 1), 0, 0);
  set_fixed_type(FIX_SYM_INT16, "int16", get_fixed_type_symbol(FIX_SYM_SINTEGER),
                 pack_type_flag(9, 0, 2), 0, 0);
  set_fixed_type(FIX_SYM_INT32, "int32", get_fixed_type_symbol(FIX_SYM_SINTEGER),
                 pack_type_flag(9, 0, 4), 0, 0);
  set_fixed_type(FIX_SYM_INT64, "int64", get_fixed_type_symbol(FIX_SYM_SINTEGER),
                 pack_type_flag(9, 0, 8), 0, 0);
  set_fixed_type(FIX_SYM_INT128, "int128", get_fixed_type_symbol(FIX_SYM_SINTEGER),
                 pack_type_flag(9, 0, 16), 0, 0);

  set_fixed_type(FIX_SYM_UINTEGER, "uinteger", get_fixed_type_symbol(FIX_SYM_INTEGER),
                 pack_type_flag(9, 0, 8), 0, 0);
  set_fixed_type(FIX_SYM_UINT8, "uint8", get_fixed_type_symbol(FIX_SYM_UINTEGER),
                 pack_type_flag(9, 0, 1), 0, 0);
  set_fixed_type(FIX_SYM_UINT16, "uint16", get_fixed_type_symbol(FIX_SYM_UINTEGER),
                 pack_type_flag(9, 0, 2), 0, 0);
  set_fixed_type(FIX_SYM_UINT32, "uint32", get_fixed_type_symbol(FIX_SYM_UINTEGER),
                 pack_type_flag(9, 0, 4), 0, 0);
  set_fixed_type(FIX_SYM_UINT64, "uint64", get_fixed_type_symbol(FIX_SYM_UINTEGER),
                 pack_type_flag(9, 0, 8), 0, 0);
  set_fixed_type(FIX_SYM_UINT128, "uint128", get_fixed_type_symbol(FIX_SYM_UINTEGER),
                 pack_type_flag(9, 0, 16), 0, 0);

  Ptr<Type>(u32_in_fixed_sym(FIX_SYM_OBJECT_TYPE))->new_method =
      make_function_from_c((void*)alloc_heap_object, true);

  make_function_symbol_from_c("string->symbol", (void*)intern);
  make_function_symbol_from_c("print", (void*)sprint);
  make_function_symbol_from_c("inspect", (void*)inspect_object);
  make_function_symbol_from_c("load", (void*)load);
  make_function_symbol_from_c("loadb", (void*)loadb);
  make_function_symbol_from_c("loado", (void*)loado);
  make_function_symbol_from_c("unload", (void*)unload);
  make_stack_arg_function_symbol_from_c("_format", (void*)format_impl_jak2);
  make_function_symbol_from_c("malloc", (void*)alloc_heap_memory);
  make_function_symbol_from_c("kmalloc", (void*)goal_malloc);
  make_function_symbol_from_c("kmemopen", (void*)kmemopen);
  make_function_symbol_from_c("kmemclose", (void*)kmemclose);
  make_function_symbol_from_c("new-dynamic-structure", (void*)new_dynamic_structure);
  make_function_symbol_from_c("method-set!", (void*)method_set);
  make_stack_arg_function_symbol_from_c("link", (void*)link_and_exec_wrapper);
  make_function_symbol_from_c("link-busy?", (void*)link_busy);
  make_function_symbol_from_c("link-reset", (void*)link_reset);
  make_function_symbol_from_c("dgo-load", (void*)load_and_link_dgo);
  make_raw_function_symbol_from_c("ultimate-memcpy", 0);
  make_raw_function_symbol_from_c("memcpy-and-rellink", 0);
  make_raw_function_symbol_from_c("symlink2", 0);
  make_raw_function_symbol_from_c("symlink3", 0);
  make_stack_arg_function_symbol_from_c("link-begin", (void*)link_begin);
  make_function_symbol_from_c("link-resume", (void*)link_resume);
  make_function_symbol_from_c("sql-query", (void*)sql_query_sync);
  make_function_symbol_from_c("mc-run", (void*)MC_run);
  make_function_symbol_from_c("mc-format", (void*)MC_format);
  make_function_symbol_from_c("mc-unformat", (void*)MC_unformat);
  make_function_symbol_from_c("mc-create-file", (void*)MC_createfile);
  make_function_symbol_from_c("mc-save", (void*)MC_save);
  make_function_symbol_from_c("mc-load", (void*)MC_load);
  make_function_symbol_from_c("mc-check-result", (void*)MC_check_result);
  make_function_symbol_from_c("mc-get-slot-info", (void*)MC_get_status);
  make_function_symbol_from_c("mc-makefile", (void*)MC_makefile);
  make_function_symbol_from_c("kset-language", (void*)MC_set_language);

  auto ds_symbol = intern_from_c("*debug-segment*");
  if (DebugSegment) {
    ds_symbol->value() = (s7 + FIX_SYM_TRUE).offset;
  } else {
    ds_symbol->value() = (s7 + FIX_SYM_FALSE).offset;
  }

  auto method_set_symbol = intern_from_c("*enable-method-set*");
  EnableMethodSet = method_set_symbol.cast<u32>() - 1;
  method_set_symbol->value() = 0;

  KernelDebug = intern_from_c("*kernel-debug*").cast<u32>() - 1;
  *KernelDebug = 0;

  intern_from_c("*boot-video-mode*")->value() = 0;
  intern_from_c("*video-mode*")->value() = 0;

  SqlResult = intern_from_c("*sql-result*");
  SqlResult->value() = s7.offset;

  CollapseQuote = intern_from_c("*collapse-quote*");
  CollapseQuote->value() = s7.offset + FIX_SYM_TRUE;
  LevelTypeList = intern_from_c("*level-type-list*");

  // load kernel!

  if (MasterUseKernel) {
    auto p = scoped_prof("load-kernel-dgo");
    *EnableMethodSet = *EnableMethodSet + 1;
    load_and_link_dgo_from_c("kernel", kglobalheap,
                             LINK_FLAG_OUTPUT_LOAD | LINK_FLAG_EXECUTE | LINK_FLAG_PRINT_LOGIN,
                             0x400000, true);
    *EnableMethodSet = *EnableMethodSet + -1;

    auto kernel_version = intern_from_c("*kernel-version*")->value();
    if (!kernel_version || ((kernel_version >> 0x13) != KERNEL_VERSION_MAJOR)) {
      lg::error(
          "Kernel version mismatch! Compiled C kernel version is {}.{} but"
          "the goal kernel is {}.{}",
          KERNEL_VERSION_MAJOR, KERNEL_VERSION_MINOR, kernel_version >> 0x13,
          (kernel_version >> 3) & 0xffff);
      return -1;
    } else {
      lg::info("Got correct kernel version {}.{}", kernel_version >> 0x13,
               (kernel_version >> 3) & 0xffff);
    }
  }

  // in real jak2, the -1 occurs in the GoalProtoHandler. But let's just do it here to make
  // things consistent.
  protoBlock.deci2count = intern_from_c("*deci-count*").cast<s32>() - 1;

  // load stuff for the listener interface
  InitListener();

  // Do final initialization, including loading and initializing the engine.
  {
    auto p = scoped_prof("init-machine-scheme");
    InitMachineScheme();
  }
  kmemclose();
  return 0;
}

u64 load(u32 /*file_name_in*/, u32 /*heap_in*/) {
  ASSERT(false);
  return 0;
}

u64 loadb(u32 /*file_name_in*/, u32 /*heap_in*/, u32 /*param3*/) {
  ASSERT(false);
  return 0;
}

u64 loadc(const char* /*file_name*/, kheapinfo* /*heap*/, u32 /*flags*/) {
  ASSERT(false);
  return 0;
}

u64 loado(u32 file_name_in, u32 heap_in) {
  char loadName[272];
  Ptr<String> file_name(file_name_in);
  Ptr<kheapinfo> heap(heap_in);
  printf("****** CALL TO loado(%s) ******\n", file_name->data());
  kstrcpy(loadName, MakeFileName(DATA_FILE_TYPE, file_name->data(), 0));
  s32 returnValue = load_and_link(file_name->data(), loadName, heap.c(), LINK_FLAG_PRINT_LOGIN);

  if (returnValue < 0) {
    return s7.offset;
  } else {
    return returnValue;
  }
}

/*!
 * "Unload". Doesn't free memory, just informs listener we unloaded.
 */
u64 unload(u32 name) {
  output_unload(Ptr<String>(name)->data());
  return 0;
}

s64 load_and_link(const char* filename, char* decode_name, kheapinfo* heap, u32 flags) {
  (void)filename;
  s32 sz;
  auto rv = FileLoad(decode_name, make_ptr(heap), Ptr<u8>(0), KMALLOC_ALIGN_64, &sz);
  if (((s32)rv.offset) > -1) {
    return (s32)link_and_exec(rv, decode_name, sz, make_ptr(heap), flags, false).offset;
  }
  return (s32)rv.offset;
}
}  // namespace jak2
