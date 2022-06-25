#include "kscheme.h"

// total number of symbols in the table
s32 NumSymbols;

// value of the GOAL s7 register, pointing to the middle of the symbol table
Ptr<u32> s7;

void kscheme_init_globals_common() {
  NumSymbols = 0;
  s7.offset = 0;
}

extern "C" {
// defined in asm_funcs.asm
#ifdef __linux__
uint64_t _call_goal_asm_linux(u64 a0, u64 a1, u64 a2, void* fptr, void* st_ptr, void* offset);
uint64_t _call_goal_on_stack_asm_linux(u64 rsp,
                                       u64 u0,
                                       u64 u1,
                                       void* fptr,
                                       void* st_ptr,
                                       void* offset);
#elif _WIN32
uint64_t _call_goal_asm_win32(u64 a0, u64 a1, u64 a2, void* fptr, void* st_ptr, void* offset);
uint64_t _call_goal_on_stack_asm_win32(u64 rsp, void* fptr, void* st_ptr, void* offset);
#endif
}

/*!
 * Wrapper around _call_goal_asm for calling a GOAL function from C.
 * Calls from the parent stack.
 */
u64 call_goal(Ptr<Function> f, u64 a, u64 b, u64 c, u64 st, void* offset) {
  // auto st_ptr = (void*)((uint8_t*)(offset) + st); updated for the new compiler!
  void* st_ptr = (void*)st;

  void* fptr = f.c();
#ifdef __linux__
  return _call_goal_asm_linux(a, b, c, fptr, st_ptr, offset);
#elif _WIN32
  return _call_goal_asm_win32(a, b, c, fptr, st_ptr, offset);
#endif
}

/*!
 * Wrapper around _call_goal_asm_on_stack for switching stacks and calling a GOAL function there.
 */
u64 call_goal_on_stack(Ptr<Function> f, u64 rsp, u64 st, void* offset) {
  void* st_ptr = (void*)st;

  void* fptr = f.c();
#ifdef __linux__
  return _call_goal_on_stack_asm_linux(rsp, 0, 0, fptr, st_ptr, offset);
#elif _WIN32
  return _call_goal_on_stack_asm_win32(rsp, fptr, st_ptr, offset);
#endif
}