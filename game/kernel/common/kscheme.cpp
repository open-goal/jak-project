#include "kscheme.h"

#include "game/kernel/common/fileio.h"
#include "game/kernel/common/kmalloc.h"
#include "game/kernel/common/kprint.h"

// total number of symbols in the table
s32 NumSymbols;

// value of the GOAL s7 register, pointing to the middle of the symbol table
Ptr<u32> s7;

// used for crc32 calculation
u32 crc_table[0x100];

// pointer to the "second" symbol table
Ptr<u32> SymbolTable2;

// pointer to the last symbol
Ptr<u32> LastSymbol;

//! Controls link mode when EnableMethodSet = 0, MasterDebug = 1, DiskBoot = 0. Will enable a
//! warning message if EnableMethodSet = 1
u32 FastLink;

// set to true to enable propagating method overrides to child types
// this is an O(N_max_symbols) operation, so it is avoided when loading DGOs for levels.
// but is enabled when loading the engine.
Ptr<u32> EnableMethodSet;

void kscheme_init_globals_common() {
  SymbolTable2.offset = 0;
  LastSymbol.offset = 0;
  NumSymbols = 0;
  s7.offset = 0;
  for (auto& x : crc_table) {
    x = 0;
  }
  EnableMethodSet.offset = 0;
  FastLink = 0;
}

/*!
 * Initialize CRC Table.
 */
void init_crc() {
  for (u32 i = 0; i < 0x100; i++) {
    u32 n = i << 24;
    for (u32 j = 0; j < 8; j++) {
      n = n & 0x80000000 ? (n << 1) ^ CRC_POLY : (n << 1);
    }
    crc_table[i] = n;
  }
}

/*!
 * Take the CRC32 hash of some data
 */
u32 crc32(const u8* data, s32 size) {
  uint32_t crc = 0;
  for (int i = size; i != 0; i--, data++) {
    crc = crc_table[crc >> 24] ^ ((crc << 8) | *data);
  }

  ASSERT(~crc);
  return ~crc;
}

/*!
 * Delete method for types which cannot have "delete" used on them.
 * Prints an error to stdout and returns false.
 */
u64 delete_illegal(u32 obj) {
  MsgErr("dkernel: illegal attempt to call delete method of static object @ #x%x\n", obj);
  return s7.offset;  // todo, maybe don't return anything?
}

/*!
 * Wrapper around kmalloc to allow GOAL programs to allocate on kernel heaps.
 */
u64 goal_malloc(u32 heap, u32 size, u32 flags, u32 name) {
  return kmalloc(Ptr<kheapinfo>(heap), size, flags, Ptr<String>(name)->data()).offset;
}

extern "C" {
// defined in asm_funcs.asm
#ifdef __linux__
uint64_t _call_goal_asm_systemv(u64 a0, u64 a1, u64 a2, void* fptr, void* st_ptr, void* offset);
uint64_t _call_goal_on_stack_asm_systemv(u64 rsp,
                                         u64 u0,
                                         u64 u1,
                                         void* fptr,
                                         void* st_ptr,
                                         void* offset);
#elif defined __APPLE__ && defined __x86_64__
uint64_t _call_goal_asm_systemv(u64 a0, u64 a1, u64 a2, void* fptr, void* st_ptr, void* offset) asm(
    "_call_goal_asm_systemv");
uint64_t _call_goal_on_stack_asm_systemv(u64 rsp,
                                         u64 u0,
                                         u64 u1,
                                         void* fptr,
                                         void* st_ptr,
                                         void* offset) asm("_call_goal_on_stack_asm_systemv");
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
  return _call_goal_asm_systemv(a, b, c, fptr, st_ptr, offset);
#elif defined __APPLE__ && defined __x86_64__
  return _call_goal_asm_systemv(a, b, c, fptr, st_ptr, offset);
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
  return _call_goal_on_stack_asm_systemv(rsp, 0, 0, fptr, st_ptr, offset);
#elif defined __APPLE__ && defined __x86_64__
  return _call_goal_on_stack_asm_systemv(rsp, 0, 0, fptr, st_ptr, offset);
#elif _WIN32
  return _call_goal_on_stack_asm_win32(rsp, fptr, st_ptr, offset);
#endif
}

/*!
 * Call a GOAL function with no arguments.
 */
u64 call_goal_function(Ptr<Function> func) {
  return call_goal(func, 0, 0, 0, s7.offset, g_ee_main_mem);
}

/*!
 * Default print method for structures.
 * Structures have no runtime type info, so there's not much we can do here.
 */
u64 print_structure(u32 s) {
  cprintf("#<structure @ #x%x>", s);
  return s;
}

/*!
 * Print an integer. Works correctly for 64-bit integers.
 */
u64 print_integer(u64 obj) {
  // not sure why this is any better than cprintf("%ld") or similar. Maybe a tiny bit faster?
  char* str = PrintPending.cast<char>().c();
  if (!str) {
    str = (PrintBufArea + 0x18).cast<char>().c();
  }

  PrintPending = make_ptr(strend(str)).cast<u8>();
  kitoa((char*)PrintPending.c(), obj, 10, 0xffffffff, '0', 0);
  return obj;
}

/*!
 * Print a boxed integer. Works correctly for 64-bit integers. Assumes signed.
 */
u64 print_binteger(u64 obj) {
  char* str = PrintPending.cast<char>().c();
  if (!PrintPending.offset) {
    str = (PrintBufArea + 0x18).cast<char>().c();
  }

  PrintPending = make_ptr(strend(str)).cast<u8>();
  kitoa((char*)PrintPending.c(), ((s64)obj) >> 3, 10, 0xffffffff, '0', 0);
  return obj;
}

/*!
 * Print floating point number.
 */
u64 print_float(u32 f) {
  // again not sure why this is any better than cprintf("%f") or similar. Maybe a tiny bit faster?
  float ff;
  *(u32*)&ff = f;
  char* str = PrintPending.cast<char>().c();
  if (!PrintPending.offset) {
    str = (PrintBufArea + 0x18).cast<char>().c();
  }

  PrintPending = make_ptr(strend(str)).cast<u8>();

  ftoa((char*)PrintPending.c(), ff, 0xffffffff, ' ', 4, 0);
  return f;
}

/*!
 * Print method for VU functions.  Again, just prints address.
 */
u64 print_vu_function(u32 obj) {
  cprintf("#<compiled vu-function @ #x%x>", obj);
  return obj;
}

/*!
 * Copy method that does no copying.
 */
u64 copy_fixed(u32 it) {
  return it;
}

/*!
 * Default copy for a structure. Since this has no idea of the actual type, it doesn't know what
 * size to copy.  So we do no copy and return a reference to the original data.
 */
u64 copy_structure(u32 it, u32 unknown) {
  (void)unknown;
  return it;
}

/*!
 * Inspect an integer (works correctly on 64-bit integers)
 */
u64 inspect_integer(u64 obj) {
  // and now we're using cprintf. Why doesn't print do this?
  cprintf("[%16lx] fixnum %ld\n", obj, obj);
  return obj;
}

/*!
 * Inspect a boxed integer (works correctly on 64-integers)
 */
u64 inspect_binteger(u64 obj) {
  cprintf("[%16lx] boxed-fixnum %ld\n", obj, s64(obj) >> 3);
  return obj;
}

/*!
 * Inspect a floating point number
 */
u64 inspect_float(u32 f) {
  float ff;
  ff = *(float*)(&f);
  cprintf("[%8x] float ", f);

  // likely copy-pasta - no need for this check because of the cprintf immediately before.
  char* str = PrintPending.cast<char>().c();
  if (!str) {
    str = (PrintBufArea + 0x18).cast<char>().c();
  }

  PrintPending = make_ptr(strend(str)).cast<u8>();

  ftoa(PrintPending.cast<char>().c(), ff, -1, ' ', 4, 0);
  cprintf("\n");
  return f;
}

/*!
 * Inspect a structure.
 */
u64 inspect_structure(u32 obj) {
  cprintf("[%8x] structure\n", obj);
  return obj;
}

/*!
 * Inspect a VU Function. Doesn't seem to be used. Also the concept of "vu-function"
 * isn't really used. VU0 macro mode stuff goes in normal functions, and micro-mode stuff goes
 * in a giant dump of many functions thats loaded and unloaded all at the same time.
 */
u64 inspect_vu_function(u32 obj) {
  struct VuFunction {
    u32 length;
    u32 origin;
    u32 qlength;
  };

  auto vf = Ptr<VuFunction>(obj);
  cprintf("[%8x] vu-function\n\tlength: %d\n\torigin: #x%x\n\tqlength: %d\n", obj, vf->length,
          vf->origin, vf->qlength);
  return obj;
}

/*!
 * This doesn't exist in the game, but we add it as a wrapper around kheapstatus.
 * Note that this isn't a great inspect as it prints to stdout instead of the printbuffer.
 */
u64 inspect_kheap(u32 obj) {
  kheapstatus(Ptr<kheapinfo>(obj));
  return obj;
}
