#pragma once

#include "common/common_types.h"

#include "game/kernel/common/Ptr.h"

extern s32 NumSymbols;
extern Ptr<u32> s7;
extern Ptr<u32> SymbolTable2;
extern Ptr<u32> LastSymbol;
extern u32 FastLink;
extern Ptr<u32> EnableMethodSet;

void kscheme_init_globals_common();

constexpr u32 CRC_POLY = 0x04c11db7;
constexpr u32 EMPTY_HASH = 0x8454B6E6;
constexpr u32 OFFSET_MASK = 7;

constexpr uint32_t UNKNOWN_PP = UINT32_MAX;

struct String {
  u32 len;
  char* data() { return ((char*)this) + sizeof(String); }
};

struct Function {};

void init_crc();
u32 crc32(const u8* data, s32 size);
u64 delete_illegal(u32 obj);
u64 goal_malloc(u32 heap, u32 size, u32 flags, u32 name);

u64 call_goal(Ptr<Function> f, u64 a, u64 b, u64 c, u64 st, void* offset);
u64 call_goal_on_stack(Ptr<Function> f, u64 rsp, u64 st, void* offset);
u64 call_goal_function(Ptr<Function> func);
u64 print_structure(u32 s);
u64 print_integer(u64 obj);
u64 print_binteger(u64 obj);
u64 print_float(u32 f);
u64 print_vu_function(u32 obj);
u64 copy_fixed(u32 it);
u64 copy_structure(u32 it, u32 unknown);
u64 inspect_integer(u64 obj);
u64 inspect_binteger(u64 obj);
u64 inspect_float(u32 f);
u64 inspect_structure(u32 obj);
u64 inspect_vu_function(u32 obj);
u64 inspect_kheap(u32 obj);