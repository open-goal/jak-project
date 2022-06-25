#pragma once

#include "common/common_types.h"

#include "game/kernel/common/Ptr.h"

extern s32 NumSymbols;
extern Ptr<u32> s7;

void kscheme_init_globals_common();

struct String {
  u32 len;
  char* data() { return ((char*)this) + sizeof(String); }
};

struct Function {};

u64 call_goal(Ptr<Function> f, u64 a, u64 b, u64 c, u64 st, void* offset);
u64 call_goal_on_stack(Ptr<Function> f, u64 rsp, u64 st, void* offset);