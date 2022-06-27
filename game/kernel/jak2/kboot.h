#pragma once

#include "common/common_types.h"

#include "game/kernel/common/kboot.h"

namespace jak2 {
extern char DebugBootUser[64];
extern char DebugBootArtGroup[64];
s32 goal_main(int argc, const char* const* argv);
void kboot_init_globals();
void KernelDispatch(u32 dispatcher_func);
void KernelShutdown(u32 reason);
}  // namespace jak2