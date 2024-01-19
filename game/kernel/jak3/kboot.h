#pragma once

#include "common/common_types.h"

namespace jak3 {
extern char DebugBootUser[64];
extern char DebugBootArtGroup[64];
void kboot_init_globals();
void KernelShutdown(u32 reason);
s32 goal_main(int argc, const char* const* argv);
}  // namespace jak3