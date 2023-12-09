#pragma once

namespace jak3 {
extern char DebugBootUser[64];
extern char DebugBootArtGroup[64];
void kboot_init_globals();
void KernelShutdown();
}  // namespace jak3