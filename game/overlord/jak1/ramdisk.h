#pragma once

/*!
 * @file ramdisk.cpp
 * A RAMDISK RPC for storing files in the extra RAM left over on the IOP.
 * Also called "Server".
 */

#include "common/common_types.h"

namespace jak1 {
extern u32 gMemFreeAtStart;

void ramdisk_init_globals();
void InitRamdisk();
u32 Thread_Server();
}  // namespace jak1