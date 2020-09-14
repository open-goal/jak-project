#pragma once

/*!
 * @file ramdisk.cpp
 * A RAMDISK RPC for storing files in the extra RAM left over on the IOP.
 * Also called "Server".
 */

#ifndef JAK_RAMDISK_H
#define JAK_RAMDISK_H

#include "common/common_types.h"

void ramdisk_init_globals();
void InitRamdisk();
u32 Thread_Server();

#endif  // JAK_RAMDISK_H
