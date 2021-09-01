#pragma once

#include "common/common_types.h"
#include "common/util/assert.h"

/*!
 * @file sceGraphicInterface.h
 * This file contains implementations of the SCE graphics library functions that manage
 * synchronization and settings.
 */

u32 sceGsSyncPath(u32 mode, u32 timeout);
u32 sceGsSyncV(u32 mode);