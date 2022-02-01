#pragma once

/*!
 * @file 989snd.h
 * Stub implementation of the 989 sound library
 */

#include "common/common_types.h"

void snd_init_globals();
int snd_GetFreeSPUDMA();
void snd_FreeSPUDMA(int loc);
