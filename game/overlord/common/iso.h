#pragma once

#include "common/common_types.h"
#include "common/versions/versions.h"

#include "game/overlord/common/isocommon.h"

void iso_init_globals();
u32 LookMbx(s32 mbx);
void WaitMbx(s32 mbx);
extern u8 VAG_SilentLoop[0x60];
