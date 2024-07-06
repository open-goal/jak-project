#pragma once

#include "common/common_types.h"

#include "game/overlord/jak3/rpc_interface.h"

namespace jak3 {
void jak3_overlord_init_globals_vblank_handler();
void VBlank_Initialize();
extern u32 g_nInfoEE;
extern SoundIOPInfo g_SRPCSoundIOPInfo;
}  // namespace jak3