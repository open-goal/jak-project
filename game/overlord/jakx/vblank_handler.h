#pragma once

#include "common/common_types.h"

#include "game/overlord/jakx/rpc_interface.h"

namespace jakx {
void jakx_overlord_init_globals_vblank_handler();
void VBlank_Initialize();
extern u32 g_nInfoEE;
extern SoundIOPInfo g_SRPCSoundIOPInfo;
}  // namespace jakx