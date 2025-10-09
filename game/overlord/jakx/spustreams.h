#pragma once

#include "common/common_types.h"

#include "game/overlord/jakx/isocommon.h"

namespace jakx {
struct ISO_Hdr;
struct ISO_VAGCommand;
void jakx_overlord_init_globals_spustreams();
EIsoStatus ProcessVAGData(ISO_Hdr* msg);
void StopVagStream(ISO_VAGCommand* cmd);
u32 GetSpuRamAddress(ISO_VAGCommand* cmd);
}  // namespace jakx