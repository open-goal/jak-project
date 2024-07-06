#pragma once

#include "common/common_types.h"

#include "game/overlord/jak3/isocommon.h"

namespace jak3 {
struct ISO_Hdr;
struct ISO_VAGCommand;
void jak3_overlord_init_globals_spustreams();
EIsoStatus ProcessVAGData(ISO_Hdr* msg);
void StopVagStream(ISO_VAGCommand* cmd);
u32 GetSpuRamAddress(ISO_VAGCommand* cmd);
}  // namespace jak3