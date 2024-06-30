#pragma once

#include "common/common_types.h"
#include "game/overlord/jak3/isocommon.h"

namespace jak3 {
struct ISO_Hdr;
void jak3_overlord_init_globals_spustreams();
EIsoStatus ProcessVAGData(ISO_Hdr* msg);
}