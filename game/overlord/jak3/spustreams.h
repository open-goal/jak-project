#pragma once

#include "common/common_types.h"

namespace jak3 {
struct ISO_Hdr;
void jak3_overlord_init_globals_spustreams();
u32 ProcessVAGData(ISO_Hdr* msg);
}