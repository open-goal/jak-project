#pragma once

namespace jak3 {
struct ISO_Hdr;
void jak3_overlord_init_globals_spustreams();
void ProcessVAGData(ISO_Hdr* msg);
}