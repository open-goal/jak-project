#pragma once

#include "game/overlord/jak3/isocommon.h"
namespace jak3 {
void jak3_overlord_init_globals_vag();

struct ISO_VAGCommand : ISO_Hdr {
  int xfer_size;
};
}