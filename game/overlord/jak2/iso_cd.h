#pragma once

#include "common/common_types.h"

#include "game/overlord/common/isocommon.h"
#include "game/overlord/jak2/iso.h"
#include "game/overlord/jak2/pages.h"

namespace jak2 {
void iso_cd_init_globals();

extern IsoFs iso_cd;
extern s32 StopPluginStreams;

}  // namespace jak2