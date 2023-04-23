#pragma once

#include "game/overlord/common/sbank.h"
#include "game/overlord/common/ssound.h"
#include "game/sce/iop.h"

namespace jak1 {

extern VolumePair gPanTable[361];
extern u32 gStreamSRAM;
extern u32 gTrapSRAM;

extern s32 gMusicVol;

void InitSound_Overlord();
void SetEarTrans(Vec3w* ear_trans1, Vec3w* ear_trans2, Vec3w* cam_trans, s32 cam_angle);
void SetMusicVol();
}  // namespace jak1