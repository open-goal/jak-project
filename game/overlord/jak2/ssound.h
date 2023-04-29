#pragma once
#include "game/overlord/common/ssound.h"

namespace jak2 {
void ssound_init_globals();
void SetBufferMem(void*, int);
void ReleaseBufferMem();
void SetMusicVol();
void SetEarTrans(Vec3w* ear_trans0, Vec3w* ear_trans1, Vec3w* cam_trans, s32 cam_angle);
void InitSound_overlord();
extern s32 StreamThread;
extern VolumePair gPanTable[361];
}  // namespace jak2