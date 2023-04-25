#include "srpc.h"

#include <cstring>

// added
u32 gMusicFadeHack = 0;
MusicTweaks gMusicTweakInfo;
s32 gMusicTweak = 0x80;
int32_t gSoundEnable = 1;
s32 gMusic = 0;
s32 gMusicPause = 0;
s32 gSoundInUse = 0;
u8 gFPS = 60;
u32 gFrameNum = 0;
const char* gLanguage = nullptr;

void srpc_init_globals() {
  gMusicFadeHack = 0;
  gSoundEnable = 1;
  gMusic = 0;
  gMusicPause = 0;
  gSoundInUse = 0;
  memset((void*)&gMusicTweakInfo, 0, sizeof(gMusicTweakInfo));
}
