#include "srpc.h"

#include <cstring>

// added
u32 gMusicFadeHack = 0;
MusicTweaks gMusicTweakInfo;
s32 gMusicTweak = 0x80;

void srpc_init_globals() {
  memset((void*)&gMusicTweakInfo, 0, sizeof(gMusicTweakInfo));
}