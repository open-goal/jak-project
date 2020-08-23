#include <cstring>
#include "srpc.h"

u8 gMusicTweakInfo[0x204];

void srpc_init_globals() {
  memset(gMusicTweakInfo, 0, sizeof(gMusicTweakInfo));
}