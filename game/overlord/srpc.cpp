#include <cstring>
#include "srpc.h"

MusicTweaks gMusicTweakInfo;

void srpc_init_globals() {
  memset((void*)&gMusicTweakInfo, 0, sizeof(gMusicTweakInfo));
}
