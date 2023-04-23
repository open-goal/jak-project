#include "ssound.h"

s32 gMusicFade = 0;
s32 gSema;

void ssound_init_globals() {
  gMusicFade = 0;
  gSema = 0;
}
