#ifndef JAK_V2_SRPC_H
#define JAK_V2_SRPC_H

#include "common/common_types.h"

void srpc_init_globals();

constexpr int MUSIC_TWEAK_SIZE = 0x204;
extern u8 gMusicTweakInfo[MUSIC_TWEAK_SIZE];

#endif //JAK_V2_SRPC_H
