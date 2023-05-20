#include "ksound.h"

#include "common/common_types.h"

#include "game/overlord/common/srpc.h"
#include "game/sound/989snd/ame_handler.h"

/*!
 * Does nothing!
 */
void InitSound() {}

/*!
 * Does nothing!
 */
void ShutdownSound() {}

/*!
 * PC port functions
 */
void set_flava_hack(u64 val) {
  snd::SoundFlavaHack = val;
}

void set_fade_hack(u64 val) {
  gMusicFadeHack = val;
}
