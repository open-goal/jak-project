/*!
 * @file ksound.cpp
 * There's not much here. My guess is this was set up as framework to match the kmachine.cpp format,
 * but whoever did the sound didn't use this.
 */

#include "ksound.h"

#include "kdgo.h"
#include "kscheme.h"

#include "common/common_types.h"

#include "game/overlord/srpc.h"
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

/*!
 * Set up some functions which are somewhat related to sound.
 */
void InitSoundScheme() {
  make_function_symbol_from_c("rpc-call", (void*)RpcCall_wrapper);
  make_function_symbol_from_c("rpc-busy?", (void*)RpcBusy);
  make_function_symbol_from_c("test-load-dgo-c", (void*)LoadDGOTest);
  make_stack_arg_function_symbol_from_c("rpc-call", (void*)RpcCall_wrapper);

  // PC port interns
  make_function_symbol_from_c("pc-sound-set-flava-hack", (void*)set_flava_hack);
  make_function_symbol_from_c("pc-sound-set-fade-hack", (void*)set_fade_hack);
}
