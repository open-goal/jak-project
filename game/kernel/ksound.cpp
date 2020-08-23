/*!
 * @file ksound.cpp
 * There's not much here. My guess is this was set up as framework to match the kmachine.cpp format,
 * but whoever did the sound didn't use this.
 */

#include "ksound.h"
#include "kscheme.h"
#include "kdgo.h"

/*!
 * Does nothing!
 */
void InitSound() {}

/*!
 * Does nothing!
 */
void ShutdownSound() {}

/*!
 * Set up some functions which are somewhat related to sound.
 */
void InitSoundScheme() {
  make_function_symbol_from_c("rpc-call", (void*)RpcCall_wrapper);
  make_function_symbol_from_c("rpc-busy?", (void*)RpcBusy);
  make_function_symbol_from_c("test-load-dgo-c", (void*)LoadDGOTest);
}