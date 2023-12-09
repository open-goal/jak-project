#include "ksound.h"

#include "game/kernel/common/kdgo.h"
#include "game/kernel/jak3/kscheme.h"

namespace jak3 {
/*!
 * Set up some functions which are somewhat related to sound.
 */
void InitSoundScheme() {
  make_function_symbol_from_c("rpc-busy?", (void*)RpcBusy);
  make_function_symbol_from_c("test-load-dgo-c", (void*)LoadDGOTest);
  make_stack_arg_function_symbol_from_c("rpc-call", (void*)RpcCall_wrapper);
}
}  // namespace jak3