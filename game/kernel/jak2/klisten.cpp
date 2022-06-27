#include "klisten.h"

#include <cstdio>
#include <cstdlib>

#include "common/symbols.h"

#include "game/kernel/common/kdsnetm.h"
#include "game/kernel/common/klink.h"
#include "game/kernel/common/klisten.h"
#include "game/kernel/common/kprint.h"
#include "game/kernel/jak2/kboot.h"
#include "game/kernel/jak2/klink.h"
#include "game/kernel/jak2/kprint.h"
#include "game/kernel/jak2/kscheme.h"

namespace jak2 {
using namespace jak2_symbols;

Ptr<Symbol4<u32>> ListenerLinkBlock;
Ptr<Symbol4<u32>> ListenerFunction;
Ptr<Symbol4<u32>> KernelFunction;  // new in jak2
Ptr<Symbol4<u32>> kernel_dispatcher;
Ptr<Symbol4<u32>> kernel_packages;
Ptr<Symbol4<u32>> sync_dispatcher;

void klisten_init_globals() {
  ListenerLinkBlock.offset = 0;
  ListenerFunction.offset = 0;
  KernelFunction.offset = 0;
  kernel_dispatcher.offset = 0;
  kernel_packages.offset = 0;
  sync_dispatcher.offset = 0;
}

/*!
 * Initialize the Listener by setting up symbols shared between GOAL and C for the listener.
 * Also adds "kernel" to the kernel_packages list.
 * There was an "ACK" message sent here, but this is removed because we don't need it.
 */
void InitListener() {
  ListenerLinkBlock = intern_from_c("*listener-link-block*");
  ListenerFunction = intern_from_c("*listener-function*");
  KernelFunction = intern_from_c("*kernel-function*");
  kernel_dispatcher = intern_from_c("kernel-dispatcher");
  sync_dispatcher = intern_from_c("sync-dispatcher");
  kernel_packages = intern_from_c("*kernel-packages*");
  print_column = intern_from_c("*print-column*").cast<u32>();
  ListenerLinkBlock->value() = s7.offset;
  ListenerFunction->value() = s7.offset;
  KernelFunction->value() = s7.offset;

  kernel_packages->value() =
      new_pair(s7.offset + FIX_SYM_GLOBAL_HEAP, *((s7 + FIX_SYM_PAIR_TYPE - 1).cast<u32>()),
               make_string_from_c("kernel"), kernel_packages->value());
  //  if(MasterDebug) {
  //    SendFromBufferD(MSG_ACK, 0, AckBufArea + sizeof(ListenerMessageHeader), 0);
  //  }
}

/*!
 * Handle an incoming listener message
 */
void ProcessListenerMessage(Ptr<char> msg) {
  // flag that the listener is connected!
  ListenerStatus = 1;
  switch (protoBlock.msg_kind) {
    case LTT_MSG_POKE:
      // just flush any pending stuff.
      ClearPending();
      break;
    case LTT_MSG_INSEPCT:
      inspect_object(atoi(msg.c()));
      ClearPending();
      break;
    case LTT_MSG_PRINT:
      print_object(atoi(msg.c()));
      ClearPending();
      break;
    case LTT_MSG_PRINT_SYMBOLS:
      printf("[ERROR] unsupported message kind LTT_MSG_PRINT_SYMBOLS (NYI)\n");
      break;
    case LTT_MSG_RESET:
      MasterExit = RuntimeExitStatus::RESTART_RUNTIME;
      break;
    case LTT_MSG_SHUTDOWN:
      MasterExit = RuntimeExitStatus::EXIT;
      break;
    case LTT_MSG_CODE: {
      auto buffer = kmalloc(kdebugheap, MessCount, 0, "listener-link-block");
      memcpy(buffer.c(), msg.c(), MessCount);
      ListenerLinkBlock->value() = buffer.offset + 4;
      // note - this will stash the linked code in the top level and free it.
      // it will then be used-after-free, but this is OK because nobody else will allocate.
      // the kernel dispatcher should immediately execute the listener function to avoid this
      // getting squashed.

      // this setup allows listener function execution to clean up after itself.

      // we have added the LINK_FLAG_OUTPUT_LOAD
      // jump from c to goal because this is called from the C++ stack.
      ListenerFunction->value() = link_and_exec(buffer, "*listener*", 0, kdebugheap,
                                                LINK_FLAG_FORCE_DEBUG | LINK_FLAG_OUTPUT_LOAD, true)
                                      .offset;
      return;  // don't ack yet, this will happen after the function runs.
    } break;
    default:
      MsgErr("dkernel: unknown message error: <%d> of %d bytes\n", protoBlock.msg_kind, MessCount);
      break;
  }
  SendAck();
}

int sql_query_sync(Ptr<String> string_in) {
  if (!MasterDebug) {
    // not debugging, no sql results.
    return s7.offset + FIX_SYM_EMPTY_PAIR - 1;
  } else {
    // clear old result
    SqlResult->value() = s7.offset;
    // output sql query to the compiler
    output_sql_query(string_in->data());
    // clear pending listener stuff, so we don't run it again.
    ListenerFunction->value() = s7.offset;
    ListenerStatus = 1;
    ClearPending();
    SendAck();

    kdebugheap->top.offset -= 0x4000;  // not sure what it's used for...

    // didn't we just set these to false?
    if (ListenerFunction->value() == s7.offset && SqlResult->value() == s7.offset) {
      do {
        KernelDispatch(sync_dispatcher->value());
        SendAck();
        if (SqlResult->value() != s7.offset) {
          break;
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(10));  // ?
        /*
        iVar2 = 99999;
        do {
          bVar1 = iVar2 != 0;
          iVar2 = iVar2 + -1;
        } while (bVar1);
         */
      } while (SqlResult->value() == s7.offset);
    }
    kdebugheap->top.offset += 0x4000;
    return SqlResult->value();
  }
}

}  // namespace jak2
