#include "kboot.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "common/goal_constants.h"
#include "common/repl/util.h"
#include "common/util/Timer.h"

#include "game/common/game_common_types.h"
#include "game/kernel/common/Ptr.h"
#include "game/kernel/common/Symbol4.h"
#include "game/kernel/common/klisten.h"
#include "game/kernel/common/kprint.h"
#include "game/kernel/common/kscheme.h"
#include "game/kernel/common/ksocket.h"
#include "game/kernel/jak2/klisten.h"
#include "game/kernel/jak2/kmachine.h"
#include "game/sce/libscf.h"

namespace jak2 {

char DebugBootUser[64];
char DebugBootArtGroup[64];

void kboot_init_globals() {
  memset(DebugBootUser, 0, sizeof(DebugBootUser));
  memset(DebugBootArtGroup, 0, sizeof(DebugBootArtGroup));
  // strcpy(DebugBootUser, "unknown");
  // CHANGED : let's just try to find the username automatically by default!
  // the default is still "unknown"
  auto username = REPL::find_repl_username();
  strcpy(DebugBootUser, username.c_str());
}

void KernelCheckAndDispatch();

s32 goal_main(int argc, const char* const* argv) {
  // only in PC port
  InitParms(argc, argv);

  init_crc();
  masterConfig.aspect = ee::sceScfGetAspect();
  auto sony_language = ee::sceScfGetLanguage();
  masterConfig.inactive_timeout = 0;
  masterConfig.volume = 100;
  masterConfig.timeout = 0;
  switch (sony_language) {
    case SCE_JAPANESE_LANGUAGE:
      masterConfig.language = (u16)Language::Japanese;
      break;
    case SCE_FRENCH_LANGUAGE:
      masterConfig.language = (u16)Language::French;
      break;
    case SCE_SPANISH_LANGUAGE:
      masterConfig.language = (u16)Language::Spanish;
      break;
    case SCE_GERMAN_LANGUAGE:
      masterConfig.language = (u16)Language::German;
      break;
    case SCE_ITALIAN_LANGUAGE:
      masterConfig.language = (u16)Language::Italian;
      break;
    default:
      masterConfig.language = (u16)Language::English;
      break;
  }
  // Set up aspect ratio override in demo
  if (!strcmp(DebugBootMessage, "demo") || !strcmp(DebugBootMessage, "demo-shared")) {
    masterConfig.aspect = SCE_ASPECT_FULL;
  }
  // removed in PC port
  //  DiskBoot = 1;
  //  MasterDebug = 0;
  //  DebugSegment = 0;

  // Launch GOAL!
  if (InitMachine() >= 0) {    // init kernel
    KernelCheckAndDispatch();  // run kernel
    ShutdownMachine();         // kernel died, we should too.
    // movie playback stuff removed.
  } else {
    fprintf(stderr, "InitMachine failed\n");
    exit(1);
  }
  return 0;
}

void KernelDispatch(u32 dispatcher_func) {
  // place our stack at the end of EE memory
  u64 goal_stack = u64(g_ee_main_mem) + EE_MAIN_MEM_SIZE - 8;

  // try to get a message from the listener, and process it if needed
  Ptr<char> new_message = WaitForMessageAndAck();
  if (new_message.offset) {
    ProcessListenerMessage(new_message);
  }

  // remember the old listener
  auto old_listener_function = ListenerFunction->value();

  // run the kernel!
  Timer dispatch_timer;
  if (MasterUseKernel) {
    call_goal_on_stack(Ptr<Function>(dispatcher_func), goal_stack, s7.offset, g_ee_main_mem);
  } else {
    // added, just calls the listener function
    if (ListenerFunction->value() != s7.offset) {
      auto result = call_goal_on_stack(Ptr<Function>(ListenerFunction->value()), goal_stack,
                                       s7.offset, g_ee_main_mem);
#ifdef __linux__
      cprintf("%ld\n", result);
#else
      cprintf("%lld\n", result);
#endif
      ListenerFunction->value() = s7.offset;
    }
  }

  float time_ms = dispatch_timer.getMs();
  if (time_ms > 50) {
    printf("Kernel dispatch time: %.3f ms\n", time_ms);
  }

  // flush stdout
  ClearPending();

  // now run the extra "kernel function"
  auto bonus_function = KernelFunction->value();
  if (bonus_function != s7.offset) {
    // clear the pending kernel function
    KernelFunction->value() = s7.offset;
    // and run
    call_goal_on_stack(Ptr<Function>(bonus_function), goal_stack, s7.offset, g_ee_main_mem);
  }

  // send ack to indicate that the listener function has been processed and the result printed
  if (MasterDebug && ListenerFunction->value() != old_listener_function) {
    SendAck();
  }

  // prevent crazy spinning if we're not vsyncing (added)
  if (time_ms < 4) {
    std::this_thread::sleep_for(std::chrono::microseconds(1000));
  }
}

void KernelShutdown(u32 reason) {
  MasterExit = (RuntimeExitStatus)reason;
}

void KernelCheckAndDispatch() {
  while (MasterExit == RuntimeExitStatus::RUNNING) {
    KernelDispatch(kernel_dispatcher->value());
  }
}

}  // namespace jak2
