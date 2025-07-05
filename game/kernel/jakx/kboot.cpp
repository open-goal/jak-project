#include "kboot.h"

#include <cstring>

#include "common/log/log.h"
#include "common/repl/repl_wrapper.h"
#include "common/util/Timer.h"

#include "game/common/game_common_types.h"
#include "game/kernel/common/Ptr.h"
#include "game/kernel/common/Symbol4.h"
#include "game/kernel/common/kboot.h"
#include "game/kernel/common/klisten.h"
#include "game/kernel/common/kprint.h"
#include "game/kernel/common/kscheme.h"
#include "game/kernel/common/ksocket.h"
#include "game/kernel/jakx/klisten.h"
#include "game/kernel/jakx/kmachine.h"
#include "game/sce/libscf.h"

// KernelDispatch__3ndiPFv_x
// KernelCheckAndDispatch__3ndiv
// KernelShutdown__3ndii
// main

namespace jakx {
int KernelCheckAndDispatch();

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

s32 goal_main(int argc, const char* const* argv) {
  // s32 thread_id = GetThreadId();
  // ChangeThreadPriority(thread_id, 0x18);
  // sceGsSyncVCallback(FUN_0026a918_probably_kernel);
  // DAT_002833fc = 1;
  // InitOnce_WS();

  // only in PC port
  InitParms(argc, argv);

  masterConfig.aspect = ee::sceScfGetAspect();
  auto sony_language = ee::sceScfGetLanguage();
  masterConfig.inactive_timeout = 0;
  masterConfig.volume = 100;
  masterConfig.timeout = 0;
  switch (sony_language) {
    case SCE_JAPANESE_LANGUAGE:
      masterConfig.language = 6;  // NOTE: Why UK_English and not Japanese?
      break;
    case SCE_FRENCH_LANGUAGE:  // 2 -> 1
      masterConfig.language = (u16)Language::French;
      break;
    case SCE_SPANISH_LANGUAGE:  // 3 -> 3
      masterConfig.language = (u16)Language::Spanish;
      break;
    case SCE_GERMAN_LANGUAGE:  // 4 -> 2
      masterConfig.language = (u16)Language::German;
      break;
    case SCE_ITALIAN_LANGUAGE:  // 5 -> 4
      masterConfig.language = (u16)Language::Italian;
      break;
    case SCE_PORTUGUESE_LANGUAGE:
      masterConfig.language = (u16)Language::Portuguese;
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
  //  DebugSymbols = 0;

  // Launch GOAL!
  int initMachineResult = InitMachine();
  bool result = (initMachineResult & 0xfff00000) == 0xfff00000;
  int shutdownReason;
  if (result) {
    printf("kboot: error; failed to initialize machine (result=0x%08x)\n", initMachineResult);
    shutdownReason = 2;
  }
  else {
    shutdownReason = KernelCheckAndDispatch();  // run kernel
  }
  ShutdownMachine(shutdownReason);         // kernel died, we should too.
  // movie playback stuff?
  if (MasterExit == RuntimeExitStatus::EXIT) {
    // LoadExecPS2("cdrom0:\\NETGUI\\NTGUI_EU.ELF;1", 1, &movie_args_Q);
  }
  else if (MasterExit == RuntimeExitStatus::RESTART_IN_DEBUG) {
    char printBuffer[32];
    sprintf(printBuffer, "cdrom0:\\SCES_%.3s.%2s;1", "532", "86");
  }
  return result;
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
    lg::print("Kernel dispatch time: {:.3f} ms\n", time_ms);
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
  if ((u32)MasterExit < reason) {
    MasterExit = (RuntimeExitStatus)reason;
  }
}

int KernelCheckAndDispatch() {
  while (MasterExit == RuntimeExitStatus::RUNNING && !POWERING_OFF_W) {
    KernelDispatch(kernel_dispatcher->value());
  }
  if (POWERING_OFF_W) {
    KernelShutdown(3);
  }
  return (u32)MasterExit;
}

}  // namespace jakx
