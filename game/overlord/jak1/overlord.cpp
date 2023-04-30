#include "overlord.h"

#include <cstring>

#include "ramdisk.h"
#include "srpc.h"
#include "ssound.h"

#include "common/util/Assert.h"

#include "game/overlord/jak1/iso.h"
#include "game/sce/iop.h"

using namespace iop;

namespace jak1 {
static s32 gargc;
static const char* const* gargv;
static bool* init_complete;

int start_overlord(int argc, const char* const* argv) {
  (void)argc;
  FlushDcache();
  CpuEnableIntr();
  if (!sceSifCheckInit()) {
    sceSifInit();
  }

  bool disable_sound = false;
  for (int i = 1; i < argc; i++) {
    if (std::string("-nosound") == argv[i]) {
      disable_sound = true;
    }
  }

  sceSifInitRpc(0);
  InitBanks();
  if (!disable_sound) {
    InitSound_Overlord();
  }
  InitRamdisk();
  RegisterVblankHandler(0, 0x20, VBlank_Handler, nullptr);

  ThreadParam thread_param;
  thread_param.attr = TH_C;
  thread_param.initPriority = 98;
  thread_param.stackSize = 0x800;
  thread_param.option = 0;
  thread_param.entry = Thread_Server;
  strcpy(thread_param.name, "Server");  // added
  auto thread_server = CreateThread(&thread_param);
  if (thread_server <= 0) {
    return 1;
  }

  thread_param.attr = TH_C;
  thread_param.initPriority = 96;
  thread_param.stackSize = 0x800;
  thread_param.option = 0;
  thread_param.entry = Thread_Player;
  strcpy(thread_param.name, "Player");  // added
  auto thread_player = CreateThread(&thread_param);
  if (thread_player <= 0) {
    return 1;
  }

  thread_param.attr = TH_C;
  thread_param.initPriority = 99;
  thread_param.stackSize = 0x1000;
  thread_param.option = 0;
  thread_param.entry = Thread_Loader;
  strcpy(thread_param.name, "Loader");  // added for debug
  auto thread_loader = CreateThread(&thread_param);
  if (thread_loader <= 0) {
    return 1;
  }

  InitISOFS(argv[1], argv[2]);
  StartThread(thread_server, 0);

  StartThread(thread_player, 0);
  StartThread(thread_loader, 0);
  return 0;
}

static u32 call_start() {
  start_overlord(gargc, gargv);
  *init_complete = true;

  while (true) {
    SleepThread();
  }
  return 0;
}

int start_overlord_wrapper(int argc, const char* const* argv, bool* signal) {
  ThreadParam param = {};

  gargc = argc;
  gargv = argv;
  init_complete = signal;

  param.attr = TH_C;
  param.initPriority = 0;
  param.stackSize = 0x800;
  param.option = 0;
  strcpy(param.name, "start");  // added for debug
  param.entry = call_start;

  auto start_thread = CreateThread(&param);
  StartThread(start_thread, 0);

  return 0;
}

}  // namespace jak1
