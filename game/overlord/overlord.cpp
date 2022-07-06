#include "overlord.h"

#include <cstring>

#include "iso.h"
#include "ramdisk.h"
#include "sbank.h"
#include "srpc.h"
#include "ssound.h"

#include "common/util/Assert.h"

#include "game/sce/iop.h"

using namespace iop;

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
  // RegisterVblankHandler(0, 0x20, VBlank_Handler, nullptr);

  ThreadParam thread_param;
  thread_param.attr = TH_C;
  thread_param.initPriority = 98;
  thread_param.stackSize = 0x800;
  thread_param.option = 0;
  thread_param.entry = (void*)Thread_Server;
  strcpy(thread_param.name, "Server");  // added
  auto thread_server = CreateThread(&thread_param);
  if (thread_server <= 0) {
    return 1;
  }

  thread_param.attr = TH_C;
  thread_param.initPriority = 96;
  thread_param.stackSize = 0x800;
  thread_param.option = 0;
  thread_param.entry = (void*)Thread_Player;
  strcpy(thread_param.name, "Player");  // added
  auto thread_player = CreateThread(&thread_param);
  if (thread_player <= 0) {
    return 1;
  }

  thread_param.attr = TH_C;
  thread_param.initPriority = 99;
  thread_param.stackSize = 0x1000;
  thread_param.option = 0;
  thread_param.entry = (void*)Thread_Loader;
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

/*!
 * Loop endlessly and never return.
 */
void ExitIOP() {
  while (true) {
  }
}
