#include <cstring>
#include "overlord.h"
#include "game/sce/iop.h"
#include "ramdisk.h"
#include "iso.h"
#include "ssound.h"
#include "sbank.h"

using namespace iop;

int start_overlord(int argc, const char* const* argv) {
  (void)argc;
  FlushDcache();
  CpuEnableIntr();
  if (!sceSifCheckInit()) {
    sceSifInit();
  }

  sceSifInitRpc(0);
  InitBanks();
  InitSound_Overlord();
  InitRamdisk();
  //  RegisterVblankHandler(0, 0x20, VBlank_Handler, nullptr);

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

  //  thread_param.attr = TH_C;
  //  thread_param.initPriority = 96;
  //  thread_param.stackSize = 0x800;
  //  thread_param.option = 0;
  //  thread_param.entry = Thread_Player;
  //  auto thread_player = CreateThread(&thread_param);
  //  if(thread_player <= 0) {
  //    return 1;
  //  }
  //
  //  thread_param.attr = TH_C;
  //  thread_param.initPriority = 99;
  //  thread_param.stackSize = 0x1000;
  //  thread_param.option = 0;
  //  thread_param.entry = Thread_Loader;
  //  auto thread_loader = CreateThread(&thread_param);
  //  if(thread_loader <= 0) {
  //    return 1;
  //  }

  InitISOFS(argv[1], argv[2]);
  StartThread(thread_server, 0);

  //  StartThread(thread_player, 0);
  //  StartThread(thread_loader, 0);
  return 0;
}

/*!
 * Loop endlessly and never return.
 */
void ExitIOP() {
  while (true) {
  }
}