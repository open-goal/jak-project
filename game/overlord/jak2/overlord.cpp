#include "overlord.h"

#include <cstdio>
#include <cstring>

#include "game/overlord/common/sbank.h"
#include "game/overlord/jak1/ramdisk.h"
#include "game/overlord/jak2/iso.h"
#include "game/overlord/jak2/iso_queue.h"
#include "game/overlord/jak2/srpc.h"
#include "game/overlord/jak2/ssound.h"
#include "game/sce/iop.h"

namespace jak2 {
using namespace iop;

u8* ScratchPadMemoryBase;

namespace {
// believed unused global
s32 SndPlayThread;
}  // namespace

/*!
 * Entry point for the overlord. This runs InitISOFS, registers the vblank handler, and starts up
 * the threads. It returns and assumes that the IOP kernel runs the threads.
 */
int start_overlord(int, const char* const*) {
  sceSifInit();
  sceSifInitRpc(0);
  printf("IOP: =========Startup===(%x)====\n", 0);
  // removed memory prints.

  ScratchPadMemory = (u8*)AllocScratchPad(0);
  ScratchPadMemoryBase = ScratchPadMemory;
  // removed allocation check code.

  InitBanks();
  InitSound_overlord();
  jak1::InitRamdisk();  // ramdisk believed unused.
  RegisterVblankHandler(0, 0x20, VBlank_Handler, 0);

  // ramdisk believed unused.
  ThreadParam param;
  param.entry = jak1::Thread_Server;
  param.attr = TH_C;
  param.initPriority = 0x7a;
  param.stackSize = 0x800;
  param.option = 0;
  strcpy(param.name, "Server");  // added
  auto thread_server = CreateThread(&param);
  if (thread_server <= 0) {
    return 1;
  }

  param.entry = Thread_Player;
  param.attr = TH_C;
  param.initPriority = 100;
  param.stackSize = 0x800;
  param.option = 0;
  strcpy(param.name, "Player");  // added
  auto thread_player = CreateThread(&param);
  if (thread_player <= 0) {
    return 1;
  }

  param.entry = Thread_Loader;
  param.attr = 0x73;
  param.initPriority = TH_C;
  param.stackSize = 0x1000;
  param.option = 0;
  SndPlayThread = thread_player;
  auto thread_loader = CreateThread(&param);
  if (thread_loader <= 0) {
    return 1;
  }

  InitISOFS(/*argv[1], argv[2]*/);
  StartThread(thread_server, 0);
  StartThread(thread_player, 0);
  StartThread(thread_loader, 0);
  printf("IOP: =========After inits=============\n");
  // removed memory printing code

  return 0;
}

static s32 gargc;
static const char* const* gargv;
static bool* init_complete;

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

}  // namespace jak2