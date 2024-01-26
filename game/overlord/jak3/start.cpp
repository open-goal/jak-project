#include <cstdio>
#include <cstring>

#include "overlord.h"
#include "ramdisk.h"
#include "sbank.h"

#include "game/overlord/jak3/srpc.h"
#include "game/sce/iop.h"

namespace jak3 {
using namespace iop;

int g_nServerThreadID;
int g_nPlayerThreadID;
int g_nLoaderThreadID;

int start_overlord(int argc, const char* const* argp) {
  ThreadParam thp;

  if (argc < 0) {
    Panic();
  }

  if (sceSifCheckInit() == 0) {
    sceSifInit();
  }
  sceSifInitRpc(0);
  printf("======== overlrd2.irx startup ========\n");
  // printf("      mem size: %lu\n", QueryMemSize());
  // printf("total mem free: %lu\n", QueryTotalFreeMemSize());
  // printf("  max mem free: %lu\n", QueryMaxFreeMemSize());
  // printf("          used: %lu\n", QueryMemSize() - QueryTotalFreeMemSize());
  // printf("  start() addr: 0x%08x\n", start_overlord);
  //__do_global_ctors();
  InitBanks();
  InitSound();
  VBlank_Initialize();

  thp.attr = TH_C;
  thp.option = 0;
  thp.entry = Thread_Server;
  thp.stackSize = 0x800;
  thp.initPriority = 59;
  g_nServerThreadID = CreateThread(&thp);
  if (g_nServerThreadID < 0) {
    Panic();
    return 1;
  }

  thp.attr = TH_C;
  thp.option = 0;
  thp.entry = Thread_Player;
  thp.stackSize = 0xb00;
  thp.initPriority = 54;
  g_nPlayerThreadID = CreateThread(&thp);
  if (g_nPlayerThreadID < 0) {
    Panic();
    return 1;
  }

  thp.attr = TH_C;
  thp.option = 0;
  thp.entry = Thread_Loader;
  thp.stackSize = 0x900;
  thp.initPriority = 58;
  g_nLoaderThreadID = CreateThread(&thp);
  if (g_nPlayerThreadID < 0) {
    Panic();
    return 1;
  }

  //CDvdDriver::Initialize(&g_DvdDriver);
  InitISOFS(argp[1], argp[2]);

  StartThread(g_nServerThreadID, 0);
  StartThread(g_nPlayerThreadID, 0);
  StartThread(g_nLoaderThreadID, 0);

  printf("======== overlrd2.irx post-startup ========\n");
  // printf("      mem size: %lu\n", QueryMemSize());
  // printf("total mem free: %lu\n", QueryTotalFreeMemSize());
  // printf("  max mem free: %lu\n", QueryMaxFreeMemSize());
  // printf("          used: %lu\n", QueryMemSize() - QueryTotalFreeMemSize());

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

}  // namespace jak3
