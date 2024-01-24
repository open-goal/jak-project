#include <cstdio>
#include <cstring>

#include "overlord.h"
#include "sbank.h"

#include "game/sce/iop.h"

namespace jak3 {
using namespace iop;

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
