#include "overlord.h"

#include "common/log/log.h"

#include "game/overlord/jak3/dvd_driver.h"
#include "game/overlord/jak3/init.h"
#include "game/overlord/jak3/iso.h"
#include "game/overlord/jak3/ramdisk.h"
#include "game/overlord/jak3/sbank.h"
#include "game/overlord/jak3/srpc.h"
#include "game/overlord/jak3/ssound.h"
#include "game/overlord/jak3/vblank_handler.h"
#include "game/sce/iop.h"

namespace jak3 {
using namespace iop;

int g_nServerThreadID = 0;
int g_nPlayerThreadID = 0;
int g_nLoaderThreadID = 0;

void jak3_overlord_init_globals_overlord() {
  g_nServerThreadID = 0;
  g_nPlayerThreadID = 0;
  g_nLoaderThreadID = 0;
}

namespace {

/*!
 * Start up overlord threads. After this returns, the overlord is initialized and
 * ready to run. (this might have been in start.cpp in the original, but this is close enough)
 */
int start_overlord() {
  // Initialize SIF to communicate with the EE. Does nothing in port
  if (!sceSifCheckInit()) {
    sceSifInit();
  }
  // Initialize RPC to EE
  sceSifInitRpc(0);

  lg::info("======== overlrd2.irx startup ========");
  // Removed some prints related to IOP memory size

  // C++ ctors ran here. In the port, we've added these "init_globals" function that
  // take care of resetting all the global/static variables and constructing C++ objects.
  // do_global_ctors();
  jak3_overlord_init_globals_all();
  InitBanks();
  InitSound();
  VBlank_Initialize();

  // RPC thread to load data from game files to the game memory.
  ThreadParam thread_param;
  thread_param.initPriority = 0x3b;
  thread_param.stackSize = 0x800;
  thread_param.entry = LoadToEE_RPC_Thread;
  thread_param.attr = TH_C;
  thread_param.option = 0;
  strcpy(thread_param.name, "load_to_ee");
  g_nServerThreadID = CreateThread(&thread_param);
  if (g_nServerThreadID <= 0) {
    return 1;
  }

  // thread to respond to sound play RPC's
  thread_param.entry = Thread_Player;
  thread_param.initPriority = 0x36;
  thread_param.stackSize = 0xb00;
  thread_param.attr = TH_C;
  thread_param.option = 0;
  strcpy(thread_param.name, "player");
  g_nPlayerThreadID = CreateThread(&thread_param);
  if (g_nPlayerThreadID <= 0) {
    return 1;
  }

  // thread to respond to sound load RPC's
  thread_param.attr = TH_C;
  thread_param.entry = Thread_Loader;
  thread_param.initPriority = 0x3a;
  thread_param.stackSize = 0x900;
  thread_param.option = 0;
  strcpy(thread_param.name, "loader");
  g_nLoaderThreadID = CreateThread(&thread_param);
  if (g_nLoaderThreadID <= 0) {
    return 1;
  }

  // Initialize the dvd driver that will be used to implement the "ISO FS" system
  get_driver()->Initialize();

  // then, initialize ISO FS itself
  InitISOFS();

  // start up RPC threads!
  StartThread(g_nServerThreadID, 0);
  StartThread(g_nPlayerThreadID, 0);
  StartThread(g_nLoaderThreadID, 0);

  lg::info("[overlord2] Threads started");
  return 0;
}

bool* init_complete;

/*!
 * Wrapper of start_overlord that can be run as an IOP thread. Sets init_complete flag to true.
 * This is a bit of hack to transition from the normal and sane game code to the world of IOP
 * threading.
 */
u32 call_start() {
  start_overlord();
  *init_complete = true;

  // TODO: figure out how to quit this thread.
  while (true) {
    SleepThread();
  }
  return 0;
}
}  // namespace

int start_overlord_wrapper(bool* signal) {
  ThreadParam param = {};
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

/*!
 * Copy null-terminated string to destination buffer with the given size.
 * If the string doesn't fit, it will be truncated and null terminated.
 */
char* strncpyz(char* dst, const char* src, size_t n) {
  if (n) {
    strncpy(dst, src, n);
    dst[n - 1] = 0;
  }
  return dst;
}

}  // namespace jak3