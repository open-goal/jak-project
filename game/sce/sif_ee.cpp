#include <cstring>
#include <cassert>
#include "sif_ee.h"
#include "game/system/iop_thread.h"
#include "game/runtime.h"

namespace ee {

namespace {
::IOP* iop;
}

void LIBRARY_sceSif_register(::IOP* i) {
  iop = i;
}

void LIBRARY_INIT_sceSif() {
  iop = nullptr;
}
void sceSifInitRpc(unsigned int mode) {
  (void)mode;
}

int sceSifRebootIop(const char* imgfile) {
  (void)imgfile;
  return 1;
}

int sceSifSyncIop() {
  return 1;
}

void sceFsReset() {}

int sceSifLoadModule(const char* name, int arg_size, const char* args) {
  if (!strcmp(name, "cdrom0:\\\\DRIVERS\\\\OVERLORD.IRX;1") ||
      !strcmp(name, "host0:binee/overlord.irx")) {
    const char* src = args;
    char* dst = iop->overlord_arg_data;
    int cnt;
    iop->overlord_argv[0] = nullptr;
    for (cnt = 1; src - args < arg_size; cnt++) {
      auto len = strlen(src);
      memcpy(dst, src, len + 1);
      iop->overlord_argv[cnt] = dst;
      dst += len + 1;
      src += len + 1;
    }
    iop->overlord_argc = cnt;

    for (int i = 0; i < cnt; i++) {
      if (iop->overlord_argv[i])
        printf("arg %d : %s\n", i, iop->overlord_argv[i]);
    }
    iop->set_ee_main_mem(g_ee_main_mem);
    iop->send_status(IOP_Status::IOP_OVERLORD_INIT);
    iop->wait_for_overlord_init_finish();
  }

  return 1;
}

int sceMcInit() {
  return 1;
}

s32 sceSifCallRpc(sceSifClientData* bd,
                  u32 fno,
                  u32 mode,
                  void* send,
                  s32 ssize,
                  void* recv,
                  s32 rsize,
                  void* end_func,
                  void* end_para) {
  assert(!end_func);
  assert(!end_para);
  assert(mode == 1);  // async
  iop->kernel.sif_rpc(bd->rpcd.id, fno, mode, send, ssize, recv, rsize);
  return 0;
}

s32 sceSifCheckStatRpc(sceSifRpcData* bd) {
  iop->signal_run_iop();
  return iop->kernel.sif_busy(bd->id);
}

s32 sceSifBindRpc(sceSifClientData* bd, u32 request, u32 mode) {
  assert(mode == 1);  // async
  bd->rpcd.id = request;
  bd->serve = (sceSifServeData*)1;
  return 0;
}
}  // namespace ee