#include "iop_thread.h"

#include <io.h>
#include "SystemThread.h"
//#include "shared_config.h"
//#include "ps2/SCE_IOP.h"
//#include "overlord/ramdisk.h"
//#include "ps2/SCE_SIF.h"
//#include "IOP.h"
//
//#include "overlord/dma.h"
//#include "overlord/fake_iso.h"
//#include "overlord/iso.h"
//#include "overlord/iso_api.h"
//#include "overlord/iso_cd.h"
//#include "overlord/iso_queue.h"
//#include "overlord/isocommon.h"
//#include "overlord/overlord.h"
//#include "overlord/ramdisk.h"
//#include "overlord/sbank.h"
//#include "overlord/soundcommon.h"
//#include "overlord/srpc.h"
//#include "overlord/ssound.h"
//#include "overlord/stream.h"

IOP::IOP() {}

void IOP::send_status(IOP_Status new_status) {
  {
    std::lock_guard<std::mutex> lck(iop_mutex);
    status = new_status;
  }
  cv.notify_all();
}

void IOP::wait_for_overlord_start_cmd() {
  std::unique_lock<std::mutex> lk(iop_mutex);
  if (status != IOP_WAIT_FOR_LOAD)
    return;

  cv.wait(lk, [&] { return status != IOP_WAIT_FOR_LOAD; });
}

void IOP::wait_for_overlord_init_finish() {
  std::unique_lock<std::mutex> lk(iop_mutex);
  if (overlord_init_done)
    return;

  cv.wait(lk, [&] { return overlord_init_done; });
}

void IOP::signal_overlord_init_finish() {
  std::unique_lock<std::mutex> lk(iop_mutex);
  overlord_init_done = true;
  cv.notify_all();
}

void IOP::reset_allocator() {
  for (auto x : allocations) {
    free(x);
  }
  allocations.clear();
}

void* IOP::iop_alloc(int size) {
  void* mem = malloc(size);
  allocations.push_back(mem);
  return mem;
}

void IOP::wait_run_iop() {
  std::unique_lock<std::mutex> lk(iters_mutex);
  if (iop_iters_des > iop_iters_act) {
    iop_iters_act++;
    return;
  }

  iop_run_cv.wait(lk, [&] { return iop_iters_des > iop_iters_act; });
  iop_iters_act++;
}

void IOP::kill_from_ee() {
  want_exit = true;
  signal_run_iop();
}

void IOP::signal_run_iop() {
  std::unique_lock<std::mutex> lk(iters_mutex);
  iop_iters_des += 100;  // todo, tune this
  iop_run_cv.notify_all();
}

IOP::~IOP() {
  reset_allocator();
}

// void launch_iop(SystemThreadInterface& interface) {
//  IOP iop;
//
//  printf("\n\n\n[IOP] Restart!\n");
//  iop.reset_allocator();
//
////  dma_init_globals();
////  iso_init_globals();
////  fake_iso_init_globals();
////  // iso_api
////  iso_cd_init_globals();
////  iso_queue_init_globals();
////  // isocommon
////  // overlord
////  ramdisk_init_globals();
////  // sbank
////  // soundcommon
////  srpc_init_globals();
//  // ssound
//  // stream
//
////  SCE_IOP::PS2_RegisterIOP(&iop);
////  PS2_RegisterIOP_EE(&iop);
//  interfaces.initialization_complete();
//
//  printf("[IOP] Wait for OVERLORD to be started...\n");
//  iop.wait_for_overlord_start_cmd();
//  if(iop.status == IOP_OVERLORD_INIT) {
//    printf("[IOP] Run!\n");
//  } else {
//    printf("[IOP] shutdown!\n");
//    return;
//  }
//
//  iop.reset_allocator();
//
//  // init
//#ifdef ENABLE_OVERLORD
//  start(iop.overlord_argc, iop.overlord_argv);
//#endif
//
//  // unblock the EE, the overlord is set up!
//  iop.signal_overlord_init_finish();
//
//  // IOP Kernel loop
//  while(!interface.get_want_exit() && !iop.want_exit) {
//    // the IOP kernel just runs at full blast, so we only run the IOP when the EE is waiting on
//    the IOP.
//    // Each time the EE is waiting on the IOP, it will run an iteration of the IOP kernel.
//    iop.wait_run_iop();
//    iop.kernel.dispatchAll();
//  }
//
//  // stop all threads in the iop kernel.
//  // if the threads are not stopped nicely, we will deadlock on trying to destroy the kernel's
//  condition variables. iop.kernel.shutdown();
//}