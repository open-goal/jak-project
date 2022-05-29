#include "iop_thread.h"

#ifdef __linux__
#include <unistd.h>
#elif _WIN32
#include <io.h>
#endif
#include "SystemThread.h"

#include <cstring>

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
  memset(mem, 0xaa, size);
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
  signal_run_iop(true);
}

void IOP::signal_run_iop(bool force) {
  std::unique_lock<std::mutex> lk(iters_mutex);
  if (iop_iters_act == iop_iters_des || force) {
    iop_iters_des++;  // todo, tune this
    if (iop_iters_des - iop_iters_act > 500) {
      iop_iters_des = iop_iters_act + 500;
    }
    iop_run_cv.notify_all();
  }
}

IOP::~IOP() {
  reset_allocator();
}