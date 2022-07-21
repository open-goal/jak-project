#pragma once

#ifndef JAK1_IOP_THREAD_H
#define JAK1_IOP_THREAD_H

#include "IOP_Kernel.h"

#include "common/common_types.h"

enum IOP_Status { IOP_WAIT_FOR_LOAD, IOP_OVERLORD_INIT, IOP_OVERLORD_RUN, IOP_OVERLORD_STOP };

class IOP {
 public:
  IOP();
  ~IOP();
  void* iop_alloc(int size);
  void reset_allocator();
  void send_status(IOP_Status new_status);
  void wait_for_overlord_start_cmd();
  void wait_for_overlord_init_finish();
  void signal_overlord_init_finish();
  void signal_run_iop();

  void wait_run_iop(
      std::chrono::time_point<std::chrono::steady_clock, std::chrono::microseconds> duration);
  void kill_from_ee();

  void set_ee_main_mem(u8* mem) { ee_main_mem = mem; }

  IOP_Status status = IOP_WAIT_FOR_LOAD;

  char overlord_arg_data[512];
  char* overlord_argv[8];
  s32 overlord_argc;

  IOP_Kernel kernel;
  u8* ee_main_mem = nullptr;
  bool want_exit = false;

 private:
  std::vector<void*> allocations;
  std::condition_variable cv;
  std::mutex iop_mutex, run_cv_mutex;
  bool overlord_init_done = false;
  std::condition_variable iop_run_cv;
};

#endif  // JAK1_IOP_THREAD_H
