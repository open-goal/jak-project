#pragma once

#ifndef JAK_IOP_KERNEL_H
#define JAK_IOP_KERNEL_H

#include <atomic>
#include <condition_variable>
#include <mutex>
#include <queue>
#include <string>
#include <thread>
#include <vector>

#include "common/common_types.h"
#include "common/util/Assert.h"

#include "game/sce/iop.h"

class IOP_Kernel;
namespace iop {
struct sceSifQueueData;
}

struct SifRpcCommand {
  bool started = true;
  bool finished = true;
  bool shutdown_now = false;

  void* buff;
  int fno;
  int size;

  void* copy_back_buff;
  int copy_back_size;
};

struct SifRecord {
  iop::sceSifQueueData* qd;
  SifRpcCommand cmd;
  u32 thread_to_wake;
};

struct IopThreadRecord {
  IopThreadRecord(std::string n, u32 (*f)(), s32 ID, IOP_Kernel* k)
      : name(n), function(f), thID(ID), kernel(k) {
    kernelToThreadCV = new std::condition_variable;
    threadToKernelCV = new std::condition_variable;
    kernelToThreadMutex = new std::mutex;
    threadToKernelMutex = new std::mutex;
  }

  ~IopThreadRecord() {
    delete kernelToThreadCV;
    delete threadToKernelCV;
    delete kernelToThreadMutex;
    delete threadToKernelMutex;
    delete thread;
  }

  std::string name;
  u32 (*function)();
  std::thread* thread = nullptr;
  bool wantExit = false;
  bool started = false;
  bool done = false;
  s32 thID = -1;
  IOP_Kernel* kernel;

  bool runThreadReady = false;
  bool syscallReady = false;
  std::mutex *kernelToThreadMutex, *threadToKernelMutex;
  std::condition_variable *kernelToThreadCV, *threadToKernelCV;

  void returnToKernel();
  void waitForReturnToKernel();
  void waitForDispatch();
  void dispatch();
};

class IOP_Kernel {
 public:
  IOP_Kernel() {
    // this ugly hack
    threads.reserve(16);
    CreateThread("null-thread", nullptr);
    CreateMbx();
  }

  ~IOP_Kernel();

  s32 CreateThread(std::string n, u32 (*f)());
  void StartThread(s32 id);
  void SuspendThread();
  void SleepThread();
  void WakeupThread(s32 id);
  void dispatchAll();
  void set_rpc_queue(iop::sceSifQueueData* qd, u32 thread);
  void rpc_loop(iop::sceSifQueueData* qd);
  void shutdown();

  /*!
   * Resume the kernel.
   */
  void returnToKernel() {
    ASSERT(_currentThread >= 0);  // must be in a thread
    threads[_currentThread].returnToKernel();
  }

  /*!
   * Get current thread ID.
   */
  s32 getCurrentThread() { return _currentThread; }

  /*!
   * Create a message box
   */
  s32 CreateMbx() {
    s32 id = mbxs.size();
    mbxs.emplace_back();
    return id;
  }

  /*!
   * Set msg to thing if its there and pop it.
   * Returns if it got something.
   */
  s32 PollMbx(void** msg, s32 mbx) {
    ASSERT(mbx < (s32)mbxs.size());
    s32 gotSomething = mbxs[mbx].empty() ? 0 : 1;
    if (gotSomething) {
      void* thing = mbxs[mbx].front();

      if (msg) {
        *msg = thing;
      }

      mbxs[mbx].pop();
    }

    return gotSomething ? KE_OK : KE_MBOX_NOMSG;
  }

  /*!
   * Push something into a mbx
   */
  s32 SendMbx(s32 mbx, void* value) {
    ASSERT(mbx < (s32)mbxs.size());
    mbxs[mbx].push(value);
    return 0;
  }

  s32 CreateSema() { return 1; }

  void read_disc_sectors(u32 sector, u32 sectors, void* buffer);
  bool sif_busy(u32 id);

  void sif_rpc(s32 rpcChannel,
               u32 fno,
               bool async,
               void* sendBuff,
               s32 sendSize,
               void* recvBuff,
               s32 recvSize);

  bool GetWantExit(s32 thid) const { return threads.at(thid).wantExit; }
  bool OnlyThreadAlive(s32 thid);

 private:
  void setupThread(s32 id);
  void runThread(s32 id);
  s32 _nextThID = 0;
  std::atomic<s32> _currentThread = {-1};
  std::vector<IopThreadRecord> threads;
  std::vector<std::queue<void*>> mbxs;
  std::vector<SifRecord> sif_records;
  bool mainThreadSleep = false;
  FILE* iso_disc_file = nullptr;
  std::mutex sif_mtx;
};

#endif  // JAK_IOP_KERNEL_H
