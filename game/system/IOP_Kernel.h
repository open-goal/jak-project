#pragma once

#ifndef JAK_IOP_KERNEL_H
#define JAK_IOP_KERNEL_H

#include <atomic>
#include <condition_variable>
#include <mutex>
#include <queue>
#include <string>
#include <thread>
#include <utility>
#include <vector>

#include "common/common_types.h"
#include "common/util/Assert.h"

#include "game/sce/iop.h"

#include "third-party/libco/libco.h"

class IOP_Kernel;
namespace iop {
struct sceSifQueueData;
}

using time_stamp = std::chrono::time_point<std::chrono::steady_clock, std::chrono::microseconds>;

struct SifRpcCommand {
  bool started = true;
  bool finished = true;

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

struct IopThread {
  enum class State {
    Run,
    Ready,
    Wait,
    WaitSuspend,
    Suspend,
    Dormant,
  };

  enum class Wait {
    None,
    Semaphore,
    Delay,
  };

  IopThread(std::string n, void (*f)(), s32 ID, u32 priority)
      : name(std::move(n)), function(f), priority(priority), thID(ID) {
    thread = co_create(0x300000, functionWrapper);
  }

  ~IopThread() { co_delete(thread); }

  static void functionWrapper();
  std::string name;
  void (*function)();
  cothread_t thread;
  State state = State::Dormant;
  Wait waitType = Wait::None;
  time_stamp resumeTime = {};
  u32 priority = 0;
  s32 thID = -1;
};

struct Semaphore {
  u32 option;
  u32 attr;
  s32 count;
  s32 maxCount;
  s32 initCount;
};

class IOP_Kernel {
 public:
  IOP_Kernel() {
    // this ugly hack
    threads.reserve(16);
    CreateThread("null-thread", nullptr, 0);
    CreateMbx();
    kernel_thread = co_active();
  }

  ~IOP_Kernel();

  s32 CreateThread(std::string n, void (*f)(), u32 priority);
  s32 ExitThread();
  void StartThread(s32 id);
  void DelayThread(u32 usec);
  void SleepThread();
  void WakeupThread(s32 id);
  time_stamp dispatch();
  void set_rpc_queue(iop::sceSifQueueData* qd, u32 thread);
  void rpc_loop(iop::sceSifQueueData* qd);
  void shutdown();

  /*!
   * Get current thread ID.
   */
  s32 getCurrentThread() {
    ASSERT(_currentThread);
    return _currentThread->thID;
  }

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

 private:
  void runThread(IopThread* thread);
  void exitThread();
  void updateDelay();
  void processWakeups();

  IopThread* schedNext();
  time_stamp nextWakeup();

  cothread_t kernel_thread;
  s32 _nextThID = 0;
  IopThread* _currentThread = nullptr;
  std::vector<IopThread> threads;
  std::vector<std::queue<void*>> mbxs;
  std::vector<SifRecord> sif_records;
  std::vector<Semaphore> semas;
  std::queue<int> wakeup_queue;
  bool mainThreadSleep = false;
  FILE* iso_disc_file = nullptr;
  std::mutex sif_mtx, wakeup_mtx;
};

#endif  // JAK_IOP_KERNEL_H
