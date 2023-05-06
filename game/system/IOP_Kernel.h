#pragma once

#include <atomic>
#include <condition_variable>
#include <list>
#include <mutex>
#include <optional>
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
  enum class attribute { fifo, prio };
  Semaphore(attribute attr, s32 option, s32 init_count, s32 max_count)
      : attr(attr), option(option), count(init_count), initCount(init_count), maxCount(max_count) {}

  attribute attr{attribute::fifo};
  u32 option{0};
  s32 count{0};
  s32 initCount{0};
  s32 maxCount{0};

  std::list<IopThread*> wait_list;
};

class IOP_Kernel {
 public:
  IOP_Kernel() {
    // this ugly hack
    threads.reserve(16);
    CreateThread("null-thread", nullptr, 0);
    CreateMbx();
    CreateSema(0, 0, 0, 0);
    kernel_thread = co_active();
  }

  s32 CreateThread(std::string n, void (*f)(), u32 priority);
  s32 ExitThread();
  void StartThread(s32 id);
  void DelayThread(u32 usec);
  void SleepThread();
  void WakeupThread(s32 id);
  void iWakeupThread(s32 id);
  std::optional<time_stamp> dispatch();
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

  s32 PeekMbx(s32 mbx) { return !mbxs[mbx].empty(); }

  /*!
   * Push something into a mbx
   */
  s32 SendMbx(s32 mbx, void* value) {
    ASSERT(mbx < (s32)mbxs.size());
    mbxs[mbx].push(value);
    return 0;
  }

  s32 CreateSema(s32 attr, s32 option, s32 init_count, s32 max_count) {
    s32 id = semas.size();
    semas.emplace_back((Semaphore::attribute)attr, option, init_count, max_count);
    return id;
  }

  s32 WaitSema(s32 id);
  s32 SignalSema(s32 id);
  s32 PollSema(s32 id);

  s32 RegisterVblankHandler(int (*handler)(void*)) {
    vblank_handler = handler;
    return 0;
  }

  void signal_vblank() { vblank_recieved = true; };

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
  void leaveThread();
  void updateDelay();
  void processWakeups();

  IopThread* schedNext();
  std::optional<time_stamp> nextWakeup();

  s32 (*vblank_handler)(void*);
  std::atomic_bool vblank_recieved = false;

  cothread_t kernel_thread;
  s32 _nextThID = 0;
  IopThread* _currentThread = nullptr;
  std::vector<IopThread> threads;
  std::vector<std::queue<void*>> mbxs;
  std::vector<SifRecord> sif_records;
  std::vector<Semaphore> semas;
  std::queue<int> wakeup_queue;
  bool mainThreadSleep = false;
  std::mutex sif_mtx, wakeup_mtx;
};
