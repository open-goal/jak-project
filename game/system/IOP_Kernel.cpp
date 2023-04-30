#include "IOP_Kernel.h"

#include <cstring>

#include "common/util/Assert.h"
#include "common/util/FileUtil.h"

#include "game/sce/iop.h"

using namespace std::chrono;

/*
** wrap thread entry points to ensure they don't return into libco
*/
static void (*thread_entry)() = nullptr;
static cothread_t wrap_return;
void IopThread::functionWrapper() {
  void (*f)() = thread_entry;
  co_switch(wrap_return);
  if (f != nullptr) {
    f();
  }
  // libco threads must not return
  while (true) {
    iop::ExitThread();
  }
}

/*
** -----------------------------------------------------------------------------
** Functions callable by threads
** -----------------------------------------------------------------------------
*/

/*!
 * Create a new thread.  Will not run the thread.
 */
s32 IOP_Kernel::CreateThread(std::string name, void (*func)(), u32 priority) {
  u32 ID = (u32)_nextThID++;
  ASSERT(ID == threads.size());

  // add entry
  threads.emplace_back(name, func, ID, priority);

  // enter the function wrapper so it can put the actual thread enry on its stack
  // to call it when the thread is eventually started
  thread_entry = func;
  wrap_return = co_active();
  co_switch(threads.at(ID).thread);

  return ID;
}

/*!
 * Start a thread. Marking it to run on each dispatch of the IOP kernel.
 */
void IOP_Kernel::StartThread(s32 id) {
  threads.at(id).state = IopThread::State::Ready;
}

s32 IOP_Kernel::ExitThread() {
  ASSERT(_currentThread);
  _currentThread->state = IopThread::State::Dormant;

  return 0;
}

/*!
 * Put a thread in Wait state for desired amount of usecs.
 */
void IOP_Kernel::DelayThread(u32 usec) {
  ASSERT(_currentThread);

  _currentThread->state = IopThread::State::Wait;
  _currentThread->waitType = IopThread::Wait::Delay;
  _currentThread->resumeTime =
      time_point_cast<microseconds>(steady_clock::now()) + microseconds(usec);
  leaveThread();
}

/*!
 * Sleep a thread.  Must be explicitly woken up.
 */
void IOP_Kernel::SleepThread() {
  ASSERT(_currentThread);

  _currentThread->state = IopThread::State::Suspend;
  leaveThread();
}

/*!
 * Wake up a thread. Doesn't run it immediately though.
 */
void IOP_Kernel::WakeupThread(s32 id) {
  ASSERT(id > 0);
  threads.at(id).state = IopThread::State::Ready;
}

void IOP_Kernel::iWakeupThread(s32 id) {
  ASSERT(id > 0);
  std::scoped_lock lock(wakeup_mtx);
  wakeup_queue.push(id);
}

s32 IOP_Kernel::WaitSema(s32 id) {
  auto& sema = semas.at(id);
  if (sema.count > 0) {
    sema.count--;
    return KE_OK;
  }

  sema.wait_list.push_back(_currentThread);
  _currentThread->state = IopThread::State::Wait;
  _currentThread->waitType = IopThread::Wait::Semaphore;
  leaveThread();

  return KE_OK;
}

s32 IOP_Kernel::SignalSema(s32 id) {
  auto& sema = semas.at(id);

  if (sema.count >= sema.maxCount) {
    return KE_SEMA_OVF;
  }

  if (sema.wait_list.empty()) {
    sema.count++;
    return KE_OK;
  }

  IopThread* to_run = nullptr;

  if (sema.attr == Semaphore::attribute::fifo) {
    to_run = sema.wait_list.front();
    sema.wait_list.pop_front();
  } else {
    auto it =
        std::max_element(sema.wait_list.begin(), sema.wait_list.end(),
                         [](IopThread*& a, IopThread*& b) { return a->priority < b->priority; });
    to_run = *it;
    sema.wait_list.erase(it);
  }

  to_run->waitType = IopThread::Wait::None;
  to_run->state = IopThread::State::Ready;
  return KE_OK;
}

s32 IOP_Kernel::PollSema(s32 id) {
  auto& sema = semas.at(id);
  if (sema.count > 0) {
    sema.count--;
    ASSERT(sema.count >= 0);
    return KE_OK;
  }

  return KE_SEMA_ZERO;
}

/*!
 * Return to kernel from a thread, not to be called from the kernel thread.
 */
void IOP_Kernel::leaveThread() {
  IopThread* oldThread = _currentThread;
  co_switch(kernel_thread);

  // check kernel resumed us correctly
  ASSERT(_currentThread == oldThread);
}

/*
** -----------------------------------------------------------------------------
** Kernel functions.
** -----------------------------------------------------------------------------
*/

/*!
 * Run a thread (call from kernel)
 */
void IOP_Kernel::runThread(IopThread* thread) {
  ASSERT(_currentThread == nullptr);  // should run in the kernel thread
  _currentThread = thread;
  thread->state = IopThread::State::Run;
  co_switch(thread->thread);
  _currentThread = nullptr;
}

/*!
** Update wait states for delayed threads
*/
void IOP_Kernel::updateDelay() {
  for (auto& t : threads) {
    if (t.waitType == IopThread::Wait::Delay) {
      if (steady_clock::now() > t.resumeTime) {
        t.waitType = IopThread::Wait::None;
        t.state = IopThread::State::Ready;
      }
    }
  }
}

std::optional<time_stamp> IOP_Kernel::nextWakeup() {
  bool found_ready = false;
  time_stamp lowest = time_point_cast<microseconds>(steady_clock::now()) + microseconds(1000);

  for (auto& t : threads) {
    if (t.waitType == IopThread::Wait::Delay) {
      if (t.resumeTime < lowest) {
        lowest = t.resumeTime;
      }
    }

    if (t.state == IopThread::State::Ready) {
      found_ready = true;
    }
  }

  if (found_ready) {
    return {};
  } else {
    return lowest;
  }
}

/*!
** Get next thread to run.
** i.e. Highest prio in ready state.
*/
IopThread* IOP_Kernel::schedNext() {
  IopThread* highest_prio = nullptr;

  for (auto& t : threads) {
    if (t.state == IopThread::State::Ready) {
      if (highest_prio == nullptr) {
        highest_prio = &t;
      }

      // Lower number = higher priority
      if (t.priority < highest_prio->priority) {
        highest_prio = &t;
      }
    }
  }

  return highest_prio;
};

void IOP_Kernel::processWakeups() {
  std::scoped_lock lock(wakeup_mtx);
  while (!wakeup_queue.empty()) {
    WakeupThread(wakeup_queue.front());
    wakeup_queue.pop();
  }
}

/*!
 * Run the next IOP thread.
 */
std::optional<time_stamp> IOP_Kernel::dispatch() {
  // Update thread states
  updateDelay();
  processWakeups();

  // Run until all threads are idle
  IopThread* next = schedNext();
  while (next != nullptr) {
    // Check vblank interrupt
    if (vblank_handler != nullptr && vblank_recieved) {
      vblank_handler(nullptr);
      vblank_recieved = false;
    }
    // printf("[IOP Kernel] Dispatch %s (%d)\n", next->name.c_str(), next->thID);
    runThread(next);
    updateDelay();
    processWakeups();
    next = schedNext();
    // printf("[IOP Kernel] back to kernel!\n");
  }

  // printf("[IOP Kernel] No runnable threads\n");
  return nextWakeup();
}

void IOP_Kernel::set_rpc_queue(iop::sceSifQueueData* qd, u32 thread) {
  sif_mtx.lock();
  for (const auto& r : sif_records) {
    ASSERT(!(r.qd == qd || r.thread_to_wake == thread));
  }
  SifRecord rec;
  rec.thread_to_wake = thread;
  rec.qd = qd;
  sif_records.push_back(rec);
  sif_mtx.unlock();
}

typedef void* (*sif_rpc_handler)(unsigned int, void*, int);

bool IOP_Kernel::sif_busy(u32 id) {
  sif_mtx.lock();
  bool rv = false;
  bool found = false;
  for (auto& r : sif_records) {
    if (r.qd->serve_data->command == id) {
      rv = !r.cmd.finished;
      found = true;
      break;
    }
  }
  ASSERT(found);
  sif_mtx.unlock();
  return rv;
}

void IOP_Kernel::sif_rpc(s32 rpcChannel,
                         u32 fno,
                         bool async,
                         void* sendBuff,
                         s32 sendSize,
                         void* recvBuff,
                         s32 recvSize) {
  ASSERT(async);
  sif_mtx.lock();
  // step 1 - find entry
  SifRecord* rec = nullptr;
  for (auto& e : sif_records) {
    if (e.qd->serve_data->command == (u32)rpcChannel) {
      rec = &e;
    }
  }
  if (!rec) {
    printf("Failed to find handler for sif channel 0x%x\n", rpcChannel);
  }
  ASSERT(rec);

  // step 2 - check entry is safe to give command to
  ASSERT(rec->cmd.finished && rec->cmd.started);

  // step 3 - memcpy!
  memcpy(rec->qd->serve_data->buff, sendBuff, sendSize);

  // step 4 - setup command
  rec->cmd.buff = rec->qd->serve_data->buff;
  rec->cmd.size = sendSize;
  rec->cmd.fno = fno;
  rec->cmd.copy_back_buff = recvBuff;
  rec->cmd.copy_back_size = recvSize;
  rec->cmd.started = false;
  rec->cmd.finished = false;

  iWakeupThread(rec->thread_to_wake);

  sif_mtx.unlock();
}

void IOP_Kernel::rpc_loop(iop::sceSifQueueData* qd) {
  while (true) {
    bool got_cmd = false;
    SifRpcCommand cmd;
    sif_rpc_handler func = nullptr;

    // get command and mark it as started if we get it
    sif_mtx.lock();
    for (auto& r : sif_records) {
      if (r.qd == qd) {
        cmd = r.cmd;
        got_cmd = true;
        r.cmd.started = true;
        func = r.qd->serve_data->func;
      }
    }
    sif_mtx.unlock();

    // handle command
    if (got_cmd) {
      if (!cmd.started) {
        // cf
        ASSERT(func);
        auto data = func(cmd.fno, cmd.buff, cmd.size);
        if (cmd.copy_back_buff && cmd.copy_back_size) {
          memcpy(cmd.copy_back_buff, data, cmd.copy_back_size);
        }

        sif_mtx.lock();
        for (auto& r : sif_records) {
          if (r.qd == qd) {
            ASSERT(r.cmd.started);
            r.cmd.finished = true;
          }
        }
        sif_mtx.unlock();
      }
    }

    SleepThread();
  }
}
