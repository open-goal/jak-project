#include <cassert>
#include <cstring>
#include "IOP_Kernel.h"
#include "game/sce/iop.h"

/*!
 * Create a new thread.  Will not run the thread.
 */
s32 IOP_Kernel::CreateThread(std::string name, u32 (*func)()) {
  if (_currentThread != -1)
    throw std::runtime_error("tried to create thread from thread");
  u32 ID = (u32)_nextThID++;
  if (threads.size() != ID)
    throw std::runtime_error("thread number error?");
  // add entry
  threads.emplace_back(name, func, ID, this);
  // setup the thread!
  // printf("[IOP Kernel] SetupThread %s...\n", name.c_str());

  // hack to allow creating a "null thread" which doesn't/can't run but occupies slot 0.
  if (func) {
    _currentThread = ID;
    // create OS thread, will run the setupThread function
    threads.back().thread = new std::thread(&IOP_Kernel::setupThread, this, ID);
    // wait for thread to finish setup.
    threads.back().waitForReturnToKernel();
    // ensure we are back in the kernel.
    _currentThread = -1;
  }

  return ID;
}

/*!
 * Start a thread.  Runs it once, then marks it to run on each dispatch of the IOP kernel.
 */
void IOP_Kernel::StartThread(s32 id) {
  threads.at(id).started = true;  // mark for run
  runThread(id);                  // run now
}

/*!
 * Wrapper around entry for a thread.
 */
void IOP_Kernel::setupThread(s32 id) {
  // printf("\tthread %s has started!\n", threads.at(id).name.c_str());
  returnToKernel();
  threads.at(id).waitForDispatch();
  // printf("[IOP Kernel] Thread %s first dispatch!\n", threads.at(id).name.c_str());
  if (_currentThread != id) {
    throw std::runtime_error("the wrong thread has run!\n");
  }
  (threads.at(id).function)();
  printf("Thread %s has returned!\n", threads.at(id).name.c_str());
  threads.at(id).done = true;
  returnToKernel();
}

/*!
 * Run a thread (call from kernel)
 */
void IOP_Kernel::runThread(s32 id) {
  if (_currentThread != -1)
    throw std::runtime_error("tried to runThread in a thread");
  _currentThread = id;
  threads.at(id).dispatch();
  threads.at(id).waitForReturnToKernel();
  _currentThread = -1;
}

/*!
 * Suspend a thread (call from user thread).  Will simply allow other threads to run.
 * Unless we are sleeping, in which case this will return when we are woken up
 * Like yield
 */
void IOP_Kernel::SuspendThread() {
  s32 oldThread = getCurrentThread();
  threads.at(oldThread).returnToKernel();
  threads.at(oldThread).waitForDispatch();
  if (_currentThread != oldThread) {
    throw std::runtime_error("bad resume");
  }
}

/*!
 * Sleep a thread.  Must be explicitly woken up.
 */
void IOP_Kernel::SleepThread() {
  if (getCurrentThread() == -1) {
    mainThreadSleep = true;
    while (mainThreadSleep) {
      dispatchAll();
    }
  } else {
    threads.at(getCurrentThread()).started = false;
    SuspendThread();
  }
}

/*!
 * Wake up a thread. Doesn't run it immediately though.
 */
void IOP_Kernel::WakeupThread(s32 id) {
  if (id == -1) {
    mainThreadSleep = false;
  } else {
    threads.at(id).started = true;
  }
  // todo, should we ever switch directly to that thread?
}

/*!
 * Dispatch all IOP threads.
 */
void IOP_Kernel::dispatchAll() {
  for (u64 i = 0; i < threads.size(); i++) {
    if (threads[i].started && !threads[i].done) {
      //      printf("[IOP Kernel] Dispatch %s (%ld)\n", threads[i].name.c_str(), i);
      _currentThread = i;
      threads[i].dispatch();
      threads[i].waitForReturnToKernel();
      _currentThread = -1;
      // printf("[IOP Kernel] back to kernel!\n");
    }
  }
}

/*!
 * Start running kernel.
 */
void IopThreadRecord::returnToKernel() {
  runThreadReady = false;
  if (kernel->getCurrentThread() != thID)
    throw std::runtime_error("tried to sleep the wrong thread!");

  {
    std::lock_guard<std::mutex> lck(*threadToKernelMutex);
    syscallReady = true;
  }
  threadToKernelCV->notify_one();
}

/*!
 * Start running thread.
 */
void IopThreadRecord::dispatch() {
  syscallReady = false;
  if (kernel->getCurrentThread() != thID)
    throw std::runtime_error("tried to dispatch the wrong thread!");
  {
    std::lock_guard<std::mutex> lck(*kernelToThreadMutex);
    runThreadReady = true;
  }
  kernelToThreadCV->notify_one();
}

/*!
 * Kernel waits for thread to return
 */
void IopThreadRecord::waitForReturnToKernel() {
  std::unique_lock<std::mutex> lck(*threadToKernelMutex);
  threadToKernelCV->wait(lck, [this] { return syscallReady; });
  //  syscallReady = false;
}

/*!
 * Thread waits for kernel to dispatch it.
 */
void IopThreadRecord::waitForDispatch() {
  // if(kernel->getCurrentThread() == -1) throw std::runtime_error("tried to suspend main!\n");
  std::unique_lock<std::mutex> lck(*kernelToThreadMutex);
  kernelToThreadCV->wait(lck, [this] { return runThreadReady; });
  // runThreadReady = false;
}

void IOP_Kernel::set_rpc_queue(iop::sceSifQueueData* qd, u32 thread) {
  for (const auto& r : sif_records) {
    assert(!(r.qd == qd || r.thread_to_wake == thread));
  }
  SifRecord rec;
  rec.thread_to_wake = thread;
  rec.qd = qd;
  sif_records.push_back(rec);
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
  assert(found);
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
  assert(async);
  sif_mtx.lock();
  // step 1 - find entry
  SifRecord* rec = nullptr;
  for (auto& e : sif_records) {
    if (e.qd->serve_data->command == (u32)rpcChannel) {
      rec = &e;
    }
  }
  assert(rec);

  // step 2 - check entry is safe to give command to
  assert(rec->cmd.finished && rec->cmd.started);

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
      if (cmd.shutdown_now) {
        return;
      }

      if (!cmd.started) {
        // cf
        assert(func);
        auto data = func(cmd.fno, cmd.buff, cmd.size);
        if (cmd.copy_back_buff && cmd.copy_back_size) {
          memcpy(cmd.copy_back_buff, data, cmd.copy_back_size);
        }

        sif_mtx.lock();
        for (auto& r : sif_records) {
          if (r.qd == qd) {
            assert(r.cmd.started);
            r.cmd.finished = true;
          }
        }
        sif_mtx.unlock();
      }
    }
    SuspendThread();
  }
}

void IOP_Kernel::read_disc_sectors(u32 sector, u32 sectors, void* buffer) {
  if (!iso_disc_file) {
    iso_disc_file = fopen("./disc.iso", "rb");
  }

  assert(iso_disc_file);
  if (fseek(iso_disc_file, sector * 0x800, SEEK_SET)) {
    assert(false);
  }
  auto rv = fread(buffer, sectors * 0x800, 1, iso_disc_file);
  assert(rv == 1);
}

void IOP_Kernel::shutdown() {
  // shutdown most threads
  for (auto& r : sif_records) {
    r.cmd.shutdown_now = true;
  }

  for (auto& t : threads) {
    t.wantExit = true;
  }

  for (auto& t : threads) {
    if (t.thID == 0)
      continue;
    while (!t.done) {
      dispatchAll();
    }
    t.thread->join();
  }
}

IOP_Kernel::~IOP_Kernel() {
  if (iso_disc_file) {
    fclose(iso_disc_file);
  }
}