#include "IOP_Kernel.h"

#include <cstring>

#include "common/util/Assert.h"
#include <common/util/FileUtil.h>

#include "game/sce/iop.h"

/*!
 * Create a new thread.  Will not run the thread.
 */
s32 IOP_Kernel::CreateThread(std::string name, void (*func)()) {
  u32 ID = (u32)_nextThID++;
  ASSERT(ID == threads.size());

  // add entry
  threads.emplace_back(name, func, ID);

  return ID;
}

/*!
 * Start a thread. Marking it to run on each dispatch of the IOP kernel.
 */
void IOP_Kernel::StartThread(s32 id) {
  threads.at(id).state = IopThread::State::Ready;
}

/*!
 * Run a thread (call from kernel)
 */
void IOP_Kernel::runThread(s32 id) {
  ASSERT(_currentThread == -1);  // should run in the kernel thread
  _currentThread = id;
  threads.at(id).state = IopThread::State::Run;
  co_switch(threads.at(id).thread);
  _currentThread = -1;
}

/*!
 * Return to kernel from a thread, not to be called from the kernel thread.
 */
void IOP_Kernel::exitThread() {
  s32 oldThread = getCurrentThread();
  co_switch(kernel_thread);

  // check kernel resumed us correctly
  ASSERT(_currentThread == oldThread);
}

/*!
 * Suspend a thread (call from user thread).  Will simply allow other threads to run.
 * Like yield
 * This does not match the behaviour of any real IOP function.
 */
void IOP_Kernel::SuspendThread() {
  ASSERT(getCurrentThread() >= 0);

  threads.at(getCurrentThread()).state = IopThread::State::Ready;
  exitThread();
}

/*!
 * Sleep a thread.  Must be explicitly woken up.
 */
void IOP_Kernel::SleepThread() {
  ASSERT(getCurrentThread() >= 0);

  threads.at(getCurrentThread()).state = IopThread::State::Suspend;
  exitThread();
}

/*!
 * Wake up a thread. Doesn't run it immediately though.
 */
void IOP_Kernel::WakeupThread(s32 id) {
  ASSERT(id > 0);
  threads.at(id).state = IopThread::State::Ready;
}

/*!
 * Dispatch all IOP threads.
 * Currently does no scheduling, on the real IOP the highest priority therad that is Ready
 * will always be scheduled.
 */
void IOP_Kernel::dispatchAll() {
  for (s64 i = 0; i < threads.size(); i++) {
    if (threads[i].state == IopThread::State::Ready) {
      // printf("[IOP Kernel] Dispatch %s (%ld)\n", threads[i].name.c_str(), i);
      runThread(i);
      // printf("[IOP Kernel] back to kernel!\n");
    }
  }
}

void IOP_Kernel::set_rpc_queue(iop::sceSifQueueData* qd, u32 thread) {
  for (const auto& r : sif_records) {
    ASSERT(!(r.qd == qd || r.thread_to_wake == thread));
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
    SuspendThread();
  }
}

void IOP_Kernel::read_disc_sectors(u32 sector, u32 sectors, void* buffer) {
  if (!iso_disc_file) {
    iso_disc_file = file_util::open_file("./disc.iso", "rb");
  }

  ASSERT(iso_disc_file);
  if (fseek(iso_disc_file, sector * 0x800, SEEK_SET)) {
    ASSERT(false);
  }
  auto rv = fread(buffer, sectors * 0x800, 1, iso_disc_file);
  ASSERT(rv == 1);
}

IOP_Kernel::~IOP_Kernel() {
  if (iso_disc_file) {
    fclose(iso_disc_file);
  }
}
