#include "dvd_driver.h"

#include <cstring>
#include <memory>

#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/util/FileUtil.h"

#include "game/overlord/jak3/isocommon.h"
#include "game/overlord/jak3/overlord.h"
#include "game/sce/iop.h"

namespace jak3 {
using namespace iop;
std::unique_ptr<CDvdDriver> g_DvdDriver;
s32 g_nDvdDriverThread = -1;

void jak3_overlord_init_globals_dvd_driver() {
  g_DvdDriver = std::make_unique<CDvdDriver>();
  g_nDvdDriverThread = -1;
}

CDvdDriver* get_driver() {
  return g_DvdDriver.get();
}

CMsg::CMsg(jak3::CMsg::MsgKind msg) : m_msg(msg) {
  m_ret = 1;
  m_thread = GetThreadId();
}

int CMsg::send() {
  // note: changed from passing data
  // and removed corresponding -4 to skip back past the vtable in DVD thread
  // (what were they thinking?)
  s32 ret = SendMbx(get_driver()->msgbox, this);
  if (ret == 0) {
    get_driver()->KickDvdThread();
    SleepThread();
    return m_ret;
  }
  return ret;
}

// CMsgLock::CMsgLock() : CMsg(CMsg::MsgKind::LOCK) {}
//
// void CMsgLock::handler() {
//   get_driver()->Lock();
//   m_ret = 0;
// }

// CMsgReadRaw::CMsgReadRaw(jak3::BlockParams* params) : CMsg(CMsg::MsgKind::READ_RAW) {
//   m_block_params = *params;
// }
//
// void CMsgReadRaw::handler() {
//   m_ret = get_driver()->ReadDirect(&m_block_params);
// }

CMsgCancelRead::CMsgCancelRead(jak3::CDescriptor* desc) : CMsg(CMsg::MsgKind::CANCEL_READ) {
  m_desc = desc;
}

void CMsgCancelRead::handler() {
  get_driver()->CancelRead(m_desc);
  m_ret = 0;
}

u32 DvdThread();

CDvdDriver::CDvdDriver() {
  fifo_entry_sema = -1;
  current_thread_priority = 0x13;
  disk_type = 5;
  tray_flag = 1;
  // m_nLockCount = 0;
  event_flag = -1;
  fifo_access_sema = -1;
  tray_flag2 = 1;
  initialized = 0;
  m_nNumFifoEntries = 0;
  ring_head = 0;
  ring_tail = 0;
  read_in_progress = 0;
  callback = nullptr;
  // locked = false;
  trayflag3 = 0;
  m_nDvdThreadAccessSemaCount = 0;
  memset(ring, 0, sizeof(Block) * 16);
}

void CDvdDriver::Initialize() {
  if (!initialized) {
    *this = {};
    ThreadParam thread_param;
    thread_param.attr = 0x2000000;
    // mbox_param.option = gDvdDriverThreadOptions; // ???
    thread_param.entry = DvdThread;
    thread_param.stackSize = 0x800;
    thread_param.initPriority = 0x13;
    thread_param.option = 0;
    strcpy(thread_param.name, "dvd");
    // mbox_param.attr = (int)PTR_DvdThread_00015c98; // ???
    g_nDvdDriverThread = CreateThread(&thread_param);
    ASSERT(g_nDvdDriverThread >= 0);

    SemaParam sema_param;
    sema_param.attr = 0;
    sema_param.init_count = 1;
    sema_param.max_count = 1;
    sema_param.option = 0;
    fifo_access_sema = CreateSema(&sema_param);
    ASSERT(fifo_access_sema >= 0);
    sema_param.max_count = 0x10;
    sema_param.attr = 0;
    sema_param.init_count = 0x10;
    sema_param.option = 0;
    fifo_entry_sema = CreateSema(&sema_param);
    ASSERT(fifo_entry_sema >= 0);
    MbxParam mbox_param;
    mbox_param.attr = 0;
    mbox_param.option = 0;
    msgbox = CreateMbx(&mbox_param);
    ASSERT(msgbox >= 0);

    EventFlagParam param;
    param.attr = 0;
    param.option = 0;
    param.init_pattern = 0;
    event_flag = CreateEventFlag(&param);
    ASSERT(event_flag >= 0);
    StartThread(g_nDvdDriverThread, 0);  // this...
  }
  initialized = 1;
}

void CDvdDriver::SetDriverCallback(std::function<void(int)> f) {
  callback = f;
}

// GetDriveCallback

// Poll - would kick the thread...

// void CDvdDriver::Lock() {
//   ASSERT_NOT_REACHED();
//   if (GetThreadId() == g_nDvdDriverThread) {
//     m_nLockCount++;
//     locked = true;
//     // needs break HACK
//     needs_break = false;
//   } else {
//     CMsgLock lock;
//     lock.send();
//   }
// }

// Read

int CDvdDriver::ReadMultiple(CDescriptor* descriptor,
                             int* pages_read_out,
                             BlockParams* params,
                             int num_blocks,
                             bool block_if_queue_full) {
  *pages_read_out = 0;
  s32 ret = 1;

  // check block parameters are reasonable
  if (ValidateBlockParams(params, num_blocks) != 0) {
    bool from_dvd_thread = GetThreadId() == g_nDvdDriverThread;
    if (from_dvd_thread) {
      // there is a setting to control if this function should block if there are too many
      // queued reads. If the is called from the DVD thread, then this would deadlock.
      // the original game ignored the block argument, but I'm asserting
      block_if_queue_full = 0;
      ASSERT_NOT_REACHED();
    }

    ovrld_log(LogCategory::DRIVER, "[driver] ReadMultiple (from our thread? {}) num_blocks {}",
              from_dvd_thread, num_blocks);

    ret = 0;
    if (0 < num_blocks) {
      // loop, until we've done all the requested reads.
      do {
        s32 acquired_slots = 0;
        if (0 < num_blocks) {
          // loop to try to get up to num_blocks slots in the fifo
          // but, if we get less, we'll take that too
          do {
            if (PollSema(this->fifo_entry_sema) == -0x1a3)
              break;
            acquired_slots = acquired_slots + 1;
          } while (acquired_slots < num_blocks);
        }
        ovrld_log(LogCategory::DRIVER, "[driver] ReadMultiple acquired {} slots in ring",
                  acquired_slots);

        // if we are blocking, and we acquired no slots, then we'll wait here until we get one slot.
        if ((block_if_queue_full != 0) && (acquired_slots < 1)) {
          ovrld_log(LogCategory::DRIVER, "[driver] ring is full, blocking!");
          acquired_slots = 1;  // the one we'll get from the WaitSema below
          do {
          } while (WaitSema(fifo_entry_sema) != 0);
        }

        // lock, now that we've gotten the slots
        AcquireFIFOSema(from_dvd_thread);
        num_blocks = num_blocks - acquired_slots;

        // if we didn't get any slots, bail.
        if (acquired_slots < 1) {
          ovrld_log(LogCategory::DRIVER, "[driver] ring is full, bailing!");
          ReleaseFIFOSema(from_dvd_thread);
          if (0 < *pages_read_out) {
            KickDvdThread();
          }
          return 2;
        }

        // loop, updating the ring for each slot we aquired
        do {
          auto* slot = ring + ring_tail;
          ovrld_log(LogCategory::DRIVER, "[driver] inserting in ring slot {}", ring_tail);

          ring_tail++;
          if (0xf < ring_tail) {
            ring_tail = 0;
          }
          m_nNumFifoEntries++;

          auto* tail = descriptor->m_pTail;
          slot->descriptor = descriptor;
          slot->params = params[*pages_read_out];
          *pages_read_out = (*pages_read_out) + 1;
          if (!tail) {
            descriptor->m_pHead = slot;
          } else {
            tail->next = slot;
          }
          acquired_slots = acquired_slots + -1;
          descriptor->m_pTail = slot;
          slot->next = nullptr;
        } while (acquired_slots != 0);
        ReleaseFIFOSema(from_dvd_thread);
        KickDvdThread();
        ret = 0;
      } while (0 < num_blocks);
    }
  } else {
    ASSERT_NOT_REACHED();
  }
  return ret;
}

void CDvdDriver::CancelRead(jak3::CDescriptor* desc) {
  if (GetThreadId() == g_nDvdDriverThread) {
    AcquireFIFOSema(true);
    if ((read_in_progress != 0) && (ring[ring_head].descriptor == desc)) {
      //      while (iVar1 = sceCdBreak(), iVar1 == 0) {
      //        DelayThread(8000);
      //        sceCdSync(0);
      //      }
      //      sceCdSync(0);
      read_in_progress = 0;
    }

    Block* iter = desc->m_pHead;
    Block* tail = desc->m_pTail;
    while (iter) {
      if (iter->descriptor) {
        CompletionHandler(iter, 6);
      }
      iter->descriptor = nullptr;
      iter->next = nullptr;
      if (iter == tail)
        break;
      iter = desc->m_pHead;
    }

    if (desc->m_pTail == tail) {
      desc->m_pTail = nullptr;
    }
    ReleaseFIFOSema(true);
  } else {
    CMsgCancelRead msg(desc);
    msg.send();
  }
}

s32 CDvdDriver::ValidateBlockParams(jak3::BlockParams* params, int num_params) {
  if (!params) {
    lg::die("Invalid BlockParams: nullptr");
    return 0;
  }
  if (num_params < 1) {
    lg::die("Invalid BlockParams: size == 0");
    return 0;
  }

  for (int i = 0; i < num_params; i++) {
    auto& p = params[i];
    if (p.destination == nullptr) {
      lg::die("Invalid BlockParams: {} had nullptr dest", i);
      return 0;
    }
    int kMaxSector = 1000000;
    if (p.sector_num > kMaxSector) {
      // this is just a sanity check - if we ever have larger files this is okay to increase.
      lg::die("Invalid BlockParams: {} had sector num {}", i, p.sector_num);
      return 0;
    }
    if (p.num_sectors > 0x1d0) {
      lg::die("Invalid BlockParams: {} had sector count {}", i, p.num_sectors);
      return 0;
    }
    if (!p.file_def) {
      lg::die("Invalid BlockParams: {} had no file", i);
      return 0;
    }
  }
  return 1;
}

void CDvdDriver::KickDvdThread() {
  while (SetEventFlag(event_flag, 1)) {
    ;
  }
}

int CDvdDriver::AcquireFIFOSema(bool from_dvd_thread) {
  int iVar1;
  int iVar2;

  if ((from_dvd_thread != 0) &&
      (iVar2 = m_nDvdThreadAccessSemaCount, m_nDvdThreadAccessSemaCount = iVar2 + 1, 0 < iVar2)) {
    return 0;
  }
  iVar1 = WaitSema(this->fifo_access_sema);
  iVar2 = 0;
  if ((iVar1 != 0) && (iVar2 = iVar1, from_dvd_thread != 0)) {
    m_nDvdThreadAccessSemaCount = m_nDvdThreadAccessSemaCount + -1;
    iVar2 = iVar1;
  }
  return iVar2;
}

int CDvdDriver::ReleaseFIFOSema(bool from_dvd_thread) {
  int iVar1;
  int iVar2;

  if (from_dvd_thread != 0) {
    iVar2 = m_nDvdThreadAccessSemaCount + -1;
    if (m_nDvdThreadAccessSemaCount < 1) {
      return -0x1a4;
    }
    m_nDvdThreadAccessSemaCount = iVar2;
    if (0 < iVar2) {
      return 0;
    }
  }
  iVar1 = SignalSema(fifo_access_sema);
  iVar2 = 0;
  if ((iVar1 != 0) && (iVar2 = iVar1, from_dvd_thread != 0)) {
    m_nDvdThreadAccessSemaCount = m_nDvdThreadAccessSemaCount + 1;
    iVar2 = iVar1;
  }
  return iVar2;
}

/*!
 * PC port added function to actually do a read of a block.
 */
void CDvdDriver::read_from_file(const jak3::Block* block) {
  const auto* fd = block->params.file_def;
  ASSERT(fd);
  FileCacheEntry* selected_entry = nullptr;

  // get a cache entry
  for (auto& entry : m_file_cache) {
    // if we already opened this file, use that
    if (entry.def == fd) {
      selected_entry = &entry;
      break;
    }

    // otherwise pick the least recently used
    if (!selected_entry) {
      selected_entry = &entry;
    } else {
      if (selected_entry->last_use_count > entry.last_use_count) {
        selected_entry = &entry;
      }
    }
  }

  // open a new file if needed
  if (selected_entry->def != fd) {
    lg::debug("CDvdDriver swapping files {} - > {}",
              selected_entry->def ? selected_entry->def->name.data : "NONE", fd->name.data);
    if (selected_entry->def) {
      fclose(selected_entry->fp);
    }

    selected_entry->def = fd;
    selected_entry->fp = file_util::open_file(fd->full_path, "rb");
    if (!selected_entry->fp) {
      lg::die("Failed to open {} {}", fd->full_path, strerror(errno));
    }
    selected_entry->offset_in_file = 0;
  }

  // increment use counter
  selected_entry->last_use_count = m_file_cache_counter++;

  const u64 desired_offset = block->params.sector_num * 0x800;

  // see if we're reading entirely past the end of the file
  if (desired_offset >= fd->length) {
    return;
  }

  if (selected_entry->offset_in_file != desired_offset) {
    lg::debug("CDvdDriver jumping in file {}: {} -> {}", fd->name.data,
              selected_entry->offset_in_file, desired_offset);
    if (fseek(selected_entry->fp, desired_offset, SEEK_SET)) {
      ASSERT_NOT_REACHED_MSG("Failed to fseek");
    }
    selected_entry->offset_in_file = desired_offset;
  }

  // read
  s64 read_length = block->params.num_sectors * 0x800;
  s64 extra_length = read_length + desired_offset - fd->length;
  if (extra_length > 0) {
    read_length -= extra_length;
  }
  auto ret = fread(block->params.destination, read_length, 1, selected_entry->fp);
  if (ret != 1) {
    lg::die("Failed to read {} {}, size {} of {} (ret {})", fd->full_path, strerror(errno),
            read_length, fd->length, ret);
  }
  selected_entry->offset_in_file += read_length;
}

u32 DvdThread() {
  auto* driver = get_driver();

  while (true) {
    // Poll for messages
    CMsg* msg = nullptr;
    while (true) {
      int poll_status = PollMbx((MsgPacket**)&msg, driver->msgbox);
      if (poll_status || !msg) {
        break;
      }
      // run message code
      msg->handler();
      // wake the waiting thread.
      WakeupThread(msg->m_thread);
    }

    bool completed = false;

    // if a read is in progress, wait for it to finish.
    if (driver->read_in_progress) {
      // TODO: if we switch to async reads, this is where we'd want to sync.
      // sceCdSync(0);
      completed = true;
      // error checking
    }

    driver->AcquireFIFOSema(true);
    // error handling

    // handle ring book-keeping.
    // note that these are somewhat double-buffered - we'll sync on read i-1, start read i, then
    // run the completion handler for read i - 1.
    // the completion is what actually removes stuff from the ring.

    s32 fifo_slots_freed = completed ? 1 : 0;
    s32 ring_entry = driver->ring_head + (completed ? 1 : 0);
    if (ring_entry > 15) {
      ring_entry = 0;
    }
    Block* block = driver->ring + ring_entry;
    s32 fifo_entries = driver->m_nNumFifoEntries - fifo_slots_freed;

    if (fifo_entries) {
      // start a new read.
      driver->read_in_progress = 1;
      ovrld_log(LogCategory::DRIVER, "[driver thread] Reading for slot {}", block - driver->ring);
      driver->read_from_file(block);

    } else {
      driver->read_in_progress = 0;
    }

    // run completion handler for the previous read - not the one we just started!
    Block sblock;
    if (completed) {
      auto* last_block = &driver->ring[driver->ring_head];
      ovrld_log(LogCategory::DRIVER, "[driver thread] Completion handler for {}",
                driver->ring_head);
      sblock = *last_block;

      if (sblock.descriptor && sblock.descriptor->m_pHead) {
        ASSERT(sblock.descriptor->m_pHead == last_block);
      }
      if (((sblock.descriptor)->m_pHead == last_block) &&
          ((sblock.descriptor)->m_pHead = &sblock, (sblock.descriptor)->m_pTail == last_block)) {
        (sblock.descriptor)->m_pTail = &sblock;
      }
    }
    driver->ring_head = ring_entry;
    driver->m_nNumFifoEntries = fifo_entries;
    while (0 < fifo_slots_freed) {
      fifo_slots_freed = fifo_slots_freed + -1;
      SignalSema(driver->fifo_entry_sema);
    }
    if (completed != 0) {
      driver->CompletionHandler(&sblock, 0);
    }
    driver->ReleaseFIFOSema(true);

    // this logic here was modified - we go to waiting if we have no more reads.
    if (driver->m_nNumFifoEntries == 0) {
      ovrld_log(LogCategory::DRIVER, "[driver thread] No work, waiting.");
      WaitEventFlag(driver->event_flag, 1, 0x11);
      ovrld_log(LogCategory::DRIVER, "[driver thread] Woken up!");
    }

    //    s32 status;
    //    if ((((driver->locked != false)) ||
    //         ((driver->needs_break == 0 && (driver->m_nNumFifoEntries == 0)))) &&
    //        ((status = WaitEventFlag(driver->event_flag, 1, 0x11),
    //          status != 0 && (status != -0x1a2)))) {
    //      if (status == -0x1a9) {
    //        do {
    //          SleepThread();
    //        } while (true);
    //      }
    //      DelayThread(8000);
    //    }
  }
}

void CDvdDriver::CompletionHandler(jak3::Block* block, int code) {
  // there is some janky thread priority changes here,
  // but they do not seem to be needed with the changes to the ISO thread.

  // PushPri(this,local_20,0x35);
  auto* desc = block->descriptor;
  if (desc && desc->m_pHead) {
    desc->m_status = code;
    int thread = desc->m_ThreadID;
    if (desc->m_Callback) {
      (desc->m_Callback)(desc->m_File, block, code);
    }
    if (0 < thread) {
      WakeupThread(thread);
    }

    Block* next;
    if ((desc->m_pHead == block) && (next = block->next, desc->m_pHead = next, next == nullptr)) {
      desc->m_pTail = nullptr;
    }
  }
  block->next = nullptr;
  block->descriptor = nullptr;
  // PopPri(this, local_20[0]);
}

CDvdDriver::~CDvdDriver() {
  for (auto& entry : m_file_cache) {
    if (entry.fp) {
      fclose(entry.fp);
    }
  }
}

}  // namespace jak3