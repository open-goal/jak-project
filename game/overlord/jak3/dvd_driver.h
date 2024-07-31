#pragma once

#include <functional>

#include "common/common_types.h"

#include "game/overlord/jak3/isocommon.h"

namespace jak3 {
void jak3_overlord_init_globals_dvd_driver();

/*!
 * The DVD Driver is what actually called the Sony CD library functions.
 * It also had many special cases for handling errors, open trays, retries, etc that are not
 * recreated in this port.
 *
 * The original DVD driver thread didn't run the actual reads - the Sony CD library had its own
 * threads for async reads. However, they would call sceCdSync() to wait until a read finished.
 * It's not clear if sceCdSync would allow other threads to run while waiting on the read to finish.
 * but if it did, this is a bit of a difference in the blocking behavior.
 *
 * This is something that could be revisited, as freads will now essentially block the entire
 * overlord, including refilling sound RAM.
 */

struct CDescriptor;
struct BlockParams;
struct CPage;
struct ISOFileDef;

// The CMSG system is used to pass messages from external threads to the driver. It's mostly used
// internally by the driver, when functions are called on it from outside threads. In these cases
// the Cmsg will get send to the driver thread and handler will run on it here.

struct CMsg {
  enum class MsgKind {
    // LOCK = 0,
    READ_RAW = 1,
    CANCEL_READ = 2,
  };
  CMsg(MsgKind msg);
  virtual int send();
  virtual void handler() = 0;

  u8 data[8];
  MsgKind m_msg;
  int m_thread;
  int m_ret;
};

// struct CMsgLock : public CMsg {
//   CMsgLock();
//   void handler() override;
// };

// struct CMsgReadRaw : public CMsg {
//   explicit CMsgReadRaw(BlockParams* params);
//   void handler() override;
//   BlockParams m_block_params;
// };

struct CMsgCancelRead : public CMsg {
  explicit CMsgCancelRead(CDescriptor* desc);
  void handler() override;
  CDescriptor* m_desc;
};

struct CISOCDFile;

/*!
 * Reference to an ongoing or requested read at the driver level, possibly made up of multiple
 * blocks. In this case, the head/tail pointers point to Blocks stored inside the CDvdDriver's
 * internal FIFO.
 */
struct CDescriptor {
  CDescriptor() = default;
  int m_unk0 = 0;
  void (*m_Callback)(CISOCDFile*, Block*, s32) = nullptr;
  int m_ThreadID = 0;
  int m_status = 0;
  CISOCDFile* m_File = nullptr;
  Block* m_pHead = nullptr;
  Block* m_pTail = nullptr;
};

class CDvdDriver {
 public:
  CDvdDriver();
  ~CDvdDriver();
  void Initialize();
  void CancelRead(CDescriptor* descriptor);
  int ReadMultiple(CDescriptor* descriptor,
                   int* pages_read_out,
                   BlockParams* params,
                   int num_pages,
                   bool block_if_queue_full);
  void SetDriverCallback(std::function<void(int)> f);
  // int ReadDirect(BlockParams* params);
  void KickDvdThread();
  // void Lock();
  int GetDiskType() const { return disk_type; }
  s32 ValidateBlockParams(BlockParams* params, int num_params);
  int ReleaseFIFOSema(bool from_dvd_thread);
  int AcquireFIFOSema(bool from_dvd_thread);
  void read_from_file(const Block* block);
  void CompletionHandler(Block* block, int code);

  u8 initialized = 0;
  s32 event_flag = -1;
  s32 fifo_access_sema = -1;
  s32 fifo_entry_sema = -1;
  s32 msgbox = -1;
  s32 m_nNumFifoEntries = 0;
  s32 ring_head = 0;
  s32 ring_tail = 0;
  Block ring[16];
  u8 read_in_progress;  // more likely: read in progress
  std::function<void(int)> callback;
  s32 current_thread_priority = 0;
  // s32 m_nLockCount = 0;
  // bool locked = false;
  //
  s32 disk_type;
  u8 tray_flag2;
  u8 trayflag3;
  u8 tray_flag;
  s32 m_nDvdThreadAccessSemaCount = 0;

 private:
  struct FileCacheEntry {
    const ISOFileDef* def = nullptr;
    FILE* fp = nullptr;
    u32 last_use_count = 0;
    u64 offset_in_file = 0;
  };
  u32 m_file_cache_counter = 0;
  static constexpr int kNumFileCacheEntries = 6;
  FileCacheEntry m_file_cache[kNumFileCacheEntries];
};

// replacement for g_DvdDriver
CDvdDriver* get_driver();

}  // namespace jak3