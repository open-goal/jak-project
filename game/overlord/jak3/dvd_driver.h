#pragma once

#include <functional>

#include "common/common_types.h"

#include "game/overlord/jak3/isocommon.h"

namespace jak3 {
void jak3_overlord_init_globals_dvd_driver();

struct CDescriptor;
struct BlockParams;
struct CPage;
struct ISOFileDef;

struct CMsg {
  enum class MsgKind {
    LOCK = 0,
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

struct CMsgLock : public CMsg {
  CMsgLock();
  void handler() override;
};

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
  // todo CTOR
  CDvdDriver();
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
  void Lock();
  int GetDiskType() const { return disk_type; }
  s32 ValidateBlockParams(BlockParams* params, int num_params);
  int ReleaseFIFOSema(bool from_dvd_thread);
  int AcquireFIFOSema(bool from_dvd_thread);
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
  u8 needs_break;
  std::function<void(int)> callback;
  s32 current_thread_priority = 0;
  s32 m_nLockCount = 0;
  bool locked = false;
  //
  s32 disk_type;
  u8 tray_flag2;
  u8 trayflag3;
  u8 tray_flag;
  s32 m_nDvdThreadAccessSemaCount = 0;
};

// replacement for g_DvdDriver
CDvdDriver* get_driver();

}  // namespace jak3