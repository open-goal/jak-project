/*!
 * @file kdsnetm.h
 * Low-level DECI2 wrapper for ksocket
 * DONE!
 */

#ifndef JAK_KDSNETM_H
#define JAK_KDSNETM_H

#include "Ptr.h"
#include "common/listener_common.h"

struct GoalMessageHeader {
  Deci2Header deci2_hdr;
  u16 msg_kind;
  u16 u6;
  u32 msg_size;
  u64 msg_id;
};

constexpr u16 DECI2_PROTOCOL = 0xe042;

struct GoalProtoBlock {
  s32 socket = 0;
  GoalMessageHeader* send_buffer = nullptr;
  GoalMessageHeader* receive_buffer = nullptr;
  u8* send_ptr = nullptr;
  s32 send_remaining = 0;
  s32 send_status =
      0;  // positive means send in progress, negative means send error, 0 means complete.

  // size of pending receive to process.
  s32 last_receive_size = 0;
  s32 receive_progress = 0;
  u32 most_recent_event = 0;
  u32 most_recent_param = 0;
  u32 msg_kind = 0;
  u64 msg_id = 0;
  Ptr<s32> deci2count;

  void reset() { *this = GoalProtoBlock(); }
};

/*!
 * Current state of the GOAL Protocol
 */
extern GoalProtoBlock protoBlock;

/*!
 * Initialize global variables for kdsnetm
 */
void kdsnetm_init_globals();

// TODO-WINDOWS
#ifdef __linux__
/*!
 * Register GOAL DECI2 Protocol Driver with DECI2 service
 * DONE, EXACT
 */
void InitGoalProto();

/*!
 * Close the DECI2 Protocol Driver
 * DONE, EXACT
 */
void ShutdownGoalProto();

#endif

/*!
 * Handle a DECI2 Protocol Event for the GOAL Proto.
 * Called by the DECI2 Protocol driver
 * DONE, EXACT
 */
void GoalProtoHandler(int event, int param, void* data);

// TODO-WINDOWS
#ifdef __linux__
/*!
 * Low level DECI2 send
 * Will block until send is complete.
 * DONE, original version used an uncached address and had a FlushCache call, which were both
 * removed
 */
s32 SendFromBufferD(s32 p1, u64 p2, char* data, s32 size);
#endif

/*!
 * Print GOAL Protocol status
 */
void GoalProtoStatus();

#endif  // JAK_KDSNETM_H
