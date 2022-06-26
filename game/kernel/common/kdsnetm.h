#pragma once

/*!
 * @file kdsnetm.h
 * Low-level DECI2 wrapper for ksocket
 * DONE!
 */

#include "common/listener_common.h"

#include "game/kernel/common/Ptr.h"

struct GoalProtoBlock {
  s32 socket = 0;
  ListenerMessageHeader* send_buffer = nullptr;
  ListenerMessageHeader* receive_buffer = nullptr;
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
void kdsnetm_init_globals_common();

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

/*!
 * Handle a DECI2 Protocol Event for the GOAL Proto.
 * Called by the DECI2 Protocol driver
 * DONE, EXACT
 */
void GoalProtoHandler(int event, int param, void* data);

/*!
 * Low level DECI2 send
 * Will block until send is complete.
 * DONE, original version used an uncached address and had a FlushCache call, which were both
 * removed
 */
s32 SendFromBufferD(s32 p1, u64 msg_id, char* data, s32 size);

/*!
 * Print GOAL Protocol status
 */
void GoalProtoStatus();
