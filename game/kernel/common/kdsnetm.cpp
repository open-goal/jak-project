#include "kdsnetm.h"

#include <cstdio>

#include "game/kernel/common/kprint.h"
#include "game/sce/deci2.h"
#include "game/system/deci_common.h"

/*!
 * Current state of the GOAL Protocol
 */

GoalProtoBlock protoBlock;

/*!
 * Initialize global variables for kdsnetm
 */
void kdsnetm_init_globals_common() {
  protoBlock.reset();
}

/*!
 * Register GOAL DECI2 Protocol Driver with DECI2 service
 * DONE, EXACT
 */
void InitGoalProto() {
  protoBlock.socket = ee::sceDeci2Open(DECI2_PROTOCOL, &protoBlock, GoalProtoHandler);
  if (protoBlock.socket < 0) {
    MsgErr("gproto: open proto error\n");
  } else {
    protoBlock.send_buffer = nullptr;
    protoBlock.receive_buffer = MessBufArea.cast<ListenerMessageHeader>().c();
    protoBlock.send_status = -1;
    protoBlock.last_receive_size = -1;
    protoBlock.receive_progress = 0;
    protoBlock.deci2count.offset = 0;
    Msg(6, "gproto: proto open at socket %d\n", protoBlock.socket);
  }
}

/*!
 * Close the DECI2 Protocol Driver
 * DONE, EXACT
 */
void ShutdownGoalProto() {
  if (protoBlock.socket > 0) {
    ee::sceDeci2Close(protoBlock.socket);
  }
}

/*!
 * Handle a DECI2 Protocol Event for the GOAL Proto.
 * Called by the DECI2 Protocol driver
 * DONE, added print statements on errors for debugging, EI and SYNC at the end were removed
 */
void GoalProtoHandler(int event, int param, void* opt) {
  // verify we got the correct opt pointer.  It's not clear why the opt pointer is used
  // like this?
  GoalProtoBlock* pb = (GoalProtoBlock*)opt;
  if (&protoBlock != pb) {
    Msg(6, "gproto: BAD OPT POINTER PASSED IN!!!!\n");  // this print statement is in the game.
    pb = &protoBlock;
  }

  // increment deci2count, if it's set up
  if (pb->deci2count.offset) {
    *pb->deci2count = *pb->deci2count + 1;
  }

  // remember what event this is
  pb->most_recent_event = event;
  pb->most_recent_param = param;

  switch (event) {
    // get some data - param is the size
    case DECI2_READ:
      // sanity check the size
      if (pb->receive_progress + param <= (int)DEBUG_MESSAGE_BUFFER_SIZE) {
        // actually get data from DECI2
        s32 received =
            ee::sceDeci2ExRecv(pb->socket, ((u8*)pb->receive_buffer) + pb->receive_progress, param);

        if (received < 0) {
          // receive failure
          pb->last_receive_size = -1;
          protoBlock.receive_progress = 0;  // why use protoBlock instead of pb here?
          printf("gproto: read error with sceDeci2ExRecv\n");
        } else {
          pb->receive_progress += received;
        }
      } else {
        // size was too large
        pb->last_receive_size = -1;
        protoBlock.receive_progress = 0;  // why use protoBlock here?
        printf("gproto: read error, message too large!\n");
      }
      break;

      // read is finished!
    case DECI2_READDONE:
      // set last_receive_size to indicate that there is a pending message in the buffer.
      pb->last_receive_size = pb->receive_progress;
      pb->receive_progress = 0;
      break;

      // send some data
    case DECI2_WRITE: {
      // note that we should not attempt to send more than 0xffff bytes at a time, or this will be
      // wrong.  This is correctly checked for prints, but not for outputs.
      ASSERT(pb->send_remaining < 0xffff);
      // why and it with 0xffff?  Seems like saturation would be better.  Either way some data
      // will be lost, so I guess it doesn't matter.
      s32 sent = ee::sceDeci2ExSend(pb->socket, (void*)pb->send_ptr, pb->send_remaining & 0xffff);
      if (sent < 0) {
        // if we got an error, put it in send status, signaling a send error (negative)
        pb->send_status = sent;
      } else {
        // otherwise don't touch send status, leave it positive to indicate we're still sending
        pb->send_ptr += sent;
        pb->send_remaining -= sent;
      }
    } break;

      // done sending!
    case DECI2_WRITEDONE:
      if (pb->send_remaining <= 0) {
        // if we've send everything we want, set status to zero to indicate success
        pb->send_status = 0;
      } else {
        // otherwise, set send status to a negative number (the negative absolute value of
        // remaining)
        s32 a = pb->send_remaining;
        if (a < 0) {
          a = -a;
        }
        pb->send_status = -a;
      }
      break;

    case DECI2_CHSTATUS:
      break;

      // other events are undefined, so we just error.
    default:
      pb->last_receive_size = -1;
      break;
  }
}

/*!
 * Low level DECI2 send
 * Will block until send is complete.
 * DONE, original version used an uncached address and had a FlushCache call, which were both
 * removed
 */
s32 SendFromBufferD(s32 msg_kind, u64 msg_id, char* data, s32 size) {
  // wait for send to finish or error first...
  while (protoBlock.send_status > 0) {
    // on actual PS2, the kernel will run this in another thread.
    ee::LIBRARY_sceDeci2_run_sends();
  }

  // retry at most 10 times until we complete without an error.
  for (s32 i = 0; i < 10; i++) {
    // or'd with 0x20000000 to get noncache version
    ListenerMessageHeader* header = (ListenerMessageHeader*)(data - sizeof(ListenerMessageHeader));
    protoBlock.send_remaining = size + sizeof(ListenerMessageHeader);
    protoBlock.send_buffer = header;
    protoBlock.send_ptr = (u8*)header;

    protoBlock.send_status = size + sizeof(ListenerMessageHeader);
    // FlushCache(0);

    // set DECI2 message header
    header->deci2_header.len = protoBlock.send_remaining;
    header->deci2_header.rsvd = 0;
    header->deci2_header.proto = DECI2_PROTOCOL;
    header->deci2_header.src = 'E';  // from EE
    header->deci2_header.dst = 'H';  // to HOST

    // set GOAL message header
    header->msg_kind = (ListenerMessageKind)msg_kind;
    header->u6 = 0;
    header->msg_size = size;
    header->msg_id = msg_id;

    // start send!
    auto rv = ee::sceDeci2ReqSend(protoBlock.socket, header->deci2_header.dst);
    if (rv < 0) {
      printf("1sceDeci2ReqSend fail, reason code = %08x\n", rv);
      return 0xfffffffa;
    }

    // wait for send to complete or error.
    while (protoBlock.send_status > 0) {
      ee::LIBRARY_sceDeci2_run_sends();
    }

    // if send completes, exit.  Otherwise if there's an error, just try again.
    if (protoBlock.send_status == 0) {
      break;
    }
  }

  return 0;
}

/*!
 * Print GOAL Protocol status
 */
void GoalProtoStatus() {
  Msg(6, "gproto: got %d %d\n", protoBlock.most_recent_event, protoBlock.most_recent_param);
  Msg(6, "gproto: %d %d\n", protoBlock.last_receive_size, protoBlock.send_remaining);
}
