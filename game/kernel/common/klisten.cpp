#include "klisten.h"

#include <cstdio>
#include <cstring>

#include "common/listener_common.h"

#include "game/kernel/common/kdsnetm.h"
#include "game/kernel/common/kprint.h"
#include "game/kernel/common/ksocket.h"

Ptr<u32> print_column;
u32 ListenerStatus;

void klisten_init_globals() {
  print_column.offset = 0;
  ListenerStatus = 0;
}

/*!
 * Flush pending messages.  If debugging, will send to compiler, otherwise to stdout.
 */
void ClearPending() {
  if (!MasterDebug) {
    // if we aren't debugging print the print buffer to stdout.
    if (PrintPending.offset != 0) {
      auto size = strlen(PrintBufArea.cast<char>().c() + sizeof(ListenerMessageHeader));
      if (size > 0) {
        printf("%s", PrintBufArea.cast<char>().c() + sizeof(ListenerMessageHeader));
      }
      clear_print();
    }
  } else {
    if (ListenerStatus) {
      if (OutputPending.offset != 0) {
        Ptr<char> msg = OutputBufArea.cast<char>() + sizeof(ListenerMessageHeader);
        auto size = strlen(msg.c());
        // note - if size is ever greater than 2^16 this will cause an issue.
        SendFromBuffer(msg.c(), size);
        clear_output();
      }

      if (PrintPending.offset != 0) {
        char* msg = PrintBufArea.cast<char>().c() + sizeof(ListenerMessageHeader);
        auto size = strlen(msg);
        while (size > 0) {
          // sends larger than 64 kB are broken by the GoalProtoBuffer thing, so they are split
          auto send_size = size;
          if (send_size > 64000) {
            send_size = 64000;
          }
          SendFromBufferD(2, 0, msg, send_size);
          size -= send_size;
          msg += send_size;
        }
        clear_print();
      }
    }
  }
}

/*!
 * Send an "ack" message. The original game had the AckBufArea which stores "ack", but did not
 * calculate the length correctly, so the message would not actually contain the "ack" text.
 * The "ack" text is unimportant, as the compiler can recognize the messages as ACK due to the
 * ListenerMessageKind::MSG_ACK field.  Both the type and msg_id fields are sent, which is enough
 * for it to work.
 * Note: jak2 sent 0 length acks.
 */
void SendAck() {
  if (MasterDebug) {
    SendFromBufferD(u16(ListenerMessageKind::MSG_ACK), protoBlock.msg_id,
                    AckBufArea + sizeof(ListenerMessageHeader),
                    strlen(AckBufArea + sizeof(ListenerMessageHeader)));
  }
}
