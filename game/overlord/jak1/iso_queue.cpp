#include "iso_queue.h"

#include <cstdio>
#include <cstring>

#include "isocommon.h"

#include "common/log/log.h"
#include "common/util/Assert.h"

#include "game/overlord/jak1/iso.h"
#include "game/sce/iop.h"

using namespace iop;

namespace jak1 {
constexpr int N_BUFFERS = 4;
constexpr int N_STR_BUFFERS = 1;
constexpr int N_VAG_CMDS = 64;

struct IsoBuffer {
  IsoBufferHeader header;
  u8 data[BUFFER_PAGE_SIZE];
};

struct IsoStrBuffer {
  IsoBufferHeader header;
  u8 data[STR_BUFFER_DATA_SIZE];
};

static IsoBuffer sBuffer[N_BUFFERS];
static IsoStrBuffer sStrBuffer[N_STR_BUFFERS];
static IsoBuffer* sFreeBuffer;
static IsoStrBuffer* sFreeStrBuffer;
PriStackEntry gPriStack[N_PRIORITIES];

u32 vag_cmd_cnt;
u32 vag_cmd_used;
u32 max_vag_cmd_cnt;
VagCommand vag_cmds[N_VAG_CMDS];

static s32 sSema;

void iso_queue_init_globals() {
  memset(sBuffer, 0, sizeof(sBuffer));
  memset(sStrBuffer, 0, sizeof(sStrBuffer));
  sFreeBuffer = nullptr;
  sFreeStrBuffer = nullptr;
  for (auto& e : gPriStack)
    e.reset();

  vag_cmd_cnt = 0;
  vag_cmd_used = 0;
  max_vag_cmd_cnt = 0;
  memset(vag_cmds, 0, sizeof(vag_cmds));
  sSema = 0;
}

void PriStackEntry::reset() {
  for (auto& c : cmds)
    c = nullptr;
  n = 0;
  for (auto& x : names)
    x.clear();
}

void InitBuffers() {
  // chain all buffers together and set them as free.
  for (uint32_t i = 0; i < N_BUFFERS; i++) {
    sBuffer[i].header.data = nullptr;
    sBuffer[i].header.data_size = 0;
    sBuffer[i].header.buffer_size = BUFFER_PAGE_SIZE;
    sBuffer[i].header.next = &sBuffer[i + 1].header;
  }
  sBuffer[N_BUFFERS - 1].header.next = nullptr;
  sFreeBuffer = &sBuffer[0];

  for (uint32_t i = 0; i < N_STR_BUFFERS; i++) {
    sStrBuffer[i].header.data = nullptr;
    sStrBuffer[i].header.data_size = 0;
    sStrBuffer[i].header.buffer_size = STR_BUFFER_DATA_SIZE;
    sStrBuffer[i].header.next = &sStrBuffer[i + 1].header;
  }
  sStrBuffer[N_STR_BUFFERS - 1].header.next = nullptr;
  sFreeStrBuffer = &sStrBuffer[0];

  SemaParam params;
  params.attr = SA_THPRI;
  params.max_count = 1;
  params.init_count = 1;
  params.option = 0;
  sSema = CreateSema(&params);

  if (sSema < 0) {
    for (;;) {
      printf("[OVERLORD] VAG Semaphore creation failed!\n");
    }
  }
}

/*!
 * Allocate a buffer of the given size. If not possible, loop forever. Size must be BUFFER_PAGE_SIZE
 * or STR_BUFFER_DATA_SIZE,
 */
IsoBufferHeader* AllocateBuffer(uint32_t size) {
  IsoBufferHeader* buffer = TryAllocateBuffer(size);
  if (buffer) {
    return buffer;
  } else {
    while (true) {
      printf("[OVERLORD ISO QUEUE] Failed to allocate buffer!\n");
    }
  }
}

/*!
 * Allocate a buffer of given size.  If the size isn't BUFFER_PAGE_SIZE, you get a streaming buffer
 * (STR_BUFFER_DATA_SIZE). If no allocation can be done, return nullptr.
 */
IsoBufferHeader* TryAllocateBuffer(uint32_t size) {
  IsoStrBuffer* top_str = sFreeStrBuffer;
  IsoBuffer* top_buff = sFreeBuffer;

  if (size == BUFFER_PAGE_SIZE) {
    if (sFreeBuffer) {
      auto next = sFreeBuffer->header.next;
      sFreeBuffer->header.data = nullptr;
      sFreeBuffer = (IsoBuffer*)next;
      top_buff->header.data_size = 0;
      top_buff->header.next = nullptr;
      return (IsoBufferHeader*)top_buff;
    }
  } else {
    if (sFreeStrBuffer) {
      auto next = sFreeStrBuffer->header.next;
      sFreeStrBuffer->header.data = nullptr;
      sFreeStrBuffer = (IsoStrBuffer*)next;
      top_str->header.data_size = 0;
      top_str->header.next = nullptr;
      return (IsoBufferHeader*)top_str;
    }
  }
  printf("[OVERLORD] Failed to allocate buffer (requested size 0x%x)\n", size);
  return nullptr;
}

/*!
 * Return a buffer once you are done using it so somebody else can have a turn
 */
void FreeBuffer(IsoBufferHeader* buffer) {
  IsoBufferHeader* b = (IsoBufferHeader*)buffer;
  if (b->buffer_size == BUFFER_PAGE_SIZE) {
    b->next = sFreeBuffer;
    sFreeBuffer = (IsoBuffer*)b;
  } else {
    b->next = sFreeStrBuffer;
    sFreeStrBuffer = (IsoStrBuffer*)b;
  }
}

/*!
 * Display all messages in the priority stack
 * The actual function does nothing.
 */
void DisplayQueue() {
  for (int pri = 0; pri < N_PRIORITIES; pri++) {
    for (int cmd = 0; cmd < (int)gPriStack[pri].n; cmd++) {
      lg::debug("  PRI {} elt {} {} @ #x{:X}", pri, cmd, gPriStack[pri].names[cmd],
                (u64)gPriStack[pri].cmds[cmd]);
    }
  }
}

/*!
 * Add a message to the back of the queue for the given priority.
 * If there is no room left in the queue, ReturnMessage with a CMD_STATUS_FAILED_TO_QUEUE.
 * Return 1 on success.
 */
u32 QueueMessage(IsoMessage* cmd, int32_t priority, const char* name) {
  u32 ok = gPriStack[priority].n != PRI_STACK_LENGTH;
  if (ok) {
    gPriStack[priority].cmds[gPriStack[priority].n] = cmd;
    gPriStack[priority].names[gPriStack[priority].n] = name;
    gPriStack[priority].n++;
    lg::debug("[OVERLORD] Queue {} ({}/{}), {}", priority, gPriStack[priority].n, PRI_STACK_LENGTH,
              name);
    DisplayQueue();
  } else {
    lg::warn("[OVERLORD ISO QUEUE] Failed to queue!");
    cmd->status = CMD_STATUS_FAILED_TO_QUEUE;
    ReturnMessage(cmd);
  }
  return ok;
}

/*!
 * Remove a message from the priority stack.
 */
void UnqueueMessage(IsoMessage* cmd) {
  int pri = 0;
  u32 idx = 0;
  PriStackEntry* pse;

  // loop over priorities
  for (pri = 0; pri < N_PRIORITIES; pri++) {
    pse = &gPriStack[pri];

    // loop over entries
    for (idx = 0; idx < pse->n; idx++) {
      if (pse->cmds[idx] == cmd) {
        goto found;
      }
    }
  }
  lg::warn("[OVERLORD ISO QUEUE] Failed to unqueue!");
  return;

found:
  ASSERT(pse->cmds[idx] == cmd);

  // pop
  pse->n--;
  // and move other entries up.
  while (idx < pse->n) {
    pse->cmds[idx] = pse->cmds[idx + 1];
    idx++;
  }
  DisplayQueue();
}

/*!
 * Get the highest priority message with an open buffer.
 * (Note - messages with priority less than max priority will be gotten if they have < 2 buffers
 * filled)
 * @return
 */
IsoMessage* GetMessage() {
  // loop over all priorities
  for (int pri = (N_PRIORITIES - 1); pri >= 0; pri--) {
    auto pse = &gPriStack[pri];
    int idx = pse->n;
    for (idx = idx - 1; idx >= 0; idx--) {
      if (pse->cmds[idx]->fd && pse->cmds[idx]->status == CMD_STATUS_IN_PROGRESS &&
          pse->cmds[idx]->ready_for_data) {
        if (pri == N_PRIORITIES - 1) {
          // return high priority commands only if they don't have any buffers filled
          if (!pse->cmds[idx]->callback_buffer) {
            return pse->cmds[idx];
          }
        } else {
          // return lower priority commands if they don't have 2 buffers filled.
          if (!pse->cmds[idx]->callback_buffer ||
              !(IsoBufferHeader*)(pse->cmds[idx]->callback_buffer)->next) {
            return pse->cmds[idx];
          }
        }
      }
    }
  }
  return nullptr;
}

/*!
 * Execute callbacks and maintain buffers for finished reads in the priority stack
 */
void ProcessMessageData() {
  for (s32 pri = N_PRIORITIES - 1; pri >= 0; pri--) {
    for (s32 n = gPriStack[pri].n - 1; n >= 0; n--) {
      IsoMessage* cmd = gPriStack[pri].cmds[n];

      if (cmd->status == CMD_STATUS_IN_PROGRESS) {
        IsoBufferHeader* callback_buffer = cmd->callback_buffer;

        if (callback_buffer != nullptr) {
          cmd->status = cmd->callback_function(cmd, callback_buffer);

          if (callback_buffer->data_size == 0) {
            cmd->callback_buffer = (IsoBufferHeader*)callback_buffer->next;
            FreeBuffer(callback_buffer);
          }
        }
      }

      if (cmd->status != CMD_STATUS_IN_PROGRESS) {
        ReleaseMessage(cmd);
        ReturnMessage(cmd);
        pri++;
        break;
      }
    }
  }
}

/*!
 * Wakeup thread/message mbx for a message
 */
void ReturnMessage(IsoMessage* cmd) {
  if (!cmd->messagebox_to_reply) {
    if (cmd->thread_id == 0) {
      FreeVAGCommand((VagCommand*)cmd);
    } else {
      WakeupThread(cmd->thread_id);
    }
  } else {
    SendMbx(cmd->messagebox_to_reply, (MsgPacket*)cmd);
  }
}

/*!
 * Free buffers, close files, and remove from priority stack
 */
void ReleaseMessage(IsoMessage* cmd) {
  // kill all buffers
  while (cmd->callback_buffer) {
    auto old_head = cmd->callback_buffer;
    cmd->callback_buffer = (IsoBufferHeader*)old_head->next;
    FreeBuffer(old_head);
  }

  // close file
  if (cmd->fd) {
    isofs->close(cmd->fd);
  }

  // unqueue message
  UnqueueMessage(cmd);
}

// GetVAGCommand
VagCommand* GetVAGCommand() {
  for (;;) {
    // wait for command to be available
    while (vag_cmd_cnt == (N_VAG_CMDS - 1)) {
      DelayThread(100);
    }

    // wait for VAG semaphore
    while (WaitSema(sSema)) {
    }

    // try to get something.
    for (s32 i = 0; i < N_VAG_CMDS; i++) {
      if (!((vag_cmd_used >> (i & 0x1f)) & 1)) {
        // free!
        vag_cmd_used |= (1 << (i & 0x1f));
        vag_cmd_cnt++;
        if (vag_cmd_cnt > max_vag_cmd_cnt) {
          max_vag_cmd_cnt = vag_cmd_cnt;
        }
        SignalSema(sSema);
        return &vag_cmds[i];
      }
    }

    SignalSema(sSema);
  }
}

void FreeVAGCommand(VagCommand* cmd) {
  s32 idx = cmd - vag_cmds;
  if (idx >= 0 && idx < N_VAG_CMDS && ((vag_cmd_used >> (idx & 0x1f)) & 1)) {
    while (WaitSema(sSema)) {
    }

    vag_cmd_used &= ~(1 << (idx & 0x1f));
    vag_cmd_cnt--;
    SignalSema(sSema);
  } else {
    printf("[OVERLORD] Invalid FreeVAGCommand!\n");
  }
}
}  // namespace jak1
