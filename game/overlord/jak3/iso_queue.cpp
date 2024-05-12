#include "iso_queue.h"

#include "common/log/log.h"
#include "common/util/Assert.h"

#include "game/overlord/jak3/pagemanager.h"
#include "game/overlord/jak3/util.h"
#include "game/sce/iop.h"

namespace jak3 {
using namespace iop;

constexpr int N_VAG_CMDS = 6;

u32 g_auStreamSRAM[N_VAG_CMDS];
u32 g_auTrapSRAM[N_VAG_CMDS];
int g_nPriQueueSema;
int g_nISOSema;

PriStackEntry gPriStack[N_PRIORITIES];

CPageManager g_PageManager;

void InitBuffers() {
  SemaParam semap;

  semap.init_count = 1;
  semap.max_count = 1;
  semap.attr = 0;
  semap.option = 0;
  g_nPriQueueSema = CreateSema(&semap);

  g_PageManager.Initialize();

  semap.init_count = 1;
  semap.max_count = 1;
  semap.attr = SA_THPRI;
  semap.option = 0;
  g_nISOSema = CreateSema(&semap);
}

bool QueueMessage(ISO_Msg* msg, int priority) {
  msg->m_nStatus = 2;
  msg->priority = priority;

  int level = priority == 5;

  {
    ScopedLock l(g_nPriQueueSema);

    if (gPriStack[level].count != 8) {
      gPriStack[level].entries[gPriStack[level].count] = msg;
      gPriStack[level].count++;

      return true;
    }
  }

  msg->m_nStatus = 4;
  ReturnMessage(msg);

  return false;
}

void UnqueueMessage(ISO_Msg* msg) {}

void ReturnMessage(ISO_Msg* msg) {
  if (msg->mbx_to_reply) {
    SendMbx(msg->mbx_to_reply, msg);
  } else if (msg->thread_id) {
    WakeupThread(msg->thread_id);
  } else {
    // FreeVAGCommand(msg);
    ASSERT_NOT_REACHED_MSG("unimplemented");
  }
}

}  // namespace jak3
