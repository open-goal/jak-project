#include "iso_queue.h"

#include "game/overlord/jak3/pagemanager.h"
#include "game/sce/iop.h"

namespace jak3 {
using namespace iop;

constexpr int N_VAG_CMDS = 6;

u32 g_auStreamSRAM[N_VAG_CMDS];
u32 g_auTrapSRAM[N_VAG_CMDS];
int g_nPriQueueSema;
int g_nISOSema;

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
}  // namespace jak3
