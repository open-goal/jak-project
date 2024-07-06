#include "vblank_handler.h"

#include <cstring>

#include "common/log/log.h"
#include "common/util/Assert.h"

#include "game/overlord/jak3/dma.h"
#include "game/overlord/jak3/iso.h"
#include "game/overlord/jak3/sbank.h"
#include "game/overlord/jak3/srpc.h"
#include "game/overlord/jak3/ssound.h"
#include "game/overlord/jak3/streamlist.h"
#include "game/overlord/jak3/vag.h"
#include "game/sce/iop.h"

namespace jak3 {
using namespace iop;
u32 g_nInfoEE = 0;
SoundIOPInfo g_SRPCSoundIOPInfo;
bool g_bVBlankInitialized = false;
s32 g_nVBlankThreadID = -1;
s32 g_nVBlankSemaphoreID = -1;
bool g_bVBlankRegistered = false;
u32 g_nIopTicks = 0;
u32 g_nFrameNum = 0;
void jak3_overlord_init_globals_vblank_handler() {
  g_nInfoEE = 0;
  g_SRPCSoundIOPInfo = {};
  g_bVBlankInitialized = false;
  g_nVBlankThreadID = -1;
  g_nVBlankSemaphoreID = -1;
  g_bVBlankRegistered = false;
  g_nIopTicks = 0;
  g_nFrameNum = 0;
}

int VBlankHandler(void*);
u32 VBlankThread();

void VBlank_Initialize() {
  ThreadParam thread_param;
  SemaParam sema_param;

  if (g_bVBlankInitialized == 0) {
    thread_param.attr = 0x2000000;
    thread_param.stackSize = 0x800;
    thread_param.initPriority = 0x34;
    thread_param.option = 0;
    thread_param.entry = VBlankThread;
    strcpy(thread_param.name, "vblank");
    g_nVBlankThreadID = CreateThread(&thread_param);
    ASSERT(g_nVBlankThreadID >= 0);
    sema_param.max_count = 200;  // hack
    sema_param.attr = 0;
    sema_param.init_count = 0;
    sema_param.option = 0;
    g_nVBlankSemaphoreID = CreateSema(&sema_param);
    ASSERT(g_nVBlankSemaphoreID >= 0);
    int ret = StartThread(g_nVBlankThreadID, 0);
    ASSERT(ret == 0);
    RegisterVblankHandler(0, 0x40, VBlankHandler, 0);
    g_bVBlankInitialized = true;
    g_bVBlankRegistered = true;
  }
}

int VBlankHandler(void*) {
  if ((g_bVBlankInitialized != 0) && (-1 < g_nVBlankSemaphoreID)) {
    SignalSema(g_nVBlankSemaphoreID);  // was iSignalSema
  }
  return 1;
}

u32 VBlankThread() {
  //  char *pcVar1;
  //  int iVar2;
  //  uint uVar3;
  //  SoundBankInfo *pSVar4;
  //  ISO_VAGCommand *cmd;
  //  SoundBankInfo **ppSVar5;
  //  uint *puVar6;
  //  uint uVar7;
  //  int iVar8;
  //  int iVar9;
  //  SoundIOPInfo *local_30;
  //  void *local_2c;
  //  undefined4 local_28;
  //  undefined4 local_24;
  //  undefined4 local_20 [2];

  do {
    while ((g_bVBlankInitialized == 0 || (g_nVBlankSemaphoreID < 0))) {
      DelayThread(1000000);
    }
    WaitSema(g_nVBlankSemaphoreID);
    g_nIopTicks = g_nIopTicks + 1;
    if (g_bSoundEnable != 0) {
      CheckVagStreamsProgress();
      if ((g_nIopTicks & 1U) != 0) {
        StreamListThread();
      }
      if (g_nMusicFadeDir < 0) {
        g_nMusicFade = g_nMusicFade + -0x200;
        if (g_nMusicFade < 0) {
          g_nMusicFade = 0;
        LAB_00011f60:
          g_nMusicFadeDir = 0;
        }
      } else {
        if ((0 < g_nMusicFadeDir) &&
            (g_nMusicFade = g_nMusicFade + 0x400, 0x10000 < g_nMusicFade)) {
          g_nMusicFade = 0x10000;
          goto LAB_00011f60;
        }
      }
      if (g_nInfoEE) {
        g_nFrameNum = g_nFrameNum + 1;
        // puVar6 = g_SRPCSoundIOPInfo.stream_status;
        for (int i = 0; i < 4; i++) {
          auto* cmd = &g_aVagCmds[i];
          u32 stream_status = cmd->pack_flags();

          if ((cmd->flags.file_disappeared != 0) && (cmd->flags.paused == 0)) {
            auto uVar3 = CalculateVAGPitch(0x400, cmd->pitch_cmd);
            if (g_nFPS == 0) {
              ASSERT_NOT_REACHED();
            }
            cmd->clockd = cmd->clockd + uVar3 / g_nFPS;
          }

          if ((cmd->flags.saw_chunks1 == 0) && (cmd->flags.clocks_set != 0)) {
            g_SRPCSoundIOPInfo.stream_status[i] = stream_status;
            g_SRPCSoundIOPInfo.stream_id[i] = cmd->id;
            g_SRPCSoundIOPInfo.stream_position[i] = 0;
          } else {
            g_SRPCSoundIOPInfo.stream_status[i] = stream_status;
            g_SRPCSoundIOPInfo.stream_id[i] = cmd->id;
            g_SRPCSoundIOPInfo.stream_position[i] = cmd->position_for_ee;
          }
        }
        // CpuSuspendIntr(local_20);

        // CpuResumeIntr(local_20[0]);
        g_SRPCSoundIOPInfo.iop_ticks = g_nIopTicks;
        g_SRPCSoundIOPInfo.freemem = 12345;  // hack
        g_SRPCSoundIOPInfo.frame = g_nFrameNum;
        g_SRPCSoundIOPInfo.freemem2 = QueryTotalFreeMemSize();
        g_SRPCSoundIOPInfo.nocd = 0;     // hack
        g_SRPCSoundIOPInfo.dirtycd = 0;  // hack
        g_SRPCSoundIOPInfo.dupseg = -1;
        g_SRPCSoundIOPInfo.diskspeed[0] = 0;
        g_SRPCSoundIOPInfo.diskspeed[1] = 0;
        g_SRPCSoundIOPInfo.lastspeed = 0;

        memset(&g_SRPCSoundIOPInfo.sound_bank0[0], 0, 8 * 16);

        if (gBanks[0]->in_use && gBanks[0]->loaded) {
          strcpy(g_SRPCSoundIOPInfo.sound_bank0, gBanks[0]->m_name1);
        }
        if (gBanks[1]->in_use && gBanks[1]->loaded) {
          strcpy(g_SRPCSoundIOPInfo.sound_bank1, gBanks[1]->m_name1);
        }
        if (gBanks[2]->in_use && gBanks[2]->loaded) {
          strcpy(g_SRPCSoundIOPInfo.sound_bank2, gBanks[2]->m_name1);
        }
        if (gBanks[3]->in_use && gBanks[3]->loaded) {
          strcpy(g_SRPCSoundIOPInfo.sound_bank3, gBanks[3]->m_name1);
        }
        if (gBanks[4]->in_use && gBanks[4]->loaded) {
          strcpy(g_SRPCSoundIOPInfo.sound_bank4, gBanks[4]->m_name1);
        }
        if (gBanks[5]->in_use && gBanks[5]->loaded) {
          strcpy(g_SRPCSoundIOPInfo.sound_bank5, gBanks[5]->m_name1);
        }
        if (gBanks[6]->in_use && gBanks[6]->loaded) {
          strcpy(g_SRPCSoundIOPInfo.sound_bank6, gBanks[6]->m_name1);
        }
        if (gBanks[7]->in_use && gBanks[7]->loaded) {
          strcpy(g_SRPCSoundIOPInfo.sound_bank7, gBanks[7]->m_name1);
        }

        for (int i = 0; i < 48; i++) {
          g_SRPCSoundIOPInfo.chinfo[i] = (snd_GetVoiceStatus(i) != 1) - 1;
        }

        sceSifDmaData dma;
        dma.data = &g_SRPCSoundIOPInfo;
        dma.addr = (void*)(u64)g_nInfoEE;
        dma.size = sizeof(g_SRPCSoundIOPInfo);
        static_assert(sizeof(g_SRPCSoundIOPInfo) == 0x2d0);
        dma.mode = 0;
        /*dmaid =*/sceSifSetDma(&dma, 1);
      }
    }
    RunDeferredVoiceTrans();
    // Poll(&g_DvdDriver);
  } while (true);
}

}  // namespace jak3