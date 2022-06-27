/*!
 * @file dma.cpp
 * DMA Related functions for Overlord.
 * This code is not great.
 */

#include "dma.h"

#include <cstdio>
#include <cstring>

#include "common/common_types.h"

#include "game/sce/iop.h"
#include "game/sound/sdshim.h"
#include "game/sound/sndshim.h"

using namespace iop;

u32 dmaid;          // ID of in-progress DMA. 0 if no DMA in progress
sceSifDmaData cmd;  // DMA settings
u32 strobe;         // ?? mysterious sound DMA flag.

void dma_init_globals() {
  dmaid = 0;
  memset(&cmd, 0, sizeof(cmd));
  strobe = 0;
}

/*!
 * Wait for an ongoing DMA transfer to finish.
 * IOP DMAs are instant in this version, so we return immediately and clear dmaid.
 */
void DMA_Sync() {
  // The DMA is complete. Clear dmaid.
  dmaid = 0;

  // for fun, the original code
  //  if(dmaid != 0) {
  //    if(sceSifDmaStat(dmaid) > 0) {
  //      u32 count = 10000;
  //      while(sceSifDmaStat(dmaid) > 0) {
  //        DelayThread(10);
  //        count--;
  //        if(count == 0) {
  //          u32 count = 10000;
  //        }
  //      }
  //    }
  //
  //    // better do that again, just to be sure i did it the first time.
  //    u32 count = 10000;
  //    while(sceSifDmaStat(dmaid) > 0) {
  //      DelayThread(10);
  //      count--;
  //      if(count == 0) {
  //        u32 count = 10000;
  //      }
  //    }
  //    dmaid = 0;
  //  }
}

/*!
 * Start DMA transfer to the EE.
 */
void DMA_SendToEE(void* data, u32 size, void* dest) {
  // finish previous DMA
  DMA_Sync();

  // setup command
  cmd.mode = 0;
  cmd.data = data;
  cmd.addr = dest;
  cmd.size = size;

  // start DMA (with disabled interrupts)
  CpuDisableIntr();
  dmaid = sceSifSetDma(&cmd, 1);
  CpuEnableIntr();

  if (dmaid == 0) {
    do {
      printf("Got a bad DMA ID!\n");  // added
    } while (true);
  }
}

/*!
 * SPU DMA interrupt handler.

 */
s32 intr(s32 /*channel*/, void* /*userdata*/) {
  strobe = 1;
  return 0;
}

bool DMA_SendToSPUAndSync(void* src_addr, u32 size, u32 dst_addr) {
  s32 channel = snd_GetFreeSPUDMA();
  if (channel == -1)
    return false;
  strobe = 0;
  sceSdSetTransIntrHandler(channel, intr, nullptr);
  // Skip this, we end up memcpy's from OOB (which trips asan)
  // u32 size_aligned = (size + 63) & 0xFFFFFFF0;
  u32 size_aligned = size;
  u32 transferred = sceSdVoiceTrans(channel, 0, src_addr, dst_addr, size_aligned);
  while (!strobe)
    ;
  sceSdSetTransIntrHandler(channel, nullptr, nullptr);
  snd_FreeSPUDMA(channel);
  return transferred >= size_aligned;
}
