/*!
 * @file dma.cpp
 * DMA Related functions for Overlord.
 * This code is not great.
 */

#include <cstring>
#include <cstdio>
#include "dma.h"
#include "common/common_types.h"
#include "game/sce/iop.h"
#include "game/overlord/989snd.h"
#include "game/sce/libsd.h"

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
u32 intr() {
  strobe = 1;
  return 0;
}

// TODO DMA_SendToSPUAndSync()
int DMA_SendToSPUAndSync(void* data, u32 size, void* dest) {
  int spu_dma = snd_GetFreeSPUDMA();
  if (spu_dma == 0xFFFFFFFF) {
    return 0;
  }
  strobe = 0;
  // sceSdSetTransIntrHandler(spu_dma, (sceSdTransIntrHandler)intr, 0);
  u32 size_ = (size + 0x3F) & 0xFFFFFFC0;
  int voiceTrans = 0;  // sceSdVoiceTrans(spu_dma, 0, (u8*)data, (u32*)dest, size_);
  do {
  } while (!strobe);
  // sceSdSetTransIntrHandler(spu_dma, 0, 0);
  snd_FreeSPUDMA(spu_dma);
  return voiceTrans >= size_;
}
