#include "dma.h"

#include "game/sce/iop.h"

using namespace iop;

// note that jak 1 and 2 have different implementations, but we make them both instant.
// jak 2 has an EE dma semaphore, but we're going to ignore that.

/*!
 * Wait for an ongoing DMA transfer to finish.
 * IOP DMAs are instant in this version, so we return immediately and clear dmaid.
 */
void DMA_Sync() {}

/*!
 * Start DMA transfer to the EE.
 */
void DMA_SendToEE(void* data, u32 size, void* dest) {
  // finish previous DMA
  DMA_Sync();

  sceSifDmaData cmd;  // DMA settings

  // setup command
  cmd.mode = 0;
  cmd.data = data;
  cmd.addr = dest;
  cmd.size = size;

  // start DMA (with disabled interrupts)
  CpuDisableIntr();
  sceSifSetDma(&cmd, 1);
  CpuEnableIntr();
}