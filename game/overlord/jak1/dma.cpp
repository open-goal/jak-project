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

namespace jak1 {
u32 strobe;  // ?? mysterious sound DMA flag.

void dma_init_globals() {
  strobe = 0;
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
}  // namespace jak1