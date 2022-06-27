#pragma once

/*!
 * @file dmac.h
 * DMAC implementation for the "PS2 virtual machine".
 * Not meant to work as a full DMAC emulator, just enough to inspect DMA packets.
 */

#include "common/common_types.h"

#include "game/kernel/common/Ptr.h"

namespace VM {

/*!
 * DMA channel CHCR register. The only one we care about right now.
 */
struct DmaChcr {
  u32 dir : 1;
  u32 _pad1 : 1;
  u32 mod : 2;
  u32 asp : 2;
  u32 tte : 1;
  u32 tie : 1;
  u32 str : 1;
  u32 _pad2 : 7;
  u16 tag : 16;
};

/*!
 * Layout of the DMA channel registers in EE memory. For simplicity's sake, all are included,
 * however, each channel may not actually have some of these registers.
 * They are 16-byte aligned.
 */
struct alignas(16) DmaChannelRegisters {
  alignas(16) DmaChcr chcr;
  alignas(16) u32 madr;
  alignas(16) u32 qwc;
  alignas(16) u32 tadr;
  alignas(16) u32 asr0;
  alignas(16) u32 asr1;
  alignas(16) u128 _pad1;
  alignas(16) u128 _pad2;
  alignas(16) u32 sadr;
};

/*!
 * Layout of the DMAC registers in EE memory.
 * They are 16-byte aligned.
 */
struct alignas(16) DmaCommonRegisters {
  alignas(16) u32 ctrl;
  alignas(16) u32 stat;
  alignas(16) u32 pcr;
  alignas(16) u32 sqwc;
  alignas(16) u32 rbsr;
  alignas(16) u32 rbor;
  alignas(16) u32 stadr;
};

// pointer to DMAC registers
extern Ptr<DmaCommonRegisters> dmac;
// array of pointers to DMAC channels (they are not stored contiguously)
extern Ptr<DmaChannelRegisters> dmac_ch[10];

// enum DmaChannel { VIF0, VIF1, GIF, fromIPU, toIPU, SIF0, SIF1, SIF2, fromSPR, toSPR };

static_assert(sizeof(DmaChannelRegisters) == 0x90, "DmaChannelRegisters wrong size");
static_assert(alignof(DmaChannelRegisters) == 0x10, "DmaChannelRegisters unaligned");

void dmac_init_globals();

}  // namespace VM
