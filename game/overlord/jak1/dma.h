#pragma once

/*!
 * @file dma.h
 * DMA Related functions for Overlord.
 * This code is not great.
 */

#include "common/common_types.h"

namespace jak1 {
bool DMA_SendToSPUAndSync(void* src_addr, u32 size, u32 dst_addr);
void dma_init_globals();

}  // namespace jak1
