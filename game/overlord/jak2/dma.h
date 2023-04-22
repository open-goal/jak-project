#pragma once

#include "common/common_types.h"

namespace jak2 {
struct VagCmd;
void dma_init_globals();
bool DMA_SendToSPUAndSync(uint8_t* iop_mem,
                          int size_one_side,
                          int spu_addr,
                          VagCmd* cmd,
                          int disable_intr);
}  // namespace jak2