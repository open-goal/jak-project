#pragma once

#include "common/common_types.h"

namespace jak3 {
void jak3_overlord_init_globals_dma();
struct ISO_VAGCommand;

int voice_trans_wrapper(s16 chan, u32 mode, const void* iop_addr, u32 spu_addr, u32 size);
void DMA_SendToEE(void* ee_dest,
                  const void* iop_src,
                  u32 length,
                  void callback(void*),
                  void* callback_arg);
int DMA_SendToSPUAndSync(const u8* iop_mem,
                         int length,
                         int spu_addr,
                         ISO_VAGCommand* cmd,
                         void* user_data);
void RunDeferredVoiceTrans();
struct ISO_VAGCommand;

struct DmaQueueEntry {
  ISO_VAGCommand* command = nullptr;
  const void* iop_mem = nullptr;
  u32 spu_addr = 0;
  u32 length = 0;
  void* user_data = nullptr;
  u32 num_isobuffered_chunks = 0;
};
void dma_intr_hack();
}  // namespace jak3