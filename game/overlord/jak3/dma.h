#pragma once

#include "common/common_types.h"

namespace jak3 {
void jak3_overlord_init_globals_dma();

int voice_trans_wrapper(s16 chan, u32 mode, const void* iop_addr, u32 spu_addr, u32 size);
struct ISO_VAGCommand;

struct DmaQueueEntry {
  ISO_VAGCommand* command = nullptr;
  const void* iop_mem =nullptr;
  u32 spu_addr = 0;
  u32 length = 0;
  void* user_data = nullptr;
  u32 num_isobuffered_chunks = 0;
};
}