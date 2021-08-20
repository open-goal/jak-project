#pragma once

#include <vector>

#include "common/common_types.h"
#include "game/graphics/dma/dma_chain_read.h"

struct DmaData {
  u32 start_offset = 0;
  std::vector<u8> data;
};

/*!
 * The fixed chunk copier considers the game's main memory as an array of fixed sized chunks.
 * Only the chunks that have dma data are included in the copy.
 * The result is cached internally.
 */
class FixedChunkDmaCopier {
 public:
  static constexpr u32 chunk_size = 0x20000;  // 128 kB, gives use 1024 chunks for a 128 MB RAM.
  FixedChunkDmaCopier(u32 main_memory_size);

  const DmaData& run(const void* memory, u32 offset, bool verify = false);
  const DmaData& get_last_result() const { return m_result; }

 private:
  struct Fixup {
    u32 source_chunk;
    u32 offset_in_source_chunk;
    u32 dest_chunk;
    u32 offset_in_dest_chunk;
  };
  std::vector<Fixup> m_fixups;

  u32 m_main_memory_size = 0;
  u32 m_chunk_count = 0;
  std::vector<u32> m_chunk_mask;
  DmaData m_result;
};

/*!
 * Convert a DMA chain to an array of bytes that can be directly fed to VIF.
 */
std::vector<u8> flatten_dma(const DmaFollower& in);