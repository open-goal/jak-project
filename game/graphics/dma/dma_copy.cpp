
#include "game/graphics/dma/dma_chain_read.h"
#include "dma_copy.h"
#include "third-party/fmt/core.h"

/*!
 * Convert a DMA chain to an array of bytes that can be directly fed to VIF.
 */
std::vector<u8> flatten_dma(const DmaFollower& in) {
  DmaFollower state = in;
  std::vector<u8> result;
  while (!state.ended()) {
    auto read_result = state.read_and_advance();

    // insert 1 quadword of zeros.
    // the first 8 bytes will remain zero and will be interpreted as VIF nop.
    for (int i = 0; i < 16; i++) {
      result.push_back(0);
    }

    // the second will contain the transferred tag.
    memcpy(result.data() + result.size() - 8, &read_result.transferred_tag, 8);

    // and the actual DMA data.
    result.insert(result.end(), read_result.data, read_result.data + read_result.size_bytes);
  }
  return result;
}

FixedChunkDmaCopier::FixedChunkDmaCopier(u32 main_memory_size)
    : m_main_memory_size(main_memory_size), m_chunk_count(main_memory_size / chunk_size) {
  assert(chunk_size * m_chunk_count == m_main_memory_size);  // make sure the memory size is valid.
  assert(chunk_size >= 16);
  m_chunk_mask.resize(m_chunk_count);
}

const DmaData& FixedChunkDmaCopier::run(const void* memory, u32 offset, bool verify) {
  std::fill(m_chunk_mask.begin(), m_chunk_mask.end(), false);
  m_fixups.clear();
  m_result.data.clear();
  m_result.start_offset = 0;

  DmaFollower dma(memory, offset);
  while (!dma.ended()) {
    auto tag_offset = dma.current_tag_offset();
    auto tag = dma.current_tag();

    // first, make sure we get this tag:
    u32 tag_chunk_idx = tag_offset / chunk_size;
    u32 tag_offset_in_chunk = tag_offset % chunk_size;
    m_chunk_mask.at(tag_chunk_idx) = true;

    if (tag.addr) {
      u32 addr_chunk_idx = tag.addr / chunk_size;
      u32 addr_offset_in_chunk = tag.addr % chunk_size;
      // next, make sure that we get the address (if applicable)
      m_chunk_mask.at(addr_chunk_idx) = true;
      // and add a fixup
      Fixup fu;
      fu.source_chunk = tag_chunk_idx;
      fu.offset_in_source_chunk = tag_offset_in_chunk;
      fu.dest_chunk = addr_chunk_idx;
      fu.offset_in_dest_chunk = addr_offset_in_chunk;
      m_fixups.push_back(fu);
    }

    auto transfer = dma.read_and_advance();
    if (transfer.size_bytes) {
      u32 initial_chunk = transfer.data_offset / chunk_size;
      m_chunk_mask.at(initial_chunk) = true;
      s32 bytes_remaining = transfer.size_bytes;
      bytes_remaining -= chunk_size - (transfer.size_bytes % chunk_size);
      u32 chunk = initial_chunk + 1;
      while (bytes_remaining >= 0) {
        bytes_remaining -= transfer.size_bytes;
        m_chunk_mask.at(chunk) = true;
      }
    }
  }

  // assign output chunks.
  u32 current_out_chunk = 0;
  for (auto& val : m_chunk_mask) {
    if (val) {
      val = current_out_chunk++;
    } else {
      val = UINT32_MAX;
    }
  }

  m_result.data.resize(current_out_chunk * chunk_size);

  // copy
  for (u32 chunk_idx = 0; chunk_idx < m_chunk_mask.size(); chunk_idx++) {
    u32 dest_idx = m_chunk_mask[chunk_idx];
    if (dest_idx != UINT32_MAX) {
      memcpy(m_result.data.data() + (dest_idx * chunk_size),
             (const u8*)memory + (chunk_idx * chunk_size), chunk_size);
    }
  }

  // fix up!
  for (const auto& fu : m_fixups) {
    u32 tag_addr = m_chunk_mask.at(fu.source_chunk) * chunk_size + fu.offset_in_source_chunk + 4;
    u32 dest_addr = m_chunk_mask.at(fu.dest_chunk) * chunk_size + fu.offset_in_dest_chunk;
    memcpy(m_result.data.data() + tag_addr, &dest_addr, 4);
  }

  // setup final offset
  m_result.start_offset = m_chunk_mask.at(offset / chunk_size) * chunk_size + (offset % chunk_size);

  if (verify) {
    auto ref = flatten_dma(DmaFollower(memory, offset));
    auto v2 = flatten_dma(DmaFollower(m_result.data.data(), m_result.start_offset));
    if (ref != v2) {
      fmt::print("Verification has failed.\n");
    } else {
      fmt::print("verification ok: {} bytes\n", ref.size());
    }
  }

  return m_result;
}