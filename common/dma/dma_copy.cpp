
#include "dma_copy.h"

#include "common/dma/dma_chain_read.h"
#include "common/goal_constants.h"
#include "common/log/log.h"
#include "common/util/Timer.h"

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

void diff_dma_chains(DmaFollower ref, DmaFollower dma) {
  while (!ref.ended() && !dma.ended()) {
    auto ref_tag = ref.current_tag();
    auto dma_tag = dma.current_tag();
    if (ref_tag.kind != dma_tag.kind) {
      lg::warn("Bad dma tag kinds");
    }

    if (ref_tag.qwc != dma_tag.qwc) {
      lg::warn("Bad dma tag qwc: {} {}", ref_tag.qwc, dma_tag.qwc);
    }

    auto ref_result = ref.read_and_advance();
    auto dma_result = dma.read_and_advance();

    for (int i = 0; i < (int)ref_result.size_bytes; i++) {
      if (ref_result.data[i] != dma_result.data[i]) {
        lg::error("Bad data ({} vs {}) at {} into transfer: {} {}", ref_result.data[i],
                  dma_result.data[i], i, ref_tag.print(), dma_tag.print());
        return;
      }
    }
  }

  if (!ref.ended()) {
    lg::warn("dma ended early");
  }

  if (!dma.ended()) {
    lg::warn("dma had extra data");
  }
}

void FixedChunkDmaCopier::serialize_last_result(Serializer& serializer) {
  serializer.from_ptr(&m_result.start_offset);
  serializer.from_pod_vector(&m_result.data);
}

FixedChunkDmaCopier::FixedChunkDmaCopier(u32 main_memory_size)
    : m_main_memory_size(main_memory_size), m_chunk_count(main_memory_size / chunk_size) {
  ASSERT(chunk_size * m_chunk_count == m_main_memory_size);  // make sure the memory size is valid.
  ASSERT(chunk_size >= 16);
  m_chunk_mask.resize(m_chunk_count);
}

void FixedChunkDmaCopier::set_input_data(const void* memory, u32 offset, bool run_copy) {
  if (run_copy) {
    run(memory, offset, false);
  } else {
    m_input_offset = offset;
    m_input_data = memory;
  }
}

const DmaData& FixedChunkDmaCopier::run(const void* memory, u32 offset, bool verify) {
  Timer timer;
  m_input_offset = offset;
  m_input_data = memory;
  std::fill(m_chunk_mask.begin(), m_chunk_mask.end(), false);
  m_fixups.clear();
  m_result.data.clear();
  m_result.stats = DmaStats();
  m_result.start_offset = 0;

  DmaFollower dma(memory, offset);
  while (!dma.ended()) {
    auto tag_offset = dma.current_tag_offset();
    auto tag = dma.current_tag();
    m_result.stats.num_tags++;

    // first, make sure we get this tag:
    u32 tag_chunk_idx = tag_offset / chunk_size;
    u32 tag_offset_in_chunk = tag_offset % chunk_size;
    m_chunk_mask.at(tag_chunk_idx) = true;

    if (tag.addr) {
      ASSERT(tag.addr > EE_MAIN_MEM_LOW_PROTECT);
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
      m_result.stats.num_data_bytes += transfer.size_bytes;
      u32 initial_chunk = transfer.data_offset / chunk_size;
      u32 end_addr = transfer.data_offset + transfer.size_bytes;
      m_chunk_mask.at(initial_chunk) = true;
      u32 chunk = initial_chunk + 1;
      u32 current_address = chunk_size * chunk;
      while (current_address < end_addr) {
        current_address += chunk_size;
        m_chunk_mask.at(chunk++) = true;
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
  m_result.stats.num_chunks = current_out_chunk;
  m_result.stats.num_copied_bytes = m_result.data.size();
  m_result.stats.num_fixups = m_fixups.size();

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
      lg::error("Verification has failed.");
      lg::error("size diff: {} {}", ref.size(), v2.size());

      for (size_t i = 0; i < std::min(ref.size(), v2.size()); i++) {
        if (ref[i] != v2[i]) {
          lg::error("first diff at {}", i);
          break;
        }
      }
      diff_dma_chains(DmaFollower(memory, offset),
                      DmaFollower(m_result.data.data(), m_result.start_offset));
      ASSERT(false);
    } else {
      lg::debug("verification ok: {} bytes", ref.size());
    }
  }

  m_result.stats.sync_time_ms = timer.getMs();
  return m_result;
}
