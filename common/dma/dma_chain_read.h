#pragma once

#include <cstring>

#include "common/dma/dma.h"
#include "common/util/Assert.h"

/*!
 * @file dma_chain_read.h
 * This file contains utilities for reading/following a DMA chain.
 *
 * This allows you to iterate through transfers like this:
 *
 * DmaFollower dma(mem, start_addr);
 * while (!reader.ended) {
 *   DmaTransfer transfer = reader.advance_to_next_transfer();
 *   // do something with the data in transfer.
 * }
 */

/*!
 * Represents a DMA transfer, including 64-bits of VIF tag.
 */
struct DmaTransfer {
  const u8* data = nullptr;
  u32 data_offset = 0;
  u32 size_bytes = 0;
  u64 transferred_tag = 0;

  u32 vif0() const { return transferred_tag & 0xffffffff; }
  u32 vif1() const { return (transferred_tag >> 32) & 0xffffffff; }

  VifCode vifcode0() const { return VifCode(vif0()); }
  VifCode vifcode1() const { return VifCode(vif1()); }

  template <typename T>
  T read_val(u32 offset) const {
    T result;
    memcpy(&result, (const u8*)data + offset, sizeof(T));
    return result;
  }
};

class DmaFollower {
 public:
  DmaFollower() { m_ended = true; }
  DmaFollower(const void* data, u32 start_offset) : m_base(data), m_tag_offset(start_offset) {}
  template <typename T>
  T read_val(u32 offset) const {
    T result;
    memcpy(&result, (const u8*)m_base + offset, sizeof(T));
    return result;
  }

  /*!
   * Read the current tag, return its transfer, then advance to the next.
   */
  DmaTransfer read_and_advance() {
    DmaTag tag(read_val<u64>(m_tag_offset));
    DmaTransfer result;
    result.transferred_tag = read_val<u64>(m_tag_offset + 8);
    result.size_bytes = (u32)tag.qwc * 16;
    ASSERT(!tag.spr);
    ASSERT(!m_ended);
    switch (tag.kind) {
      case DmaTag::Kind::CNT:
        // data, then next tag. doesn't read address.
        ASSERT(tag.addr == 0);
        result.data_offset = m_tag_offset + 16;
        m_tag_offset = result.data_offset + result.size_bytes;
        break;
      case DmaTag::Kind::NEXT:
        result.data_offset = m_tag_offset + 16;
        m_tag_offset = tag.addr;
        break;
      case DmaTag::Kind::REF:
      case DmaTag::Kind::REFS:
        result.data_offset = tag.addr;
        m_tag_offset = m_tag_offset + 16;
        break;
      case DmaTag::Kind::REFE:
        result.data_offset = tag.addr;
        m_tag_offset = m_tag_offset + 16;
        m_ended = true;
        break;
      case DmaTag::Kind::CALL:
        result.data_offset = m_tag_offset + 16;
        ASSERT(m_sp <= 1);
        m_tag_offset = tag.addr;
        m_stack[m_sp++] = result.data_offset + tag.qwc * 16;
        break;
      case DmaTag::Kind::RET:
        ASSERT(m_sp > 0);
        result.data_offset = m_tag_offset + 16;
        m_tag_offset = m_stack[--m_sp];
        break;
      case DmaTag::Kind::END:
        result.data_offset = m_tag_offset + 16;
        m_ended = true;
        break;

      default:
        ASSERT(false);
    }
    result.data = (const u8*)m_base + result.data_offset;
    return result;
  }

  DmaTransfer advance_and_print_dma(DmaFollower& dma) {
    auto data = dma.read_and_advance();
    printf(
        "dma transfer:\n%ssize: %d\nvif0: %s, data: %d\nvif1: %s, data: %d, imm: "
        "%d\n\n",
        dma.current_tag().print().c_str(), data.size_bytes, data.vifcode0().print().c_str(),
        data.vif0(), data.vifcode1().print().c_str(), data.vifcode1().num,
        data.vifcode1().immediate);
    return data;
  }

  DmaTag current_tag() const { return DmaTag(read_val<u64>(m_tag_offset)); }
  u32 current_tag_vif0() const { return read_val<u32>(m_tag_offset + 8); }
  u32 current_tag_vif1() const { return read_val<u32>(m_tag_offset + 12); }
  VifCode current_tag_vifcode0() const { return VifCode(current_tag_vif0()); }
  VifCode current_tag_vifcode1() const { return VifCode(current_tag_vif1()); }
  u32 current_tag_offset() const { return m_tag_offset; }
  bool ended() const { return m_ended; }

 private:
  const void* m_base = nullptr;
  u32 m_tag_offset = 0;
  s32 m_sp = 0;
  s32 m_stack[2] = {-1, -1};
  bool m_ended = false;
};
