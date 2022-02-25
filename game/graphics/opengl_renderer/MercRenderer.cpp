#include "MercRenderer.h"

#include "third-party/imgui/imgui.h"

MercRenderer::MercRenderer(const std::string& name, BucketId my_id)
    : BucketRenderer(name, my_id),
      m_direct(fmt::format("{}-dir", name), my_id, 0x30000, DirectRenderer::Mode::NORMAL) {
  memset(m_buffer.data, 0, sizeof(m_buffer.data));
}

void MercRenderer::render(DmaFollower& dma,
                          SharedRenderState* render_state,
                          ScopedProfilerNode& prof) {
  m_stats = Stats();

  // skip if disabled
  if (!m_enabled) {
    while (dma.current_tag_offset() != render_state->next_bucket) {
      dma.read_and_advance();
    }
    return;
  }

  // process the first tag. this is just jumping to the merc-specific dma.
  auto data0 = dma.read_and_advance();
  ASSERT(data0.vif1() == 0);
  ASSERT(data0.vif0() == 0);
  ASSERT(data0.size_bytes == 0);
  if (dma.current_tag().kind == DmaTag::Kind::CALL) {
    // renderer didn't run, let's just get out of here.
    for (int i = 0; i < 4; i++) {
      dma.read_and_advance();
    }
    ASSERT(dma.current_tag_offset() == render_state->next_bucket);
    return;
  }
  m_stats.had_data = true;
  ASSERT(data0.size_bytes == 0);
  ASSERT(data0.vif0() == 0);
  ASSERT(data0.vif1() == 0);

  // if we reach here, there's stuff to draw
  handle_setup(dma, render_state, prof);

  m_direct.reset_state();
  while (dma.current_tag_offset() != render_state->next_bucket) {
    handle_merc_chain(dma, render_state, prof);
  }
  ASSERT(dma.current_tag_offset() == render_state->next_bucket);
  m_direct.flush_pending(render_state, prof);
}

bool tag_is_nothing_next(const DmaFollower& dma) {
  return dma.current_tag().kind == DmaTag::Kind::NEXT && dma.current_tag().qwc == 0 &&
         dma.current_tag_vif0() == 0 && dma.current_tag_vif1() == 0;
}

void MercRenderer::unpack32(const VifCodeUnpack& up, const u8* data, u32 imm) {
  ASSERT(!up.is_unsigned);
  u32 addr = up.addr_qw;
  ASSERT(imm != 0);
  ASSERT(!m_vif.stmod);
  if (up.use_tops_flag) {
    addr += xitop();
  }

  u32 start_in_buff = (addr)*16;
  u32 end_in_buff = start_in_buff + imm * 16;
  ASSERT(start_in_buff < sizeof(m_buffer.data));
  ASSERT(end_in_buff <= sizeof(m_buffer.data));
  memcpy(m_buffer.data + start_in_buff, data, imm * 16);
}

void MercRenderer::unpack8(const VifCodeUnpack& up, const u8* data, u32 imm) {
  // ASSERT(m_vif.stmod);

  ASSERT(up.is_unsigned);
  u32 addr = up.addr_qw;
  if (up.use_tops_flag) {
    addr += xitop();
  }
  ASSERT(imm != 0);

  u32 start_in_buff = (addr)*16;
  u32 end_in_buff = start_in_buff + imm * 16;
  ASSERT(start_in_buff < sizeof(m_buffer.data));
  ASSERT(end_in_buff <= sizeof(m_buffer.data));

  u8* out_ptr = m_buffer.data + start_in_buff;

  if (m_vif.stmod) {
    // use row
    auto row = _mm_loadu_si128((const __m128i*)m_vif.row);
    for (u32 qw = 0; qw < imm; qw++) {
      _mm_storeu_si128((__m128i*)out_ptr,
                       _mm_add_epi32(row, _mm_cvtepu8_epi32(_mm_loadu_si64(data))));
      data += 4;
      out_ptr += 16;
    }
  } else {
    // no row
    for (u32 qw = 0; qw < imm; qw++) {
      _mm_storeu_si128((__m128i*)out_ptr, _mm_cvtepu8_epi32(_mm_loadu_si64(data)));
      data += 4;
      out_ptr += 16;
    }
  }

  /*
  u32 row[4];
  if (m_vif.stmod) {
    memcpy(row, m_vif.row, 16);
  } else {
    memset(row, 0, 16);
  }

  u32 temp[4];
  for (u32 i = 0; i < imm; i++) {
    for (u32 j = 0; j < 4; j++) {
      temp[j] = row[j] + data[4 * i + j];
    }
    memcpy(m_buffer.data + start_in_buff + i * 16, temp, 16);
  }
   */
}

void MercRenderer::handle_merc_chain(DmaFollower& dma,
                                     SharedRenderState* render_state,
                                     ScopedProfilerNode& prof) {
  //  fmt::print("DMA: {}\n", dma.current_tag().print());
  while (tag_is_nothing_next(dma)) {
    auto nothing = dma.read_and_advance();
    ASSERT(nothing.size_bytes == 0);
  }
  if (dma.current_tag().kind == DmaTag::Kind::CALL) {
    for (int i = 0; i < 4; i++) {
      dma.read_and_advance();
    }
    return;
  }

  auto init = dma.read_and_advance();

  ASSERT(init.vifcode0().kind == VifCode::Kind::STROW);
  ASSERT(init.size_bytes == 16);
  m_vif.row[0] = init.vif1();
  memcpy(m_vif.row + 1, init.data, 12);
  u32 extra;
  memcpy(&extra, init.data + 12, 4);
  ASSERT(extra == 0);
  DmaTransfer next;

  bool setting_up = true;
  u32 mscal_addr = -1;
  while (setting_up) {
    next = dma.read_and_advance();
    u32 offset_in_data = 0;
    //    fmt::print("START {} : {} {}\n", next.size_bytes, next.vifcode0().print(),
    //               next.vifcode1().print());
    auto vif0 = next.vifcode0();
    switch (vif0.kind) {
      case VifCode::Kind::NOP:
      case VifCode::Kind::FLUSHE:
        break;
      case VifCode::Kind::STMOD:
        ASSERT(vif0.immediate == 0 || vif0.immediate == 1);
        m_vif.stmod = vif0.immediate;
        break;
      default:
        ASSERT(false);
    }

    auto vif1 = next.vifcode1();
    switch (vif1.kind) {
      case VifCode::Kind::UNPACK_V4_8: {
        // todo unpack
        m_stats.unpack_count++;
        m_stats.unpack_bytes += vif1.num * 4;
        VifCodeUnpack up(vif1);
        unpack8(up, next.data, vif1.num);
        offset_in_data += 4 * vif1.num;
      } break;
      case VifCode::Kind::UNPACK_V4_32: {
        // todo unpack
        VifCodeUnpack up(vif1);
        unpack32(up, next.data, vif1.num);
        m_stats.unpack_bytes += vif1.num * 16;
        offset_in_data += 16 * vif1.num;
      } break;
      case VifCode::Kind::MSCAL:
        mscal_addr = vif1.immediate;
        ASSERT(next.size_bytes == 0);
        setting_up = false;
        break;
      default:
        ASSERT(false);
    }

    ASSERT(offset_in_data <= next.size_bytes);
    if (offset_in_data < next.size_bytes) {
      ASSERT((offset_in_data % 4) == 0);
      u32 leftover = next.size_bytes - offset_in_data;
      if (leftover < 16) {
        for (u32 i = 0; i < leftover; i++) {
          ASSERT(next.data[offset_in_data + i] == 0);
        }
      } else {
        ASSERT(false);
      }
    }
  }

  m_dbf = !m_dbf;
  switch (mscal_addr) {
    case 17:
      m_stats.mscal_17++;
      if (m_enable_prime_mscals) {
        mscal(17, render_state, prof);
      }
      break;
    case 32:
      m_stats.mscal_32++;
      if (m_enable_prime_mscals) {
        mscal(32, render_state, prof);
      }
      break;

    case 20:
      m_stats.mscal_20++;
      if (m_enable_normal_mscals) {
        mscal(20, render_state, prof);
      }
      break;
    case 35:
      m_stats.mscal_35++;
      if (m_enable_normal_mscals) {
        mscal(35, render_state, prof);
      }
      break;
    default:
      fmt::print("unknown mscal: {}\n", mscal_addr);
      ASSERT(false);
  }

  //  while (true) {
  //    next = dma.read_and_advance();
  //    if (next.vif0() == 0 && next.vifcode1().kind == VifCode::Kind::UNPACK_V4_8) {
  //
  //    } else {
  //      fmt::print("{} : {} {}\n", next.size_bytes, next.vifcode0().print(),
  //      next.vifcode1().print()); ASSERT(false);
  //    }
  //  }
}

/*!
 * Handle the setup DMA data prepared by merc-vu1-init-buffer in GOAL
 */
void MercRenderer::handle_setup(DmaFollower& dma,
                                SharedRenderState* render_state,
                                ScopedProfilerNode& prof) {
  auto first = dma.read_and_advance();

  // 10 quadword setup packet
  ASSERT(first.size_bytes == 10 * 16);
  // m_stats.str += fmt::format("Setup 0: {} {} {}", first.size_bytes / 16,
  // first.vifcode0().print(), first.vifcode1().print());

  // transferred vifcodes
  {
    auto vif0 = first.vifcode0();
    auto vif1 = first.vifcode1();
    // STCYCL 4, 4
    ASSERT(vif0.kind == VifCode::Kind::STCYCL);
    auto vif0_st = VifCodeStcycl(vif0);
    ASSERT(vif0_st.cl == 4 && vif0_st.wl == 4);
    // STMOD
    ASSERT(vif1.kind == VifCode::Kind::STMOD);
    ASSERT(vif1.immediate == 0);
  }

  // 1 qw with 4 vifcodes.
  u32 vifcode_data[4];
  memcpy(vifcode_data, first.data, 16);
  {
    auto vif0 = VifCode(vifcode_data[0]);
    ASSERT(vif0.kind == VifCode::Kind::BASE);
    ASSERT(vif0.immediate == MercDataMemory::BUFFER_BASE);
    auto vif1 = VifCode(vifcode_data[1]);
    ASSERT(vif1.kind == VifCode::Kind::OFFSET);
    ASSERT((s16)vif1.immediate == MercDataMemory::BUFFER_OFFSET);
    auto vif2 = VifCode(vifcode_data[2]);
    ASSERT(vif2.kind == VifCode::Kind::NOP);
    auto vif3 = VifCode(vifcode_data[3]);
    ASSERT(vif3.kind == VifCode::Kind::UNPACK_V4_32);
    VifCodeUnpack up(vif3);
    ASSERT(up.addr_qw == MercDataMemory::LOW_MEMORY);
    ASSERT(!up.use_tops_flag);
    ASSERT(vif3.num == 8);
  }

  // 8 qw's of low memory data
  memcpy(&m_low_memory, first.data + 16, sizeof(LowMemory));
  m_stats.str += fmt::format("Fog: {}\n", m_low_memory.fog.to_string_aligned());

  // 1 qw with another 4 vifcodes.
  u32 vifcode_final_data[4];
  memcpy(vifcode_final_data, first.data + 16 + sizeof(LowMemory), 16);
  {
    ASSERT(VifCode(vifcode_final_data[0]).kind == VifCode::Kind::FLUSHE);
    ASSERT(vifcode_final_data[1] == 0);
    ASSERT(vifcode_final_data[2] == 0);
    VifCode mscal(vifcode_final_data[3]);
    ASSERT(mscal.kind == VifCode::Kind::MSCAL);
    ASSERT(mscal.immediate == 0);
  }

  // copy low memory into the VU "emulation" RAM.
  memcpy(m_buffer.data, &m_low_memory, sizeof(LowMemory));
  mscal(0, render_state, prof);

  auto second = dma.read_and_advance();
  ASSERT(second.size_bytes == 32);  // setting up test register.
  m_direct.render_gif(second.data, 32, render_state, prof);
  auto nothing = dma.read_and_advance();
  ASSERT(nothing.size_bytes == 0);
  ASSERT(nothing.vif0() == 0);
  ASSERT(nothing.vif1() == 0);
}

void MercRenderer::draw_debug_window() {
  ImGui::Text("Ran? %d\n", m_stats.had_data);
  ImGui::Text("%d unpacks, %d bytes\n", m_stats.unpack_count, m_stats.unpack_bytes);
  ImGui::Text("MSCAL: [17] %d [20] %d [32] %d [35] %d \n", m_stats.mscal_17, m_stats.mscal_20,
              m_stats.mscal_32, m_stats.mscal_35);
  ImGui::Text("Debug:\n%s\n", m_stats.str.c_str());
  ImGui::Checkbox("Normal MSCAL enable", &m_enable_normal_mscals);
  ImGui::Checkbox("Prime MSCAL enable", &m_enable_prime_mscals);
  ImGui::Checkbox("Send to direct", &m_enable_send_to_direct);
}

void MercRenderer::xgkick(u16 addr, SharedRenderState* render_state, ScopedProfilerNode& prof) {
  if (m_enable_send_to_direct && render_state->enable_merc_xgkick) {
    m_direct.render_gif(m_buffer.data + (16 * addr), UINT32_MAX, render_state, prof);
  }
}
