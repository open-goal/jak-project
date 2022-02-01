#include "MercRenderer.h"

#include "third-party/imgui/imgui.h"

MercRenderer::MercRenderer(const std::string& name, BucketId my_id)
    : BucketRenderer(name, my_id),
      m_direct(fmt::format("{}-dir", name), my_id, 0x30000, DirectRenderer::Mode::NORMAL) {
  memset(m_buffer_memory, 0, sizeof(m_buffer_memory));
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
  assert(data0.vif1() == 0);
  assert(data0.vif0() == 0);
  assert(data0.size_bytes == 0);
  if (dma.current_tag().kind == DmaTag::Kind::CALL) {
    // renderer didn't run, let's just get out of here.
    for (int i = 0; i < 4; i++) {
      dma.read_and_advance();
    }
    assert(dma.current_tag_offset() == render_state->next_bucket);
    return;
  }
  m_stats.had_data = true;
  assert(data0.size_bytes == 0);
  assert(data0.vif0() == 0);
  assert(data0.vif1() == 0);

  // if we reach here, there's stuff to draw
  handle_setup(dma, render_state, prof);

  m_direct.reset_state();
  while (dma.current_tag_offset() != render_state->next_bucket) {
    handle_merc_chain(dma, render_state, prof);
  }
  assert(dma.current_tag_offset() == render_state->next_bucket);
  m_direct.flush_pending(render_state, prof);
}

bool tag_is_nothing_next(const DmaFollower& dma) {
  return dma.current_tag().kind == DmaTag::Kind::NEXT && dma.current_tag().qwc == 0 &&
         dma.current_tag_vif0() == 0 && dma.current_tag_vif1() == 0;
}

void MercRenderer::unpack32(const VifCodeUnpack& up, const u8* data, u32 imm) {
  assert(!up.is_unsigned);
  u32 addr = up.addr_qw;
  assert(imm != 0);
  assert(!m_vif.stmod);
  if (up.use_tops_flag) {
    addr += xitop();
  }


  u32 start_in_buff = (addr)*16;
  u32 end_in_buff = start_in_buff + imm * 16;
  assert(start_in_buff < sizeof(m_buffer_memory));
  assert(end_in_buff <= sizeof(m_buffer_memory));
  memcpy(m_buffer_memory + start_in_buff, data, imm * 16);
}

void MercRenderer::unpack8(const VifCodeUnpack& up, const u8* data, u32 imm) {
  // assert(m_vif.stmod);

  assert(up.is_unsigned);
  u32 addr = up.addr_qw;
  if (up.use_tops_flag) {
    addr += xitop();
  }
  assert(imm != 0);

  u32 start_in_buff = (addr)*16;
  u32 end_in_buff = start_in_buff + imm * 16;
  assert(start_in_buff < sizeof(m_buffer_memory));
  assert(end_in_buff <= sizeof(m_buffer_memory));
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
    memcpy(m_buffer_memory + start_in_buff + i * 16, temp, 16);
  }
}

void MercRenderer::handle_merc_chain(DmaFollower& dma,
                                     SharedRenderState* render_state,
                                     ScopedProfilerNode& prof) {
  //  fmt::print("DMA: {}\n", dma.current_tag().print());
  while (tag_is_nothing_next(dma)) {
    auto nothing = dma.read_and_advance();
    assert(nothing.size_bytes == 0);
  }
  if (dma.current_tag().kind == DmaTag::Kind::CALL) {
    for (int i = 0; i < 4; i++) {
      dma.read_and_advance();
    }
    return;
  }

  auto init = dma.read_and_advance();

  assert(init.vifcode0().kind == VifCode::Kind::STROW);
  assert(init.size_bytes == 16);
  m_vif.row[0] = init.vif1();
  memcpy(m_vif.row + 1, init.data, 12);
  u32 extra;
  memcpy(&extra, init.data + 12, 4);
  assert(extra == 0);
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
        assert(vif0.immediate == 0 || vif0.immediate == 1);
        m_vif.stmod = vif0.immediate;
        break;
      default:
        assert(false);
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
        assert(next.size_bytes == 0);
        setting_up = false;
        break;
      default:
        assert(false);
    }

    assert(offset_in_data <= next.size_bytes);
    if (offset_in_data < next.size_bytes) {
      assert((offset_in_data % 4) == 0);
      u32 leftover = next.size_bytes - offset_in_data;
      if (leftover < 16) {
        for (u32 i = 0; i < leftover; i++) {
          assert(next.data[offset_in_data + i] == 0);
        }
      } else {
        assert(false);
      }
    }
  }

  m_dbf = !m_dbf;
  switch (mscal_addr) {
    case 20:
      m_stats.mscal_20++;
      mscal(20, render_state, prof);
      break;
    case 17:
      //m_stats.mscal_35++;
      mscal(17, render_state, prof);
      break;
    case 32:
//      m_stats.mscal_35++;
      mscal(32, render_state, prof);
      break;
    case 35:
      m_stats.mscal_35++;
      mscal(35, render_state, prof);
      break;
    default:
      fmt::print("unknown mscal: {}\n", mscal_addr);
      assert(false);
  }

  //  while (true) {
  //    next = dma.read_and_advance();
  //    if (next.vif0() == 0 && next.vifcode1().kind == VifCode::Kind::UNPACK_V4_8) {
  //
  //    } else {
  //      fmt::print("{} : {} {}\n", next.size_bytes, next.vifcode0().print(),
  //      next.vifcode1().print()); assert(false);
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
  assert(first.size_bytes == 10 * 16);
  // m_stats.str += fmt::format("Setup 0: {} {} {}", first.size_bytes / 16,
  // first.vifcode0().print(), first.vifcode1().print());

  // transferred vifcodes
  {
    auto vif0 = first.vifcode0();
    auto vif1 = first.vifcode1();
    // STCYCL 4, 4
    assert(vif0.kind == VifCode::Kind::STCYCL);
    auto vif0_st = VifCodeStcycl(vif0);
    assert(vif0_st.cl == 4 && vif0_st.wl == 4);
    // STMOD
    assert(vif1.kind == VifCode::Kind::STMOD);
    assert(vif1.immediate == 0);
  }

  // 1 qw with 4 vifcodes.
  u32 vifcode_data[4];
  memcpy(vifcode_data, first.data, 16);
  {
    auto vif0 = VifCode(vifcode_data[0]);
    assert(vif0.kind == VifCode::Kind::BASE);
    assert(vif0.immediate == MercDataMemory::BUFFER_BASE);
    auto vif1 = VifCode(vifcode_data[1]);
    assert(vif1.kind == VifCode::Kind::OFFSET);
    assert((s16)vif1.immediate == MercDataMemory::BUFFER_OFFSET);
    auto vif2 = VifCode(vifcode_data[2]);
    assert(vif2.kind == VifCode::Kind::NOP);
    auto vif3 = VifCode(vifcode_data[3]);
    assert(vif3.kind == VifCode::Kind::UNPACK_V4_32);
    VifCodeUnpack up(vif3);
    assert(up.addr_qw == MercDataMemory::LOW_MEMORY);
    assert(!up.use_tops_flag);
    assert(vif3.num == 8);
  }

  // 8 qw's of low memory data
  memcpy(&m_low_memory, first.data + 16, sizeof(LowMemory));
  m_stats.str += fmt::format("Fog: {}\n", m_low_memory.fog.to_string_aligned());

  // 1 qw with another 4 vifcodes.
  u32 vifcode_final_data[4];
  memcpy(vifcode_final_data, first.data + 16 + sizeof(LowMemory), 16);
  {
    assert(VifCode(vifcode_final_data[0]).kind == VifCode::Kind::FLUSHE);
    assert(vifcode_final_data[1] == 0);
    assert(vifcode_final_data[2] == 0);
    VifCode mscal(vifcode_final_data[3]);
    assert(mscal.kind == VifCode::Kind::MSCAL);
    assert(mscal.immediate == 0);
  }

  // copy low memory into the VU "emulation" RAM.
  memcpy(m_buffer_memory, &m_low_memory, sizeof(LowMemory));
  mscal(0, render_state, prof);

  //  {
  //    //  lq.xyzw vf01, 7(vi00)      |  nop
  //    vu.vf01 = Vf(m_low_memory.fog);
  //    //  lq.xyzw vf25, 3(vi00)      |  nop
  //    vu.vf25 = Vf(m_low_memory.perspective[0]);
  //    //  lq.xyzw vf26, 4(vi00)      |  nop
  //    vu.vf26 = Vf(m_low_memory.perspective[1]);
  //    //  lq.xyzw vf27, 5(vi00)      |  nop
  //    vu.vf27 = Vf(m_low_memory.perspective[2]);
  //    //  lq.xyzw vf28, 6(vi00)      |  nop
  //    vu.vf28 = Vf(m_low_memory.perspective[3]);
  //    //  mr32.xyzw vf01, vf01       |  nop
  //    vu.vf01.mr32(Mask::xyzw, vu.vf01);
  //    //  move.y vf25, vf26          |  nop
  //    vu.vf25.move(Mask::y, vu.vf26);
  //    //  move.zw vf25, vf27         |  nop
  //    vu.vf25.move(Mask::zw, vu.vf27);
  //    //  sq.xyzw vf25, 3(vi00)      |  nop
  //    sq_buffer(Mask::xyzw, vu.vf25, 3);
  //    //  2048.0                     |  nop :i
  //    //  255.0                      |  maxi.x vf17, vf00, I :i
  //    vu.vf17.x() = 2048.f;
  //    //  -65537.0                   |  maxi.y vf17, vf00, I :i
  //    vu.vf17.y() = 255.f;
  //    //  mr32.xyzw vf02, vf01       |  minii.z vf17, vf00, I
  //    vu.vf17.z() = -65537.f;
  //    vu.vf02.mr32(Mask::xyzw, vu.vf01);
  //    //  lq.xyzw vf22, 2(vi00)      |  minii.z vf18, vf00, I
  //    vu.vf18.z() = -65537.f;
  //    vu.vf22 = Vf(m_low_memory.hvdf_offset);
  //    //  0.003921569                |  minii.z vf19, vf00, I :i
  //    vu.vf19.z() = -65537.f;
  //    //  sq.xyzw vf28, 4(vi00)      |  minii.w vf29, vf00, I :e
  //    vu.vf29.w() = 0.003921569;
  //    sq_buffer(Mask::xyzw, vu.vf28, 4);
  //    //  mr32.xyzw vf03, vf02       |  nop
  //    vu.vf03.mr32(Mask::xyzw, vu.vf02);
  //  }

  auto second = dma.read_and_advance();
  assert(second.size_bytes == 32);  // setting up test register.
  m_direct.render_gif(second.data, 32, render_state, prof);
  auto nothing = dma.read_and_advance();
  assert(nothing.size_bytes == 0);
  assert(nothing.vif0() == 0);
  assert(nothing.vif1() == 0);
}

void MercRenderer::draw_debug_window() {
  ImGui::Text("Ran? %d\n", m_stats.had_data);
  ImGui::Text("%d unpacks, %d bytes\n", m_stats.unpack_count, m_stats.unpack_bytes);
  ImGui::Text("%d mscal 20, %d mscal 35\n", m_stats.mscal_20, m_stats.mscal_35);
  ImGui::Text("Debug:\n%s\n", m_stats.str.c_str());
}

void debug_print_packet(u8* data, u32 max_offset) {
  bool eop = false;

  u32 offset = 0;
  while (!eop) {
    assert(offset < max_offset);
    GifTag tag(data + offset);
    offset += 16;

    // unpack registers.
    // faster to do it once outside of the nloop loop.
    GifTag::RegisterDescriptor reg_desc[16];
    u32 nreg = tag.nreg();
    for (u32 i = 0; i < nreg; i++) {
      reg_desc[i] = tag.reg(i);
    }

    auto format = tag.flg();
    if (format == GifTag::Format::PACKED) {
      if (tag.pre()) {
        // handle_prim(tag.prim(), render_state, prof);
      }
      for (u32 loop = 0; loop < tag.nloop(); loop++) {
        for (u32 reg = 0; reg < nreg; reg++) {
          fmt::print("{}\n", reg_descriptor_name(reg_desc[reg]));
          switch (reg_desc[reg]) {
            case GifTag::RegisterDescriptor::AD:
              // handle_ad(data + offset, render_state, prof);
              {
                u64 value;
                GsRegisterAddress addr;
                memcpy(&value, data + offset, sizeof(u64));
                memcpy(&addr, data + offset + 8, sizeof(GsRegisterAddress));
                fmt::print("  {}\n", register_address_name(addr));
              }

              break;
            case GifTag::RegisterDescriptor::ST:
              // handle_st_packed(data + offset);
              break;
            case GifTag::RegisterDescriptor::RGBAQ:
              // handle_rgbaq_packed(data + offset);
              break;
            case GifTag::RegisterDescriptor::XYZF2: {
              u32 x, y;
              memcpy(&x, data + offset, 4);
              memcpy(&y, data + offset + 4, 4);

              u64 upper;
              memcpy(&upper, data + offset + 8, 8);
              u32 z = (upper >> 4) & 0xffffff;

              u8 f = (upper >> 36);
              bool adc = upper & (1ull << 47);
              fmt::print("  {:x} {:x} {:x} {:x}\n", x, y, z, f);
//              if (x == 0x80000000 && y == 0x80000000) {
//                x = 0x8000;
//                y = 0x8000;
//                memcpy(data + offset,&x, 4);
//                memcpy(data + offset + 4,&y, 4);
//              }
            }
            // handle_xyzf2_packed(data + offset, render_state, prof);
            break;
              //            case GifTag::RegisterDescriptor::PRIM:
              //              handle_prim_packed(data + offset, render_state, prof);
              //              break;
              //            case GifTag::RegisterDescriptor::TEX0_1:
              //              handle_tex0_1_packed(data + offset, render_state, prof);
              //              break;
            default:
              fmt::print("Register {} is not supported in packed mode yet\n",
                         reg_descriptor_name(reg_desc[reg]));
              assert(false);
          }
          offset += 16;  // PACKED = quadwords
        }
      }
    } else if (format == GifTag::Format::REGLIST) {
      assert(false);
    } else {
      assert(false);  // format not packed or reglist.
    }

    eop = tag.eop();
  }
}

void MercRenderer::xgkick(u16 addr, SharedRenderState* render_state, ScopedProfilerNode& prof) {
  int remaining = sizeof(m_buffer_memory) - (16 * addr);
  // debug_print_packet(m_buffer_memory + (16 * addr), remaining);
  m_direct.render_gif(m_buffer_memory + (16 * addr), UINT32_MAX, render_state, prof);
}
