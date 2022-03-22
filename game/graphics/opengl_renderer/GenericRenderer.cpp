#include "GenericRenderer.h"
#include "third-party/imgui/imgui.h"

GenericRenderer::GenericRenderer(const std::string& name, BucketId my_id)
    : BucketRenderer(name, my_id),
      m_direct2(30000, 60000, 1000, name, true),
      m_debug_gen2(name, my_id, 1500000, 10000, 3000, 800) {}

void GenericRenderer::init_shaders(ShaderLibrary& shaders) {
  m_direct2.init_shaders(shaders);
  m_debug_gen2.init_shaders(shaders);
}

void GenericRenderer::render(DmaFollower& dma,
                             SharedRenderState* render_state,
                             ScopedProfilerNode& prof) {
  if (render_state->use_generic2) {
    m_debug_gen2.render(dma, render_state, prof);
    return;
  }
  m_xgkick_idx = 0;
  m_skipped_tags = 0;
  m_debug.clear();

  // if the first draw should have no blending, it sets ABE in PRIM, but not ALPHA.
  // the default ALPHA doesn't seem to be right. I don't know what's supposed to set it here.
  // although this is definitely a hack, it doesn't seem to cause problems when the first thing to
  // draw is transparent.
  // m_direct.hack_disable_blend();

  // skip if disabled
  if (!m_enabled) {
    while (dma.current_tag_offset() != render_state->next_bucket) {
      dma.read_and_advance();
    }
    return;
  }

  while (dma.current_tag_offset() != render_state->next_bucket) {
    auto data = dma.read_and_advance();
    m_debug += fmt::format("{} : {} {}\n", data.size_bytes, data.vifcode0().print(),
                           data.vifcode1().print());
    auto v0 = data.vifcode0();
    auto v1 = data.vifcode1();
    if (data.size_bytes == 0) {
      m_debug += "Emtpy Tag\n";
      switch (v0.kind) {
        case VifCode::Kind::STCYCL:
          vu.stcycl = v0.immediate;
          break;
        case VifCode::Kind::NOP:
          break;
        default:
          fmt::print("unknown vifcode0 empty tag: {}\n", v0.print());
          ASSERT(false);
      }
      switch (v1.kind) {
        case VifCode::Kind::STCYCL:
          vu.stcycl = v1.immediate;
          break;
        case VifCode::Kind::NOP:
          break;
        case VifCode::Kind::MSCAL:
          mscal(v1.immediate, render_state, prof);
          break;
        default:
          fmt::print("unknown vifcode1 empty tag: {}\n", v1.print());
          ASSERT(false);
      }
    } else if (v0.kind == VifCode::Kind::FLUSHA && v1.kind == VifCode::Kind::DIRECT) {
      if (render_state->use_direct2) {
        m_direct2.render_gif_data(data.data, render_state, prof);
      }
      ASSERT(v1.immediate == data.size_bytes / 16);
    } else if (v0.kind == VifCode::Kind::NOP && v1.kind == VifCode::Kind::DIRECT) {
      if (render_state->use_direct2) {
        m_direct2.render_gif_data(data.data, render_state, prof);
      }
      ASSERT(v1.immediate == data.size_bytes / 16);
    } else if (v0.kind == VifCode::Kind::STCYCL && v1.kind == VifCode::Kind::UNPACK_V4_32) {
      vu.stcycl = v0.immediate;
      u32 bytes_used = unpack32_4(VifCodeUnpack(v1), data.data, v1.num);
      if (bytes_used < data.size_bytes) {
        handle_dma_stream(data.data + bytes_used, data.size_bytes - bytes_used, render_state, prof);
      } else if (bytes_used > data.size_bytes) {
        ASSERT(false);
      }
    } else if (v0.kind == VifCode::Kind::MSCALF && v1.kind == VifCode::Kind::STMOD) {
      mscal(v0.immediate, render_state, prof);
      ASSERT(v1.immediate == 0);

      u32 data_offset = 0;
      u32 base_vifcode_data;
      memcpy(&base_vifcode_data, data.data + data_offset, 4);
      VifCode base_vc(base_vifcode_data);
      ASSERT(base_vc.immediate == 0);
      data_offset += 4;

      u32 offset_vifcode_data;
      memcpy(&offset_vifcode_data, data.data + data_offset, 4);
      VifCode offset_vc(offset_vifcode_data);
      ASSERT(offset_vc.immediate == 0);
      data_offset += 4;

      for (int i = 0; i < 1; i++) {
        u32 nop_vifcode_data;
        memcpy(&nop_vifcode_data, data.data + data_offset, 4);
        VifCode next_vc(nop_vifcode_data);
        ASSERT(next_vc.kind == VifCode::Kind::NOP);
        data_offset += 4;
      }

      u32 strow_vifcode_data;
      memcpy(&strow_vifcode_data, data.data + data_offset, 4);
      VifCode strow_vc(strow_vifcode_data);
      data_offset += 4;
      ASSERT(strow_vc.kind == VifCode::Kind::STROW);

      memcpy(vu.row, data.data + data_offset, 16);
      data_offset += 16;

      ASSERT(data_offset == 32);
      ASSERT(data_offset == data.size_bytes);
    } else if (v0.kind == VifCode::Kind::NOP && v1.kind == VifCode::Kind::UNPACK_V3_32) {
      u32 bytes_used = unpack32_3(VifCodeUnpack(v1), data.data, v1.num);
      if (bytes_used < data.size_bytes) {
        handle_dma_stream(data.data + bytes_used, data.size_bytes - bytes_used, render_state, prof);
      } else if (bytes_used > data.size_bytes) {
        ASSERT(false);
      }
    } else {
      fmt::print("Generic encountered unknown DMA.\n");
      fmt::print("Size bytes: {}\n", data.size_bytes);
      fmt::print("VIF0: {}\n", data.vifcode0().print());
      fmt::print("VIF1: {}\n", data.vifcode1().print());
      ASSERT(false);
    }
    m_skipped_tags++;
  }
  if (render_state->use_direct2) {
    m_direct2.flush_pending(render_state, prof);
  }
}

void GenericRenderer::handle_dma_stream(const u8* data,
                                        u32 bytes,
                                        SharedRenderState* render_state,
                                        ScopedProfilerNode& prof) {
  while (bytes) {
    u32 tag_data;
    memcpy(&tag_data, data, 4);
    bytes -= 4;
    data += 4;
    VifCode vc(tag_data);
    switch (vc.kind) {
      case VifCode::Kind::NOP:
        break;
      case VifCode::Kind::STCYCL:
        vu.stcycl = vc.immediate;
        break;
      case VifCode::Kind::UNPACK_V3_32: {
        u32 bytes_transferred = unpack32_3(VifCodeUnpack(vc), data, vc.num);
        bytes -= bytes_transferred;
        data += bytes_transferred;
      } break;
      case VifCode::Kind::UNPACK_V4_8: {
        u32 bytes_transferred = unpack8_4(VifCodeUnpack(vc), data, vc.num);
        bytes -= bytes_transferred;
        data += bytes_transferred;
      } break;
      case VifCode::Kind::UNPACK_V2_16: {
        u32 bytes_transferred = unpack16_2(VifCodeUnpack(vc), data, vc.num);
        bytes -= bytes_transferred;
        data += bytes_transferred;
      } break;
      case VifCode::Kind::UNPACK_V4_32: {
        u32 bytes_transferred = unpack32_4(VifCodeUnpack(vc), data, vc.num);
        bytes -= bytes_transferred;
        data += bytes_transferred;
      } break;
      case VifCode::Kind::MSCAL:
        mscal(vc.immediate, render_state, prof);
        break;
      default:
        fmt::print("Generic encountered unknown DMA in handle_dma_stream.\n");
        fmt::print("Bytes remaining: {}\n", bytes);
        fmt::print("VIF: {}\n", vc.print());
        ASSERT(false);
    }
  }
}

void GenericRenderer::draw_debug_window() {
  ImGui::Text("Skipped %d tags", m_skipped_tags);
  ImGui::InputInt("kick min", &m_min_xgkick);
  ImGui::InputInt("kick max", &m_max_xgkick);
  ImGui::Text("Debug:\n%s\n", m_debug.c_str());
  if (ImGui::TreeNode("Gen2")) {
    m_debug_gen2.draw_debug_window();
    ImGui::TreePop();
  }
}

u32 GenericRenderer::unpack32_4(const VifCodeUnpack& up, const u8* data, u32 imm) {
  ASSERT(vu.stcycl == 0x404);
  ASSERT(!up.is_unsigned);
  u32 addr = up.addr_qw;
  ASSERT(imm != 0);
  // ASSERT(!m_vif.stmod);
  ASSERT(!up.use_tops_flag);
  //  if (up.use_tops_flag) {
  //    addr += xitop();
  //  }

  u32 start_in_buff = (addr)*16;
  u32 end_in_buff = start_in_buff + imm * 16;
  ASSERT(start_in_buff < sizeof(m_buffer.data));
  ASSERT(end_in_buff <= sizeof(m_buffer.data));
  memcpy(m_buffer.data + start_in_buff, data, imm * 16);

  // fmt::print("---------------------------------unpack32_4: {} to {}\n", addr, addr + imm);
  return imm * 16;
}

u32 GenericRenderer::unpack32_3(const VifCodeUnpack& up, const u8* data, u32 imm) {
  u32 bytes_read = 0;
  ASSERT(!up.use_tops_flag);
  ASSERT(!up.is_unsigned);
  ASSERT(vu.stcycl == 0x103);  // w = 1, c = 3
  ASSERT(imm != 0);

  for (u32 i = 0; i < imm; i++) {
    u32 xyzw[4];
    memcpy(xyzw, data + bytes_read, 12);
    bytes_read += 12;
    xyzw[3] = 0xbeef;

    // check for garbage going into GENERIC VU1 code.
    float f[3];
    memcpy(f, xyzw, 12);
    //    if (std::abs(f[0]) > 100000) {
    //      fmt::print("VERY SUSPICIOUS VERTEX: {} 0x{:x} at 0x{:x}\n", f[0], xyzw[0],
    //                 (data + bytes_read) - g_ee_main_mem);
    //    }

    // fmt::print("vtx: {} {} {}\n", f[0], f[1], f[2]);
    u32 total_addr = 16 * (up.addr_qw + 3 * i);
    ASSERT(total_addr + 16 <= sizeof(m_buffer.data));
    memcpy(m_buffer.data + total_addr, xyzw, 16);
  }
  // fmt::print("---------------------------------unpack32_3: {} to {} imm {}\n", up.addr_qw,
  // (up.addr_qw + 3 * imm), imm);
  return bytes_read;
}

u32 GenericRenderer::unpack8_4(const VifCodeUnpack& up, const u8* data, u32 imm) {
  u32 bytes_read = 0;
  ASSERT(!up.use_tops_flag);
  ASSERT(up.is_unsigned);
  ASSERT(vu.stcycl == 0x103);  // w = 1, c = 3
  ASSERT(imm != 0);
  for (u32 i = 0; i < imm; i++) {
    u32 xyzw[4] = {data[0], data[1], data[2], data[3]};
    bytes_read += 4;
    data += 4;
    u32 total_addr = 16 * (up.addr_qw + 3 * i);
    ASSERT(total_addr + 16 <= sizeof(m_buffer.data));
    memcpy(m_buffer.data + total_addr, xyzw, 16);
  }
  // fmt::print("---------------------------------unpack8_4: {} to {} imm {}\n", up.addr_qw,
  //           (up.addr_qw + 3 * imm), imm);
  return bytes_read;
}

u32 GenericRenderer::unpack16_2(const VifCodeUnpack& up, const u8* data, u32 imm) {
  u32 bytes_read = 0;
  ASSERT(!up.use_tops_flag);
  ASSERT(!up.is_unsigned);
  ASSERT(vu.stcycl == 0x103);  // w = 1, c = 3
  ASSERT(imm != 0);
  for (u32 i = 0; i < imm; i++) {
    s32 xyzw[4];
    s16 x, y;
    memcpy(&x, data + bytes_read, 2);
    memcpy(&y, data + bytes_read + 2, 2);
    bytes_read += 4;
    xyzw[0] = x;
    xyzw[1] = y;
    xyzw[2] = 0;
    xyzw[3] = 0;

    u32 total_addr = 16 * (up.addr_qw + 3 * i);
    ASSERT(total_addr + 16 <= sizeof(m_buffer.data));
    memcpy(m_buffer.data + total_addr, xyzw, 16);
  }
  //  fmt::print("---------------------------------unpack16_2: {} to {} imm {}\n", up.addr_qw,
  //             (up.addr_qw + 3 * imm), imm);
  return bytes_read;
}

void GenericRenderer::mscal(int imm, SharedRenderState* render_state, ScopedProfilerNode& prof) {
  m_debug += fmt::format("mscal: {}\n", imm);
  switch (imm) {
    case 0:
      mscal0();
      break;
    default:
      mscal_dispatch(imm, render_state, prof);
      break;
  }
}

void GenericRenderer::xgkick(u16 addr, SharedRenderState* render_state, ScopedProfilerNode& prof) {
  if (render_state->enable_generic_xgkick && m_xgkick_idx >= m_min_xgkick &&
      m_xgkick_idx < m_max_xgkick) {
    if (!render_state->use_generic2) {
      if (render_state->use_direct2) {
        m_direct2.render_gif_data(m_buffer.data + (16 * addr), render_state, prof);
      }
    }
  }
  m_xgkick_idx++;
}