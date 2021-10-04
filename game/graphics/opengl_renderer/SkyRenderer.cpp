#include "SkyRenderer.h"
#include "third-party/imgui/imgui.h"
#include "game/graphics/pipelines/opengl.h"

SkyTextureHandler::SkyTextureHandler(const std::string& name, BucketId my_id)
    : BucketRenderer(name, my_id) {}

void SkyTextureHandler::render(DmaFollower& dma, SharedRenderState* render_state) {
  m_debug_dma_str.clear();
  while (dma.current_tag_offset() != render_state->next_bucket) {
    auto tag = dma.current_tag();
    m_debug_dma_str += fmt::format("@ 0x{:x} tag: {}\n", dma.current_tag_offset(), tag.print());
    auto data = dma.read_and_advance();
    VifCode code(data.vif0());
    m_debug_dma_str += fmt::format(" vif: {}\n", code.print());
    if (code.kind == VifCode::Kind::NOP) {
      m_debug_dma_str += fmt::format(" vif: {}\n", VifCode(data.vif1()).print());
    }
  }
}

void SkyTextureHandler::draw_debug_window() {
  ImGui::Separator();
  ImGui::Checkbox("DMA print", &m_print_debug_dma);
  if (m_print_debug_dma) {
    ImGui::Text("%s", m_debug_dma_str.c_str());
  }
}

SkyRenderer::SkyRenderer(const std::string& name, BucketId my_id)
    : BucketRenderer(name, my_id),
      m_direct_renderer("sky-direct", my_id, 100, DirectRenderer::Mode::NORMAL) {}

void SkyRenderer::render(DmaFollower& dma, SharedRenderState* render_state) {
  m_debug_dma_str.clear();
  m_direct_renderer.reset_state();
  // First thing should be a NEXT with two nops. this is a jump from buckets to sprite data
  auto data0 = dma.read_and_advance();
  assert(data0.vif1() == 0);
  assert(data0.vif0() == 0);
  assert(data0.size_bytes == 0);

  if (dma.current_tag().kind == DmaTag::Kind::CALL) {
    // sky renderer didn't run, let's just get out of here.
    for (int i = 0; i < 4; i++) {
      dma.read_and_advance();
    }
    assert(dma.current_tag_offset() == render_state->next_bucket);
    return;
  }

  auto setup_packet = dma.read_and_advance();
  assert(setup_packet.size_bytes == 16 * 4);
  m_direct_renderer.render_gif(setup_packet.data, setup_packet.size_bytes, render_state);


  auto draw_setup_packet = dma.read_and_advance();
  assert(draw_setup_packet.size_bytes == 16 * 5);
  m_direct_renderer.render_gif(draw_setup_packet.data, draw_setup_packet.size_bytes, render_state);
  // tex0: tbw = 1, th = 5, hw = 5, sky-base-block
  // mmag/mmin = 1
  // clamp
  // drawing.
  int dma_idx= 0;
  while (dma.current_tag().kind == DmaTag::Kind::CNT) {
    auto data = dma.read_and_advance();
    assert(data.vifcode0().kind == VifCode::Kind::NOP);
    assert(data.vifcode1().kind == VifCode::Kind::DIRECT);
    assert(data.vifcode1().immediate == data.size_bytes / 16);
    render_gif(data.data, data.size_bytes, render_state);
    // TODO:
    if (dma_idx == 1 || dma_idx == 2) {
//    if (dma_idx == 0) {
      m_direct_renderer.render_gif(data.data, data.size_bytes, render_state);
      m_direct_renderer.flush_pending(render_state);
    }
    dma_idx++;
  }

  auto empty = dma.read_and_advance();
  assert(empty.size_bytes == 0);
  assert(empty.vif0() == 0);
  assert(empty.vif1() == 0);

  assert(dma.current_tag().kind == DmaTag::Kind::CALL);
  dma.read_and_advance();
  dma.read_and_advance();  // cnt
  assert(dma.current_tag().kind == DmaTag::Kind::RET);
  dma.read_and_advance();  // ret
  dma.read_and_advance();  // ret
  assert(dma.current_tag_offset() == render_state->next_bucket);
}

void SkyRenderer::draw_debug_window() {
  ImGui::Separator();
  ImGui::Checkbox("DMA print", &m_print_debug_dma);
  if (m_print_debug_dma) {
    ImGui::Text("%s", m_debug_dma_str.c_str());
  }

  if (ImGui::TreeNode("direct")) {
    m_direct_renderer.draw_debug_window();
    ImGui::TreePop();
  }
}

void SkyRenderer::render_gif(const u8* data, u32 size, SharedRenderState* render_state) {
  assert(size >= 16);
  bool eop = false;

  u32 offset = 0;
  while (!eop) {
    assert(offset < size);
    m_debug_dma_str += "PACKET START\n";
    GifTag tag(data + offset);
    offset += 16;
    m_debug_dma_str += fmt::format("PACKET {}/{} {}\n", offset, size, tag.print());
    // fmt::print("Tag at offset {}: {}\n", offset, tag.print());

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
        //        handle_prim(tag.prim(), render_state);
        m_debug_dma_str += fmt::format("PACKED prim special: 0x{:x}\n", tag.prim());
      }
      for (u32 loop = 0; loop < tag.nloop(); loop++) {
        for (u32 reg = 0; reg < nreg; reg++) {
          m_debug_dma_str += fmt::format("packed reg: {}\n", reg_descriptor_name(reg_desc[reg]));
          //          switch (reg_desc[reg]) {
          //            case GifTag::RegisterDescriptor::AD:
          //              handle_ad(data + offset, render_state);
          //              break;
          //            case GifTag::RegisterDescriptor::ST:
          //              handle_st_packed(data + offset);
          //              break;
          //            case GifTag::RegisterDescriptor::RGBAQ:
          //              handle_rgbaq_packed(data + offset);
          //              break;
          //            case GifTag::RegisterDescriptor::XYZF2:
          //              handle_xyzf2_packed(data + offset, render_state);
          //              break;
          //            case GifTag::RegisterDescriptor::PRIM:
          //              handle_prim_packed(data + offset, render_state);
          //              break;
          //            case GifTag::RegisterDescriptor::TEX0_1:
          //              handle_tex0_1_packed(data + offset, render_state);
          //              break;
          //            default:
          //              fmt::print("Register {} is not supported in packed mode yet\n",
          //                         reg_descriptor_name(reg_desc[reg]));
          //              assert(false);
          //          }
          offset += 16;  // PACKED = quadwords
        }
      }
    } else if (format == GifTag::Format::REGLIST) {
      for (u32 loop = 0; loop < tag.nloop(); loop++) {
        for (u32 reg = 0; reg < nreg; reg++) {
          u64 register_data;
          memcpy(&register_data, data + offset, 8);
          m_debug_dma_str +=
              fmt::format("loop: {} reg: {} {}\n", loop, reg, reg_descriptor_name(reg_desc[reg]));
          //          switch (reg_desc[reg]) {
          //            case GifTag::RegisterDescriptor::PRIM:
          //              handle_prim(register_data, render_state);
          //              break;
          //            case GifTag::RegisterDescriptor::RGBAQ:
          //              handle_rgbaq(register_data);
          //              break;
          //            case GifTag::RegisterDescriptor::XYZF2:
          //              handle_xyzf2(register_data, render_state);
          //              break;
          //            default:
          //              fmt::print("Register {} is not supported in reglist mode yet\n",
          //                         reg_descriptor_name(reg_desc[reg]));
          //              assert(false);
          //          }
          offset += 8;  // PACKED = quadwords
        }
      }
    } else {
      fmt::print("invalid format {}\n", (int)format);
      assert(false);  // format not packed or reglist.
    }

    eop = tag.eop();
  }

  assert(offset == size);
}