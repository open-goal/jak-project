#include "SkyRenderer.h"
#include "third-party/imgui/imgui.h"

SkyTextureHandler::SkyTextureHandler(const std::string& name, BucketId my_id) : BucketRenderer(name, my_id) {}

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

SkyRenderer::SkyRenderer(const std::string& name, BucketId my_id) : BucketRenderer(name, my_id) {}

void SkyRenderer::render(DmaFollower& dma, SharedRenderState* render_state) {
  m_debug_dma_str.clear();
  // First thing should be a NEXT with two nops. this is a jump from buckets to sprite data
  auto data0 = dma.read_and_advance();
  assert(data0.vif1() == 0);
  assert(data0.vif0() == 0);
  assert(data0.size_bytes == 0);

  if (dma.current_tag().kind == DmaTag::Kind::CALL) {
    // sprite renderer didn't run, let's just get out of here.
    for (int i = 0; i < 4; i++) {
      dma.read_and_advance();
    }
    assert(dma.current_tag_offset() == render_state->next_bucket);
    return;
  }

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

void SkyRenderer::draw_debug_window() {
  ImGui::Separator();
  ImGui::Checkbox("DMA print", &m_print_debug_dma);
  if (m_print_debug_dma) {
    ImGui::Text("%s", m_debug_dma_str.c_str());
  }
}