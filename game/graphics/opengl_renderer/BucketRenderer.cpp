#include "BucketRenderer.h"

#include "third-party/fmt/core.h"
#include "third-party/imgui/imgui.h"

std::string BucketRenderer::name_and_id() const {
  return fmt::format("[{:2d}] {}", (int)m_my_id, m_name);
}

EmptyBucketRenderer::EmptyBucketRenderer(const std::string& name, int my_id)
    : BucketRenderer(name, my_id) {}

void EmptyBucketRenderer::render(DmaFollower& dma,
                                 SharedRenderState* render_state,
                                 ScopedProfilerNode& /*prof*/) {
  if (render_state->version == GameVersion::Jak1) {
    // an empty bucket should have 4 things:
    // a NEXT in the bucket buffer
    // a CALL that calls the default register buffer chain
    // a CNT then RET to get out of the default register buffer chain
    // a NEXT to get to the next bucket.

    // NEXT
    auto first_tag = dma.current_tag();
    dma.read_and_advance();
    ASSERT(first_tag.kind == DmaTag::Kind::NEXT && first_tag.qwc == 0);

    // CALL
    auto call_tag = dma.current_tag();
    dma.read_and_advance();
    ASSERT_MSG(call_tag.kind == DmaTag::Kind::CALL && call_tag.qwc == 0,
               fmt::format("Bucket renderer {} ({}) was supposed to be empty, but wasn't\n",
                           m_my_id, m_name));

    // in the default reg buffer:
    ASSERT(dma.current_tag_offset() == render_state->default_regs_buffer);
    dma.read_and_advance();
    ASSERT(dma.current_tag().kind == DmaTag::Kind::RET);
    dma.read_and_advance();

    // NEXT to next buffer
    auto to_next_buffer = dma.current_tag();
    ASSERT(to_next_buffer.kind == DmaTag::Kind::NEXT);
    ASSERT(to_next_buffer.qwc == 0);
    dma.read_and_advance();

    // and we should now be in the next bucket!
    ASSERT(dma.current_tag_offset() == render_state->next_bucket);
  } else {
    auto first_tag = dma.current_tag();
    dma.read_and_advance();
    if (first_tag.kind != DmaTag::Kind::CNT || first_tag.qwc != 0) {
      fmt::print("Bucket renderer {} ({}) was supposed to be empty, but wasn't\n", m_my_id, m_name);
      ASSERT(false);
    }
  }
}

SkipRenderer::SkipRenderer(const std::string& name, int my_id) : BucketRenderer(name, my_id) {}

void SkipRenderer::render(DmaFollower& dma,
                          SharedRenderState* render_state,
                          ScopedProfilerNode& /*prof*/) {
  while (dma.current_tag_offset() != render_state->next_bucket) {
    dma.read_and_advance();
  }
}

PrintRenderer::PrintRenderer(const std::string& name, int my_id) : BucketRenderer(name, my_id) {}

void PrintRenderer::render(DmaFollower& dma,
                           SharedRenderState* render_state,
                           ScopedProfilerNode& /*prof*/) {
  // jump to bucket
  dma.read_and_advance();

  auto transfers = 0;
  // print the entire chain
  fmt::print("START {} DMA!!!!!!!\n", m_name);
  while (dma.current_tag_offset() != render_state->next_bucket) {
    auto dmatag = dma.current_tag();
    auto data = dma.read_and_advance();
    printf(
        "dma transfer %d:\n%ssize: %d\nvif0: %s, data: %d\nvif1: %s, data: %d, imm: "
        "%d\n\n",
        transfers, dmatag.print().c_str(), data.size_bytes, data.vifcode0().print().c_str(),
        data.vif0(), data.vifcode1().print().c_str(), data.vifcode1().num,
        data.vifcode1().immediate);
    transfers++;
  }
  printf("transfers: %d\n\n", transfers);
}

void SharedRenderState::reset() {
  has_pc_data = false;
  for (auto& x : occlusion_vis) {
    x.valid = false;
  }
  load_status_debug.clear();
}

RenderMux::RenderMux(const std::string& name,
                     int my_id,
                     std::vector<std::unique_ptr<BucketRenderer>> renderers)
    : BucketRenderer(name, my_id), m_renderers(std::move(renderers)) {
  for (auto& r : m_renderers) {
    m_name_strs.push_back(r->name_and_id());
  }
  for (auto& n : m_name_strs) {
    m_name_str_ptrs.push_back(n.data());
  }
}

void RenderMux::render(DmaFollower& dma,
                       SharedRenderState* render_state,
                       ScopedProfilerNode& prof) {
  m_renderers[m_render_idx]->enabled() = m_enabled;
  m_renderers[m_render_idx]->render(dma, render_state, prof);
}

void RenderMux::draw_debug_window() {
  ImGui::ListBox("Pick", &m_render_idx, m_name_str_ptrs.data(), m_renderers.size());
  ImGui::Separator();
  m_renderers[m_render_idx]->draw_debug_window();
}

void RenderMux::init_textures(TexturePool& tp, GameVersion version) {
  for (auto& rend : m_renderers) {
    rend->init_textures(tp, version);
  }
}

void RenderMux::init_shaders(ShaderLibrary& sl) {
  for (auto& rend : m_renderers) {
    rend->init_shaders(sl);
  }
}