#include "ShadowRenderer.h"

ShadowRenderer::ShadowRenderer(const std::string& name, BucketId my_id)
    : BucketRenderer(name, my_id), m_direct(name, my_id, 0x4000) {}

void ShadowRenderer::draw_debug_window() {
  m_direct.draw_debug_window();
}

void ShadowRenderer::render(DmaFollower& dma,
                            SharedRenderState* render_state,
                            ScopedProfilerNode& /*prof*/) {
  while (dma.current_tag_offset() != render_state->next_bucket) {
    dma.read_and_advance();
  }
}