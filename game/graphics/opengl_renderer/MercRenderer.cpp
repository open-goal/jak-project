#include "MercRenderer.h"

MercRenderer::MercRenderer(const std::string& name, BucketId my_id) : BucketRenderer(name, my_id) {}

void MercRenderer::render(DmaFollower& dma,
                          SharedRenderState* render_state,
                          ScopedProfilerNode& prof) {
  while (dma.current_tag_offset() != render_state->next_bucket) {
    dma.read_and_advance();
  }
}

void MercRenderer::draw_debug_window() {}