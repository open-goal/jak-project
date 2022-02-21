#include "GenericRenderer.h"

GenericRenderer::GenericRenderer(const std::string& name, BucketId my_id)
    : BucketRenderer(name, my_id) {}

void GenericRenderer::render(DmaFollower& dma,
                             SharedRenderState* render_state,
                             ScopedProfilerNode& prof) {
  while (dma.current_tag_offset() != render_state->next_bucket) {
    dma.read_and_advance();
  }
}

void GenericRenderer::draw_debug_window() {}