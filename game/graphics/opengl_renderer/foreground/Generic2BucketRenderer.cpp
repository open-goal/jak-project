#include "Generic2BucketRenderer.h"

Generic2BucketRenderer::Generic2BucketRenderer(const std::string& name,
                                               int id,
                                               std::shared_ptr<Generic2> renderer,
                                               Generic2::Mode mode)
    : BucketRenderer(name, id), m_generic(renderer), m_mode(mode) {}

void Generic2BucketRenderer::draw_debug_window() {
  m_generic->draw_debug_window();
}

void Generic2BucketRenderer::render(DmaFollower& dma,
                                    SharedRenderState* render_state,
                                    ScopedProfilerNode& prof) {
  // if the user has asked to disable the renderer, just advance the dma follower to the next
  // bucket and return immediately.
  if (!m_enabled) {
    while (dma.current_tag_offset() != render_state->next_bucket) {
      dma.read_and_advance();
    }
    return;
  }
  m_generic->render_in_mode(dma, render_state, prof, m_mode);
  m_empty = m_generic->empty();
}

bool Generic2BucketRenderer::empty() const {
  return m_empty;
}
