#include "OceanNear.h"

OceanNear::OceanNear(const std::string& name, BucketId my_id)
    : BucketRenderer(name, my_id), m_texture_renderer(false) {}

void OceanNear::draw_debug_window() {
  m_texture_renderer.draw_debug_window();
}

void OceanNear::init_textures(TexturePool& pool) {
  m_texture_renderer.init_textures(pool);
}

void OceanNear::render(DmaFollower& dma,
                       SharedRenderState* render_state,
                       ScopedProfilerNode& prof) {
  // skip if disabled
  if (!m_enabled) {
    while (dma.current_tag_offset() != render_state->next_bucket) {
      dma.read_and_advance();
    }
    return;
  }

  // jump to bucket
  auto data0 = dma.read_and_advance();
  ASSERT(data0.vif1() == 0);
  ASSERT(data0.vif0() == 0);
  ASSERT(data0.size_bytes == 0);

  // see if bucket is empty or not
  if (dma.current_tag().kind == DmaTag::Kind::CALL) {
    // renderer didn't run, let's just get out of here.
    for (int i = 0; i < 4; i++) {
      dma.read_and_advance();
    }
    ASSERT(dma.current_tag_offset() == render_state->next_bucket);
    return;
  }

  {
    auto p = prof.make_scoped_child("texture");
    // TODO: this looks the same as the previous ocean renderer to me... why do it again?
    m_texture_renderer.handle_ocean_texture(dma, render_state, p);
  }

  while (dma.current_tag_offset() != render_state->next_bucket) {
    dma.read_and_advance();
  }
}