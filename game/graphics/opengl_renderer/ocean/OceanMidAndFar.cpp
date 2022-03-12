#include "OceanMidAndFar.h"
#include "third-party/imgui/imgui.h"

OceanMidAndFar::OceanMidAndFar(const std::string& name, BucketId my_id)
    : BucketRenderer(name, my_id), m_direct(name, my_id, 4096) {}

void OceanMidAndFar::draw_debug_window() {
  m_texture_renderer.draw_debug_window();
  m_direct.draw_debug_window();
}

void OceanMidAndFar::init_textures(TexturePool& pool) {
  m_texture_renderer.init_textures(pool);
}

void OceanMidAndFar::render(DmaFollower& dma,
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
  m_direct.reset_state();

  {
    auto p = prof.make_scoped_child("texture");
    m_texture_renderer.handle_ocean_texture(dma, render_state, p);
  }

  handle_ocean_far(dma, render_state, prof);

  auto final_next = dma.read_and_advance();
  ASSERT(final_next.vifcode0().kind == VifCode::Kind::NOP &&
         final_next.vifcode1().kind == VifCode::Kind::NOP && final_next.size_bytes == 0);
  for (int i = 0; i < 4; i++) {
    dma.read_and_advance();
  }
  ASSERT(dma.current_tag_offset() == render_state->next_bucket);

  m_direct.flush_pending(render_state, prof);
}

void OceanMidAndFar::handle_ocean_far(DmaFollower& dma,
                                      SharedRenderState* render_state,
                                      ScopedProfilerNode& prof) {
  auto init_data = dma.read_and_advance();
  ASSERT(init_data.size_bytes == 160);
  u8 init_data_buffer[160];
  memcpy(init_data_buffer, init_data.data, 160);

  // this is a bit of a hack, but it patches the ta0 to 0 in
  // (set! (-> (the-as (pointer gs-texa) s4-0) 8) (new 'static 'gs-texa :ta0 #x80 :ta1 #x80))
  // TODO figure out if we actually have do something here.
  u8 val = 0;
  memcpy(init_data_buffer + 80, &val, 1);
  m_direct.render_gif(init_data_buffer, 160, render_state, prof);

  while (dma.current_tag().kind == DmaTag::Kind::CNT) {
    auto data = dma.read_and_advance();
    ASSERT(data.vifcode0().kind == VifCode::Kind::NOP);
    ASSERT(data.vifcode1().kind == VifCode::Kind::DIRECT);
    ASSERT(data.size_bytes / 16 == data.vifcode1().immediate);
    m_direct.render_gif(data.data, data.size_bytes, render_state, prof);
  }
}