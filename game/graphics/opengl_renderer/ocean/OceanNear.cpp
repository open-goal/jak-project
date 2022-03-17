#include "OceanNear.h"

OceanNear::OceanNear(const std::string& name, BucketId my_id)
    : BucketRenderer(name, my_id), m_texture_renderer(false), m_direct(name, my_id, 0x4000) {
  for (auto& a : m_vu_data) {
    a.fill(0);
  }
}

void OceanNear::draw_debug_window() {
  m_direct.draw_debug_window();
}

void OceanNear::init_textures(TexturePool& pool) {
  m_texture_renderer.init_textures(pool);
}

static bool is_end_tag(const DmaTag& tag, const VifCode& v0, const VifCode& v1) {
  return tag.qwc == 2 && tag.kind == DmaTag::Kind::CNT && v0.kind == VifCode::Kind::NOP &&
         v1.kind == VifCode::Kind::DIRECT;
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

  if (dma.current_tag().qwc != 2) {
    fmt::print("abort!\n");
    while (dma.current_tag_offset() != render_state->next_bucket) {
      dma.read_and_advance();
    }
    return;
  }

  // direct setup
  m_direct.reset_state();
  {
    auto setup = dma.read_and_advance();
    ASSERT(setup.vifcode0().kind == VifCode::Kind::NOP);
    ASSERT(setup.vifcode1().kind == VifCode::Kind::DIRECT);
    ASSERT(setup.size_bytes == 32);
    m_direct.render_gif(setup.data, 32, render_state, prof);
  }

  // oofset and base
  {
    auto ob = dma.read_and_advance();
    ASSERT(ob.size_bytes == 0);
    auto base = ob.vifcode0();
    auto off = ob.vifcode1();
    ASSERT(base.kind == VifCode::Kind::BASE);
    ASSERT(off.kind == VifCode::Kind::OFFSET);
    ASSERT(base.immediate == VU1_INPUT_BUFFER_BASE);
    ASSERT(off.immediate == VU1_INPUT_BUFFER_OFFSET);
  }

  while (!is_end_tag(dma.current_tag(), dma.current_tag_vif0(), dma.current_tag_vif1())) {
    auto data = dma.read_and_advance();
    auto v0 = data.vifcode0();
    auto v1 = data.vifcode1();

    if (v0.kind == VifCode::Kind::STCYCL && v1.kind == VifCode::Kind::UNPACK_V4_32) {
      ASSERT(v0.immediate == 0x404);
      auto up = VifCodeUnpack(v1);
      u16 addr = up.addr_qw + (up.use_tops_flag ? get_upload_buffer() : 0);
      ASSERT(addr + v1.num <= 1024);
      memcpy(m_vu_data + addr, data.data, 16 * v1.num);
    } else if (v0.kind == VifCode::Kind::MSCALF && v1.kind == VifCode::Kind::STMOD) {
      ASSERT(v1.immediate == 0);
      switch (v0.immediate) {
        case 0:
          run_call0_vu2c();
          break;
        case 39:
          run_call39_vu2c(render_state, prof);
          break;
        default:
          fmt::print("unknown ocean near call: {}\n", v0.immediate);
          ASSERT(false);
      }
    }
  }

  while (dma.current_tag_offset() != render_state->next_bucket) {
    dma.read_and_advance();
  }
  m_direct.flush_pending(render_state, prof);
}