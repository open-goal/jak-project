#include "OceanNear.h"

#include "common/log/log.h"

#include "third-party/imgui/imgui.h"

OceanNear::OceanNear(const std::string& name, int my_id)
    : BucketRenderer(name, my_id), m_texture_renderer(false) {
  for (auto& a : m_vu_data) {
    a.fill(0);
  }
}

void OceanNear::draw_debug_window() {}

void OceanNear::init_textures(TexturePool& pool, GameVersion version) {
  m_texture_renderer.init_textures(pool, version);
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

  switch (render_state->version) {
    case GameVersion::Jak1:
      render_jak1(dma, render_state, prof);
      break;
    case GameVersion::Jak2:
    case GameVersion::Jak3:
      render_jak2(dma, render_state, prof);
      break;
  }
}

void OceanNear::render_jak1(DmaFollower& dma,
                            SharedRenderState* render_state,
                            ScopedProfilerNode& prof) {
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
    m_texture_renderer.handle_ocean_texture_jak1(dma, render_state, p);
  }

  if (dma.current_tag().qwc != 2) {
    lg::error("abort OceanNear::render!");
    while (dma.current_tag_offset() != render_state->next_bucket) {
      dma.read_and_advance();
    }
    return;
  }

  // direct setup
  {
    m_common_ocean_renderer.init_for_near();
    auto setup = dma.read_and_advance();
    ASSERT(setup.vifcode0().kind == VifCode::Kind::NOP);
    ASSERT(setup.vifcode1().kind == VifCode::Kind::DIRECT);
    ASSERT(setup.size_bytes == 32);
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
          run_call39_vu2c();
          break;
        default:
          ASSERT_MSG(false, fmt::format("unknown ocean near call: {}", v0.immediate));
      }
    }
  }

  while (dma.current_tag_offset() != render_state->next_bucket) {
    dma.read_and_advance();
  }

  m_common_ocean_renderer.flush_near(render_state, prof);
}

void OceanNear::render_jak2(DmaFollower& dma,
                            SharedRenderState* render_state,
                            ScopedProfilerNode& prof) {
  // jump to bucket
  auto data0 = dma.read_and_advance();
  ASSERT(data0.vif1() == 0 || data0.vifcode1().kind == VifCode::Kind::NOP);
  ASSERT(data0.vif0() == 0 || data0.vifcode0().kind == VifCode::Kind::MARK);
  ASSERT(data0.size_bytes == 0);

  // see if bucket is empty or not
  if (dma.current_tag_offset() == render_state->next_bucket) {
    // fmt::print("ocean-near: early exit!\n");
    return;
  }

  {
    auto p = prof.make_scoped_child("texture");
    m_texture_renderer.handle_ocean_texture_jak2(dma, render_state, p);
  }

  if (dma.current_tag().qwc != 2) {
    lg::error("abort OceanNear::render!");
    while (dma.current_tag_offset() != render_state->next_bucket) {
      dma.read_and_advance();
    }
    return;
  }

  // direct setup
  {
    m_common_ocean_renderer.init_for_near();
    auto setup = dma.read_and_advance();
    ASSERT(setup.vifcode0().kind == VifCode::Kind::NOP);
    ASSERT(setup.vifcode1().kind == VifCode::Kind::DIRECT);
    ASSERT(setup.size_bytes == 32);
  }

  // offset and base
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
          run_call0_vu2c_jak2();
          break;
        case 39:
          run_call39_vu2c_jak2();
          break;
        default:
          ASSERT_MSG(false, fmt::format("unknown ocean near call: {}", v0.immediate));
      }
    }
  }

  while (dma.current_tag_offset() != render_state->next_bucket) {
    dma.read_and_advance();
  }

  m_common_ocean_renderer.flush_near(render_state, prof);
}

void OceanNear::xgkick(u16 addr) {
  m_common_ocean_renderer.kick_from_near((const u8*)&m_vu_data[addr]);
}
