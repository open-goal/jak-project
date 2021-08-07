#include "DirectRenderer.h"
#include "game/graphics/dma/gs.h"
#include "common/log/log.h"
#include "third-party/fmt/core.h"
#include "game/graphics/opengl.h"

DirectRenderer::DirectRenderer(const std::string& name, BucketId my_id, int batch_size)
    : BucketRenderer(name, my_id), m_prim_buffer(batch_size) {
  glGenBuffers(1, &m_ogl.vertex_buffer);
  glGenBuffers(1, &m_ogl.color_buffer);

  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.vertex_buffer);
  m_ogl.vertex_buffer_bytes = batch_size * 3 * 3 * sizeof(u32);
  glBufferData(GL_ARRAY_BUFFER, m_ogl.vertex_buffer_bytes, nullptr, GL_DYNAMIC_DRAW);

  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.color_buffer);
  m_ogl.color_buffer_bytes = batch_size * 3 * 4 * sizeof(u8);
  glBufferData(GL_ARRAY_BUFFER, m_ogl.color_buffer_bytes, nullptr, GL_DYNAMIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
}

DirectRenderer::~DirectRenderer() {
  glDeleteBuffers(1, &m_ogl.color_buffer);
  glDeleteBuffers(1, &m_ogl.vertex_buffer);
}

/*!
 * Render from a DMA bucket.
 */
void DirectRenderer::render(DmaFollower& dma, SharedRenderState* render_state) {
  fmt::print("direct: {}\n", m_my_id);
  // if we're rendering from a bucket, we should start off we a totally reset state:
  reset_state();
  setup_common_state(render_state);

  // just dump the DMA data into the other the render function
  while (dma.current_tag_offset() != render_state->next_bucket) {
    auto data = dma.read_and_advance();
    if (data.size_bytes) {
      render_vif(data.vif0(), data.vif1(), data.data, data.size_bytes, render_state);
    }

    if (dma.current_tag_offset() == render_state->default_regs_buffer) {
      reset_state();
      dma.read_and_advance();  // cnt
      assert(dma.current_tag().kind == DmaTag::Kind::RET);
      dma.read_and_advance();  // ret
    }
  }

  flush_pending(render_state);
}

void DirectRenderer::flush_pending(SharedRenderState* render_state) {
  if (m_prim_buffer.vert_count == 0) {
    return;
  }

  lg::warn("DirectRenderer flush with {} triangles.\n", m_prim_buffer.vert_count / 3);
  // update opengl state
  if (m_prim_gl_state_needs_gl_update) {
    update_gl_prim();
    m_prim_gl_state_needs_gl_update = false;
  }

  if (m_blend_state_needs_gl_update) {
    update_gl_blend();
    m_blend_state_needs_gl_update = false;
  }

  if (m_test_state_needs_gl_update) {
    update_gl_test();
    m_test_state_needs_gl_update = false;
  }

   u32* data = ( u32*)m_prim_buffer.verts.data();



  // render!
  // update buffers:
  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.vertex_buffer);
  glBufferSubData(GL_ARRAY_BUFFER, 0, m_ogl.vertex_buffer_bytes, m_prim_buffer.verts.data());
  // glBufferSubData(GL_ARRAY_BUFFER, 0, m_ogl.vertex_buffer_bytes, verts);
  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.color_buffer);
  glBufferSubData(GL_ARRAY_BUFFER, 0, m_ogl.color_buffer_bytes, m_prim_buffer.rgba_u8.data());

  // setup attributes:

  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.vertex_buffer);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0,                // location 0 in the shader
                        3,                // 3 floats per vert
                        GL_UNSIGNED_INT,  // floats
                        GL_TRUE,          // normalized, ignored,
                        0,                // tightly packed
                        0                 // offset in array (why is is this a pointer...)
  );

  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.color_buffer);
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(1,                 // location 0 in the shader
                        4,                 // 3 floats per vert
                        GL_UNSIGNED_BYTE,  // floats
                        GL_TRUE,           // normalized, ignored,
                        0,                 // tightly packed
                        0);
  // assert(false);
  glDrawArrays(GL_TRIANGLES, 0, m_prim_buffer.vert_count);
  m_prim_buffer.vert_count = 0;
}

void DirectRenderer::update_gl_prim() {
  // currently gouraud is handled in setup.
  const auto& state = m_prim_gl_state;
  if (state.texture_enable) {
    assert(false);
  }
  if (state.fogging_enable) {
    assert(false);
  }
  if (state.aa_enable) {
    assert(false);
  }
  if (state.use_uv) {
    assert(false);
  }
  if (state.ctxt) {
    assert(false);
  }
  if (state.fix) {
    assert(false);
  }
}

void DirectRenderer::update_gl_blend() {
  const auto& state = m_blend_state;
  if (state.a == GsAlpha::BlendMode::SOURCE && state.b == GsAlpha::BlendMode::DEST &&
      state.c == GsAlpha::BlendMode::SOURCE && state.d == GsAlpha::BlendMode::DEST) {
    // (Cs - Cd) * As + Cd
    // Cs * As  + (1 - As) * Cd
    glEnable(GL_BLEND);
    // s, d
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  } else {
    fmt::print("unsupported blend\n");
    assert(false);
  }
}

void DirectRenderer::update_gl_test() {
  const auto& state = m_test_state;
  if (state.zte) {
    switch (state.ztst) {
      case GsTest::ZTest::NEVER:
        glDepthFunc(GL_NEVER);
        break;
      case GsTest::ZTest::ALWAYS:
        glDepthFunc(GL_ALWAYS);
        break;
      case GsTest::ZTest::GEQUAL:
        glDepthFunc(GL_GEQUAL);
        break;
      case GsTest::ZTest::GREATER:
        glDepthFunc(GL_GREATER);
        break;
      default:
        assert(false);
    }
  } else {
    assert(false);
  }

  if (state.date) {
    assert(false);
  }

  if (state.alpha_test_enable) {
    assert(false);
  }
}

void DirectRenderer::setup_common_state(SharedRenderState* render_state) {
  // todo texture clamp.
  render_state->shaders[ShaderId::DIRECT_BASIC].activate();
}

namespace {
/*!
 * If it's a direct, returns the qwc.
 * If it's ignorable (nop, flush), returns 0.
 * Otherwise, assert.
 */
u32 get_direct_qwc_or_nop(const VifCode& code) {
  switch (code.kind) {
    case VifCode::Kind::NOP:
    case VifCode::Kind::FLUSHA:
      return 0;
    case VifCode::Kind::DIRECT:
      if (code.immediate == 0) {
        return 65536;
      } else {
        return code.immediate;
      }
    default:
      assert(false);
  }
}
}  // namespace

/*!
 * Render VIF data.
 */
void DirectRenderer::render_vif(u32 vif0,
                                u32 vif1,
                                const u8* data,
                                u32 size,
                                SharedRenderState* render_state) {
  // here we process VIF data. Basically we just go forward, looking for DIRECTs.
  // We skip stuff like flush and nops.

  // read the vif cmds at the front.
  u32 gif_qwc = get_direct_qwc_or_nop(VifCode(vif0));
  if (gif_qwc) {
    // we got a direct. expect the second thing to be a nop/similar.
    assert(get_direct_qwc_or_nop(VifCode(vif1)) == 0);
  } else {
    gif_qwc = get_direct_qwc_or_nop(VifCode(vif1));
  }

  u32 offset_into_data = 0;
  while (offset_into_data < size) {
    if (gif_qwc) {
      if (offset_into_data & 0xf) {
        // not aligned. should get nops.
        u32 vif;
        memcpy(&vif, data + offset_into_data, 4);
        offset_into_data += 4;
        assert(get_direct_qwc_or_nop(VifCode(vif)) == 0);
      } else {
        // aligned! do a gif transfer!
        render_gif(data + offset_into_data, gif_qwc * 16, render_state);
        offset_into_data += gif_qwc * 16;
      }
    } else {
      // we are reading VIF data.
      u32 vif;
      memcpy(&vif, data + offset_into_data, 4);
      offset_into_data += 4;
      gif_qwc = get_direct_qwc_or_nop(VifCode(vif));
    }
  }
}

/*!
 * Render GIF data.
 */
void DirectRenderer::render_gif(const u8* data, u32 size, SharedRenderState* render_state) {
  fmt::print("Render GIF: {}\n", size);
  if (size == 224) {
    lg::error("Skipping 224 sized GIF render.\n");
    return;
  }
  assert(size >= 16);
  bool eop = false;

  u32 offset = 0;
  while (!eop) {
    GifTag tag(data + offset);
    offset += 16;
    fmt::print("Tag: {}\n", tag.print());

    // unpack registers.
    // faster to do it once outside of the nloop loop.
    GifTag::RegisterDescriptor reg_desc[16];
    u32 nreg = tag.nreg();
    for (u32 i = 0; i < nreg; i++) {
      reg_desc[i] = tag.reg(i);
    }

    auto format = tag.flg();
    if (format == GifTag::Format::PACKED) {
      for (u32 loop = 0; loop < tag.nloop(); loop++) {
        for (u32 reg = 0; reg < nreg; reg++) {
          switch (reg_desc[reg]) {
            case GifTag::RegisterDescriptor::AD:
              handle_ad(data + offset, render_state);
              break;
            default:
              fmt::print("Register {} is not supported in packed mode yet\n",
                         reg_descriptor_name(reg_desc[reg]));
              assert(false);
          }
          offset += 16;  // PACKED = quadwords
        }
      }
    } else if (format == GifTag::Format::REGLIST) {
      for (u32 loop = 0; loop < tag.nloop(); loop++) {
        for (u32 reg = 0; reg < nreg; reg++) {
          u64 register_data;
          memcpy(&register_data, data + offset, 8);
          fmt::print("loop: {} reg: {} {}\n", loop, reg, reg_descriptor_name(reg_desc[reg]));
          switch (reg_desc[reg]) {
            case GifTag::RegisterDescriptor::PRIM:
              handle_prim(register_data, render_state);
              break;
            case GifTag::RegisterDescriptor::RGBAQ:
              handle_rgbaq(register_data);
              break;
            case GifTag::RegisterDescriptor::XYZF2:
              handle_xyzf2(register_data, render_state);
              break;
            default:
              fmt::print("Register {} is not supported in reglist mode yet\n",
                         reg_descriptor_name(reg_desc[reg]));
              assert(false);
          }
          offset += 8;  // PACKED = quadwords
        }
      }
    } else {
      assert(false);  // format not packed or reglist.
    }

    eop = tag.eop();
  }

  assert(offset == size);

  fmt::print("{}\n", GifTag(data).print());
}

void DirectRenderer::handle_ad(const u8* data, SharedRenderState* render_state) {
  u64 value;
  GsRegisterAddress addr;
  memcpy(&value, data, sizeof(u64));
  memcpy(&addr, data + 8, sizeof(GsRegisterAddress));

  switch (addr) {
    case GsRegisterAddress::ZBUF_1:
      handle_zbuf1(value);
      break;
    case GsRegisterAddress::TEST_1:
      handle_test1(value, render_state);
      break;
    case GsRegisterAddress::ALPHA_1:
      handle_alpha1(value, render_state);
      break;
    case GsRegisterAddress::PABE:
      handle_pabe(value);
      break;
    case GsRegisterAddress::CLAMP_1:
      handle_clamp1(value);
      break;

      // for now, ignore textures/fog.
    case GsRegisterAddress::TEX1_1:
    case GsRegisterAddress::TEXA:
    case GsRegisterAddress::TEXCLUT:
    case GsRegisterAddress::FOGCOL:
      break;
    default:
      fmt::print("Address {} is not supported\n", register_address_name(addr));
      assert(false);
  }
}

void DirectRenderer::handle_zbuf1(u64 val) {
  // note: we can basically ignore this. There's a single z buffer that's always configured the same
  // way - 24-bit, at offset 448.
  GsZbuf x(val);
  assert(x.zmsk());  // note: not sure if this ever changes or not.
  assert(x.psm() == TextureFormat::PSMZ24);
  assert(x.zbp() == 448);
}

void DirectRenderer::handle_test1(u64 val, SharedRenderState* render_state) {
  GsTest reg(val);
  fmt::print("test: {}\n", reg.print());
  if (m_test_state.current_register != reg) {
    flush_pending(render_state);
    m_test_state.from_register(reg);
    m_test_state_needs_gl_update = true;
  }
}

void DirectRenderer::handle_alpha1(u64 val, SharedRenderState* render_state) {
  GsAlpha reg(val);
  fmt::print("alpha: {}\n", reg.print());
  if (m_blend_state.current_register != reg) {
    flush_pending(render_state);
    m_blend_state.from_register(reg);
    m_blend_state_needs_gl_update = true;
  }
}

void DirectRenderer::handle_pabe(u64 val) {
  assert(val == 0);  // not really sure how to handle this yet.
}

void DirectRenderer::handle_clamp1(u64 val) {
  assert(val == 0b101);  // clamp s and t.
}

void DirectRenderer::handle_prim(u64 val, SharedRenderState* render_state) {
  fmt::print("got prim: 0x{:x}\n", val);

  // need to flush any in progress prims to the buffer.
  if (m_prim_building.building_idx > 0) {
    assert(false);  // shouldn't leave any half-finished prims
  }

  GsPrim prim(val);
  if (m_prim_gl_state.current_register != prim || m_blend_state.alpha_blend_enable != prim.abe()) {
    flush_pending(render_state);
    m_prim_gl_state.from_register(prim);
    m_blend_state.alpha_blend_enable = prim.abe();
    m_prim_gl_state_needs_gl_update = true;
    m_blend_state_needs_gl_update = true;
  }

  m_prim_building.kind = prim.kind();
}

void DirectRenderer::handle_rgbaq(u64 val) {
  assert((val >> 32) == 0);  // q = 0
  memcpy(m_prim_building.rgba_reg.data(), &val, 4);
  fmt::print("a = 0x{:x}\n", m_prim_building.rgba_reg[3]);
}

void DirectRenderer::handle_xyzf2(u64 val, SharedRenderState* render_state) {
  if (m_prim_buffer.is_full()) {
    flush_pending(render_state);
  }

  m_prim_building.building_rgba[m_prim_building.building_idx] = m_prim_building.rgba_reg;

  // m_prim_buffer.rgba_u8[m_prim_buffer.vert_count] = m_prim_building.rgba;

  u32 x = val & 0xffff;
  u32 y = (val >> 16) & 0xffff;
  u32 z = (val >> 32) & 0xfffff;
  u32 f = (val >> 56) & 0xff;

  assert(f == 0);
  m_prim_building.building_vert[m_prim_building.building_idx] = {x << 16, y << 16, z};
  m_prim_building.building_idx++;

  switch (m_prim_building.kind) {
    case GsPrim::Kind::SPRITE: {
      if (m_prim_building.building_idx == 2) {
        // build triangles from the sprite.
        auto& corner1_vert = m_prim_building.building_vert[0];
        auto& corner1_rgba = m_prim_building.building_rgba[0];
        auto& corner2_vert = m_prim_building.building_vert[1];
        auto& corner2_rgba = m_prim_building.building_rgba[1];
        // should use most recent vertex z.
        math::Vector<u32, 3> corner3_vert = {corner1_vert[0], corner2_vert[1], corner2_vert[2]};
        math::Vector<u32, 3> corner4_vert = {corner2_vert[0], corner1_vert[1], corner2_vert[2]};

        if (m_prim_gl_state.gouraud_enable) {
          // I'm not really sure what the GS does here.
          assert(false);
        }
        auto& corner3_rgba = corner2_rgba;
        auto& corner4_rgba = corner2_rgba;

        m_prim_buffer.push(corner1_rgba, corner1_vert);
        m_prim_buffer.push(corner3_rgba, corner3_vert);
        m_prim_buffer.push(corner2_rgba, corner2_vert);
        m_prim_buffer.push(corner2_rgba, corner2_vert);
        m_prim_buffer.push(corner4_rgba, corner4_vert);
        m_prim_buffer.push(corner1_rgba, corner1_vert);
        m_prim_building.building_idx = 0;
      }
    } break;
    default:
      fmt::print("prim type {} is unsupported.\n", (int)m_prim_building.kind);
      assert(false);
  }
}

void DirectRenderer::reset_state() {
  m_test_state_needs_gl_update = true;
  m_test_state = TestState();

  m_blend_state_needs_gl_update = true;
  m_blend_state = BlendState();

  m_prim_gl_state_needs_gl_update = true;
  m_prim_gl_state = PrimGlState();
}

void DirectRenderer::TestState::from_register(GsTest reg) {
  current_register = reg;
  alpha_test_enable = reg.alpha_test_enable();
  if (alpha_test_enable) {
    alpha_test = reg.alpha_test();
    aref = reg.aref();
    afail = reg.afail();
  }

  date = reg.date();
  if (date) {
    datm = reg.datm();
  }

  zte = reg.zte();
  if (zte) {
    ztst = reg.ztest();
  }
}

void DirectRenderer::BlendState::from_register(GsAlpha reg) {
  current_register = reg;
  a = reg.a_mode();
  b = reg.b_mode();
  c = reg.c_mode();
  d = reg.d_mode();
  fix = reg.fix();
}

void DirectRenderer::PrimGlState::from_register(GsPrim reg) {
  current_register = reg;
  gouraud_enable = reg.gouraud();
  texture_enable = reg.tme();
  fogging_enable = reg.fge();
  aa_enable = reg.aa1();
  use_uv = reg.fst();
  ctxt = reg.ctxt();
  fix = reg.fix();
}

DirectRenderer::PrimitiveBuffer::PrimitiveBuffer(int max_triangles) {
  rgba_u8.resize(max_triangles * 3);
  verts.resize(max_triangles * 3);
  max_verts = max_triangles * 3;
}

void DirectRenderer::PrimitiveBuffer::push(const math::Vector<u8, 4>& rgba,
                                           const math::Vector<u32, 3>& vert) {
  rgba_u8[vert_count] = rgba;
  verts[vert_count] = vert;
  vert_count++;
}