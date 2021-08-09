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

  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.st_buffer);
  m_ogl.st_buffer_bytes = batch_size * 3 * 2 * sizeof(float);
  glBufferData(GL_ARRAY_BUFFER, m_ogl.st_buffer_bytes, nullptr, GL_DYNAMIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
}

DirectRenderer::~DirectRenderer() {
  glDeleteBuffers(1, &m_ogl.color_buffer);
  glDeleteBuffers(1, &m_ogl.vertex_buffer);
  glDeleteBuffers(1, &m_ogl.st_buffer);
}

/*!
 * Render from a DMA bucket.
 */
void DirectRenderer::render(DmaFollower& dma, SharedRenderState* render_state) {
  //  fmt::print("direct: {}\n", m_my_id);
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
      //      reset_state();
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

  // lg::warn("DirectRenderer flush with {} triangles.", m_prim_buffer.vert_count / 3);
  // update opengl state
  if (m_prim_gl_state_needs_gl_update) {
    update_gl_prim(render_state);
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

  // hacks
  //  glEnable(GL_DEPTH_TEST);
  //  glDepthFunc(GL_ALWAYS);

  // render!
  // update buffers:
  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.vertex_buffer);
  glBufferSubData(GL_ARRAY_BUFFER, 0, m_ogl.vertex_buffer_bytes, m_prim_buffer.verts.data());
  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.color_buffer);
  glBufferSubData(GL_ARRAY_BUFFER, 0, m_ogl.color_buffer_bytes, m_prim_buffer.rgba_u8.data());
  if (m_prim_gl_state.texture_enable) {
    glBindBuffer(GL_ARRAY_BUFFER, m_ogl.st_buffer);
    glBufferSubData(GL_ARRAY_BUFFER, 0, m_ogl.st_buffer_bytes, m_prim_buffer.sts.data());
  }

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

  if (m_prim_gl_state.texture_enable) {
    glBindBuffer(GL_ARRAY_BUFFER, m_ogl.st_buffer);
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(2,         // location 0 in the shader
                          2,         // 3 floats per vert
                          GL_FLOAT,  // floats
                          GL_FALSE,  // normalized, ignored,
                          0,         // tightly packed
                          0);
    glActiveTexture(GL_TEXTURE0);
  }
  // assert(false);
  glDrawArrays(GL_TRIANGLES, 0, m_prim_buffer.vert_count);
  m_prim_buffer.vert_count = 0;
}

void DirectRenderer::update_gl_prim(SharedRenderState* render_state) {
  // currently gouraud is handled in setup.
  const auto& state = m_prim_gl_state;
  if (state.texture_enable) {
    render_state->shaders[ShaderId::DIRECT_BASIC_TEXTURED].activate();
    update_gl_texture(render_state);
  } else {
    render_state->shaders[ShaderId::DIRECT_BASIC].activate();
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

void DirectRenderer::upload_texture(TextureRecord* tex) {
  assert(!tex->on_gpu);
  GLuint tex_id;
  glGenTextures(1, &tex_id);
  tex->gpu_texture = tex_id;
  glBindTexture(GL_TEXTURE_2D, tex_id);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tex->w, tex->h, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
               tex->data.data());
  glBindTexture(GL_TEXTURE_2D, 0);
  tex->on_gpu = true;
}

void DirectRenderer::update_gl_texture(SharedRenderState* render_state) {
  auto tex = render_state->texture_pool->lookup(m_texture_state.texture_base_ptr);
  assert(tex);
  // fmt::print("Successful texture lookup! {} {}\n", tex->page_name, tex->name);

  // first: do we need to load the texture?
  if (!tex->on_gpu) {
    upload_texture(tex);
  }

  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, tex->gpu_texture);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glUniform1i(
      glGetUniformLocation(render_state->shaders[ShaderId::DIRECT_BASIC_TEXTURED].id(), "T0"), 0);
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
  assert(size >= 16);
  bool eop = false;

  u32 offset = 0;
  while (!eop) {
    GifTag tag(data + offset);
    offset += 16;
    //    fmt::print("Tag: {}\n", tag.print());

    // unpack registers.
    // faster to do it once outside of the nloop loop.
    GifTag::RegisterDescriptor reg_desc[16];
    u32 nreg = tag.nreg();
    for (u32 i = 0; i < nreg; i++) {
      reg_desc[i] = tag.reg(i);
    }

    auto format = tag.flg();
    if (format == GifTag::Format::PACKED) {
      if (tag.pre()) {
        handle_prim(tag.prim(), render_state);
      }
      for (u32 loop = 0; loop < tag.nloop(); loop++) {
        for (u32 reg = 0; reg < nreg; reg++) {
          switch (reg_desc[reg]) {
            case GifTag::RegisterDescriptor::AD:
              handle_ad(data + offset, render_state);
              break;
            case GifTag::RegisterDescriptor::ST:
              handle_st_packed(data + offset);
              break;
            case GifTag::RegisterDescriptor::RGBAQ:
              handle_rgbaq_packed(data + offset);
              break;
            case GifTag::RegisterDescriptor::XYZF2:
              handle_xyzf2_packed(data + offset);
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
          //          fmt::print("loop: {} reg: {} {}\n", loop, reg,
          //          reg_descriptor_name(reg_desc[reg]));
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

  //  fmt::print("{}\n", GifTag(data).print());
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
    case GsRegisterAddress::PRIM:
      handle_prim(value, render_state);
      break;

    case GsRegisterAddress::TEX1_1:
      handle_tex1_1(value);
      break;
    case GsRegisterAddress::TEXA:
      handle_texa(value);
      break;
    case GsRegisterAddress::TEXCLUT:
      // TODO
      // the only thing the direct renderer does with texture is font, which does no tricks with
      // CLUT. The texture upload process will do all of the lookups with the default CLUT.
      // So we'll just assume that the TEXCLUT is set properly and ignore this.
      break;
    case GsRegisterAddress::FOGCOL:
      // TODO
      break;
    case GsRegisterAddress::TEX0_1:
      handle_tex0_1(value, render_state);
      break;
    default:
      fmt::print("Address {} is not supported\n", register_address_name(addr));
      assert(false);
  }
}

void DirectRenderer::handle_tex1_1(u64 val) {
  GsTex1 reg(val);
  // for now, we aren't going to handle mipmapping. I don't think it's used with direct.
  assert(reg.mxl() == 0);
  // if that's true, we can ignore LCM, MTBA, L, K

  // MMAG/MMIN specify texture filtering. For now, assume always linear
  assert(reg.mmag() == true);
  assert(reg.mmin() == 1);

  // fmt::print("{}\n", reg.print());
}

void DirectRenderer::handle_tex0_1(u64 val, SharedRenderState* render_state) {
  GsTex0 reg(val);
  //  fmt::print("{}\n", reg.print());

  // update tbp
  if (m_texture_state.current_register != reg) {
    flush_pending(render_state);
    m_texture_state.texture_base_ptr = reg.tbp0();
    m_texture_state_needs_gl_update = true;
    m_texture_state.current_register = reg;
  }

  // tbw: assume they got it right
  // psm: assume they got it right
  // tw: assume they got it right
  // th: assume they got it right

  // these mean that the texture is multiplied, and uses the alpha from the clut.
  assert(reg.tcc() == 1);
  assert(reg.tfx() == GsTex0::TextureFunction::MODULATE);

  // cbp: assume they got it right
  // cpsm: assume they got it right
  // csm: assume they got it right
}

void DirectRenderer::handle_texa(u64 val) {
  GsTexa reg(val);

  // rgba16 isn't used so this doesn't matter?
  // but they use sane defaults anyway
  assert(reg.ta0() == 0);
  assert(reg.ta1() == 0x80);

  assert(reg.aem() == false);
}

void DirectRenderer::handle_st_packed(const u8* data) {
  memcpy(&m_prim_building.st_reg.x(), data + 0, 4);
  memcpy(&m_prim_building.st_reg.y(), data + 4, 4);
  memcpy(&m_prim_building.Q, data + 8, 4);
}

void DirectRenderer::handle_rgbaq_packed(const u8* data) {
  // TODO update Q from st.
  m_prim_building.rgba_reg[0] = data[0];
  m_prim_building.rgba_reg[1] = data[4];
  m_prim_building.rgba_reg[2] = data[8];
  m_prim_building.rgba_reg[3] = data[12];
}

float u32_to_float(u32 in) {
  double x = (double)in / UINT32_MAX;
  return x * 2 - 1;
}

void DirectRenderer::handle_xyzf2_packed(const u8* data) {
  u32 x, y;
  memcpy(&x, data, 4);
  memcpy(&y, data + 4, 4);

  u64 upper;
  memcpy(&upper, data + 8, 8);
  u32 z = (upper >> 4) & 0xffffff;
  u8 f = (upper >> 36);
  bool adc = upper & (1ull << 47);
  assert(!adc);
  assert(!f);
  handle_xyzf2_common(x, y, z, f);
}

void debug_print_vtx(const math::Vector<u32, 3>& vtx) {
  fmt::print("{} {}\n", u32_to_float(vtx.x()), u32_to_float(vtx.y()));
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
  if (m_test_state.current_register != reg) {
    flush_pending(render_state);
    m_test_state.from_register(reg);
    m_test_state_needs_gl_update = true;
  }
}

void DirectRenderer::handle_alpha1(u64 val, SharedRenderState* render_state) {
  GsAlpha reg(val);
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
  if (m_prim_building.tri_strip_startup) {
    m_prim_building.tri_strip_startup = 0;
    m_prim_building.building_idx = 0;
  } else {
    if (m_prim_building.building_idx > 0) {
      assert(false);  // shouldn't leave any half-finished prims
    }
  }
  // need to flush any in progress prims to the buffer.

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
}

void DirectRenderer::handle_xyzf2_common(u32 x, u32 y, u32 z, u8 f) {
  assert(f == 0);
  m_prim_building.building_st.at(m_prim_building.building_idx) = m_prim_building.st_reg;
  m_prim_building.building_rgba.at(m_prim_building.building_idx) = m_prim_building.rgba_reg;
  m_prim_building.building_vert.at(m_prim_building.building_idx) = {x << 16, y << 16, z};
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

        m_prim_buffer.push(corner1_rgba, corner1_vert, {});
        m_prim_buffer.push(corner3_rgba, corner3_vert, {});
        m_prim_buffer.push(corner2_rgba, corner2_vert, {});
        m_prim_buffer.push(corner2_rgba, corner2_vert, {});
        m_prim_buffer.push(corner4_rgba, corner4_vert, {});
        m_prim_buffer.push(corner1_rgba, corner1_vert, {});
        m_prim_building.building_idx = 0;
      }
    } break;
    case GsPrim::Kind::TRI_STRIP: {
      if (m_prim_building.building_idx == 3) {
        m_prim_building.building_idx = 0;
      }

      if (m_prim_building.tri_strip_startup < 3) {
        m_prim_building.tri_strip_startup++;
      }
      if (m_prim_building.tri_strip_startup >= 3) {
        for (int i = 0; i < 3; i++) {
          m_prim_buffer.push(m_prim_building.building_rgba[i], m_prim_building.building_vert[i],
                             m_prim_building.building_st[i]);
        }
      }

    } break;
      //    case GsPrim::Kind::LINE: {
      //      if (m_prim_building.building_idx == 1) {
      //        math::Vector<double, 3> pt0 = m_prim_building.building_vert[0].cast<double>();
      //        math::Vector<double, 3> pt1 = m_prim_building.building_vert[1].cast<double>();
      //        auto normal = (pt1 - pt0).normalized().cross({0, 0, 1});
      //
      //        double line_width = (1 << 28);
      //        fmt::print("Line:\n ");
      //        fmt::print(" {} {} {} {}\n", m_prim_building.building_vert[0].x(),
      //                   m_prim_building.building_vert[0].y(),
      //                   m_prim_building.building_vert[1].x(),
      //                   m_prim_building.building_vert[1].y());
      //        //        debug_print_vtx(m_prim_building.building_vert[0]);
      //        //        debug_print_vtx(m_prim_building.building_vert[1]);
      //
      //        math::Vector<double, 3> a = pt0 + normal * line_width;
      //        math::Vector<double, 3> b = pt1 + normal * line_width;
      //        math::Vector<double, 3> c = pt0 - normal * line_width;
      //        math::Vector<double, 3> d = pt1 - normal * line_width;
      //
      //        // ACB:
      //        m_prim_buffer.push(m_prim_building.building_rgba[0], a.cast<u32>(), {});
      //        m_prim_buffer.push(m_prim_building.building_rgba[0], c.cast<u32>(), {});
      //        m_prim_buffer.push(m_prim_building.building_rgba[1], b.cast<u32>(), {});
      //        // b c d
      //        m_prim_buffer.push(m_prim_building.building_rgba[1], b.cast<u32>(), {});
      //        m_prim_buffer.push(m_prim_building.building_rgba[0], c.cast<u32>(), {});
      //        m_prim_buffer.push(m_prim_building.building_rgba[1], d.cast<u32>(), {});
      //        //
      //
      //        m_prim_building.building_idx = 0;
      //      }
      //    } break;
    default:
      fmt::print("prim type {} is unsupported.\n", (int)m_prim_building.kind);
      assert(false);
  }
}

void DirectRenderer::handle_xyzf2(u64 val, SharedRenderState* render_state) {
  if (m_prim_buffer.is_full()) {
    fmt::print("update from full\n");
    flush_pending(render_state);
  }

  // m_prim_buffer.rgba_u8[m_prim_buffer.vert_count] = m_prim_building.rgba;

  u32 x = val & 0xffff;
  u32 y = (val >> 16) & 0xffff;
  u32 z = (val >> 32) & 0xfffff;
  u32 f = (val >> 56) & 0xff;

  handle_xyzf2_common(x, y, z, f);
}

void DirectRenderer::reset_state() {
  m_test_state_needs_gl_update = true;
  m_test_state = TestState();

  m_blend_state_needs_gl_update = true;
  m_blend_state = BlendState();

  m_prim_gl_state_needs_gl_update = true;
  m_prim_gl_state = PrimGlState();

  m_texture_state_needs_gl_update = true;
  m_texture_state = TextureState();

  m_prim_building = PrimBuildState();
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
  sts.resize(max_triangles * 3);
  max_verts = max_triangles * 3;
}

void DirectRenderer::PrimitiveBuffer::push(const math::Vector<u8, 4>& rgba,
                                           const math::Vector<u32, 3>& vert,
                                           const math::Vector<float, 2>& st) {
  rgba_u8[vert_count] = rgba;
  verts[vert_count] = vert;
  sts[vert_count] = st;
  vert_count++;
}