#include "DirectRenderer.h"
#include "common/dma/gs.h"
#include "common/log/log.h"
#include "third-party/fmt/core.h"
#include "game/graphics/pipelines/opengl.h"
#include "third-party/imgui/imgui.h"

DirectRenderer::DirectRenderer(const std::string& name, BucketId my_id, int batch_size, Mode mode)
    : BucketRenderer(name, my_id), m_prim_buffer(batch_size), m_mode(mode) {
  glGenBuffers(1, &m_ogl.vertex_buffer);
  glGenBuffers(1, &m_ogl.color_buffer);
  glGenBuffers(1, &m_ogl.st_buffer);
  glGenVertexArrays(1, &m_ogl.vao);

  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.vertex_buffer);
  m_ogl.vertex_buffer_bytes = batch_size * 3 * 3 * sizeof(u32);
  glBufferData(GL_ARRAY_BUFFER, m_ogl.vertex_buffer_bytes, nullptr, GL_DYNAMIC_DRAW);

  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.color_buffer);
  m_ogl.color_buffer_bytes = batch_size * 3 * 4 * sizeof(u8);
  glBufferData(GL_ARRAY_BUFFER, m_ogl.color_buffer_bytes, nullptr, GL_DYNAMIC_DRAW);

  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.st_buffer);
  m_ogl.st_buffer_bytes = batch_size * 3 * 3 * sizeof(float);
  glBufferData(GL_ARRAY_BUFFER, m_ogl.st_buffer_bytes, nullptr, GL_DYNAMIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
}

DirectRenderer::~DirectRenderer() {
  glDeleteBuffers(1, &m_ogl.color_buffer);
  glDeleteBuffers(1, &m_ogl.vertex_buffer);
  glDeleteBuffers(1, &m_ogl.st_buffer);
  glDeleteVertexArrays(1, &m_ogl.vao);
}

/*!
 * Render from a DMA bucket.
 */
void DirectRenderer::render(DmaFollower& dma,
                            SharedRenderState* render_state,
                            ScopedProfilerNode& prof) {
  // if we're rendering from a bucket, we should start off we a totally reset state:
  reset_state();
  setup_common_state(render_state);

  // just dump the DMA data into the other the render function
  while (dma.current_tag_offset() != render_state->next_bucket) {
    auto data = dma.read_and_advance();
    if (data.size_bytes && m_enabled) {
      render_vif(data.vif0(), data.vif1(), data.data, data.size_bytes, render_state, prof);
    }

    if (dma.current_tag_offset() == render_state->default_regs_buffer) {
      //      reset_state();
      dma.read_and_advance();  // cnt
      assert(dma.current_tag().kind == DmaTag::Kind::RET);
      dma.read_and_advance();  // ret
    }
  }

  if (m_enabled) {
    flush_pending(render_state, prof);
  }
}

void DirectRenderer::draw_debug_window() {
  ImGui::Checkbox("Wireframe", &m_debug_state.wireframe);
  ImGui::SameLine();
  ImGui::Checkbox("No-texture", &m_debug_state.disable_texture);
  ImGui::SameLine();
  ImGui::Checkbox("red", &m_debug_state.red);
  ImGui::SameLine();
  ImGui::Checkbox("always", &m_debug_state.always_draw);

  if (m_mode == Mode::SPRITE_CPU) {
    ImGui::Checkbox("draw1", &m_sprite_mode.do_first_draw);
    ImGui::SameLine();
    ImGui::Checkbox("draw2", &m_sprite_mode.do_second_draw);
  }

  ImGui::Text("Triangles: %d", m_triangles);
  ImGui::SameLine();
  ImGui::Text("Draws: %d", m_draw_calls);
}

float u32_to_float(u32 in) {
  double x = (double)in / UINT32_MAX;
  return x;
}

float u32_to_sc(u32 in) {
  float flt = u32_to_float(in);
  return (flt - 0.5) * 16.0;
}

void DirectRenderer::flush_pending(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  if (m_prim_buffer.vert_count == 0) {
    return;
  }

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

  if (m_texture_state.needs_gl_update) {
    update_gl_texture(render_state);
    m_texture_state.needs_gl_update = false;
  }

  if (m_debug_state.disable_texture) {
    // a bit of a hack, this forces the non-textured shader always.
    render_state->shaders[ShaderId::DIRECT_BASIC].activate();
  }

  if (m_debug_state.red) {
    render_state->shaders[ShaderId::DEBUG_RED].activate();
    glDisable(GL_BLEND);
    m_prim_gl_state_needs_gl_update = true;
    m_blend_state_needs_gl_update = true;
  }

  // hacks
  if (m_debug_state.always_draw) {
    glDisable(GL_DEPTH_TEST);
    glDepthFunc(GL_ALWAYS);
  }

  glBindVertexArray(m_ogl.vao);

  // render!
  // update buffers:
  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.vertex_buffer);
  glBufferSubData(GL_ARRAY_BUFFER, 0, m_prim_buffer.verts.size() * sizeof(math::Vector<u32, 3>),
                  m_prim_buffer.verts.data());
  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.color_buffer);
  glBufferSubData(GL_ARRAY_BUFFER, 0, m_prim_buffer.rgba_u8.size() * sizeof(math::Vector<u8, 4>),
                  m_prim_buffer.rgba_u8.data());
  if (m_prim_gl_state.texture_enable) {
    glBindBuffer(GL_ARRAY_BUFFER, m_ogl.st_buffer);
    glBufferSubData(GL_ARRAY_BUFFER, 0, m_prim_buffer.stqs.size() * sizeof(math::Vector<float, 3>),
                    m_prim_buffer.stqs.data());
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
                          3,         // 3 floats per vert
                          GL_FLOAT,  // floats
                          GL_FALSE,  // normalized, ignored,
                          0,         // tightly packed
                          0);
    glActiveTexture(GL_TEXTURE0);
  }
  // assert(false);

  int draw_count = 0;
  if (m_mode == Mode::SPRITE_CPU) {
    if (!m_prim_gl_state.texture_enable) {
      render_state->shaders[ShaderId::DIRECT_BASIC].activate();
    } else {
      assert(m_texture_state.tcc);
      assert(m_prim_gl_state.texture_enable);
      render_state->shaders[ShaderId::SPRITE_CPU].activate();
    }

    if (m_sprite_mode.do_first_draw) {
      glDrawArrays(GL_TRIANGLES, 0, m_prim_buffer.vert_count);
      draw_count++;
    }
    if (m_sprite_mode.do_second_draw) {
      render_state->shaders[ShaderId::SPRITE_CPU_AFAIL].activate();
      glDepthMask(GL_FALSE);
      glDrawArrays(GL_TRIANGLES, 0, m_prim_buffer.vert_count);
      glDepthMask(GL_TRUE);
      m_prim_gl_state_needs_gl_update = true;
      m_blend_state_needs_gl_update = true;
      draw_count++;
    }
  } else {
    glDrawArrays(GL_TRIANGLES, 0, m_prim_buffer.vert_count);
    draw_count++;
  }

  if (m_debug_state.wireframe) {
    render_state->shaders[ShaderId::DEBUG_RED].activate();
    glDisable(GL_BLEND);
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    glDrawArrays(GL_TRIANGLES, 0, m_prim_buffer.vert_count);
    glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    draw_count++;
  }

  glBindVertexArray(0);
  int n_tris = draw_count * (m_prim_buffer.vert_count / 3);
  prof.add_tri(n_tris);
  prof.add_draw_call(draw_count);
  m_triangles += n_tris;
  m_draw_calls += draw_count;
  m_prim_buffer.vert_count = 0;
}

void DirectRenderer::update_gl_prim(SharedRenderState* render_state) {
  // currently gouraud is handled in setup.
  const auto& state = m_prim_gl_state;
  if (state.texture_enable) {
    float alpha_reject = 0.;
    if (m_test_state.alpha_test_enable) {
      switch (m_test_state.alpha_test) {
        case GsTest::AlphaTest::ALWAYS:
          break;
        case GsTest::AlphaTest::GEQUAL:
          alpha_reject = m_test_state.aref / 128.f;
          break;
        default:
          assert(false);
      }
    }
    if (m_texture_state.tcc) {
      if (m_mode == Mode::SPRITE_CPU) {
        render_state->shaders[ShaderId::SPRITE_CPU].activate();
      } else if (m_mode == Mode::SKY) {
        assert(false);
      } else {
        render_state->shaders[ShaderId::DIRECT_BASIC_TEXTURED].activate();
        glUniform1f(
            glGetUniformLocation(render_state->shaders[ShaderId::DIRECT_BASIC_TEXTURED].id(),
                                 "alpha_reject"),
            alpha_reject);
      }
    } else {
      render_state->shaders[ShaderId::DIRECT_BASIC_TEXTURED_TCC0].activate();
      glUniform1f(
          glGetUniformLocation(render_state->shaders[ShaderId::DIRECT_BASIC_TEXTURED_TCC0].id(),
                               "alpha_reject"),
          alpha_reject);
    }
    update_gl_texture(render_state);
  } else {
    if (m_mode == Mode::SKY) {
      render_state->shaders[ShaderId::SKY].activate();
    } else {
      render_state->shaders[ShaderId::DIRECT_BASIC].activate();
    }
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

void DirectRenderer::update_gl_texture(SharedRenderState* render_state) {
  TextureRecord* tex = nullptr;
  if (m_texture_state.using_mt4hh) {
    tex = render_state->texture_pool->lookup_mt4hh(m_texture_state.texture_base_ptr);
  } else {
    tex = render_state->texture_pool->lookup(m_texture_state.texture_base_ptr);
  }

  if (!tex) {
    // TODO Add back
    fmt::print("Failed to find texture at {}, using random\n", m_texture_state.texture_base_ptr);
    tex = render_state->texture_pool->get_random_texture();
    if (tex) {
      // fmt::print("Successful texture lookup! {} {}\n", tex->page_name, tex->name);
    }
  }
  assert(tex);

  // first: do we need to load the texture?
  if (!tex->on_gpu) {
    render_state->texture_pool->upload_to_gpu(tex);
  }

  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, tex->gpu_texture);
  // Note: CLAMP and CLAMP_TO_EDGE are different...
  if (m_clamp_state.clamp) {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  } else {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  }

  if (m_texture_state.enable_tex_filt) {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  } else {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  }

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
  } else if (state.a == GsAlpha::BlendMode::SOURCE &&
             state.b == GsAlpha::BlendMode::ZERO_OR_FIXED &&
             state.c == GsAlpha::BlendMode::SOURCE && state.d == GsAlpha::BlendMode::DEST) {
    // (Cs - 0) * As + Cd
    // Cs * As + (1) * CD
    glEnable(GL_BLEND);
    // s, d
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);
  } else {
    lg::error("unsupported blend: a {} b {} c {} d {}\n", (int)state.a, (int)state.b, (int)state.c,
              (int)state.d);
    assert(false);
  }
}

void DirectRenderer::update_gl_test() {
  const auto& state = m_test_state;
  glEnable(GL_DEPTH_TEST);
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

  if (state.depth_writes) {
    glDepthMask(GL_TRUE);
  } else {
    glDepthMask(GL_FALSE);
  }
}

void DirectRenderer::setup_common_state(SharedRenderState* /*render_state*/) {
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
                                SharedRenderState* render_state,
                                ScopedProfilerNode& prof) {
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
        render_gif(data + offset_into_data, gif_qwc * 16, render_state, prof);
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
void DirectRenderer::render_gif(const u8* data,
                                u32 size,
                                SharedRenderState* render_state,
                                ScopedProfilerNode& prof) {
  assert(size >= 16);
  bool eop = false;

  u32 offset = 0;
  while (!eop) {
    assert(offset < size);
    GifTag tag(data + offset);
    offset += 16;
    // fmt::print("Tag at offset {}: {}\n", offset, tag.print());

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
        handle_prim(tag.prim(), render_state, prof);
      }
      for (u32 loop = 0; loop < tag.nloop(); loop++) {
        for (u32 reg = 0; reg < nreg; reg++) {
          switch (reg_desc[reg]) {
            case GifTag::RegisterDescriptor::AD:
              handle_ad(data + offset, render_state, prof);
              break;
            case GifTag::RegisterDescriptor::ST:
              handle_st_packed(data + offset);
              break;
            case GifTag::RegisterDescriptor::RGBAQ:
              handle_rgbaq_packed(data + offset);
              break;
            case GifTag::RegisterDescriptor::XYZF2:
              handle_xyzf2_packed(data + offset, render_state, prof);
              break;
            case GifTag::RegisterDescriptor::PRIM:
              handle_prim_packed(data + offset, render_state, prof);
              break;
            case GifTag::RegisterDescriptor::TEX0_1:
              handle_tex0_1_packed(data + offset, render_state, prof);
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
              handle_prim(register_data, render_state, prof);
              break;
            case GifTag::RegisterDescriptor::RGBAQ:
              handle_rgbaq(register_data);
              break;
            case GifTag::RegisterDescriptor::XYZF2:
              handle_xyzf2(register_data, render_state, prof);
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

  assert((offset + 15) / 16 == size / 16);

  //  fmt::print("{}\n", GifTag(data).print());
}

void DirectRenderer::handle_ad(const u8* data,
                               SharedRenderState* render_state,
                               ScopedProfilerNode& prof) {
  u64 value;
  GsRegisterAddress addr;
  memcpy(&value, data, sizeof(u64));
  memcpy(&addr, data + 8, sizeof(GsRegisterAddress));

  switch (addr) {
    case GsRegisterAddress::ZBUF_1:
      handle_zbuf1(value, render_state, prof);
      break;
    case GsRegisterAddress::TEST_1:
      handle_test1(value, render_state, prof);
      break;
    case GsRegisterAddress::ALPHA_1:
      handle_alpha1(value, render_state, prof);
      break;
    case GsRegisterAddress::PABE:
      handle_pabe(value);
      break;
    case GsRegisterAddress::CLAMP_1:
      handle_clamp1(value, render_state, prof);
      break;
    case GsRegisterAddress::PRIM:
      handle_prim(value, render_state, prof);
      break;

    case GsRegisterAddress::TEX1_1:
      handle_tex1_1(value, render_state, prof);
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
      handle_tex0_1(value, render_state, prof);
      break;
    case GsRegisterAddress::MIPTBP1_1:
      // TODO this has the address of different mip levels.
      break;
    case GsRegisterAddress::TEXFLUSH:
      break;
    default:
      fmt::print("Address {} is not supported\n", register_address_name(addr));
      assert(false);
  }
}

void DirectRenderer::handle_tex1_1(u64 val,
                                   SharedRenderState* render_state,
                                   ScopedProfilerNode& prof) {
  GsTex1 reg(val);
  // for now, we aren't going to handle mipmapping. I don't think it's used with direct.
  //   assert(reg.mxl() == 0);
  // if that's true, we can ignore LCM, MTBA, L, K

  bool want_tex_filt = reg.mmag();

  if (want_tex_filt != m_texture_state.enable_tex_filt) {
    flush_pending(render_state, prof);
    m_texture_state.enable_tex_filt = want_tex_filt;
  }

  // MMAG/MMIN specify texture filtering. For now, assume always linear
  //  assert(reg.mmag() == true);
  //  if (!(reg.mmin() == 1 || reg.mmin() == 4)) {  // with mipmap off, both of these are linear
  //                                                //    lg::error("unsupported mmin");
  //  }
}

void DirectRenderer::handle_tex0_1_packed(const u8* data,
                                          SharedRenderState* render_state,
                                          ScopedProfilerNode& prof) {
  u64 val;
  memcpy(&val, data, sizeof(u64));
  handle_tex0_1(val, render_state, prof);
}

void DirectRenderer::handle_tex0_1(u64 val,
                                   SharedRenderState* render_state,
                                   ScopedProfilerNode& prof) {
  GsTex0 reg(val);

  // update tbp
  if (m_texture_state.current_register != reg) {
    // fmt::print("flush due to tex0\n");
    flush_pending(render_state, prof);
    m_texture_state.texture_base_ptr = reg.tbp0();
    m_texture_state.using_mt4hh = reg.psm() == GsTex0::PSM::PSMT4HH;
    m_prim_gl_state_needs_gl_update = true;
    m_texture_state.current_register = reg;
    if (m_texture_state.tcc != reg.tcc()) {
      m_texture_state.needs_gl_update = true;
    }
    m_texture_state.tcc = reg.tcc();
  }

  // tbw: assume they got it right
  // psm: assume they got it right
  // tw: assume they got it right
  // th: assume they got it right

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
  assert(reg.ta1() == 0x80);  // note: check rgba16_to_rgba32 if this changes.

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

void DirectRenderer::handle_xyzf2_packed(const u8* data,
                                         SharedRenderState* render_state,
                                         ScopedProfilerNode& prof) {
  u32 x, y;
  memcpy(&x, data, 4);
  memcpy(&y, data + 4, 4);

  u64 upper;
  memcpy(&upper, data + 8, 8);
  u32 z = (upper >> 4) & 0xffffff;

  u8 f = (upper >> 36);
  bool adc = upper & (1ull << 47);
  assert(!adc);
  //  assert(!f);
  handle_xyzf2_common(x, y, z, f, render_state, prof);
}

void DirectRenderer::handle_zbuf1(u64 val,
                                  SharedRenderState* render_state,
                                  ScopedProfilerNode& prof) {
  // note: we can basically ignore this. There's a single z buffer that's always configured the same
  // way - 24-bit, at offset 448.
  GsZbuf x(val);
  assert(x.psm() == TextureFormat::PSMZ24);
  assert(x.zbp() == 448);

  bool write = x.zmsk();
  //  assert(write);

  if (write != m_test_state.depth_writes) {
    // fmt::print("flush due to depth write\n");
    flush_pending(render_state, prof);
    m_test_state_needs_gl_update = true;
    m_test_state.depth_writes = write;
  }
}

void DirectRenderer::handle_test1(u64 val,
                                  SharedRenderState* render_state,
                                  ScopedProfilerNode& prof) {
  GsTest reg(val);
  if (reg.alpha_test_enable()) {
    // assert(reg.alpha_test() == GsTest::AlphaTest::ALWAYS);
  }
  assert(!reg.date());
  if (m_test_state.current_register != reg) {
    // fmt::print("flush due to test\n");
    flush_pending(render_state, prof);
    m_test_state.from_register(reg);
    m_test_state_needs_gl_update = true;
    m_prim_gl_state_needs_gl_update = true;
  }
}

void DirectRenderer::handle_alpha1(u64 val,
                                   SharedRenderState* render_state,
                                   ScopedProfilerNode& prof) {
  GsAlpha reg(val);
  if (m_blend_state.current_register != reg) {
    // fmt::print("flush due to alpha1\n");
    flush_pending(render_state, prof);
    m_blend_state.from_register(reg);
    m_blend_state_needs_gl_update = true;
  }
}

void DirectRenderer::handle_pabe(u64 val) {
  assert(val == 0);  // not really sure how to handle this yet.
}

void DirectRenderer::handle_clamp1(u64 val,
                                   SharedRenderState* render_state,
                                   ScopedProfilerNode& prof) {
  assert(val == 0b101 || val == 0);
  if (m_clamp_state.current_register != val) {
    flush_pending(render_state, prof);
    m_clamp_state.current_register = val;
    if (val == 0b101) {
      m_clamp_state.clamp = true;
    } else {
      m_clamp_state.clamp = false;
    }
    m_texture_state.needs_gl_update = true;
  }
}

void DirectRenderer::handle_prim_packed(const u8* data,
                                        SharedRenderState* render_state,
                                        ScopedProfilerNode& prof) {
  u64 val;
  memcpy(&val, data, sizeof(u64));
  handle_prim(val, render_state, prof);
}

void DirectRenderer::handle_prim(u64 val,
                                 SharedRenderState* render_state,
                                 ScopedProfilerNode& prof) {
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
    // fmt::print("flush due to prim\n");
    flush_pending(render_state, prof);
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

void DirectRenderer::handle_xyzf2_common(u32 x,
                                         u32 y,
                                         u32 z,
                                         u8 f,
                                         SharedRenderState* render_state,
                                         ScopedProfilerNode& prof) {
  assert(z < (1 << 24));
  (void)f;  // TODO: do something with this.
  if (m_prim_buffer.is_full()) {
    // fmt::print("flush due to fill {} {}\n", m_prim_buffer.vert_count, m_prim_buffer.max_verts);
    flush_pending(render_state, prof);
  }

  m_prim_building.building_stq.at(m_prim_building.building_idx) = math::Vector<float, 3>(
      m_prim_building.st_reg.x(), m_prim_building.st_reg.y(), m_prim_building.Q);
  m_prim_building.building_rgba.at(m_prim_building.building_idx) = m_prim_building.rgba_reg;
  m_prim_building.building_vert.at(m_prim_building.building_idx) = {x << 16, y << 16, z << 8};
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
                             m_prim_building.building_stq[i]);
        }
      }

    } break;

    case GsPrim::Kind::TRI:
      if (m_prim_building.building_idx == 3) {
        m_prim_building.building_idx = 0;
        for (int i = 0; i < 3; i++) {
          m_prim_buffer.push(m_prim_building.building_rgba[i], m_prim_building.building_vert[i],
                             m_prim_building.building_stq[i]);
        }
      }
      break;

    case GsPrim::Kind::TRI_FAN: {
      if (m_prim_building.tri_strip_startup < 2) {
        m_prim_building.tri_strip_startup++;
      } else {
        if (m_prim_building.building_idx == 2) {
          // nothing.
        } else if (m_prim_building.building_idx == 3) {
          m_prim_building.building_idx = 1;
        }
        for (int i = 0; i < 3; i++) {
          m_prim_buffer.push(m_prim_building.building_rgba[i], m_prim_building.building_vert[i],
                             m_prim_building.building_stq[i]);
        }
      }
    } break;

    case GsPrim::Kind::LINE: {
      if (m_prim_building.building_idx == 2) {
        math::Vector<double, 3> pt0 = m_prim_building.building_vert[0].cast<double>();
        math::Vector<double, 3> pt1 = m_prim_building.building_vert[1].cast<double>();
        auto normal = (pt1 - pt0).normalized().cross({0, 0, 1});

        double line_width = (1 << 19);
        //        debug_print_vtx(m_prim_building.building_vert[0]);
        //        debug_print_vtx(m_prim_building.building_vert[1]);

        math::Vector<double, 3> a = pt0 + normal * line_width;
        math::Vector<double, 3> b = pt1 + normal * line_width;
        math::Vector<double, 3> c = pt0 - normal * line_width;
        math::Vector<double, 3> d = pt1 - normal * line_width;

        // ACB:
        m_prim_buffer.push(m_prim_building.building_rgba[0], a.cast<u32>(), {});
        m_prim_buffer.push(m_prim_building.building_rgba[0], c.cast<u32>(), {});
        m_prim_buffer.push(m_prim_building.building_rgba[1], b.cast<u32>(), {});
        // b c d
        m_prim_buffer.push(m_prim_building.building_rgba[1], b.cast<u32>(), {});
        m_prim_buffer.push(m_prim_building.building_rgba[0], c.cast<u32>(), {});
        m_prim_buffer.push(m_prim_building.building_rgba[1], d.cast<u32>(), {});
        //

        m_prim_building.building_idx = 0;
      }
    } break;
    default:
      fmt::print("prim type {} is unsupported.\n", (int)m_prim_building.kind);
      assert(false);
  }
}

void DirectRenderer::handle_xyzf2(u64 val,
                                  SharedRenderState* render_state,
                                  ScopedProfilerNode& prof) {
  u32 x = val & 0xffff;
  u32 y = (val >> 16) & 0xffff;
  u32 z = (val >> 32) & 0xffffff;
  u32 f = (val >> 56) & 0xff;

  handle_xyzf2_common(x, y, z, f, render_state, prof);
}

void DirectRenderer::reset_state() {
  m_test_state_needs_gl_update = true;
  m_test_state = TestState();

  m_blend_state_needs_gl_update = true;
  m_blend_state = BlendState();

  m_prim_gl_state_needs_gl_update = true;
  m_prim_gl_state = PrimGlState();

  m_texture_state = TextureState();

  m_prim_building = PrimBuildState();

  m_triangles = 0;
  m_draw_calls = 0;
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
  stqs.resize(max_triangles * 3);
  max_verts = max_triangles * 3;
}

void DirectRenderer::PrimitiveBuffer::push(const math::Vector<u8, 4>& rgba,
                                           const math::Vector<u32, 3>& vert,
                                           const math::Vector<float, 3>& st) {
  rgba_u8[vert_count] = rgba;
  verts[vert_count] = vert;
  stqs[vert_count] = st;
  vert_count++;
}
