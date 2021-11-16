#include "BufferedRenderer.h"
#include "common/dma/gs.h"
#include "third-party/imgui/imgui.h"
#include "game/graphics/pipelines/opengl.h"

namespace BufferedRenderer {

std::string DrawMode::to_string() const {
  std::string result;
  result += fmt::format(" depth-write: {}\n", get_depth_write_enable());
  result += fmt::format(" depth-test: ");
  switch (get_depth_test()) {
    case GsTest::ZTest::NEVER:
      result += "never\n";
      break;
    case GsTest::ZTest::GEQUAL:
      result += "gequal\n";
      break;
    case GsTest::ZTest::ALWAYS:
      result += "always\n";
      break;
    case GsTest::ZTest::GREATER:
      result += "greater\n";
      break;
    default:
      assert(false);
  }
  result += fmt::format(" alpha: ");
  switch (get_alpha_blend()) {
    case AlphaBlend::SRC_0_SRC_DST:
      result += "src, 0, src, dst\n";
      break;
    case AlphaBlend::SRC_DST_SRC_DST:
      result += "src, dst, src, dst\n";
      break;
    case AlphaBlend::DISABLED:
      result += "disabled\n";
      break;
    default:
      assert(false);
  }
  result += fmt::format(" clamp: {}\n", get_clamp_enable());
  result += fmt::format(" filt: {}\n", get_filt_enable());
  result += fmt::format(" tcc: {}\n", get_tcc_enable());
  result += fmt::format(" aref: {}\n", get_aref());
  result += fmt::format(" ate: {}\n", get_at_enable());
  result += fmt::format(" atst: ");
  switch (get_alpha_test()) {
    case AlphaTest::ALWAYS:
      result += "always\n";
      break;
    case AlphaTest::GEQUAL:
      result += "gequal\n";
      break;
    case AlphaTest::NEVER:
      result += "never\n";
      break;
    default:
      assert(false);
  }
  result += fmt::format(" zte: {}\n", get_zt_enable());
  result += fmt::format(" abe: {}\n", get_ab_enable());
  result += fmt::format(" afail: ");
  switch (get_alpha_fail()) {
    case GsTest::AlphaFail::KEEP:
      result += "keep\n";
      break;
    case GsTest::AlphaFail::FB_ONLY:
      result += "fb-only\n";
      break;
    case GsTest::AlphaFail::RGB_ONLY:
      result += "rgb-only\n";
      break;
    case GsTest::AlphaFail::ZB_ONLY:
      result += "zb-only\n";
      break;
    default:
      assert(false);
  }
  return result;
}

Renderer::Renderer(BucketId my_id) : m_my_id(my_id) {
  glGenBuffers(1, &m_ogl.vertex_buffer);
  glGenBuffers(1, &m_ogl.index_buffer);
  glGenVertexArrays(1, &m_ogl.vao);

  // these are some big buffers
  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.vertex_buffer);
  m_ogl.vertex_buffer_size = MAX_VERTS;
  glBufferData(GL_ARRAY_BUFFER, m_ogl.vertex_buffer_size * sizeof(Vertex), nullptr,
               GL_DYNAMIC_DRAW);

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_ogl.index_buffer);
  m_ogl.index_buffer_size = MAX_VERTS * 3;
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, m_ogl.index_buffer_size * sizeof(u32), nullptr,
               GL_DYNAMIC_DRAW);
}

Renderer::~Renderer() {
  glDeleteBuffers(1, &m_ogl.vertex_buffer);
  glDeleteBuffers(1, &m_ogl.index_buffer);
  glDeleteVertexArrays(1, &m_ogl.vao);
}

void Renderer::render_list(const DrawList& list,
                           SharedRenderState* render_state,
                           ScopedProfilerNode& prof,
                           const std::vector<Vertex>& vertices) {
  // first, load primitive buffer
  glBindVertexArray(m_ogl.vao);
  u32 vert_count = std::min((int)vertices.size(), (int)m_ogl.vertex_buffer_size);
  if (vertices.size() > m_ogl.vertex_buffer_size) {
    fmt::print("TOO MANY VERTICES: {} / {}\n", vertices.size(), m_ogl.vertex_buffer_size);
    assert(false);
  }

  glEnableVertexAttribArray(0);
  glEnableVertexAttribArray(1);
  glEnableVertexAttribArray(2);
  glEnableVertexAttribArray(3);

  glBindBuffer(GL_ARRAY_BUFFER, m_ogl.vertex_buffer);
  glBufferSubData(GL_ARRAY_BUFFER, 0, vert_count * sizeof(Vertex), vertices.data());

  glVertexAttribPointer(0,                            // location 0 in the shader
                        3,                            // 3 values per vert
                        GL_UNSIGNED_INT,              // u32's
                        GL_TRUE,                      // normalized
                        sizeof(Vertex),               // stride
                        (void*)offsetof(Vertex, xyz)  // offset (0)
  );

  glVertexAttribPointer(1,                             // location 1 in the shader
                        4,                             // 4 values per vert
                        GL_UNSIGNED_BYTE,              // u8's
                        GL_TRUE,                       // normalized
                        sizeof(Vertex),                // stride
                        (void*)offsetof(Vertex, rgba)  // offset (0)
  );

  glVertexAttribPointer(2,                           // location 2 in the shader
                        3,                           // 3 values per vert
                        GL_FLOAT,                    // u32's
                        GL_FALSE,                    // normalized
                        sizeof(Vertex),              // stride
                        (void*)offsetof(Vertex, st)  // offset (0)
  );

  glActiveTexture(GL_TEXTURE0);

  for (auto& group : list.groups) {
    if (group.tbp != UINT16_MAX) {
      render_group(group, render_state, prof, vertices);
    }
  }

  glDisableVertexAttribArray(0);
  glDisableVertexAttribArray(1);
  glDisableVertexAttribArray(2);

  glBindVertexArray(0);
}

void Renderer::setup_opengl_excluding_textures(SharedRenderState* render_state, DrawMode mode) {
  if (mode.get_depth_write_enable()) {
    glDepthMask(GL_TRUE);
  } else {
    glDepthMask(GL_FALSE);
  }

  if (mode.get_zt_enable()) {
    glEnable(GL_DEPTH_TEST);
    switch (mode.get_depth_test()) {
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
    glDisable(GL_DEPTH_TEST);
  }

  if (mode.get_ab_enable() && mode.get_alpha_blend() != DrawMode::AlphaBlend::DISABLED) {
    glEnable(GL_BLEND);
    switch (mode.get_alpha_blend()) {
      case DrawMode::AlphaBlend::SRC_DST_SRC_DST:
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        break;
      case DrawMode::AlphaBlend::SRC_0_SRC_DST:
        glBlendFunc(GL_SRC_ALPHA, GL_ONE);
        break;
      default:
        assert(false);
    }
  }

  if (mode.get_clamp_enable()) {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  } else {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  }

  if (mode.get_filt_enable()) {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  } else {
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  }

  float alpha_reject = 0.;
  if (mode.get_at_enable()) {
    switch (mode.get_alpha_test()) {
      case DrawMode::AlphaTest::ALWAYS:
        break;
      case DrawMode::AlphaTest::GEQUAL:
        alpha_reject = mode.get_aref() / 127.f;
        break;
      case DrawMode::AlphaTest::NEVER:
        break;
      default:
        assert(false);
    }
  }

  // todo check afail

  if (mode.get_tcc_enable()) {
    render_state->shaders[ShaderId::BUFFERED_TCC1].activate();
    glUniform1f(
        glGetUniformLocation(render_state->shaders[ShaderId::BUFFERED_TCC1].id(), "alpha_reject"),
        alpha_reject);
    glUniform1i(glGetUniformLocation(render_state->shaders[ShaderId::BUFFERED_TCC1].id(), "T0"), 0);
  } else {
    render_state->shaders[ShaderId::BUFFERED_TCC0].activate();
    glUniform1f(
        glGetUniformLocation(render_state->shaders[ShaderId::BUFFERED_TCC0].id(), "alpha_reject"),
        alpha_reject);
    glUniform1i(glGetUniformLocation(render_state->shaders[ShaderId::BUFFERED_TCC0].id(), "T0"), 0);
  }
}

void Renderer::render_group(const DrawGroup& group,
                            SharedRenderState* render_state,
                            ScopedProfilerNode& prof,
                            const std::vector<Vertex>& /*vertices*/) {
  TextureRecord* tex = nullptr;

  tex = render_state->texture_pool->lookup(group.tbp);

  if (!tex) {
    fmt::print("Failed to find texture at {}, using random\n", group.tbp);
    tex = render_state->texture_pool->get_random_texture();
  }
  assert(tex);

  // first: do we need to load the texture?
  if (!tex->on_gpu) {
    render_state->texture_pool->upload_to_gpu(tex);
  }

  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, tex->gpu_texture);

  for (auto& draw : group.draws) {
    if (draw.mode.is_valid()) {
      m_stats.draw_calls++;
      prof.add_draw_call();
      prof.add_tri(draw.triangles.size());
      render_state->shaders[ShaderId::DEBUG_BUFFERED].activate();
      setup_opengl_excluding_textures(render_state, draw.mode);
      // check for overflows.
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_ogl.index_buffer);
      if (draw.triangles.size() * 3 > m_ogl.index_buffer_size) {
        fmt::format("TOO MANY TRIS: {}/{}\n", draw.triangles.size() * 3, m_ogl.index_buffer_size);
        assert(false);
      }

      glBufferSubData(GL_ELEMENT_ARRAY_BUFFER, 0, draw.triangles.size() * sizeof(u32) * 3,
                      draw.triangles.data());
      glDrawElements(GL_TRIANGLES, draw.triangles.size() * 3, GL_UNSIGNED_INT, (void*)0);
    }
  }
}

void Renderer::draw_debug_window() {
  // todo
  ImGui::Text("draws: %d", m_stats.draw_calls);
}

void Renderer::clear_stats() {
  m_stats = {};
}

Builder::Builder(BucketId my_id) : m_my_id(my_id), m_renderer(my_id) {}

void Builder::add_gif_data_sized(const void* data, u32 expected_size) {
  if (expected_size != add_gif_data(data)) {
    assert(false);  // todo, might be too strict due to alignment crap
  }
}

void Builder::flush(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  m_renderer.render_list(m_list, render_state, prof, m_vertices);
  m_list.clear();
  m_vertices.clear();
  m_cache = {};
}

void Builder::draw_debug_window() {
  ImGui::Text("Builder: %d tri, %d vert", m_stats.m_tri, m_stats.m_dvert);

  ImGui::Text("Renderer:");
  ImGui::Separator();
  m_renderer.draw_debug_window();
}

void Builder::reset_state() {
  m_current_mode.as_int() = 0;
  m_stats = {};
  m_renderer.clear_stats();
  m_vertex_queue = {};
  m_current_mode.enable_depth_write();
  m_current_mode.enable_ab();
  m_current_mode.enable_at();
  m_current_mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_SRC_DST);
}

u32 Builder::add_gif_data(const void* data_in) {
  bool eop = false;
  auto* data = (const u8*)data_in;

  u32 offset = 0;
  while (!eop) {
    GifTag tag(data + offset);
    offset += 16;

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
        handle_prim(tag.prim());
      }
      for (u32 loop = 0; loop < tag.nloop(); loop++) {
        for (u32 reg = 0; reg < nreg; reg++) {
          switch (reg_desc[reg]) {
            case GifTag::RegisterDescriptor::AD:
              handle_ad(data + offset);
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
              //            case GifTag::RegisterDescriptor::PRIM:
              //              handle_prim_packed(data + offset, render_state, prof);
              //              break;
              //            case GifTag::RegisterDescriptor::TEX0_1:
              //              handle_tex0_1_packed(data + offset, render_state, prof);
              //              break;
            default:
              fmt::print("Register {} is not supported in packed mode of BufferedRenderer\n",
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
              //            case GifTag::RegisterDescriptor::PRIM:
              //              handle_prim(register_data, render_state, prof);
              //              break;
              //            case GifTag::RegisterDescriptor::RGBAQ:
              //              handle_rgbaq(register_data);
              //              break;
              //            case GifTag::RegisterDescriptor::XYZF2:
              //              handle_xyzf2(register_data, render_state, prof);
              //              break;
            default:
              fmt::print("Register {} is not supported in reglist mode of BufferedRenderer\n",
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
  return offset;
}

void Builder::handle_ad(const u8* data) {
  u64 value;
  GsRegisterAddress addr;
  memcpy(&value, data, sizeof(u64));
  memcpy(&addr, data + 8, sizeof(GsRegisterAddress));

  switch (addr) {
      //    case GsRegisterAddress::ZBUF_1:
      //      handle_zbuf1(value, render_state, prof);
      //      break;
    case GsRegisterAddress::TEST_1:
      handle_test1(value);
      break;
    case GsRegisterAddress::ALPHA_1:
      handle_alpha1(value);
      break;
      //    case GsRegisterAddress::PABE:
      //      handle_pabe(value);
      break;
    case GsRegisterAddress::CLAMP_1:
      handle_clamp1(value);
      break;
      //    case GsRegisterAddress::PRIM:
      //      handle_prim(value, render_state, prof);
      //      break;
      //
    case GsRegisterAddress::TEX1_1:
      handle_tex1_1(value);
      break;
      //    case GsRegisterAddress::TEXA:
      //      handle_texa(value);
      //      break;
      //    case GsRegisterAddress::TEXCLUT:
      //      // TODO
      //      // the only thing the direct renderer does with texture is font, which does no tricks
      //      with
      //      // CLUT. The texture upload process will do all of the lookups with the default CLUT.
      //      // So we'll just assume that the TEXCLUT is set properly and ignore this.
      //      break;
      //    case GsRegisterAddress::FOGCOL:
      //      // TODO
      //      break;
    case GsRegisterAddress::TEX0_1:
      handle_tex0_1(value);
      break;
    case GsRegisterAddress::MIPTBP1_1:
    case GsRegisterAddress::MIPTBP2_1:
      // this has the address of different mip levels, we can just ignore it.
      break;
      //    case GsRegisterAddress::TEXFLUSH:
      //      break;
    default:
      fmt::print("Address {} is not supported in ad of BufferedRenderer\n",
                 register_address_name(addr));
      assert(false);
  }
}

void Builder::handle_test1(u64 val) {
  // ate, atst, aref, afail, date, datm, zte, ztest
  GsTest test(val);

  // ATE
  m_current_mode.set_at(test.alpha_test_enable());

  // ATST
  switch (test.alpha_test()) {
    case GsTest::AlphaTest::ALWAYS:
      m_current_mode.set_alpha_test(DrawMode::AlphaTest::ALWAYS);
      break;
    case GsTest::AlphaTest::GEQUAL:
      m_current_mode.set_alpha_test(DrawMode::AlphaTest::GEQUAL);
      break;
    case GsTest::AlphaTest::NEVER:
      m_current_mode.set_alpha_test(DrawMode::AlphaTest::NEVER);
      break;
    default:
      fmt::print("Alpha test: {} not supported\n", (int)test.alpha_test());
      assert(false);
  }

  // AREF
  m_current_mode.set_aref(test.aref());

  // AFAIL
  m_current_mode.set_alpha_fail(test.afail());

  // DATE
  assert(test.date() == false);

  // DATM
  // who cares, if date is off

  // ZTE
  m_current_mode.set_zt(test.zte());

  // ZTST
  m_current_mode.set_depth_test(test.ztest());
}

void Builder::handle_tex0_1(u64 val) {
  GsTex0 reg(val);

  // TBP0
  m_current_tbp = reg.tbp0();

  // TBW
  // assume it's right

  // PSM
  assert(reg.psm() != GsTex0::PSM::PSMT4HH);  // not supported in buffered yet.

  // TW, TH
  // assume it's right

  // TCC
  m_current_mode.set_tcc(reg.tcc());

  // TFX
  assert(reg.tfx() == GsTex0::TextureFunction::MODULATE);

  // CBP, CPSM, CSM
  // assume it's right
}

void Builder::handle_tex1_1(u64 val) {
  GsTex1 reg(val);
  // ignoring these and doing our own thing!
  // just need to pick between filtering and not filtering.
  m_current_mode.set_filt_enable(reg.mmag());
}

void Builder::handle_clamp1(u64 val) {
  // check that we got one of the expected ones
  if (!(val == 0b101 || val == 0 || val == 1 || val == 0b100)) {
    fmt::print("clamp: 0x{:x}\n", val);
    assert(false);
  }

  // this isn't quite right, but I'm hoping it's enough!
  m_current_mode.set_clamp_enable(val == 0b101);
}

void Builder::handle_alpha1(u64 val) {
  GsAlpha reg(val);
  if (reg.a_mode() == GsAlpha::BlendMode::SOURCE && reg.b_mode() == GsAlpha::BlendMode::DEST &&
      reg.c_mode() == GsAlpha::BlendMode::SOURCE && reg.d_mode() == GsAlpha::BlendMode::DEST) {
    // (Cs - Cd) * As + Cd
    // Cs * As  + (1 - As) * Cd
    m_current_mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_DST_SRC_DST);

  } else if (reg.a_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.b_mode() == GsAlpha::BlendMode::ZERO_OR_FIXED &&
             reg.c_mode() == GsAlpha::BlendMode::SOURCE &&
             reg.d_mode() == GsAlpha::BlendMode::DEST) {
    // (Cs - 0) * As + Cd
    // Cs * As + (1) * CD
    m_current_mode.set_alpha_blend(DrawMode::AlphaBlend::SRC_0_SRC_DST);
  } else {
    // unsupported blend: a 0 b 2 c 2 d 1
    fmt::print("unsupported blend: a {} b {} c {} d {}\n", (int)reg.a_mode(), (int)reg.b_mode(),
               (int)reg.c_mode(), (int)reg.d_mode());
    assert(false);
  }
}

void Builder::handle_prim(u64 val) {
  GsPrim prim(val);

  // PRIM
  m_prim_kind = prim.kind();

  // IIP
  assert(prim.gouraud());

  // TME
  assert(prim.tme());

  // FGE
  // TODO fog

  // ABE
  m_current_mode.set_ab(prim.abe());

  // AA1
  assert(!prim.aa1());

  // FST
  assert(!prim.fst());

  // CTXT
  assert(!prim.ctxt());

  // FIX
  assert(!prim.fix());

  if (m_vertex_queue.startup) {
    m_vertex_queue.startup = 0;
    m_vertex_queue.idx = 0;
  } else {
    // assert(false);
  }
}

void Builder::handle_st_packed(const u8* data) {
  memcpy(m_st_pending_q, data, 3 * sizeof(float));
}

void Builder::handle_rgbaq_packed(const u8* data) {
  m_q = m_st_pending_q[2];
  // memcpy(m_rgba.data(), data, 4);
  m_rgba[0] = data[0];
  m_rgba[1] = data[4];
  m_rgba[2] = data[8];
  m_rgba[3] = data[12];
}

void Builder::handle_xyzf2_packed(const u8* data) {
  u32 x, y;
  memcpy(&x, data, 4);
  memcpy(&y, data + 4, 4);

  u64 upper;
  memcpy(&upper, data + 8, 8);
  u32 z = (upper >> 4) & 0xffffff;

  u8 f = (upper >> 36);
  bool adc = upper & (1ull << 47);
  handle_xyzf2_common(x, y, z, f, !adc);
}

void Builder::handle_xyzf2_common(u32 x, u32 y, u32 z, u8 /*f*/, bool advance) {
  // first, create a vertex:
  u32 new_vertex = create_vertex_now(x, y, z);

  // next, add that vertex. This will inform us if we actually draw a prim or not
  bool new_prim = false;
  switch (m_prim_kind) {
    case GsPrim::Kind::TRI_STRIP:
      new_prim = handle_tri_strip_add(new_vertex, advance);
      break;
    default:
      assert(false);
  }

  if (new_prim) {
    // todo, winding order?
    add_prim_now({m_vertex_queue.verts[0], m_vertex_queue.verts[1], m_vertex_queue.verts[2]});
  }
}

void Builder::add_prim_now(Triangle tri) {
  if (m_cache.last_tbp == m_current_tbp && m_cache.last_mode == m_current_mode.as_int()) {
    m_cache.draw->triangles.push_back(tri);
  } else {
    auto group = m_list.get_group_for_tbp(m_current_tbp);
    assert(group);
    // todo flush and stats

    auto draw = group->get_draw_for_mode(m_current_mode);
    assert(draw);
    // todo flush and stats
    draw->triangles.push_back(tri);
    m_cache.last_mode = m_current_mode.as_int();
    m_cache.last_tbp = m_current_tbp;
    m_cache.draw = draw;
  }

  m_stats.m_tri++;
}

u32 Builder::create_vertex_now(u32 x, u32 y, u32 z) {
  m_stats.m_dvert++;
  u32 idx = m_vertices.size();
  m_vertices.emplace_back();
  auto& vert = m_vertices.back();
  vert.xyz = math::Vector<u32, 3>(x << 16, y << 16, z << 8);
  vert.rgba = m_rgba;
  vert.st = math::Vector<float, 2>(m_st_pending_q[0], m_st_pending_q[1]);
  vert.q = m_q;
  return idx;
}

bool Builder::handle_tri_strip_add(u32 new_vertex, bool advance) {
  m_vertex_queue.verts[m_vertex_queue.idx] = new_vertex;
  m_vertex_queue.idx++;

  // wrap the index
  if (m_vertex_queue.idx == 3) {
    m_vertex_queue.idx = 0;
  }

  // bump the startup
  if (m_vertex_queue.startup < 3) {
    m_vertex_queue.startup++;
  }

  if (m_vertex_queue.startup >= 3) {
    if (advance) {
      return true;
    }
  }
  return false;
}

}  // namespace BufferedRenderer
