#include "EyeRenderer.h"
#include "game/graphics/opengl_renderer/AdgifHandler.h"

#include "common/util/FileUtil.h"
#include "third-party/imgui/imgui.h"

EyeRenderer::EyeRenderer(const std::string& name, BucketId id) : BucketRenderer(name, id) {}

void EyeRenderer::init_textures(TexturePool& texture_pool) {
  for (int pair_idx = 0; pair_idx < NUM_EYE_PAIRS; pair_idx++) {
    for (int lr = 0; lr < 2; lr++) {
      GLuint gl_tex;
      glGenTextures(1, &gl_tex);
      u32 tbp = EYE_BASE_BLOCK + pair_idx * 2 + lr;
      TextureInput in;
      in.gpu_texture = gl_tex;
      in.w = 32;
      in.h = 32;
      in.page_name = "PC-EYES";
      in.name = fmt::format("{}-eye-{}", lr ? "left" : "right", pair_idx);
      auto* gpu_tex = texture_pool.give_texture_and_load_to_vram(in, tbp);
      m_eye_textures[pair_idx * 2 + lr] = {gl_tex, gpu_tex, tbp};
    }
  }
}

void EyeRenderer::render(DmaFollower& dma,
                         SharedRenderState* render_state,
                         ScopedProfilerNode& prof) {
  m_debug.clear();

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

  handle_eye_dma2<false>(dma, render_state, prof);

  while (dma.current_tag_offset() != render_state->next_bucket) {
    auto data = dma.read_and_advance();
    m_debug += fmt::format("dma: {}\n", data.size_bytes);
  }
}

struct ScissorInfo {
  int x0, x1;
  int y0, y1;
  std::string print() const { return fmt::format("x : [{}, {}], y : [{}, {}]", x0, x1, y0, y1); }
};

ScissorInfo decode_scissor(const DmaTransfer& dma) {
  ASSERT(dma.vif0() == 0);
  ASSERT(dma.vifcode1().kind == VifCode::Kind::DIRECT);
  ASSERT(dma.size_bytes == 32);

  GifTag gifTag(dma.data);
  ASSERT(gifTag.nloop() == 1);
  ASSERT(gifTag.eop());
  ASSERT(!gifTag.pre());
  ASSERT(gifTag.flg() == GifTag::Format::PACKED);
  ASSERT(gifTag.nreg() == 1);

  u8 reg_addr;
  memcpy(&reg_addr, dma.data + 24, 1);
  ASSERT((GsRegisterAddress)reg_addr == GsRegisterAddress::SCISSOR_1);
  ScissorInfo result;
  u64 val;
  memcpy(&val, dma.data + 16, 8);
  GsScissor reg(val);
  result.x0 = reg.x0();
  result.x1 = reg.x1();
  result.y0 = reg.y0();
  result.y1 = reg.y1();
  return result;
}

struct SpriteInfo {
  u8 a;
  u32 uv0[2];
  u32 uv1[2];
  u32 xyz0[3];
  u32 xyz1[3];

  std::string print() const {
    std::string result;
    result +=
        fmt::format("a: {:x} uv: ({}, {}), ({}, {}) xyz: ({}, {}, {}), ({}, {}, {})", a, uv0[0],
                    uv0[1], uv1[0], uv1[1], xyz0[0], xyz0[1], xyz0[2], xyz1[0], xyz1[1], xyz1[2]);
    return result;
  }
};

SpriteInfo decode_sprite(const DmaTransfer& dma) {
  /*
   (new 'static 'dma-gif-packet
        :dma-vif (new 'static 'dma-packet
                      :dma (new 'static 'dma-tag :qwc #x6 :id (dma-tag-id cnt))
                      :vif1 (new 'static 'vif-tag :imm #x6 :cmd (vif-cmd direct) :msk #x1)
                      )
        :gif0 (new 'static 'gif-tag64
                   :nloop #x1
                   :eop #x1
                   :pre #x1
                   :prim (new 'static 'gs-prim :prim (gs-prim-type sprite) :tme #x1 :fst #x1)
                   :nreg #x5
                   )
        :gif1 (new 'static 'gif-tag-regs
                   :regs0 (gif-reg-id rgbaq)
                   :regs1 (gif-reg-id uv)
                   :regs2 (gif-reg-id xyz2)
                   :regs3 (gif-reg-id uv)
                   :regs4 (gif-reg-id xyz2)
                   )
        )
   */

  ASSERT(dma.vif0() == 0);
  ASSERT(dma.vifcode1().kind == VifCode::Kind::DIRECT);
  ASSERT(dma.size_bytes == 6 * 16);

  // note: not checking everything here.
  GifTag gifTag(dma.data);
  ASSERT(gifTag.nloop() == 1);
  ASSERT(gifTag.eop());
  ASSERT(gifTag.pre());
  ASSERT(gifTag.flg() == GifTag::Format::PACKED);
  ASSERT(gifTag.nreg() == 5);

  SpriteInfo result;

  // rgba
  ASSERT(dma.data[16] == 128);               // r
  ASSERT(dma.data[16 + 4] == 128);           // r
  ASSERT(dma.data[16 + 8] == 128);           // r
  memcpy(&result.a, dma.data + 16 + 12, 1);  // a

  // uv0
  memcpy(&result.uv0[0], &dma.data[32], 8);

  // xyz0
  memcpy(&result.xyz0[0], &dma.data[48], 12);
  result.xyz0[2] >>= 4;

  // uv1
  memcpy(&result.uv1[0], &dma.data[64], 8);

  // xyz1
  memcpy(&result.xyz1[0], &dma.data[80], 12);
  result.xyz1[2] >>= 4;

  return result;
}

struct EyeDraw {
  SpriteInfo sprite;
  ScissorInfo scissor;
  std::string print() const { return fmt::format("{}\n{}\n", sprite.print(), scissor.print()); }
};

EyeDraw read_eye_draw(DmaFollower& dma) {
  auto scissor = decode_scissor(dma.read_and_advance());
  auto sprite = decode_sprite(dma.read_and_advance());
  return {sprite, scissor};
}

void EyeRenderer::draw_debug_window() {
  ImGui::Text("Time: %.3f ms\n", m_average_time_ms);
  ImGui::Text("Debug:\n%s", m_debug.c_str());
  ImGui::Checkbox("bilinear", &m_use_bilinear);
  ImGui::Checkbox("alpha hack", &m_alpha_hack);
}

u32 bilinear_sample_eye(const u8* tex, float tx, float ty, int texw) {
  int tx0 = tx;
  int ty0 = ty;
  int tx1 = tx0 + 1;
  int ty1 = ty0 + 1;
  tx1 = std::min(tx1, texw - 1);
  ty1 = std::min(ty1, texw - 1);

  u8 tex0[4];
  u8 tex1[4];
  u8 tex2[4];
  u8 tex3[4];
  memcpy(tex0, tex + (4 * (tx0 + ty0 * texw)), 4);
  memcpy(tex1, tex + (4 * (tx1 + ty0 * texw)), 4);
  memcpy(tex2, tex + (4 * (tx0 + ty1 * texw)), 4);
  memcpy(tex3, tex + (4 * (tx1 + ty1 * texw)), 4);

  u8 result[4] = {0, 0, 0, 0};
  float x0w = float(tx1) - tx;
  float y0w = float(ty1) - ty;
  float weights[4] = {x0w * y0w, (1.f - x0w) * y0w, x0w * (1.f - y0w), (1.f - x0w) * (1.f - y0w)};

  for (int i = 0; i < 4; i++) {
    float total = 0;
    total += weights[0] * tex0[i];
    total += weights[1] * tex1[i];
    total += weights[2] * tex2[i];
    total += weights[3] * tex3[i];
    result[i] = total;
  }

  // clamp
  u32 tex_out;
  memcpy(&tex_out, result, 4);
  return tex_out;
}

template <bool blend, bool bilinear>
void draw_eye_impl(u32* out,
                   const EyeDraw& draw,
                   const GpuTexture& tex,
                   int pair,
                   int lr,
                   bool flipx) {
  // first, figure out the rectangle we'd cover if there was no scissoring

  int x0 = ((((int)draw.sprite.xyz0[0]) - 512) >> 4);
  int x1 = ((((int)draw.sprite.xyz1[0]) - 512) >> 4);
  if (flipx) {
    std::swap(x0, x1);
  }

  int y0 = ((((int)draw.sprite.xyz0[1]) - 512) >> 4);
  int y1 = ((((int)draw.sprite.xyz1[1]) - 512) >> 4);

  // then the offset because the game tries to draw to a big texture, but we do an eye at a time
  int x_off = lr * SINGLE_EYE_SIZE;
  int y_off = pair * SINGLE_EYE_SIZE;

  // apply scissoring bounds
  int x0s = std::max(x0, (int)draw.scissor.x0) - x_off;
  int y0s = std::max(y0, (int)draw.scissor.y0) - y_off;
  int x1s = std::min(x1, (int)draw.scissor.x1) - x_off;
  int y1s = std::min(y1, (int)draw.scissor.y1) - y_off;

  // compute inverse lengths (of non-scissored)
  float inv_xl = .999f / ((float)(x1 - x0));
  float inv_yl = .999f / ((float)(y1 - y0));

  // starts
  float tx0 = tex.w * (x0s - x0 + x_off) * inv_xl;
  float ty0 = tex.h * (y0s - y0 + y_off) * inv_yl;

  // steps
  float txs = tex.w * inv_xl;
  float tys = tex.h * inv_yl;

  float ty = ty0;
  for (int yd = y0s; yd < y1s; yd++) {
    float tx = tx0;
    for (int xd = x0s; xd < x1s; xd++) {
      u32 val;
      if (bilinear) {
        val = bilinear_sample_eye(tex.get_data_ptr(), tx, ty, tex.w);
      } else {
        int tc = int(tx) + tex.w * int(ty);
        memcpy(&val, tex.get_data_ptr() + (4 * tc), 4);
      }
      if (blend) {
        if ((val >> 24) != 0) {
          out[xd + yd * SINGLE_EYE_SIZE] = val;
        }
      } else {
        out[xd + yd * SINGLE_EYE_SIZE] = val;
      }
      tx += txs;
    }
    ty += tys;
  }
}

template <bool blend>
void draw_eye(u32* out,
              const EyeDraw& draw,
              const GpuTexture& tex,
              int pair,
              int lr,
              bool flipx,
              bool bilinear) {
  if (bilinear) {
    draw_eye_impl<blend, true>(out, draw, tex, pair, lr, flipx);
  } else {
    draw_eye_impl<blend, false>(out, draw, tex, pair, lr, flipx);
  }
}

template <bool DEBUG>
void EyeRenderer::handle_eye_dma2(DmaFollower& dma,
                                  SharedRenderState* render_state,
                                  ScopedProfilerNode&) {
  Timer timer;
  m_debug.clear();

  // first should be the gs setup for render to texture
  auto offset_setup = dma.read_and_advance();
  ASSERT(offset_setup.size_bytes == 128);
  ASSERT(offset_setup.vifcode0().kind == VifCode::Kind::FLUSHA);
  ASSERT(offset_setup.vifcode1().kind == VifCode::Kind::DIRECT);

  // next should be alpha setup
  auto alpha_setup = dma.read_and_advance();
  ASSERT(alpha_setup.size_bytes == 32);
  ASSERT(alpha_setup.vifcode0().kind == VifCode::Kind::NOP);
  ASSERT(alpha_setup.vifcode1().kind == VifCode::Kind::DIRECT);

  // from the add to bucket
  ASSERT(dma.current_tag().kind == DmaTag::Kind::NEXT);
  ASSERT(dma.current_tag().qwc == 0);
  ASSERT(dma.current_tag_vif0() == 0);
  ASSERT(dma.current_tag_vif1() == 0);
  dma.read_and_advance();

  // now, loop over eyes. end condition is a 8 qw transfer to restore gs.
  while (dma.current_tag().qwc != 8) {
    // eye background setup
    auto adgif0_dma = dma.read_and_advance();
    ASSERT(adgif0_dma.size_bytes == 96);  // 5 adgifs a+d's plus tag
    ASSERT(adgif0_dma.vif0() == 0);
    ASSERT(adgif0_dma.vifcode1().kind == VifCode::Kind::DIRECT);
    AdgifHelper adgif0(adgif0_dma.data + 16);
    auto tex0 = render_state->texture_pool->lookup_gpu_texture(adgif0.tex0().tbp0());
    if (DEBUG) {
      m_debug += fmt::format("ADGIF0:\n{}\n", adgif0.print());
      m_debug += fmt::format("tex: {}\n", tex0->name);
    }

    u32 pair_idx = -1;
    // first draw. this is the background. It reads 0,0 of the texture uses that color everywhere.
    // we'll also figure out the eye index here.
    {
      auto draw0 = read_eye_draw(dma);
      ASSERT(draw0.sprite.uv0[0] == 0);
      ASSERT(draw0.sprite.uv0[1] == 0);
      ASSERT(draw0.sprite.uv1[0] == 0);
      ASSERT(draw0.sprite.uv1[1] == 0);
      if (DEBUG) {
        m_debug += fmt::format("DRAW0\n{}", draw0.print());
      }
      u32 y0 = (draw0.sprite.xyz0[1] - 512) >> 4;
      pair_idx = y0 / SINGLE_EYE_SIZE;
      if (tex0->get_data_ptr()) {
        u32 tex_val;
        memcpy(&tex_val, tex0->get_data_ptr(), 4);

        for (auto& x : m_left) {
          x = tex_val;
        }
        for (auto& x : m_right) {
          x = tex_val;
        }
      }
    }

    // up next is the pupil background
    {
      auto draw1 = read_eye_draw(dma);
      auto draw2 = read_eye_draw(dma);
      if (DEBUG) {
        m_debug += fmt::format("DRAW1\n{}", draw1.print());
        m_debug += fmt::format("DRAW2\n{}", draw2.print());
      }
      if (tex0->get_data_ptr()) {
        draw_eye<false>(m_left, draw1, *tex0, pair_idx, 0, false, m_use_bilinear);
        draw_eye<false>(m_right, draw2, *tex0, pair_idx, 1, false, m_use_bilinear);
      }
    }

    // now we'll draw the iris on top of that
    auto test1 = dma.read_and_advance();
    (void)test1;
    auto adgif1_dma = dma.read_and_advance();
    ASSERT(adgif1_dma.size_bytes == 96);  // 5 adgifs a+d's plus tag
    ASSERT(adgif1_dma.vif0() == 0);
    ASSERT(adgif1_dma.vifcode1().kind == VifCode::Kind::DIRECT);
    AdgifHelper adgif1(adgif1_dma.data + 16);
    auto tex1 = render_state->texture_pool->lookup_gpu_texture(adgif1.tex0().tbp0());
    if (DEBUG) {
      m_debug += fmt::format("ADGIF1:\n{}\n", adgif1.print());
      m_debug += fmt::format("tex: {}\n", tex1->name);
    }

    {
      auto draw1 = read_eye_draw(dma);
      auto draw2 = read_eye_draw(dma);
      if (DEBUG) {
        m_debug += fmt::format("DRAW1\n{}", draw1.print());
        m_debug += fmt::format("DRAW2\n{}", draw2.print());
      }
      if (tex1->get_data_ptr()) {
        draw_eye<true>(m_left, draw1, *tex1, pair_idx, 0, false, m_use_bilinear);
        draw_eye<true>(m_right, draw2, *tex1, pair_idx, 1, false, m_use_bilinear);
      }
    }

    // and finally the eyelid
    auto test2 = dma.read_and_advance();
    (void)test2;
    auto adgif2_dma = dma.read_and_advance();
    ASSERT(adgif2_dma.size_bytes == 96);  // 5 adgifs a+d's plus tag
    ASSERT(adgif2_dma.vif0() == 0);
    ASSERT(adgif2_dma.vifcode1().kind == VifCode::Kind::DIRECT);
    AdgifHelper adgif2(adgif2_dma.data + 16);
    auto tex2 = render_state->texture_pool->lookup_gpu_texture(adgif2.tex0().tbp0());
    if (DEBUG) {
      m_debug += fmt::format("ADGIF2:\n{}\n", adgif2.print());
      m_debug += fmt::format("tex: {}\n", tex2->name);
    }

    {
      auto draw1 = read_eye_draw(dma);
      auto draw2 = read_eye_draw(dma);
      if (DEBUG) {
        m_debug += fmt::format("DRAW1\n{}", draw1.print());
        m_debug += fmt::format("DRAW2\n{}", draw2.print());
      }
      if (tex2->get_data_ptr()) {
        draw_eye<false>(m_left, draw1, *tex2, pair_idx, 0, false, m_use_bilinear);
        draw_eye<false>(m_right, draw2, *tex2, pair_idx, 1, true, m_use_bilinear);
      }
    }

    auto end = dma.read_and_advance();
    ASSERT(end.size_bytes == 0);
    ASSERT(end.vif0() == 0);
    ASSERT(end.vif1() == 0);

    if (m_alpha_hack) {
      for (auto& a : m_left) {
        a |= 0xff000000;
      }

      for (auto& a : m_right) {
        a |= 0xff000000;
      }
    }

    // update GPU:
    auto& l = m_eye_textures[pair_idx * 2];
    auto& r = m_eye_textures[pair_idx * 2 + 1];
    glBindTexture(GL_TEXTURE_2D, l.gl_tex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 32, 32, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
                 m_left);
    glBindTexture(GL_TEXTURE_2D, r.gl_tex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 32, 32, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
                 m_right);
    // make sure they are still in vram
    render_state->texture_pool->move_existing_to_vram(l.gpu_tex, l.tbp);
    render_state->texture_pool->move_existing_to_vram(r.gpu_tex, r.tbp);
  }

  float time_ms = timer.getMs();
  m_average_time_ms = m_average_time_ms * 0.95 + time_ms * 0.05;
}
