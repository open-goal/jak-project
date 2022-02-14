#include "EyeRenderer.h"
#include "game/graphics/opengl_renderer/AdgifHandler.h"

#include "common/util/FileUtil.h"
#include "third-party/imgui/imgui.h"

EyeRenderer::EyeRenderer(const std::string& name, BucketId id) : BucketRenderer(name, id) {}

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

  handle_eye_dma(dma, render_state, prof);

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

void eye_draw_fill(u32* eye_tex_data, u32 y0, u32 y1, u32 val) {
  u32 idx_start = y0 * EYE_TEX_WIDTH;
  u32 idx_end = y1 * EYE_TEX_WIDTH;
  for (u32 i = idx_start; i < idx_end; i++) {
    eye_tex_data[i] = val;
  }
}

void eye_draw_slow(u32* eye_tex_data,
                   int x0,
                   int x1,
                   int y0,
                   int y1,
                   u32 sx0,
                   u32 sx1,
                   u32 sy0,
                   u32 sy1,
                   const TextureRecord& tex) {
  x0 = (x0 - 512) >> 4;
  y0 = (y0 - 512) >> 4;
  x1 = (x1 - 512) >> 4;
  y1 = (y1 - 512) >> 4;

  for (int y = y0; y < y1; y++) {
    if (y < sy0 || y > sy1) {
      continue;
    }
    float y_texf = float(y - y0) / float(y1 - y0);
    int y_tex = y_texf * tex.h;
    for (int x = x0; x < x1; x++) {
      if (x < sx0 || x > sx1) {
        continue;
      }

      float x_texf = float(x - x0) / float(x1 - x0);
      int x_tex = x_texf * tex.w;
      u32 val;
      memcpy(&val, tex.data.data() + (4 * (x_tex + y_tex * tex.w)), 4);
      eye_tex_data[x + y * EYE_TEX_WIDTH] = val;
    }
  }
}

void eye_draw_slow_alpha(u32* eye_tex_data,
                         int x0,
                         int x1,
                         int y0,
                         int y1,
                         u32 sx0,
                         u32 sx1,
                         u32 sy0,
                         u32 sy1,
                         const TextureRecord& tex) {
  x0 = (x0 - 512) >> 4;
  y0 = (y0 - 512) >> 4;
  x1 = (x1 - 512) >> 4;
  y1 = (y1 - 512) >> 4;

  for (int y = y0; y < y1; y++) {
    if (y < sy0 || y > sy1) {
      continue;
    }
    float y_texf = float(y - y0) / float(y1 - y0);
    int y_tex = y_texf * tex.h;
    for (int x = x0; x < x1; x++) {
      if (x < sx0 || x > sx1) {
        continue;
      }

      float x_texf = float(x - x0) / float(x1 - x0);
      int x_tex = x_texf * tex.w;
      u32 val;
      memcpy(&val, tex.data.data() + (4 * (x_tex + y_tex * tex.w)), 4);
      if ((val >> 24) == 0) {
        continue;
      }
      eye_tex_data[x + y * EYE_TEX_WIDTH] = val;
    }
  }
}

constexpr int EYE_BASE_BLOCK = 8160;

void EyeRenderer::handle_eye_dma(DmaFollower& dma,
                                 SharedRenderState* render_state,
                                 ScopedProfilerNode& prof) {
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

  // begin eyes loop (TODO)

  // from the add to bucket
  ASSERT(dma.current_tag().kind == DmaTag::Kind::NEXT);
  ASSERT(dma.current_tag().qwc == 0);
  ASSERT(dma.current_tag_vif0() == 0);
  ASSERT(dma.current_tag_vif1() == 0);
  dma.read_and_advance();

  int num_drawn = 0;
  while (dma.current_tag().qwc != 8) {
    num_drawn++;
    //  - adgif0
    auto adgif0_dma = dma.read_and_advance();
    ASSERT(adgif0_dma.size_bytes == 96);  // 5 adgifs a+d's plus tag
    ASSERT(adgif0_dma.vif0() == 0);
    ASSERT(adgif0_dma.vifcode1().kind == VifCode::Kind::DIRECT);
    AdgifHelper adgif0(adgif0_dma.data + 16);
    m_debug += fmt::format("ADGIF0:\n{}\n", adgif0.print());

    auto tex0 = render_state->texture_pool->lookup(adgif0.tex0().tbp0());
    m_debug += fmt::format("tex: {}\n", tex0->name);

    //  - scissor0
    //  - sprite0
    {
      // first draw! this is the clear. It reads 0,0 of the texture.
      auto draw0 = read_eye_draw(dma);
      ASSERT(draw0.sprite.uv0[0] == 0);
      ASSERT(draw0.sprite.uv0[1] == 0);
      ASSERT(draw0.sprite.uv1[0] == 0);
      ASSERT(draw0.sprite.uv1[1] == 0);
      m_debug += fmt::format("DRAW0\n{}", draw0.print());
      u32 tex_val;
      memcpy(&tex_val, tex0->data.data(), 4);
      u32 y0 = (draw0.sprite.xyz0[1] - 512) >> 4;
      u32 y1 = (draw0.sprite.xyz1[1] - 512) >> 4;
      eye_draw_fill(render_state->eye_texture.data(), y0, y1, tex_val);
    }

    //  - scissor1
    //  - sprite1
    auto draw1 = read_eye_draw(dma);
    m_debug += fmt::format("DRAW1\n{}", draw1.print());
    /*
     *             u32 x0,
                   u32 x1,
                   u32 y0,
                   u32 y1,
                   u32 sx0,
                   u32 sx1,
                   u32 sy0,
                   u32 sy1,
     */
    eye_draw_slow(render_state->eye_texture.data(), draw1.sprite.xyz0[0], draw1.sprite.xyz1[0],
                  draw1.sprite.xyz0[1], draw1.sprite.xyz1[1], draw1.scissor.x0, draw1.scissor.x1,
                  draw1.scissor.y0, draw1.scissor.y1, *tex0);

    //  - scissor2
    //  - sprite2
    auto draw2 = read_eye_draw(dma);
    m_debug += fmt::format("DRAW2\n{}", draw2.print());
    eye_draw_slow(render_state->eye_texture.data(), draw2.sprite.xyz0[0], draw2.sprite.xyz1[0],
                  draw2.sprite.xyz0[1], draw2.sprite.xyz1[1], draw2.scissor.x0, draw2.scissor.x1,
                  draw2.scissor.y0, draw2.scissor.y1, *tex0);

    //  - ztest/alphas
    auto test1 = dma.read_and_advance();

    //  - adgif1
    auto adgif1_dma = dma.read_and_advance();
    ASSERT(adgif1_dma.size_bytes == 96);  // 5 adgifs a+d's plus tag
    ASSERT(adgif1_dma.vif0() == 0);
    ASSERT(adgif1_dma.vifcode1().kind == VifCode::Kind::DIRECT);
    AdgifHelper adgif1(adgif1_dma.data + 16);
    m_debug += fmt::format("ADGIF1:\n{}\n", adgif1.print());
    auto tex1 = render_state->texture_pool->lookup(adgif1.tex0().tbp0());
    m_debug += fmt::format("tex: {}\n", tex1->name);

    //  - scissor0
    //  - sprite0
    auto draw3 = read_eye_draw(dma);
    m_debug += fmt::format("DRAW3\n{}", draw3.print());
    eye_draw_slow_alpha(render_state->eye_texture.data(), draw3.sprite.xyz0[0],
                        draw3.sprite.xyz1[0], draw3.sprite.xyz0[1], draw3.sprite.xyz1[1],
                        draw3.scissor.x0, draw3.scissor.x1, draw3.scissor.y0, draw3.scissor.y1,
                        *tex1);
    //  - scissor1
    //  - sprite1
    auto draw4 = read_eye_draw(dma);
    m_debug += fmt::format("DRAW4\n{}", draw4.print());
    eye_draw_slow_alpha(render_state->eye_texture.data(), draw4.sprite.xyz0[0],
                        draw4.sprite.xyz1[0], draw4.sprite.xyz0[1], draw4.sprite.xyz1[1],
                        draw4.scissor.x0, draw4.scissor.x1, draw4.scissor.y0, draw4.scissor.y1,
                        *tex1);

    //  - ztest/alphas
    auto test2 = dma.read_and_advance();

    auto adgif2_dma = dma.read_and_advance();
    ASSERT(adgif2_dma.size_bytes == 96);  // 5 adgifs a+d's plus tag
    ASSERT(adgif2_dma.vif0() == 0);
    ASSERT(adgif2_dma.vifcode1().kind == VifCode::Kind::DIRECT);
    AdgifHelper adgif2(adgif2_dma.data + 16);
    m_debug += fmt::format("ADGIF2:\n{}\n", adgif2.print());
    auto tex2 = render_state->texture_pool->lookup(adgif2.tex0().tbp0());
    m_debug += fmt::format("tex: {}\n", tex2->name);

    //  - scissor
    //  - sprite
    auto draw5 = read_eye_draw(dma);
    m_debug += fmt::format("DRAW5\n{}", draw5.print());
    eye_draw_slow(render_state->eye_texture.data(), draw5.sprite.xyz0[0], draw5.sprite.xyz1[0],
                  draw5.sprite.xyz0[1], draw5.sprite.xyz1[1], draw5.scissor.x0, draw5.scissor.x1,
                  draw5.scissor.y0, draw5.scissor.y1, *tex2);
    //  - scissor
    //  - sprite
    auto draw6 = read_eye_draw(dma);
    m_debug += fmt::format("DRAW6\n{}", draw6.print());
    eye_draw_slow(render_state->eye_texture.data(), draw6.sprite.xyz1[0], draw6.sprite.xyz0[0],
                  draw6.sprite.xyz0[1], draw6.sprite.xyz1[1], draw6.scissor.x0, draw6.scissor.x1,
                  draw6.scissor.y0, draw6.scissor.y1, *tex2);

    auto end = dma.read_and_advance();
    ASSERT(end.size_bytes == 0);
    ASSERT(end.vif0() == 0);
    ASSERT(end.vif1() == 0);
  }

  // how many pairs of eyes.
  u32 temp_upload[32 * 32];
  for (int i = 0; i < num_drawn; i++) {
    TextureRecord* left_eye = render_state->texture_pool->lookup(EYE_BASE_BLOCK + i * 2);
    if (!left_eye) {
      // no eye texture... need to create it.
      auto left = std::make_shared<TextureRecord>();
      left->name = fmt::format("left-eye-{}", i);
      left->only_on_gpu = true;
      left->on_gpu = true;
      left->do_gc = false;
      left->w = 32;
      left->h = 32;
      GLuint tex;
      glGenTextures(1, &tex);
      left->gpu_texture = tex;
      render_state->texture_pool->set_texture(EYE_BASE_BLOCK + i * 2, left);

      left_eye = left.get();
    }

    TextureRecord* right_eye = render_state->texture_pool->lookup(EYE_BASE_BLOCK + i * 2 + 1);
    if (!right_eye) {
      // no eye texture... need to create it.
      auto right = std::make_shared<TextureRecord>();
      right->name = fmt::format("right-eye-{}", i);
      right->only_on_gpu = true;
      right->on_gpu = true;
      right->do_gc = false;
      right->w = 32;
      right->h = 32;
      GLuint tex;
      glGenTextures(1, &tex);
      right->gpu_texture = tex;
      render_state->texture_pool->set_texture(EYE_BASE_BLOCK + i * 2 + 1, right);
      right_eye = right.get();
    }

    // copy left to temp
    for (int y = 0; y < 32; y++) {
      for (int x = 0; x < 32; x++) {
        temp_upload[y * 32 + x] = render_state->eye_texture[(i * 32 + y) * 64 + x];
      }
    }

    glBindTexture(GL_TEXTURE_2D, left_eye->gpu_texture);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 32, 32, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
                 temp_upload);

    // copy right to temp
    for (int y = 0; y < 32; y++) {
      for (int x = 0; x < 32; x++) {
        temp_upload[y * 32 + x] = render_state->eye_texture[(i * 32 + y) * 64 + x + 32];
      }
    }

    glBindTexture(GL_TEXTURE_2D, right_eye->gpu_texture);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 32, 32, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
                 temp_upload);
  }

  //  file_util::write_rgba_png(file_util::get_file_path({"debug_out/eyes.png"}),
  //  render_state->eye_texture.data(), EYE_TEX_WIDTH, EYE_TEX_HEIGHT, false);
}

void EyeRenderer::draw_debug_window() {
  ImGui::Text("Debug:\n%s", m_debug.c_str());
}