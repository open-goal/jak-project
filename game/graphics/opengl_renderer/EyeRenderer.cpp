#include "EyeRenderer.h"

#include "common/util/FileUtil.h"

#include "game/graphics/opengl_renderer/AdgifHandler.h"

#include "third-party/imgui/imgui.h"

/////////////////////////
// Bucket Renderer
/////////////////////////
EyeRenderer::EyeRenderer(const std::string& name, int id) : BucketRenderer(name, id) {}

void EyeRenderer::init_textures(TexturePool& texture_pool, GameVersion) {
  // set up eyes
  for (int pair_idx = 0; pair_idx < NUM_EYE_PAIRS; pair_idx++) {
    for (int lr = 0; lr < 2; lr++) {
      u32 tidx = pair_idx * 2 + lr;

      // CPU
      {
        GLuint gl_tex;
        glGenTextures(1, &gl_tex);
        u32 tbp = EYE_BASE_BLOCK + pair_idx * 2 + lr;
        TextureInput in;
        in.gpu_texture = gl_tex;
        in.w = 32;
        in.h = 32;
        in.debug_page_name = "PC-EYES";
        in.debug_name = fmt::format("{}-eye-cpu-{}", lr ? "left" : "right", pair_idx);
        in.id = texture_pool.allocate_pc_port_texture();
        auto* gpu_tex = texture_pool.give_texture_and_load_to_vram(in, tbp);
        m_cpu_eye_textures[tidx] = {gl_tex, gpu_tex, tbp};
      }

      // GPU
      {
        u32 tbp = EYE_BASE_BLOCK + pair_idx * 2 + lr;
        TextureInput in;
        in.gpu_texture = m_gpu_eye_textures[tidx].fb.texture();
        in.w = 32;
        in.h = 32;
        in.debug_page_name = "PC-EYES";
        in.debug_name = fmt::format("{}-eye-gpu-{}", lr ? "left" : "right", pair_idx);
        in.id = texture_pool.allocate_pc_port_texture();
        m_gpu_eye_textures[tidx].gpu_tex = texture_pool.give_texture_and_load_to_vram(in, tbp);
        m_gpu_eye_textures[tidx].tbp = tbp;
      }
    }
  }

  // set up vertices for GPU mode
  glGenVertexArrays(1, &m_vao);
  glBindVertexArray(m_vao);
  glGenBuffers(1, &m_gl_vertex_buffer);
  glBindBuffer(GL_ARRAY_BUFFER, m_gl_vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, VTX_BUFFER_FLOATS * sizeof(float), nullptr, GL_STREAM_DRAW);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0,                  // location 0 in the shader
                        4,                  // 2 floats per vert
                        GL_FLOAT,           // floats
                        GL_TRUE,            // normalized, ignored,
                        sizeof(float) * 4,  //
                        (void*)0            // offset in array
  );
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
}

EyeRenderer::~EyeRenderer() {
  glDeleteVertexArrays(1, &m_vao);
  glDeleteBuffers(1, &m_gl_vertex_buffer);
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

  handle_eye_dma2(dma, render_state, prof);

  while (dma.current_tag_offset() != render_state->next_bucket) {
    auto data = dma.read_and_advance();
    m_debug += fmt::format("dma: {}\n", data.size_bytes);
  }
}

void EyeRenderer::draw_debug_window() {
  ImGui::Checkbox("Use GPU", &m_use_gpu);
  ImGui::Text("Time: %.3f ms\n", m_average_time_ms);
  ImGui::Text("Debug:\n%s", m_debug.c_str());
  if (!m_use_gpu) {
    ImGui::Checkbox("bilinear", &m_use_bilinear);
  }
  ImGui::Checkbox("alpha hack", &m_alpha_hack);
}

//////////////////////
// DMA Decode
//////////////////////

EyeRenderer::ScissorInfo decode_scissor(const DmaTransfer& dma) {
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
  EyeRenderer::ScissorInfo result;
  u64 val;
  memcpy(&val, dma.data + 16, 8);
  GsScissor reg(val);
  result.x0 = reg.x0();
  result.x1 = reg.x1();
  result.y0 = reg.y0();
  result.y1 = reg.y1();
  return result;
}

EyeRenderer::SpriteInfo decode_sprite(const DmaTransfer& dma) {
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

  EyeRenderer::SpriteInfo result;

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

EyeRenderer::EyeDraw read_eye_draw(DmaFollower& dma) {
  auto scissor = decode_scissor(dma.read_and_advance());
  auto sprite = decode_sprite(dma.read_and_advance());
  return {sprite, scissor};
}

std::vector<EyeRenderer::SingleEyeDraws> EyeRenderer::get_draws(DmaFollower& dma,
                                                                SharedRenderState* render_state) {
  std::vector<SingleEyeDraws> draws;
  // now, loop over eyes. end condition is a 8 qw transfer to restore gs.
  while (dma.current_tag().qwc != 8) {
    draws.emplace_back();
    draws.emplace_back();

    auto& l_draw = draws[draws.size() - 2];
    auto& r_draw = draws[draws.size() - 1];

    l_draw.lr = 0;
    r_draw.lr = 1;

    // eye background setup
    auto adgif0_dma = dma.read_and_advance();
    ASSERT(adgif0_dma.size_bytes == 96);  // 5 adgifs a+d's plus tag
    ASSERT(adgif0_dma.vif0() == 0);
    ASSERT(adgif0_dma.vifcode1().kind == VifCode::Kind::DIRECT);
    AdgifHelper adgif0(adgif0_dma.data + 16);
    auto tex0 = render_state->texture_pool->lookup_gpu_texture(adgif0.tex0().tbp0());

    u32 pair_idx = -1;
    // first draw. this is the background. It reads 0,0 of the texture uses that color everywhere.
    // we'll also figure out the eye index here.
    {
      auto draw0 = read_eye_draw(dma);
      ASSERT(draw0.sprite.uv0[0] == 0);
      ASSERT(draw0.sprite.uv0[1] == 0);
      ASSERT(draw0.sprite.uv1[0] == 0);
      ASSERT(draw0.sprite.uv1[1] == 0);
      u32 y0 = (draw0.sprite.xyz0[1] - 512) >> 4;
      pair_idx = y0 / SINGLE_EYE_SIZE;
      l_draw.pair = pair_idx;
      r_draw.pair = pair_idx;
      if (tex0 && tex0->get_data_ptr()) {
        u32 tex_val;
        memcpy(&tex_val, tex0->get_data_ptr(), 4);
        l_draw.clear_color = tex_val;
        r_draw.clear_color = tex_val;
      } else {
        l_draw.clear_color = 0;
        r_draw.clear_color = 0;
      }
    }

    // up next is the pupil background
    {
      l_draw.iris = read_eye_draw(dma);
      r_draw.iris = read_eye_draw(dma);
      l_draw.iris_tex = tex0;
      r_draw.iris_tex = tex0;
      l_draw.iris_gl_tex = *render_state->texture_pool->lookup(adgif0.tex0().tbp0());
      r_draw.iris_gl_tex = l_draw.iris_gl_tex;
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

    if (tex1 && tex1->get_data_ptr()) {
      l_draw.pupil = read_eye_draw(dma);
      r_draw.pupil = read_eye_draw(dma);
      l_draw.pupil_tex = tex1;
      r_draw.pupil_tex = tex1;
      l_draw.pupil_gl_tex = *render_state->texture_pool->lookup(adgif1.tex0().tbp0());
      r_draw.pupil_gl_tex = l_draw.pupil_gl_tex;
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

    {
      l_draw.lid = read_eye_draw(dma);
      r_draw.lid = read_eye_draw(dma);
      l_draw.lid_tex = tex2;
      r_draw.lid_tex = tex2;
      l_draw.lid_gl_tex = *render_state->texture_pool->lookup(adgif2.tex0().tbp0());
      r_draw.lid_gl_tex = l_draw.lid_gl_tex;
    }

    auto end = dma.read_and_advance();
    ASSERT(end.size_bytes == 0);
    ASSERT(end.vif0() == 0);
    ASSERT(end.vif1() == 0);
  }
  return draws;
}

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

  auto draws = get_draws(dma, render_state);
  if (m_use_gpu) {
    run_gpu(draws, render_state);
  } else {
    run_cpu(draws, render_state);
  }

  float time_ms = timer.getMs();
  m_average_time_ms = m_average_time_ms * 0.95 + time_ms * 0.05;
}

//////////////////////
// CPU Drawing
//////////////////////

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
                   const EyeRenderer::EyeDraw& draw,
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
              const EyeRenderer::EyeDraw& draw,
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

void EyeRenderer::run_cpu(const std::vector<SingleEyeDraws>& draws,
                          SharedRenderState* render_state) {
  for (auto& draw : draws) {
    for (auto& x : m_temp_tex) {
      x = draw.clear_color;
    }

    if (draw.iris_tex) {
      draw_eye<false>(m_temp_tex, draw.iris, *draw.iris_tex, draw.pair, draw.lr, false,
                      m_use_bilinear);
    }

    if (draw.pupil_tex) {
      draw_eye<true>(m_temp_tex, draw.pupil, *draw.pupil_tex, draw.pair, draw.lr, false,
                     m_use_bilinear);
    }

    if (draw.lid_tex) {
      draw_eye<false>(m_temp_tex, draw.lid, *draw.lid_tex, draw.pair, draw.lr, draw.lr == 1,
                      m_use_bilinear);
    }

    if (m_alpha_hack) {
      for (auto& a : m_temp_tex) {
        a |= 0xff000000;
      }
    }

    // update GPU:
    auto& tex = m_cpu_eye_textures[draw.pair * 2 + draw.lr];
    glBindTexture(GL_TEXTURE_2D, tex.gl_tex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 32, 32, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
                 m_temp_tex);
    // make sure they are still in vram
    render_state->texture_pool->move_existing_to_vram(tex.gpu_tex, tex.tbp);
  }
}

int add_draw_to_buffer(int idx, const EyeRenderer::EyeDraw& draw, float* data, int pair, int lr) {
  int x_off = lr * SINGLE_EYE_SIZE * 16;
  int y_off = pair * SINGLE_EYE_SIZE * 16;
  data[idx++] = draw.sprite.xyz0[0] - x_off;
  data[idx++] = draw.sprite.xyz0[1] - y_off;
  data[idx++] = 0;
  data[idx++] = 0;

  data[idx++] = draw.sprite.xyz1[0] - x_off;
  data[idx++] = draw.sprite.xyz0[1] - y_off;
  data[idx++] = 1;
  data[idx++] = 0;

  data[idx++] = draw.sprite.xyz0[0] - x_off;
  data[idx++] = draw.sprite.xyz1[1] - y_off;
  data[idx++] = 0;
  data[idx++] = 1;

  data[idx++] = draw.sprite.xyz1[0] - x_off;
  data[idx++] = draw.sprite.xyz1[1] - y_off;
  data[idx++] = 1;
  data[idx++] = 1;
  return idx;
}

void EyeRenderer::run_gpu(const std::vector<SingleEyeDraws>& draws,
                          SharedRenderState* render_state) {
  if (draws.empty()) {
    return;
  }

  glBindVertexArray(m_vao);
  glBindBuffer(GL_ARRAY_BUFFER, m_gl_vertex_buffer);

  // the first thing we'll do is prepare the vertices
  int buffer_idx = 0;
  for (const auto& draw : draws) {
    buffer_idx = add_draw_to_buffer(buffer_idx, draw.iris, m_gpu_vertex_buffer, draw.pair, draw.lr);
    buffer_idx =
        add_draw_to_buffer(buffer_idx, draw.pupil, m_gpu_vertex_buffer, draw.pair, draw.lr);
    buffer_idx = add_draw_to_buffer(buffer_idx, draw.lid, m_gpu_vertex_buffer, draw.pair, draw.lr);
  }
  ASSERT(buffer_idx <= VTX_BUFFER_FLOATS);
  int check = buffer_idx;

  // maybe buffer sub data.
  glBufferData(GL_ARRAY_BUFFER, buffer_idx * sizeof(float), m_gpu_vertex_buffer, GL_STREAM_DRAW);

  FramebufferTexturePairContext ctxt(m_gpu_eye_textures[draws.front().tex_slot()].fb);

  // set up common opengl state
  glDisable(GL_DEPTH_TEST);
  render_state->shaders[ShaderId::EYE].activate();
  glUniform1i(glGetUniformLocation(render_state->shaders[ShaderId::EYE].id(), "tex_T0"), 0);
  glActiveTexture(GL_TEXTURE0);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  buffer_idx = 0;
  for (size_t draw_idx = 0; draw_idx < draws.size(); draw_idx++) {
    const auto& draw = draws[draw_idx];
    const auto& out_tex = m_gpu_eye_textures[draw.tex_slot()];

    // first, the clear
    float clear[4] = {0, 0, 0, 0};
    for (int i = 0; i < 4; i++) {
      clear[i] = ((draw.clear_color >> (8 * i)) & 0xff) / 255.f;
    }
    glClearBufferfv(GL_COLOR, 0, clear);

    // iris
    if (draw.iris_tex) {
      // set alpha
      // set Z
      // set texture
      glDisable(GL_BLEND);
      glBindTexture(GL_TEXTURE_2D, draw.iris_gl_tex);
      glDrawArrays(GL_TRIANGLE_STRIP, buffer_idx / 4, 4);
    }
    buffer_idx += 4 * 4;

    if (draw.pupil_tex) {
      glEnable(GL_BLEND);
      glBlendEquation(GL_FUNC_ADD);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glBindTexture(GL_TEXTURE_2D, draw.pupil_gl_tex);
      glDrawArrays(GL_TRIANGLE_STRIP, buffer_idx / 4, 4);
    }
    buffer_idx += 4 * 4;

    if (draw.lid_tex) {
      glDisable(GL_BLEND);
      glBindTexture(GL_TEXTURE_2D, draw.lid_gl_tex);
      glDrawArrays(GL_TRIANGLE_STRIP, buffer_idx / 4, 4);
    }
    buffer_idx += 4 * 4;

    // finally, give to "vram"
    render_state->texture_pool->move_existing_to_vram(out_tex.gpu_tex, out_tex.tbp);

    if (draw_idx != draws.size() - 1) {
      ctxt.switch_to(m_gpu_eye_textures[draws[draw_idx + 1].tex_slot()].fb);
    }
  }

  ASSERT(check == buffer_idx);

  glBindVertexArray(0);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
}

//////////////////////
// DMA Decode
//////////////////////

std::string EyeRenderer::SpriteInfo::print() const {
  std::string result;
  result +=
      fmt::format("a: {:x} uv: ({}, {}), ({}, {}) xyz: ({}, {}, {}), ({}, {}, {})", a, uv0[0],
                  uv0[1], uv1[0], uv1[1], xyz0[0], xyz0[1], xyz0[2], xyz1[0], xyz1[1], xyz1[2]);
  return result;
}

std::string EyeRenderer::ScissorInfo::print() const {
  return fmt::format("x : [{}, {}], y : [{}, {}]", x0, x1, y0, y1);
}

std::string EyeRenderer::EyeDraw::print() const {
  return fmt::format("{}\n{}\n", sprite.print(), scissor.print());
}