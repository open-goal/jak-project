#include "OceanEnvmap.h"

#include <cstring>

#include "common/dma/gs.h"

#include "game/graphics/texture/TexturePool.h"

#include "fmt/format.h"
#include "third-party/imgui/imgui.h"

namespace {

/*!
 * check if a gif packet from a dma-buffer-add-gs-set contains a given register address
 */
bool scan_gs_set(const u8* data, const u32 size, GsRegisterAddress reg, u64* out) {
  if (size < 16) {
    return false;
  }
  GifTag tag(data);
  if (tag.flg() != GifTag::Format::PACKED) {
    return false;
  }
  u32 nreg = tag.nreg();
  u32 offset = 16;
  for (u32 loop = 0; loop < tag.nloop(); loop++) {
    for (u32 r = 0; r < nreg; r++) {
      if (offset + 16 > size) {
        return false;
      }
      if (tag.reg(r) == GifTag::RegisterDescriptor::AD) {
        u64 value;
        u8 addr;
        memcpy(&value, data + offset, sizeof(value));
        memcpy(&addr, data + offset + 8, sizeof(addr));
        if (addr == (u8)reg) {
          *out = value;
          return true;
        }
      }
      offset += 16;
    }
  }
  return false;
}

bool is_untextured_draw(const u8* data, u32 size) {
  if (size < 16) {
    return false;
  }
  GifTag tag(data);
  if (!tag.pre()) {
    return false;
  }
  return !GsPrim(tag.prim()).tme();
}

bool find_sky_color(DmaFollower dma, u32 next_bucket, u8 out[4]) {
  for (int i = 0; i < 256 && dma.current_tag_offset() != next_bucket; i++) {
    auto d = dma.read_and_advance();
    if (d.size_bytes >= 32 && is_untextured_draw(d.data, d.size_bytes)) {
      out[0] = d.data[16 + 0];
      out[1] = d.data[16 + 4];
      out[2] = d.data[16 + 8];
      out[3] = d.data[16 + 12];
      return true;
    }
  }
  return false;
}
}  // namespace

OceanEnvmap::OceanEnvmap(const std::string& name, int my_id, int batch_size)
    : DirectRenderer(name, my_id, batch_size),
      m_first_pass_fb(ENVMAP_WIDTH, ENVMAP_HEIGHT, GL_UNSIGNED_INT_8_8_8_8_REV),
      m_envmap_fb(ENVMAP_WIDTH, ENVMAP_HEIGHT, GL_UNSIGNED_INT_8_8_8_8_REV) {}

static void make_single_level_linear(const GLuint tex) {
  glBindTexture(GL_TEXTURE_2D, tex);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, 0);
  glBindTexture(GL_TEXTURE_2D, 0);
}

void OceanEnvmap::init_textures(TexturePool& pool, GameVersion version) {
  TextureInput in;
  in.w = ENVMAP_WIDTH;
  in.h = ENVMAP_HEIGHT;
  in.debug_page_name = "PC-OCEAN-ENVMAP";

  in.gpu_texture = m_envmap_fb.texture();
  in.debug_name = "ocean-envmap";
  in.id = pool.allocate_pc_port_texture(version);
  m_envmap_gpu_tex = pool.give_texture_and_load_to_vram(in, ENVMAP_VRAM_ADDR);

  make_single_level_linear(m_first_pass_fb.texture());
  make_single_level_linear(m_envmap_fb.texture());

  glGenVertexArrays(1, &m_radial_vao);
  glBindVertexArray(m_radial_vao);
  glGenBuffers(1, &m_radial_vbo);
  glBindBuffer(GL_ARRAY_BUFFER, m_radial_vbo);
  const float verts[8] = {-1.f, -1.f, -1.f, 1.f, 1.f, -1.f, 1.f, 1.f};
  glBufferData(GL_ARRAY_BUFFER, sizeof(verts), verts, GL_STATIC_DRAW);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 2 * sizeof(float), nullptr);
  glBindVertexArray(0);
  glBindBuffer(GL_ARRAY_BUFFER, 0);

  glGenVertexArrays(1, &m_haze_vao);
  glBindVertexArray(m_haze_vao);
  glGenBuffers(1, &m_haze_vbo);
  glBindBuffer(GL_ARRAY_BUFFER, m_haze_vbo);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(float) * 6, (void*)nullptr);
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, sizeof(float) * 6, (void*)(2 * sizeof(float)));
  glBindVertexArray(0);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
}

void OceanEnvmap::render_haze(const u8* gif_data, u32 size, SharedRenderState* render_state) {
  if (size < 16) {
    return;
  }
  const float x_off = m_prim_buffer.x_off;
  const float y_off = m_prim_buffer.y_off;

  GifTag tag(gif_data);
  if (tag.flg() != GifTag::Format::PACKED) {
    return;
  }
  const u32 nreg = tag.nreg();

  std::vector<float> verts;
  verts.reserve(tag.nloop() * 2 * 6);
  float cur[4] = {1.f, 1.f, 1.f, 1.f};
  u32 offset = 16;
  for (u32 loop = 0; loop < tag.nloop(); loop++) {
    for (u32 r = 0; r < nreg; r++) {
      if (offset + 16 > size) {
        break;
      }
      const u8* d = gif_data + offset;
      switch (tag.reg(r)) {
        case GifTag::RegisterDescriptor::RGBAQ:
          cur[0] = d[0] / 255.f;
          cur[1] = d[4] / 255.f;
          cur[2] = d[8] / 255.f;
          cur[3] = d[12] / 255.f;
          break;
        case GifTag::RegisterDescriptor::XYZF2: {
          u16 rawx, rawy;
          memcpy(&rawx, d + 0, 2);
          memcpy(&rawy, d + 4, 2);
          float px = rawx / 65536.f + x_off;
          float py = rawy / 65536.f + y_off;
          float ndc_x = (px - 0.453125f) * 64.f;
          float ndc_y = (py - 0.5f + (2.25f / 64.f)) * 64.f;
          verts.insert(verts.end(), {ndc_x, ndc_y, cur[0], cur[1], cur[2], cur[3] * 2.f});
        } break;
        default:
          break;
      }
      offset += 16;
    }
  }
  if (verts.size() < 6 * 3) {
    return;
  }

  GLboolean depth = glIsEnabled(GL_DEPTH_TEST);
  glDisable(GL_DEPTH_TEST);
  glEnable(GL_BLEND);
  glBlendEquation(GL_FUNC_ADD);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE);

  render_state->shaders[ShaderId::OCEAN_ENVMAP_HAZE].activate();
  glBindVertexArray(m_haze_vao);
  glBindBuffer(GL_ARRAY_BUFFER, m_haze_vbo);
  glBufferData(GL_ARRAY_BUFFER, verts.size() * sizeof(float), verts.data(), GL_STREAM_DRAW);
  glDrawArrays(GL_TRIANGLE_STRIP, 0, (GLsizei)(verts.size() / 6));
  glBindVertexArray(0);
  glBindBuffer(GL_ARRAY_BUFFER, 0);

  if (depth) {
    glEnable(GL_DEPTH_TEST);
  }
}

void OceanEnvmap::render_envmap(SharedRenderState* render_state) {
  GLboolean blend = glIsEnabled(GL_BLEND);
  GLboolean depth_test = glIsEnabled(GL_DEPTH_TEST);
  glDisable(GL_BLEND);
  glDisable(GL_DEPTH_TEST);

  FramebufferTexturePairContext ctxt(m_envmap_fb);
  glViewport(0, 0, ENVMAP_WIDTH, ENVMAP_HEIGHT);
  auto shader = &render_state->shaders[ShaderId::OCEAN_ENVMAP];
  shader->activate();
  glActiveTexture(GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_2D, m_first_pass_fb.texture());
  glBindVertexArray(m_radial_vao);
  glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
  glBindVertexArray(0);
  glBindTexture(GL_TEXTURE_2D, 0);
  glActiveTexture(GL_TEXTURE0);

  if (blend) {
    glEnable(GL_BLEND);
  }
  if (depth_test) {
    glEnable(GL_DEPTH_TEST);
  }
}

void OceanEnvmap::handle_ocean_envmap_jak2(DmaFollower& dma,
                                           SharedRenderState* render_state,
                                           ScopedProfilerNode& prof) {
  u8 sky[4] = {0, 0, 0, 255};
  find_sky_color(dma, render_state->next_bucket, sky);

  auto scissor_backup = m_scissor;

  bool active = false;
  bool registered = false;
  int setup64_count = 0;

  for (int guard = 0; guard < 4096; guard++) {
    DmaFollower peek = dma;
    if (peek.current_tag_offset() == render_state->next_bucket) {
      break;
    }
    auto next = peek.read_and_advance();
    u64 scissor = 0;
    u64 frame = 0;
    bool has_scissor =
        scan_gs_set(next.data, next.size_bytes, GsRegisterAddress::SCISSOR_1, &scissor);
    bool has_frame = scan_gs_set(next.data, next.size_bytes, GsRegisterAddress::FRAME_1, &frame);
    if (has_scissor && GsScissor(scissor).x1() == 127) {
      break;
    }
    bool is_reset = has_scissor && GsScissor(scissor).x1() != ENVMAP_WIDTH - 1;

    auto data = dma.read_and_advance();

    if (has_scissor && GsScissor(scissor).x1() == ENVMAP_WIDTH - 1) {
      setup64_count++;
      u32 page_tbp = has_frame ? GsFrame(frame).fbp() << 5 : 0;
      if (active) {
        flush_pending(render_state, prof);
      }
      m_fb_ctxt.reset();
      reset_state();
      if (setup64_count == 1) {
        m_offscreen_mode = true;
        m_fb_ctxt.emplace(m_first_pass_fb);
        glViewport(0, 0, ENVMAP_WIDTH * 2, ENVMAP_HEIGHT * 2);
        glClearColor(sky[0] / 255.f, sky[1] / 255.f, sky[2] / 255.f, sky[3] / 255.f);
        glClear(GL_COLOR_BUFFER_BIT);
        active = true;
      } else if (setup64_count == 2) {
        m_offscreen_mode = false;
        if (page_tbp) {
          render_state->texture_pool->move_existing_to_vram(m_envmap_gpu_tex, page_tbp);
          registered = true;
        }
      }
    }

    if (active && setup64_count == 1 && !is_reset && data.size_bytes >= 16 &&
        data.vifcode1().kind == VifCode::Kind::DIRECT &&
        !is_untextured_draw(data.data, data.size_bytes)) {
      render_gif(data.data, data.size_bytes, render_state, prof);
    }

    // "haze" effect
    if (active && setup64_count == 1 && !is_reset && data.size_bytes >= 16 &&
        is_untextured_draw(data.data, data.size_bytes) &&
        GsPrim(GifTag(data.data).prim()).kind() == GsPrim::Kind::TRI_STRIP) {
      flush_pending(render_state, prof);
      render_haze(data.data, data.size_bytes, render_state);
      reinitialize_gl_state();
    }
  }

  if (active) {
    flush_pending(render_state, prof);
    m_fb_ctxt.reset();
    m_offscreen_mode = false;
    render_envmap(render_state);
    m_scissor = scissor_backup;
    if (!registered) {
      render_state->texture_pool->move_existing_to_vram(m_envmap_gpu_tex, ENVMAP_VRAM_ADDR);
    }
  }
}

void OceanEnvmap::draw_debug_window() {
  ImGui::Text("first pass        envmap");
  ImGui::Image((ImTextureID)(intptr_t)m_first_pass_fb.texture(),
               ImVec2(ENVMAP_WIDTH * 2, ENVMAP_HEIGHT * 2));
  ImGui::SameLine();
  ImGui::Image((ImTextureID)(intptr_t)m_envmap_fb.texture(),
               ImVec2(ENVMAP_WIDTH * 2, ENVMAP_HEIGHT * 2));
}
