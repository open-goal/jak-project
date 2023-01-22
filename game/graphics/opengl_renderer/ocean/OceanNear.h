#pragma once

#include "game/common/vu.h"
#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/ocean/CommonOceanRenderer.h"
#include "game/graphics/opengl_renderer/ocean/OceanTexture.h"

class OceanNear : public BucketRenderer {
 public:
  OceanNear(const std::string& name, int my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void render_jak1(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof);
  void render_jak2(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof);
  void draw_debug_window() override;
  void init_textures(TexturePool& pool, GameVersion version) override;

 private:
  void run_call0_vu2c();
  void run_call0_vu2c_jak2();
  void run_call39_vu2c();
  void run_call39_vu2c_jak2();
  void run_L15_vu2c();
  void run_L15_vu2c_jak2();
  void run_L21_vu2c();
  void run_L21_vu2c_jak2();
  void run_L23_vu2c();
  void run_L25_vu2c();
  void run_L25_vu2c_jak2();
  void run_L30_vu2c();
  void run_L32_vu2c();

  OceanTexture m_texture_renderer;
  CommonOceanRenderer m_common_ocean_renderer;

  bool m_buffer_toggle = false;
  static constexpr int VU1_INPUT_BUFFER_BASE = 0;
  static constexpr int VU1_INPUT_BUFFER_OFFSET = 0x10;

  void xgkick(u16 addr);

  u16 xtop() {
    m_buffer_toggle = !m_buffer_toggle;
    return get_vu_buffer();
  }

  u16 get_upload_buffer() {
    if (m_buffer_toggle) {
      return VU1_INPUT_BUFFER_OFFSET;
    } else {
      return VU1_INPUT_BUFFER_BASE;
    }
  }

  u16 get_vu_buffer() {
    if (!m_buffer_toggle) {
      return VU1_INPUT_BUFFER_OFFSET;
    } else {
      return VU1_INPUT_BUFFER_BASE;
    }
  }

  void lq_buffer(Mask mask, Vf& dest, u16 addr) {
    ASSERT(addr < 1024);
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        dest[i] = m_vu_data[addr].data[i];
      }
    }
  }

  void sq_buffer(Mask mask, const Vf& val, u16 addr) {
    ASSERT(addr < 1024);
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        m_vu_data[addr].data[i] = val[i];
      }
    }
  }

  void ilw_buffer(Mask mask, u16& dest, u16 addr) {
    ASSERT(addr < 1024);
    switch (mask) {
      case Mask::x:
        dest = m_vu_data[addr].x_as_u16();
        break;
      case Mask::y:
        dest = m_vu_data[addr].y_as_u16();
        break;
      case Mask::z:
        dest = m_vu_data[addr].z_as_u16();
        break;
      case Mask::w:
        dest = m_vu_data[addr].w_as_u16();
        break;
      default:
        ASSERT(false);
    }
  }

  void isw_buffer(Mask mask, u16 src, u16 addr) {
    ASSERT(addr < 1024);
    u32 val32 = src;
    switch (mask) {
      case Mask::x:
        memcpy(&m_vu_data[addr].data[0], &val32, 4);
        break;
      case Mask::y:
        memcpy(&m_vu_data[addr].data[1], &val32, 4);
        break;
      case Mask::z:
        memcpy(&m_vu_data[addr].data[2], &val32, 4);
        break;
      case Mask::w:
        memcpy(&m_vu_data[addr].data[3], &val32, 4);
        break;
      default:
        ASSERT(false);
    }
  }

  Vf m_vu_data[1024];

  struct Vu {
    const Vf vf00;

    Accumulator acc;
    Vf vf01, vf02, vf03, vf04, vf05, vf06, vf07, vf08, vf09, vf10, vf11, vf12, vf13, vf14, vf15,
        vf16, vf17, vf18, vf19, vf20, vf21, vf22, vf23, vf24, vf25, vf26, vf27, vf28, vf29, vf30,
        vf31;
    u16 vi01, vi02, vi03, vi04, vi05, vi06, vi07, vi08, vi09, vi10, vi11, vi12, vi13, vi14;

    float P, Q;
    Vu() : vf00(0, 0, 0, 1) {}

  } vu;
};
