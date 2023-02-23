#pragma once

#include "game/common/vu.h"
#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"
#include "game/graphics/opengl_renderer/ocean/CommonOceanRenderer.h"

class OceanMid {
 public:
  OceanMid();
  void run(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof);
  void run_jak2(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof);

 private:
  void run_call0();
  void run_call0_vu2c();
  void run_call41_vu2c();
  void run_call43_vu2c();
  void run_call46_vu2c();
  void run_call73_vu2c();
  void run_call73_vu2c_jak2();
  void run_call107_vu2c();
  void run_call107_vu2c_jak2();
  void run_call275_vu2c();
  void run_call275_vu2c_jak2();
  void xgkick(u16 addr);

  void run_L26_vu2c();
  void run_L32_vu2c();
  void run_L32_vu2c_jak2();
  void run_L38_vu2c();
  void run_L38_vu2c_jak2();
  void run_L43_vu2c();
  void run_L45_vu2c();

  CommonOceanRenderer m_common_ocean_renderer;
  bool m_buffer_toggle = false;
  static constexpr int VU1_INPUT_BUFFER_BASE = 0;
  static constexpr int VU1_INPUT_BUFFER_OFFSET = 0x76;

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

  // (deftype ocean-mid-constants (structure)
  struct Constants {
    //  ((hmge-scale     vector       :inline :offset-assert 0)
    math::Vector4f hmge_scale;
    //   (inv-hmge-scale vector       :inline :offset-assert 16)
    math::Vector4f inv_hmge_scale;
    //   (hvdf-offset    vector       :inline :offset-assert 32)
    math::Vector4f hvdf_offset;
    //   (fog            vector       :inline :offset-assert 48)
    math::Vector4f fog;
    //   (constants      vector       :inline :offset-assert 64)
    math::Vector4f constants;
    //   (constants2     vector       :inline :offset-assert 80)
    math::Vector4f constants2;
    //   (drw-fan        gs-gif-tag        :inline :offset-assert 96) ;; was qword
    u8 drw_fan[16];
    //   (env-fan        gs-gif-tag        :inline :offset-assert 112) ;; was qword
    u8 env_fan[16];
    //   (drw-adgif      gs-gif-tag        :inline :offset-assert 128);; was qword
    AdGifData drw_adgif;
    //   (drw-texture    adgif-shader :inline :offset-assert 144)
    u8 drw_texture[16];
    //   (drw-strip-0    gs-gif-tag        :inline :offset-assert 224) ;; was qword
    u8 drw_strip_0[16];
    //   (drw-strip-1    gs-gif-tag        :inline :offset-assert 240) ;; was qword
    u8 drw_strip_1[16];
    //   (env-adgif      gs-gif-tag        :inline :offset-assert 256) ;; was qword
    u8 env_adgif[16];
    //   (env-texture    adgif-shader :inline :offset-assert 272)
    AdGifData env_texture;
    //   (env-strip      gs-gif-tag        :inline :offset-assert 352) ;; was qword
    u8 env_strip[16];
    //   (env-color      vector       :inline :offset-assert 368)
    math::Vector4f env_color;
    //   (index-table    vector4w      8 :inline      :offset-assert 384)
    math::Vector<u32, 4> index_table[8];
    //   (pos0           vector       :inline :offset-assert 512)
    math::Vector4f pos0;
    //   (pos1           vector       :inline :offset-assert 528)
    math::Vector4f pos1;
    //   (pos2           vector       :inline :offset-assert 544)
    math::Vector4f pos2;
    //   (pos3           vector       :inline :offset-assert 560)
    math::Vector4f pos3;
    //   )
    //  :method-count-assert 9
    //  :size-assert         #x240
    //  :flag-assert         #x900000240
    //  )
  } m_constants;
  static_assert(sizeof(Constants) == 0x240);

  enum Vu1Data {
    IN_BUFFER_0 = VU1_INPUT_BUFFER_BASE,    // 0
    IN_BUFFER_1 = VU1_INPUT_BUFFER_OFFSET,  // 0x76

    CONSTANTS = 0x2dd,
  };

  Vf m_vu_data[1024];

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

  void lq_buffer(Mask mask, Vf& dest, u16 addr) {
    ASSERT(addr < 1024);
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        dest[i] = m_vu_data[addr].data[i];
      }
    }
  }

  struct Vu {
    const Vf vf00;
    Accumulator acc;
    Vf vf01, vf02, vf03, vf04, vf05, vf06, vf07, vf08, vf09, vf10, vf11, vf12, vf13, vf14, vf15,
        vf16, vf17, vf18, vf19, vf20, vf21, vf22, vf23, vf24, vf25, vf26, vf27, vf28, vf29, vf30,
        vf31;
    u16 vi01, vi02, vi03, vi04, vi05, vi06, vi07, vi08, vi09, vi10, vi11, vi12, vi13, vi14, vi15;
    float Q, P;
    Vu() : vf00(0, 0, 0, 1) {}
  } vu;
};
