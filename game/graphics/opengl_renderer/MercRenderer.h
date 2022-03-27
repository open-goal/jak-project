#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "common/math/Vector.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"
#include "game/common/vu.h"
#include "game/graphics/opengl_renderer/DirectRenderer2.h"

class MercRenderer : public BucketRenderer {
 public:
  MercRenderer(const std::string& name, BucketId my_id);
  void init_shaders(ShaderLibrary& shaders) override;

  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;

 private:
  void handle_setup(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof);
  void handle_merc_chain(DmaFollower& dma,
                         SharedRenderState* render_state,
                         ScopedProfilerNode& prof);
  void unpack8(const VifCodeUnpack& up, const u8* data, u32 imm);
  void unpack32(const VifCodeUnpack& up, const u8* data, u32 imm);

  void mscal(int enter_address, SharedRenderState* render_state, ScopedProfilerNode& prof);

  template <bool DEBUG>
  void mscal_impl(int enter_address, SharedRenderState* render_state, ScopedProfilerNode& prof);

  void xgkick(u16 addr, SharedRenderState* render_state, ScopedProfilerNode& prof);

  enum MercDataMemory {
    LOW_MEMORY = 0,
    BUFFER_BASE = 442,
    // this negative offset is what broke jak graphics in Dobiestation for a long time.
    BUFFER_OFFSET = -442
  };

  struct LowMemory {
    u8 tri_strip_tag[16];
    u8 ad_gif_tag[16];
    math::Vector4f hvdf_offset;
    math::Vector4f perspective[4];
    math::Vector4f fog;
  } m_low_memory;
  static_assert(sizeof(LowMemory) == 0x80);

  struct alignas(16) BufferMemory {
    u8 data[1024 * 16];
  } m_buffer;

  void sq_buffer(Mask mask, const Vf& data, u32 qw) {
    //    if (data.x_as_u32() == 0x80000000 && data.y_as_u32() == 0x80000000) {
    //      fmt::print("big store line {}: {} : {} {} {} {}\n", line_number, qw, data.x(), data.y(),
    //      data.z(), data.w());
    //    }
    // sketchy...
    //    qw &= 1023;
    ASSERT(qw * 16 < sizeof(m_buffer.data));
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        memcpy(m_buffer.data + qw * 16 + i * 4, data.data + i, 4);
      }
    }
  }

  void lq_buffer(Mask mask, Vf& dest, u16 addr);

  template <bool DEBUG>
  void lq_buffer_xyzw(Vf& dest, u16 addr);

  template <bool DEBUG>
  void lq_buffer_xyz(Vf& dest, u16 addr);

  template <bool DEBUG>
  void sq_buffer_xyzw(const Vf& src, u16 addr);

  void isw_buffer(Mask mask, u16 val, u16 addr);
  void ilw_buffer(Mask mask, u16& dest, u16 addr);

  u16 xtop();
  u16 xitop();

  DirectRenderer m_direct;
  DirectRenderer2 m_direct2;

  struct {
    u32 row[4] = {0, 0, 0, 0};
    bool stmod = false;
  } m_vif;

  struct Stats {
    int unpack_count = 0;
    int unpack_bytes = 0;
    int mscal_35 = 0;
    int mscal_20 = 0;
    int mscal_17 = 0;
    int mscal_32 = 0;
    bool had_data = false;
    std::string str;
  } m_stats;

  bool m_dbf = false;

  bool m_enable_prime_mscals = true;
  bool m_enable_normal_mscals = true;
  bool m_enable_send_to_direct = true;

  struct Vu {
    Vf vf01, vf02, vf03, vf04, vf05, vf06, vf07, vf08, vf09, vf10, vf11, vf12, vf13, vf14, vf15,
        vf16, vf17, vf18, vf19, vf20, vf21, vf22, vf23, vf24, vf25, vf26, vf27, vf28, vf29, vf30,
        vf31;
    const Vf vf00;
    u16 vi01, vi02, vi03, vi04, vi05, vi06, vi07, vi09, vi08, vi11, vi12, vi13, vi10, vi14, vi15;
    float I, P, Q;

    Accumulator acc;
    const u16 vi00 = 0;

    u16 hack_old_vi15 = 0;

    Vu() : vf00(0, 0, 0, 1) {}
  } vu;
};
