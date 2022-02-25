#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"
#include "game/common/vu.h"

class GenericRenderer : public BucketRenderer {
 public:
  GenericRenderer(const std::string& name, BucketId my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;

 private:
  u32 unpack32_4(const VifCodeUnpack& up, const u8* data, u32 imm);
  u32 unpack32_3(const VifCodeUnpack& up, const u8* data, u32 imm);
  u32 unpack8_4(const VifCodeUnpack& up, const u8* data, u32 imm);
  u32 unpack16_2(const VifCodeUnpack& up, const u8* data, u32 imm);

  void mscal(int imm, SharedRenderState* render_state, ScopedProfilerNode& prof);
  void mscal0();
  void mscal_dispatch(int imm, SharedRenderState* render_state, ScopedProfilerNode& prof);
  void handle_dma_stream(const u8* data,
                         u32 bytes,
                         SharedRenderState* render_state,
                         ScopedProfilerNode& prof);
  void lq_buffer(Mask mask, Vf& dest, u16 addr);
  void isw_buffer(Mask mask, u16 val, u16 addr);
  void ilw_buffer(Mask mask, u16& dest, u16 addr);
  void xgkick(u16 addr, SharedRenderState* render_state, ScopedProfilerNode& prof);

  void sq_buffer(Mask mask, const Vf& data, u32 qw) {
    ASSERT(qw * 16 < sizeof(m_buffer.data));
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        memcpy(m_buffer.data + qw * 16 + i * 4, data.data + i, 4);
      }
    }
  }

  int m_skipped_tags = 0;
  DirectRenderer m_direct;
  std::string m_debug;

  struct Vu {
    u32 row[4];
    u32 stcycl = 0;
    Vf vf01, vf02, vf03, vf04, vf05, vf06, vf07, vf08, vf09, vf10, vf11, vf12, vf13, vf14, vf15,
        vf16, vf17, vf18, vf19, vf20, vf21, vf22, vf23, vf24, vf25, vf26, vf27, vf28, vf29, vf30,
        vf31;
    const Vf vf00;
    u16 vi01, vi02, vi03, vi04, vi05, vi06, vi07, vi09, vi08, vi11, vi12, vi13, vi10, vi14, vi15;
    float I, P, Q;

    Accumulator acc;
    const u16 vi00 = 0;
    Vu() : vf00(0, 0, 0, 1) {}
  } vu;

  struct alignas(16) BufferMemory {
    u8 data[1024 * 16];
  } m_buffer;

  int m_xgkick_idx = 0;
  int m_min_xgkick = 0;
  int m_max_xgkick = 1000000;
};
