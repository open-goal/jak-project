#pragma once

#include "game/graphics/opengl_renderer/BucketRenderer.h"
#include "game/graphics/opengl_renderer/DirectRenderer.h"

#include "game/common/vu.h"

class ShadowRenderer : public BucketRenderer {
 public:
  ShadowRenderer(const std::string& name, BucketId my_id);
  void render(DmaFollower& dma, SharedRenderState* render_state, ScopedProfilerNode& prof) override;
  void draw_debug_window() override;

 private:
  void run_mscal_vu2c(u16 imm, SharedRenderState* render_state, ScopedProfilerNode& prof);
  void xgkick(u16 imm, SharedRenderState* render_state, ScopedProfilerNode& prof);

  void run_mscal10_vu2c();

  void handle_jalr_to_end_block(u16 val, u32& first_flag, u32& second_flag) {
    switch (val) {
      case 724:
        // jr vi11                    |  addx.w vf30, vf14, vf14        724
        first_flag = vu.vf30.add_and_set_sf_s(Mask::w, vu.vf14, vu.vf14.x());
        // nop                        |  addx.w vf31, vf15, vf15        725
        second_flag = vu.vf31.add_and_set_sf_s(Mask::w, vu.vf15, vu.vf15.x());
        return;
      case 726:
        // jr vi11                    |  subx.w vf30, vf14, vf14        726
        first_flag = vu.vf30.sub_and_set_sf_s(Mask::w, vu.vf14, vu.vf14.x());
        // nop                        |  subx.w vf31, vf15, vf15        727
        second_flag = vu.vf31.sub_and_set_sf_s(Mask::w, vu.vf15, vu.vf15.x());
        return;
      case 728:
        // jr vi11                    |  addy.w vf30, vf14, vf14        728
        first_flag = vu.vf30.add_and_set_sf_s(Mask::w, vu.vf14, vu.vf14.y());
        // nop                        |  addy.w vf31, vf15, vf15        729
        second_flag = vu.vf31.add_and_set_sf_s(Mask::w, vu.vf15, vu.vf15.y());
        return;
      case 730:
        // jr vi11                    |  suby.w vf30, vf14, vf14        730
        first_flag = vu.vf30.sub_and_set_sf_s(Mask::w, vu.vf14, vu.vf14.y());
        // nop                        |  suby.w vf31, vf15, vf15        731
        second_flag = vu.vf31.sub_and_set_sf_s(Mask::w, vu.vf15, vu.vf15.y());
        return;
      case 732:
        // jr vi11                    |  addz.w vf30, vf14, vf14        732
        first_flag = vu.vf30.add_and_set_sf_s(Mask::w, vu.vf14, vu.vf14.z());
        // nop                        |  addz.w vf31, vf15, vf15        733
        second_flag = vu.vf31.add_and_set_sf_s(Mask::w, vu.vf15, vu.vf15.z());
        return;
      case 734:
        // jr vi11                    |  subz.w vf30, vf14, vf14        734
        first_flag = vu.vf30.sub_and_set_sf_s(Mask::w, vu.vf14, vu.vf14.z());
        // nop                        |  subz.w vf31, vf15, vf15        735
        second_flag = vu.vf31.sub_and_set_sf_s(Mask::w, vu.vf15, vu.vf15.z());
        return;
      case 736:
        // nop                        |  sub.xyzw vf16, vf15, vf14      736
        vu.vf16.sub(Mask::xyzw, vu.vf15, vu.vf14);
        // waitq                      |  mul.xyzw vf16, vf16, Q         737
        vu.vf16.mul(Mask::xyzw, vu.vf16, vu.Q);
        // jr vi11                    |  add.xyzw vf16, vf14, vf16      738
        vu.vf16.add(Mask::xyzw, vu.vf14, vu.vf16);
        // nop                        |  nop                            739
        return;
      default:
        fmt::print("unhandled end block: {}\n", val);
        ASSERT(false);
    }


  }

  DirectRenderer m_direct;

  Vf m_vu_data[1024];

  enum Vu1Data {
    MATRIX = 0,
    CONSTANTS = 0x370,
    GIF_CONSTANTS = 0x3ac,
  };

  enum Vu1Code {
    INIT = 10,
  };

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

  struct Vu {
    const Vf vf00;
    Vu() : vf00(0, 0, 0, 1) {}

    u16 vi01, vi02, vi03, vi04, vi05, vi06, vi07, vi08, vi09, vi10, vi12, vi15;
    Vf vf01, vf02, vf03, vf04, vf05, vf06, vf07, vf08, vf09, vf10, vf11, vf12, vf13, vf14, vf15,
        vf16, vf17, vf18, vf19, vf20, vf21, vf22, vf23, vf24, vf25, vf26, vf27, vf28, vf29, vf30,
        vf31;
    Accumulator acc;
    float Q;
    const u16 vi00 = 0;
  } vu;
};
