#include "game/graphics/opengl_renderer/GenericRenderer.h"

void GenericRenderer::lq_buffer(Mask mask, Vf& dest, u16 addr) {
  ASSERT(addr * 16 < sizeof(m_buffer.data));
  for (int i = 0; i < 4; i++) {
    if ((u64)mask & (1 << i)) {
      memcpy(dest.data + i, m_buffer.data + addr * 16 + i * 4, 4);
    }
  }
}

void GenericRenderer::isw_buffer(Mask mask, u16 val, u16 addr) {
  ASSERT(addr * 16 < sizeof(m_buffer.data));
  u32 val32 = val;
  int offset;
  switch (mask) {
    case Mask::x:
      offset = 0;
      break;
    case Mask::y:
      offset = 4;
      break;
    case Mask::z:
      offset = 8;
      break;
    case Mask::w:
      offset = 12;
      break;
    default:
      ASSERT(false);
  }
  memcpy(m_buffer.data + addr * 16 + offset, &val32, 4);
}

u16 clip(const Vf& vector, float val, u16 old_clip) {
  u16 result = (old_clip << 6);
  float plus = std::abs(val);
  float minus = -plus;

  if (vector.x() > plus) {
    result |= 0b1;
  }
  if (vector.x() < minus) {
    result |= 0b10;
  }

  if (vector.y() > plus) {
    result |= 0b100;
  }
  if (vector.y() < minus) {
    result |= 0b1000;
  }

  if (vector.z() > plus) {
    result |= 0b10000;
  }
  if (vector.z() < minus) {
    result |= 0b100000;
  }
  return result;
}

bool clipping_hack = true;

// clang-format off
void GenericRenderer::mscal0() {
  // L4:
  // iaddiu vi01, vi00, 0x381   |  nop
  vu.vi01 = 0x381; /* 897 */
  // lq.xyzw vf01, 0(vi01)      |  nop
  lq_buffer(Mask::xyzw, vu.vf01, vu.vi01);
  // lq.xyzw vf02, 1(vi01)      |  nop
  lq_buffer(Mask::xyzw, vu.vf02, vu.vi01 + 1);
  // lq.xyzw vf03, 2(vi01)      |  nop
  lq_buffer(Mask::xyzw, vu.vf03, vu.vi01 + 2);
  // lq.xyzw vf04, 3(vi01)      |  nop
  lq_buffer(Mask::xyzw, vu.vf04, vu.vi01 + 3);
  // lq.xyzw vf05, 4(vi01)      |  nop
  lq_buffer(Mask::xyzw, vu.vf05, vu.vi01 + 4);
  // lq.xyzw vf06, 5(vi01)      |  nop
  lq_buffer(Mask::xyzw, vu.vf06, vu.vi01 + 5);
  // lq.xyzw vf07, 6(vi01)      |  nop
  lq_buffer(Mask::xyzw, vu.vf07, vu.vi01 + 6);
  // L5:
  // iaddiu vi13, vi00, 0x363   |  nop
  vu.vi13 = 0x363; /* 867 */
  // iaddi vi02, vi13, 0x5      |  nop
  vu.vi02 = vu.vi13 + 5;
  // iaddi vi12, vi00, 0x0      |  nop
  vu.vi12 = 0;
  // isw.x vi02, 9(vi01)        |  nop
  isw_buffer(Mask::x, vu.vi02, vu.vi01 + 9);
  // isw.y vi02, 9(vi01)        |  nop
  isw_buffer(Mask::y, vu.vi02, vu.vi01 + 9);
  // sq.xyzw vf00, 907(vi00)    |  nop
  sq_buffer(Mask::xyzw, vu.vf00, 907);
  // sq.xyzw vf00, 914(vi00)    |  nop
  sq_buffer(Mask::xyzw, vu.vf00, 914);
  // sq.xyzw vf00, 921(vi00)    |  nop
  sq_buffer(Mask::xyzw, vu.vf00, 921);
  // sq.xyzw vf00, 928(vi00)    |  nop
  sq_buffer(Mask::xyzw, vu.vf00, 928);
  // sq.xyzw vf00, 935(vi00)    |  nop
  sq_buffer(Mask::xyzw, vu.vf00, 935);
  // sq.xyzw vf00, 942(vi00)    |  nop
  sq_buffer(Mask::xyzw, vu.vf00, 942);
  // iaddiu vi01, vi00, 0x40f   |  nop
  vu.vi01 = 0x40f; /* 1039 */
  // isw.z vi01, 907(vi00)      |  nop
  isw_buffer(Mask::z, vu.vi01, 907);
  // iaddiu vi01, vi00, 0x411   |  nop
  vu.vi01 = 0x411; /* 1041 */
  // isw.z vi01, 914(vi00)      |  nop
  isw_buffer(Mask::z, vu.vi01, 914);
  // iaddiu vi01, vi00, 0x413   |  nop
  vu.vi01 = 0x413; /* 1043 */
  // isw.z vi01, 921(vi00)      |  nop
  isw_buffer(Mask::z, vu.vi01, 921);
  // iaddiu vi01, vi00, 0x415   |  nop
  vu.vi01 = 0x415; /* 1045 */
  // isw.z vi01, 928(vi00)      |  nop
  isw_buffer(Mask::z, vu.vi01, 928);
  // iaddiu vi01, vi00, 0x417   |  nop
  vu.vi01 = 0x417; /* 1047 */
  // isw.z vi01, 935(vi00)      |  nop
  isw_buffer(Mask::z, vu.vi01, 935);
  // iaddiu vi01, vi00, 0x419   |  nop :e
  vu.vi01 = 0x419; /* 1049 */
  // isw.z vi01, 942(vi00)      |  nop
  isw_buffer(Mask::z, vu.vi01, 942);
}

void GenericRenderer::ilw_buffer(Mask mask, u16& dest, u16 addr) {
  // fmt::print("addr is {}\n", addr);
  ASSERT(addr * 16 < sizeof(m_buffer.data));
  int offset;
  switch (mask) {
    case Mask::x:
      offset = 0;
      break;
    case Mask::y:
      offset = 4;
      break;
    case Mask::z:
      offset = 8;
      break;
    case Mask::w:
      offset = 12;
      break;
    default:
      ASSERT(false);
  }
  memcpy(&dest, m_buffer.data + addr * 16 + offset, 2);
}



void GenericRenderer::mscal_dispatch(int imm, SharedRenderState* render_state, ScopedProfilerNode& prof) {
  bool bc;
  u16 cf = 0;
  u16 cf0, cf1, cf2;
  switch(imm) {
    case 6:
      goto L33;
    case 8:
      goto L8;
    default:
      fmt::print("Generic dispatch mscal: {}\n", imm);
      ASSERT(false);
  }

  // BRANCH!
  // b L4                       |  nop                            0
  bc = true;
  // nop                        |  nop                            1

  if (bc) { goto L4; }

  // BRANCH!
  // b L5                       |  nop                            2
  bc = true;
  // nop                        |  nop                            3

  if (bc) { goto L5; }

  // BRANCH!
  // b L84                      |  nop                            4
  bc = true;
  // nop                        |  nop                            5

  if (bc) { goto L84; }

  // BRANCH!
  // b L33                      |  nop                            6
  bc = true;
  // nop                        |  nop                            7

  if (bc) { goto L33; }

  // BRANCH!
  // b L8                       |  nop                            8
  bc = true;
  // nop                        |  nop                            9

  if (bc) { goto L8; }

  // BRANCH!
  // b L1                       |  nop                            10
  bc = true;
  // nop                        |  nop                            11

  if (bc) { goto L1; }

  // BRANCH!
  // b L6                       |  nop                            12
  bc = true;
  // nop                        |  nop                            13

  if (bc) { goto L6; }

  L1:
  // iaddiu vi02, vi00, 0x381   |  nop                            14
  vu.vi02 = 0x381; /* 897 */
  // lq.xyzw vf31, 7(vi02)      |  nop                            15
  lq_buffer(Mask::xyzw, vu.vf31, vu.vi02 + 7);
  // isubiu vi02, vi13, 0x363   |  addw.z vf22, vf00, vf00        16
  vu.vf22.add(Mask::z, vu.vf00, vu.vf00.w());   vu.vi02 = vu.vi13 - 0x363; /* 867 */
  // iaddiu vi13, vi13, 0x1e    |  addw.z vf23, vf00, vf00        17
  vu.vf23.add(Mask::z, vu.vf00, vu.vf00.w());   vu.vi13 = vu.vi13 + 0x1e; /* 30 */
  // BRANCH!
  // ibne vi00, vi02, L2        |  addw.z vf24, vf00, vf00        18
  vu.vf24.add(Mask::z, vu.vf00, vu.vf00.w());   bc = (vu.vi02 != 0);
  // lq.xyzw vf03, 899(vi00)    |  addw.z vf25, vf00, vf00        19
  vu.vf25.add(Mask::z, vu.vf00, vu.vf00.w());   lq_buffer(Mask::xyzw, vu.vf03, 899);
  if (bc) { goto L2; }

  // iaddiu vi13, vi00, 0x345   |  nop                            20
  vu.vi13 = 0x345; /* 837 */
  L2:
  // ilw.x vi01, 5(vi13)        |  nop                            21
  ilw_buffer(Mask::x, vu.vi01, vu.vi13 + 5);
  // iaddi vi07, vi12, 0xa      |  nop                            22
  vu.vi07 = vu.vi12 + 10;
  // iaddi vi05, vi01, -0x1     |  nop                            23
  vu.vi05 = vu.vi01 + -1;
  // lq.xyzw vf17, 4(vi13)      |  nop                            24
  lq_buffer(Mask::xyzw, vu.vf17, vu.vi13 + 4);
  // sq.xyzw vf31, 5(vi13)      |  nop                            25
  sq_buffer(Mask::xyzw, vu.vf31, vu.vi13 + 5);
  // sq.xyzw vf31, 6(vi13)      |  nop                            26
  sq_buffer(Mask::xyzw, vu.vf31, vu.vi13 + 6);
  // 0.0078125                  |  nop :i                         27
  vu.I = 0.0078125;
  // move.xyzw vf13, vf17       |  muli.xyz vf17, vf17, I         28
  vu.vf17.mul(Mask::xyz, vu.vf17, vu.I);   vu.vf13.move(Mask::xyzw, vu.vf17);
  // move.xyzw vf14, vf17       |  nop                            29
  vu.vf14.move(Mask::xyzw, vu.vf17);
  // move.xyzw vf15, vf17       |  nop                            30
  vu.vf15.move(Mask::xyzw, vu.vf17);
  // move.xyzw vf16, vf17       |  nop                            31
  vu.vf16.move(Mask::xyzw, vu.vf17);
  // sq.xyzw vf03, 4(vi13)      |  nop                            32
  sq_buffer(Mask::xyzw, vu.vf03, vu.vi13 + 4);
  // isw.w vi01, 5(vi13)        |  nop                            33
  isw_buffer(Mask::w, vu.vi01, vu.vi13 + 5);
  // isw.w vi00, 6(vi13)        |  nop                            34
  isw_buffer(Mask::w, vu.vi00, vu.vi13 + 6);
  L3:
  // lq.xyz vf13, 0(vi07)       |  nop                            35
  lq_buffer(Mask::xyz, vu.vf13, vu.vi07);
  // lq.xyz vf14, 3(vi07)       |  nop                            36
  lq_buffer(Mask::xyz, vu.vf14, vu.vi07 + 3);
  // lq.xyz vf15, 6(vi07)       |  nop                            37
  lq_buffer(Mask::xyz, vu.vf15, vu.vi07 + 6);
  // lq.xyz vf16, 9(vi07)       |  nop                            38
  lq_buffer(Mask::xyz, vu.vf16, vu.vi07 + 9);
  // iaddi vi07, vi07, 0xc      |  itof0.xyz vf13, vf13           39
  vu.vf13.itof0(Mask::xyz, vu.vf13);   vu.vi07 = vu.vi07 + 12;
  // iaddi vi05, vi05, -0x4     |  itof0.xyz vf14, vf14           40
  vu.vf14.itof0(Mask::xyz, vu.vf14);   vu.vi05 = vu.vi05 + -4;
  // nop                        |  itof0.xyz vf15, vf15           41
  vu.vf15.itof0(Mask::xyz, vu.vf15);
  // nop                        |  itof0.xyz vf16, vf16           42
  vu.vf16.itof0(Mask::xyz, vu.vf16);
  // nop                        |  mul.xyz vf13, vf13, vf17       43
  vu.vf13.mul(Mask::xyz, vu.vf13, vu.vf17);
  // nop                        |  mul.xyz vf14, vf14, vf17       44
  vu.vf14.mul(Mask::xyz, vu.vf14, vu.vf17);
  // nop                        |  mul.xyz vf15, vf15, vf17       45
  vu.vf15.mul(Mask::xyz, vu.vf15, vu.vf17);
  // nop                        |  mul.xyz vf16, vf16, vf17       46
  vu.vf16.mul(Mask::xyz, vu.vf16, vu.vf17);
  // lq.xyzw vf18, -11(vi07)    |  ftoi0.xyz vf13, vf13           47
  vu.vf13.ftoi0(Mask::xyz, vu.vf13);   lq_buffer(Mask::xyzw, vu.vf18, vu.vi07 + -11);
  // lq.xyzw vf19, -8(vi07)     |  ftoi0.xyz vf14, vf14           48
  vu.vf14.ftoi0(Mask::xyz, vu.vf14);   lq_buffer(Mask::xyzw, vu.vf19, vu.vi07 + -8);
  // lq.xyzw vf20, -5(vi07)     |  ftoi0.xyz vf15, vf15           49
  vu.vf15.ftoi0(Mask::xyz, vu.vf15);   lq_buffer(Mask::xyzw, vu.vf20, vu.vi07 + -5);
  // lq.xyzw vf21, -2(vi07)     |  ftoi0.xyz vf16, vf16           50
  vu.vf16.ftoi0(Mask::xyz, vu.vf16);   lq_buffer(Mask::xyzw, vu.vf21, vu.vi07 + -2);
  // sq.xyzw vf13, -12(vi07)    |  itof0.xyzw vf18, vf18          51
  vu.vf18.itof0(Mask::xyzw, vu.vf18);   sq_buffer(Mask::xyzw, vu.vf13, vu.vi07 + -12);
  // sq.xyzw vf14, -9(vi07)     |  itof0.xyzw vf19, vf19          52
  vu.vf19.itof0(Mask::xyzw, vu.vf19);   sq_buffer(Mask::xyzw, vu.vf14, vu.vi07 + -9);
  // sq.xyzw vf15, -6(vi07)     |  itof0.xyzw vf20, vf20          53
  vu.vf20.itof0(Mask::xyzw, vu.vf20);   sq_buffer(Mask::xyzw, vu.vf15, vu.vi07 + -6);
  // sq.xyzw vf16, -3(vi07)     |  itof0.xyzw vf21, vf21          54
  vu.vf21.itof0(Mask::xyzw, vu.vf21);   sq_buffer(Mask::xyzw, vu.vf16, vu.vi07 + -3);
  // sq.xyzw vf18, -11(vi07)    |  nop                            55
  sq_buffer(Mask::xyzw, vu.vf18, vu.vi07 + -11);
  // sq.xyzw vf19, -8(vi07)     |  nop                            56
  sq_buffer(Mask::xyzw, vu.vf19, vu.vi07 + -8);
  // sq.xyzw vf20, -5(vi07)     |  nop                            57
  sq_buffer(Mask::xyzw, vu.vf20, vu.vi07 + -5);
  // BRANCH!
  // ibgez vi05, L3             |  nop                            58
  bc = ((s16)vu.vi05) >= 0;
  // sq.xyzw vf21, -2(vi07)     |  nop                            59
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi07 + -2);
  if (bc) { goto L3; }

  // BRANCH!
  // b L16                      |  nop                            60
  bc = true;
  // nop                        |  nop                            61

  if (bc) { goto L16; }

  L4:
  // iaddiu vi01, vi00, 0x381   |  nop                            62
  vu.vi01 = 0x381; /* 897 */
  // lq.xyzw vf01, 0(vi01)      |  nop                            63
  lq_buffer(Mask::xyzw, vu.vf01, vu.vi01);
  // lq.xyzw vf02, 1(vi01)      |  nop                            64
  lq_buffer(Mask::xyzw, vu.vf02, vu.vi01 + 1);
  // lq.xyzw vf03, 2(vi01)      |  nop                            65
  lq_buffer(Mask::xyzw, vu.vf03, vu.vi01 + 2);
  // lq.xyzw vf04, 3(vi01)      |  nop                            66
  lq_buffer(Mask::xyzw, vu.vf04, vu.vi01 + 3);
  // lq.xyzw vf05, 4(vi01)      |  nop                            67
  lq_buffer(Mask::xyzw, vu.vf05, vu.vi01 + 4);
  // lq.xyzw vf06, 5(vi01)      |  nop                            68
  lq_buffer(Mask::xyzw, vu.vf06, vu.vi01 + 5);
  // lq.xyzw vf07, 6(vi01)      |  nop                            69
  lq_buffer(Mask::xyzw, vu.vf07, vu.vi01 + 6);
  L5:
  // iaddiu vi13, vi00, 0x363   |  nop                            70
  vu.vi13 = 0x363; /* 867 */
  // iaddi vi02, vi13, 0x5      |  nop                            71
  vu.vi02 = vu.vi13 + 5;
  // iaddi vi12, vi00, 0x0      |  nop                            72
  vu.vi12 = 0;
  // isw.x vi02, 9(vi01)        |  nop                            73
  isw_buffer(Mask::x, vu.vi02, vu.vi01 + 9);
  // isw.y vi02, 9(vi01)        |  nop                            74
  isw_buffer(Mask::y, vu.vi02, vu.vi01 + 9);
  // sq.xyzw vf00, 907(vi00)    |  nop                            75
  sq_buffer(Mask::xyzw, vu.vf00, 907);
  // sq.xyzw vf00, 914(vi00)    |  nop                            76
  sq_buffer(Mask::xyzw, vu.vf00, 914);
  // sq.xyzw vf00, 921(vi00)    |  nop                            77
  sq_buffer(Mask::xyzw, vu.vf00, 921);
  // sq.xyzw vf00, 928(vi00)    |  nop                            78
  sq_buffer(Mask::xyzw, vu.vf00, 928);
  // sq.xyzw vf00, 935(vi00)    |  nop                            79
  sq_buffer(Mask::xyzw, vu.vf00, 935);
  // sq.xyzw vf00, 942(vi00)    |  nop                            80
  sq_buffer(Mask::xyzw, vu.vf00, 942);
  // iaddiu vi01, vi00, 0x40f   |  nop                            81
  vu.vi01 = 0x40f; /* 1039 */
  // isw.z vi01, 907(vi00)      |  nop                            82
  isw_buffer(Mask::z, vu.vi01, 907);
  // iaddiu vi01, vi00, 0x411   |  nop                            83
  vu.vi01 = 0x411; /* 1041 */
  // isw.z vi01, 914(vi00)      |  nop                            84
  isw_buffer(Mask::z, vu.vi01, 914);
  // iaddiu vi01, vi00, 0x413   |  nop                            85
  vu.vi01 = 0x413; /* 1043 */
  // isw.z vi01, 921(vi00)      |  nop                            86
  isw_buffer(Mask::z, vu.vi01, 921);
  // iaddiu vi01, vi00, 0x415   |  nop                            87
  vu.vi01 = 0x415; /* 1045 */
  // isw.z vi01, 928(vi00)      |  nop                            88
  isw_buffer(Mask::z, vu.vi01, 928);
  // iaddiu vi01, vi00, 0x417   |  nop                            89
  vu.vi01 = 0x417; /* 1047 */
  // isw.z vi01, 935(vi00)      |  nop                            90
  isw_buffer(Mask::z, vu.vi01, 935);
  // iaddiu vi01, vi00, 0x419   |  nop :e                         91
  vu.vi01 = 0x419; /* 1049 */
  // isw.z vi01, 942(vi00)      |  nop                            92
  isw_buffer(Mask::z, vu.vi01, 942);
  return;

  L6:
  // iaddiu vi01, vi00, 0x381   |  nop                            93
  vu.vi01 = 0x381; /* 897 */
  // ilw.z vi13, 9(vi01)        |  nop                            94
  ilw_buffer(Mask::z, vu.vi13, vu.vi01 + 9);
  // ilw.w vi12, 9(vi01)        |  nop                            95
  ilw_buffer(Mask::w, vu.vi12, vu.vi01 + 9);
  // iaddi vi02, vi13, 0x6      |  nop                            96
  vu.vi02 = vu.vi13 + 6;
  // isw.x vi02, 9(vi01)        |  nop :e                         97
  isw_buffer(Mask::x, vu.vi02, vu.vi01 + 9);
  // isw.y vi02, 9(vi01)        |  nop                            98
  isw_buffer(Mask::y, vu.vi02, vu.vi01 + 9);
  return;

  // isubiu vi02, vi13, 0x363   |  nop                            99
  vu.vi02 = vu.vi13 - 0x363; /* 867 */
  // iaddiu vi13, vi13, 0x1e    |  nop                            100
  vu.vi13 = vu.vi13 + 0x1e; /* 30 */
  // BRANCH!
  // ibne vi00, vi02, L7        |  nop                            101
  bc = (vu.vi02 != 0);
  // isubiu vi01, vi01, 0x100   |  nop                            102
  vu.vi01 = vu.vi01 - 0x100; /* 256 */
  if (bc) { goto L7; }

  // iaddiu vi13, vi00, 0x345   |  nop                            103
  vu.vi13 = 0x345; /* 837 */
  L7:
  // iaddi vi03, vi13, 0x7      |  nop                            104
  vu.vi03 = vu.vi13 + 7;
  // iaddi vi03, vi13, 0x7      |  nop                            105
  vu.vi03 = vu.vi13 + 7;
  // isw.x vi03, 906(vi00)      |  nop                            106
  isw_buffer(Mask::x, vu.vi03, 906);
  // jr vi15                    |  nop                            107
  ASSERT(false);
  // isw.y vi03, 906(vi00)      |  nop                            108
  isw_buffer(Mask::y, vu.vi03, 906);
  L8:
  // isubiu vi02, vi13, 0x363   |  addw.z vf22, vf00, vf00        109
  vu.vf22.add(Mask::z, vu.vf00, vu.vf00.w());   vu.vi02 = vu.vi13 - 0x363; /* 867 */
  // iaddiu vi13, vi13, 0x1e    |  addw.z vf23, vf00, vf00        110
  vu.vf23.add(Mask::z, vu.vf00, vu.vf00.w());   vu.vi13 = vu.vi13 + 0x1e; /* 30 */
  // BRANCH!
  // ibne vi00, vi02, L9        |  addw.z vf24, vf00, vf00        111
  vu.vf24.add(Mask::z, vu.vf00, vu.vf00.w());   bc = (vu.vi02 != 0);
  // nop                        |  addw.z vf25, vf00, vf00        112
  vu.vf25.add(Mask::z, vu.vf00, vu.vf00.w());
  if (bc) { goto L9; }

  // iaddiu vi13, vi00, 0x345   |  nop                            113
  vu.vi13 = 0x345; /* 837 */
  L9:
  // iaddi vi03, vi13, 0x7      |  nop                            114
  vu.vi03 = vu.vi13 + 7;
  // ilw.w vi01, 5(vi13)        |  nop                            115
  ilw_buffer(Mask::w, vu.vi01, vu.vi13 + 5);
  // isw.x vi03, 906(vi00)      |  nop                            116
  isw_buffer(Mask::x, vu.vi03, 906);
  // iaddi vi10, vi12, 0x9      |  subw.w vf18, vf00, vf00        117
  vu.vf18.sub(Mask::w, vu.vf00, vu.vf00.w());   vu.vi10 = vu.vi12 + 9;
  // lq.xyzw vf08, 0(vi13)      |  subw.w vf19, vf00, vf00        118
  vu.vf19.sub(Mask::w, vu.vf00, vu.vf00.w());   lq_buffer(Mask::xyzw, vu.vf08, vu.vi13);
  // lq.xyzw vf09, 1(vi13)      |  subw.w vf20, vf00, vf00        119
  vu.vf20.sub(Mask::w, vu.vf00, vu.vf00.w());   lq_buffer(Mask::xyzw, vu.vf09, vu.vi13 + 1);
  // lq.xyzw vf10, 2(vi13)      |  subw.w vf21, vf00, vf00        120
  vu.vf21.sub(Mask::w, vu.vf00, vu.vf00.w());   lq_buffer(Mask::xyzw, vu.vf10, vu.vi13 + 2);
  // lq.xyzw vf11, 3(vi13)      |  ftoi12.z vf22, vf22            121
  // fmt::print("a: [{}] [{}]\n", vu.vf22.print(), vu.vf23.print());
  vu.vf22.ftoi12(Mask::z, vu.vf22);   lq_buffer(Mask::xyzw, vu.vf11, vu.vi13 + 3);
  // iadd vi02, vi01, vi01      |  ftoi12.z vf23, vf23            122
  vu.vf23.ftoi12(Mask::z, vu.vf23);   vu.vi02 = vu.vi01 + vu.vi01;
  // iadd vi01, vi01, vi02      |  sub.xyzw vf16, vf16, vf16      123
  vu.vf16.set_zero();         vu.vi01 = vu.vi01 + vu.vi02;
  // iaddi vi11, vi00, -0x2     |  sub.xyzw vf17, vf17, vf17      124
  vu.vf17.set_zero();         vu.vi11 = -2;
  // lq.xy vf22, 0(vi10)        |  nop                            125
  lq_buffer(Mask::xy, vu.vf22, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  nop                            126
  lq_buffer(Mask::xyz, vu.vf16, vu.vi10 + 2);
  // mtir vi02, vf22.x          |  mulaw.xyzw ACC, vf11, vf00     127
  vu.acc.mula(Mask::xyzw, vu.vf11, vu.vf00.w());   vu.vi02 = vu.vf22.x_as_u16();
  // iaddi vi10, vi10, 0x3      |  maddax.xyzw ACC, vf08, vf16    128
  vu.acc.madda(Mask::xyzw, vu.vf08, vu.vf16.x());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf23, 0(vi10)        |  madday.xyzw ACC, vf09, vf16    129
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf16.y());   lq_buffer(Mask::xy, vu.vf23, vu.vi10);
  // lq.xyz vf17, 2(vi10)       |  nop                            130
  lq_buffer(Mask::xyz, vu.vf17, vu.vi10 + 2);
  // iand vi06, vi02, vi11      |  nop                            131
  vu.vi06 = vu.vi02 & vu.vi11;
  // mfir.x vf22, vi06          |  maddz.xyzw vf12, vf10, vf16    132
  vu.acc.madd(Mask::xyzw, vu.vf12, vu.vf10, vu.vf16.z());   vu.vf22.mfir(Mask::x, vu.vi06);
  // iadd vi14, vi10, vi01      |  ftoi12.z vf24, vf24            133
  // fmt::print("b: [{}] [{}]\n", vu.vf24.print(), vu.vf25.print());
  vu.vf24.ftoi12(Mask::z, vu.vf24);   vu.vi14 = vu.vi10 + vu.vi01;
  // isw.w vi12, 906(vi00)      |  ftoi12.z vf25, vf25            134
  vu.vf25.ftoi12(Mask::z, vu.vf25);   isw_buffer(Mask::w, vu.vi12, 906);
  // nop                        |  nop                            135

  // div Q, vf01.x, vf12.w      |  itof12.xyz vf18, vf22          136
  vu.vf18.itof12(Mask::xyz, vu.vf22);   vu.Q = vu.vf01.x() / vu.vf12.w();
  // mtir vi03, vf23.x          |  mulaw.xyzw ACC, vf11, vf00     137
  vu.acc.mula(Mask::xyzw, vu.vf11, vu.vf00.w());   vu.vi03 = vu.vf23.x_as_u16();
  // iaddi vi10, vi10, 0x3      |  maddax.xyzw ACC, vf08, vf17    138
  vu.acc.madda(Mask::xyzw, vu.vf08, vu.vf17.x());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf24, 0(vi10)        |  madday.xyzw ACC, vf09, vf17    139
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf17.y());   lq_buffer(Mask::xy, vu.vf24, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  nop                            140
  lq_buffer(Mask::xyz, vu.vf16, vu.vi10 + 2);
  // iand vi07, vi03, vi11      |  nop                            141
  vu.vi07 = vu.vi03 & vu.vi11;
  // mfir.x vf23, vi07          |  maddz.xyzw vf13, vf10, vf17    142
  vu.acc.madd(Mask::xyzw, vu.vf13, vu.vf10, vu.vf17.z());   vu.vf23.mfir(Mask::x, vu.vi07);
  // nop                        |  mul.xyz vf12, vf12, Q          143
  vu.vf12.mul(Mask::xyz, vu.vf12, vu.Q);
  // nop                        |  mul.xyz vf18, vf18, Q          144
  vu.vf18.mul(Mask::xyz, vu.vf18, vu.Q);
  // nop                        |  nop                            145

  // div Q, vf01.x, vf13.w      |  itof12.xyz vf19, vf23          146
  vu.vf19.itof12(Mask::xyz, vu.vf23);   vu.Q = vu.vf01.x() / vu.vf13.w();
  // nop                        |  add.xyzw vf12, vf12, vf04      147
  vu.vf12.add(Mask::xyzw, vu.vf12, vu.vf04);
  // mtir vi04, vf24.x          |  mulaw.xyzw ACC, vf11, vf00     148
  vu.acc.mula(Mask::xyzw, vu.vf11, vu.vf00.w());   vu.vi04 = vu.vf24.x_as_u16();
  // iaddi vi10, vi10, 0x3      |  maddax.xyzw ACC, vf08, vf16    149
  vu.acc.madda(Mask::xyzw, vu.vf08, vu.vf16.x());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf25, 0(vi10)        |  madday.xyzw ACC, vf09, vf16    150
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf16.y());   lq_buffer(Mask::xy, vu.vf25, vu.vi10);
  // lq.xyz vf17, 2(vi10)       |  miniz.w vf12, vf12, vf01       151
  vu.vf12.mini(Mask::w, vu.vf12, vu.vf01.z());   lq_buffer(Mask::xyz, vu.vf17, vu.vi10 + 2);
  // iand vi08, vi04, vi11      |  nop                            152
  vu.vi08 = vu.vi04 & vu.vi11;
  // mfir.x vf24, vi08          |  maddz.xyzw vf14, vf10, vf16    153
  vu.acc.madd(Mask::xyzw, vu.vf14, vu.vf10, vu.vf16.z());   vu.vf24.mfir(Mask::x, vu.vi08);
  // nop                        |  mul.xyz vf13, vf13, Q          154
  vu.vf13.mul(Mask::xyz, vu.vf13, vu.Q);
  // nop                        |  mul.xyz vf19, vf19, Q          155
  vu.vf19.mul(Mask::xyz, vu.vf19, vu.Q);
  // iaddi vi14, vi14, 0x9      |  maxy.w vf12, vf12, vf01        156
  vu.vf12.max(Mask::w, vu.vf12, vu.vf01.y());   vu.vi14 = vu.vi14 + 9;
  // fmt::print("vf12-1a: [{}]\n", vu.vf12.print());

L10:
  // fmt::print("vf12-1b: [{}]\n", vu.vf12.print());

  // div Q, vf01.x, vf14.w      |  itof12.xyz vf20, vf24          157
  vu.vf20.itof12(Mask::xyz, vu.vf24);   vu.Q = vu.vf01.x() / vu.vf14.w();
  // BRANCH!
  // ibeq vi02, vi06, L11       |  add.xyzw vf13, vf13, vf04      158
  vu.vf13.add(Mask::xyzw, vu.vf13, vu.vf04);   bc = (vu.vi02 == vu.vi06);
  // mtir vi05, vf25.x          |  mulaw.xyzw ACC, vf11, vf00     159
  vu.acc.mula(Mask::xyzw, vu.vf11, vu.vf00.w());   vu.vi05 = vu.vf25.x_as_u16();
  if (bc) { goto L11; }

  // nop                        |  addw.w vf12, vf12, vf01        160
  vu.vf12.add(Mask::w, vu.vf12, vu.vf01.w());
  L11:
  // iaddi vi10, vi10, 0x3      |  maddax.xyzw ACC, vf08, vf17    161
  vu.acc.madda(Mask::xyzw, vu.vf08, vu.vf17.x());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf22, 0(vi10)        |  madday.xyzw ACC, vf09, vf17    162
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf17.y());   lq_buffer(Mask::xy, vu.vf22, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  miniz.w vf13, vf13, vf01       163
  vu.vf13.mini(Mask::w, vu.vf13, vu.vf01.z());   lq_buffer(Mask::xyz, vu.vf16, vu.vi10 + 2);
  // fmt::print("vf16 vertex [{}] @ \n", vu.vf16.print(), vu.vi10 + 2);
  // iand vi09, vi05, vi11      |  ftoi4.xyzw vf12, vf12          164
  vu.vf12.ftoi4(Mask::xyzw, vu.vf12);   vu.vi09 = vu.vi05 & vu.vi11;
  // mfir.x vf25, vi09          |  maddz.xyzw vf15, vf10, vf17    165
  vu.acc.madd(Mask::xyzw, vu.vf15, vu.vf10, vu.vf17.z());   vu.vf25.mfir(Mask::x, vu.vi09);
  // sq.xyzw vf18, -12(vi10)    |  mul.xyz vf14, vf14, Q          166
  vu.vf14.mul(Mask::xyz, vu.vf14, vu.Q);   sq_buffer(Mask::xyzw, vu.vf18, vu.vi10 + -12);
  // BRANCH!
  // ibeq vi14, vi10, L15       |  mul.xyz vf20, vf20, Q          167
  vu.vf20.mul(Mask::xyz, vu.vf20, vu.Q);   bc = (vu.vi14 == vu.vi10);
  // fmt::print("store: {} {}\n", vu.vi10 - 10, vu.vf12.print_hex());
  // sq.xyzw vf12, -10(vi10)    |  maxy.w vf13, vf13, vf01        168
  vu.vf13.max(Mask::w, vu.vf13, vu.vf01.y());   sq_buffer(Mask::xyzw, vu.vf12, vu.vi10 + -10);
  if (bc) { goto L15; }

  // div Q, vf01.x, vf15.w      |  itof12.xyz vf21, vf25          169
  vu.vf21.itof12(Mask::xyz, vu.vf25);   vu.Q = vu.vf01.x() / vu.vf15.w();
  // BRANCH!
  // ibeq vi03, vi07, L12       |  add.xyzw vf14, vf14, vf04      170
  vu.vf14.add(Mask::xyzw, vu.vf14, vu.vf04);   bc = (vu.vi03 == vu.vi07);
  // mtir vi02, vf22.x          |  mulaw.xyzw ACC, vf11, vf00     171
  vu.acc.mula(Mask::xyzw, vu.vf11, vu.vf00.w());   vu.vi02 = vu.vf22.x_as_u16();
  if (bc) { goto L12; }

  // nop                        |  addw.w vf13, vf13, vf01        172
  vu.vf13.add(Mask::w, vu.vf13, vu.vf01.w());
  L12:
  // iaddi vi10, vi10, 0x3      |  maddax.xyzw ACC, vf08, vf16    173
  vu.acc.madda(Mask::xyzw, vu.vf08, vu.vf16.x());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf23, 0(vi10)        |  madday.xyzw ACC, vf09, vf16    174
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf16.y());   lq_buffer(Mask::xy, vu.vf23, vu.vi10);
  // lq.xyz vf17, 2(vi10)       |  miniz.w vf14, vf14, vf01       175
  vu.vf14.mini(Mask::w, vu.vf14, vu.vf01.z());   lq_buffer(Mask::xyz, vu.vf17, vu.vi10 + 2);
  // iand vi06, vi02, vi11      |  ftoi4.xyzw vf13, vf13          176
  vu.vf13.ftoi4(Mask::xyzw, vu.vf13);   vu.vi06 = vu.vi02 & vu.vi11;
  // mfir.x vf22, vi06          |  maddz.xyzw vf12, vf10, vf16    177
  vu.acc.madd(Mask::xyzw, vu.vf12, vu.vf10, vu.vf16.z());   vu.vf22.mfir(Mask::x, vu.vi06);
  // fmt::print("vf12 transformed: [{}]\n", vu.vf12.print());
  // sq.xyzw vf19, -12(vi10)    |  mul.xyz vf15, vf15, Q          178
  vu.vf15.mul(Mask::xyz, vu.vf15, vu.Q);   sq_buffer(Mask::xyzw, vu.vf19, vu.vi10 + -12);
  // BRANCH!
  // ibeq vi14, vi10, L15       |  mul.xyz vf21, vf21, Q          179
  vu.vf21.mul(Mask::xyz, vu.vf21, vu.Q);   bc = (vu.vi14 == vu.vi10);
  // sq.xyzw vf13, -10(vi10)    |  maxy.w vf14, vf14, vf01        180
  vu.vf14.max(Mask::w, vu.vf14, vu.vf01.y());   sq_buffer(Mask::xyzw, vu.vf13, vu.vi10 + -10);
  if (bc) { goto L15; }

  // div Q, vf01.x, vf12.w      |  itof12.xyz vf18, vf22          181
  vu.vf18.itof12(Mask::xyz, vu.vf22);   vu.Q = vu.vf01.x() / vu.vf12.w();
  // BRANCH!
  // ibeq vi04, vi08, L13       |  add.xyzw vf15, vf15, vf04      182
  vu.vf15.add(Mask::xyzw, vu.vf15, vu.vf04);   bc = (vu.vi04 == vu.vi08);
  // mtir vi03, vf23.x          |  mulaw.xyzw ACC, vf11, vf00     183
  vu.acc.mula(Mask::xyzw, vu.vf11, vu.vf00.w());   vu.vi03 = vu.vf23.x_as_u16();
  if (bc) { goto L13; }

  // nop                        |  addw.w vf14, vf14, vf01        184
  vu.vf14.add(Mask::w, vu.vf14, vu.vf01.w());
  L13:
  // iaddi vi10, vi10, 0x3      |  maddax.xyzw ACC, vf08, vf17    185
  vu.acc.madda(Mask::xyzw, vu.vf08, vu.vf17.x());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf24, 0(vi10)        |  madday.xyzw ACC, vf09, vf17    186
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf17.y());   lq_buffer(Mask::xy, vu.vf24, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  miniz.w vf15, vf15, vf01       187
  vu.vf15.mini(Mask::w, vu.vf15, vu.vf01.z());   lq_buffer(Mask::xyz, vu.vf16, vu.vi10 + 2);
  // iand vi07, vi03, vi11      |  ftoi4.xyzw vf14, vf14          188
  vu.vf14.ftoi4(Mask::xyzw, vu.vf14);   vu.vi07 = vu.vi03 & vu.vi11;
  // mfir.x vf23, vi07          |  maddz.xyzw vf13, vf10, vf17    189
  vu.acc.madd(Mask::xyzw, vu.vf13, vu.vf10, vu.vf17.z());   vu.vf23.mfir(Mask::x, vu.vi07);
  // sq.xyzw vf20, -12(vi10)    |  mul.xyz vf12, vf12, Q          190
  vu.vf12.mul(Mask::xyz, vu.vf12, vu.Q);   sq_buffer(Mask::xyzw, vu.vf20, vu.vi10 + -12);
  // BRANCH!
  // ibeq vi14, vi10, L15       |  mul.xyz vf18, vf18, Q          191
  vu.vf18.mul(Mask::xyz, vu.vf18, vu.Q);   bc = (vu.vi14 == vu.vi10);
  // sq.xyzw vf14, -10(vi10)    |  maxy.w vf15, vf15, vf01        192
  vu.vf15.max(Mask::w, vu.vf15, vu.vf01.y());   sq_buffer(Mask::xyzw, vu.vf14, vu.vi10 + -10);
  if (bc) { goto L15; }

  // div Q, vf01.x, vf13.w      |  itof12.xyz vf19, vf23          193
  vu.vf19.itof12(Mask::xyz, vu.vf23);   vu.Q = vu.vf01.x() / vu.vf13.w();
  // BRANCH!
  // ibeq vi05, vi09, L14       |  add.xyzw vf12, vf12, vf04      194
  vu.vf12.add(Mask::xyzw, vu.vf12, vu.vf04);   bc = (vu.vi05 == vu.vi09);
  // mtir vi04, vf24.x          |  mulaw.xyzw ACC, vf11, vf00     195
  vu.acc.mula(Mask::xyzw, vu.vf11, vu.vf00.w());   vu.vi04 = vu.vf24.x_as_u16();
  if (bc) { goto L14; }

  // nop                        |  addw.w vf15, vf15, vf01        196
  vu.vf15.add(Mask::w, vu.vf15, vu.vf01.w());
  L14:
  // iaddi vi10, vi10, 0x3      |  maddax.xyzw ACC, vf08, vf16    197
  vu.acc.madda(Mask::xyzw, vu.vf08, vu.vf16.x());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf25, 0(vi10)        |  madday.xyzw ACC, vf09, vf16    198
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf16.y());   lq_buffer(Mask::xy, vu.vf25, vu.vi10);
  // lq.xyz vf17, 2(vi10)       |  miniz.w vf12, vf12, vf01       199
  vu.vf12.mini(Mask::w, vu.vf12, vu.vf01.z());   lq_buffer(Mask::xyz, vu.vf17, vu.vi10 + 2);
  // iand vi08, vi04, vi11      |  ftoi4.xyzw vf15, vf15          200
  vu.vf15.ftoi4(Mask::xyzw, vu.vf15);   vu.vi08 = vu.vi04 & vu.vi11;
  // mfir.x vf24, vi08          |  maddz.xyzw vf14, vf10, vf16    201
  vu.acc.madd(Mask::xyzw, vu.vf14, vu.vf10, vu.vf16.z());   vu.vf24.mfir(Mask::x, vu.vi08);
  // sq.xyzw vf21, -12(vi10)    |  mul.xyz vf13, vf13, Q          202
  vu.vf13.mul(Mask::xyz, vu.vf13, vu.Q);   sq_buffer(Mask::xyzw, vu.vf21, vu.vi10 + -12);
  // BRANCH!
  // ibne vi14, vi10, L10       |  mul.xyz vf19, vf19, Q          203
  vu.vf19.mul(Mask::xyz, vu.vf19, vu.Q);   bc = (vu.vi14 != vu.vi10);
  // sq.xyzw vf15, -10(vi10)    |  maxy.w vf12, vf12, vf01        204
  // fmt::print("reloop {} {}\n", vu.vi14, vu.vi10);
  vu.vf12.max(Mask::w, vu.vf12, vu.vf01.y());   sq_buffer(Mask::xyzw, vu.vf15, vu.vi10 + -10);
  if (bc) { goto L10; }

  L15:
  // BRANCH!
  // b L82                      |  nop                            205
  bc = true;
  // ilw.w vi12, 906(vi00)      |  nop                            206
  ilw_buffer(Mask::w, vu.vi12, 906);
  if (bc) { goto L82; }

  // isubiu vi02, vi13, 0x363   |  addw.z vf22, vf00, vf00        207
  vu.vf22.add(Mask::z, vu.vf00, vu.vf00.w());   vu.vi02 = vu.vi13 - 0x363; /* 867 */
  // iaddiu vi13, vi13, 0x1e    |  addw.z vf23, vf00, vf00        208
  vu.vf23.add(Mask::z, vu.vf00, vu.vf00.w());   vu.vi13 = vu.vi13 + 0x1e; /* 30 */
  // BRANCH!
  // ibne vi00, vi02, L16       |  addw.z vf24, vf00, vf00        209
  vu.vf24.add(Mask::z, vu.vf00, vu.vf00.w());   bc = (vu.vi02 != 0);
  // nop                        |  addw.z vf25, vf00, vf00        210
  vu.vf25.add(Mask::z, vu.vf00, vu.vf00.w());
  if (bc) { goto L16; }

  // iaddiu vi13, vi00, 0x345   |  nop                            211
  vu.vi13 = 0x345; /* 837 */
  L16:
  // iaddi vi03, vi13, 0x7      |  nop                            212
  vu.vi03 = vu.vi13 + 7;
  // ilw.w vi01, 5(vi13)        |  nop                            213
  ilw_buffer(Mask::w, vu.vi01, vu.vi13 + 5);
  // isw.x vi03, 906(vi00)      |  nop                            214
  isw_buffer(Mask::x, vu.vi03, 906);
  // iaddi vi10, vi12, 0x9      |  subw.w vf18, vf00, vf00        215
  vu.vf18.sub(Mask::w, vu.vf00, vu.vf00.w());   vu.vi10 = vu.vi12 + 9;
  // lq.xyzw vf08, 0(vi13)      |  subw.w vf19, vf00, vf00        216
  vu.vf19.sub(Mask::w, vu.vf00, vu.vf00.w());   lq_buffer(Mask::xyzw, vu.vf08, vu.vi13);
  // lq.xyzw vf09, 1(vi13)      |  subw.w vf20, vf00, vf00        217
  vu.vf20.sub(Mask::w, vu.vf00, vu.vf00.w());   lq_buffer(Mask::xyzw, vu.vf09, vu.vi13 + 1);
  // lq.xyzw vf10, 2(vi13)      |  subw.w vf21, vf00, vf00        218
  vu.vf21.sub(Mask::w, vu.vf00, vu.vf00.w());   lq_buffer(Mask::xyzw, vu.vf10, vu.vi13 + 2);
  // lq.xyzw vf11, 3(vi13)      |  ftoi12.z vf22, vf22            219
  vu.vf22.ftoi12(Mask::z, vu.vf22);   lq_buffer(Mask::xyzw, vu.vf11, vu.vi13 + 3);
  // iadd vi02, vi01, vi01      |  ftoi12.z vf23, vf23            220
  vu.vf23.ftoi12(Mask::z, vu.vf23);   vu.vi02 = vu.vi01 + vu.vi01;
  // iadd vi01, vi01, vi02      |  sub.xyzw vf16, vf16, vf16      221
  vu.vf16.set_zero();         vu.vi01 = vu.vi01 + vu.vi02;
  // iaddi vi11, vi00, -0x2     |  nop                            222
  vu.vi11 = -2;
  // iadd vi14, vi10, vi01      |  ftoi12.z vf24, vf24            223
  vu.vf24.ftoi12(Mask::z, vu.vf24);   vu.vi14 = vu.vi10 + vu.vi01;
  // isw.w vi12, 906(vi00)      |  ftoi12.z vf25, vf25            224
  vu.vf25.ftoi12(Mask::z, vu.vf25);   isw_buffer(Mask::w, vu.vi12, 906);
  // iaddi vi14, vi14, 0x9      |  nop                            225
  vu.vi14 = vu.vi14 + 9;
  // lq.xy vf22, 0(vi10)        |  nop                            226
  lq_buffer(Mask::xy, vu.vf22, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  nop                            227
  lq_buffer(Mask::xyz, vu.vf16, vu.vi10 + 2);
  // nop                        |  nop                            228

  // nop                        |  nop                            229

  // nop                        |  mulaw.xyzw ACC, vf11, vf00     230
  vu.acc.mula(Mask::xyzw, vu.vf11, vu.vf00.w());
  // mtir vi02, vf22.x          |  maddax.xyzw ACC, vf08, vf16    231
  vu.acc.madda(Mask::xyzw, vu.vf08, vu.vf16.x());   vu.vi02 = vu.vf22.x_as_u16();
  // iaddi vi10, vi10, 0x3      |  madday.xyzw ACC, vf09, vf16    232
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf16.y());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf23, 0(vi10)        |  maddz.xyzw vf12, vf10, vf16    233
  vu.acc.madd(Mask::xyzw, vu.vf12, vu.vf10, vu.vf16.z());   lq_buffer(Mask::xy, vu.vf23, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  nop                            234
  lq_buffer(Mask::xyz, vu.vf16, vu.vi10 + 2);
  // iand vi06, vi02, vi11      |  nop                            235
  vu.vi06 = vu.vi02 & vu.vi11;
  // mfir.x vf22, vi06          |  nop                            236
  vu.vf22.mfir(Mask::x, vu.vi06);
  // nop                        |  nop                            237

  // nop                        |  nop                            238

  // nop                        |  nop                            239

  // nop                        |  itof12.xyz vf18, vf22          240
  vu.vf18.itof12(Mask::xyz, vu.vf22);
  // div Q, vf01.x, vf12.w      |  mul.xyzw vf26, vf12, vf05      241
  vu.vf26.mul(Mask::xyzw, vu.vf12, vu.vf05);   vu.Q = vu.vf01.x() / vu.vf12.w();
  // nop                        |  nop                            242

  // nop                        |  mulaw.xyzw ACC, vf11, vf00     243
  vu.acc.mula(Mask::xyzw, vu.vf11, vu.vf00.w());
  // mtir vi03, vf23.x          |  maddax.xyzw ACC, vf08, vf16    244
  vu.acc.madda(Mask::xyzw, vu.vf08, vu.vf16.x());   vu.vi03 = vu.vf23.x_as_u16();
  // iaddi vi10, vi10, 0x3      |  madday.xyzw ACC, vf09, vf16    245
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf16.y());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf24, 0(vi10)        |  maddz.xyzw vf13, vf10, vf16    246
  vu.acc.madd(Mask::xyzw, vu.vf13, vu.vf10, vu.vf16.z());   lq_buffer(Mask::xy, vu.vf24, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  nop                            247
  lq_buffer(Mask::xyz, vu.vf16, vu.vi10 + 2);
  // iand vi07, vi03, vi11      |  nop                            248
  vu.vi07 = vu.vi03 & vu.vi11;
  // mfir.x vf23, vi07          |  mul.xyz vf12, vf12, Q          249
  vu.vf12.mul(Mask::xyz, vu.vf12, vu.Q);   vu.vf23.mfir(Mask::x, vu.vi07);
  // fcset 0x0                  |  nop                            250
  ASSERT(false);
  // nop                        |  nop                            251

  // nop                        |  mul.xyz vf18, vf18, Q          252
  vu.vf18.mul(Mask::xyz, vu.vf18, vu.Q);
  // nop                        |  itof12.xyz vf19, vf23          253
  vu.vf19.itof12(Mask::xyz, vu.vf23);
  // div Q, vf01.x, vf13.w      |  mulaw.xyzw ACC, vf11, vf00     254
  vu.acc.mula(Mask::xyzw, vu.vf11, vu.vf00.w());   vu.Q = vu.vf01.x() / vu.vf13.w();
  // nop                        |  add.xyzw vf12, vf12, vf04      255
  vu.vf12.add(Mask::xyzw, vu.vf12, vu.vf04);
  // nop                        |  maddax.xyzw ACC, vf08, vf16    256
  vu.acc.madda(Mask::xyzw, vu.vf08, vu.vf16.x());
  // mtir vi04, vf24.x          |  madday.xyzw ACC, vf09, vf16    257
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf16.y());   vu.vi04 = vu.vf24.x_as_u16();
  // iaddi vi10, vi10, 0x3      |  maddz.xyzw vf14, vf10, vf16    258
  vu.acc.madd(Mask::xyzw, vu.vf14, vu.vf10, vu.vf16.z());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf25, 0(vi10)        |  miniz.w vf12, vf12, vf01       259
  vu.vf12.mini(Mask::w, vu.vf12, vu.vf01.z());   lq_buffer(Mask::xy, vu.vf25, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  mul.xyzw vf27, vf13, vf05      260
  vu.vf27.mul(Mask::xyzw, vu.vf13, vu.vf05);   lq_buffer(Mask::xyz, vu.vf16, vu.vi10 + 2);
  // iand vi08, vi04, vi11      |  nop                            261
  vu.vi08 = vu.vi04 & vu.vi11;
  // mfir.x vf24, vi08          |  mul.xyz vf13, vf13, Q          262
  vu.vf13.mul(Mask::xyz, vu.vf13, vu.Q);   vu.vf24.mfir(Mask::x, vu.vi08);
  // nop                        |  maxy.w vf12, vf12, vf01        263
  vu.vf12.max(Mask::w, vu.vf12, vu.vf01.y());
  // nop                        |  clipw.xyz vf26, vf26           264
  ASSERT(false); cf = clip(vu.vf26, vu.vf26.w(), cf);
  // nop                        |  mul.xyz vf19, vf19, Q          265
  vu.vf19.mul(Mask::xyz, vu.vf19, vu.Q);
  // BRANCH!
  // ibeq vi02, vi06, L17       |  itof12.xyz vf20, vf24          266
  vu.vf20.itof12(Mask::xyz, vu.vf24);   bc = (vu.vi02 == vu.vi06);
  // div Q, vf01.x, vf14.w      |  mulaw.xyzw ACC, vf11, vf00     267
  vu.acc.mula(Mask::xyzw, vu.vf11, vu.vf00.w());   vu.Q = vu.vf01.x() / vu.vf14.w();
  if (bc) { goto L17; }

  // nop                        |  addw.w vf12, vf12, vf01        268
  vu.vf12.add(Mask::w, vu.vf12, vu.vf01.w());
  L17:
  // nop                        |  add.xyzw vf13, vf13, vf04      269
  vu.vf13.add(Mask::xyzw, vu.vf13, vu.vf04);
  // nop                        |  maddax.xyzw ACC, vf08, vf16    270
  vu.acc.madda(Mask::xyzw, vu.vf08, vu.vf16.x());
  // mtir vi05, vf25.x          |  madday.xyzw ACC, vf09, vf16    271
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf16.y());   vu.vi05 = vu.vf25.x_as_u16();
  // iaddi vi10, vi10, 0x3      |  maddz.xyzw vf15, vf10, vf16    272
  vu.acc.madd(Mask::xyzw, vu.vf15, vu.vf10, vu.vf16.z());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf22, 0(vi10)        |  miniz.w vf13, vf13, vf01       273
  vu.vf13.mini(Mask::w, vu.vf13, vu.vf01.z());   lq_buffer(Mask::xy, vu.vf22, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  mul.xyzw vf28, vf14, vf05      274
  vu.vf28.mul(Mask::xyzw, vu.vf14, vu.vf05);   lq_buffer(Mask::xyz, vu.vf16, vu.vi10 + 2);
  // iand vi09, vi05, vi11      |  ftoi4.xyzw vf12, vf12          275
  vu.vf12.ftoi4(Mask::xyzw, vu.vf12);   vu.vi09 = vu.vi05 & vu.vi11;
  // mfir.x vf25, vi09          |  mul.xyz vf14, vf14, Q          276
  vu.vf14.mul(Mask::xyz, vu.vf14, vu.Q);   vu.vf25.mfir(Mask::x, vu.vi09);
  // sq.xyzw vf18, -12(vi10)    |  maxy.w vf13, vf13, vf01        277
  vu.vf13.max(Mask::w, vu.vf13, vu.vf01.y());   sq_buffer(Mask::xyzw, vu.vf18, vu.vi10 + -12);
  // BRANCH!
  // ibeq vi14, vi10, L28       |  clipw.xyz vf27, vf27           278
  ASSERT(false); cf = clip(vu.vf27, vu.vf27.w(), cf);   bc = (vu.vi14 == vu.vi10);
  // sq.xyzw vf12, -10(vi10)    |  mul.xyz vf20, vf20, Q          279
  vu.vf20.mul(Mask::xyz, vu.vf20, vu.Q);   sq_buffer(Mask::xyzw, vu.vf12, vu.vi10 + -10);
  if (bc) { goto L28; }

  // BRANCH!
  // ibeq vi03, vi07, L18       |  itof12.xyz vf21, vf25          280
  vu.vf21.itof12(Mask::xyz, vu.vf25);   bc = (vu.vi03 == vu.vi07);
  // div Q, vf01.x, vf15.w      |  mulaw.xyzw ACC, vf11, vf00     281
  vu.acc.mula(Mask::xyzw, vu.vf11, vu.vf00.w());   vu.Q = vu.vf01.x() / vu.vf15.w();
  if (bc) { goto L18; }

  // nop                        |  addw.w vf13, vf13, vf01        282
  vu.vf13.add(Mask::w, vu.vf13, vu.vf01.w());
  L18:
  // nop                        |  add.xyzw vf14, vf14, vf04      283
  vu.vf14.add(Mask::xyzw, vu.vf14, vu.vf04);
  // nop                        |  maddax.xyzw ACC, vf08, vf16    284
  vu.acc.madda(Mask::xyzw, vu.vf08, vu.vf16.x());
  // mtir vi02, vf22.x          |  madday.xyzw ACC, vf09, vf16    285
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf16.y());   vu.vi02 = vu.vf22.x_as_u16();
  // iaddi vi10, vi10, 0x3      |  maddz.xyzw vf12, vf10, vf16    286
  vu.acc.madd(Mask::xyzw, vu.vf12, vu.vf10, vu.vf16.z());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf23, 0(vi10)        |  miniz.w vf14, vf14, vf01       287
  vu.vf14.mini(Mask::w, vu.vf14, vu.vf01.z());   lq_buffer(Mask::xy, vu.vf23, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  mul.xyzw vf29, vf15, vf05      288
  vu.vf29.mul(Mask::xyzw, vu.vf15, vu.vf05);   lq_buffer(Mask::xyz, vu.vf16, vu.vi10 + 2);
  // iand vi06, vi02, vi11      |  ftoi4.xyzw vf13, vf13          289
  vu.vf13.ftoi4(Mask::xyzw, vu.vf13);   vu.vi06 = vu.vi02 & vu.vi11;
  // mfir.x vf22, vi06          |  mul.xyz vf15, vf15, Q          290
  vu.vf15.mul(Mask::xyz, vu.vf15, vu.Q);   vu.vf22.mfir(Mask::x, vu.vi06);
  // sq.xyzw vf19, -12(vi10)    |  maxy.w vf14, vf14, vf01        291
  vu.vf14.max(Mask::w, vu.vf14, vu.vf01.y());   sq_buffer(Mask::xyzw, vu.vf19, vu.vi10 + -12);
  // BRANCH!
  // ibeq vi14, vi10, L28       |  clipw.xyz vf28, vf28           292
  ASSERT(false); cf = clip(vu.vf28, vu.vf28.w(), cf);   bc = (vu.vi14 == vu.vi10);
  // sq.xyzw vf13, -10(vi10)    |  mul.xyz vf21, vf21, Q          293
  vu.vf21.mul(Mask::xyz, vu.vf21, vu.Q);   sq_buffer(Mask::xyzw, vu.vf13, vu.vi10 + -10);
  if (bc) { goto L28; }

  L19:
  // BRANCH!
  // ibeq vi04, vi08, L20       |  itof12.xyz vf18, vf22          294
  vu.vf18.itof12(Mask::xyz, vu.vf22);   bc = (vu.vi04 == vu.vi08);
  // div Q, vf01.x, vf12.w      |  mulaw.xyzw ACC, vf11, vf00     295
  vu.acc.mula(Mask::xyzw, vu.vf11, vu.vf00.w());   vu.Q = vu.vf01.x() / vu.vf12.w();
  if (bc) { goto L20; }

  // nop                        |  addw.w vf14, vf14, vf01        296
  vu.vf14.add(Mask::w, vu.vf14, vu.vf01.w());
  L20:
  // fcand vi01, 0x3ffff        |  add.xyzw vf15, vf15, vf04      297
  vu.vf15.add(Mask::xyzw, vu.vf15, vu.vf04);   ASSERT(false); vu.vi01 = cf & 0x3ffff;

  // BRANCH!
  // ibne vi00, vi01, L31       |  maddax.xyzw ACC, vf08, vf16    298
  vu.acc.madda(Mask::xyzw, vu.vf08, vu.vf16.x());   bc = (vu.vi01 != 0);
  // mtir vi03, vf23.x          |  madday.xyzw ACC, vf09, vf16    299
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf16.y());   vu.vi03 = vu.vf23.x_as_u16();
  if (bc) { goto L31; }

  L21:
  // iaddi vi10, vi10, 0x3      |  maddz.xyzw vf13, vf10, vf16    300
  vu.acc.madd(Mask::xyzw, vu.vf13, vu.vf10, vu.vf16.z());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf24, 0(vi10)        |  miniz.w vf15, vf15, vf01       301
  vu.vf15.mini(Mask::w, vu.vf15, vu.vf01.z());   lq_buffer(Mask::xy, vu.vf24, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  mul.xyzw vf26, vf12, vf05      302
  vu.vf26.mul(Mask::xyzw, vu.vf12, vu.vf05);   lq_buffer(Mask::xyz, vu.vf16, vu.vi10 + 2);
  // iand vi07, vi03, vi11      |  ftoi4.xyzw vf14, vf14          303
  vu.vf14.ftoi4(Mask::xyzw, vu.vf14);   vu.vi07 = vu.vi03 & vu.vi11;
  // mfir.x vf23, vi07          |  mul.xyz vf12, vf12, Q          304
  vu.vf12.mul(Mask::xyz, vu.vf12, vu.Q);   vu.vf23.mfir(Mask::x, vu.vi07);
  // sq.xyzw vf20, -12(vi10)    |  maxy.w vf15, vf15, vf01        305
  vu.vf15.max(Mask::w, vu.vf15, vu.vf01.y());   sq_buffer(Mask::xyzw, vu.vf20, vu.vi10 + -12);
  // BRANCH!
  // ibeq vi14, vi10, L28       |  clipw.xyz vf29, vf29           306
  ASSERT(false); cf = clip(vu.vf29, vu.vf29.w(), cf);   bc = (vu.vi14 == vu.vi10);
  // sq.xyzw vf14, -10(vi10)    |  mul.xyz vf18, vf18, Q          307
  vu.vf18.mul(Mask::xyz, vu.vf18, vu.Q);   sq_buffer(Mask::xyzw, vu.vf14, vu.vi10 + -10);
  if (bc) { goto L28; }

  // BRANCH!
  // ibeq vi05, vi09, L22       |  itof12.xyz vf19, vf23          308
  vu.vf19.itof12(Mask::xyz, vu.vf23);   bc = (vu.vi05 == vu.vi09);
  // div Q, vf01.x, vf13.w      |  mulaw.xyzw ACC, vf11, vf00     309
  vu.acc.mula(Mask::xyzw, vu.vf11, vu.vf00.w());   vu.Q = vu.vf01.x() / vu.vf13.w();
  if (bc) { goto L22; }

  // nop                        |  addw.w vf15, vf15, vf01        310
  vu.vf15.add(Mask::w, vu.vf15, vu.vf01.w());
  L22:
  // fcand vi01, 0x3ffff        |  add.xyzw vf12, vf12, vf04      311
  vu.vf12.add(Mask::xyzw, vu.vf12, vu.vf04);   ASSERT(false); vu.vi01 = cf & 0x3ffff;

  // BRANCH!
  // ibne vi00, vi01, L32       |  maddax.xyzw ACC, vf08, vf16    312
  vu.acc.madda(Mask::xyzw, vu.vf08, vu.vf16.x());   bc = (vu.vi01 != 0);
  // mtir vi04, vf24.x          |  madday.xyzw ACC, vf09, vf16    313
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf16.y());   vu.vi04 = vu.vf24.x_as_u16();
  if (bc) { goto L32; }

  L23:
  // iaddi vi10, vi10, 0x3      |  maddz.xyzw vf14, vf10, vf16    314
  vu.acc.madd(Mask::xyzw, vu.vf14, vu.vf10, vu.vf16.z());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf25, 0(vi10)        |  miniz.w vf12, vf12, vf01       315
  vu.vf12.mini(Mask::w, vu.vf12, vu.vf01.z());   lq_buffer(Mask::xy, vu.vf25, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  mul.xyzw vf27, vf13, vf05      316
  vu.vf27.mul(Mask::xyzw, vu.vf13, vu.vf05);   lq_buffer(Mask::xyz, vu.vf16, vu.vi10 + 2);
  // iand vi08, vi04, vi11      |  ftoi4.xyzw vf15, vf15          317
  vu.vf15.ftoi4(Mask::xyzw, vu.vf15);   vu.vi08 = vu.vi04 & vu.vi11;
  // mfir.x vf24, vi08          |  mul.xyz vf13, vf13, Q          318
  vu.vf13.mul(Mask::xyz, vu.vf13, vu.Q);   vu.vf24.mfir(Mask::x, vu.vi08);
  // sq.xyzw vf21, -12(vi10)    |  maxy.w vf12, vf12, vf01        319
  vu.vf12.max(Mask::w, vu.vf12, vu.vf01.y());   sq_buffer(Mask::xyzw, vu.vf21, vu.vi10 + -12);
  // BRANCH!
  // ibeq vi14, vi10, L28       |  clipw.xyz vf26, vf26           320
  ASSERT(false); cf = clip(vu.vf26, vu.vf26.w(), cf);   bc = (vu.vi14 == vu.vi10);
  // sq.xyzw vf15, -10(vi10)    |  mul.xyz vf19, vf19, Q          321
  vu.vf19.mul(Mask::xyz, vu.vf19, vu.Q);   sq_buffer(Mask::xyzw, vu.vf15, vu.vi10 + -10);
  if (bc) { goto L28; }

  // BRANCH!
  // ibeq vi02, vi06, L24       |  itof12.xyz vf20, vf24          322
  vu.vf20.itof12(Mask::xyz, vu.vf24);   bc = (vu.vi02 == vu.vi06);
  // div Q, vf01.x, vf14.w      |  mulaw.xyzw ACC, vf11, vf00     323
  vu.acc.mula(Mask::xyzw, vu.vf11, vu.vf00.w());   vu.Q = vu.vf01.x() / vu.vf14.w();
  if (bc) { goto L24; }

  // nop                        |  addw.w vf12, vf12, vf01        324
  vu.vf12.add(Mask::w, vu.vf12, vu.vf01.w());
  L24:
  // fcand vi01, 0x3ffff        |  add.xyzw vf13, vf13, vf04      325
  vu.vf13.add(Mask::xyzw, vu.vf13, vu.vf04);   ASSERT(false); vu.vi01 = cf & 0x3ffff;

  // BRANCH!
  // ibne vi00, vi01, L29       |  maddax.xyzw ACC, vf08, vf16    326
  vu.acc.madda(Mask::xyzw, vu.vf08, vu.vf16.x());   bc = (vu.vi01 != 0);
  // mtir vi05, vf25.x          |  madday.xyzw ACC, vf09, vf16    327
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf16.y());   vu.vi05 = vu.vf25.x_as_u16();
  if (bc) { goto L29; }

  L25:
  // iaddi vi10, vi10, 0x3      |  maddz.xyzw vf15, vf10, vf16    328
  vu.acc.madd(Mask::xyzw, vu.vf15, vu.vf10, vu.vf16.z());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf22, 0(vi10)        |  miniz.w vf13, vf13, vf01       329
  vu.vf13.mini(Mask::w, vu.vf13, vu.vf01.z());   lq_buffer(Mask::xy, vu.vf22, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  mul.xyzw vf28, vf14, vf05      330
  vu.vf28.mul(Mask::xyzw, vu.vf14, vu.vf05);   lq_buffer(Mask::xyz, vu.vf16, vu.vi10 + 2);
  // iand vi09, vi05, vi11      |  ftoi4.xyzw vf12, vf12          331
  vu.vf12.ftoi4(Mask::xyzw, vu.vf12);   vu.vi09 = vu.vi05 & vu.vi11;
  // mfir.x vf25, vi09          |  mul.xyz vf14, vf14, Q          332
  vu.vf14.mul(Mask::xyz, vu.vf14, vu.Q);   vu.vf25.mfir(Mask::x, vu.vi09);
  // sq.xyzw vf18, -12(vi10)    |  maxy.w vf13, vf13, vf01        333
  vu.vf13.max(Mask::w, vu.vf13, vu.vf01.y());   sq_buffer(Mask::xyzw, vu.vf18, vu.vi10 + -12);
  // BRANCH!
  // ibeq vi14, vi10, L28       |  clipw.xyz vf27, vf27           334
  ASSERT(false); cf = clip(vu.vf27, vu.vf27.w(), cf);   bc = (vu.vi14 == vu.vi10);
  // sq.xyzw vf12, -10(vi10)    |  mul.xyz vf20, vf20, Q          335
  vu.vf20.mul(Mask::xyz, vu.vf20, vu.Q);   sq_buffer(Mask::xyzw, vu.vf12, vu.vi10 + -10);
  if (bc) { goto L28; }

  // BRANCH!
  // ibeq vi03, vi07, L26       |  itof12.xyz vf21, vf25          336
  vu.vf21.itof12(Mask::xyz, vu.vf25);   bc = (vu.vi03 == vu.vi07);
  // div Q, vf01.x, vf15.w      |  mulaw.xyzw ACC, vf11, vf00     337
  vu.acc.mula(Mask::xyzw, vu.vf11, vu.vf00.w());   vu.Q = vu.vf01.x() / vu.vf15.w();
  if (bc) { goto L26; }

  // nop                        |  addw.w vf13, vf13, vf01        338
  vu.vf13.add(Mask::w, vu.vf13, vu.vf01.w());
  L26:
  // fcand vi01, 0x3ffff        |  add.xyzw vf14, vf14, vf04      339
  vu.vf14.add(Mask::xyzw, vu.vf14, vu.vf04);   ASSERT(false); vu.vi01 = cf & 0x3ffff;

  // BRANCH!
  // ibne vi00, vi01, L30       |  maddax.xyzw ACC, vf08, vf16    340
  vu.acc.madda(Mask::xyzw, vu.vf08, vu.vf16.x());   bc = (vu.vi01 != 0);
  // mtir vi02, vf22.x          |  madday.xyzw ACC, vf09, vf16    341
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf16.y());   vu.vi02 = vu.vf22.x_as_u16();
  if (bc) { goto L30; }

  L27:
  // iaddi vi10, vi10, 0x3      |  maddz.xyzw vf12, vf10, vf16    342
  vu.acc.madd(Mask::xyzw, vu.vf12, vu.vf10, vu.vf16.z());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf23, 0(vi10)        |  miniz.w vf14, vf14, vf01       343
  vu.vf14.mini(Mask::w, vu.vf14, vu.vf01.z());   lq_buffer(Mask::xy, vu.vf23, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  mul.xyzw vf29, vf15, vf05      344
  vu.vf29.mul(Mask::xyzw, vu.vf15, vu.vf05);   lq_buffer(Mask::xyz, vu.vf16, vu.vi10 + 2);
  // iand vi06, vi02, vi11      |  ftoi4.xyzw vf13, vf13          345
  vu.vf13.ftoi4(Mask::xyzw, vu.vf13);   vu.vi06 = vu.vi02 & vu.vi11;
  // mfir.x vf22, vi06          |  mul.xyz vf15, vf15, Q          346
  vu.vf15.mul(Mask::xyz, vu.vf15, vu.Q);   vu.vf22.mfir(Mask::x, vu.vi06);
  // sq.xyzw vf19, -12(vi10)    |  maxy.w vf14, vf14, vf01        347
  vu.vf14.max(Mask::w, vu.vf14, vu.vf01.y());   sq_buffer(Mask::xyzw, vu.vf19, vu.vi10 + -12);
  // BRANCH!
  // ibne vi14, vi10, L19       |  clipw.xyz vf28, vf28           348
  ASSERT(false); cf = clip(vu.vf28, vu.vf28.w(), cf);   bc = (vu.vi14 != vu.vi10);
  // sq.xyzw vf13, -10(vi10)    |  mul.xyz vf21, vf21, Q          349
  vu.vf21.mul(Mask::xyz, vu.vf21, vu.Q);   sq_buffer(Mask::xyzw, vu.vf13, vu.vi10 + -10);
  if (bc) { goto L19; }

  L28:
  // BRANCH!
  // b L82                      |  nop                            350
  bc = true;
  // ilw.w vi12, 906(vi00)      |  nop                            351
  ilw_buffer(Mask::w, vu.vi12, 906);
  if (bc) { goto L82; }

  L29:
  // BRANCH!
  // b L25                      |  addw.w vf12, vf12, vf01        352
  vu.vf12.add(Mask::w, vu.vf12, vu.vf01.w());   bc = true;
  // nop                        |  nop                            353

  if (bc) { goto L25; }

  L30:
  // BRANCH!
  // b L27                      |  addw.w vf13, vf13, vf01        354
  vu.vf13.add(Mask::w, vu.vf13, vu.vf01.w());   bc = true;
  // nop                        |  nop                            355

  if (bc) { goto L27; }

  L31:
  // BRANCH!
  // b L21                      |  addw.w vf14, vf14, vf01        356
  vu.vf14.add(Mask::w, vu.vf14, vu.vf01.w());   bc = true;
  // nop                        |  nop                            357

  if (bc) { goto L21; }

  L32:
  // BRANCH!
  // b L23                      |  addw.w vf15, vf15, vf01        358
  vu.vf15.add(Mask::w, vu.vf15, vu.vf01.w());   bc = true;
  // nop                        |  nop                            359

  if (bc) { goto L23; }

  L33:
  // isubiu vi02, vi13, 0x363   |  addw.z vf22, vf00, vf00        360
  vu.vf22.add(Mask::z, vu.vf00, vu.vf00.w());   vu.vi02 = vu.vi13 - 0x363; /* 867 */
  // iaddiu vi13, vi13, 0x1e    |  addw.z vf23, vf00, vf00        361
  vu.vf23.add(Mask::z, vu.vf00, vu.vf00.w());   vu.vi13 = vu.vi13 + 0x1e; /* 30 */
  // BRANCH!
  // ibne vi00, vi02, L34       |  addw.z vf24, vf00, vf00        362
  vu.vf24.add(Mask::z, vu.vf00, vu.vf00.w());   bc = (vu.vi02 != 0);
  // nop                        |  addw.z vf25, vf00, vf00        363
  vu.vf25.add(Mask::z, vu.vf00, vu.vf00.w());
  if (bc) { goto L34; }

  // iaddiu vi13, vi00, 0x345   |  nop                            364
  vu.vi13 = 0x345; /* 837 */
  L34:
  // iaddi vi03, vi13, 0x7      |  nop                            365
  vu.vi03 = vu.vi13 + 7;
  // ilw.w vi01, 5(vi13)        |  nop                            366
  ilw_buffer(Mask::w, vu.vi01, vu.vi13 + 5);
  // isw.x vi03, 906(vi00)      |  nop                            367
  isw_buffer(Mask::x, vu.vi03, 906);
  // iaddi vi10, vi12, 0x9      |  subw.w vf18, vf00, vf00        368
  vu.vf18.sub(Mask::w, vu.vf00, vu.vf00.w());   vu.vi10 = vu.vi12 + 9;
  // lq.xyzw vf08, 0(vi13)      |  subw.w vf19, vf00, vf00        369
  vu.vf19.sub(Mask::w, vu.vf00, vu.vf00.w());   lq_buffer(Mask::xyzw, vu.vf08, vu.vi13);
  // lq.xyzw vf09, 1(vi13)      |  subw.w vf20, vf00, vf00        370
  vu.vf20.sub(Mask::w, vu.vf00, vu.vf00.w());   lq_buffer(Mask::xyzw, vu.vf09, vu.vi13 + 1);
  // lq.xyzw vf10, 2(vi13)      |  subw.w vf21, vf00, vf00        371
  vu.vf21.sub(Mask::w, vu.vf00, vu.vf00.w());   lq_buffer(Mask::xyzw, vu.vf10, vu.vi13 + 2);
  // lq.xyzw vf11, 3(vi13)      |  ftoi12.z vf22, vf22            372
  vu.vf22.ftoi12(Mask::z, vu.vf22);   lq_buffer(Mask::xyzw, vu.vf11, vu.vi13 + 3);
  // iadd vi02, vi01, vi01      |  ftoi12.z vf23, vf23            373
  vu.vf23.ftoi12(Mask::z, vu.vf23);   vu.vi02 = vu.vi01 + vu.vi01;
  // iadd vi01, vi01, vi02      |  sub.xyzw vf16, vf16, vf16      374
  vu.vf16.set_zero();         vu.vi01 = vu.vi01 + vu.vi02;
  // iaddi vi11, vi00, -0x2     |  nop                            375
  vu.vi11 = -2;
  // iadd vi14, vi10, vi01      |  ftoi12.z vf24, vf24            376
  vu.vf24.ftoi12(Mask::z, vu.vf24);   vu.vi14 = vu.vi10 + vu.vi01;
  // isw.w vi12, 906(vi00)      |  ftoi12.z vf25, vf25            377
  vu.vf25.ftoi12(Mask::z, vu.vf25);   isw_buffer(Mask::w, vu.vi12, 906);
  // iaddi vi14, vi14, 0x9      |  nop                            378
  vu.vi14 = vu.vi14 + 9;
  // lq.xy vf22, 0(vi10)        |  nop                            379
  lq_buffer(Mask::xy, vu.vf22, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  nop                            380
  lq_buffer(Mask::xyz, vu.vf16, vu.vi10 + 2);
  // nop                        |  nop                            381

  // nop                        |  nop                            382

  // nop                        |  mulaw.xyzw ACC, vf11, vf00     383
  vu.acc.mula(Mask::xyzw, vu.vf11, vu.vf00.w());
  // mtir vi02, vf22.x          |  maddax.xyzw ACC, vf08, vf16    384
  vu.acc.madda(Mask::xyzw, vu.vf08, vu.vf16.x());   vu.vi02 = vu.vf22.x_as_u16();
  // iaddi vi10, vi10, 0x3      |  madday.xyzw ACC, vf09, vf16    385
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf16.y());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf23, 0(vi10)        |  maddz.xyzw vf12, vf10, vf16    386
  vu.acc.madd(Mask::xyzw, vu.vf12, vu.vf10, vu.vf16.z());   lq_buffer(Mask::xy, vu.vf23, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  nop                            387
  lq_buffer(Mask::xyz, vu.vf16, vu.vi10 + 2);
  // iand vi06, vi02, vi11      |  nop                            388
  vu.vi06 = vu.vi02 & vu.vi11;
  // mfir.x vf22, vi06          |  nop                            389
  vu.vf22.mfir(Mask::x, vu.vi06);
  // nop                        |  nop                            390

  // nop                        |  nop                            391

  // nop                        |  nop                            392

  // nop                        |  itof12.xyz vf18, vf22          393
  vu.vf18.itof12(Mask::xyz, vu.vf22);
  // div Q, vf01.x, vf12.w      |  mul.xyzw vf26, vf12, vf05      394
  vu.vf26.mul(Mask::xyzw, vu.vf12, vu.vf05);   vu.Q = vu.vf01.x() / vu.vf12.w();
  // nop                        |  nop                            395

  // nop                        |  mulaw.xyzw ACC, vf11, vf00     396
  vu.acc.mula(Mask::xyzw, vu.vf11, vu.vf00.w());
  // mtir vi03, vf23.x          |  maddax.xyzw ACC, vf08, vf16    397
  vu.acc.madda(Mask::xyzw, vu.vf08, vu.vf16.x());   vu.vi03 = vu.vf23.x_as_u16();
  // iaddi vi10, vi10, 0x3      |  madday.xyzw ACC, vf09, vf16    398
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf16.y());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf24, 0(vi10)        |  maddz.xyzw vf13, vf10, vf16    399
  vu.acc.madd(Mask::xyzw, vu.vf13, vu.vf10, vu.vf16.z());   lq_buffer(Mask::xy, vu.vf24, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  nop                            400
  lq_buffer(Mask::xyz, vu.vf16, vu.vi10 + 2);
  // iand vi07, vi03, vi11      |  nop                            401
  vu.vi07 = vu.vi03 & vu.vi11;
  // mfir.x vf23, vi07          |  mul.xyz vf12, vf12, Q          402
  vu.vf12.mul(Mask::xyz, vu.vf12, vu.Q);   vu.vf23.mfir(Mask::x, vu.vi07);
  // fcset 0x0                  |  nop                            403
  cf = 0;
  // nop                        |  nop                            404

  // nop                        |  mul.xyz vf18, vf18, Q          405
  vu.vf18.mul(Mask::xyz, vu.vf18, vu.Q);
  // nop                        |  itof12.xyz vf19, vf23          406
  vu.vf19.itof12(Mask::xyz, vu.vf23);
  // div Q, vf01.x, vf13.w      |  mulaw.xyzw ACC, vf11, vf00     407
  vu.acc.mula(Mask::xyzw, vu.vf11, vu.vf00.w());   vu.Q = vu.vf01.x() / vu.vf13.w();
  // nop                        |  add.xyzw vf12, vf12, vf04      408
  vu.vf12.add(Mask::xyzw, vu.vf12, vu.vf04);
  // nop                        |  maddax.xyzw ACC, vf08, vf16    409
  vu.acc.madda(Mask::xyzw, vu.vf08, vu.vf16.x());
  // mtir vi04, vf24.x          |  madday.xyzw ACC, vf09, vf16    410
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf16.y());   vu.vi04 = vu.vf24.x_as_u16();
  // iaddi vi10, vi10, 0x3      |  maddz.xyzw vf14, vf10, vf16    411
  vu.acc.madd(Mask::xyzw, vu.vf14, vu.vf10, vu.vf16.z());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf25, 0(vi10)        |  miniz.w vf12, vf12, vf01       412
  vu.vf12.mini(Mask::w, vu.vf12, vu.vf01.z());   lq_buffer(Mask::xy, vu.vf25, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  mul.xyzw vf27, vf13, vf05      413
  vu.vf27.mul(Mask::xyzw, vu.vf13, vu.vf05);   lq_buffer(Mask::xyz, vu.vf16, vu.vi10 + 2);
  // iand vi08, vi04, vi11      |  nop                            414
  vu.vi08 = vu.vi04 & vu.vi11;
  // mfir.x vf24, vi08          |  mul.xyz vf13, vf13, Q          415
  vu.vf13.mul(Mask::xyz, vu.vf13, vu.Q);   vu.vf24.mfir(Mask::x, vu.vi08);
  // nop                        |  maxy.w vf12, vf12, vf01        416
  vu.vf12.max(Mask::w, vu.vf12, vu.vf01.y());
  // nop                        |  clipw.xyz vf26, vf26           417
 cf = clip(vu.vf26, vu.vf26.w(), cf);
  // nop                        |  mul.xyz vf19, vf19, Q          418
  vu.vf19.mul(Mask::xyz, vu.vf19, vu.Q);
  // BRANCH!
  // ibeq vi02, vi06, L35       |  itof12.xyz vf20, vf24          419
  vu.vf20.itof12(Mask::xyz, vu.vf24);   bc = (vu.vi02 == vu.vi06);
  // div Q, vf01.x, vf14.w      |  mulaw.xyzw ACC, vf11, vf00     420
  vu.acc.mula(Mask::xyzw, vu.vf11, vu.vf00.w());   vu.Q = vu.vf01.x() / vu.vf14.w();
  if (bc) { goto L35; }

  // nop                        |  addw.w vf12, vf12, vf01        421
  vu.vf12.add(Mask::w, vu.vf12, vu.vf01.w());
  L35:
  // nop                        |  add.xyzw vf13, vf13, vf04      422
  vu.vf13.add(Mask::xyzw, vu.vf13, vu.vf04);
  // nop                        |  maddax.xyzw ACC, vf08, vf16    423
  vu.acc.madda(Mask::xyzw, vu.vf08, vu.vf16.x());
  // mtir vi05, vf25.x          |  madday.xyzw ACC, vf09, vf16    424
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf16.y());   vu.vi05 = vu.vf25.x_as_u16();
  // iaddi vi10, vi10, 0x3      |  maddz.xyzw vf15, vf10, vf16    425
  vu.acc.madd(Mask::xyzw, vu.vf15, vu.vf10, vu.vf16.z());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf22, 0(vi10)        |  miniz.w vf13, vf13, vf01       426
  vu.vf13.mini(Mask::w, vu.vf13, vu.vf01.z());   lq_buffer(Mask::xy, vu.vf22, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  mul.xyzw vf28, vf14, vf05      427
  vu.vf28.mul(Mask::xyzw, vu.vf14, vu.vf05);   lq_buffer(Mask::xyz, vu.vf16, vu.vi10 + 2);
  // iand vi09, vi05, vi11      |  ftoi4.xyzw vf12, vf12          428
  vu.vf12.ftoi4(Mask::xyzw, vu.vf12);   vu.vi09 = vu.vi05 & vu.vi11;
  // mfir.x vf25, vi09          |  mul.xyz vf14, vf14, Q          429
  vu.vf14.mul(Mask::xyz, vu.vf14, vu.Q);   vu.vf25.mfir(Mask::x, vu.vi09);
  // sq.xyzw vf18, -12(vi10)    |  maxy.w vf13, vf13, vf01        430
  vu.vf13.max(Mask::w, vu.vf13, vu.vf01.y());   sq_buffer(Mask::xyzw, vu.vf18, vu.vi10 + -12);
  // BRANCH!
  // ibeq vi14, vi10, L46       |  clipw.xyz vf27, vf27           431
   cf = clip(vu.vf27, vu.vf27.w(), cf);   bc = (vu.vi14 == vu.vi10);
  // sq.xyzw vf12, -10(vi10)    |  mul.xyz vf20, vf20, Q          432
  vu.vf20.mul(Mask::xyz, vu.vf20, vu.Q);   sq_buffer(Mask::xyzw, vu.vf12, vu.vi10 + -10);
  if (bc) { goto L46; }

  // BRANCH!
  // ibeq vi03, vi07, L36       |  itof12.xyz vf21, vf25          433
  vu.vf21.itof12(Mask::xyz, vu.vf25);   bc = (vu.vi03 == vu.vi07);
  // div Q, vf01.x, vf15.w      |  mulaw.xyzw ACC, vf11, vf00     434
  vu.acc.mula(Mask::xyzw, vu.vf11, vu.vf00.w());   vu.Q = vu.vf01.x() / vu.vf15.w();
  if (bc) { goto L36; }

  // nop                        |  addw.w vf13, vf13, vf01        435
  vu.vf13.add(Mask::w, vu.vf13, vu.vf01.w());
  L36:
  // nop                        |  add.xyzw vf14, vf14, vf04      436
  vu.vf14.add(Mask::xyzw, vu.vf14, vu.vf04);
  // nop                        |  maddax.xyzw ACC, vf08, vf16    437
  vu.acc.madda(Mask::xyzw, vu.vf08, vu.vf16.x());
  // mtir vi02, vf22.x          |  madday.xyzw ACC, vf09, vf16    438
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf16.y());   vu.vi02 = vu.vf22.x_as_u16();
  // iaddi vi10, vi10, 0x3      |  maddz.xyzw vf12, vf10, vf16    439
  vu.acc.madd(Mask::xyzw, vu.vf12, vu.vf10, vu.vf16.z());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf23, 0(vi10)        |  miniz.w vf14, vf14, vf01       440
  vu.vf14.mini(Mask::w, vu.vf14, vu.vf01.z());   lq_buffer(Mask::xy, vu.vf23, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  mul.xyzw vf29, vf15, vf05      441
  vu.vf29.mul(Mask::xyzw, vu.vf15, vu.vf05);   lq_buffer(Mask::xyz, vu.vf16, vu.vi10 + 2);
  // iand vi06, vi02, vi11      |  ftoi4.xyzw vf13, vf13          442
  vu.vf13.ftoi4(Mask::xyzw, vu.vf13);   vu.vi06 = vu.vi02 & vu.vi11;
  // mfir.x vf22, vi06          |  mul.xyz vf15, vf15, Q          443
  vu.vf15.mul(Mask::xyz, vu.vf15, vu.Q);   vu.vf22.mfir(Mask::x, vu.vi06);
  // sq.xyzw vf19, -12(vi10)    |  maxy.w vf14, vf14, vf01        444
  vu.vf14.max(Mask::w, vu.vf14, vu.vf01.y());   sq_buffer(Mask::xyzw, vu.vf19, vu.vi10 + -12);
  // BRANCH!
  // ibeq vi14, vi10, L46       |  clipw.xyz vf28, vf28           445
  cf = clip(vu.vf28, vu.vf28.w(), cf);   bc = (vu.vi14 == vu.vi10);
  // sq.xyzw vf13, -10(vi10)    |  mul.xyz vf21, vf21, Q          446
  vu.vf21.mul(Mask::xyz, vu.vf21, vu.Q);   sq_buffer(Mask::xyzw, vu.vf13, vu.vi10 + -10);
  if (bc) { goto L46; }

  L37:
  // BRANCH!
  // ibeq vi04, vi08, L38       |  itof12.xyz vf18, vf22          447
  vu.vf18.itof12(Mask::xyz, vu.vf22);   bc = (vu.vi04 == vu.vi08);
  // div Q, vf01.x, vf12.w      |  mulaw.xyzw ACC, vf11, vf00     448
  vu.acc.mula(Mask::xyzw, vu.vf11, vu.vf00.w());   vu.Q = vu.vf01.x() / vu.vf12.w();
  if (bc) { goto L38; }

  // nop                        |  addw.w vf14, vf14, vf01        449
  vu.vf14.add(Mask::w, vu.vf14, vu.vf01.w());
  L38:
  // fcand vi01, 0x3ffff        |  add.xyzw vf15, vf15, vf04      450
  vu.vf15.add(Mask::xyzw, vu.vf15, vu.vf04);    vu.vi01 = cf & 0x3ffff;

  // BRANCH!
  // ibne vi00, vi01, L55       |  maddax.xyzw ACC, vf08, vf16    451
  vu.acc.madda(Mask::xyzw, vu.vf08, vu.vf16.x());   bc = (vu.vi01 != 0);
  // mtir vi03, vf23.x          |  madday.xyzw ACC, vf09, vf16    452
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf16.y());   vu.vi03 = vu.vf23.x_as_u16();
  if (bc) { goto L55; }

  L39:
  // iaddi vi10, vi10, 0x3      |  maddz.xyzw vf13, vf10, vf16    453
  vu.acc.madd(Mask::xyzw, vu.vf13, vu.vf10, vu.vf16.z());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf24, 0(vi10)        |  miniz.w vf15, vf15, vf01       454
  vu.vf15.mini(Mask::w, vu.vf15, vu.vf01.z());   lq_buffer(Mask::xy, vu.vf24, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  mul.xyzw vf26, vf12, vf05      455
  vu.vf26.mul(Mask::xyzw, vu.vf12, vu.vf05);   lq_buffer(Mask::xyz, vu.vf16, vu.vi10 + 2);
  // iand vi07, vi03, vi11      |  ftoi4.xyzw vf14, vf14          456
  vu.vf14.ftoi4(Mask::xyzw, vu.vf14);   vu.vi07 = vu.vi03 & vu.vi11;
  // mfir.x vf23, vi07          |  mul.xyz vf12, vf12, Q          457
  vu.vf12.mul(Mask::xyz, vu.vf12, vu.Q);   vu.vf23.mfir(Mask::x, vu.vi07);
  // sq.xyzw vf20, -12(vi10)    |  maxy.w vf15, vf15, vf01        458
  vu.vf15.max(Mask::w, vu.vf15, vu.vf01.y());   sq_buffer(Mask::xyzw, vu.vf20, vu.vi10 + -12);
  // BRANCH!
  // ibeq vi14, vi10, L46       |  clipw.xyz vf29, vf29           459
  cf = clip(vu.vf29, vu.vf29.w(), cf);   bc = (vu.vi14 == vu.vi10);
  // sq.xyzw vf14, -10(vi10)    |  mul.xyz vf18, vf18, Q          460
  vu.vf18.mul(Mask::xyz, vu.vf18, vu.Q);   sq_buffer(Mask::xyzw, vu.vf14, vu.vi10 + -10);
  if (bc) { goto L46; }

  // BRANCH!
  // ibeq vi05, vi09, L40       |  itof12.xyz vf19, vf23          461
  vu.vf19.itof12(Mask::xyz, vu.vf23);   bc = (vu.vi05 == vu.vi09);
  // div Q, vf01.x, vf13.w      |  mulaw.xyzw ACC, vf11, vf00     462
  vu.acc.mula(Mask::xyzw, vu.vf11, vu.vf00.w());   vu.Q = vu.vf01.x() / vu.vf13.w();
  if (bc) { goto L40; }

  // nop                        |  addw.w vf15, vf15, vf01        463
  vu.vf15.add(Mask::w, vu.vf15, vu.vf01.w());
  L40:
  // fcand vi01, 0x3ffff        |  add.xyzw vf12, vf12, vf04      464
  vu.vf12.add(Mask::xyzw, vu.vf12, vu.vf04);    vu.vi01 = cf & 0x3ffff;

  // BRANCH!
  // ibne vi00, vi01, L59       |  maddax.xyzw ACC, vf08, vf16    465
  vu.acc.madda(Mask::xyzw, vu.vf08, vu.vf16.x());   bc = (vu.vi01 != 0);
  // mtir vi04, vf24.x          |  madday.xyzw ACC, vf09, vf16    466
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf16.y());   vu.vi04 = vu.vf24.x_as_u16();
  if (bc) { goto L59; }

  L41:
  // iaddi vi10, vi10, 0x3      |  maddz.xyzw vf14, vf10, vf16    467
  vu.acc.madd(Mask::xyzw, vu.vf14, vu.vf10, vu.vf16.z());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf25, 0(vi10)        |  miniz.w vf12, vf12, vf01       468
  vu.vf12.mini(Mask::w, vu.vf12, vu.vf01.z());   lq_buffer(Mask::xy, vu.vf25, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  mul.xyzw vf27, vf13, vf05      469
  vu.vf27.mul(Mask::xyzw, vu.vf13, vu.vf05);   lq_buffer(Mask::xyz, vu.vf16, vu.vi10 + 2);
  // iand vi08, vi04, vi11      |  ftoi4.xyzw vf15, vf15          470
  vu.vf15.ftoi4(Mask::xyzw, vu.vf15);   vu.vi08 = vu.vi04 & vu.vi11;
  // mfir.x vf24, vi08          |  mul.xyz vf13, vf13, Q          471
  vu.vf13.mul(Mask::xyz, vu.vf13, vu.Q);   vu.vf24.mfir(Mask::x, vu.vi08);
  // sq.xyzw vf21, -12(vi10)    |  maxy.w vf12, vf12, vf01        472
  vu.vf12.max(Mask::w, vu.vf12, vu.vf01.y());   sq_buffer(Mask::xyzw, vu.vf21, vu.vi10 + -12);
  // BRANCH!
  // ibeq vi14, vi10, L46       |  clipw.xyz vf26, vf26           473
  cf = clip(vu.vf26, vu.vf26.w(), cf);   bc = (vu.vi14 == vu.vi10);
  // sq.xyzw vf15, -10(vi10)    |  mul.xyz vf19, vf19, Q          474
  vu.vf19.mul(Mask::xyz, vu.vf19, vu.Q);   sq_buffer(Mask::xyzw, vu.vf15, vu.vi10 + -10);
  if (bc) { goto L46; }

  // BRANCH!
  // ibeq vi02, vi06, L42       |  itof12.xyz vf20, vf24          475
  vu.vf20.itof12(Mask::xyz, vu.vf24);   bc = (vu.vi02 == vu.vi06);
  // div Q, vf01.x, vf14.w      |  mulaw.xyzw ACC, vf11, vf00     476
  vu.acc.mula(Mask::xyzw, vu.vf11, vu.vf00.w());   vu.Q = vu.vf01.x() / vu.vf14.w();
  if (bc) { goto L42; }

  // nop                        |  addw.w vf12, vf12, vf01        477
  vu.vf12.add(Mask::w, vu.vf12, vu.vf01.w());
  L42:
  // fcand vi01, 0x3ffff        |  add.xyzw vf13, vf13, vf04      478
  vu.vf13.add(Mask::xyzw, vu.vf13, vu.vf04);    vu.vi01 = cf & 0x3ffff;

  // BRANCH!
  // ibne vi00, vi01, L47       |  maddax.xyzw ACC, vf08, vf16    479
  vu.acc.madda(Mask::xyzw, vu.vf08, vu.vf16.x());   bc = (vu.vi01 != 0);
  // mtir vi05, vf25.x          |  madday.xyzw ACC, vf09, vf16    480
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf16.y());   vu.vi05 = vu.vf25.x_as_u16();
  if (bc) { goto L47; }

  L43:
  // iaddi vi10, vi10, 0x3      |  maddz.xyzw vf15, vf10, vf16    481
  vu.acc.madd(Mask::xyzw, vu.vf15, vu.vf10, vu.vf16.z());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf22, 0(vi10)        |  miniz.w vf13, vf13, vf01       482
  vu.vf13.mini(Mask::w, vu.vf13, vu.vf01.z());   lq_buffer(Mask::xy, vu.vf22, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  mul.xyzw vf28, vf14, vf05      483
  vu.vf28.mul(Mask::xyzw, vu.vf14, vu.vf05);   lq_buffer(Mask::xyz, vu.vf16, vu.vi10 + 2);
  // iand vi09, vi05, vi11      |  ftoi4.xyzw vf12, vf12          484
  vu.vf12.ftoi4(Mask::xyzw, vu.vf12);   vu.vi09 = vu.vi05 & vu.vi11;
  // mfir.x vf25, vi09          |  mul.xyz vf14, vf14, Q          485
  vu.vf14.mul(Mask::xyz, vu.vf14, vu.Q);   vu.vf25.mfir(Mask::x, vu.vi09);
  // sq.xyzw vf18, -12(vi10)    |  maxy.w vf13, vf13, vf01        486
  vu.vf13.max(Mask::w, vu.vf13, vu.vf01.y());   sq_buffer(Mask::xyzw, vu.vf18, vu.vi10 + -12);
  // BRANCH!
  // ibeq vi14, vi10, L46       |  clipw.xyz vf27, vf27           487
  cf = clip(vu.vf27, vu.vf27.w(), cf);   bc = (vu.vi14 == vu.vi10);
  // sq.xyzw vf12, -10(vi10)    |  mul.xyz vf20, vf20, Q          488
  vu.vf20.mul(Mask::xyz, vu.vf20, vu.Q);   sq_buffer(Mask::xyzw, vu.vf12, vu.vi10 + -10);
  if (bc) { goto L46; }

  // BRANCH!
  // ibeq vi03, vi07, L44       |  itof12.xyz vf21, vf25          489
  vu.vf21.itof12(Mask::xyz, vu.vf25);   bc = (vu.vi03 == vu.vi07);
  // div Q, vf01.x, vf15.w      |  mulaw.xyzw ACC, vf11, vf00     490
  vu.acc.mula(Mask::xyzw, vu.vf11, vu.vf00.w());   vu.Q = vu.vf01.x() / vu.vf15.w();
  if (bc) { goto L44; }

  // nop                        |  addw.w vf13, vf13, vf01        491
  vu.vf13.add(Mask::w, vu.vf13, vu.vf01.w());
  L44:
  // fcand vi01, 0x3ffff        |  add.xyzw vf14, vf14, vf04      492
  vu.vf14.add(Mask::xyzw, vu.vf14, vu.vf04);    vu.vi01 = cf & 0x3ffff;

  // BRANCH!
  // ibne vi00, vi01, L51       |  maddax.xyzw ACC, vf08, vf16    493
  vu.acc.madda(Mask::xyzw, vu.vf08, vu.vf16.x());   bc = (vu.vi01 != 0);
  // mtir vi02, vf22.x          |  madday.xyzw ACC, vf09, vf16    494
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf16.y());   vu.vi02 = vu.vf22.x_as_u16();
  if (bc) { goto L51; }

  L45:
  // iaddi vi10, vi10, 0x3      |  maddz.xyzw vf12, vf10, vf16    495
  vu.acc.madd(Mask::xyzw, vu.vf12, vu.vf10, vu.vf16.z());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf23, 0(vi10)        |  miniz.w vf14, vf14, vf01       496
  vu.vf14.mini(Mask::w, vu.vf14, vu.vf01.z());   lq_buffer(Mask::xy, vu.vf23, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  mul.xyzw vf29, vf15, vf05      497
  vu.vf29.mul(Mask::xyzw, vu.vf15, vu.vf05);   lq_buffer(Mask::xyz, vu.vf16, vu.vi10 + 2);
  // iand vi06, vi02, vi11      |  ftoi4.xyzw vf13, vf13          498
  vu.vf13.ftoi4(Mask::xyzw, vu.vf13);   vu.vi06 = vu.vi02 & vu.vi11;
  // mfir.x vf22, vi06          |  mul.xyz vf15, vf15, Q          499
  vu.vf15.mul(Mask::xyz, vu.vf15, vu.Q);   vu.vf22.mfir(Mask::x, vu.vi06);
  // sq.xyzw vf19, -12(vi10)    |  maxy.w vf14, vf14, vf01        500
  vu.vf14.max(Mask::w, vu.vf14, vu.vf01.y());   sq_buffer(Mask::xyzw, vu.vf19, vu.vi10 + -12);
  // BRANCH!
  // ibne vi14, vi10, L37       |  clipw.xyz vf28, vf28           501
  cf = clip(vu.vf28, vu.vf28.w(), cf);   bc = (vu.vi14 != vu.vi10);
  // sq.xyzw vf13, -10(vi10)    |  mul.xyz vf21, vf21, Q          502
  vu.vf21.mul(Mask::xyz, vu.vf21, vu.Q);   sq_buffer(Mask::xyzw, vu.vf13, vu.vi10 + -10);
  if (bc) { goto L37; }

  L46:
  // BRANCH!
  // b L82                      |  nop                            503
  bc = true;
  // ilw.w vi12, 906(vi00)      |  nop                            504
  ilw_buffer(Mask::w, vu.vi12, 906);
  if (bc) { goto L82; }

  L47:
  // BRANCH!
  // ibne vi02, vi06, L43       |  nop                            505
  bc = (vu.vi02 != vu.vi06);
  // sq.xyzw vf23, 998(vi00)    |  addw.w vf12, vf12, vf01        506
  vu.vf12.add(Mask::w, vu.vf12, vu.vf01.w());   sq_buffer(Mask::xyzw, vu.vf23, 998);
  if (bc) { goto L43; }

  // sq.xyzw vf24, 999(vi00)    |  mul.xyzw vf23, vf28, vf07      507
  vu.vf23.mul(Mask::xyzw, vu.vf28, vu.vf07);   sq_buffer(Mask::xyzw, vu.vf24, 999);
  // sq.xyzw vf25, 1000(vi00)   |  mul.xyzw vf24, vf29, vf07      508
  vu.vf24.mul(Mask::xyzw, vu.vf29, vu.vf07);   sq_buffer(Mask::xyzw, vu.vf25, 1000);
  // isw.x vi01, 1001(vi00)     |  mul.xyzw vf25, vf26, vf07      509
  vu.vf25.mul(Mask::xyzw, vu.vf26, vu.vf07);   isw_buffer(Mask::x, vu.vi01, 1001);
  // isw.y vi02, 1001(vi00)     |  nop                            510
  isw_buffer(Mask::y, vu.vi02, 1001);
  // isw.z vi03, 1001(vi00)     |  clipw.xyz vf23, vf23           511
  // TODO: check clipping pipeline here.
  cf0 = clip(vu.vf23, vu.vf23.w(), cf);   isw_buffer(Mask::z, vu.vi03, 1001);
  // isw.w vi04, 1001(vi00)     |  clipw.xyz vf24, vf24           512
  cf1 = clip(vu.vf24, vu.vf24.w(), cf0);   isw_buffer(Mask::w, vu.vi04, 1001);
  // mfir.x vf31, vi05          |  clipw.xyz vf25, vf25           513
  cf2 = clip(vu.vf25, vu.vf25.w(), cf1);   vu.vf31.mfir(Mask::x, vu.vi05);
  // iaddiu vi04, vi00, 0x3f    |  nop                            514
  vu.vi04 = 0x3f; /* 63 */
  // fcget vi01                 |  nop                            515
  vu.vi01 = cf0;
  // fcget vi02                 |  nop                            516
  vu.vi02 = cf1;
  // fcget vi03                 |  nop                            517
  vu.vi03 = cf2;
  cf = cf2;
  // iand vi01, vi01, vi04      |  clipw.xyz vf28, vf28           518
  // TODO, is this right?
  cf = clip(vu.vf28, vu.vf28.w(), cf);   vu.vi01 = vu.vi01 & vu.vi04;
  // iand vi01, vi01, vi02      |  clipw.xyz vf29, vf29           519
  cf = clip(vu.vf29, vu.vf29.w(), cf);   vu.vi01 = vu.vi01 & vu.vi02;
  // iand vi01, vi01, vi03      |  clipw.xyz vf26, vf26           520
  cf = clip(vu.vf26, vu.vf26.w(), cf);   vu.vi01 = vu.vi01 & vu.vi03;
  // mfir.y vf31, vi06          |  nop                            521
  vu.vf31.mfir(Mask::y, vu.vi06);
  // BRANCH!
  // ibeq vi00, vi01, L49       |  nop                            522
  bc = (vu.vi01 == 0);
  // mfir.z vf31, vi07          |  nop                            523
  vu.vf31.mfir(Mask::z, vu.vi07);
  if (!clipping_hack && bc) { goto L49; }

  L48:
  // div Q, vf01.x, vf14.w      |  nop                            524
  vu.Q = vu.vf01.x() / vu.vf14.w();
  // lq.xyzw vf23, 998(vi00)    |  nop                            525
  lq_buffer(Mask::xyzw, vu.vf23, 998);
  // lq.xyzw vf24, 999(vi00)    |  nop                            526
  lq_buffer(Mask::xyzw, vu.vf24, 999);
  // lq.xyzw vf25, 1000(vi00)   |  nop                            527
  lq_buffer(Mask::xyzw, vu.vf25, 1000);
  // ilw.x vi01, 1001(vi00)     |  nop                            528
  ilw_buffer(Mask::x, vu.vi01, 1001);
  // ilw.y vi02, 1001(vi00)     |  nop                            529
  ilw_buffer(Mask::y, vu.vi02, 1001);
  // ilw.z vi03, 1001(vi00)     |  nop                            530
  ilw_buffer(Mask::z, vu.vi03, 1001);
  // BRANCH!
  // b L43                      |  nop                            531
  bc = true;
  // ilw.w vi04, 1001(vi00)     |  nop                            532
  ilw_buffer(Mask::w, vu.vi04, 1001);
  if (bc) { goto L43; }

  L49:
  // mfir.w vf31, vi08          |  nop                            533
  vu.vf31.mfir(Mask::w, vu.vi08);
  // mfir.x vf30, vi09          |  nop                            534
  vu.vf30.mfir(Mask::x, vu.vi09);
  // mfir.y vf30, vi10          |  nop                            535
  vu.vf30.mfir(Mask::y, vu.vi10);
  // mfir.z vf30, vi11          |  nop                            536
  vu.vf30.mfir(Mask::z, vu.vi11);
  // mfir.w vf30, vi12          |  nop                            537
  vu.vf30.mfir(Mask::w, vu.vi12);
  // sq.xyzw vf12, 1004(vi00)   |  nop                            538
  sq_buffer(Mask::xyzw, vu.vf12, 1004);
  // lq.xyzw vf12, -15(vi10)    |  nop                            539
  lq_buffer(Mask::xyzw, vu.vf12, vu.vi10 + -15);
  // sq.xyzw vf13, 1005(vi00)   |  nop                            540
  sq_buffer(Mask::xyzw, vu.vf13, 1005);
  // lq.xyzw vf13, -12(vi10)    |  nop                            541
  lq_buffer(Mask::xyzw, vu.vf13, vu.vi10 + -12);
  // sq.xyzw vf14, 1006(vi00)   |  nop                            542
  sq_buffer(Mask::xyzw, vu.vf14, 1006);
  // div Q, vf00.w, vf12.z      |  nop                            543
  vu.Q = vu.vf00.w() / vu.vf12.z();
  // sq.xyzw vf31, 1002(vi00)   |  nop                            544
  sq_buffer(Mask::xyzw, vu.vf31, 1002);
  // sq.xyzw vf30, 1003(vi00)   |  nop                            545
  sq_buffer(Mask::xyzw, vu.vf30, 1003);
  // sq.xyzw vf15, 1007(vi00)   |  nop                            546
  sq_buffer(Mask::xyzw, vu.vf15, 1007);
  // sq.xyzw vf16, 1008(vi00)   |  nop                            547
  sq_buffer(Mask::xyzw, vu.vf16, 1008);
  // lq.xyzw vf03, 4(vi13)      |  nop                            548
  lq_buffer(Mask::xyzw, vu.vf03, vu.vi13 + 4);
  // lq.xyzw vf15, -14(vi10)    |  sub.xw vf31, vf00, vf00        549
  vu.vf31.sub(Mask::xw, vu.vf00, vu.vf00);   lq_buffer(Mask::xyzw, vu.vf15, vu.vi10 + -14);
  // div Q, vf00.w, vf13.z      |  nop                            550
  vu.Q = vu.vf00.w() / vu.vf13.z();
  // lq.xyzw vf16, -11(vi10)    |  mul.xyz vf12, vf12, Q          551
  vu.vf12.mul(Mask::xyz, vu.vf12, vu.Q);   lq_buffer(Mask::xyzw, vu.vf16, vu.vi10 + -11);
  // lq.xyzw vf17, -8(vi10)     |  addx.y vf31, vf00, vf03        552
  vu.vf31.add(Mask::y, vu.vf00, vu.vf03.x());   lq_buffer(Mask::xyzw, vu.vf17, vu.vi10 + -8);
  // nop                        |  itof0.xyzw vf15, vf15          553
  vu.vf15.itof0(Mask::xyzw, vu.vf15);
  // sq.xyzw vf28, 989(vi00)    |  nop                            554
  sq_buffer(Mask::xyzw, vu.vf28, 989);
  // sq.xyzw vf12, 991(vi00)    |  itof0.xyzw vf16, vf16          555
  vu.vf16.itof0(Mask::xyzw, vu.vf16);   sq_buffer(Mask::xyzw, vu.vf12, 991);
  // sq.xyzw vf29, 992(vi00)    |  itof0.xyzw vf17, vf17          556
  vu.vf17.itof0(Mask::xyzw, vu.vf17);   sq_buffer(Mask::xyzw, vu.vf29, 992);
  // div Q, vf00.w, vf18.z      |  nop                            557
  vu.Q = vu.vf00.w() / vu.vf18.z();
  // nop                        |  mul.xyz vf13, vf13, Q          558
  vu.vf13.mul(Mask::xyz, vu.vf13, vu.Q);
  // sq.xyzw vf26, 995(vi00)    |  nop                            559
  sq_buffer(Mask::xyzw, vu.vf26, 995);
  // move.z vf31, vf03          |  nop                            560
  vu.vf31.move(Mask::z, vu.vf03);
  // sq.xyzw vf15, 990(vi00)    |  nop                            561
  sq_buffer(Mask::xyzw, vu.vf15, 990);
  // sq.xyzw vf16, 993(vi00)    |  nop                            562
  sq_buffer(Mask::xyzw, vu.vf16, 993);
  // sq.xyzw vf17, 996(vi00)    |  nop                            563
  sq_buffer(Mask::xyzw, vu.vf17, 996);
  // sq.xyzw vf13, 994(vi00)    |  nop                            564
  sq_buffer(Mask::xyzw, vu.vf13, 994);
  // sq.xyzw vf31, 961(vi00)    |  mul.xyz vf14, vf18, Q          565
  vu.vf14.mul(Mask::xyz, vu.vf18, vu.Q);   sq_buffer(Mask::xyzw, vu.vf31, 961);
  // nop                        |  nop                            566

  // nop                        |  nop                            567

  // BRANCH!
  // bal vi15, L66              |  nop                            568
  ASSERT(false);
  // sq.xyzw vf14, 997(vi00)    |  nop                            569
  sq_buffer(Mask::xyzw, vu.vf14, 997);
  if (bc) { goto L66; }

  // BRANCH!
  // ibeq vi00, vi05, L50       |  nop                            570
  bc = (vu.vi05 == 0);
  // nop                        |  nop                            571

  if (bc) { goto L50; }

  // BRANCH!
  // bal vi15, L63              |  nop                            572
  ASSERT(false);
  // nop                        |  nop                            573

  if (bc) { goto L63; }

  L50:
  // ilw.x vi05, 1002(vi00)     |  nop                            574
  ilw_buffer(Mask::x, vu.vi05, 1002);
  // ilw.y vi06, 1002(vi00)     |  nop                            575
  ilw_buffer(Mask::y, vu.vi06, 1002);
  // ilw.z vi07, 1002(vi00)     |  nop                            576
  ilw_buffer(Mask::z, vu.vi07, 1002);
  // ilw.w vi08, 1002(vi00)     |  nop                            577
  ilw_buffer(Mask::w, vu.vi08, 1002);
  // ilw.x vi09, 1003(vi00)     |  nop                            578
  ilw_buffer(Mask::x, vu.vi09, 1003);
  // ilw.y vi10, 1003(vi00)     |  nop                            579
  ilw_buffer(Mask::y, vu.vi10, 1003);
  // ilw.z vi11, 1003(vi00)     |  nop                            580
  ilw_buffer(Mask::z, vu.vi11, 1003);
  // ilw.w vi12, 1003(vi00)     |  nop                            581
  ilw_buffer(Mask::w, vu.vi12, 1003);
  // lq.xyzw vf12, 1004(vi00)   |  nop                            582
  lq_buffer(Mask::xyzw, vu.vf12, 1004);
  // lq.xyzw vf13, 1005(vi00)   |  nop                            583
  lq_buffer(Mask::xyzw, vu.vf13, 1005);
  // lq.xyzw vf14, 1006(vi00)   |  nop                            584
  lq_buffer(Mask::xyzw, vu.vf14, 1006);
  // lq.xyzw vf15, 1007(vi00)   |  nop                            585
  lq_buffer(Mask::xyzw, vu.vf15, 1007);
  // BRANCH!
  // b L48                      |  nop                            586
  bc = true;
  // lq.xyzw vf16, 1008(vi00)   |  nop                            587
  lq_buffer(Mask::xyzw, vu.vf16, 1008);
  if (bc) { goto L48; }

  L51:
  // BRANCH!
  // ibne vi03, vi07, L45       |  nop                            588
  bc = (vu.vi03 != vu.vi07);
  // sq.xyzw vf23, 998(vi00)    |  addw.w vf13, vf13, vf01        589
  vu.vf13.add(Mask::w, vu.vf13, vu.vf01.w());   sq_buffer(Mask::xyzw, vu.vf23, 998);
  if (bc) { goto L45; }

  // sq.xyzw vf24, 999(vi00)    |  mul.xyzw vf23, vf29, vf07      590
  vu.vf23.mul(Mask::xyzw, vu.vf29, vu.vf07);   sq_buffer(Mask::xyzw, vu.vf24, 999);
  // sq.xyzw vf25, 1000(vi00)   |  mul.xyzw vf24, vf26, vf07      591
  vu.vf24.mul(Mask::xyzw, vu.vf26, vu.vf07);   sq_buffer(Mask::xyzw, vu.vf25, 1000);
  // isw.x vi01, 1001(vi00)     |  mul.xyzw vf25, vf27, vf07      592
  vu.vf25.mul(Mask::xyzw, vu.vf27, vu.vf07);   isw_buffer(Mask::x, vu.vi01, 1001);
  // isw.y vi02, 1001(vi00)     |  nop                            593
  isw_buffer(Mask::y, vu.vi02, 1001);
  // TODO more clipping pipeline?
  // isw.z vi03, 1001(vi00)     |  clipw.xyz vf23, vf23           594
  cf0 = clip(vu.vf23, vu.vf23.w(), cf);   isw_buffer(Mask::z, vu.vi03, 1001);
  // isw.w vi04, 1001(vi00)     |  clipw.xyz vf24, vf24           595
  cf1 = clip(vu.vf24, vu.vf24.w(), cf0);   isw_buffer(Mask::w, vu.vi04, 1001);
  // mfir.x vf31, vi05          |  clipw.xyz vf25, vf25           596
  cf2 = clip(vu.vf25, vu.vf25.w(), cf1);   vu.vf31.mfir(Mask::x, vu.vi05);
  // iaddiu vi04, vi00, 0x3f    |  nop                            597
  vu.vi04 = 0x3f; /* 63 */
  // fcget vi01                 |  nop                            598
  vu.vi01 = cf0;
  // fcget vi02                 |  nop                            599
  vu.vi02 = cf1;
  // fcget vi03                 |  nop                            600
  vu.vi03 = cf2;
  cf = cf2;
  // iand vi01, vi01, vi04      |  clipw.xyz vf29, vf29           601
  cf = clip(vu.vf29, vu.vf29.w(), cf);   vu.vi01 = vu.vi01 & vu.vi04;
  // iand vi01, vi01, vi02      |  clipw.xyz vf26, vf26           602
  cf = clip(vu.vf26, vu.vf26.w(), cf);   vu.vi01 = vu.vi01 & vu.vi02;
  // iand vi01, vi01, vi03      |  clipw.xyz vf27, vf27           603
  cf = clip(vu.vf27, vu.vf27.w(), cf);   vu.vi01 = vu.vi01 & vu.vi03;
  // mfir.y vf31, vi06          |  nop                            604
  vu.vf31.mfir(Mask::y, vu.vi06);
  // BRANCH!
  // ibeq vi00, vi01, L53       |  nop                            605
  bc = (vu.vi01 == 0);
  // mfir.z vf31, vi07          |  nop                            606
  vu.vf31.mfir(Mask::z, vu.vi07);
  if (!clipping_hack && bc) { goto L53; }

  L52:
  // div Q, vf01.x, vf15.w      |  nop                            607
  vu.Q = vu.vf01.x() / vu.vf15.w();
  // lq.xyzw vf23, 998(vi00)    |  nop                            608
  lq_buffer(Mask::xyzw, vu.vf23, 998);
  // lq.xyzw vf24, 999(vi00)    |  nop                            609
  lq_buffer(Mask::xyzw, vu.vf24, 999);
  // lq.xyzw vf25, 1000(vi00)   |  nop                            610
  lq_buffer(Mask::xyzw, vu.vf25, 1000);
  // ilw.x vi01, 1001(vi00)     |  nop                            611
  ilw_buffer(Mask::x, vu.vi01, 1001);
  // ilw.y vi02, 1001(vi00)     |  nop                            612
  ilw_buffer(Mask::y, vu.vi02, 1001);
  // ilw.z vi03, 1001(vi00)     |  nop                            613
  ilw_buffer(Mask::z, vu.vi03, 1001);
  // BRANCH!
  // b L45                      |  nop                            614
  bc = true;
  // ilw.w vi04, 1001(vi00)     |  nop                            615
  ilw_buffer(Mask::w, vu.vi04, 1001);
  if (bc) { goto L45; }

  L53:
  // mfir.w vf31, vi08          |  nop                            616
  vu.vf31.mfir(Mask::w, vu.vi08);
  // mfir.x vf30, vi09          |  nop                            617
  vu.vf30.mfir(Mask::x, vu.vi09);
  // mfir.y vf30, vi10          |  nop                            618
  vu.vf30.mfir(Mask::y, vu.vi10);
  // mfir.z vf30, vi11          |  nop                            619
  vu.vf30.mfir(Mask::z, vu.vi11);
  // mfir.w vf30, vi12          |  nop                            620
  vu.vf30.mfir(Mask::w, vu.vi12);
  // sq.xyzw vf12, 1004(vi00)   |  nop                            621
  sq_buffer(Mask::xyzw, vu.vf12, 1004);
  // lq.xyzw vf12, -15(vi10)    |  nop                            622
  lq_buffer(Mask::xyzw, vu.vf12, vu.vi10 + -15);
  // sq.xyzw vf13, 1005(vi00)   |  nop                            623
  sq_buffer(Mask::xyzw, vu.vf13, 1005);
  // lq.xyzw vf13, -12(vi10)    |  nop                            624
  lq_buffer(Mask::xyzw, vu.vf13, vu.vi10 + -12);
  // sq.xyzw vf14, 1006(vi00)   |  nop                            625
  sq_buffer(Mask::xyzw, vu.vf14, 1006);
  // div Q, vf00.w, vf12.z      |  nop                            626
  vu.Q = vu.vf00.w() / vu.vf12.z();
  // sq.xyzw vf31, 1002(vi00)   |  nop                            627
  sq_buffer(Mask::xyzw, vu.vf31, 1002);
  // sq.xyzw vf30, 1003(vi00)   |  nop                            628
  sq_buffer(Mask::xyzw, vu.vf30, 1003);
  // sq.xyzw vf15, 1007(vi00)   |  nop                            629
  sq_buffer(Mask::xyzw, vu.vf15, 1007);
  // sq.xyzw vf16, 1008(vi00)   |  nop                            630
  sq_buffer(Mask::xyzw, vu.vf16, 1008);
  // lq.xyzw vf03, 4(vi13)      |  nop                            631
  lq_buffer(Mask::xyzw, vu.vf03, vu.vi13 + 4);
  // lq.xyzw vf15, -14(vi10)    |  sub.xw vf31, vf00, vf00        632
  vu.vf31.sub(Mask::xw, vu.vf00, vu.vf00);   lq_buffer(Mask::xyzw, vu.vf15, vu.vi10 + -14);
  // div Q, vf00.w, vf13.z      |  nop                            633
  vu.Q = vu.vf00.w() / vu.vf13.z();
  // lq.xyzw vf16, -11(vi10)    |  mul.xyz vf12, vf12, Q          634
  vu.vf12.mul(Mask::xyz, vu.vf12, vu.Q);   lq_buffer(Mask::xyzw, vu.vf16, vu.vi10 + -11);
  // lq.xyzw vf17, -8(vi10)     |  addx.y vf31, vf00, vf03        635
  vu.vf31.add(Mask::y, vu.vf00, vu.vf03.x());   lq_buffer(Mask::xyzw, vu.vf17, vu.vi10 + -8);
  // nop                        |  itof0.xyzw vf15, vf15          636
  vu.vf15.itof0(Mask::xyzw, vu.vf15);
  // sq.xyzw vf29, 989(vi00)    |  nop                            637
  sq_buffer(Mask::xyzw, vu.vf29, 989);
  // sq.xyzw vf12, 991(vi00)    |  itof0.xyzw vf16, vf16          638
  vu.vf16.itof0(Mask::xyzw, vu.vf16);   sq_buffer(Mask::xyzw, vu.vf12, 991);
  // sq.xyzw vf26, 992(vi00)    |  itof0.xyzw vf17, vf17          639
  vu.vf17.itof0(Mask::xyzw, vu.vf17);   sq_buffer(Mask::xyzw, vu.vf26, 992);
  // div Q, vf00.w, vf19.z      |  nop                            640
  vu.Q = vu.vf00.w() / vu.vf19.z();
  // nop                        |  mul.xyz vf13, vf13, Q          641
  vu.vf13.mul(Mask::xyz, vu.vf13, vu.Q);
  // sq.xyzw vf27, 995(vi00)    |  nop                            642
  sq_buffer(Mask::xyzw, vu.vf27, 995);
  // move.z vf31, vf03          |  nop                            643
  vu.vf31.move(Mask::z, vu.vf03);
  // sq.xyzw vf15, 990(vi00)    |  nop                            644
  sq_buffer(Mask::xyzw, vu.vf15, 990);
  // sq.xyzw vf16, 993(vi00)    |  nop                            645
  sq_buffer(Mask::xyzw, vu.vf16, 993);
  // sq.xyzw vf17, 996(vi00)    |  nop                            646
  sq_buffer(Mask::xyzw, vu.vf17, 996);
  // sq.xyzw vf13, 994(vi00)    |  nop                            647
  sq_buffer(Mask::xyzw, vu.vf13, 994);
  // sq.xyzw vf31, 961(vi00)    |  mul.xyz vf14, vf19, Q          648
  vu.vf14.mul(Mask::xyz, vu.vf19, vu.Q);   sq_buffer(Mask::xyzw, vu.vf31, 961);
  // nop                        |  nop                            649

  // nop                        |  nop                            650

  // BRANCH!
  // bal vi15, L66              |  nop                            651
  ASSERT(false);
  // sq.xyzw vf14, 997(vi00)    |  nop                            652
  sq_buffer(Mask::xyzw, vu.vf14, 997);
  if (bc) { goto L66; }

  // BRANCH!
  // ibeq vi00, vi05, L54       |  nop                            653
  bc = (vu.vi05 == 0);
  // nop                        |  nop                            654

  if (bc) { goto L54; }

  // BRANCH!
  // bal vi15, L63              |  nop                            655
  ASSERT(false);
  // nop                        |  nop                            656

  if (bc) { goto L63; }

  L54:
  // ilw.x vi05, 1002(vi00)     |  nop                            657
  ilw_buffer(Mask::x, vu.vi05, 1002);
  // ilw.y vi06, 1002(vi00)     |  nop                            658
  ilw_buffer(Mask::y, vu.vi06, 1002);
  // ilw.z vi07, 1002(vi00)     |  nop                            659
  ilw_buffer(Mask::z, vu.vi07, 1002);
  // ilw.w vi08, 1002(vi00)     |  nop                            660
  ilw_buffer(Mask::w, vu.vi08, 1002);
  // ilw.x vi09, 1003(vi00)     |  nop                            661
  ilw_buffer(Mask::x, vu.vi09, 1003);
  // ilw.y vi10, 1003(vi00)     |  nop                            662
  ilw_buffer(Mask::y, vu.vi10, 1003);
  // ilw.z vi11, 1003(vi00)     |  nop                            663
  ilw_buffer(Mask::z, vu.vi11, 1003);
  // ilw.w vi12, 1003(vi00)     |  nop                            664
  ilw_buffer(Mask::w, vu.vi12, 1003);
  // lq.xyzw vf12, 1004(vi00)   |  nop                            665
  lq_buffer(Mask::xyzw, vu.vf12, 1004);
  // lq.xyzw vf13, 1005(vi00)   |  nop                            666
  lq_buffer(Mask::xyzw, vu.vf13, 1005);
  // lq.xyzw vf14, 1006(vi00)   |  nop                            667
  lq_buffer(Mask::xyzw, vu.vf14, 1006);
  // lq.xyzw vf15, 1007(vi00)   |  nop                            668
  lq_buffer(Mask::xyzw, vu.vf15, 1007);
  // BRANCH!
  // b L52                      |  nop                            669
  bc = true;
  // lq.xyzw vf16, 1008(vi00)   |  nop                            670
  lq_buffer(Mask::xyzw, vu.vf16, 1008);
  if (bc) { goto L52; }

  L55:
  // BRANCH!
  // ibne vi04, vi08, L39       |  nop                            671
  bc = (vu.vi04 != vu.vi08);
  // sq.xyzw vf23, 998(vi00)    |  addw.w vf14, vf14, vf01        672
  vu.vf14.add(Mask::w, vu.vf14, vu.vf01.w());   sq_buffer(Mask::xyzw, vu.vf23, 998);
  if (bc) { goto L39; }

  // sq.xyzw vf24, 999(vi00)    |  mul.xyzw vf23, vf26, vf07      673
  vu.vf23.mul(Mask::xyzw, vu.vf26, vu.vf07);   sq_buffer(Mask::xyzw, vu.vf24, 999);
  // sq.xyzw vf25, 1000(vi00)   |  mul.xyzw vf24, vf27, vf07      674
  vu.vf24.mul(Mask::xyzw, vu.vf27, vu.vf07);   sq_buffer(Mask::xyzw, vu.vf25, 1000);
  // isw.x vi01, 1001(vi00)     |  mul.xyzw vf25, vf28, vf07      675
  vu.vf25.mul(Mask::xyzw, vu.vf28, vu.vf07);   isw_buffer(Mask::x, vu.vi01, 1001);
  // isw.y vi02, 1001(vi00)     |  nop                            676
  isw_buffer(Mask::y, vu.vi02, 1001);
  // TODO more clipping?
  // isw.z vi03, 1001(vi00)     |  clipw.xyz vf23, vf23           677
  cf0 = clip(vu.vf23, vu.vf23.w(), cf);   isw_buffer(Mask::z, vu.vi03, 1001);
  // isw.w vi04, 1001(vi00)     |  clipw.xyz vf24, vf24           678
  cf1 = clip(vu.vf24, vu.vf24.w(), cf0);   isw_buffer(Mask::w, vu.vi04, 1001);
  // mfir.x vf31, vi05          |  clipw.xyz vf25, vf25           679
  cf2 = clip(vu.vf25, vu.vf25.w(), cf1);   vu.vf31.mfir(Mask::x, vu.vi05);
  // iaddiu vi04, vi00, 0x3f    |  nop                            680
  vu.vi04 = 0x3f; /* 63 */
  // fcget vi01                 |  nop                            681
  vu.vi01 = cf0;
  // fcget vi02                 |  nop                            682
  vu.vi02 = cf1;
  // fcget vi03                 |  nop                            683
  vu.vi03 = cf2;
  cf = cf2;
  // iand vi01, vi01, vi04      |  clipw.xyz vf26, vf26           684
  cf = clip(vu.vf26, vu.vf26.w(), cf);   vu.vi01 = vu.vi01 & vu.vi04;
  // iand vi01, vi01, vi02      |  clipw.xyz vf27, vf27           685
  cf = clip(vu.vf27, vu.vf27.w(), cf);   vu.vi01 = vu.vi01 & vu.vi02;
  // iand vi01, vi01, vi03      |  clipw.xyz vf28, vf28           686
  cf = clip(vu.vf28, vu.vf28.w(), cf);   vu.vi01 = vu.vi01 & vu.vi03;
  // mfir.y vf31, vi06          |  nop                            687
  vu.vf31.mfir(Mask::y, vu.vi06);
  // BRANCH!
  // ibeq vi00, vi01, L57       |  nop                            688
  bc = (vu.vi01 == 0);
  // mfir.z vf31, vi07          |  nop                            689
  vu.vf31.mfir(Mask::z, vu.vi07);
  if (!clipping_hack && bc) { goto L57; }

  L56:
  // div Q, vf01.x, vf12.w      |  nop                            690
  vu.Q = vu.vf01.x() / vu.vf12.w();
  // lq.xyzw vf23, 998(vi00)    |  nop                            691
  lq_buffer(Mask::xyzw, vu.vf23, 998);
  // lq.xyzw vf24, 999(vi00)    |  nop                            692
  lq_buffer(Mask::xyzw, vu.vf24, 999);
  // lq.xyzw vf25, 1000(vi00)   |  nop                            693
  lq_buffer(Mask::xyzw, vu.vf25, 1000);
  // ilw.x vi01, 1001(vi00)     |  nop                            694
  ilw_buffer(Mask::x, vu.vi01, 1001);
  // ilw.y vi02, 1001(vi00)     |  nop                            695
  ilw_buffer(Mask::y, vu.vi02, 1001);
  // ilw.z vi03, 1001(vi00)     |  nop                            696
  ilw_buffer(Mask::z, vu.vi03, 1001);
  // BRANCH!
  // b L39                      |  nop                            697
  bc = true;
  // ilw.w vi04, 1001(vi00)     |  nop                            698
  ilw_buffer(Mask::w, vu.vi04, 1001);
  if (bc) { goto L39; }

  L57:
  // mfir.w vf31, vi08          |  nop                            699
  vu.vf31.mfir(Mask::w, vu.vi08);
  // mfir.x vf30, vi09          |  nop                            700
  vu.vf30.mfir(Mask::x, vu.vi09);
  // mfir.y vf30, vi10          |  nop                            701
  vu.vf30.mfir(Mask::y, vu.vi10);
  // mfir.z vf30, vi11          |  nop                            702
  vu.vf30.mfir(Mask::z, vu.vi11);
  // mfir.w vf30, vi12          |  nop                            703
  vu.vf30.mfir(Mask::w, vu.vi12);
  // sq.xyzw vf12, 1004(vi00)   |  nop                            704
  sq_buffer(Mask::xyzw, vu.vf12, 1004);
  // lq.xyzw vf12, -15(vi10)    |  nop                            705
  lq_buffer(Mask::xyzw, vu.vf12, vu.vi10 + -15);
  // sq.xyzw vf13, 1005(vi00)   |  nop                            706
  sq_buffer(Mask::xyzw, vu.vf13, 1005);
  // lq.xyzw vf13, -12(vi10)    |  nop                            707
  lq_buffer(Mask::xyzw, vu.vf13, vu.vi10 + -12);
  // sq.xyzw vf14, 1006(vi00)   |  nop                            708
  sq_buffer(Mask::xyzw, vu.vf14, 1006);
  // div Q, vf00.w, vf12.z      |  nop                            709
  vu.Q = vu.vf00.w() / vu.vf12.z();
  // sq.xyzw vf31, 1002(vi00)   |  nop                            710
  sq_buffer(Mask::xyzw, vu.vf31, 1002);
  // sq.xyzw vf30, 1003(vi00)   |  nop                            711
  sq_buffer(Mask::xyzw, vu.vf30, 1003);
  // sq.xyzw vf15, 1007(vi00)   |  nop                            712
  sq_buffer(Mask::xyzw, vu.vf15, 1007);
  // sq.xyzw vf16, 1008(vi00)   |  nop                            713
  sq_buffer(Mask::xyzw, vu.vf16, 1008);
  // lq.xyzw vf03, 4(vi13)      |  nop                            714
  lq_buffer(Mask::xyzw, vu.vf03, vu.vi13 + 4);
  // lq.xyzw vf15, -14(vi10)    |  sub.xw vf31, vf00, vf00        715
  vu.vf31.sub(Mask::xw, vu.vf00, vu.vf00);   lq_buffer(Mask::xyzw, vu.vf15, vu.vi10 + -14);
  // div Q, vf00.w, vf13.z      |  nop                            716
  vu.Q = vu.vf00.w() / vu.vf13.z();
  // lq.xyzw vf16, -11(vi10)    |  mul.xyz vf12, vf12, Q          717
  vu.vf12.mul(Mask::xyz, vu.vf12, vu.Q);   lq_buffer(Mask::xyzw, vu.vf16, vu.vi10 + -11);
  // lq.xyzw vf17, -8(vi10)     |  addx.y vf31, vf00, vf03        718
  vu.vf31.add(Mask::y, vu.vf00, vu.vf03.x());   lq_buffer(Mask::xyzw, vu.vf17, vu.vi10 + -8);
  // nop                        |  itof0.xyzw vf15, vf15          719
  vu.vf15.itof0(Mask::xyzw, vu.vf15);
  // sq.xyzw vf26, 989(vi00)    |  nop                            720
  sq_buffer(Mask::xyzw, vu.vf26, 989);
  // sq.xyzw vf12, 991(vi00)    |  itof0.xyzw vf16, vf16          721
  vu.vf16.itof0(Mask::xyzw, vu.vf16);   sq_buffer(Mask::xyzw, vu.vf12, 991);
  // sq.xyzw vf27, 992(vi00)    |  itof0.xyzw vf17, vf17          722
  vu.vf17.itof0(Mask::xyzw, vu.vf17);   sq_buffer(Mask::xyzw, vu.vf27, 992);
  // div Q, vf00.w, vf20.z      |  nop                            723
  vu.Q = vu.vf00.w() / vu.vf20.z();
  // nop                        |  mul.xyz vf13, vf13, Q          724
  vu.vf13.mul(Mask::xyz, vu.vf13, vu.Q);
  // sq.xyzw vf28, 995(vi00)    |  nop                            725
  sq_buffer(Mask::xyzw, vu.vf28, 995);
  // move.z vf31, vf03          |  nop                            726
  vu.vf31.move(Mask::z, vu.vf03);
  // sq.xyzw vf15, 990(vi00)    |  nop                            727
  sq_buffer(Mask::xyzw, vu.vf15, 990);
  // sq.xyzw vf16, 993(vi00)    |  nop                            728
  sq_buffer(Mask::xyzw, vu.vf16, 993);
  // sq.xyzw vf17, 996(vi00)    |  nop                            729
  sq_buffer(Mask::xyzw, vu.vf17, 996);
  // sq.xyzw vf13, 994(vi00)    |  nop                            730
  sq_buffer(Mask::xyzw, vu.vf13, 994);
  // sq.xyzw vf31, 961(vi00)    |  mul.xyz vf14, vf20, Q          731
  vu.vf14.mul(Mask::xyz, vu.vf20, vu.Q);   sq_buffer(Mask::xyzw, vu.vf31, 961);
  // nop                        |  nop                            732

  // nop                        |  nop                            733

  // BRANCH!
  // bal vi15, L66              |  nop                            734
  ASSERT(false);
  // sq.xyzw vf14, 997(vi00)    |  nop                            735
  sq_buffer(Mask::xyzw, vu.vf14, 997);
  if (bc) { goto L66; }

  // BRANCH!
  // ibeq vi00, vi05, L58       |  nop                            736
  bc = (vu.vi05 == 0);
  // nop                        |  nop                            737

  if (bc) { goto L58; }

  // BRANCH!
  // bal vi15, L63              |  nop                            738
  ASSERT(false);
  // nop                        |  nop                            739

  if (bc) { goto L63; }

  L58:
  // ilw.x vi05, 1002(vi00)     |  nop                            740
  ilw_buffer(Mask::x, vu.vi05, 1002);
  // ilw.y vi06, 1002(vi00)     |  nop                            741
  ilw_buffer(Mask::y, vu.vi06, 1002);
  // ilw.z vi07, 1002(vi00)     |  nop                            742
  ilw_buffer(Mask::z, vu.vi07, 1002);
  // ilw.w vi08, 1002(vi00)     |  nop                            743
  ilw_buffer(Mask::w, vu.vi08, 1002);
  // ilw.x vi09, 1003(vi00)     |  nop                            744
  ilw_buffer(Mask::x, vu.vi09, 1003);
  // ilw.y vi10, 1003(vi00)     |  nop                            745
  ilw_buffer(Mask::y, vu.vi10, 1003);
  // ilw.z vi11, 1003(vi00)     |  nop                            746
  ilw_buffer(Mask::z, vu.vi11, 1003);
  // ilw.w vi12, 1003(vi00)     |  nop                            747
  ilw_buffer(Mask::w, vu.vi12, 1003);
  // lq.xyzw vf12, 1004(vi00)   |  nop                            748
  lq_buffer(Mask::xyzw, vu.vf12, 1004);
  // lq.xyzw vf13, 1005(vi00)   |  nop                            749
  lq_buffer(Mask::xyzw, vu.vf13, 1005);
  // lq.xyzw vf14, 1006(vi00)   |  nop                            750
  lq_buffer(Mask::xyzw, vu.vf14, 1006);
  // lq.xyzw vf15, 1007(vi00)   |  nop                            751
  lq_buffer(Mask::xyzw, vu.vf15, 1007);
  // BRANCH!
  // b L56                      |  nop                            752
  bc = true;
  // lq.xyzw vf16, 1008(vi00)   |  nop                            753
  lq_buffer(Mask::xyzw, vu.vf16, 1008);
  if (bc) { goto L56; }

  L59:
  // BRANCH!
  // ibne vi05, vi09, L41       |  nop                            754
  bc = (vu.vi05 != vu.vi09);
  // sq.xyzw vf23, 998(vi00)    |  addw.w vf15, vf15, vf01        755
  vu.vf15.add(Mask::w, vu.vf15, vu.vf01.w());   sq_buffer(Mask::xyzw, vu.vf23, 998);
  if (bc) { goto L41; }

  // sq.xyzw vf24, 999(vi00)    |  mul.xyzw vf23, vf27, vf07      756
  vu.vf23.mul(Mask::xyzw, vu.vf27, vu.vf07);   sq_buffer(Mask::xyzw, vu.vf24, 999);
  // sq.xyzw vf25, 1000(vi00)   |  mul.xyzw vf24, vf28, vf07      757
  vu.vf24.mul(Mask::xyzw, vu.vf28, vu.vf07);   sq_buffer(Mask::xyzw, vu.vf25, 1000);
  // isw.x vi01, 1001(vi00)     |  mul.xyzw vf25, vf29, vf07      758
  vu.vf25.mul(Mask::xyzw, vu.vf29, vu.vf07);   isw_buffer(Mask::x, vu.vi01, 1001);
  // isw.y vi02, 1001(vi00)     |  nop                            759
  isw_buffer(Mask::y, vu.vi02, 1001);
  // isw.z vi03, 1001(vi00)     |  clipw.xyz vf23, vf23           760
  // TODO more clipping
  cf0 = clip(vu.vf23, vu.vf23.w(), cf);   isw_buffer(Mask::z, vu.vi03, 1001);
  // isw.w vi04, 1001(vi00)     |  clipw.xyz vf24, vf24           761
  cf1 = clip(vu.vf24, vu.vf24.w(), cf0);   isw_buffer(Mask::w, vu.vi04, 1001);
  // mfir.x vf31, vi05          |  clipw.xyz vf25, vf25           762
  cf2 = clip(vu.vf25, vu.vf25.w(), cf1);   vu.vf31.mfir(Mask::x, vu.vi05);
  // iaddiu vi04, vi00, 0x3f    |  nop                            763
  cf = cf2;
  vu.vi04 = 0x3f; /* 63 */
  // fcget vi01                 |  nop                            764
  vu.vi01 = cf0;
  // fcget vi02                 |  nop                            765
  vu.vi02 = cf1;
  // fcget vi03                 |  nop                            766
  vu.vi03 = cf2;
  // iand vi01, vi01, vi04      |  clipw.xyz vf27, vf27           767
  cf = clip(vu.vf27, vu.vf27.w(), cf);   vu.vi01 = vu.vi01 & vu.vi04;
  // iand vi01, vi01, vi02      |  clipw.xyz vf28, vf28           768
  cf = clip(vu.vf28, vu.vf28.w(), cf);   vu.vi01 = vu.vi01 & vu.vi02;
  // iand vi01, vi01, vi03      |  clipw.xyz vf29, vf29           769
  cf = clip(vu.vf29, vu.vf29.w(), cf);   vu.vi01 = vu.vi01 & vu.vi03;
  // mfir.y vf31, vi06          |  nop                            770
  vu.vf31.mfir(Mask::y, vu.vi06);
  // BRANCH!
  // ibeq vi00, vi01, L61       |  nop                            771
  bc = (vu.vi01 == 0);
  // mfir.z vf31, vi07          |  nop                            772
  vu.vf31.mfir(Mask::z, vu.vi07);
  if (!clipping_hack && bc) { goto L61; }

  L60:
  // div Q, vf01.x, vf13.w      |  nop                            773
  vu.Q = vu.vf01.x() / vu.vf13.w();
  // lq.xyzw vf23, 998(vi00)    |  nop                            774
  lq_buffer(Mask::xyzw, vu.vf23, 998);
  // lq.xyzw vf24, 999(vi00)    |  nop                            775
  lq_buffer(Mask::xyzw, vu.vf24, 999);
  // lq.xyzw vf25, 1000(vi00)   |  nop                            776
  lq_buffer(Mask::xyzw, vu.vf25, 1000);
  // ilw.x vi01, 1001(vi00)     |  nop                            777
  ilw_buffer(Mask::x, vu.vi01, 1001);
  // ilw.y vi02, 1001(vi00)     |  nop                            778
  ilw_buffer(Mask::y, vu.vi02, 1001);
  // ilw.z vi03, 1001(vi00)     |  nop                            779
  ilw_buffer(Mask::z, vu.vi03, 1001);
  // BRANCH!
  // b L41                      |  nop                            780
  bc = true;
  // ilw.w vi04, 1001(vi00)     |  nop                            781
  ilw_buffer(Mask::w, vu.vi04, 1001);
  if (bc) { goto L41; }

  L61:
  // mfir.w vf31, vi08          |  nop                            782
  vu.vf31.mfir(Mask::w, vu.vi08);
  // mfir.x vf30, vi09          |  nop                            783
  vu.vf30.mfir(Mask::x, vu.vi09);
  // mfir.y vf30, vi10          |  nop                            784
  vu.vf30.mfir(Mask::y, vu.vi10);
  // mfir.z vf30, vi11          |  nop                            785
  vu.vf30.mfir(Mask::z, vu.vi11);
  // mfir.w vf30, vi12          |  nop                            786
  vu.vf30.mfir(Mask::w, vu.vi12);
  // sq.xyzw vf12, 1004(vi00)   |  nop                            787
  sq_buffer(Mask::xyzw, vu.vf12, 1004);
  // lq.xyzw vf12, -15(vi10)    |  nop                            788
  lq_buffer(Mask::xyzw, vu.vf12, vu.vi10 + -15);
  // sq.xyzw vf13, 1005(vi00)   |  nop                            789
  sq_buffer(Mask::xyzw, vu.vf13, 1005);
  // lq.xyzw vf13, -12(vi10)    |  nop                            790
  lq_buffer(Mask::xyzw, vu.vf13, vu.vi10 + -12);
  // sq.xyzw vf14, 1006(vi00)   |  nop                            791
  sq_buffer(Mask::xyzw, vu.vf14, 1006);
  // div Q, vf00.w, vf12.z      |  nop                            792
  vu.Q = vu.vf00.w() / vu.vf12.z();
  // sq.xyzw vf31, 1002(vi00)   |  nop                            793
  sq_buffer(Mask::xyzw, vu.vf31, 1002);
  // sq.xyzw vf30, 1003(vi00)   |  nop                            794
  sq_buffer(Mask::xyzw, vu.vf30, 1003);
  // sq.xyzw vf15, 1007(vi00)   |  nop                            795
  sq_buffer(Mask::xyzw, vu.vf15, 1007);
  // sq.xyzw vf16, 1008(vi00)   |  nop                            796
  sq_buffer(Mask::xyzw, vu.vf16, 1008);
  // lq.xyzw vf03, 4(vi13)      |  nop                            797
  lq_buffer(Mask::xyzw, vu.vf03, vu.vi13 + 4);
  // lq.xyzw vf15, -14(vi10)    |  sub.xw vf31, vf00, vf00        798
  vu.vf31.sub(Mask::xw, vu.vf00, vu.vf00);   lq_buffer(Mask::xyzw, vu.vf15, vu.vi10 + -14);
  // div Q, vf00.w, vf13.z      |  nop                            799
  vu.Q = vu.vf00.w() / vu.vf13.z();
  // lq.xyzw vf16, -11(vi10)    |  mul.xyz vf12, vf12, Q          800
  vu.vf12.mul(Mask::xyz, vu.vf12, vu.Q);   lq_buffer(Mask::xyzw, vu.vf16, vu.vi10 + -11);
  // lq.xyzw vf17, -8(vi10)     |  addx.y vf31, vf00, vf03        801
  vu.vf31.add(Mask::y, vu.vf00, vu.vf03.x());   lq_buffer(Mask::xyzw, vu.vf17, vu.vi10 + -8);
  // nop                        |  itof0.xyzw vf15, vf15          802
  vu.vf15.itof0(Mask::xyzw, vu.vf15);
  // sq.xyzw vf27, 989(vi00)    |  nop                            803
  sq_buffer(Mask::xyzw, vu.vf27, 989);
  // sq.xyzw vf12, 991(vi00)    |  itof0.xyzw vf16, vf16          804
  vu.vf16.itof0(Mask::xyzw, vu.vf16);   sq_buffer(Mask::xyzw, vu.vf12, 991);
  // sq.xyzw vf28, 992(vi00)    |  itof0.xyzw vf17, vf17          805
  vu.vf17.itof0(Mask::xyzw, vu.vf17);   sq_buffer(Mask::xyzw, vu.vf28, 992);
  // div Q, vf00.w, vf21.z      |  nop                            806
  vu.Q = vu.vf00.w() / vu.vf21.z();
  // nop                        |  mul.xyz vf13, vf13, Q          807
  vu.vf13.mul(Mask::xyz, vu.vf13, vu.Q);
  // sq.xyzw vf29, 995(vi00)    |  nop                            808
  sq_buffer(Mask::xyzw, vu.vf29, 995);
  // move.z vf31, vf03          |  nop                            809
  vu.vf31.move(Mask::z, vu.vf03);
  // sq.xyzw vf15, 990(vi00)    |  nop                            810
  sq_buffer(Mask::xyzw, vu.vf15, 990);
  // sq.xyzw vf16, 993(vi00)    |  nop                            811
  sq_buffer(Mask::xyzw, vu.vf16, 993);
  // sq.xyzw vf17, 996(vi00)    |  nop                            812
  sq_buffer(Mask::xyzw, vu.vf17, 996);
  // sq.xyzw vf13, 994(vi00)    |  nop                            813
  sq_buffer(Mask::xyzw, vu.vf13, 994);
  // sq.xyzw vf31, 961(vi00)    |  mul.xyz vf14, vf21, Q          814
  vu.vf14.mul(Mask::xyz, vu.vf21, vu.Q);   sq_buffer(Mask::xyzw, vu.vf31, 961);
  // nop                        |  nop                            815

  // nop                        |  nop                            816

  // BRANCH!
  // bal vi15, L66              |  nop                            817
  ASSERT(false);
  // sq.xyzw vf14, 997(vi00)    |  nop                            818
  sq_buffer(Mask::xyzw, vu.vf14, 997);
  if (bc) { goto L66; }

  // BRANCH!
  // ibeq vi00, vi05, L62       |  nop                            819
  bc = (vu.vi05 == 0);
  // nop                        |  nop                            820

  if (bc) { goto L62; }

  // BRANCH!
  // bal vi15, L63              |  nop                            821
  ASSERT(false);
  // nop                        |  nop                            822

  if (bc) { goto L63; }

  L62:
  // ilw.x vi05, 1002(vi00)     |  nop                            823
  ilw_buffer(Mask::x, vu.vi05, 1002);
  // ilw.y vi06, 1002(vi00)     |  nop                            824
  ilw_buffer(Mask::y, vu.vi06, 1002);
  // ilw.z vi07, 1002(vi00)     |  nop                            825
  ilw_buffer(Mask::z, vu.vi07, 1002);
  // ilw.w vi08, 1002(vi00)     |  nop                            826
  ilw_buffer(Mask::w, vu.vi08, 1002);
  // ilw.x vi09, 1003(vi00)     |  nop                            827
  ilw_buffer(Mask::x, vu.vi09, 1003);
  // ilw.y vi10, 1003(vi00)     |  nop                            828
  ilw_buffer(Mask::y, vu.vi10, 1003);
  // ilw.z vi11, 1003(vi00)     |  nop                            829
  ilw_buffer(Mask::z, vu.vi11, 1003);
  // ilw.w vi12, 1003(vi00)     |  nop                            830
  ilw_buffer(Mask::w, vu.vi12, 1003);
  // lq.xyzw vf12, 1004(vi00)   |  nop                            831
  lq_buffer(Mask::xyzw, vu.vf12, 1004);
  // lq.xyzw vf13, 1005(vi00)   |  nop                            832
  lq_buffer(Mask::xyzw, vu.vf13, 1005);
  // lq.xyzw vf14, 1006(vi00)   |  nop                            833
  lq_buffer(Mask::xyzw, vu.vf14, 1006);
  // lq.xyzw vf15, 1007(vi00)   |  nop                            834
  lq_buffer(Mask::xyzw, vu.vf15, 1007);
  // BRANCH!
  // b L60                      |  nop                            835
  bc = true;
  // lq.xyzw vf16, 1008(vi00)   |  nop                            836
  lq_buffer(Mask::xyzw, vu.vf16, 1008);
  if (bc) { goto L60; }

  L63:
  // ilw.w vi01, 8(vi13)        |  nop                            837
  ilw_buffer(Mask::w, vu.vi01, vu.vi13 + 8);
  // ilw.y vi02, 1003(vi00)     |  nop                            838
  ilw_buffer(Mask::y, vu.vi02, 1003);
  // iaddi vi03, vi13, 0x7      |  nop                            839
  vu.vi03 = vu.vi13 + 7;
  // BRANCH!
  // ibltz vi01, L65            |  nop                            840
  bc = ((s16)vu.vi01) < 0;
  // ilw.w vi04, 906(vi00)      |  nop                            841
  ilw_buffer(Mask::w, vu.vi04, 906);
  if (bc) { goto L65; }

  // iaddi vi02, vi02, -0xf     |  nop                            842
  vu.vi02 = vu.vi02 + -15;
  // isub vi02, vi02, vi04      |  nop                            843
  vu.vi02 = vu.vi02 - vu.vi04;
  L64:
  // ilw.w vi04, 5(vi03)        |  nop                            844
  ilw_buffer(Mask::w, vu.vi04, vu.vi03 + 5);
  // ilw.w vi01, 6(vi03)        |  nop                            845
  ilw_buffer(Mask::w, vu.vi01, vu.vi03 + 6);
  // nop                        |  nop                            846

  // nop                        |  nop                            847

  // isub vi04, vi02, vi04      |  nop                            848
  vu.vi04 = vu.vi02 - vu.vi04;
  // nop                        |  nop                            849

  // BRANCH!
  // ibltz vi04, L65            |  nop                            850
  bc = ((s16)vu.vi04) < 0;
  // nop                        |  nop                            851

  if (bc) { goto L65; }

  // BRANCH!
  // ibgtz vi01, L64            |  nop                            852
  bc = ((s16)vu.vi01) > 0;
  // iaddi vi03, vi03, 0x5      |  nop                            853
  vu.vi03 = vu.vi03 + 5;
  if (bc) { goto L64; }

  L65:
  // iaddiu vi01, vi00, 0x3b9   |  nop                            854
  vu.vi01 = 0x3b9; /* 953 */
  // lq.xyzw vf12, 0(vi03)      |  nop                            855
  lq_buffer(Mask::xyzw, vu.vf12, vu.vi03);
  // lq.xyzw vf13, 1(vi03)      |  nop                            856
  lq_buffer(Mask::xyzw, vu.vf13, vu.vi03 + 1);
  // lq.xyzw vf14, 2(vi03)      |  nop                            857
  lq_buffer(Mask::xyzw, vu.vf14, vu.vi03 + 2);
  // lq.xyzw vf15, 3(vi03)      |  nop                            858
  lq_buffer(Mask::xyzw, vu.vf15, vu.vi03 + 3);
  // sq.xyzw vf02, 0(vi01)      |  nop                            859
  sq_buffer(Mask::xyzw, vu.vf02, vu.vi01);
  // sq.xyzw vf12, 1(vi01)      |  nop                            860
  sq_buffer(Mask::xyzw, vu.vf12, vu.vi01 + 1);
  // sq.xyzw vf13, 2(vi01)      |  nop                            861
  sq_buffer(Mask::xyzw, vu.vf13, vu.vi01 + 2);
  // sq.xyzw vf14, 3(vi01)      |  nop                            862
  sq_buffer(Mask::xyzw, vu.vf14, vu.vi01 + 3);
  // sq.xyzw vf15, 4(vi01)      |  nop                            863
  sq_buffer(Mask::xyzw, vu.vf15, vu.vi01 + 4);
  // lq.xyzw vf12, 4(vi03)      |  nop                            864
  lq_buffer(Mask::xyzw, vu.vf12, vu.vi03 + 4);
  // lq.xyzw vf13, 5(vi13)      |  nop                            865
  lq_buffer(Mask::xyzw, vu.vf13, vu.vi13 + 5);
  // lq.xyzw vf14, 6(vi13)      |  nop                            866
  lq_buffer(Mask::xyzw, vu.vf14, vu.vi13 + 6);
  // sq.xyzw vf12, 5(vi01)      |  nop                            867
  sq_buffer(Mask::xyzw, vu.vf12, vu.vi01 + 5);
  // sq.xyzw vf13, 6(vi01)      |  nop                            868
  sq_buffer(Mask::xyzw, vu.vf13, vu.vi01 + 6);
  // sq.xyzw vf14, 7(vi01)      |  nop                            869
  sq_buffer(Mask::xyzw, vu.vf14, vu.vi01 + 7);
  // xgkick vi01                |  nop                            870
  xgkick(vu.vi01, render_state, prof);
  // lq.xyzw vf12, 905(vi00)    |  nop                            871
  lq_buffer(Mask::xyzw, vu.vf12, 905);
  // iaddiu vi01, vi00, 0x3f3   |  nop                            872
  vu.vi01 = 0x3f3; /* 1011 */
  // isubiu vi02, vi00, 0x7fff  |  nop                            873
  vu.vi02 = -32767;
  // sq.xyzw vf02, 0(vi01)      |  nop                            874
  sq_buffer(Mask::xyzw, vu.vf02, vu.vi01);
  // iswr.x vi02, vi01          |  nop                            875
  isw_buffer(Mask::x, vu.vi02, vu.vi01);
  // sq.xyzw vf12, 1(vi01)      |  nop                            876
  sq_buffer(Mask::xyzw, vu.vf12, vu.vi01 + 1);
  // xgkick vi01                |  nop                            877
  xgkick(vu.vi01, render_state, prof);
  L66:
  // sq.xyzw vf00, 907(vi00)    |  nop                            878
  sq_buffer(Mask::xyzw, vu.vf00, 907);
  // sq.xyzw vf00, 914(vi00)    |  nop                            879
  sq_buffer(Mask::xyzw, vu.vf00, 914);
  // sq.xyzw vf00, 921(vi00)    |  nop                            880
  sq_buffer(Mask::xyzw, vu.vf00, 921);
  // sq.xyzw vf00, 928(vi00)    |  nop                            881
  sq_buffer(Mask::xyzw, vu.vf00, 928);
  // sq.xyzw vf00, 935(vi00)    |  nop                            882
  sq_buffer(Mask::xyzw, vu.vf00, 935);
  // sq.xyzw vf00, 942(vi00)    |  nop                            883
  sq_buffer(Mask::xyzw, vu.vf00, 942);
  // iaddiu vi01, vi00, 0x40f   |  nop                            884
  vu.vi01 = 0x40f; /* 1039 */
  // isw.z vi01, 907(vi00)      |  nop                            885
  isw_buffer(Mask::z, vu.vi01, 907);
  // iaddiu vi01, vi00, 0x411   |  nop                            886
  vu.vi01 = 0x411; /* 1041 */
  // isw.z vi01, 914(vi00)      |  nop                            887
  isw_buffer(Mask::z, vu.vi01, 914);
  // iaddiu vi01, vi00, 0x413   |  nop                            888
  vu.vi01 = 0x413; /* 1043 */
  // isw.z vi01, 921(vi00)      |  nop                            889
  isw_buffer(Mask::z, vu.vi01, 921);
  // iaddiu vi01, vi00, 0x415   |  nop                            890
  vu.vi01 = 0x415; /* 1045 */
  // isw.z vi01, 928(vi00)      |  nop                            891
  isw_buffer(Mask::z, vu.vi01, 928);
  // iaddiu vi01, vi00, 0x417   |  nop                            892
  vu.vi01 = 0x417; /* 1047 */
  // isw.z vi01, 935(vi00)      |  nop                            893
  isw_buffer(Mask::z, vu.vi01, 935);
  // iaddiu vi01, vi00, 0x419   |  nop                            894
  vu.vi01 = 0x419; /* 1049 */
  // isw.z vi01, 942(vi00)      |  nop                            895
  isw_buffer(Mask::z, vu.vi01, 942);
  // iaddiu vi03, vi00, 0x3c2   |  nop                            896
  vu.vi03 = 0x3c2; /* 962 */
  // iaddiu vi04, vi00, 0x3c1   |  nop                            897
  vu.vi04 = 0x3c1; /* 961 */
  // mfir.x vf31, vi15          |  nop                            898
  vu.vf31.mfir(Mask::x, vu.vi15);
  // iaddi vi05, vi00, 0x0      |  nop                            899
  vu.vi05 = 0;
  // BRANCH!
  // bal vi15, L67              |  nop                            900
  ASSERT(false);
  // iaddiu vi07, vi00, 0x3dd   |  nop                            901
  vu.vi07 = 0x3dd; /* 989 */
  if (bc) { goto L67; }

  // BRANCH!
  // bal vi15, L67              |  nop                            902
  ASSERT(false);
  // iaddiu vi07, vi00, 0x3e0   |  nop                            903
  vu.vi07 = 0x3e0; /* 992 */
  if (bc) { goto L67; }

  // BRANCH!
  // bal vi15, L67              |  nop                            904
  ASSERT(false);
  // iaddiu vi07, vi00, 0x3e3   |  nop                            905
  vu.vi07 = 0x3e3; /* 995 */
  if (bc) { goto L67; }

  // BRANCH!
  // b L76                      |  nop                            906
  bc = true;
  // nop                        |  nop                            907

  if (bc) { goto L76; }

  L67:
  // iaddiu vi09, vi00, 0x38b   |  nop                            908
  vu.vi09 = 0x38b; /* 907 */
  L68:
  // iaddi vi10, vi00, 0x0      |  nop                            909
  vu.vi10 = 0;
  L69:
  // isubiu vi01, vi09, 0x3b5   |  nop                            910
  vu.vi01 = vu.vi09 - 0x3b5; /* 949 */
  // ilwr.y vi08, vi09          |  nop                            911
  ilw_buffer(Mask::y, vu.vi08, vu.vi09);
  // BRANCH!
  // ibgez vi01, L73            |  nop                            912
  bc = ((s16)vu.vi01) >= 0;
  // ilwr.z vi06, vi09          |  nop                            913
  ilw_buffer(Mask::z, vu.vi06, vu.vi09);
  if (bc) { goto L73; }

  // lq.xyzw vf24, 0(vi07)      |  nop                            914
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi07);
  // lq.xyzw vf23, 0(vi08)      |  nop                            915
  lq_buffer(Mask::xyzw, vu.vf23, vu.vi08);
  // BRANCH!
  // ibne vi00, vi08, L70       |  nop                            916
  bc = (vu.vi08 != 0);
  // iswr.y vi07, vi09          |  nop                            917
  isw_buffer(Mask::y, vu.vi07, vu.vi09);
  if (bc) { goto L70; }

  // jalr vi11, vi06            |  nop                            918
  ASSERT(false);
  // iswr.x vi07, vi09          |  nop                            919
  isw_buffer(Mask::x, vu.vi07, vu.vi09);
  // nop                        |  nop                            920

  // nop                        |  nop                            921

  // nop                        |  nop                            922

  // fsand vi02, 0x2            |  nop                            923
  ASSERT(false);
  // BRANCH!
  // ibne vi00, vi02, L74       |  nop                            924
  bc = (vu.vi02 != 0);
  // nop                        |  nop                            925

  if (bc) { goto L74; }

  // BRANCH!
  // b L69                      |  nop                            926
  bc = true;
  // iaddi vi09, vi09, 0x7      |  nop                            927
  vu.vi09 = vu.vi09 + 7;
  if (bc) { goto L69; }

  L70:
  // jalr vi11, vi06            |  nop                            928
  ASSERT(false);
  // lq.xyzw vf15, 1(vi08)      |  nop                            929
  lq_buffer(Mask::xyzw, vu.vf15, vu.vi08 + 1);
  // lq.xyzw vf16, 1(vi07)      |  nop                            930
  lq_buffer(Mask::xyzw, vu.vf16, vu.vi07 + 1);
  // lq.xyzw vf12, 2(vi08)      |  nop                            931
  lq_buffer(Mask::xyzw, vu.vf12, vu.vi08 + 2);
  // fsand vi01, 0x2            |  nop                            932
  ASSERT(false);
  // fsand vi02, 0x2            |  subw.w vf31, vf30, vf31        933
  vu.vf31.sub(Mask::w, vu.vf30, vu.vf31.w());   ASSERT(false);
  // BRANCH!
  // ibne vi00, vi01, L72       |  nop                            934
  bc = (vu.vi01 != 0);
  // lq.xyzw vf13, 2(vi07)      |  nop                            935
  lq_buffer(Mask::xyzw, vu.vf13, vu.vi07 + 2);
  if (bc) { goto L72; }

  // BRANCH!
  // ibne vi00, vi02, L71       |  nop                            936
  bc = (vu.vi02 != 0);
  // div Q, vf30.w, vf31.w      |  nop                            937
  vu.Q = vu.vf30.w() / vu.vf31.w();
  if (bc) { goto L71; }

  // BRANCH!
  // b L69                      |  nop                            938
  bc = true;
  // iaddi vi09, vi09, 0x7      |  nop                            939
  vu.vi09 = vu.vi09 + 7;
  if (bc) { goto L69; }

  L71:
  // BRANCH!
  // bal vi11, L81              |  nop                            940
  ASSERT(false);
  // iaddi vi07, vi09, 0x1      |  nop                            941
  vu.vi07 = vu.vi09 + 1;
  if (bc) { goto L81; }

  // sq.xyzw vf25, 1(vi09)      |  nop                            942
  sq_buffer(Mask::xyzw, vu.vf25, vu.vi09 + 1);
  // sq.xyzw vf17, 2(vi09)      |  nop                            943
  sq_buffer(Mask::xyzw, vu.vf17, vu.vi09 + 2);
  // sq.xyzw vf14, 3(vi09)      |  nop                            944
  sq_buffer(Mask::xyzw, vu.vf14, vu.vi09 + 3);
  // BRANCH!
  // b L69                      |  nop                            945
  bc = true;
  // iaddi vi09, vi09, 0x7      |  nop                            946
  vu.vi09 = vu.vi09 + 7;
  if (bc) { goto L69; }

  L72:
  // BRANCH!
  // ibne vi00, vi02, L74       |  nop                            947
  bc = (vu.vi02 != 0);
  // div Q, vf30.w, vf31.w      |  nop                            948
  vu.Q = vu.vf30.w() / vu.vf31.w();
  if (bc) { goto L74; }

  // BRANCH!
  // bal vi11, L81              |  nop                            949
  ASSERT(false);
  // nop                        |  nop                            950

  if (bc) { goto L81; }

  // sq.xyzw vf25, 4(vi09)      |  nop                            951
  sq_buffer(Mask::xyzw, vu.vf25, vu.vi09 + 4);
  // sq.xyzw vf17, 5(vi09)      |  nop                            952
  sq_buffer(Mask::xyzw, vu.vf17, vu.vi09 + 5);
  // sq.xyzw vf14, 6(vi09)      |  nop                            953
  sq_buffer(Mask::xyzw, vu.vf14, vu.vi09 + 6);
  // iaddi vi09, vi09, 0x7      |  nop                            954
  vu.vi09 = vu.vi09 + 7;
  // isw.x vi09, 949(vi10)      |  nop                            955
  isw_buffer(Mask::x, vu.vi09, vu.vi10 + 949);
  // isw.y vi07, 949(vi10)      |  nop                            956
  isw_buffer(Mask::y, vu.vi07, vu.vi10 + 949);
  // iaddi vi10, vi10, 0x1      |  nop                            957
  vu.vi10 = vu.vi10 + 1;
  // BRANCH!
  // b L69                      |  nop                            958
  bc = true;
  // iaddi vi07, vi09, -0x3     |  nop                            959
  vu.vi07 = vu.vi09 + -3;
  if (bc) { goto L69; }

  L73:
  // lq.xyzw vf23, 0(vi07)      |  nop                            960
  lq_buffer(Mask::xyzw, vu.vf23, vu.vi07);
  // lq.xyzw vf15, 1(vi07)      |  nop                            961
  lq_buffer(Mask::xyzw, vu.vf15, vu.vi07 + 1);
  // lq.xyzw vf12, 2(vi07)      |  nop                            962
  lq_buffer(Mask::xyzw, vu.vf12, vu.vi07 + 2);
  // iaddi vi05, vi05, 0x1      |  nop                            963
  vu.vi05 = vu.vi05 + 1;
  // nop                        |  nop                            964

  // div Q, vf00.w, vf23.w      |  nop                            965
  vu.Q = vu.vf00.w() / vu.vf23.w();
  // nop                        |  ftoi0.xyzw vf15, vf15          966
  vu.vf15.ftoi0(Mask::xyzw, vu.vf15);
  // nop                        |  mul.xyzw vf23, vf23, vf06      967
  vu.vf23.mul(Mask::xyzw, vu.vf23, vu.vf06);
  // iaddi vi03, vi03, 0x3      |  nop                            968
  vu.vi03 = vu.vi03 + 3;
  // waitq                      |  nop                            969
  ASSERT(false);
  // sq.xyzw vf15, -2(vi03)     |  mul.xyz vf23, vf23, Q          970
  vu.vf23.mul(Mask::xyz, vu.vf23, vu.Q);   sq_buffer(Mask::xyzw, vu.vf15, vu.vi03 + -2);
  // nop                        |  mul.xyz vf12, vf12, Q          971
  vu.vf12.mul(Mask::xyz, vu.vf12, vu.Q);
  // nop                        |  add.xyzw vf23, vf23, vf04      972
  vu.vf23.add(Mask::xyzw, vu.vf23, vu.vf04);
  // nop                        |  maxy.w vf23, vf23, vf01        973
  vu.vf23.max(Mask::w, vu.vf23, vu.vf01.y());
  // nop                        |  miniz.w vf23, vf23, vf01       974
  vu.vf23.mini(Mask::w, vu.vf23, vu.vf01.z());
  // nop                        |  ftoi4.xyzw vf23, vf23          975
  vu.vf23.ftoi4(Mask::xyzw, vu.vf23);
  // sq.xyzw vf12, -3(vi03)     |  nop                            976
  sq_buffer(Mask::xyzw, vu.vf12, vu.vi03 + -3);
  // sq.xyzw vf23, -1(vi03)     |  nop                            977
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi03 + -1);
  L74:
  // BRANCH!
  // iblez vi10, L75            |  nop                            978
  bc = ((s16)vu.vi10) <= 0;
  // nop                        |  nop                            979

  if (bc) { goto L75; }

  // ilw.x vi09, 948(vi10)      |  nop                            980
  ilw_buffer(Mask::x, vu.vi09, vu.vi10 + 948);
  // ilw.y vi07, 948(vi10)      |  nop                            981
  ilw_buffer(Mask::y, vu.vi07, vu.vi10 + 948);
  // BRANCH!
  // b L69                      |  nop                            982
  bc = true;
  // iaddi vi10, vi10, -0x1     |  nop                            983
  vu.vi10 = vu.vi10 + -1;
  if (bc) { goto L69; }

  L75:
  // jr vi15                    |  nop                            984
  ASSERT(false);
  // nop                        |  nop                            985

  L76:
  // iaddiu vi09, vi00, 0x38b   |  nop                            986
  vu.vi09 = 0x38b; /* 907 */
  L77:
  // ilwr.x vi08, vi09          |  nop                            987
  ilw_buffer(Mask::x, vu.vi08, vu.vi09);
  // ilwr.y vi07, vi09          |  nop                            988
  ilw_buffer(Mask::y, vu.vi07, vu.vi09);
  // ilwr.z vi06, vi09          |  nop                            989
  ilw_buffer(Mask::z, vu.vi06, vu.vi09);
  // nop                        |  nop                            990

  // BRANCH!
  // ibeq vi00, vi08, L79       |  nop                            991
  bc = (vu.vi08 == 0);
  // lq.xyzw vf23, 0(vi07)      |  nop                            992
  lq_buffer(Mask::xyzw, vu.vf23, vu.vi07);
  if (bc) { goto L79; }

  // BRANCH!
  // ibeq vi07, vi08, L79       |  nop                            993
  bc = (vu.vi07 == vu.vi08);
  // lq.xyzw vf24, 0(vi08)      |  nop                            994
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi08);
  if (bc) { goto L79; }

  // jalr vi11, vi06            |  nop                            995
  ASSERT(false);
  // lq.xyzw vf15, 1(vi07)      |  nop                            996
  lq_buffer(Mask::xyzw, vu.vf15, vu.vi07 + 1);
  // lq.xyzw vf16, 1(vi08)      |  nop                            997
  lq_buffer(Mask::xyzw, vu.vf16, vu.vi08 + 1);
  // lq.xyzw vf12, 2(vi07)      |  nop                            998
  lq_buffer(Mask::xyzw, vu.vf12, vu.vi07 + 2);
  // fsand vi01, 0x2            |  nop                            999
  ASSERT(false);
  // fsand vi02, 0x2            |  subw.w vf31, vf30, vf31        1000
  vu.vf31.sub(Mask::w, vu.vf30, vu.vf31.w());   ASSERT(false);
  // BRANCH!
  // ibeq vi02, vi01, L79       |  nop                            1001
  bc = (vu.vi02 == vu.vi01);
  // lq.xyzw vf13, 2(vi08)      |  nop                            1002
  lq_buffer(Mask::xyzw, vu.vf13, vu.vi08 + 2);
  if (bc) { goto L79; }

  // BRANCH!
  // ibeq vi00, vi01, L78       |  nop                            1003
  bc = (vu.vi01 == 0);
  // div Q, vf30.w, vf31.w      |  nop                            1004
  vu.Q = vu.vf30.w() / vu.vf31.w();
  if (bc) { goto L78; }

  // BRANCH!
  // bal vi11, L81              |  nop                            1005
  ASSERT(false);
  // nop                        |  nop                            1006

  if (bc) { goto L81; }

  // sq.xyzw vf25, 4(vi09)      |  nop                            1007
  sq_buffer(Mask::xyzw, vu.vf25, vu.vi09 + 4);
  // sq.xyzw vf17, 5(vi09)      |  nop                            1008
  sq_buffer(Mask::xyzw, vu.vf17, vu.vi09 + 5);
  // sq.xyzw vf14, 6(vi09)      |  nop                            1009
  sq_buffer(Mask::xyzw, vu.vf14, vu.vi09 + 6);
  // iaddi vi07, vi09, 0x4      |  nop                            1010
  vu.vi07 = vu.vi09 + 4;
  // ior vi12, vi09, vi00       |  nop                            1011
  vu.vi12 = vu.vi09;
  // BRANCH!
  // bal vi15, L68              |  nop                            1012
  ASSERT(false);
  // iaddi vi09, vi09, 0x7      |  nop                            1013
  vu.vi09 = vu.vi09 + 7;
  if (bc) { goto L68; }

  // BRANCH!
  // b L79                      |  nop                            1014
  bc = true;
  // ior vi09, vi12, vi00       |  nop                            1015
  vu.vi09 = vu.vi12;
  if (bc) { goto L79; }

  L78:
  // BRANCH!
  // bal vi11, L81              |  nop                            1016
  ASSERT(false);
  // nop                        |  nop                            1017

  if (bc) { goto L81; }

  // sq.xyzw vf25, 1(vi09)      |  nop                            1018
  sq_buffer(Mask::xyzw, vu.vf25, vu.vi09 + 1);
  // sq.xyzw vf17, 2(vi09)      |  nop                            1019
  sq_buffer(Mask::xyzw, vu.vf17, vu.vi09 + 2);
  // sq.xyzw vf14, 3(vi09)      |  nop                            1020
  sq_buffer(Mask::xyzw, vu.vf14, vu.vi09 + 3);
  // iaddi vi07, vi09, 0x1      |  nop                            1021
  vu.vi07 = vu.vi09 + 1;
  // ior vi12, vi09, vi00       |  nop                            1022
  vu.vi12 = vu.vi09;
  // BRANCH!
  // bal vi15, L68              |  nop                            1023
  ASSERT(false);
  // iaddi vi09, vi09, 0x7      |  nop                            1024
  vu.vi09 = vu.vi09 + 7;
  if (bc) { goto L68; }

  // ior vi09, vi12, vi00       |  nop                            1025
  vu.vi09 = vu.vi12;
  L79:
  // isubiu vi01, vi09, 0x3ae   |  nop                            1026
  vu.vi01 = vu.vi09 - 0x3ae; /* 942 */
  // iswr.x vi00, vi09          |  nop                            1027
  isw_buffer(Mask::x, vu.vi00, vu.vi09);
  // iswr.y vi00, vi09          |  nop                            1028
  isw_buffer(Mask::y, vu.vi00, vu.vi09);
  // BRANCH!
  // ibltz vi01, L77            |  nop                            1029
  bc = ((s16)vu.vi01) < 0;
  // iaddi vi09, vi09, 0x7      |  nop                            1030
  vu.vi09 = vu.vi09 + 7;
  if (bc) { goto L77; }

  // BRANCH!
  // ibeq vi00, vi05, L80       |  nop                            1031
  bc = (vu.vi05 == 0);
  // mtir vi15, vf31.x          |  nop                            1032
  vu.vi15 = vu.vf31.x_as_u16();
  if (bc) { goto L80; }

  // iaddiu vi05, vi05, 0x4000  |  nop                            1033
  vu.vi05 = vu.vi05 + 0x4000; /* 16384 */
  // iaddiu vi05, vi05, 0x4000  |  nop                            1034
  vu.vi05 = vu.vi05 + 0x4000; /* 16384 */
  // iswr.x vi05, vi04          |  nop                            1035
  isw_buffer(Mask::x, vu.vi05, vu.vi04);
  L80:
  // nop                        |  nop                            1036

  // jr vi15                    |  nop                            1037
  ASSERT(false);
  // nop                        |  nop                            1038

  // jr vi11                    |  addx.w vf30, vf23, vf23        1039
  vu.vf30.add(Mask::w, vu.vf23, vu.vf23.x());   ASSERT(false);
  // nop                        |  addx.w vf31, vf24, vf24        1040
  vu.vf31.add(Mask::w, vu.vf24, vu.vf24.x());
  // jr vi11                    |  subx.w vf30, vf23, vf23        1041
  vu.vf30.sub(Mask::w, vu.vf23, vu.vf23.x());   ASSERT(false);
  // nop                        |  subx.w vf31, vf24, vf24        1042
  vu.vf31.sub(Mask::w, vu.vf24, vu.vf24.x());
  // jr vi11                    |  addy.w vf30, vf23, vf23        1043
  vu.vf30.add(Mask::w, vu.vf23, vu.vf23.y());   ASSERT(false);
  // nop                        |  addy.w vf31, vf24, vf24        1044
  vu.vf31.add(Mask::w, vu.vf24, vu.vf24.y());
  // jr vi11                    |  suby.w vf30, vf23, vf23        1045
  vu.vf30.sub(Mask::w, vu.vf23, vu.vf23.y());   ASSERT(false);
  // nop                        |  suby.w vf31, vf24, vf24        1046
  vu.vf31.sub(Mask::w, vu.vf24, vu.vf24.y());
  // jr vi11                    |  addz.w vf30, vf23, vf23        1047
  vu.vf30.add(Mask::w, vu.vf23, vu.vf23.z());   ASSERT(false);
  // nop                        |  addz.w vf31, vf24, vf24        1048
  vu.vf31.add(Mask::w, vu.vf24, vu.vf24.z());
  // jr vi11                    |  subz.w vf30, vf23, vf23        1049
  vu.vf30.sub(Mask::w, vu.vf23, vu.vf23.z());   ASSERT(false);
  // nop                        |  subz.w vf31, vf24, vf24        1050
  vu.vf31.sub(Mask::w, vu.vf24, vu.vf24.z());
  L81:
  // nop                        |  sub.xyzw vf25, vf24, vf23      1051
  vu.vf25.sub(Mask::xyzw, vu.vf24, vu.vf23);
  // nop                        |  sub.xyzw vf17, vf16, vf15      1052
  vu.vf17.sub(Mask::xyzw, vu.vf16, vu.vf15);
  // nop                        |  sub.xyzw vf14, vf13, vf12      1053
  vu.vf14.sub(Mask::xyzw, vu.vf13, vu.vf12);
  // waitq                      |  mul.xyzw vf25, vf25, Q         1054
  vu.vf25.mul(Mask::xyzw, vu.vf25, vu.Q);   ASSERT(false);
  // nop                        |  mul.xyzw vf17, vf17, Q         1055
  vu.vf17.mul(Mask::xyzw, vu.vf17, vu.Q);
  // nop                        |  mul.xyzw vf14, vf14, Q         1056
  vu.vf14.mul(Mask::xyzw, vu.vf14, vu.Q);
  // nop                        |  add.xyzw vf25, vf23, vf25      1057
  vu.vf25.add(Mask::xyzw, vu.vf23, vu.vf25);
  // jr vi11                    |  add.xyzw vf17, vf15, vf17      1058
  vu.vf17.add(Mask::xyzw, vu.vf15, vu.vf17);   ASSERT(false);
  // nop                        |  add.xyzw vf14, vf12, vf14      1059
  vu.vf14.add(Mask::xyzw, vu.vf12, vu.vf14);
  L82:
  // iaddi vi14, vi13, 0x7      |  nop                            1060
  vu.vi14 = vu.vi13 + 7;
  // lq.xyzw vf03, 4(vi13)      |  nop                            1061
  lq_buffer(Mask::xyzw, vu.vf03, vu.vi13 + 4);
  // ilw.w vi02, 6(vi13)        |  nop                            1062
  ilw_buffer(Mask::w, vu.vi02, vu.vi13 + 6);
  // lq.xyzw vf21, 5(vi13)      |  nop                            1063
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi13 + 5);
  // lq.xyzw vf22, 6(vi13)      |  nop                            1064
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi13 + 6);
  L83:
  // ilwr.w vi03, vi14          |  nop                            1065
  ilw_buffer(Mask::w, vu.vi03, vu.vi14);
  // ilw.w vi04, 1(vi14)        |  nop                            1066
  ilw_buffer(Mask::w, vu.vi04, vu.vi14 + 1);
  // lqi.xyzw vf16, vi14        |  nop                            1067
  lq_buffer(Mask::xyzw, vu.vf16, vu.vi14++);
  // lqi.xyzw vf17, vi14        |  nop                            1068
  lq_buffer(Mask::xyzw, vu.vf17, vu.vi14++);
  // lqi.xyzw vf18, vi14        |  nop                            1069
  lq_buffer(Mask::xyzw, vu.vf18, vu.vi14++);
  // lqi.xyzw vf19, vi14        |  nop                            1070
  lq_buffer(Mask::xyzw, vu.vf19, vu.vi14++);
  // lqi.xyzw vf20, vi14        |  nop                            1071
  lq_buffer(Mask::xyzw, vu.vf20, vu.vi14++);
  // iadd vi06, vi03, vi12      |  nop                            1072
  vu.vi06 = vu.vi03 + vu.vi12;
  // sqi.xyzw vf02, vi06        |  nop                            1073
  sq_buffer(Mask::xyzw, vu.vf02, vu.vi06++);
  // sqi.xyzw vf16, vi06        |  nop                            1074
  sq_buffer(Mask::xyzw, vu.vf16, vu.vi06++);
  // sqi.xyzw vf17, vi06        |  nop                            1075
  sq_buffer(Mask::xyzw, vu.vf17, vu.vi06++);
  // sqi.xyzw vf18, vi06        |  nop                            1076
  sq_buffer(Mask::xyzw, vu.vf18, vu.vi06++);
  // sqi.xyzw vf19, vi06        |  nop                            1077
  sq_buffer(Mask::xyzw, vu.vf19, vu.vi06++);
  // sqi.xyzw vf20, vi06        |  nop                            1078
  sq_buffer(Mask::xyzw, vu.vf20, vu.vi06++);
  // sqi.xyzw vf21, vi06        |  nop                            1079
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi06++);
  // sqi.xyzw vf22, vi06        |  nop                            1080
  sq_buffer(Mask::xyzw, vu.vf22, vu.vi06++);
  // sqi.xyzw vf03, vi06        |  nop                            1081
  sq_buffer(Mask::xyzw, vu.vf03, vu.vi06++);
  // BRANCH!
  // ibgez vi04, L83            |  nop                            1082
  bc = ((s16)vu.vi04) >= 0;
  // isw.x vi04, -1(vi06)       |  nop                            1083
  isw_buffer(Mask::x, vu.vi04, vu.vi06 + -1);
  if (bc) { goto L83; }

  // iadd vi02, vi12, vi02      |  nop                            1084
  vu.vi02 = vu.vi12 + vu.vi02;
  // nop                        |  nop                            1085

  // xgkick vi02                |  nop                            1086
  xgkick(vu.vi02, render_state, prof);
  // isubiu vi01, vi12, 0x22e   |  nop                            1087
  vu.vi01 = vu.vi12 - 0x22e; /* 558 */
  // nop                        |  nop                            1088

  // BRANCH!
  // ibltz vi01, L84            |  nop                            1089
  bc = ((s16)vu.vi01) < 0;
  // iaddiu vi12, vi12, 0x117   |  nop                            1090
  vu.vi12 = vu.vi12 + 0x117; /* 279 */
  if (bc) { goto L84; }

  // iaddi vi12, vi00, 0x0      |  nop                            1091
  vu.vi12 = 0;
  L84:
  // nop                        |  nop :e                         1092

  // nop                        |  nop                            1093

  return;
}