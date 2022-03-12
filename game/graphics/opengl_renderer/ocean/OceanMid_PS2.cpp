#include "OceanMid.h"
void fcand(u16& dest, u32 imm, u32 cf) {
  dest = (cf & imm) ? 1 : 0;
  //  dest = cf & imm; // wrong
}

void fcor(u16& dest, u32 imm, u32 cf) {
  dest = (cf | imm) == imm ? 1 : 0;
  //  dest = cf | imm;
}
void OceanMid::run_call0_vu2c() {
  bool bc;
  // lq.xyzw vf01, 733(vi00)    |  nop                            0
  lq_buffer(Mask::xyzw, vu.vf01, 733);
  // lq.xyzw vf02, 735(vi00)    |  nop                            1
  lq_buffer(Mask::xyzw, vu.vf02, 735);
  // lq.xyzw vf03, 736(vi00)    |  nop                            2
  lq_buffer(Mask::xyzw, vu.vf03, 736);
  // lq.xyzw vf05, 737(vi00)    |  nop                            3
  lq_buffer(Mask::xyzw, vu.vf05, 737);
  // lq.xyzw vf06, 738(vi00)    |  nop                            4
  lq_buffer(Mask::xyzw, vu.vf06, 738);
  // iaddiu vi09, vi00, 0x14f   |  nop                            5
  vu.vi09 = 0x14f; /* 335 */
  // iaddi vi01, vi00, 0x6      |  nop                            6
  vu.vi01 = 6;
L1:
  // lq.xyzw vf20, 741(vi01)    |  nop                            7
  lq_buffer(Mask::xyzw, vu.vf20, vu.vi01 + 741);
  // lq.xyzw vf21, 749(vi01)    |  nop                            8
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi01 + 749);
  // sq.xyzw vf20, 335(vi01)    |  nop                            9
  sq_buffer(Mask::xyzw, vu.vf20, vu.vi01 + 335);
  // sq.xyzw vf20, 457(vi01)    |  nop                            10
  sq_buffer(Mask::xyzw, vu.vf20, vu.vi01 + 457);
  // sq.xyzw vf21, 396(vi01)    |  nop                            11
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi01 + 396);
  // sq.xyzw vf21, 518(vi01)    |  nop                            12
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi01 + 518);
  // BRANCH!
  // ibgtz vi01, L1             |  nop                            13
  bc = ((s16)vu.vi01) > 0;
  // iaddi vi01, vi01, -0x1     |  nop                            14
  vu.vi01 = vu.vi01 + -1;
  if (bc) {
    goto L1;
  }

  // iaddi vi05, vi00, 0x0      |  mul.xyzw vf16, vf00, vf00      15
  vu.vf16.mul(Mask::xyzw, vu.vf00, vu.vf00);
  vu.vi05 = 0;
  // iaddi vi07, vi00, 0x8      |  nop                            16
  vu.vi07 = 8;
L2:
  // iaddi vi06, vi00, 0x8      |  mul.x vf16, vf00, vf00         17
  vu.vf16.mul(Mask::x, vu.vf00, vu.vf00);
  vu.vi06 = 8;
L3:
  // sq.xyzw vf16, 236(vi05)    |  nop                            18
  sq_buffer(Mask::xyzw, vu.vf16, vu.vi05 + 236);
  // iaddi vi05, vi05, 0x1      |  addw.x vf16, vf16, vf05        19
  vu.vf16.add(Mask::x, vu.vf16, vu.vf05.w());
  vu.vi05 = vu.vi05 + 1;
  // BRANCH!
  // ibgtz vi06, L3             |  nop                            20
  bc = ((s16)vu.vi06) > 0;
  // iaddi vi06, vi06, -0x1     |  nop                            21
  vu.vi06 = vu.vi06 + -1;
  if (bc) {
    goto L3;
  }

  // BRANCH!
  // ibgtz vi07, L2             |  addw.z vf16, vf16, vf05        22
  vu.vf16.add(Mask::z, vu.vf16, vu.vf05.w());
  bc = ((s16)vu.vi07) > 0;
  // iaddi vi07, vi07, -0x1     |  nop                            23
  vu.vi07 = vu.vi07 + -1;
  if (bc) {
    goto L2;
  }

  // nop                        |  sub.xyzw vf20, vf20, vf20      24
  vu.vf20.set_zero();
  // nop                        |  sub.xyzw vf21, vf21, vf21      25
  vu.vf21.set_zero();
  // iaddi vi03, vi00, 0x0      |  addw.z vf20, vf20, vf00        26
  vu.vf20.add(Mask::z, vu.vf20, vu.vf00.w());
  vu.vi03 = 0;
  // iaddi vi06, vi00, 0x8      |  addw.yz vf21, vf21, vf00       27
  vu.vf21.add(Mask::yz, vu.vf21, vu.vf00.w());
  vu.vi06 = 8;
L4:
  // sq.xyzw vf20, 317(vi03)    |  nop                            28
  sq_buffer(Mask::xyzw, vu.vf20, vu.vi03 + 317);
  // sq.xyzw vf21, 318(vi03)    |  addw.x vf20, vf20, vf00        29
  vu.vf20.add(Mask::x, vu.vf20, vu.vf00.w());
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi03 + 318);
  // iaddi vi03, vi03, 0x2      |  addw.x vf21, vf21, vf00        30
  vu.vf21.add(Mask::x, vu.vf21, vu.vf00.w());
  vu.vi03 = vu.vi03 + 2;
  // BRANCH!
  // ibgtz vi06, L4             |  nop                            31
  bc = ((s16)vu.vi06) > 0;
  // iaddi vi06, vi06, -0x1     |  nop                            32
  vu.vi06 = vu.vi06 + -1;
  if (bc) {
    goto L4;
  }

  // lq.xyzw vf08, 756(vi00)    |  nop                            33
  lq_buffer(Mask::xyzw, vu.vf08, 756);
  // iaddi vi04, vi00, 0x8      |  nop                            34
  vu.vi04 = 8;
  // iaddiu vi06, vi00, 0x11    |  ftoi0.xyzw vf08, vf08          35
  vu.vf08.ftoi0(Mask::xyzw, vu.vf08);
  vu.vi06 = 0x11; /* 17 */
L5:
  // sq.xyzw vf08, 396(vi04)    |  nop                            36
  sq_buffer(Mask::xyzw, vu.vf08, vu.vi04 + 396);
  // sq.xyzw vf08, 518(vi04)    |  nop                            37
  sq_buffer(Mask::xyzw, vu.vf08, vu.vi04 + 518);
  // iaddi vi04, vi04, 0x3      |  nop                            38
  vu.vi04 = vu.vi04 + 3;
  // BRANCH!
  // ibgtz vi06, L5             |  nop                            39
  bc = ((s16)vu.vi06) > 0;
  // iaddi vi06, vi06, -0x1     |  nop                            40
  vu.vi06 = vu.vi06 + -1;
  if (bc) {
    goto L5;
  }

  // JUMP_41:
  // nop                        |  nop :e                         41

  // nop                        |  nop                            42

  return;
}

void OceanMid::run_call46_vu2c(SharedRenderState* render_state,
                               ScopedProfilerNode& prof,
                               DirectRenderer& direct) {
  bool bc;
JUMP_46:
  // xtop vi02                  |  nop                            46
  vu.vi02 = xtop();
  // lq.xyzw vf07, 748(vi00)    |  nop                            47
  lq_buffer(Mask::xyzw, vu.vf07, 748);
  // lq.xyzw vf12, 4(vi02)      |  nop                            48
  lq_buffer(Mask::xyzw, vu.vf12, vu.vi02 + 4);
  // lq.xyzw vf13, 5(vi02)      |  nop                            49
  lq_buffer(Mask::xyzw, vu.vf13, vu.vi02 + 5);
  // lq.xyzw vf14, 6(vi02)      |  nop                            50
  lq_buffer(Mask::xyzw, vu.vf14, vu.vi02 + 6);
  // lq.xyzw vf15, 7(vi02)      |  nop                            51
  lq_buffer(Mask::xyzw, vu.vf15, vu.vi02 + 7);
  // sq.xyzw vf07, 341(vi00)    |  nop                            52
  sq_buffer(Mask::xyzw, vu.vf07, 341);
  // sq.xyzw vf07, 463(vi00)    |  nop                            53
  sq_buffer(Mask::xyzw, vu.vf07, 463);
  // iaddi vi07, vi00, 0x7      |  nop                            54
  vu.vi07 = 7;
  // lq.xyzw vf04, 116(vi02)    |  nop                            55
  lq_buffer(Mask::xyzw, vu.vf04, vu.vi02 + 116);
L9:
  // iaddi vi01, vi07, -0x4     |  nop                            56
  vu.vi01 = vu.vi07 + -4;
  // mtir vi10, vf04.x          |  nop                            57
  vu.vi10 = vu.vf04.x_as_u16();
  // iaddiu vi11, vi00, 0xff    |  nop                            58
  vu.vi11 = 0xff; /* 255 */
  // BRANCH!
  // ibne vi00, vi01, L10       |  nop                            59
  bc = (vu.vi01 != 0);
  // mr32.xyzw vf04, vf04       |  nop                            60
  vu.vf04.mr32(Mask::xyzw, vu.vf04);
  if (bc) {
    goto L10;
  }

  // lq.xyzw vf04, 117(vi02)    |  nop                            61
  lq_buffer(Mask::xyzw, vu.vf04, vu.vi02 + 117);
L10:
  // BRANCH!
  // ibeq vi11, vi10, L11       |  nop                            62
  bc = (vu.vi11 == vu.vi10);
  // iaddi vi08, vi09, 0x7      |  nop                            63
  vu.vi08 = vu.vi09 + 7;
  if (bc) {
    goto L11;
  }

  // BRANCH!
  // bal vi15, L24              |  nop                            64
  // ASSERT(false);
  run_L24_vu2c();
  // nop                        |  nop                            65

  // iaddiu vi01, vi00, 0x318   |  nop                            66
  vu.vi01 = 0x318; /* 792 */
  // xgkick vi09                |  nop                            67
  xgkick(vu.vi09, render_state, prof, direct);
  // isub vi09, vi01, vi09      |  nop                            68
  vu.vi09 = vu.vi01 - vu.vi09;
L11:
  // BRANCH!
  // ibgtz vi07, L9             |  nop                            69
  bc = ((s16)vu.vi07) > 0;
  // iaddi vi07, vi07, -0x1     |  nop                            70
  vu.vi07 = vu.vi07 + -1;
  if (bc) {
    goto L9;
  }

  // nop                        |  nop :e                         71

  // nop                        |  nop                            72

  return;
}

namespace {
u32 clip(const Vf& vector, float val, u32 old_clip) {
  u32 result = (old_clip << 6);
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
  return result & 0xffffff;  // only 24 bits
}
}  // namespace

void OceanMid::run_L24_vu2c() {
  bool bc;
  u32 cf = 0;

L24:
  // ilw.x vi05, 757(vi07)      |  nop                            391
  ilw_buffer(Mask::x, vu.vi05, vu.vi07 + 757);
  // ilw.y vi04, 757(vi07)      |  nop                            392
  ilw_buffer(Mask::y, vu.vi04, vu.vi07 + 757);
  // iaddi vi03, vi00, 0x0      |  nop                            393
  vu.vi03 = 0;
  // iadd vi04, vi04, vi02      |  nop                            394
  vu.vi04 = vu.vi04 + vu.vi02;
  // iaddi vi06, vi00, 0x8      |  nop                            395
  vu.vi06 = 8;
  // lq.xyzw vf28, 236(vi05)    |  nop                            396
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi05 + 236);
  // lq.xyzw vf29, 245(vi05)    |  mulax.xyzw ACC, vf12, vf28     397
  vu.acc.mula(Mask::xyzw, vu.vf12, vu.vf28.x());
  lq_buffer(Mask::xyzw, vu.vf29, vu.vi05 + 245);
  // iaddi vi05, vi05, 0x1      |  madday.xyzw ACC, vf13, vf28    398
  vu.acc.madda(Mask::xyzw, vu.vf13, vu.vf28.y());
  vu.vi05 = vu.vi05 + 1;
  // fcset 0x0                  |  maddaz.xyzw ACC, vf14, vf28    399
  vu.acc.madda(Mask::xyzw, vu.vf14, vu.vf28.z());
  cf = 0;
  // nop                        |  maddw.xyzw vf30, vf15, vf00    400
  vu.acc.madd(Mask::xyzw, vu.vf30, vu.vf15, vu.vf00.w());
  // div Q, vf03.x, vf30.w      |  mulax.xyzw ACC, vf12, vf29     401
  vu.acc.mula(Mask::xyzw, vu.vf12, vu.vf29.x());
  vu.Q = vu.vf03.x() / vu.vf30.w();
  // lq.xyzw vf28, 236(vi05)    |  madday.xyzw ACC, vf13, vf29    402
  vu.acc.madda(Mask::xyzw, vu.vf13, vu.vf29.y());
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi05 + 236);
  // lq.xyzw vf20, 317(vi03)    |  maddaz.xyzw ACC, vf14, vf29    403
  vu.acc.madda(Mask::xyzw, vu.vf14, vu.vf29.z());
  lq_buffer(Mask::xyzw, vu.vf20, vu.vi03 + 317);
  // lq.xyzw vf22, 8(vi04)      |  maddw.xyzw vf31, vf15, vf00    404
  vu.acc.madd(Mask::xyzw, vu.vf31, vu.vf15, vu.vf00.w());
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi04 + 8);
  // nop                        |  mul.xyzw vf18, vf30, vf01      405
  vu.vf18.mul(Mask::xyzw, vu.vf30, vu.vf01);
  // waitq                      |  mulaw.w ACC, vf30, vf00        406
  vu.acc.mula(Mask::w, vu.vf30, vu.vf00.w());
  // nop                        |  mula.xyz ACC, vf30, Q          407
  vu.acc.mula(Mask::xyz, vu.vf30, vu.Q);
  // nop                        |  maddw.xyzw vf16, vf02, vf00    408
  vu.acc.madd(Mask::xyzw, vu.vf16, vu.vf02, vu.vf00.w());
  // nop                        |  clipw.xyz vf18, vf18           409
  cf = clip(vu.vf18, vu.vf18.w(), cf);
  // nop                        |  mul.xyzw vf19, vf31, vf01      410
  vu.vf19.mul(Mask::xyzw, vu.vf31, vu.vf01);
  // div Q, vf03.x, vf31.w      |  mul.xyzw vf20, vf20, Q         411
  vu.vf20.mul(Mask::xyzw, vu.vf20, vu.Q);
  vu.Q = vu.vf03.x() / vu.vf31.w();
  // nop                        |  mulax.xyzw ACC, vf12, vf28     412
  vu.acc.mula(Mask::xyzw, vu.vf12, vu.vf28.x());
  // nop                        |  madday.xyzw ACC, vf13, vf28    413
  vu.acc.madda(Mask::xyzw, vu.vf13, vu.vf28.y());
  // iaddi vi13, vi00, 0x0      |  clipw.xyz vf19, vf19           414
  cf = clip(vu.vf19, vu.vf19.w(), cf);
  vu.vi13 = 0;
  // iaddi vi12, vi00, 0x1      |  maxy.w vf16, vf16, vf03        415
  vu.vf16.max(Mask::w, vu.vf16, vu.vf03.y());
  vu.vi12 = 1;
  // lq.xyzw vf29, 245(vi05)    |  maddaz.xyzw ACC, vf14, vf28    416
  vu.acc.madda(Mask::xyzw, vu.vf14, vu.vf28.z());
  lq_buffer(Mask::xyzw, vu.vf29, vu.vi05 + 245);
  // iaddi vi01, vi00, 0x0      |  nop                            417
  vu.vi01 = 0;
  // BRANCH!
  // b L27                      |  maddw.xyzw vf30, vf15, vf00    418
  vu.acc.madd(Mask::xyzw, vu.vf30, vu.vf15, vu.vf00.w());
  bc = true;
  // iaddi vi05, vi05, 0x1      |  miniz.w vf16, vf16, vf03       419
  vu.vf16.mini(Mask::w, vu.vf16, vu.vf03.z());
  vu.vi05 = vu.vi05 + 1;
  if (bc) {
    goto L27;
  }

L25:
  // iand vi13, vi10, vi12      |  nop                            420
  vu.vi13 = vu.vi10 & vu.vi12;
  // BRANCH!
  // ibeq vi00, vi01, L26       |  mulaw.w ACC, vf30, vf00        421
  vu.acc.mula(Mask::w, vu.vf30, vu.vf00.w());
  bc = (vu.vi01 == 0);
  // lq.xyzw vf20, 317(vi03)    |  mula.xyz ACC, vf30, Q          422
  vu.acc.mula(Mask::xyz, vu.vf30, vu.Q);
  lq_buffer(Mask::xyzw, vu.vf20, vu.vi03 + 317);
  if (bc) {
    goto L26;
  }

  // nop                        |  addw.w vf17, vf17, vf03        423
  vu.vf17.add(Mask::w, vu.vf17, vu.vf03.w());
L26:
  // lq.xyzw vf22, 8(vi04)      |  maddw.xyzw vf16, vf02, vf00    424
  vu.acc.madd(Mask::xyzw, vu.vf16, vu.vf02, vu.vf00.w());
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi04 + 8);
  // fcand vi01, 0x3ffff        |  mul.xyzw vf19, vf31, vf01      425
  vu.vf19.mul(Mask::xyzw, vu.vf31, vu.vf01);
  // vu.vi01 = cf & 0x3ffff;
  fcand(vu.vi01, 0x3ffff, cf);

  // div Q, vf03.x, vf31.w      |  mul.xyzw vf20, vf20, Q         426
  vu.vf20.mul(Mask::xyzw, vu.vf20, vu.Q);
  vu.Q = vu.vf03.x() / vu.vf31.w();
  // ior vi01, vi01, vi13       |  ftoi4.xyzw vf17, vf17          427
  vu.vf17.ftoi4(Mask::xyzw, vu.vf17);
  vu.vi01 |= vu.vi13;
  // iadd vi12, vi12, vi12      |  maxy.w vf16, vf16, vf03        428
  vu.vf16.max(Mask::w, vu.vf16, vu.vf03.y());
  vu.vi12 = vu.vi12 + vu.vi12;
  // sq.xyzw vf21, 3(vi08)      |  mulax.xyzw ACC, vf12, vf28     429
  vu.acc.mula(Mask::xyzw, vu.vf12, vu.vf28.x());
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi08 + 3);
  // sq.xyzw vf23, 4(vi08)      |  madday.xyzw ACC, vf13, vf28    430
  vu.acc.madda(Mask::xyzw, vu.vf13, vu.vf28.y());
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi08 + 4);
  // sq.xyzw vf17, 5(vi08)      |  clipw.xyz vf19, vf19           431
  cf = clip(vu.vf19, vu.vf19.w(), cf);
  sq_buffer(Mask::xyzw, vu.vf17, vu.vi08 + 5);
  // lq.xyzw vf29, 245(vi05)    |  miniz.w vf16, vf16, vf03       432
  vu.vf16.mini(Mask::w, vu.vf16, vu.vf03.z());
  lq_buffer(Mask::xyzw, vu.vf29, vu.vi05 + 245);
  // iaddi vi08, vi08, 0x6      |  maddaz.xyzw ACC, vf14, vf28    433
  vu.acc.madda(Mask::xyzw, vu.vf14, vu.vf28.z());
  vu.vi08 = vu.vi08 + 6;
  // iaddi vi05, vi05, 0x1      |  maddw.xyzw vf30, vf15, vf00    434
  vu.acc.madd(Mask::xyzw, vu.vf30, vu.vf15, vu.vf00.w());
  vu.vi05 = vu.vi05 + 1;
L27:
  // BRANCH!
  // ibeq vi00, vi01, L28       |  mulaw.w ACC, vf31, vf00        435
  vu.acc.mula(Mask::w, vu.vf31, vu.vf00.w());
  bc = (vu.vi01 == 0);
  // lq.xyzw vf21, 318(vi03)    |  mula.xyz ACC, vf31, Q          436
  vu.acc.mula(Mask::xyz, vu.vf31, vu.Q);
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi03 + 318);
  if (bc) {
    goto L28;
  }

  // nop                        |  addw.w vf16, vf16, vf03        437
  vu.vf16.add(Mask::w, vu.vf16, vu.vf03.w());
L28:
  // lq.xyzw vf23, 20(vi04)     |  maddw.xyzw vf17, vf02, vf00    438
  vu.acc.madd(Mask::xyzw, vu.vf17, vu.vf02, vu.vf00.w());
  lq_buffer(Mask::xyzw, vu.vf23, vu.vi04 + 20);
  // fcand vi01, 0x3ffff        |  mul.xyzw vf18, vf30, vf01      439
  vu.vf18.mul(Mask::xyzw, vu.vf30, vu.vf01);
  // vu.vi01 = cf & 0x3ffff;
  fcand(vu.vi01, 0x3ffff, cf);

  // div Q, vf03.x, vf30.w      |  mul.xyzw vf21, vf21, Q         440
  vu.vf21.mul(Mask::xyzw, vu.vf21, vu.Q);
  vu.Q = vu.vf03.x() / vu.vf30.w();
  // iaddi vi03, vi03, 0x2      |  ftoi4.xyzw vf16, vf16          441
  vu.vf16.ftoi4(Mask::xyzw, vu.vf16);
  vu.vi03 = vu.vi03 + 2;
  // ior vi01, vi01, vi13       |  nop                            442
  vu.vi01 |= vu.vi13;
  // iaddi vi04, vi04, 0x1      |  maxy.w vf17, vf17, vf03        443
  vu.vf17.max(Mask::w, vu.vf17, vu.vf03.y());
  vu.vi04 = vu.vi04 + 1;
  // sq.xyzw vf20, 0(vi08)      |  mulax.xyzw ACC, vf12, vf29     444
  vu.acc.mula(Mask::xyzw, vu.vf12, vu.vf29.x());
  sq_buffer(Mask::xyzw, vu.vf20, vu.vi08);
  // sq.xyzw vf22, 1(vi08)      |  madday.xyzw ACC, vf13, vf29    445
  vu.acc.madda(Mask::xyzw, vu.vf13, vu.vf29.y());
  sq_buffer(Mask::xyzw, vu.vf22, vu.vi08 + 1);
  // sq.xyzw vf16, 2(vi08)      |  clipw.xyz vf18, vf18           446
  cf = clip(vu.vf18, vu.vf18.w(), cf);
  sq_buffer(Mask::xyzw, vu.vf16, vu.vi08 + 2);
  // lq.xyzw vf28, 236(vi05)    |  miniz.w vf17, vf17, vf03       447
  vu.vf17.mini(Mask::w, vu.vf17, vu.vf03.z());
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi05 + 236);
  // BRANCH!
  // ibgtz vi06, L25            |  maddaz.xyzw ACC, vf14, vf29    448
  vu.acc.madda(Mask::xyzw, vu.vf14, vu.vf29.z());
  bc = ((s16)vu.vi06) > 0;
  // iaddi vi06, vi06, -0x1     |  maddw.xyzw vf31, vf15, vf00    449
  vu.acc.madd(Mask::xyzw, vu.vf31, vu.vf15, vu.vf00.w());
  vu.vi06 = vu.vi06 + -1;
  if (bc) {
    goto L25;
  }

  // BRANCH!
  // ibeq vi00, vi01, L29       |  nop                            450
  bc = (vu.vi01 == 0);
  // nop                        |  nop                            451

  if (bc) {
    goto L29;
  }

  // nop                        |  addw.w vf17, vf17, vf03        452
  vu.vf17.add(Mask::w, vu.vf17, vu.vf03.w());
L29:
  // nop                        |  ftoi4.xyzw vf17, vf17          453
  vu.vf17.ftoi4(Mask::xyzw, vu.vf17);
  // sq.xyzw vf21, 3(vi08)      |  nop                            454
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi08 + 3);
  // fmt::print("maybe tex: {}\n", vu.vf21.print());
  // sq.xyzw vf23, 4(vi08)      |  nop                            455
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi08 + 4);
  // sq.xyzw vf17, 5(vi08)      |  nop                            456
  sq_buffer(Mask::xyzw, vu.vf17, vu.vi08 + 5);
  // jr vi15                    |  nop                            457
  // nop                        |  nop                            458
}

void OceanMid::xgkick(u16 addr,
                      SharedRenderState* render_state,
                      ScopedProfilerNode& prof,
                      DirectRenderer& direct) {
  direct.render_gif((const u8*)&m_vu_data[addr], UINT32_MAX, render_state, prof);
}

void OceanMid::run_call73_vu2c(SharedRenderState* render_state,
                               ScopedProfilerNode& prof,
                               DirectRenderer& direct) {

  bool bc;
JUMP_73:
  // xtop vi02                  |  nop                            73
  vu.vi02 = xtop();
  // lq.xyzw vf07, 747(vi00)    |  nop                            74
  lq_buffer(Mask::xyzw, vu.vf07, 747);
  // lq.xyzw vf08, 0(vi02)      |  nop                            75
  lq_buffer(Mask::xyzw, vu.vf08, vu.vi02);
  // lq.xyzw vf09, 1(vi02)      |  nop                            76
  lq_buffer(Mask::xyzw, vu.vf09, vu.vi02 + 1);
  // lq.xyzw vf10, 2(vi02)      |  nop                            77
  lq_buffer(Mask::xyzw, vu.vf10, vu.vi02 + 2);
  // lq.xyzw vf11, 3(vi02)      |  nop                            78
  lq_buffer(Mask::xyzw, vu.vf11, vu.vi02 + 3);
  // lq.xyzw vf12, 4(vi02)      |  nop                            79
  lq_buffer(Mask::xyzw, vu.vf12, vu.vi02 + 4);
  // lq.xyzw vf13, 5(vi02)      |  nop                            80
  lq_buffer(Mask::xyzw, vu.vf13, vu.vi02 + 5);
  // lq.xyzw vf14, 6(vi02)      |  nop                            81
  lq_buffer(Mask::xyzw, vu.vf14, vu.vi02 + 6);
  // lq.xyzw vf15, 7(vi02)      |  nop                            82
  lq_buffer(Mask::xyzw, vu.vf15, vu.vi02 + 7);
  // sq.xyzw vf07, 341(vi00)    |  nop                            83
  sq_buffer(Mask::xyzw, vu.vf07, 341);
  // sq.xyzw vf07, 463(vi00)    |  nop                            84
  sq_buffer(Mask::xyzw, vu.vf07, 463);
  // iaddi vi07, vi00, 0x7      |  nop                            85
  vu.vi07 = 7;
  // lq.xyzw vf04, 116(vi02)    |  nop                            86
  lq_buffer(Mask::xyzw, vu.vf04, vu.vi02 + 116);
L13:
  // iaddi vi01, vi07, -0x4     |  nop                            87
  vu.vi01 = vu.vi07 + -4;
  // mtir vi10, vf04.x          |  nop                            88
  vu.vi10 = vu.vf04.x_as_u16();
  // iaddiu vi11, vi00, 0xff    |  nop                            89
  vu.vi11 = 0xff; /* 255 */
  // BRANCH!
  // ibne vi00, vi01, L14       |  nop                            90
  bc = (vu.vi01 != 0);
  // mr32.xyzw vf04, vf04       |  nop                            91
  vu.vf04.mr32(Mask::xyzw, vu.vf04);
  if (bc) {
    goto L14;
  }

  // lq.xyzw vf04, 117(vi02)    |  nop                            92
  lq_buffer(Mask::xyzw, vu.vf04, vu.vi02 + 117);
L14:
  // BRANCH!
  // ibeq vi11, vi10, L15       |  nop                            93
  bc = (vu.vi11 == vu.vi10);
  // iaddi vi08, vi09, 0x7      |  nop                            94
  vu.vi08 = vu.vi09 + 7;
  if (bc) {
    goto L15;
  }

  // BRANCH!
  // bal vi15, L30              |  nop                            95
  // nop                        |  nop                            96

  run_L30_vu2c();

  // iaddiu vi01, vi00, 0x318   |  nop                            97
  vu.vi01 = 0x318; /* 792 */
  // xgkick vi09                |  nop                            98
  xgkick(vu.vi09, render_state, prof, direct);
  // BRANCH!
  // ibeq vi00, vi14, L15       |  nop                            99
  bc = (vu.vi14 == 0);
  // isub vi09, vi01, vi09      |  nop                            100
  vu.vi09 = vu.vi01 - vu.vi09;
  if (bc) {
    goto L15;
  }

  // BRANCH!
  // bal vi15, L36              |  nop                            101
  // nop                        |  nop                            102

  // if (bc) { goto L36; }
  run_L36_vu2c(render_state, prof, direct);

L15:
  // BRANCH!
  // ibgtz vi07, L13            |  nop                            103
  bc = ((s16)vu.vi07) > 0;
  // iaddi vi07, vi07, -0x1     |  nop                            104
  vu.vi07 = vu.vi07 + -1;
  if (bc) {
    goto L13;
  }

  // nop                        |  nop :e                         105

  // nop                        |  nop                            106
}

static inline REALLY_INLINE float erleng(const Vf& in) {
  float len = in.x() * in.x() + in.y() * in.y() + in.z() * in.z();
  float res = _mm_cvtss_f32(_mm_rsqrt_ss(_mm_set_ss(len)));
  return res;
}

void OceanMid::run_L30_vu2c() {
  u32 cf = 0;
  bool bc;
L30:
  // ilw.x vi05, 757(vi07)      |  nop                            459
  ilw_buffer(Mask::x, vu.vi05, vu.vi07 + 757);
  // ilw.y vi04, 757(vi07)      |  nop                            460
  ilw_buffer(Mask::y, vu.vi04, vu.vi07 + 757);
  // iaddi vi03, vi00, 0x0      |  nop                            461
  vu.vi03 = 0;
  // iadd vi04, vi04, vi02      |  nop                            462
  vu.vi04 = vu.vi04 + vu.vi02;
  // iaddi vi06, vi00, 0x8      |  nop                            463
  vu.vi06 = 8;
  // lq.xyzw vf28, 236(vi05)    |  nop                            464
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi05 + 236);
  // lq.xyzw vf29, 245(vi05)    |  mulax.xyzw ACC, vf12, vf28     465
  vu.acc.mula(Mask::xyzw, vu.vf12, vu.vf28.x());
  lq_buffer(Mask::xyzw, vu.vf29, vu.vi05 + 245);
  // iaddi vi05, vi05, 0x1      |  madday.xyzw ACC, vf13, vf28    466
  vu.acc.madda(Mask::xyzw, vu.vf13, vu.vf28.y());
  vu.vi05 = vu.vi05 + 1;
  // fcset 0x0                  |  maddaz.xyzw ACC, vf14, vf28    467
  vu.acc.madda(Mask::xyzw, vu.vf14, vu.vf28.z());
  cf = 0;
  // nop                        |  maddw.xyzw vf30, vf15, vf00    468
  vu.acc.madd(Mask::xyzw, vu.vf30, vu.vf15, vu.vf00.w());
  // nop                        |  mulax.xyzw ACC, vf08, vf28     469
  vu.acc.mula(Mask::xyzw, vu.vf08, vu.vf28.x());
  // nop                        |  madday.xyzw ACC, vf09, vf28    470
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf28.y());
  // nop                        |  maddaz.xyzw ACC, vf10, vf28    471
  vu.acc.madda(Mask::xyzw, vu.vf10, vu.vf28.z());
  // nop                        |  maddw.xyzw vf26, vf11, vf00    472
  vu.acc.madd(Mask::xyzw, vu.vf26, vu.vf11, vu.vf00.w());
  // lq.xyzw vf20, 317(vi03)    |  mulax.xyzw ACC, vf12, vf29     473
  vu.acc.mula(Mask::xyzw, vu.vf12, vu.vf29.x());
  lq_buffer(Mask::xyzw, vu.vf20, vu.vi03 + 317);
  // lq.xyzw vf22, 8(vi04)      |  madday.xyzw ACC, vf13, vf29    474
  vu.acc.madda(Mask::xyzw, vu.vf13, vu.vf29.y());
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi04 + 8);
  // div Q, vf03.x, vf30.w      |  maddaz.xyzw ACC, vf14, vf29    475
  vu.acc.madda(Mask::xyzw, vu.vf14, vu.vf29.z());
  vu.Q = vu.vf03.x() / vu.vf30.w();
  // erleng.xyz P, vf26         |  maddw.xyzw vf31, vf15, vf00    476
  vu.acc.madd(Mask::xyzw, vu.vf31, vu.vf15, vu.vf00.w());
  vu.P = erleng(vu.vf26); /* TODO erleng */
  // nop                        |  mulax.xyzw ACC, vf08, vf29     477
  vu.acc.mula(Mask::xyzw, vu.vf08, vu.vf29.x());
  // nop                        |  madday.xyzw ACC, vf09, vf29    478
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf29.y());
  // lq.xyzw vf28, 236(vi05)    |  maddaz.xyzw ACC, vf10, vf29    479
  vu.acc.madda(Mask::xyzw, vu.vf10, vu.vf29.z());
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi05 + 236);
  // lq.xyzw vf29, 245(vi05)    |  maddw.xyzw vf27, vf11, vf00    480
  vu.acc.madd(Mask::xyzw, vu.vf27, vu.vf11, vu.vf00.w());
  lq_buffer(Mask::xyzw, vu.vf29, vu.vi05 + 245);
  // nop                        |  mul.xyzw vf18, vf30, vf01      481
  vu.vf18.mul(Mask::xyzw, vu.vf30, vu.vf01);
  // nop                        |  mul.xyzw vf19, vf31, vf01      482
  vu.vf19.mul(Mask::xyzw, vu.vf31, vu.vf01);
  // nop                        |  mulaw.w ACC, vf30, vf00        483
  vu.acc.mula(Mask::w, vu.vf30, vu.vf00.w());
  // nop                        |  mula.xyz ACC, vf30, Q          484
  vu.acc.mula(Mask::xyz, vu.vf30, vu.Q);
  // nop                        |  maddw.xyzw vf16, vf02, vf00    485
  vu.acc.madd(Mask::xyzw, vu.vf16, vu.vf02, vu.vf00.w());
  // div Q, vf03.x, vf31.w      |  mul.xyzw vf20, vf20, Q         486
  vu.vf20.mul(Mask::xyzw, vu.vf20, vu.Q);
  vu.Q = vu.vf03.x() / vu.vf31.w();
  // nop                        |  addz.y vf26, vf00, vf26        487
  vu.vf26.add(Mask::y, vu.vf00, vu.vf26.z());
  // waitp                      |  maxy.w vf16, vf16, vf03        488
  vu.vf16.max(Mask::w, vu.vf16, vu.vf03.y());
  // ASSERT(false);
  // mfp.w vf26, P              |  mulax.xyzw ACC, vf12, vf28     489
  vu.acc.mula(Mask::xyzw, vu.vf12, vu.vf28.x());
  vu.vf26.mfp(Mask::w, vu.P);
  // erleng.xyz P, vf27         |  madday.xyzw ACC, vf13, vf28    490
  vu.acc.madda(Mask::xyzw, vu.vf13, vu.vf28.y());
  vu.P = erleng(vu.vf27); /* TODO erleng */
  // iaddi vi12, vi00, 0x1      |  maddaz.xyzw ACC, vf14, vf28    491
  vu.acc.madda(Mask::xyzw, vu.vf14, vu.vf28.z());
  vu.vi12 = 1;
  // iaddi vi01, vi00, 0x0      |  maddw.xyzw vf30, vf15, vf00    492
  vu.acc.madd(Mask::xyzw, vu.vf30, vu.vf15, vu.vf00.w());
  vu.vi01 = 0;
  // iaddi vi13, vi00, 0x0      |  mulw.xy vf24, vf26, vf26       493
  vu.vf24.mul(Mask::xy, vu.vf26, vu.vf26.w());
  vu.vi13 = 0;
  // iaddi vi11, vi00, 0x0      |  miniz.w vf16, vf16, vf03       494
  vu.vf16.mini(Mask::w, vu.vf16, vu.vf03.z());
  vu.vi11 = 0;
  // iaddi vi14, vi00, 0x0      |  addz.y vf27, vf00, vf27        495
  vu.vf27.add(Mask::y, vu.vf00, vu.vf27.z());
  vu.vi14 = 0;
  // nop                        |  mulax.xyzw ACC, vf08, vf28     496
  vu.acc.mula(Mask::xyzw, vu.vf08, vu.vf28.x());
  // nop                        |  madday.xyzw ACC, vf09, vf28    497
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf28.y());
  // nop                        |  maddaz.xyzw ACC, vf10, vf28    498
  vu.acc.madda(Mask::xyzw, vu.vf10, vu.vf28.z());
  // nop                        |  maddw.xyzw vf26, vf11, vf00    499
  vu.acc.madd(Mask::xyzw, vu.vf26, vu.vf11, vu.vf00.w());
  // nop                        |  clipw.xyz vf18, vf18           500
  // ASSERT(false);
  cf = clip(vu.vf18, vu.vf18.w(), cf);
  // waitp                      |  clipw.xyz vf19, vf19           501
  // ASSERT(false);
  cf = clip(vu.vf19, vu.vf19.w(), cf);
  // ASSERT(false);
  // BRANCH!
  // b L33                      |  mula.xyzw ACC, vf24, vf05      502
  vu.acc.mula(Mask::xyzw, vu.vf24, vu.vf05);
  bc = true;
  // iaddi vi05, vi05, 0x1      |  maddw.xyzw vf24, vf06, vf00    503
  vu.acc.madd(Mask::xyzw, vu.vf24, vu.vf06, vu.vf00.w());
  vu.vi05 = vu.vi05 + 1;
  if (bc) {
    goto L33;
  }

L31:
  // BRANCH!
  // ibeq vi00, vi01, L32       |  mulaw.w ACC, vf30, vf00        504
  vu.acc.mula(Mask::w, vu.vf30, vu.vf00.w());
  bc = (vu.vi01 == 0);
  // lq.xyzw vf20, 317(vi03)    |  mula.xyz ACC, vf30, Q          505
  // ASSERT(false);
  vu.acc.mula(Mask::xyz, vu.vf30, vu.Q);
  lq_buffer(Mask::xyzw, vu.vf20, vu.vi03 + 317);
  if (bc) {
    goto L32;
  }

  // BRANCH!
  // ibne vi00, vi13, L32       |  addw.w vf17, vf17, vf03        506
  vu.vf17.add(Mask::w, vu.vf17, vu.vf03.w());
  bc = (vu.vi13 != 0);
  // fcor vi01, 0x7df7ff        |  nop                            507
  // ASSERT(false);
  fcor(vu.vi01, 0x7df7ff, cf);
  if (bc) {
    goto L32;
  }

  // isw.x vi12, 775(vi14)      |  nop                            508
  isw_buffer(Mask::x, vu.vi12, vu.vi14 + 775);
  // BRANCH!
  // ibne vi00, vi01, L32       |  nop                            509
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xdf7dff        |  nop                            510
  // ASSERT(false);
  fcor(vu.vi01, 0xdf7dff, cf);
  if (bc) {
    goto L32;
  }

  // isw.y vi05, 775(vi14)      |  nop                            511
  isw_buffer(Mask::y, vu.vi05, vu.vi14 + 775);
  // BRANCH!
  // ibne vi00, vi01, L32       |  nop                            512
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xefbeff        |  nop                            513
  // ASSERT(false);
  fcor(vu.vi01, 0xefbeff, cf);
  if (bc) {
    goto L32;
  }

  // ilw.z vi11, 757(vi07)      |  nop                            514
  ilw_buffer(Mask::z, vu.vi11, vu.vi07 + 757);
  // BRANCH!
  // ibne vi00, vi01, L32       |  nop                            515
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xf7df7f        |  nop                            516
  // ASSERT(false);
  fcor(vu.vi01, 0xf7df7f, cf);

  if (bc) {
    goto L32;
  }

  // isw.z vi04, 775(vi14)      |  nop                            517
  isw_buffer(Mask::z, vu.vi04, vu.vi14 + 775);
  // BRANCH!
  // ibne vi00, vi01, L32       |  nop                            518
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xfbefbf        |  nop                            519
  // ASSERT(false);
  fcor(vu.vi01, 0xfbefbf, cf);

  if (bc) {
    goto L32;
  }

  // isub vi11, vi05, vi11      |  nop                            520
  vu.vi11 = vu.vi05 - vu.vi11;
  // BRANCH!
  // ibne vi00, vi01, L32       |  nop                            521
  bc = (vu.vi01 != 0);
  // nop                        |  nop                            522

  if (bc) {
    goto L32;
  }

  // BRANCH!
  // ibltz vi11, L32            |  nop                            523
  bc = ((s16)vu.vi11) < 0;
  // nop                        |  nop                            524

  if (bc) {
    goto L32;
  }

  // iaddi vi14, vi14, 0x1      |  nop                            525
  vu.vi14 = vu.vi14 + 1;
L32:
  // lq.xyzw vf22, 8(vi04)      |  maddw.xyzw vf16, vf02, vf00    526
  vu.acc.madd(Mask::xyzw, vu.vf16, vu.vf02, vu.vf00.w());
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi04 + 8);
  // div Q, vf03.x, vf31.w      |  mul.xyzw vf20, vf20, Q         527
  vu.vf20.mul(Mask::xyzw, vu.vf20, vu.Q);
  vu.Q = vu.vf03.x() / vu.vf31.w();
  // fcand vi01, 0x3ffff        |  mul.xyzw vf19, vf31, vf01      528
  vu.vf19.mul(Mask::xyzw, vu.vf31, vu.vf01);
  // ASSERT(false);
  // vu.vi01 = cf & 0x3ffff;
  fcand(vu.vi01, 0x3ffff, cf);

  // waitp                      |  ftoi4.xyzw vf17, vf17          529
  vu.vf17.ftoi4(Mask::xyzw, vu.vf17);
  // ASSERT(false);
  // mfp.w vf26, P              |  maxy.w vf16, vf16, vf03        530
  vu.vf16.max(Mask::w, vu.vf16, vu.vf03.y());
  vu.vf26.mfp(Mask::w, vu.P);
  // erleng.xyz P, vf27         |  clipw.xyz vf19, vf19           531
  // ASSERT(false);
  cf = clip(vu.vf19, vu.vf19.w(), cf);
  vu.P = erleng(vu.vf27); /* TODO erleng */
  // iand vi13, vi10, vi12      |  addz.y vf27, vf00, vf27        532
  vu.vf27.add(Mask::y, vu.vf00, vu.vf27.z());
  vu.vi13 = vu.vi10 & vu.vi12;
  // sq.xyzw vf21, 3(vi08)      |  mulz.xyzw vf25, vf25, vf21     533
  vu.vf25.z() = 1; // HACK
  vu.vf25.mul(Mask::xyzw, vu.vf25, vu.vf21.z()); // NaN's on input to here
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi08 + 3);
  // sq.xyzw vf23, 4(vi08)      |  mulw.xy vf24, vf26, vf26       534
  vu.vf24.mul(Mask::xy, vu.vf26, vu.vf26.w());
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi08 + 4);
  // sq.xyzw vf17, 5(vi08)      |  mulax.xyzw ACC, vf12, vf28     535
  vu.acc.mula(Mask::xyzw, vu.vf12, vu.vf28.x());
  sq_buffer(Mask::xyzw, vu.vf17, vu.vi08 + 5);
  // sq.xyzw vf17, 66(vi08)     |  madday.xyzw ACC, vf13, vf28    536
  vu.acc.madda(Mask::xyzw, vu.vf13, vu.vf28.y());
  sq_buffer(Mask::xyzw, vu.vf17, vu.vi08 + 66);
  // sq.xyzw vf25, 64(vi08)     |  maddaz.xyzw ACC, vf14, vf28    537
  vu.acc.madda(Mask::xyzw, vu.vf14, vu.vf28.z());
  sq_buffer(Mask::xyzw, vu.vf25, vu.vi08 + 64);
  // iaddi vi08, vi08, 0x6      |  maddw.xyzw vf30, vf15, vf00    538
  vu.acc.madd(Mask::xyzw, vu.vf30, vu.vf15, vu.vf00.w());
  vu.vi08 = vu.vi08 + 6;
  // iadd vi12, vi12, vi12      |  mulax.xyzw ACC, vf08, vf28     539
  vu.acc.mula(Mask::xyzw, vu.vf08, vu.vf28.x());
  vu.vi12 = vu.vi12 + vu.vi12;
  // ior vi01, vi01, vi13       |  madday.xyzw ACC, vf09, vf28    540
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf28.y());
  // ASSERT(false);
  vu.vi01 |= vu.vi13;
  // nop                        |  maddaz.xyzw ACC, vf10, vf28    541
  vu.acc.madda(Mask::xyzw, vu.vf10, vu.vf28.z());
  // nop                        |  maddw.xyzw vf26, vf11, vf00    542
  vu.acc.madd(Mask::xyzw, vu.vf26, vu.vf11, vu.vf00.w());
  // lq.xyzw vf29, 245(vi05)    |  miniz.w vf16, vf16, vf03       543
  vu.vf16.mini(Mask::w, vu.vf16, vu.vf03.z());
  lq_buffer(Mask::xyzw, vu.vf29, vu.vi05 + 245);
  // iaddi vi05, vi05, 0x1      |  mula.xyzw ACC, vf24, vf05      544
  vu.acc.mula(Mask::xyzw, vu.vf24, vu.vf05);
  vu.vi05 = vu.vi05 + 1;
  // nop                        |  maddw.xyzw vf24, vf06, vf00    545
  vu.acc.madd(Mask::xyzw, vu.vf24, vu.vf06, vu.vf00.w());
L33:
  // BRANCH!
  // ibeq vi00, vi01, L34       |  mulaw.w ACC, vf31, vf00        546
  vu.acc.mula(Mask::w, vu.vf31, vu.vf00.w());
  bc = (vu.vi01 == 0);
  // lq.xyzw vf21, 318(vi03)    |  mula.xyz ACC, vf31, Q          547
  // ASSERT(false);
  vu.acc.mula(Mask::xyz, vu.vf31, vu.Q);
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi03 + 318);
  if (bc) {
    goto L34;
  }

  // BRANCH!
  // ibne vi00, vi13, L34       |  addw.w vf16, vf16, vf03        548
  vu.vf16.add(Mask::w, vu.vf16, vu.vf03.w());
  bc = (vu.vi13 != 0);
  // fcor vi01, 0x7df7ff        |  nop                            549
  // ASSERT(false);
  fcor(vu.vi01, 0x7df7ff, cf);
  if (bc) {
    goto L34;
  }

  // isw.x vi00, 775(vi14)      |  nop                            550
  isw_buffer(Mask::x, 0, vu.vi14 + 775);
  // BRANCH!
  // ibne vi00, vi01, L34       |  nop                            551
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xdf7dff        |  nop                            552
  // ASSERT(false);
  fcor(vu.vi01, 0xdf7dff, cf);

  if (bc) {
    goto L34;
  }

  // isw.y vi05, 775(vi14)      |  nop                            553
  isw_buffer(Mask::y, vu.vi05, vu.vi14 + 775);
  // BRANCH!
  // ibne vi00, vi01, L34       |  nop                            554
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xefbeff        |  nop                            555
  // ASSERT(false);
  fcor(vu.vi01, 0xefbeff, cf);

  if (bc) {
    goto L34;
  }

  // isw.z vi04, 775(vi14)      |  nop                            556
  isw_buffer(Mask::z, vu.vi04, vu.vi14 + 775);
  // BRANCH!
  // ibne vi00, vi01, L34       |  nop                            557
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xf7df7f        |  nop                            558
  // ASSERT(false);
  fcor(vu.vi01, 0xf7df7f, cf);

  if (bc) {
    goto L34;
  }

  // nop                        |  nop                            559

  // BRANCH!
  // ibne vi00, vi01, L34       |  nop                            560
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xfbefbf        |  nop                            561
  // ASSERT(false);
  fcor(vu.vi01, 0xfbefbf, cf);

  if (bc) {
    goto L34;
  }

  // nop                        |  nop                            562

  // BRANCH!
  // ibne vi00, vi01, L34       |  nop                            563
  bc = (vu.vi01 != 0);
  // nop                        |  nop                            564

  if (bc) {
    goto L34;
  }

  // iaddi vi14, vi14, 0x1      |  nop                            565
  vu.vi14 = vu.vi14 + 1;
L34:
  // lq.xyzw vf23, 20(vi04)     |  maddw.xyzw vf17, vf02, vf00    566
  vu.acc.madd(Mask::xyzw, vu.vf17, vu.vf02, vu.vf00.w());
  lq_buffer(Mask::xyzw, vu.vf23, vu.vi04 + 20);
  // div Q, vf03.x, vf30.w      |  mul.xyzw vf21, vf21, Q         567
  vu.vf21.mul(Mask::xyzw, vu.vf21, vu.Q);
  vu.Q = vu.vf03.x() / vu.vf30.w();
  // fcand vi01, 0x3ffff        |  mul.xyzw vf18, vf30, vf01      568
  vu.vf18.mul(Mask::xyzw, vu.vf30, vu.vf01);
  // ASSERT(false);
  // vu.vi01 = cf & 0x3ffff;
  fcand(vu.vi01, 0x3ffff, cf);

  // waitp                      |  ftoi4.xyzw vf16, vf16          569
  vu.vf16.ftoi4(Mask::xyzw, vu.vf16);
  // ASSERT(false);
  // mfp.w vf27, P              |  maxy.w vf17, vf17, vf03        570
  vu.vf17.max(Mask::w, vu.vf17, vu.vf03.y());
  vu.vf27.mfp(Mask::w, vu.P);
  // erleng.xyz P, vf26         |  clipw.xyz vf18, vf18           571
  // ASSERT(false);
  cf = clip(vu.vf18, vu.vf18.w(), cf);
  vu.P = erleng(vu.vf26); /* TODO erleng */
  // nop                        |  addz.y vf26, vf00, vf26        572
  vu.vf26.add(Mask::y, vu.vf00, vu.vf26.z());
  // sq.xyzw vf20, 0(vi08)      |  mulz.xyzw vf24, vf24, vf20     573
  vu.vf24.z() = 1.; // HACK
  vu.vf24.mul(Mask::xyzw, vu.vf24, vu.vf20.z());
  sq_buffer(Mask::xyzw, vu.vf20, vu.vi08);
  // sq.xyzw vf22, 1(vi08)      |  mulw.xy vf25, vf27, vf27       574
  vu.vf25.mul(Mask::xy, vu.vf27, vu.vf27.w());
  sq_buffer(Mask::xyzw, vu.vf22, vu.vi08 + 1);
  // sq.xyzw vf16, 2(vi08)      |  mulax.xyzw ACC, vf12, vf29     575
  vu.acc.mula(Mask::xyzw, vu.vf12, vu.vf29.x());
  sq_buffer(Mask::xyzw, vu.vf16, vu.vi08 + 2);
  // sq.xyzw vf16, 63(vi08)     |  madday.xyzw ACC, vf13, vf29    576
  vu.acc.madda(Mask::xyzw, vu.vf13, vu.vf29.y());
  sq_buffer(Mask::xyzw, vu.vf16, vu.vi08 + 63);
  // sq.xyzw vf24, 61(vi08)     |  maddaz.xyzw ACC, vf14, vf29    577
  vu.acc.madda(Mask::xyzw, vu.vf14, vu.vf29.z());
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi08 + 61);
  // iaddi vi04, vi04, 0x1      |  maddw.xyzw vf31, vf15, vf00    578
  vu.acc.madd(Mask::xyzw, vu.vf31, vu.vf15, vu.vf00.w());
  vu.vi04 = vu.vi04 + 1;
  // iaddi vi03, vi03, 0x2      |  mulax.xyzw ACC, vf08, vf29     579
  vu.acc.mula(Mask::xyzw, vu.vf08, vu.vf29.x());
  vu.vi03 = vu.vi03 + 2;
  // ior vi01, vi01, vi13       |  madday.xyzw ACC, vf09, vf29    580
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf29.y());
  // ASSERT(false);
  vu.vi01 |= vu.vi13;
  // nop                        |  maddaz.xyzw ACC, vf10, vf29    581
  vu.acc.madda(Mask::xyzw, vu.vf10, vu.vf29.z());
  // nop                        |  maddw.xyzw vf27, vf11, vf00    582
  vu.acc.madd(Mask::xyzw, vu.vf27, vu.vf11, vu.vf00.w());
  // lq.xyzw vf28, 236(vi05)    |  miniz.w vf17, vf17, vf03       583
  vu.vf17.mini(Mask::w, vu.vf17, vu.vf03.z());
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi05 + 236);
  // BRANCH!
  // ibgtz vi06, L31            |  mula.xyzw ACC, vf25, vf05      584
  vu.acc.mula(Mask::xyzw, vu.vf25, vu.vf05);
  bc = ((s16)vu.vi06) > 0;
  // iaddi vi06, vi06, -0x1     |  maddw.xyzw vf25, vf06, vf00    585
  vu.acc.madd(Mask::xyzw, vu.vf25, vu.vf06, vu.vf00.w());
  vu.vi06 = vu.vi06 + -1;
  if (bc) {
    goto L31;
  }

  // BRANCH!
  // ibeq vi00, vi01, L35       |  nop                            586
  bc = (vu.vi01 == 0);
  // nop                        |  nop                            587

  if (bc) {
    goto L35;
  }

  // BRANCH!
  // ibne vi00, vi13, L35       |  addw.w vf17, vf17, vf03        588
  vu.vf17.add(Mask::w, vu.vf17, vu.vf03.w());
  bc = (vu.vi13 != 0);
  // fcor vi01, 0x7df7ff        |  nop                            589
  // ASSERT(false);
  fcor(vu.vi01, 0x7df7ff, cf);
  if (bc) {
    goto L35;
  }

  // isw.x vi12, 775(vi14)      |  nop                            590
  isw_buffer(Mask::x, vu.vi12, vu.vi14 + 775);
  // BRANCH!
  // ibne vi00, vi01, L35       |  nop                            591
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xdf7dff        |  nop                            592
  // ASSERT(false);
  fcor(vu.vi01, 0xdf7dff, cf);

  if (bc) {
    goto L35;
  }

  // isw.y vi05, 775(vi14)      |  nop                            593
  isw_buffer(Mask::y, vu.vi05, vu.vi14 + 775);
  // BRANCH!
  // ibne vi00, vi01, L35       |  nop                            594
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xefbeff        |  nop                            595
  // ASSERT(false);
  fcor(vu.vi01, 0xefbeff, cf);

  if (bc) {
    goto L35;
  }

  // isw.z vi04, 775(vi14)      |  nop                            596
  isw_buffer(Mask::z, vu.vi04, vu.vi14 + 775);
  // BRANCH!
  // ibne vi00, vi01, L35       |  nop                            597
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xf7df7f        |  nop                            598
  // ASSERT(false);
  fcor(vu.vi01, 0xf7df7f, cf);

  if (bc) {
    goto L35;
  }

  // nop                        |  nop                            599

  // BRANCH!
  // ibne vi00, vi01, L35       |  nop                            600
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xfbefbf        |  nop                            601
  // ASSERT(false);
  fcor(vu.vi01, 0xfbefbf, cf);

  if (bc) {
    goto L35;
  }

  // nop                        |  nop                            602

  // BRANCH!
  // ibne vi00, vi01, L35       |  nop                            603
  bc = (vu.vi01 != 0);
  // nop                        |  nop                            604

  if (bc) {
    goto L35;
  }

  // iaddi vi14, vi14, 0x1      |  nop                            605
  vu.vi14 = vu.vi14 + 1;
L35:
  // nop                        |  ftoi4.xyzw vf17, vf17          606
  vu.vf17.ftoi4(Mask::xyzw, vu.vf17);
  // sq.xyzw vf21, 3(vi08)      |  mulz.xyzw vf25, vf25, vf21     607
  vu.vf25.mul(Mask::xyzw, vu.vf25, vu.vf21.z());
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi08 + 3);
  // sq.xyzw vf23, 4(vi08)      |  nop                            608
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi08 + 4);
  // sq.xyzw vf17, 5(vi08)      |  nop                            609
  sq_buffer(Mask::xyzw, vu.vf17, vu.vi08 + 5);
  // sq.xyzw vf17, 66(vi08)     |  nop                            610
  sq_buffer(Mask::xyzw, vu.vf17, vu.vi08 + 66);
  // sq.xyzw vf25, 64(vi08)     |  nop                            611
  sq_buffer(Mask::xyzw, vu.vf25, vu.vi08 + 64);
  // jr vi15                    |  nop                            612
  // nop                        |  nop                            613
}

void OceanMid::run_L36_vu2c(SharedRenderState* render_state,
                            ScopedProfilerNode& prof,
                            DirectRenderer& direct) {
  bool bc;
  fmt::print("run L36\n");
L36:
  // lq.xyzw vf31, 734(vi00)    |  nop                            614
  lq_buffer(Mask::xyzw, vu.vf31, 734);
  // iaddiu vi10, vi00, 0x243   |  nop                            615
  vu.vi10 = 0x243; /* 579 */
L37:
  // iaddi vi14, vi14, -0x1     |  nop                            616
  vu.vi14 = vu.vi14 + -1;
  // iaddi vi08, vi10, 0x7      |  nop                            617
  vu.vi08 = vu.vi10 + 7;
  // ilw.x vi12, 775(vi14)      |  nop                            618
  ilw_buffer(Mask::x, vu.vi12, vu.vi14 + 775);
  // ilw.y vi05, 775(vi14)      |  nop                            619
  ilw_buffer(Mask::y, vu.vi05, vu.vi14 + 775);
  // ilw.z vi04, 775(vi14)      |  nop                            620
  ilw_buffer(Mask::z, vu.vi04, vu.vi14 + 775);
  // BRANCH!
  // ibne vi00, vi12, L38       |  nop                            621
  bc = (vu.vi12 != 0);
  // nop                        |  nop                            622

  if (bc) {
    goto L38;
  }

  // lq.xyzw vf28, 233(vi05)    |  nop                            623
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi05 + 233);
  // lq.xyzw vf29, 242(vi05)    |  nop                            624
  lq_buffer(Mask::xyzw, vu.vf29, vu.vi05 + 242);
  // lq.xyzw vf30, 234(vi05)    |  nop                            625
  lq_buffer(Mask::xyzw, vu.vf30, vu.vi05 + 234);
  // lq.xyzw vf22, 7(vi04)      |  nop                            626
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi04 + 7);
  // lq.xyzw vf23, 19(vi04)     |  mulax.xyzw ACC, vf08, vf28     627
  vu.acc.mula(Mask::xyzw, vu.vf08, vu.vf28.x());
  lq_buffer(Mask::xyzw, vu.vf23, vu.vi04 + 19);
  // lq.xyzw vf16, 8(vi04)      |  madday.xyzw ACC, vf09, vf28    628
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf28.y());
  lq_buffer(Mask::xyzw, vu.vf16, vu.vi04 + 8);
  // lq.xyzw vf20, 317(vi00)    |  maddaz.xyzw ACC, vf10, vf28    629
  vu.acc.madda(Mask::xyzw, vu.vf10, vu.vf28.z());
  lq_buffer(Mask::xyzw, vu.vf20, 317);
  // lq.xyzw vf21, 318(vi00)    |  maddw.xyzw vf24, vf11, vf00    630
  vu.acc.madd(Mask::xyzw, vu.vf24, vu.vf11, vu.vf00.w());
  lq_buffer(Mask::xyzw, vu.vf21, 318);
  // BRANCH!
  // b L39                      |  mulax.xyzw ACC, vf12, vf28     631
  vu.acc.mula(Mask::xyzw, vu.vf12, vu.vf28.x());
  bc = true;
  // lq.xyzw vf07, 319(vi00)    |  madday.xyzw ACC, vf13, vf28    632
  vu.acc.madda(Mask::xyzw, vu.vf13, vu.vf28.y());
  lq_buffer(Mask::xyzw, vu.vf07, 319);
  if (bc) {
    goto L39;
  }

L38:
  // lq.xyzw vf28, 242(vi05)    |  nop                            633
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi05 + 242);
  // lq.xyzw vf29, 234(vi05)    |  nop                            634
  lq_buffer(Mask::xyzw, vu.vf29, vu.vi05 + 234);
  // lq.xyzw vf30, 243(vi05)    |  nop                            635
  lq_buffer(Mask::xyzw, vu.vf30, vu.vi05 + 243);
  // lq.xyzw vf22, 18(vi04)     |  nop                            636
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi04 + 18);
  // lq.xyzw vf23, 7(vi04)      |  mulax.xyzw ACC, vf08, vf28     637
  vu.acc.mula(Mask::xyzw, vu.vf08, vu.vf28.x());
  lq_buffer(Mask::xyzw, vu.vf23, vu.vi04 + 7);
  // lq.xyzw vf16, 19(vi04)     |  madday.xyzw ACC, vf09, vf28    638
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf28.y());
  lq_buffer(Mask::xyzw, vu.vf16, vu.vi04 + 19);
  // lq.xyzw vf20, 318(vi00)    |  maddaz.xyzw ACC, vf10, vf28    639
  vu.acc.madda(Mask::xyzw, vu.vf10, vu.vf28.z());
  lq_buffer(Mask::xyzw, vu.vf20, 318);
  // lq.xyzw vf21, 319(vi00)    |  maddw.xyzw vf24, vf11, vf00    640
  vu.acc.madd(Mask::xyzw, vu.vf24, vu.vf11, vu.vf00.w());
  lq_buffer(Mask::xyzw, vu.vf21, 319);
  // nop                        |  mulax.xyzw ACC, vf12, vf28     641
  vu.acc.mula(Mask::xyzw, vu.vf12, vu.vf28.x());
  // lq.xyzw vf07, 320(vi00)    |  madday.xyzw ACC, vf13, vf28    642
  vu.acc.madda(Mask::xyzw, vu.vf13, vu.vf28.y());
  lq_buffer(Mask::xyzw, vu.vf07, 320);
L39:
  // nop                        |  maddaz.xyzw ACC, vf14, vf28    643
  vu.acc.madda(Mask::xyzw, vu.vf14, vu.vf28.z());
  // erleng.xyz P, vf24         |  maddw.xyzw vf28, vf15, vf00    644
  vu.acc.madd(Mask::xyzw, vu.vf28, vu.vf15, vu.vf00.w());
  vu.P = erleng(vu.vf24); /* TODO erleng */
  // nop                        |  addz.y vf24, vf00, vf24        645
  vu.vf24.add(Mask::y, vu.vf00, vu.vf24.z());
  // nop                        |  itof0.xyzw vf22, vf22          646
  vu.vf22.itof0(Mask::xyzw, vu.vf22);
  // nop                        |  itof0.xyzw vf23, vf23          647
  vu.vf23.itof0(Mask::xyzw, vu.vf23);
  // nop                        |  itof0.xyzw vf16, vf16          648
  vu.vf16.itof0(Mask::xyzw, vu.vf16);
  // sq.xyzw vf20, 0(vi08)      |  mulax.xyzw ACC, vf08, vf29     649
  vu.acc.mula(Mask::xyzw, vu.vf08, vu.vf29.x());
  sq_buffer(Mask::xyzw, vu.vf20, vu.vi08);
  // sq.xyzw vf22, 1(vi08)      |  madday.xyzw ACC, vf09, vf29    650
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf29.y());
  sq_buffer(Mask::xyzw, vu.vf22, vu.vi08 + 1);
  // sq.xyzw vf21, 3(vi08)      |  maddaz.xyzw ACC, vf10, vf29    651
  vu.acc.madda(Mask::xyzw, vu.vf10, vu.vf29.z());
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi08 + 3);
  // sq.xyzw vf23, 4(vi08)      |  maddw.xyzw vf25, vf11, vf00    652
  vu.acc.madd(Mask::xyzw, vu.vf25, vu.vf11, vu.vf00.w());
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi08 + 4);
  // sq.xyzw vf07, 6(vi08)      |  mulax.xyzw ACC, vf12, vf29     653
  vu.acc.mula(Mask::xyzw, vu.vf12, vu.vf29.x());
  sq_buffer(Mask::xyzw, vu.vf07, vu.vi08 + 6);
  // sq.xyzw vf16, 7(vi08)      |  madday.xyzw ACC, vf13, vf29    654
  vu.acc.madda(Mask::xyzw, vu.vf13, vu.vf29.y());
  sq_buffer(Mask::xyzw, vu.vf16, vu.vi08 + 7);
  // sq.xyzw vf20, 9(vi08)      |  maddaz.xyzw ACC, vf14, vf29    655
  vu.acc.madda(Mask::xyzw, vu.vf14, vu.vf29.z());
  sq_buffer(Mask::xyzw, vu.vf20, vu.vi08 + 9);
  // sq.xyzw vf22, 10(vi08)     |  maddw.xyzw vf29, vf15, vf00    656
  vu.acc.madd(Mask::xyzw, vu.vf29, vu.vf15, vu.vf00.w());
  sq_buffer(Mask::xyzw, vu.vf22, vu.vi08 + 10);
  // waitp                      |  mul.xyzw vf28, vf28, vf01      657
  vu.vf28.mul(Mask::xyzw, vu.vf28, vu.vf01);
  // ASSERT(false);
  // mfp.w vf24, P              |  nop                            658
  vu.vf24.mfp(Mask::w, vu.P);
  // erleng.xyz P, vf25         |  nop                            659
  vu.P = erleng(vu.vf25); /* TODO erleng */
  // nop                        |  addz.y vf25, vf00, vf25        660
  vu.vf25.add(Mask::y, vu.vf00, vu.vf25.z());
  // sq.xyzw vf28, 2(vi08)      |  mulax.xyzw ACC, vf08, vf30     661
  vu.acc.mula(Mask::xyzw, vu.vf08, vu.vf30.x());
  sq_buffer(Mask::xyzw, vu.vf28, vu.vi08 + 2);
  // sq.xyzw vf28, 11(vi08)     |  mulw.xy vf24, vf24, vf24       662
  vu.vf24.mul(Mask::xy, vu.vf24, vu.vf24.w());
  sq_buffer(Mask::xyzw, vu.vf28, vu.vi08 + 11);
  // sq.xyzw vf28, 772(vi00)    |  madday.xyzw ACC, vf09, vf30    663
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf30.y());
  sq_buffer(Mask::xyzw, vu.vf28, 772);
  // nop                        |  maddaz.xyzw ACC, vf10, vf30    664
  vu.acc.madda(Mask::xyzw, vu.vf10, vu.vf30.z());
  // nop                        |  maddw.xyzw vf26, vf11, vf00    665
  vu.acc.madd(Mask::xyzw, vu.vf26, vu.vf11, vu.vf00.w());
  // nop                        |  mula.xyzw ACC, vf24, vf05      666
  vu.acc.mula(Mask::xyzw, vu.vf24, vu.vf05);
  // nop                        |  maddw.xyzw vf24, vf06, vf00    667
  vu.acc.madd(Mask::xyzw, vu.vf24, vu.vf06, vu.vf00.w());
  // nop                        |  mulax.xyzw ACC, vf12, vf30     668
  vu.acc.mula(Mask::xyzw, vu.vf12, vu.vf30.x());
  // nop                        |  madday.xyzw ACC, vf13, vf30    669
  vu.acc.madda(Mask::xyzw, vu.vf13, vu.vf30.y());
  // nop                        |  maddaz.xyzw ACC, vf14, vf30    670
  vu.acc.madda(Mask::xyzw, vu.vf14, vu.vf30.z());
  // sq.xyzw vf24, 769(vi00)    |  maddw.xyzw vf30, vf15, vf00    671
  vu.acc.madd(Mask::xyzw, vu.vf30, vu.vf15, vu.vf00.w());
  sq_buffer(Mask::xyzw, vu.vf24, 769);
  // waitp                      |  mul.xyzw vf29, vf29, vf01      672
  vu.vf29.mul(Mask::xyzw, vu.vf29, vu.vf01);
  // ASSERT(false);
  // mfp.w vf25, P              |  nop                            673
  vu.vf25.mfp(Mask::w, vu.P);
  // erleng.xyz P, vf26         |  nop                            674
  vu.P = erleng(vu.vf26); /* TODO erleng */
  // nop                        |  addz.y vf26, vf00, vf26        675
  vu.vf26.add(Mask::y, vu.vf00, vu.vf26.z());
  // sq.xyzw vf29, 5(vi08)      |  nop                            676
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi08 + 5);
  // sq.xyzw vf29, 773(vi00)    |  mulw.xy vf25, vf25, vf25       677
  vu.vf25.mul(Mask::xy, vu.vf25, vu.vf25.w());
  sq_buffer(Mask::xyzw, vu.vf29, 773);
  // nop                        |  mula.xyzw ACC, vf25, vf05      678
  vu.acc.mula(Mask::xyzw, vu.vf25, vu.vf05);
  // nop                        |  maddw.xyzw vf25, vf06, vf00    679
  vu.acc.madd(Mask::xyzw, vu.vf25, vu.vf06, vu.vf00.w());
  // waitp                      |  mul.xyzw vf30, vf30, vf01      680
  vu.vf30.mul(Mask::xyzw, vu.vf30, vu.vf01);
  // ASSERT(false);
  // mfp.w vf26, P              |  nop                            681
  vu.vf26.mfp(Mask::w, vu.P);
  // sq.xyzw vf30, 8(vi08)      |  nop                            682
  sq_buffer(Mask::xyzw, vu.vf30, vu.vi08 + 8);
  // sq.xyzw vf30, 774(vi00)    |  mulw.xy vf26, vf26, vf26       683
  vu.vf26.mul(Mask::xy, vu.vf26, vu.vf26.w());
  sq_buffer(Mask::xyzw, vu.vf30, 774);
  // sq.xyzw vf25, 770(vi00)    |  mula.xyzw ACC, vf26, vf05      684
  vu.acc.mula(Mask::xyzw, vu.vf26, vu.vf05);
  sq_buffer(Mask::xyzw, vu.vf25, 770);
  // lq.xyzw vf07, 739(vi00)    |  maddw.xyzw vf26, vf06, vf00    685
  vu.acc.madd(Mask::xyzw, vu.vf26, vu.vf06, vu.vf00.w());
  lq_buffer(Mask::xyzw, vu.vf07, 739);
  // ior vi03, vi08, vi00       |  nop                            686
  vu.vi03 = vu.vi08;
  // iaddi vi05, vi00, 0x3      |  nop                            687
  vu.vi05 = 3;
  // BRANCH!
  // bal vi13, L43              |  nop                            688
  // sq.xyzw vf26, 771(vi00)    |  nop                            689
  sq_buffer(Mask::xyzw, vu.vf26, 771);
  run_L43_vu2c();

  // lq.xyzw vf28, 741(vi00)    |  nop                            690
  lq_buffer(Mask::xyzw, vu.vf28, 741);
  // BRANCH!
  // ibeq vi00, vi05, L40       |  nop                            691
  bc = (vu.vi05 == 0);
  // lq.xyzw vf29, 742(vi00)    |  nop                            692
  lq_buffer(Mask::xyzw, vu.vf29, 742);
  if (bc) {
    goto L40;
  }

  // lq.xyzw vf30, 743(vi00)    |  nop                            693
  lq_buffer(Mask::xyzw, vu.vf30, 743);
  // lq.xyzw vf24, 744(vi00)    |  nop                            694
  lq_buffer(Mask::xyzw, vu.vf24, 744);
  // lq.xyzw vf25, 745(vi00)    |  nop                            695
  lq_buffer(Mask::xyzw, vu.vf25, 745);
  // lq.xyzw vf26, 746(vi00)    |  nop                            696
  lq_buffer(Mask::xyzw, vu.vf26, 746);
  // sq.xyzw vf28, 0(vi10)      |  nop                            697
  sq_buffer(Mask::xyzw, vu.vf28, vu.vi10);
  // sq.xyzw vf29, 1(vi10)      |  nop                            698
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi10 + 1);
  // sq.xyzw vf30, 2(vi10)      |  nop                            699
  sq_buffer(Mask::xyzw, vu.vf30, vu.vi10 + 2);
  // sq.xyzw vf24, 3(vi10)      |  nop                            700
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi10 + 3);
  // sq.xyzw vf25, 4(vi10)      |  nop                            701
  sq_buffer(Mask::xyzw, vu.vf25, vu.vi10 + 4);
  // sq.xyzw vf26, 5(vi10)      |  nop                            702
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi10 + 5);
  // iaddiu vi06, vi05, 0x4000  |  nop                            703
  vu.vi06 = vu.vi05 + 0x4000; /* 16384 */
  // BRANCH!
  // bal vi13, L41              |  nop                            704
  // iaddiu vi06, vi06, 0x4000  |  nop                            705
  vu.vi06 = vu.vi06 + 0x4000; /* 16384 */
  run_L41_vu2c(render_state, prof, direct);

  // iaddi vi08, vi10, 0x7      |  nop                            706
  vu.vi08 = vu.vi10 + 7;
  // lq.xyzw vf24, 769(vi00)    |  nop                            707
  lq_buffer(Mask::xyzw, vu.vf24, 769);
  // lq.xyzw vf25, 770(vi00)    |  nop                            708
  lq_buffer(Mask::xyzw, vu.vf25, 770);
  // lq.xyzw vf26, 771(vi00)    |  nop                            709
  lq_buffer(Mask::xyzw, vu.vf26, 771);
  // lq.xyzw vf22, 756(vi00)    |  nop                            710
  lq_buffer(Mask::xyzw, vu.vf22, 756);
  // lq.xyzw vf28, 772(vi00)    |  nop                            711
  lq_buffer(Mask::xyzw, vu.vf28, 772);
  // lq.xyzw vf29, 773(vi00)    |  nop                            712
  lq_buffer(Mask::xyzw, vu.vf29, 773);
  // lq.xyzw vf30, 774(vi00)    |  nop                            713
  lq_buffer(Mask::xyzw, vu.vf30, 774);
  // sq.xyzw vf24, 0(vi08)      |  nop                            714
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi08);
  // sq.xyzw vf22, 1(vi08)      |  nop                            715
  sq_buffer(Mask::xyzw, vu.vf22, vu.vi08 + 1);
  // sq.xyzw vf28, 2(vi08)      |  nop                            716
  sq_buffer(Mask::xyzw, vu.vf28, vu.vi08 + 2);
  // sq.xyzw vf25, 3(vi08)      |  nop                            717
  sq_buffer(Mask::xyzw, vu.vf25, vu.vi08 + 3);
  // sq.xyzw vf22, 4(vi08)      |  nop                            718
  sq_buffer(Mask::xyzw, vu.vf22, vu.vi08 + 4);
  // sq.xyzw vf29, 5(vi08)      |  nop                            719
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi08 + 5);
  // sq.xyzw vf26, 6(vi08)      |  nop                            720
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi08 + 6);
  // sq.xyzw vf22, 7(vi08)      |  nop                            721
  sq_buffer(Mask::xyzw, vu.vf22, vu.vi08 + 7);
  // sq.xyzw vf30, 8(vi08)      |  nop                            722
  sq_buffer(Mask::xyzw, vu.vf30, vu.vi08 + 8);
  // sq.xyzw vf24, 9(vi08)      |  nop                            723
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi08 + 9);
  // sq.xyzw vf22, 10(vi08)     |  nop                            724
  sq_buffer(Mask::xyzw, vu.vf22, vu.vi08 + 10);
  // sq.xyzw vf28, 11(vi08)     |  nop                            725
  sq_buffer(Mask::xyzw, vu.vf28, vu.vi08 + 11);
  // ior vi03, vi08, vi00       |  nop                            726
  vu.vi03 = vu.vi08;
  // iaddi vi05, vi00, 0x3      |  nop                            727
  vu.vi05 = 3;
  // BRANCH!
  // bal vi13, L43              |  nop                            728
  // lq.xyzw vf07, 740(vi00)    |  nop                            729
  lq_buffer(Mask::xyzw, vu.vf07, 740);
  run_L43_vu2c();

  // lq.xyzw vf28, 749(vi00)    |  nop                            730
  lq_buffer(Mask::xyzw, vu.vf28, 749);
  // lq.xyzw vf29, 750(vi00)    |  nop                            731
  lq_buffer(Mask::xyzw, vu.vf29, 750);
  // lq.xyzw vf30, 751(vi00)    |  nop                            732
  lq_buffer(Mask::xyzw, vu.vf30, 751);
  // lq.xyzw vf24, 752(vi00)    |  nop                            733
  lq_buffer(Mask::xyzw, vu.vf24, 752);
  // lq.xyzw vf25, 753(vi00)    |  nop                            734
  lq_buffer(Mask::xyzw, vu.vf25, 753);
  // lq.xyzw vf26, 754(vi00)    |  nop                            735
  lq_buffer(Mask::xyzw, vu.vf26, 754);
  // sq.xyzw vf28, 0(vi10)      |  nop                            736
  sq_buffer(Mask::xyzw, vu.vf28, vu.vi10);
  // sq.xyzw vf29, 1(vi10)      |  nop                            737
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi10 + 1);
  // sq.xyzw vf30, 2(vi10)      |  nop                            738
  sq_buffer(Mask::xyzw, vu.vf30, vu.vi10 + 2);
  // sq.xyzw vf24, 3(vi10)      |  nop                            739
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi10 + 3);
  // sq.xyzw vf25, 4(vi10)      |  nop                            740
  sq_buffer(Mask::xyzw, vu.vf25, vu.vi10 + 4);
  // sq.xyzw vf26, 5(vi10)      |  nop                            741
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi10 + 5);
  // iaddiu vi06, vi05, 0x4000  |  nop                            742
  vu.vi06 = vu.vi05 + 0x4000; /* 16384 */
  // BRANCH!
  // bal vi13, L41              |  nop                            743
  // iaddiu vi06, vi06, 0x4000  |  nop                            744
  vu.vi06 = vu.vi06 + 0x4000; /* 16384 */
  run_L41_vu2c(render_state, prof, direct);

L40:
  // BRANCH!
  // ibgtz vi14, L37            |  nop                            745
  bc = ((s16)vu.vi14) > 0;
  // nop                        |  nop                            746

  if (bc) {
    goto L37;
  }

  // jr vi15                    |  nop                            747
  // nop                        |  nop                            748
}

void OceanMid::run_L41_vu2c(SharedRenderState* render_state,
                            ScopedProfilerNode& prof,
                            DirectRenderer& direct) {
  bool bc;
L41:
  // sq.xyzw vf07, -1(vi08)     |  nop                            749
  sq_buffer(Mask::xyzw, vu.vf07, vu.vi08 + -1);
  // isw.x vi06, -1(vi08)       |  nop                            750
  isw_buffer(Mask::x, vu.vi06, vu.vi08 + -1);
L42:
  // lqi.xyzw vf24, vi08        |  nop                            751
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi08++);
  // lqi.xyzw vf27, vi08        |  nop                            752
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi08++);
  // lqi.xyzw vf21, vi08        |  nop                            753
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi08++);
  // nop                        |  nop                            754

  // nop                        |  nop                            755

  // nop                        |  nop                            756

  // div Q, vf00.w, vf21.w      |  mul.xyzw vf21, vf21, vf31      757
  vu.vf21.mul(Mask::xyzw, vu.vf21, vu.vf31);
  vu.Q = vu.vf00.w() / vu.vf21.w();
  // nop                        |  nop                            758

  // nop                        |  nop                            759

  // nop                        |  nop                            760

  // nop                        |  nop                            761

  // nop                        |  nop                            762

  // nop                        |  nop                            763

  // nop                        |  mul.xyz vf21, vf21, Q          764
  vu.vf21.mul(Mask::xyz, vu.vf21, vu.Q);
  // nop                        |  mul.xyzw vf24, vf24, Q         765
  vu.vf24.mul(Mask::xyzw, vu.vf24, vu.Q);
  // nop                        |  nop                            766

  // nop                        |  nop                            767

  // nop                        |  add.xyzw vf21, vf21, vf02      768
  vu.vf21.add(Mask::xyzw, vu.vf21, vu.vf02);
  // nop                        |  nop                            769

  // nop                        |  nop                            770

  // nop                        |  nop                            771

  // nop                        |  maxy.w vf21, vf21, vf03        772
  vu.vf21.max(Mask::w, vu.vf21, vu.vf03.y());
  // nop                        |  nop                            773

  // nop                        |  nop                            774

  // nop                        |  nop                            775

  // nop                        |  miniz.w vf21, vf21, vf03       776
  vu.vf21.mini(Mask::w, vu.vf21, vu.vf03.z());
  // nop                        |  nop                            777

  // nop                        |  nop                            778

  // nop                        |  ftoi0.xyzw vf27, vf27          779
  vu.vf27.ftoi0(Mask::xyzw, vu.vf27);
  // nop                        |  ftoi4.xyzw vf21, vf21          780
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);
  // nop                        |  nop                            781

  // sq.xyzw vf24, -3(vi08)     |  nop                            782
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi08 + -3);
  // sq.xyzw vf27, -2(vi08)     |  nop                            783
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi08 + -2);
  // sq.xyzw vf21, -1(vi08)     |  nop                            784
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi08 + -1);
  // iaddi vi05, vi05, -0x1     |  nop                            785
  vu.vi05 = vu.vi05 + -1;
  // nop                        |  nop                            786

  // BRANCH!
  // ibne vi00, vi05, L42       |  nop                            787
  bc = (vu.vi05 != 0);
  // nop                        |  nop                            788

  if (bc) {
    goto L42;
  }

  // iaddiu vi01, vi00, 0x4d3   |  nop                            789
  vu.vi01 = 0x4d3; /* 1235 */
  // xgkick vi10                |  nop                            790
  fmt::print("kick from 1514\n");
  xgkick(vu.vi10, render_state, prof, direct);
  // jr vi13                    |  nop                            791
  // isub vi10, vi01, vi10      |  nop                            792
  vu.vi10 = vu.vi01 - vu.vi10;
}

void OceanMid::run_L43_vu2c() {
  u32 cf = 0;  // isn't from earlier?
  bool bc;
  fmt::print("here!!!\n");
L43:
  // ior vi04, vi03, vi00       |  nop                            793
  vu.vi04 = vu.vi03;
  // lq.xyzw vf21, 2(vi03)      |  nop                            794
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi03 + 2);
  // ior vi06, vi05, vi00       |  nop                            795
  vu.vi06 = vu.vi05;
  // lq.xyzw vf24, 0(vi03)      |  nop                            796
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi03);
  // lq.xyzw vf27, 1(vi03)      |  nop                            797
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi03 + 1);
  // iaddi vi03, vi03, 0x3      |  clipw.xyz vf21, vf21           798
  cf = clip(vu.vf21, vu.vf21.w(), cf);
  vu.vi03 = vu.vi03 + 3;
L44:
  // lq.xyzw vf22, 2(vi03)      |  nop                            799
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi03 + 2);
  // lq.xyzw vf25, 0(vi03)      |  nop                            800
  lq_buffer(Mask::xyzw, vu.vf25, vu.vi03);
  // lq.xyzw vf28, 1(vi03)      |  nop                            801
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi03 + 1);
  // iaddi vi03, vi03, 0x3      |  nop                            802
  vu.vi03 = vu.vi03 + 3;
  // nop                        |  clipw.xyz vf22, vf22           803
  cf = clip(vu.vf22, vu.vf22.w(), cf);
  // nop                        |  nop                            804

  // nop                        |  nop                            805

  // nop                        |  nop                            806

  // fcor vi01, 0xfff7df        |  nop                            807
  fcor(vu.vi01, 0xfff7df, cf);
  // ASSERT(false);
  // BRANCH!
  // ibne vi00, vi01, L55       |  nop                            808
  bc = (vu.vi01 != 0);
  // fcand vi01, 0x20           |  nop                            809
  // vu.vi01 = cf & 0x20;
  fcand(vu.vi01, 0x20, cf);

  if (bc) {
    goto L55;
  }

  // BRANCH!
  // ibne vi00, vi01, L56       |  nop                            810
  bc = (vu.vi01 != 0);
  // fcand vi01, 0x800          |  nop                            811
  // ASSERT(false);
  // vu.vi01 = cf & 0x800;
  fcand(vu.vi01, 0x800, cf);

  if (bc) {
    goto L56;
  }

  // BRANCH!
  // ibne vi00, vi01, L57       |  nop                            812
  bc = (vu.vi01 != 0);
  // nop                        |  nop                            813

  if (bc) {
    goto L57;
  }

  // sqi.xyzw vf24, vi04        |  nop                            814
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  nop                            815
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  nop                            816
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
L45:
  // move.xyzw vf24, vf25       |  nop                            817
  vu.vf24.move(Mask::xyzw, vu.vf25);
  // iaddi vi05, vi05, -0x1     |  nop                            818
  vu.vi05 = vu.vi05 + -1;
  // move.xyzw vf27, vf28       |  nop                            819
  vu.vf27.move(Mask::xyzw, vu.vf28);
  // BRANCH!
  // ibne vi00, vi05, L44       |  nop                            820
  bc = (vu.vi05 != 0);
  // move.xyzw vf21, vf22       |  nop                            821
  vu.vf21.move(Mask::xyzw, vu.vf22);
  if (bc) {
    goto L44;
  }

  // lq.xyzw vf24, 0(vi08)      |  nop                            822
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi08);
  // lq.xyzw vf27, 1(vi08)      |  nop                            823
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi08 + 1);
  // lq.xyzw vf21, 2(vi08)      |  nop                            824
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi08 + 2);
  // ior vi03, vi08, vi00       |  nop                            825
  vu.vi03 = vu.vi08;
  // sqi.xyzw vf24, vi04        |  nop                            826
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  nop                            827
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  nop                            828
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
  // BRANCH!
  // ibeq vi00, vi06, L54       |  nop                            829
  bc = (vu.vi06 == 0);
  // ior vi05, vi06, vi00       |  nop                            830
  vu.vi05 = vu.vi06;
  if (bc) {
    goto L54;
  }

  // ior vi04, vi03, vi00       |  nop                            831
  vu.vi04 = vu.vi03;
  // lq.xyzw vf21, 2(vi03)      |  nop                            832
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi03 + 2);
  // ior vi06, vi05, vi00       |  nop                            833
  vu.vi06 = vu.vi05;
  // lq.xyzw vf24, 0(vi03)      |  nop                            834
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi03);
  // lq.xyzw vf27, 1(vi03)      |  nop                            835
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi03 + 1);
  // iaddi vi03, vi03, 0x3      |  clipw.xyz vf21, vf21           836
  cf = clip(vu.vf21, vu.vf21.w(), cf);
  vu.vi03 = vu.vi03 + 3;
L46:
  // lq.xyzw vf22, 2(vi03)      |  nop                            837
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi03 + 2);
  // lq.xyzw vf25, 0(vi03)      |  nop                            838
  lq_buffer(Mask::xyzw, vu.vf25, vu.vi03);
  // lq.xyzw vf28, 1(vi03)      |  nop                            839
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi03 + 1);
  // iaddi vi03, vi03, 0x3      |  nop                            840
  vu.vi03 = vu.vi03 + 3;
  // nop                        |  clipw.xyz vf22, vf22           841
  cf = clip(vu.vf22, vu.vf22.w(), cf);
  // nop                        |  nop                            842

  // nop                        |  nop                            843

  // nop                        |  nop                            844

  // fcor vi01, 0xfffdf7        |  nop                            845
  // ASSERT(false);
  fcor(vu.vi01, 0xfffdf7 ,cf);
  // BRANCH!
  // ibne vi00, vi01, L58       |  nop                            846
  bc = (vu.vi01 != 0);
  // fcand vi01, 0x8            |  nop                            847
  // ASSERT(false);
  // vu.vi01 = cf & 0x8;
  fcand(vu.vi01, 0x8, cf);

  if (bc) {
    goto L58;
  }

  // BRANCH!
  // ibne vi00, vi01, L59       |  nop                            848
  bc = (vu.vi01 != 0);
  // fcand vi01, 0x200          |  nop                            849
  // ASSERT(false);
  // vu.vi01 = cf & 0x200;
  fcand(vu.vi01, 0x200, cf);


  if (bc) {
    goto L59;
  }

  // BRANCH!
  // ibne vi00, vi01, L60       |  nop                            850
  bc = (vu.vi01 != 0);
  // nop                        |  nop                            851

  if (bc) {
    goto L60;
  }

  // sqi.xyzw vf24, vi04        |  nop                            852
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  nop                            853
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  nop                            854
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
L47:
  // move.xyzw vf24, vf25       |  nop                            855
  vu.vf24.move(Mask::xyzw, vu.vf25);
  // iaddi vi05, vi05, -0x1     |  nop                            856
  vu.vi05 = vu.vi05 + -1;
  // move.xyzw vf27, vf28       |  nop                            857
  vu.vf27.move(Mask::xyzw, vu.vf28);
  // BRANCH!
  // ibne vi00, vi05, L46       |  nop                            858
  bc = (vu.vi05 != 0);
  // move.xyzw vf21, vf22       |  nop                            859
  vu.vf21.move(Mask::xyzw, vu.vf22);
  if (bc) {
    goto L46;
  }

  // lq.xyzw vf24, 0(vi08)      |  nop                            860
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi08);
  // lq.xyzw vf27, 1(vi08)      |  nop                            861
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi08 + 1);
  // lq.xyzw vf21, 2(vi08)      |  nop                            862
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi08 + 2);
  // ior vi03, vi08, vi00       |  nop                            863
  vu.vi03 = vu.vi08;
  // sqi.xyzw vf24, vi04        |  nop                            864
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  nop                            865
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  nop                            866
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
  // BRANCH!
  // ibeq vi00, vi06, L54       |  nop                            867
  bc = (vu.vi06 == 0);
  // ior vi05, vi06, vi00       |  nop                            868
  vu.vi05 = vu.vi06;
  if (bc) {
    goto L54;
  }

  // ior vi04, vi03, vi00       |  nop                            869
  vu.vi04 = vu.vi03;
  // lq.xyzw vf21, 2(vi03)      |  nop                            870
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi03 + 2);
  // ior vi06, vi05, vi00       |  nop                            871
  vu.vi06 = vu.vi05;
  // lq.xyzw vf24, 0(vi03)      |  nop                            872
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi03);
  // lq.xyzw vf27, 1(vi03)      |  nop                            873
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi03 + 1);
  // iaddi vi03, vi03, 0x3      |  clipw.xyz vf21, vf21           874
  cf = clip(vu.vf21, vu.vf21.w(), cf);
  vu.vi03 = vu.vi03 + 3;
L48:
  // lq.xyzw vf22, 2(vi03)      |  nop                            875
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi03 + 2);
  // lq.xyzw vf25, 0(vi03)      |  nop                            876
  lq_buffer(Mask::xyzw, vu.vf25, vu.vi03);
  // lq.xyzw vf28, 1(vi03)      |  nop                            877
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi03 + 1);
  // iaddi vi03, vi03, 0x3      |  nop                            878
  vu.vi03 = vu.vi03 + 3;
  // nop                        |  clipw.xyz vf22, vf22           879
  cf = clip(vu.vf22, vu.vf22.w(), cf);
  // nop                        |  nop                            880

  // nop                        |  nop                            881

  // nop                        |  nop                            882

  // fcor vi01, 0xfffefb        |  nop                            883
  // ASSERT(false);
  fcor(vu.vi01, 0xfffefb, cf);
  // BRANCH!
  // ibne vi00, vi01, L61       |  nop                            884
  bc = (vu.vi01 != 0);
  // fcand vi01, 0x4            |  nop                            885
  // ASSERT(false);
  // vu.vi01 = cf & 0x4;
  fcand(vu.vi01, 0x4, cf);

  if (bc) {
    goto L61;
  }

  // BRANCH!
  // ibne vi00, vi01, L62       |  nop                            886
  bc = (vu.vi01 != 0);
  // fcand vi01, 0x100          |  nop                            887
  // ASSERT(false);
  // vu.vi01 = cf & 0x100;
  fcand(vu.vi01, 0x100, cf);

  if (bc) {
    goto L62;
  }

  // BRANCH!
  // ibne vi00, vi01, L63       |  nop                            888
  bc = (vu.vi01 != 0);
  // nop                        |  nop                            889

  if (bc) {
    goto L63;
  }

  // sqi.xyzw vf24, vi04        |  nop                            890
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  nop                            891
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  nop                            892
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
L49:
  // move.xyzw vf24, vf25       |  nop                            893
  vu.vf24.move(Mask::xyzw, vu.vf25);
  // iaddi vi05, vi05, -0x1     |  nop                            894
  vu.vi05 = vu.vi05 + -1;
  // move.xyzw vf27, vf28       |  nop                            895
  vu.vf27.move(Mask::xyzw, vu.vf28);
  // BRANCH!
  // ibne vi00, vi05, L48       |  nop                            896
  bc = (vu.vi05 != 0);
  // move.xyzw vf21, vf22       |  nop                            897
  vu.vf21.move(Mask::xyzw, vu.vf22);
  if (bc) {
    goto L48;
  }

  // lq.xyzw vf24, 0(vi08)      |  nop                            898
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi08);
  // lq.xyzw vf27, 1(vi08)      |  nop                            899
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi08 + 1);
  // lq.xyzw vf21, 2(vi08)      |  nop                            900
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi08 + 2);
  // ior vi03, vi08, vi00       |  nop                            901
  vu.vi03 = vu.vi08;
  // sqi.xyzw vf24, vi04        |  nop                            902
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  nop                            903
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  nop                            904
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
  // BRANCH!
  // ibeq vi00, vi06, L54       |  nop                            905
  bc = (vu.vi06 == 0);
  // ior vi05, vi06, vi00       |  nop                            906
  vu.vi05 = vu.vi06;
  if (bc) {
    goto L54;
  }

  // ior vi04, vi03, vi00       |  nop                            907
  vu.vi04 = vu.vi03;
  // lq.xyzw vf21, 2(vi03)      |  nop                            908
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi03 + 2);
  // ior vi06, vi05, vi00       |  nop                            909
  vu.vi06 = vu.vi05;
  // lq.xyzw vf24, 0(vi03)      |  nop                            910
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi03);
  // lq.xyzw vf27, 1(vi03)      |  nop                            911
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi03 + 1);
  // iaddi vi03, vi03, 0x3      |  clipw.xyz vf21, vf21           912
  // ASSERT(false);
  cf = clip(vu.vf21, vu.vf21.w(), cf);
  vu.vi03 = vu.vi03 + 3;
L50:
  // lq.xyzw vf22, 2(vi03)      |  nop                            913
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi03 + 2);
  // lq.xyzw vf25, 0(vi03)      |  nop                            914
  lq_buffer(Mask::xyzw, vu.vf25, vu.vi03);
  // lq.xyzw vf28, 1(vi03)      |  nop                            915
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi03 + 1);
  // iaddi vi03, vi03, 0x3      |  nop                            916
  vu.vi03 = vu.vi03 + 3;
  // nop                        |  clipw.xyz vf22, vf22           917
  // ASSERT(false);
  cf = clip(vu.vf22, vu.vf22.w(), cf);
  // nop                        |  nop                            918

  // nop                        |  nop                            919

  // nop                        |  nop                            920

  // fcor vi01, 0xffff7d        |  nop                            921
  // ASSERT(false);
  fcor(vu.vi01, 0xffff7d, cf);
  // BRANCH!
  // ibne vi00, vi01, L64       |  nop                            922
  bc = (vu.vi01 != 0);
  // fcand vi01, 0x2            |  nop                            923
  // ASSERT(false);
  // vu.vi01 = cf & 0x2;
  fcand(vu.vi01, 0x2, cf);

  if (bc) {
    goto L64;
  }

  // BRANCH!
  // ibne vi00, vi01, L65       |  nop                            924
  bc = (vu.vi01 != 0);
  // fcand vi01, 0x80           |  nop                            925
  // ASSERT(false);
  // vu.vi01 = cf & 0x80;
  fcand(vu.vi01, 0x80, cf);

  if (bc) {
    goto L65;
  }

  // BRANCH!
  // ibne vi00, vi01, L66       |  nop                            926
  bc = (vu.vi01 != 0);
  // nop                        |  nop                            927

  if (bc) {
    goto L66;
  }

  // sqi.xyzw vf24, vi04        |  nop                            928
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  nop                            929
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  nop                            930
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
L51:
  // move.xyzw vf24, vf25       |  nop                            931
  vu.vf24.move(Mask::xyzw, vu.vf25);
  // iaddi vi05, vi05, -0x1     |  nop                            932
  vu.vi05 = vu.vi05 + -1;
  // move.xyzw vf27, vf28       |  nop                            933
  vu.vf27.move(Mask::xyzw, vu.vf28);
  // BRANCH!
  // ibne vi00, vi05, L50       |  nop                            934
  bc = (vu.vi05 != 0);
  // move.xyzw vf21, vf22       |  nop                            935
  vu.vf21.move(Mask::xyzw, vu.vf22);
  if (bc) {
    goto L50;
  }

  // lq.xyzw vf24, 0(vi08)      |  nop                            936
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi08);
  // lq.xyzw vf27, 1(vi08)      |  nop                            937
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi08 + 1);
  // lq.xyzw vf21, 2(vi08)      |  nop                            938
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi08 + 2);
  // ior vi03, vi08, vi00       |  nop                            939
  vu.vi03 = vu.vi08;
  // sqi.xyzw vf24, vi04        |  nop                            940
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  nop                            941
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  nop                            942
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
  // BRANCH!
  // ibeq vi00, vi06, L54       |  nop                            943
  bc = (vu.vi06 == 0);
  // ior vi05, vi06, vi00       |  nop                            944
  vu.vi05 = vu.vi06;
  if (bc) {
    goto L54;
  }

  // ior vi04, vi03, vi00       |  nop                            945
  vu.vi04 = vu.vi03;
  // lq.xyzw vf21, 2(vi03)      |  nop                            946
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi03 + 2);
  // ior vi06, vi05, vi00       |  nop                            947
  vu.vi06 = vu.vi05;
  // lq.xyzw vf24, 0(vi03)      |  nop                            948
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi03);
  // lq.xyzw vf27, 1(vi03)      |  nop                            949
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi03 + 1);
  // iaddi vi03, vi03, 0x3      |  clipw.xyz vf21, vf21           950
  // ASSERT(false);
  cf = clip(vu.vf21, vu.vf21.w(), cf);
  vu.vi03 = vu.vi03 + 3;
L52:
  // lq.xyzw vf22, 2(vi03)      |  nop                            951
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi03 + 2);
  // lq.xyzw vf25, 0(vi03)      |  nop                            952
  lq_buffer(Mask::xyzw, vu.vf25, vu.vi03);
  // lq.xyzw vf28, 1(vi03)      |  nop                            953
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi03 + 1);
  // iaddi vi03, vi03, 0x3      |  nop                            954
  vu.vi03 = vu.vi03 + 3;
  // nop                        |  clipw.xyz vf22, vf22           955
  // ASSERT(false);
  cf = clip(vu.vf22, vu.vf22.w(), cf);
  // nop                        |  nop                            956

  // nop                        |  nop                            957

  // nop                        |  nop                            958

  // fcor vi01, 0xffffbe        |  nop                            959
  // ASSERT(false);
  fcor(vu.vi01, 0xffffbe, cf);
  // BRANCH!
  // ibne vi00, vi01, L67       |  nop                            960
  bc = (vu.vi01 != 0);
  // fcand vi01, 0x1            |  nop                            961
  // ASSERT(false);
  fcand(vu.vi01, 1, cf);
  // vu.vi01 = cf & 0x1;

  if (bc) {
    goto L67;
  }

  // BRANCH!
  // ibne vi00, vi01, L68       |  nop                            962
  bc = (vu.vi01 != 0);
  // fcand vi01, 0x40           |  nop                            963
  // ASSERT(false);
  fcand(vu.vi01, 0x40, cf);
  // vu.vi01 = cf & 0x40;

  if (bc) {
    goto L68;
  }

  // BRANCH!
  // ibne vi00, vi01, L69       |  nop                            964
  bc = (vu.vi01 != 0);
  // nop                        |  nop                            965

  if (bc) {
    goto L69;
  }

  // sqi.xyzw vf24, vi04        |  nop                            966
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  nop                            967
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  nop                            968
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
L53:
  // move.xyzw vf24, vf25       |  nop                            969
  vu.vf24.move(Mask::xyzw, vu.vf25);
  // iaddi vi05, vi05, -0x1     |  nop                            970
  vu.vi05 = vu.vi05 + -1;
  // move.xyzw vf27, vf28       |  nop                            971
  vu.vf27.move(Mask::xyzw, vu.vf28);
  // BRANCH!
  // ibne vi00, vi05, L52       |  nop                            972
  bc = (vu.vi05 != 0);
  // move.xyzw vf21, vf22       |  nop                            973
  vu.vf21.move(Mask::xyzw, vu.vf22);
  if (bc) {
    goto L52;
  }

  // lq.xyzw vf24, 0(vi08)      |  nop                            974
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi08);
  // lq.xyzw vf27, 1(vi08)      |  nop                            975
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi08 + 1);
  // lq.xyzw vf21, 2(vi08)      |  nop                            976
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi08 + 2);
  // ior vi03, vi08, vi00       |  nop                            977
  vu.vi03 = vu.vi08;
  // sqi.xyzw vf24, vi04        |  nop                            978
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  nop                            979
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  nop                            980
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
  // BRANCH!
  // ibeq vi00, vi06, L54       |  nop                            981
  bc = (vu.vi06 == 0);
  // ior vi05, vi06, vi00       |  nop                            982
  vu.vi05 = vu.vi06;
  if (bc) {
    goto L54;
  }

L54:
  // jr vi13                    |  nop                            983
  // ASSERT(false);
  return;
  // nop                        |  nop                            984

L55:
  // BRANCH!
  // b L45                      |  nop                            985
  bc = true;
  // iaddi vi06, vi06, -0x1     |  nop                            986
  vu.vi06 = vu.vi06 + -1;
  if (bc) {
    goto L45;
  }

L56:
  // sqi.xyzw vf24, vi04        |  sub.xyzw vf23, vf22, vf21      987
  vu.vf23.sub(Mask::xyzw, vu.vf22, vu.vf21);
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  sub.xyzw vf26, vf25, vf24      988
  vu.vf26.sub(Mask::xyzw, vu.vf25, vu.vf24);
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  sub.xyzw vf29, vf28, vf27      989
  vu.vf29.sub(Mask::xyzw, vu.vf28, vu.vf27);
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
  // BRANCH!
  // ibeq vi03, vi04, L45       |  nop                            990
  bc = (vu.vi03 == vu.vi04);
  // nop                        |  addz.w vf30, vf23, vf23        991
  vu.vf30.add(Mask::w, vu.vf23, vu.vf23.z());
  if (bc) {
    goto L45;
  }

  // nop                        |  addw.z vf30, vf21, vf21        992
  vu.vf30.add(Mask::z, vu.vf21, vu.vf21.w());
  // div Q, vf30.z, vf30.w      |  nop                            993
  vu.Q = vu.vf30.z() / vu.vf30.w();
  // waitq                      |  nop                            994
  // ASSERT(false);
  // nop                        |  mul.xyzw vf26, vf26, Q         995
  vu.vf26.mul(Mask::xyzw, vu.vf26, vu.Q);
  // nop                        |  mul.xyzw vf29, vf29, Q         996
  vu.vf29.mul(Mask::xyzw, vu.vf29, vu.Q);
  // nop                        |  mul.xyzw vf23, vf23, Q         997
  vu.vf23.mul(Mask::xyzw, vu.vf23, vu.Q);
  // nop                        |  sub.xyzw vf26, vf24, vf26      998
  vu.vf26.sub(Mask::xyzw, vu.vf24, vu.vf26);
  // nop                        |  sub.xyzw vf29, vf27, vf29      999
  vu.vf29.sub(Mask::xyzw, vu.vf27, vu.vf29);
  // nop                        |  sub.xyzw vf23, vf21, vf23      1000
  vu.vf23.sub(Mask::xyzw, vu.vf21, vu.vf23);
  // iaddi vi06, vi06, 0x1      |  nop                            1001
  vu.vi06 = vu.vi06 + 1;
  // sqi.xyzw vf26, vi04        |  nop                            1002
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi04++);
  // sqi.xyzw vf29, vi04        |  nop                            1003
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi04++);
  // BRANCH!
  // b L45                      |  nop                            1004
  bc = true;
  // sqi.xyzw vf23, vi04        |  nop                            1005
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi04++);
  if (bc) {
    goto L45;
  }

L57:
  // nop                        |  sub.xyzw vf23, vf21, vf22      1006
  vu.vf23.sub(Mask::xyzw, vu.vf21, vu.vf22);
  // nop                        |  sub.xyzw vf26, vf24, vf25      1007
  vu.vf26.sub(Mask::xyzw, vu.vf24, vu.vf25);
  // nop                        |  sub.xyzw vf29, vf27, vf28      1008
  vu.vf29.sub(Mask::xyzw, vu.vf27, vu.vf28);
  // nop                        |  addz.w vf30, vf23, vf23        1009
  vu.vf30.add(Mask::w, vu.vf23, vu.vf23.z());
  // nop                        |  addw.z vf30, vf22, vf22        1010
  vu.vf30.add(Mask::z, vu.vf22, vu.vf22.w());
  // div Q, vf30.z, vf30.w      |  nop                            1011
  vu.Q = vu.vf30.z() / vu.vf30.w();
  // waitq                      |  nop                            1012
  // ASSERT(false);
  // nop                        |  mul.xyzw vf26, vf26, Q         1013
  vu.vf26.mul(Mask::xyzw, vu.vf26, vu.Q);
  // nop                        |  mul.xyzw vf29, vf29, Q         1014
  vu.vf29.mul(Mask::xyzw, vu.vf29, vu.Q);
  // nop                        |  mul.xyzw vf23, vf23, Q         1015
  vu.vf23.mul(Mask::xyzw, vu.vf23, vu.Q);
  // nop                        |  sub.xyzw vf26, vf25, vf26      1016
  vu.vf26.sub(Mask::xyzw, vu.vf25, vu.vf26);
  // nop                        |  sub.xyzw vf29, vf28, vf29      1017
  vu.vf29.sub(Mask::xyzw, vu.vf28, vu.vf29);
  // nop                        |  sub.xyzw vf23, vf22, vf23      1018
  vu.vf23.sub(Mask::xyzw, vu.vf22, vu.vf23);
  // sqi.xyzw vf26, vi04        |  nop                            1019
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi04++);
  // sqi.xyzw vf29, vi04        |  nop                            1020
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi04++);
  // BRANCH!
  // b L45                      |  nop                            1021
  bc = true;
  // sqi.xyzw vf23, vi04        |  nop                            1022
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi04++);
  if (bc) {
    goto L45;
  }

L58:
  // BRANCH!
  // b L47                      |  nop                            1023
  bc = true;
  // iaddi vi06, vi06, -0x1     |  nop                            1024
  vu.vi06 = vu.vi06 + -1;
  if (bc) {
    goto L47;
  }

L59:
  // sqi.xyzw vf24, vi04        |  sub.xyzw vf23, vf22, vf21      1025
  vu.vf23.sub(Mask::xyzw, vu.vf22, vu.vf21);
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  sub.xyzw vf26, vf25, vf24      1026
  vu.vf26.sub(Mask::xyzw, vu.vf25, vu.vf24);
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  sub.xyzw vf29, vf28, vf27      1027
  vu.vf29.sub(Mask::xyzw, vu.vf28, vu.vf27);
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
  // BRANCH!
  // ibeq vi03, vi04, L47       |  nop                            1028
  bc = (vu.vi03 == vu.vi04);
  // nop                        |  addy.w vf30, vf23, vf23        1029
  vu.vf30.add(Mask::w, vu.vf23, vu.vf23.y());
  if (bc) {
    goto L47;
  }

  // nop                        |  addw.y vf30, vf21, vf21        1030
  vu.vf30.add(Mask::y, vu.vf21, vu.vf21.w());
  // div Q, vf30.y, vf30.w      |  nop                            1031
  vu.Q = vu.vf30.y() / vu.vf30.w();
  // waitq                      |  nop                            1032
  // ASSERT(false);
  // nop                        |  mul.xyzw vf26, vf26, Q         1033
  vu.vf26.mul(Mask::xyzw, vu.vf26, vu.Q);
  // nop                        |  mul.xyzw vf29, vf29, Q         1034
  vu.vf29.mul(Mask::xyzw, vu.vf29, vu.Q);
  // nop                        |  mul.xyzw vf23, vf23, Q         1035
  vu.vf23.mul(Mask::xyzw, vu.vf23, vu.Q);
  // nop                        |  sub.xyzw vf26, vf24, vf26      1036
  vu.vf26.sub(Mask::xyzw, vu.vf24, vu.vf26);
  // nop                        |  sub.xyzw vf29, vf27, vf29      1037
  vu.vf29.sub(Mask::xyzw, vu.vf27, vu.vf29);
  // nop                        |  sub.xyzw vf23, vf21, vf23      1038
  vu.vf23.sub(Mask::xyzw, vu.vf21, vu.vf23);
  // iaddi vi06, vi06, 0x1      |  nop                            1039
  vu.vi06 = vu.vi06 + 1;
  // sqi.xyzw vf26, vi04        |  nop                            1040
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi04++);
  // sqi.xyzw vf29, vi04        |  nop                            1041
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi04++);
  // BRANCH!
  // b L47                      |  nop                            1042
  bc = true;
  // sqi.xyzw vf23, vi04        |  nop                            1043
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi04++);
  if (bc) {
    goto L47;
  }

L60:
  // nop                        |  sub.xyzw vf23, vf21, vf22      1044
  vu.vf23.sub(Mask::xyzw, vu.vf21, vu.vf22);
  // nop                        |  sub.xyzw vf26, vf24, vf25      1045
  vu.vf26.sub(Mask::xyzw, vu.vf24, vu.vf25);
  // nop                        |  sub.xyzw vf29, vf27, vf28      1046
  vu.vf29.sub(Mask::xyzw, vu.vf27, vu.vf28);
  // nop                        |  addy.w vf30, vf23, vf23        1047
  vu.vf30.add(Mask::w, vu.vf23, vu.vf23.y());
  // nop                        |  addw.y vf30, vf22, vf22        1048
  vu.vf30.add(Mask::y, vu.vf22, vu.vf22.w());
  // div Q, vf30.y, vf30.w      |  nop                            1049
  vu.Q = vu.vf30.y() / vu.vf30.w();
  // waitq                      |  nop                            1050
  // ASSERT(false);
  // nop                        |  mul.xyzw vf26, vf26, Q         1051
  vu.vf26.mul(Mask::xyzw, vu.vf26, vu.Q);
  // nop                        |  mul.xyzw vf29, vf29, Q         1052
  vu.vf29.mul(Mask::xyzw, vu.vf29, vu.Q);
  // nop                        |  mul.xyzw vf23, vf23, Q         1053
  vu.vf23.mul(Mask::xyzw, vu.vf23, vu.Q);
  // nop                        |  sub.xyzw vf26, vf25, vf26      1054
  vu.vf26.sub(Mask::xyzw, vu.vf25, vu.vf26);
  // nop                        |  sub.xyzw vf29, vf28, vf29      1055
  vu.vf29.sub(Mask::xyzw, vu.vf28, vu.vf29);
  // nop                        |  sub.xyzw vf23, vf22, vf23      1056
  vu.vf23.sub(Mask::xyzw, vu.vf22, vu.vf23);
  // sqi.xyzw vf26, vi04        |  nop                            1057
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi04++);
  // sqi.xyzw vf29, vi04        |  nop                            1058
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi04++);
  // BRANCH!
  // b L47                      |  nop                            1059
  bc = true;
  // sqi.xyzw vf23, vi04        |  nop                            1060
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi04++);
  if (bc) {
    goto L47;
  }

L61:
  // BRANCH!
  // b L49                      |  nop                            1061
  bc = true;
  // iaddi vi06, vi06, -0x1     |  nop                            1062
  vu.vi06 = vu.vi06 + -1;
  if (bc) {
    goto L49;
  }

L62:
  // sqi.xyzw vf24, vi04        |  sub.xyzw vf23, vf22, vf21      1063
  vu.vf23.sub(Mask::xyzw, vu.vf22, vu.vf21);
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  sub.xyzw vf26, vf25, vf24      1064
  vu.vf26.sub(Mask::xyzw, vu.vf25, vu.vf24);
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  sub.xyzw vf29, vf28, vf27      1065
  vu.vf29.sub(Mask::xyzw, vu.vf28, vu.vf27);
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
  // BRANCH!
  // ibeq vi03, vi04, L49       |  nop                            1066
  bc = (vu.vi03 == vu.vi04);
  // nop                        |  suby.w vf30, vf23, vf23        1067
  vu.vf30.sub(Mask::w, vu.vf23, vu.vf23.y());
  if (bc) {
    goto L49;
  }

  // nop                        |  subw.y vf30, vf21, vf21        1068
  vu.vf30.sub(Mask::y, vu.vf21, vu.vf21.w());
  // div Q, vf30.y, vf30.w      |  nop                            1069
  vu.Q = vu.vf30.y() / vu.vf30.w();
  // waitq                      |  nop                            1070
  // ASSERT(false);
  // nop                        |  mul.xyzw vf26, vf26, Q         1071
  vu.vf26.mul(Mask::xyzw, vu.vf26, vu.Q);
  // nop                        |  mul.xyzw vf29, vf29, Q         1072
  vu.vf29.mul(Mask::xyzw, vu.vf29, vu.Q);
  // nop                        |  mul.xyzw vf23, vf23, Q         1073
  vu.vf23.mul(Mask::xyzw, vu.vf23, vu.Q);
  // nop                        |  add.xyzw vf26, vf24, vf26      1074
  vu.vf26.add(Mask::xyzw, vu.vf24, vu.vf26);
  // nop                        |  add.xyzw vf29, vf27, vf29      1075
  vu.vf29.add(Mask::xyzw, vu.vf27, vu.vf29);
  // nop                        |  add.xyzw vf23, vf21, vf23      1076
  vu.vf23.add(Mask::xyzw, vu.vf21, vu.vf23);
  // iaddi vi06, vi06, 0x1      |  nop                            1077
  vu.vi06 = vu.vi06 + 1;
  // sqi.xyzw vf26, vi04        |  nop                            1078
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi04++);
  // sqi.xyzw vf29, vi04        |  nop                            1079
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi04++);
  // BRANCH!
  // b L49                      |  nop                            1080
  bc = true;
  // sqi.xyzw vf23, vi04        |  nop                            1081
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi04++);
  if (bc) {
    goto L49;
  }

L63:
  // nop                        |  sub.xyzw vf23, vf21, vf22      1082
  vu.vf23.sub(Mask::xyzw, vu.vf21, vu.vf22);
  // nop                        |  sub.xyzw vf26, vf24, vf25      1083
  vu.vf26.sub(Mask::xyzw, vu.vf24, vu.vf25);
  // nop                        |  sub.xyzw vf29, vf27, vf28      1084
  vu.vf29.sub(Mask::xyzw, vu.vf27, vu.vf28);
  // nop                        |  suby.w vf30, vf23, vf23        1085
  vu.vf30.sub(Mask::w, vu.vf23, vu.vf23.y());
  // nop                        |  subw.y vf30, vf22, vf22        1086
  vu.vf30.sub(Mask::y, vu.vf22, vu.vf22.w());
  // div Q, vf30.y, vf30.w      |  nop                            1087
  vu.Q = vu.vf30.y() / vu.vf30.w();
  // waitq                      |  nop                            1088
  // ASSERT(false);
  // nop                        |  mul.xyzw vf26, vf26, Q         1089
  vu.vf26.mul(Mask::xyzw, vu.vf26, vu.Q);
  // nop                        |  mul.xyzw vf29, vf29, Q         1090
  vu.vf29.mul(Mask::xyzw, vu.vf29, vu.Q);
  // nop                        |  mul.xyzw vf23, vf23, Q         1091
  vu.vf23.mul(Mask::xyzw, vu.vf23, vu.Q);
  // nop                        |  add.xyzw vf26, vf25, vf26      1092
  vu.vf26.add(Mask::xyzw, vu.vf25, vu.vf26);
  // nop                        |  add.xyzw vf29, vf28, vf29      1093
  vu.vf29.add(Mask::xyzw, vu.vf28, vu.vf29);
  // nop                        |  add.xyzw vf23, vf22, vf23      1094
  vu.vf23.add(Mask::xyzw, vu.vf22, vu.vf23);
  // sqi.xyzw vf26, vi04        |  nop                            1095
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi04++);
  // sqi.xyzw vf29, vi04        |  nop                            1096
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi04++);
  // BRANCH!
  // b L49                      |  nop                            1097
  bc = true;
  // sqi.xyzw vf23, vi04        |  nop                            1098
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi04++);
  if (bc) {
    goto L49;
  }

L64:
  // BRANCH!
  // b L51                      |  nop                            1099
  bc = true;
  // iaddi vi06, vi06, -0x1     |  nop                            1100
  vu.vi06 = vu.vi06 + -1;
  if (bc) {
    goto L51;
  }

L65:
  // sqi.xyzw vf24, vi04        |  sub.xyzw vf23, vf22, vf21      1101
  vu.vf23.sub(Mask::xyzw, vu.vf22, vu.vf21);
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  sub.xyzw vf26, vf25, vf24      1102
  vu.vf26.sub(Mask::xyzw, vu.vf25, vu.vf24);
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  sub.xyzw vf29, vf28, vf27      1103
  vu.vf29.sub(Mask::xyzw, vu.vf28, vu.vf27);
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
  // BRANCH!
  // ibeq vi03, vi04, L51       |  nop                            1104
  bc = (vu.vi03 == vu.vi04);
  // nop                        |  addx.w vf30, vf23, vf23        1105
  vu.vf30.add(Mask::w, vu.vf23, vu.vf23.x());
  if (bc) {
    goto L51;
  }

  // nop                        |  addw.x vf30, vf21, vf21        1106
  vu.vf30.add(Mask::x, vu.vf21, vu.vf21.w());
  // div Q, vf30.x, vf30.w      |  nop                            1107
  vu.Q = vu.vf30.x() / vu.vf30.w();
  // waitq                      |  nop                            1108
  // ASSERT(false);
  // nop                        |  mul.xyzw vf26, vf26, Q         1109
  vu.vf26.mul(Mask::xyzw, vu.vf26, vu.Q);
  // nop                        |  mul.xyzw vf29, vf29, Q         1110
  vu.vf29.mul(Mask::xyzw, vu.vf29, vu.Q);
  // nop                        |  mul.xyzw vf23, vf23, Q         1111
  vu.vf23.mul(Mask::xyzw, vu.vf23, vu.Q);
  // nop                        |  sub.xyzw vf26, vf24, vf26      1112
  vu.vf26.sub(Mask::xyzw, vu.vf24, vu.vf26);
  // nop                        |  sub.xyzw vf29, vf27, vf29      1113
  vu.vf29.sub(Mask::xyzw, vu.vf27, vu.vf29);
  // nop                        |  sub.xyzw vf23, vf21, vf23      1114
  vu.vf23.sub(Mask::xyzw, vu.vf21, vu.vf23);
  // iaddi vi06, vi06, 0x1      |  nop                            1115
  vu.vi06 = vu.vi06 + 1;
  // sqi.xyzw vf26, vi04        |  nop                            1116
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi04++);
  // sqi.xyzw vf29, vi04        |  nop                            1117
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi04++);
  // BRANCH!
  // b L51                      |  nop                            1118
  bc = true;
  // sqi.xyzw vf23, vi04        |  nop                            1119
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi04++);
  if (bc) {
    goto L51;
  }

L66:
  // nop                        |  sub.xyzw vf23, vf21, vf22      1120
  vu.vf23.sub(Mask::xyzw, vu.vf21, vu.vf22);
  // nop                        |  sub.xyzw vf26, vf24, vf25      1121
  vu.vf26.sub(Mask::xyzw, vu.vf24, vu.vf25);
  // nop                        |  sub.xyzw vf29, vf27, vf28      1122
  vu.vf29.sub(Mask::xyzw, vu.vf27, vu.vf28);
  // nop                        |  addx.w vf30, vf23, vf23        1123
  vu.vf30.add(Mask::w, vu.vf23, vu.vf23.x());
  // nop                        |  addw.x vf30, vf22, vf22        1124
  vu.vf30.add(Mask::x, vu.vf22, vu.vf22.w());
  // div Q, vf30.x, vf30.w      |  nop                            1125
  vu.Q = vu.vf30.x() / vu.vf30.w();
  // waitq                      |  nop                            1126
  // ASSERT(false);
  // nop                        |  mul.xyzw vf26, vf26, Q         1127
  vu.vf26.mul(Mask::xyzw, vu.vf26, vu.Q);
  // nop                        |  mul.xyzw vf29, vf29, Q         1128
  vu.vf29.mul(Mask::xyzw, vu.vf29, vu.Q);
  // nop                        |  mul.xyzw vf23, vf23, Q         1129
  vu.vf23.mul(Mask::xyzw, vu.vf23, vu.Q);
  // nop                        |  sub.xyzw vf26, vf25, vf26      1130
  vu.vf26.sub(Mask::xyzw, vu.vf25, vu.vf26);
  // nop                        |  sub.xyzw vf29, vf28, vf29      1131
  vu.vf29.sub(Mask::xyzw, vu.vf28, vu.vf29);
  // nop                        |  sub.xyzw vf23, vf22, vf23      1132
  vu.vf23.sub(Mask::xyzw, vu.vf22, vu.vf23);
  // sqi.xyzw vf26, vi04        |  nop                            1133
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi04++);
  // sqi.xyzw vf29, vi04        |  nop                            1134
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi04++);
  // BRANCH!
  // b L51                      |  nop                            1135
  bc = true;
  // sqi.xyzw vf23, vi04        |  nop                            1136
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi04++);
  if (bc) {
    goto L51;
  }

L67:
  // BRANCH!
  // b L53                      |  nop                            1137
  bc = true;
  // iaddi vi06, vi06, -0x1     |  nop                            1138
  vu.vi06 = vu.vi06 + -1;
  if (bc) {
    goto L53;
  }

L68:
  // sqi.xyzw vf24, vi04        |  sub.xyzw vf23, vf22, vf21      1139
  vu.vf23.sub(Mask::xyzw, vu.vf22, vu.vf21);
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  sub.xyzw vf26, vf25, vf24      1140
  vu.vf26.sub(Mask::xyzw, vu.vf25, vu.vf24);
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  sub.xyzw vf29, vf28, vf27      1141
  vu.vf29.sub(Mask::xyzw, vu.vf28, vu.vf27);
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
  // BRANCH!
  // ibeq vi03, vi04, L53       |  nop                            1142
  bc = (vu.vi03 == vu.vi04);
  // nop                        |  subx.w vf30, vf23, vf23        1143
  vu.vf30.sub(Mask::w, vu.vf23, vu.vf23.x());
  if (bc) {
    goto L53;
  }

  // nop                        |  subw.x vf30, vf21, vf21        1144
  vu.vf30.sub(Mask::x, vu.vf21, vu.vf21.w());
  // div Q, vf30.x, vf30.w      |  nop                            1145
  vu.Q = vu.vf30.x() / vu.vf30.w();
  // waitq                      |  nop                            1146
  // ASSERT(false);
  // nop                        |  mul.xyzw vf26, vf26, Q         1147
  vu.vf26.mul(Mask::xyzw, vu.vf26, vu.Q);
  // nop                        |  mul.xyzw vf29, vf29, Q         1148
  vu.vf29.mul(Mask::xyzw, vu.vf29, vu.Q);
  // nop                        |  mul.xyzw vf23, vf23, Q         1149
  vu.vf23.mul(Mask::xyzw, vu.vf23, vu.Q);
  // nop                        |  add.xyzw vf26, vf24, vf26      1150
  vu.vf26.add(Mask::xyzw, vu.vf24, vu.vf26);
  // nop                        |  add.xyzw vf29, vf27, vf29      1151
  vu.vf29.add(Mask::xyzw, vu.vf27, vu.vf29);
  // nop                        |  add.xyzw vf23, vf21, vf23      1152
  vu.vf23.add(Mask::xyzw, vu.vf21, vu.vf23);
  // iaddi vi06, vi06, 0x1      |  nop                            1153
  vu.vi06 = vu.vi06 + 1;
  // sqi.xyzw vf26, vi04        |  nop                            1154
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi04++);
  // sqi.xyzw vf29, vi04        |  nop                            1155
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi04++);
  // BRANCH!
  // b L53                      |  nop                            1156
  bc = true;
  // sqi.xyzw vf23, vi04        |  nop                            1157
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi04++);
  if (bc) {
    goto L53;
  }

L69:
  // nop                        |  sub.xyzw vf23, vf21, vf22      1158
  vu.vf23.sub(Mask::xyzw, vu.vf21, vu.vf22);
  // nop                        |  sub.xyzw vf26, vf24, vf25      1159
  vu.vf26.sub(Mask::xyzw, vu.vf24, vu.vf25);
  // nop                        |  sub.xyzw vf29, vf27, vf28      1160
  vu.vf29.sub(Mask::xyzw, vu.vf27, vu.vf28);
  // nop                        |  subx.w vf30, vf23, vf23        1161
  vu.vf30.sub(Mask::w, vu.vf23, vu.vf23.x());
  // nop                        |  subw.x vf30, vf22, vf22        1162
  vu.vf30.sub(Mask::x, vu.vf22, vu.vf22.w());
  // div Q, vf30.x, vf30.w      |  nop                            1163
  vu.Q = vu.vf30.x() / vu.vf30.w();
  // waitq                      |  nop                            1164
  // ASSERT(false);
  // nop                        |  mul.xyzw vf26, vf26, Q         1165
  vu.vf26.mul(Mask::xyzw, vu.vf26, vu.Q);
  // nop                        |  mul.xyzw vf29, vf29, Q         1166
  vu.vf29.mul(Mask::xyzw, vu.vf29, vu.Q);
  // nop                        |  mul.xyzw vf23, vf23, Q         1167
  vu.vf23.mul(Mask::xyzw, vu.vf23, vu.Q);
  // nop                        |  add.xyzw vf26, vf25, vf26      1168
  vu.vf26.add(Mask::xyzw, vu.vf25, vu.vf26);
  // nop                        |  add.xyzw vf29, vf28, vf29      1169
  vu.vf29.add(Mask::xyzw, vu.vf28, vu.vf29);
  // nop                        |  add.xyzw vf23, vf22, vf23      1170
  vu.vf23.add(Mask::xyzw, vu.vf22, vu.vf23);
  // sqi.xyzw vf26, vi04        |  nop                            1171
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi04++);
  // sqi.xyzw vf29, vi04        |  nop                            1172
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi04++);
  // BRANCH!
  // b L53                      |  nop                            1173
  bc = true;
  // sqi.xyzw vf23, vi04        |  nop                            1174
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi04++);
  if (bc) {
    goto L53;
  }
}