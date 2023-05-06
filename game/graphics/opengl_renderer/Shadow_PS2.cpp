#include "common/log/log.h"

#include "game/graphics/opengl_renderer/ShadowRenderer.h"

void ShadowRenderer::run_mscal10_vu2c() {
  // ENTER_10:
  // iaddiu vi01, vi00, 0x358   |  nop                            10
  vu.vi01 = 0x358; /* 856 */
  // iaddiu vi02, vi00, 0x364   |  nop                            11
  vu.vi02 = 0x364; /* 868 */
  // mfir.x vf01, vi01          |  nop                            12
  vu.vf01.mfir(Mask::x, vu.vi01);
  // mfir.y vf01, vi02          |  nop                            13
  vu.vf01.mfir(Mask::y, vu.vi02);
  // mfir.z vf01, vi01          |  nop                            14
  vu.vf01.mfir(Mask::z, vu.vi01);
  // mfir.w vf01, vi02          |  nop                            15
  vu.vf01.mfir(Mask::w, vu.vi02);
  // lq.xyzw vf02, 880(vi00)    |  nop                            16
  lq_buffer(Mask::xyzw, vu.vf02, 880);
  // lq.xyzw vf03, 882(vi00)    |  nop                            17
  lq_buffer(Mask::xyzw, vu.vf03, 882);
  // lq.xyzw vf04, 883(vi00)    |  nop                            18
  lq_buffer(Mask::xyzw, vu.vf04, 883);
  // lq.xyzw vf05, 884(vi00)    |  nop                            19
  lq_buffer(Mask::xyzw, vu.vf05, 884);
  // lq.xyzw vf12, 885(vi00)    |  nop :e                         20
  lq_buffer(Mask::xyzw, vu.vf12, 885);
  // lq.xyzw vf13, 881(vi00)    |  nop                            21
  lq_buffer(Mask::xyzw, vu.vf13, 881);
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

void fcand(u16& dest, u32 imm, u32 cf) {
  // dest = (cf & imm) ? 1 : 0;
  if ((cf & 0xFFFFFF) & (imm & 0xFFFFFF))
    dest = 1;
  else
    dest = 0;
  //  dest = cf & imm; // wrong
}

void fsand(u16& dest, u16 a, u16 b) {
  dest = a & b;
}

}  // namespace

void ShadowRenderer::handle_bal52() {
  // nop                        |  sub.xyzw vf16, vf15, vf14      736
  vu.vf16.sub(Mask::xyzw, vu.vf15, vu.vf14);
  // waitq                      |  mul.xyzw vf16, vf16, Q         737
  vu.vf16.mul(Mask::xyzw, vu.vf16, vu.Q);
  // jr vi11                    |  add.xyzw vf16, vf14, vf16      738
  vu.vf16.add(Mask::xyzw, vu.vf14, vu.vf16);
  // nop                        |  nop                            739
}

void ShadowRenderer::handle_jalr_to_end_block(u16 val, u32& first_flag, u32& second_flag) {
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
      ASSERT(false);  // bad because we don't set flags here
      // nop                        |  sub.xyzw vf16, vf15, vf14      736
      vu.vf16.sub(Mask::xyzw, vu.vf15, vu.vf14);
      // waitq                      |  mul.xyzw vf16, vf16, Q         737
      vu.vf16.mul(Mask::xyzw, vu.vf16, vu.Q);
      // jr vi11                    |  add.xyzw vf16, vf14, vf16      738
      vu.vf16.add(Mask::xyzw, vu.vf14, vu.vf16);
      // nop                        |  nop                            739
      return;
    default:
      ASSERT_MSG(false, fmt::format("unhandled end block: {}", val));
  }
}

void ShadowRenderer::run_mscal_vu2c(u16 imm) {
  u32 cf = 0, sf0, sf1;
  bool bc;
  switch (imm) {
    case 2:
      goto L2;
      break;
    case 4:
      goto L13;
    case 6:
      goto L21;

    default:
      lg::warn("didn't know mscal imm: {}", imm);
      // ASSERT(false);
  }
  // clang-format off

  L2:
  // iaddiu vi03, vi00, 0x158   |  nop                            22
  vu.vi03 = 0x158; /* 344 */
  // ilwr.x vi08, vi03          |  nop                            23
  ilw_buffer(Mask::x, vu.vi08, vu.vi03);
  // mtir vi02, vf01.x          |  nop                            24
  vu.vi02 = vu.vf01.x_as_u16();
  // iaddi vi03, vi03, 0x1      |  addw.z vf25, vf00, vf00        25
  vu.vf25.add(Mask::z, vu.vf00, vu.vf00.w());   vu.vi03 = vu.vi03 + 1;
  // lq.xyzw vf25, 888(vi00)    |  addw.z vf26, vf00, vf00        26
  vu.vf26.add(Mask::z, vu.vf00, vu.vf00.w());   lq_buffer(Mask::xyzw, vu.vf25, 888);
  // lq.xyzw vf29, 889(vi00)    |  addw.z vf27, vf00, vf00        27
  vu.vf27.add(Mask::z, vu.vf00, vu.vf00.w());   lq_buffer(Mask::xyzw, vu.vf29, 889);
  // lq.xyzw vf30, 891(vi00)    |  addw.z vf28, vf00, vf00        28
  vu.vf28.add(Mask::z, vu.vf00, vu.vf00.w());   lq_buffer(Mask::xyzw, vu.vf30, 891);
  // lq.xyzw vf06, 0(vi00)      |  nop                            29
  lq_buffer(Mask::xyzw, vu.vf06, 0);
  // lq.xyzw vf07, 1(vi00)      |  nop                            30
  lq_buffer(Mask::xyzw, vu.vf07, 1);
  // lq.xyzw vf08, 2(vi00)      |  nop                            31
  lq_buffer(Mask::xyzw, vu.vf08, 2);
  // lq.xyzw vf09, 3(vi00)      |  nop                            32
  lq_buffer(Mask::xyzw, vu.vf09, 3);
  // mtir vi01, vf01.y          |  nop                            33
  vu.vi01 = vu.vf01.y_as_u16();
  // sq.xyzw vf25, 0(vi02)      |  nop                            34
  sq_buffer(Mask::xyzw, vu.vf25, vu.vi02);
  // sq.xyzw vf29, 1(vi02)      |  nop                            35
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi02 + 1);
  // sq.xyzw vf30, 2(vi02)      |  nop                            36
  sq_buffer(Mask::xyzw, vu.vf30, vu.vi02 + 2);
  // sq.xyzw vf25, 0(vi01)      |  nop                            37
  sq_buffer(Mask::xyzw, vu.vf25, vu.vi01);
  // sq.xyzw vf29, 1(vi01)      |  nop                            38
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi01 + 1);
  // sq.xyzw vf30, 2(vi01)      |  nop                            39
  sq_buffer(Mask::xyzw, vu.vf30, vu.vi01 + 2);
  L3:
  // lqi.xyzw vf16, vi03        |  nop                            40
  lq_buffer(Mask::xyzw, vu.vf16, vu.vi03++);
  // nop                        |  nop                            41

  // nop                        |  nop                            42

  // nop                        |  nop                            43

  // mtir vi04, vf16.x          |  nop                            44
  vu.vi04 = vu.vf16.x_as_u16();
  // mtir vi05, vf16.y          |  nop                            45
  vu.vi05 = vu.vf16.y_as_u16();
  // mtir vi06, vf16.z          |  nop                            46
  vu.vi06 = vu.vf16.z_as_u16();
  // nop                        |  nop                            47

  // lq.xyzw vf17, 4(vi04)      |  nop                            48
  lq_buffer(Mask::xyzw, vu.vf17, vu.vi04 + 4);
  // lq.xyzw vf18, 4(vi05)      |  nop                            49
  lq_buffer(Mask::xyzw, vu.vf18, vu.vi05 + 4);
  // lq.xyzw vf19, 4(vi06)      |  nop                            50
  lq_buffer(Mask::xyzw, vu.vf19, vu.vi06 + 4);
  // nop                        |  mulaw.xyzw ACC, vf09, vf00     51
  vu.acc.mula(Mask::xyzw, vu.vf09, vu.vf00.w());
  // move.xyzw vf15, vf17       |  maddax.xyzw ACC, vf06, vf17    52
  vu.acc.madda(Mask::xyzw, vu.vf06, vu.vf17.x());   vu.vf15.move(Mask::xyzw, vu.vf17);
  // nop                        |  madday.xyzw ACC, vf07, vf17    53
  vu.acc.madda(Mask::xyzw, vu.vf07, vu.vf17.y());
  // nop                        |  maddz.xyzw vf17, vf08, vf17    54
  vu.acc.madd(Mask::xyzw, vu.vf17, vu.vf08, vu.vf17.z());
  // nop                        |  mulaw.xyzw ACC, vf09, vf00     55
  vu.acc.mula(Mask::xyzw, vu.vf09, vu.vf00.w());
  // nop                        |  sub.xyzw vf29, vf18, vf15      56
  vu.vf29.sub(Mask::xyzw, vu.vf18, vu.vf15);
  // nop                        |  sub.xyzw vf30, vf19, vf15      57
  vu.vf30.sub(Mask::xyzw, vu.vf19, vu.vf15);
  // div Q, vf12.x, vf17.w      |  maddax.xyzw ACC, vf06, vf18    58
  vu.acc.madda(Mask::xyzw, vu.vf06, vu.vf18.x());   vu.Q = vu.vf12.x() / vu.vf17.w();
  // nop                        |  mul.xyzw vf21, vf17, vf02      59
  vu.vf21.mul(Mask::xyzw, vu.vf17, vu.vf02);
  // nop                        |  madday.xyzw ACC, vf07, vf18    60
  vu.acc.madda(Mask::xyzw, vu.vf07, vu.vf18.y());
  // nop                        |  maddz.xyzw vf18, vf08, vf18    61
  vu.acc.madd(Mask::xyzw, vu.vf18, vu.vf08, vu.vf18.z());
  // nop                        |  mulaw.xyzw ACC, vf09, vf00     62
  vu.acc.mula(Mask::xyzw, vu.vf09, vu.vf00.w());
  // nop                        |  maddax.xyzw ACC, vf06, vf19    63
  vu.acc.madda(Mask::xyzw, vu.vf06, vu.vf19.x());
  // nop                        |  madday.xyzw ACC, vf07, vf19    64
  vu.acc.madda(Mask::xyzw, vu.vf07, vu.vf19.y());
  // div Q, vf12.x, vf18.w      |  mul.xyz vf17, vf17, Q          65
  vu.vf17.mul(Mask::xyz, vu.vf17, vu.Q);   vu.Q = vu.vf12.x() / vu.vf18.w();
  // nop                        |  maddz.xyzw vf19, vf08, vf19    66
  vu.acc.madd(Mask::xyzw, vu.vf19, vu.vf08, vu.vf19.z());
  // nop                        |  mul.xyzw vf22, vf18, vf02      67
  vu.vf22.mul(Mask::xyzw, vu.vf18, vu.vf02);
  // nop                        |  opmula.xyz ACC, vf29, vf30     68
  vu.acc.opmula(vu.vf29, vu.vf30);
  // nop                        |  opmsub.xyz vf29, vf30, vf29    69
  vu.acc.opmsub(vu.vf29, vu.vf30, vu.vf29);
  // nop                        |  add.xy vf25, vf17, vf03        70
  vu.vf25.add(Mask::xy, vu.vf17, vu.vf03);
  // nop                        |  add.xyzw vf17, vf17, vf05      71
  vu.vf17.add(Mask::xyzw, vu.vf17, vu.vf05);
  // div Q, vf12.x, vf19.w      |  mul.xyz vf18, vf18, Q          72
  vu.vf18.mul(Mask::xyz, vu.vf18, vu.Q);   vu.Q = vu.vf12.x() / vu.vf19.w();
  // nop                        |  mul.xyzw vf23, vf19, vf02      73
  vu.vf23.mul(Mask::xyzw, vu.vf19, vu.vf02);
  // nop                        |  mul.xyz vf29, vf29, vf15       74
  vu.vf29.mul(Mask::xyz, vu.vf29, vu.vf15);
  // nop                        |  mul.xy vf25, vf25, vf04        75
  vu.vf25.mul(Mask::xy, vu.vf25, vu.vf04);
  // nop                        |  mini.xyzw vf10, vf10, vf17     76
  vu.vf10.mini(Mask::xyzw, vu.vf10, vu.vf17);
  // nop                        |  max.xyzw vf11, vf11, vf17      77
  vu.vf11.max(Mask::xyzw, vu.vf11, vu.vf17);
  // nop                        |  ftoi4.xyzw vf17, vf17          78
  vu.vf17.ftoi4(Mask::xyzw, vu.vf17);
  // nop                        |  add.xy vf26, vf18, vf03        79
  vu.vf26.add(Mask::xy, vu.vf18, vu.vf03);
  // nop                        |  add.xyzw vf18, vf18, vf05      80
  vu.vf18.add(Mask::xyzw, vu.vf18, vu.vf05);
  // fcset 0x0                  |  addy.x vf29, vf29, vf29        81
  vu.vf29.add(Mask::x, vu.vf29, vu.vf29.y());   cf = 0x0;

  // nop                        |  mul.xyz vf19, vf19, Q          82
  vu.vf19.mul(Mask::xyz, vu.vf19, vu.Q);
  // sq.xyzw vf25, 4(vi02)      |  mini.xyzw vf10, vf10, vf18     83
  vu.vf10.mini(Mask::xyzw, vu.vf10, vu.vf18);   sq_buffer(Mask::xyzw, vu.vf25, vu.vi02 + 4);
  // sq.xyzw vf17, 5(vi02)      |  max.xyzw vf11, vf11, vf18      84
  vu.vf11.max(Mask::xyzw, vu.vf11, vu.vf18);   sq_buffer(Mask::xyzw, vu.vf17, vu.vi02 + 5);
  // nop                        |  clipw.xyz vf21, vf21           85
  cf = clip(vu.vf21, vu.vf21.w(), cf);
  // nop                        |  mul.xy vf26, vf26, vf04        86
  vu.vf26.mul(Mask::xy, vu.vf26, vu.vf04);
  // nop                        |  addz.x vf29, vf29, vf29        87
  sf0 = vu.vf29.add_and_set_sf_s(Mask::x, vu.vf29, vu.vf29.z());
  // nop                        |  ftoi4.xyzw vf18, vf18          88
  vu.vf18.ftoi4(Mask::xyzw, vu.vf18);
  // nop                        |  add.xy vf27, vf19, vf03        89
  vu.vf27.add(Mask::xy, vu.vf19, vu.vf03);
  // sq.xyzw vf26, 6(vi02)      |  add.xyzw vf19, vf19, vf05      90
  vu.vf19.add(Mask::xyzw, vu.vf19, vu.vf05);   sq_buffer(Mask::xyzw, vu.vf26, vu.vi02 + 6);
  // fsand vi01, 0x2            |  clipw.xyz vf22, vf22           91
  cf = clip(vu.vf22, vu.vf22.w(), cf);   fsand(vu.vi01, 0x2, sf0);

  // sq.xyzw vf18, 7(vi02)      |  clipw.xyz vf23, vf23           92
  cf = clip(vu.vf23, vu.vf23.w(), cf);   sq_buffer(Mask::xyzw, vu.vf18, vu.vi02 + 7);
  // BRANCH!
  // ibeq vi00, vi01, L4        |  mul.xy vf27, vf27, vf04        93
  vu.vf27.mul(Mask::xy, vu.vf27, vu.vf04);   bc = (vu.vi01 == 0);
  // nop                        |  mini.xyzw vf10, vf10, vf19     94
  vu.vf10.mini(Mask::xyzw, vu.vf10, vu.vf19);
  if (bc) { goto L4; }

  // BRANCH!
  // b L5                       |  nop                            95
  bc = true;
  // lq.xyzw vf31, 887(vi00)    |  max.xyzw vf11, vf11, vf19      96
  vu.vf11.max(Mask::xyzw, vu.vf11, vu.vf19);   lq_buffer(Mask::xyzw, vu.vf31, 887);
  if (bc) { goto L5; }

  L4:
  // lq.xyzw vf31, 886(vi00)    |  max.xyzw vf11, vf11, vf19      97
  vu.vf11.max(Mask::xyzw, vu.vf11, vu.vf19);   lq_buffer(Mask::xyzw, vu.vf31, 886);
  L5:
  // fcand vi01, 0x3ffff        |  ftoi4.xyzw vf19, vf19          98
  vu.vf19.ftoi4(Mask::xyzw, vu.vf19);   fcand(vu.vi01, 0x3ffff, cf);

  // BRANCH!
  // ibne vi00, vi01, L11       |  nop                            99
  bc = (vu.vi01 != 0);
  // iaddi vi08, vi08, -0x1     |  nop                            100
  vu.vi08 = vu.vi08 + -1;
  if (bc) { goto L11; }

  // sq.xyzw vf27, 8(vi02)      |  nop                            101
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi02 + 8);
  // sq.xyzw vf31, 3(vi02)      |  nop                            102
  sq_buffer(Mask::xyzw, vu.vf31, vu.vi02 + 3);
  // sq.xyzw vf19, 9(vi02)      |  nop                            103
  sq_buffer(Mask::xyzw, vu.vf19, vu.vi02 + 9);
  // xgkick vi02                |  nop                            104
  xgkick(vu.vi02);
  // mtir vi02, vf01.y          |  nop                            105
  vu.vi02 = vu.vf01.y_as_u16();
  // mr32.xyzw vf01, vf01       |  nop                            106
  vu.vf01.mr32(Mask::xyzw, vu.vf01);
  L6:
  // BRANCH!
  // ibgtz vi08, L3             |  nop                            107
  bc = ((s16)vu.vi08) > 0;
  // nop                        |  nop                            108

  if (bc) { goto L3; }

  // iaddiu vi03, vi00, 0x158   |  nop                            109
  vu.vi03 = 0x158; /* 344 */
  // ilwr.x vi08, vi03          |  nop                            110
  ilw_buffer(Mask::x, vu.vi08, vu.vi03);
  // iaddi vi03, vi03, 0x1      |  nop                            111
  vu.vi03 = vu.vi03 + 1;
  L7:
  // lqi.xyzw vf16, vi03        |  nop                            112
  lq_buffer(Mask::xyzw, vu.vf16, vu.vi03++);
  // nop                        |  nop                            113

  // nop                        |  nop                            114

  // nop                        |  nop                            115

  // mtir vi04, vf16.x          |  nop                            116
  vu.vi04 = vu.vf16.x_as_u16();
  // mtir vi05, vf16.y          |  nop                            117
  vu.vi05 = vu.vf16.y_as_u16();
  // mtir vi06, vf16.z          |  nop                            118
  vu.vi06 = vu.vf16.z_as_u16();
  // nop                        |  nop                            119

  // lq.xyzw vf17, 174(vi04)    |  nop                            120
  lq_buffer(Mask::xyzw, vu.vf17, vu.vi04 + 174);
  // lq.xyzw vf18, 174(vi06)    |  nop                            121
  lq_buffer(Mask::xyzw, vu.vf18, vu.vi06 + 174);
  // lq.xyzw vf19, 174(vi05)    |  nop                            122
  lq_buffer(Mask::xyzw, vu.vf19, vu.vi05 + 174);
  // nop                        |  mulaw.xyzw ACC, vf09, vf00     123
  vu.acc.mula(Mask::xyzw, vu.vf09, vu.vf00.w());
  // move.xyzw vf15, vf17       |  maddax.xyzw ACC, vf06, vf17    124
  vu.acc.madda(Mask::xyzw, vu.vf06, vu.vf17.x());   vu.vf15.move(Mask::xyzw, vu.vf17);
  // nop                        |  madday.xyzw ACC, vf07, vf17    125
  vu.acc.madda(Mask::xyzw, vu.vf07, vu.vf17.y());
  // nop                        |  maddz.xyzw vf17, vf08, vf17    126
  vu.acc.madd(Mask::xyzw, vu.vf17, vu.vf08, vu.vf17.z());
  // nop                        |  mulaw.xyzw ACC, vf09, vf00     127
  vu.acc.mula(Mask::xyzw, vu.vf09, vu.vf00.w());
  // nop                        |  sub.xyzw vf29, vf18, vf15      128
  vu.vf29.sub(Mask::xyzw, vu.vf18, vu.vf15);
  // nop                        |  sub.xyzw vf30, vf19, vf15      129
  vu.vf30.sub(Mask::xyzw, vu.vf19, vu.vf15);
  // div Q, vf12.x, vf17.w      |  maddax.xyzw ACC, vf06, vf18    130
  vu.acc.madda(Mask::xyzw, vu.vf06, vu.vf18.x());   vu.Q = vu.vf12.x() / vu.vf17.w();
  // nop                        |  mul.xyzw vf21, vf17, vf02      131
  vu.vf21.mul(Mask::xyzw, vu.vf17, vu.vf02);
  // nop                        |  madday.xyzw ACC, vf07, vf18    132
  vu.acc.madda(Mask::xyzw, vu.vf07, vu.vf18.y());
  // nop                        |  maddz.xyzw vf18, vf08, vf18    133
  vu.acc.madd(Mask::xyzw, vu.vf18, vu.vf08, vu.vf18.z());
  // nop                        |  mulaw.xyzw ACC, vf09, vf00     134
  vu.acc.mula(Mask::xyzw, vu.vf09, vu.vf00.w());
  // nop                        |  maddax.xyzw ACC, vf06, vf19    135
  vu.acc.madda(Mask::xyzw, vu.vf06, vu.vf19.x());
  // nop                        |  madday.xyzw ACC, vf07, vf19    136
  vu.acc.madda(Mask::xyzw, vu.vf07, vu.vf19.y());
  // div Q, vf12.x, vf18.w      |  mul.xyz vf17, vf17, Q          137
  vu.vf17.mul(Mask::xyz, vu.vf17, vu.Q);   vu.Q = vu.vf12.x() / vu.vf18.w();
  // nop                        |  maddz.xyzw vf19, vf08, vf19    138
  vu.acc.madd(Mask::xyzw, vu.vf19, vu.vf08, vu.vf19.z());
  // nop                        |  mul.xyzw vf22, vf18, vf02      139
  vu.vf22.mul(Mask::xyzw, vu.vf18, vu.vf02);
  // nop                        |  opmula.xyz ACC, vf29, vf30     140
  vu.acc.opmula(vu.vf29, vu.vf30);
  // nop                        |  opmsub.xyz vf29, vf30, vf29    141
  vu.acc.opmsub(vu.vf29, vu.vf30, vu.vf29);
  // nop                        |  add.xy vf25, vf17, vf03        142
  vu.vf25.add(Mask::xy, vu.vf17, vu.vf03);
  // nop                        |  add.xyzw vf17, vf17, vf05      143
  vu.vf17.add(Mask::xyzw, vu.vf17, vu.vf05);
  // div Q, vf12.x, vf19.w      |  mul.xyz vf18, vf18, Q          144
  vu.vf18.mul(Mask::xyz, vu.vf18, vu.Q);   vu.Q = vu.vf12.x() / vu.vf19.w();
  // nop                        |  mul.xyzw vf23, vf19, vf02      145
  vu.vf23.mul(Mask::xyzw, vu.vf19, vu.vf02);
  // nop                        |  mul.xyz vf29, vf29, vf15       146
  vu.vf29.mul(Mask::xyz, vu.vf29, vu.vf15);
  // nop                        |  mul.xy vf25, vf25, vf04        147
  vu.vf25.mul(Mask::xy, vu.vf25, vu.vf04);
  // nop                        |  mini.xyzw vf10, vf10, vf17     148
  vu.vf10.mini(Mask::xyzw, vu.vf10, vu.vf17);
  // nop                        |  max.xyzw vf11, vf11, vf17      149
  vu.vf11.max(Mask::xyzw, vu.vf11, vu.vf17);
  // nop                        |  ftoi4.xyzw vf17, vf17          150
  vu.vf17.ftoi4(Mask::xyzw, vu.vf17);
  // nop                        |  add.xy vf26, vf18, vf03        151
  vu.vf26.add(Mask::xy, vu.vf18, vu.vf03);
  // nop                        |  add.xyzw vf18, vf18, vf05      152
  vu.vf18.add(Mask::xyzw, vu.vf18, vu.vf05);
  // fcset 0x0                  |  addy.x vf29, vf29, vf29        153
  vu.vf29.add(Mask::x, vu.vf29, vu.vf29.y());   cf = 0x0;

  // nop                        |  mul.xyz vf19, vf19, Q          154
  vu.vf19.mul(Mask::xyz, vu.vf19, vu.Q);
  // sq.xyzw vf25, 4(vi02)      |  mini.xyzw vf10, vf10, vf18     155
  vu.vf10.mini(Mask::xyzw, vu.vf10, vu.vf18);   sq_buffer(Mask::xyzw, vu.vf25, vu.vi02 + 4);
  // sq.xyzw vf17, 5(vi02)      |  max.xyzw vf11, vf11, vf18      156
  vu.vf11.max(Mask::xyzw, vu.vf11, vu.vf18);   sq_buffer(Mask::xyzw, vu.vf17, vu.vi02 + 5);
  // nop                        |  clipw.xyz vf21, vf21           157
  cf = clip(vu.vf21, vu.vf21.w(), cf);
  // nop                        |  mul.xy vf26, vf26, vf04        158
  vu.vf26.mul(Mask::xy, vu.vf26, vu.vf04);
  // iaddi vi08, vi08, -0x1     |  addz.x vf29, vf29, vf29        159
  sf0 = vu.vf29.add_and_set_sf_s(Mask::x, vu.vf29, vu.vf29.z());   vu.vi08 = vu.vi08 + -1;
  // nop                        |  ftoi4.xyzw vf18, vf18          160
  vu.vf18.ftoi4(Mask::xyzw, vu.vf18);
  // nop                        |  add.xy vf27, vf19, vf03        161
  vu.vf27.add(Mask::xy, vu.vf19, vu.vf03);
  // sq.xyzw vf26, 6(vi02)      |  add.xyzw vf19, vf19, vf05      162
  vu.vf19.add(Mask::xyzw, vu.vf19, vu.vf05);   sq_buffer(Mask::xyzw, vu.vf26, vu.vi02 + 6);
  // fsand vi01, 0x2            |  clipw.xyz vf22, vf22           163
  cf = clip(vu.vf22, vu.vf22.w(), cf);   fsand(vu.vi01, 0x2, sf0);

  // sq.xyzw vf18, 7(vi02)      |  clipw.xyz vf23, vf23           164
  cf = clip(vu.vf23, vu.vf23.w(), cf);   sq_buffer(Mask::xyzw, vu.vf18, vu.vi02 + 7);
  // BRANCH!
  // ibeq vi00, vi01, L8        |  mul.xy vf27, vf27, vf04        165
  vu.vf27.mul(Mask::xy, vu.vf27, vu.vf04);   bc = (vu.vi01 == 0);
  // nop                        |  mini.xyzw vf10, vf10, vf19     166
  vu.vf10.mini(Mask::xyzw, vu.vf10, vu.vf19);
  if (bc) { goto L8; }

  // BRANCH!
  // b L9                       |  nop                            167
  bc = true;
  // lq.xyzw vf31, 887(vi00)    |  max.xyzw vf11, vf11, vf19      168
  vu.vf11.max(Mask::xyzw, vu.vf11, vu.vf19);   lq_buffer(Mask::xyzw, vu.vf31, 887);
  if (bc) { goto L9; }

  L8:
  // lq.xyzw vf31, 886(vi00)    |  max.xyzw vf11, vf11, vf19      169
  vu.vf11.max(Mask::xyzw, vu.vf11, vu.vf19);   lq_buffer(Mask::xyzw, vu.vf31, 886);
  L9:
  // fcand vi01, 0x3ffff        |  ftoi4.xyzw vf19, vf19          170
  vu.vf19.ftoi4(Mask::xyzw, vu.vf19);   fcand(vu.vi01, 0x3ffff, cf);

  // BRANCH!
  // ibne vi00, vi01, L12       |  nop                            171
  bc = (vu.vi01 != 0);
  // sq.xyzw vf27, 8(vi02)      |  nop                            172
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi02 + 8);
  if (bc) { goto L12; }

  // sq.xyzw vf31, 3(vi02)      |  nop                            173
  sq_buffer(Mask::xyzw, vu.vf31, vu.vi02 + 3);
  // sq.xyzw vf19, 9(vi02)      |  nop                            174
  sq_buffer(Mask::xyzw, vu.vf19, vu.vi02 + 9);
  // xgkick vi02                |  nop                            175
  xgkick(vu.vi02);
  // mtir vi02, vf01.y          |  nop                            176
  vu.vi02 = vu.vf01.y_as_u16();
  // mr32.xyzw vf01, vf01       |  nop                            177
  vu.vf01.mr32(Mask::xyzw, vu.vf01);
  L10:
  // BRANCH!
  // ibgtz vi08, L7             |  nop                            178
  bc = ((s16)vu.vi08) > 0;
  // nop                        |  nop                            179

  if (bc) { goto L7; }

  // nop                        |  nop :e                         180

  // nop                        |  nop                            181

  return;

  L11:
  // sq.xyzw vf21, 1000(vi00)   |  nop                            182
  sq_buffer(Mask::xyzw, vu.vf21, 1000);
  // sq.xyzw vf22, 1003(vi00)   |  nop                            183
  sq_buffer(Mask::xyzw, vu.vf22, 1003);
  // sq.xyzw vf23, 1006(vi00)   |  nop                            184
  sq_buffer(Mask::xyzw, vu.vf23, 1006);
  // sq.xyzw vf31, 942(vi00)    |  nop                            185
  sq_buffer(Mask::xyzw, vu.vf31, 942);
  // mfir.x vf29, vi02          |  nop                            186
  vu.vf29.mfir(Mask::x, vu.vi02);
  // mfir.y vf29, vi03          |  nop                            187
  vu.vf29.mfir(Mask::y, vu.vi03);
  // mfir.z vf29, vi07          |  nop                            188
  vu.vf29.mfir(Mask::z, vu.vi07);
  // BRANCH!
  // bal vi15, L36              |  nop                            189
  // mfir.w vf29, vi08          |  nop                            190
  vu.vf29.mfir(Mask::w, vu.vi08);
  vu.vi15 = 191;
  goto L36;

  INSTR_191:
  // mtir vi08, vf29.w          |  nop                            191
  vu.vi08 = vu.vf29.w_as_u16();
  // mtir vi03, vf29.y          |  nop                            192
  vu.vi03 = vu.vf29.y_as_u16();
  // mtir vi07, vf29.z          |  nop                            193
  vu.vi07 = vu.vf29.z_as_u16();
  // BRANCH!
  // b L6                       |  nop                            194
  bc = true;
  // mtir vi02, vf29.x          |  nop                            195
  vu.vi02 = vu.vf29.x_as_u16();
  if (bc) { goto L6; }

  L12:
  // sq.xyzw vf21, 1000(vi00)   |  nop                            196
  sq_buffer(Mask::xyzw, vu.vf21, 1000);
  // sq.xyzw vf22, 1003(vi00)   |  nop                            197
  sq_buffer(Mask::xyzw, vu.vf22, 1003);
  // sq.xyzw vf23, 1006(vi00)   |  nop                            198
  sq_buffer(Mask::xyzw, vu.vf23, 1006);
  // sq.xyzw vf31, 942(vi00)    |  nop                            199
  sq_buffer(Mask::xyzw, vu.vf31, 942);
  // mfir.x vf29, vi02          |  nop                            200
  vu.vf29.mfir(Mask::x, vu.vi02);
  // mfir.y vf29, vi03          |  nop                            201
  vu.vf29.mfir(Mask::y, vu.vi03);
  // mfir.z vf29, vi07          |  nop                            202
  vu.vf29.mfir(Mask::z, vu.vi07);
  // BRANCH!
  // bal vi15, L36              |  nop                            203
  // mfir.w vf29, vi08          |  nop                            204
  vu.vf29.mfir(Mask::w, vu.vi08);
  vu.vi15 = 205;
  goto L36;

  INSTR_205:
  // mtir vi08, vf29.w          |  nop                            205
  vu.vi08 = vu.vf29.w_as_u16();
  // mtir vi03, vf29.y          |  nop                            206
  vu.vi03 = vu.vf29.y_as_u16();
  // mtir vi07, vf29.z          |  nop                            207
  vu.vi07 = vu.vf29.z_as_u16();
  // BRANCH!
  // b L10                      |  nop                            208
  bc = true;
  // mtir vi02, vf29.x          |  nop                            209
  vu.vi02 = vu.vf29.x_as_u16();
  if (bc) { goto L10; }

  L13:
  // iaddiu vi03, vi00, 0x258   |  nop                            210
  vu.vi03 = 0x258; /* 600 */
  // ilwr.x vi08, vi03          |  nop                            211
  ilw_buffer(Mask::x, vu.vi08, vu.vi03);
  // mtir vi02, vf01.x          |  nop                            212
  vu.vi02 = vu.vf01.x_as_u16();
  // iaddi vi03, vi03, 0x1      |  addw.z vf25, vf00, vf00        213
  vu.vf25.add(Mask::z, vu.vf00, vu.vf00.w());   vu.vi03 = vu.vi03 + 1;
  // lq.xyzw vf25, 888(vi00)    |  addw.z vf26, vf00, vf00        214
  vu.vf26.add(Mask::z, vu.vf00, vu.vf00.w());   lq_buffer(Mask::xyzw, vu.vf25, 888);
  // lq.xyzw vf29, 889(vi00)    |  addw.z vf27, vf00, vf00        215
  vu.vf27.add(Mask::z, vu.vf00, vu.vf00.w());   lq_buffer(Mask::xyzw, vu.vf29, 889);
  // lq.xyzw vf30, 892(vi00)    |  nop                            216
  lq_buffer(Mask::xyzw, vu.vf30, 892);
  // lq.xyzw vf06, 0(vi00)      |  nop                            217
  lq_buffer(Mask::xyzw, vu.vf06, 0);
  // lq.xyzw vf07, 1(vi00)      |  nop                            218
  lq_buffer(Mask::xyzw, vu.vf07, 1);
  // lq.xyzw vf08, 2(vi00)      |  nop                            219
  lq_buffer(Mask::xyzw, vu.vf08, 2);
  // lq.xyzw vf09, 3(vi00)      |  nop                            220
  lq_buffer(Mask::xyzw, vu.vf09, 3);
  // mtir vi01, vf01.y          |  nop                            221
  vu.vi01 = vu.vf01.y_as_u16();
  // sq.xyzw vf25, 0(vi02)      |  nop                            222
  sq_buffer(Mask::xyzw, vu.vf25, vu.vi02);
  // sq.xyzw vf29, 1(vi02)      |  nop                            223
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi02 + 1);
  // sq.xyzw vf30, 2(vi02)      |  nop                            224
  sq_buffer(Mask::xyzw, vu.vf30, vu.vi02 + 2);
  // sq.xyzw vf25, 0(vi01)      |  nop                            225
  sq_buffer(Mask::xyzw, vu.vf25, vu.vi01);
  // sq.xyzw vf29, 1(vi01)      |  nop                            226
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi01 + 1);
  // sq.xyzw vf30, 2(vi01)      |  nop                            227
  sq_buffer(Mask::xyzw, vu.vf30, vu.vi01 + 2);
  L14:
  // lqi.xyzw vf16, vi03        |  nop                            228
  lq_buffer(Mask::xyzw, vu.vf16, vu.vi03++);
  // nop                        |  nop                            229

  // nop                        |  nop                            230

  // nop                        |  nop                            231

  // mtir vi06, vf16.z          |  nop                            232
  vu.vi06 = vu.vf16.z_as_u16();
  // mtir vi04, vf16.x          |  nop                            233
  vu.vi04 = vu.vf16.x_as_u16();
  // mtir vi05, vf16.y          |  nop                            234
  vu.vi05 = vu.vf16.y_as_u16();
  // BRANCH!
  // ibeq vi00, vi06, L15       |  nop                            235
  bc = (vu.vi06 == 0);
  // nop                        |  nop                            236

  if (bc) { goto L15; }

  // lq.xyzw vf17, 4(vi04)      |  nop                            237
  lq_buffer(Mask::xyzw, vu.vf17, vu.vi04 + 4);
  // lq.xyzw vf18, 4(vi05)      |  nop                            238
  lq_buffer(Mask::xyzw, vu.vf18, vu.vi05 + 4);
  // lq.xyzw vf19, 174(vi05)    |  nop                            239
  lq_buffer(Mask::xyzw, vu.vf19, vu.vi05 + 174);
  // BRANCH!
  // b L16                      |  nop                            240
  bc = true;
  // lq.xyzw vf20, 174(vi04)    |  nop                            241
  lq_buffer(Mask::xyzw, vu.vf20, vu.vi04 + 174);
  if (bc) { goto L16; }

  L15:
  // lq.xyzw vf17, 4(vi05)      |  nop                            242
  lq_buffer(Mask::xyzw, vu.vf17, vu.vi05 + 4);
  // lq.xyzw vf18, 4(vi04)      |  nop                            243
  lq_buffer(Mask::xyzw, vu.vf18, vu.vi04 + 4);
  // lq.xyzw vf19, 174(vi04)    |  nop                            244
  lq_buffer(Mask::xyzw, vu.vf19, vu.vi04 + 174);
  // lq.xyzw vf20, 174(vi05)    |  nop                            245
  lq_buffer(Mask::xyzw, vu.vf20, vu.vi05 + 174);
  L16:
  // move.xyzw vf15, vf17       |  mulaw.xyzw ACC, vf09, vf00     246
  vu.acc.mula(Mask::xyzw, vu.vf09, vu.vf00.w());   vu.vf15.move(Mask::xyzw, vu.vf17);
  // nop                        |  maddax.xyzw ACC, vf06, vf17    247
  vu.acc.madda(Mask::xyzw, vu.vf06, vu.vf17.x());
  // nop                        |  madday.xyzw ACC, vf07, vf17    248
  vu.acc.madda(Mask::xyzw, vu.vf07, vu.vf17.y());
  // nop                        |  maddz.xyzw vf17, vf08, vf17    249
  vu.acc.madd(Mask::xyzw, vu.vf17, vu.vf08, vu.vf17.z());
  // nop                        |  mulaw.xyzw ACC, vf09, vf00     250
  vu.acc.mula(Mask::xyzw, vu.vf09, vu.vf00.w());
  // nop                        |  sub.xyzw vf29, vf18, vf15      251
  vu.vf29.sub(Mask::xyzw, vu.vf18, vu.vf15);
  // nop                        |  sub.xyzw vf30, vf19, vf15      252
  vu.vf30.sub(Mask::xyzw, vu.vf19, vu.vf15);
  // div Q, vf12.x, vf17.w      |  maddax.xyzw ACC, vf06, vf18    253
  vu.acc.madda(Mask::xyzw, vu.vf06, vu.vf18.x());   vu.Q = vu.vf12.x() / vu.vf17.w();
  // nop                        |  mul.xyzw vf21, vf17, vf02      254
  vu.vf21.mul(Mask::xyzw, vu.vf17, vu.vf02);
  // nop                        |  madday.xyzw ACC, vf07, vf18    255
  vu.acc.madda(Mask::xyzw, vu.vf07, vu.vf18.y());
  // nop                        |  maddz.xyzw vf18, vf08, vf18    256
  vu.acc.madd(Mask::xyzw, vu.vf18, vu.vf08, vu.vf18.z());
  // nop                        |  mulaw.xyzw ACC, vf09, vf00     257
  vu.acc.mula(Mask::xyzw, vu.vf09, vu.vf00.w());
  // nop                        |  maddax.xyzw ACC, vf06, vf19    258
  vu.acc.madda(Mask::xyzw, vu.vf06, vu.vf19.x());
  // nop                        |  madday.xyzw ACC, vf07, vf19    259
  vu.acc.madda(Mask::xyzw, vu.vf07, vu.vf19.y());
  // div Q, vf12.x, vf18.w      |  mul.xyz vf17, vf17, Q          260
  vu.vf17.mul(Mask::xyz, vu.vf17, vu.Q);   vu.Q = vu.vf12.x() / vu.vf18.w();
  // nop                        |  maddz.xyzw vf19, vf08, vf19    261
  vu.acc.madd(Mask::xyzw, vu.vf19, vu.vf08, vu.vf19.z());
  // nop                        |  mulaw.xyzw ACC, vf09, vf00     262
  vu.acc.mula(Mask::xyzw, vu.vf09, vu.vf00.w());
  // nop                        |  maddax.xyzw ACC, vf06, vf20    263
  vu.acc.madda(Mask::xyzw, vu.vf06, vu.vf20.x());
  // nop                        |  madday.xyzw ACC, vf07, vf20    264
  vu.acc.madda(Mask::xyzw, vu.vf07, vu.vf20.y());
  // nop                        |  maddz.xyzw vf20, vf08, vf20    265
  vu.acc.madd(Mask::xyzw, vu.vf20, vu.vf08, vu.vf20.z());
  // nop                        |  mul.xyzw vf22, vf18, vf02      266
  vu.vf22.mul(Mask::xyzw, vu.vf18, vu.vf02);
  // div Q, vf12.x, vf19.w      |  mul.xyz vf18, vf18, Q          267
  vu.vf18.mul(Mask::xyz, vu.vf18, vu.Q);   vu.Q = vu.vf12.x() / vu.vf19.w();
  // nop                        |  add.xy vf25, vf17, vf03        268
  vu.vf25.add(Mask::xy, vu.vf17, vu.vf03);
  // nop                        |  add.xyzw vf17, vf17, vf05      269
  vu.vf17.add(Mask::xyzw, vu.vf17, vu.vf05);
  // nop                        |  opmula.xyz ACC, vf29, vf30     270
  vu.acc.opmula(vu.vf29, vu.vf30);
  // nop                        |  opmsub.xyz vf29, vf30, vf29    271
  vu.acc.opmsub(vu.vf29, vu.vf30, vu.vf29);
  // nop                        |  mul.xyzw vf23, vf19, vf02      272
  vu.vf23.mul(Mask::xyzw, vu.vf19, vu.vf02);
  // nop                        |  mul.xyz vf29, vf29, vf15       273
  vu.vf29.mul(Mask::xyz, vu.vf29, vu.vf15);
  // div Q, vf12.x, vf20.w      |  mul.xyz vf19, vf19, Q          274
  vu.vf19.mul(Mask::xyz, vu.vf19, vu.Q);   vu.Q = vu.vf12.x() / vu.vf20.w();
  // nop                        |  mul.xyzw vf24, vf20, vf02      275
  vu.vf24.mul(Mask::xyzw, vu.vf20, vu.vf02);
  // nop                        |  mul.xy vf25, vf25, vf04        276
  vu.vf25.mul(Mask::xy, vu.vf25, vu.vf04);
  // nop                        |  mini.xyzw vf10, vf10, vf17     277
  vu.vf10.mini(Mask::xyzw, vu.vf10, vu.vf17);
  // nop                        |  max.xyzw vf11, vf11, vf17      278
  vu.vf11.max(Mask::xyzw, vu.vf11, vu.vf17);
  // nop                        |  ftoi4.xyzw vf17, vf17          279
  vu.vf17.ftoi4(Mask::xyzw, vu.vf17);
  // nop                        |  add.xy vf26, vf18, vf03        280
  vu.vf26.add(Mask::xy, vu.vf18, vu.vf03);
  // nop                        |  mul.xyz vf20, vf20, Q          281
  vu.vf20.mul(Mask::xyz, vu.vf20, vu.Q);
  // nop                        |  add.xyzw vf18, vf18, vf05      282
  vu.vf18.add(Mask::xyzw, vu.vf18, vu.vf05);
  // nop                        |  addy.x vf29, vf29, vf29        283
  vu.vf29.add(Mask::x, vu.vf29, vu.vf29.y());
  // sq.xyzw vf25, 4(vi02)      |  mini.xyzw vf10, vf10, vf18     284
  vu.vf10.mini(Mask::xyzw, vu.vf10, vu.vf18);   sq_buffer(Mask::xyzw, vu.vf25, vu.vi02 + 4);
  // sq.xyzw vf17, 5(vi02)      |  max.xyzw vf11, vf11, vf18      285
  vu.vf11.max(Mask::xyzw, vu.vf11, vu.vf18);   sq_buffer(Mask::xyzw, vu.vf17, vu.vi02 + 5);
  // nop                        |  clipw.xyz vf21, vf21           286
  cf = clip(vu.vf21, vu.vf21.w(), cf);
  // nop                        |  mul.xy vf26, vf26, vf04        287
  vu.vf26.mul(Mask::xy, vu.vf26, vu.vf04);
  // nop                        |  addz.x vf29, vf29, vf29        288
  sf0 = vu.vf29.add_and_set_sf_s(Mask::x, vu.vf29, vu.vf29.z());
  // nop                        |  ftoi4.xyzw vf18, vf18          289
  vu.vf18.ftoi4(Mask::xyzw, vu.vf18);
  // nop                        |  add.xy vf27, vf19, vf03        290
  vu.vf27.add(Mask::xy, vu.vf19, vu.vf03);
  // sq.xyzw vf26, 6(vi02)      |  add.xyzw vf19, vf19, vf05      291
  vu.vf19.add(Mask::xyzw, vu.vf19, vu.vf05);   sq_buffer(Mask::xyzw, vu.vf26, vu.vi02 + 6);
  // fsand vi01, 0x2            |  clipw.xyz vf22, vf22           292
  cf = clip(vu.vf22, vu.vf22.w(), cf);   fsand(vu.vi01, 0x2, sf0);

  // sq.xyzw vf18, 7(vi02)      |  clipw.xyz vf23, vf23           293
  cf = clip(vu.vf23, vu.vf23.w(), cf);   sq_buffer(Mask::xyzw, vu.vf18, vu.vi02 + 7);
  // nop                        |  clipw.xyz vf24, vf24           294
  cf = clip(vu.vf24, vu.vf24.w(), cf);
  // nop                        |  add.xy vf28, vf20, vf03        295
  vu.vf28.add(Mask::xy, vu.vf20, vu.vf03);
  // nop                        |  add.xyzw vf20, vf20, vf05      296
  vu.vf20.add(Mask::xyzw, vu.vf20, vu.vf05);
  // nop                        |  mul.xy vf27, vf27, vf04        297
  vu.vf27.mul(Mask::xy, vu.vf27, vu.vf04);
  // nop                        |  mini.xyzw vf10, vf10, vf19     298
  vu.vf10.mini(Mask::xyzw, vu.vf10, vu.vf19);
  // nop                        |  max.xyzw vf11, vf11, vf19      299
  vu.vf11.max(Mask::xyzw, vu.vf11, vu.vf19);
  // BRANCH!
  // ibeq vi00, vi01, L17       |  mul.xy vf28, vf28, vf04        300
  vu.vf28.mul(Mask::xy, vu.vf28, vu.vf04);   bc = (vu.vi01 == 0);
  // nop                        |  mini.xyzw vf10, vf10, vf20     301
  vu.vf10.mini(Mask::xyzw, vu.vf10, vu.vf20);
  if (bc) { goto L17; }

  // BRANCH!
  // b L18                      |  nop                            302
  bc = true;
  // lq.xyzw vf31, 887(vi00)    |  max.xyzw vf11, vf11, vf20      303
  vu.vf11.max(Mask::xyzw, vu.vf11, vu.vf20);   lq_buffer(Mask::xyzw, vu.vf31, 887);
  if (bc) { goto L18; }

  L17:
  // lq.xyzw vf31, 886(vi00)    |  max.xyzw vf11, vf11, vf20      304
  vu.vf11.max(Mask::xyzw, vu.vf11, vu.vf20);   lq_buffer(Mask::xyzw, vu.vf31, 886);
  L18:
  // fcand vi01, 0xffffff       |  ftoi4.xyzw vf19, vf19          305
  vu.vf19.ftoi4(Mask::xyzw, vu.vf19);   fcand(vu.vi01, 0xffffff, cf);

  // BRANCH!
  // ibne vi00, vi01, L20       |  max.xyzw vf11, vf11, vf20      306
  vu.vf11.max(Mask::xyzw, vu.vf11, vu.vf20);   bc = (vu.vi01 != 0);
  // iaddi vi08, vi08, -0x1     |  nop                            307
  vu.vi08 = vu.vi08 + -1;
  if (bc) { goto L20; }

  // sq.xyzw vf27, 8(vi02)      |  ftoi4.xyzw vf20, vf20          308
  vu.vf20.ftoi4(Mask::xyzw, vu.vf20);   sq_buffer(Mask::xyzw, vu.vf27, vu.vi02 + 8);
  // sq.xyzw vf31, 3(vi02)      |  nop                            309
  sq_buffer(Mask::xyzw, vu.vf31, vu.vi02 + 3);
  // sq.xyzw vf19, 9(vi02)      |  nop                            310
  sq_buffer(Mask::xyzw, vu.vf19, vu.vi02 + 9);
  // sq.xyzw vf28, 10(vi02)     |  nop                            311
  sq_buffer(Mask::xyzw, vu.vf28, vu.vi02 + 10);
  // sq.xyzw vf20, 11(vi02)     |  nop                            312
  sq_buffer(Mask::xyzw, vu.vf20, vu.vi02 + 11);
  // xgkick vi02                |  nop                            313
  xgkick(vu.vi02);
  // mtir vi02, vf01.y          |  nop                            314
  vu.vi02 = vu.vf01.y_as_u16();
  // mr32.xyzw vf01, vf01       |  nop                            315
  vu.vf01.mr32(Mask::xyzw, vu.vf01);
  L19:
  // BRANCH!
  // ibgtz vi08, L14            |  nop                            316
  bc = ((s16)vu.vi08) > 0;
  // nop                        |  nop                            317

  if (bc) { goto L14; }

  // nop                        |  nop :e                         318

  // nop                        |  nop                            319

  return;

  L20:
  // sq.xyzw vf21, 1000(vi00)   |  nop                            320
  sq_buffer(Mask::xyzw, vu.vf21, 1000);
  // sq.xyzw vf22, 1003(vi00)   |  nop                            321
  sq_buffer(Mask::xyzw, vu.vf22, 1003);
  // sq.xyzw vf23, 1006(vi00)   |  nop                            322
  sq_buffer(Mask::xyzw, vu.vf23, 1006);
  // sq.xyzw vf24, 1009(vi00)   |  nop                            323
  sq_buffer(Mask::xyzw, vu.vf24, 1009);
  // sq.xyzw vf31, 942(vi00)    |  nop                            324
  sq_buffer(Mask::xyzw, vu.vf31, 942);
  // mfir.x vf29, vi02          |  nop                            325
  vu.vf29.mfir(Mask::x, vu.vi02);
  // mfir.y vf29, vi03          |  nop                            326
  vu.vf29.mfir(Mask::y, vu.vi03);
  // mfir.z vf29, vi07          |  nop                            327
  vu.vf29.mfir(Mask::z, vu.vi07);
  // BRANCH!
  // bal vi15, L37              |  nop                            328
  // mfir.w vf29, vi08          |  nop                            329
  vu.vf29.mfir(Mask::w, vu.vi08);
  vu.vi15 = 330;
  goto L37;

  INSTR_330:
  // mtir vi08, vf29.w          |  nop                            330
  vu.vi08 = vu.vf29.w_as_u16();
  // mtir vi03, vf29.y          |  nop                            331
  vu.vi03 = vu.vf29.y_as_u16();
  // mtir vi07, vf29.z          |  nop                            332
  vu.vi07 = vu.vf29.z_as_u16();
  // BRANCH!
  // b L19                      |  nop                            333
  bc = true;
  // mtir vi02, vf29.x          |  nop                            334
  vu.vi02 = vu.vf29.x_as_u16();
  if (bc) { goto L19; }

  L21:
  // iaddiu vi03, vi00, 0x158   |  nop                            335
  vu.vi03 = 0x158; /* 344 */
  // ilwr.x vi08, vi03          |  nop                            336
  ilw_buffer(Mask::x, vu.vi08, vu.vi03);
  // mtir vi02, vf01.x          |  nop                            337
  vu.vi02 = vu.vf01.x_as_u16();
  // iaddi vi03, vi03, 0x1      |  addw.z vf25, vf00, vf00        338
  vu.vf25.add(Mask::z, vu.vf00, vu.vf00.w());   vu.vi03 = vu.vi03 + 1;
  // lq.xyzw vf25, 888(vi00)    |  addw.z vf26, vf00, vf00        339
  vu.vf26.add(Mask::z, vu.vf00, vu.vf00.w());   lq_buffer(Mask::xyzw, vu.vf25, 888);
  // lq.xyzw vf29, 889(vi00)    |  addw.z vf27, vf00, vf00        340
  vu.vf27.add(Mask::z, vu.vf00, vu.vf00.w());   lq_buffer(Mask::xyzw, vu.vf29, 889);
  // lq.xyzw vf30, 891(vi00)    |  addw.z vf28, vf00, vf00        341
  vu.vf28.add(Mask::z, vu.vf00, vu.vf00.w());   lq_buffer(Mask::xyzw, vu.vf30, 891);
  // lq.xyzw vf06, 0(vi00)      |  nop                            342
  lq_buffer(Mask::xyzw, vu.vf06, 0);
  // lq.xyzw vf07, 1(vi00)      |  nop                            343
  lq_buffer(Mask::xyzw, vu.vf07, 1);
  // lq.xyzw vf08, 2(vi00)      |  nop                            344
  lq_buffer(Mask::xyzw, vu.vf08, 2);
  // lq.xyzw vf09, 3(vi00)      |  nop                            345
  lq_buffer(Mask::xyzw, vu.vf09, 3);
  // mtir vi01, vf01.y          |  nop                            346
  vu.vi01 = vu.vf01.y_as_u16();
  // sq.xyzw vf25, 0(vi02)      |  nop                            347
  sq_buffer(Mask::xyzw, vu.vf25, vu.vi02);
  // sq.xyzw vf29, 1(vi02)      |  nop                            348
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi02 + 1);
  // sq.xyzw vf30, 2(vi02)      |  nop                            349
  sq_buffer(Mask::xyzw, vu.vf30, vu.vi02 + 2);
  // sq.xyzw vf25, 0(vi01)      |  nop                            350
  sq_buffer(Mask::xyzw, vu.vf25, vu.vi01);
  // sq.xyzw vf29, 1(vi01)      |  nop                            351
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi01 + 1);
  // sq.xyzw vf30, 2(vi01)      |  nop                            352
  sq_buffer(Mask::xyzw, vu.vf30, vu.vi01 + 2);
  L22:
  // lqi.xyzw vf16, vi03        |  nop                            353
  lq_buffer(Mask::xyzw, vu.vf16, vu.vi03++);
  // nop                        |  nop                            354

  // nop                        |  nop                            355

  // nop                        |  nop                            356

  // mtir vi01, vf16.w          |  nop                            357
  vu.vi01 = vu.vf16.w_as_u16();
  // mtir vi04, vf16.x          |  nop                            358
  vu.vi04 = vu.vf16.x_as_u16();
  // mtir vi05, vf16.y          |  nop                            359
  vu.vi05 = vu.vf16.y_as_u16();
  // mtir vi06, vf16.z          |  nop                            360
  vu.vi06 = vu.vf16.z_as_u16();
  // BRANCH!
  // ibeq vi00, vi01, L23       |  nop                            361
  bc = (vu.vi01 == 0);
  // lq.xyzw vf17, 4(vi04)      |  nop                            362
  lq_buffer(Mask::xyzw, vu.vf17, vu.vi04 + 4);
  if (bc) { goto L23; }

  // lq.xyzw vf18, 4(vi05)      |  nop                            363
  lq_buffer(Mask::xyzw, vu.vf18, vu.vi05 + 4);
  // BRANCH!
  // b L24                      |  nop                            364
  bc = true;
  // lq.xyzw vf19, 4(vi06)      |  nop                            365
  lq_buffer(Mask::xyzw, vu.vf19, vu.vi06 + 4);
  if (bc) { goto L24; }

  L23:
  // lq.xyzw vf19, 4(vi05)      |  nop                            366
  lq_buffer(Mask::xyzw, vu.vf19, vu.vi05 + 4);
  // lq.xyzw vf18, 4(vi06)      |  nop                            367
  lq_buffer(Mask::xyzw, vu.vf18, vu.vi06 + 4);
  L24:
  // nop                        |  mulaw.xyzw ACC, vf09, vf00     368
  vu.acc.mula(Mask::xyzw, vu.vf09, vu.vf00.w());
  // move.xyzw vf15, vf17       |  maddax.xyzw ACC, vf06, vf17    369
  vu.acc.madda(Mask::xyzw, vu.vf06, vu.vf17.x());   vu.vf15.move(Mask::xyzw, vu.vf17);
  // nop                        |  madday.xyzw ACC, vf07, vf17    370
  vu.acc.madda(Mask::xyzw, vu.vf07, vu.vf17.y());
  // nop                        |  sub.xyzw vf29, vf18, vf17      371
  vu.vf29.sub(Mask::xyzw, vu.vf18, vu.vf17);
  // nop                        |  sub.xyzw vf30, vf19, vf17      372
  vu.vf30.sub(Mask::xyzw, vu.vf19, vu.vf17);
  // nop                        |  maddz.xyzw vf17, vf08, vf17    373
  vu.acc.madd(Mask::xyzw, vu.vf17, vu.vf08, vu.vf17.z());
  // nop                        |  mulaw.xyzw ACC, vf09, vf00     374
  vu.acc.mula(Mask::xyzw, vu.vf09, vu.vf00.w());
  // div Q, vf12.x, vf17.w      |  maddax.xyzw ACC, vf06, vf18    375
  vu.acc.madda(Mask::xyzw, vu.vf06, vu.vf18.x());   vu.Q = vu.vf12.x() / vu.vf17.w();
  // nop                        |  madday.xyzw ACC, vf07, vf18    376
  vu.acc.madda(Mask::xyzw, vu.vf07, vu.vf18.y());
  // nop                        |  mul.xyzw vf21, vf17, vf02      377
  vu.vf21.mul(Mask::xyzw, vu.vf17, vu.vf02);
  // nop                        |  maddz.xyzw vf18, vf08, vf18    378
  vu.acc.madd(Mask::xyzw, vu.vf18, vu.vf08, vu.vf18.z());
  // nop                        |  mulaw.xyzw ACC, vf09, vf00     379
  vu.acc.mula(Mask::xyzw, vu.vf09, vu.vf00.w());
  // nop                        |  maddax.xyzw ACC, vf06, vf19    380
  vu.acc.madda(Mask::xyzw, vu.vf06, vu.vf19.x());
  // nop                        |  madday.xyzw ACC, vf07, vf19    381
  vu.acc.madda(Mask::xyzw, vu.vf07, vu.vf19.y());
  // div Q, vf12.x, vf18.w      |  mul.xyz vf17, vf17, Q          382
  vu.vf17.mul(Mask::xyz, vu.vf17, vu.Q);   vu.Q = vu.vf12.x() / vu.vf18.w();
  // nop                        |  maddz.xyzw vf19, vf08, vf19    383
  vu.acc.madd(Mask::xyzw, vu.vf19, vu.vf08, vu.vf19.z());
  // nop                        |  mul.xyzw vf22, vf18, vf02      384
  vu.vf22.mul(Mask::xyzw, vu.vf18, vu.vf02);
  // nop                        |  opmula.xyz ACC, vf29, vf30     385
  vu.acc.opmula(vu.vf29, vu.vf30);
  // nop                        |  opmsub.xyz vf29, vf30, vf29    386
  vu.acc.opmsub(vu.vf29, vu.vf30, vu.vf29);
  // nop                        |  add.xy vf25, vf17, vf03        387
  vu.vf25.add(Mask::xy, vu.vf17, vu.vf03);
  // nop                        |  add.xyzw vf17, vf17, vf05      388
  vu.vf17.add(Mask::xyzw, vu.vf17, vu.vf05);
  // div Q, vf12.x, vf19.w      |  mul.xyz vf18, vf18, Q          389
  vu.vf18.mul(Mask::xyz, vu.vf18, vu.Q);   vu.Q = vu.vf12.x() / vu.vf19.w();
  // nop                        |  mul.xyzw vf23, vf19, vf02      390
  vu.vf23.mul(Mask::xyzw, vu.vf19, vu.vf02);
  // nop                        |  mul.xyz vf29, vf29, vf15       391
  vu.vf29.mul(Mask::xyz, vu.vf29, vu.vf15);
  // nop                        |  mul.xy vf25, vf25, vf04        392
  vu.vf25.mul(Mask::xy, vu.vf25, vu.vf04);
  // nop                        |  mini.xyzw vf10, vf10, vf17     393
  vu.vf10.mini(Mask::xyzw, vu.vf10, vu.vf17);
  // nop                        |  max.xyzw vf11, vf11, vf17      394
  vu.vf11.max(Mask::xyzw, vu.vf11, vu.vf17);
  // nop                        |  ftoi4.xyzw vf17, vf17          395
  vu.vf17.ftoi4(Mask::xyzw, vu.vf17);
  // nop                        |  add.xy vf26, vf18, vf03        396
  vu.vf26.add(Mask::xy, vu.vf18, vu.vf03);
  // nop                        |  add.xyzw vf18, vf18, vf05      397
  vu.vf18.add(Mask::xyzw, vu.vf18, vu.vf05);
  // fcset 0x0                  |  addy.x vf29, vf29, vf29        398
  vu.vf29.add(Mask::x, vu.vf29, vu.vf29.y());   cf = 0x0;

  // nop                        |  mul.xyz vf19, vf19, Q          399
  vu.vf19.mul(Mask::xyz, vu.vf19, vu.Q);
  // sq.xyzw vf25, 4(vi02)      |  mini.xyzw vf10, vf10, vf18     400
  vu.vf10.mini(Mask::xyzw, vu.vf10, vu.vf18);   sq_buffer(Mask::xyzw, vu.vf25, vu.vi02 + 4);
  // sq.xyzw vf17, 5(vi02)      |  max.xyzw vf11, vf11, vf18      401
  vu.vf11.max(Mask::xyzw, vu.vf11, vu.vf18);   sq_buffer(Mask::xyzw, vu.vf17, vu.vi02 + 5);
  // nop                        |  clipw.xyz vf21, vf21           402
  cf = clip(vu.vf21, vu.vf21.w(), cf);
  // nop                        |  mul.xy vf26, vf26, vf04        403
  vu.vf26.mul(Mask::xy, vu.vf26, vu.vf04);
  // nop                        |  addz.x vf29, vf29, vf29        404
  sf0 = vu.vf29.add_and_set_sf_s(Mask::x, vu.vf29, vu.vf29.z());
  // nop                        |  ftoi4.xyzw vf18, vf18          405
  vu.vf18.ftoi4(Mask::xyzw, vu.vf18);
  // nop                        |  add.xy vf27, vf19, vf03        406
  vu.vf27.add(Mask::xy, vu.vf19, vu.vf03);
  // sq.xyzw vf26, 6(vi02)      |  add.xyzw vf19, vf19, vf05      407
  vu.vf19.add(Mask::xyzw, vu.vf19, vu.vf05);   sq_buffer(Mask::xyzw, vu.vf26, vu.vi02 + 6);
  // fsand vi01, 0x2            |  clipw.xyz vf22, vf22           408
  cf = clip(vu.vf22, vu.vf22.w(), cf);   fsand(vu.vi01, 0x2, sf0);

  // sq.xyzw vf18, 7(vi02)      |  clipw.xyz vf23, vf23           409
  cf = clip(vu.vf23, vu.vf23.w(), cf);   sq_buffer(Mask::xyzw, vu.vf18, vu.vi02 + 7);
  // BRANCH!
  // ibeq vi00, vi01, L25       |  mul.xy vf27, vf27, vf04        410
  vu.vf27.mul(Mask::xy, vu.vf27, vu.vf04);   bc = (vu.vi01 == 0);
  // nop                        |  mini.xyzw vf10, vf10, vf19     411
  vu.vf10.mini(Mask::xyzw, vu.vf10, vu.vf19);
  if (bc) { goto L25; }

  // BRANCH!
  // b L26                      |  nop                            412
  bc = true;
  // lq.xyzw vf31, 887(vi00)    |  max.xyzw vf11, vf11, vf19      413
  vu.vf11.max(Mask::xyzw, vu.vf11, vu.vf19);   lq_buffer(Mask::xyzw, vu.vf31, 887);
  if (bc) { goto L26; }

  L25:
  // lq.xyzw vf31, 886(vi00)    |  max.xyzw vf11, vf11, vf19      414
  vu.vf11.max(Mask::xyzw, vu.vf11, vu.vf19);   lq_buffer(Mask::xyzw, vu.vf31, 886);
  L26:
  // fcand vi01, 0x3ffff        |  ftoi4.xyzw vf19, vf19          415
  vu.vf19.ftoi4(Mask::xyzw, vu.vf19);   fcand(vu.vi01, 0x3ffff, cf);

  // BRANCH!
  // ibne vi00, vi01, L34       |  nop                            416
  bc = (vu.vi01 != 0);
  // iaddi vi08, vi08, -0x1     |  nop                            417
  vu.vi08 = vu.vi08 + -1;
  if (bc) { goto L34; }

  // sq.xyzw vf27, 8(vi02)      |  nop                            418
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi02 + 8);
  // sq.xyzw vf31, 3(vi02)      |  nop                            419
  sq_buffer(Mask::xyzw, vu.vf31, vu.vi02 + 3);
  // sq.xyzw vf19, 9(vi02)      |  nop                            420
  sq_buffer(Mask::xyzw, vu.vf19, vu.vi02 + 9);
  // xgkick vi02                |  nop                            421
  xgkick(vu.vi02);
  // mtir vi02, vf01.y          |  nop                            422
  vu.vi02 = vu.vf01.y_as_u16();
  // mr32.xyzw vf01, vf01       |  nop                            423
  vu.vf01.mr32(Mask::xyzw, vu.vf01);
  L27:
  // BRANCH!
  // ibgtz vi08, L22            |  nop                            424
  bc = ((s16)vu.vi08) > 0;
  // nop                        |  nop                            425

  if (bc) { goto L22; }

  // iaddiu vi03, vi00, 0x158   |  nop                            426
  vu.vi03 = 0x158; /* 344 */
  // ilwr.x vi08, vi03          |  nop                            427
  ilw_buffer(Mask::x, vu.vi08, vu.vi03);
  // iaddi vi03, vi03, 0x1      |  nop                            428
  vu.vi03 = vu.vi03 + 1;
  L28:
  // lqi.xyzw vf16, vi03        |  nop                            429
  lq_buffer(Mask::xyzw, vu.vf16, vu.vi03++);
  // nop                        |  nop                            430

  // nop                        |  nop                            431

  // nop                        |  nop                            432

  // mtir vi01, vf16.w          |  nop                            433
  vu.vi01 = vu.vf16.w_as_u16();
  // mtir vi04, vf16.x          |  nop                            434
  vu.vi04 = vu.vf16.x_as_u16();
  // mtir vi05, vf16.y          |  nop                            435
  vu.vi05 = vu.vf16.y_as_u16();
  // mtir vi06, vf16.z          |  nop                            436
  vu.vi06 = vu.vf16.z_as_u16();
  // BRANCH!
  // ibne vi00, vi01, L29       |  nop                            437
  bc = (vu.vi01 != 0);
  // lq.xyzw vf17, 174(vi04)    |  nop                            438
  lq_buffer(Mask::xyzw, vu.vf17, vu.vi04 + 174);
  if (bc) { goto L29; }

  // lq.xyzw vf18, 174(vi05)    |  nop                            439
  lq_buffer(Mask::xyzw, vu.vf18, vu.vi05 + 174);
  // BRANCH!
  // b L30                      |  nop                            440
  bc = true;
  // lq.xyzw vf19, 174(vi06)    |  nop                            441
  lq_buffer(Mask::xyzw, vu.vf19, vu.vi06 + 174);
  if (bc) { goto L30; }

  L29:
  // lq.xyzw vf19, 174(vi05)    |  nop                            442
  lq_buffer(Mask::xyzw, vu.vf19, vu.vi05 + 174);
  // lq.xyzw vf18, 174(vi06)    |  nop                            443
  lq_buffer(Mask::xyzw, vu.vf18, vu.vi06 + 174);
  L30:
  // nop                        |  mulaw.xyzw ACC, vf09, vf00     444
  vu.acc.mula(Mask::xyzw, vu.vf09, vu.vf00.w());
  // move.xyzw vf15, vf17       |  maddax.xyzw ACC, vf06, vf17    445
  vu.acc.madda(Mask::xyzw, vu.vf06, vu.vf17.x());   vu.vf15.move(Mask::xyzw, vu.vf17);
  // nop                        |  madday.xyzw ACC, vf07, vf17    446
  vu.acc.madda(Mask::xyzw, vu.vf07, vu.vf17.y());
  // nop                        |  maddz.xyzw vf17, vf08, vf17    447
  vu.acc.madd(Mask::xyzw, vu.vf17, vu.vf08, vu.vf17.z());
  // nop                        |  mulaw.xyzw ACC, vf09, vf00     448
  vu.acc.mula(Mask::xyzw, vu.vf09, vu.vf00.w());
  // nop                        |  sub.xyzw vf29, vf18, vf15      449
  vu.vf29.sub(Mask::xyzw, vu.vf18, vu.vf15);
  // nop                        |  sub.xyzw vf30, vf19, vf15      450
  vu.vf30.sub(Mask::xyzw, vu.vf19, vu.vf15);
  // div Q, vf12.x, vf17.w      |  maddax.xyzw ACC, vf06, vf18    451
  vu.acc.madda(Mask::xyzw, vu.vf06, vu.vf18.x());   vu.Q = vu.vf12.x() / vu.vf17.w();
  // nop                        |  mul.xyzw vf21, vf17, vf02      452
  vu.vf21.mul(Mask::xyzw, vu.vf17, vu.vf02);
  // nop                        |  madday.xyzw ACC, vf07, vf18    453
  vu.acc.madda(Mask::xyzw, vu.vf07, vu.vf18.y());
  // nop                        |  maddz.xyzw vf18, vf08, vf18    454
  vu.acc.madd(Mask::xyzw, vu.vf18, vu.vf08, vu.vf18.z());
  // nop                        |  mulaw.xyzw ACC, vf09, vf00     455
  vu.acc.mula(Mask::xyzw, vu.vf09, vu.vf00.w());
  // nop                        |  maddax.xyzw ACC, vf06, vf19    456
  vu.acc.madda(Mask::xyzw, vu.vf06, vu.vf19.x());
  // nop                        |  madday.xyzw ACC, vf07, vf19    457
  vu.acc.madda(Mask::xyzw, vu.vf07, vu.vf19.y());
  // div Q, vf12.x, vf18.w      |  mul.xyz vf17, vf17, Q          458
  vu.vf17.mul(Mask::xyz, vu.vf17, vu.Q);   vu.Q = vu.vf12.x() / vu.vf18.w();
  // nop                        |  maddz.xyzw vf19, vf08, vf19    459
  vu.acc.madd(Mask::xyzw, vu.vf19, vu.vf08, vu.vf19.z());
  // nop                        |  mul.xyzw vf22, vf18, vf02      460
  vu.vf22.mul(Mask::xyzw, vu.vf18, vu.vf02);
  // nop                        |  opmula.xyz ACC, vf29, vf30     461
  vu.acc.opmula(vu.vf29, vu.vf30);
  // nop                        |  opmsub.xyz vf29, vf30, vf29    462
  vu.acc.opmsub(vu.vf29, vu.vf30, vu.vf29);
  // nop                        |  add.xy vf25, vf17, vf03        463
  vu.vf25.add(Mask::xy, vu.vf17, vu.vf03);
  // nop                        |  add.xyzw vf17, vf17, vf05      464
  vu.vf17.add(Mask::xyzw, vu.vf17, vu.vf05);
  // div Q, vf12.x, vf19.w      |  mul.xyz vf18, vf18, Q          465
  vu.vf18.mul(Mask::xyz, vu.vf18, vu.Q);   vu.Q = vu.vf12.x() / vu.vf19.w();
  // nop                        |  mul.xyzw vf23, vf19, vf02      466
  vu.vf23.mul(Mask::xyzw, vu.vf19, vu.vf02);
  // nop                        |  mul.xyz vf29, vf29, vf15       467
  vu.vf29.mul(Mask::xyz, vu.vf29, vu.vf15);
  // nop                        |  mul.xy vf25, vf25, vf04        468
  vu.vf25.mul(Mask::xy, vu.vf25, vu.vf04);
  // nop                        |  mini.xyzw vf10, vf10, vf17     469
  vu.vf10.mini(Mask::xyzw, vu.vf10, vu.vf17);
  // nop                        |  max.xyzw vf11, vf11, vf17      470
  vu.vf11.max(Mask::xyzw, vu.vf11, vu.vf17);
  // nop                        |  ftoi4.xyzw vf17, vf17          471
  vu.vf17.ftoi4(Mask::xyzw, vu.vf17);
  // nop                        |  add.xy vf26, vf18, vf03        472
  vu.vf26.add(Mask::xy, vu.vf18, vu.vf03);
  // nop                        |  add.xyzw vf18, vf18, vf05      473
  vu.vf18.add(Mask::xyzw, vu.vf18, vu.vf05);
  // fcset 0x0                  |  addy.x vf29, vf29, vf29        474
  vu.vf29.add(Mask::x, vu.vf29, vu.vf29.y());   cf = 0x0;

  // nop                        |  mul.xyz vf19, vf19, Q          475
  vu.vf19.mul(Mask::xyz, vu.vf19, vu.Q);
  // sq.xyzw vf25, 4(vi02)      |  mini.xyzw vf10, vf10, vf18     476
  vu.vf10.mini(Mask::xyzw, vu.vf10, vu.vf18);   sq_buffer(Mask::xyzw, vu.vf25, vu.vi02 + 4);
  // sq.xyzw vf17, 5(vi02)      |  max.xyzw vf11, vf11, vf18      477
  vu.vf11.max(Mask::xyzw, vu.vf11, vu.vf18);   sq_buffer(Mask::xyzw, vu.vf17, vu.vi02 + 5);
  // nop                        |  clipw.xyz vf21, vf21           478
  cf = clip(vu.vf21, vu.vf21.w(), cf);
  // nop                        |  mul.xy vf26, vf26, vf04        479
  vu.vf26.mul(Mask::xy, vu.vf26, vu.vf04);
  // nop                        |  addz.x vf29, vf29, vf29        480
  sf0 = vu.vf29.add_and_set_sf_s(Mask::x, vu.vf29, vu.vf29.z());
  // nop                        |  ftoi4.xyzw vf18, vf18          481
  vu.vf18.ftoi4(Mask::xyzw, vu.vf18);
  // nop                        |  add.xy vf27, vf19, vf03        482
  vu.vf27.add(Mask::xy, vu.vf19, vu.vf03);
  // sq.xyzw vf26, 6(vi02)      |  add.xyzw vf19, vf19, vf05      483
  vu.vf19.add(Mask::xyzw, vu.vf19, vu.vf05);   sq_buffer(Mask::xyzw, vu.vf26, vu.vi02 + 6);
  // fsand vi01, 0x2            |  clipw.xyz vf22, vf22           484
  cf = clip(vu.vf22, vu.vf22.w(), cf);   fsand(vu.vi01, 0x2, sf0);

  // sq.xyzw vf18, 7(vi02)      |  clipw.xyz vf23, vf23           485
  cf = clip(vu.vf23, vu.vf23.w(), cf);   sq_buffer(Mask::xyzw, vu.vf18, vu.vi02 + 7);
  // BRANCH!
  // ibeq vi00, vi01, L31       |  mul.xy vf27, vf27, vf04        486
  vu.vf27.mul(Mask::xy, vu.vf27, vu.vf04);   bc = (vu.vi01 == 0);
  // nop                        |  mini.xyzw vf10, vf10, vf19     487
  vu.vf10.mini(Mask::xyzw, vu.vf10, vu.vf19);
  if (bc) { goto L31; }

  // BRANCH!
  // b L32                      |  nop                            488
  bc = true;
  // lq.xyzw vf31, 887(vi00)    |  max.xyzw vf11, vf11, vf19      489
  vu.vf11.max(Mask::xyzw, vu.vf11, vu.vf19);   lq_buffer(Mask::xyzw, vu.vf31, 887);
  if (bc) { goto L32; }

  L31:
  // lq.xyzw vf31, 886(vi00)    |  max.xyzw vf11, vf11, vf19      490
  vu.vf11.max(Mask::xyzw, vu.vf11, vu.vf19);   lq_buffer(Mask::xyzw, vu.vf31, 886);
  L32:
  // fcand vi01, 0x3ffff        |  ftoi4.xyzw vf19, vf19          491
  vu.vf19.ftoi4(Mask::xyzw, vu.vf19);   fcand(vu.vi01, 0x3ffff, cf);

  // BRANCH!
  // ibne vi00, vi01, L35       |  nop                            492
  bc = (vu.vi01 != 0);
  // iaddi vi08, vi08, -0x1     |  nop                            493
  vu.vi08 = vu.vi08 + -1;
  if (bc) { goto L35; }

  // sq.xyzw vf27, 8(vi02)      |  nop                            494
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi02 + 8);
  // sq.xyzw vf31, 3(vi02)      |  nop                            495
  sq_buffer(Mask::xyzw, vu.vf31, vu.vi02 + 3);
  // sq.xyzw vf19, 9(vi02)      |  nop                            496
  sq_buffer(Mask::xyzw, vu.vf19, vu.vi02 + 9);
  // xgkick vi02                |  nop                            497
  xgkick(vu.vi02);
  // mtir vi02, vf01.y          |  nop                            498
  vu.vi02 = vu.vf01.y_as_u16();
  // mr32.xyzw vf01, vf01       |  nop                            499
  vu.vf01.mr32(Mask::xyzw, vu.vf01);
  L33:
  // BRANCH!
  // ibgtz vi08, L28            |  nop                            500
  bc = ((s16)vu.vi08) > 0;
  // nop                        |  nop                            501

  if (bc) { goto L28; }

  // nop                        |  nop :e                         502

  // nop                        |  nop                            503

  return;

  L34:
  // sq.xyzw vf21, 1000(vi00)   |  nop                            504
  sq_buffer(Mask::xyzw, vu.vf21, 1000);
  // sq.xyzw vf22, 1003(vi00)   |  nop                            505
  sq_buffer(Mask::xyzw, vu.vf22, 1003);
  // sq.xyzw vf23, 1006(vi00)   |  nop                            506
  sq_buffer(Mask::xyzw, vu.vf23, 1006);
  // sq.xyzw vf31, 942(vi00)    |  nop                            507
  sq_buffer(Mask::xyzw, vu.vf31, 942);
  // mfir.x vf29, vi02          |  nop                            508
  vu.vf29.mfir(Mask::x, vu.vi02);
  // mfir.y vf29, vi03          |  nop                            509
  vu.vf29.mfir(Mask::y, vu.vi03);
  // mfir.z vf29, vi07          |  nop                            510
  vu.vf29.mfir(Mask::z, vu.vi07);
  // BRANCH!
  // bal vi15, L36              |  nop                            511
  // mfir.w vf29, vi08          |  nop                            512
  vu.vf29.mfir(Mask::w, vu.vi08);
  vu.vi15 = 513;
  goto L36;

  INSTR_513:
  // mtir vi08, vf29.w          |  nop                            513
  vu.vi08 = vu.vf29.w_as_u16();
  // mtir vi03, vf29.y          |  nop                            514
  vu.vi03 = vu.vf29.y_as_u16();
  // mtir vi07, vf29.z          |  nop                            515
  vu.vi07 = vu.vf29.z_as_u16();
  // BRANCH!
  // b L27                      |  nop                            516
  bc = true;
  // mtir vi02, vf29.x          |  nop                            517
  vu.vi02 = vu.vf29.x_as_u16();
  if (bc) { goto L27; }

  L35:
  // sq.xyzw vf21, 1000(vi00)   |  nop                            518
  sq_buffer(Mask::xyzw, vu.vf21, 1000);
  // sq.xyzw vf22, 1003(vi00)   |  nop                            519
  sq_buffer(Mask::xyzw, vu.vf22, 1003);
  // sq.xyzw vf23, 1006(vi00)   |  nop                            520
  sq_buffer(Mask::xyzw, vu.vf23, 1006);
  // sq.xyzw vf31, 942(vi00)    |  nop                            521
  sq_buffer(Mask::xyzw, vu.vf31, 942);
  // mfir.x vf29, vi02          |  nop                            522
  vu.vf29.mfir(Mask::x, vu.vi02);
  // mfir.y vf29, vi03          |  nop                            523
  vu.vf29.mfir(Mask::y, vu.vi03);
  // mfir.z vf29, vi07          |  nop                            524
  vu.vf29.mfir(Mask::z, vu.vi07);
  // BRANCH!
  // bal vi15, L36              |  nop                            525
  // mfir.w vf29, vi08          |  nop                            526
  vu.vf29.mfir(Mask::w, vu.vi08);
  vu.vi15 = 527;
  goto L36;

  INSTR_527:
  // mtir vi08, vf29.w          |  nop                            527
  vu.vi08 = vu.vf29.w_as_u16();
  // mtir vi03, vf29.y          |  nop                            528
  vu.vi03 = vu.vf29.y_as_u16();
  // mtir vi07, vf29.z          |  nop                            529
  vu.vi07 = vu.vf29.z_as_u16();
  // BRANCH!
  // b L33                      |  nop                            530
  bc = true;
  // mtir vi02, vf29.x          |  nop                            531
  vu.vi02 = vu.vf29.x_as_u16();
  if (bc) { goto L33; }

  L36:
  // sq.xyzw vf00, 893(vi00)    |  nop                            532
  sq_buffer(Mask::xyzw, vu.vf00, 893);
  // sq.xyzw vf00, 900(vi00)    |  nop                            533
  sq_buffer(Mask::xyzw, vu.vf00, 900);
  // sq.xyzw vf00, 907(vi00)    |  nop                            534
  sq_buffer(Mask::xyzw, vu.vf00, 907);
  // sq.xyzw vf00, 914(vi00)    |  nop                            535
  sq_buffer(Mask::xyzw, vu.vf00, 914);
  // sq.xyzw vf00, 921(vi00)    |  nop                            536
  sq_buffer(Mask::xyzw, vu.vf00, 921);
  // sq.xyzw vf00, 928(vi00)    |  nop                            537
  sq_buffer(Mask::xyzw, vu.vf00, 928);
  // iaddiu vi01, vi00, 0x2d4   |  nop                            538
  vu.vi01 = 0x2d4; /* 724 */
  // isw.z vi01, 893(vi00)      |  nop                            539
  isw_buffer(Mask::z, vu.vi01, 893);
  // iaddiu vi01, vi00, 0x2d6   |  nop                            540
  vu.vi01 = 0x2d6; /* 726 */
  // isw.z vi01, 900(vi00)      |  nop                            541
  isw_buffer(Mask::z, vu.vi01, 900);
  // iaddiu vi01, vi00, 0x2d8   |  nop                            542
  vu.vi01 = 0x2d8; /* 728 */
  // isw.z vi01, 907(vi00)      |  nop                            543
  isw_buffer(Mask::z, vu.vi01, 907);
  // iaddiu vi01, vi00, 0x2da   |  nop                            544
  vu.vi01 = 0x2da; /* 730 */
  // isw.z vi01, 914(vi00)      |  nop                            545
  isw_buffer(Mask::z, vu.vi01, 914);
  // iaddiu vi01, vi00, 0x2dc   |  nop                            546
  vu.vi01 = 0x2dc; /* 732 */
  // isw.z vi01, 921(vi00)      |  nop                            547
  isw_buffer(Mask::z, vu.vi01, 921);
  // iaddiu vi01, vi00, 0x2de   |  nop                            548
  vu.vi01 = 0x2de; /* 734 */
  // isw.z vi01, 928(vi00)      |  nop                            549
  isw_buffer(Mask::z, vu.vi01, 928);
  // iaddiu vi03, vi00, 0x3b0   |  nop                            550
  vu.vi03 = 0x3b0; /* 944 */
  // iaddiu vi04, vi00, 0x3af   |  nop                            551
  vu.vi04 = 0x3af; /* 943 */
  // mfir.x vf31, vi15          |  nop                            552
  vu.vf31.mfir(Mask::x, vu.vi15);
  // iaddi vi05, vi00, 0x0      |  nop                            553
  vu.vi05 = 0;
  // BRANCH!
  // bal vi15, L38              |  nop                            554
  // iaddiu vi07, vi00, 0x3e8   |  nop                            555
  vu.vi07 = 0x3e8; /* 1000 */
  vu.vi15 = 556;
  goto L38;

  INSTR_556:
  // BRANCH!
  // bal vi15, L38              |  nop                            556
  // iaddiu vi07, vi00, 0x3eb   |  nop                            557
  vu.vi07 = 0x3eb; /* 1003 */
  vu.vi15 = 558;
  goto L38;

  INSTR_558:
  // BRANCH!
  // bal vi15, L38              |  nop                            558
  // iaddiu vi07, vi00, 0x3ee   |  nop                            559
  vu.vi07 = 0x3ee; /* 1006 */
  vu.vi15 = 560;
  goto L38;

  INSTR_560:
  // BRANCH!
  // b L47                      |  nop                            560
  bc = true;
  // nop                        |  nop                            561

  if (bc) { goto L47; }

  L37:
  // sq.xyzw vf00, 893(vi00)    |  nop                            562
  sq_buffer(Mask::xyzw, vu.vf00, 893);
  // sq.xyzw vf00, 900(vi00)    |  nop                            563
  sq_buffer(Mask::xyzw, vu.vf00, 900);
  // sq.xyzw vf00, 907(vi00)    |  nop                            564
  sq_buffer(Mask::xyzw, vu.vf00, 907);
  // sq.xyzw vf00, 914(vi00)    |  nop                            565
  sq_buffer(Mask::xyzw, vu.vf00, 914);
  // sq.xyzw vf00, 921(vi00)    |  nop                            566
  sq_buffer(Mask::xyzw, vu.vf00, 921);
  // sq.xyzw vf00, 928(vi00)    |  nop                            567
  sq_buffer(Mask::xyzw, vu.vf00, 928);
  // iaddiu vi01, vi00, 0x2d4   |  nop                            568
  vu.vi01 = 0x2d4; /* 724 */
  // isw.z vi01, 893(vi00)      |  nop                            569
  isw_buffer(Mask::z, vu.vi01, 893);
  // iaddiu vi01, vi00, 0x2d6   |  nop                            570
  vu.vi01 = 0x2d6; /* 726 */
  // isw.z vi01, 900(vi00)      |  nop                            571
  isw_buffer(Mask::z, vu.vi01, 900);
  // iaddiu vi01, vi00, 0x2d8   |  nop                            572
  vu.vi01 = 0x2d8; /* 728 */
  // isw.z vi01, 907(vi00)      |  nop                            573
  isw_buffer(Mask::z, vu.vi01, 907);
  // iaddiu vi01, vi00, 0x2da   |  nop                            574
  vu.vi01 = 0x2da; /* 730 */
  // isw.z vi01, 914(vi00)      |  nop                            575
  isw_buffer(Mask::z, vu.vi01, 914);
  // iaddiu vi01, vi00, 0x2dc   |  nop                            576
  vu.vi01 = 0x2dc; /* 732 */
  // isw.z vi01, 921(vi00)      |  nop                            577
  isw_buffer(Mask::z, vu.vi01, 921);
  // iaddiu vi01, vi00, 0x2de   |  nop                            578
  vu.vi01 = 0x2de; /* 734 */
  // isw.z vi01, 928(vi00)      |  nop                            579
  isw_buffer(Mask::z, vu.vi01, 928);
  // iaddiu vi03, vi00, 0x3b0   |  nop                            580
  vu.vi03 = 0x3b0; /* 944 */
  // iaddiu vi04, vi00, 0x3af   |  nop                            581
  vu.vi04 = 0x3af; /* 943 */
  // mfir.x vf31, vi15          |  nop                            582
  vu.vf31.mfir(Mask::x, vu.vi15);
  // iaddi vi05, vi00, 0x0      |  nop                            583
  vu.vi05 = 0;
  // BRANCH!
  // bal vi15, L38              |  nop                            584
  // iaddiu vi07, vi00, 0x3e8   |  nop                            585
  vu.vi07 = 0x3e8; /* 1000 */
  vu.vi15 = 586;
  goto L38;

  INSTR_586:
  // BRANCH!
  // bal vi15, L38              |  nop                            586
  // iaddiu vi07, vi00, 0x3eb   |  nop                            587
  vu.vi07 = 0x3eb; /* 1003 */
  vu.vi15 = 588;
  goto L38;

  INSTR_588:
  // BRANCH!
  // bal vi15, L38              |  nop                            588
  // iaddiu vi07, vi00, 0x3ee   |  nop                            589
  vu.vi07 = 0x3ee; /* 1006 */
  vu.vi15 = 590;
  goto L38;

  INSTR_590:
  // BRANCH!
  // bal vi15, L38              |  nop                            590
  // iaddiu vi07, vi00, 0x3f1   |  nop                            591
  vu.vi07 = 0x3f1; /* 1009 */
  vu.vi15 = 592;
  goto L38;

  INSTR_592:
  // BRANCH!
  // b L47                      |  nop                            592
  bc = true;
  // nop                        |  nop                            593

  if (bc) { goto L47; }

  L38:
  // iaddiu vi09, vi00, 0x37d   |  nop                            594
  vu.vi09 = 0x37d; /* 893 */
  L39:
  // iaddi vi10, vi00, 0x0      |  nop                            595
  vu.vi10 = 0;
  L40:
  // isubiu vi01, vi09, 0x3a7   |  nop                            596
  vu.vi01 = vu.vi09 - 0x3a7; /* 935 */
  // ilwr.y vi08, vi09          |  nop                            597
  ilw_buffer(Mask::y, vu.vi08, vu.vi09);
  // BRANCH!
  // ibgez vi01, L44            |  nop                            598
  bc = ((s16)vu.vi01) >= 0;
  // ilwr.z vi06, vi09          |  nop                            599
  ilw_buffer(Mask::z, vu.vi06, vu.vi09);
  if (bc) { goto L44; }

  // lq.xyzw vf15, 0(vi07)      |  nop                            600
  lq_buffer(Mask::xyzw, vu.vf15, vu.vi07);
  // lq.xyzw vf14, 0(vi08)      |  nop                            601
  lq_buffer(Mask::xyzw, vu.vf14, vu.vi08);
  // BRANCH!
  // ibne vi00, vi08, L41       |  nop                            602
  bc = (vu.vi08 != 0);
  // iswr.y vi07, vi09          |  nop                            603
  isw_buffer(Mask::y, vu.vi07, vu.vi09);
  if (bc) { goto L41; }

  // jalr vi11, vi06            |  nop                            604
  // ASSERT(false);
  // iswr.x vi07, vi09          |  nop                            605
  isw_buffer(Mask::x, vu.vi07, vu.vi09);
  handle_jalr_to_end_block(vu.vi06, sf0, sf1);
  // nop                        |  nop                            606

  // nop                        |  nop                            607

  // nop                        |  nop                            608

  // fsand vi02, 0x2            |  nop                            609
  fsand(vu.vi02, 0x2, sf1);

  // BRANCH!
  // ibne vi00, vi02, L45       |  nop                            610
  bc = (vu.vi02 != 0);
  // nop                        |  nop                            611

  if (bc) { goto L45; }

  // BRANCH!
  // b L40                      |  nop                            612
  bc = true;
  // iaddi vi09, vi09, 0x7      |  nop                            613
  vu.vi09 = vu.vi09 + 7;
  if (bc) { goto L40; }

  L41:
  // jalr vi11, vi06            |  nop                            614
  // ASSERT(false);
  // nop                        |  nop                            615
  handle_jalr_to_end_block(vu.vi06, sf0, sf1);

  // nop                        |  nop                            616

  // nop                        |  nop                            617

  // fsand vi01, 0x2            |  nop                            618
  fsand(vu.vi01, 0x2, sf0);

  // fsand vi02, 0x2            |  subw.w vf31, vf30, vf31        619
  vu.vf31.sub(Mask::w, vu.vf30, vu.vf31.w());   fsand(vu.vi02, 0x2, sf1);

  // BRANCH!
  // ibne vi00, vi01, L43       |  nop                            620
  bc = (vu.vi01 != 0);
  // nop                        |  nop                            621

  if (bc) { goto L43; }

  // BRANCH!
  // ibne vi00, vi02, L42       |  nop                            622
  bc = (vu.vi02 != 0);
  // div Q, vf30.w, vf31.w      |  nop                            623
  vu.Q = vu.vf30.w() / vu.vf31.w();
  if (bc) { goto L42; }

  // BRANCH!
  // b L40                      |  nop                            624
  bc = true;
  // iaddi vi09, vi09, 0x7      |  nop                            625
  vu.vi09 = vu.vi09 + 7;
  if (bc) { goto L40; }

  L42:
  // BRANCH!
  // bal vi11, L52              |  nop                            626
  // iaddi vi07, vi09, 0x1      |  nop                            627
  vu.vi07 = vu.vi09 + 1;
  // if (bc) { goto L52; }
  handle_bal52();

  // sq.xyzw vf16, 1(vi09)      |  nop                            628
  sq_buffer(Mask::xyzw, vu.vf16, vu.vi09 + 1);
  // BRANCH!
  // b L40                      |  nop                            629
  bc = true;
  // iaddi vi09, vi09, 0x7      |  nop                            630
  vu.vi09 = vu.vi09 + 7;
  if (bc) { goto L40; }

  L43:
  // BRANCH!
  // ibne vi00, vi02, L45       |  nop                            631
  bc = (vu.vi02 != 0);
  // div Q, vf30.w, vf31.w      |  nop                            632
  vu.Q = vu.vf30.w() / vu.vf31.w();
  if (bc) { goto L45; }

  // BRANCH!
  // bal vi11, L52              |  nop                            633

  // nop                        |  nop                            634

  handle_bal52();

  // sq.xyzw vf16, 4(vi09)      |  nop                            635
  sq_buffer(Mask::xyzw, vu.vf16, vu.vi09 + 4);
  // iaddi vi09, vi09, 0x7      |  nop                            636
  vu.vi09 = vu.vi09 + 7;
  // isw.x vi09, 935(vi10)      |  nop                            637
  isw_buffer(Mask::x, vu.vi09, vu.vi10 + 935);
  // isw.y vi07, 935(vi10)      |  nop                            638
  isw_buffer(Mask::y, vu.vi07, vu.vi10 + 935);
  // iaddi vi10, vi10, 0x1      |  nop                            639
  vu.vi10 = vu.vi10 + 1;
  // BRANCH!
  // b L40                      |  nop                            640
  bc = true;
  // iaddi vi07, vi09, -0x3     |  nop                            641
  vu.vi07 = vu.vi09 + -3;
  if (bc) { goto L40; }

  L44:
  // lq.xyzw vf14, 0(vi07)      |  nop                            642
  lq_buffer(Mask::xyzw, vu.vf14, vu.vi07);
  // iaddi vi05, vi05, 0x1      |  nop                            643
  vu.vi05 = vu.vi05 + 1;
  // div Q, vf00.w, vf14.w      |  nop                            644
  vu.Q = vu.vf00.w() / vu.vf14.w();
  // nop                        |  mul.xyzw vf14, vf14, vf13      645
  vu.vf14.mul(Mask::xyzw, vu.vf14, vu.vf13);
  // iaddi vi03, vi03, 0x2      |  nop                            646
  vu.vi03 = vu.vi03 + 2;
  // waitq                      |  subw.w vf14, vf00, vf00        647
  vu.vf14.sub(Mask::w, vu.vf00, vu.vf00.w());
  // nop                        |  mul.xyz vf14, vf14, Q          648
  vu.vf14.mul(Mask::xyz, vu.vf14, vu.Q);
  // nop                        |  add.xy vf26, vf14, vf03        649
  vu.vf26.add(Mask::xy, vu.vf14, vu.vf03);
  // nop                        |  add.xyzw vf14, vf14, vf05      650
  vu.vf14.add(Mask::xyzw, vu.vf14, vu.vf05);
  // nop                        |  mul.xy vf26, vf26, vf04        651
  vu.vf26.mul(Mask::xy, vu.vf26, vu.vf04);
  // nop                        |  mini.xyzw vf10, vf10, vf14     652
  vu.vf10.mini(Mask::xyzw, vu.vf10, vu.vf14);
  // nop                        |  max.xyzw vf11, vf11, vf14      653
  vu.vf11.max(Mask::xyzw, vu.vf11, vu.vf14);
  // sq.xyzw vf26, -2(vi03)     |  ftoi4.xyzw vf14, vf14          654
  vu.vf14.ftoi4(Mask::xyzw, vu.vf14);   sq_buffer(Mask::xyzw, vu.vf26, vu.vi03 + -2);
  // sq.xyzw vf14, -1(vi03)     |  nop                            655
  sq_buffer(Mask::xyzw, vu.vf14, vu.vi03 + -1);
  L45:
  // BRANCH!
  // iblez vi10, L46            |  nop                            656
  bc = ((s16)vu.vi10) <= 0;
  // nop                        |  nop                            657

  if (bc) { goto L46; }

  // ilw.x vi09, 934(vi10)      |  nop                            658
  ilw_buffer(Mask::x, vu.vi09, vu.vi10 + 934);
  // ilw.y vi07, 934(vi10)      |  nop                            659
  ilw_buffer(Mask::y, vu.vi07, vu.vi10 + 934);
  // BRANCH!
  // b L40                      |  nop                            660
  bc = true;
  // iaddi vi10, vi10, -0x1     |  nop                            661
  vu.vi10 = vu.vi10 + -1;
  if (bc) { goto L40; }

  L46:
  // jr vi15                    |  nop                            662
  // clang-format on
  switch (vu.vi15) {
    case 556:
      goto INSTR_556;
    case 558:
      goto INSTR_558;
    case 560:
      goto INSTR_560;
    case 586:
      goto INSTR_586;
    case 588:
      goto INSTR_588;
    case 590:
      goto INSTR_590;
    case 592:
      goto INSTR_592;
    case 690:
      goto INSTR_690;
    case 699:
      goto INSTR_699;
    default:
      ASSERT_MSG(false, fmt::format("unknown vu.vi15 @ L46: {}", vu.vi15));
  }
  // clang-format off
  // nop                        |  nop                            663

  L47:
  // iaddiu vi09, vi00, 0x37d   |  nop                            664
  vu.vi09 = 0x37d; /* 893 */
  L48:
  // ilwr.x vi08, vi09          |  nop                            665
  ilw_buffer(Mask::x, vu.vi08, vu.vi09);
  // ilwr.y vi07, vi09          |  nop                            666
  ilw_buffer(Mask::y, vu.vi07, vu.vi09);
  // ilwr.z vi06, vi09          |  nop                            667
  ilw_buffer(Mask::z, vu.vi06, vu.vi09);
  // nop                        |  nop                            668

  // BRANCH!
  // ibeq vi00, vi08, L50       |  nop                            669
  bc = (vu.vi08 == 0);
  // lq.xyzw vf14, 0(vi07)      |  nop                            670
  lq_buffer(Mask::xyzw, vu.vf14, vu.vi07);
  if (bc) { goto L50; }

  // BRANCH!
  // ibeq vi07, vi08, L50       |  nop                            671
  bc = (vu.vi07 == vu.vi08);
  // lq.xyzw vf15, 0(vi08)      |  nop                            672
  lq_buffer(Mask::xyzw, vu.vf15, vu.vi08);
  if (bc) { goto L50; }

  // jalr vi11, vi06            |  nop                            673
  // ASSERT(false);
  // nop                        |  nop                            674
  handle_jalr_to_end_block(vu.vi06, sf0, sf1);

  // nop                        |  nop                            675

  // nop                        |  nop                            676

  // fsand vi01, 0x2            |  nop                            677
  fsand(vu.vi01, 0x2, sf0);

  // fsand vi02, 0x2            |  subw.w vf31, vf30, vf31        678
  vu.vf31.sub(Mask::w, vu.vf30, vu.vf31.w());   fsand(vu.vi02, 0x2, sf1);

  // BRANCH!
  // ibeq vi02, vi01, L50       |  nop                            679
  bc = (vu.vi02 == vu.vi01);
  // nop                        |  nop                            680

  if (bc) { goto L50; }

  // BRANCH!
  // ibeq vi00, vi01, L49       |  nop                            681
  bc = (vu.vi01 == 0);
  // div Q, vf30.w, vf31.w      |  nop                            682
  vu.Q = vu.vf30.w() / vu.vf31.w();
  if (bc) { goto L49; }

  // BRANCH!
  // bal vi11, L52              |  nop                            683
  handle_bal52();
  // nop                        |  nop                            684

  // sq.xyzw vf16, 4(vi09)      |  nop                            685
  sq_buffer(Mask::xyzw, vu.vf16, vu.vi09 + 4);
  // iaddi vi07, vi09, 0x4      |  nop                            686
  vu.vi07 = vu.vi09 + 4;
  // ior vi12, vi09, vi00       |  nop                            687
  vu.vi12 = vu.vi09;
  // BRANCH!
  // bal vi15, L39              |  nop                            688
  // iaddi vi09, vi09, 0x7      |  nop                            689
  vu.vi09 = vu.vi09 + 7;
  vu.vi15 = 690;
  goto L39;

  INSTR_690:
  // BRANCH!
  // b L50                      |  nop                            690
  bc = true;
  // ior vi09, vi12, vi00       |  nop                            691
  vu.vi09 = vu.vi12;
  if (bc) { goto L50; }

  L49:
  // BRANCH!
  // bal vi11, L52              |  nop                            692
  handle_bal52();
  // nop                        |  nop                            693

  // sq.xyzw vf16, 1(vi09)      |  nop                            694
  sq_buffer(Mask::xyzw, vu.vf16, vu.vi09 + 1);
  // iaddi vi07, vi09, 0x1      |  nop                            695
  vu.vi07 = vu.vi09 + 1;
  // ior vi12, vi09, vi00       |  nop                            696
  vu.vi12 = vu.vi09;
  // BRANCH!
  // bal vi15, L39              |  nop                            697
  // iaddi vi09, vi09, 0x7      |  nop                            698
  vu.vi09 = vu.vi09 + 7;
  vu.vi15 = 699;
  goto L39;

  INSTR_699:
  // ior vi09, vi12, vi00       |  nop                            699
  vu.vi09 = vu.vi12;
  L50:
  // isubiu vi01, vi09, 0x3a0   |  nop                            700
  vu.vi01 = vu.vi09 - 0x3a0; /* 928 */
  // iswr.x vi00, vi09          |  nop                            701
  isw_buffer(Mask::x, vu.vi00, vu.vi09);
  // iswr.y vi00, vi09          |  nop                            702
  isw_buffer(Mask::y, vu.vi00, vu.vi09);
  // BRANCH!
  // ibltz vi01, L48            |  nop                            703
  bc = ((s16)vu.vi01) < 0;
  // iaddi vi09, vi09, 0x7      |  nop                            704
  vu.vi09 = vu.vi09 + 7;
  if (bc) { goto L48; }

  // BRANCH!
  // ibeq vi00, vi05, L51       |  nop                            705
  bc = (vu.vi05 == 0);
  // mtir vi15, vf31.x          |  nop                            706
  vu.vi15 = vu.vf31.x_as_u16();
  if (bc) { goto L51; }

  // iaddiu vi05, vi05, 0x4000  |  nop                            707
  vu.vi05 = vu.vi05 + 0x4000; /* 16384 */
  // iaddiu vi05, vi05, 0x4000  |  nop                            708
  vu.vi05 = vu.vi05 + 0x4000; /* 16384 */
  // iswr.x vi05, vi04          |  nop                            709
  isw_buffer(Mask::x, vu.vi05, vu.vi04);
  // iaddiu vi01, vi00, 0x3ac   |  nop                            710
  vu.vi01 = 0x3ac; /* 940 */
  // xgkick vi01                |  nop                            711
  xgkick(vu.vi01);
  // lq.xyzw vf30, 888(vi00)    |  nop                            712
  lq_buffer(Mask::xyzw, vu.vf30, 888);
  // lq.xyzw vf31, 890(vi00)    |  nop                            713
  lq_buffer(Mask::xyzw, vu.vf31, 890);
  // iaddiu vi01, vi00, 0x3fe   |  nop                            714
  vu.vi01 = 0x3fe; /* 1022 */
  // isubiu vi02, vi00, 0x7fff  |  nop                            715
  vu.vi02 = -32767;
  // sq.xyzw vf30, 0(vi01)      |  nop                            716
  sq_buffer(Mask::xyzw, vu.vf30, vu.vi01);
  // iswr.x vi02, vi01          |  nop                            717
  isw_buffer(Mask::x, vu.vi02, vu.vi01);
  // sq.xyzw vf31, 1(vi01)      |  nop                            718
  sq_buffer(Mask::xyzw, vu.vf31, vu.vi01 + 1);
  // nop                        |  nop                            719

  // xgkick vi01                |  nop                            720
  xgkick(vu.vi01);
  L51:
  // nop                        |  nop                            721

  // jr vi15                    |  nop                            722
  // clang-format on
  switch (vu.vi15) {
    case 191:
      goto INSTR_191;
    case 205:
      goto INSTR_205;
    case 330:
      goto INSTR_330;
    case 513:
      goto INSTR_513;
    case 527:
      goto INSTR_527;
    default:
      ASSERT_MSG(false, fmt::format("unknown vu.vi15 @ 722: {}", vu.vi15));
  }
}