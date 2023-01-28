#include "OceanNear.h"

void OceanNear::run_call0_vu2c() {
  bool bc;
  // lq.xyzw vf01, 951(vi00)    |  nop                            0
  lq_buffer(Mask::xyzw, vu.vf01, 951);
  // lq.xyzw vf02, 953(vi00)    |  nop                            1
  lq_buffer(Mask::xyzw, vu.vf02, 953);
  // lq.xyzw vf03, 954(vi00)    |  nop                            2
  lq_buffer(Mask::xyzw, vu.vf03, 954);
  // lq.xyzw vf05, 955(vi00)    |  nop                            3
  lq_buffer(Mask::xyzw, vu.vf05, 955);
  // lq.xyzw vf06, 956(vi00)    |  nop                            4
  lq_buffer(Mask::xyzw, vu.vf06, 956);
  // iaddiu vi09, vi00, 0x213   |  nop                            5
  vu.vi09 = 0x213; /* 531 */
  // iaddi vi01, vi00, 0x6      |  nop                            6
  vu.vi01 = 6;
L1:
  // lq.xyzw vf12, 962(vi01)    |  nop                            7
  lq_buffer(Mask::xyzw, vu.vf12, vu.vi01 + 962);
  // lq.xyzw vf13, 969(vi01)    |  nop                            8
  lq_buffer(Mask::xyzw, vu.vf13, vu.vi01 + 969);
  // sq.xyzw vf12, 531(vi01)    |  nop                            9
  sq_buffer(Mask::xyzw, vu.vf12, vu.vi01 + 531);
  // sq.xyzw vf12, 671(vi01)    |  nop                            10
  sq_buffer(Mask::xyzw, vu.vf12, vu.vi01 + 671);
  // sq.xyzw vf13, 592(vi01)    |  nop                            11
  sq_buffer(Mask::xyzw, vu.vf13, vu.vi01 + 592);
  // sq.xyzw vf13, 732(vi01)    |  nop                            12
  sq_buffer(Mask::xyzw, vu.vf13, vu.vi01 + 732);
  // sq.xyzw vf12, 811(vi01)    |  nop                            13
  sq_buffer(Mask::xyzw, vu.vf12, vu.vi01 + 811);
  // sq.xyzw vf12, 881(vi01)    |  nop                            14
  sq_buffer(Mask::xyzw, vu.vf12, vu.vi01 + 881);
  // sq.xyzw vf13, 846(vi01)    |  nop                            15
  sq_buffer(Mask::xyzw, vu.vf13, vu.vi01 + 846);
  // sq.xyzw vf13, 916(vi01)    |  nop                            16
  sq_buffer(Mask::xyzw, vu.vf13, vu.vi01 + 916);
  // BRANCH!
  // ibgtz vi01, L1             |  nop                            17
  bc = ((s16)vu.vi01) > 0;
  // iaddi vi01, vi01, -0x1     |  nop                            18
  vu.vi01 = vu.vi01 + -1;
  if (bc) {
    goto L1;
  }

  // lq.xyzw vf08, 976(vi00)    |  nop                            19
  lq_buffer(Mask::xyzw, vu.vf08, 976);
  // iaddi vi04, vi00, 0x8      |  nop                            20
  vu.vi04 = 8;
  // iaddiu vi06, vi00, 0x11    |  ftoi0.xyzw vf08, vf08          21
  vu.vf08.ftoi0(Mask::xyzw, vu.vf08);
  vu.vi06 = 0x11; /* 17 */
L2:
  // sq.xyzw vf08, 592(vi04)    |  nop                            22
  sq_buffer(Mask::xyzw, vu.vf08, vu.vi04 + 592);
  // sq.xyzw vf08, 732(vi04)    |  nop                            23
  sq_buffer(Mask::xyzw, vu.vf08, vu.vi04 + 732);
  // iaddi vi04, vi04, 0x3      |  nop                            24
  vu.vi04 = vu.vi04 + 3;
  // BRANCH!
  // ibgtz vi06, L2             |  nop                            25
  bc = ((s16)vu.vi06) > 0;
  // iaddi vi06, vi06, -0x1     |  nop                            26
  vu.vi06 = vu.vi06 + -1;
  if (bc) {
    goto L2;
  }

  // lq.xyzw vf07, 957(vi00)    |  nop                            27
  lq_buffer(Mask::xyzw, vu.vf07, 957);
  // iaddi vi05, vi00, 0x0      |  mul.xyzw vf16, vf00, vf00      28
  vu.vf16.mul(Mask::xyzw, vu.vf00, vu.vf00);
  vu.vi05 = 0;
  // iaddi vi07, vi00, 0x8      |  nop                            29
  vu.vi07 = 8;
L3:
  // iaddi vi06, vi00, 0x8      |  mul.x vf16, vf00, vf00         30
  vu.vf16.mul(Mask::x, vu.vf00, vu.vf00);
  vu.vi06 = 8;
L4:
  // sq.xyzw vf16, 290(vi05)    |  nop                            31
  sq_buffer(Mask::xyzw, vu.vf16, vu.vi05 + 290);
  // iaddi vi05, vi05, 0x3      |  addx.x vf16, vf16, vf07        32
  vu.vf16.add(Mask::x, vu.vf16, vu.vf07.x());
  vu.vi05 = vu.vi05 + 3;
  // BRANCH!
  // ibgtz vi06, L4             |  nop                            33
  bc = ((s16)vu.vi06) > 0;
  // iaddi vi06, vi06, -0x1     |  nop                            34
  vu.vi06 = vu.vi06 + -1;
  if (bc) {
    goto L4;
  }

  // BRANCH!
  // ibgtz vi07, L3             |  addx.z vf16, vf16, vf07        35
  vu.vf16.add(Mask::z, vu.vf16, vu.vf07.x());
  bc = ((s16)vu.vi07) > 0;
  // iaddi vi07, vi07, -0x1     |  nop                            36
  vu.vi07 = vu.vi07 + -1;
  if (bc) {
    goto L3;
  }

  // nop                        |  nop :e                         37

  // nop                        |  nop                            38
}

void OceanNear::run_call39_vu2c() {
  bool bc;
  // xtop vi02                  |  nop                            39
  vu.vi02 = xtop();
  // ilw.x vi03, 10(vi02)       |  nop                            40
  ilw_buffer(Mask::x, vu.vi03, vu.vi02 + 10);
  // ilw.y vi04, 10(vi02)       |  nop                            41
  ilw_buffer(Mask::y, vu.vi04, vu.vi02 + 10);
  // iaddi vi05, vi00, 0x0      |  nop                            42
  vu.vi05 = 0;
  // iaddi vi07, vi00, 0x7      |  nop                            43
  vu.vi07 = 7;
L6:
  // lq.xyzw vf28, 32(vi03)     |  nop                            44
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi03 + 32);
  // lq.xyzw vf29, 33(vi03)     |  nop                            45
  lq_buffer(Mask::xyzw, vu.vf29, vu.vi03 + 33);
  // lq.xyzw vf30, 32(vi04)     |  nop                            46
  lq_buffer(Mask::xyzw, vu.vf30, vu.vi04 + 32);
  // lq.xyzw vf24, 290(vi05)    |  nop                            47
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi05 + 290);
  // lq.xyzw vf25, 293(vi05)    |  nop                            48
  lq_buffer(Mask::xyzw, vu.vf25, vu.vi05 + 293);
  // lq.xyzw vf26, 296(vi05)    |  nop                            49
  lq_buffer(Mask::xyzw, vu.vf26, vu.vi05 + 296);
  // lq.xyzw vf27, 299(vi05)    |  nop                            50
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi05 + 299);
  // lq.xyzw vf12, 302(vi05)    |  addx.y vf24, vf00, vf28        51
  vu.vf24.add(Mask::y, vu.vf00, vu.vf28.x());
  lq_buffer(Mask::xyzw, vu.vf12, vu.vi05 + 302);
  // lq.xyzw vf13, 305(vi05)    |  addy.y vf25, vf00, vf28        52
  vu.vf25.add(Mask::y, vu.vf00, vu.vf28.y());
  lq_buffer(Mask::xyzw, vu.vf13, vu.vi05 + 305);
  // lq.xyzw vf14, 308(vi05)    |  addz.y vf26, vf00, vf28        53
  vu.vf26.add(Mask::y, vu.vf00, vu.vf28.z());
  lq_buffer(Mask::xyzw, vu.vf14, vu.vi05 + 308);
  // lq.xyzw vf15, 311(vi05)    |  addw.y vf27, vf00, vf28        54
  vu.vf27.add(Mask::y, vu.vf00, vu.vf28.w());
  lq_buffer(Mask::xyzw, vu.vf15, vu.vi05 + 311);
  // lq.xyzw vf08, 314(vi05)    |  addx.y vf12, vf00, vf29        55
  vu.vf12.add(Mask::y, vu.vf00, vu.vf29.x());
  lq_buffer(Mask::xyzw, vu.vf08, vu.vi05 + 314);
  // sq.xyzw vf24, 290(vi05)    |  addy.y vf13, vf00, vf29        56
  vu.vf13.add(Mask::y, vu.vf00, vu.vf29.y());
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi05 + 290);
  // sq.xyzw vf25, 293(vi05)    |  addz.y vf14, vf00, vf29        57
  vu.vf14.add(Mask::y, vu.vf00, vu.vf29.z());
  sq_buffer(Mask::xyzw, vu.vf25, vu.vi05 + 293);
  // sq.xyzw vf26, 296(vi05)    |  addw.y vf15, vf00, vf29        58
  vu.vf15.add(Mask::y, vu.vf00, vu.vf29.w());
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi05 + 296);
  // sq.xyzw vf27, 299(vi05)    |  addx.y vf08, vf00, vf30        59
  vu.vf08.add(Mask::y, vu.vf00, vu.vf30.x());
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi05 + 299);
  // sq.xyzw vf12, 302(vi05)    |  nop                            60
  sq_buffer(Mask::xyzw, vu.vf12, vu.vi05 + 302);
  // sq.xyzw vf13, 305(vi05)    |  nop                            61
  sq_buffer(Mask::xyzw, vu.vf13, vu.vi05 + 305);
  // sq.xyzw vf14, 308(vi05)    |  nop                            62
  sq_buffer(Mask::xyzw, vu.vf14, vu.vi05 + 308);
  // sq.xyzw vf15, 311(vi05)    |  nop                            63
  sq_buffer(Mask::xyzw, vu.vf15, vu.vi05 + 311);
  // sq.xyzw vf08, 314(vi05)    |  nop                            64
  sq_buffer(Mask::xyzw, vu.vf08, vu.vi05 + 314);
  // iaddi vi03, vi03, 0x8      |  nop                            65
  vu.vi03 = vu.vi03 + 8;
  // iaddi vi04, vi04, 0x8      |  nop                            66
  vu.vi04 = vu.vi04 + 8;
  // iaddiu vi05, vi05, 0x1b    |  nop                            67
  vu.vi05 = vu.vi05 + 0x1b; /* 27 */
  // BRANCH!
  // ibgtz vi07, L6             |  nop                            68
  bc = ((s16)vu.vi07) > 0;
  // iaddi vi07, vi07, -0x1     |  nop                            69
  vu.vi07 = vu.vi07 + -1;
  if (bc) {
    goto L6;
  }

  // ilw.z vi03, 10(vi02)       |  nop                            70
  ilw_buffer(Mask::z, vu.vi03, vu.vi02 + 10);
  // ilw.w vi04, 10(vi02)       |  nop                            71
  ilw_buffer(Mask::w, vu.vi04, vu.vi02 + 10);
  // lq.xyzw vf28, 32(vi03)     |  nop                            72
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi03 + 32);
  // lq.xyzw vf29, 33(vi03)     |  nop                            73
  lq_buffer(Mask::xyzw, vu.vf29, vu.vi03 + 33);
  // lq.xyzw vf30, 32(vi04)     |  nop                            74
  lq_buffer(Mask::xyzw, vu.vf30, vu.vi04 + 32);
  // lq.xyzw vf24, 290(vi05)    |  nop                            75
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi05 + 290);
  // lq.xyzw vf25, 293(vi05)    |  nop                            76
  lq_buffer(Mask::xyzw, vu.vf25, vu.vi05 + 293);
  // lq.xyzw vf26, 296(vi05)    |  nop                            77
  lq_buffer(Mask::xyzw, vu.vf26, vu.vi05 + 296);
  // lq.xyzw vf27, 299(vi05)    |  nop                            78
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi05 + 299);
  // lq.xyzw vf12, 302(vi05)    |  addx.y vf24, vf00, vf28        79
  vu.vf24.add(Mask::y, vu.vf00, vu.vf28.x());
  lq_buffer(Mask::xyzw, vu.vf12, vu.vi05 + 302);
  // lq.xyzw vf13, 305(vi05)    |  addy.y vf25, vf00, vf28        80
  vu.vf25.add(Mask::y, vu.vf00, vu.vf28.y());
  lq_buffer(Mask::xyzw, vu.vf13, vu.vi05 + 305);
  // lq.xyzw vf14, 308(vi05)    |  addz.y vf26, vf00, vf28        81
  vu.vf26.add(Mask::y, vu.vf00, vu.vf28.z());
  lq_buffer(Mask::xyzw, vu.vf14, vu.vi05 + 308);
  // lq.xyzw vf15, 311(vi05)    |  addw.y vf27, vf00, vf28        82
  vu.vf27.add(Mask::y, vu.vf00, vu.vf28.w());
  lq_buffer(Mask::xyzw, vu.vf15, vu.vi05 + 311);
  // lq.xyzw vf08, 314(vi05)    |  addx.y vf12, vf00, vf29        83
  vu.vf12.add(Mask::y, vu.vf00, vu.vf29.x());
  lq_buffer(Mask::xyzw, vu.vf08, vu.vi05 + 314);
  // sq.xyzw vf24, 290(vi05)    |  addy.y vf13, vf00, vf29        84
  vu.vf13.add(Mask::y, vu.vf00, vu.vf29.y());
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi05 + 290);
  // sq.xyzw vf25, 293(vi05)    |  addz.y vf14, vf00, vf29        85
  vu.vf14.add(Mask::y, vu.vf00, vu.vf29.z());
  sq_buffer(Mask::xyzw, vu.vf25, vu.vi05 + 293);
  // sq.xyzw vf26, 296(vi05)    |  addw.y vf15, vf00, vf29        86
  vu.vf15.add(Mask::y, vu.vf00, vu.vf29.w());
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi05 + 296);
  // sq.xyzw vf27, 299(vi05)    |  addx.y vf08, vf00, vf30        87
  vu.vf08.add(Mask::y, vu.vf00, vu.vf30.x());
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi05 + 299);
  // sq.xyzw vf12, 302(vi05)    |  nop                            88
  sq_buffer(Mask::xyzw, vu.vf12, vu.vi05 + 302);
  // sq.xyzw vf13, 305(vi05)    |  nop                            89
  sq_buffer(Mask::xyzw, vu.vf13, vu.vi05 + 305);
  // sq.xyzw vf14, 308(vi05)    |  nop                            90
  sq_buffer(Mask::xyzw, vu.vf14, vu.vi05 + 308);
  // sq.xyzw vf15, 311(vi05)    |  nop                            91
  sq_buffer(Mask::xyzw, vu.vf15, vu.vi05 + 311);
  // sq.xyzw vf08, 314(vi05)    |  nop                            92
  sq_buffer(Mask::xyzw, vu.vf08, vu.vi05 + 314);
  // lq.xyzw vf07, 957(vi00)    |  nop                            93
  lq_buffer(Mask::xyzw, vu.vf07, 957);
  // lq.xyzw vf12, 11(vi02)     |  nop                            94
  lq_buffer(Mask::xyzw, vu.vf12, vu.vi02 + 11);
  // lq.xyzw vf13, 11(vi02)     |  nop                            95
  lq_buffer(Mask::xyzw, vu.vf13, vu.vi02 + 11);
  // lq.xyzw vf22, 12(vi02)     |  nop                            96
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi02 + 12);
  // lq.xyzw vf23, 13(vi02)     |  nop                            97
  lq_buffer(Mask::xyzw, vu.vf23, vu.vi02 + 13);
  // lq.xyzw vf16, 14(vi02)     |  nop                            98
  lq_buffer(Mask::xyzw, vu.vf16, vu.vi02 + 14);
  // lq.xyzw vf17, 15(vi02)     |  nop                            99
  lq_buffer(Mask::xyzw, vu.vf17, vu.vi02 + 15);
  // nop                        |  sub.xyzw vf09, vf16, vf22      100
  vu.vf09.sub(Mask::xyzw, vu.vf16, vu.vf22);
  // nop                        |  sub.xyzw vf11, vf17, vf23      101
  vu.vf11.sub(Mask::xyzw, vu.vf17, vu.vf23);
  // nop                        |  muly.xyzw vf09, vf09, vf07     102
  vu.vf09.mul(Mask::xyzw, vu.vf09, vu.vf07.y());
  // nop                        |  muly.xyzw vf11, vf11, vf07     103
  vu.vf11.mul(Mask::xyzw, vu.vf11, vu.vf07.y());
  // iaddi vi05, vi00, 0x0      |  nop                            104
  vu.vi05 = 0;
  // iaddi vi07, vi00, 0x8      |  nop                            105
  vu.vi07 = 8;
L7:
  // nop                        |  sub.xyzw vf08, vf23, vf22      106
  vu.vf08.sub(Mask::xyzw, vu.vf23, vu.vf22);
  // nop                        |  mulw.xyzw vf16, vf22, vf00     107
  vu.vf16.mul(Mask::xyzw, vu.vf22, vu.vf00.w());
  // iaddi vi06, vi00, 0x8      |  mulw.x vf12, vf13, vf00        108
  vu.vf12.mul(Mask::x, vu.vf13, vu.vf00.w());
  vu.vi06 = 8;
  // nop                        |  muly.xyzw vf08, vf08, vf07     109
  vu.vf08.mul(Mask::xyzw, vu.vf08, vu.vf07.y());
L8:
  // sq.xyzw vf12, 288(vi05)    |  nop                            110
  sq_buffer(Mask::xyzw, vu.vf12, vu.vi05 + 288);
  // sq.xyzw vf16, 289(vi05)    |  nop                            111
  sq_buffer(Mask::xyzw, vu.vf16, vu.vi05 + 289);
  // iaddi vi05, vi05, 0x3      |  addw.x vf12, vf12, vf07        112
  vu.vf12.add(Mask::x, vu.vf12, vu.vf07.w());
  vu.vi05 = vu.vi05 + 3;
  // BRANCH!
  // ibgtz vi06, L8             |  add.xyzw vf16, vf16, vf08      113
  vu.vf16.add(Mask::xyzw, vu.vf16, vu.vf08);
  bc = ((s16)vu.vi06) > 0;
  // iaddi vi06, vi06, -0x1     |  nop                            114
  vu.vi06 = vu.vi06 + -1;
  if (bc) {
    goto L8;
  }

  // nop                        |  add.xyzw vf22, vf22, vf09      115
  vu.vf22.add(Mask::xyzw, vu.vf22, vu.vf09);
  // nop                        |  add.xyzw vf23, vf23, vf11      116
  vu.vf23.add(Mask::xyzw, vu.vf23, vu.vf11);
  // BRANCH!
  // ibgtz vi07, L7             |  addw.y vf12, vf12, vf07        117
  vu.vf12.add(Mask::y, vu.vf12, vu.vf07.w());
  bc = ((s16)vu.vi07) > 0;
  // iaddi vi07, vi07, -0x1     |  nop                            118
  vu.vi07 = vu.vi07 + -1;
  if (bc) {
    goto L7;
  }

  // lq.xyzw vf08, 0(vi02)      |  nop                            119
  lq_buffer(Mask::xyzw, vu.vf08, vu.vi02);
  // lq.xyzw vf09, 1(vi02)      |  nop                            120
  lq_buffer(Mask::xyzw, vu.vf09, vu.vi02 + 1);
  // lq.xyzw vf10, 2(vi02)      |  nop                            121
  lq_buffer(Mask::xyzw, vu.vf10, vu.vi02 + 2);
  // lq.xyzw vf04, 8(vi02)      |  nop                            122
  lq_buffer(Mask::xyzw, vu.vf04, vu.vi02 + 8);
  // iaddi vi07, vi00, 0x3      |  nop                            123
  vu.vi07 = 3;
L9:
  // mtir vi10, vf04.x          |  nop                            124
  vu.vi10 = vu.vf04.x_as_u16();
  // iaddiu vi11, vi00, 0xff    |  nop                            125
  vu.vi11 = 0xff; /* 255 */
  // mr32.xyzw vf04, vf04       |  nop                            126
  vu.vf04.mr32(Mask::xyzw, vu.vf04);
  // BRANCH!
  // ibeq vi11, vi10, L11       |  nop                            127
  bc = (vu.vi11 == vu.vi10);
  // lq.xyzw vf11, 3(vi02)      |  nop                            128
  lq_buffer(Mask::xyzw, vu.vf11, vu.vi02 + 3);
  if (bc) {
    goto L11;
  }

  // lq.xyzw vf12, 4(vi02)      |  nop                            129
  lq_buffer(Mask::xyzw, vu.vf12, vu.vi02 + 4);
  // lq.xyzw vf13, 5(vi02)      |  nop                            130
  lq_buffer(Mask::xyzw, vu.vf13, vu.vi02 + 5);
  // lq.xyzw vf14, 6(vi02)      |  nop                            131
  lq_buffer(Mask::xyzw, vu.vf14, vu.vi02 + 6);
  // lq.xyzw vf15, 7(vi02)      |  nop                            132
  lq_buffer(Mask::xyzw, vu.vf15, vu.vi02 + 7);
  // ilw.x vi05, 983(vi07)      |  nop                            133
  ilw_buffer(Mask::x, vu.vi05, vu.vi07 + 983);
  // BRANCH!
  // bal vi15, L15              |  nop                            134
  // ASSERT(false);
  // iaddi vi08, vi09, 0x7      |  nop                            135
  vu.vi08 = vu.vi09 + 7;
  // if (bc) { goto L15; }
  run_L15_vu2c();

  // lq.xyzw vf07, 968(vi00)    |  nop                            136
  lq_buffer(Mask::xyzw, vu.vf07, 968);
  // iaddiu vi08, vi00, 0x3d1   |  nop                            137
  vu.vi08 = 0x3d1; /* 977 */
  // sq.xyzw vf07, 6(vi09)      |  nop                            138
  sq_buffer(Mask::xyzw, vu.vf07, vu.vi09 + 6);
  // xgkick vi09                |  nop                            139
  xgkick(vu.vi09);
  // lq.xyzw vf07, 980(vi00)    |  nop                            140
  lq_buffer(Mask::xyzw, vu.vf07, 980);
  // xgkick vi08                |  nop                            141
  xgkick(vu.vi08);
  // sq.xyzw vf07, 6(vi09)      |  nop                            142
  sq_buffer(Mask::xyzw, vu.vf07, vu.vi09 + 6);
  // iaddi vi08, vi09, 0x6      |  nop                            143
  vu.vi08 = vu.vi09 + 6;
  // nop                        |  nop                            144

  // xgkick vi08                |  nop                            145
  xgkick(vu.vi08);
  // iaddiu vi08, vi00, 0x3d5   |  nop                            146
  vu.vi08 = 0x3d5; /* 981 */
  // nop                        |  nop                            147

  // xgkick vi08                |  nop                            148
  xgkick(vu.vi08);
  // BRANCH!
  // bal vi15, L21              |  nop                            149
  // ASSERT(false);
  // iaddiu vi08, vi09, 0x44    |  nop                            150
  vu.vi08 = vu.vi09 + 0x44; /* 68 */
  // if (bc) { goto L21; }
  run_L21_vu2c();

  // BRANCH!
  // ibeq vi00, vi14, L10       |  nop                            151
  bc = (vu.vi14 == 0);
  // nop                        |  nop                            152

  if (bc) {
    goto L10;
  }

  // BRANCH!
  // bal vi15, L25              |  nop                            153
  // ASSERT(false);
  // nop                        |  nop                            154

  // if (bc) { goto L25; }
  run_L25_vu2c();

L10:
  // BRANCH!
  // bal vi15, L23              |  nop                            155
  // ASSERT(false);
  // iaddiu vi08, vi09, 0x44    |  nop                            156
  vu.vi08 = vu.vi09 + 0x44; /* 68 */
  // if (bc) { goto L23; }
  run_L23_vu2c();

  // iaddiu vi08, vi09, 0x3d    |  nop                            157
  vu.vi08 = vu.vi09 + 0x3d; /* 61 */
  // iaddiu vi01, vi00, 0x4b2   |  nop                            158
  vu.vi01 = 0x4b2; /* 1202 */
  // xgkick vi08                |  nop                            159
  xgkick(vu.vi08);
  // isub vi09, vi01, vi09      |  nop                            160
  vu.vi09 = vu.vi01 - vu.vi09;
L11:
  // BRANCH!
  // ibgtz vi07, L9             |  nop                            161
  bc = ((s16)vu.vi07) > 0;
  // iaddi vi07, vi07, -0x1     |  nop                            162
  vu.vi07 = vu.vi07 + -1;
  if (bc) {
    goto L9;
  }

  // lq.xyzw vf04, 9(vi02)      |  nop                            163
  lq_buffer(Mask::xyzw, vu.vf04, vu.vi02 + 9);
  // iaddi vi07, vi00, 0x3      |  nop                            164
  vu.vi07 = 3;
L12:
  // mtir vi10, vf04.x          |  nop                            165
  vu.vi10 = vu.vf04.x_as_u16();
  // iaddiu vi11, vi00, 0xff    |  nop                            166
  vu.vi11 = 0xff; /* 255 */
  // mr32.xyzw vf04, vf04       |  nop                            167
  vu.vf04.mr32(Mask::xyzw, vu.vf04);
  // BRANCH!
  // ibeq vi11, vi10, L14       |  nop                            168
  bc = (vu.vi11 == vu.vi10);
  // lq.xyzw vf11, 3(vi02)      |  nop                            169
  lq_buffer(Mask::xyzw, vu.vf11, vu.vi02 + 3);
  if (bc) {
    goto L14;
  }

  // lq.xyzw vf12, 4(vi02)      |  nop                            170
  lq_buffer(Mask::xyzw, vu.vf12, vu.vi02 + 4);
  // lq.xyzw vf13, 5(vi02)      |  nop                            171
  lq_buffer(Mask::xyzw, vu.vf13, vu.vi02 + 5);
  // lq.xyzw vf14, 6(vi02)      |  nop                            172
  lq_buffer(Mask::xyzw, vu.vf14, vu.vi02 + 6);
  // lq.xyzw vf15, 7(vi02)      |  nop                            173
  lq_buffer(Mask::xyzw, vu.vf15, vu.vi02 + 7);
  // ilw.y vi05, 983(vi07)      |  nop                            174
  ilw_buffer(Mask::y, vu.vi05, vu.vi07 + 983);
  // BRANCH!
  // bal vi15, L15              |  nop                            175
  // ASSERT(false);
  // iaddi vi08, vi09, 0x7      |  nop                            176
  vu.vi08 = vu.vi09 + 7;
  // if (bc) { goto L15; }
  run_L15_vu2c();

  // lq.xyzw vf07, 968(vi00)    |  nop                            177
  lq_buffer(Mask::xyzw, vu.vf07, 968);
  // iaddiu vi08, vi00, 0x3d1   |  nop                            178
  vu.vi08 = 0x3d1; /* 977 */
  // sq.xyzw vf07, 6(vi09)      |  nop                            179
  sq_buffer(Mask::xyzw, vu.vf07, vu.vi09 + 6);
  // xgkick vi09                |  nop                            180
  xgkick(vu.vi09);
  // lq.xyzw vf07, 980(vi00)    |  nop                            181
  lq_buffer(Mask::xyzw, vu.vf07, 980);
  // xgkick vi08                |  nop                            182
  xgkick(vu.vi08);
  // sq.xyzw vf07, 6(vi09)      |  nop                            183
  sq_buffer(Mask::xyzw, vu.vf07, vu.vi09 + 6);
  // iaddi vi08, vi09, 0x6      |  nop                            184
  vu.vi08 = vu.vi09 + 6;
  // nop                        |  nop                            185

  // xgkick vi08                |  nop                            186
  xgkick(vu.vi08);
  // iaddiu vi08, vi00, 0x3d5   |  nop                            187
  vu.vi08 = 0x3d5; /* 981 */
  // nop                        |  nop                            188

  // xgkick vi08                |  nop                            189
  xgkick(vu.vi08);
  // BRANCH!
  // bal vi15, L21              |  nop                            190
  // ASSERT(false);
  // iaddiu vi08, vi09, 0x44    |  nop                            191
  vu.vi08 = vu.vi09 + 0x44; /* 68 */
  // if (bc) { goto L21; }
  run_L21_vu2c();

  // BRANCH!
  // ibeq vi00, vi14, L13       |  nop                            192
  bc = (vu.vi14 == 0);
  // nop                        |  nop                            193

  if (bc) {
    goto L13;
  }

  // BRANCH!
  // bal vi15, L25              |  nop                            194
  // ASSERT(false);
  // nop                        |  nop                            195

  // if (bc) { goto L25; }
  run_L25_vu2c();

L13:
  // BRANCH!
  // bal vi15, L23              |  nop                            196
  // ASSERT(false);
  // iaddiu vi08, vi09, 0x44    |  nop                            197
  vu.vi08 = vu.vi09 + 0x44; /* 68 */
  // if (bc) { goto L23; }
  run_L23_vu2c();

  // iaddiu vi08, vi09, 0x3d    |  nop                            198
  vu.vi08 = vu.vi09 + 0x3d; /* 61 */
  // iaddiu vi01, vi00, 0x4b2   |  nop                            199
  vu.vi01 = 0x4b2; /* 1202 */
  // nop                        |  nop                            200

  // xgkick vi08                |  nop                            201
  xgkick(vu.vi08);
  // isub vi09, vi01, vi09      |  nop                            202
  vu.vi09 = vu.vi01 - vu.vi09;
L14:
  // BRANCH!
  // ibgtz vi07, L12            |  nop                            203
  bc = ((s16)vu.vi07) > 0;
  // iaddi vi07, vi07, -0x1     |  nop                            204
  vu.vi07 = vu.vi07 + -1;
  if (bc) {
    goto L12;
  }

  // nop                        |  nop :e                         205

  // nop                        |  nop                            206
}

void OceanNear::run_call0_vu2c_jak2() {
  bool bc;
  // lq.xyzw vf01, 951(vi00)    |  nop                            0
  lq_buffer(Mask::xyzw, vu.vf01, 951);
  // lq.xyzw vf02, 953(vi00)    |  nop                            1
  lq_buffer(Mask::xyzw, vu.vf02, 953);
  // lq.xyzw vf03, 954(vi00)    |  nop                            2
  lq_buffer(Mask::xyzw, vu.vf03, 954);
  // lq.xyzw vf05, 955(vi00)    |  nop                            3
  lq_buffer(Mask::xyzw, vu.vf05, 955);
  // lq.xyzw vf06, 956(vi00)    |  nop                            4
  lq_buffer(Mask::xyzw, vu.vf06, 956);
  // iaddiu vi09, vi00, 0x213   |  nop                            5
  vu.vi09 = 0x213; /* 531 */
  // iaddi vi01, vi00, 0x6      |  nop                            6
  vu.vi01 = 6;
L1:
  // lq.xyzw vf12, 962(vi01)    |  nop                            7
  lq_buffer(Mask::xyzw, vu.vf12, vu.vi01 + 963);
  // lq.xyzw vf13, 969(vi01)    |  nop                            8
  lq_buffer(Mask::xyzw, vu.vf13, vu.vi01 + 970);
  // sq.xyzw vf12, 531(vi01)    |  nop                            9
  sq_buffer(Mask::xyzw, vu.vf12, vu.vi01 + 531);
  // sq.xyzw vf12, 671(vi01)    |  nop                            10
  sq_buffer(Mask::xyzw, vu.vf12, vu.vi01 + 671);
  // sq.xyzw vf13, 592(vi01)    |  nop                            11
  sq_buffer(Mask::xyzw, vu.vf13, vu.vi01 + 592);
  // sq.xyzw vf13, 732(vi01)    |  nop                            12
  sq_buffer(Mask::xyzw, vu.vf13, vu.vi01 + 732);
  // sq.xyzw vf12, 811(vi01)    |  nop                            13
  sq_buffer(Mask::xyzw, vu.vf12, vu.vi01 + 811);
  // sq.xyzw vf12, 881(vi01)    |  nop                            14
  sq_buffer(Mask::xyzw, vu.vf12, vu.vi01 + 881);
  // sq.xyzw vf13, 846(vi01)    |  nop                            15
  sq_buffer(Mask::xyzw, vu.vf13, vu.vi01 + 846);
  // sq.xyzw vf13, 916(vi01)    |  nop                            16
  sq_buffer(Mask::xyzw, vu.vf13, vu.vi01 + 916);
  // BRANCH!
  // ibgtz vi01, L1             |  nop                            17
  bc = ((s16)vu.vi01) > 0;
  // iaddi vi01, vi01, -0x1     |  nop                            18
  vu.vi01 = vu.vi01 + -1;
  if (bc) {
    goto L1;
  }

  // lq.xyzw vf08, 977(vi00)    |  nop                            19
  lq_buffer(Mask::xyzw, vu.vf08, 977);
  // iaddi vi04, vi00, 0x8      |  nop                            20
  vu.vi04 = 8;
  // iaddiu vi06, vi00, 0x11    |  ftoi0.xyzw vf08, vf08          21
  vu.vf08.ftoi0(Mask::xyzw, vu.vf08);
  vu.vi06 = 0x11; /* 17 */
L2:
  // sq.xyzw vf08, 592(vi04)    |  nop                            22
  sq_buffer(Mask::xyzw, vu.vf08, vu.vi04 + 592);
  // sq.xyzw vf08, 732(vi04)    |  nop                            23
  sq_buffer(Mask::xyzw, vu.vf08, vu.vi04 + 732);
  // iaddi vi04, vi04, 0x3      |  nop                            24
  vu.vi04 = vu.vi04 + 3;
  // BRANCH!
  // ibgtz vi06, L2             |  nop                            25
  bc = ((s16)vu.vi06) > 0;
  // iaddi vi06, vi06, -0x1     |  nop                            26
  vu.vi06 = vu.vi06 + -1;
  if (bc) {
    goto L2;
  }

  // lq.xyzw vf07, 957(vi00)    |  nop                            27
  lq_buffer(Mask::xyzw, vu.vf07, 957);
  // iaddi vi05, vi00, 0x0      |  mul.xyzw vf16, vf00, vf00      28
  vu.vf16.mul(Mask::xyzw, vu.vf00, vu.vf00);
  vu.vi05 = 0;
  // iaddi vi07, vi00, 0x8      |  nop                            29
  vu.vi07 = 8;
L3:
  // iaddi vi06, vi00, 0x8      |  mul.x vf16, vf00, vf00         30
  vu.vf16.mul(Mask::x, vu.vf00, vu.vf00);
  vu.vi06 = 8;
L4:
  // sq.xyzw vf16, 290(vi05)    |  nop                            31
  sq_buffer(Mask::xyzw, vu.vf16, vu.vi05 + 290);
  // iaddi vi05, vi05, 0x3      |  addx.x vf16, vf16, vf07        32
  vu.vf16.add(Mask::x, vu.vf16, vu.vf07.x());
  vu.vi05 = vu.vi05 + 3;
  // BRANCH!
  // ibgtz vi06, L4             |  nop                            33
  bc = ((s16)vu.vi06) > 0;
  // iaddi vi06, vi06, -0x1     |  nop                            34
  vu.vi06 = vu.vi06 + -1;
  if (bc) {
    goto L4;
  }

  // BRANCH!
  // ibgtz vi07, L3             |  addx.z vf16, vf16, vf07        35
  vu.vf16.add(Mask::z, vu.vf16, vu.vf07.x());
  bc = ((s16)vu.vi07) > 0;
  // iaddi vi07, vi07, -0x1     |  nop                            36
  vu.vi07 = vu.vi07 + -1;
  if (bc) {
    goto L3;
  }

  // nop                        |  nop :e                         37

  // nop                        |  nop                            38
}

void OceanNear::run_call39_vu2c_jak2() {
  bool bc;
  // xtop vi02                  |  nop                            39
  vu.vi02 = xtop();
  // ilw.x vi03, 10(vi02)       |  nop                            40
  ilw_buffer(Mask::x, vu.vi03, vu.vi02 + 10);
  // ilw.y vi04, 10(vi02)       |  nop                            41
  ilw_buffer(Mask::y, vu.vi04, vu.vi02 + 10);
  // iaddi vi05, vi00, 0x0      |  nop                            42
  vu.vi05 = 0;
  // iaddi vi07, vi00, 0x7      |  nop                            43
  vu.vi07 = 7;
L6:
  // lq.xyzw vf28, 32(vi03)     |  nop                            44
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi03 + 32);
  // lq.xyzw vf29, 33(vi03)     |  nop                            45
  lq_buffer(Mask::xyzw, vu.vf29, vu.vi03 + 33);
  // lq.xyzw vf30, 32(vi04)     |  nop                            46
  lq_buffer(Mask::xyzw, vu.vf30, vu.vi04 + 32);
  // lq.xyzw vf24, 290(vi05)    |  nop                            47
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi05 + 290);
  // lq.xyzw vf25, 293(vi05)    |  nop                            48
  lq_buffer(Mask::xyzw, vu.vf25, vu.vi05 + 293);
  // lq.xyzw vf26, 296(vi05)    |  nop                            49
  lq_buffer(Mask::xyzw, vu.vf26, vu.vi05 + 296);
  // lq.xyzw vf27, 299(vi05)    |  nop                            50
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi05 + 299);
  // lq.xyzw vf12, 302(vi05)    |  addx.y vf24, vf00, vf28        51
  vu.vf24.add(Mask::y, vu.vf00, vu.vf28.x());
  lq_buffer(Mask::xyzw, vu.vf12, vu.vi05 + 302);
  // lq.xyzw vf13, 305(vi05)    |  addy.y vf25, vf00, vf28        52
  vu.vf25.add(Mask::y, vu.vf00, vu.vf28.y());
  lq_buffer(Mask::xyzw, vu.vf13, vu.vi05 + 305);
  // lq.xyzw vf14, 308(vi05)    |  addz.y vf26, vf00, vf28        53
  vu.vf26.add(Mask::y, vu.vf00, vu.vf28.z());
  lq_buffer(Mask::xyzw, vu.vf14, vu.vi05 + 308);
  // lq.xyzw vf15, 311(vi05)    |  addw.y vf27, vf00, vf28        54
  vu.vf27.add(Mask::y, vu.vf00, vu.vf28.w());
  lq_buffer(Mask::xyzw, vu.vf15, vu.vi05 + 311);
  // lq.xyzw vf08, 314(vi05)    |  addx.y vf12, vf00, vf29        55
  vu.vf12.add(Mask::y, vu.vf00, vu.vf29.x());
  lq_buffer(Mask::xyzw, vu.vf08, vu.vi05 + 314);
  // sq.xyzw vf24, 290(vi05)    |  addy.y vf13, vf00, vf29        56
  vu.vf13.add(Mask::y, vu.vf00, vu.vf29.y());
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi05 + 290);
  // sq.xyzw vf25, 293(vi05)    |  addz.y vf14, vf00, vf29        57
  vu.vf14.add(Mask::y, vu.vf00, vu.vf29.z());
  sq_buffer(Mask::xyzw, vu.vf25, vu.vi05 + 293);
  // sq.xyzw vf26, 296(vi05)    |  addw.y vf15, vf00, vf29        58
  vu.vf15.add(Mask::y, vu.vf00, vu.vf29.w());
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi05 + 296);
  // sq.xyzw vf27, 299(vi05)    |  addx.y vf08, vf00, vf30        59
  vu.vf08.add(Mask::y, vu.vf00, vu.vf30.x());
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi05 + 299);
  // sq.xyzw vf12, 302(vi05)    |  nop                            60
  sq_buffer(Mask::xyzw, vu.vf12, vu.vi05 + 302);
  // sq.xyzw vf13, 305(vi05)    |  nop                            61
  sq_buffer(Mask::xyzw, vu.vf13, vu.vi05 + 305);
  // sq.xyzw vf14, 308(vi05)    |  nop                            62
  sq_buffer(Mask::xyzw, vu.vf14, vu.vi05 + 308);
  // sq.xyzw vf15, 311(vi05)    |  nop                            63
  sq_buffer(Mask::xyzw, vu.vf15, vu.vi05 + 311);
  // sq.xyzw vf08, 314(vi05)    |  nop                            64
  sq_buffer(Mask::xyzw, vu.vf08, vu.vi05 + 314);
  // iaddi vi03, vi03, 0x8      |  nop                            65
  vu.vi03 = vu.vi03 + 8;
  // iaddi vi04, vi04, 0x8      |  nop                            66
  vu.vi04 = vu.vi04 + 8;
  // iaddiu vi05, vi05, 0x1b    |  nop                            67
  vu.vi05 = vu.vi05 + 0x1b; /* 27 */
  // BRANCH!
  // ibgtz vi07, L6             |  nop                            68
  bc = ((s16)vu.vi07) > 0;
  // iaddi vi07, vi07, -0x1     |  nop                            69
  vu.vi07 = vu.vi07 + -1;
  if (bc) {
    goto L6;
  }

  // ilw.z vi03, 10(vi02)       |  nop                            70
  ilw_buffer(Mask::z, vu.vi03, vu.vi02 + 10);
  // ilw.w vi04, 10(vi02)       |  nop                            71
  ilw_buffer(Mask::w, vu.vi04, vu.vi02 + 10);
  // lq.xyzw vf28, 32(vi03)     |  nop                            72
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi03 + 32);
  // lq.xyzw vf29, 33(vi03)     |  nop                            73
  lq_buffer(Mask::xyzw, vu.vf29, vu.vi03 + 33);
  // lq.xyzw vf30, 32(vi04)     |  nop                            74
  lq_buffer(Mask::xyzw, vu.vf30, vu.vi04 + 32);
  // lq.xyzw vf24, 290(vi05)    |  nop                            75
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi05 + 290);
  // lq.xyzw vf25, 293(vi05)    |  nop                            76
  lq_buffer(Mask::xyzw, vu.vf25, vu.vi05 + 293);
  // lq.xyzw vf26, 296(vi05)    |  nop                            77
  lq_buffer(Mask::xyzw, vu.vf26, vu.vi05 + 296);
  // lq.xyzw vf27, 299(vi05)    |  nop                            78
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi05 + 299);
  // lq.xyzw vf12, 302(vi05)    |  addx.y vf24, vf00, vf28        79
  vu.vf24.add(Mask::y, vu.vf00, vu.vf28.x());
  lq_buffer(Mask::xyzw, vu.vf12, vu.vi05 + 302);
  // lq.xyzw vf13, 305(vi05)    |  addy.y vf25, vf00, vf28        80
  vu.vf25.add(Mask::y, vu.vf00, vu.vf28.y());
  lq_buffer(Mask::xyzw, vu.vf13, vu.vi05 + 305);
  // lq.xyzw vf14, 308(vi05)    |  addz.y vf26, vf00, vf28        81
  vu.vf26.add(Mask::y, vu.vf00, vu.vf28.z());
  lq_buffer(Mask::xyzw, vu.vf14, vu.vi05 + 308);
  // lq.xyzw vf15, 311(vi05)    |  addw.y vf27, vf00, vf28        82
  vu.vf27.add(Mask::y, vu.vf00, vu.vf28.w());
  lq_buffer(Mask::xyzw, vu.vf15, vu.vi05 + 311);
  // lq.xyzw vf08, 314(vi05)    |  addx.y vf12, vf00, vf29        83
  vu.vf12.add(Mask::y, vu.vf00, vu.vf29.x());
  lq_buffer(Mask::xyzw, vu.vf08, vu.vi05 + 314);
  // sq.xyzw vf24, 290(vi05)    |  addy.y vf13, vf00, vf29        84
  vu.vf13.add(Mask::y, vu.vf00, vu.vf29.y());
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi05 + 290);
  // sq.xyzw vf25, 293(vi05)    |  addz.y vf14, vf00, vf29        85
  vu.vf14.add(Mask::y, vu.vf00, vu.vf29.z());
  sq_buffer(Mask::xyzw, vu.vf25, vu.vi05 + 293);
  // sq.xyzw vf26, 296(vi05)    |  addw.y vf15, vf00, vf29        86
  vu.vf15.add(Mask::y, vu.vf00, vu.vf29.w());
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi05 + 296);
  // sq.xyzw vf27, 299(vi05)    |  addx.y vf08, vf00, vf30        87
  vu.vf08.add(Mask::y, vu.vf00, vu.vf30.x());
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi05 + 299);
  // sq.xyzw vf12, 302(vi05)    |  nop                            88
  sq_buffer(Mask::xyzw, vu.vf12, vu.vi05 + 302);
  // sq.xyzw vf13, 305(vi05)    |  nop                            89
  sq_buffer(Mask::xyzw, vu.vf13, vu.vi05 + 305);
  // sq.xyzw vf14, 308(vi05)    |  nop                            90
  sq_buffer(Mask::xyzw, vu.vf14, vu.vi05 + 308);
  // sq.xyzw vf15, 311(vi05)    |  nop                            91
  sq_buffer(Mask::xyzw, vu.vf15, vu.vi05 + 311);
  // sq.xyzw vf08, 314(vi05)    |  nop                            92
  sq_buffer(Mask::xyzw, vu.vf08, vu.vi05 + 314);
  // lq.xyzw vf07, 957(vi00)    |  nop                            93
  lq_buffer(Mask::xyzw, vu.vf07, 957);
  // lq.xyzw vf12, 11(vi02)     |  nop                            94
  lq_buffer(Mask::xyzw, vu.vf12, vu.vi02 + 11);
  // lq.xyzw vf13, 11(vi02)     |  nop                            95
  lq_buffer(Mask::xyzw, vu.vf13, vu.vi02 + 11);
  // lq.xyzw vf22, 12(vi02)     |  nop                            96
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi02 + 12);
  // lq.xyzw vf23, 13(vi02)     |  nop                            97
  lq_buffer(Mask::xyzw, vu.vf23, vu.vi02 + 13);
  // lq.xyzw vf16, 14(vi02)     |  nop                            98
  lq_buffer(Mask::xyzw, vu.vf16, vu.vi02 + 14);
  // lq.xyzw vf17, 15(vi02)     |  nop                            99
  lq_buffer(Mask::xyzw, vu.vf17, vu.vi02 + 15);
  // nop                        |  sub.xyzw vf09, vf16, vf22      100
  vu.vf09.sub(Mask::xyzw, vu.vf16, vu.vf22);
  // nop                        |  sub.xyzw vf11, vf17, vf23      101
  vu.vf11.sub(Mask::xyzw, vu.vf17, vu.vf23);
  // nop                        |  muly.xyzw vf09, vf09, vf07     102
  vu.vf09.mul(Mask::xyzw, vu.vf09, vu.vf07.y());
  // nop                        |  muly.xyzw vf11, vf11, vf07     103
  vu.vf11.mul(Mask::xyzw, vu.vf11, vu.vf07.y());
  // iaddi vi05, vi00, 0x0      |  nop                            104
  vu.vi05 = 0;
  // iaddi vi07, vi00, 0x8      |  nop                            105
  vu.vi07 = 8;
L7:
  // nop                        |  sub.xyzw vf08, vf23, vf22      106
  vu.vf08.sub(Mask::xyzw, vu.vf23, vu.vf22);
  // nop                        |  mulw.xyzw vf16, vf22, vf00     107
  vu.vf16.mul(Mask::xyzw, vu.vf22, vu.vf00.w());
  // iaddi vi06, vi00, 0x8      |  mulw.x vf12, vf13, vf00        108
  vu.vf12.mul(Mask::x, vu.vf13, vu.vf00.w());
  vu.vi06 = 8;
  // nop                        |  muly.xyzw vf08, vf08, vf07     109
  vu.vf08.mul(Mask::xyzw, vu.vf08, vu.vf07.y());
L8:
  // sq.xyzw vf12, 288(vi05)    |  nop                            110
  sq_buffer(Mask::xyzw, vu.vf12, vu.vi05 + 288);
  // sq.xyzw vf16, 289(vi05)    |  nop                            111
  sq_buffer(Mask::xyzw, vu.vf16, vu.vi05 + 289);
  // iaddi vi05, vi05, 0x3      |  addw.x vf12, vf12, vf07        112
  vu.vf12.add(Mask::x, vu.vf12, vu.vf07.w());
  vu.vi05 = vu.vi05 + 3;
  // BRANCH!
  // ibgtz vi06, L8             |  add.xyzw vf16, vf16, vf08      113
  vu.vf16.add(Mask::xyzw, vu.vf16, vu.vf08);
  bc = ((s16)vu.vi06) > 0;
  // iaddi vi06, vi06, -0x1     |  nop                            114
  vu.vi06 = vu.vi06 + -1;
  if (bc) {
    goto L8;
  }

  // nop                        |  add.xyzw vf22, vf22, vf09      115
  vu.vf22.add(Mask::xyzw, vu.vf22, vu.vf09);
  // nop                        |  add.xyzw vf23, vf23, vf11      116
  vu.vf23.add(Mask::xyzw, vu.vf23, vu.vf11);
  // BRANCH!
  // ibgtz vi07, L7             |  addw.y vf12, vf12, vf07        117
  vu.vf12.add(Mask::y, vu.vf12, vu.vf07.w());
  bc = ((s16)vu.vi07) > 0;
  // iaddi vi07, vi07, -0x1     |  nop                            118
  vu.vi07 = vu.vi07 + -1;
  if (bc) {
    goto L7;
  }

  // lq.xyzw vf08, 0(vi02)      |  nop                            119
  lq_buffer(Mask::xyzw, vu.vf08, vu.vi02);
  // lq.xyzw vf09, 1(vi02)      |  nop                            120
  lq_buffer(Mask::xyzw, vu.vf09, vu.vi02 + 1);
  // lq.xyzw vf10, 2(vi02)      |  nop                            121
  lq_buffer(Mask::xyzw, vu.vf10, vu.vi02 + 2);
  // lq.xyzw vf04, 8(vi02)      |  nop                            122
  lq_buffer(Mask::xyzw, vu.vf04, vu.vi02 + 8);
  // iaddi vi07, vi00, 0x3      |  nop                            123
  vu.vi07 = 3;
L9:
  // mtir vi10, vf04.x          |  nop                            124
  vu.vi10 = vu.vf04.x_as_u16();
  // iaddiu vi11, vi00, 0xff    |  nop                            125
  vu.vi11 = 0xff; /* 255 */
  // mr32.xyzw vf04, vf04       |  nop                            126
  vu.vf04.mr32(Mask::xyzw, vu.vf04);
  // BRANCH!
  // ibeq vi11, vi10, L11       |  nop                            127
  bc = (vu.vi11 == vu.vi10);
  // lq.xyzw vf11, 3(vi02)      |  nop                            128
  lq_buffer(Mask::xyzw, vu.vf11, vu.vi02 + 3);
  if (bc) {
    goto L11;
  }

  // lq.xyzw vf12, 4(vi02)      |  nop                            129
  lq_buffer(Mask::xyzw, vu.vf12, vu.vi02 + 4);
  // lq.xyzw vf13, 5(vi02)      |  nop                            130
  lq_buffer(Mask::xyzw, vu.vf13, vu.vi02 + 5);
  // lq.xyzw vf14, 6(vi02)      |  nop                            131
  lq_buffer(Mask::xyzw, vu.vf14, vu.vi02 + 6);
  // lq.xyzw vf15, 7(vi02)      |  nop                            132
  lq_buffer(Mask::xyzw, vu.vf15, vu.vi02 + 7);
  // ilw.x vi05, 984(vi07)      |  nop                            133
  ilw_buffer(Mask::x, vu.vi05, vu.vi07 + 984);
  // BRANCH!
  // bal vi15, L15              |  nop                            134
  // ASSERT(false);
  // iaddi vi08, vi09, 0x7      |  nop                            135
  vu.vi08 = vu.vi09 + 7;
  // if (bc) { goto L15; }
  run_L15_vu2c_jak2();

  // lq.xyzw vf07, 969(vi00)    |  nop                            136
  lq_buffer(Mask::xyzw, vu.vf07, 969);
  // iaddiu vi08, vi00, 0x3d2   |  nop                            137
  vu.vi08 = 0x3d2; /* 977 */
  // sq.xyzw vf07, 6(vi09)      |  nop                            138
  sq_buffer(Mask::xyzw, vu.vf07, vu.vi09 + 6);
  // xgkick vi09                |  nop                            139
  xgkick(vu.vi09);
  // lq.xyzw vf07, 981(vi00)    |  nop                            140
  lq_buffer(Mask::xyzw, vu.vf07, 981);
  // xgkick vi08                |  nop                            141
  xgkick(vu.vi08);
  // sq.xyzw vf07, 6(vi09)      |  nop                            142
  sq_buffer(Mask::xyzw, vu.vf07, vu.vi09 + 6);
  // iaddi vi08, vi09, 0x6      |  nop                            143
  vu.vi08 = vu.vi09 + 6;
  // nop                        |  nop                            144

  // xgkick vi08                |  nop                            145
  xgkick(vu.vi08);
  // iaddiu vi08, vi00, 0x3d6   |  nop                            146
  vu.vi08 = 0x3d6; /* 981 */
  // nop                        |  nop                            147

  // xgkick vi08                |  nop                            148
  xgkick(vu.vi08);
  // BRANCH!
  // bal vi15, L21              |  nop                            149
  // ASSERT(false);
  // iaddiu vi08, vi09, 0x44    |  nop                            150
  vu.vi08 = vu.vi09 + 0x44; /* 68 */
  // if (bc) { goto L21; }
  run_L21_vu2c_jak2();

  // BRANCH!
  // ibeq vi00, vi14, L10       |  nop                            151
  bc = (vu.vi14 == 0);
  // nop                        |  nop                            152

  if (bc) {
    goto L10;
  }

  // BRANCH!
  // bal vi15, L25              |  nop                            153
  // ASSERT(false);
  // nop                        |  nop                            154

  // if (bc) { goto L25; }
  run_L25_vu2c_jak2();

L10:
  // BRANCH!
  // bal vi15, L23              |  nop                            155
  // ASSERT(false);
  // iaddiu vi08, vi09, 0x44    |  nop                            156
  vu.vi08 = vu.vi09 + 0x44; /* 68 */
  // if (bc) { goto L23; }
  run_L23_vu2c();

  // iaddiu vi08, vi09, 0x3d    |  nop                            157
  vu.vi08 = vu.vi09 + 0x3d; /* 61 */
  // iaddiu vi01, vi00, 0x4b2   |  nop                            158
  vu.vi01 = 0x4b2; /* 1202 */
  // xgkick vi08                |  nop                            159
  xgkick(vu.vi08);
  // isub vi09, vi01, vi09      |  nop                            160
  vu.vi09 = vu.vi01 - vu.vi09;
L11:
  // BRANCH!
  // ibgtz vi07, L9             |  nop                            161
  bc = ((s16)vu.vi07) > 0;
  // iaddi vi07, vi07, -0x1     |  nop                            162
  vu.vi07 = vu.vi07 + -1;
  if (bc) {
    goto L9;
  }

  // lq.xyzw vf04, 9(vi02)      |  nop                            163
  lq_buffer(Mask::xyzw, vu.vf04, vu.vi02 + 9);
  // iaddi vi07, vi00, 0x3      |  nop                            164
  vu.vi07 = 3;
L12:
  // mtir vi10, vf04.x          |  nop                            165
  vu.vi10 = vu.vf04.x_as_u16();
  // iaddiu vi11, vi00, 0xff    |  nop                            166
  vu.vi11 = 0xff; /* 255 */
  // mr32.xyzw vf04, vf04       |  nop                            167
  vu.vf04.mr32(Mask::xyzw, vu.vf04);
  // BRANCH!
  // ibeq vi11, vi10, L14       |  nop                            168
  bc = (vu.vi11 == vu.vi10);
  // lq.xyzw vf11, 3(vi02)      |  nop                            169
  lq_buffer(Mask::xyzw, vu.vf11, vu.vi02 + 3);
  if (bc) {
    goto L14;
  }

  // lq.xyzw vf12, 4(vi02)      |  nop                            170
  lq_buffer(Mask::xyzw, vu.vf12, vu.vi02 + 4);
  // lq.xyzw vf13, 5(vi02)      |  nop                            171
  lq_buffer(Mask::xyzw, vu.vf13, vu.vi02 + 5);
  // lq.xyzw vf14, 6(vi02)      |  nop                            172
  lq_buffer(Mask::xyzw, vu.vf14, vu.vi02 + 6);
  // lq.xyzw vf15, 7(vi02)      |  nop                            173
  lq_buffer(Mask::xyzw, vu.vf15, vu.vi02 + 7);
  // ilw.y vi05, 984(vi07)      |  nop                            174
  ilw_buffer(Mask::y, vu.vi05, vu.vi07 + 984);
  // BRANCH!
  // bal vi15, L15              |  nop                            175
  // ASSERT(false);
  // iaddi vi08, vi09, 0x7      |  nop                            176
  vu.vi08 = vu.vi09 + 7;
  // if (bc) { goto L15; }
  run_L15_vu2c_jak2();

  // lq.xyzw vf07, 969(vi00)    |  nop                            177
  lq_buffer(Mask::xyzw, vu.vf07, 969);
  // iaddiu vi08, vi00, 0x3d1   |  nop                            178
  vu.vi08 = 0x3d2; /* 977 */
  // sq.xyzw vf07, 6(vi09)      |  nop                            179
  sq_buffer(Mask::xyzw, vu.vf07, vu.vi09 + 6);
  // xgkick vi09                |  nop                            180
  xgkick(vu.vi09);
  // lq.xyzw vf07, 981(vi00)    |  nop                            181
  lq_buffer(Mask::xyzw, vu.vf07, 981);
  // xgkick vi08                |  nop                            182
  xgkick(vu.vi08);
  // sq.xyzw vf07, 6(vi09)      |  nop                            183
  sq_buffer(Mask::xyzw, vu.vf07, vu.vi09 + 6);
  // iaddi vi08, vi09, 0x6      |  nop                            184
  vu.vi08 = vu.vi09 + 6;
  // nop                        |  nop                            185

  // xgkick vi08                |  nop                            186
  xgkick(vu.vi08);
  // iaddiu vi08, vi00, 0x3d6   |  nop                            187
  vu.vi08 = 0x3d6; /* 981 */
  // nop                        |  nop                            188

  // xgkick vi08                |  nop                            189
  xgkick(vu.vi08);
  // BRANCH!
  // bal vi15, L21              |  nop                            190
  // ASSERT(false);
  // iaddiu vi08, vi09, 0x44    |  nop                            191
  vu.vi08 = vu.vi09 + 0x44; /* 68 */
  // if (bc) { goto L21; }
  run_L21_vu2c_jak2();

  // BRANCH!
  // ibeq vi00, vi14, L13       |  nop                            192
  bc = (vu.vi14 == 0);
  // nop                        |  nop                            193

  if (bc) {
    goto L13;
  }

  // BRANCH!
  // bal vi15, L25              |  nop                            194
  // ASSERT(false);
  // nop                        |  nop                            195

  // if (bc) { goto L25; }
  run_L25_vu2c_jak2();

L13:
  // BRANCH!
  // bal vi15, L23              |  nop                            196
  // ASSERT(false);
  // iaddiu vi08, vi09, 0x44    |  nop                            197
  vu.vi08 = vu.vi09 + 0x44; /* 68 */
  // if (bc) { goto L23; }
  run_L23_vu2c();

  // iaddiu vi08, vi09, 0x3d    |  nop                            198
  vu.vi08 = vu.vi09 + 0x3d; /* 61 */
  // iaddiu vi01, vi00, 0x4b2   |  nop                            199
  vu.vi01 = 0x4b2; /* 1202 */
  // nop                        |  nop                            200

  // xgkick vi08                |  nop                            201
  xgkick(vu.vi08);
  // isub vi09, vi01, vi09      |  nop                            202
  vu.vi09 = vu.vi01 - vu.vi09;
L14:
  // BRANCH!
  // ibgtz vi07, L12            |  nop                            203
  bc = ((s16)vu.vi07) > 0;
  // iaddi vi07, vi07, -0x1     |  nop                            204
  vu.vi07 = vu.vi07 + -1;
  if (bc) {
    goto L12;
  }

  // nop                        |  nop :e                         205

  // nop                        |  nop                            206
}

static inline REALLY_INLINE float eleng(const Vf& in) {
  float len = in.x() * in.x() + in.y() * in.y() + in.z() * in.z();
  return std::sqrt(len);
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

void fcor(u16& dest, u32 imm, u32 cf) {
  // dest = ((0xffffff & (cf | imm)) == 0xffffff) ? 1 : 0;
  u32 hold = (cf & 0xFFFFFF) | (imm & 0xFFFFFF);
  if (hold == 0xFFFFFF)
    dest = 1;
  else
    dest = 0;

  //  dest = cf | imm;
}
}  // namespace

void OceanNear::run_L15_vu2c() {
  u32 cf;
  bool bc;
  // iaddi vi01, vi05, 0x9      |  nop                            207
  vu.vi01 = vu.vi05 + 9;
  // lq.xyzw vf24, 290(vi05)    |  nop                            208
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi05 + 290);
  // lq.xyzw vf25, 317(vi05)    |  nop                            209
  lq_buffer(Mask::xyzw, vu.vf25, vu.vi05 + 317);
  // lq.xyzw vf07, 958(vi00)    |  mulax.xyzw ACC, vf08, vf24     210
  vu.acc.mula(Mask::xyzw, vu.vf08, vu.vf24.x());
  lq_buffer(Mask::xyzw, vu.vf07, 958);
  // iaddi vi05, vi05, 0x3      |  madday.xyzw ACC, vf09, vf24    211
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf24.y());
  vu.vi05 = vu.vi05 + 3;
  // isw.x vi01, 987(vi00)      |  nop                            212
  isw_buffer(Mask::x, vu.vi01, 987);
  // iaddi vi01, vi00, 0x0      |  maddaz.xyzw ACC, vf10, vf24    213
  vu.acc.madda(Mask::xyzw, vu.vf10, vu.vf24.z());
  vu.vi01 = 0;
  // iaddi vi13, vi00, 0x0      |  maddw.xyzw vf26, vf11, vf00    214
  vu.acc.madd(Mask::xyzw, vu.vf26, vu.vf11, vu.vf00.w());
  vu.vi13 = 0;
  // iaddi vi11, vi00, 0x0      |  mulax.xyzw ACC, vf08, vf25     215
  vu.acc.mula(Mask::xyzw, vu.vf08, vu.vf25.x());
  vu.vi11 = 0;
  // fcset 0x0                  |  madday.xyzw ACC, vf09, vf25    216
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf25.y());
  cf = 0x0;

  // lq.xyzw vf28, 287(vi05)    |  maddaz.xyzw ACC, vf10, vf25    217
  vu.acc.madda(Mask::xyzw, vu.vf10, vu.vf25.z());
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi05 + 287);
  // eleng.xyz P, vf26          |  maddw.xyzw vf27, vf11, vf00    218
  vu.acc.madd(Mask::xyzw, vu.vf27, vu.vf11, vu.vf00.w());
  vu.P = eleng(vu.vf26);
  // iaddi vi14, vi00, 0x0      |  mulw.xyzw vf20, vf26, vf00     219
  vu.vf20.mul(Mask::xyzw, vu.vf26, vu.vf00.w());
  vu.vi14 = 0;
  // lq.xyzw vf22, 286(vi05)    |  nop                            220
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi05 + 286);
  // waitp                      |  nop                            221
  // ASSERT(false);
  // mfp.w vf20, P              |  nop                            222
  vu.vf20.mfp(Mask::w, vu.P);
  // eleng.xyz P, vf27          |  mulw.xyzw vf21, vf27, vf00     223
  vu.vf21.mul(Mask::xyzw, vu.vf27, vu.vf00.w());
  vu.P = eleng(vu.vf27);
  // nop                        |  nop                            224

  // nop                        |  nop                            225

  // iaddi vi12, vi00, 0x1      |  mulw.w vf22, vf20, vf05        226
  vu.vf22.mul(Mask::w, vu.vf20, vu.vf05.w());
  vu.vi12 = 1;
  // nop                        |  nop                            227

  // nop                        |  nop                            228

  // nop                        |  nop                            229

  // nop                        |  miniw.w vf22, vf22, vf00       230
  vu.vf22.mini(Mask::w, vu.vf22, vu.vf00.w());
  // nop                        |  nop                            231

  // nop                        |  nop                            232

  // nop                        |  nop                            233

  // nop                        |  subw.w vf28, vf00, vf22        234
  vu.vf28.sub(Mask::w, vu.vf00, vu.vf22.w());
  // nop                        |  maxx.w vf22, vf22, vf05        235
  vu.vf22.max(Mask::w, vu.vf22, vu.vf05.x());
  // nop                        |  nop                            236

  // nop                        |  mulaz.w ACC, vf00, vf07        237
  vu.acc.mula(Mask::w, vu.vf00, vu.vf07.z());
  // nop                        |  mulw.y vf28, vf28, vf28        238
  vu.vf28.mul(Mask::y, vu.vf28, vu.vf28.w());
  // lq.xyzw vf24, 290(vi05)    |  msubx.w vf07, vf22, vf07       239
  vu.acc.msub(Mask::w, vu.vf07, vu.vf22, vu.vf07.x());
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi05 + 290);
  // nop                        |  mulax.xyzw ACC, vf12, vf28     240
  vu.acc.mula(Mask::xyzw, vu.vf12, vu.vf28.x());
  // nop                        |  madday.xyzw ACC, vf13, vf28    241
  vu.acc.madda(Mask::xyzw, vu.vf13, vu.vf28.y());
  // nop                        |  maddaz.xyzw ACC, vf14, vf28    242
  vu.acc.madda(Mask::xyzw, vu.vf14, vu.vf28.z());
  // nop                        |  maddw.xyzw vf30, vf15, vf00    243
  vu.acc.madd(Mask::xyzw, vu.vf30, vu.vf15, vu.vf00.w());
  // nop                        |  mulax.xyzw ACC, vf08, vf24     244
  vu.acc.mula(Mask::xyzw, vu.vf08, vu.vf24.x());
  // nop                        |  madday.xyzw ACC, vf09, vf24    245
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf24.y());
  // nop                        |  maddaz.xyzw ACC, vf10, vf24    246
  vu.acc.madda(Mask::xyzw, vu.vf10, vu.vf24.z());
  // nop                        |  maddw.xyzw vf26, vf11, vf00    247
  vu.acc.madd(Mask::xyzw, vu.vf26, vu.vf11, vu.vf00.w());
  // div Q, vf03.x, vf30.w      |  mul.xyzw vf18, vf30, vf01      248
  vu.vf18.mul(Mask::xyzw, vu.vf30, vu.vf01);
  vu.Q = vu.vf03.x() / vu.vf30.w();
  // lq.xyzw vf23, 313(vi05)    |  mulw.w vf22, vf22, vf06        249
  vu.vf22.mul(Mask::w, vu.vf22, vu.vf06.w());
  lq_buffer(Mask::xyzw, vu.vf23, vu.vi05 + 313);
  // waitp                      |  mulw.xyz vf22, vf22, vf07      250
  vu.vf22.mul(Mask::xyz, vu.vf22, vu.vf07.w());
  // mfp.w vf21, P              |  nop                            251
  vu.vf21.mfp(Mask::w, vu.P);
  // eleng.xyz P, vf26          |  nop                            252
  vu.P = eleng(vu.vf26);
  // nop                        |  clipw.xyz vf18, vf18           253
  cf = clip(vu.vf18, vu.vf18.w(), cf);
  // nop                        |  mulaw.w ACC, vf30, vf00        254
  vu.acc.mula(Mask::w, vu.vf30, vu.vf00.w());
  // lq.xyzw vf29, 314(vi05)    |  mulw.w vf23, vf21, vf05        255
  vu.vf23.mul(Mask::w, vu.vf21, vu.vf05.w());
  lq_buffer(Mask::xyzw, vu.vf29, vu.vi05 + 314);
  // nop                        |  mula.xyz ACC, vf30, Q          256
  vu.acc.mula(Mask::xyz, vu.vf30, vu.Q);
  // nop                        |  maddw.xyzw vf16, vf02, vf00    257
  vu.acc.madd(Mask::xyzw, vu.vf16, vu.vf02, vu.vf00.w());
  // nop                        |  miniy.xyzw vf22, vf22, vf07    258
  vu.vf22.mini(Mask::xyzw, vu.vf22, vu.vf07.y());
  // nop                        |  miniw.w vf23, vf23, vf00       259
  vu.vf23.mini(Mask::w, vu.vf23, vu.vf00.w());
  // nop                        |  nop                            260

  // nop                        |  miniz.w vf16, vf16, vf03       261
  vu.vf16.mini(Mask::w, vu.vf16, vu.vf03.z());
  // nop                        |  ftoi0.xyzw vf22, vf22          262
  vu.vf22.ftoi0(Mask::xyzw, vu.vf22);
  // waitp                      |  subw.w vf29, vf00, vf23        263
  vu.vf29.sub(Mask::w, vu.vf00, vu.vf23.w());
  // BRANCH!
  // b L18                      |  maxx.w vf23, vf23, vf05        264
  vu.vf23.max(Mask::w, vu.vf23, vu.vf05.x());
  bc = true;
  // iaddi vi06, vi00, 0x8      |  maxy.w vf16, vf16, vf03        265
  vu.vf16.max(Mask::w, vu.vf16, vu.vf03.y());
  vu.vi06 = 8;
  if (bc) {
    goto L18;
  }

L16:
  // BRANCH!
  // ibeq vi00, vi01, L17       |  nop                            266
  bc = (vu.vi01 == 0);
  // lq.xyzw vf26, 309(vi05)    |  nop                            267
  lq_buffer(Mask::xyzw, vu.vf26, vu.vi05 + 309);
  if (bc) {
    goto L17;
  }

  // BRANCH!
  // ibne vi00, vi13, L17       |  addw.w vf17, vf17, vf03        268
  vu.vf17.add(Mask::w, vu.vf17, vu.vf03.w());
  bc = (vu.vi13 != 0);
  // fcor vi01, 0xfdf7df        |  nop                            269
  fcor(vu.vi01, 0xfdf7df, cf);

  if (bc) {
    goto L17;
  }

  // isw.x vi12, 988(vi14)      |  nop                            270
  isw_buffer(Mask::x, vu.vi12, vu.vi14 + 988);
  // BRANCH!
  // ibne vi00, vi01, L17       |  nop                            271
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xff7df7        |  nop                            272
  fcor(vu.vi01, 0xff7df7, cf);

  if (bc) {
    goto L17;
  }

  // isw.y vi05, 988(vi14)      |  nop                            273
  isw_buffer(Mask::y, vu.vi05, vu.vi14 + 988);
  // BRANCH!
  // ibne vi00, vi01, L17       |  nop                            274
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xffbefb        |  nop                            275
  fcor(vu.vi01, 0xffbefb, cf);

  if (bc) {
    goto L17;
  }

  // ilw.x vi11, 987(vi00)      |  nop                            276
  ilw_buffer(Mask::x, vu.vi11, 987);
  // BRANCH!
  // ibne vi00, vi01, L17       |  nop                            277
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xffdf7d        |  nop                            278
  fcor(vu.vi01, 0xffdf7d, cf);

  if (bc) {
    goto L17;
  }

  // isw.z vi08, 988(vi14)      |  nop                            279
  isw_buffer(Mask::z, vu.vi08, vu.vi14 + 988);
  // BRANCH!
  // ibne vi00, vi01, L17       |  nop                            280
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xffefbe        |  nop                            281
  fcor(vu.vi01, 0xffefbe, cf);

  if (bc) {
    goto L17;
  }

  // isub vi11, vi05, vi11      |  nop                            282
  vu.vi11 = vu.vi05 - vu.vi11;
  // BRANCH!
  // ibne vi00, vi01, L17       |  nop                            283
  bc = (vu.vi01 != 0);
  // nop                        |  nop                            284

  if (bc) {
    goto L17;
  }

  // BRANCH!
  // ibltz vi11, L17            |  nop                            285
  bc = ((s16)vu.vi11) < 0;
  // nop                        |  nop                            286

  if (bc) {
    goto L17;
  }

  // iaddi vi14, vi14, 0x1      |  nop                            287
  vu.vi14 = vu.vi14 + 1;
L17:
  // nop                        |  mulw.y vf28, vf28, vf28        288
  vu.vf28.mul(Mask::y, vu.vf28, vu.vf28.w());
  // nop                        |  mulaz.w ACC, vf00, vf07        289
  vu.acc.mula(Mask::w, vu.vf00, vu.vf07.z());
  // nop                        |  msubx.w vf07, vf22, vf07       290
  vu.acc.msub(Mask::w, vu.vf07, vu.vf22, vu.vf07.x());
  // nop                        |  ftoi4.xyzw vf17, vf17          291
  vu.vf17.ftoi4(Mask::xyzw, vu.vf17);
  // lq.xyzw vf24, 290(vi05)    |  mul.xyzw vf26, vf26, Q         292
  vu.vf26.mul(Mask::xyzw, vu.vf26, vu.Q);
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi05 + 290);
  // sq.xyzw vf21, 64(vi08)     |  mulax.xyzw ACC, vf12, vf28     293
  vu.acc.mula(Mask::xyzw, vu.vf12, vu.vf28.x());
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi08 + 64);
  // sq.xyzw vf23, 4(vi08)      |  madday.xyzw ACC, vf13, vf28    294
  vu.acc.madda(Mask::xyzw, vu.vf13, vu.vf28.y());
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi08 + 4);
  // sq.xyzw vf17, 5(vi08)      |  maddaz.xyzw ACC, vf14, vf28    295
  vu.acc.madda(Mask::xyzw, vu.vf14, vu.vf28.z());
  sq_buffer(Mask::xyzw, vu.vf17, vu.vi08 + 5);
  // sq.xyzw vf19, 66(vi08)     |  maddw.xyzw vf30, vf15, vf00    296
  vu.acc.madd(Mask::xyzw, vu.vf30, vu.vf15, vu.vf00.w());
  sq_buffer(Mask::xyzw, vu.vf19, vu.vi08 + 66);
  // sq.xyzw vf26, 3(vi08)      |  mulax.xyzw ACC, vf08, vf24     297
  vu.acc.mula(Mask::xyzw, vu.vf08, vu.vf24.x());
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi08 + 3);
  // nop                        |  madday.xyzw ACC, vf09, vf24    298
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf24.y());
  // nop                        |  maddaz.xyzw ACC, vf10, vf24    299
  vu.acc.madda(Mask::xyzw, vu.vf10, vu.vf24.z());
  // nop                        |  maddw.xyzw vf26, vf11, vf00    300
  vu.acc.madd(Mask::xyzw, vu.vf26, vu.vf11, vu.vf00.w());
  // div Q, vf03.x, vf30.w      |  mul.xyzw vf18, vf30, vf01      301
  vu.vf18.mul(Mask::xyzw, vu.vf30, vu.vf01);
  vu.Q = vu.vf03.x() / vu.vf30.w();
  // lq.xyzw vf23, 313(vi05)    |  mulw.xyz vf21, vf27, vf00      302
  vu.vf21.mul(Mask::xyz, vu.vf27, vu.vf00.w());
  lq_buffer(Mask::xyzw, vu.vf23, vu.vi05 + 313);
  // mfp.w vf21, P              |  mulw.w vf22, vf22, vf06        303
  vu.vf22.mul(Mask::w, vu.vf22, vu.vf06.w());
  vu.vf21.mfp(Mask::w, vu.P);
  // eleng.xyz P, vf26          |  mulw.xyz vf22, vf22, vf07      304
  vu.vf22.mul(Mask::xyz, vu.vf22, vu.vf07.w());
  vu.P = eleng(vu.vf26);
  // iaddi vi08, vi08, 0x6      |  clipw.xyz vf18, vf18           305
  cf = clip(vu.vf18, vu.vf18.w(), cf);
  vu.vi08 = vu.vi08 + 6;
  // nop                        |  mulaw.w ACC, vf30, vf00        306
  vu.acc.mula(Mask::w, vu.vf30, vu.vf00.w());
  // nop                        |  mulw.w vf23, vf21, vf05        307
  vu.vf23.mul(Mask::w, vu.vf21, vu.vf05.w());
  // iand vi13, vi10, vi12      |  mula.xyz ACC, vf30, Q          308
  vu.acc.mula(Mask::xyz, vu.vf30, vu.Q);
  vu.vi13 = vu.vi10 & vu.vi12;
  // fcand vi01, 0x3ffff        |  maddw.xyzw vf16, vf02, vf00    309
  vu.acc.madd(Mask::xyzw, vu.vf16, vu.vf02, vu.vf00.w());
  fcand(vu.vi01, 0x3ffff, cf);

  // iadd vi12, vi12, vi12      |  miniy.xyzw vf22, vf22, vf07    310
  vu.vf22.mini(Mask::xyzw, vu.vf22, vu.vf07.y());
  vu.vi12 = vu.vi12 + vu.vi12;
  // lq.xyzw vf29, 314(vi05)    |  miniw.w vf23, vf23, vf00       311
  vu.vf23.mini(Mask::w, vu.vf23, vu.vf00.w());
  lq_buffer(Mask::xyzw, vu.vf29, vu.vi05 + 314);
  // nop                        |  nop                            312

  // ior vi01, vi01, vi13       |  miniz.w vf16, vf16, vf03       313
  vu.vf16.mini(Mask::w, vu.vf16, vu.vf03.z());
  vu.vi01 = vu.vi01 | vu.vi13;
  // nop                        |  ftoi0.xyzw vf22, vf22          314
  vu.vf22.ftoi0(Mask::xyzw, vu.vf22);
  // nop                        |  subw.w vf29, vf00, vf23        315
  vu.vf29.sub(Mask::w, vu.vf00, vu.vf23.w());
  // nop                        |  maxx.w vf23, vf23, vf05        316
  vu.vf23.max(Mask::w, vu.vf23, vu.vf05.x());
  // nop                        |  maxy.w vf16, vf16, vf03        317
  vu.vf16.max(Mask::w, vu.vf16, vu.vf03.y());
  // nop                        |  nop                            318

L18:
  // BRANCH!
  // ibeq vi00, vi01, L19       |  nop                            319
  bc = (vu.vi01 == 0);
  // lq.xyzw vf27, 285(vi05)    |  nop                            320
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi05 + 285);
  if (bc) {
    goto L19;
  }

  // BRANCH!
  // ibne vi00, vi13, L19       |  addw.w vf16, vf16, vf03        321
  vu.vf16.add(Mask::w, vu.vf16, vu.vf03.w());
  bc = (vu.vi13 != 0);
  // fcor vi01, 0xfdf7df        |  nop                            322
  fcor(vu.vi01, 0xfdf7df, cf);

  if (bc) {
    goto L19;
  }

  // isw.x vi00, 988(vi14)      |  nop                            323
  isw_buffer(Mask::x, 0, vu.vi14 + 988);
  // BRANCH!
  // ibne vi00, vi01, L19       |  nop                            324
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xff7df7        |  nop                            325
  fcor(vu.vi01, 0xff7df7, cf);

  if (bc) {
    goto L19;
  }

  // isw.y vi05, 988(vi14)      |  nop                            326
  isw_buffer(Mask::y, vu.vi05, vu.vi14 + 988);
  // BRANCH!
  // ibne vi00, vi01, L19       |  nop                            327
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xffbefb        |  nop                            328
  fcor(vu.vi01, 0xffbefb, cf);

  if (bc) {
    goto L19;
  }

  // isw.z vi08, 988(vi14)      |  nop                            329
  isw_buffer(Mask::z, vu.vi08, vu.vi14 + 988);
  // BRANCH!
  // ibne vi00, vi01, L19       |  nop                            330
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xffdf7d        |  nop                            331
  fcor(vu.vi01, 0xffdf7d, cf);

  if (bc) {
    goto L19;
  }

  // nop                        |  nop                            332

  // BRANCH!
  // ibne vi00, vi01, L19       |  nop                            333
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xffefbe        |  nop                            334
  fcor(vu.vi01, 0xffefbe, cf);

  if (bc) {
    goto L19;
  }

  // nop                        |  nop                            335

  // BRANCH!
  // ibne vi00, vi01, L19       |  nop                            336
  bc = (vu.vi01 != 0);
  // nop                        |  nop                            337

  if (bc) {
    goto L19;
  }

  // iaddi vi14, vi14, 0x1      |  nop                            338
  vu.vi14 = vu.vi14 + 1;
L19:
  // nop                        |  mulw.y vf29, vf29, vf29        339
  vu.vf29.mul(Mask::y, vu.vf29, vu.vf29.w());
  // nop                        |  mulaz.w ACC, vf00, vf07        340
  vu.acc.mula(Mask::w, vu.vf00, vu.vf07.z());
  // nop                        |  msubx.w vf07, vf23, vf07       341
  vu.acc.msub(Mask::w, vu.vf07, vu.vf23, vu.vf07.x());
  // nop                        |  ftoi4.xyzw vf16, vf16          342
  vu.vf16.ftoi4(Mask::xyzw, vu.vf16);
  // lq.xyzw vf25, 317(vi05)    |  mul.xyzw vf27, vf27, Q         343
  vu.vf27.mul(Mask::xyzw, vu.vf27, vu.Q);
  lq_buffer(Mask::xyzw, vu.vf25, vu.vi05 + 317);
  // sq.xyzw vf20, 61(vi08)     |  mulax.xyzw ACC, vf12, vf29     344
  vu.acc.mula(Mask::xyzw, vu.vf12, vu.vf29.x());
  sq_buffer(Mask::xyzw, vu.vf20, vu.vi08 + 61);
  // sq.xyzw vf22, 1(vi08)      |  madday.xyzw ACC, vf13, vf29    345
  vu.acc.madda(Mask::xyzw, vu.vf13, vu.vf29.y());
  sq_buffer(Mask::xyzw, vu.vf22, vu.vi08 + 1);
  // sq.xyzw vf16, 2(vi08)      |  maddaz.xyzw ACC, vf14, vf29    346
  vu.acc.madda(Mask::xyzw, vu.vf14, vu.vf29.z());
  sq_buffer(Mask::xyzw, vu.vf16, vu.vi08 + 2);
  // sq.xyzw vf18, 63(vi08)     |  maddw.xyzw vf31, vf15, vf00    347
  vu.acc.madd(Mask::xyzw, vu.vf31, vu.vf15, vu.vf00.w());
  sq_buffer(Mask::xyzw, vu.vf18, vu.vi08 + 63);
  // sq.xyzw vf27, 0(vi08)      |  mulax.xyzw ACC, vf08, vf25     348
  vu.acc.mula(Mask::xyzw, vu.vf08, vu.vf25.x());
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi08);
  // nop                        |  madday.xyzw ACC, vf09, vf25    349
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf25.y());
  // iaddi vi05, vi05, 0x3      |  maddaz.xyzw ACC, vf10, vf25    350
  vu.acc.madda(Mask::xyzw, vu.vf10, vu.vf25.z());
  vu.vi05 = vu.vi05 + 3;
  // nop                        |  maddw.xyzw vf27, vf11, vf00    351
  vu.acc.madd(Mask::xyzw, vu.vf27, vu.vf11, vu.vf00.w());
  // div Q, vf03.x, vf31.w      |  mul.xyzw vf19, vf31, vf01      352
  vu.vf19.mul(Mask::xyzw, vu.vf31, vu.vf01);
  vu.Q = vu.vf03.x() / vu.vf31.w();
  // lq.xyzw vf22, 286(vi05)    |  mulw.xyz vf20, vf26, vf00      353
  vu.vf20.mul(Mask::xyz, vu.vf26, vu.vf00.w());
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi05 + 286);
  // mfp.w vf20, P              |  mulw.w vf23, vf23, vf06        354
  vu.vf23.mul(Mask::w, vu.vf23, vu.vf06.w());
  vu.vf20.mfp(Mask::w, vu.P);
  // eleng.xyz P, vf27          |  mulw.xyz vf23, vf23, vf07      355
  vu.vf23.mul(Mask::xyz, vu.vf23, vu.vf07.w());
  vu.P = eleng(vu.vf27);
  // nop                        |  clipw.xyz vf19, vf19           356
  cf = clip(vu.vf19, vu.vf19.w(), cf);
  // nop                        |  mulaw.w ACC, vf31, vf00        357
  vu.acc.mula(Mask::w, vu.vf31, vu.vf00.w());
  // nop                        |  mulw.w vf22, vf20, vf05        358
  vu.vf22.mul(Mask::w, vu.vf20, vu.vf05.w());
  // nop                        |  mula.xyz ACC, vf31, Q          359
  vu.acc.mula(Mask::xyz, vu.vf31, vu.Q);
  // fcand vi01, 0x3ffff        |  maddw.xyzw vf17, vf02, vf00    360
  vu.acc.madd(Mask::xyzw, vu.vf17, vu.vf02, vu.vf00.w());
  fcand(vu.vi01, 0x3ffff, cf);

  // nop                        |  miniy.xyzw vf23, vf23, vf07    361
  vu.vf23.mini(Mask::xyzw, vu.vf23, vu.vf07.y());
  // lq.xyzw vf28, 287(vi05)    |  miniw.w vf22, vf22, vf00       362
  vu.vf22.mini(Mask::w, vu.vf22, vu.vf00.w());
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi05 + 287);
  // nop                        |  nop                            363

  // ior vi01, vi01, vi13       |  miniz.w vf17, vf17, vf03       364
  vu.vf17.mini(Mask::w, vu.vf17, vu.vf03.z());
  vu.vi01 = vu.vi01 | vu.vi13;
  // nop                        |  ftoi0.xyzw vf23, vf23          365
  vu.vf23.ftoi0(Mask::xyzw, vu.vf23);
  // nop                        |  subw.w vf28, vf00, vf22        366
  vu.vf28.sub(Mask::w, vu.vf00, vu.vf22.w());
  // nop                        |  maxx.w vf22, vf22, vf05        367
  vu.vf22.max(Mask::w, vu.vf22, vu.vf05.x());
  // BRANCH!
  // ibgtz vi06, L16            |  maxy.w vf17, vf17, vf03        368
  vu.vf17.max(Mask::w, vu.vf17, vu.vf03.y());
  bc = ((s16)vu.vi06) > 0;
  // iaddi vi06, vi06, -0x1     |  nop                            369
  vu.vi06 = vu.vi06 + -1;
  if (bc) {
    goto L16;
  }

  // BRANCH!
  // ibeq vi00, vi01, L20       |  nop                            370
  bc = (vu.vi01 == 0);
  // lq.xyzw vf26, 309(vi05)    |  nop                            371
  lq_buffer(Mask::xyzw, vu.vf26, vu.vi05 + 309);
  if (bc) {
    goto L20;
  }

  // BRANCH!
  // ibne vi00, vi13, L20       |  addw.w vf17, vf17, vf03        372
  vu.vf17.add(Mask::w, vu.vf17, vu.vf03.w());
  bc = (vu.vi13 != 0);
  // fcor vi01, 0xfdf7df        |  nop                            373
  fcor(vu.vi01, 0xfdf7df, cf);

  if (bc) {
    goto L20;
  }

  // isw.x vi12, 988(vi14)      |  nop                            374
  isw_buffer(Mask::x, vu.vi12, vu.vi14 + 988);
  // BRANCH!
  // ibne vi00, vi01, L20       |  nop                            375
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xff7df7        |  nop                            376
  fcor(vu.vi01, 0xff7df7, cf);

  if (bc) {
    goto L20;
  }

  // isw.y vi05, 988(vi14)      |  nop                            377
  isw_buffer(Mask::y, vu.vi05, vu.vi14 + 988);
  // BRANCH!
  // ibne vi00, vi01, L20       |  nop                            378
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xffbefb        |  nop                            379
  fcor(vu.vi01, 0xffbefb, cf);

  if (bc) {
    goto L20;
  }

  // isw.z vi08, 988(vi14)      |  nop                            380
  isw_buffer(Mask::z, vu.vi08, vu.vi14 + 988);
  // BRANCH!
  // ibne vi00, vi01, L20       |  nop                            381
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xffdf7d        |  nop                            382
  fcor(vu.vi01, 0xffdf7d, cf);

  if (bc) {
    goto L20;
  }

  // nop                        |  nop                            383

  // BRANCH!
  // ibne vi00, vi01, L20       |  nop                            384
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xffefbe        |  nop                            385
  fcor(vu.vi01, 0xffefbe, cf);

  if (bc) {
    goto L20;
  }

  // nop                        |  nop                            386

  // BRANCH!
  // ibne vi00, vi01, L20       |  nop                            387
  bc = (vu.vi01 != 0);
  // nop                        |  nop                            388

  if (bc) {
    goto L20;
  }

  // iaddi vi14, vi14, 0x1      |  nop                            389
  vu.vi14 = vu.vi14 + 1;
L20:
  // isw.y vi14, 987(vi00)      |  ftoi4.xyzw vf17, vf17          390
  vu.vf17.ftoi4(Mask::xyzw, vu.vf17);
  isw_buffer(Mask::y, vu.vi14, 987);
  // sq.xyzw vf21, 64(vi08)     |  mul.xyzw vf26, vf26, Q         391
  vu.vf26.mul(Mask::xyzw, vu.vf26, vu.Q);
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi08 + 64);
  // sq.xyzw vf23, 4(vi08)      |  nop                            392
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi08 + 4);
  // sq.xyzw vf17, 5(vi08)      |  nop                            393
  sq_buffer(Mask::xyzw, vu.vf17, vu.vi08 + 5);
  // sq.xyzw vf19, 66(vi08)     |  nop                            394
  sq_buffer(Mask::xyzw, vu.vf19, vu.vi08 + 66);
  // sq.xyzw vf26, 3(vi08)      |  nop                            395
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi08 + 3);
  // jr vi15                    |  nop                            396
  // nop                        |  nop                            397
}

void OceanNear::run_L15_vu2c_jak2() {
  u32 cf;
  bool bc;
  // iaddi vi01, vi05, 0x9      |  nop                            207
  vu.vi01 = vu.vi05 + 9;
  // lq.xyzw vf24, 290(vi05)    |  nop                            208
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi05 + 290);
  // lq.xyzw vf25, 317(vi05)    |  nop                            209
  lq_buffer(Mask::xyzw, vu.vf25, vu.vi05 + 317);
  // lq.xyzw vf05, 959(vi00)    |  mulax.xyzw ACC, vf08, vf24     210
  vu.acc.mula(Mask::xyzw, vu.vf08, vu.vf24.x());
  lq_buffer(Mask::xyzw, vu.vf05, 959);
  // iaddi vi05, vi05, 0x3      |  madday.xyzw ACC, vf09, vf24    211
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf24.y());
  vu.vi05 = vu.vi05 + 3;
  // isw.x vi01, 988(vi00)      |  nop                            212
  isw_buffer(Mask::x, vu.vi01, 988);
  // iaddi vi01, vi00, 0x0      |  maddaz.xyzw ACC, vf10, vf24    213
  vu.acc.madda(Mask::xyzw, vu.vf10, vu.vf24.z());
  vu.vi01 = 0;
  // iaddi vi13, vi00, 0x0      |  maddw.xyzw vf26, vf11, vf00    214
  vu.acc.madd(Mask::xyzw, vu.vf26, vu.vf11, vu.vf00.w());
  vu.vi13 = 0;
  // iaddi vi11, vi00, 0x0      |  mulax.xyzw ACC, vf08, vf25     215
  vu.acc.mula(Mask::xyzw, vu.vf08, vu.vf25.x());
  vu.vi11 = 0;
  // fcset 0x0                  |  madday.xyzw ACC, vf09, vf25    216
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf25.y());
  cf = 0x0;

  // lq.xyzw vf28, 287(vi05)    |  maddaz.xyzw ACC, vf10, vf25    217
  vu.acc.madda(Mask::xyzw, vu.vf10, vu.vf25.z());
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi05 + 287);
  // eleng.xyz P, vf26          |  maddw.xyzw vf27, vf11, vf00    218
  vu.acc.madd(Mask::xyzw, vu.vf27, vu.vf11, vu.vf00.w());
  vu.P = eleng(vu.vf26);
  // iaddi vi14, vi00, 0x0      |  mulw.xyzw vf20, vf26, vf00     219
  vu.vf20.mul(Mask::xyzw, vu.vf26, vu.vf00.w());
  vu.vi14 = 0;
  // lq.xyzw vf22, 286(vi05)    |  nop                            220
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi05 + 286);
  // waitp                      |  nop                            221
  // ASSERT(false);
  // mfp.w vf20, P              |  nop                            222
  vu.vf20.mfp(Mask::w, vu.P);
  // eleng.xyz P, vf27          |  mulw.xyzw vf21, vf27, vf00     223
  vu.vf21.mul(Mask::xyzw, vu.vf27, vu.vf00.w());
  vu.P = eleng(vu.vf27);
  // lq.xyzw vf07, 958(vi00)    |  nop                            224
  lq_buffer(Mask::xyzw, vu.vf07, 958);
  // nop                        |  nop                            225

  // iaddi vi12, vi00, 0x1      |  mulw.w vf22, vf20, vf05        226
  vu.vf22.mul(Mask::w, vu.vf20, vu.vf05.w());
  vu.vi12 = 1;
  // nop                        |  nop                            227

  // nop                        |  nop                            228

  // nop                        |  nop                            229

  // nop                        |  miniw.w vf22, vf22, vf00       230
  vu.vf22.mini(Mask::w, vu.vf22, vu.vf00.w());
  // nop                        |  nop                            231

  // nop                        |  nop                            232

  // nop                        |  nop                            233

  // nop                        |  subw.w vf28, vf00, vf22        234
  vu.vf28.sub(Mask::w, vu.vf00, vu.vf22.w());
  // nop                        |  maxx.w vf22, vf22, vf05        235
  vu.vf22.max(Mask::w, vu.vf22, vu.vf05.x());
  // nop                        |  nop                            236

  // nop                        |  mulaz.w ACC, vf00, vf07        237
  vu.acc.mula(Mask::w, vu.vf00, vu.vf07.z());
  // nop                        |  mulw.y vf28, vf28, vf28        238
  vu.vf28.mul(Mask::y, vu.vf28, vu.vf28.w());
  // lq.xyzw vf24, 290(vi05)    |  msubx.w vf07, vf22, vf07       239
  vu.acc.msub(Mask::w, vu.vf07, vu.vf22, vu.vf07.x());
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi05 + 290);
  // nop                        |  mulax.xyzw ACC, vf12, vf28     240
  vu.acc.mula(Mask::xyzw, vu.vf12, vu.vf28.x());
  // nop                        |  madday.xyzw ACC, vf13, vf28    241
  vu.acc.madda(Mask::xyzw, vu.vf13, vu.vf28.y());
  // nop                        |  maddaz.xyzw ACC, vf14, vf28    242
  vu.acc.madda(Mask::xyzw, vu.vf14, vu.vf28.z());
  // nop                        |  maddw.xyzw vf30, vf15, vf00    243
  vu.acc.madd(Mask::xyzw, vu.vf30, vu.vf15, vu.vf00.w());
  // nop                        |  mulax.xyzw ACC, vf08, vf24     244
  vu.acc.mula(Mask::xyzw, vu.vf08, vu.vf24.x());
  // nop                        |  madday.xyzw ACC, vf09, vf24    245
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf24.y());
  // nop                        |  maddaz.xyzw ACC, vf10, vf24    246
  vu.acc.madda(Mask::xyzw, vu.vf10, vu.vf24.z());
  // nop                        |  maddw.xyzw vf26, vf11, vf00    247
  vu.acc.madd(Mask::xyzw, vu.vf26, vu.vf11, vu.vf00.w());
  // div Q, vf03.x, vf30.w      |  mul.xyzw vf18, vf30, vf01      248
  vu.vf18.mul(Mask::xyzw, vu.vf30, vu.vf01);
  vu.Q = vu.vf03.x() / vu.vf30.w();
  // lq.xyzw vf23, 313(vi05)    |  mulw.w vf22, vf22, vf06        249
  vu.vf22.mul(Mask::w, vu.vf22, vu.vf06.w());
  lq_buffer(Mask::xyzw, vu.vf23, vu.vi05 + 313);
  // waitp                      |  mulw.xyz vf22, vf22, vf07      250
  vu.vf22.mul(Mask::xyz, vu.vf22, vu.vf07.w());
  // mfp.w vf21, P              |  nop                            251
  vu.vf21.mfp(Mask::w, vu.P);
  // eleng.xyz P, vf26          |  nop                            252
  vu.P = eleng(vu.vf26);
  // nop                        |  clipw.xyz vf18, vf18           253
  cf = clip(vu.vf18, vu.vf18.w(), cf);
  // nop                        |  mulaw.w ACC, vf30, vf00        254
  vu.acc.mula(Mask::w, vu.vf30, vu.vf00.w());
  // lq.xyzw vf29, 314(vi05)    |  mulw.w vf23, vf21, vf05        255
  vu.vf23.mul(Mask::w, vu.vf21, vu.vf05.w());
  lq_buffer(Mask::xyzw, vu.vf29, vu.vi05 + 314);
  // nop                        |  mula.xyz ACC, vf30, Q          256
  vu.acc.mula(Mask::xyz, vu.vf30, vu.Q);
  // nop                        |  maddw.xyzw vf16, vf02, vf00    257
  vu.acc.madd(Mask::xyzw, vu.vf16, vu.vf02, vu.vf00.w());
  // nop                        |  miniy.xyzw vf22, vf22, vf07    258
  vu.vf22.mini(Mask::xyzw, vu.vf22, vu.vf07.y());
  // nop                        |  miniw.w vf23, vf23, vf00       259
  vu.vf23.mini(Mask::w, vu.vf23, vu.vf00.w());
  // nop                        |  nop                            260

  // nop                        |  miniz.w vf16, vf16, vf03       261
  vu.vf16.mini(Mask::w, vu.vf16, vu.vf03.z());
  // nop                        |  ftoi0.xyzw vf22, vf22          262
  vu.vf22.ftoi0(Mask::xyzw, vu.vf22);
  // waitp                      |  subw.w vf29, vf00, vf23        263
  vu.vf29.sub(Mask::w, vu.vf00, vu.vf23.w());
  // BRANCH!
  // b L18                      |  maxx.w vf23, vf23, vf05        264
  vu.vf23.max(Mask::w, vu.vf23, vu.vf05.x());
  bc = true;
  // iaddi vi06, vi00, 0x8      |  maxy.w vf16, vf16, vf03        265
  vu.vf16.max(Mask::w, vu.vf16, vu.vf03.y());
  vu.vi06 = 8;
  if (bc) {
    goto L18;
  }

L16:
  // BRANCH!
  // ibeq vi00, vi01, L17       |  nop                            266
  bc = (vu.vi01 == 0);
  // lq.xyzw vf26, 309(vi05)    |  nop                            267
  lq_buffer(Mask::xyzw, vu.vf26, vu.vi05 + 309);
  if (bc) {
    goto L17;
  }

  // BRANCH!
  // ibne vi00, vi13, L17       |  addw.w vf17, vf17, vf03        268
  vu.vf17.add(Mask::w, vu.vf17, vu.vf03.w());
  bc = (vu.vi13 != 0);
  // fcor vi01, 0xfdf7df        |  nop                            269
  fcor(vu.vi01, 0xfdf7df, cf);

  if (bc) {
    goto L17;
  }

  // isw.x vi12, 989(vi14)      |  nop                            270
  isw_buffer(Mask::x, vu.vi12, vu.vi14 + 989);
  // BRANCH!
  // ibne vi00, vi01, L17       |  nop                            271
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xff7df7        |  nop                            272
  fcor(vu.vi01, 0xff7df7, cf);

  if (bc) {
    goto L17;
  }

  // isw.y vi05, 989(vi14)      |  nop                            273
  isw_buffer(Mask::y, vu.vi05, vu.vi14 + 989);
  // BRANCH!
  // ibne vi00, vi01, L17       |  nop                            274
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xffbefb        |  nop                            275
  fcor(vu.vi01, 0xffbefb, cf);

  if (bc) {
    goto L17;
  }

  // ilw.x vi11, 988(vi00)      |  nop                            276
  ilw_buffer(Mask::x, vu.vi11, 988);
  // BRANCH!
  // ibne vi00, vi01, L17       |  nop                            277
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xffdf7d        |  nop                            278
  fcor(vu.vi01, 0xffdf7d, cf);

  if (bc) {
    goto L17;
  }

  // isw.z vi08, 989(vi14)      |  nop                            279
  isw_buffer(Mask::z, vu.vi08, vu.vi14 + 989);
  // BRANCH!
  // ibne vi00, vi01, L17       |  nop                            280
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xffefbe        |  nop                            281
  fcor(vu.vi01, 0xffefbe, cf);

  if (bc) {
    goto L17;
  }

  // isub vi11, vi05, vi11      |  nop                            282
  vu.vi11 = vu.vi05 - vu.vi11;
  // BRANCH!
  // ibne vi00, vi01, L17       |  nop                            283
  bc = (vu.vi01 != 0);
  // nop                        |  nop                            284

  if (bc) {
    goto L17;
  }

  // BRANCH!
  // ibltz vi11, L17            |  nop                            285
  bc = ((s16)vu.vi11) < 0;
  // nop                        |  nop                            286

  if (bc) {
    goto L17;
  }

  // iaddi vi14, vi14, 0x1      |  nop                            287
  vu.vi14 = vu.vi14 + 1;
L17:
  // nop                        |  mulw.y vf28, vf28, vf28        288
  vu.vf28.mul(Mask::y, vu.vf28, vu.vf28.w());
  // nop                        |  mulaz.w ACC, vf00, vf07        289
  vu.acc.mula(Mask::w, vu.vf00, vu.vf07.z());
  // nop                        |  msubx.w vf07, vf22, vf07       290
  vu.acc.msub(Mask::w, vu.vf07, vu.vf22, vu.vf07.x());
  // nop                        |  ftoi4.xyzw vf17, vf17          291
  vu.vf17.ftoi4(Mask::xyzw, vu.vf17);
  // lq.xyzw vf24, 290(vi05)    |  mul.xyzw vf26, vf26, Q         292
  vu.vf26.mul(Mask::xyzw, vu.vf26, vu.Q);
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi05 + 290);
  // sq.xyzw vf21, 64(vi08)     |  mulax.xyzw ACC, vf12, vf28     293
  vu.acc.mula(Mask::xyzw, vu.vf12, vu.vf28.x());
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi08 + 64);
  // sq.xyzw vf23, 4(vi08)      |  madday.xyzw ACC, vf13, vf28    294
  vu.acc.madda(Mask::xyzw, vu.vf13, vu.vf28.y());
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi08 + 4);
  // sq.xyzw vf17, 5(vi08)      |  maddaz.xyzw ACC, vf14, vf28    295
  vu.acc.madda(Mask::xyzw, vu.vf14, vu.vf28.z());
  sq_buffer(Mask::xyzw, vu.vf17, vu.vi08 + 5);
  // sq.xyzw vf19, 66(vi08)     |  maddw.xyzw vf30, vf15, vf00    296
  vu.acc.madd(Mask::xyzw, vu.vf30, vu.vf15, vu.vf00.w());
  sq_buffer(Mask::xyzw, vu.vf19, vu.vi08 + 66);
  // sq.xyzw vf26, 3(vi08)      |  mulax.xyzw ACC, vf08, vf24     297
  vu.acc.mula(Mask::xyzw, vu.vf08, vu.vf24.x());
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi08 + 3);
  // nop                        |  madday.xyzw ACC, vf09, vf24    298
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf24.y());
  // nop                        |  maddaz.xyzw ACC, vf10, vf24    299
  vu.acc.madda(Mask::xyzw, vu.vf10, vu.vf24.z());
  // nop                        |  maddw.xyzw vf26, vf11, vf00    300
  vu.acc.madd(Mask::xyzw, vu.vf26, vu.vf11, vu.vf00.w());
  // div Q, vf03.x, vf30.w      |  mul.xyzw vf18, vf30, vf01      301
  vu.vf18.mul(Mask::xyzw, vu.vf30, vu.vf01);
  vu.Q = vu.vf03.x() / vu.vf30.w();
  // lq.xyzw vf23, 313(vi05)    |  mulw.xyz vf21, vf27, vf00      302
  vu.vf21.mul(Mask::xyz, vu.vf27, vu.vf00.w());
  lq_buffer(Mask::xyzw, vu.vf23, vu.vi05 + 313);
  // mfp.w vf21, P              |  mulw.w vf22, vf22, vf06        303
  vu.vf22.mul(Mask::w, vu.vf22, vu.vf06.w());
  vu.vf21.mfp(Mask::w, vu.P);
  // eleng.xyz P, vf26          |  mulw.xyz vf22, vf22, vf07      304
  vu.vf22.mul(Mask::xyz, vu.vf22, vu.vf07.w());
  vu.P = eleng(vu.vf26);
  // iaddi vi08, vi08, 0x6      |  clipw.xyz vf18, vf18           305
  cf = clip(vu.vf18, vu.vf18.w(), cf);
  vu.vi08 = vu.vi08 + 6;
  // nop                        |  mulaw.w ACC, vf30, vf00        306
  vu.acc.mula(Mask::w, vu.vf30, vu.vf00.w());
  // nop                        |  mulw.w vf23, vf21, vf05        307
  vu.vf23.mul(Mask::w, vu.vf21, vu.vf05.w());
  // iand vi13, vi10, vi12      |  mula.xyz ACC, vf30, Q          308
  vu.acc.mula(Mask::xyz, vu.vf30, vu.Q);
  vu.vi13 = vu.vi10 & vu.vi12;
  // fcand vi01, 0x3ffff        |  maddw.xyzw vf16, vf02, vf00    309
  vu.acc.madd(Mask::xyzw, vu.vf16, vu.vf02, vu.vf00.w());
  fcand(vu.vi01, 0x3ffff, cf);

  // iadd vi12, vi12, vi12      |  miniy.xyzw vf22, vf22, vf07    310
  vu.vf22.mini(Mask::xyzw, vu.vf22, vu.vf07.y());
  vu.vi12 = vu.vi12 + vu.vi12;
  // lq.xyzw vf29, 314(vi05)    |  miniw.w vf23, vf23, vf00       311
  vu.vf23.mini(Mask::w, vu.vf23, vu.vf00.w());
  lq_buffer(Mask::xyzw, vu.vf29, vu.vi05 + 314);
  // nop                        |  nop                            312

  // ior vi01, vi01, vi13       |  miniz.w vf16, vf16, vf03       313
  vu.vf16.mini(Mask::w, vu.vf16, vu.vf03.z());
  vu.vi01 = vu.vi01 | vu.vi13;
  // nop                        |  ftoi0.xyzw vf22, vf22          314
  vu.vf22.ftoi0(Mask::xyzw, vu.vf22);
  // nop                        |  subw.w vf29, vf00, vf23        315
  vu.vf29.sub(Mask::w, vu.vf00, vu.vf23.w());
  // nop                        |  maxx.w vf23, vf23, vf05        316
  vu.vf23.max(Mask::w, vu.vf23, vu.vf05.x());
  // nop                        |  maxy.w vf16, vf16, vf03        317
  vu.vf16.max(Mask::w, vu.vf16, vu.vf03.y());
  // nop                        |  nop                            318

L18:
  // BRANCH!
  // ibeq vi00, vi01, L19       |  nop                            319
  bc = (vu.vi01 == 0);
  // lq.xyzw vf27, 285(vi05)    |  nop                            320
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi05 + 285);
  if (bc) {
    goto L19;
  }

  // BRANCH!
  // ibne vi00, vi13, L19       |  addw.w vf16, vf16, vf03        321
  vu.vf16.add(Mask::w, vu.vf16, vu.vf03.w());
  bc = (vu.vi13 != 0);
  // fcor vi01, 0xfdf7df        |  nop                            322
  fcor(vu.vi01, 0xfdf7df, cf);

  if (bc) {
    goto L19;
  }

  // isw.x vi00, 989(vi14)      |  nop                            323
  isw_buffer(Mask::x, 0, vu.vi14 + 989);
  // BRANCH!
  // ibne vi00, vi01, L19       |  nop                            324
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xff7df7        |  nop                            325
  fcor(vu.vi01, 0xff7df7, cf);

  if (bc) {
    goto L19;
  }

  // isw.y vi05, 989(vi14)      |  nop                            326
  isw_buffer(Mask::y, vu.vi05, vu.vi14 + 989);
  // BRANCH!
  // ibne vi00, vi01, L19       |  nop                            327
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xffbefb        |  nop                            328
  fcor(vu.vi01, 0xffbefb, cf);

  if (bc) {
    goto L19;
  }

  // isw.z vi08, 989(vi14)      |  nop                            329
  isw_buffer(Mask::z, vu.vi08, vu.vi14 + 989);
  // BRANCH!
  // ibne vi00, vi01, L19       |  nop                            330
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xffdf7d        |  nop                            331
  fcor(vu.vi01, 0xffdf7d, cf);

  if (bc) {
    goto L19;
  }

  // nop                        |  nop                            332

  // BRANCH!
  // ibne vi00, vi01, L19       |  nop                            333
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xffefbe        |  nop                            334
  fcor(vu.vi01, 0xffefbe, cf);

  if (bc) {
    goto L19;
  }

  // nop                        |  nop                            335

  // BRANCH!
  // ibne vi00, vi01, L19       |  nop                            336
  bc = (vu.vi01 != 0);
  // nop                        |  nop                            337

  if (bc) {
    goto L19;
  }

  // iaddi vi14, vi14, 0x1      |  nop                            338
  vu.vi14 = vu.vi14 + 1;
L19:
  // nop                        |  mulw.y vf29, vf29, vf29        339
  vu.vf29.mul(Mask::y, vu.vf29, vu.vf29.w());
  // nop                        |  mulaz.w ACC, vf00, vf07        340
  vu.acc.mula(Mask::w, vu.vf00, vu.vf07.z());
  // nop                        |  msubx.w vf07, vf23, vf07       341
  vu.acc.msub(Mask::w, vu.vf07, vu.vf23, vu.vf07.x());
  // nop                        |  ftoi4.xyzw vf16, vf16          342
  vu.vf16.ftoi4(Mask::xyzw, vu.vf16);
  // lq.xyzw vf25, 317(vi05)    |  mul.xyzw vf27, vf27, Q         343
  vu.vf27.mul(Mask::xyzw, vu.vf27, vu.Q);
  lq_buffer(Mask::xyzw, vu.vf25, vu.vi05 + 317);
  // sq.xyzw vf20, 61(vi08)     |  mulax.xyzw ACC, vf12, vf29     344
  vu.acc.mula(Mask::xyzw, vu.vf12, vu.vf29.x());
  sq_buffer(Mask::xyzw, vu.vf20, vu.vi08 + 61);
  // sq.xyzw vf22, 1(vi08)      |  madday.xyzw ACC, vf13, vf29    345
  vu.acc.madda(Mask::xyzw, vu.vf13, vu.vf29.y());
  sq_buffer(Mask::xyzw, vu.vf22, vu.vi08 + 1);
  // sq.xyzw vf16, 2(vi08)      |  maddaz.xyzw ACC, vf14, vf29    346
  vu.acc.madda(Mask::xyzw, vu.vf14, vu.vf29.z());
  sq_buffer(Mask::xyzw, vu.vf16, vu.vi08 + 2);
  // sq.xyzw vf18, 63(vi08)     |  maddw.xyzw vf31, vf15, vf00    347
  vu.acc.madd(Mask::xyzw, vu.vf31, vu.vf15, vu.vf00.w());
  sq_buffer(Mask::xyzw, vu.vf18, vu.vi08 + 63);
  // sq.xyzw vf27, 0(vi08)      |  mulax.xyzw ACC, vf08, vf25     348
  vu.acc.mula(Mask::xyzw, vu.vf08, vu.vf25.x());
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi08);
  // nop                        |  madday.xyzw ACC, vf09, vf25    349
  vu.acc.madda(Mask::xyzw, vu.vf09, vu.vf25.y());
  // iaddi vi05, vi05, 0x3      |  maddaz.xyzw ACC, vf10, vf25    350
  vu.acc.madda(Mask::xyzw, vu.vf10, vu.vf25.z());
  vu.vi05 = vu.vi05 + 3;
  // nop                        |  maddw.xyzw vf27, vf11, vf00    351
  vu.acc.madd(Mask::xyzw, vu.vf27, vu.vf11, vu.vf00.w());
  // div Q, vf03.x, vf31.w      |  mul.xyzw vf19, vf31, vf01      352
  vu.vf19.mul(Mask::xyzw, vu.vf31, vu.vf01);
  vu.Q = vu.vf03.x() / vu.vf31.w();
  // lq.xyzw vf22, 286(vi05)    |  mulw.xyz vf20, vf26, vf00      353
  vu.vf20.mul(Mask::xyz, vu.vf26, vu.vf00.w());
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi05 + 286);
  // mfp.w vf20, P              |  mulw.w vf23, vf23, vf06        354
  vu.vf23.mul(Mask::w, vu.vf23, vu.vf06.w());
  vu.vf20.mfp(Mask::w, vu.P);
  // eleng.xyz P, vf27          |  mulw.xyz vf23, vf23, vf07      355
  vu.vf23.mul(Mask::xyz, vu.vf23, vu.vf07.w());
  vu.P = eleng(vu.vf27);
  // nop                        |  clipw.xyz vf19, vf19           356
  cf = clip(vu.vf19, vu.vf19.w(), cf);
  // nop                        |  mulaw.w ACC, vf31, vf00        357
  vu.acc.mula(Mask::w, vu.vf31, vu.vf00.w());
  // nop                        |  mulw.w vf22, vf20, vf05        358
  vu.vf22.mul(Mask::w, vu.vf20, vu.vf05.w());
  // nop                        |  mula.xyz ACC, vf31, Q          359
  vu.acc.mula(Mask::xyz, vu.vf31, vu.Q);
  // fcand vi01, 0x3ffff        |  maddw.xyzw vf17, vf02, vf00    360
  vu.acc.madd(Mask::xyzw, vu.vf17, vu.vf02, vu.vf00.w());
  fcand(vu.vi01, 0x3ffff, cf);

  // nop                        |  miniy.xyzw vf23, vf23, vf07    361
  vu.vf23.mini(Mask::xyzw, vu.vf23, vu.vf07.y());
  // lq.xyzw vf28, 287(vi05)    |  miniw.w vf22, vf22, vf00       362
  vu.vf22.mini(Mask::w, vu.vf22, vu.vf00.w());
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi05 + 287);
  // nop                        |  nop                            363

  // ior vi01, vi01, vi13       |  miniz.w vf17, vf17, vf03       364
  vu.vf17.mini(Mask::w, vu.vf17, vu.vf03.z());
  vu.vi01 = vu.vi01 | vu.vi13;
  // nop                        |  ftoi0.xyzw vf23, vf23          365
  vu.vf23.ftoi0(Mask::xyzw, vu.vf23);
  // nop                        |  subw.w vf28, vf00, vf22        366
  vu.vf28.sub(Mask::w, vu.vf00, vu.vf22.w());
  // nop                        |  maxx.w vf22, vf22, vf05        367
  vu.vf22.max(Mask::w, vu.vf22, vu.vf05.x());
  // BRANCH!
  // ibgtz vi06, L16            |  maxy.w vf17, vf17, vf03        368
  vu.vf17.max(Mask::w, vu.vf17, vu.vf03.y());
  bc = ((s16)vu.vi06) > 0;
  // iaddi vi06, vi06, -0x1     |  nop                            369
  vu.vi06 = vu.vi06 + -1;
  if (bc) {
    goto L16;
  }

  // BRANCH!
  // ibeq vi00, vi01, L20       |  nop                            370
  bc = (vu.vi01 == 0);
  // lq.xyzw vf26, 309(vi05)    |  nop                            371
  lq_buffer(Mask::xyzw, vu.vf26, vu.vi05 + 309);
  if (bc) {
    goto L20;
  }

  // BRANCH!
  // ibne vi00, vi13, L20       |  addw.w vf17, vf17, vf03        372
  vu.vf17.add(Mask::w, vu.vf17, vu.vf03.w());
  bc = (vu.vi13 != 0);
  // fcor vi01, 0xfdf7df        |  nop                            373
  fcor(vu.vi01, 0xfdf7df, cf);

  if (bc) {
    goto L20;
  }

  // isw.x vi12, 989(vi14)      |  nop                            374
  isw_buffer(Mask::x, vu.vi12, vu.vi14 + 989);
  // BRANCH!
  // ibne vi00, vi01, L20       |  nop                            375
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xff7df7        |  nop                            376
  fcor(vu.vi01, 0xff7df7, cf);

  if (bc) {
    goto L20;
  }

  // isw.y vi05, 989(vi14)      |  nop                            377
  isw_buffer(Mask::y, vu.vi05, vu.vi14 + 989);
  // BRANCH!
  // ibne vi00, vi01, L20       |  nop                            378
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xffbefb        |  nop                            379
  fcor(vu.vi01, 0xffbefb, cf);

  if (bc) {
    goto L20;
  }

  // isw.z vi08, 989(vi14)      |  nop                            380
  isw_buffer(Mask::z, vu.vi08, vu.vi14 + 989);
  // BRANCH!
  // ibne vi00, vi01, L20       |  nop                            381
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xffdf7d        |  nop                            382
  fcor(vu.vi01, 0xffdf7d, cf);

  if (bc) {
    goto L20;
  }

  // nop                        |  nop                            383

  // BRANCH!
  // ibne vi00, vi01, L20       |  nop                            384
  bc = (vu.vi01 != 0);
  // fcor vi01, 0xffefbe        |  nop                            385
  fcor(vu.vi01, 0xffefbe, cf);

  if (bc) {
    goto L20;
  }

  // nop                        |  nop                            386

  // BRANCH!
  // ibne vi00, vi01, L20       |  nop                            387
  bc = (vu.vi01 != 0);
  // nop                        |  nop                            388

  if (bc) {
    goto L20;
  }

  // iaddi vi14, vi14, 0x1      |  nop                            389
  vu.vi14 = vu.vi14 + 1;
L20:
  // isw.y vi14, 988(vi00)      |  ftoi4.xyzw vf17, vf17          390
  vu.vf17.ftoi4(Mask::xyzw, vu.vf17);
  isw_buffer(Mask::y, vu.vi14, 988);
  // sq.xyzw vf21, 64(vi08)     |  mul.xyzw vf26, vf26, Q         391
  vu.vf26.mul(Mask::xyzw, vu.vf26, vu.Q);
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi08 + 64);
  // sq.xyzw vf23, 4(vi08)      |  nop                            392
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi08 + 4);
  // sq.xyzw vf17, 5(vi08)      |  nop                            393
  sq_buffer(Mask::xyzw, vu.vf17, vu.vi08 + 5);
  // sq.xyzw vf19, 66(vi08)     |  nop                            394
  sq_buffer(Mask::xyzw, vu.vf19, vu.vi08 + 66);
  // sq.xyzw vf26, 3(vi08)      |  nop                            395
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi08 + 3);
  // jr vi15                    |  nop                            396
  // nop                        |  nop                            397
}

void OceanNear::run_L21_vu2c() {
  bool bc;
  // iaddi vi06, vi00, 0x5      |  nop                            398
  vu.vi06 = 5;
L22:
  // lq.xyzw vf20, 0(vi08)      |  nop                            399
  lq_buffer(Mask::xyzw, vu.vf20, vu.vi08);
  // lq.xyzw vf21, 3(vi08)      |  nop                            400
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi08 + 3);
  // lq.xyzw vf18, 6(vi08)      |  nop                            401
  lq_buffer(Mask::xyzw, vu.vf18, vu.vi08 + 6);
  // lq.xyzw vf19, 9(vi08)      |  maxw.xyzw vf11, vf00, vf00     402
  vu.vf11.max(Mask::xyzw, vu.vf00, vu.vf00.w());
  lq_buffer(Mask::xyzw, vu.vf19, vu.vi08 + 9);
  // div Q, vf00.w, vf20.w      |  nop                            403
  vu.Q = vu.vf00.w() / vu.vf20.w();
  // nop                        |  addz.y vf20, vf00, vf20        404
  vu.vf20.add(Mask::y, vu.vf00, vu.vf20.z());
  // nop                        |  addz.y vf21, vf00, vf21        405
  vu.vf21.add(Mask::y, vu.vf00, vu.vf21.z());
  // nop                        |  addz.y vf18, vf00, vf18        406
  vu.vf18.add(Mask::y, vu.vf00, vu.vf18.z());
  // nop                        |  addz.y vf19, vf00, vf19        407
  vu.vf19.add(Mask::y, vu.vf00, vu.vf19.z());
  // nop                        |  nop                            408

  // nop                        |  nop                            409

  // div Q, vf00.w, vf21.w      |  mul.xy vf20, vf20, Q           410
  vu.vf20.mul(Mask::xy, vu.vf20, vu.Q);
  vu.Q = vu.vf00.w() / vu.vf21.w();
  // nop                        |  nop                            411

  // nop                        |  nop                            412

  // nop                        |  nop                            413

  // nop                        |  nop                            414

  // nop                        |  nop                            415

  // nop                        |  nop                            416

  // div Q, vf00.w, vf18.w      |  mul.xy vf21, vf21, Q           417
  vu.vf21.mul(Mask::xy, vu.vf21, vu.Q);
  vu.Q = vu.vf00.w() / vu.vf18.w();
  // nop                        |  mula.xyzw ACC, vf20, vf05      418
  vu.acc.mula(Mask::xyzw, vu.vf20, vu.vf05);
  // nop                        |  maddw.xyzw vf20, vf06, vf00    419
  vu.acc.madd(Mask::xyzw, vu.vf20, vu.vf06, vu.vf00.w());
  // nop                        |  nop                            420

  // nop                        |  mula.xyzw ACC, vf21, vf05      421
  vu.acc.mula(Mask::xyzw, vu.vf21, vu.vf05);
  // nop                        |  maddw.xyzw vf21, vf06, vf00    422
  vu.acc.madd(Mask::xyzw, vu.vf21, vu.vf06, vu.vf00.w());
  // nop                        |  nop                            423

  // div Q, vf00.w, vf19.w      |  mul.xy vf18, vf18, Q           424
  vu.vf18.mul(Mask::xy, vu.vf18, vu.Q);
  vu.Q = vu.vf00.w() / vu.vf19.w();
  // nop                        |  nop                            425

  // nop                        |  nop                            426

  // nop                        |  nop                            427

  // nop                        |  nop                            428

  // nop                        |  nop                            429

  // nop                        |  nop                            430

  // sq.xyz vf20, 0(vi08)       |  mul.xy vf19, vf19, Q           431
  vu.vf19.mul(Mask::xy, vu.vf19, vu.Q);
  sq_buffer(Mask::xyz, vu.vf20, vu.vi08);
  // sq.xyz vf21, 3(vi08)       |  mula.xyzw ACC, vf18, vf05      432
  vu.acc.mula(Mask::xyzw, vu.vf18, vu.vf05);
  sq_buffer(Mask::xyz, vu.vf21, vu.vi08 + 3);
  // nop                        |  maddw.xyzw vf18, vf06, vf00    433
  vu.acc.madd(Mask::xyzw, vu.vf18, vu.vf06, vu.vf00.w());
  // nop                        |  nop                            434

  // nop                        |  mula.xyzw ACC, vf19, vf05      435
  vu.acc.mula(Mask::xyzw, vu.vf19, vu.vf05);
  // iaddi vi08, vi08, 0xc      |  maddw.xyzw vf19, vf06, vf00    436
  vu.acc.madd(Mask::xyzw, vu.vf19, vu.vf06, vu.vf00.w());
  vu.vi08 = vu.vi08 + 12;
  // iaddi vi06, vi06, -0x1     |  nop                            437
  vu.vi06 = vu.vi06 + -1;
  // sq.xyz vf18, -6(vi08)      |  nop                            438
  sq_buffer(Mask::xyz, vu.vf18, vu.vi08 + -6);
  // BRANCH!
  // ibgtz vi06, L22            |  nop                            439
  bc = ((s16)vu.vi06) > 0;
  // sq.xyz vf19, -3(vi08)      |  nop                            440
  sq_buffer(Mask::xyz, vu.vf19, vu.vi08 + -3);
  if (bc) {
    goto L22;
  }

  // jr vi15                    |  nop                            441
  // nop                        |  nop                            442
}

void OceanNear::run_L21_vu2c_jak2() {
  bool bc;
  // lq.xyzw vf05, 955(vi00)    |  nop                            398
  lq_buffer(Mask::xyzw, vu.vf05, 955);
  // iaddi vi06, vi00, 0x5      |  nop                            398
  vu.vi06 = 5;
L22:
  // lq.xyzw vf20, 0(vi08)      |  nop                            399
  lq_buffer(Mask::xyzw, vu.vf20, vu.vi08);
  // lq.xyzw vf21, 3(vi08)      |  nop                            400
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi08 + 3);
  // lq.xyzw vf18, 6(vi08)      |  nop                            401
  lq_buffer(Mask::xyzw, vu.vf18, vu.vi08 + 6);
  // lq.xyzw vf19, 9(vi08)      |  maxw.xyzw vf11, vf00, vf00     402
  vu.vf11.max(Mask::xyzw, vu.vf00, vu.vf00.w());
  lq_buffer(Mask::xyzw, vu.vf19, vu.vi08 + 9);
  // div Q, vf00.w, vf20.w      |  nop                            403
  vu.Q = vu.vf00.w() / vu.vf20.w();
  // nop                        |  nop                            404

  // nop                        |  nop                            405

  // nop                        |  nop                            406

  // nop                        |  nop                            407

  // nop                        |  nop                            408

  // nop                        |  nop                            409

  // div Q, vf00.w, vf21.w      |  mul.xy vf20, vf20, Q           410
  vu.vf20.mul(Mask::xy, vu.vf20, vu.Q);
  vu.Q = vu.vf00.w() / vu.vf21.w();
  // nop                        |  nop                            411

  // nop                        |  nop                            412

  // nop                        |  nop                            413

  // nop                        |  nop                            414

  // nop                        |  nop                            415

  // nop                        |  nop                            416

  // div Q, vf00.w, vf18.w      |  mul.xy vf21, vf21, Q           417
  vu.vf21.mul(Mask::xy, vu.vf21, vu.Q);
  vu.Q = vu.vf00.w() / vu.vf18.w();
  // nop                        |  mula.xyzw ACC, vf20, vf05      418
  vu.acc.mula(Mask::xyzw, vu.vf20, vu.vf05);
  // nop                        |  maddw.xyzw vf20, vf06, vf00    419
  vu.acc.madd(Mask::xyzw, vu.vf20, vu.vf06, vu.vf00.w());
  // nop                        |  nop                            420

  // nop                        |  mula.xyzw ACC, vf21, vf05      421
  vu.acc.mula(Mask::xyzw, vu.vf21, vu.vf05);
  // nop                        |  maddw.xyzw vf21, vf06, vf00    422
  vu.acc.madd(Mask::xyzw, vu.vf21, vu.vf06, vu.vf00.w());
  // nop                        |  nop                            423

  // div Q, vf00.w, vf19.w      |  mul.xy vf18, vf18, Q           424
  vu.vf18.mul(Mask::xy, vu.vf18, vu.Q);
  vu.Q = vu.vf00.w() / vu.vf19.w();
  // nop                        |  nop                            425

  // nop                        |  nop                            426

  // nop                        |  nop                            427

  // nop                        |  nop                            428

  // nop                        |  nop                            429

  // nop                        |  nop                            430

  // sq.xyz vf20, 0(vi08)       |  mul.xy vf19, vf19, Q           431
  vu.vf19.mul(Mask::xy, vu.vf19, vu.Q);
  sq_buffer(Mask::xyz, vu.vf20, vu.vi08);
  // sq.xyz vf21, 3(vi08)       |  mula.xyzw ACC, vf18, vf05      432
  vu.acc.mula(Mask::xyzw, vu.vf18, vu.vf05);
  sq_buffer(Mask::xyz, vu.vf21, vu.vi08 + 3);
  // nop                        |  maddw.xyzw vf18, vf06, vf00    433
  vu.acc.madd(Mask::xyzw, vu.vf18, vu.vf06, vu.vf00.w());
  // nop                        |  nop                            434

  // nop                        |  mula.xyzw ACC, vf19, vf05      435
  vu.acc.mula(Mask::xyzw, vu.vf19, vu.vf05);
  // iaddi vi08, vi08, 0xc      |  maddw.xyzw vf19, vf06, vf00    436
  vu.acc.madd(Mask::xyzw, vu.vf19, vu.vf06, vu.vf00.w());
  vu.vi08 = vu.vi08 + 12;
  // iaddi vi06, vi06, -0x1     |  nop                            437
  vu.vi06 = vu.vi06 + -1;
  // sq.xyz vf18, -6(vi08)      |  nop                            438
  sq_buffer(Mask::xyz, vu.vf18, vu.vi08 + -6);
  // BRANCH!
  // ibgtz vi06, L22            |  nop                            439
  bc = ((s16)vu.vi06) > 0;
  // sq.xyz vf19, -3(vi08)      |  nop                            440
  sq_buffer(Mask::xyz, vu.vf19, vu.vi08 + -3);
  if (bc) {
    goto L22;
  }

  // jr vi15                    |  nop                            441
  // nop                        |  nop                            442
}

void OceanNear::run_L23_vu2c() {
  bool bc;
  // iaddi vi06, vi00, 0x4      |  nop                            443
  vu.vi06 = 4;
L24:
  // lq.xyzw vf12, 0(vi08)      |  nop                            444
  lq_buffer(Mask::xyzw, vu.vf12, vu.vi08);
  // lq.xyzw vf13, 3(vi08)      |  nop                            445
  lq_buffer(Mask::xyzw, vu.vf13, vu.vi08 + 3);
  // lq.xyzw vf14, 6(vi08)      |  nop                            446
  lq_buffer(Mask::xyzw, vu.vf14, vu.vi08 + 6);
  // lq.xyzw vf15, 9(vi08)      |  nop                            447
  lq_buffer(Mask::xyzw, vu.vf15, vu.vi08 + 9);
  // lq.xyzw vf24, -59(vi08)    |  nop                            448
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi08 + -59);
  // lq.xyzw vf25, -56(vi08)    |  nop                            449
  lq_buffer(Mask::xyzw, vu.vf25, vu.vi08 + -56);
  // lq.xyzw vf26, -53(vi08)    |  nop                            450
  lq_buffer(Mask::xyzw, vu.vf26, vu.vi08 + -53);
  // lq.xyzw vf27, -50(vi08)    |  nop                            451
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi08 + -50);
  // lq.xyzw vf28, -61(vi08)    |  nop                            452
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi08 + -61);
  // lq.xyzw vf29, -58(vi08)    |  nop                            453
  lq_buffer(Mask::xyzw, vu.vf29, vu.vi08 + -58);
  // lq.xyzw vf30, -55(vi08)    |  nop                            454
  lq_buffer(Mask::xyzw, vu.vf30, vu.vi08 + -55);
  // lq.xyzw vf31, -52(vi08)    |  nop                            455
  lq_buffer(Mask::xyzw, vu.vf31, vu.vi08 + -52);
  // iaddi vi08, vi08, 0xc      |  mulz.xyzw vf12, vf12, vf28     456
  vu.vf12.mul(Mask::xyzw, vu.vf12, vu.vf28.z());
  vu.vi08 = vu.vi08 + 12;
  // sq.xyzw vf24, -10(vi08)    |  mulz.xyzw vf13, vf13, vf29     457
  vu.vf13.mul(Mask::xyzw, vu.vf13, vu.vf29.z());
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi08 + -10);
  // sq.xyzw vf25, -7(vi08)     |  mulz.xyzw vf14, vf14, vf30     458
  vu.vf14.mul(Mask::xyzw, vu.vf14, vu.vf30.z());
  sq_buffer(Mask::xyzw, vu.vf25, vu.vi08 + -7);
  // sq.xyzw vf26, -4(vi08)     |  mulz.xyzw vf15, vf15, vf31     459
  vu.vf15.mul(Mask::xyzw, vu.vf15, vu.vf31.z());
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi08 + -4);
  // sq.xyzw vf27, -1(vi08)     |  nop                            460
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi08 + -1);
  // sq.xyzw vf12, -12(vi08)    |  nop                            461
  sq_buffer(Mask::xyzw, vu.vf12, vu.vi08 + -12);
  // sq.xyzw vf13, -9(vi08)     |  nop                            462
  sq_buffer(Mask::xyzw, vu.vf13, vu.vi08 + -9);
  // sq.xyzw vf14, -6(vi08)     |  nop                            463
  sq_buffer(Mask::xyzw, vu.vf14, vu.vi08 + -6);
  // sq.xyzw vf15, -3(vi08)     |  nop                            464
  sq_buffer(Mask::xyzw, vu.vf15, vu.vi08 + -3);
  // BRANCH!
  // ibgtz vi06, L24            |  nop                            465
  bc = ((s16)vu.vi06) > 0;
  // iaddi vi06, vi06, -0x1     |  nop                            466
  vu.vi06 = vu.vi06 + -1;
  if (bc) {
    goto L24;
  }

  // jr vi15                    |  nop                            467
  // nop                        |  nop                            468
}

void OceanNear::run_L25_vu2c() {
  bool bc;
  // lq.xyzw vf31, 952(vi00)    |  nop                            469
  lq_buffer(Mask::xyzw, vu.vf31, 952);
  // iaddiu vi10, vi00, 0x32b   |  nop                            470
  vu.vi10 = 0x32b; /* 811 */
L26:
  // iaddi vi14, vi14, -0x1     |  nop                            471
  vu.vi14 = vu.vi14 + -1;
  // iaddi vi08, vi10, 0x7      |  nop                            472
  vu.vi08 = vu.vi10 + 7;
  // ilw.x vi12, 988(vi14)      |  nop                            473
  ilw_buffer(Mask::x, vu.vi12, vu.vi14 + 988);
  // ilw.y vi05, 988(vi14)      |  nop                            474
  ilw_buffer(Mask::y, vu.vi05, vu.vi14 + 988);
  // BRANCH!
  // ibne vi00, vi12, L27       |  nop                            475
  bc = (vu.vi12 != 0);
  // ilw.z vi03, 988(vi14)      |  nop                            476
  ilw_buffer(Mask::z, vu.vi03, vu.vi14 + 988);
  if (bc) {
    goto L27;
  }

  // lq.xyzw vf20, 282(vi05)    |  nop                            477
  lq_buffer(Mask::xyzw, vu.vf20, vu.vi05 + 282);
  // lq.xyzw vf21, 309(vi05)    |  nop                            478
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi05 + 309);
  // lq.xyzw vf18, 285(vi05)    |  nop                            479
  lq_buffer(Mask::xyzw, vu.vf18, vu.vi05 + 285);
  // lq.xyzw vf22, -5(vi03)     |  nop                            480
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi03 + -5);
  // lq.xyzw vf23, -2(vi03)     |  nop                            481
  lq_buffer(Mask::xyzw, vu.vf23, vu.vi03 + -2);
  // lq.xyzw vf16, 1(vi03)      |  nop                            482
  lq_buffer(Mask::xyzw, vu.vf16, vu.vi03 + 1);
  // lq.xyzw vf08, 55(vi03)     |  nop                            483
  lq_buffer(Mask::xyzw, vu.vf08, vu.vi03 + 55);
  // lq.xyzw vf09, 58(vi03)     |  nop                            484
  lq_buffer(Mask::xyzw, vu.vf09, vu.vi03 + 58);
  // lq.xyzw vf10, 61(vi03)     |  nop                            485
  lq_buffer(Mask::xyzw, vu.vf10, vu.vi03 + 61);
  // lq.xyzw vf28, 57(vi03)     |  nop                            486
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi03 + 57);
  // lq.xyzw vf29, 60(vi03)     |  nop                            487
  lq_buffer(Mask::xyzw, vu.vf29, vu.vi03 + 60);
  // BRANCH!
  // b L28                      |  nop                            488
  bc = true;
  // lq.xyzw vf30, 63(vi03)     |  nop                            489
  lq_buffer(Mask::xyzw, vu.vf30, vu.vi03 + 63);
  if (bc) {
    goto L28;
  }

L27:
  // lq.xyzw vf20, 306(vi05)    |  nop                            490
  lq_buffer(Mask::xyzw, vu.vf20, vu.vi05 + 306);
  // lq.xyzw vf21, 282(vi05)    |  nop                            491
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi05 + 282);
  // lq.xyzw vf18, 309(vi05)    |  nop                            492
  lq_buffer(Mask::xyzw, vu.vf18, vu.vi05 + 309);
  // lq.xyzw vf22, -2(vi03)     |  nop                            493
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi03 + -2);
  // lq.xyzw vf23, 1(vi03)      |  nop                            494
  lq_buffer(Mask::xyzw, vu.vf23, vu.vi03 + 1);
  // lq.xyzw vf16, 4(vi03)      |  nop                            495
  lq_buffer(Mask::xyzw, vu.vf16, vu.vi03 + 4);
  // lq.xyzw vf08, 58(vi03)     |  nop                            496
  lq_buffer(Mask::xyzw, vu.vf08, vu.vi03 + 58);
  // lq.xyzw vf09, 61(vi03)     |  nop                            497
  lq_buffer(Mask::xyzw, vu.vf09, vu.vi03 + 61);
  // lq.xyzw vf10, 64(vi03)     |  nop                            498
  lq_buffer(Mask::xyzw, vu.vf10, vu.vi03 + 64);
  // lq.xyzw vf28, 60(vi03)     |  nop                            499
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi03 + 60);
  // lq.xyzw vf29, 63(vi03)     |  nop                            500
  lq_buffer(Mask::xyzw, vu.vf29, vu.vi03 + 63);
  // lq.xyzw vf30, 66(vi03)     |  nop                            501
  lq_buffer(Mask::xyzw, vu.vf30, vu.vi03 + 66);
L28:
  // sq.xyzw vf20, 0(vi08)      |  itof0.xyzw vf22, vf22          502
  vu.vf22.itof0(Mask::xyzw, vu.vf22);
  sq_buffer(Mask::xyzw, vu.vf20, vu.vi08);
  // sq.xyzw vf21, 3(vi08)      |  itof0.xyzw vf23, vf23          503
  vu.vf23.itof0(Mask::xyzw, vu.vf23);
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi08 + 3);
  // sq.xyzw vf18, 6(vi08)      |  itof0.xyzw vf16, vf16          504
  vu.vf16.itof0(Mask::xyzw, vu.vf16);
  sq_buffer(Mask::xyzw, vu.vf18, vu.vi08 + 6);
  // sq.xyzw vf20, 9(vi08)      |  nop                            505
  sq_buffer(Mask::xyzw, vu.vf20, vu.vi08 + 9);
  // sq.xyzw vf22, 1(vi08)      |  nop                            506
  sq_buffer(Mask::xyzw, vu.vf22, vu.vi08 + 1);
  // sq.xyzw vf23, 4(vi08)      |  nop                            507
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi08 + 4);
  // sq.xyzw vf16, 7(vi08)      |  nop                            508
  sq_buffer(Mask::xyzw, vu.vf16, vu.vi08 + 7);
  // sq.xyzw vf22, 10(vi08)     |  nop                            509
  sq_buffer(Mask::xyzw, vu.vf22, vu.vi08 + 10);
  // sq.xyzw vf28, 2(vi08)      |  nop                            510
  sq_buffer(Mask::xyzw, vu.vf28, vu.vi08 + 2);
  // sq.xyzw vf29, 5(vi08)      |  nop                            511
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi08 + 5);
  // sq.xyzw vf30, 8(vi08)      |  nop                            512
  sq_buffer(Mask::xyzw, vu.vf30, vu.vi08 + 8);
  // sq.xyzw vf28, 11(vi08)     |  nop                            513
  sq_buffer(Mask::xyzw, vu.vf28, vu.vi08 + 11);
  // lq.xyzw vf22, 976(vi00)    |  nop                            514
  lq_buffer(Mask::xyzw, vu.vf22, 976);
  // sq.xyzw vf08, 35(vi08)     |  nop                            515
  sq_buffer(Mask::xyzw, vu.vf08, vu.vi08 + 35);
  // sq.xyzw vf09, 38(vi08)     |  nop                            516
  sq_buffer(Mask::xyzw, vu.vf09, vu.vi08 + 38);
  // sq.xyzw vf10, 41(vi08)     |  nop                            517
  sq_buffer(Mask::xyzw, vu.vf10, vu.vi08 + 41);
  // sq.xyzw vf08, 44(vi08)     |  nop                            518
  sq_buffer(Mask::xyzw, vu.vf08, vu.vi08 + 44);
  // sq.xyzw vf22, 36(vi08)     |  nop                            519
  sq_buffer(Mask::xyzw, vu.vf22, vu.vi08 + 36);
  // sq.xyzw vf22, 39(vi08)     |  nop                            520
  sq_buffer(Mask::xyzw, vu.vf22, vu.vi08 + 39);
  // sq.xyzw vf22, 42(vi08)     |  nop                            521
  sq_buffer(Mask::xyzw, vu.vf22, vu.vi08 + 42);
  // sq.xyzw vf22, 45(vi08)     |  nop                            522
  sq_buffer(Mask::xyzw, vu.vf22, vu.vi08 + 45);
  // sq.xyzw vf28, 37(vi08)     |  nop                            523
  sq_buffer(Mask::xyzw, vu.vf28, vu.vi08 + 37);
  // sq.xyzw vf29, 40(vi08)     |  nop                            524
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi08 + 40);
  // sq.xyzw vf30, 43(vi08)     |  nop                            525
  sq_buffer(Mask::xyzw, vu.vf30, vu.vi08 + 43);
  // sq.xyzw vf28, 46(vi08)     |  nop                            526
  sq_buffer(Mask::xyzw, vu.vf28, vu.vi08 + 46);
  // lq.xyzw vf07, 959(vi00)    |  maddw.xyzw vf26, vf06, vf00    527
  vu.acc.madd(Mask::xyzw, vu.vf26, vu.vf06, vu.vf00.w());
  lq_buffer(Mask::xyzw, vu.vf07, 959);
  // ior vi03, vi08, vi00       |  nop                            528
  vu.vi03 = vu.vi08;
  // BRANCH!
  // bal vi13, L32              |  nop                            529
  // ASSERT(false);
  // iaddi vi05, vi00, 0x3      |  nop                            530
  vu.vi05 = 3;
  // if (bc) { goto L32; }
  run_L32_vu2c();

  // BRANCH!
  // ibeq vi00, vi05, L29       |  nop                            531
  bc = (vu.vi05 == 0);
  // iaddiu vi06, vi05, 0x4000  |  nop                            532
  vu.vi06 = vu.vi05 + 0x4000; /* 16384 */
  if (bc) {
    goto L29;
  }

  // BRANCH!
  // bal vi13, L30              |  nop                            533
  // ASSERT(false);
  // iaddiu vi06, vi06, 0x4000  |  nop                            534
  vu.vi06 = vu.vi06 + 0x4000; /* 16384 */
  // if (bc) { goto L30; }
  run_L30_vu2c();

  // iaddiu vi08, vi00, 0x3d1   |  nop                            535
  vu.vi08 = 0x3d1; /* 977 */
  // xgkick vi10                |  nop                            536
  xgkick(vu.vi10);
  // lq.xyzw vf07, 960(vi00)    |  nop                            537
  lq_buffer(Mask::xyzw, vu.vf07, 960);
  // xgkick vi08                |  nop                            538
  xgkick(vu.vi08);
  // iaddi vi08, vi10, 0x6      |  nop                            539
  vu.vi08 = vu.vi10 + 6;
  // sq.xyzw vf07, 6(vi10)      |  nop                            540
  sq_buffer(Mask::xyzw, vu.vf07, vu.vi10 + 6);
  // isw.x vi06, 6(vi10)        |  nop                            541
  isw_buffer(Mask::x, vu.vi06, vu.vi10 + 6);
  // xgkick vi08                |  nop                            542
  xgkick(vu.vi08);
  // iaddiu vi08, vi00, 0x3d5   |  nop                            543
  vu.vi08 = 0x3d5; /* 981 */
  // nop                        |  nop                            544

  // xgkick vi08                |  nop                            545
  xgkick(vu.vi08);
  // iaddiu vi08, vi10, 0x2a    |  nop                            546
  vu.vi08 = vu.vi10 + 0x2a; /* 42 */
  // iaddiu vi03, vi10, 0x2a    |  nop                            547
  vu.vi03 = vu.vi10 + 0x2a; /* 42 */
  // iaddi vi05, vi00, 0x3      |  nop                            548
  vu.vi05 = 3;
  // BRANCH!
  // bal vi13, L32              |  nop                            549
  // ASSERT(false);
  // lq.xyzw vf07, 961(vi00)    |  nop                            550
  lq_buffer(Mask::xyzw, vu.vf07, 961);
  // if (bc) { goto L32; }
  run_L32_vu2c();

  // iaddiu vi06, vi05, 0x4000  |  nop                            551
  vu.vi06 = vu.vi05 + 0x4000; /* 16384 */
  // BRANCH!
  // bal vi13, L30              |  nop                            552
  // ASSERT(false);
  // iaddiu vi06, vi06, 0x4000  |  nop                            553
  vu.vi06 = vu.vi06 + 0x4000; /* 16384 */
  // if (bc) { goto L30; }
  run_L30_vu2c();

  // iaddiu vi08, vi10, 0x23    |  nop                            554
  vu.vi08 = vu.vi10 + 0x23; /* 35 */
  // iaddiu vi01, vi00, 0x69c   |  nop                            555
  vu.vi01 = 0x69c; /* 1692 */
  // xgkick vi08                |  nop                            556
  xgkick(vu.vi08);
  // isub vi10, vi01, vi10      |  nop                            557
  vu.vi10 = vu.vi01 - vu.vi10;
L29:
  // BRANCH!
  // ibgtz vi14, L26            |  nop                            558
  bc = ((s16)vu.vi14) > 0;
  // nop                        |  nop                            559

  if (bc) {
    goto L26;
  }

  // lq.xyzw vf08, 0(vi02)      |  nop                            560
  lq_buffer(Mask::xyzw, vu.vf08, vu.vi02);
  // lq.xyzw vf09, 1(vi02)      |  nop                            561
  lq_buffer(Mask::xyzw, vu.vf09, vu.vi02 + 1);
  // jr vi15                    |  nop                            562
  // ASSERT(false);
  // lq.xyzw vf10, 2(vi02)      |  nop                            563
  lq_buffer(Mask::xyzw, vu.vf10, vu.vi02 + 2);
}

void OceanNear::run_L25_vu2c_jak2() {
  bool bc;
  // lq.xyzw vf31, 952(vi00)    |  nop                            469
  lq_buffer(Mask::xyzw, vu.vf31, 952);
  // iaddiu vi10, vi00, 0x32b   |  nop                            470
  vu.vi10 = 0x32b; /* 811 */
L26:
  // iaddi vi14, vi14, -0x1     |  nop                            471
  vu.vi14 = vu.vi14 + -1;
  // iaddi vi08, vi10, 0x7      |  nop                            472
  vu.vi08 = vu.vi10 + 7;
  // ilw.x vi12, 989(vi14)      |  nop                            473
  ilw_buffer(Mask::x, vu.vi12, vu.vi14 + 989);
  // ilw.y vi05, 989(vi14)      |  nop                            474
  ilw_buffer(Mask::y, vu.vi05, vu.vi14 + 989);
  // BRANCH!
  // ibne vi00, vi12, L27       |  nop                            475
  bc = (vu.vi12 != 0);
  // ilw.z vi03, 989(vi14)      |  nop                            476
  ilw_buffer(Mask::z, vu.vi03, vu.vi14 + 989);
  if (bc) {
    goto L27;
  }

  // lq.xyzw vf20, 282(vi05)    |  nop                            477
  lq_buffer(Mask::xyzw, vu.vf20, vu.vi05 + 282);
  // lq.xyzw vf21, 309(vi05)    |  nop                            478
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi05 + 309);
  // lq.xyzw vf18, 285(vi05)    |  nop                            479
  lq_buffer(Mask::xyzw, vu.vf18, vu.vi05 + 285);
  // lq.xyzw vf22, -5(vi03)     |  nop                            480
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi03 + -5);
  // lq.xyzw vf23, -2(vi03)     |  nop                            481
  lq_buffer(Mask::xyzw, vu.vf23, vu.vi03 + -2);
  // lq.xyzw vf16, 1(vi03)      |  nop                            482
  lq_buffer(Mask::xyzw, vu.vf16, vu.vi03 + 1);
  // lq.xyzw vf08, 55(vi03)     |  nop                            483
  lq_buffer(Mask::xyzw, vu.vf08, vu.vi03 + 55);
  // lq.xyzw vf09, 58(vi03)     |  nop                            484
  lq_buffer(Mask::xyzw, vu.vf09, vu.vi03 + 58);
  // lq.xyzw vf10, 61(vi03)     |  nop                            485
  lq_buffer(Mask::xyzw, vu.vf10, vu.vi03 + 61);
  // lq.xyzw vf28, 57(vi03)     |  nop                            486
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi03 + 57);
  // lq.xyzw vf29, 60(vi03)     |  nop                            487
  lq_buffer(Mask::xyzw, vu.vf29, vu.vi03 + 60);
  // BRANCH!
  // b L28                      |  nop                            488
  bc = true;
  // lq.xyzw vf30, 63(vi03)     |  nop                            489
  lq_buffer(Mask::xyzw, vu.vf30, vu.vi03 + 63);
  if (bc) {
    goto L28;
  }

L27:
  // lq.xyzw vf20, 306(vi05)    |  nop                            490
  lq_buffer(Mask::xyzw, vu.vf20, vu.vi05 + 306);
  // lq.xyzw vf21, 282(vi05)    |  nop                            491
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi05 + 282);
  // lq.xyzw vf18, 309(vi05)    |  nop                            492
  lq_buffer(Mask::xyzw, vu.vf18, vu.vi05 + 309);
  // lq.xyzw vf22, -2(vi03)     |  nop                            493
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi03 + -2);
  // lq.xyzw vf23, 1(vi03)      |  nop                            494
  lq_buffer(Mask::xyzw, vu.vf23, vu.vi03 + 1);
  // lq.xyzw vf16, 4(vi03)      |  nop                            495
  lq_buffer(Mask::xyzw, vu.vf16, vu.vi03 + 4);
  // lq.xyzw vf08, 58(vi03)     |  nop                            496
  lq_buffer(Mask::xyzw, vu.vf08, vu.vi03 + 58);
  // lq.xyzw vf09, 61(vi03)     |  nop                            497
  lq_buffer(Mask::xyzw, vu.vf09, vu.vi03 + 61);
  // lq.xyzw vf10, 64(vi03)     |  nop                            498
  lq_buffer(Mask::xyzw, vu.vf10, vu.vi03 + 64);
  // lq.xyzw vf28, 60(vi03)     |  nop                            499
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi03 + 60);
  // lq.xyzw vf29, 63(vi03)     |  nop                            500
  lq_buffer(Mask::xyzw, vu.vf29, vu.vi03 + 63);
  // lq.xyzw vf30, 66(vi03)     |  nop                            501
  lq_buffer(Mask::xyzw, vu.vf30, vu.vi03 + 66);
L28:
  // sq.xyzw vf20, 0(vi08)      |  itof0.xyzw vf22, vf22          502
  vu.vf22.itof0(Mask::xyzw, vu.vf22);
  sq_buffer(Mask::xyzw, vu.vf20, vu.vi08);
  // sq.xyzw vf21, 3(vi08)      |  itof0.xyzw vf23, vf23          503
  vu.vf23.itof0(Mask::xyzw, vu.vf23);
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi08 + 3);
  // sq.xyzw vf18, 6(vi08)      |  itof0.xyzw vf16, vf16          504
  vu.vf16.itof0(Mask::xyzw, vu.vf16);
  sq_buffer(Mask::xyzw, vu.vf18, vu.vi08 + 6);
  // sq.xyzw vf20, 9(vi08)      |  nop                            505
  sq_buffer(Mask::xyzw, vu.vf20, vu.vi08 + 9);
  // sq.xyzw vf22, 1(vi08)      |  nop                            506
  sq_buffer(Mask::xyzw, vu.vf22, vu.vi08 + 1);
  // sq.xyzw vf23, 4(vi08)      |  nop                            507
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi08 + 4);
  // sq.xyzw vf16, 7(vi08)      |  nop                            508
  sq_buffer(Mask::xyzw, vu.vf16, vu.vi08 + 7);
  // sq.xyzw vf22, 10(vi08)     |  nop                            509
  sq_buffer(Mask::xyzw, vu.vf22, vu.vi08 + 10);
  // sq.xyzw vf28, 2(vi08)      |  nop                            510
  sq_buffer(Mask::xyzw, vu.vf28, vu.vi08 + 2);
  // sq.xyzw vf29, 5(vi08)      |  nop                            511
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi08 + 5);
  // sq.xyzw vf30, 8(vi08)      |  nop                            512
  sq_buffer(Mask::xyzw, vu.vf30, vu.vi08 + 8);
  // sq.xyzw vf28, 11(vi08)     |  nop                            513
  sq_buffer(Mask::xyzw, vu.vf28, vu.vi08 + 11);
  // lq.xyzw vf22, 977(vi00)    |  nop                            514
  lq_buffer(Mask::xyzw, vu.vf22, 977);
  // sq.xyzw vf08, 35(vi08)     |  nop                            515
  sq_buffer(Mask::xyzw, vu.vf08, vu.vi08 + 35);
  // sq.xyzw vf09, 38(vi08)     |  nop                            516
  sq_buffer(Mask::xyzw, vu.vf09, vu.vi08 + 38);
  // sq.xyzw vf10, 41(vi08)     |  nop                            517
  sq_buffer(Mask::xyzw, vu.vf10, vu.vi08 + 41);
  // sq.xyzw vf08, 44(vi08)     |  nop                            518
  sq_buffer(Mask::xyzw, vu.vf08, vu.vi08 + 44);
  // sq.xyzw vf22, 36(vi08)     |  nop                            519
  sq_buffer(Mask::xyzw, vu.vf22, vu.vi08 + 36);
  // sq.xyzw vf22, 39(vi08)     |  nop                            520
  sq_buffer(Mask::xyzw, vu.vf22, vu.vi08 + 39);
  // sq.xyzw vf22, 42(vi08)     |  nop                            521
  sq_buffer(Mask::xyzw, vu.vf22, vu.vi08 + 42);
  // sq.xyzw vf22, 45(vi08)     |  nop                            522
  sq_buffer(Mask::xyzw, vu.vf22, vu.vi08 + 45);
  // sq.xyzw vf28, 37(vi08)     |  nop                            523
  sq_buffer(Mask::xyzw, vu.vf28, vu.vi08 + 37);
  // sq.xyzw vf29, 40(vi08)     |  nop                            524
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi08 + 40);
  // sq.xyzw vf30, 43(vi08)     |  nop                            525
  sq_buffer(Mask::xyzw, vu.vf30, vu.vi08 + 43);
  // sq.xyzw vf28, 46(vi08)     |  nop                            526
  sq_buffer(Mask::xyzw, vu.vf28, vu.vi08 + 46);
  // lq.xyzw vf07, 960(vi00)    |  maddw.xyzw vf26, vf06, vf00    527
  vu.acc.madd(Mask::xyzw, vu.vf26, vu.vf06, vu.vf00.w());
  lq_buffer(Mask::xyzw, vu.vf07, 960);
  // ior vi03, vi08, vi00       |  nop                            528
  vu.vi03 = vu.vi08;
  // BRANCH!
  // bal vi13, L32              |  nop                            529
  // ASSERT(false);
  // iaddi vi05, vi00, 0x3      |  nop                            530
  vu.vi05 = 3;
  // if (bc) { goto L32; }
  run_L32_vu2c();

  // BRANCH!
  // ibeq vi00, vi05, L29       |  nop                            531
  bc = (vu.vi05 == 0);
  // iaddiu vi06, vi05, 0x4000  |  nop                            532
  vu.vi06 = vu.vi05 + 0x4000; /* 16384 */
  if (bc) {
    goto L29;
  }

  // BRANCH!
  // bal vi13, L30              |  nop                            533
  // ASSERT(false);
  // iaddiu vi06, vi06, 0x4000  |  nop                            534
  vu.vi06 = vu.vi06 + 0x4000; /* 16384 */
  // if (bc) { goto L30; }
  run_L30_vu2c();

  // iaddiu vi08, vi00, 0x3d2   |  nop                            535
  vu.vi08 = 0x3d2; /* 977 */
  // xgkick vi10                |  nop                            536
  xgkick(vu.vi10);
  // lq.xyzw vf07, 960(vi00)    |  nop                            537
  lq_buffer(Mask::xyzw, vu.vf07, 960);
  // xgkick vi08                |  nop                            538
  xgkick(vu.vi08);
  // iaddi vi08, vi10, 0x6      |  nop                            539
  vu.vi08 = vu.vi10 + 6;
  // sq.xyzw vf07, 6(vi10)      |  nop                            540
  sq_buffer(Mask::xyzw, vu.vf07, vu.vi10 + 6);
  // isw.x vi06, 6(vi10)        |  nop                            541
  isw_buffer(Mask::x, vu.vi06, vu.vi10 + 6);
  // xgkick vi08                |  nop                            542
  xgkick(vu.vi08);
  // iaddiu vi08, vi00, 0x3d6   |  nop                            543
  vu.vi08 = 0x3d6; /* 981 */
  // nop                        |  nop                            544

  // xgkick vi08                |  nop                            545
  xgkick(vu.vi08);
  // iaddiu vi08, vi10, 0x2a    |  nop                            546
  vu.vi08 = vu.vi10 + 0x2a; /* 42 */
  // iaddiu vi03, vi10, 0x2a    |  nop                            547
  vu.vi03 = vu.vi10 + 0x2a; /* 42 */
  // iaddi vi05, vi00, 0x3      |  nop                            548
  vu.vi05 = 3;
  // BRANCH!
  // bal vi13, L32              |  nop                            549
  // ASSERT(false);
  // lq.xyzw vf07, 962(vi00)    |  nop                            550
  lq_buffer(Mask::xyzw, vu.vf07, 962);
  // if (bc) { goto L32; }
  run_L32_vu2c();

  // iaddiu vi06, vi05, 0x4000  |  nop                            551
  vu.vi06 = vu.vi05 + 0x4000; /* 16384 */
  // BRANCH!
  // bal vi13, L30              |  nop                            552
  // ASSERT(false);
  // iaddiu vi06, vi06, 0x4000  |  nop                            553
  vu.vi06 = vu.vi06 + 0x4000; /* 16384 */
  // if (bc) { goto L30; }
  run_L30_vu2c();

  // iaddiu vi08, vi10, 0x23    |  nop                            554
  vu.vi08 = vu.vi10 + 0x23; /* 35 */
  // iaddiu vi01, vi00, 0x69c   |  nop                            555
  vu.vi01 = 0x69c; /* 1692 */
  // xgkick vi08                |  nop                            556
  xgkick(vu.vi08);
  // isub vi10, vi01, vi10      |  nop                            557
  vu.vi10 = vu.vi01 - vu.vi10;
L29:
  // BRANCH!
  // ibgtz vi14, L26            |  nop                            558
  bc = ((s16)vu.vi14) > 0;
  // nop                        |  nop                            559

  if (bc) {
    goto L26;
  }

  // lq.xyzw vf08, 0(vi02)      |  nop                            560
  lq_buffer(Mask::xyzw, vu.vf08, vu.vi02);
  // lq.xyzw vf09, 1(vi02)      |  nop                            561
  lq_buffer(Mask::xyzw, vu.vf09, vu.vi02 + 1);
  // jr vi15                    |  nop                            562
  // ASSERT(false);
  // lq.xyzw vf10, 2(vi02)      |  nop                            563
  lq_buffer(Mask::xyzw, vu.vf10, vu.vi02 + 2);
}

void OceanNear::run_L30_vu2c() {
  bool bc;
  // sq.xyzw vf07, -1(vi08)     |  nop                            564
  sq_buffer(Mask::xyzw, vu.vf07, vu.vi08 + -1);
  // isw.x vi06, -1(vi08)       |  nop                            565
  isw_buffer(Mask::x, vu.vi06, vu.vi08 + -1);
L31:
  // lqi.xyzw vf24, vi08        |  nop                            566
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi08++);
  // lqi.xyzw vf27, vi08        |  nop                            567
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi08++);
  // lqi.xyzw vf21, vi08        |  nop                            568
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi08++);
  // nop                        |  nop                            569

  // nop                        |  nop                            570

  // nop                        |  nop                            571

  // div Q, vf00.w, vf21.w      |  mul.xyzw vf21, vf21, vf31      572
  // SWAP here
  vu.Q = vu.vf00.w() / vu.vf21.w();
  vu.vf21.mul(Mask::xyzw, vu.vf21, vu.vf31);
  // nop                        |  nop                            573

  // nop                        |  nop                            574

  // nop                        |  nop                            575

  // nop                        |  nop                            576

  // nop                        |  nop                            577

  // nop                        |  nop                            578

  // nop                        |  mul.xyz vf21, vf21, Q          579
  vu.vf21.mul(Mask::xyz, vu.vf21, vu.Q);
  // nop                        |  mul.xyzw vf24, vf24, Q         580
  vu.vf24.mul(Mask::xyzw, vu.vf24, vu.Q);
  // nop                        |  nop                            581

  // nop                        |  nop                            582

  // nop                        |  add.xyzw vf21, vf21, vf02      583
  vu.vf21.add(Mask::xyzw, vu.vf21, vu.vf02);
  // nop                        |  nop                            584

  // nop                        |  nop                            585

  // nop                        |  nop                            586

  // nop                        |  maxy.w vf21, vf21, vf03        587
  vu.vf21.max(Mask::w, vu.vf21, vu.vf03.y());
  // nop                        |  nop                            588

  // nop                        |  nop                            589

  // nop                        |  nop                            590

  // nop                        |  miniz.w vf21, vf21, vf03       591
  vu.vf21.mini(Mask::w, vu.vf21, vu.vf03.z());
  // nop                        |  nop                            592

  // nop                        |  nop                            593

  // nop                        |  ftoi0.xyzw vf27, vf27          594
  vu.vf27.ftoi0(Mask::xyzw, vu.vf27);
  // nop                        |  ftoi4.xyzw vf21, vf21          595
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);
  // nop                        |  nop                            596

  // sq.xyzw vf24, -3(vi08)     |  nop                            597
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi08 + -3);
  // sq.xyzw vf27, -2(vi08)     |  nop                            598
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi08 + -2);
  // sq.xyzw vf21, -1(vi08)     |  nop                            599
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi08 + -1);
  // iaddi vi05, vi05, -0x1     |  nop                            600
  vu.vi05 = vu.vi05 + -1;
  // nop                        |  nop                            601

  // BRANCH!
  // ibne vi00, vi05, L31       |  nop                            602
  bc = (vu.vi05 != 0);
  // nop                        |  nop                            603

  if (bc) {
    goto L31;
  }

  // jr vi13                    |  nop                            604
  // nop                        |  nop                            605
}

void OceanNear::run_L32_vu2c() {
  bool bc;
  u32 cf = 0;
  // ior vi04, vi03, vi00       |  nop                            606
  vu.vi04 = vu.vi03;
  // lq.xyzw vf21, 2(vi03)      |  nop                            607
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi03 + 2);
  // ior vi06, vi05, vi00       |  nop                            608
  vu.vi06 = vu.vi05;
  // lq.xyzw vf24, 0(vi03)      |  nop                            609
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi03);
  // lq.xyzw vf27, 1(vi03)      |  nop                            610
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi03 + 1);
  // iaddi vi03, vi03, 0x3      |  clipw.xyz vf21, vf21           611
  cf = clip(vu.vf21, vu.vf21.w(), cf);
  vu.vi03 = vu.vi03 + 3;
L33:
  // lq.xyzw vf22, 2(vi03)      |  nop                            612
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi03 + 2);
  // lq.xyzw vf25, 0(vi03)      |  nop                            613
  lq_buffer(Mask::xyzw, vu.vf25, vu.vi03);
  // lq.xyzw vf28, 1(vi03)      |  nop                            614
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi03 + 1);
  // iaddi vi03, vi03, 0x3      |  nop                            615
  vu.vi03 = vu.vi03 + 3;
  // nop                        |  clipw.xyz vf22, vf22           616
  cf = clip(vu.vf22, vu.vf22.w(), cf);
  // nop                        |  nop                            617

  // nop                        |  nop                            618

  // nop                        |  nop                            619

  // fcor vi01, 0xfff7df        |  nop                            620
  fcor(vu.vi01, 0xfff7df, cf);

  // BRANCH!
  // ibne vi00, vi01, L44       |  nop                            621
  bc = (vu.vi01 != 0);
  // fcand vi01, 0x20           |  nop                            622
  fcand(vu.vi01, 0x20, cf);

  if (bc) {
    goto L44;
  }

  // BRANCH!
  // ibne vi00, vi01, L45       |  nop                            623
  bc = (vu.vi01 != 0);
  // fcand vi01, 0x800          |  nop                            624
  fcand(vu.vi01, 0x800, cf);

  if (bc) {
    goto L45;
  }

  // BRANCH!
  // ibne vi00, vi01, L46       |  nop                            625
  bc = (vu.vi01 != 0);
  // nop                        |  nop                            626

  if (bc) {
    goto L46;
  }

  // sqi.xyzw vf24, vi04        |  nop                            627
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  nop                            628
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  nop                            629
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
L34:
  // move.xyzw vf24, vf25       |  nop                            630
  vu.vf24.move(Mask::xyzw, vu.vf25);
  // iaddi vi05, vi05, -0x1     |  nop                            631
  vu.vi05 = vu.vi05 + -1;
  // move.xyzw vf27, vf28       |  nop                            632
  vu.vf27.move(Mask::xyzw, vu.vf28);
  // BRANCH!
  // ibne vi00, vi05, L33       |  nop                            633
  bc = (vu.vi05 != 0);
  // move.xyzw vf21, vf22       |  nop                            634
  vu.vf21.move(Mask::xyzw, vu.vf22);
  if (bc) {
    goto L33;
  }

  // lq.xyzw vf24, 0(vi08)      |  nop                            635
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi08);
  // lq.xyzw vf27, 1(vi08)      |  nop                            636
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi08 + 1);
  // lq.xyzw vf21, 2(vi08)      |  nop                            637
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi08 + 2);
  // ior vi03, vi08, vi00       |  nop                            638
  vu.vi03 = vu.vi08;
  // sqi.xyzw vf24, vi04        |  nop                            639
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  nop                            640
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  nop                            641
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
  // BRANCH!
  // ibeq vi00, vi06, L43       |  nop                            642
  bc = (vu.vi06 == 0);
  // ior vi05, vi06, vi00       |  nop                            643
  vu.vi05 = vu.vi06;
  if (bc) {
    goto L43;
  }

  // ior vi04, vi03, vi00       |  nop                            644
  vu.vi04 = vu.vi03;
  // lq.xyzw vf21, 2(vi03)      |  nop                            645
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi03 + 2);
  // ior vi06, vi05, vi00       |  nop                            646
  vu.vi06 = vu.vi05;
  // lq.xyzw vf24, 0(vi03)      |  nop                            647
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi03);
  // lq.xyzw vf27, 1(vi03)      |  nop                            648
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi03 + 1);
  // iaddi vi03, vi03, 0x3      |  clipw.xyz vf21, vf21           649
  cf = clip(vu.vf21, vu.vf21.w(), cf);
  vu.vi03 = vu.vi03 + 3;
L35:
  // lq.xyzw vf22, 2(vi03)      |  nop                            650
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi03 + 2);
  // lq.xyzw vf25, 0(vi03)      |  nop                            651
  lq_buffer(Mask::xyzw, vu.vf25, vu.vi03);
  // lq.xyzw vf28, 1(vi03)      |  nop                            652
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi03 + 1);
  // iaddi vi03, vi03, 0x3      |  nop                            653
  vu.vi03 = vu.vi03 + 3;
  // nop                        |  clipw.xyz vf22, vf22           654
  cf = clip(vu.vf22, vu.vf22.w(), cf);
  // nop                        |  nop                            655

  // nop                        |  nop                            656

  // nop                        |  nop                            657

  // fcor vi01, 0xfffdf7        |  nop                            658
  fcor(vu.vi01, 0xfffdf7, cf);

  // BRANCH!
  // ibne vi00, vi01, L47       |  nop                            659
  bc = (vu.vi01 != 0);
  // fcand vi01, 0x8            |  nop                            660
  fcand(vu.vi01, 0x8, cf);

  if (bc) {
    goto L47;
  }

  // BRANCH!
  // ibne vi00, vi01, L48       |  nop                            661
  bc = (vu.vi01 != 0);
  // fcand vi01, 0x200          |  nop                            662
  fcand(vu.vi01, 0x200, cf);

  if (bc) {
    goto L48;
  }

  // BRANCH!
  // ibne vi00, vi01, L49       |  nop                            663
  bc = (vu.vi01 != 0);
  // nop                        |  nop                            664

  if (bc) {
    goto L49;
  }

  // sqi.xyzw vf24, vi04        |  nop                            665
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  nop                            666
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  nop                            667
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
L36:
  // move.xyzw vf24, vf25       |  nop                            668
  vu.vf24.move(Mask::xyzw, vu.vf25);
  // iaddi vi05, vi05, -0x1     |  nop                            669
  vu.vi05 = vu.vi05 + -1;
  // move.xyzw vf27, vf28       |  nop                            670
  vu.vf27.move(Mask::xyzw, vu.vf28);
  // BRANCH!
  // ibne vi00, vi05, L35       |  nop                            671
  bc = (vu.vi05 != 0);
  // move.xyzw vf21, vf22       |  nop                            672
  vu.vf21.move(Mask::xyzw, vu.vf22);
  if (bc) {
    goto L35;
  }

  // lq.xyzw vf24, 0(vi08)      |  nop                            673
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi08);
  // lq.xyzw vf27, 1(vi08)      |  nop                            674
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi08 + 1);
  // lq.xyzw vf21, 2(vi08)      |  nop                            675
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi08 + 2);
  // ior vi03, vi08, vi00       |  nop                            676
  vu.vi03 = vu.vi08;
  // sqi.xyzw vf24, vi04        |  nop                            677
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  nop                            678
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  nop                            679
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
  // BRANCH!
  // ibeq vi00, vi06, L43       |  nop                            680
  bc = (vu.vi06 == 0);
  // ior vi05, vi06, vi00       |  nop                            681
  vu.vi05 = vu.vi06;
  if (bc) {
    goto L43;
  }

  // ior vi04, vi03, vi00       |  nop                            682
  vu.vi04 = vu.vi03;
  // lq.xyzw vf21, 2(vi03)      |  nop                            683
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi03 + 2);
  // ior vi06, vi05, vi00       |  nop                            684
  vu.vi06 = vu.vi05;
  // lq.xyzw vf24, 0(vi03)      |  nop                            685
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi03);
  // lq.xyzw vf27, 1(vi03)      |  nop                            686
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi03 + 1);
  // iaddi vi03, vi03, 0x3      |  clipw.xyz vf21, vf21           687
  cf = clip(vu.vf21, vu.vf21.w(), cf);
  vu.vi03 = vu.vi03 + 3;
L37:
  // lq.xyzw vf22, 2(vi03)      |  nop                            688
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi03 + 2);
  // lq.xyzw vf25, 0(vi03)      |  nop                            689
  lq_buffer(Mask::xyzw, vu.vf25, vu.vi03);
  // lq.xyzw vf28, 1(vi03)      |  nop                            690
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi03 + 1);
  // iaddi vi03, vi03, 0x3      |  nop                            691
  vu.vi03 = vu.vi03 + 3;
  // nop                        |  clipw.xyz vf22, vf22           692
  cf = clip(vu.vf22, vu.vf22.w(), cf);
  // nop                        |  nop                            693

  // nop                        |  nop                            694

  // nop                        |  nop                            695

  // fcor vi01, 0xfffefb        |  nop                            696
  fcor(vu.vi01, 0xfffefb, cf);

  // BRANCH!
  // ibne vi00, vi01, L50       |  nop                            697
  bc = (vu.vi01 != 0);
  // fcand vi01, 0x4            |  nop                            698
  fcand(vu.vi01, 0x4, cf);

  if (bc) {
    goto L50;
  }

  // BRANCH!
  // ibne vi00, vi01, L51       |  nop                            699
  bc = (vu.vi01 != 0);
  // fcand vi01, 0x100          |  nop                            700
  fcand(vu.vi01, 0x100, cf);

  if (bc) {
    goto L51;
  }

  // BRANCH!
  // ibne vi00, vi01, L52       |  nop                            701
  bc = (vu.vi01 != 0);
  // nop                        |  nop                            702

  if (bc) {
    goto L52;
  }

  // sqi.xyzw vf24, vi04        |  nop                            703
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  nop                            704
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  nop                            705
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
L38:
  // move.xyzw vf24, vf25       |  nop                            706
  vu.vf24.move(Mask::xyzw, vu.vf25);
  // iaddi vi05, vi05, -0x1     |  nop                            707
  vu.vi05 = vu.vi05 + -1;
  // move.xyzw vf27, vf28       |  nop                            708
  vu.vf27.move(Mask::xyzw, vu.vf28);
  // BRANCH!
  // ibne vi00, vi05, L37       |  nop                            709
  bc = (vu.vi05 != 0);
  // move.xyzw vf21, vf22       |  nop                            710
  vu.vf21.move(Mask::xyzw, vu.vf22);
  if (bc) {
    goto L37;
  }

  // lq.xyzw vf24, 0(vi08)      |  nop                            711
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi08);
  // lq.xyzw vf27, 1(vi08)      |  nop                            712
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi08 + 1);
  // lq.xyzw vf21, 2(vi08)      |  nop                            713
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi08 + 2);
  // ior vi03, vi08, vi00       |  nop                            714
  vu.vi03 = vu.vi08;
  // sqi.xyzw vf24, vi04        |  nop                            715
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  nop                            716
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  nop                            717
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
  // BRANCH!
  // ibeq vi00, vi06, L43       |  nop                            718
  bc = (vu.vi06 == 0);
  // ior vi05, vi06, vi00       |  nop                            719
  vu.vi05 = vu.vi06;
  if (bc) {
    goto L43;
  }

  // ior vi04, vi03, vi00       |  nop                            720
  vu.vi04 = vu.vi03;
  // lq.xyzw vf21, 2(vi03)      |  nop                            721
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi03 + 2);
  // ior vi06, vi05, vi00       |  nop                            722
  vu.vi06 = vu.vi05;
  // lq.xyzw vf24, 0(vi03)      |  nop                            723
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi03);
  // lq.xyzw vf27, 1(vi03)      |  nop                            724
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi03 + 1);
  // iaddi vi03, vi03, 0x3      |  clipw.xyz vf21, vf21           725
  cf = clip(vu.vf21, vu.vf21.w(), cf);
  vu.vi03 = vu.vi03 + 3;
L39:
  // lq.xyzw vf22, 2(vi03)      |  nop                            726
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi03 + 2);
  // lq.xyzw vf25, 0(vi03)      |  nop                            727
  lq_buffer(Mask::xyzw, vu.vf25, vu.vi03);
  // lq.xyzw vf28, 1(vi03)      |  nop                            728
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi03 + 1);
  // iaddi vi03, vi03, 0x3      |  nop                            729
  vu.vi03 = vu.vi03 + 3;
  // nop                        |  clipw.xyz vf22, vf22           730
  cf = clip(vu.vf22, vu.vf22.w(), cf);
  // nop                        |  nop                            731

  // nop                        |  nop                            732

  // nop                        |  nop                            733

  // fcor vi01, 0xffff7d        |  nop                            734
  fcor(vu.vi01, 0xffff7d, cf);

  // BRANCH!
  // ibne vi00, vi01, L53       |  nop                            735
  bc = (vu.vi01 != 0);
  // fcand vi01, 0x2            |  nop                            736
  fcand(vu.vi01, 0x2, cf);

  if (bc) {
    goto L53;
  }

  // BRANCH!
  // ibne vi00, vi01, L54       |  nop                            737
  bc = (vu.vi01 != 0);
  // fcand vi01, 0x80           |  nop                            738
  fcand(vu.vi01, 0x80, cf);

  if (bc) {
    goto L54;
  }

  // BRANCH!
  // ibne vi00, vi01, L55       |  nop                            739
  bc = (vu.vi01 != 0);
  // nop                        |  nop                            740

  if (bc) {
    goto L55;
  }

  // sqi.xyzw vf24, vi04        |  nop                            741
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  nop                            742
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  nop                            743
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
L40:
  // move.xyzw vf24, vf25       |  nop                            744
  vu.vf24.move(Mask::xyzw, vu.vf25);
  // iaddi vi05, vi05, -0x1     |  nop                            745
  vu.vi05 = vu.vi05 + -1;
  // move.xyzw vf27, vf28       |  nop                            746
  vu.vf27.move(Mask::xyzw, vu.vf28);
  // BRANCH!
  // ibne vi00, vi05, L39       |  nop                            747
  bc = (vu.vi05 != 0);
  // move.xyzw vf21, vf22       |  nop                            748
  vu.vf21.move(Mask::xyzw, vu.vf22);
  if (bc) {
    goto L39;
  }

  // lq.xyzw vf24, 0(vi08)      |  nop                            749
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi08);
  // lq.xyzw vf27, 1(vi08)      |  nop                            750
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi08 + 1);
  // lq.xyzw vf21, 2(vi08)      |  nop                            751
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi08 + 2);
  // ior vi03, vi08, vi00       |  nop                            752
  vu.vi03 = vu.vi08;
  // sqi.xyzw vf24, vi04        |  nop                            753
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  nop                            754
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  nop                            755
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
  // BRANCH!
  // ibeq vi00, vi06, L43       |  nop                            756
  bc = (vu.vi06 == 0);
  // ior vi05, vi06, vi00       |  nop                            757
  vu.vi05 = vu.vi06;
  if (bc) {
    goto L43;
  }

  // ior vi04, vi03, vi00       |  nop                            758
  vu.vi04 = vu.vi03;
  // lq.xyzw vf21, 2(vi03)      |  nop                            759
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi03 + 2);
  // ior vi06, vi05, vi00       |  nop                            760
  vu.vi06 = vu.vi05;
  // lq.xyzw vf24, 0(vi03)      |  nop                            761
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi03);
  // lq.xyzw vf27, 1(vi03)      |  nop                            762
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi03 + 1);
  // iaddi vi03, vi03, 0x3      |  clipw.xyz vf21, vf21           763
  cf = clip(vu.vf21, vu.vf21.w(), cf);
  vu.vi03 = vu.vi03 + 3;
L41:
  // lq.xyzw vf22, 2(vi03)      |  nop                            764
  lq_buffer(Mask::xyzw, vu.vf22, vu.vi03 + 2);
  // lq.xyzw vf25, 0(vi03)      |  nop                            765
  lq_buffer(Mask::xyzw, vu.vf25, vu.vi03);
  // lq.xyzw vf28, 1(vi03)      |  nop                            766
  lq_buffer(Mask::xyzw, vu.vf28, vu.vi03 + 1);
  // iaddi vi03, vi03, 0x3      |  nop                            767
  vu.vi03 = vu.vi03 + 3;
  // nop                        |  clipw.xyz vf22, vf22           768
  cf = clip(vu.vf22, vu.vf22.w(), cf);
  // nop                        |  nop                            769

  // nop                        |  nop                            770

  // nop                        |  nop                            771

  // fcor vi01, 0xffffbe        |  nop                            772
  fcor(vu.vi01, 0xffffbe, cf);

  // BRANCH!
  // ibne vi00, vi01, L56       |  nop                            773
  bc = (vu.vi01 != 0);
  // fcand vi01, 0x1            |  nop                            774
  fcand(vu.vi01, 0x1, cf);

  if (bc) {
    goto L56;
  }

  // BRANCH!
  // ibne vi00, vi01, L57       |  nop                            775
  bc = (vu.vi01 != 0);
  // fcand vi01, 0x40           |  nop                            776
  fcand(vu.vi01, 0x40, cf);

  if (bc) {
    goto L57;
  }

  // BRANCH!
  // ibne vi00, vi01, L58       |  nop                            777
  bc = (vu.vi01 != 0);
  // nop                        |  nop                            778

  if (bc) {
    goto L58;
  }

  // sqi.xyzw vf24, vi04        |  nop                            779
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  nop                            780
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  nop                            781
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
L42:
  // move.xyzw vf24, vf25       |  nop                            782
  vu.vf24.move(Mask::xyzw, vu.vf25);
  // iaddi vi05, vi05, -0x1     |  nop                            783
  vu.vi05 = vu.vi05 + -1;
  // move.xyzw vf27, vf28       |  nop                            784
  vu.vf27.move(Mask::xyzw, vu.vf28);
  // BRANCH!
  // ibne vi00, vi05, L41       |  nop                            785
  bc = (vu.vi05 != 0);
  // move.xyzw vf21, vf22       |  nop                            786
  vu.vf21.move(Mask::xyzw, vu.vf22);
  if (bc) {
    goto L41;
  }

  // lq.xyzw vf24, 0(vi08)      |  nop                            787
  lq_buffer(Mask::xyzw, vu.vf24, vu.vi08);
  // lq.xyzw vf27, 1(vi08)      |  nop                            788
  lq_buffer(Mask::xyzw, vu.vf27, vu.vi08 + 1);
  // lq.xyzw vf21, 2(vi08)      |  nop                            789
  lq_buffer(Mask::xyzw, vu.vf21, vu.vi08 + 2);
  // ior vi03, vi08, vi00       |  nop                            790
  vu.vi03 = vu.vi08;
  // sqi.xyzw vf24, vi04        |  nop                            791
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  nop                            792
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  nop                            793
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
  // BRANCH!
  // ibeq vi00, vi06, L43       |  nop                            794
  bc = (vu.vi06 == 0);
  // ior vi05, vi06, vi00       |  nop                            795
  vu.vi05 = vu.vi06;
  if (bc) {
    goto L43;
  }

L43:
  // jr vi13                    |  nop                            796
  // ASSERT(false);
  return;
  // nop                        |  nop                            797

L44:
  // BRANCH!
  // b L34                      |  nop                            798
  bc = true;
  // iaddi vi06, vi06, -0x1     |  nop                            799
  vu.vi06 = vu.vi06 + -1;
  if (bc) {
    goto L34;
  }

L45:
  // sqi.xyzw vf24, vi04        |  sub.xyzw vf23, vf22, vf21      800
  vu.vf23.sub(Mask::xyzw, vu.vf22, vu.vf21);
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  sub.xyzw vf26, vf25, vf24      801
  vu.vf26.sub(Mask::xyzw, vu.vf25, vu.vf24);
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  sub.xyzw vf29, vf28, vf27      802
  vu.vf29.sub(Mask::xyzw, vu.vf28, vu.vf27);
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
  // BRANCH!
  // ibeq vi03, vi04, L34       |  nop                            803
  bc = (vu.vi03 == vu.vi04);
  // nop                        |  addz.w vf30, vf23, vf23        804
  vu.vf30.add(Mask::w, vu.vf23, vu.vf23.z());
  if (bc) {
    goto L34;
  }

  // nop                        |  addw.z vf30, vf21, vf21        805
  vu.vf30.add(Mask::z, vu.vf21, vu.vf21.w());
  // div Q, vf30.z, vf30.w      |  nop                            806
  vu.Q = vu.vf30.z() / vu.vf30.w();
  // waitq                      |  nop                            807

  // nop                        |  mul.xyzw vf26, vf26, Q         808
  vu.vf26.mul(Mask::xyzw, vu.vf26, vu.Q);
  // nop                        |  mul.xyzw vf29, vf29, Q         809
  vu.vf29.mul(Mask::xyzw, vu.vf29, vu.Q);
  // nop                        |  mul.xyzw vf23, vf23, Q         810
  vu.vf23.mul(Mask::xyzw, vu.vf23, vu.Q);
  // nop                        |  sub.xyzw vf26, vf24, vf26      811
  vu.vf26.sub(Mask::xyzw, vu.vf24, vu.vf26);
  // nop                        |  sub.xyzw vf29, vf27, vf29      812
  vu.vf29.sub(Mask::xyzw, vu.vf27, vu.vf29);
  // nop                        |  sub.xyzw vf23, vf21, vf23      813
  vu.vf23.sub(Mask::xyzw, vu.vf21, vu.vf23);
  // iaddi vi06, vi06, 0x1      |  nop                            814
  vu.vi06 = vu.vi06 + 1;
  // sqi.xyzw vf26, vi04        |  nop                            815
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi04++);
  // sqi.xyzw vf29, vi04        |  nop                            816
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi04++);
  // BRANCH!
  // b L34                      |  nop                            817
  bc = true;
  // sqi.xyzw vf23, vi04        |  nop                            818
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi04++);
  if (bc) {
    goto L34;
  }

L46:
  // nop                        |  sub.xyzw vf23, vf21, vf22      819
  vu.vf23.sub(Mask::xyzw, vu.vf21, vu.vf22);
  // nop                        |  sub.xyzw vf26, vf24, vf25      820
  vu.vf26.sub(Mask::xyzw, vu.vf24, vu.vf25);
  // nop                        |  sub.xyzw vf29, vf27, vf28      821
  vu.vf29.sub(Mask::xyzw, vu.vf27, vu.vf28);
  // nop                        |  addz.w vf30, vf23, vf23        822
  vu.vf30.add(Mask::w, vu.vf23, vu.vf23.z());
  // nop                        |  addw.z vf30, vf22, vf22        823
  vu.vf30.add(Mask::z, vu.vf22, vu.vf22.w());
  // div Q, vf30.z, vf30.w      |  nop                            824
  vu.Q = vu.vf30.z() / vu.vf30.w();
  // waitq                      |  nop                            825
  // ASSERT(false);
  // nop                        |  mul.xyzw vf26, vf26, Q         826
  vu.vf26.mul(Mask::xyzw, vu.vf26, vu.Q);
  // nop                        |  mul.xyzw vf29, vf29, Q         827
  vu.vf29.mul(Mask::xyzw, vu.vf29, vu.Q);
  // nop                        |  mul.xyzw vf23, vf23, Q         828
  vu.vf23.mul(Mask::xyzw, vu.vf23, vu.Q);
  // nop                        |  sub.xyzw vf26, vf25, vf26      829
  vu.vf26.sub(Mask::xyzw, vu.vf25, vu.vf26);
  // nop                        |  sub.xyzw vf29, vf28, vf29      830
  vu.vf29.sub(Mask::xyzw, vu.vf28, vu.vf29);
  // nop                        |  sub.xyzw vf23, vf22, vf23      831
  vu.vf23.sub(Mask::xyzw, vu.vf22, vu.vf23);
  // sqi.xyzw vf26, vi04        |  nop                            832
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi04++);
  // sqi.xyzw vf29, vi04        |  nop                            833
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi04++);
  // BRANCH!
  // b L34                      |  nop                            834
  bc = true;
  // sqi.xyzw vf23, vi04        |  nop                            835
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi04++);
  if (bc) {
    goto L34;
  }

L47:
  // BRANCH!
  // b L36                      |  nop                            836
  bc = true;
  // iaddi vi06, vi06, -0x1     |  nop                            837
  vu.vi06 = vu.vi06 + -1;
  if (bc) {
    goto L36;
  }

L48:
  // sqi.xyzw vf24, vi04        |  sub.xyzw vf23, vf22, vf21      838
  vu.vf23.sub(Mask::xyzw, vu.vf22, vu.vf21);
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  sub.xyzw vf26, vf25, vf24      839
  vu.vf26.sub(Mask::xyzw, vu.vf25, vu.vf24);
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  sub.xyzw vf29, vf28, vf27      840
  vu.vf29.sub(Mask::xyzw, vu.vf28, vu.vf27);
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
  // BRANCH!
  // ibeq vi03, vi04, L36       |  nop                            841
  bc = (vu.vi03 == vu.vi04);
  // nop                        |  addy.w vf30, vf23, vf23        842
  vu.vf30.add(Mask::w, vu.vf23, vu.vf23.y());
  if (bc) {
    goto L36;
  }

  // nop                        |  addw.y vf30, vf21, vf21        843
  vu.vf30.add(Mask::y, vu.vf21, vu.vf21.w());
  // div Q, vf30.y, vf30.w      |  nop                            844
  vu.Q = vu.vf30.y() / vu.vf30.w();
  // waitq                      |  nop                            845
  // ASSERT(false);
  // nop                        |  mul.xyzw vf26, vf26, Q         846
  vu.vf26.mul(Mask::xyzw, vu.vf26, vu.Q);
  // nop                        |  mul.xyzw vf29, vf29, Q         847
  vu.vf29.mul(Mask::xyzw, vu.vf29, vu.Q);
  // nop                        |  mul.xyzw vf23, vf23, Q         848
  vu.vf23.mul(Mask::xyzw, vu.vf23, vu.Q);
  // nop                        |  sub.xyzw vf26, vf24, vf26      849
  vu.vf26.sub(Mask::xyzw, vu.vf24, vu.vf26);
  // nop                        |  sub.xyzw vf29, vf27, vf29      850
  vu.vf29.sub(Mask::xyzw, vu.vf27, vu.vf29);
  // nop                        |  sub.xyzw vf23, vf21, vf23      851
  vu.vf23.sub(Mask::xyzw, vu.vf21, vu.vf23);
  // iaddi vi06, vi06, 0x1      |  nop                            852
  vu.vi06 = vu.vi06 + 1;
  // sqi.xyzw vf26, vi04        |  nop                            853
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi04++);
  // sqi.xyzw vf29, vi04        |  nop                            854
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi04++);
  // BRANCH!
  // b L36                      |  nop                            855
  bc = true;
  // sqi.xyzw vf23, vi04        |  nop                            856
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi04++);
  if (bc) {
    goto L36;
  }

L49:
  // nop                        |  sub.xyzw vf23, vf21, vf22      857
  vu.vf23.sub(Mask::xyzw, vu.vf21, vu.vf22);
  // nop                        |  sub.xyzw vf26, vf24, vf25      858
  vu.vf26.sub(Mask::xyzw, vu.vf24, vu.vf25);
  // nop                        |  sub.xyzw vf29, vf27, vf28      859
  vu.vf29.sub(Mask::xyzw, vu.vf27, vu.vf28);
  // nop                        |  addy.w vf30, vf23, vf23        860
  vu.vf30.add(Mask::w, vu.vf23, vu.vf23.y());
  // nop                        |  addw.y vf30, vf22, vf22        861
  vu.vf30.add(Mask::y, vu.vf22, vu.vf22.w());
  // div Q, vf30.y, vf30.w      |  nop                            862
  vu.Q = vu.vf30.y() / vu.vf30.w();
  // waitq                      |  nop                            863
  // ASSERT(false);
  // nop                        |  mul.xyzw vf26, vf26, Q         864
  vu.vf26.mul(Mask::xyzw, vu.vf26, vu.Q);
  // nop                        |  mul.xyzw vf29, vf29, Q         865
  vu.vf29.mul(Mask::xyzw, vu.vf29, vu.Q);
  // nop                        |  mul.xyzw vf23, vf23, Q         866
  vu.vf23.mul(Mask::xyzw, vu.vf23, vu.Q);
  // nop                        |  sub.xyzw vf26, vf25, vf26      867
  vu.vf26.sub(Mask::xyzw, vu.vf25, vu.vf26);
  // nop                        |  sub.xyzw vf29, vf28, vf29      868
  vu.vf29.sub(Mask::xyzw, vu.vf28, vu.vf29);
  // nop                        |  sub.xyzw vf23, vf22, vf23      869
  vu.vf23.sub(Mask::xyzw, vu.vf22, vu.vf23);
  // sqi.xyzw vf26, vi04        |  nop                            870
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi04++);
  // sqi.xyzw vf29, vi04        |  nop                            871
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi04++);
  // BRANCH!
  // b L36                      |  nop                            872
  bc = true;
  // sqi.xyzw vf23, vi04        |  nop                            873
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi04++);
  if (bc) {
    goto L36;
  }

L50:
  // BRANCH!
  // b L38                      |  nop                            874
  bc = true;
  // iaddi vi06, vi06, -0x1     |  nop                            875
  vu.vi06 = vu.vi06 + -1;
  if (bc) {
    goto L38;
  }

L51:
  // sqi.xyzw vf24, vi04        |  sub.xyzw vf23, vf22, vf21      876
  vu.vf23.sub(Mask::xyzw, vu.vf22, vu.vf21);
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  sub.xyzw vf26, vf25, vf24      877
  vu.vf26.sub(Mask::xyzw, vu.vf25, vu.vf24);
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  sub.xyzw vf29, vf28, vf27      878
  vu.vf29.sub(Mask::xyzw, vu.vf28, vu.vf27);
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
  // BRANCH!
  // ibeq vi03, vi04, L38       |  nop                            879
  bc = (vu.vi03 == vu.vi04);
  // nop                        |  suby.w vf30, vf23, vf23        880
  vu.vf30.sub(Mask::w, vu.vf23, vu.vf23.y());
  if (bc) {
    goto L38;
  }

  // nop                        |  subw.y vf30, vf21, vf21        881
  vu.vf30.sub(Mask::y, vu.vf21, vu.vf21.w());
  // div Q, vf30.y, vf30.w      |  nop                            882
  vu.Q = vu.vf30.y() / vu.vf30.w();
  // waitq                      |  nop                            883
  // ASSERT(false);
  // nop                        |  mul.xyzw vf26, vf26, Q         884
  vu.vf26.mul(Mask::xyzw, vu.vf26, vu.Q);
  // nop                        |  mul.xyzw vf29, vf29, Q         885
  vu.vf29.mul(Mask::xyzw, vu.vf29, vu.Q);
  // nop                        |  mul.xyzw vf23, vf23, Q         886
  vu.vf23.mul(Mask::xyzw, vu.vf23, vu.Q);
  // nop                        |  add.xyzw vf26, vf24, vf26      887
  vu.vf26.add(Mask::xyzw, vu.vf24, vu.vf26);
  // nop                        |  add.xyzw vf29, vf27, vf29      888
  vu.vf29.add(Mask::xyzw, vu.vf27, vu.vf29);
  // nop                        |  add.xyzw vf23, vf21, vf23      889
  vu.vf23.add(Mask::xyzw, vu.vf21, vu.vf23);
  // iaddi vi06, vi06, 0x1      |  nop                            890
  vu.vi06 = vu.vi06 + 1;
  // sqi.xyzw vf26, vi04        |  nop                            891
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi04++);
  // sqi.xyzw vf29, vi04        |  nop                            892
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi04++);
  // BRANCH!
  // b L38                      |  nop                            893
  bc = true;
  // sqi.xyzw vf23, vi04        |  nop                            894
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi04++);
  if (bc) {
    goto L38;
  }

L52:
  // nop                        |  sub.xyzw vf23, vf21, vf22      895
  vu.vf23.sub(Mask::xyzw, vu.vf21, vu.vf22);
  // nop                        |  sub.xyzw vf26, vf24, vf25      896
  vu.vf26.sub(Mask::xyzw, vu.vf24, vu.vf25);
  // nop                        |  sub.xyzw vf29, vf27, vf28      897
  vu.vf29.sub(Mask::xyzw, vu.vf27, vu.vf28);
  // nop                        |  suby.w vf30, vf23, vf23        898
  vu.vf30.sub(Mask::w, vu.vf23, vu.vf23.y());
  // nop                        |  subw.y vf30, vf22, vf22        899
  vu.vf30.sub(Mask::y, vu.vf22, vu.vf22.w());
  // div Q, vf30.y, vf30.w      |  nop                            900
  vu.Q = vu.vf30.y() / vu.vf30.w();
  // waitq                      |  nop                            901
  // ASSERT(false);
  // nop                        |  mul.xyzw vf26, vf26, Q         902
  vu.vf26.mul(Mask::xyzw, vu.vf26, vu.Q);
  // nop                        |  mul.xyzw vf29, vf29, Q         903
  vu.vf29.mul(Mask::xyzw, vu.vf29, vu.Q);
  // nop                        |  mul.xyzw vf23, vf23, Q         904
  vu.vf23.mul(Mask::xyzw, vu.vf23, vu.Q);
  // nop                        |  add.xyzw vf26, vf25, vf26      905
  vu.vf26.add(Mask::xyzw, vu.vf25, vu.vf26);
  // nop                        |  add.xyzw vf29, vf28, vf29      906
  vu.vf29.add(Mask::xyzw, vu.vf28, vu.vf29);
  // nop                        |  add.xyzw vf23, vf22, vf23      907
  vu.vf23.add(Mask::xyzw, vu.vf22, vu.vf23);
  // sqi.xyzw vf26, vi04        |  nop                            908
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi04++);
  // sqi.xyzw vf29, vi04        |  nop                            909
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi04++);
  // BRANCH!
  // b L38                      |  nop                            910
  bc = true;
  // sqi.xyzw vf23, vi04        |  nop                            911
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi04++);
  if (bc) {
    goto L38;
  }

L53:
  // BRANCH!
  // b L40                      |  nop                            912
  bc = true;
  // iaddi vi06, vi06, -0x1     |  nop                            913
  vu.vi06 = vu.vi06 + -1;
  if (bc) {
    goto L40;
  }

L54:
  // sqi.xyzw vf24, vi04        |  sub.xyzw vf23, vf22, vf21      914
  vu.vf23.sub(Mask::xyzw, vu.vf22, vu.vf21);
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  sub.xyzw vf26, vf25, vf24      915
  vu.vf26.sub(Mask::xyzw, vu.vf25, vu.vf24);
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  sub.xyzw vf29, vf28, vf27      916
  vu.vf29.sub(Mask::xyzw, vu.vf28, vu.vf27);
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
  // BRANCH!
  // ibeq vi03, vi04, L40       |  nop                            917
  bc = (vu.vi03 == vu.vi04);
  // nop                        |  addx.w vf30, vf23, vf23        918
  vu.vf30.add(Mask::w, vu.vf23, vu.vf23.x());
  if (bc) {
    goto L40;
  }

  // nop                        |  addw.x vf30, vf21, vf21        919
  vu.vf30.add(Mask::x, vu.vf21, vu.vf21.w());
  // div Q, vf30.x, vf30.w      |  nop                            920
  vu.Q = vu.vf30.x() / vu.vf30.w();
  // waitq                      |  nop                            921
  // ASSERT(false);
  // nop                        |  mul.xyzw vf26, vf26, Q         922
  vu.vf26.mul(Mask::xyzw, vu.vf26, vu.Q);
  // nop                        |  mul.xyzw vf29, vf29, Q         923
  vu.vf29.mul(Mask::xyzw, vu.vf29, vu.Q);
  // nop                        |  mul.xyzw vf23, vf23, Q         924
  vu.vf23.mul(Mask::xyzw, vu.vf23, vu.Q);
  // nop                        |  sub.xyzw vf26, vf24, vf26      925
  vu.vf26.sub(Mask::xyzw, vu.vf24, vu.vf26);
  // nop                        |  sub.xyzw vf29, vf27, vf29      926
  vu.vf29.sub(Mask::xyzw, vu.vf27, vu.vf29);
  // nop                        |  sub.xyzw vf23, vf21, vf23      927
  vu.vf23.sub(Mask::xyzw, vu.vf21, vu.vf23);
  // iaddi vi06, vi06, 0x1      |  nop                            928
  vu.vi06 = vu.vi06 + 1;
  // sqi.xyzw vf26, vi04        |  nop                            929
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi04++);
  // sqi.xyzw vf29, vi04        |  nop                            930
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi04++);
  // BRANCH!
  // b L40                      |  nop                            931
  bc = true;
  // sqi.xyzw vf23, vi04        |  nop                            932
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi04++);
  if (bc) {
    goto L40;
  }

L55:
  // nop                        |  sub.xyzw vf23, vf21, vf22      933
  vu.vf23.sub(Mask::xyzw, vu.vf21, vu.vf22);
  // nop                        |  sub.xyzw vf26, vf24, vf25      934
  vu.vf26.sub(Mask::xyzw, vu.vf24, vu.vf25);
  // nop                        |  sub.xyzw vf29, vf27, vf28      935
  vu.vf29.sub(Mask::xyzw, vu.vf27, vu.vf28);
  // nop                        |  addx.w vf30, vf23, vf23        936
  vu.vf30.add(Mask::w, vu.vf23, vu.vf23.x());
  // nop                        |  addw.x vf30, vf22, vf22        937
  vu.vf30.add(Mask::x, vu.vf22, vu.vf22.w());
  // div Q, vf30.x, vf30.w      |  nop                            938
  vu.Q = vu.vf30.x() / vu.vf30.w();
  // waitq                      |  nop                            939
  // ASSERT(false);
  // nop                        |  mul.xyzw vf26, vf26, Q         940
  vu.vf26.mul(Mask::xyzw, vu.vf26, vu.Q);
  // nop                        |  mul.xyzw vf29, vf29, Q         941
  vu.vf29.mul(Mask::xyzw, vu.vf29, vu.Q);
  // nop                        |  mul.xyzw vf23, vf23, Q         942
  vu.vf23.mul(Mask::xyzw, vu.vf23, vu.Q);
  // nop                        |  sub.xyzw vf26, vf25, vf26      943
  vu.vf26.sub(Mask::xyzw, vu.vf25, vu.vf26);
  // nop                        |  sub.xyzw vf29, vf28, vf29      944
  vu.vf29.sub(Mask::xyzw, vu.vf28, vu.vf29);
  // nop                        |  sub.xyzw vf23, vf22, vf23      945
  vu.vf23.sub(Mask::xyzw, vu.vf22, vu.vf23);
  // sqi.xyzw vf26, vi04        |  nop                            946
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi04++);
  // sqi.xyzw vf29, vi04        |  nop                            947
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi04++);
  // BRANCH!
  // b L40                      |  nop                            948
  bc = true;
  // sqi.xyzw vf23, vi04        |  nop                            949
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi04++);
  if (bc) {
    goto L40;
  }

L56:
  // BRANCH!
  // b L42                      |  nop                            950
  bc = true;
  // iaddi vi06, vi06, -0x1     |  nop                            951
  vu.vi06 = vu.vi06 + -1;
  if (bc) {
    goto L42;
  }

L57:
  // sqi.xyzw vf24, vi04        |  sub.xyzw vf23, vf22, vf21      952
  vu.vf23.sub(Mask::xyzw, vu.vf22, vu.vf21);
  sq_buffer(Mask::xyzw, vu.vf24, vu.vi04++);
  // sqi.xyzw vf27, vi04        |  sub.xyzw vf26, vf25, vf24      953
  vu.vf26.sub(Mask::xyzw, vu.vf25, vu.vf24);
  sq_buffer(Mask::xyzw, vu.vf27, vu.vi04++);
  // sqi.xyzw vf21, vi04        |  sub.xyzw vf29, vf28, vf27      954
  vu.vf29.sub(Mask::xyzw, vu.vf28, vu.vf27);
  sq_buffer(Mask::xyzw, vu.vf21, vu.vi04++);
  // BRANCH!
  // ibeq vi03, vi04, L42       |  nop                            955
  bc = (vu.vi03 == vu.vi04);
  // nop                        |  subx.w vf30, vf23, vf23        956
  vu.vf30.sub(Mask::w, vu.vf23, vu.vf23.x());
  if (bc) {
    goto L42;
  }

  // nop                        |  subw.x vf30, vf21, vf21        957
  vu.vf30.sub(Mask::x, vu.vf21, vu.vf21.w());
  // div Q, vf30.x, vf30.w      |  nop                            958
  vu.Q = vu.vf30.x() / vu.vf30.w();
  // waitq                      |  nop                            959
  // ASSERT(false);
  // nop                        |  mul.xyzw vf26, vf26, Q         960
  vu.vf26.mul(Mask::xyzw, vu.vf26, vu.Q);
  // nop                        |  mul.xyzw vf29, vf29, Q         961
  vu.vf29.mul(Mask::xyzw, vu.vf29, vu.Q);
  // nop                        |  mul.xyzw vf23, vf23, Q         962
  vu.vf23.mul(Mask::xyzw, vu.vf23, vu.Q);
  // nop                        |  add.xyzw vf26, vf24, vf26      963
  vu.vf26.add(Mask::xyzw, vu.vf24, vu.vf26);
  // nop                        |  add.xyzw vf29, vf27, vf29      964
  vu.vf29.add(Mask::xyzw, vu.vf27, vu.vf29);
  // nop                        |  add.xyzw vf23, vf21, vf23      965
  vu.vf23.add(Mask::xyzw, vu.vf21, vu.vf23);
  // iaddi vi06, vi06, 0x1      |  nop                            966
  vu.vi06 = vu.vi06 + 1;
  // sqi.xyzw vf26, vi04        |  nop                            967
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi04++);
  // sqi.xyzw vf29, vi04        |  nop                            968
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi04++);
  // BRANCH!
  // b L42                      |  nop                            969
  bc = true;
  // sqi.xyzw vf23, vi04        |  nop                            970
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi04++);
  if (bc) {
    goto L42;
  }

L58:
  // nop                        |  sub.xyzw vf23, vf21, vf22      971
  vu.vf23.sub(Mask::xyzw, vu.vf21, vu.vf22);
  // nop                        |  sub.xyzw vf26, vf24, vf25      972
  vu.vf26.sub(Mask::xyzw, vu.vf24, vu.vf25);
  // nop                        |  sub.xyzw vf29, vf27, vf28      973
  vu.vf29.sub(Mask::xyzw, vu.vf27, vu.vf28);
  // nop                        |  subx.w vf30, vf23, vf23        974
  vu.vf30.sub(Mask::w, vu.vf23, vu.vf23.x());
  // nop                        |  subw.x vf30, vf22, vf22        975
  vu.vf30.sub(Mask::x, vu.vf22, vu.vf22.w());
  // div Q, vf30.x, vf30.w      |  nop                            976
  vu.Q = vu.vf30.x() / vu.vf30.w();
  // waitq                      |  nop                            977
  // ASSERT(false);
  // nop                        |  mul.xyzw vf26, vf26, Q         978
  vu.vf26.mul(Mask::xyzw, vu.vf26, vu.Q);
  // nop                        |  mul.xyzw vf29, vf29, Q         979
  vu.vf29.mul(Mask::xyzw, vu.vf29, vu.Q);
  // nop                        |  mul.xyzw vf23, vf23, Q         980
  vu.vf23.mul(Mask::xyzw, vu.vf23, vu.Q);
  // nop                        |  add.xyzw vf26, vf25, vf26      981
  vu.vf26.add(Mask::xyzw, vu.vf25, vu.vf26);
  // nop                        |  add.xyzw vf29, vf28, vf29      982
  vu.vf29.add(Mask::xyzw, vu.vf28, vu.vf29);
  // nop                        |  add.xyzw vf23, vf22, vf23      983
  vu.vf23.add(Mask::xyzw, vu.vf22, vu.vf23);
  // sqi.xyzw vf26, vi04        |  nop                            984
  sq_buffer(Mask::xyzw, vu.vf26, vu.vi04++);
  // sqi.xyzw vf29, vi04        |  nop                            985
  sq_buffer(Mask::xyzw, vu.vf29, vu.vi04++);
  // BRANCH!
  // b L42                      |  nop                            986
  bc = true;
  // sqi.xyzw vf23, vi04        |  nop                            987
  sq_buffer(Mask::xyzw, vu.vf23, vu.vi04++);
  if (bc) {
    goto L42;
  }
}