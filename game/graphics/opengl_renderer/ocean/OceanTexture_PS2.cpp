#include "OceanTexture.h"

void OceanTexture::run_L1(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  //  L1:
  //  lq.xyzw vf14_startx, 988(vi00)    |  maxw.xyzw vf01_ones, vf00, vf00
  vu.startx = Vf(m_texture_constants.start);
  //  lq.xyzw vf02_offset, 989(vi00)
  //  lq.xyzw vf03_tbuf, 986(vi00)
  //  lq.xyzw vf04_dbuf, 987(vi00)
  //  lq.xyzw vf05_giftag, 985(vi00)
  //  lq.xyzw vf06_cam_nrm, 991(vi00)
  //  lq.xyzw vf07_constants, 990(vi00)
  //  iaddiu vi11_0x80, vi00, 0x80
  //  mtir vi08_tptr, vf03_tbuf.x
  vu.tptr = get_tbuf();
  //  mtir vi09_tbase, vf03_tbuf.x
  vu.tbase = get_tbuf();
  //  mr32.xyzw vf03_tbuf, vf03_tbuf
  swap_tbuf();
  //  xtop vi05_in_ptr
  vu.in_ptr = swap_vu_upload_buffers();

  //  mtir vi06_dbuf_write, vf04_dbuf.x
  vu.dbuf_write = get_dbuf();
  //  bal vi12_ra, L3
  //  mr32.xyzw vf04_dbuf, vf04_dbuf
  swap_dbuf();
  run_L3();

  //  mtir vi06_dbuf_write, vf04_dbuf.x
  vu.dbuf_write = get_dbuf();
  //  bal vi12_ra, L3
  //  mr32.xyzw vf04_dbuf, vf04_dbuf
  swap_dbuf();
  run_L3();

  //  mtir vi03_dbuf_read_a, vf04_dbuf.x
  vu.dbuf_read_a = get_dbuf();
  //  bal vi12_ra, L5
  //  mtir vi04_dbuf_read_b, vf04_dbuf.y
  vu.dbuf_read_b = get_dbuf_other();
  run_L5(render_state, prof);

  //  mtir vi06_dbuf_write, vf04_dbuf.x
  vu.dbuf_write = get_dbuf();
  //  bal vi12_ra, L3
  //  mr32.xyzw vf04_dbuf, vf04_dbuf
  swap_dbuf();
  run_L3();

  //  mtir vi03_dbuf_read_a, vf04_dbuf.x
  vu.dbuf_read_a = get_dbuf();
  //  bal vi12_ra, L5
  //  mtir vi04_dbuf_read_b, vf04_dbuf.y
  vu.dbuf_read_b = get_dbuf_other();
  run_L5(render_state, prof);

  //  nop                     :e
  //  nop
}

void OceanTexture::run_L2(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  //  L2:
  //  xtop vi05_in_ptr
  vu.in_ptr = swap_vu_upload_buffers();
  //  mtir vi06_dbuf_write, vf04_dbuf.x
  vu.dbuf_write = get_dbuf();
  //  bal vi12_ra, L3
  //  mr32.xyzw vf04_dbuf, vf04_dbuf
  swap_dbuf();
  run_L3();

  //  mtir vi03_dbuf_read_a, vf04_dbuf.x
  vu.dbuf_read_a = get_dbuf();
  //  bal vi12_ra, L5
  //  mtir vi04_dbuf_read_b, vf04_dbuf.y
  vu.dbuf_read_b = get_dbuf_other();
  run_L5(render_state, prof);

  //  mtir vi06_dbuf_write, vf04_dbuf.x
  vu.dbuf_write = get_dbuf();
  //  bal vi12_ra, L3
  //  mr32.xyzw vf04_dbuf, vf04_dbuf
  swap_dbuf();
  run_L3();

  //  mtir vi03_dbuf_read_a, vf04_dbuf.x
  vu.dbuf_read_a = get_dbuf();
  //  bal vi12_ra, L5
  //  mtir vi04_dbuf_read_b, vf04_dbuf.y
  vu.dbuf_read_b = get_dbuf_other();
  run_L5(render_state, prof);

  //  mtir vi06_dbuf_write, vf04_dbuf.x
  vu.dbuf_write = get_dbuf();
  //  bal vi12_ra, L3
  //  mr32.xyzw vf04_dbuf, vf04_dbuf
  swap_dbuf();
  run_L3();

  //  mtir vi03_dbuf_read_a, vf04_dbuf.x
  vu.dbuf_read_a = get_dbuf();
  //  bal vi12_ra, L5
  //  mtir vi04_dbuf_read_b, vf04_dbuf.y
  vu.dbuf_read_b = get_dbuf_other();
  run_L5(render_state, prof);

  //  nop                     :e
  //  nop
}

namespace {
void lq_buffer(Mask mask, Vf& dest, Vf* src) {
  for (int i = 0; i < 4; i++) {
    if ((u64)mask & (1 << i)) {
      dest.data[i] = src->data[i];
    }
  }
}

void sq_buffer(Mask mask, const Vf& src, Vf* dest) {
  for (int i = 0; i < 4; i++) {
    if ((u64)mask & (1 << i)) {
      dest->data[i] = src.data[i];
    }
  }
}

void sq_buffer_giftag(const u8* src, Vf* dest) {
  memcpy(dest, src, 16);
}
}  // namespace

void OceanTexture::run_L3() {
  Vf base_pos;  // vf15
  u16 loop_idx;

  Vf vtx0;  // vf16
  Vf vtx1;  // vf17
  Vf vtx2;  // vf18
  Vf vtx3;  // vf19

  Vf res0;  // vf20
  Vf res1;  // vf21
  Vf res2;  // vf22
  Vf res3;  // vf23

  Vf nrm0;  // vf24
  Vf nrm1;  // vf25
  Vf nrm2;  // vf26

  Vf reflect;  // vf27

  Vf cout0;  // vf28
  Vf cout1;  // vf29
  Vf cout2;  // vf30
  Vf cout3;  // vf31

  Accumulator acc;
  const Vf ones(1, 1, 1, 1);
  const Vf vf00(0, 0, 0, 1);
  const u16 vi11 = 0x80;
  bool bc;

  // clang-format off
  // L3:
  // ior vi07, vi06, vi00       |  nop                            56
  vu.dbuf_write_base = vu.dbuf_write;
  // move.xyzw vf15, vf14       |  nop                            57
  base_pos.move(Mask::xyzw, vu.startx);
  // iaddi vi01, vi00, 0x8      |  nop                            58
  loop_idx = 8;
  // lq.xyzw vf24, 1(vi05)      |  mulw.xyzw vf20, vf15, vf00     59 (?? what are they doing here)
  res0.mul(Mask::xyzw, base_pos, 1.f);   lq_buffer(Mask::xyzw, nrm0, vu.in_ptr + 1);
  // lq.xyzw vf25, 3(vi05)      |  mulw.xyzw vf21, vf15, vf00     60
  res1.mul(Mask::xyzw, base_pos, 1.f);   lq_buffer(Mask::xyzw, nrm1, vu.in_ptr + 3);
  // lq.xyzw vf26, 5(vi05)      |  mulw.xyzw vf22, vf15, vf00     61
  res2.mul(Mask::xyzw, base_pos, 1.f);   lq_buffer(Mask::xyzw, nrm2, vu.in_ptr + 5);
  // nop                        |  mulw.xyzw vf23, vf15, vf00     62
  res3.mul(Mask::xyzw, base_pos, 1.f);
  // nop                        |  mulax.xyzw ACC, vf24, vf06     63
  acc.mula(Mask::xyzw, nrm0, m_texture_constants.cam_nrm.x());
  // nop                        |  madday.xyzw ACC, vf25, vf06    64
  acc.madda(Mask::xyzw, nrm1, m_texture_constants.cam_nrm.y());
  // nop                        |  maddz.xyzw vf27, vf26, vf06    65
  acc.madd(Mask::xyzw, reflect, nrm2, m_texture_constants.cam_nrm.z());
  // nop                        |  addx.x vf21, vf21, vf02        66
  res1.add(Mask::x, res1, m_texture_constants.offsets.x());
  // nop                        |  addy.x vf22, vf22, vf02        67
  res2.add(Mask::x, res2, m_texture_constants.offsets.y());
  L4:
  // nop                        |  addz.x vf23, vf23, vf02        68
  res3.add(Mask::x, res3, m_texture_constants.offsets.z());
  // nop                        |  addw.x vf15, vf15, vf02        69
  base_pos.add(Mask::x, base_pos, m_texture_constants.offsets.w());
  // sq.xyzw vf20, 2(vi06)      |  mulx.x vf28, vf01, vf24        70
  cout0.mul(Mask::x, ones, nrm0.x());   sq_buffer(Mask::xyzw, res0, vu.dbuf_write + 2);
  // sq.xyzw vf21, 5(vi06)      |  muly.x vf29, vf01, vf24        71
  cout1.mul(Mask::x, ones, nrm0.y());   sq_buffer(Mask::xyzw, res1, vu.dbuf_write + 5);
  // sq.xyzw vf22, 8(vi06)      |  mulz.x vf30, vf01, vf24        72
  cout2.mul(Mask::x, ones, nrm0.z());   sq_buffer(Mask::xyzw, res2, vu.dbuf_write + 8);
  // sq.xyzw vf23, 11(vi06)     |  mulw.x vf31, vf01, vf24        73
  cout3.mul(Mask::x, ones, nrm0.w());   sq_buffer(Mask::xyzw, res3, vu.dbuf_write + 11);
  // lq.xyzw vf16, 0(vi05)      |  mulx.y vf28, vf01, vf25        74
  cout0.mul(Mask::y, ones, nrm1.x());   lq_buffer(Mask::xyzw, vtx0, vu.in_ptr);
  // lq.xyzw vf17, 2(vi05)      |  muly.y vf29, vf01, vf25        75
  cout1.mul(Mask::y, ones, nrm1.y());   lq_buffer(Mask::xyzw, vtx1, vu.in_ptr + 2);
  // lq.xyzw vf18, 4(vi05)      |  mulz.y vf30, vf01, vf25        76
  cout2.mul(Mask::y, ones, nrm1.z());   lq_buffer(Mask::xyzw, vtx2, vu.in_ptr + 4);
  // lq.xyzw vf19, 6(vi05)      |  mulw.y vf31, vf01, vf25        77
  cout3.mul(Mask::y, ones, nrm1.w());   lq_buffer(Mask::xyzw, vtx3, vu.in_ptr + 6);
  // iaddi vi05, vi05, 0x8      |  mulx.xy vf28, vf28, vf27       78
  cout0.mul(Mask::xy, cout0, reflect.x());   vu.in_ptr = vu.in_ptr + 8;
  // nop                        |  muly.xy vf29, vf29, vf27       79
  cout1.mul(Mask::xy, cout1, reflect.y());
  // nop                        |  mulz.xy vf30, vf30, vf27       80
  cout2.mul(Mask::xy, cout2, reflect.z());
  // nop                        |  mulw.xy vf31, vf31, vf27       81
  cout3.mul(Mask::xy, cout3, reflect.w());
  // nop                        |  mulw.xy vf28, vf28, vf16       82
  cout0.mul(Mask::xy, cout0, vtx0.w());
  // nop                        |  mulw.xy vf29, vf29, vf17       83
  cout1.mul(Mask::xy, cout1, vtx1.w());
  // nop                        |  mulw.xy vf30, vf30, vf18       84
  cout2.mul(Mask::xy, cout2, vtx2.w());
  // nop                        |  mulw.xy vf31, vf31, vf19       85
  cout3.mul(Mask::xy, cout3, vtx3.w());
  // nop                        |  ftoi0.xyzw vf16, vf16          86
  vtx0.ftoi0(Mask::xyzw, vtx0);
  // nop                        |  ftoi0.xyzw vf17, vf17          87
  vtx1.ftoi0(Mask::xyzw, vtx1);
  // nop                        |  ftoi0.xyzw vf18, vf18          88
  vtx2.ftoi0(Mask::xyzw, vtx2);
  // iaddi vi01, vi01, -0x1     |  ftoi0.xyzw vf19, vf19          89
  vtx3.ftoi0(Mask::xyzw, vtx3);   loop_idx = loop_idx + -1;
  // mfir.w vf16, vi11          |  add.xyzw vf28, vf28, vf06      90
  cout0.add(Mask::xyzw, cout0, m_texture_constants.cam_nrm);   vtx0.mfir(Mask::w, vi11);
  // mfir.w vf17, vi11          |  add.xyzw vf29, vf29, vf06      91
  cout1.add(Mask::xyzw, cout1, m_texture_constants.cam_nrm);   vtx1.mfir(Mask::w, vi11);
  // mfir.w vf18, vi11          |  add.xyzw vf30, vf30, vf06      92
  cout2.add(Mask::xyzw, cout2, m_texture_constants.cam_nrm);   vtx2.mfir(Mask::w, vi11);
  // mfir.w vf19, vi11          |  add.xyzw vf31, vf31, vf06      93
  cout3.add(Mask::xyzw, cout3, m_texture_constants.cam_nrm);   vtx3.mfir(Mask::w, vi11);
  // nop                        |  mulx.xyzw vf28, vf28, vf07     94
  cout0.mul(Mask::xyzw, cout0, m_texture_constants.constants.x());
  // nop                        |  mulx.xyzw vf29, vf29, vf07     95
  cout1.mul(Mask::xyzw, cout1, m_texture_constants.constants.x());
  // nop                        |  mulx.xyzw vf30, vf30, vf07     96
  cout2.mul(Mask::xyzw, cout2, m_texture_constants.constants.x());
  // nop                        |  mulx.xyzw vf31, vf31, vf07     97
  cout3.mul(Mask::xyzw, cout3, m_texture_constants.constants.x());
  // nop                        |  addy.xyzw vf28, vf28, vf07     98
  cout0.add(Mask::xyzw, cout0, m_texture_constants.constants.y());
  // nop                        |  addy.xyzw vf29, vf29, vf07     99
  cout1.add(Mask::xyzw, cout1, m_texture_constants.constants.y());
  // nop                        |  addy.xyzw vf30, vf30, vf07     100
  cout2.add(Mask::xyzw, cout2, m_texture_constants.constants.y());
  // nop                        |  addy.xyzw vf31, vf31, vf07     101
  cout3.add(Mask::xyzw, cout3, m_texture_constants.constants.y());
  // sq.xyzw vf16, 1(vi06)      |  sub.zw vf28, vf01, vf00        102
  cout0.sub(Mask::zw, ones, vf00);   sq_buffer(Mask::xyzw, vtx0, vu.dbuf_write + 1);
  // sq.xyzw vf17, 4(vi06)      |  sub.zw vf29, vf01, vf00        103
  cout1.sub(Mask::zw, ones, vf00);   sq_buffer(Mask::xyzw, vtx1, vu.dbuf_write + 4);
  // sq.xyzw vf18, 7(vi06)      |  sub.zw vf30, vf01, vf00        104
  cout2.sub(Mask::zw, ones, vf00);   sq_buffer(Mask::xyzw, vtx2, vu.dbuf_write + 7);
  // sq.xyzw vf19, 10(vi06)     |  sub.zw vf31, vf01, vf00        105
  cout3.sub(Mask::zw, ones, vf00);   sq_buffer(Mask::xyzw, vtx3, vu.dbuf_write + 10);
  // lq.xyzw vf24, 1(vi05)      |  mulw.xyzw vf20, vf15, vf00     106
  res0.mul(Mask::xyzw, base_pos, 1.f);   lq_buffer(Mask::xyzw, nrm0, vu.in_ptr + 1);
  // lq.xyzw vf25, 3(vi05)      |  mulw.xyzw vf21, vf15, vf00     107
  res1.mul(Mask::xyzw, base_pos, 1.f);   lq_buffer(Mask::xyzw, nrm1, vu.in_ptr + 3);
  // lq.xyzw vf26, 5(vi05)      |  mulw.xyzw vf22, vf15, vf00     108
  res2.mul(Mask::xyzw, base_pos, 1.f);   lq_buffer(Mask::xyzw, nrm2, vu.in_ptr + 5);
  // sq.xyzw vf28, 0(vi06)      |  mulw.xyzw vf23, vf15, vf00     109
  res3.mul(Mask::xyzw, base_pos, 1.f);   sq_buffer(Mask::xyzw, cout0, vu.dbuf_write);
  // sq.xyzw vf29, 3(vi06)      |  mulax.xyzw ACC, vf24, vf06     110
  acc.mula(Mask::xyzw, nrm0, m_texture_constants.cam_nrm.x());   sq_buffer(Mask::xyzw, cout1, vu.dbuf_write + 3);
  // sq.xyzw vf30, 6(vi06)      |  madday.xyzw ACC, vf25, vf06    111
  acc.madda(Mask::xyzw, nrm1, m_texture_constants.cam_nrm.y());   sq_buffer(Mask::xyzw, cout2, vu.dbuf_write + 6);
  // sq.xyzw vf31, 9(vi06)      |  maddz.xyzw vf27, vf26, vf06    112
  acc.madd(Mask::xyzw, reflect, nrm2, m_texture_constants.cam_nrm.z());   sq_buffer(Mask::xyzw, cout3, vu.dbuf_write + 9);
  // BRANCH!
  // ibgtz vi01, L4             |  addx.x vf21, vf21, vf02        113
  res1.add(Mask::x, res1, m_texture_constants.offsets.x());   bc = ((s16)loop_idx) > 0;
  // iaddi vi06, vi06, 0xc      |  addy.x vf22, vf22, vf02        114
  res2.add(Mask::x, res2, m_texture_constants.offsets.y());   vu.dbuf_write = vu.dbuf_write + 12;
  if (bc) { goto L4; }

  // lq.xyzw vf28, 0(vi07)      |  addx.y vf14, vf14, vf02        115
  vu.startx.add(Mask::y, vu.startx, m_texture_constants.offsets.x());   lq_buffer(Mask::xyzw, cout0, vu.dbuf_write_base);
  // lq.xyzw vf16, 1(vi07)      |  nop                            116
  lq_buffer(Mask::xyzw, vtx0, vu.dbuf_write_base + 1);
  // sq.xyzw vf20, 2(vi06)      |  nop                            117
  sq_buffer(Mask::xyzw, res0, vu.dbuf_write + 2);
  // sq.xyzw vf28, 0(vi06)      |  nop                            118
  sq_buffer(Mask::xyzw, cout0, vu.dbuf_write);
  // jr vi12                    |  nop                            119
  // sq.xyzw vf16, 1(vi06)      |  nop                            120
  sq_buffer(Mask::xyzw, vtx0, vu.dbuf_write + 1);
  // clang-format on
}

void OceanTexture::run_L5(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  // clang-format off
  u16 loop_idx;
  Vf res0;
  Vf res1;
  Vf cout0;
  Vf cout1;
  Vf vtx0;
  Vf vtx1;
  bool bc;
  // L5:
  // iaddiu vi01, vi00, 0x21    |  nop                            121
  loop_idx = 0x21; /* 33 */
  // sq.xyzw vf05, 0(vi08)      |  nop                            122
  sq_buffer_giftag(m_texture_constants.giftag, vu.tptr);
  // iaddi vi08, vi08, 0x1      |  nop                            123
  vu.tptr = vu.tptr + 1;
  L6:
  // iaddi vi01, vi01, -0x1     |  nop                            124
  loop_idx = loop_idx + -1;
  // lq.xyzw vf20, 2(vi03)      |  nop                            125
  lq_buffer(Mask::xyzw, res0, vu.dbuf_read_a + 2);
  // lq.xyzw vf21, 2(vi04)      |  nop                            126
  lq_buffer(Mask::xyzw, res1, vu.dbuf_read_b + 2);
  // lq.xyzw vf28, 0(vi03)      |  nop                            127
  lq_buffer(Mask::xyzw, cout0, vu.dbuf_read_a);
  // lq.xyzw vf16, 1(vi03)      |  nop                            128
  lq_buffer(Mask::xyzw, vtx0, vu.dbuf_read_a + 1);
  // lq.xyzw vf29, 0(vi04)      |  ftoi4.xyzw vf20, vf20          129
  res0.ftoi4(Mask::xyzw, res0);   lq_buffer(Mask::xyzw, cout1, vu.dbuf_read_b);
  // lq.xyzw vf17, 1(vi04)      |  ftoi4.xyzw vf21, vf21          130
  res1.ftoi4(Mask::xyzw, res1);   lq_buffer(Mask::xyzw, vtx1, vu.dbuf_read_b + 1);
  // sq.xyzw vf28, 0(vi08)      |  nop                            131
  sq_buffer(Mask::xyzw, cout0, vu.tptr);
  // sq.xyzw vf16, 1(vi08)      |  nop                            132
  sq_buffer(Mask::xyzw, vtx0, vu.tptr + 1);
  // sq.xyzw vf20, 2(vi08)      |  nop                            133
  sq_buffer(Mask::xyzw, res0, vu.tptr + 2);
  // sq.xyzw vf29, 3(vi08)      |  nop                            134
  sq_buffer(Mask::xyzw, cout1, vu.tptr + 3);
  // sq.xyzw vf17, 4(vi08)      |  nop                            135
  sq_buffer(Mask::xyzw, vtx1, vu.tptr + 4);
  // sq.xyzw vf21, 5(vi08)      |  nop                            136
  sq_buffer(Mask::xyzw, res1, vu.tptr + 5);

  // iaddi vi03, vi03, 0x3      |  nop                            137
  vu.dbuf_read_a = vu.dbuf_read_a + 3;
  // iaddi vi04, vi04, 0x3      |  nop                            138
  vu.dbuf_read_b = vu.dbuf_read_b + 3;
  // BRANCH!
  // ibgtz vi01, L6             |  nop                            139
  bc = ((s16)loop_idx) > 0;
  // iaddi vi08, vi08, 0x6      |  nop                            140
  vu.tptr = vu.tptr + 6;
  if (bc) { goto L6; }

  // xgkick vi09                |  nop                            141
  xgkick(vu.tbase, render_state, prof);
  // mtir vi08, vf03.x          |  nop                            142
  // vu.tptr = vu.vf03.x_as_u16();
  vu.tptr = get_tbuf();
  // mtir vi09, vf03.x          |  nop                            143
  // vu.vi09 = vu.vf03.x_as_u16();
  vu.tbase = get_tbuf();
  // jr vi12                    |  nop                            144
  // ASSERT(false);
  // mr32.xyzw vf03, vf03       |  nop                            145
  // vu.vf03.mr32(Mask::xyzw, vu.vf03);
  swap_tbuf();
  // clang-format on
}

void OceanTexture::xgkick(Vf* src, SharedRenderState* render_state, ScopedProfilerNode& prof) {
  // KICK
  m_hack_renderer.render_gif((const u8*)src, UINT32_MAX, render_state, prof);
}