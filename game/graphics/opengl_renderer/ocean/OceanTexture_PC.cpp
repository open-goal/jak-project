#include "OceanTexture.h"

void OceanTexture::run_L1_PC() {
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
  run_L3_PC();

  //  mtir vi06_dbuf_write, vf04_dbuf.x
  vu.dbuf_write = get_dbuf();
  //  bal vi12_ra, L3
  //  mr32.xyzw vf04_dbuf, vf04_dbuf
  swap_dbuf();
  run_L3_PC();

  //  mtir vi03_dbuf_read_a, vf04_dbuf.x
  vu.dbuf_read_a = get_dbuf();
  //  bal vi12_ra, L5
  //  mtir vi04_dbuf_read_b, vf04_dbuf.y
  vu.dbuf_read_b = get_dbuf_other();
  run_L5_PC();

  //  mtir vi06_dbuf_write, vf04_dbuf.x
  vu.dbuf_write = get_dbuf();
  //  bal vi12_ra, L3
  //  mr32.xyzw vf04_dbuf, vf04_dbuf
  swap_dbuf();
  run_L3_PC();

  //  mtir vi03_dbuf_read_a, vf04_dbuf.x
  vu.dbuf_read_a = get_dbuf();
  //  bal vi12_ra, L5
  //  mtir vi04_dbuf_read_b, vf04_dbuf.y
  vu.dbuf_read_b = get_dbuf_other();
  run_L5_PC();

  //  nop                     :e
  //  nop
}

void OceanTexture::run_L2_PC() {
  //  L2:
  //  xtop vi05_in_ptr
  vu.in_ptr = swap_vu_upload_buffers();
  //  mtir vi06_dbuf_write, vf04_dbuf.x
  vu.dbuf_write = get_dbuf();
  //  bal vi12_ra, L3
  //  mr32.xyzw vf04_dbuf, vf04_dbuf
  swap_dbuf();
  run_L3_PC();

  //  mtir vi03_dbuf_read_a, vf04_dbuf.x
  vu.dbuf_read_a = get_dbuf();
  //  bal vi12_ra, L5
  //  mtir vi04_dbuf_read_b, vf04_dbuf.y
  vu.dbuf_read_b = get_dbuf_other();
  run_L5_PC();

  //  mtir vi06_dbuf_write, vf04_dbuf.x
  vu.dbuf_write = get_dbuf();
  //  bal vi12_ra, L3
  //  mr32.xyzw vf04_dbuf, vf04_dbuf
  swap_dbuf();
  run_L3_PC();

  //  mtir vi03_dbuf_read_a, vf04_dbuf.x
  vu.dbuf_read_a = get_dbuf();
  //  bal vi12_ra, L5
  //  mtir vi04_dbuf_read_b, vf04_dbuf.y
  vu.dbuf_read_b = get_dbuf_other();
  run_L5_PC();

  //  mtir vi06_dbuf_write, vf04_dbuf.x
  vu.dbuf_write = get_dbuf();
  //  bal vi12_ra, L3
  //  mr32.xyzw vf04_dbuf, vf04_dbuf
  swap_dbuf();
  run_L3_PC();

  //  mtir vi03_dbuf_read_a, vf04_dbuf.x
  vu.dbuf_read_a = get_dbuf();
  //  bal vi12_ra, L5
  //  mtir vi04_dbuf_read_b, vf04_dbuf.y
  vu.dbuf_read_b = get_dbuf_other();
  run_L5_PC();

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

void OceanTexture::run_L3_PC() {
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

void OceanTexture::run_L5_PC() {
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
  xgkick_PC(vu.tbase);
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

void OceanTexture::xgkick_PC(Vf* src) {
  // we're going to rely on the data being the exact layout we expect here.
  u32 offset = 16;
  const u8* data = (const u8*)src;

  for (u32 i = 0; i < NUM_VERTS_PER_STRIP; i++) {
    auto& v = m_pc.vertex_dynamic[m_pc.vtx_idx];
    // st
    memcpy(&v.s, data + offset, sizeof(float) * 2);
    // rgbaq
    v.rgba.x() = data[offset + 16];
    v.rgba.y() = data[offset + 20];
    v.rgba.z() = data[offset + 24];
    v.rgba.w() = data[offset + 28];  // we don't actually need it, it's always 0x80

    // xyz2
    offset += 48;
    m_pc.vtx_idx++;
  }
}

void OceanTexture::run_L1_PC_jak2() {
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
  run_L3_PC_jak2();

  //  mtir vi06_dbuf_write, vf04_dbuf.x
  vu.dbuf_write = get_dbuf();
  //  bal vi12_ra, L3
  //  mr32.xyzw vf04_dbuf, vf04_dbuf
  swap_dbuf();
  run_L3_PC_jak2();

  //  mtir vi03_dbuf_read_a, vf04_dbuf.x
  vu.dbuf_read_a = get_dbuf();
  //  bal vi12_ra, L5
  //  mtir vi04_dbuf_read_b, vf04_dbuf.y
  vu.dbuf_read_b = get_dbuf_other();
  run_L5_PC();

  //  mtir vi06_dbuf_write, vf04_dbuf.x
  vu.dbuf_write = get_dbuf();
  //  bal vi12_ra, L3
  //  mr32.xyzw vf04_dbuf, vf04_dbuf
  swap_dbuf();
  run_L3_PC_jak2();

  //  mtir vi03_dbuf_read_a, vf04_dbuf.x
  vu.dbuf_read_a = get_dbuf();
  //  bal vi12_ra, L5
  //  mtir vi04_dbuf_read_b, vf04_dbuf.y
  vu.dbuf_read_b = get_dbuf_other();
  run_L5_PC();

  //  nop                     :e
  //  nop
}

void OceanTexture::run_L2_PC_jak2() {
  //  L2:
  //  xtop vi05_in_ptr
  vu.in_ptr = swap_vu_upload_buffers();
  //  mtir vi06_dbuf_write, vf04_dbuf.x
  vu.dbuf_write = get_dbuf();
  //  bal vi12_ra, L3
  //  mr32.xyzw vf04_dbuf, vf04_dbuf
  swap_dbuf();
  run_L3_PC_jak2();

  //  mtir vi03_dbuf_read_a, vf04_dbuf.x
  vu.dbuf_read_a = get_dbuf();
  //  bal vi12_ra, L5
  //  mtir vi04_dbuf_read_b, vf04_dbuf.y
  vu.dbuf_read_b = get_dbuf_other();
  run_L5_PC();

  //  mtir vi06_dbuf_write, vf04_dbuf.x
  vu.dbuf_write = get_dbuf();
  //  bal vi12_ra, L3
  //  mr32.xyzw vf04_dbuf, vf04_dbuf
  swap_dbuf();
  run_L3_PC_jak2();

  //  mtir vi03_dbuf_read_a, vf04_dbuf.x
  vu.dbuf_read_a = get_dbuf();
  //  bal vi12_ra, L5
  //  mtir vi04_dbuf_read_b, vf04_dbuf.y
  vu.dbuf_read_b = get_dbuf_other();
  run_L5_PC();

  //  mtir vi06_dbuf_write, vf04_dbuf.x
  vu.dbuf_write = get_dbuf();
  //  bal vi12_ra, L3
  //  mr32.xyzw vf04_dbuf, vf04_dbuf
  swap_dbuf();
  run_L3_PC_jak2();

  //  mtir vi03_dbuf_read_a, vf04_dbuf.x
  vu.dbuf_read_a = get_dbuf();
  //  bal vi12_ra, L5
  //  mtir vi04_dbuf_read_b, vf04_dbuf.y
  vu.dbuf_read_b = get_dbuf_other();
  run_L5_PC();

  //  nop                     :e
  //  nop
}

void OceanTexture::run_L3_PC_jak2() {
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
  // Vf nrm1;  // vf25
  Vf nrm2;  // vf26

  // Vf reflect;  // vf27

  Vf cout0;  // vf28
  Vf cout1;  // vf29
  Vf cout2;  // vf30
  Vf cout3;  // vf31

  // Accumulator acc;
  const Vf ones(1, 1, 1, 1);
  const Vf vf00(0, 0, 0, 1);
  // const u16 vi11 = 0x80;
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
  // lq.xyzw vf26, 3(vi05)      |  mulw.xyzw vf21, vf15, vf00     60
  res1.mul(Mask::xyzw, base_pos, 1.f);   lq_buffer(Mask::xyzw, nrm2, vu.in_ptr + 5);
  // nop                        |  mulw.xyzw vf22, vf15, vf00     61
  lq_buffer(Mask::xyzw, nrm2, vu.in_ptr + 5);
  // nop                        |  mulw.xyzw vf23, vf15, vf00     62
  res3.mul(Mask::xyzw, base_pos, 1.f);
  // nop                        |  addx.x vf21, vf21, vf02        63
  res1.add(Mask::x, res1, m_texture_constants.offsets.x());
  // nop                        |  addy.x vf22, vf22, vf02        64
  res2.add(Mask::x, res2, m_texture_constants.offsets.y());
  L4:
  // nop                        |  addz.x vf23, vf23, vf02        65
  res3.add(Mask::x, res3, m_texture_constants.offsets.z());
  // nop                        |  addw.x vf15, vf15, vf02        66
  base_pos.add(Mask::x, base_pos, m_texture_constants.offsets.w());
  // sq.xyzw vf20, 2(vi06)      |  mulx.x vf28, vf01, vf24        67
  cout0.mul(Mask::x, ones, nrm0.x());   sq_buffer(Mask::xyzw, res0, vu.dbuf_write + 2);
  // sq.xyzw vf21, 5(vi06)      |  muly.x vf29, vf01, vf24        68
  cout1.mul(Mask::x, ones, nrm0.y());   sq_buffer(Mask::xyzw, res1, vu.dbuf_write + 5);
  // sq.xyzw vf22, 8(vi06)      |  mulz.x vf30, vf01, vf24        69
  cout2.mul(Mask::x, ones, nrm0.z());   sq_buffer(Mask::xyzw, res2, vu.dbuf_write + 8);
  // sq.xyzw vf23, 11(vi06)     |  mulw.x vf31, vf01, vf24        70
  cout3.mul(Mask::x, ones, nrm0.w());   sq_buffer(Mask::xyzw, res3, vu.dbuf_write + 11);
  // lq.xyzw vf16, 0(vi05)      |  mulx.y vf28, vf01, vf26        71
  cout0.mul(Mask::y, ones, nrm2.x());   lq_buffer(Mask::xyzw, vtx0, vu.in_ptr);
  // lq.xyzw vf17, 2(vi05)      |  muly.y vf29, vf01, vf26        72
  cout1.mul(Mask::y, ones, nrm2.y());   lq_buffer(Mask::xyzw, vtx1, vu.in_ptr + 2);
  // lq.xyzw vf18, 4(vi05)      |  mulz.y vf30, vf01, vf26        73
  cout2.mul(Mask::y, ones, nrm2.z());   lq_buffer(Mask::xyzw, vtx2, vu.in_ptr + 4);
  // lq.xyzw vf19, 6(vi05)      |  mulw.y vf31, vf01, vf26        74
  cout3.mul(Mask::y, ones, nrm2.w());   lq_buffer(Mask::xyzw, vtx3, vu.in_ptr + 6);
  // iaddi vi05, vi05, 0x8      |  ftoi0.xyzw vf16, vf16          75
  vtx0.ftoi0(Mask::xyzw, vtx0);   vu.in_ptr = vu.in_ptr + 8;
  // nop                        |  ftoi0.xyzw vf17, vf17          76
  vtx1.ftoi0(Mask::xyzw, vtx1);
  // nop                        |  ftoi0.xyzw vf18, vf18          77
  vtx2.ftoi0(Mask::xyzw, vtx2);
  // iaddi vi01, vi01, -0x1     |  ftoi0.xyzw vf19, vf19          78
  vtx3.ftoi0(Mask::xyzw, vtx3);   loop_idx = loop_idx + -1;
  // sq.xyzw vf16, 1(vi06)      |  add.xyzw vf28, vf28, vf07      79
  cout0.add(Mask::xyzw, cout0, m_texture_constants.cam_nrm);   sq_buffer(Mask::xyzw, vtx0, vu.dbuf_write + 1);
  // sq.xyzw vf17, 4(vi06)      |  add.xyzw vf29, vf29, vf07      80
  cout1.add(Mask::xyzw, cout1, m_texture_constants.cam_nrm);   sq_buffer(Mask::xyzw, vtx1, vu.dbuf_write + 4);
  // sq.xyzw vf18, 7(vi06)      |  add.xyzw vf30, vf30, vf07      81
  cout2.add(Mask::xyzw, cout2, m_texture_constants.cam_nrm);   sq_buffer(Mask::xyzw, vtx2, vu.dbuf_write + 7);
  // sq.xyzw vf19, 10(vi06)     |  add.xyzw vf31, vf31, vf07      82
  cout3.add(Mask::xyzw, cout3, m_texture_constants.cam_nrm);   sq_buffer(Mask::xyzw, vtx3, vu.dbuf_write + 10);
  // lq.xyzw vf24, 1(vi05)      |  sub.zw vf28, vf01, vf00        83
  cout0.sub(Mask::zw, ones, vf00); lq_buffer(Mask::xyzw, nrm0, vu.in_ptr + 1);
  // lq.xyzw vf26, 5(vi05)      |  sub.zw vf29, vf01, vf00        84
  cout1.sub(Mask::zw, ones, vf00); lq_buffer(Mask::xyzw, nrm2, vu.in_ptr + 5);
  // nop                        |  sub.zw vf30, vf01, vf00        85
  cout2.sub(Mask::zw, ones, vf00);
  // nop                        |  sub.zw vf31, vf01, vf00        86
  cout3.sub(Mask::zw, ones, vf00);
  // sq.xyzw vf28, 0(vi06)      |  mulw.xyzw vf20, vf15, vf00     87
  res0.mul(Mask::xyzw, base_pos, 1.f);   sq_buffer(Mask::xyzw, cout0, vu.dbuf_write);
  // sq.xyzw vf29, 3(vi06)      |  mulw.xyzw vf21, vf15, vf00     88
  res1.mul(Mask::xyzw, base_pos, 1.f);   sq_buffer(Mask::xyzw, cout1, vu.dbuf_write + 3);
  // sq.xyzw vf30, 6(vi06)      |  mulw.xyzw vf22, vf15, vf00     89
  res2.mul(Mask::xyzw, base_pos, 1.f);   sq_buffer(Mask::xyzw, cout2, vu.dbuf_write + 6);
  // sq.xyzw vf31, 9(vi06)      |  mulw.xyzw vf23, vf15, vf00     90
  res3.mul(Mask::xyzw, base_pos, 1.f);   sq_buffer(Mask::xyzw, cout3, vu.dbuf_write + 9);
  // BRANCH!
  // ibgtz vi01, L4             |  addx.x vf21, vf21, vf02        91
  res1.add(Mask::x, res1, m_texture_constants.offsets.x());   bc = ((s16)loop_idx) > 0;
  // iaddi vi06, vi06, 0xc      |  addy.x vf22, vf22, vf02        92
  res2.add(Mask::x, res2, m_texture_constants.offsets.y());   vu.dbuf_write = vu.dbuf_write + 12;
  if (bc) { goto L4; }

  // lq.xyzw vf28, 0(vi07)      |  addx.y vf14, vf14, vf02        93
  vu.startx.add(Mask::y, vu.startx, m_texture_constants.offsets.x());   lq_buffer(Mask::xyzw, cout0, vu.dbuf_write_base);
  // lq.xyzw vf16, 1(vi07)      |  nop                            94
  lq_buffer(Mask::xyzw, vtx0, vu.dbuf_write_base + 1);
  // sq.xyzw vf20, 2(vi06)      |  nop                            95
  sq_buffer(Mask::xyzw, res0, vu.dbuf_write + 2);
  // sq.xyzw vf28, 0(vi06)      |  nop                            96
  sq_buffer(Mask::xyzw, cout0, vu.dbuf_write);
  // jr vi12                    |  nop                            97
  // sq.xyzw vf16, 1(vi06)      |  nop                            98
  sq_buffer(Mask::xyzw, vtx0, vu.dbuf_write + 1);
  // clang-format on
}

void OceanTexture::setup_renderer() {
  m_pc.vtx_idx = 0;
}

void OceanTexture::flush(SharedRenderState* render_state, ScopedProfilerNode& prof) {
  ASSERT(m_pc.vtx_idx == 2112);
  glBindVertexArray(m_pc.vao);
  glBindBuffer(GL_ARRAY_BUFFER, m_pc.dynamic_vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, sizeof(Vertex) * NUM_VERTS, m_pc.vertex_dynamic.data(),
               GL_DYNAMIC_DRAW);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_pc.gl_index_buffer);

  render_state->shaders[ShaderId::OCEAN_TEXTURE].activate();

  GsTex0 tex0(m_envmap_adgif.tex0_data);
  auto lookup = render_state->texture_pool->lookup(tex0.tbp0());
  if (!lookup) {
    lookup = render_state->texture_pool->get_placeholder_texture();
  }
  // no decal
  // yes tcc
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, *lookup);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  glUniform1i(glGetUniformLocation(render_state->shaders[ShaderId::OCEAN_TEXTURE].id(), "tex_T0"),
              0);

  glDisable(GL_DEPTH_TEST);
  glDisable(GL_BLEND);
  // glDrawArrays(GL_TRIANGLE_STRIP, 0, NUM_VERTS);
  glEnable(GL_PRIMITIVE_RESTART);
  glPrimitiveRestartIndex(UINT32_MAX);
  glDrawElements(GL_TRIANGLE_STRIP, m_pc.index_buffer.size(), GL_UNSIGNED_INT, (void*)0);
  prof.add_draw_call();
  prof.add_tri(NUM_STRIPS * NUM_STRIPS * 2);

  glBindVertexArray(0);
}

void OceanTexture::init_pc() {
  int i = 0;
  m_pc.vertex_positions.resize(NUM_VERTS);
  m_pc.vertex_dynamic.resize(NUM_VERTS);
  m_pc.index_buffer.clear();
  for (u32 strip = 0; strip < NUM_STRIPS; strip++) {
    u32 lo = 64 * strip;
    u32 hi = 64 * (strip + 1);
    for (u32 vert_pair = 0; vert_pair < NUM_VERTS_PER_STRIP / 2; vert_pair++) {
      m_pc.index_buffer.push_back(i);
      auto& v0 = m_pc.vertex_positions[i++];
      v0 = math::Vector2f(vert_pair * 64, lo);
      m_pc.index_buffer.push_back(i);
      auto& v1 = m_pc.vertex_positions[i++];
      v1 = math::Vector2f(vert_pair * 64, hi);
    }
    m_pc.index_buffer.push_back(UINT32_MAX);
  }

  glGenVertexArrays(1, &m_pc.vao);
  glBindVertexArray(m_pc.vao);

  glGenBuffers(1, &m_pc.gl_index_buffer);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_pc.gl_index_buffer);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(u32) * m_pc.index_buffer.size(),
               m_pc.index_buffer.data(), GL_STATIC_DRAW);

  glGenBuffers(1, &m_pc.static_vertex_buffer);
  glBindBuffer(GL_ARRAY_BUFFER, m_pc.static_vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, sizeof(math::Vector2f) * NUM_VERTS, m_pc.vertex_positions.data(),
               GL_STATIC_DRAW);
  glEnableVertexAttribArray(0);
  glEnableVertexAttribArray(1);
  glEnableVertexAttribArray(2);

  glVertexAttribPointer(0,         // location 0 in the shader
                        2,         // 3 floats per vert
                        GL_FLOAT,  // floats
                        GL_TRUE,   // normalized, ignored,
                        0,         // tightly packed
                        0

  );

  glGenBuffers(1, &m_pc.dynamic_vertex_buffer);
  glBindBuffer(GL_ARRAY_BUFFER, m_pc.dynamic_vertex_buffer);
  glBufferData(GL_ARRAY_BUFFER, sizeof(Vertex) * NUM_VERTS, nullptr, GL_DYNAMIC_DRAW);
  glVertexAttribPointer(1,                             // location 0 in the shader
                        4,                             // 4 color components
                        GL_UNSIGNED_BYTE,              // floats
                        GL_TRUE,                       // normalized, ignored,
                        sizeof(Vertex),                //
                        (void*)offsetof(Vertex, rgba)  // offset in array (why is this a pointer...)
  );
  glVertexAttribPointer(2,                          // location 0 in the shader
                        2,                          // 2 floats per vert
                        GL_FLOAT,                   // floats
                        GL_FALSE,                   // normalized, ignored,
                        sizeof(Vertex),             //
                        (void*)offsetof(Vertex, s)  // offset in array (why is this a pointer...)
  );
}

void OceanTexture::destroy_pc() {}