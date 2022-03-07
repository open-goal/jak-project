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

constexpr float kFogFloatOffset = 3071.f;

// clang-format off
void GenericRenderer::mscal0() {
  // L4:
  // iaddiu vi01, vi00, 0x381   |  nop
  vu.vi01 = 0x381; /* 897 */
  // lq.xyzw vf01, 0(vi01)      |  nop
  lq_buffer(Mask::xyzw, gen.fog, vu.vi01);
  // lq.xyzw vf02, 1(vi01)      |  nop
  lq_buffer(Mask::xyzw, gen.adgif_tmpl, vu.vi01 + 1);
  // lq.xyzw vf03, 2(vi01)      |  nop
  // lq_buffer(Mask::xyzw, vu.vf03, vu.vi01 + 2);
  vu.vf03.fill(0);
  // lq.xyzw vf04, 3(vi01)      |  nop
  lq_buffer(Mask::xyzw, gen.hvdf_off, vu.vi01 + 3);
  // lq.xyzw vf05, 4(vi01)      |  nop
  lq_buffer(Mask::xyzw, gen.hmge_scale, vu.vi01 + 4);
  // lq.xyzw vf06, 5(vi01)      |  nop
  // lq_buffer(Mask::xyzw, vu.vf06, vu.vi01 + 5); not used
  // lq.xyzw vf07, 6(vi01)      |  nop
  lq_buffer(Mask::xyzw, gen.guard, vu.vi01 + 6);
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

void GenericRenderer::mscal_noclip_nopipe(SharedRenderState *render_state, ScopedProfilerNode &prof) {
  // buffer crap
  vu.vi02 = vu.vi13 - 0x363; // 867
  vu.vi13 = vu.vi13 + 0x1e;
  if (vu.vi02 == 0) {
    vu.vi13 = 0x345; /* 837 */
  }


  vu.vi03 = vu.vi13 + 7;
  ilw_buffer(Mask::w, vu.vi01, vu.vi13 + 5);
  isw_buffer(Mask::x, vu.vi03, 906);
  vu.vi10 = vu.vi12 + 9;

  lq_buffer(Mask::xyzw, gen.mat0, vu.vi13);
  lq_buffer(Mask::xyzw, gen.mat1, vu.vi13 + 1);
  lq_buffer(Mask::xyzw, gen.mat2, vu.vi13 + 2);
  lq_buffer(Mask::xyzw, gen.mat3, vu.vi13 + 3);
  vu.vi02 = vu.vi01 + vu.vi01;
  vu.vi01 = vu.vi01 + vu.vi02;
  vu.vi11 = -2;
  vu.vi14 = vu.vi10 + vu.vi01;
  isw_buffer(Mask::w, vu.vi12, 906);

  vu.vf18.sub(Mask::w, vu.vf00, vu.vf00.w());

  vu.vf22.add(Mask::z, vu.vf00, vu.vf00.w());
  vu.vf22.ftoi12_check(Mask::z, vu.vf22);

  // this is the vertex transformation loop, unpipelined.
  while (vu.vi10 != vu.vi14) {
    // lq.xy vf22, 0(vi10)          texture load?
    lq_buffer(Mask::xy, vu.vf22, vu.vi10);
    // lq.xyz vf16, 2(vi10)         vertex load
    lq_buffer(Mask::xyz, gen.vtx_load0, vu.vi10 + 2);
    // mtir vi02, vf22.x            grab s coordinate of texture
    vu.vi02 = vu.vf22.x_as_u16();

    // mulaw.xyzw ACC, vf11, vf00   matrix multiply W
    vu.acc.mula(Mask::xyzw, gen.mat3, vu.vf00.w());

    // maddax.xyzw ACC, vf08, vf16  matrix multiply X
    vu.acc.madda(Mask::xyzw, gen.mat0, gen.vtx_load0.x());

    // madday.xyzw ACC, vf09, vf16  matrix multiply Y
    vu.acc.madda(Mask::xyzw, gen.mat1, gen.vtx_load0.y());

    // iand vi06, vi02, vi11        mask s tex coord
    vu.vi06 = vu.vi02 & vu.vi11;

    // mfir.x vf22, vi06            replace s coord
    vu.vf22.mfir(Mask::x, vu.vi06);

    // maddz.xyzw vf12, vf10, vf16  matrix multiply Z
    vu.acc.madd(Mask::xyzw, gen.vtx_p0, gen.mat2, gen.vtx_load0.z());

    // div Q, vf01.x, vf12.w        perspective divide
    vu.Q = gen.fog.x() / gen.vtx_p0.w();

    // itof12.xyz vf18, vf22        texture int to float
    vu.vf18.itof12(Mask::xyz, vu.vf22);

    // mul.xyz vf12, vf12, Q        persepective divide
    gen.vtx_p0.mul(Mask::xyz, gen.vtx_p0, vu.Q);

    // mul.xyz vf18, vf18, Q        texture perspective divide
    vu.vf18.mul(Mask::xyz, vu.vf18, vu.Q);

    // add.xyzw vf12, vf12, vf04    apply hvdf
    gen.vtx_p0.add(Mask::xyzw, gen.vtx_p0, gen.hvdf_off);

    // miniz.w vf12, vf12, vf01     fog clamp
    gen.vtx_p0.mini(Mask::w, gen.vtx_p0, gen.fog.z());

    // maxy.w vf12, vf12, vf01      fog clamp 2
    gen.vtx_p0.max(Mask::w, gen.vtx_p0, gen.fog.y());

    // addw.w vf12, vf12, vf01      ONLY if vi02 != vi06 fog offset.
    if (vu.vi02 != vu.vi06) {
      gen.vtx_p0.add(Mask::w, gen.vtx_p0, kFogFloatOffset);
    }
    // ftoi4.xyzw vf12, vf12        to ints for GS
    gen.vtx_p0.ftoi4_check(Mask::xyzw, gen.vtx_p0);

    // store!
    sq_buffer(Mask::xyzw, gen.vtx_p0, vu.vi10 + 2);
    // this divide should happen in the vertex shader to get perspective correct textures.
    //    vu.vf18.x() /= vu.vf18.z();
    //    vu.vf18.y() /= vu.vf18.z();
    //    vu.vf18.z() = 1.f;

    // fmt::print("tex.z = {}\n", vu.vf18.z());
    sq_buffer(Mask::xyzw, vu.vf18, vu.vi10);


    // iaddi vi10, vi10, 0x3        inc vertex pointer
    vu.vi10 = vu.vi10 + 3;
  }


  // this loop places giftag templates and adgif shaders
  // it allows the same vertices to be drawn several times with different shaders.
  bool bc;
  // iaddi vi14, vi13, 0x7      |  nop                            1060
  vu.vi14 = vu.vi13 + 7;
  // lq.xyzw vf03, 4(vi13)      |  nop                            1061
  Vf draw_hdr2;
  lq_buffer(Mask::xyzw, draw_hdr2, vu.vi13 + 4);
  // ilw.w vi02, 6(vi13)        |  nop                            1062
  ilw_buffer(Mask::w, vu.vi02, vu.vi13 + 6);
  // lq.xyzw vf21, 5(vi13)      |  nop                            1063
  Vf draw_hdr0;
  lq_buffer(Mask::xyzw, draw_hdr0, vu.vi13 + 5);
  // lq.xyzw vf22, 6(vi13)      |  nop                            1064
  Vf draw_hdr1;
  lq_buffer(Mask::xyzw, draw_hdr1, vu.vi13 + 6);
  L83:
  // ilwr.w vi03, vi14          |  nop                            1065
  ilw_buffer(Mask::w, vu.vi03, vu.vi14);
  // ilw.w vi04, 1(vi14)        |  nop                            1066
  ilw_buffer(Mask::w, vu.vi04, vu.vi14 + 1);
  // lqi.xyzw vf16, vi14        |  nop                            1067
  Vf adgif_temp0;
  lq_buffer(Mask::xyzw, adgif_temp0, vu.vi14++);
  // lqi.xyzw vf17, vi14        |  nop                            1068
  Vf adgif_temp1;
  lq_buffer(Mask::xyzw, adgif_temp1, vu.vi14++);
  // lqi.xyzw vf18, vi14        |  nop                            1069
  Vf adgif_temp2;
  lq_buffer(Mask::xyzw, adgif_temp2, vu.vi14++);
  // lqi.xyzw vf19, vi14        |  nop                            1070
  Vf adgif_temp3;
  lq_buffer(Mask::xyzw, adgif_temp3, vu.vi14++);
  // lqi.xyzw vf20, vi14        |  nop                            1071
  Vf adgif_temp4;
  lq_buffer(Mask::xyzw, adgif_temp4, vu.vi14++);
  // iadd vi06, vi03, vi12      |  nop                            1072
  vu.vi06 = vu.vi03 + vu.vi12;
  // sqi.xyzw vf02, vi06        |  nop                            1073
  sq_buffer(Mask::xyzw, gen.adgif_tmpl, vu.vi06++);
  // sqi.xyzw vf16, vi06        |  nop                            1074
  sq_buffer(Mask::xyzw, adgif_temp0, vu.vi06++);
  // sqi.xyzw vf17, vi06        |  nop                            1075
  sq_buffer(Mask::xyzw, adgif_temp1, vu.vi06++);
  // sqi.xyzw vf18, vi06        |  nop                            1076
  sq_buffer(Mask::xyzw, adgif_temp2, vu.vi06++);
  // sqi.xyzw vf19, vi06        |  nop                            1077
  sq_buffer(Mask::xyzw, adgif_temp3, vu.vi06++);
  // sqi.xyzw vf20, vi06        |  nop                            1078
  sq_buffer(Mask::xyzw, adgif_temp4, vu.vi06++);
  // sqi.xyzw vf21, vi06        |  nop                            1079
  sq_buffer(Mask::xyzw, draw_hdr0, vu.vi06++);
  // sqi.xyzw vf22, vi06        |  nop                            1080
  sq_buffer(Mask::xyzw, draw_hdr1, vu.vi06++);
  // sqi.xyzw vf03, vi06        |  nop                            1081
  sq_buffer(Mask::xyzw, draw_hdr2, vu.vi06++);
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


void GenericRenderer::mscal_dispatch(int imm, SharedRenderState* render_state, ScopedProfilerNode& prof) {
  bool bc;
  u16 cf = 0;
  u16 cf0, cf1, cf2;
  switch(imm) {
    case 6:
      goto L33;
    case 8:
      mscal_noclip_nopipe(render_state, prof);
      return;
    default:
      fmt::print("Generic dispatch mscal: {}\n", imm);
      ASSERT(false);
  }

  L33: // R
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
  L34: // R
  // iaddi vi03, vi13, 0x7      |  nop                            365
  vu.vi03 = vu.vi13 + 7;
  // ilw.w vi01, 5(vi13)        |  nop                            366
  ilw_buffer(Mask::w, vu.vi01, vu.vi13 + 5);
  // isw.x vi03, 906(vi00)      |  nop                            367
  isw_buffer(Mask::x, vu.vi03, 906);
  // iaddi vi10, vi12, 0x9      |  subw.w vf18, vf00, vf00        368
  vu.vf18.sub(Mask::w, vu.vf00, vu.vf00.w());   vu.vi10 = vu.vi12 + 9;
  // lq.xyzw vf08, 0(vi13)      |  subw.w vf19, vf00, vf00        369
  vu.vf19.sub(Mask::w, vu.vf00, vu.vf00.w());   lq_buffer(Mask::xyzw, gen.mat0, vu.vi13);
  // lq.xyzw vf09, 1(vi13)      |  subw.w vf20, vf00, vf00        370
  vu.vf20.sub(Mask::w, vu.vf00, vu.vf00.w());   lq_buffer(Mask::xyzw, gen.mat1, vu.vi13 + 1);
  // lq.xyzw vf10, 2(vi13)      |  subw.w vf21, vf00, vf00        371
  vu.vf21.sub(Mask::w, vu.vf00, vu.vf00.w());   lq_buffer(Mask::xyzw, gen.mat2, vu.vi13 + 2);
  // lq.xyzw vf11, 3(vi13)      |  ftoi12.z vf22, vf22            372
  vu.vf22.ftoi12_check(Mask::z, vu.vf22);   lq_buffer(Mask::xyzw, gen.mat3, vu.vi13 + 3);
  // iadd vi02, vi01, vi01      |  ftoi12.z vf23, vf23            373
  vu.vf23.ftoi12_check(Mask::z, vu.vf23);   vu.vi02 = vu.vi01 + vu.vi01;
  // iadd vi01, vi01, vi02      |  sub.xyzw vf16, vf16, vf16      374
  gen.vtx_load0.set_zero();         vu.vi01 = vu.vi01 + vu.vi02;
  // iaddi vi11, vi00, -0x2     |  nop                            375
  vu.vi11 = -2;
  // iadd vi14, vi10, vi01      |  ftoi12.z vf24, vf24            376
  vu.vf24.ftoi12_check(Mask::z, vu.vf24);   vu.vi14 = vu.vi10 + vu.vi01;
  // isw.w vi12, 906(vi00)      |  ftoi12.z vf25, vf25            377
  vu.vf25.ftoi12_check(Mask::z, vu.vf25);   isw_buffer(Mask::w, vu.vi12, 906);
  // iaddi vi14, vi14, 0x9      |  nop                            378
  vu.vi14 = vu.vi14 + 9;
  // lq.xy vf22, 0(vi10)        |  nop                            379
  lq_buffer(Mask::xy, vu.vf22, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  nop                            380
  lq_buffer(Mask::xyz, gen.vtx_load0, vu.vi10 + 2);
  // nop                        |  nop                            381

  // nop                        |  nop                            382

  // nop                        |  mulaw.xyzw ACC, vf11, vf00     383
  vu.acc.mula(Mask::xyzw, gen.mat3, vu.vf00.w());
  // mtir vi02, vf22.x          |  maddax.xyzw ACC, vf08, vf16    384
  vu.acc.madda(Mask::xyzw, gen.mat0, gen.vtx_load0.x());   vu.vi02 = vu.vf22.x_as_u16();
  // iaddi vi10, vi10, 0x3      |  madday.xyzw ACC, vf09, vf16    385
  vu.acc.madda(Mask::xyzw, gen.mat1, gen.vtx_load0.y());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf23, 0(vi10)        |  maddz.xyzw vf12, vf10, vf16    386
  vu.acc.madd(Mask::xyzw, gen.vtx_p0, gen.mat2, gen.vtx_load0.z());   lq_buffer(Mask::xy, vu.vf23, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  nop                            387
  lq_buffer(Mask::xyz, gen.vtx_load0, vu.vi10 + 2);
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
  vu.vf26.mul(Mask::xyzw, gen.vtx_p0, gen.hmge_scale);   vu.Q = gen.fog.x() / gen.vtx_p0.w();
  // nop                        |  nop                            395

  // nop                        |  mulaw.xyzw ACC, vf11, vf00     396
  vu.acc.mula(Mask::xyzw, gen.mat3, vu.vf00.w());
  // mtir vi03, vf23.x          |  maddax.xyzw ACC, vf08, vf16    397
  vu.acc.madda(Mask::xyzw, gen.mat0, gen.vtx_load0.x());   vu.vi03 = vu.vf23.x_as_u16();
  // iaddi vi10, vi10, 0x3      |  madday.xyzw ACC, vf09, vf16    398
  vu.acc.madda(Mask::xyzw, gen.mat1, gen.vtx_load0.y());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf24, 0(vi10)        |  maddz.xyzw vf13, vf10, vf16    399
  vu.acc.madd(Mask::xyzw, gen.vtx_p1, gen.mat2, gen.vtx_load0.z());   lq_buffer(Mask::xy, vu.vf24, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  nop                            400
  lq_buffer(Mask::xyz, gen.vtx_load0, vu.vi10 + 2);
  // iand vi07, vi03, vi11      |  nop                            401
  vu.vi07 = vu.vi03 & vu.vi11;
  // mfir.x vf23, vi07          |  mul.xyz vf12, vf12, Q          402
  gen.vtx_p0.mul(Mask::xyz, gen.vtx_p0, vu.Q);   vu.vf23.mfir(Mask::x, vu.vi07);
  // fcset 0x0                  |  nop                            403
  cf = 0;
  // nop                        |  nop                            404

  // nop                        |  mul.xyz vf18, vf18, Q          405
  vu.vf18.mul(Mask::xyz, vu.vf18, vu.Q);
  // nop                        |  itof12.xyz vf19, vf23          406
  vu.vf19.itof12(Mask::xyz, vu.vf23);
  // div Q, vf01.x, vf13.w      |  mulaw.xyzw ACC, vf11, vf00     407
  vu.acc.mula(Mask::xyzw, gen.mat3, vu.vf00.w());   vu.Q = gen.fog.x() / gen.vtx_p1.w();
  // nop                        |  add.xyzw vf12, vf12, vf04      408
  gen.vtx_p0.add(Mask::xyzw, gen.vtx_p0, gen.hvdf_off);
  // nop                        |  maddax.xyzw ACC, vf08, vf16    409
  vu.acc.madda(Mask::xyzw, gen.mat0, gen.vtx_load0.x());
  // mtir vi04, vf24.x          |  madday.xyzw ACC, vf09, vf16    410
  vu.acc.madda(Mask::xyzw, gen.mat1, gen.vtx_load0.y());   vu.vi04 = vu.vf24.x_as_u16();
  // iaddi vi10, vi10, 0x3      |  maddz.xyzw vf14, vf10, vf16    411
  vu.acc.madd(Mask::xyzw, gen.vtx_p2, gen.mat2, gen.vtx_load0.z());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf25, 0(vi10)        |  miniz.w vf12, vf12, vf01       412
  gen.vtx_p0.mini(Mask::w, gen.vtx_p0, gen.fog.z());   lq_buffer(Mask::xy, vu.vf25, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  mul.xyzw vf27, vf13, vf05      413
  vu.vf27.mul(Mask::xyzw, gen.vtx_p1, gen.hmge_scale);   lq_buffer(Mask::xyz, gen.vtx_load0, vu.vi10 + 2);
  // iand vi08, vi04, vi11      |  nop                            414
  vu.vi08 = vu.vi04 & vu.vi11;
  // mfir.x vf24, vi08          |  mul.xyz vf13, vf13, Q          415
  gen.vtx_p1.mul(Mask::xyz, gen.vtx_p1, vu.Q);   vu.vf24.mfir(Mask::x, vu.vi08);
  // nop                        |  maxy.w vf12, vf12, vf01        416
  gen.vtx_p0.max(Mask::w, gen.vtx_p0, gen.fog.y());
  // nop                        |  clipw.xyz vf26, vf26           417
 cf = clip(vu.vf26, vu.vf26.w(), cf);
  // nop                        |  mul.xyz vf19, vf19, Q          418
  vu.vf19.mul(Mask::xyz, vu.vf19, vu.Q);
  // BRANCH!
  // ibeq vi02, vi06, L35       |  itof12.xyz vf20, vf24          419
  vu.vf20.itof12(Mask::xyz, vu.vf24);   bc = (vu.vi02 == vu.vi06);
  // div Q, vf01.x, vf14.w      |  mulaw.xyzw ACC, vf11, vf00     420
  vu.acc.mula(Mask::xyzw, gen.mat3, vu.vf00.w());   vu.Q = gen.fog.x() / gen.vtx_p2.w();
  if (bc) { goto L35; }

  // nop                        |  addw.w vf12, vf12, vf01        421
  gen.vtx_p0.add(Mask::w, gen.vtx_p0, kFogFloatOffset);
  L35:
  // nop                        |  add.xyzw vf13, vf13, vf04      422
  gen.vtx_p1.add(Mask::xyzw, gen.vtx_p1, gen.hvdf_off);
  // nop                        |  maddax.xyzw ACC, vf08, vf16    423
  vu.acc.madda(Mask::xyzw, gen.mat0, gen.vtx_load0.x());
  // mtir vi05, vf25.x          |  madday.xyzw ACC, vf09, vf16    424
  vu.acc.madda(Mask::xyzw, gen.mat1, gen.vtx_load0.y());   vu.vi05 = vu.vf25.x_as_u16();
  // iaddi vi10, vi10, 0x3      |  maddz.xyzw vf15, vf10, vf16    425
  vu.acc.madd(Mask::xyzw, gen.vtx_p3, gen.mat2, gen.vtx_load0.z());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf22, 0(vi10)        |  miniz.w vf13, vf13, vf01       426
  gen.vtx_p1.mini(Mask::w, gen.vtx_p1, gen.fog.z());   lq_buffer(Mask::xy, vu.vf22, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  mul.xyzw vf28, vf14, vf05      427
  vu.vf28.mul(Mask::xyzw, gen.vtx_p2, gen.hmge_scale);   lq_buffer(Mask::xyz, gen.vtx_load0, vu.vi10 + 2);
  // iand vi09, vi05, vi11      |  ftoi4.xyzw vf12, vf12          428
  gen.vtx_p0.ftoi4_check(Mask::xyzw, gen.vtx_p0);   vu.vi09 = vu.vi05 & vu.vi11;
  // mfir.x vf25, vi09          |  mul.xyz vf14, vf14, Q          429
  gen.vtx_p2.mul(Mask::xyz, gen.vtx_p2, vu.Q);   vu.vf25.mfir(Mask::x, vu.vi09);
  // sq.xyzw vf18, -12(vi10)    |  maxy.w vf13, vf13, vf01        430
  gen.vtx_p1.max(Mask::w, gen.vtx_p1, gen.fog.y());   sq_buffer(Mask::xyzw, vu.vf18, vu.vi10 + -12);
  // BRANCH!
  // ibeq vi14, vi10, L46       |  clipw.xyz vf27, vf27           431
   cf = clip(vu.vf27, vu.vf27.w(), cf);   bc = (vu.vi14 == vu.vi10);
  // sq.xyzw vf12, -10(vi10)    |  mul.xyz vf20, vf20, Q          432
  vu.vf20.mul(Mask::xyz, vu.vf20, vu.Q);   sq_buffer(Mask::xyzw, gen.vtx_p0, vu.vi10 + -10);
  if (bc) { goto L46; }

  // BRANCH!
  // ibeq vi03, vi07, L36       |  itof12.xyz vf21, vf25          433
  vu.vf21.itof12(Mask::xyz, vu.vf25);   bc = (vu.vi03 == vu.vi07);
  // div Q, vf01.x, vf15.w      |  mulaw.xyzw ACC, vf11, vf00     434
  vu.acc.mula(Mask::xyzw, gen.mat3, vu.vf00.w());   vu.Q = gen.fog.x() / gen.vtx_p3.w();
  if (bc) { goto L36; }

  // nop                        |  addw.w vf13, vf13, vf01        435
  gen.vtx_p1.add(Mask::w, gen.vtx_p1, kFogFloatOffset);
  L36:
  // nop                        |  add.xyzw vf14, vf14, vf04      436
  gen.vtx_p2.add(Mask::xyzw, gen.vtx_p2, gen.hvdf_off);
  // nop                        |  maddax.xyzw ACC, vf08, vf16    437
  vu.acc.madda(Mask::xyzw, gen.mat0, gen.vtx_load0.x());
  // mtir vi02, vf22.x          |  madday.xyzw ACC, vf09, vf16    438
  vu.acc.madda(Mask::xyzw, gen.mat1, gen.vtx_load0.y());   vu.vi02 = vu.vf22.x_as_u16();
  // iaddi vi10, vi10, 0x3      |  maddz.xyzw vf12, vf10, vf16    439
  vu.acc.madd(Mask::xyzw, gen.vtx_p0, gen.mat2, gen.vtx_load0.z());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf23, 0(vi10)        |  miniz.w vf14, vf14, vf01       440
  gen.vtx_p2.mini(Mask::w, gen.vtx_p2, gen.fog.z());   lq_buffer(Mask::xy, vu.vf23, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  mul.xyzw vf29, vf15, vf05      441
  vu.vf29.mul(Mask::xyzw, gen.vtx_p3, gen.hmge_scale);   lq_buffer(Mask::xyz, gen.vtx_load0, vu.vi10 + 2);
  // iand vi06, vi02, vi11      |  ftoi4.xyzw vf13, vf13          442
  gen.vtx_p1.ftoi4_check(Mask::xyzw, gen.vtx_p1);   vu.vi06 = vu.vi02 & vu.vi11;
  // mfir.x vf22, vi06          |  mul.xyz vf15, vf15, Q          443
  gen.vtx_p3.mul(Mask::xyz, gen.vtx_p3, vu.Q);   vu.vf22.mfir(Mask::x, vu.vi06);
  // sq.xyzw vf19, -12(vi10)    |  maxy.w vf14, vf14, vf01        444
  gen.vtx_p2.max(Mask::w, gen.vtx_p2, gen.fog.y());   sq_buffer(Mask::xyzw, vu.vf19, vu.vi10 + -12);
  // BRANCH!
  // ibeq vi14, vi10, L46       |  clipw.xyz vf28, vf28           445
  cf = clip(vu.vf28, vu.vf28.w(), cf);   bc = (vu.vi14 == vu.vi10);
  // sq.xyzw vf13, -10(vi10)    |  mul.xyz vf21, vf21, Q          446
  vu.vf21.mul(Mask::xyz, vu.vf21, vu.Q);   sq_buffer(Mask::xyzw, gen.vtx_p1, vu.vi10 + -10);
  if (bc) { goto L46; }

  L37:
  // BRANCH!
  // ibeq vi04, vi08, L38       |  itof12.xyz vf18, vf22          447
  vu.vf18.itof12(Mask::xyz, vu.vf22);   bc = (vu.vi04 == vu.vi08);
  // div Q, vf01.x, vf12.w      |  mulaw.xyzw ACC, vf11, vf00     448
  vu.acc.mula(Mask::xyzw, gen.mat3, vu.vf00.w());   vu.Q = gen.fog.x() / gen.vtx_p0.w();
  if (bc) { goto L38; }

  // nop                        |  addw.w vf14, vf14, vf01        449
  gen.vtx_p2.add(Mask::w, gen.vtx_p2, kFogFloatOffset);
  L38:
  // fcand vi01, 0x3ffff        |  add.xyzw vf15, vf15, vf04      450
  gen.vtx_p3.add(Mask::xyzw, gen.vtx_p3, gen.hvdf_off);    vu.vi01 = cf & 0x3ffff;

  // BRANCH!
  // ibne vi00, vi01, L55       |  maddax.xyzw ACC, vf08, vf16    451
  vu.acc.madda(Mask::xyzw, gen.mat0, gen.vtx_load0.x());   bc = (vu.vi01 != 0);
  // mtir vi03, vf23.x          |  madday.xyzw ACC, vf09, vf16    452
  vu.acc.madda(Mask::xyzw, gen.mat1, gen.vtx_load0.y());   vu.vi03 = vu.vf23.x_as_u16();
  if (bc) { goto L55; }

  L39:
  // iaddi vi10, vi10, 0x3      |  maddz.xyzw vf13, vf10, vf16    453
  vu.acc.madd(Mask::xyzw, gen.vtx_p1, gen.mat2, gen.vtx_load0.z());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf24, 0(vi10)        |  miniz.w vf15, vf15, vf01       454
  gen.vtx_p3.mini(Mask::w, gen.vtx_p3, gen.fog.z());   lq_buffer(Mask::xy, vu.vf24, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  mul.xyzw vf26, vf12, vf05      455
  vu.vf26.mul(Mask::xyzw, gen.vtx_p0, gen.hmge_scale);   lq_buffer(Mask::xyz, gen.vtx_load0, vu.vi10 + 2);
  // iand vi07, vi03, vi11      |  ftoi4.xyzw vf14, vf14          456
  gen.vtx_p2.ftoi4_check(Mask::xyzw, gen.vtx_p2);   vu.vi07 = vu.vi03 & vu.vi11;
  // mfir.x vf23, vi07          |  mul.xyz vf12, vf12, Q          457
  gen.vtx_p0.mul(Mask::xyz, gen.vtx_p0, vu.Q);   vu.vf23.mfir(Mask::x, vu.vi07);
  // sq.xyzw vf20, -12(vi10)    |  maxy.w vf15, vf15, vf01        458
  gen.vtx_p3.max(Mask::w, gen.vtx_p3, gen.fog.y());   sq_buffer(Mask::xyzw, vu.vf20, vu.vi10 + -12);
  // BRANCH!
  // ibeq vi14, vi10, L46       |  clipw.xyz vf29, vf29           459
  cf = clip(vu.vf29, vu.vf29.w(), cf);   bc = (vu.vi14 == vu.vi10);
  // sq.xyzw vf14, -10(vi10)    |  mul.xyz vf18, vf18, Q          460
  vu.vf18.mul(Mask::xyz, vu.vf18, vu.Q);   sq_buffer(Mask::xyzw, gen.vtx_p2, vu.vi10 + -10);
  if (bc) { goto L46; }

  // BRANCH!
  // ibeq vi05, vi09, L40       |  itof12.xyz vf19, vf23          461
  vu.vf19.itof12(Mask::xyz, vu.vf23);   bc = (vu.vi05 == vu.vi09);
  // div Q, vf01.x, vf13.w      |  mulaw.xyzw ACC, vf11, vf00     462
  vu.acc.mula(Mask::xyzw, gen.mat3, vu.vf00.w());   vu.Q = gen.fog.x() / gen.vtx_p1.w();
  if (bc) { goto L40; }

  // nop                        |  addw.w vf15, vf15, vf01        463
  gen.vtx_p3.add(Mask::w, gen.vtx_p3, kFogFloatOffset);
  L40:
  // fcand vi01, 0x3ffff        |  add.xyzw vf12, vf12, vf04      464
  gen.vtx_p0.add(Mask::xyzw, gen.vtx_p0, gen.hvdf_off);    vu.vi01 = cf & 0x3ffff;

  // BRANCH!
  // ibne vi00, vi01, L59       |  maddax.xyzw ACC, vf08, vf16    465
  vu.acc.madda(Mask::xyzw, gen.mat0, gen.vtx_load0.x());   bc = (vu.vi01 != 0);
  // mtir vi04, vf24.x          |  madday.xyzw ACC, vf09, vf16    466
  vu.acc.madda(Mask::xyzw, gen.mat1, gen.vtx_load0.y());   vu.vi04 = vu.vf24.x_as_u16();
  if (bc) { goto L59; }

  L41:
  // iaddi vi10, vi10, 0x3      |  maddz.xyzw vf14, vf10, vf16    467
  vu.acc.madd(Mask::xyzw, gen.vtx_p2, gen.mat2, gen.vtx_load0.z());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf25, 0(vi10)        |  miniz.w vf12, vf12, vf01       468
  gen.vtx_p0.mini(Mask::w, gen.vtx_p0, gen.fog.z());   lq_buffer(Mask::xy, vu.vf25, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  mul.xyzw vf27, vf13, vf05      469
  vu.vf27.mul(Mask::xyzw, gen.vtx_p1, gen.hmge_scale);   lq_buffer(Mask::xyz, gen.vtx_load0, vu.vi10 + 2);
  // iand vi08, vi04, vi11      |  ftoi4.xyzw vf15, vf15          470
  gen.vtx_p3.ftoi4_check(Mask::xyzw, gen.vtx_p3);   vu.vi08 = vu.vi04 & vu.vi11;
  // mfir.x vf24, vi08          |  mul.xyz vf13, vf13, Q          471
  gen.vtx_p1.mul(Mask::xyz, gen.vtx_p1, vu.Q);   vu.vf24.mfir(Mask::x, vu.vi08);
  // sq.xyzw vf21, -12(vi10)    |  maxy.w vf12, vf12, vf01        472
  gen.vtx_p0.max(Mask::w, gen.vtx_p0, gen.fog.y());   sq_buffer(Mask::xyzw, vu.vf21, vu.vi10 + -12);
  // BRANCH!
  // ibeq vi14, vi10, L46       |  clipw.xyz vf26, vf26           473
  cf = clip(vu.vf26, vu.vf26.w(), cf);   bc = (vu.vi14 == vu.vi10);
  // sq.xyzw vf15, -10(vi10)    |  mul.xyz vf19, vf19, Q          474
  vu.vf19.mul(Mask::xyz, vu.vf19, vu.Q);   sq_buffer(Mask::xyzw, gen.vtx_p3, vu.vi10 + -10);
  if (bc) { goto L46; }

  // BRANCH!
  // ibeq vi02, vi06, L42       |  itof12.xyz vf20, vf24          475
  vu.vf20.itof12(Mask::xyz, vu.vf24);   bc = (vu.vi02 == vu.vi06);
  // div Q, vf01.x, vf14.w      |  mulaw.xyzw ACC, vf11, vf00     476
  vu.acc.mula(Mask::xyzw, gen.mat3, vu.vf00.w());   vu.Q = gen.fog.x() / gen.vtx_p2.w();
  if (bc) { goto L42; }

  // nop                        |  addw.w vf12, vf12, vf01        477
  gen.vtx_p0.add(Mask::w, gen.vtx_p0, kFogFloatOffset);
  L42:
  // fcand vi01, 0x3ffff        |  add.xyzw vf13, vf13, vf04      478
  gen.vtx_p1.add(Mask::xyzw, gen.vtx_p1, gen.hvdf_off);    vu.vi01 = cf & 0x3ffff;

  // BRANCH!
  // ibne vi00, vi01, L47       |  maddax.xyzw ACC, vf08, vf16    479
  vu.acc.madda(Mask::xyzw, gen.mat0, gen.vtx_load0.x());   bc = (vu.vi01 != 0);
  // mtir vi05, vf25.x          |  madday.xyzw ACC, vf09, vf16    480
  vu.acc.madda(Mask::xyzw, gen.mat1, gen.vtx_load0.y());   vu.vi05 = vu.vf25.x_as_u16();
  if (bc) { goto L47; }

  L43:
  // iaddi vi10, vi10, 0x3      |  maddz.xyzw vf15, vf10, vf16    481
  vu.acc.madd(Mask::xyzw, gen.vtx_p3, gen.mat2, gen.vtx_load0.z());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf22, 0(vi10)        |  miniz.w vf13, vf13, vf01       482
  gen.vtx_p1.mini(Mask::w, gen.vtx_p1, gen.fog.z());   lq_buffer(Mask::xy, vu.vf22, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  mul.xyzw vf28, vf14, vf05      483
  vu.vf28.mul(Mask::xyzw, gen.vtx_p2, gen.hmge_scale);   lq_buffer(Mask::xyz, gen.vtx_load0, vu.vi10 + 2);
  // iand vi09, vi05, vi11      |  ftoi4.xyzw vf12, vf12          484
  gen.vtx_p0.ftoi4_check(Mask::xyzw, gen.vtx_p0);   vu.vi09 = vu.vi05 & vu.vi11;
  // mfir.x vf25, vi09          |  mul.xyz vf14, vf14, Q          485
  gen.vtx_p2.mul(Mask::xyz, gen.vtx_p2, vu.Q);   vu.vf25.mfir(Mask::x, vu.vi09);
  // sq.xyzw vf18, -12(vi10)    |  maxy.w vf13, vf13, vf01        486
  gen.vtx_p1.max(Mask::w, gen.vtx_p1, gen.fog.y());   sq_buffer(Mask::xyzw, vu.vf18, vu.vi10 + -12);
  // BRANCH!
  // ibeq vi14, vi10, L46       |  clipw.xyz vf27, vf27           487
  cf = clip(vu.vf27, vu.vf27.w(), cf);   bc = (vu.vi14 == vu.vi10);
  // sq.xyzw vf12, -10(vi10)    |  mul.xyz vf20, vf20, Q          488
  vu.vf20.mul(Mask::xyz, vu.vf20, vu.Q);   sq_buffer(Mask::xyzw, gen.vtx_p0, vu.vi10 + -10);
  if (bc) { goto L46; }

  // BRANCH!
  // ibeq vi03, vi07, L44       |  itof12.xyz vf21, vf25          489
  vu.vf21.itof12(Mask::xyz, vu.vf25);   bc = (vu.vi03 == vu.vi07);
  // div Q, vf01.x, vf15.w      |  mulaw.xyzw ACC, vf11, vf00     490
  vu.acc.mula(Mask::xyzw, gen.mat3, vu.vf00.w());   vu.Q = gen.fog.x() / gen.vtx_p3.w();
  if (bc) { goto L44; }

  // nop                        |  addw.w vf13, vf13, vf01        491
  gen.vtx_p1.add(Mask::w, gen.vtx_p1, kFogFloatOffset);
  L44:
  // fcand vi01, 0x3ffff        |  add.xyzw vf14, vf14, vf04      492
  gen.vtx_p2.add(Mask::xyzw, gen.vtx_p2, gen.hvdf_off);    vu.vi01 = cf & 0x3ffff;

  // BRANCH!
  // ibne vi00, vi01, L51       |  maddax.xyzw ACC, vf08, vf16    493
  vu.acc.madda(Mask::xyzw, gen.mat0, gen.vtx_load0.x());   bc = (vu.vi01 != 0);
  // mtir vi02, vf22.x          |  madday.xyzw ACC, vf09, vf16    494
  vu.acc.madda(Mask::xyzw, gen.mat1, gen.vtx_load0.y());   vu.vi02 = vu.vf22.x_as_u16();
  if (bc) { goto L51; }

  L45:
  // iaddi vi10, vi10, 0x3      |  maddz.xyzw vf12, vf10, vf16    495
  vu.acc.madd(Mask::xyzw, gen.vtx_p0, gen.mat2, gen.vtx_load0.z());   vu.vi10 = vu.vi10 + 3;
  // lq.xy vf23, 0(vi10)        |  miniz.w vf14, vf14, vf01       496
  gen.vtx_p2.mini(Mask::w, gen.vtx_p2, gen.fog.z());   lq_buffer(Mask::xy, vu.vf23, vu.vi10);
  // lq.xyz vf16, 2(vi10)       |  mul.xyzw vf29, vf15, vf05      497
  vu.vf29.mul(Mask::xyzw, gen.vtx_p3, gen.hmge_scale);   lq_buffer(Mask::xyz, gen.vtx_load0, vu.vi10 + 2);
  // iand vi06, vi02, vi11      |  ftoi4.xyzw vf13, vf13          498
  gen.vtx_p1.ftoi4_check(Mask::xyzw, gen.vtx_p1);   vu.vi06 = vu.vi02 & vu.vi11;
  // mfir.x vf22, vi06          |  mul.xyz vf15, vf15, Q          499
  gen.vtx_p3.mul(Mask::xyz, gen.vtx_p3, vu.Q);   vu.vf22.mfir(Mask::x, vu.vi06);
  // sq.xyzw vf19, -12(vi10)    |  maxy.w vf14, vf14, vf01        500
  gen.vtx_p2.max(Mask::w, gen.vtx_p2, gen.fog.y());   sq_buffer(Mask::xyzw, vu.vf19, vu.vi10 + -12);
  // BRANCH!
  // ibne vi14, vi10, L37       |  clipw.xyz vf28, vf28           501
  cf = clip(vu.vf28, vu.vf28.w(), cf);   bc = (vu.vi14 != vu.vi10);
  // sq.xyzw vf13, -10(vi10)    |  mul.xyz vf21, vf21, Q          502
  vu.vf21.mul(Mask::xyz, vu.vf21, vu.Q);   sq_buffer(Mask::xyzw, gen.vtx_p1, vu.vi10 + -10);
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
  gen.vtx_p0.add(Mask::w, gen.vtx_p0, kFogFloatOffset);   sq_buffer(Mask::xyzw, vu.vf23, 998);
  if (bc) { goto L43; }

  // sq.xyzw vf24, 999(vi00)    |  mul.xyzw vf23, vf28, vf07      507
  vu.vf23.mul(Mask::xyzw, vu.vf28, gen.guard);   sq_buffer(Mask::xyzw, vu.vf24, 999);
  // sq.xyzw vf25, 1000(vi00)   |  mul.xyzw vf24, vf29, vf07      508
  vu.vf24.mul(Mask::xyzw, vu.vf29, gen.guard);   sq_buffer(Mask::xyzw, vu.vf25, 1000);
  // isw.x vi01, 1001(vi00)     |  mul.xyzw vf25, vf26, vf07      509
  vu.vf25.mul(Mask::xyzw, vu.vf26, gen.guard);   isw_buffer(Mask::x, vu.vi01, 1001);
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

  // L48:
  // div Q, vf01.x, vf14.w      |  nop                            524
  vu.Q = gen.fog.x() / gen.vtx_p2.w();
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
  ASSERT(false);

  L51:
  // BRANCH!
  // ibne vi03, vi07, L45       |  nop                            588
  bc = (vu.vi03 != vu.vi07);
  // sq.xyzw vf23, 998(vi00)    |  addw.w vf13, vf13, vf01        589
  gen.vtx_p1.add(Mask::w, gen.vtx_p1, kFogFloatOffset);   sq_buffer(Mask::xyzw, vu.vf23, 998);
  if (bc) { goto L45; }

  // sq.xyzw vf24, 999(vi00)    |  mul.xyzw vf23, vf29, vf07      590
  vu.vf23.mul(Mask::xyzw, vu.vf29, gen.guard);   sq_buffer(Mask::xyzw, vu.vf24, 999);
  // sq.xyzw vf25, 1000(vi00)   |  mul.xyzw vf24, vf26, vf07      591
  vu.vf24.mul(Mask::xyzw, vu.vf26, gen.guard);   sq_buffer(Mask::xyzw, vu.vf25, 1000);
  // isw.x vi01, 1001(vi00)     |  mul.xyzw vf25, vf27, vf07      592
  vu.vf25.mul(Mask::xyzw, vu.vf27, gen.guard);   isw_buffer(Mask::x, vu.vi01, 1001);
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

  //  L52:
  // div Q, vf01.x, vf15.w      |  nop                            607
  vu.Q = gen.fog.x() / gen.vtx_p3.w();
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
  ASSERT(false);

  L55:
  // BRANCH!
  // ibne vi04, vi08, L39       |  nop                            671
  bc = (vu.vi04 != vu.vi08);
  // sq.xyzw vf23, 998(vi00)    |  addw.w vf14, vf14, vf01        672
  gen.vtx_p2.add(Mask::w, gen.vtx_p2, kFogFloatOffset);   sq_buffer(Mask::xyzw, vu.vf23, 998);
  if (bc) { goto L39; }

  // sq.xyzw vf24, 999(vi00)    |  mul.xyzw vf23, vf26, vf07      673
  vu.vf23.mul(Mask::xyzw, vu.vf26, gen.guard);   sq_buffer(Mask::xyzw, vu.vf24, 999);
  // sq.xyzw vf25, 1000(vi00)   |  mul.xyzw vf24, vf27, vf07      674
  vu.vf24.mul(Mask::xyzw, vu.vf27, gen.guard);   sq_buffer(Mask::xyzw, vu.vf25, 1000);
  // isw.x vi01, 1001(vi00)     |  mul.xyzw vf25, vf28, vf07      675
  vu.vf25.mul(Mask::xyzw, vu.vf28, gen.guard);   isw_buffer(Mask::x, vu.vi01, 1001);
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

  // L56:
  // div Q, vf01.x, vf12.w      |  nop                            690
  vu.Q = gen.fog.x() / gen.vtx_p0.w();
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
  ASSERT(false);

  L59:
  // BRANCH!
  // ibne vi05, vi09, L41       |  nop                            754
  bc = (vu.vi05 != vu.vi09);
  // sq.xyzw vf23, 998(vi00)    |  addw.w vf15, vf15, vf01        755
  gen.vtx_p3.add(Mask::w, gen.vtx_p3, kFogFloatOffset);   sq_buffer(Mask::xyzw, vu.vf23, 998);
  if (bc) { goto L41; }

  // sq.xyzw vf24, 999(vi00)    |  mul.xyzw vf23, vf27, vf07      756
  vu.vf23.mul(Mask::xyzw, vu.vf27, gen.guard);   sq_buffer(Mask::xyzw, vu.vf24, 999);
  // sq.xyzw vf25, 1000(vi00)   |  mul.xyzw vf24, vf28, vf07      757
  vu.vf24.mul(Mask::xyzw, vu.vf28, gen.guard);   sq_buffer(Mask::xyzw, vu.vf25, 1000);
  // isw.x vi01, 1001(vi00)     |  mul.xyzw vf25, vf29, vf07      758
  vu.vf25.mul(Mask::xyzw, vu.vf29, gen.guard);   isw_buffer(Mask::x, vu.vi01, 1001);
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

  // L60:
  // div Q, vf01.x, vf13.w      |  nop                            773
  vu.Q = gen.fog.x() / gen.vtx_p1.w();
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
  ASSERT(false);


  L82: // R
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
  Vf adgif_temp0;
  lq_buffer(Mask::xyzw, adgif_temp0, vu.vi14++);
  // lqi.xyzw vf17, vi14        |  nop                            1068
  Vf adgif_temp1;
  lq_buffer(Mask::xyzw, adgif_temp1, vu.vi14++);
  // lqi.xyzw vf18, vi14        |  nop                            1069
  lq_buffer(Mask::xyzw, vu.vf18, vu.vi14++);
  // lqi.xyzw vf19, vi14        |  nop                            1070
  lq_buffer(Mask::xyzw, vu.vf19, vu.vi14++);
  // lqi.xyzw vf20, vi14        |  nop                            1071
  lq_buffer(Mask::xyzw, vu.vf20, vu.vi14++);
  // iadd vi06, vi03, vi12      |  nop                            1072
  vu.vi06 = vu.vi03 + vu.vi12;
  // sqi.xyzw vf02, vi06        |  nop                            1073
  sq_buffer(Mask::xyzw, gen.adgif_tmpl, vu.vi06++);
  // sqi.xyzw vf16, vi06        |  nop                            1074
  sq_buffer(Mask::xyzw, adgif_temp0, vu.vi06++);
  // sqi.xyzw vf17, vi06        |  nop                            1075
  sq_buffer(Mask::xyzw, adgif_temp1, vu.vi06++);
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