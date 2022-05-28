#include "game/graphics/opengl_renderer/MercRenderer.h"

void MercRenderer::mscal(int enter_address,
                         SharedRenderState* render_state,
                         ScopedProfilerNode& prof) {
  mscal_impl<false>(enter_address, render_state, prof);
}

static inline REALLY_INLINE float erleng(const Vf& in) {
  float len = in.x() * in.x() + in.y() * in.y() + in.z() * in.z();
  return _mm_cvtss_f32(_mm_rsqrt_ss(_mm_set_ss(len)));
}

u16 MercRenderer::xitop() {
  return m_dbf ? BUFFER_BASE : 0;
}

u16 MercRenderer::xtop() {
  return m_dbf ? 0 : BUFFER_BASE;
}

void MercRenderer::lq_buffer(Mask mask, Vf& dest, u16 addr) {
  ASSERT(addr * 16 < sizeof(m_buffer.data));
  for (int i = 0; i < 4; i++) {
    if ((u64)mask & (1 << i)) {
      memcpy(dest.data + i, m_buffer.data + addr * 16 + i * 4, 4);
    }
  }
}

template <bool DEBUG>
REALLY_INLINE void MercRenderer::lq_buffer_xyzw(Vf& dest, u16 addr) {
  if constexpr (DEBUG) {
    ASSERT(addr * 16 < sizeof(m_buffer.data));
  }
  copy_vector(dest.data, m_buffer.data + addr * 16);
}

template <bool DEBUG>
REALLY_INLINE void MercRenderer::lq_buffer_xyz(Vf& dest, u16 addr) {
  if constexpr (DEBUG) {
    ASSERT(addr * 16 < sizeof(m_buffer.data));
  }
  auto reg = _mm_load_ps(dest.data);
  auto mem = _mm_load_ps((const float*)(m_buffer.data + addr * 16));
  _mm_store_ps(dest.data, _mm_blend_ps(mem, reg, 0b1000));
}

template <bool DEBUG>
REALLY_INLINE void MercRenderer::sq_buffer_xyzw(const Vf& src, u16 addr) {
  if constexpr (DEBUG) {
    ASSERT(addr * 16 < sizeof(m_buffer.data));
  }
  copy_vector(m_buffer.data + addr * 16, src.data);
}

void MercRenderer::isw_buffer(Mask mask, u16 val, u16 addr) {
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

void MercRenderer::ilw_buffer(Mask mask, u16& dest, u16 addr) {
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

// clang-format off
template<bool DEBUG>
void MercRenderer::mscal_impl(int enter_address,
                         SharedRenderState* render_state,
                         ScopedProfilerNode& prof) {
  bool bc = false;
    //  bool bc = false;
  switch (enter_address) {
    case 0:
      goto ENTER_0;
    case 17:
      goto ENTER_17;
    case 20:
      goto ENTER_20;
    case 32:
      goto ENTER_32;
    case 35:
      goto ENTER_35;
    default:
      ASSERT(false);
  }
  ENTER_0:
  // lq.xyzw vf01, 7(vi00)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf01, 7);
  // lq.xyzw vf25, 3(vi00)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf25, 3);
  // lq.xyzw vf26, 4(vi00)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf26, 4);
  // lq.xyzw vf27, 5(vi00)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf27, 5);
  // lq.xyzw vf28, 6(vi00)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf28, 6);
  // mr32.xyzw vf01, vf01       |  nop
  vu.vf01.mr32(Mask::xyzw, vu.vf01);
  // move.y vf25, vf26          |  nop
  vu.vf25.move(Mask::y, vu.vf26);
  // move.zw vf25, vf27         |  nop
  vu.vf25.move(Mask::zw, vu.vf27);
  // sq.xyzw vf25, 3(vi00)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf25, 3);
  // 2048.0                     |  nop :i
  vu.I = 2048.0;
  // 255.0                      |  maxi.x vf17, vf00, I :i
  vu.vf17.maxi(Mask::x, vu.vf00, vu.I);   vu.I = 255.0;
  // -65537.0                   |  maxi.y vf17, vf00, I :i
  vu.vf17.maxi(Mask::y, vu.vf00, vu.I);   vu.I = -65537.0;
  // mr32.xyzw vf02, vf01       |  minii.z vf17, vf00, I
  vu.vf17.minii(Mask::z, vu.vf00, vu.I);   vu.vf02.mr32(Mask::xyzw, vu.vf01);
  // lq.xyzw vf22, 2(vi00)      |  minii.z vf18, vf00, I
  vu.vf18.minii(Mask::z, vu.vf00, vu.I);   lq_buffer_xyzw<DEBUG>(vu.vf22, 2);
  // 0.003921569                |  minii.z vf19, vf00, I :i
  vu.vf19.minii(Mask::z, vu.vf00, vu.I);   vu.I = 0.003921569;
  // sq.xyzw vf28, 4(vi00)      |  minii.w vf29, vf00, I :e
  vu.vf29.minii(Mask::w, vu.vf00, vu.I);   sq_buffer_xyzw<DEBUG>(vu.vf28, 4);
  // mr32.xyzw vf03, vf02       |  nop
  vu.vf03.mr32(Mask::xyzw, vu.vf02);
  return;

  ENTER_17:
  // iaddi vi07, vi00, 0x1      |  nop
  vu.vi07 = 1;
// BRANCH!
  // b L2                       |  nop
  bc = true;
  // isw.w vi07, 1(vi00)        |  nop
  isw_buffer(Mask::w, vu.vi07, 1);
  if (bc) { goto L2; }


  ENTER_20:
  // iaddi vi07, vi00, 0x0      |  nop
  vu.vi07 = 0;
  L2:
  // lq.xyzw vf25, 139(vi00)    |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf25, 139);
  // lq.xyzw vf26, 3(vi00)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf26, 3);
  // lq.xyz vf01, 132(vi00)     |  nop
  lq_buffer_xyz<DEBUG>(vu.vf01, 132);
  // lq.xyz vf02, 133(vi00)     |  nop
  lq_buffer_xyz<DEBUG>(vu.vf02, 133);
  // lq.xyz vf03, 134(vi00)     |  addy.xy vf19, vf00, vf25
  vu.vf19.add(Mask::xy, vu.vf00, vu.vf25.y());   lq_buffer_xyz<DEBUG>(vu.vf03, 134);
  // lq.xyzw vf04, 135(vi00)    |  mulx.xyzw vf26, vf26, vf25
  vu.vf26.mul_xyzw(vu.vf26, vu.vf25.x());   lq_buffer_xyzw<DEBUG>(vu.vf04, 135);
  // lq.xyzw vf05, 136(vi00)    |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf05, 136);
  // lq.xyzw vf06, 137(vi00)    |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf06, 137);
  // lq.xyzw vf07, 138(vi00)    |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf07, 138);
// BRANCH!
  // b L4                       |  nop
  bc = true;
  // sq.xyzw vf26, 5(vi00)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf26, 5);
  if (bc) { goto L4; }

  ENTER_32:

  // iaddi vi07, vi00, 0x1      |  nop
  vu.vi07 = 1;
// BRANCH!
  // b L4                       |  nop
  bc = true;
  // isw.w vi07, 1(vi00)        |  nop
  isw_buffer(Mask::w, vu.vi07, 1);
  if (bc) { goto L4; }


  ENTER_35:
  // iaddi vi07, vi00, 0x0      |  nop
  vu.vi07 = 0;
  L4:
  // lq.xyzw vf28, 139(vi00)    |  minix.xyzw vf15, vf00, vf00
  vu.vf15.mini_xyzw(vu.vf00, vu.vf00.x());   lq_buffer_xyzw<DEBUG>(vu.vf28, 139);
  // xtop vi15                  |  nop
  vu.vi15 = xtop();
  // iaddiu vi12, vi15, 0x8c    |  nop
  vu.vi12 = vu.vi15 + 0x8c; /* 140 */
// BRANCH!
  // ibeq vi00, vi15, L5        |  nop
  bc = (vu.vi15 == 0);
  // ilwr.w vi03, vi12          |  maxz.xy vf18, vf00, vf28
  vu.vf18.max(Mask::xy, vu.vf00, vu.vf28.z());   ilw_buffer(Mask::w, vu.vi03, vu.vi12);
  if (bc) { goto L5; }


  // nop                        |  maxw.xy vf18, vf00, vf28
  vu.vf18.max(Mask::xy, vu.vf00, vu.vf28.w());
  L5:
  // ilw.w vi10, 133(vi00)      |  nop
  ilw_buffer(Mask::w, vu.vi10, 133);
  // iaddiu vi15, vi15, 0x173   |  nop
  vu.vi15 = vu.vi15 + 0x173; /* 371 */
  // ilw.y vi02, 2(vi12)        |  nop
  ilw_buffer(Mask::y, vu.vi02, vu.vi12 + 2);
  // lq.xyzw vf14, 0(vi00)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf14, 0);
// BRANCH!
  // ibeq vi00, vi10, L6        |  nop
  bc = (vu.vi10 == 0);
  // iadd vi03, vi03, vi12      |  nop
  vu.vi03 = vu.vi03 + vu.vi12;
  if (bc) { goto L6; }


  // mr32.xyzw vf27, vf14       |  nop
  vu.vf27.mr32(Mask::xyzw, vu.vf14);
  // ilw.w vi11, 134(vi00)      |  nop
  ilw_buffer(Mask::w, vu.vi11, 134);
  // iaddiu vi13, vi00, 0x42    |  nop
  vu.vi13 = 0x42; /* 66 */

  // mr32.y vf14, vf27          |  nop
  vu.vf14.mr32(Mask::y, vu.vf27);
  L6:
  // ilwr.w vi09, vi03          |  nop
  ilw_buffer(Mask::w, vu.vi09, vu.vi03);
  // lqi.xyzw vf27, vi03        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi03++);
  // ilw.x vi04, 1(vi12)        |  nop
  ilw_buffer(Mask::x, vu.vi04, vu.vi12 + 1);
  // iaddiu vi05, vi00, 0x7f    |  addw.xyz vf15, vf15, vf00
  vu.vf15.add(Mask::xyz, vu.vf15, vu.vf00.w());   vu.vi05 = 0x7f; /* 127 */

  // iand vi09, vi09, vi05      |  nop
  vu.vi09 = vu.vi09 & vu.vi05;
  // ilw.y vi06, 1(vi12)        |  miniz.w vf19, vf00, vf27
  vu.vf19.mini(Mask::w, vu.vf00, vu.vf27.z());   ilw_buffer(Mask::y, vu.vi06, vu.vi12 + 1);
// BRANCH!
  // ibeq vi00, vi02, L8        |  miniy.w vf18, vf00, vf27
  vu.vf18.mini(Mask::w, vu.vf00, vu.vf27.y());   bc = (vu.vi02 == 0);
  // ilwr.z vi01, vi12          |  minix.w vf17, vf00, vf27
  vu.vf17.mini(Mask::w, vu.vf00, vu.vf27.x());   ilw_buffer(Mask::z, vu.vi01, vu.vi12);
  if (bc) { goto L8; }


// BRANCH!
  // ibne vi00, vi09, L7        |  nop
  bc = (vu.vi09 != 0);
  // sq.yzw vf14, 0(vi15)       |  nop
  sq_buffer(Mask::yzw, vu.vf14, vu.vi15);
  if (bc) { goto L7; }


  // iaddiu vi02, vi02, 0x4000  |  nop
  vu.vi02 = vu.vi02 + 0x4000; /* 16384 */
  // iaddiu vi02, vi02, 0x4000  |  nop
  vu.vi02 = vu.vi02 + 0x4000; /* 16384 */
  // iswr.x vi02, vi15          |  nop
  isw_buffer(Mask::x, vu.vi02, vu.vi15);
// BRANCH!
  // b L11                      |  nop
  bc = true;
  // nop                        |  nop

  if (bc) { goto L11; }


  L7:
  // iswr.x vi02, vi15          |  nop
  isw_buffer(Mask::x, vu.vi02, vu.vi15);
  L8:
  // lq.xyzw vf13, 1(vi00)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf13, 1);
  L9:
  // ilwr.w vi02, vi03          |  nop
  ilw_buffer(Mask::w, vu.vi02, vu.vi03);
  // lqi.xyzw vf08, vi03        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf08, vu.vi03++);
  // lqi.xyzw vf09, vi03        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf09, vu.vi03++);
  // lqi.xyzw vf10, vi03        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf10, vu.vi03++);
  // lqi.xyzw vf11, vi03        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi03++);
  // lqi.xyzw vf12, vi03        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi03++);
  // iadd vi02, vi02, vi15      |  nop
  vu.vi02 = vu.vi02 + vu.vi15;
  // mtir vi08, vf09.w          |  nop
  vu.vi08 = vu.vf09.w_as_u16();
  // sqi.xyzw vf13, vi02        |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi02++);
  // sqi.xyzw vf08, vi02        |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf08, vu.vi02++);
  // sqi.xyzw vf09, vi02        |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf09, vu.vi02++);
  // mfir.x vf14, vi08          |  nop
  vu.vf14.mfir(Mask::x, vu.vi08);
  // sqi.xyzw vf10, vi02        |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf10, vu.vi02++);
  // sqi.xyzw vf11, vi02        |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi02++);
// BRANCH!
  // ibeq vi00, vi10, L10       |  nop
  bc = (vu.vi10 == 0);
  // sqi.xyzw vf12, vi02        |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi02++);
  if (bc) { goto L10; }


  // mtir vi14, vf12.z          |  nop
  vu.vi14 = vu.vf12.z_as_u16();
  // isw.x vi10, -1(vi02)       |  nop
  isw_buffer(Mask::x, vu.vi10, vu.vi02 + -1);
// BRANCH!
  // ibeq vi14, vi13, L10       |  nop
  bc = (vu.vi14 == vu.vi13);
  // isw.y vi11, -1(vi02)       |  nop
  isw_buffer(Mask::y, vu.vi11, vu.vi02 + -1);
  if (bc) { goto L10; }


  // ilw.x vi13, -4(vi02)       |  nop
  ilw_buffer(Mask::x, vu.vi13, vu.vi02 + -4);
  // isubiu vi14, vi00, 0x1d    |  nop
  vu.vi14 = -29;

  // iand vi13, vi13, vi14      |  nop
  vu.vi13 = vu.vi13 & vu.vi14;
  // iaddi vi13, vi13, 0xc      |  nop
  vu.vi13 = vu.vi13 + 12;
  // isw.x vi13, -4(vi02)       |  nop
  isw_buffer(Mask::x, vu.vi13, vu.vi02 + -4);
  // iaddiu vi13, vi00, 0x42    |  nop
  vu.vi13 = 0x42; /* 66 */

  // isw.z vi13, -1(vi02)       |  nop
  isw_buffer(Mask::z, vu.vi13, vu.vi02 + -1);
  L10:
// BRANCH!
  // ibgtz vi08, L9             |  nop
  bc = ((s16)vu.vi08) > 0;
  // sq.xyzw vf14, 0(vi02)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi02);
  if (bc) { goto L9; }


  L11:
  // lq.xyzw vf28, 3(vi00)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf28, 3);
  // ilw.y vi08, 3(vi12)        |  nop
  ilw_buffer(Mask::y, vu.vi08, vu.vi12 + 3);
  // lq.xyzw vf16, 5(vi00)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf16, 5);
  // lq.xyzw vf20, 4(vi00)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf20, 4);
  // ilw.z vi09, 3(vi12)        |  mul.xyzw vf27, vf28, vf15
  vu.vf27.mul_xyzw(vu.vf28, vu.vf15);   ilw_buffer(Mask::z, vu.vi09, vu.vi12 + 3);
  // ior vi11, vi08, vi00       |  mul.xyzw vf28, vf28, vf00
  vu.vf28.mul_xyzw(vu.vf28, vu.vf00);   vu.vi11 = vu.vi08;
// BRANCH!
  // ibeq vi00, vi08, L13       |  mul.xyzw vf15, vf16, vf15
  vu.vf15.mul_xyzw(vu.vf16, vu.vf15);   bc = (vu.vi08 == 0);
  // iaddi vi13, vi12, 0x3      |  mul.xyzw vf16, vf16, vf00
  vu.vf16.mul_xyzw(vu.vf16, vu.vf00);   vu.vi13 = vu.vi12 + 3;
  if (bc) { goto L13; }


  L12:
  // lq.xyzw vf08, 0(vi08)      |  addax.xyzw vf20, vf00
  vu.acc.adda(Mask::xyzw, vu.vf20, vu.vf00.x());   lq_buffer_xyzw<DEBUG>(vu.vf08, vu.vi08);
  // lq.xyzw vf10, 1(vi08)      |  madda.xyzw ACC, vf27, vf25
  vu.acc.madda_xyzw(vu.vf27, vu.vf25);   lq_buffer_xyzw<DEBUG>(vu.vf10, vu.vi08 + 1);
  // lq.xyzw vf12, 2(vi08)      |  maddz.xyzw vf26, vf28, vf25
  vu.acc.madd_xyzw(vu.vf26, vu.vf28, vu.vf25.z());   lq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi08 + 2);
  // lq.xyzw vf25, 3(vi08)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi08 + 3);
  // sq.xyzw vf09, 0(vi11)      |  mula.xyzw ACC, vf15, vf08
  vu.acc.mula_xyzw(vu.vf15, vu.vf08);   sq_buffer_xyzw<DEBUG>(vu.vf09, vu.vi11);
  // sq.xyzw vf11, 1(vi11)      |  maddz.xyzw vf09, vf16, vf08
  vu.acc.madd_xyzw(vu.vf09, vu.vf16, vu.vf08.z());   sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi11 + 1);
  // sq.xyzw vf13, 2(vi11)      |  mula.xyzw ACC, vf15, vf10
  vu.acc.mula_xyzw(vu.vf15, vu.vf10);   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi11 + 2);
  // sq.xyzw vf26, 3(vi11)      |  maddz.xyzw vf11, vf16, vf10
  vu.acc.madd_xyzw(vu.vf11, vu.vf16, vu.vf10.z());   sq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi11 + 3);
// BRANCH!
  // ibeq vi00, vi08, L13       |  mula.xyzw ACC, vf15, vf12
  vu.acc.mula_xyzw(vu.vf15, vu.vf12);   bc = (vu.vi08 == 0);
  // ilwr.w vi10, vi13          |  maddz.xyzw vf13, vf16, vf12
  vu.acc.madd_xyzw(vu.vf13, vu.vf16, vu.vf12.z());   ilw_buffer(Mask::w, vu.vi10, vu.vi13);
  if (bc) { goto L13; }


  // lq.xyzw vf08, 0(vi09)      |  addax.xyzw vf20, vf00
  vu.acc.adda(Mask::xyzw, vu.vf20, vu.vf00.x());   lq_buffer_xyzw<DEBUG>(vu.vf08, vu.vi09);
  // lq.xyzw vf10, 1(vi09)      |  madda.xyzw ACC, vf27, vf25
  vu.acc.madda_xyzw(vu.vf27, vu.vf25);   lq_buffer_xyzw<DEBUG>(vu.vf10, vu.vi09 + 1);
  // lq.xyzw vf12, 2(vi09)      |  maddz.xyzw vf26, vf28, vf25
  vu.acc.madd_xyzw(vu.vf26, vu.vf28, vu.vf25.z());   lq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi09 + 2);
  // lq.xyzw vf25, 3(vi09)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi09 + 3);
  // sq.xyzw vf09, 0(vi08)      |  mula.xyzw ACC, vf15, vf08
  vu.acc.mula_xyzw(vu.vf15, vu.vf08);   sq_buffer_xyzw<DEBUG>(vu.vf09, vu.vi08);
  // sq.xyzw vf11, 1(vi08)      |  maddz.xyzw vf09, vf16, vf08
  vu.acc.madd_xyzw(vu.vf09, vu.vf16, vu.vf08.z());   sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi08 + 1);
  // sq.xyzw vf13, 2(vi08)      |  mula.xyzw ACC, vf15, vf10
  vu.acc.mula_xyzw(vu.vf15, vu.vf10);   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi08 + 2);
  // sq.xyzw vf26, 3(vi08)      |  maddz.xyzw vf11, vf16, vf10
  vu.acc.madd_xyzw(vu.vf11, vu.vf16, vu.vf10.z());   sq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi08 + 3);
// BRANCH!
  // ibeq vi00, vi09, L13       |  mula.xyzw ACC, vf15, vf12
  vu.acc.mula_xyzw(vu.vf15, vu.vf12);   bc = (vu.vi09 == 0);
  // ilw.x vi11, 1(vi13)        |  maddz.xyzw vf13, vf16, vf12
  vu.acc.madd_xyzw(vu.vf13, vu.vf16, vu.vf12.z());   ilw_buffer(Mask::x, vu.vi11, vu.vi13 + 1);
  if (bc) { goto L13; }


  // lq.xyzw vf08, 0(vi10)      |  addax.xyzw vf20, vf00
  vu.acc.adda(Mask::xyzw, vu.vf20, vu.vf00.x());   lq_buffer_xyzw<DEBUG>(vu.vf08, vu.vi10);
  // lq.xyzw vf10, 1(vi10)      |  madda.xyzw ACC, vf27, vf25
  vu.acc.madda_xyzw(vu.vf27, vu.vf25);   lq_buffer_xyzw<DEBUG>(vu.vf10, vu.vi10 + 1);
  // lq.xyzw vf12, 2(vi10)      |  maddz.xyzw vf26, vf28, vf25
  vu.acc.madd_xyzw(vu.vf26, vu.vf28, vu.vf25.z());   lq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi10 + 2);
  // lq.xyzw vf25, 3(vi10)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi10 + 3);
  // sq.xyzw vf09, 0(vi09)      |  mula.xyzw ACC, vf15, vf08
  vu.acc.mula_xyzw(vu.vf15, vu.vf08);   sq_buffer_xyzw<DEBUG>(vu.vf09, vu.vi09);
  // sq.xyzw vf11, 1(vi09)      |  maddz.xyzw vf09, vf16, vf08
  vu.acc.madd_xyzw(vu.vf09, vu.vf16, vu.vf08.z());   sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi09 + 1);
  // sq.xyzw vf13, 2(vi09)      |  mula.xyzw ACC, vf15, vf10
  vu.acc.mula_xyzw(vu.vf15, vu.vf10);   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi09 + 2);
  // sq.xyzw vf26, 3(vi09)      |  maddz.xyzw vf11, vf16, vf10
  vu.acc.madd_xyzw(vu.vf11, vu.vf16, vu.vf10.z());   sq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi09 + 3);
// BRANCH!
  // ibeq vi00, vi10, L13       |  mula.xyzw ACC, vf15, vf12
  vu.acc.mula_xyzw(vu.vf15, vu.vf12);   bc = (vu.vi10 == 0);
  // ilw.y vi08, 1(vi13)        |  maddz.xyzw vf13, vf16, vf12
  vu.acc.madd_xyzw(vu.vf13, vu.vf16, vu.vf12.z());   ilw_buffer(Mask::y, vu.vi08, vu.vi13 + 1);
  if (bc) { goto L13; }


  // lq.xyzw vf08, 0(vi11)      |  addax.xyzw vf20, vf00
  vu.acc.adda(Mask::xyzw, vu.vf20, vu.vf00.x());   lq_buffer_xyzw<DEBUG>(vu.vf08, vu.vi11);
  // lq.xyzw vf10, 1(vi11)      |  madda.xyzw ACC, vf27, vf25
  vu.acc.madda_xyzw(vu.vf27, vu.vf25);   lq_buffer_xyzw<DEBUG>(vu.vf10, vu.vi11 + 1);
  // lq.xyzw vf12, 2(vi11)      |  maddz.xyzw vf26, vf28, vf25
  vu.acc.madd_xyzw(vu.vf26, vu.vf28, vu.vf25.z());   lq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi11 + 2);
  // lq.xyzw vf25, 3(vi11)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi11 + 3);
  // sq.xyzw vf09, 0(vi10)      |  mula.xyzw ACC, vf15, vf08
  vu.acc.mula_xyzw(vu.vf15, vu.vf08);   sq_buffer_xyzw<DEBUG>(vu.vf09, vu.vi10);
  // sq.xyzw vf11, 1(vi10)      |  maddz.xyzw vf09, vf16, vf08
  vu.acc.madd_xyzw(vu.vf09, vu.vf16, vu.vf08.z());   sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi10 + 1);
  // sq.xyzw vf13, 2(vi10)      |  mula.xyzw ACC, vf15, vf10
  vu.acc.mula_xyzw(vu.vf15, vu.vf10);   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi10 + 2);
  // sq.xyzw vf26, 3(vi10)      |  maddz.xyzw vf11, vf16, vf10
  vu.acc.madd_xyzw(vu.vf11, vu.vf16, vu.vf10.z());   sq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi10 + 3);
  // iaddi vi13, vi13, 0x1      |  nop
  vu.vi13 = vu.vi13 + 1;
// BRANCH!
  // ibne vi00, vi11, L12       |  mula.xyzw ACC, vf15, vf12
  vu.acc.mula_xyzw(vu.vf15, vu.vf12);   bc = (vu.vi11 != 0);
  // ilwr.z vi09, vi13          |  maddz.xyzw vf13, vf16, vf12
  vu.acc.madd_xyzw(vu.vf13, vu.vf16, vu.vf12.z());   ilw_buffer(Mask::z, vu.vi09, vu.vi13);
  if (bc) { goto L12; }


  L13:
  // ilw.x vi02, 3(vi12)        |  nop
  ilw_buffer(Mask::x, vu.vi02, vu.vi12 + 3);
// BRANCH!
  // ibeq vi00, vi04, L25       |  nop
  bc = (vu.vi04 == 0);
  // iadd vi01, vi01, vi12      |  nop
  vu.vi01 = vu.vi01 + vu.vi12;
  if (bc) { goto L25; }


  // ilwr.x vi08, vi01          |  nop
  ilw_buffer(Mask::x, vu.vi08, vu.vi01);
  // lqi.xyzw vf08, vi01        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf08, vu.vi01++);
  // lqi.xyzw vf11, vi01        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi01++);
  // lqi.xyzw vf14, vi01        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi01++);
  // lq.xyz vf29, 4(vi08)       |  nop
  lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi08 + 4);
  // lq.xyz vf30, 5(vi08)       |  add.zw vf08, vf08, vf17
  vu.vf08.add(Mask::zw, vu.vf08, vu.vf17);   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi08 + 5);
  // lq.xyzw vf31, 6(vi08)      |  add.xyzw vf11, vf11, vf18
  vu.vf11.add_xyzw(vu.vf11, vu.vf18);   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi08 + 6);
  // iaddi vi04, vi04, -0x1     |  add.xyzw vf14, vf14, vf19
  vu.vf14.add_xyzw(vu.vf14, vu.vf19);   vu.vi04 = vu.vi04 + -1;
  // iadd vi02, vi02, vi12      |  nop
  vu.vi02 = vu.vi02 + vu.vi12;
  // lqi.xyzw vf24, vi02        |  mulaz.xyzw ACC, vf29, vf08
  vu.acc.mula_xyzw(vu.vf29, vu.vf08.z());   lq_buffer_xyzw<DEBUG>(vu.vf24, vu.vi02++);
  // mtir vi10, vf11.x          |  maddaz.xyzw ACC, vf30, vf11
  vu.acc.madda_xyzw(vu.vf30, vu.vf11.z());   vu.vi10 = vu.vf11.x_as_u16();
  // mtir vi13, vf11.y          |  maddz.xyz vf11, vf31, vf14
  vu.vi13 = vu.vf11.y_as_u16(); vu.acc.madd_xyz(vu.vf11, vu.vf31, vu.vf14.z());
  // lq.xyzw vf25, 0(vi08)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi08);
  // lq.xyzw vf26, 1(vi08)      |  itof0.xyzw vf24, vf24
  vu.vf24.itof0(Mask::xyzw, vu.vf24);   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi08 + 1);
  // lq.xyzw vf27, 2(vi08)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi08 + 2);
  // erleng.xyz P, vf11         |  nop
  vu.P = erleng(vu.vf11); /* TODO erleng */
  // lq.xyzw vf28, 3(vi08)      |  mulaw.xyzw ACC, vf25, vf08
  vu.acc.mula_xyzw(vu.vf25, vu.vf08.w());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi08 + 3);
  // ior vi15, vi07, vi00       |  maddaw.xyzw ACC, vf26, vf11
  vu.acc.madda_xyzw(vu.vf26, vu.vf11.w());   vu.vi15 = vu.vi07;
  // mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
  vu.acc.madd_xyzw(vu.vf08, vu.vf27, vu.vf14.w());   vu.vf14.z() = 1.f;
  // lqi.xyzw vf09, vi01        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf09, vu.vi01++);
  // ilwr.y vi03, vi12          |  nop
  ilw_buffer(Mask::y, vu.vi03, vu.vi12);
  // ilw.z vi07, 1(vi12)        |  nop
  ilw_buffer(Mask::z, vu.vi07, vu.vi12 + 1);
  // lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  vu.vf08.add_xyzw(vu.vf08, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi01++);
  // lqi.xyzw vf15, vi01        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi01++);
  // mtir vi08, vf09.x          |  nop
  vu.vi08 = vu.vf09.x_as_u16();
// BRANCH!
  // ibeq vi00, vi15, L14       |  nop
  bc = (vu.vi15 == 0);
  // iadd vi03, vi03, vi12      |  nop
  vu.vi03 = vu.vi03 + vu.vi12;
  if (bc) { goto L14; }


  // nop                        |  miniw.w vf08, vf08, vf01
  vu.vf08.mini(Mask::w, vu.vf08, vu.vf01.w());
  L14:
  // div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  vu.vf09.add(Mask::zw, vu.vf09, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf08.w();

  // iadd vi04, vi04, vi03      |  add.xyzw vf12, vf12, vf18
  vu.vf12.add_xyzw(vu.vf12, vu.vf18);   vu.vi04 = vu.vi04 + vu.vi03;
  // lq.xyz vf29, 4(vi08)       |  add.xyzw vf15, vf15, vf19
  vu.vf15.add_xyzw(vu.vf15, vu.vf19);   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi08 + 4);
  // lq.xyz vf30, 5(vi08)       |  nop
  lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi08 + 5);
  // iadd vi06, vi06, vi04      |  nop
  vu.vi06 = vu.vi06 + vu.vi04;
  // lq.xyzw vf31, 6(vi08)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi08 + 6);
  // lq.xyzw vf25, 0(vi08)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi08);
  // lq.xyzw vf26, 1(vi08)      |  mul.xyz vf08, vf08, Q
  vu.vf08.mul(Mask::xyz, vu.vf08, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi08 + 1);
  // mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  vu.vf14.mul_xyzw(vu.vf14, vu.Q); /* TODO mulq */   vu.vi11 = vu.vf12.x_as_u16();
  // mtir vi14, vf12.y          |  nop
  vu.vi14 = vu.vf12.y_as_u16();
  // lq.xyzw vf27, 2(vi08)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi08 + 2);
  // lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
  vu.vf08.add_xyzw(vu.vf08, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  // iadd vi07, vi07, vi06      |  mulaz.xyzw ACC, vf29, vf09
  vu.acc.mula_xyzw(vu.vf29, vu.vf09.z());   vu.vi07 = vu.vi07 + vu.vi06;
  // lq.xyzw vf28, 3(vi08)      |  maddaz.xyzw ACC, vf30, vf12
  vu.acc.madda_xyzw(vu.vf30, vu.vf12.z());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi08 + 3);
  // mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  vu.acc.madd_xyz(vu.vf12, vu.vf31, vu.vf15.z());   vu.vf20.mfp(Mask::w, vu.P);
  // nop                        |  nop

  // 1024.0                     |  miniw.w vf08, vf08, vf03 :i
  vu.vf08.mini(Mask::w, vu.vf08, vu.vf03.w());   vu.I = 1024.0;
// BRANCH!
  // ibne vi00, vi15, L82       |  mulaw.xyzw ACC, vf25, vf09
  vu.acc.mula_xyzw(vu.vf25, vu.vf09.w());   bc = (vu.vi15 != 0);
  // ilw.y vi09, -6(vi01)       |  mulw.xyzw vf11, vf11, vf20
  vu.vf11.mul_xyzw(vu.vf11, vu.vf20.w());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  if (bc) { goto L82; }


  // erleng.xyz P, vf12         |  nop
  vu.P = erleng(vu.vf12); /* TODO erleng */
  // nop                        |  maddaw.xyzw ACC, vf26, vf12
  vu.acc.madda_xyzw(vu.vf26, vu.vf12.w());
  // mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  vu.acc.madd_xyzw(vu.vf09, vu.vf27, vu.vf15.w());   vu.vf15.z() = 1.f;
  // lqi.xyzw vf10, vi01        |  mulax.xyzw ACC, vf01, vf11
  vu.acc.mula_xyzw(vu.vf01, vu.vf11.x());   lq_buffer_xyzw<DEBUG>(vu.vf10, vu.vi01++);
// BRANCH!
  // ibne vi04, vi03, L16       |  madday.xyzw ACC, vf02, vf11
  vu.acc.madda_xyzw(vu.vf02, vu.vf11.y());   bc = (vu.vi04 != vu.vi03);
  // nop                        |  maddz.xyzw vf11, vf03, vf11
  vu.acc.madd_xyzw(vu.vf11, vu.vf03, vu.vf11.z());
  if (bc) { goto L16; }


// BRANCH!
  // ibne vi06, vi03, JUMP_1A1  |  nop
  bc = (vu.vi06 != vu.vi03);
  // nop                        |  nop

  if (bc) { goto JUMP_1A1; }


// BRANCH!
  // b L67                      |  nop
  bc = true;
  // nop                        |  nop

  if (bc) { goto L67; }


  L15:
  // lqi.xyzw vf10, vi01        |  mulax.xyzw ACC, vf01, vf11
  vu.acc.mula_xyzw(vu.vf01, vu.vf11.x());   lq_buffer_xyzw<DEBUG>(vu.vf10, vu.vi01++);
  // sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  vu.acc.madda_xyzw(vu.vf02, vu.vf11.y());   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi12 + 1);
  // sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
  vu.acc.madd_xyzw(vu.vf11, vu.vf03, vu.vf11.z());   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi15 + 1);
  L16:
  // lqi.xyzw vf13, vi01        |  add.xyzw vf09, vf09, vf28
  vu.vf09.add_xyzw(vu.vf09, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi01++);
  // lqi.xyzw vf16, vi01        |  maxw.w vf08, vf08, vf02
  vu.vf08.max(Mask::w, vu.vf08, vu.vf02.w());   lq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi01++);
  // mtir vi08, vf10.x          |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   vu.vi08 = vu.vf10.x_as_u16();
  // ilw.y vi09, -9(vi01)       |  maxx.xyzw vf11, vf11, vf00
  vu.vf11.max_xyzw(vu.vf11, vu.vf00.x());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -9);
  // div Q, vf01.w, vf09.w      |  add.zw vf10, vf10, vf17
  vu.vf10.add(Mask::zw, vu.vf10, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf09.w();

  // move.xyzw vf21, vf08       |  add.xyzw vf13, vf13, vf18
  vu.vf13.add_xyzw(vu.vf13, vu.vf18);   vu.vf21.move_xyzw(vu.vf08);
  // lq.xyz vf29, 4(vi08)       |  add.xyzw vf16, vf16, vf19
  vu.vf16.add_xyzw(vu.vf16, vu.vf19);   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi08 + 4);
  // lq.xyz vf30, 5(vi08)       |  mulax.xyzw ACC, vf04, vf11
  vu.acc.mula_xyzw(vu.vf04, vu.vf11.x());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi08 + 5);
// BRANCH!
  // ibgtz vi09, L17            |  madday.xyzw ACC, vf05, vf11
  vu.acc.madda_xyzw(vu.vf05, vu.vf11.y());   bc = ((s16)vu.vi09) > 0;
  // lq.xyzw vf31, 6(vi08)      |  maddaz.xyzw ACC, vf06, vf11
  vu.acc.madda_xyzw(vu.vf06, vu.vf11.z());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi08 + 6);
  if (bc) { goto L17; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L17:
  // lq.xyzw vf25, 0(vi08)      |  maddw.xyzw vf11, vf07, vf00
  vu.acc.madd_xyzw(vu.vf11, vu.vf07, vu.vf00.w());   lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi08);
  // lq.xyzw vf26, 1(vi08)      |  mul.xyz vf09, vf09, Q
  vu.vf09.mul(Mask::xyz, vu.vf09, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi08 + 1);
  // mtir vi12, vf13.x          |  mul.xyzw vf15, vf15, Q
  vu.vf15.mul_xyzw(vu.vf15, vu.Q); /* TODO mulq */   vu.vi12 = vu.vf13.x_as_u16();
  // mtir vi15, vf13.y          |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi15 = vu.vf13.y_as_u16();
  // lq.xyzw vf27, 2(vi08)      |  mul.xyzw vf11, vf11, vf23
  vu.vf11.mul_xyzw(vu.vf11, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi08 + 2);
  // lqi.xyzw vf23, vi03        |  add.xyzw vf09, vf09, vf22
  vu.vf09.add_xyzw(vu.vf09, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
// BRANCH!
  // ibne vi00, vi09, L18       |  mulaz.xyzw ACC, vf29, vf10
  vu.acc.mula_xyzw(vu.vf29, vu.vf10.z());   bc = (vu.vi09 != 0);
  // sq.xyzw vf21, 2(vi10)      |  maddaz.xyzw ACC, vf30, vf13
  vu.acc.madda_xyzw(vu.vf30, vu.vf13.z());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi10 + 2);
  if (bc) { goto L18; }


  // nop                        |  ftoi4.xyzw vf21, vf08
  vu.vf21.ftoi4(Mask::xyzw, vu.vf08);
  L18:
  // mfp.w vf20, P              |  maddz.xyz vf13, vf31, vf16
  vu.acc.madd_xyz(vu.vf13, vu.vf31, vu.vf16.z());   vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  vu.vf11.mini_xyzw(vu.vf11, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi10);
  // sq.xyzw vf14, 0(vi13)      |  miniw.w vf09, vf09, vf03
  vu.vf09.mini(Mask::w, vu.vf09, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi13);
  // sq.xyzw vf21, 2(vi13)      |  mulaw.xyzw ACC, vf25, vf10
  vu.acc.mula_xyzw(vu.vf25, vu.vf10.w());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi13 + 2);
  // lq.xyzw vf28, 3(vi08)      |  mulw.xyzw vf12, vf12, vf20
  vu.vf12.mul_xyzw(vu.vf12, vu.vf20.w());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi08 + 3);
  // erleng.xyz P, vf13         |  ftoi0.xyzw vf11, vf11
  vu.vf11.ftoi0(Mask::xyzw, vu.vf11);   vu.P = erleng(vu.vf13); /* TODO erleng */
// BRANCH!
  // ibne vi04, vi03, L19       |  maddaw.xyzw ACC, vf26, vf13
  vu.acc.madda_xyzw(vu.vf26, vu.vf13.w());   bc = (vu.vi04 != vu.vi03);
  // mr32.z vf16, vf00          |  maddw.xyzw vf10, vf27, vf16
  vu.acc.madd_xyzw(vu.vf10, vu.vf27, vu.vf16.w());   vu.vf16.z() = 1.f;
  if (bc) { goto L19; }


// BRANCH!
  // ibne vi06, vi03, L35       |  nop
  bc = (vu.vi06 != vu.vi03);
  // ilw.y vi09, -6(vi01)       |  nop
  ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  if (bc) { goto L35; }


// BRANCH!
  // ibne vi07, vi03, L72       |  nop
  bc = (vu.vi07 != vu.vi03);
  // nop                        |  nop

  if (bc) { goto L72; }


// BRANCH!
  // b L143                     |  nop
  bc = true;
  // nop                        |  nop

  if (bc) { goto L143; }


  L19:
  // lqi.xyzw vf08, vi01        |  mulax.xyzw ACC, vf01, vf12
  vu.acc.mula_xyzw(vu.vf01, vu.vf12.x());   lq_buffer_xyzw<DEBUG>(vu.vf08, vu.vi01++);
  // sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12
  vu.acc.madda_xyzw(vu.vf02, vu.vf12.y());   sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi10 + 1);
  // sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12
  vu.acc.madd_xyzw(vu.vf12, vu.vf03, vu.vf12.z());   sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi13 + 1);
  // lqi.xyzw vf11, vi01        |  add.xyzw vf10, vf10, vf28
  vu.vf10.add_xyzw(vu.vf10, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi01++);
  // lqi.xyzw vf14, vi01        |  maxw.w vf09, vf09, vf02
  vu.vf09.max(Mask::w, vu.vf09, vu.vf02.w());   lq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi01++);
  // mtir vi08, vf08.x          |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   vu.vi08 = vu.vf08.x_as_u16();
  // ilw.y vi09, -9(vi01)       |  maxx.xyzw vf12, vf12, vf00
  vu.vf12.max_xyzw(vu.vf12, vu.vf00.x());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -9);
  // div Q, vf01.w, vf10.w      |  add.zw vf08, vf08, vf17
  vu.vf08.add(Mask::zw, vu.vf08, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf10.w();

  // move.xyzw vf21, vf09       |  add.xyzw vf11, vf11, vf18
  vu.vf11.add_xyzw(vu.vf11, vu.vf18);   vu.vf21.move_xyzw(vu.vf09);
  // lq.xyz vf29, 4(vi08)       |  add.xyzw vf14, vf14, vf19
  vu.vf14.add_xyzw(vu.vf14, vu.vf19);   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi08 + 4);
  // lq.xyz vf30, 5(vi08)       |  mulax.xyzw ACC, vf04, vf12
  vu.acc.mula_xyzw(vu.vf04, vu.vf12.x());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi08 + 5);
// BRANCH!
  // ibgtz vi09, L20            |  madday.xyzw ACC, vf05, vf12
  vu.acc.madda_xyzw(vu.vf05, vu.vf12.y());   bc = ((s16)vu.vi09) > 0;
  // lq.xyzw vf31, 6(vi08)      |  maddaz.xyzw ACC, vf06, vf12
  vu.acc.madda_xyzw(vu.vf06, vu.vf12.z());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi08 + 6);
  if (bc) { goto L20; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L20:
  // lq.xyzw vf25, 0(vi08)      |  maddw.xyzw vf12, vf07, vf00
  vu.acc.madd_xyzw(vu.vf12, vu.vf07, vu.vf00.w());   lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi08);
  // lq.xyzw vf26, 1(vi08)      |  mul.xyz vf10, vf10, Q
  vu.vf10.mul(Mask::xyz, vu.vf10, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi08 + 1);
  // mtir vi10, vf11.x          |  mul.xyzw vf16, vf16, Q
  vu.vf16.mul_xyzw(vu.vf16, vu.Q); /* TODO mulq */   vu.vi10 = vu.vf11.x_as_u16();
  // mtir vi13, vf11.y          |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi13 = vu.vf11.y_as_u16();
  // lq.xyzw vf27, 2(vi08)      |  mul.xyzw vf12, vf12, vf23
  vu.vf12.mul_xyzw(vu.vf12, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi08 + 2);
  // lqi.xyzw vf23, vi03        |  add.xyzw vf10, vf10, vf22
  vu.vf10.add_xyzw(vu.vf10, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
// BRANCH!
  // ibne vi00, vi09, L21       |  mulaz.xyzw ACC, vf29, vf08
  vu.acc.mula_xyzw(vu.vf29, vu.vf08.z());   bc = (vu.vi09 != 0);
  // sq.xyzw vf21, 2(vi11)      |  maddaz.xyzw ACC, vf30, vf11
  vu.acc.madda_xyzw(vu.vf30, vu.vf11.z());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi11 + 2);
  if (bc) { goto L21; }


  // nop                        |  ftoi4.xyzw vf21, vf09
  vu.vf21.ftoi4(Mask::xyzw, vu.vf09);
  L21:
  // mfp.w vf20, P              |  maddz.xyz vf11, vf31, vf14
  vu.acc.madd_xyz(vu.vf11, vu.vf31, vu.vf14.z());   vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17
  vu.vf12.mini_xyzw(vu.vf12, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi11);
  // sq.xyzw vf15, 0(vi14)      |  miniw.w vf10, vf10, vf03
  vu.vf10.mini(Mask::w, vu.vf10, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi14);
  // sq.xyzw vf21, 2(vi14)      |  mulaw.xyzw ACC, vf25, vf08
  vu.acc.mula_xyzw(vu.vf25, vu.vf08.w());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi14 + 2);
  // lq.xyzw vf28, 3(vi08)      |  mulw.xyzw vf13, vf13, vf20
  vu.vf13.mul_xyzw(vu.vf13, vu.vf20.w());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi08 + 3);
  // erleng.xyz P, vf11         |  ftoi0.xyzw vf12, vf12
  vu.vf12.ftoi0(Mask::xyzw, vu.vf12);   vu.P = erleng(vu.vf11); /* TODO erleng */
// BRANCH!
  // ibne vi04, vi03, L22       |  maddaw.xyzw ACC, vf26, vf11
  vu.acc.madda_xyzw(vu.vf26, vu.vf11.w());   bc = (vu.vi04 != vu.vi03);
  // mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
  vu.acc.madd_xyzw(vu.vf08, vu.vf27, vu.vf14.w());   vu.vf14.z() = 1.f;
  if (bc) { goto L22; }


// BRANCH!
  // ibne vi06, vi03, L40       |  nop
  bc = (vu.vi06 != vu.vi03);
  // ilw.y vi09, -6(vi01)       |  nop
  ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  if (bc) { goto L40; }


// BRANCH!
  // ibne vi07, vi03, L77       |  nop
  bc = (vu.vi07 != vu.vi03);
  // nop                        |  nop

  if (bc) { goto L77; }


// BRANCH!
  // b L153                     |  nop
  bc = true;
  // nop                        |  nop

  if (bc) { goto L153; }


  L22:
  // lqi.xyzw vf09, vi01        |  mulax.xyzw ACC, vf01, vf13
  vu.acc.mula_xyzw(vu.vf01, vu.vf13.x());   lq_buffer_xyzw<DEBUG>(vu.vf09, vu.vi01++);
  // sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13
  vu.acc.madda_xyzw(vu.vf02, vu.vf13.y());   sq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi11 + 1);
  // sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13
  vu.acc.madd_xyzw(vu.vf13, vu.vf03, vu.vf13.z());   sq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi14 + 1);
  // lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  vu.vf08.add_xyzw(vu.vf08, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi01++);
  // lqi.xyzw vf15, vi01        |  maxw.w vf10, vf10, vf02
  vu.vf10.max(Mask::w, vu.vf10, vu.vf02.w());   lq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi01++);
  // mtir vi08, vf09.x          |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   vu.vi08 = vu.vf09.x_as_u16();
  // ilw.y vi09, -9(vi01)       |  maxx.xyzw vf13, vf13, vf00
  vu.vf13.max_xyzw(vu.vf13, vu.vf00.x());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -9);
  // div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  vu.vf09.add(Mask::zw, vu.vf09, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf08.w();

  // move.xyzw vf21, vf10       |  add.xyzw vf12, vf12, vf18
  vu.vf12.add_xyzw(vu.vf12, vu.vf18);   vu.vf21.move_xyzw(vu.vf10);
  // lq.xyz vf29, 4(vi08)       |  add.xyzw vf15, vf15, vf19
  vu.vf15.add_xyzw(vu.vf15, vu.vf19);   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi08 + 4);
  // lq.xyz vf30, 5(vi08)       |  mulax.xyzw ACC, vf04, vf13
  vu.acc.mula_xyzw(vu.vf04, vu.vf13.x());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi08 + 5);
// BRANCH!
  // ibgtz vi09, L23            |  madday.xyzw ACC, vf05, vf13
  vu.acc.madda_xyzw(vu.vf05, vu.vf13.y());   bc = ((s16)vu.vi09) > 0;
  // lq.xyzw vf31, 6(vi08)      |  maddaz.xyzw ACC, vf06, vf13
  vu.acc.madda_xyzw(vu.vf06, vu.vf13.z());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi08 + 6);
  if (bc) { goto L23; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L23:
  // lq.xyzw vf25, 0(vi08)      |  maddw.xyzw vf13, vf07, vf00
  vu.acc.madd_xyzw(vu.vf13, vu.vf07, vu.vf00.w());   lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi08);
  // lq.xyzw vf26, 1(vi08)      |  mul.xyz vf08, vf08, Q
  vu.vf08.mul(Mask::xyz, vu.vf08, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi08 + 1);
  // mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  vu.vf14.mul_xyzw(vu.vf14, vu.Q); /* TODO mulq */   vu.vi11 = vu.vf12.x_as_u16();
  // mtir vi14, vf12.y          |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi14 = vu.vf12.y_as_u16();
  // lq.xyzw vf27, 2(vi08)      |  mul.xyzw vf13, vf13, vf23
  vu.vf13.mul_xyzw(vu.vf13, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi08 + 2);
  // lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
  vu.vf08.add_xyzw(vu.vf08, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
// BRANCH!
  // ibne vi00, vi09, L24       |  mulaz.xyzw ACC, vf29, vf09
  vu.acc.mula_xyzw(vu.vf29, vu.vf09.z());   bc = (vu.vi09 != 0);
  // sq.xyzw vf21, 2(vi12)      |  maddaz.xyzw ACC, vf30, vf12
  vu.acc.madda_xyzw(vu.vf30, vu.vf12.z());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi12 + 2);
  if (bc) { goto L24; }


  // nop                        |  ftoi4.xyzw vf21, vf10
  vu.vf21.ftoi4(Mask::xyzw, vu.vf10);
  L24:
  // mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  vu.acc.madd_xyz(vu.vf12, vu.vf31, vu.vf15.z());   vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  vu.vf13.mini_xyzw(vu.vf13, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi12);
  // sq.xyzw vf16, 0(vi15)      |  miniw.w vf08, vf08, vf03
  vu.vf08.mini(Mask::w, vu.vf08, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi15);
  // sq.xyzw vf21, 2(vi15)      |  mulaw.xyzw ACC, vf25, vf09
  vu.acc.mula_xyzw(vu.vf25, vu.vf09.w());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi15 + 2);
  // lq.xyzw vf28, 3(vi08)      |  mulw.xyzw vf11, vf11, vf20
  vu.vf11.mul_xyzw(vu.vf11, vu.vf20.w());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi08 + 3);
  // erleng.xyz P, vf12         |  ftoi0.xyzw vf13, vf13
  vu.vf13.ftoi0(Mask::xyzw, vu.vf13);   vu.P = erleng(vu.vf12); /* TODO erleng */
// BRANCH!
  // ibne vi04, vi03, L15       |  maddaw.xyzw ACC, vf26, vf12
  vu.acc.madda_xyzw(vu.vf26, vu.vf12.w());   bc = (vu.vi04 != vu.vi03);
  // mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  vu.acc.madd_xyzw(vu.vf09, vu.vf27, vu.vf15.w());   vu.vf15.z() = 1.f;
  if (bc) { goto L15; }


// BRANCH!
  // ibne vi06, vi03, L29       |  nop
  bc = (vu.vi06 != vu.vi03);
  // ilw.y vi09, -6(vi01)       |  nop
  ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  if (bc) { goto L29; }


// BRANCH!
  // ibne vi07, vi03, L66       |  nop
  bc = (vu.vi07 != vu.vi03);
  // nop                        |  nop

  if (bc) { goto L66; }


// BRANCH!
  // b L163                     |  nop
  bc = true;
  // nop                        |  nop

  if (bc) { goto L163; }


  L25:
// BRANCH!
  // ibeq vi00, vi06, L61       |  nop
  bc = (vu.vi06 == 0);
  // iadd vi02, vi02, vi12      |  nop
  vu.vi02 = vu.vi02 + vu.vi12;
  if (bc) { goto L61; }


  // lqi.xyzw vf08, vi01        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf08, vu.vi01++);
  // lqi.xyzw vf24, vi02        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf24, vu.vi02++);
  // lqi.xyzw vf11, vi01        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi01++);
  // lqi.xyzw vf14, vi01        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi01++);
  // mtir vi10, vf08.x          |  nop
  vu.vi10 = vu.vf08.x_as_u16();
  // mtir vi13, vf08.y          |  itof0.xyzw vf24, vf24
  vu.vf24.itof0(Mask::xyzw, vu.vf24);   vu.vi13 = vu.vf08.y_as_u16();
  // iaddi vi06, vi06, -0x1     |  add.zw vf08, vf08, vf17
  vu.vf08.add(Mask::zw, vu.vf08, vu.vf17);   vu.vi06 = vu.vi06 + -1;
  // nop                        |  add.xyzw vf11, vf11, vf18
  vu.vf11.add_xyzw(vu.vf11, vu.vf18);
  // iand vi10, vi10, vi05      |  add.xyzw vf14, vf14, vf19
  vu.vf14.add_xyzw(vu.vf14, vu.vf19);   vu.vi10 = vu.vi10 & vu.vi05;
  // nop                        |  mulw.xyzw vf24, vf24, vf29
  vu.vf24.mul_xyzw(vu.vf24, vu.vf29.w());
  // iand vi13, vi13, vi05      |  nop
  vu.vi13 = vu.vi13 & vu.vi05;
  // lq.xyzw vf20, 0(vi10)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10);
  // lq.xyzw vf25, 0(vi13)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi13);
  // lq.xyzw vf23, 1(vi10)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi10 + 1);
  // lq.xyzw vf26, 1(vi13)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi13 + 1);
  // lq.xyzw vf20, 2(vi10)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10 + 2);
  // lq.xyzw vf27, 2(vi13)      |  maddy.xyzw vf25, vf25, vf24
  vu.acc.madd_xyzw(vu.vf25, vu.vf25, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi13 + 2);
  // lq.xyzw vf23, 3(vi10)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi10 + 3);
  // lq.xyzw vf28, 3(vi13)      |  maddy.xyzw vf26, vf26, vf24
  vu.acc.madd_xyzw(vu.vf26, vu.vf26, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi13 + 3);
  // lq.xyzw vf20, 4(vi10)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10 + 4);
  // lq.xyz vf29, 4(vi13)       |  maddy.xyzw vf27, vf27, vf24
  vu.acc.madd_xyzw(vu.vf27, vu.vf27, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi13 + 4);
  // lq.xyzw vf23, 5(vi10)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi10 + 5);
  // lq.xyz vf30, 5(vi13)       |  maddy.xyzw vf28, vf28, vf24
  vu.acc.madd_xyzw(vu.vf28, vu.vf28, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi13 + 5);
  // lq.xyzw vf20, 6(vi10)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10 + 6);
  // lq.xyzw vf31, 6(vi13)      |  maddy.xyz vf29, vf29, vf24
  vu.acc.madd_xyz(vu.vf29, vu.vf29, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi13 + 6);
  // mtir vi10, vf11.x          |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   vu.vi10 = vu.vf11.x_as_u16();
  // mtir vi13, vf11.y          |  maddy.xyz vf30, vf30, vf24
  vu.acc.madd_xyz(vu.vf30, vu.vf30, vu.vf24.y());   vu.vi13 = vu.vf11.y_as_u16();
  // nop                        |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());
  // nop                        |  maddy.xyzw vf31, vf31, vf24
  vu.acc.madd_xyzw(vu.vf31, vu.vf31, vu.vf24.y());
  // nop                        |  mulaz.xyzw ACC, vf29, vf08
  vu.acc.mula_xyzw(vu.vf29, vu.vf08.z());
  // nop                        |  maddaz.xyzw ACC, vf30, vf11
  vu.acc.madda_xyzw(vu.vf30, vu.vf11.z());
  // nop                        |  maddz.xyz vf11, vf31, vf14
  vu.acc.madd_xyz(vu.vf11, vu.vf31, vu.vf14.z());
  // nop                        |  nop

  // nop                        |  nop

  // nop                        |  mulaw.xyzw ACC, vf25, vf08
  vu.acc.mula_xyzw(vu.vf25, vu.vf08.w());
  // iaddiu vi08, vi00, 0x243   |  nop
  vu.vi08 = 0x243; /* 579 */

  // erleng.xyz P, vf11         |  nop
  vu.P = erleng(vu.vf11); /* TODO erleng */
  // ior vi15, vi07, vi00       |  maddaw.xyzw ACC, vf26, vf11
  vu.acc.madda_xyzw(vu.vf26, vu.vf11.w());   vu.vi15 = vu.vi07;
  // mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
  vu.acc.madd_xyzw(vu.vf08, vu.vf27, vu.vf14.w());   vu.vf14.z() = 1.f;
  // lqi.xyzw vf09, vi01        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf09, vu.vi01++);
  // ilwr.y vi03, vi12          |  nop
  ilw_buffer(Mask::y, vu.vi03, vu.vi12);
  // ilw.z vi07, 1(vi12)        |  nop
  ilw_buffer(Mask::z, vu.vi07, vu.vi12 + 1);
  // lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  vu.vf08.add_xyzw(vu.vf08, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi01++);
  // lqi.xyzw vf15, vi01        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi01++);
  // mtir vi11, vf09.x          |  nop
  vu.vi11 = vu.vf09.x_as_u16();
// BRANCH!
  // ibeq vi00, vi15, L26       |  nop
  bc = (vu.vi15 == 0);
  // mtir vi14, vf09.y          |  nop
  vu.vi14 = vu.vf09.y_as_u16();
  if (bc) { goto L26; }


  // iaddiu vi08, vi00, 0x539   |  miniw.w vf08, vf08, vf01
  vu.vf08.mini(Mask::w, vu.vf08, vu.vf01.w());   vu.vi08 = 0x539; /* 1337 */

  L26:
  // div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  vu.vf09.add(Mask::zw, vu.vf09, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf08.w();

  // iadd vi03, vi03, vi12      |  add.xyzw vf12, vf12, vf18
  vu.vf12.add_xyzw(vu.vf12, vu.vf18);   vu.vi03 = vu.vi03 + vu.vi12;
  // iand vi11, vi11, vi05      |  add.xyzw vf15, vf15, vf19
  vu.vf15.add_xyzw(vu.vf15, vu.vf19);   vu.vi11 = vu.vi11 & vu.vi05;
  // iadd vi06, vi06, vi03      |  nop
  vu.vi06 = vu.vi06 + vu.vi03;
  // iadd vi07, vi07, vi06      |  nop
  vu.vi07 = vu.vi07 + vu.vi06;
  // iand vi14, vi14, vi05      |  nop
  vu.vi14 = vu.vi14 & vu.vi05;
// BRANCH!
  // ibne vi05, vi11, L27       |  nop
  bc = (vu.vi05 != vu.vi11);
  // nop                        |  mul.xyz vf08, vf08, Q
  vu.vf08.mul(Mask::xyz, vu.vf08, vu.Q); /* TODO mulq */
  if (bc) { goto L27; }


  // mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  vu.vf14.mul_xyzw(vu.vf14, vu.Q); /* TODO mulq */   vu.vi11 = vu.vf12.x_as_u16();
  // mtir vi14, vf12.y          |  nop
  vu.vi14 = vu.vf12.y_as_u16();
// BRANCH!
  // b L28                      |  nop
  bc = true;
  // lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
  vu.vf08.add_xyzw(vu.vf08, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L28; }


  L27:
  // lq.xyzw vf20, 0(vi11)      |  mul.xyzw vf14, vf14, Q
  vu.vf14.mul_xyzw(vu.vf14, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11);
  // lq.xyzw vf25, 0(vi14)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi14);
  // lq.xyzw vf23, 1(vi11)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi11 + 1);
  // lq.xyzw vf26, 1(vi14)      |  add.xyzw vf08, vf08, vf22
  vu.vf08.add_xyzw(vu.vf08, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi14 + 1);
  // lq.xyzw vf20, 2(vi11)      |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11 + 2);
  // lq.xyzw vf27, 2(vi14)      |  maddw.xyzw vf25, vf25, vf24
  vu.acc.madd_xyzw(vu.vf25, vu.vf25, vu.vf24.w());   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi14 + 2);
  // lq.xyzw vf23, 3(vi11)      |  mulaz.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi11 + 3);
  // lq.xyzw vf28, 3(vi14)      |  maddw.xyzw vf26, vf26, vf24
  vu.acc.madd_xyzw(vu.vf26, vu.vf26, vu.vf24.w());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi14 + 3);
  // lq.xyzw vf20, 4(vi11)      |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11 + 4);
  // lq.xyz vf29, 4(vi14)       |  maddw.xyzw vf27, vf27, vf24
  vu.acc.madd_xyzw(vu.vf27, vu.vf27, vu.vf24.w());   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi14 + 4);
  // lq.xyzw vf23, 5(vi11)      |  mulaz.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi11 + 5);
  // lq.xyz vf30, 5(vi14)       |  maddw.xyzw vf28, vf28, vf24
  vu.acc.madd_xyzw(vu.vf28, vu.vf28, vu.vf24.w());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi14 + 5);
  // lq.xyzw vf20, 6(vi11)      |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11 + 6);
  // lq.xyzw vf31, 6(vi14)      |  maddw.xyz vf29, vf29, vf24
  vu.acc.madd_xyz(vu.vf29, vu.vf29, vu.vf24.w());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi14 + 6);
  // lqi.xyzw vf23, vi02        |  mulaz.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi02++);
  // mtir vi11, vf12.x          |  maddw.xyz vf30, vf30, vf24
  vu.acc.madd_xyz(vu.vf30, vu.vf30, vu.vf24.w());   vu.vi11 = vu.vf12.x_as_u16();
  // mtir vi14, vf12.y          |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   vu.vi14 = vu.vf12.y_as_u16();
  // iaddiu vi08, vi00, 0x1a1   |  maddw.xyzw vf31, vf31, vf24
  vu.acc.madd_xyzw(vu.vf31, vu.vf31, vu.vf24.w());   vu.vi08 = 0x1a1; /* 417 */

// BRANCH!
  // ibeq vi00, vi15, L28       |  nop
  bc = (vu.vi15 == 0);
  // lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
  vu.vf24.itof0(Mask::xyzw, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L28; }


  // iaddiu vi08, vi00, 0x48e   |  nop
  vu.vi08 = 0x48e; /* 1166 */

  L28:
  // nop                        |  mulaz.xyzw ACC, vf29, vf09
  vu.acc.mula_xyzw(vu.vf29, vu.vf09.z());
  // nop                        |  maddaz.xyzw ACC, vf30, vf12
  vu.acc.madda_xyzw(vu.vf30, vu.vf12.z());
  // mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  vu.acc.madd_xyz(vu.vf12, vu.vf31, vu.vf15.z());   vu.vf20.mfp(Mask::w, vu.P);
  // nop                        |  nop

  // 1024.0                     |  miniw.w vf08, vf08, vf03 :i
  vu.vf08.mini(Mask::w, vu.vf08, vu.vf03.w());   vu.I = 1024.0;
// BRANCH!
  // ibne vi00, vi15, L93       |  mulaw.xyzw ACC, vf25, vf09
  vu.acc.mula_xyzw(vu.vf25, vu.vf09.w());   bc = (vu.vi15 != 0);
  // ilw.y vi09, -6(vi01)       |  mulw.xyzw vf11, vf11, vf20
  vu.vf11.mul_xyzw(vu.vf11, vu.vf20.w());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  if (bc) { goto L93; }


  // erleng.xyz P, vf12         |  nop
  vu.P = erleng(vu.vf12); /* TODO erleng */
// BRANCH!
  // ibeq vi06, vi03, L65       |  maddaw.xyzw ACC, vf26, vf12
  vu.acc.madda_xyzw(vu.vf26, vu.vf12.w());   bc = (vu.vi06 == vu.vi03);
  // mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  vu.acc.madd_xyzw(vu.vf09, vu.vf27, vu.vf15.w());   vu.vf15.z() = 1.f;
  if (bc) { goto L65; }


  // lqi.xyzw vf10, vi01        |  mulax.xyzw ACC, vf01, vf11
  vu.acc.mula_xyzw(vu.vf01, vu.vf11.x());   lq_buffer_xyzw<DEBUG>(vu.vf10, vu.vi01++);
  // jr vi08                    |  madday.xyzw ACC, vf02, vf11
  vu.acc.madda_xyzw(vu.vf02, vu.vf11.y());
  // nop                        |  maddz.xyzw vf11, vf03, vf11
  vu.acc.madd_xyzw(vu.vf11, vu.vf03, vu.vf11.z());
  switch (vu.vi08) {
  case 0x1a1:
  goto JUMP_1A1;
  case 0x48e:
  goto JUMP_48E;
case 0x243:
goto JUMP_243;
default:
ASSERT(false);
}
  L29:
  // lqi.xyzw vf10, vi01        |  mulax.xyzw ACC, vf01, vf11
  vu.acc.mula_xyzw(vu.vf01, vu.vf11.x());   lq_buffer_xyzw<DEBUG>(vu.vf10, vu.vi01++);
  // sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  vu.acc.madda_xyzw(vu.vf02, vu.vf11.y());   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi12 + 1);
  // sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
  vu.acc.madd_xyzw(vu.vf11, vu.vf03, vu.vf11.z());   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi15 + 1);
  JUMP_1A1:
  // lqi.xyzw vf13, vi01        |  add.xyzw vf09, vf09, vf28
  vu.vf09.add_xyzw(vu.vf09, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi01++);
  // lqi.xyzw vf16, vi01        |  maxw.w vf08, vf08, vf02
  vu.vf08.max(Mask::w, vu.vf08, vu.vf02.w());   lq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi01++);
  // mtir vi12, vf10.x          |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   vu.vi12 = vu.vf10.x_as_u16();
  // mtir vi15, vf10.y          |  maxx.xyzw vf11, vf11, vf00
  vu.vf11.max_xyzw(vu.vf11, vu.vf00.x());   vu.vi15 = vu.vf10.y_as_u16();
  // div Q, vf01.w, vf09.w      |  add.zw vf10, vf10, vf17
  vu.vf10.add(Mask::zw, vu.vf10, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf09.w();

  // move.xyzw vf21, vf08       |  add.xyzw vf13, vf13, vf18
  vu.vf13.add_xyzw(vu.vf13, vu.vf18);   vu.vf21.move_xyzw(vu.vf08);
  // iand vi12, vi12, vi05      |  add.xyzw vf16, vf16, vf19
  vu.vf16.add_xyzw(vu.vf16, vu.vf19);   vu.vi12 = vu.vi12 & vu.vi05;
  // nop                        |  mulax.xyzw ACC, vf04, vf11
  vu.acc.mula_xyzw(vu.vf04, vu.vf11.x());
// BRANCH!
  // ibgtz vi09, L31            |  madday.xyzw ACC, vf05, vf11
  vu.acc.madda_xyzw(vu.vf05, vu.vf11.y());   bc = ((s16)vu.vi09) > 0;
  // iand vi15, vi15, vi05      |  maddaz.xyzw ACC, vf06, vf11
  vu.acc.madda_xyzw(vu.vf06, vu.vf11.z());   vu.vi15 = vu.vi15 & vu.vi05;
  if (bc) { goto L31; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L31:
// BRANCH!
  // ibne vi05, vi12, L32       |  maddw.xyzw vf11, vf07, vf00
  vu.acc.madd_xyzw(vu.vf11, vu.vf07, vu.vf00.w());   bc = (vu.vi05 != vu.vi12);
  // ilw.x vi09, -9(vi01)       |  mul.xyz vf09, vf09, Q
  vu.vf09.mul(Mask::xyz, vu.vf09, vu.Q); /* TODO mulq */   ilw_buffer(Mask::x, vu.vi09, vu.vi01 + -9);
  if (bc) { goto L32; }


  // mtir vi12, vf13.x          |  mul.xyzw vf15, vf15, Q
  vu.vf15.mul_xyzw(vu.vf15, vu.Q); /* TODO mulq */   vu.vi12 = vu.vf13.x_as_u16();
  // mtir vi15, vf13.y          |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi15 = vu.vf13.y_as_u16();
// BRANCH!
  // b L33                      |  mul.xyzw vf11, vf11, vf23
  vu.vf11.mul_xyzw(vu.vf11, vu.vf23);   bc = true;
  // lqi.xyzw vf23, vi03        |  add.xyzw vf09, vf09, vf22
  vu.vf09.add_xyzw(vu.vf09, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L33; }


  L32:
  // lq.xyzw vf20, 0(vi12)      |  mul.xyzw vf15, vf15, Q
  vu.vf15.mul_xyzw(vu.vf15, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi12);
  // nop                        |  mulw.xyzw vf24, vf24, vf29
  vu.vf24.mul_xyzw(vu.vf24, vu.vf29.w());
  // lq.xyzw vf25, 0(vi15)      |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi15);
  // lq.xyzw vf23, 1(vi12)      |  mul.xyzw vf11, vf11, vf23
  vu.vf11.mul_xyzw(vu.vf11, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi12 + 1);
  // lq.xyzw vf26, 1(vi15)      |  add.xyzw vf09, vf09, vf22
  vu.vf09.add_xyzw(vu.vf09, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi15 + 1);
  // lq.xyzw vf20, 2(vi12)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi12 + 2);
  // lq.xyzw vf27, 2(vi15)      |  maddy.xyzw vf25, vf25, vf24
  vu.acc.madd_xyzw(vu.vf25, vu.vf25, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi15 + 2);
  // lq.xyzw vf23, 3(vi12)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi12 + 3);
  // lq.xyzw vf28, 3(vi15)      |  maddy.xyzw vf26, vf26, vf24
  vu.acc.madd_xyzw(vu.vf26, vu.vf26, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi15 + 3);
  // lq.xyzw vf20, 4(vi12)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi12 + 4);
  // lq.xyz vf29, 4(vi15)       |  maddy.xyzw vf27, vf27, vf24
  vu.acc.madd_xyzw(vu.vf27, vu.vf27, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi15 + 4);
  // lq.xyzw vf23, 5(vi12)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi12 + 5);
  // lq.xyz vf30, 5(vi15)       |  maddy.xyzw vf28, vf28, vf24
  vu.acc.madd_xyzw(vu.vf28, vu.vf28, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi15 + 5);
  // lq.xyzw vf20, 6(vi12)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi12 + 6);
  // lq.xyzw vf31, 6(vi15)      |  maddy.xyz vf29, vf29, vf24
  vu.acc.madd_xyz(vu.vf29, vu.vf29, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi15 + 6);
  // mtir vi12, vf13.x          |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   vu.vi12 = vu.vf13.x_as_u16();
  // mtir vi15, vf13.y          |  maddy.xyz vf30, vf30, vf24
  vu.acc.madd_xyz(vu.vf30, vu.vf30, vu.vf24.y());   vu.vi15 = vu.vf13.y_as_u16();
// BRANCH!
  // b L49                      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   bc = true;
  // lqi.xyzw vf23, vi03        |  maddy.xyzw vf31, vf31, vf24
  vu.acc.madd_xyzw(vu.vf31, vu.vf31, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L49; }


  L33:
// BRANCH!
  // ibgez vi09, L34            |  mulaz.xyzw ACC, vf29, vf10
  vu.acc.mula_xyzw(vu.vf29, vu.vf10.z());   bc = ((s16)vu.vi09) >= 0;
  // sq.xyzw vf21, 2(vi10)      |  maddaz.xyzw ACC, vf30, vf13
  vu.acc.madda_xyzw(vu.vf30, vu.vf13.z());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi10 + 2);
  if (bc) { goto L34; }


  // nop                        |  ftoi4.xyzw vf21, vf08
  vu.vf21.ftoi4(Mask::xyzw, vu.vf08);
  L34:
  // mfp.w vf20, P              |  maddz.xyz vf13, vf31, vf16
  vu.acc.madd_xyz(vu.vf13, vu.vf31, vu.vf16.z());   vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  vu.vf11.mini_xyzw(vu.vf11, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi10);
  // sq.xyzw vf14, 0(vi13)      |  miniw.w vf09, vf09, vf03
  vu.vf09.mini(Mask::w, vu.vf09, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi13);
  // sq.xyzw vf21, 2(vi13)      |  mulaw.xyzw ACC, vf25, vf10
  vu.acc.mula_xyzw(vu.vf25, vu.vf10.w());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi13 + 2);
  // ilw.y vi09, -6(vi01)       |  mulw.xyzw vf12, vf12, vf20
  vu.vf12.mul_xyzw(vu.vf12, vu.vf20.w());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  // erleng.xyz P, vf13         |  ftoi0.xyzw vf11, vf11
  vu.vf11.ftoi0(Mask::xyzw, vu.vf11);   vu.P = erleng(vu.vf13); /* TODO erleng */
// BRANCH!
  // ibne vi06, vi03, L35       |  maddaw.xyzw ACC, vf26, vf13
  vu.acc.madda_xyzw(vu.vf26, vu.vf13.w());   bc = (vu.vi06 != vu.vi03);
  // mr32.z vf16, vf00          |  maddw.xyzw vf10, vf27, vf16
  vu.acc.madd_xyzw(vu.vf10, vu.vf27, vu.vf16.w());   vu.vf16.z() = 1.f;
  if (bc) { goto L35; }


// BRANCH!
  // ibne vi07, vi03, L72       |  nop
  bc = (vu.vi07 != vu.vi03);
  // nop                        |  nop

  if (bc) { goto L72; }


// BRANCH!
  // b L143                     |  nop
  bc = true;
  // nop                        |  nop

  if (bc) { goto L143; }


  L35:
  // lqi.xyzw vf08, vi01        |  mulax.xyzw ACC, vf01, vf12
  vu.acc.mula_xyzw(vu.vf01, vu.vf12.x());   lq_buffer_xyzw<DEBUG>(vu.vf08, vu.vi01++);
  // sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12
  vu.acc.madda_xyzw(vu.vf02, vu.vf12.y());   sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi10 + 1);
  // sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12
  vu.acc.madd_xyzw(vu.vf12, vu.vf03, vu.vf12.z());   sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi13 + 1);
  // lqi.xyzw vf11, vi01        |  add.xyzw vf10, vf10, vf28
  vu.vf10.add_xyzw(vu.vf10, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi01++);
  // lqi.xyzw vf14, vi01        |  maxw.w vf09, vf09, vf02
  vu.vf09.max(Mask::w, vu.vf09, vu.vf02.w());   lq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi01++);
  // mtir vi10, vf08.x          |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   vu.vi10 = vu.vf08.x_as_u16();
  // mtir vi13, vf08.y          |  maxx.xyzw vf12, vf12, vf00
  vu.vf12.max_xyzw(vu.vf12, vu.vf00.x());   vu.vi13 = vu.vf08.y_as_u16();
  // div Q, vf01.w, vf10.w      |  add.zw vf08, vf08, vf17
  vu.vf08.add(Mask::zw, vu.vf08, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf10.w();

  // move.xyzw vf21, vf09       |  add.xyzw vf11, vf11, vf18
  vu.vf11.add_xyzw(vu.vf11, vu.vf18);   vu.vf21.move_xyzw(vu.vf09);
  // iand vi10, vi10, vi05      |  add.xyzw vf14, vf14, vf19
  vu.vf14.add_xyzw(vu.vf14, vu.vf19);   vu.vi10 = vu.vi10 & vu.vi05;
  // nop                        |  mulax.xyzw ACC, vf04, vf12
  vu.acc.mula_xyzw(vu.vf04, vu.vf12.x());
// BRANCH!
  // ibgtz vi09, L36            |  madday.xyzw ACC, vf05, vf12
  vu.acc.madda_xyzw(vu.vf05, vu.vf12.y());   bc = ((s16)vu.vi09) > 0;
  // iand vi13, vi13, vi05      |  maddaz.xyzw ACC, vf06, vf12
  vu.acc.madda_xyzw(vu.vf06, vu.vf12.z());   vu.vi13 = vu.vi13 & vu.vi05;
  if (bc) { goto L36; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L36:
// BRANCH!
  // ibne vi05, vi10, L37       |  maddw.xyzw vf12, vf07, vf00
  vu.acc.madd_xyzw(vu.vf12, vu.vf07, vu.vf00.w());   bc = (vu.vi05 != vu.vi10);
  // ilw.x vi09, -9(vi01)       |  mul.xyz vf10, vf10, Q
  vu.vf10.mul(Mask::xyz, vu.vf10, vu.Q); /* TODO mulq */   ilw_buffer(Mask::x, vu.vi09, vu.vi01 + -9);
  if (bc) { goto L37; }


  // mtir vi10, vf11.x          |  mul.xyzw vf16, vf16, Q
  vu.vf16.mul_xyzw(vu.vf16, vu.Q); /* TODO mulq */   vu.vi10 = vu.vf11.x_as_u16();
  // mtir vi13, vf11.y          |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi13 = vu.vf11.y_as_u16();
// BRANCH!
  // b L38                      |  mul.xyzw vf12, vf12, vf23
  vu.vf12.mul_xyzw(vu.vf12, vu.vf23);   bc = true;
  // lqi.xyzw vf23, vi03        |  add.xyzw vf10, vf10, vf22
  vu.vf10.add_xyzw(vu.vf10, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L38; }


  L37:
  // lq.xyzw vf20, 0(vi10)      |  mul.xyzw vf16, vf16, Q
  vu.vf16.mul_xyzw(vu.vf16, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10);
  // nop                        |  mulw.xyzw vf24, vf24, vf29
  vu.vf24.mul_xyzw(vu.vf24, vu.vf29.w());
  // lq.xyzw vf25, 0(vi13)      |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi13);
  // lq.xyzw vf23, 1(vi10)      |  mul.xyzw vf12, vf12, vf23
  vu.vf12.mul_xyzw(vu.vf12, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi10 + 1);
  // lq.xyzw vf26, 1(vi13)      |  add.xyzw vf10, vf10, vf22
  vu.vf10.add_xyzw(vu.vf10, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi13 + 1);
  // lq.xyzw vf20, 2(vi10)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10 + 2);
  // lq.xyzw vf27, 2(vi13)      |  maddy.xyzw vf25, vf25, vf24
  vu.acc.madd_xyzw(vu.vf25, vu.vf25, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi13 + 2);
  // lq.xyzw vf23, 3(vi10)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi10 + 3);
  // lq.xyzw vf28, 3(vi13)      |  maddy.xyzw vf26, vf26, vf24
  vu.acc.madd_xyzw(vu.vf26, vu.vf26, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi13 + 3);
  // lq.xyzw vf20, 4(vi10)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10 + 4);
  // lq.xyz vf29, 4(vi13)       |  maddy.xyzw vf27, vf27, vf24
  vu.acc.madd_xyzw(vu.vf27, vu.vf27, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi13 + 4);
  // lq.xyzw vf23, 5(vi10)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi10 + 5);
  // lq.xyz vf30, 5(vi13)       |  maddy.xyzw vf28, vf28, vf24
  vu.acc.madd_xyzw(vu.vf28, vu.vf28, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi13 + 5);
  // lq.xyzw vf20, 6(vi10)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10 + 6);
  // lq.xyzw vf31, 6(vi13)      |  maddy.xyz vf29, vf29, vf24
  vu.acc.madd_xyz(vu.vf29, vu.vf29, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi13 + 6);
  // mtir vi10, vf11.x          |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   vu.vi10 = vu.vf11.x_as_u16();
  // mtir vi13, vf11.y          |  maddy.xyz vf30, vf30, vf24
  vu.acc.madd_xyz(vu.vf30, vu.vf30, vu.vf24.y());   vu.vi13 = vu.vf11.y_as_u16();
// BRANCH!
  // b L54                      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   bc = true;
  // lqi.xyzw vf23, vi03        |  maddy.xyzw vf31, vf31, vf24
  vu.acc.madd_xyzw(vu.vf31, vu.vf31, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L54; }


  L38:
// BRANCH!
  // ibgez vi09, L39            |  mulaz.xyzw ACC, vf29, vf08
  vu.acc.mula_xyzw(vu.vf29, vu.vf08.z());   bc = ((s16)vu.vi09) >= 0;
  // sq.xyzw vf21, 2(vi11)      |  maddaz.xyzw ACC, vf30, vf11
  vu.acc.madda_xyzw(vu.vf30, vu.vf11.z());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi11 + 2);
  if (bc) { goto L39; }


  // nop                        |  ftoi4.xyzw vf21, vf09
  vu.vf21.ftoi4(Mask::xyzw, vu.vf09);
  L39:
  // mfp.w vf20, P              |  maddz.xyz vf11, vf31, vf14
  vu.acc.madd_xyz(vu.vf11, vu.vf31, vu.vf14.z());   vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17
  vu.vf12.mini_xyzw(vu.vf12, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi11);
  // sq.xyzw vf15, 0(vi14)      |  miniw.w vf10, vf10, vf03
  vu.vf10.mini(Mask::w, vu.vf10, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi14);
  // sq.xyzw vf21, 2(vi14)      |  mulaw.xyzw ACC, vf25, vf08
  vu.acc.mula_xyzw(vu.vf25, vu.vf08.w());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi14 + 2);
  // ilw.y vi09, -6(vi01)       |  mulw.xyzw vf13, vf13, vf20
  vu.vf13.mul_xyzw(vu.vf13, vu.vf20.w());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  // erleng.xyz P, vf11         |  ftoi0.xyzw vf12, vf12
  vu.vf12.ftoi0(Mask::xyzw, vu.vf12);   vu.P = erleng(vu.vf11); /* TODO erleng */
// BRANCH!
  // ibne vi06, vi03, L40       |  maddaw.xyzw ACC, vf26, vf11
  vu.acc.madda_xyzw(vu.vf26, vu.vf11.w());   bc = (vu.vi06 != vu.vi03);
  // mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
  vu.acc.madd_xyzw(vu.vf08, vu.vf27, vu.vf14.w());   vu.vf14.z() = 1.f;
  if (bc) { goto L40; }


// BRANCH!
  // ibne vi07, vi03, L77       |  nop
  bc = (vu.vi07 != vu.vi03);
  // nop                        |  nop

  if (bc) { goto L77; }


// BRANCH!
  // b L153                     |  nop
  bc = true;
  // nop                        |  nop

  if (bc) { goto L153; }


  L40:
  // lqi.xyzw vf09, vi01        |  mulax.xyzw ACC, vf01, vf13
  vu.acc.mula_xyzw(vu.vf01, vu.vf13.x());   lq_buffer_xyzw<DEBUG>(vu.vf09, vu.vi01++);
  // sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13
  vu.acc.madda_xyzw(vu.vf02, vu.vf13.y());   sq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi11 + 1);
  // sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13
  vu.acc.madd_xyzw(vu.vf13, vu.vf03, vu.vf13.z());   sq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi14 + 1);
  // lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  vu.vf08.add_xyzw(vu.vf08, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi01++);
  // lqi.xyzw vf15, vi01        |  maxw.w vf10, vf10, vf02
  vu.vf10.max(Mask::w, vu.vf10, vu.vf02.w());   lq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi01++);
  // mtir vi11, vf09.x          |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   vu.vi11 = vu.vf09.x_as_u16();
  // mtir vi14, vf09.y          |  maxx.xyzw vf13, vf13, vf00
  vu.vf13.max_xyzw(vu.vf13, vu.vf00.x());   vu.vi14 = vu.vf09.y_as_u16();
  // div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  vu.vf09.add(Mask::zw, vu.vf09, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf08.w();

  // move.xyzw vf21, vf10       |  add.xyzw vf12, vf12, vf18
  vu.vf12.add_xyzw(vu.vf12, vu.vf18);   vu.vf21.move_xyzw(vu.vf10);
  // iand vi11, vi11, vi05      |  add.xyzw vf15, vf15, vf19
  vu.vf15.add_xyzw(vu.vf15, vu.vf19);   vu.vi11 = vu.vi11 & vu.vi05;
  // nop                        |  mulax.xyzw ACC, vf04, vf13
  vu.acc.mula_xyzw(vu.vf04, vu.vf13.x());
// BRANCH!
  // ibgtz vi09, L41            |  madday.xyzw ACC, vf05, vf13
  vu.acc.madda_xyzw(vu.vf05, vu.vf13.y());   bc = ((s16)vu.vi09) > 0;
  // iand vi14, vi14, vi05      |  maddaz.xyzw ACC, vf06, vf13
  vu.acc.madda_xyzw(vu.vf06, vu.vf13.z());   vu.vi14 = vu.vi14 & vu.vi05;
  if (bc) { goto L41; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L41:
// BRANCH!
  // ibne vi05, vi11, L42       |  maddw.xyzw vf13, vf07, vf00
  vu.acc.madd_xyzw(vu.vf13, vu.vf07, vu.vf00.w());   bc = (vu.vi05 != vu.vi11);
  // ilw.x vi09, -9(vi01)       |  mul.xyz vf08, vf08, Q
  vu.vf08.mul(Mask::xyz, vu.vf08, vu.Q); /* TODO mulq */   ilw_buffer(Mask::x, vu.vi09, vu.vi01 + -9);
  if (bc) { goto L42; }


  // mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  vu.vf14.mul_xyzw(vu.vf14, vu.Q); /* TODO mulq */   vu.vi11 = vu.vf12.x_as_u16();
  // mtir vi14, vf12.y          |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi14 = vu.vf12.y_as_u16();
// BRANCH!
  // b L43                      |  mul.xyzw vf13, vf13, vf23
  vu.vf13.mul_xyzw(vu.vf13, vu.vf23);   bc = true;
  // lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
  vu.vf08.add_xyzw(vu.vf08, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L43; }


  L42:
  // lq.xyzw vf20, 0(vi11)      |  mul.xyzw vf14, vf14, Q
  vu.vf14.mul_xyzw(vu.vf14, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11);
  // nop                        |  mulw.xyzw vf24, vf24, vf29
  vu.vf24.mul_xyzw(vu.vf24, vu.vf29.w());
  // lq.xyzw vf25, 0(vi14)      |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi14);
  // lq.xyzw vf23, 1(vi11)      |  mul.xyzw vf13, vf13, vf23
  vu.vf13.mul_xyzw(vu.vf13, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi11 + 1);
  // lq.xyzw vf26, 1(vi14)      |  add.xyzw vf08, vf08, vf22
  vu.vf08.add_xyzw(vu.vf08, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi14 + 1);
  // lq.xyzw vf20, 2(vi11)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11 + 2);
  // lq.xyzw vf27, 2(vi14)      |  maddy.xyzw vf25, vf25, vf24
  vu.acc.madd_xyzw(vu.vf25, vu.vf25, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi14 + 2);
  // lq.xyzw vf23, 3(vi11)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi11 + 3);
  // lq.xyzw vf28, 3(vi14)      |  maddy.xyzw vf26, vf26, vf24
  vu.acc.madd_xyzw(vu.vf26, vu.vf26, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi14 + 3);
  // lq.xyzw vf20, 4(vi11)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11 + 4);
  // lq.xyz vf29, 4(vi14)       |  maddy.xyzw vf27, vf27, vf24
  vu.acc.madd_xyzw(vu.vf27, vu.vf27, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi14 + 4);
  // lq.xyzw vf23, 5(vi11)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi11 + 5);
  // lq.xyz vf30, 5(vi14)       |  maddy.xyzw vf28, vf28, vf24
  vu.acc.madd_xyzw(vu.vf28, vu.vf28, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi14 + 5);
  // lq.xyzw vf20, 6(vi11)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11 + 6);
  // lq.xyzw vf31, 6(vi14)      |  maddy.xyz vf29, vf29, vf24
  vu.acc.madd_xyz(vu.vf29, vu.vf29, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi14 + 6);
  // mtir vi11, vf12.x          |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   vu.vi11 = vu.vf12.x_as_u16();
  // mtir vi14, vf12.y          |  maddy.xyz vf30, vf30, vf24
  vu.acc.madd_xyz(vu.vf30, vu.vf30, vu.vf24.y());   vu.vi14 = vu.vf12.y_as_u16();
// BRANCH!
  // b L59                      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   bc = true;
  // lqi.xyzw vf23, vi03        |  maddy.xyzw vf31, vf31, vf24
  vu.acc.madd_xyzw(vu.vf31, vu.vf31, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L59; }


  L43:
// BRANCH!
  // ibgez vi09, L44            |  mulaz.xyzw ACC, vf29, vf09
  vu.acc.mula_xyzw(vu.vf29, vu.vf09.z());   bc = ((s16)vu.vi09) >= 0;
  // sq.xyzw vf21, 2(vi12)      |  maddaz.xyzw ACC, vf30, vf12
  vu.acc.madda_xyzw(vu.vf30, vu.vf12.z());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi12 + 2);
  if (bc) { goto L44; }


  // nop                        |  ftoi4.xyzw vf21, vf10
  vu.vf21.ftoi4(Mask::xyzw, vu.vf10);
  L44:
  // mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  vu.acc.madd_xyz(vu.vf12, vu.vf31, vu.vf15.z());   vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  vu.vf13.mini_xyzw(vu.vf13, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi12);
  // sq.xyzw vf16, 0(vi15)      |  miniw.w vf08, vf08, vf03
  vu.vf08.mini(Mask::w, vu.vf08, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi15);
  // sq.xyzw vf21, 2(vi15)      |  mulaw.xyzw ACC, vf25, vf09
  vu.acc.mula_xyzw(vu.vf25, vu.vf09.w());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi15 + 2);
  // ilw.y vi09, -6(vi01)       |  mulw.xyzw vf11, vf11, vf20
  vu.vf11.mul_xyzw(vu.vf11, vu.vf20.w());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  // erleng.xyz P, vf12         |  ftoi0.xyzw vf13, vf13
  vu.vf13.ftoi0(Mask::xyzw, vu.vf13);   vu.P = erleng(vu.vf12); /* TODO erleng */
// BRANCH!
  // ibne vi06, vi03, L29       |  maddaw.xyzw ACC, vf26, vf12
  vu.acc.madda_xyzw(vu.vf26, vu.vf12.w());   bc = (vu.vi06 != vu.vi03);
  // mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  vu.acc.madd_xyzw(vu.vf09, vu.vf27, vu.vf15.w());   vu.vf15.z() = 1.f;
  if (bc) { goto L29; }


// BRANCH!
  // ibne vi07, vi03, L66       |  nop
  bc = (vu.vi07 != vu.vi03);
  // nop                        |  nop

  if (bc) { goto L66; }


// BRANCH!
  // b L163                     |  nop
  bc = true;
  // nop                        |  nop

  if (bc) { goto L163; }


  L45:
  // lqi.xyzw vf10, vi01        |  mulax.xyzw ACC, vf01, vf11
  vu.acc.mula_xyzw(vu.vf01, vu.vf11.x());   lq_buffer_xyzw<DEBUG>(vu.vf10, vu.vi01++);
  // sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  vu.acc.madda_xyzw(vu.vf02, vu.vf11.y());   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi12 + 1);
  // sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
  vu.acc.madd_xyzw(vu.vf11, vu.vf03, vu.vf11.z());   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi15 + 1);
  JUMP_243:
  // lqi.xyzw vf13, vi01        |  add.xyzw vf09, vf09, vf28
  vu.vf09.add_xyzw(vu.vf09, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi01++);
  // lqi.xyzw vf16, vi01        |  maxw.w vf08, vf08, vf02
  vu.vf08.max(Mask::w, vu.vf08, vu.vf02.w());   lq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi01++);
  // mtir vi12, vf10.x          |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   vu.vi12 = vu.vf10.x_as_u16();
  // mtir vi15, vf10.y          |  maxx.xyzw vf11, vf11, vf00
  vu.vf11.max_xyzw(vu.vf11, vu.vf00.x());   vu.vi15 = vu.vf10.y_as_u16();
  // div Q, vf01.w, vf09.w      |  add.zw vf10, vf10, vf17
  vu.vf10.add(Mask::zw, vu.vf10, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf09.w();

  // move.xyzw vf21, vf08       |  add.xyzw vf13, vf13, vf18
  vu.vf13.add_xyzw(vu.vf13, vu.vf18);   vu.vf21.move_xyzw(vu.vf08);
  // iand vi12, vi12, vi05      |  add.xyzw vf16, vf16, vf19
  vu.vf16.add_xyzw(vu.vf16, vu.vf19);   vu.vi12 = vu.vi12 & vu.vi05;
  // nop                        |  mulax.xyzw ACC, vf04, vf11
  vu.acc.mula_xyzw(vu.vf04, vu.vf11.x());
// BRANCH!
  // ibgtz vi09, L47            |  madday.xyzw ACC, vf05, vf11
  vu.acc.madda_xyzw(vu.vf05, vu.vf11.y());   bc = ((s16)vu.vi09) > 0;
  // iand vi15, vi15, vi05      |  maddaz.xyzw ACC, vf06, vf11
  vu.acc.madda_xyzw(vu.vf06, vu.vf11.z());   vu.vi15 = vu.vi15 & vu.vi05;
  if (bc) { goto L47; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L47:
// BRANCH!
  // ibne vi05, vi12, L48       |  maddw.xyzw vf11, vf07, vf00
  vu.acc.madd_xyzw(vu.vf11, vu.vf07, vu.vf00.w());   bc = (vu.vi05 != vu.vi12);
  // ilw.x vi09, -9(vi01)       |  mul.xyz vf09, vf09, Q
  vu.vf09.mul(Mask::xyz, vu.vf09, vu.Q); /* TODO mulq */   ilw_buffer(Mask::x, vu.vi09, vu.vi01 + -9);
  if (bc) { goto L48; }


  // mtir vi12, vf13.x          |  mul.xyzw vf15, vf15, Q
  vu.vf15.mul_xyzw(vu.vf15, vu.Q); /* TODO mulq */   vu.vi12 = vu.vf13.x_as_u16();
  // mtir vi15, vf13.y          |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi15 = vu.vf13.y_as_u16();
// BRANCH!
  // b L49                      |  mul.xyzw vf11, vf11, vf23
  vu.vf11.mul_xyzw(vu.vf11, vu.vf23);   bc = true;
  // lqi.xyzw vf23, vi03        |  add.xyzw vf09, vf09, vf22
  vu.vf09.add_xyzw(vu.vf09, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L49; }


  L48:
  // lq.xyzw vf20, 0(vi12)      |  mul.xyzw vf15, vf15, Q
  vu.vf15.mul_xyzw(vu.vf15, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi12);
  // lq.xyzw vf25, 0(vi15)      |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi15);
  // lq.xyzw vf23, 1(vi12)      |  mul.xyzw vf11, vf11, vf23
  vu.vf11.mul_xyzw(vu.vf11, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi12 + 1);
  // lq.xyzw vf26, 1(vi15)      |  add.xyzw vf09, vf09, vf22
  vu.vf09.add_xyzw(vu.vf09, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi15 + 1);
  // lq.xyzw vf20, 2(vi12)      |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi12 + 2);
  // lq.xyzw vf27, 2(vi15)      |  maddw.xyzw vf25, vf25, vf24
  vu.acc.madd_xyzw(vu.vf25, vu.vf25, vu.vf24.w());   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi15 + 2);
  // lq.xyzw vf23, 3(vi12)      |  mulaz.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi12 + 3);
  // lq.xyzw vf28, 3(vi15)      |  maddw.xyzw vf26, vf26, vf24
  vu.acc.madd_xyzw(vu.vf26, vu.vf26, vu.vf24.w());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi15 + 3);
  // lq.xyzw vf20, 4(vi12)      |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi12 + 4);
  // lq.xyz vf29, 4(vi15)       |  maddw.xyzw vf27, vf27, vf24
  vu.acc.madd_xyzw(vu.vf27, vu.vf27, vu.vf24.w());   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi15 + 4);
  // lq.xyzw vf23, 5(vi12)      |  mulaz.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi12 + 5);
  // lq.xyz vf30, 5(vi15)       |  maddw.xyzw vf28, vf28, vf24
  vu.acc.madd_xyzw(vu.vf28, vu.vf28, vu.vf24.w());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi15 + 5);
  // lq.xyzw vf20, 6(vi12)      |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi12 + 6);
  // lq.xyzw vf31, 6(vi15)      |  maddw.xyz vf29, vf29, vf24
  vu.acc.madd_xyz(vu.vf29, vu.vf29, vu.vf24.w());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi15 + 6);
  // lqi.xyzw vf23, vi02        |  mulaz.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi02++);
  // mtir vi12, vf13.x          |  maddw.xyz vf30, vf30, vf24
  vu.acc.madd_xyz(vu.vf30, vu.vf30, vu.vf24.w());   vu.vi12 = vu.vf13.x_as_u16();
  // mtir vi15, vf13.y          |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   vu.vi15 = vu.vf13.y_as_u16();
// BRANCH!
  // b L33                      |  maddw.xyzw vf31, vf31, vf24
  vu.acc.madd_xyzw(vu.vf31, vu.vf31, vu.vf24.w());   bc = true;
  // lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
  vu.vf24.itof0(Mask::xyzw, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L33; }


  L49:
// BRANCH!
  // ibgez vi09, L50            |  mulaz.xyzw ACC, vf29, vf10
  vu.acc.mula_xyzw(vu.vf29, vu.vf10.z());   bc = ((s16)vu.vi09) >= 0;
  // sq.xyzw vf21, 2(vi10)      |  maddaz.xyzw ACC, vf30, vf13
  vu.acc.madda_xyzw(vu.vf30, vu.vf13.z());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi10 + 2);
  if (bc) { goto L50; }


  // nop                        |  ftoi4.xyzw vf21, vf08
  vu.vf21.ftoi4(Mask::xyzw, vu.vf08);
  L50:
  // mfp.w vf20, P              |  maddz.xyz vf13, vf31, vf16
  vu.acc.madd_xyz(vu.vf13, vu.vf31, vu.vf16.z());   vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  vu.vf11.mini_xyzw(vu.vf11, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi10);
  // sq.xyzw vf14, 0(vi13)      |  miniw.w vf09, vf09, vf03
  vu.vf09.mini(Mask::w, vu.vf09, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi13);
  // sq.xyzw vf21, 2(vi13)      |  mulaw.xyzw ACC, vf25, vf10
  vu.acc.mula_xyzw(vu.vf25, vu.vf10.w());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi13 + 2);
  // ilw.y vi09, -6(vi01)       |  mulw.xyzw vf12, vf12, vf20
  vu.vf12.mul_xyzw(vu.vf12, vu.vf20.w());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  // erleng.xyz P, vf13         |  ftoi0.xyzw vf11, vf11
  vu.vf11.ftoi0(Mask::xyzw, vu.vf11);   vu.P = erleng(vu.vf13); /* TODO erleng */
// BRANCH!
  // ibne vi06, vi03, L51       |  maddaw.xyzw ACC, vf26, vf13
  vu.acc.madda_xyzw(vu.vf26, vu.vf13.w());   bc = (vu.vi06 != vu.vi03);
  // mr32.z vf16, vf00          |  maddw.xyzw vf10, vf27, vf16
  vu.acc.madd_xyzw(vu.vf10, vu.vf27, vu.vf16.w());   vu.vf16.z() = 1.f;
  if (bc) { goto L51; }


// BRANCH!
  // ibne vi07, vi03, L72       |  nop
  bc = (vu.vi07 != vu.vi03);
  // nop                        |  nop

  if (bc) { goto L72; }


// BRANCH!
  // b L143                     |  nop
  bc = true;
  // nop                        |  nop

  if (bc) { goto L143; }


  L51:
  // lqi.xyzw vf08, vi01        |  mulax.xyzw ACC, vf01, vf12
  vu.acc.mula_xyzw(vu.vf01, vu.vf12.x());   lq_buffer_xyzw<DEBUG>(vu.vf08, vu.vi01++);
  // sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12
  vu.acc.madda_xyzw(vu.vf02, vu.vf12.y());   sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi10 + 1);
  // sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12
  vu.acc.madd_xyzw(vu.vf12, vu.vf03, vu.vf12.z());   sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi13 + 1);
  // lqi.xyzw vf11, vi01        |  add.xyzw vf10, vf10, vf28
  vu.vf10.add_xyzw(vu.vf10, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi01++);
  // lqi.xyzw vf14, vi01        |  maxw.w vf09, vf09, vf02
  vu.vf09.max(Mask::w, vu.vf09, vu.vf02.w());   lq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi01++);
  // mtir vi10, vf08.x          |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   vu.vi10 = vu.vf08.x_as_u16();
  // mtir vi13, vf08.y          |  maxx.xyzw vf12, vf12, vf00
  vu.vf12.max_xyzw(vu.vf12, vu.vf00.x());   vu.vi13 = vu.vf08.y_as_u16();
  // div Q, vf01.w, vf10.w      |  add.zw vf08, vf08, vf17
  vu.vf08.add(Mask::zw, vu.vf08, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf10.w();

  // move.xyzw vf21, vf09       |  add.xyzw vf11, vf11, vf18
  vu.vf11.add_xyzw(vu.vf11, vu.vf18);   vu.vf21.move_xyzw(vu.vf09);
  // iand vi10, vi10, vi05      |  add.xyzw vf14, vf14, vf19
  vu.vf14.add_xyzw(vu.vf14, vu.vf19);   vu.vi10 = vu.vi10 & vu.vi05;
  // nop                        |  mulax.xyzw ACC, vf04, vf12
  vu.acc.mula_xyzw(vu.vf04, vu.vf12.x());
// BRANCH!
  // ibgtz vi09, L52            |  madday.xyzw ACC, vf05, vf12
  vu.acc.madda_xyzw(vu.vf05, vu.vf12.y());   bc = ((s16)vu.vi09) > 0;
  // iand vi13, vi13, vi05      |  maddaz.xyzw ACC, vf06, vf12
  vu.acc.madda_xyzw(vu.vf06, vu.vf12.z());   vu.vi13 = vu.vi13 & vu.vi05;
  if (bc) { goto L52; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L52:
// BRANCH!
  // ibne vi05, vi10, L53       |  maddw.xyzw vf12, vf07, vf00
  vu.acc.madd_xyzw(vu.vf12, vu.vf07, vu.vf00.w());   bc = (vu.vi05 != vu.vi10);
  // ilw.x vi09, -9(vi01)       |  mul.xyz vf10, vf10, Q
  vu.vf10.mul(Mask::xyz, vu.vf10, vu.Q); /* TODO mulq */   ilw_buffer(Mask::x, vu.vi09, vu.vi01 + -9);
  if (bc) { goto L53; }


  // mtir vi10, vf11.x          |  mul.xyzw vf16, vf16, Q
  vu.vf16.mul_xyzw(vu.vf16, vu.Q); /* TODO mulq */   vu.vi10 = vu.vf11.x_as_u16();
  // mtir vi13, vf11.y          |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi13 = vu.vf11.y_as_u16();
// BRANCH!
  // b L54                      |  mul.xyzw vf12, vf12, vf23
  vu.vf12.mul_xyzw(vu.vf12, vu.vf23);   bc = true;
  // lqi.xyzw vf23, vi03        |  add.xyzw vf10, vf10, vf22
  vu.vf10.add_xyzw(vu.vf10, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L54; }


  L53:
  // lq.xyzw vf20, 0(vi10)      |  mul.xyzw vf16, vf16, Q
  vu.vf16.mul_xyzw(vu.vf16, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10);
  // lq.xyzw vf25, 0(vi13)      |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi13);
  // lq.xyzw vf23, 1(vi10)      |  mul.xyzw vf12, vf12, vf23
  vu.vf12.mul_xyzw(vu.vf12, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi10 + 1);
  // lq.xyzw vf26, 1(vi13)      |  add.xyzw vf10, vf10, vf22
  vu.vf10.add_xyzw(vu.vf10, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi13 + 1);
  // lq.xyzw vf20, 2(vi10)      |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10 + 2);
  // lq.xyzw vf27, 2(vi13)      |  maddw.xyzw vf25, vf25, vf24
  vu.acc.madd_xyzw(vu.vf25, vu.vf25, vu.vf24.w());   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi13 + 2);
  // lq.xyzw vf23, 3(vi10)      |  mulaz.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi10 + 3);
  // lq.xyzw vf28, 3(vi13)      |  maddw.xyzw vf26, vf26, vf24
  vu.acc.madd_xyzw(vu.vf26, vu.vf26, vu.vf24.w());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi13 + 3);
  // lq.xyzw vf20, 4(vi10)      |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10 + 4);
  // lq.xyz vf29, 4(vi13)       |  maddw.xyzw vf27, vf27, vf24
  vu.acc.madd_xyzw(vu.vf27, vu.vf27, vu.vf24.w());   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi13 + 4);
  // lq.xyzw vf23, 5(vi10)      |  mulaz.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi10 + 5);
  // lq.xyz vf30, 5(vi13)       |  maddw.xyzw vf28, vf28, vf24
  vu.acc.madd_xyzw(vu.vf28, vu.vf28, vu.vf24.w());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi13 + 5);
  // lq.xyzw vf20, 6(vi10)      |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10 + 6);
  // lq.xyzw vf31, 6(vi13)      |  maddw.xyz vf29, vf29, vf24
  vu.acc.madd_xyz(vu.vf29, vu.vf29, vu.vf24.w());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi13 + 6);
  // lqi.xyzw vf23, vi02        |  mulaz.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi02++);
  // mtir vi10, vf11.x          |  maddw.xyz vf30, vf30, vf24
  vu.acc.madd_xyz(vu.vf30, vu.vf30, vu.vf24.w());   vu.vi10 = vu.vf11.x_as_u16();
  // mtir vi13, vf11.y          |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   vu.vi13 = vu.vf11.y_as_u16();
// BRANCH!
  // b L38                      |  maddw.xyzw vf31, vf31, vf24
  vu.acc.madd_xyzw(vu.vf31, vu.vf31, vu.vf24.w());   bc = true;
  // lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
  vu.vf24.itof0(Mask::xyzw, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L38; }


  L54:
// BRANCH!
  // ibgez vi09, L55            |  mulaz.xyzw ACC, vf29, vf08
  vu.acc.mula_xyzw(vu.vf29, vu.vf08.z());   bc = ((s16)vu.vi09) >= 0;
  // sq.xyzw vf21, 2(vi11)      |  maddaz.xyzw ACC, vf30, vf11
  vu.acc.madda_xyzw(vu.vf30, vu.vf11.z());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi11 + 2);
  if (bc) { goto L55; }


  // nop                        |  ftoi4.xyzw vf21, vf09
  vu.vf21.ftoi4(Mask::xyzw, vu.vf09);
  L55:
  // mfp.w vf20, P              |  maddz.xyz vf11, vf31, vf14
  vu.acc.madd_xyz(vu.vf11, vu.vf31, vu.vf14.z());   vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17
  vu.vf12.mini_xyzw(vu.vf12, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi11);
  // sq.xyzw vf15, 0(vi14)      |  miniw.w vf10, vf10, vf03
  vu.vf10.mini(Mask::w, vu.vf10, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi14);
  // sq.xyzw vf21, 2(vi14)      |  mulaw.xyzw ACC, vf25, vf08
  vu.acc.mula_xyzw(vu.vf25, vu.vf08.w());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi14 + 2);
  // ilw.y vi09, -6(vi01)       |  mulw.xyzw vf13, vf13, vf20
  vu.vf13.mul_xyzw(vu.vf13, vu.vf20.w());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  // erleng.xyz P, vf11         |  ftoi0.xyzw vf12, vf12
  vu.vf12.ftoi0(Mask::xyzw, vu.vf12);   vu.P = erleng(vu.vf11); /* TODO erleng */
// BRANCH!
  // ibne vi06, vi03, L56       |  maddaw.xyzw ACC, vf26, vf11
  vu.acc.madda_xyzw(vu.vf26, vu.vf11.w());   bc = (vu.vi06 != vu.vi03);
  // mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
  vu.acc.madd_xyzw(vu.vf08, vu.vf27, vu.vf14.w());   vu.vf14.z() = 1.f;
  if (bc) { goto L56; }


// BRANCH!
  // ibne vi07, vi03, L77       |  nop
  bc = (vu.vi07 != vu.vi03);
  // nop                        |  nop

  if (bc) { goto L77; }


// BRANCH!
  // b L153                     |  nop
  bc = true;
  // nop                        |  nop

  if (bc) { goto L153; }


  L56:
  // lqi.xyzw vf09, vi01        |  mulax.xyzw ACC, vf01, vf13
  vu.acc.mula_xyzw(vu.vf01, vu.vf13.x());   lq_buffer_xyzw<DEBUG>(vu.vf09, vu.vi01++);
  // sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13
  vu.acc.madda_xyzw(vu.vf02, vu.vf13.y());   sq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi11 + 1);
  // sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13
  vu.acc.madd_xyzw(vu.vf13, vu.vf03, vu.vf13.z());   sq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi14 + 1);
  // lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  vu.vf08.add_xyzw(vu.vf08, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi01++);
  // lqi.xyzw vf15, vi01        |  maxw.w vf10, vf10, vf02
  vu.vf10.max(Mask::w, vu.vf10, vu.vf02.w());   lq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi01++);
  // mtir vi11, vf09.x          |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   vu.vi11 = vu.vf09.x_as_u16();
  // mtir vi14, vf09.y          |  maxx.xyzw vf13, vf13, vf00
  vu.vf13.max_xyzw(vu.vf13, vu.vf00.x());   vu.vi14 = vu.vf09.y_as_u16();
  // div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  vu.vf09.add(Mask::zw, vu.vf09, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf08.w();

  // move.xyzw vf21, vf10       |  add.xyzw vf12, vf12, vf18
  vu.vf12.add_xyzw(vu.vf12, vu.vf18);   vu.vf21.move_xyzw(vu.vf10);
  // iand vi11, vi11, vi05      |  add.xyzw vf15, vf15, vf19
  vu.vf15.add_xyzw(vu.vf15, vu.vf19);   vu.vi11 = vu.vi11 & vu.vi05;
  // nop                        |  mulax.xyzw ACC, vf04, vf13
  vu.acc.mula_xyzw(vu.vf04, vu.vf13.x());
// BRANCH!
  // ibgtz vi09, L57            |  madday.xyzw ACC, vf05, vf13
  vu.acc.madda_xyzw(vu.vf05, vu.vf13.y());   bc = ((s16)vu.vi09) > 0;
  // iand vi14, vi14, vi05      |  maddaz.xyzw ACC, vf06, vf13
  vu.acc.madda_xyzw(vu.vf06, vu.vf13.z());   vu.vi14 = vu.vi14 & vu.vi05;
  if (bc) { goto L57; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L57:
// BRANCH!
  // ibne vi05, vi11, L58       |  maddw.xyzw vf13, vf07, vf00
  vu.acc.madd_xyzw(vu.vf13, vu.vf07, vu.vf00.w());   bc = (vu.vi05 != vu.vi11);
  // ilw.x vi09, -9(vi01)       |  mul.xyz vf08, vf08, Q
  vu.vf08.mul(Mask::xyz, vu.vf08, vu.Q); /* TODO mulq */   ilw_buffer(Mask::x, vu.vi09, vu.vi01 + -9);
  if (bc) { goto L58; }


  // mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  vu.vf14.mul_xyzw(vu.vf14, vu.Q); /* TODO mulq */   vu.vi11 = vu.vf12.x_as_u16();
  // mtir vi14, vf12.y          |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi14 = vu.vf12.y_as_u16();
// BRANCH!
  // b L59                      |  mul.xyzw vf13, vf13, vf23
  vu.vf13.mul_xyzw(vu.vf13, vu.vf23);   bc = true;
  // lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
  vu.vf08.add_xyzw(vu.vf08, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L59; }


  L58:
  // lq.xyzw vf20, 0(vi11)      |  mul.xyzw vf14, vf14, Q
  vu.vf14.mul_xyzw(vu.vf14, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11);
  // lq.xyzw vf25, 0(vi14)      |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi14);
  // lq.xyzw vf23, 1(vi11)      |  mul.xyzw vf13, vf13, vf23
  vu.vf13.mul_xyzw(vu.vf13, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi11 + 1);
  // lq.xyzw vf26, 1(vi14)      |  add.xyzw vf08, vf08, vf22
  vu.vf08.add_xyzw(vu.vf08, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi14 + 1);
  // lq.xyzw vf20, 2(vi11)      |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11 + 2);
  // lq.xyzw vf27, 2(vi14)      |  maddw.xyzw vf25, vf25, vf24
  vu.acc.madd_xyzw(vu.vf25, vu.vf25, vu.vf24.w());   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi14 + 2);
  // lq.xyzw vf23, 3(vi11)      |  mulaz.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi11 + 3);
  // lq.xyzw vf28, 3(vi14)      |  maddw.xyzw vf26, vf26, vf24
  vu.acc.madd_xyzw(vu.vf26, vu.vf26, vu.vf24.w());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi14 + 3);
  // lq.xyzw vf20, 4(vi11)      |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11 + 4);
  // lq.xyz vf29, 4(vi14)       |  maddw.xyzw vf27, vf27, vf24
  vu.acc.madd_xyzw(vu.vf27, vu.vf27, vu.vf24.w());   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi14 + 4);
  // lq.xyzw vf23, 5(vi11)      |  mulaz.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi11 + 5);
  // lq.xyz vf30, 5(vi14)       |  maddw.xyzw vf28, vf28, vf24
  vu.acc.madd_xyzw(vu.vf28, vu.vf28, vu.vf24.w());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi14 + 5);
  // lq.xyzw vf20, 6(vi11)      |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11 + 6);
  // lq.xyzw vf31, 6(vi14)      |  maddw.xyz vf29, vf29, vf24
  vu.acc.madd_xyz(vu.vf29, vu.vf29, vu.vf24.w());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi14 + 6);
  // lqi.xyzw vf23, vi02        |  mulaz.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi02++);
  // mtir vi11, vf12.x          |  maddw.xyz vf30, vf30, vf24
  vu.acc.madd_xyz(vu.vf30, vu.vf30, vu.vf24.w());   vu.vi11 = vu.vf12.x_as_u16();
  // mtir vi14, vf12.y          |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   vu.vi14 = vu.vf12.y_as_u16();
// BRANCH!
  // b L43                      |  maddw.xyzw vf31, vf31, vf24
  vu.acc.madd_xyzw(vu.vf31, vu.vf31, vu.vf24.w());   bc = true;
  // lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
  vu.vf24.itof0(Mask::xyzw, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L43; }


  L59:
// BRANCH!
  // ibgez vi09, L60            |  mulaz.xyzw ACC, vf29, vf09
  vu.acc.mula_xyzw(vu.vf29, vu.vf09.z());   bc = ((s16)vu.vi09) >= 0;
  // sq.xyzw vf21, 2(vi12)      |  maddaz.xyzw ACC, vf30, vf12
  vu.acc.madda_xyzw(vu.vf30, vu.vf12.z());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi12 + 2);
  if (bc) { goto L60; }


  // nop                        |  ftoi4.xyzw vf21, vf10
  vu.vf21.ftoi4(Mask::xyzw, vu.vf10);
  L60:
  // mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  vu.acc.madd_xyz(vu.vf12, vu.vf31, vu.vf15.z());   vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  vu.vf13.mini_xyzw(vu.vf13, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi12);
  // sq.xyzw vf16, 0(vi15)      |  miniw.w vf08, vf08, vf03
  vu.vf08.mini(Mask::w, vu.vf08, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi15);
  // sq.xyzw vf21, 2(vi15)      |  mulaw.xyzw ACC, vf25, vf09
  vu.acc.mula_xyzw(vu.vf25, vu.vf09.w());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi15 + 2);
  // ilw.y vi09, -6(vi01)       |  mulw.xyzw vf11, vf11, vf20
  vu.vf11.mul_xyzw(vu.vf11, vu.vf20.w());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  // erleng.xyz P, vf12         |  ftoi0.xyzw vf13, vf13
  vu.vf13.ftoi0(Mask::xyzw, vu.vf13);   vu.P = erleng(vu.vf12); /* TODO erleng */
// BRANCH!
  // ibne vi06, vi03, L45       |  maddaw.xyzw ACC, vf26, vf12
  vu.acc.madda_xyzw(vu.vf26, vu.vf12.w());   bc = (vu.vi06 != vu.vi03);
  // mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  vu.acc.madd_xyzw(vu.vf09, vu.vf27, vu.vf15.w());   vu.vf15.z() = 1.f;
  if (bc) { goto L45; }


// BRANCH!
  // ibne vi07, vi03, L72       |  nop
  bc = (vu.vi07 != vu.vi03);
  // nop                        |  nop

  if (bc) { goto L72; }


// BRANCH!
  // b L163                     |  nop
  bc = true;
  // nop                        |  nop

  if (bc) { goto L163; }


  L61:
  // lqi.xyzw vf08, vi01        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf08, vu.vi01++);
  // lqi.xyzw vf24, vi02        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf24, vu.vi02++);
  // lqi.xyzw vf11, vi01        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi01++);
  // lqi.xyzw vf14, vi01        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi01++);
  // mtir vi10, vf08.x          |  nop
  vu.vi10 = vu.vf08.x_as_u16();
  // mtir vi13, vf08.y          |  itof0.xyzw vf24, vf24
  vu.vf24.itof0(Mask::xyzw, vu.vf24);   vu.vi13 = vu.vf08.y_as_u16();
  // nop                        |  add.zw vf08, vf08, vf17
  vu.vf08.add(Mask::zw, vu.vf08, vu.vf17);
  // nop                        |  add.xyzw vf11, vf11, vf18
  vu.vf11.add_xyzw(vu.vf11, vu.vf18);
  // iand vi10, vi10, vi05      |  add.xyzw vf14, vf14, vf19
  vu.vf14.add_xyzw(vu.vf14, vu.vf19);   vu.vi10 = vu.vi10 & vu.vi05;
  // ilw.w vi08, -1(vi02)       |  mulw.xyzw vf24, vf24, vf29
  vu.vf24.mul_xyzw(vu.vf24, vu.vf29.w());   ilw_buffer(Mask::w, vu.vi08, vu.vi02 + -1);
  // iand vi13, vi13, vi05      |  nop
  vu.vi13 = vu.vi13 & vu.vi05;
  // lq.xyzw vf20, 0(vi10)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10);
  // lq.xyzw vf31, 0(vi13)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi13);
  // lq.xyzw vf25, 0(vi08)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi08);
  // lq.xyzw vf23, 1(vi10)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi10 + 1);
  // lq.xyzw vf20, 1(vi13)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi13 + 1);
  // lq.xyzw vf26, 1(vi08)      |  madday.xyzw ACC, vf31, vf24
  vu.acc.madda_xyzw(vu.vf31, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi08 + 1);
  // lq.xyzw vf31, 2(vi10)      |  maddz.xyzw vf25, vf25, vf24
  vu.acc.madd_xyzw(vu.vf25, vu.vf25, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi10 + 2);
  // lq.xyzw vf23, 2(vi13)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi13 + 2);
  // lq.xyzw vf27, 2(vi08)      |  madday.xyzw ACC, vf20, vf24
  vu.acc.madda_xyzw(vu.vf20, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi08 + 2);
  // lq.xyzw vf20, 3(vi10)      |  maddz.xyzw vf26, vf26, vf24
  vu.acc.madd_xyzw(vu.vf26, vu.vf26, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10 + 3);
  // lq.xyzw vf31, 3(vi13)      |  mulax.xyzw ACC, vf31, vf24
  vu.acc.mula_xyzw(vu.vf31, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi13 + 3);
  // lq.xyzw vf28, 3(vi08)      |  madday.xyzw ACC, vf23, vf24
  vu.acc.madda_xyzw(vu.vf23, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi08 + 3);
  // lq.xyzw vf23, 4(vi10)      |  maddz.xyzw vf27, vf27, vf24
  vu.acc.madd_xyzw(vu.vf27, vu.vf27, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi10 + 4);
  // lq.xyzw vf20, 4(vi13)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi13 + 4);
  // lq.xyz vf29, 4(vi08)       |  madday.xyzw ACC, vf31, vf24
  vu.acc.madda_xyzw(vu.vf31, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi08 + 4);
  // lq.xyzw vf31, 5(vi10)      |  maddz.xyzw vf28, vf28, vf24
  vu.acc.madd_xyzw(vu.vf28, vu.vf28, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi10 + 5);
  // lq.xyzw vf23, 5(vi13)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi13 + 5);
  // lq.xyz vf30, 5(vi08)       |  madday.xyzw ACC, vf20, vf24
  vu.acc.madda_xyzw(vu.vf20, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi08 + 5);
  // lq.xyzw vf20, 6(vi10)      |  maddz.xyz vf29, vf29, vf24
  vu.acc.madd_xyz(vu.vf29, vu.vf29, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10 + 6);
  // lq.xyzw vf22, 6(vi13)      |  mulax.xyzw ACC, vf31, vf24
  vu.acc.mula_xyzw(vu.vf31, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf22, vu.vi13 + 6);
  // lq.xyzw vf31, 6(vi08)      |  madday.xyzw ACC, vf23, vf24
  vu.acc.madda_xyzw(vu.vf23, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi08 + 6);
  // lqi.xyzw vf23, vi02        |  maddz.xyz vf30, vf30, vf24
  vu.acc.madd_xyz(vu.vf30, vu.vf30, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi02++);
  // mtir vi10, vf11.x          |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   vu.vi10 = vu.vf11.x_as_u16();
  // mtir vi13, vf11.y          |  madday.xyzw ACC, vf22, vf24
  vu.acc.madda_xyzw(vu.vf22, vu.vf24.y());   vu.vi13 = vu.vf11.y_as_u16();
  // lq.xyzw vf22, 2(vi00)      |  maddz.xyzw vf31, vf31, vf24
  vu.acc.madd_xyzw(vu.vf31, vu.vf31, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf22, 2);
  // nop                        |  itof0.xyzw vf24, vf23
  vu.vf24.itof0(Mask::xyzw, vu.vf23);
  // nop                        |  mulaz.xyzw ACC, vf29, vf08
  vu.acc.mula_xyzw(vu.vf29, vu.vf08.z());
  // nop                        |  maddaz.xyzw ACC, vf30, vf11
  vu.acc.madda_xyzw(vu.vf30, vu.vf11.z());
  // nop                        |  maddz.xyz vf11, vf31, vf14
  vu.acc.madd_xyz(vu.vf11, vu.vf31, vu.vf14.z());
  // nop                        |  nop

  // nop                        |  nop

  // nop                        |  mulaw.xyzw ACC, vf25, vf08
  vu.acc.mula_xyzw(vu.vf25, vu.vf08.w());
  // nop                        |  nop

  // erleng.xyz P, vf11         |  nop
  vu.P = erleng(vu.vf11); /* TODO erleng */
  // ior vi15, vi07, vi00       |  maddaw.xyzw ACC, vf26, vf11
  vu.acc.madda_xyzw(vu.vf26, vu.vf11.w());   vu.vi15 = vu.vi07;
  // mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
  vu.acc.madd_xyzw(vu.vf08, vu.vf27, vu.vf14.w());   vu.vf14.z() = 1.f;
  // lqi.xyzw vf09, vi01        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf09, vu.vi01++);
  // ilwr.y vi03, vi12          |  nop
  ilw_buffer(Mask::y, vu.vi03, vu.vi12);
  // ilw.z vi07, 1(vi12)        |  nop
  ilw_buffer(Mask::z, vu.vi07, vu.vi12 + 1);
  // lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  vu.vf08.add_xyzw(vu.vf08, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi01++);
  // lqi.xyzw vf15, vi01        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi01++);
  // mtir vi11, vf09.x          |  nop
  vu.vi11 = vu.vf09.x_as_u16();
// BRANCH!
  // ibeq vi00, vi15, L62       |  nop
  bc = (vu.vi15 == 0);
  // mtir vi14, vf09.y          |  nop
  vu.vi14 = vu.vf09.y_as_u16();
  if (bc) { goto L62; }


  // nop                        |  miniw.w vf08, vf08, vf01
  vu.vf08.mini(Mask::w, vu.vf08, vu.vf01.w());
  L62:
  // div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  vu.vf09.add(Mask::zw, vu.vf09, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf08.w();

  // iadd vi03, vi03, vi12      |  add.xyzw vf12, vf12, vf18
  vu.vf12.add_xyzw(vu.vf12, vu.vf18);   vu.vi03 = vu.vi03 + vu.vi12;
  // iand vi11, vi11, vi05      |  add.xyzw vf15, vf15, vf19
  vu.vf15.add_xyzw(vu.vf15, vu.vf19);   vu.vi11 = vu.vi11 & vu.vi05;
  // ilw.w vi08, -1(vi02)       |  nop
  ilw_buffer(Mask::w, vu.vi08, vu.vi02 + -1);
  // iadd vi07, vi07, vi03      |  nop
  vu.vi07 = vu.vi07 + vu.vi03;
  // iand vi14, vi14, vi05      |  nop
  vu.vi14 = vu.vi14 & vu.vi05;
// BRANCH!
  // ibne vi05, vi11, L63       |  nop
  bc = (vu.vi05 != vu.vi11);
  // iaddi vi07, vi07, -0x1     |  mul.xyz vf08, vf08, Q
  vu.vf08.mul(Mask::xyz, vu.vf08, vu.Q); /* TODO mulq */   vu.vi07 = vu.vi07 + -1;
  if (bc) { goto L63; }


  // mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  vu.vf14.mul_xyzw(vu.vf14, vu.Q); /* TODO mulq */   vu.vi11 = vu.vf12.x_as_u16();
  // mtir vi14, vf12.y          |  nop
  vu.vi14 = vu.vf12.y_as_u16();
// BRANCH!
  // b L64                      |  nop
  bc = true;
  // lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
  vu.vf08.add_xyzw(vu.vf08, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L64; }


  L63:
  // lq.xyzw vf20, 0(vi11)      |  mul.xyzw vf14, vf14, Q
  vu.vf14.mul_xyzw(vu.vf14, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11);
  // nop                        |  mulw.xyzw vf24, vf24, vf29
  vu.vf24.mul_xyzw(vu.vf24, vu.vf29.w());
  // lq.xyzw vf31, 0(vi14)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi14);
  // lq.xyzw vf25, 0(vi08)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi08);
  // lq.xyzw vf23, 1(vi11)      |  add.xyzw vf08, vf08, vf22
  vu.vf08.add_xyzw(vu.vf08, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi11 + 1);
  // lq.xyzw vf20, 1(vi14)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi14 + 1);
  // lq.xyzw vf26, 1(vi08)      |  madday.xyzw ACC, vf31, vf24
  vu.acc.madda_xyzw(vu.vf31, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi08 + 1);
  // lq.xyzw vf31, 2(vi11)      |  maddz.xyzw vf25, vf25, vf24
  vu.acc.madd_xyzw(vu.vf25, vu.vf25, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi11 + 2);
  // lq.xyzw vf23, 2(vi14)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi14 + 2);
  // lq.xyzw vf27, 2(vi08)      |  madday.xyzw ACC, vf20, vf24
  vu.acc.madda_xyzw(vu.vf20, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi08 + 2);
  // lq.xyzw vf20, 3(vi11)      |  maddz.xyzw vf26, vf26, vf24
  vu.acc.madd_xyzw(vu.vf26, vu.vf26, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11 + 3);
  // lq.xyzw vf31, 3(vi14)      |  mulax.xyzw ACC, vf31, vf24
  vu.acc.mula_xyzw(vu.vf31, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi14 + 3);
  // lq.xyzw vf28, 3(vi08)      |  madday.xyzw ACC, vf23, vf24
  vu.acc.madda_xyzw(vu.vf23, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi08 + 3);
  // lq.xyzw vf23, 4(vi11)      |  maddz.xyzw vf27, vf27, vf24
  vu.acc.madd_xyzw(vu.vf27, vu.vf27, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi11 + 4);
  // lq.xyzw vf20, 4(vi14)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi14 + 4);
  // lq.xyz vf29, 4(vi08)       |  madday.xyzw ACC, vf31, vf24
  vu.acc.madda_xyzw(vu.vf31, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi08 + 4);
  // lq.xyzw vf31, 5(vi11)      |  maddz.xyzw vf28, vf28, vf24
  vu.acc.madd_xyzw(vu.vf28, vu.vf28, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi11 + 5);
  // lq.xyzw vf23, 5(vi14)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi14 + 5);
  // lq.xyz vf30, 5(vi08)       |  madday.xyzw ACC, vf20, vf24
  vu.acc.madda_xyzw(vu.vf20, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi08 + 5);
  // lq.xyzw vf20, 6(vi11)      |  maddz.xyz vf29, vf29, vf24
  vu.acc.madd_xyz(vu.vf29, vu.vf29, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11 + 6);
  // lq.xyzw vf22, 6(vi14)      |  mulax.xyzw ACC, vf31, vf24
  vu.acc.mula_xyzw(vu.vf31, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf22, vu.vi14 + 6);
  // lq.xyzw vf31, 6(vi08)      |  madday.xyzw ACC, vf23, vf24
  vu.acc.madda_xyzw(vu.vf23, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi08 + 6);
  // lqi.xyzw vf23, vi02        |  maddz.xyz vf30, vf30, vf24
  vu.acc.madd_xyz(vu.vf30, vu.vf30, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi02++);
  // mtir vi11, vf12.x          |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   vu.vi11 = vu.vf12.x_as_u16();
  // mtir vi14, vf12.y          |  madday.xyzw ACC, vf22, vf24
  vu.acc.madda_xyzw(vu.vf22, vu.vf24.y());   vu.vi14 = vu.vf12.y_as_u16();
  // lq.xyzw vf22, 2(vi00)      |  maddz.xyzw vf31, vf31, vf24
  vu.acc.madd_xyzw(vu.vf31, vu.vf31, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf22, 2);
  // lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
  vu.vf24.itof0(Mask::xyzw, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  L64:
  // nop                        |  mulaz.xyzw ACC, vf29, vf09
  vu.acc.mula_xyzw(vu.vf29, vu.vf09.z());
  // nop                        |  maddaz.xyzw ACC, vf30, vf12
  vu.acc.madda_xyzw(vu.vf30, vu.vf12.z());
  // mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  vu.acc.madd_xyz(vu.vf12, vu.vf31, vu.vf15.z());   vu.vf20.mfp(Mask::w, vu.P);
  // nop                        |  nop

  // 1024.0                     |  miniw.w vf08, vf08, vf03 :i
  vu.vf08.mini(Mask::w, vu.vf08, vu.vf03.w());   vu.I = 1024.0;
// BRANCH!
  // ibne vi00, vi15, L125      |  mulaw.xyzw ACC, vf25, vf09
  vu.acc.mula_xyzw(vu.vf25, vu.vf09.w());   bc = (vu.vi15 != 0);
  // ilw.y vi09, -6(vi01)       |  mulw.xyzw vf11, vf11, vf20
  vu.vf11.mul_xyzw(vu.vf11, vu.vf20.w());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  if (bc) { goto L125; }


  // erleng.xyz P, vf12         |  nop
  vu.P = erleng(vu.vf12); /* TODO erleng */
  // nop                        |  maddaw.xyzw ACC, vf26, vf12
  vu.acc.madda_xyzw(vu.vf26, vu.vf12.w());
  // mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  vu.acc.madd_xyzw(vu.vf09, vu.vf27, vu.vf15.w());   vu.vf15.z() = 1.f;
  L65:
  // lqi.xyzw vf10, vi01        |  mulax.xyzw ACC, vf01, vf11
  vu.acc.mula_xyzw(vu.vf01, vu.vf11.x());   lq_buffer_xyzw<DEBUG>(vu.vf10, vu.vi01++);
// BRANCH!
  // b L67                      |  madday.xyzw ACC, vf02, vf11
  vu.acc.madda_xyzw(vu.vf02, vu.vf11.y());   bc = true;
  // nop                        |  maddz.xyzw vf11, vf03, vf11
  vu.acc.madd_xyzw(vu.vf11, vu.vf03, vu.vf11.z());
  if (bc) { goto L67; }


  L66:
  // lqi.xyzw vf10, vi01        |  mulax.xyzw ACC, vf01, vf11
  vu.acc.mula_xyzw(vu.vf01, vu.vf11.x());   lq_buffer_xyzw<DEBUG>(vu.vf10, vu.vi01++);
  // sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  vu.acc.madda_xyzw(vu.vf02, vu.vf11.y());   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi12 + 1);
  // sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
  vu.acc.madd_xyzw(vu.vf11, vu.vf03, vu.vf11.z());   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi15 + 1);
  L67:
  // lqi.xyzw vf13, vi01        |  add.xyzw vf09, vf09, vf28
  vu.vf09.add_xyzw(vu.vf09, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi01++);
  // lqi.xyzw vf16, vi01        |  maxw.w vf08, vf08, vf02
  vu.vf08.max(Mask::w, vu.vf08, vu.vf02.w());   lq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi01++);
  // mtir vi12, vf10.x          |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   vu.vi12 = vu.vf10.x_as_u16();
  // mtir vi15, vf10.y          |  maxx.xyzw vf11, vf11, vf00
  vu.vf11.max_xyzw(vu.vf11, vu.vf00.x());   vu.vi15 = vu.vf10.y_as_u16();
  // div Q, vf01.w, vf09.w      |  add.zw vf10, vf10, vf17
  vu.vf10.add(Mask::zw, vu.vf10, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf09.w();

  // move.xyzw vf21, vf08       |  add.xyzw vf13, vf13, vf18
  vu.vf13.add_xyzw(vu.vf13, vu.vf18);   vu.vf21.move_xyzw(vu.vf08);
  // iand vi12, vi12, vi05      |  add.xyzw vf16, vf16, vf19
  vu.vf16.add_xyzw(vu.vf16, vu.vf19);   vu.vi12 = vu.vi12 & vu.vi05;
  // ilw.w vi08, -1(vi02)       |  mulax.xyzw ACC, vf04, vf11
  vu.acc.mula_xyzw(vu.vf04, vu.vf11.x());   ilw_buffer(Mask::w, vu.vi08, vu.vi02 + -1);
// BRANCH!
  // ibgtz vi09, L68            |  madday.xyzw ACC, vf05, vf11
  vu.acc.madda_xyzw(vu.vf05, vu.vf11.y());   bc = ((s16)vu.vi09) > 0;
  // iand vi15, vi15, vi05      |  maddaz.xyzw ACC, vf06, vf11
  vu.acc.madda_xyzw(vu.vf06, vu.vf11.z());   vu.vi15 = vu.vi15 & vu.vi05;
  if (bc) { goto L68; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L68:
// BRANCH!
  // ibne vi05, vi12, L69       |  maddw.xyzw vf11, vf07, vf00
  vu.acc.madd_xyzw(vu.vf11, vu.vf07, vu.vf00.w());   bc = (vu.vi05 != vu.vi12);
  // ilw.x vi09, -9(vi01)       |  mul.xyz vf09, vf09, Q
  vu.vf09.mul(Mask::xyz, vu.vf09, vu.Q); /* TODO mulq */   ilw_buffer(Mask::x, vu.vi09, vu.vi01 + -9);
  if (bc) { goto L69; }


  // mtir vi12, vf13.x          |  mul.xyzw vf15, vf15, Q
  vu.vf15.mul_xyzw(vu.vf15, vu.Q); /* TODO mulq */   vu.vi12 = vu.vf13.x_as_u16();
  // mtir vi15, vf13.y          |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi15 = vu.vf13.y_as_u16();
// BRANCH!
  // b L70                      |  mul.xyzw vf11, vf11, vf23
  vu.vf11.mul_xyzw(vu.vf11, vu.vf23);   bc = true;
  // lqi.xyzw vf23, vi03        |  add.xyzw vf09, vf09, vf22
  vu.vf09.add_xyzw(vu.vf09, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L70; }


  L69:
  // lq.xyzw vf20, 0(vi12)      |  mul.xyzw vf15, vf15, Q
  vu.vf15.mul_xyzw(vu.vf15, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi12);
  // nop                        |  mulw.xyzw vf24, vf24, vf29
  vu.vf24.mul_xyzw(vu.vf24, vu.vf29.w());
  // lq.xyzw vf31, 0(vi15)      |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi15);
  // lq.xyzw vf25, 0(vi08)      |  mul.xyzw vf11, vf11, vf23
  vu.vf11.mul_xyzw(vu.vf11, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi08);
  // lq.xyzw vf23, 1(vi12)      |  add.xyzw vf09, vf09, vf22
  vu.vf09.add_xyzw(vu.vf09, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi12 + 1);
  // lq.xyzw vf20, 1(vi15)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi15 + 1);
  // lq.xyzw vf26, 1(vi08)      |  madday.xyzw ACC, vf31, vf24
  vu.acc.madda_xyzw(vu.vf31, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi08 + 1);
  // lq.xyzw vf31, 2(vi12)      |  maddz.xyzw vf25, vf25, vf24
  vu.acc.madd_xyzw(vu.vf25, vu.vf25, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi12 + 2);
  // lq.xyzw vf23, 2(vi15)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi15 + 2);
  // lq.xyzw vf27, 2(vi08)      |  madday.xyzw ACC, vf20, vf24
  vu.acc.madda_xyzw(vu.vf20, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi08 + 2);
  // lq.xyzw vf20, 3(vi12)      |  maddz.xyzw vf26, vf26, vf24
  vu.acc.madd_xyzw(vu.vf26, vu.vf26, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi12 + 3);
  // lq.xyzw vf31, 3(vi15)      |  mulax.xyzw ACC, vf31, vf24
  vu.acc.mula_xyzw(vu.vf31, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi15 + 3);
  // lq.xyzw vf28, 3(vi08)      |  madday.xyzw ACC, vf23, vf24
  vu.acc.madda_xyzw(vu.vf23, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi08 + 3);
  // lq.xyzw vf23, 4(vi12)      |  maddz.xyzw vf27, vf27, vf24
  vu.acc.madd_xyzw(vu.vf27, vu.vf27, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi12 + 4);
  // lq.xyzw vf20, 4(vi15)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi15 + 4);
  // lq.xyz vf29, 4(vi08)       |  madday.xyzw ACC, vf31, vf24
  vu.acc.madda_xyzw(vu.vf31, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi08 + 4);
  // lq.xyzw vf31, 5(vi12)      |  maddz.xyzw vf28, vf28, vf24
  vu.acc.madd_xyzw(vu.vf28, vu.vf28, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi12 + 5);
  // lq.xyzw vf23, 5(vi15)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi15 + 5);
  // lq.xyz vf30, 5(vi08)       |  madday.xyzw ACC, vf20, vf24
  vu.acc.madda_xyzw(vu.vf20, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi08 + 5);
  // lq.xyzw vf20, 6(vi12)      |  maddz.xyz vf29, vf29, vf24
  vu.acc.madd_xyz(vu.vf29, vu.vf29, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi12 + 6);
  // lq.xyzw vf22, 6(vi15)      |  mulax.xyzw ACC, vf31, vf24
  vu.acc.mula_xyzw(vu.vf31, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf22, vu.vi15 + 6);
  // lq.xyzw vf31, 6(vi08)      |  madday.xyzw ACC, vf23, vf24
  vu.acc.madda_xyzw(vu.vf23, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi08 + 6);
  // lqi.xyzw vf23, vi02        |  maddz.xyz vf30, vf30, vf24
  vu.acc.madd_xyz(vu.vf30, vu.vf30, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi02++);
  // mtir vi12, vf13.x          |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   vu.vi12 = vu.vf13.x_as_u16();
  // mtir vi15, vf13.y          |  madday.xyzw ACC, vf22, vf24
  vu.acc.madda_xyzw(vu.vf22, vu.vf24.y());   vu.vi15 = vu.vf13.y_as_u16();
  // lq.xyzw vf22, 2(vi00)      |  maddz.xyzw vf31, vf31, vf24
  vu.acc.madd_xyzw(vu.vf31, vu.vf31, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf22, 2);
  // lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
  vu.vf24.itof0(Mask::xyzw, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  L70:
// BRANCH!
  // ibgez vi09, L71            |  mulaz.xyzw ACC, vf29, vf10
  vu.acc.mula_xyzw(vu.vf29, vu.vf10.z());   bc = ((s16)vu.vi09) >= 0;
  // sq.xyzw vf21, 2(vi10)      |  maddaz.xyzw ACC, vf30, vf13
  vu.acc.madda_xyzw(vu.vf30, vu.vf13.z());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi10 + 2);
  if (bc) { goto L71; }


  // nop                        |  ftoi4.xyzw vf21, vf08
  vu.vf21.ftoi4(Mask::xyzw, vu.vf08);
  L71:
  // mfp.w vf20, P              |  maddz.xyz vf13, vf31, vf16
  vu.acc.madd_xyz(vu.vf13, vu.vf31, vu.vf16.z());   vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  vu.vf11.mini_xyzw(vu.vf11, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi10);
  // sq.xyzw vf14, 0(vi13)      |  miniw.w vf09, vf09, vf03
  vu.vf09.mini(Mask::w, vu.vf09, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi13);
  // sq.xyzw vf21, 2(vi13)      |  mulaw.xyzw ACC, vf25, vf10
  vu.acc.mula_xyzw(vu.vf25, vu.vf10.w());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi13 + 2);
  // ilw.y vi09, -6(vi01)       |  mulw.xyzw vf12, vf12, vf20
  vu.vf12.mul_xyzw(vu.vf12, vu.vf20.w());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  // erleng.xyz P, vf13         |  ftoi0.xyzw vf11, vf11
  vu.vf11.ftoi0(Mask::xyzw, vu.vf11);   vu.P = erleng(vu.vf13); /* TODO erleng */
// BRANCH!
  // ibeq vi07, vi03, L143      |  maddaw.xyzw ACC, vf26, vf13
  vu.acc.madda_xyzw(vu.vf26, vu.vf13.w());   bc = (vu.vi07 == vu.vi03);
  // mr32.z vf16, vf00          |  maddw.xyzw vf10, vf27, vf16
  vu.acc.madd_xyzw(vu.vf10, vu.vf27, vu.vf16.w());   vu.vf16.z() = 1.f;
  if (bc) { goto L143; }


  L72:
  // lqi.xyzw vf08, vi01        |  mulax.xyzw ACC, vf01, vf12
  vu.acc.mula_xyzw(vu.vf01, vu.vf12.x());   lq_buffer_xyzw<DEBUG>(vu.vf08, vu.vi01++);
  // sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12
  vu.acc.madda_xyzw(vu.vf02, vu.vf12.y());   sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi10 + 1);
  // sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12
  vu.acc.madd_xyzw(vu.vf12, vu.vf03, vu.vf12.z());   sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi13 + 1);
  // lqi.xyzw vf11, vi01        |  add.xyzw vf10, vf10, vf28
  vu.vf10.add_xyzw(vu.vf10, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi01++);
  // lqi.xyzw vf14, vi01        |  maxw.w vf09, vf09, vf02
  vu.vf09.max(Mask::w, vu.vf09, vu.vf02.w());   lq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi01++);
  // mtir vi10, vf08.x          |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   vu.vi10 = vu.vf08.x_as_u16();
  // mtir vi13, vf08.y          |  maxx.xyzw vf12, vf12, vf00
  vu.vf12.max_xyzw(vu.vf12, vu.vf00.x());   vu.vi13 = vu.vf08.y_as_u16();
  // div Q, vf01.w, vf10.w      |  add.zw vf08, vf08, vf17
  vu.vf08.add(Mask::zw, vu.vf08, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf10.w();

  // move.xyzw vf21, vf09       |  add.xyzw vf11, vf11, vf18
  vu.vf11.add_xyzw(vu.vf11, vu.vf18);   vu.vf21.move_xyzw(vu.vf09);
  // iand vi10, vi10, vi05      |  add.xyzw vf14, vf14, vf19
  vu.vf14.add_xyzw(vu.vf14, vu.vf19);   vu.vi10 = vu.vi10 & vu.vi05;
  // ilw.w vi08, -1(vi02)       |  mulax.xyzw ACC, vf04, vf12
  vu.acc.mula_xyzw(vu.vf04, vu.vf12.x());   ilw_buffer(Mask::w, vu.vi08, vu.vi02 + -1);
// BRANCH!
  // ibgtz vi09, L73            |  madday.xyzw ACC, vf05, vf12
  vu.acc.madda_xyzw(vu.vf05, vu.vf12.y());   bc = ((s16)vu.vi09) > 0;
  // iand vi13, vi13, vi05      |  maddaz.xyzw ACC, vf06, vf12
  vu.acc.madda_xyzw(vu.vf06, vu.vf12.z());   vu.vi13 = vu.vi13 & vu.vi05;
  if (bc) { goto L73; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L73:
// BRANCH!
  // ibne vi05, vi10, L74       |  maddw.xyzw vf12, vf07, vf00
  vu.acc.madd_xyzw(vu.vf12, vu.vf07, vu.vf00.w());   bc = (vu.vi05 != vu.vi10);
  // ilw.x vi09, -9(vi01)       |  mul.xyz vf10, vf10, Q
  vu.vf10.mul(Mask::xyz, vu.vf10, vu.Q); /* TODO mulq */   ilw_buffer(Mask::x, vu.vi09, vu.vi01 + -9);
  if (bc) { goto L74; }


  // mtir vi10, vf11.x          |  mul.xyzw vf16, vf16, Q
  vu.vf16.mul_xyzw(vu.vf16, vu.Q); /* TODO mulq */   vu.vi10 = vu.vf11.x_as_u16();
  // mtir vi13, vf11.y          |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi13 = vu.vf11.y_as_u16();
// BRANCH!
  // b L75                      |  mul.xyzw vf12, vf12, vf23
  vu.vf12.mul_xyzw(vu.vf12, vu.vf23);   bc = true;
  // lqi.xyzw vf23, vi03        |  add.xyzw vf10, vf10, vf22
  vu.vf10.add_xyzw(vu.vf10, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L75; }


  L74:
  // lq.xyzw vf20, 0(vi10)      |  mul.xyzw vf16, vf16, Q
  vu.vf16.mul_xyzw(vu.vf16, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10);
  // nop                        |  mulw.xyzw vf24, vf24, vf29
  vu.vf24.mul_xyzw(vu.vf24, vu.vf29.w());
  // lq.xyzw vf31, 0(vi13)      |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi13);
  // lq.xyzw vf25, 0(vi08)      |  mul.xyzw vf12, vf12, vf23
  vu.vf12.mul_xyzw(vu.vf12, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi08);
  // lq.xyzw vf23, 1(vi10)      |  add.xyzw vf10, vf10, vf22
  vu.vf10.add_xyzw(vu.vf10, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi10 + 1);
  // lq.xyzw vf20, 1(vi13)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi13 + 1);
  // lq.xyzw vf26, 1(vi08)      |  madday.xyzw ACC, vf31, vf24
  vu.acc.madda_xyzw(vu.vf31, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi08 + 1);
  // lq.xyzw vf31, 2(vi10)      |  maddz.xyzw vf25, vf25, vf24
  vu.acc.madd_xyzw(vu.vf25, vu.vf25, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi10 + 2);
  // lq.xyzw vf23, 2(vi13)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi13 + 2);
  // lq.xyzw vf27, 2(vi08)      |  madday.xyzw ACC, vf20, vf24
  vu.acc.madda_xyzw(vu.vf20, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi08 + 2);
  // lq.xyzw vf20, 3(vi10)      |  maddz.xyzw vf26, vf26, vf24
  vu.acc.madd_xyzw(vu.vf26, vu.vf26, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10 + 3);
  // lq.xyzw vf31, 3(vi13)      |  mulax.xyzw ACC, vf31, vf24
  vu.acc.mula_xyzw(vu.vf31, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi13 + 3);
  // lq.xyzw vf28, 3(vi08)      |  madday.xyzw ACC, vf23, vf24
  vu.acc.madda_xyzw(vu.vf23, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi08 + 3);
  // lq.xyzw vf23, 4(vi10)      |  maddz.xyzw vf27, vf27, vf24
  vu.acc.madd_xyzw(vu.vf27, vu.vf27, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi10 + 4);
  // lq.xyzw vf20, 4(vi13)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi13 + 4);
  // lq.xyz vf29, 4(vi08)       |  madday.xyzw ACC, vf31, vf24
  vu.acc.madda_xyzw(vu.vf31, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi08 + 4);
  // lq.xyzw vf31, 5(vi10)      |  maddz.xyzw vf28, vf28, vf24
  vu.acc.madd_xyzw(vu.vf28, vu.vf28, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi10 + 5);
  // lq.xyzw vf23, 5(vi13)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi13 + 5);
  // lq.xyz vf30, 5(vi08)       |  madday.xyzw ACC, vf20, vf24
  vu.acc.madda_xyzw(vu.vf20, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi08 + 5);
  // lq.xyzw vf20, 6(vi10)      |  maddz.xyz vf29, vf29, vf24
  vu.acc.madd_xyz(vu.vf29, vu.vf29, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10 + 6);
  // lq.xyzw vf22, 6(vi13)      |  mulax.xyzw ACC, vf31, vf24
  vu.acc.mula_xyzw(vu.vf31, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf22, vu.vi13 + 6);
  // lq.xyzw vf31, 6(vi08)      |  madday.xyzw ACC, vf23, vf24
  vu.acc.madda_xyzw(vu.vf23, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi08 + 6);
  // lqi.xyzw vf23, vi02        |  maddz.xyz vf30, vf30, vf24
  vu.acc.madd_xyz(vu.vf30, vu.vf30, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi02++);
  // mtir vi10, vf11.x          |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   vu.vi10 = vu.vf11.x_as_u16();
  // mtir vi13, vf11.y          |  madday.xyzw ACC, vf22, vf24
  vu.acc.madda_xyzw(vu.vf22, vu.vf24.y());   vu.vi13 = vu.vf11.y_as_u16();
  // lq.xyzw vf22, 2(vi00)      |  maddz.xyzw vf31, vf31, vf24
  vu.acc.madd_xyzw(vu.vf31, vu.vf31, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf22, 2);
  // lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
  vu.vf24.itof0(Mask::xyzw, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  L75:
// BRANCH!
  // ibgez vi09, L76            |  mulaz.xyzw ACC, vf29, vf08
  vu.acc.mula_xyzw(vu.vf29, vu.vf08.z());   bc = ((s16)vu.vi09) >= 0;
  // sq.xyzw vf21, 2(vi11)      |  maddaz.xyzw ACC, vf30, vf11
  vu.acc.madda_xyzw(vu.vf30, vu.vf11.z());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi11 + 2);
  if (bc) { goto L76; }


  // nop                        |  ftoi4.xyzw vf21, vf09
  vu.vf21.ftoi4(Mask::xyzw, vu.vf09);
  L76:
  // mfp.w vf20, P              |  maddz.xyz vf11, vf31, vf14
  vu.acc.madd_xyz(vu.vf11, vu.vf31, vu.vf14.z());   vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17
  vu.vf12.mini_xyzw(vu.vf12, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi11);
  // sq.xyzw vf15, 0(vi14)      |  miniw.w vf10, vf10, vf03
  vu.vf10.mini(Mask::w, vu.vf10, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi14);
  // sq.xyzw vf21, 2(vi14)      |  mulaw.xyzw ACC, vf25, vf08
  vu.acc.mula_xyzw(vu.vf25, vu.vf08.w());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi14 + 2);
  // ilw.y vi09, -6(vi01)       |  mulw.xyzw vf13, vf13, vf20
  vu.vf13.mul_xyzw(vu.vf13, vu.vf20.w());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  // erleng.xyz P, vf11         |  ftoi0.xyzw vf12, vf12
  vu.vf12.ftoi0(Mask::xyzw, vu.vf12);   vu.P = erleng(vu.vf11); /* TODO erleng */
// BRANCH!
  // ibeq vi07, vi03, L153      |  maddaw.xyzw ACC, vf26, vf11
  vu.acc.madda_xyzw(vu.vf26, vu.vf11.w());   bc = (vu.vi07 == vu.vi03);
  // mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
  vu.acc.madd_xyzw(vu.vf08, vu.vf27, vu.vf14.w());   vu.vf14.z() = 1.f;
  if (bc) { goto L153; }


  L77:
  // lqi.xyzw vf09, vi01        |  mulax.xyzw ACC, vf01, vf13
  vu.acc.mula_xyzw(vu.vf01, vu.vf13.x());   lq_buffer_xyzw<DEBUG>(vu.vf09, vu.vi01++);
  // sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13
  vu.acc.madda_xyzw(vu.vf02, vu.vf13.y());   sq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi11 + 1);
  // sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13
  vu.acc.madd_xyzw(vu.vf13, vu.vf03, vu.vf13.z());   sq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi14 + 1);
  // lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  vu.vf08.add_xyzw(vu.vf08, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi01++);
  // lqi.xyzw vf15, vi01        |  maxw.w vf10, vf10, vf02
  vu.vf10.max(Mask::w, vu.vf10, vu.vf02.w());   lq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi01++);
  // mtir vi11, vf09.x          |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   vu.vi11 = vu.vf09.x_as_u16();
  // mtir vi14, vf09.y          |  maxx.xyzw vf13, vf13, vf00
  vu.vf13.max_xyzw(vu.vf13, vu.vf00.x());   vu.vi14 = vu.vf09.y_as_u16();
  // div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  vu.vf09.add(Mask::zw, vu.vf09, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf08.w();

  // move.xyzw vf21, vf10       |  add.xyzw vf12, vf12, vf18
  vu.vf12.add_xyzw(vu.vf12, vu.vf18);   vu.vf21.move_xyzw(vu.vf10);
  // iand vi11, vi11, vi05      |  add.xyzw vf15, vf15, vf19
  vu.vf15.add_xyzw(vu.vf15, vu.vf19);   vu.vi11 = vu.vi11 & vu.vi05;
  // ilw.w vi08, -1(vi02)       |  mulax.xyzw ACC, vf04, vf13
  vu.acc.mula_xyzw(vu.vf04, vu.vf13.x());   ilw_buffer(Mask::w, vu.vi08, vu.vi02 + -1);
// BRANCH!
  // ibgtz vi09, L78            |  madday.xyzw ACC, vf05, vf13
  vu.acc.madda_xyzw(vu.vf05, vu.vf13.y());   bc = ((s16)vu.vi09) > 0;
  // iand vi14, vi14, vi05      |  maddaz.xyzw ACC, vf06, vf13
  vu.acc.madda_xyzw(vu.vf06, vu.vf13.z());   vu.vi14 = vu.vi14 & vu.vi05;
  if (bc) { goto L78; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L78:
// BRANCH!
  // ibne vi05, vi11, L79       |  maddw.xyzw vf13, vf07, vf00
  vu.acc.madd_xyzw(vu.vf13, vu.vf07, vu.vf00.w());   bc = (vu.vi05 != vu.vi11);
  // ilw.x vi09, -9(vi01)       |  mul.xyz vf08, vf08, Q
  vu.vf08.mul(Mask::xyz, vu.vf08, vu.Q); /* TODO mulq */   ilw_buffer(Mask::x, vu.vi09, vu.vi01 + -9);
  if (bc) { goto L79; }


  // mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  vu.vf14.mul_xyzw(vu.vf14, vu.Q); /* TODO mulq */   vu.vi11 = vu.vf12.x_as_u16();
  // mtir vi14, vf12.y          |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi14 = vu.vf12.y_as_u16();
// BRANCH!
  // b L80                      |  mul.xyzw vf13, vf13, vf23
  vu.vf13.mul_xyzw(vu.vf13, vu.vf23);   bc = true;
  // lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
  vu.vf08.add_xyzw(vu.vf08, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L80; }


  L79:
  // lq.xyzw vf20, 0(vi11)      |  mul.xyzw vf14, vf14, Q
  vu.vf14.mul_xyzw(vu.vf14, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11);
  // nop                        |  mulw.xyzw vf24, vf24, vf29
  vu.vf24.mul_xyzw(vu.vf24, vu.vf29.w());
  // lq.xyzw vf31, 0(vi14)      |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi14);
  // lq.xyzw vf25, 0(vi08)      |  mul.xyzw vf13, vf13, vf23
  vu.vf13.mul_xyzw(vu.vf13, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi08);
  // lq.xyzw vf23, 1(vi11)      |  add.xyzw vf08, vf08, vf22
  vu.vf08.add_xyzw(vu.vf08, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi11 + 1);
  // lq.xyzw vf20, 1(vi14)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi14 + 1);
  // lq.xyzw vf26, 1(vi08)      |  madday.xyzw ACC, vf31, vf24
  vu.acc.madda_xyzw(vu.vf31, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi08 + 1);
  // lq.xyzw vf31, 2(vi11)      |  maddz.xyzw vf25, vf25, vf24
  vu.acc.madd_xyzw(vu.vf25, vu.vf25, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi11 + 2);
  // lq.xyzw vf23, 2(vi14)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi14 + 2);
  // lq.xyzw vf27, 2(vi08)      |  madday.xyzw ACC, vf20, vf24
  vu.acc.madda_xyzw(vu.vf20, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi08 + 2);
  // lq.xyzw vf20, 3(vi11)      |  maddz.xyzw vf26, vf26, vf24
  vu.acc.madd_xyzw(vu.vf26, vu.vf26, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11 + 3);
  // lq.xyzw vf31, 3(vi14)      |  mulax.xyzw ACC, vf31, vf24
  vu.acc.mula_xyzw(vu.vf31, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi14 + 3);
  // lq.xyzw vf28, 3(vi08)      |  madday.xyzw ACC, vf23, vf24
  vu.acc.madda_xyzw(vu.vf23, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi08 + 3);
  // lq.xyzw vf23, 4(vi11)      |  maddz.xyzw vf27, vf27, vf24
  vu.acc.madd_xyzw(vu.vf27, vu.vf27, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi11 + 4);
  // lq.xyzw vf20, 4(vi14)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi14 + 4);
  // lq.xyz vf29, 4(vi08)       |  madday.xyzw ACC, vf31, vf24
  vu.acc.madda_xyzw(vu.vf31, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi08 + 4);
  // lq.xyzw vf31, 5(vi11)      |  maddz.xyzw vf28, vf28, vf24
  vu.acc.madd_xyzw(vu.vf28, vu.vf28, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi11 + 5);
  // lq.xyzw vf23, 5(vi14)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi14 + 5);
  // lq.xyz vf30, 5(vi08)       |  madday.xyzw ACC, vf20, vf24
  vu.acc.madda_xyzw(vu.vf20, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi08 + 5);
  // lq.xyzw vf20, 6(vi11)      |  maddz.xyz vf29, vf29, vf24
  vu.acc.madd_xyz(vu.vf29, vu.vf29, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11 + 6);
  // lq.xyzw vf22, 6(vi14)      |  mulax.xyzw ACC, vf31, vf24
  vu.acc.mula_xyzw(vu.vf31, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf22, vu.vi14 + 6);
  // lq.xyzw vf31, 6(vi08)      |  madday.xyzw ACC, vf23, vf24
  vu.acc.madda_xyzw(vu.vf23, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi08 + 6);
  // lqi.xyzw vf23, vi02        |  maddz.xyz vf30, vf30, vf24
  vu.acc.madd_xyz(vu.vf30, vu.vf30, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi02++);
  // mtir vi11, vf12.x          |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   vu.vi11 = vu.vf12.x_as_u16();
  // mtir vi14, vf12.y          |  madday.xyzw ACC, vf22, vf24
  vu.acc.madda_xyzw(vu.vf22, vu.vf24.y());   vu.vi14 = vu.vf12.y_as_u16();
  // lq.xyzw vf22, 2(vi00)      |  maddz.xyzw vf31, vf31, vf24
  vu.acc.madd_xyzw(vu.vf31, vu.vf31, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf22, 2);
  // lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
  vu.vf24.itof0(Mask::xyzw, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  L80:
// BRANCH!
  // ibgez vi09, L81            |  mulaz.xyzw ACC, vf29, vf09
  vu.acc.mula_xyzw(vu.vf29, vu.vf09.z());   bc = ((s16)vu.vi09) >= 0;
  // sq.xyzw vf21, 2(vi12)      |  maddaz.xyzw ACC, vf30, vf12
  vu.acc.madda_xyzw(vu.vf30, vu.vf12.z());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi12 + 2);
  if (bc) { goto L81; }


  // nop                        |  ftoi4.xyzw vf21, vf10
  vu.vf21.ftoi4(Mask::xyzw, vu.vf10);
  L81:
  // mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  vu.acc.madd_xyz(vu.vf12, vu.vf31, vu.vf15.z());   vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  vu.vf13.mini_xyzw(vu.vf13, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi12);
  // sq.xyzw vf16, 0(vi15)      |  miniw.w vf08, vf08, vf03
  vu.vf08.mini(Mask::w, vu.vf08, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi15);
  // sq.xyzw vf21, 2(vi15)      |  mulaw.xyzw ACC, vf25, vf09
  vu.acc.mula_xyzw(vu.vf25, vu.vf09.w());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi15 + 2);
  // ilw.y vi09, -6(vi01)       |  mulw.xyzw vf11, vf11, vf20
  vu.vf11.mul_xyzw(vu.vf11, vu.vf20.w());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  // erleng.xyz P, vf12         |  ftoi0.xyzw vf13, vf13
  vu.vf13.ftoi0(Mask::xyzw, vu.vf13);   vu.P = erleng(vu.vf12); /* TODO erleng */
// BRANCH!
  // ibne vi07, vi03, L66       |  maddaw.xyzw ACC, vf26, vf12
  vu.acc.madda_xyzw(vu.vf26, vu.vf12.w());   bc = (vu.vi07 != vu.vi03);
  // mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  vu.acc.madd_xyzw(vu.vf09, vu.vf27, vu.vf15.w());   vu.vf15.z() = 1.f;
  if (bc) { goto L66; }


// BRANCH!
  // b L163                     |  nop
  bc = true;
  // nop                        |  nop

  if (bc) { goto L163; }


  L82:
  // erleng.xyz P, vf12         |  maxi.xy vf08, vf08, I
  vu.vf08.maxi(Mask::xy, vu.vf08, vu.I);   vu.P = erleng(vu.vf12); /* TODO erleng */
  // 3072.0                     |  nop :i
  vu.I = 3072.0;
  // nop                        |  minii.xy vf08, vf08, I
  vu.vf08.minii(Mask::xy, vu.vf08, vu.I);
  // nop                        |  maddaw.xyzw ACC, vf26, vf12
  vu.acc.madda_xyzw(vu.vf26, vu.vf12.w());
  // mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  vu.acc.madd_xyzw(vu.vf09, vu.vf27, vu.vf15.w());   vu.vf15.z() = 1.f;
  // lqi.xyzw vf10, vi01        |  mulax.xyzw ACC, vf01, vf11
  vu.acc.mula_xyzw(vu.vf01, vu.vf11.x());   lq_buffer_xyzw<DEBUG>(vu.vf10, vu.vi01++);
// BRANCH!
  // ibne vi04, vi03, L84       |  madday.xyzw ACC, vf02, vf11
  vu.acc.madda_xyzw(vu.vf02, vu.vf11.y());   bc = (vu.vi04 != vu.vi03);
  // nop                        |  maddz.xyzw vf11, vf03, vf11
  vu.acc.madd_xyzw(vu.vf11, vu.vf03, vu.vf11.z());
  if (bc) { goto L84; }


// BRANCH!
  // ibne vi06, vi03, JUMP_48E  |  nop
  bc = (vu.vi06 != vu.vi03);
  // nop                        |  nop

  if (bc) { goto JUMP_48E; }


// BRANCH!
  // b L128                     |  nop
  bc = true;
  // nop                        |  nop

  if (bc) { goto L128; }


  L83:
  // 3072.0                     |  mulax.xyzw ACC, vf01, vf11 :i
  vu.acc.mula_xyzw(vu.vf01, vu.vf11.x());   vu.I = 3072.0;
  // lqi.xyzw vf10, vi01        |  minii.xy vf08, vf08, I
  vu.vf08.minii(Mask::xy, vu.vf08, vu.I);   lq_buffer_xyzw<DEBUG>(vu.vf10, vu.vi01++);
  // sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  vu.acc.madda_xyzw(vu.vf02, vu.vf11.y());   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi12 + 1);
  // sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
  vu.acc.madd_xyzw(vu.vf11, vu.vf03, vu.vf11.z());   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi15 + 1);
  L84:
  // lqi.xyzw vf13, vi01        |  add.xyzw vf09, vf09, vf28
  vu.vf09.add_xyzw(vu.vf09, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi01++);
  // lqi.xyzw vf16, vi01        |  maxw.w vf08, vf08, vf02
  vu.vf08.max(Mask::w, vu.vf08, vu.vf02.w());   lq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi01++);
  // mtir vi08, vf10.x          |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   vu.vi08 = vu.vf10.x_as_u16();
  // ilw.y vi09, -9(vi01)       |  maxx.xyzw vf11, vf11, vf00
  vu.vf11.max_xyzw(vu.vf11, vu.vf00.x());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -9);
  // nop                        |  miniw.w vf09, vf09, vf01
  vu.vf09.mini(Mask::w, vu.vf09, vu.vf01.w());
  // div Q, vf01.w, vf09.w      |  add.zw vf10, vf10, vf17
  vu.vf10.add(Mask::zw, vu.vf10, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf09.w();

  // move.xyzw vf21, vf08       |  add.xyzw vf13, vf13, vf18
  vu.vf13.add_xyzw(vu.vf13, vu.vf18);   vu.vf21.move_xyzw(vu.vf08);
  // lq.xyz vf29, 4(vi08)       |  add.xyzw vf16, vf16, vf19
  vu.vf16.add_xyzw(vu.vf16, vu.vf19);   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi08 + 4);
  // lq.xyz vf30, 5(vi08)       |  mulax.xyzw ACC, vf04, vf11
  vu.acc.mula_xyzw(vu.vf04, vu.vf11.x());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi08 + 5);
// BRANCH!
  // ibgtz vi09, L85            |  madday.xyzw ACC, vf05, vf11
  vu.acc.madda_xyzw(vu.vf05, vu.vf11.y());   bc = ((s16)vu.vi09) > 0;
  // lq.xyzw vf31, 6(vi08)      |  maddaz.xyzw ACC, vf06, vf11
  vu.acc.madda_xyzw(vu.vf06, vu.vf11.z());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi08 + 6);
  if (bc) { goto L85; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L85:
  // lq.xyzw vf25, 0(vi08)      |  maddw.xyzw vf11, vf07, vf00
  vu.acc.madd_xyzw(vu.vf11, vu.vf07, vu.vf00.w());   lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi08);
  // lq.xyzw vf26, 1(vi08)      |  mul.xyz vf09, vf09, Q
  vu.vf09.mul(Mask::xyz, vu.vf09, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi08 + 1);
  // mtir vi12, vf13.x          |  mul.xyzw vf15, vf15, Q
  vu.vf15.mul_xyzw(vu.vf15, vu.Q); /* TODO mulq */   vu.vi12 = vu.vf13.x_as_u16();
  // mtir vi15, vf13.y          |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi15 = vu.vf13.y_as_u16();
  // lq.xyzw vf27, 2(vi08)      |  mul.xyzw vf11, vf11, vf23
  vu.vf11.mul_xyzw(vu.vf11, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi08 + 2);
  // lqi.xyzw vf23, vi03        |  add.xyzw vf09, vf09, vf22
  vu.vf09.add_xyzw(vu.vf09, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
// BRANCH!
  // ibne vi00, vi09, L86       |  mulaz.xyzw ACC, vf29, vf10
  vu.acc.mula_xyzw(vu.vf29, vu.vf10.z());   bc = (vu.vi09 != 0);
  // sq.xyzw vf21, 2(vi10)      |  maddaz.xyzw ACC, vf30, vf13
  vu.acc.madda_xyzw(vu.vf30, vu.vf13.z());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi10 + 2);
  if (bc) { goto L86; }


  // nop                        |  ftoi4.xyzw vf21, vf08
  vu.vf21.ftoi4(Mask::xyzw, vu.vf08);
  L86:
  // mfp.w vf20, P              |  maddz.xyz vf13, vf31, vf16
  vu.acc.madd_xyz(vu.vf13, vu.vf31, vu.vf16.z());   vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  vu.vf11.mini_xyzw(vu.vf11, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi10);
  // sq.xyzw vf14, 0(vi13)      |  miniw.w vf09, vf09, vf03
  vu.vf09.mini(Mask::w, vu.vf09, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi13);
  // sq.xyzw vf21, 2(vi13)      |  mulaw.xyzw ACC, vf25, vf10
  vu.acc.mula_xyzw(vu.vf25, vu.vf10.w());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi13 + 2);
  // lq.xyzw vf28, 3(vi08)      |  mulw.xyzw vf12, vf12, vf20
  vu.vf12.mul_xyzw(vu.vf12, vu.vf20.w());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi08 + 3);
  // 1024.0                     |  ftoi0.xyzw vf11, vf11 :i
  vu.vf11.ftoi0(Mask::xyzw, vu.vf11);   vu.I = 1024.0;
  // erleng.xyz P, vf13         |  maxi.xy vf09, vf09, I
  vu.vf09.maxi(Mask::xy, vu.vf09, vu.I);   vu.P = erleng(vu.vf13); /* TODO erleng */
// BRANCH!
  // ibne vi04, vi03, L87       |  maddaw.xyzw ACC, vf26, vf13
  vu.acc.madda_xyzw(vu.vf26, vu.vf13.w());   bc = (vu.vi04 != vu.vi03);
  // mr32.z vf16, vf00          |  maddw.xyzw vf10, vf27, vf16
  vu.acc.madd_xyzw(vu.vf10, vu.vf27, vu.vf16.w());   vu.vf16.z() = 1.f;
  if (bc) { goto L87; }


// BRANCH!
  // ibne vi06, vi03, L100      |  nop
  bc = (vu.vi06 != vu.vi03);
  // ilw.y vi09, -6(vi01)       |  nop
  ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  if (bc) { goto L100; }


// BRANCH!
  // ibne vi07, vi03, L133      |  nop
  bc = (vu.vi07 != vu.vi03);
  // nop                        |  nop

  if (bc) { goto L133; }


// BRANCH!
  // b L143                     |  nop
  bc = true;
  // nop                        |  nop

  if (bc) { goto L143; }


  L87:
  // 3072.0                     |  mulax.xyzw ACC, vf01, vf12 :i
  vu.acc.mula_xyzw(vu.vf01, vu.vf12.x());   vu.I = 3072.0;
  // lqi.xyzw vf08, vi01        |  minii.xy vf09, vf09, I
  vu.vf09.minii(Mask::xy, vu.vf09, vu.I);   lq_buffer_xyzw<DEBUG>(vu.vf08, vu.vi01++);
  // sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12
  vu.acc.madda_xyzw(vu.vf02, vu.vf12.y());   sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi10 + 1);
  // sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12
  vu.acc.madd_xyzw(vu.vf12, vu.vf03, vu.vf12.z());   sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi13 + 1);
  // lqi.xyzw vf11, vi01        |  add.xyzw vf10, vf10, vf28
  vu.vf10.add_xyzw(vu.vf10, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi01++);
  // lqi.xyzw vf14, vi01        |  maxw.w vf09, vf09, vf02
  vu.vf09.max(Mask::w, vu.vf09, vu.vf02.w());   lq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi01++);
  // mtir vi08, vf08.x          |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   vu.vi08 = vu.vf08.x_as_u16();
  // ilw.y vi09, -9(vi01)       |  maxx.xyzw vf12, vf12, vf00
  vu.vf12.max_xyzw(vu.vf12, vu.vf00.x());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -9);
  // nop                        |  miniw.w vf10, vf10, vf01
  vu.vf10.mini(Mask::w, vu.vf10, vu.vf01.w());
  // div Q, vf01.w, vf10.w      |  add.zw vf08, vf08, vf17
  vu.vf08.add(Mask::zw, vu.vf08, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf10.w();

  // move.xyzw vf21, vf09       |  add.xyzw vf11, vf11, vf18
  vu.vf11.add_xyzw(vu.vf11, vu.vf18);   vu.vf21.move_xyzw(vu.vf09);
  // lq.xyz vf29, 4(vi08)       |  add.xyzw vf14, vf14, vf19
  vu.vf14.add_xyzw(vu.vf14, vu.vf19);   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi08 + 4);
  // lq.xyz vf30, 5(vi08)       |  mulax.xyzw ACC, vf04, vf12
  vu.acc.mula_xyzw(vu.vf04, vu.vf12.x());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi08 + 5);
// BRANCH!
  // ibgtz vi09, L88            |  madday.xyzw ACC, vf05, vf12
  vu.acc.madda_xyzw(vu.vf05, vu.vf12.y());   bc = ((s16)vu.vi09) > 0;
  // lq.xyzw vf31, 6(vi08)      |  maddaz.xyzw ACC, vf06, vf12
  vu.acc.madda_xyzw(vu.vf06, vu.vf12.z());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi08 + 6);
  if (bc) { goto L88; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L88:
  // lq.xyzw vf25, 0(vi08)      |  maddw.xyzw vf12, vf07, vf00
  vu.acc.madd_xyzw(vu.vf12, vu.vf07, vu.vf00.w());   lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi08);
  // lq.xyzw vf26, 1(vi08)      |  mul.xyz vf10, vf10, Q
  vu.vf10.mul(Mask::xyz, vu.vf10, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi08 + 1);
  // mtir vi10, vf11.x          |  mul.xyzw vf16, vf16, Q
  vu.vf16.mul_xyzw(vu.vf16, vu.Q); /* TODO mulq */   vu.vi10 = vu.vf11.x_as_u16();
  // mtir vi13, vf11.y          |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi13 = vu.vf11.y_as_u16();
  // lq.xyzw vf27, 2(vi08)      |  mul.xyzw vf12, vf12, vf23
  vu.vf12.mul_xyzw(vu.vf12, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi08 + 2);
  // lqi.xyzw vf23, vi03        |  add.xyzw vf10, vf10, vf22
  vu.vf10.add_xyzw(vu.vf10, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
// BRANCH!
  // ibne vi00, vi09, L89       |  mulaz.xyzw ACC, vf29, vf08
  vu.acc.mula_xyzw(vu.vf29, vu.vf08.z());   bc = (vu.vi09 != 0);
  // sq.xyzw vf21, 2(vi11)      |  maddaz.xyzw ACC, vf30, vf11
  vu.acc.madda_xyzw(vu.vf30, vu.vf11.z());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi11 + 2);
  if (bc) { goto L89; }


  // nop                        |  ftoi4.xyzw vf21, vf09
  vu.vf21.ftoi4(Mask::xyzw, vu.vf09);
  L89:
  // mfp.w vf20, P              |  maddz.xyz vf11, vf31, vf14
  vu.acc.madd_xyz(vu.vf11, vu.vf31, vu.vf14.z());   vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17
  vu.vf12.mini_xyzw(vu.vf12, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi11);
  // sq.xyzw vf15, 0(vi14)      |  miniw.w vf10, vf10, vf03
  vu.vf10.mini(Mask::w, vu.vf10, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi14);
  // sq.xyzw vf21, 2(vi14)      |  mulaw.xyzw ACC, vf25, vf08
  vu.acc.mula_xyzw(vu.vf25, vu.vf08.w());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi14 + 2);
  // lq.xyzw vf28, 3(vi08)      |  mulw.xyzw vf13, vf13, vf20
  vu.vf13.mul_xyzw(vu.vf13, vu.vf20.w());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi08 + 3);
  // 1024.0                     |  ftoi0.xyzw vf12, vf12 :i
  vu.vf12.ftoi0(Mask::xyzw, vu.vf12);   vu.I = 1024.0;
  // erleng.xyz P, vf11         |  maxi.xy vf10, vf10, I
  vu.vf10.maxi(Mask::xy, vu.vf10, vu.I);   vu.P = erleng(vu.vf11); /* TODO erleng */
// BRANCH!
  // ibne vi04, vi03, L90       |  maddaw.xyzw ACC, vf26, vf11
  vu.acc.madda_xyzw(vu.vf26, vu.vf11.w());   bc = (vu.vi04 != vu.vi03);
  // mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
  vu.acc.madd_xyzw(vu.vf08, vu.vf27, vu.vf14.w());   vu.vf14.z() = 1.f;
  if (bc) { goto L90; }


// BRANCH!
  // ibne vi06, vi03, L105      |  nop
  bc = (vu.vi06 != vu.vi03);
  // ilw.y vi09, -6(vi01)       |  nop
  ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  if (bc) { goto L105; }


// BRANCH!
  // ibne vi07, vi03, L138      |  nop
  bc = (vu.vi07 != vu.vi03);
  // nop                        |  nop

  if (bc) { goto L138; }


// BRANCH!
  // b L153                     |  nop
  bc = true;
  // nop                        |  nop

  if (bc) { goto L153; }


  L90:
  // 3072.0                     |  mulax.xyzw ACC, vf01, vf13 :i
  vu.acc.mula_xyzw(vu.vf01, vu.vf13.x());   vu.I = 3072.0;
  // lqi.xyzw vf09, vi01        |  minii.xy vf10, vf10, I
  vu.vf10.minii(Mask::xy, vu.vf10, vu.I);   lq_buffer_xyzw<DEBUG>(vu.vf09, vu.vi01++);
  // sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13
  vu.acc.madda_xyzw(vu.vf02, vu.vf13.y());   sq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi11 + 1);
  // sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13
  vu.acc.madd_xyzw(vu.vf13, vu.vf03, vu.vf13.z());   sq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi14 + 1);
  // lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  vu.vf08.add_xyzw(vu.vf08, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi01++);
  // lqi.xyzw vf15, vi01        |  maxw.w vf10, vf10, vf02
  vu.vf10.max(Mask::w, vu.vf10, vu.vf02.w());   lq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi01++);
  // mtir vi08, vf09.x          |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   vu.vi08 = vu.vf09.x_as_u16();
  // ilw.y vi09, -9(vi01)       |  maxx.xyzw vf13, vf13, vf00
  vu.vf13.max_xyzw(vu.vf13, vu.vf00.x());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -9);
  // nop                        |  miniw.w vf08, vf08, vf01
  vu.vf08.mini(Mask::w, vu.vf08, vu.vf01.w());
  // div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  vu.vf09.add(Mask::zw, vu.vf09, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf08.w();

  // move.xyzw vf21, vf10       |  add.xyzw vf12, vf12, vf18
  vu.vf12.add_xyzw(vu.vf12, vu.vf18);   vu.vf21.move_xyzw(vu.vf10);
  // lq.xyz vf29, 4(vi08)       |  add.xyzw vf15, vf15, vf19
  vu.vf15.add_xyzw(vu.vf15, vu.vf19);   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi08 + 4);
  // lq.xyz vf30, 5(vi08)       |  mulax.xyzw ACC, vf04, vf13
  vu.acc.mula_xyzw(vu.vf04, vu.vf13.x());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi08 + 5);
// BRANCH!
  // ibgtz vi09, L91            |  madday.xyzw ACC, vf05, vf13
  vu.acc.madda_xyzw(vu.vf05, vu.vf13.y());   bc = ((s16)vu.vi09) > 0;
  // lq.xyzw vf31, 6(vi08)      |  maddaz.xyzw ACC, vf06, vf13
  vu.acc.madda_xyzw(vu.vf06, vu.vf13.z());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi08 + 6);
  if (bc) { goto L91; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L91:
  // lq.xyzw vf25, 0(vi08)      |  maddw.xyzw vf13, vf07, vf00
  vu.acc.madd_xyzw(vu.vf13, vu.vf07, vu.vf00.w());   lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi08);
  // lq.xyzw vf26, 1(vi08)      |  mul.xyz vf08, vf08, Q
  vu.vf08.mul(Mask::xyz, vu.vf08, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi08 + 1);
  // mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  vu.vf14.mul_xyzw(vu.vf14, vu.Q); /* TODO mulq */   vu.vi11 = vu.vf12.x_as_u16();
  // mtir vi14, vf12.y          |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi14 = vu.vf12.y_as_u16();
  // lq.xyzw vf27, 2(vi08)      |  mul.xyzw vf13, vf13, vf23
  vu.vf13.mul_xyzw(vu.vf13, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi08 + 2);
  // lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
  vu.vf08.add_xyzw(vu.vf08, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
// BRANCH!
  // ibne vi00, vi09, L92       |  mulaz.xyzw ACC, vf29, vf09
  vu.acc.mula_xyzw(vu.vf29, vu.vf09.z());   bc = (vu.vi09 != 0);
  // sq.xyzw vf21, 2(vi12)      |  maddaz.xyzw ACC, vf30, vf12
  vu.acc.madda_xyzw(vu.vf30, vu.vf12.z());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi12 + 2);
  if (bc) { goto L92; }


  // nop                        |  ftoi4.xyzw vf21, vf10
  vu.vf21.ftoi4(Mask::xyzw, vu.vf10);
  L92:
  // mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  vu.acc.madd_xyz(vu.vf12, vu.vf31, vu.vf15.z());   vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  vu.vf13.mini_xyzw(vu.vf13, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi12);
  // sq.xyzw vf16, 0(vi15)      |  miniw.w vf08, vf08, vf03
  vu.vf08.mini(Mask::w, vu.vf08, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi15);
  // sq.xyzw vf21, 2(vi15)      |  mulaw.xyzw ACC, vf25, vf09
  vu.acc.mula_xyzw(vu.vf25, vu.vf09.w());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi15 + 2);
  // lq.xyzw vf28, 3(vi08)      |  mulw.xyzw vf11, vf11, vf20
  vu.vf11.mul_xyzw(vu.vf11, vu.vf20.w());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi08 + 3);
  // 1024.0                     |  ftoi0.xyzw vf13, vf13 :i
  vu.vf13.ftoi0(Mask::xyzw, vu.vf13);   vu.I = 1024.0;
  // erleng.xyz P, vf12         |  maxi.xy vf08, vf08, I
  vu.vf08.maxi(Mask::xy, vu.vf08, vu.I);   vu.P = erleng(vu.vf12); /* TODO erleng */
// BRANCH!
  // ibne vi04, vi03, L83       |  maddaw.xyzw ACC, vf26, vf12
  vu.acc.madda_xyzw(vu.vf26, vu.vf12.w());   bc = (vu.vi04 != vu.vi03);
  // mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  vu.acc.madd_xyzw(vu.vf09, vu.vf27, vu.vf15.w());   vu.vf15.z() = 1.f;
  if (bc) { goto L83; }


// BRANCH!
  // ibne vi06, vi03, L94       |  nop
  bc = (vu.vi06 != vu.vi03);
  // ilw.y vi09, -6(vi01)       |  nop
  ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  if (bc) { goto L94; }


// BRANCH!
  // ibne vi07, vi03, L127      |  nop
  bc = (vu.vi07 != vu.vi03);
  // nop                        |  nop

  if (bc) { goto L127; }


// BRANCH!
  // b L163                     |  nop
  bc = true;
  // nop                        |  nop

  if (bc) { goto L163; }


  L93:
  // erleng.xyz P, vf12         |  maxi.xy vf08, vf08, I
  vu.vf08.maxi(Mask::xy, vu.vf08, vu.I);   vu.P = erleng(vu.vf12); /* TODO erleng */
  // 3072.0                     |  nop :i
  vu.I = 3072.0;
  // nop                        |  minii.xy vf08, vf08, I
  vu.vf08.minii(Mask::xy, vu.vf08, vu.I);
// BRANCH!
  // ibeq vi06, vi03, L126      |  maddaw.xyzw ACC, vf26, vf12
  vu.acc.madda_xyzw(vu.vf26, vu.vf12.w());   bc = (vu.vi06 == vu.vi03);
  // mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  vu.acc.madd_xyzw(vu.vf09, vu.vf27, vu.vf15.w());   vu.vf15.z() = 1.f;
  if (bc) { goto L126; }


  // lqi.xyzw vf10, vi01        |  mulax.xyzw ACC, vf01, vf11
  vu.acc.mula_xyzw(vu.vf01, vu.vf11.x());   lq_buffer_xyzw<DEBUG>(vu.vf10, vu.vi01++);
  // jr vi08                    |  madday.xyzw ACC, vf02, vf11
  vu.acc.madda_xyzw(vu.vf02, vu.vf11.y());
  // nop                        |  maddz.xyzw vf11, vf03, vf11
  vu.acc.madd_xyzw(vu.vf11, vu.vf03, vu.vf11.z());
  switch (vu.vi08) {
  case 0x1a1:
  goto JUMP_1A1;
  case 0x48e:
  goto JUMP_48E;
  case 0x243:
  goto JUMP_243;
  case 0x539:
    goto JUMP_539;
  default:
    ASSERT_MSG(false, fmt::format("bad jump to {:x}", vu.vi08));
  }
  L94:
  // 3072.0                     |  mulax.xyzw ACC, vf01, vf11 :i
  vu.acc.mula_xyzw(vu.vf01, vu.vf11.x());   vu.I = 3072.0;
  // lqi.xyzw vf10, vi01        |  minii.xy vf08, vf08, I
  vu.vf08.minii(Mask::xy, vu.vf08, vu.I);   lq_buffer_xyzw<DEBUG>(vu.vf10, vu.vi01++);
  // sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  vu.acc.madda_xyzw(vu.vf02, vu.vf11.y());   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi12 + 1);
  // sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
  vu.acc.madd_xyzw(vu.vf11, vu.vf03, vu.vf11.z());   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi15 + 1);
  JUMP_48E:
  // lqi.xyzw vf13, vi01        |  add.xyzw vf09, vf09, vf28
  vu.vf09.add_xyzw(vu.vf09, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi01++);
  // lqi.xyzw vf16, vi01        |  maxw.w vf08, vf08, vf02
  vu.vf08.max(Mask::w, vu.vf08, vu.vf02.w());   lq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi01++);
  // mtir vi12, vf10.x          |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   vu.vi12 = vu.vf10.x_as_u16();
  // mtir vi15, vf10.y          |  maxx.xyzw vf11, vf11, vf00
  vu.vf11.max_xyzw(vu.vf11, vu.vf00.x());   vu.vi15 = vu.vf10.y_as_u16();
  // nop                        |  miniw.w vf09, vf09, vf01
  vu.vf09.mini(Mask::w, vu.vf09, vu.vf01.w());
  // div Q, vf01.w, vf09.w      |  add.zw vf10, vf10, vf17
  vu.vf10.add(Mask::zw, vu.vf10, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf09.w();

  // move.xyzw vf21, vf08       |  add.xyzw vf13, vf13, vf18
  vu.vf13.add_xyzw(vu.vf13, vu.vf18);   vu.vf21.move_xyzw(vu.vf08);
  // iand vi12, vi12, vi05      |  add.xyzw vf16, vf16, vf19
  vu.vf16.add_xyzw(vu.vf16, vu.vf19);   vu.vi12 = vu.vi12 & vu.vi05;
  // nop                        |  mulax.xyzw ACC, vf04, vf11
  vu.acc.mula_xyzw(vu.vf04, vu.vf11.x());
// BRANCH!
  // ibgtz vi09, L96            |  madday.xyzw ACC, vf05, vf11
  vu.acc.madda_xyzw(vu.vf05, vu.vf11.y());   bc = ((s16)vu.vi09) > 0;
  // iand vi15, vi15, vi05      |  maddaz.xyzw ACC, vf06, vf11
  vu.acc.madda_xyzw(vu.vf06, vu.vf11.z());   vu.vi15 = vu.vi15 & vu.vi05;
  if (bc) { goto L96; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L96:
// BRANCH!
  // ibne vi05, vi12, L97       |  maddw.xyzw vf11, vf07, vf00
  vu.acc.madd_xyzw(vu.vf11, vu.vf07, vu.vf00.w());   bc = (vu.vi05 != vu.vi12);
  // ilw.x vi09, -9(vi01)       |  mul.xyz vf09, vf09, Q
  vu.vf09.mul(Mask::xyz, vu.vf09, vu.Q); /* TODO mulq */   ilw_buffer(Mask::x, vu.vi09, vu.vi01 + -9);
  if (bc) { goto L97; }


  // mtir vi12, vf13.x          |  mul.xyzw vf15, vf15, Q
  vu.vf15.mul_xyzw(vu.vf15, vu.Q); /* TODO mulq */   vu.vi12 = vu.vf13.x_as_u16();
  // mtir vi15, vf13.y          |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi15 = vu.vf13.y_as_u16();
// BRANCH!
  // b L98                      |  mul.xyzw vf11, vf11, vf23
  vu.vf11.mul_xyzw(vu.vf11, vu.vf23);   bc = true;
  // lqi.xyzw vf23, vi03        |  add.xyzw vf09, vf09, vf22
  vu.vf09.add_xyzw(vu.vf09, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L98; }


  L97:
  // lq.xyzw vf20, 0(vi12)      |  mul.xyzw vf15, vf15, Q
  vu.vf15.mul_xyzw(vu.vf15, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi12);
  // nop                        |  mulw.xyzw vf24, vf24, vf29
  vu.vf24.mul_xyzw(vu.vf24, vu.vf29.w());
  // lq.xyzw vf25, 0(vi15)      |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi15);
  // lq.xyzw vf23, 1(vi12)      |  mul.xyzw vf11, vf11, vf23
  vu.vf11.mul_xyzw(vu.vf11, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi12 + 1);
  // lq.xyzw vf26, 1(vi15)      |  add.xyzw vf09, vf09, vf22
  vu.vf09.add_xyzw(vu.vf09, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi15 + 1);
  // lq.xyzw vf20, 2(vi12)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi12 + 2);
  // lq.xyzw vf27, 2(vi15)      |  maddy.xyzw vf25, vf25, vf24
  vu.acc.madd_xyzw(vu.vf25, vu.vf25, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi15 + 2);
  // lq.xyzw vf23, 3(vi12)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi12 + 3);
  // lq.xyzw vf28, 3(vi15)      |  maddy.xyzw vf26, vf26, vf24
  vu.acc.madd_xyzw(vu.vf26, vu.vf26, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi15 + 3);
  // lq.xyzw vf20, 4(vi12)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi12 + 4);
  // lq.xyz vf29, 4(vi15)       |  maddy.xyzw vf27, vf27, vf24
  vu.acc.madd_xyzw(vu.vf27, vu.vf27, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi15 + 4);
  // lq.xyzw vf23, 5(vi12)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi12 + 5);
  // lq.xyz vf30, 5(vi15)       |  maddy.xyzw vf28, vf28, vf24
  vu.acc.madd_xyzw(vu.vf28, vu.vf28, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi15 + 5);
  // lq.xyzw vf20, 6(vi12)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi12 + 6);
  // lq.xyzw vf31, 6(vi15)      |  maddy.xyz vf29, vf29, vf24
  vu.acc.madd_xyz(vu.vf29, vu.vf29, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi15 + 6);
  // mtir vi12, vf13.x          |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   vu.vi12 = vu.vf13.x_as_u16();
  // mtir vi15, vf13.y          |  maddy.xyz vf30, vf30, vf24
  vu.acc.madd_xyz(vu.vf30, vu.vf30, vu.vf24.y());   vu.vi15 = vu.vf13.y_as_u16();
// BRANCH!
  // b L113                     |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   bc = true;
  // lqi.xyzw vf23, vi03        |  maddy.xyzw vf31, vf31, vf24
  vu.acc.madd_xyzw(vu.vf31, vu.vf31, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L113; }


  L98:
// BRANCH!
  // ibgez vi09, L99            |  mulaz.xyzw ACC, vf29, vf10
  vu.acc.mula_xyzw(vu.vf29, vu.vf10.z());   bc = ((s16)vu.vi09) >= 0;
  // sq.xyzw vf21, 2(vi10)      |  maddaz.xyzw ACC, vf30, vf13
  vu.acc.madda_xyzw(vu.vf30, vu.vf13.z());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi10 + 2);
  if (bc) { goto L99; }


  // nop                        |  ftoi4.xyzw vf21, vf08
  vu.vf21.ftoi4(Mask::xyzw, vu.vf08);
  L99:
  // mfp.w vf20, P              |  maddz.xyz vf13, vf31, vf16
  vu.acc.madd_xyz(vu.vf13, vu.vf31, vu.vf16.z());   vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  vu.vf11.mini_xyzw(vu.vf11, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi10);
  // sq.xyzw vf14, 0(vi13)      |  miniw.w vf09, vf09, vf03
  vu.vf09.mini(Mask::w, vu.vf09, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi13);
  // sq.xyzw vf21, 2(vi13)      |  mulaw.xyzw ACC, vf25, vf10
  vu.acc.mula_xyzw(vu.vf25, vu.vf10.w());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi13 + 2);
  // ilw.y vi09, -6(vi01)       |  mulw.xyzw vf12, vf12, vf20
  vu.vf12.mul_xyzw(vu.vf12, vu.vf20.w());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  // 1024.0                     |  ftoi0.xyzw vf11, vf11 :i
  vu.vf11.ftoi0(Mask::xyzw, vu.vf11);   vu.I = 1024.0;
  // erleng.xyz P, vf13         |  maxi.xy vf09, vf09, I
  vu.vf09.maxi(Mask::xy, vu.vf09, vu.I);   vu.P = erleng(vu.vf13); /* TODO erleng */
// BRANCH!
  // ibne vi06, vi03, L100      |  maddaw.xyzw ACC, vf26, vf13
  vu.acc.madda_xyzw(vu.vf26, vu.vf13.w());   bc = (vu.vi06 != vu.vi03);
  // mr32.z vf16, vf00          |  maddw.xyzw vf10, vf27, vf16
  vu.acc.madd_xyzw(vu.vf10, vu.vf27, vu.vf16.w());   vu.vf16.z() = 1.f;
  if (bc) { goto L100; }


// BRANCH!
  // ibne vi07, vi03, L133      |  nop
  bc = (vu.vi07 != vu.vi03);
  // nop                        |  nop

  if (bc) { goto L133; }


// BRANCH!
  // b L143                     |  nop
  bc = true;
  // nop                        |  nop

  if (bc) { goto L143; }


  L100:
  // 3072.0                     |  mulax.xyzw ACC, vf01, vf12 :i
  vu.acc.mula_xyzw(vu.vf01, vu.vf12.x());   vu.I = 3072.0;
  // lqi.xyzw vf08, vi01        |  minii.xy vf09, vf09, I
  vu.vf09.minii(Mask::xy, vu.vf09, vu.I);   lq_buffer_xyzw<DEBUG>(vu.vf08, vu.vi01++);
  // sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12
  vu.acc.madda_xyzw(vu.vf02, vu.vf12.y());   sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi10 + 1);
  // sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12
  vu.acc.madd_xyzw(vu.vf12, vu.vf03, vu.vf12.z());   sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi13 + 1);
  // lqi.xyzw vf11, vi01        |  add.xyzw vf10, vf10, vf28
  vu.vf10.add_xyzw(vu.vf10, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi01++);
  // lqi.xyzw vf14, vi01        |  maxw.w vf09, vf09, vf02
  vu.vf09.max(Mask::w, vu.vf09, vu.vf02.w());   lq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi01++);
  // mtir vi10, vf08.x          |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   vu.vi10 = vu.vf08.x_as_u16();
  // mtir vi13, vf08.y          |  maxx.xyzw vf12, vf12, vf00
  vu.vf12.max_xyzw(vu.vf12, vu.vf00.x());   vu.vi13 = vu.vf08.y_as_u16();
  // nop                        |  miniw.w vf10, vf10, vf01
  vu.vf10.mini(Mask::w, vu.vf10, vu.vf01.w());
  // div Q, vf01.w, vf10.w      |  add.zw vf08, vf08, vf17
  vu.vf08.add(Mask::zw, vu.vf08, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf10.w();

  // move.xyzw vf21, vf09       |  add.xyzw vf11, vf11, vf18
  vu.vf11.add_xyzw(vu.vf11, vu.vf18);   vu.vf21.move_xyzw(vu.vf09);
  // iand vi10, vi10, vi05      |  add.xyzw vf14, vf14, vf19
  vu.vf14.add_xyzw(vu.vf14, vu.vf19);   vu.vi10 = vu.vi10 & vu.vi05;
  // nop                        |  mulax.xyzw ACC, vf04, vf12
  vu.acc.mula_xyzw(vu.vf04, vu.vf12.x());
// BRANCH!
  // ibgtz vi09, L101           |  madday.xyzw ACC, vf05, vf12
  vu.acc.madda_xyzw(vu.vf05, vu.vf12.y());   bc = ((s16)vu.vi09) > 0;
  // iand vi13, vi13, vi05      |  maddaz.xyzw ACC, vf06, vf12
  vu.acc.madda_xyzw(vu.vf06, vu.vf12.z());   vu.vi13 = vu.vi13 & vu.vi05;
  if (bc) { goto L101; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L101:
// BRANCH!
  // ibne vi05, vi10, L102      |  maddw.xyzw vf12, vf07, vf00
  vu.acc.madd_xyzw(vu.vf12, vu.vf07, vu.vf00.w());   bc = (vu.vi05 != vu.vi10);
  // ilw.x vi09, -9(vi01)       |  mul.xyz vf10, vf10, Q
  vu.vf10.mul(Mask::xyz, vu.vf10, vu.Q); /* TODO mulq */   ilw_buffer(Mask::x, vu.vi09, vu.vi01 + -9);
  if (bc) { goto L102; }


  // mtir vi10, vf11.x          |  mul.xyzw vf16, vf16, Q
  vu.vf16.mul_xyzw(vu.vf16, vu.Q); /* TODO mulq */   vu.vi10 = vu.vf11.x_as_u16();
  // mtir vi13, vf11.y          |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi13 = vu.vf11.y_as_u16();
// BRANCH!
  // b L103                     |  mul.xyzw vf12, vf12, vf23
  vu.vf12.mul_xyzw(vu.vf12, vu.vf23);   bc = true;
  // lqi.xyzw vf23, vi03        |  add.xyzw vf10, vf10, vf22
  vu.vf10.add_xyzw(vu.vf10, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L103; }


  L102:
  // lq.xyzw vf20, 0(vi10)      |  mul.xyzw vf16, vf16, Q
  vu.vf16.mul_xyzw(vu.vf16, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10);
  // nop                        |  mulw.xyzw vf24, vf24, vf29
  vu.vf24.mul_xyzw(vu.vf24, vu.vf29.w());
  // lq.xyzw vf25, 0(vi13)      |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi13);
  // lq.xyzw vf23, 1(vi10)      |  mul.xyzw vf12, vf12, vf23
  vu.vf12.mul_xyzw(vu.vf12, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi10 + 1);
  // lq.xyzw vf26, 1(vi13)      |  add.xyzw vf10, vf10, vf22
  vu.vf10.add_xyzw(vu.vf10, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi13 + 1);
  // lq.xyzw vf20, 2(vi10)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10 + 2);
  // lq.xyzw vf27, 2(vi13)      |  maddy.xyzw vf25, vf25, vf24
  vu.acc.madd_xyzw(vu.vf25, vu.vf25, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi13 + 2);
  // lq.xyzw vf23, 3(vi10)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi10 + 3);
  // lq.xyzw vf28, 3(vi13)      |  maddy.xyzw vf26, vf26, vf24
  vu.acc.madd_xyzw(vu.vf26, vu.vf26, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi13 + 3);
  // lq.xyzw vf20, 4(vi10)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10 + 4);
  // lq.xyz vf29, 4(vi13)       |  maddy.xyzw vf27, vf27, vf24
  vu.acc.madd_xyzw(vu.vf27, vu.vf27, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi13 + 4);
  // lq.xyzw vf23, 5(vi10)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi10 + 5);
  // lq.xyz vf30, 5(vi13)       |  maddy.xyzw vf28, vf28, vf24
  vu.acc.madd_xyzw(vu.vf28, vu.vf28, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi13 + 5);
  // lq.xyzw vf20, 6(vi10)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10 + 6);
  // lq.xyzw vf31, 6(vi13)      |  maddy.xyz vf29, vf29, vf24
  vu.acc.madd_xyz(vu.vf29, vu.vf29, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi13 + 6);
  // mtir vi10, vf11.x          |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   vu.vi10 = vu.vf11.x_as_u16();
  // mtir vi13, vf11.y          |  maddy.xyz vf30, vf30, vf24
  vu.acc.madd_xyz(vu.vf30, vu.vf30, vu.vf24.y());   vu.vi13 = vu.vf11.y_as_u16();
// BRANCH!
  // b L118                     |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   bc = true;
  // lqi.xyzw vf23, vi03        |  maddy.xyzw vf31, vf31, vf24
  vu.acc.madd_xyzw(vu.vf31, vu.vf31, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L118; }


  L103:
// BRANCH!
  // ibgez vi09, L104           |  mulaz.xyzw ACC, vf29, vf08
  vu.acc.mula_xyzw(vu.vf29, vu.vf08.z());   bc = ((s16)vu.vi09) >= 0;
  // sq.xyzw vf21, 2(vi11)      |  maddaz.xyzw ACC, vf30, vf11
  vu.acc.madda_xyzw(vu.vf30, vu.vf11.z());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi11 + 2);
  if (bc) { goto L104; }


  // nop                        |  ftoi4.xyzw vf21, vf09
  vu.vf21.ftoi4(Mask::xyzw, vu.vf09);
  L104:
  // mfp.w vf20, P              |  maddz.xyz vf11, vf31, vf14
  vu.acc.madd_xyz(vu.vf11, vu.vf31, vu.vf14.z());   vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17
  vu.vf12.mini_xyzw(vu.vf12, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi11);
  // sq.xyzw vf15, 0(vi14)      |  miniw.w vf10, vf10, vf03
  vu.vf10.mini(Mask::w, vu.vf10, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi14);
  // sq.xyzw vf21, 2(vi14)      |  mulaw.xyzw ACC, vf25, vf08
  vu.acc.mula_xyzw(vu.vf25, vu.vf08.w());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi14 + 2);
  // ilw.y vi09, -6(vi01)       |  mulw.xyzw vf13, vf13, vf20
  vu.vf13.mul_xyzw(vu.vf13, vu.vf20.w());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  // 1024.0                     |  ftoi0.xyzw vf12, vf12 :i
  vu.vf12.ftoi0(Mask::xyzw, vu.vf12);   vu.I = 1024.0;
  // erleng.xyz P, vf11         |  maxi.xy vf10, vf10, I
  vu.vf10.maxi(Mask::xy, vu.vf10, vu.I);   vu.P = erleng(vu.vf11); /* TODO erleng */
// BRANCH!
  // ibne vi06, vi03, L105      |  maddaw.xyzw ACC, vf26, vf11
  vu.acc.madda_xyzw(vu.vf26, vu.vf11.w());   bc = (vu.vi06 != vu.vi03);
  // mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
  vu.acc.madd_xyzw(vu.vf08, vu.vf27, vu.vf14.w());   vu.vf14.z() = 1.f;
  if (bc) { goto L105; }


// BRANCH!
  // ibne vi07, vi03, L138      |  nop
  bc = (vu.vi07 != vu.vi03);
  // nop                        |  nop

  if (bc) { goto L138; }


// BRANCH!
  // b L153                     |  nop
  bc = true;
  // nop                        |  nop

  if (bc) { goto L153; }


  L105:
  // 3072.0                     |  mulax.xyzw ACC, vf01, vf13 :i
  vu.acc.mula_xyzw(vu.vf01, vu.vf13.x());   vu.I = 3072.0;
  // lqi.xyzw vf09, vi01        |  minii.xy vf10, vf10, I
  vu.vf10.minii(Mask::xy, vu.vf10, vu.I);   lq_buffer_xyzw<DEBUG>(vu.vf09, vu.vi01++);
  // sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13
  vu.acc.madda_xyzw(vu.vf02, vu.vf13.y());   sq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi11 + 1);
  // sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13
  vu.acc.madd_xyzw(vu.vf13, vu.vf03, vu.vf13.z());   sq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi14 + 1);
  // lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  vu.vf08.add_xyzw(vu.vf08, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi01++);
  // lqi.xyzw vf15, vi01        |  maxw.w vf10, vf10, vf02
  vu.vf10.max(Mask::w, vu.vf10, vu.vf02.w());   lq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi01++);
  // mtir vi11, vf09.x          |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   vu.vi11 = vu.vf09.x_as_u16();
  // mtir vi14, vf09.y          |  maxx.xyzw vf13, vf13, vf00
  vu.vf13.max_xyzw(vu.vf13, vu.vf00.x());   vu.vi14 = vu.vf09.y_as_u16();
  // nop                        |  miniw.w vf08, vf08, vf01
  vu.vf08.mini(Mask::w, vu.vf08, vu.vf01.w());
  // div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  vu.vf09.add(Mask::zw, vu.vf09, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf08.w();

  // move.xyzw vf21, vf10       |  add.xyzw vf12, vf12, vf18
  vu.vf12.add_xyzw(vu.vf12, vu.vf18);   vu.vf21.move_xyzw(vu.vf10);
  // iand vi11, vi11, vi05      |  add.xyzw vf15, vf15, vf19
  vu.vf15.add_xyzw(vu.vf15, vu.vf19);   vu.vi11 = vu.vi11 & vu.vi05;
  // nop                        |  mulax.xyzw ACC, vf04, vf13
  vu.acc.mula_xyzw(vu.vf04, vu.vf13.x());
// BRANCH!
  // ibgtz vi09, L106           |  madday.xyzw ACC, vf05, vf13
  vu.acc.madda_xyzw(vu.vf05, vu.vf13.y());   bc = ((s16)vu.vi09) > 0;
  // iand vi14, vi14, vi05      |  maddaz.xyzw ACC, vf06, vf13
  vu.acc.madda_xyzw(vu.vf06, vu.vf13.z());   vu.vi14 = vu.vi14 & vu.vi05;
  if (bc) { goto L106; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L106:
// BRANCH!
  // ibne vi05, vi11, L107      |  maddw.xyzw vf13, vf07, vf00
  vu.acc.madd_xyzw(vu.vf13, vu.vf07, vu.vf00.w());   bc = (vu.vi05 != vu.vi11);
  // ilw.x vi09, -9(vi01)       |  mul.xyz vf08, vf08, Q
  vu.vf08.mul(Mask::xyz, vu.vf08, vu.Q); /* TODO mulq */   ilw_buffer(Mask::x, vu.vi09, vu.vi01 + -9);
  if (bc) { goto L107; }


  // mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  vu.vf14.mul_xyzw(vu.vf14, vu.Q); /* TODO mulq */   vu.vi11 = vu.vf12.x_as_u16();
  // mtir vi14, vf12.y          |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi14 = vu.vf12.y_as_u16();
// BRANCH!
  // b L108                     |  mul.xyzw vf13, vf13, vf23
  vu.vf13.mul_xyzw(vu.vf13, vu.vf23);   bc = true;
  // lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
  vu.vf08.add_xyzw(vu.vf08, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L108; }


  L107:
  // lq.xyzw vf20, 0(vi11)      |  mul.xyzw vf14, vf14, Q
  vu.vf14.mul_xyzw(vu.vf14, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11);
  // nop                        |  mulw.xyzw vf24, vf24, vf29
  vu.vf24.mul_xyzw(vu.vf24, vu.vf29.w());
  // lq.xyzw vf25, 0(vi14)      |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi14);
  // lq.xyzw vf23, 1(vi11)      |  mul.xyzw vf13, vf13, vf23
  vu.vf13.mul_xyzw(vu.vf13, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi11 + 1);
  // lq.xyzw vf26, 1(vi14)      |  add.xyzw vf08, vf08, vf22
  vu.vf08.add_xyzw(vu.vf08, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi14 + 1);
  // lq.xyzw vf20, 2(vi11)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11 + 2);
  // lq.xyzw vf27, 2(vi14)      |  maddy.xyzw vf25, vf25, vf24
  vu.acc.madd_xyzw(vu.vf25, vu.vf25, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi14 + 2);
  // lq.xyzw vf23, 3(vi11)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi11 + 3);
  // lq.xyzw vf28, 3(vi14)      |  maddy.xyzw vf26, vf26, vf24
  vu.acc.madd_xyzw(vu.vf26, vu.vf26, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi14 + 3);
  // lq.xyzw vf20, 4(vi11)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11 + 4);
  // lq.xyz vf29, 4(vi14)       |  maddy.xyzw vf27, vf27, vf24
  vu.acc.madd_xyzw(vu.vf27, vu.vf27, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi14 + 4);
  // lq.xyzw vf23, 5(vi11)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi11 + 5);
  // lq.xyz vf30, 5(vi14)       |  maddy.xyzw vf28, vf28, vf24
  vu.acc.madd_xyzw(vu.vf28, vu.vf28, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi14 + 5);
  // lq.xyzw vf20, 6(vi11)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11 + 6);
  // lq.xyzw vf31, 6(vi14)      |  maddy.xyz vf29, vf29, vf24
  vu.acc.madd_xyz(vu.vf29, vu.vf29, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi14 + 6);
  // mtir vi11, vf12.x          |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   vu.vi11 = vu.vf12.x_as_u16();
  // mtir vi14, vf12.y          |  maddy.xyz vf30, vf30, vf24
  vu.acc.madd_xyz(vu.vf30, vu.vf30, vu.vf24.y());   vu.vi14 = vu.vf12.y_as_u16();
// BRANCH!
  // b L123                     |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   bc = true;
  // lqi.xyzw vf23, vi03        |  maddy.xyzw vf31, vf31, vf24
  vu.acc.madd_xyzw(vu.vf31, vu.vf31, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L123; }


  L108:
// BRANCH!
  // ibgez vi09, L109           |  mulaz.xyzw ACC, vf29, vf09
  vu.acc.mula_xyzw(vu.vf29, vu.vf09.z());   bc = ((s16)vu.vi09) >= 0;
  // sq.xyzw vf21, 2(vi12)      |  maddaz.xyzw ACC, vf30, vf12
  vu.acc.madda_xyzw(vu.vf30, vu.vf12.z());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi12 + 2);
  if (bc) { goto L109; }


  // nop                        |  ftoi4.xyzw vf21, vf10
  vu.vf21.ftoi4(Mask::xyzw, vu.vf10);
  L109:
  // mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  vu.acc.madd_xyz(vu.vf12, vu.vf31, vu.vf15.z());   vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  vu.vf13.mini_xyzw(vu.vf13, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi12);
  // sq.xyzw vf16, 0(vi15)      |  miniw.w vf08, vf08, vf03
  vu.vf08.mini(Mask::w, vu.vf08, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi15);
  // sq.xyzw vf21, 2(vi15)      |  mulaw.xyzw ACC, vf25, vf09
  vu.acc.mula_xyzw(vu.vf25, vu.vf09.w());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi15 + 2);
  // ilw.y vi09, -6(vi01)       |  mulw.xyzw vf11, vf11, vf20
  vu.vf11.mul_xyzw(vu.vf11, vu.vf20.w());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  // 1024.0                     |  ftoi0.xyzw vf13, vf13 :i
  vu.vf13.ftoi0(Mask::xyzw, vu.vf13);   vu.I = 1024.0;
  // erleng.xyz P, vf12         |  maxi.xy vf08, vf08, I
  vu.vf08.maxi(Mask::xy, vu.vf08, vu.I);   vu.P = erleng(vu.vf12); /* TODO erleng */
// BRANCH!
  // ibne vi06, vi03, L94       |  maddaw.xyzw ACC, vf26, vf12
  vu.acc.madda_xyzw(vu.vf26, vu.vf12.w());   bc = (vu.vi06 != vu.vi03);
  // mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  vu.acc.madd_xyzw(vu.vf09, vu.vf27, vu.vf15.w());   vu.vf15.z() = 1.f;
  if (bc) { goto L94; }


// BRANCH!
  // ibne vi07, vi03, L127      |  nop
  bc = (vu.vi07 != vu.vi03);
  // nop                        |  nop

  if (bc) { goto L127; }


// BRANCH!
  // b L163                     |  nop
  bc = true;
  // nop                        |  nop

  if (bc) { goto L163; }


  L110:
  // 3072.0                     |  mulax.xyzw ACC, vf01, vf11 :i
  vu.acc.mula_xyzw(vu.vf01, vu.vf11.x());   vu.I = 3072.0;
  // lqi.xyzw vf10, vi01        |  minii.xy vf08, vf08, I
  vu.vf08.minii(Mask::xy, vu.vf08, vu.I);   lq_buffer_xyzw<DEBUG>(vu.vf10, vu.vi01++);
  // sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  vu.acc.madda_xyzw(vu.vf02, vu.vf11.y());   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi12 + 1);
  // sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
  vu.acc.madd_xyzw(vu.vf11, vu.vf03, vu.vf11.z());   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi15 + 1);
  JUMP_539:
  // lqi.xyzw vf13, vi01        |  add.xyzw vf09, vf09, vf28
  vu.vf09.add_xyzw(vu.vf09, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi01++);
  // lqi.xyzw vf16, vi01        |  maxw.w vf08, vf08, vf02
  vu.vf08.max(Mask::w, vu.vf08, vu.vf02.w());   lq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi01++);
  // mtir vi12, vf10.x          |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   vu.vi12 = vu.vf10.x_as_u16();
  // mtir vi15, vf10.y          |  maxx.xyzw vf11, vf11, vf00
  vu.vf11.max_xyzw(vu.vf11, vu.vf00.x());   vu.vi15 = vu.vf10.y_as_u16();
  // nop                        |  miniw.w vf09, vf09, vf01
  vu.vf09.mini(Mask::w, vu.vf09, vu.vf01.w());
  // div Q, vf01.w, vf09.w      |  add.zw vf10, vf10, vf17
  vu.vf10.add(Mask::zw, vu.vf10, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf09.w();

  // move.xyzw vf21, vf08       |  add.xyzw vf13, vf13, vf18
  vu.vf13.add_xyzw(vu.vf13, vu.vf18);   vu.vf21.move_xyzw(vu.vf08);
  // iand vi12, vi12, vi05      |  add.xyzw vf16, vf16, vf19
  vu.vf16.add_xyzw(vu.vf16, vu.vf19);   vu.vi12 = vu.vi12 & vu.vi05;
  // nop                        |  mulax.xyzw ACC, vf04, vf11
  vu.acc.mula_xyzw(vu.vf04, vu.vf11.x());
// BRANCH!
  // ibgtz vi09, L111           |  madday.xyzw ACC, vf05, vf11
  vu.acc.madda_xyzw(vu.vf05, vu.vf11.y());   bc = ((s16)vu.vi09) > 0;
  // iand vi15, vi15, vi05      |  maddaz.xyzw ACC, vf06, vf11
  vu.acc.madda_xyzw(vu.vf06, vu.vf11.z());   vu.vi15 = vu.vi15 & vu.vi05;
  if (bc) { goto L111; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L111:
// BRANCH!
  // ibne vi05, vi12, L112      |  maddw.xyzw vf11, vf07, vf00
  vu.acc.madd_xyzw(vu.vf11, vu.vf07, vu.vf00.w());   bc = (vu.vi05 != vu.vi12);
  // ilw.x vi09, -9(vi01)       |  mul.xyz vf09, vf09, Q
  vu.vf09.mul(Mask::xyz, vu.vf09, vu.Q); /* TODO mulq */   ilw_buffer(Mask::x, vu.vi09, vu.vi01 + -9);
  if (bc) { goto L112; }


  // mtir vi12, vf13.x          |  mul.xyzw vf15, vf15, Q
  vu.vf15.mul_xyzw(vu.vf15, vu.Q); /* TODO mulq */   vu.vi12 = vu.vf13.x_as_u16();
  // mtir vi15, vf13.y          |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi15 = vu.vf13.y_as_u16();
// BRANCH!
  // b L113                     |  mul.xyzw vf11, vf11, vf23
  vu.vf11.mul_xyzw(vu.vf11, vu.vf23);   bc = true;
  // lqi.xyzw vf23, vi03        |  add.xyzw vf09, vf09, vf22
  vu.vf09.add_xyzw(vu.vf09, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L113; }


  L112:
  // lq.xyzw vf20, 0(vi12)      |  mul.xyzw vf15, vf15, Q
  vu.vf15.mul_xyzw(vu.vf15, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi12);
  // lq.xyzw vf25, 0(vi15)      |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi15);
  // lq.xyzw vf23, 1(vi12)      |  mul.xyzw vf11, vf11, vf23
  vu.vf11.mul_xyzw(vu.vf11, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi12 + 1);
  // lq.xyzw vf26, 1(vi15)      |  add.xyzw vf09, vf09, vf22
  vu.vf09.add_xyzw(vu.vf09, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi15 + 1);
  // lq.xyzw vf20, 2(vi12)      |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi12 + 2);
  // lq.xyzw vf27, 2(vi15)      |  maddw.xyzw vf25, vf25, vf24
  vu.acc.madd_xyzw(vu.vf25, vu.vf25, vu.vf24.w());   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi15 + 2);
  // lq.xyzw vf23, 3(vi12)      |  mulaz.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi12 + 3);
  // lq.xyzw vf28, 3(vi15)      |  maddw.xyzw vf26, vf26, vf24
  vu.acc.madd_xyzw(vu.vf26, vu.vf26, vu.vf24.w());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi15 + 3);
  // lq.xyzw vf20, 4(vi12)      |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi12 + 4);
  // lq.xyz vf29, 4(vi15)       |  maddw.xyzw vf27, vf27, vf24
  vu.acc.madd_xyzw(vu.vf27, vu.vf27, vu.vf24.w());   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi15 + 4);
  // lq.xyzw vf23, 5(vi12)      |  mulaz.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi12 + 5);
  // lq.xyz vf30, 5(vi15)       |  maddw.xyzw vf28, vf28, vf24
  vu.acc.madd_xyzw(vu.vf28, vu.vf28, vu.vf24.w());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi15 + 5);
  // lq.xyzw vf20, 6(vi12)      |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi12 + 6);
  // lq.xyzw vf31, 6(vi15)      |  maddw.xyz vf29, vf29, vf24
  vu.acc.madd_xyz(vu.vf29, vu.vf29, vu.vf24.w());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi15 + 6);
  // lqi.xyzw vf23, vi02        |  mulaz.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi02++);
  // mtir vi12, vf13.x          |  maddw.xyz vf30, vf30, vf24
  vu.acc.madd_xyz(vu.vf30, vu.vf30, vu.vf24.w());   vu.vi12 = vu.vf13.x_as_u16();
  // mtir vi15, vf13.y          |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   vu.vi15 = vu.vf13.y_as_u16();
// BRANCH!
  // b L98                      |  maddw.xyzw vf31, vf31, vf24
  vu.acc.madd_xyzw(vu.vf31, vu.vf31, vu.vf24.w());   bc = true;
  // lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
  vu.vf24.itof0(Mask::xyzw, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L98; }


  L113:
// BRANCH!
  // ibgez vi09, L114           |  mulaz.xyzw ACC, vf29, vf10
  vu.acc.mula_xyzw(vu.vf29, vu.vf10.z());   bc = ((s16)vu.vi09) >= 0;
  // sq.xyzw vf21, 2(vi10)      |  maddaz.xyzw ACC, vf30, vf13
  vu.acc.madda_xyzw(vu.vf30, vu.vf13.z());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi10 + 2);
  if (bc) { goto L114; }


  // nop                        |  ftoi4.xyzw vf21, vf08
  vu.vf21.ftoi4(Mask::xyzw, vu.vf08);
  L114:
  // mfp.w vf20, P              |  maddz.xyz vf13, vf31, vf16
  vu.acc.madd_xyz(vu.vf13, vu.vf31, vu.vf16.z());   vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  vu.vf11.mini_xyzw(vu.vf11, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi10);
  // sq.xyzw vf14, 0(vi13)      |  miniw.w vf09, vf09, vf03
  vu.vf09.mini(Mask::w, vu.vf09, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi13);
  // sq.xyzw vf21, 2(vi13)      |  mulaw.xyzw ACC, vf25, vf10
  vu.acc.mula_xyzw(vu.vf25, vu.vf10.w());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi13 + 2);
  // ilw.y vi09, -6(vi01)       |  mulw.xyzw vf12, vf12, vf20
  vu.vf12.mul_xyzw(vu.vf12, vu.vf20.w());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  // 1024.0                     |  ftoi0.xyzw vf11, vf11 :i
  vu.vf11.ftoi0(Mask::xyzw, vu.vf11);   vu.I = 1024.0;
  // erleng.xyz P, vf13         |  maxi.xy vf09, vf09, I
  vu.vf09.maxi(Mask::xy, vu.vf09, vu.I);   vu.P = erleng(vu.vf13); /* TODO erleng */
// BRANCH!
  // ibne vi06, vi03, L115      |  maddaw.xyzw ACC, vf26, vf13
  vu.acc.madda_xyzw(vu.vf26, vu.vf13.w());   bc = (vu.vi06 != vu.vi03);
  // mr32.z vf16, vf00          |  maddw.xyzw vf10, vf27, vf16
  vu.acc.madd_xyzw(vu.vf10, vu.vf27, vu.vf16.w());   vu.vf16.z() = 1.f;
  if (bc) { goto L115; }


// BRANCH!
  // ibne vi07, vi03, L133      |  nop
  bc = (vu.vi07 != vu.vi03);
  // nop                        |  nop

  if (bc) { goto L133; }


// BRANCH!
  // b L143                     |  nop
  bc = true;
  // nop                        |  nop

  if (bc) { goto L143; }


  L115:
  // 3072.0                     |  mulax.xyzw ACC, vf01, vf12 :i
  vu.acc.mula_xyzw(vu.vf01, vu.vf12.x());   vu.I = 3072.0;
  // lqi.xyzw vf08, vi01        |  minii.xy vf09, vf09, I
  vu.vf09.minii(Mask::xy, vu.vf09, vu.I);   lq_buffer_xyzw<DEBUG>(vu.vf08, vu.vi01++);
  // sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12
  vu.acc.madda_xyzw(vu.vf02, vu.vf12.y());   sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi10 + 1);
  // sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12
  vu.acc.madd_xyzw(vu.vf12, vu.vf03, vu.vf12.z());   sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi13 + 1);
  // lqi.xyzw vf11, vi01        |  add.xyzw vf10, vf10, vf28
  vu.vf10.add_xyzw(vu.vf10, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi01++);
  // lqi.xyzw vf14, vi01        |  maxw.w vf09, vf09, vf02
  vu.vf09.max(Mask::w, vu.vf09, vu.vf02.w());   lq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi01++);
  // mtir vi10, vf08.x          |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   vu.vi10 = vu.vf08.x_as_u16();
  // mtir vi13, vf08.y          |  maxx.xyzw vf12, vf12, vf00
  vu.vf12.max_xyzw(vu.vf12, vu.vf00.x());   vu.vi13 = vu.vf08.y_as_u16();
  // nop                        |  miniw.w vf10, vf10, vf01
  vu.vf10.mini(Mask::w, vu.vf10, vu.vf01.w());
  // div Q, vf01.w, vf10.w      |  add.zw vf08, vf08, vf17
  vu.vf08.add(Mask::zw, vu.vf08, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf10.w();

  // move.xyzw vf21, vf09       |  add.xyzw vf11, vf11, vf18
  vu.vf11.add_xyzw(vu.vf11, vu.vf18);   vu.vf21.move_xyzw(vu.vf09);
  // iand vi10, vi10, vi05      |  add.xyzw vf14, vf14, vf19
  vu.vf14.add_xyzw(vu.vf14, vu.vf19);   vu.vi10 = vu.vi10 & vu.vi05;
  // nop                        |  mulax.xyzw ACC, vf04, vf12
  vu.acc.mula_xyzw(vu.vf04, vu.vf12.x());
// BRANCH!
  // ibgtz vi09, L116           |  madday.xyzw ACC, vf05, vf12
  vu.acc.madda_xyzw(vu.vf05, vu.vf12.y());   bc = ((s16)vu.vi09) > 0;
  // iand vi13, vi13, vi05      |  maddaz.xyzw ACC, vf06, vf12
  vu.acc.madda_xyzw(vu.vf06, vu.vf12.z());   vu.vi13 = vu.vi13 & vu.vi05;
  if (bc) { goto L116; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L116:
// BRANCH!
  // ibne vi05, vi10, L117      |  maddw.xyzw vf12, vf07, vf00
  vu.acc.madd_xyzw(vu.vf12, vu.vf07, vu.vf00.w());   bc = (vu.vi05 != vu.vi10);
  // ilw.x vi09, -9(vi01)       |  mul.xyz vf10, vf10, Q
  vu.vf10.mul(Mask::xyz, vu.vf10, vu.Q); /* TODO mulq */   ilw_buffer(Mask::x, vu.vi09, vu.vi01 + -9);
  if (bc) { goto L117; }


  // mtir vi10, vf11.x          |  mul.xyzw vf16, vf16, Q
  vu.vf16.mul_xyzw(vu.vf16, vu.Q); /* TODO mulq */   vu.vi10 = vu.vf11.x_as_u16();
  // mtir vi13, vf11.y          |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi13 = vu.vf11.y_as_u16();
// BRANCH!
  // b L118                     |  mul.xyzw vf12, vf12, vf23
  vu.vf12.mul_xyzw(vu.vf12, vu.vf23);   bc = true;
  // lqi.xyzw vf23, vi03        |  add.xyzw vf10, vf10, vf22
  vu.vf10.add_xyzw(vu.vf10, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L118; }


  L117:
  // lq.xyzw vf20, 0(vi10)      |  mul.xyzw vf16, vf16, Q
  vu.vf16.mul_xyzw(vu.vf16, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10);
  // lq.xyzw vf25, 0(vi13)      |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi13);
  // lq.xyzw vf23, 1(vi10)      |  mul.xyzw vf12, vf12, vf23
  vu.vf12.mul_xyzw(vu.vf12, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi10 + 1);
  // lq.xyzw vf26, 1(vi13)      |  add.xyzw vf10, vf10, vf22
  vu.vf10.add_xyzw(vu.vf10, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi13 + 1);
  // lq.xyzw vf20, 2(vi10)      |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10 + 2);
  // lq.xyzw vf27, 2(vi13)      |  maddw.xyzw vf25, vf25, vf24
  vu.acc.madd_xyzw(vu.vf25, vu.vf25, vu.vf24.w());   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi13 + 2);
  // lq.xyzw vf23, 3(vi10)      |  mulaz.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi10 + 3);
  // lq.xyzw vf28, 3(vi13)      |  maddw.xyzw vf26, vf26, vf24
  vu.acc.madd_xyzw(vu.vf26, vu.vf26, vu.vf24.w());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi13 + 3);
  // lq.xyzw vf20, 4(vi10)      |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10 + 4);
  // lq.xyz vf29, 4(vi13)       |  maddw.xyzw vf27, vf27, vf24
  vu.acc.madd_xyzw(vu.vf27, vu.vf27, vu.vf24.w());   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi13 + 4);
  // lq.xyzw vf23, 5(vi10)      |  mulaz.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi10 + 5);
  // lq.xyz vf30, 5(vi13)       |  maddw.xyzw vf28, vf28, vf24
  vu.acc.madd_xyzw(vu.vf28, vu.vf28, vu.vf24.w());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi13 + 5);
  // lq.xyzw vf20, 6(vi10)      |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10 + 6);
  // lq.xyzw vf31, 6(vi13)      |  maddw.xyz vf29, vf29, vf24
  vu.acc.madd_xyz(vu.vf29, vu.vf29, vu.vf24.w());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi13 + 6);
  // lqi.xyzw vf23, vi02        |  mulaz.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi02++);
  // mtir vi10, vf11.x          |  maddw.xyz vf30, vf30, vf24
  vu.acc.madd_xyz(vu.vf30, vu.vf30, vu.vf24.w());   vu.vi10 = vu.vf11.x_as_u16();
  // mtir vi13, vf11.y          |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   vu.vi13 = vu.vf11.y_as_u16();
// BRANCH!
  // b L103                     |  maddw.xyzw vf31, vf31, vf24
  vu.acc.madd_xyzw(vu.vf31, vu.vf31, vu.vf24.w());   bc = true;
  // lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
  vu.vf24.itof0(Mask::xyzw, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L103; }


  L118:
// BRANCH!
  // ibgez vi09, L119           |  mulaz.xyzw ACC, vf29, vf08
  vu.acc.mula_xyzw(vu.vf29, vu.vf08.z());   bc = ((s16)vu.vi09) >= 0;
  // sq.xyzw vf21, 2(vi11)      |  maddaz.xyzw ACC, vf30, vf11
  vu.acc.madda_xyzw(vu.vf30, vu.vf11.z());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi11 + 2);
  if (bc) { goto L119; }


  // nop                        |  ftoi4.xyzw vf21, vf09
  vu.vf21.ftoi4(Mask::xyzw, vu.vf09);
  L119:
  // mfp.w vf20, P              |  maddz.xyz vf11, vf31, vf14
  vu.acc.madd_xyz(vu.vf11, vu.vf31, vu.vf14.z());   vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17
  vu.vf12.mini_xyzw(vu.vf12, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi11);
  // sq.xyzw vf15, 0(vi14)      |  miniw.w vf10, vf10, vf03
  vu.vf10.mini(Mask::w, vu.vf10, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi14);
  // sq.xyzw vf21, 2(vi14)      |  mulaw.xyzw ACC, vf25, vf08
  vu.acc.mula_xyzw(vu.vf25, vu.vf08.w());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi14 + 2);
  // ilw.y vi09, -6(vi01)       |  mulw.xyzw vf13, vf13, vf20
  vu.vf13.mul_xyzw(vu.vf13, vu.vf20.w());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  // 1024.0                     |  ftoi0.xyzw vf12, vf12 :i
  vu.vf12.ftoi0(Mask::xyzw, vu.vf12);   vu.I = 1024.0;
  // erleng.xyz P, vf11         |  maxi.xy vf10, vf10, I
  vu.vf10.maxi(Mask::xy, vu.vf10, vu.I);   vu.P = erleng(vu.vf11); /* TODO erleng */
// BRANCH!
  // ibne vi06, vi03, L120      |  maddaw.xyzw ACC, vf26, vf11
  vu.acc.madda_xyzw(vu.vf26, vu.vf11.w());   bc = (vu.vi06 != vu.vi03);
  // mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
  vu.acc.madd_xyzw(vu.vf08, vu.vf27, vu.vf14.w());   vu.vf14.z() = 1.f;
  if (bc) { goto L120; }


// BRANCH!
  // ibne vi07, vi03, L138      |  nop
  bc = (vu.vi07 != vu.vi03);
  // nop                        |  nop

  if (bc) { goto L138; }


// BRANCH!
  // b L153                     |  nop
  bc = true;
  // nop                        |  nop

  if (bc) { goto L153; }


  L120:
  // 3072.0                     |  mulax.xyzw ACC, vf01, vf13 :i
  vu.acc.mula_xyzw(vu.vf01, vu.vf13.x());   vu.I = 3072.0;
  // lqi.xyzw vf09, vi01        |  minii.xy vf10, vf10, I
  vu.vf10.minii(Mask::xy, vu.vf10, vu.I);   lq_buffer_xyzw<DEBUG>(vu.vf09, vu.vi01++);
  // sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13
  vu.acc.madda_xyzw(vu.vf02, vu.vf13.y());   sq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi11 + 1);
  // sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13
  vu.acc.madd_xyzw(vu.vf13, vu.vf03, vu.vf13.z());   sq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi14 + 1);
  // lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  vu.vf08.add_xyzw(vu.vf08, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi01++);
  // lqi.xyzw vf15, vi01        |  maxw.w vf10, vf10, vf02
  vu.vf10.max(Mask::w, vu.vf10, vu.vf02.w());   lq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi01++);
  // mtir vi11, vf09.x          |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   vu.vi11 = vu.vf09.x_as_u16();
  // mtir vi14, vf09.y          |  maxx.xyzw vf13, vf13, vf00
  vu.vf13.max_xyzw(vu.vf13, vu.vf00.x());   vu.vi14 = vu.vf09.y_as_u16();
  // nop                        |  miniw.w vf08, vf08, vf01
  vu.vf08.mini(Mask::w, vu.vf08, vu.vf01.w());
  // div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  vu.vf09.add(Mask::zw, vu.vf09, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf08.w();

  // move.xyzw vf21, vf10       |  add.xyzw vf12, vf12, vf18
  vu.vf12.add_xyzw(vu.vf12, vu.vf18);   vu.vf21.move_xyzw(vu.vf10);
  // iand vi11, vi11, vi05      |  add.xyzw vf15, vf15, vf19
  vu.vf15.add_xyzw(vu.vf15, vu.vf19);   vu.vi11 = vu.vi11 & vu.vi05;
  // nop                        |  mulax.xyzw ACC, vf04, vf13
  vu.acc.mula_xyzw(vu.vf04, vu.vf13.x());
// BRANCH!
  // ibgtz vi09, L121           |  madday.xyzw ACC, vf05, vf13
  vu.acc.madda_xyzw(vu.vf05, vu.vf13.y());   bc = ((s16)vu.vi09) > 0;
  // iand vi14, vi14, vi05      |  maddaz.xyzw ACC, vf06, vf13
  vu.acc.madda_xyzw(vu.vf06, vu.vf13.z());   vu.vi14 = vu.vi14 & vu.vi05;
  if (bc) { goto L121; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L121:
// BRANCH!
  // ibne vi05, vi11, L122      |  maddw.xyzw vf13, vf07, vf00
  vu.acc.madd_xyzw(vu.vf13, vu.vf07, vu.vf00.w());   bc = (vu.vi05 != vu.vi11);
  // ilw.x vi09, -9(vi01)       |  mul.xyz vf08, vf08, Q
  vu.vf08.mul(Mask::xyz, vu.vf08, vu.Q); /* TODO mulq */   ilw_buffer(Mask::x, vu.vi09, vu.vi01 + -9);
  if (bc) { goto L122; }


  // mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  vu.vf14.mul_xyzw(vu.vf14, vu.Q); /* TODO mulq */   vu.vi11 = vu.vf12.x_as_u16();
  // mtir vi14, vf12.y          |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi14 = vu.vf12.y_as_u16();
// BRANCH!
  // b L123                     |  mul.xyzw vf13, vf13, vf23
  vu.vf13.mul_xyzw(vu.vf13, vu.vf23);   bc = true;
  // lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
  vu.vf08.add_xyzw(vu.vf08, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L123; }


  L122:
  // lq.xyzw vf20, 0(vi11)      |  mul.xyzw vf14, vf14, Q
  vu.vf14.mul_xyzw(vu.vf14, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11);
  // lq.xyzw vf25, 0(vi14)      |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi14);
  // lq.xyzw vf23, 1(vi11)      |  mul.xyzw vf13, vf13, vf23
  vu.vf13.mul_xyzw(vu.vf13, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi11 + 1);
  // lq.xyzw vf26, 1(vi14)      |  add.xyzw vf08, vf08, vf22
  vu.vf08.add_xyzw(vu.vf08, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi14 + 1);
  // lq.xyzw vf20, 2(vi11)      |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11 + 2);
  // lq.xyzw vf27, 2(vi14)      |  maddw.xyzw vf25, vf25, vf24
  vu.acc.madd_xyzw(vu.vf25, vu.vf25, vu.vf24.w());   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi14 + 2);
  // lq.xyzw vf23, 3(vi11)      |  mulaz.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi11 + 3);
  // lq.xyzw vf28, 3(vi14)      |  maddw.xyzw vf26, vf26, vf24
  vu.acc.madd_xyzw(vu.vf26, vu.vf26, vu.vf24.w());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi14 + 3);
  // lq.xyzw vf20, 4(vi11)      |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11 + 4);
  // lq.xyz vf29, 4(vi14)       |  maddw.xyzw vf27, vf27, vf24
  vu.acc.madd_xyzw(vu.vf27, vu.vf27, vu.vf24.w());   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi14 + 4);
  // lq.xyzw vf23, 5(vi11)      |  mulaz.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi11 + 5);
  // lq.xyz vf30, 5(vi14)       |  maddw.xyzw vf28, vf28, vf24
  vu.acc.madd_xyzw(vu.vf28, vu.vf28, vu.vf24.w());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi14 + 5);
  // lq.xyzw vf20, 6(vi11)      |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11 + 6);
  // lq.xyzw vf31, 6(vi14)      |  maddw.xyz vf29, vf29, vf24
  vu.acc.madd_xyz(vu.vf29, vu.vf29, vu.vf24.w());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi14 + 6);
  // lqi.xyzw vf23, vi02        |  mulaz.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi02++);
  // mtir vi11, vf12.x          |  maddw.xyz vf30, vf30, vf24
  vu.acc.madd_xyz(vu.vf30, vu.vf30, vu.vf24.w());   vu.vi11 = vu.vf12.x_as_u16();
  // mtir vi14, vf12.y          |  mulaz.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.z());   vu.vi14 = vu.vf12.y_as_u16();
// BRANCH!
  // b L108                     |  maddw.xyzw vf31, vf31, vf24
  vu.acc.madd_xyzw(vu.vf31, vu.vf31, vu.vf24.w());   bc = true;
  // lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
  vu.vf24.itof0(Mask::xyzw, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L108; }


  L123:
// BRANCH!
  // ibgez vi09, L124           |  mulaz.xyzw ACC, vf29, vf09
  vu.acc.mula_xyzw(vu.vf29, vu.vf09.z());   bc = ((s16)vu.vi09) >= 0;
  // sq.xyzw vf21, 2(vi12)      |  maddaz.xyzw ACC, vf30, vf12
  vu.acc.madda_xyzw(vu.vf30, vu.vf12.z());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi12 + 2);
  if (bc) { goto L124; }


  // nop                        |  ftoi4.xyzw vf21, vf10
  vu.vf21.ftoi4(Mask::xyzw, vu.vf10);
  L124:
  // mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  vu.acc.madd_xyz(vu.vf12, vu.vf31, vu.vf15.z());   vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  vu.vf13.mini_xyzw(vu.vf13, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi12);
  // sq.xyzw vf16, 0(vi15)      |  miniw.w vf08, vf08, vf03
  vu.vf08.mini(Mask::w, vu.vf08, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi15);
  // sq.xyzw vf21, 2(vi15)      |  mulaw.xyzw ACC, vf25, vf09
  vu.acc.mula_xyzw(vu.vf25, vu.vf09.w());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi15 + 2);
  // ilw.y vi09, -6(vi01)       |  mulw.xyzw vf11, vf11, vf20
  vu.vf11.mul_xyzw(vu.vf11, vu.vf20.w());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  // 1024.0                     |  ftoi0.xyzw vf13, vf13 :i
  vu.vf13.ftoi0(Mask::xyzw, vu.vf13);   vu.I = 1024.0;
  // erleng.xyz P, vf12         |  maxi.xy vf08, vf08, I
  vu.vf08.maxi(Mask::xy, vu.vf08, vu.I);   vu.P = erleng(vu.vf12); /* TODO erleng */
// BRANCH!
  // ibne vi06, vi03, L110      |  maddaw.xyzw ACC, vf26, vf12
  vu.acc.madda_xyzw(vu.vf26, vu.vf12.w());   bc = (vu.vi06 != vu.vi03);
  // mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  vu.acc.madd_xyzw(vu.vf09, vu.vf27, vu.vf15.w());   vu.vf15.z() = 1.f;
  if (bc) { goto L110; }


// BRANCH!
  // ibne vi07, vi03, L133      |  nop
  bc = (vu.vi07 != vu.vi03);
  // nop                        |  nop

  if (bc) { goto L133; }


// BRANCH!
  // b L163                     |  nop
  bc = true;
  // nop                        |  nop

  if (bc) { goto L163; }


  L125:
  // erleng.xyz P, vf12         |  maxi.xy vf08, vf08, I
  vu.vf08.maxi(Mask::xy, vu.vf08, vu.I);   vu.P = erleng(vu.vf12); /* TODO erleng */
  // 3072.0                     |  nop :i
  vu.I = 3072.0;
  // nop                        |  minii.xy vf08, vf08, I
  vu.vf08.minii(Mask::xy, vu.vf08, vu.I);
  // nop                        |  maddaw.xyzw ACC, vf26, vf12
  vu.acc.madda_xyzw(vu.vf26, vu.vf12.w());
  // mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  vu.acc.madd_xyzw(vu.vf09, vu.vf27, vu.vf15.w());   vu.vf15.z() = 1.f;
  L126:
  // lqi.xyzw vf10, vi01        |  mulax.xyzw ACC, vf01, vf11
  vu.acc.mula_xyzw(vu.vf01, vu.vf11.x());   lq_buffer_xyzw<DEBUG>(vu.vf10, vu.vi01++);
// BRANCH!
  // b L128                     |  madday.xyzw ACC, vf02, vf11
  vu.acc.madda_xyzw(vu.vf02, vu.vf11.y());   bc = true;
  // nop                        |  maddz.xyzw vf11, vf03, vf11
  vu.acc.madd_xyzw(vu.vf11, vu.vf03, vu.vf11.z());
  if (bc) { goto L128; }


  L127:
  // 3072.0                     |  mulax.xyzw ACC, vf01, vf11 :i
  vu.acc.mula_xyzw(vu.vf01, vu.vf11.x());   vu.I = 3072.0;
  // lqi.xyzw vf10, vi01        |  minii.xy vf08, vf08, I
  vu.vf08.minii(Mask::xy, vu.vf08, vu.I);   lq_buffer_xyzw<DEBUG>(vu.vf10, vu.vi01++);
  // sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  vu.acc.madda_xyzw(vu.vf02, vu.vf11.y());   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi12 + 1);
  // sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
  vu.acc.madd_xyzw(vu.vf11, vu.vf03, vu.vf11.z());   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi15 + 1);
  L128:
  // lqi.xyzw vf13, vi01        |  add.xyzw vf09, vf09, vf28
  vu.vf09.add_xyzw(vu.vf09, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi01++);
  // lqi.xyzw vf16, vi01        |  maxw.w vf08, vf08, vf02
  vu.vf08.max(Mask::w, vu.vf08, vu.vf02.w());   lq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi01++);
  // mtir vi12, vf10.x          |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   vu.vi12 = vu.vf10.x_as_u16();
  // mtir vi15, vf10.y          |  maxx.xyzw vf11, vf11, vf00
  vu.vf11.max_xyzw(vu.vf11, vu.vf00.x());   vu.vi15 = vu.vf10.y_as_u16();
  // nop                        |  miniw.w vf09, vf09, vf01
  vu.vf09.mini(Mask::w, vu.vf09, vu.vf01.w());
  // div Q, vf01.w, vf09.w      |  add.zw vf10, vf10, vf17
  vu.vf10.add(Mask::zw, vu.vf10, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf09.w();

  // move.xyzw vf21, vf08       |  add.xyzw vf13, vf13, vf18
  vu.vf13.add_xyzw(vu.vf13, vu.vf18);   vu.vf21.move_xyzw(vu.vf08);
  // iand vi12, vi12, vi05      |  add.xyzw vf16, vf16, vf19
  vu.vf16.add_xyzw(vu.vf16, vu.vf19);   vu.vi12 = vu.vi12 & vu.vi05;
  // ilw.w vi08, -1(vi02)       |  mulax.xyzw ACC, vf04, vf11
  vu.acc.mula_xyzw(vu.vf04, vu.vf11.x());   ilw_buffer(Mask::w, vu.vi08, vu.vi02 + -1);
// BRANCH!
  // ibgtz vi09, L129           |  madday.xyzw ACC, vf05, vf11
  vu.acc.madda_xyzw(vu.vf05, vu.vf11.y());   bc = ((s16)vu.vi09) > 0;
  // iand vi15, vi15, vi05      |  maddaz.xyzw ACC, vf06, vf11
  vu.acc.madda_xyzw(vu.vf06, vu.vf11.z());   vu.vi15 = vu.vi15 & vu.vi05;
  if (bc) { goto L129; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L129:
// BRANCH!
  // ibne vi05, vi12, L130      |  maddw.xyzw vf11, vf07, vf00
  vu.acc.madd_xyzw(vu.vf11, vu.vf07, vu.vf00.w());   bc = (vu.vi05 != vu.vi12);
  // ilw.x vi09, -9(vi01)       |  mul.xyz vf09, vf09, Q
  vu.vf09.mul(Mask::xyz, vu.vf09, vu.Q); /* TODO mulq */   ilw_buffer(Mask::x, vu.vi09, vu.vi01 + -9);
  if (bc) { goto L130; }


  // mtir vi12, vf13.x          |  mul.xyzw vf15, vf15, Q
  vu.vf15.mul_xyzw(vu.vf15, vu.Q); /* TODO mulq */   vu.vi12 = vu.vf13.x_as_u16();
  // mtir vi15, vf13.y          |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi15 = vu.vf13.y_as_u16();
// BRANCH!
  // b L131                     |  mul.xyzw vf11, vf11, vf23
  vu.vf11.mul_xyzw(vu.vf11, vu.vf23);   bc = true;
  // lqi.xyzw vf23, vi03        |  add.xyzw vf09, vf09, vf22
  vu.vf09.add_xyzw(vu.vf09, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L131; }


  L130:
  // lq.xyzw vf20, 0(vi12)      |  mul.xyzw vf15, vf15, Q
  vu.vf15.mul_xyzw(vu.vf15, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi12);
  // nop                        |  mulw.xyzw vf24, vf24, vf29
  vu.vf24.mul_xyzw(vu.vf24, vu.vf29.w());
  // lq.xyzw vf31, 0(vi15)      |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi15);
  // lq.xyzw vf25, 0(vi08)      |  mul.xyzw vf11, vf11, vf23
  vu.vf11.mul_xyzw(vu.vf11, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi08);
  // lq.xyzw vf23, 1(vi12)      |  add.xyzw vf09, vf09, vf22
  vu.vf09.add_xyzw(vu.vf09, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi12 + 1);
  // lq.xyzw vf20, 1(vi15)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi15 + 1);
  // lq.xyzw vf26, 1(vi08)      |  madday.xyzw ACC, vf31, vf24
  vu.acc.madda_xyzw(vu.vf31, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi08 + 1);
  // lq.xyzw vf31, 2(vi12)      |  maddz.xyzw vf25, vf25, vf24
  vu.acc.madd_xyzw(vu.vf25, vu.vf25, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi12 + 2);
  // lq.xyzw vf23, 2(vi15)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi15 + 2);
  // lq.xyzw vf27, 2(vi08)      |  madday.xyzw ACC, vf20, vf24
  vu.acc.madda_xyzw(vu.vf20, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi08 + 2);
  // lq.xyzw vf20, 3(vi12)      |  maddz.xyzw vf26, vf26, vf24
  vu.acc.madd_xyzw(vu.vf26, vu.vf26, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi12 + 3);
  // lq.xyzw vf31, 3(vi15)      |  mulax.xyzw ACC, vf31, vf24
  vu.acc.mula_xyzw(vu.vf31, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi15 + 3);
  // lq.xyzw vf28, 3(vi08)      |  madday.xyzw ACC, vf23, vf24
  vu.acc.madda_xyzw(vu.vf23, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi08 + 3);
  // lq.xyzw vf23, 4(vi12)      |  maddz.xyzw vf27, vf27, vf24
  vu.acc.madd_xyzw(vu.vf27, vu.vf27, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi12 + 4);
  // lq.xyzw vf20, 4(vi15)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi15 + 4);
  // lq.xyz vf29, 4(vi08)       |  madday.xyzw ACC, vf31, vf24
  vu.acc.madda_xyzw(vu.vf31, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi08 + 4);
  // lq.xyzw vf31, 5(vi12)      |  maddz.xyzw vf28, vf28, vf24
  vu.acc.madd_xyzw(vu.vf28, vu.vf28, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi12 + 5);
  // lq.xyzw vf23, 5(vi15)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi15 + 5);
  // lq.xyz vf30, 5(vi08)       |  madday.xyzw ACC, vf20, vf24
  vu.acc.madda_xyzw(vu.vf20, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi08 + 5);
  // lq.xyzw vf20, 6(vi12)      |  maddz.xyz vf29, vf29, vf24
  vu.acc.madd_xyz(vu.vf29, vu.vf29, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi12 + 6);
  // lq.xyzw vf22, 6(vi15)      |  mulax.xyzw ACC, vf31, vf24
  vu.acc.mula_xyzw(vu.vf31, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf22, vu.vi15 + 6);
  // lq.xyzw vf31, 6(vi08)      |  madday.xyzw ACC, vf23, vf24
  vu.acc.madda_xyzw(vu.vf23, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi08 + 6);
  // lqi.xyzw vf23, vi02        |  maddz.xyz vf30, vf30, vf24
  vu.acc.madd_xyz(vu.vf30, vu.vf30, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi02++);
  // mtir vi12, vf13.x          |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   vu.vi12 = vu.vf13.x_as_u16();
  // mtir vi15, vf13.y          |  madday.xyzw ACC, vf22, vf24
  vu.acc.madda_xyzw(vu.vf22, vu.vf24.y());   vu.vi15 = vu.vf13.y_as_u16();
  // lq.xyzw vf22, 2(vi00)      |  maddz.xyzw vf31, vf31, vf24
  vu.acc.madd_xyzw(vu.vf31, vu.vf31, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf22, 2);
  // lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
  vu.vf24.itof0(Mask::xyzw, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  L131:
// BRANCH!
  // ibgez vi09, L132           |  mulaz.xyzw ACC, vf29, vf10
  vu.acc.mula_xyzw(vu.vf29, vu.vf10.z());   bc = ((s16)vu.vi09) >= 0;
  // sq.xyzw vf21, 2(vi10)      |  maddaz.xyzw ACC, vf30, vf13
  vu.acc.madda_xyzw(vu.vf30, vu.vf13.z());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi10 + 2);
  if (bc) { goto L132; }


  // nop                        |  ftoi4.xyzw vf21, vf08
  vu.vf21.ftoi4(Mask::xyzw, vu.vf08);
  L132:
  // mfp.w vf20, P              |  maddz.xyz vf13, vf31, vf16
  vu.acc.madd_xyz(vu.vf13, vu.vf31, vu.vf16.z());   vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  vu.vf11.mini_xyzw(vu.vf11, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi10);
  // sq.xyzw vf14, 0(vi13)      |  miniw.w vf09, vf09, vf03
  vu.vf09.mini(Mask::w, vu.vf09, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi13);
  // sq.xyzw vf21, 2(vi13)      |  mulaw.xyzw ACC, vf25, vf10
  vu.acc.mula_xyzw(vu.vf25, vu.vf10.w());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi13 + 2);
  // ilw.y vi09, -6(vi01)       |  mulw.xyzw vf12, vf12, vf20
  vu.vf12.mul_xyzw(vu.vf12, vu.vf20.w());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  // 1024.0                     |  ftoi0.xyzw vf11, vf11 :i
  vu.vf11.ftoi0(Mask::xyzw, vu.vf11);   vu.I = 1024.0;
  // erleng.xyz P, vf13         |  maxi.xy vf09, vf09, I
  vu.vf09.maxi(Mask::xy, vu.vf09, vu.I);   vu.P = erleng(vu.vf13); /* TODO erleng */
// BRANCH!
  // ibeq vi07, vi03, L143      |  maddaw.xyzw ACC, vf26, vf13
  vu.acc.madda_xyzw(vu.vf26, vu.vf13.w());   bc = (vu.vi07 == vu.vi03);
  // mr32.z vf16, vf00          |  maddw.xyzw vf10, vf27, vf16
  vu.acc.madd_xyzw(vu.vf10, vu.vf27, vu.vf16.w());   vu.vf16.z() = 1.f;
  if (bc) { goto L143; }


  L133:
  // 3072.0                     |  mulax.xyzw ACC, vf01, vf12 :i
  vu.acc.mula_xyzw(vu.vf01, vu.vf12.x());   vu.I = 3072.0;
  // lqi.xyzw vf08, vi01        |  minii.xy vf09, vf09, I
  vu.vf09.minii(Mask::xy, vu.vf09, vu.I);   lq_buffer_xyzw<DEBUG>(vu.vf08, vu.vi01++);
  // sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12
  vu.acc.madda_xyzw(vu.vf02, vu.vf12.y());   sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi10 + 1);
  // sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12
  vu.acc.madd_xyzw(vu.vf12, vu.vf03, vu.vf12.z());   sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi13 + 1);
  // lqi.xyzw vf11, vi01        |  add.xyzw vf10, vf10, vf28
  vu.vf10.add_xyzw(vu.vf10, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi01++);
  // lqi.xyzw vf14, vi01        |  maxw.w vf09, vf09, vf02
  vu.vf09.max(Mask::w, vu.vf09, vu.vf02.w());   lq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi01++);
  // mtir vi10, vf08.x          |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   vu.vi10 = vu.vf08.x_as_u16();
  // mtir vi13, vf08.y          |  maxx.xyzw vf12, vf12, vf00
  vu.vf12.max_xyzw(vu.vf12, vu.vf00.x());   vu.vi13 = vu.vf08.y_as_u16();
  // nop                        |  miniw.w vf10, vf10, vf01
  vu.vf10.mini(Mask::w, vu.vf10, vu.vf01.w());
  // div Q, vf01.w, vf10.w      |  add.zw vf08, vf08, vf17
  vu.vf08.add(Mask::zw, vu.vf08, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf10.w();

  // move.xyzw vf21, vf09       |  add.xyzw vf11, vf11, vf18
  vu.vf11.add_xyzw(vu.vf11, vu.vf18);   vu.vf21.move_xyzw(vu.vf09);
  // iand vi10, vi10, vi05      |  add.xyzw vf14, vf14, vf19
  vu.vf14.add_xyzw(vu.vf14, vu.vf19);   vu.vi10 = vu.vi10 & vu.vi05;
  // ilw.w vi08, -1(vi02)       |  mulax.xyzw ACC, vf04, vf12
  vu.acc.mula_xyzw(vu.vf04, vu.vf12.x());   ilw_buffer(Mask::w, vu.vi08, vu.vi02 + -1);
// BRANCH!
  // ibgtz vi09, L134           |  madday.xyzw ACC, vf05, vf12
  vu.acc.madda_xyzw(vu.vf05, vu.vf12.y());   bc = ((s16)vu.vi09) > 0;
  // iand vi13, vi13, vi05      |  maddaz.xyzw ACC, vf06, vf12
  vu.acc.madda_xyzw(vu.vf06, vu.vf12.z());   vu.vi13 = vu.vi13 & vu.vi05;
  if (bc) { goto L134; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L134:
// BRANCH!
  // ibne vi05, vi10, L135      |  maddw.xyzw vf12, vf07, vf00
  vu.acc.madd_xyzw(vu.vf12, vu.vf07, vu.vf00.w());   bc = (vu.vi05 != vu.vi10);
  // ilw.x vi09, -9(vi01)       |  mul.xyz vf10, vf10, Q
  vu.vf10.mul(Mask::xyz, vu.vf10, vu.Q); /* TODO mulq */   ilw_buffer(Mask::x, vu.vi09, vu.vi01 + -9);
  if (bc) { goto L135; }


  // mtir vi10, vf11.x          |  mul.xyzw vf16, vf16, Q
  vu.vf16.mul_xyzw(vu.vf16, vu.Q); /* TODO mulq */   vu.vi10 = vu.vf11.x_as_u16();
  // mtir vi13, vf11.y          |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi13 = vu.vf11.y_as_u16();
// BRANCH!
  // b L136                     |  mul.xyzw vf12, vf12, vf23
  vu.vf12.mul_xyzw(vu.vf12, vu.vf23);   bc = true;
  // lqi.xyzw vf23, vi03        |  add.xyzw vf10, vf10, vf22
  vu.vf10.add_xyzw(vu.vf10, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L136; }


  L135:
  // lq.xyzw vf20, 0(vi10)      |  mul.xyzw vf16, vf16, Q
  vu.vf16.mul_xyzw(vu.vf16, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10);
  // nop                        |  mulw.xyzw vf24, vf24, vf29
  vu.vf24.mul_xyzw(vu.vf24, vu.vf29.w());
  // lq.xyzw vf31, 0(vi13)      |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi13);
  // lq.xyzw vf25, 0(vi08)      |  mul.xyzw vf12, vf12, vf23
  vu.vf12.mul_xyzw(vu.vf12, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi08);
  // lq.xyzw vf23, 1(vi10)      |  add.xyzw vf10, vf10, vf22
  vu.vf10.add_xyzw(vu.vf10, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi10 + 1);
  // lq.xyzw vf20, 1(vi13)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi13 + 1);
  // lq.xyzw vf26, 1(vi08)      |  madday.xyzw ACC, vf31, vf24
  vu.acc.madda_xyzw(vu.vf31, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi08 + 1);
  // lq.xyzw vf31, 2(vi10)      |  maddz.xyzw vf25, vf25, vf24
  vu.acc.madd_xyzw(vu.vf25, vu.vf25, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi10 + 2);
  // lq.xyzw vf23, 2(vi13)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi13 + 2);
  // lq.xyzw vf27, 2(vi08)      |  madday.xyzw ACC, vf20, vf24
  vu.acc.madda_xyzw(vu.vf20, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi08 + 2);
  // lq.xyzw vf20, 3(vi10)      |  maddz.xyzw vf26, vf26, vf24
  vu.acc.madd_xyzw(vu.vf26, vu.vf26, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10 + 3);
  // lq.xyzw vf31, 3(vi13)      |  mulax.xyzw ACC, vf31, vf24
  vu.acc.mula_xyzw(vu.vf31, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi13 + 3);
  // lq.xyzw vf28, 3(vi08)      |  madday.xyzw ACC, vf23, vf24
  vu.acc.madda_xyzw(vu.vf23, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi08 + 3);
  // lq.xyzw vf23, 4(vi10)      |  maddz.xyzw vf27, vf27, vf24
  vu.acc.madd_xyzw(vu.vf27, vu.vf27, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi10 + 4);
  // lq.xyzw vf20, 4(vi13)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi13 + 4);
  // lq.xyz vf29, 4(vi08)       |  madday.xyzw ACC, vf31, vf24
  vu.acc.madda_xyzw(vu.vf31, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi08 + 4);
  // lq.xyzw vf31, 5(vi10)      |  maddz.xyzw vf28, vf28, vf24
  vu.acc.madd_xyzw(vu.vf28, vu.vf28, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi10 + 5);
  // lq.xyzw vf23, 5(vi13)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi13 + 5);
  // lq.xyz vf30, 5(vi08)       |  madday.xyzw ACC, vf20, vf24
  vu.acc.madda_xyzw(vu.vf20, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi08 + 5);
  // lq.xyzw vf20, 6(vi10)      |  maddz.xyz vf29, vf29, vf24
  vu.acc.madd_xyz(vu.vf29, vu.vf29, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi10 + 6);
  // lq.xyzw vf22, 6(vi13)      |  mulax.xyzw ACC, vf31, vf24
  vu.acc.mula_xyzw(vu.vf31, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf22, vu.vi13 + 6);
  // lq.xyzw vf31, 6(vi08)      |  madday.xyzw ACC, vf23, vf24
  vu.acc.madda_xyzw(vu.vf23, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi08 + 6);
  // lqi.xyzw vf23, vi02        |  maddz.xyz vf30, vf30, vf24
  vu.acc.madd_xyz(vu.vf30, vu.vf30, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi02++);
  // mtir vi10, vf11.x          |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   vu.vi10 = vu.vf11.x_as_u16();
  // mtir vi13, vf11.y          |  madday.xyzw ACC, vf22, vf24
  vu.acc.madda_xyzw(vu.vf22, vu.vf24.y());   vu.vi13 = vu.vf11.y_as_u16();
  // lq.xyzw vf22, 2(vi00)      |  maddz.xyzw vf31, vf31, vf24
  vu.acc.madd_xyzw(vu.vf31, vu.vf31, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf22, 2);
  // lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
  vu.vf24.itof0(Mask::xyzw, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  L136:
// BRANCH!
  // ibgez vi09, L137           |  mulaz.xyzw ACC, vf29, vf08
  vu.acc.mula_xyzw(vu.vf29, vu.vf08.z());   bc = ((s16)vu.vi09) >= 0;
  // sq.xyzw vf21, 2(vi11)      |  maddaz.xyzw ACC, vf30, vf11
  vu.acc.madda_xyzw(vu.vf30, vu.vf11.z());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi11 + 2);
  if (bc) { goto L137; }


  // nop                        |  ftoi4.xyzw vf21, vf09
  vu.vf21.ftoi4(Mask::xyzw, vu.vf09);
  L137:
  // mfp.w vf20, P              |  maddz.xyz vf11, vf31, vf14
  vu.acc.madd_xyz(vu.vf11, vu.vf31, vu.vf14.z());   vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17
  vu.vf12.mini_xyzw(vu.vf12, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi11);
  // sq.xyzw vf15, 0(vi14)      |  miniw.w vf10, vf10, vf03
  vu.vf10.mini(Mask::w, vu.vf10, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi14);
  // sq.xyzw vf21, 2(vi14)      |  mulaw.xyzw ACC, vf25, vf08
  vu.acc.mula_xyzw(vu.vf25, vu.vf08.w());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi14 + 2);
  // ilw.y vi09, -6(vi01)       |  mulw.xyzw vf13, vf13, vf20
  vu.vf13.mul_xyzw(vu.vf13, vu.vf20.w());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  // 1024.0                     |  ftoi0.xyzw vf12, vf12 :i
  vu.vf12.ftoi0(Mask::xyzw, vu.vf12);   vu.I = 1024.0;
  // erleng.xyz P, vf11         |  maxi.xy vf10, vf10, I
  vu.vf10.maxi(Mask::xy, vu.vf10, vu.I);   vu.P = erleng(vu.vf11); /* TODO erleng */
// BRANCH!
  // ibeq vi07, vi03, L153      |  maddaw.xyzw ACC, vf26, vf11
  vu.acc.madda_xyzw(vu.vf26, vu.vf11.w());   bc = (vu.vi07 == vu.vi03);
  // mr32.z vf14, vf00          |  maddw.xyzw vf08, vf27, vf14
  vu.acc.madd_xyzw(vu.vf08, vu.vf27, vu.vf14.w());   vu.vf14.z() = 1.f;
  if (bc) { goto L153; }


  L138:
  // 3072.0                     |  mulax.xyzw ACC, vf01, vf13 :i
  vu.acc.mula_xyzw(vu.vf01, vu.vf13.x());   vu.I = 3072.0;
  // lqi.xyzw vf09, vi01        |  minii.xy vf10, vf10, I
  vu.vf10.minii(Mask::xy, vu.vf10, vu.I);   lq_buffer_xyzw<DEBUG>(vu.vf09, vu.vi01++);
  // sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13
  vu.acc.madda_xyzw(vu.vf02, vu.vf13.y());   sq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi11 + 1);
  // sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13
  vu.acc.madd_xyzw(vu.vf13, vu.vf03, vu.vf13.z());   sq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi14 + 1);
  // lqi.xyzw vf12, vi01        |  add.xyzw vf08, vf08, vf28
  vu.vf08.add_xyzw(vu.vf08, vu.vf28);   lq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi01++);
  // lqi.xyzw vf15, vi01        |  maxw.w vf10, vf10, vf02
  vu.vf10.max(Mask::w, vu.vf10, vu.vf02.w());   lq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi01++);
  // mtir vi11, vf09.x          |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   vu.vi11 = vu.vf09.x_as_u16();
  // mtir vi14, vf09.y          |  maxx.xyzw vf13, vf13, vf00
  vu.vf13.max_xyzw(vu.vf13, vu.vf00.x());   vu.vi14 = vu.vf09.y_as_u16();
  // nop                        |  miniw.w vf08, vf08, vf01
  vu.vf08.mini(Mask::w, vu.vf08, vu.vf01.w());
  // div Q, vf01.w, vf08.w      |  add.zw vf09, vf09, vf17
  vu.vf09.add(Mask::zw, vu.vf09, vu.vf17);   vu.Q = vu.vf01.w() / vu.vf08.w();

  // move.xyzw vf21, vf10       |  add.xyzw vf12, vf12, vf18
  vu.vf12.add_xyzw(vu.vf12, vu.vf18);   vu.vf21.move_xyzw(vu.vf10);
  // iand vi11, vi11, vi05      |  add.xyzw vf15, vf15, vf19
  vu.vf15.add_xyzw(vu.vf15, vu.vf19);   vu.vi11 = vu.vi11 & vu.vi05;
  // ilw.w vi08, -1(vi02)       |  mulax.xyzw ACC, vf04, vf13
  vu.acc.mula_xyzw(vu.vf04, vu.vf13.x());   ilw_buffer(Mask::w, vu.vi08, vu.vi02 + -1);
// BRANCH!
  // ibgtz vi09, L139           |  madday.xyzw ACC, vf05, vf13
  vu.acc.madda_xyzw(vu.vf05, vu.vf13.y());   bc = ((s16)vu.vi09) > 0;
  // iand vi14, vi14, vi05      |  maddaz.xyzw ACC, vf06, vf13
  vu.acc.madda_xyzw(vu.vf06, vu.vf13.z());   vu.vi14 = vu.vi14 & vu.vi05;
  if (bc) { goto L139; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L139:
// BRANCH!
  // ibne vi05, vi11, L140      |  maddw.xyzw vf13, vf07, vf00
  vu.acc.madd_xyzw(vu.vf13, vu.vf07, vu.vf00.w());   bc = (vu.vi05 != vu.vi11);
  // ilw.x vi09, -9(vi01)       |  mul.xyz vf08, vf08, Q
  vu.vf08.mul(Mask::xyz, vu.vf08, vu.Q); /* TODO mulq */   ilw_buffer(Mask::x, vu.vi09, vu.vi01 + -9);
  if (bc) { goto L140; }


  // mtir vi11, vf12.x          |  mul.xyzw vf14, vf14, Q
  vu.vf14.mul_xyzw(vu.vf14, vu.Q); /* TODO mulq */   vu.vi11 = vu.vf12.x_as_u16();
  // mtir vi14, vf12.y          |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi14 = vu.vf12.y_as_u16();
// BRANCH!
  // b L141                     |  mul.xyzw vf13, vf13, vf23
  vu.vf13.mul_xyzw(vu.vf13, vu.vf23);   bc = true;
  // lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
  vu.vf08.add_xyzw(vu.vf08, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  if (bc) { goto L141; }


  L140:
  // lq.xyzw vf20, 0(vi11)      |  mul.xyzw vf14, vf14, Q
  vu.vf14.mul_xyzw(vu.vf14, vu.Q); /* TODO mulq */   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11);
  // nop                        |  mulw.xyzw vf24, vf24, vf29
  vu.vf24.mul_xyzw(vu.vf24, vu.vf29.w());
  // lq.xyzw vf31, 0(vi14)      |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi14);
  // lq.xyzw vf25, 0(vi08)      |  mul.xyzw vf13, vf13, vf23
  vu.vf13.mul_xyzw(vu.vf13, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf25, vu.vi08);
  // lq.xyzw vf23, 1(vi11)      |  add.xyzw vf08, vf08, vf22
  vu.vf08.add_xyzw(vu.vf08, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi11 + 1);
  // lq.xyzw vf20, 1(vi14)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi14 + 1);
  // lq.xyzw vf26, 1(vi08)      |  madday.xyzw ACC, vf31, vf24
  vu.acc.madda_xyzw(vu.vf31, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf26, vu.vi08 + 1);
  // lq.xyzw vf31, 2(vi11)      |  maddz.xyzw vf25, vf25, vf24
  vu.acc.madd_xyzw(vu.vf25, vu.vf25, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi11 + 2);
  // lq.xyzw vf23, 2(vi14)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi14 + 2);
  // lq.xyzw vf27, 2(vi08)      |  madday.xyzw ACC, vf20, vf24
  vu.acc.madda_xyzw(vu.vf20, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi08 + 2);
  // lq.xyzw vf20, 3(vi11)      |  maddz.xyzw vf26, vf26, vf24
  vu.acc.madd_xyzw(vu.vf26, vu.vf26, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11 + 3);
  // lq.xyzw vf31, 3(vi14)      |  mulax.xyzw ACC, vf31, vf24
  vu.acc.mula_xyzw(vu.vf31, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi14 + 3);
  // lq.xyzw vf28, 3(vi08)      |  madday.xyzw ACC, vf23, vf24
  vu.acc.madda_xyzw(vu.vf23, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf28, vu.vi08 + 3);
  // lq.xyzw vf23, 4(vi11)      |  maddz.xyzw vf27, vf27, vf24
  vu.acc.madd_xyzw(vu.vf27, vu.vf27, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi11 + 4);
  // lq.xyzw vf20, 4(vi14)      |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi14 + 4);
  // lq.xyz vf29, 4(vi08)       |  madday.xyzw ACC, vf31, vf24
  vu.acc.madda_xyzw(vu.vf31, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf29, vu.vi08 + 4);
  // lq.xyzw vf31, 5(vi11)      |  maddz.xyzw vf28, vf28, vf24
  vu.acc.madd_xyzw(vu.vf28, vu.vf28, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi11 + 5);
  // lq.xyzw vf23, 5(vi14)      |  mulax.xyzw ACC, vf23, vf24
  vu.acc.mula_xyzw(vu.vf23, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi14 + 5);
  // lq.xyz vf30, 5(vi08)       |  madday.xyzw ACC, vf20, vf24
  vu.acc.madda_xyzw(vu.vf20, vu.vf24.y());   lq_buffer_xyz<DEBUG>(vu.vf30, vu.vi08 + 5);
  // lq.xyzw vf20, 6(vi11)      |  maddz.xyz vf29, vf29, vf24
  vu.acc.madd_xyz(vu.vf29, vu.vf29, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf20, vu.vi11 + 6);
  // lq.xyzw vf22, 6(vi14)      |  mulax.xyzw ACC, vf31, vf24
  vu.acc.mula_xyzw(vu.vf31, vu.vf24.x());   lq_buffer_xyzw<DEBUG>(vu.vf22, vu.vi14 + 6);
  // lq.xyzw vf31, 6(vi08)      |  madday.xyzw ACC, vf23, vf24
  vu.acc.madda_xyzw(vu.vf23, vu.vf24.y());   lq_buffer_xyzw<DEBUG>(vu.vf31, vu.vi08 + 6);
  // lqi.xyzw vf23, vi02        |  maddz.xyz vf30, vf30, vf24
  vu.acc.madd_xyz(vu.vf30, vu.vf30, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi02++);
  // mtir vi11, vf12.x          |  mulax.xyzw ACC, vf20, vf24
  vu.acc.mula_xyzw(vu.vf20, vu.vf24.x());   vu.vi11 = vu.vf12.x_as_u16();
  // mtir vi14, vf12.y          |  madday.xyzw ACC, vf22, vf24
  vu.acc.madda_xyzw(vu.vf22, vu.vf24.y());   vu.vi14 = vu.vf12.y_as_u16();
  // lq.xyzw vf22, 2(vi00)      |  maddz.xyzw vf31, vf31, vf24
  vu.acc.madd_xyzw(vu.vf31, vu.vf31, vu.vf24.z());   lq_buffer_xyzw<DEBUG>(vu.vf22, 2);
  // lqi.xyzw vf23, vi03        |  itof0.xyzw vf24, vf23
  vu.vf24.itof0(Mask::xyzw, vu.vf23);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
  L141:
// BRANCH!
  // ibgez vi09, L142           |  mulaz.xyzw ACC, vf29, vf09
  vu.acc.mula_xyzw(vu.vf29, vu.vf09.z());   bc = ((s16)vu.vi09) >= 0;
  // sq.xyzw vf21, 2(vi12)      |  maddaz.xyzw ACC, vf30, vf12
  vu.acc.madda_xyzw(vu.vf30, vu.vf12.z());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi12 + 2);
  if (bc) { goto L142; }


  // nop                        |  ftoi4.xyzw vf21, vf10
  vu.vf21.ftoi4(Mask::xyzw, vu.vf10);
  L142:
  // mfp.w vf20, P              |  maddz.xyz vf12, vf31, vf15
  vu.acc.madd_xyz(vu.vf12, vu.vf31, vu.vf15.z());   vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  vu.vf13.mini_xyzw(vu.vf13, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi12);
  // sq.xyzw vf16, 0(vi15)      |  miniw.w vf08, vf08, vf03
  vu.vf08.mini(Mask::w, vu.vf08, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi15);
  // sq.xyzw vf21, 2(vi15)      |  mulaw.xyzw ACC, vf25, vf09
  vu.acc.mula_xyzw(vu.vf25, vu.vf09.w());   sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi15 + 2);
  // ilw.y vi09, -6(vi01)       |  mulw.xyzw vf11, vf11, vf20
  vu.vf11.mul_xyzw(vu.vf11, vu.vf20.w());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -6);
  // 1024.0                     |  ftoi0.xyzw vf13, vf13 :i
  vu.vf13.ftoi0(Mask::xyzw, vu.vf13);   vu.I = 1024.0;
  // erleng.xyz P, vf12         |  maxi.xy vf08, vf08, I
  vu.vf08.maxi(Mask::xy, vu.vf08, vu.I);   vu.P = erleng(vu.vf12); /* TODO erleng */
// BRANCH!
  // ibne vi07, vi03, L127      |  maddaw.xyzw ACC, vf26, vf12
  vu.acc.madda_xyzw(vu.vf26, vu.vf12.w());   bc = (vu.vi07 != vu.vi03);
  // mr32.z vf15, vf00          |  maddw.xyzw vf09, vf27, vf15
  vu.acc.madd_xyzw(vu.vf09, vu.vf27, vu.vf15.w());   vu.vf15.z() = 1.f;
  if (bc) { goto L127; }


// BRANCH!
  // b L163                     |  nop
  bc = true;
  // nop                        |  nop

  if (bc) { goto L163; }


  L143:
  // ilw.w vi08, 1(vi00)        |  nop
  ilw_buffer(Mask::w, vu.vi08, 1);
  // xtop vi02                  |  mulax.xyzw ACC, vf01, vf12
  vu.acc.mula_xyzw(vu.vf01, vu.vf12.x());   vu.vi02 = xtop();
  // sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12
  vu.acc.madda_xyzw(vu.vf02, vu.vf12.y());   sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi10 + 1);
  // sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12
  vu.acc.madd_xyzw(vu.vf12, vu.vf03, vu.vf12.z());   sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi13 + 1);
  // iaddiu vi04, vi02, 0x8c    |  add.xyzw vf10, vf10, vf28
  vu.vf10.add_xyzw(vu.vf10, vu.vf28);   vu.vi04 = vu.vi02 + 0x8c; /* 140 */
  // ilwr.x vi05, vi04          |  maxw.w vf09, vf09, vf02
  vu.vf09.max(Mask::w, vu.vf09, vu.vf02.w());   ilw_buffer(Mask::x, vu.vi05, vu.vi04);
  // ilw.w vi06, 1(vi04)        |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   ilw_buffer(Mask::w, vu.vi06, vu.vi04 + 1);
// BRANCH!
  // ibne vi00, vi08, L151      |  nop
  bc = (vu.vi08 != 0);
  // ilw.x vi07, 2(vi04)        |  maxx.xyzw vf12, vf12, vf00
  vu.vf12.max_xyzw(vu.vf12, vu.vf00.x());   ilw_buffer(Mask::x, vu.vi07, vu.vi04 + 2);
  if (bc) { goto L151; }


  L144:
  // div Q, vf01.w, vf10.w      |  minix.xyzw vf25, vf00, vf00
  vu.vf25.mini_xyzw(vu.vf00, vu.vf00.x());   vu.Q = vu.vf01.w() / vu.vf10.w();

  // move.xyzw vf21, vf09       |  minix.xyzw vf26, vf00, vf00
  vu.vf26.mini_xyzw(vu.vf00, vu.vf00.x());   vu.vf21.move_xyzw(vu.vf09);
  // iadd vi05, vi05, vi04      |  nop
  vu.vi05 = vu.vi05 + vu.vi04;
  // iaddiu vi04, vi02, 0x173   |  mulax.xyzw ACC, vf04, vf12
  vu.acc.mula_xyzw(vu.vf04, vu.vf12.x());   vu.vi04 = vu.vi02 + 0x173; /* 371 */
// BRANCH!
  // ibgtz vi09, L145           |  madday.xyzw ACC, vf05, vf12
  vu.acc.madda_xyzw(vu.vf05, vu.vf12.y());   bc = ((s16)vu.vi09) > 0;
  // iadd vi06, vi06, vi05      |  maddaz.xyzw ACC, vf06, vf12
  vu.acc.madda_xyzw(vu.vf06, vu.vf12.z());   vu.vi06 = vu.vi06 + vu.vi05;
  if (bc) { goto L145; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L145:
  // iadd vi07, vi07, vi06      |  maddw.xyzw vf12, vf07, vf00
  vu.acc.madd_xyzw(vu.vf12, vu.vf07, vu.vf00.w());   vu.vi07 = vu.vi07 + vu.vi06;
  // ilw.x vi09, -6(vi01)       |  mul.xyz vf10, vf10, Q
  vu.vf10.mul(Mask::xyz, vu.vf10, vu.Q); /* TODO mulq */   ilw_buffer(Mask::x, vu.vi09, vu.vi01 + -6);
  // iaddiu vi08, vi00, 0x1ba   |  mul.xyzw vf16, vf16, Q
  vu.vf16.mul_xyzw(vu.vf16, vu.Q); /* TODO mulq */   vu.vi08 = 0x1ba; /* 442 */

  // isub vi08, vi08, vi02      |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi08 = vu.vi08 - vu.vi02;
  // iaddiu vi08, vi08, 0x173   |  mul.xyzw vf12, vf12, vf23
  vu.vf12.mul_xyzw(vu.vf12, vu.vf23);   vu.vi08 = vu.vi08 + 0x173; /* 371 */
  // lqi.xyzw vf23, vi03        |  add.xyzw vf10, vf10, vf22
  vu.vf10.add_xyzw(vu.vf10, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
// BRANCH!
  // ibgez vi09, L146           |  nop
  bc = ((s16)vu.vi09) >= 0;
  // sq.xyzw vf21, 2(vi11)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi11 + 2);
  if (bc) { goto L146; }


  // nop                        |  ftoi4.xyzw vf21, vf09
  vu.vf21.ftoi4(Mask::xyzw, vu.vf09);
  L146:
  // mfp.w vf20, P              |  nop
  vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17
  vu.vf12.mini_xyzw(vu.vf12, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi11);
  // sq.xyzw vf15, 0(vi14)      |  miniw.w vf10, vf10, vf03
  vu.vf10.mini(Mask::w, vu.vf10, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi14);
  // sq.xyzw vf21, 2(vi14)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi14 + 2);
  // ilw.y vi09, -3(vi01)       |  mulw.xyzw vf13, vf13, vf20
  vu.vf13.mul_xyzw(vu.vf13, vu.vf20.w());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -3);
  // mfir.x vf25, vi04          |  ftoi0.xyzw vf12, vf12
  vu.vf12.ftoi0(Mask::xyzw, vu.vf12);   vu.vf25.mfir(Mask::x, vu.vi04);
  // mfir.y vf25, vi04          |  nop
  vu.vf25.mfir(Mask::y, vu.vi04);
  // mfir.x vf26, vi08          |  nop
  vu.vf26.mfir(Mask::x, vu.vi08);
  // ilw.w vi02, 1(vi00)        |  nop
  ilw_buffer(Mask::w, vu.vi02, 1);
  // mfir.y vf26, vi04          |  mulax.xyzw ACC, vf01, vf13
  vu.acc.mula_xyzw(vu.vf01, vu.vf13.x());   vu.vf26.mfir(Mask::y, vu.vi04);
  // sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13
  vu.acc.madda_xyzw(vu.vf02, vu.vf13.y());   sq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi11 + 1);
  // sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13
  vu.acc.madd_xyzw(vu.vf13, vu.vf03, vu.vf13.z());   sq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi14 + 1);
  // nop                        |  nop

// BRANCH!
  // ibne vi00, vi02, L152      |  maxw.w vf10, vf10, vf02
  vu.vf10.max(Mask::w, vu.vf10, vu.vf02.w());   bc = (vu.vi02 != 0);
  // nop                        |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);
  if (bc) { goto L152; }


  L147:
  // 8388608.0                  |  maxx.xyzw vf13, vf13, vf00 :i
  vu.vf13.max_xyzw(vu.vf13, vu.vf00.x());   vu.I = 8388608.0;
  // 256.0                      |  maxi.xy vf27, vf00, I :i
  vu.vf27.maxi(Mask::xy, vu.vf00, vu.I);   vu.I = 256.0;
  // move.xyzw vf21, vf10       |  maxi.w vf27, vf00, I
  vu.vf27.maxi(Mask::w, vu.vf00, vu.I);   vu.vf21.move_xyzw(vu.vf10);
  // nop                        |  nop

  // nop                        |  mulax.xyzw ACC, vf04, vf13
  vu.acc.mula_xyzw(vu.vf04, vu.vf13.x());
// BRANCH!
  // ibgtz vi09, L148           |  madday.xyzw ACC, vf05, vf13
  vu.acc.madda_xyzw(vu.vf05, vu.vf13.y());   bc = ((s16)vu.vi09) > 0;
  // nop                        |  maddaz.xyzw ACC, vf06, vf13
  vu.acc.madda_xyzw(vu.vf06, vu.vf13.z());
  if (bc) { goto L148; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L148:
  // nop                        |  maddw.xyzw vf13, vf07, vf00
  vu.acc.madd_xyzw(vu.vf13, vu.vf07, vu.vf00.w());
  // ilw.x vi09, -3(vi01)       |  itof0.xyzw vf25, vf25
  vu.vf25.itof0(Mask::xyzw, vu.vf25);   ilw_buffer(Mask::x, vu.vi09, vu.vi01 + -3);
  // nop                        |  itof0.xyzw vf26, vf26
  vu.vf26.itof0(Mask::xyzw, vu.vf26);
  // nop                        |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);
  // nop                        |  mul.xyzw vf13, vf13, vf23
  vu.vf13.mul_xyzw(vu.vf13, vu.vf23);
  // ior vi02, vi05, vi00       |  add.xyzw vf25, vf25, vf27
  vu.vf25.add_xyzw(vu.vf25, vu.vf27);   vu.vi02 = vu.vi05;
// BRANCH!
  // ibgez vi09, L149           |  add.xyzw vf26, vf26, vf27
  vu.vf26.add_xyzw(vu.vf26, vu.vf27);   bc = ((s16)vu.vi09) >= 0;
  // sq.xyzw vf21, 2(vi12)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi12 + 2);
  if (bc) { goto L149; }


  // nop                        |  ftoi4.xyzw vf21, vf10
  vu.vf21.ftoi4(Mask::xyzw, vu.vf10);
  L149:
// BRANCH!
  // ibne vi06, vi05, L150      |  nop
  bc = (vu.vi06 != vu.vi05);
  // sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  vu.vf13.mini_xyzw(vu.vf13, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi12);
  if (bc) { goto L150; }


  // ior vi06, vi07, vi00       |  max.xyzw vf25, vf26, vf26
  vu.vf25.max_xyzw(vu.vf26, vu.vf26);   vu.vi06 = vu.vi07;
  L150:
  // sq.xyzw vf16, 0(vi15)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi15);
  // sq.xyzw vf21, 2(vi15)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi15 + 2);
  // lqi.xyzw vf27, vi05        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi05++);
  // nop                        |  ftoi0.xyzw vf13, vf13
  vu.vf13.ftoi0(Mask::xyzw, vu.vf13);
  // nop                        |  nop

  // nop                        |  nop

  // nop                        |  itof0.xyzw vf27, vf27
  vu.vf27.itof0(Mask::xyzw, vu.vf27);
  // sq.xyzw vf13, 1(vi12)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi12 + 1);
// BRANCH!
  // b L173                     |  nop
  bc = true;
  // sq.xyzw vf13, 1(vi15)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi15 + 1);
  if (bc) { goto L173; }


  L151:
  // 3072.0                     |  miniw.w vf10, vf10, vf01 :i
  vu.vf10.mini(Mask::w, vu.vf10, vu.vf01.w());   vu.I = 3072.0;
// BRANCH!
  // b L144                     |  minii.xy vf09, vf09, I
  vu.vf09.minii(Mask::xy, vu.vf09, vu.I);   bc = true;
  // nop                        |  nop

  if (bc) { goto L144; }


  L152:
  // 1024.0                     |  nop :i
  vu.I = 1024.0;
  // 3072.0                     |  maxi.xy vf10, vf10, I :i
  vu.vf10.maxi(Mask::xy, vu.vf10, vu.I);   vu.I = 3072.0;
// BRANCH!
  // b L147                     |  minii.xy vf10, vf10, I
  vu.vf10.minii(Mask::xy, vu.vf10, vu.I);   bc = true;
  // isw.w vi00, 1(vi00)        |  nop
  isw_buffer(Mask::w, vu.vi00, 1);
  if (bc) { goto L147; }


  L153:
  // ilw.w vi08, 1(vi00)        |  nop
  ilw_buffer(Mask::w, vu.vi08, 1);
  // xtop vi02                  |  mulax.xyzw ACC, vf01, vf13
  vu.acc.mula_xyzw(vu.vf01, vu.vf13.x());   vu.vi02 = xtop();
  // sq.xyzw vf12, 1(vi11)      |  madday.xyzw ACC, vf02, vf13
  vu.acc.madda_xyzw(vu.vf02, vu.vf13.y());   sq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi11 + 1);
  // sq.xyzw vf12, 1(vi14)      |  maddz.xyzw vf13, vf03, vf13
  vu.acc.madd_xyzw(vu.vf13, vu.vf03, vu.vf13.z());   sq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi14 + 1);
  // iaddiu vi04, vi02, 0x8c    |  add.xyzw vf08, vf08, vf28
  vu.vf08.add_xyzw(vu.vf08, vu.vf28);   vu.vi04 = vu.vi02 + 0x8c; /* 140 */
  // ilwr.x vi05, vi04          |  maxw.w vf10, vf10, vf02
  vu.vf10.max(Mask::w, vu.vf10, vu.vf02.w());   ilw_buffer(Mask::x, vu.vi05, vu.vi04);
  // ilw.w vi06, 1(vi04)        |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   ilw_buffer(Mask::w, vu.vi06, vu.vi04 + 1);
// BRANCH!
  // ibne vi00, vi08, L161      |  nop
  bc = (vu.vi08 != 0);
  // ilw.x vi07, 2(vi04)        |  maxx.xyzw vf13, vf13, vf00
  vu.vf13.max_xyzw(vu.vf13, vu.vf00.x());   ilw_buffer(Mask::x, vu.vi07, vu.vi04 + 2);
  if (bc) { goto L161; }


  L154:
  // div Q, vf01.w, vf08.w      |  minix.xyzw vf25, vf00, vf00
  vu.vf25.mini_xyzw(vu.vf00, vu.vf00.x());   vu.Q = vu.vf01.w() / vu.vf08.w();

  // move.xyzw vf21, vf10       |  minix.xyzw vf26, vf00, vf00
  vu.vf26.mini_xyzw(vu.vf00, vu.vf00.x());   vu.vf21.move_xyzw(vu.vf10);
  // iadd vi05, vi05, vi04      |  nop
  vu.vi05 = vu.vi05 + vu.vi04;
  // iaddiu vi04, vi02, 0x173   |  mulax.xyzw ACC, vf04, vf13
  vu.acc.mula_xyzw(vu.vf04, vu.vf13.x());   vu.vi04 = vu.vi02 + 0x173; /* 371 */
// BRANCH!
  // ibgtz vi09, L155           |  madday.xyzw ACC, vf05, vf13
  vu.acc.madda_xyzw(vu.vf05, vu.vf13.y());   bc = ((s16)vu.vi09) > 0;
  // iadd vi06, vi06, vi05      |  maddaz.xyzw ACC, vf06, vf13
  vu.acc.madda_xyzw(vu.vf06, vu.vf13.z());   vu.vi06 = vu.vi06 + vu.vi05;
  if (bc) { goto L155; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L155:
  // iadd vi07, vi07, vi06      |  maddw.xyzw vf13, vf07, vf00
  vu.acc.madd_xyzw(vu.vf13, vu.vf07, vu.vf00.w());   vu.vi07 = vu.vi07 + vu.vi06;
  // ilw.x vi09, -6(vi01)       |  mul.xyz vf08, vf08, Q
  vu.vf08.mul(Mask::xyz, vu.vf08, vu.Q); /* TODO mulq */   ilw_buffer(Mask::x, vu.vi09, vu.vi01 + -6);
  // iaddiu vi08, vi00, 0x1ba   |  mul.xyzw vf14, vf14, Q
  vu.vf14.mul_xyzw(vu.vf14, vu.Q); /* TODO mulq */   vu.vi08 = 0x1ba; /* 442 */

  // isub vi08, vi08, vi02      |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi08 = vu.vi08 - vu.vi02;
  // iaddiu vi08, vi08, 0x173   |  mul.xyzw vf13, vf13, vf23
  vu.vf13.mul_xyzw(vu.vf13, vu.vf23);   vu.vi08 = vu.vi08 + 0x173; /* 371 */
  // lqi.xyzw vf23, vi03        |  add.xyzw vf08, vf08, vf22
  vu.vf08.add_xyzw(vu.vf08, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
// BRANCH!
  // ibgez vi09, L156           |  nop
  bc = ((s16)vu.vi09) >= 0;
  // sq.xyzw vf21, 2(vi12)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi12 + 2);
  if (bc) { goto L156; }


  // nop                        |  ftoi4.xyzw vf21, vf10
  vu.vf21.ftoi4(Mask::xyzw, vu.vf10);
  L156:
  // mfp.w vf20, P              |  nop
  vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf16, 0(vi12)      |  miniy.xyzw vf13, vf13, vf17
  vu.vf13.mini_xyzw(vu.vf13, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi12);
  // sq.xyzw vf16, 0(vi15)      |  miniw.w vf08, vf08, vf03
  vu.vf08.mini(Mask::w, vu.vf08, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi15);
  // sq.xyzw vf21, 2(vi15)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi15 + 2);
  // ilw.y vi09, -3(vi01)       |  mulw.xyzw vf11, vf11, vf20
  vu.vf11.mul_xyzw(vu.vf11, vu.vf20.w());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -3);
  // mfir.x vf25, vi04          |  ftoi0.xyzw vf13, vf13
  vu.vf13.ftoi0(Mask::xyzw, vu.vf13);   vu.vf25.mfir(Mask::x, vu.vi04);
  // mfir.y vf25, vi04          |  nop
  vu.vf25.mfir(Mask::y, vu.vi04);
  // mfir.x vf26, vi08          |  nop
  vu.vf26.mfir(Mask::x, vu.vi08);
  // ilw.w vi02, 1(vi00)        |  nop
  ilw_buffer(Mask::w, vu.vi02, 1);
  // mfir.y vf26, vi04          |  mulax.xyzw ACC, vf01, vf11
  vu.acc.mula_xyzw(vu.vf01, vu.vf11.x());   vu.vf26.mfir(Mask::y, vu.vi04);
  // sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  vu.acc.madda_xyzw(vu.vf02, vu.vf11.y());   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi12 + 1);
  // sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
  vu.acc.madd_xyzw(vu.vf11, vu.vf03, vu.vf11.z());   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi15 + 1);
  // nop                        |  nop

// BRANCH!
  // ibne vi00, vi02, L162      |  maxw.w vf08, vf08, vf02
  vu.vf08.max(Mask::w, vu.vf08, vu.vf02.w());   bc = (vu.vi02 != 0);
  // nop                        |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);
  if (bc) { goto L162; }


  L157:
  // 8388608.0                  |  maxx.xyzw vf11, vf11, vf00 :i
  vu.vf11.max_xyzw(vu.vf11, vu.vf00.x());   vu.I = 8388608.0;
  // 256.0                      |  maxi.xy vf27, vf00, I :i
  vu.vf27.maxi(Mask::xy, vu.vf00, vu.I);   vu.I = 256.0;
  // move.xyzw vf21, vf08       |  maxi.w vf27, vf00, I
  vu.vf27.maxi(Mask::w, vu.vf00, vu.I);   vu.vf21.move_xyzw(vu.vf08);
  // nop                        |  nop

  // nop                        |  mulax.xyzw ACC, vf04, vf11
  vu.acc.mula_xyzw(vu.vf04, vu.vf11.x());
// BRANCH!
  // ibgtz vi09, L158           |  madday.xyzw ACC, vf05, vf11
  vu.acc.madda_xyzw(vu.vf05, vu.vf11.y());   bc = ((s16)vu.vi09) > 0;
  // nop                        |  maddaz.xyzw ACC, vf06, vf11
  vu.acc.madda_xyzw(vu.vf06, vu.vf11.z());
  if (bc) { goto L158; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L158:
  // nop                        |  maddw.xyzw vf11, vf07, vf00
  vu.acc.madd_xyzw(vu.vf11, vu.vf07, vu.vf00.w());
  // ilw.x vi09, -3(vi01)       |  itof0.xyzw vf25, vf25
  vu.vf25.itof0(Mask::xyzw, vu.vf25);   ilw_buffer(Mask::x, vu.vi09, vu.vi01 + -3);
  // nop                        |  itof0.xyzw vf26, vf26
  vu.vf26.itof0(Mask::xyzw, vu.vf26);
  // nop                        |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);
  // nop                        |  mul.xyzw vf11, vf11, vf23
  vu.vf11.mul_xyzw(vu.vf11, vu.vf23);
  // ior vi02, vi05, vi00       |  add.xyzw vf25, vf25, vf27
  vu.vf25.add_xyzw(vu.vf25, vu.vf27);   vu.vi02 = vu.vi05;
// BRANCH!
  // ibgez vi09, L159           |  add.xyzw vf26, vf26, vf27
  vu.vf26.add_xyzw(vu.vf26, vu.vf27);   bc = ((s16)vu.vi09) >= 0;
  // sq.xyzw vf21, 2(vi10)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi10 + 2);
  if (bc) { goto L159; }


  // nop                        |  ftoi4.xyzw vf21, vf08
  vu.vf21.ftoi4(Mask::xyzw, vu.vf08);
  L159:
// BRANCH!
  // ibne vi06, vi05, L160      |  nop
  bc = (vu.vi06 != vu.vi05);
  // sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  vu.vf11.mini_xyzw(vu.vf11, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi10);
  if (bc) { goto L160; }


  // ior vi06, vi07, vi00       |  max.xyzw vf25, vf26, vf26
  vu.vf25.max_xyzw(vu.vf26, vu.vf26);   vu.vi06 = vu.vi07;
  L160:
  // sq.xyzw vf14, 0(vi13)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi13);
  // sq.xyzw vf21, 2(vi13)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi13 + 2);
  // lqi.xyzw vf27, vi05        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi05++);
  // nop                        |  ftoi0.xyzw vf11, vf11
  vu.vf11.ftoi0(Mask::xyzw, vu.vf11);
  // nop                        |  nop

  // nop                        |  nop

  // nop                        |  itof0.xyzw vf27, vf27
  vu.vf27.itof0(Mask::xyzw, vu.vf27);
  // sq.xyzw vf11, 1(vi10)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi10 + 1);
// BRANCH!
  // b L173                     |  nop
  bc = true;
  // sq.xyzw vf11, 1(vi13)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi13 + 1);
  if (bc) { goto L173; }


  L161:
  // 3072.0                     |  miniw.w vf08, vf08, vf01 :i
  vu.vf08.mini(Mask::w, vu.vf08, vu.vf01.w());   vu.I = 3072.0;
// BRANCH!
  // b L154                     |  minii.xy vf10, vf10, I
  vu.vf10.minii(Mask::xy, vu.vf10, vu.I);   bc = true;
  // nop                        |  nop

  if (bc) { goto L154; }


  L162:
  // 1024.0                     |  nop :i
  vu.I = 1024.0;
  // 3072.0                     |  maxi.xy vf08, vf08, I :i
  vu.vf08.maxi(Mask::xy, vu.vf08, vu.I);   vu.I = 3072.0;
// BRANCH!
  // b L157                     |  minii.xy vf08, vf08, I
  vu.vf08.minii(Mask::xy, vu.vf08, vu.I);   bc = true;
  // isw.w vi00, 1(vi00)        |  nop
  isw_buffer(Mask::w, vu.vi00, 1);
  if (bc) { goto L157; }


  L163:
  // ilw.w vi08, 1(vi00)        |  nop
  ilw_buffer(Mask::w, vu.vi08, 1);
  // xtop vi02                  |  mulax.xyzw ACC, vf01, vf11
  vu.acc.mula_xyzw(vu.vf01, vu.vf11.x());   vu.vi02 = xtop();
  // sq.xyzw vf13, 1(vi12)      |  madday.xyzw ACC, vf02, vf11
  vu.acc.madda_xyzw(vu.vf02, vu.vf11.y());   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi12 + 1);
  // sq.xyzw vf13, 1(vi15)      |  maddz.xyzw vf11, vf03, vf11
  vu.acc.madd_xyzw(vu.vf11, vu.vf03, vu.vf11.z());   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi15 + 1);
  // iaddiu vi04, vi02, 0x8c    |  add.xyzw vf09, vf09, vf28
  vu.vf09.add_xyzw(vu.vf09, vu.vf28);   vu.vi04 = vu.vi02 + 0x8c; /* 140 */
  // ilwr.x vi05, vi04          |  maxw.w vf08, vf08, vf02
  vu.vf08.max(Mask::w, vu.vf08, vu.vf02.w());   ilw_buffer(Mask::x, vu.vi05, vu.vi04);
  // ilw.w vi06, 1(vi04)        |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);   ilw_buffer(Mask::w, vu.vi06, vu.vi04 + 1);
// BRANCH!
  // ibne vi00, vi08, L171      |  nop
  bc = (vu.vi08 != 0);
  // ilw.x vi07, 2(vi04)        |  maxx.xyzw vf11, vf11, vf00
  vu.vf11.max_xyzw(vu.vf11, vu.vf00.x());   ilw_buffer(Mask::x, vu.vi07, vu.vi04 + 2);
  if (bc) { goto L171; }


  L164:
  // div Q, vf01.w, vf09.w      |  minix.xyzw vf25, vf00, vf00
  vu.vf25.mini_xyzw(vu.vf00, vu.vf00.x());   vu.Q = vu.vf01.w() / vu.vf09.w();

  // move.xyzw vf21, vf08       |  minix.xyzw vf26, vf00, vf00
  vu.vf26.mini_xyzw(vu.vf00, vu.vf00.x());   vu.vf21.move_xyzw(vu.vf08);
  // iadd vi05, vi05, vi04      |  nop
  vu.vi05 = vu.vi05 + vu.vi04;
  // iaddiu vi04, vi02, 0x173   |  mulax.xyzw ACC, vf04, vf11
  vu.acc.mula_xyzw(vu.vf04, vu.vf11.x());   vu.vi04 = vu.vi02 + 0x173; /* 371 */
// BRANCH!
  // ibgtz vi09, L165           |  madday.xyzw ACC, vf05, vf11
  vu.acc.madda_xyzw(vu.vf05, vu.vf11.y());   bc = ((s16)vu.vi09) > 0;
  // iadd vi06, vi06, vi05      |  maddaz.xyzw ACC, vf06, vf11
  vu.acc.madda_xyzw(vu.vf06, vu.vf11.z());   vu.vi06 = vu.vi06 + vu.vi05;
  if (bc) { goto L165; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L165:
  // iadd vi07, vi07, vi06      |  maddw.xyzw vf11, vf07, vf00
  vu.acc.madd_xyzw(vu.vf11, vu.vf07, vu.vf00.w());   vu.vi07 = vu.vi07 + vu.vi06;
  // ilw.x vi09, -6(vi01)       |  mul.xyz vf09, vf09, Q
  vu.vf09.mul(Mask::xyz, vu.vf09, vu.Q); /* TODO mulq */   ilw_buffer(Mask::x, vu.vi09, vu.vi01 + -6);
  // iaddiu vi08, vi00, 0x1ba   |  mul.xyzw vf15, vf15, Q
  vu.vf15.mul_xyzw(vu.vf15, vu.Q); /* TODO mulq */   vu.vi08 = 0x1ba; /* 442 */

  // isub vi08, vi08, vi02      |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);   vu.vi08 = vu.vi08 - vu.vi02;
  // iaddiu vi08, vi08, 0x173   |  mul.xyzw vf11, vf11, vf23
  vu.vf11.mul_xyzw(vu.vf11, vu.vf23);   vu.vi08 = vu.vi08 + 0x173; /* 371 */
  // lqi.xyzw vf23, vi03        |  add.xyzw vf09, vf09, vf22
  vu.vf09.add_xyzw(vu.vf09, vu.vf22);   lq_buffer_xyzw<DEBUG>(vu.vf23, vu.vi03++);
// BRANCH!
  // ibgez vi09, L166           |  nop
  bc = ((s16)vu.vi09) >= 0;
  // sq.xyzw vf21, 2(vi10)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi10 + 2);
  if (bc) { goto L166; }


  // nop                        |  ftoi4.xyzw vf21, vf08
  vu.vf21.ftoi4(Mask::xyzw, vu.vf08);
  L166:
  // mfp.w vf20, P              |  nop
  vu.vf20.mfp(Mask::w, vu.P);
  // sq.xyzw vf14, 0(vi10)      |  miniy.xyzw vf11, vf11, vf17
  vu.vf11.mini_xyzw(vu.vf11, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi10);
  // sq.xyzw vf14, 0(vi13)      |  miniw.w vf09, vf09, vf03
  vu.vf09.mini(Mask::w, vu.vf09, vu.vf03.w());   sq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi13);
  // sq.xyzw vf21, 2(vi13)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi13 + 2);
  // ilw.y vi09, -3(vi01)       |  mulw.xyzw vf12, vf12, vf20
  vu.vf12.mul_xyzw(vu.vf12, vu.vf20.w());   ilw_buffer(Mask::y, vu.vi09, vu.vi01 + -3);
  // mfir.x vf25, vi04          |  ftoi0.xyzw vf11, vf11
  vu.vf11.ftoi0(Mask::xyzw, vu.vf11);   vu.vf25.mfir(Mask::x, vu.vi04);
  // mfir.y vf25, vi04          |  nop
  vu.vf25.mfir(Mask::y, vu.vi04);
  // mfir.x vf26, vi08          |  nop
  vu.vf26.mfir(Mask::x, vu.vi08);
  // ilw.w vi02, 1(vi00)        |  nop
  ilw_buffer(Mask::w, vu.vi02, 1);
  // mfir.y vf26, vi04          |  mulax.xyzw ACC, vf01, vf12
  vu.acc.mula_xyzw(vu.vf01, vu.vf12.x());   vu.vf26.mfir(Mask::y, vu.vi04);
  // sq.xyzw vf11, 1(vi10)      |  madday.xyzw ACC, vf02, vf12
  vu.acc.madda_xyzw(vu.vf02, vu.vf12.y());   sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi10 + 1);
  // sq.xyzw vf11, 1(vi13)      |  maddz.xyzw vf12, vf03, vf12
  vu.acc.madd_xyzw(vu.vf12, vu.vf03, vu.vf12.z());   sq_buffer_xyzw<DEBUG>(vu.vf11, vu.vi13 + 1);
  // nop                        |  nop

// BRANCH!
  // ibne vi00, vi02, L172      |  maxw.w vf09, vf09, vf02
  vu.vf09.max(Mask::w, vu.vf09, vu.vf02.w());   bc = (vu.vi02 != 0);
  // nop                        |  itof0.xyzw vf23, vf23
  vu.vf23.itof0(Mask::xyzw, vu.vf23);
  if (bc) { goto L172; }


  L167:
  // 8388608.0                  |  maxx.xyzw vf12, vf12, vf00 :i
  vu.vf12.max_xyzw(vu.vf12, vu.vf00.x());   vu.I = 8388608.0;
  // 256.0                      |  maxi.xy vf27, vf00, I :i
  vu.vf27.maxi(Mask::xy, vu.vf00, vu.I);   vu.I = 256.0;
  // move.xyzw vf21, vf09       |  maxi.w vf27, vf00, I
  vu.vf27.maxi(Mask::w, vu.vf00, vu.I);   vu.vf21.move_xyzw(vu.vf09);
  // nop                        |  nop

  // nop                        |  mulax.xyzw ACC, vf04, vf12
  vu.acc.mula_xyzw(vu.vf04, vu.vf12.x());
// BRANCH!
  // ibgtz vi09, L168           |  madday.xyzw ACC, vf05, vf12
  vu.acc.madda_xyzw(vu.vf05, vu.vf12.y());   bc = ((s16)vu.vi09) > 0;
  // nop                        |  maddaz.xyzw ACC, vf06, vf12
  vu.acc.madda_xyzw(vu.vf06, vu.vf12.z());
  if (bc) { goto L168; }


  // nop                        |  addx.w vf21, vf21, vf17
  vu.vf21.add(Mask::w, vu.vf21, vu.vf17.x());
  L168:
  // nop                        |  maddw.xyzw vf12, vf07, vf00
  vu.acc.madd_xyzw(vu.vf12, vu.vf07, vu.vf00.w());
  // ilw.x vi09, -3(vi01)       |  itof0.xyzw vf25, vf25
  vu.vf25.itof0(Mask::xyzw, vu.vf25);   ilw_buffer(Mask::x, vu.vi09, vu.vi01 + -3);
  // nop                        |  itof0.xyzw vf26, vf26
  vu.vf26.itof0(Mask::xyzw, vu.vf26);
  // nop                        |  ftoi4.xyzw vf21, vf21
  vu.vf21.ftoi4(Mask::xyzw, vu.vf21);
  // nop                        |  mul.xyzw vf12, vf12, vf23
  vu.vf12.mul_xyzw(vu.vf12, vu.vf23);
  // ior vi02, vi05, vi00       |  add.xyzw vf25, vf25, vf27
  vu.vf25.add_xyzw(vu.vf25, vu.vf27);   vu.vi02 = vu.vi05;
// BRANCH!
  // ibgez vi09, L169           |  add.xyzw vf26, vf26, vf27
  vu.vf26.add_xyzw(vu.vf26, vu.vf27);   bc = ((s16)vu.vi09) >= 0;
  // sq.xyzw vf21, 2(vi11)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi11 + 2);
  if (bc) { goto L169; }


  // nop                        |  ftoi4.xyzw vf21, vf09
  vu.vf21.ftoi4(Mask::xyzw, vu.vf09);
  L169:
// BRANCH!
  // ibne vi06, vi05, L170      |  nop
  bc = (vu.vi06 != vu.vi05);
  // sq.xyzw vf15, 0(vi11)      |  miniy.xyzw vf12, vf12, vf17
  vu.vf12.mini_xyzw(vu.vf12, vu.vf17.y());   sq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi11);
  if (bc) { goto L170; }


  // ior vi06, vi07, vi00       |  max.xyzw vf25, vf26, vf26
  vu.vf25.max_xyzw(vu.vf26, vu.vf26);   vu.vi06 = vu.vi07;
  L170:
  // sq.xyzw vf15, 0(vi14)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf15, vu.vi14);
  // sq.xyzw vf21, 2(vi14)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf21, vu.vi14 + 2);
  // lqi.xyzw vf27, vi05        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi05++);
  // nop                        |  ftoi0.xyzw vf12, vf12
  vu.vf12.ftoi0(Mask::xyzw, vu.vf12);
  // nop                        |  nop

  // nop                        |  nop

  // nop                        |  itof0.xyzw vf27, vf27
  vu.vf27.itof0(Mask::xyzw, vu.vf27);
  // sq.xyzw vf12, 1(vi11)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi11 + 1);
// BRANCH!
  // b L173                     |  nop
  bc = true;
  // sq.xyzw vf12, 1(vi14)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi14 + 1);
  if (bc) { goto L173; }


  L171:
  // 3072.0                     |  miniw.w vf09, vf09, vf01 :i
  vu.vf09.mini(Mask::w, vu.vf09, vu.vf01.w());   vu.I = 3072.0;
// BRANCH!
  // b L164                     |  minii.xy vf08, vf08, I
  vu.vf08.minii(Mask::xy, vu.vf08, vu.I);   bc = true;
  // nop                        |  nop

  if (bc) { goto L164; }


  L172:
  // 1024.0                     |  nop :i
  vu.I = 1024.0;
  // 3072.0                     |  maxi.xy vf09, vf09, I :i
  vu.vf09.maxi(Mask::xy, vu.vf09, vu.I);   vu.I = 3072.0;
// BRANCH!
  // b L167                     |  minii.xy vf09, vf09, I
  vu.vf09.minii(Mask::xy, vu.vf09, vu.I);   bc = true;
  // isw.w vi00, 1(vi00)        |  nop
  isw_buffer(Mask::w, vu.vi00, 1);
  if (bc) { goto L167; }


  L173:
// BRANCH!
  // ibeq vi07, vi02, L179      |  nop
  bc = (vu.vi07 == vu.vi02);
  // ilw.w vi15, 132(vi00)      |  nop
  vu.hack_old_vi15 = vu.vi15;
  ilw_buffer(Mask::w, vu.vi15, 132);
  if (bc) { goto L179; }


// BRANCH!
  // ibne vi06, vi05, L174      |  add.xyzw vf11, vf27, vf25
  vu.vf11.add_xyzw(vu.vf27, vu.vf25);   bc = (vu.vi06 != vu.vi05);
  // nop                        |  nop

  if (bc) { goto L174; }


// BRANCH!
  // ibne vi07, vi06, L174      |  nop
  bc = (vu.vi07 != vu.vi06);
  // ior vi06, vi07, vi00       |  max.xyzw vf25, vf26, vf26
  vu.vf25.max_xyzw(vu.vf26, vu.vf26);   vu.vi06 = vu.vi07;
  if (bc) { goto L174; }


  // nop                        |  nop

  // nop                        |  nop

  // mtir vi08, vf11.x          |  nop
  vu.vi08 = vu.vf11.x_as_u16();
  // mtir vi10, vf11.y          |  nop
  vu.vi10 = vu.vf11.y_as_u16();
  // nop                        |  nop

  // nop                        |  nop

  // lq.xyzw vf16, 2(vi08)      |  maxx.xyzw vf15, vf11, vf00
  vu.vf15.max_xyzw(vu.vf11, vu.vf00.x());   lq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi08 + 2);
  // lq.xyzw vf13, 0(vi08)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi08);
// BRANCH!
  // b L178                     |  nop
  bc = true;
  // nop                        |  nop

  if (bc) { goto L178; }


  L174:
  // lqi.xyzw vf27, vi05        |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi05++);
  // nop                        |  nop

  // mtir vi08, vf11.x          |  nop
  vu.vi08 = vu.vf11.x_as_u16();
  // mtir vi09, vf11.y          |  nop
  vu.vi09 = vu.vf11.y_as_u16();
  // nop                        |  itof0.xyzw vf27, vf27
  vu.vf27.itof0(Mask::xyzw, vu.vf27);
  // nop                        |  nop

  // lq.xyzw vf12, 2(vi08)      |  maxx.xyzw vf15, vf11, vf00
  vu.vf15.max_xyzw(vu.vf11, vu.vf00.x());   lq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi08 + 2);
  // lq.xyzw vf13, 0(vi08)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi08);
// BRANCH!
  // ibne vi06, vi05, L175      |  add.xyzw vf11, vf27, vf25
  vu.vf11.add_xyzw(vu.vf27, vu.vf25);   bc = (vu.vi06 != vu.vi05);
  // nop                        |  nop

  if (bc) { goto L175; }


// BRANCH!
  // ibeq vi07, vi06, L177      |  nop
  bc = (vu.vi07 == vu.vi06);
  // ior vi06, vi07, vi00       |  max.xyzw vf25, vf26, vf26
  vu.vf25.max_xyzw(vu.vf26, vu.vf26);   vu.vi06 = vu.vi07;
  if (bc) { goto L177; }


  L175:
  // lqi.xyzw vf27, vi05        |  itof15.w vf12, vf12
  vu.vf12.itof15(Mask::w, vu.vf12);   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi05++);
  // lq.xyzw vf14, 1(vi08)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi08 + 1);
  // mtir vi08, vf11.x          |  nop
  vu.vi08 = vu.vf11.x_as_u16();
  // mtir vi10, vf11.y          |  nop
  vu.vi10 = vu.vf11.y_as_u16();
  // sq.xyzw vf13, 0(vi09)      |  itof0.xyzw vf27, vf27
  vu.vf27.itof0(Mask::xyzw, vu.vf27);   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi09);
  // sq.xyzw vf14, 1(vi09)      |  add.w vf12, vf12, vf15
  vu.vf12.add(Mask::w, vu.vf12, vu.vf15);   sq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi09 + 1);
  // lq.xyzw vf16, 2(vi08)      |  maxx.xyzw vf15, vf11, vf00
  vu.vf15.max_xyzw(vu.vf11, vu.vf00.x());   lq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi08 + 2);
  // lq.xyzw vf13, 0(vi08)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi08);
// BRANCH!
  // ibne vi06, vi05, L176      |  add.xyzw vf11, vf27, vf25
  vu.vf11.add_xyzw(vu.vf27, vu.vf25);   bc = (vu.vi06 != vu.vi05);
  // sq.xyzw vf12, 2(vi09)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi09 + 2);
  if (bc) { goto L176; }


// BRANCH!
  // ibne vi07, vi06, L176      |  nop
  bc = (vu.vi07 != vu.vi06);
  // ior vi06, vi07, vi00       |  max.xyzw vf25, vf26, vf26
  vu.vf25.max_xyzw(vu.vf26, vu.vf26);   vu.vi06 = vu.vi07;
  if (bc) { goto L176; }


  // move.xyzw vf12, vf16       |  nop
  vu.vf12.move_xyzw(vu.vf16);
// BRANCH!
  // b L177                     |  nop
  bc = true;
  // ior vi09, vi10, vi00       |  nop
  vu.vi09 = vu.vi10;
  if (bc) { goto L177; }


  L176:
  // lqi.xyzw vf27, vi05        |  itof15.w vf16, vf16
  vu.vf16.itof15(Mask::w, vu.vf16);   lq_buffer_xyzw<DEBUG>(vu.vf27, vu.vi05++);
  // lq.xyzw vf14, 1(vi08)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi08 + 1);
  // mtir vi08, vf11.x          |  nop
  vu.vi08 = vu.vf11.x_as_u16();
  // mtir vi09, vf11.y          |  nop
  vu.vi09 = vu.vf11.y_as_u16();
  // sq.xyzw vf13, 0(vi10)      |  itof0.xyzw vf27, vf27
  vu.vf27.itof0(Mask::xyzw, vu.vf27);   sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi10);
  // sq.xyzw vf14, 1(vi10)      |  add.w vf16, vf16, vf15
  vu.vf16.add(Mask::w, vu.vf16, vu.vf15);   sq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi10 + 1);
  // lq.xyzw vf12, 2(vi08)      |  maxx.xyzw vf15, vf11, vf00
  vu.vf15.max_xyzw(vu.vf11, vu.vf00.x());   lq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi08 + 2);
  // lq.xyzw vf13, 0(vi08)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi08);
// BRANCH!
  // ibne vi06, vi05, L175      |  add.xyzw vf11, vf27, vf25
  vu.vf11.add_xyzw(vu.vf27, vu.vf25);   bc = (vu.vi06 != vu.vi05);
  // sq.xyzw vf16, 2(vi10)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi10 + 2);
  if (bc) { goto L175; }


// BRANCH!
  // ibne vi07, vi06, L175      |  nop
  bc = (vu.vi07 != vu.vi06);
  // ior vi06, vi07, vi00       |  max.xyzw vf25, vf26, vf26
  vu.vf25.max_xyzw(vu.vf26, vu.vf26);   vu.vi06 = vu.vi07;
  if (bc) { goto L175; }


  L177:
  // nop                        |  itof15.w vf12, vf12
  vu.vf12.itof15(Mask::w, vu.vf12);
  // lq.xyzw vf14, 1(vi08)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi08 + 1);
  // mtir vi08, vf11.x          |  nop
  vu.vi08 = vu.vf11.x_as_u16();
  // mtir vi10, vf11.y          |  nop
  vu.vi10 = vu.vf11.y_as_u16();
  // sq.xyzw vf13, 0(vi09)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi09);
  // sq.xyzw vf14, 1(vi09)      |  add.w vf12, vf12, vf15
  vu.vf12.add(Mask::w, vu.vf12, vu.vf15);   sq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi09 + 1);
  // lq.xyzw vf16, 2(vi08)      |  maxx.xyzw vf15, vf11, vf00
  vu.vf15.max_xyzw(vu.vf11, vu.vf00.x());   lq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi08 + 2);
  // lq.xyzw vf13, 0(vi08)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi08);
  // nop                        |  nop

  // sq.xyzw vf12, 2(vi09)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf12, vu.vi09 + 2);
  L178:
  // nop                        |  itof15.w vf16, vf16
  vu.vf16.itof15(Mask::w, vu.vf16);
  // lq.xyzw vf14, 1(vi08)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi08 + 1);
  // nop                        |  nop

  // nop                        |  nop

  // sq.xyzw vf13, 0(vi10)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf13, vu.vi10);
  // sq.xyzw vf14, 1(vi10)      |  add.w vf16, vf16, vf15
  vu.vf16.add(Mask::w, vu.vf16, vu.vf15);   sq_buffer_xyzw<DEBUG>(vu.vf14, vu.vi10 + 1);
  // nop                        |  nop

  // nop                        |  nop

  // nop                        |  nop

  // sq.xyzw vf16, 2(vi10)      |  nop
  sq_buffer_xyzw<DEBUG>(vu.vf16, vu.vi10 + 2);
  L179:
// BRANCH!
  // ibne vi00, vi15, L180      |  nop
  bc = (vu.vi15 != 0);
  // nop                        |  nop

  if (bc) { goto L180; }


  // xgkick vi04                |  nop
  xgkick(vu.vi04, render_state, prof);
  // nop                        |  nop :e
  return;

  // nop                        |  nop

  L180:
  // lq.xyzw vf20, 132(vi00)    |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf20, 132);
  // lq.xyzw vf21, 1(vi00)      |  nop
  lq_buffer_xyzw<DEBUG>(vu.vf21, 1);
  // iaddi vi01, vi00, 0x1      |  nop
  vu.vi01 = 1;
  // isw.x vi01, -2(vi04)       |  nop
  isw_buffer(Mask::x, vu.vi01, vu.vi04 + -2);
  // iaddiu vi02, vi00, 0x47    |  maxw.x vf20, vf00, vf20
  vu.vf20.max(Mask::x, vu.vf00, vu.vf20.w());   vu.vi02 = 0x47; /* 71 */

  // isw.z vi02, -1(vi04)       |  nop
  isw_buffer(Mask::z, vu.vi02, vu.vi04 + -1);
  // sq.yzw vf21, -2(vi04)      |  nop
  sq_buffer(Mask::yzw, vu.vf21, vu.vi04 + -2);
  // isw.w vi00, 132(vi00)      |  nop
  isw_buffer(Mask::w, vu.vi00, 132);
  // sq.x vf20, -1(vi04)        |  nop
  sq_buffer(Mask::x, vu.vf20, vu.vi04 + -1);
  // iaddi vi04, vi04, -0x2     |  nop
  vu.vi04 = vu.vi04 + -2;
  // xgkick vi04                |  nop
  xgkick(vu.vi04, render_state, prof);
  // nop                        |  nop :e
}

