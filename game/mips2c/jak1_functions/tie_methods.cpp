
//--------------------------MIPS2C---------------------
#include "game/kernel/jak1/kscheme.h"
#include "game/mips2c/mips2c_private.h"
using namespace jak1;
namespace Mips2C::jak1 {
namespace draw_inline_array_instance_tie {
struct Cache {
  void* fake_scratchpad_data;    // *fake-scratchpad-data*
  void* instance_tie_work_copy;  // *instance-tie-work-copy*
  void* wind_work;               // *wind-work*
  void* math_camera;             // *math-camera*
} cache;

Vf background_vu0_data[16];

u16 vcallms_42(ExecutionContext* c) {
  // TODO
  //  lq.xyzw vf16, 0(vi00)      |  nop
  c->vfs[vf16].vf = background_vu0_data[0];
  //  lq.xyzw vf17, 1(vi00)      |  nop
  c->vfs[vf17].vf = background_vu0_data[1];
  //  lq.xyzw vf18, 2(vi00)      |  nop
  c->vfs[vf18].vf = background_vu0_data[2];
  //  lq.xyzw vf19, 3(vi00)      |  nop
  c->vfs[vf19].vf = background_vu0_data[3];
  //  lq.xyzw vf28, 8(vi00)      |  mulax.xyzw ACC, vf16, vf02
  c->vfs[vf28].vf = background_vu0_data[8];
  c->acc.vf.mula(Mask::xyzw, c->vfs[vf16].vf, c->vfs[vf02].vf.x());

  //  lq.xyzw vf29, 9(vi00)      |  madday.xyzw ACC, vf17, vf02
  c->vfs[vf29].vf = background_vu0_data[9];
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf17].vf, c->vfs[vf02].vf.y());

  //  lq.xyzw vf30, 10(vi00)     |  maddaz.xyzw ACC, vf18, vf02
  c->vfs[vf30].vf = background_vu0_data[10];
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf18].vf, c->vfs[vf02].vf.z());

  //  lq.xyzw vf31, 11(vi00)     |  msubaw.xyzw ACC, vf19, vf00
  c->vfs[vf31].vf = background_vu0_data[11];
  c->acc.vf.msuba(Mask::xyzw, c->vfs[vf19].vf, 1.f);

  //  lq.xyzw vf24, 4(vi00)      |  maddw.xyzw vf04, vf01, vf02
  c->vfs[vf24].vf = background_vu0_data[4];
  u16 vi01 =
      c->acc.vf.madd_flag(Mask::xyzw, c->vfs[vf04].vf, c->vfs[vf01].vf, c->vfs[vf02].vf.w()) & 0xf0;

  //  lq.xyzw vf25, 5(vi00)      |  mulax.xyzw ACC, vf28, vf02
  c->vfs[vf25].vf = background_vu0_data[5];
  c->acc.vf.mula(Mask::xyzw, c->vfs[vf28].vf, c->vfs[vf02].vf.x());
  //  lq.xyzw vf26, 6(vi00)      |  madday.xyzw ACC, vf29, vf02
  c->vfs[vf26].vf = background_vu0_data[6];
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf29].vf, c->vfs[vf02].vf.y());
  //  lq.xyzw vf27, 7(vi00)      |  maddaz.xyzw ACC, vf30, vf02
  c->vfs[vf27].vf = background_vu0_data[7];
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf30].vf, c->vfs[vf02].vf.z());
  //  fmand vi01, vi02           |  maddw.xyzw vf05, vf31, vf00
  c->acc.vf.madd(Mask::xyzw, c->vfs[vf05].vf, c->vfs[vf31].vf, 1.f);
  //  nop                        |  mulax.xyzw ACC, vf24, vf02
  c->acc.vf.mula(Mask::xyzw, c->vfs[vf24].vf, c->vfs[vf02].vf.x());
  //  nop                        |  madday.xyzw ACC, vf25, vf02
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf25].vf, c->vfs[vf02].vf.y());
  //  nop                        |  maddaz.xyzw ACC, vf26, vf02 :e
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf26].vf, c->vfs[vf02].vf.z());
  //  nop                        |  maddw.xyzw vf06, vf27, vf00
  c->acc.vf.madd(Mask::xyzw, c->vfs[vf06].vf, c->vfs[vf27].vf, 1.f);

  return vi01;
}

void vcallms_29(ExecutionContext* c) {
  //  lq.xyzw vf24, 4(vi00)      |  nop
  c->vfs[vf24].vf = background_vu0_data[4];
  //  lq.xyzw vf25, 5(vi00)      |  nop
  c->vfs[vf25].vf = background_vu0_data[5];
  //  lq.xyzw vf26, 6(vi00)      |  nop :e
  c->vfs[vf26].vf = background_vu0_data[6];
  //  lq.xyzw vf27, 7(vi00)      |  nop
  c->vfs[vf27].vf = background_vu0_data[7];
}

/*!
 * Set up the VU0 context like it would be for background. See background-upload-vu0 in
 * background.gc for more details.
 */
void init_background(ExecutionContext* c) {
  // this first part is GOAL to init VU1 vf registers from the math camera.

  //  lw v1, *math-camera*(s7)
  c->load_symbol(v1, cache.math_camera);
  //  lqc2 vf16, 860(v1)
  c->lqc2(vf16, 860, v1);
  //  lqc2 vf17, 876(v1)
  c->lqc2(vf17, 876, v1);
  //  lqc2 vf18, 892(v1)
  c->lqc2(vf18, 892, v1);
  //  lqc2 vf19, 908(v1)
  c->lqc2(vf19, 908, v1);
  //  lqc2 vf20, 988(v1)
  c->lqc2(vf20, 988, v1);
  //  lqc2 vf21, 1004(v1)
  c->lqc2(vf21, 1004, v1);
  //  lqc2 vf22, 1020(v1)
  c->lqc2(vf22, 1020, v1);
  //  lqc2 vf23, 1036(v1)
  c->lqc2(vf23, 1036, v1);
  //  lqc2 vf24, 364(v1)
  c->lqc2(vf24, 364, v1);
  //  lqc2 vf25, 380(v1)
  c->lqc2(vf25, 380, v1);
  //  lqc2 vf26, 396(v1)
  c->lqc2(vf26, 396, v1);
  //  lqc2 vf27, 412(v1)
  c->lqc2(vf27, 412, v1);
  //  lqc2 vf28, 572(v1)
  c->lqc2(vf28, 572, v1);
  //  lqc2 vf29, 588(v1)
  c->lqc2(vf29, 588, v1);
  //  lqc2 vf30, 604(v1)
  c->lqc2(vf30, 604, v1);
  //  lqc2 vf31, 620(v1)
  c->lqc2(vf31, 620, v1);
  //  lqc2 vf31, 620(v1)
  c->lqc2(vf31, 620, v1);

  // dump them in VU0 data memory for fast access later.
  //  sq.xyzw vf24, 4(vi00)      |  maxw.xyzw vf01, vf00, vf00
  c->vfs[vf01].vf.fill(1.f);
  background_vu0_data[4] = c->vfs[vf24].vf;
  //  sq.xyzw vf25, 5(vi00)      |  nop
  background_vu0_data[5] = c->vfs[vf25].vf;
  //  sq.xyzw vf26, 6(vi00)      |  nop
  background_vu0_data[6] = c->vfs[vf26].vf;
  //  sq.xyzw vf27, 7(vi00)      |  nop
  background_vu0_data[7] = c->vfs[vf27].vf;
  //  sq.xyzw vf16, 0(vi00)      |  mulz.xyzw vf24, vf01, vf24
  background_vu0_data[0] = c->vfs[vf16].vf;
  c->vfs[vf24].vf.mul(Mask::xyzw, c->vfs[vf01].vf, c->vfs[vf24].vf.z());
  //  sq.xyzw vf17, 1(vi00)      |  mulz.xyzw vf25, vf01, vf25
  background_vu0_data[1] = c->vfs[vf17].vf;
  c->vfs[vf25].vf.mul(Mask::xyzw, c->vfs[vf01].vf, c->vfs[vf25].vf.z());
  //  sq.xyzw vf18, 2(vi00)      |  mulz.xyzw vf26, vf01, vf26
  background_vu0_data[2] = c->vfs[vf18].vf;
  c->vfs[vf26].vf.mul(Mask::xyzw, c->vfs[vf01].vf, c->vfs[vf26].vf.z());
  //  sq.xyzw vf19, 3(vi00)      |  mulz.xyzw vf27, vf01, vf27
  background_vu0_data[3] = c->vfs[vf19].vf;
  c->vfs[vf27].vf.mul(Mask::xyzw, c->vfs[vf01].vf, c->vfs[vf27].vf.z());
  //  sq.xyzw vf24, 12(vi00)     |  nop
  background_vu0_data[12] = c->vfs[vf24].vf;
  //  sq.xyzw vf25, 13(vi00)     |  nop
  background_vu0_data[13] = c->vfs[vf25].vf;
  //  sq.xyzw vf26, 14(vi00)     |  nop
  background_vu0_data[14] = c->vfs[vf26].vf;
  //  sq.xyzw vf27, 15(vi00)     |  nop
  background_vu0_data[15] = c->vfs[vf27].vf;
  //  sq.xyzw vf28, 8(vi00)      |  nop
  background_vu0_data[8] = c->vfs[vf28].vf;
  //  sq.xyzw vf29, 9(vi00)      |  nop
  background_vu0_data[9] = c->vfs[vf29].vf;
  //  sq.xyzw vf30, 10(vi00)     |  nop
  background_vu0_data[10] = c->vfs[vf30].vf;
  //  sq.xyzw vf31, 11(vi00)     |  nop :e
  background_vu0_data[11] = c->vfs[vf31].vf;
  //  iaddiu vi02, vi00, 0xf0    |  nop
}

// clang-format off
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  init_background(c);
  bool bc = false;
  u16 vi01 = 0;

  u32 madr, sadr, qwc;
  c->daddiu(sp, sp, -128);                          // daddiu sp, sp, -128
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s0, 16, sp);                                // sq s0, 16(sp)
  c->sq(s1, 32, sp);                                // sq s1, 32(sp)
  c->sq(s2, 48, sp);                                // sq s2, 48(sp)
  c->sq(s3, 64, sp);                                // sq s3, 64(sp)
  c->sq(s4, 80, sp);                                // sq s4, 80(sp)
  c->sq(s5, 96, sp);                                // sq s5, 96(sp)
  c->sq(gp, 112, sp);                               // sq gp, 112(sp)
  get_fake_spad_addr(t4, cache.fake_scratchpad_data, 0, c);// lui t4, 28672
  c->lw(v1, 4, a3);                                 // lw v1, 4(a3)
  c->lui(t1, 4096);                                 // lui t1, 4096
  c->lui(t2, 4096);                                 // lui t2, 4096
  // sync.l
  // cache dxwbin v1, 0
  // sync.l
  // cache dxwbin v1, 1
  // sync.l
  c->load_symbol(t0, cache.instance_tie_work_copy); // lw t0, *instance-tie-work-copy*(s7)
  c->ori(t1, t1, 54272);                            // ori t1, t1, 54272 SPR TO
  c->sw(a3, 396, t0);                               // sw a3, 396(t0)
  c->ori(a3, t2, 53248);                            // ori a3, t2, 53248 SPR FROM
  c->load_symbol(t5, cache.wind_work);              // lw t5, *wind-work*(s7)
  c->lw(t6, 0, a0);                                 // lw t6, 0(a0)
  c->ori(t2, t4, 16);                               // ori t2, t4, 16
  c->vmax_bc(DEST::xyzw, BC::w, vf1, vf0, vf0);     // vmaxw.xyzw vf1, vf0, vf0
  c->addiu(t3, a1, -4);                             // addiu t3, a1, -4
  c->ori(a1, t4, 4112);                             // ori a1, t4, 4112
  c->sw(t1, 400, t0);                               // sw t1, 400(t0)
  c->addiu(t9, r0, 0);                              // addiu t9, r0, 0
  c->sw(a3, 404, t0);                               // sw a3, 404(t0)
  c->mov64(t8, a1);                                 // or t8, a1, r0
  c->sw(t5, 408, t0);                               // sw t5, 408(t0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 64, t0);                             // lqc2 vf3, 64(t0)
  // nop                                            // sll r0, r0, 0
  c->sw(r0, 432, t0);                               // sw r0, 432(t0)

  block_1:
  bc = c->sgpr64(t6) != 0;                          // bne t6, r0, L139
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_4;}                           // branch non-likely

  c->addiu(a0, a0, 4);                              // addiu a0, a0, 4
  c->addiu(t3, t3, 2048);                           // addiu t3, t3, 2048
  c->daddiu(a2, a2, -32);                           // daddiu a2, a2, -32
  c->lw(t6, 0, a0);                                 // lw t6, 0(a0)
  bc = ((s64)c->sgpr64(a2)) <= 0;                   // blez a2, L177
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_70;}                          // branch non-likely

  //beq r0, r0, L138                                // beq r0, r0, L138
  // nop                                            // sll r0, r0, 0
  goto block_1;                                     // branch always


  block_4:
  /* Wait for SPR TO to be free.
  c->lw(t4, 0, t1);                                 // lw t4, 0(t1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(t4, t4, 256);                             // andi t4, t4, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t4) != 0;                          // bne t4, r0, L139
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_4;}                           // branch non-likely
  */

  // c->sw(t3, 16, t1);                                // sw t3, 16(t1)
  madr = c->sgpr64(t3);
  c->xori(t4, t2, 2048);                            // xori t4, t2, 2048
  // c->sw(t4, 128, t1);                               // sw t4, 128(t1)
  sadr = c->sgpr64(t4);
  c->addiu(t4, r0, 128);                            // addiu t4, r0, 128
  // c->sw(t4, 32, t1);                                // sw t4, 32(t1)
  qwc = c->sgpr64(t4);
  c->addiu(t4, r0, 256);                            // addiu t4, r0, 256
  // c->sw(t4, 0, t1);                                 // sw t4, 0(t1)
  spad_to_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
  // nop                                            // sll r0, r0, 0

  block_6:
  c->mov64(ra, a0);                                 // or ra, a0, r0
  c->xori(t2, t2, 2048);                            // xori t2, t2, 2048
  c->daddiu(a0, a0, 4);                             // daddiu a0, a0, 4
  c->mov64(t7, a0);                                 // or t7, a0, r0
  c->mov64(t4, t2);                                 // or t4, t2, r0
  c->daddiu(t6, a2, -32);                           // daddiu t6, a2, -32
  bc = ((s64)c->sgpr64(t6)) > 0;                    // bgtz t6, L142
  c->lw(t6, 0, a0);                                 // lw t6, 0(a0)
  if (bc) {goto block_11;}                          // branch non-likely

  //beq r0, r0, L145                                // beq r0, r0, L145
  // nop                                            // sll r0, r0, 0
  goto block_15;                                    // branch always

  // nop                                            // sll r0, r0, 0
  c->lw(v1, 400, r0);                               // lw v1, 400(r0)

  block_9:
  c->daddiu(a2, a2, -32);                           // daddiu a2, a2, -32
  c->addiu(a0, a0, 4);                              // addiu a0, a0, 4
  bc = ((s64)c->sgpr64(a2)) <= 0;                   // blez a2, L145
  c->lw(t6, 0, a0);                                 // lw t6, 0(a0)
  if (bc) {goto block_15;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0

  block_11:
  bc = c->sgpr64(t6) == 0;                          // beq t6, r0, L141
  c->addiu(t3, t3, 2048);                           // addiu t3, t3, 2048
  if (bc) {goto block_9;}                           // branch non-likely

  /* Wait on DMA TO
  block_12:
  c->lw(t6, 0, t1);                                 // lw t6, 0(t1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(t6, t6, 256);                             // andi t6, t6, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t6) == 0;                          // beq t6, r0, L144
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_14;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(t6, 444, t0);                               // lw t6, 444(t0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(t6, t6, 1);                             // daddiu t6, t6, 1
  // nop                                            // sll r0, r0, 0
  c->sw(t6, 444, t0);                               // sw t6, 444(t0)
  //beq r0, r0, L143                                // beq r0, r0, L143
  // nop                                            // sll r0, r0, 0
  goto block_12;                                    // branch always
   */


  // block_14:
  // c->sw(t3, 16, t1);                                // sw t3, 16(t1)
  madr = c->sgpr64(t3);
  c->xori(t6, t2, 2048);                            // xori t6, t2, 2048
  // c->sw(t6, 128, t1);                               // sw t6, 128(t1)
  sadr = c->sgpr64(t6);
  c->addiu(t6, r0, 128);                            // addiu t6, r0, 128
  // c->sw(t6, 32, t1);                                // sw t6, 32(t1)
  qwc = c->sgpr64(t6);
  c->addiu(t6, r0, 256);                            // addiu t6, r0, 256
  //beq r0, r0, L146                                // beq r0, r0, L146
  // c->sw(t6, 0, t1);                                 // sw t6, 0(t1)
  spad_to_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
  goto block_17;                                    // branch always


  block_15:
  /*
  c->lw(t6, 0, t1);                                 // lw t6, 0(t1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(t6, t6, 256);                             // andi t6, t6, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t6) == 0;                          // beq t6, r0, L146
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_17;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(t6, 444, t0);                               // lw t6, 444(t0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(t6, t6, 1);                             // daddiu t6, t6, 1
  // nop                                            // sll r0, r0, 0
  c->sw(t6, 444, t0);                               // sw t6, 444(t0)
  //beq r0, r0, L145                                // beq r0, r0, L145
  // nop                                            // sll r0, r0, 0
  goto block_15;                                    // branch always
  */


  block_17:
  c->lb(t6, 0, ra);                                 // lb t6, 0(ra)
  c->addiu(ra, ra, 1);                              // addiu ra, ra, 1
  // nop                                            // sll r0, r0, 0
  c->sw(ra, 412, t0);                               // sw ra, 412(t0)
  bc = c->sgpr64(t6) != 0;                          // bne t6, r0, L147
  c->sw(t7, 416, t0);                               // sw t7, 416(t0)
  if (bc) {goto block_19;}                          // branch non-likely

  c->daddiu(a2, a2, -8);                            // daddiu a2, a2, -8
  c->addiu(t4, t4, 512);                            // addiu t4, t4, 512
  //beq r0, r0, L173                                // beq r0, r0, L173
  // nop                                            // sll r0, r0, 0
  goto block_62;                                    // branch always


  block_19:
  c->addiu(t7, r0, 128);                            // addiu t7, r0, 128
  c->lqc2(vf2, 16, t4);                             // lqc2 vf2, 16(t4)

  block_20:
  c->daddiu(ra, t9, -246);                          // daddiu ra, t9, -246
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(ra)) <= 0;                   // blez ra, L151
  // vcallms 42
  vi01 = vcallms_42(c);
  if (bc) {goto block_24;}                          // branch non-likely

  /*
  block_21:
  c->lw(t8, 0, a3);                                 // lw t8, 0(a3)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(t8, t8, 256);                             // andi t8, t8, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t8) == 0;                          // beq t8, r0, L150
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_23;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(t8, 440, t0);                               // lw t8, 440(t0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(t8, t8, 1);                             // daddiu t8, t8, 1
  // nop                                            // sll r0, r0, 0
  c->sw(t8, 440, t0);                               // sw t8, 440(t0)
  //beq r0, r0, L149                                // beq r0, r0, L149
  // nop                                            // sll r0, r0, 0
  goto block_21;                                    // branch always
  */


  // block_23:
  // c->sw(a1, 128, a3);                               // sw a1, 128(a3)
  sadr = c->sgpr64(a1);
  c->xori(a1, a1, 12288);                           // xori a1, a1, 12288
  // c->sw(v1, 16, a3);                                // sw v1, 16(a3)
  madr = c->sgpr64(v1);
  c->sll(t8, t9, 4);                                // sll t8, t9, 4
  c->addu(v1, v1, t8);                              // addu v1, v1, t8
  c->mov64(t8, a1);                                 // or t8, a1, r0
  // c->sw(t9, 32, a3);                                // sw t9, 32(a3)
  qwc = c->sgpr64(t9);
  c->addiu(t9, r0, 256);                            // addiu t9, r0, 256
  // c->sw(t9, 0, a3);                                 // sw t9, 0(a3)
  spad_from_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
  c->addiu(t9, r0, 0);                              // addiu t9, r0, 0

  block_24:
  // nop                                            // sll r0, r0, 0
  c->lw(ra, 12, t4);                                // lw ra, 12(t4)
  c->and_(gp, t6, t7);                              // and gp, t6, t7
  c->ld(s5, 56, t4);                                // ld s5, 56(t4)
  bc = c->sgpr64(gp) == 0;                          // beq gp, r0, L172
  c->ld(s2, 32, t4);                                // ld s2, 32(t4)
  if (bc) {goto block_61;}                          // branch non-likely

  c->sll(gp, t9, 4);                                // sll gp, t9, 4
  c->ld(s4, 40, t4);                                // ld s4, 40(t4)
  c->pextlh(s3, s5, r0);                            // pextlh s3, s5, r0
  c->ld(s5, 48, t4);                                // ld s5, 48(t4)
  c->psraw(s3, s3, 10);                             // psraw s3, s3, 10
  c->lq(s1, 28, ra);                                // lq s1, 28(ra)
  c->pextlh(s2, s2, r0);                            // pextlh s2, s2, r0
  c->lq(s0, 44, ra);                                // lq s0, 44(ra)
  c->psraw(s2, s2, 16);                             // psraw s2, s2, 16
  c->mov128_vf_gpr(vf14, s1);                       // qmtc2.ni vf14, s1
  c->pextlh(s4, s4, r0);                            // pextlh s4, s4, r0
  c->mov128_vf_gpr(vf15, s0);                       // qmtc2.ni vf15, s0
  c->psraw(s4, s4, 16);                             // psraw s4, s4, 16
  c->mov128_vf_gpr(vf13, s3);                       // qmtc2.ni vf13, s3
  c->pextlh(s5, s5, r0);                            // pextlh s5, s5, r0
  c->mov128_vf_gpr(vf10, s2);                       // qmtc2.ni vf10, s2
  c->psraw(s3, s5, 16);                             // psraw s3, s5, 16
  c->lhu(s2, 62, t4);                               // lhu s2, 62(t4)
  c->addu(gp, gp, v1);                              // addu gp, gp, v1
  c->mov128_vf_gpr(vf11, s4);                       // qmtc2.ni vf11, s4
  c->dsll(s5, s2, 4);                               // dsll s5, s2, 4
  c->mov128_vf_gpr(vf12, s3);                       // qmtc2.ni vf12, s3
  c->daddu(s4, s2, t5);                             // daddu s4, s2, t5
  c->lw(s2, 408, t0);                               // lw s2, 408(t0)
  c->andi(s4, s4, 63);                              // andi s4, s4, 63
  c->lw(s3, 384, t0);                               // lw s3, 384(t0)
  c->sll(s1, s4, 4);                                // sll s1, s4, 4
  c->lw(s4, 4, ra);                                 // lw s4, 4(ra)
  c->daddu(s5, s3, s5);                             // daddu s5, s3, s5
  c->addu(s3, s1, s2);                              // addu s3, s1, s2
  c->andi(s1, s4, 1);                               // andi s1, s4, 1
  c->andi(s4, s4, 2);                               // andi s4, s4, 2
  bc = c->sgpr64(s1) != 0;                          // bne s1, r0, L172
  c->gprs[s1].du64[0] = vi01;                        // cfc2.ni s1, vi1
  if (bc) {goto block_61;}                          // branch non-likely

  c->vitof0(DEST::xyzw, vf13, vf13);                // vitof0.xyzw vf13, vf13
  c->lw(t5, 1324, s2);                              // lw t5, 1324(s2)
  bc = c->sgpr64(s1) != 0;                          // bne s1, r0, L172
  c->lqc2(vf25, 112, t0);                           // lqc2 vf25, 112(t0)
  if (bc) {goto block_61;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lqc2(vf16, 16, t0);                            // lqc2 vf16, 16(t0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf17, 32, t0);                            // lqc2 vf17, 32(t0)
  c->vmula_bc(DEST::xyzw, BC::z, vf1, vf6);         // vmulaz.xyzw acc, vf1, vf6
  c->sw(gp, 196, t0);                               // sw gp, 196(t0)
  c->vmsub_bc(DEST::xyzw, BC::w, vf8, vf1, vf2);    // vmsubw.xyzw vf8, vf1, vf2
  c->sw(gp, 276, t0);                               // sw gp, 276(t0)
  c->vadd(DEST::xyz, vf5, vf0, vf0);                // vadd.xyz vf5, vf0, vf0
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyz, vf13, vf13, vf2);              // vadd.xyz vf13, vf13, vf2
  // nop                                            // sll r0, r0, 0
  // Unknown instr: vmula.xyzw acc, vf1, vf1
  c->vmula(DEST::xyzw, vf1, vf1);
  // nop                                            // sll r0, r0, 0
  c->vsub(DEST::xyzw, vf14, vf8, vf14);             // vsub.xyzw vf14, vf8, vf14
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::w, BC::w, vf5, vf5, vf17);       // vaddw.w vf5, vf5, vf17
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf30, 80, t0);                            // lqc2 vf30, 80(t0)
  c->vmini(DEST::xyzw, vf25, vf8, vf25);            // vmini.xyzw vf25, vf8, vf25
  // nop                                            // sll r0, r0, 0
  // Unknown instr: vmsub.xyz vf15, vf14, vf15
  c->vmsub(DEST::xyz, vf15, vf14, vf15);
  // nop                                            // sll r0, r0, 0
  c->vmini_bc(DEST::w, BC::y, vf5, vf5, vf16);      // vminiy.w vf5, vf5, vf16
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf24, 128, t0);                           // lqc2 vf24, 128(t0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf25, 112, t0);                           // sqc2 vf25, 112(t0)
  c->vmini(DEST::xyz, vf15, vf15, vf1);             // vmini.xyz vf15, vf15, vf1
  // nop                                            // sll r0, r0, 0
  c->vmax_bc(DEST::w, BC::x, vf5, vf5, vf16);       // vmaxx.w vf5, vf5, vf16
  // nop                                            // sll r0, r0, 0
  c->vsub_bc(DEST::xyzw, BC::z, vf16, vf8, vf16);   // vsubz.xyzw vf16, vf8, vf16
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf25, 144, t0);                           // lqc2 vf25, 144(t0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf26, 160, t0);                           // lqc2 vf26, 160(t0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf27, 176, t0);                           // lqc2 vf27, 176(t0)
  c->vmula_bc(DEST::xyzw, BC::x, vf24, vf2);        // vmulax.xyzw acc, vf24, vf2
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf25, vf2);       // vmadday.xyzw acc, vf25, vf2
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::z, vf26, vf2);       // vmaddaz.xyzw acc, vf26, vf2
  // nop                                            // sll r0, r0, 0
  c->vmsuba_bc(DEST::xyzw, BC::w, vf27, vf0);       // vmsubaw.xyzw acc, vf27, vf0
  // nop                                            // sll r0, r0, 0
  c->vmsub_bc(DEST::xyzw, BC::w, vf24, vf1, vf2);   // vmsubw.xyzw vf24, vf1, vf2
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(s2, vf16);                       // qmfc2.i s2, vf16
  c->vmul_bc(DEST::xyzw, BC::w, vf28, vf15, vf30);  // vmulw.xyzw vf28, vf15, vf30
  // nop                                            // sll r0, r0, 0
  c->vmul_bc(DEST::xyzw, BC::w, vf29, vf15, vf30);  // vmulw.xyzw vf29, vf15, vf30
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf19, 0, t0);                             // lqc2 vf19, 0(t0)
  c->vitof12(DEST::xyzw, vf10, vf10);               // vitof12.xyzw vf10, vf10
  // nop                                            // sll r0, r0, 0
  c->pcgtw(s1, r0, s2);                             // pcgtw s1, r0, s2
  c->mov128_gpr_vf(s0, vf24);                       // qmfc2.i s0, vf24
  c->vmul_bc(DEST::xyzw, BC::x, vf28, vf1, vf28);   // vmulx.xyzw vf28, vf1, vf28
  // nop                                            // sll r0, r0, 0
  c->vmul_bc(DEST::xyzw, BC::z, vf29, vf1, vf29);   // vmulz.xyzw vf29, vf1, vf29
  c->lw(s2, 56, ra);                                // lw s2, 56(ra)
  c->pcgtw(s0, r0, s0);                             // pcgtw s0, r0, s0
  c->sqc2(vf5, 80, t8);                             // sqc2 vf5, 80(t8)
  c->ppach(s0, r0, s0);                             // ppach s0, r0, s0
  c->sw(s4, 80, t8);                                // sw s4, 80(t8)
  c->or_(s1, s0, s1);                               // or s1, s0, s1
  c->sqc2(vf14, 96, t0);                            // sqc2 vf14, 96(t0)
  c->ppacb(s1, r0, s1);                             // ppacb s1, r0, s1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  bc = c->sgpr64(s2) == 0;                          // beq s2, r0, L153
  c->sw(s1, 84, t8);                                // sw s1, 84(t8)
  if (bc) {goto block_33;}                          // branch non-likely

  c->vftoi0(DEST::zw, vf28, vf28);                  // vftoi0.zw vf28, vf28
  c->ld(s1, 8, s5);                                 // ld s1, 8(s5)
  c->vftoi0(DEST::zw, vf29, vf29);                  // vftoi0.zw vf29, vf29
  c->ld(s2, 0, s5);                                 // ld s2, 0(s5)
  c->pextlw(s1, r0, s1);                            // pextlw s1, r0, s1
  c->lqc2(vf16, 12, s3);                            // lqc2 vf16, 12(s3)
  c->pextlw(s3, r0, s2);                            // pextlw s3, r0, s2
  c->mov128_vf_gpr(vf18, s1);                       // qmtc2.i vf18, s1
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf17, s3);                       // qmtc2.i vf17, s3
  // Unknown instr: vmula.xyzw acc, vf16, vf1
  c->vmula(DEST::xyzw, vf16, vf1);
  // nop                                            // sll r0, r0, 0
  c->vmsuba_bc(DEST::xyzw, BC::x, vf18, vf19);      // vmsubax.xyzw acc, vf18, vf19
  // nop                                            // sll r0, r0, 0
  c->vmsub_bc(DEST::xyzw, BC::y, vf16, vf17, vf19); // vmsuby.xyzw vf16, vf17, vf19
  // nop                                            // sll r0, r0, 0
  c->vsub_bc(DEST::x, BC::x, vf28, vf30, vf15);     // vsubx.x vf28, vf30, vf15
  // nop                                            // sll r0, r0, 0
  c->vsub_bc(DEST::x, BC::z, vf29, vf1, vf15);      // vsubz.x vf29, vf1, vf15
  // nop                                            // sll r0, r0, 0
  c->vitof0(DEST::zw, vf28, vf28);                  // vitof0.zw vf28, vf28
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::z, vf16, vf19);       // vmulaz.xyzw acc, vf16, vf19
  // nop                                            // sll r0, r0, 0
  c->vmadd(DEST::xyzw, vf18, vf1, vf18);            // vmadd.xyzw vf18, vf1, vf18
  // nop                                            // sll r0, r0, 0
  c->vitof0(DEST::zw, vf29, vf29);                  // vitof0.zw vf29, vf29
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::y, BC::y, vf28, vf0, vf0);       // vaddy.y vf28, vf0, vf0
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::y, BC::y, vf29, vf0, vf0);       // vaddy.y vf29, vf0, vf0
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::z, vf18, vf19);       // vmulaz.xyzw acc, vf18, vf19
  // nop                                            // sll r0, r0, 0
  c->vmadd(DEST::xyzw, vf17, vf17, vf1);            // vmadd.xyzw vf17, vf17, vf1
  // nop                                            // sll r0, r0, 0
  c->vitof12(DEST::xyzw, vf11, vf11);               // vitof12.xyzw vf11, vf11
  // nop                                            // sll r0, r0, 0
  c->vitof12(DEST::xyzw, vf12, vf12);               // vitof12.xyzw vf12, vf12
  // nop                                            // sll r0, r0, 0
  c->vsub_bc(DEST::w, BC::w, vf28, vf30, vf28);     // vsubw.w vf28, vf30, vf28
  // nop                                            // sll r0, r0, 0
  c->vmini_bc(DEST::xyzw, BC::w, vf17, vf17, vf0);  // vminiw.xyzw vf17, vf17, vf0
  // nop                                            // sll r0, r0, 0
  c->vsub_bc(DEST::w, BC::w, vf29, vf30, vf29);     // vsubw.w vf29, vf30, vf29
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(s3, vf18);                       // qmfc2.i s3, vf18
  c->vmax_bc(DEST::xyzw, BC::w, vf27, vf17, vf19);  // vmaxw.xyzw vf27, vf17, vf19
  // nop                                            // sll r0, r0, 0
  c->ppacw(s3, r0, s3);                             // ppacw s3, r0, s3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->vmul_bc(DEST::xyzw, BC::w, vf27, vf27, vf15);  // vmulw.xyzw vf27, vf27, vf15
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::yw, BC::x, vf0, vf0);           // vmulax.yw acc, vf0, vf0
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xz, BC::y, vf27, vf10);         // vmulay.xz acc, vf27, vf10
  // nop                                            // sll r0, r0, 0
  c->vmadd(DEST::xyzw, vf10, vf1, vf10);            // vmadd.xyzw vf10, vf1, vf10
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(s2, vf27);                       // qmfc2.i s2, vf27
  c->vmula_bc(DEST::yw, BC::x, vf0, vf0);           // vmulax.yw acc, vf0, vf0
  c->lw(s1, 436, t0);                               // lw s1, 436(t0)
  c->vmula_bc(DEST::xz, BC::y, vf27, vf11);         // vmulay.xz acc, vf27, vf11
  // nop                                            // sll r0, r0, 0
  c->vmadd(DEST::xyzw, vf11, vf1, vf11);            // vmadd.xyzw vf11, vf1, vf11
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s1) != c->sgpr64(s7);              // bne s1, s7, L152
  c->ppacw(s2, r0, s2);                             // ppacw s2, r0, s2
  if (bc) {goto block_31;}                          // branch non-likely

  c->vmula_bc(DEST::yw, BC::x, vf0, vf0);           // vmulax.yw acc, vf0, vf0
  c->sd(s3, 8, s5);                                 // sd s3, 8(s5)
  c->vmula_bc(DEST::xz, BC::y, vf27, vf12);         // vmulay.xz acc, vf27, vf12
  c->sd(s2, 0, s5);                                 // sd s2, 0(s5)
  bc = c->sgpr64(s4) != 0;                          // bne s4, r0, L164
  c->vmadd(DEST::xyzw, vf12, vf1, vf12);            // vmadd.xyzw vf12, vf1, vf12
  if (bc) {goto block_49;}                          // branch non-likely

  //beq r0, r0, L154                                // beq r0, r0, L154
  // nop                                            // sll r0, r0, 0
  goto block_34;                                    // branch always


  block_31:
  c->vmula_bc(DEST::yw, BC::x, vf0, vf0);           // vmulax.yw acc, vf0, vf0
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xz, BC::y, vf27, vf12);         // vmulay.xz acc, vf27, vf12
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s4) != 0;                          // bne s4, r0, L164
  c->vmadd(DEST::xyzw, vf12, vf1, vf12);            // vmadd.xyzw vf12, vf1, vf12
  if (bc) {goto block_49;}                          // branch non-likely

  //beq r0, r0, L154                                // beq r0, r0, L154
  // nop                                            // sll r0, r0, 0
  goto block_34;                                    // branch always


  block_33:
  c->vftoi0(DEST::zw, vf28, vf28);                  // vftoi0.zw vf28, vf28
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::zw, vf29, vf29);                  // vftoi0.zw vf29, vf29
  // nop                                            // sll r0, r0, 0
  c->vsub_bc(DEST::x, BC::x, vf28, vf30, vf15);     // vsubx.x vf28, vf30, vf15
  // nop                                            // sll r0, r0, 0
  c->vsub_bc(DEST::x, BC::z, vf29, vf1, vf15);      // vsubz.x vf29, vf1, vf15
  // nop                                            // sll r0, r0, 0
  c->vitof0(DEST::zw, vf28, vf28);                  // vitof0.zw vf28, vf28
  // nop                                            // sll r0, r0, 0
  c->vitof0(DEST::zw, vf29, vf29);                  // vitof0.zw vf29, vf29
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::y, BC::y, vf28, vf0, vf0);       // vaddy.y vf28, vf0, vf0
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::y, BC::y, vf29, vf0, vf0);       // vaddy.y vf29, vf0, vf0
  // nop                                            // sll r0, r0, 0
  c->vsub_bc(DEST::w, BC::w, vf28, vf30, vf28);     // vsubw.w vf28, vf30, vf28
  // nop                                            // sll r0, r0, 0
  c->vsub_bc(DEST::w, BC::w, vf29, vf30, vf29);     // vsubw.w vf29, vf30, vf29
  // nop                                            // sll r0, r0, 0
  c->vitof12(DEST::xyzw, vf11, vf11);               // vitof12.xyzw vf11, vf11
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s4) != 0;                          // bne s4, r0, L164
  c->vitof12(DEST::xyzw, vf12, vf12);               // vitof12.xyzw vf12, vf12
  if (bc) {goto block_49;}                          // branch non-likely


  block_34:
  // nop                                            // sll r0, r0, 0
  c->lw(s5, 84, t8);                                // lw s5, 84(t8)
  // nop                                            // sll r0, r0, 0
  c->lw(s4, 108, t0);                               // lw s4, 108(t0)
  c->addiu(t9, t9, 6);                              // addiu t9, t9, 6
  c->lw(s3, 104, t0);                               // lw s3, 104(t0)
  bc = c->sgpr64(s5) != 0;                          // bne s5, r0, L158
  c->vsub_bc(DEST::w, BC::w, vf10, vf10, vf10);     // vsubw.w vf10, vf10, vf10
  if (bc) {goto block_41;}                          // branch non-likely

  bc = ((s64)c->sgpr64(s4)) > 0;                    // bgtz s4, L156
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_39;}                          // branch non-likely

  bc = ((s64)c->sgpr64(s3)) > 0;                    // bgtz s3, L155
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_38;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lh(s4, 78, ra);                                // lh s4, 78(ra)
  // nop                                            // sll r0, r0, 0
  c->lw(s5, 64, ra);                                // lw s5, 64(ra)
  c->daddiu(s4, s4, 1);                             // daddiu s4, s4, 1
  c->sqc2(vf28, 64, t8);                            // sqc2 vf28, 64(t8)
  c->vmula_bc(DEST::xyzw, BC::x, vf20, vf10);       // vmulax.xyzw acc, vf20, vf10
  c->addiu(gp, gp, 96);                             // addiu gp, gp, 96
  c->vmadda_bc(DEST::xyzw, BC::y, vf21, vf10);      // vmadday.xyzw acc, vf21, vf10
  c->sw(gp, 64, ra);                                // sw gp, 64(ra)
  c->vmadd_bc(DEST::xyzw, BC::z, vf10, vf22, vf10); // vmaddz.xyzw vf10, vf22, vf10
  c->sh(s4, 78, ra);                                // sh s4, 78(ra)
  c->vmula_bc(DEST::xyzw, BC::x, vf20, vf11);       // vmulax.xyzw acc, vf20, vf11
  c->lbu(s4, 109, ra);                              // lbu s4, 109(ra)
  c->vmadda_bc(DEST::xyzw, BC::y, vf21, vf11);      // vmadday.xyzw acc, vf21, vf11
  c->lhu(gp, 118, ra);                              // lhu gp, 118(ra)
  c->vmadd_bc(DEST::xyzw, BC::z, vf11, vf22, vf11); // vmaddz.xyzw vf11, vf22, vf11
  c->lbu(s3, 113, ra);                              // lbu s3, 113(ra)
  //beq r0, r0, L157                                // beq r0, r0, L157
  // nop                                            // sll r0, r0, 0
  goto block_40;                                    // branch always


  block_38:
  // nop                                            // sll r0, r0, 0
  c->lh(s4, 80, ra);                                // lh s4, 80(ra)
  // nop                                            // sll r0, r0, 0
  c->lw(s5, 68, ra);                                // lw s5, 68(ra)
  c->daddiu(s4, s4, 1);                             // daddiu s4, s4, 1
  c->sqc2(vf29, 64, t8);                            // sqc2 vf29, 64(t8)
  c->vmula_bc(DEST::xyzw, BC::x, vf20, vf10);       // vmulax.xyzw acc, vf20, vf10
  c->addiu(gp, gp, 96);                             // addiu gp, gp, 96
  c->vmadda_bc(DEST::xyzw, BC::y, vf21, vf10);      // vmadday.xyzw acc, vf21, vf10
  c->sw(gp, 68, ra);                                // sw gp, 68(ra)
  c->vmadd_bc(DEST::xyzw, BC::z, vf10, vf22, vf10); // vmaddz.xyzw vf10, vf22, vf10
  c->sh(s4, 80, ra);                                // sh s4, 80(ra)
  c->vmula_bc(DEST::xyzw, BC::x, vf20, vf11);       // vmulax.xyzw acc, vf20, vf11
  c->lbu(s4, 110, ra);                              // lbu s4, 110(ra)
  c->vmadda_bc(DEST::xyzw, BC::y, vf21, vf11);      // vmadday.xyzw acc, vf21, vf11
  c->lhu(gp, 120, ra);                              // lhu gp, 120(ra)
  c->vmadd_bc(DEST::xyzw, BC::z, vf11, vf22, vf11); // vmaddz.xyzw vf11, vf22, vf11
  c->lbu(s3, 114, ra);                              // lbu s3, 114(ra)
  //beq r0, r0, L157                                // beq r0, r0, L157
  // nop                                            // sll r0, r0, 0
  goto block_40;                                    // branch always


  block_39:
  // nop                                            // sll r0, r0, 0
  c->lh(s4, 82, ra);                                // lh s4, 82(ra)
  // nop                                            // sll r0, r0, 0
  c->lw(s5, 72, ra);                                // lw s5, 72(ra)
  c->daddiu(s4, s4, 1);                             // daddiu s4, s4, 1
  c->sqc2(vf30, 64, t8);                            // sqc2 vf30, 64(t8)
  c->vmula_bc(DEST::xyzw, BC::x, vf20, vf10);       // vmulax.xyzw acc, vf20, vf10
  c->addiu(gp, gp, 96);                             // addiu gp, gp, 96
  c->vmadda_bc(DEST::xyzw, BC::y, vf21, vf10);      // vmadday.xyzw acc, vf21, vf10
  c->sw(gp, 72, ra);                                // sw gp, 72(ra)
  c->vmadd_bc(DEST::xyzw, BC::z, vf10, vf22, vf10); // vmaddz.xyzw vf10, vf22, vf10
  c->sh(s4, 82, ra);                                // sh s4, 82(ra)
  c->vmula_bc(DEST::xyzw, BC::x, vf20, vf11);       // vmulax.xyzw acc, vf20, vf11
  c->lbu(s4, 111, ra);                              // lbu s4, 111(ra)
  c->vmadda_bc(DEST::xyzw, BC::y, vf21, vf11);      // vmadday.xyzw acc, vf21, vf11
  c->lhu(gp, 122, ra);                              // lhu gp, 122(ra)
  c->vmadd_bc(DEST::xyzw, BC::z, vf11, vf22, vf11); // vmaddz.xyzw vf11, vf22, vf11
  c->lbu(s3, 115, ra);                              // lbu s3, 115(ra)

  block_40:
  c->vmula_bc(DEST::xyzw, BC::x, vf20, vf12);       // vmulax.xyzw acc, vf20, vf12
  c->lq(s2, 224, t0);                               // lq s2, 224(t0)
  c->vmadda_bc(DEST::xyzw, BC::y, vf21, vf12);      // vmadday.xyzw acc, vf21, vf12
  c->lq(s1, 240, t0);                               // lq s1, 240(t0)
  c->vmadd_bc(DEST::xyzw, BC::z, vf12, vf22, vf12); // vmaddz.xyzw vf12, vf22, vf12
  c->dsll(gp, gp, 4);                               // dsll gp, gp, 4
  c->vmula_bc(DEST::xyzw, BC::x, vf20, vf13);       // vmulax.xyzw acc, vf20, vf13
  c->daddu(s3, s3, ra);                             // daddu s3, s3, ra
  c->vmadda_bc(DEST::xyzw, BC::y, vf21, vf13);      // vmadday.xyzw acc, vf21, vf13
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::z, vf22, vf13);      // vmaddaz.xyzw acc, vf22, vf13
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::w, vf13, vf23, vf0);  // vmaddw.xyzw vf13, vf23, vf0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf10, 0, t8);                             // sqc2 vf10, 0(t8)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf11, 16, t8);                            // sqc2 vf11, 16(t8)
  c->movz(s2, s1, s5);                              // movz s2, s1, s5
  c->sqc2(vf12, 32, t8);                            // sqc2 vf12, 32(t8)
  c->daddiu(t8, t8, 96);                            // daddiu t8, t8, 96
  //beq r0, r0, L159                                // beq r0, r0, L159
  c->sqc2(vf13, -48, t8);                           // sqc2 vf13, -48(t8)
  goto block_42;                                    // branch always


  block_41:
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf24, 320, t0);                           // lqc2 vf24, 320(t0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf25, 336, t0);                           // lqc2 vf25, 336(t0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf26, 352, t0);                           // lqc2 vf26, 352(t0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf27, 368, t0);                           // lqc2 vf27, 368(t0)
  // nop                                            // sll r0, r0, 0
  c->lh(s4, 76, ra);                                // lh s4, 76(ra)
  // nop                                            // sll r0, r0, 0
  c->lw(s5, 60, ra);                                // lw s5, 60(ra)
  c->daddiu(s4, s4, 1);                             // daddiu s4, s4, 1
  c->sqc2(vf28, 64, t8);                            // sqc2 vf28, 64(t8)
  c->vmula_bc(DEST::xyzw, BC::x, vf24, vf10);       // vmulax.xyzw acc, vf24, vf10
  c->addiu(gp, gp, 96);                             // addiu gp, gp, 96
  c->vmadda_bc(DEST::xyzw, BC::y, vf25, vf10);      // vmadday.xyzw acc, vf25, vf10
  c->sw(gp, 60, ra);                                // sw gp, 60(ra)
  c->vmadd_bc(DEST::xyzw, BC::z, vf10, vf26, vf10); // vmaddz.xyzw vf10, vf26, vf10
  c->sh(s4, 76, ra);                                // sh s4, 76(ra)
  c->vmula_bc(DEST::xyzw, BC::x, vf24, vf11);       // vmulax.xyzw acc, vf24, vf11
  c->lbu(s4, 108, ra);                              // lbu s4, 108(ra)
  c->vmadda_bc(DEST::xyzw, BC::y, vf25, vf11);      // vmadday.xyzw acc, vf25, vf11
  c->lhu(gp, 116, ra);                              // lhu gp, 116(ra)
  c->vmadd_bc(DEST::xyzw, BC::z, vf11, vf26, vf11); // vmaddz.xyzw vf11, vf26, vf11
  c->lbu(s3, 112, ra);                              // lbu s3, 112(ra)
  c->vmula_bc(DEST::xyzw, BC::x, vf24, vf12);       // vmulax.xyzw acc, vf24, vf12
  c->lq(s2, 224, t0);                               // lq s2, 224(t0)
  c->vmadda_bc(DEST::xyzw, BC::y, vf25, vf12);      // vmadday.xyzw acc, vf25, vf12
  c->lq(s1, 240, t0);                               // lq s1, 240(t0)
  c->vmadd_bc(DEST::xyzw, BC::z, vf12, vf26, vf12); // vmaddz.xyzw vf12, vf26, vf12
  c->dsll(gp, gp, 4);                               // dsll gp, gp, 4
  c->vmula_bc(DEST::xyzw, BC::x, vf24, vf13);       // vmulax.xyzw acc, vf24, vf13
  c->daddu(s3, s3, ra);                             // daddu s3, s3, ra
  c->vmadda_bc(DEST::xyzw, BC::y, vf25, vf13);      // vmadday.xyzw acc, vf25, vf13
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::z, vf26, vf13);      // vmaddaz.xyzw acc, vf26, vf13
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::w, vf13, vf27, vf0);  // vmaddw.xyzw vf13, vf27, vf0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf10, 0, t8);                             // sqc2 vf10, 0(t8)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf11, 16, t8);                            // sqc2 vf11, 16(t8)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf12, 32, t8);                            // sqc2 vf12, 32(t8)
  c->movz(s2, s1, s5);                              // movz s2, s1, s5
  c->sqc2(vf13, 48, t8);                            // sqc2 vf13, 48(t8)
  c->daddiu(t8, t8, 96);                            // daddiu t8, t8, 96

  block_42:
  // nop                                            // sll r0, r0, 0
  c->lw(ra, 8, t4);                                 // lw ra, 8(t4)
  // nop                                            // sll r0, r0, 0
  c->sq(s2, 256, t0);                               // sq s2, 256(t0)
  // nop                                            // sll r0, r0, 0
  c->lbu(s2, 144, s3);                              // lbu s2, 144(s3)
  c->addu(s1, gp, ra);                              // addu s1, gp, ra
  c->sw(s5, 260, t0);                               // sw s5, 260(t0)
  c->daddiu(t9, t9, 3);                             // daddiu t9, t9, 3
  c->sw(s1, 212, t0);                               // sw s1, 212(t0)
  c->sll(s1, s2, 2);                                // sll s1, s2, 2
  c->sh(s2, 208, t0);                               // sh s2, 208(t0)
  c->sll(s2, s2, 4);                                // sll s2, s2, 4
  c->sb(s1, 222, t0);                               // sb s1, 222(t0)
  c->daddu(gp, gp, s2);                             // daddu gp, gp, s2
  c->lq(s2, 192, t0);                               // lq s2, 192(t0)
  c->daddiu(s5, s5, 48);                            // daddiu s5, s5, 48
  c->lq(s1, 208, t0);                               // lq s1, 208(t0)
  c->daddiu(t8, t8, 48);                            // daddiu t8, t8, 48
  c->lq(s0, 256, t0);                               // lq s0, 256(t0)
  c->daddiu(s3, s3, 1);                             // daddiu s3, s3, 1
  c->sq(s2, -48, t8);                               // sq s2, -48(t8)
  c->daddiu(s4, s4, -1);                            // daddiu s4, s4, -1
  c->sq(s1, -32, t8);                               // sq s1, -32(t8)
  bc = ((s64)c->sgpr64(s4)) <= 0;                   // blez s4, L172
  c->sq(s0, -16, t8);                               // sq s0, -16(t8)
  if (bc) {goto block_61;}                          // branch non-likely


  block_43:
  c->daddiu(s2, t9, -252);                          // daddiu s2, t9, -252
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(s2)) <= 0;                   // blez s2, L163
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_47;}                          // branch non-likely

  /*
  block_44:
  c->lw(t8, 0, a3);                                 // lw t8, 0(a3)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(t8, t8, 256);                             // andi t8, t8, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t8) == 0;                          // beq t8, r0, L162
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_46;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(t8, 440, t0);                               // lw t8, 440(t0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(t8, t8, 1);                             // daddiu t8, t8, 1
  // nop                                            // sll r0, r0, 0
  c->sw(t8, 440, t0);                               // sw t8, 440(t0)
  //beq r0, r0, L161                                // beq r0, r0, L161
  // nop                                            // sll r0, r0, 0
  goto block_44;                                    // branch always
  */


  // block_46:
  // c->sw(a1, 128, a3);                               // sw a1, 128(a3)
  sadr = c->sgpr64(a1);
  c->xori(a1, a1, 12288);                           // xori a1, a1, 12288
  // c->sw(v1, 16, a3);                                // sw v1, 16(a3)
  madr = c->sgpr64(v1);
  c->sll(t8, t9, 4);                                // sll t8, t9, 4
  c->addu(v1, v1, t8);                              // addu v1, v1, t8
  c->mov64(t8, a1);                                 // or t8, a1, r0
  // c->sw(t9, 32, a3);                                // sw t9, 32(a3)
  qwc = c->sgpr64(t9);
  c->addiu(t9, r0, 256);                            // addiu t9, r0, 256
  // c->sw(t9, 0, a3);                                 // sw t9, 0(a3)
  spad_from_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
  c->addiu(t9, r0, 0);                              // addiu t9, r0, 0

  block_47:
  // nop                                            // sll r0, r0, 0
  c->lbu(s2, 144, s3);                              // lbu s2, 144(s3)
  c->addu(s1, gp, ra);                              // addu s1, gp, ra
  c->sw(s5, 260, t0);                               // sw s5, 260(t0)
  c->daddiu(t9, t9, 3);                             // daddiu t9, t9, 3
  c->sw(s1, 212, t0);                               // sw s1, 212(t0)
  c->sll(s1, s2, 2);                                // sll s1, s2, 2
  c->sh(s2, 208, t0);                               // sh s2, 208(t0)
  c->sll(s2, s2, 4);                                // sll s2, s2, 4
  c->sb(s1, 222, t0);                               // sb s1, 222(t0)
  c->daddu(gp, gp, s2);                             // daddu gp, gp, s2
  c->lq(s2, 192, t0);                               // lq s2, 192(t0)
  c->daddiu(s5, s5, 48);                            // daddiu s5, s5, 48
  c->lq(s1, 208, t0);                               // lq s1, 208(t0)
  c->daddiu(t8, t8, 48);                            // daddiu t8, t8, 48
  c->lq(s0, 256, t0);                               // lq s0, 256(t0)
  c->daddiu(s3, s3, 1);                             // daddiu s3, s3, 1
  c->sq(s2, -48, t8);                               // sq s2, -48(t8)
  c->daddiu(s4, s4, -1);                            // daddiu s4, s4, -1
  c->sq(s1, -32, t8);                               // sq s1, -32(t8)
  bc = ((s64)c->sgpr64(s4)) > 0;                    // bgtz s4, L160
  c->sq(s0, -16, t8);                               // sq s0, -16(t8)
  if (bc) {goto block_43;}                          // branch non-likely

  //beq r0, r0, L172                                // beq r0, r0, L172
  // nop                                            // sll r0, r0, 0
  goto block_61;                                    // branch always


  block_49:
  c->vmul(DEST::xyz, vf16, vf6, vf6);               // vmul.xyz vf16, vf6, vf6
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf9, 124, ra);                            // lqc2 vf9, 124(ra)
  c->vsub_bc(DEST::w, BC::w, vf10, vf10, vf10);     // vsubw.w vf10, vf10, vf10
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->vadda_bc(DEST::x, BC::y, vf16, vf16);          // vadday.x acc, vf16, vf16
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::x, BC::z, vf16, vf1, vf16);     // vmaddz.x vf16, vf1, vf16
  // nop                                            // sll r0, r0, 0
  c->vsqrt(vf16, BC::x);                            // vsqrt Q, vf16.x
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::y, vf1, vf9);         // vmulay.xyzw acc, vf1, vf9
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::w, vf1, vf2);        // vmaddaw.xyzw acc, vf1, vf2
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->vwaitq();                                      // vwaitq
  // Unknown instr: vmsubq.xyzw vf16, vf1, Q
  c->vmsubq(DEST::xyzw, vf16, vf1);
  // nop                                            // sll r0, r0, 0
  c->vmul_bc(DEST::xyzw, BC::x, vf16, vf16, vf9);   // vmulx.xyzw vf16, vf16, vf9
  // nop                                            // sll r0, r0, 0
  c->vmax_bc(DEST::x, BC::x, vf16, vf16, vf0);      // vmaxx.x vf16, vf16, vf0
  // nop                                            // sll r0, r0, 0
  c->vmini_bc(DEST::x, BC::y, vf16, vf16, vf3);     // vminiy.x vf16, vf16, vf3
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf16, vf16);                // vftoi0.xyzw vf16, vf16
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(s5, vf16);                       // qmfc2.i s5, vf16
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(s5, s5, 255);                             // andi s5, s5, 255
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s5) == 0;                          // beq s5, r0, L154
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_34;}                          // branch non-likely

  // Unknown instr: vcallms 29
  vcallms_29(c);
  c->sw(s4, 432, t0);                               // sw s4, 432(t0)
  // nop                                            // sll r0, r0, 0
  c->lw(s4, 108, t0);                               // lw s4, 108(t0)
  c->addiu(t9, t9, 6);                              // addiu t9, t9, 6
  c->lw(s3, 104, t0);                               // lw s3, 104(t0)
  // nop                                            // sll r0, r0, 0
  c->sw(s5, 80, t8);                                // sw s5, 80(t8)
  bc = ((s64)c->sgpr64(s4)) > 0;                    // bgtz s4, L166
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_54;}                          // branch non-likely

  bc = ((s64)c->sgpr64(s3)) > 0;                    // bgtz s3, L165
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_53;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lh(s4, 86, ra);                                // lh s4, 86(ra)
  // nop                                            // sll r0, r0, 0
  c->lw(s5, 96, ra);                                // lw s5, 96(ra)
  c->daddiu(s4, s4, 1);                             // daddiu s4, s4, 1
  c->sqc2(vf28, 64, t8);                            // sqc2 vf28, 64(t8)
  c->vmula_bc(DEST::xyzw, BC::x, vf24, vf10);       // vmulax.xyzw acc, vf24, vf10
  c->addiu(gp, gp, 96);                             // addiu gp, gp, 96
  c->vmadda_bc(DEST::xyzw, BC::y, vf25, vf10);      // vmadday.xyzw acc, vf25, vf10
  c->sw(gp, 96, ra);                                // sw gp, 96(ra)
  c->vmadd_bc(DEST::xyzw, BC::z, vf10, vf26, vf10); // vmaddz.xyzw vf10, vf26, vf10
  c->sh(s4, 86, ra);                                // sh s4, 86(ra)
  c->vmula_bc(DEST::xyzw, BC::x, vf24, vf11);       // vmulax.xyzw acc, vf24, vf11
  c->lbu(s3, 109, ra);                              // lbu s3, 109(ra)
  c->vmadda_bc(DEST::xyzw, BC::y, vf25, vf11);      // vmadday.xyzw acc, vf25, vf11
  c->lhu(gp, 118, ra);                              // lhu gp, 118(ra)
  c->vmadd_bc(DEST::xyzw, BC::z, vf11, vf26, vf11); // vmaddz.xyzw vf11, vf26, vf11
  c->lbu(s4, 113, ra);                              // lbu s4, 113(ra)
  //beq r0, r0, L167                                // beq r0, r0, L167
  // nop                                            // sll r0, r0, 0
  goto block_55;                                    // branch always


  block_53:
  // nop                                            // sll r0, r0, 0
  c->lh(s4, 88, ra);                                // lh s4, 88(ra)
  // nop                                            // sll r0, r0, 0
  c->lw(s5, 100, ra);                               // lw s5, 100(ra)
  c->daddiu(s4, s4, 1);                             // daddiu s4, s4, 1
  c->sqc2(vf29, 64, t8);                            // sqc2 vf29, 64(t8)
  c->vmula_bc(DEST::xyzw, BC::x, vf24, vf10);       // vmulax.xyzw acc, vf24, vf10
  c->addiu(gp, gp, 96);                             // addiu gp, gp, 96
  c->vmadda_bc(DEST::xyzw, BC::y, vf25, vf10);      // vmadday.xyzw acc, vf25, vf10
  c->sw(gp, 100, ra);                               // sw gp, 100(ra)
  c->vmadd_bc(DEST::xyzw, BC::z, vf10, vf26, vf10); // vmaddz.xyzw vf10, vf26, vf10
  c->sh(s4, 88, ra);                                // sh s4, 88(ra)
  c->vmula_bc(DEST::xyzw, BC::x, vf24, vf11);       // vmulax.xyzw acc, vf24, vf11
  c->lbu(s3, 110, ra);                              // lbu s3, 110(ra)
  c->vmadda_bc(DEST::xyzw, BC::y, vf25, vf11);      // vmadday.xyzw acc, vf25, vf11
  c->lhu(gp, 120, ra);                              // lhu gp, 120(ra)
  c->vmadd_bc(DEST::xyzw, BC::z, vf11, vf26, vf11); // vmaddz.xyzw vf11, vf26, vf11
  c->lbu(s4, 114, ra);                              // lbu s4, 114(ra)
  //beq r0, r0, L167                                // beq r0, r0, L167
  // nop                                            // sll r0, r0, 0
  goto block_55;                                    // branch always


  block_54:
  // nop                                            // sll r0, r0, 0
  c->lh(s4, 90, ra);                                // lh s4, 90(ra)
  // nop                                            // sll r0, r0, 0
  c->lw(s5, 104, ra);                               // lw s5, 104(ra)
  c->daddiu(s4, s4, 1);                             // daddiu s4, s4, 1
  c->sqc2(vf30, 64, t8);                            // sqc2 vf30, 64(t8)
  c->vmula_bc(DEST::xyzw, BC::x, vf24, vf10);       // vmulax.xyzw acc, vf24, vf10
  c->addiu(gp, gp, 96);                             // addiu gp, gp, 96
  c->vmadda_bc(DEST::xyzw, BC::y, vf25, vf10);      // vmadday.xyzw acc, vf25, vf10
  c->sw(gp, 104, ra);                               // sw gp, 104(ra)
  c->vmadd_bc(DEST::xyzw, BC::z, vf10, vf26, vf10); // vmaddz.xyzw vf10, vf26, vf10
  c->sh(s4, 90, ra);                                // sh s4, 90(ra)
  c->vmula_bc(DEST::xyzw, BC::x, vf24, vf11);       // vmulax.xyzw acc, vf24, vf11
  c->lbu(s3, 111, ra);                              // lbu s3, 111(ra)
  c->vmadda_bc(DEST::xyzw, BC::y, vf25, vf11);      // vmadday.xyzw acc, vf25, vf11
  c->lhu(gp, 122, ra);                              // lhu gp, 122(ra)
  c->vmadd_bc(DEST::xyzw, BC::z, vf11, vf26, vf11); // vmaddz.xyzw vf11, vf26, vf11
  c->lbu(s4, 115, ra);                              // lbu s4, 115(ra)

  block_55:
  c->vmula_bc(DEST::xyzw, BC::x, vf24, vf12);       // vmulax.xyzw acc, vf24, vf12
  c->dsll(gp, gp, 4);                               // dsll gp, gp, 4
  c->vmadda_bc(DEST::xyzw, BC::y, vf25, vf12);      // vmadday.xyzw acc, vf25, vf12
  c->daddu(s4, s4, ra);                             // daddu s4, s4, ra
  c->vmadd_bc(DEST::xyzw, BC::z, vf12, vf26, vf12); // vmaddz.xyzw vf12, vf26, vf12
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::x, vf24, vf13);       // vmulax.xyzw acc, vf24, vf13
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf25, vf13);      // vmadday.xyzw acc, vf25, vf13
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::z, vf26, vf13);      // vmaddaz.xyzw acc, vf26, vf13
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::w, vf13, vf27, vf0);  // vmaddw.xyzw vf13, vf27, vf0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf10, 0, t8);                             // sqc2 vf10, 0(t8)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf11, 16, t8);                            // sqc2 vf11, 16(t8)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf12, 32, t8);                            // sqc2 vf12, 32(t8)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf13, 48, t8);                            // sqc2 vf13, 48(t8)
  c->daddiu(t8, t8, 96);                            // daddiu t8, t8, 96
  // nop                                            // sll r0, r0, 0
  c->lw(ra, 8, t4);                                 // lw ra, 8(t4)
  // nop                                            // sll r0, r0, 0
  c->lbu(s2, 144, s4);                              // lbu s2, 144(s4)
  c->addu(s1, gp, ra);                              // addu s1, gp, ra
  c->sw(s5, 284, t0);                               // sw s5, 284(t0)
  c->daddiu(t9, t9, 3);                             // daddiu t9, t9, 3
  c->sw(s1, 292, t0);                               // sw s1, 292(t0)
  c->sll(s1, s2, 4);                                // sll s1, s2, 4
  c->sh(s2, 288, t0);                               // sh s2, 288(t0)
  c->daddu(gp, gp, s1);                             // daddu gp, gp, s1
  c->lq(s2, 272, t0);                               // lq s2, 272(t0)
  c->daddiu(s5, s5, 48);                            // daddiu s5, s5, 48
  c->lq(s1, 288, t0);                               // lq s1, 288(t0)
  c->daddiu(t8, t8, 48);                            // daddiu t8, t8, 48
  c->lq(s0, 304, t0);                               // lq s0, 304(t0)
  c->daddiu(s4, s4, 1);                             // daddiu s4, s4, 1
  c->sq(s2, -48, t8);                               // sq s2, -48(t8)
  c->daddiu(s3, s3, -1);                            // daddiu s3, s3, -1
  c->sq(s1, -32, t8);                               // sq s1, -32(t8)
  bc = ((s64)c->sgpr64(s3)) <= 0;                   // blez s3, L172
  c->sq(s0, -16, t8);                               // sq s0, -16(t8)
  if (bc) {goto block_61;}                          // branch non-likely


  block_56:
  c->daddiu(s2, t9, -252);                          // daddiu s2, t9, -252
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(s2)) <= 0;                   // blez s2, L171
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_60;}                          // branch non-likely

  /*
  block_57:
  c->lw(t8, 0, a3);                                 // lw t8, 0(a3)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(t8, t8, 256);                             // andi t8, t8, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t8) == 0;                          // beq t8, r0, L170
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_59;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(t8, 440, t0);                               // lw t8, 440(t0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(t8, t8, 1);                             // daddiu t8, t8, 1
  // nop                                            // sll r0, r0, 0
  c->sw(t8, 440, t0);                               // sw t8, 440(t0)
  //beq r0, r0, L169                                // beq r0, r0, L169
  // nop                                            // sll r0, r0, 0
  goto block_57;                                    // branch always
  */


  // block_59:
  // c->sw(a1, 128, a3);                               // sw a1, 128(a3)
  sadr = c->sgpr64(a1);
  c->xori(a1, a1, 12288);                           // xori a1, a1, 12288
  // c->sw(v1, 16, a3);                                // sw v1, 16(a3)
  madr = c->sgpr64(v1);
  c->sll(t8, t9, 4);                                // sll t8, t9, 4
  c->addu(v1, v1, t8);                              // addu v1, v1, t8
  c->mov64(t8, a1);                                 // or t8, a1, r0
  // c->sw(t9, 32, a3);                                // sw t9, 32(a3)
  qwc = c->sgpr64(t9);
  c->addiu(t9, r0, 256);                            // addiu t9, r0, 256
  // c->sw(t9, 0, a3);                                 // sw t9, 0(a3)
  spad_from_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
  c->addiu(t9, r0, 0);                              // addiu t9, r0, 0

  block_60:
  // nop                                            // sll r0, r0, 0
  c->lbu(s2, 144, s4);                              // lbu s2, 144(s4)
  c->addu(s1, gp, ra);                              // addu s1, gp, ra
  c->sw(s5, 284, t0);                               // sw s5, 284(t0)
  c->daddiu(t9, t9, 3);                             // daddiu t9, t9, 3
  c->sw(s1, 292, t0);                               // sw s1, 292(t0)
  c->sll(s1, s2, 4);                                // sll s1, s2, 4
  c->sh(s2, 288, t0);                               // sh s2, 288(t0)
  c->daddu(gp, gp, s1);                             // daddu gp, gp, s1
  c->lq(s2, 272, t0);                               // lq s2, 272(t0)
  c->daddiu(s5, s5, 48);                            // daddiu s5, s5, 48
  c->lq(s1, 288, t0);                               // lq s1, 288(t0)
  c->daddiu(t8, t8, 48);                            // daddiu t8, t8, 48
  c->lq(s0, 304, t0);                               // lq s0, 304(t0)
  c->daddiu(s4, s4, 1);                             // daddiu s4, s4, 1
  c->sq(s2, -48, t8);                               // sq s2, -48(t8)
  c->daddiu(s3, s3, -1);                            // daddiu s3, s3, -1
  c->sq(s1, -32, t8);                               // sq s1, -32(t8)
  bc = ((s64)c->sgpr64(s3)) > 0;                    // bgtz s3, L168
  c->sq(s0, -16, t8);                               // sq s0, -16(t8)
  if (bc) {goto block_56;}                          // branch non-likely


  block_61:
  c->addiu(a2, a2, -1);                             // addiu a2, a2, -1
  c->srl(t7, t7, 1);                                // srl t7, t7, 1
  c->daddiu(t4, t4, 64);                            // daddiu t4, t4, 64
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t7) != 0;                          // bne t7, r0, L148
  c->lqc2(vf2, 16, t4);                             // lqc2 vf2, 16(t4)
  if (bc) {goto block_20;}                          // branch non-likely


  block_62:
  // nop                                            // sll r0, r0, 0
  c->lw(ra, 412, t0);                               // lw ra, 412(t0)
  // nop                                            // sll r0, r0, 0
  c->lw(t7, 416, t0);                               // lw t7, 416(t0)
  bc = c->sgpr64(ra) != c->sgpr64(t7);              // bne ra, t7, L146
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_17;}                          // branch non-likely

  bc = ((s64)c->sgpr64(a2)) > 0;                    // bgtz a2, L140
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_6;}                           // branch non-likely

  bc = c->sgpr64(t9) == 0;                          // beq t9, r0, L176
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_68;}                          // branch non-likely

  /*
  block_65:
  c->lw(a0, 0, a3);                                 // lw a0, 0(a3)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(a0, a0, 256);                             // andi a0, a0, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L175
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_67;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(a0, 440, t0);                               // lw a0, 440(t0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 440, t0);                               // sw a0, 440(t0)
  //beq r0, r0, L174                                // beq r0, r0, L174
  // nop                                            // sll r0, r0, 0
  goto block_65;                                    // branch always
  */


  // block_67:
  // c->sw(a1, 128, a3);                               // sw a1, 128(a3)
  sadr = c->sgpr64(a1);
  c->xori(a0, a1, 12288);                           // xori a0, a1, 12288
  // c->sw(v1, 16, a3);                                // sw v1, 16(a3)
  madr = c->sgpr64(v1);
  c->sll(a1, t9, 4);                                // sll a1, t9, 4
  c->addu(v1, v1, a1);                              // addu v1, v1, a1
  c->mov64(a0, a0);                                 // or a0, a0, r0
  // c->sw(t9, 32, a3);                                // sw t9, 32(a3)
  qwc = c->sgpr64(t9);
  c->addiu(a0, r0, 256);                            // addiu a0, r0, 256
  // c->sw(a0, 0, a3);                                 // sw a0, 0(a3)
  spad_from_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0

  block_68:
  /*
  c->lw(a0, 0, a3);                                 // lw a0, 0(a3)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(a0, a0, 256);                             // andi a0, a0, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L177
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_70;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(a0, 440, t0);                               // lw a0, 440(t0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 440, t0);                               // sw a0, 440(t0)
  //beq r0, r0, L176                                // beq r0, r0, L176
  // nop                                            // sll r0, r0, 0
  goto block_68;                                    // branch always
   */


  block_70:
  c->lw(a0, 396, t0);                               // lw a0, 396(t0)
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 4, a0);                                 // sw v1, 4(a0)
  // nop                                            // sll r0, r0, 0
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 112, sp);                               // lq gp, 112(sp)
  c->lq(s5, 96, sp);                                // lq s5, 96(sp)
  c->lq(s4, 80, sp);                                // lq s4, 80(sp)
  c->lq(s3, 64, sp);                                // lq s3, 64(sp)
  c->lq(s2, 48, sp);                                // lq s2, 48(sp)
  c->lq(s1, 32, sp);                                // lq s1, 32(sp)
  c->lq(s0, 16, sp);                                // lq s0, 16(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 128);                           // daddiu sp, sp, 128
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  cache.instance_tie_work_copy = intern_from_c("*instance-tie-work-copy*").c();
  cache.wind_work = intern_from_c("*wind-work*").c();
  cache.math_camera = intern_from_c("*math-camera*").c();
  gLinkedFunctionTable.reg("draw-inline-array-instance-tie", execute, 512);
}

} // namespace draw_inline_array_instance_tie
} // namespace Mips2C


//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace draw_inline_array_prototype_tie_generic_asm {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* prototype_tie_work; // *prototype-tie-work*
} cache;

void block29_call(ExecutionContext* c) {
  bool bc;
  u32 sadr, madr, qwc;
  // block_29:
  c->addiu(t6, t6, 32);                             // addiu t6, t6, 32
  c->sw(t5, 232, t0);                               // sw t5, 232(t0)

  block_30:
  c->addiu(t7, a0, 4);                              // addiu t7, a0, 4
  c->addiu(t8, r0, 255);                            // addiu t8, r0, 255
  c->dsubu(t8, t8, t7);                             // dsubu t8, t8, t7
  c->lw(t7, 0, t6);                                 // lw t7, 0(t6)
  bc = ((s64)c->sgpr64(t8)) >= 0;                   // bgez t8, L131
  c->lhu(t8, 30, t6);                               // lhu t8, 30(t6)
  if (bc) {goto block_34;}                          // branch non-likely

  /*
  block_31:
  c->lw(a1, 0, t1);                                 // lw a1, 0(t1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(a1, a1, 256);                             // andi a1, a1, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L130
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_33;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(a1, 284, t0);                               // lw a1, 284(t0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(a1, a1, 1);                             // daddiu a1, a1, 1
  // nop                                            // sll r0, r0, 0
  c->sw(a1, 284, t0);                               // sw a1, 284(t0)
  //beq r0, r0, L129                                // beq r0, r0, L129
  // nop                                            // sll r0, r0, 0
  goto block_31;                                    // branch always
  */


  // block_33:
  // c->sw(t2, 128, t1);                               // sw t2, 128(t1)
  sadr = c->sgpr64(t2);
  c->xori(t2, t2, 4096);                            // xori t2, t2, 4096
  // c->sw(v1, 16, t1);                                // sw v1, 16(t1)
  madr = c->sgpr64(v1);
  c->sll(a1, a0, 4);                                // sll a1, a0, 4
  c->addu(v1, v1, a1);                              // addu v1, v1, a1
  c->mov64(a1, t2);                                 // or a1, t2, r0
  // c->sw(a0, 32, t1);                                // sw a0, 32(t1)
  qwc = c->sgpr64(a0);
  c->addiu(a0, r0, 256);                            // addiu a0, r0, 256
  // c->sw(a0, 0, t1);                                 // sw a0, 0(t1)
  spad_from_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0

  block_34:
  // nop                                            // sll r0, r0, 0
  c->lhu(t9, 28, t6);                               // lhu t9, 28(t6)
  c->daddu(t8, t8, t9);                             // daddu t8, t8, t9
  c->lw(t9, 4, t6);                                 // lw t9, 4(t6)
  c->daddiu(a0, a0, 4);                             // daddiu a0, a0, 4
  c->lhu(gp, 32, t6);                               // lhu gp, 32(t6)
  c->sll(s5, a0, 4);                                // sll s5, a0, 4
  c->sw(t7, 212, t0);                               // sw t7, 212(t0)
  c->daddu(t7, s5, v1);                             // daddu t7, s5, v1
  c->sh(t8, 208, t0);                               // sh t8, 208(t0)
  // nop                                            // sll r0, r0, 0
  c->sw(t7, 220, t0);                               // sw t7, 220(t0)
  // nop                                            // sll r0, r0, 0
  c->sw(a2, 260, t0);                               // sw a2, 260(t0)
  // nop                                            // sll r0, r0, 0
  c->sw(t9, 228, t0);                               // sw t9, 228(t0)
  // nop                                            // sll r0, r0, 0
  c->sh(gp, 224, t0);                               // sh gp, 224(t0)
  // nop                                            // sll r0, r0, 0
  c->lw(t7, 48, t6);                                // lw t7, 48(t6)
  // nop                                            // sll r0, r0, 0
  c->lhu(t8, 52, t6);                               // lhu t8, 52(t6)
  // nop                                            // sll r0, r0, 0
  c->sw(t7, 244, t0);                               // sw t7, 244(t0)
  // nop                                            // sll r0, r0, 0
  c->sh(t8, 240, t0);                               // sh t8, 240(t0)
  // nop                                            // sll r0, r0, 0
  c->lq(t7, 208, t0);                               // lq t7, 208(t0)
  // nop                                            // sll r0, r0, 0
  c->lq(t8, 224, t0);                               // lq t8, 224(t0)
  // nop                                            // sll r0, r0, 0
  c->lq(t9, 240, t0);                               // lq t9, 240(t0)
  // nop                                            // sll r0, r0, 0
  c->lq(gp, 256, t0);                               // lq gp, 256(t0)
  // nop                                            // sll r0, r0, 0
  c->sq(t7, 0, a1);                                 // sq t7, 0(a1)
  c->daddiu(t5, t5, -1);                            // daddiu t5, t5, -1
  c->sq(t8, 16, a1);                                // sq t8, 16(a1)
  c->daddiu(a2, a2, 48);                            // daddiu a2, a2, 48
  c->sq(t9, 32, a1);                                // sq t9, 32(a1)
  c->sq(gp, 48, a1);                                // sq gp, 48(a1)
  c->daddiu(a1, a1, 64);                            // daddiu a1, a1, 64
  bc = ((s64)c->sgpr64(t5)) > 0;                    // bgtz t5, L128
  c->daddiu(t6, t6, 64);                            // daddiu t6, t6, 64
  if (bc) {goto block_30;}                          // branch non-likely

  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
}

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 madr, sadr, qwc;
  c->daddiu(sp, sp, -112);                          // daddiu sp, sp, -112
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s1, 16, sp);                                // sq s1, 16(sp)
  c->sq(s2, 32, sp);                                // sq s2, 32(sp)
  c->sq(s3, 48, sp);                                // sq s3, 48(sp)
  c->sq(s4, 64, sp);                                // sq s4, 64(sp)
  c->sq(s5, 80, sp);                                // sq s5, 80(sp)
  c->sq(gp, 96, sp);                                // sq gp, 96(sp)
  // nop                                            // sll r0, r0, 0
  get_fake_spad_addr(a3, cache.fake_scratchpad_data, 0, c);// lui a3, 28672
  c->lw(v1, 4, a0);                                 // lw v1, 4(a0)
  c->lui(t1, 4096);                                 // lui t1, 4096
  c->lui(t2, 4096);                                 // lui t2, 4096
  // sync.l
  // cache dxwbin v1, 0
  // sync.l
  // cache dxwbin v1, 1
  // sync.l
  c->load_symbol(t0, cache.prototype_tie_work);     // lw t0, *prototype-tie-work*(s7)
  c->ori(t1, t1, 53248);                            // ori t1, t1, 53248
  c->ori(t4, t2, 54272);                            // ori t4, t2, 54272 SPR TO
  c->ori(t3, a3, 16);                               // ori t3, a3, 16
  c->ori(t2, a3, 2064);                             // ori t2, a3, 2064
  c->sw(a0, 10260, a3);                             // sw a0, 10260(a3)
  c->daddiu(t7, a1, -1);                            // daddiu t7, a1, -1
  // nop                                            // sll r0, r0, 0
  c->lw(t6, 12, a2);                                // lw t6, 12(a2)
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  c->mov64(a1, t2);                                 // or a1, t2, r0

  block_1:
  // nop                                            // sll r0, r0, 0
  c->lq(t5, 92, t6);                                // lq t5, 92(t6)
  c->daddiu(t8, a2, 4);                             // daddiu t8, a2, 4
  c->sw(t7, 10256, a3);                             // sw t7, 10256(a3)
  c->dsrl32(a2, t5, 0);                             // dsrl32 a2, t5, 0
  c->sw(t8, 280, t0);                               // sw t8, 280(t0)
  c->pcpyud(t7, t5, t5);                            // pcpyud t7, t5, t5
  c->lw(t9, 140, t6);                               // lw t9, 140(t6)
  c->or_(t7, a2, t7);                               // or t7, a2, t7
  c->lw(a2, 108, t6);                               // lw a2, 108(t6)
  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L126
  c->lw(t8, 4, a3);                                 // lw t8, 4(a3)
  if (bc) {goto block_27;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lq(t7, 12, t6);                                // lq t7, 12(t6)
  // nop                                            // sll r0, r0, 0
  c->sq(t5, 10272, a3);                             // sq t5, 10272(a3)
  // nop                                            // sll r0, r0, 0
  c->sw(a2, 10304, a3);                             // sw a2, 10304(a3)
  // nop                                            // sll r0, r0, 0
  c->sq(t7, 10288, a3);                             // sq t7, 10288(a3)
  // nop                                            // sll r0, r0, 0
  c->ld(a2, 272, t0);                               // ld a2, 272(t0)
  // nop                                            // sll r0, r0, 0
  c->lw(t7, 4, t9);                                 // lw t7, 4(t9)
  c->daddiu(ra, t9, 12);                            // daddiu ra, t9, 12
  c->lq(t5, 1852, t8);                              // lq t5, 1852(t8)
  c->sra(t9, t7, 2);                                // sra t9, t7, 2
  // nop                                            // sll r0, r0, 0
  c->addu(t9, t9, a0);                              // addu t9, t9, a0
  c->addiu(gp, r0, 221);                            // addiu gp, r0, 221
  c->dsubu(t9, gp, t9);                             // dsubu t9, gp, t9
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(t9)) >= 0;                   // bgez t9, L114
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_6;}                           // branch non-likely

  /*
  block_3:
  c->lw(a1, 0, t1);                                 // lw a1, 0(t1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(a1, a1, 256);                             // andi a1, a1, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L113
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_5;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(a1, 284, t0);                               // lw a1, 284(t0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(a1, a1, 1);                             // daddiu a1, a1, 1
  // nop                                            // sll r0, r0, 0
  c->sw(a1, 284, t0);                               // sw a1, 284(t0)
  //beq r0, r0, L112                                // beq r0, r0, L112
  // nop                                            // sll r0, r0, 0
  goto block_3;                                     // branch always
  */


  // block_5:
  // c->sw(t2, 128, t1);                               // sw t2, 128(t1)
  sadr = c->sgpr64(t2);
  c->xori(t2, t2, 4096);                            // xori t2, t2, 4096
  // c->sw(v1, 16, t1);                                // sw v1, 16(t1)
  madr = c->sgpr64(v1);
  c->sll(a1, a0, 4);                                // sll a1, a0, 4
  c->addu(v1, v1, a1);                              // addu v1, v1, a1
  c->mov64(a1, t2);                                 // or a1, t2, r0
  // c->sw(a0, 32, t1);                                // sw a0, 32(t1)
  qwc = c->sgpr64(a0);
  c->addiu(a0, r0, 256);                            // addiu a0, r0, 256
  // c->sw(a0, 0, t1);                                 // sw a0, 0(t1)
  spad_from_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0

  block_6:
  c->addiu(t7, t7, 31);                             // addiu t7, t7, 31
  c->lw(t6, 132, t6);                               // lw t6, 132(t6)
  c->sra(t7, t7, 5);                                // sra t7, t7, 5
  c->addiu(a0, a0, 1);                              // addiu a0, a0, 1
  c->sll(t7, t7, 3);                                // sll t7, t7, 3
  c->sw(t6, 200, t0);                               // sw t6, 200(t0)
  c->addu(a0, a0, t7);                              // addu a0, a0, t7
  c->sh(t7, 192, t0);                               // sh t7, 192(t0)
  c->sll(t9, t7, 2);                                // sll t9, t7, 2
  c->lq(t6, 1868, t8);                              // lq t6, 1868(t8)
  // nop                                            // sll r0, r0, 0
  c->lq(gp, 192, t0);                               // lq gp, 192(t0)
  // nop                                            // sll r0, r0, 0
  c->lq(t7, 1884, t8);                              // lq t7, 1884(t8)
  // nop                                            // sll r0, r0, 0
  c->sq(gp, 0, a1);                                 // sq gp, 0(a1)
  c->addiu(a1, a1, 16);                             // addiu a1, a1, 16
  c->lq(t8, 1900, t8);                              // lq t8, 1900(t8)

  /*
  block_7:
  c->lw(gp, 0, t4);                                 // lw gp, 0(t4)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(gp, gp, 256);                             // andi gp, gp, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(gp) == 0;                          // beq gp, r0, L116
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_9;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(gp, 288, t0);                               // lw gp, 288(t0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(gp, gp, 1);                             // daddiu gp, gp, 1
  // nop                                            // sll r0, r0, 0
  c->sw(gp, 288, t0);                               // sw gp, 288(t0)
  //beq r0, r0, L115                                // beq r0, r0, L115
  // nop                                            // sll r0, r0, 0
  goto block_7;                                     // branch always
  */


  // block_9:
  // c->sw(ra, 16, t4);                                // sw ra, 16(t4)
  madr = c->sgpr64(ra);
  c->daddiu(t9, t9, -32);                           // daddiu t9, t9, -32
  // c->sw(t3, 128, t4);                               // sw t3, 128(t4)
  sadr = c->sgpr64(t3);
  c->addiu(gp, r0, 64);                             // addiu gp, r0, 64
  // c->sw(gp, 32, t4);                                // sw gp, 32(t4)
  qwc = c->sgpr64(gp);
  c->addiu(gp, r0, 256);                            // addiu gp, r0, 256
  // c->sw(gp, 0, t4);                                 // sw gp, 0(t4)
  spad_to_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
  c->daddiu(ra, ra, 1024);                          // daddiu ra, ra, 1024

  block_10:
  c->mov64(s5, t3);                                 // or s5, t3, r0
  c->xori(t3, t3, 1024);                            // xori t3, t3, 1024
  bc = ((s64)c->sgpr64(t9)) <= 0;                   // blez t9, L120
  c->daddiu(t9, t9, -32);                           // daddiu t9, t9, -32
  if (bc) {goto block_14;}                          // branch non-likely

  /*
  block_11:
  c->lw(gp, 0, t4);                                 // lw gp, 0(t4)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(gp, gp, 256);                             // andi gp, gp, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(gp) == 0;                          // beq gp, r0, L119
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_13;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(gp, 288, t0);                               // lw gp, 288(t0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(gp, gp, 1);                             // daddiu gp, gp, 1
  // nop                                            // sll r0, r0, 0
  c->sw(gp, 288, t0);                               // sw gp, 288(t0)
  //beq r0, r0, L118                                // beq r0, r0, L118
  // nop                                            // sll r0, r0, 0
  goto block_11;                                    // branch always
  */


  // block_13:
  // c->sw(ra, 16, t4);                                // sw ra, 16(t4)
  madr = c->sgpr64(ra);
  // nop                                            // sll r0, r0, 0
  // c->sw(t3, 128, t4);                               // sw t3, 128(t4)
  sadr = c->sgpr64(t3);
  c->addiu(gp, r0, 64);                             // addiu gp, r0, 64
  // c->sw(gp, 32, t4);                                // sw gp, 32(t4)
  qwc = c->sgpr64(gp);
  c->addiu(gp, r0, 256);                            // addiu gp, r0, 256
  // c->sw(gp, 0, t4);                                 // sw gp, 0(t4)
  spad_to_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
  c->daddiu(ra, ra, 1024);                          // daddiu ra, ra, 1024
  //beq r0, r0, L121                                // beq r0, r0, L121
  // nop                                            // sll r0, r0, 0
  goto block_16;                                    // branch always


  block_14:
  /*
  c->lw(gp, 0, t4);                                 // lw gp, 0(t4)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(gp, gp, 256);                             // andi gp, gp, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(gp) == 0;                          // beq gp, r0, L121
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_16;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(gp, 288, t0);                               // lw gp, 288(t0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(gp, gp, 1);                             // daddiu gp, gp, 1
  // nop                                            // sll r0, r0, 0
  c->sw(gp, 288, t0);                               // sw gp, 288(t0)
  //beq r0, r0, L120                                // beq r0, r0, L120
  // nop                                            // sll r0, r0, 0
  goto block_14;                                    // branch always
  */


  block_16:
  c->addiu(gp, a1, 128);                            // addiu gp, a1, 128
  c->lq(s2, 12, s5);                                // lq s2, 12(s5)
  // nop                                            // sll r0, r0, 0
  c->lq(s4, 28, s5);                                // lq s4, 28(s5)
  c->pextlb(s3, r0, s2);                            // pextlb s3, r0, s2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextub(s2, r0, s2);                            // pextub s2, r0, s2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmulth(r0, s3, t5);                            // pmulth r0, s3, t5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(s3, r0, s4);                            // pextlb s3, r0, s4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaddh(r0, s2, t6);                            // pmaddh r0, s2, t6
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextub(s4, r0, s4);                            // pextub s4, r0, s4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaddh(r0, s3, t7);                            // pmaddh r0, s3, t7
  c->lq(s3, 44, s5);                                // lq s3, 44(s5)
  c->addiu(s5, s5, 32);                             // addiu s5, s5, 32
  // nop                                            // sll r0, r0, 0
  c->pmaddh(r0, s4, t8);                            // pmaddh r0, s4, t8
  c->lq(s4, 28, s5);                                // lq s4, 28(s5)
  c->pextlb(s2, r0, s3);                            // pextlb s2, r0, s3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31

  block_17:
  c->pextub(s3, r0, s3);                            // pextub s3, r0, s3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmfhl_lh(s1);                                  // pmfhl.lh s1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmulth(r0, s2, t5);                            // pmulth r0, s2, t5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psrlh(s2, s1, 6);                              // psrlh s2, s1, 6
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyud(s1, s2, s2);                            // pcpyud s1, s2, s2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->paddh(s2, s1, s2);                             // paddh s2, s1, s2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pminh(s2, s2, a2);                             // pminh s2, s2, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppacb(s1, r0, s2);                             // ppacb s1, r0, s2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(s2, r0, s4);                            // pextlb s2, r0, s4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaddh(r0, s3, t6);                            // pmaddh r0, s3, t6
  c->sw(s1, 0, a1);                                 // sw s1, 0(a1)
  c->pextub(s4, r0, s4);                            // pextub s4, r0, s4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaddh(r0, s2, t7);                            // pmaddh r0, s2, t7
  c->lq(s3, 44, s5);                                // lq s3, 44(s5)
  c->addiu(s5, s5, 32);                             // addiu s5, s5, 32
  c->addiu(a1, a1, 4);                              // addiu a1, a1, 4
  c->pmaddh(r0, s4, t8);                            // pmaddh r0, s4, t8
  c->lq(s4, 28, s5);                                // lq s4, 28(s5)
  bc = c->sgpr64(a1) != c->sgpr64(gp);              // bne a1, gp, L122
  c->pextlb(s2, r0, s3);                            // pextlb s2, r0, s3
  if (bc) {goto block_17;}                          // branch non-likely

  bc = ((s64)c->sgpr64(t9)) >= 0;                   // bgez t9, L117
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_10;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(a2, 10272, a3);                             // lw a2, 10272(a3)
  // nop                                            // sll r0, r0, 0
  c->lw(t6, 10292, a3);                             // lw t6, 10292(a3)
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L123
  c->lbu(t5, 10305, a3);                            // lbu t5, 10305(a3)
  if (bc) {goto block_21;}                          // branch non-likely

  // Unknown instr: bgezal r0, L127
  block29_call(c);
  // nop                                            // sll r0, r0, 0



  block_21:
  // nop                                            // sll r0, r0, 0
  c->lw(a2, 10276, a3);                             // lw a2, 10276(a3)
  // nop                                            // sll r0, r0, 0
  c->lw(t6, 10292, a3);                             // lw t6, 10292(a3)
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L124
  c->lbu(t5, 10305, a3);                            // lbu t5, 10305(a3)
  if (bc) {goto block_23;}                          // branch non-likely

  // Unknown instr: bgezal r0, L127
  // nop                                            // sll r0, r0, 0
  block29_call(c);


  block_23:
  // nop                                            // sll r0, r0, 0
  c->lw(a2, 10280, a3);                             // lw a2, 10280(a3)
  // nop                                            // sll r0, r0, 0
  c->lw(t6, 10296, a3);                             // lw t6, 10296(a3)
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L125
  c->lbu(t5, 10306, a3);                            // lbu t5, 10306(a3)
  if (bc) {goto block_25;}                          // branch non-likely

  // Unknown instr: bgezal r0, L127
  // nop                                            // sll r0, r0, 0
  block29_call(c);


  block_25:
  // nop                                            // sll r0, r0, 0
  c->lw(a2, 10284, a3);                             // lw a2, 10284(a3)
  // nop                                            // sll r0, r0, 0
  c->lw(t6, 10300, a3);                             // lw t6, 10300(a3)
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L126
  c->lbu(t5, 10307, a3);                            // lbu t5, 10307(a3)
  if (bc) {goto block_27;}                          // branch non-likely

  // Unknown instr: bgezal r0, L127
  // nop                                            // sll r0, r0, 0
  block29_call(c);


  block_27:
  // nop                                            // sll r0, r0, 0
  c->lw(a2, 280, t0);                               // lw a2, 280(t0)
  // nop                                            // sll r0, r0, 0
  c->lw(t5, 10256, a3);                             // lw t5, 10256(a3)
  // nop                                            // sll r0, r0, 0
  c->lw(t6, 12, a2);                                // lw t6, 12(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(r0, -56, a1);                               // sw r0, -56(a1)
  bc = c->sgpr64(t5) != 0;                          // bne t5, r0, L111
  c->daddiu(t7, t5, -1);                            // daddiu t7, t5, -1
  if (bc) {goto block_1;}                           // branch non-likely

  //beq r0, r0, L132                                // beq r0, r0, L132
  c->sw(r0, -52, a1);                               // sw r0, -52(a1)
  goto block_36;                                    // branch always


  ASSERT(false);

  block_36:
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L135
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_40;}                          // branch non-likely

  /*
  block_37:
  c->lw(a1, 0, t1);                                 // lw a1, 0(t1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(a1, a1, 256);                             // andi a1, a1, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L134
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_39;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(a1, 284, t0);                               // lw a1, 284(t0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(a1, a1, 1);                             // daddiu a1, a1, 1
  // nop                                            // sll r0, r0, 0
  c->sw(a1, 284, t0);                               // sw a1, 284(t0)
  //beq r0, r0, L133                                // beq r0, r0, L133
  // nop                                            // sll r0, r0, 0
  goto block_37;                                    // branch always
  */


  // block_39:
  // c->sw(t2, 128, t1);                               // sw t2, 128(t1)
  sadr = c->sgpr64(t2);
  // nop                                            // sll r0, r0, 0
  // c->sw(v1, 16, t1);                                // sw v1, 16(t1)
  madr = c->sgpr64(v1);
  c->sll(a1, a0, 4);                                // sll a1, a0, 4
  c->addu(v1, v1, a1);                              // addu v1, v1, a1
  // nop                                            // sll r0, r0, 0
  // c->sw(a0, 32, t1);                                // sw a0, 32(t1)
  qwc = c->sgpr64(a0);
  c->addiu(a0, r0, 256);                            // addiu a0, r0, 256
  // c->sw(a0, 0, t1);                                 // sw a0, 0(t1)
  spad_from_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
  // nop                                            // sll r0, r0, 0

  block_40:
  /*
  c->lw(a0, 0, t1);                                 // lw a0, 0(t1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(a0, a0, 256);                             // andi a0, a0, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L136
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_42;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(a0, 284, t0);                               // lw a0, 284(t0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 284, t0);                               // sw a0, 284(t0)
  //beq r0, r0, L135                                // beq r0, r0, L135
  // nop                                            // sll r0, r0, 0
  goto block_40;                                    // branch always
  */


  // block_42:
  c->lw(a0, 10260, a3);                             // lw a0, 10260(a3)
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 4, a0);                                 // sw v1, 4(a0)
  // nop                                            // sll r0, r0, 0
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 96, sp);                                // lq gp, 96(sp)
  c->lq(s5, 80, sp);                                // lq s5, 80(sp)
  c->lq(s4, 64, sp);                                // lq s4, 64(sp)
  c->lq(s3, 48, sp);                                // lq s3, 48(sp)
  c->lq(s2, 32, sp);                                // lq s2, 32(sp)
  c->lq(s1, 16, sp);                                // lq s1, 16(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 112);                           // daddiu sp, sp, 112
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  cache.prototype_tie_work = intern_from_c("*prototype-tie-work*").c();
  gLinkedFunctionTable.reg("draw-inline-array-prototype-tie-generic-asm", execute, 256);
}

} // namespace draw_inline_array_prototype_tie_generic_asm
} // namespace Mips2C
