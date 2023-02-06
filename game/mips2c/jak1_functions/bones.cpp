
//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak1/kscheme.h"
using namespace jak1;
namespace Mips2C::jak1 {
namespace bones_mtx_calc {

struct Cache {
  void* fake_scratchpad_data;  // *fake-scratchpad-data*
} cache;

void exec_mpg(ExecutionContext* c) {
/*
  nop                        |  mulax.xyzw ACC, vf05, vf01
  nop                        |  madday.xyzw ACC, vf06, vf01
  nop                        |  maddaz.xyzw ACC, vf07, vf01
  nop                        |  maddw.xyzw vf13, vf08, vf01
  nop                        |  mulax.xyzw ACC, vf05, vf02
  nop                        |  madday.xyzw ACC, vf06, vf02
  nop                        |  maddaz.xyzw ACC, vf07, vf02
  nop                        |  maddw.xyzw vf14, vf08, vf02
  nop                        |  mulax.xyzw ACC, vf05, vf03
  nop                        |  madday.xyzw ACC, vf06, vf03
  nop                        |  maddaz.xyzw ACC, vf07, vf03
  nop                        |  maddw.xyzw vf15, vf08, vf03
  nop                        |  mulax.xyzw ACC, vf05, vf04
  nop                        |  madday.xyzw ACC, vf06, vf04
  nop                        |  maddaz.xyzw ACC, vf07, vf04
  nop                        |  maddw.xyzw vf16, vf08, vf04
  nop                        |  opmula.xyz ACC, vf14, vf15
  nop                        |  opmsub.xyz vf09, vf15, vf14
  nop                        |  opmula.xyz ACC, vf15, vf13
  nop                        |  opmsub.xyz vf10, vf13, vf15
  nop                        |  opmula.xyz ACC, vf13, vf14
  nop                        |  mul.xyz vf12, vf13, vf09
  nop                        |  opmsub.xyz vf11, vf14, vf13
  nop                        |  mulax.xyzw ACC, vf28, vf13
  nop                        |  madday.xyzw ACC, vf29, vf13
  nop                        |  maddaz.xyzw ACC, vf30, vf13
  nop                        |  maddw.xyzw vf13, vf31, vf13
  nop                        |  mulax.w ACC, vf00, vf12
  nop                        |  madday.w ACC, vf00, vf12
  nop                        |  maddz.w vf12, vf00, vf12
  nop                        |  mulax.xyzw ACC, vf28, vf14
  nop                        |  madday.xyzw ACC, vf29, vf14
  nop                        |  maddaz.xyzw ACC, vf30, vf14
  div Q, vf00.w, vf12.w      |  maddw.xyzw vf14, vf31, vf14
  nop                        |  mulax.xyzw ACC, vf28, vf15
  nop                        |  madday.xyzw ACC, vf29, vf15
  nop                        |  maddaz.xyzw ACC, vf30, vf15
  nop                        |  maddw.xyzw vf15, vf31, vf15
  nop                        |  mulax.xyzw ACC, vf28, vf16
  nop                        |  madday.xyzw ACC, vf29, vf16
  nop                        |  maddaz.xyzw ACC, vf30, vf16
  nop                        |  maddw.xyzw vf16, vf31, vf16
  nop                        |  mul.xyzw vf09, vf09, Q
  nop                        |  mul.xyzw vf10, vf10, Q
  nop                        |  mul.xyzw vf11, vf11, Q
  nop                        |  mulax.xyzw ACC, vf25, vf09
  nop                        |  madday.xyzw ACC, vf26, vf09
  nop                        |  maddz.xyzw vf09, vf27, vf09
  nop                        |  mulax.xyzw ACC, vf25, vf10
  nop                        |  madday.xyzw ACC, vf26, vf10
  nop                        |  maddz.xyzw vf10, vf27, vf10
  nop                        |  mulax.xyzw ACC, vf25, vf11
  nop                        |  madday.xyzw ACC, vf26, vf11 :e
  nop                        |  maddz.xyzw vf11, vf27, vf11
 */

//  printf("vf1 is %f %f %f %f\n", c->vfs[vf1].f[0], c->vfs[vf1].f[1], c->vfs[vf1].f[2], c->vfs[vf1].f[3]);
  c->vmula_bc(DEST::xyzw, BC::x, vf05, vf1);
  c->vmadda_bc(DEST::xyzw, BC::y, vf06, vf01);
  c->vmadda_bc(DEST::xyzw, BC::z, vf07, vf01);
  c->vmadd_bc(DEST::xyzw, BC::w, vf13, vf08, vf01);
  c->vmula_bc(DEST::xyzw, BC::x, vf05, vf02);
  c->vmadda_bc(DEST::xyzw, BC::y, vf06, vf02);
  c->vmadda_bc(DEST::xyzw, BC::z, vf07, vf02);
  c->vmadd_bc(DEST::xyzw, BC::w, vf14, vf08, vf02);
  c->vmula_bc(DEST::xyzw, BC::x, vf05, vf03);
  c->vmadda_bc(DEST::xyzw, BC::y, vf06, vf03);
  c->vmadda_bc(DEST::xyzw, BC::z, vf07, vf03);
  c->vmadd_bc(DEST::xyzw, BC::w, vf15, vf08, vf03);
  c->vmula_bc(DEST::xyzw, BC::x, vf05, vf04);
  c->vmadda_bc(DEST::xyzw, BC::y, vf06, vf04);
  c->vmadda_bc(DEST::xyzw, BC::z, vf07, vf04);
  c->vmadd_bc(DEST::xyzw, BC::w, vf16, vf08, vf04);
//  printf("vf05 is %f %f %f %f\n", c->vfs[vf05].f[0], c->vfs[vf05].f[1], c->vfs[vf05].f[2], c->vfs[vf05].f[3]);

//  printf("vf06 is %f %f %f %f\n", c->vfs[vf06].f[0], c->vfs[vf06].f[1], c->vfs[vf06].f[2], c->vfs[vf06].f[3]);
  c->vopmula(vf14, vf15);
  c->vopmsub(vf09, vf15, vf14);
  c->vopmula(vf15, vf13);
  c->vopmsub(vf10, vf13, vf15);
  c->vopmula(vf13, vf14);
  //nop                        |  mul.xyz vf12, vf13, vf09
  c->vmul(DEST::xyz, vf12, vf13, vf09);
//  printf("vf12 is %f %f %f %f\n", c->vfs[vf12].f[0], c->vfs[vf12].f[1], c->vfs[vf12].f[2], c->vfs[vf12].f[3]);
//  printf("vf13 is %f %f %f %f\n", c->vfs[vf13].f[0], c->vfs[vf13].f[1], c->vfs[vf13].f[2], c->vfs[vf13].f[3]);

//  printf("vf09 is %f %f %f %f\n", c->vfs[vf09].f[0], c->vfs[vf09].f[1], c->vfs[vf09].f[2], c->vfs[vf09].f[3]);

  c->vopmsub(vf11, vf14, vf13);
  c->vmula_bc(DEST::xyzw, BC::x, vf28, vf13);
  c->vmadda_bc(DEST::xyzw, BC::y, vf29, vf13);
  c->vmadda_bc(DEST::xyzw, BC::z, vf30, vf13);
  c->vmadd_bc(DEST::xyzw, BC::w, vf13, vf31, vf13);
  //nop                        |  mulax.w ACC, vf00, vf12
  c->vmula_bc(DEST::w, BC::x, vf0, vf12);
  //nop                        |  madday.w ACC, vf00, vf12
  c->vmadda_bc(DEST::w, BC::y, vf0, vf12);
  //nop                        |  maddz.w vf12, vf00, vf12
  c->vmadd_bc(DEST::w, BC::z, vf12, vf0, vf12);
  c->vmula_bc(DEST::xyzw, BC::x, vf28, vf14);
  c->vmadda_bc(DEST::xyzw, BC::y, vf29, vf14);
  c->vmadda_bc(DEST::xyzw, BC::z, vf30, vf14);
  //div Q, vf00.w, vf12.w      |  maddw.xyzw vf14, vf31, vf14
  c->vdiv(vf0, BC::w, vf12, BC::w);
//printf("vf12.w is %f\n", c->vfs[vf12].f[3]);
  c->vmadd_bc(DEST::xyzw, BC::w, vf14, vf31, vf14);
  c->vmula_bc(DEST::xyzw, BC::x, vf28, vf15);
  c->vmadda_bc(DEST::xyzw, BC::y, vf29, vf15);
  c->vmadda_bc(DEST::xyzw, BC::z, vf30, vf15);
  c->vmadd_bc(DEST::xyzw, BC::w, vf15, vf31, vf15);
  c->vmula_bc(DEST::xyzw, BC::x, vf28, vf16);
  c->vmadda_bc(DEST::xyzw, BC::y, vf29, vf16);
  c->vmadda_bc(DEST::xyzw, BC::z, vf30, vf16);
  c->vmadd_bc(DEST::xyzw, BC::w, vf16, vf31, vf16);
  //nop                        |  mul.xyzw vf09, vf09, Q
  c->vmulq(DEST::xyzw, vf09, vf09);
  // nop                        |  mul.xyzw vf10, vf10, Q
  c->vmulq(DEST::xyzw, vf10, vf10);
  // nop                        |  mul.xyzw vf11, vf11, Q
  c->vmulq(DEST::xyzw, vf11, vf11);
  c->vmula_bc(DEST::xyzw, BC::x, vf25, vf09);
  c->vmadda_bc(DEST::xyzw, BC::y, vf26, vf09);
  //nop                        |  maddz.xyzw vf09, vf27, vf09
  c->vmadd_bc(DEST::xyzw, BC::z, vf09, vf27, vf09);
  c->vmula_bc(DEST::xyzw, BC::x, vf25, vf10);
  c->vmadda_bc(DEST::xyzw, BC::y, vf26, vf10);
  c->vmadd_bc(DEST::xyzw, BC::z, vf10, vf27, vf10);
  c->vmula_bc(DEST::xyzw, BC::x, vf25, vf11);
  c->vmadda_bc(DEST::xyzw, BC::y, vf26, vf11); // :e
  c->vmadd_bc(DEST::xyzw, BC::z, vf11, vf27, vf11);
}

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
//printf("start\n");
  bool bc = false;
  u32 madr, sadr, qwc;
  // hack, added this that should be loaded by the caller.
  //  lqc2 vf28, 0(v1)          ;; [ 60] (set! vf28 (l.vf v1-13)) [v1: matrix ] -> []
  c->lqc2(vf28, 0, t0);
  //  lqc2 vf29, 16(v1)         ;; [ 61] (set! vf29 (l.vf (+ v1-13 16))) [v1: matrix ] -> []
  c->lqc2(vf29, 16, t0);
  //  lqc2 vf30, 32(v1)         ;; [ 62] (set! vf30 (l.vf (+ v1-13 32))) [v1: matrix ] -> []
  c->lqc2(vf30, 32, t0);
  //  lqc2 vf31, 48(v1)         ;; [ 63] (set! vf31 (l.vf (+ v1-13 48))) [v1: matrix ] -> []
  c->lqc2(vf31, 48, t0);
  //  lqc2 vf25, 0(v1)          ;; [ 64] (set! vf25 (l.vf v1-13)) [v1: matrix ] -> []
  c->lqc2(vf25, 0, t0);
  //  lqc2 vf26, 16(v1)         ;; [ 65] (set! vf26 (l.vf (+ v1-13 16))) [v1: matrix ] -> []
  c->lqc2(vf26, 16, t0);
  //  lqc2 vf27, 32(v1)         ;; [ 66] (set! vf27 (l.vf (+ v1-13 32))) [v1: matrix ] -> []
  c->lqc2(vf27, 32, t0);

  c->daddiu(sp, sp, -96);                           // daddiu sp, sp, -96
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s2, 16, sp);                                // sq s2, 16(sp)
  c->sq(s3, 32, sp);                                // sq s3, 32(sp)
  c->sq(s4, 48, sp);                                // sq s4, 48(sp)
  c->sq(s5, 64, sp);                                // sq s5, 64(sp)
  c->sq(gp, 80, sp);                                // sq gp, 80(sp)
  c->lui(v1, 4096);                                 // lui v1, 4096
  c->lui(t0, 4096);                                 // lui t0, 4096
  c->ori(v1, v1, 54272);                            // ori v1, v1, 54272 0xd400 (spr to)
  c->ori(t0, t0, 53248);                            // ori t0, t0, 53248 0xd000 (spr from)
  c->lui(t2, 32767);                                // lui t2, 32767
  c->daddiu(t1, a3, -16);                           // daddiu t1, a3, -16
  c->ori(t2, t2, 65535);                            // ori t2, t2, 65535
  // c->lui(at, 28672);                                // lui at, 28672 <- spr
  get_fake_spad_addr(at, cache.fake_scratchpad_data, 0, c);
  c->addiu(t4, r0, 64);                             // addiu t4, r0, 64
  c->addiu(t5, r0, 1280);                           // addiu t5, r0, 1280
  bc = ((s64)c->sgpr64(t1)) >= 0;                   // bgez t1, L117
  c->addiu(t3, r0, 16);                             // addiu t3, r0, 16
  if (bc) {goto block_2;}                           // branch non-likely

  c->addiu(t1, r0, 0);                              // addiu t1, r0, 0
  c->mov64(t3, a3);                                 // or t3, a3, r0
  c->dsll(t4, t3, 2);                               // dsll t4, t3, 2
  c->dsll(a3, t3, 4);                               // dsll a3, t3, 4
  c->dsll(t5, t3, 6);                               // dsll t5, t3, 6
  c->daddu(t5, t5, a3);                             // daddu t5, t5, a3

  block_2:
  c->addiu(a3, r0, 0);                              // addiu a3, r0, 0
  c->addiu(t6, r0, 1);                              // addiu t6, r0, 1
  c->and_(a1, a1, t2);                              // and a1, a1, t2
  // nop                                            // sll r0, r0, 0
  c->daddiu(a1, a1, 12);                            // daddiu a1, a1, 12
  c->mov64(a0, a0);                                 // or a0, a0, r0
  c->daddiu(a1, a1, -80);                           // daddiu a1, a1, -80
  // nop                                            // sll r0, r0, 0

  /*
  block_3:
  c->lw(t6, 0, v1);                                 // lw t6, 0(v1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(t6, t6, 256);                             // andi t6, t6, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t6) == 0;                          // beq t6, r0, L119
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_5;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L118                                // beq r0, r0, L118
  // nop                                            // sll r0, r0, 0
  goto block_3;                                     // branch always
  */


  c->addiu(t6, r0, 256);                            // addiu t6, r0, 256
  c->addiu(t7, r0, 264);                            // addiu t7, r0, 264
  // c->sw(t6, 128, v1);                               // sw t6, 128(v1) sadr
  // c->sw(a1, 16, v1);                                // sw a1, 16(v1)  madr
  // c->sw(t4, 32, v1);                                // sw t4, 32(v1)  qwc
  // c->sw(t7, 0, v1);                                 // sw t7, 0(v1)   go!
  spad_to_dma_no_sadr_off_bones_interleave(cache.fake_scratchpad_data, c->sgpr64(a1), c->sgpr64(t6), c->sgpr64(t4));
  c->daddu(a1, a1, t5);                             // daddu a1, a1, t5

  /*
  block_6:
  c->lw(t4, 0, v1);                                 // lw t4, 0(v1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(t4, t4, 256);                             // andi t4, t4, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L121
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_8;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L120                                // beq r0, r0, L120
  // nop                                            // sll r0, r0, 0
  goto block_6;                                     // branch always
  */


  c->and_(a2, a2, t2);                              // and a2, a2, t2
  c->dsll(t2, t3, 1);                               // dsll t2, t3, 1
  c->dsll(t5, t3, 2);                               // dsll t5, t3, 2
  c->addiu(t4, r0, 256);                            // addiu t4, r0, 256
  c->daddu(t2, t5, t2);                             // daddu t2, t5, t2
  c->addiu(t6, r0, 1280);                           // addiu t6, r0, 1280
  c->dsll(t5, t2, 4);                               // dsll t5, t2, 4
  // c->sw(t6, 128, v1);                               // sw t6, 128(v1)
  sadr = c->sgpr64(t6);
  c->addiu(t8, r0, 0);                              // addiu t8, r0, 0
  // c->sw(a2, 16, v1);                                // sw a2, 16(v1)
  madr = c->sgpr64(a2);
  c->daddu(a2, a2, t5);                             // daddu a2, a2, t5
  // c->sw(t2, 32, v1);                                // sw t2, 32(v1)
  qwc = c->sgpr64(t2);
  c->addiu(t2, r0, 1);                              // addiu t2, r0, 1
  // c->sw(t4, 0, v1);                                 // sw t4, 0(v1)
  spad_to_dma_no_sadr_off(cache.fake_scratchpad_data, madr, sadr, qwc);


  block_9:
  /*
  c->lw(t5, 0, v1);                                 // lw t5, 0(v1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(t5, t5, 256);                             // andi t5, t5, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L123
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_11;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L122                                // beq r0, r0, L122
  // nop                                            // sll r0, r0, 0
  goto block_9;                                     // branch always
  */


  c->dsll(t5, t8, 2);                               // dsll t5, t8, 2
  c->daddu(t9, t5, at);                             // daddu t9, t5, at
  // nop                                            // sll r0, r0, 0
  c->lwu(t5, 16, t9);                               // lwu t5, 16(t9)
  c->mov64(t6, t3);                                 // or t6, t3, r0
  c->lwu(t7, 24, t9);                               // lwu t7, 24(t9)
  c->mov64(ra, t3);                                 // or ra, t3, r0
  c->lwu(t3, 32, t9);                               // lwu t3, 32(t9)
  // nop                                            // sll r0, r0, 0
  c->sw(ra, 232, at);                               // sw ra, 232(at)
  bc = c->sgpr64(ra) == 0;                          // beq ra, r0, L136
  c->sw(t8, 236, at);                               // sw t8, 236(at)
  if (bc) {goto block_33;}                          // branch non-likely

  c->daddiu(t1, t1, -16);                           // daddiu t1, t1, -16
  c->addiu(t9, r0, 1280);                           // addiu t9, r0, 1280
  bc = ((s64)c->sgpr64(t1)) >= 0;                   // bgez t1, L124
  c->addiu(t8, r0, 16);                             // addiu t8, r0, 16
  if (bc) {goto block_14;}                          // branch non-likely

  c->daddiu(t8, t1, 16);                            // daddiu t8, t1, 16
  c->addiu(t1, r0, 0);                              // addiu t1, r0, 0
  c->dsll(t9, t8, 4);                               // dsll t9, t8, 4
  c->dsll(ra, t8, 6);                               // dsll ra, t8, 6
  bc = c->sgpr64(t8) == 0;                          // beq t8, r0, L125
  c->daddu(t9, ra, t9);                             // daddu t9, ra, t9
  if (bc) {goto block_15;}                          // branch non-likely


  block_14:
  c->dsll(t4, t8, 2);                               // dsll t4, t8, 2
  c->dsll(ra, t2, 2);                               // dsll ra, t2, 2
  c->daddu(gp, ra, at);                             // daddu gp, ra, at
  // c->sw(a1, 16, v1);                                // sw a1, 16(v1)
  madr = c->sgpr64(a1);
  c->addiu(ra, r0, 264);                            // addiu ra, r0, 264
  c->lwu(gp, 16, gp);                               // lwu gp, 16(gp)
  c->andi(gp, gp, 16383);                           // andi gp, gp, 16383
  // c->sw(t4, 32, v1);                                // sw t4, 32(v1)
  qwc = c->sgpr64(t4);
  c->daddu(a1, a1, t9);                             // daddu a1, a1, t9
  // c->sw(gp, 128, v1);                               // sw gp, 128(v1)
  sadr = c->sgpr64(gp);
  c->addiu(t4, r0, 0);                              // addiu t4, r0, 0
  // c->sw(ra, 0, v1);                                 // sw ra, 0(v1)
  spad_to_dma_no_sadr_off_bones_interleave(cache.fake_scratchpad_data, madr, sadr, qwc);

  block_15:
  // nop                                            // sll r0, r0, 0
  c->sw(t8, 228, at);                               // sw t8, 228(at)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf1, 0, t5);                              // lqc2 vf1, 0(t5)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 16, t5);                             // lqc2 vf2, 16(t5)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 32, t5);                             // lqc2 vf3, 32(t5)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf4, 48, t5);                             // lqc2 vf4, 48(t5)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf5, 0, t7);                              // lqc2 vf5, 0(t7)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf6, 16, t7);                             // lqc2 vf6, 16(t7)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf7, 32, t7);                             // lqc2 vf7, 32(t7)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf8, 48, t7);                             // lqc2 vf8, 48(t7)
  // Unknown instr: vcallms 0
  exec_mpg(c);
  // nop                                            // sll r0, r0, 0

  block_16:
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(t5, t5, 64);                            // daddiu t5, t5, 64
  // nop                                            // sll r0, r0, 0
  c->daddiu(t7, t7, 96);                            // daddiu t7, t7, 96
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lq(t8, 0, t5);                                 // lq t8, 0(t5)
//  printf("load is 0x%lx, 0x%lx\n", c->sgpr64(t5), c->sgpr64(t7));
//  printf("t8: %f %f %f\n", c->gprs[t8].f[0], c->gprs[t8].f[1], c->gprs[t8].f[2]);

  // nop                                            // sll r0, r0, 0
  c->lq(t9, 16, t5);                                // lq t9, 16(t5)
  // nop                                            // sll r0, r0, 0
  c->lq(ra, 32, t5);                                // lq ra, 32(t5)
  // nop                                            // sll r0, r0, 0
  c->lq(gp, 48, t5);                                // lq gp, 48(t5)
//  printf("t5: %f %f %f\n", c->gprs[gp].f[0], c->gprs[gp].f[1], c->gprs[gp].f[2]);
  // nop                                            // sll r0, r0, 0
  c->lq(s5, 0, t7);                                 // lq s5, 0(t7)
  // nop                                            // sll r0, r0, 0
  c->lq(s4, 16, t7);                                // lq s4, 16(t7)
  // nop                                            // sll r0, r0, 0
  c->lq(s3, 32, t7);                                // lq s3, 32(t7)
  // nop                                            // sll r0, r0, 0
  c->lq(s2, 48, t7);                                // lq s2, 48(t7)
//  printf("t7: %f %f %f\n", c->gprs[s3].f[0], c->gprs[s3].f[1], c->gprs[s3].f[2]);
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf1, t8);                        // qmtc2.ni vf1, t8
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf2, t9);                        // qmtc2.ni vf2, t9
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf3, ra);                        // qmtc2.ni vf3, ra
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf4, gp);                        // qmtc2.ni vf4, gp
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf5, s5);                        // qmtc2.ni vf5, s5
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf6, s4);                        // qmtc2.ni vf6, s4
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf7, s3);                        // qmtc2.ni vf7, s3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf8, s2);                        // qmtc2.ni vf8, s2
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t8, vf13);                       // qmfc2.i t8, vf13
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t9, vf14);                       // qmfc2.ni t9, vf14
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(ra, vf15);                       // qmfc2.ni ra, vf15
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(gp, vf16);                       // qmfc2.ni gp, vf16
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(s5, vf9);                        // qmfc2.ni s5, vf9
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(s4, vf10);                       // qmfc2.ni s4, vf10
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(s3, vf11);                       // qmfc2.ni s3, vf11
  // Unknown instr: vcallms 0
  exec_mpg(c);
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
//  printf("store is 0x%lx\n", c->sgpr64(t3));
  c->sq(t8, 0, t3);                                 // sq t8, 0(t3)
  // nop                                            // sll r0, r0, 0
  c->sq(t9, 16, t3);                                // sq t9, 16(t3)
  // nop                                            // sll r0, r0, 0
  c->sq(ra, 32, t3);                                // sq ra, 32(t3)
  // nop                                            // sll r0, r0, 0
  c->sq(gp, 48, t3);                                // sq gp, 48(t3)
  // nop                                            // sll r0, r0, 0
  c->sq(s5, 64, t3);                                // sq s5, 64(t3)
  // nop                                            // sll r0, r0, 0
  c->sq(s4, 80, t3);                                // sq s4, 80(t3)
  // nop                                            // sll r0, r0, 0
  c->sq(s3, 96, t3);                                // sq s3, 96(t3)
  // nop                                            // sll r0, r0, 0
  c->sq(r0, 112, t3);                               // sq r0, 112(t3)
  c->daddiu(t3, t3, 128);                           // daddiu t3, t3, 128
  c->daddiu(t6, t6, -1);                            // daddiu t6, t6, -1
  bc = ((s64)c->sgpr64(t6)) > 0;                    // bgtz t6, L126
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_16;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(t3, 228, at);                               // lw t3, 228(at)
  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L129
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_21;}                          // branch non-likely

  /*
  block_18:
  c->lw(t4, 0, v1);                                 // lw t4, 0(v1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(t4, t4, 256);                             // andi t4, t4, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L128
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_20;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L127                                // beq r0, r0, L127
  // nop                                            // sll r0, r0, 0
  goto block_18;                                    // branch always
  */


  c->dsll(t5, t2, 2);                               // dsll t5, t2, 2
  // nop                                            // sll r0, r0, 0
  c->addiu(t4, r0, 1);                              // addiu t4, r0, 1
  c->daddu(t6, t5, at);                             // daddu t6, t5, at
  c->dsll(t5, t3, 1);                               // dsll t5, t3, 1
  c->lwu(t7, 24, t6);                               // lwu t7, 24(t6)
  c->dsll(t6, t3, 2);                               // dsll t6, t3, 2
  c->andi(t7, t7, 16383);                           // andi t7, t7, 16383
  c->daddu(t5, t6, t5);                             // daddu t5, t6, t5
  // c->sw(t7, 128, v1);                               // sw t7, 128(v1)
  sadr = c->sgpr64(t7);
  c->dsll(t6, t5, 4);                               // dsll t6, t5, 4
  //c->sw(a2, 16, v1);                                // sw a2, 16(v1)
  madr = c->sgpr64(a2);
  c->addiu(t7, r0, 256);                            // addiu t7, r0, 256
  // c->sw(t5, 32, v1);                                // sw t5, 32(v1)
  qwc = c->sgpr64(t5);
  c->daddu(a2, a2, t6);                             // daddu a2, a2, t6
  // c->sw(t7, 0, v1);                                 // sw t7, 0(v1)
  spad_to_dma_no_sadr_off(cache.fake_scratchpad_data, madr, sadr, qwc);

  block_21:
  // nop                                            // sll r0, r0, 0
  c->lw(t5, 236, at);                               // lw t5, 236(at)
  // nop                                            // sll r0, r0, 0
  c->lw(t6, 232, at);                               // lw t6, 232(at)

  /*
  block_22:
  c->lw(t7, 0, t0);                                 // lw t7, 0(t0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(t7, t7, 256);                             // andi t7, t7, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L131
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_24;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L130                                // beq r0, r0, L130
  // nop                                            // sll r0, r0, 0
  goto block_22;                                    // branch always
  */


  bc = c->sgpr64(t6) == 0;                          // beq t6, r0, L132
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_26;}                          // branch non-likely

  c->dsll(t7, t5, 2);                               // dsll t7, t5, 2
  // c->lui(t8, 28672);                                // lui t8, 28672
  get_fake_spad_addr(t8, cache.fake_scratchpad_data, 0, c);
  c->daddu(t7, t7, t8);                             // daddu t7, t7, t8
  c->lwu(t7, 32, t7);                               // lwu t7, 32(t7)
  c->andi(t7, t7, 16383);                           // andi t7, t7, 16383
  //c->sw(t7, 128, t0);                               // sw t7, 128(t0)
  sadr = c->sgpr64(t7);
  //c->sw(a0, 16, t0);                                // sw a0, 16(t0)
  madr = c->sgpr64(a0);
  c->dsll(t7, t6, 3);                               // dsll t7, t6, 3
  // c->sw(t7, 32, t0);                                // sw t7, 32(t0)
  qwc = c->sgpr64(t7);
  c->addiu(t7, r0, 256);                            // addiu t7, r0, 256
  // c->sw(t7, 0, t0);                                 // sw t7, 0(t0)
  spad_from_dma_no_sadr_off(cache.fake_scratchpad_data, madr, sadr, qwc);
  c->dsll(t6, t6, 7);                               // dsll t6, t6, 7
  c->daddu(a0, a0, t6);                             // daddu a0, a0, t6

  block_26:
  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L135
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_31;}                          // branch non-likely

  bc = c->sgpr64(t4) != 0;                          // bne t4, r0, L135
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_31;}                          // branch non-likely

  /*
  block_28:
  c->lw(t6, 0, v1);                                 // lw t6, 0(v1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(t6, t6, 256);                             // andi t6, t6, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t6) == 0;                          // beq t6, r0, L134
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_30;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L133                                // beq r0, r0, L133
  // nop                                            // sll r0, r0, 0
  goto block_28;                                    // branch always
  */


  c->dsll(t6, t2, 2);                               // dsll t6, t2, 2
  // c->lui(t7, 28672);                                // lui t7, 28672
  get_fake_spad_addr(t7, cache.fake_scratchpad_data, 0, c);
  c->daddu(t6, t6, t7);                             // daddu t6, t6, t7
  c->lwu(t6, 24, t6);                               // lwu t6, 24(t6)
  c->andi(t6, t6, 16383);                           // andi t6, t6, 16383
  //c->sw(t6, 128, v1);                               // sw t6, 128(v1)
  sadr = c->sgpr64(t6);
  //c->sw(a2, 16, v1);                                // sw a2, 16(v1)
  madr = c->sgpr64(a2);
  c->addiu(t6, r0, 6);                              // addiu t6, r0, 6
  c->mult3(t6, t6, t3);                             // mult3 t6, t6, t3
  //c->sw(t6, 32, v1);                                // sw t6, 32(v1)
  qwc = c->sgpr64(t6);
  c->addiu(t6, r0, 256);                            // addiu t6, r0, 256
  //c->sw(t6, 0, v1);                                 // sw t6, 0(v1)
  spad_to_dma_no_sadr_off(cache.fake_scratchpad_data, madr, sadr, qwc);
  c->addiu(t6, r0, 96);                             // addiu t6, r0, 96
  c->mult3(t6, t6, t3);                             // mult3 t6, t6, t3
  c->daddu(a2, a2, t6);                             // daddu a2, a2, t6

  block_31:
  c->mov64(t8, t2);                                 // or t8, t2, r0
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L122
  c->mov64(t2, t5);                                 // or t2, t5, r0
  if (bc) {goto block_9;}                           // branch non-likely

  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L122
  c->addiu(a3, r0, 1);                              // addiu a3, r0, 1
  if (bc) {goto block_9;}                           // branch non-likely


  block_33:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 80, sp);                                // lq gp, 80(sp)
  c->lq(s5, 64, sp);                                // lq s5, 64(sp)
  c->lq(s4, 48, sp);                                // lq s4, 48(sp)
  c->lq(s3, 32, sp);                                // lq s3, 32(sp)
  c->lq(s2, 16, sp);                                // lq s2, 16(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 96);                            // daddiu sp, sp, 96
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("bones-mtx-calc", execute, 256);
}

} // namespace bones_mtx_calc
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace draw_bones_merc {
struct Cache {
  void* merc_bucket_info; // *merc-bucket-info*
  void* merc_global_stats; // *merc-global-stats*
  void* fake_scratchpad_data;
} cache;

/*
 (deftype merc-effect-bucket-info (structure)
  ((color-fade    rgba   :offset-assert   0)
   (use-mercneric uint8  :offset-assert   4)
   (ignore-alpha  uint8  :offset-assert   5)
   (pad0          uint8  :offset-assert   6)
   (pad1          uint8  :offset-assert   7)
   )
  :pack-me
  :method-count-assert 9
  :size-assert         #x8
  :flag-assert         #x900000008
  )

;; information for everything being submitted.
(deftype merc-bucket-info (structure)
  ((light                       vu-lights               :inline    :offset-assert   0)
   (needs-clip                  int32                              :offset-assert 112)
   (need-mercprime-if-merc      int32                              :offset-assert 116)
   (must-use-mercneric-for-clip int32                              :offset-assert 120)
   (effect                      merc-effect-bucket-info 16 :inline :offset-assert 124)
   )
  :method-count-assert 9
  :size-assert         #xfc
  :flag-assert         #x9000000fc
  )
 */

struct MercEffectBucketInfo {
  u8 color_fade[4];
  u8 use_mercneric;
  u8 ignore_alpha;
  u8 pad0;
  u8 pad1;
};

struct MercBucketInfo {
  u8 lights[0x70];
  u32 needs_clip;
  u32 mercprime;
  u32 mercneric;
  MercEffectBucketInfo effects[16];
};
static_assert(sizeof(MercBucketInfo) == 0xfc);

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->daddiu(sp, sp, -128);                          // daddiu sp, sp, -128
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s0, 16, sp);                                // sq s0, 16(sp)
  c->sq(s1, 32, sp);                                // sq s1, 32(sp)
  c->sq(s2, 48, sp);                                // sq s2, 48(sp)
  c->sq(s3, 64, sp);                                // sq s3, 64(sp)
  c->sq(s4, 80, sp);                                // sq s4, 80(sp)
  c->sq(s5, 96, sp);                                // sq s5, 96(sp)
  c->sq(gp, 112, sp);                               // sq gp, 112(sp)
  c->mov64(t8, a3);                                 // or t8, a3, r0
  c->mov64(v1, t0);                                 // or v1, t0, r0
  c->lui(t0, 4096);                                 // lui t0, 4096      0x1000
  c->lui(t1, 18304);                                // lui t1, 18304     0x4780
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  c->dsll32(t1, t1, 0);                             // dsll32 t1, t1, 0
  c->lui(a3, 12288);                                // lui a3, 12288     0x3000
  c->lui(t7, 19201);                                // lui t7, 19201     0x4B01
  c->pcpyld(t0, a3, t0);                            // pcpyld t0, a3, t0
  c->lbu(a3, 58, a0);                               // lbu a3, 58(a0)
  c->pcpyld(t1, t7, t1);                            // pcpyld t1, t7, t1
  c->lui(t2, 28160);                                // lui t2, 28160    0x6E00
  c->addiu(t7, r0, 8);                              // addiu t7, r0, 8
  c->multu3(a3, a3, t7);                            // multu3 a3, a3, t7
  c->lui(t3, 1280);                                 // lui t3, 1280    0x500
  c->lui(t4, 27648);                                // lui t4, 27648   0x6C00
  c->dsll32(t2, t2, 0);                             // dsll32 t2, t2, 0
  c->dsll32(t4, t4, 0);                             // dsll32 t4, t4, 0
  c->daddu(t4, t4, t3);                             // daddu t4, t4, t3
  c->daddu(t3, t2, t3);                             // daddu t3, t2, t3
  c->daddiu(t3, t3, 1);                             // daddiu t3, t3, 1
  c->daddu(a0, a3, a0);                             // daddu a0, a3, a0
  c->pcpyld(t2, t2, r0);                            // pcpyld t2, t2, r0
  c->lw(t7, 24, a0);                                // lw t7, 24(a0)      // load the merc-ctrl!
  c->pcpyld(t3, t3, r0);                            // pcpyld t3, t3, r0
  c->pcpyld(t4, t4, r0);                            // pcpyld t4, t4, r0
  c->mov64(a0, a2);                                 // or a0, a2, r0
  c->lui(t5, 12288);                                // lui t5, 12288     // 0x3000
  c->lui(t6, 4096);                                 // lui t6, 4096
  c->daddiu(t5, t5, 7);                             // daddiu t5, t5, 7
  c->lui(t9, 5120);                                 // lui t9, 5120
  c->lui(a3, 27655);                                // lui a3, 27655
  c->daddu(t8, t9, t8);                             // daddu t8, t9, t8
  c->dsll32(a3, a3, 0);                             // dsll32 a3, a3, 0
  c->dsll32(t9, t8, 0);                             // dsll32 t9, t8, 0
  c->pcpyld(t5, a3, t5);                            // pcpyld t5, a3, t5
  c->lwu(t8, 52, t7);                               // lwu t8, 52(t7)    // effect count.
  c->pcpyld(t6, t9, t6);                            // pcpyld t6, t9, t6
  c->daddiu(t9, t7, 108);                           // daddiu t9, t7, 108 // the actual merc-effect
  c->load_symbol(a3, cache.merc_bucket_info);       // lw a3, *merc-bucket-info*(s7)

  // PC HACK: built a bitmask of which effects end up using mercneric.
  const MercBucketInfo* mbi = (const MercBucketInfo*)(g_ee_main_mem + c->sgpr64(a3));
  u16 use_pc_merc_bits = 0;
  u16 ignore_alpha_bits = 0;
  for (int i = 0; i < 16; i++) {
    if (!mbi->effects[i].use_mercneric) {
      use_pc_merc_bits |= (1 << i);
    }
    if (mbi->effects[i].ignore_alpha) {
      ignore_alpha_bits |= (1 << i);
    }
  }

  c->daddiu(ra, a3, 124);                           // daddiu ra, a3, 124  // effect bucket infos

  // effect loop!
  block_1:
  c->lbu(gp, 4, ra);                                // lbu gp, 4(ra)   effect.use-mercneric
  c->load_symbol(a3, cache.merc_global_stats);      // lw a3, *merc-global-stats*(s7)
  c->daddu(a3, r0, a3);                             // daddu a3, r0, a3
  bc = c->sgpr64(gp) != 0;                          // bne gp, r0, L77
  c->lhu(s4, 2, a3);                                // lhu s4, 2(a3) merc-global-stats.merc.fragments
  if (bc) {goto block_11;}                          // branch non-likely skip mercneric effects

  // ra is the effect-info
  // t9 is the effect
  // a3 is merc-global-stats
  c->lhu(s3, 18, t9);                               // lhu s3, 18(t9) // s3 = effect.frag-count
  c->lwu(gp, 4, a3);                                // lwu gp, 4(a3)  // gp = global-stats.merc.tris
  c->lhu(s5, 22, t9);                               // lhu s5, 22(t9) // s5 = merc-effect.tri-count
  c->daddu(s4, s4, s3);                             // daddu s4, s4, s3 // inc frag count
  c->lwu(s3, 8, a3);                                // lwu s3, 8(a3)    // s3 = global-stats.merc.dverts
  c->lhu(s2, 24, t9);                               // lhu s2, 24(t9)   // s2 = merc-effect.dvert-count
  c->daddu(gp, gp, s5);                             // daddu gp, gp, s5 // inc tri count
  c->sh(s4, 2, a3);                                 // sh s4, 2(a3)     // store frag count
  c->sw(gp, 4, a3);                                 // sw gp, 4(a3)     // store tri count
  c->daddu(s5, s3, s2);                             // daddu s5, s3, s2 // inc dvert count
  c->lwu(t2, 0, t9);                                // lwu t2, 0(t9)    // t2 = merc-fragment
  c->lwu(gp, 4, t9);                                // lwu gp, 4(t9)    // gp = merc-fragment-control
  c->lui(s4, 12288);                                // lui s4, 12288    // dma thing?
  c->dsll32(t2, t2, 0);                             // dsll32 t2, t2, 0 // dma merc-fragment
  c->sw(s5, 8, a3);                                 // sw s5, 8(a3)     // store dvert stats
  c->or_(t2, t2, s4);                               // or t2, t2, s4    // lower 64 of dma tag?
  c->addiu(s5, r0, 0);                              // addiu s5, r0, 0  // s5 = 0 (frag counter)
  c->lhu(s4, 18, t9);                               // lhu s4, 18(t9)   // s4 = effect.frag-count

  // fragment loop
  // (note: frag 0 gets added before header)
  // 0 = strow tag
  // 16 = strow data
  // 32 =
  //  DMA: 0xe1e903000000f
  //  vif0: 0x0
  //  vif1: 0x6e39c08c
  //  unpack8 (top true): 140, 57


block_3:
  c->lbu(s0, 0, gp);                                // lbu s0, 0(gp)   // s0 = fragment-control.unsigned-four-count (4-byte word count in EE mem)
  // nop                                            // sll r0, r0, 0
  c->lbu(s2, 1, gp);                                // lbu s2, 1(gp)       // s2 = fragment-control.lump-four-count
  c->xori(s1, r0, 49292);                           // xori s1, r0, 49292  // maybe vif crap 0xC08C
  c->lbu(s3, 2, gp);                                // lbu s3, 2(gp)       // s3 = fragment-control.fp-qwc
  c->daddiu(v0, s0, 3);                             // daddiu v0, s0, 3    // v0 = unsigned-four-count + 3
  c->lw(a3, 44, t7);                                // lw a3, 44(t7)       // a3 = merc-ctrl.header.st-vif-add
  c->srl(v0, v0, 2);                                // srl v0, v0, 2       // v0 = qwc to transfer for unsigned-four-count

  c->sq(t0, 0, a2);                                 // sq t0, 0(a2)        // dma/vif template for strow setup

  c->xor_(t2, t2, v0);                              // xor t2, t2, v0  // add qwc
  c->sq(t2, 32, a2);                                // sq t2, 32(a2)   // this is the 0xC08C first unpack 8 with top. always to 140. unsigned-four data.
  c->xor_(t2, t2, v0);                              // xor t2, t2, v0  // remove qwc
  c->sh(s1, 44, a2);                                // sh s1, 44(a2) // here's the 0xc08c store

  c->daddu(s1, s1, s0);                             // daddu s1, s1, s0 // s1 is VU data ptr (qw), inc by number of 4 byte words because we're unpacking 4x.
  c->sb(s0, 46, a2);                                // sb s0, 46(a2)    // store qw to unpack in viftag (output qw's)
  c->dsll32(s0, v0, 4);                             // dsll32 s0, v0, 4 // qw -> bytes for input unsigned-fours (add offset to addr field of dma tag, it's 4 + 32 bit shift)
  c->daddu(t3, t2, s0);                             // daddu t3, t2, s0 // t3 = next
  c->daddiu(s0, s2, 3);                             // daddiu s0, s2, 3 // s0 = lump-four-count + 3
  c->sw(a3, 12, a2);                                // sw a3, 12(a2)    // st-vif-add's x.
  c->srl(s0, s0, 2);                                // srl s0, s0, 2    // lump fours / 4
  c->sq(t1, 16, a2);                                // sq t1, 16(a2)    // row y (will be overwritten) z w (nop).
  // PC HACK: sneak in the bits here:
  memcpy(g_ee_main_mem + c->sgpr64(a2) + 28, &use_pc_merc_bits, 2);
  memcpy(g_ee_main_mem + c->sgpr64(a2) + 30, &ignore_alpha_bits, 2);


  // store the dma tag for the lump fours
  c->xor_(t3, t3, s0);                              // xor t3, t3, s0
  c->sq(t3, 48, a2);                                // sq t3, 48(a2)
  c->xor_(t3, t3, s0);                              // xor t3, t3, s0

  c->sh(s1, 60, a2);                                // sh s1, 60(a2)        // lump 4 destination.
  c->daddu(s1, s1, s2);                             // daddu s1, s1, s2     // inc VU dest ptr
  c->sb(s2, 62, a2);                                // sb s2, 62(a2)        // unpack qwc
  c->dsll32(s2, s0, 4);                             // dsll32 s2, s0, 4     // EE bytes
  c->sw(a3, 16, a2);                                // sw a3, 16(a2)        // row y overwrite with st-vif-add
  c->daddu(t4, t3, s2);                             // daddu t4, t3, s2     // next dma
  c->xor_(t4, t4, s3);                              // xor t4, t4, s3       // fp-qwc
  c->xori(a3, s1, 16384);                           // xori a3, s1, 16384
  c->sq(t4, 64, a2);                                // sq t4, 64(a2)        // dma for fp's
  c->xor_(t4, t4, s3);                              // xor t4, t4, s3
  c->sb(s3, 78, a2);                                // sb s3, 78(a2)        // unpack qwc
  c->dsll32(s3, s3, 4);                             // dsll32 s3, s3, 4     // bytes (in upper 32)
  c->sh(a3, 76, a2);                                // sh a3, 76(a2)        // destination in VU
  c->daddu(t2, t4, s3);                             // daddu t2, t4, s3     // next dma
  c->lbu(s3, 3, gp);                                // lbu s3, 3(gp)        // frag-ctrl.mat-xfer-count
  c->daddiu(gp, gp, 4);                             // daddiu gp, gp, 4     // gp = frag-ctrl.mat-dest-data

  // skip ahead if on not-first fragment.
  bc = c->sgpr64(s5) != 0;                          // bne s5, r0, L73
  c->daddiu(a2, a2, 80);                            // daddiu a2, a2, 80
  if (bc) {goto block_5;}                           // branch non-likely

  // on first, need to set up lights and stuff common to all fragments.
  // setup 8 qw upload (132 - 140)
  c->sd(t6, 0, a2);                                 // sd t6, 0(a2)
  c->addiu(s2, r0, 8);                              // addiu s2, r0, 8
  c->sd(t6, 8, a2);                                 // sd t6, 8(a2)
  c->lui(a3, 27656);                                // lui a3, 27656
  c->sb(s2, 0, a2);                                 // sb s2, 0(a2)
  c->daddiu(a3, a3, 132);                           // daddiu a3, a3, 132 // (inc global dma buf)

  c->load_symbol(s2, cache.merc_bucket_info);       // lw s2, *merc-bucket-info*(s7)
  c->daddu(s2, r0, s2);                             // daddu s2, r0, s2
  c->sw(a3, 12, a2);                                // sw a3, 12(a2)

  c->lq(a3, 0, s2);                                 // lq a3, 0(s2)   // load l0
  c->lq(s1, 16, s2);                                // lq s1, 16(s2)  // load l1
  c->lq(s0, 32, s2);                                // lq s0, 32(s2)  // load l2
  c->lq(v0, 48, s2);                                // lq v0, 48(s2)  // load l3
  c->sq(a3, 16, a2);                                // sq a3, 16(a2)  // store l0
  c->sq(s1, 32, a2);                                // sq s1, 32(a2)  // store l1
  c->sq(s0, 48, a2);                                // sq s0, 48(a2)  // store l2
  c->sq(v0, 64, a2);                                // sq v0, 64(a2)  // store l3
  c->lq(a3, 64, s2);                                // lq a3, 64(s2)  // load l4
  c->lq(s1, 80, s2);                                // lq s1, 80(s2)  // load l5
  c->lq(s0, 96, s2);                                // lq s0, 96(s2)  // load l6
  c->lui(v0, 16261);                                // lui v0, 16261  // 0x3F85
  c->lq(s2, 28, t7);                                // lq s2, 28(t7)  // first qw of merc-ctrl header.
  c->daddiu(v0, v0, 619);                           // daddiu v0, v0, 619 // 0x26B
  c->sq(a3, 80, a2);                                // sq a3, 80(a2) // store l4
  c->lbu(a3, 5, ra);                                // lbu a3, 5(ra) // effect-info.ignore-alpha
  c->sq(s1, 96, a2);                                // sq s1, 96(a2) // store l5
  c->sq(s0, 112, a2);                               // sq s0, 112(a2) // store l6
  c->dsubu(a3, v0, a3);                             // dsubu a3, v0, a3 //
  c->sq(s2, 128, a2);                               // sq s2, 128(a2)
  c->sw(a3, 28, a2);                                // sw a3, 28(a2)
  c->daddiu(a2, a2, 144);                           // daddiu a2, a2, 144

  // PC ADD BONUS DATA (bonus!)
  {
  // 10 qw test
  u64 dmatag = 5 | (1 << 28);
  memcpy(g_ee_main_mem + c->sgpr64(a2), &dmatag, 8);
  u32 vif = (0b1001 << 24);
  memcpy(g_ee_main_mem + c->sgpr64(a2) + 8, &vif, 4);

    for (int i = 0; i < 16; i++) {
      memcpy(g_ee_main_mem + c->sgpr64(a2) + 16 + i * 4, mbi->effects[i].color_fade, 4);
    }

  c->gprs[a2].du32[0] += 6 * 16;
  }


  // after first frag setup
  block_5:
  bc = c->sgpr64(s3) == 0;                          // beq s3, r0, L75
  c->addiu(s2, r0, 128);                            // addiu s2, r0, 128 // s2 = mat size
  if (bc) {goto block_8;}                           // branch non-likely

  c->lbu(a3, 0, gp);                                // lbu a3, 0(gp) // a3 = mat-number

  block_7:
  c->multu3(s1, a3, s2);                            // multu3 s1, a3, s2  // s1 = mat-number * 128
  c->sq(t5, 0, a2);                                 // sq t5, 0(a2)       // dma template
  c->lbu(s0, 1, gp);                                // lbu s0, 1(gp)      // s0 = mat-dest
  c->daddiu(gp, gp, 2);                             // daddiu gp, gp, 2   // inc mat-dest-data pr

  // HACK for PC PORT: stash the source matrix number in the unused bits of nop viftag.
  c->sb(a3, 8, a2);

  c->lbu(a3, 0, gp);                                // lbu a3, 0(gp)      // load for next iter (ugh)
  c->daddiu(s3, s3, -1);                            // daddiu s3, s3, -1  // dec count
  c->sb(s0, 12, a2);                                // sb s0, 12(a2)      // store matrix destination.
  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16  // increment dma output.
  c->daddu(s1, s1, a1);                             // daddu s1, s1, a1   // matrix data + 128 * mat-number
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s3) != 0;                          // bne s3, r0, L74    // see if we're done
  c->sw(s1, -12, a2);                               // sw s1, -12(a2)     // store pointer in input matrix data in dma tag
  if (bc) {goto block_7;}                           // branch non-likely


  block_8:
  c->sq(t6, 0, a2);                                 // sq t6, 0(a2)        // dma tag template
  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16   // inc
  bc = c->sgpr64(s5) != 0;                          // bne s5, r0, L76     // skip ahead on non-first fragment
  c->daddiu(s5, s5, 1);                             // daddiu s5, s5, 1    // inc fragment counter
  if (bc) {goto block_10;}                          // branch non-likely

  c->mov64(a3, v1);                                 // or a3, v1, r0
  c->sb(a3, -4, a2);                                // sb a3, -4(a2)

  block_10:
  bc = c->sgpr64(s5) != c->sgpr64(s4);              // bne s5, s4, L72
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_3;}                           // branch non-likely


  block_11:
  c->daddiu(t9, t9, 32);                            // daddiu t9, t9, 32
  c->daddiu(ra, ra, 8);                             // daddiu ra, ra, 8
  c->daddiu(t8, t8, -1);                            // daddiu t8, t8, -1
  bc = c->sgpr64(t8) != 0;                          // bne t8, r0, L71
  // c->lui(a3, 28672);                                // lui a3, 28672
  get_fake_spad_addr(a3, cache.fake_scratchpad_data, 0, c);
  if (bc) {goto block_1;}                           // branch non-likely

  c->lw(v1, 220, a3);                               // lw v1, 220(a3)
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L78
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_15;}                          // branch non-likely

  bc = c->sgpr64(a2) == c->sgpr64(a0);              // beq a2, a0, L78
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_15;}                          // branch non-likely

  c->sw(a0, 220, a3);                               // sw a0, 220(a3)

  block_15:
  c->mov64(v0, a2);                                 // or v0, a2, r0
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
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.merc_bucket_info = intern_from_c("*merc-bucket-info*").c();
  cache.merc_global_stats = intern_from_c("*merc-global-stats*").c();
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("draw-bones-merc", execute, 512);
}

} // namespace draw_bones_merc
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace draw_bones_check_longest_edge_asm {
struct Cache {
  void* math_camera; // *math-camera*
  void* fake_scratchpad_data;
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  bool cop1_bc = false;
  float acc;
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  // c->lui(at, 28672);                                // lui at, 28672
  get_fake_spad_addr(at, cache.fake_scratchpad_data, 0, c);
  c->mov64(v0, s7);                                 // or v0, s7, r0
  c->load_symbol(v1, cache.math_camera);            // lw v1, *math-camera*(s7)
  c->lwc1(f0, 64, a0);                              // lwc1 f0, 64(a0)
  c->lwc1(f7, 176, at);                             // lwc1 f7, 176(at)
  c->lwc1(f3, 180, at);                             // lwc1 f3, 180(at)
  c->lwc1(f6, 184, at);                             // lwc1 f6, 184(at)
  c->lwc1(f4, 136, a0);                             // lwc1 f4, 136(a0)
  c->lwc1(f12, 0, v1);                              // lwc1 f12, 0(v1)
  c->lwc1(f11, 64, v1);                             // lwc1 f11, 64(v1)
  c->mtc1(f1, a1);                                  // mtc1 f1, a1
  c->mtc1(f9, r0);                                  // mtc1 f9, r0
  c->mtc1(f10, r0);                                 // mtc1 f10, r0
  c->mtc1(f2, r0);                                  // mtc1 f2, r0
  c->mtc1(f2, r0);                                  // mtc1 f2, r0
  c->mtc1(f5, r0);                                  // mtc1 f5, r0
  c->mtc1(f8, r0);                                  // mtc1 f8, r0
  c->lw_float_constant(a0, 0x3f800000);             // lw a0, L168(fp) 1.0
  c->mtc1(f5, a0);                                  // mtc1 f5, a0
  cop1_bc = c->fprs[f1] < c->fprs[f11];             // c.lt.s f1, f11
  bc = cop1_bc;                                     // bc1t L69
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_37;}                          // branch non-likely

  c->lwc1(f11, 12, v1);                             // lwc1 f11, 12(v1)
  c->lwc1(f13, 16, v1);                             // lwc1 f13, 16(v1)
  c->muls(f11, f11, f12);                           // mul.s f11, f11, f12
  c->muls(f13, f13, f12);                           // mul.s f13, f13, f12
  c->subs(f14, f6, f4);                             // sub.s f14, f6, f4
  cop1_bc = c->fprs[f12] < c->fprs[f14];            // c.lt.s f12, f14
  bc = !cop1_bc;                                    // bc1f L53
  c->lwc1(f12, 60, v1);                             // lwc1 f12, 60(v1)
  if (bc) {goto block_3;}                           // branch non-likely

  c->muls(f12, f14, f12);                           // mul.s f12, f14, f12
  cop1_bc = c->fprs[f0] < c->fprs[f12];             // c.lt.s f0, f12
  bc = cop1_bc;                                     // bc1t L68
  // nop                                            // sll r0, r0, 0
  if (bc) {
//   fmt::print("pass 1 due to {} {}\n",  c->fprs[f0], c->fprs[f12]);
goto block_36;}                          // branch non-likely


  block_3:
  c->subs(f14, f3, f4);                             // sub.s f14, f3, f4
  cop1_bc = c->fprs[f13] < c->fprs[f14];            // c.lt.s f13, f14
  bc = !cop1_bc;                                    // bc1f L54
  c->lwc1(f12, 56, v1);                             // lwc1 f12, 56(v1)
  if (bc) {goto block_5;}                           // branch non-likely

  //beq r0, r0, L55                                 // beq r0, r0, L55
  c->muls(f10, f14, f12);                           // mul.s f10, f14, f12
  goto block_7;                                     // branch always


  block_5:
  c->adds(f14, f3, f4);                             // add.s f14, f3, f4
  c->negs(f13, f13);                                // neg.s f13, f13
  cop1_bc = c->fprs[f14] < c->fprs[f13];            // c.lt.s f14, f13
  bc = !cop1_bc;                                    // bc1f L55
  c->negs(f13, f14);                                // neg.s f13, f14
  if (bc) {goto block_7;}                           // branch non-likely

  c->muls(f10, f13, f12);                           // mul.s f10, f13, f12

  block_7:
  cop1_bc = c->fprs[f0] < c->fprs[f10];             // c.lt.s f0, f10
  bc = cop1_bc;                                     // bc1t L68
  // nop                                            // sll r0, r0, 0
  if (bc) {
//    fmt::print("pass 2 due to {} {}\n",  c->fprs[f0], c->fprs[f10]);
goto block_36;}                          // branch non-likely

  c->subs(f12, f7, f4);                             // sub.s f12, f7, f4
  cop1_bc = c->fprs[f11] < c->fprs[f12];            // c.lt.s f11, f12
  bc = !cop1_bc;                                    // bc1f L56
  c->lwc1(f10, 52, v1);                             // lwc1 f10, 52(v1)
  if (bc) {goto block_10;}                          // branch non-likely

  //beq r0, r0, L57                                 // beq r0, r0, L57
  c->muls(f9, f12, f10);                            // mul.s f9, f12, f10
  goto block_12;                                    // branch always


  block_10:
  c->adds(f12, f7, f4);                             // add.s f12, f7, f4
  c->negs(f11, f11);                                // neg.s f11, f11
  cop1_bc = c->fprs[f12] < c->fprs[f11];            // c.lt.s f12, f11
  bc = !cop1_bc;                                    // bc1f L57
  c->negs(f11, f12);                                // neg.s f11, f12
  if (bc) {goto block_12;}                          // branch non-likely

  c->muls(f9, f11, f10);                            // mul.s f9, f11, f10

  block_12:
  cop1_bc = c->fprs[f0] < c->fprs[f9];              // c.lt.s f0, f9
  bc = cop1_bc;                                     // bc1t L68
  // nop                                            // sll r0, r0, 0
  if (bc) {
//    fmt::print("pass 3 due to {} {}\n",  c->fprs[f0], c->fprs[f9]);
goto block_36;}                          // branch non-likely


  c->abss(f14, f7);// Unknown instr: abs.s f14, f7
  c->movs(f12, f6);                                 // mov.s f12, f6
  acc = c->fprs[f14] * c->fprs[f14]; // Unknown instr: mula.s f14, f14
  c->fprs[f15] = acc + c->fprs[f12] * c->fprs[f12]; // Unknown instr: madd.s f15, f12, f12
  c->lwc1(f9, 76, v1);                              // lwc1 f9, 76(v1)
  c->lwc1(f13, 80, v1);                             // lwc1 f13, 80(v1)
  c->lwc1(f10, 84, v1);                             // lwc1 f10, 84(v1)
  c->lwc1(f11, 88, v1);                             // lwc1 f11, 88(v1)
  c->fprs[f16] = c->fprs[f5] / (std::sqrt(std::abs(c->fprs[f15]))); // Unknown instr: rsqrt.s f16, f5, f15
  c->muls(f15, f14, f16);                           // mul.s f15, f14, f16
  c->muls(f16, f12, f16);                           // mul.s f16, f12, f16
  acc = c->fprs[f9] * c->fprs[f16]; // Unknown instr: mula.s f9, f16
  c->fprs[f12] = acc - c->fprs[f13] * c->fprs[f15]; // Unknown instr: msub.s f12, f13, f15
  acc = c->fprs[f10] * c->fprs[f16]; // Unknown instr: mula.s f10, f16
  c->fprs[f14] = acc - c->fprs[f11] * c->fprs[f15];// Unknown instr: msub.s f14, f11, f15
  acc = c->fprs[f9] * c->fprs[f15];// Unknown instr: mula.s f9, f15
  c->fprs[f9] = acc + c->fprs[f13] * c->fprs[f16];// Unknown instr: madd.s f9, f13, f16
  acc = c->fprs[f10] * c->fprs[f15];// Unknown instr: mula.s f10, f15
  c->fprs[f10] = acc + c->fprs[f11] * c->fprs[f16];// Unknown instr: madd.s f10, f11, f16
  cop1_bc = c->fprs[f8] < c->fprs[f12];             // c.lt.s f8, f12
  bc = cop1_bc;                                     // bc1t L58
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_17;}                          // branch non-likely

  cop1_bc = c->fprs[f8] < c->fprs[f14];             // c.lt.s f8, f14
  bc = cop1_bc;                                     // bc1t L59
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_18;}                          // branch non-likely

  cop1_bc = c->fprs[f8] < c->fprs[f9];              // c.lt.s f8, f9
  bc = cop1_bc;                                     // bc1t L60
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_19;}                          // branch non-likely

  //beq r0, r0, L62                                 // beq r0, r0, L62
  // nop                                            // sll r0, r0, 0
  goto block_23;                                    // branch always


  block_17:
  //beq r0, r0, L62                                 // beq r0, r0, L62
  c->divs(f2, f1, f10);                             // div.s f2, f1, f10
  goto block_23;                                    // branch always


  block_18:
  c->negs(f2, f12);                                 // neg.s f2, f12
  c->divs(f2, f2, f9);                              // div.s f2, f2, f9
  c->divs(f7, f14, f10);                            // div.s f7, f14, f10
  c->adds(f2, f7, f2);                              // add.s f2, f7, f2
  //beq r0, r0, L62                                 // beq r0, r0, L62
  c->muls(f2, f2, f1);                              // mul.s f2, f2, f1
  goto block_23;                                    // branch always


  block_19:
  c->subs(f8, f7, f4);                              // sub.s f8, f7, f4
  c->adds(f10, f7, f4);                             // add.s f10, f7, f4
  c->negs(f11, f7);                                 // neg.s f11, f7
  cop1_bc = c->fprs[f7] < c->fprs[f8];              // c.lt.s f7, f8
  bc = cop1_bc;                                     // bc1t L61
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_22;}                          // branch non-likely

  cop1_bc = c->fprs[f10] < c->fprs[f11];            // c.lt.s f10, f11
  bc = cop1_bc;                                     // bc1t L61
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_22;}                          // branch non-likely

  //beq r0, r0, L62                                 // beq r0, r0, L62
  // nop                                            // sll r0, r0, 0
  goto block_23;                                    // branch always


  block_22:
  c->negs(f2, f12);                                 // neg.s f2, f12
  c->muls(f2, f1, f2);                              // mul.s f2, f1, f2
  c->divs(f2, f2, f9);                              // div.s f2, f2, f9

  block_23:
  cop1_bc = c->fprs[f0] < c->fprs[f2];              // c.lt.s f0, f2
  bc = cop1_bc;                                     // bc1t L68
  // nop                                            // sll r0, r0, 0
  if (bc) {
//    fmt::print("pass 4 due to {} {}\n",  c->fprs[f0], c->fprs[f2]);
goto block_36;}                          // branch non-likely

  c->abss(f10, f3);// Unknown instr: abs.s f10, f3
  c->movs(f12, f6);                                 // mov.s f12, f6
  acc = c->fprs[f10] * c->fprs[f10];// Unknown instr: mula.s f10, f10
  c->fprs[f9] = acc + c->fprs[f12] * c->fprs[f12];// Unknown instr: madd.s f9, f12, f12
  c->lwc1(f7, 96, v1);                              // lwc1 f7, 96(v1)
  c->lwc1(f8, 100, v1);                             // lwc1 f8, 100(v1)
  c->lwc1(f6, 104, v1);                             // lwc1 f6, 104(v1)
  c->fprs[f5] = c->fprs[f5] / std::sqrt(std::abs(c->fprs[f9])); // Unknown instr: rsqrt.s f5, f5, f9
  c->lwc1(f11, 108, v1);                            // lwc1 f11, 108(v1)
  c->mtc1(f9, r0);                                  // mtc1 f9, r0
  c->muls(f13, f10, f5);                            // mul.s f13, f10, f5
  c->muls(f14, f12, f5);                            // mul.s f14, f12, f5
  acc = c->fprs[f7] * c->fprs[f14];// Unknown instr: mula.s f7, f14
  c->fprs[f10] = acc - c->fprs[f8] * c->fprs[f13];// Unknown instr: msub.s f10, f8, f13
  acc = c->fprs[f6] * c->fprs[f14];// Unknown instr: mula.s f6, f14
  c->fprs[f12] = acc - c->fprs[f11] * c->fprs[f13];// Unknown instr: msub.s f12, f11, f13
  acc = c->fprs[f7] * c->fprs[f13];// Unknown instr: mula.s f7, f13
  c->fprs[f5] = acc + c->fprs[f8] * c->fprs[f14];// Unknown instr: madd.s f5, f8, f14
  acc = c->fprs[f6] * c->fprs[f13];// Unknown instr: mula.s f6, f13
  c->fprs[f6] = acc + c->fprs[f11] * c->fprs[f14];// Unknown instr: madd.s f6, f11, f14
  cop1_bc = c->fprs[f9] < c->fprs[f10];             // c.lt.s f9, f10
  bc = cop1_bc;                                     // bc1t L63
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_28;}                          // branch non-likely

  cop1_bc = c->fprs[f9] < c->fprs[f12];             // c.lt.s f9, f12
  bc = cop1_bc;                                     // bc1t L64
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_29;}                          // branch non-likely

  cop1_bc = c->fprs[f9] < c->fprs[f5];              // c.lt.s f9, f5
  bc = cop1_bc;                                     // bc1t L65
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_30;}                          // branch non-likely

  //beq r0, r0, L67                                 // beq r0, r0, L67
  // nop                                            // sll r0, r0, 0
  goto block_34;                                    // branch always


  block_28:
  //beq r0, r0, L67                                 // beq r0, r0, L67
  c->divs(f1, f1, f6);                              // div.s f1, f1, f6
  goto block_34;                                    // branch always


  block_29:
  c->negs(f3, f10);                                 // neg.s f3, f10
  c->divs(f3, f3, f5);                              // div.s f3, f3, f5
  c->divs(f4, f12, f6);                             // div.s f4, f12, f6
  c->adds(f3, f4, f3);                              // add.s f3, f4, f3
  //beq r0, r0, L67                                 // beq r0, r0, L67
  c->muls(f1, f3, f1);                              // mul.s f1, f3, f1
  goto block_34;                                    // branch always


  block_30:
  c->subs(f6, f3, f4);                              // sub.s f6, f3, f4
  c->adds(f4, f3, f4);                              // add.s f4, f3, f4
  c->negs(f7, f3);                                  // neg.s f7, f3
  cop1_bc = c->fprs[f3] < c->fprs[f6];              // c.lt.s f3, f6
  bc = cop1_bc;                                     // bc1t L66
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_33;}                          // branch non-likely

  cop1_bc = c->fprs[f4] < c->fprs[f7];              // c.lt.s f4, f7
  bc = cop1_bc;                                     // bc1t L66
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_33;}                          // branch non-likely

  //beq r0, r0, L67                                 // beq r0, r0, L67
  // nop                                            // sll r0, r0, 0
  goto block_34;                                    // branch always


  block_33:
  c->negs(f3, f10);                                 // neg.s f3, f10
  c->muls(f1, f1, f3);                              // mul.s f1, f1, f3
  c->divs(f1, f1, f5);                              // div.s f1, f1, f5

  block_34:
//fmt::print("check: {} {}\n", c->fprs[f0], c->fprs[f2]);
  cop1_bc = c->fprs[f0] < c->fprs[f2];              // c.lt.s f0, f2
  bc = cop1_bc;                                     // bc1t L68
  // nop                                            // sll r0, r0, 0
  if (bc) {
//    fmt::print("pass 5 due to {} {}\n",  c->fprs[f0], c->fprs[f2]);
goto block_36;}                          // branch non-likely

  //beq r0, r0, L69                                 // beq r0, r0, L69
  // nop                                            // sll r0, r0, 0
  goto block_37;                                    // branch always


  block_36:
//  fmt::print("pass!\n");
  c->daddiu(v0, s7, 8);                             // daddiu v0, s7, #t

  block_37:
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.math_camera = intern_from_c("*math-camera*").c();
  gLinkedFunctionTable.reg("draw-bones-check-longest-edge-asm", execute, 128);
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
}

} // namespace draw_bones_check_longest_edge_asm
} // namespace Mips2C

