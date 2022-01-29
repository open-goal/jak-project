
//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/kscheme.h"
namespace Mips2C {
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

