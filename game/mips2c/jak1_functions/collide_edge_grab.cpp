//--------------------------MIPS2C---------------------
#include "game/kernel/jak1/kscheme.h"
#include "game/mips2c/mips2c_private.h"
using namespace jak1;
namespace Mips2C::jak1 {
namespace method_16_collide_edge_work {
struct Cache {
  void* format;  // format
} cache;

// clang-format off
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  [[maybe_unused]] u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -32);                           // daddiu sp, sp, -32
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  c->sq(gp, 16, sp);                                // sq gp, 16(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->fprs[f0] = 0.707;                              // lwc1 f0, L95(fp)
  c->addiu(v1, r0, 5);                              // addiu v1, r0, 5
  // nop                                            // sll r0, r0, 0
  c->addiu(a0, r0, 56);                             // addiu a0, r0, 56
  c->lwu(t3, 0, gp);                                // lwu t3, 0(gp)
  c->addiu(a1, r0, 0);                              // addiu a1, r0, 0
  c->daddiu(a2, gp, 6272);                          // daddiu a2, gp, 6272
  c->addiu(a3, r0, 16);                             // addiu a3, r0, 16
  c->lwu(t0, 0, t3);                                // lwu t0, 0(t3)
  c->gprs[t1].du64[0] = 0;                          // or t1, r0, r0
  c->lq(t2, 96, gp);                                // lq t2, 96(gp)
  c->daddiu(t3, t3, 4908);                          // daddiu t3, t3, 4908
  c->lq(t4, 112, gp);                               // lq t4, 112(gp)

  block_1:
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L30
  c->lwu(t5, 48, t3);                               // lwu t5, 48(t3)
  if (bc) {goto block_14;}                          // branch non-likely

  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  c->lqc2(vf1, 0, t3);                              // lqc2 vf1, 0(t3)
  c->and_(t6, t5, a0);                              // and t6, t5, a0
  c->lqc2(vf2, 16, t3);                             // lqc2 vf2, 16(t3)
  bc = c->sgpr64(t6) == c->sgpr64(a1);              // beq t6, a1, L29
  c->lqc2(vf3, 32, t3);                             // lqc2 vf3, 32(t3)
  if (bc) {goto block_5;}                           // branch non-likely

  if (((s64)c->sgpr64(t6)) != ((s64)c->sgpr64(a3))) {// bnel t6, a3, L28
    c->daddiu(t3, t3, 64);                          // daddiu t3, t3, 64
    goto block_1;
  }

  block_5:
  c->and_(t6, t5, v1);                              // and t6, t5, v1
  c->vmini(DEST::xyzw, vf7, vf1, vf2);              // vmini.xyzw vf7, vf1, vf2
  bc = c->sgpr64(t6) != 0;                          // bne t6, r0, L28
  c->vmax(DEST::xyzw, vf8, vf1, vf2);               // vmax.xyzw vf8, vf1, vf2
  if (bc) {goto block_1;}                           // branch non-likely

  c->vsub(DEST::xyz, vf4, vf2, vf1);                // vsub.xyz vf4, vf2, vf1
  // nop                                            // sll r0, r0, 0
  c->vsub(DEST::xyz, vf5, vf3, vf1);                // vsub.xyz vf5, vf3, vf1
  // nop                                            // sll r0, r0, 0
  c->vmini(DEST::xyzw, vf7, vf7, vf3);              // vmini.xyzw vf7, vf7, vf3
  // nop                                            // sll r0, r0, 0
  c->vmax(DEST::xyzw, vf8, vf8, vf3);               // vmax.xyzw vf8, vf8, vf3
  // nop                                            // sll r0, r0, 0
  c->vopmula(vf4, vf5);                             // vopmula.xyz acc, vf4, vf5
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf7, vf7);                  // vftoi0.xyzw vf7, vf7
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf8, vf8);                  // vftoi0.xyzw vf8, vf8
  // nop                                            // sll r0, r0, 0
  c->vopmsub(vf6, vf5, vf4);                        // vopmsub.xyz vf6, vf5, vf4
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t7, vf7);                        // qmfc2.i t7, vf7
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t6, vf8);                        // qmfc2.i t6, vf8
  // nop                                            // sll r0, r0, 0
  c->pcgtw(t7, t7, t4);                             // pcgtw t7, t7, t4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->vmul(DEST::xyzw, vf9, vf6, vf6);               // vmul.xyzw vf9, vf6, vf6
  // nop                                            // sll r0, r0, 0
  c->pcgtw(t6, t2, t6);                             // pcgtw t6, t2, t6
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->por(t6, t7, t6);                               // por t6, t7, t6
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppach(t6, r0, t6);                             // ppach t6, r0, t6
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsll(t6, t6, 16);                              // dsll t6, t6, 16
  // nop                                            // sll r0, r0, 0
  if (((s64)c->sgpr64(t6)) != ((s64)0)) {           // bnel t6, r0, L28
    c->daddiu(t3, t3, 64);                          // daddiu t3, t3, 64
    goto block_1;
  }

  // block_8:
  c->vmula_bc(DEST::w, BC::x, vf0, vf9);            // vmulax.w acc, vf0, vf9
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::w, BC::y, vf0, vf9);           // vmadday.w acc, vf0, vf9
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::w, BC::z, vf9, vf0, vf9);       // vmaddz.w vf9, vf0, vf9
  // nop                                            // sll r0, r0, 0
  c->vrsqrt(vf0, BC::w, vf9, BC::w);                // vrsqrt Q, vf0.w, vf9.w
  if (c->vfs[vf9].f[3] == 0.f) {
    // hack to prevent NaNs from getting in the collide data if we have a zero-area triangle
    // this value doesn't matter - the normal is zeros anyway and the triangle will be rejected
    // by every other step.
    c->Q = 0.f;
  }
  // nop                                            // sll r0, r0, 0
  c->dsll32(t5, t5, 12);                            // dsll32 t5, t5, 12
  c->dsrl32(t5, t5, 26);                            // dsrl32 t5, t5, 26
  c->addiu(t6, r0, 2);                              // addiu t6, r0, 2
  // nop                                            // sll r0, r0, 0
  c->vwaitq();                                      // vwaitq
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf6, vf6);                    // vmulq.xyz vf6, vf6, Q
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t7, vf6);                        // qmfc2.i t7, vf6
  // nop                                            // sll r0, r0, 0
  c->dsra32(t7, t7, 0);                             // dsra32 t7, t7, 0
  // nop                                            // sll r0, r0, 0
  c->mtc1(f1, t7);                                  // mtc1 f1, t7
  // nop                                            // sll r0, r0, 0
  cop1_bc = c->fprs[f1] < c->fprs[f0];              // c.lt.s f1, f0
  c->addiu(t7, r0, 48);                             // addiu t7, r0, 48
  if (cop1_bc) {                                    // bc1tl L28
    c->daddiu(t3, t3, 64);                          // daddiu t3, t3, 64
    goto block_1;
  }

  // block_10:
  if (((s64)c->sgpr64(t5)) == ((s64)c->sgpr64(t6))) {// beql t5, t6, L28
    c->daddiu(t3, t3, 64);                          // daddiu t3, t3, 64
    goto block_1;
  }

  // block_12:
  bc = c->sgpr64(t1) == c->sgpr64(t7);              // beq t1, t7, L31
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_15;}                          // branch non-likely

  c->daddiu(t1, t1, 1);                             // daddiu t1, t1, 1
  c->sw(t3, 0, a2);                                 // sw t3, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf6, 16, a2);                             // sqc2 vf6, 16(a2)
  c->daddiu(a2, a2, 32);                            // daddiu a2, a2, 32
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L28                                 // beq r0, r0, L28
  c->daddiu(t3, t3, 64);                            // daddiu t3, t3, 64
  goto block_1;                                     // branch always


  block_14:
  //beq r0, r0, L32                                 // beq r0, r0, L32
  c->sw(t1, 16, gp);                                // sw t1, 16(gp)
  goto block_16;                                    // branch always


  block_15:
  c->load_symbol(t9, cache.format);                 // lw t9, format(s7)
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  //daddiu a1, fp, L88                                // daddiu a1, fp, L88
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  //c->jalr(call_addr);                               // jalr ra, t9
  printf("ERROR: Exceeded max # of grabbable tris\n");
  c->addiu(v1, r0, 48);                             // addiu v1, r0, 48
  c->sw(v1, 16, gp);                                // sw v1, 16(gp)

  block_16:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
  c->lq(gp, 16, sp);                                // lq gp, 16(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 32);                            // daddiu sp, sp, 32
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.format = intern_from_c("format").c();
  gLinkedFunctionTable.reg("(method 16 collide-edge-work)", execute, 128);
}

} // namespace method_16_collide_edge_work
} // namespace Mips2C

namespace Mips2C::jak1 {
namespace method_15_collide_edge_work {
struct Cache {
  void* format; // format
} cache;

void sub_l20_b26(ExecutionContext* c) {
  bool bc;
  bool cop1_bc;
//  block_26:
  c->gprs[t2].du64[0] = 0;                          // or t2, r0, r0
  c->lwu(t1, 8, a0);                                // lwu t1, 8(a0)
  c->daddiu(t0, a0, 640);                           // daddiu t0, a0, 640
  // nop                                            // sll r0, r0, 0

  block_27:
  bc = c->sgpr64(t2) == c->sgpr64(t1);              // beq t2, t1, L22
  c->lqc2(vf9, 0, t0);                              // lqc2 vf9, 0(t0)
  if (bc) {goto block_36;}                          // branch non-likely

  c->daddiu(t2, t2, 1);                             // daddiu t2, t2, 1
  c->lqc2(vf10, 16, t0);                            // lqc2 vf10, 16(t0)
  c->vsub(DEST::xyzw, vf9, vf9, vf8);               // vsub.xyzw vf9, vf9, vf8
  c->lqc2(vf11, 32, t0);                            // lqc2 vf11, 32(t0)
  c->vsub(DEST::xyzw, vf10, vf10, vf8);             // vsub.xyzw vf10, vf10, vf8
  c->lqc2(vf12, 48, t0);                            // lqc2 vf12, 48(t0)
  c->vsub(DEST::xyzw, vf11, vf11, vf8);             // vsub.xyzw vf11, vf11, vf8
  // nop                                            // sll r0, r0, 0
  c->vsub(DEST::xyzw, vf12, vf12, vf8);             // vsub.xyzw vf12, vf12, vf8
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf9, vf9, vf9);               // vmul.xyzw vf9, vf9, vf9
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf10, vf10, vf10);            // vmul.xyzw vf10, vf10, vf10
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf11, vf11, vf11);            // vmul.xyzw vf11, vf11, vf11
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf12, vf12, vf12);            // vmul.xyzw vf12, vf12, vf12
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::y, vf9, vf9, vf9);        // vaddy.x vf9, vf9, vf9
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::y, vf10, vf10, vf10);     // vaddy.x vf10, vf10, vf10
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::y, vf11, vf11, vf11);     // vaddy.x vf11, vf11, vf11
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::y, vf12, vf12, vf12);     // vaddy.x vf12, vf12, vf12
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::z, vf9, vf9, vf9);        // vaddz.x vf9, vf9, vf9
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::z, vf10, vf10, vf10);     // vaddz.x vf10, vf10, vf10
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::z, vf11, vf11, vf11);     // vaddz.x vf11, vf11, vf11
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::z, vf12, vf12, vf12);     // vaddz.x vf12, vf12, vf12
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t3, vf9);                        // qmfc2.i t3, vf9
  // nop                                            // sll r0, r0, 0
  c->mtc1(f1, t3);                                  // mtc1 f1, t3
  // nop                                            // sll r0, r0, 0
  cop1_bc = c->fprs[f1] <= c->fprs[f0];             // c.le.s f1, f0
  // nop                                            // sll r0, r0, 0
  bc = cop1_bc;                                     // bc1t L23
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_39;}                          // branch non-likely

  bc = c->sgpr64(t2) == c->sgpr64(t1);              // beq t2, t1, L22
  c->daddiu(t0, t0, 16);                            // daddiu t0, t0, 16
  if (bc) {goto block_36;}                          // branch non-likely

  c->mov128_gpr_vf(t3, vf10);                       // qmfc2.i t3, vf10
  c->daddiu(t2, t2, 1);                             // daddiu t2, t2, 1
  c->mtc1(f1, t3);                                  // mtc1 f1, t3
  // nop                                            // sll r0, r0, 0
  cop1_bc = c->fprs[f1] <= c->fprs[f0];             // c.le.s f1, f0
  // nop                                            // sll r0, r0, 0
  bc = cop1_bc;                                     // bc1t L23
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_39;}                          // branch non-likely

  bc = c->sgpr64(t2) == c->sgpr64(t1);              // beq t2, t1, L22
  c->daddiu(t0, t0, 16);                            // daddiu t0, t0, 16
  if (bc) {goto block_36;}                          // branch non-likely

  c->mov128_gpr_vf(t3, vf11);                       // qmfc2.i t3, vf11
  c->daddiu(t2, t2, 1);                             // daddiu t2, t2, 1
  c->mtc1(f1, t3);                                  // mtc1 f1, t3
  // nop                                            // sll r0, r0, 0
  cop1_bc = c->fprs[f1] <= c->fprs[f0];             // c.le.s f1, f0
  // nop                                            // sll r0, r0, 0
  bc = cop1_bc;                                     // bc1t L23
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_39;}                          // branch non-likely

  bc = c->sgpr64(t2) == c->sgpr64(t1);              // beq t2, t1, L22
  c->daddiu(t0, t0, 16);                            // daddiu t0, t0, 16
  if (bc) {goto block_36;}                          // branch non-likely

  c->mov128_gpr_vf(t3, vf12);                       // qmfc2.i t3, vf12
  c->daddiu(t2, t2, 1);                             // daddiu t2, t2, 1
  c->mtc1(f1, t3);                                  // mtc1 f1, t3
  // nop                                            // sll r0, r0, 0
  cop1_bc = c->fprs[f1] <= c->fprs[f0];             // c.le.s f1, f0
  // nop                                            // sll r0, r0, 0
  bc = cop1_bc;                                     // bc1t L23
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_39;}                          // branch non-likely

  //beq r0, r0, L21                                 // beq r0, r0, L21
  c->daddiu(t0, t0, 16);                            // daddiu t0, t0, 16
  goto block_27;                                    // branch always


  block_36:
  c->addiu(t2, r0, 64);                             // addiu t2, r0, 64
  // nop                                            // sll r0, r0, 0
  if (((s64)c->sgpr64(t2)) == ((s64)c->sgpr64(t1))) {// beql t2, t1, L23
    c->mov64(t0, s7);                               // or t0, s7, r0
    goto block_39;
  }

//  block_38:
  c->daddiu(t1, t1, 1);                             // daddiu t1, t1, 1
  c->sqc2(vf8, 0, t0);                              // sqc2 vf8, 0(t0)
  // nop                                            // sll r0, r0, 0
  c->sw(t1, 8, a0);                                 // sw t1, 8(a0)

  block_39:
;
  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0

}

void sub_l16_b15(ExecutionContext* c) {
  bool bc;
  bool cop1_bc;
//  block_15:
  c->mov64(t4, t1);                                 // or t4, t1, r0
  c->lwu(t3, 12, a0);                               // lwu t3, 12(a0)
  c->dsll32(t4, t4, 0);                             // dsll32 t4, t4, 0
  c->gprs[t5].du64[0] = 0;                          // or t5, r0, r0
  c->or_(t6, t4, t2);                               // or t6, t4, t2
  c->daddiu(t4, a0, 1664);                          // daddiu t4, a0, 1664

  block_16:
  bc = c->sgpr64(t5) == c->sgpr64(t3);              // beq t5, t3, L18
  c->ld(t7, 8, t4);                                 // ld t7, 8(t4)
  if (bc) {goto block_20;}                          // branch non-likely

  c->daddiu(t5, t5, 1);                             // daddiu t5, t5, 1
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  if (((s64)c->sgpr64(t6)) != ((s64)c->sgpr64(t7))) {// bnel t6, t7, L17
    c->daddiu(t4, t4, 48);                          // daddiu t4, t4, 48
    goto block_16;
  }

//  block_19:
  //beq r0, r0, L19                                 // beq r0, r0, L19
  c->sw(r0, 0, t4);                                 // sw r0, 0(t4)
  goto block_25;                                    // branch always


  block_20:
  c->addiu(t5, r0, 96);                             // addiu t5, r0, 96
  // nop                                            // sll r0, r0, 0
  if (((s64)c->sgpr64(t3)) == ((s64)c->sgpr64(t5))) {// beql t3, t5, L19
    c->mov64(t4, s7);                               // or t4, s7, r0
    goto block_25;
  }

//  block_22:
  c->daddiu(t3, t3, 1);                             // daddiu t3, t3, 1
  c->sw(s7, 0, t4);                                 // sw s7, 0(t4)
  // nop                                            // sll r0, r0, 0
  c->sw(t1, 8, t4);                                 // sw t1, 8(t4)
  // nop                                            // sll r0, r0, 0
  c->sw(t2, 12, t4);                                // sw t2, 12(t4)
  // nop                                            // sll r0, r0, 0
  c->sw(a1, 4, t4);                                 // sw a1, 4(t4)
  // nop                                            // sll r0, r0, 0
  c->sw(t3, 12, a0);                                // sw t3, 12(a0)
  c->vmove(DEST::xyzw, vf13, vf0);                  // vmove.xyzw vf13, vf0
  c->lqc2(vf16, 0, t1);                             // lqc2 vf16, 0(t1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf17, 0, t2);                             // lqc2 vf17, 0(t2)
  c->vsub(DEST::xyzw, vf18, vf17, vf16);            // vsub.xyzw vf18, vf17, vf16
  // nop                                            // sll r0, r0, 0
  c->vsub(DEST::xyzw, vf19, vf1, vf16);             // vsub.xyzw vf19, vf1, vf16
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf20, vf18, vf18);            // vmul.xyzw vf20, vf18, vf18
  // nop                                            // sll r0, r0, 0
  c->vsub_bc(DEST::x, BC::z, vf13, vf0, vf18);      // vsubz.x vf13, vf0, vf18
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::y, vf20, vf20, vf20);     // vaddy.x vf20, vf20, vf20
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::z, BC::x, vf13, vf0, vf18);      // vaddx.z vf13, vf0, vf18
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::z, vf20, vf20, vf20);     // vaddz.x vf20, vf20, vf20
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf15, vf13, vf19);            // vmul.xyzw vf15, vf13, vf19
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf14, vf13, vf13);            // vmul.xyzw vf14, vf13, vf13
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::z, vf15, vf15, vf15);     // vaddz.x vf15, vf15, vf15
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::z, vf14, vf14, vf14);     // vaddz.x vf14, vf14, vf14
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t2, vf15);                       // qmfc2.i t2, vf15
  // nop                                            // sll r0, r0, 0
  c->vrsqrt(vf0, BC::w, vf14, BC::x);               // vrsqrt Q, vf0.w, vf14.x
  // nop                                            // sll r0, r0, 0
  c->mtc1(f3, t2);                                  // mtc1 f3, t2
  // nop                                            // sll r0, r0, 0
  cop1_bc = c->fprs[f3] < c->fprs[f2];              // c.lt.s f3, f2
  // nop                                            // sll r0, r0, 0
  if (cop1_bc) {                                    // bc1tl L19
    c->sw(r0, 0, t4);                               // sw r0, 0(t4)
    goto block_25;
  }

//  block_24:
  c->vwaitq();                                      // vwaitq
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf13, vf13);                  // vmulq.xyz vf13, vf13, Q
  // nop                                            // sll r0, r0, 0
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  c->vrsqrt(vf0, BC::w, vf20, BC::x);               // vrsqrt Q, vf0.w, vf20.x
  // nop                                            // sll r0, r0, 0
  c->vmul_bc(DEST::w, BC::w, vf18, vf0, vf0);       // vmulw.w vf18, vf0, vf0
  c->sqc2(vf13, 16, t4);                            // sqc2 vf13, 16(t4)
  c->vwaitq();                                      // vwaitq
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf18, vf18);                  // vmulq.xyz vf18, vf18, Q
  // nop                                            // sll r0, r0, 0
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf18, 32, t4);                            // sqc2 vf18, 32(t4)

  block_25:
;
  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0

}

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  [[maybe_unused]] u32 call_addr = 0;
//  bool cop1_bc = false;
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  c->fprs[f0] = 1677.7216;                          // lwc1 f0, L90(fp)
  c->mtc1(f2, r0);                                  // mtc1 f2, r0
  c->lwu(a1, 4, a0);                                // lwu a1, 4(a0)
  // nop                                            // sll r0, r0, 0
  c->lwu(v1, 16, a0);                               // lwu v1, 16(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf1, 12, a1);                             // lqc2 vf1, 12(a1)
  c->daddiu(a1, a0, 6272);                          // daddiu a1, a0, 6272
  c->lqc2(vf6, 64, a0);                             // lqc2 vf6, 64(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf7, 80, a0);                             // lqc2 vf7, 80(a0)
  c->vmove(DEST::xyzw, vf2, vf1);                   // vmove.xyzw vf2, vf1
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::y, BC::y, vf1, vf0, vf6);        // vaddy.y vf1, vf0, vf6
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::y, BC::y, vf2, vf0, vf7);        // vaddy.y vf2, vf0, vf7
  // nop                                            // sll r0, r0, 0

  block_1:
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L26
  c->lwu(t0, 0, a1);                                // lwu t0, 0(a1)
  if (bc) {goto block_42;}                          // branch non-likely

  c->daddiu(v1, v1, -1);                            // daddiu v1, v1, -1
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 0, t0);                              // lqc2 vf3, 0(t0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf4, 16, t0);                             // lqc2 vf4, 16(t0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf5, 32, t0);                             // lqc2 vf5, 32(t0)
  // Unknown instr: bgezal r0, L20
  c->vmove(DEST::xyzw, vf8, vf3);                   // vmove.xyzw vf8, vf3
  //if (bc) {goto block_26;}                          // branch non-likely
  sub_l20_b26(c);

  bc = c->sgpr64(t0) == c->sgpr64(s7);              // beq t0, s7, L24
  c->mov64(a2, t0);                                 // or a2, t0, r0
  if (bc) {goto block_40;}                          // branch non-likely

  // Unknown instr: bgezal r0, L20
  c->vmove(DEST::xyzw, vf8, vf4);                   // vmove.xyzw vf8, vf4
  //if (bc) {goto block_26;}                          // branch non-likely
  sub_l20_b26(c);

  bc = c->sgpr64(t0) == c->sgpr64(s7);              // beq t0, s7, L24
  c->mov64(a3, t0);                                 // or a3, t0, r0
  if (bc) {goto block_40;}                          // branch non-likely

  // Unknown instr: bgezal r0, L20
  c->vmove(DEST::xyzw, vf8, vf5);                   // vmove.xyzw vf8, vf5
  //if (bc) {goto block_26;}                          // branch non-likely
  sub_l20_b26(c);

  bc = c->sgpr64(t0) == c->sgpr64(s7);              // beq t0, s7, L24
  c->mov64(t0, t0);                                 // or t0, t0, r0
  if (bc) {goto block_40;}                          // branch non-likely

  c->mov64(t1, a2);                                 // or t1, a2, r0
  // nop                                            // sll r0, r0, 0
  // Unknown instr: bgezal r0, L16
  c->mov64(t2, a3);                                 // or t2, a3, r0
  //if (bc) {goto block_15;}                          // branch non-likely
  sub_l16_b15(c);

  bc = c->sgpr64(t4) == c->sgpr64(s7);              // beq t4, s7, L25
  c->mov64(t1, a3);                                 // or t1, a3, r0
  if (bc) {goto block_41;}                          // branch non-likely

  // Unknown instr: bgezal r0, L16
  c->mov64(t2, t0);                                 // or t2, t0, r0
  //if (bc) {goto block_15;}                          // branch non-likely
  sub_l16_b15(c);

  bc = c->sgpr64(t4) == c->sgpr64(s7);              // beq t4, s7, L25
  c->mov64(t1, t0);                                 // or t1, t0, r0
  if (bc) {goto block_41;}                          // branch non-likely

  // Unknown instr: bgezal r0, L16
  c->mov64(t2, a2);                                 // or t2, a2, r0
  //if (bc) {goto block_15;}                          // branch non-likely
  sub_l16_b15(c);

  bc = c->sgpr64(t1) == c->sgpr64(s7);              // beq t1, s7, L25
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_41;}                          // branch non-likely

  //beq r0, r0, L15                                 // beq r0, r0, L15
  c->daddiu(a1, a1, 32);                            // daddiu a1, a1, 32
  goto block_1;                                     // branch always


  block_40:
  c->load_symbol(t9, cache.format);                 // lw t9, format(s7)
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  //daddiu a1, fp, L87                                // daddiu a1, fp, L87
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  //c->jalr(call_addr);                               // jalr ra, t9
  printf("ERROR: Too many edge verts found in edge grab!\n");
  //beq r0, r0, L26                                 // beq r0, r0, L26
  // nop                                            // sll r0, r0, 0
  goto block_42;                                    // branch always


  block_41:
  c->load_symbol(t9, cache.format);                 // lw t9, format(s7)
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  //daddiu a1, fp, L86                                // daddiu a1, fp, L86
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  //c->jalr(call_addr);                               // jalr ra, t9
  printf("ERROR: Too many edges found in edge grab!\n");

  block_42:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
  //jr ra                               method_10_collide_edge_hold_list            // jr ra
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.format = intern_from_c("format").c();
  gLinkedFunctionTable.reg("(method 15 collide-edge-work)", execute, 128);
}

} // namespace method_15_collide_edge_work
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace method_10_collide_edge_hold_list {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
//  bool bc = false;
//  u32 call_addr = 0;
  bool cop1_bc = false;
  // nop                                            // sll r0, r0, 0
  c->daddiu(a2, a0, 1552);                          // daddiu a2, a0, 1552
  c->lwu(t0, 4, a0);                                // lwu t0, 4(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf1, 16, a1);                             // lqc2 vf1, 16(a1)
  c->dsll(a3, t0, 4);                               // dsll a3, t0, 4
  c->lwu(v1, 8, a0);                                // lwu v1, 8(a0)
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  c->lwc1(f0, 4, a1);                               // lwc1 f0, 4(a1)
  c->daddu(a2, a2, a3);                             // daddu a2, a2, a3
  c->sw(t0, 4, a0);                                 // sw t0, 4(a0)
  c->sqc2(vf1, 0, a2);                              // sqc2 vf1, 0(a2)
  if (((s64)c->sgpr64(v1)) == ((s64)c->sgpr64(s7))) {// beql v1, s7, L68
    c->sw(a1, 8, a0);                               // sw a1, 8(a0)
    goto block_10;
  }

//  block_2:
  c->lwc1(f1, 4, v1);                               // lwc1 f1, 4(v1)
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  if (cop1_bc) {                                    // bc1tl L69
    c->sw(a1, 8, a0);                               // sw a1, 8(a0)
    goto block_11;
  }

//  block_4:
  c->mov64(a0, v1);                                 // or a0, v1, r0

  block_5:
  c->lwu(v1, 0, v1);                                // lwu v1, 0(v1)
  if (((s64)c->sgpr64(v1)) == ((s64)c->sgpr64(s7))) {// beql v1, s7, L70
    c->sw(a1, 0, a0);                               // sw a1, 0(a0)
    goto block_12;
  }

//  block_7:
  c->lwc1(f1, 4, v1);                               // lwc1 f1, 4(v1)
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  if (!cop1_bc) {                                   // bc1fl L67
    c->mov64(a0, v1);                               // or a0, v1, r0
    goto block_5;
  }

//  block_9:
  c->sw(a1, 0, a0);                                 // sw a1, 0(a0)
  //beq r0, r0, L71                                 // beq r0, r0, L71
  c->sw(v1, 0, a1);                                 // sw v1, 0(a1)
  goto block_13;                                    // branch always


  block_10:
  //beq r0, r0, L71                                 // beq r0, r0, L71
  c->sw(s7, 0, a1);                                 // sw s7, 0(a1)
  goto block_13;                                    // branch always


  block_11:
  //beq r0, r0, L71                                 // beq r0, r0, L71
  c->sw(v1, 0, a1);                                 // sw v1, 0(a1)
  goto block_13;                                    // branch always


  block_12:
  c->sw(s7, 0, a1);                                 // sw s7, 0(a1)

  block_13:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  gLinkedFunctionTable.reg("(method 10 collide-edge-hold-list)", execute, 128);
}

} // namespace method_10_collide_edge_hold_list
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace method_18_collide_edge_work {
struct Cache {
  void* collide_edge_hold_list; // collide-edge-hold-list
  void* collide_edge_work; // collide-edge-work
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -176);                          // daddiu sp, sp, -176
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s0, 64, sp);                                // sq s0, 64(sp)
  c->sq(s1, 80, sp);                                // sq s1, 80(sp)
  c->sq(s2, 96, sp);                                // sq s2, 96(sp)
  c->sq(s3, 112, sp);                               // sq s3, 112(sp)
  c->sq(s4, 128, sp);                               // sq s4, 128(sp)
  c->sq(s5, 144, sp);                               // sq s5, 144(sp)
  c->sq(gp, 160, sp);                               // sq gp, 160(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->mov64(s5, a1);                                 // or s5, a1, r0
  c->mov64(s4, a2);                                 // or s4, a2, r0
  c->daddiu(s3, sp, 16);                            // daddiu s3, sp, 16
  c->addiu(s2, r0, 16);                             // addiu s2, r0, 16

  block_1:
  bc = c->sgpr64(s2) == 0;                          // beq s2, r0, L64
  c->daddiu(s2, s2, -1);                            // daddiu s2, s2, -1
  if (bc) {goto block_25;}                          // branch non-likely

  c->lwu(s1, 8, s5);                                // lwu s1, 8(s5)
  bc = c->sgpr64(s1) == c->sgpr64(s7);              // beq s1, s7, L64
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_25;}                          // branch non-likely

  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->load_symbol(v1, cache.collide_edge_work);      // lw v1, collide-edge-work(s7)
  c->lwu(t9, 92, v1);                               // lwu t9, 92(v1)
  c->mov64(a1, s1);                                 // or a1, s1, r0
  c->mov64(a2, s4);                                 // or a2, s4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  bc = c->sgpr64(v1) == c->sgpr64(s7);              // beq v1, s7, L59
  c->lwu(v1, 0, s1);                                // lwu v1, 0(s1)
  if (bc) {goto block_5;}                           // branch non-likely

  c->daddiu(v1, s7, 8);                             // daddiu v1, s7, #t
  c->mov64(v0, v1);                                 // or v0, v1, r0
  //beq r0, r0, L65                                 // beq r0, r0, L65
  // nop                                            // sll r0, r0, 0
  goto block_26;                                    // branch always


  block_5:
  c->lb(s0, 8, s1);                                 // lb s0, 8(s1)
  c->sw(v1, 8, s5);                                 // sw v1, 8(s5)
  bc = ((s64)c->sgpr64(s0)) > 0;                    // bgtz s0, L62
  c->addiu(v1, r0, 2);                              // addiu v1, r0, 2
  if (bc) {goto block_17;}                          // branch non-likely

  if (((s64)c->sgpr64(s0)) < 0) {                   // bltzl s0, L63
    c->dsubu(s0, r0, s0);                           // dsubu s0, r0, s0
    goto block_21;
  }

//  block_8:
  c->dsll(v1, s0, 2);                               // dsll v1, s0, 2
  c->daddiu(a0, gp, 168);                           // daddiu a0, gp, 168
  c->daddu(v1, a0, v1);                             // daddu v1, a0, v1
  c->lw(v1, 0, v1);                                 // lw v1, 0(v1)
  c->mov128_vf_gpr(vf1, v1);                        // qmtc2.i vf1, v1
  c->lwu(a2, 12, s1);                               // lwu a2, 12(s1)
  c->vmove(DEST::xyzw, vf5, vf0);                   // vmove.xyzw vf5, vf0
  c->lqc2(vf3, 16, s1);                             // lqc2 vf3, 16(s1)
  c->vmove(DEST::xyzw, vf6, vf0);                   // vmove.xyzw vf6, vf0
  c->lqc2(vf4, 32, a2);                             // lqc2 vf4, 32(a2)
  c->lwu(v1, 12, a2);                               // lwu v1, 12(a2)
  c->vmul_bc(DEST::xyz, BC::x, vf2, vf4, vf1);      // vmulx.xyz vf2, vf4, vf1
  c->vadd(DEST::xyz, vf5, vf3, vf2);                // vadd.xyz vf5, vf3, vf2
  c->vsub(DEST::xyz, vf6, vf3, vf2);                // vsub.xyz vf6, vf3, vf2
  c->lqc2(vf8, 0, v1);                              // lqc2 vf8, 0(v1)
  c->sqc2(vf6, 16, s3);                             // sqc2 vf6, 16(s3)
  c->sqc2(vf2, 32, s3);                             // sqc2 vf2, 32(s3)
  c->sw(a2, 0, s3);                                 // sw a2, 0(s3)
  c->sw(s7, 4, s3);                                 // sw s7, 4(s3)
  c->vsub(DEST::xyz, vf9, vf8, vf5);                // vsub.xyz vf9, vf8, vf5
  c->vmul(DEST::xyz, vf9, vf9, vf2);                // vmul.xyz vf9, vf9, vf2
  c->vadd_bc(DEST::x, BC::y, vf9, vf9, vf9);        // vaddy.x vf9, vf9, vf9
  c->vadd_bc(DEST::x, BC::z, vf9, vf9, vf9);        // vaddz.x vf9, vf9, vf9
  c->mov128_gpr_vf(v1, vf9);                        // qmfc2.i v1, vf9
  bc = ((s64)c->sgpr64(v1)) < 0;                    // bltz v1, L60
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely

  c->sqc2(vf5, 16, s1);                             // sqc2 vf5, 16(s1)
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->load_symbol(v1, cache.collide_edge_work);      // lw v1, collide-edge-work(s7)
  c->lwu(t9, 84, v1);                               // lwu t9, 84(v1)
  c->mov64(a1, s1);                                 // or a1, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  bc = c->sgpr64(v1) == c->sgpr64(s7);              // beq v1, s7, L60
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely

  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->load_symbol(v1, cache.collide_edge_hold_list); // lw v1, collide-edge-hold-list(s7)
  c->lwu(t9, 56, v1);                               // lwu t9, 56(v1)
  c->mov64(a1, s1);                                 // or a1, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->addiu(v1, r0, 1);                              // addiu v1, r0, 1
  c->sb(v1, 8, s1);                                 // sb v1, 8(s1)
  c->addiu(v1, r0, 32);                             // addiu v1, r0, 32
  c->lwu(a0, 0, s5);                                // lwu a0, 0(s5)
  c->addiu(a1, r0, 48);                             // addiu a1, r0, 48
  bc = c->sgpr64(a0) == c->sgpr64(v1);              // beq a0, v1, L58
  c->mult3(v1, a0, a1);                             // mult3 v1, a0, a1
  if (bc) {goto block_1;}                           // branch non-likely

  c->daddu(v1, v1, s5);                             // daddu v1, v1, s5
  c->sw(r0, 4, s3);                                 // sw r0, 4(s3)
  c->daddiu(s1, v1, 16);                            // daddiu s1, v1, 16

  block_12:
  c->lwu(a2, 0, s3);                                // lwu a2, 0(s3)
  c->lqc2(vf6, 16, s3);                             // lqc2 vf6, 16(s3)
  c->lqc2(vf2, 32, s3);                             // lqc2 vf2, 32(s3)
  c->lwu(v1, 8, a2);                                // lwu v1, 8(a2)
  c->lqc2(vf7, 0, v1);                              // lqc2 vf7, 0(v1)
  c->vsub(DEST::xyz, vf9, vf6, vf7);                // vsub.xyz vf9, vf6, vf7
  c->vmul(DEST::xyz, vf9, vf9, vf2);                // vmul.xyz vf9, vf9, vf2
  c->vadd_bc(DEST::x, BC::y, vf9, vf9, vf9);        // vaddy.x vf9, vf9, vf9
  c->vadd_bc(DEST::x, BC::z, vf9, vf9, vf9);        // vaddz.x vf9, vf9, vf9
  c->mov128_gpr_vf(v1, vf9);                        // qmfc2.i v1, vf9
  bc = ((s64)c->sgpr64(v1)) < 0;                    // bltz v1, L58
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->sqc2(vf6, 16, s1);                             // sqc2 vf6, 16(s1)
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->load_symbol(v1, cache.collide_edge_work);      // lw v1, collide-edge-work(s7)
  c->lwu(t9, 84, v1);                               // lwu t9, 84(v1)
  c->mov64(a1, s1);                                 // or a1, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  bc = c->sgpr64(v1) == c->sgpr64(s7);              // beq v1, s7, L58
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->load_symbol(v1, cache.collide_edge_hold_list); // lw v1, collide-edge-hold-list(s7)
  c->lwu(t9, 56, v1);                               // lwu t9, 56(v1)
  c->mov64(a1, s1);                                 // or a1, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->lwu(v1, 4, s3);                                // lwu v1, 4(s3)
  bc = c->sgpr64(v1) == c->sgpr64(s7);              // beq v1, s7, L61
  c->lwu(v1, 0, s5);                                // lwu v1, 0(s5)
  if (bc) {goto block_16;}                          // branch non-likely

  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  c->sw(v1, 0, s5);                                 // sw v1, 0(s5)

  block_16:
  c->addiu(v1, r0, -1);                             // addiu v1, r0, -1
  //beq r0, r0, L58                                 // beq r0, r0, L58
  c->sb(v1, 8, s1);                                 // sb v1, 8(s1)
  goto block_1;                                     // branch always


  block_17:
  bc = c->sgpr64(s0) == c->sgpr64(v1);              // beq s0, v1, L58
  c->lwu(a2, 12, s1);                               // lwu a2, 12(s1)
  if (bc) {goto block_1;}                           // branch non-likely

  c->dsll(v1, s0, 2);                               // dsll v1, s0, 2
  c->daddiu(a0, gp, 168);                           // daddiu a0, gp, 168
  c->daddu(v1, a0, v1);                             // daddu v1, a0, v1
  c->lw(v1, 0, v1);                                 // lw v1, 0(v1)
  c->mov128_vf_gpr(vf1, v1);                        // qmtc2.i vf1, v1
  c->lqc2(vf3, 16, s1);                             // lqc2 vf3, 16(s1)
  c->vmove(DEST::xyzw, vf5, vf0);                   // vmove.xyzw vf5, vf0
  c->lqc2(vf4, 32, a2);                             // lqc2 vf4, 32(a2)
  c->lwu(v1, 12, a2);                               // lwu v1, 12(a2)
  c->vmul_bc(DEST::xyz, BC::x, vf2, vf4, vf1);      // vmulx.xyz vf2, vf4, vf1
  c->vadd(DEST::xyz, vf5, vf3, vf2);                // vadd.xyz vf5, vf3, vf2
  c->lqc2(vf8, 0, v1);                              // lqc2 vf8, 0(v1)
  c->vsub(DEST::xyz, vf9, vf8, vf5);                // vsub.xyz vf9, vf8, vf5
  c->vmul(DEST::xyz, vf9, vf9, vf2);                // vmul.xyz vf9, vf9, vf2
  c->vadd_bc(DEST::x, BC::y, vf9, vf9, vf9);        // vaddy.x vf9, vf9, vf9
  c->vadd_bc(DEST::x, BC::z, vf9, vf9, vf9);        // vaddz.x vf9, vf9, vf9
  c->mov128_gpr_vf(v1, vf9);                        // qmfc2.i v1, vf9
  bc = ((s64)c->sgpr64(v1)) < 0;                    // bltz v1, L58
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->sqc2(vf5, 16, s1);                             // sqc2 vf5, 16(s1)
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->load_symbol(v1, cache.collide_edge_work);      // lw v1, collide-edge-work(s7)
  c->lwu(t9, 84, v1);                               // lwu t9, 84(v1)
  c->mov64(a1, s1);                                 // or a1, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  bc = c->sgpr64(v1) == c->sgpr64(s7);              // beq v1, s7, L58
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->load_symbol(v1, cache.collide_edge_hold_list); // lw v1, collide-edge-hold-list(s7)
  c->lwu(t9, 56, v1);                               // lwu t9, 56(v1)
  c->mov64(a1, s1);                                 // or a1, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->daddiu(v1, s0, 1);                             // daddiu v1, s0, 1
  //beq r0, r0, L58                                 // beq r0, r0, L58
  c->sb(v1, 8, s1);                                 // sb v1, 8(s1)
  goto block_1;                                     // branch always


  block_21:
  bc = c->sgpr64(s0) == c->sgpr64(v1);              // beq s0, v1, L58
  c->lwu(a2, 12, s1);                               // lwu a2, 12(s1)
  if (bc) {goto block_1;}                           // branch non-likely

  c->dsll(v1, s0, 2);                               // dsll v1, s0, 2
  c->daddiu(a0, gp, 168);                           // daddiu a0, gp, 168
  c->daddu(v1, a0, v1);                             // daddu v1, a0, v1
  c->lw(v1, 0, v1);                                 // lw v1, 0(v1)
  c->mov128_vf_gpr(vf1, v1);                        // qmtc2.i vf1, v1
  c->lqc2(vf3, 16, s1);                             // lqc2 vf3, 16(s1)
  c->vmove(DEST::xyzw, vf6, vf0);                   // vmove.xyzw vf6, vf0
  c->lqc2(vf4, 32, a2);                             // lqc2 vf4, 32(a2)
  c->lwu(v1, 8, a2);                                // lwu v1, 8(a2)
  c->vmul_bc(DEST::xyz, BC::x, vf2, vf4, vf1);      // vmulx.xyz vf2, vf4, vf1
  c->vsub(DEST::xyz, vf6, vf3, vf2);                // vsub.xyz vf6, vf3, vf2
  c->lqc2(vf7, 0, v1);                              // lqc2 vf7, 0(v1)
  c->vsub(DEST::xyz, vf9, vf6, vf7);                // vsub.xyz vf9, vf6, vf7
  c->vmul(DEST::xyz, vf9, vf9, vf2);                // vmul.xyz vf9, vf9, vf2
  c->vadd_bc(DEST::x, BC::y, vf9, vf9, vf9);        // vaddy.x vf9, vf9, vf9
  c->vadd_bc(DEST::x, BC::z, vf9, vf9, vf9);        // vaddz.x vf9, vf9, vf9
  c->mov128_gpr_vf(v1, vf9);                        // qmfc2.i v1, vf9
  bc = ((s64)c->sgpr64(v1)) < 0;                    // bltz v1, L58
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->sqc2(vf6, 16, s1);                             // sqc2 vf6, 16(s1)
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->load_symbol(v1, cache.collide_edge_work);      // lw v1, collide-edge-work(s7)
  c->lwu(t9, 84, v1);                               // lwu t9, 84(v1)
  c->mov64(a1, s1);                                 // or a1, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  bc = c->sgpr64(v1) == c->sgpr64(s7);              // beq v1, s7, L58
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->load_symbol(v1, cache.collide_edge_hold_list); // lw v1, collide-edge-hold-list(s7)
  c->lwu(t9, 56, v1);                               // lwu t9, 56(v1)
  c->mov64(a1, s1);                                 // or a1, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->daddiu(v1, s0, 1);                             // daddiu v1, s0, 1
  c->dsubu(v1, r0, v1);                             // dsubu v1, r0, v1
  //beq r0, r0, L58                                 // beq r0, r0, L58
  c->sb(v1, 8, s1);                                 // sb v1, 8(s1)
  goto block_1;                                     // branch always


  block_25:
  c->mov64(v0, s7);                                 // or v0, s7, r0

  block_26:
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 160, sp);                               // lq gp, 160(sp)
  c->lq(s5, 144, sp);                               // lq s5, 144(sp)
  c->lq(s4, 128, sp);                               // lq s4, 128(sp)
  c->lq(s3, 112, sp);                               // lq s3, 112(sp)
  c->lq(s2, 96, sp);                                // lq s2, 96(sp)
  c->lq(s1, 80, sp);                                // lq s1, 80(sp)
  c->lq(s0, 64, sp);                                // lq s0, 64(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 176);                           // daddiu sp, sp, 176
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.collide_edge_hold_list = intern_from_c("collide-edge-hold-list").c();
  cache.collide_edge_work = intern_from_c("collide-edge-work").c();
  gLinkedFunctionTable.reg("(method 18 collide-edge-work)", execute, 256);
}

} // namespace method_18_collide_edge_work
} // namespace Mips2C
