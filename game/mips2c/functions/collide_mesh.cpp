//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/kscheme.h"

// clang-format off
namespace Mips2C {
namespace method_12_collide_mesh {
struct Cache {
  void* closest_pt_in_triangle; // closest-pt-in-triangle
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -160);                          // daddiu sp, sp, -160
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  c->sq(s1, 64, sp);                                // sq s1, 64(sp)
  c->sq(s2, 80, sp);                                // sq s2, 80(sp)
  c->sq(s3, 96, sp);                                // sq s3, 96(sp)
  c->sq(s4, 112, sp);                               // sq s4, 112(sp)
  c->sq(s5, 128, sp);                               // sq s5, 128(sp)
  c->sq(gp, 144, sp);                               // sq gp, 144(sp)
  c->mov64(gp, a2);                                 // or gp, a2, r0
  c->mov64(s5, a3);                                 // or s5, a3, r0
  c->mov64(s2, t0);                                 // or s2, t0, r0
  c->daddiu(s4, sp, 16);                            // daddiu s4, sp, 16
  c->lw_float_constant(v1, 0x42f5c28f);             // lw v1, L34(fp) 122.88
  c->mov64(s3, a1);                                 // or s3, a1, r0
  c->lqc2(vf3, 0, s5);                              // lqc2 vf3, 0(s5)
  c->mov128_vf_gpr(vf14, v1);                       // qmtc2.i vf14, v1
  c->lwu(s1, 4, a0);                                // lwu s1, 4(a0)
  c->vadd_bc(DEST::w, BC::x, vf3, vf3, vf14);       // vaddx.w vf3, vf3, vf14
  // nop                                            // sll r0, r0, 0
  c->vsub_bc(DEST::xyzw, BC::w, vf12, vf3, vf3);    // vsubw.xyzw vf12, vf3, vf3
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::xyzw, BC::w, vf13, vf3, vf3);    // vaddw.xyzw vf13, vf3, vf3
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf12, vf12);                // vftoi0.xyzw vf12, vf12
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf13, vf13);                // vftoi0.xyzw vf13, vf13
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(v1, vf12);                       // qmfc2.i v1, vf12
  c->sqc2(vf12, 16, s4);                            // sqc2 vf12, 16(s4)
  c->mov128_gpr_vf(a0, vf13);                       // qmfc2.i a0, vf13
  c->sqc2(vf13, 32, s4);                            // sqc2 vf13, 32(s4)

  block_1:
  bc = c->sgpr64(s1) == 0;                          // beq s1, r0, L29
  c->lwu(a2, 60, s3);                               // lwu a2, 60(s3)
  if (bc) {goto block_14;}                          // branch non-likely

  c->addiu(a3, r0, 56);                             // addiu a3, r0, 56
  c->addiu(a1, r0, 0);                              // addiu a1, r0, 0
  c->and_(a2, a2, a3);                              // and a2, a2, a3
  c->addiu(a3, r0, 16);                             // addiu a3, r0, 16
  bc = c->sgpr64(a2) == c->sgpr64(a1);              // beq a2, a1, L27
  c->daddiu(s1, s1, -1);                            // daddiu s1, s1, -1
  if (bc) {goto block_5;}                           // branch non-likely

  if (((s64)c->sgpr64(a2)) != ((s64)c->sgpr64(a3))) {// bnel a2, a3, L26
    c->daddiu(s3, s3, 96);                          // daddiu s3, s3, 96
    goto block_1;
  }

  block_5:
  // nop                                            // sll r0, r0, 0
  c->lq(a2, 64, s3);                                // lq a2, 64(s3)
  // nop                                            // sll r0, r0, 0
  c->lq(a1, 80, s3);                                // lq a1, 80(s3)
  c->pcgtw(a2, a2, a0);                             // pcgtw a2, a2, a0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(a1, v1, a1);                             // pcgtw a1, v1, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->por(a1, a2, a1);                               // por a1, a2, a1
  // nop                                            // sll r0, r0, 0
  c->ppach(a1, r0, a1);                             // ppach a1, r0, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsll(a1, a1, 16);                              // dsll a1, a1, 16
  // nop                                            // sll r0, r0, 0
  if (((s64)c->sgpr64(a1)) != ((s64)0)) {           // bnel a1, r0, L26
    c->daddiu(s3, s3, 96);                          // daddiu s3, s3, 96
    goto block_1;
  }

  block_7:
  c->load_symbol(t9, cache.closest_pt_in_triangle); // lw t9, closest-pt-in-triangle(s7)
  c->daddu(a0, r0, s4);                             // daddu a0, r0, s4
  c->mov64(a1, s5);                                 // or a1, s5, r0
  c->daddu(a2, r0, s3);                             // daddu a2, r0, s3
  c->daddiu(a3, s3, 48);                            // daddiu a3, s3, 48
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lqc2(vf2, 0, s4);                              // lqc2 vf2, 0(s4)
  c->lqc2(vf3, 0, s5);                              // lqc2 vf3, 0(s5)
  c->lqc2(vf1, 48, s3);                             // lqc2 vf1, 48(s3)
  c->lq(v1, 16, s4);                                // lq v1, 16(s4)
  c->lq(a0, 32, s4);                                // lq a0, 32(s4)
  c->vsub(DEST::xyzw, vf4, vf3, vf2);               // vsub.xyzw vf4, vf3, vf2
  c->lwu(a1, 60, s3);                               // lwu a1, 60(s3)
  c->vmul(DEST::xyzw, vf5, vf4, vf1);               // vmul.xyzw vf5, vf4, vf1
  c->lqc2(vf7, 0, s3);                              // lqc2 vf7, 0(s3)
  c->vmul(DEST::xyzw, vf6, vf4, vf4);               // vmul.xyzw vf6, vf4, vf4
  c->lqc2(vf8, 16, s3);                             // lqc2 vf8, 16(s3)
  c->vmove(DEST::w, vf1, vf0);                      // vmove.w vf1, vf0
  c->lqc2(vf9, 32, s3);                             // lqc2 vf9, 32(s3)
  c->vadd_bc(DEST::y, BC::x, vf5, vf5, vf5);        // vaddx.y vf5, vf5, vf5
  c->daddiu(s3, s3, 96);                            // daddiu s3, s3, 96
  c->vadd_bc(DEST::x, BC::y, vf6, vf6, vf6);        // vaddy.x vf6, vf6, vf6
  c->mtc1(f5, s2);                                  // mtc1 f5, s2
  c->vadd_bc(DEST::y, BC::z, vf5, vf5, vf5);        // vaddz.y vf5, vf5, vf5
  c->fprs[f3] = 122.88;                             // lwc1 f3, L34(fp)
  c->vadd_bc(DEST::x, BC::z, vf6, vf6, vf6);        // vaddz.x vf6, vf6, vf6
  c->fprs[f4] = -1024.0;                            // lwc1 f4, L33(fp)
  c->vsqrt(vf6, BC::x);                             // vsqrt Q, vf6.x
  c->mov128_gpr_vf(a2, vf5);                        // qmfc2.i a2, vf5
  c->vwaitq();                                      // vwaitq
  c->lwc1(f1, 12, s5);                              // lwc1 f1, 12(s5)
  c->vaddq(DEST::x, vf6, vf0);                      // vaddq.x vf6, vf0, Q
  c->vmove(DEST::xyzw, vf10, vf6);                  // vmove.xyzw vf10, vf6
  if (((s64)c->sgpr64(a2)) < 0) {                   // bltzl a2, L28
    c->vsub(DEST::xyzw, vf10, vf0, vf10);           // vsub.xyzw vf10, vf0, vf10
    goto block_9;
  }

  block_9:
  c->mov128_gpr_vf(a2, vf10);                       // qmfc2.i a2, vf10
  c->mtc1(f2, a2);                                  // mtc1 f2, a2
  c->subs(f2, f2, f1);                              // sub.s f2, f2, f1
  cop1_bc = c->fprs[f5] < c->fprs[f2];              // c.lt.s f5, f2
  bc = cop1_bc;                                     // bc1t L26
  c->vdiv(vf0, BC::w, vf6, BC::x);                  // vdiv Q, vf0.w, vf6.x
  if (bc) {goto block_1;}                           // branch non-likely

  cop1_bc = c->fprs[f3] <= c->fprs[f2];             // c.le.s f3, f2
  bc = cop1_bc;                                     // bc1t L26
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  cop1_bc = c->fprs[f2] <= c->fprs[f4];             // c.le.s f2, f4
  bc = cop1_bc;                                     // bc1t L26
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->vwaitq();                                      // vwaitq
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyzw, vf11, vf4);                  // vmulq.xyzw vf11, vf4, Q
  c->fprs[f6] = 0.707;                              // lwc1 f6, L35(fp)
  c->vmul(DEST::xyzw, vf5, vf11, vf1);              // vmul.xyzw vf5, vf11, vf1
  c->vadd_bc(DEST::x, BC::y, vf5, vf5, vf5);        // vaddy.x vf5, vf5, vf5
  c->vadd_bc(DEST::x, BC::z, vf5, vf5, vf5);        // vaddz.x vf5, vf5, vf5
  c->mov128_gpr_vf(a2, vf5);                        // qmfc2.i a2, vf5
  c->mtc1(f7, a2);                                  // mtc1 f7, a2
  cop1_bc = c->fprs[f7] < c->fprs[f6];              // c.lt.s f7, f6
  bc = cop1_bc;                                     // bc1t L26
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mfc1(s2, f2);                                  // mfc1 s2, f2
  c->sqc2(vf7, 0, gp);                              // sqc2 vf7, 0(gp)
  c->sqc2(vf8, 16, gp);                             // sqc2 vf8, 16(gp)
  c->sqc2(vf9, 32, gp);                             // sqc2 vf9, 32(gp)
  c->sqc2(vf2, 48, gp);                             // sqc2 vf2, 48(gp)
  c->sqc2(vf1, 64, gp);                             // sqc2 vf1, 64(gp)
  //beq r0, r0, L26                                 // beq r0, r0, L26
  c->sw(a1, 80, gp);                                // sw a1, 80(gp)
  goto block_1;                                     // branch always


  block_14:
  c->mov64(v0, s2);                                 // or v0, s2, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
  c->lq(gp, 144, sp);                               // lq gp, 144(sp)
  c->lq(s5, 128, sp);                               // lq s5, 128(sp)
  c->lq(s4, 112, sp);                               // lq s4, 112(sp)
  c->lq(s3, 96, sp);                                // lq s3, 96(sp)
  c->lq(s2, 80, sp);                                // lq s2, 80(sp)
  c->lq(s1, 64, sp);                                // lq s1, 64(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 160);                           // daddiu sp, sp, 160
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.closest_pt_in_triangle = intern_from_c("closest-pt-in-triangle").c();
  gLinkedFunctionTable.reg("(method 12 collide-mesh)", execute, 256);
}

} // namespace method_12_collide_mesh
} // namespace Mips2C


//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/kscheme.h"
namespace Mips2C {
namespace method_11_collide_mesh {
struct Cache {
  void* closest_pt_in_triangle; // closest-pt-in-triangle
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -160);                          // daddiu sp, sp, -160
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  c->sq(s1, 64, sp);                                // sq s1, 64(sp)
  c->sq(s2, 80, sp);                                // sq s2, 80(sp)
  c->sq(s3, 96, sp);                                // sq s3, 96(sp)
  c->sq(s4, 112, sp);                               // sq s4, 112(sp)
  c->sq(s5, 128, sp);                               // sq s5, 128(sp)
  c->sq(gp, 144, sp);                               // sq gp, 144(sp)
  c->mov64(gp, a2);                                 // or gp, a2, r0
  c->mov64(s5, a3);                                 // or s5, a3, r0
  c->mov64(s1, t0);                                 // or s1, t0, r0
  c->daddiu(s4, sp, 16);                            // daddiu s4, sp, 16
  // nop                                            // sll r0, r0, 0
  c->mov64(s3, a1);                                 // or s3, a1, r0
  c->lqc2(vf3, 0, s5);                              // lqc2 vf3, 0(s5)
  // nop                                            // sll r0, r0, 0
  c->lwu(s2, 4, a0);                                // lwu s2, 4(a0)
  c->vsub_bc(DEST::xyzw, BC::w, vf12, vf3, vf3);    // vsubw.xyzw vf12, vf3, vf3
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::xyzw, BC::w, vf13, vf3, vf3);    // vaddw.xyzw vf13, vf3, vf3
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf12, vf12);                // vftoi0.xyzw vf12, vf12
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf13, vf13);                // vftoi0.xyzw vf13, vf13
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(v1, vf12);                       // qmfc2.i v1, vf12
  c->sqc2(vf12, 16, s4);                            // sqc2 vf12, 16(s4)
  c->mov128_gpr_vf(a0, vf13);                       // qmfc2.i a0, vf13
  c->sqc2(vf13, 32, s4);                            // sqc2 vf13, 32(s4)

  block_1:
  bc = c->sgpr64(s2) == 0;                          // beq s2, r0, L24
  c->lq(a2, 64, s3);                                // lq a2, 64(s3)
  if (bc) {goto block_10;}                          // branch non-likely

  c->daddiu(s2, s2, -1);                            // daddiu s2, s2, -1
  c->lq(a1, 80, s3);                                // lq a1, 80(s3)
  c->pcgtw(a2, a2, a0);                             // pcgtw a2, a2, a0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(a1, v1, a1);                             // pcgtw a1, v1, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->por(a1, a2, a1);                               // por a1, a2, a1
  // nop                                            // sll r0, r0, 0
  c->ppach(a1, r0, a1);                             // ppach a1, r0, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsll(a1, a1, 16);                              // dsll a1, a1, 16
  // nop                                            // sll r0, r0, 0
  if (((s64)c->sgpr64(a1)) != ((s64)0)) {           // bnel a1, r0, L22
    c->daddiu(s3, s3, 96);                          // daddiu s3, s3, 96
    goto block_1;
  }

  block_4:
  c->load_symbol(t9, cache.closest_pt_in_triangle); // lw t9, closest-pt-in-triangle(s7)
  c->daddu(a0, r0, s4);                             // daddu a0, r0, s4
  c->mov64(a1, s5);                                 // or a1, s5, r0
  c->daddu(a2, r0, s3);                             // daddu a2, r0, s3
  c->daddiu(a3, s3, 48);                            // daddiu a3, s3, 48
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lqc2(vf2, 0, s4);                              // lqc2 vf2, 0(s4)
  c->lqc2(vf3, 0, s5);                              // lqc2 vf3, 0(s5)
  c->lqc2(vf1, 48, s3);                             // lqc2 vf1, 48(s3)
  c->lq(v1, 16, s4);                                // lq v1, 16(s4)
  c->lq(a0, 32, s4);                                // lq a0, 32(s4)
  c->vsub(DEST::xyzw, vf4, vf3, vf2);               // vsub.xyzw vf4, vf3, vf2
  c->lwu(a1, 60, s3);                               // lwu a1, 60(s3)
  c->vmul(DEST::xyzw, vf5, vf4, vf1);               // vmul.xyzw vf5, vf4, vf1
  c->lqc2(vf7, 0, s3);                              // lqc2 vf7, 0(s3)
  c->vmul(DEST::xyzw, vf6, vf4, vf4);               // vmul.xyzw vf6, vf4, vf4
  c->lqc2(vf8, 16, s3);                             // lqc2 vf8, 16(s3)
  c->vmove(DEST::w, vf1, vf0);                      // vmove.w vf1, vf0
  c->lqc2(vf9, 32, s3);                             // lqc2 vf9, 32(s3)
  c->vadd_bc(DEST::y, BC::x, vf5, vf5, vf5);        // vaddx.y vf5, vf5, vf5
  c->daddiu(s3, s3, 96);                            // daddiu s3, s3, 96
  c->vadd_bc(DEST::x, BC::y, vf6, vf6, vf6);        // vaddy.x vf6, vf6, vf6
  c->mtc1(f3, s1);                                  // mtc1 f3, s1
  c->vadd_bc(DEST::y, BC::z, vf5, vf5, vf5);        // vaddz.y vf5, vf5, vf5
  c->vadd_bc(DEST::x, BC::z, vf6, vf6, vf6);        // vaddz.x vf6, vf6, vf6
  c->vsqrt(vf6, BC::x);                             // vsqrt Q, vf6.x
  c->mov128_gpr_vf(a2, vf5);                        // qmfc2.i a2, vf5
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->vwaitq();                                      // vwaitq
  c->lwc1(f1, 12, s5);                              // lwc1 f1, 12(s5)
  c->vaddq(DEST::x, vf6, vf0);                      // vaddq.x vf6, vf0, Q
  c->vmove(DEST::xyzw, vf10, vf6);                  // vmove.xyzw vf10, vf6
  if (((s64)c->sgpr64(a2)) < 0) {                   // bltzl a2, L23
    c->vsub(DEST::xyzw, vf10, vf0, vf10);           // vsub.xyzw vf10, vf0, vf10
    goto block_6;
  }

  block_6:
  c->mov128_gpr_vf(a2, vf10);                       // qmfc2.i a2, vf10
  c->mtc1(f2, a2);                                  // mtc1 f2, a2
  c->subs(f2, f2, f1);                              // sub.s f2, f2, f1
  c->vdiv(vf0, BC::w, vf6, BC::x);                  // vdiv Q, vf0.w, vf6.x
  cop1_bc = c->fprs[f3] < c->fprs[f2];              // c.lt.s f3, f2
  bc = cop1_bc;                                     // bc1t L22
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  cop1_bc = c->fprs[f0] <= c->fprs[f2];             // c.le.s f0, f2
  bc = cop1_bc;                                     // bc1t L22
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->vwaitq();                                      // vwaitq
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyzw, vf11, vf4);                  // vmulq.xyzw vf11, vf4, Q
  c->fprs[f4] = 0.707;                              // lwc1 f4, L35(fp)
  c->vmul(DEST::xyzw, vf5, vf11, vf1);              // vmul.xyzw vf5, vf11, vf1
  c->vadd_bc(DEST::x, BC::y, vf5, vf5, vf5);        // vaddy.x vf5, vf5, vf5
  c->vadd_bc(DEST::x, BC::z, vf5, vf5, vf5);        // vaddz.x vf5, vf5, vf5
  c->mov128_gpr_vf(a2, vf5);                        // qmfc2.i a2, vf5
  c->mtc1(f5, a2);                                  // mtc1 f5, a2
  cop1_bc = c->fprs[f5] < c->fprs[f4];              // c.lt.s f5, f4
  bc = cop1_bc;                                     // bc1t L22
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mfc1(s1, f2);                                  // mfc1 s1, f2
  c->sqc2(vf7, 0, gp);                              // sqc2 vf7, 0(gp)
  c->sqc2(vf8, 16, gp);                             // sqc2 vf8, 16(gp)
  c->sqc2(vf9, 32, gp);                             // sqc2 vf9, 32(gp)
  c->sqc2(vf2, 48, gp);                             // sqc2 vf2, 48(gp)
  c->sqc2(vf1, 64, gp);                             // sqc2 vf1, 64(gp)
  //beq r0, r0, L22                                 // beq r0, r0, L22
  c->sw(a1, 80, gp);                                // sw a1, 80(gp)
  goto block_1;                                     // branch always


  block_10:
  c->mov64(v0, s1);                                 // or v0, s1, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
  c->lq(gp, 144, sp);                               // lq gp, 144(sp)
  c->lq(s5, 128, sp);                               // lq s5, 128(sp)
  c->lq(s4, 112, sp);                               // lq s4, 112(sp)
  c->lq(s3, 96, sp);                                // lq s3, 96(sp)
  c->lq(s2, 80, sp);                                // lq s2, 80(sp)
  c->lq(s1, 64, sp);                                // lq s1, 64(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 160);                           // daddiu sp, sp, 160
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.closest_pt_in_triangle = intern_from_c("closest-pt-in-triangle").c();
  gLinkedFunctionTable.reg("(method 11 collide-mesh)", execute, 512);
}

} // namespace method_11_collide_mesh
} // namespace Mips2C

