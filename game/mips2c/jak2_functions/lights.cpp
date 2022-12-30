//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace add_light_sphere_to_light_group {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  bool cop1_bc = false;
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 0, a2);                              // lqc2 vf2, 0(a2)
  c->vmax_bc(DEST::xyzw, BC::w, vf1, vf0, vf0);     // vmaxw.xyzw vf1, vf0, vf0
  c->lqc2(vf3, 16, a1);                             // lqc2 vf3, 16(a1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf19, 0, a1);                             // lqc2 vf19, 0(a1)
  c->vsub(DEST::xyzw, vf4, vf3, vf2);               // vsub.xyzw vf4, vf3, vf2
  c->lqc2(vf8, 16, a0);                             // lqc2 vf8, 16(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf9, 64, a0);                             // lqc2 vf9, 64(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf10, 112, a0);                           // lqc2 vf10, 112(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf11, 160, a0);                           // lqc2 vf11, 160(a0)
  c->vmul(DEST::xyzw, vf6, vf4, vf4);               // vmul.xyzw vf6, vf4, vf4
  c->lqc2(vf5, 32, a1);                             // lqc2 vf5, 32(a1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf25, 0, a0);                             // lqc2 vf25, 0(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf26, 48, a0);                            // lqc2 vf26, 48(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf27, 96, a0);                            // lqc2 vf27, 96(a0)
  c->vmula_bc(DEST::w, BC::x, vf0, vf6);            // vmulax.w acc, vf0, vf6
  c->lqc2(vf15, 32, a0);                            // lqc2 vf15, 32(a0)
  c->vmadda_bc(DEST::w, BC::y, vf0, vf6);           // vmadday.w acc, vf0, vf6
  c->lqc2(vf16, 80, a0);                            // lqc2 vf16, 80(a0)
  c->vmadd_bc(DEST::w, BC::z, vf6, vf0, vf6);       // vmaddz.w vf6, vf0, vf6
  c->lqc2(vf17, 128, a0);                           // lqc2 vf17, 128(a0)
  c->vsqrt(vf6, BC::w);                             // vsqrt Q, vf6.w
  c->lqc2(vf18, 176, a0);                           // lqc2 vf18, 176(a0)
  c->vmul_bc(DEST::xyzw, BC::x, vf8, vf8, vf15);    // vmulx.xyzw vf8, vf8, vf15
  c->lwc1(f2, 28, a1);                              // lwc1 f2, 28(a1)
  c->vmul_bc(DEST::xyzw, BC::x, vf9, vf9, vf16);    // vmulx.xyzw vf9, vf9, vf16
  c->lwc1(f3, 4, a1);                               // lwc1 f3, 4(a1)
  c->vmul_bc(DEST::xyzw, BC::x, vf10, vf10, vf17);  // vmulx.xyzw vf10, vf10, vf17
  c->lwc1(f4, 44, a1);                              // lwc1 f4, 44(a1)
  c->vmul_bc(DEST::xyzw, BC::x, vf11, vf11, vf18);  // vmulx.xyzw vf11, vf11, vf18
  c->mov128_gpr_vf(v1, vf1);                        // qmfc2.i v1, vf1
  c->vsub_bc(DEST::w, BC::z, vf19, vf0, vf19);      // vsubz.w vf19, vf0, vf19
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->vmul_bc(DEST::xyzw, BC::x, vf25, vf25, vf15);  // vmulx.xyzw vf25, vf25, vf15
  c->lwc1(f6, 12, a1);                              // lwc1 f6, 12(a1)
  c->vmul_bc(DEST::xyzw, BC::x, vf26, vf26, vf16);  // vmulx.xyzw vf26, vf26, vf16
  c->lb(v1, 63, a1);                                // lb v1, 63(a1)
  c->subs(f7, f1, f3);                              // sub.s f7, f1, f3
  c->lqc2(vf21, 48, a1);                            // lqc2 vf21, 48(a1)
  c->vmul_bc(DEST::xyzw, BC::z, vf20, vf21, vf19);  // vmulz.xyzw vf20, vf21, vf19
  c->dsll(a1, v1, 4);                               // dsll a1, v1, 4
  c->vmul_bc(DEST::xyzw, BC::w, vf21, vf21, vf19);  // vmulw.xyzw vf21, vf21, vf19
  c->daddu(a1, a1, a3);                             // daddu a1, a1, a3
  c->vmul_bc(DEST::xyzw, BC::x, vf27, vf27, vf17);  // vmulx.xyzw vf27, vf27, vf17
  c->lwc1(f9, 1660, a1);                            // lwc1 f9, 1660(a1)
  // nop                                            // sll r0, r0, 0
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyzw, vf7, vf1);                   // vmulq.xyzw vf7, vf1, Q
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a3, vf15);                       // qmfc2.i a3, vf15
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a2, vf16);                       // qmfc2.i a2, vf16
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a1, vf17);                       // qmfc2.i a1, vf17
  c->vdiv(vf0, BC::w, vf7, BC::w);                  // vdiv Q, vf0.w, vf7.w
  c->mov128_gpr_vf(t0, vf7);                        // qmfc2.i t0, vf7
  // nop                                            // sll r0, r0, 0
  c->mtc1(f5, t0);                                  // mtc1 f5, t0
  cop1_bc = c->fprs[f2] < c->fprs[f5];              // c.lt.s f2, f5
  c->divs(f5, f5, f2);                              // div.s f5, f5, f2
  bc = cop1_bc;                                     // bc1t L11
  cop1_bc = c->fprs[f4] == c->fprs[f0];             // c.eq.s f4, f0
  if (bc) {goto block_21;}                          // branch non-likely

  bc = cop1_bc;                                     // bc1t L2
  cop1_bc = c->fprs[f3] == c->fprs[f1];             // c.eq.s f3, f1
  if (bc) {goto block_3;}                           // branch non-likely

  //beq r0, r0, L3                                  // beq r0, r0, L3
  c->vmove(DEST::xyzw, vf6, vf5);                   // vmove.xyzw vf6, vf5
  goto block_4;                                     // branch always


  block_3:
  // nop                                            // sll r0, r0, 0
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyzw, vf6, vf4);                   // vmulq.xyzw vf6, vf4, Q
  // nop                                            // sll r0, r0, 0

  block_4:
  bc = cop1_bc;                                     // bc1t L4
  c->subs(f8, f5, f3);                              // sub.s f8, f5, f3
  if (bc) {goto block_6;}                           // branch non-likely

  c->maxs(f8, f8, f0);                              // max.s f8, f8, f0
  // nop                                            // sll r0, r0, 0
  c->divs(f8, f8, f7);                              // div.s f8, f8, f7
  // nop                                            // sll r0, r0, 0
  c->subs(f8, f1, f8);                              // sub.s f8, f1, f8
  // nop                                            // sll r0, r0, 0
  c->muls(f6, f6, f8);                              // mul.s f6, f6, f8
  // nop                                            // sll r0, r0, 0

  block_6:
  bc = ((s64)c->sgpr64(v1)) < 0;                    // bltz v1, L5
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_8;}                           // branch non-likely

  c->muls(f6, f6, f9);                              // mul.s f6, f6, f9
  // nop                                            // sll r0, r0, 0

  block_8:
  // nop                                            // sll r0, r0, 0
  c->dsll32(v1, a3, 0);                             // dsll32 v1, a3, 0
  // nop                                            // sll r0, r0, 0
  c->dsll32(a2, a2, 0);                             // dsll32 a2, a2, 0
  // nop                                            // sll r0, r0, 0
  c->dsll32(a1, a1, 0);                             // dsll32 a1, a1, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(a3, f6);                                  // mfc1 a3, f6
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L11
  c->mov128_vf_gpr(vf23, a3);                       // qmtc2.i vf23, a3
  if (bc) {goto block_21;}                          // branch non-likely

  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L8
  c->vmul_bc(DEST::xyzw, BC::x, vf21, vf21, vf23);  // vmulx.xyzw vf21, vf21, vf23
  if (bc) {goto block_18;}                          // branch non-likely

  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L9
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_19;}                          // branch non-likely

  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L10
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_20;}                          // branch non-likely

  c->vmula_bc(DEST::xyzw, BC::x, vf15, vf15);       // vmulax.xyzw acc, vf15, vf15
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf16, vf16);      // vmaddax.xyzw acc, vf16, vf16
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf22, vf17, vf17); // vmaddx.xyzw vf22, vf17, vf17
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyzw, vf11, vf11, vf21);            // vadd.xyzw vf11, vf11, vf21
  c->swc1(f1, 176, a0);                             // swc1 f1, 176(a0)
  c->vmul_bc(DEST::xyzw, BC::x, vf20, vf20, vf23);  // vmulx.xyzw vf20, vf20, vf23
  // nop                                            // sll r0, r0, 0
  c->vmul_bc(DEST::xyzw, BC::x, vf6, vf6, vf23);    // vmulx.xyzw vf6, vf6, vf23
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->vrsqrt(vf0, BC::w, vf22, BC::x);               // vrsqrt Q, vf0.w, vf22.x
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf11, 160, a0);                           // sqc2 vf11, 160(a0)
  // nop                                            // sll r0, r0, 0
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyzw, vf15, vf15);                 // vmulq.xyzw vf15, vf15, Q
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyzw, vf16, vf16);                 // vmulq.xyzw vf16, vf16, Q
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyzw, vf17, vf17);                 // vmulq.xyzw vf17, vf17, Q
  // nop                                            // sll r0, r0, 0
  c->vsub_bc(DEST::xyzw, BC::x, vf15, vf1, vf15);   // vsubx.xyzw vf15, vf1, vf15
  // nop                                            // sll r0, r0, 0
  c->vsub_bc(DEST::xyzw, BC::x, vf16, vf1, vf16);   // vsubx.xyzw vf16, vf1, vf16
  // nop                                            // sll r0, r0, 0
  c->vsub_bc(DEST::xyzw, BC::x, vf17, vf1, vf17);   // vsubx.xyzw vf17, vf1, vf17
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::x, vf1, vf15);        // vmulax.xyzw acc, vf1, vf15
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf16);       // vmaddax.xyzw acc, vf1, vf16
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf22, vf1, vf17);  // vmaddx.xyzw vf22, vf1, vf17
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->vdiv(vf0, BC::w, vf22, BC::x);                 // vdiv Q, vf0.w, vf22.x
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf12, 32, a0);                            // lqc2 vf12, 32(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf13, 80, a0);                            // lqc2 vf13, 80(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 128, a0);                           // lqc2 vf14, 128(a0)
  // nop                                            // sll r0, r0, 0
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyzw, vf15, vf15);                 // vmulq.xyzw vf15, vf15, Q
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyzw, vf16, vf16);                 // vmulq.xyzw vf16, vf16, Q
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyzw, vf17, vf17);                 // vmulq.xyzw vf17, vf17, Q
  // nop                                            // sll r0, r0, 0
  c->vmula(DEST::xyzw,  vf1, vf25);                 // vmula.xyzw acc, vf1, vf25
  c->mov128_gpr_vf(a2, vf15);                       // qmfc2.i a2, vf15
  c->vmadd_bc(DEST::xyzw, BC::x, vf25, vf6, vf15);  // vmaddx.xyzw vf25, vf6, vf15
  c->mov128_gpr_vf(a1, vf16);                       // qmfc2.i a1, vf16
  c->vmula(DEST::xyzw,  vf1, vf26);                 // vmula.xyzw acc, vf1, vf26
  c->mov128_gpr_vf(v1, vf17);                       // qmfc2.i v1, vf17
  c->vmadd_bc(DEST::xyzw, BC::x, vf26, vf6, vf16);  // vmaddx.xyzw vf26, vf6, vf16
  c->dsll32(a2, a2, 0);                             // dsll32 a2, a2, 0
  c->vmula(DEST::xyzw,  vf1, vf27);                 // vmula.xyzw acc, vf1, vf27
  c->dsll32(a1, a1, 0);                             // dsll32 a1, a1, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf27, vf6, vf17);  // vmaddx.xyzw vf27, vf6, vf17
  c->dsll32(v1, v1, 0);                             // dsll32 v1, v1, 0
  c->vmula(DEST::xyzw,  vf1, vf8);                  // vmula.xyzw acc, vf1, vf8
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf8, vf20, vf15);  // vmaddx.xyzw vf8, vf20, vf15
  // nop                                            // sll r0, r0, 0
  c->vmula(DEST::xyzw,  vf1, vf9);                  // vmula.xyzw acc, vf1, vf9
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf20, vf16);  // vmaddx.xyzw vf9, vf20, vf16
  // nop                                            // sll r0, r0, 0
  c->vmula(DEST::xyzw,  vf1, vf10);                 // vmula.xyzw acc, vf1, vf10
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf10, vf20, vf17); // vmaddx.xyzw vf10, vf20, vf17
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L6
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_14;}                          // branch non-likely

  c->vmul(DEST::xyzw, vf24, vf25, vf25);            // vmul.xyzw vf24, vf25, vf25
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf15, vf15, vf23);            // vmul.xyzw vf15, vf15, vf23
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::x, vf1, vf24);        // vmulax.xyzw acc, vf1, vf24
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf1, vf24);       // vmadday.xyzw acc, vf1, vf24
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf24, vf1, vf24);  // vmaddz.xyzw vf24, vf1, vf24
  // nop                                            // sll r0, r0, 0
  c->vrsqrt(vf0, BC::w, vf24, BC::x);               // vrsqrt Q, vf0.w, vf24.x
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyzw, vf15, vf15, vf12);            // vadd.xyzw vf15, vf15, vf12
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a2, vf15);                       // qmfc2.i a2, vf15
  // nop                                            // sll r0, r0, 0
  c->mtc1(f10, a2);                                 // mtc1 f10, a2
  c->divs(f10, f1, f10);                            // div.s f10, f1, f10
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sw(a2, 32, a0);                                // sw a2, 32(a0)
  // nop                                            // sll r0, r0, 0
  c->mfc1(a2, f10);                                 // mfc1 a2, f10
  // nop                                            // sll r0, r0, 0
  c->mfc1(a2, f10);                                 // mfc1 a2, f10
  c->mov128_vf_gpr(vf15, a2);                       // qmtc2.i vf15, a2
  c->vmul_bc(DEST::xyzw, BC::x, vf8, vf8, vf15);    // vmulx.xyzw vf8, vf8, vf15
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyzw, vf25, vf25);                 // vmulq.xyzw vf25, vf25, Q
  c->sqc2(vf8, 16, a0);                             // sqc2 vf8, 16(a0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf25, 0, a0);                             // sqc2 vf25, 0(a0)

  block_14:
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L7
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_16;}                          // branch non-likely

  c->vmul(DEST::xyzw, vf24, vf26, vf26);            // vmul.xyzw vf24, vf26, vf26
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf16, vf16, vf23);            // vmul.xyzw vf16, vf16, vf23
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::x, vf1, vf24);        // vmulax.xyzw acc, vf1, vf24
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf1, vf24);       // vmadday.xyzw acc, vf1, vf24
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf24, vf1, vf24);  // vmaddz.xyzw vf24, vf1, vf24
  // nop                                            // sll r0, r0, 0
  c->vrsqrt(vf0, BC::w, vf24, BC::x);               // vrsqrt Q, vf0.w, vf24.x
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyzw, vf16, vf16, vf13);            // vadd.xyzw vf16, vf16, vf13
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a1, vf16);                       // qmfc2.i a1, vf16
  // nop                                            // sll r0, r0, 0
  c->mtc1(f11, a1);                                 // mtc1 f11, a1
  c->divs(f11, f1, f11);                            // div.s f11, f1, f11
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sw(a1, 80, a0);                                // sw a1, 80(a0)
  // nop                                            // sll r0, r0, 0
  c->mfc1(a1, f11);                                 // mfc1 a1, f11
  // nop                                            // sll r0, r0, 0
  c->mfc1(a1, f11);                                 // mfc1 a1, f11
  c->mov128_vf_gpr(vf16, a1);                       // qmtc2.i vf16, a1
  c->vmul_bc(DEST::xyzw, BC::x, vf9, vf9, vf16);    // vmulx.xyzw vf9, vf9, vf16
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyzw, vf26, vf26);                 // vmulq.xyzw vf26, vf26, Q
  c->sqc2(vf9, 64, a0);                             // sqc2 vf9, 64(a0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf26, 48, a0);                            // sqc2 vf26, 48(a0)

  block_16:
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L11
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_21;}                          // branch non-likely

  c->vmul(DEST::xyzw, vf24, vf27, vf27);            // vmul.xyzw vf24, vf27, vf27
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf17, vf17, vf23);            // vmul.xyzw vf17, vf17, vf23
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::x, vf1, vf24);        // vmulax.xyzw acc, vf1, vf24
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf1, vf24);       // vmadday.xyzw acc, vf1, vf24
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf24, vf1, vf24);  // vmaddz.xyzw vf24, vf1, vf24
  // nop                                            // sll r0, r0, 0
  c->vrsqrt(vf0, BC::w, vf24, BC::x);               // vrsqrt Q, vf0.w, vf24.x
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyzw, vf17, vf17, vf14);            // vadd.xyzw vf17, vf17, vf14
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(v1, vf17);                       // qmfc2.i v1, vf17
  // nop                                            // sll r0, r0, 0
  c->mtc1(f12, v1);                                 // mtc1 f12, v1
  c->divs(f12, f1, f12);                            // div.s f12, f1, f12
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 128, a0);                               // sw v1, 128(a0)
  // nop                                            // sll r0, r0, 0
  c->mfc1(v1, f12);                                 // mfc1 v1, f12
  // nop                                            // sll r0, r0, 0
  c->mfc1(v1, f12);                                 // mfc1 v1, f12
  c->mov128_vf_gpr(vf17, v1);                       // qmtc2.i vf17, v1
  c->vmul_bc(DEST::xyzw, BC::x, vf10, vf10, vf17);  // vmulx.xyzw vf10, vf10, vf17
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyzw, vf27, vf27);                 // vmulq.xyzw vf27, vf27, Q
  c->sqc2(vf10, 112, a0);                           // sqc2 vf10, 112(a0)
  //beq r0, r0, L11                                 // beq r0, r0, L11
  c->sqc2(vf27, 96, a0);                            // sqc2 vf27, 96(a0)
  goto block_21;                                    // branch always


  block_18:
  c->vadd(DEST::xyzw, vf11, vf11, vf21);            // vadd.xyzw vf11, vf11, vf21
  c->swc1(f1, 176, a0);                             // swc1 f1, 176(a0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf6, 0, a0);                              // sqc2 vf6, 0(a0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf20, 16, a0);                            // sqc2 vf20, 16(a0)
  // nop                                            // sll r0, r0, 0
  c->swc1(f6, 32, a0);                              // swc1 f6, 32(a0)
  //beq r0, r0, L11                                 // beq r0, r0, L11
  c->sqc2(vf11, 160, a0);                           // sqc2 vf11, 160(a0)
  goto block_21;                                    // branch always


  block_19:
  c->vadd(DEST::xyzw, vf11, vf11, vf21);            // vadd.xyzw vf11, vf11, vf21
  c->swc1(f1, 176, a0);                             // swc1 f1, 176(a0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf6, 48, a0);                             // sqc2 vf6, 48(a0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf20, 64, a0);                            // sqc2 vf20, 64(a0)
  // nop                                            // sll r0, r0, 0
  c->swc1(f6, 80, a0);                              // swc1 f6, 80(a0)
  //beq r0, r0, L11                                 // beq r0, r0, L11
  c->sqc2(vf11, 160, a0);                           // sqc2 vf11, 160(a0)
  goto block_21;                                    // branch always


  block_20:
  c->vadd(DEST::xyzw, vf11, vf11, vf21);            // vadd.xyzw vf11, vf11, vf21
  c->swc1(f1, 176, a0);                             // swc1 f1, 176(a0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf6, 96, a0);                             // sqc2 vf6, 96(a0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf20, 112, a0);                           // sqc2 vf20, 112(a0)
  // nop                                            // sll r0, r0, 0
  c->swc1(f6, 128, a0);                             // swc1 f6, 128(a0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf11, 160, a0);                           // sqc2 vf11, 160(a0)

  block_21:
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
  gLinkedFunctionTable.reg("add-light-sphere-to-light-group", execute, 256);
}

} // namespace add_light_sphere_to_light_group
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace light_hash_add_items {
struct Cache {
  void* light_hash_work; // *light-hash-work*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->daddiu(sp, sp, -48);                           // daddiu sp, sp, -48
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->daddiu(t0, sp, 16);                            // daddiu t0, sp, 16
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 16, a1);                             // lqc2 vf2, 16(a1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf1, 12, a0);                             // lqc2 vf1, 12(a0)
  c->vadd_bc(DEST::xyzw, BC::w, vf3, vf2, vf2);     // vaddw.xyzw vf3, vf2, vf2
  c->lqc2(vf4, 28, a0);                             // lqc2 vf4, 28(a0)
  c->vsub_bc(DEST::xyzw, BC::w, vf2, vf2, vf2);     // vsubw.xyzw vf2, vf2, vf2
  c->load_symbol2(a1, cache.light_hash_work);       // lw a1, *light-hash-work*(s7)
  c->vsub(DEST::xyzw, vf2, vf2, vf1);               // vsub.xyzw vf2, vf2, vf1
  c->lq(v1, 44, a0);                                // lq v1, 44(a0)
  c->vsub(DEST::xyzw, vf3, vf3, vf1);               // vsub.xyzw vf3, vf3, vf1
  c->lq(a1, 0, a1);                                 // lq a1, 0(a1)
  c->vmul(DEST::xyzw, vf2, vf2, vf4);               // vmul.xyzw vf2, vf2, vf4
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf3, vf3, vf4);               // vmul.xyzw vf3, vf3, vf4
  c->dsubu(v1, v1, a1);                             // dsubu v1, v1, a1
  c->vftoi0(DEST::xyzw, vf2, vf2);                  // vftoi0.xyzw vf2, vf2
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf3, vf3);                  // vftoi0.xyzw vf3, vf3
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a3, vf2);                        // qmfc2.i a3, vf2
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a1, vf3);                        // qmfc2.i a1, vf3
  c->pmaxw(a3, a3, r0);                             // pmaxw a3, a3, r0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaxw(a1, a1, r0);                             // pmaxw a1, a1, r0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pminw(a3, a3, v1);                             // pminw a3, a3, v1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pminw(v1, a1, v1);                             // pminw v1, a1, v1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 0, t0);                                 // sq a3, 0(t0)
  // nop                                            // sll r0, r0, 0
  c->sq(v1, 16, t0);                                // sq v1, 16(t0)
  c->addiu(v1, r0, 4);                              // addiu v1, r0, 4
  c->lw(a1, 44, a0);                                // lw a1, 44(a0)
  c->mult3(a1, a1, v1);                             // mult3 a1, a1, v1
  c->lw(a3, 52, a0);                                // lw a3, 52(a0)
  c->mult3(a3, a3, a1);                             // mult3 a3, a3, a1
  c->addiu(t1, r0, 1);                              // addiu t1, r0, 1
  c->lw(t2, 0, t0);                                 // lw t2, 0(t0)
  c->dsubu(t1, t1, t2);                             // dsubu t1, t1, t2
  c->lw(t2, 16, t0);                                // lw t2, 16(t0)
  c->daddu(t1, t1, t2);                             // daddu t1, t1, t2
  c->addiu(t2, r0, 1);                              // addiu t2, r0, 1
  c->lw(t3, 4, t0);                                 // lw t3, 4(t0)
  c->dsubu(t2, t2, t3);                             // dsubu t2, t2, t3
  c->lw(t3, 20, t0);                                // lw t3, 20(t0)
  c->daddu(t3, t2, t3);                             // daddu t3, t2, t3
  c->addiu(t2, r0, 1);                              // addiu t2, r0, 1
  c->lw(t4, 8, t0);                                 // lw t4, 8(t0)
  c->dsubu(t2, t2, t4);                             // dsubu t2, t2, t4
  c->lw(t4, 24, t0);                                // lw t4, 24(t0)
  c->daddu(t2, t2, t4);                             // daddu t2, t2, t4
  c->lwu(t4, 60, a0);                               // lwu t4, 60(a0)
  c->lw(t5, 0, t0);                                 // lw t5, 0(t0)
  c->mult3(t5, t5, v1);                             // mult3 t5, t5, v1
  c->lw(t6, 4, t0);                                 // lw t6, 4(t0)
  c->mult3(t6, t6, a3);                             // mult3 t6, t6, a3
  c->daddu(t5, t5, t6);                             // daddu t5, t5, t6
  c->lw(t0, 8, t0);                                 // lw t0, 8(t0)
  c->mult3(t0, t0, a1);                             // mult3 t0, t0, a1
  c->daddu(t0, t5, t0);                             // daddu t0, t5, t0
  c->daddu(t0, t4, t0);                             // daddu t0, t4, t0
  c->mov64(t3, t3);                                 // or t3, t3, r0
  // nop                                            // sll r0, r0, 0

  block_1:
  c->mov64(t4, t2);                                 // or t4, t2, r0
  c->mov64(t5, t0);                                 // or t5, t0, r0

  block_2:
  c->mov64(t6, t1);                                 // or t6, t1, r0
  c->mov64(t7, t5);                                 // or t7, t5, r0

  block_3:
  // nop                                            // sll r0, r0, 0
  c->lhu(ra, 0, t7);                                // lhu ra, 0(t7)
  // nop                                            // sll r0, r0, 0
  c->lhu(t9, 2, t7);                                // lhu t9, 2(t7)
  // nop                                            // sll r0, r0, 0
  c->lw(t8, 64, a0);                                // lw t8, 64(a0)
  c->daddu(ra, ra, t9);                             // daddu ra, ra, t9
  c->daddiu(t9, t9, 1);                             // daddiu t9, t9, 1
  c->daddu(t8, t8, ra);                             // daddu t8, t8, ra
  c->sh(t9, 2, t7);                                 // sh t9, 2(t7)
  // nop                                            // sll r0, r0, 0
  c->sb(a2, 0, t8);                                 // sb a2, 0(t8)
  c->daddiu(t6, t6, -1);                            // daddiu t6, t6, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t6) != 0;                          // bne t6, r0, L51
  c->daddu(t7, t7, v1);                             // daddu t7, t7, v1
  if (bc) {goto block_3;}                           // branch non-likely

  c->daddiu(t4, t4, -1);                            // daddiu t4, t4, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t4) != 0;                          // bne t4, r0, L50
  c->daddu(t5, t5, a1);                             // daddu t5, t5, a1
  if (bc) {goto block_2;}                           // branch non-likely

  c->daddiu(t3, t3, -1);                            // daddiu t3, t3, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L49
  c->daddu(t0, t0, a3);                             // daddu t0, t0, a3
  if (bc) {goto block_1;}                           // branch non-likely

  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 48);                            // daddiu sp, sp, 48
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.light_hash_work = intern_from_c("*light-hash-work*").c();
  gLinkedFunctionTable.reg("light-hash-add-items", execute, 64);
}

} // namespace light_hash_add_items
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace light_hash_count_items {
struct Cache {
  void* light_hash_work; // *light-hash-work*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->daddiu(sp, sp, -48);                           // daddiu sp, sp, -48
  c->daddiu(v1, sp, 16);                            // daddiu v1, sp, 16
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 16, a1);                             // lqc2 vf2, 16(a1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf1, 12, a0);                             // lqc2 vf1, 12(a0)
  c->vadd_bc(DEST::xyzw, BC::w, vf3, vf2, vf2);     // vaddw.xyzw vf3, vf2, vf2
  c->lqc2(vf4, 28, a0);                             // lqc2 vf4, 28(a0)
  c->vsub_bc(DEST::xyzw, BC::w, vf2, vf2, vf2);     // vsubw.xyzw vf2, vf2, vf2
  c->load_symbol2(a2, cache.light_hash_work);       // lw a2, *light-hash-work*(s7)
  c->vsub(DEST::xyzw, vf2, vf2, vf1);               // vsub.xyzw vf2, vf2, vf1
  c->lq(a1, 44, a0);                                // lq a1, 44(a0)
  c->vsub(DEST::xyzw, vf3, vf3, vf1);               // vsub.xyzw vf3, vf3, vf1
  c->lq(a2, 0, a2);                                 // lq a2, 0(a2)
  c->vmul(DEST::xyzw, vf2, vf2, vf4);               // vmul.xyzw vf2, vf2, vf4
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf3, vf3, vf4);               // vmul.xyzw vf3, vf3, vf4
  c->dsubu(a1, a1, a2);                             // dsubu a1, a1, a2
  c->vftoi0(DEST::xyzw, vf2, vf2);                  // vftoi0.xyzw vf2, vf2
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf3, vf3);                  // vftoi0.xyzw vf3, vf3
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a3, vf2);                        // qmfc2.i a3, vf2
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a2, vf3);                        // qmfc2.i a2, vf3
  c->pmaxw(a3, a3, r0);                             // pmaxw a3, a3, r0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaxw(a2, a2, r0);                             // pmaxw a2, a2, r0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pminw(a3, a3, a1);                             // pminw a3, a3, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pminw(a1, a2, a1);                             // pminw a1, a2, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 0, v1);                                 // sq a3, 0(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(a1, 16, v1);                                // sq a1, 16(v1)
  c->addiu(a1, r0, 4);                              // addiu a1, r0, 4
  c->lw(a2, 44, a0);                                // lw a2, 44(a0)
  c->mult3(a2, a2, a1);                             // mult3 a2, a2, a1
  c->lw(a3, 52, a0);                                // lw a3, 52(a0)
  c->mult3(a3, a3, a2);                             // mult3 a3, a3, a2
  c->addiu(t0, r0, 1);                              // addiu t0, r0, 1
  c->lw(t1, 0, v1);                                 // lw t1, 0(v1)
  c->dsubu(t0, t0, t1);                             // dsubu t0, t0, t1
  c->lw(t1, 16, v1);                                // lw t1, 16(v1)
  c->daddu(t0, t0, t1);                             // daddu t0, t0, t1
  c->addiu(t1, r0, 1);                              // addiu t1, r0, 1
  c->lw(t2, 4, v1);                                 // lw t2, 4(v1)
  c->dsubu(t1, t1, t2);                             // dsubu t1, t1, t2
  c->lw(t2, 20, v1);                                // lw t2, 20(v1)
  c->daddu(t2, t1, t2);                             // daddu t2, t1, t2
  c->addiu(t1, r0, 1);                              // addiu t1, r0, 1
  c->lw(t3, 8, v1);                                 // lw t3, 8(v1)
  c->dsubu(t1, t1, t3);                             // dsubu t1, t1, t3
  c->lw(t3, 24, v1);                                // lw t3, 24(v1)
  c->daddu(t1, t1, t3);                             // daddu t1, t1, t3
  c->lwu(a0, 60, a0);                               // lwu a0, 60(a0)
  c->lw(t3, 0, v1);                                 // lw t3, 0(v1)
  c->mult3(t3, t3, a1);                             // mult3 t3, t3, a1
  c->lw(t4, 4, v1);                                 // lw t4, 4(v1)
  c->mult3(t4, t4, a3);                             // mult3 t4, t4, a3
  c->daddu(t3, t3, t4);                             // daddu t3, t3, t4
  c->lw(v1, 8, v1);                                 // lw v1, 8(v1)
  c->mult3(v1, v1, a2);                             // mult3 v1, v1, a2
  c->daddu(v1, t3, v1);                             // daddu v1, t3, v1
  c->daddu(v1, a0, v1);                             // daddu v1, a0, v1
  c->mov64(a0, t2);                                 // or a0, t2, r0
  // nop                                            // sll r0, r0, 0

  block_1:
  c->mov64(t2, t1);                                 // or t2, t1, r0
  c->mov64(t3, v1);                                 // or t3, v1, r0

  block_2:
  c->mov64(t4, t0);                                 // or t4, t0, r0
  c->mov64(t5, t3);                                 // or t5, t3, r0

  block_3:
  // nop                                            // sll r0, r0, 0
  c->lhu(t6, 2, t5);                                // lhu t6, 2(t5)
  // nop                                            // sll r0, r0, 0
  c->daddiu(t6, t6, 1);                             // daddiu t6, t6, 1
  // nop                                            // sll r0, r0, 0
  c->sh(t6, 2, t5);                                 // sh t6, 2(t5)
  c->daddiu(t4, t4, -1);                            // daddiu t4, t4, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t4) != 0;                          // bne t4, r0, L55
  c->daddu(t5, t5, a1);                             // daddu t5, t5, a1
  if (bc) {goto block_3;}                           // branch non-likely

  c->daddiu(t2, t2, -1);                            // daddiu t2, t2, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t2) != 0;                          // bne t2, r0, L54
  c->daddu(t3, t3, a2);                             // daddu t3, t3, a2
  if (bc) {goto block_2;}                           // branch non-likely

  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L53
  c->daddu(v1, v1, a3);                             // daddu v1, v1, a3
  if (bc) {goto block_1;}                           // branch non-likely

  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 48);                            // daddiu sp, sp, 48
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.light_hash_work = intern_from_c("*light-hash-work*").c();
  gLinkedFunctionTable.reg("light-hash-count-items", execute, 128);
}

} // namespace light_hash_count_items
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace light_hash_get_bucket_index {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->daddiu(sp, sp, -32);                           // daddiu sp, sp, -32
  c->daddiu(v1, sp, 16);                            // daddiu v1, sp, 16
  // nop                                            // sll r0, r0, 0
  c->pceqw(a2, r0, r0);                             // pceqw a2, r0, r0
  c->lqc2(vf2, 0, a1);                              // lqc2 vf2, 0(a1)
  c->lqc2(vf1, 12, a0);                             // lqc2 vf1, 12(a0)
  c->lqc2(vf3, 28, a0);                             // lqc2 vf3, 28(a0)
  c->vsub(DEST::xyzw, vf2, vf2, vf1);               // vsub.xyzw vf2, vf2, vf1
  c->lq(a3, 44, a0);                                // lq a3, 44(a0)
  c->vmul(DEST::xyzw, vf2, vf2, vf3);               // vmul.xyzw vf2, vf2, vf3
  c->vftoi0(DEST::xyzw, vf2, vf2);                  // vftoi0.xyzw vf2, vf2
  c->mov128_gpr_vf(a1, vf2);                        // qmfc2.i a1, vf2
  c->paddw(a3, a3, a2);                             // paddw a3, a3, a2
  c->pcgtw(a2, r0, a1);                             // pcgtw a2, r0, a1
  c->pcgtw(a3, a1, a3);                             // pcgtw a3, a1, a3
  c->ppach(a2, r0, a2);                             // ppach a2, r0, a2
  c->ppach(a3, r0, a3);                             // ppach a3, r0, a3
  c->sq(a1, 0, v1);                                 // sq a1, 0(v1)
  c->or_(a1, a2, a3);                               // or a1, a2, a3
  c->dsll(a1, a1, 16);                              // dsll a1, a1, 16
  bc = c->sgpr64(a1) != 0;                          // bne a1, r0, L13
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  c->lw(a1, 0, v1);                                 // lw a1, 0(v1)
  c->lw(a2, 4, v1);                                 // lw a2, 4(v1)
  c->lbu(a3, 6, a0);                                // lbu a3, 6(a0)
  c->mult3(a2, a2, a3);                             // mult3 a2, a2, a3
  c->daddu(a1, a1, a2);                             // daddu a1, a1, a2
  c->lw(v1, 8, v1);                                 // lw v1, 8(v1)
  c->lbu(a0, 7, a0);                                // lbu a0, 7(a0)
  c->mult3(v1, v1, a0);                             // mult3 v1, v1, a0
  c->daddu(v0, a1, v1);                             // daddu v0, a1, v1
  //beq r0, r0, L14                                 // beq r0, r0, L14
  // nop                                            // sll r0, r0, 0
  goto block_3;                                     // branch always


  block_2:
  c->addiu(v0, r0, -1);                             // addiu v0, r0, -1

  block_3:
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
  gLinkedFunctionTable.reg("light-hash-get-bucket-index", execute, 128);
}

} // namespace light_hash_get_bucket_index
} // namespace Mips2C
