//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace method_21_cloth_system {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -80);                           // daddiu sp, sp, -80
  c->daddiu(a3, sp, 16);                            // daddiu a3, sp, 16
  c->daddiu(t0, sp, 32);                            // daddiu t0, sp, 32
  c->lwu(v1, 68, a0);                               // lwu v1, 68(a0)
  c->lw(v1, 0, v1);                                 // lw v1, 0(v1)
  c->lw(a1, 96, a0);                                // lw a1, 96(a0)
  c->daddiu(a1, a1, -1);                            // daddiu a1, a1, -1
  c->lw(a2, 100, a0);                               // lw a2, 100(a0)
  c->daddiu(a2, a2, -1);                            // daddiu a2, a2, -1
  c->addiu(t1, r0, 0);                              // addiu t1, r0, 0
  c->addiu(t1, r0, 0);                              // addiu t1, r0, 0
  c->mov64(t1, a3);                                 // or t1, a3, r0
  c->lui(t2, 16000);                                // lui t2, 16000
  c->mtc1(f0, t2);                                  // mtc1 f0, t2
  c->swc1(f0, 0, t1);                               // swc1 f0, 0(t1)
  c->lui(t2, 16000);                                // lui t2, 16000
  c->mtc1(f0, t2);                                  // mtc1 f0, t2
  c->swc1(f0, 4, t1);                               // swc1 f0, 4(t1)
  c->lui(t2, 16000);                                // lui t2, 16000
  c->mtc1(f0, t2);                                  // mtc1 f0, t2
  c->swc1(f0, 8, t1);                               // swc1 f0, 8(t1)
  c->lui(t2, 16000);                                // lui t2, 16000
  c->mtc1(f0, t2);                                  // mtc1 f0, t2
  c->swc1(f0, 12, t1);                              // swc1 f0, 12(t1)
  c->mov64(t1, t0);                                 // or t1, t0, r0
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 0, t1);                               // swc1 f0, 0(t1)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 4, t1);                               // swc1 f0, 4(t1)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 8, t1);                               // swc1 f0, 8(t1)
  c->lwc1(f0, 144, a0);                             // lwc1 f0, 144(a0)
  c->swc1(f0, 12, t1);                              // swc1 f0, 12(t1)
  c->vmax_bc(DEST::xyzw, BC::w, vf1, vf0, vf0);     // vmaxw.xyzw vf1, vf0, vf0
  c->lqc2(vf10, 0, a3);                             // lqc2 vf10, 0(a3)
  c->lqc2(vf15, 0, t0);                             // lqc2 vf15, 0(t0)
  c->sd(r0, 48, sp);                                // sd r0, 48(sp)
  c->addiu(a3, r0, 0);                              // addiu a3, r0, 0
  //beq r0, r0, L187                                // beq r0, r0, L187
  // nop                                            // sll r0, r0, 0
  goto block_10;                                    // branch always


block_1:
  c->addiu(t0, r0, 0);                              // addiu t0, r0, 0
  //beq r0, r0, L186                                // beq r0, r0, L186
  // nop                                            // sll r0, r0, 0
  goto block_8;                                     // branch always


block_2:
  c->addiu(t1, r0, 48);                             // addiu t1, r0, 48
  c->ld(t2, 48, sp);                                // ld t2, 48(sp)
  c->mult3(t1, t1, t2);                             // mult3 t1, t1, t2
  c->daddiu(t1, t1, 12);                            // daddiu t1, t1, 12
  c->lwu(t2, 0, a0);                                // lwu t2, 0(a0)
  c->daddu(t1, t1, t2);                             // daddu t1, t1, t2
  c->sw(t1, 56, sp);                                // sw t1, 56(sp)
  c->addiu(t1, r0, 48);                             // addiu t1, r0, 48
  c->ld(t2, 48, sp);                                // ld t2, 48(sp)
  c->daddiu(t2, t2, 1);                             // daddiu t2, t2, 1
  c->mult3(t1, t1, t2);                             // mult3 t1, t1, t2
  c->daddiu(t1, t1, 12);                            // daddiu t1, t1, 12
  c->lwu(t2, 0, a0);                                // lwu t2, 0(a0)
  c->daddu(t1, t1, t2);                             // daddu t1, t1, t2
  c->sw(t1, 60, sp);                                // sw t1, 60(sp)
  c->addiu(t1, r0, 48);                             // addiu t1, r0, 48
  c->ld(t2, 48, sp);                                // ld t2, 48(sp)
  c->lw(t3, 96, a0);                                // lw t3, 96(a0)
  c->daddu(t2, t2, t3);                             // daddu t2, t2, t3
  c->mult3(t1, t1, t2);                             // mult3 t1, t1, t2
  c->daddiu(t1, t1, 12);                            // daddiu t1, t1, 12
  c->lwu(t2, 0, a0);                                // lwu t2, 0(a0)
  c->daddu(t1, t1, t2);                             // daddu t1, t1, t2
  c->sw(t1, 64, sp);                                // sw t1, 64(sp)
  c->addiu(t1, r0, 48);                             // addiu t1, r0, 48
  c->lw(t2, 96, a0);                                // lw t2, 96(a0)
  c->daddiu(t2, t2, 1);                             // daddiu t2, t2, 1
  c->ld(t3, 48, sp);                                // ld t3, 48(sp)
  c->daddu(t2, t2, t3);                             // daddu t2, t2, t3
  c->mult3(t1, t1, t2);                             // mult3 t1, t1, t2
  c->daddiu(t1, t1, 12);                            // daddiu t1, t1, 12
  c->lwu(t2, 0, a0);                                // lwu t2, 0(a0)
  c->daddu(t1, t1, t2);                             // daddu t1, t1, t2
  c->sw(t1, 68, sp);                                // sw t1, 68(sp)
  c->lwu(t1, 56, sp);                               // lwu t1, 56(sp)
  c->lqc2(vf2, 0, t1);                              // lqc2 vf2, 0(t1)
  c->lwu(t1, 68, sp);                               // lwu t1, 68(sp)
  c->lqc2(vf5, 0, t1);                              // lqc2 vf5, 0(t1)
  c->lwu(t1, 60, sp);                               // lwu t1, 60(sp)
  c->lqc2(vf3, 0, t1);                              // lqc2 vf3, 0(t1)
  c->lwu(t1, 64, sp);                               // lwu t1, 64(sp)
  c->lqc2(vf4, 0, t1);                              // lqc2 vf4, 0(t1)
  c->lwu(t1, 56, sp);                               // lwu t1, 56(sp)
  c->lqc2(vf20, 32, t1);                            // lqc2 vf20, 32(t1)
  c->lwu(t1, 68, sp);                               // lwu t1, 68(sp)
  c->lqc2(vf23, 32, t1);                            // lqc2 vf23, 32(t1)
  c->lwu(t1, 60, sp);                               // lwu t1, 60(sp)
  c->lqc2(vf21, 32, t1);                            // lqc2 vf21, 32(t1)
  c->lwu(t1, 64, sp);                               // lwu t1, 64(sp)
  c->lqc2(vf22, 32, t1);                            // lqc2 vf22, 32(t1)
  c->vmula_bc(DEST::xyzw, BC::x, vf2, vf20);        // vmulax.xyzw acc, vf2, vf20
  c->vmadda_bc(DEST::xyzw, BC::x, vf3, vf21);       // vmaddax.xyzw acc, vf3, vf21
  c->vmadda_bc(DEST::xyzw, BC::x, vf5, vf23);       // vmaddax.xyzw acc, vf5, vf23
  c->vmadd_bc(DEST::xyzw, BC::x, vf8, vf4, vf22);   // vmaddx.xyzw vf8, vf4, vf22
  c->vmul_bc(DEST::xyzw, BC::x, vf8, vf8, vf10);    // vmulx.xyzw vf8, vf8, vf10
  c->lwu(t1, 68, a0);                               // lwu t1, 68(a0)
  c->daddiu(t1, t1, 12);                            // daddiu t1, t1, 12
  c->addiu(t2, r0, 0);                              // addiu t2, r0, 0
  //beq r0, r0, L185                                // beq r0, r0, L185
  // nop                                            // sll r0, r0, 0
  goto block_6;                                     // branch always


block_3:
  c->lqc2(vf11, 0, t1);                             // lqc2 vf11, 0(t1)
  c->vsub(DEST::xyzw, vf13, vf8, vf11);             // vsub.xyzw vf13, vf8, vf11
  c->vmul(DEST::xyzw, vf9, vf13, vf13);             // vmul.xyzw vf9, vf13, vf13
  c->vmula_bc(DEST::xyzw, BC::x, vf1, vf9);         // vmulax.xyzw acc, vf1, vf9
  c->vmadda_bc(DEST::xyzw, BC::y, vf1, vf9);        // vmadday.xyzw acc, vf1, vf9
  c->vmadd_bc(DEST::xyzw, BC::z, vf14, vf1, vf9);   // vmaddz.xyzw vf14, vf1, vf9
  c->vadd_bc(DEST::w, BC::w, vf11, vf11, vf15);     // vaddw.w vf11, vf11, vf15
  c->vmul(DEST::xyzw, vf12, vf11, vf11);            // vmul.xyzw vf12, vf11, vf11
  c->vsub_bc(DEST::xyzw, BC::w, vf9, vf14, vf12);   // vsubw.xyzw vf9, vf14, vf12
  c->mov128_gpr_vf(t3, vf9);                        // qmfc2.i t3, vf9
  bc = ((s64)c->sgpr64(t3)) >= 0;                   // bgez t3, L184
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_5;}                           // branch non-likely

  c->vrsqrt(vf11, BC::w, vf14, BC::x);              // vrsqrt Q, vf11.w, vf14.x
  c->vmax_bc(DEST::xyz, BC::x, vf20, vf0, vf20);    // vmaxx.xyz vf20, vf0, vf20
  c->vmax_bc(DEST::xyz, BC::x, vf21, vf0, vf21);    // vmaxx.xyz vf21, vf0, vf21
  c->vmax_bc(DEST::xyz, BC::x, vf22, vf0, vf22);    // vmaxx.xyz vf22, vf0, vf22
  c->vmax_bc(DEST::xyz, BC::x, vf23, vf0, vf23);    // vmaxx.xyz vf23, vf0, vf23
  c->vwaitq();                                      // vwaitq
  // Unknown instr: vmulaq.xyzw acc, vf13, Q
  c->vmula_q(DEST::xyzw, vf13);
  c->vmsuba(DEST::xyzw,  vf1, vf13);                // vmsuba.xyzw acc, vf1, vf13
  c->vmadd(DEST::xyzw, vf2, vf1, vf2);              // vmadd.xyzw vf2, vf1, vf2
  c->vmadd(DEST::xyzw, vf3, vf1, vf3);              // vmadd.xyzw vf3, vf1, vf3
  c->vmadd(DEST::xyzw, vf5, vf1, vf5);              // vmadd.xyzw vf5, vf1, vf5
  c->vmadd(DEST::xyzw, vf4, vf1, vf4);              // vmadd.xyzw vf4, vf1, vf4
  c->vmadd(DEST::xyzw, vf8, vf1, vf8);              // vmadd.xyzw vf8, vf1, vf8

block_5:
  c->daddiu(t1, t1, 16);                            // daddiu t1, t1, 16
  c->daddiu(t2, t2, 1);                             // daddiu t2, t2, 1

block_6:
  c->slt(t3, t2, v1);                               // slt t3, t2, v1
  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L183
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_3;}                           // branch non-likely

  c->mov64(t1, s7);                                 // or t1, s7, r0
  c->mov64(t1, s7);                                 // or t1, s7, r0
  c->lwu(t1, 56, sp);                               // lwu t1, 56(sp)
  c->sqc2(vf2, 0, t1);                              // sqc2 vf2, 0(t1)
  c->lwu(t1, 68, sp);                               // lwu t1, 68(sp)
  c->sqc2(vf5, 0, t1);                              // sqc2 vf5, 0(t1)
  c->lwu(t1, 60, sp);                               // lwu t1, 60(sp)
  c->sqc2(vf3, 0, t1);                              // sqc2 vf3, 0(t1)
  c->lwu(t1, 64, sp);                               // lwu t1, 64(sp)
  c->sqc2(vf4, 0, t1);                              // sqc2 vf4, 0(t1)
  c->mov128_gpr_vf(t1, vf4);                        // qmfc2.i t1, vf4
  c->ld(t1, 48, sp);                                // ld t1, 48(sp)
  c->daddiu(t1, t1, 1);                             // daddiu t1, t1, 1
  c->sd(t1, 48, sp);                                // sd t1, 48(sp)
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1

block_8:
  c->slt(t1, t0, a1);                               // slt t1, t0, a1
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L182
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  c->mov64(t0, s7);                                 // or t0, s7, r0
  c->mov64(t0, s7);                                 // or t0, s7, r0
  c->ld(t0, 48, sp);                                // ld t0, 48(sp)
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  c->sd(t0, 48, sp);                                // sd t0, 48(sp)
  c->daddiu(a3, a3, 1);                             // daddiu a3, a3, 1

block_10:
  c->slt(t0, a3, a2);                               // slt t0, a3, a2
  bc = c->sgpr64(t0) != 0;                          // bne t0, r0, L181
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v0, s7);                                 // or v0, s7, r0
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 80);                            // daddiu sp, sp, 80
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  gLinkedFunctionTable.reg("(method 21 cloth-system)", execute, 256);
}

} // namespace method_21_cloth_system
} // namespace Mips2C

