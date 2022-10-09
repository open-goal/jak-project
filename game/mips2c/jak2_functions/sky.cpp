#include "sky.h"

#include "game/kernel/jak2/kscheme.h"
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak2 {

ExecutionContext sky_regs_vfs;

namespace set_sky_vf27 {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  // sky_regs_vfs.vfs[27]
  memcpy(&sky_regs_vfs.vfs[27].f[0], g_ee_main_mem + c->gpr_addr(a0), 16);
  return 0;
}

void link() {
  gLinkedFunctionTable.reg("set-sky-vf27", execute, 64);
}

}  // namespace set_sky_vf27

}  // namespace Mips2C::jak2

//--------------------------MIPS2C---------------------
// clang-format off
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace clip_polygon_against_positive_hyperplane {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  bool cop1_bc = false;
  // nop                                            // sll r0, r0, 0
  c->mov64(t1, t0);                                 // or t1, t0, r0
  c->addiu(t0, r0, 0);                              // addiu t0, r0, 0
  c->lwc1(f1, 0, t2);                               // lwc1 f1, 0(t2)
  c->daddiu(t2, t2, 48);                            // daddiu t2, t2, 48
  c->lwc1(f0, 12, a2);                              // lwc1 f0, 12(a2)
  c->mov64(t3, a3);                                 // or t3, a3, r0
  c->lqc2(vf1, 0, a2);                              // lqc2 vf1, 0(a2)
  c->daddiu(a2, a2, 48);                            // daddiu a2, a2, 48
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  c->lqc2(vf2, -32, a2);                            // lqc2 vf2, -32(a2)
  bc = !cop1_bc;                                    // bc1f L112
  c->lqc2(vf3, -16, a2);                            // lqc2 vf3, -16(a2)
  if (bc) {goto block_9;}                           // branch non-likely


  block_1:
  c->lwc1(f3, 0, t2);                               // lwc1 f3, 0(t2)
  c->daddiu(t2, t2, 48);                            // daddiu t2, t2, 48
  c->lwc1(f2, 12, a2);                              // lwc1 f2, 12(a2)
  c->daddiu(t1, t1, -1);                            // daddiu t1, t1, -1
  c->lqc2(vf4, 0, a2);                              // lqc2 vf4, 0(a2)
  c->daddiu(a2, a2, 48);                            // daddiu a2, a2, 48
  cop1_bc = c->fprs[f2] < c->fprs[f3];              // c.lt.s f2, f3
  c->lqc2(vf5, -32, a2);                            // lqc2 vf5, -32(a2)
  bc = !cop1_bc;                                    // bc1f L110
  c->lqc2(vf6, -16, a2);                            // lqc2 vf6, -16(a2)
  if (bc) {goto block_6;}                           // branch non-likely

  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L116
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_17;}                          // branch non-likely


  block_3:
  c->lwc1(f1, 0, t2);                               // lwc1 f1, 0(t2)
  c->daddiu(t2, t2, 48);                            // daddiu t2, t2, 48
  c->lwc1(f0, 12, a2);                              // lwc1 f0, 12(a2)
  c->daddiu(t1, t1, -1);                            // daddiu t1, t1, -1
  c->lqc2(vf1, 0, a2);                              // lqc2 vf1, 0(a2)
  c->daddiu(a2, a2, 48);                            // daddiu a2, a2, 48
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  c->lqc2(vf2, -32, a2);                            // lqc2 vf2, -32(a2)
  bc = !cop1_bc;                                    // bc1f L111
  c->lqc2(vf3, -16, a2);                            // lqc2 vf3, -16(a2)
  if (bc) {goto block_8;}                           // branch non-likely

  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L108
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  //beq r0, r0, L116                                // beq r0, r0, L116
  // nop                                            // sll r0, r0, 0
  goto block_17;                                    // branch always


  block_6:
  c->subs(f7, f0, f1);                              // sub.s f7, f0, f1
  c->vsub(DEST::xyzw, vf7, vf4, vf1);               // vsub.xyzw vf7, vf4, vf1
  c->subs(f8, f2, f3);                              // sub.s f8, f2, f3
  c->vsub(DEST::xyzw, vf8, vf5, vf2);               // vsub.xyzw vf8, vf5, vf2
  c->subs(f5, f7, f8);                              // sub.s f5, f7, f8
  c->vsub(DEST::xyzw, vf9, vf6, vf3);               // vsub.xyzw vf9, vf6, vf3
  c->divs(f6, f7, f5);                              // div.s f6, f7, f5
  c->daddiu(a3, a3, 48);                            // daddiu a3, a3, 48
  c->mfc1(v1, f6);                                  // mfc1 v1, f6
  c->mov128_vf_gpr(vf10, v1);                       // qmtc2.i vf10, v1
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  c->vmula_bc(DEST::xyzw, BC::w, vf1, vf0);         // vmulaw.xyzw acc, vf1, vf0
  c->daddiu(t6, s7, 4);                             // daddiu t6, s7, 4
  c->vmadd_bc(DEST::xyzw, BC::x, vf7, vf7, vf10);   // vmaddx.xyzw vf7, vf7, vf10
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf2, vf0);         // vmulaw.xyzw acc, vf2, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf8, vf8, vf10);   // vmaddx.xyzw vf8, vf8, vf10
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf9, vf10);   // vmaddx.xyzw vf9, vf9, vf10
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf7, -48, a3);                            // sqc2 vf7, -48(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, -32, a3);                            // sqc2 vf8, -32(a3)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L113
  c->sqc2(vf9, -16, a3);                            // sqc2 vf9, -16(a3)
  if (bc) {goto block_11;}                          // branch non-likely

  //beq r0, r0, L116                                // beq r0, r0, L116
  // nop                                            // sll r0, r0, 0
  goto block_17;                                    // branch always


  block_8:
  c->subs(f7, f0, f1);                              // sub.s f7, f0, f1
  c->vsub(DEST::xyzw, vf7, vf1, vf4);               // vsub.xyzw vf7, vf1, vf4
  c->subs(f8, f2, f3);                              // sub.s f8, f2, f3
  c->vsub(DEST::xyzw, vf8, vf2, vf5);               // vsub.xyzw vf8, vf2, vf5
  c->subs(f5, f8, f7);                              // sub.s f5, f8, f7
  c->vsub(DEST::xyzw, vf9, vf3, vf6);               // vsub.xyzw vf9, vf3, vf6
  c->divs(f6, f8, f5);                              // div.s f6, f8, f5
  c->daddiu(a3, a3, 48);                            // daddiu a3, a3, 48
  c->mfc1(v1, f6);                                  // mfc1 v1, f6
  c->mov128_vf_gpr(vf10, v1);                       // qmtc2.i vf10, v1
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  c->daddiu(t6, s7, 4);                             // daddiu t6, s7, 4
  c->vmadd_bc(DEST::xyzw, BC::x, vf7, vf7, vf10);   // vmaddx.xyzw vf7, vf7, vf10
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf5, vf0);         // vmulaw.xyzw acc, vf5, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf8, vf8, vf10);   // vmaddx.xyzw vf8, vf8, vf10
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf6, vf0);         // vmulaw.xyzw acc, vf6, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf9, vf10);   // vmaddx.xyzw vf9, vf9, vf10
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf7, -48, a3);                            // sqc2 vf7, -48(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, -32, a3);                            // sqc2 vf8, -32(a3)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L116
  c->sqc2(vf9, -16, a3);                            // sqc2 vf9, -16(a3)
  if (bc) {goto block_17;}                          // branch non-likely


  block_9:
  c->lwc1(f3, 0, t2);                               // lwc1 f3, 0(t2)
  c->daddiu(t2, t2, 48);                            // daddiu t2, t2, 48
  c->lwc1(f2, 12, a2);                              // lwc1 f2, 12(a2)
  c->daddiu(t1, t1, -1);                            // daddiu t1, t1, -1
  c->lqc2(vf4, 0, a2);                              // lqc2 vf4, 0(a2)
  c->daddiu(a2, a2, 48);                            // daddiu a2, a2, 48
  cop1_bc = c->fprs[f2] < c->fprs[f3];              // c.lt.s f2, f3
  c->lqc2(vf5, -32, a2);                            // lqc2 vf5, -32(a2)
  bc = cop1_bc;                                     // bc1t L114
  c->lqc2(vf6, -16, a2);                            // lqc2 vf6, -16(a2)
  if (bc) {goto block_14;}                          // branch non-likely

  c->sqc2(vf1, 0, a3);                              // sqc2 vf1, 0(a3)
  c->daddiu(a3, a3, 48);                            // daddiu a3, a3, 48
  c->sqc2(vf2, -32, a3);                            // sqc2 vf2, -32(a3)
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L116
  c->sqc2(vf3, -16, a3);                            // sqc2 vf3, -16(a3)
  if (bc) {goto block_17;}                          // branch non-likely


  block_11:
  c->lwc1(f1, 0, t2);                               // lwc1 f1, 0(t2)
  c->daddiu(t2, t2, 48);                            // daddiu t2, t2, 48
  c->lwc1(f0, 12, a2);                              // lwc1 f0, 12(a2)
  c->daddiu(t1, t1, -1);                            // daddiu t1, t1, -1
  c->lqc2(vf1, 0, a2);                              // lqc2 vf1, 0(a2)
  c->daddiu(a2, a2, 48);                            // daddiu a2, a2, 48
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  c->lqc2(vf2, -32, a2);                            // lqc2 vf2, -32(a2)
  bc = cop1_bc;                                     // bc1t L115
  c->lqc2(vf3, -16, a2);                            // lqc2 vf3, -16(a2)
  if (bc) {goto block_16;}                          // branch non-likely

  c->sqc2(vf4, 0, a3);                              // sqc2 vf4, 0(a3)
  c->daddiu(a3, a3, 48);                            // daddiu a3, a3, 48
  c->sqc2(vf5, -32, a3);                            // sqc2 vf5, -32(a3)
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L112
  c->sqc2(vf6, -16, a3);                            // sqc2 vf6, -16(a3)
  if (bc) {goto block_9;}                           // branch non-likely

  //beq r0, r0, L116                                // beq r0, r0, L116
  // nop                                            // sll r0, r0, 0
  goto block_17;                                    // branch always


  block_14:
  c->subs(f7, f0, f1);                              // sub.s f7, f0, f1
  c->vsub(DEST::xyzw, vf7, vf1, vf4);               // vsub.xyzw vf7, vf1, vf4
  c->subs(f8, f2, f3);                              // sub.s f8, f2, f3
  c->vsub(DEST::xyzw, vf8, vf2, vf5);               // vsub.xyzw vf8, vf2, vf5
  c->subs(f5, f8, f7);                              // sub.s f5, f8, f7
  c->vsub(DEST::xyzw, vf9, vf3, vf6);               // vsub.xyzw vf9, vf3, vf6
  c->divs(f6, f8, f5);                              // div.s f6, f8, f5
  c->daddiu(a3, a3, 96);                            // daddiu a3, a3, 96
  c->mfc1(v1, f6);                                  // mfc1 v1, f6
  c->mov128_vf_gpr(vf10, v1);                       // qmtc2.i vf10, v1
  c->sqc2(vf1, -96, a3);                            // sqc2 vf1, -96(a3)
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  c->sqc2(vf2, -80, a3);                            // sqc2 vf2, -80(a3)
  c->vmadd_bc(DEST::xyzw, BC::x, vf7, vf7, vf10);   // vmaddx.xyzw vf7, vf7, vf10
  c->sqc2(vf3, -64, a3);                            // sqc2 vf3, -64(a3)
  c->vmula_bc(DEST::xyzw, BC::w, vf5, vf0);         // vmulaw.xyzw acc, vf5, vf0
  c->daddiu(t0, t0, 2);                             // daddiu t0, t0, 2
  c->vmadd_bc(DEST::xyzw, BC::x, vf8, vf8, vf10);   // vmaddx.xyzw vf8, vf8, vf10
  c->daddiu(t6, s7, 4);                             // daddiu t6, s7, 4
  c->vmula_bc(DEST::xyzw, BC::w, vf6, vf0);         // vmulaw.xyzw acc, vf6, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf9, vf10);   // vmaddx.xyzw vf9, vf9, vf10
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf7, -48, a3);                            // sqc2 vf7, -48(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, -32, a3);                            // sqc2 vf8, -32(a3)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L109
  c->sqc2(vf9, -16, a3);                            // sqc2 vf9, -16(a3)
  if (bc) {goto block_3;}                           // branch non-likely

  //beq r0, r0, L116                                // beq r0, r0, L116
  // nop                                            // sll r0, r0, 0
  goto block_17;                                    // branch always


  block_16:
  c->subs(f7, f0, f1);                              // sub.s f7, f0, f1
  c->vsub(DEST::xyzw, vf7, vf4, vf1);               // vsub.xyzw vf7, vf4, vf1
  c->subs(f8, f2, f3);                              // sub.s f8, f2, f3
  c->vsub(DEST::xyzw, vf8, vf5, vf2);               // vsub.xyzw vf8, vf5, vf2
  c->subs(f5, f7, f8);                              // sub.s f5, f7, f8
  c->vsub(DEST::xyzw, vf9, vf6, vf3);               // vsub.xyzw vf9, vf6, vf3
  c->divs(f6, f7, f5);                              // div.s f6, f7, f5
  c->daddiu(a3, a3, 96);                            // daddiu a3, a3, 96
  c->mfc1(v1, f6);                                  // mfc1 v1, f6
  c->mov128_vf_gpr(vf10, v1);                       // qmtc2.i vf10, v1
  c->sqc2(vf4, -96, a3);                            // sqc2 vf4, -96(a3)
  c->vmula_bc(DEST::xyzw, BC::w, vf1, vf0);         // vmulaw.xyzw acc, vf1, vf0
  c->sqc2(vf5, -80, a3);                            // sqc2 vf5, -80(a3)
  c->vmadd_bc(DEST::xyzw, BC::x, vf7, vf7, vf10);   // vmaddx.xyzw vf7, vf7, vf10
  c->sqc2(vf6, -64, a3);                            // sqc2 vf6, -64(a3)
  c->vmula_bc(DEST::xyzw, BC::w, vf2, vf0);         // vmulaw.xyzw acc, vf2, vf0
  c->daddiu(t0, t0, 2);                             // daddiu t0, t0, 2
  c->vmadd_bc(DEST::xyzw, BC::x, vf8, vf8, vf10);   // vmaddx.xyzw vf8, vf8, vf10
  c->daddiu(t6, s7, 4);                             // daddiu t6, s7, 4
  c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf9, vf10);   // vmaddx.xyzw vf9, vf9, vf10
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf7, -48, a3);                            // sqc2 vf7, -48(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, -32, a3);                            // sqc2 vf8, -32(a3)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L108
  c->sqc2(vf9, -16, a3);                            // sqc2 vf9, -16(a3)
  if (bc) {goto block_1;}                           // branch non-likely


  block_17:
  c->lqc2(vf1, 0, t3);                              // lqc2 vf1, 0(t3)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 16, t3);                             // lqc2 vf2, 16(t3)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 32, t3);                             // lqc2 vf3, 32(t3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf1, 0, a3);                              // sqc2 vf1, 0(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf2, 16, a3);                             // sqc2 vf2, 16(a3)
  // nop                                            // sll r0, r0, 0
  //jr ra                                           // jr ra
  c->sqc2(vf3, 32, a3);                             // sqc2 vf3, 32(a3)
  goto end_of_function;                             // return

  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

} // namespace clip_polygon_against_positive_hyperplane
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace clip_polygon_against_negative_hyperplane {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  bool cop1_bc = false;
  // nop                                            // sll r0, r0, 0
  c->mov64(t1, t0);                                 // or t1, t0, r0
  c->addiu(t0, r0, 0);                              // addiu t0, r0, 0
  c->lwc1(f0, 12, a2);                              // lwc1 f0, 12(a2)
  c->mov64(t3, a3);                                 // or t3, a3, r0
  c->lwc1(f1, 0, t2);                               // lwc1 f1, 0(t2)
  c->daddiu(t2, t2, 48);                            // daddiu t2, t2, 48
  c->lqc2(vf1, 0, a2);                              // lqc2 vf1, 0(a2)
  c->negs(f0, f0);                                  // neg.s f0, f0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 16, a2);                             // lqc2 vf2, 16(a2)
  cop1_bc = c->fprs[f1] < c->fprs[f0];              // c.lt.s f1, f0
  c->daddiu(a2, a2, 48);                            // daddiu a2, a2, 48
  bc = !cop1_bc;                                    // bc1f L102
  c->lqc2(vf3, -16, a2);                            // lqc2 vf3, -16(a2)
  if (bc) {goto block_9;}                           // branch non-likely


  block_1:
  c->lwc1(f2, 12, a2);                              // lwc1 f2, 12(a2)
  c->daddiu(t1, t1, -1);                            // daddiu t1, t1, -1
  c->lwc1(f3, 0, t2);                               // lwc1 f3, 0(t2)
  c->daddiu(t2, t2, 48);                            // daddiu t2, t2, 48
  c->lqc2(vf4, 0, a2);                              // lqc2 vf4, 0(a2)
  c->negs(f2, f2);                                  // neg.s f2, f2
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf5, 16, a2);                             // lqc2 vf5, 16(a2)
  cop1_bc = c->fprs[f3] < c->fprs[f2];              // c.lt.s f3, f2
  c->daddiu(a2, a2, 48);                            // daddiu a2, a2, 48
  bc = !cop1_bc;                                    // bc1f L100
  c->lqc2(vf6, -16, a2);                            // lqc2 vf6, -16(a2)
  if (bc) {goto block_6;}                           // branch non-likely

  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L106
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_17;}                          // branch non-likely


  block_3:
  c->lwc1(f0, 12, a2);                              // lwc1 f0, 12(a2)
  c->daddiu(t1, t1, -1);                            // daddiu t1, t1, -1
  c->lwc1(f1, 0, t2);                               // lwc1 f1, 0(t2)
  c->daddiu(t2, t2, 48);                            // daddiu t2, t2, 48
  c->lqc2(vf1, 0, a2);                              // lqc2 vf1, 0(a2)
  c->negs(f0, f0);                                  // neg.s f0, f0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 16, a2);                             // lqc2 vf2, 16(a2)
  cop1_bc = c->fprs[f1] < c->fprs[f0];              // c.lt.s f1, f0
  c->daddiu(a2, a2, 48);                            // daddiu a2, a2, 48
  bc = !cop1_bc;                                    // bc1f L101
  c->lqc2(vf3, -16, a2);                            // lqc2 vf3, -16(a2)
  if (bc) {goto block_8;}                           // branch non-likely

  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L98
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  //beq r0, r0, L106                                // beq r0, r0, L106
  // nop                                            // sll r0, r0, 0
  goto block_17;                                    // branch always


  block_6:
  c->subs(f7, f0, f1);                              // sub.s f7, f0, f1
  c->vsub(DEST::xyzw, vf7, vf4, vf1);               // vsub.xyzw vf7, vf4, vf1
  c->subs(f8, f2, f3);                              // sub.s f8, f2, f3
  c->vsub(DEST::xyzw, vf8, vf5, vf2);               // vsub.xyzw vf8, vf5, vf2
  c->subs(f5, f7, f8);                              // sub.s f5, f7, f8
  c->vsub(DEST::xyzw, vf9, vf6, vf3);               // vsub.xyzw vf9, vf6, vf3
  c->divs(f6, f7, f5);                              // div.s f6, f7, f5
  c->daddiu(a3, a3, 48);                            // daddiu a3, a3, 48
  c->mfc1(v1, f6);                                  // mfc1 v1, f6
  c->mov128_vf_gpr(vf10, v1);                       // qmtc2.i vf10, v1
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  c->vmula_bc(DEST::xyzw, BC::w, vf1, vf0);         // vmulaw.xyzw acc, vf1, vf0
  c->daddiu(t6, s7, 4);                             // daddiu t6, s7, 4
  c->vmadd_bc(DEST::xyzw, BC::x, vf7, vf7, vf10);   // vmaddx.xyzw vf7, vf7, vf10
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf2, vf0);         // vmulaw.xyzw acc, vf2, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf8, vf8, vf10);   // vmaddx.xyzw vf8, vf8, vf10
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf9, vf10);   // vmaddx.xyzw vf9, vf9, vf10
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf7, -48, a3);                            // sqc2 vf7, -48(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, -32, a3);                            // sqc2 vf8, -32(a3)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L103
  c->sqc2(vf9, -16, a3);                            // sqc2 vf9, -16(a3)
  if (bc) {goto block_11;}                          // branch non-likely

  //beq r0, r0, L106                                // beq r0, r0, L106
  // nop                                            // sll r0, r0, 0
  goto block_17;                                    // branch always


  block_8:
  c->subs(f7, f0, f1);                              // sub.s f7, f0, f1
  c->vsub(DEST::xyzw, vf7, vf1, vf4);               // vsub.xyzw vf7, vf1, vf4
  c->subs(f8, f2, f3);                              // sub.s f8, f2, f3
  c->vsub(DEST::xyzw, vf8, vf2, vf5);               // vsub.xyzw vf8, vf2, vf5
  c->subs(f5, f8, f7);                              // sub.s f5, f8, f7
  c->vsub(DEST::xyzw, vf9, vf3, vf6);               // vsub.xyzw vf9, vf3, vf6
  c->divs(f6, f8, f5);                              // div.s f6, f8, f5
  c->daddiu(a3, a3, 48);                            // daddiu a3, a3, 48
  c->mfc1(v1, f6);                                  // mfc1 v1, f6
  c->mov128_vf_gpr(vf10, v1);                       // qmtc2.i vf10, v1
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  c->daddiu(t6, s7, 4);                             // daddiu t6, s7, 4
  c->vmadd_bc(DEST::xyzw, BC::x, vf7, vf7, vf10);   // vmaddx.xyzw vf7, vf7, vf10
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf5, vf0);         // vmulaw.xyzw acc, vf5, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf8, vf8, vf10);   // vmaddx.xyzw vf8, vf8, vf10
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf6, vf0);         // vmulaw.xyzw acc, vf6, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf9, vf10);   // vmaddx.xyzw vf9, vf9, vf10
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf7, -48, a3);                            // sqc2 vf7, -48(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, -32, a3);                            // sqc2 vf8, -32(a3)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L106
  c->sqc2(vf9, -16, a3);                            // sqc2 vf9, -16(a3)
  if (bc) {goto block_17;}                          // branch non-likely


  block_9:
  c->lwc1(f2, 12, a2);                              // lwc1 f2, 12(a2)
  c->daddiu(t1, t1, -1);                            // daddiu t1, t1, -1
  c->lwc1(f3, 0, t2);                               // lwc1 f3, 0(t2)
  c->daddiu(t2, t2, 48);                            // daddiu t2, t2, 48
  c->lqc2(vf4, 0, a2);                              // lqc2 vf4, 0(a2)
  c->negs(f2, f2);                                  // neg.s f2, f2
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf5, 16, a2);                             // lqc2 vf5, 16(a2)
  cop1_bc = c->fprs[f3] < c->fprs[f2];              // c.lt.s f3, f2
  c->daddiu(a2, a2, 48);                            // daddiu a2, a2, 48
  bc = cop1_bc;                                     // bc1t L104
  c->lqc2(vf6, -16, a2);                            // lqc2 vf6, -16(a2)
  if (bc) {goto block_14;}                          // branch non-likely

  c->sqc2(vf1, 0, a3);                              // sqc2 vf1, 0(a3)
  c->daddiu(a3, a3, 48);                            // daddiu a3, a3, 48
  c->sqc2(vf2, -32, a3);                            // sqc2 vf2, -32(a3)
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L106
  c->sqc2(vf3, -16, a3);                            // sqc2 vf3, -16(a3)
  if (bc) {goto block_17;}                          // branch non-likely


  block_11:
  c->lwc1(f0, 12, a2);                              // lwc1 f0, 12(a2)
  c->daddiu(t1, t1, -1);                            // daddiu t1, t1, -1
  c->lwc1(f1, 0, t2);                               // lwc1 f1, 0(t2)
  c->daddiu(t2, t2, 48);                            // daddiu t2, t2, 48
  c->lqc2(vf1, 0, a2);                              // lqc2 vf1, 0(a2)
  c->negs(f0, f0);                                  // neg.s f0, f0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 16, a2);                             // lqc2 vf2, 16(a2)
  cop1_bc = c->fprs[f1] < c->fprs[f0];              // c.lt.s f1, f0
  c->daddiu(a2, a2, 48);                            // daddiu a2, a2, 48
  bc = cop1_bc;                                     // bc1t L105
  c->lqc2(vf3, -16, a2);                            // lqc2 vf3, -16(a2)
  if (bc) {goto block_16;}                          // branch non-likely

  c->sqc2(vf4, 0, a3);                              // sqc2 vf4, 0(a3)
  c->daddiu(a3, a3, 48);                            // daddiu a3, a3, 48
  c->sqc2(vf5, -32, a3);                            // sqc2 vf5, -32(a3)
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L102
  c->sqc2(vf6, -16, a3);                            // sqc2 vf6, -16(a3)
  if (bc) {goto block_9;}                           // branch non-likely

  //beq r0, r0, L106                                // beq r0, r0, L106
  // nop                                            // sll r0, r0, 0
  goto block_17;                                    // branch always


  block_14:
  c->subs(f7, f0, f1);                              // sub.s f7, f0, f1
  c->vsub(DEST::xyzw, vf7, vf1, vf4);               // vsub.xyzw vf7, vf1, vf4
  c->subs(f8, f2, f3);                              // sub.s f8, f2, f3
  c->vsub(DEST::xyzw, vf8, vf2, vf5);               // vsub.xyzw vf8, vf2, vf5
  c->subs(f5, f8, f7);                              // sub.s f5, f8, f7
  c->vsub(DEST::xyzw, vf9, vf3, vf6);               // vsub.xyzw vf9, vf3, vf6
  c->divs(f6, f8, f5);                              // div.s f6, f8, f5
  c->daddiu(a3, a3, 96);                            // daddiu a3, a3, 96
  c->mfc1(v1, f6);                                  // mfc1 v1, f6
  c->mov128_vf_gpr(vf10, v1);                       // qmtc2.i vf10, v1
  c->sqc2(vf1, -96, a3);                            // sqc2 vf1, -96(a3)
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  c->sqc2(vf2, -80, a3);                            // sqc2 vf2, -80(a3)
  c->vmadd_bc(DEST::xyzw, BC::x, vf7, vf7, vf10);   // vmaddx.xyzw vf7, vf7, vf10
  c->sqc2(vf3, -64, a3);                            // sqc2 vf3, -64(a3)
  c->vmula_bc(DEST::xyzw, BC::w, vf5, vf0);         // vmulaw.xyzw acc, vf5, vf0
  c->daddiu(t0, t0, 2);                             // daddiu t0, t0, 2
  c->vmadd_bc(DEST::xyzw, BC::x, vf8, vf8, vf10);   // vmaddx.xyzw vf8, vf8, vf10
  c->daddiu(t6, s7, 4);                             // daddiu t6, s7, 4
  c->vmula_bc(DEST::xyzw, BC::w, vf6, vf0);         // vmulaw.xyzw acc, vf6, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf9, vf10);   // vmaddx.xyzw vf9, vf9, vf10
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf7, -48, a3);                            // sqc2 vf7, -48(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, -32, a3);                            // sqc2 vf8, -32(a3)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L99
  c->sqc2(vf9, -16, a3);                            // sqc2 vf9, -16(a3)
  if (bc) {goto block_3;}                           // branch non-likely

  //beq r0, r0, L106                                // beq r0, r0, L106
  // nop                                            // sll r0, r0, 0
  goto block_17;                                    // branch always


  block_16:
  c->subs(f7, f0, f1);                              // sub.s f7, f0, f1
  c->vsub(DEST::xyzw, vf7, vf4, vf1);               // vsub.xyzw vf7, vf4, vf1
  c->subs(f8, f2, f3);                              // sub.s f8, f2, f3
  c->vsub(DEST::xyzw, vf8, vf5, vf2);               // vsub.xyzw vf8, vf5, vf2
  c->subs(f5, f7, f8);                              // sub.s f5, f7, f8
  c->vsub(DEST::xyzw, vf9, vf6, vf3);               // vsub.xyzw vf9, vf6, vf3
  c->divs(f6, f7, f5);                              // div.s f6, f7, f5
  c->daddiu(a3, a3, 96);                            // daddiu a3, a3, 96
  c->mfc1(v1, f6);                                  // mfc1 v1, f6
  c->mov128_vf_gpr(vf10, v1);                       // qmtc2.i vf10, v1
  c->sqc2(vf4, -96, a3);                            // sqc2 vf4, -96(a3)
  c->vmula_bc(DEST::xyzw, BC::w, vf1, vf0);         // vmulaw.xyzw acc, vf1, vf0
  c->sqc2(vf5, -80, a3);                            // sqc2 vf5, -80(a3)
  c->vmadd_bc(DEST::xyzw, BC::x, vf7, vf7, vf10);   // vmaddx.xyzw vf7, vf7, vf10
  c->sqc2(vf6, -64, a3);                            // sqc2 vf6, -64(a3)
  c->vmula_bc(DEST::xyzw, BC::w, vf2, vf0);         // vmulaw.xyzw acc, vf2, vf0
  c->daddiu(t0, t0, 2);                             // daddiu t0, t0, 2
  c->vmadd_bc(DEST::xyzw, BC::x, vf8, vf8, vf10);   // vmaddx.xyzw vf8, vf8, vf10
  c->daddiu(t6, s7, 4);                             // daddiu t6, s7, 4
  c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf9, vf10);   // vmaddx.xyzw vf9, vf9, vf10
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf7, -48, a3);                            // sqc2 vf7, -48(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, -32, a3);                            // sqc2 vf8, -32(a3)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L98
  c->sqc2(vf9, -16, a3);                            // sqc2 vf9, -16(a3)
  if (bc) {goto block_1;}                           // branch non-likely


  block_17:
  c->lqc2(vf1, 0, t3);                              // lqc2 vf1, 0(t3)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 16, t3);                             // lqc2 vf2, 16(t3)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 32, t3);                             // lqc2 vf3, 32(t3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf1, 0, a3);                              // sqc2 vf1, 0(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf2, 16, a3);                             // sqc2 vf2, 16(a3)
  // nop                                            // sll r0, r0, 0
  //jr ra                                           // jr ra
  c->sqc2(vf3, 32, a3);                             // sqc2 vf3, 32(a3)
  goto end_of_function;                             // return

  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

} // namespace clip_polygon_against_negative_hyperplane
} // namespace Mips2C

