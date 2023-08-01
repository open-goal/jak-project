//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace method_39_nav_state {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -96);                           // daddiu sp, sp, -96
  c->swc1(f20, 80, sp);                             // swc1 f20, 80(sp)
  c->swc1(f22, 84, sp);                             // swc1 f22, 84(sp)
  c->lwc1(f0, 32, a0);                              // lwc1 f0, 32(a0)
  c->lwu(v1, 4, a0);                                // lwu v1, 4(a0)
  c->lwc1(f1, 28, v1);                              // lwc1 f1, 28(v1)
  c->muls(f1, f0, f1);                              // mul.s f1, f0, f1
  c->daddiu(v1, a0, 48);                            // daddiu v1, a0, 48
  c->lwc1(f0, 0, v1);                               // lwc1 f0, 0(v1)
  c->lwc1(f2, 0, v1);                               // lwc1 f2, 0(v1)
  c->muls(f0, f0, f2);                              // mul.s f0, f0, f2
  c->lwc1(f2, 8, v1);                               // lwc1 f2, 8(v1)
  c->lwc1(f3, 8, v1);                               // lwc1 f3, 8(v1)
  c->muls(f2, f2, f3);                              // mul.s f2, f2, f3
  c->adds(f0, f0, f2);                              // add.s f0, f0, f2
  c->sqrts(f0, f0);                                 // sqrt.s f0, f0
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->daddiu(v1, sp, 16);                            // daddiu v1, sp, 16
  c->daddiu(a1, sp, 32);                            // daddiu a1, sp, 32
  c->lwu(a2, 12, a0);                               // lwu a2, 12(a0)
  c->lwu(a3, 0, a2);                                // lwu a3, 0(a2)
  c->lwu(a2, 12, a0);                               // lwu a2, 12(a0)
  c->lwu(a2, 0, a2);                                // lwu a2, 0(a2)
  c->lwc1(f2, 28, a2);                              // lwc1 f2, 28(a2)
  cop1_bc = c->fprs[f2] < c->fprs[f0];              // c.lt.s f2, f0
  bc = !cop1_bc;                                    // bc1f L154
  c->mov64(v0, s7);                                 // or v0, s7, r0
  if (bc) {goto block_6;}                           // branch non-likely

  c->mov64(a2, v1);                                 // or a2, v1, r0
  c->daddiu(t0, a0, 48);                            // daddiu t0, a0, 48
  c->lui(t1, 16256);                                // lui t1, 16256
  c->mtc1(f2, t1);                                  // mtc1 f2, t1
  c->divs(f2, f2, f0);                              // div.s f2, f2, f0
  c->lqc2(vf1, 0, t0);                              // lqc2 vf1, 0(t0)
  c->mfc1(t0, f2);                                  // mfc1 t0, f2
  c->mov128_vf_gpr(vf2, t0);                        // qmtc2.i vf2, t0
  c->vadd_bc(DEST::w, BC::x, vf1, vf0, vf0);        // vaddx.w vf1, vf0, vf0
  c->vmul_bc(DEST::xyz, BC::x, vf1, vf1, vf2);      // vmulx.xyz vf1, vf1, vf2
  c->sqc2(vf1, 0, a2);                              // sqc2 vf1, 0(a2)
  c->mtc1(f2, r0);                                  // mtc1 f2, r0
  c->swc1(f2, 4, v1);                               // swc1 f2, 4(v1)
  c->mov64(a2, a1);                                 // or a2, a1, r0
  c->lwc1(f2, 16, a3);                              // lwc1 f2, 16(a3)
  c->cvtws(f1, f1);                                 // cvt.w.s f1, f1
  c->mfc1(a3, f1);                                  // mfc1 a3, f1
  c->dsll32(a3, a3, 16);                            // dsll32 a3, a3, 16
  c->dsra32(a3, a3, 16);                            // dsra32 a3, a3, 16
  c->mtc1(f1, a3);                                  // mtc1 f1, a3
  c->cvtsw(f1, f1);                                 // cvt.s.w f1, f1
  c->muls(f1, f2, f1);                              // mul.s f1, f2, f1
  c->lui(a3, 16255);                                // lui a3, 16255
  c->lui(t0, -16854);                               // lui t0, -16854
  c->ori(a3, a3, 65502);                            // ori a3, a3, 65502
  c->ori(t0, t0, 43253);                            // ori t0, t0, 43253
  c->subs(f22, f22, f22);                           // sub.s f22, f22, f22
  c->lui(t1, 15368);                                // lui t1, 15368
  c->mtc1(f10, a3);                                 // mtc1 f10, a3
  c->ori(a3, t1, 27638);                            // ori a3, t1, 27638
  c->mtc1(f11, t0);                                 // mtc1 f11, t0
  c->lui(t0, -18099);                               // lui t0, -18099
  c->muls(f2, f1, f1);                              // mul.s f2, f1, f1
  c->ori(t0, t0, 8306);                             // ori t0, t0, 8306
  c->mtc1(f12, a3);                                 // mtc1 f12, a3
  c->lui(a3, 13850);                                // lui a3, 13850
  c->mtc1(f14, t0);                                 // mtc1 f14, t0
  c->ori(t0, a3, 41599);                            // ori t0, a3, 41599
  // Unknown instr: mula.s f1, f10
  float acc;
  acc = c->fprs[f1] * c->fprs[f10];
  c->lui(a3, 16256);                                // lui a3, 16256
  c->muls(f3, f2, f1);                              // mul.s f3, f2, f1
  c->mov64(a3, a3);                                 // or a3, a3, r0
  c->muls(f4, f2, f2);                              // mul.s f4, f2, f2
  c->lui(t1, -16641);                               // lui t1, -16641
  c->mtc1(f15, t0);                                 // mtc1 f15, t0
  c->lui(t0, -16641);                               // lui t0, -16641
  c->or_(t0, t1, t0);                               // or t0, t1, t0
  c->mtc1(f16, a3);                                 // mtc1 f16, a3
  // nop                                            // sll r0, r0, 0
  c->mtc1(f17, t0);                                 // mtc1 f17, t0
  // nop                                            // sll r0, r0, 0
  c->muls(f5, f3, f2);                              // mul.s f5, f3, f2
  // nop                                            // sll r0, r0, 0
  c->muls(f6, f3, f3);                              // mul.s f6, f3, f3
  // nop                                            // sll r0, r0, 0
  c->muls(f7, f4, f3);                              // mul.s f7, f4, f3
  // nop                                            // sll r0, r0, 0
  c->muls(f8, f4, f4);                              // mul.s f8, f4, f4
  // nop                                            // sll r0, r0, 0
  c->muls(f9, f5, f4);                              // mul.s f9, f5, f4
  c->lui(a3, 15658);                                // lui a3, 15658
  // Unknown instr: madda.s f3, f11
  acc += c->fprs[f3] * c->fprs[f11];
  c->ori(a3, a3, 31272);                            // ori a3, a3, 31272
  // Unknown instr: madda.s f5, f12
  acc += c->fprs[f5] * c->fprs[f12];
  c->lui(t0, -17742);                               // lui t0, -17742
  // Unknown instr: madda.s f7, f14
  acc += c->fprs[f7] * c->fprs[f14];
  c->ori(t0, t0, 48177);                            // ori t0, t0, 48177
  // Unknown instr: madd.s f21, f9, f15
  c->fprs[f21] = acc + (c->fprs[f9] * c->fprs[f15]);
  c->lui(t1, 14249);                                // lui t1, 14249
  c->mtc1(f18, a3);                                 // mtc1 f18, a3
  c->ori(a3, t1, 13291);                            // ori a3, t1, 13291
  c->mtc1(f19, t0);                                 // mtc1 f19, t0
  // nop                                            // sll r0, r0, 0
  c->mtc1(f20, a3);                                 // mtc1 f20, a3
  // nop                                            // sll r0, r0, 0
  // Unknown instr: mula.s f16, f16
  acc = c->fprs[f16] * c->fprs[f16];
  // nop                                            // sll r0, r0, 0
  // Unknown instr: madda.s f2, f17
  acc += c->fprs[f2] * c->fprs[f17];
  // nop                                            // sll r0, r0, 0
  // Unknown instr: madda.s f4, f18
  acc += c->fprs[f4] * c->fprs[f18];
  // nop                                            // sll r0, r0, 0
  // Unknown instr: madda.s f6, f19
  acc += c->fprs[f6] * c->fprs[f19];
  // nop                                            // sll r0, r0, 0
  // Unknown instr: madd.s f22, f8, f20
  c->fprs[f22] = acc + (c->fprs[f8] * c->fprs[f20]);
  // nop                                            // sll r0, r0, 0
  c->swc1(f21, 0, a2);                              // swc1 f21, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->swc1(f22, 4, a2);                              // swc1 f22, 4(a2)
  c->gprs[a2].du64[0] = 0;                          // or a2, r0, r0
  c->mov64(a3, v1);                                 // or a3, v1, r0
  c->daddiu(a2, a0, 144);                           // daddiu a2, a0, 144
  c->lwc1(f1, 0, a3);                               // lwc1 f1, 0(a3)
  c->lwc1(f2, 4, a3);                               // lwc1 f2, 4(a3)
  c->lwc1(f3, 8, a3);                               // lwc1 f3, 8(a3)
  c->lwc1(f4, 0, a2);                               // lwc1 f4, 0(a2)
  c->lwc1(f5, 4, a2);                               // lwc1 f5, 4(a2)
  c->lwc1(f6, 8, a2);                               // lwc1 f6, 8(a2)
  // Unknown instr: mula.s f1, f4
  // Unknown instr: madda.s f2, f5
  // Unknown instr: madd.s f1, f3, f6
  c->fprs[f1] = (c->fprs[f3] * c->fprs[f6]) + (c->fprs[f2] * c->fprs[f5]) + (c->fprs[f1] * c->fprs[f4]);
  c->mfc1(a2, f1);                                  // mfc1 a2, f1
  c->mtc1(f1, a2);                                  // mtc1 f1, a2
  c->lwc1(f2, 4, a1);                               // lwc1 f2, 4(a1)
  cop1_bc = c->fprs[f1] < c->fprs[f2];              // c.lt.s f1, f2
  bc = !cop1_bc;                                    // bc1f L154
  c->mov64(v0, s7);                                 // or v0, s7, r0
  if (bc) {goto block_6;}                           // branch non-likely

  c->daddiu(a2, sp, 48);                            // daddiu a2, sp, 48
  c->daddiu(a3, sp, 64);                            // daddiu a3, sp, 64
  c->mov64(t0, a2);                                 // or t0, a2, r0
  c->daddiu(t1, a0, 144);                           // daddiu t1, a0, 144
  c->lwc1(f1, 0, a1);                               // lwc1 f1, 0(a1)
  c->lwc1(f3, 4, a1);                               // lwc1 f3, 4(a1)
  c->lwc1(f2, 0, t1);                               // lwc1 f2, 0(t1)
  c->lwc1(f4, 8, t1);                               // lwc1 f4, 8(t1)
  c->muls(f5, f3, f2);                              // mul.s f5, f3, f2
  c->muls(f6, f1, f4);                              // mul.s f6, f1, f4
  c->adds(f5, f5, f6);                              // add.s f5, f5, f6
  c->swc1(f5, 0, t0);                               // swc1 f5, 0(t0)
  c->lwc1(f5, 4, t1);                               // lwc1 f5, 4(t1)
  c->swc1(f5, 4, t0);                               // swc1 f5, 4(t0)
  c->muls(f3, f3, f4);                              // mul.s f3, f3, f4
  c->muls(f1, f1, f2);                              // mul.s f1, f1, f2
  c->subs(f1, f3, f1);                              // sub.s f1, f3, f1
  c->swc1(f1, 8, t0);                               // swc1 f1, 8(t0)
  c->mfc1(t0, f1);                                  // mfc1 t0, f1
  c->mov64(t0, a3);                                 // or t0, a3, r0
  c->daddiu(t1, a0, 144);                           // daddiu t1, a0, 144
  c->lwc1(f1, 0, a1);                               // lwc1 f1, 0(a1)
  c->negs(f1, f1);                                  // neg.s f1, f1
  c->lwc1(f3, 4, a1);                               // lwc1 f3, 4(a1)
  c->lwc1(f2, 0, t1);                               // lwc1 f2, 0(t1)
  c->lwc1(f4, 8, t1);                               // lwc1 f4, 8(t1)
  c->muls(f5, f3, f2);                              // mul.s f5, f3, f2
  c->muls(f6, f1, f4);                              // mul.s f6, f1, f4
  c->adds(f5, f5, f6);                              // add.s f5, f5, f6
  c->swc1(f5, 0, t0);                               // swc1 f5, 0(t0)
  c->lwc1(f5, 4, t1);                               // lwc1 f5, 4(t1)
  c->swc1(f5, 4, t0);                               // swc1 f5, 4(t0)
  c->muls(f3, f3, f4);                              // mul.s f3, f3, f4
  c->muls(f1, f1, f2);                              // mul.s f1, f1, f2
  c->subs(f1, f3, f1);                              // sub.s f1, f3, f1
  c->swc1(f1, 8, t0);                               // swc1 f1, 8(t0)
  c->mfc1(a1, f1);                                  // mfc1 a1, f1
  c->mov64(t0, a2);                                 // or t0, a2, r0
  c->mov64(a1, v1);                                 // or a1, v1, r0
  c->lwc1(f1, 0, t0);                               // lwc1 f1, 0(t0)
  c->lwc1(f2, 4, t0);                               // lwc1 f2, 4(t0)
  c->lwc1(f3, 8, t0);                               // lwc1 f3, 8(t0)
  c->lwc1(f4, 0, a1);                               // lwc1 f4, 0(a1)
  c->lwc1(f5, 4, a1);                               // lwc1 f5, 4(a1)
  c->lwc1(f6, 8, a1);                               // lwc1 f6, 8(a1)
  // Unknown instr: mula.s f1, f4
  // Unknown instr: madda.s f2, f5
  // Unknown instr: madd.s f1, f3, f6
  c->fprs[f1] = (c->fprs[f3] * c->fprs[f6]) + (c->fprs[f2] * c->fprs[f5]) + (c->fprs[f1] * c->fprs[f4]);
  c->mfc1(a1, f1);                                  // mfc1 a1, f1
  c->mtc1(f1, a1);                                  // mtc1 f1, a1
  c->mov64(t0, a3);                                 // or t0, a3, r0
  c->mov64(a1, v1);                                 // or a1, v1, r0
  c->lwc1(f2, 0, t0);                               // lwc1 f2, 0(t0)
  c->lwc1(f3, 4, t0);                               // lwc1 f3, 4(t0)
  c->lwc1(f4, 8, t0);                               // lwc1 f4, 8(t0)
  c->lwc1(f5, 0, a1);                               // lwc1 f5, 0(a1)
  c->lwc1(f6, 4, a1);                               // lwc1 f6, 4(a1)
  c->lwc1(f7, 8, a1);                               // lwc1 f7, 8(a1)
  // Unknown instr: mula.s f2, f5
  // Unknown instr: madda.s f3, f6
  // Unknown instr: madd.s f2, f4, f7
  c->fprs[f2] = (c->fprs[f4] * c->fprs[f7]) + (c->fprs[f3] * c->fprs[f6]) + (c->fprs[f2] * c->fprs[f5]);
  c->mfc1(a1, f2);                                  // mfc1 a1, f2
  c->mtc1(f2, a1);                                  // mtc1 f2, a1
  cop1_bc = c->fprs[f2] < c->fprs[f1];              // c.lt.s f2, f1
  bc = !cop1_bc;                                    // bc1f L152
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_4;}                           // branch non-likely

  c->mov64(a1, v1);                                 // or a1, v1, r0
  c->lq(a2, 0, a2);                                 // lq a2, 0(a2)
  c->sq(a2, 0, a1);                                 // sq a2, 0(a1)
  //beq r0, r0, L153                                // beq r0, r0, L153
  // nop                                            // sll r0, r0, 0
  goto block_5;                                     // branch always


block_4:
  c->mov64(a1, v1);                                 // or a1, v1, r0
  c->lq(a2, 0, a3);                                 // lq a2, 0(a3)
  c->sq(a2, 0, a1);                                 // sq a2, 0(a1)

block_5:
  c->daddiu(a0, a0, 48);                            // daddiu a0, a0, 48
  c->lqc2(vf1, 0, v1);                              // lqc2 vf1, 0(v1)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mov128_vf_gpr(vf2, v1);                        // qmtc2.i vf2, v1
  c->vadd_bc(DEST::w, BC::x, vf1, vf0, vf0);        // vaddx.w vf1, vf0, vf0
  c->vmul_bc(DEST::xyz, BC::x, vf1, vf1, vf2);      // vmulx.xyz vf1, vf1, vf2
  c->sqc2(vf1, 0, a0);                              // sqc2 vf1, 0(a0)
  c->daddiu(v0, s7, 4);                             // daddiu v0, s7, #t

block_6:
  c->lwc1(f22, 84, sp);                             // lwc1 f22, 84(sp)
  c->lwc1(f20, 80, sp);                             // lwc1 f20, 80(sp)
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
  gLinkedFunctionTable.reg("(method 39 nav-state)", execute, 96);
}

} // namespace method_39_nav_state
} // namespace Mips2C
