//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace find_closest_circle_ray_intersection {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -32);                           // daddiu sp, sp, -32
  c->lui(v1, 16256);                                // lui v1, 16256
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->addiu(v0, r0, -1);                             // addiu v0, r0, -1
  c->daddiu(v1, sp, 16);                            // daddiu v1, sp, 16
  c->mov64(t2, a1);                                 // or t2, a1, r0
  c->mtc1(f0, a2);                                  // mtc1 f0, a2
  c->lqc2(vf1, 0, t2);                              // lqc2 vf1, 0(t2)
  c->mfc1(t2, f0);                                  // mfc1 t2, f0
  c->mov128_vf_gpr(vf2, t2);                        // qmtc2.i vf2, t2
  c->vadd_bc(DEST::w, BC::x, vf1, vf0, vf0);        // vaddx.w vf1, vf0, vf0
  c->vmul_bc(DEST::xyz, BC::x, vf1, vf1, vf2);      // vmulx.xyz vf1, vf1, vf2
  c->sqc2(vf1, 0, v1);                              // sqc2 vf1, 0(v1)
  c->addiu(v1, r0, 0);                              // addiu v1, r0, 0
  //beq r0, r0, L311                                // beq r0, r0, L311
  // nop                                            // sll r0, r0, 0
  goto block_18;                                    // branch always

  
block_1:
  c->addiu(t2, r0, 1);                              // addiu t2, r0, 1
  if (((s64)c->sgpr64(v1)) >= 0) {                  // bgezl v1, L304
    c->dsllv(t2, t2, v1);                           // dsllv t2, t2, v1
    goto block_4;
  }
  
block_3:
  c->dsubu(t3, r0, v1);                             // dsubu t3, r0, v1
  c->dsrav(t2, t2, t3);                             // dsrav t2, t2, t3
  
block_4:
  c->and_(t2, t1, t2);                              // and t2, t1, t2
  bc = c->sgpr64(t2) != 0;                          // bne t2, r0, L310
  c->mov64(t2, s7);                                 // or t2, s7, r0
  if (bc) {goto block_17;}                          // branch non-likely

  c->mov64(t4, a0);                                 // or t4, a0, r0
  c->mov64(t3, a1);                                 // or t3, a1, r0
  c->mtc1(f0, a2);                                  // mtc1 f0, a2
  c->dsll(t2, v1, 4);                               // dsll t2, v1, 4
  c->daddu(t2, t0, t2);                             // daddu t2, t0, t2
  c->lwc1(f1, 0, t2);                               // lwc1 f1, 0(t2)
  c->lwc1(f2, 0, t4);                               // lwc1 f2, 0(t4)
  c->subs(f2, f1, f2);                              // sub.s f2, f1, f2
  c->lwc1(f1, 8, t2);                               // lwc1 f1, 8(t2)
  c->lwc1(f3, 8, t4);                               // lwc1 f3, 8(t4)
  c->subs(f1, f1, f3);                              // sub.s f1, f1, f3
  c->lwc1(f3, 8, t3);                               // lwc1 f3, 8(t3)
  c->muls(f3, f3, f2);                              // mul.s f3, f3, f2
  c->lwc1(f4, 0, t3);                               // lwc1 f4, 0(t3)
  c->muls(f4, f4, f1);                              // mul.s f4, f4, f1
  c->subs(f3, f3, f4);                              // sub.s f3, f3, f4
  c->abss(f3, f3);                                  // abs.s f3, f3
  c->lwc1(f4, 12, t2);                              // lwc1 f4, 12(t2)
  cop1_bc = c->fprs[f3] < c->fprs[f4];              // c.lt.s f3, f4
  bc = !cop1_bc;                                    // bc1f L309
  c->mov64(t5, s7);                                 // or t5, s7, r0
  if (bc) {goto block_15;}                          // branch non-likely

  c->lwc1(f3, 0, t3);                               // lwc1 f3, 0(t3)
  c->muls(f3, f3, f2);                              // mul.s f3, f3, f2
  c->lwc1(f4, 8, t3);                               // lwc1 f4, 8(t3)
  c->muls(f4, f4, f1);                              // mul.s f4, f4, f1
  c->adds(f3, f3, f4);                              // add.s f3, f3, f4
  c->mtc1(f4, r0);                                  // mtc1 f4, r0
  cop1_bc = c->fprs[f3] < c->fprs[f4];              // c.lt.s f3, f4
  bc = !cop1_bc;                                    // bc1f L306
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_10;}                          // branch non-likely

  c->muls(f0, f2, f2);                              // mul.s f0, f2, f2
  c->mfc1(t3, f0);                                  // mfc1 t3, f0
  c->mtc1(f0, t3);                                  // mtc1 f0, t3
  c->muls(f1, f1, f1);                              // mul.s f1, f1, f1
  c->mfc1(t3, f1);                                  // mfc1 t3, f1
  c->mtc1(f1, t3);                                  // mtc1 f1, t3
  c->adds(f0, f0, f1);                              // add.s f0, f0, f1
  c->lwc1(f1, 12, t2);                              // lwc1 f1, 12(t2)
  c->muls(f1, f1, f1);                              // mul.s f1, f1, f1
  c->mfc1(t2, f1);                                  // mfc1 t2, f1
  c->mtc1(f1, t2);                                  // mtc1 f1, t2
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = cop1_bc;                                     // bc1t L305
  c->daddiu(t5, s7, 4);                             // daddiu t5, s7, 4
  if (bc) {goto block_9;}                           // branch non-likely

  c->mov64(t5, s7);                                 // or t5, s7, r0
  
block_9:
  //beq r0, r0, L309                                // beq r0, r0, L309
  // nop                                            // sll r0, r0, 0
  goto block_15;                                    // branch always

  
block_10:
  cop1_bc = c->fprs[f0] < c->fprs[f3];              // c.lt.s f0, f3
  bc = !cop1_bc;                                    // bc1f L308
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_14;}                          // branch non-likely

  c->lwc1(f1, 0, t2);                               // lwc1 f1, 0(t2)
  c->lwc1(f2, 0, t4);                               // lwc1 f2, 0(t4)
  c->lwc1(f3, 0, t3);                               // lwc1 f3, 0(t3)
  c->muls(f3, f3, f0);                              // mul.s f3, f3, f0
  c->adds(f2, f2, f3);                              // add.s f2, f2, f3
  c->subs(f1, f1, f2);                              // sub.s f1, f1, f2
  c->lwc1(f2, 8, t2);                               // lwc1 f2, 8(t2)
  c->lwc1(f3, 8, t4);                               // lwc1 f3, 8(t4)
  c->lwc1(f4, 8, t3);                               // lwc1 f4, 8(t3)
  c->muls(f0, f4, f0);                              // mul.s f0, f4, f0
  c->adds(f0, f3, f0);                              // add.s f0, f3, f0
  c->subs(f2, f2, f0);                              // sub.s f2, f2, f0
  c->muls(f0, f1, f1);                              // mul.s f0, f1, f1
  c->mfc1(t3, f0);                                  // mfc1 t3, f0
  c->mtc1(f0, t3);                                  // mtc1 f0, t3
  c->muls(f1, f2, f2);                              // mul.s f1, f2, f2
  c->mfc1(t3, f1);                                  // mfc1 t3, f1
  c->mtc1(f1, t3);                                  // mtc1 f1, t3
  c->adds(f0, f0, f1);                              // add.s f0, f0, f1
  c->lwc1(f1, 12, t2);                              // lwc1 f1, 12(t2)
  c->muls(f1, f1, f1);                              // mul.s f1, f1, f1
  c->mfc1(t2, f1);                                  // mfc1 t2, f1
  c->mtc1(f1, t2);                                  // mtc1 f1, t2
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = cop1_bc;                                     // bc1t L307
  c->daddiu(t5, s7, 4);                             // daddiu t5, s7, 4
  if (bc) {goto block_13;}                          // branch non-likely

  c->mov64(t5, s7);                                 // or t5, s7, r0
  
block_13:
  //beq r0, r0, L309                                // beq r0, r0, L309
  // nop                                            // sll r0, r0, 0
  goto block_15;                                    // branch always

  
block_14:
  c->daddiu(t5, s7, 4);                             // daddiu t5, s7, #t
  
block_15:
  bc = c->sgpr64(s7) == c->sgpr64(t5);              // beq s7, t5, L310
  c->mov64(t2, s7);                                 // or t2, s7, r0
  if (bc) {goto block_17;}                          // branch non-likely

  c->mov64(v0, v1);                                 // or v0, v1, r0
  //beq r0, r0, L312                                // beq r0, r0, L312
  // nop                                            // sll r0, r0, 0
  goto block_20;                                    // branch always

  
block_17:
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  
block_18:
  c->slt(t2, v1, a3);                               // slt t2, v1, a3
  bc = c->sgpr64(t2) != 0;                          // bne t2, r0, L303
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  
block_20:
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 32);                            // daddiu sp, sp, 32
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  gLinkedFunctionTable.reg("find-closest-circle-ray-intersection", execute, 32);
}

} // namespace find_closest_circle_ray_intersection

namespace method_51_nav_state {
struct Cache {
  void* cos; // cos
  void* ray_ccw_line_segment_intersection; // ray-ccw-line-segment-intersection?
  void* test_xz_point_on_line_segment; // test-xz-point-on-line-segment?
  void* vector_normalize; // vector-normalize!
  void* vector_segment_distance_point; // vector-segment-distance-point!
  void* vector_vector_xz_distance_squared; // vector-vector-xz-distance-squared
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -384);                          // daddiu sp, sp, -384
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s2, 288, sp);                               // sq s2, 288(sp)
  c->sq(s3, 304, sp);                               // sq s3, 304(sp)
  c->sq(s4, 320, sp);                               // sq s4, 320(sp)
  c->sq(s5, 336, sp);                               // sq s5, 336(sp)
  c->sq(gp, 352, sp);                               // sq gp, 352(sp)
  c->swc1(f28, 368, sp);                            // swc1 f28, 368(sp)
  c->swc1(f30, 372, sp);                            // swc1 f30, 372(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->lwu(v1, 16, gp);                               // lwu v1, 16(gp)
  c->sw(v1, 20, gp);                                // sw v1, 20(gp)
  c->daddiu(v1, sp, 16);                            // daddiu v1, sp, 16
  c->sw(v1, 112, sp);                               // sw v1, 112(sp)
  c->daddiu(v1, sp, 32);                            // daddiu v1, sp, 32
  c->sw(v1, 116, sp);                               // sw v1, 116(sp)
  c->daddiu(v1, sp, 80);                            // daddiu v1, sp, 80
  c->sw(v1, 120, sp);                               // sw v1, 120(sp)
  c->sw(s7, 124, sp);                               // sw s7, 124(sp)
  c->daddiu(a0, gp, 96);                            // daddiu a0, gp, 96
  c->daddiu(v1, gp, 80);                            // daddiu v1, gp, 80
  c->lwu(a1, 12, gp);                               // lwu a1, 12(gp)
  c->daddiu(a1, a1, 44);                            // daddiu a1, a1, 44
  c->lqc2(vf4, 0, v1);                              // lqc2 vf4, 0(v1)
  c->lqc2(vf5, 0, a1);                              // lqc2 vf5, 0(a1)
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, a0);                              // sqc2 vf6, 0(a0)
  c->daddiu(v1, gp, 112);                           // daddiu v1, gp, 112
  c->daddiu(a0, gp, 96);                            // daddiu a0, gp, 96
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->lwu(v1, 116, sp);                              // lwu v1, 116(sp)
  c->sw(s7, 32, v1);                                // sw s7, 32(v1)
  c->lwu(v1, 0, gp);                                // lwu v1, 0(gp)
  c->andi(v1, v1, 16);                              // andi v1, v1, 16
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L128
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_12;}                          // branch non-likely

  c->daddiu(s5, sp, 128);                           // daddiu s5, sp, 128
  c->daddiu(s4, sp, 144);                           // daddiu s4, sp, 144
  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->daddiu(v1, gp, 64);                            // daddiu v1, gp, 64
  c->lwu(a1, 12, gp);                               // lwu a1, 12(gp)
  c->daddiu(a1, a1, 44);                            // daddiu a1, a1, 44
  c->lqc2(vf4, 0, v1);                              // lqc2 vf4, 0(v1)
  c->lqc2(vf5, 0, a1);                              // lqc2 vf5, 0(a1)
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, a0);                              // sqc2 vf6, 0(a0)
  c->lwu(v1, 0, gp);                                // lwu v1, 0(gp)
  c->andi(v1, v1, 64);                              // andi v1, v1, 64
  if (((s64)c->sgpr64(v1)) != ((s64)0)) {           // bnel v1, r0, L124
    c->daddiu(v1, s7, 4);                           // daddiu v1, s7, 4
    goto block_5;
  }
  
block_3:
  c->lwu(v1, 28, gp);                               // lwu v1, 28(gp)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L124
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  if (bc) {goto block_5;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  
block_5:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L126
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_9;}                           // branch non-likely

  c->daddu(v1, r0, s4);                             // daddu v1, r0, s4
  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->lwu(v1, 4, gp);                                // lwu v1, 4(gp)
  c->lwc1(f0, 20, v1);                              // lwc1 f0, 20(v1)
  c->swc1(f0, 16, s4);                              // swc1 f0, 16(s4)
  c->addiu(v1, r0, 3);                              // addiu v1, r0, 3
  c->sb(v1, 20, s4);                                // sb v1, 20(s4)
  c->lwu(a0, 12, gp);                               // lwu a0, 12(gp)
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 188, v1);                              // lwu t9, 188(v1)
  c->mov64(a1, s4);                                 // or a1, s4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->lwu(v1, 24, s4);                               // lwu v1, 24(s4)
  c->sw(v1, 28, gp);                                // sw v1, 28(gp)
  c->lwu(v1, 28, gp);                               // lwu v1, 28(gp)
  bc = c->sgpr64(s7) != c->sgpr64(v1);              // bne s7, v1, L125
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_8;}                           // branch non-likely

  c->lwu(v1, 16, gp);                               // lwu v1, 16(gp)
  c->sw(v1, 28, gp);                                // sw v1, 28(gp)
  
block_8:
  c->addiu(v1, r0, -65);                            // addiu v1, r0, -65
  c->lwu(a0, 0, gp);                                // lwu a0, 0(gp)
  c->and_(v1, v1, a0);                              // and v1, v1, a0
  c->sw(v1, 0, gp);                                 // sw v1, 0(gp)
  
block_9:
  c->lwu(a0, 12, gp);                               // lwu a0, 12(gp)
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 180, v1);                              // lwu t9, 180(v1)
  c->lwu(a1, 28, gp);                               // lwu a1, 28(gp)
  c->lwu(a2, 112, sp);                              // lwu a2, 112(sp)
  c->mov64(a3, s5);                                 // or a3, s5, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->lwc1(f0, 100, gp);                             // lwc1 f0, 100(gp)
  c->lwu(v1, 112, sp);                              // lwu v1, 112(sp)
  c->swc1(f0, 4, v1);                               // swc1 f0, 4(v1)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->daddiu(a1, gp, 48);                            // daddiu a1, gp, 48
  c->lwu(v1, 112, sp);                              // lwu v1, 112(sp)
  c->daddiu(a0, gp, 96);                            // daddiu a0, gp, 96
  c->lqc2(vf4, 0, v1);                              // lqc2 vf4, 0(v1)
  c->lqc2(vf5, 0, a0);                              // lqc2 vf5, 0(a0)
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, a1);                              // sqc2 vf6, 0(a1)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 52, gp);                              // swc1 f0, 52(gp)
  c->daddiu(v1, gp, 48);                            // daddiu v1, gp, 48
  c->lwc1(f0, 0, v1);                               // lwc1 f0, 0(v1)
  c->lwc1(f1, 0, v1);                               // lwc1 f1, 0(v1)
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->lwc1(f1, 8, v1);                               // lwc1 f1, 8(v1)
  c->lwc1(f2, 8, v1);                               // lwc1 f2, 8(v1)
  c->muls(f1, f1, f2);                              // mul.s f1, f1, f2
  c->adds(f0, f0, f1);                              // add.s f0, f0, f1
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lui(v1, 17792);                                // lui v1, 17792
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->muls(f1, f1, f1);                              // mul.s f1, f1, f1
  c->mfc1(v1, f1);                                  // mfc1 v1, f1
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = !cop1_bc;                                    // bc1f L127
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_11;}                          // branch non-likely

  c->lwu(v1, 0, gp);                                // lwu v1, 0(gp)
  c->ori(v1, v1, 512);                              // ori v1, v1, 512
  c->sw(v1, 0, gp);                                 // sw v1, 0(gp)
  
block_11:
  c->lwu(a0, 12, gp);                               // lwu a0, 12(gp)
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 72, v1);                               // lwu t9, 72(v1)
  c->lwu(a1, 16, gp);                               // lwu a1, 16(gp)
  c->lwu(a2, 28, gp);                               // lwu a2, 28(gp)
  c->lwu(a3, 116, sp);                              // lwu a3, 116(sp)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  
block_12:
  c->lwu(v1, 116, sp);                              // lwu v1, 116(sp)
  c->lwu(v1, 32, v1);                               // lwu v1, 32(v1)
  bc = c->sgpr64(s7) != c->sgpr64(v1);              // bne s7, v1, L129
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_14;}                          // branch non-likely

  c->sw(s7, 24, gp);                                // sw s7, 24(gp)
  c->mov64(v1, s7);                                 // or v1, s7, r0
  //beq r0, r0, L135                                // beq r0, r0, L135
  // nop                                            // sll r0, r0, 0
  goto block_27;                                    // branch always

  
block_14:
  c->lwu(v1, 116, sp);                              // lwu v1, 116(sp)
  c->lwu(v1, 32, v1);                               // lwu v1, 32(v1)
  c->sw(v1, 24, gp);                                // sw v1, 24(gp)
  c->lwu(v1, 120, sp);                              // lwu v1, 120(sp)
  c->daddu(v1, r0, v1);                             // daddu v1, r0, v1
  c->lwu(a0, 116, sp);                              // lwu a0, 116(sp)
  c->daddu(a0, r0, a0);                             // daddu a0, r0, a0
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->lwu(v1, 120, sp);                              // lwu v1, 120(sp)
  c->daddiu(v1, v1, 16);                            // daddiu v1, v1, 16
  c->lwu(a0, 116, sp);                              // lwu a0, 116(sp)
  c->daddiu(a0, a0, 16);                            // daddiu a0, a0, 16
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  c->sw(v1, 124, sp);                               // sw v1, 124(sp)
  c->mov64(v1, s7);                                 // or v1, s7, r0
  //beq r0, r0, L133                                // beq r0, r0, L133
  // nop                                            // sll r0, r0, 0
  goto block_20;                                    // branch always

  
block_15:
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L131
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_17;}                          // branch non-likely

  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  c->load_symbol2(t9, cache.vector_segment_distance_point);// lw t9, vector-segment-distance-point!(s7)
  c->daddiu(a0, gp, 96);                            // daddiu a0, gp, 96
  c->lwu(v1, 116, sp);                              // lwu v1, 116(sp)
  c->daddu(a1, r0, v1);                             // daddu a1, r0, v1
  c->lwu(v1, 116, sp);                              // lwu v1, 116(sp)
  c->daddiu(a2, v1, 16);                            // daddiu a2, v1, 16
  c->daddiu(a3, gp, 112);                           // daddiu a3, gp, 112
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->daddiu(a1, gp, 48);                            // daddiu a1, gp, 48
  c->lwu(v1, 112, sp);                              // lwu v1, 112(sp)
  c->daddiu(a0, gp, 112);                           // daddiu a0, gp, 112
  c->lqc2(vf4, 0, v1);                              // lqc2 vf4, 0(v1)
  c->lqc2(vf5, 0, a0);                              // lqc2 vf5, 0(a0)
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, a1);                              // sqc2 vf6, 0(a1)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 52, gp);                              // swc1 f0, 52(gp)
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  
block_17:
  c->lwu(v1, 116, sp);                              // lwu v1, 116(sp)
  c->lwu(v1, 32, v1);                               // lwu v1, 32(v1)
  c->sw(v1, 20, gp);                                // sw v1, 20(gp)
  c->lwu(a0, 12, gp);                               // lwu a0, 12(gp)
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 72, v1);                               // lwu t9, 72(v1)
  c->lwu(v1, 116, sp);                              // lwu v1, 116(sp)
  c->lwu(a1, 32, v1);                               // lwu a1, 32(v1)
  c->lwu(a2, 28, gp);                               // lwu a2, 28(gp)
  c->lwu(a3, 116, sp);                              // lwu a3, 116(sp)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L132
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_19;}                          // branch non-likely

  c->lwu(v1, 116, sp);                              // lwu v1, 116(sp)
  c->lwu(v1, 32, v1);                               // lwu v1, 32(v1)
  c->sw(v1, 24, gp);                                // sw v1, 24(gp)
  c->lwu(v1, 120, sp);                              // lwu v1, 120(sp)
  c->daddu(v1, r0, v1);                             // daddu v1, r0, v1
  c->lwu(a0, 116, sp);                              // lwu a0, 116(sp)
  c->daddu(a0, r0, a0);                             // daddu a0, r0, a0
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->lwu(v1, 120, sp);                              // lwu v1, 120(sp)
  c->daddiu(v1, v1, 16);                            // daddiu v1, v1, 16
  c->lwu(a0, 116, sp);                              // lwu a0, 116(sp)
  c->daddiu(a0, a0, 16);                            // daddiu a0, a0, 16
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  //beq r0, r0, L133                                // beq r0, r0, L133
  // nop                                            // sll r0, r0, 0
  goto block_20;                                    // branch always

  
block_19:
  c->sw(s7, 24, gp);                                // sw s7, 24(gp)
  c->sw(s7, 124, sp);                               // sw s7, 124(sp)
  c->mov64(v1, s7);                                 // or v1, s7, r0
  
block_20:
  c->lwu(v1, 124, sp);                              // lwu v1, 124(sp)
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(v1))) {// beql s7, v1, L134
    c->mov64(v1, v1);                               // or v1, v1, r0
    goto block_25;
  }
  
block_22:
  c->lwu(v1, 116, sp);                              // lwu v1, 116(sp)
  c->lwu(v1, 32, v1);                               // lwu v1, 32(v1)
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(v1))) {// beql s7, v1, L134
    c->mov64(v1, v1);                               // or v1, v1, r0
    goto block_25;
  }
  
block_24:
  c->load_symbol2(t9, cache.test_xz_point_on_line_segment);// lw t9, test-xz-point-on-line-segment?(s7)
  c->daddiu(a0, gp, 96);                            // daddiu a0, gp, 96
  c->lwu(v1, 116, sp);                              // lwu v1, 116(sp)
  c->daddu(a1, r0, v1);                             // daddu a1, r0, v1
  c->lwu(v1, 116, sp);                              // lwu v1, 116(sp)
  c->daddiu(a2, v1, 16);                            // daddiu a2, v1, 16
  c->lui(v1, 17356);                                // lui v1, 17356
  c->ori(a3, v1, 52428);                            // ori a3, v1, 52428
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  
block_25:
  bc = c->sgpr64(s7) != c->sgpr64(v1);              // bne s7, v1, L130
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_15;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  
block_27:
  c->lwu(v1, 124, sp);                              // lwu v1, 124(sp)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L143
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_44;}                          // branch non-likely

  c->daddiu(s5, sp, 192);                           // daddiu s5, sp, 192
  c->daddu(v1, r0, s5);                             // daddu v1, r0, s5
  c->lwu(a0, 120, sp);                              // lwu a0, 120(sp)
  c->daddiu(a0, a0, 16);                            // daddiu a0, a0, 16
  c->lwu(a1, 120, sp);                              // lwu a1, 120(sp)
  c->daddu(a1, r0, a1);                             // daddu a1, r0, a1
  c->lqc2(vf4, 0, a0);                              // lqc2 vf4, 0(a0)
  c->lqc2(vf5, 0, a1);                              // lqc2 vf5, 0(a1)
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, v1);                              // sqc2 vf6, 0(v1)
  c->load_symbol2(t9, cache.vector_normalize);      // lw t9, vector-normalize!(s7)
  c->daddu(a0, r0, s5);                             // daddu a0, r0, s5
  c->lui(v1, 17356);                                // lui v1, 17356
  c->ori(a1, v1, 52429);                            // ori a1, v1, 52429
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lwu(v1, 120, sp);                              // lwu v1, 120(sp)
  c->daddu(v1, r0, v1);                             // daddu v1, r0, v1
  c->lwu(a0, 120, sp);                              // lwu a0, 120(sp)
  c->daddu(a0, r0, a0);                             // daddu a0, r0, a0
  c->daddu(a1, r0, s5);                             // daddu a1, r0, s5
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->lqc2(vf4, 0, a0);                              // lqc2 vf4, 0(a0)
  c->lqc2(vf5, 0, a1);                              // lqc2 vf5, 0(a1)
  c->vadd(DEST::xyz, vf6, vf4, vf5);                // vadd.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, v1);                              // sqc2 vf6, 0(v1)
  c->lwu(v1, 120, sp);                              // lwu v1, 120(sp)
  c->daddiu(v1, v1, 16);                            // daddiu v1, v1, 16
  c->lwu(a0, 120, sp);                              // lwu a0, 120(sp)
  c->daddiu(a0, a0, 16);                            // daddiu a0, a0, 16
  c->daddu(a1, r0, s5);                             // daddu a1, r0, s5
  c->lqc2(vf4, 0, a0);                              // lqc2 vf4, 0(a0)
  c->lqc2(vf5, 0, a1);                              // lqc2 vf5, 0(a1)
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, v1);                              // sqc2 vf6, 0(v1)
  c->load_symbol2(t9, cache.ray_ccw_line_segment_intersection);// lw t9, ray-ccw-line-segment-intersection?(s7)
  c->daddiu(a0, gp, 112);                           // daddiu a0, gp, 112
  c->daddiu(a1, gp, 48);                            // daddiu a1, gp, 48
  c->lwu(v1, 120, sp);                              // lwu v1, 120(sp)
  c->daddu(a2, r0, v1);                             // daddu a2, r0, v1
  c->lwu(v1, 120, sp);                              // lwu v1, 120(sp)
  c->daddiu(a3, v1, 16);                            // daddiu a3, v1, 16
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  bc = c->sgpr64(s7) != c->sgpr64(v0);              // bne s7, v0, L143
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_44;}                          // branch non-likely

  c->addiu(s5, r0, -1);                             // addiu s5, r0, -1
  c->load_symbol2(t9, cache.cos);                   // lw t9, cos(s7)
  c->lui(a0, 17920);                                // lui a0, 17920
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mtc1(f0, v0);                                  // mtc1 f0, v0
  c->muls(f0, f0, f0);                              // mul.s f0, f0, f0
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->daddiu(v1, sp, 208);                           // daddiu v1, sp, 208
  c->daddiu(a0, gp, 48);                            // daddiu a0, gp, 48
  c->lwc1(f1, 0, a0);                               // lwc1 f1, 0(a0)
  c->lwc1(f2, 0, a0);                               // lwc1 f2, 0(a0)
  c->muls(f1, f1, f2);                              // mul.s f1, f1, f2
  c->lwc1(f2, 8, a0);                               // lwc1 f2, 8(a0)
  c->lwc1(f3, 8, a0);                               // lwc1 f3, 8(a0)
  c->muls(f2, f2, f3);                              // mul.s f2, f2, f3
  c->adds(f1, f1, f2);                              // add.s f1, f1, f2
  c->mfc1(a0, f1);                                  // mfc1 a0, f1
  c->mtc1(f1, a0);                                  // mtc1 f1, a0
  c->addiu(a0, r0, 2);                              // addiu a0, r0, 2
  //beq r0, r0, L137                                // beq r0, r0, L137
  // nop                                            // sll r0, r0, 0
  goto block_33;                                    // branch always

  
block_30:
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->mov64(a1, v1);                                 // or a1, v1, r0
  c->lwu(a2, 120, sp);                              // lwu a2, 120(sp)
  c->dsll(a3, a0, 4);                               // dsll a3, a0, 4
  c->daddu(a2, a2, a3);                             // daddu a2, a2, a3
  c->daddiu(a3, gp, 112);                           // daddiu a3, gp, 112
  c->lqc2(vf4, 0, a2);                              // lqc2 vf4, 0(a2)
  c->lqc2(vf5, 0, a3);                              // lqc2 vf5, 0(a3)
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, a1);                              // sqc2 vf6, 0(a1)
  c->daddiu(a2, gp, 48);                            // daddiu a2, gp, 48
  c->mov64(a1, v1);                                 // or a1, v1, r0
  c->lwc1(f2, 0, a2);                               // lwc1 f2, 0(a2)
  c->lwc1(f3, 4, a2);                               // lwc1 f3, 4(a2)
  c->lwc1(f4, 8, a2);                               // lwc1 f4, 8(a2)
  c->lwc1(f5, 0, a1);                               // lwc1 f5, 0(a1)
  c->lwc1(f6, 4, a1);                               // lwc1 f6, 4(a1)
  c->lwc1(f7, 8, a1);                               // lwc1 f7, 8(a1)
  // Unknown instr: mula.s f2, f5
  // Unknown instr: madda.s f3, f6
  // Unknown instr: madd.s f2, f4, f7
  c->mfc1(a1, f2);                                  // mfc1 a1, f2
  c->mtc1(f2, a1);                                  // mtc1 f2, a1
  c->mtc1(f3, r0);                                  // mtc1 f3, r0
  cop1_bc = c->fprs[f3] < c->fprs[f2];              // c.lt.s f3, f2
  bc = !cop1_bc;                                    // bc1f L137
  c->mov64(a1, s7);                                 // or a1, s7, r0
  if (bc) {goto block_33;}                          // branch non-likely

  c->muls(f2, f2, f2);                              // mul.s f2, f2, f2
  c->mfc1(a1, f2);                                  // mfc1 a1, f2
  c->mtc1(f2, a1);                                  // mtc1 f2, a1
  c->mov64(a1, v1);                                 // or a1, v1, r0
  c->lwc1(f3, 0, a1);                               // lwc1 f3, 0(a1)
  c->lwc1(f4, 0, a1);                               // lwc1 f4, 0(a1)
  c->muls(f3, f3, f4);                              // mul.s f3, f3, f4
  c->lwc1(f4, 8, a1);                               // lwc1 f4, 8(a1)
  c->lwc1(f5, 8, a1);                               // lwc1 f5, 8(a1)
  c->muls(f4, f4, f5);                              // mul.s f4, f4, f5
  c->adds(f3, f3, f4);                              // add.s f3, f3, f4
  c->mfc1(a1, f3);                                  // mfc1 a1, f3
  c->mtc1(f3, a1);                                  // mtc1 f3, a1
  c->muls(f3, f1, f3);                              // mul.s f3, f1, f3
  c->divs(f2, f2, f3);                              // div.s f2, f2, f3
  cop1_bc = c->fprs[f0] < c->fprs[f2];              // c.lt.s f0, f2
  bc = !cop1_bc;                                    // bc1f L137
  c->mov64(a1, s7);                                 // or a1, s7, r0
  if (bc) {goto block_33;}                          // branch non-likely

  c->movs(f0, f2);                                  // mov.s f0, f2
  c->mov64(s5, a0);                                 // or s5, a0, r0
  c->mov64(a1, s7);                                 // or a1, s7, r0
  
block_33:
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L136
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_30;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->addiu(v1, r0, -1);                             // addiu v1, r0, -1
  bc = c->sgpr64(s5) != c->sgpr64(v1);              // bne s5, v1, L142
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_43;}                          // branch non-likely

  c->lui(v1, 32640);                                // lui v1, 32640
  c->mtc1(f30, v1);                                 // mtc1 f30, v1
  c->daddiu(s3, sp, 224);                           // daddiu s3, sp, 224
  c->daddiu(s4, sp, 272);                           // daddiu s4, sp, 272
  c->lwu(a0, 12, gp);                               // lwu a0, 12(gp)
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 72, v1);                               // lwu t9, 72(v1)
  c->lwu(v1, 116, sp);                              // lwu v1, 116(sp)
  c->lwu(a1, 32, v1);                               // lwu a1, 32(v1)
  c->lwu(a2, 28, gp);                               // lwu a2, 28(gp)
  c->mov64(a3, s3);                                 // or a3, s3, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->lwu(v1, 32, s3);                               // lwu v1, 32(s3)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L138
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_37;}                          // branch non-likely

  c->mov64(a0, s4);                                 // or a0, s4, r0
  c->daddu(v1, r0, s3);                             // daddu v1, r0, s3
  c->daddiu(a1, s3, 16);                            // daddiu a1, s3, 16
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->lqc2(vf4, 0, v1);                              // lqc2 vf4, 0(v1)
  c->lqc2(vf5, 0, a1);                              // lqc2 vf5, 0(a1)
  c->vadd(DEST::xyz, vf6, vf4, vf5);                // vadd.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, a0);                              // sqc2 vf6, 0(a0)
  c->mov64(v1, s4);                                 // or v1, s4, r0
  c->mov64(a0, s4);                                 // or a0, s4, r0
  c->lui(a1, 16128);                                // lui a1, 16128
  c->mtc1(f0, a1);                                  // mtc1 f0, a1
  c->lqc2(vf1, 0, a0);                              // lqc2 vf1, 0(a0)
  c->mfc1(a0, f0);                                  // mfc1 a0, f0
  c->mov128_vf_gpr(vf2, a0);                        // qmtc2.i vf2, a0
  c->vadd_bc(DEST::w, BC::x, vf1, vf0, vf0);        // vaddx.w vf1, vf0, vf0
  c->vmul_bc(DEST::xyz, BC::x, vf1, vf1, vf2);      // vmulx.xyz vf1, vf1, vf2
  c->sqc2(vf1, 0, v1);                              // sqc2 vf1, 0(v1)
  c->mov64(v1, s7);                                 // or v1, s7, r0
  //beq r0, r0, L139                                // beq r0, r0, L139
  // nop                                            // sll r0, r0, 0
  goto block_38;                                    // branch always

  
block_37:
  c->mov64(v1, s4);                                 // or v1, s4, r0
  c->lwu(a0, 112, sp);                              // lwu a0, 112(sp)
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  
block_38:
  c->addiu(s3, r0, 2);                              // addiu s3, r0, 2
  //beq r0, r0, L141                                // beq r0, r0, L141
  // nop                                            // sll r0, r0, 0
  goto block_41;                                    // branch always

  
block_39:
  c->daddiu(s3, s3, -1);                            // daddiu s3, s3, -1
  c->lwu(v1, 120, sp);                              // lwu v1, 120(sp)
  c->dsll(a0, s3, 4);                               // dsll a0, s3, 4
  c->daddu(s2, v1, a0);                             // daddu s2, v1, a0
  c->load_symbol2(t9, cache.vector_vector_xz_distance_squared);// lw t9, vector-vector-xz-distance-squared(s7)
  c->daddiu(a0, gp, 112);                           // daddiu a0, gp, 112
  c->mov64(a1, s2);                                 // or a1, s2, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mtc1(f28, v0);                                 // mtc1 f28, v0
  c->load_symbol2(t9, cache.vector_vector_xz_distance_squared);// lw t9, vector-vector-xz-distance-squared(s7)
  c->mov64(a0, s4);                                 // or a0, s4, r0
  c->mov64(a1, s2);                                 // or a1, s2, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mtc1(f0, v0);                                  // mtc1 f0, v0
  c->adds(f0, f28, f0);                             // add.s f0, f28, f0
  cop1_bc = c->fprs[f0] < c->fprs[f30];             // c.lt.s f0, f30
  bc = !cop1_bc;                                    // bc1f L141
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_41;}                          // branch non-likely

  c->movs(f30, f0);                                 // mov.s f30, f0
  c->mov64(s5, s3);                                 // or s5, s3, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  
block_41:
  bc = c->sgpr64(s3) != 0;                          // bne s3, r0, L140
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_39;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  
block_43:
  c->daddiu(v1, gp, 48);                            // daddiu v1, gp, 48
  c->lwu(a0, 120, sp);                              // lwu a0, 120(sp)
  c->dsll(a1, s5, 4);                               // dsll a1, s5, 4
  c->daddu(a0, a0, a1);                             // daddu a0, a0, a1
  c->daddiu(a1, gp, 112);                           // daddiu a1, gp, 112
  c->lqc2(vf4, 0, a0);                              // lqc2 vf4, 0(a0)
  c->lqc2(vf5, 0, a1);                              // lqc2 vf5, 0(a1)
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, v1);                              // sqc2 vf6, 0(v1)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 52, gp);                              // swc1 f0, 52(gp)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  
block_44:
  c->daddiu(v1, gp, 48);                            // daddiu v1, gp, 48
  c->lqc2(vf1, 0, v1);                              // lqc2 vf1, 0(v1)
  c->vadd_bc(DEST::x, BC::w, vf2, vf0, vf0);        // vaddw.x vf2, vf0, vf0
  c->vmul(DEST::xyzw, vf1, vf1, vf1);               // vmul.xyzw vf1, vf1, vf1
  c->vmula_bc(DEST::x, BC::x, vf2, vf1);            // vmulax.x acc, vf2, vf1
  c->vmadda_bc(DEST::x, BC::y, vf2, vf1);           // vmadday.x acc, vf2, vf1
  c->vmadd_bc(DEST::x, BC::z, vf1, vf2, vf1);       // vmaddz.x vf1, vf2, vf1
  c->mov128_gpr_vf(v1, vf1);                        // qmfc2.i v1, vf1
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lwu(v1, 4, gp);                                // lwu v1, 4(gp)
  c->lwc1(f1, 24, v1);                              // lwc1 f1, 24(v1)
  c->muls(f1, f1, f1);                              // mul.s f1, f1, f1
  c->mfc1(v1, f1);                                  // mfc1 v1, f1
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  cop1_bc = c->fprs[f1] < c->fprs[f0];              // c.lt.s f1, f0
  bc = !cop1_bc;                                    // bc1f L144
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_46;}                          // branch non-likely

  c->daddiu(v1, gp, 48);                            // daddiu v1, gp, 48
  c->daddiu(a0, gp, 48);                            // daddiu a0, gp, 48
  c->lwu(a1, 4, gp);                                // lwu a1, 4(gp)
  c->lwc1(f1, 24, a1);                              // lwc1 f1, 24(a1)
  c->sqrts(f0, f0);                                 // sqrt.s f0, f0
  c->divs(f0, f1, f0);                              // div.s f0, f1, f0
  c->lqc2(vf1, 0, a0);                              // lqc2 vf1, 0(a0)
  c->mfc1(a0, f0);                                  // mfc1 a0, f0
  c->mov128_vf_gpr(vf2, a0);                        // qmtc2.i vf2, a0
  c->vadd_bc(DEST::w, BC::x, vf1, vf0, vf0);        // vaddx.w vf1, vf0, vf0
  c->vmul_bc(DEST::xyz, BC::x, vf1, vf1, vf2);      // vmulx.xyz vf1, vf1, vf2
  c->sqc2(vf1, 0, v1);                              // sqc2 vf1, 0(v1)
  
block_46:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lwc1(f30, 372, sp);                            // lwc1 f30, 372(sp)
  c->lwc1(f28, 368, sp);                            // lwc1 f28, 368(sp)
  c->lq(gp, 352, sp);                               // lq gp, 352(sp)
  c->lq(s5, 336, sp);                               // lq s5, 336(sp)
  c->lq(s4, 320, sp);                               // lq s4, 320(sp)
  c->lq(s3, 304, sp);                               // lq s3, 304(sp)
  c->lq(s2, 288, sp);                               // lq s2, 288(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 384);                           // daddiu sp, sp, 384
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.cos = intern_from_c("cos").c();
  cache.ray_ccw_line_segment_intersection = intern_from_c("ray-ccw-line-segment-intersection?").c();
  cache.test_xz_point_on_line_segment = intern_from_c("test-xz-point-on-line-segment?").c();
  cache.vector_normalize = intern_from_c("vector-normalize!").c();
  cache.vector_segment_distance_point = intern_from_c("vector-segment-distance-point!").c();
  cache.vector_vector_xz_distance_squared = intern_from_c("vector-vector-xz-distance-squared").c();
  gLinkedFunctionTable.reg("(method 51 nav-state)", execute, 384);
}

} // namespace method_51_nav_state

namespace method_40_nav_state {
struct Cache {
  void* nav_find_poly_parms; // nav-find-poly-parms
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -272);                          // daddiu sp, sp, -272
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s2, 192, sp);                               // sq s2, 192(sp)
  c->sq(s3, 208, sp);                               // sq s3, 208(sp)
  c->sq(s4, 224, sp);                               // sq s4, 224(sp)
  c->sq(s5, 240, sp);                               // sq s5, 240(sp)
  c->sq(gp, 256, sp);                               // sq gp, 256(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->mov64(s5, a1);                                 // or s5, a1, r0
  c->lwu(v1, 16, gp);                               // lwu v1, 16(gp)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L170
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_15;}                          // branch non-likely

  c->lwu(s3, 12, gp);                               // lwu s3, 12(gp)
  c->daddiu(s4, sp, 16);                            // daddiu s4, sp, 16
  c->addiu(s2, r0, 0);                              // addiu s2, r0, 0
  c->lwu(v1, 16, gp);                               // lwu v1, 16(gp)
  c->sw(v1, 48, s4);                                // sw v1, 48(s4)
  c->daddu(a1, r0, s4);                             // daddu a1, r0, s4
  c->daddiu(v1, gp, 80);                            // daddiu v1, gp, 80
  c->daddiu(a0, s3, 44);                            // daddiu a0, s3, 44
  c->lqc2(vf4, 0, v1);                              // lqc2 vf4, 0(v1)
  c->lqc2(vf5, 0, a0);                              // lqc2 vf5, 0(a0)
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, a1);                              // sqc2 vf6, 0(a1)
  c->daddiu(a1, s4, 32);                            // daddiu a1, s4, 32
  c->mov64(v1, s5);                                 // or v1, s5, r0
  c->daddiu(a0, s3, 44);                            // daddiu a0, s3, 44
  c->lqc2(vf4, 0, v1);                              // lqc2 vf4, 0(v1)
  c->lqc2(vf5, 0, a0);                              // lqc2 vf5, 0(a0)
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, a1);                              // sqc2 vf6, 0(a1)
  c->mov64(a0, s3);                                 // or a0, s3, r0
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 164, v1);                              // lwu t9, 164(v1)
  c->lwu(a1, 16, gp);                               // lwu a1, 16(gp)
  c->daddiu(a2, s4, 32);                            // daddiu a2, s4, 32
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L164
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_3;}                           // branch non-likely

  c->lwu(v1, 0, gp);                                // lwu v1, 0(gp)
  c->ori(v1, v1, 2048);                             // ori v1, v1, 2048
  c->sw(v1, 0, gp);                                 // sw v1, 0(gp)
  c->daddiu(v1, gp, 80);                            // daddiu v1, gp, 80
  c->lq(a0, 0, s5);                                 // lq a0, 0(s5)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  //beq r0, r0, L171                                // beq r0, r0, L171
  // nop                                            // sll r0, r0, 0
  goto block_17;                                    // branch always

  
block_3:
  c->mov64(v1, s4);                                 // or v1, s4, r0
  c->daddiu(a2, v1, 16);                            // daddiu a2, v1, 16
  c->daddiu(a0, v1, 32);                            // daddiu a0, v1, 32
  c->daddu(a1, r0, v1);                             // daddu a1, r0, v1
  c->lqc2(vf4, 0, a0);                              // lqc2 vf4, 0(a0)
  c->lqc2(vf5, 0, a1);                              // lqc2 vf5, 0(a1)
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, a2);                              // sqc2 vf6, 0(a2)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 20, v1);                              // swc1 f0, 20(v1)
  c->daddiu(a0, v1, 16);                            // daddiu a0, v1, 16
  c->lui(a1, 16256);                                // lui a1, 16256
  c->mtc1(f0, a1);                                  // mtc1 f0, a1
  c->lqc2(vf1, 0, a0);                              // lqc2 vf1, 0(a0)
  c->vmul(DEST::xyz, vf2, vf1, vf1);                // vmul.xyz vf2, vf1, vf1
  c->mfc1(a1, f0);                                  // mfc1 a1, f0
  c->mov128_vf_gpr(vf3, a1);                        // qmtc2.i vf3, a1
  c->vmula_bc(DEST::w, BC::x, vf0, vf2);            // vmulax.w acc, vf0, vf2
  c->vmadda_bc(DEST::w, BC::y, vf0, vf2);           // vmadday.w acc, vf0, vf2
  c->vmadd_bc(DEST::w, BC::z, vf2, vf0, vf2);       // vmaddz.w vf2, vf0, vf2
  c->vrsqrt(vf3, BC::x, vf2, BC::w);                // vrsqrt Q, vf3.x, vf2.w
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyz, vf1, vf1);                    // vmulq.xyz vf1, vf1, Q
  // nop                                            // vnop
  // nop                                            // vnop
  // nop                                            // vnop
  c->sqc2(vf1, 0, a0);                              // sqc2 vf1, 0(a0)
  c->sw(s7, 52, v1);                                // sw s7, 52(v1)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 56, v1);                              // swc1 f0, 56(v1)
  c->addiu(a0, r0, -1);                             // addiu a0, r0, -1
  c->sb(a0, 60, v1);                                // sb a0, 60(v1)
  c->sw(s7, 64, v1);                                // sw s7, 64(v1)
  c->sw(s7, 68, v1);                                // sw s7, 68(v1)
  c->sw(s7, 72, v1);                                // sw s7, 72(v1)
  c->sw(s7, 76, v1);                                // sw s7, 76(v1)
  c->addiu(a0, r0, 3);                              // addiu a0, r0, 3
  c->sb(a0, 61, v1);                                // sb a0, 61(v1)
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->addiu(v1, r0, 1);                              // addiu v1, r0, 1
  c->sb(v1, 61, s4);                                // sb v1, 61(s4)
  
block_4:
  c->daddiu(s2, s2, 1);                             // daddiu s2, s2, 1
  c->mov64(a0, s3);                                 // or a0, s3, r0
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 80, v1);                               // lwu t9, 80(v1)
  c->mov64(a1, s4);                                 // or a1, s4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->slti(v1, s2, 15);                              // slti v1, s2, 15
  c->daddiu(a0, s7, 4);                             // daddiu a0, s7, 4
  c->movn(a0, s7, v1);                              // movn a0, s7, v1
  if (((s64)c->sgpr64(s7)) != ((s64)c->sgpr64(a0))) {// bnel s7, a0, L166
    c->mov64(v1, a0);                               // or v1, a0, r0
    goto block_7;
  }
  
block_6:
  c->lwu(v1, 64, s4);                               // lwu v1, 64(s4)
  
block_7:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L165
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_4;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->lwu(v1, 48, s4);                               // lwu v1, 48(s4)
  c->sw(v1, 16, gp);                                // sw v1, 16(gp)
  c->lwu(v1, 68, s4);                               // lwu v1, 68(s4)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L167
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_11;}                          // branch non-likely

  c->lwu(v1, -4, s3);                               // lwu v1, -4(s3)
  c->lwu(t9, 164, v1);                              // lwu t9, 164(v1)
  c->lwu(a1, 16, gp);                               // lwu a1, 16(gp)
  c->daddiu(a2, s4, 32);                            // daddiu a2, s4, 32
  c->mov64(a0, s3);                                 // or a0, s3, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  bc = c->sgpr64(s7) != c->sgpr64(v1);              // bne s7, v1, L167
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_11;}                          // branch non-likely

  c->sw(s7, 68, s4);                                // sw s7, 68(s4)
  c->mov64(v1, s7);                                 // or v1, s7, r0
  
block_11:
  c->lwu(v1, 68, s4);                               // lwu v1, 68(s4)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L168
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_13;}                          // branch non-likely

  c->lwu(v1, 0, gp);                                // lwu v1, 0(gp)
  c->ori(v1, v1, 2048);                             // ori v1, v1, 2048
  c->sw(v1, 0, gp);                                 // sw v1, 0(gp)
  c->daddiu(v1, gp, 80);                            // daddiu v1, gp, 80
  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  //beq r0, r0, L169                                // beq r0, r0, L169
  // nop                                            // sll r0, r0, 0
  goto block_14;                                    // branch always

  
block_13:
  c->addiu(v1, r0, -2049);                          // addiu v1, r0, -2049
  c->lwu(a0, 0, gp);                                // lwu a0, 0(gp)
  c->and_(v1, v1, a0);                              // and v1, v1, a0
  c->sw(v1, 0, gp);                                 // sw v1, 0(gp)
  c->daddiu(v1, gp, 80);                            // daddiu v1, gp, 80
  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->load_symbol2(v1, cache.nav_find_poly_parms);   // lw v1, nav-find-poly-parms(s7)
  c->lwu(t9, 16, v1);                               // lwu t9, 16(v1)
  c->daddiu(a0, sp, 96);                            // daddiu a0, sp, 96
  c->load_symbol2(a1, cache.nav_find_poly_parms);   // lw a1, nav-find-poly-parms(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(s4, v0);                                 // or s4, v0, r0
  c->daddu(v1, r0, s4);                             // daddu v1, r0, s4
  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->lwu(a1, 12, gp);                               // lwu a1, 12(gp)
  c->daddiu(a1, a1, 44);                            // daddiu a1, a1, 44
  c->lqc2(vf4, 0, a0);                              // lqc2 vf4, 0(a0)
  c->lqc2(vf5, 0, a1);                              // lqc2 vf5, 0(a1)
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, v1);                              // sqc2 vf6, 0(v1)
  c->lwu(v1, 4, gp);                                // lwu v1, 4(gp)
  c->lwc1(f0, 20, v1);                              // lwc1 f0, 20(v1)
  c->swc1(f0, 16, s4);                              // swc1 f0, 16(s4)
  c->addiu(v1, r0, 1);                              // addiu v1, r0, 1
  c->sb(v1, 20, s4);                                // sb v1, 20(s4)
  c->lwu(a0, 12, gp);                               // lwu a0, 12(gp)
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 188, v1);                              // lwu t9, 188(v1)
  c->mov64(a1, s4);                                 // or a1, s4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->lwu(v1, 24, s4);                               // lwu v1, 24(s4)
  c->sw(v1, 16, gp);                                // sw v1, 16(gp)
  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  
block_14:
  //beq r0, r0, L171                                // beq r0, r0, L171
  // nop                                            // sll r0, r0, 0
  goto block_17;                                    // branch always

  
block_15:
  c->daddiu(v1, gp, 80);                            // daddiu v1, gp, 80
  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->load_symbol2(v1, cache.nav_find_poly_parms);   // lw v1, nav-find-poly-parms(s7)
  c->lwu(t9, 16, v1);                               // lwu t9, 16(v1)
  c->daddiu(a0, sp, 144);                           // daddiu a0, sp, 144
  c->load_symbol2(a1, cache.nav_find_poly_parms);   // lw a1, nav-find-poly-parms(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(s4, v0);                                 // or s4, v0, r0
  c->daddu(v1, r0, s4);                             // daddu v1, r0, s4
  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->lwu(a1, 12, gp);                               // lwu a1, 12(gp)
  c->daddiu(a1, a1, 44);                            // daddiu a1, a1, 44
  c->lqc2(vf4, 0, a0);                              // lqc2 vf4, 0(a0)
  c->lqc2(vf5, 0, a1);                              // lqc2 vf5, 0(a1)
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, v1);                              // sqc2 vf6, 0(v1)
  c->lwu(v1, 4, gp);                                // lwu v1, 4(gp)
  c->lwc1(f0, 20, v1);                              // lwc1 f0, 20(v1)
  c->swc1(f0, 16, s4);                              // swc1 f0, 16(s4)
  c->addiu(v1, r0, 1);                              // addiu v1, r0, 1
  c->sb(v1, 20, s4);                                // sb v1, 20(s4)
  c->lwu(a0, 12, gp);                               // lwu a0, 12(gp)
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 188, v1);                              // lwu t9, 188(v1)
  c->mov64(a1, s4);                                 // or a1, s4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->lwu(v1, 24, s4);                               // lwu v1, 24(s4)
  c->sw(v1, 16, gp);                                // sw v1, 16(gp)
  c->addiu(v1, r0, -2049);                          // addiu v1, r0, -2049
  c->lwu(a0, 0, gp);                                // lwu a0, 0(gp)
  c->and_(v1, v1, a0);                              // and v1, v1, a0
  c->sw(v1, 0, gp);                                 // sw v1, 0(gp)
  c->lwu(v1, 32, s4);                               // lwu v1, 32(s4)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L171
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_17;}                          // branch non-likely

  c->lwu(v1, 0, gp);                                // lwu v1, 0(gp)
  c->ori(v1, v1, 2048);                             // ori v1, v1, 2048
  c->sw(v1, 0, gp);                                 // sw v1, 0(gp)
  
block_17:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 256, sp);                               // lq gp, 256(sp)
  c->lq(s5, 240, sp);                               // lq s5, 240(sp)
  c->lq(s4, 224, sp);                               // lq s4, 224(sp)
  c->lq(s3, 208, sp);                               // lq s3, 208(sp)
  c->lq(s2, 192, sp);                               // lq s2, 192(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 272);                           // daddiu sp, sp, 272
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.nav_find_poly_parms = intern_from_c("nav-find-poly-parms").c();
  gLinkedFunctionTable.reg("(method 40 nav-state)", execute, 272);
}

} // namespace method_40_nav_state

namespace method_18_nav_mesh {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -208);                          // daddiu sp, sp, -208
  bc = c->sgpr64(s7) == c->sgpr64(t2);              // beq s7, t2, L200
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_2;}                           // branch non-likely

  c->sw(s7, 0, t2);                                 // sw s7, 0(t2)
  c->sw(s7, 84, t2);                                // sw s7, 84(t2)
  c->mov64(v1, s7);                                 // or v1, s7, r0
  
block_2:
  c->daddiu(v1, sp, 16);                            // daddiu v1, sp, 16
  c->sw(v1, 96, sp);                                // sw v1, 96(sp)
  c->sw(s7, 100, sp);                               // sw s7, 100(sp)
  c->sd(r0, 104, sp);                               // sd r0, 104(sp)
  c->lwu(v1, 0, a0);                                // lwu v1, 0(a0)
  c->sw(v1, 112, sp);                               // sw v1, 112(sp)
  c->lwu(v1, 96, sp);                               // lwu v1, 96(sp)
  c->sw(a2, 48, v1);                                // sw a2, 48(v1)
  c->lwu(v1, 96, sp);                               // lwu v1, 96(sp)
  c->daddu(v1, r0, v1);                             // daddu v1, r0, v1
  c->mov64(t3, a1);                                 // or t3, a1, r0
  c->lq(t3, 0, t3);                                 // lq t3, 0(t3)
  c->sq(t3, 0, v1);                                 // sq t3, 0(v1)
  c->lwu(v1, 96, sp);                               // lwu v1, 96(sp)
  c->daddiu(t4, v1, 32);                            // daddiu t4, v1, 32
  c->mov64(v1, a1);                                 // or v1, a1, r0
  c->mov64(t3, a3);                                 // or t3, a3, r0
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->lqc2(vf4, 0, v1);                              // lqc2 vf4, 0(v1)
  c->lqc2(vf5, 0, t3);                              // lqc2 vf5, 0(t3)
  c->vadd(DEST::xyz, vf6, vf4, vf5);                // vadd.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, t4);                              // sqc2 vf6, 0(t4)
  c->mov64(t6, a0);                                 // or t6, a0, r0
  c->mov64(t4, a2);                                 // or t4, a2, r0
  c->lwu(v1, 96, sp);                               // lwu v1, 96(sp)
  c->daddiu(v1, v1, 32);                            // daddiu v1, v1, 32
  c->lbu(t3, 14, t4);                               // lbu t3, 14(t4)
  c->daddu(t4, r0, t4);                             // daddu t4, r0, t4
  c->lwu(t5, 0, t6);                                // lwu t5, 0(t6)
  c->daddu(t5, r0, t5);                             // daddu t5, r0, t5
  c->lwu(t6, 0, t6);                                // lwu t6, 0(t6)
  c->daddiu(t6, t6, 4);                             // daddiu t6, t6, 4
  c->addiu(t7, r0, 0);                              // addiu t7, r0, 0
  //beq r0, r0, L203                                // beq r0, r0, L203
  // nop                                            // sll r0, r0, 0
  goto block_7;                                     // branch always

  
block_3:
  c->daddu(t8, t5, t7);                             // daddu t8, t5, t7
  c->lb(t8, 0, t8);                                 // lb t8, 0(t8)
  c->dsll(t8, t8, 4);                               // dsll t8, t8, 4
  c->daddu(t8, t4, t8);                             // daddu t8, t4, t8
  c->daddu(t9, t6, t7);                             // daddu t9, t6, t7
  c->lb(t9, 0, t9);                                 // lb t9, 0(t9)
  c->dsll(t9, t9, 4);                               // dsll t9, t9, 4
  c->daddu(t9, t4, t9);                             // daddu t9, t4, t9
  c->lwc1(f0, 8, t8);                               // lwc1 f0, 8(t8)
  c->lwc1(f1, 8, t9);                               // lwc1 f1, 8(t9)
  c->subs(f0, f0, f1);                              // sub.s f0, f0, f1
  c->lwc1(f1, 0, t9);                               // lwc1 f1, 0(t9)
  c->lwc1(f2, 0, t8);                               // lwc1 f2, 0(t8)
  c->subs(f1, f1, f2);                              // sub.s f1, f1, f2
  c->lwc1(f2, 0, v1);                               // lwc1 f2, 0(v1)
  c->lwc1(f3, 0, t8);                               // lwc1 f3, 0(t8)
  c->subs(f2, f2, f3);                              // sub.s f2, f2, f3
  c->lwc1(f3, 8, v1);                               // lwc1 f3, 8(v1)
  c->lwc1(f4, 8, t8);                               // lwc1 f4, 8(t8)
  c->subs(f3, f3, f4);                              // sub.s f3, f3, f4
  c->muls(f0, f2, f0);                              // mul.s f0, f2, f0
  c->muls(f1, f3, f1);                              // mul.s f1, f3, f1
  c->adds(f0, f0, f1);                              // add.s f0, f0, f1
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  cop1_bc = c->fprs[f1] < c->fprs[f0];              // c.lt.s f1, f0
  bc = !cop1_bc;                                    // bc1f L202
  c->mov64(t8, s7);                                 // or t8, s7, r0
  if (bc) {goto block_6;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  //beq r0, r0, L204                                // beq r0, r0, L204
  // nop                                            // sll r0, r0, 0
  goto block_9;                                     // branch always

  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  
block_6:
  c->daddiu(t7, t7, 1);                             // daddiu t7, t7, 1
  
block_7:
  c->slt(t8, t7, t3);                               // slt t8, t7, t3
  bc = c->sgpr64(t8) != 0;                          // bne t8, r0, L201
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_3;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  
block_9:
  bc = c->sgpr64(s7) != c->sgpr64(v1);              // bne s7, v1, L228
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_62;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  
block_11:
  c->sd(r0, 120, sp);                               // sd r0, 120(sp)
  c->lwu(v1, 96, sp);                               // lwu v1, 96(sp)
  c->daddiu(t5, v1, 16);                            // daddiu t5, v1, 16
  c->daddiu(t3, v1, 32);                            // daddiu t3, v1, 32
  c->daddu(t4, r0, v1);                             // daddu t4, r0, v1
  c->lqc2(vf4, 0, t3);                              // lqc2 vf4, 0(t3)
  c->lqc2(vf5, 0, t4);                              // lqc2 vf5, 0(t4)
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, t5);                              // sqc2 vf6, 0(t5)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 20, v1);                              // swc1 f0, 20(v1)
  c->daddiu(t3, v1, 16);                            // daddiu t3, v1, 16
  c->lui(t4, 16256);                                // lui t4, 16256
  c->mtc1(f0, t4);                                  // mtc1 f0, t4
  c->lqc2(vf1, 0, t3);                              // lqc2 vf1, 0(t3)
  c->vmul(DEST::xyz, vf2, vf1, vf1);                // vmul.xyz vf2, vf1, vf1
  c->mfc1(t4, f0);                                  // mfc1 t4, f0
  c->mov128_vf_gpr(vf3, t4);                        // qmtc2.i vf3, t4
  c->vmula_bc(DEST::w, BC::x, vf0, vf2);            // vmulax.w acc, vf0, vf2
  c->vmadda_bc(DEST::w, BC::y, vf0, vf2);           // vmadday.w acc, vf0, vf2
  c->vmadd_bc(DEST::w, BC::z, vf2, vf0, vf2);       // vmaddz.w vf2, vf0, vf2
  c->vrsqrt(vf3, BC::x, vf2, BC::w);                // vrsqrt Q, vf3.x, vf2.w
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyz, vf1, vf1);                    // vmulq.xyz vf1, vf1, Q
  // nop                                            // vnop
  // nop                                            // vnop
  // nop                                            // vnop
  c->sqc2(vf1, 0, t3);                              // sqc2 vf1, 0(t3)
  c->sw(s7, 52, v1);                                // sw s7, 52(v1)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 56, v1);                              // swc1 f0, 56(v1)
  c->addiu(t3, r0, -1);                             // addiu t3, r0, -1
  c->sb(t3, 60, v1);                                // sb t3, 60(v1)
  c->sw(s7, 64, v1);                                // sw s7, 64(v1)
  c->sw(s7, 68, v1);                                // sw s7, 68(v1)
  c->sw(s7, 72, v1);                                // sw s7, 72(v1)
  c->sw(s7, 76, v1);                                // sw s7, 76(v1)
  c->addiu(t3, r0, 3);                              // addiu t3, r0, 3
  c->sb(t3, 61, v1);                                // sb t3, 61(v1)
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  
block_12:
  c->ld(v1, 120, sp);                               // ld v1, 120(sp)
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  c->sd(v1, 120, sp);                               // sd v1, 120(sp)
  c->mov64(t3, a0);                                 // or t3, a0, r0
  c->lwu(v1, 96, sp);                               // lwu v1, 96(sp)
  c->addiu(t4, r0, -1);                             // addiu t4, r0, -1
  c->sd(t4, 128, sp);                               // sd t4, 128(sp)
  c->lwu(t4, 0, t3);                                // lwu t4, 0(t3)
  c->sw(t4, 136, sp);                               // sw t4, 136(sp)
  c->lwu(t4, 48, v1);                               // lwu t4, 48(v1)
  c->sw(t4, 140, sp);                               // sw t4, 140(sp)
  c->lwu(t4, 48, v1);                               // lwu t4, 48(v1)
  c->lbu(t4, 14, t4);                               // lbu t4, 14(t4)
  c->sb(t4, 144, sp);                               // sb t4, 144(sp)
  c->lwu(t4, 0, t3);                                // lwu t4, 0(t3)
  c->daddu(t4, r0, t4);                             // daddu t4, r0, t4
  c->sw(t4, 148, sp);                               // sw t4, 148(sp)
  c->lwu(t4, 0, t3);                                // lwu t4, 0(t3)
  c->daddiu(t4, t4, 4);                             // daddiu t4, t4, 4
  c->sw(t4, 152, sp);                               // sw t4, 152(sp)
  c->lwc1(f0, 32, v1);                              // lwc1 f0, 32(v1)
  c->lwc1(f1, 0, v1);                               // lwc1 f1, 0(v1)
  c->subs(f0, f0, f1);                              // sub.s f0, f0, f1
  c->swc1(f0, 156, sp);                             // swc1 f0, 156(sp)
  c->lwc1(f0, 40, v1);                              // lwc1 f0, 40(v1)
  c->lwc1(f1, 8, v1);                               // lwc1 f1, 8(v1)
  c->subs(f0, f0, f1);                              // sub.s f0, f0, f1
  c->swc1(f0, 160, sp);                             // swc1 f0, 160(sp)
  c->addiu(t4, r0, 0);                              // addiu t4, r0, 0
  //beq r0, r0, L209                                // beq r0, r0, L209
  // nop                                            // sll r0, r0, 0
  goto block_17;                                    // branch always

  
block_13:
  c->lwu(t5, 148, sp);                              // lwu t5, 148(sp)
  c->daddu(t5, t5, t4);                             // daddu t5, t5, t4
  c->lb(t5, 0, t5);                                 // lb t5, 0(t5)
  c->dsll(t5, t5, 4);                               // dsll t5, t5, 4
  c->daddu(t5, r0, t5);                             // daddu t5, r0, t5
  c->lwu(t6, 140, sp);                              // lwu t6, 140(sp)
  c->daddu(t5, t5, t6);                             // daddu t5, t5, t6
  c->sw(t5, 164, sp);                               // sw t5, 164(sp)
  c->lwu(t5, 152, sp);                              // lwu t5, 152(sp)
  c->daddu(t5, t5, t4);                             // daddu t5, t5, t4
  c->lb(t5, 0, t5);                                 // lb t5, 0(t5)
  c->dsll(t5, t5, 4);                               // dsll t5, t5, 4
  c->daddu(t5, r0, t5);                             // daddu t5, r0, t5
  c->lwu(t6, 140, sp);                              // lwu t6, 140(sp)
  c->daddu(t5, t5, t6);                             // daddu t5, t5, t6
  c->sw(t5, 168, sp);                               // sw t5, 168(sp)
  c->lwu(t5, 164, sp);                              // lwu t5, 164(sp)
  c->lwc1(f0, 8, t5);                               // lwc1 f0, 8(t5)
  c->lwu(t5, 168, sp);                              // lwu t5, 168(sp)
  c->lwc1(f1, 8, t5);                               // lwc1 f1, 8(t5)
  c->subs(f0, f0, f1);                              // sub.s f0, f0, f1
  c->swc1(f0, 172, sp);                             // swc1 f0, 172(sp)
  c->lwu(t5, 168, sp);                              // lwu t5, 168(sp)
  c->lwc1(f0, 0, t5);                               // lwc1 f0, 0(t5)
  c->lwu(t5, 164, sp);                              // lwu t5, 164(sp)
  c->lwc1(f1, 0, t5);                               // lwc1 f1, 0(t5)
  c->subs(f0, f0, f1);                              // sub.s f0, f0, f1
  c->swc1(f0, 176, sp);                             // swc1 f0, 176(sp)
  c->lw(t5, 156, sp);                               // lw t5, 156(sp)
  c->mtc1(f0, t5);                                  // mtc1 f0, t5
  c->lw(t5, 172, sp);                               // lw t5, 172(sp)
  c->mtc1(f1, t5);                                  // mtc1 f1, t5
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->lw(t5, 160, sp);                               // lw t5, 160(sp)
  c->mtc1(f1, t5);                                  // mtc1 f1, t5
  c->lw(t5, 176, sp);                               // lw t5, 176(sp)
  c->mtc1(f2, t5);                                  // mtc1 f2, t5
  c->muls(f1, f1, f2);                              // mul.s f1, f1, f2
  c->adds(f0, f0, f1);                              // add.s f0, f0, f1
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  cop1_bc = c->fprs[f1] < c->fprs[f0];              // c.lt.s f1, f0
  bc = !cop1_bc;                                    // bc1f L208
  c->mov64(t5, s7);                                 // or t5, s7, r0
  if (bc) {goto block_16;}                          // branch non-likely

  c->lw(t5, 172, sp);                               // lw t5, 172(sp)
  c->mtc1(f1, t5);                                  // mtc1 f1, t5
  c->lwu(t5, 164, sp);                              // lwu t5, 164(sp)
  c->lwc1(f2, 0, t5);                               // lwc1 f2, 0(t5)
  c->lwc1(f3, 0, v1);                               // lwc1 f3, 0(v1)
  c->subs(f2, f2, f3);                              // sub.s f2, f2, f3
  c->muls(f1, f1, f2);                              // mul.s f1, f1, f2
  c->lw(t5, 176, sp);                               // lw t5, 176(sp)
  c->mtc1(f2, t5);                                  // mtc1 f2, t5
  c->lwu(t5, 164, sp);                              // lwu t5, 164(sp)
  c->lwc1(f3, 8, t5);                               // lwc1 f3, 8(t5)
  c->lwc1(f4, 8, v1);                               // lwc1 f4, 8(v1)
  c->subs(f3, f3, f4);                              // sub.s f3, f3, f4
  c->muls(f2, f2, f3);                              // mul.s f2, f2, f3
  c->adds(f1, f1, f2);                              // add.s f1, f1, f2
  cop1_bc = c->fprs[f1] < c->fprs[f0];              // c.lt.s f1, f0
  bc = !cop1_bc;                                    // bc1f L208
  c->mov64(t5, s7);                                 // or t5, s7, r0
  if (bc) {goto block_16;}                          // branch non-likely

  c->sd(t4, 128, sp);                               // sd t4, 128(sp)
  c->mtc1(f2, r0);                                  // mtc1 f2, r0
  c->divs(f0, f1, f0);                              // div.s f0, f1, f0
  c->maxs(f0, f2, f0);                              // max.s f0, f2, f0
  c->lw(t5, 156, sp);                               // lw t5, 156(sp)
  c->mtc1(f1, t5);                                  // mtc1 f1, t5
  c->muls(f1, f1, f0);                              // mul.s f1, f1, f0
  c->swc1(f1, 156, sp);                             // swc1 f1, 156(sp)
  c->lw(t5, 160, sp);                               // lw t5, 160(sp)
  c->mtc1(f1, t5);                                  // mtc1 f1, t5
  c->muls(f0, f1, f0);                              // mul.s f0, f1, f0
  c->swc1(f0, 160, sp);                             // swc1 f0, 160(sp)
  c->mfc1(t5, f0);                                  // mfc1 t5, f0
  
block_16:
  c->daddiu(t4, t4, 1);                             // daddiu t4, t4, 1
  
block_17:
  c->lbu(t5, 144, sp);                              // lbu t5, 144(sp)
  c->slt(t5, t4, t5);                               // slt t5, t4, t5
  bc = c->sgpr64(t5) != 0;                          // bne t5, r0, L207
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_13;}                          // branch non-likely

  c->mov64(t4, s7);                                 // or t4, s7, r0
  c->mov64(t4, s7);                                 // or t4, s7, r0
  c->lw(t4, 156, sp);                               // lw t4, 156(sp)
  c->mtc1(f0, t4);                                  // mtc1 f0, t4
  c->lwc1(f1, 16, v1);                              // lwc1 f1, 16(v1)
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->lw(t4, 160, sp);                               // lw t4, 160(sp)
  c->mtc1(f1, t4);                                  // mtc1 f1, t4
  c->lwc1(f2, 24, v1);                              // lwc1 f2, 24(v1)
  c->muls(f1, f1, f2);                              // mul.s f1, f1, f2
  c->adds(f0, f0, f1);                              // add.s f0, f0, f1
  c->lwc1(f1, 56, v1);                              // lwc1 f1, 56(v1)
  c->adds(f0, f1, f0);                              // add.s f0, f1, f0
  c->swc1(f0, 56, v1);                              // swc1 f0, 56(v1)
  c->gprs[t4].du64[0] = 0;                          // or t4, r0, r0
  c->sw(s7, 52, v1);                                // sw s7, 52(v1)
  c->addiu(t4, r0, -1);                             // addiu t4, r0, -1
  c->ld(t5, 128, sp);                               // ld t5, 128(sp)
  bc = c->sgpr64(t5) != c->sgpr64(t4);              // bne t5, t4, L210
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_20;}                          // branch non-likely

  c->daddu(t3, r0, v1);                             // daddu t3, r0, v1
  c->daddiu(t4, v1, 32);                            // daddiu t4, v1, 32
  c->lq(t4, 0, t4);                                 // lq t4, 0(t4)
  c->sq(t4, 0, t3);                                 // sq t4, 0(t3)
  c->daddiu(t3, s7, 4);                             // daddiu t3, s7, #t
  c->sw(t3, 68, v1);                                // sw t3, 68(v1)
  c->daddiu(t3, s7, 4);                             // daddiu t3, s7, #t
  c->sw(t3, 64, v1);                                // sw t3, 64(v1)
  //beq r0, r0, L216                                // beq r0, r0, L216
  // nop                                            // sll r0, r0, 0
  goto block_31;                                    // branch always

  
block_20:
  c->lwc1(f0, 0, v1);                               // lwc1 f0, 0(v1)
  c->lw(t4, 156, sp);                               // lw t4, 156(sp)
  c->mtc1(f1, t4);                                  // mtc1 f1, t4
  c->adds(f0, f0, f1);                              // add.s f0, f0, f1
  c->swc1(f0, 0, v1);                               // swc1 f0, 0(v1)
  c->lwc1(f0, 8, v1);                               // lwc1 f0, 8(v1)
  c->lw(t4, 160, sp);                               // lw t4, 160(sp)
  c->mtc1(f1, t4);                                  // mtc1 f1, t4
  c->adds(f0, f0, f1);                              // add.s f0, f0, f1
  c->swc1(f0, 8, v1);                               // swc1 f0, 8(v1)
  c->ld(t4, 128, sp);                               // ld t4, 128(sp)
  c->lwu(t5, 140, sp);                              // lwu t5, 140(sp)
  c->daddu(t4, t4, t5);                             // daddu t4, t4, t5
  c->lbu(t4, 28, t4);                               // lbu t4, 28(t4)
  c->sb(t4, 180, sp);                               // sb t4, 180(sp)
  c->addiu(t4, r0, 255);                            // addiu t4, r0, 255
  c->lbu(t5, 180, sp);                              // lbu t5, 180(sp)
  bc = c->sgpr64(t5) == c->sgpr64(t4);              // beq t5, t4, L211
  c->mov64(t4, s7);                                 // or t4, s7, r0
  if (bc) {goto block_22;}                          // branch non-likely

  c->lwu(t3, 4, t3);                                // lwu t3, 4(t3)
  c->lbu(t4, 180, sp);                              // lbu t4, 180(sp)
  c->dsll(t4, t4, 6);                               // dsll t4, t4, 6
  c->daddu(t3, t3, t4);                             // daddu t3, t3, t4
  c->sw(t3, 52, v1);                                // sw t3, 52(v1)
  c->mov64(t3, s7);                                 // or t3, s7, r0
  
block_22:
  c->lwu(t3, 52, v1);                               // lwu t3, 52(v1)
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(t3))) {// beql s7, t3, L212
    c->mov64(t3, t3);                               // or t3, t3, r0
    goto block_25;
  }
  
block_24:
  c->daddiu(t3, s7, 4);                             // daddiu t3, s7, 4
  c->lwu(t4, 52, v1);                               // lwu t4, 52(v1)
  c->lbu(t4, 13, t4);                               // lbu t4, 13(t4)
  c->lbu(t5, 61, v1);                               // lbu t5, 61(v1)
  c->and_(t4, t4, t5);                              // and t4, t4, t5
  c->movn(t3, s7, t4);                              // movn t3, s7, t4
  
block_25:
  bc = c->sgpr64(s7) == c->sgpr64(t3);              // beq s7, t3, L213
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_27;}                          // branch non-likely

  c->lwu(t3, 52, v1);                               // lwu t3, 52(v1)
  c->sw(t3, 48, v1);                                // sw t3, 48(v1)
  //beq r0, r0, L216                                // beq r0, r0, L216
  // nop                                            // sll r0, r0, 0
  goto block_31;                                    // branch always

  
block_27:
  c->ld(t3, 128, sp);                               // ld t3, 128(sp)
  c->sb(t3, 60, v1);                                // sb t3, 60(v1)
  c->lwu(t3, 52, v1);                               // lwu t3, 52(v1)
  bc = c->sgpr64(s7) == c->sgpr64(t3);              // beq s7, t3, L214
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_29;}                          // branch non-likely

  c->daddiu(t3, s7, 4);                             // daddiu t3, s7, #t
  c->sw(t3, 76, v1);                                // sw t3, 76(v1)
  //beq r0, r0, L215                                // beq r0, r0, L215
  // nop                                            // sll r0, r0, 0
  goto block_30;                                    // branch always

  
block_29:
  c->daddiu(t3, s7, 4);                             // daddiu t3, s7, #t
  c->sw(t3, 72, v1);                                // sw t3, 72(v1)
  
block_30:
  c->daddiu(t3, s7, 4);                             // daddiu t3, s7, #t
  c->sw(t3, 64, v1);                                // sw t3, 64(v1)
  
block_31:
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->ld(v1, 120, sp);                               // ld v1, 120(sp)
  c->slti(v1, v1, 15);                              // slti v1, v1, 15
  c->daddiu(t3, s7, 4);                             // daddiu t3, s7, 4
  c->movn(t3, s7, v1);                              // movn t3, s7, v1
  if (((s64)c->sgpr64(s7)) != ((s64)c->sgpr64(t3))) {// bnel s7, t3, L218
    c->mov64(v1, t3);                               // or v1, t3, r0
    goto block_38;
  }
  
block_33:
  c->lwu(v1, 96, sp);                               // lwu v1, 96(sp)
  c->lwc1(f0, 56, v1);                              // lwc1 f0, 56(v1)
  c->lui(v1, 18080);                                // lui v1, 18080
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->mtc1(f2, t0);                                  // mtc1 f2, t0
  c->maxs(f1, f1, f2);                              // max.s f1, f1, f2
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = !cop1_bc;                                    // bc1f L217
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  if (bc) {goto block_35;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  
block_35:
  if (((s64)c->sgpr64(s7)) != ((s64)c->sgpr64(v1))) {// bnel s7, v1, L218
    c->mov64(v1, v1);                               // or v1, v1, r0
    goto block_38;
  }
  
block_37:
  c->lwu(v1, 96, sp);                               // lwu v1, 96(sp)
  c->lwu(v1, 64, v1);                               // lwu v1, 64(v1)
  
block_38:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L206
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->lwu(v1, 96, sp);                               // lwu v1, 96(sp)
  c->lwu(v1, 72, v1);                               // lwu v1, 72(v1)
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(v1))) {// beql s7, v1, L220
    c->mov64(v1, v1);                               // or v1, v1, r0
    goto block_48;
  }
  
block_41:
  c->lwu(v1, 96, sp);                               // lwu v1, 96(sp)
  c->lwc1(f0, 56, v1);                              // lwc1 f0, 56(v1)
  c->mtc1(f1, t0);                                  // mtc1 f1, t0
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = cop1_bc;                                     // bc1t L219
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  if (bc) {goto block_43;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  
block_43:
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(v1))) {// beql s7, v1, L220
    c->mov64(v1, v1);                               // or v1, v1, r0
    goto block_48;
  }
  
block_45:
  c->lwu(v1, 96, sp);                               // lwu v1, 96(sp)
  c->lb(v1, 60, v1);                                // lb v1, 60(v1)
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  c->daddiu(t3, s7, 4);                             // daddiu t3, s7, 4
  c->movz(t3, s7, v1);                              // movz t3, s7, v1
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(t3))) {// beql s7, t3, L220
    c->mov64(v1, t3);                               // or v1, t3, r0
    goto block_48;
  }
  
block_47:
  c->ld(v1, 104, sp);                               // ld v1, 104(sp)
  c->slti(t3, v1, 1);                               // slti t3, v1, 1
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  c->movz(v1, s7, t3);                              // movz v1, s7, t3
  
block_48:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L224
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_55;}                          // branch non-likely

  c->ld(v1, 104, sp);                               // ld v1, 104(sp)
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  c->sd(v1, 104, sp);                               // sd v1, 104(sp)
  c->lwu(v1, 96, sp);                               // lwu v1, 96(sp)
  c->lb(v1, 60, v1);                                // lb v1, 60(v1)
  c->lwu(t3, 112, sp);                              // lwu t3, 112(sp)
  c->daddu(v1, v1, t3);                             // daddu v1, v1, t3
  c->lb(v1, 0, v1);                                 // lb v1, 0(v1)
  c->dsll(v1, v1, 4);                               // dsll v1, v1, 4
  c->daddu(v1, r0, v1);                             // daddu v1, r0, v1
  c->daddu(v1, v1, a2);                             // daddu v1, v1, a2
  c->sw(v1, 184, sp);                               // sw v1, 184(sp)
  c->lwu(v1, 96, sp);                               // lwu v1, 96(sp)
  c->lb(v1, 60, v1);                                // lb v1, 60(v1)
  c->lwu(t3, 112, sp);                              // lwu t3, 112(sp)
  c->daddu(v1, v1, t3);                             // daddu v1, v1, t3
  c->lb(v1, 4, v1);                                 // lb v1, 4(v1)
  c->dsll(v1, v1, 4);                               // dsll v1, v1, 4
  c->daddu(v1, r0, v1);                             // daddu v1, r0, v1
  c->daddu(v1, v1, a2);                             // daddu v1, v1, a2
  c->sw(v1, 188, sp);                               // sw v1, 188(sp)
  c->lwu(v1, 184, sp);                              // lwu v1, 184(sp)
  c->lwc1(f0, 8, v1);                               // lwc1 f0, 8(v1)
  c->lwu(v1, 188, sp);                              // lwu v1, 188(sp)
  c->lwc1(f1, 8, v1);                               // lwc1 f1, 8(v1)
  c->subs(f0, f0, f1);                              // sub.s f0, f0, f1
  c->swc1(f0, 192, sp);                             // swc1 f0, 192(sp)
  c->lwu(v1, 188, sp);                              // lwu v1, 188(sp)
  c->lwc1(f0, 0, v1);                               // lwc1 f0, 0(v1)
  c->lwu(v1, 184, sp);                              // lwu v1, 184(sp)
  c->lwc1(f1, 0, v1);                               // lwc1 f1, 0(v1)
  c->subs(f0, f0, f1);                              // sub.s f0, f0, f1
  c->swc1(f0, 196, sp);                             // swc1 f0, 196(sp)
  c->lwc1(f0, 0, a3);                               // lwc1 f0, 0(a3)
  c->swc1(f0, 200, sp);                             // swc1 f0, 200(sp)
  c->lwc1(f0, 8, a3);                               // lwc1 f0, 8(a3)
  c->swc1(f0, 204, sp);                             // swc1 f0, 204(sp)
  c->lwc1(f0, 192, sp);                             // lwc1 f0, 192(sp)
  c->muls(f0, f0, f0);                              // mul.s f0, f0, f0
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lwc1(f1, 196, sp);                             // lwc1 f1, 196(sp)
  c->muls(f1, f1, f1);                              // mul.s f1, f1, f1
  c->mfc1(v1, f1);                                  // mfc1 v1, f1
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->adds(f0, f0, f1);                              // add.s f0, f0, f1
  c->sqrts(f0, f0);                                 // sqrt.s f0, f0
  c->lui(v1, 16256);                                // lui v1, 16256
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->divs(f0, f1, f0);                              // div.s f0, f1, f0
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lw(v1, 192, sp);                               // lw v1, 192(sp)
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->muls(f1, f1, f0);                              // mul.s f1, f1, f0
  c->swc1(f1, 192, sp);                             // swc1 f1, 192(sp)
  c->lw(v1, 196, sp);                               // lw v1, 196(sp)
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->muls(f0, f1, f0);                              // mul.s f0, f1, f0
  c->swc1(f0, 196, sp);                             // swc1 f0, 196(sp)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  bc = c->sgpr64(s7) == c->sgpr64(t2);              // beq s7, t2, L221
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_51;}                          // branch non-likely

  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  c->sw(v1, 0, t2);                                 // sw v1, 0(t2)
  c->daddiu(v1, t2, 16);                            // daddiu v1, t2, 16
  c->lwu(t3, 96, sp);                               // lwu t3, 96(sp)
  c->daddu(t3, r0, t3);                             // daddu t3, r0, t3
  c->daddiu(t4, a0, 44);                            // daddiu t4, a0, 44
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->lqc2(vf4, 0, t3);                              // lqc2 vf4, 0(t3)
  c->lqc2(vf5, 0, t4);                              // lqc2 vf5, 0(t4)
  c->vadd(DEST::xyz, vf6, vf4, vf5);                // vadd.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, v1);                              // sqc2 vf6, 0(v1)
  c->lw(v1, 192, sp);                               // lw v1, 192(sp)
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->swc1(f0, 32, t2);                              // swc1 f0, 32(t2)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 36, t2);                              // swc1 f0, 36(t2)
  c->lw(v1, 196, sp);                               // lw v1, 196(sp)
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->swc1(f0, 40, t2);                              // swc1 f0, 40(t2)
  c->lwu(v1, 96, sp);                               // lwu v1, 96(sp)
  c->lwu(v1, 48, v1);                               // lwu v1, 48(v1)
  c->sw(v1, 80, t2);                                // sw v1, 80(t2)
  c->lwu(v1, 96, sp);                               // lwu v1, 96(sp)
  c->lb(v1, 60, v1);                                // lb v1, 60(v1)
  c->sb(v1, 88, t2);                                // sb v1, 88(t2)
  c->daddiu(t4, t2, 112);                           // daddiu t4, t2, 112
  c->lwu(v1, 184, sp);                              // lwu v1, 184(sp)
  c->daddiu(t3, a0, 44);                            // daddiu t3, a0, 44
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->lqc2(vf4, 0, v1);                              // lqc2 vf4, 0(v1)
  c->lqc2(vf5, 0, t3);                              // lqc2 vf5, 0(t3)
  c->vadd(DEST::xyz, vf6, vf4, vf5);                // vadd.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, t4);                              // sqc2 vf6, 0(t4)
  c->daddiu(t4, t2, 128);                           // daddiu t4, t2, 128
  c->lwu(v1, 188, sp);                              // lwu v1, 188(sp)
  c->daddiu(t3, a0, 44);                            // daddiu t3, a0, 44
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->lqc2(vf4, 0, v1);                              // lqc2 vf4, 0(v1)
  c->lqc2(vf5, 0, t3);                              // lqc2 vf5, 0(t3)
  c->vadd(DEST::xyz, vf6, vf4, vf5);                // vadd.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, t4);                              // sqc2 vf6, 0(t4)
  
block_51:
  c->lwu(v1, 96, sp);                               // lwu v1, 96(sp)
  c->daddiu(v1, v1, 32);                            // daddiu v1, v1, 32
  c->lwu(t3, 96, sp);                               // lwu t3, 96(sp)
  c->daddu(t3, r0, t3);                             // daddu t3, r0, t3
  c->lq(t3, 0, t3);                                 // lq t3, 0(t3)
  c->sq(t3, 0, v1);                                 // sq t3, 0(v1)
  bc = c->sgpr64(s7) == c->sgpr64(t1);              // beq s7, t1, L222
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_53;}                          // branch non-likely

  c->lui(v1, 16257);                                // lui v1, 16257
  c->ori(v1, v1, 18350);                            // ori v1, v1, 18350
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lw(v1, 192, sp);                               // lw v1, 192(sp)
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->lw(v1, 200, sp);                               // lw v1, 200(sp)
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  c->muls(f1, f1, f2);                              // mul.s f1, f1, f2
  c->lw(v1, 196, sp);                               // lw v1, 196(sp)
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  c->lw(v1, 204, sp);                               // lw v1, 204(sp)
  c->mtc1(f3, v1);                                  // mtc1 f3, v1
  c->muls(f2, f2, f3);                              // mul.s f2, f2, f3
  c->adds(f1, f1, f2);                              // add.s f1, f1, f2
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->lw(v1, 200, sp);                               // lw v1, 200(sp)
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->lw(v1, 192, sp);                               // lw v1, 192(sp)
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  c->muls(f2, f2, f0);                              // mul.s f2, f2, f0
  c->subs(f1, f1, f2);                              // sub.s f1, f1, f2
  c->swc1(f1, 200, sp);                             // swc1 f1, 200(sp)
  c->lw(v1, 204, sp);                               // lw v1, 204(sp)
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->lw(v1, 196, sp);                               // lw v1, 196(sp)
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  c->muls(f0, f2, f0);                              // mul.s f0, f2, f0
  c->subs(f0, f1, f0);                              // sub.s f0, f1, f0
  c->swc1(f0, 204, sp);                             // swc1 f0, 204(sp)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->lwu(v1, 96, sp);                               // lwu v1, 96(sp)
  c->lwc1(f0, 32, v1);                              // lwc1 f0, 32(v1)
  c->lw(v1, 200, sp);                               // lw v1, 200(sp)
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->adds(f0, f0, f1);                              // add.s f0, f0, f1
  c->lwu(v1, 96, sp);                               // lwu v1, 96(sp)
  c->swc1(f0, 32, v1);                              // swc1 f0, 32(v1)
  c->lwu(v1, 96, sp);                               // lwu v1, 96(sp)
  c->lwc1(f0, 40, v1);                              // lwc1 f0, 40(v1)
  c->lw(v1, 204, sp);                               // lw v1, 204(sp)
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->adds(f0, f0, f1);                              // add.s f0, f0, f1
  c->lwu(v1, 96, sp);                               // lwu v1, 96(sp)
  c->swc1(f0, 40, v1);                              // swc1 f0, 40(v1)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  //beq r0, r0, L223                                // beq r0, r0, L223
  // nop                                            // sll r0, r0, 0
  goto block_54;                                    // branch always

  
block_53:
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  c->sw(v1, 100, sp);                               // sw v1, 100(sp)
  
block_54:
  //beq r0, r0, L227                                // beq r0, r0, L227
  // nop                                            // sll r0, r0, 0
  goto block_60;                                    // branch always

  
block_55:
  c->lwu(v1, 96, sp);                               // lwu v1, 96(sp)
  c->lwu(v1, 76, v1);                               // lwu v1, 76(v1)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L226
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_59;}                          // branch non-likely

  bc = c->sgpr64(s7) == c->sgpr64(t2);              // beq s7, t2, L225
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_58;}                          // branch non-likely

  c->lwu(v1, 96, sp);                               // lwu v1, 96(sp)
  c->lwu(v1, 52, v1);                               // lwu v1, 52(v1)
  c->sw(v1, 84, t2);                                // sw v1, 84(t2)
  
block_58:
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  c->sw(v1, 100, sp);                               // sw v1, 100(sp)
  //beq r0, r0, L227                                // beq r0, r0, L227
  // nop                                            // sll r0, r0, 0
  goto block_60;                                    // branch always

  
block_59:
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  c->sw(v1, 100, sp);                               // sw v1, 100(sp)
  
block_60:
  c->lwu(v1, 100, sp);                              // lwu v1, 100(sp)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L205
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_11;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, a3);                                 // or v1, a3, r0
  c->lwu(a0, 96, sp);                               // lwu a0, 96(sp)
  c->daddu(a0, r0, a0);                             // daddu a0, r0, a0
  c->lqc2(vf4, 0, a0);                              // lqc2 vf4, 0(a0)
  c->lqc2(vf5, 0, a1);                              // lqc2 vf5, 0(a1)
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, v1);                              // sqc2 vf6, 0(v1)
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  
block_62:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 208);                           // daddiu sp, sp, 208
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  gLinkedFunctionTable.reg("(method 18 nav-mesh)", execute, 208);
}

} // namespace method_18_nav_mesh

namespace method_19_nav_mesh {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -224);                          // daddiu sp, sp, -224
  c->sw(s7, 0, t0);                                 // sw s7, 0(t0)
  c->sw(s7, 84, t0);                                // sw s7, 84(t0)
  c->daddiu(v1, sp, 16);                            // daddiu v1, sp, 16
  c->sw(v1, 112, sp);                               // sw v1, 112(sp)
  c->daddiu(v1, sp, 96);                            // daddiu v1, sp, 96
  c->sw(v1, 116, sp);                               // sw v1, 116(sp)
  c->sw(s7, 120, sp);                               // sw s7, 120(sp)
  c->lwu(v1, 0, a0);                                // lwu v1, 0(a0)
  c->sw(v1, 124, sp);                               // sw v1, 124(sp)
  c->lwu(v1, 116, sp);                              // lwu v1, 116(sp)
  c->daddiu(t1, a0, 44);                            // daddiu t1, a0, 44
  c->lqc2(vf4, 0, a1);                              // lqc2 vf4, 0(a1)
  c->lqc2(vf5, 0, t1);                              // lqc2 vf5, 0(t1)
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, v1);                              // sqc2 vf6, 0(v1)
  c->lwu(v1, 112, sp);                              // lwu v1, 112(sp)
  c->sw(a2, 48, v1);                                // sw a2, 48(v1)
  c->lwu(v1, 112, sp);                              // lwu v1, 112(sp)
  c->daddu(v1, r0, v1);                             // daddu v1, r0, v1
  c->lwu(a1, 116, sp);                              // lwu a1, 116(sp)
  c->lq(a1, 0, a1);                                 // lq a1, 0(a1)
  c->sq(a1, 0, v1);                                 // sq a1, 0(v1)
  c->lwu(v1, 112, sp);                              // lwu v1, 112(sp)
  c->daddiu(t1, v1, 32);                            // daddiu t1, v1, 32
  c->lwu(v1, 116, sp);                              // lwu v1, 116(sp)
  c->mov64(a1, a3);                                 // or a1, a3, r0
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->lqc2(vf4, 0, v1);                              // lqc2 vf4, 0(v1)
  c->lqc2(vf5, 0, a1);                              // lqc2 vf5, 0(a1)
  c->vadd(DEST::xyz, vf6, vf4, vf5);                // vadd.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, t1);                              // sqc2 vf6, 0(t1)
  c->mov64(t2, a0);                                 // or t2, a0, r0
  c->lwu(v1, 112, sp);                              // lwu v1, 112(sp)
  c->daddiu(v1, v1, 32);                            // daddiu v1, v1, 32
  c->lbu(a1, 14, a2);                               // lbu a1, 14(a2)
  c->daddu(a2, r0, a2);                             // daddu a2, r0, a2
  c->lwu(t1, 0, t2);                                // lwu t1, 0(t2)
  c->daddu(t1, r0, t1);                             // daddu t1, r0, t1
  c->lwu(t2, 0, t2);                                // lwu t2, 0(t2)
  c->daddiu(t2, t2, 4);                             // daddiu t2, t2, 4
  c->addiu(t3, r0, 0);                              // addiu t3, r0, 0
  //beq r0, r0, L245                                // beq r0, r0, L245
  // nop                                            // sll r0, r0, 0
  goto block_5;                                     // branch always

  
block_1:
  c->daddu(t4, t1, t3);                             // daddu t4, t1, t3
  c->lb(t4, 0, t4);                                 // lb t4, 0(t4)
  c->dsll(t4, t4, 4);                               // dsll t4, t4, 4
  c->daddu(t4, a2, t4);                             // daddu t4, a2, t4
  c->daddu(t5, t2, t3);                             // daddu t5, t2, t3
  c->lb(t5, 0, t5);                                 // lb t5, 0(t5)
  c->dsll(t5, t5, 4);                               // dsll t5, t5, 4
  c->daddu(t5, a2, t5);                             // daddu t5, a2, t5
  c->lwc1(f0, 8, t4);                               // lwc1 f0, 8(t4)
  c->lwc1(f1, 8, t5);                               // lwc1 f1, 8(t5)
  c->subs(f0, f0, f1);                              // sub.s f0, f0, f1
  c->lwc1(f1, 0, t5);                               // lwc1 f1, 0(t5)
  c->lwc1(f2, 0, t4);                               // lwc1 f2, 0(t4)
  c->subs(f1, f1, f2);                              // sub.s f1, f1, f2
  c->lwc1(f2, 0, v1);                               // lwc1 f2, 0(v1)
  c->lwc1(f3, 0, t4);                               // lwc1 f3, 0(t4)
  c->subs(f2, f2, f3);                              // sub.s f2, f2, f3
  c->lwc1(f3, 8, v1);                               // lwc1 f3, 8(v1)
  c->lwc1(f4, 8, t4);                               // lwc1 f4, 8(t4)
  c->subs(f3, f3, f4);                              // sub.s f3, f3, f4
  c->muls(f0, f2, f0);                              // mul.s f0, f2, f0
  c->muls(f1, f3, f1);                              // mul.s f1, f3, f1
  c->adds(f0, f0, f1);                              // add.s f0, f0, f1
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  cop1_bc = c->fprs[f1] < c->fprs[f0];              // c.lt.s f1, f0
  bc = !cop1_bc;                                    // bc1f L244
  c->mov64(t4, s7);                                 // or t4, s7, r0
  if (bc) {goto block_4;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  //beq r0, r0, L246                                // beq r0, r0, L246
  // nop                                            // sll r0, r0, 0
  goto block_7;                                     // branch always

  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  
block_4:
  c->daddiu(t3, t3, 1);                             // daddiu t3, t3, 1
  
block_5:
  c->slt(t4, t3, a1);                               // slt t4, t3, a1
  bc = c->sgpr64(t4) != 0;                          // bne t4, r0, L243
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  
block_7:
  bc = c->sgpr64(s7) != c->sgpr64(v1);              // bne s7, v1, L263
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_41;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->sd(r0, 128, sp);                               // sd r0, 128(sp)
  c->lwu(v1, 112, sp);                              // lwu v1, 112(sp)
  c->daddiu(t1, v1, 16);                            // daddiu t1, v1, 16
  c->daddiu(a1, v1, 32);                            // daddiu a1, v1, 32
  c->daddu(a2, r0, v1);                             // daddu a2, r0, v1
  c->lqc2(vf4, 0, a1);                              // lqc2 vf4, 0(a1)
  c->lqc2(vf5, 0, a2);                              // lqc2 vf5, 0(a2)
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, t1);                              // sqc2 vf6, 0(t1)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 20, v1);                              // swc1 f0, 20(v1)
  c->daddiu(a1, v1, 16);                            // daddiu a1, v1, 16
  c->lui(a2, 16256);                                // lui a2, 16256
  c->mtc1(f0, a2);                                  // mtc1 f0, a2
  c->lqc2(vf1, 0, a1);                              // lqc2 vf1, 0(a1)
  c->vmul(DEST::xyz, vf2, vf1, vf1);                // vmul.xyz vf2, vf1, vf1
  c->mfc1(a2, f0);                                  // mfc1 a2, f0
  c->mov128_vf_gpr(vf3, a2);                        // qmtc2.i vf3, a2
  c->vmula_bc(DEST::w, BC::x, vf0, vf2);            // vmulax.w acc, vf0, vf2
  c->vmadda_bc(DEST::w, BC::y, vf0, vf2);           // vmadday.w acc, vf0, vf2
  c->vmadd_bc(DEST::w, BC::z, vf2, vf0, vf2);       // vmaddz.w vf2, vf0, vf2
  c->vrsqrt(vf3, BC::x, vf2, BC::w);                // vrsqrt Q, vf3.x, vf2.w
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyz, vf1, vf1);                    // vmulq.xyz vf1, vf1, Q
  // nop                                            // vnop
  // nop                                            // vnop
  // nop                                            // vnop
  c->sqc2(vf1, 0, a1);                              // sqc2 vf1, 0(a1)
  c->sw(s7, 52, v1);                                // sw s7, 52(v1)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 56, v1);                              // swc1 f0, 56(v1)
  c->addiu(a1, r0, -1);                             // addiu a1, r0, -1
  c->sb(a1, 60, v1);                                // sb a1, 60(v1)
  c->sw(s7, 64, v1);                                // sw s7, 64(v1)
  c->sw(s7, 68, v1);                                // sw s7, 68(v1)
  c->sw(s7, 72, v1);                                // sw s7, 72(v1)
  c->sw(s7, 76, v1);                                // sw s7, 76(v1)
  c->addiu(a1, r0, 3);                              // addiu a1, r0, 3
  c->sb(a1, 61, v1);                                // sb a1, 61(v1)
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  
block_9:
  c->ld(v1, 128, sp);                               // ld v1, 128(sp)
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  c->sd(v1, 128, sp);                               // sd v1, 128(sp)
  c->mov64(a1, a0);                                 // or a1, a0, r0
  c->lwu(v1, 112, sp);                              // lwu v1, 112(sp)
  c->addiu(a2, r0, -1);                             // addiu a2, r0, -1
  c->sd(a2, 136, sp);                               // sd a2, 136(sp)
  c->lwu(a2, 0, a1);                                // lwu a2, 0(a1)
  c->sw(a2, 144, sp);                               // sw a2, 144(sp)
  c->lwu(a2, 48, v1);                               // lwu a2, 48(v1)
  c->sw(a2, 148, sp);                               // sw a2, 148(sp)
  c->lwu(a2, 48, v1);                               // lwu a2, 48(v1)
  c->lbu(a2, 14, a2);                               // lbu a2, 14(a2)
  c->sb(a2, 152, sp);                               // sb a2, 152(sp)
  c->lwu(a2, 0, a1);                                // lwu a2, 0(a1)
  c->daddu(a2, r0, a2);                             // daddu a2, r0, a2
  c->sw(a2, 156, sp);                               // sw a2, 156(sp)
  c->lwu(a2, 0, a1);                                // lwu a2, 0(a1)
  c->daddiu(a2, a2, 4);                             // daddiu a2, a2, 4
  c->sw(a2, 160, sp);                               // sw a2, 160(sp)
  c->lwc1(f0, 32, v1);                              // lwc1 f0, 32(v1)
  c->lwc1(f1, 0, v1);                               // lwc1 f1, 0(v1)
  c->subs(f0, f0, f1);                              // sub.s f0, f0, f1
  c->swc1(f0, 164, sp);                             // swc1 f0, 164(sp)
  c->lwc1(f0, 40, v1);                              // lwc1 f0, 40(v1)
  c->lwc1(f1, 8, v1);                               // lwc1 f1, 8(v1)
  c->subs(f0, f0, f1);                              // sub.s f0, f0, f1
  c->swc1(f0, 168, sp);                             // swc1 f0, 168(sp)
  c->addiu(a2, r0, 0);                              // addiu a2, r0, 0
  //beq r0, r0, L250                                // beq r0, r0, L250
  // nop                                            // sll r0, r0, 0
  goto block_14;                                    // branch always

  
block_10:
  c->lwu(t1, 156, sp);                              // lwu t1, 156(sp)
  c->daddu(t1, t1, a2);                             // daddu t1, t1, a2
  c->lb(t1, 0, t1);                                 // lb t1, 0(t1)
  c->dsll(t1, t1, 4);                               // dsll t1, t1, 4
  c->daddu(t1, r0, t1);                             // daddu t1, r0, t1
  c->lwu(t2, 148, sp);                              // lwu t2, 148(sp)
  c->daddu(t1, t1, t2);                             // daddu t1, t1, t2
  c->sw(t1, 172, sp);                               // sw t1, 172(sp)
  c->lwu(t1, 160, sp);                              // lwu t1, 160(sp)
  c->daddu(t1, t1, a2);                             // daddu t1, t1, a2
  c->lb(t1, 0, t1);                                 // lb t1, 0(t1)
  c->dsll(t1, t1, 4);                               // dsll t1, t1, 4
  c->daddu(t1, r0, t1);                             // daddu t1, r0, t1
  c->lwu(t2, 148, sp);                              // lwu t2, 148(sp)
  c->daddu(t1, t1, t2);                             // daddu t1, t1, t2
  c->sw(t1, 176, sp);                               // sw t1, 176(sp)
  c->lwu(t1, 172, sp);                              // lwu t1, 172(sp)
  c->lwc1(f0, 8, t1);                               // lwc1 f0, 8(t1)
  c->lwu(t1, 176, sp);                              // lwu t1, 176(sp)
  c->lwc1(f1, 8, t1);                               // lwc1 f1, 8(t1)
  c->subs(f0, f0, f1);                              // sub.s f0, f0, f1
  c->swc1(f0, 180, sp);                             // swc1 f0, 180(sp)
  c->lwu(t1, 176, sp);                              // lwu t1, 176(sp)
  c->lwc1(f0, 0, t1);                               // lwc1 f0, 0(t1)
  c->lwu(t1, 172, sp);                              // lwu t1, 172(sp)
  c->lwc1(f1, 0, t1);                               // lwc1 f1, 0(t1)
  c->subs(f0, f0, f1);                              // sub.s f0, f0, f1
  c->swc1(f0, 184, sp);                             // swc1 f0, 184(sp)
  c->lw(t1, 164, sp);                               // lw t1, 164(sp)
  c->mtc1(f0, t1);                                  // mtc1 f0, t1
  c->lw(t1, 180, sp);                               // lw t1, 180(sp)
  c->mtc1(f1, t1);                                  // mtc1 f1, t1
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->lw(t1, 168, sp);                               // lw t1, 168(sp)
  c->mtc1(f1, t1);                                  // mtc1 f1, t1
  c->lw(t1, 184, sp);                               // lw t1, 184(sp)
  c->mtc1(f2, t1);                                  // mtc1 f2, t1
  c->muls(f1, f1, f2);                              // mul.s f1, f1, f2
  c->adds(f0, f0, f1);                              // add.s f0, f0, f1
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  cop1_bc = c->fprs[f1] < c->fprs[f0];              // c.lt.s f1, f0
  bc = !cop1_bc;                                    // bc1f L249
  c->mov64(t1, s7);                                 // or t1, s7, r0
  if (bc) {goto block_13;}                          // branch non-likely

  c->lw(t1, 180, sp);                               // lw t1, 180(sp)
  c->mtc1(f1, t1);                                  // mtc1 f1, t1
  c->lwu(t1, 172, sp);                              // lwu t1, 172(sp)
  c->lwc1(f2, 0, t1);                               // lwc1 f2, 0(t1)
  c->lwc1(f3, 0, v1);                               // lwc1 f3, 0(v1)
  c->subs(f2, f2, f3);                              // sub.s f2, f2, f3
  c->muls(f1, f1, f2);                              // mul.s f1, f1, f2
  c->lw(t1, 184, sp);                               // lw t1, 184(sp)
  c->mtc1(f2, t1);                                  // mtc1 f2, t1
  c->lwu(t1, 172, sp);                              // lwu t1, 172(sp)
  c->lwc1(f3, 8, t1);                               // lwc1 f3, 8(t1)
  c->lwc1(f4, 8, v1);                               // lwc1 f4, 8(v1)
  c->subs(f3, f3, f4);                              // sub.s f3, f3, f4
  c->muls(f2, f2, f3);                              // mul.s f2, f2, f3
  c->adds(f1, f1, f2);                              // add.s f1, f1, f2
  cop1_bc = c->fprs[f1] < c->fprs[f0];              // c.lt.s f1, f0
  bc = !cop1_bc;                                    // bc1f L249
  c->mov64(t1, s7);                                 // or t1, s7, r0
  if (bc) {goto block_13;}                          // branch non-likely

  c->sd(a2, 136, sp);                               // sd a2, 136(sp)
  c->mtc1(f2, r0);                                  // mtc1 f2, r0
  c->divs(f0, f1, f0);                              // div.s f0, f1, f0
  c->maxs(f0, f2, f0);                              // max.s f0, f2, f0
  c->lw(t1, 164, sp);                               // lw t1, 164(sp)
  c->mtc1(f1, t1);                                  // mtc1 f1, t1
  c->muls(f1, f1, f0);                              // mul.s f1, f1, f0
  c->swc1(f1, 164, sp);                             // swc1 f1, 164(sp)
  c->lw(t1, 168, sp);                               // lw t1, 168(sp)
  c->mtc1(f1, t1);                                  // mtc1 f1, t1
  c->muls(f0, f1, f0);                              // mul.s f0, f1, f0
  c->swc1(f0, 168, sp);                             // swc1 f0, 168(sp)
  c->mfc1(t1, f0);                                  // mfc1 t1, f0
  
block_13:
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  
block_14:
  c->lbu(t1, 152, sp);                              // lbu t1, 152(sp)
  c->slt(t1, a2, t1);                               // slt t1, a2, t1
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L248
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_10;}                          // branch non-likely

  c->mov64(a2, s7);                                 // or a2, s7, r0
  c->mov64(a2, s7);                                 // or a2, s7, r0
  c->lw(a2, 164, sp);                               // lw a2, 164(sp)
  c->mtc1(f0, a2);                                  // mtc1 f0, a2
  c->lwc1(f1, 16, v1);                              // lwc1 f1, 16(v1)
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->lw(a2, 168, sp);                               // lw a2, 168(sp)
  c->mtc1(f1, a2);                                  // mtc1 f1, a2
  c->lwc1(f2, 24, v1);                              // lwc1 f2, 24(v1)
  c->muls(f1, f1, f2);                              // mul.s f1, f1, f2
  c->adds(f0, f0, f1);                              // add.s f0, f0, f1
  c->lwc1(f1, 56, v1);                              // lwc1 f1, 56(v1)
  c->adds(f0, f1, f0);                              // add.s f0, f1, f0
  c->swc1(f0, 56, v1);                              // swc1 f0, 56(v1)
  c->gprs[a2].du64[0] = 0;                          // or a2, r0, r0
  c->sw(s7, 52, v1);                                // sw s7, 52(v1)
  c->addiu(a2, r0, -1);                             // addiu a2, r0, -1
  c->ld(t1, 136, sp);                               // ld t1, 136(sp)
  bc = c->sgpr64(t1) != c->sgpr64(a2);              // bne t1, a2, L251
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_17;}                          // branch non-likely

  c->daddu(a1, r0, v1);                             // daddu a1, r0, v1
  c->daddiu(a2, v1, 32);                            // daddiu a2, v1, 32
  c->lq(a2, 0, a2);                                 // lq a2, 0(a2)
  c->sq(a2, 0, a1);                                 // sq a2, 0(a1)
  c->daddiu(a1, s7, 4);                             // daddiu a1, s7, #t
  c->sw(a1, 68, v1);                                // sw a1, 68(v1)
  c->daddiu(a1, s7, 4);                             // daddiu a1, s7, #t
  c->sw(a1, 64, v1);                                // sw a1, 64(v1)
  //beq r0, r0, L257                                // beq r0, r0, L257
  // nop                                            // sll r0, r0, 0
  goto block_28;                                    // branch always

  
block_17:
  c->lwc1(f0, 0, v1);                               // lwc1 f0, 0(v1)
  c->lw(a2, 164, sp);                               // lw a2, 164(sp)
  c->mtc1(f1, a2);                                  // mtc1 f1, a2
  c->adds(f0, f0, f1);                              // add.s f0, f0, f1
  c->swc1(f0, 0, v1);                               // swc1 f0, 0(v1)
  c->lwc1(f0, 8, v1);                               // lwc1 f0, 8(v1)
  c->lw(a2, 168, sp);                               // lw a2, 168(sp)
  c->mtc1(f1, a2);                                  // mtc1 f1, a2
  c->adds(f0, f0, f1);                              // add.s f0, f0, f1
  c->swc1(f0, 8, v1);                               // swc1 f0, 8(v1)
  c->ld(a2, 136, sp);                               // ld a2, 136(sp)
  c->lwu(t1, 148, sp);                              // lwu t1, 148(sp)
  c->daddu(a2, a2, t1);                             // daddu a2, a2, t1
  c->lbu(a2, 28, a2);                               // lbu a2, 28(a2)
  c->sb(a2, 188, sp);                               // sb a2, 188(sp)
  c->addiu(a2, r0, 255);                            // addiu a2, r0, 255
  c->lbu(t1, 188, sp);                              // lbu t1, 188(sp)
  bc = c->sgpr64(t1) == c->sgpr64(a2);              // beq t1, a2, L252
  c->mov64(a2, s7);                                 // or a2, s7, r0
  if (bc) {goto block_19;}                          // branch non-likely

  c->lwu(a1, 4, a1);                                // lwu a1, 4(a1)
  c->lbu(a2, 188, sp);                              // lbu a2, 188(sp)
  c->dsll(a2, a2, 6);                               // dsll a2, a2, 6
  c->daddu(a1, a1, a2);                             // daddu a1, a1, a2
  c->sw(a1, 52, v1);                                // sw a1, 52(v1)
  c->mov64(a1, s7);                                 // or a1, s7, r0
  
block_19:
  c->lwu(a1, 52, v1);                               // lwu a1, 52(v1)
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(a1))) {// beql s7, a1, L253
    c->mov64(a1, a1);                               // or a1, a1, r0
    goto block_22;
  }
  
block_21:
  c->daddiu(a1, s7, 4);                             // daddiu a1, s7, 4
  c->lwu(a2, 52, v1);                               // lwu a2, 52(v1)
  c->lbu(a2, 13, a2);                               // lbu a2, 13(a2)
  c->lbu(t1, 61, v1);                               // lbu t1, 61(v1)
  c->and_(a2, a2, t1);                              // and a2, a2, t1
  c->movn(a1, s7, a2);                              // movn a1, s7, a2
  
block_22:
  bc = c->sgpr64(s7) == c->sgpr64(a1);              // beq s7, a1, L254
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_24;}                          // branch non-likely

  c->lwu(a1, 52, v1);                               // lwu a1, 52(v1)
  c->sw(a1, 48, v1);                                // sw a1, 48(v1)
  //beq r0, r0, L257                                // beq r0, r0, L257
  // nop                                            // sll r0, r0, 0
  goto block_28;                                    // branch always

  
block_24:
  c->ld(a1, 136, sp);                               // ld a1, 136(sp)
  c->sb(a1, 60, v1);                                // sb a1, 60(v1)
  c->lwu(a1, 52, v1);                               // lwu a1, 52(v1)
  bc = c->sgpr64(s7) == c->sgpr64(a1);              // beq s7, a1, L255
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_26;}                          // branch non-likely

  c->daddiu(a1, s7, 4);                             // daddiu a1, s7, #t
  c->sw(a1, 76, v1);                                // sw a1, 76(v1)
  //beq r0, r0, L256                                // beq r0, r0, L256
  // nop                                            // sll r0, r0, 0
  goto block_27;                                    // branch always

  
block_26:
  c->daddiu(a1, s7, 4);                             // daddiu a1, s7, #t
  c->sw(a1, 72, v1);                                // sw a1, 72(v1)
  
block_27:
  c->daddiu(a1, s7, 4);                             // daddiu a1, s7, #t
  c->sw(a1, 64, v1);                                // sw a1, 64(v1)
  
block_28:
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->ld(v1, 128, sp);                               // ld v1, 128(sp)
  c->slti(v1, v1, 15);                              // slti v1, v1, 15
  c->daddiu(a1, s7, 4);                             // daddiu a1, s7, 4
  c->movn(a1, s7, v1);                              // movn a1, s7, v1
  if (((s64)c->sgpr64(s7)) != ((s64)c->sgpr64(a1))) {// bnel s7, a1, L258
    c->mov64(v1, a1);                               // or v1, a1, r0
    goto block_31;
  }
  
block_30:
  c->lwu(v1, 112, sp);                              // lwu v1, 112(sp)
  c->lwu(v1, 64, v1);                               // lwu v1, 64(v1)
  
block_31:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L247
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_9;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->lwu(v1, 112, sp);                              // lwu v1, 112(sp)
  c->lwu(v1, 72, v1);                               // lwu v1, 72(v1)
  if (((s64)c->sgpr64(s7)) != ((s64)c->sgpr64(v1))) {// bnel s7, v1, L259
    c->mov64(v1, v1);                               // or v1, v1, r0
    goto block_35;
  }
  
block_34:
  c->lwu(v1, 112, sp);                              // lwu v1, 112(sp)
  c->lwu(v1, 76, v1);                               // lwu v1, 76(v1)
  
block_35:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L261
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_39;}                          // branch non-likely

  c->lwu(v1, 112, sp);                              // lwu v1, 112(sp)
  c->lb(v1, 60, v1);                                // lb v1, 60(v1)
  c->lwu(a1, 124, sp);                              // lwu a1, 124(sp)
  c->daddu(v1, v1, a1);                             // daddu v1, v1, a1
  c->lb(v1, 0, v1);                                 // lb v1, 0(v1)
  c->dsll(v1, v1, 4);                               // dsll v1, v1, 4
  c->daddu(v1, r0, v1);                             // daddu v1, r0, v1
  c->lwu(a1, 112, sp);                              // lwu a1, 112(sp)
  c->lwu(a1, 48, a1);                               // lwu a1, 48(a1)
  c->daddu(v1, v1, a1);                             // daddu v1, v1, a1
  c->sw(v1, 192, sp);                               // sw v1, 192(sp)
  c->lwu(v1, 112, sp);                              // lwu v1, 112(sp)
  c->lb(v1, 60, v1);                                // lb v1, 60(v1)
  c->lwu(a1, 124, sp);                              // lwu a1, 124(sp)
  c->daddu(v1, v1, a1);                             // daddu v1, v1, a1
  c->lb(v1, 4, v1);                                 // lb v1, 4(v1)
  c->dsll(v1, v1, 4);                               // dsll v1, v1, 4
  c->daddu(v1, r0, v1);                             // daddu v1, r0, v1
  c->lwu(a1, 112, sp);                              // lwu a1, 112(sp)
  c->lwu(a1, 48, a1);                               // lwu a1, 48(a1)
  c->daddu(v1, v1, a1);                             // daddu v1, v1, a1
  c->sw(v1, 196, sp);                               // sw v1, 196(sp)
  c->lwu(v1, 192, sp);                              // lwu v1, 192(sp)
  c->lwc1(f0, 8, v1);                               // lwc1 f0, 8(v1)
  c->lwu(v1, 196, sp);                              // lwu v1, 196(sp)
  c->lwc1(f1, 8, v1);                               // lwc1 f1, 8(v1)
  c->subs(f0, f0, f1);                              // sub.s f0, f0, f1
  c->swc1(f0, 200, sp);                             // swc1 f0, 200(sp)
  c->lwu(v1, 196, sp);                              // lwu v1, 196(sp)
  c->lwc1(f0, 0, v1);                               // lwc1 f0, 0(v1)
  c->lwu(v1, 192, sp);                              // lwu v1, 192(sp)
  c->lwc1(f1, 0, v1);                               // lwc1 f1, 0(v1)
  c->subs(f0, f0, f1);                              // sub.s f0, f0, f1
  c->swc1(f0, 204, sp);                             // swc1 f0, 204(sp)
  c->lwc1(f0, 0, a3);                               // lwc1 f0, 0(a3)
  c->swc1(f0, 208, sp);                             // swc1 f0, 208(sp)
  c->lwc1(f0, 8, a3);                               // lwc1 f0, 8(a3)
  c->swc1(f0, 212, sp);                             // swc1 f0, 212(sp)
  c->lwc1(f0, 200, sp);                             // lwc1 f0, 200(sp)
  c->muls(f0, f0, f0);                              // mul.s f0, f0, f0
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lwc1(f1, 204, sp);                             // lwc1 f1, 204(sp)
  c->muls(f1, f1, f1);                              // mul.s f1, f1, f1
  c->mfc1(v1, f1);                                  // mfc1 v1, f1
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->adds(f0, f0, f1);                              // add.s f0, f0, f1
  c->sqrts(f0, f0);                                 // sqrt.s f0, f0
  c->lui(v1, 16256);                                // lui v1, 16256
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->divs(f0, f1, f0);                              // div.s f0, f1, f0
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lw(v1, 200, sp);                               // lw v1, 200(sp)
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->muls(f1, f1, f0);                              // mul.s f1, f1, f0
  c->swc1(f1, 200, sp);                             // swc1 f1, 200(sp)
  c->lw(v1, 204, sp);                               // lw v1, 204(sp)
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->muls(f0, f1, f0);                              // mul.s f0, f1, f0
  c->swc1(f0, 204, sp);                             // swc1 f0, 204(sp)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  c->sw(v1, 0, t0);                                 // sw v1, 0(t0)
  c->daddiu(v1, t0, 16);                            // daddiu v1, t0, 16
  c->lwu(a1, 112, sp);                              // lwu a1, 112(sp)
  c->daddu(a1, r0, a1);                             // daddu a1, r0, a1
  c->daddiu(a2, a0, 44);                            // daddiu a2, a0, 44
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->lqc2(vf4, 0, a1);                              // lqc2 vf4, 0(a1)
  c->lqc2(vf5, 0, a2);                              // lqc2 vf5, 0(a2)
  c->vadd(DEST::xyz, vf6, vf4, vf5);                // vadd.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, v1);                              // sqc2 vf6, 0(v1)
  c->lw(v1, 200, sp);                               // lw v1, 200(sp)
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->swc1(f0, 32, t0);                              // swc1 f0, 32(t0)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 36, t0);                              // swc1 f0, 36(t0)
  c->lw(v1, 204, sp);                               // lw v1, 204(sp)
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->swc1(f0, 40, t0);                              // swc1 f0, 40(t0)
  c->lwu(v1, 112, sp);                              // lwu v1, 112(sp)
  c->lwu(v1, 48, v1);                               // lwu v1, 48(v1)
  c->sw(v1, 80, t0);                                // sw v1, 80(t0)
  c->lwu(v1, 112, sp);                              // lwu v1, 112(sp)
  c->lb(v1, 60, v1);                                // lb v1, 60(v1)
  c->sb(v1, 88, t0);                                // sb v1, 88(t0)
  c->daddiu(a2, t0, 112);                           // daddiu a2, t0, 112
  c->lwu(v1, 192, sp);                              // lwu v1, 192(sp)
  c->daddiu(a1, a0, 44);                            // daddiu a1, a0, 44
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->lqc2(vf4, 0, v1);                              // lqc2 vf4, 0(v1)
  c->lqc2(vf5, 0, a1);                              // lqc2 vf5, 0(a1)
  c->vadd(DEST::xyz, vf6, vf4, vf5);                // vadd.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, a2);                              // sqc2 vf6, 0(a2)
  c->daddiu(a1, t0, 128);                           // daddiu a1, t0, 128
  c->lwu(v1, 196, sp);                              // lwu v1, 196(sp)
  c->daddiu(a0, a0, 44);                            // daddiu a0, a0, 44
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->lqc2(vf4, 0, v1);                              // lqc2 vf4, 0(v1)
  c->lqc2(vf5, 0, a0);                              // lqc2 vf5, 0(a0)
  c->vadd(DEST::xyz, vf6, vf4, vf5);                // vadd.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, a1);                              // sqc2 vf6, 0(a1)
  c->lwu(v1, 112, sp);                              // lwu v1, 112(sp)
  c->daddiu(v1, v1, 32);                            // daddiu v1, v1, 32
  c->lwu(a0, 112, sp);                              // lwu a0, 112(sp)
  c->daddu(a0, r0, a0);                             // daddu a0, r0, a0
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->lwu(v1, 112, sp);                              // lwu v1, 112(sp)
  c->lwu(v1, 76, v1);                               // lwu v1, 76(v1)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L260
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_38;}                          // branch non-likely

  c->lwu(v1, 112, sp);                              // lwu v1, 112(sp)
  c->lwu(v1, 52, v1);                               // lwu v1, 52(v1)
  c->sw(v1, 84, t0);                                // sw v1, 84(t0)
  
block_38:
  //beq r0, r0, L262                                // beq r0, r0, L262
  // nop                                            // sll r0, r0, 0
  goto block_40;                                    // branch always

  
block_39:
  c->mov64(v1, s7);                                 // or v1, s7, r0
  
block_40:
  c->mov64(v1, a3);                                 // or v1, a3, r0
  c->lwu(a0, 112, sp);                              // lwu a0, 112(sp)
  c->daddu(a0, r0, a0);                             // daddu a0, r0, a0
  c->lwu(a1, 116, sp);                              // lwu a1, 116(sp)
  c->lqc2(vf4, 0, a0);                              // lqc2 vf4, 0(a0)
  c->lqc2(vf5, 0, a1);                              // lqc2 vf5, 0(a1)
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, v1);                              // sqc2 vf6, 0(v1)
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  
block_41:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 224);                           // daddiu sp, sp, 224
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  gLinkedFunctionTable.reg("(method 19 nav-mesh)", execute, 224);
}

} // namespace method_19_nav_mesh

namespace method_19_nav_control {
struct Cache {
  void* circle_tangent_directions; // circle-tangent-directions
  void* find_closest_circle_ray_intersection; // find-closest-circle-ray-intersection
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -256);                          // daddiu sp, sp, -256
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s4, 208, sp);                               // sq s4, 208(sp)
  c->sq(s5, 224, sp);                               // sq s5, 224(sp)
  c->sq(gp, 240, sp);                               // sq gp, 240(sp)
  c->mov64(s4, a0);                                 // or s4, a0, r0
  c->mov64(gp, a1);                                 // or gp, a1, r0
  c->daddiu(s5, sp, 16);                            // daddiu s5, sp, 16
  c->daddu(v1, r0, s5);                             // daddu v1, r0, s5
  c->daddiu(a0, gp, 16);                            // daddiu a0, gp, 16
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 4, s5);                               // swc1 f0, 4(s5)
  c->daddu(v1, r0, s5);                             // daddu v1, r0, s5
  c->lui(a0, 16256);                                // lui a0, 16256
  c->mtc1(f0, a0);                                  // mtc1 f0, a0
  c->lqc2(vf1, 0, v1);                              // lqc2 vf1, 0(v1)
  c->vmul(DEST::xyz, vf2, vf1, vf1);                // vmul.xyz vf2, vf1, vf1
  c->mfc1(a0, f0);                                  // mfc1 a0, f0
  c->mov128_vf_gpr(vf3, a0);                        // qmtc2.i vf3, a0
  c->vmula_bc(DEST::w, BC::x, vf0, vf2);            // vmulax.w acc, vf0, vf2
  c->vmadda_bc(DEST::w, BC::y, vf0, vf2);           // vmadday.w acc, vf0, vf2
  c->vmadd_bc(DEST::w, BC::z, vf2, vf0, vf2);       // vmaddz.w vf2, vf0, vf2
  c->vrsqrt(vf3, BC::x, vf2, BC::w);                // vrsqrt Q, vf3.x, vf2.w
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyz, vf1, vf1);                    // vmulq.xyz vf1, vf1, Q
  // nop                                            // vnop
  // nop                                            // vnop
  // nop                                            // vnop
  c->sqc2(vf1, 0, v1);                              // sqc2 vf1, 0(v1)
  c->daddu(a0, r0, s5);                             // daddu a0, r0, s5
  c->daddiu(v1, gp, 16);                            // daddiu v1, gp, 16
  c->lwc1(f0, 0, a0);                               // lwc1 f0, 0(a0)
  c->lwc1(f1, 4, a0);                               // lwc1 f1, 4(a0)
  c->lwc1(f2, 8, a0);                               // lwc1 f2, 8(a0)
  c->lwc1(f3, 0, v1);                               // lwc1 f3, 0(v1)
  c->lwc1(f4, 4, v1);                               // lwc1 f4, 4(v1)
  c->lwc1(f5, 8, v1);                               // lwc1 f5, 8(v1)
  // Unknown instr: mula.s f0, f3
  // Unknown instr: madda.s f1, f4
  // Unknown instr: madd.s f0, f2, f5
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->swc1(f0, 156, s5);                             // swc1 f0, 156(s5)
  c->daddiu(v1, s5, 16);                            // daddiu v1, s5, 16
  c->daddu(a0, r0, s5);                             // daddu a0, r0, s5
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->lwc1(f0, 8, s5);                               // lwc1 f0, 8(s5)
  c->negs(f0, f0);                                  // neg.s f0, f0
  c->swc1(f0, 16, s5);                              // swc1 f0, 16(s5)
  c->lwc1(f0, 0, s5);                               // lwc1 f0, 0(s5)
  c->swc1(f0, 24, s5);                              // swc1 f0, 24(s5)
  c->daddiu(v1, s5, 32);                            // daddiu v1, s5, 32
  c->daddu(a0, r0, s5);                             // daddu a0, r0, s5
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->daddiu(v1, s5, 48);                            // daddiu v1, s5, 48
  c->daddu(a0, r0, s5);                             // daddu a0, r0, s5
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 112, s5);                             // swc1 f0, 112(s5)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 116, s5);                             // swc1 f0, 116(s5)
  c->sd(r0, 128, s5);                               // sd r0, 128(s5)
  c->lui(v1, 18304);                                // lui v1, 18304
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->muls(f0, f0, f0);                              // mul.s f0, f0, f0
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->swc1(f0, 80, gp);                              // swc1 f0, 80(gp)
  c->addiu(v1, r0, 0);                              // addiu v1, r0, 0
  //beq r0, r0, L269                                // beq r0, r0, L269
  // nop                                            // sll r0, r0, 0
  goto block_8;                                     // branch always

  
block_1:
  c->lwu(a0, 56, s4);                               // lwu a0, 56(s4)
  c->dsll(a1, v1, 4);                               // dsll a1, v1, 4
  c->daddu(a0, a0, a1);                             // daddu a0, a0, a1
  c->daddu(a1, r0, gp);                             // daddu a1, r0, gp
  c->mov64(a2, a0);                                 // or a2, a0, r0
  c->lqc2(vf2, 0, a1);                              // lqc2 vf2, 0(a1)
  c->lqc2(vf3, 0, a2);                              // lqc2 vf3, 0(a2)
  c->vsub(DEST::xyzw, vf1, vf3, vf2);               // vsub.xyzw vf1, vf3, vf2
  c->vmul(DEST::xyzw, vf1, vf1, vf1);               // vmul.xyzw vf1, vf1, vf1
  c->vadd_bc(DEST::x, BC::z, vf1, vf1, vf1);        // vaddz.x vf1, vf1, vf1
  c->mov128_gpr_vf(a1, vf1);                        // qmfc2.i a1, vf1
  c->mtc1(f0, a1);                                  // mtc1 f0, a1
  c->swc1(f0, 160, s5);                             // swc1 f0, 160(s5)
  c->lwc1(f0, 80, gp);                              // lwc1 f0, 80(gp)
  c->lwc1(f1, 160, s5);                             // lwc1 f1, 160(s5)
  c->lwc1(f2, 12, a0);                              // lwc1 f2, 12(a0)
  c->muls(f2, f2, f2);                              // mul.s f2, f2, f2
  c->mfc1(a1, f2);                                  // mfc1 a1, f2
  c->mtc1(f2, a1);                                  // mtc1 f2, a1
  c->subs(f1, f1, f2);                              // sub.s f1, f1, f2
  c->mins(f0, f0, f1);                              // min.s f0, f0, f1
  c->swc1(f0, 80, gp);                              // swc1 f0, 80(gp)
  c->lwc1(f0, 80, gp);                              // lwc1 f0, 80(gp)
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = !cop1_bc;                                    // bc1f L268
  c->mov64(a1, s7);                                 // or a1, s7, r0
  if (bc) {goto block_7;}                           // branch non-likely

  c->daddiu(a2, s5, 64);                            // daddiu a2, s5, 64
  c->daddu(a1, r0, gp);                             // daddu a1, r0, gp
  c->lqc2(vf4, 0, a1);                              // lqc2 vf4, 0(a1)
  c->lqc2(vf5, 0, a0);                              // lqc2 vf5, 0(a0)
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, a2);                              // sqc2 vf6, 0(a2)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 68, s5);                              // swc1 f0, 68(s5)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->daddiu(a1, s5, 64);                            // daddiu a1, s5, 64
  c->daddu(a0, r0, s5);                             // daddu a0, r0, s5
  c->lwc1(f1, 0, a1);                               // lwc1 f1, 0(a1)
  c->lwc1(f2, 4, a1);                               // lwc1 f2, 4(a1)
  c->lwc1(f3, 8, a1);                               // lwc1 f3, 8(a1)
  c->lwc1(f4, 0, a0);                               // lwc1 f4, 0(a0)
  c->lwc1(f5, 4, a0);                               // lwc1 f5, 4(a0)
  c->lwc1(f6, 8, a0);                               // lwc1 f6, 8(a0)
  // Unknown instr: mula.s f1, f4
  // Unknown instr: madda.s f2, f5
  // Unknown instr: madd.s f1, f3, f6
  c->mfc1(a0, f1);                                  // mfc1 a0, f1
  c->mtc1(f1, a0);                                  // mtc1 f1, a0
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = !cop1_bc;                                    // bc1f L268
  c->mov64(a1, s7);                                 // or a1, s7, r0
  if (bc) {goto block_7;}                           // branch non-likely

  c->ld(a0, 128, s5);                               // ld a0, 128(s5)
  c->addiu(a1, r0, 1);                              // addiu a1, r0, 1
  if (((s64)c->sgpr64(v1)) >= 0) {                  // bgezl v1, L267
    c->dsllv(a1, a1, v1);                           // dsllv a1, a1, v1
    goto block_6;
  }
  
block_5:
  c->dsubu(a2, r0, v1);                             // dsubu a2, r0, v1
  // Unknown instr: dsrlv a1, a1, a2
  
block_6:
  c->daddu(a1, a0, a1);                             // daddu a1, a0, a1
  c->sd(a1, 128, s5);                               // sd a1, 128(s5)
  
block_7:
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  
block_8:
  c->lw(a0, 52, s4);                                // lw a0, 52(s4)
  c->slt(a0, v1, a0);                               // slt a0, v1, a0
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L266
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->load_symbol2(t9, cache.find_closest_circle_ray_intersection);// lw t9, find-closest-circle-ray-intersection(s7)
  c->daddu(a0, r0, gp);                             // daddu a0, r0, gp
  c->daddu(a1, r0, s5);                             // daddu a1, r0, s5
  c->lwc1(f0, 156, s5);                             // lwc1 f0, 156(s5)
  c->mfc1(a2, f0);                                  // mfc1 a2, f0
  c->lw(a3, 52, s4);                                // lw a3, 52(s4)
  c->lwu(t0, 56, s4);                               // lwu t0, 56(s4)
  c->ld(t1, 128, s5);                               // ld t1, 128(s5)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->sw(v0, 140, s5);                               // sw v0, 140(s5)
  c->addiu(v1, r0, -1);                             // addiu v1, r0, -1
  c->lw(a0, 140, s5);                               // lw a0, 140(s5)
  bc = c->sgpr64(a0) != c->sgpr64(v1);              // bne a0, v1, L270
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_11;}                          // branch non-likely

  c->daddiu(v1, gp, 48);                            // daddiu v1, gp, 48
  c->daddiu(a0, gp, 16);                            // daddiu a0, gp, 16
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->daddiu(v1, gp, 64);                            // daddiu v1, gp, 64
  c->sq(r0, 0, v1);                                 // sq r0, 0(v1)
  c->sw(s7, 84, gp);                                // sw s7, 84(gp)
  //beq r0, r0, L276                                // beq r0, r0, L276
  // nop                                            // sll r0, r0, 0
  goto block_21;                                    // branch always

  
block_11:
  c->ld(v1, 128, s5);                               // ld v1, 128(s5)
  c->addiu(a0, r0, 1);                              // addiu a0, r0, 1
  c->lw(a1, 140, s5);                               // lw a1, 140(s5)
  if (((s64)c->sgpr64(a1)) >= 0) {                  // bgezl a1, L271
    c->dsllv(a0, a0, a1);                           // dsllv a0, a0, a1
    goto block_14;
  }
  
block_13:
  c->dsubu(a1, r0, a1);                             // dsubu a1, r0, a1
  // Unknown instr: dsrlv a0, a0, a1
  
block_14:
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  c->sd(v1, 128, s5);                               // sd v1, 128(s5)
  c->lwu(v1, 56, s4);                               // lwu v1, 56(s4)
  c->lw(a0, 140, s5);                               // lw a0, 140(s5)
  c->dsll(a0, a0, 4);                               // dsll a0, a0, 4
  c->daddu(a1, v1, a0);                             // daddu a1, v1, a0
  c->load_symbol2(t9, cache.circle_tangent_directions);// lw t9, circle-tangent-directions(s7)
  c->daddu(a0, r0, gp);                             // daddu a0, r0, gp
  c->daddiu(a2, s5, 64);                            // daddiu a2, s5, 64
  c->daddiu(a3, s5, 80);                            // daddiu a3, s5, 80
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->addiu(v1, r0, 0);                              // addiu v1, r0, 0
  //beq r0, r0, L273                                // beq r0, r0, L273
  // nop                                            // sll r0, r0, 0
  goto block_16;                                    // branch always

  
block_15:
  c->daddiu(a1, s5, 16);                            // daddiu a1, s5, 16
  c->dsll(a0, v1, 4);                               // dsll a0, v1, 4
  c->daddiu(a0, a0, 64);                            // daddiu a0, a0, 64
  c->daddu(a0, a0, s5);                             // daddu a0, a0, s5
  c->lwc1(f0, 0, a1);                               // lwc1 f0, 0(a1)
  c->lwc1(f1, 4, a1);                               // lwc1 f1, 4(a1)
  c->lwc1(f2, 8, a1);                               // lwc1 f2, 8(a1)
  c->lwc1(f3, 0, a0);                               // lwc1 f3, 0(a0)
  c->lwc1(f4, 4, a0);                               // lwc1 f4, 4(a0)
  c->lwc1(f5, 8, a0);                               // lwc1 f5, 8(a0)
  // Unknown instr: mula.s f0, f3
  // Unknown instr: madda.s f1, f4
  // Unknown instr: madd.s f0, f2, f5
  c->mfc1(a0, f0);                                  // mfc1 a0, f0
  c->mtc1(f0, a0);                                  // mtc1 f0, a0
  c->mfc1(a0, f0);                                  // mfc1 a0, f0
  c->sra(a0, a0, 31);                               // sra a0, a0, 31
  c->andi(a0, a0, 1);                               // andi a0, a0, 1
  c->lui(a1, 16256);                                // lui a1, 16256
  c->mtc1(f0, a1);                                  // mtc1 f0, a1
  c->daddu(a2, r0, s5);                             // daddu a2, r0, s5
  c->dsll(a1, v1, 4);                               // dsll a1, v1, 4
  c->daddiu(a1, a1, 64);                            // daddiu a1, a1, 64
  c->daddu(a1, a1, s5);                             // daddu a1, a1, s5
  c->lwc1(f1, 0, a2);                               // lwc1 f1, 0(a2)
  c->lwc1(f2, 4, a2);                               // lwc1 f2, 4(a2)
  c->lwc1(f3, 8, a2);                               // lwc1 f3, 8(a2)
  c->lwc1(f4, 0, a1);                               // lwc1 f4, 0(a1)
  c->lwc1(f5, 4, a1);                               // lwc1 f5, 4(a1)
  c->lwc1(f6, 8, a1);                               // lwc1 f6, 8(a1)
  // Unknown instr: mula.s f1, f4
  // Unknown instr: madda.s f2, f5
  // Unknown instr: madd.s f1, f3, f6
  c->mfc1(a1, f1);                                  // mfc1 a1, f1
  c->mtc1(f1, a1);                                  // mtc1 f1, a1
  c->subs(f0, f0, f1);                              // sub.s f0, f0, f1
  c->dsll(a1, a0, 4);                               // dsll a1, a0, 4
  c->daddiu(a1, a1, 32);                            // daddiu a1, a1, 32
  c->daddu(a1, a1, s5);                             // daddu a1, a1, s5
  c->dsll(a2, v1, 4);                               // dsll a2, v1, 4
  c->daddiu(a2, a2, 64);                            // daddiu a2, a2, 64
  c->daddu(a2, a2, s5);                             // daddu a2, a2, s5
  c->lq(a2, 0, a2);                                 // lq a2, 0(a2)
  c->sq(a2, 0, a1);                                 // sq a2, 0(a1)
  c->dsll(a0, a0, 2);                               // dsll a0, a0, 2
  c->daddu(a0, a0, s5);                             // daddu a0, a0, s5
  c->swc1(f0, 112, a0);                             // swc1 f0, 112(a0)
  c->mfc1(a0, f0);                                  // mfc1 a0, f0
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  
block_16:
  c->slti(a0, v1, 2);                               // slti a0, v1, 2
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L272
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_15;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->daddu(a0, r0, s5);                             // daddu a0, r0, s5
  c->daddiu(v1, s5, 32);                            // daddiu v1, s5, 32
  c->lwc1(f0, 0, a0);                               // lwc1 f0, 0(a0)
  c->lwc1(f1, 4, a0);                               // lwc1 f1, 4(a0)
  c->lwc1(f2, 8, a0);                               // lwc1 f2, 8(a0)
  c->lwc1(f3, 0, v1);                               // lwc1 f3, 0(v1)
  c->lwc1(f4, 4, v1);                               // lwc1 f4, 4(v1)
  c->lwc1(f5, 8, v1);                               // lwc1 f5, 8(v1)
  // Unknown instr: mula.s f0, f3
  // Unknown instr: madda.s f1, f4
  // Unknown instr: madd.s f0, f2, f5
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->daddu(a0, r0, s5);                             // daddu a0, r0, s5
  c->daddiu(v1, s5, 48);                            // daddiu v1, s5, 48
  c->lwc1(f0, 0, a0);                               // lwc1 f0, 0(a0)
  c->lwc1(f1, 4, a0);                               // lwc1 f1, 4(a0)
  c->lwc1(f2, 8, a0);                               // lwc1 f2, 8(a0)
  c->lwc1(f3, 0, v1);                               // lwc1 f3, 0(v1)
  c->lwc1(f4, 4, v1);                               // lwc1 f4, 4(v1)
  c->lwc1(f5, 8, v1);                               // lwc1 f5, 8(v1)
  // Unknown instr: mula.s f0, f3
  // Unknown instr: madda.s f1, f4
  // Unknown instr: madd.s f0, f2, f5
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->daddiu(a0, gp, 32);                            // daddiu a0, gp, 32
  c->daddiu(v1, s5, 32);                            // daddiu v1, s5, 32
  c->lwc1(f0, 0, a0);                               // lwc1 f0, 0(a0)
  c->lwc1(f1, 4, a0);                               // lwc1 f1, 4(a0)
  c->lwc1(f2, 8, a0);                               // lwc1 f2, 8(a0)
  c->lwc1(f3, 0, v1);                               // lwc1 f3, 0(v1)
  c->lwc1(f4, 4, v1);                               // lwc1 f4, 4(v1)
  c->lwc1(f5, 8, v1);                               // lwc1 f5, 8(v1)
  // Unknown instr: mula.s f0, f3
  // Unknown instr: madda.s f1, f4
  // Unknown instr: madd.s f0, f2, f5
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->daddiu(a0, gp, 32);                            // daddiu a0, gp, 32
  c->daddiu(v1, s5, 48);                            // daddiu v1, s5, 48
  c->lwc1(f1, 0, a0);                               // lwc1 f1, 0(a0)
  c->lwc1(f2, 4, a0);                               // lwc1 f2, 4(a0)
  c->lwc1(f3, 8, a0);                               // lwc1 f3, 8(a0)
  c->lwc1(f4, 0, v1);                               // lwc1 f4, 0(v1)
  c->lwc1(f5, 4, v1);                               // lwc1 f5, 4(v1)
  c->lwc1(f6, 8, v1);                               // lwc1 f6, 8(v1)
  // Unknown instr: mula.s f1, f4
  // Unknown instr: madda.s f2, f5
  // Unknown instr: madd.s f1, f3, f6
  c->mfc1(v1, f1);                                  // mfc1 v1, f1
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  cop1_bc = c->fprs[f1] < c->fprs[f0];              // c.lt.s f1, f0
  bc = !cop1_bc;                                    // bc1f L274
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_19;}                          // branch non-likely

  c->addiu(a1, r0, 0);                              // addiu a1, r0, 0
  //beq r0, r0, L275                                // beq r0, r0, L275
  // nop                                            // sll r0, r0, 0
  goto block_20;                                    // branch always

  
block_19:
  c->addiu(a1, r0, 1);                              // addiu a1, r0, 1
  
block_20:
  c->addiu(v1, r0, 1);                              // addiu v1, r0, 1
  c->dsubu(v1, v1, a1);                             // dsubu v1, v1, a1
  c->daddiu(a0, gp, 48);                            // daddiu a0, gp, 48
  c->dsll(a1, a1, 4);                               // dsll a1, a1, 4
  c->daddiu(a1, a1, 32);                            // daddiu a1, a1, 32
  c->daddu(a1, a1, s5);                             // daddu a1, a1, s5
  c->lwc1(f0, 156, s5);                             // lwc1 f0, 156(s5)
  c->lqc2(vf1, 0, a1);                              // lqc2 vf1, 0(a1)
  c->mfc1(a1, f0);                                  // mfc1 a1, f0
  c->mov128_vf_gpr(vf2, a1);                        // qmtc2.i vf2, a1
  c->vadd_bc(DEST::w, BC::x, vf1, vf0, vf0);        // vaddx.w vf1, vf0, vf0
  c->vmul_bc(DEST::xyz, BC::x, vf1, vf1, vf2);      // vmulx.xyz vf1, vf1, vf2
  c->sqc2(vf1, 0, a0);                              // sqc2 vf1, 0(a0)
  c->daddiu(a0, gp, 64);                            // daddiu a0, gp, 64
  c->dsll(v1, v1, 4);                               // dsll v1, v1, 4
  c->daddiu(v1, v1, 32);                            // daddiu v1, v1, 32
  c->daddu(v1, v1, s5);                             // daddu v1, v1, s5
  c->lwc1(f0, 156, s5);                             // lwc1 f0, 156(s5)
  c->lqc2(vf1, 0, v1);                              // lqc2 vf1, 0(v1)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mov128_vf_gpr(vf2, v1);                        // qmtc2.i vf2, v1
  c->vadd_bc(DEST::w, BC::x, vf1, vf0, vf0);        // vaddx.w vf1, vf0, vf0
  c->vmul_bc(DEST::xyz, BC::x, vf1, vf1, vf2);      // vmulx.xyz vf1, vf1, vf2
  c->sqc2(vf1, 0, a0);                              // sqc2 vf1, 0(a0)
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  c->sw(v1, 84, gp);                                // sw v1, 84(gp)
  
block_21:
  c->lwu(v0, 84, gp);                               // lwu v0, 84(gp)
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 240, sp);                               // lq gp, 240(sp)
  c->lq(s5, 224, sp);                               // lq s5, 224(sp)
  c->lq(s4, 208, sp);                               // lq s4, 208(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 256);                           // daddiu sp, sp, 256
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.circle_tangent_directions = intern_from_c("circle-tangent-directions").c();
  cache.find_closest_circle_ray_intersection = intern_from_c("find-closest-circle-ray-intersection").c();
  gLinkedFunctionTable.reg("(method 19 nav-control)", execute, 256);
}

} // namespace method_19_nav_control

namespace method_18_nav_control {
struct Cache {
  void* circle_tangent_directions; // circle-tangent-directions
  void* cos; // cos
  void* find_closest_circle_ray_intersection; // find-closest-circle-ray-intersection
  void* rand_vu; // rand-vu
  void* sin; // sin
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -288);                          // daddiu sp, sp, -288
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s3, 208, sp);                               // sq s3, 208(sp)
  c->sq(s4, 224, sp);                               // sq s4, 224(sp)
  c->sq(s5, 240, sp);                               // sq s5, 240(sp)
  c->sq(gp, 256, sp);                               // sq gp, 256(sp)
  c->swc1(f30, 272, sp);                            // swc1 f30, 272(sp)
  c->mov64(s4, a0);                                 // or s4, a0, r0
  c->mov64(gp, a1);                                 // or gp, a1, r0
  c->daddiu(s5, sp, 16);                            // daddiu s5, sp, 16
  c->daddu(v1, r0, s5);                             // daddu v1, r0, s5
  c->daddiu(a0, gp, 16);                            // daddiu a0, gp, 16
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 4, s5);                               // swc1 f0, 4(s5)
  c->daddu(v1, r0, s5);                             // daddu v1, r0, s5
  c->lui(a0, 16256);                                // lui a0, 16256
  c->mtc1(f0, a0);                                  // mtc1 f0, a0
  c->lqc2(vf1, 0, v1);                              // lqc2 vf1, 0(v1)
  c->vmul(DEST::xyz, vf2, vf1, vf1);                // vmul.xyz vf2, vf1, vf1
  c->mfc1(a0, f0);                                  // mfc1 a0, f0
  c->mov128_vf_gpr(vf3, a0);                        // qmtc2.i vf3, a0
  c->vmula_bc(DEST::w, BC::x, vf0, vf2);            // vmulax.w acc, vf0, vf2
  c->vmadda_bc(DEST::w, BC::y, vf0, vf2);           // vmadday.w acc, vf0, vf2
  c->vmadd_bc(DEST::w, BC::z, vf2, vf0, vf2);       // vmaddz.w vf2, vf0, vf2
  c->vrsqrt(vf3, BC::x, vf2, BC::w);                // vrsqrt Q, vf3.x, vf2.w
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyz, vf1, vf1);                    // vmulq.xyz vf1, vf1, Q
  // nop                                            // vnop
  // nop                                            // vnop
  // nop                                            // vnop
  c->sqc2(vf1, 0, v1);                              // sqc2 vf1, 0(v1)
  c->daddu(a0, r0, s5);                             // daddu a0, r0, s5
  c->daddiu(v1, gp, 16);                            // daddiu v1, gp, 16
  c->lwc1(f0, 0, a0);                               // lwc1 f0, 0(a0)
  c->lwc1(f1, 4, a0);                               // lwc1 f1, 4(a0)
  c->lwc1(f2, 8, a0);                               // lwc1 f2, 8(a0)
  c->lwc1(f3, 0, v1);                               // lwc1 f3, 0(v1)
  c->lwc1(f4, 4, v1);                               // lwc1 f4, 4(v1)
  c->lwc1(f5, 8, v1);                               // lwc1 f5, 8(v1)
  // Unknown instr: mula.s f0, f3
  // Unknown instr: madda.s f1, f4
  // Unknown instr: madd.s f0, f2, f5
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->swc1(f0, 156, s5);                             // swc1 f0, 156(s5)
  c->daddiu(v1, s5, 16);                            // daddiu v1, s5, 16
  c->daddu(a0, r0, s5);                             // daddu a0, r0, s5
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->lwc1(f0, 8, s5);                               // lwc1 f0, 8(s5)
  c->negs(f0, f0);                                  // neg.s f0, f0
  c->swc1(f0, 16, s5);                              // swc1 f0, 16(s5)
  c->lwc1(f0, 0, s5);                               // lwc1 f0, 0(s5)
  c->swc1(f0, 24, s5);                              // swc1 f0, 24(s5)
  c->daddiu(v1, s5, 32);                            // daddiu v1, s5, 32
  c->daddu(a0, r0, s5);                             // daddu a0, r0, s5
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->daddiu(v1, s5, 48);                            // daddiu v1, s5, 48
  c->daddu(a0, r0, s5);                             // daddu a0, r0, s5
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 112, s5);                             // swc1 f0, 112(s5)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 116, s5);                             // swc1 f0, 116(s5)
  c->sd(r0, 128, s5);                               // sd r0, 128(s5)
  c->addiu(v1, r0, -1);                             // addiu v1, r0, -1
  c->sw(v1, 144, s5);                               // sw v1, 144(s5)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 148, s5);                             // swc1 f0, 148(s5)
  c->lui(v1, 18304);                                // lui v1, 18304
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->muls(f0, f0, f0);                              // mul.s f0, f0, f0
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->swc1(f0, 80, gp);                              // swc1 f0, 80(gp)
  c->addiu(v1, r0, 0);                              // addiu v1, r0, 0
  //beq r0, r0, L282                                // beq r0, r0, L282
  // nop                                            // sll r0, r0, 0
  goto block_10;                                    // branch always

  
block_1:
  c->lwu(a0, 56, s4);                               // lwu a0, 56(s4)
  c->dsll(a1, v1, 4);                               // dsll a1, v1, 4
  c->daddu(a0, a0, a1);                             // daddu a0, a0, a1
  c->daddu(a1, r0, gp);                             // daddu a1, r0, gp
  c->mov64(a2, a0);                                 // or a2, a0, r0
  c->lqc2(vf2, 0, a1);                              // lqc2 vf2, 0(a1)
  c->lqc2(vf3, 0, a2);                              // lqc2 vf3, 0(a2)
  c->vsub(DEST::xyzw, vf1, vf3, vf2);               // vsub.xyzw vf1, vf3, vf2
  c->vmul(DEST::xyzw, vf1, vf1, vf1);               // vmul.xyzw vf1, vf1, vf1
  c->vadd_bc(DEST::x, BC::z, vf1, vf1, vf1);        // vaddz.x vf1, vf1, vf1
  c->mov128_gpr_vf(a1, vf1);                        // qmfc2.i a1, vf1
  c->mtc1(f0, a1);                                  // mtc1 f0, a1
  c->swc1(f0, 160, s5);                             // swc1 f0, 160(s5)
  c->lwc1(f0, 80, gp);                              // lwc1 f0, 80(gp)
  c->lwc1(f1, 160, s5);                             // lwc1 f1, 160(s5)
  c->lwc1(f2, 12, a0);                              // lwc1 f2, 12(a0)
  c->muls(f2, f2, f2);                              // mul.s f2, f2, f2
  c->mfc1(a1, f2);                                  // mfc1 a1, f2
  c->mtc1(f2, a1);                                  // mtc1 f2, a1
  c->subs(f1, f1, f2);                              // sub.s f1, f1, f2
  c->mins(f0, f0, f1);                              // min.s f0, f0, f1
  c->swc1(f0, 80, gp);                              // swc1 f0, 80(gp)
  c->lwc1(f0, 80, gp);                              // lwc1 f0, 80(gp)
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = !cop1_bc;                                    // bc1f L281
  c->mov64(a1, s7);                                 // or a1, s7, r0
  if (bc) {goto block_9;}                           // branch non-likely

  c->daddiu(a3, s5, 64);                            // daddiu a3, s5, 64
  c->daddu(a1, r0, gp);                             // daddu a1, r0, gp
  c->mov64(a2, a0);                                 // or a2, a0, r0
  c->lqc2(vf4, 0, a1);                              // lqc2 vf4, 0(a1)
  c->lqc2(vf5, 0, a2);                              // lqc2 vf5, 0(a2)
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, a3);                              // sqc2 vf6, 0(a3)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 68, s5);                              // swc1 f0, 68(s5)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->daddiu(a2, s5, 64);                            // daddiu a2, s5, 64
  c->daddu(a1, r0, s5);                             // daddu a1, r0, s5
  c->lwc1(f1, 0, a2);                               // lwc1 f1, 0(a2)
  c->lwc1(f2, 4, a2);                               // lwc1 f2, 4(a2)
  c->lwc1(f3, 8, a2);                               // lwc1 f3, 8(a2)
  c->lwc1(f4, 0, a1);                               // lwc1 f4, 0(a1)
  c->lwc1(f5, 4, a1);                               // lwc1 f5, 4(a1)
  c->lwc1(f6, 8, a1);                               // lwc1 f6, 8(a1)
  // Unknown instr: mula.s f1, f4
  // Unknown instr: madda.s f2, f5
  // Unknown instr: madd.s f1, f3, f6
  c->mfc1(a1, f1);                                  // mfc1 a1, f1
  c->mtc1(f1, a1);                                  // mtc1 f1, a1
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = !cop1_bc;                                    // bc1f L280
  c->mov64(a1, s7);                                 // or a1, s7, r0
  if (bc) {goto block_7;}                           // branch non-likely

  c->ld(a1, 128, s5);                               // ld a1, 128(s5)
  c->addiu(a2, r0, 1);                              // addiu a2, r0, 1
  if (((s64)c->sgpr64(v1)) >= 0) {                  // bgezl v1, L279
    c->dsllv(a2, a2, v1);                           // dsllv a2, a2, v1
    goto block_6;
  }
  
block_5:
  c->dsubu(a3, r0, v1);                             // dsubu a3, r0, v1
  // Unknown instr: dsrlv a2, a2, a3
  
block_6:
  c->daddu(a1, a1, a2);                             // daddu a1, a1, a2
  c->sd(a1, 128, s5);                               // sd a1, 128(s5)
  
block_7:
  c->lwc1(f0, 12, a0);                              // lwc1 f0, 12(a0)
  c->lwc1(f1, 160, s5);                             // lwc1 f1, 160(s5)
  c->sqrts(f1, f1);                                 // sqrt.s f1, f1
  c->subs(f0, f0, f1);                              // sub.s f0, f0, f1
  c->swc1(f0, 164, s5);                             // swc1 f0, 164(s5)
  c->lwc1(f0, 148, s5);                             // lwc1 f0, 148(s5)
  c->lwc1(f1, 164, s5);                             // lwc1 f1, 164(s5)
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = !cop1_bc;                                    // bc1f L281
  c->mov64(a1, s7);                                 // or a1, s7, r0
  if (bc) {goto block_9;}                           // branch non-likely

  c->sw(v1, 144, s5);                               // sw v1, 144(s5)
  c->lwc1(f0, 164, s5);                             // lwc1 f0, 164(s5)
  c->swc1(f0, 148, s5);                             // swc1 f0, 148(s5)
  c->mfc1(a1, f0);                                  // mfc1 a1, f0
  
block_9:
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  
block_10:
  c->lw(a0, 52, s4);                                // lw a0, 52(s4)
  c->slt(a0, v1, a0);                               // slt a0, v1, a0
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L278
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->load_symbol2(t9, cache.find_closest_circle_ray_intersection);// lw t9, find-closest-circle-ray-intersection(s7)
  c->daddu(a0, r0, gp);                             // daddu a0, r0, gp
  c->daddu(a1, r0, s5);                             // daddu a1, r0, s5
  c->lwc1(f0, 156, s5);                             // lwc1 f0, 156(s5)
  c->mfc1(a2, f0);                                  // mfc1 a2, f0
  c->lw(a3, 52, s4);                                // lw a3, 52(s4)
  c->lwu(t0, 56, s4);                               // lwu t0, 56(s4)
  c->ld(t1, 128, s5);                               // ld t1, 128(s5)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->sw(v0, 140, s5);                               // sw v0, 140(s5)
  c->addiu(v1, r0, -1);                             // addiu v1, r0, -1
  c->lw(a0, 140, s5);                               // lw a0, 140(s5)
  bc = c->sgpr64(a0) != c->sgpr64(v1);              // bne a0, v1, L283
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_13;}                          // branch non-likely

  c->daddiu(v1, gp, 48);                            // daddiu v1, gp, 48
  c->daddiu(a0, gp, 16);                            // daddiu a0, gp, 16
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->daddiu(v1, gp, 64);                            // daddiu v1, gp, 64
  c->sq(r0, 0, v1);                                 // sq r0, 0(v1)
  c->sw(s7, 84, gp);                                // sw s7, 84(gp)
  //beq r0, r0, L300                                // beq r0, r0, L300
  // nop                                            // sll r0, r0, 0
  goto block_43;                                    // branch always

  
block_13:
  c->ld(v1, 128, s5);                               // ld v1, 128(s5)
  c->addiu(a0, r0, 1);                              // addiu a0, r0, 1
  c->lw(a1, 140, s5);                               // lw a1, 140(s5)
  if (((s64)c->sgpr64(a1)) >= 0) {                  // bgezl a1, L284
    c->dsllv(a0, a0, a1);                           // dsllv a0, a0, a1
    goto block_16;
  }
  
block_15:
  c->dsubu(a1, r0, a1);                             // dsubu a1, r0, a1
  // Unknown instr: dsrlv a0, a0, a1
  
block_16:
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  c->sd(v1, 128, s5);                               // sd v1, 128(s5)
  c->lwu(v1, 56, s4);                               // lwu v1, 56(s4)
  c->lw(a0, 140, s5);                               // lw a0, 140(s5)
  c->dsll(a0, a0, 4);                               // dsll a0, a0, 4
  c->daddu(a1, v1, a0);                             // daddu a1, v1, a0
  c->load_symbol2(t9, cache.circle_tangent_directions);// lw t9, circle-tangent-directions(s7)
  c->daddu(a0, r0, gp);                             // daddu a0, r0, gp
  c->daddiu(a2, s5, 64);                            // daddiu a2, s5, 64
  c->daddiu(a3, s5, 80);                            // daddiu a3, s5, 80
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->addiu(v1, r0, 0);                              // addiu v1, r0, 0
  //beq r0, r0, L286                                // beq r0, r0, L286
  // nop                                            // sll r0, r0, 0
  goto block_18;                                    // branch always

  
block_17:
  c->daddiu(a1, s5, 16);                            // daddiu a1, s5, 16
  c->dsll(a0, v1, 4);                               // dsll a0, v1, 4
  c->daddiu(a0, a0, 64);                            // daddiu a0, a0, 64
  c->daddu(a0, a0, s5);                             // daddu a0, a0, s5
  c->lwc1(f0, 0, a1);                               // lwc1 f0, 0(a1)
  c->lwc1(f1, 4, a1);                               // lwc1 f1, 4(a1)
  c->lwc1(f2, 8, a1);                               // lwc1 f2, 8(a1)
  c->lwc1(f3, 0, a0);                               // lwc1 f3, 0(a0)
  c->lwc1(f4, 4, a0);                               // lwc1 f4, 4(a0)
  c->lwc1(f5, 8, a0);                               // lwc1 f5, 8(a0)
  // Unknown instr: mula.s f0, f3
  // Unknown instr: madda.s f1, f4
  // Unknown instr: madd.s f0, f2, f5
  c->mfc1(a0, f0);                                  // mfc1 a0, f0
  c->mtc1(f0, a0);                                  // mtc1 f0, a0
  c->mfc1(a0, f0);                                  // mfc1 a0, f0
  c->sra(a0, a0, 31);                               // sra a0, a0, 31
  c->andi(a0, a0, 1);                               // andi a0, a0, 1
  c->lui(a1, 16256);                                // lui a1, 16256
  c->mtc1(f0, a1);                                  // mtc1 f0, a1
  c->daddu(a2, r0, s5);                             // daddu a2, r0, s5
  c->dsll(a1, v1, 4);                               // dsll a1, v1, 4
  c->daddiu(a1, a1, 64);                            // daddiu a1, a1, 64
  c->daddu(a1, a1, s5);                             // daddu a1, a1, s5
  c->lwc1(f1, 0, a2);                               // lwc1 f1, 0(a2)
  c->lwc1(f2, 4, a2);                               // lwc1 f2, 4(a2)
  c->lwc1(f3, 8, a2);                               // lwc1 f3, 8(a2)
  c->lwc1(f4, 0, a1);                               // lwc1 f4, 0(a1)
  c->lwc1(f5, 4, a1);                               // lwc1 f5, 4(a1)
  c->lwc1(f6, 8, a1);                               // lwc1 f6, 8(a1)
  // Unknown instr: mula.s f1, f4
  // Unknown instr: madda.s f2, f5
  // Unknown instr: madd.s f1, f3, f6
  c->mfc1(a1, f1);                                  // mfc1 a1, f1
  c->mtc1(f1, a1);                                  // mtc1 f1, a1
  c->subs(f0, f0, f1);                              // sub.s f0, f0, f1
  c->dsll(a1, a0, 4);                               // dsll a1, a0, 4
  c->daddiu(a1, a1, 32);                            // daddiu a1, a1, 32
  c->daddu(a1, a1, s5);                             // daddu a1, a1, s5
  c->dsll(a2, v1, 4);                               // dsll a2, v1, 4
  c->daddiu(a2, a2, 64);                            // daddiu a2, a2, 64
  c->daddu(a2, a2, s5);                             // daddu a2, a2, s5
  c->lq(a2, 0, a2);                                 // lq a2, 0(a2)
  c->sq(a2, 0, a1);                                 // sq a2, 0(a1)
  c->dsll(a0, a0, 2);                               // dsll a0, a0, 2
  c->daddu(a0, a0, s5);                             // daddu a0, a0, s5
  c->swc1(f0, 112, a0);                             // swc1 f0, 112(a0)
  c->mfc1(a0, f0);                                  // mfc1 a0, f0
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  
block_18:
  c->slti(a0, v1, 2);                               // slti a0, v1, 2
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L285
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_17;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->lui(v1, 16256);                                // lui v1, 16256
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->swc1(f0, 152, s5);                             // swc1 f0, 152(s5)
  c->addiu(s3, r0, 0);                              // addiu s3, r0, 0
  //beq r0, r0, L295                                // beq r0, r0, L295
  // nop                                            // sll r0, r0, 0
  goto block_34;                                    // branch always

  
block_20:
  c->lw(v1, 140, s5);                               // lw v1, 140(s5)
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  c->sw(v1, 172, s5);                               // sw v1, 172(s5)
  c->ld(v1, 128, s5);                               // ld v1, 128(s5)
  c->sd(v1, 120, s5);                               // sd v1, 120(s5)
  //beq r0, r0, L293                                // beq r0, r0, L293
  // nop                                            // sll r0, r0, 0
  goto block_30;                                    // branch always

  
block_21:
  c->ld(a0, 120, s5);                               // ld a0, 120(s5)
  c->addiu(a1, r0, 1);                              // addiu a1, r0, 1
  if (((s64)c->sgpr64(v1)) >= 0) {                  // bgezl v1, L289
    c->dsllv(a1, a1, v1);                           // dsllv a1, a1, v1
    goto block_24;
  }
  
block_23:
  c->dsubu(a2, r0, v1);                             // dsubu a2, r0, v1
  // Unknown instr: dsrlv a1, a1, a2
  
block_24:
  c->daddu(a0, a0, a1);                             // daddu a0, a0, a1
  c->sd(a0, 120, s5);                               // sd a0, 120(s5)
  c->load_symbol2(t9, cache.circle_tangent_directions);// lw t9, circle-tangent-directions(s7)
  c->daddu(a0, r0, gp);                             // daddu a0, r0, gp
  c->lwu(a1, 56, s4);                               // lwu a1, 56(s4)
  c->dsll(v1, v1, 4);                               // dsll v1, v1, 4
  c->daddu(a1, a1, v1);                             // daddu a1, a1, v1
  c->daddiu(a2, s5, 64);                            // daddiu a2, s5, 64
  c->daddiu(a3, s5, 80);                            // daddiu a3, s5, 80
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->sw(s7, 172, s5);                               // sw s7, 172(s5)
  c->addiu(v1, r0, 0);                              // addiu v1, r0, 0
  //beq r0, r0, L292                                // beq r0, r0, L292
  // nop                                            // sll r0, r0, 0
  goto block_28;                                    // branch always

  
block_25:
  c->lwc1(f0, 152, s5);                             // lwc1 f0, 152(s5)
  c->dsll(a0, v1, 4);                               // dsll a0, v1, 4
  c->daddiu(a0, a0, 64);                            // daddiu a0, a0, 64
  c->daddu(a1, a0, s5);                             // daddu a1, a0, s5
  c->daddu(a0, r0, s5);                             // daddu a0, r0, s5
  c->daddiu(a2, s5, 16);                            // daddiu a2, s5, 16
  c->mov64(a3, a1);                                 // or a3, a1, r0
  c->lwc1(f1, 0, a3);                               // lwc1 f1, 0(a3)
  c->lwc1(f2, 4, a3);                               // lwc1 f2, 4(a3)
  c->lwc1(f3, 8, a3);                               // lwc1 f3, 8(a3)
  c->lwc1(f4, 0, a2);                               // lwc1 f4, 0(a2)
  c->lwc1(f5, 4, a2);                               // lwc1 f5, 4(a2)
  c->lwc1(f6, 8, a2);                               // lwc1 f6, 8(a2)
  // Unknown instr: mula.s f1, f4
  // Unknown instr: madda.s f2, f5
  // Unknown instr: madd.s f1, f3, f6
  c->mfc1(a2, f1);                                  // mfc1 a2, f1
  c->mtc1(f1, a2);                                  // mtc1 f1, a2
  c->mfc1(a3, f1);                                  // mfc1 a3, f1
  c->lui(t0, -32768);                               // lui t0, -32768
  c->lui(a2, 16256);                                // lui a2, 16256
  c->and_(a3, a3, t0);                              // and a3, a3, t0
  c->or_(a2, a3, a2);                               // or a2, a3, a2
  c->mtc1(f1, a2);                                  // mtc1 f1, a2
  c->lui(a2, 16256);                                // lui a2, 16256
  c->mtc1(f2, a2);                                  // mtc1 f2, a2
  c->lwc1(f3, 0, a1);                               // lwc1 f3, 0(a1)
  c->lwc1(f4, 4, a1);                               // lwc1 f4, 4(a1)
  c->lwc1(f5, 8, a1);                               // lwc1 f5, 8(a1)
  c->lwc1(f6, 0, a0);                               // lwc1 f6, 0(a0)
  c->lwc1(f7, 4, a0);                               // lwc1 f7, 4(a0)
  c->lwc1(f8, 8, a0);                               // lwc1 f8, 8(a0)
  // Unknown instr: mula.s f3, f6
  // Unknown instr: madda.s f4, f7
  // Unknown instr: madd.s f3, f5, f8
  c->mfc1(a0, f3);                                  // mfc1 a0, f3
  c->mtc1(f3, a0);                                  // mtc1 f3, a0
  c->subs(f2, f2, f3);                              // sub.s f2, f2, f3
  c->muls(f1, f1, f2);                              // mul.s f1, f1, f2
  c->mfc1(a0, f1);                                  // mfc1 a0, f1
  c->mtc1(f1, a0);                                  // mtc1 f1, a0
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->dsll(a0, s3, 2);                               // dsll a0, s3, 2
  c->daddu(a0, a0, s5);                             // daddu a0, a0, s5
  c->lwc1(f1, 112, a0);                             // lwc1 f1, 112(a0)
  cop1_bc = c->fprs[f1] < c->fprs[f0];              // c.lt.s f1, f0
  bc = !cop1_bc;                                    // bc1f L291
  c->mov64(a0, s7);                                 // or a0, s7, r0
  if (bc) {goto block_27;}                          // branch non-likely

  c->dsll(a0, s3, 4);                               // dsll a0, s3, 4
  c->daddiu(a0, a0, 32);                            // daddiu a0, a0, 32
  c->daddu(a0, a0, s5);                             // daddu a0, a0, s5
  c->dsll(a1, v1, 4);                               // dsll a1, v1, 4
  c->daddiu(a1, a1, 64);                            // daddiu a1, a1, 64
  c->daddu(a1, a1, s5);                             // daddu a1, a1, s5
  c->lq(a1, 0, a1);                                 // lq a1, 0(a1)
  c->sq(a1, 0, a0);                                 // sq a1, 0(a0)
  c->dsll(a0, s3, 2);                               // dsll a0, s3, 2
  c->daddu(a0, a0, s5);                             // daddu a0, a0, s5
  c->swc1(f0, 112, a0);                             // swc1 f0, 112(a0)
  c->daddiu(a0, s7, 4);                             // daddiu a0, s7, #t
  c->sw(a0, 172, s5);                               // sw a0, 172(s5)
  
block_27:
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  
block_28:
  c->slti(a0, v1, 2);                               // slti a0, v1, 2
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L290
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_25;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  
block_30:
  c->lwu(v1, 172, s5);                              // lwu v1, 172(s5)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L294
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_33;}                          // branch non-likely

  c->load_symbol2(t9, cache.find_closest_circle_ray_intersection);// lw t9, find-closest-circle-ray-intersection(s7)
  c->daddu(a0, r0, gp);                             // daddu a0, r0, gp
  c->dsll(v1, s3, 4);                               // dsll v1, s3, 4
  c->daddiu(v1, v1, 32);                            // daddiu v1, v1, 32
  c->daddu(a1, v1, s5);                             // daddu a1, v1, s5
  c->lwc1(f0, 156, s5);                             // lwc1 f0, 156(s5)
  c->mfc1(a2, f0);                                  // mfc1 a2, f0
  c->lw(a3, 52, s4);                                // lw a3, 52(s4)
  c->lwu(t0, 56, s4);                               // lwu t0, 56(s4)
  c->ld(t1, 120, s5);                               // ld t1, 120(s5)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->addiu(a0, r0, -1);                             // addiu a0, r0, -1
  bc = c->sgpr64(v1) != c->sgpr64(a0);              // bne v1, a0, L288
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_21;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  
block_33:
  c->lui(v1, -16512);                               // lui v1, -16512
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lwc1(f1, 152, s5);                             // lwc1 f1, 152(s5)
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->swc1(f0, 152, s5);                             // swc1 f0, 152(s5)
  c->daddiu(s3, s3, 1);                             // daddiu s3, s3, 1
  
block_34:
  c->slti(v1, s3, 2);                               // slti v1, s3, 2
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L287
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_20;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->addiu(v1, r0, -1);                             // addiu v1, r0, -1
  c->lw(a0, 144, s5);                               // lw a0, 144(s5)
  bc = c->sgpr64(a0) == c->sgpr64(v1);              // beq a0, v1, L297
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_39;}                          // branch non-likely

  c->lwu(v1, 56, s4);                               // lwu v1, 56(s4)
  c->lw(a0, 144, s5);                               // lw a0, 144(s5)
  c->dsll(a0, a0, 4);                               // dsll a0, a0, 4
  c->daddu(s4, v1, a0);                             // daddu s4, v1, a0
  c->daddiu(a1, s5, 96);                            // daddiu a1, s5, 96
  c->daddu(v1, r0, gp);                             // daddu v1, r0, gp
  c->mov64(a0, s4);                                 // or a0, s4, r0
  c->lqc2(vf4, 0, v1);                              // lqc2 vf4, 0(v1)
  c->lqc2(vf5, 0, a0);                              // lqc2 vf5, 0(a0)
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, a1);                              // sqc2 vf6, 0(a1)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 100, s5);                             // swc1 f0, 100(s5)
  c->lui(v1, 16931);                                // lui v1, 16931
  c->ori(v1, v1, 55050);                            // ori v1, v1, 55050
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->daddiu(v1, s5, 96);                            // daddiu v1, s5, 96
  c->lqc2(vf1, 0, v1);                              // lqc2 vf1, 0(v1)
  c->vmul(DEST::xyzw, vf1, vf1, vf1);               // vmul.xyzw vf1, vf1, vf1
  c->vmula_bc(DEST::w, BC::x, vf0, vf1);            // vmulax.w acc, vf0, vf1
  c->vmadda_bc(DEST::w, BC::y, vf0, vf1);           // vmadday.w acc, vf0, vf1
  c->vmadd_bc(DEST::w, BC::z, vf1, vf0, vf1);       // vmaddz.w vf1, vf0, vf1
  c->vsqrt(vf1, BC::w);                             // vsqrt Q, vf1.w
  c->vadd_bc(DEST::x, BC::w, vf1, vf0, vf0);        // vaddw.x vf1, vf0, vf0
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::x, vf1, vf1);                      // vmulq.x vf1, vf1, Q
  // nop                                            // vnop
  // nop                                            // vnop
  c->mov128_gpr_vf(v1, vf1);                        // qmfc2.i v1, vf1
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = cop1_bc;                                     // bc1t L296
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_38;}                          // branch non-likely

  c->lui(v1, 18304);                                // lui v1, 18304
  c->mtc1(f30, v1);                                 // mtc1 f30, v1
  c->load_symbol2(t9, cache.rand_vu);               // lw t9, rand-vu(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mtc1(f0, v0);                                  // mtc1 f0, v0
  c->muls(f0, f30, f0);                             // mul.s f0, f30, f0
  c->swc1(f0, 168, s5);                             // swc1 f0, 168(s5)
  c->load_symbol2(t9, cache.cos);                   // lw t9, cos(s7)
  c->lwc1(f0, 168, s5);                             // lwc1 f0, 168(s5)
  c->mfc1(a0, f0);                                  // mfc1 a0, f0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mtc1(f0, v0);                                  // mtc1 f0, v0
  c->swc1(f0, 96, s5);                              // swc1 f0, 96(s5)
  c->load_symbol2(t9, cache.sin);                   // lw t9, sin(s7)
  c->lwc1(f0, 168, s5);                             // lwc1 f0, 168(s5)
  c->mfc1(a0, f0);                                  // mfc1 a0, f0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mtc1(f0, v0);                                  // mtc1 f0, v0
  c->swc1(f0, 104, s5);                             // swc1 f0, 104(s5)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  
block_38:
  c->daddiu(v1, s5, 96);                            // daddiu v1, s5, 96
  c->lui(a0, 16256);                                // lui a0, 16256
  c->mtc1(f0, a0);                                  // mtc1 f0, a0
  c->lqc2(vf1, 0, v1);                              // lqc2 vf1, 0(v1)
  c->vmul(DEST::xyz, vf2, vf1, vf1);                // vmul.xyz vf2, vf1, vf1
  c->mfc1(a0, f0);                                  // mfc1 a0, f0
  c->mov128_vf_gpr(vf3, a0);                        // qmtc2.i vf3, a0
  c->vmula_bc(DEST::w, BC::x, vf0, vf2);            // vmulax.w acc, vf0, vf2
  c->vmadda_bc(DEST::w, BC::y, vf0, vf2);           // vmadday.w acc, vf0, vf2
  c->vmadd_bc(DEST::w, BC::z, vf2, vf0, vf2);       // vmaddz.w vf2, vf0, vf2
  c->vrsqrt(vf3, BC::x, vf2, BC::w);                // vrsqrt Q, vf3.x, vf2.w
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyz, vf1, vf1);                    // vmulq.xyz vf1, vf1, Q
  // nop                                            // vnop
  // nop                                            // vnop
  // nop                                            // vnop
  c->sqc2(vf1, 0, v1);                              // sqc2 vf1, 0(v1)
  c->lwc1(f0, 148, s5);                             // lwc1 f0, 148(s5)
  c->lwc1(f1, 12, s4);                              // lwc1 f1, 12(s4)
  c->divs(f0, f0, f1);                              // div.s f0, f0, f1
  c->daddiu(v1, s5, 32);                            // daddiu v1, s5, 32
  c->daddiu(a0, s5, 32);                            // daddiu a0, s5, 32
  c->daddiu(a1, s5, 96);                            // daddiu a1, s5, 96
  c->movs(f1, f0);                                  // mov.s f1, f0
  c->lqc2(vf1, 0, a0);                              // lqc2 vf1, 0(a0)
  c->lqc2(vf2, 0, a1);                              // lqc2 vf2, 0(a1)
  c->mfc1(a0, f1);                                  // mfc1 a0, f1
  c->mov128_vf_gpr(vf4, a0);                        // qmtc2.i vf4, a0
  c->vadd_bc(DEST::w, BC::x, vf3, vf0, vf0);        // vaddx.w vf3, vf0, vf0
  c->vsub(DEST::xyzw, vf2, vf2, vf1);               // vsub.xyzw vf2, vf2, vf1
  c->vmul_bc(DEST::xyzw, BC::x, vf2, vf2, vf4);     // vmulx.xyzw vf2, vf2, vf4
  c->vadd(DEST::xyz, vf3, vf1, vf2);                // vadd.xyz vf3, vf1, vf2
  c->sqc2(vf3, 0, v1);                              // sqc2 vf3, 0(v1)
  c->daddiu(v1, s5, 48);                            // daddiu v1, s5, 48
  c->daddiu(a0, s5, 48);                            // daddiu a0, s5, 48
  c->daddiu(a1, s5, 96);                            // daddiu a1, s5, 96
  c->lqc2(vf1, 0, a0);                              // lqc2 vf1, 0(a0)
  c->lqc2(vf2, 0, a1);                              // lqc2 vf2, 0(a1)
  c->mfc1(a0, f0);                                  // mfc1 a0, f0
  c->mov128_vf_gpr(vf4, a0);                        // qmtc2.i vf4, a0
  c->vadd_bc(DEST::w, BC::x, vf3, vf0, vf0);        // vaddx.w vf3, vf0, vf0
  c->vsub(DEST::xyzw, vf2, vf2, vf1);               // vsub.xyzw vf2, vf2, vf1
  c->vmul_bc(DEST::xyzw, BC::x, vf2, vf2, vf4);     // vmulx.xyzw vf2, vf2, vf4
  c->vadd(DEST::xyz, vf3, vf1, vf2);                // vadd.xyz vf3, vf1, vf2
  c->sqc2(vf3, 0, v1);                              // sqc2 vf3, 0(v1)
  c->daddiu(v1, s5, 32);                            // daddiu v1, s5, 32
  c->lui(a0, 16256);                                // lui a0, 16256
  c->mtc1(f0, a0);                                  // mtc1 f0, a0
  c->lqc2(vf1, 0, v1);                              // lqc2 vf1, 0(v1)
  c->vmul(DEST::xyz, vf2, vf1, vf1);                // vmul.xyz vf2, vf1, vf1
  c->mfc1(a0, f0);                                  // mfc1 a0, f0
  c->mov128_vf_gpr(vf3, a0);                        // qmtc2.i vf3, a0
  c->vmula_bc(DEST::w, BC::x, vf0, vf2);            // vmulax.w acc, vf0, vf2
  c->vmadda_bc(DEST::w, BC::y, vf0, vf2);           // vmadday.w acc, vf0, vf2
  c->vmadd_bc(DEST::w, BC::z, vf2, vf0, vf2);       // vmaddz.w vf2, vf0, vf2
  c->vrsqrt(vf3, BC::x, vf2, BC::w);                // vrsqrt Q, vf3.x, vf2.w
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyz, vf1, vf1);                    // vmulq.xyz vf1, vf1, Q
  // nop                                            // vnop
  // nop                                            // vnop
  // nop                                            // vnop
  c->sqc2(vf1, 0, v1);                              // sqc2 vf1, 0(v1)
  c->daddiu(v1, s5, 48);                            // daddiu v1, s5, 48
  c->lui(a0, 16256);                                // lui a0, 16256
  c->mtc1(f0, a0);                                  // mtc1 f0, a0
  c->lqc2(vf1, 0, v1);                              // lqc2 vf1, 0(v1)
  c->vmul(DEST::xyz, vf2, vf1, vf1);                // vmul.xyz vf2, vf1, vf1
  c->mfc1(a0, f0);                                  // mfc1 a0, f0
  c->mov128_vf_gpr(vf3, a0);                        // qmtc2.i vf3, a0
  c->vmula_bc(DEST::w, BC::x, vf0, vf2);            // vmulax.w acc, vf0, vf2
  c->vmadda_bc(DEST::w, BC::y, vf0, vf2);           // vmadday.w acc, vf0, vf2
  c->vmadd_bc(DEST::w, BC::z, vf2, vf0, vf2);       // vmaddz.w vf2, vf0, vf2
  c->vrsqrt(vf3, BC::x, vf2, BC::w);                // vrsqrt Q, vf3.x, vf2.w
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyz, vf1, vf1);                    // vmulq.xyz vf1, vf1, Q
  // nop                                            // vnop
  // nop                                            // vnop
  // nop                                            // vnop
  c->sqc2(vf1, 0, v1);                              // sqc2 vf1, 0(v1)
  
block_39:
  c->daddu(a0, r0, s5);                             // daddu a0, r0, s5
  c->daddiu(v1, s5, 32);                            // daddiu v1, s5, 32
  c->lwc1(f0, 0, a0);                               // lwc1 f0, 0(a0)
  c->lwc1(f1, 4, a0);                               // lwc1 f1, 4(a0)
  c->lwc1(f2, 8, a0);                               // lwc1 f2, 8(a0)
  c->lwc1(f3, 0, v1);                               // lwc1 f3, 0(v1)
  c->lwc1(f4, 4, v1);                               // lwc1 f4, 4(v1)
  c->lwc1(f5, 8, v1);                               // lwc1 f5, 8(v1)
  // Unknown instr: mula.s f0, f3
  // Unknown instr: madda.s f1, f4
  // Unknown instr: madd.s f0, f2, f5
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->daddu(a0, r0, s5);                             // daddu a0, r0, s5
  c->daddiu(v1, s5, 48);                            // daddiu v1, s5, 48
  c->lwc1(f0, 0, a0);                               // lwc1 f0, 0(a0)
  c->lwc1(f1, 4, a0);                               // lwc1 f1, 4(a0)
  c->lwc1(f2, 8, a0);                               // lwc1 f2, 8(a0)
  c->lwc1(f3, 0, v1);                               // lwc1 f3, 0(v1)
  c->lwc1(f4, 4, v1);                               // lwc1 f4, 4(v1)
  c->lwc1(f5, 8, v1);                               // lwc1 f5, 8(v1)
  // Unknown instr: mula.s f0, f3
  // Unknown instr: madda.s f1, f4
  // Unknown instr: madd.s f0, f2, f5
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->daddiu(a0, gp, 32);                            // daddiu a0, gp, 32
  c->daddiu(v1, s5, 32);                            // daddiu v1, s5, 32
  c->lwc1(f0, 0, a0);                               // lwc1 f0, 0(a0)
  c->lwc1(f1, 4, a0);                               // lwc1 f1, 4(a0)
  c->lwc1(f2, 8, a0);                               // lwc1 f2, 8(a0)
  c->lwc1(f3, 0, v1);                               // lwc1 f3, 0(v1)
  c->lwc1(f4, 4, v1);                               // lwc1 f4, 4(v1)
  c->lwc1(f5, 8, v1);                               // lwc1 f5, 8(v1)
  // Unknown instr: mula.s f0, f3
  // Unknown instr: madda.s f1, f4
  // Unknown instr: madd.s f0, f2, f5
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->daddiu(a0, gp, 32);                            // daddiu a0, gp, 32
  c->daddiu(v1, s5, 48);                            // daddiu v1, s5, 48
  c->lwc1(f1, 0, a0);                               // lwc1 f1, 0(a0)
  c->lwc1(f2, 4, a0);                               // lwc1 f2, 4(a0)
  c->lwc1(f3, 8, a0);                               // lwc1 f3, 8(a0)
  c->lwc1(f4, 0, v1);                               // lwc1 f4, 0(v1)
  c->lwc1(f5, 4, v1);                               // lwc1 f5, 4(v1)
  c->lwc1(f6, 8, v1);                               // lwc1 f6, 8(v1)
  // Unknown instr: mula.s f1, f4
  // Unknown instr: madda.s f2, f5
  // Unknown instr: madd.s f1, f3, f6
  c->mfc1(v1, f1);                                  // mfc1 v1, f1
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  cop1_bc = c->fprs[f1] < c->fprs[f0];              // c.lt.s f1, f0
  bc = !cop1_bc;                                    // bc1f L298
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_41;}                          // branch non-likely

  c->addiu(a1, r0, 0);                              // addiu a1, r0, 0
  //beq r0, r0, L299                                // beq r0, r0, L299
  // nop                                            // sll r0, r0, 0
  goto block_42;                                    // branch always

  
block_41:
  c->addiu(a1, r0, 1);                              // addiu a1, r0, 1
  
block_42:
  c->addiu(v1, r0, 1);                              // addiu v1, r0, 1
  c->dsubu(v1, v1, a1);                             // dsubu v1, v1, a1
  c->daddiu(a0, gp, 48);                            // daddiu a0, gp, 48
  c->dsll(a1, a1, 4);                               // dsll a1, a1, 4
  c->daddiu(a1, a1, 32);                            // daddiu a1, a1, 32
  c->daddu(a1, a1, s5);                             // daddu a1, a1, s5
  c->lwc1(f0, 156, s5);                             // lwc1 f0, 156(s5)
  c->lqc2(vf1, 0, a1);                              // lqc2 vf1, 0(a1)
  c->mfc1(a1, f0);                                  // mfc1 a1, f0
  c->mov128_vf_gpr(vf2, a1);                        // qmtc2.i vf2, a1
  c->vadd_bc(DEST::w, BC::x, vf1, vf0, vf0);        // vaddx.w vf1, vf0, vf0
  c->vmul_bc(DEST::xyz, BC::x, vf1, vf1, vf2);      // vmulx.xyz vf1, vf1, vf2
  c->sqc2(vf1, 0, a0);                              // sqc2 vf1, 0(a0)
  c->daddiu(a0, gp, 64);                            // daddiu a0, gp, 64
  c->dsll(v1, v1, 4);                               // dsll v1, v1, 4
  c->daddiu(v1, v1, 32);                            // daddiu v1, v1, 32
  c->daddu(v1, v1, s5);                             // daddu v1, v1, s5
  c->lwc1(f0, 156, s5);                             // lwc1 f0, 156(s5)
  c->lqc2(vf1, 0, v1);                              // lqc2 vf1, 0(v1)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mov128_vf_gpr(vf2, v1);                        // qmtc2.i vf2, v1
  c->vadd_bc(DEST::w, BC::x, vf1, vf0, vf0);        // vaddx.w vf1, vf0, vf0
  c->vmul_bc(DEST::xyz, BC::x, vf1, vf1, vf2);      // vmulx.xyz vf1, vf1, vf2
  c->sqc2(vf1, 0, a0);                              // sqc2 vf1, 0(a0)
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  c->sw(v1, 84, gp);                                // sw v1, 84(gp)
  
block_43:
  c->lwu(v0, 84, gp);                               // lwu v0, 84(gp)
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lwc1(f30, 272, sp);                            // lwc1 f30, 272(sp)
  c->lq(gp, 256, sp);                               // lq gp, 256(sp)
  c->lq(s5, 240, sp);                               // lq s5, 240(sp)
  c->lq(s4, 224, sp);                               // lq s4, 224(sp)
  c->lq(s3, 208, sp);                               // lq s3, 208(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 288);                           // daddiu sp, sp, 288
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.circle_tangent_directions = intern_from_c("circle-tangent-directions").c();
  cache.cos = intern_from_c("cos").c();
  cache.find_closest_circle_ray_intersection = intern_from_c("find-closest-circle-ray-intersection").c();
  cache.rand_vu = intern_from_c("rand-vu").c();
  cache.sin = intern_from_c("sin").c();
  gLinkedFunctionTable.reg("(method 18 nav-control)", execute, 288);
}

} // namespace method_18_nav_control
} // namespace Mips2C
