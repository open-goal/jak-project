// cppcheck-suppress-file unusedLabels
//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace method_10_collide_edge_hold_list {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  // bool bc = false;
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
  if (((s64)c->sgpr64(v1)) == ((s64)c->sgpr64(s7))) {// beql v1, s7, L96
    c->sw(a1, 8, a0);                               // sw a1, 8(a0)
    goto block_10;
  }

// block_2:
  c->lwc1(f1, 4, v1);                               // lwc1 f1, 4(v1)
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  if (cop1_bc) {                                    // bc1tl L97
    c->sw(a1, 8, a0);                               // sw a1, 8(a0)
    goto block_11;
  }

// block_4:
  c->mov64(a0, v1);                                 // or a0, v1, r0

block_5:
  c->lwu(v1, 0, v1);                                // lwu v1, 0(v1)
  if (((s64)c->sgpr64(v1)) == ((s64)c->sgpr64(s7))) {// beql v1, s7, L98
    c->sw(a1, 0, a0);                               // sw a1, 0(a0)
    goto block_12;
  }

// block_7:
  c->lwc1(f1, 4, v1);                               // lwc1 f1, 4(v1)
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  if (!cop1_bc) {                                   // bc1fl L95
    c->mov64(a0, v1);                               // or a0, v1, r0
    goto block_5;
  }

// block_9:
  c->sw(a1, 0, a0);                                 // sw a1, 0(a0)
  //beq r0, r0, L99                                 // beq r0, r0, L99
  c->sw(v1, 0, a1);                                 // sw v1, 0(a1)
  goto block_13;                                    // branch always


block_10:
  //beq r0, r0, L99                                 // beq r0, r0, L99
  c->sw(s7, 0, a1);                                 // sw s7, 0(a1)
  goto block_13;                                    // branch always


block_11:
  //beq r0, r0, L99                                 // beq r0, r0, L99
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
  gLinkedFunctionTable.reg("(method 10 collide-edge-hold-list)", execute, 256);
}

} // namespace method_10_collide_edge_hold_list
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace method_19_collide_edge_work {
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
  c->sd(r0, 256, s4);                               // sd r0, 256(s4)
  c->addiu(s2, r0, 16);                             // addiu s2, r0, 16

block_1:
  bc = c->sgpr64(s2) == 0;                          // beq s2, r0, L92
  c->daddiu(s2, s2, -1);                            // daddiu s2, s2, -1
  if (bc) {goto block_25;}                          // branch non-likely

  c->lwu(s1, 8, s5);                                // lwu s1, 8(s5)
  bc = c->sgpr64(s1) == c->sgpr64(s7);              // beq s1, s7, L92
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_25;}                          // branch non-likely

  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->load_symbol2(v1, cache.collide_edge_work);     // lw v1, collide-edge-work(s7)
  c->lwu(t9, 96, v1);                               // lwu t9, 96(v1)
  c->mov64(a1, s1);                                 // or a1, s1, r0
  c->mov64(a2, s4);                                 // or a2, s4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  bc = c->sgpr64(v1) == c->sgpr64(s7);              // beq v1, s7, L87
  c->lwu(v1, 0, s1);                                // lwu v1, 0(s1)
  if (bc) {goto block_5;}                           // branch non-likely

  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  c->mov64(v0, v1);                                 // or v0, v1, r0
  //beq r0, r0, L93                                 // beq r0, r0, L93
  // nop                                            // sll r0, r0, 0
  goto block_26;                                    // branch always


block_5:
  c->lb(s0, 8, s1);                                 // lb s0, 8(s1)
  c->sw(v1, 8, s5);                                 // sw v1, 8(s5)
  bc = ((s64)c->sgpr64(s0)) > 0;                    // bgtz s0, L90
  c->addiu(v1, r0, 2);                              // addiu v1, r0, 2
  if (bc) {goto block_17;}                          // branch non-likely

  if (((s64)c->sgpr64(s0)) < 0) {                   // bltzl s0, L91
    c->dsubu(s0, r0, s0);                           // dsubu s0, r0, s0
    goto block_21;
  }

// block_8:
  c->dsll(v1, s0, 2);                               // dsll v1, s0, 2
  c->daddiu(a0, gp, 352);                           // daddiu a0, gp, 352
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
  bc = ((s64)c->sgpr64(v1)) < 0;                    // bltz v1, L88
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely

  c->sqc2(vf5, 16, s1);                             // sqc2 vf5, 16(s1)
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->load_symbol2(v1, cache.collide_edge_work);     // lw v1, collide-edge-work(s7)
  c->lwu(t9, 88, v1);                               // lwu t9, 88(v1)
  c->mov64(a1, s1);                                 // or a1, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  bc = c->sgpr64(v1) == c->sgpr64(s7);              // beq v1, s7, L88
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely

  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->load_symbol2(v1, cache.collide_edge_hold_list);// lw v1, collide-edge-hold-list(s7)
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
  bc = c->sgpr64(a0) == c->sgpr64(v1);              // beq a0, v1, L86
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
  bc = ((s64)c->sgpr64(v1)) < 0;                    // bltz v1, L86
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->sqc2(vf6, 16, s1);                             // sqc2 vf6, 16(s1)
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->load_symbol2(v1, cache.collide_edge_work);     // lw v1, collide-edge-work(s7)
  c->lwu(t9, 88, v1);                               // lwu t9, 88(v1)
  c->mov64(a1, s1);                                 // or a1, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  bc = c->sgpr64(v1) == c->sgpr64(s7);              // beq v1, s7, L86
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->load_symbol2(v1, cache.collide_edge_hold_list);// lw v1, collide-edge-hold-list(s7)
  c->lwu(t9, 56, v1);                               // lwu t9, 56(v1)
  c->mov64(a1, s1);                                 // or a1, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->lwu(v1, 4, s3);                                // lwu v1, 4(s3)
  bc = c->sgpr64(v1) == c->sgpr64(s7);              // beq v1, s7, L89
  c->lwu(v1, 0, s5);                                // lwu v1, 0(s5)
  if (bc) {goto block_16;}                          // branch non-likely

  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  c->sw(v1, 0, s5);                                 // sw v1, 0(s5)

block_16:
  c->addiu(v1, r0, -1);                             // addiu v1, r0, -1
  //beq r0, r0, L86                                 // beq r0, r0, L86
  c->sb(v1, 8, s1);                                 // sb v1, 8(s1)
  goto block_1;                                     // branch always


block_17:
  bc = c->sgpr64(s0) == c->sgpr64(v1);              // beq s0, v1, L86
  c->lwu(a2, 12, s1);                               // lwu a2, 12(s1)
  if (bc) {goto block_1;}                           // branch non-likely

  c->dsll(v1, s0, 2);                               // dsll v1, s0, 2
  c->daddiu(a0, gp, 352);                           // daddiu a0, gp, 352
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
  bc = ((s64)c->sgpr64(v1)) < 0;                    // bltz v1, L86
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->sqc2(vf5, 16, s1);                             // sqc2 vf5, 16(s1)
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->load_symbol2(v1, cache.collide_edge_work);     // lw v1, collide-edge-work(s7)
  c->lwu(t9, 88, v1);                               // lwu t9, 88(v1)
  c->mov64(a1, s1);                                 // or a1, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  bc = c->sgpr64(v1) == c->sgpr64(s7);              // beq v1, s7, L86
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->load_symbol2(v1, cache.collide_edge_hold_list);// lw v1, collide-edge-hold-list(s7)
  c->lwu(t9, 56, v1);                               // lwu t9, 56(v1)
  c->mov64(a1, s1);                                 // or a1, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->daddiu(v1, s0, 1);                             // daddiu v1, s0, 1
  //beq r0, r0, L86                                 // beq r0, r0, L86
  c->sb(v1, 8, s1);                                 // sb v1, 8(s1)
  goto block_1;                                     // branch always


block_21:
  bc = c->sgpr64(s0) == c->sgpr64(v1);              // beq s0, v1, L86
  c->lwu(a2, 12, s1);                               // lwu a2, 12(s1)
  if (bc) {goto block_1;}                           // branch non-likely

  c->dsll(v1, s0, 2);                               // dsll v1, s0, 2
  c->daddiu(a0, gp, 352);                           // daddiu a0, gp, 352
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
  bc = ((s64)c->sgpr64(v1)) < 0;                    // bltz v1, L86
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->sqc2(vf6, 16, s1);                             // sqc2 vf6, 16(s1)
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->load_symbol2(v1, cache.collide_edge_work);     // lw v1, collide-edge-work(s7)
  c->lwu(t9, 88, v1);                               // lwu t9, 88(v1)
  c->mov64(a1, s1);                                 // or a1, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  bc = c->sgpr64(v1) == c->sgpr64(s7);              // beq v1, s7, L86
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->load_symbol2(v1, cache.collide_edge_hold_list);// lw v1, collide-edge-hold-list(s7)
  c->lwu(t9, 56, v1);                               // lwu t9, 56(v1)
  c->mov64(a1, s1);                                 // or a1, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->daddiu(v1, s0, 1);                             // daddiu v1, s0, 1
  c->dsubu(v1, r0, v1);                             // dsubu v1, r0, v1
  //beq r0, r0, L86                                 // beq r0, r0, L86
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
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.collide_edge_hold_list = intern_from_c(-1, 0, "collide-edge-hold-list").c();
  cache.collide_edge_work = intern_from_c(-1, 0, "collide-edge-work").c();
  gLinkedFunctionTable.reg("(method 19 collide-edge-work)", execute, 256);
}

} // namespace method_19_collide_edge_work
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace method_9_edge_grab_info {
struct Cache {
  void* collide_cache; // *collide-cache*
  void* collide_edge_work; // *collide-edge-work*
  void* target; // *target*
  void* edge_grabbed; // edge-grabbed
  void* matrix; // matrix
  void* send_event_function; // send-event-function
  void* target_edge_grab_jump; // target-edge-grab-jump
  void* vector_matrix; // vector-matrix*!
  void* vector_normalize; // vector-normalize!
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -704);                          // daddiu sp, sp, -704
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s3, 640, sp);                               // sq s3, 640(sp)
  c->sq(s4, 656, sp);                               // sq s4, 656(sp)
  c->sq(s5, 672, sp);                               // sq s5, 672(sp)
  c->sq(gp, 688, sp);                               // sq gp, 688(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->mov64(s5, s7);                                 // or s5, s7, r0
  c->daddiu(v1, gp, 384);                           // daddiu v1, gp, 384
  c->daddiu(a0, gp, 32);                            // daddiu a0, gp, 32
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->lw(v1, 264, gp);                               // lw v1, 264(gp)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L46
  c->mov64(a0, s7);                                 // or a0, s7, r0
  if (bc) {goto block_18;}                          // branch non-likely

  c->ld(a0, 272, gp);                               // ld a0, 272(gp)
  c->subu(a1, a0, s7);                              // subu a1, a0, s7
  if (((s64)c->sgpr64(a1)) == ((s64)0)) {           // beql a1, r0, L40
    c->mov64(a0, s7);                               // or a0, s7, r0
    goto block_6;
  }

// block_3:
  c->sllv(a1, a0, r0);                              // sllv a1, a0, r0
  c->lwu(a1, 0, a1);                                // lwu a1, 0(a1)
  c->lw(a2, 40, a1);                                // lw a2, 40(a1)
  c->dsra32(a0, a0, 0);                             // dsra32 a0, a0, 0
  bc = c->sgpr64(a0) != c->sgpr64(a2);              // bne a0, a2, L39
  c->mov64(a0, s7);                                 // or a0, s7, r0
  if (bc) {goto block_5;}                           // branch non-likely

  c->mov64(a0, a1);                                 // or a0, a1, r0

block_5:
  c->mov64(a1, a0);                                 // or a1, a0, r0

block_6:
  bc = c->sgpr64(s7) != c->sgpr64(a0);              // bne s7, a0, L41
  c->mov64(a1, s7);                                 // or a1, s7, r0
  if (bc) {goto block_9;}                           // branch non-likely

  c->mov64(v0, s7);                                 // or v0, s7, r0
  //beq r0, r0, L54                                 // beq r0, r0, L54
  // nop                                            // sll r0, r0, 0
  goto block_34;                                    // branch always

  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0

block_9:
  c->daddu(s5, a0, v1);                             // daddu s5, a0, v1
  c->lwu(v1, 28, s5);                               // lwu v1, 28(s5)
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L42
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_12;}                          // branch non-likely

  c->mov64(v0, s7);                                 // or v0, s7, r0
  //beq r0, r0, L54                                 // beq r0, r0, L54
  // nop                                            // sll r0, r0, 0
  goto block_34;                                    // branch always

  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0

block_12:
  c->lwu(v1, 128, a0);                              // lwu v1, 128(a0)
  c->lb(a0, 8, s5);                                 // lb a0, 8(s5)
  c->dsll(a0, a0, 5);                               // dsll a0, a0, 5
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  c->lwu(v1, 28, v1);                               // lwu v1, 28(v1)
  c->daddu(s4, r0, v1);                             // daddu s4, r0, v1
  c->addiu(s3, r0, 0);                              // addiu s3, r0, 0
  //beq r0, r0, L44                                 // beq r0, r0, L44
  // nop                                            // sll r0, r0, 0
  goto block_14;                                    // branch always


block_13:
  c->load_symbol2(t9, cache.vector_matrix);         // lw t9, vector-matrix*!(s7)
  c->dsll(v1, s3, 4);                               // dsll v1, s3, 4
  c->daddu(v1, r0, v1);                             // daddu v1, r0, v1
  c->daddu(a0, v1, gp);                             // daddu a0, v1, gp
  c->dsll(v1, s3, 4);                               // dsll v1, s3, 4
  c->daddiu(v1, v1, 128);                           // daddiu v1, v1, 128
  c->daddu(a1, v1, gp);                             // daddu a1, v1, gp
  c->mov64(a2, s4);                                 // or a2, s4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->daddiu(s3, s3, 1);                             // daddiu s3, s3, 1

block_14:
  c->slti(v1, s3, 8);                               // slti v1, s3, 8
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L43
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_13;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->lqc2(vf1, 48, gp);                             // lqc2 vf1, 48(gp)
  c->lqc2(vf2, 64, gp);                             // lqc2 vf2, 64(gp)
  c->lqc2(vf3, 80, gp);                             // lqc2 vf3, 80(gp)
  c->vsub(DEST::xyzw, vf4, vf2, vf1);               // vsub.xyzw vf4, vf2, vf1
  c->vsub(DEST::xyzw, vf5, vf3, vf1);               // vsub.xyzw vf5, vf3, vf1
  c->vopmula(vf4, vf5);                             // vopmula.xyz acc, vf4, vf5
  c->vopmsub(vf6, vf5, vf4);                        // vopmsub.xyz vf6, vf5, vf4
  c->vmul(DEST::xyzw, vf7, vf6, vf6);               // vmul.xyzw vf7, vf6, vf6
  c->vmula_bc(DEST::w, BC::x, vf0, vf7);            // vmulax.w acc, vf0, vf7
  c->vmadda_bc(DEST::w, BC::y, vf0, vf7);           // vmadday.w acc, vf0, vf7
  c->vmadd_bc(DEST::w, BC::z, vf7, vf0, vf7);       // vmaddz.w vf7, vf0, vf7
  c->vrsqrt(vf0, BC::w, vf7, BC::w);                // vrsqrt Q, vf0.w, vf7.w
  c->lui(v1, 16180);                                // lui v1, 16180
  c->ori(v1, v1, 65012);                            // ori v1, v1, 65012
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->vwaitq();                                      // vwaitq
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf6, vf6);                    // vmulq.xyz vf6, vf6, Q
  c->mov128_gpr_vf(v1, vf6);                        // qmfc2.i v1, vf6
  c->dsra32(v1, v1, 0);                             // dsra32 v1, v1, 0
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  cop1_bc = c->fprs[f2] < c->fprs[f1];              // c.lt.s f2, f1
  bc = !cop1_bc;                                    // bc1f L45
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_17;}                          // branch non-likely

  c->mov64(v0, s7);                                 // or v0, s7, r0
  //beq r0, r0, L54                                 // beq r0, r0, L54
  // nop                                            // sll r0, r0, 0
  goto block_34;                                    // branch always


block_17:
  c->daddiu(v1, gp, 304);                           // daddiu v1, gp, 304
  c->load_symbol2(a0, cache.target);                // lw a0, *target*(s7)
  c->lwu(a0, 124, a0);                              // lwu a0, 124(a0)
  c->lwu(a0, 464, a0);                              // lwu a0, 464(a0)
  c->daddiu(a0, a0, 28);                            // daddiu a0, a0, 28
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->load_symbol2(t9, cache.vector_normalize);      // lw t9, vector-normalize!(s7)
  c->daddiu(a0, gp, 320);                           // daddiu a0, gp, 320
  c->daddiu(v1, gp, 16);                            // daddiu v1, gp, 16
  c->daddu(a1, r0, gp);                             // daddu a1, r0, gp
  c->lqc2(vf4, 0, v1);                              // lqc2 vf4, 0(v1)
  c->lqc2(vf5, 0, a1);                              // lqc2 vf5, 0(a1)
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, a0);                              // sqc2 vf6, 0(a0)
  c->lui(a1, 16256);                                // lui a1, 16256
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->load_symbol2(t9, cache.vector_normalize);      // lw t9, vector-normalize!(s7)
  c->daddiu(a0, gp, 288);                           // daddiu a0, gp, 288
  c->daddiu(v1, gp, 320);                           // daddiu v1, gp, 320
  c->daddiu(a1, gp, 304);                           // daddiu a1, gp, 304
  c->lqc2(vf1, 0, v1);                              // lqc2 vf1, 0(v1)
  c->lqc2(vf2, 0, a1);                              // lqc2 vf2, 0(a1)
  c->vopmula(vf1, vf2);                             // vopmula.xyz acc, vf1, vf2
  c->vopmsub(vf3, vf2, vf1);                        // vopmsub.xyz vf3, vf2, vf1
  c->sqc2(vf3, 0, a0);                              // sqc2 vf3, 0(a0)
  c->lui(a1, 16256);                                // lui a1, 16256
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->daddiu(a1, gp, 320);                           // daddiu a1, gp, 320
  c->daddiu(v1, gp, 288);                           // daddiu v1, gp, 288
  c->daddiu(a0, gp, 304);                           // daddiu a0, gp, 304
  c->lqc2(vf1, 0, v1);                              // lqc2 vf1, 0(v1)
  c->lqc2(vf2, 0, a0);                              // lqc2 vf2, 0(a0)
  c->vopmula(vf1, vf2);                             // vopmula.xyz acc, vf1, vf2
  c->vopmsub(vf3, vf2, vf1);                        // vopmsub.xyz vf3, vf2, vf1
  c->sqc2(vf3, 0, a1);                              // sqc2 vf3, 0(a1)
  c->daddiu(v1, gp, 336);                           // daddiu v1, gp, 336
  c->daddiu(a0, gp, 32);                            // daddiu a0, gp, 32
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->load_symbol2(v1, cache.collide_edge_work);     // lw v1, *collide-edge-work*(s7)
  c->daddiu(a0, gp, 288);                           // daddiu a0, gp, 288
  c->load_symbol2(a1, cache.matrix);                // lw a1, matrix(s7)
  c->lwu(t9, 52, a1);                               // lwu t9, 52(a1)
  c->daddiu(a1, v1, 160);                           // daddiu a1, v1, 160
  c->daddiu(a2, v1, 480);                           // daddiu a2, v1, 480
  c->addiu(a3, r0, 12);                             // addiu a3, r0, 12
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(a0, v0);                                 // or a0, v0, r0

block_18:
  c->load_symbol2(v1, cache.collide_edge_work);     // lw v1, *collide-edge-work*(s7)
  c->daddiu(a1, sp, 16);                            // daddiu a1, sp, 16
  c->lui(a0, 4);                                    // lui a0, 4
  c->load_symbol2(a2, cache.target);                // lw a2, *target*(s7)
  c->ld(a2, 196, a2);                               // ld a2, 196(a2)
  c->and_(a0, a0, a2);                              // and a0, a0, a2
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L47
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_20;}                          // branch non-likely

  c->mov64(a0, a1);                                 // or a0, a1, r0
  c->daddiu(a2, v1, 160);                           // daddiu a2, v1, 160
  c->sw(a2, 112, a0);                               // sw a2, 112(a0)
  c->addiu(a2, r0, 6);                              // addiu a2, r0, 6
  c->sw(a2, 116, a0);                               // sw a2, 116(a0)
  c->lwu(v1, 4, v1);                                // lwu v1, 4(v1)
  c->lwu(v1, 156, v1);                              // lwu v1, 156(v1)
  c->lwu(v1, 32, v1);                               // lwu v1, 32(v1)
  c->sw(v1, 100, a0);                               // sw v1, 100(a0)
  c->sw(s7, 88, a0);                                // sw s7, 88(a0)
  c->sw(s7, 92, a0);                                // sw s7, 92(a0)
  c->lui(v1, 2304);                                 // lui v1, 2304
  c->ori(v1, v1, 17);                               // ori v1, v1, 17
  c->sw(v1, 96, a0);                                // sw v1, 96(a0)
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  c->sw(v1, 120, a0);                               // sw v1, 120(a0)
  c->addiu(v1, r0, 1);                              // addiu v1, r0, 1
  c->sw(v1, 352, a0);                               // sw v1, 352(a0)
  //beq r0, r0, L50                                 // beq r0, r0, L50
  // nop                                            // sll r0, r0, 0
  goto block_26;                                    // branch always


block_20:
  c->load_symbol2(a0, cache.target);                // lw a0, *target*(s7)
  c->lwu(a0, 68, a0);                               // lwu a0, 68(a0)
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(a0))) {// beql s7, a0, L48
    c->mov64(a0, a0);                               // or a0, a0, r0
    goto block_23;
  }

// block_22:
  c->load_symbol2(a0, cache.target);                // lw a0, *target*(s7)
  c->lwu(a0, 68, a0);                               // lwu a0, 68(a0)
  c->lwu(a0, 0, a0);                                // lwu a0, 0(a0)
  c->load_symbol_addr(a2, cache.target_edge_grab_jump);// daddiu a2, s7, target-edge-grab-jump
  c->dsubu(a2, a0, a2);                             // dsubu a2, a0, a2
  c->daddiu(a0, s7, 4);                             // daddiu a0, s7, 4
  c->movn(a0, s7, a2);                              // movn a0, s7, a2

block_23:
  bc = c->sgpr64(s7) == c->sgpr64(a0);              // beq s7, a0, L49
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_25;}                          // branch non-likely

  c->mov64(a0, a1);                                 // or a0, a1, r0
  c->daddiu(a2, v1, 256);                           // daddiu a2, v1, 256
  c->sw(a2, 112, a0);                               // sw a2, 112(a0)
  c->addiu(a2, r0, 6);                              // addiu a2, r0, 6
  c->sw(a2, 116, a0);                               // sw a2, 116(a0)
  c->lwu(v1, 4, v1);                                // lwu v1, 4(v1)
  c->lwu(v1, 156, v1);                              // lwu v1, 156(v1)
  c->lwu(v1, 32, v1);                               // lwu v1, 32(v1)
  c->sw(v1, 100, a0);                               // sw v1, 100(a0)
  c->sw(s7, 88, a0);                                // sw s7, 88(a0)
  c->sw(s7, 92, a0);                                // sw s7, 92(a0)
  c->lui(v1, 2304);                                 // lui v1, 2304
  c->ori(v1, v1, 17);                               // ori v1, v1, 17
  c->sw(v1, 96, a0);                                // sw v1, 96(a0)
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  c->sw(v1, 120, a0);                               // sw v1, 120(a0)
  c->addiu(v1, r0, 1);                              // addiu v1, r0, 1
  c->sw(v1, 352, a0);                               // sw v1, 352(a0)
  //beq r0, r0, L50                                 // beq r0, r0, L50
  // nop                                            // sll r0, r0, 0
  goto block_26;                                    // branch always


block_25:
  c->mov64(a0, a1);                                 // or a0, a1, r0
  c->daddiu(a2, v1, 160);                           // daddiu a2, v1, 160
  c->sw(a2, 112, a0);                               // sw a2, 112(a0)
  c->addiu(a2, r0, 6);                              // addiu a2, r0, 6
  c->sw(a2, 116, a0);                               // sw a2, 116(a0)
  c->lwu(v1, 4, v1);                                // lwu v1, 4(v1)
  c->lwu(v1, 156, v1);                              // lwu v1, 156(v1)
  c->lwu(v1, 32, v1);                               // lwu v1, 32(v1)
  c->sw(v1, 100, a0);                               // sw v1, 100(a0)
  c->sw(s7, 88, a0);                                // sw s7, 88(a0)
  c->sw(s7, 92, a0);                                // sw s7, 92(a0)
  c->lui(v1, 2304);                                 // lui v1, 2304
  c->ori(v1, v1, 17);                               // ori v1, v1, 17
  c->sw(v1, 96, a0);                                // sw v1, 96(a0)
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  c->sw(v1, 120, a0);                               // sw v1, 120(a0)
  c->addiu(v1, r0, 1);                              // addiu v1, r0, 1
  c->sw(v1, 352, a0);                               // sw v1, 352(a0)

block_26:
  c->load_symbol2(a0, cache.collide_cache);         // lw a0, *collide-cache*(s7)
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 60, v1);                               // lwu t9, 60(v1)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L51
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_29;}                          // branch non-likely

  c->mov64(v0, s7);                                 // or v0, s7, r0
  //beq r0, r0, L54                                 // beq r0, r0, L54
  // nop                                            // sll r0, r0, 0
  goto block_34;                                    // branch always

  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0

block_29:
  bc = c->sgpr64(s7) == c->sgpr64(s5);              // beq s7, s5, L53
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_33;}                          // branch non-likely

  c->lwu(v1, 0, s5);                                // lwu v1, 0(s5)
  c->daddiu(a1, sp, 560);                           // daddiu a1, sp, 560
  c->mov64(a0, s6);                                 // or a0, s6, r0
  bc = c->sgpr64(s7) == c->sgpr64(a0);              // beq s7, a0, L52
  c->mov64(a2, s7);                                 // or a2, s7, r0
  if (bc) {goto block_32;}                          // branch non-likely

  c->lwu(a2, 24, a0);                               // lwu a2, 24(a0)

block_32:
  c->sw(a2, 8, a1);                                 // sw a2, 8(a1)
  c->addiu(a0, r0, 1);                              // addiu a0, r0, 1
  c->sw(a0, 68, a1);                                // sw a0, 68(a1)
  c->load_symbol_addr(a0, cache.edge_grabbed);      // daddiu a0, s7, edge-grabbed
  c->sw(a0, 64, a1);                                // sw a0, 64(a1)
  c->sd(gp, 16, a1);                                // sd gp, 16(a1)
  c->load_symbol2(t9, cache.send_event_function);   // lw t9, send-event-function(s7)
  c->lwu(a0, 136, v1);                              // lwu a0, 136(v1)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0

block_33:
  c->daddiu(v0, s7, 4);                             // daddiu v0, s7, #t

block_34:
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 688, sp);                               // lq gp, 688(sp)
  c->lq(s5, 672, sp);                               // lq s5, 672(sp)
  c->lq(s4, 656, sp);                               // lq s4, 656(sp)
  c->lq(s3, 640, sp);                               // lq s3, 640(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 704);                           // daddiu sp, sp, 704
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.collide_cache = intern_from_c(-1, 0, "*collide-cache*").c();
  cache.collide_edge_work = intern_from_c(-1, 0, "*collide-edge-work*").c();
  cache.target = intern_from_c(-1, 0, "*target*").c();
  cache.edge_grabbed = intern_from_c(-1, 0, "edge-grabbed").c();
  cache.matrix = intern_from_c(-1, 0, "matrix").c();
  cache.send_event_function = intern_from_c(-1, 0, "send-event-function").c();
  cache.target_edge_grab_jump = intern_from_c(-1, 0, "target-edge-grab-jump").c();
  cache.vector_matrix = intern_from_c(-1, 0, "vector-matrix*!").c();
  cache.vector_normalize = intern_from_c(-1, 0, "vector-normalize!").c();
  gLinkedFunctionTable.reg("(method 9 edge-grab-info)", execute, 1024);
}

} // namespace method_9_edge_grab_info
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace method_17_collide_edge_work {
struct Cache {
  void* format; // format
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -32);                           // daddiu sp, sp, -32
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  c->sq(gp, 16, sp);                                // sq gp, 16(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->lui(v1, 16180);                                // lui v1, 16180
  c->ori(v1, v1, 65012);                            // ori v1, v1, 65012
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lwu(v1, 392, gp);                              // lwu v1, 392(gp)
  // nop                                            // sll r0, r0, 0
  c->addiu(a0, r0, 896);                            // addiu a0, r0, 896
  c->lwu(t3, 0, gp);                                // lwu t3, 0(gp)
  c->addiu(a1, r0, 0);                              // addiu a1, r0, 0
  c->daddiu(a2, gp, 6320);                          // daddiu a2, gp, 6320
  c->addiu(a3, r0, 256);                            // addiu a3, r0, 256
  c->lwu(t0, 0, t3);                                // lwu t0, 0(t3)
  c->gprs[t1].du64[0] = 0;                          // or t1, r0, r0
  c->lq(t2, 96, gp);                                // lq t2, 96(gp)
  c->daddiu(t3, t3, 4908);                          // daddiu t3, t3, 4908
  c->lq(t4, 112, gp);                               // lq t4, 112(gp)

block_1:
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L35
  c->lwu(t5, 48, t3);                               // lwu t5, 48(t3)
  if (bc) {goto block_14;}                          // branch non-likely

  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  c->lqc2(vf1, 0, t3);                              // lqc2 vf1, 0(t3)
  c->and_(t6, t5, a0);                              // and t6, t5, a0
  c->lqc2(vf2, 16, t3);                             // lqc2 vf2, 16(t3)
  bc = c->sgpr64(t6) == c->sgpr64(a1);              // beq t6, a1, L34
  c->lqc2(vf3, 32, t3);                             // lqc2 vf3, 32(t3)
  if (bc) {goto block_5;}                           // branch non-likely

  if (((s64)c->sgpr64(t6)) != ((s64)c->sgpr64(a3))) {// bnel t6, a3, L33
    c->daddiu(t3, t3, 64);                          // daddiu t3, t3, 64
    goto block_1;
  }

block_5:
  c->and_(t6, t5, v1);                              // and t6, t5, v1
  c->vmini(DEST::xyzw, vf7, vf1, vf2);              // vmini.xyzw vf7, vf1, vf2
  bc = c->sgpr64(t6) != 0;                          // bne t6, r0, L33
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
  if (((s64)c->sgpr64(t6)) != ((s64)0)) {           // bnel t6, r0, L33
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
  // nop                                            // sll r0, r0, 0
  c->dsll32(t5, t5, 8);                             // dsll32 t5, t5, 8
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
  if (cop1_bc) {                                    // bc1tl L33
    c->daddiu(t3, t3, 64);                          // daddiu t3, t3, 64
    goto block_1;
  }

// block_10:
  if (((s64)c->sgpr64(t5)) == ((s64)c->sgpr64(t6))) {// beql t5, t6, L33
    c->daddiu(t3, t3, 64);                          // daddiu t3, t3, 64
    goto block_1;
  }

// block_12:
  bc = c->sgpr64(t1) == c->sgpr64(t7);              // beq t1, t7, L36
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_15;}                          // branch non-likely

  c->daddiu(t1, t1, 1);                             // daddiu t1, t1, 1
  c->sw(t3, 0, a2);                                 // sw t3, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf6, 16, a2);                             // sqc2 vf6, 16(a2)
  c->daddiu(a2, a2, 32);                            // daddiu a2, a2, 32
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L33                                 // beq r0, r0, L33
  c->daddiu(t3, t3, 64);                            // daddiu t3, t3, 64
  goto block_1;                                     // branch always


block_14:
  //beq r0, r0, L37                                 // beq r0, r0, L37
  c->sw(t1, 16, gp);                                // sw t1, 16(gp)
  goto block_16;                                    // branch always


block_15:
  c->load_symbol2(t9, cache.format);                // lw t9, format(s7)
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  // daddiu a1, fp, L119                               // daddiu a1, fp, L119
  // call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  printf("ERROR: Exceeded max # of grabbable tris!\n");
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
  cache.format = intern_from_c(-1, 0, "format").c();
  gLinkedFunctionTable.reg("(method 17 collide-edge-work)", execute, 512);
}

} // namespace method_17_collide_edge_work
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace method_16_collide_edge_work {
struct Cache {
  void* format; // format
} cache;

void sub_l20_b15(ExecutionContext* c) {
  bool bc = false;
  bool cop1_bc = false;
// block_15:
  c->mov64(t4, t1);                                 // or t4, t1, r0
  c->lwu(t3, 12, a0);                               // lwu t3, 12(a0)
  c->dsll32(t4, t4, 0);                             // dsll32 t4, t4, 0
  c->gprs[t5].du64[0] = 0;                          // or t5, r0, r0
  c->or_(t6, t4, t2);                               // or t6, t4, t2
  c->daddiu(t4, a0, 1712);                          // daddiu t4, a0, 1712

block_16:
  bc = c->sgpr64(t5) == c->sgpr64(t3);              // beq t5, t3, L22
  c->ld(t7, 8, t4);                                 // ld t7, 8(t4)
  if (bc) {goto block_20;}                          // branch non-likely

  c->daddiu(t5, t5, 1);                             // daddiu t5, t5, 1
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  if (((s64)c->sgpr64(t6)) != ((s64)c->sgpr64(t7))) {// bnel t6, t7, L21
    c->daddiu(t4, t4, 48);                          // daddiu t4, t4, 48
    goto block_16;
  }

// block_19:
  //beq r0, r0, L24                                 // beq r0, r0, L24
  c->sw(r0, 0, t4);                                 // sw r0, 0(t4)
  goto block_26;                                    // branch always


block_20:
  c->addiu(t5, r0, 96);                             // addiu t5, r0, 96
  // nop                                            // sll r0, r0, 0
  if (((s64)c->sgpr64(t3)) == ((s64)c->sgpr64(t5))) {// beql t3, t5, L24
    c->mov64(t4, s7);                               // or t4, s7, r0
    goto block_26;
  }

// block_22:
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
  cop1_bc = c->fprs[f4] < c->fprs[f2];              // c.lt.s f4, f2
  // nop                                            // sll r0, r0, 0
  bc = cop1_bc;                                     // bc1t L23
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_25;}                          // branch non-likely

  cop1_bc = c->fprs[f3] < c->fprs[f4];              // c.lt.s f3, f4
  // nop                                            // sll r0, r0, 0
  if (cop1_bc) {                                    // bc1tl L24
    c->sw(r0, 0, t4);                               // sw r0, 0(t4)
    goto block_26;
  }

block_25:
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

block_26:
  ;
  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  // goto end_of_function;                          // return
}

void sub_l25_b27(ExecutionContext* c) {
  bool bc = false;
  bool cop1_bc = false;
  c->gprs[t2].du64[0] = 0;                          // or t2, r0, r0
  c->lwu(t1, 8, a0);                                // lwu t1, 8(a0)
  c->daddiu(t0, a0, 688);                           // daddiu t0, a0, 688
  // nop                                            // sll r0, r0, 0

block_28:
  bc = c->sgpr64(t2) == c->sgpr64(t1);              // beq t2, t1, L27
  c->lqc2(vf9, 0, t0);                              // lqc2 vf9, 0(t0)
  if (bc) {goto block_37;}                          // branch non-likely

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
  bc = cop1_bc;                                     // bc1t L28
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_40;}                          // branch non-likely

  bc = c->sgpr64(t2) == c->sgpr64(t1);              // beq t2, t1, L27
  c->daddiu(t0, t0, 16);                            // daddiu t0, t0, 16
  if (bc) {goto block_37;}                          // branch non-likely

  c->mov128_gpr_vf(t3, vf10);                       // qmfc2.i t3, vf10
  c->daddiu(t2, t2, 1);                             // daddiu t2, t2, 1
  c->mtc1(f1, t3);                                  // mtc1 f1, t3
  // nop                                            // sll r0, r0, 0
  cop1_bc = c->fprs[f1] <= c->fprs[f0];             // c.le.s f1, f0
  // nop                                            // sll r0, r0, 0
  bc = cop1_bc;                                     // bc1t L28
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_40;}                          // branch non-likely

  bc = c->sgpr64(t2) == c->sgpr64(t1);              // beq t2, t1, L27
  c->daddiu(t0, t0, 16);                            // daddiu t0, t0, 16
  if (bc) {goto block_37;}                          // branch non-likely

  c->mov128_gpr_vf(t3, vf11);                       // qmfc2.i t3, vf11
  c->daddiu(t2, t2, 1);                             // daddiu t2, t2, 1
  c->mtc1(f1, t3);                                  // mtc1 f1, t3
  // nop                                            // sll r0, r0, 0
  cop1_bc = c->fprs[f1] <= c->fprs[f0];             // c.le.s f1, f0
  // nop                                            // sll r0, r0, 0
  bc = cop1_bc;                                     // bc1t L28
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_40;}                          // branch non-likely

  bc = c->sgpr64(t2) == c->sgpr64(t1);              // beq t2, t1, L27
  c->daddiu(t0, t0, 16);                            // daddiu t0, t0, 16
  if (bc) {goto block_37;}                          // branch non-likely

  c->mov128_gpr_vf(t3, vf12);                       // qmfc2.i t3, vf12
  c->daddiu(t2, t2, 1);                             // daddiu t2, t2, 1
  c->mtc1(f1, t3);                                  // mtc1 f1, t3
  // nop                                            // sll r0, r0, 0
  cop1_bc = c->fprs[f1] <= c->fprs[f0];             // c.le.s f1, f0
  // nop                                            // sll r0, r0, 0
  bc = cop1_bc;                                     // bc1t L28
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_40;}                          // branch non-likely

  //beq r0, r0, L26                                 // beq r0, r0, L26
  c->daddiu(t0, t0, 16);                            // daddiu t0, t0, 16
  goto block_28;                                    // branch always


block_37:
  c->addiu(t2, r0, 64);                             // addiu t2, r0, 64
  // nop                                            // sll r0, r0, 0
  if (((s64)c->sgpr64(t2)) == ((s64)c->sgpr64(t1))) {// beql t2, t1, L28
    c->mov64(t0, s7);                               // or t0, s7, r0
    goto block_40;
  }

// block_39:
  c->daddiu(t1, t1, 1);                             // daddiu t1, t1, 1
  c->sqc2(vf8, 0, t0);                              // sqc2 vf8, 0(t0)
  // nop                                            // sll r0, r0, 0
  c->sw(t1, 8, a0);                                 // sw t1, 8(a0)

block_40:
  ;
  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  // goto end_of_function;                          // return
}

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  // bool cop1_bc = false;
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  c->lui(v1, 17617);                                // lui v1, 17617
  c->ori(v1, v1, 46871);                            // ori v1, v1, 46871
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lwc1(f4, 404, a0);                             // lwc1 f4, 404(a0)
  // nop                                            // sll r0, r0, 0
  c->mtc1(f2, r0);                                  // mtc1 f2, r0
  c->lwu(a1, 4, a0);                                // lwu a1, 4(a0)
  // nop                                            // sll r0, r0, 0
  c->lwu(v1, 16, a0);                               // lwu v1, 16(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf1, 12, a1);                             // lqc2 vf1, 12(a1)
  c->daddiu(a1, a0, 6320);                          // daddiu a1, a0, 6320
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
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L31
  c->lwu(t0, 0, a1);                                // lwu t0, 0(a1)
  if (bc) {goto block_43;}                          // branch non-likely

  c->daddiu(v1, v1, -1);                            // daddiu v1, v1, -1
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 0, t0);                              // lqc2 vf3, 0(t0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf4, 16, t0);                             // lqc2 vf4, 16(t0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf5, 32, t0);                             // lqc2 vf5, 32(t0)
  // Unknown instr: bgezal r0, L25
  c->vmove(DEST::xyzw, vf8, vf3);                   // vmove.xyzw vf8, vf3
  // if (bc) {goto block_27;}                       // branch non-likely
  sub_l25_b27(c);

  bc = c->sgpr64(t0) == c->sgpr64(s7);              // beq t0, s7, L29
  c->mov64(a2, t0);                                 // or a2, t0, r0
  if (bc) {goto block_41;}                          // branch non-likely

  // Unknown instr: bgezal r0, L25
  c->vmove(DEST::xyzw, vf8, vf4);                   // vmove.xyzw vf8, vf4
  // if (bc) {goto block_27;}                       // branch non-likely
  sub_l25_b27(c);

  bc = c->sgpr64(t0) == c->sgpr64(s7);              // beq t0, s7, L29
  c->mov64(a3, t0);                                 // or a3, t0, r0
  if (bc) {goto block_41;}                          // branch non-likely

  // Unknown instr: bgezal r0, L25
  c->vmove(DEST::xyzw, vf8, vf5);                   // vmove.xyzw vf8, vf5
  // if (bc) {goto block_27;}                       // branch non-likely
  sub_l25_b27(c);

  bc = c->sgpr64(t0) == c->sgpr64(s7);              // beq t0, s7, L29
  c->mov64(t0, t0);                                 // or t0, t0, r0
  if (bc) {goto block_41;}                          // branch non-likely

  c->mov64(t1, a2);                                 // or t1, a2, r0
  // nop                                            // sll r0, r0, 0
  // Unknown instr: bgezal r0, L20
  c->mov64(t2, a3);                                 // or t2, a3, r0
  // if (bc) {goto block_15;}                       // branch non-likely
  sub_l20_b15(c);

  bc = c->sgpr64(t4) == c->sgpr64(s7);              // beq t4, s7, L30
  c->mov64(t1, a3);                                 // or t1, a3, r0
  if (bc) {goto block_42;}                          // branch non-likely

  // Unknown instr: bgezal r0, L20
  c->mov64(t2, t0);                                 // or t2, t0, r0
  // if (bc) {goto block_15;}                       // branch non-likely
  sub_l20_b15(c);

  bc = c->sgpr64(t4) == c->sgpr64(s7);              // beq t4, s7, L30
  c->mov64(t1, t0);                                 // or t1, t0, r0
  if (bc) {goto block_42;}                          // branch non-likely

  // Unknown instr: bgezal r0, L20
  c->mov64(t2, a2);                                 // or t2, a2, r0
  // if (bc) {goto block_15;}                       // branch non-likely
  sub_l20_b15(c);

  bc = c->sgpr64(t1) == c->sgpr64(s7);              // beq t1, s7, L30
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_42;}                          // branch non-likely

  //beq r0, r0, L19                                 // beq r0, r0, L19
  c->daddiu(a1, a1, 32);                            // daddiu a1, a1, 32
  goto block_1;                                     // branch always

block_41:
  c->load_symbol2(t9, cache.format);                // lw t9, format(s7)
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  // daddiu a1, fp, L118                               // daddiu a1, fp, L118
  // call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  printf("ERROR: Too many edge verts found in edge grab!\n");
  //beq r0, r0, L31                                 // beq r0, r0, L31
  // nop                                            // sll r0, r0, 0
  goto block_43;                                    // branch always


block_42:
  c->load_symbol2(t9, cache.format);                // lw t9, format(s7)
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  // daddiu a1, fp, L117                               // daddiu a1, fp, L117
  // call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  printf("ERROR: Too many edges found in edge grab!\n");

block_43:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
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
  cache.format = intern_from_c(-1, 0, "format").c();
  gLinkedFunctionTable.reg("(method 16 collide-edge-work)", execute, 512);
}

} // namespace method_16_collide_edge_work
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace method_18_collide_edge_work {
struct Cache {
  void* vector_vector_distance; // vector-vector-distance
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  float acc;
  c->daddiu(sp, sp, -96);                           // daddiu sp, sp, -96
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s3, 16, sp);                                // sq s3, 16(sp)
  c->sq(s4, 32, sp);                                // sq s4, 32(sp)
  c->sq(s5, 48, sp);                                // sq s5, 48(sp)
  c->sq(gp, 64, sp);                                // sq gp, 64(sp)
  c->swc1(f30, 80, sp);                             // swc1 f30, 80(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->mov64(s5, a1);                                 // or s5, a1, r0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf1, 16, s5);                             // lqc2 vf1, 16(s5)
  // nop                                            // sll r0, r0, 0
  c->lq(a0, 96, gp);                                // lq a0, 96(gp)
  // nop                                            // sll r0, r0, 0
  c->lq(a3, 112, gp);                               // lq a3, 112(gp)
  c->vftoi0(DEST::xyzw, vf2, vf1);                  // vftoi0.xyzw vf2, vf1
  c->lwu(v1, 4, gp);                                // lwu v1, 4(gp)
  c->mov128_gpr_vf(a1, vf2);                        // qmfc2.i a1, vf2
  c->lqc2(vf3, 16, a2);                             // lqc2 vf3, 16(a2)
  c->pcgtw(a3, a1, a3);                             // pcgtw a3, a1, a3
  c->lqc2(vf4, 368, gp);                            // lqc2 vf4, 368(gp)
  c->pcgtw(a0, a0, a1);                             // pcgtw a0, a0, a1
  c->lqc2(vf5, 12, v1);                             // lqc2 vf5, 12(v1)
  c->por(v1, a3, a0);                               // por v1, a3, a0
  c->lwc1(f0, 396, gp);                             // lwc1 f0, 396(gp)
  c->ppach(v1, r0, v1);                             // ppach v1, r0, v1
  c->lwc1(f1, 400, gp);                             // lwc1 f1, 400(gp)
  c->dsll(v1, v1, 16);                              // dsll v1, v1, 16
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L10
  c->lqc2(vf6, 144, gp);                            // lqc2 vf6, 144(gp)
  if (bc) {goto block_24;}                          // branch non-likely

  c->vmul_bc(DEST::xyzw, BC::x, vf10, vf3, vf4);    // vmulx.xyzw vf10, vf3, vf4
  c->lqc2(vf11, 128, gp);                           // lqc2 vf11, 128(gp)
  c->vadd(DEST::xyzw, vf10, vf10, vf1);             // vadd.xyzw vf10, vf10, vf1
  c->vsub(DEST::xyzw, vf7, vf5, vf10);              // vsub.xyzw vf7, vf5, vf10
  c->vmul(DEST::xyzw, vf7, vf7, vf7);               // vmul.xyzw vf7, vf7, vf7
  c->vadd_bc(DEST::x, BC::z, vf7, vf7, vf7);        // vaddz.x vf7, vf7, vf7
  c->mov128_gpr_vf(v1, vf7);                        // qmfc2.i v1, vf7
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  cop1_bc = c->fprs[f0] < c->fprs[f2];              // c.lt.s f0, f2
  bc = cop1_bc;                                     // bc1t L10
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_24;}                          // branch non-likely

  c->vsub(DEST::xyzw, vf8, vf1, vf5);               // vsub.xyzw vf8, vf1, vf5
  c->vmul(DEST::xyzw, vf7, vf8, vf8);               // vmul.xyzw vf7, vf8, vf8
  c->vadd_bc(DEST::x, BC::z, vf7, vf7, vf7);        // vaddz.x vf7, vf7, vf7
  c->vrsqrt(vf0, BC::w, vf7, BC::x);                // vrsqrt Q, vf0.w, vf7.x
  // nop                                            // sll r0, r0, 0
  c->vwaitq();                                      // vwaitq
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xz, vf8, vf8);                     // vmulq.xz vf8, vf8, Q
  c->vmul(DEST::xyzw, vf9, vf8, vf6);               // vmul.xyzw vf9, vf8, vf6
  c->vadd_bc(DEST::x, BC::z, vf9, vf9, vf9);        // vaddz.x vf9, vf9, vf9
  c->mov128_gpr_vf(v1, vf9);                        // qmfc2.i v1, vf9
  c->mtc1(f3, v1);                                  // mtc1 f3, v1
  cop1_bc = c->fprs[f3] < c->fprs[f1];              // c.lt.s f3, f1
  bc = cop1_bc;                                     // bc1t L10
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_24;}                          // branch non-likely

  c->vsub(DEST::xyzw, vf7, vf11, vf1);              // vsub.xyzw vf7, vf11, vf1
  c->sqc2(vf1, 16, s5);                             // sqc2 vf1, 16(s5)
  c->vmul(DEST::xyzw, vf7, vf7, vf7);               // vmul.xyzw vf7, vf7, vf7
  c->sb(r0, 8, s5);                                 // sb r0, 8(s5)
  c->sqc2(vf10, 32, s5);                            // sqc2 vf10, 32(s5)
  c->vadd_bc(DEST::x, BC::z, vf7, vf7, vf7);        // vaddz.x vf7, vf7, vf7
  c->sw(a2, 12, s5);                                // sw a2, 12(s5)
  c->mov128_gpr_vf(v1, vf7);                        // qmfc2.i v1, vf7
  c->sw(v1, 4, s5);                                 // sw v1, 4(s5)
  c->ld(v1, 384, gp);                               // ld v1, 384(gp)
  c->andi(v1, v1, 2);                               // andi v1, v1, 2
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L8
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_20;}                          // branch non-likely

  c->lwu(s3, 12, s5);                               // lwu s3, 12(s5)
  c->lwu(s4, 408, gp);                              // lwu s4, 408(gp)
  c->lui(v1, 16256);                                // lui v1, 16256
  c->mtc1(f30, v1);                                 // mtc1 f30, v1
  c->load_symbol2(t9, cache.vector_vector_distance);// lw t9, vector-vector-distance(s7)
  c->lwu(v1, 8, s3);                                // lwu v1, 8(s3)
  c->daddu(a0, r0, v1);                             // daddu a0, r0, v1
  c->daddu(a1, r0, s4);                             // daddu a1, r0, s4
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mtc1(f0, v0);                                  // mtc1 f0, v0
  cop1_bc = c->fprs[f0] < c->fprs[f30];             // c.lt.s f0, f30
  bc = cop1_bc;                                     // bc1t L4
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  if (bc) {goto block_6;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0

block_6:
  if (((s64)c->sgpr64(s7)) != ((s64)c->sgpr64(v1))) {// bnel s7, v1, L7
    c->mov64(v1, v1);                               // or v1, v1, r0
    goto block_18;
  }

// block_8:
  c->load_symbol2(t9, cache.vector_vector_distance);// lw t9, vector-vector-distance(s7)
  c->lwu(v1, 12, s3);                               // lwu v1, 12(s3)
  c->daddu(a0, r0, v1);                             // daddu a0, r0, v1
  c->daddu(a1, r0, s4);                             // daddu a1, r0, s4
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mtc1(f0, v0);                                  // mtc1 f0, v0
  cop1_bc = c->fprs[f0] < c->fprs[f30];             // c.lt.s f0, f30
  bc = cop1_bc;                                     // bc1t L5
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  if (bc) {goto block_10;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0

block_10:
  if (((s64)c->sgpr64(s7)) != ((s64)c->sgpr64(v1))) {// bnel s7, v1, L7
    c->mov64(v1, v1);                               // or v1, v1, r0
    goto block_18;
  }

// block_12:
  c->load_symbol2(t9, cache.vector_vector_distance);// lw t9, vector-vector-distance(s7)
  c->lwu(v1, 8, s3);                                // lwu v1, 8(s3)
  c->daddu(a0, r0, v1);                             // daddu a0, r0, v1
  c->daddiu(a1, s4, 16);                            // daddiu a1, s4, 16
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mtc1(f0, v0);                                  // mtc1 f0, v0
  cop1_bc = c->fprs[f0] < c->fprs[f30];             // c.lt.s f0, f30
  bc = cop1_bc;                                     // bc1t L6
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  if (bc) {goto block_14;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0

block_14:
  if (((s64)c->sgpr64(s7)) != ((s64)c->sgpr64(v1))) {// bnel s7, v1, L7
    c->mov64(v1, v1);                               // or v1, v1, r0
    goto block_18;
  }

// block_16:
  c->load_symbol2(t9, cache.vector_vector_distance);// lw t9, vector-vector-distance(s7)
  c->lwu(v1, 12, s3);                               // lwu v1, 12(s3)
  c->daddu(a0, r0, v1);                             // daddu a0, r0, v1
  c->daddiu(a1, s4, 16);                            // daddiu a1, s4, 16
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mtc1(f0, v0);                                  // mtc1 f0, v0
  cop1_bc = c->fprs[f0] < c->fprs[f30];             // c.lt.s f0, f30
  bc = cop1_bc;                                     // bc1t L7
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  if (bc) {goto block_18;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0

block_18:
  bc = c->sgpr64(s7) != c->sgpr64(v1);              // bne s7, v1, L8
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_20;}                          // branch non-likely

  //beq r0, r0, L10                                 // beq r0, r0, L10
  // nop                                            // sll r0, r0, 0
  goto block_24;                                    // branch always


block_20:
  c->ld(v1, 384, gp);                               // ld v1, 384(gp)
  c->andi(v1, v1, 4);                               // andi v1, v1, 4
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L9
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_23;}                          // branch non-likely

  c->lwu(v1, 12, s5);                               // lwu v1, 12(s5)
  c->daddiu(a0, v1, 16);                            // daddiu a0, v1, 16
  c->daddiu(v1, gp, 144);                           // daddiu v1, gp, 144
  c->lwc1(f0, 0, a0);                               // lwc1 f0, 0(a0)
  c->lwc1(f1, 4, a0);                               // lwc1 f1, 4(a0)
  c->lwc1(f2, 8, a0);                               // lwc1 f2, 8(a0)
  c->lwc1(f3, 0, v1);                               // lwc1 f3, 0(v1)
  c->lwc1(f4, 4, v1);                               // lwc1 f4, 4(v1)
  c->lwc1(f5, 8, v1);                               // lwc1 f5, 8(v1)
  // Unknown instr: mula.s f0, f3
  acc = c->fprs[f0] * c->fprs[f3];
  // Unknown instr: madda.s f1, f4
  acc += c->fprs[f1] * c->fprs[f4];
  // Unknown instr: madd.s f0, f2, f5
  c->fprs[f0] = acc + c->fprs[f2] * c->fprs[f5];
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->abss(f0, f0);                                  // abs.s f0, f0
  c->lui(v1, 16179);                                // lui v1, 16179
  c->ori(v1, v1, 13107);                            // ori v1, v1, 13107
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  cop1_bc = c->fprs[f1] < c->fprs[f0];              // c.lt.s f1, f0
  bc = !cop1_bc;                                    // bc1f L9
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_23;}                          // branch non-likely

  //beq r0, r0, L10                                 // beq r0, r0, L10
  // nop                                            // sll r0, r0, 0
  goto block_24;                                    // branch always


block_23:
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  c->mov64(v0, v1);                                 // or v0, v1, r0
  //beq r0, r0, L11                                 // beq r0, r0, L11
  // nop                                            // sll r0, r0, 0
  goto block_26;                                    // branch always


block_24:
  c->mov64(v0, s7);                                 // or v0, s7, r0
  //beq r0, r0, L11                                 // beq r0, r0, L11
  // nop                                            // sll r0, r0, 0
  goto block_26;                                    // branch always

  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0

block_26:
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lwc1(f30, 80, sp);                             // lwc1 f30, 80(sp)
  c->lq(gp, 64, sp);                                // lq gp, 64(sp)
  c->lq(s5, 48, sp);                                // lq s5, 48(sp)
  c->lq(s4, 32, sp);                                // lq s4, 32(sp)
  c->lq(s3, 16, sp);                                // lq s3, 16(sp)
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
  cache.vector_vector_distance = intern_from_c(-1, 0, "vector-vector-distance").c();
  gLinkedFunctionTable.reg("(method 18 collide-edge-work)", execute, 256);
}

} // namespace method_18_collide_edge_work
} // namespace Mips2C