//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace method_22_grid_hash {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -48);                           // daddiu sp, sp, -48
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s5, 16, sp);                                // sq s5, 16(sp)
  c->sq(gp, 32, sp);                                // sq gp, 32(sp)
  c->lh(v1, 10, a0);                                // lh v1, 10(a0)
  c->lb(a3, 24, a0);                                // lb a3, 24(a0)
  c->mult3(a3, v1, a3);                             // mult3 a3, v1, a3
  c->lb(t0, 26, a0);                                // lb t0, 26(a0)
  c->mult3(t0, a3, t0);                             // mult3 t0, a3, t0
  c->lb(t1, 3, a1);                                 // lb t1, 3(a1)
  c->lb(t2, 0, a1);                                 // lb t2, 0(a1)
  c->dsubu(t1, t1, t2);                             // dsubu t1, t1, t2
  c->lb(t2, 5, a1);                                 // lb t2, 5(a1)
  c->lb(t3, 2, a1);                                 // lb t3, 2(a1)
  c->dsubu(t2, t2, t3);                             // dsubu t2, t2, t3
  c->lb(t3, 4, a1);                                 // lb t3, 4(a1)
  c->lb(t4, 1, a1);                                 // lb t4, 1(a1)
  c->dsubu(t3, t3, t4);                             // dsubu t3, t3, t4
  c->lb(t4, 0, a1);                                 // lb t4, 0(a1)
  c->mult3(t4, t4, v1);                             // mult3 t4, t4, v1
  c->lb(t5, 1, a1);                                 // lb t5, 1(a1)
  c->mult3(t5, t5, t0);                             // mult3 t5, t5, t0
  c->daddu(t4, t4, t5);                             // daddu t4, t4, t5
  c->lb(a1, 2, a1);                                 // lb a1, 2(a1)
  c->mult3(a1, a1, a3);                             // mult3 a1, a1, a3
  c->daddu(a1, t4, a1);                             // daddu a1, t4, a1
  c->daddu(a1, r0, a1);                             // daddu a1, r0, a1
  c->lwu(t4, 28, a0);                               // lwu t4, 28(a0)
  c->daddu(a1, a1, t4);                             // daddu a1, a1, t4
  c->lh(a0, 10, a0);                                // lh a0, 10(a0)
  c->mov64(t4, a2);                                 // or t4, a2, r0

block_1:
  c->daddiu(a0, a0, -16);                           // daddiu a0, a0, -16
  c->sq(r0, 0, t4);                                 // sq r0, 0(t4)
  bc = ((s64)c->sgpr64(a0)) > 0;                    // bgtz a0, L177
  c->daddiu(t4, t4, 16);                            // daddiu t4, t4, 16
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(a0, t3);                                 // or a0, t3, r0
  // nop                                            // sll r0, r0, 0

block_3:
  c->mov64(t3, t2);                                 // or t3, t2, r0
  c->mov64(t4, a1);                                 // or t4, a1, r0

block_4:
  c->mov64(t5, t1);                                 // or t5, t1, r0
  c->mov64(t6, t4);                                 // or t6, t4, r0

block_5:
  c->slti(t8, v1, 9);                               // slti t8, v1, 9
  c->mov64(t7, v1);                                 // or t7, v1, r0
  bc = c->sgpr64(t8) != 0;                          // bne t8, r0, L182
  c->mov64(t8, a2);                                 // or t8, a2, r0
  if (bc) {goto block_8;}                           // branch non-likely


block_6:
  c->mov64(ra, t8);                                 // or ra, t8, r0
  c->ldr(t9, 0, t6);                                // ldr t9, 0(t6)
  c->daddiu(t8, t8, 8);                             // daddiu t8, t8, 8
  c->ldl(t9, 7, t6);                                // ldl t9, 7(t6)
  c->daddiu(t7, t7, -8);                            // daddiu t7, t7, -8
  c->ld(s5, 0, ra);                                 // ld s5, 0(ra)
  c->slti(gp, t7, 8);                               // slti gp, t7, 8
  c->or_(t9, s5, t9);                               // or t9, s5, t9
  c->daddiu(t6, t6, 8);                             // daddiu t6, t6, 8
  c->sd(t9, 0, ra);                                 // sd t9, 0(ra)
  bc = c->sgpr64(gp) == 0;                          // beq gp, r0, L181
  c->gprs[t9].du64[0] = 0;                          // or t9, r0, r0
  if (bc) {goto block_6;}                           // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L183
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_9;}                           // branch non-likely


block_8:
  // nop                                            // sll r0, r0, 0
  c->ld(ra, 0, t8);                                 // ld ra, 0(t8)
  // nop                                            // sll r0, r0, 0
  c->ldr(t9, 0, t6);                                // ldr t9, 0(t6)
  // nop                                            // sll r0, r0, 0
  c->ldl(t9, 7, t6);                                // ldl t9, 7(t6)
  c->daddu(t6, t6, t7);                             // daddu t6, t6, t7
  c->or_(t7, ra, t9);                               // or t7, ra, t9
  // nop                                            // sll r0, r0, 0
  c->sd(t7, 0, t8);                                 // sd t7, 0(t8)

block_9:
  bc = ((s64)c->sgpr64(t5)) > 0;                    // bgtz t5, L180
  c->daddiu(t5, t5, -1);                            // daddiu t5, t5, -1
  if (bc) {goto block_5;}                           // branch non-likely

  c->daddu(t4, t4, a3);                             // daddu t4, t4, a3
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(t3)) > 0;                    // bgtz t3, L179
  c->daddiu(t3, t3, -1);                            // daddiu t3, t3, -1
  if (bc) {goto block_4;}                           // branch non-likely

  c->daddu(a1, a1, t0);                             // daddu a1, a1, t0
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(a0)) > 0;                    // bgtz a0, L178
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  if (bc) {goto block_3;}                           // branch non-likely

  c->mov64(v0, a2);                                 // or v0, a2, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 32, sp);                                // lq gp, 32(sp)
  c->lq(s5, 16, sp);                                // lq s5, 16(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 48);                            // daddiu sp, sp, 48
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  gLinkedFunctionTable.reg("(method 22 grid-hash)", execute, 256);
}

} // namespace method_22_grid_hash
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace method_24_grid_hash {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lq(v1, 0, a2);                                 // lq v1, 0(a2)
  c->mov128_vf_gpr(vf1, v1);                        // qmtc2.i vf1, v1
  c->lq(v1, 12, a0);                                // lq v1, 12(a0)
  c->mov128_vf_gpr(vf5, v1);                        // qmtc2.i vf5, v1
  c->vsub_bc(DEST::xyzw, BC::w, vf3, vf1, vf1);     // vsubw.xyzw vf3, vf1, vf1
  c->vadd_bc(DEST::xyzw, BC::w, vf2, vf1, vf1);     // vaddw.xyzw vf2, vf1, vf1
  c->lq(v1, 28, a0);                                // lq v1, 28(a0)
  c->mov128_vf_gpr(vf4, v1);                        // qmtc2.i vf4, v1
  c->vmr32(DEST::xyzw, vf4, vf4);                   // vmr32.xyzw vf4, vf4
  c->vsub(DEST::xyzw, vf6, vf3, vf4);               // vsub.xyzw vf6, vf3, vf4
  c->vsub(DEST::xyzw, vf7, vf2, vf4);               // vsub.xyzw vf7, vf2, vf4
  c->vmul(DEST::xyzw, vf6, vf6, vf5);               // vmul.xyzw vf6, vf6, vf5
  c->vmul(DEST::xyzw, vf7, vf7, vf5);               // vmul.xyzw vf7, vf7, vf5
  c->vftoi0(DEST::xyz, vf6, vf6);                   // vftoi0.xyz vf6, vf6
  c->vftoi0(DEST::xyz, vf7, vf7);                   // vftoi0.xyz vf7, vf7
  c->nor(v1, r0, r0);                               // nor v1, r0, r0
  c->lw(a0, 24, a0);                                // lw a0, 24(a0)
  c->sll(a2, a0, 8);                                // sll a2, a0, 8
  c->lui(a0, 1);                                    // lui a0, 1
  c->srl(a2, a2, 8);                                // srl a2, a2, 8
  c->ori(a0, a0, 257);                              // ori a0, a0, 257
  c->dsubu(a0, a2, a0);                             // dsubu a0, a2, a0
  // nop                                            // sll r0, r0, 0
  c->pextlb(a0, r0, a0);                            // pextlb a0, r0, a0
  // nop                                            // sll r0, r0, 0
  c->pextlh(a0, r0, a0);                            // pextlh a0, r0, a0
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a3, vf6);                        // qmfc2.i a3, vf6
  c->mov128_gpr_vf(a2, vf7);                        // qmfc2.i a2, vf7
  c->pmaxw(a3, a3, r0);                             // pmaxw a3, a3, r0
  c->pmaxw(a2, a2, r0);                             // pmaxw a2, a2, r0
  c->pminw(a3, a3, a0);                             // pminw a3, a3, a0
  c->pminw(a0, a2, a0);                             // pminw a0, a2, a0
  c->ppach(a2, r0, a3);                             // ppach a2, r0, a3
  c->ppach(a3, r0, a0);                             // ppach a3, r0, a0
  c->ppacb(a0, r0, a2);                             // ppacb a0, r0, a2
  c->ppacb(a2, r0, a3);                             // ppacb a2, r0, a3
  c->dsll(a2, a2, 24);                              // dsll a2, a2, 24
  c->or_(a0, a0, a2);                               // or a0, a0, a2
  c->dsll32(v1, v1, 16);                            // dsll32 v1, v1, 16
  c->ld(a2, 0, a1);                                 // ld a2, 0(a1)
  c->and_(v1, a2, v1);                              // and v1, a2, v1
  c->or_(v1, v1, a0);                               // or v1, v1, a0
  c->sd(v1, 0, a1);                                 // sd v1, 0(a1)
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
  gLinkedFunctionTable.reg("(method 24 grid-hash)", execute, 0);
}

} // namespace method_24_grid_hash
} // namespace Mips2C
