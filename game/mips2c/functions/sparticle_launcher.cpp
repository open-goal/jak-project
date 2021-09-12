//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/kscheme.h"
namespace Mips2C {
namespace sp_init_fields {
struct Cache {
  void* part_id_table; // *part-id-table*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->mov64(v1, a0);                                 // or v1, a0, r0
  c->mov64(v1, a2);                                 // or v1, a2, r0
  c->mov64(v1, a3);                                 // or v1, a3, r0
  c->mov64(v1, t0);                                 // or v1, t0, r0
  // nop                                            // sll r0, r0, 0
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  c->mov64(v0, a1);                                 // or v0, a1, r0

  block_1:
  c->lh(a1, 0, v0);                                 // lh a1, 0(v0)
  // nop                                            // sll r0, r0, 0
  c->dsubu(a1, a1, a2);                             // dsubu a1, a1, a2
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  if (((s64)c->sgpr64(a1)) < 0) {                   // bltzl a1, L156
    c->daddiu(v0, v0, 16);                          // daddiu v0, v0, 16
    goto block_1;
  }

  c->dsubu(a1, a2, a3);                             // dsubu a1, a2, a3
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(a1)) >= 0;                   // bgez a1, L169
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_38;}                          // branch non-likely


  block_4:
  c->lh(a1, 0, v0);                                 // lh a1, 0(v0)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a1) != c->sgpr64(a2);              // bne a1, a2, L167
  c->vrget(DEST::xyzw, vf1);                        // vrget.xyzw vf1
  if (bc) {goto block_35;}                          // branch non-likely

  c->vsqrt(vf1, BC::x);                             // vsqrt Q, vf1.x
  c->lh(a1, 2, v0);                                 // lh a1, 2(v0)
  c->vaddq(DEST::x, vf2, vf0);                      // vaddq.x vf2, vf0, Q
  c->lw(t2, 8, v0);                                 // lw t2, 8(v0)
  c->addiu(v1, r0, 7);                              // addiu v1, r0, 7
  bc = c->sgpr64(a2) == c->sgpr64(v1);              // beq a2, v1, L159
  c->addiu(t1, r0, 1);                              // addiu t1, r0, 1
  if (bc) {goto block_17;}                          // branch non-likely

  bc = c->sgpr64(a1) == c->sgpr64(t1);              // beq a1, t1, L160
  c->addiu(t1, r0, 2);                              // addiu t1, r0, 2
  if (bc) {goto block_19;}                          // branch non-likely

  bc = c->sgpr64(a1) == c->sgpr64(t1);              // beq a1, t1, L162
  c->addiu(t1, r0, 3);                              // addiu t1, r0, 3
  if (bc) {goto block_24;}                          // branch non-likely

  bc = c->sgpr64(a1) == c->sgpr64(t1);              // beq a1, t1, L163
  c->addiu(t1, r0, 5);                              // addiu t1, r0, 5
  if (bc) {goto block_27;}                          // branch non-likely

  bc = c->sgpr64(a1) == c->sgpr64(t1);              // beq a1, t1, L164
  c->addiu(t1, r0, 6);                              // addiu t1, r0, 6
  if (bc) {goto block_29;}                          // branch non-likely

  bc = c->sgpr64(a1) == c->sgpr64(t1);              // beq a1, t1, L165
  c->addiu(t1, r0, 4);                              // addiu t1, r0, 4
  if (bc) {goto block_31;}                          // branch non-likely

  bc = c->sgpr64(a1) == c->sgpr64(t1);              // beq a1, t1, L166
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_33;}                          // branch non-likely

  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L158
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_15;}                          // branch non-likely

  c->vrxor(vf2, BC::w);                             // vrxorw vf2
  c->lw(t1, 12, v0);                                // lw t1, 12(v0)
  c->vrnext(DEST::xyzw, vf1);                       // vrnext.xyzw vf1
  c->lw(t3, 4, v0);                                 // lw t3, 4(v0)
  c->vsub_bc(DEST::xyzw, BC::w, vf1, vf1, vf0);     // vsubw.xyzw vf1, vf1, vf0
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf2, t2);                        // qmtc2.i vf2, t2
  // nop                                            // sll r0, r0, 0
  c->vitof0(DEST::xyzw, vf2, vf2);                  // vitof0.xyzw vf2, vf2
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf1, vf1, vf2);               // vmul.xyzw vf1, vf1, vf2
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf1, vf1);                  // vftoi0.xyzw vf1, vf1
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t2, vf1);                        // qmfc2.i t2, vf1
  // nop                                            // sll r0, r0, 0
  c->mult3(t2, t2, t1);                             // mult3 t2, t2, t1
  // nop                                            // sll r0, r0, 0
  c->daddu(t2, t2, t3);                             // daddu t2, t2, t3
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  c->sw(t2, 0, a0);                                 // sw t2, 0(a0)
  c->daddiu(a0, a0, 4);                             // daddiu a0, a0, 4
  bc = c->sgpr64(a2) != c->sgpr64(a3);              // bne a2, a3, L157
  c->daddiu(v0, v0, 16);                            // daddiu v0, v0, 16
  if (bc) {goto block_4;}                           // branch non-likely

  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return


  block_15:
  c->lw(t3, 4, v0);                                 // lw t3, 4(v0)
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  c->sw(t3, 0, a0);                                 // sw t3, 0(a0)
  c->daddiu(a0, a0, 4);                             // daddiu a0, a0, 4
  bc = c->sgpr64(a2) != c->sgpr64(a3);              // bne a2, a3, L157
  c->daddiu(v0, v0, 16);                            // daddiu v0, v0, 16
  if (bc) {goto block_4;}                           // branch non-likely

  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return


  block_17:
  c->lw(t3, 4, v0);                                 // lw t3, 4(v0)
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  c->sw(t3, 0, a0);                                 // sw t3, 0(a0)
  c->daddiu(a0, a0, 4);                             // daddiu a0, a0, 4
  bc = c->sgpr64(a2) != c->sgpr64(a3);              // bne a2, a3, L157
  c->daddiu(v0, v0, 16);                            // daddiu v0, v0, 16
  if (bc) {goto block_4;}                           // branch non-likely

  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return


  block_19:
  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L161
  c->vrxor(vf2, BC::w);                             // vrxorw vf2
  if (bc) {goto block_22;}                          // branch non-likely

  c->vrnext(DEST::xyzw, vf1);                       // vrnext.xyzw vf1
  c->lw(t1, 12, v0);                                // lw t1, 12(v0)
  c->vsub_bc(DEST::xyzw, BC::w, vf1, vf1, vf0);     // vsubw.xyzw vf1, vf1, vf0
  c->lw(t3, 4, v0);                                 // lw t3, 4(v0)
  c->mov128_vf_gpr(vf2, t2);                        // qmtc2.i vf2, t2
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf1, vf1, vf2);               // vmul.xyzw vf1, vf1, vf2
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf2, t1);                        // qmtc2.i vf2, t1
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf1, vf1, vf2);               // vmul.xyzw vf1, vf1, vf2
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf2, t3);                        // qmtc2.i vf2, t3
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyzw, vf1, vf1, vf2);               // vadd.xyzw vf1, vf1, vf2
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t2, vf1);                        // qmfc2.i t2, vf1
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  c->sw(t2, 0, a0);                                 // sw t2, 0(a0)
  c->daddiu(a0, a0, 4);                             // daddiu a0, a0, 4
  bc = c->sgpr64(a2) != c->sgpr64(a3);              // bne a2, a3, L157
  c->daddiu(v0, v0, 16);                            // daddiu v0, v0, 16
  if (bc) {goto block_4;}                           // branch non-likely

  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return


  block_22:
  c->lw(t3, 4, v0);                                 // lw t3, 4(v0)
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  c->sw(t3, 0, a0);                                 // sw t3, 0(a0)
  c->daddiu(a0, a0, 4);                             // daddiu a0, a0, 4
  bc = c->sgpr64(a2) != c->sgpr64(a3);              // bne a2, a3, L157
  c->daddiu(v0, v0, 16);                            // daddiu v0, v0, 16
  if (bc) {goto block_4;}                           // branch non-likely

  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return


  block_24:
  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L161
  c->vrxor(vf2, BC::w);                             // vrxorw vf2
  if (bc) {goto block_22;}                          // branch non-likely

  c->vrnext(DEST::xyzw, vf1);                       // vrnext.xyzw vf1
  c->lw(t1, 12, v0);                                // lw t1, 12(v0)
  c->vsub_bc(DEST::xyzw, BC::w, vf1, vf1, vf0);     // vsubw.xyzw vf1, vf1, vf0
  c->daddiu(t2, t2, 1);                             // daddiu t2, t2, 1
  c->mov128_vf_gpr(vf2, t2);                        // qmtc2.i vf2, t2
  c->lw(t3, 4, v0);                                 // lw t3, 4(v0)
  c->vitof0(DEST::xyzw, vf2, vf2);                  // vitof0.xyzw vf2, vf2
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf1, vf1, vf2);               // vmul.xyzw vf1, vf1, vf2
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf1, vf1);                  // vftoi0.xyzw vf1, vf1
  // nop                                            // sll r0, r0, 0
  c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf2, t1);                        // qmtc2.i vf2, t1
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf1, vf1, vf2);               // vmul.xyzw vf1, vf1, vf2
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf2, t3);                        // qmtc2.i vf2, t3
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyzw, vf1, vf1, vf2);               // vadd.xyzw vf1, vf1, vf2
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t2, vf1);                        // qmfc2.i t2, vf1
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  c->sw(t2, 0, a0);                                 // sw t2, 0(a0)
  c->daddiu(a0, a0, 4);                             // daddiu a0, a0, 4
  bc = c->sgpr64(a2) != c->sgpr64(a3);              // bne a2, a3, L157
  c->daddiu(v0, v0, 16);                            // daddiu v0, v0, 16
  if (bc) {goto block_4;}                           // branch non-likely

  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return


  block_27:
  c->lw(t1, 4, v0);                                 // lw t1, 4(v0)
  // nop                                            // sll r0, r0, 0
  c->dsll(t1, t1, 2);                               // dsll t1, t1, 2
  // nop                                            // sll r0, r0, 0
  c->daddu(t1, t1, a0);                             // daddu t1, t1, a0
  // nop                                            // sll r0, r0, 0
  c->lw(t3, 0, t1);                                 // lw t3, 0(t1)
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  c->sw(t3, 0, a0);                                 // sw t3, 0(a0)
  c->daddiu(a0, a0, 4);                             // daddiu a0, a0, 4
  bc = c->sgpr64(a2) != c->sgpr64(a3);              // bne a2, a3, L157
  c->daddiu(v0, v0, 16);                            // daddiu v0, v0, 16
  if (bc) {goto block_4;}                           // branch non-likely

  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return


  block_29:
  c->lw(t1, 4, v0);                                 // lw t1, 4(v0)
  // nop                                            // sll r0, r0, 0
  c->lw(t3, 0, t1);                                 // lw t3, 0(t1)
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  c->sw(t3, 0, a0);                                 // sw t3, 0(a0)
  c->daddiu(a0, a0, 4);                             // daddiu a0, a0, 4
  bc = c->sgpr64(a2) != c->sgpr64(a3);              // bne a2, a3, L157
  c->daddiu(v0, v0, 16);                            // daddiu v0, v0, 16
  if (bc) {goto block_4;}                           // branch non-likely

  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return


  block_31:
  c->load_symbol(t1, cache.part_id_table);          // lw t1, *part-id-table*(s7)
  // nop                                            // sll r0, r0, 0
  c->lw(t3, 4, v0);                                 // lw t3, 4(v0)
  // nop                                            // sll r0, r0, 0
  c->dsll(t3, t3, 2);                               // dsll t3, t3, 2
  c->daddiu(t1, t1, 12);                            // daddiu t1, t1, 12
  c->daddu(t3, t3, t1);                             // daddu t3, t3, t1
  // nop                                            // sll r0, r0, 0
  c->lw(t2, 0, t3);                                 // lw t2, 0(t3)
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  c->sw(t2, 0, a0);                                 // sw t2, 0(a0)
  c->daddiu(a0, a0, 4);                             // daddiu a0, a0, 4
  bc = c->sgpr64(a2) != c->sgpr64(a3);              // bne a2, a3, L157
  c->daddiu(v0, v0, 16);                            // daddiu v0, v0, 16
  if (bc) {goto block_4;}                           // branch non-likely

  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return


  block_33:
  c->lw(t3, 4, v0);                                 // lw t3, 4(v0)
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  c->sw(t3, 0, a0);                                 // sw t3, 0(a0)
  c->daddiu(a0, a0, 4);                             // daddiu a0, a0, 4
  bc = c->sgpr64(a2) != c->sgpr64(a3);              // bne a2, a3, L157
  c->daddiu(v0, v0, 16);                            // daddiu v0, v0, 16
  if (bc) {goto block_4;}                           // branch non-likely

  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return


  block_35:
  if (((s64)c->sgpr64(t0)) != ((s64)c->sgpr64(s7))) {// bnel t0, s7, L168
    c->sw(r0, 0, a0);                               // sw r0, 0(a0)
    goto block_37;
  }

  block_37:
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  c->daddiu(a0, a0, 4);                             // daddiu a0, a0, 4
  bc = c->sgpr64(a2) != c->sgpr64(a3);              // bne a2, a3, L157
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_4;}                           // branch non-likely


  block_38:
  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return

  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.part_id_table = intern_from_c("*part-id-table*").c();
  gLinkedFunctionTable.reg("sp-init-fields!", execute, 0);
}

} // namespace sp_init_fields
} // namespace Mips2C

