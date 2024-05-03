//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace sp_init_fields {
struct Cache {
  void* part_id_table; // *part-id-table*
  void* sp_temp; // *sp-temp*
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
  if (((s64)c->sgpr64(a1)) < 0) {                   // bltzl a1, L350
    c->daddiu(v0, v0, 16);                          // daddiu v0, v0, 16
    goto block_1;
  }

// block_3:
  c->dsubu(a1, a2, a3);                             // dsubu a1, a2, a3
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(a1)) >= 0;                   // bgez a1, L366
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_47;}                          // branch non-likely


block_4:
  c->lh(a1, 0, v0);                                 // lh a1, 0(v0)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a1) != c->sgpr64(a2);              // bne a1, a2, L364
  c->vrget(DEST::xyzw, vf1);                        // vrget.xyzw vf1
  if (bc) {goto block_44;}                          // branch non-likely

  c->vsqrt(vf1, BC::x);                             // vsqrt Q, vf1.x
  c->lh(a1, 2, v0);                                 // lh a1, 2(v0)
  c->vaddq(DEST::x, vf2, vf0);                      // vaddq.x vf2, vf0, Q
  c->lw(t2, 8, v0);                                 // lw t2, 8(v0)
  c->addiu(v1, r0, 7);                              // addiu v1, r0, 7
  bc = c->sgpr64(a2) == c->sgpr64(v1);              // beq a2, v1, L353
  c->addiu(t1, r0, 1);                              // addiu t1, r0, 1
  if (bc) {goto block_19;}                          // branch non-likely

  bc = c->sgpr64(a1) == c->sgpr64(t1);              // beq a1, t1, L354
  c->addiu(t1, r0, 2);                              // addiu t1, r0, 2
  if (bc) {goto block_21;}                          // branch non-likely

  bc = c->sgpr64(a1) == c->sgpr64(t1);              // beq a1, t1, L358
  c->addiu(t1, r0, 7);                              // addiu t1, r0, 7
  if (bc) {goto block_31;}                          // branch non-likely

  bc = c->sgpr64(a1) == c->sgpr64(t1);              // beq a1, t1, L356
  c->addiu(t1, r0, 3);                              // addiu t1, r0, 3
  if (bc) {goto block_26;}                          // branch non-likely

  bc = c->sgpr64(a1) == c->sgpr64(t1);              // beq a1, t1, L359
  c->addiu(t1, r0, 5);                              // addiu t1, r0, 5
  if (bc) {goto block_34;}                          // branch non-likely

  bc = c->sgpr64(a1) == c->sgpr64(t1);              // beq a1, t1, L361
  c->addiu(t1, r0, 6);                              // addiu t1, r0, 6
  if (bc) {goto block_38;}                          // branch non-likely

  bc = c->sgpr64(a1) == c->sgpr64(t1);              // beq a1, t1, L362
  c->addiu(t1, r0, 4);                              // addiu t1, r0, 4
  if (bc) {goto block_40;}                          // branch non-likely

  bc = c->sgpr64(a1) == c->sgpr64(t1);              // beq a1, t1, L363
  c->addiu(t1, r0, 8);                              // addiu t1, r0, 8
  if (bc) {goto block_42;}                          // branch non-likely

  bc = c->sgpr64(a1) == c->sgpr64(t1);              // beq a1, t1, L360
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_36;}                          // branch non-likely

  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L352
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_17;}                          // branch non-likely

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
  bc = c->sgpr64(a2) != c->sgpr64(a3);              // bne a2, a3, L351
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
  bc = c->sgpr64(a2) != c->sgpr64(a3);              // bne a2, a3, L351
  c->daddiu(v0, v0, 16);                            // daddiu v0, v0, 16
  if (bc) {goto block_4;}                           // branch non-likely

  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return


block_19:
  c->lw(t3, 4, v0);                                 // lw t3, 4(v0)
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  c->sw(t3, 0, a0);                                 // sw t3, 0(a0)
  c->daddiu(a0, a0, 4);                             // daddiu a0, a0, 4
  bc = c->sgpr64(a2) != c->sgpr64(a3);              // bne a2, a3, L351
  c->daddiu(v0, v0, 16);                            // daddiu v0, v0, 16
  if (bc) {goto block_4;}                           // branch non-likely

  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return


block_21:
  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L355
  c->vrxor(vf2, BC::w);                             // vrxorw vf2
  if (bc) {goto block_24;}                          // branch non-likely

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
  bc = c->sgpr64(a2) != c->sgpr64(a3);              // bne a2, a3, L351
  c->daddiu(v0, v0, 16);                            // daddiu v0, v0, 16
  if (bc) {goto block_4;}                           // branch non-likely

  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return


block_24:
  c->lw(t3, 4, v0);                                 // lw t3, 4(v0)
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  c->sw(t3, 0, a0);                                 // sw t3, 0(a0)
  c->daddiu(a0, a0, 4);                             // daddiu a0, a0, 4
  bc = c->sgpr64(a2) != c->sgpr64(a3);              // bne a2, a3, L351
  c->daddiu(v0, v0, 16);                            // daddiu v0, v0, 16
  if (bc) {goto block_4;}                           // branch non-likely

  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return


block_26:
  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L357
  c->vrxor(vf2, BC::w);                             // vrxorw vf2
  if (bc) {goto block_29;}                          // branch non-likely

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
  c->store_symbol2(t2, cache.sp_temp);              // sw t2, *sp-temp*(s7)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a2) != c->sgpr64(a3);              // bne a2, a3, L351
  c->daddiu(v0, v0, 16);                            // daddiu v0, v0, 16
  if (bc) {goto block_4;}                           // branch non-likely

  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return


block_29:
  c->lw(t3, 4, v0);                                 // lw t3, 4(v0)
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  c->sw(t3, 0, a0);                                 // sw t3, 0(a0)
  c->daddiu(a0, a0, 4);                             // daddiu a0, a0, 4
  c->store_symbol2(t3, cache.sp_temp);              // sw t3, *sp-temp*(s7)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a2) != c->sgpr64(a3);              // bne a2, a3, L351
  c->daddiu(v0, v0, 16);                            // daddiu v0, v0, 16
  if (bc) {goto block_4;}                           // branch non-likely

  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return


block_31:
  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L355
  c->vrxor(vf2, BC::w);                             // vrxorw vf2
  if (bc) {goto block_24;}                          // branch non-likely

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
  bc = c->sgpr64(a2) != c->sgpr64(a3);              // bne a2, a3, L351
  c->daddiu(v0, v0, 16);                            // daddiu v0, v0, 16
  if (bc) {goto block_4;}                           // branch non-likely

  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return


block_34:
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
  bc = c->sgpr64(a2) != c->sgpr64(a3);              // bne a2, a3, L351
  c->daddiu(v0, v0, 16);                            // daddiu v0, v0, 16
  if (bc) {goto block_4;}                           // branch non-likely

  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return


block_36:
  // nop                                            // sll r0, r0, 0
  c->lw(t1, 4, v0);                                 // lw t1, 4(v0)
  c->dsll(t1, t1, 2);                               // dsll t1, t1, 2
  // nop                                            // sll r0, r0, 0
  c->daddu(t1, t1, a0);                             // daddu t1, t1, a0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lwc1(f2, 0, t1);                               // lwc1 f2, 0(t1)
  // nop                                            // sll r0, r0, 0
  c->lwc1(f1, 12, v0);                              // lwc1 f1, 12(v0)
  c->cvtsw(f2, f2);                                 // cvt.s.w f2, f2
  // nop                                            // sll r0, r0, 0
  c->muls(f1, f1, f2);                              // mul.s f1, f1, f2
  // nop                                            // sll r0, r0, 0
  c->cvtws(f1, f1);                                 // cvt.w.s f1, f1
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  c->swc1(f1, 0, a0);                               // swc1 f1, 0(a0)
  c->daddiu(a0, a0, 4);                             // daddiu a0, a0, 4
  bc = c->sgpr64(a2) != c->sgpr64(a3);              // bne a2, a3, L351
  c->daddiu(v0, v0, 16);                            // daddiu v0, v0, 16
  if (bc) {goto block_4;}                           // branch non-likely

  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return


block_38:
  c->lw(t1, 4, v0);                                 // lw t1, 4(v0)
  // nop                                            // sll r0, r0, 0
  c->lw(t3, -1, t1);                                // lw t3, -1(t1)
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  c->sw(t3, 0, a0);                                 // sw t3, 0(a0)
  c->daddiu(a0, a0, 4);                             // daddiu a0, a0, 4
  bc = c->sgpr64(a2) != c->sgpr64(a3);              // bne a2, a3, L351
  c->daddiu(v0, v0, 16);                            // daddiu v0, v0, 16
  if (bc) {goto block_4;}                           // branch non-likely

  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return


block_40:
  c->load_symbol2(t1, cache.part_id_table);         // lw t1, *part-id-table*(s7)
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
  bc = c->sgpr64(a2) != c->sgpr64(a3);              // bne a2, a3, L351
  c->daddiu(v0, v0, 16);                            // daddiu v0, v0, 16
  if (bc) {goto block_4;}                           // branch non-likely

  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return


block_42:
  c->lw(t3, 4, v0);                                 // lw t3, 4(v0)
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  c->sw(t3, 0, a0);                                 // sw t3, 0(a0)
  c->daddiu(a0, a0, 4);                             // daddiu a0, a0, 4
  bc = c->sgpr64(a2) != c->sgpr64(a3);              // bne a2, a3, L351
  c->daddiu(v0, v0, 16);                            // daddiu v0, v0, 16
  if (bc) {goto block_4;}                           // branch non-likely

  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return


block_44:
  if (((s64)c->sgpr64(t0)) != ((s64)c->sgpr64(s7))) {// bnel t0, s7, L365
    c->sw(r0, 0, a0);                               // sw r0, 0(a0)
    goto block_46;
  }

block_46:
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  c->daddiu(a0, a0, 4);                             // daddiu a0, a0, 4
  bc = c->sgpr64(a2) != c->sgpr64(a3);              // bne a2, a3, L351
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_4;}                           // branch non-likely


block_47:
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
  cache.part_id_table = intern_from_c(-1, 0, "*part-id-table*").c();
  cache.sp_temp = intern_from_c(-1, 0, "*sp-temp*").c();
  gLinkedFunctionTable.reg("sp-init-fields!", execute, 256);
}

} // namespace sp_init_fields
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace particle_adgif {
struct Cache {
  void* particle_adgif_cache; // *particle-adgif-cache*
  void* particle_setup_adgif; // particle-setup-adgif
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  // nop                                            // sll r0, r0, 0
  c->dsra(a3, a1, 20);                              // dsra a3, a1, 20
  c->load_symbol2(t1, cache.particle_adgif_cache);  // lw t1, *particle-adgif-cache*(s7)
  c->dsra(t0, a1, 8);                               // dsra t0, a1, 8
  c->lw(t2, 0, t1);                                 // lw t2, 0(t1)
  c->xor_(a3, a3, t0);                              // xor a3, a3, t0
  c->lhu(v1, 4, t1);                                // lhu v1, 4(t1)
  c->andi(a3, a3, 65535);                           // andi a3, a3, 65535
  c->lw(t4, 8, t1);                                 // lw t4, 8(t1)
  bc = c->sgpr64(v1) == c->sgpr64(a3);              // beq v1, a3, L343
  c->daddiu(t3, t1, 12);                            // daddiu t3, t1, 12
  if (bc) {goto block_7;}                           // branch non-likely

  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L342
  c->daddiu(t4, t1, 172);                           // daddiu t4, t1, 172
  if (bc) {goto block_4;}                           // branch non-likely


block_2:
  c->lhu(v1, 0, t3);                                // lhu v1, 0(t3)
  c->daddiu(t3, t3, 2);                             // daddiu t3, t3, 2
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(v1) == c->sgpr64(a3);              // beq v1, a3, L343
  c->daddiu(t2, t2, -1);                            // daddiu t2, t2, -1
  if (bc) {goto block_7;}                           // branch non-likely

  bc = c->sgpr64(t2) != 0;                          // bne t2, r0, L341
  c->daddiu(t4, t4, 80);                            // daddiu t4, t4, 80
  if (bc) {goto block_2;}                           // branch non-likely


block_4:
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
  c->lw(v1, 0, t1);                                 // lw v1, 0(t1)
  c->daddiu(v1, v1, -80);                           // daddiu v1, v1, -80
  c->sw(a0, 0, sp);                                 // sw a0, 0(sp)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L344
  c->daddiu(v1, v1, 81);                            // daddiu v1, v1, 81
  if (bc) {goto block_8;}                           // branch non-likely

  c->sh(a3, 0, t3);                                 // sh a3, 0(t3)
  // nop                                            // sll r0, r0, 0
  c->sw(t4, 4, sp);                                 // sw t4, 4(sp)
  c->mov64(a0, t4);                                 // or a0, t4, r0
  c->load_symbol2(t9, cache.particle_setup_adgif);  // lw t9, particle-setup-adgif(s7)
  // nop                                            // sll r0, r0, 0
  c->sw(ra, 8, sp);                                 // sw ra, 8(sp)
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sw(v1, 0, t1);                                 // sw v1, 0(t1)
  c->jalr(call_addr);                               // jalr ra, t9
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 8, t4);                                 // lw v1, 8(t4)
  c->lw(a0, 0, sp);                                 // lw a0, 0(sp)
  // nop                                            // sll r0, r0, 0
  c->lw(t4, 4, sp);                                 // lw t4, 4(sp)
  c->andi(v1, v1, 1024);                            // andi v1, v1, 1024
  c->lw(ra, 8, sp);                                 // lw ra, 8(sp)
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L343
  c->lw(v1, 0, t1);                                 // lw v1, 0(t1)
  if (bc) {goto block_7;}                           // branch non-likely

  c->daddiu(v1, v1, -1);                            // daddiu v1, v1, -1
  c->sw(v1, 0, t1);                                 // sw v1, 0(t1)

block_7:
  c->lqc2(vf16, 0, t4);                             // lqc2 vf16, 0(t4)
  c->lqc2(vf17, 16, t4);                            // lqc2 vf17, 16(t4)
  c->lqc2(vf18, 32, t4);                            // lqc2 vf18, 32(t4)
  c->lqc2(vf19, 48, t4);                            // lqc2 vf19, 48(t4)
  c->lqc2(vf20, 64, t4);                            // lqc2 vf20, 64(t4)
  c->sqc2(vf16, 0, a0);                             // sqc2 vf16, 0(a0)
  c->sqc2(vf17, 16, a0);                            // sqc2 vf17, 16(a0)
  c->sqc2(vf18, 32, a0);                            // sqc2 vf18, 32(a0)
  c->sqc2(vf19, 48, a0);                            // sqc2 vf19, 48(a0)
  c->sqc2(vf20, 64, a0);                            // sqc2 vf20, 64(a0)
  c->sw(t4, 8, t1);                                 // sw t4, 8(t1)
  c->sh(a3, 4, t1);                                 // sh a3, 4(t1)
  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return


block_8:
  c->sw(t4, 4, sp);                                 // sw t4, 4(sp)
  // nop                                            // sll r0, r0, 0
  c->load_symbol2(t9, cache.particle_setup_adgif);  // lw t9, particle-setup-adgif(s7)
  // nop                                            // sll r0, r0, 0
  c->sw(ra, 8, sp);                                 // sw ra, 8(sp)
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lw(t4, 4, sp);                                 // lw t4, 4(sp)
  // nop                                            // sll r0, r0, 0
  c->lw(ra, 8, sp);                                 // lw ra, 8(sp)
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return

  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.particle_adgif_cache = intern_from_c(-1, 0, "*particle-adgif-cache*").c();
  cache.particle_setup_adgif = intern_from_c(-1, 0, "particle-setup-adgif").c();
  gLinkedFunctionTable.reg("particle-adgif", execute, 128);
}

} // namespace particle_adgif
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace sp_launch_particles_var {
struct Cache {
  void* level; // *level*
  void* sp_launcher_enable; // *sp-launcher-enable*
  void* sp_launcher_lock; // *sp-launcher-lock*
  void* time_of_day_context; // *time-of-day-context*
  void* active; // active
  void* add_to_sprite_aux_list; // add-to-sprite-aux-list
  void* cos; // cos
  void* new_sound_id; // new-sound-id
  void* particle_adgif; // particle-adgif
  void* quaternion_axis_angle; // quaternion-axis-angle!
  void* sin; // sin
  void* sound_play_by_spec; // sound-play-by-spec
  void* sp_adjust_launch; // sp-adjust-launch
  void* sp_euler_convert; // sp-euler-convert
  void* sp_get_particle; // sp-get-particle
  void* sp_init_fields; // sp-init-fields!
  void* sp_queue_launch; // sp-queue-launch
  void* sp_rotate_system; // sp-rotate-system
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  u16 vis[16];
  c->mov64(v1, a0);                                 // or v1, a0, r0
  c->mov64(v1, a1);                                 // or v1, a1, r0
  c->mov64(v1, a2);                                 // or v1, a2, r0
  c->mov64(v1, a3);                                 // or v1, a3, r0
  c->mov64(v1, t0);                                 // or v1, t0, r0
  c->mov64(v1, t1);                                 // or v1, t1, r0
  // nop                                            // sll r0, r0, 0
  c->daddiu(sp, sp, -304);                          // daddiu sp, sp, -304
  // nop                                            // sll r0, r0, 0
  c->load_symbol2(v1, cache.sp_launcher_enable);    // lw v1, *sp-launcher-enable*(s7)
  c->sw(ra, 0, sp);                                 // sw ra, 0(sp)
  bc = c->sgpr64(v1) == c->sgpr64(s7);              // beq v1, s7, L292
  c->sq(s0, 16, sp);                                // sq s0, 16(sp)
  if (bc) {goto block_6;}                           // branch non-likely

  c->load_symbol2(v1, cache.sp_launcher_lock);      // lw v1, *sp-launcher-lock*(s7)
  c->sq(s1, 32, sp);                                // sq s1, 32(sp)
  bc = c->sgpr64(v1) == c->sgpr64(s7);              // beq v1, s7, L294
  c->sq(s2, 48, sp);                                // sq s2, 48(sp)
  if (bc) {goto block_8;}                           // branch non-likely

  bc = c->sgpr64(a3) != c->sgpr64(s7);              // bne a3, s7, L293
  c->lui(v1, 16256);                                // lui v1, 16256
  if (bc) {goto block_7;}                           // branch non-likely

  bc = c->sgpr64(t0) != c->sgpr64(s7);              // bne t0, s7, L293
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  bc = c->sgpr64(t1) != c->sgpr64(v1);              // bne t1, v1, L293
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  c->load_symbol2(t9, cache.sp_queue_launch);       // lw t9, sp-queue-launch(s7)
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lw(ra, 0, sp);                                 // lw ra, 0(sp)
  // nop                                            // sll r0, r0, 0

block_6:
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 304);                           // daddiu sp, sp, 304
  goto end_of_function;                             // return


block_7:
  c->sd(r0, 2, r0);                                 // sd r0, 2(r0)
  // nop                                            // sll r0, r0, 0

block_8:
  c->sq(s3, 240, sp);                               // sq s3, 240(sp)
  // nop                                            // sll r0, r0, 0
  c->sq(s4, 256, sp);                               // sq s4, 256(sp)
  // nop                                            // sll r0, r0, 0
  c->sq(s5, 272, sp);                               // sq s5, 272(sp)
  // nop                                            // sll r0, r0, 0
  c->sq(s6, 288, sp);                               // sq s6, 288(sp)
  c->sqc2(vf0, 128, sp);                            // sqc2 vf0, 128(sp)
  c->lwu(v1, 8, s6);                                // lwu v1, 8(s6)
  c->lh(v1, 0, v1);                                 // lh v1, 0(v1)
  vis[14] = c->gpr_src(v1).du16[0];                 // ctc2.i vi14, v1
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf30, 0, a2);                             // lqc2 vf30, 0(a2)
  c->sqc2(vf30, 80, sp);                            // sqc2 vf30, 80(sp)
  c->lqc2(vf30, 16, a2);                            // lqc2 vf30, 16(a2)
  c->sqc2(vf30, 96, sp);                            // sqc2 vf30, 96(sp)
  c->lqc2(vf30, 32, a2);                            // lqc2 vf30, 32(a2)
  c->sqc2(vf30, 112, sp);                           // sqc2 vf30, 112(sp)
  c->lqc2(vf30, 48, a2);                            // lqc2 vf30, 48(a2)
  c->mtc1(f1, t1);                                  // mtc1 f1, t1
  c->sqc2(vf30, 64, sp);                            // sqc2 vf30, 64(sp)
  c->lui(v1, 17279);                                // lui v1, 17279
  c->mov64(s3, a0);                                 // or s3, a0, r0
  c->mov128_vf_gpr(vf31, v1);                       // qmtc2.i vf31, v1
  c->mov64(s4, a1);                                 // or s4, a1, r0
  c->lw(s6, 24, s3);                                // lw s6, 24(s3)
  c->mov64(s0, a3);                                 // or s0, a3, r0
  c->mov64(s1, t0);                                 // or s1, t0, r0
  c->lw(a1, 8, a1);                                 // lw a1, 8(a1)
  c->daddiu(a0, sp, 160);                           // daddiu a0, sp, 160
  c->addiu(a2, r0, 0);                              // addiu a2, r0, 0
  c->addiu(a3, r0, 8);                              // addiu a3, r0, 8
  c->load_symbol2(t9, cache.sp_init_fields);        // lw t9, sp-init-fields!(s7)
  c->daddiu(t0, s7, 4);                             // daddiu t0, s7, 4
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->sw(v0, 12, sp);                                // sw v0, 12(sp)
  // nop                                            // sll r0, r0, 0
  c->lwc1(f2, 180, sp);                             // lwc1 f2, 180(sp)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s0) == c->sgpr64(s7);              // beq s0, s7, L295
  c->muls(f1, f2, f1);                              // mul.s f1, f2, f1
  if (bc) {goto block_11;}                          // branch non-likely

  c->lwc1(f2, 24, s0);                              // lwc1 f2, 24(s0)
  // nop                                            // sll r0, r0, 0
  c->adds(f2, f2, f1);                              // add.s f2, f2, f1
  // nop                                            // sll r0, r0, 0
  c->swc1(f2, 24, s0);                              // swc1 f2, 24(s0)
  // nop                                            // sll r0, r0, 0
  c->cvtws(f2, f2);                                 // cvt.w.s f2, f2
  // nop                                            // sll r0, r0, 0
  c->sw(s1, 32, s0);                                // sw s1, 32(s0)
  // nop                                            // sll r0, r0, 0
  c->mfc1(v1, f2);                                  // mfc1 v1, f2
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L323
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_76;}                          // branch non-likely

  //beq r0, r0, L296                                // beq r0, r0, L296
  // nop                                            // sll r0, r0, 0
  goto block_12;                                    // branch always


block_11:
  // nop                                            // sll r0, r0, 0
  c->lwc1(f2, 0, s4);                               // lwc1 f2, 0(s4)
  // nop                                            // sll r0, r0, 0
  c->adds(f2, f2, f1);                              // add.s f2, f2, f1
  // nop                                            // sll r0, r0, 0
  c->swc1(f2, 0, s4);                               // swc1 f2, 0(s4)
  // nop                                            // sll r0, r0, 0
  c->cvtws(f2, f2);                                 // cvt.w.s f2, f2
  // nop                                            // sll r0, r0, 0
  c->mfc1(v1, f2);                                  // mfc1 v1, f2
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L323
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_76;}                          // branch non-likely


block_12:
  c->addiu(a1, r0, 0);                              // addiu a1, r0, 0
  c->mov64(a2, s7);                                 // or a2, s7, r0
  bc = c->sgpr64(s1) == c->sgpr64(s7);              // beq s1, s7, L297
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_15;}                          // branch non-likely

  c->lw(v1, 12, s1);                                // lw v1, 12(s1)
  bc = c->sgpr64(v1) == c->sgpr64(s7);              // beq v1, s7, L297
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_15;}                          // branch non-likely

  c->lh(v1, 6, v1);                                 // lh v1, 6(v1)
  c->andi(a0, v1, 4);                               // andi a0, v1, 4
  c->addiu(a3, r0, 1);                              // addiu a3, r0, 1
  c->movn(a1, a3, a0);                              // movn a1, a3, a0
  c->andi(a0, v1, 8);                               // andi a0, v1, 8
  c->movn(a2, s0, a0);                              // movn a2, s0, a0
  // nop                                            // sll r0, r0, 0

block_15:
  c->load_symbol2(t9, cache.sp_get_particle);       // lw t9, sp-get-particle(s7)
  c->mov64(a0, s3);                                 // or a0, s3, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9
  bc = c->sgpr64(v0) == c->sgpr64(s7);              // beq v0, s7, L323
  c->mov64(s2, v0);                                 // or s2, v0, r0
  if (bc) {goto block_76;}                          // branch non-likely

  c->daddiu(a0, sp, 192);                           // daddiu a0, sp, 192
  c->lw(a1, 12, sp);                                // lw a1, 12(sp)
  c->addiu(a2, r0, 9);                              // addiu a2, r0, 9
  c->addiu(a3, r0, 22);                             // addiu a3, r0, 22
  c->load_symbol2(t9, cache.sp_init_fields);        // lw t9, sp-init-fields!(s7)
  c->daddiu(t0, s7, 4);                             // daddiu t0, s7, 4
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->daddiu(a0, s2, 12);                            // daddiu a0, s2, 12
  c->mov64(a1, v0);                                 // or a1, v0, r0
  c->addiu(a2, r0, 23);                             // addiu a2, r0, 23
  c->addiu(a3, r0, 52);                             // addiu a3, r0, 52
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->daddiu(t0, s7, 4);                             // daddiu t0, s7, 4
  c->jalr(call_addr);                               // jalr ra, t9
  c->sw(v0, 144, sp);                               // sw v0, 144(sp)
  // nop                                            // sll r0, r0, 0
  c->lw(s5, 104, s2);                               // lw s5, 104(s2)
  // nop                                            // sll r0, r0, 0
  c->andi(v1, s5, 8);                               // andi v1, s5, 8
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L298
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_18;}                          // branch non-likely

  c->ori(s5, s5, 16384);                            // ori s5, s5, 16384
  // nop                                            // sll r0, r0, 0
  c->sw(s5, 104, s2);                               // sw s5, 104(s2)
  // nop                                            // sll r0, r0, 0

block_18:
  bc = c->sgpr64(s6) != c->sgpr64(s7);              // bne s6, s7, L301
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_26;}                          // branch non-likely

  bc = c->sgpr64(s1) == c->sgpr64(s7);              // beq s1, s7, L299
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_24;}                          // branch non-likely

  c->lw(v1, 12, s1);                                // lw v1, 12(s1)
  bc = c->sgpr64(v1) == c->sgpr64(s7);              // beq v1, s7, L299
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_24;}                          // branch non-likely

  c->lh(v1, 6, v1);                                 // lh v1, 6(v1)
  c->andi(v1, v1, 4);                               // andi v1, v1, 4
  c->mov64(a0, s5);                                 // or a0, s5, r0
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L299
  c->lb(v1, 28, s1);                                // lb v1, 28(s1)
  if (bc) {goto block_24;}                          // branch non-likely

  c->andi(a1, a0, 32768);                           // andi a1, a0, 32768
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a1) != 0;                          // bne a1, r0, L300
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_25;}                          // branch non-likely

  c->andi(a0, a0, 128);                             // andi a0, a0, 128
  c->addiu(a1, r0, 1);                              // addiu a1, r0, 1
  c->movn(v1, a1, a0);                              // movn v1, a1, a0
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L300                                // beq r0, r0, L300
  c->sw(v1, 212, sp);                               // sw v1, 212(sp)
  goto block_25;                                    // branch always


block_24:
  c->andi(v1, s5, 16384);                           // andi v1, s5, 16384
  // nop                                            // sll r0, r0, 0
  c->dsra(v1, v1, 14);                              // dsra v1, v1, 14
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 212, sp);                               // sw v1, 212(sp)
  // nop                                            // sll r0, r0, 0

block_25:
  c->lwc1(f2, 216, sp);                             // lwc1 f2, 216(sp)
  // nop                                            // sll r0, r0, 0
  c->cvtws(f2, f2);                                 // cvt.w.s f2, f2
  // nop                                            // sll r0, r0, 0
  c->mfc1(v1, f2);                                  // mfc1 v1, f2
  // nop                                            // sll r0, r0, 0
  c->dsll32(v1, v1, 16);                            // dsll32 v1, v1, 16
  // nop                                            // sll r0, r0, 0
  c->dsra32(v1, v1, 16);                            // dsra32 v1, v1, 16
  // nop                                            // sll r0, r0, 0
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  // nop                                            // sll r0, r0, 0
  c->cvtsw(f2, f2);                                 // cvt.s.w f2, f2
  // nop                                            // sll r0, r0, 0
  c->swc1(f2, 216, sp);                             // swc1 f2, 216(sp)
  // nop                                            // sll r0, r0, 0

block_26:
  c->andi(v1, s5, 128);                             // andi v1, s5, 128
  c->lqc2(vf4, 224, sp);                            // lqc2 vf4, 224(sp)
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L302
  c->vmini_bc(DEST::xyz, BC::x, vf4, vf4, vf31);    // vminix.xyz vf4, vf4, vf31
  if (bc) {goto block_28;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf4, vf4);                  // vftoi0.xyzw vf4, vf4
  // nop                                            // sll r0, r0, 0
  c->vitof0(DEST::xyzw, vf4, vf4);                  // vitof0.xyzw vf4, vf4
  c->addiu(v1, r0, -2);                             // addiu v1, r0, -2
  c->mov128_gpr_vf(a0, vf4);                        // qmfc2.i a0, vf4
  c->and_(a0, a0, v1);                              // and a0, a0, v1
  c->andi(v1, s5, 16384);                           // andi v1, s5, 16384
  c->sra(v1, v1, 14);                               // sra v1, v1, 14
  // nop                                            // sll r0, r0, 0
  c->or_(a0, a0, v1);                               // or a0, a0, v1
  // nop                                            // sll r0, r0, 0
  c->sq(a0, 224, sp);                               // sq a0, 224(sp)
  // nop                                            // sll r0, r0, 0

block_28:
  c->gprs[v1].du64[0] = vis[14];                    // cfc2.i v1, vi14
  c->sb(v1, 129, s2);                               // sb v1, 129(s2)
  bc = c->sgpr64(s1) == c->sgpr64(s7);              // beq s1, s7, L303
  c->andi(v1, s5, 64);                              // andi v1, s5, 64
  if (bc) {goto block_31;}                          // branch non-likely

  c->lwu(a0, 16, s1);                               // lwu a0, 16(s1)
  c->lwu(a0, 8, a0);                                // lwu a0, 8(a0)
  c->lh(a0, 0, a0);                                 // lh a0, 0(a0)
  c->sb(a0, 129, s2);                               // sb a0, 129(s2)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L303
  c->lw(a0, 192, sp);                               // lw a0, 192(sp)
  if (bc) {goto block_31;}                          // branch non-likely

  c->load_symbol2(t9, cache.cos);                   // lw t9, cos(s7)
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(a1, v0);                                 // or a1, v0, r0
  c->lw(a0, 192, sp);                               // lw a0, 192(sp)
  c->load_symbol2(t9, cache.sin);                   // lw t9, sin(s7)
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->ori(v1, r0, 32768);                            // ori v1, r0, 32768
  c->dsll(v1, v1, 16);                              // dsll v1, v1, 16
  c->lw(a0, 200, sp);                               // lw a0, 200(sp)
  c->xor_(a3, v0, v1);                              // xor a3, v0, v1
  c->sw(a0, 8, s2);                                 // sw a0, 8(s2)
  c->gprs[a2].du64[0] = 0;                          // or a2, r0, r0
  c->lw(t0, 196, sp);                               // lw t0, 196(sp)
  c->load_symbol2(t9, cache.quaternion_axis_angle); // lw t9, quaternion-axis-angle!(s7)
  c->daddiu(a0, s2, 80);                            // daddiu a0, s2, 80
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lw(v1, 16, s0);                                // lw v1, 16(s0)
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 0, v1);                                 // lw v1, 0(v1)
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 108, s2);                               // sw v1, 108(s2)
  // nop                                            // sll r0, r0, 0

block_31:
  c->sw(s7, 136, s2);                               // sw s7, 136(s2)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s0) == c->sgpr64(s7);              // beq s0, s7, L307
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_40;}                          // branch non-likely

  c->lw(a0, 0, s0);                                 // lw a0, 0(s0)
  // nop                                            // sll r0, r0, 0
  c->lw(a1, 24, a0);                                // lw a1, 24(a0)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L307
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_40;}                          // branch non-likely

  c->lw(a2, 0, s1);                                 // lw a2, 0(s1)
  c->daddiu(a3, s1, 108);                           // daddiu a3, s1, 108

block_34:
  c->lw(v1, 0, a3);                                 // lw v1, 0(a3)
  c->daddiu(a2, a2, -1);                            // daddiu a2, a2, -1
  c->lw(v1, 0, v1);                                 // lw v1, 0(v1)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(v1) != c->sgpr64(a1);              // bne v1, a1, L306
  c->lh(v1, 4, a3);                                 // lh v1, 4(a3)
  if (bc) {goto block_39;}                          // branch non-likely

  c->andi(a0, v1, 1);                               // andi a0, v1, 1
  c->ori(v1, v1, 1);                                // ori v1, v1, 1
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L306
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_39;}                          // branch non-likely

  c->sh(v1, 4, a3);                                 // sh v1, 4(a3)
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 0, s2);                                 // lw v1, 0(s2)
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 8, a3);                                 // sw v1, 8(a3)
  c->mov64(a0, s7);                                 // or a0, s7, r0
  if (((s64)c->sgpr64(s6)) != ((s64)c->sgpr64(s7))) {// bnel s6, s7, L305
    c->lw(a0, 0, s2);                               // lw a0, 0(s2)
    goto block_38;
  }

block_38:
  c->sw(a0, 12, a3);                                // sw a0, 12(a3)
  // nop                                            // sll r0, r0, 0
  c->sw(s2, 16, a3);                                // sw s2, 16(a3)
  // nop                                            // sll r0, r0, 0
  c->sw(a3, 136, s2);                               // sw a3, 136(s2)
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L307                                // beq r0, r0, L307
  // nop                                            // sll r0, r0, 0
  goto block_40;                                    // branch always


block_39:
  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L304
  c->daddiu(a3, a3, 48);                            // daddiu a3, a3, 48
  if (bc) {goto block_34;}                          // branch non-likely


block_40:
  c->lw(a2, 144, sp);                               // lw a2, 144(sp)
  c->daddiu(a0, sp, 192);                           // daddiu a0, sp, 192
  c->lh(v1, 0, a2);                                 // lh v1, 0(a2)
  c->mov64(a1, s2);                                 // or a1, s2, r0
  c->load_symbol2(t9, cache.sp_adjust_launch);      // lw t9, sp-adjust-launch(s7)
  c->daddiu(v1, v1, -64);                           // daddiu v1, v1, -64
  bc = ((s64)c->sgpr64(v1)) >= 0;                   // bgez v1, L308
  c->daddiu(a3, sp, 80);                            // daddiu a3, sp, 80
  if (bc) {goto block_42;}                          // branch non-likely

  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->mov64(t0, s6);                                 // or t0, s6, r0
  c->jalr(call_addr);                               // jalr ra, t9

block_42:
  bc = c->sgpr64(s6) == c->sgpr64(s7);              // beq s6, s7, L309
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_44;}                          // branch non-likely

  c->load_symbol2(t9, cache.sp_euler_convert);      // lw t9, sp-euler-convert(s7)
  c->daddiu(a0, sp, 192);                           // daddiu a0, sp, 192
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->mov64(a1, s2);                                 // or a1, s2, r0
  c->jalr(call_addr);                               // jalr ra, t9

block_44:
  bc = c->sgpr64(s0) == c->sgpr64(s7);              // beq s0, s7, L310
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_48;}                          // branch non-likely

  c->lw(a2, 12, s0);                                // lw a2, 12(s0)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a2) == c->sgpr64(s7);              // beq a2, s7, L310
  c->daddiu(a0, sp, 192);                           // daddiu a0, sp, 192
  if (bc) {goto block_48;}                          // branch non-likely

  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L310
  c->mov64(a1, s2);                                 // or a1, s2, r0
  if (bc) {goto block_48;}                          // branch non-likely

  c->load_symbol2(t9, cache.sp_rotate_system);      // lw t9, sp-rotate-system(s7)
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9

block_48:
  c->lqc2(vf4, 192, sp);                            // lqc2 vf4, 192(sp)
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyz, vf4, vf4, vf30);               // vadd.xyz vf4, vf4, vf30
  c->lw(a0, 4, s2);                                 // lw a0, 4(s2)
  c->lw(a1, 160, sp);                               // lw a1, 160(sp)
  // nop                                            // sll r0, r0, 0
  c->load_symbol2(t9, cache.particle_adgif);        // lw t9, particle-adgif(s7)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf4, 192, sp);                            // sqc2 vf4, 192(sp)
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lw(a2, 4, s2);                                 // lw a2, 4(s2)
  c->lui(a3, 256);                                  // lui a3, 256
  c->lw(v1, 64, a2);                                // lw v1, 64(a2)
  c->andi(a0, s5, 16);                              // andi a0, s5, 16
  c->addiu(a1, r0, 66);                             // addiu a1, r0, 66
  // nop                                            // sll r0, r0, 0
  c->movn(v1, a1, a0);                              // movn v1, a1, a0
  c->andi(a0, s5, 8);                               // andi a0, s5, 8
  c->addiu(a1, r0, 72);                             // addiu a1, r0, 72
  // nop                                            // sll r0, r0, 0
  c->movn(v1, a1, a0);                              // movn v1, a1, a0
  c->andi(a0, s5, 256);                             // andi a0, s5, 256
  c->sw(v1, 64, a2);                                // sw v1, 64(a2)
  c->ori(a1, a3, 304);                              // ori a1, a3, 304
  if (((s64)c->sgpr64(a0)) != ((s64)0)) {           // bnel a0, r0, L311
    c->sd(a1, 48, a2);                              // sd a1, 48(a2)
    goto block_50;
  }

block_50:
  c->lw(t9, 172, sp);                               // lw t9, 172(sp)
  c->mov64(a1, s2);                                 // or a1, s2, r0
  c->mov64(a0, s3);                                 // or a0, s3, r0
  c->mov64(t0, s0);                                 // or t0, s0, r0
  bc = c->sgpr64(t9) == 0;                          // beq t9, r0, L312
  c->daddiu(a2, sp, 192);                           // daddiu a2, sp, 192
  if (bc) {goto block_52;}                          // branch non-likely

  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->mov64(a3, s4);                                 // or a3, s4, r0
  c->jalr(call_addr);                               // jalr ra, t9

block_52:
  c->lw(a0, 184, sp);                               // lw a0, 184(sp)
  // nop                                            // sll r0, r0, 0
  c->lwc1(f2, 4, s4);                               // lwc1 f2, 4(s4)
  c->lui(a1, 16256);                                // lui a1, 16256
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L313
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_55;}                          // branch non-likely

  c->lwc1(f3, 4, a0);                               // lwc1 f3, 4(a0)
  // nop                                            // sll r0, r0, 0
  c->adds(f2, f2, f3);                              // add.s f2, f2, f3
  // nop                                            // sll r0, r0, 0
  c->swc1(f2, 4, s4);                               // swc1 f2, 4(s4)
  c->mtc1(f3, a1);                                  // mtc1 f3, a1
  c->subs(f2, f2, f3);                              // sub.s f2, f2, f3
  // nop                                            // sll r0, r0, 0
  c->mfc1(a1, f2);                                  // mfc1 a1, f2
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(a1)) < 0;                    // bltz a1, L313
  c->load_symbol2(t9, cache.new_sound_id);          // lw t9, new-sound-id(s7)
  if (bc) {goto block_55;}                          // branch non-likely

  c->sw(a1, 4, s4);                                 // sw a1, 4(s4)
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lw(a0, 184, sp);                               // lw a0, 184(sp)
  c->mov64(a1, v0);                                 // or a1, v0, r0
  c->load_symbol2(t9, cache.sound_play_by_spec);    // lw t9, sound-play-by-spec(s7)
  c->daddiu(a2, sp, 64);                            // daddiu a2, sp, 64
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9

block_55:
  c->addiu(a0, r0, 4);                              // addiu a0, r0, 4
  c->andi(v1, s5, 128);                             // andi v1, s5, 128
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L314
  c->dsubu(a0, r0, a0);                             // dsubu a0, r0, a0
  if (bc) {goto block_57;}                          // branch non-likely

  c->load_symbol2(v1, cache.add_to_sprite_aux_list);// lw v1, add-to-sprite-aux-list(s7)
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  // nop                                            // sll r0, r0, 0
  c->and_(s5, s5, a0);                              // and s5, s5, a0
  c->sw(r0, 236, sp);                               // sw r0, 236(sp)
  // nop                                            // sll r0, r0, 0
  c->sw(r0, 60, s2);                                // sw r0, 60(s2)
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 208, sp);                               // lw v1, 208(sp)
  c->addiu(a0, r0, 3);                              // addiu a0, r0, 3
  c->pmaxw(v1, v1, a0);                             // pmaxw v1, v1, a0
  c->addiu(a0, r0, 11);                             // addiu a0, r0, 11
  c->pminw(v1, v1, a0);                             // pminw v1, v1, a0
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 208, sp);                               // sw v1, 208(sp)
  // nop                                            // sll r0, r0, 0

block_57:
  c->addiu(v1, r0, 10);                             // addiu v1, r0, 10
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  //beq r0, r0, L318                                // beq r0, r0, L318
  // nop                                            // sll r0, r0, 0
  goto block_66;                                    // branch always


block_58:
  c->addiu(a1, r0, 5424);                           // addiu a1, r0, 5424
  c->mult3(a1, a1, a0);                             // mult3 a1, a1, a0
  c->daddiu(a1, a1, 320);                           // daddiu a1, a1, 320
  c->load_symbol2(a2, cache.level);                 // lw a2, *level*(s7)
  c->daddu(a1, a1, a2);                             // daddu a1, a1, a2
  c->load_symbol_addr(a2, cache.active);            // daddiu a2, s7, active
  c->lwu(a3, 16, a1);                               // lwu a3, 16(a1)
  bc = c->sgpr64(a3) != c->sgpr64(a2);              // bne a3, a2, L317
  c->mov64(a2, s7);                                 // or a2, s7, r0
  if (bc) {goto block_65;}                          // branch non-likely

  c->lwu(a2, 44, a1);                               // lwu a2, 44(a1)
  c->slt(a2, s4, a2);                               // slt a2, s4, a2
  c->daddiu(a3, s7, 4);                             // daddiu a3, s7, 4
  c->movn(a3, s7, a2);                              // movn a3, s7, a2
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(a3))) {// beql s7, a3, L316
    c->mov64(a2, a3);                               // or a2, a3, r0
    goto block_62;
  }

// block_61:
  c->lwu(a2, 48, a1);                               // lwu a2, 48(a1)
  c->slt(a3, s4, a2);                               // slt a3, s4, a2
  c->daddiu(a2, s7, 4);                             // daddiu a2, s7, 4
  c->movz(a2, s7, a3);                              // movz a2, s7, a3

block_62:
  bc = c->sgpr64(s7) == c->sgpr64(a2);              // beq s7, a2, L317
  c->mov64(a2, s7);                                 // or a2, s7, r0
  if (bc) {goto block_65;}                          // branch non-likely

  c->lw(v1, 12, a1);                                // lw v1, 12(a1)
  c->mov64(s7, s7);                                 // or s7, s7, r0
  //beq r0, r0, L319                                // beq r0, r0, L319
  // nop                                            // sll r0, r0, 0
  goto block_68;                                    // branch always

  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0

block_65:
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1

block_66:
  c->load_symbol2(a1, cache.level);                 // lw a1, *level*(s7)
  c->lw(a1, 0, a1);                                 // lw a1, 0(a1)
  c->slt(a1, a0, a1);                               // slt a1, a0, a1
  bc = c->sgpr64(a1) != 0;                          // bne a1, r0, L315
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_58;}                          // branch non-likely

  c->mov64(a0, s7);                                 // or a0, s7, r0

block_68:
  c->dsll(v1, v1, 9);                               // dsll v1, v1, 9
  c->or_(s5, s5, v1);                               // or s5, s5, v1
  // nop                                            // sll r0, r0, 0
  c->andi(v1, s5, 8320);                            // andi v1, s5, 8320
  c->addiu(a0, r0, 8192);                           // addiu a0, r0, 8192
  bc = c->sgpr64(v1) != c->sgpr64(a0);              // bne v1, a0, L320
  c->load_symbol2(a0, cache.time_of_day_context);   // lw a0, *time-of-day-context*(s7)
  if (bc) {goto block_70;}                          // branch non-likely

  c->lqc2(vf4, 124, a0);                            // lqc2 vf4, 124(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf5, 224, sp);                            // lqc2 vf5, 224(sp)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf6, 48, s2);                             // lqc2 vf6, 48(s2)
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyz, vf5, vf5, vf4);                // vmul.xyz vf5, vf5, vf4
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyz, vf6, vf6, vf4);                // vmul.xyz vf6, vf6, vf4
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf5, 224, sp);                            // sqc2 vf5, 224(sp)
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L321                                // beq r0, r0, L321
  c->sqc2(vf6, 48, s2);                             // sqc2 vf6, 48(s2)
  goto block_72;                                    // branch always


block_70:
  // nop                                            // sll r0, r0, 0
  c->andi(v1, s5, 128);                             // andi v1, s5, 128
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L321
  c->lqc2(vf5, 224, sp);                            // lqc2 vf5, 224(sp)
  if (bc) {goto block_72;}                          // branch non-likely

  c->lqc2(vf6, 48, s2);                             // lqc2 vf6, 48(s2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf5, 224, sp);                            // sqc2 vf5, 224(sp)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf6, 48, s2);                             // sqc2 vf6, 48(s2)
  // nop                                            // sll r0, r0, 0

block_72:
  c->mov64(v1, s1);                                 // or v1, s1, r0
  c->dsubu(a0, s1, s7);                             // dsubu a0, s1, s7
  c->movz(v1, r0, a0);                              // movz v1, r0, a0
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 132, s2);                               // sw v1, 132(s2)
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 0, s2);                                 // lw v1, 0(s2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf4, 192, sp);                            // lqc2 vf4, 192(sp)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf5, 208, sp);                            // lqc2 vf5, 208(sp)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf6, 224, sp);                            // lqc2 vf6, 224(sp)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf4, 0, v1);                              // sqc2 vf4, 0(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf5, 16, v1);                             // sqc2 vf5, 16(v1)
  // nop                                            // sll r0, r0, 0
  c->vsub_bc(DEST::w, BC::w, vf6, vf0, vf0);        // vsubw.w vf6, vf0, vf0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf6, 32, v1);                             // sqc2 vf6, 32(v1)
  // nop                                            // sll r0, r0, 0
  c->ori(s5, s5, 32);                               // ori s5, s5, 32
  c->lw(a0, 236, sp);                               // lw a0, 236(sp)
  c->sw(s5, 104, s2);                               // sw s5, 104(s2)
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 124, s2);                               // sw a0, 124(s2)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s0) == c->sgpr64(s7);              // beq s0, s7, L322
  c->lui(v1, 16256);                                // lui v1, 16256
  if (bc) {goto block_75;}                          // branch non-likely

  c->lwc1(f2, 24, s0);                              // lwc1 f2, 24(s0)
  c->mtc1(f3, v1);                                  // mtc1 f3, v1
  c->subs(f2, f2, f3);                              // sub.s f2, f2, f3
  // nop                                            // sll r0, r0, 0
  c->swc1(f2, 24, s0);                              // swc1 f2, 24(s0)
  c->cvtws(f2, f2);                                 // cvt.w.s f2, f2
  c->mfc1(v1, f2);                                  // mfc1 v1, f2
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L296
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely

  //beq r0, r0, L323                                // beq r0, r0, L323
  // nop                                            // sll r0, r0, 0
  goto block_76;                                    // branch always


block_75:
  c->lwc1(f2, 0, s4);                               // lwc1 f2, 0(s4)
  c->mtc1(f3, v1);                                  // mtc1 f3, v1
  c->subs(f2, f2, f3);                              // sub.s f2, f2, f3
  // nop                                            // sll r0, r0, 0
  c->swc1(f2, 0, s4);                               // swc1 f2, 0(s4)
  c->cvtws(f2, f2);                                 // cvt.w.s f2, f2
  c->mfc1(v1, f2);                                  // mfc1 v1, f2
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L296
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely


block_76:
  c->lw(ra, 0, sp);                                 // lw ra, 0(sp)
  // nop                                            // sll r0, r0, 0
  c->lq(s0, 16, sp);                                // lq s0, 16(sp)
  // nop                                            // sll r0, r0, 0
  c->lq(s1, 32, sp);                                // lq s1, 32(sp)
  // nop                                            // sll r0, r0, 0
  c->lq(s2, 48, sp);                                // lq s2, 48(sp)
  // nop                                            // sll r0, r0, 0
  c->lq(s3, 240, sp);                               // lq s3, 240(sp)
  // nop                                            // sll r0, r0, 0
  c->lq(s4, 256, sp);                               // lq s4, 256(sp)
  // nop                                            // sll r0, r0, 0
  c->lq(s5, 272, sp);                               // lq s5, 272(sp)
  // nop                                            // sll r0, r0, 0
  c->lq(s6, 288, sp);                               // lq s6, 288(sp)
  // nop                                            // sll r0, r0, 0
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 304);                           // daddiu sp, sp, 304
  goto end_of_function;                             // return

  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.level = intern_from_c(-1, 0, "*level*").c();
  cache.sp_launcher_enable = intern_from_c(-1, 0, "*sp-launcher-enable*").c();
  cache.sp_launcher_lock = intern_from_c(-1, 0, "*sp-launcher-lock*").c();
  cache.time_of_day_context = intern_from_c(-1, 0, "*time-of-day-context*").c();
  cache.active = intern_from_c(-1, 0, "active").c();
  cache.add_to_sprite_aux_list = intern_from_c(-1, 0, "add-to-sprite-aux-list").c();
  cache.cos = intern_from_c(-1, 0, "cos").c();
  cache.new_sound_id = intern_from_c(-1, 0, "new-sound-id").c();
  cache.particle_adgif = intern_from_c(-1, 0, "particle-adgif").c();
  cache.quaternion_axis_angle = intern_from_c(-1, 0, "quaternion-axis-angle!").c();
  cache.sin = intern_from_c(-1, 0, "sin").c();
  cache.sound_play_by_spec = intern_from_c(-1, 0, "sound-play-by-spec").c();
  cache.sp_adjust_launch = intern_from_c(-1, 0, "sp-adjust-launch").c();
  cache.sp_euler_convert = intern_from_c(-1, 0, "sp-euler-convert").c();
  cache.sp_get_particle = intern_from_c(-1, 0, "sp-get-particle").c();
  cache.sp_init_fields = intern_from_c(-1, 0, "sp-init-fields!").c();
  cache.sp_queue_launch = intern_from_c(-1, 0, "sp-queue-launch").c();
  cache.sp_rotate_system = intern_from_c(-1, 0, "sp-rotate-system").c();
  gLinkedFunctionTable.reg("sp-launch-particles-var", execute, 512);
}

} // namespace sp_launch_particles_var
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace sparticle_motion_blur {
struct Cache {
  void* math_camera; // *math-camera*
  void* atan; // atan
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  uint32_t Clipping = 0;
  c->daddiu(sp, sp, -80);                           // daddiu sp, sp, -80
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s3, 16, sp);                                // sq s3, 16(sp)
  c->sq(s4, 32, sp);                                // sq s4, 32(sp)
  c->sq(s5, 48, sp);                                // sq s5, 48(sp)
  c->sq(gp, 64, sp);                                // sq gp, 64(sp)
  c->mov64(s3, a1);                                 // or s3, a1, r0
  c->mov64(gp, a2);                                 // or gp, a2, r0
  c->ori(a0, r0, 65535);                            // ori a0, r0, 65535
  c->load_symbol2(v1, cache.math_camera);           // lw v1, *math-camera*(s7)
  c->dsll32(a1, a0, 16);                            // dsll32 a1, a0, 16
  c->lq(a0, 16, s3);                                // lq a0, 16(s3)
  c->lqc2(vf1, 0, gp);                              // lqc2 vf1, 0(gp)
  c->pceqw(a2, a0, r0);                             // pceqw a2, a0, r0
  c->lqc2(vf24, 572, v1);                           // lqc2 vf24, 572(v1)
  c->ppach(a2, r0, a2);                             // ppach a2, r0, a2
  c->lqc2(vf25, 588, v1);                           // lqc2 vf25, 588(v1)
  c->or_(a1, a2, a1);                               // or a1, a2, a1
  c->lqc2(vf26, 604, v1);                           // lqc2 vf26, 604(v1)
  c->daddiu(a1, a1, 1);                             // daddiu a1, a1, 1
  c->lqc2(vf27, 620, v1);                           // lqc2 vf27, 620(v1)
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L72
  c->mov128_vf_gpr(vf4, a0);                        // qmtc2.i vf4, a0
  if (bc) {goto block_5;}                           // branch non-likely

  c->lui(a0, 16896);                                // lui a0, 16896
  c->lqc2(vf30, 812, v1);                           // lqc2 vf30, 812(v1)
  c->lqc2(vf29, 780, v1);                           // lqc2 vf29, 780(v1)
  c->vmula_bc(DEST::xyzw, BC::x, vf24, vf1);        // vmulax.xyzw acc, vf24, vf1
  c->vmadda_bc(DEST::xyzw, BC::y, vf25, vf1);       // vmadday.xyzw acc, vf25, vf1
  c->vmadda_bc(DEST::xyzw, BC::z, vf26, vf1);       // vmaddaz.xyzw acc, vf26, vf1
  c->vmadd_bc(DEST::xyzw, BC::w, vf10, vf27, vf0);  // vmaddw.xyzw vf10, vf27, vf0
  c->mov128_vf_gpr(vf5, a0);                        // qmtc2.i vf5, a0
  c->vmul(DEST::xyzw, vf12, vf10, vf29);            // vmul.xyzw vf12, vf10, vf29
  c->vmula_bc(DEST::xyzw, BC::w, vf1, vf0);         // vmulaw.xyzw acc, vf1, vf0
  c->vmadd_bc(DEST::xyzw, BC::x, vf1, vf4, vf5);    // vmaddx.xyzw vf1, vf4, vf5
  c->vdiv(vf0, BC::w, vf12, BC::w);                 // vdiv Q, vf0.w, vf12.w
  Clipping = c->clip(vf12, vf12, Clipping);         // Unknown instr: vclip.xyz vf12, vf12
  c->vmula_bc(DEST::xyzw, BC::x, vf24, vf1);        // vmulax.xyzw acc, vf24, vf1
  c->vmadda_bc(DEST::xyzw, BC::y, vf25, vf1);       // vmadday.xyzw acc, vf25, vf1
  c->vmadda_bc(DEST::xyzw, BC::z, vf26, vf1);       // vmaddaz.xyzw acc, vf26, vf1
  c->vmadd_bc(DEST::xyzw, BC::w, vf11, vf27, vf0);  // vmaddw.xyzw vf11, vf27, vf0
  c->vwaitq();                                      // vwaitq
  c->gprs[v1].du64[0] = Clipping;                   // cfc2.i v1, Clipping
  c->vmulq(DEST::xyz, vf10, vf10);                  // vmulq.xyz vf10, vf10, Q
  c->vmul(DEST::xyzw, vf13, vf11, vf29);            // vmul.xyzw vf13, vf11, vf29
  c->andi(v1, v1, 63);                              // andi v1, v1, 63
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L72
  c->vadd(DEST::xyzw, vf10, vf10, vf30);            // vadd.xyzw vf10, vf10, vf30
  if (bc) {goto block_5;}                           // branch non-likely

  c->vdiv(vf0, BC::w, vf13, BC::w);                 // vdiv Q, vf0.w, vf13.w
  Clipping = c->clip(vf13, vf13, Clipping);         // Unknown instr: vclip.xyz vf13, vf13
  c->vmax_bc(DEST::w, BC::x, vf10, vf10, vf0);      // vmaxx.w vf10, vf10, vf0
  c->vftoi4(DEST::xyzw, vf2, vf10);                 // vftoi4.xyzw vf2, vf10
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyz, vf11, vf11);                  // vmulq.xyz vf11, vf11, Q
  c->gprs[v1].du64[0] = Clipping;                   // cfc2.i v1, Clipping
  c->vitof0(DEST::xyzw, vf6, vf2);                  // vitof0.xyzw vf6, vf2
  c->vadd(DEST::xyzw, vf11, vf11, vf30);            // vadd.xyzw vf11, vf11, vf30
  c->andi(v1, v1, 63);                              // andi v1, v1, 63
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L72
  c->vdiv(vf0, BC::w, vf6, BC::z);                  // vdiv Q, vf0.w, vf6.z
  if (bc) {goto block_5;}                           // branch non-likely

  c->vmax_bc(DEST::w, BC::x, vf11, vf11, vf0);      // vmaxx.w vf11, vf11, vf0
  c->vadd_bc(DEST::x, BC::w, vf9, vf0, vf0);        // vaddw.x vf9, vf0, vf0
  c->vftoi4(DEST::xyzw, vf3, vf11);                 // vftoi4.xyzw vf3, vf11
  c->vitof0(DEST::xyzw, vf7, vf3);                  // vitof0.xyzw vf7, vf3
  c->vsub(DEST::xy, vf8, vf7, vf6);                 // vsub.xy vf8, vf7, vf6
  c->mov128_gpr_vf(s4, vf8);                        // qmfc2.i s4, vf8
  c->dsra32(s5, s4, 0);                             // dsra32 s5, s4, 0
  c->vmulq(DEST::x, vf9, vf9);                      // vmulq.x vf9, vf9, Q
  c->load_symbol2(t9, cache.atan);                  // lw t9, atan(s7)
  c->mov64(a0, s4);                                 // or a0, s4, r0
  c->mov64(a1, s5);                                 // or a1, s5, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(a0, v0);                                 // or a0, v0, r0
  c->lw(v1, 12, s3);                                // lw v1, 12(s3)
  c->lui(a1, -14720);                               // lui a1, -14720
  c->mtc1(f0, a1);                                  // mtc1 f0, a1
  c->mtc1(f1, a0);                                  // mtc1 f1, a0
  c->adds(f0, f0, f1);                              // add.s f0, f0, f1
  c->swc1(f0, 24, gp);                              // swc1 f0, 24(gp)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L73
  c->mov128_gpr_vf(a0, vf9);                        // qmfc2.i a0, vf9
  if (bc) {goto block_7;}                           // branch non-likely

  c->mtc1(f2, a0);                                  // mtc1 f2, a0
  c->lui(a0, 16256);                                // lui a0, 16256
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  c->lui(a1, 13702);                                // lui a1, 13702
  c->ori(a1, a1, 14269);                            // ori a1, a1, 14269
  c->lui(a2, 13337);                                // lui a2, 13337
  c->ori(a2, a2, 25670);                            // ori a2, a2, 25670
  c->mtc1(f0, a0);                                  // mtc1 f0, a0
  c->mtc1(f3, a1);                                  // mtc1 f3, a1
  c->mtc1(f4, a2);                                  // mtc1 f4, a2
  c->subs(f2, f2, f3);                              // sub.s f2, f2, f3
  c->subs(f3, f4, f3);                              // sub.s f3, f4, f3
  c->divs(f2, f2, f3);                              // div.s f2, f2, f3
  c->mtc1(f3, s4);                                  // mtc1 f3, s4
  c->mtc1(f4, s5);                                  // mtc1 f4, s5
  // Unknown instr: mula.s f3, f3
  // Unknown instr: madd.s f3, f4, f4
  {
    float f3 = c->fprs[3];
    float f4 = c->fprs[4];
    c->fprs[3] = (f3 * f3) + (f4 * f4);
  }
  c->maxs(f2, f2, f1);                              // max.s f2, f2, f1
  c->sqrts(f1, f3);                                 // sqrt.s f1, f3
  c->mins(f2, f2, f0);                              // min.s f2, f2, f0
  c->lui(a0, 16448);                                // lui a0, 16448
  c->subs(f0, f0, f2);                              // sub.s f0, f0, f2
  c->lui(a1, 16000);                                // lui a1, 16000
  c->mtc1(f3, a0);                                  // mtc1 f3, a0
  c->mtc1(f5, a1);                                  // mtc1 f5, a1
  c->mtc1(f4, v1);                                  // mtc1 f4, v1
  // Unknown instr: mula.s f0, f3
  // Unknown instr: madd.s f0, f2, f5
  {
    float f0 = c->fprs[0];
    float f2 = c->fprs[2];
    float f3 = c->fprs[3];
    float f5 = c->fprs[5];
    c->fprs[0] = (f0 * f3) + (f2 * f5);
  }
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->muls(f0, f0, f4);                              // mul.s f0, f0, f4
  //beq r0, r0, L73                                 // beq r0, r0, L73
  c->swc1(f0, 12, gp);                              // swc1 f0, 12(gp)
  goto block_7;                                     // branch always


block_5:
  c->lwc1(f0, 12, s3);                              // lwc1 f0, 12(s3)
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  cop1_bc = c->fprs[f0] == c->fprs[f1];             // c.eq.s f0, f1
  bc = cop1_bc;                                     // bc1t L73
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_7;}                           // branch non-likely

  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 12, gp);                              // swc1 f0, 12(gp)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0

block_7:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 64, sp);                                // lq gp, 64(sp)
  c->lq(s5, 48, sp);                                // lq s5, 48(sp)
  c->lq(s4, 32, sp);                                // lq s4, 32(sp)
  c->lq(s3, 16, sp);                                // lq s3, 16(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 80);                            // daddiu sp, sp, 80
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.math_camera = intern_from_c(-1, 0, "*math-camera*").c();
  cache.atan = intern_from_c(-1, 0, "atan").c();
  gLinkedFunctionTable.reg("sparticle-motion-blur", execute, 128);
}

} // namespace sparticle_motion_blur
} // namespace Mips2C