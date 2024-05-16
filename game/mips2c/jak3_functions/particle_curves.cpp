//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace live_func_curve {
struct Cache {
  void* random_generator; // *random-generator*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->daddiu(sp, sp, -336);                          // daddiu sp, sp, -336
  c->mov64(t0, a1);                                 // or t0, a1, r0
  c->lwu(a3, 108, t0);                              // lwu a3, 108(t0)
  c->lw(v1, 12, t0);                                // lw v1, 12(t0)
  c->load_symbol2(a0, cache.random_generator);      // lw a0, *random-generator*(s7)
  c->lwu(a0, 0, a0);                                // lwu a0, 0(a0)
  c->addiu(a1, r0, 0);                              // addiu a1, r0, 0
  c->addiu(a1, r0, 0);                              // addiu a1, r0, 0
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->load_symbol2(a1, cache.random_generator);      // lw a1, *random-generator*(s7)
  c->sw(v1, 0, a1);                                 // sw v1, 0(a1)
  c->ld(t1, 40, a3);                                // ld t1, 40(a3)
  c->ld(v1, 48, a3);                                // ld v1, 48(a3)
  c->slt(v1, r0, v1);                               // slt v1, r0, v1
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L5
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_2;}                           // branch non-likely

  c->load_symbol2(t2, cache.random_generator);      // lw t2, *random-generator*(s7)
  c->lwu(a1, 0, t2);                                // lwu a1, 0(t2)
  c->addiu(v1, r0, 16807);                          // addiu v1, r0, 16807
  c->mult3(v0, v1, a1);                             // mult3 v0, v1, a1
  c->mfhi(v1);                                      // mfhi v1
  c->daddu(v1, v1, v1);                             // daddu v1, v1, v1
  c->srl(a1, v0, 31);                               // srl a1, v0, 31
  c->or_(v1, v1, a1);                               // or v1, v1, a1
  c->daddu(v0, v0, v1);                             // daddu v0, v0, v1
  c->sll(v0, v0, 1);                                // sll v0, v0, 1
  c->srl(v0, v0, 1);                                // srl v0, v0, 1
  c->sw(v0, 0, t2);                                 // sw v0, 0(t2)
  c->ld(v1, 48, a3);                                // ld v1, 48(a3)
  c->div(v0, v1);                                   // div v0, v1
  c->mfhi(v1);                                      // mfhi v1
  c->daddu(t1, t1, v1);                             // daddu t1, t1, v1
  c->mov64(v1, t1);                                 // or v1, t1, r0

block_2:
  c->lw(v1, 100, t0);                               // lw v1, 100(t0)
  c->dsubu(v1, t1, v1);                             // dsubu v1, t1, v1
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->cvtsw(f0, f0);                                 // cvt.s.w f0, f0
  c->mtc1(f1, t1);                                  // mtc1 f1, t1
  c->cvtsw(f1, f1);                                 // cvt.s.w f1, f1
  c->divs(f0, f0, f1);                              // div.s f0, f0, f1
  c->lwu(v1, 0, a3);                                // lwu v1, 0(a3)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L14
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_24;}                          // branch non-likely

  c->load_symbol2(t0, cache.random_generator);      // lw t0, *random-generator*(s7)
  c->lwu(a1, 0, t0);                                // lwu a1, 0(t0)
  c->addiu(v1, r0, 16807);                          // addiu v1, r0, 16807
  c->mult3(v0, v1, a1);                             // mult3 v0, v1, a1
  c->mfhi(v1);                                      // mfhi v1
  c->daddu(v1, v1, v1);                             // daddu v1, v1, v1
  c->srl(a1, v0, 31);                               // srl a1, v0, 31
  c->or_(v1, v1, a1);                               // or v1, v1, a1
  c->daddu(v0, v0, v1);                             // daddu v0, v0, v1
  c->sll(v0, v0, 1);                                // sll v0, v0, 1
  c->srl(v0, v0, 1);                                // srl v0, v0, 1
  c->sw(v0, 0, t0);                                 // sw v0, 0(t0)
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->dsra(v1, v1, 8);                               // dsra v1, v1, 8
  c->lui(a1, 16256);                                // lui a1, 16256
  c->or_(v1, a1, v1);                               // or v1, a1, v1
  c->lui(a1, -16512);                               // lui a1, -16512
  c->mtc1(f1, a1);                                  // mtc1 f1, a1
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  c->adds(f1, f1, f2);                              // add.s f1, f1, f2
  c->mfc1(v1, f1);                                  // mfc1 v1, f1
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->daddiu(v1, sp, 16);                            // daddiu v1, sp, 16
  c->ld(a1, 56, a3);                                // ld a1, 56(a3)
  c->andi(a1, a1, 2);                               // andi a1, a1, 2
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L14
  c->mov64(a1, s7);                                 // or a1, s7, r0
  if (bc) {goto block_24;}                          // branch non-likely

  c->lwu(a1, 0, a3);                                // lwu a1, 0(a3)
  c->mov64(t0, v1);                                 // or t0, v1, r0
  c->daddiu(t1, sp, 32);                            // daddiu t1, sp, 32
  c->mfc1(t2, f1);                                  // mfc1 t2, f1
  c->mov128_vf_gpr(vf26, t2);                       // qmtc2.i vf26, t2
  c->lqc2(vf23, 12, a1);                            // lqc2 vf23, 12(a1)
  c->lqc2(vf25, 92, a1);                            // lqc2 vf25, 92(a1)
  c->vmax_bc(DEST::xyzw, BC::w, vf3, vf0, vf0);     // vmaxw.xyzw vf3, vf0, vf0
  c->vmula(DEST::xyzw,  vf25, vf23);                // vmula.xyzw acc, vf25, vf23
  c->vmadd_bc(DEST::xyzw, BC::x, vf28, vf25, vf26); // vmaddx.xyzw vf28, vf25, vf26
  c->sqc2(vf28, 0, t1);                             // sqc2 vf28, 0(t1)
  c->lw(t2, 8, t1);                                 // lw t2, 8(t1)
  c->lw(t1, 4, t1);                                 // lw t1, 4(t1)
  bc = ((s64)c->sgpr64(t2)) >= 0;                   // bgez t2, L7
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_8;}                           // branch non-likely

  bc = ((s64)c->sgpr64(t1)) >= 0;                   // bgez t1, L6
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  c->lqc2(vf1, 28, a1);                             // lqc2 vf1, 28(a1)
  c->lqc2(vf2, 44, a1);                             // lqc2 vf2, 44(a1)
  c->vsub_bc(DEST::xyzw, BC::x, vf4, vf3, vf28);    // vsubx.xyzw vf4, vf3, vf28
  c->vmula_bc(DEST::xyzw, BC::x, vf2, vf28);        // vmulax.xyzw acc, vf2, vf28
  c->vmadd_bc(DEST::xyzw, BC::x, vf5, vf1, vf4);    // vmaddx.xyzw vf5, vf1, vf4
  //beq r0, r0, L8                                  // beq r0, r0, L8
  c->sqc2(vf5, 0, t0);                              // sqc2 vf5, 0(t0)
  goto block_9;                                     // branch always


block_7:
  c->lqc2(vf1, 44, a1);                             // lqc2 vf1, 44(a1)
  c->lqc2(vf2, 60, a1);                             // lqc2 vf2, 60(a1)
  c->vsub_bc(DEST::xyzw, BC::y, vf4, vf3, vf28);    // vsuby.xyzw vf4, vf3, vf28
  c->vmula_bc(DEST::xyzw, BC::y, vf2, vf28);        // vmulay.xyzw acc, vf2, vf28
  c->vmadd_bc(DEST::xyzw, BC::y, vf5, vf1, vf4);    // vmaddy.xyzw vf5, vf1, vf4
  //beq r0, r0, L8                                  // beq r0, r0, L8
  c->sqc2(vf5, 0, t0);                              // sqc2 vf5, 0(t0)
  goto block_9;                                     // branch always


block_8:
  c->lqc2(vf1, 60, a1);                             // lqc2 vf1, 60(a1)
  c->lqc2(vf2, 76, a1);                             // lqc2 vf2, 76(a1)
  c->vsub_bc(DEST::xyzw, BC::z, vf4, vf3, vf28);    // vsubz.xyzw vf4, vf3, vf28
  c->vmula_bc(DEST::xyzw, BC::z, vf2, vf28);        // vmulaz.xyzw acc, vf2, vf28
  c->vmadd_bc(DEST::xyzw, BC::z, vf5, vf1, vf4);    // vmaddz.xyzw vf5, vf1, vf4
  c->sqc2(vf5, 0, t0);                              // sqc2 vf5, 0(t0)

block_9:
  c->lwu(a1, 16, a3);                               // lwu a1, 16(a3)
  bc = c->sgpr64(s7) == c->sgpr64(a1);              // beq s7, a1, L10
  c->mov64(a1, s7);                                 // or a1, s7, r0
  if (bc) {goto block_14;}                          // branch non-likely

  c->lwc1(f1, 0, v1);                               // lwc1 f1, 0(v1)
  c->lwu(t1, 16, a3);                               // lwu t1, 16(a3)
  c->movs(f2, f0);                                  // mov.s f2, f0
  c->daddiu(t0, sp, 48);                            // daddiu t0, sp, 48
  c->daddiu(a1, sp, 64);                            // daddiu a1, sp, 64
  c->mfc1(t2, f2);                                  // mfc1 t2, f2
  c->mov128_vf_gpr(vf27, t2);                       // qmtc2.i vf27, t2
  c->lqc2(vf24, 12, t1);                            // lqc2 vf24, 12(t1)
  c->lqc2(vf25, 28, t1);                            // lqc2 vf25, 28(t1)
  c->lqc2(vf26, 44, t1);                            // lqc2 vf26, 44(t1)
  c->vmini_bc(DEST::xyzw, BC::w, vf27, vf27, vf0);  // vminiw.xyzw vf27, vf27, vf0
  c->vmax_bc(DEST::xyzw, BC::x, vf27, vf27, vf0);   // vmaxx.xyzw vf27, vf27, vf0
  c->vadd_bc(DEST::xyzw, BC::x, vf28, vf24, vf27);  // vaddx.xyzw vf28, vf24, vf27
  c->vmula_bc(DEST::xyzw, BC::w, vf25, vf0);        // vmulaw.xyzw acc, vf25, vf0
  c->vmadd(DEST::xyzw, vf29, vf28, vf26);           // vmadd.xyzw vf29, vf28, vf26
  c->sqc2(vf28, 0, t0);                             // sqc2 vf28, 0(t0)
  c->sqc2(vf29, 0, a1);                             // sqc2 vf29, 0(a1)
  c->lw(t1, 8, t0);                                 // lw t1, 8(t0)
  c->lw(t0, 4, t0);                                 // lw t0, 4(t0)
  bc = ((s64)c->sgpr64(t1)) >= 0;                   // bgez t1, L9
  c->lw(v0, 8, a1);                                 // lw v0, 8(a1)
  if (bc) {goto block_13;}                          // branch non-likely

  bc = ((s64)c->sgpr64(t0)) >= 0;                   // bgez t0, L9
  c->lw(v0, 4, a1);                                 // lw v0, 4(a1)
  if (bc) {goto block_13;}                          // branch non-likely

  c->lw(v0, 0, a1);                                 // lw v0, 0(a1)

block_13:
  c->mov64(a1, v0);                                 // or a1, v0, r0
  c->mtc1(f2, a1);                                  // mtc1 f2, a1
  c->muls(f1, f1, f2);                              // mul.s f1, f1, f2
  c->swc1(f1, 32, a2);                              // swc1 f1, 32(a2)
  c->mfc1(a1, f1);                                  // mfc1 a1, f1

block_14:
  c->lwu(a1, 20, a3);                               // lwu a1, 20(a3)
  bc = c->sgpr64(s7) == c->sgpr64(a1);              // beq s7, a1, L12
  c->mov64(a1, s7);                                 // or a1, s7, r0
  if (bc) {goto block_19;}                          // branch non-likely

  c->lwc1(f1, 4, v1);                               // lwc1 f1, 4(v1)
  c->lwu(t1, 20, a3);                               // lwu t1, 20(a3)
  c->movs(f2, f0);                                  // mov.s f2, f0
  c->daddiu(t0, sp, 80);                            // daddiu t0, sp, 80
  c->daddiu(a1, sp, 96);                            // daddiu a1, sp, 96
  c->mfc1(t2, f2);                                  // mfc1 t2, f2
  c->mov128_vf_gpr(vf27, t2);                       // qmtc2.i vf27, t2
  c->lqc2(vf24, 12, t1);                            // lqc2 vf24, 12(t1)
  c->lqc2(vf25, 28, t1);                            // lqc2 vf25, 28(t1)
  c->lqc2(vf26, 44, t1);                            // lqc2 vf26, 44(t1)
  c->vmini_bc(DEST::xyzw, BC::w, vf27, vf27, vf0);  // vminiw.xyzw vf27, vf27, vf0
  c->vmax_bc(DEST::xyzw, BC::x, vf27, vf27, vf0);   // vmaxx.xyzw vf27, vf27, vf0
  c->vadd_bc(DEST::xyzw, BC::x, vf28, vf24, vf27);  // vaddx.xyzw vf28, vf24, vf27
  c->vmula_bc(DEST::xyzw, BC::w, vf25, vf0);        // vmulaw.xyzw acc, vf25, vf0
  c->vmadd(DEST::xyzw, vf29, vf28, vf26);           // vmadd.xyzw vf29, vf28, vf26
  c->sqc2(vf28, 0, t0);                             // sqc2 vf28, 0(t0)
  c->sqc2(vf29, 0, a1);                             // sqc2 vf29, 0(a1)
  c->lw(t1, 8, t0);                                 // lw t1, 8(t0)
  c->lw(t0, 4, t0);                                 // lw t0, 4(t0)
  bc = ((s64)c->sgpr64(t1)) >= 0;                   // bgez t1, L11
  c->lw(v0, 8, a1);                                 // lw v0, 8(a1)
  if (bc) {goto block_18;}                          // branch non-likely

  bc = ((s64)c->sgpr64(t0)) >= 0;                   // bgez t0, L11
  c->lw(v0, 4, a1);                                 // lw v0, 4(a1)
  if (bc) {goto block_18;}                          // branch non-likely

  c->lw(v0, 0, a1);                                 // lw v0, 0(a1)

block_18:
  c->mov64(a1, v0);                                 // or a1, v0, r0
  c->mtc1(f2, a1);                                  // mtc1 f2, a1
  c->muls(f1, f1, f2);                              // mul.s f1, f1, f2
  c->swc1(f1, 36, a2);                              // swc1 f1, 36(a2)
  c->mfc1(a1, f1);                                  // mfc1 a1, f1

block_19:
  c->lwu(a1, 24, a3);                               // lwu a1, 24(a3)
  bc = c->sgpr64(s7) == c->sgpr64(a1);              // beq s7, a1, L14
  c->mov64(a1, s7);                                 // or a1, s7, r0
  if (bc) {goto block_24;}                          // branch non-likely

  c->lwc1(f1, 8, v1);                               // lwc1 f1, 8(v1)
  c->lwu(t0, 24, a3);                               // lwu t0, 24(a3)
  c->movs(f2, f0);                                  // mov.s f2, f0
  c->daddiu(a1, sp, 112);                           // daddiu a1, sp, 112
  c->daddiu(v1, sp, 128);                           // daddiu v1, sp, 128
  c->mfc1(t1, f2);                                  // mfc1 t1, f2
  c->mov128_vf_gpr(vf27, t1);                       // qmtc2.i vf27, t1
  c->lqc2(vf24, 12, t0);                            // lqc2 vf24, 12(t0)
  c->lqc2(vf25, 28, t0);                            // lqc2 vf25, 28(t0)
  c->lqc2(vf26, 44, t0);                            // lqc2 vf26, 44(t0)
  c->vmini_bc(DEST::xyzw, BC::w, vf27, vf27, vf0);  // vminiw.xyzw vf27, vf27, vf0
  c->vmax_bc(DEST::xyzw, BC::x, vf27, vf27, vf0);   // vmaxx.xyzw vf27, vf27, vf0
  c->vadd_bc(DEST::xyzw, BC::x, vf28, vf24, vf27);  // vaddx.xyzw vf28, vf24, vf27
  c->vmula_bc(DEST::xyzw, BC::w, vf25, vf0);        // vmulaw.xyzw acc, vf25, vf0
  c->vmadd(DEST::xyzw, vf29, vf28, vf26);           // vmadd.xyzw vf29, vf28, vf26
  c->sqc2(vf28, 0, a1);                             // sqc2 vf28, 0(a1)
  c->sqc2(vf29, 0, v1);                             // sqc2 vf29, 0(v1)
  c->lw(t0, 8, a1);                                 // lw t0, 8(a1)
  c->lw(a1, 4, a1);                                 // lw a1, 4(a1)
  bc = ((s64)c->sgpr64(t0)) >= 0;                   // bgez t0, L13
  c->lw(v0, 8, v1);                                 // lw v0, 8(v1)
  if (bc) {goto block_23;}                          // branch non-likely

  bc = ((s64)c->sgpr64(a1)) >= 0;                   // bgez a1, L13
  c->lw(v0, 4, v1);                                 // lw v0, 4(v1)
  if (bc) {goto block_23;}                          // branch non-likely

  c->lw(v0, 0, v1);                                 // lw v0, 0(v1)

block_23:
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  c->muls(f1, f1, f2);                              // mul.s f1, f1, f2
  c->swc1(f1, 40, a2);                              // swc1 f1, 40(a2)
  c->mfc1(a1, f1);                                  // mfc1 a1, f1

block_24:
  c->lwu(v1, 4, a3);                                // lwu v1, 4(a3)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L17
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_33;}                          // branch non-likely

  c->load_symbol2(t0, cache.random_generator);      // lw t0, *random-generator*(s7)
  c->lwu(a1, 0, t0);                                // lwu a1, 0(t0)
  c->addiu(v1, r0, 16807);                          // addiu v1, r0, 16807
  c->mult3(v0, v1, a1);                             // mult3 v0, v1, a1
  c->mfhi(v1);                                      // mfhi v1
  c->daddu(v1, v1, v1);                             // daddu v1, v1, v1
  c->srl(a1, v0, 31);                               // srl a1, v0, 31
  c->or_(v1, v1, a1);                               // or v1, v1, a1
  c->daddu(v0, v0, v1);                             // daddu v0, v0, v1
  c->sll(v0, v0, 1);                                // sll v0, v0, 1
  c->srl(v0, v0, 1);                                // srl v0, v0, 1
  c->sw(v0, 0, t0);                                 // sw v0, 0(t0)
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->dsra(v1, v1, 8);                               // dsra v1, v1, 8
  c->lui(a1, 16256);                                // lui a1, 16256
  c->or_(v1, a1, v1);                               // or v1, a1, v1
  c->lui(a1, -16512);                               // lui a1, -16512
  c->mtc1(f1, a1);                                  // mtc1 f1, a1
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  c->adds(f1, f1, f2);                              // add.s f1, f1, f2
  c->mfc1(v1, f1);                                  // mfc1 v1, f1
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->lwu(v1, 28, a3);                               // lwu v1, 28(a3)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L17
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_33;}                          // branch non-likely

  c->lwu(t0, 4, a3);                                // lwu t0, 4(a3)
  c->daddiu(a1, sp, 144);                           // daddiu a1, sp, 144
  c->daddiu(v1, sp, 160);                           // daddiu v1, sp, 160
  c->mfc1(t1, f1);                                  // mfc1 t1, f1
  c->mov128_vf_gpr(vf27, t1);                       // qmtc2.i vf27, t1
  c->lqc2(vf24, 12, t0);                            // lqc2 vf24, 12(t0)
  c->lqc2(vf25, 28, t0);                            // lqc2 vf25, 28(t0)
  c->lqc2(vf26, 44, t0);                            // lqc2 vf26, 44(t0)
  c->vmini_bc(DEST::xyzw, BC::w, vf27, vf27, vf0);  // vminiw.xyzw vf27, vf27, vf0
  c->vmax_bc(DEST::xyzw, BC::x, vf27, vf27, vf0);   // vmaxx.xyzw vf27, vf27, vf0
  c->vadd_bc(DEST::xyzw, BC::x, vf28, vf24, vf27);  // vaddx.xyzw vf28, vf24, vf27
  c->vmula_bc(DEST::xyzw, BC::w, vf25, vf0);        // vmulaw.xyzw acc, vf25, vf0
  c->vmadd(DEST::xyzw, vf29, vf28, vf26);           // vmadd.xyzw vf29, vf28, vf26
  c->sqc2(vf28, 0, a1);                             // sqc2 vf28, 0(a1)
  c->sqc2(vf29, 0, v1);                             // sqc2 vf29, 0(v1)
  c->lw(t0, 8, a1);                                 // lw t0, 8(a1)
  c->lw(a1, 4, a1);                                 // lw a1, 4(a1)
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(t0)) >= 0;                   // bgez t0, L15
  c->lw(v0, 8, v1);                                 // lw v0, 8(v1)
  if (bc) {goto block_29;}                          // branch non-likely

  bc = ((s64)c->sgpr64(a1)) >= 0;                   // bgez a1, L15
  c->lw(v0, 4, v1);                                 // lw v0, 4(v1)
  if (bc) {goto block_29;}                          // branch non-likely

  c->lw(v0, 0, v1);                                 // lw v0, 0(v1)

block_29:
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->lwu(t0, 28, a3);                               // lwu t0, 28(a3)
  c->movs(f2, f0);                                  // mov.s f2, f0
  c->daddiu(a1, sp, 176);                           // daddiu a1, sp, 176
  c->daddiu(v1, sp, 192);                           // daddiu v1, sp, 192
  c->mfc1(t1, f2);                                  // mfc1 t1, f2
  c->mov128_vf_gpr(vf27, t1);                       // qmtc2.i vf27, t1
  c->lqc2(vf24, 12, t0);                            // lqc2 vf24, 12(t0)
  c->lqc2(vf25, 28, t0);                            // lqc2 vf25, 28(t0)
  c->lqc2(vf26, 44, t0);                            // lqc2 vf26, 44(t0)
  c->vmini_bc(DEST::xyzw, BC::w, vf27, vf27, vf0);  // vminiw.xyzw vf27, vf27, vf0
  c->vmax_bc(DEST::xyzw, BC::x, vf27, vf27, vf0);   // vmaxx.xyzw vf27, vf27, vf0
  c->vadd_bc(DEST::xyzw, BC::x, vf28, vf24, vf27);  // vaddx.xyzw vf28, vf24, vf27
  c->vmula_bc(DEST::xyzw, BC::w, vf25, vf0);        // vmulaw.xyzw acc, vf25, vf0
  c->vmadd(DEST::xyzw, vf29, vf28, vf26);           // vmadd.xyzw vf29, vf28, vf26
  c->sqc2(vf28, 0, a1);                             // sqc2 vf28, 0(a1)
  c->sqc2(vf29, 0, v1);                             // sqc2 vf29, 0(v1)
  c->lw(t0, 8, a1);                                 // lw t0, 8(a1)
  c->lw(a1, 4, a1);                                 // lw a1, 4(a1)
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(t0)) >= 0;                   // bgez t0, L16
  c->lw(v0, 8, v1);                                 // lw v0, 8(v1)
  if (bc) {goto block_32;}                          // branch non-likely

  bc = ((s64)c->sgpr64(a1)) >= 0;                   // bgez a1, L16
  c->lw(v0, 4, v1);                                 // lw v0, 4(v1)
  if (bc) {goto block_32;}                          // branch non-likely

  c->lw(v0, 0, v1);                                 // lw v0, 0(v1)

block_32:
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  c->muls(f1, f1, f2);                              // mul.s f1, f1, f2
  c->swc1(f1, 44, a2);                              // swc1 f1, 44(a2)
  c->mfc1(v1, f1);                                  // mfc1 v1, f1

block_33:
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  c->lwu(v1, 8, a3);                                // lwu v1, 8(a3)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L20
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_42;}                          // branch non-likely

  c->load_symbol2(t0, cache.random_generator);      // lw t0, *random-generator*(s7)
  c->lwu(a1, 0, t0);                                // lwu a1, 0(t0)
  c->addiu(v1, r0, 16807);                          // addiu v1, r0, 16807
  c->mult3(v0, v1, a1);                             // mult3 v0, v1, a1
  c->mfhi(v1);                                      // mfhi v1
  c->daddu(v1, v1, v1);                             // daddu v1, v1, v1
  c->srl(a1, v0, 31);                               // srl a1, v0, 31
  c->or_(v1, v1, a1);                               // or v1, v1, a1
  c->daddu(v0, v0, v1);                             // daddu v0, v0, v1
  c->sll(v0, v0, 1);                                // sll v0, v0, 1
  c->srl(v0, v0, 1);                                // srl v0, v0, 1
  c->sw(v0, 0, t0);                                 // sw v0, 0(t0)
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->dsra(v1, v1, 8);                               // dsra v1, v1, 8
  c->lui(a1, 16256);                                // lui a1, 16256
  c->or_(v1, a1, v1);                               // or v1, a1, v1
  c->lui(a1, -16512);                               // lui a1, -16512
  c->mtc1(f1, a1);                                  // mtc1 f1, a1
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  c->adds(f1, f1, f2);                              // add.s f1, f1, f2
  c->mfc1(v1, f1);                                  // mfc1 v1, f1
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  c->lwu(v1, 32, a3);                               // lwu v1, 32(a3)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L20
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_42;}                          // branch non-likely

  c->lui(v1, 17792);                                // lui v1, 17792
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->lwu(t0, 8, a3);                                // lwu t0, 8(a3)
  c->daddiu(a1, sp, 208);                           // daddiu a1, sp, 208
  c->daddiu(v1, sp, 224);                           // daddiu v1, sp, 224
  c->mfc1(t1, f2);                                  // mfc1 t1, f2
  c->mov128_vf_gpr(vf27, t1);                       // qmtc2.i vf27, t1
  c->lqc2(vf24, 12, t0);                            // lqc2 vf24, 12(t0)
  c->lqc2(vf25, 28, t0);                            // lqc2 vf25, 28(t0)
  c->lqc2(vf26, 44, t0);                            // lqc2 vf26, 44(t0)
  c->vmini_bc(DEST::xyzw, BC::w, vf27, vf27, vf0);  // vminiw.xyzw vf27, vf27, vf0
  c->vmax_bc(DEST::xyzw, BC::x, vf27, vf27, vf0);   // vmaxx.xyzw vf27, vf27, vf0
  c->vadd_bc(DEST::xyzw, BC::x, vf28, vf24, vf27);  // vaddx.xyzw vf28, vf24, vf27
  c->vmula_bc(DEST::xyzw, BC::w, vf25, vf0);        // vmulaw.xyzw acc, vf25, vf0
  c->vmadd(DEST::xyzw, vf29, vf28, vf26);           // vmadd.xyzw vf29, vf28, vf26
  c->sqc2(vf28, 0, a1);                             // sqc2 vf28, 0(a1)
  c->sqc2(vf29, 0, v1);                             // sqc2 vf29, 0(v1)
  c->lw(t0, 8, a1);                                 // lw t0, 8(a1)
  c->lw(a1, 4, a1);                                 // lw a1, 4(a1)
  bc = ((s64)c->sgpr64(t0)) >= 0;                   // bgez t0, L18
  c->lw(v0, 8, v1);                                 // lw v0, 8(v1)
  if (bc) {goto block_38;}                          // branch non-likely

  bc = ((s64)c->sgpr64(a1)) >= 0;                   // bgez a1, L18
  c->lw(v0, 4, v1);                                 // lw v0, 4(v1)
  if (bc) {goto block_38;}                          // branch non-likely

  c->lw(v0, 0, v1);                                 // lw v0, 0(v1)

block_38:
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  c->muls(f1, f1, f2);                              // mul.s f1, f1, f2
  c->lwu(t0, 32, a3);                               // lwu t0, 32(a3)
  c->movs(f2, f0);                                  // mov.s f2, f0
  c->daddiu(a1, sp, 240);                           // daddiu a1, sp, 240
  c->daddiu(v1, sp, 256);                           // daddiu v1, sp, 256
  c->mfc1(t1, f2);                                  // mfc1 t1, f2
  c->mov128_vf_gpr(vf27, t1);                       // qmtc2.i vf27, t1
  c->lqc2(vf24, 12, t0);                            // lqc2 vf24, 12(t0)
  c->lqc2(vf25, 28, t0);                            // lqc2 vf25, 28(t0)
  c->lqc2(vf26, 44, t0);                            // lqc2 vf26, 44(t0)
  c->vmini_bc(DEST::xyzw, BC::w, vf27, vf27, vf0);  // vminiw.xyzw vf27, vf27, vf0
  c->vmax_bc(DEST::xyzw, BC::x, vf27, vf27, vf0);   // vmaxx.xyzw vf27, vf27, vf0
  c->vadd_bc(DEST::xyzw, BC::x, vf28, vf24, vf27);  // vaddx.xyzw vf28, vf24, vf27
  c->vmula_bc(DEST::xyzw, BC::w, vf25, vf0);        // vmulaw.xyzw acc, vf25, vf0
  c->vmadd(DEST::xyzw, vf29, vf28, vf26);           // vmadd.xyzw vf29, vf28, vf26
  c->sqc2(vf28, 0, a1);                             // sqc2 vf28, 0(a1)
  c->sqc2(vf29, 0, v1);                             // sqc2 vf29, 0(v1)
  c->lw(t0, 8, a1);                                 // lw t0, 8(a1)
  c->lw(a1, 4, a1);                                 // lw a1, 4(a1)
  bc = ((s64)c->sgpr64(t0)) >= 0;                   // bgez t0, L19
  c->lw(v0, 8, v1);                                 // lw v0, 8(v1)
  if (bc) {goto block_41;}                          // branch non-likely

  bc = ((s64)c->sgpr64(a1)) >= 0;                   // bgez a1, L19
  c->lw(v0, 4, v1);                                 // lw v0, 4(v1)
  if (bc) {goto block_41;}                          // branch non-likely

  c->lw(v0, 0, v1);                                 // lw v0, 0(v1)

block_41:
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  c->muls(f1, f1, f2);                              // mul.s f1, f1, f2
  c->abss(f1, f1);                                  // abs.s f1, f1
  c->swc1(f1, 12, a2);                              // swc1 f1, 12(a2)
  c->mfc1(v1, f1);                                  // mfc1 v1, f1

block_42:
  c->ld(v1, 56, a3);                                // ld v1, 56(a3)
  c->andi(v1, v1, 1);                               // andi v1, v1, 1
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L21
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_44;}                          // branch non-likely

  c->lwc1(f0, 12, a2);                              // lwc1 f0, 12(a2)
  c->swc1(f0, 28, a2);                              // swc1 f0, 28(a2)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  //beq r0, r0, L24                                 // beq r0, r0, L24
  // nop                                            // sll r0, r0, 0
  goto block_53;                                    // branch always


block_44:
  c->lwu(v1, 12, a3);                               // lwu v1, 12(a3)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L24
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_53;}                          // branch non-likely

  c->load_symbol2(t0, cache.random_generator);      // lw t0, *random-generator*(s7)
  c->lwu(a1, 0, t0);                                // lwu a1, 0(t0)
  c->addiu(v1, r0, 16807);                          // addiu v1, r0, 16807
  c->mult3(v0, v1, a1);                             // mult3 v0, v1, a1
  c->mfhi(v1);                                      // mfhi v1
  c->daddu(v1, v1, v1);                             // daddu v1, v1, v1
  c->srl(a1, v0, 31);                               // srl a1, v0, 31
  c->or_(v1, v1, a1);                               // or v1, v1, a1
  c->daddu(v0, v0, v1);                             // daddu v0, v0, v1
  c->sll(v0, v0, 1);                                // sll v0, v0, 1
  c->srl(v0, v0, 1);                                // srl v0, v0, 1
  c->sw(v0, 0, t0);                                 // sw v0, 0(t0)
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->dsra(v1, v1, 8);                               // dsra v1, v1, 8
  c->lui(a1, 16256);                                // lui a1, 16256
  c->or_(v1, a1, v1);                               // or v1, a1, v1
  c->lui(a1, -16512);                               // lui a1, -16512
  c->mtc1(f1, a1);                                  // mtc1 f1, a1
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  c->adds(f1, f1, f2);                              // add.s f1, f1, f2
  c->mfc1(v1, f1);                                  // mfc1 v1, f1
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  c->lwu(v1, 36, a3);                               // lwu v1, 36(a3)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L24
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_53;}                          // branch non-likely

  c->lui(v1, 17792);                                // lui v1, 17792
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->lwu(t0, 12, a3);                               // lwu t0, 12(a3)
  c->daddiu(a1, sp, 272);                           // daddiu a1, sp, 272
  c->daddiu(v1, sp, 288);                           // daddiu v1, sp, 288
  c->mfc1(t1, f2);                                  // mfc1 t1, f2
  c->mov128_vf_gpr(vf27, t1);                       // qmtc2.i vf27, t1
  c->lqc2(vf24, 12, t0);                            // lqc2 vf24, 12(t0)
  c->lqc2(vf25, 28, t0);                            // lqc2 vf25, 28(t0)
  c->lqc2(vf26, 44, t0);                            // lqc2 vf26, 44(t0)
  c->vmini_bc(DEST::xyzw, BC::w, vf27, vf27, vf0);  // vminiw.xyzw vf27, vf27, vf0
  c->vmax_bc(DEST::xyzw, BC::x, vf27, vf27, vf0);   // vmaxx.xyzw vf27, vf27, vf0
  c->vadd_bc(DEST::xyzw, BC::x, vf28, vf24, vf27);  // vaddx.xyzw vf28, vf24, vf27
  c->vmula_bc(DEST::xyzw, BC::w, vf25, vf0);        // vmulaw.xyzw acc, vf25, vf0
  c->vmadd(DEST::xyzw, vf29, vf28, vf26);           // vmadd.xyzw vf29, vf28, vf26
  c->sqc2(vf28, 0, a1);                             // sqc2 vf28, 0(a1)
  c->sqc2(vf29, 0, v1);                             // sqc2 vf29, 0(v1)
  c->lw(t0, 8, a1);                                 // lw t0, 8(a1)
  c->lw(a1, 4, a1);                                 // lw a1, 4(a1)
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(t0)) >= 0;                   // bgez t0, L22
  c->lw(v0, 8, v1);                                 // lw v0, 8(v1)
  if (bc) {goto block_49;}                          // branch non-likely

  bc = ((s64)c->sgpr64(a1)) >= 0;                   // bgez a1, L22
  c->lw(v0, 4, v1);                                 // lw v0, 4(v1)
  if (bc) {goto block_49;}                          // branch non-likely

  c->lw(v0, 0, v1);                                 // lw v0, 0(v1)

block_49:
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  c->muls(f1, f1, f2);                              // mul.s f1, f1, f2
  c->lwu(a3, 36, a3);                               // lwu a3, 36(a3)
  c->daddiu(a1, sp, 304);                           // daddiu a1, sp, 304
  c->daddiu(v1, sp, 320);                           // daddiu v1, sp, 320
  c->mfc1(t0, f0);                                  // mfc1 t0, f0
  c->mov128_vf_gpr(vf27, t0);                       // qmtc2.i vf27, t0
  c->lqc2(vf24, 12, a3);                            // lqc2 vf24, 12(a3)
  c->lqc2(vf25, 28, a3);                            // lqc2 vf25, 28(a3)
  c->lqc2(vf26, 44, a3);                            // lqc2 vf26, 44(a3)
  c->vmini_bc(DEST::xyzw, BC::w, vf27, vf27, vf0);  // vminiw.xyzw vf27, vf27, vf0
  c->vmax_bc(DEST::xyzw, BC::x, vf27, vf27, vf0);   // vmaxx.xyzw vf27, vf27, vf0
  c->vadd_bc(DEST::xyzw, BC::x, vf28, vf24, vf27);  // vaddx.xyzw vf28, vf24, vf27
  c->vmula_bc(DEST::xyzw, BC::w, vf25, vf0);        // vmulaw.xyzw acc, vf25, vf0
  c->vmadd(DEST::xyzw, vf29, vf28, vf26);           // vmadd.xyzw vf29, vf28, vf26
  c->sqc2(vf28, 0, a1);                             // sqc2 vf28, 0(a1)
  c->sqc2(vf29, 0, v1);                             // sqc2 vf29, 0(v1)
  c->lw(a3, 8, a1);                                 // lw a3, 8(a1)
  c->lw(a1, 4, a1);                                 // lw a1, 4(a1)
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(a3)) >= 0;                   // bgez a3, L23
  c->lw(v0, 8, v1);                                 // lw v0, 8(v1)
  if (bc) {goto block_52;}                          // branch non-likely

  bc = ((s64)c->sgpr64(a1)) >= 0;                   // bgez a1, L23
  c->lw(v0, 4, v1);                                 // lw v0, 4(v1)
  if (bc) {goto block_52;}                          // branch non-likely

  c->lw(v0, 0, v1);                                 // lw v0, 0(v1)

block_52:
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->muls(f0, f1, f0);                              // mul.s f0, f1, f0
  c->abss(f0, f0);                                  // abs.s f0, f0
  c->swc1(f0, 28, a2);                              // swc1 f0, 28(a2)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0

block_53:
  c->load_symbol2(v1, cache.random_generator);      // lw v1, *random-generator*(s7)
  c->sw(a0, 0, v1);                                 // sw a0, 0(v1)
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 336);                           // daddiu sp, sp, 336
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.random_generator = intern_from_c(-1, 0, "*random-generator*").c();
  gLinkedFunctionTable.reg("live-func-curve", execute, 512);
}

} // namespace live_func_curve
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace birth_func_curve {
struct Cache {
  void* random_generator; // *random-generator*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->daddiu(sp, sp, -112);                          // daddiu sp, sp, -112
  c->mov64(t1, a1);                                 // or t1, a1, r0
  c->lwu(a0, 108, t1);                              // lwu a0, 108(t1)
  c->addiu(v1, r0, 0);                              // addiu v1, r0, 0
  c->load_symbol2(v1, cache.random_generator);      // lw v1, *random-generator*(s7)
  c->lwu(v1, 0, v1);                                // lwu v1, 0(v1)
  c->sw(v1, 12, t1);                                // sw v1, 12(t1)
  c->ld(a3, 40, a0);                                // ld a3, 40(a0)
  c->ld(v1, 48, a0);                                // ld v1, 48(a0)
  c->slt(v1, r0, v1);                               // slt v1, r0, v1
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L26
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_2;}                           // branch non-likely

  c->load_symbol2(t0, cache.random_generator);      // lw t0, *random-generator*(s7)
  c->lwu(a1, 0, t0);                                // lwu a1, 0(t0)
  c->addiu(v1, r0, 16807);                          // addiu v1, r0, 16807
  c->mult3(v0, v1, a1);                             // mult3 v0, v1, a1
  c->mfhi(v1);                                      // mfhi v1
  c->daddu(v1, v1, v1);                             // daddu v1, v1, v1
  c->srl(a1, v0, 31);                               // srl a1, v0, 31
  c->or_(v1, v1, a1);                               // or v1, v1, a1
  c->daddu(v0, v0, v1);                             // daddu v0, v0, v1
  c->sll(v0, v0, 1);                                // sll v0, v0, 1
  c->srl(v0, v0, 1);                                // srl v0, v0, 1
  c->sw(v0, 0, t0);                                 // sw v0, 0(t0)
  c->ld(v1, 48, a0);                                // ld v1, 48(a0)
  c->div(v0, v1);                                   // div v0, v1
  c->mfhi(v1);                                      // mfhi v1
  c->daddu(a3, a3, v1);                             // daddu a3, a3, v1
  c->mov64(v1, a3);                                 // or v1, a3, r0

block_2:
  c->sw(a3, 100, t1);                               // sw a3, 100(t1)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 44, a2);                              // swc1 f0, 44(a2)
  c->lwu(v1, 0, a0);                                // lwu v1, 0(a0)
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(v1))) {// beql s7, v1, L27
    c->mov64(v1, v1);                               // or v1, v1, r0
    goto block_7;
  }

// block_4:
  c->ld(v1, 56, a0);                                // ld v1, 56(a0)
  c->andi(v1, v1, 4);                               // andi v1, v1, 4
  if (((s64)c->sgpr64(v1)) != ((s64)0)) {           // bnel v1, r0, L27
    c->daddiu(v1, s7, 4);                           // daddiu v1, s7, 4
    goto block_7;
  }

//block_6:
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  c->ld(a1, 56, a0);                                // ld a1, 56(a0)
  c->andi(a1, a1, 2);                               // andi a1, a1, 2
  c->movn(v1, s7, a1);                              // movn v1, s7, a1

block_7:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L31
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_14;}                          // branch non-likely

  c->lwu(a3, 0, a0);                                // lwu a3, 0(a0)
  c->load_symbol2(t0, cache.random_generator);      // lw t0, *random-generator*(s7)
  c->lwu(a1, 0, t0);                                // lwu a1, 0(t0)
  c->addiu(v1, r0, 16807);                          // addiu v1, r0, 16807
  c->mult3(v0, v1, a1);                             // mult3 v0, v1, a1
  c->mfhi(v1);                                      // mfhi v1
  c->daddu(v1, v1, v1);                             // daddu v1, v1, v1
  c->srl(a1, v0, 31);                               // srl a1, v0, 31
  c->or_(v1, v1, a1);                               // or v1, v1, a1
  c->daddu(v0, v0, v1);                             // daddu v0, v0, v1
  c->sll(v0, v0, 1);                                // sll v0, v0, 1
  c->srl(v0, v0, 1);                                // srl v0, v0, 1
  c->sw(v0, 0, t0);                                 // sw v0, 0(t0)
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->dsra(v1, v1, 8);                               // dsra v1, v1, 8
  c->lui(a1, 16256);                                // lui a1, 16256
  c->or_(v1, a1, v1);                               // or v1, a1, v1
  c->lui(a1, -16512);                               // lui a1, -16512
  c->mtc1(f0, a1);                                  // mtc1 f0, a1
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->adds(f0, f0, f1);                              // add.s f0, f0, f1
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->daddiu(v1, sp, 16);                            // daddiu v1, sp, 16
  c->daddiu(a1, sp, 32);                            // daddiu a1, sp, 32
  c->mfc1(t0, f0);                                  // mfc1 t0, f0
  c->mov128_vf_gpr(vf26, t0);                       // qmtc2.i vf26, t0
  c->lqc2(vf23, 12, a3);                            // lqc2 vf23, 12(a3)
  c->lqc2(vf25, 92, a3);                            // lqc2 vf25, 92(a3)
  c->vmax_bc(DEST::xyzw, BC::w, vf3, vf0, vf0);     // vmaxw.xyzw vf3, vf0, vf0
  c->vmula(DEST::xyzw,  vf25, vf23);                // vmula.xyzw acc, vf25, vf23
  c->vmadd_bc(DEST::xyzw, BC::x, vf28, vf25, vf26); // vmaddx.xyzw vf28, vf25, vf26
  c->sqc2(vf28, 0, a1);                             // sqc2 vf28, 0(a1)
  c->lw(t0, 8, a1);                                 // lw t0, 8(a1)
  c->lw(a1, 4, a1);                                 // lw a1, 4(a1)
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(t0)) >= 0;                   // bgez t0, L29
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely

  bc = ((s64)c->sgpr64(a1)) >= 0;                   // bgez a1, L28
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_11;}                          // branch non-likely

  c->lqc2(vf1, 28, a3);                             // lqc2 vf1, 28(a3)
  c->lqc2(vf2, 44, a3);                             // lqc2 vf2, 44(a3)
  c->vsub_bc(DEST::xyzw, BC::x, vf4, vf3, vf28);    // vsubx.xyzw vf4, vf3, vf28
  c->vmula_bc(DEST::xyzw, BC::x, vf2, vf28);        // vmulax.xyzw acc, vf2, vf28
  c->vmadd_bc(DEST::xyzw, BC::x, vf5, vf1, vf4);    // vmaddx.xyzw vf5, vf1, vf4
  //beq r0, r0, L30                                 // beq r0, r0, L30
  c->sqc2(vf5, 0, v1);                              // sqc2 vf5, 0(v1)
  goto block_13;                                    // branch always


block_11:
  c->lqc2(vf1, 44, a3);                             // lqc2 vf1, 44(a3)
  c->lqc2(vf2, 60, a3);                             // lqc2 vf2, 60(a3)
  c->vsub_bc(DEST::xyzw, BC::y, vf4, vf3, vf28);    // vsuby.xyzw vf4, vf3, vf28
  c->vmula_bc(DEST::xyzw, BC::y, vf2, vf28);        // vmulay.xyzw acc, vf2, vf28
  c->vmadd_bc(DEST::xyzw, BC::y, vf5, vf1, vf4);    // vmaddy.xyzw vf5, vf1, vf4
  //beq r0, r0, L30                                 // beq r0, r0, L30
  c->sqc2(vf5, 0, v1);                              // sqc2 vf5, 0(v1)
  goto block_13;                                    // branch always


block_12:
  c->lqc2(vf1, 60, a3);                             // lqc2 vf1, 60(a3)
  c->lqc2(vf2, 76, a3);                             // lqc2 vf2, 76(a3)
  c->vsub_bc(DEST::xyzw, BC::z, vf4, vf3, vf28);    // vsubz.xyzw vf4, vf3, vf28
  c->vmula_bc(DEST::xyzw, BC::z, vf2, vf28);        // vmulaz.xyzw acc, vf2, vf28
  c->vmadd_bc(DEST::xyzw, BC::z, vf5, vf1, vf4);    // vmaddz.xyzw vf5, vf1, vf4
  c->sqc2(vf5, 0, v1);                              // sqc2 vf5, 0(v1)

block_13:
  c->lwc1(f0, 0, v1);                               // lwc1 f0, 0(v1)
  c->swc1(f0, 32, a2);                              // swc1 f0, 32(a2)
  c->lwc1(f0, 4, v1);                               // lwc1 f0, 4(v1)
  c->swc1(f0, 36, a2);                              // swc1 f0, 36(a2)
  c->lwc1(f0, 8, v1);                               // lwc1 f0, 8(v1)
  c->swc1(f0, 40, a2);                              // swc1 f0, 40(a2)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0

block_14:
  c->ld(v1, 56, a0);                                // ld v1, 56(a0)
  c->andi(v1, v1, 4);                               // andi v1, v1, 4
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L36
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_27;}                          // branch non-likely

  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->lwu(v1, 8, a0);                                // lwu v1, 8(a0)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L33
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_20;}                          // branch non-likely

  c->load_symbol2(a3, cache.random_generator);      // lw a3, *random-generator*(s7)
  c->lwu(a1, 0, a3);                                // lwu a1, 0(a3)
  c->addiu(v1, r0, 16807);                          // addiu v1, r0, 16807
  c->mult3(v0, v1, a1);                             // mult3 v0, v1, a1
  c->mfhi(v1);                                      // mfhi v1
  c->daddu(v1, v1, v1);                             // daddu v1, v1, v1
  c->srl(a1, v0, 31);                               // srl a1, v0, 31
  c->or_(v1, v1, a1);                               // or v1, v1, a1
  c->daddu(v0, v0, v1);                             // daddu v0, v0, v1
  c->sll(v0, v0, 1);                                // sll v0, v0, 1
  c->srl(v0, v0, 1);                                // srl v0, v0, 1
  c->sw(v0, 0, a3);                                 // sw v0, 0(a3)
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->dsra(v1, v1, 8);                               // dsra v1, v1, 8
  c->lui(a1, 16256);                                // lui a1, 16256
  c->or_(v1, a1, v1);                               // or v1, a1, v1
  c->lui(a1, -16512);                               // lui a1, -16512
  c->mtc1(f0, a1);                                  // mtc1 f0, a1
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->adds(f0, f0, f1);                              // add.s f0, f0, f1
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->lui(v1, 17792);                                // lui v1, 17792
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lwu(a3, 8, a0);                                // lwu a3, 8(a0)
  c->daddiu(a1, sp, 48);                            // daddiu a1, sp, 48
  c->daddiu(v1, sp, 64);                            // daddiu v1, sp, 64
  c->mfc1(t0, f1);                                  // mfc1 t0, f1
  c->mov128_vf_gpr(vf27, t0);                       // qmtc2.i vf27, t0
  c->lqc2(vf24, 12, a3);                            // lqc2 vf24, 12(a3)
  c->lqc2(vf25, 28, a3);                            // lqc2 vf25, 28(a3)
  c->lqc2(vf26, 44, a3);                            // lqc2 vf26, 44(a3)
  c->vmini_bc(DEST::xyzw, BC::w, vf27, vf27, vf0);  // vminiw.xyzw vf27, vf27, vf0
  c->vmax_bc(DEST::xyzw, BC::x, vf27, vf27, vf0);   // vmaxx.xyzw vf27, vf27, vf0
  c->vadd_bc(DEST::xyzw, BC::x, vf28, vf24, vf27);  // vaddx.xyzw vf28, vf24, vf27
  c->vmula_bc(DEST::xyzw, BC::w, vf25, vf0);        // vmulaw.xyzw acc, vf25, vf0
  c->vmadd(DEST::xyzw, vf29, vf28, vf26);           // vmadd.xyzw vf29, vf28, vf26
  c->sqc2(vf28, 0, a1);                             // sqc2 vf28, 0(a1)
  c->sqc2(vf29, 0, v1);                             // sqc2 vf29, 0(v1)
  c->lw(a3, 8, a1);                                 // lw a3, 8(a1)
  c->lw(a1, 4, a1);                                 // lw a1, 4(a1)
  bc = ((s64)c->sgpr64(a3)) >= 0;                   // bgez a3, L32
  c->lw(v0, 8, v1);                                 // lw v0, 8(v1)
  if (bc) {goto block_19;}                          // branch non-likely

  bc = ((s64)c->sgpr64(a1)) >= 0;                   // bgez a1, L32
  c->lw(v0, 4, v1);                                 // lw v0, 4(v1)
  if (bc) {goto block_19;}                          // branch non-likely

  c->lw(v0, 0, v1);                                 // lw v0, 0(v1)

block_19:
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->swc1(f0, 12, a2);                              // swc1 f0, 12(a2)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0

block_20:
  c->ld(v1, 56, a0);                                // ld v1, 56(a0)
  c->andi(v1, v1, 1);                               // andi v1, v1, 1
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L34
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_22;}                          // branch non-likely

  c->lwc1(f0, 12, a2);                              // lwc1 f0, 12(a2)
  c->swc1(f0, 28, a2);                              // swc1 f0, 28(a2)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  //beq r0, r0, L36                                 // beq r0, r0, L36
  // nop                                            // sll r0, r0, 0
  goto block_27;                                    // branch always


block_22:
  c->lwu(v1, 12, a0);                               // lwu v1, 12(a0)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L36
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_27;}                          // branch non-likely

  c->load_symbol2(a3, cache.random_generator);      // lw a3, *random-generator*(s7)
  c->lwu(a1, 0, a3);                                // lwu a1, 0(a3)
  c->addiu(v1, r0, 16807);                          // addiu v1, r0, 16807
  c->mult3(v0, v1, a1);                             // mult3 v0, v1, a1
  c->mfhi(v1);                                      // mfhi v1
  c->daddu(v1, v1, v1);                             // daddu v1, v1, v1
  c->srl(a1, v0, 31);                               // srl a1, v0, 31
  c->or_(v1, v1, a1);                               // or v1, v1, a1
  c->daddu(v0, v0, v1);                             // daddu v0, v0, v1
  c->sll(v0, v0, 1);                                // sll v0, v0, 1
  c->srl(v0, v0, 1);                                // srl v0, v0, 1
  c->sw(v0, 0, a3);                                 // sw v0, 0(a3)
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->dsra(v1, v1, 8);                               // dsra v1, v1, 8
  c->lui(a1, 16256);                                // lui a1, 16256
  c->or_(v1, a1, v1);                               // or v1, a1, v1
  c->lui(a1, -16512);                               // lui a1, -16512
  c->mtc1(f0, a1);                                  // mtc1 f0, a1
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->adds(f0, f0, f1);                              // add.s f0, f0, f1
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lui(v1, 17792);                                // lui v1, 17792
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lwu(a0, 12, a0);                               // lwu a0, 12(a0)
  c->load_symbol2(a3, cache.random_generator);      // lw a3, *random-generator*(s7)
  c->lwu(a1, 0, a3);                                // lwu a1, 0(a3)
  c->addiu(v1, r0, 16807);                          // addiu v1, r0, 16807
  c->mult3(v0, v1, a1);                             // mult3 v0, v1, a1
  c->mfhi(v1);                                      // mfhi v1
  c->daddu(v1, v1, v1);                             // daddu v1, v1, v1
  c->srl(a1, v0, 31);                               // srl a1, v0, 31
  c->or_(v1, v1, a1);                               // or v1, v1, a1
  c->daddu(v0, v0, v1);                             // daddu v0, v0, v1
  c->sll(v0, v0, 1);                                // sll v0, v0, 1
  c->srl(v0, v0, 1);                                // srl v0, v0, 1
  c->sw(v0, 0, a3);                                 // sw v0, 0(a3)
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->dsra(v1, v1, 8);                               // dsra v1, v1, 8
  c->lui(a1, 16256);                                // lui a1, 16256
  c->or_(v1, a1, v1);                               // or v1, a1, v1
  c->lui(a1, -16512);                               // lui a1, -16512
  c->mtc1(f1, a1);                                  // mtc1 f1, a1
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  c->adds(f1, f1, f2);                              // add.s f1, f1, f2
  c->mfc1(v1, f1);                                  // mfc1 v1, f1
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->daddiu(a1, sp, 80);                            // daddiu a1, sp, 80
  c->daddiu(v1, sp, 96);                            // daddiu v1, sp, 96
  c->mfc1(a3, f1);                                  // mfc1 a3, f1
  c->mov128_vf_gpr(vf27, a3);                       // qmtc2.i vf27, a3
  c->lqc2(vf24, 12, a0);                            // lqc2 vf24, 12(a0)
  c->lqc2(vf25, 28, a0);                            // lqc2 vf25, 28(a0)
  c->lqc2(vf26, 44, a0);                            // lqc2 vf26, 44(a0)
  c->vmini_bc(DEST::xyzw, BC::w, vf27, vf27, vf0);  // vminiw.xyzw vf27, vf27, vf0
  c->vmax_bc(DEST::xyzw, BC::x, vf27, vf27, vf0);   // vmaxx.xyzw vf27, vf27, vf0
  c->vadd_bc(DEST::xyzw, BC::x, vf28, vf24, vf27);  // vaddx.xyzw vf28, vf24, vf27
  c->vmula_bc(DEST::xyzw, BC::w, vf25, vf0);        // vmulaw.xyzw acc, vf25, vf0
  c->vmadd(DEST::xyzw, vf29, vf28, vf26);           // vmadd.xyzw vf29, vf28, vf26
  c->sqc2(vf28, 0, a1);                             // sqc2 vf28, 0(a1)
  c->sqc2(vf29, 0, v1);                             // sqc2 vf29, 0(v1)
  c->lw(a0, 8, a1);                                 // lw a0, 8(a1)
  c->lw(a1, 4, a1);                                 // lw a1, 4(a1)
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(a0)) >= 0;                   // bgez a0, L35
  c->lw(v0, 8, v1);                                 // lw v0, 8(v1)
  if (bc) {goto block_26;}                          // branch non-likely

  bc = ((s64)c->sgpr64(a1)) >= 0;                   // bgez a1, L35
  c->lw(v0, 4, v1);                                 // lw v0, 4(v1)
  if (bc) {goto block_26;}                          // branch non-likely

  c->lw(v0, 0, v1);                                 // lw v0, 0(v1)

block_26:
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->swc1(f0, 28, a2);                              // swc1 f0, 28(a2)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0

block_27:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 112);                           // daddiu sp, sp, 112
  goto end_of_function;                             // return

end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.random_generator = intern_from_c(-1, 0, "*random-generator*").c();
  gLinkedFunctionTable.reg("birth-func-curve", execute, 128);
}

} // namespace birth_func_curve
} // namespace Mips2C