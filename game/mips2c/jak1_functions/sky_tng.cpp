//--------------------------MIPS2C---------------------

#include "game/kernel/jak1/kscheme.h"
#include "game/mips2c/mips2c_private.h"
using namespace jak1;
namespace Mips2C::jak1 {

ExecutionContext sky_regs_vfs;

}  // namespace Mips2C::jak1

// clang-format off
//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace init_sky_regs {
struct Cache {
  void* math_camera; // *math-camera*
  void* sky_tng_data; // *sky-tng-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -32);                           // daddiu sp, sp, -32
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  c->load_symbol(v1, cache.math_camera);            // lw v1, *math-camera*(s7)
  c->daddiu(a0, sp, 16);                            // daddiu a0, sp, 16
  c->sq(r0, 0, a0);                                 // sq r0, 0(a0)
  c->mov64(a1, a0);                                 // or a1, a0, r0
  c->daddiu(a2, v1, 732);                           // daddiu a2, v1, 732
  c->lq(a2, 0, a2);                                 // lq a2, 0(a2)
  c->sq(a2, 0, a1);                                 // sq a2, 0(a1)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->lwc1(f1, 848, v1);                             // lwc1 f1, 848(v1)
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = !cop1_bc;                                    // bc1f L70
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  c->fprs[f0] = 2049.0;                             // lwc1 f0, L98(fp)
  c->swc1(f0, 4, a0);                               // swc1 f0, 4(a0)
  c->mfc1(a1, f0);                                  // mfc1 a1, f0
  //beq r0, r0, L71                                 // beq r0, r0, L71
  // nop                                            // sll r0, r0, 0
  goto block_3;                                     // branch always


  block_2:
  c->fprs[f0] = 2047.0;                             // lwc1 f0, L100(fp)
  c->swc1(f0, 4, a0);                               // swc1 f0, 4(a0)
  c->mfc1(a1, f0);                                  // mfc1 a1, f0

  block_3:
  c->load_symbol(a1, cache.sky_tng_data);           // lw a1, *sky-tng-data*(s7)
  c->daddiu(a1, a1, 60);                            // daddiu a1, a1, 60
  c->lwc1(f0, 828, v1);                             // lwc1 f0, 828(v1)
  c->swc1(f0, 0, a1);                               // swc1 f0, 0(a1)
  c->lwc1(f0, 128, v1);                             // lwc1 f0, 128(v1)
  c->swc1(f0, 4, a1);                               // swc1 f0, 4(a1)
  c->lwc1(f0, 124, v1);                             // lwc1 f0, 124(v1)
  c->swc1(f0, 8, a1);                               // swc1 f0, 8(a1)
  c->fprs[f0] = 3071.0;                             // lwc1 f0, L83(fp)
  c->swc1(f0, 12, a1);                              // swc1 f0, 12(a1)
  c->lqc2(vf31, 572, v1);                           // lqc2 vf31, 572(v1)
  c->lqc2(vf30, 588, v1);                           // lqc2 vf30, 588(v1)
  c->lqc2(vf29, 604, v1);                           // lqc2 vf29, 604(v1)
  c->lqc2(vf28, 620, v1);                           // lqc2 vf28, 620(v1)
  c->lqc2(vf14, 700, v1);                           // lqc2 vf14, 700(v1)
  c->lqc2(vf26, 716, v1);                           // lqc2 vf26, 716(v1)
  c->lqc2(vf25, 0, a0);                             // lqc2 vf25, 0(a0)
  c->load_symbol(v1, cache.sky_tng_data);           // lw v1, *sky-tng-data*(s7)
  c->lqc2(vf13, 60, v1);                            // lqc2 vf13, 60(v1)
  c->vmul(DEST::xyzw, vf31, vf31, vf14);            // vmul.xyzw vf31, vf31, vf14
  c->vmul(DEST::xyzw, vf30, vf30, vf14);            // vmul.xyzw vf30, vf30, vf14
  c->vmul(DEST::xyzw, vf29, vf29, vf14);            // vmul.xyzw vf29, vf29, vf14
  c->vmul(DEST::xyzw, vf28, vf28, vf14);            // vmul.xyzw vf28, vf28, vf14
  c->vmove(DEST::z, vf25, vf0);                     // vmove.z vf25, vf0
  c->vmove(DEST::xyzw, vf24, vf0);                  // vmove.xyzw vf24, vf0
  c->vmove(DEST::xyzw, vf23, vf0);                  // vmove.xyzw vf23, vf0
  c->mov128_gpr_vf(v1, vf23);                       // qmfc2.i v1, vf23
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 32);                            // daddiu sp, sp, 32
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  sky_regs_vfs.copy_vfs_from_other(c);
  return c->gprs[v0].du64[0];
}

void link() {
  cache.math_camera = intern_from_c("*math-camera*").c();
  cache.sky_tng_data = intern_from_c("*sky-tng-data*").c();
  gLinkedFunctionTable.reg("init-sky-regs", execute, 256);
}

} // namespace init_sky_regs
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace set_tex_offset {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  c->daddiu(sp, sp, -32);                           // daddiu sp, sp, -32
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  c->daddiu(v1, sp, 16);                            // daddiu v1, sp, 16
  c->sq(r0, 0, v1);                                 // sq r0, 0(v1)
  c->fprs[f0] = 0.000015258789;                     // lwc1 f0, L99(fp)
  c->mtc1(f1, a0);                                  // mtc1 f1, a0
  c->cvtsw(f1, f1);                                 // cvt.s.w f1, f1
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->swc1(f0, 0, v1);                               // swc1 f0, 0(v1)
  c->fprs[f0] = 0.000015258789;                     // lwc1 f0, L99(fp)
  c->mtc1(f1, a1);                                  // mtc1 f1, a1
  c->cvtsw(f1, f1);                                 // cvt.s.w f1, f1
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->swc1(f0, 4, v1);                               // swc1 f0, 4(v1)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 8, v1);                               // swc1 f0, 8(v1)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 12, v1);                              // swc1 f0, 12(v1)
  c->lqc2(vf24, 0, v1);                             // lqc2 vf24, 0(v1)
  c->mov128_gpr_vf(v1, vf24);                       // qmfc2.i v1, vf24
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 32);                            // daddiu sp, sp, 32
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  sky_regs_vfs.copy_vfs_from_other(c);
  return c->gprs[v0].du64[0];
}

void link() {
  gLinkedFunctionTable.reg("set-tex-offset", execute, 64);
}

} // namespace set_tex_offset
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
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
  bc = !cop1_bc;                                    // bc1f L60
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
  bc = !cop1_bc;                                    // bc1f L58
  c->lqc2(vf6, -16, a2);                            // lqc2 vf6, -16(a2)
  if (bc) {goto block_6;}                           // branch non-likely

  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L64
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
  bc = !cop1_bc;                                    // bc1f L59
  c->lqc2(vf3, -16, a2);                            // lqc2 vf3, -16(a2)
  if (bc) {goto block_8;}                           // branch non-likely

  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L56
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  //beq r0, r0, L64                                 // beq r0, r0, L64
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
  c->daddiu(t6, s7, 8);                             // daddiu t6, s7, 8
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
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L61
  c->sqc2(vf9, -16, a3);                            // sqc2 vf9, -16(a3)
  if (bc) {goto block_11;}                          // branch non-likely

  //beq r0, r0, L64                                 // beq r0, r0, L64
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
  c->daddiu(t6, s7, 8);                             // daddiu t6, s7, 8
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
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L64
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
  bc = cop1_bc;                                     // bc1t L62
  c->lqc2(vf6, -16, a2);                            // lqc2 vf6, -16(a2)
  if (bc) {goto block_14;}                          // branch non-likely

  c->sqc2(vf1, 0, a3);                              // sqc2 vf1, 0(a3)
  c->daddiu(a3, a3, 48);                            // daddiu a3, a3, 48
  c->sqc2(vf2, -32, a3);                            // sqc2 vf2, -32(a3)
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L64
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
  bc = cop1_bc;                                     // bc1t L63
  c->lqc2(vf3, -16, a2);                            // lqc2 vf3, -16(a2)
  if (bc) {goto block_16;}                          // branch non-likely

  c->sqc2(vf4, 0, a3);                              // sqc2 vf4, 0(a3)
  c->daddiu(a3, a3, 48);                            // daddiu a3, a3, 48
  c->sqc2(vf5, -32, a3);                            // sqc2 vf5, -32(a3)
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L60
  c->sqc2(vf6, -16, a3);                            // sqc2 vf6, -16(a3)
  if (bc) {goto block_9;}                           // branch non-likely

  //beq r0, r0, L64                                 // beq r0, r0, L64
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
  c->daddiu(t6, s7, 8);                             // daddiu t6, s7, 8
  c->vmula_bc(DEST::xyzw, BC::w, vf6, vf0);         // vmulaw.xyzw acc, vf6, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf9, vf10);   // vmaddx.xyzw vf9, vf9, vf10
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf7, -48, a3);                            // sqc2 vf7, -48(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, -32, a3);                            // sqc2 vf8, -32(a3)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L57
  c->sqc2(vf9, -16, a3);                            // sqc2 vf9, -16(a3)
  if (bc) {goto block_3;}                           // branch non-likely

  //beq r0, r0, L64                                 // beq r0, r0, L64
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
  c->daddiu(t6, s7, 8);                             // daddiu t6, s7, 8
  c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf9, vf10);   // vmaddx.xyzw vf9, vf9, vf10
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf7, -48, a3);                            // sqc2 vf7, -48(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, -32, a3);                            // sqc2 vf8, -32(a3)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L56
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

void link() {
  gLinkedFunctionTable.reg("clip-polygon-against-positive-hyperplane", execute, 1024);
}

} // namespace clip_polygon_against_positive_hyperplane
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
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
  bc = !cop1_bc;                                    // bc1f L50
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
  bc = !cop1_bc;                                    // bc1f L48
  c->lqc2(vf6, -16, a2);                            // lqc2 vf6, -16(a2)
  if (bc) {goto block_6;}                           // branch non-likely

  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L54
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
  bc = !cop1_bc;                                    // bc1f L49
  c->lqc2(vf3, -16, a2);                            // lqc2 vf3, -16(a2)
  if (bc) {goto block_8;}                           // branch non-likely

  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L46
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  //beq r0, r0, L54                                 // beq r0, r0, L54
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
  c->daddiu(t6, s7, 8);                             // daddiu t6, s7, 8
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
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L51
  c->sqc2(vf9, -16, a3);                            // sqc2 vf9, -16(a3)
  if (bc) {goto block_11;}                          // branch non-likely

  //beq r0, r0, L54                                 // beq r0, r0, L54
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
  c->daddiu(t6, s7, 8);                             // daddiu t6, s7, 8
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
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L54
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
  bc = cop1_bc;                                     // bc1t L52
  c->lqc2(vf6, -16, a2);                            // lqc2 vf6, -16(a2)
  if (bc) {goto block_14;}                          // branch non-likely

  c->sqc2(vf1, 0, a3);                              // sqc2 vf1, 0(a3)
  c->daddiu(a3, a3, 48);                            // daddiu a3, a3, 48
  c->sqc2(vf2, -32, a3);                            // sqc2 vf2, -32(a3)
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L54
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
  bc = cop1_bc;                                     // bc1t L53
  c->lqc2(vf3, -16, a2);                            // lqc2 vf3, -16(a2)
  if (bc) {goto block_16;}                          // branch non-likely

  c->sqc2(vf4, 0, a3);                              // sqc2 vf4, 0(a3)
  c->daddiu(a3, a3, 48);                            // daddiu a3, a3, 48
  c->sqc2(vf5, -32, a3);                            // sqc2 vf5, -32(a3)
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L50
  c->sqc2(vf6, -16, a3);                            // sqc2 vf6, -16(a3)
  if (bc) {goto block_9;}                           // branch non-likely

  //beq r0, r0, L54                                 // beq r0, r0, L54
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
  c->daddiu(t6, s7, 8);                             // daddiu t6, s7, 8
  c->vmula_bc(DEST::xyzw, BC::w, vf6, vf0);         // vmulaw.xyzw acc, vf6, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf9, vf10);   // vmaddx.xyzw vf9, vf9, vf10
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf7, -48, a3);                            // sqc2 vf7, -48(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, -32, a3);                            // sqc2 vf8, -32(a3)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L47
  c->sqc2(vf9, -16, a3);                            // sqc2 vf9, -16(a3)
  if (bc) {goto block_3;}                           // branch non-likely

  //beq r0, r0, L54                                 // beq r0, r0, L54
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
  c->daddiu(t6, s7, 8);                             // daddiu t6, s7, 8
  c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf9, vf10);   // vmaddx.xyzw vf9, vf9, vf10
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf7, -48, a3);                            // sqc2 vf7, -48(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, -32, a3);                            // sqc2 vf8, -32(a3)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L46
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

void link() {
  gLinkedFunctionTable.reg("clip-polygon-against-negative-hyperplane", execute, 1024);
}

} // namespace clip_polygon_against_negative_hyperplane
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace draw_large_polygon {
struct Cache {
  void* clip_polygon_against_negative_hyperplane; // clip-polygon-against-negative-hyperplane
  void* clip_polygon_against_positive_hyperplane; // clip-polygon-against-positive-hyperplane
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  [[maybe_unused]] u32 call_addr = 0;
  // nop                                            // sll r0, r0, 0
  c->daddiu(sp, sp, -8);                            // daddiu sp, sp, -8
  // nop                                            // sll r0, r0, 0
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->load_symbol(t9, cache.clip_polygon_against_positive_hyperplane);// lw t9, clip-polygon-against-positive-hyperplane(s7)
  c->mov64(a2, t4);                                 // or a2, t4, r0
  c->mov64(a3, t5);                                 // or a3, t5, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->daddu(t2, a2, r0);                             // daddu t2, a2, r0
  //c->jalr(call_addr);                               // jalr ra, t9
  clip_polygon_against_positive_hyperplane::execute(ctxt);
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L67
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  c->mov64(a2, t5);                                 // or a2, t5, r0
  c->mov64(a3, t4);                                 // or a3, t4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->daddiu(t2, a2, 4);                             // daddiu t2, a2, 4
  //c->jalr(call_addr);                               // jalr ra, t9
  clip_polygon_against_positive_hyperplane::execute(ctxt);
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L67
  c->load_symbol(t9, cache.clip_polygon_against_negative_hyperplane);// lw t9, clip-polygon-against-negative-hyperplane(s7)
  if (bc) {goto block_7;}                           // branch non-likely

  c->mov64(a2, t4);                                 // or a2, t4, r0
  c->mov64(a3, t5);                                 // or a3, t5, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->daddu(t2, a2, r0);                             // daddu t2, a2, r0
  //c->jalr(call_addr);                               // jalr ra, t9
  clip_polygon_against_negative_hyperplane::execute(ctxt);
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L67
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  c->mov64(a2, t5);                                 // or a2, t5, r0
  c->mov64(a3, t4);                                 // or a3, t4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->daddiu(t2, a2, 4);                             // daddiu t2, a2, 4
  //c->jalr(call_addr);                               // jalr ra, t9
  clip_polygon_against_negative_hyperplane::execute(ctxt);
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L67
  c->lw(a3, 4, a1);                                 // lw a3, 4(a1)
  if (bc) {goto block_7;}                           // branch non-likely

  c->mov64(a2, t4);                                 // or a2, t4, r0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf27, 0, a3);                             // sqc2 vf27, 0(a3)
  c->daddiu(a3, a3, 16);                            // daddiu a3, a3, 16
  c->sw(t0, -16, a3);                               // sw t0, -16(a3)
  // nop                                            // sll r0, r0, 0

  //fmt::print("draw loop of {}\n", c->gpr_src(t0).du64[0]);
  block_5:
  c->lqc2(vf1, 0, a2);                              // lqc2 vf1, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 16, a2);                             // lqc2 vf2, 16(a2)
  //fmt::print("st orig: {}\n", c->print_vf_float(vf2));
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 32, a2);                             // lqc2 vf3, 32(a2)
  c->vdiv(vf0, BC::w, vf1, BC::w);                  // vdiv Q, vf0.w, vf1.w
  c->vmul(DEST::xyzw, vf1, vf1, vf26);              // vmul.xyzw vf1, vf1, vf26
  // nop                                            // sll r0, r0, 0

  // these look like rgba's
  c->vftoi0(DEST::xyzw, vf3, vf3);                  // vftoi0.xyzw vf3, vf3

  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyzw, vf2, vf2, vf24);              // vadd.xyzw vf2, vf2, vf24
  //fmt::print("st offset: {}\n", c->print_vf_float(vf2));
  // nop                                            // sll r0, r0, 0
  c->vwaitq();                                      // vwaitq
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf1, vf1);                    // vmulq.xyz vf1, vf1, Q

  // store rgbaq
  c->sqc2(vf3, 16, a3);                             // sqc2 vf3, 16(a3)
  c->vmulq(DEST::xyzw, vf2, vf2);                   // vmulq.xyzw vf2, vf2, Q
  //fmt::print("st multiplied: {}\n", c->print_vf_float(vf2));
  c->daddiu(a2, a2, 48);                            // daddiu a2, a2, 48
  c->vadd(DEST::xyzw, vf1, vf1, vf25);              // vadd.xyzw vf1, vf1, vf25
  c->daddiu(a3, a3, 48);                            // daddiu a3, a3, 48
  c->vmul_bc(DEST::z, BC::z, vf1, vf1, vf0);        // vmulz.z vf1, vf1, vf0
  // nop                                            // sll r0, r0, 0
  c->vmini_bc(DEST::w, BC::z, vf1, vf1, vf13);      // vminiz.w vf1, vf1, vf13
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::z, BC::x, vf1, vf1, vf23);       // vaddx.z vf1, vf1, vf23
  // nop                                            // sll r0, r0, 0
  c->vmax_bc(DEST::w, BC::y, vf1, vf1, vf13);       // vmaxy.w vf1, vf1, vf13
  // nop                                            // sll r0, r0, 0
  // store st
  c->sqc2(vf2, -48, a3);                            // sqc2 vf2, -48(a3)
  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  //fmt::print("pos: {}\n", c->print_vf_float(vf1));
  c->vftoi4(DEST::xyzw, vf1, vf1);                  // vftoi4.xyzw vf1, vf1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t0) != 0;                          // bne t0, r0, L66
  // store pos
  c->sqc2(vf1, -16, a3);                            // sqc2 vf1, -16(a3)
  if (bc) {goto block_5;}                           // branch non-likely

  c->sw(a3, 4, a1);                                 // sw a3, 4(a1)
  // nop                                            // sll r0, r0, 0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->daddiu(v0, s7, 8);                             // daddiu v0, s7, 8
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 8);                             // daddiu sp, sp, 8
  goto end_of_function;                             // return


  block_7:
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->mov64(v0, s7);                                 // or v0, s7, r0
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 8);                             // daddiu sp, sp, 8
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
  cache.clip_polygon_against_negative_hyperplane = intern_from_c("clip-polygon-against-negative-hyperplane").c();
  cache.clip_polygon_against_positive_hyperplane = intern_from_c("clip-polygon-against-positive-hyperplane").c();
  gLinkedFunctionTable.reg("draw-large-polygon", execute, 1024);
}

} // namespace draw_large_polygon
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace render_sky_quad {
struct Cache {
  void* math_camera; // *math-camera*
  void* draw_large_polygon; // draw-large-polygon
  void* fake_scratchpad_data;  // *fake-scratchpad-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  c->copy_vfs_from_other(&sky_regs_vfs);
  c->mov64(v1, a0);                                 // or v1, a0, r0
  c->load_symbol(v1, cache.math_camera);            // lw v1, *math-camera*(s7)
  c->lqc2(vf14, 700, v1);                           // lqc2 vf14, 700(v1)
  //c->lui(t4, 28672);                                // lui t4, 28672
  //c->ori(t4, t4, 12288);                            // ori t4, t4, 12288
  get_fake_spad_addr(t4, cache.fake_scratchpad_data, 12288, c);
  //c->lui(t5, 28672);                                // lui t5, 28672
  //c->ori(t5, t5, 14336);                            // ori t5, t5, 14336
  get_fake_spad_addr(t5, cache.fake_scratchpad_data, 14336, c);
  c->lui(v1, 12288);                                // lui v1, 12288
  c->mov64(a3, t4);                                 // or a3, t4, r0
  //c->or_(a0, a0, v1);                               // or a0, a0, v1
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf1, 0, a0);                              // lqc2 vf1, 0(a0)
  c->addiu(t0, r0, 4);                              // addiu t0, r0, 4
  c->lqc2(vf2, 16, a0);                             // lqc2 vf2, 16(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 32, a0);                             // lqc2 vf3, 32(a0)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf1);        // vmulax.xyzw acc, vf31, vf1
  c->lqc2(vf4, 48, a0);                             // lqc2 vf4, 48(a0)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf1);       // vmadday.xyzw acc, vf30, vf1
  c->lqc2(vf5, 64, a0);                             // lqc2 vf5, 64(a0)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf1);       // vmaddaz.xyzw acc, vf29, vf1
  c->lqc2(vf6, 80, a0);                             // lqc2 vf6, 80(a0)
  c->vmadd_bc(DEST::xyw, BC::w, vf1, vf28, vf1);    // vmaddw.xyw vf1, vf28, vf1
  c->lqc2(vf7, 96, a0);                             // lqc2 vf7, 96(a0)
  c->vmove(DEST::z, vf1, vf0);                      // vmove.z vf1, vf0
  c->lqc2(vf8, 112, a0);                            // lqc2 vf8, 112(a0)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf4);        // vmulax.xyzw acc, vf31, vf4
  c->lqc2(vf9, 128, a0);                            // lqc2 vf9, 128(a0)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf4);       // vmadday.xyzw acc, vf30, vf4
  c->lqc2(vf10, 144, a0);                           // lqc2 vf10, 144(a0)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf4);       // vmaddaz.xyzw acc, vf29, vf4
  c->lqc2(vf11, 160, a0);                           // lqc2 vf11, 160(a0)
  c->vmadd_bc(DEST::xyw, BC::w, vf4, vf28, vf4);    // vmaddw.xyw vf4, vf28, vf4
  c->lqc2(vf12, 176, a0);                           // lqc2 vf12, 176(a0)
  c->vmove(DEST::z, vf4, vf0);                      // vmove.z vf4, vf0
  c->sqc2(vf2, 16, a3);                             // sqc2 vf2, 16(a3)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf7);        // vmulax.xyzw acc, vf31, vf7
  c->sqc2(vf3, 32, a3);                             // sqc2 vf3, 32(a3)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf7);       // vmadday.xyzw acc, vf30, vf7
  c->sqc2(vf5, 64, a3);                             // sqc2 vf5, 64(a3)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf7);       // vmaddaz.xyzw acc, vf29, vf7
  c->sqc2(vf6, 80, a3);                             // sqc2 vf6, 80(a3)
  c->vmadd_bc(DEST::xyw, BC::w, vf7, vf28, vf7);    // vmaddw.xyw vf7, vf28, vf7
  c->sqc2(vf8, 112, a3);                            // sqc2 vf8, 112(a3)
  c->vmove(DEST::z, vf7, vf0);                      // vmove.z vf7, vf0
  c->sqc2(vf9, 128, a3);                            // sqc2 vf9, 128(a3)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf10);       // vmulax.xyzw acc, vf31, vf10
  c->sqc2(vf11, 160, a3);                           // sqc2 vf11, 160(a3)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf10);      // vmadday.xyzw acc, vf30, vf10
  c->sqc2(vf12, 176, a3);                           // sqc2 vf12, 176(a3)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf10);      // vmaddaz.xyzw acc, vf29, vf10
  c->sqc2(vf2, 208, a3);                            // sqc2 vf2, 208(a3)
  c->vmadd_bc(DEST::xyw, BC::w, vf10, vf28, vf10);  // vmaddw.xyw vf10, vf28, vf10
  c->sqc2(vf3, 224, a3);                            // sqc2 vf3, 224(a3)
  c->vmove(DEST::z, vf10, vf0);                     // vmove.z vf10, vf0
  c->sqc2(vf1, 0, a3);                              // sqc2 vf1, 0(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf1, 192, a3);                            // sqc2 vf1, 192(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf4, 48, a3);                             // sqc2 vf4, 48(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf7, 96, a3);                             // sqc2 vf7, 96(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf10, 144, a3);                           // sqc2 vf10, 144(a3)
  c->load_symbol(t9, cache.draw_large_polygon);     // lw t9, draw-large-polygon(s7)
  draw_large_polygon::execute(ctxt);
  // Unknown instr: jr t9
  // nop                                            // sll r0, r0, 0
  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.math_camera = intern_from_c("*math-camera*").c();
  cache.draw_large_polygon = intern_from_c("draw-large-polygon").c();
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("render-sky-quad", execute, 1024);
}

} // namespace render_sky_quad
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace render_sky_tri {
struct Cache {
  void* draw_large_polygon; // draw-large-polygon
  void* fake_scratchpad_data;
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  c->copy_vfs_from_other(&sky_regs_vfs);
  c->mov64(v1, a0);                                 // or v1, a0, r0
  //c->lui(t4, 28672);                                // lui t4, 28672
  //c->ori(t4, t4, 12288);                            // ori t4, t4, 12288
  get_fake_spad_addr(t4, cache.fake_scratchpad_data, 12288, c);
  //c->lui(t5, 28672);                                // lui t5, 28672
  //c->ori(t5, t5, 14336);                            // ori t5, t5, 14336
  get_fake_spad_addr(t5, cache.fake_scratchpad_data, 14336, c);
  c->lui(v1, 12288);                                // lui v1, 12288
  c->mov64(a3, t4);                                 // or a3, t4, r0
  //c->_or(a0, a0, v1);                               // or a0, a0, v1
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf1, 0, a0);                              // lqc2 vf1, 0(a0)
  c->addiu(t0, r0, 3);                              // addiu t0, r0, 3
  c->lqc2(vf2, 16, a0);                             // lqc2 vf2, 16(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 32, a0);                             // lqc2 vf3, 32(a0)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf1);        // vmulax.xyzw acc, vf31, vf1
  c->lqc2(vf4, 48, a0);                             // lqc2 vf4, 48(a0)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf1);       // vmadday.xyzw acc, vf30, vf1
  c->lqc2(vf5, 64, a0);                             // lqc2 vf5, 64(a0)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf1);       // vmaddaz.xyzw acc, vf29, vf1
  c->lqc2(vf6, 80, a0);                             // lqc2 vf6, 80(a0)
  c->vmadd_bc(DEST::xyw, BC::w, vf1, vf28, vf1);    // vmaddw.xyw vf1, vf28, vf1
  c->lqc2(vf7, 96, a0);                             // lqc2 vf7, 96(a0)
  c->vmove(DEST::z, vf1, vf0);                      // vmove.z vf1, vf0
  c->lqc2(vf8, 112, a0);                            // lqc2 vf8, 112(a0)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf4);        // vmulax.xyzw acc, vf31, vf4
  c->lqc2(vf9, 128, a0);                            // lqc2 vf9, 128(a0)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf4);       // vmadday.xyzw acc, vf30, vf4
  c->sqc2(vf2, 16, a3);                             // sqc2 vf2, 16(a3)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf4);       // vmaddaz.xyzw acc, vf29, vf4
  c->sqc2(vf3, 32, a3);                             // sqc2 vf3, 32(a3)
  c->vmadd_bc(DEST::xyw, BC::w, vf4, vf28, vf4);    // vmaddw.xyw vf4, vf28, vf4
  c->sqc2(vf5, 64, a3);                             // sqc2 vf5, 64(a3)
  c->vmove(DEST::z, vf4, vf0);                      // vmove.z vf4, vf0
  c->sqc2(vf6, 80, a3);                             // sqc2 vf6, 80(a3)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf7);        // vmulax.xyzw acc, vf31, vf7
  c->sqc2(vf8, 112, a3);                            // sqc2 vf8, 112(a3)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf7);       // vmadday.xyzw acc, vf30, vf7
  c->sqc2(vf9, 128, a3);                            // sqc2 vf9, 128(a3)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf7);       // vmaddaz.xyzw acc, vf29, vf7
  c->sqc2(vf2, 160, a3);                            // sqc2 vf2, 160(a3)
  c->vmadd_bc(DEST::xyw, BC::w, vf7, vf28, vf7);    // vmaddw.xyw vf7, vf28, vf7
  c->sqc2(vf3, 176, a3);                            // sqc2 vf3, 176(a3)
  c->vmove(DEST::z, vf7, vf0);                      // vmove.z vf7, vf0
  c->sqc2(vf1, 0, a3);                              // sqc2 vf1, 0(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf1, 144, a3);                            // sqc2 vf1, 144(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf4, 48, a3);                             // sqc2 vf4, 48(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf7, 96, a3);                             // sqc2 vf7, 96(a3)
  c->load_symbol(t9, cache.draw_large_polygon);     // lw t9, draw-large-polygon(s7)
  draw_large_polygon::execute(ctxt);
  // Unknown instr: jr t9
  // nop                                            // sll r0, r0, 0
  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.draw_large_polygon = intern_from_c("draw-large-polygon").c();
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("render-sky-tri", execute, 1024);
}

} // namespace render_sky_tri
} // namespace Mips2C

namespace Mips2C::jak1 {
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
}  // namespace Mips2C

namespace Mips2C::jak1 {
namespace set_sky_vf23_value {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  // sky_regs_vfs.vfs[27]
  u64 value = c->sgpr64(a0);
  memcpy(&sky_regs_vfs.vfs[23].f[0], &value, 8);
  return 0;
}

void link() {
  gLinkedFunctionTable.reg("set-sky-vf23-value", execute, 64);
}

}  // namespace set_sky_vf23_value
}  // namespace Mips2C

namespace Mips2C::jak1 {
namespace init_boundary_regs {
struct Cache {
  void* math_camera; // *math-camera*
  void* sky_tng_data; // *sky-tng-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  c->load_symbol(v1, cache.math_camera);            // lw v1, *math-camera*(s7)
  c->load_symbol(a0, cache.sky_tng_data);           // lw a0, *sky-tng-data*(s7)
  c->daddiu(a0, a0, 60);                            // daddiu a0, a0, 60
  c->lwc1(f0, 828, v1);                             // lwc1 f0, 828(v1)
  c->swc1(f0, 0, a0);                               // swc1 f0, 0(a0)
  c->lwc1(f0, 128, v1);                             // lwc1 f0, 128(v1)
  c->swc1(f0, 4, a0);                               // swc1 f0, 4(a0)
  c->lwc1(f0, 124, v1);                             // lwc1 f0, 124(v1)
  c->swc1(f0, 8, a0);                               // swc1 f0, 8(a0)
  c->fprs[f0] = 3071.0;                             // lwc1 f0, L532(fp)
  c->swc1(f0, 12, a0);                              // swc1 f0, 12(a0)
  c->lqc2(vf31, 572, v1);                           // lqc2 vf31, 572(v1)
  c->lqc2(vf30, 588, v1);                           // lqc2 vf30, 588(v1)
  c->lqc2(vf29, 604, v1);                           // lqc2 vf29, 604(v1)
  c->lqc2(vf28, 620, v1);                           // lqc2 vf28, 620(v1)
  c->lqc2(vf14, 700, v1);                           // lqc2 vf14, 700(v1)
  c->lqc2(vf26, 716, v1);                           // lqc2 vf26, 716(v1)
  c->lqc2(vf25, 732, v1);                           // lqc2 vf25, 732(v1)
  c->load_symbol(v1, cache.sky_tng_data);           // lw v1, *sky-tng-data*(s7)
  c->lqc2(vf13, 60, v1);                            // lqc2 vf13, 60(v1)
  c->vmul(DEST::xyzw, vf31, vf31, vf14);            // vmul.xyzw vf31, vf31, vf14
  c->vmul(DEST::xyzw, vf30, vf30, vf14);            // vmul.xyzw vf30, vf30, vf14
  c->vmul(DEST::xyzw, vf29, vf29, vf14);            // vmul.xyzw vf29, vf29, vf14
  c->vmul(DEST::xyzw, vf28, vf28, vf14);            // vmul.xyzw vf28, vf28, vf14
  c->vmove(DEST::xyzw, vf24, vf0);                  // vmove.xyzw vf24, vf0
  c->mov128_gpr_vf(v1, vf24);                       // qmfc2.i v1, vf24
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  sky_regs_vfs.copy_vfs_from_other(c);
  return c->gprs[v0].du64[0];
}

void link() {
  cache.math_camera = intern_from_c("*math-camera*").c();
  cache.sky_tng_data = intern_from_c("*sky-tng-data*").c();
  gLinkedFunctionTable.reg("init-boundary-regs", execute, 32);
}

} // namespace init_boundary_regs
} // namespace Mips2C


namespace Mips2C::jak1 {
namespace draw_boundary_polygon {
struct Cache {
  void* clip_polygon_against_negative_hyperplane; // clip-polygon-against-negative-hyperplane
  void* clip_polygon_against_positive_hyperplane; // clip-polygon-against-positive-hyperplane
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  [[maybe_unused]] u32 call_addr = 0;
  // nop                                            // sll r0, r0, 0
  c->daddiu(sp, sp, -8);                            // daddiu sp, sp, -8
  // nop                                            // sll r0, r0, 0
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->load_symbol(t9, cache.clip_polygon_against_positive_hyperplane);// lw t9, clip-polygon-against-positive-hyperplane(s7)
  c->mov64(a2, t4);                                 // or a2, t4, r0
  c->mov64(a3, t5);                                 // or a3, t5, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->daddu(t2, a2, r0);                             // daddu t2, a2, r0
  //c->jalr(call_addr);                               // jalr ra, t9
  clip_polygon_against_positive_hyperplane::execute(ctxt);
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L480
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  c->mov64(a2, t5);                                 // or a2, t5, r0
  c->mov64(a3, t4);                                 // or a3, t4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->daddiu(t2, a2, 4);                             // daddiu t2, a2, 4
  //c->jalr(call_addr);                               // jalr ra, t9
  clip_polygon_against_positive_hyperplane::execute(ctxt);
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L480
  c->load_symbol(t9, cache.clip_polygon_against_negative_hyperplane);// lw t9, clip-polygon-against-negative-hyperplane(s7)
  if (bc) {goto block_7;}                           // branch non-likely

  c->mov64(a2, t4);                                 // or a2, t4, r0
  c->mov64(a3, t5);                                 // or a3, t5, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->daddu(t2, a2, r0);                             // daddu t2, a2, r0
  //c->jalr(call_addr);                               // jalr ra, t9
  clip_polygon_against_negative_hyperplane::execute(ctxt);
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L480
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  c->mov64(a2, t5);                                 // or a2, t5, r0
  c->mov64(a3, t4);                                 // or a3, t4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->daddiu(t2, a2, 4);                             // daddiu t2, a2, 4
  //c->jalr(call_addr);                               // jalr ra, t9
  clip_polygon_against_negative_hyperplane::execute(ctxt);
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L480
  c->lw(a3, 4, a1);                                 // lw a3, 4(a1)
  if (bc) {goto block_7;}                           // branch non-likely

  c->mov64(a2, t4);                                 // or a2, t4, r0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf27, 0, a3);                             // sqc2 vf27, 0(a3)
  c->daddiu(a3, a3, 16);                            // daddiu a3, a3, 16
  c->sw(t0, -16, a3);                               // sw t0, -16(a3)
  // nop                                            // sll r0, r0, 0

  block_5:
  c->lqc2(vf1, 0, a2);                              // lqc2 vf1, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 16, a2);                             // lqc2 vf2, 16(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 32, a2);                             // lqc2 vf3, 32(a2)
  c->vdiv(vf0, BC::w, vf1, BC::w);                  // vdiv Q, vf0.w, vf1.w
  c->vmul(DEST::xyzw, vf1, vf1, vf26);              // vmul.xyzw vf1, vf1, vf26
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf3, vf3);                  // vftoi0.xyzw vf3, vf3
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyzw, vf2, vf2, vf24);              // vadd.xyzw vf2, vf2, vf24
  // nop                                            // sll r0, r0, 0
  c->vwaitq();                                      // vwaitq
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf1, vf1);                    // vmulq.xyz vf1, vf1, Q
  c->sqc2(vf3, 16, a3);                             // sqc2 vf3, 16(a3)
  c->vmulq(DEST::xyzw, vf2, vf2);                   // vmulq.xyzw vf2, vf2, Q
  c->daddiu(a2, a2, 48);                            // daddiu a2, a2, 48
  c->vadd(DEST::xyzw, vf1, vf1, vf25);              // vadd.xyzw vf1, vf1, vf25
  c->daddiu(a3, a3, 48);                            // daddiu a3, a3, 48
  c->vmax_bc(DEST::z, BC::z, vf1, vf1, vf0);        // vmaxz.z vf1, vf1, vf0
  // nop                                            // sll r0, r0, 0
  c->vmini_bc(DEST::w, BC::z, vf1, vf1, vf13);      // vminiz.w vf1, vf1, vf13
  // nop                                            // sll r0, r0, 0
  c->vmax_bc(DEST::w, BC::y, vf1, vf1, vf13);       // vmaxy.w vf1, vf1, vf13
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf2, -48, a3);                            // sqc2 vf2, -48(a3)
  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  c->vftoi4(DEST::xyzw, vf1, vf1);                  // vftoi4.xyzw vf1, vf1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t0) != 0;                          // bne t0, r0, L479
  c->sqc2(vf1, -16, a3);                            // sqc2 vf1, -16(a3)
  if (bc) {goto block_5;}                           // branch non-likely

  c->sw(a3, 4, a1);                                 // sw a3, 4(a1)
  // nop                                            // sll r0, r0, 0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->daddiu(v0, s7, 8);                             // daddiu v0, s7, 8
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 8);                             // daddiu sp, sp, 8
  goto end_of_function;                             // return


  block_7:
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->mov64(v0, s7);                                 // or v0, s7, r0
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 8);                             // daddiu sp, sp, 8
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
  cache.clip_polygon_against_negative_hyperplane = intern_from_c("clip-polygon-against-negative-hyperplane").c();
  cache.clip_polygon_against_positive_hyperplane = intern_from_c("clip-polygon-against-positive-hyperplane").c();
  gLinkedFunctionTable.reg("draw-boundary-polygon", execute, 32);
}

} // namespace draw_boundary_polygon
} // namespace Mips2C


namespace Mips2C::jak1 {
namespace render_boundary_quad {
struct Cache {
  void* math_camera; // *math-camera*
  void* draw_boundary_polygon; // draw-boundary-polygon
  void* fake_scratchpad_data;
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->copy_vfs_from_other(&sky_regs_vfs);
  c->mov64(v1, a0);                                 // or v1, a0, r0
  c->load_symbol(v1, cache.math_camera);            // lw v1, *math-camera*(s7)
  c->lqc2(vf14, 700, v1);                           // lqc2 vf14, 700(v1)
  //c->lui(t4, 28672);                                // lui t4, 28672
  //c->ori(t4, t4, 12288);                            // ori t4, t4, 12288
  get_fake_spad_addr(t4, cache.fake_scratchpad_data, 12288, c);
  //c->lui(t5, 28672);                                // lui t5, 28672
  //c->ori(t5, t5, 14336);                            // ori t5, t5, 14336
  get_fake_spad_addr(t5, cache.fake_scratchpad_data, 14336, c);
  c->mov64(a3, t4);                                 // or a3, t4, r0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf1, 0, a0);                              // lqc2 vf1, 0(a0)
  c->addiu(t0, r0, 4);                              // addiu t0, r0, 4
  c->lqc2(vf2, 16, a0);                             // lqc2 vf2, 16(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 32, a0);                             // lqc2 vf3, 32(a0)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf1);        // vmulax.xyzw acc, vf31, vf1
  c->lqc2(vf4, 48, a0);                             // lqc2 vf4, 48(a0)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf1);       // vmadday.xyzw acc, vf30, vf1
  c->lqc2(vf5, 64, a0);                             // lqc2 vf5, 64(a0)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf1);       // vmaddaz.xyzw acc, vf29, vf1
  c->lqc2(vf6, 80, a0);                             // lqc2 vf6, 80(a0)
  c->vmadd_bc(DEST::xyzw, BC::w, vf1, vf28, vf1);   // vmaddw.xyzw vf1, vf28, vf1
  c->lqc2(vf7, 96, a0);                             // lqc2 vf7, 96(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf8, 112, a0);                            // lqc2 vf8, 112(a0)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf4);        // vmulax.xyzw acc, vf31, vf4
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf4);       // vmadday.xyzw acc, vf30, vf4
  c->lqc2(vf10, 144, a0);                           // lqc2 vf10, 144(a0)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf4);       // vmaddaz.xyzw acc, vf29, vf4
  c->lqc2(vf11, 160, a0);                           // lqc2 vf11, 160(a0)
  c->vmadd_bc(DEST::xyzw, BC::w, vf4, vf28, vf4);   // vmaddw.xyzw vf4, vf28, vf4
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf2, 16, a3);                             // sqc2 vf2, 16(a3)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf7);        // vmulax.xyzw acc, vf31, vf7
  c->sqc2(vf3, 32, a3);                             // sqc2 vf3, 32(a3)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf7);       // vmadday.xyzw acc, vf30, vf7
  c->sqc2(vf5, 64, a3);                             // sqc2 vf5, 64(a3)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf7);       // vmaddaz.xyzw acc, vf29, vf7
  c->sqc2(vf3, 80, a3);                             // sqc2 vf3, 80(a3)
  c->vmadd_bc(DEST::xyzw, BC::w, vf7, vf28, vf7);   // vmaddw.xyzw vf7, vf28, vf7
  c->sqc2(vf8, 112, a3);                            // sqc2 vf8, 112(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf3, 128, a3);                            // sqc2 vf3, 128(a3)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf10);       // vmulax.xyzw acc, vf31, vf10
  c->sqc2(vf11, 160, a3);                           // sqc2 vf11, 160(a3)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf10);      // vmadday.xyzw acc, vf30, vf10
  c->sqc2(vf3, 176, a3);                            // sqc2 vf3, 176(a3)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf10);      // vmaddaz.xyzw acc, vf29, vf10
  c->sqc2(vf2, 208, a3);                            // sqc2 vf2, 208(a3)
  c->vmadd_bc(DEST::xyzw, BC::w, vf10, vf28, vf10); // vmaddw.xyzw vf10, vf28, vf10
  c->sqc2(vf3, 224, a3);                            // sqc2 vf3, 224(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf1, 0, a3);                              // sqc2 vf1, 0(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf1, 192, a3);                            // sqc2 vf1, 192(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf4, 48, a3);                             // sqc2 vf4, 48(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf7, 96, a3);                             // sqc2 vf7, 96(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf10, 144, a3);                           // sqc2 vf10, 144(a3)
  c->load_symbol(t9, cache.draw_boundary_polygon);  // lw t9, draw-boundary-polygon(s7)
  c->vsub(DEST::xyzw, vf4, vf4, vf1);               // vsub.xyzw vf4, vf4, vf1
  // nop                                            // sll r0, r0, 0
  c->vsub(DEST::xyzw, vf7, vf7, vf1);               // vsub.xyzw vf7, vf7, vf1
  // nop                                            // sll r0, r0, 0
  c->vopmula(vf4, vf7);                             // vopmula.xyz acc, vf4, vf7
  // nop                                            // sll r0, r0, 0
  c->vopmsub(vf10, vf7, vf4);                   // vopmsub.xyz vf10, vf7, vf4
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf10, vf10, vf1);             // vmul.xyzw vf10, vf10, vf1
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::y, BC::x, vf10, vf10, vf10);     // vaddx.y vf10, vf10, vf10
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::y, BC::z, vf10, vf10, vf10);     // vaddz.y vf10, vf10, vf10
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(v0, vf10);                       // qmfc2.i v0, vf10
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(v0)) >= 0;                   // bgez v0, L477
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  // Unknown instr: jr t9
  draw_boundary_polygon::execute(ctxt);
  goto end_of_function;
  // nop                                            // sll r0, r0, 0

  block_2:
  c->sqc2(vf6, 32, a3);                             // sqc2 vf6, 32(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf6, 80, a3);                             // sqc2 vf6, 80(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf6, 128, a3);                            // sqc2 vf6, 128(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf6, 176, a3);                            // sqc2 vf6, 176(a3)
  // nop                                            // sll r0, r0, 0
  // Unknown instr: jr t9
  draw_boundary_polygon::execute(ctxt);
  // nop                                            // sll r0, r0, 0
  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.math_camera = intern_from_c("*math-camera*").c();
  cache.draw_boundary_polygon = intern_from_c("draw-boundary-polygon").c();
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("render-boundary-quad", execute, 32);
}

} // namespace render_boundary_quad
} // namespace Mips2C

namespace Mips2C::jak1 {
namespace render_boundary_tri {
struct Cache {
  void* draw_boundary_polygon; // draw-boundary-polygon
  void* fake_scratchpad_data;
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->copy_vfs_from_other(&sky_regs_vfs);
  c->mov64(v1, a0);                                 // or v1, a0, r0
//  c->lui(t4, 28672);                                // lui t4, 28672
//  c->ori(t4, t4, 12288);                            // ori t4, t4, 12288
  get_fake_spad_addr(t4, cache.fake_scratchpad_data, 12288, c);
//  c->lui(t5, 28672);                                // lui t5, 28672
//  c->ori(t5, t5, 14336);                            // ori t5, t5, 14336
  get_fake_spad_addr(t5, cache.fake_scratchpad_data, 14336, c);
  c->mov64(a3, t4);                                 // or a3, t4, r0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf1, 0, a0);                              // lqc2 vf1, 0(a0)
  c->addiu(t0, r0, 3);                              // addiu t0, r0, 3
  c->lqc2(vf2, 16, a0);                             // lqc2 vf2, 16(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 32, a0);                             // lqc2 vf3, 32(a0)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf1);        // vmulax.xyzw acc, vf31, vf1
  c->lqc2(vf4, 48, a0);                             // lqc2 vf4, 48(a0)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf1);       // vmadday.xyzw acc, vf30, vf1
  c->lqc2(vf5, 64, a0);                             // lqc2 vf5, 64(a0)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf1);       // vmaddaz.xyzw acc, vf29, vf1
  c->lqc2(vf6, 80, a0);                             // lqc2 vf6, 80(a0)
  c->vmadd_bc(DEST::xyzw, BC::w, vf1, vf28, vf1);   // vmaddw.xyzw vf1, vf28, vf1
  c->lqc2(vf7, 96, a0);                             // lqc2 vf7, 96(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf8, 112, a0);                            // lqc2 vf8, 112(a0)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf4);        // vmulax.xyzw acc, vf31, vf4
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf4);       // vmadday.xyzw acc, vf30, vf4
  c->sqc2(vf2, 16, a3);                             // sqc2 vf2, 16(a3)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf4);       // vmaddaz.xyzw acc, vf29, vf4
  c->sqc2(vf3, 32, a3);                             // sqc2 vf3, 32(a3)
  c->vmadd_bc(DEST::xyzw, BC::w, vf4, vf28, vf4);   // vmaddw.xyzw vf4, vf28, vf4
  c->sqc2(vf5, 64, a3);                             // sqc2 vf5, 64(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf3, 80, a3);                             // sqc2 vf3, 80(a3)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf7);        // vmulax.xyzw acc, vf31, vf7
  c->sqc2(vf8, 112, a3);                            // sqc2 vf8, 112(a3)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf7);       // vmadday.xyzw acc, vf30, vf7
  c->sqc2(vf3, 128, a3);                            // sqc2 vf3, 128(a3)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf7);       // vmaddaz.xyzw acc, vf29, vf7
  c->sqc2(vf2, 160, a3);                            // sqc2 vf2, 160(a3)
  c->vmadd_bc(DEST::xyzw, BC::w, vf7, vf28, vf7);   // vmaddw.xyzw vf7, vf28, vf7
  c->sqc2(vf3, 176, a3);                            // sqc2 vf3, 176(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf1, 0, a3);                              // sqc2 vf1, 0(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf1, 144, a3);                            // sqc2 vf1, 144(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf4, 48, a3);                             // sqc2 vf4, 48(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf7, 96, a3);                             // sqc2 vf7, 96(a3)
  c->load_symbol(t9, cache.draw_boundary_polygon);  // lw t9, draw-boundary-polygon(s7)
  c->vsub(DEST::xyzw, vf4, vf4, vf1);               // vsub.xyzw vf4, vf4, vf1
  // nop                                            // sll r0, r0, 0
  c->vsub(DEST::xyzw, vf7, vf7, vf1);               // vsub.xyzw vf7, vf7, vf1
  // nop                                            // sll r0, r0, 0
  c->vopmula(vf4, vf7);                             // vopmula.xyz acc, vf4, vf7
  // nop                                            // sll r0, r0, 0
  c->vopmsub(vf10, vf7, vf4);                   // vopmsub.xyz vf10, vf7, vf4
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf10, vf10, vf1);             // vmul.xyzw vf10, vf10, vf1
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::y, BC::x, vf10, vf10, vf10);     // vaddx.y vf10, vf10, vf10
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::y, BC::z, vf10, vf10, vf10);     // vaddz.y vf10, vf10, vf10
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(v0, vf10);                       // qmfc2.i v0, vf10
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(v0)) >= 0;                   // bgez v0, L475
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  // Unknown instr: jr t9
  draw_boundary_polygon::execute(ctxt);
  goto end_of_function;
  // nop                                            // sll r0, r0, 0

  block_2:
  c->sqc2(vf6, 32, a3);                             // sqc2 vf6, 32(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf6, 80, a3);                             // sqc2 vf6, 80(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf6, 128, a3);                            // sqc2 vf6, 128(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf6, 176, a3);                            // sqc2 vf6, 176(a3)
  // nop                                            // sll r0, r0, 0
  // Unknown instr: jr t9
  draw_boundary_polygon::execute(ctxt);
  // nop                                            // sll r0, r0, 0
  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.draw_boundary_polygon = intern_from_c("draw-boundary-polygon").c();
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("render-boundary-tri", execute, 32);
}

} // namespace render_boundary_tri
} // namespace Mips2C
