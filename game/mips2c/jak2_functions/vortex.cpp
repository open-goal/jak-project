//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
#include "game/mips2c/jak2_functions/sky.h"

using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace init_vortex_regs {
struct Cache {
  void* math_camera; // *math-camera*
  void* sky_work; // *sky-work*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  c->copy_vfs_from_other(&sky_regs_vfs);
  c->load_symbol2(v1, cache.math_camera);           // lw v1, *math-camera*(s7)
  c->load_symbol2(a0, cache.sky_work);              // lw a0, *sky-work*(s7)
  c->daddiu(a1, a0, 1008);                          // daddiu a1, a0, 1008
  c->lwc1(f0, 908, v1);                             // lwc1 f0, 908(v1)
  c->swc1(f0, 0, a1);                               // swc1 f0, 0(a1)
  c->lwc1(f0, 128, v1);                             // lwc1 f0, 128(v1)
  c->swc1(f0, 4, a1);                               // swc1 f0, 4(a1)
  c->lwc1(f0, 124, v1);                             // lwc1 f0, 124(v1)
  c->swc1(f0, 8, a1);                               // swc1 f0, 8(a1)
  c->lui(a2, 17727);                                // lui a2, 17727
  c->ori(a2, a2, 61440);                            // ori a2, a2, 61440
  c->mtc1(f0, a2);                                  // mtc1 f0, a2
  c->swc1(f0, 12, a1);                              // swc1 f0, 12(a1)
  c->lqc2(vf31, 572, v1);                           // lqc2 vf31, 572(v1)
  c->lqc2(vf30, 588, v1);                           // lqc2 vf30, 588(v1)
  c->lqc2(vf29, 604, v1);                           // lqc2 vf29, 604(v1)
  c->lqc2(vf28, 620, v1);                           // lqc2 vf28, 620(v1)
  c->lqc2(vf26, 796, v1);                           // lqc2 vf26, 796(v1)
  c->lqc2(vf14, 780, v1);                           // lqc2 vf14, 780(v1)
  c->lqc2(vf25, 812, v1);                           // lqc2 vf25, 812(v1)
  c->lqc2(vf13, 1008, a0);                          // lqc2 vf13, 1008(a0)
  c->lqc2(vf27, 992, a0);                           // lqc2 vf27, 992(a0)
  c->vmove(DEST::xyzw, vf24, vf0);                  // vmove.xyzw vf24, vf0
  c->mov128_gpr_vf(v1, vf24);                       // qmfc2.i v1, vf24
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  sky_regs_vfs.copy_vfs_from_other(c);
  return c->gprs[v0].du64[0];
}

void link() {
  cache.math_camera = intern_from_c("*math-camera*").c();
  cache.sky_work = intern_from_c("*sky-work*").c();
  gLinkedFunctionTable.reg("init-vortex-regs", execute, 128);
}

} // namespace init_vortex_regs
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace draw_large_polygon_vortex {
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
  c->mov64(t6, s7);                                 // or t6, s7, r0
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->load_symbol2(t9, cache.clip_polygon_against_positive_hyperplane);// lw t9, clip-polygon-against-positive-hyperplane(s7)
  c->mov64(a2, t4);                                 // or a2, t4, r0
  c->mov64(a3, t5);                                 // or a3, t5, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->daddu(t2, a2, r0);                             // daddu t2, a2, r0
  // c->jalr(call_addr);                               // jalr ra, t9
  clip_polygon_against_positive_hyperplane::execute(ctxt);
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L15
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_11;}                          // branch non-likely

  c->mov64(a2, t5);                                 // or a2, t5, r0
  c->mov64(a3, t4);                                 // or a3, t4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->daddiu(t2, a2, 4);                             // daddiu t2, a2, 4
  // c->jalr(call_addr);                               // jalr ra, t9
  clip_polygon_against_positive_hyperplane::execute(ctxt);
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L15
  c->load_symbol2(t9, cache.clip_polygon_against_negative_hyperplane);// lw t9, clip-polygon-against-negative-hyperplane(s7)
  if (bc) {goto block_11;}                          // branch non-likely

  c->mov64(a2, t4);                                 // or a2, t4, r0
  c->mov64(a3, t5);                                 // or a3, t5, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->daddu(t2, a2, r0);                             // daddu t2, a2, r0
  // c->jalr(call_addr);                               // jalr ra, t9
  clip_polygon_against_negative_hyperplane::execute(ctxt);
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L15
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_11;}                          // branch non-likely

  c->mov64(a2, t5);                                 // or a2, t5, r0
  c->mov64(a3, t4);                                 // or a3, t4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->daddiu(t2, a2, 4);                             // daddiu t2, a2, 4
  // c->jalr(call_addr);                               // jalr ra, t9
  clip_polygon_against_negative_hyperplane::execute(ctxt);
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L15
  c->lw(a3, 4, a1);                                 // lw a3, 4(a1)
  if (bc) {goto block_11;}                          // branch non-likely

  bc = c->sgpr64(t6) == c->sgpr64(s7);              // beq t6, s7, L13
  c->mov64(a2, t4);                                 // or a2, t4, r0
  if (bc) {goto block_8;}                           // branch non-likely

  c->sqc2(vf27, 0, a3);                             // sqc2 vf27, 0(a3)
  c->daddiu(a3, a3, 16);                            // daddiu a3, a3, 16
  c->sw(t0, -16, a3);                               // sw t0, -16(a3)
  // nop                                            // sll r0, r0, 0

  block_6:
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
  bc = c->sgpr64(t0) != 0;                          // bne t0, r0, L12
  c->sqc2(vf1, -16, a3);                            // sqc2 vf1, -16(a3)
  if (bc) {goto block_6;}                           // branch non-likely

  c->sw(a3, 4, a1);                                 // sw a3, 4(a1)
  // nop                                            // sll r0, r0, 0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->daddiu(v0, s7, 4);                             // daddiu v0, s7, 4
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 8);                             // daddiu sp, sp, 8
  goto end_of_function;                             // return


  block_8:
  c->sqc2(vf27, 0, a3);                             // sqc2 vf27, 0(a3)
  c->daddiu(a3, a3, 16);                            // daddiu a3, a3, 16
  c->sw(t0, -16, a3);                               // sw t0, -16(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf15, 0, a2);                             // sqc2 vf15, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf15, 192, a2);                           // sqc2 vf15, 192(a2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf16, 48, a2);                            // sqc2 vf16, 48(a2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf17, 96, a2);                            // sqc2 vf17, 96(a2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf18, 144, a2);                           // sqc2 vf18, 144(a2)
  // nop                                            // sll r0, r0, 0

  block_9:
  c->lqc2(vf1, 0, a2);                              // lqc2 vf1, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 32, a2);                             // lqc2 vf3, 32(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 16, a2);                             // lqc2 vf2, 16(a2)
  c->vdiv(vf13, BC::x, vf1, BC::w);                 // vdiv Q, vf13.x, vf1.w
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
  bc = c->sgpr64(t0) != 0;                          // bne t0, r0, L14
  c->sqc2(vf1, -16, a3);                            // sqc2 vf1, -16(a3)
  if (bc) {goto block_9;}                           // branch non-likely

  c->sw(a3, 4, a1);                                 // sw a3, 4(a1)
  // nop                                            // sll r0, r0, 0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->daddiu(v0, s7, 4);                             // daddiu v0, s7, 4
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 8);                             // daddiu sp, sp, 8
  goto end_of_function;                             // return


  block_11:
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
  gLinkedFunctionTable.reg("draw-large-polygon-vortex", execute, 128);
}

} // namespace draw_large_polygon_vortex
} // namespace Mips2C


//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace render_vortex_quad {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* draw_large_polygon_vortex; // draw-large-polygon-vortex
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  c->copy_vfs_from_other(&sky_regs_vfs);
  c->mov64(v1, a0);                                 // or v1, a0, r0
  get_fake_spad_addr2(t4, cache.fake_scratchpad_data, 0, c);// lui t4, 28672
  c->ori(t4, t4, 12288);                            // ori t4, t4, 12288
  get_fake_spad_addr2(t5, cache.fake_scratchpad_data, 0, c);// lui t5, 28672
  c->ori(t5, t5, 14336);                            // ori t5, t5, 14336
  c->mov64(a3, t4);                                 // or a3, t4, r0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf1, 0, a0);                              // lqc2 vf1, 0(a0)
  c->addiu(t0, r0, 4);                              // addiu t0, r0, 4
  c->lqc2(vf2, 16, a0);                             // lqc2 vf2, 16(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 0, a2);                              // lqc2 vf3, 0(a2)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf1);        // vmulax.xyzw acc, vf31, vf1
  c->lqc2(vf4, 32, a0);                             // lqc2 vf4, 32(a0)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf1);       // vmadday.xyzw acc, vf30, vf1
  c->lqc2(vf5, 48, a0);                             // lqc2 vf5, 48(a0)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf1);       // vmaddaz.xyzw acc, vf29, vf1
  c->lqc2(vf6, 0, a2);                              // lqc2 vf6, 0(a2)
  c->vmadd_bc(DEST::xyzw, BC::w, vf15, vf28, vf1);  // vmaddw.xyzw vf15, vf28, vf1
  c->lqc2(vf7, 64, a0);                             // lqc2 vf7, 64(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf8, 80, a0);                             // lqc2 vf8, 80(a0)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf4);        // vmulax.xyzw acc, vf31, vf4
  c->lqc2(vf9, 0, a2);                              // lqc2 vf9, 0(a2)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf4);       // vmadday.xyzw acc, vf30, vf4
  c->lqc2(vf10, 96, a0);                            // lqc2 vf10, 96(a0)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf4);       // vmaddaz.xyzw acc, vf29, vf4
  c->lqc2(vf11, 112, a0);                           // lqc2 vf11, 112(a0)
  c->vmadd_bc(DEST::xyzw, BC::w, vf16, vf28, vf4);  // vmaddw.xyzw vf16, vf28, vf4
  c->lqc2(vf12, 0, a2);                             // lqc2 vf12, 0(a2)
  c->vmul(DEST::xyzw, vf1, vf15, vf14);             // vmul.xyzw vf1, vf15, vf14
  c->sqc2(vf2, 16, a3);                             // sqc2 vf2, 16(a3)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf7);        // vmulax.xyzw acc, vf31, vf7
  c->sqc2(vf3, 32, a3);                             // sqc2 vf3, 32(a3)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf7);       // vmadday.xyzw acc, vf30, vf7
  c->sqc2(vf5, 64, a3);                             // sqc2 vf5, 64(a3)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf7);       // vmaddaz.xyzw acc, vf29, vf7
  c->sqc2(vf6, 80, a3);                             // sqc2 vf6, 80(a3)
  c->vmadd_bc(DEST::xyzw, BC::w, vf17, vf28, vf7);  // vmaddw.xyzw vf17, vf28, vf7
  c->sqc2(vf8, 112, a3);                            // sqc2 vf8, 112(a3)
  c->vmul(DEST::xyzw, vf4, vf16, vf14);             // vmul.xyzw vf4, vf16, vf14
  c->sqc2(vf9, 128, a3);                            // sqc2 vf9, 128(a3)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf10);       // vmulax.xyzw acc, vf31, vf10
  c->sqc2(vf11, 160, a3);                           // sqc2 vf11, 160(a3)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf10);      // vmadday.xyzw acc, vf30, vf10
  c->sqc2(vf12, 176, a3);                           // sqc2 vf12, 176(a3)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf10);      // vmaddaz.xyzw acc, vf29, vf10
  c->sqc2(vf2, 208, a3);                            // sqc2 vf2, 208(a3)
  c->vmadd_bc(DEST::xyzw, BC::w, vf18, vf28, vf10); // vmaddw.xyzw vf18, vf28, vf10
  c->sqc2(vf3, 224, a3);                            // sqc2 vf3, 224(a3)
  c->vmul(DEST::xyzw, vf7, vf17, vf14);             // vmul.xyzw vf7, vf17, vf14
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf1, 0, a3);                              // sqc2 vf1, 0(a3)
  c->vmul(DEST::xyzw, vf10, vf18, vf14);            // vmul.xyzw vf10, vf18, vf14
  c->sqc2(vf1, 192, a3);                            // sqc2 vf1, 192(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf4, 48, a3);                             // sqc2 vf4, 48(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf7, 96, a3);                             // sqc2 vf7, 96(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf10, 144, a3);                           // sqc2 vf10, 144(a3)
  c->load_symbol2(t9, cache.draw_large_polygon_vortex);// lw t9, draw-large-polygon-vortex(s7)
  // Unknown instr: jr t9
  draw_large_polygon_vortex::execute(ctxt);
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
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  cache.draw_large_polygon_vortex = intern_from_c("draw-large-polygon-vortex").c();
  gLinkedFunctionTable.reg("render-vortex-quad", execute, 128);
}

} // namespace render_vortex_quad
} // namespace Mips2C
