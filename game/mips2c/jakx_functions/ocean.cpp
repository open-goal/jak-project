//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace draw_large_polygon_ocean {
struct Cache {
  void* clip_polygon_against_negative_hyperplane; // clip-polygon-against-negative-hyperplane
  void* clip_polygon_against_positive_hyperplane; // clip-polygon-against-positive-hyperplane
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  // nop                                            // sll r0, r0, 0
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
  c->mov64(t7, s7);                                 // or t7, s7, r0
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->load_symbol2(t9, cache.clip_polygon_against_positive_hyperplane);// lw t9, clip-polygon-against-positive-hyperplane(s7)
  c->mov64(a3, t5);                                 // or a3, t5, r0
  c->mov64(t0, t6);                                 // or t0, t6, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->daddu(t3, a3, r0);                             // daddu t3, a3, r0
  c->jalr(call_addr);                               // jalr ra, t9
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L117
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_11;}                          // branch non-likely

  c->mov64(a3, t6);                                 // or a3, t6, r0
  c->mov64(t0, t5);                                 // or t0, t5, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->daddiu(t3, a3, 4);                             // daddiu t3, a3, 4
  c->jalr(call_addr);                               // jalr ra, t9
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L117
  c->load_symbol2(t9, cache.clip_polygon_against_negative_hyperplane);// lw t9, clip-polygon-against-negative-hyperplane(s7)
  if (bc) {goto block_11;}                          // branch non-likely

  c->mov64(a3, t5);                                 // or a3, t5, r0
  c->mov64(t0, t6);                                 // or t0, t6, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->daddu(t3, a3, r0);                             // daddu t3, a3, r0
  c->jalr(call_addr);                               // jalr ra, t9
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L117
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_11;}                          // branch non-likely

  c->mov64(a3, t6);                                 // or a3, t6, r0
  c->mov64(t0, t5);                                 // or t0, t5, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->daddiu(t3, a3, 4);                             // daddiu t3, a3, 4
  c->jalr(call_addr);                               // jalr ra, t9
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L117
  c->lw(t0, 4, a1);                                 // lw t0, 4(a1)
  if (bc) {goto block_11;}                          // branch non-likely

  bc = c->sgpr64(t7) == c->sgpr64(s7);              // beq t7, s7, L115
  c->mov64(a3, t5);                                 // or a3, t5, r0
  if (bc) {goto block_8;}                           // branch non-likely

  c->sqc2(vf27, 0, t0);                             // sqc2 vf27, 0(t0)
  c->daddiu(t0, t0, 16);                            // daddiu t0, t0, 16
  c->sw(t1, -16, t0);                               // sw t1, -16(t0)
  // nop                                            // sll r0, r0, 0
  
block_6:
  c->lqc2(vf1, 0, a3);                              // lqc2 vf1, 0(a3)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 16, a3);                             // lqc2 vf2, 16(a3)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 32, a3);                             // lqc2 vf3, 32(a3)
  c->vdiv(vf0, BC::w, vf1, BC::w);                  // vdiv Q, vf0.w, vf1.w
  c->vmul(DEST::xyzw, vf1, vf1, vf26);              // vmul.xyzw vf1, vf1, vf26
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf3, vf3);                  // vftoi0.xyzw vf3, vf3
  // nop                                            // sll r0, r0, 0
  c->vwaitq();                                      // vwaitq
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf1, vf1);                    // vmulq.xyz vf1, vf1, Q
  c->sqc2(vf3, 16, t0);                             // sqc2 vf3, 16(t0)
  c->vmulq(DEST::xyzw, vf2, vf2);                   // vmulq.xyzw vf2, vf2, Q
  c->daddiu(a3, a3, 48);                            // daddiu a3, a3, 48
  c->vadd(DEST::xyzw, vf1, vf1, vf25);              // vadd.xyzw vf1, vf1, vf25
  c->daddiu(t0, t0, 48);                            // daddiu t0, t0, 48
  c->vmax_bc(DEST::z, BC::z, vf1, vf1, vf0);        // vmaxz.z vf1, vf1, vf0
  // nop                                            // sll r0, r0, 0
  c->vmini_bc(DEST::w, BC::z, vf1, vf1, vf13);      // vminiz.w vf1, vf1, vf13
  // nop                                            // sll r0, r0, 0
  c->vmax_bc(DEST::w, BC::y, vf1, vf1, vf13);       // vmaxy.w vf1, vf1, vf13
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf2, -48, t0);                            // sqc2 vf2, -48(t0)
  c->daddiu(t1, t1, -1);                            // daddiu t1, t1, -1
  c->vftoi4(DEST::xyzw, vf1, vf1);                  // vftoi4.xyzw vf1, vf1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L114
  c->sqc2(vf1, -16, t0);                            // sqc2 vf1, -16(t0)
  if (bc) {goto block_6;}                           // branch non-likely

  c->sw(t0, 4, a1);                                 // sw t0, 4(a1)
  // nop                                            // sll r0, r0, 0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->daddiu(v0, s7, 4);                             // daddiu v0, s7, 4
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
  goto end_of_function;                             // return

  
block_8:
  c->sqc2(vf27, 0, t0);                             // sqc2 vf27, 0(t0)
  c->daddiu(t0, t0, 16);                            // daddiu t0, t0, 16
  c->sw(t1, -16, t0);                               // sw t1, -16(t0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf15, 0, a3);                             // sqc2 vf15, 0(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf15, 192, a3);                           // sqc2 vf15, 192(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf16, 48, a3);                            // sqc2 vf16, 48(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf17, 96, a3);                            // sqc2 vf17, 96(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf18, 144, a3);                           // sqc2 vf18, 144(a3)
  // nop                                            // sll r0, r0, 0
  
block_9:
  c->lqc2(vf1, 0, a3);                              // lqc2 vf1, 0(a3)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 32, a3);                             // lqc2 vf3, 32(a3)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 16, a3);                             // lqc2 vf2, 16(a3)
  c->vdiv(vf13, BC::x, vf1, BC::w);                 // vdiv Q, vf13.x, vf1.w
  c->vftoi0(DEST::xyzw, vf3, vf3);                  // vftoi0.xyzw vf3, vf3
  // nop                                            // sll r0, r0, 0
  c->vwaitq();                                      // vwaitq
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf1, vf1);                    // vmulq.xyz vf1, vf1, Q
  c->sqc2(vf3, 16, t0);                             // sqc2 vf3, 16(t0)
  c->vmulq(DEST::xyzw, vf2, vf2);                   // vmulq.xyzw vf2, vf2, Q
  c->daddiu(a3, a3, 48);                            // daddiu a3, a3, 48
  c->vadd(DEST::xyzw, vf1, vf1, vf25);              // vadd.xyzw vf1, vf1, vf25
  c->daddiu(t0, t0, 48);                            // daddiu t0, t0, 48
  c->vmax_bc(DEST::z, BC::z, vf1, vf1, vf0);        // vmaxz.z vf1, vf1, vf0
  // nop                                            // sll r0, r0, 0
  c->vmini_bc(DEST::w, BC::z, vf1, vf1, vf13);      // vminiz.w vf1, vf1, vf13
  // nop                                            // sll r0, r0, 0
  c->vmax_bc(DEST::w, BC::y, vf1, vf1, vf13);       // vmaxy.w vf1, vf1, vf13
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf2, -48, t0);                            // sqc2 vf2, -48(t0)
  c->daddiu(t1, t1, -1);                            // daddiu t1, t1, -1
  c->vftoi4(DEST::xyzw, vf1, vf1);                  // vftoi4.xyzw vf1, vf1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L116
  c->sqc2(vf1, -16, t0);                            // sqc2 vf1, -16(t0)
  if (bc) {goto block_9;}                           // branch non-likely

  c->sw(t0, 4, a1);                                 // sw t0, 4(a1)
  // nop                                            // sll r0, r0, 0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->daddiu(v0, s7, 4);                             // daddiu v0, s7, 4
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
  goto end_of_function;                             // return

  
block_11:
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->mov64(v0, s7);                                 // or v0, s7, r0
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
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
  cache.clip_polygon_against_negative_hyperplane = intern_from_c(-1, 0, "clip-polygon-against-negative-hyperplane").c();
  cache.clip_polygon_against_positive_hyperplane = intern_from_c(-1, 0, "clip-polygon-against-positive-hyperplane").c();
  gLinkedFunctionTable.reg("draw-large-polygon-ocean", execute, 256);
}

} // namespace draw_large_polygon_ocean
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace render_ocean_quad {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* draw_large_polygon_ocean; // draw-large-polygon-ocean
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->mov64(v1, a0);                                 // or v1, a0, r0
  get_fake_spad_addr2(t5, cache.fake_scratchpad_data, 0, c);// lui t5, 28672
  c->ori(t5, t5, 12288);                            // ori t5, t5, 12288
  get_fake_spad_addr2(t6, cache.fake_scratchpad_data, 0, c);// lui t6, 28672
  c->ori(t6, t6, 14336);                            // ori t6, t6, 14336
  c->mov64(t0, t5);                                 // or t0, t5, r0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf1, 0, a0);                              // lqc2 vf1, 0(a0)
  c->addiu(t1, r0, 4);                              // addiu t1, r0, 4
  c->lqc2(vf2, 16, a0);                             // lqc2 vf2, 16(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 32, a0);                             // lqc2 vf3, 32(a0)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf1);        // vmulax.xyzw acc, vf31, vf1
  c->lqc2(vf4, 48, a0);                             // lqc2 vf4, 48(a0)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf1);       // vmadday.xyzw acc, vf30, vf1
  c->lqc2(vf5, 64, a0);                             // lqc2 vf5, 64(a0)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf1);       // vmaddaz.xyzw acc, vf29, vf1
  c->lqc2(vf6, 80, a0);                             // lqc2 vf6, 80(a0)
  c->vmadd_bc(DEST::xyzw, BC::w, vf15, vf28, vf1);  // vmaddw.xyzw vf15, vf28, vf1
  c->lqc2(vf7, 96, a0);                             // lqc2 vf7, 96(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf8, 112, a0);                            // lqc2 vf8, 112(a0)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf4);        // vmulax.xyzw acc, vf31, vf4
  c->lqc2(vf9, 128, a0);                            // lqc2 vf9, 128(a0)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf4);       // vmadday.xyzw acc, vf30, vf4
  c->lqc2(vf10, 144, a0);                           // lqc2 vf10, 144(a0)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf4);       // vmaddaz.xyzw acc, vf29, vf4
  c->lqc2(vf11, 160, a0);                           // lqc2 vf11, 160(a0)
  c->vmadd_bc(DEST::xyzw, BC::w, vf16, vf28, vf4);  // vmaddw.xyzw vf16, vf28, vf4
  c->lqc2(vf12, 176, a0);                           // lqc2 vf12, 176(a0)
  c->vmul(DEST::xyzw, vf1, vf15, vf14);             // vmul.xyzw vf1, vf15, vf14
  c->sqc2(vf2, 16, t0);                             // sqc2 vf2, 16(t0)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf7);        // vmulax.xyzw acc, vf31, vf7
  c->sqc2(vf3, 32, t0);                             // sqc2 vf3, 32(t0)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf7);       // vmadday.xyzw acc, vf30, vf7
  c->sqc2(vf5, 64, t0);                             // sqc2 vf5, 64(t0)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf7);       // vmaddaz.xyzw acc, vf29, vf7
  c->sqc2(vf6, 80, t0);                             // sqc2 vf6, 80(t0)
  c->vmadd_bc(DEST::xyzw, BC::w, vf17, vf28, vf7);  // vmaddw.xyzw vf17, vf28, vf7
  c->sqc2(vf8, 112, t0);                            // sqc2 vf8, 112(t0)
  c->vmul(DEST::xyzw, vf4, vf16, vf14);             // vmul.xyzw vf4, vf16, vf14
  c->sqc2(vf9, 128, t0);                            // sqc2 vf9, 128(t0)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf10);       // vmulax.xyzw acc, vf31, vf10
  c->sqc2(vf11, 160, t0);                           // sqc2 vf11, 160(t0)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf10);      // vmadday.xyzw acc, vf30, vf10
  c->sqc2(vf12, 176, t0);                           // sqc2 vf12, 176(t0)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf10);      // vmaddaz.xyzw acc, vf29, vf10
  c->sqc2(vf2, 208, t0);                            // sqc2 vf2, 208(t0)
  c->vmadd_bc(DEST::xyzw, BC::w, vf18, vf28, vf10); // vmaddw.xyzw vf18, vf28, vf10
  c->sqc2(vf3, 224, t0);                            // sqc2 vf3, 224(t0)
  c->vmul(DEST::xyzw, vf7, vf17, vf14);             // vmul.xyzw vf7, vf17, vf14
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf1, 0, t0);                              // sqc2 vf1, 0(t0)
  c->vmul(DEST::xyzw, vf10, vf18, vf14);            // vmul.xyzw vf10, vf18, vf14
  c->sqc2(vf1, 192, t0);                            // sqc2 vf1, 192(t0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf4, 48, t0);                             // sqc2 vf4, 48(t0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf7, 96, t0);                             // sqc2 vf7, 96(t0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf10, 144, t0);                           // sqc2 vf10, 144(t0)
  c->load_symbol2(t9, cache.draw_large_polygon_ocean);// lw t9, draw-large-polygon-ocean(s7)
  // Unknown instr: jr t9
  return draw_large_polygon_ocean::execute(c);
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
  cache.fake_scratchpad_data = intern_from_c(-1, 0, "*fake-scratchpad-data*").c();
  cache.draw_large_polygon_ocean = intern_from_c(-1, 0, "draw-large-polygon-ocean").c();
  gLinkedFunctionTable.reg("render-ocean-quad", execute, 256);
}

} // namespace render_ocean_quad
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace init_ocean_far_regs {
struct Cache {
  void* ocean_map; // *ocean-map*
  void* sky_work; // *sky-work*
  void* time_of_day_context; // *time-of-day-context*
  void* matrix; // matrix*!
  void* vector_matrix; // vector-matrix*!
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -144);                          // daddiu sp, sp, -144
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s4, 96, sp);                                // sq s4, 96(sp)
  c->sq(s5, 112, sp);                               // sq s5, 112(sp)
  c->sq(gp, 128, sp);                               // sq gp, 128(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->load_symbol2(v1, cache.sky_work);              // lw v1, *sky-work*(s7)
  c->daddiu(v1, v1, 1024);                          // daddiu v1, v1, 1024
  c->lwu(a0, 176, gp);                              // lwu a0, 176(gp)
  c->lwc1(f0, 908, a0);                             // lwc1 f0, 908(a0)
  c->swc1(f0, 0, v1);                               // swc1 f0, 0(v1)
  c->lwu(a0, 176, gp);                              // lwu a0, 176(gp)
  c->lwc1(f0, 128, a0);                             // lwc1 f0, 128(a0)
  c->swc1(f0, 4, v1);                               // swc1 f0, 4(v1)
  c->lwu(a0, 176, gp);                              // lwu a0, 176(gp)
  c->lwc1(f0, 124, a0);                             // lwc1 f0, 124(a0)
  c->swc1(f0, 8, v1);                               // swc1 f0, 8(v1)
  c->lui(a0, 16256);                                // lui a0, 16256
  c->mtc1(f0, a0);                                  // mtc1 f0, a0
  c->swc1(f0, 12, v1);                              // swc1 f0, 12(v1)
  c->daddiu(s5, sp, 16);                            // daddiu s5, sp, 16
  c->sq(r0, 0, s5);                                 // sq r0, 0(s5)
  c->sq(r0, 16, s5);                                // sq r0, 16(s5)
  c->sq(r0, 32, s5);                                // sq r0, 32(s5)
  c->sq(r0, 48, s5);                                // sq r0, 48(s5)
  c->daddiu(s4, sp, 80);                            // daddiu s4, sp, 80
  c->sq(r0, 0, s4);                                 // sq r0, 0(s4)
  c->load_symbol2(v1, cache.time_of_day_context);   // lw v1, *time-of-day-context*(s7)
  c->lwu(v1, 2544, v1);                             // lwu v1, 2544(v1)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L119
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  c->mov64(v1, s4);                                 // or v1, s4, r0
  c->lwu(a0, 176, gp);                              // lwu a0, 176(gp)
  c->daddiu(a0, a0, 1148);                          // daddiu a0, a0, 1148
  c->load_symbol2(a1, cache.ocean_map);             // lw a1, *ocean-map*(s7)
  c->daddu(a1, r0, a1);                             // daddu a1, r0, a1
  c->lqc2(vf4, 0, a0);                              // lqc2 vf4, 0(a0)
  c->lqc2(vf5, 0, a1);                              // lqc2 vf5, 0(a1)
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, v1);                              // sqc2 vf6, 0(v1)
  c->lwc1(f0, 0, s4);                               // lwc1 f0, 0(s4)
  c->negs(f0, f0);                                  // neg.s f0, f0
  c->swc1(f0, 0, s4);                               // swc1 f0, 0(s4)
  c->lwc1(f0, 4, s4);                               // lwc1 f0, 4(s4)
  c->negs(f0, f0);                                  // neg.s f0, f0
  c->swc1(f0, 4, s4);                               // swc1 f0, 4(s4)
  c->lwc1(f0, 8, s4);                               // lwc1 f0, 8(s4)
  c->negs(f0, f0);                                  // neg.s f0, f0
  c->swc1(f0, 8, s4);                               // swc1 f0, 8(s4)
  c->lui(v1, 16256);                                // lui v1, 16256
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->swc1(f0, 12, s4);                              // swc1 f0, 12(s4)
  c->load_symbol2(t9, cache.vector_matrix);         // lw t9, vector-matrix*!(s7)
  c->mov64(a0, s4);                                 // or a0, s4, r0
  c->mov64(a1, s4);                                 // or a1, s4, r0
  c->lwu(v1, 176, gp);                              // lwu v1, 176(gp)
  c->daddiu(a2, v1, 1292);                          // daddiu a2, v1, 1292
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lwu(v1, 176, gp);                              // lwu v1, 176(gp)
  c->lq(v1, 1292, v1);                              // lq v1, 1292(v1)
  c->sq(v1, 0, s5);                                 // sq v1, 0(s5)
  c->lwu(v1, 176, gp);                              // lwu v1, 176(gp)
  c->lq(v1, 1308, v1);                              // lq v1, 1308(v1)
  c->sq(v1, 16, s5);                                // sq v1, 16(s5)
  c->lwu(v1, 176, gp);                              // lwu v1, 176(gp)
  c->lq(v1, 1324, v1);                              // lq v1, 1324(v1)
  c->sq(v1, 32, s5);                                // sq v1, 32(s5)
  c->lq(v1, 0, s4);                                 // lq v1, 0(s4)
  c->sq(v1, 48, s5);                                // sq v1, 48(s5)
  c->load_symbol2(t9, cache.matrix);                // lw t9, matrix*!(s7)
  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->mov64(a1, s5);                                 // or a1, s5, r0
  c->lwu(v1, 176, gp);                              // lwu v1, 176(gp)
  c->daddiu(a2, v1, 156);                           // daddiu a2, v1, 156
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lqc2(vf31, 0, s5);                             // lqc2 vf31, 0(s5)
  c->lqc2(vf30, 16, s5);                            // lqc2 vf30, 16(s5)
  c->lqc2(vf29, 32, s5);                            // lqc2 vf29, 32(s5)
  c->lqc2(vf28, 48, s5);                            // lqc2 vf28, 48(s5)
  c->mov128_gpr_vf(v1, vf28);                       // qmfc2.i v1, vf28
  //beq r0, r0, L120                                // beq r0, r0, L120
  // nop                                            // sll r0, r0, 0
  goto block_3;                                     // branch always

  
block_2:
  c->mov64(v1, s4);                                 // or v1, s4, r0
  c->lwu(a0, 176, gp);                              // lwu a0, 176(gp)
  c->daddiu(a0, a0, 924);                           // daddiu a0, a0, 924
  c->load_symbol2(a1, cache.ocean_map);             // lw a1, *ocean-map*(s7)
  c->daddu(a1, r0, a1);                             // daddu a1, r0, a1
  c->lqc2(vf4, 0, a0);                              // lqc2 vf4, 0(a0)
  c->lqc2(vf5, 0, a1);                              // lqc2 vf5, 0(a1)
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, v1);                              // sqc2 vf6, 0(v1)
  c->lwc1(f0, 0, s4);                               // lwc1 f0, 0(s4)
  c->negs(f0, f0);                                  // neg.s f0, f0
  c->swc1(f0, 0, s4);                               // swc1 f0, 0(s4)
  c->lwc1(f0, 4, s4);                               // lwc1 f0, 4(s4)
  c->negs(f0, f0);                                  // neg.s f0, f0
  c->swc1(f0, 4, s4);                               // swc1 f0, 4(s4)
  c->lwc1(f0, 8, s4);                               // lwc1 f0, 8(s4)
  c->negs(f0, f0);                                  // neg.s f0, f0
  c->swc1(f0, 8, s4);                               // swc1 f0, 8(s4)
  c->lui(v1, 16256);                                // lui v1, 16256
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->swc1(f0, 12, s4);                              // swc1 f0, 12(s4)
  c->lwu(v1, 176, gp);                              // lwu v1, 176(gp)
  c->lq(v1, 364, v1);                               // lq v1, 364(v1)
  c->sq(v1, 0, s5);                                 // sq v1, 0(s5)
  c->lwu(v1, 176, gp);                              // lwu v1, 176(gp)
  c->lq(v1, 380, v1);                               // lq v1, 380(v1)
  c->sq(v1, 16, s5);                                // sq v1, 16(s5)
  c->lwu(v1, 176, gp);                              // lwu v1, 176(gp)
  c->lq(v1, 396, v1);                               // lq v1, 396(v1)
  c->sq(v1, 32, s5);                                // sq v1, 32(s5)
  c->sqc2(vf0, 48, s5);                             // sqc2 vf0, 48(s5)
  c->load_symbol2(t9, cache.vector_matrix);         // lw t9, vector-matrix*!(s7)
  c->mov64(a0, s4);                                 // or a0, s4, r0
  c->mov64(a1, s4);                                 // or a1, s4, r0
  c->mov64(a2, s5);                                 // or a2, s5, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lq(v1, 0, s4);                                 // lq v1, 0(s4)
  c->sq(v1, 48, s5);                                // sq v1, 48(s5)
  c->load_symbol2(t9, cache.matrix);                // lw t9, matrix*!(s7)
  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->mov64(a1, s5);                                 // or a1, s5, r0
  c->lwu(v1, 176, gp);                              // lwu v1, 176(gp)
  c->daddiu(a2, v1, 156);                           // daddiu a2, v1, 156
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lqc2(vf31, 0, s5);                             // lqc2 vf31, 0(s5)
  c->lqc2(vf30, 16, s5);                            // lqc2 vf30, 16(s5)
  c->lqc2(vf29, 32, s5);                            // lqc2 vf29, 32(s5)
  c->lqc2(vf28, 48, s5);                            // lqc2 vf28, 48(s5)
  c->mov128_gpr_vf(v1, vf28);                       // qmfc2.i v1, vf28
  
block_3:
  c->lwu(v1, 176, gp);                              // lwu v1, 176(gp)
  c->lqc2(vf26, 796, v1);                           // lqc2 vf26, 796(v1)
  c->lwu(v1, 176, gp);                              // lwu v1, 176(gp)
  c->lqc2(vf14, 780, v1);                           // lqc2 vf14, 780(v1)
  c->lwu(v1, 176, gp);                              // lwu v1, 176(gp)
  c->lqc2(vf25, 812, v1);                           // lqc2 vf25, 812(v1)
  c->load_symbol2(v1, cache.sky_work);              // lw v1, *sky-work*(s7)
  c->lqc2(vf13, 1024, v1);                          // lqc2 vf13, 1024(v1)
  c->load_symbol2(v1, cache.sky_work);              // lw v1, *sky-work*(s7)
  c->lqc2(vf27, 1008, v1);                          // lqc2 vf27, 1008(v1)
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 128, sp);                               // lq gp, 128(sp)
  c->lq(s5, 112, sp);                               // lq s5, 112(sp)
  c->lq(s4, 96, sp);                                // lq s4, 96(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 144);                           // daddiu sp, sp, 144
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.ocean_map = intern_from_c(-1, 0, "*ocean-map*").c();
  cache.sky_work = intern_from_c(-1, 0, "*sky-work*").c();
  cache.time_of_day_context = intern_from_c(-1, 0, "*time-of-day-context*").c();
  cache.matrix = intern_from_c(-1, 0, "matrix*!").c();
  cache.vector_matrix = intern_from_c(-1, 0, "vector-matrix*!").c();
  gLinkedFunctionTable.reg("init-ocean-far-regs", execute, 512);
}

} // namespace init_ocean_far_regs
} // namespace Mips2C