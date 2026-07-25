//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace render_boundary_tri {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* draw_boundary_polygon; // draw-boundary-polygon
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
  c->addiu(t1, r0, 3);                              // addiu t1, r0, 3
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
  c->sqc2(vf2, 16, t0);                             // sqc2 vf2, 16(t0)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf4);       // vmaddaz.xyzw acc, vf29, vf4
  c->sqc2(vf3, 32, t0);                             // sqc2 vf3, 32(t0)
  c->vmadd_bc(DEST::xyzw, BC::w, vf4, vf28, vf4);   // vmaddw.xyzw vf4, vf28, vf4
  c->sqc2(vf5, 64, t0);                             // sqc2 vf5, 64(t0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf3, 80, t0);                             // sqc2 vf3, 80(t0)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf7);        // vmulax.xyzw acc, vf31, vf7
  c->sqc2(vf8, 112, t0);                            // sqc2 vf8, 112(t0)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf7);       // vmadday.xyzw acc, vf30, vf7
  c->sqc2(vf3, 128, t0);                            // sqc2 vf3, 128(t0)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf7);       // vmaddaz.xyzw acc, vf29, vf7
  c->sqc2(vf2, 160, t0);                            // sqc2 vf2, 160(t0)
  c->vmadd_bc(DEST::xyzw, BC::w, vf7, vf28, vf7);   // vmaddw.xyzw vf7, vf28, vf7
  c->sqc2(vf3, 176, t0);                            // sqc2 vf3, 176(t0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf1, 0, t0);                              // sqc2 vf1, 0(t0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf1, 144, t0);                            // sqc2 vf1, 144(t0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf4, 48, t0);                             // sqc2 vf4, 48(t0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf7, 96, t0);                             // sqc2 vf7, 96(t0)
  c->load_symbol2(t9, cache.draw_boundary_polygon); // lw t9, draw-boundary-polygon(s7)
  c->vsub(DEST::xyzw, vf4, vf4, vf1);               // vsub.xyzw vf4, vf4, vf1
  // nop                                            // sll r0, r0, 0
  c->vsub(DEST::xyzw, vf7, vf7, vf1);               // vsub.xyzw vf7, vf7, vf1
  // nop                                            // sll r0, r0, 0
  c->vopmula(vf4, vf7);                             // vopmula.xyz acc, vf4, vf7
  // nop                                            // sll r0, r0, 0
  c->vopmsub(vf10, vf7, vf4);                       // vopmsub.xyz vf10, vf7, vf4
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf10, vf10, vf1);             // vmul.xyzw vf10, vf10, vf1
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::y, BC::x, vf10, vf10, vf10);     // vaddx.y vf10, vf10, vf10
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::y, BC::z, vf10, vf10, vf10);     // vaddz.y vf10, vf10, vf10
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(v0, vf10);                       // qmfc2.i v0, vf10
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(v0)) >= 0;                   // bgez v0, L31
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  // Unknown instr: jr t9
  // nop                                            // sll r0, r0, 0
  
block_2:
  c->sqc2(vf6, 32, t0);                             // sqc2 vf6, 32(t0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf6, 80, t0);                             // sqc2 vf6, 80(t0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf6, 128, t0);                            // sqc2 vf6, 128(t0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf6, 176, t0);                            // sqc2 vf6, 176(t0)
  // nop                                            // sll r0, r0, 0
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
  cache.fake_scratchpad_data = intern_from_c(-1, 0, "*fake-scratchpad-data*").c();
  cache.draw_boundary_polygon = intern_from_c(-1, 0, "draw-boundary-polygon").c();
  gLinkedFunctionTable.reg("render-boundary-tri", execute, 512);
}

} // namespace render_boundary_tri
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace render_boundary_quad {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* draw_boundary_polygon; // draw-boundary-polygon
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->mov64(v1, a0);                                 // or v1, a0, r0
  c->mov64(v1, a2);                                 // or v1, a2, r0
  c->lqc2(vf14, 780, a2);                           // lqc2 vf14, 780(a2)
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
  c->sqc2(vf2, 16, t0);                             // sqc2 vf2, 16(t0)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf7);        // vmulax.xyzw acc, vf31, vf7
  c->sqc2(vf3, 32, t0);                             // sqc2 vf3, 32(t0)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf7);       // vmadday.xyzw acc, vf30, vf7
  c->sqc2(vf5, 64, t0);                             // sqc2 vf5, 64(t0)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf7);       // vmaddaz.xyzw acc, vf29, vf7
  c->sqc2(vf3, 80, t0);                             // sqc2 vf3, 80(t0)
  c->vmadd_bc(DEST::xyzw, BC::w, vf7, vf28, vf7);   // vmaddw.xyzw vf7, vf28, vf7
  c->sqc2(vf8, 112, t0);                            // sqc2 vf8, 112(t0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf3, 128, t0);                            // sqc2 vf3, 128(t0)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf10);       // vmulax.xyzw acc, vf31, vf10
  c->sqc2(vf11, 160, t0);                           // sqc2 vf11, 160(t0)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf10);      // vmadday.xyzw acc, vf30, vf10
  c->sqc2(vf3, 176, t0);                            // sqc2 vf3, 176(t0)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf10);      // vmaddaz.xyzw acc, vf29, vf10
  c->sqc2(vf2, 208, t0);                            // sqc2 vf2, 208(t0)
  c->vmadd_bc(DEST::xyzw, BC::w, vf10, vf28, vf10); // vmaddw.xyzw vf10, vf28, vf10
  c->sqc2(vf3, 224, t0);                            // sqc2 vf3, 224(t0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf1, 0, t0);                              // sqc2 vf1, 0(t0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf1, 192, t0);                            // sqc2 vf1, 192(t0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf4, 48, t0);                             // sqc2 vf4, 48(t0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf7, 96, t0);                             // sqc2 vf7, 96(t0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf10, 144, t0);                           // sqc2 vf10, 144(t0)
  c->load_symbol2(t9, cache.draw_boundary_polygon); // lw t9, draw-boundary-polygon(s7)
  c->vsub(DEST::xyzw, vf4, vf4, vf1);               // vsub.xyzw vf4, vf4, vf1
  // nop                                            // sll r0, r0, 0
  c->vsub(DEST::xyzw, vf7, vf7, vf1);               // vsub.xyzw vf7, vf7, vf1
  // nop                                            // sll r0, r0, 0
  c->vopmula(vf4, vf7);                             // vopmula.xyz acc, vf4, vf7
  // nop                                            // sll r0, r0, 0
  c->vopmsub(vf10, vf7, vf4);                       // vopmsub.xyz vf10, vf7, vf4
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf10, vf10, vf1);             // vmul.xyzw vf10, vf10, vf1
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::y, BC::x, vf10, vf10, vf10);     // vaddx.y vf10, vf10, vf10
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::y, BC::z, vf10, vf10, vf10);     // vaddz.y vf10, vf10, vf10
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(v0, vf10);                       // qmfc2.i v0, vf10
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(v0)) >= 0;                   // bgez v0, L33
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  // Unknown instr: jr t9
  // nop                                            // sll r0, r0, 0
  
block_2:
  c->sqc2(vf6, 32, t0);                             // sqc2 vf6, 32(t0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf6, 80, t0);                             // sqc2 vf6, 80(t0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf6, 128, t0);                            // sqc2 vf6, 128(t0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf6, 176, t0);                            // sqc2 vf6, 176(t0)
  // nop                                            // sll r0, r0, 0
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
  cache.fake_scratchpad_data = intern_from_c(-1, 0, "*fake-scratchpad-data*").c();
  cache.draw_boundary_polygon = intern_from_c(-1, 0, "draw-boundary-polygon").c();
  gLinkedFunctionTable.reg("render-boundary-quad", execute, 512);
}

} // namespace render_boundary_quad
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace draw_boundary_polygon {
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
  // nop                                            // sll r0, r0, 0
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->load_symbol2(t9, cache.clip_polygon_against_positive_hyperplane);// lw t9, clip-polygon-against-positive-hyperplane(s7)
  c->mov64(a3, t5);                                 // or a3, t5, r0
  c->mov64(t0, t6);                                 // or t0, t6, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->daddu(t3, a3, r0);                             // daddu t3, a3, r0
  c->jalr(call_addr);                               // jalr ra, t9
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L36
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  c->mov64(a3, t6);                                 // or a3, t6, r0
  c->mov64(t0, t5);                                 // or t0, t5, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->daddiu(t3, a3, 4);                             // daddiu t3, a3, 4
  c->jalr(call_addr);                               // jalr ra, t9
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L36
  c->load_symbol2(t9, cache.clip_polygon_against_negative_hyperplane);// lw t9, clip-polygon-against-negative-hyperplane(s7)
  if (bc) {goto block_7;}                           // branch non-likely

  c->mov64(a3, t5);                                 // or a3, t5, r0
  c->mov64(t0, t6);                                 // or t0, t6, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->daddu(t3, a3, r0);                             // daddu t3, a3, r0
  c->jalr(call_addr);                               // jalr ra, t9
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L36
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  c->mov64(a3, t6);                                 // or a3, t6, r0
  c->mov64(t0, t5);                                 // or t0, t5, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->daddiu(t3, a3, 4);                             // daddiu t3, a3, 4
  c->jalr(call_addr);                               // jalr ra, t9
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L36
  c->lw(t0, 4, a1);                                 // lw t0, 4(a1)
  if (bc) {goto block_7;}                           // branch non-likely

  c->mov64(a3, t5);                                 // or a3, t5, r0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf27, 0, t0);                             // sqc2 vf27, 0(t0)
  c->daddiu(t0, t0, 16);                            // daddiu t0, t0, 16
  c->sw(t1, -16, t0);                               // sw t1, -16(t0)
  // nop                                            // sll r0, r0, 0
  
block_5:
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
  c->vadd(DEST::xyzw, vf2, vf2, vf24);              // vadd.xyzw vf2, vf2, vf24
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
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L35
  c->sqc2(vf1, -16, t0);                            // sqc2 vf1, -16(t0)
  if (bc) {goto block_5;}                           // branch non-likely

  c->sw(t0, 4, a1);                                 // sw t0, 4(a1)
  // nop                                            // sll r0, r0, 0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->daddiu(v0, s7, 4);                             // daddiu v0, s7, 4
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
  goto end_of_function;                             // return

  
block_7:
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
  gLinkedFunctionTable.reg("draw-boundary-polygon", execute, 512);
}

} // namespace draw_boundary_polygon
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace init_boundary_regs {
struct Cache {
  void* sky_work; // *sky-work*
  void* view_get_active_math_camera; // view-get-active-math-camera
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->load_symbol2(t9, cache.view_get_active_math_camera);// lw t9, view-get-active-math-camera(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(a2, v0);                                 // or a2, v0, r0
  c->load_symbol2(v1, cache.sky_work);              // lw v1, *sky-work*(s7)
  c->daddiu(v1, v1, 1024);                          // daddiu v1, v1, 1024
  c->lwc1(f0, 908, a2);                             // lwc1 f0, 908(a2)
  c->swc1(f0, 0, v1);                               // swc1 f0, 0(v1)
  c->lwc1(f0, 128, a2);                             // lwc1 f0, 128(a2)
  c->swc1(f0, 4, v1);                               // swc1 f0, 4(v1)
  c->lwc1(f0, 124, a2);                             // lwc1 f0, 124(a2)
  c->swc1(f0, 8, v1);                               // swc1 f0, 8(v1)
  c->lui(a0, 17727);                                // lui a0, 17727
  c->ori(a0, a0, 61440);                            // ori a0, a0, 61440
  c->mtc1(f0, a0);                                  // mtc1 f0, a0
  c->swc1(f0, 12, v1);                              // swc1 f0, 12(v1)
  c->lqc2(vf31, 572, a2);                           // lqc2 vf31, 572(a2)
  c->lqc2(vf30, 588, a2);                           // lqc2 vf30, 588(a2)
  c->lqc2(vf29, 604, a2);                           // lqc2 vf29, 604(a2)
  c->lqc2(vf28, 620, a2);                           // lqc2 vf28, 620(a2)
  c->lqc2(vf14, 780, a2);                           // lqc2 vf14, 780(a2)
  c->lqc2(vf26, 796, a2);                           // lqc2 vf26, 796(a2)
  c->lqc2(vf25, 812, a2);                           // lqc2 vf25, 812(a2)
  c->load_symbol2(v1, cache.sky_work);              // lw v1, *sky-work*(s7)
  c->lqc2(vf13, 1024, v1);                          // lqc2 vf13, 1024(v1)
  c->vmul(DEST::xyzw, vf31, vf31, vf14);            // vmul.xyzw vf31, vf31, vf14
  c->vmul(DEST::xyzw, vf30, vf30, vf14);            // vmul.xyzw vf30, vf30, vf14
  c->vmul(DEST::xyzw, vf29, vf29, vf14);            // vmul.xyzw vf29, vf29, vf14
  c->vmul(DEST::xyzw, vf28, vf28, vf14);            // vmul.xyzw vf28, vf28, vf14
  c->vmove(DEST::xyzw, vf24, vf0);                  // vmove.xyzw vf24, vf0
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
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
  cache.sky_work = intern_from_c(-1, 0, "*sky-work*").c();
  cache.view_get_active_math_camera = intern_from_c(-1, 0, "view-get-active-math-camera").c();
  gLinkedFunctionTable.reg("init-boundary-regs", execute, 256);
}

} // namespace init_boundary_regs
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace debug_line_clip {
struct Cache {
  void* view_get_active_math_camera; // view-get-active-math-camera
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -80);                           // daddiu sp, sp, -80
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s3, 16, sp);                                // sq s3, 16(sp)
  c->sq(s4, 32, sp);                                // sq s4, 32(sp)
  c->sq(s5, 48, sp);                                // sq s5, 48(sp)
  c->sq(gp, 64, sp);                                // sq gp, 64(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->mov64(s5, a1);                                 // or s5, a1, r0
  c->mov64(s4, a2);                                 // or s4, a2, r0
  c->mov64(s3, a3);                                 // or s3, a3, r0
  c->load_symbol2(t9, cache.view_get_active_math_camera);// lw t9, view-get-active-math-camera(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->lqc2(vf9, 0, s4);                              // lqc2 vf9, 0(s4)
  c->lqc2(vf10, 0, s3);                             // lqc2 vf10, 0(s3)
  c->lqc2(vf16, 940, v1);                           // lqc2 vf16, 940(v1)
  c->lqc2(vf17, 956, v1);                           // lqc2 vf17, 956(v1)
  c->lqc2(vf18, 972, v1);                           // lqc2 vf18, 972(v1)
  c->lqc2(vf19, 988, v1);                           // lqc2 vf19, 988(v1)
  c->vmula_bc(DEST::xyzw, BC::x, vf16, vf9);        // vmulax.xyzw acc, vf16, vf9
  c->vmadda_bc(DEST::xyzw, BC::y, vf17, vf9);       // vmadday.xyzw acc, vf17, vf9
  c->vmadda_bc(DEST::xyzw, BC::z, vf18, vf9);       // vmaddaz.xyzw acc, vf18, vf9
  c->vmsub_bc(DEST::xyzw, BC::w, vf11, vf19, vf0);  // vmsubw.xyzw vf11, vf19, vf0
  c->vmula_bc(DEST::xyzw, BC::x, vf16, vf10);       // vmulax.xyzw acc, vf16, vf10
  c->vmadda_bc(DEST::xyzw, BC::y, vf17, vf10);      // vmadday.xyzw acc, vf17, vf10
  c->vmadda_bc(DEST::xyzw, BC::z, vf18, vf10);      // vmaddaz.xyzw acc, vf18, vf10
  c->vmsub_bc(DEST::xyzw, BC::w, vf12, vf19, vf0);  // vmsubw.xyzw vf12, vf19, vf0
  c->mov128_gpr_vf(v1, vf11);                       // qmfc2.i v1, vf11
  c->pcgtw(v1, r0, v1);                             // pcgtw v1, r0, v1
  c->ppach(a0, r0, v1);                             // ppach a0, r0, v1
  c->mov128_gpr_vf(v1, vf12);                       // qmfc2.i v1, vf12
  c->pcgtw(v1, r0, v1);                             // pcgtw v1, r0, v1
  c->ppach(a1, r0, v1);                             // ppach a1, r0, v1
  c->and_(v1, a0, a1);                              // and v1, a0, a1
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L258
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_10;}                          // branch non-likely

  c->vmove(DEST::xyzw, vf13, vf9);                  // vmove.xyzw vf13, vf9
  c->vmove(DEST::xyzw, vf14, vf10);                 // vmove.xyzw vf14, vf10
  c->or_(a0, a0, a1);                               // or a0, a0, a1
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L257
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_9;}                           // branch non-likely

  c->mov128_gpr_vf(a0, vf11);                       // qmfc2.i a0, vf11
  c->mov128_gpr_vf(a1, vf12);                       // qmfc2.i a1, vf12
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  c->lui(a2, 16256);                                // lui a2, 16256
  c->mtc1(f2, a2);                                  // mtc1 f2, a2
  c->addiu(a2, r0, 3);                              // addiu a2, r0, 3
  // Unknown instr: mtsab r0, 4
  
block_3:
  c->mtc1(f3, a0);                                  // mtc1 f3, a0
  c->mtc1(f4, a1);                                  // mtc1 f4, a1
  c->subs(f5, f3, f4);                              // sub.s f5, f3, f4
  cop1_bc = c->fprs[f3] < c->fprs[f0];              // c.lt.s f3, f0
  bc = !cop1_bc;                                    // bc1f L255
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_5;}                           // branch non-likely

  c->divs(f6, f3, f5);                              // div.s f6, f3, f5
  c->maxs(f1, f6, f1);                              // max.s f1, f6, f1
  
block_5:
  cop1_bc = c->fprs[f4] < c->fprs[f0];              // c.lt.s f4, f0
  bc = !cop1_bc;                                    // bc1f L256
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  c->divs(f3, f3, f5);                              // div.s f3, f3, f5
  c->mins(f2, f3, f2);                              // min.s f2, f3, f2
  
block_7:
  // Unknown instr: qfsrv a0, a0, a0
  // Unknown instr: qfsrv a1, a1, a1
  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L254
  c->daddiu(a2, a2, -1);                            // daddiu a2, a2, -1
  if (bc) {goto block_3;}                           // branch non-likely

  c->mfc1(a0, f1);                                  // mfc1 a0, f1
  c->mfc1(a1, f2);                                  // mfc1 a1, f2
  c->mov128_vf_gpr(vf20, a0);                       // qmtc2.i vf20, a0
  c->mov128_vf_gpr(vf21, a1);                       // qmtc2.i vf21, a1
  c->vsub(DEST::xyzw, vf15, vf10, vf9);             // vsub.xyzw vf15, vf10, vf9
  c->vmul_bc(DEST::xyzw, BC::x, vf13, vf15, vf20);  // vmulx.xyzw vf13, vf15, vf20
  c->vadd(DEST::xyzw, vf13, vf9, vf13);             // vadd.xyzw vf13, vf9, vf13
  c->vmul_bc(DEST::xyzw, BC::x, vf14, vf15, vf21);  // vmulx.xyzw vf14, vf15, vf21
  c->vadd(DEST::xyzw, vf14, vf9, vf14);             // vadd.xyzw vf14, vf9, vf14
  
block_9:
  c->sqc2(vf13, 0, gp);                             // sqc2 vf13, 0(gp)
  c->sqc2(vf14, 0, s5);                             // sqc2 vf14, 0(s5)
  
block_10:
  c->daddiu(v0, s7, 4);                             // daddiu v0, s7, 4
  c->movn(v0, s7, v1);                              // movn v0, s7, v1
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
  cache.view_get_active_math_camera = intern_from_c(-1, 0, "view-get-active-math-camera").c();
  gLinkedFunctionTable.reg("debug-line-clip?", execute, 256);
}

} // namespace debug_line_clip
} // namespace Mips2C