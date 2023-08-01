//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace moving_sphere_triangle_intersect {
struct Cache {
  void* collide_do_primitives; // collide-do-primitives
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->lqc2(vf14, 0, a0);                             // lqc2 vf14, 0(a0)
  c->lqc2(vf15, 0, a1);                             // lqc2 vf15, 0(a1)
  c->lqc2(vf11, 0, a3);                             // lqc2 vf11, 0(a3)
  c->lqc2(vf12, 16, a3);                            // lqc2 vf12, 16(a3)
  c->lqc2(vf13, 32, a3);                            // lqc2 vf13, 32(a3)
  c->mov128_vf_gpr(vf1, a2);                        // qmtc2.i vf1, a2
  c->vadd(DEST::xyzw, vf6, vf14, vf15);             // vadd.xyzw vf6, vf14, vf15
  c->vsub(DEST::xyzw, vf11, vf11, vf12);            // vsub.xyzw vf11, vf11, vf12
  c->vsub(DEST::xyzw, vf13, vf13, vf12);            // vsub.xyzw vf13, vf13, vf12
  c->vsub(DEST::xyzw, vf14, vf14, vf12);            // vsub.xyzw vf14, vf14, vf12
  c->vsub(DEST::xyzw, vf6, vf6, vf12);              // vsub.xyzw vf6, vf6, vf12
  c->vmini(DEST::xyzw, vf2, vf11, vf0);             // vmini.xyzw vf2, vf11, vf0
  c->vopmula(vf13, vf11);                           // vopmula.xyz acc, vf13, vf11
  c->vopmsub(vf16, vf11, vf13);                     // vopmsub.xyz vf16, vf11, vf13
  c->vmax(DEST::xyzw, vf3, vf11, vf0);              // vmax.xyzw vf3, vf11, vf0
  c->vmini(DEST::xyzw, vf4, vf14, vf6);             // vmini.xyzw vf4, vf14, vf6
  c->vmax(DEST::xyzw, vf5, vf14, vf6);              // vmax.xyzw vf5, vf14, vf6
  c->vmul(DEST::xyzw, vf7, vf16, vf16);             // vmul.xyzw vf7, vf16, vf16
  c->vmini(DEST::xyzw, vf2, vf2, vf13);             // vmini.xyzw vf2, vf2, vf13
  c->vmax(DEST::xyzw, vf3, vf3, vf13);              // vmax.xyzw vf3, vf3, vf13
  c->vsub_bc(DEST::xyzw, BC::x, vf4, vf4, vf1);     // vsubx.xyzw vf4, vf4, vf1
  c->vadd_bc(DEST::x, BC::y, vf7, vf7, vf7);        // vaddy.x vf7, vf7, vf7
  c->vadd_bc(DEST::xyzw, BC::x, vf5, vf5, vf1);     // vaddx.xyzw vf5, vf5, vf1
  // nop                                            // sll r0, r0, 0
  c->vsub(DEST::xyzw, vf3, vf3, vf4);               // vsub.xyzw vf3, vf3, vf4
  c->vadd_bc(DEST::x, BC::z, vf7, vf7, vf7);        // vaddz.x vf7, vf7, vf7
  c->vsub(DEST::xyzw, vf5, vf5, vf2);               // vsub.xyzw vf5, vf5, vf2
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t2, vf3);                        // qmfc2.i t2, vf3
  c->mov128_gpr_vf(v1, vf5);                        // qmfc2.i v1, vf5
  c->vrsqrt(vf0, BC::w, vf7, BC::x);                // vrsqrt Q, vf0.w, vf7.x
  c->or_(v1, t2, v1);                               // or v1, t2, v1
  c->pcgtw(v1, r0, v1);                             // pcgtw v1, r0, v1
  c->ppach(v1, r0, v1);                             // ppach v1, r0, v1
  c->dsll(v1, v1, 16);                              // dsll v1, v1, 16
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L14
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_14;}                          // branch non-likely

  c->vmul(DEST::xyzw, vf2, vf16, vf15);             // vmul.xyzw vf2, vf16, vf15
  c->vmul(DEST::xyzw, vf3, vf16, vf14);             // vmul.xyzw vf3, vf16, vf14
  c->vadd_bc(DEST::x, BC::y, vf2, vf2, vf2);        // vaddy.x vf2, vf2, vf2
  c->vsub_bc(DEST::y, BC::y, vf3, vf0, vf3);        // vsuby.y vf3, vf0, vf3
  c->vadd_bc(DEST::x, BC::z, vf2, vf2, vf2);        // vaddz.x vf2, vf2, vf2
  c->vsub_bc(DEST::y, BC::x, vf3, vf3, vf3);        // vsubx.y vf3, vf3, vf3
  c->vsub_bc(DEST::y, BC::z, vf3, vf3, vf3);        // vsubz.y vf3, vf3, vf3
  c->vadd_bc(DEST::x, BC::x, vf3, vf0, vf0);        // vaddx.x vf3, vf0, vf0
  c->vadd_bc(DEST::x, BC::x, vf4, vf0, vf0);        // vaddx.x vf4, vf0, vf0
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyzw, vf16, vf16);                 // vmulq.xyzw vf16, vf16, Q
  c->vmove(DEST::w, vf16, vf0);                     // vmove.w vf16, vf0
  c->vmulq(DEST::xyzw, vf2, vf2);                   // vmulq.xyzw vf2, vf2, Q
  c->vmulq(DEST::xyzw, vf4, vf3);                   // vmulq.xyzw vf4, vf3, Q
  c->vmulq(DEST::xyzw, vf3, vf3);                   // vmulq.xyzw vf3, vf3, Q
  c->sqc2(vf16, 0, t1);                             // sqc2 vf16, 0(t1)
  // nop                                            // vnop
  // nop                                            // vnop
  c->vdiv(vf0, BC::w, vf2, BC::x);                  // vdiv Q, vf0.w, vf2.x
  c->vadd_bc(DEST::y, BC::x, vf3, vf3, vf1);        // vaddx.y vf3, vf3, vf1
  c->vsub_bc(DEST::y, BC::x, vf4, vf4, vf1);        // vsubx.y vf4, vf4, vf1
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyzw, vf3, vf3);                   // vmulq.xyzw vf3, vf3, Q
  c->vmulq(DEST::xyzw, vf4, vf4);                   // vmulq.xyzw vf4, vf4, Q
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(v1, vf3);                        // qmfc2.i v1, vf3
  c->mov128_gpr_vf(t1, vf4);                        // qmfc2.i t1, vf4
  bc = ((s64)c->sgpr64(v1)) < 0;                    // bltz v1, L12
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_11;}                          // branch non-likely

  bc = ((s64)c->sgpr64(t1)) < 0;                    // bltz t1, L13
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely

  c->dsubu(v1, v1, t1);                             // dsubu v1, v1, t1
  bc = ((s64)c->sgpr64(v1)) < 0;                    // bltz v1, L9
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_6;}                           // branch non-likely

  c->vsub_bc(DEST::xyzw, BC::w, vf2, vf4, vf0);     // vsubw.xyzw vf2, vf4, vf0
  c->mov128_gpr_vf(v1, vf2);                        // qmfc2.i v1, vf2
  bc = ((s64)c->sgpr64(v1)) >= 0;                   // bgez v1, L14
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_14;}                          // branch non-likely

  c->mov128_gpr_vf(v0, vf4);                        // qmfc2.i v0, vf4
  c->vmula_bc(DEST::xyzw, BC::w, vf14, vf0);        // vmulaw.xyzw acc, vf14, vf0
  c->vmadd_bc(DEST::xyzw, BC::y, vf8, vf15, vf4);   // vmaddy.xyzw vf8, vf15, vf4
  //beq r0, r0, L10                                 // beq r0, r0, L10
  // nop                                            // sll r0, r0, 0
  goto block_8;                                     // branch always


  block_6:
  c->vsub_bc(DEST::xyzw, BC::w, vf2, vf3, vf0);     // vsubw.xyzw vf2, vf3, vf0
  c->mov128_gpr_vf(v1, vf2);                        // qmfc2.i v1, vf2
  bc = ((s64)c->sgpr64(v1)) >= 0;                   // bgez v1, L14
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_14;}                          // branch non-likely

  c->vmula_bc(DEST::xyzw, BC::w, vf14, vf0);        // vmulaw.xyzw acc, vf14, vf0
  c->vmadd_bc(DEST::xyzw, BC::y, vf8, vf15, vf3);   // vmaddy.xyzw vf8, vf15, vf3
  c->mov128_gpr_vf(v0, vf3);                        // qmfc2.i v0, vf3

  block_8:
  c->dsra32(v0, v0, 0);                             // dsra32 v0, v0, 0
  c->vsub(DEST::xyzw, vf9, vf8, vf13);              // vsub.xyzw vf9, vf8, vf13
  c->vsub(DEST::xyzw, vf10, vf8, vf11);             // vsub.xyzw vf10, vf8, vf11
  c->vopmula(vf13, vf8);                            // vopmula.xyz acc, vf13, vf8
  c->vopmsub(vf5, vf8, vf13);                       // vopmsub.xyz vf5, vf8, vf13
  c->vopmula(vf8, vf11);                            // vopmula.xyz acc, vf8, vf11
  c->vopmsub(vf6, vf11, vf8);                       // vopmsub.xyz vf6, vf11, vf8
  c->vopmula(vf9, vf10);                            // vopmula.xyz acc, vf9, vf10
  c->vopmsub(vf7, vf10, vf9);                       // vopmsub.xyz vf7, vf10, vf9
  c->vmul(DEST::xyzw, vf5, vf5, vf16);              // vmul.xyzw vf5, vf5, vf16
  c->vmul(DEST::xyzw, vf6, vf6, vf16);              // vmul.xyzw vf6, vf6, vf16
  c->vmul(DEST::xyzw, vf7, vf7, vf16);              // vmul.xyzw vf7, vf7, vf16
  c->vadd_bc(DEST::y, BC::x, vf5, vf5, vf5);        // vaddx.y vf5, vf5, vf5
  c->vadd_bc(DEST::y, BC::x, vf6, vf6, vf6);        // vaddx.y vf6, vf6, vf6
  c->vadd_bc(DEST::y, BC::x, vf7, vf7, vf7);        // vaddx.y vf7, vf7, vf7
  c->vadd_bc(DEST::y, BC::z, vf5, vf5, vf5);        // vaddz.y vf5, vf5, vf5
  c->vadd_bc(DEST::y, BC::z, vf6, vf6, vf6);        // vaddz.y vf6, vf6, vf6
  c->vadd_bc(DEST::y, BC::z, vf7, vf7, vf7);        // vaddz.y vf7, vf7, vf7
  c->mov128_gpr_vf(t1, vf5);                        // qmfc2.i t1, vf5
  c->mov128_gpr_vf(t2, vf6);                        // qmfc2.i t2, vf6
  c->mov128_gpr_vf(v1, vf7);                        // qmfc2.i v1, vf7
  c->or_(t1, t1, t2);                               // or t1, t1, t2
  c->or_(v1, t1, v1);                               // or v1, t1, v1
  bc = ((s64)c->sgpr64(v1)) < 0;                    // bltz v1, L11
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_10;}                          // branch non-likely

  c->vopmula(vf8, vf16);                            // vopmula.xyz acc, vf8, vf16
  c->vopmsub(vf5, vf16, vf8);                       // vopmsub.xyz vf5, vf16, vf8
  c->vopmula(vf16, vf5);                            // vopmula.xyz acc, vf16, vf5
  c->vopmsub(vf5, vf5, vf16);                       // vopmsub.xyz vf5, vf5, vf16
  c->vadd(DEST::xyzw, vf5, vf5, vf12);              // vadd.xyzw vf5, vf5, vf12
  c->sqc2(vf5, 0, t0);                              // sqc2 vf5, 0(t0)
  //beq r0, r0, L15                                 // beq r0, r0, L15
  // nop                                            // sll r0, r0, 0
  goto block_15;                                    // branch always


  block_10:
  c->load_symbol2(t9, cache.collide_do_primitives);  // lw t9, collide-do-primitives(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  //beq r0, r0, L15                                 // beq r0, r0, L15
  // nop                                            // sll r0, r0, 0
  goto block_15;                                    // branch always


  block_11:
  bc = ((s64)c->sgpr64(t1)) < 0;                    // bltz t1, L14
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_14;}                          // branch non-likely


  block_12:
  c->vsub(DEST::xyzw, vf9, vf14, vf13);             // vsub.xyzw vf9, vf14, vf13
  c->vsub(DEST::xyzw, vf10, vf14, vf11);            // vsub.xyzw vf10, vf14, vf11
  c->vopmula(vf13, vf14);                           // vopmula.xyz acc, vf13, vf14
  c->vopmsub(vf5, vf14, vf13);                      // vopmsub.xyz vf5, vf14, vf13
  c->vopmula(vf14, vf11);                           // vopmula.xyz acc, vf14, vf11
  c->vopmsub(vf6, vf11, vf14);                      // vopmsub.xyz vf6, vf11, vf14
  c->vopmula(vf9, vf10);                            // vopmula.xyz acc, vf9, vf10
  c->vopmsub(vf7, vf10, vf9);                       // vopmsub.xyz vf7, vf10, vf9
  c->vmul(DEST::xyzw, vf5, vf5, vf16);              // vmul.xyzw vf5, vf5, vf16
  c->vmul(DEST::xyzw, vf6, vf6, vf16);              // vmul.xyzw vf6, vf6, vf16
  c->vmul(DEST::xyzw, vf7, vf7, vf16);              // vmul.xyzw vf7, vf7, vf16
  c->vadd_bc(DEST::y, BC::x, vf5, vf5, vf5);        // vaddx.y vf5, vf5, vf5
  c->vadd_bc(DEST::y, BC::x, vf6, vf6, vf6);        // vaddx.y vf6, vf6, vf6
  c->vadd_bc(DEST::y, BC::x, vf7, vf7, vf7);        // vaddx.y vf7, vf7, vf7
  c->vadd_bc(DEST::y, BC::z, vf5, vf5, vf5);        // vaddz.y vf5, vf5, vf5
  c->vadd_bc(DEST::y, BC::z, vf6, vf6, vf6);        // vaddz.y vf6, vf6, vf6
  c->vadd_bc(DEST::y, BC::z, vf7, vf7, vf7);        // vaddz.y vf7, vf7, vf7
  c->mov128_gpr_vf(t1, vf5);                        // qmfc2.i t1, vf5
  c->mov128_gpr_vf(t2, vf6);                        // qmfc2.i t2, vf6
  c->mov128_gpr_vf(v1, vf7);                        // qmfc2.i v1, vf7
  c->or_(t1, t1, t2);                               // or t1, t1, t2
  c->or_(v1, t1, v1);                               // or v1, t1, v1
  bc = ((s64)c->sgpr64(v1)) < 0;                    // bltz v1, L11
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_10;}                          // branch non-likely

  c->vopmula(vf14, vf16);                           // vopmula.xyz acc, vf14, vf16
  c->vopmsub(vf5, vf16, vf14);                      // vopmsub.xyz vf5, vf16, vf14
  c->vopmula(vf16, vf5);                            // vopmula.xyz acc, vf16, vf5
  c->vopmsub(vf5, vf5, vf16);                       // vopmsub.xyz vf5, vf5, vf16
  c->vadd(DEST::xyzw, vf5, vf5, vf12);              // vadd.xyzw vf5, vf5, vf12
  c->sqc2(vf5, 0, t0);                              // sqc2 vf5, 0(t0)
  //beq r0, r0, L15                                 // beq r0, r0, L15
  c->addiu(v0, r0, 0);                              // addiu v0, r0, 0
  goto block_15;                                    // branch always


  block_14:
  c->lui(v0, -13122);                               // lui v0, -13122
  c->ori(v0, v0, 48160);                            // ori v0, v0, 48160

  block_15:
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
  cache.collide_do_primitives = intern_from_c("collide-do-primitives").c();
  gLinkedFunctionTable.reg("moving-sphere-triangle-intersect", execute, 512);
}

} // namespace moving_sphere_triangle_intersect
} // namespace Mips2C


//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace collide_do_primitives {
struct Cache {
  void* ray_cylinder_intersect; // ray-cylinder-intersect
  void* ray_sphere_intersect; // ray-sphere-intersect
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -144);                          // daddiu sp, sp, -144
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s1, 32, sp);                                // sq s1, 32(sp)
  c->sq(s2, 48, sp);                                // sq s2, 48(sp)
  c->sq(s3, 64, sp);                                // sq s3, 64(sp)
  c->sq(s4, 80, sp);                                // sq s4, 80(sp)
  c->sq(s5, 96, sp);                                // sq s5, 96(sp)
  c->sq(gp, 112, sp);                               // sq gp, 112(sp)
  c->swc1(f28, 128, sp);                            // swc1 f28, 128(sp)
  c->swc1(f30, 132, sp);                            // swc1 f30, 132(sp)
  c->mov64(s4, a0);                                 // or s4, a0, r0
  c->mov64(s3, a1);                                 // or s3, a1, r0
  c->mov64(s1, a2);                                 // or s1, a2, r0
  c->mov64(s5, a3);                                 // or s5, a3, r0
  c->mov64(gp, t0);                                 // or gp, t0, r0
  c->daddiu(s2, sp, 16);                            // daddiu s2, sp, 16
  c->lui(v1, 16384);                                // lui v1, 16384
  c->mtc1(f31, v1);                                 // mtc1 f31, v1
  c->mtc1(f28, r0);                                 // mtc1 f28, r0
  c->load_symbol2(t9, cache.ray_sphere_intersect);   // lw t9, ray-sphere-intersect(s7)
  c->mov64(a0, s4);                                 // or a0, s4, r0
  c->mov64(a1, s3);                                 // or a1, s3, r0
  c->daddu(a2, r0, s5);                             // daddu a2, r0, s5
  c->mov64(a3, s1);                                 // or a3, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->mtc1(f30, v0);                                 // mtc1 f30, v0
  cop1_bc = c->fprs[f30] < c->fprs[f28];            // c.lt.s f30, f28
  bc = cop1_bc;                                     // bc1t L17
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  c->mtc1(f31, v0);                                 // mtc1 f31, v0
  c->lqc2(vf31, 0, s5);                             // lqc2 vf31, 0(s5)

  block_2:
  c->load_symbol2(t9, cache.ray_sphere_intersect);   // lw t9, ray-sphere-intersect(s7)
  c->mov64(a0, s4);                                 // or a0, s4, r0
  c->mov64(a1, s3);                                 // or a1, s3, r0
  c->daddiu(a2, s5, 16);                            // daddiu a2, s5, 16
  c->mov64(a3, s1);                                 // or a3, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->mtc1(f30, v0);                                 // mtc1 f30, v0
  cop1_bc = c->fprs[f30] < c->fprs[f28];            // c.lt.s f30, f28
  bc = cop1_bc;                                     // bc1t L18
  cop1_bc = c->fprs[f30] < c->fprs[f31];            // c.lt.s f30, f31
  if (bc) {goto block_5;}                           // branch non-likely

  bc = !cop1_bc;                                    // bc1f L18
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_5;}                           // branch non-likely

  c->mtc1(f31, v0);                                 // mtc1 f31, v0
  c->lqc2(vf31, 16, s5);                            // lqc2 vf31, 16(s5)

  block_5:
  c->load_symbol2(t9, cache.ray_sphere_intersect);   // lw t9, ray-sphere-intersect(s7)
  c->mov64(a0, s4);                                 // or a0, s4, r0
  c->mov64(a1, s3);                                 // or a1, s3, r0
  c->daddiu(a2, s5, 32);                            // daddiu a2, s5, 32
  c->mov64(a3, s1);                                 // or a3, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->mtc1(f30, v0);                                 // mtc1 f30, v0
  cop1_bc = c->fprs[f30] < c->fprs[f28];            // c.lt.s f30, f28
  bc = cop1_bc;                                     // bc1t L19
  cop1_bc = c->fprs[f30] < c->fprs[f31];            // c.lt.s f30, f31
  if (bc) {goto block_8;}                           // branch non-likely

  bc = !cop1_bc;                                    // bc1f L19
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_8;}                           // branch non-likely

  c->mtc1(f31, v0);                                 // mtc1 f31, v0
  c->lqc2(vf31, 32, s5);                            // lqc2 vf31, 32(s5)

  block_8:
  c->lqc2(vf1, 0, s5);                              // lqc2 vf1, 0(s5)
  c->lqc2(vf2, 16, s5);                             // lqc2 vf2, 16(s5)
  c->vsub(DEST::xyzw, vf2, vf2, vf1);               // vsub.xyzw vf2, vf2, vf1
  c->vmul(DEST::xyzw, vf3, vf2, vf2);               // vmul.xyzw vf3, vf2, vf2
  c->vadd_bc(DEST::x, BC::y, vf3, vf3, vf3);        // vaddy.x vf3, vf3, vf3
  c->vadd_bc(DEST::x, BC::z, vf3, vf3, vf3);        // vaddz.x vf3, vf3, vf3
  c->vrsqrt(vf0, BC::w, vf3, BC::x);                // vrsqrt Q, vf0.w, vf3.x
  c->mov128_gpr_vf(v1, vf3);                        // qmfc2.i v1, vf3
  c->mtc1(f30, v1);                                 // mtc1 f30, v1
  c->sqrts(f30, f30);                               // sqrt.s f30, f30
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyzw, vf2, vf2);                   // vmulq.xyzw vf2, vf2, Q
  c->sqc2(vf2, 0, s2);                              // sqc2 vf2, 0(s2)
  c->mfc1(t1, f30);                                 // mfc1 t1, f30
  c->load_symbol2(t9, cache.ray_cylinder_intersect); // lw t9, ray-cylinder-intersect(s7)
  c->mov64(a0, s4);                                 // or a0, s4, r0
  c->mov64(a1, s3);                                 // or a1, s3, r0
  c->daddu(a2, r0, s5);                             // daddu a2, r0, s5
  c->mov64(a3, s2);                                 // or a3, s2, r0
  c->mov64(t0, s1);                                 // or t0, s1, r0
  c->mov64(t2, gp);                                 // or t2, gp, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->mtc1(f30, v0);                                 // mtc1 f30, v0
  cop1_bc = c->fprs[f30] < c->fprs[f28];            // c.lt.s f30, f28
  bc = cop1_bc;                                     // bc1t L20
  cop1_bc = c->fprs[f30] < c->fprs[f31];            // c.lt.s f30, f31
  if (bc) {goto block_11;}                          // branch non-likely

  bc = !cop1_bc;                                    // bc1f L20
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_11;}                          // branch non-likely

  c->mtc1(f31, v0);                                 // mtc1 f31, v0
  c->lqc2(vf31, 0, gp);                             // lqc2 vf31, 0(gp)

  block_11:
  c->lqc2(vf1, 16, s5);                             // lqc2 vf1, 16(s5)
  c->lqc2(vf2, 32, s5);                             // lqc2 vf2, 32(s5)
  c->vsub(DEST::xyzw, vf2, vf2, vf1);               // vsub.xyzw vf2, vf2, vf1
  c->vmul(DEST::xyzw, vf3, vf2, vf2);               // vmul.xyzw vf3, vf2, vf2
  c->vadd_bc(DEST::x, BC::y, vf3, vf3, vf3);        // vaddy.x vf3, vf3, vf3
  c->vadd_bc(DEST::x, BC::z, vf3, vf3, vf3);        // vaddz.x vf3, vf3, vf3
  c->vrsqrt(vf0, BC::w, vf3, BC::x);                // vrsqrt Q, vf0.w, vf3.x
  c->mov128_gpr_vf(v1, vf3);                        // qmfc2.i v1, vf3
  c->mtc1(f30, v1);                                 // mtc1 f30, v1
  c->sqrts(f30, f30);                               // sqrt.s f30, f30
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyzw, vf2, vf2);                   // vmulq.xyzw vf2, vf2, Q
  c->sqc2(vf2, 0, s2);                              // sqc2 vf2, 0(s2)
  c->mfc1(t1, f30);                                 // mfc1 t1, f30
  c->load_symbol2(t9, cache.ray_cylinder_intersect); // lw t9, ray-cylinder-intersect(s7)
  c->mov64(a0, s4);                                 // or a0, s4, r0
  c->mov64(a1, s3);                                 // or a1, s3, r0
  c->daddiu(a2, s5, 16);                            // daddiu a2, s5, 16
  c->mov64(a3, s2);                                 // or a3, s2, r0
  c->mov64(t0, s1);                                 // or t0, s1, r0
  c->mov64(t2, gp);                                 // or t2, gp, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->mtc1(f30, v0);                                 // mtc1 f30, v0
  cop1_bc = c->fprs[f30] < c->fprs[f28];            // c.lt.s f30, f28
  bc = cop1_bc;                                     // bc1t L21
  cop1_bc = c->fprs[f30] < c->fprs[f31];            // c.lt.s f30, f31
  if (bc) {goto block_14;}                          // branch non-likely

  bc = !cop1_bc;                                    // bc1f L21
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_14;}                          // branch non-likely

  c->mtc1(f31, v0);                                 // mtc1 f31, v0
  c->lqc2(vf31, 0, gp);                             // lqc2 vf31, 0(gp)

  block_14:
  c->lqc2(vf1, 32, s5);                             // lqc2 vf1, 32(s5)
  c->lqc2(vf2, 0, s5);                              // lqc2 vf2, 0(s5)
  c->vsub(DEST::xyzw, vf2, vf2, vf1);               // vsub.xyzw vf2, vf2, vf1
  c->vmul(DEST::xyzw, vf3, vf2, vf2);               // vmul.xyzw vf3, vf2, vf2
  c->vadd_bc(DEST::x, BC::y, vf3, vf3, vf3);        // vaddy.x vf3, vf3, vf3
  c->vadd_bc(DEST::x, BC::z, vf3, vf3, vf3);        // vaddz.x vf3, vf3, vf3
  c->vrsqrt(vf0, BC::w, vf3, BC::x);                // vrsqrt Q, vf0.w, vf3.x
  c->mov128_gpr_vf(v1, vf3);                        // qmfc2.i v1, vf3
  c->mtc1(f30, v1);                                 // mtc1 f30, v1
  c->sqrts(f30, f30);                               // sqrt.s f30, f30
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyzw, vf2, vf2);                   // vmulq.xyzw vf2, vf2, Q
  c->sqc2(vf2, 0, s2);                              // sqc2 vf2, 0(s2)
  c->mfc1(t1, f30);                                 // mfc1 t1, f30
  c->load_symbol2(t9, cache.ray_cylinder_intersect); // lw t9, ray-cylinder-intersect(s7)
  c->daddiu(a2, s5, 32);                            // daddiu a2, s5, 32
  c->mov64(t2, gp);                                 // or t2, gp, r0
  c->mov64(a0, s4);                                 // or a0, s4, r0
  c->mov64(a1, s3);                                 // or a1, s3, r0
  c->mov64(a3, s2);                                 // or a3, s2, r0
  c->mov64(t0, s1);                                 // or t0, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->mtc1(f30, v0);                                 // mtc1 f30, v0
  cop1_bc = c->fprs[f30] < c->fprs[f28];            // c.lt.s f30, f28
  bc = cop1_bc;                                     // bc1t L22
  cop1_bc = c->fprs[f30] < c->fprs[f31];            // c.lt.s f30, f31
  if (bc) {goto block_17;}                          // branch non-likely

  bc = !cop1_bc;                                    // bc1f L22
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_17;}                          // branch non-likely

  c->mtc1(f31, v0);                                 // mtc1 f31, v0
  c->lqc2(vf31, 0, gp);                             // lqc2 vf31, 0(gp)

  block_17:
  c->lui(v1, 16256);                                // lui v1, 16256
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lui(v1, -13122);                               // lui v1, -13122
  c->ori(v1, v1, 48160);                            // ori v1, v1, 48160
  cop1_bc = c->fprs[f0] < c->fprs[f31];             // c.lt.s f0, f31
  if (cop1_bc) {                                    // bc1tl L23
    c->mtc1(f31, v1);                               // mtc1 f31, v1
    goto block_19;
  }

  block_19:
  c->mfc1(v0, f31);                                 // mfc1 v0, f31
  c->sqc2(vf31, 0, gp);                             // sqc2 vf31, 0(gp)
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lwc1(f30, 132, sp);                            // lwc1 f30, 132(sp)
  c->lwc1(f28, 128, sp);                            // lwc1 f28, 128(sp)
  c->lq(gp, 112, sp);                               // lq gp, 112(sp)
  c->lq(s5, 96, sp);                                // lq s5, 96(sp)
  c->lq(s4, 80, sp);                                // lq s4, 80(sp)
  c->lq(s3, 64, sp);                                // lq s3, 64(sp)
  c->lq(s2, 48, sp);                                // lq s2, 48(sp)
  c->lq(s1, 32, sp);                                // lq s1, 32(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 144);                           // daddiu sp, sp, 144
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.ray_cylinder_intersect = intern_from_c("ray-cylinder-intersect").c();
  cache.ray_sphere_intersect = intern_from_c("ray-sphere-intersect").c();
  gLinkedFunctionTable.reg("collide-do-primitives", execute, 512);
}

} // namespace collide_do_primitives
} // namespace Mips2C
