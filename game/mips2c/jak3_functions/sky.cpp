
#include "sky.h"

#include "game/kernel/jak3/kscheme.h"
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak3 {

ExecutionContext sky_regs_vfs;

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
}  // namespace Mips2C::jak3

namespace Mips2C::jak3 {
namespace set_tex_offset {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  c->daddiu(sp, sp, -32);      // daddiu sp, sp, -32
  c->daddiu(v1, sp, 16);       // daddiu v1, sp, 16
  c->sq(r0, 0, v1);            // sq r0, 0(v1)
  c->lui(a2, 14208);           // lui a2, 14208
  c->mtc1(f0, a2);             // mtc1 f0, a2
  c->mtc1(f1, a0);             // mtc1 f1, a0
  c->cvtsw(f1, f1);            // cvt.s.w f1, f1
  c->muls(f0, f0, f1);         // mul.s f0, f0, f1
  c->swc1(f0, 0, v1);          // swc1 f0, 0(v1)
  c->lui(a0, 14208);           // lui a0, 14208
  c->mtc1(f0, a0);             // mtc1 f0, a0
  c->mtc1(f1, a1);             // mtc1 f1, a1
  c->cvtsw(f1, f1);            // cvt.s.w f1, f1
  c->muls(f0, f0, f1);         // mul.s f0, f0, f1
  c->swc1(f0, 4, v1);          // swc1 f0, 4(v1)
  c->mtc1(f0, r0);             // mtc1 f0, r0
  c->swc1(f0, 8, v1);          // swc1 f0, 8(v1)
  c->mtc1(f0, r0);             // mtc1 f0, r0
  c->swc1(f0, 12, v1);         // swc1 f0, 12(v1)
  c->lqc2(vf24, 0, v1);        // lqc2 vf24, 0(v1)
  c->mov128_gpr_vf(v1, vf24);  // qmfc2.i v1, vf24
  c->gprs[v0].du64[0] = 0;     // or v0, r0, r0
  // jr ra                                           // jr ra
  c->daddiu(sp, sp, 32);  // daddiu sp, sp, 32
  goto end_of_function;   // return

end_of_function:
  memcpy(&sky_regs_vfs.vfs[vf24].f[0], c->gprs[v1].du32, 16);
  return c->gprs[v0].du64[0];
}

void link() {
  gLinkedFunctionTable.reg("set-tex-offset", execute, 16);
}

}  // namespace set_tex_offset
}  // namespace Mips2C::jak3
using ::jak3::intern_from_c;

namespace Mips2C::jak3 {
namespace draw_large_polygon {
struct Cache {
  void* clip_polygon_against_negative_hyperplane;  // clip-polygon-against-negative-hyperplane
  void* clip_polygon_against_positive_hyperplane;  // clip-polygon-against-positive-hyperplane
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  [[maybe_unused]] u32 call_addr = 0;
  // nop                                            // sll r0, r0, 0
  c->daddiu(sp, sp, -8);  // daddiu sp, sp, -8
  // nop                                            // sll r0, r0, 0
  c->sd(ra, 0, sp);  // sd ra, 0(sp)
  c->load_symbol2(
      t9,
      cache
          .clip_polygon_against_positive_hyperplane);  // lw t9,
                                                       // clip-polygon-against-positive-hyperplane(s7)
  c->mov64(a2, t4);                 // or a2, t4, r0
  c->mov64(a3, t5);                 // or a3, t5, r0
  call_addr = c->gprs[t9].du32[0];  // function call:
  c->daddu(t2, a2, r0);             // daddu t2, a2, r0
  // c->jalr(call_addr);                               // jalr ra, t9
  clip_polygon_against_positive_hyperplane::execute(ctxt);
  bc = c->sgpr64(t0) == 0;  // beq t0, r0, L126
  // nop                                            // sll r0, r0, 0
  if (bc) {
    goto block_7;
  }  // branch non-likely

  c->mov64(a2, t5);                 // or a2, t5, r0
  c->mov64(a3, t4);                 // or a3, t4, r0
  call_addr = c->gprs[t9].du32[0];  // function call:
  c->daddiu(t2, a2, 4);             // daddiu t2, a2, 4
  // c->jalr(call_addr);                               // jalr ra, t9
  clip_polygon_against_positive_hyperplane::execute(ctxt);
  bc = c->sgpr64(t0) == 0;  // beq t0, r0, L126
  c->load_symbol2(
      t9,
      cache
          .clip_polygon_against_negative_hyperplane);  // lw t9,
                                                       // clip-polygon-against-negative-hyperplane(s7)
  if (bc) {
    goto block_7;
  }  // branch non-likely

  c->mov64(a2, t4);                 // or a2, t4, r0
  c->mov64(a3, t5);                 // or a3, t5, r0
  call_addr = c->gprs[t9].du32[0];  // function call:
  c->daddu(t2, a2, r0);             // daddu t2, a2, r0
  // c->jalr(call_addr);                               // jalr ra, t9
  clip_polygon_against_negative_hyperplane::execute(ctxt);
  bc = c->sgpr64(t0) == 0;  // beq t0, r0, L126
  // nop                                            // sll r0, r0, 0
  if (bc) {
    goto block_7;
  }  // branch non-likely

  c->mov64(a2, t5);                 // or a2, t5, r0
  c->mov64(a3, t4);                 // or a3, t4, r0
  call_addr = c->gprs[t9].du32[0];  // function call:
  c->daddiu(t2, a2, 4);             // daddiu t2, a2, 4
  // c->jalr(call_addr);                               // jalr ra, t9
  clip_polygon_against_negative_hyperplane::execute(ctxt);
  bc = c->sgpr64(t0) == 0;  // beq t0, r0, L126
  c->lw(a3, 4, a1);         // lw a3, 4(a1)
  if (bc) {
    goto block_7;
  }  // branch non-likely

  c->mov64(a2, t4);  // or a2, t4, r0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf27, 0, a3);   // sqc2 vf27, 0(a3)
  c->daddiu(a3, a3, 16);  // daddiu a3, a3, 16
  c->sw(t0, -16, a3);     // sw t0, -16(a3)
  // nop                                            // sll r0, r0, 0

block_5:
  c->lqc2(vf1, 0, a2);  // lqc2 vf1, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 16, a2);  // lqc2 vf2, 16(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 32, a2);                 // lqc2 vf3, 32(a2)
  c->vdiv(vf0, BC::w, vf1, BC::w);      // vdiv Q, vf0.w, vf1.w
  c->vmul(DEST::xyzw, vf1, vf1, vf26);  // vmul.xyzw vf1, vf1, vf26
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf3, vf3);  // vftoi0.xyzw vf3, vf3
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyzw, vf2, vf2, vf24);  // vadd.xyzw vf2, vf2, vf24
  // nop                                            // sll r0, r0, 0
  c->vwaitq();  // vwaitq
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf1, vf1);              // vmulq.xyz vf1, vf1, Q
  c->sqc2(vf3, 16, a3);                       // sqc2 vf3, 16(a3)
  c->vmulq(DEST::xyzw, vf2, vf2);             // vmulq.xyzw vf2, vf2, Q
  c->daddiu(a2, a2, 48);                      // daddiu a2, a2, 48
  c->vadd(DEST::xyzw, vf1, vf1, vf25);        // vadd.xyzw vf1, vf1, vf25
  c->daddiu(a3, a3, 48);                      // daddiu a3, a3, 48
  c->vmul_bc(DEST::z, BC::z, vf1, vf1, vf0);  // vmulz.z vf1, vf1, vf0
  // nop                                            // sll r0, r0, 0
  c->vmini_bc(DEST::w, BC::z, vf1, vf1, vf13);  // vminiz.w vf1, vf1, vf13
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::z, BC::x, vf1, vf1, vf23);  // vaddx.z vf1, vf1, vf23
  // nop                                            // sll r0, r0, 0
  c->vmax_bc(DEST::w, BC::y, vf1, vf1, vf13);  // vmaxy.w vf1, vf1, vf13
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf2, -48, a3);            // sqc2 vf2, -48(a3)
  c->daddiu(t0, t0, -1);            // daddiu t0, t0, -1
  c->vftoi4(DEST::xyzw, vf1, vf1);  // vftoi4.xyzw vf1, vf1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t0) != 0;  // bne t0, r0, L125
  c->sqc2(vf1, -16, a3);    // sqc2 vf1, -16(a3)
  if (bc) {
    goto block_5;
  }  // branch non-likely

  c->sw(a3, 4, a1);  // sw a3, 4(a1)
  // nop                                            // sll r0, r0, 0
  c->ld(ra, 0, sp);      // ld ra, 0(sp)
  c->daddiu(v0, s7, 4);  // daddiu v0, s7, 4
  // jr ra                                           // jr ra
  c->daddiu(sp, sp, 8);  // daddiu sp, sp, 8
  goto end_of_function;  // return

block_7:
  c->ld(ra, 0, sp);  // ld ra, 0(sp)
  c->mov64(v0, s7);  // or v0, s7, r0
  // jr ra                                           // jr ra
  c->daddiu(sp, sp, 8);  // daddiu sp, sp, 8
  goto end_of_function;  // return

  // jr ra                                           // jr ra
  c->daddu(sp, sp, r0);  // daddu sp, sp, r0
  goto end_of_function;  // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.clip_polygon_against_negative_hyperplane =
      intern_from_c(-1, 0, "clip-polygon-against-negative-hyperplane").c();
  cache.clip_polygon_against_positive_hyperplane =
      intern_from_c(-1, 0, "clip-polygon-against-positive-hyperplane").c();
  gLinkedFunctionTable.reg("draw-large-polygon", execute, 1024);
}

}  // namespace draw_large_polygon
}  // namespace Mips2C::jak3

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
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
  bc = !cop1_bc;                                    // bc1f L119
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
  bc = !cop1_bc;                                    // bc1f L117
  c->lqc2(vf6, -16, a2);                            // lqc2 vf6, -16(a2)
  if (bc) {goto block_6;}                           // branch non-likely

  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L123
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
  bc = !cop1_bc;                                    // bc1f L118
  c->lqc2(vf3, -16, a2);                            // lqc2 vf3, -16(a2)
  if (bc) {goto block_8;}                           // branch non-likely

  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L115
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  //beq r0, r0, L123                                // beq r0, r0, L123
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
  c->daddiu(t6, s7, 4);                             // daddiu t6, s7, 4
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
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L120
  c->sqc2(vf9, -16, a3);                            // sqc2 vf9, -16(a3)
  if (bc) {goto block_11;}                          // branch non-likely

  //beq r0, r0, L123                                // beq r0, r0, L123
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
  c->daddiu(t6, s7, 4);                             // daddiu t6, s7, 4
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
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L123
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
  bc = cop1_bc;                                     // bc1t L121
  c->lqc2(vf6, -16, a2);                            // lqc2 vf6, -16(a2)
  if (bc) {goto block_14;}                          // branch non-likely

  c->sqc2(vf1, 0, a3);                              // sqc2 vf1, 0(a3)
  c->daddiu(a3, a3, 48);                            // daddiu a3, a3, 48
  c->sqc2(vf2, -32, a3);                            // sqc2 vf2, -32(a3)
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L123
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
  bc = cop1_bc;                                     // bc1t L122
  c->lqc2(vf3, -16, a2);                            // lqc2 vf3, -16(a2)
  if (bc) {goto block_16;}                          // branch non-likely

  c->sqc2(vf4, 0, a3);                              // sqc2 vf4, 0(a3)
  c->daddiu(a3, a3, 48);                            // daddiu a3, a3, 48
  c->sqc2(vf5, -32, a3);                            // sqc2 vf5, -32(a3)
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L119
  c->sqc2(vf6, -16, a3);                            // sqc2 vf6, -16(a3)
  if (bc) {goto block_9;}                           // branch non-likely

  //beq r0, r0, L123                                // beq r0, r0, L123
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
  c->daddiu(t6, s7, 4);                             // daddiu t6, s7, 4
  c->vmula_bc(DEST::xyzw, BC::w, vf6, vf0);         // vmulaw.xyzw acc, vf6, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf9, vf10);   // vmaddx.xyzw vf9, vf9, vf10
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf7, -48, a3);                            // sqc2 vf7, -48(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, -32, a3);                            // sqc2 vf8, -32(a3)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L116
  c->sqc2(vf9, -16, a3);                            // sqc2 vf9, -16(a3)
  if (bc) {goto block_3;}                           // branch non-likely

  //beq r0, r0, L123                                // beq r0, r0, L123
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
  c->daddiu(t6, s7, 4);                             // daddiu t6, s7, 4
  c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf9, vf10);   // vmaddx.xyzw vf9, vf9, vf10
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf7, -48, a3);                            // sqc2 vf7, -48(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, -32, a3);                            // sqc2 vf8, -32(a3)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L115
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
  gLinkedFunctionTable.reg("clip-polygon-against-positive-hyperplane", execute, 128);
}

} // namespace clip_polygon_against_positive_hyperplane
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
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
  bc = !cop1_bc;                                    // bc1f L109
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
  bc = !cop1_bc;                                    // bc1f L107
  c->lqc2(vf6, -16, a2);                            // lqc2 vf6, -16(a2)
  if (bc) {goto block_6;}                           // branch non-likely

  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L113
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
  bc = !cop1_bc;                                    // bc1f L108
  c->lqc2(vf3, -16, a2);                            // lqc2 vf3, -16(a2)
  if (bc) {goto block_8;}                           // branch non-likely

  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L105
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  //beq r0, r0, L113                                // beq r0, r0, L113
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
  c->daddiu(t6, s7, 4);                             // daddiu t6, s7, 4
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
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L110
  c->sqc2(vf9, -16, a3);                            // sqc2 vf9, -16(a3)
  if (bc) {goto block_11;}                          // branch non-likely

  //beq r0, r0, L113                                // beq r0, r0, L113
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
  c->daddiu(t6, s7, 4);                             // daddiu t6, s7, 4
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
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L113
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
  bc = cop1_bc;                                     // bc1t L111
  c->lqc2(vf6, -16, a2);                            // lqc2 vf6, -16(a2)
  if (bc) {goto block_14;}                          // branch non-likely

  c->sqc2(vf1, 0, a3);                              // sqc2 vf1, 0(a3)
  c->daddiu(a3, a3, 48);                            // daddiu a3, a3, 48
  c->sqc2(vf2, -32, a3);                            // sqc2 vf2, -32(a3)
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L113
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
  bc = cop1_bc;                                     // bc1t L112
  c->lqc2(vf3, -16, a2);                            // lqc2 vf3, -16(a2)
  if (bc) {goto block_16;}                          // branch non-likely

  c->sqc2(vf4, 0, a3);                              // sqc2 vf4, 0(a3)
  c->daddiu(a3, a3, 48);                            // daddiu a3, a3, 48
  c->sqc2(vf5, -32, a3);                            // sqc2 vf5, -32(a3)
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L109
  c->sqc2(vf6, -16, a3);                            // sqc2 vf6, -16(a3)
  if (bc) {goto block_9;}                           // branch non-likely

  //beq r0, r0, L113                                // beq r0, r0, L113
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
  c->daddiu(t6, s7, 4);                             // daddiu t6, s7, 4
  c->vmula_bc(DEST::xyzw, BC::w, vf6, vf0);         // vmulaw.xyzw acc, vf6, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf9, vf10);   // vmaddx.xyzw vf9, vf9, vf10
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf7, -48, a3);                            // sqc2 vf7, -48(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, -32, a3);                            // sqc2 vf8, -32(a3)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L106
  c->sqc2(vf9, -16, a3);                            // sqc2 vf9, -16(a3)
  if (bc) {goto block_3;}                           // branch non-likely

  //beq r0, r0, L113                                // beq r0, r0, L113
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
  c->daddiu(t6, s7, 4);                             // daddiu t6, s7, 4
  c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf9, vf10);   // vmaddx.xyzw vf9, vf9, vf10
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf7, -48, a3);                            // sqc2 vf7, -48(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, -32, a3);                            // sqc2 vf8, -32(a3)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L105
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
  gLinkedFunctionTable.reg("clip-polygon-against-negative-hyperplane", execute, 128);
}

} // namespace clip_polygon_against_negative_hyperplane
} // namespace Mips2C

namespace Mips2C::jak3 {
namespace render_sky_quad {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* math_camera; // *math-camera*
  void* draw_large_polygon; // draw-large-polygon
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  c->copy_vfs_from_other(&sky_regs_vfs);
  // bool bc = false;
  // u32 call_addr = 0;
  c->mov64(v1, a0);                                 // or v1, a0, r0
  c->load_symbol2(v1, cache.math_camera);           // lw v1, *math-camera*(s7)
  c->lqc2(vf14, 780, v1);                           // lqc2 vf14, 780(v1)
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
  c->load_symbol2(t9, cache.draw_large_polygon);    // lw t9, draw-large-polygon(s7)
  // Unknown instr: jr t9
  draw_large_polygon::execute(ctxt);
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
  cache.math_camera = intern_from_c(-1, 0, "*math-camera*").c();
  cache.draw_large_polygon = intern_from_c(-1, 0, "draw-large-polygon").c();
  gLinkedFunctionTable.reg("render-sky-quad", execute, 1024);
}

} // namespace render_sky_quad
} // namespace Mips2C

namespace Mips2C::jak3 {
namespace render_sky_tri {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* draw_large_polygon; // draw-large-polygon
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  c->copy_vfs_from_other(&sky_regs_vfs);
  // bool bc = false;
  // u32 call_addr = 0;
  c->mov64(v1, a0);                                 // or v1, a0, r0
  get_fake_spad_addr2(t4, cache.fake_scratchpad_data, 0, c);// lui t4, 28672
  c->ori(t4, t4, 12288);                            // ori t4, t4, 12288
  get_fake_spad_addr2(t5, cache.fake_scratchpad_data, 0, c);// lui t5, 28672
  c->ori(t5, t5, 14336);                            // ori t5, t5, 14336
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
  c->load_symbol2(t9, cache.draw_large_polygon);    // lw t9, draw-large-polygon(s7)
  // Unknown instr: jr t9
  draw_large_polygon::execute(ctxt);
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
  cache.draw_large_polygon = intern_from_c(-1, 0, "draw-large-polygon").c();
  gLinkedFunctionTable.reg("render-sky-tri", execute, 1024);
}

} // namespace render_sky_tri
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace method_17_sky_work {
struct Cache {
  void* math_camera; // *math-camera*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  c->copy_vfs_from_other(&sky_regs_vfs);
  bool bc = false;
  // u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -32);                           // daddiu sp, sp, -32
  c->load_symbol2(v1, cache.math_camera);           // lw v1, *math-camera*(s7)
  c->daddiu(a1, sp, 16);                            // daddiu a1, sp, 16
  c->sq(r0, 0, a1);                                 // sq r0, 0(a1)
  c->daddiu(a2, a0, 1600);                          // daddiu a2, a0, 1600
  c->mov64(a3, a1);                                 // or a3, a1, r0
  c->daddiu(t0, v1, 812);                           // daddiu t0, v1, 812
  c->lq(t0, 0, t0);                                 // lq t0, 0(t0)
  c->sq(t0, 0, a3);                                 // sq t0, 0(a3)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->lwc1(f1, 928, v1);                             // lwc1 f1, 928(v1)
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = !cop1_bc;                                    // bc1f L92
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  c->lui(a3, 17664);                                // lui a3, 17664
  c->ori(a3, a3, 4096);                             // ori a3, a3, 4096
  c->mtc1(f0, a3);                                  // mtc1 f0, a3
  c->swc1(f0, 4, a1);                               // swc1 f0, 4(a1)
  c->mfc1(a3, f0);                                  // mfc1 a3, f0
  //beq r0, r0, L93                                 // beq r0, r0, L93
  // nop                                            // sll r0, r0, 0
  goto block_3;                                     // branch always


block_2:
  c->lui(a3, 17663);                                // lui a3, 17663
  c->ori(a3, a3, 57344);                            // ori a3, a3, 57344
  c->mtc1(f0, a3);                                  // mtc1 f0, a3
  c->swc1(f0, 4, a1);                               // swc1 f0, 4(a1)
  c->mfc1(a3, f0);                                  // mfc1 a3, f0

block_3:
  c->daddiu(a3, a0, 1088);                          // daddiu a3, a0, 1088
  c->lwc1(f0, 908, v1);                             // lwc1 f0, 908(v1)
  c->swc1(f0, 0, a3);                               // swc1 f0, 0(a3)
  c->lwc1(f0, 128, v1);                             // lwc1 f0, 128(v1)
  c->swc1(f0, 4, a3);                               // swc1 f0, 4(a3)
  c->lwc1(f0, 124, v1);                             // lwc1 f0, 124(v1)
  c->swc1(f0, 8, a3);                               // swc1 f0, 8(a3)
  c->lui(t0, 16256);                                // lui t0, 16256
  c->mtc1(f0, t0);                                  // mtc1 f0, t0
  c->swc1(f0, 12, a3);                              // swc1 f0, 12(a3)
  c->lqc2(vf31, 0, a2);                             // lqc2 vf31, 0(a2)
  c->lqc2(vf30, 16, a2);                            // lqc2 vf30, 16(a2)
  c->lqc2(vf29, 32, a2);                            // lqc2 vf29, 32(a2)
  c->lqc2(vf28, 48, a2);                            // lqc2 vf28, 48(a2)
  c->lqc2(vf14, 780, v1);                           // lqc2 vf14, 780(v1)
  c->lqc2(vf26, 796, v1);                           // lqc2 vf26, 796(v1)
  c->lqc2(vf25, 0, a1);                             // lqc2 vf25, 0(a1)
  c->lqc2(vf13, 1088, a0);                          // lqc2 vf13, 1088(a0)
  c->vmul(DEST::xyzw, vf31, vf31, vf14);            // vmul.xyzw vf31, vf31, vf14
  c->vmul(DEST::xyzw, vf30, vf30, vf14);            // vmul.xyzw vf30, vf30, vf14
  c->vmul(DEST::xyzw, vf29, vf29, vf14);            // vmul.xyzw vf29, vf29, vf14
  c->vmul(DEST::xyzw, vf28, vf28, vf14);            // vmul.xyzw vf28, vf28, vf14
  c->vmove(DEST::z, vf25, vf0);                     // vmove.z vf25, vf0
  c->vmove(DEST::xyzw, vf24, vf0);                  // vmove.xyzw vf24, vf0
  c->vmove(DEST::xyzw, vf23, vf0);                  // vmove.xyzw vf23, vf0
  c->mov128_gpr_vf(v1, vf23);                       // qmfc2.i v1, vf23
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
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
  cache.math_camera = intern_from_c(-1, 0, "*math-camera*").c();
  gLinkedFunctionTable.reg("(method 17 sky-work)", execute, 64);
}

} // namespace method_17_sky_work
} // namespace Mips2C


using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace method_18_sky_work {
struct Cache {
  void* math_camera; // *math-camera*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  // bool bc = false;
  c->copy_vfs_from_other(&sky_regs_vfs);
  // u32 call_addr = 0;
  c->daddiu(sp, sp, -32);                           // daddiu sp, sp, -32
  c->load_symbol2(a1, cache.math_camera);           // lw a1, *math-camera*(s7)
  c->daddiu(v1, a0, 1600);                          // daddiu v1, a0, 1600
  c->lqc2(vf31, 0, v1);                             // lqc2 vf31, 0(v1)
  c->lqc2(vf30, 16, v1);                            // lqc2 vf30, 16(v1)
  c->lqc2(vf29, 32, v1);                            // lqc2 vf29, 32(v1)
  c->lqc2(vf28, 48, v1);                            // lqc2 vf28, 48(v1)
  c->lqc2(vf14, 780, a1);                           // lqc2 vf14, 780(a1)
  c->lqc2(vf25, 812, a1);                           // lqc2 vf25, 812(a1)
  c->lqc2(vf13, 1088, a0);                          // lqc2 vf13, 1088(a0)
  c->vmove(DEST::z, vf25, vf0);                     // vmove.z vf25, vf0
  c->vmove(DEST::xyzw, vf23, vf0);                  // vmove.xyzw vf23, vf0
  c->daddiu(v1, sp, 16);                            // daddiu v1, sp, 16
  c->sq(r0, 0, v1);                                 // sq r0, 0(v1)
  c->lui(a0, 16256);                                // lui a0, 16256
  c->mtc1(f0, a0);                                  // mtc1 f0, a0
  c->lui(a0, 17974);                                // lui a0, 17974
  c->ori(a0, a0, 2913);                             // ori a0, a0, 2913
  c->mtc1(f1, a0);                                  // mtc1 f1, a0
  c->lwc1(f2, 8, a1);                               // lwc1 f2, 8(a1)
  c->divs(f1, f1, f2);                              // div.s f1, f1, f2
  c->mins(f0, f0, f1);                              // min.s f0, f0, f1
  c->swc1(f0, 12, v1);                              // swc1 f0, 12(v1)
  c->lqc2(vf26, 0, v1);                             // lqc2 vf26, 0(v1)
  c->mov128_gpr_vf(v1, vf26);                       // qmfc2.i v1, vf26
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
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
  cache.math_camera = intern_from_c(-1, 0, "*math-camera*").c();
  gLinkedFunctionTable.reg("(method 18 sky-work)", execute, 64);
}

} // namespace method_18_sky_work
} // namespace Mips2C

namespace Mips2C::jak3 {
namespace method_29_sky_work {
struct Cache {
  void* adgif_shader_texture_simple; // adgif-shader<-texture-simple!
  void* lookup_texture_by_id_fast; // lookup-texture-by-id-fast
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->copy_vfs_from_other(&sky_regs_vfs);
  c->daddiu(sp, sp, -64);                           // daddiu sp, sp, -64
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s5, 32, sp);                                // sq s5, 32(sp)
  c->sq(gp, 48, sp);                                // sq gp, 48(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->sw(a1, 2044, gp);                              // sw a1, 2044(gp)
  c->load_symbol2(t9, cache.lookup_texture_by_id_fast);// lw t9, lookup-texture-by-id-fast(s7)
  c->lui(v1, 128);                                  // lui v1, 128
  c->ori(a0, v1, 768);                              // ori a0, v1, 768
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->sw(v0, 16, sp);                                // sw v0, 16(sp)
  c->lwu(v1, 16, sp);                               // lwu v1, 16(sp)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L62
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_14;}                          // branch non-likely

  c->lqc2(vf15, 1248, gp);                          // lqc2 vf15, 1248(gp)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf15);       // vmulax.xyzw acc, vf31, vf15
  c->lqc2(vf1, 224, gp);                            // lqc2 vf1, 224(gp)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf15);      // vmadday.xyzw acc, vf30, vf15
  c->lqc2(vf4, 240, gp);                            // lqc2 vf4, 240(gp)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf15);      // vmaddaz.xyzw acc, vf29, vf15
  c->lqc2(vf2, 864, gp);                            // lqc2 vf2, 864(gp)
  c->vmadd_bc(DEST::xyzw, BC::w, vf15, vf28, vf0);  // vmaddw.xyzw vf15, vf28, vf0
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf16, vf15, vf14);            // vmul.xyzw vf16, vf15, vf14
  c->lqc2(vf5, 880, gp);                            // lqc2 vf5, 880(gp)
  c->vdiv(vf0, BC::w, vf16, BC::w);                 // vdiv Q, vf0.w, vf16.w
  c->lqc2(vf8, 32, gp);                             // lqc2 vf8, 32(gp)
  c->lqc2(vf11, 48, gp);                            // lqc2 vf11, 48(gp)
  c->lqc2(vf3, 448, gp);                            // lqc2 vf3, 448(gp)
  c->lqc2(vf6, 464, gp);                            // lqc2 vf6, 464(gp)
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyz, vf15, vf15);                  // vmulq.xyz vf15, vf15, Q
  c->vmulq(DEST::xyzw, vf16, vf26);                 // vmulq.xyzw vf16, vf26, Q
  c->vmul_bc(DEST::xyzw, BC::w, vf1, vf1, vf16);    // vmulw.xyzw vf1, vf1, vf16
  c->vmul_bc(DEST::xyzw, BC::w, vf4, vf4, vf16);    // vmulw.xyzw vf4, vf4, vf16
  c->vadd(DEST::xyzw, vf15, vf15, vf25);            // vadd.xyzw vf15, vf15, vf25
  c->vadd(DEST::xyz, vf1, vf1, vf15);               // vadd.xyz vf1, vf1, vf15
  c->vadd(DEST::xyz, vf4, vf4, vf15);               // vadd.xyz vf4, vf4, vf15
  c->vftoi4(DEST::xyz, vf1, vf1);                   // vftoi4.xyz vf1, vf1
  c->vftoi4(DEST::xyzw, vf4, vf4);                  // vftoi4.xyzw vf4, vf4
  c->sqc2(vf1, 1728, gp);                           // sqc2 vf1, 1728(gp)
  c->sqc2(vf4, 1744, gp);                           // sqc2 vf4, 1744(gp)
  c->lwu(v1, 2044, gp);                             // lwu v1, 2044(gp)
  c->lwu(a3, 4, v1);                                // lwu a3, 4(v1)
  c->lw(v1, 1740, gp);                              // lw v1, 1740(gp)
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = !cop1_bc;                                    // bc1f L60
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  if (bc) {goto block_3;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0

block_3:
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(v1))) {// beql s7, v1, L61
    c->mov64(v1, v1);                               // or v1, v1, r0
    goto block_12;
  }

// block_5:
  c->lw(v1, 1728, gp);                              // lw v1, 1728(gp)
  c->ori(a0, r0, 36864);                            // ori a0, r0, 36864
  c->slt(v1, v1, a0);                               // slt v1, v1, a0
  c->daddiu(a0, s7, 4);                             // daddiu a0, s7, 4
  c->movz(a0, s7, v1);                              // movz a0, s7, v1
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(a0))) {// beql s7, a0, L61
    c->mov64(v1, a0);                               // or v1, a0, r0
    goto block_12;
  }

// block_7:
  c->lw(v1, 1732, gp);                              // lw v1, 1732(gp)
  c->ori(a0, r0, 36096);                            // ori a0, r0, 36096
  c->slt(v1, v1, a0);                               // slt v1, v1, a0
  c->daddiu(a0, s7, 4);                             // daddiu a0, s7, 4
  c->movz(a0, s7, v1);                              // movz a0, s7, v1
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(a0))) {// beql s7, a0, L61
    c->mov64(v1, a0);                               // or v1, a0, r0
    goto block_12;
  }

// block_9:
  c->addiu(v1, r0, 28672);                          // addiu v1, r0, 28672
  c->lw(a0, 1744, gp);                              // lw a0, 1744(gp)
  c->slt(v1, v1, a0);                               // slt v1, v1, a0
  c->daddiu(a0, s7, 4);                             // daddiu a0, s7, 4
  c->movz(a0, s7, v1);                              // movz a0, s7, v1
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(a0))) {// beql s7, a0, L61
    c->mov64(v1, a0);                               // or v1, a0, r0
    goto block_12;
  }

// block_11:
  c->addiu(v1, r0, 29440);                          // addiu v1, r0, 29440
  c->lw(a0, 1748, gp);                              // lw a0, 1748(gp)
  c->slt(a0, v1, a0);                               // slt a0, v1, a0
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  c->movz(v1, s7, a0);                              // movz v1, s7, a0

block_12:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L62
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_14;}                          // branch non-likely

  c->vsub(DEST::z, vf1, vf0, vf0);                  // vsub.z vf1, vf0, vf0
  c->vsub(DEST::z, vf4, vf0, vf0);                  // vsub.z vf4, vf0, vf0
  c->sqc2(vf8, 112, a3);                            // sqc2 vf8, 112(a3)
  c->sqc2(vf11, 128, a3);                           // sqc2 vf11, 128(a3)
  c->sqc2(vf3, 144, a3);                            // sqc2 vf3, 144(a3)
  c->sqc2(vf2, 160, a3);                            // sqc2 vf2, 160(a3)
  c->sqc2(vf1, 176, a3);                            // sqc2 vf1, 176(a3)
  c->sqc2(vf5, 192, a3);                            // sqc2 vf5, 192(a3)
  c->sqc2(vf4, 208, a3);                            // sqc2 vf4, 208(a3)
  c->sqc2(vf8, 224, a3);                            // sqc2 vf8, 224(a3)
  c->sqc2(vf11, 240, a3);                           // sqc2 vf11, 240(a3)
  c->sqc2(vf6, 256, a3);                            // sqc2 vf6, 256(a3)
  c->sqc2(vf2, 272, a3);                            // sqc2 vf2, 272(a3)
  c->sqc2(vf1, 288, a3);                            // sqc2 vf1, 288(a3)
  c->sqc2(vf5, 304, a3);                            // sqc2 vf5, 304(a3)
  c->sqc2(vf4, 320, a3);                            // sqc2 vf4, 320(a3)
  c->lwu(v1, 2044, gp);                             // lwu v1, 2044(gp)
  c->lwu(v1, 4, v1);                                // lwu v1, 4(v1)
  c->lq(a0, 0, gp);                                 // lq a0, 0(gp)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->lq(a0, 16, gp);                                // lq a0, 16(gp)
  c->sq(a0, 16, v1);                                // sq a0, 16(v1)
  c->daddiu(s5, v1, 32);                            // daddiu s5, v1, 32
  c->load_symbol2(t9, cache.adgif_shader_texture_simple);// lw t9, adgif-shader<-texture-simple!(s7)
  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->lwu(a1, 16, sp);                               // lwu a1, 16(sp)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->addiu(v1, r0, 72);                             // addiu v1, r0, 72
  c->sd(v1, 64, s5);                                // sd v1, 64(s5)
  c->lwu(v1, 2044, gp);                             // lwu v1, 2044(gp)
  c->lwu(v1, 4, v1);                                // lwu v1, 4(v1)
  c->daddiu(v1, v1, 336);                           // daddiu v1, v1, 336
  c->lwu(a0, 2044, gp);                             // lwu a0, 2044(gp)
  c->sw(v1, 4, a0);                                 // sw v1, 4(a0)

block_14:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 48, sp);                                // lq gp, 48(sp)
  c->lq(s5, 32, sp);                                // lq s5, 32(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 64);                            // daddiu sp, sp, 64
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  sky_regs_vfs.copy_vfs_from_other(c);
  return c->gprs[v0].du64[0];
}

void link() {
  cache.adgif_shader_texture_simple = intern_from_c(-1, 0, "adgif-shader<-texture-simple!").c();
  cache.lookup_texture_by_id_fast = intern_from_c(-1, 0, "lookup-texture-by-id-fast").c();
  gLinkedFunctionTable.reg("(method 29 sky-work)", execute, 128);
}

} // namespace method_29_sky_work
} // namespace Mips2C

namespace Mips2C::jak3 {
namespace method_30_sky_work {
struct Cache {
  void* adgif_shader_texture_simple; // adgif-shader<-texture-simple!
  void* lookup_texture_by_id_fast; // lookup-texture-by-id-fast
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->copy_vfs_from_other(&sky_regs_vfs);
  c->daddiu(sp, sp, -64);                           // daddiu sp, sp, -64
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s5, 32, sp);                                // sq s5, 32(sp)
  c->sq(gp, 48, sp);                                // sq gp, 48(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->sw(a1, 2044, gp);                              // sw a1, 2044(gp)
  c->load_symbol2(t9, cache.lookup_texture_by_id_fast);// lw t9, lookup-texture-by-id-fast(s7)
  c->lui(v1, 128);                                  // lui v1, 128
  c->ori(a0, v1, 768);                              // ori a0, v1, 768
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->sw(v0, 16, sp);                                // sw v0, 16(sp)
  c->lwu(v1, 16, sp);                               // lwu v1, 16(sp)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L58
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_14;}                          // branch non-likely

  c->lqc2(vf15, 1312, gp);                          // lqc2 vf15, 1312(gp)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf15);       // vmulax.xyzw acc, vf31, vf15
  c->lqc2(vf1, 256, gp);                            // lqc2 vf1, 256(gp)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf15);      // vmadday.xyzw acc, vf30, vf15
  c->lqc2(vf4, 272, gp);                            // lqc2 vf4, 272(gp)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf15);      // vmaddaz.xyzw acc, vf29, vf15
  c->lqc2(vf2, 864, gp);                            // lqc2 vf2, 864(gp)
  c->vmadd_bc(DEST::xyzw, BC::w, vf15, vf28, vf0);  // vmaddw.xyzw vf15, vf28, vf0
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf16, vf15, vf14);            // vmul.xyzw vf16, vf15, vf14
  c->lqc2(vf5, 880, gp);                            // lqc2 vf5, 880(gp)
  c->vdiv(vf0, BC::w, vf16, BC::w);                 // vdiv Q, vf0.w, vf16.w
  c->lqc2(vf8, 32, gp);                             // lqc2 vf8, 32(gp)
  c->lqc2(vf11, 48, gp);                            // lqc2 vf11, 48(gp)
  c->lqc2(vf3, 480, gp);                            // lqc2 vf3, 480(gp)
  c->lqc2(vf6, 496, gp);                            // lqc2 vf6, 496(gp)
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyz, vf15, vf15);                  // vmulq.xyz vf15, vf15, Q
  c->vmulq(DEST::xyzw, vf16, vf26);                 // vmulq.xyzw vf16, vf26, Q
  c->vmul_bc(DEST::xyzw, BC::w, vf1, vf1, vf16);    // vmulw.xyzw vf1, vf1, vf16
  c->vmul_bc(DEST::xyzw, BC::w, vf4, vf4, vf16);    // vmulw.xyzw vf4, vf4, vf16
  c->vadd(DEST::xyzw, vf15, vf15, vf25);            // vadd.xyzw vf15, vf15, vf25
  c->vadd(DEST::xyz, vf1, vf1, vf15);               // vadd.xyz vf1, vf1, vf15
  c->vadd(DEST::xyz, vf4, vf4, vf15);               // vadd.xyz vf4, vf4, vf15
  c->vftoi4(DEST::xyz, vf1, vf1);                   // vftoi4.xyz vf1, vf1
  c->vftoi4(DEST::xyzw, vf4, vf4);                  // vftoi4.xyzw vf4, vf4
  c->sqc2(vf1, 1728, gp);                           // sqc2 vf1, 1728(gp)
  c->sqc2(vf4, 1744, gp);                           // sqc2 vf4, 1744(gp)
  c->lwu(v1, 2044, gp);                             // lwu v1, 2044(gp)
  c->lwu(a3, 4, v1);                                // lwu a3, 4(v1)
  c->lw(v1, 1740, gp);                              // lw v1, 1740(gp)
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = !cop1_bc;                                    // bc1f L56
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  if (bc) {goto block_3;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0

block_3:
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(v1))) {// beql s7, v1, L57
    c->mov64(v1, v1);                               // or v1, v1, r0
    goto block_12;
  }

// block_5:
  c->lw(v1, 1728, gp);                              // lw v1, 1728(gp)
  c->ori(a0, r0, 36864);                            // ori a0, r0, 36864
  c->slt(v1, v1, a0);                               // slt v1, v1, a0
  c->daddiu(a0, s7, 4);                             // daddiu a0, s7, 4
  c->movz(a0, s7, v1);                              // movz a0, s7, v1
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(a0))) {// beql s7, a0, L57
    c->mov64(v1, a0);                               // or v1, a0, r0
    goto block_12;
  }

// block_7:
  c->lw(v1, 1732, gp);                              // lw v1, 1732(gp)
  c->ori(a0, r0, 36096);                            // ori a0, r0, 36096
  c->slt(v1, v1, a0);                               // slt v1, v1, a0
  c->daddiu(a0, s7, 4);                             // daddiu a0, s7, 4
  c->movz(a0, s7, v1);                              // movz a0, s7, v1
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(a0))) {// beql s7, a0, L57
    c->mov64(v1, a0);                               // or v1, a0, r0
    goto block_12;
  }

// block_9:
  c->addiu(v1, r0, 28672);                          // addiu v1, r0, 28672
  c->lw(a0, 1744, gp);                              // lw a0, 1744(gp)
  c->slt(v1, v1, a0);                               // slt v1, v1, a0
  c->daddiu(a0, s7, 4);                             // daddiu a0, s7, 4
  c->movz(a0, s7, v1);                              // movz a0, s7, v1
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(a0))) {// beql s7, a0, L57
    c->mov64(v1, a0);                               // or v1, a0, r0
    goto block_12;
  }

// block_11:
  c->addiu(v1, r0, 29440);                          // addiu v1, r0, 29440
  c->lw(a0, 1748, gp);                              // lw a0, 1748(gp)
  c->slt(a0, v1, a0);                               // slt a0, v1, a0
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  c->movz(v1, s7, a0);                              // movz v1, s7, a0

block_12:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L58
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_14;}                          // branch non-likely

  c->vsub(DEST::z, vf1, vf0, vf0);                  // vsub.z vf1, vf0, vf0
  c->vsub(DEST::z, vf4, vf0, vf0);                  // vsub.z vf4, vf0, vf0
  c->sqc2(vf8, 112, a3);                            // sqc2 vf8, 112(a3)
  c->sqc2(vf11, 128, a3);                           // sqc2 vf11, 128(a3)
  c->sqc2(vf3, 144, a3);                            // sqc2 vf3, 144(a3)
  c->sqc2(vf2, 160, a3);                            // sqc2 vf2, 160(a3)
  c->sqc2(vf1, 176, a3);                            // sqc2 vf1, 176(a3)
  c->sqc2(vf5, 192, a3);                            // sqc2 vf5, 192(a3)
  c->sqc2(vf4, 208, a3);                            // sqc2 vf4, 208(a3)
  c->sqc2(vf8, 224, a3);                            // sqc2 vf8, 224(a3)
  c->sqc2(vf11, 240, a3);                           // sqc2 vf11, 240(a3)
  c->sqc2(vf6, 256, a3);                            // sqc2 vf6, 256(a3)
  c->sqc2(vf2, 272, a3);                            // sqc2 vf2, 272(a3)
  c->sqc2(vf1, 288, a3);                            // sqc2 vf1, 288(a3)
  c->sqc2(vf5, 304, a3);                            // sqc2 vf5, 304(a3)
  c->sqc2(vf4, 320, a3);                            // sqc2 vf4, 320(a3)
  c->lwu(v1, 2044, gp);                             // lwu v1, 2044(gp)
  c->lwu(v1, 4, v1);                                // lwu v1, 4(v1)
  c->lq(a0, 0, gp);                                 // lq a0, 0(gp)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->lq(a0, 16, gp);                                // lq a0, 16(gp)
  c->sq(a0, 16, v1);                                // sq a0, 16(v1)
  c->daddiu(s5, v1, 32);                            // daddiu s5, v1, 32
  c->load_symbol2(t9, cache.adgif_shader_texture_simple);// lw t9, adgif-shader<-texture-simple!(s7)
  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->lwu(a1, 16, sp);                               // lwu a1, 16(sp)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->addiu(v1, r0, 72);                             // addiu v1, r0, 72
  c->sd(v1, 64, s5);                                // sd v1, 64(s5)
  c->lwu(v1, 2044, gp);                             // lwu v1, 2044(gp)
  c->lwu(v1, 4, v1);                                // lwu v1, 4(v1)
  c->daddiu(v1, v1, 336);                           // daddiu v1, v1, 336
  c->lwu(a0, 2044, gp);                             // lwu a0, 2044(gp)
  c->sw(v1, 4, a0);                                 // sw v1, 4(a0)

block_14:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 48, sp);                                // lq gp, 48(sp)
  c->lq(s5, 32, sp);                                // lq s5, 32(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 64);                            // daddiu sp, sp, 64
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  sky_regs_vfs.copy_vfs_from_other(c);
  return c->gprs[v0].du64[0];
}

void link() {
  cache.adgif_shader_texture_simple = intern_from_c(-1, 0, "adgif-shader<-texture-simple!").c();
  cache.lookup_texture_by_id_fast = intern_from_c(-1, 0, "lookup-texture-by-id-fast").c();
  gLinkedFunctionTable.reg("(method 30 sky-work)", execute, 128);
}

} // namespace method_30_sky_work
} // namespace Mips2C


namespace Mips2C::jak3 {
namespace method_31_sky_work {
struct Cache {
  void* adgif_shader_texture_simple; // adgif-shader<-texture-simple!
  void* lookup_texture_by_id_fast; // lookup-texture-by-id-fast
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->copy_vfs_from_other(&sky_regs_vfs);

  c->daddiu(sp, sp, -80);                           // daddiu sp, sp, -80
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s4, 32, sp);                                // sq s4, 32(sp)
  c->sq(s5, 48, sp);                                // sq s5, 48(sp)
  c->sq(gp, 64, sp);                                // sq gp, 64(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->sw(a1, 2044, gp);                              // sw a1, 2044(gp)
  c->load_symbol2(t9, cache.lookup_texture_by_id_fast);// lw t9, lookup-texture-by-id-fast(s7)
  c->lui(v1, 128);                                  // lui v1, 128
  c->ori(a0, v1, 768);                              // ori a0, v1, 768
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->sw(v0, 16, sp);                                // sw v0, 16(sp)
  c->load_symbol2(t9, cache.lookup_texture_by_id_fast);// lw t9, lookup-texture-by-id-fast(s7)
  c->lui(v1, 128);                                  // lui v1, 128
  c->ori(a0, v1, 512);                              // ori a0, v1, 512
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->sw(v0, 20, sp);                                // sw v0, 20(sp)
  c->lwu(v1, 16, sp);                               // lwu v1, 16(sp)
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(v1))) {// beql s7, v1, L51
    c->mov64(v1, v1);                               // or v1, v1, r0
    goto block_3;
  }

// block_2:
  c->lwu(v1, 20, sp);                               // lwu v1, 20(sp)

block_3:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L54
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_17;}                          // branch non-likely

  c->lqc2(vf15, 1376, gp);                          // lqc2 vf15, 1376(gp)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf15);       // vmulax.xyzw acc, vf31, vf15
  c->lqc2(vf1, 288, gp);                            // lqc2 vf1, 288(gp)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf15);      // vmadday.xyzw acc, vf30, vf15
  c->lqc2(vf4, 304, gp);                            // lqc2 vf4, 304(gp)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf15);      // vmaddaz.xyzw acc, vf29, vf15
  c->lqc2(vf2, 864, gp);                            // lqc2 vf2, 864(gp)
  c->vmadd_bc(DEST::xyzw, BC::w, vf15, vf28, vf0);  // vmaddw.xyzw vf15, vf28, vf0
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf16, vf15, vf14);            // vmul.xyzw vf16, vf15, vf14
  c->lqc2(vf5, 880, gp);                            // lqc2 vf5, 880(gp)
  c->vdiv(vf0, BC::w, vf16, BC::w);                 // vdiv Q, vf0.w, vf16.w
  c->lqc2(vf8, 32, gp);                             // lqc2 vf8, 32(gp)
  c->lqc2(vf11, 48, gp);                            // lqc2 vf11, 48(gp)
  c->lqc2(vf7, 320, gp);                            // lqc2 vf7, 320(gp)
  c->lqc2(vf10, 336, gp);                           // lqc2 vf10, 336(gp)
  c->lqc2(vf17, 352, gp);                           // lqc2 vf17, 352(gp)
  c->lqc2(vf18, 368, gp);                           // lqc2 vf18, 368(gp)
  c->lqc2(vf3, 512, gp);                            // lqc2 vf3, 512(gp)
  c->lqc2(vf6, 528, gp);                            // lqc2 vf6, 528(gp)
  c->lqc2(vf9, 544, gp);                            // lqc2 vf9, 544(gp)
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyz, vf15, vf15);                  // vmulq.xyz vf15, vf15, Q
  c->vmulq(DEST::xyzw, vf16, vf26);                 // vmulq.xyzw vf16, vf26, Q
  c->vmul_bc(DEST::xyzw, BC::w, vf1, vf1, vf16);    // vmulw.xyzw vf1, vf1, vf16
  c->vmul_bc(DEST::xyzw, BC::w, vf4, vf4, vf16);    // vmulw.xyzw vf4, vf4, vf16
  c->vmul_bc(DEST::xyzw, BC::w, vf7, vf7, vf16);    // vmulw.xyzw vf7, vf7, vf16
  c->vmul_bc(DEST::xyzw, BC::w, vf10, vf10, vf16);  // vmulw.xyzw vf10, vf10, vf16
  c->vmul_bc(DEST::xyzw, BC::w, vf17, vf17, vf16);  // vmulw.xyzw vf17, vf17, vf16
  c->vmul_bc(DEST::xyzw, BC::w, vf18, vf18, vf16);  // vmulw.xyzw vf18, vf18, vf16
  c->vadd(DEST::xyzw, vf15, vf15, vf25);            // vadd.xyzw vf15, vf15, vf25
  c->vadd(DEST::xyz, vf1, vf1, vf15);               // vadd.xyz vf1, vf1, vf15
  c->vadd(DEST::xyz, vf4, vf4, vf15);               // vadd.xyz vf4, vf4, vf15
  c->vadd(DEST::xyz, vf7, vf7, vf15);               // vadd.xyz vf7, vf7, vf15
  c->vadd(DEST::xyz, vf10, vf10, vf15);             // vadd.xyz vf10, vf10, vf15
  c->vadd(DEST::xyz, vf17, vf17, vf15);             // vadd.xyz vf17, vf17, vf15
  c->vadd(DEST::xyz, vf18, vf18, vf15);             // vadd.xyz vf18, vf18, vf15
  c->vftoi4(DEST::xyzw, vf1, vf1);                  // vftoi4.xyzw vf1, vf1
  c->vftoi4(DEST::xyzw, vf4, vf4);                  // vftoi4.xyzw vf4, vf4
  c->vftoi4(DEST::xyz, vf7, vf7);                   // vftoi4.xyz vf7, vf7
  c->vftoi4(DEST::xyzw, vf10, vf10);                // vftoi4.xyzw vf10, vf10
  c->vftoi4(DEST::xyzw, vf17, vf17);                // vftoi4.xyzw vf17, vf17
  c->vftoi4(DEST::xyzw, vf18, vf18);                // vftoi4.xyzw vf18, vf18
  c->sqc2(vf7, 1728, gp);                           // sqc2 vf7, 1728(gp)
  c->sqc2(vf10, 1744, gp);                          // sqc2 vf10, 1744(gp)
  c->lwu(v1, 2044, gp);                             // lwu v1, 2044(gp)
  c->lwu(a3, 4, v1);                                // lwu a3, 4(v1)
  c->lw(v1, 1740, gp);                              // lw v1, 1740(gp)
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = !cop1_bc;                                    // bc1f L52
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  if (bc) {goto block_6;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0

block_6:
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(v1))) {// beql s7, v1, L53
    c->mov64(v1, v1);                               // or v1, v1, r0
    goto block_15;
  }

// block_8:
  c->lw(v1, 1728, gp);                              // lw v1, 1728(gp)
  c->ori(a0, r0, 36864);                            // ori a0, r0, 36864
  c->slt(v1, v1, a0);                               // slt v1, v1, a0
  c->daddiu(a0, s7, 4);                             // daddiu a0, s7, 4
  c->movz(a0, s7, v1);                              // movz a0, s7, v1
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(a0))) {// beql s7, a0, L53
    c->mov64(v1, a0);                               // or v1, a0, r0
    goto block_15;
  }

// block_10:
  c->lw(v1, 1732, gp);                              // lw v1, 1732(gp)
  c->ori(a0, r0, 36096);                            // ori a0, r0, 36096
  c->slt(v1, v1, a0);                               // slt v1, v1, a0
  c->daddiu(a0, s7, 4);                             // daddiu a0, s7, 4
  c->movz(a0, s7, v1);                              // movz a0, s7, v1
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(a0))) {// beql s7, a0, L53
    c->mov64(v1, a0);                               // or v1, a0, r0
    goto block_15;
  }

// block_12:
  c->addiu(v1, r0, 28672);                          // addiu v1, r0, 28672
  c->lw(a0, 1744, gp);                              // lw a0, 1744(gp)
  c->slt(v1, v1, a0);                               // slt v1, v1, a0
  c->daddiu(a0, s7, 4);                             // daddiu a0, s7, 4
  c->movz(a0, s7, v1);                              // movz a0, s7, v1
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(a0))) {// beql s7, a0, L53
    c->mov64(v1, a0);                               // or v1, a0, r0
    goto block_15;
  }

// block_14:
  c->addiu(v1, r0, 29440);                          // addiu v1, r0, 29440
  c->lw(a0, 1748, gp);                              // lw a0, 1748(gp)
  c->slt(a0, v1, a0);                               // slt a0, v1, a0
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  c->movz(v1, s7, a0);                              // movz v1, s7, a0

block_15:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L54
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_17;}                          // branch non-likely

  c->vsub(DEST::z, vf1, vf0, vf0);                  // vsub.z vf1, vf0, vf0
  c->vsub(DEST::z, vf4, vf0, vf0);                  // vsub.z vf4, vf0, vf0
  c->vsub(DEST::z, vf7, vf0, vf0);                  // vsub.z vf7, vf0, vf0
  c->vsub(DEST::z, vf10, vf0, vf0);                 // vsub.z vf10, vf0, vf0
  c->vsub(DEST::z, vf17, vf0, vf0);                 // vsub.z vf17, vf0, vf0
  c->vsub(DEST::z, vf18, vf0, vf0);                 // vsub.z vf18, vf0, vf0
  c->sqc2(vf8, 112, a3);                            // sqc2 vf8, 112(a3)
  c->sqc2(vf11, 128, a3);                           // sqc2 vf11, 128(a3)
  c->sqc2(vf3, 144, a3);                            // sqc2 vf3, 144(a3)
  c->sqc2(vf2, 160, a3);                            // sqc2 vf2, 160(a3)
  c->sqc2(vf1, 176, a3);                            // sqc2 vf1, 176(a3)
  c->sqc2(vf5, 192, a3);                            // sqc2 vf5, 192(a3)
  c->sqc2(vf4, 208, a3);                            // sqc2 vf4, 208(a3)
  c->sqc2(vf8, 224, a3);                            // sqc2 vf8, 224(a3)
  c->sqc2(vf11, 240, a3);                           // sqc2 vf11, 240(a3)
  c->sqc2(vf6, 256, a3);                            // sqc2 vf6, 256(a3)
  c->sqc2(vf2, 272, a3);                            // sqc2 vf2, 272(a3)
  c->sqc2(vf7, 288, a3);                            // sqc2 vf7, 288(a3)
  c->sqc2(vf5, 304, a3);                            // sqc2 vf5, 304(a3)
  c->sqc2(vf10, 320, a3);                           // sqc2 vf10, 320(a3)
  c->sqc2(vf8, 448, a3);                            // sqc2 vf8, 448(a3)
  c->sqc2(vf11, 464, a3);                           // sqc2 vf11, 464(a3)
  c->sqc2(vf9, 480, a3);                            // sqc2 vf9, 480(a3)
  c->sqc2(vf2, 496, a3);                            // sqc2 vf2, 496(a3)
  c->sqc2(vf17, 512, a3);                           // sqc2 vf17, 512(a3)
  c->sqc2(vf5, 528, a3);                            // sqc2 vf5, 528(a3)
  c->sqc2(vf18, 544, a3);                           // sqc2 vf18, 544(a3)
  c->lwu(v1, 2044, gp);                             // lwu v1, 2044(gp)
  c->lwu(s5, 4, v1);                                // lwu s5, 4(v1)
  c->lq(v1, 0, gp);                                 // lq v1, 0(gp)
  c->sq(v1, 0, s5);                                 // sq v1, 0(s5)
  c->lq(v1, 16, gp);                                // lq v1, 16(gp)
  c->sq(v1, 16, s5);                                // sq v1, 16(s5)
  c->daddiu(s4, s5, 32);                            // daddiu s4, s5, 32
  c->load_symbol2(t9, cache.adgif_shader_texture_simple);// lw t9, adgif-shader<-texture-simple!(s7)
  c->mov64(a0, s4);                                 // or a0, s4, r0
  c->lwu(a1, 16, sp);                               // lwu a1, 16(sp)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->addiu(v1, r0, 72);                             // addiu v1, r0, 72
  c->sd(v1, 64, s4);                                // sd v1, 64(s4)
  c->lq(v1, 0, gp);                                 // lq v1, 0(gp)
  c->sq(v1, 336, s5);                               // sq v1, 336(s5)
  c->lq(v1, 16, gp);                                // lq v1, 16(gp)
  c->sq(v1, 352, s5);                               // sq v1, 352(s5)
  c->daddiu(s5, s5, 368);                           // daddiu s5, s5, 368
  c->load_symbol2(t9, cache.adgif_shader_texture_simple);// lw t9, adgif-shader<-texture-simple!(s7)
  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->lwu(a1, 20, sp);                               // lwu a1, 20(sp)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->addiu(v1, r0, 68);                             // addiu v1, r0, 68
  c->sd(v1, 64, s5);                                // sd v1, 64(s5)
  c->lwu(v1, 2044, gp);                             // lwu v1, 2044(gp)
  c->lwu(v1, 4, v1);                                // lwu v1, 4(v1)
  c->daddiu(v1, v1, 560);                           // daddiu v1, v1, 560
  c->lwu(a0, 2044, gp);                             // lwu a0, 2044(gp)
  c->sw(v1, 4, a0);                                 // sw v1, 4(a0)

block_17:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 64, sp);                                // lq gp, 64(sp)
  c->lq(s5, 48, sp);                                // lq s5, 48(sp)
  c->lq(s4, 32, sp);                                // lq s4, 32(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 80);                            // daddiu sp, sp, 80
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  sky_regs_vfs.copy_vfs_from_other(c);
  return c->gprs[v0].du64[0];
}

void link() {
  cache.adgif_shader_texture_simple = intern_from_c(-1, 0, "adgif-shader<-texture-simple!").c();
  cache.lookup_texture_by_id_fast = intern_from_c(-1, 0, "lookup-texture-by-id-fast").c();
  gLinkedFunctionTable.reg("(method 31 sky-work)", execute, 128);
}

} // namespace method_31_sky_work
} // namespace Mips2C

namespace Mips2C::jak3 {
namespace method_34_sky_work {
struct Cache {
  void* vector_normalize; // vector-normalize!
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  c->copy_vfs_from_other(&sky_regs_vfs);
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -48);                           // daddiu sp, sp, -48
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s5, 16, sp);                                // sq s5, 16(sp)
  c->sq(gp, 32, sp);                                // sq gp, 32(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->addiu(a1, r0, 0);                              // addiu a1, r0, 0
  c->mov64(v1, gp);                                 // or v1, gp, r0
  c->lqc2(vf31, 1664, gp);                          // lqc2 vf31, 1664(gp)
  c->lqc2(vf30, 1680, gp);                          // lqc2 vf30, 1680(gp)
  c->lqc2(vf29, 1696, gp);                          // lqc2 vf29, 1696(gp)
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  //beq r0, r0, L43                                 // beq r0, r0, L43
  // nop                                            // sll r0, r0, 0
  goto block_10;                                    // branch always


block_1:
  c->daddiu(a2, a1, 3);                             // daddiu a2, a1, 3
  c->daddiu(t0, a1, 5);                             // daddiu t0, a1, 5
  c->daddiu(a3, a1, 7);                             // daddiu a3, a1, 7
  c->andi(a2, a2, 7);                               // andi a2, a2, 7
  c->andi(t1, t0, 7);                               // andi t1, t0, 7
  c->andi(t2, a3, 7);                               // andi t2, a3, 7
  c->dsll(a3, a1, 4);                               // dsll a3, a1, 4
  c->dsll(t0, a2, 4);                               // dsll t0, a2, 4
  c->dsll(t1, t1, 4);                               // dsll t1, t1, 4
  c->dsll(a2, t2, 4);                               // dsll a2, t2, 4
  c->daddu(t3, a3, gp);                             // daddu t3, a3, gp
  c->daddu(t2, t0, gp);                             // daddu t2, t0, gp
  c->daddu(a3, t1, gp);                             // daddu a3, t1, gp
  c->lq(t0, 896, t3);                               // lq t0, 896(t3)
  c->daddu(a2, a2, gp);                             // daddu a2, a2, gp
  c->lq(t1, 896, t2);                               // lq t1, 896(t2)
  c->daddiu(a1, a1, 1);                             // daddiu a1, a1, 1
  c->lq(a3, 896, a3);                               // lq a3, 896(a3)
  c->paddb(t0, t0, t1);                             // paddb t0, t0, t1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->paddb(a3, t0, a3);                             // paddb a3, t0, a3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->andi(a1, a1, 7);                               // andi a1, a1, 7
  c->sq(a3, 896, a2);                               // sq a3, 896(a2)
  c->pextlb(t0, a3, r0);                            // pextlb t0, a3, r0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextub(a2, a3, r0);                            // pextub a2, a3, r0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextuh(a3, t0, r0);                            // pextuh a3, t0, r0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(t0, t0, r0);                            // pextlh t0, t0, r0
  c->mov128_vf_gpr(vf15, a3);                       // qmtc2.i vf15, a3
  c->pextuh(a3, a2, r0);                            // pextuh a3, a2, r0
  c->mov128_vf_gpr(vf16, t0);                       // qmtc2.i vf16, t0
  c->pextlh(a2, a2, r0);                            // pextlh a2, a2, r0
  c->mov128_vf_gpr(vf17, a3);                       // qmtc2.i vf17, a3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf18, a2);                       // qmtc2.i vf18, a2
  c->vitof15(DEST::xyzw, vf15, vf15);               // vitof15.xyzw vf15, vf15
  // nop                                            // sll r0, r0, 0
  c->vitof15(DEST::xyzw, vf16, vf16);               // vitof15.xyzw vf16, vf16
  // nop                                            // sll r0, r0, 0
  c->vitof15(DEST::xyzw, vf17, vf17);               // vitof15.xyzw vf17, vf17
  // nop                                            // sll r0, r0, 0
  c->vitof15(DEST::xyzw, vf18, vf18);               // vitof15.xyzw vf18, vf18
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf15);       // vmulax.xyzw acc, vf31, vf15
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf15);      // vmadday.xyzw acc, vf30, vf15
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf15, vf29, vf15); // vmaddz.xyzw vf15, vf29, vf15
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf16);       // vmulax.xyzw acc, vf31, vf16
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf16);      // vmadday.xyzw acc, vf30, vf16
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf16, vf29, vf16); // vmaddz.xyzw vf16, vf29, vf16
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf17);       // vmulax.xyzw acc, vf31, vf17
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf17);      // vmadday.xyzw acc, vf30, vf17
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf17, vf29, vf17); // vmaddz.xyzw vf17, vf29, vf17
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf18);       // vmulax.xyzw acc, vf31, vf18
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf18);      // vmadday.xyzw acc, vf30, vf18
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf18, vf29, vf18); // vmaddz.xyzw vf18, vf29, vf18
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a2, vf15);                       // qmfc2.i a2, vf15
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a3, vf16);                       // qmfc2.i a3, vf16
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t0, vf17);                       // qmfc2.i t0, vf17
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t1, vf18);                       // qmfc2.i t1, vf18
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(a2)) >= 0;                   // bgez a2, L39
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_3;}                           // branch non-likely

  c->vsub(DEST::xyz, vf15, vf0, vf15);              // vsub.xyz vf15, vf0, vf15
  // nop                                            // sll r0, r0, 0

block_3:
  bc = ((s64)c->sgpr64(a3)) >= 0;                   // bgez a3, L40
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_5;}                           // branch non-likely

  c->vsub(DEST::xyz, vf16, vf0, vf16);              // vsub.xyz vf16, vf0, vf16
  // nop                                            // sll r0, r0, 0

block_5:
  bc = ((s64)c->sgpr64(t0)) >= 0;                   // bgez t0, L41
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  c->vsub(DEST::xyz, vf17, vf0, vf17);              // vsub.xyz vf17, vf0, vf17
  // nop                                            // sll r0, r0, 0

block_7:
  bc = ((s64)c->sgpr64(t1)) >= 0;                   // bgez t1, L42
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_9;}                           // branch non-likely

  c->vsub(DEST::xyz, vf18, vf0, vf18);              // vsub.xyz vf18, vf0, vf18
  // nop                                            // sll r0, r0, 0

block_9:
  c->sqc2(vf15, 2064, v1);                          // sqc2 vf15, 2064(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf16, 2080, v1);                          // sqc2 vf16, 2080(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf17, 2096, v1);                          // sqc2 vf17, 2096(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf18, 2112, v1);                          // sqc2 vf18, 2112(v1)
  c->daddiu(v1, v1, 64);                            // daddiu v1, v1, 64
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1

block_10:
  c->slti(a2, a0, 128);                             // slti a2, a0, 128
  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L38
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->addiu(s5, r0, 0);                              // addiu s5, r0, 0
  //beq r0, r0, L45                                 // beq r0, r0, L45
  // nop                                            // sll r0, r0, 0
  goto block_13;                                    // branch always


block_12:
  c->load_symbol2(t9, cache.vector_normalize);      // lw t9, vector-normalize!(s7)
  c->dsll(v1, s5, 4);                               // dsll v1, s5, 4
  c->daddiu(v1, v1, 2064);                          // daddiu v1, v1, 2064
  c->daddu(a0, v1, gp);                             // daddu a0, v1, gp
  c->lui(v1, 18243);                                // lui v1, 18243
  c->ori(a1, v1, 20480);                            // ori a1, v1, 20480
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->daddiu(s5, s5, 1);                             // daddiu s5, s5, 1

block_13:
  c->slti(v1, s5, 512);                             // slti v1, s5, 512
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L44
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 32, sp);                                // lq gp, 32(sp)
  c->lq(s5, 16, sp);                                // lq s5, 16(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 48);                            // daddiu sp, sp, 48
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  sky_regs_vfs.copy_vfs_from_other(c);
  return c->gprs[v0].du64[0];
}

void link() {
  cache.vector_normalize = intern_from_c(-1, 0, "vector-normalize!").c();
  gLinkedFunctionTable.reg("(method 34 sky-work)", execute, 128);
}

} // namespace method_34_sky_work
} // namespace Mips2C

namespace Mips2C::jak3 {
namespace method_35_sky_work {
struct Cache {
  void* adgif_shader_texture_simple; // adgif-shader<-texture-simple!
  void* lookup_texture_by_id_fast; // lookup-texture-by-id-fast
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->copy_vfs_from_other(&sky_regs_vfs);
  u32 call_addr = 0;
  c->daddiu(sp, sp, -64);                           // daddiu sp, sp, -64
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s4, 16, sp);                                // sq s4, 16(sp)
  c->sq(s5, 32, sp);                                // sq s5, 32(sp)
  c->sq(gp, 48, sp);                                // sq gp, 48(sp)
  c->mov64(s5, a0);                                 // or s5, a0, r0
  c->mov64(gp, a1);                                 // or gp, a1, r0
  c->sw(gp, 2044, s5);                              // sw gp, 2044(s5)
  c->load_symbol2(t9, cache.lookup_texture_by_id_fast);// lw t9, lookup-texture-by-id-fast(s7)
  c->lui(v1, 128);                                  // lui v1, 128
  c->ori(a0, v1, 1024);                             // ori a0, v1, 1024
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(a1, v0);                                 // or a1, v0, r0
  bc = c->sgpr64(s7) == c->sgpr64(a1);              // beq s7, a1, L36
  c->mov64(a3, s7);                                 // or a3, s7, r0
  if (bc) {goto block_15;}                          // branch non-likely

  c->lwu(v1, 2044, s5);                             // lwu v1, 2044(s5)
  c->lwu(v1, 4, v1);                                // lwu v1, 4(v1)
  c->lq(a0, 0, s5);                                 // lq a0, 0(s5)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->lq(a0, 16, s5);                                // lq a0, 16(s5)
  c->sq(a0, 16, v1);                                // sq a0, 16(v1)
  c->daddiu(s4, v1, 32);                            // daddiu s4, v1, 32
  c->load_symbol2(t9, cache.adgif_shader_texture_simple);// lw t9, adgif-shader<-texture-simple!(s7)
  c->mov64(a0, s4);                                 // or a0, s4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->addiu(v1, r0, 72);                             // addiu v1, r0, 72
  c->sd(v1, 64, s4);                                // sd v1, 64(s4)
  c->lwu(v1, 2044, s5);                             // lwu v1, 2044(s5)
  c->lwu(v1, 4, v1);                                // lwu v1, 4(v1)
  c->daddiu(v1, v1, 112);                           // daddiu v1, v1, 112
  c->lwu(a0, 2044, s5);                             // lwu a0, 2044(s5)
  c->sw(v1, 4, a0);                                 // sw v1, 4(a0)
  c->lw(a3, 4, gp);                                 // lw a3, 4(gp)
  c->mov64(v1, s5);                                 // or v1, s5, r0
  c->addiu(a1, r0, 0);                              // addiu a1, r0, 0
  c->lqc2(vf2, 864, s5);                            // lqc2 vf2, 864(s5)
  c->lqc2(vf5, 880, s5);                            // lqc2 vf5, 880(s5)
  c->lqc2(vf8, 32, s5);                             // lqc2 vf8, 32(s5)
  c->lqc2(vf11, 48, s5);                            // lqc2 vf11, 48(s5)
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  //beq r0, r0, L35                                 // beq r0, r0, L35
  // nop                                            // sll r0, r0, 0
  goto block_13;                                    // branch always

  // nop                                            // sll r0, r0, 0

block_3:
  c->lqc2(vf15, 2064, v1);                          // lqc2 vf15, 2064(v1)
  c->daddiu(v1, v1, 16);                            // daddiu v1, v1, 16
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf15);       // vmulax.xyzw acc, vf31, vf15
  c->lqc2(vf1, 416, s5);                            // lqc2 vf1, 416(s5)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf15);      // vmadday.xyzw acc, vf30, vf15
  c->lqc2(vf4, 432, s5);                            // lqc2 vf4, 432(s5)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf15);      // vmaddaz.xyzw acc, vf29, vf15
  c->daddu(a2, s5, a1);                             // daddu a2, s5, a1
  c->vmadd_bc(DEST::xyzw, BC::w, vf15, vf28, vf0);  // vmaddw.xyzw vf15, vf28, vf0
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf16, vf15, vf14);            // vmul.xyzw vf16, vf15, vf14
  c->lqc2(vf3, 608, a2);                            // lqc2 vf3, 608(a2)
  c->vdiv(vf0, BC::w, vf16, BC::w);                 // vdiv Q, vf0.w, vf16.w
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->vwaitq();                                      // vwaitq
  c->andi(a1, a1, 255);                             // andi a1, a1, 255
  c->vmulq(DEST::xyz, vf15, vf15);                  // vmulq.xyz vf15, vf15, Q
  c->vadd(DEST::xyzw, vf15, vf15, vf25);            // vadd.xyzw vf15, vf15, vf25
  c->vadd(DEST::xyz, vf1, vf1, vf15);               // vadd.xyz vf1, vf1, vf15
  c->vadd(DEST::xyz, vf4, vf4, vf15);               // vadd.xyz vf4, vf4, vf15
  c->vftoi4(DEST::xyzw, vf1, vf1);                  // vftoi4.xyzw vf1, vf1
  c->vftoi4(DEST::xyzw, vf4, vf4);                  // vftoi4.xyzw vf4, vf4
  c->sqc2(vf1, 1728, s5);                           // sqc2 vf1, 1728(s5)
  c->sqc2(vf4, 1744, s5);                           // sqc2 vf4, 1744(s5)
  c->lw(a2, 1728, s5);                              // lw a2, 1728(s5)
  c->ori(t0, r0, 36864);                            // ori t0, r0, 36864
  c->slt(a2, a2, t0);                               // slt a2, a2, t0
  c->daddiu(t0, s7, 4);                             // daddiu t0, s7, 4
  c->movz(t0, s7, a2);                              // movz t0, s7, a2
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(t0))) {// beql s7, t0, L33
    c->mov64(a2, t0);                               // or a2, t0, r0
    goto block_10;
  }

// block_5:
  c->lw(a2, 1732, s5);                              // lw a2, 1732(s5)
  c->ori(t0, r0, 36096);                            // ori t0, r0, 36096
  c->slt(a2, a2, t0);                               // slt a2, a2, t0
  c->daddiu(t0, s7, 4);                             // daddiu t0, s7, 4
  c->movz(t0, s7, a2);                              // movz t0, s7, a2
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(t0))) {// beql s7, t0, L33
    c->mov64(a2, t0);                               // or a2, t0, r0
    goto block_10;
  }

// block_7:
  c->addiu(a2, r0, 28672);                          // addiu a2, r0, 28672
  c->lw(t0, 1744, s5);                              // lw t0, 1744(s5)
  c->slt(a2, a2, t0);                               // slt a2, a2, t0
  c->daddiu(t0, s7, 4);                             // daddiu t0, s7, 4
  c->movz(t0, s7, a2);                              // movz t0, s7, a2
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(t0))) {// beql s7, t0, L33
    c->mov64(a2, t0);                               // or a2, t0, r0
    goto block_10;
  }

// block_9:
  c->addiu(a2, r0, 29440);                          // addiu a2, r0, 29440
  c->lw(t0, 1748, s5);                              // lw t0, 1748(s5)
  c->slt(t0, a2, t0);                               // slt t0, a2, t0
  c->daddiu(a2, s7, 4);                             // daddiu a2, s7, 4
  c->movz(a2, s7, t0);                              // movz a2, s7, t0

block_10:
  bc = c->sgpr64(s7) == c->sgpr64(a2);              // beq s7, a2, L34
  c->mov64(a2, s7);                                 // or a2, s7, r0
  if (bc) {goto block_12;}                          // branch non-likely

  c->vsub(DEST::z, vf1, vf0, vf0);                  // vsub.z vf1, vf0, vf0
  c->vsub(DEST::z, vf4, vf0, vf0);                  // vsub.z vf4, vf0, vf0
  c->sqc2(vf8, 0, a3);                              // sqc2 vf8, 0(a3)
  c->sqc2(vf11, 16, a3);                            // sqc2 vf11, 16(a3)
  c->sqc2(vf3, 32, a3);                             // sqc2 vf3, 32(a3)
  c->sqc2(vf2, 48, a3);                             // sqc2 vf2, 48(a3)
  c->sqc2(vf1, 64, a3);                             // sqc2 vf1, 64(a3)
  c->sqc2(vf5, 80, a3);                             // sqc2 vf5, 80(a3)
  c->sqc2(vf4, 96, a3);                             // sqc2 vf4, 96(a3)
  c->daddiu(a3, a3, 112);                           // daddiu a3, a3, 112
  c->mov64(a2, a3);                                 // or a2, a3, r0

block_12:
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1

block_13:
  c->slti(a2, a0, 512);                             // slti a2, a0, 512
  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L32
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_3;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->sw(a3, 4, gp);                                 // sw a3, 4(gp)

block_15:
  c->mov64(v1, a3);                                 // or v1, a3, r0
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 48, sp);                                // lq gp, 48(sp)
  c->lq(s5, 32, sp);                                // lq s5, 32(sp)
  c->lq(s4, 16, sp);                                // lq s4, 16(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 64);                            // daddiu sp, sp, 64
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  sky_regs_vfs.copy_vfs_from_other(c);
  return c->gprs[v0].du64[0];
}

void link() {
  cache.adgif_shader_texture_simple = intern_from_c(-1, 0, "adgif-shader<-texture-simple!").c();
  cache.lookup_texture_by_id_fast = intern_from_c(-1, 0, "lookup-texture-by-id-fast").c();
  gLinkedFunctionTable.reg("(method 35 sky-work)", execute, 256);
}

} // namespace method_35_sky_work
} // namespace Mips2C

namespace Mips2C::jak3 {
namespace method_32_sky_work {
struct Cache {
  void* game_info; // *game-info*
  void* adgif_shader_texture_simple; // adgif-shader<-texture-simple!
  void* lookup_texture_by_id_fast; // lookup-texture-by-id-fast
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->copy_vfs_from_other(&sky_regs_vfs);
  c->daddiu(sp, sp, -64);                           // daddiu sp, sp, -64
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s5, 32, sp);                                // sq s5, 32(sp)
  c->sq(gp, 48, sp);                                // sq gp, 48(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->sw(a1, 2044, gp);                              // sw a1, 2044(gp)
  c->load_symbol2(t9, cache.lookup_texture_by_id_fast);// lw t9, lookup-texture-by-id-fast(s7)
  c->lui(v1, 128);                                  // lui v1, 128
  c->ori(a0, v1, 768);                              // ori a0, v1, 768
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->sw(v0, 16, sp);                                // sw v0, 16(sp)
  c->lwu(v1, 16, sp);                               // lwu v1, 16(sp)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L26
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_14;}                          // branch non-likely

  c->lui(v1, 17148);                                // lui v1, 17148
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lui(v1, 16025);                                // lui v1, 16025
  c->ori(v1, v1, 39322);                            // ori v1, v1, 39322
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->lui(v1, 16256);                                // lui v1, 16256
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  c->lui(v1, 15395);                                // lui v1, 15395
  c->ori(v1, v1, 55050);                            // ori v1, v1, 55050
  c->mtc1(f3, v1);                                  // mtc1 f3, v1
  c->load_symbol2(v1, cache.game_info);             // lw v1, *game-info*(s7)
  c->lwc1(f4, 800, v1);                             // lwc1 f4, 800(v1)
  c->muls(f3, f3, f4);                              // mul.s f3, f3, f4
  c->mins(f2, f2, f3);                              // min.s f2, f2, f3
  c->maxs(f1, f1, f2);                              // max.s f1, f1, f2
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->negs(f1, f0);                                  // neg.s f1, f0
  c->daddiu(v1, gp, 384);                           // daddiu v1, gp, 384
  c->swc1(f1, 0, v1);                               // swc1 f1, 0(v1)
  c->swc1(f1, 4, v1);                               // swc1 f1, 4(v1)
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  c->swc1(f1, 8, v1);                               // swc1 f1, 8(v1)
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  c->swc1(f1, 12, v1);                              // swc1 f1, 12(v1)
  c->daddiu(v1, gp, 400);                           // daddiu v1, gp, 400
  c->swc1(f0, 0, v1);                               // swc1 f0, 0(v1)
  c->swc1(f0, 4, v1);                               // swc1 f0, 4(v1)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 8, v1);                               // swc1 f0, 8(v1)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 12, v1);                              // swc1 f0, 12(v1)
  c->lqc2(vf15, 1440, gp);                          // lqc2 vf15, 1440(gp)
  c->vmula_bc(DEST::xyzw, BC::x, vf31, vf15);       // vmulax.xyzw acc, vf31, vf15
  c->lqc2(vf1, 384, gp);                            // lqc2 vf1, 384(gp)
  c->vmadda_bc(DEST::xyzw, BC::y, vf30, vf15);      // vmadday.xyzw acc, vf30, vf15
  c->lqc2(vf4, 400, gp);                            // lqc2 vf4, 400(gp)
  c->vmadda_bc(DEST::xyzw, BC::z, vf29, vf15);      // vmaddaz.xyzw acc, vf29, vf15
  c->lqc2(vf2, 864, gp);                            // lqc2 vf2, 864(gp)
  c->vmadd_bc(DEST::xyzw, BC::w, vf15, vf28, vf0);  // vmaddw.xyzw vf15, vf28, vf0
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf16, vf15, vf14);            // vmul.xyzw vf16, vf15, vf14
  c->lqc2(vf5, 880, gp);                            // lqc2 vf5, 880(gp)
  c->vdiv(vf0, BC::w, vf16, BC::w);                 // vdiv Q, vf0.w, vf16.w
  c->lqc2(vf8, 32, gp);                             // lqc2 vf8, 32(gp)
  c->lqc2(vf11, 48, gp);                            // lqc2 vf11, 48(gp)
  c->lqc2(vf3, 560, gp);                            // lqc2 vf3, 560(gp)
  c->lqc2(vf6, 576, gp);                            // lqc2 vf6, 576(gp)
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyz, vf15, vf15);                  // vmulq.xyz vf15, vf15, Q
  c->vmulq(DEST::xyzw, vf16, vf26);                 // vmulq.xyzw vf16, vf26, Q
  c->vmul_bc(DEST::xyzw, BC::w, vf1, vf1, vf16);    // vmulw.xyzw vf1, vf1, vf16
  c->vmul_bc(DEST::xyzw, BC::w, vf4, vf4, vf16);    // vmulw.xyzw vf4, vf4, vf16
  c->vadd(DEST::xyzw, vf15, vf15, vf25);            // vadd.xyzw vf15, vf15, vf25
  c->vadd(DEST::xyz, vf1, vf1, vf15);               // vadd.xyz vf1, vf1, vf15
  c->vadd(DEST::xyz, vf4, vf4, vf15);               // vadd.xyz vf4, vf4, vf15
  c->vftoi4(DEST::xyz, vf1, vf1);                   // vftoi4.xyz vf1, vf1
  c->vftoi4(DEST::xyzw, vf4, vf4);                  // vftoi4.xyzw vf4, vf4
  c->sqc2(vf1, 1728, gp);                           // sqc2 vf1, 1728(gp)
  c->sqc2(vf4, 1744, gp);                           // sqc2 vf4, 1744(gp)
  c->lwu(v1, 2044, gp);                             // lwu v1, 2044(gp)
  c->lwu(a3, 4, v1);                                // lwu a3, 4(v1)
  c->lw(v1, 1740, gp);                              // lw v1, 1740(gp)
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = !cop1_bc;                                    // bc1f L24
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  if (bc) {goto block_3;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0

block_3:
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(v1))) {// beql s7, v1, L25
    c->mov64(v1, v1);                               // or v1, v1, r0
    goto block_12;
  }

// block_5:
  c->lw(v1, 1728, gp);                              // lw v1, 1728(gp)
  c->ori(a0, r0, 36864);                            // ori a0, r0, 36864
  c->slt(v1, v1, a0);                               // slt v1, v1, a0
  c->daddiu(a0, s7, 4);                             // daddiu a0, s7, 4
  c->movz(a0, s7, v1);                              // movz a0, s7, v1
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(a0))) {// beql s7, a0, L25
    c->mov64(v1, a0);                               // or v1, a0, r0
    goto block_12;
  }

// block_7:
  c->lw(v1, 1732, gp);                              // lw v1, 1732(gp)
  c->ori(a0, r0, 36096);                            // ori a0, r0, 36096
  c->slt(v1, v1, a0);                               // slt v1, v1, a0
  c->daddiu(a0, s7, 4);                             // daddiu a0, s7, 4
  c->movz(a0, s7, v1);                              // movz a0, s7, v1
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(a0))) {// beql s7, a0, L25
    c->mov64(v1, a0);                               // or v1, a0, r0
    goto block_12;
  }

// block_9:
  c->addiu(v1, r0, 28672);                          // addiu v1, r0, 28672
  c->lw(a0, 1744, gp);                              // lw a0, 1744(gp)
  c->slt(v1, v1, a0);                               // slt v1, v1, a0
  c->daddiu(a0, s7, 4);                             // daddiu a0, s7, 4
  c->movz(a0, s7, v1);                              // movz a0, s7, v1
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(a0))) {// beql s7, a0, L25
    c->mov64(v1, a0);                               // or v1, a0, r0
    goto block_12;
  }

// block_11:
  c->addiu(v1, r0, 29440);                          // addiu v1, r0, 29440
  c->lw(a0, 1748, gp);                              // lw a0, 1748(gp)
  c->slt(a0, v1, a0);                               // slt a0, v1, a0
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  c->movz(v1, s7, a0);                              // movz v1, s7, a0

block_12:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L26
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_14;}                          // branch non-likely

  c->vsub(DEST::z, vf1, vf0, vf0);                  // vsub.z vf1, vf0, vf0
  c->vsub(DEST::z, vf4, vf0, vf0);                  // vsub.z vf4, vf0, vf0
  c->sqc2(vf8, 112, a3);                            // sqc2 vf8, 112(a3)
  c->sqc2(vf11, 128, a3);                           // sqc2 vf11, 128(a3)
  c->sqc2(vf3, 144, a3);                            // sqc2 vf3, 144(a3)
  c->sqc2(vf2, 160, a3);                            // sqc2 vf2, 160(a3)
  c->sqc2(vf1, 176, a3);                            // sqc2 vf1, 176(a3)
  c->sqc2(vf5, 192, a3);                            // sqc2 vf5, 192(a3)
  c->sqc2(vf4, 208, a3);                            // sqc2 vf4, 208(a3)
  c->sqc2(vf8, 224, a3);                            // sqc2 vf8, 224(a3)
  c->sqc2(vf11, 240, a3);                           // sqc2 vf11, 240(a3)
  c->sqc2(vf6, 256, a3);                            // sqc2 vf6, 256(a3)
  c->sqc2(vf2, 272, a3);                            // sqc2 vf2, 272(a3)
  c->sqc2(vf1, 288, a3);                            // sqc2 vf1, 288(a3)
  c->sqc2(vf5, 304, a3);                            // sqc2 vf5, 304(a3)
  c->sqc2(vf4, 320, a3);                            // sqc2 vf4, 320(a3)
  c->lwu(v1, 2044, gp);                             // lwu v1, 2044(gp)
  c->lwu(v1, 4, v1);                                // lwu v1, 4(v1)
  c->lq(a0, 0, gp);                                 // lq a0, 0(gp)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->lq(a0, 16, gp);                                // lq a0, 16(gp)
  c->sq(a0, 16, v1);                                // sq a0, 16(v1)
  c->daddiu(s5, v1, 32);                            // daddiu s5, v1, 32
  c->load_symbol2(t9, cache.adgif_shader_texture_simple);// lw t9, adgif-shader<-texture-simple!(s7)
  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->lwu(a1, 16, sp);                               // lwu a1, 16(sp)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->addiu(v1, r0, 72);                             // addiu v1, r0, 72
  c->sd(v1, 64, s5);                                // sd v1, 64(s5)
  c->lwu(v1, 2044, gp);                             // lwu v1, 2044(gp)
  c->lwu(v1, 4, v1);                                // lwu v1, 4(v1)
  c->daddiu(v1, v1, 336);                           // daddiu v1, v1, 336
  c->lwu(a0, 2044, gp);                             // lwu a0, 2044(gp)
  c->sw(v1, 4, a0);                                 // sw v1, 4(a0)

block_14:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 48, sp);                                // lq gp, 48(sp)
  c->lq(s5, 32, sp);                                // lq s5, 32(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 64);                            // daddiu sp, sp, 64
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  sky_regs_vfs.copy_vfs_from_other(c);
  return c->gprs[v0].du64[0];
}

void link() {
  cache.game_info = intern_from_c(-1, 0, "*game-info*").c();
  cache.adgif_shader_texture_simple = intern_from_c(-1, 0, "adgif-shader<-texture-simple!").c();
  cache.lookup_texture_by_id_fast = intern_from_c(-1, 0, "lookup-texture-by-id-fast").c();
  gLinkedFunctionTable.reg("(method 32 sky-work)", execute, 128);
}

} // namespace method_32_sky_work
} // namespace Mips2C

