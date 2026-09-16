//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace sparticle_motion_blur_dirt {
struct Cache {
  void* atan; // atan
  void* transform_point_qword; // transform-point-qword!
  void* view_get_active_target; // view-get-active-target
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -176);                          // daddiu sp, sp, -176
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s2, 80, sp);                                // sq s2, 80(sp)
  c->sq(s3, 96, sp);                                // sq s3, 96(sp)
  c->sq(s4, 112, sp);                               // sq s4, 112(sp)
  c->sq(s5, 128, sp);                               // sq s5, 128(sp)
  c->sq(gp, 144, sp);                               // sq gp, 144(sp)
  c->swc1(f26, 160, sp);                            // swc1 f26, 160(sp)
  c->swc1(f28, 164, sp);                            // swc1 f28, 164(sp)
  c->swc1(f30, 168, sp);                            // swc1 f30, 168(sp)
  c->mov64(s4, a1);                                 // or s4, a1, r0
  c->mov64(gp, a2);                                 // or gp, a2, r0
  c->daddiu(s5, sp, 16);                            // daddiu s5, sp, 16
  c->lwc1(f0, 0, gp);                               // lwc1 f0, 0(gp)
  c->swc1(f0, 0, s5);                               // swc1 f0, 0(s5)
  c->lwc1(f0, 4, gp);                               // lwc1 f0, 4(gp)
  c->swc1(f0, 4, s5);                               // swc1 f0, 4(s5)
  c->lwc1(f0, 8, gp);                               // lwc1 f0, 8(gp)
  c->swc1(f0, 8, s5);                               // swc1 f0, 8(s5)
  c->lui(v1, 16256);                                // lui v1, 16256
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->swc1(f0, 12, s5);                              // swc1 f0, 12(s5)
  c->lwc1(f0, 16, s4);                              // lwc1 f0, 16(s4)
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  cop1_bc = c->fprs[f0] == c->fprs[f1];             // c.eq.s f0, f1
  bc = !cop1_bc;                                    // bc1f L8
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  if (bc) {goto block_2;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  
block_2:
  if (((s64)c->sgpr64(s7)) != ((s64)c->sgpr64(v1))) {// bnel s7, v1, L10
    c->mov64(v1, v1);                               // or v1, v1, r0
    goto block_10;
  }
  
block_4:
  c->lwc1(f0, 20, s4);                              // lwc1 f0, 20(s4)
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  cop1_bc = c->fprs[f0] == c->fprs[f1];             // c.eq.s f0, f1
  bc = !cop1_bc;                                    // bc1f L9
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  if (bc) {goto block_6;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  
block_6:
  if (((s64)c->sgpr64(s7)) != ((s64)c->sgpr64(v1))) {// bnel s7, v1, L10
    c->mov64(v1, v1);                               // or v1, v1, r0
    goto block_10;
  }
  
block_8:
  c->lwc1(f0, 24, s4);                              // lwc1 f0, 24(s4)
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  cop1_bc = c->fprs[f0] == c->fprs[f1];             // c.eq.s f0, f1
  bc = !cop1_bc;                                    // bc1f L10
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  if (bc) {goto block_10;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  
block_10:
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(v1))) {// beql s7, v1, L11
    c->mov64(v1, v1);                               // or v1, v1, r0
    goto block_13;
  }
  
block_12:
  c->load_symbol2(t9, cache.transform_point_qword); // lw t9, transform-point-qword!(s7)
  c->daddiu(a0, s5, 32);                            // daddiu a0, s5, 32
  c->daddu(a1, r0, s5);                             // daddu a1, r0, s5
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  
block_13:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L14
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_21;}                          // branch non-likely

  c->daddiu(v1, s5, 16);                            // daddiu v1, s5, 16
  c->daddiu(a0, s4, 16);                            // daddiu a0, s4, 16
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->load_symbol2(t9, cache.view_get_active_target);// lw t9, view-get-active-target(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  bc = c->sgpr64(s7) == c->sgpr64(v0);              // beq s7, v0, L12
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_16;}                          // branch non-likely

  c->daddiu(s3, s5, 16);                            // daddiu s3, s5, 16
  c->daddiu(s2, s5, 16);                            // daddiu s2, s5, 16
  c->load_symbol2(t9, cache.view_get_active_target);// lw t9, view-get-active-target(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lwu(v1, 184, v0);                              // lwu v1, 184(v0)
  c->daddiu(v1, v1, 60);                            // daddiu v1, v1, 60
  c->lui(a0, 15194);                                // lui a0, 15194
  c->ori(a0, a0, 29710);                            // ori a0, a0, 29710
  c->mtc1(f0, a0);                                  // mtc1 f0, a0
  c->lqc2(vf2, 0, v1);                              // lqc2 vf2, 0(v1)
  c->lqc2(vf1, 0, s2);                              // lqc2 vf1, 0(s2)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mov128_vf_gpr(vf3, v1);                        // qmtc2.i vf3, v1
  c->vadd_bc(DEST::w, BC::x, vf4, vf0, vf0);        // vaddx.w vf4, vf0, vf0
  c->vmula_bc(DEST::xyzw, BC::x, vf2, vf3);         // vmulax.xyzw acc, vf2, vf3
  c->vmadd_bc(DEST::xyz, BC::w, vf4, vf1, vf0);     // vmaddw.xyz vf4, vf1, vf0
  c->sqc2(vf4, 0, s3);                              // sqc2 vf4, 0(s3)
  
block_16:
  c->daddu(v1, r0, s5);                             // daddu v1, r0, s5
  c->daddu(a0, r0, s5);                             // daddu a0, r0, s5
  c->daddiu(a1, s5, 16);                            // daddiu a1, s5, 16
  c->lui(a2, 16896);                                // lui a2, 16896
  c->mtc1(f0, a2);                                  // mtc1 f0, a2
  c->lqc2(vf2, 0, a1);                              // lqc2 vf2, 0(a1)
  c->lqc2(vf1, 0, a0);                              // lqc2 vf1, 0(a0)
  c->mfc1(a0, f0);                                  // mfc1 a0, f0
  c->mov128_vf_gpr(vf3, a0);                        // qmtc2.i vf3, a0
  c->vadd_bc(DEST::w, BC::x, vf4, vf0, vf0);        // vaddx.w vf4, vf0, vf0
  c->vmula_bc(DEST::xyzw, BC::x, vf2, vf3);         // vmulax.xyzw acc, vf2, vf3
  c->vmadd_bc(DEST::xyz, BC::w, vf4, vf1, vf0);     // vmaddw.xyz vf4, vf1, vf0
  c->sqc2(vf4, 0, v1);                              // sqc2 vf4, 0(v1)
  c->load_symbol2(t9, cache.transform_point_qword); // lw t9, transform-point-qword!(s7)
  c->daddiu(a0, s5, 48);                            // daddiu a0, s5, 48
  c->daddu(a1, r0, s5);                             // daddu a1, r0, s5
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  bc = c->sgpr64(s7) == c->sgpr64(v0);              // beq s7, v0, L14
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_21;}                          // branch non-likely

  c->lw(v1, 32, s5);                                // lw v1, 32(s5)
  c->daddiu(v1, v1, -28672);                        // daddiu v1, v1, -28672
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->cvtsw(f0, f0);                                 // cvt.s.w f0, f0
  c->lw(v1, 36, s5);                                // lw v1, 36(s5)
  c->daddiu(v1, v1, -29440);                        // daddiu v1, v1, -29440
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->cvtsw(f1, f1);                                 // cvt.s.w f1, f1
  c->lw(v1, 48, s5);                                // lw v1, 48(s5)
  c->daddiu(v1, v1, -28672);                        // daddiu v1, v1, -28672
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  c->cvtsw(f2, f2);                                 // cvt.s.w f2, f2
  c->lw(v1, 52, s5);                                // lw v1, 52(s5)
  c->daddiu(v1, v1, -29440);                        // daddiu v1, v1, -29440
  c->mtc1(f3, v1);                                  // mtc1 f3, v1
  c->cvtsw(f3, f3);                                 // cvt.s.w f3, f3
  c->subs(f30, f2, f0);                             // sub.s f30, f2, f0
  c->subs(f28, f3, f1);                             // sub.s f28, f3, f1
  c->lui(v1, -14720);                               // lui v1, -14720
  c->mtc1(f26, v1);                                 // mtc1 f26, v1
  c->load_symbol2(t9, cache.atan);                  // lw t9, atan(s7)
  c->mfc1(a0, f30);                                 // mfc1 a0, f30
  c->mfc1(a1, f28);                                 // mfc1 a1, f28
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mtc1(f0, v0);                                  // mtc1 f0, v0
  c->adds(f0, f26, f0);                             // add.s f0, f26, f0
  c->swc1(f0, 24, gp);                              // swc1 f0, 24(gp)
  c->lwc1(f0, 12, s4);                              // lwc1 f0, 12(s4)
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  cop1_bc = c->fprs[f0] == c->fprs[f1];             // c.eq.s f0, f1
  bc = cop1_bc;                                     // bc1t L13
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_19;}                          // branch non-likely

  c->lui(v1, 16580);                                // lui v1, 16580
  c->ori(v1, v1, 39846);                            // ori v1, v1, 39846
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lui(v1, 18804);                                // lui v1, 18804
  c->ori(v1, v1, 9216);                             // ori v1, v1, 9216
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->lw(v1, 40, s5);                                // lw v1, 40(s5)
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  c->cvtsw(f2, f2);                                 // cvt.s.w f2, f2
  c->divs(f1, f1, f2);                              // div.s f1, f1, f2
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->muls(f1, f30, f30);                            // mul.s f1, f30, f30
  c->muls(f2, f28, f28);                            // mul.s f2, f28, f28
  c->adds(f1, f1, f2);                              // add.s f1, f1, f2
  c->sqrts(f1, f1);                                 // sqrt.s f1, f1
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->swc1(f0, 12, gp);                              // swc1 f0, 12(gp)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  
block_19:
  c->mov64(v0, s7);                                 // or v0, s7, r0
  //beq r0, r0, L16                                 // beq r0, r0, L16
  // nop                                            // sll r0, r0, 0
  goto block_24;                                    // branch always

  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  
block_21:
  c->lwc1(f0, 12, s4);                              // lwc1 f0, 12(s4)
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  cop1_bc = c->fprs[f0] == c->fprs[f1];             // c.eq.s f0, f1
  bc = cop1_bc;                                     // bc1t L15
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_23;}                          // branch non-likely

  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 12, gp);                              // swc1 f0, 12(gp)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  
block_23:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  
block_24:
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lwc1(f30, 168, sp);                            // lwc1 f30, 168(sp)
  c->lwc1(f28, 164, sp);                            // lwc1 f28, 164(sp)
  c->lwc1(f26, 160, sp);                            // lwc1 f26, 160(sp)
  c->lq(gp, 144, sp);                               // lq gp, 144(sp)
  c->lq(s5, 128, sp);                               // lq s5, 128(sp)
  c->lq(s4, 112, sp);                               // lq s4, 112(sp)
  c->lq(s3, 96, sp);                                // lq s3, 96(sp)
  c->lq(s2, 80, sp);                                // lq s2, 80(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 176);                           // daddiu sp, sp, 176
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.atan = intern_from_c(-1, 0, "atan").c();
  cache.transform_point_qword = intern_from_c(-1, 0, "transform-point-qword!").c();
  cache.view_get_active_target = intern_from_c(-1, 0, "view-get-active-target").c();
  gLinkedFunctionTable.reg("sparticle-motion-blur-dirt", execute, 512);
}

} // namespace sparticle_motion_blur_dirt
} // namespace Mips2C