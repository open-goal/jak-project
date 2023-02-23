//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace sp_process_block_2d {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* add_to_sprite_aux_list; // add-to-sprite-aux-list
  void* sp_free_particle; // sp-free-particle
  void* sp_orbiter; // sp-orbiter
  void* sp_relaunch_particle_2d; // sp-relaunch-particle-2d
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -128);                          // daddiu sp, sp, -128
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s0, 16, sp);                                // sq s0, 16(sp)
  c->sq(s1, 32, sp);                                // sq s1, 32(sp)
  c->sq(s2, 48, sp);                                // sq s2, 48(sp)
  c->sq(s3, 64, sp);                                // sq s3, 64(sp)
  c->sq(s4, 80, sp);                                // sq s4, 80(sp)
  c->sq(s5, 96, sp);                                // sq s5, 96(sp)
  c->sq(gp, 112, sp);                               // sq gp, 112(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->mov64(s5, a1);                                 // or s5, a1, r0
  c->mov64(s4, a2);                                 // or s4, a2, r0
  c->mov64(s1, a3);                                 // or s1, a3, r0
  c->mov64(s3, t0);                                 // or s3, t0, r0
  c->mov64(s2, t1);                                 // or s2, t1, r0

  block_1:
  c->lb(v1, 128, s5);                               // lb v1, 128(s5)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L113
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_32;}                          // branch non-likely

  c->lb(a0, 129, s5);                               // lb a0, 129(s5)
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->dsll(a0, a0, 4);                               // dsll a0, a0, 4
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  c->lqc2(vf9, 0, v1);                              // lqc2 vf9, 0(v1)
  c->mov128_gpr_vf(v1, vf9);                        // qmfc2.i v1, vf9
  c->andi(s0, v1, 255);                             // andi s0, v1, 255
  bc = c->sgpr64(s0) != 0;                          // bne s0, r0, L103
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_9;}                           // branch non-likely

  c->lw(v1, 104, s5);                               // lw v1, 104(s5)
  // nop                                            // sll r0, r0, 0
  c->andi(v1, v1, 32896);                           // andi v1, v1, 32896
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L101
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_5;}                           // branch non-likely

  c->load_symbol2(t9, cache.add_to_sprite_aux_list);// lw t9, add-to-sprite-aux-list(s7)
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->mov64(a1, s5);                                 // or a1, s5, r0
  c->mov64(a2, s4);                                 // or a2, s4, r0
  c->mov64(a3, s1);                                 // or a3, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9

  block_5:
  c->lw(v1, 100, s5);                               // lw v1, 100(s5)
  c->addiu(a0, r0, -1);                             // addiu a0, r0, -1
  bc = c->sgpr64(v1) == c->sgpr64(a0);              // beq v1, a0, L102
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L112
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_31;}                          // branch non-likely


  block_7:
  c->lw(a0, 104, s5);                               // lw a0, 104(s5)
  c->andi(v1, a0, 32);                              // andi v1, a0, 32
  c->xor_(a0, a0, v1);                              // xor a0, a0, v1
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L113
  c->sw(a0, 104, s5);                               // sw a0, 104(s5)
  if (bc) {goto block_32;}                          // branch non-likely

  c->lw(v1, 124, s5);                               // lw v1, 124(s5)
  //beq r0, r0, L113                                // beq r0, r0, L113
  c->sw(v1, 44, s4);                                // sw v1, 44(s4)
  goto block_32;                                    // branch always


  block_9:
  c->lw(v1, 100, s5);                               // lw v1, 100(s5)
  c->addiu(a0, r0, -1);                             // addiu a0, r0, -1
  bc = c->sgpr64(v1) == c->sgpr64(a0);              // beq v1, a0, L104
  c->dsubu(a0, v1, s0);                             // dsubu a0, v1, s0
  if (bc) {goto block_12;}                          // branch non-likely

  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L112
  c->pmaxw(v1, a0, r0);                             // pmaxw v1, a0, r0
  if (bc) {goto block_31;}                          // branch non-likely

  c->sw(v1, 100, s5);                               // sw v1, 100(s5)

  block_12:
  c->lw(v1, 104, s5);                               // lw v1, 104(s5)
  c->andi(a0, v1, 32);                              // andi a0, v1, 32
  c->xor_(v1, v1, a0);                              // xor v1, v1, a0
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L105
  c->sw(v1, 104, s5);                               // sw v1, 104(s5)
  if (bc) {goto block_14;}                          // branch non-likely

  c->lw(a0, 124, s5);                               // lw a0, 124(s5)
  c->sw(a0, 44, s4);                                // sw a0, 44(s4)

  block_14:
  c->lw(t9, 112, s5);                               // lw t9, 112(s5)
  c->andi(v1, v1, 32896);                           // andi v1, v1, 32896
  c->load_symbol2(a0, cache.add_to_sprite_aux_list);// lw a0, add-to-sprite-aux-list(s7)
  c->movn(t9, a0, v1);                              // movn t9, a0, v1
  bc = c->sgpr64(t9) == 0;                          // beq t9, r0, L106
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_16;}                          // branch non-likely

  c->daddiu(sp, sp, -80);                           // daddiu sp, sp, -80
  c->sq(gp, 0, sp);                                 // sq gp, 0(sp)
  c->sq(s5, 16, sp);                                // sq s5, 16(sp)
  c->sq(s4, 32, sp);                                // sq s4, 32(sp)
  c->sq(s1, 48, sp);                                // sq s1, 48(sp)
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->mov64(a1, s5);                                 // or a1, s5, r0
  c->mov64(a2, s4);                                 // or a2, s4, r0
  c->mov64(a3, s1);                                 // or a3, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sq(s3, 64, sp);                                // sq s3, 64(sp)
  c->jalr(call_addr);                               // jalr ra, t9
  c->lq(gp, 0, sp);                                 // lq gp, 0(sp)
  c->lq(s5, 16, sp);                                // lq s5, 16(sp)
  c->lq(s4, 32, sp);                                // lq s4, 32(sp)
  c->lq(s1, 48, sp);                                // lq s1, 48(sp)
  c->lq(s3, 64, sp);                                // lq s3, 64(sp)
  c->daddiu(sp, sp, 80);                            // daddiu sp, sp, 80

  block_16:
  c->lw(a1, 120, s5);                               // lw a1, 120(s5)
  c->lw(v1, 116, s5);                               // lw v1, 116(s5)
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L107
  c->dsubu(a0, v1, s0);                             // dsubu a0, v1, s0
  if (bc) {goto block_19;}                          // branch non-likely

  c->daddiu(v1, a0, -1);                            // daddiu v1, a0, -1
  bc = ((s64)c->sgpr64(v1)) >= 0;                   // bgez v1, L107
  c->sw(a0, 116, s5);                               // sw a0, 116(s5)
  if (bc) {goto block_19;}                          // branch non-likely

  c->daddiu(sp, sp, -96);                           // daddiu sp, sp, -96
  c->sq(gp, 0, sp);                                 // sq gp, 0(sp)
  c->sq(s5, 16, sp);                                // sq s5, 16(sp)
  c->sq(s4, 32, sp);                                // sq s4, 32(sp)
  c->sq(s1, 48, sp);                                // sq s1, 48(sp)
  c->sq(s3, 64, sp);                                // sq s3, 64(sp)
  c->sq(s2, 80, sp);                                // sq s2, 80(sp)
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->mov64(a3, s4);                                 // or a3, s4, r0
  c->mov64(a2, s5);                                 // or a2, s5, r0
  c->load_symbol2(t9, cache.sp_relaunch_particle_2d);// lw t9, sp-relaunch-particle-2d(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lq(gp, 0, sp);                                 // lq gp, 0(sp)
  c->lq(s5, 16, sp);                                // lq s5, 16(sp)
  c->lq(s4, 32, sp);                                // lq s4, 32(sp)
  c->lq(s1, 48, sp);                                // lq s1, 48(sp)
  c->lq(s3, 64, sp);                                // lq s3, 64(sp)
  c->lq(s2, 80, sp);                                // lq s2, 80(sp)
  c->daddiu(sp, sp, 96);                            // daddiu sp, sp, 96

  block_19:
  c->lqc2(vf1, 0, s4);                              // lqc2 vf1, 0(s4)
  c->lqc2(vf2, 16, s4);                             // lqc2 vf2, 16(s4)
  c->lqc2(vf3, 32, s4);                             // lqc2 vf3, 32(s4)
  c->lqc2(vf4, 16, s5);                             // lqc2 vf4, 16(s5)
  c->lqc2(vf5, 32, s5);                             // lqc2 vf5, 32(s5)
  c->lqc2(vf6, 48, s5);                             // lqc2 vf6, 48(s5)
  c->lqc2(vf7, 64, s5);                             // lqc2 vf7, 64(s5)
  c->lwc1(f0, 96, s5);                              // lwc1 f0, 96(s5)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->vmul_bc(DEST::xyzw, BC::z, vf7, vf7, vf9);     // vmulz.xyzw vf7, vf7, vf9
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L108
  c->vadd(DEST::xyz, vf4, vf4, vf7);                // vadd.xyz vf4, vf4, vf7
  if (bc) {goto block_21;}                          // branch non-likely

  c->mov128_vf_gpr(vf8, v1);                        // qmtc2.i vf8, v1
  c->vsub_bc(DEST::w, BC::x, vf8, vf0, vf8);        // vsubx.w vf8, vf0, vf8
  c->vmul_bc(DEST::xyzw, BC::w, vf8, vf8, vf9);     // vmulw.xyzw vf8, vf8, vf9
  c->vsub_bc(DEST::w, BC::w, vf8, vf0, vf8);        // vsubw.w vf8, vf0, vf8
  c->vmul_bc(DEST::xyz, BC::w, vf4, vf4, vf8);      // vmulw.xyz vf4, vf4, vf8

  block_21:
  c->vmul_bc(DEST::xyzw, BC::y, vf10, vf4, vf9);    // vmuly.xyzw vf10, vf4, vf9
  c->vmul_bc(DEST::xyzw, BC::y, vf11, vf5, vf9);    // vmuly.xyzw vf11, vf5, vf9
  c->vmul_bc(DEST::xyzw, BC::y, vf12, vf6, vf9);    // vmuly.xyzw vf12, vf6, vf9
  c->vadd(DEST::xyzw, vf1, vf1, vf10);              // vadd.xyzw vf1, vf1, vf10
  c->vadd(DEST::zw, vf2, vf2, vf11);                // vadd.zw vf2, vf2, vf11
  c->vadd(DEST::xyzw, vf3, vf3, vf12);              // vadd.xyzw vf3, vf3, vf12
  c->vmax_bc(DEST::xyzw, BC::x, vf3, vf3, vf0);     // vmaxx.xyzw vf3, vf3, vf0
  c->sqc2(vf4, 16, s5);                             // sqc2 vf4, 16(s5)
  c->sqc2(vf1, 0, s4);                              // sqc2 vf1, 0(s4)
  c->sqc2(vf2, 16, s4);                             // sqc2 vf2, 16(s4)
  c->sqc2(vf3, 32, s4);                             // sqc2 vf3, 32(s4)
  c->lwc1(f0, 24, s4);                              // lwc1 f0, 24(s4)
  c->cvtws(f0, f0);                                 // cvt.w.s f0, f0
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->dsll32(v1, v1, 16);                            // dsll32 v1, v1, 16
  c->dsra32(v1, v1, 16);                            // dsra32 v1, v1, 16
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->cvtsw(f0, f0);                                 // cvt.s.w f0, f0
  c->swc1(f0, 24, s4);                              // swc1 f0, 24(s4)
  c->lw(v1, 104, s5);                               // lw v1, 104(s5)
  c->andi(v1, v1, 64);                              // andi v1, v1, 64
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L109
  c->load_symbol2(t9, cache.sp_orbiter);            // lw t9, sp-orbiter(s7)
  if (bc) {goto block_23;}                          // branch non-likely

  c->daddiu(sp, sp, -96);                           // daddiu sp, sp, -96
  c->sq(gp, 0, sp);                                 // sq gp, 0(sp)
  c->sq(s5, 16, sp);                                // sq s5, 16(sp)
  c->sq(s4, 32, sp);                                // sq s4, 32(sp)
  c->sq(s1, 48, sp);                                // sq s1, 48(sp)
  c->sq(s3, 64, sp);                                // sq s3, 64(sp)
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->mov64(a1, s5);                                 // or a1, s5, r0
  c->mov64(a2, s4);                                 // or a2, s4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sq(s2, 80, sp);                                // sq s2, 80(sp)
  c->jalr(call_addr);                               // jalr ra, t9
  c->lq(gp, 0, sp);                                 // lq gp, 0(sp)
  c->lq(s5, 16, sp);                                // lq s5, 16(sp)
  c->lq(s4, 32, sp);                                // lq s4, 32(sp)
  c->lq(s1, 48, sp);                                // lq s1, 48(sp)
  c->lq(s3, 64, sp);                                // lq s3, 64(sp)
  c->lq(s2, 80, sp);                                // lq s2, 80(sp)
  c->daddiu(sp, sp, 96);                            // daddiu sp, sp, 96

  block_23:
  c->lq(v1, 32, s4);                                // lq v1, 32(s4)
  c->lw(a0, 104, s5);                               // lw a0, 104(s5)
  c->andi(a1, a0, 2);                               // andi a1, a0, 2
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L110
  c->andi(a1, a0, 4);                               // andi a1, a0, 4
  if (bc) {goto block_26;}                          // branch non-likely

  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L110
  c->pextuw(t4, v1, r0);                            // pextuw t4, v1, r0
  if (bc) {goto block_26;}                          // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L112
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_31;}                          // branch non-likely


  block_26:
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L111
  c->andi(a0, a0, 1);                               // andi a0, a0, 1
  if (bc) {goto block_28;}                          // branch non-likely

  c->pcpyud(t4, v1, r0);                            // pcpyud t4, v1, r0
  c->pexew(t4, t4);                                 // pexew t4, t4
  bc = ((s64)c->sgpr64(t4)) <= 0;                   // blez t4, L112
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_31;}                          // branch non-likely


  block_28:
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L113
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_32;}                          // branch non-likely

  c->mov128_gpr_vf(v1, vf1);                        // qmfc2.i v1, vf1
  c->pcpyud(v1, v1, r0);                            // pcpyud v1, v1, r0
  c->pexew(v1, v1);                                 // pexew v1, v1
  bc = ((s64)c->sgpr64(v1)) < 0;                    // bltz v1, L112
  c->mov128_gpr_vf(v1, vf2);                        // qmfc2.i v1, vf2
  if (bc) {goto block_31;}                          // branch non-likely

  c->pcpyud(v1, v1, r0);                            // pcpyud v1, v1, r0
  c->pexew(v1, v1);                                 // pexew v1, v1
  bc = ((s64)c->sgpr64(v1)) >= 0;                   // bgez v1, L113
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_32;}                          // branch non-likely


  block_31:
  c->load_symbol2(t9, cache.sp_free_particle);      // lw t9, sp-free-particle(s7)
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->mov64(a1, s1);                                 // or a1, s1, r0
  c->mov64(a2, s5);                                 // or a2, s5, r0
  c->mov64(a3, s4);                                 // or a3, s4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9

  block_32:
  c->daddiu(s3, s3, -1);                            // daddiu s3, s3, -1
  c->daddiu(s5, s5, 144);                           // daddiu s5, s5, 144
  c->daddiu(s4, s4, 48);                            // daddiu s4, s4, 48
  bc = c->sgpr64(s3) != 0;                          // bne s3, r0, L100
  c->daddiu(s1, s1, 1);                             // daddiu s1, s1, 1
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(v0, s1);                                 // or v0, s1, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 112, sp);                               // lq gp, 112(sp)
  c->lq(s5, 96, sp);                                // lq s5, 96(sp)
  c->lq(s4, 80, sp);                                // lq s4, 80(sp)
  c->lq(s3, 64, sp);                                // lq s3, 64(sp)
  c->lq(s2, 48, sp);                                // lq s2, 48(sp)
  c->lq(s1, 32, sp);                                // lq s1, 32(sp)
  c->lq(s0, 16, sp);                                // lq s0, 16(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 128);                           // daddiu sp, sp, 128
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  cache.add_to_sprite_aux_list = intern_from_c("add-to-sprite-aux-list").c();
  cache.sp_free_particle = intern_from_c("sp-free-particle").c();
  cache.sp_orbiter = intern_from_c("sp-orbiter").c();
  cache.sp_relaunch_particle_2d = intern_from_c("sp-relaunch-particle-2d").c();
  gLinkedFunctionTable.reg("sp-process-block-2d", execute, 256);
}

} // namespace sp_process_block_2d
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace sp_process_block_3d {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* quaternion; // quaternion*!
  void* quaternion_normalize; // quaternion-normalize!
  void* sp_free_particle; // sp-free-particle
  void* sp_relaunch_particle_3d; // sp-relaunch-particle-3d
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -176);                          // daddiu sp, sp, -176
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s0, 64, sp);                                // sq s0, 64(sp)
  c->sq(s1, 80, sp);                                // sq s1, 80(sp)
  c->sq(s2, 96, sp);                                // sq s2, 96(sp)
  c->sq(s3, 112, sp);                               // sq s3, 112(sp)
  c->sq(s4, 128, sp);                               // sq s4, 128(sp)
  c->sq(s5, 144, sp);                               // sq s5, 144(sp)
  c->sq(gp, 160, sp);                               // sq gp, 160(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->mov64(s5, a1);                                 // or s5, a1, r0
  c->mov64(s4, a2);                                 // or s4, a2, r0
  c->mov64(s0, a3);                                 // or s0, a3, r0
  c->mov64(s3, t0);                                 // or s3, t0, r0
  c->mov64(s2, t1);                                 // or s2, t1, r0
  c->daddiu(s1, sp, 16);                            // daddiu s1, sp, 16
  c->sq(r0, 0, s1);                                 // sq r0, 0(s1)

  block_1:
  c->lb(v1, 128, s5);                               // lb v1, 128(s5)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L98
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_33;}                          // branch non-likely

  c->lb(a0, 129, s5);                               // lb a0, 129(s5)
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->dsll(a0, a0, 4);                               // dsll a0, a0, 4
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  c->lqc2(vf16, 0, v1);                             // lqc2 vf16, 0(v1)
  c->mov128_gpr_vf(v1, vf16);                       // qmfc2.i v1, vf16
  c->andi(v1, v1, 255);                             // andi v1, v1, 255
  c->sq(v1, 32, sp);                                // sq v1, 32(sp)
  c->lq(v1, 32, sp);                                // lq v1, 32(sp)
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L86
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  c->lw(v1, 100, s5);                               // lw v1, 100(s5)
  c->addiu(a0, r0, -1);                             // addiu a0, r0, -1
  bc = c->sgpr64(v1) == c->sgpr64(a0);              // beq v1, a0, L85
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_5;}                           // branch non-likely

  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L97
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_32;}                          // branch non-likely


  block_5:
  c->lw(a0, 104, s5);                               // lw a0, 104(s5)
  c->andi(v1, a0, 32);                              // andi v1, a0, 32
  c->xor_(a0, a0, v1);                              // xor a0, a0, v1
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L98
  c->sw(a0, 104, s5);                               // sw a0, 104(s5)
  if (bc) {goto block_33;}                          // branch non-likely

  c->lw(v1, 124, s5);                               // lw v1, 124(s5)
  //beq r0, r0, L98                                 // beq r0, r0, L98
  c->sw(v1, 44, s4);                                // sw v1, 44(s4)
  goto block_33;                                    // branch always


  block_7:
  c->lw(v1, 100, s5);                               // lw v1, 100(s5)
  c->addiu(a0, r0, -1);                             // addiu a0, r0, -1
  bc = c->sgpr64(v1) == c->sgpr64(a0);              // beq v1, a0, L87
  c->lq(a0, 32, sp);                                // lq a0, 32(sp)
  if (bc) {goto block_10;}                          // branch non-likely

  c->dsubu(a0, v1, a0);                             // dsubu a0, v1, a0
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L97
  c->pmaxw(v1, a0, r0);                             // pmaxw v1, a0, r0
  if (bc) {goto block_32;}                          // branch non-likely

  c->sw(v1, 100, s5);                               // sw v1, 100(s5)

  block_10:
  c->lw(a0, 104, s5);                               // lw a0, 104(s5)
  c->andi(v1, a0, 32);                              // andi v1, a0, 32
  c->xor_(a0, a0, v1);                              // xor a0, a0, v1
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L88
  c->sw(a0, 104, s5);                               // sw a0, 104(s5)
  if (bc) {goto block_12;}                          // branch non-likely

  c->lw(v1, 124, s5);                               // lw v1, 124(s5)
  c->sw(v1, 44, s4);                                // sw v1, 44(s4)

  block_12:
  c->lw(t9, 112, s5);                               // lw t9, 112(s5)
  bc = c->sgpr64(t9) == 0;                          // beq t9, r0, L89
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_14;}                          // branch non-likely

  c->daddiu(sp, sp, -96);                           // daddiu sp, sp, -96
  c->sq(gp, 0, sp);                                 // sq gp, 0(sp)
  c->sq(s5, 16, sp);                                // sq s5, 16(sp)
  c->sq(s4, 32, sp);                                // sq s4, 32(sp)
  c->sq(s0, 48, sp);                                // sq s0, 48(sp)
  c->sq(s3, 64, sp);                                // sq s3, 64(sp)
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->mov64(a1, s5);                                 // or a1, s5, r0
  c->mov64(a2, s4);                                 // or a2, s4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sq(s2, 80, sp);                                // sq s2, 80(sp)
  c->jalr(call_addr);                               // jalr ra, t9
  c->lq(gp, 0, sp);                                 // lq gp, 0(sp)
  c->lq(s5, 16, sp);                                // lq s5, 16(sp)
  c->lq(s4, 32, sp);                                // lq s4, 32(sp)
  c->lq(s0, 48, sp);                                // lq s0, 48(sp)
  c->lq(s3, 64, sp);                                // lq s3, 64(sp)
  c->lq(s2, 80, sp);                                // lq s2, 80(sp)
  c->daddiu(sp, sp, 96);                            // daddiu sp, sp, 96

  block_14:
  c->lw(a1, 120, s5);                               // lw a1, 120(s5)
  c->lw(v1, 116, s5);                               // lw v1, 116(s5)
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L90
  c->lq(a0, 32, sp);                                // lq a0, 32(sp)
  if (bc) {goto block_17;}                          // branch non-likely

  c->dsubu(v1, v1, a0);                             // dsubu v1, v1, a0
  bc = ((s64)c->sgpr64(v1)) >= 0;                   // bgez v1, L90
  c->sw(v1, 116, s5);                               // sw v1, 116(s5)
  if (bc) {goto block_17;}                          // branch non-likely

  c->daddiu(sp, sp, -96);                           // daddiu sp, sp, -96
  c->sq(gp, 0, sp);                                 // sq gp, 0(sp)
  c->sq(s5, 16, sp);                                // sq s5, 16(sp)
  c->sq(s4, 32, sp);                                // sq s4, 32(sp)
  c->sq(s0, 48, sp);                                // sq s0, 48(sp)
  c->sq(s3, 64, sp);                                // sq s3, 64(sp)
  c->sq(s2, 80, sp);                                // sq s2, 80(sp)
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->mov64(a3, s4);                                 // or a3, s4, r0
  c->mov64(a2, s5);                                 // or a2, s5, r0
  c->load_symbol2(t9, cache.sp_relaunch_particle_3d);// lw t9, sp-relaunch-particle-3d(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lq(gp, 0, sp);                                 // lq gp, 0(sp)
  c->lq(s5, 16, sp);                                // lq s5, 16(sp)
  c->lq(s4, 32, sp);                                // lq s4, 32(sp)
  c->lq(s0, 48, sp);                                // lq s0, 48(sp)
  c->lq(s3, 64, sp);                                // lq s3, 64(sp)
  c->lq(s2, 80, sp);                                // lq s2, 80(sp)
  c->daddiu(sp, sp, 96);                            // daddiu sp, sp, 96

  block_17:
  c->lqc2(vf8, 0, s4);                              // lqc2 vf8, 0(s4)
  c->lqc2(vf9, 16, s4);                             // lqc2 vf9, 16(s4)
  c->lqc2(vf10, 32, s4);                            // lqc2 vf10, 32(s4)
  c->lqc2(vf11, 16, s5);                            // lqc2 vf11, 16(s5)
  c->lqc2(vf12, 32, s5);                            // lqc2 vf12, 32(s5)
  c->lqc2(vf13, 48, s5);                            // lqc2 vf13, 48(s5)
  c->lqc2(vf14, 64, s5);                            // lqc2 vf14, 64(s5)
  c->lwc1(f0, 96, s5);                              // lwc1 f0, 96(s5)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->vmul_bc(DEST::xyzw, BC::z, vf14, vf14, vf16);  // vmulz.xyzw vf14, vf14, vf16
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L91
  c->vadd(DEST::xyz, vf11, vf11, vf14);             // vadd.xyz vf11, vf11, vf14
  if (bc) {goto block_19;}                          // branch non-likely

  c->mov128_vf_gpr(vf15, v1);                       // qmtc2.i vf15, v1
  c->vsub_bc(DEST::w, BC::x, vf15, vf0, vf15);      // vsubx.w vf15, vf0, vf15
  c->vmul_bc(DEST::xyzw, BC::w, vf15, vf15, vf16);  // vmulw.xyzw vf15, vf15, vf16
  c->vsub_bc(DEST::w, BC::w, vf15, vf0, vf15);      // vsubw.w vf15, vf0, vf15
  c->vmul_bc(DEST::xyz, BC::w, vf11, vf11, vf15);   // vmulw.xyz vf11, vf11, vf15

  block_19:
  c->vmul_bc(DEST::xyzw, BC::y, vf17, vf11, vf16);  // vmuly.xyzw vf17, vf11, vf16
  c->vmul_bc(DEST::xyzw, BC::y, vf18, vf12, vf16);  // vmuly.xyzw vf18, vf12, vf16
  c->vmul_bc(DEST::xyzw, BC::y, vf19, vf13, vf16);  // vmuly.xyzw vf19, vf13, vf16
  c->vadd(DEST::xyzw, vf8, vf8, vf17);              // vadd.xyzw vf8, vf8, vf17
  c->vadd_bc(DEST::w, BC::w, vf9, vf9, vf18);       // vaddw.w vf9, vf9, vf18
  c->vadd(DEST::xyzw, vf10, vf10, vf19);            // vadd.xyzw vf10, vf10, vf19
  c->vmax_bc(DEST::xyzw, BC::x, vf10, vf10, vf0);   // vmaxx.xyzw vf10, vf10, vf0
  c->sqc2(vf11, 16, s5);                            // sqc2 vf11, 16(s5)
  c->sqc2(vf8, 0, s4);                              // sqc2 vf8, 0(s4)
  c->sqc2(vf9, 16, s4);                             // sqc2 vf9, 16(s4)
  c->sqc2(vf10, 32, s4);                            // sqc2 vf10, 32(s4)
  c->mov64(v1, s1);                                 // or v1, s1, r0
  c->mov64(a0, s4);                                 // or a0, s4, r0
  c->lwc1(f0, 16, a0);                              // lwc1 f0, 16(a0)
  c->lwc1(f1, 20, a0);                              // lwc1 f1, 20(a0)
  c->lwc1(f2, 24, a0);                              // lwc1 f2, 24(a0)
  c->swc1(f0, 0, v1);                               // swc1 f0, 0(v1)
  c->swc1(f1, 4, v1);                               // swc1 f1, 4(v1)
  c->swc1(f2, 8, v1);                               // swc1 f2, 8(v1)
  c->lui(a0, 16256);                                // lui a0, 16256
  c->mtc1(f3, a0);                                  // mtc1 f3, a0
  c->muls(f2, f2, f2);                              // mul.s f2, f2, f2
  c->subs(f2, f3, f2);                              // sub.s f2, f3, f2
  c->muls(f1, f1, f1);                              // mul.s f1, f1, f1
  c->subs(f1, f2, f1);                              // sub.s f1, f2, f1
  c->muls(f0, f0, f0);                              // mul.s f0, f0, f0
  c->subs(f0, f1, f0);                              // sub.s f0, f1, f0
  c->sqrts(f0, f0);                                 // sqrt.s f0, f0
  c->swc1(f0, 12, v1);                              // swc1 f0, 12(v1)
  c->mfc1(a0, f0);                                  // mfc1 a0, f0
  c->lq(v1, 32, sp);                                // lq v1, 32(sp)
  c->mov64(v1, v1);                                 // or v1, v1, r0
  c->sq(v1, 48, sp);                                // sq v1, 48(sp)

  block_20:
  c->load_symbol2(t9, cache.quaternion);            // lw t9, quaternion*!(s7)
  c->mov64(a0, s1);                                 // or a0, s1, r0
  c->mov64(a1, s1);                                 // or a1, s1, r0
  c->daddiu(a2, s5, 80);                            // daddiu a2, s5, 80
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lq(v1, 48, sp);                                // lq v1, 48(sp)
  c->daddiu(v1, v1, -10);                           // daddiu v1, v1, -10
  c->sq(v1, 48, sp);                                // sq v1, 48(sp)
  // nop                                            // sll r0, r0, 0
  c->lq(v1, 48, sp);                                // lq v1, 48(sp)
  bc = ((s64)c->sgpr64(v1)) > 0;                    // bgtz v1, L92
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_20;}                          // branch non-likely

  c->load_symbol2(t9, cache.quaternion_normalize);  // lw t9, quaternion-normalize!(s7)
  c->mov64(a0, s1);                                 // or a0, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(a0, s4);                                 // or a0, s4, r0
  c->mov64(v1, s1);                                 // or v1, s1, r0
  c->lwc1(f0, 12, v1);                              // lwc1 f0, 12(v1)
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = !cop1_bc;                                    // bc1f L93
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_23;}                          // branch non-likely

  c->lqc2(vf1, 16, a0);                             // lqc2 vf1, 16(a0)
  c->lqc2(vf2, 0, v1);                              // lqc2 vf2, 0(v1)
  c->vsub(DEST::xyz, vf1, vf0, vf2);                // vsub.xyz vf1, vf0, vf2
  c->sqc2(vf1, 16, a0);                             // sqc2 vf1, 16(a0)
  c->mov128_gpr_vf(a0, vf1);                        // qmfc2.i a0, vf1
  //beq r0, r0, L94                                 // beq r0, r0, L94
  // nop                                            // sll r0, r0, 0
  goto block_24;                                    // branch always


  block_23:
  c->lqc2(vf1, 16, a0);                             // lqc2 vf1, 16(a0)
  c->lqc2(vf2, 0, v1);                              // lqc2 vf2, 0(v1)
  c->vadd(DEST::xyz, vf1, vf0, vf2);                // vadd.xyz vf1, vf0, vf2
  c->sqc2(vf1, 16, a0);                             // sqc2 vf1, 16(a0)
  c->mov128_gpr_vf(a0, vf1);                        // qmfc2.i a0, vf1

  block_24:
  c->mov128_gpr_vf(v1, vf10);                       // qmfc2.i v1, vf10
  c->lw(a0, 104, s5);                               // lw a0, 104(s5)
  c->andi(a1, a0, 2);                               // andi a1, a0, 2
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L95
  c->andi(a1, a0, 4);                               // andi a1, a0, 4
  if (bc) {goto block_27;}                          // branch non-likely

  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L95
  c->pextuw(a2, v1, r0);                            // pextuw a2, v1, r0
  if (bc) {goto block_27;}                          // branch non-likely

  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L97
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_32;}                          // branch non-likely


  block_27:
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L96
  c->andi(a0, a0, 1);                               // andi a0, a0, 1
  if (bc) {goto block_29;}                          // branch non-likely

  c->pcpyud(v1, v1, r0);                            // pcpyud v1, v1, r0
  c->pexew(v1, v1);                                 // pexew v1, v1
  bc = ((s64)c->sgpr64(v1)) <= 0;                   // blez v1, L97
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_32;}                          // branch non-likely


  block_29:
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L98
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_33;}                          // branch non-likely

  c->mov128_gpr_vf(v1, vf8);                        // qmfc2.i v1, vf8
  c->pcpyud(v1, v1, r0);                            // pcpyud v1, v1, r0
  c->pexew(v1, v1);                                 // pexew v1, v1
  bc = ((s64)c->sgpr64(v1)) < 0;                    // bltz v1, L97
  c->mov128_gpr_vf(v1, vf9);                        // qmfc2.i v1, vf9
  if (bc) {goto block_32;}                          // branch non-likely

  c->pcpyud(v1, v1, r0);                            // pcpyud v1, v1, r0
  c->pexew(v1, v1);                                 // pexew v1, v1
  bc = ((s64)c->sgpr64(v1)) >= 0;                   // bgez v1, L98
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_33;}                          // branch non-likely


  block_32:
  c->load_symbol2(t9, cache.sp_free_particle);      // lw t9, sp-free-particle(s7)
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->mov64(a1, s0);                                 // or a1, s0, r0
  c->mov64(a2, s5);                                 // or a2, s5, r0
  c->mov64(a3, s4);                                 // or a3, s4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9

  block_33:
  c->daddiu(s3, s3, -1);                            // daddiu s3, s3, -1
  c->daddiu(s5, s5, 144);                           // daddiu s5, s5, 144
  c->daddiu(s4, s4, 48);                            // daddiu s4, s4, 48
  bc = c->sgpr64(s3) != 0;                          // bne s3, r0, L84
  c->daddiu(s0, s0, 1);                             // daddiu s0, s0, 1
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(v0, s0);                                 // or v0, s0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 160, sp);                               // lq gp, 160(sp)
  c->lq(s5, 144, sp);                               // lq s5, 144(sp)
  c->lq(s4, 128, sp);                               // lq s4, 128(sp)
  c->lq(s3, 112, sp);                               // lq s3, 112(sp)
  c->lq(s2, 96, sp);                                // lq s2, 96(sp)
  c->lq(s1, 80, sp);                                // lq s1, 80(sp)
  c->lq(s0, 64, sp);                                // lq s0, 64(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 176);                           // daddiu sp, sp, 176
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  cache.quaternion = intern_from_c("quaternion*!").c();
  cache.quaternion_normalize = intern_from_c("quaternion-normalize!").c();
  cache.sp_free_particle = intern_from_c("sp-free-particle").c();
  cache.sp_relaunch_particle_3d = intern_from_c("sp-relaunch-particle-3d").c();
  gLinkedFunctionTable.reg("sp-process-block-3d", execute, 512);
}

} // namespace sp_process_block_3d
} // namespace Mips2C
