//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak1/kscheme.h"
using namespace jak1;

namespace Mips2C::jak1 {
namespace particle_adgif {
struct Cache {
  void* particle_adgif_cache; // *particle-adgif-cache*
  void* particle_setup_adgif; // particle-setup-adgif
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->mov64(v1, a0);                                 // or v1, a0, r0
  c->mov64(v1, a1);                                 // or v1, a1, r0
  // nop                                            // sll r0, r0, 0
  c->dsra(a3, a1, 20);                              // dsra a3, a1, 20
  c->load_symbol(t1, cache.particle_adgif_cache);   // lw t1, *particle-adgif-cache*(s7)
  c->dsra(t0, a1, 8);                               // dsra t0, a1, 8
  c->lw(t2, 0, t1);                                 // lw t2, 0(t1)
  c->xor_(a3, a3, t0);                              // xor a3, a3, t0
  c->lhu(v1, 4, t1);                                // lhu v1, 4(t1)
  c->andi(a3, a3, 65535);                           // andi a3, a3, 65535
  c->lw(t4, 8, t1);                                 // lw t4, 8(t1)
  bc = c->sgpr64(v1) == c->sgpr64(a3);              // beq v1, a3, L151
  c->daddiu(t3, t1, 12);                            // daddiu t3, t1, 12
  if (bc) {
    if (c->sgpr64(t4) != 0) {
      goto block_6;
    } else {
      printf("HACK: ignoring cached adgif to avoid crash on particle with unset texture.\n");
     }
  }                           // branch non-likely

  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L150
  c->daddiu(t4, t1, 172);                           // daddiu t4, t1, 172
  if (bc) {goto block_4;}                           // branch non-likely


  block_2:
  c->lhu(v1, 0, t3);                                // lhu v1, 0(t3)
  c->daddiu(t3, t3, 2);                             // daddiu t3, t3, 2
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(v1) == c->sgpr64(a3);              // beq v1, a3, L151
  c->daddiu(t2, t2, -1);                            // daddiu t2, t2, -1
  if (bc) {goto block_6;}                           // branch non-likely

  bc = c->sgpr64(t2) != 0;                          // bne t2, r0, L149
  c->daddiu(t4, t4, 80);                            // daddiu t4, t4, 80
  if (bc) {goto block_2;}                           // branch non-likely


  block_4:
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
  c->lw(v1, 0, t1);                                 // lw v1, 0(t1)
  c->daddiu(v1, v1, -80);                           // daddiu v1, v1, -80
  c->sw(a0, 0, sp);                                 // sw a0, 0(sp)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L152
  c->daddiu(v1, v1, 81);                            // daddiu v1, v1, 81
  if (bc) {goto block_7;}                           // branch non-likely

  c->sh(a3, 0, t3);                                 // sh a3, 0(t3)
  // nop                                            // sll r0, r0, 0
  c->sw(t4, 4, sp);                                 // sw t4, 4(sp)
  c->mov64(a0, t4);                                 // or a0, t4, r0
  c->load_symbol(t9, cache.particle_setup_adgif);   // lw t9, particle-setup-adgif(s7)
  // nop                                            // sll r0, r0, 0
  c->sw(ra, 8, sp);                                 // sw ra, 8(sp)
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sw(v1, 0, t1);                                 // sw v1, 0(t1)
  c->jalr(call_addr);                               // jalr ra, t9
  c->lw(a0, 0, sp);                                 // lw a0, 0(sp)
  // nop                                            // sll r0, r0, 0
  c->lw(t4, 4, sp);                                 // lw t4, 4(sp)
  // nop                                            // sll r0, r0, 0
  c->lw(ra, 8, sp);                                 // lw ra, 8(sp)
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16

  block_6:
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


  block_7:
  c->sw(t4, 4, sp);                                 // sw t4, 4(sp)
  // nop                                            // sll r0, r0, 0
  c->load_symbol(t9, cache.particle_setup_adgif);   // lw t9, particle-setup-adgif(s7)
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
  cache.particle_adgif_cache = intern_from_c("*particle-adgif-cache*").c();
  cache.particle_setup_adgif = intern_from_c("particle-setup-adgif").c();
  gLinkedFunctionTable.reg("particle-adgif", execute, 128);
}

} // namespace particle_adgif
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace sp_launch_particles_var {
struct Cache {
  void* level; // *level*
  void* sp_launcher_enable; // *sp-launcher-enable*
  void* sp_launcher_lock; // *sp-launcher-lock*
  void* time_of_day_context; // *time-of-day-context*
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
  c->mov64(v1, a0);                                 // or v1, a0, r0
  c->mov64(v1, a1);                                 // or v1, a1, r0
  c->mov64(v1, a2);                                 // or v1, a2, r0
  c->mov64(v1, a3);                                 // or v1, a3, r0
  c->mov64(v1, t0);                                 // or v1, t0, r0
  c->mov64(v1, t1);                                 // or v1, t1, r0
  // nop                                            // sll r0, r0, 0
  c->daddiu(sp, sp, -256);                          // daddiu sp, sp, -256
  // nop                                            // sll r0, r0, 0
  c->load_symbol(v1, cache.sp_launcher_enable);     // lw v1, *sp-launcher-enable*(s7)
  c->sw(ra, 0, sp);                                 // sw ra, 0(sp)
  bc = c->sgpr64(v1) == c->sgpr64(s7);              // beq v1, s7, L106
  c->sq(s0, 16, sp);                                // sq s0, 16(sp)
  if (bc) {goto block_6;}                           // branch non-likely

  c->load_symbol(v1, cache.sp_launcher_lock);       // lw v1, *sp-launcher-lock*(s7)
  c->sq(s1, 32, sp);                                // sq s1, 32(sp)
  bc = c->sgpr64(v1) == c->sgpr64(s7);              // beq v1, s7, L108
  c->sq(s2, 48, sp);                                // sq s2, 48(sp)
  if (bc) {goto block_8;}                           // branch non-likely

  bc = c->sgpr64(a3) != c->sgpr64(s7);              // bne a3, s7, L107
  c->lui(v1, 16256);                                // lui v1, 16256
  if (bc) {goto block_7;}                           // branch non-likely

  bc = c->sgpr64(t0) != c->sgpr64(s7);              // bne t0, s7, L107
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  bc = c->sgpr64(t1) != c->sgpr64(v1);              // bne t1, v1, L107
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  c->load_symbol(t9, cache.sp_queue_launch);        // lw t9, sp-queue-launch(s7)
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lw(ra, 0, sp);                                 // lw ra, 0(sp)
  // nop                                            // sll r0, r0, 0

  block_6:
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 256);                           // daddiu sp, sp, 256
  goto end_of_function;                             // return


  block_7:
  c->sd(r0, 2, r0);                                 // sd r0, 2(r0)
  // nop                                            // sll r0, r0, 0

  block_8:
  c->sq(s3, 176, sp);                               // sq s3, 176(sp)
  // nop                                            // sll r0, r0, 0
  c->sq(s4, 192, sp);                               // sq s4, 192(sp)
  // nop                                            // sll r0, r0, 0
  c->sq(s5, 208, sp);                               // sq s5, 208(sp)
  // nop                                            // sll r0, r0, 0
  c->sq(s6, 224, sp);                               // sq s6, 224(sp)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf30, 0, a2);                             // lqc2 vf30, 0(a2)
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
  c->daddiu(a0, sp, 96);                            // daddiu a0, sp, 96
  c->addiu(a2, r0, 0);                              // addiu a2, r0, 0
  c->addiu(a3, r0, 8);                              // addiu a3, r0, 8
  c->load_symbol(t9, cache.sp_init_fields);         // lw t9, sp-init-fields!(s7)
  c->daddiu(t0, s7, 8);                             // daddiu t0, s7, 8
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->sw(v0, 12, sp);                                // sw v0, 12(sp)
  // nop                                            // sll r0, r0, 0
  c->lwc1(f2, 116, sp);                             // lwc1 f2, 116(sp)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s0) == c->sgpr64(s7);              // beq s0, s7, L109
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
  c->mfc1(v1, f2);                                  // mfc1 v1, f2
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L132
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_66;}                          // branch non-likely

  //beq r0, r0, L110                                // beq r0, r0, L110
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
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L132
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_66;}                          // branch non-likely


  block_12:
  c->addiu(a1, r0, 0);                              // addiu a1, r0, 0
  c->mov64(a2, s7);                                 // or a2, s7, r0
  bc = c->sgpr64(s1) == c->sgpr64(s7);              // beq s1, s7, L111
  c->lw(v1, 12, s1);                                // lw v1, 12(s1)
  if (bc) {goto block_15;}                          // branch non-likely

  bc = c->sgpr64(v1) == c->sgpr64(s7);              // beq v1, s7, L111
  c->lh(v1, 6, v1);                                 // lh v1, 6(v1)
  if (bc) {goto block_15;}                          // branch non-likely

  c->andi(a0, v1, 4);                               // andi a0, v1, 4
  c->addiu(a3, r0, 1);                              // addiu a3, r0, 1
  c->movn(a1, a3, a0);                              // movn a1, a3, a0
  c->andi(a0, v1, 8);                               // andi a0, v1, 8
  c->movn(a2, s0, a0);                              // movn a2, s0, a0
  // nop                                            // sll r0, r0, 0

  block_15:
  c->load_symbol(t9, cache.sp_get_particle);        // lw t9, sp-get-particle(s7)
  c->mov64(a0, s3);                                 // or a0, s3, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9
  bc = c->sgpr64(v0) == c->sgpr64(s7);              // beq v0, s7, L132
  c->mov64(s2, v0);                                 // or s2, v0, r0
  if (bc) {goto block_66;}                          // branch non-likely

  c->daddiu(a0, sp, 128);                           // daddiu a0, sp, 128
  c->lw(a1, 12, sp);                                // lw a1, 12(sp)
  c->addiu(a2, r0, 9);                              // addiu a2, r0, 9
  c->addiu(a3, r0, 22);                             // addiu a3, r0, 22
  c->load_symbol(t9, cache.sp_init_fields);         // lw t9, sp-init-fields!(s7)
  c->daddiu(t0, s7, 8);                             // daddiu t0, s7, 8
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->daddiu(a0, s2, 12);                            // daddiu a0, s2, 12
  c->mov64(a1, v0);                                 // or a1, v0, r0
  c->addiu(a2, r0, 23);                             // addiu a2, r0, 23
  c->addiu(a3, r0, 52);                             // addiu a3, r0, 52
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->daddiu(t0, s7, 8);                             // daddiu t0, s7, 8
  c->jalr(call_addr);                               // jalr ra, t9
  c->sw(v0, 80, sp);                                // sw v0, 80(sp)
  // nop                                            // sll r0, r0, 0
  c->lw(s5, 104, s2);                               // lw s5, 104(s2)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s6) != c->sgpr64(s7);              // bne s6, s7, L114
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_23;}                          // branch non-likely

  bc = c->sgpr64(s1) == c->sgpr64(s7);              // beq s1, s7, L112
  c->lw(v1, 12, s1);                                // lw v1, 12(s1)
  if (bc) {goto block_21;}                          // branch non-likely

  bc = c->sgpr64(v1) == c->sgpr64(s7);              // beq v1, s7, L112
  c->lh(v1, 6, v1);                                 // lh v1, 6(v1)
  if (bc) {goto block_21;}                          // branch non-likely

  c->andi(v1, v1, 4);                               // andi v1, v1, 4
  c->mov64(a0, s5);                                 // or a0, s5, r0
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L112
  c->lw(v1, 28, s1);                                // lw v1, 28(s1)
  if (bc) {goto block_21;}                          // branch non-likely

  c->andi(a0, a0, 256);                             // andi a0, a0, 256
  c->addiu(a1, r0, 1);                              // addiu a1, r0, 1
  c->movn(v1, a1, a0);                              // movn v1, a1, a0
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L113                                // beq r0, r0, L113
  c->sw(v1, 148, sp);                               // sw v1, 148(sp)
  goto block_22;                                    // branch always


  block_21:
  c->andi(v1, s5, 16384);                           // andi v1, s5, 16384
  // nop                                            // sll r0, r0, 0
  c->dsra(v1, v1, 14);                              // dsra v1, v1, 14
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 148, sp);                               // sw v1, 148(sp)
  // nop                                            // sll r0, r0, 0

  block_22:
  c->lwc1(f2, 152, sp);                             // lwc1 f2, 152(sp)
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
  c->swc1(f2, 152, sp);                             // swc1 f2, 152(sp)
  // nop                                            // sll r0, r0, 0

  block_23:
  c->andi(v1, s5, 256);                             // andi v1, s5, 256
  c->lqc2(vf4, 160, sp);                            // lqc2 vf4, 160(sp)
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L115
  c->vmini_bc(DEST::xyz, BC::x, vf4, vf4, vf31);    // vminix.xyz vf4, vf4, vf31
  if (bc) {goto block_25;}                          // branch non-likely

  c->sqc2(vf4, 160, sp);                            // sqc2 vf4, 160(sp)
  // nop                                            // sll r0, r0, 0

  block_25:
  bc = c->sgpr64(s1) == c->sgpr64(s7);              // beq s1, s7, L116
  c->andi(v1, s5, 128);                             // andi v1, s5, 128
  if (bc) {goto block_28;}                          // branch non-likely

  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L116
  c->lw(a0, 128, sp);                               // lw a0, 128(sp)
  if (bc) {goto block_28;}                          // branch non-likely

  c->load_symbol(t9, cache.cos);                    // lw t9, cos(s7)
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(a1, v0);                                 // or a1, v0, r0
  c->lw(a0, 128, sp);                               // lw a0, 128(sp)
  c->load_symbol(t9, cache.sin);                    // lw t9, sin(s7)
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->ori(v1, r0, 32768);                            // ori v1, r0, 32768
  c->dsll(v1, v1, 16);                              // dsll v1, v1, 16
  c->lw(a0, 136, sp);                               // lw a0, 136(sp)
  c->xor_(a3, v0, v1);                              // xor a3, v0, v1
  c->sw(a0, 8, s2);                                 // sw a0, 8(s2)
  c->gprs[a2].du64[0] = 0;                          // or a2, r0, r0
  c->lw(t0, 132, sp);                               // lw t0, 132(sp)
  c->load_symbol(t9, cache.quaternion_axis_angle);  // lw t9, quaternion-axis-angle!(s7)
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

  block_28:
  c->sw(s7, 136, s2);                               // sw s7, 136(s2)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s0) == c->sgpr64(s7);              // beq s0, s7, L120
  c->lw(a0, 0, s0);                                 // lw a0, 0(s0)
  if (bc) {goto block_37;}                          // branch non-likely

  c->lw(a1, 24, a0);                                // lw a1, 24(a0)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L120
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_37;}                          // branch non-likely

  c->lw(a2, 0, s1);                                 // lw a2, 0(s1)
  c->daddiu(a3, s1, 60);                            // daddiu a3, s1, 60

  block_31:
  c->lw(v1, 0, a3);                                 // lw v1, 0(a3)
  c->daddiu(a2, a2, -1);                            // daddiu a2, a2, -1
  c->lw(v1, 0, v1);                                 // lw v1, 0(v1)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(v1) != c->sgpr64(a1);              // bne v1, a1, L119
  c->lh(v1, 4, a3);                                 // lh v1, 4(a3)
  if (bc) {goto block_36;}                          // branch non-likely

  c->andi(a0, v1, 1);                               // andi a0, v1, 1
  c->ori(v1, v1, 1);                                // ori v1, v1, 1
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L119
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_36;}                          // branch non-likely

  c->sh(v1, 4, a3);                                 // sh v1, 4(a3)
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 0, s2);                                 // lw v1, 0(s2)
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 8, a3);                                 // sw v1, 8(a3)
  c->mov64(a0, s7);                                 // or a0, s7, r0
  if (((s64)c->sgpr64(s6)) != ((s64)c->sgpr64(s7))) {// bnel s6, s7, L118
    c->lw(a0, 0, s2);                               // lw a0, 0(s2)
    goto block_35;
  }

  block_35:
  c->sw(a0, 12, a3);                                // sw a0, 12(a3)
  // nop                                            // sll r0, r0, 0
  c->sw(s2, 16, a3);                                // sw s2, 16(a3)
  // nop                                            // sll r0, r0, 0
  c->sw(a3, 136, s2);                               // sw a3, 136(s2)
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L120                                // beq r0, r0, L120
  // nop                                            // sll r0, r0, 0
  goto block_37;                                    // branch always


  block_36:
  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L117
  c->daddiu(a3, a3, 32);                            // daddiu a3, a3, 32
  if (bc) {goto block_31;}                          // branch non-likely


  block_37:
  c->lw(a2, 80, sp);                                // lw a2, 80(sp)
  c->daddiu(a0, sp, 128);                           // daddiu a0, sp, 128
  c->lh(v1, 0, a2);                                 // lh v1, 0(a2)
  c->mov64(a1, s2);                                 // or a1, s2, r0
  c->load_symbol(t9, cache.sp_adjust_launch);       // lw t9, sp-adjust-launch(s7)
  c->daddiu(v1, v1, -64);                           // daddiu v1, v1, -64
  bc = ((s64)c->sgpr64(v1)) >= 0;                   // bgez v1, L121
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_39;}                          // branch non-likely

  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9

  block_39:
  bc = c->sgpr64(s6) == c->sgpr64(s7);              // beq s6, s7, L122
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_41;}                          // branch non-likely

  c->load_symbol(t9, cache.sp_euler_convert);       // lw t9, sp-euler-convert(s7)
  c->daddiu(a0, sp, 128);                           // daddiu a0, sp, 128
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->mov64(a1, s2);                                 // or a1, s2, r0
  c->jalr(call_addr);                               // jalr ra, t9

  block_41:
  bc = c->sgpr64(s0) == c->sgpr64(s7);              // beq s0, s7, L123
  c->lw(a2, 12, s0);                                // lw a2, 12(s0)
  if (bc) {goto block_45;}                          // branch non-likely

  bc = c->sgpr64(a2) == c->sgpr64(s7);              // beq a2, s7, L123
  c->daddiu(a0, sp, 128);                           // daddiu a0, sp, 128
  if (bc) {goto block_45;}                          // branch non-likely

  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L123
  c->mov64(a1, s2);                                 // or a1, s2, r0
  if (bc) {goto block_45;}                          // branch non-likely

  c->load_symbol(t9, cache.sp_rotate_system);       // lw t9, sp-rotate-system(s7)
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9

  block_45:
  c->lqc2(vf4, 128, sp);                            // lqc2 vf4, 128(sp)
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyz, vf4, vf4, vf30);               // vadd.xyz vf4, vf4, vf30
  c->lw(a0, 4, s2);                                 // lw a0, 4(s2)
  c->lw(a1, 96, sp);                                // lw a1, 96(sp)
  // nop                                            // sll r0, r0, 0
  c->load_symbol(t9, cache.particle_adgif);         // lw t9, particle-adgif(s7)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf4, 128, sp);                            // sqc2 vf4, 128(sp)
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lw(a2, 4, s2);                                 // lw a2, 4(s2)
  c->lui(a3, 256);                                  // lui a3, 256
  c->lw(v1, 64, a2);                                // lw v1, 64(a2)
  c->andi(a0, s5, 32);                              // andi a0, s5, 32
  c->addiu(a1, r0, 134);                            // addiu a1, r0, 134
  // nop                                            // sll r0, r0, 0
  c->movn(v1, a1, a0);                              // movn v1, a1, a0
  c->andi(a0, s5, 16);                              // andi a0, s5, 16
  c->addiu(a1, r0, 66);                             // addiu a1, r0, 66
  // nop                                            // sll r0, r0, 0
  c->movn(v1, a1, a0);                              // movn v1, a1, a0
  c->andi(a0, s5, 8);                               // andi a0, s5, 8
  c->addiu(a1, r0, 72);                             // addiu a1, r0, 72
  // nop                                            // sll r0, r0, 0
  c->movn(v1, a1, a0);                              // movn v1, a1, a0
  c->andi(a0, s5, 512);                             // andi a0, s5, 512
  c->sw(v1, 64, a2);                                // sw v1, 64(a2)
  c->ori(a1, a3, 448);                              // ori a1, a3, 448
  if (((s64)c->sgpr64(a0)) != ((s64)0)) {           // bnel a0, r0, L124
    c->sd(a1, 48, a2);                              // sd a1, 48(a2)
    goto block_47;
  }

  block_47:
  c->lw(t9, 108, sp);                               // lw t9, 108(sp)
  c->mov64(a1, s2);                                 // or a1, s2, r0
  c->mov64(a0, s3);                                 // or a0, s3, r0
  c->mov64(t0, s0);                                 // or t0, s0, r0
  bc = c->sgpr64(t9) == 0;                          // beq t9, r0, L125
  c->daddiu(a2, sp, 128);                           // daddiu a2, sp, 128
  if (bc) {goto block_49;}                          // branch non-likely

  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->mov64(a3, s4);                                 // or a3, s4, r0
  c->jalr(call_addr);                               // jalr ra, t9

  block_49:
  c->lw(a0, 120, sp);                               // lw a0, 120(sp)
  // nop                                            // sll r0, r0, 0
  c->lwc1(f2, 4, s4);                               // lwc1 f2, 4(s4)
  c->lui(a1, 16256);                                // lui a1, 16256
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L126
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_52;}                          // branch non-likely

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
  bc = ((s64)c->sgpr64(a1)) < 0;                    // bltz a1, L126
  c->load_symbol(t9, cache.new_sound_id);           // lw t9, new-sound-id(s7)
  if (bc) {goto block_52;}                          // branch non-likely

  c->sw(a1, 4, s4);                                 // sw a1, 4(s4)
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lw(a0, 120, sp);                               // lw a0, 120(sp)
  c->mov64(a1, v0);                                 // or a1, v0, r0
  c->load_symbol(t9, cache.sound_play_by_spec);     // lw t9, sound-play-by-spec(s7)
  c->daddiu(a2, sp, 64);                            // daddiu a2, sp, 64
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9

  block_52:
  c->addiu(a0, r0, 4);                              // addiu a0, r0, 4
  c->andi(v1, s5, 256);                             // andi v1, s5, 256
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L127
  c->dsubu(a0, r0, a0);                             // dsubu a0, r0, a0
  if (bc) {goto block_54;}                          // branch non-likely

  c->load_symbol(v1, cache.add_to_sprite_aux_list); // lw v1, add-to-sprite-aux-list(s7)
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sw(v1, 112, s2);                               // sw v1, 112(s2)
  c->and_(s5, s5, a0);                              // and s5, s5, a0
  c->sw(r0, 172, sp);                               // sw r0, 172(sp)
  // nop                                            // sll r0, r0, 0
  c->sw(r0, 60, s2);                                // sw r0, 60(s2)
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 144, sp);                               // lw v1, 144(sp)
  c->addiu(a0, r0, 3);                              // addiu a0, r0, 3
  c->pmaxw(v1, v1, a0);                             // pmaxw v1, v1, a0
  c->addiu(a0, r0, 11);                             // addiu a0, r0, 11
  c->pminw(v1, v1, a0);                             // pminw v1, v1, a0
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 144, sp);                               // sw v1, 144(sp)
  // nop                                            // sll r0, r0, 0

  block_54:
  c->load_symbol(a0, cache.level);                  // lw a0, *level*(s7)
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 124, a0);                               // lw v1, 124(a0)
  // nop                                            // sll r0, r0, 0
  c->dsubu(v1, s4, v1);                             // dsubu v1, s4, v1
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(v1)) < 0;                    // bltz v1, L128
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_57;}                          // branch non-likely

  c->lw(v1, 128, a0);                               // lw v1, 128(a0)
  // nop                                            // sll r0, r0, 0
  c->dsubu(v1, s4, v1);                             // dsubu v1, s4, v1
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(v1)) >= 0;                   // bgez v1, L128
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_57;}                          // branch non-likely

  //beq r0, r0, L129                                // beq r0, r0, L129
  c->ori(s5, s5, 1024);                             // ori s5, s5, 1024
  goto block_60;                                    // branch always


  block_57:
  c->lw(v1, 2732, a0);                              // lw v1, 2732(a0)
  // nop                                            // sll r0, r0, 0
  c->dsubu(v1, s4, v1);                             // dsubu v1, s4, v1
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(v1)) < 0;                    // bltz v1, L129
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_60;}                          // branch non-likely

  c->lw(v1, 2736, a0);                              // lw v1, 2736(a0)
  // nop                                            // sll r0, r0, 0
  c->dsubu(v1, s4, v1);                             // dsubu v1, s4, v1
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(v1)) >= 0;                   // bgez v1, L129
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_60;}                          // branch non-likely

  c->ori(s5, s5, 2048);                             // ori s5, s5, 2048
  // nop                                            // sll r0, r0, 0

  block_60:
  c->andi(v1, s5, 4352);                            // andi v1, s5, 4352
  c->addiu(a0, r0, 4096);                           // addiu a0, r0, 4096
  bc = c->sgpr64(v1) != c->sgpr64(a0);              // bne v1, a0, L130
  c->load_symbol(a0, cache.time_of_day_context);    // lw a0, *time-of-day-context*(s7)
  if (bc) {goto block_62;}                          // branch non-likely

  c->lqc2(vf4, 108, a0);                            // lqc2 vf4, 108(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf5, 160, sp);                            // lqc2 vf5, 160(sp)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf6, 48, s2);                             // lqc2 vf6, 48(s2)
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyz, vf5, vf5, vf4);                // vmul.xyz vf5, vf5, vf4
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyz, vf6, vf6, vf4);                // vmul.xyz vf6, vf6, vf4
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf5, 160, sp);                            // sqc2 vf5, 160(sp)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf6, 48, s2);                             // sqc2 vf6, 48(s2)
  // nop                                            // sll r0, r0, 0

  block_62:
  c->mov64(v1, s1);                                 // or v1, s1, r0
  c->dsubu(a0, s1, s7);                             // dsubu a0, s1, s7
  c->movz(v1, r0, a0);                              // movz v1, r0, a0
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 132, s2);                               // sw v1, 132(s2)
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 0, s2);                                 // lw v1, 0(s2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf4, 128, sp);                            // lqc2 vf4, 128(sp)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf5, 144, sp);                            // lqc2 vf5, 144(sp)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf6, 160, sp);                            // lqc2 vf6, 160(sp)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf4, 0, v1);                              // sqc2 vf4, 0(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf5, 16, v1);                             // sqc2 vf5, 16(v1)
  // nop                                            // sll r0, r0, 0
  c->vsub_bc(DEST::w, BC::w, vf6, vf0, vf0);        // vsubw.w vf6, vf0, vf0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf6, 32, v1);                             // sqc2 vf6, 32(v1)
  // nop                                            // sll r0, r0, 0
  c->ori(s5, s5, 64);                               // ori s5, s5, 64
  c->lw(a0, 172, sp);                               // lw a0, 172(sp)
  c->sw(s5, 104, s2);                               // sw s5, 104(s2)
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 124, s2);                               // sw a0, 124(s2)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s0) == c->sgpr64(s7);              // beq s0, s7, L131
  c->lui(v1, 16256);                                // lui v1, 16256
  if (bc) {goto block_65;}                          // branch non-likely

  c->lwc1(f2, 24, s0);                              // lwc1 f2, 24(s0)
  c->mtc1(f3, v1);                                  // mtc1 f3, v1
  c->subs(f2, f2, f3);                              // sub.s f2, f2, f3
  // nop                                            // sll r0, r0, 0
  c->swc1(f2, 24, s0);                              // swc1 f2, 24(s0)
  c->cvtws(f2, f2);                                 // cvt.w.s f2, f2
  c->mfc1(v1, f2);                                  // mfc1 v1, f2
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L110
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely

  //beq r0, r0, L132                                // beq r0, r0, L132
  // nop                                            // sll r0, r0, 0
  goto block_66;                                    // branch always


  block_65:
  c->lwc1(f2, 0, s4);                               // lwc1 f2, 0(s4)
  c->mtc1(f3, v1);                                  // mtc1 f3, v1
  c->subs(f2, f2, f3);                              // sub.s f2, f2, f3
  // nop                                            // sll r0, r0, 0
  c->swc1(f2, 0, s4);                               // swc1 f2, 0(s4)
  c->cvtws(f2, f2);                                 // cvt.w.s f2, f2
  c->mfc1(v1, f2);                                  // mfc1 v1, f2
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L110
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely


  block_66:
  c->lw(ra, 0, sp);                                 // lw ra, 0(sp)
  // nop                                            // sll r0, r0, 0
  c->lq(s0, 16, sp);                                // lq s0, 16(sp)
  // nop                                            // sll r0, r0, 0
  c->lq(s1, 32, sp);                                // lq s1, 32(sp)
  // nop                                            // sll r0, r0, 0
  c->lq(s2, 48, sp);                                // lq s2, 48(sp)
  // nop                                            // sll r0, r0, 0
  c->lq(s3, 176, sp);                               // lq s3, 176(sp)
  // nop                                            // sll r0, r0, 0
  c->lq(s4, 192, sp);                               // lq s4, 192(sp)
  // nop                                            // sll r0, r0, 0
  c->lq(s5, 208, sp);                               // lq s5, 208(sp)
  // nop                                            // sll r0, r0, 0
  c->lq(s6, 224, sp);                               // lq s6, 224(sp)
  // nop                                            // sll r0, r0, 0
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 256);                           // daddiu sp, sp, 256
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
  cache.level = intern_from_c("*level*").c();
  cache.sp_launcher_enable = intern_from_c("*sp-launcher-enable*").c();
  cache.sp_launcher_lock = intern_from_c("*sp-launcher-lock*").c();
  cache.time_of_day_context = intern_from_c("*time-of-day-context*").c();
  cache.add_to_sprite_aux_list = intern_from_c("add-to-sprite-aux-list").c();
  cache.cos = intern_from_c("cos").c();
  cache.new_sound_id = intern_from_c("new-sound-id").c();
  cache.particle_adgif = intern_from_c("particle-adgif").c();
  cache.quaternion_axis_angle = intern_from_c("quaternion-axis-angle!").c();
  cache.sin = intern_from_c("sin").c();
  cache.sound_play_by_spec = intern_from_c("sound-play-by-spec").c();
  cache.sp_adjust_launch = intern_from_c("sp-adjust-launch").c();
  cache.sp_euler_convert = intern_from_c("sp-euler-convert").c();
  cache.sp_get_particle = intern_from_c("sp-get-particle").c();
  cache.sp_init_fields = intern_from_c("sp-init-fields!").c();
  cache.sp_queue_launch = intern_from_c("sp-queue-launch").c();
  cache.sp_rotate_system = intern_from_c("sp-rotate-system").c();
  gLinkedFunctionTable.reg("sp-launch-particles-var", execute, 512);
}

} // namespace sp_launch_particles_var
} // namespace Mips2C
