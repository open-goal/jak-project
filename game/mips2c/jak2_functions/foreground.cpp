//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace foreground_check_longest_edge_asm {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* math_camera; // *math-camera*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  bool cop1_bc = false;
  float acc;
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->mov64(v0, s7);                                 // or v0, s7, r0
  c->load_symbol2(v1, cache.math_camera);           // lw v1, *math-camera*(s7)
  c->lwc1(f0, 84, a0);                              // lwc1 f0, 84(a0)
  c->lwc1(f7, 2592, at);                            // lwc1 f7, 2592(at)
  c->lwc1(f3, 2596, at);                            // lwc1 f3, 2596(at)
  c->lwc1(f6, 2600, at);                            // lwc1 f6, 2600(at)
  c->lwc1(f4, 152, a0);                             // lwc1 f4, 152(a0)
  c->lwc1(f12, 0, v1);                              // lwc1 f12, 0(v1)
  c->lwc1(f11, 64, v1);                             // lwc1 f11, 64(v1)
  c->mtc1(f1, a1);                                  // mtc1 f1, a1
  c->mtc1(f9, r0);                                  // mtc1 f9, r0
  c->mtc1(f10, r0);                                 // mtc1 f10, r0
  c->mtc1(f2, r0);                                  // mtc1 f2, r0
  c->mtc1(f2, r0);                                  // mtc1 f2, r0
  c->mtc1(f5, r0);                                  // mtc1 f5, r0
  c->mtc1(f8, r0);                                  // mtc1 f8, r0
  c->lui(a0, 16256);                                // lui a0, 16256
  c->mtc1(f5, a0);                                  // mtc1 f5, a0
  cop1_bc = c->fprs[f1] < c->fprs[f11];             // c.lt.s f1, f11
  bc = cop1_bc;                                     // bc1t L100
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_37;}                          // branch non-likely

  c->lwc1(f11, 12, v1);                             // lwc1 f11, 12(v1)
  c->lwc1(f13, 16, v1);                             // lwc1 f13, 16(v1)
  c->muls(f11, f11, f12);                           // mul.s f11, f11, f12
  c->muls(f13, f13, f12);                           // mul.s f13, f13, f12
  c->subs(f14, f6, f4);                             // sub.s f14, f6, f4
  cop1_bc = c->fprs[f12] < c->fprs[f14];            // c.lt.s f12, f14
  bc = !cop1_bc;                                    // bc1f L84
  c->lwc1(f12, 60, v1);                             // lwc1 f12, 60(v1)
  if (bc) {goto block_3;}                           // branch non-likely

  c->muls(f12, f14, f12);                           // mul.s f12, f14, f12
  cop1_bc = c->fprs[f0] < c->fprs[f12];             // c.lt.s f0, f12
  bc = cop1_bc;                                     // bc1t L99
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_36;}                          // branch non-likely


  block_3:
  c->subs(f14, f3, f4);                             // sub.s f14, f3, f4
  cop1_bc = c->fprs[f13] < c->fprs[f14];            // c.lt.s f13, f14
  bc = !cop1_bc;                                    // bc1f L85
  c->lwc1(f12, 56, v1);                             // lwc1 f12, 56(v1)
  if (bc) {goto block_5;}                           // branch non-likely

  //beq r0, r0, L86                                 // beq r0, r0, L86
  c->muls(f10, f14, f12);                           // mul.s f10, f14, f12
  goto block_7;                                     // branch always


  block_5:
  c->adds(f14, f3, f4);                             // add.s f14, f3, f4
  c->negs(f13, f13);                                // neg.s f13, f13
  cop1_bc = c->fprs[f14] < c->fprs[f13];            // c.lt.s f14, f13
  bc = !cop1_bc;                                    // bc1f L86
  c->negs(f13, f14);                                // neg.s f13, f14
  if (bc) {goto block_7;}                           // branch non-likely

  c->muls(f10, f13, f12);                           // mul.s f10, f13, f12

  block_7:
  cop1_bc = c->fprs[f0] < c->fprs[f10];             // c.lt.s f0, f10
  bc = cop1_bc;                                     // bc1t L99
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_36;}                          // branch non-likely

  c->subs(f12, f7, f4);                             // sub.s f12, f7, f4
  cop1_bc = c->fprs[f11] < c->fprs[f12];            // c.lt.s f11, f12
  bc = !cop1_bc;                                    // bc1f L87
  c->lwc1(f10, 52, v1);                             // lwc1 f10, 52(v1)
  if (bc) {goto block_10;}                          // branch non-likely

  //beq r0, r0, L88                                 // beq r0, r0, L88
  c->muls(f9, f12, f10);                            // mul.s f9, f12, f10
  goto block_12;                                    // branch always


  block_10:
  c->adds(f12, f7, f4);                             // add.s f12, f7, f4
  c->negs(f11, f11);                                // neg.s f11, f11
  cop1_bc = c->fprs[f12] < c->fprs[f11];            // c.lt.s f12, f11
  bc = !cop1_bc;                                    // bc1f L88
  c->negs(f11, f12);                                // neg.s f11, f12
  if (bc) {goto block_12;}                          // branch non-likely

  c->muls(f9, f11, f10);                            // mul.s f9, f11, f10

  block_12:
  cop1_bc = c->fprs[f0] < c->fprs[f9];              // c.lt.s f0, f9
  bc = cop1_bc;                                     // bc1t L99
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_36;}                          // branch non-likely

  c->abss(f14, f7);                                 // abs.s f14, f7
  c->movs(f12, f6);                                 // mov.s f12, f6
  acc = c->fprs[f14] * c->fprs[f14]; // Unknown instr: mula.s f14, f14
  c->fprs[f15] = acc + c->fprs[f12] * c->fprs[f12]; // Unknown instr: madd.s f15, f12, f12
  c->lwc1(f9, 76, v1);                              // lwc1 f9, 76(v1)
  c->lwc1(f13, 80, v1);                             // lwc1 f13, 80(v1)
  c->lwc1(f10, 84, v1);                             // lwc1 f10, 84(v1)
  c->lwc1(f11, 88, v1);                             // lwc1 f11, 88(v1)
  c->fprs[f16] = c->fprs[f5] / (std::sqrt(std::abs(c->fprs[f15]))); // Unknown instr: rsqrt.s f16, f5, f15
  c->muls(f15, f14, f16);                           // mul.s f15, f14, f16
  c->muls(f16, f12, f16);                           // mul.s f16, f12, f16
  acc = c->fprs[f9] * c->fprs[f16]; // Unknown instr: mula.s f9, f16
  c->fprs[f12] = acc - c->fprs[f13] * c->fprs[f15]; // Unknown instr: msub.s f12, f13, f15
  acc = c->fprs[f10] * c->fprs[f16]; // Unknown instr: mula.s f10, f16
  c->fprs[f14] = acc - c->fprs[f11] * c->fprs[f15];// Unknown instr: msub.s f14, f11, f15
  acc = c->fprs[f9] * c->fprs[f15];// Unknown instr: mula.s f9, f15
  c->fprs[f9] = acc + c->fprs[f13] * c->fprs[f16];// Unknown instr: madd.s f9, f13, f16
  acc = c->fprs[f10] * c->fprs[f15];// Unknown instr: mula.s f10, f15
  c->fprs[f10] = acc + c->fprs[f11] * c->fprs[f16];// Unknown instr: madd.s f10, f11, f16
  cop1_bc = c->fprs[f8] < c->fprs[f12];             // c.lt.s f8, f12
  bc = cop1_bc;                                     // bc1t L89
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_17;}                          // branch non-likely

  cop1_bc = c->fprs[f8] < c->fprs[f14];             // c.lt.s f8, f14
  bc = cop1_bc;                                     // bc1t L90
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_18;}                          // branch non-likely

  cop1_bc = c->fprs[f8] < c->fprs[f9];              // c.lt.s f8, f9
  bc = cop1_bc;                                     // bc1t L91
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_19;}                          // branch non-likely

  //beq r0, r0, L93                                 // beq r0, r0, L93
  // nop                                            // sll r0, r0, 0
  goto block_23;                                    // branch always


  block_17:
  //beq r0, r0, L93                                 // beq r0, r0, L93
  c->divs(f2, f1, f10);                             // div.s f2, f1, f10
  goto block_23;                                    // branch always


  block_18:
  c->negs(f2, f12);                                 // neg.s f2, f12
  c->divs(f2, f2, f9);                              // div.s f2, f2, f9
  c->divs(f7, f14, f10);                            // div.s f7, f14, f10
  c->adds(f2, f7, f2);                              // add.s f2, f7, f2
  //beq r0, r0, L93                                 // beq r0, r0, L93
  c->muls(f2, f2, f1);                              // mul.s f2, f2, f1
  goto block_23;                                    // branch always


  block_19:
  c->subs(f8, f7, f4);                              // sub.s f8, f7, f4
  c->adds(f10, f7, f4);                             // add.s f10, f7, f4
  c->negs(f11, f7);                                 // neg.s f11, f7
  cop1_bc = c->fprs[f7] < c->fprs[f8];              // c.lt.s f7, f8
  bc = cop1_bc;                                     // bc1t L92
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_22;}                          // branch non-likely

  cop1_bc = c->fprs[f10] < c->fprs[f11];            // c.lt.s f10, f11
  bc = cop1_bc;                                     // bc1t L92
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_22;}                          // branch non-likely

  //beq r0, r0, L93                                 // beq r0, r0, L93
  // nop                                            // sll r0, r0, 0
  goto block_23;                                    // branch always


  block_22:
  c->negs(f2, f12);                                 // neg.s f2, f12
  c->muls(f2, f1, f2);                              // mul.s f2, f1, f2
  c->divs(f2, f2, f9);                              // div.s f2, f2, f9

  block_23:
  cop1_bc = c->fprs[f0] < c->fprs[f2];              // c.lt.s f0, f2
  bc = cop1_bc;                                     // bc1t L99
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_36;}                          // branch non-likely

  c->abss(f10, f3);                                 // abs.s f10, f3
  c->movs(f12, f6);                                 // mov.s f12, f6
  acc = c->fprs[10] * c->fprs[10]; // Unknown instr: mula.s f10, f10
  c->fprs[9] = acc + c->fprs[12] * c->fprs[12]; // Unknown instr: madd.s f9, f12, f12
  c->lwc1(f7, 96, v1);                              // lwc1 f7, 96(v1)
  c->lwc1(f8, 100, v1);                             // lwc1 f8, 100(v1)
  c->lwc1(f6, 104, v1);                             // lwc1 f6, 104(v1)
  c->fprs[f5] = c->fprs[f5] / (std::sqrt(std::abs(c->fprs[f9]))); // Unknown instr: rsqrt.s f5, f5, f9
  c->lwc1(f11, 108, v1);                            // lwc1 f11, 108(v1)
  c->mtc1(f9, r0);                                  // mtc1 f9, r0
  c->muls(f13, f10, f5);                            // mul.s f13, f10, f5
  c->muls(f14, f12, f5);                            // mul.s f14, f12, f5
  acc = c->fprs[f7] * c->fprs[f14];// Unknown instr: mula.s f7, f14
  c->fprs[f10] = acc - c->fprs[f8] * c->fprs[f13];// Unknown instr: msub.s f10, f8, f13
  acc = c->fprs[f6] * c->fprs[f14];// Unknown instr: mula.s f6, f14
  c->fprs[f12] = acc - c->fprs[f11] * c->fprs[f13];// Unknown instr: msub.s f12, f11, f13
  acc = c->fprs[f7] * c->fprs[f13];// Unknown instr: mula.s f7, f13
  c->fprs[f5] = acc + c->fprs[f8] * c->fprs[f14];// Unknown instr: madd.s f5, f8, f14
  acc = c->fprs[f6] * c->fprs[f13];// Unknown instr: mula.s f6, f13
  c->fprs[f6] = acc + c->fprs[f11] * c->fprs[f14];// Unknown instr: madd.s f6, f11, f14
  cop1_bc = c->fprs[f9] < c->fprs[f10];             // c.lt.s f9, f10
  bc = cop1_bc;                                     // bc1t L94
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_28;}                          // branch non-likely

  cop1_bc = c->fprs[f9] < c->fprs[f12];             // c.lt.s f9, f12
  bc = cop1_bc;                                     // bc1t L95
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_29;}                          // branch non-likely

  cop1_bc = c->fprs[f9] < c->fprs[f5];              // c.lt.s f9, f5
  bc = cop1_bc;                                     // bc1t L96
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_30;}                          // branch non-likely

  //beq r0, r0, L98                                 // beq r0, r0, L98
  // nop                                            // sll r0, r0, 0
  goto block_34;                                    // branch always


  block_28:
  //beq r0, r0, L98                                 // beq r0, r0, L98
  c->divs(f1, f1, f6);                              // div.s f1, f1, f6
  goto block_34;                                    // branch always


  block_29:
  c->negs(f3, f10);                                 // neg.s f3, f10
  c->divs(f3, f3, f5);                              // div.s f3, f3, f5
  c->divs(f4, f12, f6);                             // div.s f4, f12, f6
  c->adds(f3, f4, f3);                              // add.s f3, f4, f3
  //beq r0, r0, L98                                 // beq r0, r0, L98
  c->muls(f1, f3, f1);                              // mul.s f1, f3, f1
  goto block_34;                                    // branch always


  block_30:
  c->subs(f6, f3, f4);                              // sub.s f6, f3, f4
  c->adds(f4, f3, f4);                              // add.s f4, f3, f4
  c->negs(f7, f3);                                  // neg.s f7, f3
  cop1_bc = c->fprs[f3] < c->fprs[f6];              // c.lt.s f3, f6
  bc = cop1_bc;                                     // bc1t L97
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_33;}                          // branch non-likely

  cop1_bc = c->fprs[f4] < c->fprs[f7];              // c.lt.s f4, f7
  bc = cop1_bc;                                     // bc1t L97
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_33;}                          // branch non-likely

  //beq r0, r0, L98                                 // beq r0, r0, L98
  // nop                                            // sll r0, r0, 0
  goto block_34;                                    // branch always


  block_33:
  c->negs(f3, f10);                                 // neg.s f3, f10
  c->muls(f1, f1, f3);                              // mul.s f1, f1, f3
  c->divs(f1, f1, f5);                              // div.s f1, f1, f5

  block_34:
  cop1_bc = c->fprs[f0] < c->fprs[f2];              // c.lt.s f0, f2
  bc = cop1_bc;                                     // bc1t L99
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_36;}                          // branch non-likely

  //beq r0, r0, L100                                // beq r0, r0, L100
  // nop                                            // sll r0, r0, 0
  goto block_37;                                    // branch always


  block_36:
  c->daddiu(v0, s7, 4);                             // daddiu v0, s7, #t

  block_37:
  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  cache.math_camera = intern_from_c("*math-camera*").c();
  gLinkedFunctionTable.reg("foreground-check-longest-edge-asm", execute, 128);
}

} // namespace foreground_check_longest_edge_asm
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace foreground_merc {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* foreground; // *foreground*
  void* merc_global_stats; // *merc-global-stats*
} cache;

/*
(deftype merc-effect-bucket-info (structure)
  ((color-fade     rgba   :offset-assert 0) ;; guessed by decompiler
   (alpha uint8 :offset 3)
   (merc-path      uint8  :offset-assert 4)
   (ignore-alpha   uint8  :offset-assert 5)
   (disable-draw   uint8  :offset-assert 6)
   (disable-envmap uint8  :offset-assert 7)
   )
  :pack-me
  :method-count-assert 9
  :size-assert         #x8
  :flag-assert         #x900000008
  )

(deftype merc-bucket-info (structure)
  ((light                       vu-lights               :inline :offset-assert 0)
   (needs-clip                  int32                           :offset-assert 112)
   (need-mercprime-if-merc      int32                           :offset-assert 116)
   (must-use-mercneric-for-clip int32                           :offset-assert 120)
   (effect                      merc-effect-bucket-info 64 :inline      :offset-assert 124) ;; guessed by decompiler
   )
  :method-count-assert 9
  :size-assert         #x27c
  :flag-assert         #x90000027c
  )
 */

struct MercEffectBucketInfo {
  u8 color_fade[4];
  u8 merc_path;
  u8 ignore_alpha;
  u8 disable_draw;
  u8 disable_envmap;
};

struct MercBucketInfo {
  u8 lights[0x70];
  u32 needs_clip;
  u32 mercprime;
  u32 mercneric;
  MercEffectBucketInfo effects[64];
};
static_assert(sizeof(MercBucketInfo) == 0x27c);

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->daddiu(sp, sp, -128);                          // daddiu sp, sp, -128
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s0, 16, sp);                                // sq s0, 16(sp)
  c->sq(s1, 32, sp);                                // sq s1, 32(sp)
  c->sq(s2, 48, sp);                                // sq s2, 48(sp)
  c->sq(s3, 64, sp);                                // sq s3, 64(sp)
  c->sq(s4, 80, sp);                                // sq s4, 80(sp)
  c->sq(s5, 96, sp);                                // sq s5, 96(sp)
  c->sq(gp, 112, sp);                               // sq gp, 112(sp)
  const MercBucketInfo* mbi = (const MercBucketInfo*)(g_ee_main_mem + c->sgpr64(t1));
  c->mov64(t7, a3);                                 // or t7, a3, r0
  c->mov64(v1, t0);                                 // or v1, t0, r0
  c->lui(t0, 4096);                                 // lui t0, 4096
  c->lui(t1, 18304);                                // lui t1, 18304
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  c->dsll32(t1, t1, 0);                             // dsll32 t1, t1, 0
  c->lui(a3, 12288);                                // lui a3, 12288
  c->lui(t8, 19201);                                // lui t8, 19201
  c->pcpyld(t0, a3, t0);                            // pcpyld t0, a3, t0
  c->lbu(a3, 78, a0);                               // lbu a3, 78(a0)
  c->pcpyld(t1, t8, t1);                            // pcpyld t1, t8, t1
  c->lui(t2, 28160);                                // lui t2, 28160
  c->addiu(t8, r0, 8);                              // addiu t8, r0, 8
  c->multu3(a3, a3, t8);                            // multu3 a3, a3, t8
  c->lui(t3, 1280);                                 // lui t3, 1280
  c->lui(t4, 27648);                                // lui t4, 27648
  c->dsll32(t2, t2, 0);                             // dsll32 t2, t2, 0
  c->dsll32(t4, t4, 0);                             // dsll32 t4, t4, 0
  c->daddu(t4, t4, t3);                             // daddu t4, t4, t3
  c->daddu(t3, t2, t3);                             // daddu t3, t2, t3
  c->daddiu(t3, t3, 1);                             // daddiu t3, t3, 1
  c->daddu(a0, a3, a0);                             // daddu a0, a3, a0
  c->pcpyld(t2, t2, r0);                            // pcpyld t2, t2, r0
  c->lw(a0, 28, a0);                                // lw a0, 28(a0)
  c->pcpyld(t3, t3, r0);                            // pcpyld t3, t3, r0
  c->pcpyld(t4, t4, r0);                            // pcpyld t4, t4, r0
  c->lui(t5, 12288);                                // lui t5, 12288
  c->lui(t6, 4096);                                 // lui t6, 4096
  c->daddiu(t5, t5, 7);                             // daddiu t5, t5, 7
  c->lui(t8, 5120);                                 // lui t8, 5120
  c->lui(a3, 27655);                                // lui a3, 27655
  c->daddu(t7, t8, t7);                             // daddu t7, t8, t7
  c->dsll32(a3, a3, 0);                             // dsll32 a3, a3, 0
  c->dsll32(t8, t7, 0);                             // dsll32 t8, t7, 0
  c->pcpyld(t5, a3, t5);                            // pcpyld t5, a3, t5
  c->lwu(t7, 52, a0);                               // lwu t7, 52(a0)
  c->pcpyld(t6, t8, t6);                            // pcpyld t6, t8, t6
  c->daddiu(t8, a0, 156);                           // daddiu t8, a0, 156
  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L117
  c->load_symbol2(a3, cache.foreground);            // lw a3, *foreground*(s7)
  if (bc) {goto block_16;}                          // branch non-likely

  c->daddiu(t9, a3, 2508);                          // daddiu t9, a3, 2508

  block_2:
  c->mov64(ra, a2);                                 // or ra, a2, r0
  c->lbu(a3, 6, t9);                                // lbu a3, 6(t9)
  c->lbu(gp, 4, t9);                                // lbu gp, 4(t9)
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L117
  c->load_symbol2(a3, cache.merc_global_stats);     // lw a3, *merc-global-stats*(s7)
  if (bc) {goto block_16;}                          // branch non-likely

  c->daddu(a3, r0, a3);                             // daddu a3, r0, a3
  bc = c->sgpr64(gp) != 0;                          // bne gp, r0, L117
  c->lhu(s4, 2, a3);                                // lhu s4, 2(a3)
  if (bc) {goto block_16;}                          // branch non-likely

  c->lhu(s3, 18, t8);                               // lhu s3, 18(t8)
  c->lwu(gp, 4, a3);                                // lwu gp, 4(a3)
  c->lhu(s5, 22, t8);                               // lhu s5, 22(t8)
  c->daddu(s4, s4, s3);                             // daddu s4, s4, s3
  c->lwu(s3, 8, a3);                                // lwu s3, 8(a3)
  c->lhu(s2, 24, t8);                               // lhu s2, 24(t8)
  c->daddu(gp, gp, s5);                             // daddu gp, gp, s5
  c->sh(s4, 2, a3);                                 // sh s4, 2(a3)
  c->sw(gp, 4, a3);                                 // sw gp, 4(a3)
  c->daddu(s5, s3, s2);                             // daddu s5, s3, s2
  c->lwu(t2, 0, t8);                                // lwu t2, 0(t8)
  c->lwu(gp, 4, t8);                                // lwu gp, 4(t8)
  c->lui(s4, 12288);                                // lui s4, 12288
  c->dsll32(t2, t2, 0);                             // dsll32 t2, t2, 0
  c->sw(s5, 8, a3);                                 // sw s5, 8(a3)
  c->or_(t2, t2, s4);                               // or t2, t2, s4
  c->lhu(s5, 18, t8);                               // lhu s5, 18(t8)
  c->addiu(s4, r0, 0);                              // addiu s4, r0, 0
  bc = c->sgpr64(s5) == 0;                          // beq s5, r0, L117
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_16;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0

  block_6:
  c->lbu(s0, 0, gp);                                // lbu s0, 0(gp)
  // nop                                            // sll r0, r0, 0
  c->lbu(s2, 1, gp);                                // lbu s2, 1(gp)
  c->xori(s1, r0, 49292);                           // xori s1, r0, 49292
  c->lbu(s3, 2, gp);                                // lbu s3, 2(gp)
  c->daddiu(v0, s0, 3);                             // daddiu v0, s0, 3
  c->lw(a3, 44, a0);                                // lw a3, 44(a0)
  c->srl(v0, v0, 2);                                // srl v0, v0, 2
  c->sq(t0, 0, a2);                                 // sq t0, 0(a2)
  c->xor_(t2, t2, v0);                              // xor t2, t2, v0
  c->sq(t2, 32, a2);                                // sq t2, 32(a2)
  c->xor_(t2, t2, v0);                              // xor t2, t2, v0
  c->sh(s1, 44, a2);                                // sh s1, 44(a2)
  c->daddu(s1, s1, s0);                             // daddu s1, s1, s0
  c->sb(s0, 46, a2);                                // sb s0, 46(a2)
  c->dsll32(s0, v0, 4);                             // dsll32 s0, v0, 4
  c->daddu(t3, t2, s0);                             // daddu t3, t2, s0
  c->daddiu(s0, s2, 3);                             // daddiu s0, s2, 3
  c->sw(a3, 12, a2);                                // sw a3, 12(a2)
  c->srl(s0, s0, 2);                                // srl s0, s0, 2
  c->sq(t1, 16, a2);                                // sq t1, 16(a2)

  // pc hack
  {
    u16 use_pc_merc_bits = 0;
    u16 ignore_alpha_bits = 0;
    for (int i = 0; i < 16; i++) {
      if (!mbi->effects[i].disable_draw) {
        use_pc_merc_bits |= (1 << i);
      }
      if (mbi->effects[i].ignore_alpha) {
        ignore_alpha_bits |= (1 << i);
      }
    }
    memcpy(g_ee_main_mem + c->sgpr64(a2) + 28, &use_pc_merc_bits, 2);
    memcpy(g_ee_main_mem + c->sgpr64(a2) + 30, &ignore_alpha_bits, 2);
  }

  c->xor_(t3, t3, s0);                              // xor t3, t3, s0
  c->sq(t3, 48, a2);                                // sq t3, 48(a2)
  c->xor_(t3, t3, s0);                              // xor t3, t3, s0
  c->sh(s1, 60, a2);                                // sh s1, 60(a2)
  c->daddu(s1, s1, s2);                             // daddu s1, s1, s2
  c->sb(s2, 62, a2);                                // sb s2, 62(a2)
  c->dsll32(s2, s0, 4);                             // dsll32 s2, s0, 4
  c->sw(a3, 16, a2);                                // sw a3, 16(a2)
  c->daddu(t4, t3, s2);                             // daddu t4, t3, s2
  c->xor_(t4, t4, s3);                              // xor t4, t4, s3
  c->xori(a3, s1, 16384);                           // xori a3, s1, 16384
  c->sq(t4, 64, a2);                                // sq t4, 64(a2)
  c->xor_(t4, t4, s3);                              // xor t4, t4, s3
  c->sb(s3, 78, a2);                                // sb s3, 78(a2)
  c->dsll32(s3, s3, 4);                             // dsll32 s3, s3, 4
  c->sh(a3, 76, a2);                                // sh a3, 76(a2)
  c->daddu(t2, t4, s3);                             // daddu t2, t4, s3
  c->lbu(s3, 3, gp);                                // lbu s3, 3(gp)
  c->daddiu(gp, gp, 4);                             // daddiu gp, gp, 4
  bc = c->sgpr64(s4) != 0;                          // bne s4, r0, L113
  c->daddiu(a2, a2, 80);                            // daddiu a2, a2, 80
  if (bc) {goto block_8;}                           // branch non-likely

  c->sd(t6, 0, a2);                                 // sd t6, 0(a2)
  c->addiu(s2, r0, 8);                              // addiu s2, r0, 8
  c->sd(t6, 8, a2);                                 // sd t6, 8(a2)
  c->lui(a3, 27656);                                // lui a3, 27656
  c->sb(s2, 0, a2);                                 // sb s2, 0(a2)
  c->daddiu(a3, a3, 132);                           // daddiu a3, a3, 132
  c->load_symbol2(s2, cache.foreground);            // lw s2, *foreground*(s7)
  c->daddiu(s1, s2, 2384);                          // daddiu s1, s2, 2384
  c->sw(a3, 12, a2);                                // sw a3, 12(a2)
  c->lq(a3, 0, s1);                                 // lq a3, 0(s1)
  c->lq(s2, 16, s1);                                // lq s2, 16(s1)
  c->lq(s0, 32, s1);                                // lq s0, 32(s1)
  c->lq(v0, 48, s1);                                // lq v0, 48(s1)
  c->sq(a3, 16, a2);                                // sq a3, 16(a2)
  c->sq(s2, 32, a2);                                // sq s2, 32(a2)
  c->sq(s0, 48, a2);                                // sq s0, 48(a2)
  c->sq(v0, 64, a2);                                // sq v0, 64(a2)
  c->lq(a3, 64, s1);                                // lq a3, 64(s1)
  c->lq(s2, 80, s1);                                // lq s2, 80(s1)
  c->lq(s1, 96, s1);                                // lq s1, 96(s1)
  c->lui(s0, 16261);                                // lui s0, 16261
  c->daddiu(v0, s0, 4715);                          // daddiu v0, s0, 4715
  c->daddiu(v0, s0, 619);                           // daddiu v0, s0, 619
  c->lq(s0, 28, a0);                                // lq s0, 28(a0)
  c->sq(a3, 80, a2);                                // sq a3, 80(a2)
  c->lbu(a3, 5, t9);                                // lbu a3, 5(t9)
  c->sq(s2, 96, a2);                                // sq s2, 96(a2)
  c->sq(s1, 112, a2);                               // sq s1, 112(a2)
  c->dsubu(a3, v0, a3);                             // dsubu a3, v0, a3
  c->sq(s0, 128, a2);                               // sq s0, 128(a2)
  c->sw(a3, 28, a2);                                // sw a3, 28(a2)
  c->daddiu(a2, a2, 144);                           // daddiu a2, a2, 144

  // PC ADD BONUS DATA (bonus!)
  {
    // 10 qw test
    u64 dmatag = 5 | (1 << 28);
    memcpy(g_ee_main_mem + c->sgpr64(a2), &dmatag, 8);
    u32 vif = (0b1001 << 24);
    memcpy(g_ee_main_mem + c->sgpr64(a2) + 8, &vif, 4);

    for (int i = 0; i < 16; i++) {
      memcpy(g_ee_main_mem + c->sgpr64(a2) + 16 + i * 4, mbi->effects[i].color_fade, 4);
    }

    c->gprs[a2].du32[0] += 6 * 16;
  }

  block_8:
  bc = c->sgpr64(s3) == 0;                          // beq s3, r0, L115
  c->addiu(s2, r0, 128);                            // addiu s2, r0, 128
  if (bc) {goto block_11;}                          // branch non-likely

  c->lbu(a3, 0, gp);                                // lbu a3, 0(gp)
  // nop                                            // sll r0, r0, 0

  block_10:
  c->multu3(s1, a3, s2);                            // multu3 s1, a3, s2
  c->sq(t5, 0, a2);                                 // sq t5, 0(a2)
  c->lbu(s0, 1, gp);                                // lbu s0, 1(gp)
  c->daddiu(gp, gp, 2);                             // daddiu gp, gp, 2

  // HACK for PC PORT: stash the source matrix number in the unused bits of nop viftag.
  c->sb(a3, 8, a2);

  c->lbu(a3, 0, gp);                                // lbu a3, 0(gp)
  c->daddiu(s3, s3, -1);                            // daddiu s3, s3, -1
  c->sb(s0, 12, a2);                                // sb s0, 12(a2)
  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddu(s1, s1, a1);                             // daddu s1, s1, a1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s3) != 0;                          // bne s3, r0, L114
  c->sw(s1, -12, a2);                               // sw s1, -12(a2)
  if (bc) {goto block_10;}                          // branch non-likely


  block_11:
  c->sq(t6, 0, a2);                                 // sq t6, 0(a2)
  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  bc = c->sgpr64(s4) != 0;                          // bne s4, r0, L116
  c->daddiu(s4, s4, 1);                             // daddiu s4, s4, 1
  if (bc) {goto block_13;}                          // branch non-likely

  c->mov64(a3, v1);                                 // or a3, v1, r0
  c->sb(a3, -4, a2);                                // sb a3, -4(a2)

  block_13:
  bc = c->sgpr64(s4) != c->sgpr64(s5);              // bne s4, s5, L112
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_6;}                           // branch non-likely

  get_fake_spad_addr2(s5, cache.fake_scratchpad_data, 0, c);// lui s5, 28672
  c->lbu(a3, 26, t8);                               // lbu a3, 26(t8)
  c->addiu(gp, r0, 48);                             // addiu gp, r0, 48
  c->lw(s5, 52, s5);                                // lw s5, 52(s5)
  c->mult3(a3, a3, gp);                             // mult3 a3, a3, gp
  // nop                                            // sll r0, r0, 0
  c->daddu(a3, s5, a3);                             // daddu a3, s5, a3
  // nop                                            // sll r0, r0, 0
  c->lw(s4, 0, a3);                                 // lw s4, 0(a3)
  // nop                                            // sll r0, r0, 0
  c->lw(gp, 4, a3);                                 // lw gp, 4(a3)
  c->movz(s4, ra, s4);                              // movz s4, ra, s4
  c->sq(r0, 0, a2);                                 // sq r0, 0(a2)
  c->lui(s5, 8192);                                 // lui s5, 8192
  c->sw(s4, 0, a3);                                 // sw s4, 0(a3)
  c->mov64(s4, a2);                                 // or s4, a2, r0
  c->sw(s5, 0, a2);                                 // sw s5, 0(a2)
  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  bc = c->sgpr64(gp) == 0;                          // beq gp, r0, L117
  c->sw(s4, 4, a3);                                 // sw s4, 4(a3)
  if (bc) {goto block_16;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->sw(ra, 4, gp);                                 // sw ra, 4(gp)

  block_16:
  c->daddiu(t8, t8, 32);                            // daddiu t8, t8, 32
  c->daddiu(t9, t9, 8);                             // daddiu t9, t9, 8
  c->daddiu(t7, t7, -1);                            // daddiu t7, t7, -1
  bc = c->sgpr64(t7) != 0;                          // bne t7, r0, L111
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  c->mov64(v0, a2);                                 // or v0, a2, r0
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
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  cache.foreground = intern_from_c("*foreground*").c();
  cache.merc_global_stats = intern_from_c("*merc-global-stats*").c();
  gLinkedFunctionTable.reg("foreground-merc", execute, 256);
}

} // namespace foreground_merc
} // namespace Mips2C


//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace foreground_generic_merc {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* foreground; // *foreground*
  void* merc_global_stats; // *merc-global-stats*
  void* foreground_generic_merc_add_fragments; // foreground-generic-merc-add-fragments
  void* foreground_generic_merc_death; // foreground-generic-merc-death
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -112);                          // daddiu sp, sp, -112
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s1, 16, sp);                                // sq s1, 16(sp)
  c->sq(s2, 32, sp);                                // sq s2, 32(sp)
  c->sq(s3, 48, sp);                                // sq s3, 48(sp)
  c->sq(s4, 64, sp);                                // sq s4, 64(sp)
  c->sq(s5, 80, sp);                                // sq s5, 80(sp)
  c->sq(gp, 96, sp);                                // sq gp, 96(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->daddu(v1, r0, v1);                             // daddu v1, r0, v1
  c->sw(a2, 16, v1);                                // sw a2, 16(v1)
  c->sw(a1, 20, v1);                                // sw a1, 20(v1)
  c->lb(v1, 78, gp);                                // lb v1, 78(gp)
  c->dsll(v1, v1, 3);                               // dsll v1, v1, 3
  c->daddu(v1, gp, v1);                             // daddu v1, gp, v1
  c->lwu(s5, 28, v1);                               // lwu s5, 28(v1)
  c->daddiu(s3, a1, 16);                            // daddiu s3, a1, 16
  c->addiu(s4, r0, 0);                              // addiu s4, r0, 0
  //beq r0, r0, L153                                // beq r0, r0, L153
  // nop                                            // sll r0, r0, 0
  goto block_77;                                    // branch always


  block_1:
  c->load_symbol2(v1, cache.foreground);            // lw v1, *foreground*(s7)
  c->daddiu(v1, v1, 2384);                          // daddiu v1, v1, 2384
  c->dsll(a0, s4, 3);                               // dsll a0, s4, 3
  c->daddu(a0, v1, a0);                             // daddu a0, v1, a0
  c->lbu(a0, 130, a0);                              // lbu a0, 130(a0)
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->daddiu(a1, s7, 4);                             // daddiu a1, s7, 4
  c->movz(a1, s7, a0);                              // movz a1, s7, a0
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(a1))) {// beql s7, a1, L120
    c->mov64(v1, a1);                               // or v1, a1, r0
    goto block_4;
  }

  c->dsll(a0, s4, 3);                               // dsll a0, s4, 3
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  c->lbu(v1, 128, v1);                              // lbu v1, 128(v1)
  c->daddiu(a0, v1, -2);                            // daddiu a0, v1, -2
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  c->movn(v1, s7, a0);                              // movn v1, s7, a0

  block_4:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L152
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_76;}                          // branch non-likely

  c->mov64(s1, s3);                                 // or s1, s3, r0
  c->dsll(v1, s4, 5);                               // dsll v1, s4, 5
  c->daddiu(v1, v1, 156);                           // daddiu v1, v1, 156
  c->daddu(s2, v1, s5);                             // daddu s2, v1, s5
  c->load_symbol2(v1, cache.merc_global_stats);     // lw v1, *merc-global-stats*(s7)
  c->daddiu(v1, v1, 32);                            // daddiu v1, v1, 32
  c->lhu(a0, 2, v1);                                // lhu a0, 2(v1)
  c->lhu(a1, 18, s2);                               // lhu a1, 18(s2)
  c->daddu(a0, a0, a1);                             // daddu a0, a0, a1
  c->sh(a0, 2, v1);                                 // sh a0, 2(v1)
  c->lwu(a0, 4, v1);                                // lwu a0, 4(v1)
  c->lhu(a1, 22, s2);                               // lhu a1, 22(s2)
  c->daddu(a0, a0, a1);                             // daddu a0, a0, a1
  c->sw(a0, 4, v1);                                 // sw a0, 4(v1)
  c->lwu(a0, 8, v1);                                // lwu a0, 8(v1)
  c->lhu(a1, 24, s2);                               // lhu a1, 24(s2)
  c->daddu(a0, a0, a1);                             // daddu a0, a0, a1
  c->sw(a0, 8, v1);                                 // sw a0, 8(v1)
  c->load_symbol2(v1, cache.foreground);            // lw v1, *foreground*(s7)
  c->daddiu(v1, v1, 2384);                          // daddiu v1, v1, 2384
  c->daddiu(a1, s1, 16);                            // daddiu a1, s1, 16
  c->daddu(a2, r0, v1);                             // daddu a2, r0, v1
  c->addiu(a0, r0, 7);                              // addiu a0, r0, 7
  // nop                                            // sll r0, r0, 0
  c->daddiu(a3, a0, -4);                            // daddiu a3, a0, -4
  c->mov64(a1, a1);                                 // or a1, a1, r0
  bc = ((s64)c->sgpr64(a3)) < 0;                    // bltz a3, L122
  c->mov64(a2, a2);                                 // or a2, a2, r0
  if (bc) {goto block_7;}                           // branch non-likely


  block_6:
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 0, a2);                                 // lq t2, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(a3, 16, a2);                                // lq a3, 16(a2)
  c->daddiu(a0, a0, -4);                            // daddiu a0, a0, -4
  c->lq(t0, 32, a2);                                // lq t0, 32(a2)
  c->daddiu(a1, a1, 64);                            // daddiu a1, a1, 64
  c->lq(t1, 48, a2);                                // lq t1, 48(a2)
  c->daddiu(a2, a2, 64);                            // daddiu a2, a2, 64
  c->sq(t2, -64, a1);                               // sq t2, -64(a1)
  c->daddiu(t2, a0, -4);                            // daddiu t2, a0, -4
  c->sq(a3, -48, a1);                               // sq a3, -48(a1)
  // nop                                            // sll r0, r0, 0
  c->sq(t0, -32, a1);                               // sq t0, -32(a1)
  bc = ((s64)c->sgpr64(t2)) >= 0;                   // bgez t2, L121
  c->sq(t1, -16, a1);                               // sq t1, -16(a1)
  if (bc) {goto block_6;}                           // branch non-likely


  block_7:
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L123
  c->lq(a3, 0, a2);                                 // lq a3, 0(a2)
  if (bc) {goto block_12;}                          // branch non-likely

  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, -16, a1);                               // sq a3, -16(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L123
  c->lq(a3, 0, a2);                                 // lq a3, 0(a2)
  if (bc) {goto block_12;}                          // branch non-likely

  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, -16, a1);                               // sq a3, -16(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L123
  c->lq(a3, 0, a2);                                 // lq a3, 0(a2)
  if (bc) {goto block_12;}                          // branch non-likely

  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, -16, a1);                               // sq a3, -16(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L123
  c->lq(a3, 0, a2);                                 // lq a3, 0(a2)
  if (bc) {goto block_12;}                          // branch non-likely

  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, -16, a1);                               // sq a3, -16(a1)

  block_12:
  c->gprs[a0].du64[0] = 0;                          // or a0, r0, r0
  c->daddiu(a1, s1, 128);                           // daddiu a1, s1, 128
  c->daddiu(a2, s5, 28);                            // daddiu a2, s5, 28
  c->addiu(a0, r0, 8);                              // addiu a0, r0, 8
  c->daddiu(a3, a0, -4);                            // daddiu a3, a0, -4
  c->mov64(a1, a1);                                 // or a1, a1, r0
  bc = ((s64)c->sgpr64(a3)) < 0;                    // bltz a3, L125
  c->mov64(a2, a2);                                 // or a2, a2, r0
  if (bc) {goto block_14;}                          // branch non-likely


  block_13:
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 0, a2);                                 // lq t2, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(a3, 16, a2);                                // lq a3, 16(a2)
  c->daddiu(a0, a0, -4);                            // daddiu a0, a0, -4
  c->lq(t0, 32, a2);                                // lq t0, 32(a2)
  c->daddiu(a1, a1, 64);                            // daddiu a1, a1, 64
  c->lq(t1, 48, a2);                                // lq t1, 48(a2)
  c->daddiu(a2, a2, 64);                            // daddiu a2, a2, 64
  c->sq(t2, -64, a1);                               // sq t2, -64(a1)
  c->daddiu(t2, a0, -4);                            // daddiu t2, a0, -4
  c->sq(a3, -48, a1);                               // sq a3, -48(a1)
  // nop                                            // sll r0, r0, 0
  c->sq(t0, -32, a1);                               // sq t0, -32(a1)
  bc = ((s64)c->sgpr64(t2)) >= 0;                   // bgez t2, L124
  c->sq(t1, -16, a1);                               // sq t1, -16(a1)
  if (bc) {goto block_13;}                          // branch non-likely


  block_14:
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L126
  c->lq(a3, 0, a2);                                 // lq a3, 0(a2)
  if (bc) {goto block_19;}                          // branch non-likely

  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, -16, a1);                               // sq a3, -16(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L126
  c->lq(a3, 0, a2);                                 // lq a3, 0(a2)
  if (bc) {goto block_19;}                          // branch non-likely

  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, -16, a1);                               // sq a3, -16(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L126
  c->lq(a3, 0, a2);                                 // lq a3, 0(a2)
  if (bc) {goto block_19;}                          // branch non-likely

  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, -16, a1);                               // sq a3, -16(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L126
  c->lq(a3, 0, a2);                                 // lq a3, 0(a2)
  if (bc) {goto block_19;}                          // branch non-likely

  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, -16, a1);                               // sq a3, -16(a1)

  block_19:
  c->gprs[a0].du64[0] = 0;                          // or a0, r0, r0
  c->lbu(a0, 17, s2);                               // lbu a0, 17(s2)
  c->andi(a1, a0, 32);                              // andi a1, a0, 32
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L127
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_21;}                          // branch non-likely

  c->addiu(a0, r0, 1);                              // addiu a0, r0, 1
  c->sb(a0, 182, s1);                               // sb a0, 182(s1)
  //beq r0, r0, L129                                // beq r0, r0, L129
  // nop                                            // sll r0, r0, 0
  goto block_24;                                    // branch always


  block_21:
  c->andi(a0, a0, 64);                              // andi a0, a0, 64
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L128
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_23;}                          // branch non-likely

  c->addiu(a0, r0, 2);                              // addiu a0, r0, 2
  c->sb(a0, 182, s1);                               // sb a0, 182(s1)
  //beq r0, r0, L129                                // beq r0, r0, L129
  // nop                                            // sll r0, r0, 0
  goto block_24;                                    // branch always


  block_23:
  c->sb(r0, 182, s1);                               // sb r0, 182(s1)
  c->gprs[a0].du64[0] = 0;                          // or a0, r0, r0

  block_24:
  c->lbu(a0, 26, s2);                               // lbu a0, 26(s2)
  c->lbu(a1, 182, s1);                              // lbu a1, 182(s1)
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L130
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_26;}                          // branch non-likely

  c->addiu(a0, r0, 2432);                           // addiu a0, r0, 2432
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->daddu(a0, a0, a1);                             // daddu a0, a0, a1
  //beq r0, r0, L131                                // beq r0, r0, L131
  // nop                                            // sll r0, r0, 0
  goto block_27;                                    // branch always


  block_26:
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->lwu(a1, 52, a1);                               // lwu a1, 52(a1)
  c->daddiu(a1, a1, 24);                            // daddiu a1, a1, 24
  c->addiu(a2, r0, 48);                             // addiu a2, r0, 48
  c->multu3(a0, a2, a0);                            // multu3 a0, a2, a0
  c->daddu(a0, a1, a0);                             // daddu a0, a1, a0

  block_27:
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->sw(a0, 48, a1);                                // sw a0, 48(a1)
  c->lwu(a1, 4, a0);                                // lwu a1, 4(a0)
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L132
  c->mov64(a2, s7);                                 // or a2, s7, r0
  if (bc) {goto block_29;}                          // branch non-likely

  c->sw(s1, 0, a1);                                 // sw s1, 0(a1)
  c->mov64(a1, s1);                                 // or a1, s1, r0

  block_29:
  c->lwu(a1, 0, a0);                                // lwu a1, 0(a0)
  bc = c->sgpr64(a1) != 0;                          // bne a1, r0, L133
  c->mov64(a1, s7);                                 // or a1, s7, r0
  if (bc) {goto block_31;}                          // branch non-likely

  c->sw(s1, 0, a0);                                 // sw s1, 0(a0)
  c->mov64(a1, s1);                                 // or a1, s1, r0

  block_31:
  c->daddiu(a1, s1, 12);                            // daddiu a1, s1, 12
  c->sw(a1, 4, a0);                                 // sw a1, 4(a0)
  c->dsll(a0, s4, 3);                               // dsll a0, s4, 3
  c->daddiu(a0, a0, 124);                           // daddiu a0, a0, 124
  c->daddu(a0, a0, v1);                             // daddu a0, a0, v1
  c->lwu(a1, 0, a0);                                // lwu a1, 0(a0)
  c->sw(a1, 160, s1);                               // sw a1, 160(s1)
  c->lbu(a1, 5, a0);                                // lbu a1, 5(a0)
  c->sb(a1, 183, s1);                               // sb a1, 183(s1)
  c->lbu(a0, 7, a0);                                // lbu a0, 7(a0)
  c->sb(a0, 186, s1);                               // sb a0, 186(s1)
  c->lw(v1, 112, v1);                               // lw v1, 112(v1)
  c->sb(v1, 168, s1);                               // sb v1, 168(s1)
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(v1, 16, v1);                               // lwu v1, 16(v1)
  c->sb(v1, 169, s1);                               // sb v1, 169(s1)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L134
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_33;}                          // branch non-likely

  c->sb(r0, 168, s1);                               // sb r0, 168(s1)
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0

  block_33:
  c->sb(r0, 170, s1);                               // sb r0, 170(s1)
  c->addiu(v1, r0, 1);                              // addiu v1, r0, 1
  c->sb(v1, 171, s1);                               // sb v1, 171(s1)
  c->sh(r0, 172, s1);                               // sh r0, 172(s1)
  c->addiu(v1, r0, 255);                            // addiu v1, r0, 255
  c->sb(v1, 184, s1);                               // sb v1, 184(s1)
  c->addiu(v1, r0, 0);                              // addiu v1, r0, 0
  c->lbu(a0, 17, s2);                               // lbu a0, 17(s2)
  c->andi(a0, a0, 2);                               // andi a0, a0, 2
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L135
  c->mov64(a0, s7);                                 // or a0, s7, r0
  if (bc) {goto block_35;}                          // branch non-likely

  c->addiu(v1, r0, 1);                              // addiu v1, r0, 1
  c->mov64(a0, v1);                                 // or a0, v1, r0

  block_35:
  c->lhu(a0, 4, gp);                                // lhu a0, 4(gp)
  c->andi(a0, a0, 1024);                            // andi a0, a0, 1024
  if (((s64)c->sgpr64(a0)) != ((s64)0)) {           // bnel a0, r0, L136
    c->daddiu(a0, s7, 4);                           // daddiu a0, s7, 4
    goto block_40;
  }

  c->lhu(a0, 4, gp);                                // lhu a0, 4(gp)
  c->andi(a0, a0, 2048);                            // andi a0, a0, 2048
  if (((s64)c->sgpr64(a0)) == ((s64)0)) {           // beql a0, r0, L136
    c->mov64(a0, s7);                               // or a0, s7, r0
    goto block_40;
  }

  c->daddiu(a0, s7, 4);                             // daddiu a0, s7, 4
  c->lbu(a1, 17, s2);                               // lbu a1, 17(s2)
  c->andi(a1, a1, 64);                              // andi a1, a1, 64
  c->movz(a0, s7, a1);                              // movz a0, s7, a1

  block_40:
  bc = c->sgpr64(s7) == c->sgpr64(a0);              // beq s7, a0, L137
  c->mov64(a0, s7);                                 // or a0, s7, r0
  if (bc) {goto block_42;}                          // branch non-likely

  c->lbu(v1, 206, gp);                              // lbu v1, 206(gp)
  c->sb(v1, 184, s1);                               // sb v1, 184(s1)
  c->addiu(v1, r0, 1);                              // addiu v1, r0, 1
  c->mov64(a0, v1);                                 // or a0, v1, r0

  block_42:
  c->sb(v1, 180, s1);                               // sb v1, 180(s1)
  c->lbu(v1, 17, s2);                               // lbu v1, 17(s2)
  c->andi(v1, v1, 64);                              // andi v1, v1, 64
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L138
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_44;}                          // branch non-likely

  c->addiu(v1, r0, 4);                              // addiu v1, r0, 4
  c->sb(v1, 185, s1);                               // sb v1, 185(s1)
  //beq r0, r0, L140                                // beq r0, r0, L140
  // nop                                            // sll r0, r0, 0
  goto block_47;                                    // branch always


  block_44:
  c->lhu(v1, 4, gp);                                // lhu v1, 4(gp)
  c->andi(v1, v1, 8192);                            // andi v1, v1, 8192
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L139
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_46;}                          // branch non-likely

  c->addiu(v1, r0, 4);                              // addiu v1, r0, 4
  c->sb(v1, 185, s1);                               // sb v1, 185(s1)
  //beq r0, r0, L140                                // beq r0, r0, L140
  // nop                                            // sll r0, r0, 0
  goto block_47;                                    // branch always


  block_46:
  c->sb(r0, 185, s1);                               // sb r0, 185(s1)
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0

  block_47:
  c->lbu(v1, 92, gp);                               // lbu v1, 92(gp)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L141
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_49;}                          // branch non-likely

  c->load_symbol2(t9, cache.foreground_generic_merc_death);// lw t9, foreground-generic-merc-death(s7)
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->mov64(a1, s1);                                 // or a1, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0

  block_49:
  c->sw(r0, 164, s1);                               // sw r0, 164(s1)
  c->lbu(v1, 17, s2);                               // lbu v1, 17(s2)
  c->andi(v1, v1, 4);                               // andi v1, v1, 4
  if (((s64)c->sgpr64(v1)) == ((s64)0)) {           // beql v1, r0, L142
    c->mov64(v1, s7);                               // or v1, s7, r0
    goto block_54;
  }

  c->lwu(v1, 80, gp);                               // lwu v1, 80(gp)
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(v1))) {// beql s7, v1, L142
    c->mov64(v1, v1);                               // or v1, v1, r0
    goto block_54;
  }

  c->lwu(v1, 80, gp);                               // lwu v1, 80(gp)
  c->lwu(v1, 28, v1);                               // lwu v1, 28(v1)

  block_54:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L143
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_56;}                          // branch non-likely

  c->lwu(v1, 80, gp);                               // lwu v1, 80(gp)
  c->lwu(v1, 32, v1);                               // lwu v1, 32(v1)
  c->sw(v1, 164, s1);                               // sw v1, 164(s1)

  block_56:
  c->daddiu(a0, s1, 256);                           // daddiu a0, s1, 256
  c->mov64(a1, s2);                                 // or a1, s2, r0
  c->addiu(v1, r0, 2);                              // addiu v1, r0, 2
  c->daddiu(a2, v1, -4);                            // daddiu a2, v1, -4
  c->mov64(a0, a0);                                 // or a0, a0, r0
  bc = ((s64)c->sgpr64(a2)) < 0;                    // bltz a2, L145
  c->mov64(a1, a1);                                 // or a1, a1, r0
  if (bc) {goto block_58;}                          // branch non-likely


  block_57:
  // nop                                            // sll r0, r0, 0
  c->lq(t1, 0, a1);                                 // lq t1, 0(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(a2, 16, a1);                                // lq a2, 16(a1)
  c->daddiu(v1, v1, -4);                            // daddiu v1, v1, -4
  c->lq(a3, 32, a1);                                // lq a3, 32(a1)
  c->daddiu(a0, a0, 64);                            // daddiu a0, a0, 64
  c->lq(t0, 48, a1);                                // lq t0, 48(a1)
  c->daddiu(a1, a1, 64);                            // daddiu a1, a1, 64
  c->sq(t1, -64, a0);                               // sq t1, -64(a0)
  c->daddiu(t1, v1, -4);                            // daddiu t1, v1, -4
  c->sq(a2, -48, a0);                               // sq a2, -48(a0)
  // nop                                            // sll r0, r0, 0
  c->sq(a3, -32, a0);                               // sq a3, -32(a0)
  bc = ((s64)c->sgpr64(t1)) >= 0;                   // bgez t1, L144
  c->sq(t0, -16, a0);                               // sq t0, -16(a0)
  if (bc) {goto block_57;}                          // branch non-likely


  block_58:
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L146
  c->lq(a2, 0, a1);                                 // lq a2, 0(a1)
  if (bc) {goto block_63;}                          // branch non-likely

  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, 16);                            // daddiu a0, a0, 16
  c->daddiu(v1, v1, -1);                            // daddiu v1, v1, -1
  c->sq(a2, -16, a0);                               // sq a2, -16(a0)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L146
  c->lq(a2, 0, a1);                                 // lq a2, 0(a1)
  if (bc) {goto block_63;}                          // branch non-likely

  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, 16);                            // daddiu a0, a0, 16
  c->daddiu(v1, v1, -1);                            // daddiu v1, v1, -1
  c->sq(a2, -16, a0);                               // sq a2, -16(a0)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L146
  c->lq(a2, 0, a1);                                 // lq a2, 0(a1)
  if (bc) {goto block_63;}                          // branch non-likely

  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, 16);                            // daddiu a0, a0, 16
  c->daddiu(v1, v1, -1);                            // daddiu v1, v1, -1
  c->sq(a2, -16, a0);                               // sq a2, -16(a0)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L146
  c->lq(a2, 0, a1);                                 // lq a2, 0(a1)
  if (bc) {goto block_63;}                          // branch non-likely

  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, 16);                            // daddiu a0, a0, 16
  c->daddiu(v1, v1, -1);                            // daddiu v1, v1, -1
  c->sq(a2, -16, a0);                               // sq a2, -16(a0)

  block_63:
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->addiu(v1, r0, 18);                             // addiu v1, r0, 18
  c->lwu(a0, 28, s2);                               // lwu a0, 28(s2)
  if (((s64)c->sgpr64(a0)) == ((s64)0)) {           // beql a0, r0, L147
    c->mov64(a0, s7);                               // or a0, s7, r0
    goto block_66;
  }

  c->daddiu(a0, s7, 4);                             // daddiu a0, s7, 4
  c->lwu(a1, 28, s2);                               // lwu a1, 28(s2)
  c->lbu(a1, 1, a1);                                // lbu a1, 1(a1)
  c->movz(a0, s7, a1);                              // movz a0, s7, a1

  block_66:
  bc = c->sgpr64(s7) == c->sgpr64(a0);              // beq s7, a0, L151
  c->mov64(a0, s7);                                 // or a0, s7, r0
  if (bc) {goto block_75;}                          // branch non-likely

  c->addiu(a0, r0, 1);                              // addiu a0, r0, 1
  c->sb(a0, 170, s1);                               // sb a0, 170(s1)
  c->daddiu(a1, s1, 288);                           // daddiu a1, s1, 288
  c->lwu(a0, 28, s2);                               // lwu a0, 28(s2)
  c->lwu(a2, 28, s2);                               // lwu a2, 28(s2)
  c->lbu(a2, 1, a2);                                // lbu a2, 1(a2)
  c->dsll(a2, a2, 4);                               // dsll a2, a2, 4
  c->daddu(a2, a0, a2);                             // daddu a2, a0, a2
  c->addiu(a0, r0, 5);                              // addiu a0, r0, 5
  c->daddiu(a3, a0, -4);                            // daddiu a3, a0, -4
  c->mov64(a1, a1);                                 // or a1, a1, r0
  bc = ((s64)c->sgpr64(a3)) < 0;                    // bltz a3, L149
  c->mov64(a2, a2);                                 // or a2, a2, r0
  if (bc) {goto block_69;}                          // branch non-likely


  block_68:
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 0, a2);                                 // lq t2, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(a3, 16, a2);                                // lq a3, 16(a2)
  c->daddiu(a0, a0, -4);                            // daddiu a0, a0, -4
  c->lq(t0, 32, a2);                                // lq t0, 32(a2)
  c->daddiu(a1, a1, 64);                            // daddiu a1, a1, 64
  c->lq(t1, 48, a2);                                // lq t1, 48(a2)
  c->daddiu(a2, a2, 64);                            // daddiu a2, a2, 64
  c->sq(t2, -64, a1);                               // sq t2, -64(a1)
  c->daddiu(t2, a0, -4);                            // daddiu t2, a0, -4
  c->sq(a3, -48, a1);                               // sq a3, -48(a1)
  // nop                                            // sll r0, r0, 0
  c->sq(t0, -32, a1);                               // sq t0, -32(a1)
  bc = ((s64)c->sgpr64(t2)) >= 0;                   // bgez t2, L148
  c->sq(t1, -16, a1);                               // sq t1, -16(a1)
  if (bc) {goto block_68;}                          // branch non-likely


  block_69:
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L150
  c->lq(a3, 0, a2);                                 // lq a3, 0(a2)
  if (bc) {goto block_74;}                          // branch non-likely

  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, -16, a1);                               // sq a3, -16(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L150
  c->lq(a3, 0, a2);                                 // lq a3, 0(a2)
  if (bc) {goto block_74;}                          // branch non-likely

  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, -16, a1);                               // sq a3, -16(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L150
  c->lq(a3, 0, a2);                                 // lq a3, 0(a2)
  if (bc) {goto block_74;}                          // branch non-likely

  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, -16, a1);                               // sq a3, -16(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L150
  c->lq(a3, 0, a2);                                 // lq a3, 0(a2)
  if (bc) {goto block_74;}                          // branch non-likely

  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, -16, a1);                               // sq a3, -16(a1)

  block_74:
  c->gprs[a0].du64[0] = 0;                          // or a0, r0, r0
  c->daddiu(v1, v1, 5);                             // daddiu v1, v1, 5
  c->mov64(a0, v1);                                 // or a0, v1, r0

  block_75:
  c->lui(a0, 4096);                                 // lui a0, 4096
  c->daddiu(a1, v1, -1);                            // daddiu a1, v1, -1
  c->dsll32(a1, a1, 16);                            // dsll32 a1, a1, 16
  c->dsrl32(a1, a1, 16);                            // dsrl32 a1, a1, 16
  c->or_(a0, a0, a1);                               // or a0, a0, a1
  c->sd(a0, 0, s1);                                 // sd a0, 0(s1)
  c->sw(v1, 8, s1);                                 // sw v1, 8(s1)
  c->sw(r0, 12, s1);                                // sw r0, 12(s1)
  c->dsll(v1, v1, 4);                               // dsll v1, v1, 4
  c->daddu(a1, s3, v1);                             // daddu a1, s3, v1
  c->mov64(v1, a1);                                 // or v1, a1, r0
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(a2, 48, v1);                               // lwu a2, 48(v1)
  c->load_symbol2(t9, cache.foreground_generic_merc_add_fragments);// lw t9, foreground-generic-merc-add-fragments(s7)
  c->mov64(a0, s2);                                 // or a0, s2, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(s3, v0);                                 // or s3, v0, r0
  c->mov64(v1, s3);                                 // or v1, s3, r0

  block_76:
  c->daddiu(s4, s4, 1);                             // daddiu s4, s4, 1

  block_77:
  c->lwu(v1, 52, s5);                               // lwu v1, 52(s5)
  c->slt(v1, s4, v1);                               // slt v1, s4, v1
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L119
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v0, s3);                                 // or v0, s3, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 96, sp);                                // lq gp, 96(sp)
  c->lq(s5, 80, sp);                                // lq s5, 80(sp)
  c->lq(s4, 64, sp);                                // lq s4, 64(sp)
  c->lq(s3, 48, sp);                                // lq s3, 48(sp)
  c->lq(s2, 32, sp);                                // lq s2, 32(sp)
  c->lq(s1, 16, sp);                                // lq s1, 16(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 112);                           // daddiu sp, sp, 112
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  cache.foreground = intern_from_c("*foreground*").c();
  cache.merc_global_stats = intern_from_c("*merc-global-stats*").c();
  cache.foreground_generic_merc_add_fragments = intern_from_c("foreground-generic-merc-add-fragments").c();
  cache.foreground_generic_merc_death = intern_from_c("foreground-generic-merc-death").c();
  gLinkedFunctionTable.reg("foreground-generic-merc", execute, 256);
}

} // namespace foreground_generic_merc
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace foreground_draw_hud {
struct Cache {
  void* bone_calculation_list; // *bone-calculation-list*
  void* display; // *display*
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* foreground; // *foreground*
  void* foreground_generic_merc; // foreground-generic-merc
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->lwu(a2, 4, a1);                                // lwu a2, 4(a1)
  c->daddiu(v1, a2, 64);                            // daddiu v1, a2, 64
  c->andi(a3, v1, 48);                              // andi a3, v1, 48
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L2
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  c->daddiu(v1, v1, 64);                            // daddiu v1, v1, 64
  c->dsubu(v1, v1, a3);                             // dsubu v1, v1, a3

block_2:
  c->lwu(a3, 16, a0);                               // lwu a3, 16(a0)
  c->lw(a3, 16, a3);                                // lw a3, 16(a3)
  c->daddiu(t1, a3, 3);                             // daddiu t1, a3, 3
  c->dsll(a3, t1, 7);                               // dsll a3, t1, 7
  get_fake_spad_addr2(t0, cache.fake_scratchpad_data, 0, c);// lui t0, 28672
  c->daddu(t0, r0, t0);                             // daddu t0, r0, t0
  c->lwu(t2, 12, a0);                               // lwu t2, 12(a0)
  c->lwu(t2, 28, t2);                               // lwu t2, 28(t2)
  c->sw(t2, 24, t0);                                // sw t2, 24(t0)
  c->lwu(t2, 24, a0);                               // lwu t2, 24(a0)
  c->daddiu(t2, t2, 12);                            // daddiu t2, t2, 12
  c->sw(t2, 28, t0);                                // sw t2, 28(t0)
  c->sw(t1, 32, t0);                                // sw t1, 32(t0)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 0, t0);                               // swc1 f0, 0(t0)
  c->sw(a1, 40, t0);                                // sw a1, 40(t0)
  c->addiu(a1, r0, 336);                            // addiu a1, r0, 336
  c->lbu(t1, 90, a0);                               // lbu t1, 90(a0)
  get_fake_spad_addr2(t2, cache.fake_scratchpad_data, 0, c);// lui t2, 28672
  c->daddu(t1, t1, t2);                             // daddu t1, t1, t2
  c->lbu(t1, 64, t1);                               // lbu t1, 64(t1)
  c->multu3(a1, a1, t1);                            // multu3 a1, a1, t1
  c->daddiu(a1, a1, 80);                            // daddiu a1, a1, 80
  get_fake_spad_addr2(t1, cache.fake_scratchpad_data, 0, c);// lui t1, 28672
  c->daddu(a1, a1, t1);                             // daddu a1, a1, t1
  c->sw(a1, 52, t0);                                // sw a1, 52(t0)
  c->mov64(t1, v1);                                 // or t1, v1, r0
  c->addiu(t2, r0, 2);                              // addiu t2, r0, 2
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->daddu(t5, r0, a1);                             // daddu t5, r0, a1
  c->load_symbol2(a1, cache.bone_calculation_list); // lw a1, *bone-calculation-list*(s7)
  c->mov64(t0, a2);                                 // or t0, a2, r0
  c->lwu(t3, 24, t5);                               // lwu t3, 24(t5)
  c->lwu(t4, 28, t5);                               // lwu t4, 28(t5)
  c->lwu(t5, 32, t5);                               // lwu t5, 32(t5)
  c->mov64(t6, t0);                                 // or t6, t0, r0
  c->sh(t2, 0, t6);                                 // sh t2, 0(t6)
  c->sh(t5, 2, t6);                                 // sh t5, 2(t6)
  c->sw(t1, 4, t6);                                 // sw t1, 4(t6)
  c->sw(t3, 8, t6);                                 // sw t3, 8(t6)
  c->sw(t4, 12, t6);                                // sw t4, 12(t6)
  c->sw(r0, 32, t6);                                // sw r0, 32(t6)
  c->lwu(t1, 4, a1);                                // lwu t1, 4(a1)
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L3
  c->mov64(t1, s7);                                 // or t1, s7, r0
  if (bc) {goto block_4;}                           // branch non-likely

  c->lwu(t1, 4, a1);                                // lwu t1, 4(a1)
  c->sw(t0, 32, t1);                                // sw t0, 32(t1)
  c->mov64(t1, t0);                                 // or t1, t0, r0

block_4:
  c->lwu(t1, 0, a1);                                // lwu t1, 0(a1)
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L4
  c->mov64(t1, s7);                                 // or t1, s7, r0
  if (bc) {goto block_6;}                           // branch non-likely

  c->sw(t0, 0, a1);                                 // sw t0, 0(a1)
  c->mov64(t1, t0);                                 // or t1, t0, r0

block_6:
  c->sw(t0, 4, a1);                                 // sw t0, 4(a1)
  c->daddiu(a1, a2, 48);                            // daddiu a1, a2, 48
  c->daddu(a1, v1, a3);                             // daddu a1, v1, a3
  c->sw(v1, 36, at);                                // sw v1, 36(at)
  c->load_symbol2(v1, cache.foreground);            // lw v1, *foreground*(s7)
  c->daddiu(v1, v1, 2384);                          // daddiu v1, v1, 2384
  c->addiu(a2, r0, 1);                              // addiu a2, r0, 1
  c->lbu(a3, 6, a0);                                // lbu a3, 6(a0)
  bc = c->sgpr64(a3) != c->sgpr64(a2);              // bne a3, a2, L14
  c->mov64(a2, s7);                                 // or a2, s7, r0
  if (bc) {goto block_25;}                          // branch non-likely

  c->lwu(a2, 28, a0);                               // lwu a2, 28(a0)
  c->addiu(a3, r0, 0);                              // addiu a3, r0, 0
  //beq r0, r0, L13                                 // beq r0, r0, L13
  // nop                                            // sll r0, r0, 0
  goto block_22;                                    // branch always


block_8:
  c->dsll(t0, a3, 5);                               // dsll t0, a3, 5
  c->daddiu(t0, t0, 156);                           // daddiu t0, t0, 156
  c->daddu(t0, t0, a2);                             // daddu t0, t0, a2
  c->lbu(t0, 27, t0);                               // lbu t0, 27(t0)
  c->andi(t0, t0, 1);                               // andi t0, t0, 1
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L9
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_17;}                          // branch non-likely

  c->daddu(t1, r0, v1);                             // daddu t1, r0, v1
  c->addiu(t0, r0, 2480);                           // addiu t0, r0, 2480
  get_fake_spad_addr2(t2, cache.fake_scratchpad_data, 0, c);// lui t2, 28672
  c->daddu(t2, t0, t2);                             // daddu t2, t0, t2
  c->addiu(t0, r0, 7);                              // addiu t0, r0, 7
  c->daddiu(t3, t0, -4);                            // daddiu t3, t0, -4
  c->mov64(t1, t1);                                 // or t1, t1, r0
  bc = ((s64)c->sgpr64(t3)) < 0;                    // bltz t3, L7
  c->mov64(t2, t2);                                 // or t2, t2, r0
  if (bc) {goto block_11;}                          // branch non-likely


block_10:
  // nop                                            // sll r0, r0, 0
  c->lq(t6, 0, t2);                                 // lq t6, 0(t2)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 16, t2);                                // lq t3, 16(t2)
  c->daddiu(t0, t0, -4);                            // daddiu t0, t0, -4
  c->lq(t4, 32, t2);                                // lq t4, 32(t2)
  c->daddiu(t1, t1, 64);                            // daddiu t1, t1, 64
  c->lq(t5, 48, t2);                                // lq t5, 48(t2)
  c->daddiu(t2, t2, 64);                            // daddiu t2, t2, 64
  c->sq(t6, -64, t1);                               // sq t6, -64(t1)
  c->daddiu(t6, t0, -4);                            // daddiu t6, t0, -4
  c->sq(t3, -48, t1);                               // sq t3, -48(t1)
  // nop                                            // sll r0, r0, 0
  c->sq(t4, -32, t1);                               // sq t4, -32(t1)
  bc = ((s64)c->sgpr64(t6)) >= 0;                   // bgez t6, L6
  c->sq(t5, -16, t1);                               // sq t5, -16(t1)
  if (bc) {goto block_10;}                          // branch non-likely


block_11:
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L8
  c->lq(t3, 0, t2);                                 // lq t3, 0(t2)
  if (bc) {goto block_16;}                          // branch non-likely

  c->daddiu(t2, t2, 16);                            // daddiu t2, t2, 16
  c->daddiu(t1, t1, 16);                            // daddiu t1, t1, 16
  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  c->sq(t3, -16, t1);                               // sq t3, -16(t1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L8
  c->lq(t3, 0, t2);                                 // lq t3, 0(t2)
  if (bc) {goto block_16;}                          // branch non-likely

  c->daddiu(t2, t2, 16);                            // daddiu t2, t2, 16
  c->daddiu(t1, t1, 16);                            // daddiu t1, t1, 16
  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  c->sq(t3, -16, t1);                               // sq t3, -16(t1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L8
  c->lq(t3, 0, t2);                                 // lq t3, 0(t2)
  if (bc) {goto block_16;}                          // branch non-likely

  c->daddiu(t2, t2, 16);                            // daddiu t2, t2, 16
  c->daddiu(t1, t1, 16);                            // daddiu t1, t1, 16
  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  c->sq(t3, -16, t1);                               // sq t3, -16(t1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L8
  c->lq(t3, 0, t2);                                 // lq t3, 0(t2)
  if (bc) {goto block_16;}                          // branch non-likely

  c->daddiu(t2, t2, 16);                            // daddiu t2, t2, 16
  c->daddiu(t1, t1, 16);                            // daddiu t1, t1, 16
  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  c->sq(t3, -16, t1);                               // sq t3, -16(t1)

block_16:
  c->gprs[t0].du64[0] = 0;                          // or t0, r0, r0
  // Unknown instr: ld t0, L216(fp)
  // L216:
  // .word 0x80808080
  // .word 0x0
  c->gprs[t0].du32[0] = 0x80808080;
  c->gprs[t0].du32[1] = 0x0;
  c->dsll(t1, a3, 3);                               // dsll t1, a3, 3
  c->daddu(t1, v1, t1);                             // daddu t1, v1, t1
  c->sw(t0, 124, t1);                               // sw t0, 124(t1)
  c->addiu(t0, r0, 2);                              // addiu t0, r0, 2
  c->dsll(t1, a3, 3);                               // dsll t1, a3, 3
  c->daddu(t1, v1, t1);                             // daddu t1, v1, t1
  c->sb(t0, 128, t1);                               // sb t0, 128(t1)
  //beq r0, r0, L10                                 // beq r0, r0, L10
  // nop                                            // sll r0, r0, 0
  goto block_18;                                    // branch always


block_17:
  c->addiu(t0, r0, 2);                              // addiu t0, r0, 2
  c->dsll(t1, a3, 3);                               // dsll t1, a3, 3
  c->daddu(t1, v1, t1);                             // daddu t1, v1, t1
  c->sb(t0, 128, t1);                               // sb t0, 128(t1)

block_18:
  c->dsll(t0, a3, 3);                               // dsll t0, a3, 3
  c->daddu(t0, v1, t0);                             // daddu t0, v1, t0
  c->sb(r0, 130, t0);                               // sb r0, 130(t0)
  c->dsll(t0, a3, 5);                               // dsll t0, a3, 5
  c->daddu(t0, a2, t0);                             // daddu t0, a2, t0
  c->lbu(t0, 173, t0);                              // lbu t0, 173(t0)
  c->andi(t0, t0, 128);                             // andi t0, t0, 128
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L11
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_20;}                          // branch non-likely

  c->addiu(t0, r0, 1);                              // addiu t0, r0, 1
  //beq r0, r0, L12                                 // beq r0, r0, L12
  // nop                                            // sll r0, r0, 0
  goto block_21;                                    // branch always


block_20:
  c->addiu(t0, r0, 0);                              // addiu t0, r0, 0

block_21:
  c->dsll(t1, a3, 3);                               // dsll t1, a3, 3
  c->daddu(t1, v1, t1);                             // daddu t1, v1, t1
  c->sb(t0, 129, t1);                               // sb t0, 129(t1)
  c->daddiu(a3, a3, 1);                             // daddiu a3, a3, 1

block_22:
  c->lwu(t0, 52, a2);                               // lwu t0, 52(a2)
  c->slt(t0, a3, t0);                               // slt t0, a3, t0
  bc = c->sgpr64(t0) != 0;                          // bne t0, r0, L5
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_8;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->load_symbol2(v1, cache.display);               // lw v1, *display*(s7)
  c->ld(v1, 132, v1);                               // ld v1, 132(v1)
  c->andi(v1, v1, 4096);                            // andi v1, v1, 4096
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L14
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_25;}                          // branch non-likely

  c->load_symbol2(t9, cache.foreground_generic_merc);// lw t9, foreground-generic-merc(s7)
  c->addiu(a2, r0, 1);                              // addiu a2, r0, 1
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(a1, v0);                                 // or a1, v0, r0
  c->mov64(v1, a1);                                 // or v1, a1, r0

block_25:
  c->andi(v1, a1, 48);                              // andi v1, a1, 48
  c->mov128_gpr_gpr(a0, r0);                        // por a0, r0, r0
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L15
  c->lui(a0, 8192);                                 // lui a0, 8192
  if (bc) {goto block_27;}                          // branch non-likely

  c->sq(a0, 0, a1);                                 // sq a0, 0(a1)
  c->mov64(a0, a1);                                 // or a0, a1, r0
  c->dsubu(v1, a1, v1);                             // dsubu v1, a1, v1
  c->daddiu(a1, v1, 64);                            // daddiu a1, v1, 64
  c->sw(a1, 4, a0);                                 // sw a1, 4(a0)

block_27:
  c->lui(v1, 4095);                                 // lui v1, 4095
  c->ori(v1, v1, 65535);                            // ori v1, v1, 65535
  c->and_(v1, a1, v1);                              // and v1, a1, v1
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->lwu(a0, 40, a0);                               // lwu a0, 40(a0)
  c->sw(v1, 4, a0);                                 // sw v1, 4(a0)
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
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
  cache.bone_calculation_list = intern_from_c("*bone-calculation-list*").c();
  cache.display = intern_from_c("*display*").c();
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  cache.foreground = intern_from_c("*foreground*").c();
  cache.foreground_generic_merc = intern_from_c("foreground-generic-merc").c();
  gLinkedFunctionTable.reg("foreground-draw-hud", execute, 256);
}

} // namespace foreground_draw_hud
} // namespace Mips2C

