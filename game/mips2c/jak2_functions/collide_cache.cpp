//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace method_10_collide_shape_prim_mesh {
struct Cache {
  void* cheat_mode; // *cheat-mode*
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* debug; // debug
  void* format; // format
  void* print_exceeded_max_cache_tris; // print-exceeded-max-cache-tris
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -96);                           // daddiu sp, sp, -96
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  c->sq(s2, 16, sp);                                // sq s2, 16(sp)
  c->sq(s3, 32, sp);                                // sq s3, 32(sp)
  c->sq(s4, 48, sp);                                // sq s4, 48(sp)
  c->sq(s5, 64, sp);                                // sq s5, 64(sp)
  c->sq(gp, 80, sp);                                // sq gp, 80(sp)
  c->mov64(s5, a0);                                 // or s5, a0, r0
  c->mov64(gp, a1);                                 // or gp, a1, r0
  // nop                                            // sll r0, r0, 0
  c->lwu(s3, 60, s5);                               // lwu s3, 60(s5)
  c->daddiu(v1, gp, 108);                           // daddiu v1, gp, 108
  c->lwu(a0, 4, gp);                                // lwu a0, 4(gp)
  bc = c->sgpr64(s3) == c->sgpr64(s7);              // beq s3, s7, L91
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_14;}                          // branch non-likely

  c->addiu(a1, r0, 100);                            // addiu a1, r0, 100
  c->dsll(a2, a0, 1);                               // dsll a2, a0, 1
  bc = c->sgpr64(a0) == c->sgpr64(a1);              // beq a0, a1, L89
  c->daddu(a0, a2, a0);                             // daddu a0, a2, a0
  if (bc) {goto block_11;}                          // branch non-likely

  c->dsll(a0, a0, 4);                               // dsll a0, a0, 4
  c->daddu(s4, v1, a0);                             // daddu s4, v1, a0
  c->mov64(a0, s3);                                 // or a0, s3, r0
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 72, v1);                               // lwu t9, 72(v1)
  c->lwu(v1, 0, s5);                                // lwu v1, 0(s5)
  c->lwu(v1, 136, v1);                              // lwu v1, 136(v1)
  c->lwu(v1, 128, v1);                              // lwu v1, 128(v1)
  c->lb(a1, 8, s5);                                 // lb a1, 8(s5)
  c->dsll(a1, a1, 5);                               // dsll a1, a1, 5
  c->daddu(v1, v1, a1);                             // daddu v1, v1, a1
  c->lwu(v1, 28, v1);                               // lwu v1, 28(v1)
  c->daddu(a1, r0, v1);                             // daddu a1, r0, v1
  get_fake_spad_addr2(a2, cache.fake_scratchpad_data, 0, c);// lui a2, 28672
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lwu(a1, 4, gp);                                // lwu a1, 4(gp)
  c->addiu(a2, r0, 460);                            // addiu a2, r0, 460
  c->lwu(v1, 0, gp);                                // lwu v1, 0(gp)
  c->dsll32(a0, a1, 0);                             // dsll32 a0, a1, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf1, a0);                        // qmtc2.i vf1, a0
  // nop                                            // sll r0, r0, 0
  c->dsubu(a0, a2, v1);                             // dsubu a0, a2, v1
  c->dsll(t2, v1, 6);                               // dsll t2, v1, 6
  bc = ((s64)c->sgpr64(a0)) < 0;                    // bltz a0, L90
  c->daddiu(a3, s3, 28);                            // daddiu a3, s3, 28
  if (bc) {goto block_12;}                          // branch non-likely

  c->mov64(a0, v1);                                 // or a0, v1, r0
  c->lwu(t0, 4, s3);                                // lwu t0, 4(s3)
  c->daddiu(t3, gp, 4908);                          // daddiu t3, gp, 4908
  c->lq(t1, 60, gp);                                // lq t1, 60(gp)
  c->daddu(t2, t3, t2);                             // daddu t2, t3, t2
  c->lq(t3, 76, gp);                                // lq t3, 76(gp)
  get_fake_spad_addr2(t4, cache.fake_scratchpad_data, 0, c);// lui t4, 28672
  c->lwu(t5, 8, gp);                                // lwu t5, 8(gp)
  c->vsub(DEST::zw, vf1, vf0, vf0);                 // vsub.zw vf1, vf0, vf0
  // nop                                            // sll r0, r0, 0

  block_4:
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L88
  c->lbu(t6, 0, a3);                                // lbu t6, 0(a3)
  if (bc) {goto block_9;}                           // branch non-likely

  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  c->lbu(t7, 1, a3);                                // lbu t7, 1(a3)
  c->dsll(t9, t6, 5);                               // dsll t9, t6, 5
  c->lbu(t6, 2, a3);                                // lbu t6, 2(a3)
  c->dsll(t8, t7, 5);                               // dsll t8, t7, 5
  c->daddu(t7, t9, t4);                             // daddu t7, t9, t4
  c->dsll(t6, t6, 5);                               // dsll t6, t6, 5
  // nop                                            // sll r0, r0, 0
  c->daddu(t8, t8, t4);                             // daddu t8, t8, t4
  c->lq(ra, 16, t7);                                // lq ra, 16(t7)
  c->daddu(t6, t6, t4);                             // daddu t6, t6, t4
  c->lq(s2, 16, t8);                                // lq s2, 16(t8)
  c->pminw(s3, ra, s2);                             // pminw s3, ra, s2
  c->lq(t9, 16, t6);                                // lq t9, 16(t6)
  c->pmaxw(ra, ra, s2);                             // pmaxw ra, ra, s2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pminw(s3, s3, t9);                             // pminw s3, s3, t9
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaxw(t9, ra, t9);                             // pmaxw t9, ra, t9
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(ra, s3, t3);                             // pcgtw ra, s3, t3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(s3, t1, t9);                             // pcgtw s3, t1, t9
  c->lwu(t9, 4, a3);                                // lwu t9, 4(a3)
  c->por(ra, ra, s3);                               // por ra, ra, s3
  c->lq(t7, 0, t7);                                 // lq t7, 0(t7)
  c->ppach(ra, r0, ra);                             // ppach ra, r0, ra
  c->lq(t8, 0, t8);                                 // lq t8, 0(t8)
  c->dsll(ra, ra, 16);                              // dsll ra, ra, 16
  c->lq(t6, 0, t6);                                 // lq t6, 0(t6)
  bc = c->sgpr64(ra) != 0;                          // bne ra, r0, L87
  c->daddiu(a3, a3, 8);                             // daddiu a3, a3, 8
  if (bc) {goto block_4;}                           // branch non-likely

  bc = c->sgpr64(v1) == c->sgpr64(a2);              // beq v1, a2, L90
  c->sqc2(vf1, 48, t2);                             // sqc2 vf1, 48(t2)
  if (bc) {goto block_12;}                          // branch non-likely

  c->and_(ra, t9, t5);                              // and ra, t9, t5
  c->sw(t9, 48, t2);                                // sw t9, 48(t2)
  c->sw(s5, 52, t2);                                // sw s5, 52(t2)
  c->sh(a1, 56, t2);                                // sh a1, 56(t2)
  bc = c->sgpr64(ra) != 0;                          // bne ra, r0, L87
  c->sq(t7, 0, t2);                                 // sq t7, 0(t2)
  if (bc) {goto block_4;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->sq(t8, 16, t2);                                // sq t8, 16(t2)
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  c->sq(t6, 32, t2);                                // sq t6, 32(t2)
  //beq r0, r0, L87                                 // beq r0, r0, L87
  c->daddiu(t2, t2, 64);                            // daddiu t2, t2, 64
  goto block_4;                                     // branch always


  block_9:
  c->dsubu(a3, v1, a0);                             // dsubu a3, v1, a0
  c->lwu(t0, 4, gp);                                // lwu t0, 4(gp)
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L91
  c->lq(a1, 12, s5);                                // lq a1, 12(s5)
  if (bc) {goto block_14;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lq(a2, 28, s5);                                // lq a2, 28(s5)
  // nop                                            // sll r0, r0, 0
  c->sq(r0, 32, s4);                                // sq r0, 32(s4)
  // nop                                            // sll r0, r0, 0
  c->sh(a3, 42, s4);                                // sh a3, 42(s4)
  // nop                                            // sll r0, r0, 0
  c->sw(gp, 32, s4);                                // sw gp, 32(s4)
  // nop                                            // sll r0, r0, 0
  c->sw(s5, 36, s4);                                // sw s5, 36(s4)
  // nop                                            // sll r0, r0, 0
  c->sh(a0, 40, s4);                                // sh a0, 40(s4)
  c->daddiu(a0, t0, 1);                             // daddiu a0, t0, 1
  c->sq(a1, 0, s4);                                 // sq a1, 0(s4)
  // nop                                            // sll r0, r0, 0
  c->sq(a2, 16, s4);                                // sq a2, 16(s4)
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 4, gp);                                 // sw a0, 4(gp)
  //beq r0, r0, L91                                 // beq r0, r0, L91
  c->sw(v1, 0, gp);                                 // sw v1, 0(gp)
  goto block_14;                                    // branch always


  block_11:
  c->load_symbol2(t9, cache.format);                // lw t9, format(s7)
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  // daddiu a1, fp, L172                               // daddiu a1, fp, L172
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  printf("ERROR: Exceeded max number of collide-cache prims!\n");
  //beq r0, r0, L91                                 // beq r0, r0, L91
  // nop                                            // sll r0, r0, 0
  goto block_14;                                    // branch always


  block_12:
  c->load_symbol_addr(v1, cache.debug);             // daddiu v1, s7, debug
  c->load_symbol2(a0, cache.cheat_mode);            // lw a0, *cheat-mode*(s7)
  bc = c->sgpr64(a0) != c->sgpr64(v1);              // bne a0, v1, L91
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_14;}                          // branch non-likely

  c->load_symbol2(t9, cache.print_exceeded_max_cache_tris);// lw t9, print-exceeded-max-cache-tris(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0

  block_14:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
  c->lq(gp, 80, sp);                                // lq gp, 80(sp)
  c->lq(s5, 64, sp);                                // lq s5, 64(sp)
  c->lq(s4, 48, sp);                                // lq s4, 48(sp)
  c->lq(s3, 32, sp);                                // lq s3, 32(sp)
  c->lq(s2, 16, sp);                                // lq s2, 16(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 96);                            // daddiu sp, sp, 96
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.cheat_mode = intern_from_c("*cheat-mode*").c();
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  cache.debug = intern_from_c("debug").c();
  cache.format = intern_from_c("format").c();
  cache.print_exceeded_max_cache_tris = intern_from_c("print-exceeded-max-cache-tris").c();
  gLinkedFunctionTable.reg("(method 10 collide-shape-prim-mesh)", execute, 256);
}

} // namespace method_10_collide_shape_prim_mesh
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace method_10_collide_shape_prim_sphere {
struct Cache {
  void* format; // format
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  [[maybe_unused]] u32 call_addr = 0;
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  // nop                                            // sll r0, r0, 0
  c->daddiu(t0, a1, 108);                           // daddiu t0, a1, 108
  c->lwu(a3, 4, a1);                                // lwu a3, 4(a1)
  c->addiu(t1, r0, 100);                            // addiu t1, r0, 100
  c->lq(v1, 12, a0);                                // lq v1, 12(a0)
  c->dsll(t2, a3, 1);                               // dsll t2, a3, 1
  c->lq(a2, 28, a0);                                // lq a2, 28(a0)
  bc = c->sgpr64(a3) == c->sgpr64(t1);              // beq a3, t1, L84
  c->daddu(t1, t2, a3);                             // daddu t1, t2, a3
  if (bc) {goto block_2;}                           // branch non-likely

  c->dsll(t1, t1, 4);                               // dsll t1, t1, 4
  c->daddu(t0, t0, t1);                             // daddu t0, t0, t1
  c->daddiu(a3, a3, 1);                             // daddiu a3, a3, 1
  c->sq(r0, 32, t0);                                // sq r0, 32(t0)
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 36, t0);                                // sw a0, 36(t0)
  // nop                                            // sll r0, r0, 0
  c->sq(v1, 0, t0);                                 // sq v1, 0(t0)
  // nop                                            // sll r0, r0, 0
  c->sq(a2, 16, t0);                                // sq a2, 16(t0)
  // nop                                            // sll r0, r0, 0
  c->sw(a3, 4, a1);                                 // sw a3, 4(a1)
  // nop                                            // sll r0, r0, 0
  c->sw(a1, 32, t0);                                // sw a1, 32(t0)
  c->mov64(v0, s7);                                 // or v0, s7, r0
  //beq r0, r0, L85                                 // beq r0, r0, L85
  // nop                                            // sll r0, r0, 0
  goto block_3;                                     // branch always


  block_2:
  c->load_symbol2(t9, cache.format);                // lw t9, format(s7)
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  // daddiu a1, fp, L172                               // daddiu a1, fp, L172
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  printf("ERROR: Exceeded max number of collide-cache prims!\n");

  block_3:
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.format = intern_from_c("format").c();
  gLinkedFunctionTable.reg("(method 10 collide-shape-prim-sphere)", execute, 128);
}

} // namespace method_10_collide_shape_prim_sphere
} // namespace Mips2C
//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace method_10_collide_shape_prim_group {
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
  c->mov64(gp, a1);                                 // or gp, a1, r0
  c->mov64(s5, a0);                                 // or s5, a0, r0
  c->lbu(s4, 60, a0);                               // lbu s4, 60(a0)
  // nop                                            // sll r0, r0, 0
  c->lq(s3, 60, gp);                                // lq s3, 60(gp)
  // nop                                            // sll r0, r0, 0
  c->lq(s2, 76, gp);                                // lq s2, 76(gp)
  // nop                                            // sll r0, r0, 0
  c->lwu(s1, 92, gp);                               // lwu s1, 92(gp)

  block_1:
  bc = c->sgpr64(s4) == 0;                          // beq s4, r0, L82
  c->daddiu(s5, s5, 80);                            // daddiu s5, s5, 80
  if (bc) {goto block_5;}                           // branch non-likely


  block_2:
  c->daddiu(s4, s4, -1);                            // daddiu s4, s4, -1
  c->lqc2(vf1, 12, s5);                             // lqc2 vf1, 12(s5)
  // nop                                            // sll r0, r0, 0
  c->lwu(v1, 28, s5);                               // lwu v1, 28(s5)
  c->vsub_bc(DEST::xyzw, BC::w, vf2, vf1, vf1);     // vsubw.xyzw vf2, vf1, vf1
  c->and_(v1, s1, v1);                              // and v1, s1, v1
  c->vadd_bc(DEST::xyzw, BC::w, vf3, vf1, vf1);     // vaddw.xyzw vf3, vf1, vf1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L80
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->vftoi0(DEST::xyzw, vf4, vf2);                  // vftoi0.xyzw vf4, vf2
  c->vftoi0(DEST::xyzw, vf5, vf3);                  // vftoi0.xyzw vf5, vf3
  c->mov128_gpr_vf(a0, vf4);                        // qmfc2.i a0, vf4
  c->mov128_gpr_vf(v1, vf5);                        // qmfc2.i v1, vf5
  c->pcgtw(a0, a0, s2);                             // pcgtw a0, a0, s2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(v1, s3, v1);                             // pcgtw v1, s3, v1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->por(v1, a0, v1);                               // por v1, a0, v1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppach(v1, r0, v1);                             // ppach v1, r0, v1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsll(v1, v1, 16);                              // dsll v1, v1, 16
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L80
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 56, v1);                               // lwu t9, 56(v1)
  c->mov64(a1, gp);                                 // or a1, gp, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  bc = c->sgpr64(s4) != 0;                          // bne s4, r0, L81
  c->daddiu(s5, s5, 80);                            // daddiu s5, s5, 80
  if (bc) {goto block_2;}                           // branch non-likely


  block_5:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
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
  gLinkedFunctionTable.reg("(method 10 collide-shape-prim-group)", execute, 128);
}

} // namespace method_10_collide_shape_prim_group
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace method_11_collide_shape_prim_mesh {
struct Cache {
  void* cheat_mode; // *cheat-mode*
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* debug; // debug
  void* format; // format
  void* print_exceeded_max_cache_tris; // print-exceeded-max-cache-tris
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -96);                           // daddiu sp, sp, -96
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  c->sq(s2, 16, sp);                                // sq s2, 16(sp)
  c->sq(s3, 32, sp);                                // sq s3, 32(sp)
  c->sq(s4, 48, sp);                                // sq s4, 48(sp)
  c->sq(s5, 64, sp);                                // sq s5, 64(sp)
  c->sq(gp, 80, sp);                                // sq gp, 80(sp)
  c->mov64(s5, a0);                                 // or s5, a0, r0
  c->mov64(gp, a1);                                 // or gp, a1, r0
  c->mov64(s3, a2);                                 // or s3, a2, r0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lwu(s2, 60, s5);                               // lwu s2, 60(s5)
  c->daddiu(v1, gp, 108);                           // daddiu v1, gp, 108
  c->lwu(a0, 4, gp);                                // lwu a0, 4(gp)
  bc = c->sgpr64(s2) == c->sgpr64(s7);              // beq s2, s7, L62
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_14;}                          // branch non-likely

  c->addiu(a1, r0, 100);                            // addiu a1, r0, 100
  c->dsll(a2, a0, 1);                               // dsll a2, a0, 1
  bc = c->sgpr64(a0) == c->sgpr64(a1);              // beq a0, a1, L60
  c->daddu(a0, a2, a0);                             // daddu a0, a2, a0
  if (bc) {goto block_11;}                          // branch non-likely

  c->dsll(a0, a0, 4);                               // dsll a0, a0, 4
  c->daddu(s4, v1, a0);                             // daddu s4, v1, a0
  c->mov64(a0, s2);                                 // or a0, s2, r0
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 76, v1);                               // lwu t9, 76(v1)
  c->lwu(v1, 0, s5);                                // lwu v1, 0(s5)
  c->lwu(v1, 136, v1);                              // lwu v1, 136(v1)
  c->lwu(v1, 128, v1);                              // lwu v1, 128(v1)
  c->lb(a1, 8, s5);                                 // lb a1, 8(s5)
  c->dsll(a1, a1, 5);                               // dsll a1, a1, 5
  c->daddu(v1, v1, a1);                             // daddu v1, v1, a1
  c->lwu(v1, 28, v1);                               // lwu v1, 28(v1)
  c->daddu(a1, r0, v1);                             // daddu a1, r0, v1
  c->daddiu(a2, s3, 288);                           // daddiu a2, s3, 288
  get_fake_spad_addr2(a3, cache.fake_scratchpad_data, 0, c);// lui a3, 28672
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  // nop                                            // sll r0, r0, 0
  c->lwu(a0, 4, gp);                                // lwu a0, 4(gp)
  c->addiu(a1, r0, 460);                            // addiu a1, r0, 460
  c->lwu(v1, 0, gp);                                // lwu v1, 0(gp)
  c->dsll32(a0, a0, 0);                             // dsll32 a0, a0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf1, a0);                        // qmtc2.i vf1, a0
  // nop                                            // sll r0, r0, 0
  c->dsubu(a0, a1, v1);                             // dsubu a0, a1, v1
  c->dsll(t1, v1, 6);                               // dsll t1, v1, 6
  bc = ((s64)c->sgpr64(a0)) < 0;                    // bltz a0, L61
  c->daddiu(a2, s2, 28);                            // daddiu a2, s2, 28
  if (bc) {goto block_12;}                          // branch non-likely

  c->mov64(a0, v1);                                 // or a0, v1, r0
  c->lwu(a3, 4, s2);                                // lwu a3, 4(s2)
  c->daddiu(t2, gp, 4908);                          // daddiu t2, gp, 4908
  c->lq(t0, 368, s3);                               // lq t0, 368(s3)
  c->daddu(t1, t2, t1);                             // daddu t1, t2, t1
  c->lq(t2, 384, s3);                               // lq t2, 384(s3)
  get_fake_spad_addr2(t3, cache.fake_scratchpad_data, 0, c);// lui t3, 28672
  c->lwu(t4, 8, gp);                                // lwu t4, 8(gp)
  c->vsub(DEST::zw, vf1, vf0, vf0);                 // vsub.zw vf1, vf0, vf0
  // nop                                            // sll r0, r0, 0

  block_4:
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L59
  c->lbu(t5, 0, a2);                                // lbu t5, 0(a2)
  if (bc) {goto block_9;}                           // branch non-likely

  c->daddiu(a3, a3, -1);                            // daddiu a3, a3, -1
  c->lbu(t6, 1, a2);                                // lbu t6, 1(a2)
  c->dsll(t8, t5, 5);                               // dsll t8, t5, 5
  c->lbu(t5, 2, a2);                                // lbu t5, 2(a2)
  c->dsll(t7, t6, 5);                               // dsll t7, t6, 5
  c->daddu(t6, t8, t3);                             // daddu t6, t8, t3
  c->dsll(t5, t5, 5);                               // dsll t5, t5, 5
  // nop                                            // sll r0, r0, 0
  c->daddu(t7, t7, t3);                             // daddu t7, t7, t3
  c->lq(t9, 16, t6);                                // lq t9, 16(t6)
  c->daddu(t5, t5, t3);                             // daddu t5, t5, t3
  c->lq(s3, 16, t7);                                // lq s3, 16(t7)
  // nop                                            // sll r0, r0, 0
  c->lq(t8, 16, t5);                                // lq t8, 16(t5)
  c->pminw(ra, t9, s3);                             // pminw ra, t9, s3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaxw(t9, t9, s3);                             // pmaxw t9, t9, s3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pminw(ra, ra, t8);                             // pminw ra, ra, t8
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaxw(t8, t9, t8);                             // pmaxw t8, t9, t8
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(t9, ra, t2);                             // pcgtw t9, ra, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(ra, t0, t8);                             // pcgtw ra, t0, t8
  c->lwu(t8, 4, a2);                                // lwu t8, 4(a2)
  c->por(t9, t9, ra);                               // por t9, t9, ra
  c->lq(t6, 0, t6);                                 // lq t6, 0(t6)
  c->ppach(t9, r0, t9);                             // ppach t9, r0, t9
  c->lq(t7, 0, t7);                                 // lq t7, 0(t7)
  c->dsll(t9, t9, 16);                              // dsll t9, t9, 16
  c->lq(t5, 0, t5);                                 // lq t5, 0(t5)
  bc = c->sgpr64(t9) != 0;                          // bne t9, r0, L58
  c->daddiu(a2, a2, 8);                             // daddiu a2, a2, 8
  if (bc) {goto block_4;}                           // branch non-likely

  bc = c->sgpr64(v1) == c->sgpr64(a1);              // beq v1, a1, L61
  c->sqc2(vf1, 48, t1);                             // sqc2 vf1, 48(t1)
  if (bc) {goto block_12;}                          // branch non-likely

  c->and_(t9, t8, t4);                              // and t9, t8, t4
  c->sw(t8, 48, t1);                                // sw t8, 48(t1)
  // nop                                            // sll r0, r0, 0
  c->sw(s5, 52, t1);                                // sw s5, 52(t1)
  bc = c->sgpr64(t9) != 0;                          // bne t9, r0, L58
  c->sq(t6, 0, t1);                                 // sq t6, 0(t1)
  if (bc) {goto block_4;}                           // branch non-likely

  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  c->sq(t7, 16, t1);                                // sq t7, 16(t1)
  // nop                                            // sll r0, r0, 0
  c->sq(t5, 32, t1);                                // sq t5, 32(t1)
  //beq r0, r0, L58                                 // beq r0, r0, L58
  c->daddiu(t1, t1, 64);                            // daddiu t1, t1, 64
  goto block_4;                                     // branch always


  block_9:
  c->dsubu(a3, v1, a0);                             // dsubu a3, v1, a0
  c->lwu(t0, 4, gp);                                // lwu t0, 4(gp)
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L62
  c->lq(a1, 12, s5);                                // lq a1, 12(s5)
  if (bc) {goto block_14;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lq(a2, 28, s5);                                // lq a2, 28(s5)
  // nop                                            // sll r0, r0, 0
  c->sq(r0, 32, s4);                                // sq r0, 32(s4)
  // nop                                            // sll r0, r0, 0
  c->sh(a3, 42, s4);                                // sh a3, 42(s4)
  // nop                                            // sll r0, r0, 0
  c->sw(gp, 32, s4);                                // sw gp, 32(s4)
  // nop                                            // sll r0, r0, 0
  c->sw(s5, 36, s4);                                // sw s5, 36(s4)
  // nop                                            // sll r0, r0, 0
  c->sh(a0, 40, s4);                                // sh a0, 40(s4)
  c->daddiu(a0, t0, 1);                             // daddiu a0, t0, 1
  c->sq(a1, 0, s4);                                 // sq a1, 0(s4)
  // nop                                            // sll r0, r0, 0
  c->sq(a2, 16, s4);                                // sq a2, 16(s4)
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 4, gp);                                 // sw a0, 4(gp)
  //beq r0, r0, L62                                 // beq r0, r0, L62
  c->sw(v1, 0, gp);                                 // sw v1, 0(gp)
  goto block_14;                                    // branch always


  block_11:
  c->load_symbol2(t9, cache.format);                // lw t9, format(s7)
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  // daddiu a1, fp, L172                               // daddiu a1, fp, L172
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  printf("ERROR: Exceeded max number of collide-cache prims!\n");
  //beq r0, r0, L62                                 // beq r0, r0, L62
  // nop                                            // sll r0, r0, 0
  goto block_14;                                    // branch always


  block_12:
  c->load_symbol_addr(v1, cache.debug);             // daddiu v1, s7, debug
  c->load_symbol2(a0, cache.cheat_mode);            // lw a0, *cheat-mode*(s7)
  bc = c->sgpr64(a0) != c->sgpr64(v1);              // bne a0, v1, L62
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_14;}                          // branch non-likely

  c->load_symbol2(t9, cache.print_exceeded_max_cache_tris);// lw t9, print-exceeded-max-cache-tris(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0

  block_14:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
  c->lq(gp, 80, sp);                                // lq gp, 80(sp)
  c->lq(s5, 64, sp);                                // lq s5, 64(sp)
  c->lq(s4, 48, sp);                                // lq s4, 48(sp)
  c->lq(s3, 32, sp);                                // lq s3, 32(sp)
  c->lq(s2, 16, sp);                                // lq s2, 16(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 96);                            // daddiu sp, sp, 96
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.cheat_mode = intern_from_c("*cheat-mode*").c();
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  cache.debug = intern_from_c("debug").c();
  cache.format = intern_from_c("format").c();
  cache.print_exceeded_max_cache_tris = intern_from_c("print-exceeded-max-cache-tris").c();
  gLinkedFunctionTable.reg("(method 11 collide-shape-prim-mesh)", execute, 128);
}

} // namespace method_11_collide_shape_prim_mesh
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace method_11_collide_shape_prim_sphere {
struct Cache {
  void* format; // format
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  [[maybe_unused]] u32 call_addr = 0;
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  // nop                                            // sll r0, r0, 0
  c->daddiu(t0, a1, 108);                           // daddiu t0, a1, 108
  c->lwu(a3, 4, a1);                                // lwu a3, 4(a1)
  c->addiu(t1, r0, 100);                            // addiu t1, r0, 100
  c->lq(v1, 12, a0);                                // lq v1, 12(a0)
  c->dsll(t2, a3, 1);                               // dsll t2, a3, 1
  c->lq(a2, 28, a0);                                // lq a2, 28(a0)
  bc = c->sgpr64(a3) == c->sgpr64(t1);              // beq a3, t1, L55
  c->daddu(t1, t2, a3);                             // daddu t1, t2, a3
  if (bc) {goto block_2;}                           // branch non-likely

  c->dsll(t1, t1, 4);                               // dsll t1, t1, 4
  c->daddu(t0, t0, t1);                             // daddu t0, t0, t1
  c->daddiu(a3, a3, 1);                             // daddiu a3, a3, 1
  c->sq(r0, 32, t0);                                // sq r0, 32(t0)
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 36, t0);                                // sw a0, 36(t0)
  // nop                                            // sll r0, r0, 0
  c->sq(v1, 0, t0);                                 // sq v1, 0(t0)
  // nop                                            // sll r0, r0, 0
  c->sq(a2, 16, t0);                                // sq a2, 16(t0)
  // nop                                            // sll r0, r0, 0
  c->sw(a3, 4, a1);                                 // sw a3, 4(a1)
  // nop                                            // sll r0, r0, 0
  c->sw(a1, 32, t0);                                // sw a1, 32(t0)
  c->mov64(v0, s7);                                 // or v0, s7, r0
  //beq r0, r0, L56                                 // beq r0, r0, L56
  // nop                                            // sll r0, r0, 0
  goto block_3;                                     // branch always


  block_2:
  c->load_symbol2(t9, cache.format);                // lw t9, format(s7)
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  // daddiu a1, fp, L172                               // daddiu a1, fp, L172
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  printf("ERROR: Exceeded max number of collide-cache prims!\n");

  block_3:
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.format = intern_from_c("format").c();
  gLinkedFunctionTable.reg("(method 11 collide-shape-prim-sphere)", execute, 128);
}

} // namespace method_11_collide_shape_prim_sphere
} // namespace Mips2C


//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace method_11_collide_shape_prim_group {
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
  c->mov64(gp, a1);                                 // or gp, a1, r0
  c->mov64(s5, a2);                                 // or s5, a2, r0
  c->mov64(s4, a0);                                 // or s4, a0, r0
  c->lbu(s3, 60, a0);                               // lbu s3, 60(a0)
  // nop                                            // sll r0, r0, 0
  c->lq(s2, 368, s5);                               // lq s2, 368(s5)
  // nop                                            // sll r0, r0, 0
  c->lq(s1, 384, s5);                               // lq s1, 384(s5)
  // nop                                            // sll r0, r0, 0
  c->lwu(s0, 92, gp);                               // lwu s0, 92(gp)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf28, 288, s5);                           // lqc2 vf28, 288(s5)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf29, 304, s5);                           // lqc2 vf29, 304(s5)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf30, 320, s5);                           // lqc2 vf30, 320(s5)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf31, 336, s5);                           // lqc2 vf31, 336(s5)

  block_1:
  bc = c->sgpr64(s3) == 0;                          // beq s3, r0, L53
  c->daddiu(s4, s4, 80);                            // daddiu s4, s4, 80
  if (bc) {goto block_5;}                           // branch non-likely


  block_2:
  c->daddiu(s3, s3, -1);                            // daddiu s3, s3, -1
  c->lqc2(vf1, 12, s4);                             // lqc2 vf1, 12(s4)
  // nop                                            // sll r0, r0, 0
  c->lwu(v1, 28, s4);                               // lwu v1, 28(s4)
  c->vmula_bc(DEST::xyzw, BC::w, vf31, vf0);        // vmulaw.xyzw acc, vf31, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf28, vf1);       // vmaddax.xyzw acc, vf28, vf1
  c->and_(v1, s0, v1);                              // and v1, s0, v1
  c->vmadda_bc(DEST::xyzw, BC::y, vf29, vf1);       // vmadday.xyzw acc, vf29, vf1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L51
  c->vmadd_bc(DEST::xyzw, BC::z, vf10, vf30, vf1);  // vmaddz.xyzw vf10, vf30, vf1
  if (bc) {goto block_1;}                           // branch non-likely

  c->vsub_bc(DEST::xyz, BC::w, vf2, vf10, vf1);     // vsubw.xyz vf2, vf10, vf1
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::xyz, BC::w, vf3, vf10, vf1);     // vaddw.xyz vf3, vf10, vf1
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf4, vf2);                  // vftoi0.xyzw vf4, vf2
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf5, vf3);                  // vftoi0.xyzw vf5, vf3
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a0, vf4);                        // qmfc2.i a0, vf4
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(v1, vf5);                        // qmfc2.i v1, vf5
  // nop                                            // sll r0, r0, 0
  c->pcgtw(a0, a0, s1);                             // pcgtw a0, a0, s1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(v1, s2, v1);                             // pcgtw v1, s2, v1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->por(v1, a0, v1);                               // por v1, a0, v1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppach(v1, r0, v1);                             // ppach v1, r0, v1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsll(v1, v1, 16);                              // dsll v1, v1, 16
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L51
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(a0, s4);                                 // or a0, s4, r0
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 60, v1);                               // lwu t9, 60(v1)
  c->mov64(a1, gp);                                 // or a1, gp, r0
  c->mov64(a2, s5);                                 // or a2, s5, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  bc = c->sgpr64(s3) != 0;                          // bne s3, r0, L52
  c->daddiu(s4, s4, 80);                            // daddiu s4, s4, 80
  if (bc) {goto block_2;}                           // branch non-likely


  block_5:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
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
  gLinkedFunctionTable.reg("(method 11 collide-shape-prim-group)", execute, 256);
}

} // namespace method_11_collide_shape_prim_group
} // namespace Mips2C
//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace method_9_collide_cache_prim {
struct Cache {
  void* moving_sphere_triangle_intersect; // moving-sphere-triangle-intersect
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -672);                          // daddiu sp, sp, -672
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s1, 576, sp);                               // sq s1, 576(sp)
  c->sq(s2, 592, sp);                               // sq s2, 592(sp)
  c->sq(s3, 608, sp);                               // sq s3, 608(sp)
  c->sq(s4, 624, sp);                               // sq s4, 624(sp)
  c->sq(s5, 640, sp);                               // sq s5, 640(sp)
  c->sq(gp, 656, sp);                               // sq gp, 656(sp)
  c->mov64(gp, a1);                                 // or gp, a1, r0
  c->mov64(s5, a2);                                 // or s5, a2, r0
  c->mov64(s4, a3);                                 // or s4, a3, r0
  c->daddiu(s3, sp, 16);                            // daddiu s3, sp, 16
  // nop                                            // sll r0, r0, 0
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->sw(t1, 8, s3);                                 // sw t1, 8(s3)
  c->mtc1(f1, t0);                                  // mtc1 f1, t0
  c->lhu(v1, 40, a0);                               // lhu v1, 40(a0)
  // nop                                            // sll r0, r0, 0
  c->lwu(a1, 32, a0);                               // lwu a1, 32(a0)
  cop1_bc = c->fprs[f0] <= c->fprs[f1];             // c.le.s f0, f1
  c->dsll(v1, v1, 6);                               // dsll v1, v1, 6
  bc = cop1_bc;                                     // bc1t L37
  c->daddiu(a1, a1, 4908);                          // daddiu a1, a1, 4908
  if (bc) {goto block_2;}                           // branch non-likely

  c->lui(a2, 16384);                                // lui a2, 16384
  c->mtc1(f1, a2);                                  // mtc1 f1, a2

  block_2:
  c->daddu(s2, a1, v1);                             // daddu s2, a1, v1
  c->swc1(f1, 0, s3);                               // swc1 f1, 0(s3)
  // nop                                            // sll r0, r0, 0
  c->lhu(s1, 42, a0);                               // lhu s1, 42(a0)
  // nop                                            // sll r0, r0, 0
  c->swc1(f1, 4, s3);                               // swc1 f1, 4(s3)

  block_3:
  bc = c->sgpr64(s1) == 0;                          // beq s1, r0, L40
  c->daddiu(s1, s1, -1);                            // daddiu s1, s1, -1
  if (bc) {goto block_14;}                          // branch non-likely

  c->load_symbol2(t9, cache.moving_sphere_triangle_intersect);// lw t9, moving-sphere-triangle-intersect(s7)
  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->mov64(a1, s4);                                 // or a1, s4, r0
  c->lwc1(f0, 12, s5);                              // lwc1 f0, 12(s5)
  c->mfc1(a2, f0);                                  // mfc1 a2, f0
  c->daddu(a3, r0, s2);                             // daddu a3, r0, s2
  c->daddiu(t0, s3, 64);                            // daddiu t0, s3, 64
  c->daddiu(t1, s3, 80);                            // daddiu t1, s3, 80
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  // nop                                            // sll r0, r0, 0
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->lqc2(vf1, 0, s4);                              // lqc2 vf1, 0(s4)
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  c->lwc1(f1, 0, s3);                               // lwc1 f1, 0(s3)
  cop1_bc = c->fprs[f2] < c->fprs[f0];              // c.lt.s f2, f0
  c->lqc2(vf2, 80, s3);                             // lqc2 vf2, 80(s3)
  if (cop1_bc) {                                    // bc1tl L38
    c->daddiu(s2, s2, 64);                          // daddiu s2, s2, 64
    goto block_3;
  }

  cop1_bc = c->fprs[f1] <= c->fprs[f2];             // c.le.s f1, f2
  c->lqc2(vf3, 64, s3);                             // lqc2 vf3, 64(s3)
  if (cop1_bc) {                                    // bc1tl L38
    c->daddiu(s2, s2, 64);                          // daddiu s2, s2, 64
    goto block_3;
  }

  c->vmul(DEST::xyzw, vf5, vf1, vf2);               // vmul.xyzw vf5, vf1, vf2
  c->lqc2(vf4, 0, s5);                              // lqc2 vf4, 0(s5)
  // nop                                            // sll r0, r0, 0
  c->lwu(v1, 8, s3);                                // lwu v1, 8(s3)
  c->vsub(DEST::xyzw, vf7, vf4, vf3);               // vsub.xyzw vf7, vf4, vf3
  c->andi(v1, v1, 1);                               // andi v1, v1, 1
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L39
  c->vadd_bc(DEST::x, BC::y, vf5, vf5, vf5);        // vaddy.x vf5, vf5, vf5
  if (bc) {goto block_13;}                          // branch non-likely

  c->vmul(DEST::xyzw, vf6, vf7, vf2);               // vmul.xyzw vf6, vf7, vf2
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::z, vf5, vf5, vf5);        // vaddz.x vf5, vf5, vf5
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::y, vf6, vf6, vf6);        // vaddy.x vf6, vf6, vf6
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(v1, vf5);                        // qmfc2.i v1, vf5
  // nop                                            // sll r0, r0, 0
  c->mtc1(f3, v1);                                  // mtc1 f3, v1
  // nop                                            // sll r0, r0, 0
  cop1_bc = c->fprs[f0] <= c->fprs[f3];             // c.le.s f0, f3
  // nop                                            // sll r0, r0, 0
  if (cop1_bc) {                                    // bc1tl L38
    c->daddiu(s2, s2, 64);                          // daddiu s2, s2, 64
    goto block_3;
  }

  c->vadd_bc(DEST::x, BC::z, vf6, vf6, vf6);        // vaddz.x vf6, vf6, vf6
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(v1, vf6);                        // qmfc2.i v1, vf6
  // nop                                            // sll r0, r0, 0
  c->mtc1(f4, v1);                                  // mtc1 f4, v1
  // nop                                            // sll r0, r0, 0
  cop1_bc = c->fprs[f4] < c->fprs[f0];              // c.lt.s f4, f0
  // nop                                            // sll r0, r0, 0
  if (cop1_bc) {                                    // bc1tl L38
    c->daddiu(s2, s2, 64);                          // daddiu s2, s2, 64
    goto block_3;
  }

  block_13:
  c->lqc2(vf8, 0, s2);                              // lqc2 vf8, 0(s2)
  c->lqc2(vf9, 16, s2);                             // lqc2 vf9, 16(s2)
  c->lqc2(vf10, 32, s2);                            // lqc2 vf10, 32(s2)
  c->lwu(v1, 48, s2);                               // lwu v1, 48(s2)
  c->lw(a0, 52, s2);                                // lw a0, 52(s2)
  c->swc1(f2, 0, s3);                               // swc1 f2, 0(s3)
  c->sqc2(vf3, 48, gp);                             // sqc2 vf3, 48(gp)
  c->sqc2(vf2, 64, gp);                             // sqc2 vf2, 64(gp)
  c->sqc2(vf8, 0, gp);                              // sqc2 vf8, 0(gp)
  c->sqc2(vf9, 16, gp);                             // sqc2 vf9, 16(gp)
  c->sqc2(vf10, 32, gp);                            // sqc2 vf10, 32(gp)
  c->sw(v1, 80, gp);                                // sw v1, 80(gp)
  c->sw(a0, 84, gp);                                // sw a0, 84(gp)
  //beq r0, r0, L38                                 // beq r0, r0, L38
  c->daddiu(s2, s2, 64);                            // daddiu s2, s2, 64
  goto block_3;                                     // branch always


  block_14:
  c->lwc1(f1, 0, s3);                               // lwc1 f1, 0(s3)
  c->lwc1(f5, 4, s3);                               // lwc1 f5, 4(s3)
  cop1_bc = c->fprs[f1] == c->fprs[f5];             // c.eq.s f1, f5
  if (!cop1_bc) {                                   // bc1fl L41
    c->mfc1(v0, f1);                                // mfc1 v0, f1
    goto block_17;
  }

  c->lui(v0, -13122);                               // lui v0, -13122
  c->ori(v0, v0, 48160);                            // ori v0, v0, 48160

  block_17:
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 656, sp);                               // lq gp, 656(sp)
  c->lq(s5, 640, sp);                               // lq s5, 640(sp)
  c->lq(s4, 624, sp);                               // lq s4, 624(sp)
  c->lq(s3, 608, sp);                               // lq s3, 608(sp)
  c->lq(s2, 592, sp);                               // lq s2, 592(sp)
  c->lq(s1, 576, sp);                               // lq s1, 576(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 672);                           // daddiu sp, sp, 672
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.moving_sphere_triangle_intersect = intern_from_c("moving-sphere-triangle-intersect").c();
  gLinkedFunctionTable.reg("(method 9 collide-cache-prim)", execute, 1024);
}

} // namespace method_9_collide_cache_prim
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace method_10_collide_cache_prim {
struct Cache {
  void* moving_sphere_sphere_intersect; // moving-sphere-sphere-intersect
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -128);                          // daddiu sp, sp, -128
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  c->sq(s1, 32, sp);                                // sq s1, 32(sp)
  c->sq(s2, 48, sp);                                // sq s2, 48(sp)
  c->sq(s3, 64, sp);                                // sq s3, 64(sp)
  c->sq(s4, 80, sp);                                // sq s4, 80(sp)
  c->sq(s5, 96, sp);                                // sq s5, 96(sp)
  c->sq(gp, 112, sp);                               // sq gp, 112(sp)
  c->mov64(s3, a0);                                 // or s3, a0, r0
  c->mov64(gp, a1);                                 // or gp, a1, r0
  c->mov64(s5, a3);                                 // or s5, a3, r0
  c->mov64(s2, t0);                                 // or s2, t0, r0
  c->mov64(s4, t1);                                 // or s4, t1, r0
  c->daddiu(s1, sp, 16);                            // daddiu s1, sp, 16
  c->load_symbol2(t9, cache.moving_sphere_sphere_intersect);// lw t9, moving-sphere-sphere-intersect(s7)
  c->mov64(a0, a2);                                 // or a0, a2, r0
  c->mov64(a1, s5);                                 // or a1, s5, r0
  c->daddu(a2, r0, s3);                             // daddu a2, r0, s3
  c->mov64(a3, s1);                                 // or a3, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mtc1(f3, v0);                                  // mtc1 f3, v0
  c->lui(v1, -13122);                               // lui v1, -13122
  c->ori(v1, v1, 48160);                            // ori v1, v1, 48160
  c->mtc1(f4, v1);                                  // mtc1 f4, v1
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->lqc2(vf4, 0, s1);                              // lqc2 vf4, 0(s1)
  c->movs(f1, f3);                                  // mov.s f1, f3
  c->lqc2(vf5, 0, s3);                              // lqc2 vf5, 0(s3)
  cop1_bc = c->fprs[f1] < c->fprs[f0];              // c.lt.s f1, f0
  c->vmove(DEST::xyzw, vf1, vf0);                   // vmove.xyzw vf1, vf0
  bc = cop1_bc;                                     // bc1t L35
  c->mtc1(f2, s2);                                  // mtc1 f2, s2
  if (bc) {goto block_11;}                          // branch non-likely

  cop1_bc = c->fprs[f2] < c->fprs[f0];              // c.lt.s f2, f0
  c->vsub(DEST::xyz, vf1, vf4, vf5);                // vsub.xyz vf1, vf4, vf5
  bc = cop1_bc;                                     // bc1t L32
  c->lwu(v1, 36, s3);                               // lwu v1, 36(s3)
  if (bc) {goto block_4;}                           // branch non-likely

  cop1_bc = c->fprs[f2] <= c->fprs[f1];             // c.le.s f2, f1
  // nop                                            // sll r0, r0, 0
  if (cop1_bc) {                                    // bc1tl L35
    c->movs(f3, f4);                                // mov.s f3, f4
    goto block_11;
  }

  block_4:
  c->andi(a0, s4, 1);                               // andi a0, s4, 1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L33
  c->lqc2(vf15, 0, s5);                             // lqc2 vf15, 0(s5)
  if (bc) {goto block_7;}                           // branch non-likely

  c->vmul(DEST::xyzw, vf16, vf15, vf1);             // vmul.xyzw vf16, vf15, vf1
  c->vadd_bc(DEST::y, BC::x, vf16, vf16, vf16);     // vaddx.y vf16, vf16, vf16
  c->vadd_bc(DEST::y, BC::z, vf16, vf16, vf16);     // vaddz.y vf16, vf16, vf16
  c->mov128_gpr_vf(a0, vf16);                       // qmfc2.i a0, vf16
  if (((s64)c->sgpr64(a0)) >= 0) {                  // bgezl a0, L35
    c->movs(f3, f4);                                // mov.s f3, f4
    goto block_11;
  }

  block_7:
  // daddiu a0, fp, L168                               // daddiu a0, fp, L168
//  L168:
//      .word 0x0
//      .word 0x45800000
//      .word 0x0
//      .word 0x3f800000

//      .word 0x0
//      .word 0xc5800000
//      .word 0x45800000
//      .word 0x3f800000

//      .word 0x0
//      .word 0xc5800000
//      .word 0xc5800000
//      .word 0x3f800000
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf12, vf1, vf1);              // vmul.xyzw vf12, vf1, vf1
  c->sqc2(vf4, 48, gp);                             // sqc2 vf4, 48(gp)
  c->vmula_bc(DEST::w, BC::x, vf0, vf12);           // vmulax.w acc, vf0, vf12
  //c->lqc2(vf9, 0, a0);                              // lqc2 vf9, 0(a0)
  c->vfs[9].vf.set_u32s(0, 0x45800000, 0, 0x3f800000);
  c->vmadda_bc(DEST::w, BC::y, vf0, vf12);          // vmadday.w acc, vf0, vf12
  //c->lqc2(vf10, 16, a0);                            // lqc2 vf10, 16(a0)
  c->vfs[10].vf.set_u32s(0, 0xc5800000, 0x45800000, 0x3f800000);
  c->vmadd_bc(DEST::w, BC::z, vf12, vf0, vf12);     // vmaddz.w vf12, vf0, vf12
  //c->lqc2(vf11, 32, a0);                            // lqc2 vf11, 32(a0)
  c->vfs[11].vf.set_u32s(0, 0xc5800000, 0xc5800000, 0x3f800000);
  c->vrsqrt(vf0, BC::w, vf12, BC::w);               // vrsqrt Q, vf0.w, vf12.w
  // nop                                            // sll r0, r0, 0
  c->vwaitq();                                      // vwaitq
  c->lwu(a0, 60, v1);                               // lwu a0, 60(v1)
  c->vmulq(DEST::xyz, vf1, vf1);                    // vmulq.xyz vf1, vf1, Q
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf14, vf1, vf1);              // vmul.xyzw vf14, vf1, vf1
  c->sqc2(vf1, 64, gp);                             // sqc2 vf1, 64(gp)
  c->vabs(DEST::xyzw, vf13, vf1);                   // vabs.xyzw vf13, vf1
  c->sw(a0, 80, gp);                                // sw a0, 80(gp)
  c->vmove(DEST::xyzw, vf2, vf0);                   // vmove.xyzw vf2, vf0
  c->sw(v1, 84, gp);                                // sw v1, 84(gp)
  c->vadd_bc(DEST::x, BC::y, vf14, vf14, vf14);     // vaddy.x vf14, vf14, vf14
  c->mov128_gpr_vf(v1, vf13);                       // qmfc2.i v1, vf13
  if (((s64)c->sgpr64(v1)) == ((s64)0)) {           // beql v1, r0, L34
    c->vadd_bc(DEST::x, BC::z, vf2, vf0, vf1);      // vaddz.x vf2, vf0, vf1
    goto block_10;
  }

  c->vsub_bc(DEST::x, BC::y, vf2, vf0, vf1);        // vsuby.x vf2, vf0, vf1
  // nop                                            // sll r0, r0, 0
  c->vrsqrt(vf0, BC::w, vf14, BC::x);               // vrsqrt Q, vf0.w, vf14.x
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::y, BC::x, vf2, vf0, vf1);        // vaddx.y vf2, vf0, vf1
  // nop                                            // sll r0, r0, 0
  c->vwaitq();                                      // vwaitq
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xy, vf2, vf2);                     // vmulq.xy vf2, vf2, Q
  // nop                                            // sll r0, r0, 0

  block_10:
  c->vopmula(vf1, vf2);                             // vopmula.xyz acc, vf1, vf2
  // nop                                            // sll r0, r0, 0
  c->vopmsub(vf3, vf2, vf1);                        // vopmsub.xyz vf3, vf2, vf1
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf9);        // vmaddax.xyzw acc, vf1, vf9
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf9);        // vmadday.xyzw acc, vf2, vf9
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyz, BC::z, vf9, vf3, vf9);     // vmaddz.xyz vf9, vf3, vf9
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf10);       // vmaddax.xyzw acc, vf1, vf10
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf10);       // vmadday.xyzw acc, vf2, vf10
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyz, BC::z, vf10, vf3, vf10);   // vmaddz.xyz vf10, vf3, vf10
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf11);       // vmaddax.xyzw acc, vf1, vf11
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf11);       // vmadday.xyzw acc, vf2, vf11
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyz, BC::z, vf11, vf3, vf11);   // vmaddz.xyz vf11, vf3, vf11
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf9, 0, gp);                              // sqc2 vf9, 0(gp)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf10, 16, gp);                            // sqc2 vf10, 16(gp)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf11, 32, gp);                            // sqc2 vf11, 32(gp)
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0

  block_11:
  c->mfc1(v0, f3);                                  // mfc1 v0, f3
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
  c->lq(gp, 112, sp);                               // lq gp, 112(sp)
  c->lq(s5, 96, sp);                                // lq s5, 96(sp)
  c->lq(s4, 80, sp);                                // lq s4, 80(sp)
  c->lq(s3, 64, sp);                                // lq s3, 64(sp)
  c->lq(s2, 48, sp);                                // lq s2, 48(sp)
  c->lq(s1, 32, sp);                                // lq s1, 32(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 128);                           // daddiu sp, sp, 128
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.moving_sphere_sphere_intersect = intern_from_c("moving-sphere-sphere-intersect").c();
  gLinkedFunctionTable.reg("(method 10 collide-cache-prim)", execute, 256);
}

} // namespace method_10_collide_cache_prim
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace method_17_collide_cache {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* collide_puss_work; // collide-puss-work
  void* format; // format
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -96);                           // daddiu sp, sp, -96
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  c->sq(s2, 16, sp);                                // sq s2, 16(sp)
  c->sq(s3, 32, sp);                                // sq s3, 32(sp)
  c->sq(s4, 48, sp);                                // sq s4, 48(sp)
  c->sq(s5, 64, sp);                                // sq s5, 64(sp)
  c->sq(gp, 80, sp);                                // sq gp, 80(sp)
  c->mov64(gp, a1);                                 // or gp, a1, r0
  get_fake_spad_addr2(s5, cache.fake_scratchpad_data, 0, c);// lui s5, 28672
  c->addiu(a3, r0, 64);                             // addiu a3, r0, 64
  c->lwu(a2, 116, gp);                              // lwu a2, 116(gp)
  c->daddiu(v1, s5, 96);                            // daddiu v1, s5, 96
  c->lwu(a1, 112, gp);                              // lwu a1, 112(gp)
  c->dsubu(a3, a2, a3);                             // dsubu a3, a2, a3
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(a3)) > 0;                    // bgtz a3, L26
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_20;}                          // branch non-likely

  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L19
  c->lqc2(vf1, 0, a1);                              // lqc2 vf1, 0(a1)
  if (bc) {goto block_5;}                           // branch non-likely

  c->daddiu(a2, a2, -1);                            // daddiu a2, a2, -1
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->vsub_bc(DEST::xyz, BC::w, vf2, vf1, vf1);      // vsubw.xyz vf2, vf1, vf1
  c->sqc2(vf1, 0, v1);                              // sqc2 vf1, 0(v1)
  c->vadd_bc(DEST::xyz, BC::w, vf3, vf1, vf1);      // vaddw.xyz vf3, vf1, vf1
  c->daddiu(v1, v1, 48);                            // daddiu v1, v1, 48
  c->vftoi0(DEST::xyzw, vf4, vf2);                  // vftoi0.xyzw vf4, vf2
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf5, vf3);                  // vftoi0.xyzw vf5, vf3
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf4, -32, v1);                            // sqc2 vf4, -32(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf5, -16, v1);                            // sqc2 vf5, -16(v1)

  block_3:
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L19
  c->lqc2(vf1, 0, a1);                              // lqc2 vf1, 0(a1)
  if (bc) {goto block_5;}                           // branch non-likely

  c->daddiu(a2, a2, -1);                            // daddiu a2, a2, -1
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->vsub_bc(DEST::xyz, BC::w, vf4, vf1, vf1);      // vsubw.xyz vf4, vf1, vf1
  c->sqc2(vf1, 0, v1);                              // sqc2 vf1, 0(v1)
  c->vadd_bc(DEST::xyz, BC::w, vf5, vf1, vf1);      // vaddw.xyz vf5, vf1, vf1
  // nop                                            // sll r0, r0, 0
  c->vmini(DEST::xyz, vf2, vf2, vf4);               // vmini.xyz vf2, vf2, vf4
  // nop                                            // sll r0, r0, 0
  c->vmax(DEST::xyz, vf3, vf3, vf5);                // vmax.xyz vf3, vf3, vf5
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf4, vf4);                  // vftoi0.xyzw vf4, vf4
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf5, vf5);                  // vftoi0.xyzw vf5, vf5
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf4, 16, v1);                             // sqc2 vf4, 16(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf5, 32, v1);                             // sqc2 vf5, 32(v1)
  //beq r0, r0, L18                                 // beq r0, r0, L18
  c->daddiu(v1, v1, 48);                            // daddiu v1, v1, 48
  goto block_3;                                     // branch always


  block_5:
  c->vftoi0(DEST::xyzw, vf2, vf2);                  // vftoi0.xyzw vf2, vf2
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf3, vf3);                  // vftoi0.xyzw vf3, vf3
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf2, 64, s5);                             // sqc2 vf2, 64(s5)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf3, 80, s5);                             // sqc2 vf3, 80(s5)
  c->daddiu(s4, a0, 108);                           // daddiu s4, a0, 108
  c->lwu(s3, 100, gp);                              // lwu s3, 100(gp)
  c->lw(s2, 4, a0);                                 // lw s2, 4(a0)
  //beq r0, r0, L25                                 // beq r0, r0, L25
  // nop                                            // sll r0, r0, 0
  goto block_18;                                    // branch always


  block_6:
  c->daddiu(s2, s2, -1);                            // daddiu s2, s2, -1
  c->lwu(v1, 16, s4);                               // lwu v1, 16(s4)
  c->and_(v1, s3, v1);                              // and v1, s3, v1
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L24
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_17;}                          // branch non-likely

  c->lwu(v1, 120, gp);                              // lwu v1, 120(gp)
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(v1))) {// beql s7, v1, L21
    c->daddiu(v1, s7, 4);                           // daddiu v1, s7, 4
    goto block_10;
  }

  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  c->lwu(a0, 24, s4);                               // lwu a0, 24(s4)
  c->andi(a0, a0, 1);                               // andi a0, a0, 1
  c->movz(v1, s7, a0);                              // movz v1, s7, a0

  block_10:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L24
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_17;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->lb(v1, 28, s4);                                // lb v1, 28(s4)
  c->slt(v1, v1, r0);                               // slt v1, v1, r0
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L22
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_13;}                          // branch non-likely

  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->load_symbol2(v1, cache.collide_puss_work);     // lw v1, collide-puss-work(s7)
  c->lwu(t9, 52, v1);                               // lwu t9, 52(v1)
  c->mov64(a1, s4);                                 // or a1, s4, r0
  c->mov64(a2, gp);                                 // or a2, gp, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->mov64(a0, v1);                                 // or a0, v1, r0
  //beq r0, r0, L23                                 // beq r0, r0, L23
  // nop                                            // sll r0, r0, 0
  goto block_14;                                    // branch always


  block_13:
  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->load_symbol2(v1, cache.collide_puss_work);     // lw v1, collide-puss-work(s7)
  c->lwu(t9, 56, v1);                               // lwu t9, 56(v1)
  c->mov64(a1, s4);                                 // or a1, s4, r0
  c->mov64(a2, gp);                                 // or a2, gp, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->mov64(a0, v1);                                 // or a0, v1, r0

  block_14:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L24
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_17;}                          // branch non-likely

  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  c->mov64(v0, v1);                                 // or v0, v1, r0
  //beq r0, r0, L28                                 // beq r0, r0, L28
  // nop                                            // sll r0, r0, 0
  goto block_22;                                    // branch always

  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0

  block_17:
  c->daddiu(s4, s4, 48);                            // daddiu s4, s4, 48

  block_18:
  bc = c->sgpr64(s2) != 0;                          // bne s2, r0, L20
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_6;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  //beq r0, r0, L27                                 // beq r0, r0, L27
  // nop                                            // sll r0, r0, 0
  goto block_21;                                    // branch always


  block_20:
  c->load_symbol2(t9, cache.format);                // lw t9, format(s7)
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  // daddiu a1, fp, L167                               // daddiu a1, fp, L167
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  printf("ERROR: Exceeded max # of spheres in collide-cache::probe-using-spheres!\n");

  block_21:
  c->mov64(v0, s7);                                 // or v0, s7, r0

  block_22:
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
  c->lq(gp, 80, sp);                                // lq gp, 80(sp)
  c->lq(s5, 64, sp);                                // lq s5, 64(sp)
  c->lq(s4, 48, sp);                                // lq s4, 48(sp)
  c->lq(s3, 32, sp);                                // lq s3, 32(sp)
  c->lq(s2, 16, sp);                                // lq s2, 16(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 96);                            // daddiu sp, sp, 96
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  cache.collide_puss_work = intern_from_c("collide-puss-work").c();
  cache.format = intern_from_c("format").c();
  gLinkedFunctionTable.reg("(method 17 collide-cache)", execute, 128);
}

} // namespace method_17_collide_cache
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace method_9_collide_puss_work {
struct Cache {
  void* closest_pt_in_triangle; // closest-pt-in-triangle
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
  c->mov64(s5, a2);                                 // or s5, a2, r0
  // nop                                            // sll r0, r0, 0
  c->lwu(a0, 32, a1);                               // lwu a0, 32(a1)
  // nop                                            // sll r0, r0, 0
  c->lhu(v1, 40, a1);                               // lhu v1, 40(a1)
  c->daddiu(a0, a0, 4908);                          // daddiu a0, a0, 4908
  c->lhu(s4, 42, a1);                               // lhu s4, 42(a1)
  c->dsll(v1, v1, 6);                               // dsll v1, v1, 6
  // nop                                            // sll r0, r0, 0
  c->daddu(s3, a0, v1);                             // daddu s3, a0, v1
  // nop                                            // sll r0, r0, 0

  block_1:
  bc = c->sgpr64(s4) == 0;                          // beq s4, r0, L15
  c->lqc2(vf1, 0, s3);                              // lqc2 vf1, 0(s3)
  if (bc) {goto block_12;}                          // branch non-likely

  c->daddiu(s4, s4, -1);                            // daddiu s4, s4, -1
  c->lqc2(vf2, 16, s3);                             // lqc2 vf2, 16(s3)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 32, s3);                             // lqc2 vf3, 32(s3)
  c->vsub(DEST::xyzw, vf4, vf2, vf1);               // vsub.xyzw vf4, vf2, vf1
  c->lq(a1, 64, gp);                                // lq a1, 64(gp)
  c->vsub(DEST::xyzw, vf5, vf3, vf1);               // vsub.xyzw vf5, vf3, vf1
  c->lq(v1, 80, gp);                                // lq v1, 80(gp)
  c->vmini(DEST::xyzw, vf6, vf1, vf2);              // vmini.xyzw vf6, vf1, vf2
  // nop                                            // sll r0, r0, 0
  c->vmax(DEST::xyzw, vf7, vf1, vf2);               // vmax.xyzw vf7, vf1, vf2
  // nop                                            // sll r0, r0, 0
  c->vopmula(vf4, vf5);                             // vopmula.xyz acc, vf4, vf5
  // nop                                            // sll r0, r0, 0
  c->vmove(DEST::xyzw, vf8, vf0);                   // vmove.xyzw vf8, vf0
  // nop                                            // sll r0, r0, 0
  c->vmini(DEST::xyzw, vf6, vf6, vf3);              // vmini.xyzw vf6, vf6, vf3
  // nop                                            // sll r0, r0, 0
  c->vmax(DEST::xyzw, vf7, vf7, vf3);               // vmax.xyzw vf7, vf7, vf3
  // nop                                            // sll r0, r0, 0
  c->vopmsub(vf8, vf5, vf4);                        // vopmsub.xyz vf8, vf5, vf4
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf6, vf6);                  // vftoi0.xyzw vf6, vf6
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf7, vf7);                  // vftoi0.xyzw vf7, vf7
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf9, vf8, vf8);               // vmul.xyzw vf9, vf8, vf8
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a0, vf6);                        // qmfc2.i a0, vf6
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a2, vf7);                        // qmfc2.i a2, vf7
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::w, BC::x, vf0, vf9);            // vmulax.w acc, vf0, vf9
  c->sqc2(vf6, 32, gp);                             // sqc2 vf6, 32(gp)
  c->vmadda_bc(DEST::w, BC::y, vf0, vf9);           // vmadday.w acc, vf0, vf9
  c->sqc2(vf7, 48, gp);                             // sqc2 vf7, 48(gp)
  c->vmadd_bc(DEST::w, BC::z, vf9, vf0, vf9);       // vmaddz.w vf9, vf0, vf9
  // nop                                            // sll r0, r0, 0
  c->pcgtw(a1, a1, a2);                             // pcgtw a1, a1, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(v1, a0, v1);                             // pcgtw v1, a0, v1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->por(v1, a1, v1);                               // por v1, a1, v1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->vrsqrt(vf0, BC::w, vf9, BC::w);                // vrsqrt Q, vf0.w, vf9.w
  // nop                                            // sll r0, r0, 0
  c->ppach(v1, r0, v1);                             // ppach v1, r0, v1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsll(v1, v1, 16);                              // dsll v1, v1, 16
  // nop                                            // sll r0, r0, 0
  if (((s64)c->sgpr64(v1)) != ((s64)0)) {           // bnel v1, r0, L13
    c->daddiu(s3, s3, 64);                          // daddiu s3, s3, 64
    goto block_1;
  }

  c->vwaitq();                                      // vwaitq
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf8, vf8);                    // vmulq.xyz vf8, vf8, Q
  c->daddiu(s2, gp, 96);                            // daddiu s2, gp, 96
  // nop                                            // sll r0, r0, 0
  c->lwu(s1, 116, s5);                              // lwu s1, 116(s5)
  c->gprs[s0].du64[0] = 0;                          // or s0, r0, r0
  c->sqc2(vf8, 16, gp);                             // sqc2 vf8, 16(gp)

  block_5:
  if (((s64)c->sgpr64(s0)) == ((s64)c->sgpr64(s1))) {// beql s0, s1, L13
    c->daddiu(s3, s3, 64);                          // daddiu s3, s3, 64
    goto block_1;
  }

  c->daddiu(s0, s0, 1);                             // daddiu s0, s0, 1
  c->lq(a1, 16, s2);                                // lq a1, 16(s2)
  // nop                                            // sll r0, r0, 0
  c->lq(a2, 48, gp);                                // lq a2, 48(gp)
  // nop                                            // sll r0, r0, 0
  c->lq(v1, 32, s2);                                // lq v1, 32(s2)
  // nop                                            // sll r0, r0, 0
  c->lq(a0, 32, gp);                                // lq a0, 32(gp)
  c->pcgtw(a1, a1, a2);                             // pcgtw a1, a1, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(v1, a0, v1);                             // pcgtw v1, a0, v1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->por(v1, a1, v1);                               // por v1, a1, v1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppach(v1, r0, v1);                             // ppach v1, r0, v1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsll(v1, v1, 16);                              // dsll v1, v1, 16
  // nop                                            // sll r0, r0, 0
  if (((s64)c->sgpr64(v1)) != ((s64)0)) {           // bnel v1, r0, L14
    c->daddiu(s2, s2, 48);                          // daddiu s2, s2, 48
    goto block_5;
  }

  c->load_symbol2(t9, cache.closest_pt_in_triangle);// lw t9, closest-pt-in-triangle(s7)
  c->daddu(a0, r0, gp);                             // daddu a0, r0, gp
  c->daddu(a1, r0, s2);                             // daddu a1, r0, s2
  c->daddu(a2, r0, s3);                             // daddu a2, r0, s3
  c->daddiu(a3, gp, 16);                            // daddiu a3, gp, 16
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lqc2(vf10, 0, gp);                             // lqc2 vf10, 0(gp)
  c->lqc2(vf11, 0, s2);                             // lqc2 vf11, 0(s2)
  c->daddiu(s2, s2, 48);                            // daddiu s2, s2, 48
  c->vsub(DEST::xyz, vf9, vf10, vf11);              // vsub.xyz vf9, vf10, vf11
  c->vmul(DEST::w, vf11, vf11, vf11);               // vmul.w vf11, vf11, vf11
  c->vmul(DEST::xyzw, vf9, vf9, vf9);               // vmul.xyzw vf9, vf9, vf9
  c->vmula_bc(DEST::w, BC::x, vf0, vf9);            // vmulax.w acc, vf0, vf9
  c->vmadda_bc(DEST::w, BC::y, vf0, vf9);           // vmadday.w acc, vf0, vf9
  c->vmadd_bc(DEST::w, BC::z, vf9, vf0, vf9);       // vmaddz.w vf9, vf0, vf9
  c->vsub(DEST::w, vf9, vf9, vf11);                 // vsub.w vf9, vf9, vf11
  c->mov128_gpr_vf(v1, vf9);                        // qmfc2.i v1, vf9
  c->pcpyud(v1, v1, v1);                            // pcpyud v1, v1, v1
  if (((s64)c->sgpr64(v1)) > 0) {                   // bgtzl v1, L14
    // nop                                          // sll r0, r0, 0
    goto block_5;
  }

  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  c->mov64(v0, v1);                                 // or v0, v1, r0
  //beq r0, r0, L16                                 // beq r0, r0, L16
  // nop                                            // sll r0, r0, 0
  goto block_14;                                    // branch always


  block_12:
  c->mov64(v0, s7);                                 // or v0, s7, r0
  //beq r0, r0, L16                                 // beq r0, r0, L16
  // nop                                            // sll r0, r0, 0
  goto block_14;                                    // branch always

  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0

  block_14:
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
  cache.closest_pt_in_triangle = intern_from_c("closest-pt-in-triangle").c();
  gLinkedFunctionTable.reg("(method 9 collide-puss-work)", execute, 256);
}

} // namespace method_9_collide_puss_work
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace method_10_collide_puss_work {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lwu(v1, 116, a2);                              // lwu v1, 116(a2)
  c->daddiu(a2, a0, 96);                            // daddiu a2, a0, 96
  c->lqc2(vf1, 0, a1);                              // lqc2 vf1, 0(a1)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L10
  c->lq(a1, 64, a0);                                // lq a1, 64(a0)
  if (bc) {goto block_12;}                          // branch non-likely

  c->vmax_bc(DEST::xyzw, BC::w, vf4, vf0, vf0);     // vmaxw.xyzw vf4, vf0, vf0
  c->lq(a3, 80, a0);                                // lq a3, 80(a0)
  c->vsub_bc(DEST::xyz, BC::w, vf2, vf1, vf1);      // vsubw.xyz vf2, vf1, vf1
  c->gprs[a0].du64[0] = 0;                          // or a0, r0, r0
  c->vadd_bc(DEST::xyz, BC::w, vf3, vf1, vf1);      // vaddw.xyz vf3, vf1, vf1
  c->lqc2(vf5, 0, a2);                              // lqc2 vf5, 0(a2)
  c->vftoi0(DEST::xyzw, vf2, vf2);                  // vftoi0.xyzw vf2, vf2
  c->lqc2(vf6, 48, a2);                             // lqc2 vf6, 48(a2)
  c->vftoi0(DEST::xyzw, vf3, vf3);                  // vftoi0.xyzw vf3, vf3
  c->lqc2(vf7, 96, a2);                             // lqc2 vf7, 96(a2)
  // nop                                            // sll r0, r0, 0
  c->vsub_bc(DEST::w, BC::w, vf1, vf0, vf1);        // vsubw.w vf1, vf0, vf1
  c->mov128_gpr_vf(t0, vf2);                        // qmfc2.i t0, vf2
  c->lqc2(vf8, 144, a2);                            // lqc2 vf8, 144(a2)
  c->mov128_gpr_vf(t1, vf3);                        // qmfc2.i t1, vf3
  // nop                                            // sll r0, r0, 0
  c->pcgtw(a1, a1, t1);                             // pcgtw a1, a1, t1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(a3, t0, a3);                             // pcgtw a3, t0, a3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->por(a1, a1, a3);                               // por a1, a1, a3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppach(a1, r0, a1);                             // ppach a1, r0, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsll(a1, a1, 16);                              // dsll a1, a1, 16
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a1) != 0;                          // bne a1, r0, L10
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely


  block_2:
  c->vsub(DEST::xyzw, vf9, vf5, vf1);               // vsub.xyzw vf9, vf5, vf1
  c->daddiu(a2, a2, 192);                           // daddiu a2, a2, 192
  c->vsub(DEST::xyzw, vf10, vf6, vf1);              // vsub.xyzw vf10, vf6, vf1
  c->lqc2(vf5, 0, a2);                              // lqc2 vf5, 0(a2)
  c->vsub(DEST::xyzw, vf11, vf7, vf1);              // vsub.xyzw vf11, vf7, vf1
  c->lqc2(vf6, 48, a2);                             // lqc2 vf6, 48(a2)
  c->vsub(DEST::xyzw, vf12, vf8, vf1);              // vsub.xyzw vf12, vf8, vf1
  c->lqc2(vf7, 96, a2);                             // lqc2 vf7, 96(a2)
  c->vmul(DEST::xyzw, vf9, vf9, vf9);               // vmul.xyzw vf9, vf9, vf9
  c->lqc2(vf8, 144, a2);                            // lqc2 vf8, 144(a2)
  c->vmul(DEST::xyzw, vf10, vf10, vf10);            // vmul.xyzw vf10, vf10, vf10
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf11, vf11, vf11);            // vmul.xyzw vf11, vf11, vf11
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf12, vf12, vf12);            // vmul.xyzw vf12, vf12, vf12
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::x, vf4, vf9);         // vmulax.xyzw acc, vf4, vf9
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf4, vf9);        // vmadday.xyzw acc, vf4, vf9
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::z, vf4, vf9);        // vmaddaz.xyzw acc, vf4, vf9
  // nop                                            // sll r0, r0, 0
  c->vmsub_bc(DEST::xyzw, BC::w, vf9, vf4, vf9);    // vmsubw.xyzw vf9, vf4, vf9
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::x, vf4, vf10);        // vmulax.xyzw acc, vf4, vf10
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf4, vf10);       // vmadday.xyzw acc, vf4, vf10
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::z, vf4, vf10);       // vmaddaz.xyzw acc, vf4, vf10
  // nop                                            // sll r0, r0, 0
  c->vmsub_bc(DEST::xyzw, BC::w, vf10, vf4, vf10);  // vmsubw.xyzw vf10, vf4, vf10
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a1, vf9);                        // qmfc2.i a1, vf9
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  bc = ((s64)c->sgpr64(a1)) <= 0;                   // blez a1, L9
  c->vmula_bc(DEST::xyzw, BC::x, vf4, vf11);        // vmulax.xyzw acc, vf4, vf11
  if (bc) {goto block_11;}                          // branch non-likely

  bc = c->sgpr64(a0) == c->sgpr64(v1);              // beq a0, v1, L10
  c->vmadda_bc(DEST::xyzw, BC::y, vf4, vf11);       // vmadday.xyzw acc, vf4, vf11
  if (bc) {goto block_12;}                          // branch non-likely

  c->vmadda_bc(DEST::xyzw, BC::z, vf4, vf11);       // vmaddaz.xyzw acc, vf4, vf11
  // nop                                            // sll r0, r0, 0
  c->vmsub_bc(DEST::xyzw, BC::w, vf11, vf4, vf11);  // vmsubw.xyzw vf11, vf4, vf11
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a1, vf10);                       // qmfc2.i a1, vf10
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  bc = ((s64)c->sgpr64(a1)) <= 0;                   // blez a1, L9
  c->vmula_bc(DEST::xyzw, BC::x, vf4, vf12);        // vmulax.xyzw acc, vf4, vf12
  if (bc) {goto block_11;}                          // branch non-likely

  bc = c->sgpr64(a0) == c->sgpr64(v1);              // beq a0, v1, L10
  c->vmadda_bc(DEST::xyzw, BC::y, vf4, vf12);       // vmadday.xyzw acc, vf4, vf12
  if (bc) {goto block_12;}                          // branch non-likely

  c->vmadda_bc(DEST::xyzw, BC::z, vf4, vf12);       // vmaddaz.xyzw acc, vf4, vf12
  // nop                                            // sll r0, r0, 0
  c->vmsub_bc(DEST::xyzw, BC::w, vf12, vf4, vf12);  // vmsubw.xyzw vf12, vf4, vf12
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a1, vf11);                       // qmfc2.i a1, vf11
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  bc = ((s64)c->sgpr64(a1)) <= 0;                   // blez a1, L9
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_11;}                          // branch non-likely

  bc = c->sgpr64(a0) == c->sgpr64(v1);              // beq a0, v1, L10
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely

  c->mov128_gpr_vf(a1, vf12);                       // qmfc2.i a1, vf12
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  bc = ((s64)c->sgpr64(a1)) <= 0;                   // blez a1, L9
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_11;}                          // branch non-likely

  bc = c->sgpr64(a0) != c->sgpr64(v1);              // bne a0, v1, L8
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  //beq r0, r0, L10                                 // beq r0, r0, L10
  // nop                                            // sll r0, r0, 0
  goto block_12;                                    // branch always


  block_11:
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  c->mov64(v0, v1);                                 // or v0, v1, r0
  //beq r0, r0, L11                                 // beq r0, r0, L11
  // nop                                            // sll r0, r0, 0
  goto block_14;                                    // branch always


  block_12:
  c->mov64(v0, s7);                                 // or v0, s7, r0
  //beq r0, r0, L11                                 // beq r0, r0, L11
  // nop                                            // sll r0, r0, 0
  goto block_14;                                    // branch always

  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0

  block_14:
  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  gLinkedFunctionTable.reg("(method 10 collide-puss-work)", execute, 256);
}

} // namespace method_10_collide_puss_work
} // namespace Mips2C
