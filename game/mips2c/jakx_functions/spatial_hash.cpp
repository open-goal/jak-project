//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace method_36_spatial_hash {
struct Cache {
  void* debug_segment; // *debug-segment*
  void* format; // format
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -160);                          // daddiu sp, sp, -160
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  c->sq(s0, 48, sp);                                // sq s0, 48(sp)
  c->sq(s1, 64, sp);                                // sq s1, 64(sp)
  c->sq(s2, 80, sp);                                // sq s2, 80(sp)
  c->sq(s3, 96, sp);                                // sq s3, 96(sp)
  c->sq(s4, 112, sp);                               // sq s4, 112(sp)
  c->sq(s5, 128, sp);                               // sq s5, 128(sp)
  c->sq(gp, 144, sp);                               // sq gp, 144(sp)
  c->mov64(s5, a0);                                 // or s5, a0, r0
  c->mov64(s3, a1);                                 // or s3, a1, r0
  c->mov64(s4, a2);                                 // or s4, a2, r0
  c->lh(gp, 56, s5);                                // lh gp, 56(s5)
  c->lh(v1, 88, s5);                                // lh v1, 88(s5)
  c->slt(v1, gp, v1);                               // slt v1, gp, v1
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L39
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_8;}                           // branch non-likely

  c->lwu(v1, 84, s5);                               // lwu v1, 84(s5)
  c->dsll(a0, gp, 4);                               // dsll a0, gp, 4
  c->daddu(s2, v1, a0);                             // daddu s2, v1, a0
  c->lwu(v1, 100, s5);                              // lwu v1, 100(s5)
  c->dsll(a0, gp, 4);                               // dsll a0, gp, 4
  c->daddu(s1, v1, a0);                             // daddu s1, v1, a0
  c->daddiu(s0, sp, 16);                            // daddiu s0, sp, 16
  c->daddu(a0, r0, s0);                             // daddu a0, r0, s0
  c->mov64(v1, s3);                                 // or v1, s3, r0
  c->lwc1(f0, 12, s3);                              // lwc1 f0, 12(s3)
  c->negs(f0, f0);                                  // neg.s f0, f0
  c->mfc1(a1, f0);                                  // mfc1 a1, f0
  c->mov128_vf_gpr(vf6, a1);                        // qmtc2.i vf6, a1
  c->lqc2(vf4, 0, v1);                              // lqc2 vf4, 0(v1)
  c->vadd_bc(DEST::w, BC::x, vf5, vf0, vf0);        // vaddx.w vf5, vf0, vf0
  c->vadd_bc(DEST::xyz, BC::x, vf5, vf4, vf6);      // vaddx.xyz vf5, vf4, vf6
  c->sqc2(vf5, 0, a0);                              // sqc2 vf5, 0(a0)
  c->daddiu(a0, s0, 16);                            // daddiu a0, s0, 16
  c->mov64(v1, s3);                                 // or v1, s3, r0
  c->lwc1(f0, 12, s3);                              // lwc1 f0, 12(s3)
  c->mfc1(a1, f0);                                  // mfc1 a1, f0
  c->mov128_vf_gpr(vf6, a1);                        // qmtc2.i vf6, a1
  c->lqc2(vf4, 0, v1);                              // lqc2 vf4, 0(v1)
  c->vadd_bc(DEST::w, BC::x, vf5, vf0, vf0);        // vaddx.w vf5, vf0, vf0
  c->vadd_bc(DEST::xyz, BC::x, vf5, vf4, vf6);      // vaddx.xyz vf5, vf4, vf6
  c->sqc2(vf5, 0, a0);                              // sqc2 vf5, 0(a0)
  c->load_symbol2(v1, cache.debug_segment);         // lw v1, *debug-segment*(s7)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L36
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_4;}                           // branch non-likely

  c->lui(v1, 19584);                                // lui v1, 19584
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lwc1(f1, 0, s3);                               // lwc1 f1, 0(s3)
  c->abss(f1, f1);                                  // abs.s f1, f1
  c->lwc1(f2, 4, s3);                               // lwc1 f2, 4(s3)
  c->abss(f2, f2);                                  // abs.s f2, f2
  c->maxs(f1, f1, f2);                              // max.s f1, f1, f2
  c->lwc1(f2, 8, s3);                               // lwc1 f2, 8(s3)
  c->abss(f2, f2);                                  // abs.s f2, f2
  c->maxs(f1, f1, f2);                              // max.s f1, f1, f2
  c->lwc1(f2, 12, s3);                              // lwc1 f2, 12(s3)
  c->abss(f2, f2);                                  // abs.s f2, f2
  c->maxs(f1, f1, f2);                              // max.s f1, f1, f2
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = !cop1_bc;                                    // bc1f L36
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_4;}                           // branch non-likely

  c->load_symbol2(t9, cache.format);                // lw t9, format(s7)
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  // TODO - fix daddiu a1, fp, L198                               // daddiu a1, fp, L198
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lw(r0, 2, r0);                                 // lw r0, 2(r0)
  c->addiu(v1, r0, 0);                              // addiu v1, r0, 0
  
block_4:
  c->addiu(v1, r0, 0);                              // addiu v1, r0, 0
  //beq r0, r0, L38                                 // beq r0, r0, L38
  // nop                                            // sll r0, r0, 0
  goto block_6;                                     // branch always

  
block_5:
  c->dsll(a0, v1, 2);                               // dsll a0, v1, 2
  c->daddu(a0, a0, s5);                             // daddu a0, a0, s5
  c->lwc1(f0, 32, a0);                              // lwc1 f0, 32(a0)
  c->dsll(a0, v1, 2);                               // dsll a0, v1, 2
  c->daddu(a0, a0, s0);                             // daddu a0, a0, s0
  c->lwc1(f1, 0, a0);                               // lwc1 f1, 0(a0)
  c->mins(f0, f0, f1);                              // min.s f0, f0, f1
  c->dsll(a0, v1, 2);                               // dsll a0, v1, 2
  c->daddu(a0, a0, s5);                             // daddu a0, a0, s5
  c->swc1(f0, 32, a0);                              // swc1 f0, 32(a0)
  c->dsll(a0, v1, 2);                               // dsll a0, v1, 2
  c->daddu(a0, a0, s5);                             // daddu a0, a0, s5
  c->lwc1(f0, 44, a0);                              // lwc1 f0, 44(a0)
  c->dsll(a0, v1, 2);                               // dsll a0, v1, 2
  c->daddu(a0, a0, s0);                             // daddu a0, a0, s0
  c->lwc1(f1, 16, a0);                              // lwc1 f1, 16(a0)
  c->maxs(f0, f0, f1);                              // max.s f0, f0, f1
  c->dsll(a0, v1, 2);                               // dsll a0, v1, 2
  c->daddu(a0, a0, s5);                             // daddu a0, a0, s5
  c->swc1(f0, 44, a0);                              // swc1 f0, 44(a0)
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  
block_6:
  c->slti(a0, v1, 3);                               // slti a0, v1, 3
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L37
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_5;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->lq(v1, 0, s3);                                 // lq v1, 0(s3)
  c->sq(v1, 0, s2);                                 // sq v1, 0(s2)
  c->sw(s4, 0, s1);                                 // sw s4, 0(s1)
  c->lh(v1, 56, s5);                                // lh v1, 56(s5)
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  c->sh(v1, 56, s5);                                // sh v1, 56(s5)
  //beq r0, r0, L40                                 // beq r0, r0, L40
  // nop                                            // sll r0, r0, 0
  goto block_9;                                     // branch always

  
block_8:
  c->addiu(gp, r0, -1);                             // addiu gp, r0, -1
  c->mov64(v1, gp);                                 // or v1, gp, r0
  
block_9:
  c->mov64(v0, gp);                                 // or v0, gp, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
  c->lq(gp, 144, sp);                               // lq gp, 144(sp)
  c->lq(s5, 128, sp);                               // lq s5, 128(sp)
  c->lq(s4, 112, sp);                               // lq s4, 112(sp)
  c->lq(s3, 96, sp);                                // lq s3, 96(sp)
  c->lq(s2, 80, sp);                                // lq s2, 80(sp)
  c->lq(s1, 64, sp);                                // lq s1, 64(sp)
  c->lq(s0, 48, sp);                                // lq s0, 48(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 160);                           // daddiu sp, sp, 160
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.debug_segment = intern_from_c(-1, 0, "*debug-segment*").c();
  cache.format = intern_from_c(-1, 0, "format").c();
  gLinkedFunctionTable.reg("(method 36 spatial-hash)", execute, 512);
}

} // namespace method_36_spatial_hash
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace method_35_spatial_hash {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -48);                           // daddiu sp, sp, -48
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s5, 16, sp);                                // sq s5, 16(sp)
  c->sq(gp, 32, sp);                                // sq gp, 32(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->mov64(s5, a2);                                 // or s5, a2, r0
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 112, v1);                              // lwu t9, 112(v1)
  c->daddiu(v1, gp, 4);                             // daddiu v1, gp, 4
  c->mov64(a2, a1);                                 // or a2, a1, r0
  c->mov64(a1, v1);                                 // or a1, v1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->mov64(t1, gp);                                 // or t1, gp, r0
  c->daddiu(t2, gp, 4);                             // daddiu t2, gp, 4
  c->mov64(t0, s5);                                 // or t0, s5, r0
  c->lh(v1, 10, t1);                                // lh v1, 10(t1)
  c->lb(a0, 24, t1);                                // lb a0, 24(t1)
  c->mult3(a0, a0, v1);                             // mult3 a0, a0, v1
  c->lb(a1, 26, t1);                                // lb a1, 26(t1)
  c->mult3(a1, a1, a0);                             // mult3 a1, a1, a0
  c->addiu(a2, r0, 1);                              // addiu a2, r0, 1
  c->lb(a3, 0, t2);                                 // lb a3, 0(t2)
  c->dsubu(a2, a2, a3);                             // dsubu a2, a2, a3
  c->lb(a3, 3, t2);                                 // lb a3, 3(t2)
  c->daddu(a2, a2, a3);                             // daddu a2, a2, a3
  c->addiu(a3, r0, 1);                              // addiu a3, r0, 1
  c->lb(t3, 2, t2);                                 // lb t3, 2(t2)
  c->dsubu(a3, a3, t3);                             // dsubu a3, a3, t3
  c->lb(t3, 5, t2);                                 // lb t3, 5(t2)
  c->daddu(a3, a3, t3);                             // daddu a3, a3, t3
  c->addiu(t3, r0, 1);                              // addiu t3, r0, 1
  c->lb(t4, 1, t2);                                 // lb t4, 1(t2)
  c->dsubu(t3, t3, t4);                             // dsubu t3, t3, t4
  c->lb(t4, 4, t2);                                 // lb t4, 4(t2)
  c->daddu(t3, t3, t4);                             // daddu t3, t3, t4
  c->lb(t4, 0, t2);                                 // lb t4, 0(t2)
  c->mult3(t4, t4, v1);                             // mult3 t4, t4, v1
  c->lb(t5, 1, t2);                                 // lb t5, 1(t2)
  c->mult3(t5, t5, a1);                             // mult3 t5, t5, a1
  c->daddu(t4, t4, t5);                             // daddu t4, t4, t5
  c->lb(t2, 2, t2);                                 // lb t2, 2(t2)
  c->mult3(t2, t2, a0);                             // mult3 t2, t2, a0
  c->daddu(t2, t4, t2);                             // daddu t2, t4, t2
  c->dsra(t4, t0, 3);                               // dsra t4, t0, 3
  c->daddu(t2, t2, t4);                             // daddu t2, t2, t4
  c->daddu(t2, r0, t2);                             // daddu t2, r0, t2
  c->lwu(t1, 28, t1);                               // lwu t1, 28(t1)
  c->daddu(t1, t2, t1);                             // daddu t1, t2, t1
  c->addiu(t2, r0, 1);                              // addiu t2, r0, 1
  c->andi(t0, t0, 7);                               // andi t0, t0, 7
  if (((s64)c->sgpr64(t0)) >= 0) {                  // bgezl t0, L42
    c->dsllv(t0, t2, t0);                           // dsllv t0, t2, t0
    goto block_3;
  }
  
block_2:
  c->dsubu(t0, r0, t0);                             // dsubu t0, r0, t0
  c->dsrav(t0, t2, t0);                             // dsrav t0, t2, t0
  
block_3:
  c->nor(t0, t0, r0);                               // nor t0, t0, r0
  c->mov64(t2, t3);                                 // or t2, t3, r0
  
block_4:
  c->mov64(t3, a3);                                 // or t3, a3, r0
  c->mov64(t4, t1);                                 // or t4, t1, r0
  
block_5:
  c->mov64(t5, a2);                                 // or t5, a2, r0
  c->mov64(t6, t4);                                 // or t6, t4, r0
  
block_6:
  // nop                                            // sll r0, r0, 0
  c->lbu(t7, 0, t6);                                // lbu t7, 0(t6)
  // nop                                            // sll r0, r0, 0
  c->and_(t7, t7, t0);                              // and t7, t7, t0
  c->daddiu(t5, t5, -1);                            // daddiu t5, t5, -1
  c->sb(t7, 0, t6);                                 // sb t7, 0(t6)
  bc = c->sgpr64(t5) != 0;                          // bne t5, r0, L45
  c->daddu(t6, t6, v1);                             // daddu t6, t6, v1
  if (bc) {goto block_6;}                           // branch non-likely

  c->daddiu(t3, t3, -1);                            // daddiu t3, t3, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L44
  c->daddu(t4, t4, a0);                             // daddu t4, t4, a0
  if (bc) {goto block_5;}                           // branch non-likely

  c->daddiu(t2, t2, -1);                            // daddiu t2, t2, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t2) != 0;                          // bne t2, r0, L43
  c->daddu(t1, t1, a1);                             // daddu t1, t1, a1
  if (bc) {goto block_4;}                           // branch non-likely

  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->dsll(v1, s5, 4);                               // dsll v1, s5, 4
  c->lwu(a0, 100, gp);                              // lwu a0, 100(gp)
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  c->sw(s7, 0, v1);                                 // sw s7, 0(v1)
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
  return c->gprs[v0].du64[0];
}

void link() {
  gLinkedFunctionTable.reg("(method 35 spatial-hash)", execute, 256);
}

} // namespace method_35_spatial_hash
} // namespace Mips2C
// add method_35_spatial_hash::link to the link callback table for the object file.
// FWD DEC:
namespace method_35_spatial_hash { extern void link(); }
//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace method_33_sphere_hash {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -32);                           // daddiu sp, sp, -32
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->daddiu(v1, sp, 16);                            // daddiu v1, sp, 16
  c->lwu(t1, -4, a0);                               // lwu t1, -4(a0)
  c->lwu(t9, 152, t1);                              // lwu t9, 152(t1)
  c->addiu(t1, r0, 1);                              // addiu t1, r0, 1
  c->mov64(t2, t0);                                 // or t2, t0, r0
  c->mov64(t0, v1);                                 // or t0, v1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->slt(v1, r0, v1);                               // slt v1, r0, v1
  c->daddiu(v0, s7, 4);                             // daddiu v0, s7, 4
  c->movz(v0, s7, v1);                              // movz v0, s7, v1
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 32);                            // daddiu sp, sp, 32
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  gLinkedFunctionTable.reg("(method 33 sphere-hash)", execute, 256);
}

} // namespace method_33_sphere_hash
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace method_30_sphere_hash {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -48);                           // daddiu sp, sp, -48
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s5, 16, sp);                                // sq s5, 16(sp)
  c->sq(gp, 32, sp);                                // sq gp, 32(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 120, v1);                              // lwu t9, 120(v1)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->addiu(s5, r0, 0);                              // addiu s5, r0, 0
  //beq r0, r0, L104                                // beq r0, r0, L104
  // nop                                            // sll r0, r0, 0
  goto block_11;                                    // branch always

  
block_1:
  c->lwu(v1, 84, gp);                               // lwu v1, 84(gp)
  c->dsll(a0, s5, 4);                               // dsll a0, s5, 4
  c->daddu(a2, v1, a0);                             // daddu a2, v1, a0
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 112, v1);                              // lwu t9, 112(v1)
  c->daddiu(a1, gp, 4);                             // daddiu a1, gp, 4
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->mov64(t1, gp);                                 // or t1, gp, r0
  c->daddiu(t2, gp, 4);                             // daddiu t2, gp, 4
  c->mov64(a3, s5);                                 // or a3, s5, r0
  c->lh(v1, 10, t1);                                // lh v1, 10(t1)
  c->lb(a0, 24, t1);                                // lb a0, 24(t1)
  c->mult3(a0, a0, v1);                             // mult3 a0, a0, v1
  c->lb(a1, 26, t1);                                // lb a1, 26(t1)
  c->mult3(a1, a1, a0);                             // mult3 a1, a1, a0
  c->addiu(a2, r0, 1);                              // addiu a2, r0, 1
  c->lb(t0, 0, t2);                                 // lb t0, 0(t2)
  c->dsubu(a2, a2, t0);                             // dsubu a2, a2, t0
  c->lb(t0, 3, t2);                                 // lb t0, 3(t2)
  c->daddu(a2, a2, t0);                             // daddu a2, a2, t0
  c->addiu(t0, r0, 1);                              // addiu t0, r0, 1
  c->lb(t3, 2, t2);                                 // lb t3, 2(t2)
  c->dsubu(t0, t0, t3);                             // dsubu t0, t0, t3
  c->lb(t3, 5, t2);                                 // lb t3, 5(t2)
  c->daddu(t0, t0, t3);                             // daddu t0, t0, t3
  c->addiu(t3, r0, 1);                              // addiu t3, r0, 1
  c->lb(t4, 1, t2);                                 // lb t4, 1(t2)
  c->dsubu(t3, t3, t4);                             // dsubu t3, t3, t4
  c->lb(t4, 4, t2);                                 // lb t4, 4(t2)
  c->daddu(t3, t3, t4);                             // daddu t3, t3, t4
  c->lb(t4, 0, t2);                                 // lb t4, 0(t2)
  c->mult3(t4, t4, v1);                             // mult3 t4, t4, v1
  c->lb(t5, 1, t2);                                 // lb t5, 1(t2)
  c->mult3(t5, t5, a1);                             // mult3 t5, t5, a1
  c->daddu(t4, t4, t5);                             // daddu t4, t4, t5
  c->lb(t2, 2, t2);                                 // lb t2, 2(t2)
  c->mult3(t2, t2, a0);                             // mult3 t2, t2, a0
  c->daddu(t2, t4, t2);                             // daddu t2, t4, t2
  c->dsra(t4, a3, 3);                               // dsra t4, a3, 3
  c->daddu(t2, t2, t4);                             // daddu t2, t2, t4
  c->daddu(t2, r0, t2);                             // daddu t2, r0, t2
  c->lwu(t1, 28, t1);                               // lwu t1, 28(t1)
  c->daddu(t1, t2, t1);                             // daddu t1, t2, t1
  c->addiu(t2, r0, 1);                              // addiu t2, r0, 1
  c->andi(a3, a3, 7);                               // andi a3, a3, 7
  if (((s64)c->sgpr64(a3)) >= 0) {                  // bgezl a3, L100
    c->dsllv(a3, t2, a3);                           // dsllv a3, t2, a3
    goto block_4;
  }
  
block_3:
  c->dsubu(a3, r0, a3);                             // dsubu a3, r0, a3
  c->dsrav(a3, t2, a3);                             // dsrav a3, t2, a3
  
block_4:
  c->mov64(t2, t3);                                 // or t2, t3, r0
  
block_5:
  c->mov64(t3, t0);                                 // or t3, t0, r0
  c->mov64(t4, t1);                                 // or t4, t1, r0
  
block_6:
  c->mov64(t5, a2);                                 // or t5, a2, r0
  c->mov64(t6, t4);                                 // or t6, t4, r0
  
block_7:
  // nop                                            // sll r0, r0, 0
  c->lbu(t7, 0, t6);                                // lbu t7, 0(t6)
  // nop                                            // sll r0, r0, 0
  c->or_(t7, t7, a3);                               // or t7, t7, a3
  c->daddiu(t5, t5, -1);                            // daddiu t5, t5, -1
  c->sb(t7, 0, t6);                                 // sb t7, 0(t6)
  bc = c->sgpr64(t5) != 0;                          // bne t5, r0, L103
  c->daddu(t6, t6, v1);                             // daddu t6, t6, v1
  if (bc) {goto block_7;}                           // branch non-likely

  c->daddiu(t3, t3, -1);                            // daddiu t3, t3, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L102
  c->daddu(t4, t4, a0);                             // daddu t4, t4, a0
  if (bc) {goto block_6;}                           // branch non-likely

  c->daddiu(t2, t2, -1);                            // daddiu t2, t2, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t2) != 0;                          // bne t2, r0, L101
  c->daddu(t1, t1, a1);                             // daddu t1, t1, a1
  if (bc) {goto block_5;}                           // branch non-likely

  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->daddiu(s5, s5, 1);                             // daddiu s5, s5, 1
  
block_11:
  c->lh(v1, 56, gp);                                // lh v1, 56(gp)
  c->slt(v1, s5, v1);                               // slt v1, s5, v1
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L99
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

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
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  gLinkedFunctionTable.reg("(method 30 sphere-hash)", execute, 256);
}

} // namespace method_30_sphere_hash
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace method_22_grid_hash {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -48);                           // daddiu sp, sp, -48
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s5, 16, sp);                                // sq s5, 16(sp)
  c->sq(gp, 32, sp);                                // sq gp, 32(sp)
  c->lh(v1, 10, a0);                                // lh v1, 10(a0)
  c->lb(a3, 24, a0);                                // lb a3, 24(a0)
  c->mult3(a3, v1, a3);                             // mult3 a3, v1, a3
  c->lb(t0, 26, a0);                                // lb t0, 26(a0)
  c->mult3(t0, a3, t0);                             // mult3 t0, a3, t0
  c->lb(t1, 3, a1);                                 // lb t1, 3(a1)
  c->lb(t2, 0, a1);                                 // lb t2, 0(a1)
  c->dsubu(t1, t1, t2);                             // dsubu t1, t1, t2
  c->lb(t2, 5, a1);                                 // lb t2, 5(a1)
  c->lb(t3, 2, a1);                                 // lb t3, 2(a1)
  c->dsubu(t2, t2, t3);                             // dsubu t2, t2, t3
  c->lb(t3, 4, a1);                                 // lb t3, 4(a1)
  c->lb(t4, 1, a1);                                 // lb t4, 1(a1)
  c->dsubu(t3, t3, t4);                             // dsubu t3, t3, t4
  c->lb(t4, 0, a1);                                 // lb t4, 0(a1)
  c->mult3(t4, t4, v1);                             // mult3 t4, t4, v1
  c->lb(t5, 1, a1);                                 // lb t5, 1(a1)
  c->mult3(t5, t5, t0);                             // mult3 t5, t5, t0
  c->daddu(t4, t4, t5);                             // daddu t4, t4, t5
  c->lb(a1, 2, a1);                                 // lb a1, 2(a1)
  c->mult3(a1, a1, a3);                             // mult3 a1, a1, a3
  c->daddu(a1, t4, a1);                             // daddu a1, t4, a1
  c->daddu(a1, r0, a1);                             // daddu a1, r0, a1
  c->lwu(t4, 28, a0);                               // lwu t4, 28(a0)
  c->daddu(a1, a1, t4);                             // daddu a1, a1, t4
  c->lh(a0, 10, a0);                                // lh a0, 10(a0)
  c->mov64(t4, a2);                                 // or t4, a2, r0
  
block_1:
  c->daddiu(a0, a0, -16);                           // daddiu a0, a0, -16
  c->sq(r0, 0, t4);                                 // sq r0, 0(t4)
  bc = ((s64)c->sgpr64(a0)) > 0;                    // bgtz a0, L177
  c->daddiu(t4, t4, 16);                            // daddiu t4, t4, 16
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(a0, t3);                                 // or a0, t3, r0
  // nop                                            // sll r0, r0, 0
  
block_3:
  c->mov64(t3, t2);                                 // or t3, t2, r0
  c->mov64(t4, a1);                                 // or t4, a1, r0
  
block_4:
  c->mov64(t5, t1);                                 // or t5, t1, r0
  c->mov64(t6, t4);                                 // or t6, t4, r0
  
block_5:
  c->slti(t8, v1, 9);                               // slti t8, v1, 9
  c->mov64(t7, v1);                                 // or t7, v1, r0
  bc = c->sgpr64(t8) != 0;                          // bne t8, r0, L182
  c->mov64(t8, a2);                                 // or t8, a2, r0
  if (bc) {goto block_8;}                           // branch non-likely

  
block_6:
  c->mov64(ra, t8);                                 // or ra, t8, r0
  c->ldr(t9, 0, t6);                                // ldr t9, 0(t6)
  c->daddiu(t8, t8, 8);                             // daddiu t8, t8, 8
  c->ldl(t9, 7, t6);                                // ldl t9, 7(t6)
  c->daddiu(t7, t7, -8);                            // daddiu t7, t7, -8
  c->ld(s5, 0, ra);                                 // ld s5, 0(ra)
  c->slti(gp, t7, 8);                               // slti gp, t7, 8
  c->or_(t9, s5, t9);                               // or t9, s5, t9
  c->daddiu(t6, t6, 8);                             // daddiu t6, t6, 8
  c->sd(t9, 0, ra);                                 // sd t9, 0(ra)
  bc = c->sgpr64(gp) == 0;                          // beq gp, r0, L181
  c->gprs[t9].du64[0] = 0;                          // or t9, r0, r0
  if (bc) {goto block_6;}                           // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L183
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_9;}                           // branch non-likely

  
block_8:
  // nop                                            // sll r0, r0, 0
  c->ld(ra, 0, t8);                                 // ld ra, 0(t8)
  // nop                                            // sll r0, r0, 0
  c->ldr(t9, 0, t6);                                // ldr t9, 0(t6)
  // nop                                            // sll r0, r0, 0
  c->ldl(t9, 7, t6);                                // ldl t9, 7(t6)
  c->daddu(t6, t6, t7);                             // daddu t6, t6, t7
  c->or_(t7, ra, t9);                               // or t7, ra, t9
  // nop                                            // sll r0, r0, 0
  c->sd(t7, 0, t8);                                 // sd t7, 0(t8)
  
block_9:
  bc = ((s64)c->sgpr64(t5)) > 0;                    // bgtz t5, L180
  c->daddiu(t5, t5, -1);                            // daddiu t5, t5, -1
  if (bc) {goto block_5;}                           // branch non-likely

  c->daddu(t4, t4, a3);                             // daddu t4, t4, a3
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(t3)) > 0;                    // bgtz t3, L179
  c->daddiu(t3, t3, -1);                            // daddiu t3, t3, -1
  if (bc) {goto block_4;}                           // branch non-likely

  c->daddu(a1, a1, t0);                             // daddu a1, a1, t0
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(a0)) > 0;                    // bgtz a0, L178
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  if (bc) {goto block_3;}                           // branch non-likely

  c->mov64(v0, a2);                                 // or v0, a2, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 32, sp);                                // lq gp, 32(sp)
  c->lq(s5, 16, sp);                                // lq s5, 16(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 48);                            // daddiu sp, sp, 48
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  gLinkedFunctionTable.reg("(method 22 grid-hash)", execute, 256);
}

} // namespace method_22_grid_hash
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace method_20_grid_hash {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->lh(v1, 10, a0);                                // lh v1, 10(a0)
  c->lb(a3, 24, a0);                                // lb a3, 24(a0)
  c->mult3(a3, a3, v1);                             // mult3 a3, a3, v1
  c->lb(t0, 26, a0);                                // lb t0, 26(a0)
  c->mult3(t0, t0, a3);                             // mult3 t0, t0, a3
  c->addiu(t1, r0, 1);                              // addiu t1, r0, 1
  c->lb(t2, 0, a1);                                 // lb t2, 0(a1)
  c->dsubu(t1, t1, t2);                             // dsubu t1, t1, t2
  c->lb(t2, 3, a1);                                 // lb t2, 3(a1)
  c->daddu(t1, t1, t2);                             // daddu t1, t1, t2
  c->addiu(t2, r0, 1);                              // addiu t2, r0, 1
  c->lb(t3, 2, a1);                                 // lb t3, 2(a1)
  c->dsubu(t2, t2, t3);                             // dsubu t2, t2, t3
  c->lb(t3, 5, a1);                                 // lb t3, 5(a1)
  c->daddu(t2, t2, t3);                             // daddu t2, t2, t3
  c->addiu(t3, r0, 1);                              // addiu t3, r0, 1
  c->lb(t4, 1, a1);                                 // lb t4, 1(a1)
  c->dsubu(t3, t3, t4);                             // dsubu t3, t3, t4
  c->lb(t4, 4, a1);                                 // lb t4, 4(a1)
  c->daddu(t3, t3, t4);                             // daddu t3, t3, t4
  c->lb(t4, 0, a1);                                 // lb t4, 0(a1)
  c->mult3(t4, t4, v1);                             // mult3 t4, t4, v1
  c->lb(t5, 1, a1);                                 // lb t5, 1(a1)
  c->mult3(t5, t5, t0);                             // mult3 t5, t5, t0
  c->daddu(t4, t4, t5);                             // daddu t4, t4, t5
  c->lb(a1, 2, a1);                                 // lb a1, 2(a1)
  c->mult3(a1, a1, a3);                             // mult3 a1, a1, a3
  c->daddu(a1, t4, a1);                             // daddu a1, t4, a1
  c->dsra(t4, a2, 3);                               // dsra t4, a2, 3
  c->daddu(a1, a1, t4);                             // daddu a1, a1, t4
  c->daddu(a1, r0, a1);                             // daddu a1, r0, a1
  c->lwu(a0, 28, a0);                               // lwu a0, 28(a0)
  c->daddu(a0, a1, a0);                             // daddu a0, a1, a0
  c->addiu(a1, r0, 1);                              // addiu a1, r0, 1
  c->andi(a2, a2, 7);                               // andi a2, a2, 7
  if (((s64)c->sgpr64(a2)) >= 0) {                  // bgezl a2, L190
    c->dsllv(a1, a1, a2);                           // dsllv a1, a1, a2
    goto block_3;
  }
  
block_2:
  c->dsubu(a2, r0, a2);                             // dsubu a2, r0, a2
  c->dsrav(a1, a1, a2);                             // dsrav a1, a1, a2
  
block_3:
  c->mov64(a2, t3);                                 // or a2, t3, r0
  // nop                                            // sll r0, r0, 0
  
block_4:
  c->mov64(t3, t2);                                 // or t3, t2, r0
  c->mov64(t4, a0);                                 // or t4, a0, r0
  
block_5:
  c->mov64(t5, t1);                                 // or t5, t1, r0
  c->mov64(t6, t4);                                 // or t6, t4, r0
  
block_6:
  // nop                                            // sll r0, r0, 0
  c->lbu(t7, 0, t6);                                // lbu t7, 0(t6)
  // nop                                            // sll r0, r0, 0
  c->or_(t7, t7, a1);                               // or t7, t7, a1
  c->daddiu(t5, t5, -1);                            // daddiu t5, t5, -1
  c->sb(t7, 0, t6);                                 // sb t7, 0(t6)
  bc = c->sgpr64(t5) != 0;                          // bne t5, r0, L193
  c->daddu(t6, t6, v1);                             // daddu t6, t6, v1
  if (bc) {goto block_6;}                           // branch non-likely

  c->daddiu(t3, t3, -1);                            // daddiu t3, t3, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L192
  c->daddu(t4, t4, a3);                             // daddu t4, t4, a3
  if (bc) {goto block_5;}                           // branch non-likely

  c->daddiu(a2, a2, -1);                            // daddiu a2, a2, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L191
  c->daddu(a0, a0, t0);                             // daddu a0, a0, t0
  if (bc) {goto block_4;}                           // branch non-likely

  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
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
  gLinkedFunctionTable.reg("(method 20 grid-hash)", execute, 256);
}

} // namespace method_20_grid_hash
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace method_21_grid_hash {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->lh(v1, 10, a0);                                // lh v1, 10(a0)
  c->lb(a3, 24, a0);                                // lb a3, 24(a0)
  c->mult3(a3, a3, v1);                             // mult3 a3, a3, v1
  c->lb(t0, 26, a0);                                // lb t0, 26(a0)
  c->mult3(t0, t0, a3);                             // mult3 t0, t0, a3
  c->addiu(t1, r0, 1);                              // addiu t1, r0, 1
  c->lb(t2, 0, a1);                                 // lb t2, 0(a1)
  c->dsubu(t1, t1, t2);                             // dsubu t1, t1, t2
  c->lb(t2, 3, a1);                                 // lb t2, 3(a1)
  c->daddu(t1, t1, t2);                             // daddu t1, t1, t2
  c->addiu(t2, r0, 1);                              // addiu t2, r0, 1
  c->lb(t3, 2, a1);                                 // lb t3, 2(a1)
  c->dsubu(t2, t2, t3);                             // dsubu t2, t2, t3
  c->lb(t3, 5, a1);                                 // lb t3, 5(a1)
  c->daddu(t2, t2, t3);                             // daddu t2, t2, t3
  c->addiu(t3, r0, 1);                              // addiu t3, r0, 1
  c->lb(t4, 1, a1);                                 // lb t4, 1(a1)
  c->dsubu(t3, t3, t4);                             // dsubu t3, t3, t4
  c->lb(t4, 4, a1);                                 // lb t4, 4(a1)
  c->daddu(t3, t3, t4);                             // daddu t3, t3, t4
  c->lb(t4, 0, a1);                                 // lb t4, 0(a1)
  c->mult3(t4, t4, v1);                             // mult3 t4, t4, v1
  c->lb(t5, 1, a1);                                 // lb t5, 1(a1)
  c->mult3(t5, t5, t0);                             // mult3 t5, t5, t0
  c->daddu(t4, t4, t5);                             // daddu t4, t4, t5
  c->lb(a1, 2, a1);                                 // lb a1, 2(a1)
  c->mult3(a1, a1, a3);                             // mult3 a1, a1, a3
  c->daddu(a1, t4, a1);                             // daddu a1, t4, a1
  c->dsra(t4, a2, 3);                               // dsra t4, a2, 3
  c->daddu(a1, a1, t4);                             // daddu a1, a1, t4
  c->daddu(a1, r0, a1);                             // daddu a1, r0, a1
  c->lwu(a0, 28, a0);                               // lwu a0, 28(a0)
  c->daddu(a0, a1, a0);                             // daddu a0, a1, a0
  c->addiu(a1, r0, 1);                              // addiu a1, r0, 1
  c->andi(a2, a2, 7);                               // andi a2, a2, 7
  if (((s64)c->sgpr64(a2)) >= 0) {                  // bgezl a2, L185
    c->dsllv(a1, a1, a2);                           // dsllv a1, a1, a2
    goto block_3;
  }
  
block_2:
  c->dsubu(a2, r0, a2);                             // dsubu a2, r0, a2
  c->dsrav(a1, a1, a2);                             // dsrav a1, a1, a2
  
block_3:
  c->nor(a1, a1, r0);                               // nor a1, a1, r0
  c->mov64(a2, t3);                                 // or a2, t3, r0
  
block_4:
  c->mov64(t3, t2);                                 // or t3, t2, r0
  c->mov64(t4, a0);                                 // or t4, a0, r0
  
block_5:
  c->mov64(t5, t1);                                 // or t5, t1, r0
  c->mov64(t6, t4);                                 // or t6, t4, r0
  
block_6:
  // nop                                            // sll r0, r0, 0
  c->lbu(t7, 0, t6);                                // lbu t7, 0(t6)
  // nop                                            // sll r0, r0, 0
  c->and_(t7, t7, a1);                              // and t7, t7, a1
  c->daddiu(t5, t5, -1);                            // daddiu t5, t5, -1
  c->sb(t7, 0, t6);                                 // sb t7, 0(t6)
  bc = c->sgpr64(t5) != 0;                          // bne t5, r0, L188
  c->daddu(t6, t6, v1);                             // daddu t6, t6, v1
  if (bc) {goto block_6;}                           // branch non-likely

  c->daddiu(t3, t3, -1);                            // daddiu t3, t3, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L187
  c->daddu(t4, t4, a3);                             // daddu t4, t4, a3
  if (bc) {goto block_5;}                           // branch non-likely

  c->daddiu(a2, a2, -1);                            // daddiu a2, a2, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L186
  c->daddu(a0, a0, t0);                             // daddu a0, a0, t0
  if (bc) {goto block_4;}                           // branch non-likely

  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
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
  gLinkedFunctionTable.reg("(method 21 grid-hash)", execute, 256);
}

} // namespace method_21_grid_hash
} // namespace Mips2C
// add method_21_grid_hash::link to the link callback table for the object file.
// FWD DEC:
namespace method_21_grid_hash { extern void link(); }

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace method_24_grid_hash {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lq(v1, 0, a2);                                 // lq v1, 0(a2)
  c->mov128_vf_gpr(vf1, v1);                        // qmtc2.i vf1, v1
  c->lq(v1, 12, a0);                                // lq v1, 12(a0)
  c->mov128_vf_gpr(vf5, v1);                        // qmtc2.i vf5, v1
  c->vsub_bc(DEST::xyzw, BC::w, vf3, vf1, vf1);     // vsubw.xyzw vf3, vf1, vf1
  c->vadd_bc(DEST::xyzw, BC::w, vf2, vf1, vf1);     // vaddw.xyzw vf2, vf1, vf1
  c->lq(v1, 28, a0);                                // lq v1, 28(a0)
  c->mov128_vf_gpr(vf4, v1);                        // qmtc2.i vf4, v1
  c->vmr32(DEST::xyzw, vf4, vf4);                   // vmr32.xyzw vf4, vf4
  c->vsub(DEST::xyzw, vf6, vf3, vf4);               // vsub.xyzw vf6, vf3, vf4
  c->vsub(DEST::xyzw, vf7, vf2, vf4);               // vsub.xyzw vf7, vf2, vf4
  c->vmul(DEST::xyzw, vf6, vf6, vf5);               // vmul.xyzw vf6, vf6, vf5
  c->vmul(DEST::xyzw, vf7, vf7, vf5);               // vmul.xyzw vf7, vf7, vf5
  c->vftoi0(DEST::xyz, vf6, vf6);                   // vftoi0.xyz vf6, vf6
  c->vftoi0(DEST::xyz, vf7, vf7);                   // vftoi0.xyz vf7, vf7
  c->nor(v1, r0, r0);                               // nor v1, r0, r0
  c->lw(a0, 24, a0);                                // lw a0, 24(a0)
  c->sll(a2, a0, 8);                                // sll a2, a0, 8
  c->lui(a0, 1);                                    // lui a0, 1
  c->srl(a2, a2, 8);                                // srl a2, a2, 8
  c->ori(a0, a0, 257);                              // ori a0, a0, 257
  c->dsubu(a0, a2, a0);                             // dsubu a0, a2, a0
  // nop                                            // sll r0, r0, 0
  c->pextlb(a0, r0, a0);                            // pextlb a0, r0, a0
  // nop                                            // sll r0, r0, 0
  c->pextlh(a0, r0, a0);                            // pextlh a0, r0, a0
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a3, vf6);                        // qmfc2.i a3, vf6
  c->mov128_gpr_vf(a2, vf7);                        // qmfc2.i a2, vf7
  c->pmaxw(a3, a3, r0);                             // pmaxw a3, a3, r0
  c->pmaxw(a2, a2, r0);                             // pmaxw a2, a2, r0
  c->pminw(a3, a3, a0);                             // pminw a3, a3, a0
  c->pminw(a0, a2, a0);                             // pminw a0, a2, a0
  c->ppach(a2, r0, a3);                             // ppach a2, r0, a3
  c->ppach(a3, r0, a0);                             // ppach a3, r0, a0
  c->ppacb(a0, r0, a2);                             // ppacb a0, r0, a2
  c->ppacb(a2, r0, a3);                             // ppacb a2, r0, a3
  c->dsll(a2, a2, 24);                              // dsll a2, a2, 24
  c->or_(a0, a0, a2);                               // or a0, a0, a2
  c->dsll32(v1, v1, 16);                            // dsll32 v1, v1, 16
  c->ld(a2, 0, a1);                                 // ld a2, 0(a1)
  c->and_(v1, a2, v1);                              // and v1, a2, v1
  c->or_(v1, v1, a0);                               // or v1, v1, a0
  c->sd(v1, 0, a1);                                 // sd v1, 0(a1)
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  gLinkedFunctionTable.reg("(method 24 grid-hash)", execute, 256);
}

} // namespace method_24_grid_hash
} // namespace Mips2C
// add method_24_grid_hash::link to the link callback table for the object file.
// FWD DEC:
namespace method_24_grid_hash { extern void link(); }

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace method_35_sphere_hash {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -48);                           // daddiu sp, sp, -48
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s5, 16, sp);                                // sq s5, 16(sp)
  c->sq(gp, 32, sp);                                // sq gp, 32(sp)
  c->mov64(s5, a0);                                 // or s5, a0, r0
  c->mov64(gp, a2);                                 // or gp, a2, r0
  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 112, v1);                              // lwu t9, 112(v1)
  c->daddiu(v1, s5, 4);                             // daddiu v1, s5, 4
  c->mov64(a2, a1);                                 // or a2, a1, r0
  c->mov64(a1, v1);                                 // or a1, v1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->mov64(t0, s5);                                 // or t0, s5, r0
  c->daddiu(t1, s5, 4);                             // daddiu t1, s5, 4
  c->lh(v1, 10, t0);                                // lh v1, 10(t0)
  c->lb(a0, 24, t0);                                // lb a0, 24(t0)
  c->mult3(a0, a0, v1);                             // mult3 a0, a0, v1
  c->lb(a1, 26, t0);                                // lb a1, 26(t0)
  c->mult3(a1, a1, a0);                             // mult3 a1, a1, a0
  c->addiu(a2, r0, 1);                              // addiu a2, r0, 1
  c->lb(a3, 0, t1);                                 // lb a3, 0(t1)
  c->dsubu(a2, a2, a3);                             // dsubu a2, a2, a3
  c->lb(a3, 3, t1);                                 // lb a3, 3(t1)
  c->daddu(a2, a2, a3);                             // daddu a2, a2, a3
  c->addiu(a3, r0, 1);                              // addiu a3, r0, 1
  c->lb(t2, 2, t1);                                 // lb t2, 2(t1)
  c->dsubu(a3, a3, t2);                             // dsubu a3, a3, t2
  c->lb(t2, 5, t1);                                 // lb t2, 5(t1)
  c->daddu(a3, a3, t2);                             // daddu a3, a3, t2
  c->addiu(t2, r0, 1);                              // addiu t2, r0, 1
  c->lb(t3, 1, t1);                                 // lb t3, 1(t1)
  c->dsubu(t2, t2, t3);                             // dsubu t2, t2, t3
  c->lb(t3, 4, t1);                                 // lb t3, 4(t1)
  c->daddu(t2, t2, t3);                             // daddu t2, t2, t3
  c->lb(t3, 0, t1);                                 // lb t3, 0(t1)
  c->mult3(t3, t3, v1);                             // mult3 t3, t3, v1
  c->lb(t4, 1, t1);                                 // lb t4, 1(t1)
  c->mult3(t4, t4, a1);                             // mult3 t4, t4, a1
  c->daddu(t3, t3, t4);                             // daddu t3, t3, t4
  c->lb(t1, 2, t1);                                 // lb t1, 2(t1)
  c->mult3(t1, t1, a0);                             // mult3 t1, t1, a0
  c->daddu(t1, t3, t1);                             // daddu t1, t3, t1
  c->dsra(t3, gp, 3);                               // dsra t3, gp, 3
  c->daddu(t1, t1, t3);                             // daddu t1, t1, t3
  c->daddu(t1, r0, t1);                             // daddu t1, r0, t1
  c->lwu(t0, 28, t0);                               // lwu t0, 28(t0)
  c->daddu(t0, t1, t0);                             // daddu t0, t1, t0
  c->addiu(t1, r0, 1);                              // addiu t1, r0, 1
  c->andi(t3, gp, 7);                               // andi t3, gp, 7
  if (((s64)c->sgpr64(t3)) >= 0) {                  // bgezl t3, L84
    c->dsllv(t1, t1, t3);                           // dsllv t1, t1, t3
    goto block_3;
  }
  
block_2:
  c->dsubu(t3, r0, t3);                             // dsubu t3, r0, t3
  c->dsrav(t1, t1, t3);                             // dsrav t1, t1, t3
  
block_3:
  c->nor(t1, t1, r0);                               // nor t1, t1, r0
  c->mov64(t2, t2);                                 // or t2, t2, r0
  // nop                                            // sll r0, r0, 0
  
block_4:
  c->mov64(t3, a3);                                 // or t3, a3, r0
  c->mov64(t4, t0);                                 // or t4, t0, r0
  
block_5:
  c->mov64(t5, a2);                                 // or t5, a2, r0
  c->mov64(t6, t4);                                 // or t6, t4, r0
  
block_6:
  // nop                                            // sll r0, r0, 0
  c->lbu(t7, 0, t6);                                // lbu t7, 0(t6)
  // nop                                            // sll r0, r0, 0
  c->and_(t7, t7, t1);                              // and t7, t7, t1
  c->daddiu(t5, t5, -1);                            // daddiu t5, t5, -1
  c->sb(t7, 0, t6);                                 // sb t7, 0(t6)
  bc = c->sgpr64(t5) != 0;                          // bne t5, r0, L87
  c->daddu(t6, t6, v1);                             // daddu t6, t6, v1
  if (bc) {goto block_6;}                           // branch non-likely

  c->daddiu(t3, t3, -1);                            // daddiu t3, t3, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L86
  c->daddu(t4, t4, a0);                             // daddu t4, t4, a0
  if (bc) {goto block_5;}                           // branch non-likely

  c->daddiu(t2, t2, -1);                            // daddiu t2, t2, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t2) != 0;                          // bne t2, r0, L85
  c->daddu(t0, t0, a1);                             // daddu t0, t0, a1
  if (bc) {goto block_4;}                           // branch non-likely

  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
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
  return c->gprs[v0].du64[0];
}

void link() {
  gLinkedFunctionTable.reg("(method 35 sphere-hash)", execute, 256);
}

} // namespace method_35_sphere_hash
} // namespace Mips2C
// add method_35_sphere_hash::link to the link callback table for the object file.
// FWD DEC:
namespace method_35_sphere_hash { extern void link(); }

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace method_40_spatial_hash {
struct Cache {
  void* mem_copy; // mem-copy!
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -96);                           // daddiu sp, sp, -96
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s2, 16, sp);                                // sq s2, 16(sp)
  c->sq(s3, 32, sp);                                // sq s3, 32(sp)
  c->sq(s4, 48, sp);                                // sq s4, 48(sp)
  c->sq(s5, 64, sp);                                // sq s5, 64(sp)
  c->sq(gp, 80, sp);                                // sq gp, 80(sp)
  c->mov64(s5, a0);                                 // or s5, a0, r0
  c->mov64(s4, a2);                                 // or s4, a2, r0
  c->lh(gp, 56, s5);                                // lh gp, 56(s5)
  c->lh(v1, 88, s5);                                // lh v1, 88(s5)
  c->slt(v1, gp, v1);                               // slt v1, gp, v1
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L33
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_11;}                          // branch non-likely

  c->lwu(v1, 84, s5);                               // lwu v1, 84(s5)
  c->dsll(a0, gp, 4);                               // dsll a0, gp, 4
  c->daddu(s3, v1, a0);                             // daddu s3, v1, a0
  c->lwu(v1, 100, s5);                              // lwu v1, 100(s5)
  c->dsll(a0, gp, 4);                               // dsll a0, gp, 4
  c->daddu(s2, v1, a0);                             // daddu s2, v1, a0
  c->load_symbol2(t9, cache.mem_copy);              // lw t9, mem-copy!(s7)
  c->mov64(a0, s3);                                 // or a0, s3, r0
  c->addiu(a2, r0, 16);                             // addiu a2, r0, 16
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->sw(s4, 0, s2);                                 // sw s4, 0(s2)
  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 112, v1);                              // lwu t9, 112(v1)
  c->daddiu(a1, s5, 4);                             // daddiu a1, s5, 4
  c->mov64(a2, s3);                                 // or a2, s3, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->mov64(t1, s5);                                 // or t1, s5, r0
  c->daddiu(t2, s5, 4);                             // daddiu t2, s5, 4
  c->mov64(a3, gp);                                 // or a3, gp, r0
  c->lh(v1, 10, t1);                                // lh v1, 10(t1)
  c->lb(a0, 24, t1);                                // lb a0, 24(t1)
  c->mult3(a0, a0, v1);                             // mult3 a0, a0, v1
  c->lb(a1, 26, t1);                                // lb a1, 26(t1)
  c->mult3(a1, a1, a0);                             // mult3 a1, a1, a0
  c->addiu(a2, r0, 1);                              // addiu a2, r0, 1
  c->lb(t0, 0, t2);                                 // lb t0, 0(t2)
  c->dsubu(a2, a2, t0);                             // dsubu a2, a2, t0
  c->lb(t0, 3, t2);                                 // lb t0, 3(t2)
  c->daddu(a2, a2, t0);                             // daddu a2, a2, t0
  c->addiu(t0, r0, 1);                              // addiu t0, r0, 1
  c->lb(t3, 2, t2);                                 // lb t3, 2(t2)
  c->dsubu(t0, t0, t3);                             // dsubu t0, t0, t3
  c->lb(t3, 5, t2);                                 // lb t3, 5(t2)
  c->daddu(t0, t0, t3);                             // daddu t0, t0, t3
  c->addiu(t3, r0, 1);                              // addiu t3, r0, 1
  c->lb(t4, 1, t2);                                 // lb t4, 1(t2)
  c->dsubu(t3, t3, t4);                             // dsubu t3, t3, t4
  c->lb(t4, 4, t2);                                 // lb t4, 4(t2)
  c->daddu(t3, t3, t4);                             // daddu t3, t3, t4
  c->lb(t4, 0, t2);                                 // lb t4, 0(t2)
  c->mult3(t4, t4, v1);                             // mult3 t4, t4, v1
  c->lb(t5, 1, t2);                                 // lb t5, 1(t2)
  c->mult3(t5, t5, a1);                             // mult3 t5, t5, a1
  c->daddu(t4, t4, t5);                             // daddu t4, t4, t5
  c->lb(t2, 2, t2);                                 // lb t2, 2(t2)
  c->mult3(t2, t2, a0);                             // mult3 t2, t2, a0
  c->daddu(t2, t4, t2);                             // daddu t2, t4, t2
  c->dsra(t4, a3, 3);                               // dsra t4, a3, 3
  c->daddu(t2, t2, t4);                             // daddu t2, t2, t4
  c->daddu(t2, r0, t2);                             // daddu t2, r0, t2
  c->lwu(t1, 28, t1);                               // lwu t1, 28(t1)
  c->daddu(t1, t2, t1);                             // daddu t1, t2, t1
  c->addiu(t2, r0, 1);                              // addiu t2, r0, 1
  c->andi(a3, a3, 7);                               // andi a3, a3, 7
  if (((s64)c->sgpr64(a3)) >= 0) {                  // bgezl a3, L29
    c->dsllv(a3, t2, a3);                           // dsllv a3, t2, a3
    goto block_4;
  }
  
block_3:
  c->dsubu(a3, r0, a3);                             // dsubu a3, r0, a3
  c->dsrav(a3, t2, a3);                             // dsrav a3, t2, a3
  
block_4:
  c->mov64(t2, t3);                                 // or t2, t3, r0
  
block_5:
  c->mov64(t3, t0);                                 // or t3, t0, r0
  c->mov64(t4, t1);                                 // or t4, t1, r0
  
block_6:
  c->mov64(t5, a2);                                 // or t5, a2, r0
  c->mov64(t6, t4);                                 // or t6, t4, r0
  
block_7:
  // nop                                            // sll r0, r0, 0
  c->lbu(t7, 0, t6);                                // lbu t7, 0(t6)
  // nop                                            // sll r0, r0, 0
  c->or_(t7, t7, a3);                               // or t7, t7, a3
  c->daddiu(t5, t5, -1);                            // daddiu t5, t5, -1
  c->sb(t7, 0, t6);                                 // sb t7, 0(t6)
  bc = c->sgpr64(t5) != 0;                          // bne t5, r0, L32
  c->daddu(t6, t6, v1);                             // daddu t6, t6, v1
  if (bc) {goto block_7;}                           // branch non-likely

  c->daddiu(t3, t3, -1);                            // daddiu t3, t3, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L31
  c->daddu(t4, t4, a0);                             // daddu t4, t4, a0
  if (bc) {goto block_6;}                           // branch non-likely

  c->daddiu(t2, t2, -1);                            // daddiu t2, t2, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t2) != 0;                          // bne t2, r0, L30
  c->daddu(t1, t1, a1);                             // daddu t1, t1, a1
  if (bc) {goto block_5;}                           // branch non-likely

  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->lh(v1, 56, s5);                                // lh v1, 56(s5)
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  c->sh(v1, 56, s5);                                // sh v1, 56(s5)
  //beq r0, r0, L34                                 // beq r0, r0, L34
  // nop                                            // sll r0, r0, 0
  goto block_12;                                    // branch always

  
block_11:
  c->addiu(gp, r0, -1);                             // addiu gp, r0, -1
  c->mov64(v1, gp);                                 // or v1, gp, r0
  
block_12:
  c->mov64(v0, gp);                                 // or v0, gp, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
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
  cache.mem_copy = intern_from_c(-1, 0, "mem-copy!").c();
  gLinkedFunctionTable.reg("(method 40 spatial-hash)", execute, 256);
}

} // namespace method_40_spatial_hash
} // namespace Mips2C
// add method_40_spatial_hash::link to the link callback table for the object file.
// FWD DEC:
namespace method_40_spatial_hash { extern void link(); }
