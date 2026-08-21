//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace method_19_grid_hash {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -32);                           // daddiu sp, sp, -32
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(gp, 16, sp);                                // sq gp, 16(sp)
  c->addiu(v1, r0, 0);                              // addiu v1, r0, 0
  //beq r0, r0, L235                                // beq r0, r0, L235
  // nop                                            // sll r0, r0, 0
  goto block_2;                                     // branch always

  
block_1:
  c->daddu(a3, v1, a0);                             // daddu a3, v1, a0
  c->lb(a3, 24, a3);                                // lb a3, 24(a3)
  c->daddu(t0, v1, a2);                             // daddu t0, v1, a2
  c->sb(a3, 0, t0);                                 // sb a3, 0(t0)
  c->addiu(a3, r0, -1);                             // addiu a3, r0, -1
  c->daddu(t0, v1, a2);                             // daddu t0, v1, a2
  c->sb(a3, 3, t0);                                 // sb a3, 3(t0)
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  
block_2:
  c->slti(a3, v1, 3);                               // slti a3, v1, 3
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L234
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->lh(v1, 10, a0);                                // lh v1, 10(a0)
  c->lb(a3, 24, a0);                                // lb a3, 24(a0)
  c->mult3(a3, a3, v1);                             // mult3 a3, a3, v1
  c->lb(t0, 26, a0);                                // lb t0, 26(a0)
  c->mult3(t0, t0, a3);                             // mult3 t0, t0, a3
  c->lb(t1, 24, a0);                                // lb t1, 24(a0)
  c->lb(t2, 26, a0);                                // lb t2, 26(a0)
  c->lb(t3, 25, a0);                                // lb t3, 25(a0)
  c->dsra(t4, a1, 3);                               // dsra t4, a1, 3
  c->daddu(t4, r0, t4);                             // daddu t4, r0, t4
  c->lwu(a0, 28, a0);                               // lwu a0, 28(a0)
  c->daddu(a0, t4, a0);                             // daddu a0, t4, a0
  c->addiu(t4, r0, 1);                              // addiu t4, r0, 1
  c->andi(a1, a1, 7);                               // andi a1, a1, 7
  if (((s64)c->sgpr64(a1)) >= 0) {                  // bgezl a1, L236
    c->dsllv(a1, t4, a1);                           // dsllv a1, t4, a1
    goto block_6;
  }
  
block_5:
  c->dsubu(a1, r0, a1);                             // dsubu a1, r0, a1
  c->dsrav(a1, t4, a1);                             // dsrav a1, t4, a1
  
block_6:
  c->addiu(t4, r0, 0);                              // addiu t4, r0, 0
  //beq r0, r0, L243                                // beq r0, r0, L243
  // nop                                            // sll r0, r0, 0
  goto block_16;                                    // branch always

  
block_7:
  c->mov64(t5, a0);                                 // or t5, a0, r0
  c->addiu(t6, r0, 0);                              // addiu t6, r0, 0
  //beq r0, r0, L242                                // beq r0, r0, L242
  // nop                                            // sll r0, r0, 0
  goto block_14;                                    // branch always

  
block_8:
  c->mov64(t7, t5);                                 // or t7, t5, r0
  c->addiu(t8, r0, 0);                              // addiu t8, r0, 0
  //beq r0, r0, L241                                // beq r0, r0, L241
  // nop                                            // sll r0, r0, 0
  goto block_12;                                    // branch always

  
block_9:
  c->lbu(t9, 0, t7);                                // lbu t9, 0(t7)
  c->and_(t9, t9, a1);                              // and t9, t9, a1
  bc = c->sgpr64(t9) == 0;                          // beq t9, r0, L240
  c->mov64(t9, s7);                                 // or t9, s7, r0
  if (bc) {goto block_11;}                          // branch non-likely

  c->lb(t9, 0, a2);                                 // lb t9, 0(a2)
  c->mov64(t9, t9);                                 // or t9, t9, r0
  c->mov64(ra, t8);                                 // or ra, t8, r0
  c->slt(gp, t9, ra);                               // slt gp, t9, ra
  c->movz(t9, ra, gp);                              // movz t9, ra, gp
  c->sb(t9, 0, a2);                                 // sb t9, 0(a2)
  c->lb(t9, 1, a2);                                 // lb t9, 1(a2)
  c->mov64(t9, t9);                                 // or t9, t9, r0
  c->mov64(ra, t4);                                 // or ra, t4, r0
  c->slt(gp, t9, ra);                               // slt gp, t9, ra
  c->movz(t9, ra, gp);                              // movz t9, ra, gp
  c->sb(t9, 1, a2);                                 // sb t9, 1(a2)
  c->lb(t9, 2, a2);                                 // lb t9, 2(a2)
  c->mov64(t9, t9);                                 // or t9, t9, r0
  c->mov64(ra, t6);                                 // or ra, t6, r0
  c->slt(gp, t9, ra);                               // slt gp, t9, ra
  c->movz(t9, ra, gp);                              // movz t9, ra, gp
  c->sb(t9, 2, a2);                                 // sb t9, 2(a2)
  c->lb(t9, 3, a2);                                 // lb t9, 3(a2)
  c->mov64(t9, t9);                                 // or t9, t9, r0
  c->mov64(ra, t8);                                 // or ra, t8, r0
  c->slt(gp, t9, ra);                               // slt gp, t9, ra
  c->movn(t9, ra, gp);                              // movn t9, ra, gp
  c->sb(t9, 3, a2);                                 // sb t9, 3(a2)
  c->lb(t9, 4, a2);                                 // lb t9, 4(a2)
  c->mov64(t9, t9);                                 // or t9, t9, r0
  c->mov64(ra, t4);                                 // or ra, t4, r0
  c->slt(gp, t9, ra);                               // slt gp, t9, ra
  c->movn(t9, ra, gp);                              // movn t9, ra, gp
  c->sb(t9, 4, a2);                                 // sb t9, 4(a2)
  c->lb(t9, 5, a2);                                 // lb t9, 5(a2)
  c->mov64(gp, t9);                                 // or gp, t9, r0
  c->mov64(t9, t6);                                 // or t9, t6, r0
  c->slt(ra, gp, t9);                               // slt ra, gp, t9
  c->movn(gp, t9, ra);                              // movn gp, t9, ra
  c->sb(gp, 5, a2);                                 // sb gp, 5(a2)
  
block_11:
  c->daddu(t7, t7, v1);                             // daddu t7, t7, v1
  c->daddiu(t8, t8, 1);                             // daddiu t8, t8, 1
  
block_12:
  c->slt(t9, t8, t1);                               // slt t9, t8, t1
  bc = c->sgpr64(t9) != 0;                          // bne t9, r0, L239
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_9;}                           // branch non-likely

  c->mov64(t7, s7);                                 // or t7, s7, r0
  c->mov64(t7, s7);                                 // or t7, s7, r0
  c->daddu(t5, t5, a3);                             // daddu t5, t5, a3
  c->daddiu(t6, t6, 1);                             // daddiu t6, t6, 1
  
block_14:
  c->slt(t7, t6, t2);                               // slt t7, t6, t2
  bc = c->sgpr64(t7) != 0;                          // bne t7, r0, L238
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_8;}                           // branch non-likely

  c->mov64(t5, s7);                                 // or t5, s7, r0
  c->mov64(t5, s7);                                 // or t5, s7, r0
  c->daddu(a0, a0, t0);                             // daddu a0, a0, t0
  c->daddiu(t4, t4, 1);                             // daddiu t4, t4, 1
  
block_16:
  c->slt(t5, t4, t3);                               // slt t5, t4, t3
  bc = c->sgpr64(t5) != 0;                          // bne t5, r0, L237
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 16, sp);                                // lq gp, 16(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 32);                            // daddiu sp, sp, 32
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  gLinkedFunctionTable.reg("(method 19 grid-hash)", execute, 256);
}

} // namespace method_19_grid_hash
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace method_18_grid_hash {
struct Cache {
  void* format; // format
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -64);                           // daddiu sp, sp, -64
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  c->sq(s4, 16, sp);                                // sq s4, 16(sp)
  c->sq(s5, 32, sp);                                // sq s5, 32(sp)
  c->sq(gp, 48, sp);                                // sq gp, 48(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->addiu(s5, r0, 0);                              // addiu s5, r0, 0
  c->addiu(v1, r0, 0);                              // addiu v1, r0, 0
  c->addiu(v1, r0, 8);                              // addiu v1, r0, 8
  c->lh(a0, 56, gp);                                // lh a0, 56(gp)
  c->lh(a1, 10, gp);                                // lh a1, 10(gp)
  c->dsll(a1, a1, 3);                               // dsll a1, a1, 3
  //beq r0, r0, L247                                // beq r0, r0, L247
  // nop                                            // sll r0, r0, 0
  goto block_5;                                     // branch always

  
block_1:
  c->div(a0, v1);                                   // div a0, v1
  c->mflo(a2);                                      // mflo a2
  c->mult3(a2, a2, v1);                             // mult3 a2, a2, v1
  c->dsubu(a3, a0, a2);                             // dsubu a3, a0, a2
  c->addiu(a2, r0, 1);                              // addiu a2, r0, 1
  if (((s64)c->sgpr64(a3)) >= 0) {                  // bgezl a3, L246
    c->dsllv(a2, a2, a3);                           // dsllv a2, a2, a3
    goto block_4;
  }
  
block_3:
  c->dsubu(a3, r0, a3);                             // dsubu a3, r0, a3
  c->dsrav(a2, a2, a3);                             // dsrav a2, a2, a3
  
block_4:
  c->or_(s5, s5, a2);                               // or s5, s5, a2
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  c->mov64(a2, a0);                                 // or a2, a0, r0
  
block_5:
  c->slt(a2, a0, a1);                               // slt a2, a0, a1
  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L245
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->addiu(s4, r0, 0);                              // addiu s4, r0, 0
  //beq r0, r0, L250                                // beq r0, r0, L250
  // nop                                            // sll r0, r0, 0
  goto block_10;                                    // branch always

  
block_7:
  c->lwu(v1, 28, gp);                               // lwu v1, 28(gp)
  c->lh(a0, 10, gp);                                // lh a0, 10(gp)
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->lh(a1, 10, gp);                                // lh a1, 10(gp)
  c->mult3(a1, s4, a1);                             // mult3 a1, s4, a1
  c->daddu(a0, a0, a1);                             // daddu a0, a0, a1
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  c->lbu(a3, 0, v1);                                // lbu a3, 0(v1)
  c->and_(v1, a3, s5);                              // and v1, a3, s5
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L249
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_9;}                           // branch non-likely

  c->load_symbol2(t9, cache.format);                // lw t9, format(s7)
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  // TODO FIX - daddiu a1, fp, L259                               // daddiu a1, fp, L259
  c->mov64(a2, s4);                                 // or a2, s4, r0
  c->mov64(t0, s5);                                 // or t0, s5, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lw(r0, 2, r0);                                 // lw r0, 2(r0)
  c->addiu(v1, r0, 0);                              // addiu v1, r0, 0
  
block_9:
  c->daddiu(s4, s4, 1);                             // daddiu s4, s4, 1
  
block_10:
  c->lh(v1, 58, gp);                                // lh v1, 58(gp)
  c->slt(v1, s4, v1);                               // slt v1, s4, v1
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L248
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
  c->lq(gp, 48, sp);                                // lq gp, 48(sp)
  c->lq(s5, 32, sp);                                // lq s5, 32(sp)
  c->lq(s4, 16, sp);                                // lq s4, 16(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 64);                            // daddiu sp, sp, 64
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.format = intern_from_c(-1, 0, "format").c();
  gLinkedFunctionTable.reg("(method 18 grid-hash)", execute, 256);
}

} // namespace method_18_grid_hash
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace method_34_spatial_hash {
struct Cache {
  void* vector_vector_distance_squared; // vector-vector-distance-squared
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -208);                          // daddiu sp, sp, -208
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s1, 112, sp);                               // sq s1, 112(sp)
  c->sq(s2, 128, sp);                               // sq s2, 128(sp)
  c->sq(s3, 144, sp);                               // sq s3, 144(sp)
  c->sq(s4, 160, sp);                               // sq s4, 160(sp)
  c->sq(s5, 176, sp);                               // sq s5, 176(sp)
  c->sq(gp, 192, sp);                               // sq gp, 192(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->mov64(s5, t2);                                 // or s5, t2, r0
  c->sw(a1, 16, sp);                                // sw a1, 16(sp)
  c->sw(a2, 20, sp);                                // sw a2, 20(sp)
  c->mtc1(f0, a3);                                  // mtc1 f0, a3
  c->swc1(f0, 24, sp);                              // swc1 f0, 24(sp)
  c->sw(t0, 28, sp);                                // sw t0, 28(sp)
  c->sd(t1, 32, sp);                                // sd t1, 32(sp)
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 116, v1);                              // lwu t9, 116(v1)
  c->daddiu(a1, gp, 4);                             // daddiu a1, gp, 4
  c->lwu(a2, 16, sp);                               // lwu a2, 16(sp)
  c->lwu(a3, 20, sp);                               // lwu a3, 20(sp)
  c->lwc1(f0, 24, sp);                              // lwc1 f0, 24(sp)
  c->mfc1(t0, f0);                                  // mfc1 t0, f0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->lwu(v1, 0, gp);                                // lwu v1, 0(gp)
  c->daddiu(v1, v1, 12);                            // daddiu v1, v1, 12
  c->sw(v1, 40, sp);                                // sw v1, 40(sp)
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 104, v1);                              // lwu t9, 104(v1)
  c->daddiu(a1, gp, 4);                             // daddiu a1, gp, 4
  c->lwu(a2, 40, sp);                               // lwu a2, 40(sp)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->addiu(v1, r0, -1);                             // addiu v1, r0, -1
  bc = c->sgpr64(s5) == c->sgpr64(v1);              // beq s5, v1, L17
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_5;}                           // branch non-likely

  c->dsra(v1, s5, 3);                               // dsra v1, s5, 3
  c->andi(a1, s5, 7);                               // andi a1, s5, 7
  c->addiu(a0, r0, 1);                              // addiu a0, r0, 1
  if (((s64)c->sgpr64(a1)) >= 0) {                  // bgezl a1, L16
    c->dsllv(a0, a0, a1);                           // dsllv a0, a0, a1
    goto block_4;
  }
  
block_3:
  c->dsubu(a1, r0, a1);                             // dsubu a1, r0, a1
  c->dsrav(a0, a0, a1);                             // dsrav a0, a0, a1
  
block_4:
  c->nor(a0, a0, r0);                               // nor a0, a0, r0
  c->lwu(a1, 40, sp);                               // lwu a1, 40(sp)
  c->daddu(a1, a1, v1);                             // daddu a1, a1, v1
  c->lbu(a1, 0, a1);                                // lbu a1, 0(a1)
  c->and_(a0, a1, a0);                              // and a0, a1, a0
  c->lwu(a1, 40, sp);                               // lwu a1, 40(sp)
  c->daddu(v1, a1, v1);                             // daddu v1, a1, v1
  c->sb(a0, 0, v1);                                 // sb a0, 0(v1)
  
block_5:
  c->sd(r0, 64, sp);                                // sd r0, 64(sp)
  c->daddiu(v1, sp, 48);                            // daddiu v1, sp, 48
  c->sw(v1, 72, sp);                                // sw v1, 72(sp)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 76, sp);                              // swc1 f0, 76(sp)
  c->lwu(v1, 20, sp);                               // lwu v1, 20(sp)
  c->lqc2(vf1, 0, v1);                              // lqc2 vf1, 0(v1)
  c->vadd_bc(DEST::x, BC::w, vf2, vf0, vf0);        // vaddw.x vf2, vf0, vf0
  c->vmul(DEST::xyzw, vf1, vf1, vf1);               // vmul.xyzw vf1, vf1, vf1
  c->vmula_bc(DEST::x, BC::x, vf2, vf1);            // vmulax.x acc, vf2, vf1
  c->vmadda_bc(DEST::x, BC::y, vf2, vf1);           // vmadday.x acc, vf2, vf1
  c->vmadd_bc(DEST::x, BC::z, vf1, vf2, vf1);       // vmaddz.x vf1, vf2, vf1
  c->mov128_gpr_vf(v1, vf1);                        // qmfc2.i v1, vf1
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  cop1_bc = c->fprs[f1] < c->fprs[f0];              // c.lt.s f1, f0
  bc = !cop1_bc;                                    // bc1f L18
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_7;}                           // branch non-likely

  c->lui(v1, 16256);                                // lui v1, 16256
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->divs(f0, f1, f0);                              // div.s f0, f1, f0
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->swc1(f0, 76, sp);                              // swc1 f0, 76(sp)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  
block_7:
  c->lh(s5, 10, gp);                                // lh s5, 10(gp)
  c->lwu(s4, 40, sp);                               // lwu s4, 40(sp)
  c->gprs[s3].du64[0] = 0;                          // or s3, r0, r0
  // nop                                            // sll r0, r0, 0
  
block_8:
  c->dsll(s2, s3, 3);                               // dsll s2, s3, 3
  c->lbu(s1, 0, s4);                                // lbu s1, 0(s4)
  bc = c->sgpr64(s1) == 0;                          // beq s1, r0, L22
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_14;}                          // branch non-likely

  
block_9:
  c->andi(v1, s1, 1);                               // andi v1, s1, 1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L21
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_13;}                          // branch non-likely

  c->lwu(v1, 84, gp);                               // lwu v1, 84(gp)
  c->dsll(a0, s2, 4);                               // dsll a0, s2, 4
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  c->sw(v1, 80, sp);                                // sw v1, 80(sp)
  c->daddiu(v1, sp, 96);                            // daddiu v1, sp, 96
  c->mov64(a2, v1);                                 // or a2, v1, r0
  c->lwu(a0, 80, sp);                               // lwu a0, 80(sp)
  c->lwu(a1, 16, sp);                               // lwu a1, 16(sp)
  c->lqc2(vf4, 0, a0);                              // lqc2 vf4, 0(a0)
  c->lqc2(vf5, 0, a1);                              // lqc2 vf5, 0(a1)
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, a2);                              // sqc2 vf6, 0(a2)
  c->lwu(a0, 20, sp);                               // lwu a0, 20(sp)
  c->lwc1(f0, 0, a0);                               // lwc1 f0, 0(a0)
  c->lwc1(f1, 4, a0);                               // lwc1 f1, 4(a0)
  c->lwc1(f2, 8, a0);                               // lwc1 f2, 8(a0)
  c->lwc1(f3, 0, v1);                               // lwc1 f3, 0(v1)
  c->lwc1(f4, 4, v1);                               // lwc1 f4, 4(v1)
  c->lwc1(f5, 8, v1);                               // lwc1 f5, 8(v1)
  // Unknown instr: mula.s f0, f3
  // Unknown instr: madda.s f1, f4
  // Unknown instr: madd.s f0, f2, f5
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lwc1(f1, 76, sp);                              // lwc1 f1, 76(sp)
  c->muls(f1, f0, f1);                              // mul.s f1, f0, f1
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->lui(v1, 16256);                                // lui v1, 16256
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  c->mins(f1, f2, f1);                              // min.s f1, f2, f1
  c->maxs(f0, f0, f1);                              // max.s f0, f0, f1
  c->lwu(v1, 72, sp);                               // lwu v1, 72(sp)
  c->lwu(a0, 16, sp);                               // lwu a0, 16(sp)
  c->lwu(a1, 20, sp);                               // lwu a1, 20(sp)
  c->lqc2(vf2, 0, a1);                              // lqc2 vf2, 0(a1)
  c->lqc2(vf1, 0, a0);                              // lqc2 vf1, 0(a0)
  c->mfc1(a0, f0);                                  // mfc1 a0, f0
  c->mov128_vf_gpr(vf3, a0);                        // qmtc2.i vf3, a0
  c->vadd_bc(DEST::w, BC::x, vf4, vf0, vf0);        // vaddx.w vf4, vf0, vf0
  c->vmula_bc(DEST::xyzw, BC::x, vf2, vf3);         // vmulax.xyzw acc, vf2, vf3
  c->vmadd_bc(DEST::xyz, BC::w, vf4, vf1, vf0);     // vmaddw.xyz vf4, vf1, vf0
  c->sqc2(vf4, 0, v1);                              // sqc2 vf4, 0(v1)
  c->load_symbol2(t9, cache.vector_vector_distance_squared);// lw t9, vector-vector-distance-squared(s7)
  c->lwu(a0, 72, sp);                               // lwu a0, 72(sp)
  c->lwu(a1, 80, sp);                               // lwu a1, 80(sp)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mtc1(f0, v0);                                  // mtc1 f0, v0
  c->lwc1(f1, 24, sp);                              // lwc1 f1, 24(sp)
  c->lwu(v1, 80, sp);                               // lwu v1, 80(sp)
  c->lwc1(f2, 12, v1);                              // lwc1 f2, 12(v1)
  c->adds(f1, f1, f2);                              // add.s f1, f1, f2
  c->muls(f1, f1, f1);                              // mul.s f1, f1, f1
  c->mfc1(v1, f1);                                  // mfc1 v1, f1
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = !cop1_bc;                                    // bc1f L21
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_13;}                          // branch non-likely

  c->ld(v1, 64, sp);                                // ld v1, 64(sp)
  c->ld(a0, 32, sp);                                // ld a0, 32(sp)
  c->slt(v1, v1, a0);                               // slt v1, v1, a0
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L21
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_13;}                          // branch non-likely

  c->dsll(v1, s2, 4);                               // dsll v1, s2, 4
  c->lwu(a0, 100, gp);                              // lwu a0, 100(gp)
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  c->lwu(v1, 0, v1);                                // lwu v1, 0(v1)
  c->lwu(a0, 28, sp);                               // lwu a0, 28(sp)
  c->ld(a1, 64, sp);                                // ld a1, 64(sp)
  c->dsll(a1, a1, 2);                               // dsll a1, a1, 2
  c->daddu(a0, a0, a1);                             // daddu a0, a0, a1
  c->sw(v1, 0, a0);                                 // sw v1, 0(a0)
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->ld(v1, 64, sp);                                // ld v1, 64(sp)
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  c->sd(v1, 64, sp);                                // sd v1, 64(sp)
  // nop                                            // sll r0, r0, 0
  
block_13:
  c->dsra(s1, s1, 1);                               // dsra s1, s1, 1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s1) != 0;                          // bne s1, r0, L20
  c->daddiu(s2, s2, 1);                             // daddiu s2, s2, 1
  if (bc) {goto block_9;}                           // branch non-likely

  
block_14:
  c->daddiu(s3, s3, 1);                             // daddiu s3, s3, 1
  c->daddiu(s4, s4, 1);                             // daddiu s4, s4, 1
  c->slt(v1, s3, s5);                               // slt v1, s3, s5
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L19
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_8;}                           // branch non-likely

  c->ld(v0, 64, sp);                                // ld v0, 64(sp)
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 192, sp);                               // lq gp, 192(sp)
  c->lq(s5, 176, sp);                               // lq s5, 176(sp)
  c->lq(s4, 160, sp);                               // lq s4, 160(sp)
  c->lq(s3, 144, sp);                               // lq s3, 144(sp)
  c->lq(s2, 128, sp);                               // lq s2, 128(sp)
  c->lq(s1, 112, sp);                               // lq s1, 112(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 208);                           // daddiu sp, sp, 208
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.vector_vector_distance_squared = intern_from_c(-1, 0, "vector-vector-distance-squared").c();
  gLinkedFunctionTable.reg("(method 34 spatial-hash)", execute, 512);
}

} // namespace method_34_spatial_hash
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace method_38_spatial_hash {
struct Cache {
  void* perf_stats; // *perf-stats*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -112);                          // daddiu sp, sp, -112
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s3, 48, sp);                                // sq s3, 48(sp)
  c->sq(s4, 64, sp);                                // sq s4, 64(sp)
  c->sq(s5, 80, sp);                                // sq s5, 80(sp)
  c->sq(gp, 96, sp);                                // sq gp, 96(sp)
  c->mov64(s5, a0);                                 // or s5, a0, r0
  c->mov64(gp, a1);                                 // or gp, a1, r0
  c->mov64(s4, a2);                                 // or s4, a2, r0
  c->mov64(s3, a3);                                 // or s3, a3, r0
  c->load_symbol2(v1, cache.perf_stats);            // lw v1, *perf-stats*(s7)
  c->daddiu(v1, v1, 116);                           // daddiu v1, v1, 116
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 112, v1);                              // lwu t9, 112(v1)
  c->daddiu(a1, s5, 4);                             // daddiu a1, s5, 4
  c->mov64(a2, gp);                                 // or a2, gp, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->lwu(v1, 0, s5);                                // lwu v1, 0(s5)
  c->daddiu(v1, v1, 12);                            // daddiu v1, v1, 12
  c->sw(v1, 16, sp);                                // sw v1, 16(sp)
  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 104, v1);                              // lwu t9, 104(v1)
  c->daddiu(a1, s5, 4);                             // daddiu a1, s5, 4
  c->lwu(a2, 16, sp);                               // lwu a2, 16(sp)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->sd(r0, 24, sp);                                // sd r0, 24(sp)
  c->sw(s5, 32, sp);                                // sw s5, 32(sp)
  c->sw(s4, 36, sp);                                // sw s4, 36(sp)
  c->sd(s3, 40, sp);                                // sd s3, 40(sp)
  c->lwu(v1, 32, sp);                               // lwu v1, 32(sp)
  c->lh(v1, 10, v1);                                // lh v1, 10(v1)
  c->lwu(a0, 16, sp);                               // lwu a0, 16(sp)
  c->gprs[a1].du64[0] = 0;                          // or a1, r0, r0
  // nop                                            // sll r0, r0, 0
  
block_1:
  c->dsll(a2, a1, 3);                               // dsll a2, a1, 3
  c->lbu(a3, 0, a0);                                // lbu a3, 0(a0)
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L27
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  
block_2:
  c->andi(t0, a3, 1);                               // andi t0, a3, 1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L26
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_6;}                           // branch non-likely

  c->lwu(t0, 32, sp);                               // lwu t0, 32(sp)
  c->lwu(t0, 84, t0);                               // lwu t0, 84(t0)
  c->dsll(t1, a2, 4);                               // dsll t1, a2, 4
  c->daddu(t0, t0, t1);                             // daddu t0, t0, t1
  c->mov64(t1, gp);                                 // or t1, gp, r0
  c->mov64(t2, t0);                                 // or t2, t0, r0
  c->lqc2(vf2, 0, t1);                              // lqc2 vf2, 0(t1)
  c->lqc2(vf3, 0, t2);                              // lqc2 vf3, 0(t2)
  c->vsub(DEST::xyzw, vf1, vf3, vf2);               // vsub.xyzw vf1, vf3, vf2
  c->vmul(DEST::xyzw, vf1, vf1, vf1);               // vmul.xyzw vf1, vf1, vf1
  c->vadd_bc(DEST::x, BC::y, vf1, vf1, vf1);        // vaddy.x vf1, vf1, vf1
  c->vadd_bc(DEST::x, BC::z, vf1, vf1, vf1);        // vaddz.x vf1, vf1, vf1
  c->mov128_gpr_vf(t1, vf1);                        // qmfc2.i t1, vf1
  c->mtc1(f0, t1);                                  // mtc1 f0, t1
  c->lwc1(f1, 12, gp);                              // lwc1 f1, 12(gp)
  c->lwc1(f2, 12, t0);                              // lwc1 f2, 12(t0)
  c->adds(f1, f1, f2);                              // add.s f1, f1, f2
  c->muls(f1, f1, f1);                              // mul.s f1, f1, f1
  c->mfc1(t0, f1);                                  // mfc1 t0, f1
  c->mtc1(f1, t0);                                  // mtc1 f1, t0
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = !cop1_bc;                                    // bc1f L26
  c->mov64(t0, s7);                                 // or t0, s7, r0
  if (bc) {goto block_6;}                           // branch non-likely

  c->ld(t0, 24, sp);                                // ld t0, 24(sp)
  c->ld(t1, 40, sp);                                // ld t1, 40(sp)
  c->slt(t0, t0, t1);                               // slt t0, t0, t1
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L26
  c->mov64(t0, s7);                                 // or t0, s7, r0
  if (bc) {goto block_6;}                           // branch non-likely

  c->dsll(t0, a2, 4);                               // dsll t0, a2, 4
  c->lwu(t1, 32, sp);                               // lwu t1, 32(sp)
  c->lwu(t1, 100, t1);                              // lwu t1, 100(t1)
  c->daddu(t0, t0, t1);                             // daddu t0, t0, t1
  c->lwu(t0, 0, t0);                                // lwu t0, 0(t0)
  c->lwu(t1, 36, sp);                               // lwu t1, 36(sp)
  c->ld(t2, 24, sp);                                // ld t2, 24(sp)
  c->dsll(t2, t2, 2);                               // dsll t2, t2, 2
  c->daddu(t1, t1, t2);                             // daddu t1, t1, t2
  c->sw(t0, 0, t1);                                 // sw t0, 0(t1)
  c->ld(t0, 24, sp);                                // ld t0, 24(sp)
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  c->sd(t0, 24, sp);                                // sd t0, 24(sp)
  // nop                                            // sll r0, r0, 0
  
block_6:
  c->dsra(a3, a3, 1);                               // dsra a3, a3, 1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L25
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  if (bc) {goto block_2;}                           // branch non-likely

  
block_7:
  c->daddiu(a1, a1, 1);                             // daddiu a1, a1, 1
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  c->slt(a2, a1, v1);                               // slt a2, a1, v1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L24
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->load_symbol2(v1, cache.perf_stats);            // lw v1, *perf-stats*(s7)
  c->daddiu(v1, v1, 116);                           // daddiu v1, v1, 116
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->ld(v0, 24, sp);                                // ld v0, 24(sp)
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 96, sp);                                // lq gp, 96(sp)
  c->lq(s5, 80, sp);                                // lq s5, 80(sp)
  c->lq(s4, 64, sp);                                // lq s4, 64(sp)
  c->lq(s3, 48, sp);                                // lq s3, 48(sp)
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
  cache.perf_stats = intern_from_c(-1, 0, "*perf-stats*").c();
  gLinkedFunctionTable.reg("(method 38 spatial-hash)", execute, 512);
}

} // namespace method_38_spatial_hash
} // namespace Mips2C

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
namespace method_32_sphere_hash {
struct Cache {
  void* perf_stats; // *perf-stats*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -96);                           // daddiu sp, sp, -96
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s3, 32, sp);                                // sq s3, 32(sp)
  c->sq(s4, 48, sp);                                // sq s4, 48(sp)
  c->sq(s5, 64, sp);                                // sq s5, 64(sp)
  c->sq(gp, 80, sp);                                // sq gp, 80(sp)
  c->mov64(s4, a0);                                 // or s4, a0, r0
  c->mov64(gp, a1);                                 // or gp, a1, r0
  c->mov64(s5, a2);                                 // or s5, a2, r0
  c->mov64(s3, a3);                                 // or s3, a3, r0
  c->load_symbol2(v1, cache.perf_stats);            // lw v1, *perf-stats*(s7)
  c->daddiu(v1, v1, 116);                           // daddiu v1, v1, 116
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->mov64(a0, s4);                                 // or a0, s4, r0
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 112, v1);                              // lwu t9, 112(v1)
  c->daddiu(a1, s4, 4);                             // daddiu a1, s4, 4
  c->mov64(a2, gp);                                 // or a2, gp, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->lwu(v1, 0, s4);                                // lwu v1, 0(s4)
  c->daddiu(v1, v1, 12);                            // daddiu v1, v1, 12
  c->sw(v1, 16, sp);                                // sw v1, 16(sp)
  c->mov64(a0, s4);                                 // or a0, s4, r0
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 104, v1);                              // lwu t9, 104(v1)
  c->daddiu(a1, s4, 4);                             // daddiu a1, s4, 4
  c->lwu(a2, 16, sp);                               // lwu a2, 16(sp)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->addiu(v1, r0, -1);                             // addiu v1, r0, -1
  bc = c->sgpr64(s3) == c->sgpr64(v1);              // beq s3, v1, L69
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_5;}                           // branch non-likely

  c->dsra(v1, s3, 3);                               // dsra v1, s3, 3
  c->andi(a1, s3, 7);                               // andi a1, s3, 7
  c->addiu(a0, r0, 1);                              // addiu a0, r0, 1
  if (((s64)c->sgpr64(a1)) >= 0) {                  // bgezl a1, L68
    c->dsllv(a0, a0, a1);                           // dsllv a0, a0, a1
    goto block_4;
  }
  
block_3:
  c->dsubu(a1, r0, a1);                             // dsubu a1, r0, a1
  c->dsrav(a0, a0, a1);                             // dsrav a0, a0, a1
  
block_4:
  c->nor(a0, a0, r0);                               // nor a0, a0, r0
  c->lwu(a1, 16, sp);                               // lwu a1, 16(sp)
  c->daddu(a1, a1, v1);                             // daddu a1, a1, v1
  c->lbu(a1, 0, a1);                                // lbu a1, 0(a1)
  c->and_(a0, a1, a0);                              // and a0, a1, a0
  c->lwu(a1, 16, sp);                               // lwu a1, 16(sp)
  c->daddu(v1, a1, v1);                             // daddu v1, a1, v1
  c->sb(a0, 0, v1);                                 // sb a0, 0(v1)
  
block_5:
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  c->sw(v1, 20, sp);                                // sw v1, 20(sp)
  c->sw(s4, 24, sp);                                // sw s4, 24(sp)
  c->lwu(v1, 24, sp);                               // lwu v1, 24(sp)
  c->lh(v1, 10, v1);                                // lh v1, 10(v1)
  c->lwu(a0, 16, sp);                               // lwu a0, 16(sp)
  c->gprs[a1].du64[0] = 0;                          // or a1, r0, r0
  
block_6:
  c->dsll(a2, a1, 3);                               // dsll a2, a1, 3
  c->lbu(a3, 0, a0);                                // lbu a3, 0(a0)
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L75
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_13;}                          // branch non-likely

  
block_7:
  c->andi(t0, a3, 1);                               // andi t0, a3, 1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L74
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely

  c->lwu(t0, 24, sp);                               // lwu t0, 24(sp)
  c->lwu(t0, 84, t0);                               // lwu t0, 84(t0)
  c->dsll(t1, a2, 4);                               // dsll t1, a2, 4
  c->daddu(t0, t0, t1);                             // daddu t0, t0, t1
  c->sw(t0, 28, sp);                                // sw t0, 28(sp)
  c->lwu(t0, 28, sp);                               // lwu t0, 28(sp)
  c->lwc1(f0, 12, t0);                              // lwc1 f0, 12(t0)
  c->mfc1(t0, f0);                                  // mfc1 t0, f0
  c->and_(t0, s5, t0);                              // and t0, s5, t0
  bc = c->sgpr64(t0) != 0;                          // bne t0, r0, L72
  c->mov64(t0, s7);                                 // or t0, s7, r0
  if (bc) {goto block_10;}                          // branch non-likely

  //beq r0, r0, L73                                 // beq r0, r0, L73
  // nop                                            // sll r0, r0, 0
  goto block_11;                                    // branch always

  
block_10:
  c->lwc1(f0, 0, gp);                               // lwc1 f0, 0(gp)
  c->lwu(t0, 28, sp);                               // lwu t0, 28(sp)
  c->lwc1(f1, 0, t0);                               // lwc1 f1, 0(t0)
  c->lwc1(f2, 4, gp);                               // lwc1 f2, 4(gp)
  c->lwu(t0, 28, sp);                               // lwu t0, 28(sp)
  c->lwc1(f2, 4, t0);                               // lwc1 f2, 4(t0)
  c->lwc1(f2, 8, gp);                               // lwc1 f2, 8(gp)
  c->lwu(t0, 28, sp);                               // lwu t0, 28(sp)
  c->lwc1(f3, 8, t0);                               // lwc1 f3, 8(t0)
  c->subs(f0, f1, f0);                              // sub.s f0, f1, f0
  c->subs(f1, f3, f2);                              // sub.s f1, f3, f2
  c->subs(f2, f3, f2);                              // sub.s f2, f3, f2
  // Unknown instr: mula.s f0, f0
  // Unknown instr: madda.s f1, f1
  // Unknown instr: madd.s f0, f2, f2
  c->lwu(t0, 28, sp);                               // lwu t0, 28(sp)
  c->lwc1(f1, 12, t0);                              // lwc1 f1, 12(t0)
  c->lwc1(f2, 12, gp);                              // lwc1 f2, 12(gp)
  c->adds(f1, f1, f2);                              // add.s f1, f1, f2
  c->muls(f1, f1, f1);                              // mul.s f1, f1, f1
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = cop1_bc;                                     // bc1t L76
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_15;}                          // branch non-likely

  
block_11:
  c->gprs[t0].du64[0] = 0;                          // or t0, r0, r0
  // nop                                            // sll r0, r0, 0
  
block_12:
  c->dsra(a3, a3, 1);                               // dsra a3, a3, 1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L71
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  if (bc) {goto block_7;}                           // branch non-likely

  
block_13:
  c->daddiu(a1, a1, 1);                             // daddiu a1, a1, 1
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  c->slt(a2, a1, v1);                               // slt a2, a1, v1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L70
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_6;}                           // branch non-likely

  c->sw(s7, 20, sp);                                // sw s7, 20(sp)
  
block_15:
  c->load_symbol2(v1, cache.perf_stats);            // lw v1, *perf-stats*(s7)
  c->daddiu(v1, v1, 116);                           // daddiu v1, v1, 116
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->lwu(v0, 20, sp);                               // lwu v0, 20(sp)
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 80, sp);                                // lq gp, 80(sp)
  c->lq(s5, 64, sp);                                // lq s5, 64(sp)
  c->lq(s4, 48, sp);                                // lq s4, 48(sp)
  c->lq(s3, 32, sp);                                // lq s3, 32(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 96);                            // daddiu sp, sp, 96
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.perf_stats = intern_from_c(-1, 0, "*perf-stats*").c();
  gLinkedFunctionTable.reg("(method 32 sphere-hash)", execute, 256);
}

} // namespace method_32_sphere_hash
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace method_31_sphere_hash {
struct Cache {
  void* perf_stats; // *perf-stats*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -64);                           // daddiu sp, sp, -64
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s4, 16, sp);                                // sq s4, 16(sp)
  c->sq(s5, 32, sp);                                // sq s5, 32(sp)
  c->sq(gp, 48, sp);                                // sq gp, 48(sp)
  c->mov64(s5, a0);                                 // or s5, a0, r0
  c->mov64(gp, a1);                                 // or gp, a1, r0
  c->load_symbol2(v1, cache.perf_stats);            // lw v1, *perf-stats*(s7)
  c->daddiu(v1, v1, 116);                           // daddiu v1, v1, 116
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 112, v1);                              // lwu t9, 112(v1)
  c->daddiu(a1, s5, 4);                             // daddiu a1, s5, 4
  c->daddu(a2, r0, gp);                             // daddu a2, r0, gp
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->lwu(v1, 0, s5);                                // lwu v1, 0(s5)
  c->daddiu(s4, v1, 12);                            // daddiu s4, v1, 12
  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 104, v1);                              // lwu t9, 104(v1)
  c->daddiu(a1, s5, 4);                             // daddiu a1, s5, 4
  c->mov64(a2, s4);                                 // or a2, s4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->sh(r0, 20, gp);                                // sh r0, 20(gp)
  c->lh(v1, 10, s5);                                // lh v1, 10(s5)
  c->gprs[a0].du64[0] = 0;                          // or a0, r0, r0
  
block_1:
  c->dsll(a1, a0, 3);                               // dsll a1, a0, 3
  c->lbu(a2, 0, s4);                                // lbu a2, 0(s4)
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L82
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_11;}                          // branch non-likely

  
block_2:
  c->andi(a3, a2, 1);                               // andi a3, a2, 1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L81
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_10;}                          // branch non-likely

  c->lwu(a3, 84, s5);                               // lwu a3, 84(s5)
  c->dsll(t0, a1, 4);                               // dsll t0, a1, 4
  c->daddu(a3, a3, t0);                             // daddu a3, a3, t0
  c->lbu(t0, 24, gp);                               // lbu t0, 24(gp)
  c->lwc1(f0, 12, a3);                              // lwc1 f0, 12(a3)
  c->mfc1(t1, f0);                                  // mfc1 t1, f0
  c->and_(t0, t0, t1);                              // and t0, t0, t1
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L80
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_9;}                           // branch non-likely

  c->mov64(t0, s7);                                 // or t0, s7, r0
  c->lwc1(f3, 0, gp);                               // lwc1 f3, 0(gp)
  c->lwc1(f1, 8, gp);                               // lwc1 f1, 8(gp)
  c->lwc1(f4, 0, a3);                               // lwc1 f4, 0(a3)
  c->lwc1(f2, 8, a3);                               // lwc1 f2, 8(a3)
  c->lwc1(f0, 12, gp);                              // lwc1 f0, 12(gp)
  c->subs(f3, f4, f3);                              // sub.s f3, f4, f3
  c->subs(f1, f2, f1);                              // sub.s f1, f2, f1
  c->mtc1(f2, r0);                                  // mtc1 f2, r0
  // Unknown instr: mula.s f3, f3
  // Unknown instr: madd.s f1, f1, f1
  cop1_bc = c->fprs[f2] < c->fprs[f1];              // c.lt.s f2, f1
  bc = !cop1_bc;                                    // bc1f L80
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_9;}                           // branch non-likely

  c->lwc1(f2, 4, gp);                               // lwc1 f2, 4(gp)
  c->lwc1(f3, 4, a3);                               // lwc1 f3, 4(a3)
  c->subs(f3, f3, f2);                              // sub.s f3, f3, f2
  c->lwc1(f2, 16, gp);                              // lwc1 f2, 16(gp)
  c->abss(f3, f3);                                  // abs.s f3, f3
  cop1_bc = c->fprs[f3] < c->fprs[f2];              // c.lt.s f3, f2
  bc = !cop1_bc;                                    // bc1f L80
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_9;}                           // branch non-likely

  c->lwc1(f2, 12, a3);                              // lwc1 f2, 12(a3)
  c->adds(f0, f2, f0);                              // add.s f0, f2, f0
  c->muls(f0, f0, f0);                              // mul.s f0, f0, f0
  cop1_bc = c->fprs[f1] < c->fprs[f0];              // c.lt.s f1, f0
  bc = !cop1_bc;                                    // bc1f L80
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_9;}                           // branch non-likely

  c->lh(a3, 20, gp);                                // lh a3, 20(gp)
  c->lh(t0, 22, gp);                                // lh t0, 22(gp)
  c->slt(a3, a3, t0);                               // slt a3, a3, t0
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L80
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_9;}                           // branch non-likely

  c->mov64(a3, s7);                                 // or a3, s7, r0
  c->lwu(a3, 28, gp);                               // lwu a3, 28(gp)
  c->lh(t0, 20, gp);                                // lh t0, 20(gp)
  c->daddu(a3, a3, t0);                             // daddu a3, a3, t0
  c->sb(a1, 0, a3);                                 // sb a1, 0(a3)
  c->lh(a3, 20, gp);                                // lh a3, 20(gp)
  c->daddiu(a3, a3, 1);                             // daddiu a3, a3, 1
  c->sh(a3, 20, gp);                                // sh a3, 20(gp)
  
block_9:
  c->gprs[a3].du64[0] = 0;                          // or a3, r0, r0
  // nop                                            // sll r0, r0, 0
  
block_10:
  c->dsra(a2, a2, 1);                               // dsra a2, a2, 1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L79
  c->daddiu(a1, a1, 1);                             // daddiu a1, a1, 1
  if (bc) {goto block_2;}                           // branch non-likely

  
block_11:
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  c->daddiu(s4, s4, 1);                             // daddiu s4, s4, 1
  c->slt(a1, a0, v1);                               // slt a1, a0, v1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a1) != 0;                          // bne a1, r0, L78
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->load_symbol2(v1, cache.perf_stats);            // lw v1, *perf-stats*(s7)
  c->daddiu(v1, v1, 116);                           // daddiu v1, v1, 116
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
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
  return c->gprs[v0].du64[0];
}

void link() {
  cache.perf_stats = intern_from_c(-1, 0, "*perf-stats*").c();
  gLinkedFunctionTable.reg("(method 31 sphere-hash)", execute, 256);
}

} // namespace method_31_sphere_hash
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace method_29_sphere_hash {
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
  c->mov64(s3, a1);                                 // or s3, a1, r0
  c->mov64(s4, a2);                                 // or s4, a2, r0
  c->lh(gp, 56, s5);                                // lh gp, 56(s5)
  c->lh(v1, 88, s5);                                // lh v1, 88(s5)
  c->slt(v1, gp, v1);                               // slt v1, gp, v1
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L91
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_5;}                           // branch non-likely

  c->lwu(v1, 84, s5);                               // lwu v1, 84(s5)
  c->dsll(a0, gp, 4);                               // dsll a0, gp, 4
  c->daddu(s2, v1, a0);                             // daddu s2, v1, a0
  c->load_symbol2(t9, cache.mem_copy);              // lw t9, mem-copy!(s7)
  c->mov64(a0, s2);                                 // or a0, s2, r0
  c->mov64(a1, s3);                                 // or a1, s3, r0
  c->addiu(a2, r0, 16);                             // addiu a2, r0, 16
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->addiu(v1, r0, 0);                              // addiu v1, r0, 0
  //beq r0, r0, L90                                 // beq r0, r0, L90
  // nop                                            // sll r0, r0, 0
  goto block_3;                                     // branch always

  
block_2:
  c->dsll(a0, v1, 2);                               // dsll a0, v1, 2
  c->daddu(a0, a0, s5);                             // daddu a0, a0, s5
  c->lwc1(f0, 32, a0);                              // lwc1 f0, 32(a0)
  c->dsll(a0, v1, 2);                               // dsll a0, v1, 2
  c->daddu(a0, a0, s3);                             // daddu a0, a0, s3
  c->lwc1(f1, 0, a0);                               // lwc1 f1, 0(a0)
  c->lwc1(f2, 12, s3);                              // lwc1 f2, 12(s3)
  c->subs(f1, f1, f2);                              // sub.s f1, f1, f2
  c->mins(f0, f0, f1);                              // min.s f0, f0, f1
  c->dsll(a0, v1, 2);                               // dsll a0, v1, 2
  c->daddu(a0, a0, s5);                             // daddu a0, a0, s5
  c->swc1(f0, 32, a0);                              // swc1 f0, 32(a0)
  c->dsll(a0, v1, 2);                               // dsll a0, v1, 2
  c->daddu(a0, a0, s5);                             // daddu a0, a0, s5
  c->lwc1(f0, 44, a0);                              // lwc1 f0, 44(a0)
  c->dsll(a0, v1, 2);                               // dsll a0, v1, 2
  c->daddu(a0, a0, s3);                             // daddu a0, a0, s3
  c->lwc1(f1, 0, a0);                               // lwc1 f1, 0(a0)
  c->lwc1(f2, 12, s3);                              // lwc1 f2, 12(s3)
  c->adds(f1, f1, f2);                              // add.s f1, f1, f2
  c->maxs(f0, f0, f1);                              // max.s f0, f0, f1
  c->dsll(a0, v1, 2);                               // dsll a0, v1, 2
  c->daddu(a0, a0, s5);                             // daddu a0, a0, s5
  c->swc1(f0, 44, a0);                              // swc1 f0, 44(a0)
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  
block_3:
  c->slti(a0, v1, 3);                               // slti a0, v1, 3
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L89
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->sb(s4, 12, s2);                                // sb s4, 12(s2)
  c->lh(v1, 56, s5);                                // lh v1, 56(s5)
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  c->sh(v1, 56, s5);                                // sh v1, 56(s5)
  //beq r0, r0, L92                                 // beq r0, r0, L92
  // nop                                            // sll r0, r0, 0
  goto block_6;                                     // branch always

  
block_5:
  c->addiu(gp, r0, -1);                             // addiu gp, r0, -1
  c->mov64(v1, gp);                                 // or v1, gp, r0
  
block_6:
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
  gLinkedFunctionTable.reg("(method 29 sphere-hash)", execute, 256);
}

} // namespace method_29_sphere_hash
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace method_28_sphere_hash {
struct Cache {
  void* mem_copy; // mem-copy!
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -64);                           // daddiu sp, sp, -64
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s4, 16, sp);                                // sq s4, 16(sp)
  c->sq(s5, 32, sp);                                // sq s5, 32(sp)
  c->sq(gp, 48, sp);                                // sq gp, 48(sp)
  c->mov64(s5, a0);                                 // or s5, a0, r0
  c->mov64(s4, a1);                                 // or s4, a1, r0
  c->lh(gp, 56, s5);                                // lh gp, 56(s5)
  c->lh(v1, 88, s5);                                // lh v1, 88(s5)
  c->slt(v1, gp, v1);                               // slt v1, gp, v1
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L96
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_5;}                           // branch non-likely

  c->lwu(v1, 84, s5);                               // lwu v1, 84(s5)
  c->dsll(a0, gp, 4);                               // dsll a0, gp, 4
  c->daddu(a0, v1, a0);                             // daddu a0, v1, a0
  c->load_symbol2(t9, cache.mem_copy);              // lw t9, mem-copy!(s7)
  c->mov64(a1, s4);                                 // or a1, s4, r0
  c->addiu(a2, r0, 16);                             // addiu a2, r0, 16
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->addiu(v1, r0, 0);                              // addiu v1, r0, 0
  //beq r0, r0, L95                                 // beq r0, r0, L95
  // nop                                            // sll r0, r0, 0
  goto block_3;                                     // branch always

  
block_2:
  c->dsll(a0, v1, 2);                               // dsll a0, v1, 2
  c->daddu(a0, a0, s5);                             // daddu a0, a0, s5
  c->lwc1(f0, 32, a0);                              // lwc1 f0, 32(a0)
  c->dsll(a0, v1, 2);                               // dsll a0, v1, 2
  c->daddu(a0, a0, s4);                             // daddu a0, a0, s4
  c->lwc1(f1, 0, a0);                               // lwc1 f1, 0(a0)
  c->lwc1(f2, 12, s4);                              // lwc1 f2, 12(s4)
  c->subs(f1, f1, f2);                              // sub.s f1, f1, f2
  c->mins(f0, f0, f1);                              // min.s f0, f0, f1
  c->dsll(a0, v1, 2);                               // dsll a0, v1, 2
  c->daddu(a0, a0, s5);                             // daddu a0, a0, s5
  c->swc1(f0, 32, a0);                              // swc1 f0, 32(a0)
  c->dsll(a0, v1, 2);                               // dsll a0, v1, 2
  c->daddu(a0, a0, s5);                             // daddu a0, a0, s5
  c->lwc1(f0, 44, a0);                              // lwc1 f0, 44(a0)
  c->dsll(a0, v1, 2);                               // dsll a0, v1, 2
  c->daddu(a0, a0, s4);                             // daddu a0, a0, s4
  c->lwc1(f1, 0, a0);                               // lwc1 f1, 0(a0)
  c->lwc1(f2, 12, s4);                              // lwc1 f2, 12(s4)
  c->adds(f1, f1, f2);                              // add.s f1, f1, f2
  c->maxs(f0, f0, f1);                              // max.s f0, f0, f1
  c->dsll(a0, v1, 2);                               // dsll a0, v1, 2
  c->daddu(a0, a0, s5);                             // daddu a0, a0, s5
  c->swc1(f0, 44, a0);                              // swc1 f0, 44(a0)
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  
block_3:
  c->slti(a0, v1, 3);                               // slti a0, v1, 3
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L94
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->lh(v1, 56, s5);                                // lh v1, 56(s5)
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  c->sh(v1, 56, s5);                                // sh v1, 56(s5)
  //beq r0, r0, L97                                 // beq r0, r0, L97
  // nop                                            // sll r0, r0, 0
  goto block_6;                                     // branch always

  
block_5:
  c->addiu(gp, r0, -1);                             // addiu gp, r0, -1
  c->mov64(v1, gp);                                 // or v1, gp, r0
  
block_6:
  c->mov64(v0, gp);                                 // or v0, gp, r0
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
  return c->gprs[v0].du64[0];
}

void link() {
  cache.mem_copy = intern_from_c(-1, 0, "mem-copy!").c();
  gLinkedFunctionTable.reg("(method 28 sphere-hash)", execute, 256);
}

} // namespace method_28_sphere_hash
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