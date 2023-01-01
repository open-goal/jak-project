//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace nav_state_patch_pointers {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  c->lw(v1, 16, a0);                                // lw v1, 16(a0)
  c->daddu(a2, v1, a1);                             // daddu a2, v1, a1
  c->xor_(a3, v1, s7);                              // xor a3, v1, s7
  c->sltiu(a3, a3, 1);                              // sltiu a3, a3, 1
  c->movz(v1, a2, a3);                              // movz v1, a2, a3
  c->sw(v1, 16, a0);                                // sw v1, 16(a0)
  c->lw(v1, 28, a0);                                // lw v1, 28(a0)
  c->daddu(a2, v1, a1);                             // daddu a2, v1, a1
  c->xor_(a3, v1, s7);                              // xor a3, v1, s7
  c->sltiu(a3, a3, 1);                              // sltiu a3, a3, 1
  c->movz(v1, a2, a3);                              // movz v1, a2, a3
  c->sw(v1, 28, a0);                                // sw v1, 28(a0)
  c->lw(v1, 24, a0);                                // lw v1, 24(a0)
  c->daddu(a2, v1, a1);                             // daddu a2, v1, a1
  c->xor_(a3, v1, s7);                              // xor a3, v1, s7
  c->sltiu(a3, a3, 1);                              // sltiu a3, a3, 1
  c->movz(v1, a2, a3);                              // movz v1, a2, a3
  c->sw(v1, 24, a0);                                // sw v1, 24(a0)
  c->lw(v1, 20, a0);                                // lw v1, 20(a0)
  c->daddu(a1, v1, a1);                             // daddu a1, v1, a1
  c->xor_(a2, v1, s7);                              // xor a2, v1, s7
  c->sltiu(a2, a2, 1);                              // sltiu a2, a2, 1
  c->movz(v1, a1, a2);                              // movz v1, a1, a2
  c->sw(v1, 20, a0);                                // sw v1, 20(a0)
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
  gLinkedFunctionTable.reg("nav-state-patch-pointers", execute, 0);
}

} // namespace nav_state_patch_pointers

namespace method_45_nav_mesh {
struct Cache {
  void* entity_nav_mesh; // entity-nav-mesh
  void* entity_nav_mesh_by_aid; // entity-nav-mesh-by-aid
  void* type; // type?
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
  c->mov64(gp, a1);                                 // or gp, a1, r0
  c->lwu(v1, 12, gp);                               // lwu v1, 12(gp)
  bc = c->sgpr64(s7) != c->sgpr64(v1);              // bne s7, v1, L274
  c->mov64(v0, s7);                                 // or v0, s7, r0
  if (bc) {goto block_12;}                          // branch non-likely

  c->load_symbol2(t9, cache.entity_nav_mesh_by_aid);// lw t9, entity-nav-mesh-by-aid(s7)
  c->lwu(a0, 4, gp);                                // lwu a0, 4(gp)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(s4, v0);                                 // or s4, v0, r0
  c->load_symbol2(t9, cache.type);                  // lw t9, type?(s7)
  c->mov64(a0, s4);                                 // or a0, s4, r0
  c->load_symbol2(a1, cache.entity_nav_mesh);       // lw a1, entity-nav-mesh(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  bc = c->sgpr64(s7) == c->sgpr64(v0);              // beq s7, v0, L269
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_3;}                           // branch non-likely

  c->mov64(v1, s4);                                 // or v1, s4, r0
  
block_3:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L274
  c->mov64(v0, s7);                                 // or v0, s7, r0
  if (bc) {goto block_12;}                          // branch non-likely

  c->lwu(a0, 48, v1);                               // lwu a0, 48(v1)
  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->addiu(a1, r0, 0);                              // addiu a1, r0, 0
  //beq r0, r0, L272                                // beq r0, r0, L272
  // nop                                            // sll r0, r0, 0
  goto block_8;                                     // branch always

  
block_5:
  c->lwu(a2, 64, a0);                               // lwu a2, 64(a0)
  c->dsll(a3, a1, 4);                               // dsll a3, a1, 4
  c->daddu(a2, a2, a3);                             // daddu a2, a2, a3
  c->lwu(a3, 0, gp);                                // lwu a3, 0(gp)
  c->lwu(t0, 0, a2);                                // lwu t0, 0(a2)
  bc = c->sgpr64(t0) != c->sgpr64(a3);              // bne t0, a3, L271
  c->mov64(a3, s7);                                 // or a3, s7, r0
  if (bc) {goto block_7;}                           // branch non-likely

  c->mov64(v1, a2);                                 // or v1, a2, r0
  //beq r0, r0, L273                                // beq r0, r0, L273
  // nop                                            // sll r0, r0, 0
  goto block_10;                                    // branch always

  
block_7:
  c->daddiu(a1, a1, 1);                             // daddiu a1, a1, 1
  
block_8:
  c->lbu(a2, 68, a0);                               // lbu a2, 68(a0)
  c->slt(a2, a1, a2);                               // slt a2, a1, a2
  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L270
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_5;}                           // branch non-likely

  c->mov64(a1, s7);                                 // or a1, s7, r0
  c->mov64(a1, s7);                                 // or a1, s7, r0
  
block_10:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L274
  c->mov64(v0, s7);                                 // or v0, s7, r0
  if (bc) {goto block_12;}                          // branch non-likely

  c->sw(a0, 12, gp);                                // sw a0, 12(gp)
  c->sw(s5, 12, v1);                                // sw s5, 12(v1)
  c->lbu(a0, 9, v1);                                // lbu a0, 9(v1)
  c->sb(a0, 10, gp);                                // sb a0, 10(gp)
  c->lbu(a0, 8, v1);                                // lbu a0, 8(v1)
  c->sb(a0, 11, gp);                                // sb a0, 11(gp)
  c->lbu(a0, 9, gp);                                // lbu a0, 9(gp)
  c->sb(a0, 10, v1);                                // sb a0, 10(v1)
  c->lbu(a0, 8, gp);                                // lbu a0, 8(gp)
  c->sb(a0, 11, v1);                                // sb a0, 11(v1)
  c->daddiu(v0, s7, 4);                             // daddiu v0, s7, #t
  
block_12:
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
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
  cache.entity_nav_mesh = intern_from_c("entity-nav-mesh").c();
  cache.entity_nav_mesh_by_aid = intern_from_c("entity-nav-mesh-by-aid").c();
  cache.type = intern_from_c("type?").c();
  gLinkedFunctionTable.reg("(method 45 nav-mesh)", execute, 64);
}

} // namespace method_45_nav_mesh

namespace method_20_nav_engine {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->lwu(v1, 8, a0);                                // lwu v1, 8(a0)
  c->lwu(v1, 4, v1);                                // lwu v1, 4(v1)
  c->lwu(a2, 80, a0);                               // lwu a2, 80(a0)
  c->lwu(a2, 4, a2);                                // lwu a2, 4(a2)
  c->dsubu(v1, v1, a2);                             // dsubu v1, v1, a2
  c->addiu(a2, r0, 0);                              // addiu a2, r0, 0
  //beq r0, r0, L178                                // beq r0, r0, L178
  // nop                                            // sll r0, r0, 0
  goto block_6;                                     // branch always

  
block_1:
  c->lwu(a3, 4, a1);                                // lwu a3, 4(a1)
  c->addiu(t0, r0, 288);                            // addiu t0, r0, 288
  c->mult3(t0, t0, a2);                             // mult3 t0, t0, a2
  c->daddu(a3, a3, t0);                             // daddu a3, a3, t0
  c->daddiu(t0, a3, 112);                           // daddiu t0, a3, 112
  c->lwu(t1, 8, a3);                                // lwu t1, 8(a3)
  bc = c->sgpr64(s7) == c->sgpr64(t1);              // beq s7, t1, L177
  c->mov64(t1, s7);                                 // or t1, s7, r0
  if (bc) {goto block_5;}                           // branch non-likely

  c->addiu(t1, r0, -257);                           // addiu t1, r0, -257
  c->lwu(t2, 0, a3);                                // lwu t2, 0(a3)
  c->and_(t1, t1, t2);                              // and t1, t1, t2
  c->sw(t1, 0, a3);                                 // sw t1, 0(a3)
  c->lwu(t1, 8, a3);                                // lwu t1, 8(a3)
  c->lwu(t1, 4, t1);                                // lwu t1, 4(t1)
  c->andi(t1, t1, 2048);                            // andi t1, t1, 2048
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L176
  c->mov64(t1, s7);                                 // or t1, s7, r0
  if (bc) {goto block_4;}                           // branch non-likely

  c->lwu(t1, 0, a3);                                // lwu t1, 0(a3)
  c->ori(t1, t1, 256);                              // ori t1, t1, 256
  c->sw(t1, 0, a3);                                 // sw t1, 0(a3)
  
block_4:
  c->lwu(t1, 4, a0);                                // lwu t1, 4(a0)
  c->daddiu(t1, t1, 32);                            // daddiu t1, t1, 32
  c->sw(t1, 56, a3);                                // sw t1, 56(a3)
  c->lwu(t1, 8, a0);                                // lwu t1, 8(a0)
  c->sw(t1, 124, a3);                               // sw t1, 124(a3)
  c->mov64(t1, v1);                                 // or t1, v1, r0
  c->lw(t2, 16, t0);                                // lw t2, 16(t0)
  c->daddu(t3, t2, t1);                             // daddu t3, t2, t1
  c->xor_(t4, t2, s7);                              // xor t4, t2, s7
  c->slti(t4, t4, 1); // Unknown instr: sltiu t4, t4, 1
  c->movz(t2, t3, t4);                              // movz t2, t3, t4
  c->sw(t2, 16, t0);                                // sw t2, 16(t0)
  c->lw(t2, 28, t0);                                // lw t2, 28(t0)
  c->daddu(t3, t2, t1);                             // daddu t3, t2, t1
  c->xor_(t4, t2, s7);                              // xor t4, t2, s7
  c->slti(t4, t4, 1); // Unknown instr: sltiu t4, t4, 1
  c->movz(t2, t3, t4);                              // movz t2, t3, t4
  c->sw(t2, 28, t0);                                // sw t2, 28(t0)
  c->lw(t2, 24, t0);                                // lw t2, 24(t0)
  c->daddu(t3, t2, t1);                             // daddu t3, t2, t1
  c->xor_(t4, t2, s7);                              // xor t4, t2, s7
  c->slti(t4, t4, 1); // Unknown instr: sltiu t4, t4, 1
  c->movz(t2, t3, t4);                              // movz t2, t3, t4
  c->sw(t2, 24, t0);                                // sw t2, 24(t0)
  c->lw(t2, 20, t0);                                // lw t2, 20(t0)
  c->daddu(t1, t2, t1);                             // daddu t1, t2, t1
  c->xor_(t3, t2, s7);                              // xor t3, t2, s7
  c->slti(t3, t3, 1); // Unknown instr: sltiu t3, t3, 1
  c->movz(t2, t1, t3);                              // movz t2, t1, t3
  c->sw(t2, 20, t0);                                // sw t2, 20(t0)
  c->gprs[t0].du64[0] = 0;                          // or t0, r0, r0
  c->lwu(a3, 8, a3);                                // lwu a3, 8(a3)
  c->sw(s7, 140, a3);                               // sw s7, 140(a3)
  c->mov64(t1, s7);                                 // or t1, s7, r0
  
block_5:
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  
block_6:
  c->lb(a3, 14, a1);                                // lb a3, 14(a1)
  c->slt(a3, a2, a3);                               // slt a3, a2, a3
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L175
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
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
  gLinkedFunctionTable.reg("(method 20 nav-engine)", execute, 0);
}

} // namespace method_20_nav_engine

namespace method_43_nav_mesh {
struct Cache {
  void* point_poly_distance_min; // point-poly-distance-min
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
  c->mov64(s5, a0);                                 // or s5, a0, r0
  c->mov64(gp, a1);                                 // or gp, a1, r0
  c->sw(s7, 16, sp);                                // sw s7, 16(sp)
  c->lwu(a0, 16, s5);                               // lwu a0, 16(s5)
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 68, v1);                               // lwu t9, 68(v1)
  c->daddu(a1, r0, gp);                             // daddu a1, r0, gp
  c->lui(a2, 17984);                                // lui a2, 17984
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->sw(v1, 20, sp);                                // sw v1, 20(sp)
  c->sw(s7, 32, gp);                                // sw s7, 32(gp)
  c->lwu(v1, 16, s5);                               // lwu v1, 16(s5)
  c->lh(s4, 10, v1);                                // lh s4, 10(v1)
  c->lwu(s3, 20, sp);                               // lwu s3, 20(sp)
  c->gprs[s2].du64[0] = 0;                          // or s2, r0, r0
  // nop                                            // sll r0, r0, 0
  
block_1:
  c->dsll(s1, s2, 3);                               // dsll s1, s2, 3
  c->lbu(s0, 0, s3);                                // lbu s0, 0(s3)
  bc = c->sgpr64(s0) == 0;                          // beq s0, r0, L100
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_17;}                          // branch non-likely

  
block_2:
  c->andi(v1, s0, 1);                               // andi v1, s0, 1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L99
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_16;}                          // branch non-likely

  c->lwu(v1, 4, s5);                                // lwu v1, 4(s5)
  c->dsll(a0, s1, 6);                               // dsll a0, s1, 6
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  c->sw(v1, 24, sp);                                // sw v1, 24(sp)
  c->lwu(v1, 24, sp);                               // lwu v1, 24(sp)
  c->lwc1(f0, 4, gp);                               // lwc1 f0, 4(gp)
  c->lwc1(f1, 16, gp);                              // lwc1 f1, 16(gp)
  c->lwc1(f2, 60, v1);                              // lwc1 f2, 60(v1)
  c->adds(f2, f2, f1);                              // add.s f2, f2, f1
  cop1_bc = c->fprs[f2] < c->fprs[f0];              // c.lt.s f2, f0
  bc = !cop1_bc;                                    // bc1f L96
  c->daddiu(a0, s7, 4);                             // daddiu a0, s7, 4
  if (bc) {goto block_5;}                           // branch non-likely

  c->mov64(a0, s7);                                 // or a0, s7, r0
  
block_5:
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(a0))) {// beql s7, a0, L97
    c->mov64(v1, a0);                               // or v1, a0, r0
    goto block_9;
  }
  
  c->lwc1(f2, 44, v1);                              // lwc1 f2, 44(v1)
  c->subs(f1, f2, f1);                              // sub.s f1, f2, f1
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = !cop1_bc;                                    // bc1f L97
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  if (bc) {goto block_9;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  
block_9:
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(v1))) {// beql s7, v1, L98
    c->mov64(v1, v1);                               // or v1, v1, r0
    goto block_12;
  }
  
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  c->lwu(a0, 24, sp);                               // lwu a0, 24(sp)
  c->lbu(a0, 13, a0);                               // lbu a0, 13(a0)
  c->lbu(a1, 20, gp);                               // lbu a1, 20(gp)
  c->and_(a0, a0, a1);                              // and a0, a0, a1
  c->movn(v1, s7, a0);                              // movn v1, s7, a0
  
block_12:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L99
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_16;}                          // branch non-likely

  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 164, v1);                              // lwu t9, 164(v1)
  c->lwu(a1, 24, sp);                               // lwu a1, 24(sp)
  c->daddu(a2, r0, gp);                             // daddu a2, r0, gp
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L99
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_16;}                          // branch non-likely

  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  c->sw(v1, 32, gp);                                // sw v1, 32(gp)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 28, gp);                              // swc1 f0, 28(gp)
  c->lwu(v1, 24, sp);                               // lwu v1, 24(sp)
  c->sw(v1, 16, sp);                                // sw v1, 16(sp)
  //beq r0, r0, L109                                // beq r0, r0, L109
  // nop                                            // sll r0, r0, 0
  goto block_38;                                    // branch always

  // nop                                            // sll r0, r0, 0
  
block_16:
  c->dsra(s0, s0, 1);                               // dsra s0, s0, 1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s0) != 0;                          // bne s0, r0, L95
  c->daddiu(s1, s1, 1);                             // daddiu s1, s1, 1
  if (bc) {goto block_2;}                           // branch non-likely

  
block_17:
  c->daddiu(s2, s2, 1);                             // daddiu s2, s2, 1
  c->daddiu(s3, s3, 1);                             // daddiu s3, s3, 1
  c->slt(v1, s2, s4);                               // slt v1, s2, s4
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L94
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->lui(v1, 31984);                                // lui v1, 31984
  c->ori(v1, v1, 48578);                            // ori v1, v1, 48578
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->swc1(f0, 28, sp);                              // swc1 f0, 28(sp)
  c->sd(r0, 32, sp);                                // sd r0, 32(sp)
  c->sd(r0, 40, sp);                                // sd r0, 40(sp)
  c->lwu(v1, 16, s5);                               // lwu v1, 16(s5)
  c->lh(s4, 10, v1);                                // lh s4, 10(v1)
  c->lwu(s3, 20, sp);                               // lwu s3, 20(sp)
  c->gprs[s2].du64[0] = 0;                          // or s2, r0, r0
  
block_19:
  c->dsll(s1, s2, 3);                               // dsll s1, s2, 3
  c->lbu(s0, 0, s3);                                // lbu s0, 0(s3)
  bc = c->sgpr64(s0) == 0;                          // beq s0, r0, L107
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_34;}                          // branch non-likely

  
block_20:
  c->andi(v1, s0, 1);                               // andi v1, s0, 1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L106
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_33;}                          // branch non-likely

  c->lwu(v1, 4, s5);                                // lwu v1, 4(s5)
  c->dsll(a0, s1, 6);                               // dsll a0, s1, 6
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  c->sw(v1, 48, sp);                                // sw v1, 48(sp)
  c->lwu(v1, 48, sp);                               // lwu v1, 48(sp)
  c->lwc1(f0, 4, gp);                               // lwc1 f0, 4(gp)
  c->lwc1(f1, 16, gp);                              // lwc1 f1, 16(gp)
  c->lwc1(f2, 60, v1);                              // lwc1 f2, 60(v1)
  c->adds(f2, f2, f1);                              // add.s f2, f2, f1
  cop1_bc = c->fprs[f2] < c->fprs[f0];              // c.lt.s f2, f0
  bc = !cop1_bc;                                    // bc1f L103
  c->daddiu(a0, s7, 4);                             // daddiu a0, s7, 4
  if (bc) {goto block_23;}                          // branch non-likely

  c->mov64(a0, s7);                                 // or a0, s7, r0
  
block_23:
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(a0))) {// beql s7, a0, L104
    c->mov64(v1, a0);                               // or v1, a0, r0
    goto block_27;
  }
  
  c->lwc1(f2, 44, v1);                              // lwc1 f2, 44(v1)
  c->subs(f1, f2, f1);                              // sub.s f1, f2, f1
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = !cop1_bc;                                    // bc1f L104
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  if (bc) {goto block_27;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  
block_27:
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(v1))) {// beql s7, v1, L105
    c->mov64(v1, v1);                               // or v1, v1, r0
    goto block_30;
  }
  
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  c->lwu(a0, 48, sp);                               // lwu a0, 48(sp)
  c->lbu(a0, 13, a0);                               // lbu a0, 13(a0)
  c->lbu(a1, 20, gp);                               // lbu a1, 20(gp)
  c->and_(a0, a0, a1);                              // and a0, a0, a1
  c->movn(v1, s7, a0);                              // movn v1, s7, a0
  
block_30:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L106
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_33;}                          // branch non-likely

  c->ld(v1, 40, sp);                                // ld v1, 40(sp)
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  c->sd(v1, 40, sp);                                // sd v1, 40(sp)
  c->load_symbol2(t9, cache.point_poly_distance_min);// lw t9, point-poly-distance-min(s7)
  c->lwu(a0, 0, s5);                                // lwu a0, 0(s5)
  c->daddu(a1, r0, gp);                             // daddu a1, r0, gp
  c->lw(a2, 28, sp);                                // lw a2, 28(sp)
  c->lwu(a3, 48, sp);                               // lwu a3, 48(sp)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mtc1(f0, v0);                                  // mtc1 f0, v0
  c->swc1(f0, 52, sp);                              // swc1 f0, 52(sp)
  c->lw(v1, 52, sp);                                // lw v1, 52(sp)
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lw(v1, 28, sp);                                // lw v1, 28(sp)
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = !cop1_bc;                                    // bc1f L106
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_33;}                          // branch non-likely

  c->lw(v1, 52, sp);                                // lw v1, 52(sp)
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->swc1(f0, 28, sp);                              // swc1 f0, 28(sp)
  c->lwu(v1, 48, sp);                               // lwu v1, 48(sp)
  c->sw(v1, 16, sp);                                // sw v1, 16(sp)
  // nop                                            // sll r0, r0, 0
  
block_33:
  c->dsra(s0, s0, 1);                               // dsra s0, s0, 1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s0) != 0;                          // bne s0, r0, L102
  c->daddiu(s1, s1, 1);                             // daddiu s1, s1, 1
  if (bc) {goto block_20;}                          // branch non-likely

  
block_34:
  c->daddiu(s2, s2, 1);                             // daddiu s2, s2, 1
  c->daddiu(s3, s3, 1);                             // daddiu s3, s3, 1
  c->slt(v1, s2, s4);                               // slt v1, s2, s4
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L101
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_19;}                          // branch non-likely

  c->lwu(v1, 16, sp);                               // lwu v1, 16(sp)
  bc = c->sgpr64(s7) != c->sgpr64(v1);              // bne s7, v1, L108
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_37;}                          // branch non-likely

  c->lwu(v1, 4, s5);                                // lwu v1, 4(s5)
  c->daddu(v1, r0, v1);                             // daddu v1, r0, v1
  c->sw(v1, 16, sp);                                // sw v1, 16(sp)
  
block_37:
  c->lw(v1, 28, sp);                                // lw v1, 28(sp)
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->swc1(f0, 28, gp);                              // swc1 f0, 28(gp)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  
block_38:
  c->lwu(v1, 16, sp);                               // lwu v1, 16(sp)
  c->sw(v1, 24, gp);                                // sw v1, 24(gp)
  c->mov64(v0, gp);                                 // or v0, gp, r0
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
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.point_poly_distance_min = intern_from_c("point-poly-distance-min").c();
  gLinkedFunctionTable.reg("(method 43 nav-mesh)", execute, 176);
}

} // namespace method_43_nav_mesh

namespace nav_dma_send_to_spr_no_flush {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  c->lui(v1, 4096);                                 // lui v1, 4096
  c->ori(v1, v1, 54272);                            // ori v1, v1, 54272
  c->addiu(a3, r0, 0);                              // addiu a3, r0, 0

  /*
block_1:
  c->lw(a3, 0, v1);                                 // lw a3, 0(v1)
  c->andi(a3, a3, 256);                             // andi a3, a3, 256
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L194
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely
  */

  // Unknown instr: sync.l
  c->lui(a3, 4095);                                 // lui a3, 4095
  c->ori(a3, a3, 65535);                            // ori a3, a3, 65535
  c->and_(a1, a3, a1);                              // and a1, a3, a1
  // c->sw(a1, 16, v1);                                // sw a1, 16(v1)
  u32 madr = c->sgpr64(a1);

  c->lui(a1, 4095);                                 // lui a1, 4095
  c->ori(a1, a1, 65535);                            // ori a1, a1, 65535
  c->and_(a0, a1, a0);                              // and a0, a1, a0
  // c->sw(a0, 128, v1);                               // sw a0, 128(v1)
  u32 sadr = c->sgpr64(a0);

  // c->sw(a2, 32, v1);                                // sw a2, 32(v1)
  u32 qwc = c->sgpr64(a2);
  // Unknown instr: sync.l
  c->addiu(a0, r0, 256);                            // addiu a0, r0, 256
  // c->sw(a0, 0, v1);                                 // sw a0, 0(v1)
  spad_to_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
  // Unknown instr: sync.l
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
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("nav-dma-send-to-spr-no-flush", execute, 0);
}

} // namespace nav_dma_send_to_spr_no_flush

namespace nav_dma_send_from_spr_no_flush {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  c->lui(v1, 4096);                                 // lui v1, 4096
  c->ori(v1, v1, 53248);                            // ori v1, v1, 53248
  c->addiu(a3, r0, 0);                              // addiu a3, r0, 0
  /*
block_1:
  c->lw(a3, 0, v1);                                 // lw a3, 0(v1)
  c->andi(a3, a3, 256);                             // andi a3, a3, 256
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L192
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely
  */

  // Unknown instr: sync.l
  c->lui(a3, 4095);                                 // lui a3, 4095
  c->ori(a3, a3, 65535);                            // ori a3, a3, 65535
  c->and_(a0, a3, a0);                              // and a0, a3, a0
  //c->sw(a0, 16, v1);                                // sw a0, 16(v1)
  u32 madr = c->sgpr64(a0);
  c->lui(a0, 4095);                                 // lui a0, 4095
  c->ori(a0, a0, 65535);                            // ori a0, a0, 65535
  c->and_(a0, a0, a1);                              // and a0, a0, a1
  // c->sw(a0, 128, v1);                               // sw a0, 128(v1)
  u32 sadr = c->sgpr64(a0);
  // c->sw(a2, 32, v1);                                // sw a2, 32(v1)
  u32 qwc = c->sgpr64(a2);
  // Unknown instr: sync.l
  c->addiu(a0, r0, 256);                            // addiu a0, r0, 256
  // c->sw(a0, 0, v1);                                 // sw a0, 0(v1)
  spad_from_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
  // Unknown instr: sync.l
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
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("nav-dma-send-from-spr-no-flush", execute, 0);
}

} // namespace nav_dma_send_from_spr_no_flush

namespace method_17_nav_engine {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  c->lwu(v1, 4, a1);                                // lwu v1, 4(a1)
  c->lwu(a2, 0, a1);                                // lwu a2, 0(a1)
  c->lwu(a0, 8, a1);                                // lwu a0, 8(a1)
  c->lui(a1, 4096);                                 // lui a1, 4096
  c->ori(a1, a1, 54272);                            // ori a1, a1, 54272
  c->addiu(a3, r0, 0);                              // addiu a3, r0, 0
  
//block_1:
//  c->lw(a3, 0, a1);                                 // lw a3, 0(a1)
//  c->andi(a3, a3, 256);                             // andi a3, a3, 256
//  // nop                                            // sll r0, r0, 0
//  // nop                                            // sll r0, r0, 0
//  // nop                                            // sll r0, r0, 0
//  // nop                                            // sll r0, r0, 0
//  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L188
//  // nop                                            // sll r0, r0, 0
//  if (bc) {goto block_1;}                           // branch non-likely

  // Unknown instr: sync.l
  c->lui(a3, 4095);                                 // lui a3, 4095
  c->ori(a3, a3, 65535);                            // ori a3, a3, 65535
  c->and_(a2, a3, a2);                              // and a2, a3, a2
  // c->sw(a2, 16, a1);                                // sw a2, 16(a1)
  u32 madr = c->sgpr64(a2);
  c->lui(a2, 4095);                                 // lui a2, 4095
  c->ori(a2, a2, 65535);                            // ori a2, a2, 65535
  c->and_(v1, a2, v1);                              // and v1, a2, v1
  // c->sw(v1, 128, a1);                               // sw v1, 128(a1)
  u32 sadr = c->sgpr64(v1);
  // c->sw(a0, 32, a1);                                // sw a0, 32(a1)
  u32 qwc = c->sgpr64(a0);
  // Unknown instr: sync.l
  c->addiu(v1, r0, 256);                            // addiu v1, r0, 256
  // c->sw(v1, 0, a1);                                 // sw v1, 0(a1)
  spad_to_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
  // Unknown instr: sync.l
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
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
  gLinkedFunctionTable.reg("(method 17 nav-engine)", execute, 0);
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
}

} // namespace method_17_nav_engine

namespace method_18_nav_engine {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  c->lwu(a2, 0, a1);                                // lwu a2, 0(a1)
  c->lwu(v1, 4, a1);                                // lwu v1, 4(a1)
  c->lwu(a0, 8, a1);                                // lwu a0, 8(a1)
  c->lui(a1, 4096);                                 // lui a1, 4096
  c->ori(a1, a1, 53248);                            // ori a1, a1, 53248
  c->addiu(a3, r0, 0);                              // addiu a3, r0, 0
  
//block_1:
//  c->lw(a3, 0, a1);                                 // lw a3, 0(a1)
//  c->andi(a3, a3, 256);                             // andi a3, a3, 256
//  // nop                                            // sll r0, r0, 0
//  // nop                                            // sll r0, r0, 0
//  // nop                                            // sll r0, r0, 0
//  // nop                                            // sll r0, r0, 0
//  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L186
//  // nop                                            // sll r0, r0, 0
//  if (bc) {goto block_1;}                           // branch non-likely

  // Unknown instr: sync.l
  c->lui(a3, 4095);                                 // lui a3, 4095
  c->ori(a3, a3, 65535);                            // ori a3, a3, 65535
  c->and_(a2, a3, a2);                              // and a2, a3, a2
  // c->sw(a2, 16, a1);                                // sw a2, 16(a1)
  u32 madr = c->sgpr64(a2);

  c->lui(a2, 4095);                                 // lui a2, 4095
  c->ori(a2, a2, 65535);                            // ori a2, a2, 65535
  c->and_(v1, a2, v1);                              // and v1, a2, v1
  // c->sw(v1, 128, a1);                               // sw v1, 128(a1)
  u32 sadr = c->sgpr64(v1);

  // c->sw(a0, 32, a1);                                // sw a0, 32(a1)
  u32 qwc = c->sgpr64(a0);
  // Unknown instr: sync.l
  c->addiu(v1, r0, 256);                            // addiu v1, r0, 256
  // c->sw(v1, 0, a1);                                 // sw v1, 0(a1)
  spad_from_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
  // Unknown instr: sync.l
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
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
  gLinkedFunctionTable.reg("(method 18 nav-engine)", execute, 0);
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
}

} // namespace method_18_nav_engine

namespace method_21_nav_engine {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->lwu(v1, 80, a0);                               // lwu v1, 80(a0)
  c->lwu(v1, 4, v1);                                // lwu v1, 4(v1)
  c->lwu(a2, 8, a0);                                // lwu a2, 8(a0)
  c->lwu(a2, 4, a2);                                // lwu a2, 4(a2)
  c->dsubu(v1, v1, a2);                             // dsubu v1, v1, a2
  c->addiu(a2, r0, 0);                              // addiu a2, r0, 0
  //beq r0, r0, L173                                // beq r0, r0, L173
  // nop                                            // sll r0, r0, 0
  goto block_4;                                     // branch always

  
block_1:
  c->lwu(a3, 4, a1);                                // lwu a3, 4(a1)
  c->addiu(t0, r0, 288);                            // addiu t0, r0, 288
  c->mult3(t0, t0, a2);                             // mult3 t0, t0, a2
  c->daddu(a3, a3, t0);                             // daddu a3, a3, t0
  c->daddiu(t0, a3, 112);                           // daddiu t0, a3, 112
  c->lwu(t1, 8, a3);                                // lwu t1, 8(a3)
  bc = c->sgpr64(s7) == c->sgpr64(t1);              // beq s7, t1, L172
  c->mov64(t1, s7);                                 // or t1, s7, r0
  if (bc) {goto block_3;}                           // branch non-likely

  c->lwu(t1, 76, a0);                               // lwu t1, 76(a0)
  c->daddiu(t1, t1, 32);                            // daddiu t1, t1, 32
  c->sw(t1, 56, a3);                                // sw t1, 56(a3)
  c->lwu(t1, 80, a0);                               // lwu t1, 80(a0)
  c->sw(t1, 124, a3);                               // sw t1, 124(a3)
  c->lwu(t1, 0, a1);                                // lwu t1, 0(a1)
  c->addiu(t2, r0, 288);                            // addiu t2, r0, 288
  c->mult3(t2, t2, a2);                             // mult3 t2, t2, a2
  c->daddu(t1, t1, t2);                             // daddu t1, t1, t2
  c->sw(t1, 4, t0);                                 // sw t1, 4(t0)
  c->mov64(t1, v1);                                 // or t1, v1, r0
  c->lw(t2, 16, t0);                                // lw t2, 16(t0)
  c->daddu(t3, t2, t1);                             // daddu t3, t2, t1
  c->xor_(t4, t2, s7);                              // xor t4, t2, s7
  c->sltiu(t4, t4, 1); // Unknown instr: sltiu t4, t4, 1
  c->movz(t2, t3, t4);                              // movz t2, t3, t4
  c->sw(t2, 16, t0);                                // sw t2, 16(t0)
  c->lw(t2, 28, t0);                                // lw t2, 28(t0)
  c->daddu(t3, t2, t1);                             // daddu t3, t2, t1
  c->xor_(t4, t2, s7);                              // xor t4, t2, s7
  c->sltiu(t4, t4, 1); // Unknown instr: sltiu t4, t4, 1
  c->movz(t2, t3, t4);                              // movz t2, t3, t4
  c->sw(t2, 28, t0);                                // sw t2, 28(t0)
  c->lw(t2, 24, t0);                                // lw t2, 24(t0)
  c->daddu(t3, t2, t1);                             // daddu t3, t2, t1
  c->xor_(t4, t2, s7);                              // xor t4, t2, s7
  c->sltiu(t4, t4, 1); // Unknown instr: sltiu t4, t4, 1
  c->movz(t2, t3, t4);                              // movz t2, t3, t4
  c->sw(t2, 24, t0);                                // sw t2, 24(t0)
  c->lw(t2, 20, t0);                                // lw t2, 20(t0)
  c->daddu(t1, t2, t1);                             // daddu t1, t2, t1
  c->xor_(t3, t2, s7);                              // xor t3, t2, s7
  c->sltiu(t3, t3, 1); // Unknown instr: sltiu t3, t3, 1
  c->movz(t2, t1, t3);                              // movz t2, t1, t3
  c->sw(t2, 20, t0);                                // sw t2, 20(t0)
  c->gprs[t0].du64[0] = 0;                          // or t0, r0, r0
  c->lwu(t0, 0, a1);                                // lwu t0, 0(a1)
  c->addiu(t1, r0, 288);                            // addiu t1, r0, 288
  c->mult3(t1, t1, a2);                             // mult3 t1, t1, a2
  c->daddu(t1, t0, t1);                             // daddu t1, t0, t1
  c->lwu(a3, 8, a3);                                // lwu a3, 8(a3)
  c->sw(t1, 140, a3);                               // sw t1, 140(a3)
  
block_3:
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  
block_4:
  c->lb(a3, 14, a1);                                // lb a3, 14(a1)
  c->slt(a3, a2, a3);                               // slt a3, a2, a3
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L171
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
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
  gLinkedFunctionTable.reg("(method 21 nav-engine)", execute, 0);
}

} // namespace method_21_nav_engine
} // namespace Mips2C
