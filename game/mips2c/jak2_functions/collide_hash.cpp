//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace method_11_collide_hash {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->daddiu(sp, sp, -96);                           // daddiu sp, sp, -96
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s2, 16, sp);                                // sq s2, 16(sp)
  c->sq(s3, 32, sp);                                // sq s3, 32(sp)
  c->sq(s4, 48, sp);                                // sq s4, 48(sp)
  c->sq(s5, 64, sp);                                // sq s5, 64(sp)
  c->sq(gp, 80, sp);                                // sq gp, 80(sp)
  c->addiu(v1, r0, 0);                              // addiu v1, r0, 0
  //beq r0, r0, L91                                 // beq r0, r0, L91
  // nop                                            // sll r0, r0, 0
  goto block_2;                                     // branch always


  block_1:
  c->dsll(a1, v1, 4);                               // dsll a1, v1, 4
  get_fake_spad_addr2(t0, cache.fake_scratchpad_data, 0, c);// lui t0, 28672
  c->daddu(a1, a1, t0);                             // daddu a1, a1, t0
  c->sq(r0, 0, a1);                                 // sq r0, 0(a1)
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1

  block_2:
  c->lwu(a1, 8, a0);                                // lwu a1, 8(a0)
  c->slt(a1, v1, a1);                               // slt a1, v1, a1
  bc = c->sgpr64(a1) != 0;                          // bne a1, r0, L90
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 72, a0);                                // lw v1, 72(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf1, 28, a0);                             // lqc2 vf1, 28(a0)
  c->lui(a1, 1);                                    // lui a1, 1
  c->lqc2(vf2, 128, a3);                            // lqc2 vf2, 128(a3)
  c->ori(a1, a1, 257);                              // ori a1, a1, 257
  c->lqc2(vf3, 144, a3);                            // lqc2 vf3, 144(a3)
  c->dsubu(v1, v1, a1);                             // dsubu v1, v1, a1
  c->lqc2(vf4, 44, a0);                             // lqc2 vf4, 44(a0)
  c->vsub(DEST::xyzw, vf2, vf2, vf1);               // vsub.xyzw vf2, vf2, vf1
  c->lq(a1, 160, a3);                               // lq a1, 160(a3)
  c->vsub(DEST::xyzw, vf3, vf3, vf1);               // vsub.xyzw vf3, vf3, vf1
  c->lq(t0, 176, a3);                               // lq t0, 176(a3)
  c->pextlb(v1, r0, v1);                            // pextlb v1, r0, v1
  c->lq(t1, 60, a0);                                // lq t1, 60(a0)
  c->pextlh(v1, r0, v1);                            // pextlh v1, r0, v1
  c->lq(t2, 76, a0);                                // lq t2, 76(a0)
  c->pcgtw(t0, t1, t0);                             // pcgtw t0, t1, t0
  c->vmul(DEST::xyzw, vf2, vf2, vf4);               // vmul.xyzw vf2, vf2, vf4
  c->pcgtw(a1, a1, t2);                             // pcgtw a1, a1, t2
  c->vmul(DEST::xyzw, vf3, vf3, vf4);               // vmul.xyzw vf3, vf3, vf4
  c->por(a1, t0, a1);                               // por a1, t0, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppach(a1, r0, a1);                             // ppach a1, r0, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsll(a1, a1, 16);                              // dsll a1, a1, 16
  c->vftoi0(DEST::xyzw, vf2, vf2);                  // vftoi0.xyzw vf2, vf2
  bc = c->sgpr64(a1) != 0;                          // bne a1, r0, L98
  c->vftoi0(DEST::xyzw, vf3, vf3);                  // vftoi0.xyzw vf3, vf3
  if (bc) {goto block_16;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t0, vf2);                        // qmfc2.i t0, vf2
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a1, vf3);                        // qmfc2.i a1, vf3
  c->pmaxw(t0, t0, r0);                             // pmaxw t0, t0, r0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaxw(a1, a1, r0);                             // pmaxw a1, a1, r0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pminw(t0, t0, v1);                             // pminw t0, t0, v1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pminw(v1, a1, v1);                             // pminw v1, a1, v1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->sq(t0, 400, a3);                               // sq t0, 400(a3)
  // nop                                            // sll r0, r0, 0
  c->sq(v1, 416, a3);                               // sq v1, 416(a3)
  c->addiu(v1, r0, 4);                              // addiu v1, r0, 4
  c->lbu(a1, 72, a0);                               // lbu a1, 72(a0)
  c->multu3(a1, a1, v1);                            // multu3 a1, a1, v1
  c->lbu(t0, 74, a0);                               // lbu t0, 74(a0)
  c->multu3(t0, t0, a1);                            // multu3 t0, t0, a1
  c->addiu(t1, r0, 1);                              // addiu t1, r0, 1
  c->lw(t2, 400, a3);                               // lw t2, 400(a3)
  c->dsubu(t1, t1, t2);                             // dsubu t1, t1, t2
  c->lw(t2, 416, a3);                               // lw t2, 416(a3)
  c->daddu(t1, t1, t2);                             // daddu t1, t1, t2
  c->addiu(t2, r0, 1);                              // addiu t2, r0, 1
  c->lw(t3, 404, a3);                               // lw t3, 404(a3)
  c->dsubu(t2, t2, t3);                             // dsubu t2, t2, t3
  c->lw(t3, 420, a3);                               // lw t3, 420(a3)
  c->daddu(t4, t2, t3);                             // daddu t4, t2, t3
  c->addiu(t2, r0, 1);                              // addiu t2, r0, 1
  c->lw(t3, 408, a3);                               // lw t3, 408(a3)
  c->dsubu(t2, t2, t3);                             // dsubu t2, t2, t3
  c->lw(t3, 424, a3);                               // lw t3, 424(a3)
  c->daddu(t2, t2, t3);                             // daddu t2, t2, t3
  c->lwu(t3, 40, a0);                               // lwu t3, 40(a0)
  c->lw(t5, 400, a3);                               // lw t5, 400(a3)
  c->mult3(t5, t5, v1);                             // mult3 t5, t5, v1
  c->lw(t6, 404, a3);                               // lw t6, 404(a3)
  c->mult3(t6, t6, t0);                             // mult3 t6, t6, t0
  c->daddu(t5, t5, t6);                             // daddu t5, t5, t6
  c->lw(t6, 408, a3);                               // lw t6, 408(a3)
  c->mult3(t6, t6, a1);                             // mult3 t6, t6, a1
  c->daddu(t5, t5, t6);                             // daddu t5, t5, t6
  c->daddu(t3, t3, t5);                             // daddu t3, t3, t5
  c->mov64(t4, t4);                                 // or t4, t4, r0
  // nop                                            // sll r0, r0, 0

  block_5:
  c->mov64(t5, t2);                                 // or t5, t2, r0
  c->mov64(t6, t3);                                 // or t6, t3, r0

  block_6:
  c->mov64(t7, t1);                                 // or t7, t1, r0
  c->mov64(t8, t6);                                 // or t8, t6, r0

  block_7:
  // nop                                            // sll r0, r0, 0
  c->lhu(t9, 0, t8);                                // lhu t9, 0(t8)
  // nop                                            // sll r0, r0, 0
  c->lw(ra, 56, a0);                                // lw ra, 56(a0)
  c->sll(gp, t9, 3);                                // sll gp, t9, 3
  c->lhu(t9, 2, t8);                                // lhu t9, 2(t8)
  bc = c->sgpr64(t9) == 0;                          // beq t9, r0, L97
  c->daddu(ra, gp, ra);                             // daddu ra, gp, ra
  if (bc) {goto block_13;}                          // branch non-likely


  block_8:
  get_fake_spad_addr2(s5, cache.fake_scratchpad_data, 0, c);// lui s5, 28672
  c->lw(s2, 0, ra);                                 // lw s2, 0(ra)
  c->addiu(s4, r0, 1);                              // addiu s4, r0, 1
  c->lw(gp, 4, ra);                                 // lw gp, 4(ra)
  c->andi(s3, s2, 7);                               // andi s3, s2, 7
  c->sra(s2, s2, 3);                                // sra s2, s2, 3
  c->daddu(s5, s2, s5);                             // daddu s5, s2, s5
  c->lqc2(vf8, 12, gp);                             // lqc2 vf8, 12(gp)
  c->sllv(s4, s4, s3);                              // sllv s4, s4, s3
  c->lb(s3, 0, s5);                                 // lb s3, 0(s5)
  c->and_(s2, s3, s4);                              // and s2, s3, s4
  c->daddiu(t9, t9, -1);                            // daddiu t9, t9, -1
  bc = c->sgpr64(s2) != 0;                          // bne s2, r0, L96
  c->or_(s4, s3, s4);                               // or s4, s3, s4
  if (bc) {goto block_12;}                          // branch non-likely

  c->vsub_bc(DEST::xyzw, BC::w, vf2, vf8, vf8);     // vsubw.xyzw vf2, vf8, vf8
  c->sb(s4, 0, s5);                                 // sb s4, 0(s5)
  c->vadd_bc(DEST::xyzw, BC::w, vf3, vf8, vf8);     // vaddw.xyzw vf3, vf8, vf8
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf2, vf2);                  // vftoi0.xyzw vf2, vf2
  c->lq(s5, 160, a3);                               // lq s5, 160(a3)
  c->vftoi0(DEST::xyzw, vf3, vf3);                  // vftoi0.xyzw vf3, vf3
  c->lq(s3, 176, a3);                               // lq s3, 176(a3)
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(s2, vf2);                        // qmfc2.i s2, vf2
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(s4, vf3);                        // qmfc2.i s4, vf3
  c->pcgtw(s3, s2, s3);                             // pcgtw s3, s2, s3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(s5, s5, s4);                             // pcgtw s5, s5, s4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->por(s5, s3, s5);                               // por s5, s3, s5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppach(s5, r0, s5);                             // ppach s5, r0, s5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsll(s5, s5, 16);                              // dsll s5, s5, 16
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s5) != 0;                          // bne s5, r0, L96
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(s4, 0, a2);                                 // lw s4, 0(a2)
  c->daddiu(s3, s4, -256);                          // daddiu s3, s4, -256
  c->sll(s5, s4, 3);                                // sll s5, s4, 3
  bc = ((s64)c->sgpr64(s3)) >= 0;                   // bgez s3, L98
  c->daddiu(s4, s4, 1);                             // daddiu s4, s4, 1
  if (bc) {goto block_16;}                          // branch non-likely

  c->daddu(s5, s5, a2);                             // daddu s5, s5, a2
  c->sw(s4, 0, a2);                                 // sw s4, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(s7, 20, s5);                                // sw s7, 20(s5)
  // nop                                            // sll r0, r0, 0
  c->sw(gp, 16, s5);                                // sw gp, 16(s5)

  block_12:
  bc = ((s64)c->sgpr64(t9)) > 0;                    // bgtz t9, L95
  c->daddiu(ra, ra, 8);                             // daddiu ra, ra, 8
  if (bc) {goto block_8;}                           // branch non-likely


  block_13:
  c->daddiu(t7, t7, -1);                            // daddiu t7, t7, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t7) != 0;                          // bne t7, r0, L94
  c->daddu(t8, t8, v1);                             // daddu t8, t8, v1
  if (bc) {goto block_7;}                           // branch non-likely

  c->daddiu(t5, t5, -1);                            // daddiu t5, t5, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t5) != 0;                          // bne t5, r0, L93
  c->daddu(t6, t6, a1);                             // daddu t6, t6, a1
  if (bc) {goto block_6;}                           // branch non-likely

  c->daddiu(t4, t4, -1);                            // daddiu t4, t4, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t4) != 0;                          // bne t4, r0, L92
  c->daddu(t3, t3, t0);                             // daddu t3, t3, t0
  if (bc) {goto block_5;}                           // branch non-likely


  block_16:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
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
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("(method 11 collide-hash)", execute, 256);
}

} // namespace method_11_collide_hash
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace method_12_collide_hash {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->daddiu(sp, sp, -112);                          // daddiu sp, sp, -112
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s1, 16, sp);                                // sq s1, 16(sp)
  c->sq(s2, 32, sp);                                // sq s2, 32(sp)
  c->sq(s3, 48, sp);                                // sq s3, 48(sp)
  c->sq(s4, 64, sp);                                // sq s4, 64(sp)
  c->sq(s5, 80, sp);                                // sq s5, 80(sp)
  c->sq(gp, 96, sp);                                // sq gp, 96(sp)
  c->addiu(v1, r0, 0);                              // addiu v1, r0, 0
  //beq r0, r0, L81                                 // beq r0, r0, L81
  // nop                                            // sll r0, r0, 0
  goto block_2;                                     // branch always


  block_1:
  c->dsll(a1, v1, 4);                               // dsll a1, v1, 4
  get_fake_spad_addr2(t0, cache.fake_scratchpad_data, 0, c);// lui t0, 28672
  c->daddu(a1, a1, t0);                             // daddu a1, a1, t0
  c->sq(r0, 0, a1);                                 // sq r0, 0(a1)
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1

  block_2:
  c->lwu(a1, 8, a0);                                // lwu a1, 8(a0)
  c->slt(a1, v1, a1);                               // slt a1, v1, a1
  bc = c->sgpr64(a1) != 0;                          // bne a1, r0, L80
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 72, a0);                                // lw v1, 72(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 28, a0);                             // lqc2 vf2, 28(a0)
  c->lui(a1, 1);                                    // lui a1, 1
  c->lqc2(vf3, 128, a3);                            // lqc2 vf3, 128(a3)
  c->ori(a1, a1, 257);                              // ori a1, a1, 257
  c->lqc2(vf4, 144, a3);                            // lqc2 vf4, 144(a3)
  c->dsubu(v1, v1, a1);                             // dsubu v1, v1, a1
  c->lqc2(vf5, 44, a0);                             // lqc2 vf5, 44(a0)
  c->vsub(DEST::xyzw, vf3, vf3, vf2);               // vsub.xyzw vf3, vf3, vf2
  c->lq(a1, 160, a3);                               // lq a1, 160(a3)
  c->vsub(DEST::xyzw, vf4, vf4, vf2);               // vsub.xyzw vf4, vf4, vf2
  c->lq(t0, 176, a3);                               // lq t0, 176(a3)
  c->pextlb(v1, r0, v1);                            // pextlb v1, r0, v1
  c->lq(t1, 60, a0);                                // lq t1, 60(a0)
  c->pextlh(v1, r0, v1);                            // pextlh v1, r0, v1
  c->lq(t2, 76, a0);                                // lq t2, 76(a0)
  c->pcgtw(t0, t1, t0);                             // pcgtw t0, t1, t0
  c->vmul(DEST::xyzw, vf3, vf3, vf5);               // vmul.xyzw vf3, vf3, vf5
  c->pcgtw(a1, a1, t2);                             // pcgtw a1, a1, t2
  c->vmul(DEST::xyzw, vf4, vf4, vf5);               // vmul.xyzw vf4, vf4, vf5
  c->por(a1, t0, a1);                               // por a1, t0, a1
  c->lqc2(vf10, 256, a3);                           // lqc2 vf10, 256(a3)
  c->ppach(a1, r0, a1);                             // ppach a1, r0, a1
  c->lqc2(vf11, 272, a3);                           // lqc2 vf11, 272(a3)
  c->dsll(a1, a1, 16);                              // dsll a1, a1, 16
  c->vftoi0(DEST::xyzw, vf3, vf3);                  // vftoi0.xyzw vf3, vf3
  bc = c->sgpr64(a1) != 0;                          // bne a1, r0, L88
  c->vftoi0(DEST::xyzw, vf4, vf4);                  // vftoi0.xyzw vf4, vf4
  if (bc) {goto block_19;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lqc2(vf8, 208, a3);                            // lqc2 vf8, 208(a3)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf9, 224, a3);                            // lqc2 vf9, 224(a3)
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t0, vf3);                        // qmfc2.i t0, vf3
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a1, vf4);                        // qmfc2.i a1, vf4
  c->pmaxw(t0, t0, r0);                             // pmaxw t0, t0, r0
  c->lqc2(vf6, 12, a0);                             // lqc2 vf6, 12(a0)
  c->pmaxw(a1, a1, r0);                             // pmaxw a1, a1, r0
  c->lqc2(vf7, 240, a3);                            // lqc2 vf7, 240(a3)
  c->pminw(t0, t0, v1);                             // pminw t0, t0, v1
  c->vmax_bc(DEST::xyzw, BC::w, vf1, vf0, vf0);     // vmaxw.xyzw vf1, vf0, vf0
  c->pminw(v1, a1, v1);                             // pminw v1, a1, v1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->sq(t0, 400, a3);                               // sq t0, 400(a3)
  // nop                                            // sll r0, r0, 0
  c->sq(v1, 416, a3);                               // sq v1, 416(a3)
  c->addiu(v1, r0, 4);                              // addiu v1, r0, 4
  c->lbu(a1, 72, a0);                               // lbu a1, 72(a0)
  c->multu3(a1, a1, v1);                            // multu3 a1, a1, v1
  c->lbu(t0, 74, a0);                               // lbu t0, 74(a0)
  c->multu3(t0, t0, a1);                            // multu3 t0, t0, a1
  c->addiu(t1, r0, 1);                              // addiu t1, r0, 1
  c->lw(t2, 400, a3);                               // lw t2, 400(a3)
  c->dsubu(t1, t1, t2);                             // dsubu t1, t1, t2
  c->lw(t2, 416, a3);                               // lw t2, 416(a3)
  c->daddu(t1, t1, t2);                             // daddu t1, t1, t2
  c->addiu(t2, r0, 1);                              // addiu t2, r0, 1
  c->lw(t3, 404, a3);                               // lw t3, 404(a3)
  c->dsubu(t2, t2, t3);                             // dsubu t2, t2, t3
  c->lw(t3, 420, a3);                               // lw t3, 420(a3)
  c->daddu(t2, t2, t3);                             // daddu t2, t2, t3
  c->addiu(t3, r0, 1);                              // addiu t3, r0, 1
  c->lw(t4, 408, a3);                               // lw t4, 408(a3)
  c->dsubu(t3, t3, t4);                             // dsubu t3, t3, t4
  c->lw(t4, 424, a3);                               // lw t4, 424(a3)
  c->daddu(t3, t3, t4);                             // daddu t3, t3, t4
  c->lwu(t4, 40, a0);                               // lwu t4, 40(a0)
  c->lw(t5, 400, a3);                               // lw t5, 400(a3)
  c->mult3(t5, t5, v1);                             // mult3 t5, t5, v1
  c->lw(t6, 404, a3);                               // lw t6, 404(a3)
  c->mult3(t6, t6, t0);                             // mult3 t6, t6, t0
  c->daddu(t5, t5, t6);                             // daddu t5, t5, t6
  c->lw(t6, 408, a3);                               // lw t6, 408(a3)
  c->mult3(t6, t6, a1);                             // mult3 t6, t6, a1
  c->daddu(t5, t5, t6);                             // daddu t5, t5, t6
  c->daddu(t4, t4, t5);                             // daddu t4, t4, t5
  c->mov64(t5, t2);                                 // or t5, t2, r0
  // nop                                            // sll r0, r0, 0

  block_5:
  c->mov64(t6, t3);                                 // or t6, t3, r0
  c->mov64(t7, t4);                                 // or t7, t4, r0

  block_6:
  c->mov64(t8, t1);                                 // or t8, t1, r0
  c->mov64(t9, t7);                                 // or t9, t7, r0

  block_7:
  c->dsubu(s3, t1, t8);                             // dsubu s3, t1, t8
  c->lw(s2, 400, a3);                               // lw s2, 400(a3)
  c->dsubu(s5, t2, t5);                             // dsubu s5, t2, t5
  c->lw(s4, 404, a3);                               // lw s4, 404(a3)
  c->dsubu(ra, t3, t6);                             // dsubu ra, t3, t6
  c->lw(gp, 408, a3);                               // lw gp, 408(a3)
  c->daddu(s3, s3, s2);                             // daddu s3, s3, s2
  c->sw(r0, 444, a3);                               // sw r0, 444(a3)
  c->daddu(s5, s5, s4);                             // daddu s5, s5, s4
  c->sw(s3, 432, a3);                               // sw s3, 432(a3)
  c->daddu(ra, ra, gp);                             // daddu ra, ra, gp
  c->sw(s5, 436, a3);                               // sw s5, 436(a3)
  // nop                                            // sll r0, r0, 0
  c->sw(ra, 440, a3);                               // sw ra, 440(a3)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 432, a3);                            // lqc2 vf3, 432(a3)
  c->vitof0(DEST::xyzw, vf3, vf3);                  // vitof0.xyzw vf3, vf3
  // nop                                            // sll r0, r0, 0
  c->vmula(DEST::xyzw,  vf3, vf6);                  // vmula.xyzw acc, vf3, vf6
  // nop                                            // sll r0, r0, 0
  c->vmadda(DEST::xyzw,  vf1, vf2);                 // vmadda.xyzw acc, vf1, vf2
  // nop                                            // sll r0, r0, 0
  c->vmsub_bc(DEST::xyzw, BC::w, vf3, vf1, vf10);   // vmsubw.xyzw vf3, vf1, vf10
  // nop                                            // sll r0, r0, 0
  c->vmadda(DEST::xyzw,  vf1, vf6);                 // vmadda.xyzw acc, vf1, vf6
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::w, vf4, vf1, vf10);   // vmaddw.xyzw vf4, vf1, vf10
  // nop                                            // sll r0, r0, 0
  c->vmula(DEST::xyzw,  vf7, vf3);                  // vmula.xyzw acc, vf7, vf3
  // nop                                            // sll r0, r0, 0
  c->vmsuba(DEST::xyzw,  vf7, vf8);                 // vmsuba.xyzw acc, vf7, vf8
  // nop                                            // sll r0, r0, 0
  c->vmsub(DEST::xyzw, vf13, vf1, vf10);            // vmsub.xyzw vf13, vf1, vf10
  // nop                                            // sll r0, r0, 0
  c->vmadd(DEST::xyzw, vf15, vf1, vf11);            // vmadd.xyzw vf15, vf1, vf11
  // nop                                            // sll r0, r0, 0
  c->vmula(DEST::xyzw,  vf7, vf4);                  // vmula.xyzw acc, vf7, vf4
  // nop                                            // sll r0, r0, 0
  c->vmsuba(DEST::xyzw,  vf7, vf8);                 // vmsuba.xyzw acc, vf7, vf8
  // nop                                            // sll r0, r0, 0
  c->vmsub(DEST::xyzw, vf14, vf1, vf11);            // vmsub.xyzw vf14, vf1, vf11
  // nop                                            // sll r0, r0, 0
  c->vmadd(DEST::xyzw, vf16, vf1, vf10);            // vmadd.xyzw vf16, vf1, vf10
  // nop                                            // sll r0, r0, 0
  c->vmax(DEST::xyzw, vf13, vf13, vf14);            // vmax.xyzw vf13, vf13, vf14
  // nop                                            // sll r0, r0, 0
  c->vmini(DEST::xyzw, vf15, vf15, vf16);           // vmini.xyzw vf15, vf15, vf16
  // nop                                            // sll r0, r0, 0
  c->vmax_bc(DEST::xyzw, BC::y, vf13, vf13, vf13);  // vmaxy.xyzw vf13, vf13, vf13
  // nop                                            // sll r0, r0, 0
  c->vmini_bc(DEST::xyzw, BC::y, vf15, vf15, vf15); // vminiy.xyzw vf15, vf15, vf15
  // nop                                            // sll r0, r0, 0
  c->vmax_bc(DEST::xyzw, BC::z, vf13, vf13, vf13);  // vmaxz.xyzw vf13, vf13, vf13
  // nop                                            // sll r0, r0, 0
  c->vmini_bc(DEST::xyzw, BC::z, vf15, vf15, vf15); // vminiz.xyzw vf15, vf15, vf15
  // nop                                            // sll r0, r0, 0
  c->vftoi4(DEST::xyzw, vf13, vf13);                // vftoi4.xyzw vf13, vf13
  // nop                                            // sll r0, r0, 0
  c->vftoi4(DEST::xyzw, vf15, vf15);                // vftoi4.xyzw vf15, vf15
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(s5, vf13);                       // qmfc2.i s5, vf13
  c->addiu(s4, r0, 4096);                           // addiu s4, r0, 4096
  c->mov128_gpr_vf(ra, vf15);                       // qmfc2.i ra, vf15
  c->subu(gp, ra, s5);                              // subu gp, ra, s5
  c->subu(s5, s4, s5);                              // subu s5, s4, s5
  bc = ((s64)c->sgpr64(gp)) < 0;                    // bltz gp, L87
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_16;}                          // branch non-likely

  bc = ((s64)c->sgpr64(s5)) < 0;                    // bltz s5, L87
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_16;}                          // branch non-likely

  bc = ((s64)c->sgpr64(ra)) < 0;                    // bltz ra, L87
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_16;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lhu(ra, 0, t9);                                // lhu ra, 0(t9)
  // nop                                            // sll r0, r0, 0
  c->lw(gp, 56, a0);                                // lw gp, 56(a0)
  c->sll(s5, ra, 3);                                // sll s5, ra, 3
  c->lhu(ra, 2, t9);                                // lhu ra, 2(t9)
  bc = c->sgpr64(ra) == 0;                          // beq ra, r0, L87
  c->daddu(gp, s5, gp);                             // daddu gp, s5, gp
  if (bc) {goto block_16;}                          // branch non-likely


  block_11:
  get_fake_spad_addr2(s4, cache.fake_scratchpad_data, 0, c);// lui s4, 28672
  c->lw(s1, 0, gp);                                 // lw s1, 0(gp)
  c->addiu(s3, r0, 1);                              // addiu s3, r0, 1
  c->lw(s5, 4, gp);                                 // lw s5, 4(gp)
  c->andi(s2, s1, 7);                               // andi s2, s1, 7
  c->sra(s1, s1, 3);                                // sra s1, s1, 3
  c->daddu(s4, s1, s4);                             // daddu s4, s1, s4
  c->lqc2(vf12, 12, s5);                            // lqc2 vf12, 12(s5)
  c->sllv(s3, s3, s2);                              // sllv s3, s3, s2
  c->lb(s2, 0, s4);                                 // lb s2, 0(s4)
  c->and_(s1, s2, s3);                              // and s1, s2, s3
  c->daddiu(ra, ra, -1);                            // daddiu ra, ra, -1
  bc = c->sgpr64(s1) != 0;                          // bne s1, r0, L86
  c->or_(s3, s2, s3);                               // or s3, s2, s3
  if (bc) {goto block_15;}                          // branch non-likely

  c->vmula(DEST::xyzw,  vf9, vf12);                 // vmula.xyzw acc, vf9, vf12
  c->sb(s3, 0, s4);                                 // sb s3, 0(s4)
  c->vmsub(DEST::xyzw, vf13, vf9, vf8);             // vmsub.xyzw vf13, vf9, vf8
  // nop                                            // sll r0, r0, 0
  c->vmul_bc(DEST::xyzw, BC::w, vf14, vf1, vf7);    // vmulw.xyzw vf14, vf1, vf7
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf1, vf12);        // vmulaw.xyzw acc, vf1, vf12
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::w, vf15, vf1, vf10);  // vmaddw.xyzw vf15, vf1, vf10
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf13, vf7);        // vmulaw.xyzw acc, vf13, vf7
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf14, vf13);      // vmadday.xyzw acc, vf14, vf13
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf13, vf14, vf13); // vmaddz.xyzw vf13, vf14, vf13
  // nop                                            // sll r0, r0, 0
  c->vmini_bc(DEST::xyzw, BC::w, vf13, vf13, vf0);  // vminiw.xyzw vf13, vf13, vf0
  // nop                                            // sll r0, r0, 0
  c->vmax_bc(DEST::xyzw, BC::x, vf13, vf13, vf0);   // vmaxx.xyzw vf13, vf13, vf0
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::x, vf9, vf13);        // vmulax.xyzw acc, vf9, vf13
  // nop                                            // sll r0, r0, 0
  c->vmadda(DEST::xyzw,  vf1, vf8);                 // vmadda.xyzw acc, vf1, vf8
  // nop                                            // sll r0, r0, 0
  c->vmsub(DEST::xyzw, vf13, vf1, vf12);            // vmsub.xyzw vf13, vf1, vf12
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf13, vf13, vf13);            // vmul.xyzw vf13, vf13, vf13
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::x, vf0, vf0);         // vmulax.xyzw acc, vf0, vf0
  // nop                                            // sll r0, r0, 0
  c->vmsuba(DEST::xyzw,  vf15, vf15);               // vmsuba.xyzw acc, vf15, vf15
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf13);       // vmaddax.xyzw acc, vf1, vf13
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf1, vf13);       // vmadday.xyzw acc, vf1, vf13
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf13, vf1, vf13);  // vmaddz.xyzw vf13, vf1, vf13
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(s4, vf13);                       // qmfc2.i s4, vf13
  bc = ((s64)c->sgpr64(s4)) > 0;                    // bgtz s4, L86
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_15;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(s3, 0, a2);                                 // lw s3, 0(a2)
  c->daddiu(s2, s3, -256);                          // daddiu s2, s3, -256
  c->sll(s4, s3, 3);                                // sll s4, s3, 3
  bc = ((s64)c->sgpr64(s2)) >= 0;                   // bgez s2, L88
  c->daddiu(s3, s3, 1);                             // daddiu s3, s3, 1
  if (bc) {goto block_19;}                          // branch non-likely

  c->daddu(s4, s4, a2);                             // daddu s4, s4, a2
  c->sw(s3, 0, a2);                                 // sw s3, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(s7, 20, s4);                                // sw s7, 20(s4)
  // nop                                            // sll r0, r0, 0
  c->sw(s5, 16, s4);                                // sw s5, 16(s4)

  block_15:
  bc = ((s64)c->sgpr64(ra)) > 0;                    // bgtz ra, L85
  c->daddiu(gp, gp, 8);                             // daddiu gp, gp, 8
  if (bc) {goto block_11;}                          // branch non-likely


  block_16:
  c->daddiu(t8, t8, -1);                            // daddiu t8, t8, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t8) != 0;                          // bne t8, r0, L84
  c->daddu(t9, t9, v1);                             // daddu t9, t9, v1
  if (bc) {goto block_7;}                           // branch non-likely

  c->daddiu(t6, t6, -1);                            // daddiu t6, t6, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t6) != 0;                          // bne t6, r0, L83
  c->daddu(t7, t7, a1);                             // daddu t7, t7, a1
  if (bc) {goto block_6;}                           // branch non-likely

  c->daddiu(t5, t5, -1);                            // daddiu t5, t5, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t5) != 0;                          // bne t5, r0, L82
  c->daddu(t4, t4, t0);                             // daddu t4, t4, t0
  if (bc) {goto block_5;}                           // branch non-likely


  block_19:
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
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("(method 12 collide-hash)", execute, 256);
}

} // namespace method_12_collide_hash
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace fill_bg_using_box_new {
struct Cache {
  void* cheat_mode; // *cheat-mode*
  void* collide_stats; // *collide-stats*
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* debug; // debug
  void* print_exceeded_max_cache_tris; // print-exceeded-max-cache-tris
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
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 40, a1);                                // lw v1, 40(a1)      v1 = frag.dimension_array;
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf1, 44, a1);                             // lqc2 vf1, 44(a1)   vf1 = frag.bbox.min
  c->lui(a3, 1);                                    // lui a3, 1          a3 = 0x10000
  c->lqc2(vf2, 128, a2);                            // lqc2 vf2, 128(a2)  vf2 = query.bbox.min
  c->ori(a3, a3, 257);                              // ori a3, a3, 257    a3=0x10101
  c->lqc2(vf3, 144, a2);                            // lqc2 vf3, 144(a2)  vf3 = query.bbox.max
  c->dsubu(v1, v1, a3);                             // dsubu v1, v1, a3   some math on the dim array
  c->lqc2(vf6, 60, a1);                             // lqc2 vf6, 60(a1)   vf6 = frag.axis_scale;
  c->vsub(DEST::xyzw, vf2, vf2, vf1);               // vsub.xyzw vf2, vf2, vf1  vf2 = query.min - frag.min
  c->lq(a3, 160, a2);                               // lq a3, 160(a2)          a3 = query.bbox.min.i
  c->vsub(DEST::xyzw, vf3, vf3, vf1);               // vsub.xyzw vf3, vf3, vf1 vf3 = query.max - frag.min
  c->lq(t0, 176, a2);                               // lq t0, 176(a2)          t0 = query.bbox.max.i
  c->pextlb(v1, r0, v1);                            // pextlb v1, r0, v1
  c->lq(t1, 76, a1);                                // lq t1, 76(a1)           t1 = frag.bbox.min.i
  c->pextlh(v1, r0, v1);                            // pextlh v1, r0, v1       v1 = dimensions, int.
  c->lq(t2, 92, a1);                                // lq t2, 92(a1)           t2 = frag.bbox.max.i
  c->pcgtw(t0, t1, t0);                             // pcgtw t0, t1, t0        check for bbox miss.
  c->vftoi0(DEST::xyzw, vf4, vf2);                  // vftoi0.xyzw vf4, vf2    vf4 = int(q.min - f.min)
  c->pcgtw(a3, a3, t2);                             // pcgtw a3, a3, t2        check for bbox miss.
  c->vftoi0(DEST::xyzw, vf5, vf3);                  // vftoi0.xyzw vf5, vf3    vf5 = int(q.max - f.min)
  c->por(a3, t0, a3);                               // por a3, t0, a3          or miss
  c->vmul(DEST::xyzw, vf2, vf2, vf6);               // vmul.xyzw vf2, vf2, vf6  axis scale
  c->ppach(a3, r0, a3);                             // ppach a3, r0, a3         or stuff
  c->vmul(DEST::xyzw, vf3, vf3, vf6);               // vmul.xyzw vf3, vf3, vf6  axis scale
  c->dsll(t0, a3, 16);                              // dsll t0, a3, 16
  c->mov128_gpr_vf(a3, vf4);                        // qmfc2.i a3, vf4         a3 = int(q.min - f.min)
  bc = c->sgpr64(t0) != 0;                          // bne t0, r0, L75
  c->mov128_gpr_vf(t0, vf5);                        // qmfc2.i t0, vf5         t0 = int(q.max - f.min)
  if (bc) {goto block_17;}                          // branch non-likely

  // patch because there's a totally bogus collide fragment with an axis scale of
  // 0.00012207031, 0.00012207031, 1807931
  c->vftoi0_sat(DEST::xyzw, vf2, vf2);              // vftoi0.xyzw vf2, vf2
  c->psraw(a3, a3, 4);                              // psraw a3, a3, 4
  c->vftoi0_sat(DEST::xyzw, vf3, vf3);              // vftoi0.xyzw vf3, vf3
  c->psraw(t0, t0, 4);                              // psraw t0, t0, 4
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 368, a2);                               // sq a3, 368(a2)
  // nop                                            // sll r0, r0, 0
  c->sq(t0, 384, a2);                               // sq t0, 384(a2)
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t0, vf2);                        // qmfc2.i t0, vf2
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a3, vf3);                        // qmfc2.i a3, vf3
  c->pmaxw(t0, t0, r0);                             // pmaxw t0, t0, r0
  c->lqc2(vf10, 448, a2);                           // lqc2 vf10, 448(a2)
  c->pmaxw(a3, a3, r0);                             // pmaxw a3, a3, r0
  c->lqc2(vf11, 464, a2);                           // lqc2 vf11, 464(a2)
  c->pminw(t0, t0, v1);                             // pminw t0, t0, v1
  c->lqc2(vf12, 480, a2);                           // lqc2 vf12, 480(a2)
  c->pminw(v1, a3, v1);                             // pminw v1, a3, v1
  c->lqc2(vf13, 496, a2);                           // lqc2 vf13, 496(a2)
  // nop                                            // sll r0, r0, 0
  c->sq(t0, 400, a2);                               // sq t0, 400(a2)
  // nop                                            // sll r0, r0, 0
  c->sq(v1, 416, a2);                               // sq v1, 416(a2)
  c->addiu(v1, r0, 4);                              // addiu v1, r0, 4
  c->lbu(a3, 40, a1);                               // lbu a3, 40(a1)
  c->multu3(t8, a3, v1);                            // multu3 t8, a3, v1
  c->lbu(a3, 42, a1);                               // lbu a3, 42(a1)
  c->multu3(t6, a3, t8);                            // multu3 t6, a3, t8
  c->addiu(a3, r0, 1);                              // addiu a3, r0, 1
  c->lw(t0, 400, a2);                               // lw t0, 400(a2)
  c->dsubu(a3, a3, t0);                             // dsubu a3, a3, t0
  c->lw(t0, 416, a2);                               // lw t0, 416(a2)
  c->daddu(a3, a3, t0);                             // daddu a3, a3, t0
  c->addiu(t0, r0, 1);                              // addiu t0, r0, 1
  c->lw(t1, 404, a2);                               // lw t1, 404(a2)
  c->dsubu(t0, t0, t1);                             // dsubu t0, t0, t1
  c->lw(t1, 420, a2);                               // lw t1, 420(a2)
  c->daddu(t1, t0, t1);                             // daddu t1, t0, t1
  c->addiu(t0, r0, 1);                              // addiu t0, r0, 1
  c->lw(t2, 408, a2);                               // lw t2, 408(a2)
  c->dsubu(t0, t0, t2);                             // dsubu t0, t0, t2
  c->lw(t2, 424, a2);                               // lw t2, 424(a2)
  c->daddu(t0, t0, t2);                             // daddu t0, t0, t2
  c->lwu(t2, 8, a1);                                // lwu t2, 8(a1)
  c->lw(t3, 400, a2);                               // lw t3, 400(a2)
  c->mult3(t3, t3, v1);                             // mult3 t3, t3, v1
  c->lw(t4, 404, a2);                               // lw t4, 404(a2)
  c->mult3(t4, t4, t6);                             // mult3 t4, t4, t6
  c->daddu(t3, t3, t4);                             // daddu t3, t3, t4
  c->lw(t4, 408, a2);                               // lw t4, 408(a2)
  c->mult3(t4, t4, t8);                             // mult3 t4, t4, t8
  c->daddu(t3, t3, t4);                             // daddu t3, t3, t4
  c->daddu(t7, t2, t3);                             // daddu t7, t2, t3
  c->mov64(t1, t1);                                 // or t1, t1, r0
  // nop                                            // sll r0, r0, 0

  block_2:
  c->mov64(t5, t0);                                 // or t5, t0, r0
  c->mov64(t9, t7);                                 // or t9, t7, r0
  // nop                                            // sll r0, r0, 0
  c->sw(t7, 524, a2);                               // sw t7, 524(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(t6, 528, a2);                               // sw t6, 528(a2)

  block_3:
  c->mov64(t6, a3);                                 // or t6, a3, r0
  c->mov64(t7, t9);                                 // or t7, t9, r0
  // nop                                            // sll r0, r0, 0
  c->sw(t9, 532, a2);                               // sw t9, 532(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(t8, 536, a2);                               // sw t8, 536(a2)

  block_4:
  // nop                                            // sll r0, r0, 0
  c->lhu(ra, 0, t7);                                // lhu ra, 0(t7)
  // nop                                            // sll r0, r0, 0
  c->lw(gp, 104, a1);                               // lw gp, 104(a1)
  // nop                                            // sll r0, r0, 0
  c->lhu(t8, 2, t7);                                // lhu t8, 2(t7)
  // nop                                            // sll r0, r0, 0
  c->lw(t9, 72, a1);                                // lw t9, 72(a1)
  bc = c->sgpr64(t8) == 0;                          // beq t8, r0, L74
  c->daddu(ra, ra, gp);                             // daddu ra, ra, gp
  if (bc) {goto block_13;}                          // branch non-likely


  block_5:
  get_fake_spad_addr2(gp, cache.fake_scratchpad_data, 0, c);// lui gp, 28672
  c->lbu(s4, 0, ra);                                // lbu s4, 0(ra)
  c->addiu(s3, r0, 1);                              // addiu s3, r0, 1
  c->andi(s2, s4, 7);                               // andi s2, s4, 7
  c->sra(s5, s4, 3);                                // sra s5, s4, 3
  c->sll(s4, s4, 2);                                // sll s4, s4, 2
  c->daddu(s5, s5, gp);                             // daddu s5, s5, gp
  c->daddu(s4, s4, t9);                             // daddu s4, s4, t9
  c->sllv(s3, s3, s2);                              // sllv s3, s3, s2
  c->lb(s2, 0, s5);                                 // lb s2, 0(s5)
  // nop                                            // sll r0, r0, 0
  c->lbu(s0, 3, s4);                                // lbu s0, 3(s4)
  c->and_(s1, s2, s3);                              // and s1, s2, s3
  c->daddiu(t8, t8, -1);                            // daddiu t8, t8, -1
  bc = c->sgpr64(s1) != 0;                          // bne s1, r0, L73
  c->lw(s1, 4, a1);                                 // lw s1, 4(a1)
  if (bc) {goto block_12;}                          // branch non-likely

  c->dsll(s0, s0, 2);                               // dsll s0, s0, 2
  c->or_(s3, s2, s3);                               // or s3, s2, s3
  c->daddu(s2, s0, s1);                             // daddu s2, s0, s1
  c->sb(s3, 0, s5);                                 // sb s3, 0(s5)
  // nop                                            // sll r0, r0, 0
  c->lwu(s5, 0, s2);                                // lwu s5, 0(s2)
  // nop                                            // sll r0, r0, 0
  c->lwu(s2, 96, a2);                               // lwu s2, 96(a2)
  // nop                                            // sll r0, r0, 0
  c->lw(s3, 88, a1);                                // lw s3, 88(a1)
  c->and_(s2, s2, s5);                              // and s2, s2, s5
  c->lbu(s1, 0, s4);                                // lbu s1, 0(s4)
  bc = c->sgpr64(s2) != 0;                          // bne s2, r0, L73
  c->lbu(s2, 1, s4);                                // lbu s2, 1(s4)
  if (bc) {goto block_12;}                          // branch non-likely

  c->sll(s1, s1, 1);                                // sll s1, s1, 1
  c->lbu(s4, 2, s4);                                // lbu s4, 2(s4)
  c->sll(s0, s1, 1);                                // sll s0, s1, 1
  c->sll(s2, s2, 1);                                // sll s2, s2, 1
  c->daddu(s1, s1, s0);                             // daddu s1, s1, s0
  c->sll(s0, s2, 1);                                // sll s0, s2, 1
  c->sll(s4, s4, 1);                                // sll s4, s4, 1
  c->daddu(s0, s2, s0);                             // daddu s0, s2, s0
  c->sll(v0, s4, 1);                                // sll v0, s4, 1
  c->daddu(s2, s1, s3);                             // daddu s2, s1, s3
  c->daddu(s4, s4, v0);                             // daddu s4, s4, v0
  c->daddu(s1, s0, s3);                             // daddu s1, s0, s3
  c->daddu(s4, s4, s3);                             // daddu s4, s4, s3
  c->ldr(t2, 0, s2);                                // ldr t2, 0(s2)
  // nop                                            // sll r0, r0, 0
  c->ldl(t2, 7, s2);                                // ldl t2, 7(s2)
  // nop                                            // sll r0, r0, 0
  c->ldr(t3, 0, s1);                                // ldr t3, 0(s1)
  // nop                                            // sll r0, r0, 0
  c->ldl(t3, 7, s1);                                // ldl t3, 7(s1)
  // nop                                            // sll r0, r0, 0
  c->ldr(t4, 0, s4);                                // ldr t4, 0(s4)
  c->pextlh(t2, r0, t2);                            // pextlh t2, r0, t2
  c->ldl(t4, 7, s4);                                // ldl t4, 7(s4)
  c->pextlh(t3, r0, t3);                            // pextlh t3, r0, t3
  c->lq(s4, 368, a2);                               // lq s4, 368(a2)
  c->pextlh(t4, r0, t4);                            // pextlh t4, r0, t4
  c->lq(s3, 384, a2);                               // lq s3, 384(a2)
  c->pminw(s1, t2, t3);                             // pminw s1, t2, t3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaxw(s2, t2, t3);                             // pmaxw s2, t2, t3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pminw(s1, s1, t4);                             // pminw s1, s1, t4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaxw(s2, s2, t4);                             // pmaxw s2, s2, t4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(s3, s1, s3);                             // pcgtw s3, s1, s3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(s4, s4, s2);                             // pcgtw s4, s4, s2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->por(s4, s3, s4);                               // por s4, s3, s4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppach(s4, r0, s4);                             // ppach s4, r0, s4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsll(s4, s4, 16);                              // dsll s4, s4, 16
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s4) != 0;                          // bne s4, r0, L73
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely

  c->psllw(t2, t2, 4);                              // psllw t2, t2, 4
  c->lw(s4, 2048, gp);                              // lw s4, 2048(gp)
  c->psllw(t3, t3, 4);                              // psllw t3, t3, 4
  c->mov128_vf_gpr(vf7, t2);                        // qmtc2.i vf7, t2
  c->psllw(t4, t4, 4);                              // psllw t4, t4, 4
  c->mov128_vf_gpr(vf8, t3);                        // qmtc2.i vf8, t3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf9, t4);                        // qmtc2.i vf9, t4
  c->vitof0(DEST::xyzw, vf7, vf7);                  // vitof0.xyzw vf7, vf7
  c->daddiu(s4, s4, 1);                             // daddiu s4, s4, 1
  c->vitof0(DEST::xyzw, vf8, vf8);                  // vitof0.xyzw vf8, vf8
  c->sw(s4, 2048, gp);                              // sw s4, 2048(gp)
  c->vitof0(DEST::xyzw, vf9, vf9);                  // vitof0.xyzw vf9, vf9
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyzw, vf7, vf7, vf1);               // vadd.xyzw vf7, vf7, vf1
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyzw, vf8, vf8, vf1);               // vadd.xyzw vf8, vf8, vf1
  c->lwu(gp, 512, a2);                              // lwu gp, 512(a2)
  c->vadd(DEST::xyzw, vf9, vf9, vf1);               // vadd.xyzw vf9, vf9, vf1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(gp) == c->sgpr64(s7);              // beq gp, s7, L72
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_10;}                          // branch non-likely

  c->vmula_bc(DEST::xyzw, BC::x, vf10, vf7);        // vmulax.xyzw acc, vf10, vf7
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf11, vf7);       // vmadday.xyzw acc, vf11, vf7
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::z, vf12, vf7);       // vmaddaz.xyzw acc, vf12, vf7
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::w, vf7, vf13, vf0);   // vmaddw.xyzw vf7, vf13, vf0
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::x, vf10, vf8);        // vmulax.xyzw acc, vf10, vf8
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf11, vf8);       // vmadday.xyzw acc, vf11, vf8
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::z, vf12, vf8);       // vmaddaz.xyzw acc, vf12, vf8
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::w, vf8, vf13, vf0);   // vmaddw.xyzw vf8, vf13, vf0
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::x, vf10, vf9);        // vmulax.xyzw acc, vf10, vf9
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf11, vf9);       // vmadday.xyzw acc, vf11, vf9
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::z, vf12, vf9);       // vmaddaz.xyzw acc, vf12, vf9
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::w, vf9, vf13, vf0);   // vmaddw.xyzw vf9, vf13, vf0
  // nop                                            // sll r0, r0, 0

  block_10:
  c->pextlw(gp, gp, s5);                            // pextlw gp, gp, s5
  c->lw(s4, 0, a0);                                 // lw s4, 0(a0)
  // nop                                            // sll r0, r0, 0
  c->daddiu(s5, a0, 4908);                          // daddiu s5, a0, 4908
  c->daddiu(s3, s4, -460);                          // daddiu s3, s4, -460
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(s3)) >= 0;                   // bgez s3, L76
  c->dsll(s3, s4, 6);                               // dsll s3, s4, 6
  if (bc) {goto block_18;}                          // branch non-likely

  c->daddiu(s4, s4, 1);                             // daddiu s4, s4, 1
  c->daddu(s5, s5, s3);                             // daddu s5, s5, s3
  // nop                                            // sll r0, r0, 0
  c->sw(s4, 0, a0);                                 // sw s4, 0(a0)
  // nop                                            // sll r0, r0, 0
  c->sq(gp, 48, s5);                                // sq gp, 48(s5)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf7, 0, s5);                              // sqc2 vf7, 0(s5)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, 16, s5);                             // sqc2 vf8, 16(s5)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf9, 32, s5);                             // sqc2 vf9, 32(s5)

  block_12:
  bc = ((s64)c->sgpr64(t8)) > 0;                    // bgtz t8, L71
  c->daddiu(ra, ra, 1);                             // daddiu ra, ra, 1
  if (bc) {goto block_5;}                           // branch non-likely


  block_13:
  c->daddiu(t6, t6, -1);                            // daddiu t6, t6, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t6) != 0;                          // bne t6, r0, L70
  c->daddu(t7, t7, v1);                             // daddu t7, t7, v1
  if (bc) {goto block_4;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(t6, 532, a2);                               // lw t6, 532(a2)
  // nop                                            // sll r0, r0, 0
  c->lw(t8, 536, a2);                               // lw t8, 536(a2)
  c->daddiu(t5, t5, -1);                            // daddiu t5, t5, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t5) != 0;                          // bne t5, r0, L69
  c->daddu(t9, t6, t8);                             // daddu t9, t6, t8
  if (bc) {goto block_3;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(t5, 524, a2);                               // lw t5, 524(a2)
  // nop                                            // sll r0, r0, 0
  c->lw(t6, 528, a2);                               // lw t6, 528(a2)
  c->daddiu(t1, t1, -1);                            // daddiu t1, t1, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L68
  c->daddu(t7, t5, t6);                             // daddu t7, t5, t6
  if (bc) {goto block_2;}                           // branch non-likely

  c->load_symbol2(v1, cache.collide_stats);         // lw v1, *collide-stats*(s7)
  c->lwu(v1, 12, v1);                               // lwu v1, 12(v1)
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  c->load_symbol2(a0, cache.collide_stats);         // lw a0, *collide-stats*(s7)
  c->sw(v1, 12, a0);                                // sw v1, 12(a0)

  block_17:
  c->mov64(v0, s7);                                 // or v0, s7, r0
  //beq r0, r0, L78                                 // beq r0, r0, L78
  // nop                                            // sll r0, r0, 0
  goto block_21;                                    // branch always


  block_18:
  c->load_symbol_addr(v1, cache.debug);             // daddiu v1, s7, debug
  c->load_symbol2(a0, cache.cheat_mode);            // lw a0, *cheat-mode*(s7)
  bc = c->sgpr64(a0) != c->sgpr64(v1);              // bne a0, v1, L77
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_20;}                          // branch non-likely

  c->load_symbol2(t9, cache.print_exceeded_max_cache_tris);// lw t9, print-exceeded-max-cache-tris(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0

  block_20:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0

  block_21:
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
  cache.cheat_mode = intern_from_c("*cheat-mode*").c();
  cache.collide_stats = intern_from_c("*collide-stats*").c();
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  cache.debug = intern_from_c("debug").c();
  cache.print_exceeded_max_cache_tris = intern_from_c("print-exceeded-max-cache-tris").c();
  gLinkedFunctionTable.reg("fill-bg-using-box-new", execute, 256);
}

} // namespace fill_bg_using_box_new
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace fill_bg_using_line_sphere_new {
struct Cache {
  void* cheat_mode; // *cheat-mode*
  void* collide_stats; // *collide-stats*
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* debug; // debug
  void* print_exceeded_max_cache_tris; // print-exceeded-max-cache-tris
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
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 40, a1);                                // lw v1, 40(a1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 44, a1);                             // lqc2 vf2, 44(a1)
  c->lui(a3, 1);                                    // lui a3, 1
  c->lqc2(vf3, 128, a2);                            // lqc2 vf3, 128(a2)
  c->ori(a3, a3, 257);                              // ori a3, a3, 257
  c->lqc2(vf4, 144, a2);                            // lqc2 vf4, 144(a2)
  c->dsubu(v1, v1, a3);                             // dsubu v1, v1, a3
  c->lqc2(vf5, 60, a1);                             // lqc2 vf5, 60(a1)
  c->vsub(DEST::xyzw, vf3, vf3, vf2);               // vsub.xyzw vf3, vf3, vf2
  c->lq(a3, 160, a2);                               // lq a3, 160(a2)
  c->vsub(DEST::xyzw, vf4, vf4, vf2);               // vsub.xyzw vf4, vf4, vf2
  c->lq(t0, 176, a2);                               // lq t0, 176(a2)
  c->pextlb(v1, r0, v1);                            // pextlb v1, r0, v1
  c->lq(t1, 76, a1);                                // lq t1, 76(a1)
  c->pextlh(v1, r0, v1);                            // pextlh v1, r0, v1
  c->lq(t2, 92, a1);                                // lq t2, 92(a1)
  c->pcgtw(t0, t1, t0);                             // pcgtw t0, t1, t0
  c->vmul(DEST::xyzw, vf3, vf3, vf5);               // vmul.xyzw vf3, vf3, vf5
  c->pcgtw(a3, a3, t2);                             // pcgtw a3, a3, t2
  c->vmul(DEST::xyzw, vf4, vf4, vf5);               // vmul.xyzw vf4, vf4, vf5
  c->por(a3, t0, a3);                               // por a3, t0, a3
  c->lqc2(vf10, 256, a2);                           // lqc2 vf10, 256(a2)
  c->ppach(a3, r0, a3);                             // ppach a3, r0, a3
  c->lqc2(vf11, 272, a2);                           // lqc2 vf11, 272(a2)
  c->dsll(a3, a3, 16);                              // dsll a3, a3, 16
  c->vftoi0(DEST::xyzw, vf3, vf3);                  // vftoi0.xyzw vf3, vf3
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L63
  c->vftoi0(DEST::xyzw, vf4, vf4);                  // vftoi0.xyzw vf4, vf4
  if (bc) {goto block_22;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lqc2(vf8, 208, a2);                            // lqc2 vf8, 208(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf9, 224, a2);                            // lqc2 vf9, 224(a2)
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t0, vf3);                        // qmfc2.i t0, vf3
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a3, vf4);                        // qmfc2.i a3, vf4
  c->pmaxw(t0, t0, r0);                             // pmaxw t0, t0, r0
  c->lqc2(vf6, 28, a1);                             // lqc2 vf6, 28(a1)
  c->pmaxw(a3, a3, r0);                             // pmaxw a3, a3, r0
  c->lqc2(vf7, 240, a2);                            // lqc2 vf7, 240(a2)
  c->pminw(t0, t0, v1);                             // pminw t0, t0, v1
  c->vmax_bc(DEST::xyzw, BC::w, vf1, vf0, vf0);     // vmaxw.xyzw vf1, vf0, vf0
  c->pminw(v1, a3, v1);                             // pminw v1, a3, v1
  c->lqc2(vf23, 448, a2);                           // lqc2 vf23, 448(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf24, 464, a2);                           // lqc2 vf24, 464(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf25, 480, a2);                           // lqc2 vf25, 480(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf26, 496, a2);                           // lqc2 vf26, 496(a2)
  // nop                                            // sll r0, r0, 0
  c->sq(t0, 400, a2);                               // sq t0, 400(a2)
  // nop                                            // sll r0, r0, 0
  c->sq(v1, 416, a2);                               // sq v1, 416(a2)
  c->vsub(DEST::xyzw, vf8, vf8, vf2);               // vsub.xyzw vf8, vf8, vf2
  // nop                                            // sll r0, r0, 0
  c->addiu(t8, r0, 4);                              // addiu t8, r0, 4
  c->lbu(v1, 40, a1);                               // lbu v1, 40(a1)
  c->multu3(t9, v1, t8);                            // multu3 t9, v1, t8
  c->lbu(v1, 42, a1);                               // lbu v1, 42(a1)
  c->multu3(t6, v1, t9);                            // multu3 t6, v1, t9
  c->addiu(v1, r0, 1);                              // addiu v1, r0, 1
  c->lw(a3, 400, a2);                               // lw a3, 400(a2)
  c->dsubu(v1, v1, a3);                             // dsubu v1, v1, a3
  c->lw(a3, 416, a2);                               // lw a3, 416(a2)
  c->daddu(v1, v1, a3);                             // daddu v1, v1, a3
  c->addiu(a3, r0, 1);                              // addiu a3, r0, 1
  c->lw(t0, 404, a2);                               // lw t0, 404(a2)
  c->dsubu(a3, a3, t0);                             // dsubu a3, a3, t0
  c->lw(t0, 420, a2);                               // lw t0, 420(a2)
  c->daddu(a3, a3, t0);                             // daddu a3, a3, t0
  c->addiu(t0, r0, 1);                              // addiu t0, r0, 1
  c->lw(t1, 408, a2);                               // lw t1, 408(a2)
  c->dsubu(t0, t0, t1);                             // dsubu t0, t0, t1
  c->lw(t1, 424, a2);                               // lw t1, 424(a2)
  c->daddu(t0, t0, t1);                             // daddu t0, t0, t1
  c->lwu(t1, 8, a1);                                // lwu t1, 8(a1)
  c->lw(t2, 400, a2);                               // lw t2, 400(a2)
  c->mult3(t2, t2, t8);                             // mult3 t2, t2, t8
  c->lw(t3, 404, a2);                               // lw t3, 404(a2)
  c->mult3(t3, t3, t6);                             // mult3 t3, t3, t6
  c->daddu(t2, t2, t3);                             // daddu t2, t2, t3
  c->lw(t3, 408, a2);                               // lw t3, 408(a2)
  c->mult3(t3, t3, t9);                             // mult3 t3, t3, t9
  c->daddu(t2, t2, t3);                             // daddu t2, t2, t3
  c->daddu(t7, t1, t2);                             // daddu t7, t1, t2
  c->mov64(t1, a3);                                 // or t1, a3, r0
  // nop                                            // sll r0, r0, 0

  block_2:
  c->mov64(t2, t0);                                 // or t2, t0, r0
  c->mov64(ra, t7);                                 // or ra, t7, r0
  // nop                                            // sll r0, r0, 0
  c->sw(t7, 524, a2);                               // sw t7, 524(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(t6, 528, a2);                               // sw t6, 528(a2)

  block_3:
  c->mov64(t6, v1);                                 // or t6, v1, r0
  c->mov64(t7, ra);                                 // or t7, ra, r0
  // nop                                            // sll r0, r0, 0
  c->sw(ra, 532, a2);                               // sw ra, 532(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(t9, 536, a2);                               // sw t9, 536(a2)

  block_4:
  // nop                                            // sll r0, r0, 0
  c->sw(t8, 520, a2);                               // sw t8, 520(a2)
  c->dsubu(s5, v1, t6);                             // dsubu s5, v1, t6
  c->lw(s4, 400, a2);                               // lw s4, 400(a2)
  c->dsubu(ra, a3, t1);                             // dsubu ra, a3, t1
  c->lw(gp, 404, a2);                               // lw gp, 404(a2)
  c->dsubu(t8, t0, t2);                             // dsubu t8, t0, t2
  c->lw(t9, 408, a2);                               // lw t9, 408(a2)
  c->daddu(s5, s5, s4);                             // daddu s5, s5, s4
  c->sw(r0, 444, a2);                               // sw r0, 444(a2)
  c->daddu(ra, ra, gp);                             // daddu ra, ra, gp
  c->sw(s5, 432, a2);                               // sw s5, 432(a2)
  c->daddu(t8, t8, t9);                             // daddu t8, t8, t9
  c->sw(ra, 436, a2);                               // sw ra, 436(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(t8, 440, a2);                               // sw t8, 440(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 432, a2);                            // lqc2 vf3, 432(a2)
  c->vitof0(DEST::xyzw, vf3, vf3);                  // vitof0.xyzw vf3, vf3
  // nop                                            // sll r0, r0, 0
  c->vmula(DEST::xyzw,  vf3, vf6);                  // vmula.xyzw acc, vf3, vf6
  // nop                                            // sll r0, r0, 0
  c->vmsub_bc(DEST::xyzw, BC::w, vf3, vf1, vf10);   // vmsubw.xyzw vf3, vf1, vf10
  // nop                                            // sll r0, r0, 0
  c->vmadda(DEST::xyzw,  vf1, vf6);                 // vmadda.xyzw acc, vf1, vf6
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::w, vf4, vf1, vf10);   // vmaddw.xyzw vf4, vf1, vf10
  // nop                                            // sll r0, r0, 0
  c->vmula(DEST::xyzw,  vf7, vf3);                  // vmula.xyzw acc, vf7, vf3
  // nop                                            // sll r0, r0, 0
  c->vmsuba(DEST::xyzw,  vf7, vf8);                 // vmsuba.xyzw acc, vf7, vf8
  // nop                                            // sll r0, r0, 0
  c->vmsub(DEST::xyzw, vf13, vf1, vf10);            // vmsub.xyzw vf13, vf1, vf10
  // nop                                            // sll r0, r0, 0
  c->vmadd(DEST::xyzw, vf15, vf1, vf11);            // vmadd.xyzw vf15, vf1, vf11
  // nop                                            // sll r0, r0, 0
  c->vmula(DEST::xyzw,  vf7, vf4);                  // vmula.xyzw acc, vf7, vf4
  // nop                                            // sll r0, r0, 0
  c->vmsuba(DEST::xyzw,  vf7, vf8);                 // vmsuba.xyzw acc, vf7, vf8
  // nop                                            // sll r0, r0, 0
  c->vmsub(DEST::xyzw, vf14, vf1, vf11);            // vmsub.xyzw vf14, vf1, vf11
  // nop                                            // sll r0, r0, 0
  c->vmadd(DEST::xyzw, vf16, vf1, vf10);            // vmadd.xyzw vf16, vf1, vf10
  // nop                                            // sll r0, r0, 0
  c->vmax(DEST::xyzw, vf13, vf13, vf14);            // vmax.xyzw vf13, vf13, vf14
  // nop                                            // sll r0, r0, 0
  c->vmini(DEST::xyzw, vf15, vf15, vf16);           // vmini.xyzw vf15, vf15, vf16
  // nop                                            // sll r0, r0, 0
  c->vmax_bc(DEST::xyzw, BC::y, vf13, vf13, vf13);  // vmaxy.xyzw vf13, vf13, vf13
  // nop                                            // sll r0, r0, 0
  c->vmini_bc(DEST::xyzw, BC::y, vf15, vf15, vf15); // vminiy.xyzw vf15, vf15, vf15
  // nop                                            // sll r0, r0, 0
  c->vmax_bc(DEST::xyzw, BC::z, vf13, vf13, vf13);  // vmaxz.xyzw vf13, vf13, vf13
  // nop                                            // sll r0, r0, 0
  c->vmini_bc(DEST::xyzw, BC::z, vf15, vf15, vf15); // vminiz.xyzw vf15, vf15, vf15
  // nop                                            // sll r0, r0, 0
  c->vftoi12(DEST::xyzw, vf13, vf13);               // vftoi12.xyzw vf13, vf13
  // nop                                            // sll r0, r0, 0
  c->vftoi12(DEST::xyzw, vf15, vf15);               // vftoi12.xyzw vf15, vf15
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(ra, vf13);                       // qmfc2.i ra, vf13
  c->addiu(gp, r0, 4096);                           // addiu gp, r0, 4096
  c->mov128_gpr_vf(t8, vf15);                       // qmfc2.i t8, vf15
  c->subu(t9, t8, ra);                              // subu t9, t8, ra
  c->subu(ra, gp, ra);                              // subu ra, gp, ra
  bc = ((s64)c->sgpr64(t9)) < 0;                    // bltz t9, L62
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_18;}                          // branch non-likely

  bc = ((s64)c->sgpr64(ra)) < 0;                    // bltz ra, L62
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_18;}                          // branch non-likely

  bc = ((s64)c->sgpr64(t8)) < 0;                    // bltz t8, L62
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_18;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lhu(ra, 0, t7);                                // lhu ra, 0(t7)
  // nop                                            // sll r0, r0, 0
  c->lw(gp, 104, a1);                               // lw gp, 104(a1)
  // nop                                            // sll r0, r0, 0
  c->lhu(t8, 2, t7);                                // lhu t8, 2(t7)
  // nop                                            // sll r0, r0, 0
  c->lw(t9, 72, a1);                                // lw t9, 72(a1)
  bc = c->sgpr64(t8) == 0;                          // beq t8, r0, L62
  c->daddu(ra, ra, gp);                             // daddu ra, ra, gp
  if (bc) {goto block_18;}                          // branch non-likely


  block_8:
  get_fake_spad_addr2(gp, cache.fake_scratchpad_data, 0, c);// lui gp, 28672
  c->lbu(s4, 0, ra);                                // lbu s4, 0(ra)
  c->addiu(s3, r0, 1);                              // addiu s3, r0, 1
  c->andi(s2, s4, 7);                               // andi s2, s4, 7
  c->sra(s5, s4, 3);                                // sra s5, s4, 3
  c->sll(s4, s4, 2);                                // sll s4, s4, 2
  c->daddu(s5, s5, gp);                             // daddu s5, s5, gp
  c->daddu(s4, s4, t9);                             // daddu s4, s4, t9
  c->sllv(s3, s3, s2);                              // sllv s3, s3, s2
  c->lb(s2, 0, s5);                                 // lb s2, 0(s5)
  // nop                                            // sll r0, r0, 0
  c->lbu(s0, 3, s4);                                // lbu s0, 3(s4)
  c->and_(s1, s2, s3);                              // and s1, s2, s3
  c->daddiu(t8, t8, -1);                            // daddiu t8, t8, -1
  bc = c->sgpr64(s1) != 0;                          // bne s1, r0, L61
  c->lw(s1, 4, a1);                                 // lw s1, 4(a1)
  if (bc) {goto block_17;}                          // branch non-likely

  c->dsll(s0, s0, 2);                               // dsll s0, s0, 2
  c->or_(s3, s2, s3);                               // or s3, s2, s3
  c->daddu(s2, s0, s1);                             // daddu s2, s0, s1
  c->sb(s3, 0, s5);                                 // sb s3, 0(s5)
  // nop                                            // sll r0, r0, 0
  c->lwu(s5, 0, s2);                                // lwu s5, 0(s2)
  // nop                                            // sll r0, r0, 0
  c->lwu(s2, 96, a2);                               // lwu s2, 96(a2)
  // nop                                            // sll r0, r0, 0
  c->lw(s3, 88, a1);                                // lw s3, 88(a1)
  c->and_(s2, s2, s5);                              // and s2, s2, s5
  c->lbu(s1, 0, s4);                                // lbu s1, 0(s4)
  bc = c->sgpr64(s2) != 0;                          // bne s2, r0, L61
  c->lbu(s2, 1, s4);                                // lbu s2, 1(s4)
  if (bc) {goto block_17;}                          // branch non-likely

  c->sll(s1, s1, 1);                                // sll s1, s1, 1
  c->lbu(s4, 2, s4);                                // lbu s4, 2(s4)
  c->sll(s0, s1, 1);                                // sll s0, s1, 1
  c->sll(s2, s2, 1);                                // sll s2, s2, 1
  c->daddu(s1, s1, s0);                             // daddu s1, s1, s0
  c->sll(s0, s2, 1);                                // sll s0, s2, 1
  c->sll(s4, s4, 1);                                // sll s4, s4, 1
  c->daddu(s0, s2, s0);                             // daddu s0, s2, s0
  c->sll(v0, s4, 1);                                // sll v0, s4, 1
  c->daddu(s2, s1, s3);                             // daddu s2, s1, s3
  c->daddu(s1, s4, v0);                             // daddu s1, s4, v0
  c->daddu(s4, s0, s3);                             // daddu s4, s0, s3
  c->daddu(s3, s1, s3);                             // daddu s3, s1, s3
  c->ldr(t3, 0, s2);                                // ldr t3, 0(s2)
  // nop                                            // sll r0, r0, 0
  c->ldl(t3, 7, s2);                                // ldl t3, 7(s2)
  // nop                                            // sll r0, r0, 0
  c->ldr(t4, 0, s4);                                // ldr t4, 0(s4)
  c->pextlh(t3, r0, t3);                            // pextlh t3, r0, t3
  c->ldl(t4, 7, s4);                                // ldl t4, 7(s4)
  c->psllw(t3, t3, 4);                              // psllw t3, t3, 4
  c->ldr(t5, 0, s3);                                // ldr t5, 0(s3)
  c->pextlh(t4, r0, t4);                            // pextlh t4, r0, t4
  c->ldl(t5, 7, s3);                                // ldl t5, 7(s3)
  c->psllw(t4, t4, 4);                              // psllw t4, t4, 4
  c->mov128_vf_gpr(vf17, t3);                       // qmtc2.i vf17, t3
  c->pextlh(t5, r0, t5);                            // pextlh t5, r0, t5
  c->mov128_vf_gpr(vf18, t4);                       // qmtc2.i vf18, t4
  c->psllw(t5, t5, 4);                              // psllw t5, t5, 4
  c->lw(s4, 2048, gp);                              // lw s4, 2048(gp)
  c->mov128_vf_gpr(vf19, t5);                       // qmtc2.i vf19, t5
  // nop                                            // sll r0, r0, 0
  c->vitof0(DEST::xyzw, vf17, vf17);                // vitof0.xyzw vf17, vf17
  c->daddiu(s4, s4, 1);                             // daddiu s4, s4, 1
  c->vitof0(DEST::xyzw, vf18, vf18);                // vitof0.xyzw vf18, vf18
  c->sw(s4, 2048, gp);                              // sw s4, 2048(gp)
  c->vitof0(DEST::xyzw, vf19, vf19);                // vitof0.xyzw vf19, vf19
  // nop                                            // sll r0, r0, 0
  c->vmini(DEST::xyzw, vf3, vf17, vf18);            // vmini.xyzw vf3, vf17, vf18
  // nop                                            // sll r0, r0, 0
  c->vmax(DEST::xyzw, vf4, vf17, vf18);             // vmax.xyzw vf4, vf17, vf18
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyzw, vf17, vf17, vf2);             // vadd.xyzw vf17, vf17, vf2
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyzw, vf18, vf18, vf2);             // vadd.xyzw vf18, vf18, vf2
  // nop                                            // sll r0, r0, 0
  c->vmini(DEST::xyzw, vf3, vf3, vf19);             // vmini.xyzw vf3, vf3, vf19
  // nop                                            // sll r0, r0, 0
  c->vmax(DEST::xyzw, vf4, vf4, vf19);              // vmax.xyzw vf4, vf4, vf19
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyzw, vf19, vf19, vf2);             // vadd.xyzw vf19, vf19, vf2
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->vsub_bc(DEST::xyzw, BC::w, vf3, vf3, vf10);    // vsubw.xyzw vf3, vf3, vf10
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::xyzw, BC::w, vf4, vf4, vf10);    // vaddw.xyzw vf4, vf4, vf10
  // nop                                            // sll r0, r0, 0
  c->vmula(DEST::xyzw,  vf7, vf3);                  // vmula.xyzw acc, vf7, vf3
  // nop                                            // sll r0, r0, 0
  c->vmsuba(DEST::xyzw,  vf7, vf8);                 // vmsuba.xyzw acc, vf7, vf8
  // nop                                            // sll r0, r0, 0
  c->vmsub(DEST::xyzw, vf13, vf1, vf10);            // vmsub.xyzw vf13, vf1, vf10
  // nop                                            // sll r0, r0, 0
  c->vmadd(DEST::xyzw, vf15, vf1, vf11);            // vmadd.xyzw vf15, vf1, vf11
  // nop                                            // sll r0, r0, 0
  c->vmula(DEST::xyzw,  vf7, vf4);                  // vmula.xyzw acc, vf7, vf4
  // nop                                            // sll r0, r0, 0
  c->vmsuba(DEST::xyzw,  vf7, vf8);                 // vmsuba.xyzw acc, vf7, vf8
  // nop                                            // sll r0, r0, 0
  c->vmsub(DEST::xyzw, vf14, vf1, vf11);            // vmsub.xyzw vf14, vf1, vf11
  // nop                                            // sll r0, r0, 0
  c->vmadd(DEST::xyzw, vf16, vf1, vf10);            // vmadd.xyzw vf16, vf1, vf10
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::x, vf23, vf17);       // vmulax.xyzw acc, vf23, vf17
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf24, vf17);      // vmadday.xyzw acc, vf24, vf17
  // nop                                            // sll r0, r0, 0
  c->vmax(DEST::xyzw, vf13, vf13, vf14);            // vmax.xyzw vf13, vf13, vf14
  // nop                                            // sll r0, r0, 0
  c->vmini(DEST::xyzw, vf15, vf15, vf16);           // vmini.xyzw vf15, vf15, vf16
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::z, vf25, vf17);      // vmaddaz.xyzw acc, vf25, vf17
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::w, vf20, vf26, vf0);  // vmaddw.xyzw vf20, vf26, vf0
  // nop                                            // sll r0, r0, 0
  c->vmax_bc(DEST::xyzw, BC::y, vf13, vf13, vf13);  // vmaxy.xyzw vf13, vf13, vf13
  // nop                                            // sll r0, r0, 0
  c->vmini_bc(DEST::xyzw, BC::y, vf15, vf15, vf15); // vminiy.xyzw vf15, vf15, vf15
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::x, vf23, vf18);       // vmulax.xyzw acc, vf23, vf18
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf24, vf18);      // vmadday.xyzw acc, vf24, vf18
  // nop                                            // sll r0, r0, 0
  c->vmax_bc(DEST::xyzw, BC::z, vf13, vf13, vf13);  // vmaxz.xyzw vf13, vf13, vf13
  // nop                                            // sll r0, r0, 0
  c->vmini_bc(DEST::xyzw, BC::z, vf15, vf15, vf15); // vminiz.xyzw vf15, vf15, vf15
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::z, vf25, vf18);      // vmaddaz.xyzw acc, vf25, vf18
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::w, vf21, vf26, vf0);  // vmaddw.xyzw vf21, vf26, vf0
  // nop                                            // sll r0, r0, 0
  c->vftoi12(DEST::xyzw, vf13, vf13);               // vftoi12.xyzw vf13, vf13
  // nop                                            // sll r0, r0, 0
  c->vftoi12(DEST::xyzw, vf15, vf15);               // vftoi12.xyzw vf15, vf15
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::x, vf23, vf19);       // vmulax.xyzw acc, vf23, vf19
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf24, vf19);      // vmadday.xyzw acc, vf24, vf19
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(s3, vf13);                       // qmfc2.i s3, vf13
  c->addiu(s2, r0, 4096);                           // addiu s2, r0, 4096
  c->mov128_gpr_vf(s4, vf15);                       // qmfc2.i s4, vf15
  c->subu(gp, s4, s3);                              // subu gp, s4, s3
  c->subu(s3, s2, s3);                              // subu s3, s2, s3
  bc = ((s64)c->sgpr64(gp)) < 0;                    // bltz gp, L61
  c->lwu(gp, 512, a2);                              // lwu gp, 512(a2)
  if (bc) {goto block_17;}                          // branch non-likely

  bc = ((s64)c->sgpr64(s3)) < 0;                    // bltz s3, L61
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_17;}                          // branch non-likely

  bc = ((s64)c->sgpr64(s4)) < 0;                    // bltz s4, L61
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_17;}                          // branch non-likely

  c->pextlw(s5, gp, s5);                            // pextlw s5, gp, s5
  c->lw(s3, 0, a0);                                 // lw s3, 0(a0)
  // nop                                            // sll r0, r0, 0
  c->daddiu(s4, a0, 4908);                          // daddiu s4, a0, 4908
  c->daddiu(s2, s3, -460);                          // daddiu s2, s3, -460
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(s2)) >= 0;                   // bgez s2, L64
  c->dsll(s2, s3, 6);                               // dsll s2, s3, 6
  if (bc) {goto block_23;}                          // branch non-likely

  c->daddiu(s3, s3, 1);                             // daddiu s3, s3, 1
  c->daddu(s4, s4, s2);                             // daddu s4, s4, s2
  bc = c->sgpr64(gp) == c->sgpr64(s7);              // beq gp, s7, L60
  c->sw(s3, 0, a0);                                 // sw s3, 0(a0)
  if (bc) {goto block_16;}                          // branch non-likely

  c->vmadda_bc(DEST::xyzw, BC::z, vf25, vf19);      // vmaddaz.xyzw acc, vf25, vf19
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::w, vf22, vf26, vf0);  // vmaddw.xyzw vf22, vf26, vf0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sq(s5, 48, s4);                                // sq s5, 48(s4)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf20, 0, s4);                             // sqc2 vf20, 0(s4)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf21, 16, s4);                            // sqc2 vf21, 16(s4)
  //beq r0, r0, L61                                 // beq r0, r0, L61
  c->sqc2(vf22, 32, s4);                            // sqc2 vf22, 32(s4)
  goto block_17;                                    // branch always


  block_16:
  // nop                                            // sll r0, r0, 0
  c->sq(s5, 48, s4);                                // sq s5, 48(s4)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf17, 0, s4);                             // sqc2 vf17, 0(s4)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf18, 16, s4);                            // sqc2 vf18, 16(s4)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf19, 32, s4);                            // sqc2 vf19, 32(s4)

  block_17:
  bc = ((s64)c->sgpr64(t8)) > 0;                    // bgtz t8, L59
  c->daddiu(ra, ra, 1);                             // daddiu ra, ra, 1
  if (bc) {goto block_8;}                           // branch non-likely


  block_18:
  // nop                                            // sll r0, r0, 0
  c->lw(t8, 520, a2);                               // lw t8, 520(a2)
  c->daddiu(t6, t6, -1);                            // daddiu t6, t6, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t6) != 0;                          // bne t6, r0, L58
  c->daddu(t7, t7, t8);                             // daddu t7, t7, t8
  if (bc) {goto block_4;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(t6, 532, a2);                               // lw t6, 532(a2)
  // nop                                            // sll r0, r0, 0
  c->lw(t9, 536, a2);                               // lw t9, 536(a2)
  c->daddiu(t2, t2, -1);                            // daddiu t2, t2, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t2) != 0;                          // bne t2, r0, L57
  c->daddu(ra, t6, t9);                             // daddu ra, t6, t9
  if (bc) {goto block_3;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(t2, 524, a2);                               // lw t2, 524(a2)
  // nop                                            // sll r0, r0, 0
  c->lw(t6, 528, a2);                               // lw t6, 528(a2)
  c->daddiu(t1, t1, -1);                            // daddiu t1, t1, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L56
  c->daddu(t7, t2, t6);                             // daddu t7, t2, t6
  if (bc) {goto block_2;}                           // branch non-likely

  c->load_symbol2(v1, cache.collide_stats);         // lw v1, *collide-stats*(s7)
  c->lwu(v1, 12, v1);                               // lwu v1, 12(v1)
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  c->load_symbol2(a0, cache.collide_stats);         // lw a0, *collide-stats*(s7)
  c->sw(v1, 12, a0);                                // sw v1, 12(a0)

  block_22:
  c->mov64(v0, s7);                                 // or v0, s7, r0
  //beq r0, r0, L66                                 // beq r0, r0, L66
  // nop                                            // sll r0, r0, 0
  goto block_26;                                    // branch always


  block_23:
  c->load_symbol_addr(v1, cache.debug);             // daddiu v1, s7, debug
  c->load_symbol2(a0, cache.cheat_mode);            // lw a0, *cheat-mode*(s7)
  bc = c->sgpr64(a0) != c->sgpr64(v1);              // bne a0, v1, L65
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_25;}                          // branch non-likely

  c->load_symbol2(t9, cache.print_exceeded_max_cache_tris);// lw t9, print-exceeded-max-cache-tris(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0

  block_25:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0

  block_26:
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
  cache.cheat_mode = intern_from_c("*cheat-mode*").c();
  cache.collide_stats = intern_from_c("*collide-stats*").c();
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  cache.debug = intern_from_c("debug").c();
  cache.print_exceeded_max_cache_tris = intern_from_c("print-exceeded-max-cache-tris").c();
  gLinkedFunctionTable.reg("fill-bg-using-line-sphere-new", execute, 256);
}

} // namespace fill_bg_using_line_sphere_new
} // namespace Mips2C

