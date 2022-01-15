
//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/kscheme.h"
namespace Mips2C {
namespace method_16_collide_edge_work {
struct Cache {
  void* format;  // format
} cache;

// clang-format off
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -32);                           // daddiu sp, sp, -32
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  c->sq(gp, 16, sp);                                // sq gp, 16(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->fprs[f0] = 0.707;                              // lwc1 f0, L95(fp)
  c->addiu(v1, r0, 5);                              // addiu v1, r0, 5
  // nop                                            // sll r0, r0, 0
  c->addiu(a0, r0, 56);                             // addiu a0, r0, 56
  c->lwu(t3, 0, gp);                                // lwu t3, 0(gp)
  c->addiu(a1, r0, 0);                              // addiu a1, r0, 0
  c->daddiu(a2, gp, 6272);                          // daddiu a2, gp, 6272
  c->addiu(a3, r0, 16);                             // addiu a3, r0, 16
  c->lwu(t0, 0, t3);                                // lwu t0, 0(t3)
  c->gprs[t1].du64[0] = 0;                          // or t1, r0, r0
  c->lq(t2, 96, gp);                                // lq t2, 96(gp)
  c->daddiu(t3, t3, 4908);                          // daddiu t3, t3, 4908
  c->lq(t4, 112, gp);                               // lq t4, 112(gp)

  block_1:
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L30
  c->lwu(t5, 48, t3);                               // lwu t5, 48(t3)
  if (bc) {goto block_14;}                          // branch non-likely

  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  c->lqc2(vf1, 0, t3);                              // lqc2 vf1, 0(t3)
  c->and_(t6, t5, a0);                              // and t6, t5, a0
  c->lqc2(vf2, 16, t3);                             // lqc2 vf2, 16(t3)
  bc = c->sgpr64(t6) == c->sgpr64(a1);              // beq t6, a1, L29
  c->lqc2(vf3, 32, t3);                             // lqc2 vf3, 32(t3)
  if (bc) {goto block_5;}                           // branch non-likely

  if (((s64)c->sgpr64(t6)) != ((s64)c->sgpr64(a3))) {// bnel t6, a3, L28
    c->daddiu(t3, t3, 64);                          // daddiu t3, t3, 64
    goto block_1;
  }

  block_5:
  c->and_(t6, t5, v1);                              // and t6, t5, v1
  c->vmini(DEST::xyzw, vf7, vf1, vf2);              // vmini.xyzw vf7, vf1, vf2
  bc = c->sgpr64(t6) != 0;                          // bne t6, r0, L28
  c->vmax(DEST::xyzw, vf8, vf1, vf2);               // vmax.xyzw vf8, vf1, vf2
  if (bc) {goto block_1;}                           // branch non-likely

  c->vsub(DEST::xyz, vf4, vf2, vf1);                // vsub.xyz vf4, vf2, vf1
  // nop                                            // sll r0, r0, 0
  c->vsub(DEST::xyz, vf5, vf3, vf1);                // vsub.xyz vf5, vf3, vf1
  // nop                                            // sll r0, r0, 0
  c->vmini(DEST::xyzw, vf7, vf7, vf3);              // vmini.xyzw vf7, vf7, vf3
  // nop                                            // sll r0, r0, 0
  c->vmax(DEST::xyzw, vf8, vf8, vf3);               // vmax.xyzw vf8, vf8, vf3
  // nop                                            // sll r0, r0, 0
  c->vopmula(vf4, vf5);                             // vopmula.xyz acc, vf4, vf5
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf7, vf7);                  // vftoi0.xyzw vf7, vf7
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf8, vf8);                  // vftoi0.xyzw vf8, vf8
  // nop                                            // sll r0, r0, 0
  c->vopmsub(vf6, vf5, vf4);                        // vopmsub.xyz vf6, vf5, vf4
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t7, vf7);                        // qmfc2.i t7, vf7
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t6, vf8);                        // qmfc2.i t6, vf8
  // nop                                            // sll r0, r0, 0
  c->pcgtw(t7, t7, t4);                             // pcgtw t7, t7, t4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->vmul(DEST::xyzw, vf9, vf6, vf6);               // vmul.xyzw vf9, vf6, vf6
  // nop                                            // sll r0, r0, 0
  c->pcgtw(t6, t2, t6);                             // pcgtw t6, t2, t6
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->por(t6, t7, t6);                               // por t6, t7, t6
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppach(t6, r0, t6);                             // ppach t6, r0, t6
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsll(t6, t6, 16);                              // dsll t6, t6, 16
  // nop                                            // sll r0, r0, 0
  if (((s64)c->sgpr64(t6)) != ((s64)0)) {           // bnel t6, r0, L28
    c->daddiu(t3, t3, 64);                          // daddiu t3, t3, 64
    goto block_1;
  }

  // block_8:
  c->vmula_bc(DEST::w, BC::x, vf0, vf9);            // vmulax.w acc, vf0, vf9
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::w, BC::y, vf0, vf9);           // vmadday.w acc, vf0, vf9
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::w, BC::z, vf9, vf0, vf9);       // vmaddz.w vf9, vf0, vf9
  // nop                                            // sll r0, r0, 0
  c->vrsqrt(vf0, BC::w, vf9, BC::w);                // vrsqrt Q, vf0.w, vf9.w
  // nop                                            // sll r0, r0, 0
  c->dsll32(t5, t5, 12);                            // dsll32 t5, t5, 12
  c->dsrl32(t5, t5, 26);                            // dsrl32 t5, t5, 26
  c->addiu(t6, r0, 2);                              // addiu t6, r0, 2
  // nop                                            // sll r0, r0, 0
  c->vwaitq();                                      // vwaitq
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf6, vf6);                    // vmulq.xyz vf6, vf6, Q
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t7, vf6);                        // qmfc2.i t7, vf6
  // nop                                            // sll r0, r0, 0
  c->dsra32(t7, t7, 0);                             // dsra32 t7, t7, 0
  // nop                                            // sll r0, r0, 0
  c->mtc1(f1, t7);                                  // mtc1 f1, t7
  // nop                                            // sll r0, r0, 0
  cop1_bc = c->fprs[f1] < c->fprs[f0];              // c.lt.s f1, f0
  c->addiu(t7, r0, 48);                             // addiu t7, r0, 48
  if (cop1_bc) {                                    // bc1tl L28
    c->daddiu(t3, t3, 64);                          // daddiu t3, t3, 64
    goto block_1;
  }

  // block_10:
  if (((s64)c->sgpr64(t5)) == ((s64)c->sgpr64(t6))) {// beql t5, t6, L28
    c->daddiu(t3, t3, 64);                          // daddiu t3, t3, 64
    goto block_1;
  }

  // block_12:
  bc = c->sgpr64(t1) == c->sgpr64(t7);              // beq t1, t7, L31
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_15;}                          // branch non-likely

  c->daddiu(t1, t1, 1);                             // daddiu t1, t1, 1
  c->sw(t3, 0, a2);                                 // sw t3, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf6, 16, a2);                             // sqc2 vf6, 16(a2)
  c->daddiu(a2, a2, 32);                            // daddiu a2, a2, 32
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L28                                 // beq r0, r0, L28
  c->daddiu(t3, t3, 64);                            // daddiu t3, t3, 64
  goto block_1;                                     // branch always


  block_14:
  //beq r0, r0, L32                                 // beq r0, r0, L32
  c->sw(t1, 16, gp);                                // sw t1, 16(gp)
  goto block_16;                                    // branch always


  block_15:
  c->load_symbol(t9, cache.format);                 // lw t9, format(s7)
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  //daddiu a1, fp, L88                                // daddiu a1, fp, L88
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  //c->jalr(call_addr);                               // jalr ra, t9
  printf("ERROR: Exceeded max # of grabbable tris\n");
  c->addiu(v1, r0, 48);                             // addiu v1, r0, 48
  c->sw(v1, 16, gp);                                // sw v1, 16(gp)

  block_16:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
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
  cache.format = intern_from_c("format").c();
  gLinkedFunctionTable.reg("(method 16 collide-edge-work)", execute, 128);
}

} // namespace method_16_collide_edge_work
} // namespace Mips2C

