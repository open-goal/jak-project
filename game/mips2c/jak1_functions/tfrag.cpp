
//--------------------------MIPS2C---------------------
#include "game/kernel/jak1/kscheme.h"
#include "game/mips2c/mips2c_private.h"
using namespace jak1;
namespace Mips2C::jak1 {
namespace draw_inline_array_tfrag {
struct Cache {
  void* tfrag_work;            // *tfrag-work*
  void* fake_scratchpad_data;  // *fake-scratchpad-data*
  void* transform_regs;
} cache;

// t0 = tfrag work
// t8 = tfrags
// t9 = tfrag count
// clang-format off
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  load_vfs_from_tf_regs(cache.transform_regs, c);
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
  c->lui(t2, 5120);                                 // lui t2, 5120    = (0x14000000)
  c->lw(v1, 4, a3);                                 // lw v1, 4(a3)
  c->lui(t3, 4096);                                 // lui t3, 4096    = (0x10000000)
  c->lui(t1, 4096);                                 // lui t1, 4096    = (0x10000000)
  // Unknown instr: sync.l
  // Unknown instr: cache dxwbin v1, 0
  // Unknown instr: sync.l
  // Unknown instr: cache dxwbin v1, 1
  // Unknown instr: sync.l
  c->load_symbol(t0, cache.tfrag_work);             // lw t0, *tfrag-work*(s7)
  c->ori(t4, t3, 54272);                            // ori t4, t3, 54272 = (0x1000D400) SPR TO
  c->ori(t1, t1, 53248);                            // ori t1, t1, 53248 = (0x1000D000) SPR FROM

  // patched access to scratchpad
  // c->lui(t5, 28672);                                // lui t5, 28672     = (0x70000000)
  get_fake_spad_addr(t5, cache.fake_scratchpad_data, 0, c);

  c->lqc2(vf3, 80, t0);                             // lqc2 vf3, 80(t0)
  c->sw(a3, 176, t0);                               // sw a3, 176(t0)
  c->ori(a3, t5, 2064);                             // ori a3, t5, 2064
  c->addiu(t3, r0, 0);                              // addiu t3, r0, 0
  c->ori(t5, t5, 1040);                             // ori t5, t5, 1040
  c->vmax_bc(DEST::xyzw, BC::w, vf1, vf0, vf0);     // vmaxw.xyzw vf1, vf0, vf0
  c->lh(t7, 0, a0);                                 // lh t7, 0(a0)
  c->lqc2(vf4, 96, t0);                             // lqc2 vf4, 96(t0)
  c->addiu(a1, a1, -4);                             // addiu a1, a1, -4
  c->addiu(t6, r0, 0);                              // addiu t6, r0, 0
  c->mov64(ra, a3);                                 // or ra, a3, r0

  block_1:
  bc = c->sgpr64(t7) != 0;                          // bne t7, r0, L42
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_4;}                           // branch non-likely

  c->addiu(a0, a0, 2);                              // addiu a0, a0, 2
  c->addiu(a1, a1, 1024);                           // addiu a1, a1, 1024
  c->daddiu(a2, a2, -16);                           // daddiu a2, a2, -16
  c->lh(t7, 0, a0);                                 // lh t7, 0(a0)
  bc = ((s64)c->sgpr64(a2)) <= 0;                   // blez a2, L69
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_55;}                          // branch non-likely

  //beq r0, r0, L41                                 // beq r0, r0, L41
  // nop                                            // sll r0, r0, 0
  goto block_1;                                     // branch always


  block_4:

  // this block is just waiting for any in-progress SPR TO's to end
  // we can just skip it.
  /*
  c->lw(t7, 0, t4);                                 // lw t7, 0(t4)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(t7, t7, 256);                             // andi t7, t7, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t7) != 0;                          // bne t7, r0, L42
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_4;}                           // branch non-likely
  */

  // this is setting up a scratchpad TO transfer.
  {
    // set MADR
    //c->sw(a1, 16, t4);                                // sw a1, 16(t4)
    u32 madr = c->sgpr64(a1);

    c->xori(t7, t5, 1024);                            // xori t7, t5, 1024

    // set SADR
    //c->sw(t7, 128, t4);                               // sw t7, 128(t4)
    u32 sadr = c->sgpr64(t7);

    c->addiu(t7, r0, 64);                             // addiu t7, r0, 64

    // set QWC
    //c->sw(t7, 32, t4);                                // sw t7, 32(t4)
    u32 qwc = c->sgpr64(t7);

    c->addiu(t7, r0, 256);                            // addiu t7, r0, 256

    // GO!
    //c->sw(t7, 0, t4);                                 // sw t7, 0(t4)
    spad_to_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
    // nop                                            // sll r0, r0, 0
  }


  block_6:
  c->mov64(gp, a0);                                 // or gp, a0, r0
  // fprintf(stderr, "block_6: gp = 0x%lx\n", c->sgpr64(gp));
  c->xori(t5, t5, 1024);                            // xori t5, t5, 1024
  c->daddiu(a0, a0, 2);                             // daddiu a0, a0, 2
  c->mov64(t9, a0);                                 // or t9, a0, r0
  c->mov64(t8, t5);                                 // or t8, t5, r0
  c->daddiu(t7, a2, -16);                           // daddiu t7, a2, -16
  bc = ((s64)c->sgpr64(t7)) > 0;                    // bgtz t7, L45
  c->lh(t7, 0, a0);                                 // lh t7, 0(a0)
  if (bc) {goto block_10;}                          // branch non-likely

  //beq r0, r0, L48                                 // beq r0, r0, L48
  // nop                                            // sll r0, r0, 0
  goto block_14;                                    // branch always


  block_8:
  c->daddiu(a2, a2, -16);                           // daddiu a2, a2, -16
  c->addiu(a0, a0, 2);                              // addiu a0, a0, 2
  bc = ((s64)c->sgpr64(a2)) <= 0;                   // blez a2, L48
  c->lh(t7, 0, a0);                                 // lh t7, 0(a0)
  if (bc) {goto block_14;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0

  block_10:
  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L44
  c->addiu(a1, a1, 1024);                           // addiu a1, a1, 1024
  if (bc) {goto block_8;}                           // branch non-likely


  // this is waiting on spad transfer and incrementing wait counts
  // block_11:
  /*
  c->lw(t7, 0, t4);                                 // lw t7, 0(t4)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(t7, t7, 256);                             // andi t7, t7, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L47
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_13;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(t7, 188, t0);                               // lw t7, 188(t0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(t7, t7, 1);                             // daddiu t7, t7, 1
  // nop                                            // sll r0, r0, 0
  c->sw(t7, 188, t0);                               // sw t7, 188(t0)
  //beq r0, r0, L46                                 // beq r0, r0, L46
  // nop                                            // sll r0, r0, 0
  goto block_11;                                    // branch always
   */


  // tfrag bank loop?
  // block_13:
  {
    //c->sw(a1, 16, t4);                                // sw a1, 16(t4)
    u32 madr = c->sgpr64(a1);
    c->xori(t7, t5, 1024);                            // xori t7, t5, 1024
    //c->sw(t7, 128, t4);                               // sw t7, 128(t4)
    u32 sadr = c->sgpr64(t7);
    c->addiu(t7, r0, 64);                             // addiu t7, r0, 64
    //c->sw(t7, 32, t4);                                // sw t7, 32(t4)
    u32 qwc = c->sgpr64(t7);
    c->addiu(t7, r0, 256);                            // addiu t7, r0, 256
    //beq r0, r0, L49                                 // beq r0, r0, L49
    //c->sw(t7, 0, t4);                                 // sw t7, 0(t4)
    spad_to_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
  }
  goto block_16;                                    // branch always


  block_14:
  /*
  c->lw(t7, 0, t4);                                 // lw t7, 0(t4)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(t7, t7, 256);                             // andi t7, t7, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L49
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_16;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(t7, 188, t0);                               // lw t7, 188(t0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(t7, t7, 1);                             // daddiu t7, t7, 1
  // nop                                            // sll r0, r0, 0
  c->sw(t7, 188, t0);                               // sw t7, 188(t0)
  //beq r0, r0, L48                                 // beq r0, r0, L48
  // nop                                            // sll r0, r0, 0
  goto block_14;                                    // branch always
  */


  block_16:
  c->lb(t7, 0, gp);                                 // lb t7, 0(gp)
  c->addiu(gp, gp, 1);                              // addiu gp, gp, 1
  // nop                                            // sll r0, r0, 0
  c->sw(gp, 160, t0);                               // sw gp, 160(t0)
  bc = c->sgpr64(t7) != 0;                          // bne t7, r0, L50
  c->sw(t9, 164, t0);                               // sw t9, 164(t0)
  if (bc) {goto block_18;}                          // branch non-likely

  c->daddiu(a2, a2, -8);                            // daddiu a2, a2, -8
  c->addiu(t8, t8, 512);                            // addiu t8, t8, 512
  //beq r0, r0, L65                                 // beq r0, r0, L65
  // nop                                            // sll r0, r0, 0
  goto block_47;                                    // branch always


  block_18:
  c->addiu(t9, r0, 128);                            // addiu t9, r0, 128
  c->lqc2(vf2, 16, t8);                             // lqc2 vf2, 16(t8)

  block_19:
  c->daddiu(gp, t6, -124);                          // daddiu gp, t6, -124
  // fprintf(stderr, "block_19: gp = 0x%lx\n", c->sgpr64(gp));
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(gp)) <= 0;                   // blez gp, L54
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_23;}                          // branch non-likely


  // block_20:
  /*
  c->lw(ra, 0, t1);                                 // lw ra, 0(t1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(ra, ra, 256);                             // andi ra, ra, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(ra) == 0;                          // beq ra, r0, L53
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_22;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(ra, 184, t0);                               // lw ra, 184(t0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(ra, ra, 1);                             // daddiu ra, ra, 1
  // nop                                            // sll r0, r0, 0
  c->sw(ra, 184, t0);                               // sw ra, 184(t0)
  //beq r0, r0, L52                                 // beq r0, r0, L52
  // nop                                            // sll r0, r0, 0
  goto block_20;                                    // branch always
   */


  // block_22:
  {
    //c->sw(a3, 128, t1);                               // sw a3, 128(t1)
    u32 sadr = c->sgpr64(a3);
    c->xori(a3, a3, 6144);                            // xori a3, a3, 6144
    //c->sw(v1, 16, t1);                                // sw v1, 16(t1)
    u32 madr = c->sgpr64(v1);
    c->sll(ra, t6, 4);                                // sll ra, t6, 4
    c->addu(v1, v1, ra);                              // addu v1, v1, ra
    c->mov64(ra, a3);                                 // or ra, a3, r0
    //c->sw(t6, 32, t1);                                // sw t6, 32(t1)
    u32 qwc = c->sgpr64(t6);
    c->addiu(t6, r0, 256);                            // addiu t6, r0, 256
    //c->sw(t6, 0, t1);                                 // sw t6, 0(t1)
    spad_from_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
    c->addiu(t6, r0, 0);                              // addiu t6, r0, 0
  }

  block_23:
  c->and_(gp, t7, t9);                              // and gp, t7, t9
  c->vmula_bc(DEST::xyzw, BC::x, vf16, vf2);        // vmulax.xyzw acc, vf16, vf2
  bc = c->sgpr64(gp) == 0;                          // beq gp, r0, L64
  c->lwu(gp, 36, t8);                               // lwu gp, 36(t8)
  if (bc) {goto block_46;}                          // branch non-likely

  c->vmadda_bc(DEST::xyzw, BC::y, vf17, vf2);       // vmadday.xyzw acc, vf17, vf2
  c->lbu(s5, 45, t8);                               // lbu s5, 45(t8)
  c->vmadda_bc(DEST::xyzw, BC::z, vf18, vf2);       // vmaddaz.xyzw acc, vf18, vf2
  c->sw(gp, 4, t0);                                 // sw gp, 4(t0)
  c->vmsuba_bc(DEST::xyzw, BC::w, vf19, vf0);       // vmsubaw.xyzw acc, vf19, vf0
  c->sh(s5, 0, t0);                                 // sh s5, 0(t0)
  c->vmadd_bc(DEST::xyzw, BC::w, vf5, vf1, vf2);    // vmaddw.xyzw vf5, vf1, vf2
  c->lwu(gp, 32, t8);                               // lwu gp, 32(t8)
  // fprintf(stderr, "block_23-0 gp = 0x%lx\n", c->sgpr64(gp));
  c->vmula_bc(DEST::xyzw, BC::w, vf27, vf0);        // vmulaw.xyzw acc, vf27, vf0
  c->lbu(s5, 47, t8);                               // lbu s5, 47(t8)
  c->vmadda_bc(DEST::xyzw, BC::x, vf24, vf2);       // vmaddax.xyzw acc, vf24, vf2
  c->sw(gp, 20, t0);                                // sw gp, 20(t0)
  c->vmadda_bc(DEST::xyzw, BC::y, vf25, vf2);       // vmadday.xyzw acc, vf25, vf2
  c->sh(s5, 16, t0);                                // sh s5, 16(t0)
  c->vmadda_bc(DEST::xyzw, BC::z, vf26, vf2);       // vmaddaz.xyzw acc, vf26, vf2
  c->lwu(gp, 32, t8);                               // lwu gp, 32(t8)
  c->mov128_gpr_vf(s5, vf5);                        // qmfc2.i s5, vf5
  c->lbu(s4, 44, t8);                               // lbu s4, 44(t8)
  c->vmadd_bc(DEST::xyzw, BC::w, vf6, vf1, vf2);    // vmaddw.xyzw vf6, vf1, vf2
  c->sw(gp, 36, t0);                                // sw gp, 36(t0)
  c->vmsub_bc(DEST::xyzw, BC::w, vf8, vf1, vf2);    // vmsubw.xyzw vf8, vf1, vf2
  c->sh(s4, 32, t0);                                // sh s4, 32(t0)
  c->pcgtw(s5, r0, s5);                             // pcgtw s5, r0, s5
  c->lwu(gp, 40, t8);                               // lwu gp, 40(t8)
  // fprintf(stderr, "block_23-1: gp = 0x%lx\n", c->sgpr64(gp));
  c->ppach(s5, r0, s5);                             // ppach s5, r0, s5
  c->lbu(s4, 46, t8);                               // lbu s4, 46(t8)
  c->vadd_bc(DEST::xyzw, BC::z, vf6, vf3, vf6);     // vaddz.xyzw vf6, vf3, vf6
  c->sw(gp, 52, t0);                                // sw gp, 52(t0)
  c->vadd_bc(DEST::xyzw, BC::z, vf7, vf3, vf8);     // vaddz.xyzw vf7, vf3, vf8
  c->sw(t3, 12, t0);                                // sw t3, 12(t0)
  bc = c->sgpr64(s5) != 0;                          // bne s5, r0, L63
  c->sh(s4, 48, t0);                                // sh s4, 48(t0)
  if (bc) {goto block_45;}                          // branch non-likely

  c->vmini(DEST::xyzw, vf4, vf4, vf8);              // vmini.xyzw vf4, vf4, vf8
  c->sw(t3, 28, t0);                                // sw t3, 28(t0)
  // nop                                            // sll r0, r0, 0
  c->lbu(s5, 53, t8);                               // lbu s5, 53(t8)
  c->mov128_gpr_vf(gp, vf6);                        // qmfc2.i gp, vf6
  c->sw(t3, 44, t0);                                // sw t3, 44(t0)
  c->mov128_gpr_vf(s3, vf7);                        // qmfc2.i s3, vf7
  c->lbu(s4, 56, t8);                               // lbu s4, 56(t8)
  c->pcgtw(s2, r0, gp);                             // pcgtw s2, r0, gp
  c->lw(gp, 12, t8);                                // lw gp, 12(t8)
  // fprintf(stderr, "loaded gp: 0x%lx from tfragment: 0x%lx\n", c->sgpr64(gp), c->sgpr64(t8));
  c->pcgtw(s3, r0, s3);                             // pcgtw s3, r0, s3
  c->sb(s4, 76, t0);                                // sb s4, 76(t0)
  c->pinteh(s4, s2, s3);                            // pinteh s4, s2, s3
  c->lbu(s2, 54, t8);                               // lbu s2, 54(t8)
  c->ppacb(s3, r0, s4);                             // ppacb s3, r0, s4
  c->lbu(s1, 55, t8);                               // lbu s1, 55(t8)
  bc = c->sgpr64(s3) == 0;                          // beq s3, r0, L56
  c->dsrl32(s4, s3, 8);                             // dsrl32 s4, s3, 8
  if (bc) {goto block_36;}                          // branch non-likely

  bc = c->sgpr64(s2) == 0;                          // beq s2, r0, L56
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_36;}                          // branch non-likely

  bc = c->sgpr64(s1) == 0;                          // beq s1, r0, L55
  c->dsrl(s5, s3, 16);                              // dsrl s5, s3, 16
  if (bc) {goto block_33;}                          // branch non-likely

  bc = c->sgpr64(s5) == 0;                          // beq s5, r0, L55
  c->dsrl32(s5, s3, 24);                            // dsrl32 s5, s3, 24
  if (bc) {goto block_33;}                          // branch non-likely

  bc = c->sgpr64(s5) != 0;                          // bne s5, r0, L64
  c->addiu(s5, s1, 3);                              // addiu s5, s1, 3
  if (bc) {goto block_46;}                          // branch non-likely

  c->sra(s4, s5, 2);                                // sra s4, s5, 2
  c->mov64(s5, s1);                                 // or s5, s1, r0
  c->sll(t3, s4, 2);                                // sll t3, s4, 2
  c->sh(s4, 64, t0);                                // sh s4, 64(t0)
  // nop                                            // sll r0, r0, 0
  c->sb(t3, 78, t0);                                // sb t3, 78(t0)
  c->daddiu(t6, t6, 3);                             // daddiu t6, t6, 3
  c->lq(s2, 32, t0);                                // lq s2, 32(t0)
  // nop                                            // sll r0, r0, 0
  c->lq(s1, 48, t0);                                // lq s1, 48(t0)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 64, t0);                                // lq t3, 64(t0)
  c->sq(s2, 0, ra);                                 // sq s2, 0(ra)
  // nop                                            // sll r0, r0, 0
  c->sq(s1, 16, ra);                                // sq s1, 16(ra)
  c->dsrl32(s2, s3, 16);                            // dsrl32 s2, s3, 16
  c->sq(t3, 32, ra);                                // sq t3, 32(ra)
  c->daddiu(ra, ra, 48);                            // daddiu ra, ra, 48
  bc = c->sgpr64(s2) != 0;                          // bne s2, r0, L57
  c->ori(t3, t2, 18);                               // ori t3, t2, 18
  if (bc) {goto block_38;}                          // branch non-likely

  c->dsrl32(t3, s3, 8);                             // dsrl32 t3, s3, 8
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L57
  c->ori(t3, t2, 16);                               // ori t3, t2, 16
  if (bc) {goto block_38;}                          // branch non-likely

  //beq r0, r0, L57                                 // beq r0, r0, L57
  c->ori(t3, t2, 14);                               // ori t3, t2, 14
  goto block_38;                                    // branch always


  block_33:
  bc = c->sgpr64(s4) != 0;                          // bne s4, r0, L64
  c->addiu(s5, s2, 3);                              // addiu s5, s2, 3
  if (bc) {goto block_46;}                          // branch non-likely

  c->sra(s4, s5, 2);                                // sra s4, s5, 2
  c->mov64(s5, s2);                                 // or s5, s2, r0
  c->sll(t3, s4, 2);                                // sll t3, s4, 2
  c->sh(s4, 64, t0);                                // sh s4, 64(t0)
  // nop                                            // sll r0, r0, 0
  c->sb(t3, 78, t0);                                // sb t3, 78(t0)
  c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
  c->lq(s2, 16, t0);                                // lq s2, 16(t0)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 64, t0);                                // lq t3, 64(t0)
  c->sq(s2, 0, ra);                                 // sq s2, 0(ra)
  c->dsrl(s3, s3, 8);                               // dsrl s3, s3, 8
  c->sq(t3, 16, ra);                                // sq t3, 16(ra)
  c->daddiu(ra, ra, 32);                            // daddiu ra, ra, 32
  bc = c->sgpr64(s3) != 0;                          // bne s3, r0, L57
  c->ori(t3, t2, 10);                               // ori t3, t2, 10
  if (bc) {goto block_38;}                          // branch non-likely

  //beq r0, r0, L57                                 // beq r0, r0, L57
  c->ori(t3, t2, 8);                                // ori t3, t2, 8
  goto block_38;                                    // branch always


  block_36:
  bc = c->sgpr64(s4) != 0;                          // bne s4, r0, L64
  c->addiu(s4, s5, 3);                              // addiu s4, s5, 3
  if (bc) {goto block_46;}                          // branch non-likely

  c->sra(s4, s4, 2);                                // sra s4, s4, 2
  // nop                                            // sll r0, r0, 0
  c->sll(t3, s4, 2);                                // sll t3, s4, 2
  c->sh(s4, 64, t0);                                // sh s4, 64(t0)
  // nop                                            // sll r0, r0, 0
  c->sb(t3, 78, t0);                                // sb t3, 78(t0)
  c->ori(t3, t2, 6);                                // ori t3, t2, 6
  c->lq(s3, 0, t0);                                 // lq s3, 0(t0)
  c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
  c->lq(s2, 64, t0);                                // lq s2, 64(t0)
  c->sq(s3, 0, ra);                                 // sq s3, 0(ra)
  // nop                                            // sll r0, r0, 0
  c->sq(s2, 16, ra);                                // sq s2, 16(ra)
  c->daddiu(ra, ra, 32);                            // daddiu ra, ra, 32

  block_38:
  c->addiu(s3, r0, 127);                            // addiu s3, r0, 127
  c->daddu(s2, t6, s4);                             // daddu s2, t6, s4
  c->dsubu(s3, s3, s2);                             // dsubu s3, s3, s2
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(s3)) >= 0;                   // bgez s3, L60
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_42;}                          // branch non-likely


  // block_39:
  /*
  c->lw(ra, 0, t1);                                 // lw ra, 0(t1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(ra, ra, 256);                             // andi ra, ra, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(ra) == 0;                          // beq ra, r0, L59
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_41;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(ra, 184, t0);                               // lw ra, 184(t0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(ra, ra, 1);                             // daddiu ra, ra, 1
  // nop                                            // sll r0, r0, 0
  c->sw(ra, 184, t0);                               // sw ra, 184(t0)
  //beq r0, r0, L58                                 // beq r0, r0, L58
  // nop                                            // sll r0, r0, 0
  goto block_39;                                    // branch always
  */


  // block_41:
  {
    //c->sw(a3, 128, t1);                               // sw a3, 128(t1)
    u32 sadr = c->sgpr64(a3);
    c->xori(a3, a3, 6144);                            // xori a3, a3, 6144
    //c->sw(v1, 16, t1);                                // sw v1, 16(t1)
    u32 madr = c->sgpr64(v1);
    c->sll(ra, t6, 4);                                // sll ra, t6, 4
    c->addu(v1, v1, ra);                              // addu v1, v1, ra
    c->mov64(ra, a3);                                 // or ra, a3, r0
    //c->sw(t6, 32, t1);                                // sw t6, 32(t1)
    u32 qwc = c->sgpr64(t6);
    c->addiu(t6, r0, 256);                            // addiu t6, r0, 256
    //c->sw(t6, 0, t1);                                 // sw t6, 0(t1)
    spad_from_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
    c->addiu(t6, r0, 0);                              // addiu t6, r0, 0
  }

  block_42:
  c->daddu(t6, t6, s4);                             // daddu t6, t6, s4
  c->sw(t8, 168, t0);                               // sw t8, 168(t0)
  c->ld(s4, 0, gp);                                 // ld s4, 0(gp)
  c->daddiu(t8, gp, 8);                             // daddiu t8, gp, 8
  c->daddiu(gp, s5, -4);                            // daddiu gp, s5, -4
  // fprintf(stderr, "block_42: gp = 0x%lx\n", c->sgpr64(gp));
  c->lq(s5, 128, t0);                               // lq s5, 128(t0)
  c->pextlh(s4, r0, s4);                            // pextlh s4, r0, s4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->paddw(s2, s4, s5);                             // paddw s2, s4, s5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->lw(s4, 0, s2);                                 // lw s4, 0(s2)
  c->dsra32(s3, s2, 0);                             // dsra32 s3, s2, 0
  c->lw(s3, 0, s3);                                 // lw s3, 0(s3)
  c->pcpyud(s1, s2, s2);                            // pcpyud s1, s2, s2
  c->lw(s2, 0, s1);                                 // lw s2, 0(s1)
  c->dsra32(s1, s1, 0);                             // dsra32 s1, s1, 0
  bc = ((s64)c->sgpr64(gp)) <= 0;                   // blez gp, L62
  c->lw(s1, 0, s1);                                 // lw s1, 0(s1)
  if (bc) {goto block_44;}                          // branch non-likely


  block_43:
  c->ld(s0, 0, t8);                                 // ld s0, 0(t8)
  c->daddiu(ra, ra, 16);                            // daddiu ra, ra, 16
  c->daddiu(t8, t8, 8);                             // daddiu t8, t8, 8
  c->sw(s4, -16, ra);                               // sw s4, -16(ra)
  c->daddiu(gp, gp, -4);                            // daddiu gp, gp, -4
  c->sw(s3, -12, ra);                               // sw s3, -12(ra)
  c->pextlh(s4, r0, s0);                            // pextlh s4, r0, s0
  c->sw(s2, -8, ra);                                // sw s2, -8(ra)
  c->paddw(s2, s4, s5);                             // paddw s2, s4, s5
  // this one is storing a 0!
  c->sw(s1, -4, ra);                                // sw s1, -4(ra)
  c->lw(s4, 0, s2);                                 // lw s4, 0(s2)
  c->dsra32(s3, s2, 0);                             // dsra32 s3, s2, 0
  c->lw(s3, 0, s3);                                 // lw s3, 0(s3)
  c->pcpyud(s1, s2, s2);                            // pcpyud s1, s2, s2
  c->lw(s2, 0, s1);                                 // lw s2, 0(s1)
  c->dsra32(s1, s1, 0);                             // dsra32 s1, s1, 0
  bc = ((s64)c->sgpr64(gp)) > 0;                    // bgtz gp, L61
  c->lw(s1, 0, s1);                                 // lw s1, 0(s1)
  if (bc) {goto block_43;}                          // branch non-likely


  block_44:
  c->daddiu(ra, ra, 16);                            // daddiu ra, ra, 16
  c->lw(t8, 168, t0);                               // lw t8, 168(t0)
  // nop                                            // sll r0, r0, 0
  c->sw(s4, -16, ra);                               // sw s4, -16(ra)
  // nop                                            // sll r0, r0, 0
  c->sw(s3, -12, ra);                               // sw s3, -12(ra)
  // nop                                            // sll r0, r0, 0
  c->sw(s2, -8, ra);                                // sw s2, -8(ra)
  // nop                                            // sll r0, r0, 0
  c->sw(s1, -4, ra);                                // sw s1, -4(ra)

  block_45:
  c->xor_(t7, t7, t9);                              // xor t7, t7, t9
  // nop                                            // sll r0, r0, 0

  block_46:
  c->daddiu(t8, t8, 64);                            // daddiu t8, t8, 64
  c->srl(t9, t9, 1);                                // srl t9, t9, 1
  c->addiu(a2, a2, -1);                             // addiu a2, a2, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t9) != 0;                          // bne t9, r0, L51
  c->lqc2(vf2, 16, t8);                             // lqc2 vf2, 16(t8)
  if (bc) {goto block_19;}                          // branch non-likely


  block_47:
  // nop                                            // sll r0, r0, 0
  c->lw(gp, 160, t0);                               // lw gp, 160(t0)
  // fprintf(stderr, "block_47: gp = 0x%lx\n", c->sgpr64(gp));
  // nop                                            // sll r0, r0, 0
  c->lw(t9, 164, t0);                               // lw t9, 164(t0)
  bc = c->sgpr64(gp) != c->sgpr64(t9);              // bne gp, t9, L49
  c->sb(t7, -1, gp);                                // sb t7, -1(gp)
  if (bc) {goto block_16;}                          // branch non-likely

  bc = ((s64)c->sgpr64(a2)) > 0;                    // bgtz a2, L43
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_6;}                           // branch non-likely

  bc = c->sgpr64(t6) == 0;                          // beq t6, r0, L68
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_53;}                          // branch non-likely


  // block_50:
  /*
  c->lw(a0, 0, t1);                                 // lw a0, 0(t1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(a0, a0, 256);                             // andi a0, a0, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L67
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_52;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(a0, 184, t0);                               // lw a0, 184(t0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 184, t0);                               // sw a0, 184(t0)
  //beq r0, r0, L66                                 // beq r0, r0, L66
  // nop                                            // sll r0, r0, 0
  goto block_50;                                    // branch always
  */


  // block_52:
  {
    //c->sw(a3, 128, t1);                               // sw a3, 128(t1)
    u32 sadr = c->sgpr64(a3);
    c->xori(a0, a3, 6144);                            // xori a0, a3, 6144
    //c->sw(v1, 16, t1);                                // sw v1, 16(t1)
    u32 madr = c->sgpr64(v1);
    c->sll(a1, t6, 4);                                // sll a1, t6, 4
    c->addu(v1, v1, a1);                              // addu v1, v1, a1
    c->mov64(a0, a0);                                 // or a0, a0, r0
    //c->sw(t6, 32, t1);                                // sw t6, 32(t1)
    u32 qwc = c->sgpr64(t6);
    c->addiu(a0, r0, 256);                            // addiu a0, r0, 256
    //c->sw(a0, 0, t1);                                 // sw a0, 0(t1)
    spad_from_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
    c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
 }

  block_53:
  /*
  c->lw(a0, 0, t1);                                 // lw a0, 0(t1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(a0, a0, 256);                             // andi a0, a0, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L69
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_55;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(a0, 184, t0);                               // lw a0, 184(t0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 184, t0);                               // sw a0, 184(t0)
  //beq r0, r0, L68                                 // beq r0, r0, L68
  // nop                                            // sll r0, r0, 0
  goto block_53;                                    // branch always
  */


  block_55:
  c->lw(a0, 176, t0);                               // lw a0, 176(t0)
  // nop                                            // sll r0, r0, 0
  c->sw(t3, 172, t0);                               // sw t3, 172(t0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf4, 112, t0);                            // sqc2 vf4, 112(t0)
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 4, a0);                                 // sw v1, 4(a0)
  // nop                                            // sll r0, r0, 0
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

// clang-format on
void link() {
  cache.tfrag_work = intern_from_c("*tfrag-work*").c();
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  cache.transform_regs = intern_from_c("*transform-regs*").c();
  gLinkedFunctionTable.reg("draw-inline-array-tfrag", execute, 512);
}

}  // namespace draw_inline_array_tfrag
}  // namespace Mips2C::jak1

// clang-format off
//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace stats_tfrag_asm {
struct Cache {
  void* tfrag_work; // *tfrag-work*
  void* transform_regs;
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  load_vfs_from_tf_regs(cache.transform_regs, c);
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->load_symbol(v1, cache.tfrag_work);             // lw v1, *tfrag-work*(s7)
  // nop                                            // sll r0, r0, 0
  c->lw(a1, 4, a0);                                 // lw a1, 4(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf10, 12, a0);                            // lqc2 vf10, 12(a0)
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L7
  c->lqc2(vf14, 80, v1);                            // lqc2 vf14, 80(v1)
  if (bc) {goto block_15;}                          // branch non-likely

  c->vmula_bc(DEST::xyzw, BC::x, vf16, vf10);       // vmulax.xyzw acc, vf16, vf10
  c->lb(a2, 49, a0);                                // lb a2, 49(a0)
  c->vmadda_bc(DEST::xyzw, BC::y, vf17, vf10);      // vmadday.xyzw acc, vf17, vf10
  c->lb(a2, 50, a0);                                // lb a2, 50(a0)
  c->vmadda_bc(DEST::xyzw, BC::z, vf18, vf10);      // vmaddaz.xyzw acc, vf18, vf10
  c->lb(a0, 51, a0);                                // lb a0, 51(a0)
  c->vmsub_bc(DEST::xyzw, BC::w, vf9, vf19, vf0);   // vmsubw.xyzw vf9, vf19, vf0
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf27, vf0);        // vmulaw.xyzw acc, vf27, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf24, vf10);      // vmaddax.xyzw acc, vf24, vf10
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf25, vf10);      // vmadday.xyzw acc, vf25, vf10
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::xyzw, BC::w, vf9, vf9, vf10);    // vaddw.xyzw vf9, vf9, vf10
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf11, vf26, vf10); // vmaddz.xyzw vf11, vf26, vf10
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::xyzw, BC::w, vf12, vf11, vf10);  // vaddw.xyzw vf12, vf11, vf10
  // nop                                            // sll r0, r0, 0
  c->vsub_bc(DEST::xyzw, BC::w, vf13, vf11, vf10);  // vsubw.xyzw vf13, vf11, vf10
  // nop                                            // sll r0, r0, 0
  c->vsub_bc(DEST::xyzw, BC::z, vf11, vf0, vf12);   // vsubz.xyzw vf11, vf0, vf12
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a3, vf9);                        // qmfc2.i a3, vf9
  // nop                                            // sll r0, r0, 0
  c->pcgtw(a3, r0, a3);                             // pcgtw a3, r0, a3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppach(a3, r0, a3);                             // ppach a3, r0, a3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L7
  c->vadd_bc(DEST::xyzw, BC::z, vf12, vf14, vf12);  // vaddz.xyzw vf12, vf14, vf12
  if (bc) {goto block_15;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::xyzw, BC::z, vf13, vf14, vf13);  // vaddz.xyzw vf13, vf14, vf13
  c->mov128_gpr_vf(t0, vf12);                       // qmfc2.i t0, vf12
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a3, vf13);                       // qmfc2.i a3, vf13
  // nop                                            // sll r0, r0, 0
  c->pcgtw(t0, r0, t0);                             // pcgtw t0, r0, t0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(a3, r0, a3);                             // pcgtw a3, r0, a3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pinteh(a3, t0, a3);                            // pinteh a3, t0, a3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppacb(a3, r0, a3);                             // ppacb a3, r0, a3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L5
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely

  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L5
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely

  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L3
  c->dsrl(a0, a3, 16);                              // dsrl a0, a3, 16
  if (bc) {goto block_9;}                           // branch non-likely

  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L3
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_9;}                           // branch non-likely

  c->dsrl32(a0, a3, 24);                            // dsrl32 a0, a3, 24
  c->lbu(a2, 4, a1);                                // lbu a2, 4(a1)
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L2
  c->lw(a0, 148, v1);                               // lw a0, 148(v1)
  if (bc) {goto block_8;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(a0, 144, v1);                               // lw a0, 144(v1)

  block_8:
  // nop                                            // sll r0, r0, 0
  c->lbu(v1, 12, a1);                               // lbu v1, 12(a1)
  // nop                                            // sll r0, r0, 0
  c->lw(a3, 4, a0);                                 // lw a3, 4(a0)
  // nop                                            // sll r0, r0, 0
  c->lw(a1, 8, a0);                                 // lw a1, 8(a0)
  c->addu(a2, a3, a2);                              // addu a2, a3, a2
  c->sw(a2, 4, a0);                                 // sw a2, 4(a0)
  // nop                                            // sll r0, r0, 0
  c->lh(a2, 2, a0);                                 // lh a2, 2(a0)
  c->daddu(v1, a1, v1);                             // daddu v1, a1, v1
  c->sw(v1, 8, a0);                                 // sw v1, 8(a0)
  c->daddiu(v1, a2, 1);                             // daddiu v1, a2, 1
  c->sh(v1, 2, a0);                                 // sh v1, 2(a0)
  //beq r0, r0, L7                                  // beq r0, r0, L7
  // nop                                            // sll r0, r0, 0
  goto block_15;                                    // branch always


  block_9:
  c->dsrl32(a0, a3, 8);                             // dsrl32 a0, a3, 8
  c->lbu(a2, 2, a1);                                // lbu a2, 2(a1)
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L4
  c->lw(a0, 148, v1);                               // lw a0, 148(v1)
  if (bc) {goto block_11;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(a0, 144, v1);                               // lw a0, 144(v1)

  block_11:
  // nop                                            // sll r0, r0, 0
  c->lbu(v1, 10, a1);                               // lbu v1, 10(a1)
  // nop                                            // sll r0, r0, 0
  c->lw(a3, 4, a0);                                 // lw a3, 4(a0)
  // nop                                            // sll r0, r0, 0
  c->lw(a1, 8, a0);                                 // lw a1, 8(a0)
  c->addu(a2, a3, a2);                              // addu a2, a3, a2
  c->sw(a2, 4, a0);                                 // sw a2, 4(a0)
  // nop                                            // sll r0, r0, 0
  c->lh(a2, 2, a0);                                 // lh a2, 2(a0)
  c->daddu(v1, a1, v1);                             // daddu v1, a1, v1
  c->sw(v1, 8, a0);                                 // sw v1, 8(a0)
  c->daddiu(v1, a2, 1);                             // daddiu v1, a2, 1
  c->sh(v1, 2, a0);                                 // sh v1, 2(a0)
  //beq r0, r0, L7                                  // beq r0, r0, L7
  // nop                                            // sll r0, r0, 0
  goto block_15;                                    // branch always


  block_12:
  c->dsrl32(a0, a3, 8);                             // dsrl32 a0, a3, 8
  c->lbu(a2, 0, a1);                                // lbu a2, 0(a1)
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L6
  c->lw(a0, 148, v1);                               // lw a0, 148(v1)
  if (bc) {goto block_14;}                          // branch non-likely

  //beq r0, r0, L6                                  // beq r0, r0, L6
  c->lw(a0, 144, v1);                               // lw a0, 144(v1)
  goto block_14;                                    // branch always


  block_14:
  // nop                                            // sll r0, r0, 0
  c->lbu(v1, 8, a1);                                // lbu v1, 8(a1)
  // nop                                            // sll r0, r0, 0
  c->lw(a3, 4, a0);                                 // lw a3, 4(a0)
  // nop                                            // sll r0, r0, 0
  c->lw(a1, 8, a0);                                 // lw a1, 8(a0)
  c->addu(a2, a3, a2);                              // addu a2, a3, a2
  c->sw(a2, 4, a0);                                 // sw a2, 4(a0)
  // nop                                            // sll r0, r0, 0
  c->lh(a2, 2, a0);                                 // lh a2, 2(a0)
  c->daddu(v1, a1, v1);                             // daddu v1, a1, v1
  c->sw(v1, 8, a0);                                 // sw v1, 8(a0)
  c->daddiu(v1, a2, 1);                             // daddiu v1, a2, 1
  c->sh(v1, 2, a0);                                 // sh v1, 2(a0)

  block_15:
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
  cache.tfrag_work = intern_from_c("*tfrag-work*").c();
  cache.transform_regs = intern_from_c("*transform-regs*").c();
  gLinkedFunctionTable.reg("stats-tfrag-asm", execute, 512);
}

} // namespace stats_tfrag_asm
} // namespace Mips2C
