
//--------------------------MIPS2C---------------------

#include "game/kernel/jak1/kscheme.h"
#include "game/mips2c/mips2c_private.h"
using namespace jak1;

// clang-format off
namespace Mips2C::jak1 {
namespace time_of_day_interp_colors_scratch {

struct Cache {
  void* fake_scratchpad_data;  // *fake-scratchpad-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  // nop                                            // sll r0, r0, 0
  c->lui(v1, 28672);                                // lui v1, 28672      0x7000
  c->daddiu(t4, a1, 12);                            // daddiu t4, a1, 12
  //c->ori(v1, v1, 2064);                           // ori v1, v1, 2064 SPAD mods
  get_fake_spad_addr(v1, cache.fake_scratchpad_data, 2064, c);
  // Unknown instr: ld a3, L168(fp)
  // L168:
  //    .word 0xff00ff
  //    .word 0x8000ff
  c->gprs[a3].du32[0] = 0xff00ff;
  c->gprs[a3].du32[1] = 0x8000ff;

  c->lui(t0, 4096);                                 // lui t0, 4096
  c->lw(t1, 4, a1);                                 // lw t1, 4(a1)
  c->ori(a1, t0, 54272);                            // ori a1, t0, 54272 = (0x1000D400) SPR TO
  c->lq(t0, 1852, a2);                              // lq t0, 1852(a2)
  c->addiu(t2, t1, 31);                             // addiu t2, t1, 31
  c->lq(t1, 1868, a2);                              // lq t1, 1868(a2)
  c->sra(t3, t2, 5);                                // sra t3, t2, 5
  c->lq(t2, 1884, a2);                              // lq t2, 1884(a2)
  c->sll(t3, t3, 5);                                // sll t3, t3, 5
  c->lq(a2, 1900, a2);                              // lq a2, 1900(a2)

  // wait for DMA to finish, can just remove this
  /*
  block_1:
  c->lw(t5, 0, a1);                                 // lw t5, 0(a1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(t5, t5, 256);                             // andi t5, t5, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t5) != 0;                          // bne t5, r0, L62
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely
  */

  {
    // c->sw(t4, 16, a1);                                // sw t4, 16(a1)
    u32 madr = c->sgpr64(t4);
    c->daddiu(t3, t3, -32);                           // daddiu t3, t3, -32
    // c->sw(v1, 128, a1);                               // sw v1, 128(a1)
    u32 sadr = c->sgpr64(v1);
    c->addiu(t5, r0, 64);                             // addiu t5, r0, 64
    //c->sw(t5, 32, a1);                                // sw t5, 32(a1)
    u32 qwc = c->sgpr64(t5);
    c->addiu(t5, r0, 256);                            // addiu t5, r0, 256
    // c->sw(t5, 0, a1);                                 // sw t5, 0(a1)
    spad_to_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
    c->daddiu(t4, t4, 1024);                          // daddiu t4, t4, 1024
  }

  block_3:
  c->mov64(t6, v1);                                 // or t6, v1, r0
  c->xori(v1, v1, 1024);                            // xori v1, v1, 1024
  bc = ((s64)c->sgpr64(t3)) <= 0;                   // blez t3, L66
  c->daddiu(t3, t3, -32);                           // daddiu t3, t3, -32
  if (bc) {goto block_7;}                           // branch non-likely


  /*
  block_4:
  c->lw(t5, 0, a1);                                 // lw t5, 0(a1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(t5, t5, 256);                             // andi t5, t5, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L65
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_6;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L64                                 // beq r0, r0, L64
  // nop                                            // sll r0, r0, 0
  goto block_4;                                     // branch always
  */

  {
    // block_6:
    // c->sw(t4, 16, a1);                                // sw t4, 16(a1)
    u32 madr = c->sgpr64(t4);
    // nop                                            // sll r0, r0, 0
    // c->sw(v1, 128, a1);                               // sw v1, 128(a1)
    u32 sadr = c->sgpr64(v1);
    c->addiu(t5, r0, 64);                             // addiu t5, r0, 64
    // c->sw(t5, 32, a1);                                // sw t5, 32(a1)
    u32 qwc = c->sgpr64(t5);
    c->addiu(t5, r0, 256);                            // addiu t5, r0, 256
    // c->sw(t5, 0, a1);                                 // sw t5, 0(a1)
    spad_to_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
    c->daddiu(t4, t4, 1024);                          // daddiu t4, t4, 1024
    //beq r0, r0, L67                                 // beq r0, r0, L67
    // nop                                            // sll r0, r0, 0
  }
  goto block_9;                                     // branch always



  block_7:
  /*
  c->lw(t5, 0, a1);                                 // lw t5, 0(a1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(t5, t5, 256);                             // andi t5, t5, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L67
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_9;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L66                                 // beq r0, r0, L66
  // nop                                            // sll r0, r0, 0
  goto block_7;                                     // branch always
  */

  /*
  fmt::print("{} -> {} [{}]\n", c->gprs[a0].du32[0], c->gprs[t5].du32[0], c->gprs[t3].ds64[0]);
  fmt::print("[2] t0: {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}\n", c->gprs[t0].du16[0], c->gprs[t0].du16[1], c->gprs[t0].du16[2], c->gprs[t0].du16[3], c->gprs[t0].du16[4], c->gprs[t0].du16[5], c->gprs[t0].du16[6], c->gprs[t0].du16[7]);
  fmt::print("[2] t1: {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}\n", c->gprs[t1].du16[0], c->gprs[t1].du16[1], c->gprs[t1].du16[2], c->gprs[t1].du16[3], c->gprs[t1].du16[4], c->gprs[t1].du16[5], c->gprs[t1].du16[6], c->gprs[t1].du16[7]);
  fmt::print("[2] t2: {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}\n", c->gprs[t2].du16[0], c->gprs[t2].du16[1], c->gprs[t2].du16[2], c->gprs[t2].du16[3], c->gprs[t2].du16[4], c->gprs[t2].du16[5], c->gprs[t2].du16[6], c->gprs[t2].du16[7]);
  fmt::print("[2] a2: {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}\n", c->gprs[a2].du16[0], c->gprs[a2].du16[1], c->gprs[a2].du16[2], c->gprs[a2].du16[3], c->gprs[a2].du16[4], c->gprs[a2].du16[5], c->gprs[a2].du16[6], c->gprs[a2].du16[7]);
  */
  block_9:
  c->lq(t9, 12, t6);                                // lq t9, 12(t6)
  c->daddiu(t5, a0, 128);                           // daddiu t5, a0, 128
  c->lq(t7, 28, t6);                                // lq t7, 28(t6)
  // nop                                            // sll r0, r0, 0
  c->pextlb(t8, r0, t9);                            // pextlb t8, r0, t9
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextub(t9, r0, t9);                            // pextub t9, r0, t9
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmulth(r0, t8, t0);                            // pmulth r0, t8, t0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(t8, r0, t7);                            // pextlb t8, r0, t7
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaddh(r0, t9, t1);                            // pmaddh r0, t9, t1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextub(t7, r0, t7);                            // pextub t7, r0, t7
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaddh(r0, t8, t2);                            // pmaddh r0, t8, t2
  c->lq(t8, 44, t6);                                // lq t8, 44(t6)
  c->addiu(t6, t6, 32);                             // addiu t6, t6, 32
  // nop                                            // sll r0, r0, 0
  c->pmaddh(r0, t7, a2);                            // pmaddh r0, t7, a2
  c->lq(t7, 28, t6);                                // lq t7, 28(t6)
  c->pextlb(t9, r0, t8);                            // pextlb t9, r0, t8
  c->mfc1(r0, f31);                                 // mfc1 r0, f31

  block_10:
  c->pextub(t8, r0, t8);                            // pextub t8, r0, t8
  // fmt::print("[0] t1: {:02x} {:02x} {:02x} {:02x}\n", c->gprs[t1].du32[0], c->gprs[t1].du32[1], c->gprs[t1].du32[2], c->gprs[t1].du32[3]);
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmfhl_lh(ra);                                  // pmfhl.lh ra
  // fmt::print("[1] ra: {:02x} {:02x} {:02x} {:02x}\n", c->gprs[ra].du32[0], c->gprs[ra].du32[1], c->gprs[ra].du32[2], c->gprs[ra].du32[3]);
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmulth(r0, t9, t0);                            // pmulth r0, t9, t0

  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psrlh(t9, ra, 6);                              // psrlh t9, ra, 6
  // fmt::print("[3] t9: {:02x} {:02x} {:02x} {:02x}\n", c->gprs[t9].du32[0], c->gprs[t9].du32[1], c->gprs[t9].du32[2], c->gprs[t9].du32[3]);
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyud(ra, t9, t9);                            // pcpyud ra, t9, t9
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->paddh(t9, ra, t9);                             // paddh t9, ra, t9
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pminh(t9, t9, a3);                             // pminh t9, t9, a3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppacb(ra, r0, t9);                             // ppacb ra, r0, t9
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(t9, r0, t7);                            // pextlb t9, r0, t7
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaddh(r0, t8, t1);                            // pmaddh r0, t8, t1
  c->sw(ra, 0, a0);                                 // sw ra, 0(a0)
  c->pextub(t7, r0, t7);                            // pextub t7, r0, t7
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaddh(r0, t9, t2);                            // pmaddh r0, t9, t2
  c->lq(t8, 44, t6);                                // lq t8, 44(t6)
  c->addiu(t6, t6, 32);                             // addiu t6, t6, 32
  c->addiu(a0, a0, 4);                              // addiu a0, a0, 4
  c->pmaddh(r0, t7, a2);                            // pmaddh r0, t7, a2
  c->lq(t7, 28, t6);                                // lq t7, 28(t6)
  /*
  fmt::print(" N0");
  for (int i = 0; i < 4; i++) {
    for (int j = 0; j < 4; j++) {
      fmt::print(" {:02x}", c->gprs[t8].du8[i*4 + j]);
    }
    fmt::print(" |");
  }
  fmt::print("\n N1");
  for (int i = 0; i < 4; i++) {
    for (int j = 0; j < 4; j++) {
      fmt::print(" {:02x}", c->gprs[t7].du8[i*4 + j]);
    }
    fmt::print(" |");
  }
  fmt::print("\n");
  */
//  fmt::print("next: {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x} {:02x}\n", c->gprs[t8].du32[0], c->gprs[t8].du32[1], c->gprs[t8].du32[2], c->gprs[t8].du32[3],
//            c->gprs[t7].du32[0], c->gprs[t7].du32[1], c->gprs[t7].du32[2], c->gprs[t7].du32[3]);
  bc = c->sgpr64(a0) != c->sgpr64(t5);              // bne a0, t5, L68
  c->pextlb(t9, r0, t8);                            // pextlb t9, r0, t8
  if (bc) {goto block_10;}                          // branch non-likely

  bc = ((s64)c->sgpr64(t3)) >= 0;                   // bgez t3, L63
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_3;}                           // branch non-likely

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
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("time-of-day-interp-colors-scratch", execute, 512);
}

} // namespace time_of_day_interp_colors_scratch
} // namespace Mips2C

