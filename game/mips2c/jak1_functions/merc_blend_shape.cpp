
// clang-format off
//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak1/kscheme.h"
using namespace jak1;
namespace Mips2C::jak1 {
namespace blerc_execute {
struct Cache {
  void* blerc_globals; // *blerc-globals*
  void* gsf_buffer; // *gsf-buffer*
  void* stats_blerc; // *stats-blerc*
  void* flush_cache; // flush-cache
  void* fake_scratchpad_data;  // *fake-scratchpad-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  u32 madr, sadr, qwc, tadr;
  c->daddiu(sp, sp, -96);                           // daddiu sp, sp, -96
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s2, 16, sp);                                // sq s2, 16(sp)
  c->sq(s3, 32, sp);                                // sq s3, 32(sp)
  c->sq(s4, 48, sp);                                // sq s4, 48(sp)
  c->sq(s5, 64, sp);                                // sq s5, 64(sp)
  c->sq(gp, 80, sp);                                // sq gp, 80(sp)
  c->load_symbol(v1, cache.blerc_globals);          // lw v1, *blerc-globals*(s7)
  c->lwu(s5, 0, v1);                                // lwu s5, 0(v1)
  bc = c->sgpr64(s5) == 0;                          // beq s5, r0, L46
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_33;}                          // branch non-likely

  c->addiu(v1, r0, 0);                              // addiu v1, r0, 0
  c->addiu(gp, r0, 0);                              // addiu gp, r0, 0
  c->addiu(v1, r0, 0);                              // addiu v1, r0, 0
  c->load_symbol(v1, cache.gsf_buffer);             // lw v1, *gsf-buffer*(s7)
  c->load_symbol(t9, cache.flush_cache);            // lw t9, flush-cache(s7)
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->addiu(v1, r0, 848);                            // addiu v1, r0, 848
  // c->lui(a0, 28672);                                // lui a0, 28672
  get_fake_spad_addr(a0, cache.fake_scratchpad_data, 0, c);
  c->daddu(a1, v1, a0);                             // daddu a1, v1, a0
  c->load_symbol(v1, cache.blerc_globals);          // lw v1, *blerc-globals*(s7)
  c->daddu(v1, r0, v1);                             // daddu v1, r0, v1
  // nop                                            // sll r0, r0, 0
  c->lui(a0, 4096);                                 // lui a0, 4096
  // nop                                            // sll r0, r0, 0
  c->ori(a0, a0, 54272);                            // ori a0, a0, 54272 // spr to
  c->andi(a1, a1, 16383);                           // andi a1, a1, 16383
/*
  block_2:
  c->lw(a2, 0, a0);                                 // lw a2, 0(a0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(a2, a2, 256);                             // andi a2, a2, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L28
  c->lw(a2, 0, v1);                                 // lw a2, 0(v1)
  if (bc) {goto block_2;}                           // branch non-likely
  */
  c->lw(a2, 0, v1);

  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L29
  //c->sw(a1, 128, a0);                               // sw a1, 128(a0)
  sadr = c->sgpr64(a1);
  if (bc) {goto block_5;}                           // branch non-likely

  c->addiu(v1, r0, 324);                            // addiu v1, r0, 324
  //c->sw(a2, 48, a0);                                // sw a2, 48(a0)
  tadr = c->sgpr64(a2);
  //c->sw(r0, 32, a0);                                // sw r0, 32(a0)
  // Unknown instr: sync.l
  //c->sw(v1, 0, a0);                                 // sw v1, 0(a0)
  spad_to_dma_blerc_chain(cache.fake_scratchpad_data, sadr, tadr);
  // Unknown instr: sync.l

  block_5:
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  //beq r0, r0, L45                                 // beq r0, r0, L45
  // nop                                            // sll r0, r0, 0
  goto block_31;                                    // branch always


  block_6:
  bc = c->sgpr64(gp) != 0;                          // bne gp, r0, L31
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_8;}                           // branch non-likely

  //c->lui(v1, 28672);                                // lui v1, 28672
  get_fake_spad_addr(v1, cache.fake_scratchpad_data, 0, c);
  c->daddu(a0, r0, v1);                             // daddu a0, r0, v1
  //beq r0, r0, L32                                 // beq r0, r0, L32
  // nop                                            // sll r0, r0, 0
  goto block_9;                                     // branch always


  block_8:
  c->addiu(v1, r0, 8192);                           // addiu v1, r0, 8192
  // c->lui(a0, 28672);                                // lui a0, 28672
  get_fake_spad_addr(a0, cache.fake_scratchpad_data, 0, c);
  c->daddu(a0, v1, a0);                             // daddu a0, v1, a0

  block_9:
  bc = c->sgpr64(gp) != 0;                          // bne gp, r0, L33
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_11;}                          // branch non-likely

  c->addiu(v1, r0, 8192);                           // addiu v1, r0, 8192
  // c->lui(a1, 28672);                                // lui a1, 28672
  get_fake_spad_addr(a1, cache.fake_scratchpad_data, 0, c);
  c->daddu(a1, v1, a1);                             // daddu a1, v1, a1
  //beq r0, r0, L34                                 // beq r0, r0, L34
  // nop                                            // sll r0, r0, 0
  goto block_12;                                    // branch always


  block_11:
  // c->lui(v1, 28672);                                // lui v1, 28672
  get_fake_spad_addr(v1, cache.fake_scratchpad_data, 0, c);
  c->daddu(a1, r0, v1);                             // daddu a1, r0, v1

  block_12:
  c->daddiu(v1, a0, 848);                           // daddiu v1, a0, 848
  c->daddu(a2, r0, a0);                             // daddu a2, r0, a0
  c->daddiu(a3, a1, 848);                           // daddiu a3, a1, 848
  c->daddiu(a1, v1, 12);                            // daddiu a1, v1, 12
  // nop                                            // sll r0, r0, 0
  c->lui(a2, 4096);                                 // lui a2, 4096
  // nop                                            // sll r0, r0, 0
  c->ori(a2, a2, 54272);                            // ori a2, a2, 54272
  c->andi(a3, a3, 16383);                           // andi a3, a3, 16383


/*
  block_13:
  c->lw(t0, 0, a2);                                 // lw t0, 0(a2)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(t0, t0, 256);                             // andi t0, t0, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t0) != 0;                          // bne t0, r0, L35
  c->lw(t0, 0, a1);                                 // lw t0, 0(a1)
  if (bc) {goto block_13;}                          // branch non-likely
  */

  c->lw(t0, 0, a1);


  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L36
  // c->sw(a3, 128, a2);                               // sw a3, 128(a2)
  sadr = c->sgpr64(a3);
  if (bc) {goto block_16;}                          // branch non-likely

  c->addiu(a1, r0, 324);                            // addiu a1, r0, 324
  // c->sw(t0, 48, a2);                                // sw t0, 48(a2)
  tadr = c->sgpr64(t0);
  // c->sw(r0, 32, a2);                                // sw r0, 32(a2)
  // Unknown instr: sync.l
  //c->sw(a1, 0, a2);                                 // sw a1, 0(a2)
  // tadr here is bogus, it's reading something uploaded by the other transfer.
  spad_to_dma_blerc_chain(cache.fake_scratchpad_data, sadr, tadr);

  // Unknown instr: sync.l

  block_16:
  c->gprs[a1].du64[0] = 0;                          // or a1, r0, r0
  c->mov64(a2, a0);                                 // or a2, a0, r0
  c->load_symbol(a3, cache.gsf_buffer);             // lw a3, *gsf-buffer*(s7)
  c->daddiu(t2, a2, 880);                           // daddiu t2, a2, 880
  c->lb(t1, 0, t2);                                 // lb t1, 0(t2)
  // nop                                            // sll r0, r0, 0
  c->mov64(t0, a2);                                 // or t0, a2, r0
  c->lw(a1, 868, a2);                               // lw a1, 868(a2)
  c->daddiu(t3, t1, 1);                             // daddiu t3, t1, 1

  c->mov64(t1, a3);                                 // or t1, a3, r0
  c->sll(t8, t3, 4);                                // sll t8, t3, 4
  c->sll(t3, a1, 4);                                // sll t3, a1, 4
  c->daddu(t9, t3, a3);                             // daddu t9, t3, a3
  c->daddu(t3, t2, t8);                             // daddu t3, t2, t8
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L39
  c->daddiu(ra, t2, 16);                            // daddiu ra, t2, 16
  if (bc) {goto block_21;}                          // branch non-likely

  c->lh(t5, 12, t3);                                // lh t5, 12(t3)
  c->daddu(t2, t3, t8);                             // daddu t2, t3, t8
  //beq r0, r0, L38                                 // beq r0, r0, L38
  // nop                                            // sll r0, r0, 0
  goto block_19;                                    // branch always


  block_18:
  c->lh(t5, 12, t2);                                // lh t5, 12(t2)
  c->daddu(t2, t2, t8);                             // daddu t2, t2, t8
  c->sq(t6, 0, t1);                                 // sq t6, 0(t1)
  c->daddiu(t1, t1, 16);                            // daddiu t1, t1, 16

  block_19:
  c->pcpyh(t5, t5);                                 // pcpyh t5, t5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
   //fmt::print("loop: {:x} {:x}\n", c->sgpr64(t1), c->sgpr64(t9));
  bc = c->sgpr64(t1) != c->sgpr64(t9);              // bne t1, t9, L37
  c->pcpyld(t6, t5, t5);                            // pcpyld t6, t5, t5
  if (bc) {goto block_18;}                          // branch non-likely

  c->dsubu(t3, t2, t8);                             // dsubu t3, t2, t8
  // nop                                            // sll r0, r0, 0

  block_21:
  c->addiu(t1, r0, 255);                            // addiu t1, r0, 255
  c->addiu(t2, r0, 8192);                           // addiu t2, r0, 8192
  c->lb(s5, 0, t3);                                 // lb s5, 0(t3)
  c->daddiu(s4, t3, 16);                            // daddiu s4, t3, 16
  c->pcpyh(t1, t1);                                 // pcpyh t1, t1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyld(t1, t1, t1);                            // pcpyld t1, t1, t1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyh(t2, t2);                                 // pcpyh t2, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyld(t2, t2, t2);                            // pcpyld t2, t2, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->mov128_gpr_gpr(t3, t1);                        // por t3, t1, r0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->mov128_gpr_gpr(t4, r0);                        // por t4, r0, r0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31

  block_22:
  c->ld(t6, 0, ra);                                 // ld t6, 0(ra)
  c->daddu(s2, ra, t8);                             // daddu s2, ra, t8
  c->daddiu(ra, ra, 8);                             // daddiu ra, ra, 8
  c->mov64(s3, a3);                                 // or s3, a3, r0
  c->pextlb(t6, r0, t6);                            // pextlb t6, r0, t6
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmulth(t7, t6, t2);                            // pmulth t7, t6, t2
  c->ld(t5, 0, s2);                                 // ld t5, 0(s2)
  c->daddiu(s5, s5, -1);                            // daddiu s5, s5, -1
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L42                                 // beq r0, r0, L42
  c->daddu(s2, s2, t8);                             // daddu s2, s2, t8
  goto block_24;                                    // branch always


  block_23:
  c->pmaddh(t7, t5, t6);                            // pmaddh t7, t5, t6
  c->ld(t5, 0, s2);                                 // ld t5, 0(s2)
  c->daddu(s2, s2, t8);                             // daddu s2, s2, t8
  c->daddiu(s3, s3, 16);                            // daddiu s3, s3, 16

  block_24:
  c->lq(t6, 0, s3);                                 // lq t6, 0(s3)
  c->pextlb(t5, t5, r0);                            // pextlb t5, t5, r0
  bc = c->sgpr64(s3) != c->sgpr64(t9);              // bne s3, t9, L41
  c->psrah(t5, t5, 8);                              // psrah t5, t5, 8
  if (bc) {goto block_23;}                          // branch non-likely

  //todo // Unknown instr: pmfhl.uw t5
  c->gprs[t5].du32[0] = c->lo.du32[1];
  c->gprs[t5].du32[1] = c->hi.du32[1];
  c->gprs[t5].du32[2] = c->lo.du32[3];
  c->gprs[t5].du32[3] = c->hi.du32[3];
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psraw(t7, t7, 13);                             // psraw t7, t7, 13
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psraw(t5, t5, 13);                             // psraw t5, t5, 13
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pinteh(t5, t5, t7);                            // pinteh t5, t5, t7
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pminh(t3, t3, t5);                             // pminh t3, t3, t5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaxh(t4, t4, t5);                             // pmaxh t4, t4, t5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pminh(t5, t5, t1);                             // pminh t5, t5, t1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaxh(t5, t5, r0);                             // pmaxh t5, t5, r0
  c->lq(t7, 0, s4);                                 // lq t7, 0(s4)
  c->ppacb(t5, r0, t5);                             // ppacb t5, r0, t5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppach(t7, r0, t7);                             // ppach t7, r0, t7
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(t5, t5, t7);                            // pextlh t5, t5, t7
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->sq(t5, 0, t0);                                 // sq t5, 0(t0)
  c->daddiu(t0, t0, 16);                            // daddiu t0, t0, 16
  bc = c->sgpr64(s5) != 0;                          // bne s5, r0, L40
  c->daddiu(s4, s4, 16);                            // daddiu s4, s4, 16
  if (bc) {goto block_22;}                          // branch non-likely

  c->load_symbol(a3, cache.stats_blerc);            // lw a3, *stats-blerc*(s7)
  bc = c->sgpr64(a3) == c->sgpr64(s7);              // beq a3, s7, L43
  c->load_symbol(a3, cache.blerc_globals);          // lw a3, *blerc-globals*(s7)
  if (bc) {goto block_28;}                          // branch non-likely

  c->lw(t2, 12, a3);                                // lw t2, 12(a3)
  c->lw(t1, 16, a3);                                // lw t1, 16(a3)
  c->lw(t0, 20, a3);                                // lw t0, 20(a3)
  c->lw(a2, 864, a2);                               // lw a2, 864(a2)
  c->multu3(a1, a1, a2);                            // multu3 a1, a1, a2
  c->daddiu(t2, t2, 1);                             // daddiu t2, t2, 1
  c->daddu(a2, t1, a2);                             // daddu a2, t1, a2
  c->daddu(a1, t0, a1);                             // daddu a1, t0, a1
  c->sw(t2, 12, a3);                                // sw t2, 12(a3)
  c->sw(a2, 16, a3);                                // sw a2, 16(a3)
  c->sw(a1, 20, a3);                                // sw a1, 20(a3)
  c->pcpyud(a1, t3, r0);                            // pcpyud a1, t3, r0
  c->pminh(t3, t3, a1);                             // pminh t3, t3, a1
  c->dsrl32(a1, t3, 0);                             // dsrl32 a1, t3, 0
  c->pminh(t3, t3, a1);                             // pminh t3, t3, a1
  c->dsrl(t3, t3, 16);                              // dsrl t3, t3, 16
  c->lh(a1, 8, a3);                                 // lh a1, 8(a3)
  c->pminh(t3, t3, a1);                             // pminh t3, t3, a1
  c->sh(t3, 8, a3);                                 // sh t3, 8(a3)
  c->pcpyud(a1, t4, r0);                            // pcpyud a1, t4, r0
  c->pmaxh(t4, t4, a1);                             // pmaxh t4, t4, a1
  c->dsrl32(a1, t4, 0);                             // dsrl32 a1, t4, 0
  c->pmaxh(t4, t4, a1);                             // pmaxh t4, t4, a1
  c->dsrl(t4, t4, 16);                              // dsrl t4, t4, 16
  c->lh(a1, 10, a3);                                // lh a1, 10(a3)
  c->pmaxh(t4, t4, a1);                             // pmaxh t4, t4, a1
  c->sh(t4, 10, a3);                                // sh t4, 10(a3)

  block_28:
  c->gprs[a1].du64[0] = 0;                          // or a1, r0, r0
  c->lwu(a1, 872, a0);                              // lwu a1, 872(a0)
  c->mov64(a3, a0);                                 // or a3, a0, r0
  c->lwu(a0, 876, a0);                              // lwu a0, 876(a0)
  c->lui(a2, 4096);                                 // lui a2, 4096
  // nop                                            // sll r0, r0, 0
  c->ori(a2, a2, 53248);                            // ori a2, a2, 53248
  c->andi(a3, a3, 16383);                           // andi a3, a3, 16383

/*
  block_29:
  c->lw(t0, 0, a2);                                 // lw t0, 0(a2)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(t0, t0, 256);                             // andi t0, t0, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t0) != 0;                          // bne t0, r0, L44
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_29;}                          // branch non-likely
  */

  // c->sw(a3, 128, a2);                               // sw a3, 128(a2)
  sadr = c->sgpr64(a3);
  c->addiu(a3, r0, 256);                            // addiu a3, r0, 256
  // c->sw(a1, 16, a2);                                // sw a1, 16(a2)
  madr = c->sgpr64(a1);
  // nop                                            // sll r0, r0, 0
  // c->sw(a0, 32, a2);                                // sw a0, 32(a2)
  qwc = c->sgpr64(a0);
  // Unknown instr: sync.l
  // c->sw(a3, 0, a2);                                 // sw a3, 0(a2)
  spad_from_dma_no_sadr_off(cache.fake_scratchpad_data, madr, sadr, qwc);
  // Unknown instr: sync.l
  c->gprs[a0].du64[0] = 0;                          // or a0, r0, r0
  c->addiu(a0, r0, 1);                              // addiu a0, r0, 1
  c->dsubu(gp, a0, gp);                             // dsubu gp, a0, gp
  c->lwu(s5, 12, v1);                               // lwu s5, 12(v1)
  c->mov64(v1, s5);                                 // or v1, s5, r0

  block_31:
  bc = c->sgpr64(s5) != 0;                          // bne s5, r0, L30
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_6;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0

  block_33:
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
  cache.blerc_globals = intern_from_c("*blerc-globals*").c();
  cache.gsf_buffer = intern_from_c("*gsf-buffer*").c();
  cache.stats_blerc = intern_from_c("*stats-blerc*").c();
  cache.flush_cache = intern_from_c("flush-cache").c();
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("blerc-execute", execute, 256);
}

} // namespace blerc_execute
} // namespace Mips2C


//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace setup_blerc_chains_for_one_fragment {
struct Cache {
  void* blerc_globals; // *blerc-globals*
  void* fake_scratchpad_data;  // *fake-scratchpad-data*

} cache;

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
  c->lb(v1, 0, t0);                                 // lb v1, 0(t0)
  c->addiu(t2, r0, 0);                              // addiu t2, r0, 0
  c->mov128_gpr_gpr(t7, r0);                        // por t7, r0, r0
  c->load_symbol(t3, cache.blerc_globals);          // lw t3, *blerc-globals*(s7)
  c->lw(t3, 4, t3);                                 // lw t3, 4(t3)
  c->mov64(t4, v1);                                 // or t4, v1, r0

  block_1:
  c->lui(t7, 4096);                                 // lui t7, 4096
  // nop                                            // sll r0, r0, 0
  c->daddiu(t7, t7, 1);                             // daddiu t7, t7, 1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L11
  c->sq(t7, 0, a2);                                 // sq t7, 0(a2)
  if (bc) {goto block_3;}                           // branch non-likely

  c->sw(a2, 12, t3);                                // sw a2, 12(t3)
  // nop                                            // sll r0, r0, 0

  block_3:
  c->mov64(t3, a2);                                 // or t3, a2, r0
  c->daddu(t6, t4, t4);                             // daddu t6, t4, t4
  c->daddu(t5, v1, v1);                             // daddu t5, v1, v1
  c->daddu(t6, t6, t4);                             // daddu t6, t6, t4
  c->daddu(t7, t5, v1);                             // daddu t7, t5, v1
  c->daddu(t5, t6, t6);                             // daddu t5, t6, t6
  c->daddu(t6, t7, t7);                             // daddu t6, t7, t7
  c->daddu(t7, t5, t5);                             // daddu t7, t5, t5
  c->daddiu(t6, t6, 15);                            // daddiu t6, t6, 15
  c->daddiu(t5, t5, 15);                            // daddiu t5, t5, 15
  c->andi(t6, t6, 65520);                           // andi t6, t6, 65520
  c->dsrl(t5, t5, 4);                               // dsrl t5, t5, 4
  c->daddu(t8, t2, t2);                             // daddu t8, t2, t2
  c->daddiu(t7, t7, 15);                            // daddiu t7, t7, 15
  c->daddu(t9, t8, t2);                             // daddu t9, t8, t2
  c->dsrl(t8, t7, 4);                               // dsrl t8, t7, 4
  c->daddu(ra, t9, t9);                             // daddu ra, t9, t9
  c->addiu(t9, r0, 0);                              // addiu t9, r0, 0
  c->daddu(t7, ra, ra);                             // daddu t7, ra, ra
  c->daddiu(s3, a2, 32);                            // daddiu s3, a2, 32
  c->daddu(s2, ra, a3);                             // daddu s2, ra, a3
  c->daddu(ra, t7, t1);                             // daddu ra, t7, t1
  c->lui(t7, 12288);                                // lui t7, 12288
  c->daddiu(gp, a0, -1);                            // daddiu gp, a0, -1
  c->daddu(t7, t7, t5);                             // daddu t7, t7, t5
  c->mov64(s5, a1);                                 // or s5, a1, r0
  c->sq(t7, 0, s3);                                 // sq t7, 0(s3)
  c->daddiu(s4, t0, 2);                             // daddiu s4, t0, 2
  c->sw(s2, 4, s3);                                 // sw s2, 4(s3)
  c->daddu(s2, s2, t6);                             // daddu s2, s2, t6
  c->daddiu(s3, s3, 16);                            // daddiu s3, s3, 16
  // nop                                            // sll r0, r0, 0

  block_4:
  c->lb(s1, 0, s4);                                 // lb s1, 0(s4)
  c->daddiu(s4, s4, 1);                             // daddiu s4, s4, 1
  c->lh(s0, 0, s5);                                 // lh s0, 0(s5)
  c->daddiu(s5, s5, 2);                             // daddiu s5, s5, 2
  bc = c->sgpr64(s1) == 0;                          // beq s1, r0, L13
  c->sq(t7, 0, s3);                                 // sq t7, 0(s3)
  if (bc) {goto block_7;}                           // branch non-likely

  c->sw(s2, 4, s3);                                 // sw s2, 4(s3)
  c->daddu(s2, s2, t6);                             // daddu s2, s2, t6
  bc = c->sgpr64(s0) == 0;                          // beq s0, r0, L13
  c->sw(s0, 12, s3);                                // sw s0, 12(s3) no
  if (bc) {goto block_7;}                           // branch non-likely

  c->daddiu(s3, s3, 16);                            // daddiu s3, s3, 16
  c->daddiu(t9, t9, 1);                             // daddiu t9, t9, 1

  block_7:
  bc = c->sgpr64(gp) != 0;                          // bne gp, r0, L12
  c->daddiu(gp, gp, -1);                            // daddiu gp, gp, -1
  if (bc) {goto block_4;}                           // branch non-likely

  c->sq(t7, 0, s3);                                 // sq t7, 0(s3)
  c->mov128_gpr_gpr(t6, r0);                        // por t6, r0, r0
  c->sw(ra, 4, s3);                                 // sw ra, 4(s3)
  // c->lui(t6, 28672);                                // lui t6, 28672
  get_fake_spad_addr(t6, cache.fake_scratchpad_data, 0, c);
  c->sb(t8, 0, s3);                                 // sb t8, 0(s3)
  // nop                                            // sll r0, r0, 0
  c->sq(t6, 16, s3);                                // sq t6, 16(s3)
  c->daddiu(t6, s3, 32);                            // daddiu t6, s3, 32
  c->sw(t9, 20, a2);                                // sw t9, 20(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(t4, 16, a2);                                // sw t4, 16(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(ra, 24, a2);                                // sw ra, 24(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(t8, 28, a2);                                // sw t8, 28(a2)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t4) != c->sgpr64(v1);              // bne t4, v1, L14
  c->daddiu(t5, t5, 1);                             // daddiu t5, t5, 1
  if (bc) {goto block_11;}                          // branch non-likely

  c->daddiu(t7, t9, 3);                             // daddiu t7, t9, 3
  c->multu3(t5, t5, t7);                            // multu3 t5, t5, t7
  c->daddiu(t5, t5, -457);                          // daddiu t5, t5, -457
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(t5)) <= 0;                   // blez t5, L14
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_11;}                          // branch non-likely

  //beq r0, r0, L10                                 // beq r0, r0, L10
  c->addiu(t4, r0, 24);                             // addiu t4, r0, 24
  goto block_1;                                     // branch always


  block_11:
  c->mov64(a2, t6);                                 // or a2, t6, r0
  c->daddu(t2, t2, t4);                             // daddu t2, t2, t4
  bc = c->sgpr64(t2) == c->sgpr64(v1);              // beq t2, v1, L15
  c->daddu(t5, t2, t4);                             // daddu t5, t2, t4
  if (bc) {goto block_14;}                          // branch non-likely

  c->dsubu(t5, t5, v1);                             // dsubu t5, t5, v1
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(t5)) <= 0;                   // blez t5, L10
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  //beq r0, r0, L10                                 // beq r0, r0, L10
  c->dsubu(t4, v1, t2);                             // dsubu t4, v1, t2
  goto block_1;                                     // branch always


  block_14:
  c->load_symbol(v1, cache.blerc_globals);          // lw v1, *blerc-globals*(s7)
  c->sw(t3, 4, v1);                                 // sw t3, 4(v1)
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
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.blerc_globals = intern_from_c("*blerc-globals*").c();
  gLinkedFunctionTable.reg("setup-blerc-chains-for-one-fragment", execute, 256);
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
}

} // namespace setup_blerc_chains_for_one_fragment
} // namespace Mips2C
