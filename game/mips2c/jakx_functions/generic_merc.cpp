//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace generic_merc_do_chain {
struct Cache {
  void* display; // *display*
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* viewport_array; // *viewport-array*
  void* dma_bucket_insert_tag; // dma-bucket-insert-tag
  void* generic_merc_execute_asm; // generic-merc-execute-asm
  void* generic_work_init; // generic-work-init
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -80);                           // daddiu sp, sp, -80
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s3, 16, sp);                                // sq s3, 16(sp)
  c->sq(s4, 32, sp);                                // sq s4, 32(sp)
  c->sq(s5, 48, sp);                                // sq s5, 48(sp)
  c->sq(gp, 64, sp);                                // sq gp, 64(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->mov64(s5, a1);                                 // or s5, a1, r0
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->sw(gp, 7456, v1);                              // sw gp, 7456(v1)
  c->daddiu(a0, gp, 8);                             // daddiu a0, gp, 8
  c->load_symbol2(v1, cache.viewport_array);        // lw v1, *viewport-array*(s7)
  c->addiu(a1, r0, 688);                            // addiu a1, r0, 688
  c->lwu(a2, 8, v1);                                // lwu a2, 8(v1)
  c->multu3(a1, a1, a2);                            // multu3 a1, a1, a2
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddu(v1, a1, v1);                             // daddu v1, a1, v1
  c->lwu(v1, 0, gp);                                // lwu v1, 0(gp)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L17
  c->mov64(v0, s7);                                 // or v0, s7, r0
  if (bc) {goto block_7;}                           // branch non-likely

  c->load_symbol2(v1, cache.display);               // lw v1, *display*(s7)
  c->lw(v1, 0, v1);                                 // lw v1, 0(v1)
  c->dsll(v1, v1, 2);                               // dsll v1, v1, 2
  c->load_symbol2(a1, cache.display);               // lw a1, *display*(s7)
  c->daddu(v1, v1, a1);                             // daddu v1, v1, a1
  c->lwu(v1, 8, v1);                                // lwu v1, 8(v1)
  c->lwu(s3, 36, v1);                               // lwu s3, 36(v1)
  c->lwu(s4, 4, s3);                                // lwu s4, 4(s3)
  c->load_symbol2(t9, cache.generic_work_init);     // lw t9, generic-work-init(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lwu(v1, 4, s3);                                // lwu v1, 4(s3)
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->sw(v1, 60, a0);                                // sw v1, 60(a0)
  c->load_symbol2(t9, cache.generic_merc_execute_asm);// lw t9, generic-merc-execute-asm(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(v1, 60, v1);                               // lwu v1, 60(v1)
  c->sw(v1, 4, s3);                                 // sw v1, 4(s3)
  c->lui(v1, 4096);                                 // lui v1, 4096
  c->ori(v1, v1, 53248);                            // ori v1, v1, 53248
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->ori(a0, a0, 92);                               // ori a0, a0, 92
  c->lw(a1, 0, v1);                                 // lw a1, 0(v1)
  c->andi(a1, a1, 256);                             // andi a1, a1, 256
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L15
  //   TODO - fix DANGER jump to delay slot, this MUST be fixed manually!
  c->lw(a1, 0, a0);                                 // lw a1, 0(a0)
  //   TODO - fix if (bc) {goto block_-1;}                          // branch non-likely

  
block_2:
  c->lw(a1, 0, a0);                                 // lw a1, 0(a0)
  // nop                                            // sll r0, r0, 0
  c->lw(a2, 0, v1);                                 // lw a2, 0(v1)
  // nop                                            // sll r0, r0, 0
  c->andi(a2, a2, 256);                             // andi a2, a2, 256
  c->daddiu(a1, a1, 1);                             // daddiu a1, a1, 1
  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L14
  c->sw(a1, 0, a0);                                 // sw a1, 0(a0)
  if (bc) {goto block_2;}                           // branch non-likely

  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->lwu(a3, 4, s3);                                // lwu a3, 4(s3)
  bc = c->sgpr64(s4) == c->sgpr64(a3);              // beq s4, a3, L16
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_6;}                           // branch non-likely

  c->lwu(v1, 4, s3);                                // lwu v1, 4(s3)
  c->lui(a0, 8192);                                 // lui a0, 8192
  c->sd(a0, 0, v1);                                 // sd a0, 0(v1)
  c->sw(r0, 8, v1);                                 // sw r0, 8(v1)
  c->sw(r0, 12, v1);                                // sw r0, 12(v1)
  c->daddiu(v1, v1, 16);                            // daddiu v1, v1, 16
  c->sw(v1, 4, s3);                                 // sw v1, 4(s3)
  c->load_symbol2(t9, cache.dma_bucket_insert_tag); // lw t9, dma-bucket-insert-tag(s7)
  c->load_symbol2(v1, cache.display);               // lw v1, *display*(s7)
  c->lw(v1, 0, v1);                                 // lw v1, 0(v1)
  c->dsll(v1, v1, 2);                               // dsll v1, v1, 2
  c->load_symbol2(a0, cache.display);               // lw a0, *display*(s7)
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  c->lwu(v1, 8, v1);                                // lwu v1, 8(v1)
  c->lwu(a0, 40, v1);                               // lwu a0, 40(v1)
  c->lw(a1, 16, gp);                                // lw a1, 16(gp)
  c->mov64(a2, s4);                                 // or a2, s4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  
block_6:
  c->lwu(v1, 4, s5);                                // lwu v1, 4(s5)
  // Unknown instr: sync.l
  // Unknown instr: cache dxwbin v1, 0
  // Unknown instr: sync.l
  // Unknown instr: cache dxwbin v1, 1
  // Unknown instr: sync.l
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  
block_7:
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 64, sp);                                // lq gp, 64(sp)
  c->lq(s5, 48, sp);                                // lq s5, 48(sp)
  c->lq(s4, 32, sp);                                // lq s4, 32(sp)
  c->lq(s3, 16, sp);                                // lq s3, 16(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 80);                            // daddiu sp, sp, 80
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.display = intern_from_c(-1, 0, "*display*").c();
  cache.fake_scratchpad_data = intern_from_c(-1, 0, "*fake-scratchpad-data*").c();
  cache.viewport_array = intern_from_c(-1, 0, "*viewport-array*").c();
  cache.dma_bucket_insert_tag = intern_from_c(-1, 0, "dma-bucket-insert-tag").c();
  cache.generic_merc_execute_asm = intern_from_c(-1, 0, "generic-merc-execute-asm").c();
  cache.generic_work_init = intern_from_c(-1, 0, "generic-work-init").c();
  gLinkedFunctionTable.reg("generic-merc-do-chain", execute, 256);
}

} // namespace generic_merc_do_chain
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace generic_merc_execute_asm {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* gsf_buffer; // *gsf-buffer*
  void* generic_merc_death; // generic-merc-death
  void* generic_merc_query; // generic-merc-query
  void* generic_translucent; // generic-translucent
  void* generic_warp_dest; // generic-warp-dest
  void* generic_warp_envmap_dest; // generic-warp-envmap-dest
  void* generic_warp_source; // generic-warp-source
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
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(v1, 7456, v1);                             // lwu v1, 7456(v1)
  c->lwu(s2, 0, v1);                                // lwu s2, 0(v1)
  c->addiu(v1, r0, 0);                              // addiu v1, r0, 0
  c->addiu(gp, r0, 0);                              // addiu gp, r0, 0
  c->addiu(s5, r0, 0);                              // addiu s5, r0, 0
  c->load_symbol2(s4, cache.gsf_buffer);            // lw s4, *gsf-buffer*(s7)
  c->lui(v1, 4096);                                 // lui v1, 4096
  c->ori(s3, v1, 54272);                            // ori s3, v1, 54272
  c->addiu(v1, r0, 624);                            // addiu v1, r0, 624
  c->lui(a0, 4096);                                 // lui a0, 4096
  c->ori(a0, a0, 54272);                            // ori a0, a0, 54272
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->ori(a1, a1, 88);                               // ori a1, a1, 88
  c->lw(a2, 0, a0);                                 // lw a2, 0(a0)
  c->andi(a2, a2, 256);                             // andi a2, a2, 256
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L20
  //   TODO - fix DANGER jump to delay slot, this MUST be fixed manually!
  c->lw(a2, 0, a1);                                 // lw a2, 0(a1)
  //   TODO - fix if (bc) {goto block_-1;}                          // branch non-likely

  
block_1:
  c->lw(a2, 0, a1);                                 // lw a2, 0(a1)
  // nop                                            // sll r0, r0, 0
  c->lw(a3, 0, a0);                                 // lw a3, 0(a0)
  // nop                                            // sll r0, r0, 0
  c->andi(a3, a3, 256);                             // andi a3, a3, 256
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L19
  c->sw(a2, 0, a1);                                 // sw a2, 0(a1)
  if (bc) {goto block_1;}                           // branch non-likely

  c->gprs[a0].du64[0] = 0;                          // or a0, r0, r0
  c->sw(v1, 128, s3);                               // sw v1, 128(s3)
  c->sw(s2, 48, s3);                                // sw s2, 48(s3)
  c->sw(r0, 32, s3);                                // sw r0, 32(s3)
  // Unknown instr: sync.l
  c->addiu(v1, r0, 324);                            // addiu v1, r0, 324
  c->sw(v1, 0, s3);                                 // sw v1, 0(s3)
  // Unknown instr: sync.l
  //beq r0, r0, L66                                 // beq r0, r0, L66
  // nop                                            // sll r0, r0, 0
  goto block_88;                                    // branch always

  
block_4:
  bc = c->sgpr64(gp) != 0;                          // bne gp, r0, L22
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_6;}                           // branch non-likely

  c->addiu(v1, r0, 624);                            // addiu v1, r0, 624
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  //beq r0, r0, L23                                 // beq r0, r0, L23
  // nop                                            // sll r0, r0, 0
  goto block_7;                                     // branch always

  
block_6:
  c->addiu(v1, r0, 3648);                           // addiu v1, r0, 3648
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  
block_7:
  bc = c->sgpr64(gp) != 0;                          // bne gp, r0, L24
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_9;}                           // branch non-likely

  c->addiu(a0, r0, 3648);                           // addiu a0, r0, 3648
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->daddu(a0, a0, a1);                             // daddu a0, a0, a1
  //beq r0, r0, L25                                 // beq r0, r0, L25
  // nop                                            // sll r0, r0, 0
  goto block_10;                                    // branch always

  
block_9:
  c->addiu(a0, r0, 624);                            // addiu a0, r0, 624
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->daddu(a0, a0, a1);                             // daddu a0, a0, a1
  
block_10:
  bc = c->sgpr64(gp) != 0;                          // bne gp, r0, L26
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely

  c->addiu(a1, r0, 3568);                           // addiu a1, r0, 3568
  get_fake_spad_addr2(a2, cache.fake_scratchpad_data, 0, c);// lui a2, 28672
  c->daddu(a1, a1, a2);                             // daddu a1, a1, a2
  //beq r0, r0, L27                                 // beq r0, r0, L27
  // nop                                            // sll r0, r0, 0
  goto block_13;                                    // branch always

  
block_12:
  c->addiu(a1, r0, 6592);                           // addiu a1, r0, 6592
  get_fake_spad_addr2(a2, cache.fake_scratchpad_data, 0, c);// lui a2, 28672
  c->daddu(a1, a1, a2);                             // daddu a1, a1, a2
  
block_13:
  bc = c->sgpr64(gp) != 0;                          // bne gp, r0, L28
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_15;}                          // branch non-likely

  c->addiu(a1, r0, 6592);                           // addiu a1, r0, 6592
  get_fake_spad_addr2(a2, cache.fake_scratchpad_data, 0, c);// lui a2, 28672
  c->daddu(a1, a1, a2);                             // daddu a1, a1, a2
  //beq r0, r0, L29                                 // beq r0, r0, L29
  // nop                                            // sll r0, r0, 0
  goto block_16;                                    // branch always

  
block_15:
  c->addiu(a1, r0, 3568);                           // addiu a1, r0, 3568
  get_fake_spad_addr2(a2, cache.fake_scratchpad_data, 0, c);// lui a2, 28672
  c->daddu(a1, a1, a2);                             // daddu a1, a1, a2
  
block_16:
  bc = c->sgpr64(gp) != 0;                          // bne gp, r0, L30
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_18;}                          // branch non-likely

  c->daddu(s1, r0, s4);                             // daddu s1, r0, s4
  //beq r0, r0, L31                                 // beq r0, r0, L31
  // nop                                            // sll r0, r0, 0
  goto block_19;                                    // branch always

  
block_18:
  c->daddiu(s1, s4, 2752);                          // daddiu s1, s4, 2752
  
block_19:
  bc = c->sgpr64(gp) != 0;                          // bne gp, r0, L32
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_21;}                          // branch non-likely

  c->daddiu(a1, s4, 2752);                          // daddiu a1, s4, 2752
  //beq r0, r0, L33                                 // beq r0, r0, L33
  // nop                                            // sll r0, r0, 0
  goto block_22;                                    // branch always

  
block_21:
  c->daddu(a1, r0, s4);                             // daddu a1, r0, s4
  
block_22:
  c->addiu(a2, r0, 7136);                           // addiu a2, r0, 7136
  get_fake_spad_addr2(a3, cache.fake_scratchpad_data, 0, c);// lui a3, 28672
  c->daddu(a2, a2, a3);                             // daddu a2, a2, a3
  c->sw(gp, 280, a2);                               // sw gp, 280(a2)
  c->sw(s5, 284, a2);                               // sw s5, 284(a2)
  c->sw(v1, 292, a2);                               // sw v1, 292(a2)
  c->sw(a0, 296, a2);                               // sw a0, 296(a2)
  c->sw(s1, 300, a2);                               // sw s1, 300(a2)
  c->sw(a1, 304, a2);                               // sw a1, 304(a2)
  c->sw(s4, 312, a2);                               // sw s4, 312(a2)
  c->mov64(a1, s4);                                 // or a1, s4, r0
  c->lui(a1, 4096);                                 // lui a1, 4096
  c->ori(a1, a1, 54272);                            // ori a1, a1, 54272
  get_fake_spad_addr2(a2, cache.fake_scratchpad_data, 0, c);// lui a2, 28672
  c->ori(a2, a2, 88);                               // ori a2, a2, 88
  c->lw(a3, 0, a1);                                 // lw a3, 0(a1)
  c->andi(a3, a3, 256);                             // andi a3, a3, 256
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L35
  //   TODO - fix DANGER jump to delay slot, this MUST be fixed manually!
  c->lw(a3, 0, a2);                                 // lw a3, 0(a2)
  //   TODO - fix if (bc) {goto block_-1;}                          // branch non-likely

  
block_23:
  c->lw(a3, 0, a2);                                 // lw a3, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->lw(t0, 0, a1);                                 // lw t0, 0(a1)
  // nop                                            // sll r0, r0, 0
  c->andi(t0, t0, 256);                             // andi t0, t0, 256
  c->daddiu(a3, a3, 1);                             // daddiu a3, a3, 1
  bc = c->sgpr64(t0) != 0;                          // bne t0, r0, L34
  c->sw(a3, 0, a2);                                 // sw a3, 0(a2)
  if (bc) {goto block_23;}                          // branch non-likely

  c->gprs[a1].du64[0] = 0;                          // or a1, r0, r0
  c->lwu(s2, 12, v1);                               // lwu s2, 12(v1)
  bc = c->sgpr64(s2) == 0;                          // beq s2, r0, L36
  c->mov64(a1, s7);                                 // or a1, s7, r0
  if (bc) {goto block_27;}                          // branch non-likely

  c->daddu(a0, r0, a0);                             // daddu a0, r0, a0
  c->andi(a0, a0, 65535);                           // andi a0, a0, 65535
  c->sw(a0, 128, s3);                               // sw a0, 128(s3)
  c->sw(s2, 48, s3);                                // sw s2, 48(s3)
  c->sw(r0, 32, s3);                                // sw r0, 32(s3)
  // Unknown instr: sync.l
  c->addiu(a0, r0, 324);                            // addiu a0, r0, 324
  c->sw(a0, 0, s3);                                 // sw a0, 0(s3)
  // Unknown instr: sync.l
  c->gprs[a0].du64[0] = 0;                          // or a0, r0, r0
  
block_27:
  bc = c->sgpr64(s5) != 0;                          // bne s5, r0, L45
  c->mov64(a0, s7);                                 // or a0, s7, r0
  if (bc) {goto block_44;}                          // branch non-likely

  c->addiu(a0, r0, 6672);                           // addiu a0, r0, 6672
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->daddu(a1, a0, a1);                             // daddu a1, a0, a1
  c->mov64(a2, v1);                                 // or a2, v1, r0
  c->lwu(a0, 8, v1);                                // lwu a0, 8(v1)
  // nop                                            // sll r0, r0, 0
  c->daddiu(a3, a0, -4);                            // daddiu a3, a0, -4
  c->mov64(a1, a1);                                 // or a1, a1, r0
  bc = ((s64)c->sgpr64(a3)) < 0;                    // bltz a3, L38
  c->mov64(a2, a2);                                 // or a2, a2, r0
  if (bc) {goto block_30;}                          // branch non-likely

  
block_29:
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
  bc = ((s64)c->sgpr64(t2)) >= 0;                   // bgez t2, L37
  c->sq(t1, -16, a1);                               // sq t1, -16(a1)
  if (bc) {goto block_29;}                          // branch non-likely

  
block_30:
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L39
  c->lq(a3, 0, a2);                                 // lq a3, 0(a2)
  if (bc) {goto block_35;}                          // branch non-likely

  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, -16, a1);                               // sq a3, -16(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L39
  c->lq(a3, 0, a2);                                 // lq a3, 0(a2)
  if (bc) {goto block_35;}                          // branch non-likely

  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, -16, a1);                               // sq a3, -16(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L39
  c->lq(a3, 0, a2);                                 // lq a3, 0(a2)
  if (bc) {goto block_35;}                          // branch non-likely

  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, -16, a1);                               // sq a3, -16(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L39
  c->lq(a3, 0, a2);                                 // lq a3, 0(a2)
  if (bc) {goto block_35;}                          // branch non-likely

  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, -16, a1);                               // sq a3, -16(a1)
  
block_35:
  c->gprs[a0].du64[0] = 0;                          // or a0, r0, r0
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->lbu(a0, 6840, a0);                             // lbu a0, 6840(a0)
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L40
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_37;}                          // branch non-likely

  c->addiu(a0, r0, 8);                              // addiu a0, r0, 8
  //beq r0, r0, L41                                 // beq r0, r0, L41
  // nop                                            // sll r0, r0, 0
  goto block_38;                                    // branch always

  
block_37:
  c->addiu(a0, r0, 6);                              // addiu a0, r0, 6
  
block_38:
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->ori(a1, a1, 11984);                            // ori a1, a1, 11984
  c->sh(a0, 0, a1);                                 // sh a0, 0(a1)
  c->addiu(a0, r0, 12048);                          // addiu a0, r0, 12048
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->daddu(a0, a0, a1);                             // daddu a0, a0, a1
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->lwu(a1, 6832, a1);                             // lwu a1, 6832(a1)
  c->sw(a1, 0, a0);                                 // sw a1, 0(a0)
  c->sw(a1, 4, a0);                                 // sw a1, 4(a0)
  c->sw(a1, 8, a0);                                 // sw a1, 8(a0)
  c->sw(a1, 12, a0);                                // sw a1, 12(a0)
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->addiu(a0, a1, 7136);                           // addiu a0, a1, 7136
  c->lbu(a2, 6841, a1);                             // lbu a2, 6841(a1)
  c->addiu(a3, a1, 7200);                           // addiu a3, a1, 7200
  c->addiu(a1, a1, 11808);                          // addiu a1, a1, 11808
  c->movn(a0, a3, a2);                              // movn a0, a3, a2
  c->lq(a2, 0, a0);                                 // lq a2, 0(a0)
  c->lq(a3, 16, a0);                                // lq a3, 16(a0)
  c->lq(t0, 32, a0);                                // lq t0, 32(a0)
  c->lq(a0, 48, a0);                                // lq a0, 48(a0)
  c->sq(a2, 0, a1);                                 // sq a2, 0(a1)
  c->sq(a3, 16, a1);                                // sq a3, 16(a1)
  c->sq(t0, 32, a1);                                // sq t0, 32(a1)
  c->sq(a0, 48, a1);                                // sq a0, 48(a1)
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->lbu(a0, 6843, a0);                             // lbu a0, 6843(a0)
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->sb(a0, 6853, a1);                              // sb a0, 6853(a1)
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->lqc2(vf1, 6688, at);                           // lqc2 vf1, 6688(at)
  c->lqc2(vf2, 6704, at);                           // lqc2 vf2, 6704(at)
  c->lqc2(vf3, 6720, at);                           // lqc2 vf3, 6720(at)
  c->lqc2(vf4, 6736, at);                           // lqc2 vf4, 6736(at)
  c->lqc2(vf5, 6752, at);                           // lqc2 vf5, 6752(at)
  c->lqc2(vf6, 6768, at);                           // lqc2 vf6, 6768(at)
  c->lqc2(vf7, 6784, at);                           // lqc2 vf7, 6784(at)
  c->sqc2(vf1, 12688, at);                          // sqc2 vf1, 12688(at)
  c->sqc2(vf2, 12704, at);                          // sqc2 vf2, 12704(at)
  c->sqc2(vf3, 12720, at);                          // sqc2 vf3, 12720(at)
  c->sqc2(vf4, 12736, at);                          // sqc2 vf4, 12736(at)
  c->sqc2(vf5, 12752, at);                          // sqc2 vf5, 12752(at)
  c->sqc2(vf6, 12768, at);                          // sqc2 vf6, 12768(at)
  c->sqc2(vf7, 12784, at);                          // sqc2 vf7, 12784(at)
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->lbu(a0, 6852, a0);                             // lbu a0, 6852(a0)
  c->andi(a0, a0, 2);                               // andi a0, a0, 2
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L42
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_40;}                          // branch non-likely

  c->lui(a0, 12342);                                // lui a0, 12342
  c->ori(a0, a0, 16384);                            // ori a0, a0, 16384
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->sw(a0, 11876, a1);                             // sw a0, 11876(a1)
  c->lui(a0, 12342);                                // lui a0, 12342
  c->ori(a1, a0, 49152);                            // ori a1, a0, 49152
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->sw(a1, 11872, a0);                             // sw a1, 11872(a0)
  //beq r0, r0, L43                                 // beq r0, r0, L43
  // nop                                            // sll r0, r0, 0
  goto block_41;                                    // branch always

  
block_40:
  c->lui(a0, 12350);                                // lui a0, 12350
  c->ori(a0, a0, 16384);                            // ori a0, a0, 16384
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->sw(a0, 11876, a1);                             // sw a0, 11876(a1)
  c->lui(a0, 12350);                                // lui a0, 12350
  c->ori(a1, a0, 49152);                            // ori a1, a0, 49152
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->sw(a1, 11872, a0);                             // sw a1, 11872(a0)
  
block_41:
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->lbu(a0, 6856, a0);                             // lbu a0, 6856(a0)
  c->addiu(a1, r0, 128);                            // addiu a1, r0, 128
  c->sltu(a1, a1, a0);                              // sltu a1, a1, a0
  bc = c->sgpr64(a1) != 0;                          // bne a1, r0, L44
  c->mov64(a1, s7);                                 // or a1, s7, r0
  if (bc) {goto block_43;}                          // branch non-likely

  c->lui(a1, 15360);                                // lui a1, 15360
  c->mtc1(f0, a1);                                  // mtc1 f0, a1
  c->mtc1(f1, a0);                                  // mtc1 f1, a0
  c->cvtsw(f1, f1);                                 // cvt.s.w f1, f1
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->lwc1(f1, 12796, a0);                           // lwc1 f1, 12796(a0)
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->swc1(f0, 12796, a0);                           // swc1 f0, 12796(a0)
  c->mfc1(a1, f0);                                  // mfc1 a1, f0
  
block_43:
  c->lwu(a0, 8, v1);                                // lwu a0, 8(v1)
  c->dsll(a0, a0, 4);                               // dsll a0, a0, 4
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  c->mov64(a0, v1);                                 // or a0, v1, r0
  
block_44:
  c->addiu(a0, r0, 7136);                           // addiu a0, r0, 7136
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->daddu(a0, a0, a1);                             // daddu a0, a0, a1
  c->daddiu(v1, v1, 16);                            // daddiu v1, v1, 16
  c->sw(v1, 308, a0);                               // sw v1, 308(a0)
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(t9, 7460, v1);                             // lwu t9, 7460(v1)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->addiu(a0, r0, 1);                              // addiu a0, r0, 1
  c->lbu(a1, 6840, v1);                             // lbu a1, 6840(v1)
  c->daddiu(a2, s1, 448);                           // daddiu a2, s1, 448
  c->lbu(a3, 6843, v1);                             // lbu a3, 6843(v1)
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L47
  c->lqc2(vf1, 0, a2);                              // lqc2 vf1, 0(a2)
  if (bc) {goto block_50;}                          // branch non-likely

  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L47
  c->vmax_bc(DEST::xyzw, BC::w, vf9, vf0, vf0);     // vmaxw.xyzw vf9, vf0, vf0
  if (bc) {goto block_50;}                          // branch non-likely

  c->sb(a0, 6853, v1);                              // sb a0, 6853(v1)
  c->lqc2(vf10, 7488, v1);                          // lqc2 vf10, 7488(v1)
  c->lqc2(vf11, 7504, v1);                          // lqc2 vf11, 7504(v1)
  c->vmula_bc(DEST::xyzw, BC::z, vf9, vf1);         // vmulaz.xyzw acc, vf9, vf1
  c->lw(a0, 8, a2);                                 // lw a0, 8(a2)
  c->vmadda_bc(DEST::xyzw, BC::x, vf10, vf1);       // vmaddax.xyzw acc, vf10, vf1
  bc = ((s64)c->sgpr64(a0)) < 0;                    // bltz a0, L46
  c->lw(v1, 7480, v1);                              // lw v1, 7480(v1)
  //   TODO - fix if (bc) {goto block_48;}                          // branch non-likely

  c->vmadd_bc(DEST::xyzw, BC::y, vf1, vf11, vf1);   // vmaddy.xyzw vf1, vf11, vf1
  c->mov128_gpr_vf(a0, vf1);                        // qmfc2.i a0, vf1
  c->pcgtw(a0, r0, a0);                             // pcgtw a0, r0, a0
  c->ppach(a0, r0, a0);                             // ppach a0, r0, a0
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L47
  //   TODO - fix DANGER jump to delay slot, this MUST be fixed manually!
  // nop                                            // sll r0, r0, 0
  //   TODO - fix if (bc) {goto block_-1;}                          // branch non-likely

  
block_48:
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[v1].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, v1
  
block_50:
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(v1, 6836, v1);                             // lwu v1, 6836(v1)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L48
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_52;}                          // branch non-likely

  c->load_symbol2(t9, cache.generic_merc_query);    // lw t9, generic-merc-query(s7)
  c->mov64(a0, s1);                                 // or a0, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  
block_52:
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lhu(v1, 6844, v1);                             // lhu v1, 6844(v1)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L49
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_54;}                          // branch non-likely

  c->load_symbol2(t9, cache.generic_merc_death);    // lw t9, generic-merc-death(s7)
  c->mov64(a0, s1);                                 // or a0, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  
block_54:
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lw(v1, 7424, v1);                              // lw v1, 7424(v1)
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->lwu(a0, 60, a0);                               // lwu a0, 60(a0)
  c->dsubu(v1, v1, a0);                             // dsubu v1, v1, a0
  c->slt(v1, r0, v1);                               // slt v1, r0, v1
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L50
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_56;}                          // branch non-likely

  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->sb(r0, 6853, v1);                              // sb r0, 6853(v1)
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->sb(r0, 6843, v1);                              // sb r0, 6843(v1)
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  
block_56:
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lbu(v1, 6853, v1);                             // lbu v1, 6853(v1)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L64
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_85;}                          // branch non-likely

  c->addiu(v1, r0, 1);                              // addiu v1, r0, 1
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->lbu(a0, 6854, a0);                             // lbu a0, 6854(a0)
  bc = c->sgpr64(a0) != c->sgpr64(v1);              // bne a0, v1, L51
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_59;}                          // branch non-likely

  c->load_symbol2(t9, cache.generic_warp_source);   // lw t9, generic-warp-source(s7)
  c->mov64(a0, s1);                                 // or a0, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  //beq r0, r0, L64                                 // beq r0, r0, L64
  // nop                                            // sll r0, r0, 0
  goto block_85;                                    // branch always

  
block_59:
  c->addiu(v1, r0, 2);                              // addiu v1, r0, 2
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->lbu(a0, 6854, a0);                             // lbu a0, 6854(a0)
  bc = c->sgpr64(a0) != c->sgpr64(v1);              // bne a0, v1, L54
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_64;}                          // branch non-likely

  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lbu(v1, 6955, v1);                             // lbu v1, 6955(v1)
  c->andi(v1, v1, 1);                               // andi v1, v1, 1
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L52
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_62;}                          // branch non-likely

  c->load_symbol2(t9, cache.generic_warp_envmap_dest);// lw t9, generic-warp-envmap-dest(s7)
  c->mov64(a0, s1);                                 // or a0, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  //beq r0, r0, L53                                 // beq r0, r0, L53
  // nop                                            // sll r0, r0, 0
  goto block_63;                                    // branch always

  
block_62:
  c->load_symbol2(t9, cache.generic_warp_dest);     // lw t9, generic-warp-dest(s7)
  c->mov64(a0, s1);                                 // or a0, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  
block_63:
  //beq r0, r0, L64                                 // beq r0, r0, L64
  // nop                                            // sll r0, r0, 0
  goto block_85;                                    // branch always

  
block_64:
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lbu(v1, 6955, v1);                             // lbu v1, 6955(v1)
  c->andi(v1, v1, 1);                               // andi v1, v1, 1
  if (((s64)c->sgpr64(v1)) == ((s64)0)) {           // beql v1, r0, L55
    c->mov64(v1, s7);                               // or v1, s7, r0
    goto block_67;
  }
  
block_66:
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->lbu(a0, 6852, a0);                             // lbu a0, 6852(a0)
  c->andi(a0, a0, 1);                               // andi a0, a0, 1
  c->movz(v1, s7, a0);                              // movz v1, s7, a0
  
block_67:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L56
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_69;}                          // branch non-likely

  c->load_symbol2(t9, cache.generic_translucent);   // lw t9, generic-translucent(s7)
  c->mov64(a0, s1);                                 // or a0, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  //beq r0, r0, L64                                 // beq r0, r0, L64
  // nop                                            // sll r0, r0, 0
  goto block_85;                                    // branch always

  
block_69:
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lbu(v1, 6955, v1);                             // lbu v1, 6955(v1)
  c->andi(v1, v1, 1);                               // andi v1, v1, 1
  if (((s64)c->sgpr64(v1)) == ((s64)0)) {           // beql v1, r0, L57
    c->mov64(v1, s7);                               // or v1, s7, r0
    goto block_72;
  }
  
block_71:
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lbu(v1, 6858, v1);                             // lbu v1, 6858(v1)
  c->daddiu(a0, v1, -1);                            // daddiu a0, v1, -1
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  c->movz(v1, s7, a0);                              // movz v1, s7, a0
  
block_72:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L61
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_80;}                          // branch non-likely

  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->mov64(v1, s1);                                 // or v1, s1, r0
  c->daddiu(a0, at, 12064);                         // daddiu a0, at, 12064
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->lbu(a1, 6842, a1);                             // lbu a1, 6842(a1)
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L58
  c->mov64(a1, s7);                                 // or a1, s7, r0
  if (bc) {goto block_75;}                          // branch non-likely

  c->addiu(a0, r0, 6960);                           // addiu a0, r0, 6960
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->daddu(a0, a0, a1);                             // daddu a0, a0, a1
  c->mov64(a0, a0);                                 // or a0, a0, r0
  c->mov64(a1, a0);                                 // or a1, a0, r0
  
block_75:
  c->sw(r0, 48, at);                                // sw r0, 48(at)
  c->sw(a0, 52, at);                                // sw a0, 52(at)
  c->sw(v1, 44, at);                                // sw v1, 44(at)
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(t9, 7468, v1);                             // lwu t9, 7468(v1)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(t9, 7472, v1);                             // lwu t9, 7472(v1)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(t9, 7476, v1);                             // lwu t9, 7476(v1)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lw(v1, 24, at);                                // lw v1, 24(at)
  c->lw(a0, 40, at);                                // lw a0, 40(at)
  c->mov64(a3, v1);                                 // or a3, v1, r0
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->lui(a2, 4096);                                 // lui a2, 4096
  c->lwu(a1, 60, at);                               // lwu a1, 60(at)
  c->ori(a2, a2, 53248);                            // ori a2, a2, 53248
  c->lw(t1, 0, a2);                                 // lw t1, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->daddiu(t0, at, 92);                            // daddiu t0, at, 92
  c->andi(a3, a3, 16383);                           // andi a3, a3, 16383
  c->andi(t1, t1, 256);                             // andi t1, t1, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L60
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_79;}                          // branch non-likely

  c->mov64(t1, a2);                                 // or t1, a2, r0
  // nop                                            // sll r0, r0, 0
  
block_77:
  c->lw(t2, 0, t0);                                 // lw t2, 0(t0)
  // nop                                            // sll r0, r0, 0
  c->lw(t3, 0, t1);                                 // lw t3, 0(t1)
  // nop                                            // sll r0, r0, 0
  c->andi(t3, t3, 256);                             // andi t3, t3, 256
  c->daddiu(t2, t2, 1);                             // daddiu t2, t2, 1
  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L59
  c->sw(t2, 0, t0);                                 // sw t2, 0(t0)
  if (bc) {goto block_77;}                          // branch non-likely

  c->gprs[t0].du64[0] = 0;                          // or t0, r0, r0
  
block_79:
  c->dsll(t0, a0, 4);                               // dsll t0, a0, 4
  c->sw(a3, 128, a2);                               // sw a3, 128(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(a1, 16, a2);                                // sw a1, 16(a2)
  c->addiu(a3, r0, 256);                            // addiu a3, r0, 256
  c->sw(a0, 32, a2);                                // sw a0, 32(a2)
  c->daddu(a0, a1, t0);                             // daddu a0, a1, t0
  c->sw(a3, 0, a2);                                 // sw a3, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 60, at);                                // sw a0, 60(at)
  c->gprs[a0].du64[0] = 0;                          // or a0, r0, r0
  c->xori(v1, v1, 4608);                            // xori v1, v1, 4608
  c->sw(v1, 24, at);                                // sw v1, 24(at)
  //beq r0, r0, L64                                 // beq r0, r0, L64
  // nop                                            // sll r0, r0, 0
  goto block_85;                                    // branch always

  
block_80:
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->mov64(v1, s1);                                 // or v1, s1, r0
  c->sw(r0, 48, at);                                // sw r0, 48(at)
  c->sw(v1, 44, at);                                // sw v1, 44(at)
  c->sh(r0, 56, at);                                // sh r0, 56(at)
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(t9, 7464, v1);                             // lwu t9, 7464(v1)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(t9, 7472, v1);                             // lwu t9, 7472(v1)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lw(v1, 24, at);                                // lw v1, 24(at)
  c->lw(a0, 40, at);                                // lw a0, 40(at)
  c->mov64(a3, v1);                                 // or a3, v1, r0
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->lui(a2, 4096);                                 // lui a2, 4096
  c->lwu(a1, 60, at);                               // lwu a1, 60(at)
  c->ori(a2, a2, 53248);                            // ori a2, a2, 53248
  c->lw(t1, 0, a2);                                 // lw t1, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->daddiu(t0, at, 92);                            // daddiu t0, at, 92
  c->andi(a3, a3, 16383);                           // andi a3, a3, 16383
  c->andi(t1, t1, 256);                             // andi t1, t1, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L63
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_84;}                          // branch non-likely

  c->mov64(t1, a2);                                 // or t1, a2, r0
  // nop                                            // sll r0, r0, 0
  
block_82:
  c->lw(t2, 0, t0);                                 // lw t2, 0(t0)
  // nop                                            // sll r0, r0, 0
  c->lw(t3, 0, t1);                                 // lw t3, 0(t1)
  // nop                                            // sll r0, r0, 0
  c->andi(t3, t3, 256);                             // andi t3, t3, 256
  c->daddiu(t2, t2, 1);                             // daddiu t2, t2, 1
  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L62
  c->sw(t2, 0, t0);                                 // sw t2, 0(t0)
  if (bc) {goto block_82;}                          // branch non-likely

  c->gprs[t0].du64[0] = 0;                          // or t0, r0, r0
  
block_84:
  c->dsll(t0, a0, 4);                               // dsll t0, a0, 4
  c->sw(a3, 128, a2);                               // sw a3, 128(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(a1, 16, a2);                                // sw a1, 16(a2)
  c->addiu(a3, r0, 256);                            // addiu a3, r0, 256
  c->sw(a0, 32, a2);                                // sw a0, 32(a2)
  c->daddu(a0, a1, t0);                             // daddu a0, a1, t0
  c->sw(a3, 0, a2);                                 // sw a3, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 60, at);                                // sw a0, 60(at)
  c->gprs[a0].du64[0] = 0;                          // or a0, r0, r0
  c->xori(v1, v1, 4608);                            // xori v1, v1, 4608
  c->sw(v1, 24, at);                                // sw v1, 24(at)
  
block_85:
  c->addiu(v1, r0, 1);                              // addiu v1, r0, 1
  c->dsubu(gp, v1, gp);                             // dsubu gp, v1, gp
  c->daddiu(s5, s5, 1);                             // daddiu s5, s5, 1
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lhu(v1, 6946, v1);                             // lhu v1, 6946(v1)
  bc = c->sgpr64(s5) != c->sgpr64(v1);              // bne s5, v1, L65
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_87;}                          // branch non-likely

  c->addiu(s5, r0, 0);                              // addiu s5, r0, 0
  c->mov64(v1, s5);                                 // or v1, s5, r0
  
block_87:
  c->mov64(v1, s2);                                 // or v1, s2, r0
  
block_88:
  bc = c->sgpr64(s2) != 0;                          // bne s2, r0, L21
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_4;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(v1, 7456, v1);                             // lwu v1, 7456(v1)
  c->sw(r0, 0, v1);                                 // sw r0, 0(v1)
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
  cache.fake_scratchpad_data = intern_from_c(-1, 0, "*fake-scratchpad-data*").c();
  cache.gsf_buffer = intern_from_c(-1, 0, "*gsf-buffer*").c();
  cache.generic_merc_death = intern_from_c(-1, 0, "generic-merc-death").c();
  cache.generic_merc_query = intern_from_c(-1, 0, "generic-merc-query").c();
  cache.generic_translucent = intern_from_c(-1, 0, "generic-translucent").c();
  cache.generic_warp_dest = intern_from_c(-1, 0, "generic-warp-dest").c();
  cache.generic_warp_envmap_dest = intern_from_c(-1, 0, "generic-warp-envmap-dest").c();
  cache.generic_warp_source = intern_from_c(-1, 0, "generic-warp-source").c();
  gLinkedFunctionTable.reg("generic-merc-execute-asm", execute, 512);
}

} // namespace generic_merc_execute_asm
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace generic_merc_death {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* merc_death_spawn; // merc-death-spawn
  void* vector_matrix; // vector-matrix*!
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -144);                          // daddiu sp, sp, -144
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s1, 48, sp);                                // sq s1, 48(sp)
  c->sq(s2, 64, sp);                                // sq s2, 64(sp)
  c->sq(s3, 80, sp);                                // sq s3, 80(sp)
  c->sq(s4, 96, sp);                                // sq s4, 96(sp)
  c->sq(s5, 112, sp);                               // sq s5, 112(sp)
  c->sq(gp, 128, sp);                               // sq gp, 128(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lhu(s5, 6846, v1);                             // lhu s5, 6846(v1)
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lhu(s3, 6844, v1);                             // lhu s3, 6844(v1)
  c->lhu(s4, 20, gp);                               // lhu s4, 20(gp)
  c->daddiu(s2, sp, 16);                            // daddiu s2, sp, 16
  c->sq(r0, 0, s2);                                 // sq r0, 0(s2)
  c->daddiu(s1, sp, 32);                            // daddiu s1, sp, 32
  c->sq(r0, 0, s1);                                 // sq r0, 0(s1)
  //beq r0, r0, L69                                 // beq r0, r0, L69
  // nop                                            // sll r0, r0, 0
  goto block_2;                                     // branch always

  
block_1:
  c->dsll(v1, s5, 5);                               // dsll v1, s5, 5
  c->daddu(v1, v1, gp);                             // daddu v1, v1, gp
  c->lwc1(f0, 448, v1);                             // lwc1 f0, 448(v1)
  c->swc1(f0, 0, s2);                               // swc1 f0, 0(s2)
  c->dsll(v1, s5, 5);                               // dsll v1, s5, 5
  c->daddu(v1, v1, gp);                             // daddu v1, v1, gp
  c->lwc1(f0, 452, v1);                             // lwc1 f0, 452(v1)
  c->swc1(f0, 4, s2);                               // swc1 f0, 4(s2)
  c->dsll(v1, s5, 5);                               // dsll v1, s5, 5
  c->daddu(v1, v1, gp);                             // daddu v1, v1, gp
  c->lwc1(f0, 456, v1);                             // lwc1 f0, 456(v1)
  c->swc1(f0, 8, s2);                               // swc1 f0, 8(s2)
  c->lui(v1, 16256);                                // lui v1, 16256
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->swc1(f0, 12, s2);                              // swc1 f0, 12(s2)
  c->dsll(v1, s5, 5);                               // dsll v1, s5, 5
  c->daddu(v1, v1, gp);                             // daddu v1, v1, gp
  c->lwc1(f0, 464, v1);                             // lwc1 f0, 464(v1)
  c->swc1(f0, 0, s1);                               // swc1 f0, 0(s1)
  c->dsll(v1, s5, 5);                               // dsll v1, s5, 5
  c->daddu(v1, v1, gp);                             // daddu v1, v1, gp
  c->lwc1(f0, 468, v1);                             // lwc1 f0, 468(v1)
  c->swc1(f0, 4, s1);                               // swc1 f0, 4(s1)
  c->dsll(v1, s5, 5);                               // dsll v1, s5, 5
  c->daddu(v1, v1, gp);                             // daddu v1, v1, gp
  c->lwc1(f0, 472, v1);                             // lwc1 f0, 472(v1)
  c->swc1(f0, 8, s1);                               // swc1 f0, 8(s1)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 12, s1);                              // swc1 f0, 12(s1)
  c->load_symbol2(t9, cache.vector_matrix);         // lw t9, vector-matrix*!(s7)
  c->mov64(a0, s2);                                 // or a0, s2, r0
  c->mov64(a1, s2);                                 // or a1, s2, r0
  c->addiu(v1, r0, 7264);                           // addiu v1, r0, 7264
  get_fake_spad_addr2(a2, cache.fake_scratchpad_data, 0, c);// lui a2, 28672
  c->daddu(a2, v1, a2);                             // daddu a2, v1, a2
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->load_symbol2(t9, cache.vector_matrix);         // lw t9, vector-matrix*!(s7)
  c->mov64(a0, s1);                                 // or a0, s1, r0
  c->mov64(a1, s1);                                 // or a1, s1, r0
  c->addiu(v1, r0, 7264);                           // addiu v1, r0, 7264
  get_fake_spad_addr2(a2, cache.fake_scratchpad_data, 0, c);// lui a2, 28672
  c->daddu(a2, v1, a2);                             // daddu a2, v1, a2
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->load_symbol2(t9, cache.merc_death_spawn);      // lw t9, merc-death-spawn(s7)
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(a0, 6848, v1);                             // lwu a0, 6848(v1)
  c->mov64(a1, s2);                                 // or a1, s2, r0
  c->mov64(a2, s1);                                 // or a2, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->daddu(s5, s5, s3);                             // daddu s5, s5, s3
  
block_2:
  c->sltu(v1, s5, s4);                              // sltu v1, s5, s4
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L68
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->dsubu(a0, s5, s4);                             // dsubu a0, s5, s4
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->sh(a0, 6846, v1);                              // sh a0, 6846(v1)
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 128, sp);                               // lq gp, 128(sp)
  c->lq(s5, 112, sp);                               // lq s5, 112(sp)
  c->lq(s4, 96, sp);                                // lq s4, 96(sp)
  c->lq(s3, 80, sp);                                // lq s3, 80(sp)
  c->lq(s2, 64, sp);                                // lq s2, 64(sp)
  c->lq(s1, 48, sp);                                // lq s1, 48(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 144);                           // daddiu sp, sp, 144
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c(-1, 0, "*fake-scratchpad-data*").c();
  cache.merc_death_spawn = intern_from_c(-1, 0, "merc-death-spawn").c();
  cache.vector_matrix = intern_from_c(-1, 0, "vector-matrix*!").c();
  gLinkedFunctionTable.reg("generic-merc-death", execute, 512);
}

} // namespace generic_merc_death
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace generic_merc_query {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* vector_matrix; // vector-matrix*!
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
  c->mov64(s4, a0);                                 // or s4, a0, r0
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(gp, 6836, v1);                             // lwu gp, 6836(v1)
  c->lhu(s3, 20, s4);                               // lhu s3, 20(s4)
  c->lw(s2, 24, gp);                                // lw s2, 24(gp)
  c->lw(s5, 20, gp);                                // lw s5, 20(gp)
  c->addiu(v1, r0, 7136);                           // addiu v1, r0, 7136
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  c->lwu(v1, 284, v1);                              // lwu v1, 284(v1)
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L71
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_2;}                           // branch non-likely

  c->addiu(s5, r0, 0);                              // addiu s5, r0, 0
  c->lw(s2, 12, gp);                                // lw s2, 12(gp)
  c->mov64(v1, s2);                                 // or v1, s2, r0
  
block_2:
  //beq r0, r0, L74                                 // beq r0, r0, L74
  // nop                                            // sll r0, r0, 0
  goto block_6;                                     // branch always

  
block_3:
  c->lw(v1, 0, gp);                                 // lw v1, 0(gp)
  c->slt(v1, s5, v1);                               // slt v1, s5, v1
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L73
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_5;}                           // branch non-likely

  c->dsll(v1, s5, 4);                               // dsll v1, s5, 4
  c->daddiu(v1, v1, 28);                            // daddiu v1, v1, 28
  c->daddu(a1, v1, gp);                             // daddu a1, v1, gp
  c->dsll(v1, s2, 5);                               // dsll v1, s2, 5
  c->daddu(v1, v1, s4);                             // daddu v1, v1, s4
  c->lwc1(f0, 448, v1);                             // lwc1 f0, 448(v1)
  c->swc1(f0, 0, a1);                               // swc1 f0, 0(a1)
  c->dsll(v1, s2, 5);                               // dsll v1, s2, 5
  c->daddu(v1, v1, s4);                             // daddu v1, v1, s4
  c->lwc1(f0, 452, v1);                             // lwc1 f0, 452(v1)
  c->swc1(f0, 4, a1);                               // swc1 f0, 4(a1)
  c->dsll(v1, s2, 5);                               // dsll v1, s2, 5
  c->daddu(v1, v1, s4);                             // daddu v1, v1, s4
  c->lwc1(f0, 456, v1);                             // lwc1 f0, 456(v1)
  c->swc1(f0, 8, a1);                               // swc1 f0, 8(a1)
  c->lui(v1, 16256);                                // lui v1, 16256
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->swc1(f0, 12, a1);                              // swc1 f0, 12(a1)
  c->load_symbol2(t9, cache.vector_matrix);         // lw t9, vector-matrix*!(s7)
  c->mov64(a0, a1);                                 // or a0, a1, r0
  c->addiu(v1, r0, 7264);                           // addiu v1, r0, 7264
  get_fake_spad_addr2(a2, cache.fake_scratchpad_data, 0, c);// lui a2, 28672
  c->daddu(a2, v1, a2);                             // daddu a2, v1, a2
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->daddiu(s5, s5, 1);                             // daddiu s5, s5, 1
  c->mov64(v1, s5);                                 // or v1, s5, r0
  
block_5:
  c->lw(v1, 16, gp);                                // lw v1, 16(gp)
  c->daddu(s2, s2, v1);                             // daddu s2, s2, v1
  
block_6:
  c->slt(v1, s2, s3);                               // slt v1, s2, s3
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L72
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_3;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->dsubu(v1, s2, s3);                             // dsubu v1, s2, s3
  c->sw(v1, 24, gp);                                // sw v1, 24(gp)
  c->sw(s5, 20, gp);                                // sw s5, 20(gp)
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
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c(-1, 0, "*fake-scratchpad-data*").c();
  cache.vector_matrix = intern_from_c(-1, 0, "vector-matrix*!").c();
  gLinkedFunctionTable.reg("generic-merc-query", execute, 256);
}

} // namespace generic_merc_query
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace generic_translucent {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->mov64(v1, a0);                                 // or v1, a0, r0
  c->daddiu(a0, at, 12064);                         // daddiu a0, at, 12064
  c->lbu(a1, 6842, at);                             // lbu a1, 6842(at)
  c->daddiu(a2, at, 6960);                          // daddiu a2, at, 6960
  c->mov64(a2, a2);                                 // or a2, a2, r0
  c->movn(a0, a2, a1);                              // movn a0, a2, a1
  c->lbu(a1, 6856, at);                             // lbu a1, 6856(at)
  c->addiu(a2, r0, 128);                            // addiu a2, r0, 128
  c->sltu(a2, a2, a1);                              // sltu a2, a2, a1
  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L76
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  c->addiu(a3, r0, 100);                            // addiu a3, r0, 100
  c->lw(a2, 24, at);                                // lw a2, 24(at)
  c->sw(a3, 112, a2);                               // sw a3, 112(a2)
  c->addiu(a3, r0, 66);                             // addiu a3, r0, 66
  c->sw(a3, 120, a2);                               // sw a3, 120(a2)
  c->sw(a1, 116, a2);                               // sw a1, 116(a2)
  //beq r0, r0, L77                                 // beq r0, r0, L77
  // nop                                            // sll r0, r0, 0
  goto block_3;                                     // branch always

  
block_2:
  c->gprs[a1].du64[0] = 0;                          // or a1, r0, r0
  
block_3:
  c->sw(r0, 48, at);                                // sw r0, 48(at)
  c->sw(a0, 52, at);                                // sw a0, 52(at)
  c->sw(v1, 44, at);                                // sw v1, 44(at)
  c->lwu(t9, 7468, at);                             // lwu t9, 7468(at)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lwu(t9, 7472, at);                             // lwu t9, 7472(at)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lwu(t9, 7476, at);                             // lwu t9, 7476(at)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lw(v1, 24, at);                                // lw v1, 24(at)
  c->lw(a0, 40, at);                                // lw a0, 40(at)
  c->mov64(a3, v1);                                 // or a3, v1, r0
  // nop                                            // sll r0, r0, 0
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->lui(a2, 4096);                                 // lui a2, 4096
  c->lwu(a1, 60, at);                               // lwu a1, 60(at)
  c->ori(a2, a2, 53248);                            // ori a2, a2, 53248
  c->lw(t1, 0, a2);                                 // lw t1, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->daddiu(t0, at, 92);                            // daddiu t0, at, 92
  c->andi(a3, a3, 16383);                           // andi a3, a3, 16383
  c->andi(t1, t1, 256);                             // andi t1, t1, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L79
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  c->mov64(t1, a2);                                 // or t1, a2, r0
  // nop                                            // sll r0, r0, 0
  
block_5:
  c->lw(t2, 0, t0);                                 // lw t2, 0(t0)
  // nop                                            // sll r0, r0, 0
  c->lw(t3, 0, t1);                                 // lw t3, 0(t1)
  // nop                                            // sll r0, r0, 0
  c->andi(t3, t3, 256);                             // andi t3, t3, 256
  c->daddiu(t2, t2, 1);                             // daddiu t2, t2, 1
  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L78
  c->sw(t2, 0, t0);                                 // sw t2, 0(t0)
  if (bc) {goto block_5;}                           // branch non-likely

  c->gprs[t0].du64[0] = 0;                          // or t0, r0, r0
  
block_7:
  c->dsll(t0, a0, 4);                               // dsll t0, a0, 4
  c->sw(a3, 128, a2);                               // sw a3, 128(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(a1, 16, a2);                                // sw a1, 16(a2)
  c->addiu(a3, r0, 256);                            // addiu a3, r0, 256
  c->sw(a0, 32, a2);                                // sw a0, 32(a2)
  c->daddu(a0, a1, t0);                             // daddu a0, a1, t0
  c->sw(a3, 0, a2);                                 // sw a3, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 60, at);                                // sw a0, 60(at)
  c->gprs[a0].du64[0] = 0;                          // or a0, r0, r0
  c->xori(v1, v1, 4608);                            // xori v1, v1, 4608
  c->sw(v1, 24, at);                                // sw v1, 24(at)
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c(-1, 0, "*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("generic-translucent", execute, 256);
}

} // namespace generic_translucent
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace high_speed_reject {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lw(a0, 7436, v1);                              // lw a0, 7436(v1)
  c->daddiu(a1, a0, 448);                           // daddiu a1, a0, 448
  c->lhu(a3, 20, a0);                               // lhu a3, 20(a0)
  c->vmax_bc(DEST::xyzw, BC::w, vf9, vf0, vf0);     // vmaxw.xyzw vf9, vf0, vf0
  c->lqc2(vf10, 7488, v1);                          // lqc2 vf10, 7488(v1)
  c->lqc2(vf11, 7504, v1);                          // lqc2 vf11, 7504(v1)
  c->lqc2(vf1, 0, a1);                              // lqc2 vf1, 0(a1)
  c->addiu(a0, r0, 240);                            // addiu a0, r0, 240
  c->lqc2(vf2, 32, a1);                             // lqc2 vf2, 32(a1)
//   TODO FIX - vi9 = c->gpr_src(a0).du16[0];                     // ctc2.i vi9, a0
  c->lqc2(vf3, 64, a1);                             // lqc2 vf3, 64(a1)
  c->lqc2(vf4, 96, a1);                             // lqc2 vf4, 96(a1)
  c->daddiu(a0, r0, -1);                            // daddiu a0, r0, -1
  // nop                                            // sll r0, r0, 0
  // Unknown instr: vcallms 438
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf5, 128, a1);                            // lqc2 vf5, 128(a1)
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
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf6, 160, a1);                            // lqc2 vf6, 160(a1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lwu(t0, 8, a1);                                // lwu t0, 8(a1)
  c->lqc2(vf7, 192, a1);                            // lqc2 vf7, 192(a1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
//   TODO - fix c->gprs[a2].du64[0] = vi1;                        // cfc2.ni a2, vi1
  c->lqc2(vf8, 224, a1);                            // lqc2 vf8, 224(a1)
  c->srl(t0, t0, 31);                               // srl t0, t0, 31
  c->or_(t0, a2, t0);                               // or t0, a2, t0
  c->lwu(a2, 40, a1);                               // lwu a2, 40(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L83
  c->and_(a0, a0, t0);                              // and a0, a0, t0
  if (bc) {goto block_20;}                          // branch non-likely

  c->daddiu(a1, a1, 128);                           // daddiu a1, a1, 128
  c->daddiu(a3, a3, -1);                            // daddiu a3, a3, -1
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L82
//   TODO - fix c->gprs[t0].du64[0] = vi2;                        // cfc2.ni t0, vi2
  if (bc) {goto block_18;}                          // branch non-likely

  
block_2:
  // Unknown instr: vcallms 454
  c->srl(a2, a2, 31);                               // srl a2, a2, 31
  c->or_(a2, t0, a2);                               // or a2, t0, a2
  c->lwu(t0, -56, a1);                              // lwu t0, -56(a1)
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L83
  c->and_(a0, a0, a2);                              // and a0, a0, a2
  if (bc) {goto block_20;}                          // branch non-likely

  c->lqc2(vf1, 128, a1);                            // lqc2 vf1, 128(a1)
  c->daddiu(a2, a3, -1);                            // daddiu a2, a3, -1
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L82
//   TODO FIX - c->gprs[a3].du64[0] = vi3;                        // cfc2.ni a3, vi3
  if (bc) {goto block_18;}                          // branch non-likely

  c->daddiu(a2, a2, -1);                            // daddiu a2, a2, -1
  c->srl(t0, t0, 31);                               // srl t0, t0, 31
  c->or_(t0, a3, t0);                               // or t0, a3, t0
  c->lwu(a3, -24, a1);                              // lwu a3, -24(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L83
  c->and_(a0, a0, t0);                              // and a0, a0, t0
  if (bc) {goto block_20;}                          // branch non-likely

  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L82
//   TODO - fix c->gprs[t0].du64[0] = vi4;                        // cfc2.ni t0, vi4
  if (bc) {goto block_18;}                          // branch non-likely

  c->lqc2(vf2, 160, a1);                            // lqc2 vf2, 160(a1)
  c->srl(a3, a3, 31);                               // srl a3, a3, 31
  c->or_(t0, t0, a3);                               // or t0, t0, a3
  c->lwu(a3, 8, a1);                                // lwu a3, 8(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L83
  c->and_(a0, a0, t0);                              // and a0, a0, t0
  if (bc) {goto block_20;}                          // branch non-likely

  c->lqc2(vf3, 192, a1);                            // lqc2 vf3, 192(a1)
  c->daddiu(a2, a2, -1);                            // daddiu a2, a2, -1
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L82
//   TODO - fix c->gprs[t0].du64[0] = vi5;                        // cfc2.ni t0, vi5
  if (bc) {goto block_18;}                          // branch non-likely

  c->lqc2(vf4, 224, a1);                            // lqc2 vf4, 224(a1)
  c->srl(a3, a3, 31);                               // srl a3, a3, 31
  c->or_(t0, t0, a3);                               // or t0, t0, a3
  c->lwu(a3, 40, a1);                               // lwu a3, 40(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L83
  c->and_(a0, a0, t0);                              // and a0, a0, t0
  if (bc) {goto block_20;}                          // branch non-likely

  c->daddiu(a1, a1, 128);                           // daddiu a1, a1, 128
  c->daddiu(a2, a2, -1);                            // daddiu a2, a2, -1
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L82
//   TODO FIX - c->gprs[t0].du64[0] = vi6;                        // cfc2.ni t0, vi6
  if (bc) {goto block_18;}                          // branch non-likely

  // Unknown instr: vcallms 438
  c->srl(a3, a3, 31);                               // srl a3, a3, 31
  c->or_(t0, t0, a3);                               // or t0, t0, a3
  c->lwu(a3, -56, a1);                              // lwu a3, -56(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L83
  c->and_(a0, a0, t0);                              // and a0, a0, t0
  if (bc) {goto block_20;}                          // branch non-likely

  c->lqc2(vf5, 128, a1);                            // lqc2 vf5, 128(a1)
  c->daddiu(a2, a2, -1);                            // daddiu a2, a2, -1
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L82
//   TODO - fix c->gprs[t0].du64[0] = vi7;                        // cfc2.ni t0, vi7
  if (bc) {goto block_18;}                          // branch non-likely

  c->daddiu(a2, a2, -1);                            // daddiu a2, a2, -1
  c->srl(a3, a3, 31);                               // srl a3, a3, 31
  c->or_(t0, t0, a3);                               // or t0, t0, a3
  c->lwu(a3, -24, a1);                              // lwu a3, -24(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L83
  c->and_(a0, a0, t0);                              // and a0, a0, t0
  if (bc) {goto block_20;}                          // branch non-likely

  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L82
//   TODO FIX - c->gprs[t0].du64[0] = vi8;                        // cfc2.ni t0, vi8
  if (bc) {goto block_18;}                          // branch non-likely

  c->lqc2(vf6, 160, a1);                            // lqc2 vf6, 160(a1)
  c->srl(a3, a3, 31);                               // srl a3, a3, 31
  c->or_(a3, t0, a3);                               // or a3, t0, a3
  c->lwu(t0, 8, a1);                                // lwu t0, 8(a1)
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L83
  c->and_(a0, a0, a3);                              // and a0, a0, a3
  if (bc) {goto block_20;}                          // branch non-likely

  c->lqc2(vf7, 192, a1);                            // lqc2 vf7, 192(a1)
  c->daddiu(a3, a2, -1);                            // daddiu a3, a2, -1
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L82
//   TODO FIX - c->gprs[a2].du64[0] = vi1;                        // cfc2.ni a2, vi1
  if (bc) {goto block_18;}                          // branch non-likely

  c->lqc2(vf8, 224, a1);                            // lqc2 vf8, 224(a1)
  c->srl(t0, t0, 31);                               // srl t0, t0, 31
  c->or_(t0, a2, t0);                               // or t0, a2, t0
  c->lwu(a2, 40, a1);                               // lwu a2, 40(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L83
  c->and_(a0, a0, t0);                              // and a0, a0, t0
  if (bc) {goto block_20;}                          // branch non-likely

  c->daddiu(a1, a1, 128);                           // daddiu a1, a1, 128
  c->daddiu(a3, a3, -1);                            // daddiu a3, a3, -1
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L81
//   TODO - fix c->gprs[t0].du64[0] = vi2;                        // cfc2.ni t0, vi2
  if (bc) {goto block_2;}                           // branch non-likely

  
block_18:
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L83
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_20;}                          // branch non-likely

  c->sb(r0, 6853, v1);                              // sb r0, 6853(v1)
  
block_20:
  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return

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
  cache.fake_scratchpad_data = intern_from_c(-1, 0, "*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("high-speed-reject", execute, 512);
}

} // namespace high_speed_reject
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace mercneric_convert {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->sq(s0, 7552, at);                              // sq s0, 7552(at)
  c->sq(s1, 7568, at);                              // sq s1, 7568(at)
  c->sq(s2, 7584, at);                              // sq s2, 7584(at)
  c->sq(s3, 7600, at);                              // sq s3, 7600(at)
  c->sq(s4, 7616, at);                              // sq s4, 7616(at)
  c->sq(s5, 7632, at);                              // sq s5, 7632(at)
  c->sq(s6, 7648, at);                              // sq s6, 7648(at)
  c->sq(t8, 7664, at);                              // sq t8, 7664(at)
  c->sq(t9, 7680, at);                              // sq t9, 7680(at)
  c->sq(gp, 7696, at);                              // sq gp, 7696(at)
  c->sq(sp, 7712, at);                              // sq sp, 7712(at)
  c->sq(fp, 7728, at);                              // sq fp, 7728(at)
  c->sq(ra, 7744, at);                              // sq ra, 7744(at)
  // nop                                            // sll r0, r0, 0
  get_fake_spad_addr2(t2, cache.fake_scratchpad_data, 0, c);// lui t2, 28672
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  c->lbu(a1, 6955, t2);                             // lbu a1, 6955(t2)
  c->addiu(a2, r0, 4);                              // addiu a2, r0, 4
  c->lw(t0, 7444, t2);                              // lw t0, 7444(t2)
  c->addiu(a3, r0, 1);                              // addiu a3, r0, 1
  c->lw(v1, 7448, t2);                              // lw v1, 7448(t2)
  c->and_(a3, a1, a3);                              // and a3, a1, a3
  c->lw(a1, 24, t2);                                // lw a1, 24(t2)
  c->movn(a0, a2, a3);                              // movn a0, a2, a3
  c->lbu(a3, 6855, t2);                             // lbu a3, 6855(t2)
  c->daddu(t1, a0, t2);                             // daddu t1, a0, t2
  c->lbu(t3, 6852, t2);                             // lbu t3, 6852(t2)
  c->daddiu(a2, a1, 128);                           // daddiu a2, a1, 128
  c->lw(a0, 7428, t2);                              // lw a0, 7428(t2)
  c->dsll(t4, a3, 3);                               // dsll t4, a3, 3
  // nop                                            // sll r0, r0, 0
  c->andi(t3, t3, 1);                               // andi t3, t3, 1
  c->lbu(a3, 10, t0);                               // lbu a3, 10(t0)
  c->dsll(t5, t3, 1);                               // dsll t5, t3, 1
  c->lbu(t3, 9, t0);                                // lbu t3, 9(t0)
  c->daddu(t1, t1, t5);                             // daddu t1, t1, t5
  // nop                                            // sll r0, r0, 0
  c->daddu(t4, t1, t4);                             // daddu t4, t1, t4
  c->lbu(t1, 11953, t4);                            // lbu t1, 11953(t4)
  // nop                                            // sll r0, r0, 0
  c->lbu(t5, 11952, t4);                            // lbu t5, 11952(t4)
  // nop                                            // sll r0, r0, 0
  c->lw(t4, 7432, t2);                              // lw t4, 7432(t2)
  c->sll(t6, a3, 4);                                // sll t6, a3, 4
  c->daddu(t5, t5, t2);                             // daddu t5, t5, t2
  c->daddu(t7, t1, t2);                             // daddu t7, t1, t2
  c->lw(a3, 7436, t2);                              // lw a3, 7436(t2)
  c->daddu(t1, t6, t0);                             // daddu t1, t6, t0
  c->sw(t1, 7452, t2);                              // sw t1, 7452(t2)
  // nop                                            // sll r0, r0, 0
  c->lbu(t0, 12, t1);                               // lbu t0, 12(t1)
  // nop                                            // sll r0, r0, 0
  c->lq(t7, 11888, t7);                             // lq t7, 11888(t7)
  // nop                                            // sll r0, r0, 0
  c->lq(s0, 11888, t5);                             // lq s0, 11888(t5)
  // nop                                            // sll r0, r0, 0
  c->sq(t7, 96, a1);                                // sq t7, 96(a1)
  c->daddiu(t5, v1, 5504);                          // daddiu t5, v1, 5504
  c->sq(s0, 112, a1);                               // sq s0, 112(a1)
  c->daddiu(v1, v1, 6048);                          // daddiu v1, v1, 6048
  c->lbu(ra, 6857, t2);                             // lbu ra, 6857(t2)
  c->movn(v1, t5, t3);                              // movn v1, t5, t3
  c->sw(r0, 124, a1);                               // sw r0, 124(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(t7, 11744, t2);                             // lq t7, 11744(t2)
  c->daddiu(t6, t1, 16);                            // daddiu t6, t1, 16
  c->lq(s0, 11808, t2);                             // lq s0, 11808(t2)
  c->addiu(t9, r0, 0);                              // addiu t9, r0, 0
  c->lq(s4, 11824, t2);                             // lq s4, 11824(t2)
  c->addiu(t5, r0, 0);                              // addiu t5, r0, 0
  c->lq(gp, 11840, t2);                             // lq gp, 11840(t2)
  c->addiu(t1, r0, -3);                             // addiu t1, r0, -3
  c->lq(s5, 11856, t2);                             // lq s5, 11856(t2)
  c->addiu(t8, r0, 6);                              // addiu t8, r0, 6
  c->lq(t2, 11872, t2);                             // lq t2, 11872(t2)
  c->andi(s3, ra, 4);                               // andi s3, ra, 4
  c->sq(t7, 0, a1);                                 // sq t7, 0(a1)
  c->movz(t8, r0, t3);                              // movz t8, r0, t3
  c->sq(s0, 16, a1);                                // sq s0, 16(a1)
  c->addiu(ra, r0, 0);                              // addiu ra, r0, 0
  c->sq(s4, 32, a1);                                // sq s4, 32(a1)
  c->addiu(t7, r0, -32768);                         // addiu t7, r0, -32768
  c->sq(gp, 48, a1);                                // sq gp, 48(a1)
  c->movz(ra, t7, t0);                              // movz ra, t7, t0
  c->sq(s5, 64, a1);                                // sq s5, 64(a1)
  c->dsll(t7, s3, 18);                              // dsll t7, s3, 18
  c->pextlw(t7, t7, t7);                            // pextlw t7, t7, t7
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // Unknown instr: pnor t7, r0, t7
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pand(t2, t2, t7);                              // pand t2, t2, t7
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L85
  c->sq(t2, 80, a1);                                // sq t2, 80(a1)
  if (bc) {goto block_4;}                           // branch non-likely

  c->lq(t7, 2944, t4);                              // lq t7, 2944(t4)
  c->daddu(ra, t3, ra);                             // daddu ra, t3, ra
  c->lq(s0, 2960, t4);                              // lq s0, 2960(t4)
  c->addiu(gp, r0, -6);                             // addiu gp, r0, -6
  c->lq(t3, 2976, t4);                              // lq t3, 2976(t4)
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  c->lq(t2, 2992, t4);                              // lq t2, 2992(t4)
  c->daddiu(s5, t0, -3);                            // daddiu s5, t0, -3
  bc = ((s64)c->sgpr64(s5)) <= 0;                   // blez s5, L87
  c->lq(t4, 3008, t4);                              // lq t4, 3008(t4)
  if (bc) {goto block_8;}                           // branch non-likely

  c->daddiu(t9, t0, -5);                            // daddiu t9, t0, -5
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(t9)) <= 0;                   // blez t9, L87
  c->lh(t9, 172, t6);                               // lh t9, 172(t6)
  if (bc) {goto block_8;}                           // branch non-likely

  //beq r0, r0, L87                                 // beq r0, r0, L87
  c->lh(t5, 332, t6);                               // lh t5, 332(t6)
  goto block_8;                                     // branch always

  
block_4:
  c->daddiu(t2, t0, -3);                            // daddiu t2, t0, -3
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(t2)) <= 0;                   // blez t2, L86
  c->daddiu(t2, t0, -5);                            // daddiu t2, t0, -5
  if (bc) {goto block_7;}                           // branch non-likely

  bc = ((s64)c->sgpr64(t2)) <= 0;                   // blez t2, L86
  c->lh(t9, 252, t6);                               // lh t9, 252(t6)
  if (bc) {goto block_7;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lh(t5, 412, t6);                               // lh t5, 412(t6)
  
block_7:
  c->lh(gp, 12, t6);                                // lh gp, 12(t6)
  // nop                                            // sll r0, r0, 0
  c->lh(ra, 28, t6);                                // lh ra, 28(t6)
  // nop                                            // sll r0, r0, 0
  c->lq(t7, 0, t6);                                 // lq t7, 0(t6)
  // nop                                            // sll r0, r0, 0
  c->lq(s0, 16, t6);                                // lq s0, 16(t6)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 32, t6);                                // lq t3, 32(t6)
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 48, t6);                                // lq t2, 48(t6)
  // nop                                            // sll r0, r0, 0
  c->lq(t4, 64, t6);                                // lq t4, 64(t6)
  c->daddiu(t6, t6, 80);                            // daddiu t6, t6, 80
  
block_8:
  c->sq(t7, 0, a2);                                 // sq t7, 0(a2)
  c->daddu(gp, gp, t8);                             // daddu gp, gp, t8
  c->sq(s0, 16, a2);                                // sq s0, 16(a2)
  c->daddiu(t8, t8, 2);                             // daddiu t8, t8, 2
  c->sq(t3, 32, a2);                                // sq t3, 32(a2)
  c->daddu(t1, t1, ra);                             // daddu t1, t1, ra
  c->sq(t2, 48, a2);                                // sq t2, 48(a2)
  c->daddiu(t1, t1, 3);                             // daddiu t1, t1, 3
  c->sq(t4, 64, a2);                                // sq t4, 64(a2)
  c->andi(t1, t1, 255);                             // andi t1, t1, 255
  c->sw(gp, 12, a2);                                // sw gp, 12(a2)
  c->daddiu(a2, a2, 80);                            // daddiu a2, a2, 80
  bc = ((s64)c->sgpr64(ra)) > 0;                    // bgtz ra, L86
  c->sw(ra, -52, a2);                               // sw ra, -52(a2)
  if (bc) {goto block_7;}                           // branch non-likely

  c->sw(t1, 108, a1);                               // sw t1, 108(a1)
  c->daddiu(a1, t9, 7);                             // daddiu a1, t9, 7
  c->sq(t7, 2944, a0);                              // sq t7, 2944(a0)
  c->daddiu(t5, t5, 7);                             // daddiu t5, t5, 7
  c->sq(s0, 2960, a0);                              // sq s0, 2960(a0)
  c->sra(a2, a1, 4);                                // sra a2, a1, 4
  c->sq(t3, 2976, a0);                              // sq t3, 2976(a0)
  c->sra(t3, t5, 4);                                // sra t3, t5, 4
  c->sq(t2, 2992, a0);                              // sq t2, 2992(a0)
  c->daddiu(a1, a3, 192);                           // daddiu a1, a3, 192
  c->sq(t4, 3008, a0);                              // sq t4, 3008(a0)
  c->dsll(a0, t0, 2);                               // dsll a0, t0, 2
  c->sh(t1, 18, a3);                                // sh t1, 18(a3)
  c->daddu(a0, a0, t0);                             // daddu a0, a0, t0
  c->sb(t0, 16, a3);                                // sb t0, 16(a3)
  c->daddiu(a0, a0, 7);                             // daddiu a0, a0, 7
  c->dsubu(a0, t3, a2);                             // dsubu a0, t3, a2
  c->lq(t0, 0, v1);                                 // lq t0, 0(v1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(t1, a2, -3);                            // daddiu t1, a2, -3
  c->lq(a3, 16, v1);                                // lq a3, 16(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t0, 0, a1);                                 // sq t0, 0(a1)
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L88
  c->lq(a2, 32, v1);                                // lq a2, 32(v1)
  if (bc) {goto block_21;}                          // branch non-likely

  c->daddiu(t0, t1, -1);                            // daddiu t0, t1, -1
  c->sq(a3, 16, a1);                                // sq a3, 16(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L89
  c->lq(a3, 48, v1);                                // lq a3, 48(v1)
  if (bc) {goto block_22;}                          // branch non-likely

  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  c->sq(a2, 32, a1);                                // sq a2, 32(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L90
  c->lq(a2, 64, v1);                                // lq a2, 64(v1)
  if (bc) {goto block_23;}                          // branch non-likely

  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  c->sq(a3, 48, a1);                                // sq a3, 48(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L91
  c->lq(a3, 80, v1);                                // lq a3, 80(v1)
  if (bc) {goto block_24;}                          // branch non-likely

  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  c->sq(a2, 64, a1);                                // sq a2, 64(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L92
  c->lq(a2, 96, v1);                                // lq a2, 96(v1)
  if (bc) {goto block_25;}                          // branch non-likely

  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  c->sq(a3, 80, a1);                                // sq a3, 80(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L93
  c->lq(a3, 112, v1);                               // lq a3, 112(v1)
  if (bc) {goto block_26;}                          // branch non-likely

  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  c->sq(a2, 96, a1);                                // sq a2, 96(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L94
  c->lq(a2, 128, v1);                               // lq a2, 128(v1)
  if (bc) {goto block_27;}                          // branch non-likely

  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  c->sq(a3, 112, a1);                               // sq a3, 112(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L95
  c->lq(a3, 144, v1);                               // lq a3, 144(v1)
  if (bc) {goto block_28;}                          // branch non-likely

  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  c->sq(a2, 128, a1);                               // sq a2, 128(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L96
  c->lq(a2, 160, v1);                               // lq a2, 160(v1)
  if (bc) {goto block_29;}                          // branch non-likely

  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  c->sq(a3, 144, a1);                               // sq a3, 144(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L97
  c->lq(a3, 176, v1);                               // lq a3, 176(v1)
  if (bc) {goto block_30;}                          // branch non-likely

  c->daddiu(a0, t0, -1);                            // daddiu a0, t0, -1
  c->sq(a2, 160, a1);                               // sq a2, 160(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L98
  c->lq(a0, 192, v1);                               // lq a0, 192(v1)
  if (bc) {goto block_31;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->sq(a3, 176, a1);                               // sq a3, 176(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(v1, 208, v1);                               // lq v1, 208(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(a0, 192, a1);                               // sq a0, 192(a1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sq(v1, 208, a1);                               // sq v1, 208(a1)
  //beq r0, r0, L108                                // beq r0, r0, L108
  // nop                                            // sll r0, r0, 0
  goto block_41;                                    // branch always

  
block_21:
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, 16, a1);                                // sq a3, 16(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(a3, 224, v1);                               // lq a3, 224(v1)
  
block_22:
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a2, 32, a1);                                // sq a2, 32(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L99
  c->lq(a2, 240, v1);                               // lq a2, 240(v1)
  if (bc) {goto block_32;}                          // branch non-likely

  
block_23:
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, 48, a1);                                // sq a3, 48(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L100
  c->lq(a3, 256, v1);                               // lq a3, 256(v1)
  if (bc) {goto block_33;}                          // branch non-likely

  
block_24:
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a2, 64, a1);                                // sq a2, 64(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L101
  c->lq(a2, 272, v1);                               // lq a2, 272(v1)
  if (bc) {goto block_34;}                          // branch non-likely

  
block_25:
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, 80, a1);                                // sq a3, 80(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L102
  c->lq(a3, 288, v1);                               // lq a3, 288(v1)
  if (bc) {goto block_35;}                          // branch non-likely

  
block_26:
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a2, 96, a1);                                // sq a2, 96(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L103
  c->lq(a2, 304, v1);                               // lq a2, 304(v1)
  if (bc) {goto block_36;}                          // branch non-likely

  
block_27:
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, 112, a1);                               // sq a3, 112(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L104
  c->lq(a3, 320, v1);                               // lq a3, 320(v1)
  if (bc) {goto block_37;}                          // branch non-likely

  
block_28:
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a2, 128, a1);                               // sq a2, 128(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L105
  c->lq(a2, 336, v1);                               // lq a2, 336(v1)
  if (bc) {goto block_38;}                          // branch non-likely

  
block_29:
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, 144, a1);                               // sq a3, 144(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L106
  c->lq(a3, 352, v1);                               // lq a3, 352(v1)
  if (bc) {goto block_39;}                          // branch non-likely

  
block_30:
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a2, 160, a1);                               // sq a2, 160(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L107
  c->lq(a0, 368, v1);                               // lq a0, 368(v1)
  if (bc) {goto block_40;}                          // branch non-likely

  
block_31:
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 176, a1);                               // sq a3, 176(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(v1, 384, v1);                               // lq v1, 384(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(a0, 192, a1);                               // sq a0, 192(a1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sq(v1, 208, a1);                               // sq v1, 208(a1)
  //beq r0, r0, L108                                // beq r0, r0, L108
  // nop                                            // sll r0, r0, 0
  goto block_41;                                    // branch always

  
block_32:
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 48, a1);                                // sq a3, 48(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(a3, 400, v1);                               // lq a3, 400(v1)
  
block_33:
  // nop                                            // sll r0, r0, 0
  c->sq(a2, 64, a1);                                // sq a2, 64(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(a2, 416, v1);                               // lq a2, 416(v1)
  
block_34:
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 80, a1);                                // sq a3, 80(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(a3, 432, v1);                               // lq a3, 432(v1)
  
block_35:
  // nop                                            // sll r0, r0, 0
  c->sq(a2, 96, a1);                                // sq a2, 96(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(a2, 448, v1);                               // lq a2, 448(v1)
  
block_36:
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 112, a1);                               // sq a3, 112(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(a3, 464, v1);                               // lq a3, 464(v1)
  
block_37:
  // nop                                            // sll r0, r0, 0
  c->sq(a2, 128, a1);                               // sq a2, 128(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(a2, 480, v1);                               // lq a2, 480(v1)
  
block_38:
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 144, a1);                               // sq a3, 144(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(a3, 496, v1);                               // lq a3, 496(v1)
  
block_39:
  // nop                                            // sll r0, r0, 0
  c->sq(a2, 160, a1);                               // sq a2, 160(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(a0, 512, v1);                               // lq a0, 512(v1)
  
block_40:
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 176, a1);                               // sq a3, 176(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(v1, 528, v1);                               // lq v1, 528(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(a0, 192, a1);                               // sq a0, 192(a1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sq(v1, 208, a1);                               // sq v1, 208(a1)
  
block_41:
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  // nop                                            // sll r0, r0, 0
  c->lw(a1, 7452, v1);                              // lw a1, 7452(v1)
  // nop                                            // sll r0, r0, 0
  c->lw(a0, 7436, v1);                              // lw a0, 7436(v1)
  // nop                                            // sll r0, r0, 0
  c->lbu(a2, 13, a1);                               // lbu a2, 13(a1)
  // nop                                            // sll r0, r0, 0
  c->lbu(v1, 14, a1);                               // lbu v1, 14(a1)
  c->daddiu(a0, a0, 32);                            // daddiu a0, a0, 32
  c->lbu(a3, 15, a1);                               // lbu a3, 15(a1)
  c->daddu(a1, a1, a2);                             // daddu a1, a1, a2
  c->mult3(a3, v1, a3);                             // mult3 a3, v1, a3
  c->mov64(a1, a1);                                 // or a1, a1, r0
  c->mov64(v1, v1);                                 // or v1, v1, r0
  c->mov64(a0, a0);                                 // or a0, a0, r0
  c->addiu(a2, r0, 513);                            // addiu a2, r0, 513
  c->addiu(t0, r0, 257);                            // addiu t0, r0, 257
  c->dsll(t2, a2, 18);                              // dsll t2, a2, 18
  c->dsll(t1, t0, 16);                              // dsll t1, t0, 16
  c->or_(a2, a2, t2);                               // or a2, a2, t2
  c->or_(t0, t0, t1);                               // or t0, t0, t1
  c->dsll32(t2, a2, 4);                             // dsll32 t2, a2, 4
  c->dsll32(t1, t0, 0);                             // dsll32 t1, t0, 0
  c->or_(a2, a2, t2);                               // or a2, a2, t2
  c->or_(t0, t0, t1);                               // or t0, t0, t1
  c->pcpyld(a2, a2, a2);                            // pcpyld a2, a2, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyld(t0, t0, t0);                            // pcpyld t0, t0, t0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->daddu(a3, a3, a1);                             // daddu a3, a3, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->lhu(t1, 0, a1);                                // lhu t1, 0(a1)
  c->daddu(a1, a1, v1);                             // daddu a1, a1, v1
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->pextlb(t1, t1, t1);                            // pextlb t1, t1, t1
  //beq r0, r0, L110                                // beq r0, r0, L110
  c->pextlb(t2, t1, t1);                            // pextlb t2, t1, t1
  goto block_43;                                    // branch always

  
block_42:
  c->daddu(a1, a1, v1);                             // daddu a1, a1, v1
  c->daddiu(a0, a0, 32);                            // daddiu a0, a0, 32
  c->pextlb(t2, t2, t2);                            // pextlb t2, t2, t2
  c->sq(t1, -16, a0);                               // sq t1, -16(a0)
  
block_43:
  c->pextlb(t1, t2, t2);                            // pextlb t1, t2, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pand(t1, t1, a2);                              // pand t1, t1, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pceqb(t1, t1, a2);                             // pceqb t1, t1, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pand(t1, t1, t0);                              // pand t1, t1, t0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(t2, t1, r0);                            // pextlb t2, t1, r0
  c->lhu(t3, 0, a1);                                // lhu t3, 0(a1)
  c->pextub(t1, t1, r0);                            // pextub t1, t1, r0
  c->sq(t2, 0, a0);                                 // sq t2, 0(a0)
  bc = c->sgpr64(a1) != c->sgpr64(a3);              // bne a1, a3, L109
  c->pextlb(t2, t3, t3);                            // pextlb t2, t3, t3
  if (bc) {goto block_42;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->sq(t1, 16, a0);                                // sq t1, 16(a0)
  get_fake_spad_addr2(s4, cache.fake_scratchpad_data, 0, c);// lui s4, 28672
  // nop                                            // sll r0, r0, 0
  c->lw(a0, 7444, s4);                              // lw a0, 7444(s4)
  // nop                                            // sll r0, r0, 0
  c->lw(a1, 7436, s4);                              // lw a1, 7436(s4)
  // nop                                            // sll r0, r0, 0
  c->lq(t8, 16, a0);                                // lq t8, 16(a0)
  c->daddiu(s6, a0, 48);                            // daddiu s6, a0, 48
  c->lbu(a3, 12, a0);                               // lbu a3, 12(a0)
  // nop                                            // sll r0, r0, 0
  c->lbu(t2, 10, a0);                               // lbu t2, 10(a0)
  c->pextlb(t9, r0, t8);                            // pextlb t9, r0, t8
//   TODO - fix vi10 = c->gpr_src(a3).du16[0];                    // ctc2.ni vi10, a3
  c->pextlh(gp, r0, t9);                            // pextlh gp, r0, t9
  c->mov128_vf_gpr(vf1, gp);                        // qmtc2.ni vf1, gp
  c->pextuh(t9, r0, t9);                            // pextuh t9, r0, t9
  c->mov128_vf_gpr(vf2, t9);                        // qmtc2.ni vf2, t9
  c->pextub(t9, r0, t8);                            // pextub t9, r0, t8
  c->lbu(a3, 4, a0);                                // lbu a3, 4(a0)
  c->pextlh(gp, r0, t9);                            // pextlh gp, r0, t9
  c->mov128_vf_gpr(vf3, gp);                        // qmtc2.ni vf3, gp
  c->pextuh(t9, r0, t9);                            // pextuh t9, r0, t9
  c->mov128_vf_gpr(vf4, t9);                        // qmtc2.ni vf4, t9
  c->sll(t2, t2, 4);                                // sll t2, t2, 4
  c->lbu(t0, 5, a0);                                // lbu t0, 5(a0)
  c->daddu(t2, t2, a0);                             // daddu t2, t2, a0
  c->lbu(t1, 6, a0);                                // lbu t1, 6(a0)
  c->daddiu(s5, a3, -1);                            // daddiu s5, a3, -1
  //   TODO - fix vi11 = c->gpr_src(a3).du16[0];                    // ctc2.ni vi11, a3
  c->daddu(s5, s5, t0);                             // daddu s5, s5, t0
  //   TODO - fix vi12 = c->gpr_src(t0).du16[0];                    // ctc2.ni vi12, t0
  c->daddu(s5, s5, t1);                             // daddu s5, s5, t1
//   TODO - fix vi13 = c->gpr_src(t1).du16[0];                    // ctc2.ni vi13, t1
  c->daddiu(t4, a1, 32);                            // daddiu t4, a1, 32
  c->lqc2(vf27, 0, t2);                             // lqc2 vf27, 0(t2)
  c->daddiu(t3, a1, 192);                           // daddiu t3, a1, 192
  // Unknown instr: vcallms 280
  c->daddiu(v1, a1, 448);                           // daddiu v1, a1, 448
  c->lhu(s1, 6820, s4);                             // lhu s1, 6820(s4)
  c->pextlw(t3, t3, t3);                            // pextlw t3, t3, t3
  c->lhu(s2, 6822, s4);                             // lhu s2, 6822(s4)
  c->pcpyld(t3, t3, t3);                            // pcpyld t3, t3, t3
  c->lqc2(vf19, 6800, s4);                          // lqc2 vf19, 6800(s4)
  c->pcpyh(s1, s1);                                 // pcpyh s1, s1
  c->lbu(v0, 2, a0);                                // lbu v0, 2(a0)
  c->addiu(t5, r0, 0);                              // addiu t5, r0, 0
  c->lbu(at, 1, a0);                                // lbu at, 1(a0)
  c->addiu(s4, r0, -1);                             // addiu s4, r0, -1
  c->lui(a1, 19201);                                // lui a1, 19201
  c->daddiu(v0, v0, 3);                             // daddiu v0, v0, 3
  c->daddiu(a1, a1, 18304);                         // daddiu a1, a1, 18304
  c->andi(v0, v0, 252);                             // andi v0, v0, 252
  c->sll(at, at, 2);                                // sll at, at, 2
  c->sll(v0, v0, 2);                                // sll v0, v0, 2
  c->lq(t8, 32, a0);                                // lq t8, 32(a0)
  c->daddu(v0, v0, a0);                             // daddu v0, v0, a0
  c->lbu(t0, 13, a0);                               // lbu t0, 13(a0)
  c->daddu(at, at, a0);                             // daddu at, at, a0
  c->lbu(t2, 11, a0);                               // lbu t2, 11(a0)
  c->pextlw(a1, a1, r0);                            // pextlw a1, a1, r0
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L113
  c->pcpyld(a1, a1, a1);                            // pcpyld a1, a1, a1
  if (bc) {goto block_49;}                          // branch non-likely

  c->sll(t2, t2, 4);                                // sll t2, t2, 4
  c->daddiu(a3, a0, 14);                            // daddiu a3, a0, 14
  //beq r0, r0, L112                                // beq r0, r0, L112
  c->daddu(t2, t2, a0);                             // daddu t2, t2, a0
  goto block_47;                                    // branch always

  
block_46:
  // Unknown instr: vcallms 303
  c->mov64(t0, t1);                                 // or t0, t1, r0
  
block_47:
  c->lbu(t1, 0, a3);                                // lbu t1, 0(a3)
  c->daddiu(a3, a3, 1);                             // daddiu a3, a3, 1
  c->lqc2(vf23, 64, t2);                            // lqc2 vf23, 64(t2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf24, 80, t2);                            // lqc2 vf24, 80(t2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf25, 96, t2);                            // lqc2 vf25, 96(t2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf26, 112, t2);                           // lqc2 vf26, 112(t2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf20, 16, t2);                            // lqc2 vf20, 16(t2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf21, 32, t2);                            // lqc2 vf21, 32(t2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf22, 48, t2);                            // lqc2 vf22, 48(t2)
  c->daddiu(t2, t2, 128);                           // daddiu t2, t2, 128
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L111
  //   TODO - fix vi14 = c->gpr_src(t0).du16[0];                    // ctc2.ni vi14, t0
  if (bc) {goto block_46;}                          // branch non-likely

  // Unknown instr: vcallms 303
  // nop                                            // sll r0, r0, 0
  
block_49:
  c->lq(a2, 0, v0);                                 // lq a2, 0(v0)
  c->pextlb(t9, r0, t8);                            // pextlb t9, r0, t8
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(gp, r0, t9);                            // pextlh gp, r0, t9
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(a3, r0, a2);                            // pextlb a3, r0, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextub(a2, r0, a2);                            // pextub a2, r0, a2
  c->mov128_vf_gpr(vf4, gp);                        // qmtc2.ni vf4, gp
  c->pextlh(t0, a1, a3);                            // pextlh t0, a1, a3
  c->mov128_vf_gpr(vf1, t0);                        // qmtc2.ni vf1, t0
  c->pextuh(a3, a1, a3);                            // pextuh a3, a1, a3
  c->mov128_vf_gpr(vf2, a3);                        // qmtc2.ni vf2, a3
  c->pextlh(t1, a1, a2);                            // pextlh t1, a1, a2
  c->mov128_vf_gpr(vf3, t1);                        // qmtc2.ni vf3, t1
  c->pextuh(t9, r0, t9);                            // pextuh t9, r0, t9
  // Unknown instr: vcallms 311
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
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 16, v0);                                // lq t2, 16(v0)
  // nop                                            // sll r0, r0, 0
  c->daddiu(at, at, -16);                           // daddiu at, at, -16
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextuh(a2, a1, a2);                            // pextuh a2, a1, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(t0, r0, t2);                            // pextlb t0, r0, t2
  c->mov128_vf_gpr(vf4, t9);                        // qmtc2.ni vf4, t9
  c->pextlh(t1, a1, t0);                            // pextlh t1, a1, t0
  c->mov128_vf_gpr(vf1, a2);                        // qmtc2.ni vf1, a2
  c->pextuh(t0, a1, t0);                            // pextuh t0, a1, t0
  c->mov128_vf_gpr(vf2, t1);                        // qmtc2.ni vf2, t1
  c->pextub(t9, r0, t8);                            // pextub t9, r0, t8
  c->mov128_vf_gpr(vf3, t0);                        // qmtc2.ni vf3, t0
  c->pextlh(gp, r0, t9);                            // pextlh gp, r0, t9
  // nop                                            // sll r0, r0, 0
  c->daddiu(v0, v0, -48);                           // daddiu v0, v0, -48
  // Unknown instr: vcallms 311
  // nop                                            // sll r0, r0, 0
  c->addiu(t6, r0, 0);                              // addiu t6, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lwl(t6, 53, v0);                               // lwl t6, 53(v0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(t6, r0, t6);                            // pextlb t6, r0, t6
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(t6, r0, t6);                            // pextlb t6, r0, t6
  c->lq(a3, 80, v0);                                // lq a3, 80(v0)
  c->paddw(t6, t6, t3);                             // paddw t6, t6, t3
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextub(t2, r0, t2);                            // pextub t2, r0, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(t0, a1, t2);                            // pextlh t0, a1, t2
  c->mov128_vf_gpr(vf4, gp);                        // qmtc2.ni vf4, gp
  c->pextuh(t2, a1, t2);                            // pextuh t2, a1, t2
  c->mov128_vf_gpr(vf1, t0);                        // qmtc2.ni vf1, t0
  c->pextlb(t1, r0, a3);                            // pextlb t1, r0, a3
  c->mov128_vf_gpr(vf2, t2);                        // qmtc2.ni vf2, t2
  c->pextlh(a2, a1, t1);                            // pextlh a2, a1, t1
  c->mov128_vf_gpr(vf3, a2);                        // qmtc2.ni vf3, a2
  c->pextuh(t9, r0, t9);                            // pextuh t9, r0, t9
  // nop                                            // sll r0, r0, 0
  c->daddiu(v1, v1, -64);                           // daddiu v1, v1, -64
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf15, 64, v1);                            // sqc2 vf15, 64(v1)
  // Unknown instr: vcallms 311
  // nop                                            // sll r0, r0, 0
  c->lwr(s3, 56, v0);                               // lwr s3, 56(v0)
  // nop                                            // sll r0, r0, 0
  c->lwl(s3, 69, v0);                               // lwl s3, 69(v0)
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyud(t6, t6, r0);                            // pcpyud t6, t6, r0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(s3, r0, s3);                            // pextlb s3, r0, s3
  // nop                                            // sll r0, r0, 0
  c->dsllv(s3, s3, s2);                             // dsllv s3, s3, s2
  c->lq(t8, 0, s6);                                 // lq t8, 0(s6)
  c->paddh(s3, s3, s1);                             // paddh s3, s3, s1
  c->lbu(t7, 0, t6);                                // lbu t7, 0(t6)
  c->dsrl32(t6, t6, 0);                             // dsrl32 t6, t6, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextuh(t0, a1, t1);                            // pextuh t0, a1, t1
  c->lbu(s0, 0, t6);                                // lbu s0, 0(t6)
  c->pextub(a3, r0, a3);                            // pextub a3, r0, a3
  c->mov128_vf_gpr(vf4, t9);                        // qmtc2.ni vf4, t9
  c->pextlh(t2, a1, a3);                            // pextlh t2, a1, a3
  c->mov128_vf_gpr(vf1, t0);                        // qmtc2.ni vf1, t0
  c->pextuh(a3, a1, a3);                            // pextuh a3, a1, a3
  c->mov128_vf_gpr(vf2, t2);                        // qmtc2.ni vf2, t2
  c->pextlb(t9, r0, t8);                            // pextlb t9, r0, t8
  c->mov128_vf_gpr(vf3, a3);                        // qmtc2.ni vf3, a3
  c->pextlh(gp, r0, t9);                            // pextlh gp, r0, t9
  // nop                                            // sll r0, r0, 0
  c->daddiu(s6, s6, 16);                            // daddiu s6, s6, 16
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf15, 96, v1);                            // sqc2 vf15, 96(v1)
  // Unknown instr: vcallms 311
  c->sw(s3, 76, v1);                                // sw s3, 76(v1)
  c->lwr(t6, 64, v0);                               // lwr t6, 64(v0)
  c->daddu(t7, t7, t4);                             // daddu t7, t7, t4
  c->lwl(t6, 77, v0);                               // lwl t6, 77(v0)
  c->daddu(s0, s0, t4);                             // daddu s0, s0, t4
  //beq r0, r0, L115                                // beq r0, r0, L115
  c->dsrl32(s3, s3, 0);                             // dsrl32 s3, s3, 0
  goto block_51;                                    // branch always

  
block_50:
  // Unknown instr: vcallms 311
  c->sw(s3, 76, v1);                                // sw s3, 76(v1)
  c->lwr(t6, 64, v0);                               // lwr t6, 64(v0)
  c->daddu(t7, t7, t4);                             // daddu t7, t7, t4
  c->lwl(t6, 77, v0);                               // lwl t6, 77(v0)
  c->daddu(s0, s0, t4);                             // daddu s0, s0, t4
  c->sw(a0, 60, v1);                                // sw a0, 60(v1)
  c->dsrl32(s3, s3, 0);                             // dsrl32 s3, s3, 0
  
block_51:
  c->sb(t5, 0, t7);                                 // sb t5, 0(t7)
  c->pextlb(t6, r0, t6);                            // pextlb t6, r0, t6
  c->sb(t5, 0, s0);                                 // sb t5, 0(s0)
  c->pextlb(t6, r0, t6);                            // pextlb t6, r0, t6
  c->lq(a2, 96, v0);                                // lq a2, 96(v0)
  c->paddw(t6, t6, t3);                             // paddw t6, t6, t3
  c->lbu(t7, 0, t6);                                // lbu t7, 0(t6)
  c->dsrl32(t6, t6, 0);                             // dsrl32 t6, t6, 0
  c->lw(a0, 16, at);                                // lw a0, 16(at)
  c->pextlb(a3, r0, a2);                            // pextlb a3, r0, a2
  c->lbu(s0, 0, t6);                                // lbu s0, 0(t6)
  c->pextub(a2, r0, a2);                            // pextub a2, r0, a2
  c->mov128_vf_gpr(vf4, gp);                        // qmtc2.ni vf4, gp
  c->pextlh(t0, a1, a3);                            // pextlh t0, a1, a3
  c->mov128_vf_gpr(vf1, t0);                        // qmtc2.ni vf1, t0
  c->pextuh(a3, a1, a3);                            // pextuh a3, a1, a3
  c->mov128_vf_gpr(vf2, a3);                        // qmtc2.ni vf2, a3
  c->pextlh(t1, a1, a2);                            // pextlh t1, a1, a2
  c->mov128_vf_gpr(vf3, t1);                        // qmtc2.ni vf3, t1
  c->pextuh(t9, r0, t9);                            // pextuh t9, r0, t9
  c->sqc2(vf14, 80, v1);                            // sqc2 vf14, 80(v1)
  c->daddiu(at, at, 16);                            // daddiu at, at, 16
  c->addiu(s4, s4, 2);                              // addiu s4, s4, 2
  c->sqc2(vf15, 128, v1);                           // sqc2 vf15, 128(v1)
  bc = c->sgpr64(t5) == c->sgpr64(s5);              // beq t5, s5, L116
  c->daddiu(t5, t5, 2);                             // daddiu t5, t5, 2
  if (bc) {goto block_56;}                          // branch non-likely

  // Unknown instr: vcallms 311
  c->sw(s3, 108, v1);                               // sw s3, 108(v1)
  c->lwr(s3, 80, v0);                               // lwr s3, 80(v0)
  c->daddu(t7, t7, t4);                             // daddu t7, t7, t4
  c->lwl(s3, 93, v0);                               // lwl s3, 93(v0)
  c->daddu(s0, s0, t4);                             // daddu s0, s0, t4
  c->sw(a0, 92, v1);                                // sw a0, 92(v1)
  c->pcpyud(t6, t6, r0);                            // pcpyud t6, t6, r0
  c->sb(s4, 0, t7);                                 // sb s4, 0(t7)
  c->pextlb(s3, r0, s3);                            // pextlb s3, r0, s3
  c->sb(s4, 0, s0);                                 // sb s4, 0(s0)
  c->dsllv(s3, s3, s2);                             // dsllv s3, s3, s2
  c->lq(t2, 112, v0);                               // lq t2, 112(v0)
  c->paddh(s3, s3, s1);                             // paddh s3, s3, s1
  c->lbu(t7, 0, t6);                                // lbu t7, 0(t6)
  c->dsrl32(t6, t6, 0);                             // dsrl32 t6, t6, 0
  c->lw(a0, 4, at);                                 // lw a0, 4(at)
  c->pextuh(a2, a1, a2);                            // pextuh a2, a1, a2
  c->lbu(s0, 0, t6);                                // lbu s0, 0(t6)
  c->pextlb(t0, r0, t2);                            // pextlb t0, r0, t2
  c->mov128_vf_gpr(vf4, t9);                        // qmtc2.ni vf4, t9
  c->pextlh(t1, a1, t0);                            // pextlh t1, a1, t0
  c->mov128_vf_gpr(vf1, a2);                        // qmtc2.ni vf1, a2
  c->pextuh(t0, a1, t0);                            // pextuh t0, a1, t0
  c->mov128_vf_gpr(vf2, t1);                        // qmtc2.ni vf2, t1
  c->pextub(t9, r0, t8);                            // pextub t9, r0, t8
  c->mov128_vf_gpr(vf3, t0);                        // qmtc2.ni vf3, t0
  c->pextlh(gp, r0, t9);                            // pextlh gp, r0, t9
  c->sqc2(vf14, 112, v1);                           // sqc2 vf14, 112(v1)
  c->daddiu(v0, v0, 48);                            // daddiu v0, v0, 48
  bc = c->sgpr64(s4) == c->sgpr64(s5);              // beq s4, s5, L117
  c->sqc2(vf15, 160, v1);                           // sqc2 vf15, 160(v1)
  if (bc) {goto block_57;}                          // branch non-likely

  // Unknown instr: vcallms 311
  c->sw(s3, 140, v1);                               // sw s3, 140(v1)
  c->lwr(t6, 40, v0);                               // lwr t6, 40(v0)
  c->daddu(t7, t7, t4);                             // daddu t7, t7, t4
  c->lwl(t6, 53, v0);                               // lwl t6, 53(v0)
  c->daddu(s0, s0, t4);                             // daddu s0, s0, t4
  c->sw(a0, 124, v1);                               // sw a0, 124(v1)
  c->dsrl32(s3, s3, 0);                             // dsrl32 s3, s3, 0
  c->sb(t5, 0, t7);                                 // sb t5, 0(t7)
  c->pextlb(t6, r0, t6);                            // pextlb t6, r0, t6
  c->sb(t5, 0, s0);                                 // sb t5, 0(s0)
  c->pextlb(t6, r0, t6);                            // pextlb t6, r0, t6
  c->lq(a3, 80, v0);                                // lq a3, 80(v0)
  c->paddw(t6, t6, t3);                             // paddw t6, t6, t3
  c->lbu(t7, 0, t6);                                // lbu t7, 0(t6)
  c->dsrl32(t6, t6, 0);                             // dsrl32 t6, t6, 0
  c->lw(a0, 8, at);                                 // lw a0, 8(at)
  c->pextub(t2, r0, t2);                            // pextub t2, r0, t2
  c->lbu(s0, 0, t6);                                // lbu s0, 0(t6)
  c->pextlh(t0, a1, t2);                            // pextlh t0, a1, t2
  c->mov128_vf_gpr(vf4, gp);                        // qmtc2.ni vf4, gp
  c->pextuh(t2, a1, t2);                            // pextuh t2, a1, t2
  c->mov128_vf_gpr(vf1, t0);                        // qmtc2.ni vf1, t0
  c->pextlb(t1, r0, a3);                            // pextlb t1, r0, a3
  c->mov128_vf_gpr(vf2, t2);                        // qmtc2.ni vf2, t2
  c->pextlh(a2, a1, t1);                            // pextlh a2, a1, t1
  c->mov128_vf_gpr(vf3, a2);                        // qmtc2.ni vf3, a2
  c->pextuh(t9, r0, t9);                            // pextuh t9, r0, t9
  c->sqc2(vf14, 144, v1);                           // sqc2 vf14, 144(v1)
  c->daddiu(v1, v1, 128);                           // daddiu v1, v1, 128
  c->addiu(s4, s4, 2);                              // addiu s4, s4, 2
  c->sqc2(vf15, 64, v1);                            // sqc2 vf15, 64(v1)
  bc = c->sgpr64(t5) == c->sgpr64(s5);              // beq t5, s5, L118
  c->daddiu(t5, t5, 2);                             // daddiu t5, t5, 2
  if (bc) {goto block_58;}                          // branch non-likely

  // Unknown instr: vcallms 311
  c->sw(s3, 44, v1);                                // sw s3, 44(v1)
  c->lwr(s3, 56, v0);                               // lwr s3, 56(v0)
  c->daddu(t7, t7, t4);                             // daddu t7, t7, t4
  c->lwl(s3, 69, v0);                               // lwl s3, 69(v0)
  c->daddu(s0, s0, t4);                             // daddu s0, s0, t4
  c->sw(a0, 28, v1);                                // sw a0, 28(v1)
  c->pcpyud(t6, t6, r0);                            // pcpyud t6, t6, r0
  c->sb(s4, 0, t7);                                 // sb s4, 0(t7)
  c->pextlb(s3, r0, s3);                            // pextlb s3, r0, s3
  c->sb(s4, 0, s0);                                 // sb s4, 0(s0)
  c->dsllv(s3, s3, s2);                             // dsllv s3, s3, s2
  c->lq(t8, 0, s6);                                 // lq t8, 0(s6)
  c->paddh(s3, s3, s1);                             // paddh s3, s3, s1
  c->lbu(t7, 0, t6);                                // lbu t7, 0(t6)
  c->dsrl32(t6, t6, 0);                             // dsrl32 t6, t6, 0
  c->lw(a0, 12, at);                                // lw a0, 12(at)
  c->pextuh(t0, a1, t1);                            // pextuh t0, a1, t1
  c->lbu(s0, 0, t6);                                // lbu s0, 0(t6)
  c->pextub(a3, r0, a3);                            // pextub a3, r0, a3
  c->mov128_vf_gpr(vf4, t9);                        // qmtc2.ni vf4, t9
  c->pextlh(t2, a1, a3);                            // pextlh t2, a1, a3
  c->mov128_vf_gpr(vf1, t0);                        // qmtc2.ni vf1, t0
  c->pextuh(a3, a1, a3);                            // pextuh a3, a1, a3
  c->mov128_vf_gpr(vf2, t2);                        // qmtc2.ni vf2, t2
  c->pextlb(t9, r0, t8);                            // pextlb t9, r0, t8
  c->mov128_vf_gpr(vf3, a3);                        // qmtc2.ni vf3, a3
  c->pextlh(gp, r0, t9);                            // pextlh gp, r0, t9
  c->sqc2(vf14, 48, v1);                            // sqc2 vf14, 48(v1)
  c->daddiu(s6, s6, 16);                            // daddiu s6, s6, 16
  bc = c->sgpr64(s4) != c->sgpr64(s5);              // bne s4, s5, L114
  c->sqc2(vf15, 96, v1);                            // sqc2 vf15, 96(v1)
  if (bc) {goto block_50;}                          // branch non-likely

  //beq r0, r0, L118                                // beq r0, r0, L118
  c->daddiu(v1, v1, 32);                            // daddiu v1, v1, 32
  goto block_58;                                    // branch always

  
block_56:
  //beq r0, r0, L118                                // beq r0, r0, L118
  c->daddiu(v1, v1, 64);                            // daddiu v1, v1, 64
  goto block_58;                                    // branch always

  
block_57:
  //beq r0, r0, L118                                // beq r0, r0, L118
  c->daddiu(v1, v1, 96);                            // daddiu v1, v1, 96
  goto block_58;                                    // branch always

  
block_58:
  c->sw(a0, 28, v1);                                // sw a0, 28(v1)
  c->daddiu(s5, s5, 1);                             // daddiu s5, s5, 1
  c->daddiu(v1, v1, 32);                            // daddiu v1, v1, 32
  // nop                                            // sll r0, r0, 0
  get_fake_spad_addr2(a3, cache.fake_scratchpad_data, 0, c);// lui a3, 28672
  // nop                                            // sll r0, r0, 0
  c->lw(a0, 7444, a3);                              // lw a0, 7444(a3)
  // nop                                            // sll r0, r0, 0
  c->lw(a1, 7436, a3);                              // lw a1, 7436(a3)
  // nop                                            // sll r0, r0, 0
  c->lw(a3, 7440, a3);                              // lw a3, 7440(a3)
  // nop                                            // sll r0, r0, 0
  c->sb(s5, 17, a1);                                // sb s5, 17(a1)
  // nop                                            // sll r0, r0, 0
  c->lbu(s0, 0, a0);                                // lbu s0, 0(a0)
  c->addiu(s4, r0, 0);                              // addiu s4, r0, 0
  c->lbu(s1, 7, a0);                                // lbu s1, 7(a0)
  c->addiu(t8, r0, 0);                              // addiu t8, r0, 0
  c->lbu(s2, 8, a0);                                // lbu s2, 8(a0)
  c->sll(s0, s0, 2);                                // sll s0, s0, 2
  c->daddu(s0, s0, a0);                             // daddu s0, s0, a0
  c->sll(s1, s1, 2);                                // sll s1, s1, 2
  bc = c->sgpr64(s1) == 0;                          // beq s1, r0, L120
  c->daddu(s1, s1, s0);                             // daddu s1, s1, s0
  if (bc) {goto block_61;}                          // branch non-likely

  
block_59:
  c->lbu(s3, 0, s0);                                // lbu s3, 0(s0)
  c->daddu(s4, s4, t4);                             // daddu s4, s4, t4
  c->lbu(s6, 1, s0);                                // lbu s6, 1(s0)
  c->daddu(t8, t8, t4);                             // daddu t8, t8, t4
  c->lbu(t9, 0, s4);                                // lbu t9, 0(s4)
  c->daddu(s3, s3, t3);                             // daddu s3, s3, t3
  c->lbu(s4, 0, s3);                                // lbu s4, 0(s3)
  c->daddu(s6, s6, t3);                             // daddu s6, s6, t3
  c->sb(t9, 0, t8);                                 // sb t9, 0(t8)
  c->daddiu(s0, s0, 4);                             // daddiu s0, s0, 4
  bc = c->sgpr64(s0) != c->sgpr64(s1);              // bne s0, s1, L119
  c->lbu(t8, 0, s6);                                // lbu t8, 0(s6)
  if (bc) {goto block_59;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->daddu(s4, s4, t4);                             // daddu s4, s4, t4
  c->lbu(t9, 0, s4);                                // lbu t9, 0(s4)
  c->daddu(t8, t8, t4);                             // daddu t8, t8, t4
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sb(t9, 0, t8);                                 // sb t9, 0(t8)
  // nop                                            // sll r0, r0, 0
  
block_61:
  c->sll(s2, s2, 2);                                // sll s2, s2, 2
  c->daddiu(t5, a3, 32);                            // daddiu t5, a3, 32
  bc = c->sgpr64(s2) == 0;                          // beq s2, r0, L123
  c->daddu(s2, s2, s1);                             // daddu s2, s2, s1
  if (bc) {goto block_65;}                          // branch non-likely

  c->lbu(s3, 0, s0);                                // lbu s3, 0(s0)
  c->daddiu(t6, a3, 192);                           // daddiu t6, a3, 192
  c->lbu(s6, 1, s0);                                // lbu s6, 1(s0)
  c->daddiu(t7, a3, 448);                           // daddiu t7, a3, 448
  // nop                                            // sll r0, r0, 0
  c->daddu(s3, s3, t6);                             // daddu s3, s3, t6
  c->lbu(s3, 0, s3);                                // lbu s3, 0(s3)
  c->daddu(s6, s6, t3);                             // daddu s6, s6, t3
  c->lbu(s6, 0, s6);                                // lbu s6, 0(s6)
  c->daddiu(v1, v1, -32);                           // daddiu v1, v1, -32
  // nop                                            // sll r0, r0, 0
  c->daddu(s3, s3, t5);                             // daddu s3, s3, t5
  c->lbu(s4, 0, s3);                                // lbu s4, 0(s3)
  c->daddu(s6, s6, t4);                             // daddu s6, s6, t4
  // nop                                            // sll r0, r0, 0
  c->daddiu(s0, s0, 4);                             // daddiu s0, s0, 4
  c->sb(s5, 0, s6);                                 // sb s5, 0(s6)
  c->daddiu(s5, s5, 1);                             // daddiu s5, s5, 1
  bc = c->sgpr64(s0) == c->sgpr64(s2);              // beq s0, s2, L122
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_64;}                          // branch non-likely

  
block_63:
  c->lbu(s3, 0, s0);                                // lbu s3, 0(s0)
  c->sll(s4, s4, 5);                                // sll s4, s4, 5
  c->lbu(s6, 1, s0);                                // lbu s6, 1(s0)
  c->daddu(s4, s4, t7);                             // daddu s4, s4, t7
  c->lq(t9, 0, s4);                                 // lq t9, 0(s4)
  c->daddu(s3, s3, t6);                             // daddu s3, s3, t6
  c->lbu(s3, 0, s3);                                // lbu s3, 0(s3)
  c->daddu(s6, s6, t3);                             // daddu s6, s6, t3
  c->lbu(s6, 0, s6);                                // lbu s6, 0(s6)
  c->daddiu(v1, v1, 32);                            // daddiu v1, v1, 32
  c->lq(gp, 16, s4);                                // lq gp, 16(s4)
  c->daddu(s3, s3, t5);                             // daddu s3, s3, t5
  c->lbu(s4, 0, s3);                                // lbu s4, 0(s3)
  c->daddu(s6, s6, t4);                             // daddu s6, s6, t4
  c->sq(t9, 0, v1);                                 // sq t9, 0(v1)
  c->daddiu(s0, s0, 4);                             // daddiu s0, s0, 4
  c->sb(s5, 0, s6);                                 // sb s5, 0(s6)
  c->daddiu(s5, s5, 1);                             // daddiu s5, s5, 1
  bc = c->sgpr64(s0) != c->sgpr64(s2);              // bne s0, s2, L121
  c->sq(gp, 16, v1);                                // sq gp, 16(v1)
  if (bc) {goto block_63;}                          // branch non-likely

  
block_64:
  // nop                                            // sll r0, r0, 0
  c->sll(s4, s4, 5);                                // sll s4, s4, 5
  // nop                                            // sll r0, r0, 0
  c->daddu(s4, s4, t7);                             // daddu s4, s4, t7
  c->lq(t9, 0, s4);                                 // lq t9, 0(s4)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(v1, v1, 32);                            // daddiu v1, v1, 32
  c->lq(gp, 16, s4);                                // lq gp, 16(s4)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sq(t9, 0, v1);                                 // sq t9, 0(v1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sq(gp, 16, v1);                                // sq gp, 16(v1)
  
block_65:
  c->sh(s5, 20, a1);                                // sh s5, 20(a1)
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->lq(s0, 7552, at);                              // lq s0, 7552(at)
  c->lq(s1, 7568, at);                              // lq s1, 7568(at)
  c->lq(s2, 7584, at);                              // lq s2, 7584(at)
  c->lq(s3, 7600, at);                              // lq s3, 7600(at)
  c->lq(s4, 7616, at);                              // lq s4, 7616(at)
  c->lq(s5, 7632, at);                              // lq s5, 7632(at)
  c->lq(s6, 7648, at);                              // lq s6, 7648(at)
  c->lq(t8, 7664, at);                              // lq t8, 7664(at)
  c->lq(t9, 7680, at);                              // lq t9, 7680(at)
  c->lq(gp, 7696, at);                              // lq gp, 7696(at)
  c->lq(ra, 7744, at);                              // lq ra, 7744(at)
  c->lq(sp, 7712, at);                              // lq sp, 7712(at)
  //jr ra                                           // jr ra
  c->lq(fp, 7728, at);                              // lq fp, 7728(at)
  goto end_of_function;                             // return

  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c(-1, 0, "*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("mercneric-convert", execute, 512);
}

} // namespace mercneric_convert
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace generic_merc_init_asm {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* gsf_buffer; // *gsf-buffer*
  void* inv_init_table; // *inv-init-table*
  void* screen_shot_work; // *screen-shot-work*
  void* viewport_array; // *viewport-array*
  void* generic_envmap_proc; // generic-envmap-proc
  void* generic_light_proc; // generic-light-proc
  void* generic_prepare_dma_double; // generic-prepare-dma-double
  void* generic_prepare_dma_single; // generic-prepare-dma-single
  void* high_speed_reject; // high-speed-reject
  void* mercneric_convert; // mercneric-convert
  void* mercneric_vu0_block; // mercneric-vu0-block
  void* upload_vu0_program; // upload-vu0-program
  void* view_get_active_math_camera; // view-get-active-math-camera
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->load_symbol2(t9, cache.upload_vu0_program);    // lw t9, upload-vu0-program(s7)
  c->load_symbol2(a0, cache.mercneric_vu0_block);   // lw a0, mercneric-vu0-block(s7)
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->ori(a1, v1, 84);                               // ori a1, v1, 84
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->load_symbol2(t9, cache.view_get_active_math_camera);// lw t9, view-get-active-math-camera(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(a0, v0);                                 // or a0, v0, r0
  c->addiu(v1, r0, 7136);                           // addiu v1, r0, 7136
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->daddu(v1, v1, a1);                             // daddu v1, v1, a1
  c->load_symbol2(a1, cache.mercneric_convert);     // lw a1, mercneric-convert(s7)
  c->sw(a1, 324, v1);                               // sw a1, 324(v1)
  c->load_symbol2(a1, cache.generic_prepare_dma_single);// lw a1, generic-prepare-dma-single(s7)
  c->sw(a1, 328, v1);                               // sw a1, 328(v1)
  c->load_symbol2(a1, cache.generic_prepare_dma_double);// lw a1, generic-prepare-dma-double(s7)
  c->sw(a1, 332, v1);                               // sw a1, 332(v1)
  c->load_symbol2(a1, cache.generic_light_proc);    // lw a1, generic-light-proc(s7)
  c->sw(a1, 336, v1);                               // sw a1, 336(v1)
  c->load_symbol2(a1, cache.generic_envmap_proc);   // lw a1, generic-envmap-proc(s7)
  c->sw(a1, 340, v1);                               // sw a1, 340(v1)
  c->load_symbol2(a1, cache.high_speed_reject);     // lw a1, high-speed-reject(s7)
  c->sw(a1, 344, v1);                               // sw a1, 344(v1)
  c->sw(r0, 320, v1);                               // sw r0, 320(v1)
  c->daddu(t0, r0, v1);                             // daddu t0, r0, v1
  c->daddiu(t1, a0, 156);                           // daddiu t1, a0, 156
  c->lq(a1, 0, t1);                                 // lq a1, 0(t1)
  c->lq(a2, 16, t1);                                // lq a2, 16(t1)
  c->lq(a3, 32, t1);                                // lq a3, 32(t1)
  c->lq(t1, 48, t1);                                // lq t1, 48(t1)
  c->sq(a1, 0, t0);                                 // sq a1, 0(t0)
  c->sq(a2, 16, t0);                                // sq a2, 16(t0)
  c->sq(a3, 32, t0);                                // sq a3, 32(t0)
  c->sq(t1, 48, t0);                                // sq t1, 48(t0)
  c->daddiu(t0, v1, 64);                            // daddiu t0, v1, 64
  c->daddiu(t1, a0, 220);                           // daddiu t1, a0, 220
  c->lq(a1, 0, t1);                                 // lq a1, 0(t1)
  c->lq(a2, 16, t1);                                // lq a2, 16(t1)
  c->lq(a3, 32, t1);                                // lq a3, 32(t1)
  c->lq(t1, 48, t1);                                // lq t1, 48(t1)
  c->sq(a1, 0, t0);                                 // sq a1, 0(t0)
  c->sq(a2, 16, t0);                                // sq a2, 16(t0)
  c->sq(a3, 32, t0);                                // sq a3, 32(t0)
  c->sq(t1, 48, t0);                                // sq t1, 48(t0)
  c->daddiu(t0, v1, 128);                           // daddiu t0, v1, 128
  c->daddiu(t1, a0, 428);                           // daddiu t1, a0, 428
  c->lq(a1, 0, t1);                                 // lq a1, 0(t1)
  c->lq(a2, 16, t1);                                // lq a2, 16(t1)
  c->lq(a3, 32, t1);                                // lq a3, 32(t1)
  c->lq(t1, 48, t1);                                // lq t1, 48(t1)
  c->sq(a1, 0, t0);                                 // sq a1, 0(t0)
  c->sq(a2, 16, t0);                                // sq a2, 16(t0)
  c->sq(a3, 32, t0);                                // sq a3, 32(t0)
  c->sq(t1, 48, t0);                                // sq t1, 48(t0)
  c->lui(a1, 16256);                                // lui a1, 16256
  c->mtc1(f0, a1);                                  // mtc1 f0, a1
  c->lwc1(f1, 12, a0);                              // lwc1 f1, 12(a0)
  c->divs(f1, f0, f1);                              // div.s f1, f0, f1
  c->lui(a1, 16256);                                // lui a1, 16256
  c->mtc1(f0, a1);                                  // mtc1 f0, a1
  c->lwc1(f2, 16, a0);                              // lwc1 f2, 16(a0)
  c->divs(f0, f0, f2);                              // div.s f0, f0, f2
  c->daddiu(a3, v1, 352);                           // daddiu a3, v1, 352
  c->daddiu(a2, v1, 368);                           // daddiu a2, v1, 368
  c->daddiu(a1, v1, 384);                           // daddiu a1, v1, 384
  c->daddiu(v1, v1, 400);                           // daddiu v1, v1, 400
  c->swc1(f1, 0, a3);                               // swc1 f1, 0(a3)
  c->negs(f1, f1);                                  // neg.s f1, f1
  c->swc1(f1, 4, a3);                               // swc1 f1, 4(a3)
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  c->swc1(f1, 8, a3);                               // swc1 f1, 8(a3)
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  c->swc1(f1, 12, a3);                              // swc1 f1, 12(a3)
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  c->swc1(f1, 0, a2);                               // swc1 f1, 0(a2)
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  c->swc1(f1, 4, a2);                               // swc1 f1, 4(a2)
  c->swc1(f0, 8, a2);                               // swc1 f0, 8(a2)
  c->negs(f0, f0);                                  // neg.s f0, f0
  c->swc1(f0, 12, a2);                              // swc1 f0, 12(a2)
  c->lui(a2, 15104);                                // lui a2, 15104
  c->mtc1(f0, a2);                                  // mtc1 f0, a2
  c->swc1(f0, 0, a1);                               // swc1 f0, 0(a1)
  c->lui(a2, 15104);                                // lui a2, 15104
  c->mtc1(f0, a2);                                  // mtc1 f0, a2
  c->swc1(f0, 4, a1);                               // swc1 f0, 4(a1)
  c->lui(a2, 16128);                                // lui a2, 16128
  c->mtc1(f0, a2);                                  // mtc1 f0, a2
  c->swc1(f0, 8, a1);                               // swc1 f0, 8(a1)
  c->lwc1(f0, 908, a0);                             // lwc1 f0, 908(a0)
  c->swc1(f0, 12, a1);                              // swc1 f0, 12(a1)
  c->load_symbol2(a1, cache.viewport_array);        // lw a1, *viewport-array*(s7)
  c->lwu(a1, 0, a1);                                // lwu a1, 0(a1)
  bc = c->sgpr64(a1) != 0;                          // bne a1, r0, L167
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  c->mov64(a1, v1);                                 // or a1, v1, r0
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 0, a1);                               // swc1 f0, 0(a1)
  c->lui(a2, -16972);                               // lui a2, -16972
  c->mtc1(f0, a2);                                  // mtc1 f0, a2
  c->swc1(f0, 4, a1);                               // swc1 f0, 4(a1)
  c->lwc1(f0, 0, a0);                               // lwc1 f0, 0(a0)
  c->swc1(f0, 8, a1);                               // swc1 f0, 8(a1)
  c->lui(a0, 16256);                                // lui a0, 16256
  c->mtc1(f0, a0);                                  // mtc1 f0, a0
  c->swc1(f0, 12, a1);                              // swc1 f0, 12(a1)
  //beq r0, r0, L169                                // beq r0, r0, L169
  // nop                                            // sll r0, r0, 0
  goto block_5;                                     // branch always

  
block_2:
  c->addiu(a2, r0, 1);                              // addiu a2, r0, 1
  bc = c->sgpr64(a1) != c->sgpr64(a2);              // bne a1, a2, L168
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_4;}                           // branch non-likely

  c->mov64(a1, v1);                                 // or a1, v1, r0
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 0, a1);                               // swc1 f0, 0(a1)
  c->lui(a2, -16736);                               // lui a2, -16736
  c->mtc1(f0, a2);                                  // mtc1 f0, a2
  c->swc1(f0, 4, a1);                               // swc1 f0, 4(a1)
  c->lwc1(f0, 0, a0);                               // lwc1 f0, 0(a0)
  c->swc1(f0, 8, a1);                               // swc1 f0, 8(a1)
  c->lui(a0, 16256);                                // lui a0, 16256
  c->mtc1(f0, a0);                                  // mtc1 f0, a0
  c->swc1(f0, 12, a1);                              // swc1 f0, 12(a1)
  //beq r0, r0, L169                                // beq r0, r0, L169
  // nop                                            // sll r0, r0, 0
  goto block_5;                                     // branch always

  
block_4:
  c->mov64(a1, v1);                                 // or a1, v1, r0
  c->lui(a2, -16768);                               // lui a2, -16768
  c->mtc1(f0, a2);                                  // mtc1 f0, a2
  c->swc1(f0, 0, a1);                               // swc1 f0, 0(a1)
  c->lui(a2, -16972);                               // lui a2, -16972
  c->mtc1(f0, a2);                                  // mtc1 f0, a2
  c->swc1(f0, 4, a1);                               // swc1 f0, 4(a1)
  c->lwc1(f0, 0, a0);                               // lwc1 f0, 0(a0)
  c->swc1(f0, 8, a1);                               // swc1 f0, 8(a1)
  c->lui(a0, 16256);                                // lui a0, 16256
  c->mtc1(f0, a0);                                  // mtc1 f0, a0
  c->swc1(f0, 12, a1);                              // swc1 f0, 12(a1)
  
block_5:
  c->load_symbol2(a0, cache.screen_shot_work);      // lw a0, *screen-shot-work*(s7)
  if (((s64)c->sgpr64(a0)) == ((s64)0)) {           // beql a0, r0, L170
    c->mov64(a0, s7);                               // or a0, s7, r0
    goto block_8;
  }
  
block_7:
  c->load_symbol2(a0, cache.screen_shot_work);      // lw a0, *screen-shot-work*(s7)
  c->lh(a0, 0, a0);                                 // lh a0, 0(a0)
  c->daddiu(a1, a0, 1);                             // daddiu a1, a0, 1
  c->daddiu(a0, s7, 4);                             // daddiu a0, s7, 4
  c->movz(a0, s7, a1);                              // movz a0, s7, a1
  
block_8:
  bc = c->sgpr64(s7) == c->sgpr64(a0);              // beq s7, a0, L171
  c->mov64(a0, s7);                                 // or a0, s7, r0
  if (bc) {goto block_10;}                          // branch non-likely

  c->load_symbol2(a0, cache.screen_shot_work);      // lw a0, *screen-shot-work*(s7)
  c->lh(a0, 0, a0);                                 // lh a0, 0(a0)
  c->load_symbol2(a1, cache.screen_shot_work);      // lw a1, *screen-shot-work*(s7)
  c->lh(a1, 2, a1);                                 // lh a1, 2(a1)
  c->mtc1(f0, a1);                                  // mtc1 f0, a1
  c->cvtsw(f1, f0);                                 // cvt.s.w f1, f0
  c->div(a0, a1);                                   // div a0, a1
  c->mfhi(a2);                                      // mfhi a2
  c->mtc1(f0, a2);                                  // mtc1 f0, a2
  c->cvtsw(f0, f0);                                 // cvt.s.w f0, f0
  c->divs(f0, f0, f1);                              // div.s f0, f0, f1
  c->div(a0, a1);                                   // div a0, a1
  c->mflo(a0);                                      // mflo a0
  c->mtc1(f2, a0);                                  // mtc1 f2, a0
  c->cvtsw(f2, f2);                                 // cvt.s.w f2, f2
  c->divs(f1, f2, f1);                              // div.s f1, f2, f1
  c->lwc1(f2, 0, v1);                               // lwc1 f2, 0(v1)
  c->lui(a0, 15104);                                // lui a0, 15104
  c->mtc1(f3, a0);                                  // mtc1 f3, a0
  c->muls(f0, f3, f0);                              // mul.s f0, f3, f0
  c->subs(f0, f2, f0);                              // sub.s f0, f2, f0
  c->swc1(f0, 0, v1);                               // swc1 f0, 0(v1)
  c->lwc1(f0, 4, v1);                               // lwc1 f0, 4(v1)
  c->lui(a0, 15104);                                // lui a0, 15104
  c->mtc1(f2, a0);                                  // mtc1 f2, a0
  c->muls(f1, f2, f1);                              // mul.s f1, f2, f1
  c->subs(f0, f0, f1);                              // sub.s f0, f0, f1
  c->swc1(f0, 4, v1);                               // swc1 f0, 4(v1)
  c->mfc1(a0, f0);                                  // mfc1 a0, f0
  
block_10:
  c->load_symbol2(v1, cache.gsf_buffer);            // lw v1, *gsf-buffer*(s7)
  c->daddiu(a0, v1, 32);                            // daddiu a0, v1, 32
  c->sw(a0, 0, v1);                                 // sw a0, 0(v1)
  c->daddiu(a0, v1, 448);                           // daddiu a0, v1, 448
  c->sw(a0, 4, v1);                                 // sw a0, 4(v1)
  c->daddiu(a0, v1, 6608);                          // daddiu a0, v1, 6608
  c->sw(a0, 8, v1);                                 // sw a0, 8(v1)
  c->daddiu(a0, v1, 2784);                          // daddiu a0, v1, 2784
  c->sw(a0, 2752, v1);                              // sw a0, 2752(v1)
  c->daddiu(a0, v1, 3200);                          // daddiu a0, v1, 3200
  c->sw(a0, 2756, v1);                              // sw a0, 2756(v1)
  c->daddiu(a0, v1, 6608);                          // daddiu a0, v1, 6608
  c->sw(a0, 2760, v1);                              // sw a0, 2760(v1)
  c->load_symbol2(v1, cache.gsf_buffer);            // lw v1, *gsf-buffer*(s7)
  c->daddiu(a0, v1, 5504);                          // daddiu a0, v1, 5504
  c->mov64(a0, a0);                                 // or a0, a0, r0
  c->load_symbol2(a1, cache.inv_init_table);        // lw a1, *inv-init-table*(s7)
  c->mov64(a1, a1);                                 // or a1, a1, r0
  c->sq(r0, 0, a0);                                 // sq r0, 0(a0)
  c->sq(r0, 544, a0);                               // sq r0, 544(a0)
  c->sq(r0, 16, a0);                                // sq r0, 16(a0)
  c->sq(r0, 560, a0);                               // sq r0, 560(a0)
  c->sq(r0, 208, a0);                               // sq r0, 208(a0)
  c->sq(r0, 752, a0);                               // sq r0, 752(a0)
  c->sq(r0, 224, a0);                               // sq r0, 224(a0)
  c->sq(r0, 768, a0);                               // sq r0, 768(a0)
  c->sq(r0, 240, a0);                               // sq r0, 240(a0)
  c->sq(r0, 784, a0);                               // sq r0, 784(a0)
  c->sq(r0, 384, a0);                               // sq r0, 384(a0)
  c->sq(r0, 800, a0);                               // sq r0, 800(a0)
  c->sq(r0, 400, a0);                               // sq r0, 400(a0)
  c->sq(r0, 944, a0);                               // sq r0, 944(a0)
  c->sq(r0, 416, a0);                               // sq r0, 416(a0)
  c->sq(r0, 960, a0);                               // sq r0, 960(a0)
  c->sq(r0, 432, a0);                               // sq r0, 432(a0)
  c->sq(r0, 976, a0);                               // sq r0, 976(a0)
  c->sq(r0, 448, a0);                               // sq r0, 448(a0)
  c->sq(r0, 992, a0);                               // sq r0, 992(a0)
  c->sq(r0, 464, a0);                               // sq r0, 464(a0)
  c->sq(r0, 1008, a0);                              // sq r0, 1008(a0)
  c->sq(r0, 480, a0);                               // sq r0, 480(a0)
  c->sq(r0, 1024, a0);                              // sq r0, 1024(a0)
  c->sq(r0, 496, a0);                               // sq r0, 496(a0)
  c->sq(r0, 1040, a0);                              // sq r0, 1040(a0)
  c->sq(r0, 512, a0);                               // sq r0, 512(a0)
  c->sq(r0, 1056, a0);                              // sq r0, 1056(a0)
  c->sq(r0, 528, a0);                               // sq r0, 528(a0)
  c->sq(r0, 1072, a0);                              // sq r0, 1072(a0)
  c->addiu(a2, r0, 8);                              // addiu a2, r0, 8
  // nop                                            // sll r0, r0, 0
  
block_11:
  c->lhu(t1, 2, a1);                                // lhu t1, 2(a1)
  c->daddiu(a2, a2, -1);                            // daddiu a2, a2, -1
  c->lbu(a3, 0, a1);                                // lbu a3, 0(a1)
  // nop                                            // sll r0, r0, 0
  c->lbu(t0, 1, a1);                                // lbu t0, 1(a1)
  // nop                                            // sll r0, r0, 0
  c->daddu(t1, t1, a0);                             // daddu t1, t1, a0
  c->daddiu(a1, a1, 4);                             // daddiu a1, a1, 4
  
block_12:
  c->sb(t0, 0, t1);                                 // sb t0, 0(t1)
  c->daddiu(a3, a3, -1);                            // daddiu a3, a3, -1
  c->sb(t0, 550, t1);                               // sb t0, 550(t1)
  c->daddiu(t0, t0, 2);                             // daddiu t0, t0, 2
  c->daddiu(t1, t1, 3);                             // daddiu t1, t1, 3
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L173
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely

  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L172
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_11;}                          // branch non-likely

  c->daddu(a0, r0, v1);                             // daddu a0, r0, v1
  c->mov64(a0, a0);                                 // or a0, a0, r0
  c->sq(r0, 416, a0);                               // sq r0, 416(a0)
  c->sq(r0, 432, a0);                               // sq r0, 432(a0)
  c->daddiu(v1, v1, 2752);                          // daddiu v1, v1, 2752
  c->mov64(v1, v1);                                 // or v1, v1, r0
  c->sq(r0, 416, v1);                               // sq r0, 416(v1)
  c->sq(r0, 432, v1);                               // sq r0, 432(v1)
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->lui(v1, 4096);                                 // lui v1, 4096
  c->ori(v1, v1, 32768);                            // ori v1, v1, 32768
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->ori(a0, a0, 84);                               // ori a0, a0, 84
  c->lw(a1, 0, v1);                                 // lw a1, 0(v1)
  c->andi(a1, a1, 256);                             // andi a1, a1, 256
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L175
  //   TODO - fix DANGER jump to delay slot, this MUST be fixed manually!
  c->lw(a1, 0, a0);                                 // lw a1, 0(a0)
  //   TODO - fix if (bc) {goto block_-1;}                          // branch non-likely

  
block_15:
  c->lw(a1, 0, a0);                                 // lw a1, 0(a0)
  // nop                                            // sll r0, r0, 0
  c->lw(a2, 0, v1);                                 // lw a2, 0(v1)
  // nop                                            // sll r0, r0, 0
  c->andi(a2, a2, 256);                             // andi a2, a2, 256
  c->daddiu(a1, a1, 1);                             // daddiu a1, a1, 1
  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L174
  c->sw(a1, 0, a0);                                 // sw a1, 0(a0)
  if (bc) {goto block_15;}                          // branch non-likely

  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
  goto end_of_function;                             // return

end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c(-1, 0, "*fake-scratchpad-data*").c();
  cache.gsf_buffer = intern_from_c(-1, 0, "*gsf-buffer*").c();
  cache.inv_init_table = intern_from_c(-1, 0, "*inv-init-table*").c();
  cache.screen_shot_work = intern_from_c(-1, 0, "*screen-shot-work*").c();
  cache.viewport_array = intern_from_c(-1, 0, "*viewport-array*").c();
  cache.generic_envmap_proc = intern_from_c(-1, 0, "generic-envmap-proc").c();
  cache.generic_light_proc = intern_from_c(-1, 0, "generic-light-proc").c();
  cache.generic_prepare_dma_double = intern_from_c(-1, 0, "generic-prepare-dma-double").c();
  cache.generic_prepare_dma_single = intern_from_c(-1, 0, "generic-prepare-dma-single").c();
  cache.high_speed_reject = intern_from_c(-1, 0, "high-speed-reject").c();
  cache.mercneric_convert = intern_from_c(-1, 0, "mercneric-convert").c();
  cache.mercneric_vu0_block = intern_from_c(-1, 0, "mercneric-vu0-block").c();
  cache.upload_vu0_program = intern_from_c(-1, 0, "upload-vu0-program").c();
  cache.view_get_active_math_camera = intern_from_c(-1, 0, "view-get-active-math-camera").c();
  gLinkedFunctionTable.reg("generic-merc-init-asm", execute, 256);
}

} // namespace generic_merc_init_asm
} // namespace Mips2C