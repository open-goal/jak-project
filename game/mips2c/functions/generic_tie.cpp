
//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/kscheme.h"
namespace Mips2C {
namespace generic_tie_dma_to_spad_sync {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  // nop                                            // sll r0, r0, 0
  get_fake_spad_addr(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->ori(a2, r0, 65535);                            // ori a2, r0, 65535
  c->lui(v1, 4096);                                 // lui v1, 4096
  // nop                                            // sll r0, r0, 0
  c->ori(v1, v1, 54272);                            // ori v1, v1, 54272 // SPR TO
  c->and_(a2, a1, a2);                              // and a2, a1, a2
  /*
  block_1:
  c->lw(a3, 0, v1);                                 // lw a3, 0(v1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(a3, a3, 256);                             // andi a3, a3, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L90
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_3;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(a1, a1, 1);                             // daddiu a1, a1, 1
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L89                                 // beq r0, r0, L89
  // nop                                            // sll r0, r0, 0
  goto block_1;                                     // branch always
  */


  block_3:
  c->addiu(a3, r0, 324);                            // addiu a3, r0, 324
  // c->sw(a2, 128, v1);                               // sw a2, 128(v1)
  u32 sadr = c->sgpr64(a2);
  // c->sw(a0, 48, v1);                                // sw a0, 48(v1)
  u32 tadr = c->sgpr64(a0);
  // c->sw(r0, 32, v1);                                // sw r0, 32(v1)
  // Unknown instr: sync.l
  // c->sw(a3, 0, v1);                                 // sw a3, 0(v1)
  // same and hack as generic merc.
  spad_to_dma_blerc_chain(cache.fake_scratchpad_data, sadr & 0x3fff, tadr);

  /*
  block_4:
  c->lw(a0, 0, v1);                                 // lw a0, 0(v1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(a0, a0, 256);                             // andi a0, a0, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L92
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_6;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(a1, a1, 1);                             // daddiu a1, a1, 1
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L91                                 // beq r0, r0, L91
  // nop                                            // sll r0, r0, 0
  goto block_4;                                     // branch always
   */


  block_6:
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
  gLinkedFunctionTable.reg("generic-tie-dma-to-spad-sync", execute, 128);
}

} // namespace generic_tie_dma_to_spad_sync
} // namespace Mips2C
