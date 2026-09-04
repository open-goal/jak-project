//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace calc_animation_from_spr {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* clear_frame_accumulator; // clear-frame-accumulator
  void* decompress_fixed_data_to_accumulator; // decompress-fixed-data-to-accumulator
  void* decompress_frame_data_pair_to_accumulator; // decompress-frame-data-pair-to-accumulator
  void* decompress_frame_data_to_accumulator; // decompress-frame-data-to-accumulator
  void* normalize_frame_quaternions; // normalize-frame-quaternions
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->mov64(v1, a1);                                 // or v1, a1, r0
  c->daddiu(sp, sp, -192);                          // daddiu sp, sp, -192
  c->sq(s0, 0, sp);                                 // sq s0, 0(sp)
  c->sq(s1, 16, sp);                                // sq s1, 16(sp)
  c->sq(s2, 32, sp);                                // sq s2, 32(sp)
  c->sq(s3, 48, sp);                                // sq s3, 48(sp)
  c->sq(s4, 64, sp);                                // sq s4, 64(sp)
  c->sq(s5, 80, sp);                                // sq s5, 80(sp)
  c->sq(s6, 96, sp);                                // sq s6, 96(sp)
  c->sq(t8, 112, sp);                               // sq t8, 112(sp)
  c->sq(t9, 128, sp);                               // sq t9, 128(sp)
  c->sq(gp, 144, sp);                               // sq gp, 144(sp)
  c->sq(fp, 160, sp);                               // sq fp, 160(sp)
  c->sq(ra, 176, sp);                               // sq ra, 176(sp)
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
  c->mov128_vf_gpr(vf15, r0);                       // qmtc2.i vf15, r0
  c->sw(a1, 0, sp);                                 // sw a1, 0(sp)
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lw(s1, 2384, v1);                              // lw s1, 2384(v1)
  c->daddiu(t7, v1, 1808);                          // daddiu t7, v1, 1808
  c->lui(s0, 4096);                                 // lui s0, 4096
  c->daddiu(t1, v1, 7328);                          // daddiu t1, v1, 7328
  bc = c->sgpr64(s1) == 0;                          // beq s1, r0, L20
  c->ori(s0, s0, 54272);                            // ori s0, s0, 54272
  if (bc) {goto block_12;}                          // branch non-likely

  c->lw(t2, 0, t7);                                 // lw t2, 0(t7)
  c->addiu(t3, r0, 7392);                           // addiu t3, r0, 7392
  c->lw(t4, 4, t7);                                 // lw t4, 4(t7)
  c->addiu(v1, r0, 256);                            // addiu v1, r0, 256
  c->sw(t2, 16, s0);                                // sw t2, 16(s0)
  c->vadd_bc(DEST::xyzw, BC::w, vf14, vf15, vf0);   // vaddw.xyzw vf14, vf15, vf0
  c->sw(t3, 128, s0);                               // sw t3, 128(s0)
  // nop                                            // sll r0, r0, 0
  c->sw(t4, 32, s0);                                // sw t4, 32(s0)
  // Unknown instr: sync.l
  c->sw(v1, 0, s0);                                 // sw v1, 0(s0)
  // Unknown instr: sync.l
  c->load_symbol2(t9, cache.clear_frame_accumulator);// lw t9, clear-frame-accumulator(s7)
  c->vadd(DEST::yz, vf14, vf14, vf14);              // vadd.yz vf14, vf14, vf14
  c->lw(s2, 0, sp);                                 // lw s2, 0(sp)
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->vadd(DEST::yz, vf14, vf14, vf14);              // vadd.yz vf14, vf14, vf14
  c->jalr(call_addr);                               // jalr ra, t9
  
block_2:
  c->lw(v1, 0, s0);                                 // lw v1, 0(s0)
  // nop                                            // sll r0, r0, 0
  c->andi(v1, v1, 256);                             // andi v1, v1, 256
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L16
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  c->lw(t2, 8, t7);                                 // lw t2, 8(t7)
  c->addiu(t3, r0, 9600);                           // addiu t3, r0, 9600
  c->lw(t4, 12, t7);                                // lw t4, 12(t7)
  c->addiu(v1, r0, 256);                            // addiu v1, r0, 256
  c->sw(t2, 16, s0);                                // sw t2, 16(s0)
  // nop                                            // sll r0, r0, 0
  c->sw(t3, 128, s0);                               // sw t3, 128(s0)
  // nop                                            // sll r0, r0, 0
  c->sw(t4, 32, s0);                                // sw t4, 32(s0)
  // Unknown instr: sync.l
  c->sw(v1, 0, s0);                                 // sw v1, 0(s0)
  // Unknown instr: sync.l
  c->lw(a2, 16, t7);                                // lw a2, 16(t7)
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->load_symbol2(t9, cache.decompress_fixed_data_to_accumulator);// lw t9, decompress-fixed-data-to-accumulator(s7)
  c->daddiu(a1, a1, 7392);                          // daddiu a1, a1, 7392
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->daddiu(s1, s1, -1);                            // daddiu s1, s1, -1
  c->jalr(call_addr);                               // jalr ra, t9
  
block_4:
  c->lw(v1, 0, s0);                                 // lw v1, 0(s0)
  // nop                                            // sll r0, r0, 0
  c->andi(v1, v1, 256);                             // andi v1, v1, 256
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L17
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_4;}                           // branch non-likely

  bc = c->sgpr64(s1) == 0;                          // beq s1, r0, L18
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  c->lw(t2, 24, t7);                                // lw t2, 24(t7)
  c->addiu(t3, r0, 7392);                           // addiu t3, r0, 7392
  c->lw(t4, 28, t7);                                // lw t4, 28(t7)
  c->addiu(v1, r0, 256);                            // addiu v1, r0, 256
  c->sw(t2, 16, s0);                                // sw t2, 16(s0)
  // nop                                            // sll r0, r0, 0
  c->sw(t3, 128, s0);                               // sw t3, 128(s0)
  // nop                                            // sll r0, r0, 0
  c->sw(t4, 32, s0);                                // sw t4, 32(s0)
  // Unknown instr: sync.l
  c->sw(v1, 0, s0);                                 // sw v1, 0(s0)
  // Unknown instr: sync.l
  
block_7:
  c->lw(t0, 20, t7);                                // lw t0, 20(t7)
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->lw(a2, 16, t7);                                // lw a2, 16(t7)
  c->daddiu(a1, a1, 9600);                          // daddiu a1, a1, 9600
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L19
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_10;}                          // branch non-likely

  c->lw(a3, 12, t7);                                // lw a3, 12(t7)
  // nop                                            // sll r0, r0, 0
  c->load_symbol2(t9, cache.decompress_frame_data_pair_to_accumulator);// lw t9, decompress-frame-data-pair-to-accumulator(s7)
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(a3, a3, 3);                                // sll a3, a3, 3
  c->jalr(call_addr);                               // jalr ra, t9
  bc = c->sgpr64(s1) != 0;                          // bne s1, r0, L16
  c->daddiu(t7, t7, 24);                            // daddiu t7, t7, 24
  if (bc) {goto block_2;}                           // branch non-likely

  c->load_symbol2(t9, cache.normalize_frame_quaternions);// lw t9, normalize-frame-quaternions(s7)
  // nop                                            // sll r0, r0, 0
  c->lw(s2, 0, sp);                                 // lw s2, 0(sp)
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
  // nop                                            // sll r0, r0, 0
  c->lq(s0, 0, sp);                                 // lq s0, 0(sp)
  c->lq(s1, 16, sp);                                // lq s1, 16(sp)
  c->lq(s2, 32, sp);                                // lq s2, 32(sp)
  c->lq(s3, 48, sp);                                // lq s3, 48(sp)
  c->lq(s4, 64, sp);                                // lq s4, 64(sp)
  c->lq(s5, 80, sp);                                // lq s5, 80(sp)
  c->lq(s6, 96, sp);                                // lq s6, 96(sp)
  c->lq(t8, 112, sp);                               // lq t8, 112(sp)
  c->lq(t9, 128, sp);                               // lq t9, 128(sp)
  c->lq(gp, 144, sp);                               // lq gp, 144(sp)
  c->lq(ra, 176, sp);                               // lq ra, 176(sp)
  c->lq(fp, 160, sp);                               // lq fp, 160(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 192);                           // daddiu sp, sp, 192
  goto end_of_function;                             // return

  
block_10:
  c->load_symbol2(t9, cache.decompress_frame_data_to_accumulator);// lw t9, decompress-frame-data-to-accumulator(s7)
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9
  bc = c->sgpr64(s1) != 0;                          // bne s1, r0, L16
  c->daddiu(t7, t7, 24);                            // daddiu t7, t7, 24
  if (bc) {goto block_2;}                           // branch non-likely

  c->load_symbol2(t9, cache.normalize_frame_quaternions);// lw t9, normalize-frame-quaternions(s7)
  // nop                                            // sll r0, r0, 0
  c->lw(s2, 0, sp);                                 // lw s2, 0(sp)
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, t9
  
block_12:
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
  // nop                                            // sll r0, r0, 0
  c->lq(s0, 0, sp);                                 // lq s0, 0(sp)
  c->lq(s1, 16, sp);                                // lq s1, 16(sp)
  c->lq(s2, 32, sp);                                // lq s2, 32(sp)
  c->lq(s3, 48, sp);                                // lq s3, 48(sp)
  c->lq(s4, 64, sp);                                // lq s4, 64(sp)
  c->lq(s5, 80, sp);                                // lq s5, 80(sp)
  c->lq(s6, 96, sp);                                // lq s6, 96(sp)
  c->lq(t8, 112, sp);                               // lq t8, 112(sp)
  c->lq(t9, 128, sp);                               // lq t9, 128(sp)
  c->lq(gp, 144, sp);                               // lq gp, 144(sp)
  c->lq(ra, 176, sp);                               // lq ra, 176(sp)
  c->lq(fp, 160, sp);                               // lq fp, 160(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 192);                           // daddiu sp, sp, 192
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
  cache.clear_frame_accumulator = intern_from_c(-1, 0, "clear-frame-accumulator").c();
  cache.decompress_fixed_data_to_accumulator = intern_from_c(-1, 0, "decompress-fixed-data-to-accumulator").c();
  cache.decompress_frame_data_pair_to_accumulator = intern_from_c(-1, 0, "decompress-frame-data-pair-to-accumulator").c();
  cache.decompress_frame_data_to_accumulator = intern_from_c(-1, 0, "decompress-frame-data-to-accumulator").c();
  cache.normalize_frame_quaternions = intern_from_c(-1, 0, "normalize-frame-quaternions").c();
  gLinkedFunctionTable.reg("calc-animation-from-spr", execute, 512);
}

} // namespace calc_animation_from_spr
} // namespace Mips2C