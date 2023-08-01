
//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace draw_string {
struct Cache {
  void* font_work; // *font-work*
  void* dma_buffer_free; // dma-buffer-free
  void* draw_string_asm; // draw-string-asm
  void* string; // string
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -80);                           // daddiu sp, sp, -80
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  c->sq(s3, 16, sp);                                // sq s3, 16(sp)
  c->sq(s4, 32, sp);                                // sq s4, 32(sp)
  c->sq(s5, 48, sp);                                // sq s5, 48(sp)
  c->sq(gp, 64, sp);                                // sq gp, 64(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->mov64(s5, a1);                                 // or s5, a1, r0
  c->mov64(s4, a2);                                 // or s4, a2, r0
  c->lui(v1, 17152);                                // lui v1, 17152
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lwc1(f1, 76, s4);                              // lwc1 f1, 76(s4)
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->cvtws(f0, f0);                                 // cvt.w.s f0, f0
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->lq(a0, 12, s4);                                // lq a0, 12(s4)
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  //beq r0, r0, L27                                 // beq r0, r0, L27
  // nop                                            // sll r0, r0, 0
  goto block_5;                                     // branch always


  block_1:
  c->addiu(a1, r0, 0);                              // addiu a1, r0, 0
  //beq r0, r0, L26                                 // beq r0, r0, L26
  // nop                                            // sll r0, r0, 0
  goto block_3;                                     // branch always


  block_2:
  c->dsll(a2, a1, 2);                               // dsll a2, a1, 2
  c->dsll(a3, a0, 4);                               // dsll a3, a0, 4
  c->daddu(a2, a2, a3);                             // daddu a2, a2, a3
  c->load_symbol2(a3, cache.font_work);              // lw a3, *font-work*(s7)
  c->daddu(a2, a2, a3);                             // daddu a2, a2, a3
  c->lwu(a2, 2160, a2);                             // lwu a2, 2160(a2)
  // Unknown instr: ld a3, L164(fp)
  c->gprs[a3].du64[0] = 0xffffffff'00ffffff;
  c->and_(a2, a2, a3);                              // and a2, a2, a3
  c->dsll32(a3, v1, 24);                            // dsll32 a3, v1, 24
  c->dsrl32(a3, a3, 0);                             // dsrl32 a3, a3, 0
  c->or_(a2, a2, a3);                               // or a2, a2, a3
  c->dsll(a3, a1, 2);                               // dsll a3, a1, 2
  c->dsll(t0, a0, 4);                               // dsll t0, a0, 4
  c->daddu(a3, a3, t0);                             // daddu a3, a3, t0
  c->load_symbol2(t0, cache.font_work);              // lw t0, *font-work*(s7)
  c->daddu(a3, a3, t0);                             // daddu a3, a3, t0
  c->sw(a2, 2160, a3);                              // sw a2, 2160(a3)
  c->daddiu(a1, a1, 1);                             // daddiu a1, a1, 1

  block_3:
  c->slti(a2, a1, 4);                               // slti a2, a1, 4
  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L25
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  c->mov64(a1, s7);                                 // or a1, s7, r0
  c->mov64(a1, s7);                                 // or a1, s7, r0
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1

  block_5:
  c->slti(a1, a0, 40);                              // slti a1, a0, 40
  bc = c->sgpr64(a1) != 0;                          // bne a1, r0, L24
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(a0, s7);                                 // or a0, s7, r0
  c->mov64(a0, s7);                                 // or a0, s7, r0
  c->load_symbol2(a0, cache.font_work);              // lw a0, *font-work*(s7)
  c->sw(v1, 2156, a0);                              // sw v1, 2156(a0)
  c->load_symbol2(t9, cache.dma_buffer_free);        // lw t9, dma-buffer-free(s7)
  c->mov64(a0, s5);                                 // or a0, s5, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(s3, v0);                                 // or s3, v0, r0
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->load_symbol2(v1, cache.string);                 // lw v1, string(s7)
  c->lwu(t9, 32, v1);                               // lwu t9, 32(v1)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->dsll(v1, v1, 5);                               // dsll v1, v1, 5
  c->sltu(v1, s3, v1);                              // sltu v1, s3, v1
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L28
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_8;}                           // branch non-likely

  c->lq(v0, 12, s4);                                // lq v0, 12(s4)
  c->mov128_gpr_gpr(v1, v0);                        // por v1, v0, r0
  //beq r0, r0, L29                                 // beq r0, r0, L29
  // nop                                            // sll r0, r0, 0
  goto block_9;                                     // branch always


  block_8:
  c->load_symbol2(t9, cache.draw_string_asm);        // lw t9, draw-string-asm(s7)
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->mov64(a1, s5);                                 // or a1, s5, r0
  c->mov64(a2, s4);                                 // or a2, s4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov128_gpr_gpr(v1, v0);                        // por v1, v0, r0

  block_9:
  c->addiu(v1, r0, 0);                              // addiu v1, r0, 0
  //beq r0, r0, L33                                 // beq r0, r0, L33
  // nop                                            // sll r0, r0, 0
  goto block_14;                                    // branch always


  block_10:
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  //beq r0, r0, L32                                 // beq r0, r0, L32
  // nop                                            // sll r0, r0, 0
  goto block_12;                                    // branch always


  block_11:
  c->dsll(a1, a0, 2);                               // dsll a1, a0, 2
  c->dsll(a2, v1, 4);                               // dsll a2, v1, 4
  c->daddu(a1, a1, a2);                             // daddu a1, a1, a2
  c->load_symbol2(a2, cache.font_work);              // lw a2, *font-work*(s7)
  c->daddu(a1, a1, a2);                             // daddu a1, a1, a2
  c->lwu(a1, 2160, a1);                             // lwu a1, 2160(a1)
  // Unknown instr: ld a2, L164(fp)
  c->gprs[a2].du64[0] = 0xffffffff'00ffffff;
  c->and_(a1, a1, a2);                              // and a1, a1, a2
  c->ori(a2, r0, 32768);                            // ori a2, r0, 32768
  c->dsll(a2, a2, 16);                              // dsll a2, a2, 16
  c->or_(a1, a1, a2);                               // or a1, a1, a2
  c->dsll(a2, a0, 2);                               // dsll a2, a0, 2
  c->dsll(a3, v1, 4);                               // dsll a3, v1, 4
  c->daddu(a2, a2, a3);                             // daddu a2, a2, a3
  c->load_symbol2(a3, cache.font_work);              // lw a3, *font-work*(s7)
  c->daddu(a2, a2, a3);                             // daddu a2, a2, a3
  c->sw(a1, 2160, a2);                              // sw a1, 2160(a2)
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1

  block_12:
  c->slti(a1, a0, 4);                               // slti a1, a0, 4
  bc = c->sgpr64(a1) != 0;                          // bne a1, r0, L31
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_11;}                          // branch non-likely

  c->mov64(a0, s7);                                 // or a0, s7, r0
  c->mov64(a0, s7);                                 // or a0, s7, r0
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1

  block_14:
  c->slti(a0, v1, 40);                              // slti a0, v1, 40
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L30
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_10;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->addiu(v1, r0, 128);                            // addiu v1, r0, 128
  c->load_symbol2(a0, cache.font_work);              // lw a0, *font-work*(s7)
  c->sw(v1, 2156, a0);                              // sw v1, 2156(a0)
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
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
  cache.font_work = intern_from_c("*font-work*").c();
  cache.dma_buffer_free = intern_from_c("dma-buffer-free").c();
  cache.draw_string_asm = intern_from_c("draw-string-asm").c();
  cache.string = intern_from_c("string").c();
  gLinkedFunctionTable.reg("draw-string", execute, 512);
}

} // namespace draw_string
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace get_string_length {
struct Cache {
  void* font_work; // *font-work*
  void* font12_table; // *font12-table*
  void* font24_table; // *font24-table*
  void* video_params; // *video-params*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  // u32 call_addr = 0;
  c->lqc2(vf23, 12, a1);                            // lqc2 vf23, 12(a1)
  c->lqc2(vf24, 12, a1);                            // lqc2 vf24, 12(a1)
  c->lw(v1, 64, a1);                                // lw v1, 64(a1)
  c->load_symbol2(a2, cache.font_work);              // lw a2, *font-work*(s7)
  c->mov64(a2, a2);                                 // or a2, a2, r0
  c->sw(a0, 2876, a2);                              // sw a0, 2876(a2)
  c->sw(v1, 2880, a2);                              // sw v1, 2880(a2)
  c->vmove(DEST::xyzw, vf1, vf0);                   // vmove.xyzw vf1, vf0
  // nop                                            // sll r0, r0, 0
  c->andi(a3, v1, 32);                              // andi a3, v1, 32
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L4
  c->load_symbol2(a3, cache.font12_table);           // lw a3, *font12-table*(s7)
  if (bc) {goto block_2;}                           // branch non-likely

  c->mov64(a3, a3);                                 // or a3, a3, r0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf13, 208, a2);                           // lqc2 vf13, 208(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 224, a2);                           // lqc2 vf14, 224(a2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 352, a2);                           // sqc2 vf14, 352(a2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 368, a2);                           // sqc2 vf14, 368(a2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 384, a2);                           // sqc2 vf14, 384(a2)
  //beq r0, r0, L5                                  // beq r0, r0, L5
  c->sqc2(vf14, 400, a2);                           // sqc2 vf14, 400(a2)
  goto block_3;                                     // branch always


  block_2:
  // nop                                            // sll r0, r0, 0
  c->load_symbol2(a3, cache.font24_table);           // lw a3, *font24-table*(s7)
  c->mov64(a3, a3);                                 // or a3, a3, r0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf13, 256, a2);                           // lqc2 vf13, 256(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 272, a2);                           // lqc2 vf14, 272(a2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 352, a2);                           // sqc2 vf14, 352(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 288, a2);                           // lqc2 vf14, 288(a2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 368, a2);                           // sqc2 vf14, 368(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 304, a2);                           // lqc2 vf14, 304(a2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 384, a2);                           // sqc2 vf14, 384(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 320, a2);                           // lqc2 vf14, 320(a2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 400, a2);                           // sqc2 vf14, 400(a2)

  block_3:
  c->lbu(t0, 4, a0);                                // lbu t0, 4(a0)
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L22
  c->daddiu(t1, t0, -3);                            // daddiu t1, t0, -3
  if (bc) {goto block_60;}                          // branch non-likely

  bc = ((s64)c->sgpr64(t1)) <= 0;                   // blez t1, L15
  c->daddiu(t1, t0, -126);                          // daddiu t1, t0, -126
  if (bc) {goto block_48;}                          // branch non-likely

  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L18
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_53;}                          // branch non-likely

  c->lbu(t0, 4, a0);                                // lbu t0, 4(a0)
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  c->addiu(t1, r0, 0);                              // addiu t1, r0, 0
  c->addiu(t2, r0, 0);                              // addiu t2, r0, 0
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L22
  c->daddiu(t3, t0, -43);                           // daddiu t3, t0, -43
  if (bc) {goto block_60;}                          // branch non-likely

  c->movz(t1, t0, t3);                              // movz t1, t0, t3
  c->daddiu(t3, t0, -45);                           // daddiu t3, t0, -45
  c->movz(t1, t0, t3);                              // movz t1, t0, t3
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L6
  c->daddiu(t3, t0, -121);                          // daddiu t3, t0, -121
  if (bc) {goto block_15;}                          // branch non-likely

  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L13
  c->daddiu(t2, t0, -89);                           // daddiu t2, t0, -89
  if (bc) {goto block_46;}                          // branch non-likely

  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L13
  c->daddiu(t2, t0, -122);                          // daddiu t2, t0, -122
  if (bc) {goto block_46;}                          // branch non-likely

  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L14
  c->daddiu(t2, t0, -90);                           // daddiu t2, t0, -90
  if (bc) {goto block_47;}                          // branch non-likely

  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L14
  c->daddiu(t2, t0, -48);                           // daddiu t2, t0, -48
  if (bc) {goto block_47;}                          // branch non-likely

  bc = ((s64)c->sgpr64(t2)) < 0;                    // bltz t2, L18
  c->daddiu(t2, t0, -57);                           // daddiu t2, t0, -57
  if (bc) {goto block_53;}                          // branch non-likely

  bc = ((s64)c->sgpr64(t2)) > 0;                    // bgtz t2, L18
  c->daddiu(t2, t0, -126);                          // daddiu t2, t0, -126
  if (bc) {goto block_53;}                          // branch non-likely

  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L18
  c->daddiu(t2, t0, -48);                           // daddiu t2, t0, -48
  if (bc) {goto block_53;}                          // branch non-likely


  block_15:
  c->lbu(t0, 4, a0);                                // lbu t0, 4(a0)
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L22
  c->daddiu(t3, t0, -110);                          // daddiu t3, t0, -110
  if (bc) {goto block_60;}                          // branch non-likely

  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L7
  c->daddiu(t3, t0, -78);                           // daddiu t3, t0, -78
  if (bc) {goto block_36;}                          // branch non-likely

  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L7
  c->daddiu(t3, t0, -108);                          // daddiu t3, t0, -108
  if (bc) {goto block_36;}                          // branch non-likely

  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L5
  c->daddiu(t3, t0, -76);                           // daddiu t3, t0, -76
  if (bc) {goto block_3;}                           // branch non-likely

  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L5
  c->daddiu(t3, t0, -119);                          // daddiu t3, t0, -119
  if (bc) {goto block_3;}                           // branch non-likely

  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L5
  c->daddiu(t3, t0, -87);                           // daddiu t3, t0, -87
  if (bc) {goto block_3;}                           // branch non-likely

  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L5
  c->daddiu(t3, t0, -107);                          // daddiu t3, t0, -107
  if (bc) {goto block_3;}                           // branch non-likely

  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L9
  c->daddiu(t3, t0, -75);                           // daddiu t3, t0, -75
  if (bc) {goto block_39;}                          // branch non-likely

  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L9
  c->daddiu(t3, t0, -106);                          // daddiu t3, t0, -106
  if (bc) {goto block_39;}                          // branch non-likely

  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L5
  c->daddiu(t3, t0, -74);                           // daddiu t3, t0, -74
  if (bc) {goto block_3;}                           // branch non-likely

  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L5
  c->daddiu(t3, t0, -104);                          // daddiu t3, t0, -104
  if (bc) {goto block_3;}                           // branch non-likely

  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L10
  c->daddiu(t3, t0, -72);                           // daddiu t3, t0, -72
  if (bc) {goto block_41;}                          // branch non-likely

  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L10
  c->daddiu(t3, t0, -118);                          // daddiu t3, t0, -118
  if (bc) {goto block_41;}                          // branch non-likely

  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L5
  c->daddiu(t3, t0, -86);                           // daddiu t3, t0, -86
  if (bc) {goto block_3;}                           // branch non-likely

  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L5
  c->daddiu(t3, t0, -117);                          // daddiu t3, t0, -117
  if (bc) {goto block_3;}                           // branch non-likely

  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L5
  c->daddiu(t3, t0, -85);                           // daddiu t3, t0, -85
  if (bc) {goto block_3;}                           // branch non-likely

  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L5
  c->daddiu(t3, t0, -48);                           // daddiu t3, t0, -48
  if (bc) {goto block_3;}                           // branch non-likely

  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L5
  c->daddiu(t3, t0, -48);                           // daddiu t3, t0, -48
  if (bc) {goto block_3;}                           // branch non-likely

  bc = ((s64)c->sgpr64(t3)) < 0;                    // bltz t3, L18
  c->daddiu(t4, t0, -57);                           // daddiu t4, t0, -57
  if (bc) {goto block_53;}                          // branch non-likely

  bc = ((s64)c->sgpr64(t4)) > 0;                    // bgtz t4, L18
  c->sll(t4, t2, 2);                                // sll t4, t2, 2
  if (bc) {goto block_53;}                          // branch non-likely

  c->daddu(t0, t2, t4);                             // daddu t0, t2, t4
  // nop                                            // sll r0, r0, 0
  c->sll(t0, t0, 1);                                // sll t0, t0, 1
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L6                                  // beq r0, r0, L6
  c->daddu(t2, t0, t3);                             // daddu t2, t0, t3
  goto block_15;                                    // branch always


  block_36:
  bc = c->sgpr64(t2) != 0;                          // bne t2, r0, L8
  c->load_symbol2(a3, cache.font12_table);           // lw a3, *font12-table*(s7)
  if (bc) {goto block_38;}                          // branch non-likely

  c->mov64(a3, a3);                                 // or a3, a3, r0
  c->addiu(t0, r0, -33);                            // addiu t0, r0, -33
  c->lqc2(vf13, 208, a2);                           // lqc2 vf13, 208(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 224, a2);                           // lqc2 vf14, 224(a2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 352, a2);                           // sqc2 vf14, 352(a2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 368, a2);                           // sqc2 vf14, 368(a2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 384, a2);                           // sqc2 vf14, 384(a2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 400, a2);                           // sqc2 vf14, 400(a2)
  //beq r0, r0, L5                                  // beq r0, r0, L5
  c->and_(v1, v1, t0);                              // and v1, v1, t0
  goto block_3;                                     // branch always


  block_38:
  // nop                                            // sll r0, r0, 0
  c->load_symbol2(a3, cache.font24_table);           // lw a3, *font24-table*(s7)
  c->mov64(a3, a3);                                 // or a3, a3, r0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf13, 256, a2);                           // lqc2 vf13, 256(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 272, a2);                           // lqc2 vf14, 272(a2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 352, a2);                           // sqc2 vf14, 352(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 288, a2);                           // lqc2 vf14, 288(a2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 368, a2);                           // sqc2 vf14, 368(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 304, a2);                           // lqc2 vf14, 304(a2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 384, a2);                           // sqc2 vf14, 384(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 320, a2);                           // lqc2 vf14, 320(a2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 400, a2);                           // sqc2 vf14, 400(a2)
  //beq r0, r0, L5                                  // beq r0, r0, L5
  c->ori(v1, v1, 32);                               // ori v1, v1, 32
  goto block_3;                                     // branch always


  block_39:
  c->addiu(t0, r0, -3);                             // addiu t0, r0, -3
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L5
  c->and_(v1, v1, t0);                              // and v1, v1, t0
  if (bc) {goto block_3;}                           // branch non-likely

  //beq r0, r0, L5                                  // beq r0, r0, L5
  c->ori(v1, v1, 2);                                // ori v1, v1, 2
  goto block_3;                                     // branch always


  block_41:
  c->mov128_vf_gpr(vf1, t2);                        // qmtc2.i vf1, t2
  c->daddiu(t0, t1, -45);                           // daddiu t0, t1, -45
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L12
  c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
  if (bc) {goto block_45;}                          // branch non-likely

  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L11
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_44;}                          // branch non-likely

  //beq r0, r0, L5                                  // beq r0, r0, L5
  c->vadd_bc(DEST::x, BC::x, vf23, vf23, vf1);      // vaddx.x vf23, vf23, vf1
  goto block_3;                                     // branch always


  block_44:
  //beq r0, r0, L5                                  // beq r0, r0, L5
  c->vsub_bc(DEST::x, BC::x, vf23, vf23, vf1);      // vsubx.x vf23, vf23, vf1
  goto block_3;                                     // branch always


  block_45:
  //beq r0, r0, L5                                  // beq r0, r0, L5
  c->vadd_bc(DEST::x, BC::x, vf23, vf0, vf1);       // vaddx.x vf23, vf0, vf1
  goto block_3;                                     // branch always


  block_46:
  //beq r0, r0, L5                                  // beq r0, r0, L5
  c->sqc2(vf23, 464, a2);                           // sqc2 vf23, 464(a2)
  goto block_3;                                     // branch always


  block_47:
  //beq r0, r0, L5                                  // beq r0, r0, L5
  c->lqc2(vf23, 464, a2);                           // lqc2 vf23, 464(a2)
  goto block_3;                                     // branch always


  block_48:
  c->daddiu(t1, t0, -3);                            // daddiu t1, t0, -3
  c->ori(v1, v1, 64);                               // ori v1, v1, 64
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L16
  c->daddiu(t0, t0, -2);                            // daddiu t0, t0, -2
  if (bc) {goto block_51;}                          // branch non-likely

  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L17
  c->lqc2(vf14, 384, a2);                           // lqc2 vf14, 384(a2)
  if (bc) {goto block_52;}                          // branch non-likely

  //beq r0, r0, L17                                 // beq r0, r0, L17
  c->lqc2(vf14, 368, a2);                           // lqc2 vf14, 368(a2)
  goto block_52;                                    // branch always


  block_51:
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 400, a2);                           // lqc2 vf14, 400(a2)

  block_52:
  c->lbu(t0, 4, a0);                                // lbu t0, 4(a0)
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  //beq r0, r0, L20                                 // beq r0, r0, L20
  c->sll(t1, t0, 4);                                // sll t1, t0, 4
  goto block_56;                                    // branch always


  block_53:
  // nop                                            // sll r0, r0, 0
  c->addiu(t1, r0, -65);                            // addiu t1, r0, -65
  c->and_(v1, v1, t1);                              // and v1, v1, t1
  c->lqc2(vf14, 352, a2);                           // lqc2 vf14, 352(a2)
  // nop                                            // sll r0, r0, 0
  c->sll(t1, t0, 4);                                // sll t1, t0, 4
  // nop                                            // sll r0, r0, 0
  c->daddiu(t2, t0, -10);                           // daddiu t2, t0, -10
  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L19
  c->daddiu(t0, t0, -13);                           // daddiu t0, t0, -13
  if (bc) {goto block_55;}                          // branch non-likely

  bc = c->sgpr64(t0) != 0;                          // bne t0, r0, L20
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_56;}                          // branch non-likely


  block_55:
  //beq r0, r0, L5                                  // beq r0, r0, L5
  c->vadd_bc(DEST::x, BC::x, vf23, vf0, vf24);      // vaddx.x vf23, vf0, vf24
  goto block_3;                                     // branch always


  block_56:
  c->addu(t0, t1, a3);                              // addu t0, t1, a3
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf5, -96, t0);                            // lqc2 vf5, -96(t0)
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf19, vf5, vf13);             // vmul.xyzw vf19, vf5, vf13
  c->andi(t0, v1, 2);                               // andi t0, v1, 2
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L21
  c->andi(t0, v1, 64);                              // andi t0, v1, 64
  if (bc) {goto block_59;}                          // branch non-likely

  bc = c->sgpr64(t0) != 0;                          // bne t0, r0, L21
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_59;}                          // branch non-likely

  //beq r0, r0, L5                                  // beq r0, r0, L5
  c->vadd_bc(DEST::x, BC::w, vf23, vf23, vf19);     // vaddw.x vf23, vf23, vf19
  goto block_3;                                     // branch always


  block_59:
  //beq r0, r0, L5                                  // beq r0, r0, L5
  c->vadd_bc(DEST::x, BC::w, vf23, vf23, vf14);     // vaddw.x vf23, vf23, vf14
  goto block_3;                                     // branch always


  block_60:
  c->vsub(DEST::xyzw, vf23, vf23, vf24);            // vsub.xyzw vf23, vf23, vf24
  c->load_symbol2(v1, cache.video_params);           // lw v1, *video-params*(s7)
  c->mov64(v1, v1);                                 // or v1, v1, r0
  c->lqc2(vf1, 16, v1);                             // lqc2 vf1, 16(v1)
  c->vmul(DEST::x, vf23, vf23, vf1);                // vmul.x vf23, vf23, vf1
  c->lqc2(vf1, 44, a1);                             // lqc2 vf1, 44(a1)
  c->vmul_bc(DEST::x, BC::w, vf23, vf23, vf1);      // vmulw.x vf23, vf23, vf1
  c->mov128_gpr_vf(v0, vf23);                       // qmfc2.i v0, vf23
  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.font_work = intern_from_c("*font-work*").c();
  cache.font12_table = intern_from_c("*font12-table*").c();
  cache.font24_table = intern_from_c("*font24-table*").c();
  cache.video_params = intern_from_c("*video-params*").c();
  gLinkedFunctionTable.reg("get-string-length", execute, 512);
}

} // namespace get_string_length
} // namespace Mips2C


//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace draw_string_asm {
struct Cache {
  void* font_work; // *font-work*
  void* font12_table; // *font12-table*
  void* font24_table; // *font24-table*
  void* math_camera; // *math-camera*
  void* video_params; // *video-params*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  // u32 call_addr = 0;
  c->load_symbol2(v1, cache.math_camera);            // lw v1, *math-camera*(s7)
  c->lqc2(vf26, 812, v1);                           // lqc2 vf26, 812(v1)
  c->lqc2(vf27, 812, v1);                           // lqc2 vf27, 812(v1)
  // shadow hack begin
  c->load_symbol2(v1, cache.video_params);
  c->lw(t0, 16, v1);
  c->vfs[vf26].f[0] += c->gprs[t0].f[0] * 2;
  c->vfs[vf26].f[1] += 1;
  // c->vadd_bc(DEST::xy, BC::w, vf26, vf26, vf0);     // vaddw.xy vf26, vf26, vf0
  // c->vadd_bc(DEST::x, BC::w, vf26, vf26, vf0);      // vaddw.x vf26, vf26, vf0
  // shadow hack end
  c->lw(v1, 68, a2);                                // lw v1, 68(a2)
  c->lqc2(vf25, 44, a2);                            // lqc2 vf25, 44(a2)
  c->lqc2(vf23, 12, a2);                            // lqc2 vf23, 12(a2)
  c->lqc2(vf24, 12, a2);                            // lqc2 vf24, 12(a2)
  c->lqc2(vf28, 0, v1);                             // lqc2 vf28, 0(v1)
  c->lqc2(vf29, 16, v1);                            // lqc2 vf29, 16(v1)
  c->lqc2(vf30, 32, v1);                            // lqc2 vf30, 32(v1)
  c->lqc2(vf31, 48, v1);                            // lqc2 vf31, 48(v1)
  c->load_symbol2(v1, cache.video_params);           // lw v1, *video-params*(s7)
  c->mov64(v1, v1);                                 // or v1, v1, r0
  c->lqc2(vf1, 32, v1);                             // lqc2 vf1, 32(v1)
  c->vdiv(vf0, BC::w, vf25, BC::w);                 // vdiv Q, vf0.w, vf25.w
  c->lqc2(vf2, 16, v1);                             // lqc2 vf2, 16(v1)
  c->vmul(DEST::x, vf25, vf25, vf1);                // vmul.x vf25, vf25, vf1
  c->vmul(DEST::x, vf23, vf23, vf1);                // vmul.x vf23, vf23, vf1
  c->vmul(DEST::x, vf24, vf24, vf1);                // vmul.x vf24, vf24, vf1
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xy, vf25, vf25);                   // vmulq.xy vf25, vf25, Q
  c->vmulq(DEST::xy, vf23, vf23);                   // vmulq.xy vf23, vf23, Q
  c->vmulq(DEST::xy, vf24, vf24);                   // vmulq.xy vf24, vf24, Q
  c->vadd(DEST::xy, vf25, vf25, vf24);              // vadd.xy vf25, vf25, vf24
  c->vmul_bc(DEST::x, BC::w, vf28, vf28, vf25);     // vmulw.x vf28, vf28, vf25
  c->vmul_bc(DEST::y, BC::w, vf29, vf29, vf25);     // vmulw.y vf29, vf29, vf25
  c->vmul(DEST::x, vf28, vf28, vf2);                // vmul.x vf28, vf28, vf2
  c->load_symbol2(v1, cache.font_work);              // lw v1, *font-work*(s7)
  c->mov64(v1, v1);                                 // or v1, v1, r0
  c->sw(a1, 2872, v1);                              // sw a1, 2872(v1)
  c->lw(a1, 4, a1);                                 // lw a1, 4(a1)
  c->sw(a0, 2876, v1);                              // sw a0, 2876(v1)
  c->lw(t0, 64, a2);                                // lw t0, 64(a2)
  c->vmove(DEST::xyzw, vf1, vf0);                   // vmove.xyzw vf1, vf0
  c->vmove(DEST::xyzw, vf2, vf0);                   // vmove.xyzw vf2, vf0
  c->vmove(DEST::xyzw, vf3, vf0);                   // vmove.xyzw vf3, vf0
  c->vmove(DEST::xyzw, vf4, vf0);                   // vmove.xyzw vf4, vf0
  c->sw(t0, 2880, v1);                              // sw t0, 2880(v1)
  c->lqc2(vf16, 416, v1);                           // lqc2 vf16, 416(v1)
  c->lqc2(vf17, 432, v1);                           // lqc2 vf17, 432(v1)
  c->lqc2(vf18, 448, v1);                           // lqc2 vf18, 448(v1)
  c->load_symbol2(a3, cache.video_params);           // lw a3, *video-params*(s7)
  c->mov64(a3, a3);                                 // or a3, a3, r0
  c->lqc2(vf1, 16, a3);                             // lqc2 vf1, 16(a3)
  c->andi(a3, t0, 32);                              // andi a3, t0, 32
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L35
  c->load_symbol2(a3, cache.font12_table);           // lw a3, *font12-table*(s7)
  if (bc) {goto block_2;}                           // branch non-likely

  c->mov64(a3, a3);                                 // or a3, a3, r0
  // nop                                            // sll r0, r0, 0
  c->lq(t1, 80, v1);                                // lq t1, 80(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 96, v1);                                // lq t2, 96(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 112, v1);                               // lq t3, 112(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t4, 128, v1);                               // lq t4, 128(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t1, 2800, v1);                              // sq t1, 2800(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 2816, v1);                              // sq t2, 2816(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t3, 2832, v1);                              // sq t3, 2832(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t4, 2848, v1);                              // sq t4, 2848(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf13, 208, v1);                           // lqc2 vf13, 208(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 224, v1);                           // lqc2 vf14, 224(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf15, 240, v1);                           // lqc2 vf15, 240(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 352, v1);                           // sqc2 vf14, 352(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 368, v1);                           // sqc2 vf14, 368(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 384, v1);                           // sqc2 vf14, 384(v1)
  //beq r0, r0, L36                                 // beq r0, r0, L36
  c->sqc2(vf14, 400, v1);                           // sqc2 vf14, 400(v1)
  goto block_3;                                     // branch always


  block_2:
  // nop                                            // sll r0, r0, 0
  c->load_symbol2(a3, cache.font24_table);           // lw a3, *font24-table*(s7)
  c->mov64(a3, a3);                                 // or a3, a3, r0
  // nop                                            // sll r0, r0, 0
  c->lq(t1, 144, v1);                               // lq t1, 144(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 160, v1);                               // lq t2, 160(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 176, v1);                               // lq t3, 176(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t4, 192, v1);                               // lq t4, 192(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t1, 2800, v1);                              // sq t1, 2800(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 2816, v1);                              // sq t2, 2816(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t3, 2832, v1);                              // sq t3, 2832(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t4, 2848, v1);                              // sq t4, 2848(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf13, 256, v1);                           // lqc2 vf13, 256(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf15, 336, v1);                           // lqc2 vf15, 336(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 272, v1);                           // lqc2 vf14, 272(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 352, v1);                           // sqc2 vf14, 352(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 288, v1);                           // lqc2 vf14, 288(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 368, v1);                           // sqc2 vf14, 368(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 304, v1);                           // lqc2 vf14, 304(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 384, v1);                           // sqc2 vf14, 384(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 320, v1);                           // lqc2 vf14, 320(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 400, v1);                           // sqc2 vf14, 400(v1)
  // nop                                            // sll r0, r0, 0

  block_3:
  c->lw(t1, 60, a2);                                // lw t1, 60(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(t1, 2864, v1);                              // sw t1, 2864(v1)
  // nop                                            // sll r0, r0, 0
  c->sll(t1, t1, 4);                                // sll t1, t1, 4
  // nop                                            // sll r0, r0, 0
  c->daddu(t2, t1, v1);                             // daddu t2, t1, v1
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lwu(t1, 2160, t2);                             // lwu t1, 2160(t2)
  // nop                                            // sll r0, r0, 0
  c->lwu(t3, 2164, t2);                             // lwu t3, 2164(t2)
  c->pextlb(t4, r0, t1);                            // pextlb t4, r0, t1
  c->lwu(t1, 2168, t2);                             // lwu t1, 2168(t2)
  c->pextlh(t4, r0, t4);                            // pextlh t4, r0, t4
  c->lwu(t2, 2172, t2);                             // lwu t2, 2172(t2)
  c->pextlb(t3, r0, t3);                            // pextlb t3, r0, t3
  c->sq(t4, 608, v1);                               // sq t4, 608(v1)
  c->pextlh(t3, r0, t3);                            // pextlh t3, r0, t3
  c->sq(t4, 992, v1);                               // sq t4, 992(v1)
  c->pextlb(t1, r0, t1);                            // pextlb t1, r0, t1
  c->sq(t3, 624, v1);                               // sq t3, 624(v1)
  c->pextlh(t1, r0, t1);                            // pextlh t1, r0, t1
  c->sq(t3, 1008, v1);                              // sq t3, 1008(v1)
  c->pextlb(t2, r0, t2);                            // pextlb t2, r0, t2
  c->sq(t1, 640, v1);                               // sq t1, 640(v1)
  c->pextlh(t2, r0, t2);                            // pextlh t2, r0, t2
  c->sq(t1, 1024, v1);                              // sq t1, 1024(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 656, v1);                               // sq t2, 656(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 1040, v1);                              // sq t2, 1040(v1)
  c->mov64(t1, v1);                                 // or t1, v1, r0
  // nop                                            // sll r0, r0, 0

  block_4:
  c->lbu(t2, 4, a0);                                // lbu t2, 4(a0)
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  c->lqc2(vf20, 2800, v1);                          // lqc2 vf20, 2800(v1)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L63
  c->daddiu(t3, t2, -3);                            // daddiu t3, t2, -3
  if (bc) {goto block_78;}                          // branch non-likely

  bc = ((s64)c->sgpr64(t3)) <= 0;                   // blez t3, L52
  c->daddiu(t3, t2, -126);                          // daddiu t3, t2, -126
  if (bc) {goto block_59;}                          // branch non-likely

  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L56
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_65;}                          // branch non-likely

  c->lbu(t2, 4, a0);                                // lbu t2, 4(a0)
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  c->addiu(t3, r0, 0);                              // addiu t3, r0, 0
  c->addiu(t4, r0, 0);                              // addiu t4, r0, 0
  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L63
  c->daddiu(t5, t2, -43);                           // daddiu t5, t2, -43
  if (bc) {goto block_78;}                          // branch non-likely

  c->movz(t3, t2, t5);                              // movz t3, t2, t5
  c->daddiu(t5, t2, -45);                           // daddiu t5, t2, -45
  c->movz(t3, t2, t5);                              // movz t3, t2, t5
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L38
  c->daddiu(t5, t2, -91);                           // daddiu t5, t2, -91
  if (bc) {goto block_18;}                          // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L37
  c->daddiu(t4, t2, -93);                           // daddiu t4, t2, -93
  if (bc) {goto block_4;}                           // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L37
  c->daddiu(t4, t2, -121);                          // daddiu t4, t2, -121
  if (bc) {goto block_4;}                           // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L50
  c->daddiu(t4, t2, -89);                           // daddiu t4, t2, -89
  if (bc) {goto block_57;}                          // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L50
  c->daddiu(t4, t2, -122);                          // daddiu t4, t2, -122
  if (bc) {goto block_57;}                          // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L51
  c->daddiu(t4, t2, -90);                           // daddiu t4, t2, -90
  if (bc) {goto block_58;}                          // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L51
  c->daddiu(t4, t2, -48);                           // daddiu t4, t2, -48
  if (bc) {goto block_58;}                          // branch non-likely

  bc = ((s64)c->sgpr64(t4)) < 0;                    // bltz t4, L56
  c->daddiu(t4, t2, -57);                           // daddiu t4, t2, -57
  if (bc) {goto block_65;}                          // branch non-likely

  bc = ((s64)c->sgpr64(t4)) > 0;                    // bgtz t4, L56
  c->daddiu(t4, t2, -126);                          // daddiu t4, t2, -126
  if (bc) {goto block_65;}                          // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L56
  c->daddiu(t4, t2, -48);                           // daddiu t4, t2, -48
  if (bc) {goto block_65;}                          // branch non-likely


  block_18:
  c->lbu(t2, 4, a0);                                // lbu t2, 4(a0)
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L63
  c->daddiu(t5, t2, -110);                          // daddiu t5, t2, -110
  if (bc) {goto block_78;}                          // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L39
  c->daddiu(t5, t2, -78);                           // daddiu t5, t2, -78
  if (bc) {goto block_38;}                          // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L39
  c->daddiu(t5, t2, -108);                          // daddiu t5, t2, -108
  if (bc) {goto block_38;}                          // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L37
  c->daddiu(t5, t2, -76);                           // daddiu t5, t2, -76
  if (bc) {goto block_4;}                           // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L37
  c->daddiu(t5, t2, -119);                          // daddiu t5, t2, -119
  if (bc) {goto block_4;}                           // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L37
  c->daddiu(t5, t2, -87);                           // daddiu t5, t2, -87
  if (bc) {goto block_4;}                           // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L37
  c->daddiu(t5, t2, -107);                          // daddiu t5, t2, -107
  if (bc) {goto block_4;}                           // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L41
  c->daddiu(t5, t2, -75);                           // daddiu t5, t2, -75
  if (bc) {goto block_41;}                          // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L41
  c->daddiu(t5, t2, -106);                          // daddiu t5, t2, -106
  if (bc) {goto block_41;}                          // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L42
  c->daddiu(t5, t2, -74);                           // daddiu t5, t2, -74
  if (bc) {goto block_43;}                          // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L42
  c->daddiu(t5, t2, -104);                          // daddiu t5, t2, -104
  if (bc) {goto block_43;}                          // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L44
  c->daddiu(t5, t2, -72);                           // daddiu t5, t2, -72
  if (bc) {goto block_47;}                          // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L44
  c->daddiu(t5, t2, -118);                          // daddiu t5, t2, -118
  if (bc) {goto block_47;}                          // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L47
  c->daddiu(t5, t2, -86);                           // daddiu t5, t2, -86
  if (bc) {goto block_52;}                          // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L47
  c->daddiu(t5, t2, -117);                          // daddiu t5, t2, -117
  if (bc) {goto block_52;}                          // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L37
  c->daddiu(t5, t2, -85);                           // daddiu t5, t2, -85
  if (bc) {goto block_4;}                           // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L37
  c->daddiu(t5, t2, -48);                           // daddiu t5, t2, -48
  if (bc) {goto block_4;}                           // branch non-likely

  bc = ((s64)c->sgpr64(t5)) < 0;                    // bltz t5, L56
  c->daddiu(t6, t2, -57);                           // daddiu t6, t2, -57
  if (bc) {goto block_65;}                          // branch non-likely

  bc = ((s64)c->sgpr64(t6)) > 0;                    // bgtz t6, L56
  c->sll(t6, t4, 2);                                // sll t6, t4, 2
  if (bc) {goto block_65;}                          // branch non-likely

  c->daddu(t2, t4, t6);                             // daddu t2, t4, t6
  // nop                                            // sll r0, r0, 0
  c->sll(t2, t2, 1);                                // sll t2, t2, 1
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L38                                 // beq r0, r0, L38
  c->daddu(t4, t2, t5);                             // daddu t4, t2, t5
  goto block_18;                                    // branch always


  block_38:
  bc = c->sgpr64(t4) != 0;                          // bne t4, r0, L40
  c->addiu(t2, r0, -33);                            // addiu t2, r0, -33
  if (bc) {goto block_40;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lq(a3, 80, v1);                                // lq a3, 80(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 96, v1);                                // lq t3, 96(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t4, 112, v1);                               // lq t4, 112(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t5, 128, v1);                               // lq t5, 128(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 2800, v1);                              // sq a3, 2800(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t3, 2816, v1);                              // sq t3, 2816(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t4, 2832, v1);                              // sq t4, 2832(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t5, 2848, v1);                              // sq t5, 2848(v1)
  // nop                                            // sll r0, r0, 0
  c->load_symbol2(a3, cache.font12_table);           // lw a3, *font12-table*(s7)
  c->mov64(a3, a3);                                 // or a3, a3, r0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf13, 208, v1);                           // lqc2 vf13, 208(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 224, v1);                           // lqc2 vf14, 224(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf15, 240, v1);                           // lqc2 vf15, 240(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 352, v1);                           // sqc2 vf14, 352(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 368, v1);                           // sqc2 vf14, 368(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 384, v1);                           // sqc2 vf14, 384(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 400, v1);                           // sqc2 vf14, 400(v1)
  //beq r0, r0, L37                                 // beq r0, r0, L37
  c->and_(t0, t0, t2);                              // and t0, t0, t2
  goto block_4;                                     // branch always


  block_40:
  // nop                                            // sll r0, r0, 0
  c->lq(a3, 144, v1);                               // lq a3, 144(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 160, v1);                               // lq t2, 160(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 176, v1);                               // lq t3, 176(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t4, 192, v1);                               // lq t4, 192(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 2800, v1);                              // sq a3, 2800(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 2816, v1);                              // sq t2, 2816(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t3, 2832, v1);                              // sq t3, 2832(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t4, 2848, v1);                              // sq t4, 2848(v1)
  // nop                                            // sll r0, r0, 0
  c->load_symbol2(a3, cache.font24_table);           // lw a3, *font24-table*(s7)
  c->mov64(a3, a3);                                 // or a3, a3, r0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf13, 256, v1);                           // lqc2 vf13, 256(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf15, 336, v1);                           // lqc2 vf15, 336(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 272, v1);                           // lqc2 vf14, 272(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 352, v1);                           // sqc2 vf14, 352(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 288, v1);                           // lqc2 vf14, 288(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 368, v1);                           // sqc2 vf14, 368(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 304, v1);                           // lqc2 vf14, 304(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 384, v1);                           // sqc2 vf14, 384(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 320, v1);                           // lqc2 vf14, 320(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 400, v1);                           // sqc2 vf14, 400(v1)
  //beq r0, r0, L37                                 // beq r0, r0, L37
  c->ori(t0, t0, 32);                               // ori t0, t0, 32
  goto block_4;                                     // branch always


  block_41:
  c->addiu(t2, r0, -3);                             // addiu t2, r0, -3
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L37
  c->and_(t0, t0, t2);                              // and t0, t0, t2
  if (bc) {goto block_4;}                           // branch non-likely

  //beq r0, r0, L37                                 // beq r0, r0, L37
  c->ori(t0, t0, 2);                                // ori t0, t0, 2
  goto block_4;                                     // branch always


  block_43:
  c->addiu(t2, r0, -21);                            // addiu t2, r0, -21
  c->daddiu(t3, t4, -2);                            // daddiu t3, t4, -2
  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L37
  c->and_(t0, t0, t2);                              // and t0, t0, t2
  if (bc) {goto block_4;}                           // branch non-likely

  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L43
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_46;}                          // branch non-likely

  //beq r0, r0, L37                                 // beq r0, r0, L37
  c->ori(t0, t0, 16);                               // ori t0, t0, 16
  goto block_4;                                     // branch always


  block_46:
  //beq r0, r0, L37                                 // beq r0, r0, L37
  c->ori(t0, t0, 4);                                // ori t0, t0, 4
  goto block_4;                                     // branch always


  block_47:
  c->mov128_vf_gpr(vf1, t4);                        // qmtc2.i vf1, t4
  c->daddiu(t2, t3, -45);                           // daddiu t2, t3, -45
  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L46
  c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
  if (bc) {goto block_51;}                          // branch non-likely

  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L45
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_50;}                          // branch non-likely

  //beq r0, r0, L37                                 // beq r0, r0, L37
  c->vadd_bc(DEST::x, BC::x, vf23, vf23, vf1);      // vaddx.x vf23, vf23, vf1
  goto block_4;                                     // branch always


  block_50:
  //beq r0, r0, L37                                 // beq r0, r0, L37
  c->vsub_bc(DEST::x, BC::x, vf23, vf23, vf1);      // vsubx.x vf23, vf23, vf1
  goto block_4;                                     // branch always


  block_51:
  //beq r0, r0, L37                                 // beq r0, r0, L37
  c->vadd_bc(DEST::x, BC::x, vf23, vf0, vf1);       // vaddx.x vf23, vf0, vf1
  goto block_4;                                     // branch always


  block_52:
  c->mov128_vf_gpr(vf1, t4);                        // qmtc2.i vf1, t4
  c->daddiu(t2, t3, -45);                           // daddiu t2, t3, -45
  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L49
  c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
  if (bc) {goto block_56;}                          // branch non-likely

  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L48
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_55;}                          // branch non-likely

  //beq r0, r0, L37                                 // beq r0, r0, L37
  c->vadd_bc(DEST::y, BC::x, vf23, vf23, vf1);      // vaddx.y vf23, vf23, vf1
  goto block_4;                                     // branch always


  block_55:
  //beq r0, r0, L37                                 // beq r0, r0, L37
  c->vsub_bc(DEST::y, BC::x, vf23, vf23, vf1);      // vsubx.y vf23, vf23, vf1
  goto block_4;                                     // branch always


  block_56:
  //beq r0, r0, L37                                 // beq r0, r0, L37
  c->vadd_bc(DEST::y, BC::x, vf23, vf0, vf1);       // vaddx.y vf23, vf0, vf1
  goto block_4;                                     // branch always


  block_57:
  //beq r0, r0, L37                                 // beq r0, r0, L37
  c->sqc2(vf23, 464, v1);                           // sqc2 vf23, 464(v1)
  goto block_4;                                     // branch always


  block_58:
  //beq r0, r0, L37                                 // beq r0, r0, L37
  c->lqc2(vf23, 464, v1);                           // lqc2 vf23, 464(v1)
  goto block_4;                                     // branch always


  block_59:
  c->daddiu(t3, t2, -3);                            // daddiu t3, t2, -3
  c->ori(t0, t0, 64);                               // ori t0, t0, 64
  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L54
  c->daddiu(t2, t2, -2);                            // daddiu t2, t2, -2
  if (bc) {goto block_63;}                          // branch non-likely

  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L53
  c->lqc2(vf14, 384, v1);                           // lqc2 vf14, 384(v1)
  if (bc) {goto block_62;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 368, v1);                           // lqc2 vf14, 368(v1)
  //beq r0, r0, L55                                 // beq r0, r0, L55
  c->lqc2(vf20, 2816, v1);                          // lqc2 vf20, 2816(v1)
  goto block_64;                                    // branch always


  block_62:
  //beq r0, r0, L55                                 // beq r0, r0, L55
  c->lqc2(vf20, 2832, v1);                          // lqc2 vf20, 2832(v1)
  goto block_64;                                    // branch always


  block_63:
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 400, v1);                           // lqc2 vf14, 400(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf20, 2848, v1);                          // lqc2 vf20, 2848(v1)

  block_64:
  c->lbu(t2, 4, a0);                                // lbu t2, 4(a0)
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  c->vadd(DEST::xyz, vf4, vf23, vf15);              // vadd.xyz vf4, vf23, vf15
  c->sll(t3, t2, 4);                                // sll t3, t2, 4
  //beq r0, r0, L60                                 // beq r0, r0, L60
  c->vsub(DEST::xyzw, vf1, vf25, vf23);             // vsub.xyzw vf1, vf25, vf23
  goto block_72;                                    // branch always


  block_65:
  // nop                                            // sll r0, r0, 0
  c->addiu(t3, r0, -65);                            // addiu t3, r0, -65
  c->and_(t0, t0, t3);                              // and t0, t0, t3
  c->lqc2(vf14, 352, v1);                           // lqc2 vf14, 352(v1)
  c->vadd(DEST::xyz, vf4, vf23, vf15);              // vadd.xyz vf4, vf23, vf15
  c->sll(t3, t2, 4);                                // sll t3, t2, 4
  c->vsub(DEST::xyzw, vf1, vf25, vf23);             // vsub.xyzw vf1, vf25, vf23
  c->daddiu(t4, t2, -10);                           // daddiu t4, t2, -10
  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L57
  c->daddiu(t2, t2, -13);                           // daddiu t2, t2, -13
  if (bc) {goto block_67;}                          // branch non-likely

  bc = c->sgpr64(t2) != 0;                          // bne t2, r0, L60
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_72;}                          // branch non-likely


  block_67:
  c->vsub(DEST::xyzw, vf1, vf23, vf24);             // vsub.xyzw vf1, vf23, vf24
  c->andi(t2, t0, 16);                              // andi t2, t0, 16
  bc = c->sgpr64(t2) != 0;                          // bne t2, r0, L58
  c->andi(t2, t0, 4);                               // andi t2, t0, 4
  if (bc) {goto block_70;}                          // branch non-likely

  bc = c->sgpr64(t2) != 0;                          // bne t2, r0, L59
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_71;}                          // branch non-likely

  c->vadd_bc(DEST::x, BC::x, vf23, vf0, vf24);      // vaddx.x vf23, vf0, vf24
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf23, 1120, t1);                          // sqc2 vf23, 1120(t1)
  c->vadd_bc(DEST::y, BC::w, vf23, vf23, vf15);     // vaddw.y vf23, vf23, vf15
  //beq r0, r0, L37                                 // beq r0, r0, L37
  c->daddiu(t1, t1, 16);                            // daddiu t1, t1, 16
  goto block_4;                                     // branch always


  block_70:
  c->vsub(DEST::x, vf23, vf24, vf1);                // vsub.x vf23, vf24, vf1
  c->sqc2(vf23, 1120, t1);                          // sqc2 vf23, 1120(t1)
  c->vadd_bc(DEST::x, BC::x, vf23, vf0, vf24);      // vaddx.x vf23, vf0, vf24
  c->vadd_bc(DEST::y, BC::w, vf23, vf23, vf15);     // vaddw.y vf23, vf23, vf15
  //beq r0, r0, L37                                 // beq r0, r0, L37
  c->daddiu(t1, t1, 16);                            // daddiu t1, t1, 16
  goto block_4;                                     // branch always


  block_71:
  c->vmul_bc(DEST::x, BC::w, vf1, vf1, vf16);       // vmulw.x vf1, vf1, vf16
  // nop                                            // sll r0, r0, 0
  c->vsub(DEST::x, vf23, vf24, vf1);                // vsub.x vf23, vf24, vf1
  c->sqc2(vf23, 1120, t1);                          // sqc2 vf23, 1120(t1)
  c->vadd_bc(DEST::x, BC::x, vf23, vf0, vf24);      // vaddx.x vf23, vf0, vf24
  c->vadd_bc(DEST::y, BC::w, vf23, vf23, vf15);     // vaddw.y vf23, vf23, vf15
  //beq r0, r0, L37                                 // beq r0, r0, L37
  c->daddiu(t1, t1, 16);                            // daddiu t1, t1, 16
  goto block_4;                                     // branch always


  block_72:
  c->addu(t2, t3, a3);                              // addu t2, t3, a3
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf5, -96, t2);                            // lqc2 vf5, -96(t2)
  c->mov128_gpr_vf(t2, vf1);                        // qmfc2.i t2, vf1
  bc = ((s64)c->sgpr64(t2)) < 0;                    // bltz t2, L63
  c->sra(t2, t2, 31);                               // sra t2, t2, 31
  if (bc) {goto block_78;}                          // branch non-likely

  c->vmul(DEST::xyzw, vf19, vf5, vf13);             // vmul.xyzw vf19, vf5, vf13
  c->andi(t2, t0, 2);                               // andi t2, t0, 2
  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L61
  c->andi(t2, t0, 64);                              // andi t2, t0, 64
  if (bc) {goto block_76;}                          // branch non-likely

  bc = c->sgpr64(t2) != 0;                          // bne t2, r0, L61
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_76;}                          // branch non-likely

  //beq r0, r0, L62                                 // beq r0, r0, L62
  c->vadd_bc(DEST::x, BC::w, vf23, vf23, vf19);     // vaddw.x vf23, vf23, vf19
  goto block_77;                                    // branch always


  block_76:
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::w, vf23, vf23, vf14);     // vaddw.x vf23, vf23, vf14

  block_77:
  //beq r0, r0, L37                                 // beq r0, r0, L37
  // nop                                            // sll r0, r0, 0
  goto block_4;                                     // branch always


  block_78:
  c->vsub(DEST::xyzw, vf1, vf23, vf24);             // vsub.xyzw vf1, vf23, vf24
  c->andi(a0, t0, 16);                              // andi a0, t0, 16
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L64
  c->andi(a0, t0, 4);                               // andi a0, t0, 4
  if (bc) {goto block_81;}                          // branch non-likely

  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L65
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_82;}                          // branch non-likely

  c->vadd_bc(DEST::x, BC::x, vf23, vf0, vf24);      // vaddx.x vf23, vf0, vf24
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L66                                 // beq r0, r0, L66
  c->sqc2(vf23, 1120, t1);                          // sqc2 vf23, 1120(t1)
  goto block_83;                                    // branch always


  block_81:
  c->vsub(DEST::x, vf23, vf24, vf1);                // vsub.x vf23, vf24, vf1
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L66                                 // beq r0, r0, L66
  c->sqc2(vf23, 1120, t1);                          // sqc2 vf23, 1120(t1)
  goto block_83;                                    // branch always


  block_82:
  c->vmul_bc(DEST::x, BC::w, vf1, vf1, vf16);       // vmulw.x vf1, vf1, vf16
  // nop                                            // sll r0, r0, 0
  c->vsub(DEST::x, vf23, vf24, vf1);                // vsub.x vf23, vf24, vf1
  c->sqc2(vf23, 1120, t1);                          // sqc2 vf23, 1120(t1)

  block_83:
  c->lw(a0, 2880, v1);                              // lw a0, 2880(v1)
  c->mov64(t0, v1);                                 // or t0, v1, r0
  c->lw(t1, 2876, v1);                              // lw t1, 2876(v1)
  c->lqc2(vf23, 1120, t0);                          // lqc2 vf23, 1120(t0)

  block_84:
  c->lbu(t2, 4, t1);                                // lbu t2, 4(t1)
  c->daddiu(t1, t1, 1);                             // daddiu t1, t1, 1
  c->lqc2(vf20, 2800, v1);                          // lqc2 vf20, 2800(v1)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L90
  c->daddiu(t3, t2, -3);                            // daddiu t3, t2, -3
  if (bc) {goto block_158;}                         // branch non-likely

  bc = ((s64)c->sgpr64(t3)) <= 0;                   // blez t3, L81
  c->daddiu(t3, t2, -126);                          // daddiu t3, t2, -126
  if (bc) {goto block_137;}                         // branch non-likely

  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L85
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_143;}                         // branch non-likely

  c->lbu(t2, 4, t1);                                // lbu t2, 4(t1)
  c->daddiu(t1, t1, 1);                             // daddiu t1, t1, 1
  c->addiu(t3, r0, 0);                              // addiu t3, r0, 0
  c->addiu(t4, r0, 0);                              // addiu t4, r0, 0
  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L90
  c->daddiu(t5, t2, -43);                           // daddiu t5, t2, -43
  if (bc) {goto block_158;}                         // branch non-likely

  c->movz(t3, t2, t5);                              // movz t3, t2, t5
  c->daddiu(t5, t2, -45);                           // daddiu t5, t2, -45
  c->movz(t3, t2, t5);                              // movz t3, t2, t5
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L68
  c->daddiu(t5, t2, -91);                           // daddiu t5, t2, -91
  if (bc) {goto block_98;}                          // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L67
  c->daddiu(t4, t2, -93);                           // daddiu t4, t2, -93
  if (bc) {goto block_84;}                          // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L67
  c->daddiu(t4, t2, -121);                          // daddiu t4, t2, -121
  if (bc) {goto block_84;}                          // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L79
  c->daddiu(t4, t2, -89);                           // daddiu t4, t2, -89
  if (bc) {goto block_135;}                         // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L79
  c->daddiu(t4, t2, -122);                          // daddiu t4, t2, -122
  if (bc) {goto block_135;}                         // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L80
  c->daddiu(t4, t2, -90);                           // daddiu t4, t2, -90
  if (bc) {goto block_136;}                         // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L80
  c->daddiu(t4, t2, -48);                           // daddiu t4, t2, -48
  if (bc) {goto block_136;}                         // branch non-likely

  bc = ((s64)c->sgpr64(t4)) < 0;                    // bltz t4, L85
  c->daddiu(t4, t2, -57);                           // daddiu t4, t2, -57
  if (bc) {goto block_143;}                         // branch non-likely

  bc = ((s64)c->sgpr64(t4)) > 0;                    // bgtz t4, L85
  c->daddiu(t4, t2, -126);                          // daddiu t4, t2, -126
  if (bc) {goto block_143;}                         // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L85
  c->daddiu(t4, t2, -48);                           // daddiu t4, t2, -48
  if (bc) {goto block_143;}                         // branch non-likely


  block_98:
  c->lbu(t2, 4, t1);                                // lbu t2, 4(t1)
  c->daddiu(t1, t1, 1);                             // daddiu t1, t1, 1
  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L90
  c->daddiu(t5, t2, -110);                          // daddiu t5, t2, -110
  if (bc) {goto block_158;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L69
  c->daddiu(t5, t2, -78);                           // daddiu t5, t2, -78
  if (bc) {goto block_118;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L69
  c->daddiu(t5, t2, -108);                          // daddiu t5, t2, -108
  if (bc) {goto block_118;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L67
  c->daddiu(t5, t2, -76);                           // daddiu t5, t2, -76
  if (bc) {goto block_84;}                          // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L67
  c->daddiu(t5, t2, -119);                          // daddiu t5, t2, -119
  if (bc) {goto block_84;}                          // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L71
  c->daddiu(t5, t2, -87);                           // daddiu t5, t2, -87
  if (bc) {goto block_121;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L71
  c->daddiu(t5, t2, -107);                          // daddiu t5, t2, -107
  if (bc) {goto block_121;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L72
  c->daddiu(t5, t2, -75);                           // daddiu t5, t2, -75
  if (bc) {goto block_123;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L72
  c->daddiu(t5, t2, -106);                          // daddiu t5, t2, -106
  if (bc) {goto block_123;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L67
  c->daddiu(t5, t2, -74);                           // daddiu t5, t2, -74
  if (bc) {goto block_84;}                          // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L67
  c->daddiu(t5, t2, -104);                          // daddiu t5, t2, -104
  if (bc) {goto block_84;}                          // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L73
  c->daddiu(t5, t2, -72);                           // daddiu t5, t2, -72
  if (bc) {goto block_125;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L73
  c->daddiu(t5, t2, -118);                          // daddiu t5, t2, -118
  if (bc) {goto block_125;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L76
  c->daddiu(t5, t2, -86);                           // daddiu t5, t2, -86
  if (bc) {goto block_130;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L76
  c->daddiu(t5, t2, -117);                          // daddiu t5, t2, -117
  if (bc) {goto block_130;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L67
  c->daddiu(t5, t2, -85);                           // daddiu t5, t2, -85
  if (bc) {goto block_84;}                          // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L67
  c->daddiu(t5, t2, -48);                           // daddiu t5, t2, -48
  if (bc) {goto block_84;}                          // branch non-likely

  bc = ((s64)c->sgpr64(t5)) < 0;                    // bltz t5, L85
  c->daddiu(t6, t2, -57);                           // daddiu t6, t2, -57
  if (bc) {goto block_143;}                         // branch non-likely

  bc = ((s64)c->sgpr64(t6)) > 0;                    // bgtz t6, L85
  c->sll(t6, t4, 2);                                // sll t6, t4, 2
  if (bc) {goto block_143;}                         // branch non-likely

  c->daddu(t2, t4, t6);                             // daddu t2, t4, t6
  // nop                                            // sll r0, r0, 0
  c->sll(t2, t2, 1);                                // sll t2, t2, 1
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L68                                 // beq r0, r0, L68
  c->daddu(t4, t2, t5);                             // daddu t4, t2, t5
  goto block_98;                                    // branch always


  block_118:
  bc = c->sgpr64(t4) != 0;                          // bne t4, r0, L70
  c->addiu(t2, r0, -33);                            // addiu t2, r0, -33
  if (bc) {goto block_120;}                         // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lq(a3, 80, v1);                                // lq a3, 80(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 96, v1);                                // lq t3, 96(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t4, 112, v1);                               // lq t4, 112(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t5, 128, v1);                               // lq t5, 128(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 2800, v1);                              // sq a3, 2800(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t3, 2816, v1);                              // sq t3, 2816(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t4, 2832, v1);                              // sq t4, 2832(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t5, 2848, v1);                              // sq t5, 2848(v1)
  // nop                                            // sll r0, r0, 0
  c->load_symbol2(a3, cache.font12_table);           // lw a3, *font12-table*(s7)
  c->mov64(a3, a3);                                 // or a3, a3, r0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf13, 208, v1);                           // lqc2 vf13, 208(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 224, v1);                           // lqc2 vf14, 224(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf15, 240, v1);                           // lqc2 vf15, 240(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 352, v1);                           // sqc2 vf14, 352(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 368, v1);                           // sqc2 vf14, 368(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 384, v1);                           // sqc2 vf14, 384(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 400, v1);                           // sqc2 vf14, 400(v1)
  //beq r0, r0, L67                                 // beq r0, r0, L67
  c->and_(a0, a0, t2);                              // and a0, a0, t2
  goto block_84;                                    // branch always


  block_120:
  // nop                                            // sll r0, r0, 0
  c->lq(a3, 144, v1);                               // lq a3, 144(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 160, v1);                               // lq t2, 160(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 176, v1);                               // lq t3, 176(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t4, 192, v1);                               // lq t4, 192(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 2800, v1);                              // sq a3, 2800(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 2816, v1);                              // sq t2, 2816(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t3, 2832, v1);                              // sq t3, 2832(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t4, 2848, v1);                              // sq t4, 2848(v1)
  // nop                                            // sll r0, r0, 0
  c->load_symbol2(a3, cache.font24_table);           // lw a3, *font24-table*(s7)
  c->mov64(a3, a3);                                 // or a3, a3, r0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf13, 256, v1);                           // lqc2 vf13, 256(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf15, 336, v1);                           // lqc2 vf15, 336(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 272, v1);                           // lqc2 vf14, 272(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 352, v1);                           // sqc2 vf14, 352(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 288, v1);                           // lqc2 vf14, 288(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 368, v1);                           // sqc2 vf14, 368(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 304, v1);                           // lqc2 vf14, 304(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 384, v1);                           // sqc2 vf14, 384(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 320, v1);                           // lqc2 vf14, 320(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 400, v1);                           // sqc2 vf14, 400(v1)
  //beq r0, r0, L67                                 // beq r0, r0, L67
  c->ori(a0, a0, 32);                               // ori a0, a0, 32
  goto block_84;                                    // branch always


  block_121:
  c->addiu(t2, r0, -2);                             // addiu t2, r0, -2
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L67
  c->and_(a0, a0, t2);                              // and a0, a0, t2
  if (bc) {goto block_84;}                          // branch non-likely

  //beq r0, r0, L67                                 // beq r0, r0, L67
  c->ori(a0, a0, 1);                                // ori a0, a0, 1
  goto block_84;                                    // branch always


  block_123:
  c->addiu(t2, r0, -3);                             // addiu t2, r0, -3
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L67
  c->and_(a0, a0, t2);                              // and a0, a0, t2
  if (bc) {goto block_84;}                          // branch non-likely

  //beq r0, r0, L67                                 // beq r0, r0, L67
  c->ori(a0, a0, 2);                                // ori a0, a0, 2
  goto block_84;                                    // branch always


  block_125:
  c->mov128_vf_gpr(vf1, t4);                        // qmtc2.i vf1, t4
  c->daddiu(t2, t3, -45);                           // daddiu t2, t3, -45
  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L75
  c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
  if (bc) {goto block_129;}                         // branch non-likely

  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L74
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_128;}                         // branch non-likely

  //beq r0, r0, L67                                 // beq r0, r0, L67
  c->vadd_bc(DEST::x, BC::x, vf23, vf23, vf1);      // vaddx.x vf23, vf23, vf1
  goto block_84;                                    // branch always


  block_128:
  //beq r0, r0, L67                                 // beq r0, r0, L67
  c->vsub_bc(DEST::x, BC::x, vf23, vf23, vf1);      // vsubx.x vf23, vf23, vf1
  goto block_84;                                    // branch always


  block_129:
  //beq r0, r0, L67                                 // beq r0, r0, L67
  c->vadd_bc(DEST::x, BC::x, vf23, vf0, vf1);       // vaddx.x vf23, vf0, vf1
  goto block_84;                                    // branch always


  block_130:
  c->mov128_vf_gpr(vf1, t4);                        // qmtc2.i vf1, t4
  c->daddiu(t2, t3, -45);                           // daddiu t2, t3, -45
  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L78
  c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
  if (bc) {goto block_134;}                         // branch non-likely

  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L77
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_133;}                         // branch non-likely

  //beq r0, r0, L67                                 // beq r0, r0, L67
  c->vadd_bc(DEST::y, BC::x, vf23, vf23, vf1);      // vaddx.y vf23, vf23, vf1
  goto block_84;                                    // branch always


  block_133:
  //beq r0, r0, L67                                 // beq r0, r0, L67
  c->vsub_bc(DEST::y, BC::x, vf23, vf23, vf1);      // vsubx.y vf23, vf23, vf1
  goto block_84;                                    // branch always


  block_134:
  //beq r0, r0, L67                                 // beq r0, r0, L67
  c->vadd_bc(DEST::y, BC::x, vf23, vf0, vf1);       // vaddx.y vf23, vf0, vf1
  goto block_84;                                    // branch always


  block_135:
  //beq r0, r0, L67                                 // beq r0, r0, L67
  c->sqc2(vf23, 464, v1);                           // sqc2 vf23, 464(v1)
  goto block_84;                                    // branch always


  block_136:
  //beq r0, r0, L67                                 // beq r0, r0, L67
  c->lqc2(vf23, 464, v1);                           // lqc2 vf23, 464(v1)
  goto block_84;                                    // branch always


  block_137:
  c->daddiu(t3, t2, -3);                            // daddiu t3, t2, -3
  c->ori(a0, a0, 64);                               // ori a0, a0, 64
  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L83
  c->daddiu(t2, t2, -2);                            // daddiu t2, t2, -2
  if (bc) {goto block_141;}                         // branch non-likely

  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L82
  c->lqc2(vf14, 384, v1);                           // lqc2 vf14, 384(v1)
  if (bc) {goto block_140;}                         // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 368, v1);                           // lqc2 vf14, 368(v1)
  //beq r0, r0, L84                                 // beq r0, r0, L84
  c->lqc2(vf20, 2816, v1);                          // lqc2 vf20, 2816(v1)
  goto block_142;                                   // branch always


  block_140:
  //beq r0, r0, L84                                 // beq r0, r0, L84
  c->lqc2(vf20, 2832, v1);                          // lqc2 vf20, 2832(v1)
  goto block_142;                                   // branch always


  block_141:
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 400, v1);                           // lqc2 vf14, 400(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf20, 2848, v1);                          // lqc2 vf20, 2848(v1)

  block_142:
  c->lbu(t2, 4, t1);                                // lbu t2, 4(t1)
  c->daddiu(t1, t1, 1);                             // daddiu t1, t1, 1
  c->vadd(DEST::xyz, vf4, vf23, vf15);              // vadd.xyz vf4, vf23, vf15
  c->sll(t3, t2, 4);                                // sll t3, t2, 4
  //beq r0, r0, L87                                 // beq r0, r0, L87
  c->vsub(DEST::xyzw, vf1, vf25, vf23);             // vsub.xyzw vf1, vf25, vf23
  goto block_146;                                   // branch always


  block_143:
  // nop                                            // sll r0, r0, 0
  c->addiu(t3, r0, -65);                            // addiu t3, r0, -65
  c->and_(a0, a0, t3);                              // and a0, a0, t3
  c->lqc2(vf14, 352, v1);                           // lqc2 vf14, 352(v1)
  c->vadd(DEST::xyz, vf4, vf23, vf15);              // vadd.xyz vf4, vf23, vf15
  c->sll(t3, t2, 4);                                // sll t3, t2, 4
  c->vsub(DEST::xyzw, vf1, vf25, vf23);             // vsub.xyzw vf1, vf25, vf23
  c->daddiu(t4, t2, -10);                           // daddiu t4, t2, -10
  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L86
  c->daddiu(t2, t2, -13);                           // daddiu t2, t2, -13
  if (bc) {goto block_145;}                         // branch non-likely

  bc = c->sgpr64(t2) != 0;                          // bne t2, r0, L87
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_146;}                         // branch non-likely


  block_145:
  // nop                                            // sll r0, r0, 0
  c->daddiu(t0, t0, 16);                            // daddiu t0, t0, 16
  //beq r0, r0, L67                                 // beq r0, r0, L67
  c->lqc2(vf23, 1120, t0);                          // lqc2 vf23, 1120(t0)
  goto block_84;                                    // branch always


  block_146:
  // nop                                            // sll r0, r0, 0
  c->addu(t2, t3, a3);                              // addu t2, t3, a3
  c->lqc2(vf5, -96, t2);                            // lqc2 vf5, -96(t2)
  c->mov128_gpr_vf(t2, vf1);                        // qmfc2.i t2, vf1
  bc = ((s64)c->sgpr64(t2)) < 0;                    // bltz t2, L90
  c->vadd(DEST::xyz, vf6, vf5, vf16);               // vadd.xyz vf6, vf5, vf16
  if (bc) {goto block_158;}                         // branch non-likely

  c->sra(t2, t2, 31);                               // sra t2, t2, 31
  c->vadd(DEST::xyz, vf7, vf5, vf17);               // vadd.xyz vf7, vf5, vf17
  bc = ((s64)c->sgpr64(t2)) < 0;                    // bltz t2, L67
  c->vadd(DEST::xyz, vf8, vf5, vf18);               // vadd.xyz vf8, vf5, vf18
  if (bc) {goto block_84;}                          // branch non-likely

  c->vadd(DEST::xyz, vf1, vf23, vf0);               // vadd.xyz vf1, vf23, vf0
  c->andi(t2, a0, 1);                               // andi t2, a0, 1
  c->vadd(DEST::xyz, vf2, vf23, vf13);              // vadd.xyz vf2, vf23, vf13
  c->sqc2(vf5, 672, v1);                            // sqc2 vf5, 672(v1)
  c->vadd(DEST::xyz, vf3, vf23, vf14);              // vadd.xyz vf3, vf23, vf14
  c->sqc2(vf6, 688, v1);                            // sqc2 vf6, 688(v1)
  c->vmul(DEST::xyzw, vf19, vf5, vf13);             // vmul.xyzw vf19, vf5, vf13
  c->sqc2(vf7, 704, v1);                            // sqc2 vf7, 704(v1)
  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L67
  c->sqc2(vf8, 720, v1);                            // sqc2 vf8, 720(v1)
  if (bc) {goto block_84;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->sqc2(vf1, 544, v1);                            // sqc2 vf1, 544(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf2, 560, v1);                            // sqc2 vf2, 560(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf3, 576, v1);                            // sqc2 vf3, 576(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf4, 592, v1);                            // sqc2 vf4, 592(v1)
  c->lqc2(vf1, 544, v1);                            // lqc2 vf1, 544(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 560, v1);                            // lqc2 vf2, 560(v1)
  c->vmula_bc(DEST::xyzw, BC::w, vf31, vf0);        // vmulaw.xyzw acc, vf31, vf0
  c->lqc2(vf3, 576, v1);                            // lqc2 vf3, 576(v1)
  c->vmadda_bc(DEST::xyzw, BC::x, vf28, vf1);       // vmaddax.xyzw acc, vf28, vf1
  c->lqc2(vf4, 592, v1);                            // lqc2 vf4, 592(v1)
  c->vmadda_bc(DEST::xyzw, BC::y, vf29, vf1);       // vmadday.xyzw acc, vf29, vf1
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf1, vf30, vf1);   // vmaddz.xyzw vf1, vf30, vf1
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf31, vf0);        // vmulaw.xyzw acc, vf31, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf28, vf2);       // vmaddax.xyzw acc, vf28, vf2
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf29, vf2);       // vmadday.xyzw acc, vf29, vf2
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf2, vf30, vf2);   // vmaddz.xyzw vf2, vf30, vf2
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf31, vf0);        // vmulaw.xyzw acc, vf31, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf28, vf3);       // vmaddax.xyzw acc, vf28, vf3
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf29, vf3);       // vmadday.xyzw acc, vf29, vf3
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf3, vf30, vf3);   // vmaddz.xyzw vf3, vf30, vf3
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf31, vf0);        // vmulaw.xyzw acc, vf31, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf28, vf4);       // vmaddax.xyzw acc, vf28, vf4
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf29, vf4);       // vmadday.xyzw acc, vf29, vf4
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf4, vf30, vf4);   // vmaddz.xyzw vf4, vf30, vf4
  c->vdiv(vf25, BC::z, vf1, BC::w);                 // vdiv Q, vf25.z, vf1.w
  c->lq(t2, 32, v1);                                // lq t2, 32(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 48, v1);                                // lq t3, 48(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 0, a1);                                 // sq t2, 0(a1)
  // nop                                            // sll r0, r0, 0
  c->sq(t3, 16, a1);                                // sq t3, 16(a1)
  c->lqc2(vf5, 672, v1);                            // lqc2 vf5, 672(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf6, 688, v1);                            // lqc2 vf6, 688(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf7, 704, v1);                            // lqc2 vf7, 704(v1)
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf1, vf1);                    // vmulq.xyz vf1, vf1, Q
  c->sqc2(vf20, 32, a1);                            // sqc2 vf20, 32(a1)
  c->vmulq(DEST::xyz, vf5, vf5);                    // vmulq.xyz vf5, vf5, Q
  // nop                                            // sll r0, r0, 0
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  c->vdiv(vf25, BC::z, vf2, BC::w);                 // vdiv Q, vf25.z, vf2.w
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf8, 720, v1);                            // lqc2 vf8, 720(v1)
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyzw, vf1, vf1, vf26);              // vadd.xyzw vf1, vf1, vf26
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf9, 2144, v1);                           // lqc2 vf9, 2144(v1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->vftoi4_sat(DEST::xyzw, vf1, vf1);                  // vftoi4.xyzw vf1, vf1
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf2, vf2);                    // vmulq.xyz vf2, vf2, Q
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf6, vf6);                    // vmulq.xyz vf6, vf6, Q
  c->sqc2(vf5, 48, a1);                             // sqc2 vf5, 48(a1)
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  c->vdiv(vf25, BC::z, vf3, BC::w);                 // vdiv Q, vf25.z, vf3.w
  c->sqc2(vf9, 64, a1);                             // sqc2 vf9, 64(a1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf1, 80, a1);                             // sqc2 vf1, 80(a1)
  c->vadd(DEST::xyzw, vf2, vf2, vf26);              // vadd.xyzw vf2, vf2, vf26
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->vftoi4_sat(DEST::xyzw, vf2, vf2);                  // vftoi4.xyzw vf2, vf2
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf3, vf3);                    // vmulq.xyz vf3, vf3, Q
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf7, vf7);                    // vmulq.xyz vf7, vf7, Q
  c->sqc2(vf6, 96, a1);                             // sqc2 vf6, 96(a1)
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  c->vdiv(vf25, BC::z, vf4, BC::w);                 // vdiv Q, vf25.z, vf4.w
  c->sqc2(vf9, 112, a1);                            // sqc2 vf9, 112(a1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf2, 128, a1);                            // sqc2 vf2, 128(a1)
  c->vadd(DEST::xyzw, vf3, vf3, vf26);              // vadd.xyzw vf3, vf3, vf26
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->vftoi4_sat(DEST::xyzw, vf3, vf3);                  // vftoi4.xyzw vf3, vf3
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf4, vf4);                    // vmulq.xyz vf4, vf4, Q
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf8, vf8);                    // vmulq.xyz vf8, vf8, Q
  c->sqc2(vf7, 144, a1);                            // sqc2 vf7, 144(a1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf9, 160, a1);                            // sqc2 vf9, 160(a1)
  c->vadd(DEST::xyzw, vf4, vf4, vf26);              // vadd.xyzw vf4, vf4, vf26
  c->sqc2(vf3, 176, a1);                            // sqc2 vf3, 176(a1)
  c->andi(t2, a0, 2);                               // andi t2, a0, 2
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L88
  c->andi(t2, a0, 64);                              // andi t2, a0, 64
  if (bc) {goto block_152;}                         // branch non-likely

  bc = c->sgpr64(t2) != 0;                          // bne t2, r0, L88
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_152;}                         // branch non-likely

  //beq r0, r0, L89                                 // beq r0, r0, L89
  c->vadd_bc(DEST::x, BC::w, vf23, vf23, vf19);     // vaddw.x vf23, vf23, vf19
  goto block_153;                                   // branch always


  block_152:
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::w, vf23, vf23, vf14);     // vaddw.x vf23, vf23, vf14

  block_153:
  c->vftoi4_sat(DEST::xyzw, vf4, vf4);                  // vftoi4.xyzw vf4, vf4
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, 192, a1);                            // sqc2 vf8, 192(a1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf9, 208, a1);                            // sqc2 vf9, 208(a1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf4, 224, a1);                            // sqc2 vf4, 224(a1)
  // nop                                            // sll r0, r0, 0
  c->lw(t3, 80, a1);                                // lw t3, 80(a1)
  // nop                                            // sll r0, r0, 0
  c->lw(t2, 84, a1);                                // lw t2, 84(a1)
  c->ori(t4, r0, 36864);                            // ori t4, r0, 36864
  c->dsubu(t3, t3, t4);                             // dsubu t3, t3, t4
  c->lw(t4, 224, a1);                               // lw t4, 224(a1)
  c->ori(t5, r0, 36096);                            // ori t5, r0, 36096
  c->dsubu(t2, t2, t5);                             // dsubu t2, t2, t5
  c->lw(t5, 228, a1);                               // lw t5, 228(a1)
  bc = ((s64)c->sgpr64(t3)) > 0;                    // bgtz t3, L67
  c->daddiu(t3, t4, -28672);                        // daddiu t3, t4, -28672
  if (bc) {goto block_84;}                          // branch non-likely

  bc = ((s64)c->sgpr64(t2)) > 0;                    // bgtz t2, L67
  c->daddiu(t2, t5, -29440);                        // daddiu t2, t5, -29440
  if (bc) {goto block_84;}                          // branch non-likely

  bc = ((s64)c->sgpr64(t3)) < 0;                    // bltz t3, L67
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_84;}                          // branch non-likely

  bc = ((s64)c->sgpr64(t2)) < 0;                    // bltz t2, L67
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_84;}                          // branch non-likely

  //beq r0, r0, L67                                 // beq r0, r0, L67
  c->daddiu(a1, a1, 240);                           // daddiu a1, a1, 240
  goto block_84;                                    // branch always


  block_158:
  c->lw(a0, 2880, v1);                              // lw a0, 2880(v1)
  c->mov64(t0, v1);                                 // or t0, v1, r0
  c->lw(t1, 2876, v1);                              // lw t1, 2876(v1)
  c->lqc2(vf23, 1120, t0);                          // lqc2 vf23, 1120(t0)

  block_159:
  c->lbu(t2, 4, t1);                                // lbu t2, 4(t1)
  c->daddiu(t1, t1, 1);                             // daddiu t1, t1, 1
  c->lqc2(vf20, 2800, v1);                          // lqc2 vf20, 2800(v1)
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L117
  c->daddiu(t3, t2, -3);                            // daddiu t3, t2, -3
  if (bc) {goto block_234;}                         // branch non-likely

  bc = ((s64)c->sgpr64(t3)) <= 0;                   // blez t3, L108
  c->daddiu(t3, t2, -126);                          // daddiu t3, t2, -126
  if (bc) {goto block_214;}                         // branch non-likely

  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L112
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_220;}                         // branch non-likely

  c->lbu(t2, 4, t1);                                // lbu t2, 4(t1)
  c->daddiu(t1, t1, 1);                             // daddiu t1, t1, 1
  c->addiu(t3, r0, 0);                              // addiu t3, r0, 0
  c->addiu(t4, r0, 0);                              // addiu t4, r0, 0
  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L117
  c->daddiu(t5, t2, -43);                           // daddiu t5, t2, -43
  if (bc) {goto block_234;}                         // branch non-likely

  c->movz(t3, t2, t5);                              // movz t3, t2, t5
  c->daddiu(t5, t2, -45);                           // daddiu t5, t2, -45
  c->movz(t3, t2, t5);                              // movz t3, t2, t5
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L92
  c->daddiu(t5, t2, -91);                           // daddiu t5, t2, -91
  if (bc) {goto block_173;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L104
  c->daddiu(t4, t2, -93);                           // daddiu t4, t2, -93
  if (bc) {goto block_210;}                         // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L105
  c->daddiu(t4, t2, -121);                          // daddiu t4, t2, -121
  if (bc) {goto block_211;}                         // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L106
  c->daddiu(t4, t2, -89);                           // daddiu t4, t2, -89
  if (bc) {goto block_212;}                         // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L106
  c->daddiu(t4, t2, -122);                          // daddiu t4, t2, -122
  if (bc) {goto block_212;}                         // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L107
  c->daddiu(t4, t2, -90);                           // daddiu t4, t2, -90
  if (bc) {goto block_213;}                         // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L107
  c->daddiu(t4, t2, -48);                           // daddiu t4, t2, -48
  if (bc) {goto block_213;}                         // branch non-likely

  bc = ((s64)c->sgpr64(t4)) < 0;                    // bltz t4, L112
  c->daddiu(t4, t2, -57);                           // daddiu t4, t2, -57
  if (bc) {goto block_220;}                         // branch non-likely

  bc = ((s64)c->sgpr64(t4)) > 0;                    // bgtz t4, L112
  c->daddiu(t4, t2, -126);                          // daddiu t4, t2, -126
  if (bc) {goto block_220;}                         // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L112
  c->daddiu(t4, t2, -48);                           // daddiu t4, t2, -48
  if (bc) {goto block_220;}                         // branch non-likely


  block_173:
  c->lbu(t2, 4, t1);                                // lbu t2, 4(t1)
  c->daddiu(t1, t1, 1);                             // daddiu t1, t1, 1
  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L117
  c->daddiu(t5, t2, -110);                          // daddiu t5, t2, -110
  if (bc) {goto block_234;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L93
  c->daddiu(t5, t2, -78);                           // daddiu t5, t2, -78
  if (bc) {goto block_193;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L93
  c->daddiu(t5, t2, -108);                          // daddiu t5, t2, -108
  if (bc) {goto block_193;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L95
  c->daddiu(t5, t2, -76);                           // daddiu t5, t2, -76
  if (bc) {goto block_196;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L95
  c->daddiu(t5, t2, -119);                          // daddiu t5, t2, -119
  if (bc) {goto block_196;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L91
  c->daddiu(t5, t2, -87);                           // daddiu t5, t2, -87
  if (bc) {goto block_159;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L91
  c->daddiu(t5, t2, -107);                          // daddiu t5, t2, -107
  if (bc) {goto block_159;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L97
  c->daddiu(t5, t2, -75);                           // daddiu t5, t2, -75
  if (bc) {goto block_198;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L97
  c->daddiu(t5, t2, -106);                          // daddiu t5, t2, -106
  if (bc) {goto block_198;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L91
  c->daddiu(t5, t2, -74);                           // daddiu t5, t2, -74
  if (bc) {goto block_159;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L91
  c->daddiu(t5, t2, -104);                          // daddiu t5, t2, -104
  if (bc) {goto block_159;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L98
  c->daddiu(t5, t2, -72);                           // daddiu t5, t2, -72
  if (bc) {goto block_200;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L98
  c->daddiu(t5, t2, -118);                          // daddiu t5, t2, -118
  if (bc) {goto block_200;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L101
  c->daddiu(t5, t2, -86);                           // daddiu t5, t2, -86
  if (bc) {goto block_205;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L101
  c->daddiu(t5, t2, -117);                          // daddiu t5, t2, -117
  if (bc) {goto block_205;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L96
  c->daddiu(t5, t2, -85);                           // daddiu t5, t2, -85
  if (bc) {goto block_197;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L96
  c->daddiu(t5, t2, -48);                           // daddiu t5, t2, -48
  if (bc) {goto block_197;}                         // branch non-likely

  bc = ((s64)c->sgpr64(t5)) < 0;                    // bltz t5, L112
  c->daddiu(t6, t2, -57);                           // daddiu t6, t2, -57
  if (bc) {goto block_220;}                         // branch non-likely

  bc = ((s64)c->sgpr64(t6)) > 0;                    // bgtz t6, L112
  c->sll(t6, t4, 2);                                // sll t6, t4, 2
  if (bc) {goto block_220;}                         // branch non-likely

  c->daddu(t2, t4, t6);                             // daddu t2, t4, t6
  // nop                                            // sll r0, r0, 0
  c->sll(t2, t2, 1);                                // sll t2, t2, 1
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L92                                 // beq r0, r0, L92
  c->daddu(t4, t2, t5);                             // daddu t4, t2, t5
  goto block_173;                                   // branch always


  block_193:
  bc = c->sgpr64(t4) != 0;                          // bne t4, r0, L94
  c->addiu(t2, r0, -33);                            // addiu t2, r0, -33
  if (bc) {goto block_195;}                         // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lq(a3, 80, v1);                                // lq a3, 80(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 96, v1);                                // lq t3, 96(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t4, 112, v1);                               // lq t4, 112(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t5, 128, v1);                               // lq t5, 128(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 2800, v1);                              // sq a3, 2800(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t3, 2816, v1);                              // sq t3, 2816(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t4, 2832, v1);                              // sq t4, 2832(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t5, 2848, v1);                              // sq t5, 2848(v1)
  // nop                                            // sll r0, r0, 0
  c->load_symbol2(a3, cache.font12_table);           // lw a3, *font12-table*(s7)
  c->mov64(a3, a3);                                 // or a3, a3, r0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf13, 208, v1);                           // lqc2 vf13, 208(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 224, v1);                           // lqc2 vf14, 224(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf15, 240, v1);                           // lqc2 vf15, 240(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 352, v1);                           // sqc2 vf14, 352(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 368, v1);                           // sqc2 vf14, 368(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 384, v1);                           // sqc2 vf14, 384(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 400, v1);                           // sqc2 vf14, 400(v1)
  //beq r0, r0, L91                                 // beq r0, r0, L91
  c->and_(a0, a0, t2);                              // and a0, a0, t2
  goto block_159;                                   // branch always


  block_195:
  // nop                                            // sll r0, r0, 0
  c->lq(a3, 144, v1);                               // lq a3, 144(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 160, v1);                               // lq t2, 160(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 176, v1);                               // lq t3, 176(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t4, 192, v1);                               // lq t4, 192(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 2800, v1);                              // sq a3, 2800(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 2816, v1);                              // sq t2, 2816(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t3, 2832, v1);                              // sq t3, 2832(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t4, 2848, v1);                              // sq t4, 2848(v1)
  // nop                                            // sll r0, r0, 0
  c->load_symbol2(a3, cache.font24_table);           // lw a3, *font24-table*(s7)
  c->mov64(a3, a3);                                 // or a3, a3, r0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf13, 256, v1);                           // lqc2 vf13, 256(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf15, 336, v1);                           // lqc2 vf15, 336(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 272, v1);                           // lqc2 vf14, 272(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 352, v1);                           // sqc2 vf14, 352(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 288, v1);                           // lqc2 vf14, 288(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 368, v1);                           // sqc2 vf14, 368(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 304, v1);                           // lqc2 vf14, 304(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 384, v1);                           // sqc2 vf14, 384(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 320, v1);                           // lqc2 vf14, 320(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf14, 400, v1);                           // sqc2 vf14, 400(v1)
  //beq r0, r0, L91                                 // beq r0, r0, L91
  c->ori(a0, a0, 32);                               // ori a0, a0, 32
  goto block_159;                                   // branch always


  block_196:
  // nop                                            // sll r0, r0, 0
  c->sw(t4, 2864, v1);                              // sw t4, 2864(v1)
  c->sll(t2, t4, 4);                                // sll t2, t4, 4
  c->lq(t3, 608, v1);                               // lq t3, 608(v1)
  c->daddu(t2, t2, v1);                             // daddu t2, t2, v1
  c->lq(t4, 624, v1);                               // lq t4, 624(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t3, 800, v1);                               // sq t3, 800(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t4, 816, v1);                               // sq t4, 816(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 640, v1);                               // lq t3, 640(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t4, 656, v1);                               // lq t4, 656(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t3, 832, v1);                               // sq t3, 832(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t4, 848, v1);                               // sq t4, 848(v1)
  // nop                                            // sll r0, r0, 0
  c->lwu(t4, 2160, t2);                             // lwu t4, 2160(t2)
  // nop                                            // sll r0, r0, 0
  c->lwu(t3, 2164, t2);                             // lwu t3, 2164(t2)
  c->pextlb(t4, r0, t4);                            // pextlb t4, r0, t4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(t4, r0, t4);                            // pextlh t4, r0, t4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(t3, r0, t3);                            // pextlb t3, r0, t3
  c->sq(t4, 608, v1);                               // sq t4, 608(v1)
  c->pextlh(t3, r0, t3);                            // pextlh t3, r0, t3
  c->sq(t4, 992, v1);                               // sq t4, 992(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t3, 624, v1);                               // sq t3, 624(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t3, 1008, v1);                              // sq t3, 1008(v1)
  // nop                                            // sll r0, r0, 0
  c->lwu(t3, 2168, t2);                             // lwu t3, 2168(t2)
  // nop                                            // sll r0, r0, 0
  c->lwu(t2, 2172, t2);                             // lwu t2, 2172(t2)
  c->pextlb(t3, r0, t3);                            // pextlb t3, r0, t3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(t3, r0, t3);                            // pextlh t3, r0, t3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(t2, r0, t2);                            // pextlb t2, r0, t2
  c->sq(t3, 640, v1);                               // sq t3, 640(v1)
  c->pextlh(t2, r0, t2);                            // pextlh t2, r0, t2
  c->sq(t3, 1024, v1);                              // sq t3, 1024(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 656, v1);                               // sq t2, 656(v1)
  //beq r0, r0, L91                                 // beq r0, r0, L91
  c->sq(t2, 1040, v1);                              // sq t2, 1040(v1)
  goto block_159;                                   // branch always


  block_197:
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 608, v1);                               // lq t2, 608(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 624, v1);                               // lq t3, 624(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 800, v1);                               // sq t2, 800(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t3, 816, v1);                               // sq t3, 816(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 640, v1);                               // lq t2, 640(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 656, v1);                               // lq t3, 656(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 832, v1);                               // sq t2, 832(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t3, 848, v1);                               // sq t3, 848(v1)
  c->pextlb(t2, r0, t4);                            // pextlb t2, r0, t4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(t2, r0, t2);                            // pextlh t2, r0, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 608, v1);                               // sq t2, 608(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 992, v1);                               // sq t2, 992(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 624, v1);                               // sq t2, 624(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 1008, v1);                              // sq t2, 1008(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 640, v1);                               // sq t2, 640(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 1024, v1);                              // sq t2, 1024(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 656, v1);                               // sq t2, 656(v1)
  //beq r0, r0, L91                                 // beq r0, r0, L91
  c->sq(t2, 1040, v1);                              // sq t2, 1040(v1)
  goto block_159;                                   // branch always


  block_198:
  c->addiu(t2, r0, -3);                             // addiu t2, r0, -3
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L91
  c->and_(a0, a0, t2);                              // and a0, a0, t2
  if (bc) {goto block_159;}                         // branch non-likely

  //beq r0, r0, L91                                 // beq r0, r0, L91
  c->ori(a0, a0, 2);                                // ori a0, a0, 2
  goto block_159;                                   // branch always


  block_200:
  c->mov128_vf_gpr(vf1, t4);                        // qmtc2.i vf1, t4
  c->daddiu(t2, t3, -45);                           // daddiu t2, t3, -45
  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L100
  c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
  if (bc) {goto block_204;}                         // branch non-likely

  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L99
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_203;}                         // branch non-likely

  //beq r0, r0, L91                                 // beq r0, r0, L91
  c->vadd_bc(DEST::x, BC::x, vf23, vf23, vf1);      // vaddx.x vf23, vf23, vf1
  goto block_159;                                   // branch always


  block_203:
  //beq r0, r0, L91                                 // beq r0, r0, L91
  c->vsub_bc(DEST::x, BC::x, vf23, vf23, vf1);      // vsubx.x vf23, vf23, vf1
  goto block_159;                                   // branch always


  block_204:
  //beq r0, r0, L91                                 // beq r0, r0, L91
  c->vadd_bc(DEST::x, BC::x, vf23, vf0, vf1);       // vaddx.x vf23, vf0, vf1
  goto block_159;                                   // branch always


  block_205:
  c->mov128_vf_gpr(vf1, t4);                        // qmtc2.i vf1, t4
  c->daddiu(t2, t3, -45);                           // daddiu t2, t3, -45
  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L103
  c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
  if (bc) {goto block_209;}                         // branch non-likely

  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L102
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_208;}                         // branch non-likely

  //beq r0, r0, L91                                 // beq r0, r0, L91
  c->vadd_bc(DEST::y, BC::x, vf23, vf23, vf1);      // vaddx.y vf23, vf23, vf1
  goto block_159;                                   // branch always


  block_208:
  //beq r0, r0, L91                                 // beq r0, r0, L91
  c->vsub_bc(DEST::y, BC::x, vf23, vf23, vf1);      // vsubx.y vf23, vf23, vf1
  goto block_159;                                   // branch always


  block_209:
  //beq r0, r0, L91                                 // beq r0, r0, L91
  c->vadd_bc(DEST::y, BC::x, vf23, vf0, vf1);       // vaddx.y vf23, vf0, vf1
  goto block_159;                                   // branch always


  block_210:
  // nop                                            // sll r0, r0, 0
  c->lw(t2, 2864, v1);                              // lw t2, 2864(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf9, 608, v1);                            // lqc2 vf9, 608(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf10, 624, v1);                           // lqc2 vf10, 624(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf11, 640, v1);                           // lqc2 vf11, 640(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf12, 656, v1);                           // lqc2 vf12, 656(v1)
  // nop                                            // sll r0, r0, 0
  c->sw(t2, 2868, v1);                              // sw t2, 2868(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf9, 480, v1);                            // sqc2 vf9, 480(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf10, 496, v1);                           // sqc2 vf10, 496(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf11, 512, v1);                           // sqc2 vf11, 512(v1)
  //beq r0, r0, L91                                 // beq r0, r0, L91
  c->sqc2(vf12, 528, v1);                           // sqc2 vf12, 528(v1)
  goto block_159;                                   // branch always


  block_211:
  // nop                                            // sll r0, r0, 0
  c->lw(t2, 2868, v1);                              // lw t2, 2868(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf9, 480, v1);                            // lqc2 vf9, 480(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf10, 496, v1);                           // lqc2 vf10, 496(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf11, 512, v1);                           // lqc2 vf11, 512(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf12, 528, v1);                           // lqc2 vf12, 528(v1)
  // nop                                            // sll r0, r0, 0
  c->sw(t2, 2864, v1);                              // sw t2, 2864(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf9, 608, v1);                            // sqc2 vf9, 608(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf9, 992, v1);                            // sqc2 vf9, 992(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf10, 624, v1);                           // sqc2 vf10, 624(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf10, 1008, v1);                          // sqc2 vf10, 1008(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf11, 640, v1);                           // sqc2 vf11, 640(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf11, 1024, v1);                          // sqc2 vf11, 1024(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf12, 656, v1);                           // sqc2 vf12, 656(v1)
  //beq r0, r0, L91                                 // beq r0, r0, L91
  c->sqc2(vf12, 1040, v1);                          // sqc2 vf12, 1040(v1)
  goto block_159;                                   // branch always


  block_212:
  // nop                                            // sll r0, r0, 0
  c->lw(t2, 2864, v1);                              // lw t2, 2864(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf9, 608, v1);                            // lqc2 vf9, 608(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf10, 624, v1);                           // lqc2 vf10, 624(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf11, 640, v1);                           // lqc2 vf11, 640(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf12, 656, v1);                           // lqc2 vf12, 656(v1)
  // nop                                            // sll r0, r0, 0
  c->sw(t2, 2868, v1);                              // sw t2, 2868(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf9, 480, v1);                            // sqc2 vf9, 480(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf10, 496, v1);                           // sqc2 vf10, 496(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf11, 512, v1);                           // sqc2 vf11, 512(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf12, 528, v1);                           // sqc2 vf12, 528(v1)
  //beq r0, r0, L91                                 // beq r0, r0, L91
  c->sqc2(vf23, 464, v1);                           // sqc2 vf23, 464(v1)
  goto block_159;                                   // branch always


  block_213:
  // nop                                            // sll r0, r0, 0
  c->lw(t2, 2868, v1);                              // lw t2, 2868(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf9, 480, v1);                            // lqc2 vf9, 480(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf10, 496, v1);                           // lqc2 vf10, 496(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf11, 512, v1);                           // lqc2 vf11, 512(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf12, 528, v1);                           // lqc2 vf12, 528(v1)
  // nop                                            // sll r0, r0, 0
  c->sw(t2, 2864, v1);                              // sw t2, 2864(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf9, 608, v1);                            // sqc2 vf9, 608(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf9, 992, v1);                            // sqc2 vf9, 992(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf10, 624, v1);                           // sqc2 vf10, 624(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf10, 1008, v1);                          // sqc2 vf10, 1008(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf11, 640, v1);                           // sqc2 vf11, 640(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf11, 1024, v1);                          // sqc2 vf11, 1024(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf12, 656, v1);                           // sqc2 vf12, 656(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf12, 1040, v1);                          // sqc2 vf12, 1040(v1)
  //beq r0, r0, L91                                 // beq r0, r0, L91
  c->lqc2(vf23, 464, v1);                           // lqc2 vf23, 464(v1)
  goto block_159;                                   // branch always


  block_214:
  c->daddiu(t3, t2, -3);                            // daddiu t3, t2, -3
  c->ori(a0, a0, 64);                               // ori a0, a0, 64
  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L110
  c->daddiu(t2, t2, -2);                            // daddiu t2, t2, -2
  if (bc) {goto block_218;}                         // branch non-likely

  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L109
  c->lqc2(vf14, 384, v1);                           // lqc2 vf14, 384(v1)
  if (bc) {goto block_217;}                         // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 368, v1);                           // lqc2 vf14, 368(v1)
  //beq r0, r0, L111                                // beq r0, r0, L111
  c->lqc2(vf20, 2816, v1);                          // lqc2 vf20, 2816(v1)
  goto block_219;                                   // branch always


  block_217:
  //beq r0, r0, L111                                // beq r0, r0, L111
  c->lqc2(vf20, 2832, v1);                          // lqc2 vf20, 2832(v1)
  goto block_219;                                   // branch always


  block_218:
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 400, v1);                           // lqc2 vf14, 400(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf20, 2848, v1);                          // lqc2 vf20, 2848(v1)

  block_219:
  c->lbu(t2, 4, t1);                                // lbu t2, 4(t1)
  c->daddiu(t1, t1, 1);                             // daddiu t1, t1, 1
  c->vadd(DEST::xyz, vf4, vf23, vf15);              // vadd.xyz vf4, vf23, vf15
  c->sll(t3, t2, 4);                                // sll t3, t2, 4
  //beq r0, r0, L114                                // beq r0, r0, L114
  c->vsub(DEST::xyzw, vf1, vf25, vf23);             // vsub.xyzw vf1, vf25, vf23
  goto block_223;                                   // branch always


  block_220:
  // nop                                            // sll r0, r0, 0
  c->addiu(t3, r0, -65);                            // addiu t3, r0, -65
  c->and_(a0, a0, t3);                              // and a0, a0, t3
  c->lqc2(vf14, 352, v1);                           // lqc2 vf14, 352(v1)
  c->vadd(DEST::xyz, vf4, vf23, vf15);              // vadd.xyz vf4, vf23, vf15
  c->sll(t3, t2, 4);                                // sll t3, t2, 4
  c->vsub(DEST::xyzw, vf1, vf25, vf23);             // vsub.xyzw vf1, vf25, vf23
  c->daddiu(t4, t2, -10);                           // daddiu t4, t2, -10
  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L113
  c->daddiu(t2, t2, -13);                           // daddiu t2, t2, -13
  if (bc) {goto block_222;}                         // branch non-likely

  bc = c->sgpr64(t2) != 0;                          // bne t2, r0, L114
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_223;}                         // branch non-likely


  block_222:
  c->daddiu(t0, t0, 16);                            // daddiu t0, t0, 16
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L91                                 // beq r0, r0, L91
  c->lqc2(vf23, 1120, t0);                          // lqc2 vf23, 1120(t0)
  goto block_159;                                   // branch always


  block_223:
  c->addu(t2, t3, a3);                              // addu t2, t3, a3
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf5, -96, t2);                            // lqc2 vf5, -96(t2)
  c->mov128_gpr_vf(t2, vf1);                        // qmfc2.i t2, vf1
  bc = ((s64)c->sgpr64(t2)) < 0;                    // bltz t2, L117
  c->vadd(DEST::xyz, vf6, vf5, vf16);               // vadd.xyz vf6, vf5, vf16
  if (bc) {goto block_234;}                         // branch non-likely

  c->sra(t2, t2, 31);                               // sra t2, t2, 31
  c->vadd(DEST::xyz, vf7, vf5, vf17);               // vadd.xyz vf7, vf5, vf17
  bc = ((s64)c->sgpr64(t2)) < 0;                    // bltz t2, L91
  c->vadd(DEST::xyz, vf8, vf5, vf18);               // vadd.xyz vf8, vf5, vf18
  if (bc) {goto block_159;}                         // branch non-likely

  c->vadd(DEST::xyz, vf1, vf23, vf0);               // vadd.xyz vf1, vf23, vf0
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyz, vf2, vf23, vf13);              // vadd.xyz vf2, vf23, vf13
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyz, vf3, vf23, vf14);              // vadd.xyz vf3, vf23, vf14
  c->sqc2(vf5, 672, v1);                            // sqc2 vf5, 672(v1)
  c->vadd(DEST::xyz, vf4, vf23, vf15);              // vadd.xyz vf4, vf23, vf15
  c->sqc2(vf6, 688, v1);                            // sqc2 vf6, 688(v1)
  c->vmul(DEST::xyzw, vf19, vf5, vf13);             // vmul.xyzw vf19, vf5, vf13
  c->sqc2(vf7, 704, v1);                            // sqc2 vf7, 704(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, 720, v1);                            // sqc2 vf8, 720(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf1, 544, v1);                            // sqc2 vf1, 544(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf2, 560, v1);                            // sqc2 vf2, 560(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf3, 576, v1);                            // sqc2 vf3, 576(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf4, 592, v1);                            // sqc2 vf4, 592(v1)
  c->lqc2(vf1, 544, v1);                            // lqc2 vf1, 544(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 560, v1);                            // lqc2 vf2, 560(v1)
  c->vmula_bc(DEST::xyzw, BC::w, vf31, vf0);        // vmulaw.xyzw acc, vf31, vf0
  c->lqc2(vf3, 576, v1);                            // lqc2 vf3, 576(v1)
  c->vmadda_bc(DEST::xyzw, BC::x, vf28, vf1);       // vmaddax.xyzw acc, vf28, vf1
  c->lqc2(vf4, 592, v1);                            // lqc2 vf4, 592(v1)
  c->vmadda_bc(DEST::xyzw, BC::y, vf29, vf1);       // vmadday.xyzw acc, vf29, vf1
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf1, vf30, vf1);   // vmaddz.xyzw vf1, vf30, vf1
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf31, vf0);        // vmulaw.xyzw acc, vf31, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf28, vf2);       // vmaddax.xyzw acc, vf28, vf2
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf29, vf2);       // vmadday.xyzw acc, vf29, vf2
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf2, vf30, vf2);   // vmaddz.xyzw vf2, vf30, vf2
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf31, vf0);        // vmulaw.xyzw acc, vf31, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf28, vf3);       // vmaddax.xyzw acc, vf28, vf3
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf29, vf3);       // vmadday.xyzw acc, vf29, vf3
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf3, vf30, vf3);   // vmaddz.xyzw vf3, vf30, vf3
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf31, vf0);        // vmulaw.xyzw acc, vf31, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf28, vf4);       // vmaddax.xyzw acc, vf28, vf4
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf29, vf4);       // vmadday.xyzw acc, vf29, vf4
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf4, vf30, vf4);   // vmaddz.xyzw vf4, vf30, vf4
  c->vdiv(vf25, BC::z, vf1, BC::w);                 // vdiv Q, vf25.z, vf1.w
  c->lq(t2, 32, v1);                                // lq t2, 32(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 48, v1);                                // lq t3, 48(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 0, a1);                                 // sq t2, 0(a1)
  // nop                                            // sll r0, r0, 0
  c->sq(t3, 16, a1);                                // sq t3, 16(a1)
  c->lqc2(vf5, 672, v1);                            // lqc2 vf5, 672(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf6, 688, v1);                            // lqc2 vf6, 688(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf7, 704, v1);                            // lqc2 vf7, 704(v1)
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf1, vf1);                    // vmulq.xyz vf1, vf1, Q
  c->sqc2(vf20, 32, a1);                            // sqc2 vf20, 32(a1)
  c->vmulq(DEST::xyz, vf5, vf5);                    // vmulq.xyz vf5, vf5, Q
  // nop                                            // sll r0, r0, 0
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  c->vdiv(vf25, BC::z, vf2, BC::w);                 // vdiv Q, vf25.z, vf2.w
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf8, 720, v1);                            // lqc2 vf8, 720(v1)
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyzw, vf1, vf1, vf27);              // vadd.xyzw vf1, vf1, vf27
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf9, 608, v1);                            // lqc2 vf9, 608(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf10, 624, v1);                           // lqc2 vf10, 624(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf11, 640, v1);                           // lqc2 vf11, 640(v1)
  // nop                                            // sll r0, r0, 0
  c->vftoi4_sat(DEST::xyzw, vf1, vf1);                  // vftoi4.xyzw vf1, vf1
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf2, vf2);                    // vmulq.xyz vf2, vf2, Q
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf6, vf6);                    // vmulq.xyz vf6, vf6, Q
  c->sqc2(vf5, 48, a1);                             // sqc2 vf5, 48(a1)
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  c->vdiv(vf25, BC::z, vf3, BC::w);                 // vdiv Q, vf25.z, vf3.w
  c->sqc2(vf9, 64, a1);                             // sqc2 vf9, 64(a1)
  c->lqc2(vf12, 656, v1);                           // lqc2 vf12, 656(v1)
  c->sqc2(vf1, 80, a1);                             // sqc2 vf1, 80(a1)
  c->vadd(DEST::xyzw, vf2, vf2, vf27);              // vadd.xyzw vf2, vf2, vf27
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->vftoi4_sat(DEST::xyzw, vf2, vf2);                  // vftoi4.xyzw vf2, vf2
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf3, vf3);                    // vmulq.xyz vf3, vf3, Q
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf7, vf7);                    // vmulq.xyz vf7, vf7, Q
  c->sqc2(vf6, 96, a1);                             // sqc2 vf6, 96(a1)
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  c->vdiv(vf25, BC::z, vf4, BC::w);                 // vdiv Q, vf25.z, vf4.w
  c->sqc2(vf10, 112, a1);                           // sqc2 vf10, 112(a1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf2, 128, a1);                            // sqc2 vf2, 128(a1)
  c->vadd(DEST::xyzw, vf3, vf3, vf27);              // vadd.xyzw vf3, vf3, vf27
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->vftoi4_sat(DEST::xyzw, vf3, vf3);                  // vftoi4.xyzw vf3, vf3
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf4, vf4);                    // vmulq.xyz vf4, vf4, Q
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf8, vf8);                    // vmulq.xyz vf8, vf8, Q
  c->sqc2(vf7, 144, a1);                            // sqc2 vf7, 144(a1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf11, 160, a1);                           // sqc2 vf11, 160(a1)
  c->vadd(DEST::xyzw, vf4, vf4, vf27);              // vadd.xyzw vf4, vf4, vf27
  c->sqc2(vf3, 176, a1);                            // sqc2 vf3, 176(a1)
  c->andi(t2, a0, 2);                               // andi t2, a0, 2
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L115
  c->andi(t2, a0, 64);                              // andi t2, a0, 64
  if (bc) {goto block_228;}                         // branch non-likely

  bc = c->sgpr64(t2) != 0;                          // bne t2, r0, L115
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_228;}                         // branch non-likely

  //beq r0, r0, L116                                // beq r0, r0, L116
  c->vadd_bc(DEST::x, BC::w, vf23, vf23, vf19);     // vaddw.x vf23, vf23, vf19
  goto block_229;                                   // branch always


  block_228:
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::w, vf23, vf23, vf14);     // vaddw.x vf23, vf23, vf14

  block_229:
  c->vftoi4_sat(DEST::xyzw, vf4, vf4);                  // vftoi4.xyzw vf4, vf4
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, 192, a1);                            // sqc2 vf8, 192(a1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf12, 208, a1);                           // sqc2 vf12, 208(a1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf4, 224, a1);                            // sqc2 vf4, 224(a1)
  // nop                                            // sll r0, r0, 0
  c->lw(t3, 80, a1);                                // lw t3, 80(a1)
  // nop                                            // sll r0, r0, 0
  c->lw(t2, 84, a1);                                // lw t2, 84(a1)
  c->ori(t4, r0, 36864);                            // ori t4, r0, 36864
  c->dsubu(t3, t3, t4);                             // dsubu t3, t3, t4
  c->lw(t4, 224, a1);                               // lw t4, 224(a1)
  c->ori(t5, r0, 36096);                            // ori t5, r0, 36096
  c->dsubu(t2, t2, t5);                             // dsubu t2, t2, t5
  c->lw(t5, 228, a1);                               // lw t5, 228(a1)
  bc = ((s64)c->sgpr64(t3)) > 0;                    // bgtz t3, L91
  c->daddiu(t3, t4, -28672);                        // daddiu t3, t4, -28672
  if (bc) {goto block_159;}                         // branch non-likely

  bc = ((s64)c->sgpr64(t2)) > 0;                    // bgtz t2, L91
  c->daddiu(t2, t5, -29440);                        // daddiu t2, t5, -29440
  if (bc) {goto block_159;}                         // branch non-likely

  bc = ((s64)c->sgpr64(t3)) < 0;                    // bltz t3, L91
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_159;}                         // branch non-likely

  bc = ((s64)c->sgpr64(t2)) < 0;                    // bltz t2, L91
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_159;}                         // branch non-likely

  //beq r0, r0, L91                                 // beq r0, r0, L91
  c->daddiu(a1, a1, 240);                           // daddiu a1, a1, 240
  goto block_159;                                   // branch always


  block_234:
  c->lw(v1, 2872, v1);                              // lw v1, 2872(v1)
  c->sw(a1, 4, v1);                                 // sw a1, 4(v1)
  c->lqc2(vf24, 12, a2);                            // lqc2 vf24, 12(a2)
  c->vsub(DEST::xyzw, vf23, vf23, vf24);            // vsub.xyzw vf23, vf23, vf24
  c->mov128_gpr_vf(v0, vf23);                       // qmfc2.i v0, vf23
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
  cache.font_work = intern_from_c("*font-work*").c();
  cache.font12_table = intern_from_c("*font12-table*").c();
  cache.font24_table = intern_from_c("*font24-table*").c();
  cache.math_camera = intern_from_c("*math-camera*").c();
  cache.video_params = intern_from_c("*video-params*").c();
  gLinkedFunctionTable.reg("draw-string-asm", execute, 512);
}

} // namespace draw_string_asm
} // namespace Mips2C

