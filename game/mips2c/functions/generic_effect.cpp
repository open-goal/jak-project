
//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/kscheme.h"
namespace Mips2C {
namespace generic_prepare_dma_double {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -128);                          // daddiu sp, sp, -128
  c->sd(ra, 12432, at);                             // sd ra, 12432(at)
  c->sq(s0, 12448, at);                             // sq s0, 12448(at)
  c->sq(s1, 12464, at);                             // sq s1, 12464(at)
  c->sq(s2, 12480, at);                             // sq s2, 12480(at)
  c->sq(s3, 12496, at);                             // sq s3, 12496(at)
  c->sq(s4, 12512, at);                             // sq s4, 12512(at)
  c->sq(s5, 12528, at);                             // sq s5, 12528(at)
  c->sq(gp, 12544, at);                             // sq gp, 12544(at)
  get_fake_spad_addr(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lw(a3, 60, at);                                // lw a3, 60(at)
  // nop                                            // sll r0, r0, 0
  c->lbu(v1, 16, a3);                               // lbu v1, 16(a3)
  // nop                                            // sll r0, r0, 0
  c->lh(a0, 18, a3);                                // lh a0, 18(a3)
  // nop                                            // sll r0, r0, 0
  c->lw(t7, 40, at);                                // lw t7, 40(at)
  c->sll(a2, v1, 2);                                // sll a2, v1, 2
  c->daddiu(a1, a0, 3);                             // daddiu a1, a0, 3
  c->daddu(a2, a2, v1);                             // daddu a2, a2, v1
  c->addiu(t0, r0, -4);                             // addiu t0, r0, -4
  c->and_(t6, a1, t0);                              // and t6, a1, t0
  c->sll(a1, a2, 4);                                // sll a1, a2, 4
  c->daddu(a2, t6, t6);                             // daddu a2, t6, t6
  c->daddiu(a1, a1, 112);                           // daddiu a1, a1, 112
  c->daddu(a2, a2, t6);                             // daddu a2, a2, t6
  c->dsll(t0, t6, 2);                               // dsll t0, t6, 2
  // nop                                            // sll r0, r0, 0
  c->daddiu(t0, t0, 15);                            // daddiu t0, t0, 15
  c->dsll(a2, a2, 2);                               // dsll a2, a2, 2
  c->dsra(t0, t0, 4);                               // dsra t0, t0, 4
  c->daddiu(a2, a2, 15);                            // daddiu a2, a2, 15
  c->dsll(t0, t0, 4);                               // dsll t0, t0, 4
  c->dsra(t9, a2, 4);                               // dsra t9, a2, 4
  c->dsll(t2, t9, 4);                               // dsll t2, t9, 4
  c->mov64(a2, t7);                                 // or a2, t7, r0
  c->dsra(t8, a1, 4);                               // dsra t8, a1, 4
  c->daddu(t1, a2, a1);                             // daddu t1, a2, a1
  // nop                                            // sll r0, r0, 0
  c->daddu(gp, t1, t2);                             // daddu gp, t1, t2
  // nop                                            // sll r0, r0, 0
  c->daddu(t2, gp, t0);                             // daddu t2, gp, t0
  // nop                                            // sll r0, r0, 0
  c->daddu(t3, t2, t0);                             // daddu t3, t2, t0
  // nop                                            // sll r0, r0, 0
  c->daddu(ra, t3, a1);                             // daddu ra, t3, a1
  // nop                                            // sll r0, r0, 0
  c->daddu(t4, ra, t0);                             // daddu t4, ra, t0
  // nop                                            // sll r0, r0, 0
  c->daddu(t5, t4, t0);                             // daddu t5, t4, t0
  c->daddiu(t0, t1, 32);                            // daddiu t0, t1, 32
  c->daddiu(t1, gp, 64);                            // daddiu t1, gp, 64
  c->daddiu(t2, t2, 80);                            // daddiu t2, t2, 80
  c->daddiu(a1, t3, 80);                            // daddiu a1, t3, 80
  c->daddiu(t3, ra, 112);                           // daddiu t3, ra, 112
  c->daddiu(t4, t4, 128);                           // daddiu t4, t4, 128
  c->daddiu(t5, t5, 128);                           // daddiu t5, t5, 128
  c->sq(r0, -16, t0);                               // sq r0, -16(t0)
  c->sq(r0, -32, t1);                               // sq r0, -32(t1)
  c->sq(r0, -16, t1);                               // sq r0, -16(t1)
  c->sq(r0, -32, t2);                               // sq r0, -32(t2)
  c->sq(r0, -16, t2);                               // sq r0, -16(t2)
  c->sq(r0, -16, a1);                               // sq r0, -16(a1)
  c->sq(r0, -16, t3);                               // sq r0, -16(t3)
  c->sq(r0, -32, t4);                               // sq r0, -32(t4)
  c->sq(r0, -16, t4);                               // sq r0, -16(t4)
  c->sq(r0, -16, t5);                               // sq r0, -16(t5)
  c->lq(ra, 11744, at);                             // lq ra, 11744(at)
  c->lq(gp, 11760, at);                             // lq gp, 11760(at)
  c->lq(s5, 11776, at);                             // lq s5, 11776(at)
  c->lq(s4, 11792, at);                             // lq s4, 11792(at)
  c->lw(s3, 11984, at);                             // lw s3, 11984(at)
  c->sq(ra, 0, a2);                                 // sq ra, 0(a2)
  c->sq(gp, 0, a1);                                 // sq gp, 0(a1)
  c->sq(s5, 0, t5);                                 // sq s5, 0(t5)
  c->sh(t9, 0, t5);                                 // sh t9, 0(t5)
  c->sq(s4, 16, t5);                                // sq s4, 16(t5)
  c->sw(s3, 24, t5);                                // sw s3, 24(t5)
  c->subu(t9, t5, t7);                              // subu t9, t5, t7
  c->sra(t9, t9, 4);                                // sra t9, t9, 4
  c->daddiu(t9, t9, -1);                            // daddiu t9, t9, -1
  c->sh(t9, 0, t7);                                 // sh t9, 0(t7)
  c->daddiu(t7, t9, 3);                             // daddiu t7, t9, 3
  c->sw(t7, 56, at);                                // sw t7, 56(at)
  c->lw(t7, 76, at);                                // lw t7, 76(at)
  c->dsubu(t9, t0, a2);                             // dsubu t9, t0, a2
  c->daddu(t7, t7, t9);                             // daddu t7, t7, t9
  // nop                                            // sll r0, r0, 0
  c->lw(t9, 12, a2);                                // lw t9, 12(a2)
  c->sll(ra, t8, 16);                               // sll ra, t8, 16
  c->lw(t8, 84, at);                                // lw t8, 84(at)
  // nop                                            // sll r0, r0, 0
  c->lw(gp, 12, a1);                                // lw gp, 12(a1)
  c->or_(s4, t9, t8);                               // or s4, t9, t8
  c->lw(t9, 88, at);                                // lw t9, 88(at)
  c->xori(s5, t8, 38);                              // xori s5, t8, 38
  // nop                                            // sll r0, r0, 0
  c->or_(s4, s4, ra);                               // or s4, s4, ra
  c->lw(t8, 11968, at);                             // lw t8, 11968(at)
  // nop                                            // sll r0, r0, 0
  c->lw(s3, 11988, at);                             // lw s3, 11988(at)
  c->or_(gp, gp, s5);                               // or gp, gp, s5
  c->sw(s4, 12, a2);                                // sw s4, 12(a2)
  c->or_(s5, gp, ra);                               // or s5, gp, ra
  c->lw(ra, 11972, at);                             // lw ra, 11972(at)
  c->daddiu(gp, t9, 1);                             // daddiu gp, t9, 1
  c->sw(s5, 12, a1);                                // sw s5, 12(a1)
  c->daddiu(s4, t9, 2);                             // daddiu s4, t9, 2
  c->lw(s3, 11976, at);                             // lw s3, 11976(at)
  c->dsll(t6, t6, 16);                              // dsll t6, t6, 16
  c->lw(s5, 11980, at);                             // lw s5, 11980(at)
  c->or_(s4, ra, s4);                               // or s4, ra, s4
  c->lw(ra, 11984, at);                             // lw ra, 11984(at)
  c->or_(s3, s3, gp);                               // or s3, s3, gp
  c->lw(gp, 11992, at);                             // lw gp, 11992(at)
  c->mov64(s2, t9);                                 // or s2, t9, r0
  c->sw(t8, -8, t0);                                // sw t8, -8(t0)
  c->or_(s5, s5, s2);                               // or s5, s5, s2
  c->sw(gp, 4, a1);                                 // sw gp, 4(a1)
  c->or_(s4, s4, t6);                               // or s4, s4, t6
  c->sw(ra, 0, a1);                                 // sw ra, 0(a1)
  c->or_(s3, s3, t6);                               // or s3, s3, t6
  c->sw(s4, -4, t0);                                // sw s4, -4(t0)
  c->or_(s5, s5, t6);                               // or s5, s5, t6
  c->sw(s3, -4, t1);                                // sw s3, -4(t1)
  c->addiu(s4, r0, 567);                            // addiu s4, r0, 567
  c->sw(s5, -4, t2);                                // sw s5, -4(t2)
  bc = c->sgpr64(t9) != c->sgpr64(s4);              // bne t9, s4, L59
  c->daddiu(t9, t9, 279);                           // daddiu t9, t9, 279
  if (bc) {goto block_2;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->addiu(t9, r0, 9);                              // addiu t9, r0, 9

  block_2:
  c->daddiu(s1, t9, 1);                             // daddiu s1, t9, 1
  c->lw(s0, 11976, at);                             // lw s0, 11976(at)
  c->mov64(s3, t9);                                 // or s3, t9, r0
  c->lw(s2, 11980, at);                             // lw s2, 11980(at)
  c->daddiu(s5, t9, 2);                             // daddiu s5, t9, 2
  c->lw(s4, 11972, at);                             // lw s4, 11972(at)
  c->or_(s1, s0, s1);                               // or s1, s0, s1
  c->sw(t8, -8, t3);                                // sw t8, -8(t3)
  c->or_(t8, s2, s3);                               // or t8, s2, s3
  c->sw(gp, 24, t5);                                // sw gp, 24(t5)
  c->or_(gp, s4, s5);                               // or gp, s4, s5
  // nop                                            // sll r0, r0, 0
  c->or_(s5, s1, t6);                               // or s5, s1, t6
  c->sw(ra, 28, t5);                                // sw ra, 28(t5)
  c->or_(t8, t8, t6);                               // or t8, t8, t6
  c->sw(s5, -4, t3);                                // sw s5, -4(t3)
  c->or_(ra, gp, t6);                               // or ra, gp, t6
  c->sw(t8, -4, t4);                                // sw t8, -4(t4)
  c->addiu(t6, r0, 567);                            // addiu t6, r0, 567
  c->sw(t7, 4, t5);                                 // sw t7, 4(t5)
  // nop                                            // sll r0, r0, 0
  c->sw(ra, 12, t5);                                // sw ra, 12(t5)
  bc = c->sgpr64(t9) != c->sgpr64(t6);              // bne t9, t6, L60
  c->daddiu(t5, t9, 279);                           // daddiu t5, t9, 279
  if (bc) {goto block_4;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->addiu(t5, r0, 9);                              // addiu t5, r0, 9

  block_4:
  // nop                                            // sll r0, r0, 0
  c->sw(t5, 88, at);                                // sw t5, 88(at)
  // nop                                            // sll r0, r0, 0
  c->sw(t0, 20, at);                                // sw t0, 20(at)
  // nop                                            // sll r0, r0, 0
  c->sw(t1, 24, at);                                // sw t1, 24(at)
  // nop                                            // sll r0, r0, 0
  c->sw(t2, 28, at);                                // sw t2, 28(at)
  // nop                                            // sll r0, r0, 0
  c->sw(t3, 32, at);                                // sw t3, 32(at)
  // nop                                            // sll r0, r0, 0
  c->sw(t4, 36, at);                                // sw t4, 36(at)
  // nop                                            // sll r0, r0, 0
  c->lw(t0, 64, at);                                // lw t0, 64(at)
  // nop                                            // sll r0, r0, 0
  c->lq(t1, 11808, at);                             // lq t1, 11808(at)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L63
  c->lq(t2, 11824, at);                             // lq t2, 11824(at)
  if (bc) {goto block_10;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lq(t3, 11840, at);                             // lq t3, 11840(at)
  // nop                                            // sll r0, r0, 0
  c->lq(t4, 11856, at);                             // lq t4, 11856(at)
  // nop                                            // sll r0, r0, 0
  c->sq(t1, 16, a2);                                // sq t1, 16(a2)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 32, a2);                                // sq t2, 32(a2)
  // nop                                            // sll r0, r0, 0
  c->sq(t3, 48, a2);                                // sq t3, 48(a2)
  // nop                                            // sll r0, r0, 0
  c->sq(t4, 64, a2);                                // sq t4, 64(a2)
  // nop                                            // sll r0, r0, 0
  c->sq(t1, 16, a1);                                // sq t1, 16(a1)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 32, a1);                                // sq t2, 32(a1)
  // nop                                            // sll r0, r0, 0
  c->sq(t3, 48, a1);                                // sq t3, 48(a1)
  // nop                                            // sll r0, r0, 0
  c->sq(t4, 64, a1);                                // sq t4, 64(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(t1, 11872, at);                             // lq t1, 11872(at)
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 11888, at);                             // lq t2, 11888(at)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 11936, at);                             // lq t3, 11936(at)
  // nop                                            // sll r0, r0, 0
  c->lq(t4, 12032, at);                             // lq t4, 12032(at)
  // nop                                            // sll r0, r0, 0
  c->sq(t1, 80, a2);                                // sq t1, 80(a2)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 96, a2);                                // sq t2, 96(a2)
  // nop                                            // sll r0, r0, 0
  c->sq(t3, 112, a2);                               // sq t3, 112(a2)
  // nop                                            // sll r0, r0, 0
  c->sq(t4, 80, a1);                                // sq t4, 80(a1)
  // nop                                            // sll r0, r0, 0
  c->sq(t3, 96, a1);                                // sq t3, 96(a1)
  // nop                                            // sll r0, r0, 0
  c->sq(t3, 112, a1);                               // sq t3, 112(a1)
  c->daddiu(t2, a3, 22);                            // daddiu t2, a3, 22
  c->addiu(t3, r0, 0);                              // addiu t3, r0, 0
  c->mov64(t4, v1);                                 // or t4, v1, r0
  c->addiu(t6, r0, 128);                            // addiu t6, r0, 128
  c->mov64(t1, a2);                                 // or t1, a2, r0
  // nop                                            // sll r0, r0, 0

  block_6:
  c->daddu(t1, t1, t6);                             // daddu t1, t1, t6
  c->lq(t6, 0, t0);                                 // lq t6, 0(t0)
  c->daddiu(t4, t4, -1);                            // daddiu t4, t4, -1
  c->lbu(t5, 0, t2);                                // lbu t5, 0(t2)
  // nop                                            // sll r0, r0, 0
  c->lq(t7, 16, t0);                                // lq t7, 16(t0)
  c->daddu(t9, t5, t5);                             // daddu t9, t5, t5
  c->lq(t8, 32, t0);                                // lq t8, 32(t0)
  c->daddu(ra, t9, t5);                             // daddu ra, t9, t5
  c->lq(t9, 48, t0);                                // lq t9, 48(t0)
  c->daddiu(gp, ra, 9);                             // daddiu gp, ra, 9
  c->lq(ra, 64, t0);                                // lq ra, 64(t0)
  c->daddiu(t0, t0, 80);                            // daddiu t0, t0, 80
  c->sq(t6, 0, t1);                                 // sq t6, 0(t1)
  // nop                                            // sll r0, r0, 0
  c->sw(t3, 12, t1);                                // sw t3, 12(t1)
  c->daddiu(t2, t2, 1);                             // daddiu t2, t2, 1
  c->sq(t7, 16, t1);                                // sq t7, 16(t1)
  c->daddu(t3, t3, gp);                             // daddu t3, t3, gp
  c->sw(t5, 28, t1);                                // sw t5, 28(t1)
  // nop                                            // sll r0, r0, 0
  c->sq(t8, 32, t1);                                // sq t8, 32(t1)
  c->addiu(t6, r0, 80);                             // addiu t6, r0, 80
  c->sq(t9, 48, t1);                                // sq t9, 48(t1)
  bc = ((s64)c->sgpr64(t4)) > 0;                    // bgtz t4, L61
  c->sq(ra, 64, t1);                                // sq ra, 64(t1)
  if (bc) {goto block_6;}                           // branch non-likely

  c->ori(t0, t5, 32768);                            // ori t0, t5, 32768
  c->sw(a0, 52, at);                                // sw a0, 52(at)
  // nop                                            // sll r0, r0, 0
  c->sw(t0, 28, t1);                                // sw t0, 28(t1)
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 92, a2);                                // sw v1, 92(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 108, a2);                               // sw a0, 108(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(r0, 124, a2);                               // sw r0, 124(a2)
  c->daddiu(a3, a3, 22);                            // daddiu a3, a3, 22
  c->addiu(t0, r0, 0);                              // addiu t0, r0, 0
  c->mov64(t1, v1);                                 // or t1, v1, r0
  c->lw(t6, 68, at);                                // lw t6, 68(at)
  c->mov64(a2, a1);                                 // or a2, a1, r0
  c->addiu(t8, r0, 128);                            // addiu t8, r0, 128
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 0, t6);                                 // lq t2, 0(t6)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 16, t6);                                // lq t3, 16(t6)
  // nop                                            // sll r0, r0, 0
  c->lq(t4, 32, t6);                                // lq t4, 32(t6)
  // nop                                            // sll r0, r0, 0
  c->lq(t5, 48, t6);                                // lq t5, 48(t6)
  // nop                                            // sll r0, r0, 0
  c->lq(t6, 64, t6);                                // lq t6, 64(t6)

  block_8:
  c->daddu(a2, a2, t8);                             // daddu a2, a2, t8
  c->lbu(t7, 0, a3);                                // lbu t7, 0(a3)
  c->daddiu(t1, t1, -1);                            // daddiu t1, t1, -1
  c->sq(t2, 0, a2);                                 // sq t2, 0(a2)
  c->daddu(t8, t7, t7);                             // daddu t8, t7, t7
  c->sw(t0, 12, a2);                                // sw t0, 12(a2)
  c->daddu(t8, t8, t7);                             // daddu t8, t8, t7
  c->sq(t3, 16, a2);                                // sq t3, 16(a2)
  c->daddiu(t8, t8, 9);                             // daddiu t8, t8, 9
  c->sw(t7, 28, a2);                                // sw t7, 28(a2)
  c->daddiu(a3, a3, 1);                             // daddiu a3, a3, 1
  // nop                                            // sll r0, r0, 0
  c->daddu(t0, t0, t8);                             // daddu t0, t0, t8
  c->sq(t4, 32, a2);                                // sq t4, 32(a2)
  c->addiu(t8, r0, 80);                             // addiu t8, r0, 80
  c->sq(t5, 48, a2);                                // sq t5, 48(a2)
  bc = ((s64)c->sgpr64(t1)) > 0;                    // bgtz t1, L62
  c->sq(t6, 64, a2);                                // sq t6, 64(a2)
  if (bc) {goto block_8;}                           // branch non-likely

  c->ori(a3, t7, 32768);                            // ori a3, t7, 32768
  c->sw(a0, 52, at);                                // sw a0, 52(at)
  // nop                                            // sll r0, r0, 0
  c->sw(a3, 28, a2);                                // sw a3, 28(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 92, a1);                                // sw v1, 92(a1)
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 108, a1);                               // sw a0, 108(a1)
  //beq r0, r0, L65                                 // beq r0, r0, L65
  c->sw(r0, 124, a1);                               // sw r0, 124(a1)
  goto block_13;                                    // branch always


  block_10:
  // nop                                            // sll r0, r0, 0
  c->lq(a3, 16, a2);                                // lq a3, 16(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t0, 32, a2);                                // lq t0, 32(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t1, 48, a2);                                // lq t1, 48(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 64, a2);                                // lq t2, 64(a2)
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 16, a1);                                // sq a3, 16(a1)
  // nop                                            // sll r0, r0, 0
  c->sq(t0, 32, a1);                                // sq t0, 32(a1)
  // nop                                            // sll r0, r0, 0
  c->sq(t1, 48, a1);                                // sq t1, 48(a1)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 64, a1);                                // sq t2, 64(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(a3, 12032, at);                             // lq a3, 12032(at)
  // nop                                            // sll r0, r0, 0
  c->lq(t0, 11936, at);                             // lq t0, 11936(at)
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 80, a1);                                // sq a3, 80(a1)
  // nop                                            // sll r0, r0, 0
  c->sq(t0, 96, a1);                                // sq t0, 96(a1)
  // nop                                            // sll r0, r0, 0
  c->sq(t0, 112, a1);                               // sq t0, 112(a1)
  c->mov64(a3, v1);                                 // or a3, v1, r0
  c->lw(t5, 68, at);                                // lw t5, 68(at)
  c->mov64(t0, a1);                                 // or t0, a1, r0
  c->addiu(t7, r0, 128);                            // addiu t7, r0, 128
  // nop                                            // sll r0, r0, 0
  c->lq(t1, 0, t5);                                 // lq t1, 0(t5)
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 16, t5);                                // lq t2, 16(t5)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 32, t5);                                // lq t3, 32(t5)
  // nop                                            // sll r0, r0, 0
  c->lq(t4, 48, t5);                                // lq t4, 48(t5)
  // nop                                            // sll r0, r0, 0
  c->lq(t5, 64, t5);                                // lq t5, 64(t5)
  c->daddu(a2, a2, t7);                             // daddu a2, a2, t7
  // nop                                            // sll r0, r0, 0

  block_11:
  c->daddu(t0, t0, t7);                             // daddu t0, t0, t7
  c->lwu(t8, 12, a2);                               // lwu t8, 12(a2)
  c->daddiu(a3, a3, -1);                            // daddiu a3, a3, -1
  c->lwu(t7, 28, a2);                               // lwu t7, 28(a2)
  c->daddiu(a2, a2, 80);                            // daddiu a2, a2, 80
  c->sq(t1, 0, t0);                                 // sq t1, 0(t0)
  // nop                                            // sll r0, r0, 0
  c->sw(t8, 12, t0);                                // sw t8, 12(t0)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 16, t0);                                // sq t2, 16(t0)
  c->daddu(t8, t8, t6);                             // daddu t8, t8, t6
  c->sw(t7, 28, t0);                                // sw t7, 28(t0)
  // nop                                            // sll r0, r0, 0
  c->sq(t3, 32, t0);                                // sq t3, 32(t0)
  c->addiu(t7, r0, 80);                             // addiu t7, r0, 80
  c->sq(t4, 48, t0);                                // sq t4, 48(t0)
  bc = ((s64)c->sgpr64(a3)) > 0;                    // bgtz a3, L64
  c->sq(t5, 64, t0);                                // sq t5, 64(t0)
  if (bc) {goto block_11;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->sw(a0, 52, at);                                // sw a0, 52(at)
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 92, a1);                                // sw v1, 92(a1)
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 108, a1);                               // sw a0, 108(a1)
  // nop                                            // sll r0, r0, 0
  c->sw(r0, 124, a1);                               // sw r0, 124(a1)

  block_13:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 12432, at);                             // ld ra, 12432(at)
  c->lq(gp, 12544, at);                             // lq gp, 12544(at)
  c->lq(s5, 12528, at);                             // lq s5, 12528(at)
  c->lq(s4, 12512, at);                             // lq s4, 12512(at)
  c->lq(s3, 12496, at);                             // lq s3, 12496(at)
  c->lq(s2, 12480, at);                             // lq s2, 12480(at)
  c->lq(s1, 12464, at);                             // lq s1, 12464(at)
  c->lq(s0, 12448, at);                             // lq s0, 12448(at)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 128);                           // daddiu sp, sp, 128
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("generic-prepare-dma-double", execute, 256);
}

} // namespace generic_prepare_dma_double
} // namespace Mips2C
