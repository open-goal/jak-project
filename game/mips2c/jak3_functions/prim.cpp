//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace method_9_prim_strip {
struct Cache {
  void* display; // *display*
  void* prim_work; // *prim-work*
  void* stdcon; // *stdcon*
  void* adgif_shader_texture_simple; // adgif-shader<-texture-simple!
  void* dma_bucket_insert_tag; // dma-bucket-insert-tag
  void* format; // format
  void* lookup_texture_by_id; // lookup-texture-by-id
  void* paused; // paused?
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -112);                          // daddiu sp, sp, -112
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  c->sq(s1, 16, sp);                                // sq s1, 16(sp)
  c->sq(s2, 32, sp);                                // sq s2, 32(sp)
  c->sq(s3, 48, sp);                                // sq s3, 48(sp)
  c->sq(s4, 64, sp);                                // sq s4, 64(sp)
  c->sq(s5, 80, sp);                                // sq s5, 80(sp)
  c->sq(gp, 96, sp);                                // sq gp, 96(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->mov64(s1, a1);                                 // or s1, a1, r0
  c->load_symbol2(v1, cache.display);               // lw v1, *display*(s7)
  c->lw(v1, 0, v1);                                 // lw v1, 0(v1)
  c->dsll(v1, v1, 2);                               // dsll v1, v1, 2
  c->load_symbol2(a0, cache.display);               // lw a0, *display*(s7)
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  c->lwu(v1, 8, v1);                                // lwu v1, 8(v1)
  c->lwu(v1, 36, v1);                               // lwu v1, 36(v1)
  c->lhu(a0, 8, gp);                                // lhu a0, 8(gp)
  c->daddiu(a0, a0, 79);                            // daddiu a0, a0, 79
  c->addiu(a1, r0, 80);                             // addiu a1, r0, 80
  // Unknown instr: divu a0, a1
  // Unknown instr: mflo a0
  c->gprs[a0].du64[0] = c->gprs[a0].du32[0] / c->gprs[a1].du32[0];
  c->addiu(a1, r0, 144);                            // addiu a1, r0, 144
  c->lhu(a2, 8, gp);                                // lhu a2, 8(gp)
  c->multu3(a1, a1, a2);                            // multu3 a1, a1, a2
  c->dsll(a0, a0, 7);                               // dsll a0, a0, 7
  c->daddu(a0, a1, a0);                             // daddu a0, a1, a0
  c->lwu(a1, 8, v1);                                // lwu a1, 8(v1)
  c->lwu(v1, 4, v1);                                // lwu v1, 4(v1)
  c->dsubu(v1, a1, v1);                             // dsubu v1, a1, v1
  c->sltu(v1, a0, v1);                              // sltu v1, a0, v1
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L23
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_28;}                          // branch non-likely

  c->load_symbol2(t9, cache.lookup_texture_by_id);  // lw t9, lookup-texture-by-id(s7)
  c->lwu(a0, 12, gp);                               // lwu a0, 12(gp)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(a1, v0);                                 // or a1, v0, r0
  c->load_symbol2(s3, cache.prim_work);             // lw s3, *prim-work*(s7)
  c->lwu(v1, 80, gp);                               // lwu v1, 80(gp)
  c->dsll(v1, v1, 4);                               // dsll v1, v1, 4
  c->daddiu(v1, v1, 236);                           // daddiu v1, v1, 236
  c->daddu(s2, v1, s3);                             // daddu s2, v1, s3
  c->load_symbol2(v1, cache.display);               // lw v1, *display*(s7)
  c->lw(v1, 0, v1);                                 // lw v1, 0(v1)
  c->dsll(v1, v1, 2);                               // dsll v1, v1, 2
  c->load_symbol2(a0, cache.display);               // lw a0, *display*(s7)
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  c->lwu(v1, 8, v1);                                // lwu v1, 8(v1)
  c->lwu(s4, 36, v1);                               // lwu s4, 36(v1)
  c->lwu(s5, 4, s4);                                // lwu s5, 4(s4)
  c->lhu(v1, 8, gp);                                // lhu v1, 8(gp)
  c->sw(v1, 224, s3);                               // sw v1, 224(s3)
  c->daddiu(v1, gp, 92);                            // daddiu v1, gp, 92
  c->sw(v1, 232, s3);                               // sw v1, 232(s3)
  c->lwu(v1, 0, gp);                                // lwu v1, 0(gp)
  c->andi(v1, v1, 1);                               // andi v1, v1, 1
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L8
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_3;}                           // branch non-likely

  c->addiu(v1, r0, 1);                              // addiu v1, r0, 1
  //beq r0, r0, L9                                  // beq r0, r0, L9
  // nop                                            // sll r0, r0, 0
  goto block_4;                                     // branch always


block_3:
  c->addiu(v1, r0, 0);                              // addiu v1, r0, 0

block_4:
  c->lwu(a0, 0, gp);                                // lwu a0, 0(gp)
  c->andi(a0, a0, 2);                               // andi a0, a0, 2
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L10
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_6;}                           // branch non-likely

  c->addiu(a0, r0, 1);                              // addiu a0, r0, 1
  //beq r0, r0, L11                                 // beq r0, r0, L11
  // nop                                            // sll r0, r0, 0
  goto block_7;                                     // branch always


block_6:
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0

block_7:
  c->lwu(a2, 0, gp);                                // lwu a2, 0(gp)
  c->andi(a2, a2, 4);                               // andi a2, a2, 4
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L12
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_9;}                           // branch non-likely

  c->addiu(a2, r0, 1);                              // addiu a2, r0, 1
  //beq r0, r0, L13                                 // beq r0, r0, L13
  // nop                                            // sll r0, r0, 0
  goto block_10;                                    // branch always


block_9:
  c->addiu(a2, r0, 0);                              // addiu a2, r0, 0

block_10:
  c->lui(a3, 12288);                                // lui a3, 12288
  c->ori(a3, a3, 16384);                            // ori a3, a3, 16384
  c->dsll32(t0, a2, 31);                            // dsll32 t0, a2, 31
  c->dsrl32(t0, t0, 26);                            // dsrl32 t0, t0, 26
  c->ori(t0, t0, 13);                               // ori t0, t0, 13
  c->dsll32(t1, a0, 31);                            // dsll32 t1, a0, 31
  c->dsrl32(t1, t1, 27);                            // dsrl32 t1, t1, 27
  c->or_(t0, t0, t1);                               // or t0, t0, t1
  c->dsll32(t1, v1, 31);                            // dsll32 t1, v1, 31
  c->dsrl32(t1, t1, 25);                            // dsrl32 t1, t1, 25
  c->or_(t0, t0, t1);                               // or t0, t0, t1
  c->dsll32(t0, t0, 21);                            // dsll32 t0, t0, 21
  c->dsrl32(t0, t0, 6);                             // dsrl32 t0, t0, 6
  c->or_(a3, a3, t0);                               // or a3, a3, t0
  c->sw(a3, 80, s3);                                // sw a3, 80(s3)
  c->lui(a3, 12288);                                // lui a3, 12288
  c->ori(a3, a3, 16384);                            // ori a3, a3, 16384
  c->dsll32(a2, a2, 31);                            // dsll32 a2, a2, 31
  c->dsrl32(a2, a2, 26);                            // dsrl32 a2, a2, 26
  c->ori(a2, a2, 12);                               // ori a2, a2, 12
  c->dsll32(a0, a0, 31);                            // dsll32 a0, a0, 31
  c->dsrl32(a0, a0, 27);                            // dsrl32 a0, a0, 27
  c->or_(a0, a2, a0);                               // or a0, a2, a0
  c->dsll32(v1, v1, 31);                            // dsll32 v1, v1, 31
  c->dsrl32(v1, v1, 25);                            // dsrl32 v1, v1, 25
  c->or_(v1, a0, v1);                               // or v1, a0, v1
  c->dsll32(v1, v1, 21);                            // dsll32 v1, v1, 21
  c->dsrl32(v1, v1, 6);                             // dsrl32 v1, v1, 6
  c->or_(v1, a3, v1);                               // or v1, a3, v1
  c->sw(v1, 84, s3);                                // sw v1, 84(s3)
  bc = c->sgpr64(s7) == c->sgpr64(a1);              // beq s7, a1, L19
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_20;}                          // branch non-likely

  c->lwu(v1, 84, gp);                               // lwu v1, 84(gp)
  c->lwu(a0, 88, gp);                               // lwu a0, 88(gp)
  c->lwu(a0, 88, gp);                               // lwu a0, 88(gp)
  c->daddiu(a0, a0, 12);                            // daddiu a0, a0, 12
  c->addiu(a2, r0, 0);                              // addiu a2, r0, 0
  //beq r0, r0, L15                                 // beq r0, r0, L15
  // nop                                            // sll r0, r0, 0
  goto block_13;                                    // branch always


block_12:
  c->dsll(a3, a0, 4);                               // dsll a3, a0, 4
  c->daddu(a3, v1, a3);                             // daddu a3, v1, a3
  c->lq(a3, 4828, a3);                              // lq a3, 4828(a3)
  c->dsll(t0, a2, 4);                               // dsll t0, a2, 4
  c->daddu(t0, a1, t0);                             // daddu t0, a1, t0
  c->lq(t0, 60, t0);                                // lq t0, 60(t0)
  c->por(a3, a3, t0);                               // por a3, a3, t0
  c->dsll(t0, a0, 4);                               // dsll t0, a0, 4
  c->daddu(t0, v1, t0);                             // daddu t0, v1, t0
  c->sq(a3, 4828, t0);                              // sq a3, 4828(t0)
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1

block_13:
  c->slti(a3, a2, 3);                               // slti a3, a2, 3
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L14
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->load_symbol2(t9, cache.adgif_shader_texture_simple);// lw t9, adgif-shader<-texture-simple!(s7)
  c->daddiu(a0, s3, 128);                           // daddiu a0, s3, 128
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->ld(v1, 60, gp);                                // ld v1, 60(gp)
  c->sd(v1, 176, s3);                               // sd v1, 176(s3)
  c->addiu(v1, r0, 8);                              // addiu v1, r0, 8
  c->sd(v1, 184, s3);                               // sd v1, 184(s3)
  c->ld(v1, 68, gp);                                // ld v1, 68(gp)
  c->sd(v1, 192, s3);                               // sd v1, 192(s3)
  c->addiu(v1, r0, 66);                             // addiu v1, r0, 66
  c->sb(v1, 200, s3);                               // sb v1, 200(s3)
  c->lqc2(vf1, 0, s1);                              // lqc2 vf1, 0(s1)
  c->lqc2(vf2, 16, s1);                             // lqc2 vf2, 16(s1)
  c->lqc2(vf3, 32, s1);                             // lqc2 vf3, 32(s1)
  c->lqc2(vf4, 48, s1);                             // lqc2 vf4, 48(s1)
  c->lqc2(vf5, 128, s3);                            // lqc2 vf5, 128(s3)
  c->lqc2(vf6, 144, s3);                            // lqc2 vf6, 144(s3)
  c->lqc2(vf7, 160, s3);                            // lqc2 vf7, 160(s3)
  c->lqc2(vf8, 176, s3);                            // lqc2 vf8, 176(s3)
  c->lqc2(vf9, 192, s3);                            // lqc2 vf9, 192(s3)
  c->lqc2(vf10, 80, s3);                            // lqc2 vf10, 80(s3)
  c->lqc2(vf11, 28, gp);                            // lqc2 vf11, 28(gp)
  c->lqc2(vf12, 44, gp);                            // lqc2 vf12, 44(gp)
  //beq r0, r0, L18                                 // beq r0, r0, L18
  // nop                                            // sll r0, r0, 0
  goto block_18;                                    // branch always


block_15:
  c->addiu(v1, r0, 80);                             // addiu v1, r0, 80
  c->lw(a0, 224, s3);                               // lw a0, 224(s3)
  c->slt(a1, v1, a0);                               // slt a1, v1, a0
  c->movz(v1, a0, a1);                              // movz v1, a0, a1
  c->sw(v1, 228, s3);                               // sw v1, 228(s3)
  c->lbu(v1, 1, s2);                                // lbu v1, 1(s2)
  c->andi(v1, v1, 1);                               // andi v1, v1, 1
  c->dsll(v1, v1, 4);                               // dsll v1, v1, 4
  c->daddiu(v1, v1, 48);                            // daddiu v1, v1, 48
  c->daddu(a0, v1, s3);                             // daddu a0, v1, s3
  c->lw(v1, 4, s4);                                 // lw v1, 4(s4)
  c->lq(a0, 0, a0);                                 // lq a0, 0(a0)
  c->daddiu(a1, v1, 16);                            // daddiu a1, v1, 16
  c->lw(v1, 228, s3);                               // lw v1, 228(s3)
  c->sq(a0, -16, a1);                               // sq a0, -16(a1)
  c->sqc2(vf1, 0, a1);                              // sqc2 vf1, 0(a1)
  c->sqc2(vf2, 16, a1);                             // sqc2 vf2, 16(a1)
  c->sqc2(vf3, 32, a1);                             // sqc2 vf3, 32(a1)
  c->sqc2(vf4, 48, a1);                             // sqc2 vf4, 48(a1)
  c->sqc2(vf10, 64, a1);                            // sqc2 vf10, 64(a1)
  c->sqc2(vf11, 80, a1);                            // sqc2 vf11, 80(a1)
  c->sqc2(vf12, 96, a1);                            // sqc2 vf12, 96(a1)
  c->sw(v1, 92, a1);                                // sw v1, 92(a1)
  c->sqc2(vf5, 112, a1);                            // sqc2 vf5, 112(a1)
  c->sqc2(vf6, 128, a1);                            // sqc2 vf6, 128(a1)
  c->sqc2(vf7, 144, a1);                            // sqc2 vf7, 144(a1)
  c->sqc2(vf8, 160, a1);                            // sqc2 vf8, 160(a1)
  c->sqc2(vf9, 176, a1);                            // sqc2 vf9, 176(a1)
  c->ori(a0, v1, 32768);                            // ori a0, v1, 32768
  c->sw(a0, 140, a1);                               // sw a0, 140(a1)
  c->daddiu(a0, a1, 192);                           // daddiu a0, a1, 192
  c->sw(a0, 4, s4);                                 // sw a0, 4(s4)
  c->lbu(a0, 0, s2);                                // lbu a0, 0(s2)
  c->addiu(a1, r0, 3);                              // addiu a1, r0, 3
  // Unknown instr: divu a0, a1
  // Unknown instr: mfhi a0
  c->gprs[a0].du64[0] = c->gprs[a0].du32[0] % c->gprs[a1].du32[0];
  c->dsll(a0, a0, 4);                               // dsll a0, a0, 4
  c->daddu(a0, r0, a0);                             // daddu a0, r0, a0
  c->daddu(a0, a0, s3);                             // daddu a0, a0, s3
  c->addiu(a1, r0, 3);                              // addiu a1, r0, 3
  c->mult3(a1, a1, v1);                             // mult3 a1, a1, v1
  c->ld(a2, 0, a0);                                 // ld a2, 0(a0)
  c->lui(a3, -1);                                   // lui a3, -1
  c->and_(a2, a2, a3);                              // and a2, a2, a3
  c->dsll32(a3, a1, 16);                            // dsll32 a3, a1, 16
  c->dsrl32(a3, a3, 16);                            // dsrl32 a3, a3, 16
  c->or_(a2, a2, a3);                               // or a2, a2, a3
  c->sd(a2, 0, a0);                                 // sd a2, 0(a0)
  c->lwu(a2, 12, a0);                               // lwu a2, 12(a0)
  c->lui(a3, -256);                                 // lui a3, -256
  c->ori(a3, a3, 65535);                            // ori a3, a3, 65535
  c->and_(a2, a2, a3);                              // and a2, a2, a3
  c->dsll32(a1, a1, 24);                            // dsll32 a1, a1, 24
  c->dsrl32(a1, a1, 8);                             // dsrl32 a1, a1, 8
  c->or_(a1, a2, a1);                               // or a1, a2, a1
  c->sw(a1, 12, a0);                                // sw a1, 12(a0)
  c->lw(a2, 4, s4);                                 // lw a2, 4(s4)
  c->lq(a3, 0, a0);                                 // lq a3, 0(a0)
  c->lq(a0, 208, s3);                               // lq a0, 208(s3)
  c->lw(a1, 232, s3);                               // lw a1, 232(s3)
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 0, a2);                                 // sq a3, 0(a2)
  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->sw(a2, 4, s4);                                 // sw a2, 4(s4)
  // nop                                            // sll r0, r0, 0

block_16:
  // nop                                            // sll r0, r0, 0
  c->lw(t0, 12, a1);                                // lw t0, 12(a1)
  // nop                                            // sll r0, r0, 0
  c->lw(a3, 44, a1);                                // lw a3, 44(a1)
  c->pextlb(t0, r0, t0);                            // pextlb t0, r0, t0
  c->lqc2(vf15, 0, a1);                             // lqc2 vf15, 0(a1)
  c->pextlh(t0, r0, t0);                            // pextlh t0, r0, t0
  c->lqc2(vf16, 32, a1);                            // lqc2 vf16, 32(a1)
  c->pextlb(a3, r0, a3);                            // pextlb a3, r0, a3
  c->lqc2(vf13, 16, a1);                            // lqc2 vf13, 16(a1)
  c->pextlh(a3, r0, a3);                            // pextlh a3, r0, a3
  c->lqc2(vf14, 48, a1);                            // lqc2 vf14, 48(a1)
  c->vftoi12(DEST::xyzw, vf15, vf15);               // vftoi12.xyzw vf15, vf15
  c->sq(t0, 16, a2);                                // sq t0, 16(a2)
  c->vftoi12(DEST::xyzw, vf16, vf16);               // vftoi12.xyzw vf16, vf16
  c->sq(a3, 64, a2);                                // sq a3, 64(a2)
  // nop                                            // sll r0, r0, 0
  c->lw(t0, 8, a1);                                 // lw t0, 8(a1)
  // nop                                            // sll r0, r0, 0
  c->lw(a3, 40, a1);                                // lw a3, 40(a1)
  c->daddiu(a1, a1, 64);                            // daddiu a1, a1, 64
  c->mov128_gpr_vf(t2, vf15);                       // qmfc2.i t2, vf15
  c->daddiu(v1, v1, -2);                            // daddiu v1, v1, -2
  c->mov128_gpr_vf(t1, vf16);                       // qmfc2.i t1, vf16
  c->pand(t2, t2, a0);                              // pand t2, t2, a0
  c->sqc2(vf13, 32, a2);                            // sqc2 vf13, 32(a2)
  c->pand(t1, t1, a0);                              // pand t1, t1, a0
  c->sqc2(vf14, 80, a2);                            // sqc2 vf14, 80(a2)
  c->or_(t0, t2, t0);                               // or t0, t2, t0
  // nop                                            // sll r0, r0, 0
  c->or_(a3, t1, a3);                               // or a3, t1, a3
  // nop                                            // sll r0, r0, 0
  c->sq(t0, 0, a2);                                 // sq t0, 0(a2)
  c->daddiu(a2, a2, 96);                            // daddiu a2, a2, 96
  bc = ((s64)c->sgpr64(v1)) > 0;                    // bgtz v1, L17
  c->sq(a3, -48, a2);                               // sq a3, -48(a2)
  if (bc) {goto block_16;}                          // branch non-likely

  c->lwu(v1, 4, s4);                                // lwu v1, 4(s4)
  c->addiu(a0, r0, 48);                             // addiu a0, r0, 48
  c->lw(a1, 228, s3);                               // lw a1, 228(s3)
  c->mult3(a0, a0, a1);                             // mult3 a0, a0, a1
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  c->sw(v1, 4, s4);                                 // sw v1, 4(s4)
  c->lwu(v1, 4, s4);                                // lwu v1, 4(s4)
  c->lq(a0, 96, s3);                                // lq a0, 96(s3)
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->lwu(v1, 4, s4);                                // lwu v1, 4(s4)
  c->daddiu(v1, v1, 16);                            // daddiu v1, v1, 16
  c->sw(v1, 4, s4);                                 // sw v1, 4(s4)
  c->lw(v1, 224, s3);                               // lw v1, 224(s3)
  c->daddiu(v1, v1, -78);                           // daddiu v1, v1, -78
  c->sw(v1, 224, s3);                               // sw v1, 224(s3)
  c->lwu(v1, 232, s3);                              // lwu v1, 232(s3)
  c->daddiu(v1, v1, 2496);                          // daddiu v1, v1, 2496
  c->sw(v1, 232, s3);                               // sw v1, 232(s3)
  c->lbu(v1, 0, s2);                                // lbu v1, 0(s2)
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  c->sb(v1, 0, s2);                                 // sb v1, 0(s2)
  c->lbu(v1, 1, s2);                                // lbu v1, 1(s2)
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  c->sb(v1, 1, s2);                                 // sb v1, 1(s2)

block_18:
  c->addiu(v1, r0, 2);                              // addiu v1, r0, 2
  c->lw(a0, 224, s3);                               // lw a0, 224(s3)
  c->slt(v1, v1, a0);                               // slt v1, v1, a0
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L16
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_15;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0

block_20:
  c->load_symbol2(t9, cache.paused);                // lw t9, paused?(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  bc = c->sgpr64(s7) != c->sgpr64(v0);              // bne s7, v0, L21
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_25;}                          // branch non-likely

  c->lwu(v1, 0, gp);                                // lwu v1, 0(gp)
  c->andi(v1, v1, 8);                               // andi v1, v1, 8
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L21
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_25;}                          // branch non-likely

  c->lwu(v1, 0, gp);                                // lwu v1, 0(gp)
  c->andi(v1, v1, 16);                              // andi v1, v1, 16
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L20
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_24;}                          // branch non-likely

  c->sh(r0, 8, gp);                                 // sh r0, 8(gp)
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0

block_24:
  c->addiu(v1, r0, -17);                            // addiu v1, r0, -17
  c->lwu(a0, 0, gp);                                // lwu a0, 0(gp)
  c->and_(v1, v1, a0);                              // and v1, v1, a0
  c->sw(v1, 0, gp);                                 // sw v1, 0(gp)

block_25:
  c->lwu(a3, 4, s4);                                // lwu a3, 4(s4)
  bc = c->sgpr64(s5) == c->sgpr64(a3);              // beq s5, a3, L22
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_27;}                          // branch non-likely

  c->lwu(v1, 4, s4);                                // lwu v1, 4(s4)
  c->lui(a0, 8192);                                 // lui a0, 8192
  c->sd(a0, 0, v1);                                 // sd a0, 0(v1)
  c->sw(r0, 8, v1);                                 // sw r0, 8(v1)
  c->sw(r0, 12, v1);                                // sw r0, 12(v1)
  c->daddiu(v1, v1, 16);                            // daddiu v1, v1, 16
  c->sw(v1, 4, s4);                                 // sw v1, 4(s4)
  c->load_symbol2(t9, cache.dma_bucket_insert_tag); // lw t9, dma-bucket-insert-tag(s7)
  c->load_symbol2(v1, cache.display);               // lw v1, *display*(s7)
  c->lw(v1, 0, v1);                                 // lw v1, 0(v1)
  c->dsll(v1, v1, 2);                               // dsll v1, v1, 2
  c->load_symbol2(a0, cache.display);               // lw a0, *display*(s7)
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  c->lwu(v1, 8, v1);                                // lwu v1, 8(v1)
  c->lwu(a0, 40, v1);                               // lwu a0, 40(v1)
  c->lw(a1, 76, gp);                                // lw a1, 76(gp)
  c->mov64(a2, s5);                                 // or a2, s5, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0

block_27:
  //beq r0, r0, L24                                 // beq r0, r0, L24
  // nop                                            // sll r0, r0, 0
  goto block_29;                                    // branch always


block_28:
  c->load_symbol2(t9, cache.format);                // lw t9, format(s7)
  c->load_symbol2(a0, cache.stdcon);                // lw a0, *stdcon*(s7)
  // daddiu a1, fp, L29                                // daddiu a1, fp, L29
  ASSERT_NOT_REACHED(); // ran out of memory error print.
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0

block_29:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
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
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.display = intern_from_c(-1, 0, "*display*").c();
  cache.prim_work = intern_from_c(-1, 0, "*prim-work*").c();
  cache.stdcon = intern_from_c(-1, 0, "*stdcon*").c();
  cache.adgif_shader_texture_simple = intern_from_c(-1, 0, "adgif-shader<-texture-simple!").c();
  cache.dma_bucket_insert_tag = intern_from_c(-1, 0, "dma-bucket-insert-tag").c();
  cache.format = intern_from_c(-1, 0, "format").c();
  cache.lookup_texture_by_id = intern_from_c(-1, 0, "lookup-texture-by-id").c();
  cache.paused = intern_from_c(-1, 0, "paused?").c();
  gLinkedFunctionTable.reg("(method 9 prim-strip)", execute, 256);
}

} // namespace method_9_prim_strip
} // namespace Mips2C
// add method_9_prim_strip::link to the link callback table for the object file.
// FWD DEC:
