//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak1/kscheme.h"
using namespace jak1;
namespace Mips2C::jak1 {
namespace adgif_shader_texture_with_update {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->ld(a2, 16, a0);                                // ld a2, 16(a0)
  c->addiu(v1, r0, 256);                            // addiu v1, r0, 256
  c->andi(a2, a2, 513);                             // andi a2, a2, 513
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->cvtsw(f0, f0);                                 // cvt.s.w f0, f0
  c->lbu(v1, 4, a1);                                // lbu v1, 4(a1)
  c->lwc1(f1, 44, a1);                              // lwc1 f1, 44(a1)
  c->daddiu(v1, v1, -1);                            // daddiu v1, v1, -1
  c->divs(f0, f0, f1);                              // div.s f0, f0, f1
  c->dsll(v1, v1, 2);                               // dsll v1, v1, 2
  c->or_(a2, a2, v1);                               // or a2, a2, v1
  c->lbu(v1, 7, a1);                                // lbu v1, 7(a1)
  c->dsll(v1, v1, 19);                              // dsll v1, v1, 19
  c->lbu(a3, 5, a1);                                // lbu a3, 5(a1)
  c->or_(a2, a2, v1);                               // or a2, a2, v1
  c->dsll(a3, a3, 5);                               // dsll a3, a3, 5
  c->or_(a2, a2, a3);                               // or a2, a2, a3
  c->ld(t1, 0, a0);                                 // ld t1, 0(a0)
  c->dsll(t1, t1, 27);                              // dsll t1, t1, 27
  c->lbu(v1, 6, a1);                                // lbu v1, 6(a1)
  c->dsra32(t1, t1, 30);                            // dsra32 t1, t1, 30
  c->dsll(v1, v1, 20);                              // dsll v1, v1, 20
  c->dsll32(t1, t1, 3);                             // dsll32 t1, t1, 3
  c->lhu(a3, 10, a1);                               // lhu a3, 10(a1)
  c->or_(t1, t1, v1);                               // or t1, t1, v1
  c->lbu(v1, 26, a1);                               // lbu v1, 26(a1)
  c->or_(t1, t1, a3);                               // or t1, t1, a3
  c->dsll(v1, v1, 14);                              // dsll v1, v1, 14
  c->or_(t1, t1, v1);                               // or t1, t1, v1
  c->lhu(v1, 0, a1);                                // lhu v1, 0(a1)
  c->plzcw(v1, v1);                                 // plzcw v1, v1
  c->addiu(t0, r0, 30);                             // addiu t0, r0, 30
  c->subu(v1, t0, v1);                              // subu v1, t0, v1
  c->lhu(a3, 2, a1);                                // lhu a3, 2(a1)
  c->dsll(v1, v1, 26);                              // dsll v1, v1, 26
  c->plzcw(a3, a3);                                 // plzcw a3, a3
  c->or_(t1, t1, v1);                               // or t1, t1, v1
  c->subu(a3, t0, a3);                              // subu a3, t0, a3
  c->dsll(a3, a3, 30);                              // dsll a3, a3, 30
  c->addiu(v1, r0, 1);                              // addiu v1, r0, 1
  c->or_(t1, t1, a3);                               // or t1, t1, a3
  c->dsll32(v1, v1, 2);                             // dsll32 v1, v1, 2
  c->or_(t1, t1, v1);                               // or t1, t1, v1
  c->lhu(v1, 24, a1);                               // lhu v1, 24(a1)
  c->dsll32(v1, v1, 5);                             // dsll32 v1, v1, 5
  c->lhu(a3, 8, a1);                                // lhu a3, 8(a1)
  c->dsll32(a3, a3, 19);                            // dsll32 a3, a3, 19
  c->or_(t1, t1, v1);                               // or t1, t1, v1
  c->or_(t1, t1, a3);                               // or t1, t1, a3
  c->addiu(v1, r0, 1);                              // addiu v1, r0, 1
  c->dsll32(v1, v1, 29);                            // dsll32 v1, v1, 29
  c->cvtws(f0, f0);                                 // cvt.w.s f0, f0
  c->or_(t1, t1, v1);                               // or t1, t1, v1
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->sd(t1, 0, a0);                                 // sd t1, 0(a0)
  c->plzcw(a3, v1);                                 // plzcw a3, v1
  c->subu(a3, t0, a3);                              // subu a3, t0, a3
  c->lbu(t0, 7, a1);                                // lbu t0, 7(a1)
  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L22
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  c->daddiu(t0, a3, -4);                            // daddiu t0, a3, -4
  c->dsll(a3, a3, 4);                               // dsll a3, a3, 4
  c->dsrav(t0, v1, t0);                             // dsrav t0, v1, t0
  c->daddiu(a3, a3, -175);                          // daddiu a3, a3, -175
  c->andi(t0, t0, 15);                              // andi t0, t0, 15
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L23                                 // beq r0, r0, L23
  c->daddu(a3, a3, t0);                             // daddu a3, a3, t0
  goto block_3;                                     // branch always


  block_2:
  c->daddiu(t0, a3, -5);                            // daddiu t0, a3, -5
  c->dsll(a3, a3, 5);                               // dsll a3, a3, 5
  c->dsrav(t0, v1, t0);                             // dsrav t0, v1, t0
  c->daddiu(a3, a3, -350);                          // daddiu a3, a3, -350
  c->andi(t0, t0, 31);                              // andi t0, t0, 31
  // nop                                            // sll r0, r0, 0
  c->daddu(a3, a3, t0);                             // daddu a3, a3, t0
  // nop                                            // sll r0, r0, 0

  block_3:
  c->andi(a3, a3, 4095);                            // andi a3, a3, 4095
  c->lhu(t1, 12, a1);                               // lhu t1, 12(a1)
  c->dsll32(a3, a3, 0);                             // dsll32 a3, a3, 0
  c->lbu(v1, 27, a1);                               // lbu v1, 27(a1)
  c->or_(a2, a2, a3);                               // or a2, a2, a3
  c->dsll(v1, v1, 14);                              // dsll v1, v1, 14
  c->sd(a2, 16, a0);                                // sd a2, 16(a0)
  c->or_(a2, t1, v1);                               // or a2, t1, v1
  c->lhu(v1, 14, a1);                               // lhu v1, 14(a1)
  // nop                                            // sll r0, r0, 0
  c->lbu(a3, 28, a1);                               // lbu a3, 28(a1)
  c->dsll(v1, v1, 20);                              // dsll v1, v1, 20
  c->or_(a2, a2, v1);                               // or a2, a2, v1
  c->dsll32(a3, a3, 2);                             // dsll32 a3, a3, 2
  c->or_(a2, a2, a3);                               // or a2, a2, a3
  c->lhu(v1, 16, a1);                               // lhu v1, 16(a1)
  c->lbu(a3, 29, a1);                               // lbu a3, 29(a1)
  c->dsll32(v1, v1, 8);                             // dsll32 v1, v1, 8
  c->or_(a2, a2, v1);                               // or a2, a2, v1
  c->dsll32(a3, a3, 22);                            // dsll32 a3, a3, 22
  c->or_(a2, a2, a3);                               // or a2, a2, a3
  c->lbu(t0, 4, a1);                                // lbu t0, 4(a1)
  c->daddiu(t0, t0, -5);                            // daddiu t0, t0, -5
  c->sd(a2, 32, a0);                                // sd a2, 32(a0)
  bc = ((s64)c->sgpr64(t0)) < 0;                    // bltz t0, L24
  c->lbu(a3, 30, a1);                               // lbu a3, 30(a1)
  if (bc) {goto block_5;}                           // branch non-likely

  c->lhu(a2, 18, a1);                               // lhu a2, 18(a1)
  c->dsll(a3, a3, 14);                              // dsll a3, a3, 14
  c->or_(a2, a2, a3);                               // or a2, a2, a3
  c->lhu(v1, 20, a1);                               // lhu v1, 20(a1)
  c->dsll(v1, v1, 20);                              // dsll v1, v1, 20
  c->lbu(a3, 31, a1);                               // lbu a3, 31(a1)
  c->or_(a2, a2, v1);                               // or a2, a2, v1
  c->dsll32(a3, a3, 2);                             // dsll32 a3, a3, 2
  c->or_(a2, a2, a3);                               // or a2, a2, a3
  c->lhu(v1, 22, a1);                               // lhu v1, 22(a1)
  c->dsll32(v1, v1, 8);                             // dsll32 v1, v1, 8
  c->lbu(a3, 32, a1);                               // lbu a3, 32(a1)
  c->or_(a2, a2, v1);                               // or a2, a2, v1
  c->dsll32(a3, a3, 22);                            // dsll32 a3, a3, 22
  c->or_(a2, a2, a3);                               // or a2, a2, a3
  c->addiu(v1, r0, 54);                             // addiu v1, r0, 54
  c->sd(a2, 64, a0);                                // sd a2, 64(a0)
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 72, a0);                                // sw v1, 72(a0)
  // nop                                            // sll r0, r0, 0

  block_5:
  c->mov64(v0, a0);                                 // or v0, a0, r0
  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  gLinkedFunctionTable.reg("adgif-shader<-texture-with-update!", execute, 128);
}

} // namespace adgif_shader<_texture_with_update
} // namespace Mips2C

