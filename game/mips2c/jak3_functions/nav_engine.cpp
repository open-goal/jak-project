//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace method_21_nav_engine {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->lwu(v1, 80, a0);                               // lwu v1, 80(a0)
  c->lwu(v1, 4, v1);                                // lwu v1, 4(v1)
  c->lwu(a2, 8, a0);                                // lwu a2, 8(a0)
  c->lwu(a2, 4, a2);                                // lwu a2, 4(a2)
  c->dsubu(v1, v1, a2);                             // dsubu v1, v1, a2
  c->addiu(a2, r0, 0);                              // addiu a2, r0, 0
  //beq r0, r0, L30                                 // beq r0, r0, L30
  // nop                                            // sll r0, r0, 0
  goto block_4;                                     // branch always


block_1:
  c->lwu(a3, 4, a1);                                // lwu a3, 4(a1)
  c->addiu(t0, r0, 288);                            // addiu t0, r0, 288
  c->mult3(t0, t0, a2);                             // mult3 t0, t0, a2
  c->daddu(a3, a3, t0);                             // daddu a3, a3, t0
  c->daddiu(t0, a3, 112);                           // daddiu t0, a3, 112
  c->lwu(t1, 8, a3);                                // lwu t1, 8(a3)
  bc = c->sgpr64(s7) == c->sgpr64(t1);              // beq s7, t1, L29
  c->mov64(t1, s7);                                 // or t1, s7, r0
  if (bc) {goto block_3;}                           // branch non-likely

  c->lwu(t1, 76, a0);                               // lwu t1, 76(a0)
  c->daddiu(t1, t1, 32);                            // daddiu t1, t1, 32
  c->sw(t1, 56, a3);                                // sw t1, 56(a3)
  c->lwu(t1, 80, a0);                               // lwu t1, 80(a0)
  c->sw(t1, 124, a3);                               // sw t1, 124(a3)
  c->lwu(t1, 0, a1);                                // lwu t1, 0(a1)
  c->addiu(t2, r0, 288);                            // addiu t2, r0, 288
  c->mult3(t2, t2, a2);                             // mult3 t2, t2, a2
  c->daddu(t1, t1, t2);                             // daddu t1, t1, t2
  c->sw(t1, 4, t0);                                 // sw t1, 4(t0)
  c->mov64(t1, v1);                                 // or t1, v1, r0
  c->lw(t2, 16, t0);                                // lw t2, 16(t0)
  c->daddu(t3, t2, t1);                             // daddu t3, t2, t1
  c->xor_(t4, t2, s7);                              // xor t4, t2, s7
  c->sltiu(t4, t4, 1);                              // sltiu t4, t4, 1
  c->movz(t2, t3, t4);                              // movz t2, t3, t4
  c->sw(t2, 16, t0);                                // sw t2, 16(t0)
  c->lw(t2, 28, t0);                                // lw t2, 28(t0)
  c->daddu(t3, t2, t1);                             // daddu t3, t2, t1
  c->xor_(t4, t2, s7);                              // xor t4, t2, s7
  c->sltiu(t4, t4, 1);                              // sltiu t4, t4, 1
  c->movz(t2, t3, t4);                              // movz t2, t3, t4
  c->sw(t2, 28, t0);                                // sw t2, 28(t0)
  c->lw(t2, 24, t0);                                // lw t2, 24(t0)
  c->daddu(t3, t2, t1);                             // daddu t3, t2, t1
  c->xor_(t4, t2, s7);                              // xor t4, t2, s7
  c->sltiu(t4, t4, 1);                              // sltiu t4, t4, 1
  c->movz(t2, t3, t4);                              // movz t2, t3, t4
  c->sw(t2, 24, t0);                                // sw t2, 24(t0)
  c->lw(t2, 20, t0);                                // lw t2, 20(t0)
  c->daddu(t1, t2, t1);                             // daddu t1, t2, t1
  c->xor_(t3, t2, s7);                              // xor t3, t2, s7
  c->sltiu(t3, t3, 1);                              // sltiu t3, t3, 1
  c->movz(t2, t1, t3);                              // movz t2, t1, t3
  c->sw(t2, 20, t0);                                // sw t2, 20(t0)
  c->gprs[t0].du64[0] = 0;                          // or t0, r0, r0
  c->lwu(t0, 0, a1);                                // lwu t0, 0(a1)
  c->addiu(t1, r0, 288);                            // addiu t1, r0, 288
  c->mult3(t1, t1, a2);                             // mult3 t1, t1, a2
  c->daddu(t1, t0, t1);                             // daddu t1, t0, t1
  c->lwu(a3, 8, a3);                                // lwu a3, 8(a3)
  c->sw(t1, 140, a3);                               // sw t1, 140(a3)

block_3:
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1

block_4:
  c->lb(a3, 14, a1);                                // lb a3, 14(a1)
  c->slt(a3, a2, a3);                               // slt a3, a2, a3
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L28
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
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
  gLinkedFunctionTable.reg("(method 21 nav-engine)", execute, 0);
}

} // namespace method_21_nav_engine
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace method_20_nav_engine {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->lwu(v1, 8, a0);                                // lwu v1, 8(a0)
  c->lwu(v1, 4, v1);                                // lwu v1, 4(v1)
  c->lwu(a2, 80, a0);                               // lwu a2, 80(a0)
  c->lwu(a2, 4, a2);                                // lwu a2, 4(a2)
  c->dsubu(v1, v1, a2);                             // dsubu v1, v1, a2
  c->addiu(a2, r0, 0);                              // addiu a2, r0, 0
  //beq r0, r0, L35                                 // beq r0, r0, L35
  // nop                                            // sll r0, r0, 0
  goto block_6;                                     // branch always


block_1:
  c->lwu(a3, 4, a1);                                // lwu a3, 4(a1)
  c->addiu(t0, r0, 288);                            // addiu t0, r0, 288
  c->mult3(t0, t0, a2);                             // mult3 t0, t0, a2
  c->daddu(a3, a3, t0);                             // daddu a3, a3, t0
  c->daddiu(t0, a3, 112);                           // daddiu t0, a3, 112
  c->lwu(t1, 8, a3);                                // lwu t1, 8(a3)
  bc = c->sgpr64(s7) == c->sgpr64(t1);              // beq s7, t1, L34
  c->mov64(t1, s7);                                 // or t1, s7, r0
  if (bc) {goto block_5;}                           // branch non-likely

  c->addiu(t1, r0, -257);                           // addiu t1, r0, -257
  c->lwu(t2, 0, a3);                                // lwu t2, 0(a3)
  c->and_(t1, t1, t2);                              // and t1, t1, t2
  c->sw(t1, 0, a3);                                 // sw t1, 0(a3)
  c->lwu(t1, 8, a3);                                // lwu t1, 8(a3)
  c->lwu(t1, 4, t1);                                // lwu t1, 4(t1)
  c->andi(t1, t1, 2048);                            // andi t1, t1, 2048
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L33
  c->mov64(t1, s7);                                 // or t1, s7, r0
  if (bc) {goto block_4;}                           // branch non-likely

  c->lwu(t1, 0, a3);                                // lwu t1, 0(a3)
  c->ori(t1, t1, 256);                              // ori t1, t1, 256
  c->sw(t1, 0, a3);                                 // sw t1, 0(a3)

block_4:
  c->lwu(t1, 4, a0);                                // lwu t1, 4(a0)
  c->daddiu(t1, t1, 32);                            // daddiu t1, t1, 32
  c->sw(t1, 56, a3);                                // sw t1, 56(a3)
  c->lwu(t1, 8, a0);                                // lwu t1, 8(a0)
  c->sw(t1, 124, a3);                               // sw t1, 124(a3)
  c->mov64(t1, v1);                                 // or t1, v1, r0
  c->lw(t2, 16, t0);                                // lw t2, 16(t0)
  c->daddu(t3, t2, t1);                             // daddu t3, t2, t1
  c->xor_(t4, t2, s7);                              // xor t4, t2, s7
  c->slti(t4, t4, 1);                               // sltiu t4, t4, 1
  c->movz(t2, t3, t4);                              // movz t2, t3, t4
  c->sw(t2, 16, t0);                                // sw t2, 16(t0)
  c->lw(t2, 28, t0);                                // lw t2, 28(t0)
  c->daddu(t3, t2, t1);                             // daddu t3, t2, t1
  c->xor_(t4, t2, s7);                              // xor t4, t2, s7
  c->slti(t4, t4, 1);                               // sltiu t4, t4, 1
  c->movz(t2, t3, t4);                              // movz t2, t3, t4
  c->sw(t2, 28, t0);                                // sw t2, 28(t0)
  c->lw(t2, 24, t0);                                // lw t2, 24(t0)
  c->daddu(t3, t2, t1);                             // daddu t3, t2, t1
  c->xor_(t4, t2, s7);                              // xor t4, t2, s7
  c->slti(t4, t4, 1);                               // sltiu t4, t4, 1
  c->movz(t2, t3, t4);                              // movz t2, t3, t4
  c->sw(t2, 24, t0);                                // sw t2, 24(t0)
  c->lw(t2, 20, t0);                                // lw t2, 20(t0)
  c->daddu(t1, t2, t1);                             // daddu t1, t2, t1
  c->xor_(t3, t2, s7);                              // xor t3, t2, s7
  c->slti(t3, t3, 1);                               // sltiu t3, t3, 1
  c->movz(t2, t1, t3);                              // movz t2, t1, t3
  c->sw(t2, 20, t0);                                // sw t2, 20(t0)
  c->gprs[t0].du64[0] = 0;                          // or t0, r0, r0
  c->lwu(a3, 8, a3);                                // lwu a3, 8(a3)
  c->sw(s7, 140, a3);                               // sw s7, 140(a3)
  c->mov64(t1, s7);                                 // or t1, s7, r0

block_5:
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1

block_6:
  c->lb(a3, 14, a1);                                // lb a3, 14(a1)
  c->slt(a3, a2, a3);                               // slt a3, a2, a3
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L32
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
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
  gLinkedFunctionTable.reg("(method 20 nav-engine)", execute, 0);
}

} // namespace method_20_nav_engine
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace method_18_nav_engine {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  // bool bc = false;
  c->lwu(a2, 0, a1);                                // lwu a2, 0(a1)
  c->lwu(v1, 4, a1);                                // lwu v1, 4(a1)
  c->lwu(a0, 8, a1);                                // lwu a0, 8(a1)
  c->lui(a1, 4096);                                 // lui a1, 4096
  c->ori(a1, a1, 53248);                            // ori a1, a1, 53248
  c->addiu(a3, r0, 0);                              // addiu a3, r0, 0

// block_1:
//   c->lw(a3, 0, a1);                                 // lw a3, 0(a1)
//   c->andi(a3, a3, 256);                             // andi a3, a3, 256
//   // nop                                            // sll r0, r0, 0
//   // nop                                            // sll r0, r0, 0
//   // nop                                            // sll r0, r0, 0
//   // nop                                            // sll r0, r0, 0
//   bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L43
//   // nop                                            // sll r0, r0, 0
//   if (bc) {goto block_1;}                           // branch non-likely

  // Unknown instr: sync.l
  c->lui(a3, 4095);                                 // lui a3, 4095
  c->ori(a3, a3, 65535);                            // ori a3, a3, 65535
  c->and_(a2, a3, a2);                              // and a2, a3, a2
  // c->sw(a2, 16, a1);                             // sw a2, 16(a1)
  u32 madr = c->sgpr64(a2);
  c->lui(a2, 4095);                                 // lui a2, 4095
  c->ori(a2, a2, 65535);                            // ori a2, a2, 65535
  c->and_(v1, a2, v1);                              // and v1, a2, v1
  // c->sw(v1, 128, a1);                            // sw v1, 128(a1)
  u32 sadr = c->sgpr64(v1);
  // c->sw(a0, 32, a1);                             // sw a0, 32(a1)
  u32 qwc = c->sgpr64(a0);
  // Unknown instr: sync.l
  c->addiu(v1, r0, 256);                            // addiu v1, r0, 256
  // c->sw(v1, 0, a1);                              // sw v1, 0(a1)
  spad_from_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
  // Unknown instr: sync.l
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
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
  gLinkedFunctionTable.reg("(method 18 nav-engine)", execute, 0);
  cache.fake_scratchpad_data = intern_from_c(-1, 0, "*fake-scratchpad-data*").c();
}

} // namespace method_18_nav_engine
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace method_17_nav_engine {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  c->lwu(v1, 4, a1);                                // lwu v1, 4(a1)
  c->lwu(a2, 0, a1);                                // lwu a2, 0(a1)
  c->lwu(a0, 8, a1);                                // lwu a0, 8(a1)
  c->lui(a1, 4096);                                 // lui a1, 4096
  c->ori(a1, a1, 54272);                            // ori a1, a1, 54272
  c->addiu(a3, r0, 0);                              // addiu a3, r0, 0

// block_1:
//   c->lw(a3, 0, a1);                                 // lw a3, 0(a1)
//   c->andi(a3, a3, 256);                             // andi a3, a3, 256
//   // nop                                            // sll r0, r0, 0
//   // nop                                            // sll r0, r0, 0
//   // nop                                            // sll r0, r0, 0
//   // nop                                            // sll r0, r0, 0
//   bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L45
//   // nop                                            // sll r0, r0, 0
//   if (bc) {goto block_1;}                           // branch non-likely

  // Unknown instr: sync.l
  c->lui(a3, 4095);                                 // lui a3, 4095
  c->ori(a3, a3, 65535);                            // ori a3, a3, 65535
  c->and_(a2, a3, a2);                              // and a2, a3, a2
  // c->sw(a2, 16, a1);                             // sw a2, 16(a1)
  u32 madr = c->sgpr64(a2);
  c->lui(a2, 4095);                                 // lui a2, 4095
  c->ori(a2, a2, 65535);                            // ori a2, a2, 65535
  c->and_(v1, a2, v1);                              // and v1, a2, v1
  // c->sw(v1, 128, a1);                            // sw v1, 128(a1)
  u32 sadr = c->sgpr64(v1);
  // c->sw(a0, 32, a1);                             // sw a0, 32(a1)
  u32 qwc = c->sgpr64(a0);
  // Unknown instr: sync.l
  c->addiu(v1, r0, 256);                            // addiu v1, r0, 256
  // c->sw(v1, 0, a1);                                 // sw v1, 0(a1)
  spad_to_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
  // Unknown instr: sync.l
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
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
  gLinkedFunctionTable.reg("(method 17 nav-engine)", execute, 0);
  cache.fake_scratchpad_data = intern_from_c(-1, 0, "*fake-scratchpad-data*").c();
}

} // namespace method_17_nav_engine
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace nav_state_patch_pointers {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  c->lw(v1, 16, a0);                                // lw v1, 16(a0)
  c->daddu(a2, v1, a1);                             // daddu a2, v1, a1
  c->xor_(a3, v1, s7);                              // xor a3, v1, s7
  c->sltiu(a3, a3, 1);                              // sltiu a3, a3, 1
  c->movz(v1, a2, a3);                              // movz v1, a2, a3
  c->sw(v1, 16, a0);                                // sw v1, 16(a0)
  c->lw(v1, 28, a0);                                // lw v1, 28(a0)
  c->daddu(a2, v1, a1);                             // daddu a2, v1, a1
  c->xor_(a3, v1, s7);                              // xor a3, v1, s7
  c->sltiu(a3, a3, 1);                              // sltiu a3, a3, 1
  c->movz(v1, a2, a3);                              // movz v1, a2, a3
  c->sw(v1, 28, a0);                                // sw v1, 28(a0)
  c->lw(v1, 24, a0);                                // lw v1, 24(a0)
  c->daddu(a2, v1, a1);                             // daddu a2, v1, a1
  c->xor_(a3, v1, s7);                              // xor a3, v1, s7
  c->sltiu(a3, a3, 1);                              // sltiu a3, a3, 1
  c->movz(v1, a2, a3);                              // movz v1, a2, a3
  c->sw(v1, 24, a0);                                // sw v1, 24(a0)
  c->lw(v1, 20, a0);                                // lw v1, 20(a0)
  c->daddu(a1, v1, a1);                             // daddu a1, v1, a1
  c->xor_(a2, v1, s7);                              // xor a2, v1, s7
  c->sltiu(a2, a2, 1);                              // sltiu a2, a2, 1
  c->movz(v1, a1, a2);                              // movz v1, a1, a2
  c->sw(v1, 20, a0);                                // sw v1, 20(a0)
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  gLinkedFunctionTable.reg("nav-state-patch-pointers", execute, 0);
}

} // namespace nav_state_patch_pointers
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace nav_dma_send_from_spr_no_flush {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  c->lui(v1, 4096);                                 // lui v1, 4096
  c->ori(v1, v1, 53248);                            // ori v1, v1, 53248
  c->addiu(a3, r0, 0);                              // addiu a3, r0, 0

// block_1:
//   c->lw(a3, 0, v1);                                 // lw a3, 0(v1)
//   c->andi(a3, a3, 256);                             // andi a3, a3, 256
//   // nop                                            // sll r0, r0, 0
//   // nop                                            // sll r0, r0, 0
//   // nop                                            // sll r0, r0, 0
//   // nop                                            // sll r0, r0, 0
//   bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L49
//   // nop                                            // sll r0, r0, 0
//   if (bc) {goto block_1;}                           // branch non-likely

  // Unknown instr: sync.l
  c->lui(a3, 4095);                                 // lui a3, 4095
  c->ori(a3, a3, 65535);                            // ori a3, a3, 65535
  c->and_(a0, a3, a0);                              // and a0, a3, a0
  // c->sw(a0, 16, v1);                             // sw a0, 16(v1)
  u32 madr = c->sgpr64(a0);
  c->lui(a0, 4095);                                 // lui a0, 4095
  c->ori(a0, a0, 65535);                            // ori a0, a0, 65535
  c->and_(a0, a0, a1);                              // and a0, a0, a1
  // c->sw(a0, 128, v1);                            // sw a0, 128(v1)
  u32 sadr = c->sgpr64(a0);
  // c->sw(a2, 32, v1);                             // sw a2, 32(v1)
  u32 qwc = c->sgpr64(a2);
  // Unknown instr: sync.l
  c->addiu(a0, r0, 256);                            // addiu a0, r0, 256
  // c->sw(a0, 0, v1);                              // sw a0, 0(v1)
  spad_from_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
  // Unknown instr: sync.l
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
  cache.fake_scratchpad_data = intern_from_c(-1, 0, "*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("nav-dma-send-from-spr-no-flush", execute, 0);
}

} // namespace nav_dma_send_from_spr_no_flush
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace nav_dma_send_to_spr_no_flush {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  c->lui(v1, 4096);                                 // lui v1, 4096
  c->ori(v1, v1, 54272);                            // ori v1, v1, 54272
  c->addiu(a3, r0, 0);                              // addiu a3, r0, 0

// block_1:
//   c->lw(a3, 0, v1);                                 // lw a3, 0(v1)
//   c->andi(a3, a3, 256);                             // andi a3, a3, 256
//   // nop                                            // sll r0, r0, 0
//   // nop                                            // sll r0, r0, 0
//   // nop                                            // sll r0, r0, 0
//   // nop                                            // sll r0, r0, 0
//   bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L51
//   // nop                                            // sll r0, r0, 0
//   if (bc) {goto block_1;}                           // branch non-likely

  // Unknown instr: sync.l
  c->lui(a3, 4095);                                 // lui a3, 4095
  c->ori(a3, a3, 65535);                            // ori a3, a3, 65535
  c->and_(a1, a3, a1);                              // and a1, a3, a1
  // c->sw(a1, 16, v1);                             // sw a1, 16(v1)
  u32 madr = c->sgpr64(a1);
  c->lui(a1, 4095);                                 // lui a1, 4095
  c->ori(a1, a1, 65535);                            // ori a1, a1, 65535
  c->and_(a0, a1, a0);                              // and a0, a1, a0
  // c->sw(a0, 128, v1);                            // sw a0, 128(v1)
  u32 sadr = c->sgpr64(a0);
  // c->sw(a2, 32, v1);                             // sw a2, 32(v1)
  u32 qwc = c->sgpr64(a2);
  // Unknown instr: sync.l
  c->addiu(a0, r0, 256);                            // addiu a0, r0, 256
  // c->sw(a0, 0, v1);                              // sw a0, 0(v1)
  spad_to_dma(cache.fake_scratchpad_data, madr, sadr, qwc);
  // Unknown instr: sync.l
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
  cache.fake_scratchpad_data = intern_from_c(-1, 0, "*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("nav-dma-send-to-spr-no-flush", execute, 0);
}

} // namespace nav_dma_send_to_spr_no_flush
} // namespace Mips2C