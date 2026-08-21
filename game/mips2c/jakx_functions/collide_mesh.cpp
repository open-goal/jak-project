//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace method_12_collide_mesh {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  // nop                                            // sll r0, r0, 0
  get_fake_spad_addr2(t0, cache.fake_scratchpad_data, 0, c);// lui t0, 28672
  c->lwu(v1, 8, a0);                                // lwu v1, 8(a0)
  // nop                                            // sll r0, r0, 0
  c->lwu(a3, 12, a0);                               // lwu a3, 12(a0)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L7
  c->lqc2(vf1, 0, a2);                              // lqc2 vf1, 0(a2)
  if (bc) {goto block_3;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 16, a2);                             // lqc2 vf2, 16(a2)
  c->daddiu(t0, t0, -64);                           // daddiu t0, t0, -64
  c->lqc2(vf3, 32, a2);                             // lqc2 vf3, 32(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf4, 48, a2);                             // lqc2 vf4, 48(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf5, 0, a3);                              // lqc2 vf5, 0(a3)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf6, 16, a3);                             // lqc2 vf6, 16(a3)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf7, 32, a3);                             // lqc2 vf7, 32(a3)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf8, 48, a3);                             // lqc2 vf8, 48(a3)
  
block_2:
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  c->daddiu(a3, a3, 64);                            // daddiu a3, a3, 64
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf5);        // vmaddax.xyzw acc, vf1, vf5
  c->daddiu(t0, t0, 64);                            // daddiu t0, t0, 64
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf5);        // vmadday.xyzw acc, vf2, vf5
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf9, vf3, vf5);    // vmaddz.xyzw vf9, vf3, vf5
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  c->lqc2(vf5, 0, a3);                              // lqc2 vf5, 0(a3)
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf6);        // vmaddax.xyzw acc, vf1, vf6
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf6);        // vmadday.xyzw acc, vf2, vf6
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf10, vf3, vf6);   // vmaddz.xyzw vf10, vf3, vf6
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  c->lqc2(vf6, 16, a3);                             // lqc2 vf6, 16(a3)
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf7);        // vmaddax.xyzw acc, vf1, vf7
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf7);        // vmadday.xyzw acc, vf2, vf7
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf11, vf3, vf7);   // vmaddz.xyzw vf11, vf3, vf7
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  c->lqc2(vf7, 32, a3);                             // lqc2 vf7, 32(a3)
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf8);        // vmaddax.xyzw acc, vf1, vf8
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf8);        // vmadday.xyzw acc, vf2, vf8
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf12, vf3, vf8);   // vmaddz.xyzw vf12, vf3, vf8
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf8, 48, a3);                             // lqc2 vf8, 48(a3)
  c->daddiu(v1, v1, -4);                            // daddiu v1, v1, -4
  c->sqc2(vf9, 0, t0);                              // sqc2 vf9, 0(t0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf10, 16, t0);                            // sqc2 vf10, 16(t0)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf11, 32, t0);                            // sqc2 vf11, 32(t0)
  bc = ((s64)c->sgpr64(v1)) > 0;                    // bgtz v1, L6
  c->sqc2(vf12, 48, t0);                            // sqc2 vf12, 48(t0)
  if (bc) {goto block_2;}                           // branch non-likely

  
block_3:
  c->daddiu(v1, a0, 28);                            // daddiu v1, a0, 28
  // nop                                            // sll r0, r0, 0
  get_fake_spad_addr2(a2, cache.fake_scratchpad_data, 0, c);// lui a2, 28672
  c->lwu(a0, 4, a0);                                // lwu a0, 4(a0)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L9
  c->lbu(t0, 0, v1);                                // lbu t0, 0(v1)
  if (bc) {goto block_6;}                           // branch non-likely

  c->daddiu(a1, a1, -96);                           // daddiu a1, a1, -96
  c->lbu(a3, 1, v1);                                // lbu a3, 1(v1)
  c->dsll(t0, t0, 4);                               // dsll t0, t0, 4
  c->lbu(t2, 2, v1);                                // lbu t2, 2(v1)
  c->dsll(t1, a3, 4);                               // dsll t1, a3, 4
  c->lwu(a3, 4, v1);                                // lwu a3, 4(v1)
  c->dsll(t2, t2, 4);                               // dsll t2, t2, 4
  c->daddu(t0, t0, a2);                             // daddu t0, t0, a2
  c->daddu(t1, t1, a2);                             // daddu t1, t1, a2
  c->daddu(t2, t2, a2);                             // daddu t2, t2, a2
  
block_5:
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->lqc2(vf1, 0, t0);                              // lqc2 vf1, 0(t0)
  c->daddiu(v1, v1, 8);                             // daddiu v1, v1, 8
  c->lqc2(vf2, 0, t1);                              // lqc2 vf2, 0(t1)
  c->daddiu(a1, a1, 96);                            // daddiu a1, a1, 96
  c->lqc2(vf3, 0, t2);                              // lqc2 vf3, 0(t2)
  c->vsub(DEST::xyzw, vf4, vf2, vf1);               // vsub.xyzw vf4, vf2, vf1
  c->sqc2(vf1, 0, a1);                              // sqc2 vf1, 0(a1)
  c->vmini(DEST::xyzw, vf8, vf1, vf2);              // vmini.xyzw vf8, vf1, vf2
  c->sqc2(vf2, 16, a1);                             // sqc2 vf2, 16(a1)
  c->vsub(DEST::xyzw, vf5, vf3, vf1);               // vsub.xyzw vf5, vf3, vf1
  c->sqc2(vf3, 32, a1);                             // sqc2 vf3, 32(a1)
  c->vmax(DEST::xyzw, vf9, vf1, vf2);               // vmax.xyzw vf9, vf1, vf2
  c->lbu(t1, 0, v1);                                // lbu t1, 0(v1)
  c->vopmula(vf4, vf5);                             // vopmula.xyz acc, vf4, vf5
  c->lbu(t2, 1, v1);                                // lbu t2, 1(v1)
  c->vopmsub(vf6, vf5, vf4);                        // vopmsub.xyz vf6, vf5, vf4
  c->lbu(t0, 2, v1);                                // lbu t0, 2(v1)
  c->vmul(DEST::xyzw, vf7, vf6, vf6);               // vmul.xyzw vf7, vf6, vf6
  // nop                                            // sll r0, r0, 0
  c->vmini(DEST::xyzw, vf8, vf8, vf3);              // vmini.xyzw vf8, vf8, vf3
  c->dsll(t1, t1, 4);                               // dsll t1, t1, 4
  c->vmax(DEST::xyzw, vf9, vf9, vf3);               // vmax.xyzw vf9, vf9, vf3
  c->dsll(t2, t2, 4);                               // dsll t2, t2, 4
  c->vmula_bc(DEST::w, BC::x, vf0, vf7);            // vmulax.w acc, vf0, vf7
  c->dsll(t3, t0, 4);                               // dsll t3, t0, 4
  c->vmadda_bc(DEST::w, BC::y, vf0, vf7);           // vmadday.w acc, vf0, vf7
  c->daddu(t0, t1, a2);                             // daddu t0, t1, a2
  c->vmadd_bc(DEST::w, BC::z, vf7, vf0, vf7);       // vmaddz.w vf7, vf0, vf7
  c->daddu(t1, t2, a2);                             // daddu t1, t2, a2
  c->vrsqrt(vf0, BC::w, vf7, BC::w);                // vrsqrt Q, vf0.w, vf7.w
  c->daddu(t2, t3, a2);                             // daddu t2, t3, a2
  c->vftoi0(DEST::xyzw, vf8, vf8);                  // vftoi0.xyzw vf8, vf8
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf9, vf9);                  // vftoi0.xyzw vf9, vf9
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, 64, a1);                             // sqc2 vf8, 64(a1)
  c->vwaitq();                                      // vwaitq
  c->sqc2(vf9, 80, a1);                             // sqc2 vf9, 80(a1)
  c->vmulq(DEST::xyz, vf6, vf6);                    // vmulq.xyz vf6, vf6, Q
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf6, 48, a1);                             // sqc2 vf6, 48(a1)
  // nop                                            // sll r0, r0, 0
  c->sw(a3, 60, a1);                                // sw a3, 60(a1)
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L8
  c->lwu(a3, 4, v1);                                // lwu a3, 4(v1)
  if (bc) {goto block_5;}                           // branch non-likely

  
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
  cache.fake_scratchpad_data = intern_from_c(-1, 0, "*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("(method 12 collide-mesh)", execute, 512);
}

} // namespace method_12_collide_mesh
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jakx/kscheme.h"
using ::jakx::intern_from_c;
namespace Mips2C::jakx {
namespace method_14_collide_mesh {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lwu(v1, 12, a0);                               // lwu v1, 12(a0)
  // nop                                            // sll r0, r0, 0
  c->lwu(a0, 8, a0);                                // lwu a0, 8(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf1, 0, a1);                              // lqc2 vf1, 0(a1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 16, a1);                             // lqc2 vf2, 16(a1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 32, a1);                             // lqc2 vf3, 32(a1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf4, 48, a1);                             // lqc2 vf4, 48(a1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf13, 0, a2);                             // lqc2 vf13, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 16, a2);                            // lqc2 vf14, 16(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf15, 32, a2);                            // lqc2 vf15, 32(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf16, 48, a2);                            // lqc2 vf16, 48(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf5, 0, v1);                              // lqc2 vf5, 0(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf6, 16, v1);                             // lqc2 vf6, 16(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf7, 32, v1);                             // lqc2 vf7, 32(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf8, 48, v1);                             // lqc2 vf8, 48(v1)
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  c->lqc2(vf9, 64, v1);                             // lqc2 vf9, 64(v1)
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf5);        // vmaddax.xyzw acc, vf1, vf5
  c->lqc2(vf10, 80, v1);                            // lqc2 vf10, 80(v1)
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf5);        // vmadday.xyzw acc, vf2, vf5
  c->lqc2(vf11, 96, v1);                            // lqc2 vf11, 96(v1)
  c->vmadd_bc(DEST::xyzw, BC::z, vf5, vf3, vf5);    // vmaddz.xyzw vf5, vf3, vf5
  c->lqc2(vf12, 112, v1);                           // lqc2 vf12, 112(v1)
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf6);        // vmaddax.xyzw acc, vf1, vf6
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf6);        // vmadday.xyzw acc, vf2, vf6
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf6, vf3, vf6);    // vmaddz.xyzw vf6, vf3, vf6
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf7);        // vmaddax.xyzw acc, vf1, vf7
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf7);        // vmadday.xyzw acc, vf2, vf7
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf7, vf3, vf7);    // vmaddz.xyzw vf7, vf3, vf7
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf8);        // vmaddax.xyzw acc, vf1, vf8
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf8);        // vmadday.xyzw acc, vf2, vf8
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf8, vf3, vf8);    // vmaddz.xyzw vf8, vf3, vf8
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf16, vf0);        // vmulaw.xyzw acc, vf16, vf0
  c->sqc2(vf5, 0, a3);                              // sqc2 vf5, 0(a3)
  c->vmadda_bc(DEST::xyzw, BC::x, vf13, vf5);       // vmaddax.xyzw acc, vf13, vf5
  c->sqc2(vf6, 32, a3);                             // sqc2 vf6, 32(a3)
  c->vmadda_bc(DEST::xyzw, BC::y, vf14, vf5);       // vmadday.xyzw acc, vf14, vf5
  c->sqc2(vf7, 64, a3);                             // sqc2 vf7, 64(a3)
  c->vmadd_bc(DEST::xyzw, BC::z, vf5, vf15, vf5);   // vmaddz.xyzw vf5, vf15, vf5
  c->sqc2(vf8, 96, a3);                             // sqc2 vf8, 96(a3)
  c->vmula_bc(DEST::xyzw, BC::w, vf16, vf0);        // vmulaw.xyzw acc, vf16, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf13, vf6);       // vmaddax.xyzw acc, vf13, vf6
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf14, vf6);       // vmadday.xyzw acc, vf14, vf6
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf6, vf15, vf6);   // vmaddz.xyzw vf6, vf15, vf6
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf16, vf0);        // vmulaw.xyzw acc, vf16, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf13, vf7);       // vmaddax.xyzw acc, vf13, vf7
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf14, vf7);       // vmadday.xyzw acc, vf14, vf7
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf7, vf15, vf7);   // vmaddz.xyzw vf7, vf15, vf7
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf16, vf0);        // vmulaw.xyzw acc, vf16, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf13, vf8);       // vmaddax.xyzw acc, vf13, vf8
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf14, vf8);       // vmadday.xyzw acc, vf14, vf8
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf8, vf15, vf8);   // vmaddz.xyzw vf8, vf15, vf8
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf5, vf5);                  // vftoi0.xyzw vf5, vf5
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf6, vf6);                  // vftoi0.xyzw vf6, vf6
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf7, vf7);                  // vftoi0.xyzw vf7, vf7
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf8, vf8);                  // vftoi0.xyzw vf8, vf8
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf5, 16, a3);                             // sqc2 vf5, 16(a3)
  c->daddiu(a0, a0, -4);                            // daddiu a0, a0, -4
  c->sqc2(vf6, 48, a3);                             // sqc2 vf6, 48(a3)
  c->daddiu(v1, v1, 128);                           // daddiu v1, v1, 128
  c->sqc2(vf7, 80, a3);                             // sqc2 vf7, 80(a3)
  bc = ((s64)c->sgpr64(a0)) <= 0;                   // blez a0, L22
  c->sqc2(vf8, 112, a3);                            // sqc2 vf8, 112(a3)
  if (bc) {goto block_3;}                           // branch non-likely

  
block_1:
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  c->lqc2(vf5, 0, v1);                              // lqc2 vf5, 0(v1)
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf9);        // vmaddax.xyzw acc, vf1, vf9
  c->lqc2(vf6, 16, v1);                             // lqc2 vf6, 16(v1)
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf9);        // vmadday.xyzw acc, vf2, vf9
  c->lqc2(vf7, 32, v1);                             // lqc2 vf7, 32(v1)
  c->vmadd_bc(DEST::xyzw, BC::z, vf9, vf3, vf9);    // vmaddz.xyzw vf9, vf3, vf9
  c->lqc2(vf8, 48, v1);                             // lqc2 vf8, 48(v1)
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf10);       // vmaddax.xyzw acc, vf1, vf10
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf10);       // vmadday.xyzw acc, vf2, vf10
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf10, vf3, vf10);  // vmaddz.xyzw vf10, vf3, vf10
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf11);       // vmaddax.xyzw acc, vf1, vf11
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf11);       // vmadday.xyzw acc, vf2, vf11
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf11, vf3, vf11);  // vmaddz.xyzw vf11, vf3, vf11
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf12);       // vmaddax.xyzw acc, vf1, vf12
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf12);       // vmadday.xyzw acc, vf2, vf12
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf12, vf3, vf12);  // vmaddz.xyzw vf12, vf3, vf12
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf16, vf0);        // vmulaw.xyzw acc, vf16, vf0
  c->sqc2(vf9, 128, a3);                            // sqc2 vf9, 128(a3)
  c->vmadda_bc(DEST::xyzw, BC::x, vf13, vf9);       // vmaddax.xyzw acc, vf13, vf9
  c->sqc2(vf10, 160, a3);                           // sqc2 vf10, 160(a3)
  c->vmadda_bc(DEST::xyzw, BC::y, vf14, vf9);       // vmadday.xyzw acc, vf14, vf9
  c->sqc2(vf11, 192, a3);                           // sqc2 vf11, 192(a3)
  c->vmadd_bc(DEST::xyzw, BC::z, vf9, vf15, vf9);   // vmaddz.xyzw vf9, vf15, vf9
  c->sqc2(vf12, 224, a3);                           // sqc2 vf12, 224(a3)
  c->vmula_bc(DEST::xyzw, BC::w, vf16, vf0);        // vmulaw.xyzw acc, vf16, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf13, vf10);      // vmaddax.xyzw acc, vf13, vf10
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf14, vf10);      // vmadday.xyzw acc, vf14, vf10
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf10, vf15, vf10); // vmaddz.xyzw vf10, vf15, vf10
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf16, vf0);        // vmulaw.xyzw acc, vf16, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf13, vf11);      // vmaddax.xyzw acc, vf13, vf11
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf14, vf11);      // vmadday.xyzw acc, vf14, vf11
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf11, vf15, vf11); // vmaddz.xyzw vf11, vf15, vf11
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf16, vf0);        // vmulaw.xyzw acc, vf16, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf13, vf12);      // vmaddax.xyzw acc, vf13, vf12
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf14, vf12);      // vmadday.xyzw acc, vf14, vf12
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf12, vf15, vf12); // vmaddz.xyzw vf12, vf15, vf12
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf9, vf9);                  // vftoi0.xyzw vf9, vf9
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf10, vf10);                // vftoi0.xyzw vf10, vf10
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf11, vf11);                // vftoi0.xyzw vf11, vf11
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf12, vf12);                // vftoi0.xyzw vf12, vf12
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf9, 144, a3);                            // sqc2 vf9, 144(a3)
  c->daddiu(a0, a0, -4);                            // daddiu a0, a0, -4
  c->sqc2(vf10, 176, a3);                           // sqc2 vf10, 176(a3)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf11, 208, a3);                           // sqc2 vf11, 208(a3)
  bc = ((s64)c->sgpr64(a0)) <= 0;                   // blez a0, L22
  c->sqc2(vf12, 240, a3);                           // sqc2 vf12, 240(a3)
  if (bc) {goto block_3;}                           // branch non-likely

  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  c->lqc2(vf9, 64, v1);                             // lqc2 vf9, 64(v1)
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf5);        // vmaddax.xyzw acc, vf1, vf5
  c->lqc2(vf10, 80, v1);                            // lqc2 vf10, 80(v1)
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf5);        // vmadday.xyzw acc, vf2, vf5
  c->lqc2(vf11, 96, v1);                            // lqc2 vf11, 96(v1)
  c->vmadd_bc(DEST::xyzw, BC::z, vf5, vf3, vf5);    // vmaddz.xyzw vf5, vf3, vf5
  c->lqc2(vf12, 112, v1);                           // lqc2 vf12, 112(v1)
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  c->daddiu(a3, a3, 256);                           // daddiu a3, a3, 256
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf6);        // vmaddax.xyzw acc, vf1, vf6
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf6);        // vmadday.xyzw acc, vf2, vf6
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf6, vf3, vf6);    // vmaddz.xyzw vf6, vf3, vf6
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf7);        // vmaddax.xyzw acc, vf1, vf7
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf7);        // vmadday.xyzw acc, vf2, vf7
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf7, vf3, vf7);    // vmaddz.xyzw vf7, vf3, vf7
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf8);        // vmaddax.xyzw acc, vf1, vf8
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf8);        // vmadday.xyzw acc, vf2, vf8
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf8, vf3, vf8);    // vmaddz.xyzw vf8, vf3, vf8
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf16, vf0);        // vmulaw.xyzw acc, vf16, vf0
  c->sqc2(vf5, 0, a3);                              // sqc2 vf5, 0(a3)
  c->vmadda_bc(DEST::xyzw, BC::x, vf13, vf5);       // vmaddax.xyzw acc, vf13, vf5
  c->sqc2(vf6, 32, a3);                             // sqc2 vf6, 32(a3)
  c->vmadda_bc(DEST::xyzw, BC::y, vf14, vf5);       // vmadday.xyzw acc, vf14, vf5
  c->sqc2(vf7, 64, a3);                             // sqc2 vf7, 64(a3)
  c->vmadd_bc(DEST::xyzw, BC::z, vf5, vf15, vf5);   // vmaddz.xyzw vf5, vf15, vf5
  c->sqc2(vf8, 96, a3);                             // sqc2 vf8, 96(a3)
  c->vmula_bc(DEST::xyzw, BC::w, vf16, vf0);        // vmulaw.xyzw acc, vf16, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf13, vf6);       // vmaddax.xyzw acc, vf13, vf6
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf14, vf6);       // vmadday.xyzw acc, vf14, vf6
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf6, vf15, vf6);   // vmaddz.xyzw vf6, vf15, vf6
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf16, vf0);        // vmulaw.xyzw acc, vf16, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf13, vf7);       // vmaddax.xyzw acc, vf13, vf7
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf14, vf7);       // vmadday.xyzw acc, vf14, vf7
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf7, vf15, vf7);   // vmaddz.xyzw vf7, vf15, vf7
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf16, vf0);        // vmulaw.xyzw acc, vf16, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf13, vf8);       // vmaddax.xyzw acc, vf13, vf8
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf14, vf8);       // vmadday.xyzw acc, vf14, vf8
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf8, vf15, vf8);   // vmaddz.xyzw vf8, vf15, vf8
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf5, vf5);                  // vftoi0.xyzw vf5, vf5
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf6, vf6);                  // vftoi0.xyzw vf6, vf6
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf7, vf7);                  // vftoi0.xyzw vf7, vf7
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf8, vf8);                  // vftoi0.xyzw vf8, vf8
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf5, 16, a3);                             // sqc2 vf5, 16(a3)
  c->daddiu(a0, a0, -4);                            // daddiu a0, a0, -4
  c->sqc2(vf6, 48, a3);                             // sqc2 vf6, 48(a3)
  c->daddiu(v1, v1, 128);                           // daddiu v1, v1, 128
  c->sqc2(vf7, 80, a3);                             // sqc2 vf7, 80(a3)
  bc = ((s64)c->sgpr64(a0)) > 0;                    // bgtz a0, L21
  c->sqc2(vf8, 112, a3);                            // sqc2 vf8, 112(a3)
  if (bc) {goto block_1;}                           // branch non-likely

  
block_3:
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
  gLinkedFunctionTable.reg("(method 14 collide-mesh)", execute, 512);
}

} // namespace method_14_collide_mesh
} // namespace Mips2C