
//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace debug_line_clip {
struct Cache {
  void* math_camera; // *math-camera*
} cache;

void qfsrv_same_mtsab_4(ExecutionContext* c, int reg) {
  u32 temp[4];
  auto& val = c->gprs[reg];
  temp[0] = val.du32[1];
  temp[1] = val.du32[2];
  temp[2] = val.du32[3];
  temp[3] = val.du32[0];
  val.du32[0] = temp[0];
  val.du32[1] = temp[1];
  val.du32[2] = temp[2];
  val.du32[3] = temp[3];
}

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  bool cop1_bc = false;
  c->load_symbol2(v1, cache.math_camera);           // lw v1, *math-camera*(s7)
  c->lqc2(vf9, 0, a2);                              // lqc2 vf9, 0(a2)
  c->lqc2(vf10, 0, a3);                             // lqc2 vf10, 0(a3)
  c->lqc2(vf16, 940, v1);                           // lqc2 vf16, 940(v1)
  c->lqc2(vf17, 956, v1);                           // lqc2 vf17, 956(v1)
  c->lqc2(vf18, 972, v1);                           // lqc2 vf18, 972(v1)
  c->lqc2(vf19, 988, v1);                           // lqc2 vf19, 988(v1)
  c->vmula_bc(DEST::xyzw, BC::x, vf16, vf9);        // vmulax.xyzw acc, vf16, vf9
  c->vmadda_bc(DEST::xyzw, BC::y, vf17, vf9);       // vmadday.xyzw acc, vf17, vf9
  c->vmadda_bc(DEST::xyzw, BC::z, vf18, vf9);       // vmaddaz.xyzw acc, vf18, vf9
  c->vmsub_bc(DEST::xyzw, BC::w, vf11, vf19, vf0);  // vmsubw.xyzw vf11, vf19, vf0
  c->vmula_bc(DEST::xyzw, BC::x, vf16, vf10);       // vmulax.xyzw acc, vf16, vf10
  c->vmadda_bc(DEST::xyzw, BC::y, vf17, vf10);      // vmadday.xyzw acc, vf17, vf10
  c->vmadda_bc(DEST::xyzw, BC::z, vf18, vf10);      // vmaddaz.xyzw acc, vf18, vf10
  c->vmsub_bc(DEST::xyzw, BC::w, vf12, vf19, vf0);  // vmsubw.xyzw vf12, vf19, vf0
  c->mov128_gpr_vf(v1, vf11);                       // qmfc2.i v1, vf11
  c->pcgtw(v1, r0, v1);                             // pcgtw v1, r0, v1
  c->ppach(a2, r0, v1);                             // ppach a2, r0, v1
  c->mov128_gpr_vf(v1, vf12);                       // qmfc2.i v1, vf12
  c->pcgtw(v1, r0, v1);                             // pcgtw v1, r0, v1
  c->ppach(a3, r0, v1);                             // ppach a3, r0, v1
  c->and_(v1, a2, a3);                              // and v1, a2, a3
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L220
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_10;}                          // branch non-likely

  c->vmove(DEST::xyzw, vf13, vf9);                  // vmove.xyzw vf13, vf9
  c->vmove(DEST::xyzw, vf14, vf10);                 // vmove.xyzw vf14, vf10
  c->or_(a2, a2, a3);                               // or a2, a2, a3
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L219
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_9;}                           // branch non-likely

  c->mov128_gpr_vf(a2, vf11);                       // qmfc2.i a2, vf11
  c->mov128_gpr_vf(a3, vf12);                       // qmfc2.i a3, vf12
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  c->lui(t0, 16256);                                // lui t0, 16256
  c->mtc1(f2, t0);                                  // mtc1 f2, t0
  c->addiu(t0, r0, 3);                              // addiu t0, r0, 3
  // Unknown instr: mtsab r0, 4

  block_3:
  c->mtc1(f3, a2);                                  // mtc1 f3, a2
  c->mtc1(f4, a3);                                  // mtc1 f4, a3
  c->subs(f5, f3, f4);                              // sub.s f5, f3, f4
  cop1_bc = c->fprs[f3] < c->fprs[f0];              // c.lt.s f3, f0
  bc = !cop1_bc;                                    // bc1f L217
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_5;}                           // branch non-likely

  c->divs(f6, f3, f5);                              // div.s f6, f3, f5
  c->maxs(f1, f6, f1);                              // max.s f1, f6, f1

  block_5:
  cop1_bc = c->fprs[f4] < c->fprs[f0];              // c.lt.s f4, f0
  bc = !cop1_bc;                                    // bc1f L218
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  c->divs(f3, f3, f5);                              // div.s f3, f3, f5
  c->mins(f2, f3, f2);                              // min.s f2, f3, f2

  block_7:
  // Unknown instr: qfsrv a2, a2, a2
  qfsrv_same_mtsab_4(c, a2);
  // Unknown instr: qfsrv a3, a3, a3
  qfsrv_same_mtsab_4(c, a3);
  bc = c->sgpr64(t0) != 0;                          // bne t0, r0, L216
  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  if (bc) {goto block_3;}                           // branch non-likely

  c->mfc1(a2, f1);                                  // mfc1 a2, f1
  c->mfc1(a3, f2);                                  // mfc1 a3, f2
  c->mov128_vf_gpr(vf20, a2);                       // qmtc2.i vf20, a2
  c->mov128_vf_gpr(vf21, a3);                       // qmtc2.i vf21, a3
  c->vsub(DEST::xyzw, vf15, vf10, vf9);             // vsub.xyzw vf15, vf10, vf9
  c->vmul_bc(DEST::xyzw, BC::x, vf13, vf15, vf20);  // vmulx.xyzw vf13, vf15, vf20
  c->vadd(DEST::xyzw, vf13, vf9, vf13);             // vadd.xyzw vf13, vf9, vf13
  c->vmul_bc(DEST::xyzw, BC::x, vf14, vf15, vf21);  // vmulx.xyzw vf14, vf15, vf21
  c->vadd(DEST::xyzw, vf14, vf9, vf14);             // vadd.xyzw vf14, vf9, vf14

  block_9:
  c->sqc2(vf13, 0, a0);                             // sqc2 vf13, 0(a0)
  c->sqc2(vf14, 0, a1);                             // sqc2 vf14, 0(a1)

  block_10:
  c->daddiu(v0, s7, 4);                             // daddiu v0, s7, 4
  c->movn(v0, s7, v1);                              // movn v0, s7, v1
  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.math_camera = intern_from_c("*math-camera*").c();
  gLinkedFunctionTable.reg("debug-line-clip?", execute, 32);
}

} // namespace debug_line_clip
} // namespace Mips2C
