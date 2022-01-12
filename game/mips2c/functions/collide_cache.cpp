//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/kscheme.h"

#include "common/dma/gs.h"

namespace {
u32 vu0_buffer[1024];  // todo, maybe can be 512.
u32 vi1 = 0;

void vlqi(Mips2C::ExecutionContext* c, int reg) {
  memcpy(&c->vfs[reg].f[0], &vu0_buffer[vi1 * 4], 16);
  vi1++;
}
}  // namespace

namespace Mips2C {

namespace pc_upload_collide_frag {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  // vif init is
  // #x30000000 = STROW
  // #x4d000000 ROW x
  // #x4d000000 ROW y
  // #x4d000000 ROW z
  // #x3f800000 ROW w
  // #x5000001  = STMOD 0b1
  // #x20000000 = STMASK
  // #x40404040 = 1's on all w's
  // #x1000404  = STCYCL cl: 4 wl: 4

  int qw_in_source = c->sgpr64(a1);
  int qw_to_write = c->sgpr64(a2);
  const u16* data_in = (const u16*)(g_ee_main_mem + c->sgpr64(a0));
  // I don't quite get why this is wrong sometimes.
  //  assert(qw_to_write * 3 == qw_in_source * 8);
  assert(qw_to_write <= 128);

  int in_idx = 0;
  int out_idx = 0;

  while (out_idx < qw_to_write * 4) {
    vu0_buffer[out_idx++] = 0x4d000000 + data_in[in_idx++];
    vu0_buffer[out_idx++] = 0x4d000000 + data_in[in_idx++];
    vu0_buffer[out_idx++] = 0x4d000000 + data_in[in_idx++];
    vu0_buffer[out_idx++] = 0x3f800000;
  }

  return 0;

  // (vif-cmd unpack-v3-16)
  // (__pc-upload-collide-frag (-> frag mesh packed-data) (-> frag mesh vertex-data-qwc) (-> frag
  // mesh vertex-count))
}

void link() {
  gLinkedFunctionTable.reg("__pc-upload-collide-frag", execute, 128);
}

}  // namespace pc_upload_collide_frag

// clang-format off


// download from VU0.

namespace method_32_collide_cache {

struct Cache {
  void* fake_scratchpad_data;  // *fake-scratchpad-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  // daddiu a0, fp, L307                            // daddiu a0, fp, L307
  // handled at the lq's
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a2) == c->sgpr64(s7);              // beq a2, s7, L293
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_6;}                           // branch non-likely

  //c->lui(v1, 28672);                              // lui v1, 28672
  get_fake_spad_addr(v1, cache.fake_scratchpad_data, 0, c);
  c->lqc2(vf14, 12, a1);                            // lqc2 vf14, 12(a1)
  c->vmove(DEST::xyzw, vf1, vf0);                   // vmove.xyzw vf1, vf0
  // c->lqc2(vf13, 0, a0);                             // lqc2 vf13, 0(a0)
  c->vfs[vf13].du32[0] = 0x4d000000;
  c->vfs[vf13].du32[1] = 0x4d000000;
  c->vfs[vf13].du32[2] = 0x4d000000;
  c->vfs[vf13].du32[3] =  0;
  c->vmove(DEST::xyzw, vf2, vf0);                   // vmove.xyzw vf2, vf0
  c->lbu(a0, 24, a1);                               // lbu a0, 24(a1)
  assert(c->sgpr64(a3) == 0);
  vi1 = 0;
  // Unknown instr: ctc2.i vi1, a3
  c->vmove(DEST::xyzw, vf3, vf0);                   // vmove.xyzw vf3, vf0
  c->vitof0(DEST::xyzw, vf14, vf14);                // vitof0.xyzw vf14, vf14
  c->vmove(DEST::xyzw, vf4, vf0);                   // vmove.xyzw vf4, vf0
  c->vmove(DEST::xyzw, vf5, vf0);                   // vmove.xyzw vf5, vf0
  // nop                                            // sll r0, r0, 0
  c->vmove(DEST::xyzw, vf6, vf0);                   // vmove.xyzw vf6, vf0
  // nop                                            // sll r0, r0, 0
  c->vmove(DEST::xyzw, vf7, vf0);                   // vmove.xyzw vf7, vf0
  // nop                                            // sll r0, r0, 0
  c->vmove(DEST::xyzw, vf8, vf0);                   // vmove.xyzw vf8, vf0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->ld(a1, 52, a2);                                // ld a1, 52(a2)
  // nop                                            // sll r0, r0, 0
  c->ld(t1, 28, a2);                                // ld t1, 28(a2)
  c->pextlh(a1, a1, r0);                            // pextlh a1, a1, r0
  c->ld(a3, 36, a2);                                // ld a3, 36(a2)
  c->psraw(t0, a1, 10);                             // psraw t0, a1, 10
  c->ld(a1, 44, a2);                                // ld a1, 44(a2)
  c->pextlh(t1, t1, r0);                            // pextlh t1, t1, r0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psraw(t1, t1, 16);                             // psraw t1, t1, 16
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(a3, a3, r0);                            // pextlh a3, a3, r0
  c->mov128_vf_gpr(vf18, t0);                       // qmtc2.i vf18, t0
  c->psraw(a3, a3, 16);                             // psraw a3, a3, 16
  c->mov128_vf_gpr(vf15, t1);                       // qmtc2.i vf15, t1
  c->pextlh(a1, a1, r0);                            // pextlh a1, a1, r0
  c->mov128_vf_gpr(vf16, a3);                       // qmtc2.i vf16, a3
  c->psraw(a1, a1, 16);                             // psraw a1, a1, 16
  c->lqc2(vf1, 12, a2);                             // lqc2 vf1, 12(a2)
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf17, a1);                       // qmtc2.i vf17, a1
  c->vitof0(DEST::xyzw, vf18, vf18);                // vitof0.xyzw vf18, vf18
  // nop                                            // sll r0, r0, 0
  c->vitof12(DEST::xyzw, vf15, vf15);               // vitof12.xyzw vf15, vf15
  // nop                                            // sll r0, r0, 0
  c->vitof12(DEST::xyzw, vf16, vf16);               // vitof12.xyzw vf16, vf16
  // nop                                            // sll r0, r0, 0
  c->vitof12(DEST::xyzw, vf17, vf17);               // vitof12.xyzw vf17, vf17
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyz, vf18, vf18, vf1);              // vadd.xyz vf18, vf18, vf1
  // nop                                            // sll r0, r0, 0
  c->vsub(DEST::xyzw, vf13, vf13, vf14);            // vsub.xyzw vf13, vf13, vf14
  // Unknown instr: vlqi.xyz vf1, vi1
  vlqi(c, vf1);
  c->daddiu(a0, a0, -4);                            // daddiu a0, a0, -4
  // Unknown instr: vlqi.xyz vf2, vi1
  vlqi(c, vf2);
  // nop                                            // sll r0, r0, 0
  // Unknown instr: vlqi.xyz vf3, vi1
  vlqi(c, vf3);
  // nop                                            // sll r0, r0, 0
  // Unknown instr: vlqi.xyz vf4, vi1
  vlqi(c, vf4);
  c->vsub(DEST::xyz, vf1, vf1, vf13);               // vsub.xyz vf1, vf1, vf13
  // Unknown instr: vlqi.xyz vf5, vi1
  vlqi(c, vf5);
  c->vsub(DEST::xyz, vf2, vf2, vf13);               // vsub.xyz vf2, vf2, vf13
  // Unknown instr: vlqi.xyz vf6, vi1
  vlqi(c, vf6);
  c->vsub(DEST::xyz, vf3, vf3, vf13);               // vsub.xyz vf3, vf3, vf13
  // Unknown instr: vlqi.xyz vf7, vi1
  vlqi(c, vf7);
  c->vsub(DEST::xyz, vf4, vf4, vf13);               // vsub.xyz vf4, vf4, vf13
  // Unknown instr: vlqi.xyz vf8, vi1
  vlqi(c, vf8);
  c->vmula_bc(DEST::xyzw, BC::w, vf18, vf0);        // vmulaw.xyzw acc, vf18, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf15, vf1);       // vmaddax.xyzw acc, vf15, vf1
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf16, vf1);       // vmadday.xyzw acc, vf16, vf1
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf1, vf17, vf1);   // vmaddz.xyzw vf1, vf17, vf1
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf18, vf0);        // vmulaw.xyzw acc, vf18, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf15, vf2);       // vmaddax.xyzw acc, vf15, vf2
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf16, vf2);       // vmadday.xyzw acc, vf16, vf2
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf2, vf17, vf2);   // vmaddz.xyzw vf2, vf17, vf2
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf18, vf0);        // vmulaw.xyzw acc, vf18, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf15, vf3);       // vmaddax.xyzw acc, vf15, vf3
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf16, vf3);       // vmadday.xyzw acc, vf16, vf3
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf3, vf17, vf3);   // vmaddz.xyzw vf3, vf17, vf3
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf18, vf0);        // vmulaw.xyzw acc, vf18, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf15, vf4);       // vmaddax.xyzw acc, vf15, vf4
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf16, vf4);       // vmadday.xyzw acc, vf16, vf4
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf4, vf17, vf4);   // vmaddz.xyzw vf4, vf17, vf4
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf9, vf1);                  // vftoi0.xyzw vf9, vf1
  c->sqc2(vf1, 16, v1);                             // sqc2 vf1, 16(v1)
  c->vftoi0(DEST::xyzw, vf10, vf2);                 // vftoi0.xyzw vf10, vf2
  c->sqc2(vf2, 48, v1);                             // sqc2 vf2, 48(v1)
  c->vftoi0(DEST::xyzw, vf11, vf3);                 // vftoi0.xyzw vf11, vf3
  c->sqc2(vf3, 80, v1);                             // sqc2 vf3, 80(v1)
  c->vftoi0(DEST::xyzw, vf12, vf4);                 // vftoi0.xyzw vf12, vf4
  c->sqc2(vf4, 112, v1);                            // sqc2 vf4, 112(v1)
  c->vsub(DEST::xyz, vf5, vf5, vf13);               // vsub.xyz vf5, vf5, vf13
  c->sqc2(vf9, 0, v1);                              // sqc2 vf9, 0(v1)
  c->vsub(DEST::xyz, vf6, vf6, vf13);               // vsub.xyz vf6, vf6, vf13
  c->sqc2(vf10, 32, v1);                            // sqc2 vf10, 32(v1)
  c->vsub(DEST::xyz, vf7, vf7, vf13);               // vsub.xyz vf7, vf7, vf13
  c->sqc2(vf11, 64, v1);                            // sqc2 vf11, 64(v1)
  c->vsub(DEST::xyz, vf8, vf8, vf13);               // vsub.xyz vf8, vf8, vf13
  c->sqc2(vf12, 96, v1);                            // sqc2 vf12, 96(v1)
  c->vmula_bc(DEST::xyzw, BC::w, vf18, vf0);        // vmulaw.xyzw acc, vf18, vf0
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(a0)) <= 0;                   // blez a0, L295
  c->vmadda_bc(DEST::xyzw, BC::x, vf15, vf5);       // vmaddax.xyzw acc, vf15, vf5
  if (bc) {goto block_8;}                           // branch non-likely

  c->vmadda_bc(DEST::xyzw, BC::y, vf16, vf5);       // vmadday.xyzw acc, vf16, vf5
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf5, vf17, vf5);   // vmaddz.xyzw vf5, vf17, vf5
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf18, vf0);        // vmulaw.xyzw acc, vf18, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf15, vf6);       // vmaddax.xyzw acc, vf15, vf6
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf16, vf6);       // vmadday.xyzw acc, vf16, vf6
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf6, vf17, vf6);   // vmaddz.xyzw vf6, vf17, vf6
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf18, vf0);        // vmulaw.xyzw acc, vf18, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf15, vf7);       // vmaddax.xyzw acc, vf15, vf7
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf16, vf7);       // vmadday.xyzw acc, vf16, vf7
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf7, vf17, vf7);   // vmaddz.xyzw vf7, vf17, vf7
  c->daddiu(v1, v1, 256);                           // daddiu v1, v1, 256
  c->vmula_bc(DEST::xyzw, BC::w, vf18, vf0);        // vmulaw.xyzw acc, vf18, vf0
  // Unknown instr: vlqi.xyz vf1, vi1
  vlqi(c, vf1);
  c->vmadda_bc(DEST::xyzw, BC::x, vf15, vf8);       // vmaddax.xyzw acc, vf15, vf8
  // Unknown instr: vlqi.xyz vf2, vi1
  vlqi(c, vf2);
  c->vmadda_bc(DEST::xyzw, BC::y, vf16, vf8);       // vmadday.xyzw acc, vf16, vf8
  // Unknown instr: vlqi.xyz vf3, vi1
  vlqi(c, vf3);
  c->vmadd_bc(DEST::xyzw, BC::z, vf8, vf17, vf8);   // vmaddz.xyzw vf8, vf17, vf8
  // Unknown instr: vlqi.xyz vf4, vi1
  vlqi(c, vf4);
  c->vftoi0(DEST::xyzw, vf9, vf5);                  // vftoi0.xyzw vf9, vf5
  c->sqc2(vf5, -112, v1);                           // sqc2 vf5, -112(v1)
  c->vftoi0(DEST::xyzw, vf10, vf6);                 // vftoi0.xyzw vf10, vf6
  c->sqc2(vf6, -80, v1);                            // sqc2 vf6, -80(v1)
  c->vftoi0(DEST::xyzw, vf11, vf7);                 // vftoi0.xyzw vf11, vf7
  c->sqc2(vf7, -48, v1);                            // sqc2 vf7, -48(v1)
  c->vftoi0(DEST::xyzw, vf12, vf8);                 // vftoi0.xyzw vf12, vf8
  c->sqc2(vf8, -16, v1);                            // sqc2 vf8, -16(v1)
  c->daddiu(a0, a0, -4);                            // daddiu a0, a0, -4
  c->sqc2(vf9, -128, v1);                           // sqc2 vf9, -128(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf10, -96, v1);                           // sqc2 vf10, -96(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf11, -64, v1);                           // sqc2 vf11, -64(v1)
  bc = ((s64)c->sgpr64(a0)) <= 0;                   // blez a0, L295
  c->sqc2(vf12, -32, v1);                           // sqc2 vf12, -32(v1)
  if (bc) {goto block_8;}                           // branch non-likely


  block_3:
  c->vsub(DEST::xyz, vf1, vf1, vf13);               // vsub.xyz vf1, vf1, vf13
  // Unknown instr: vlqi.xyz vf5, vi1
  vlqi(c, vf5);
  c->vsub(DEST::xyz, vf2, vf2, vf13);               // vsub.xyz vf2, vf2, vf13
  // Unknown instr: vlqi.xyz vf6, vi1
  vlqi(c, vf6);
  c->vsub(DEST::xyz, vf3, vf3, vf13);               // vsub.xyz vf3, vf3, vf13
  // Unknown instr: vlqi.xyz vf7, vi1
  vlqi(c, vf7);
  c->vsub(DEST::xyz, vf4, vf4, vf13);               // vsub.xyz vf4, vf4, vf13
  // Unknown instr: vlqi.xyz vf8, vi1
  vlqi(c, vf8);
  c->vmula_bc(DEST::xyzw, BC::w, vf18, vf0);        // vmulaw.xyzw acc, vf18, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf15, vf1);       // vmaddax.xyzw acc, vf15, vf1
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf16, vf1);       // vmadday.xyzw acc, vf16, vf1
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf1, vf17, vf1);   // vmaddz.xyzw vf1, vf17, vf1
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf18, vf0);        // vmulaw.xyzw acc, vf18, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf15, vf2);       // vmaddax.xyzw acc, vf15, vf2
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf16, vf2);       // vmadday.xyzw acc, vf16, vf2
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf2, vf17, vf2);   // vmaddz.xyzw vf2, vf17, vf2
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf18, vf0);        // vmulaw.xyzw acc, vf18, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf15, vf3);       // vmaddax.xyzw acc, vf15, vf3
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf16, vf3);       // vmadday.xyzw acc, vf16, vf3
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf3, vf17, vf3);   // vmaddz.xyzw vf3, vf17, vf3
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf18, vf0);        // vmulaw.xyzw acc, vf18, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf15, vf4);       // vmaddax.xyzw acc, vf15, vf4
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf16, vf4);       // vmadday.xyzw acc, vf16, vf4
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf4, vf17, vf4);   // vmaddz.xyzw vf4, vf17, vf4
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf9, vf1);                  // vftoi0.xyzw vf9, vf1
  c->sqc2(vf1, 16, v1);                             // sqc2 vf1, 16(v1)
  c->vftoi0(DEST::xyzw, vf10, vf2);                 // vftoi0.xyzw vf10, vf2
  c->sqc2(vf2, 48, v1);                             // sqc2 vf2, 48(v1)
  c->vftoi0(DEST::xyzw, vf11, vf3);                 // vftoi0.xyzw vf11, vf3
  c->sqc2(vf3, 80, v1);                             // sqc2 vf3, 80(v1)
  c->vftoi0(DEST::xyzw, vf12, vf4);                 // vftoi0.xyzw vf12, vf4
  c->sqc2(vf4, 112, v1);                            // sqc2 vf4, 112(v1)
  c->vsub(DEST::xyz, vf5, vf5, vf13);               // vsub.xyz vf5, vf5, vf13
  c->sqc2(vf9, 0, v1);                              // sqc2 vf9, 0(v1)
  c->vsub(DEST::xyz, vf6, vf6, vf13);               // vsub.xyz vf6, vf6, vf13
  c->sqc2(vf10, 32, v1);                            // sqc2 vf10, 32(v1)
  c->vsub(DEST::xyz, vf7, vf7, vf13);               // vsub.xyz vf7, vf7, vf13
  c->sqc2(vf11, 64, v1);                            // sqc2 vf11, 64(v1)
  c->vsub(DEST::xyz, vf8, vf8, vf13);               // vsub.xyz vf8, vf8, vf13
  c->sqc2(vf12, 96, v1);                            // sqc2 vf12, 96(v1)
  c->daddiu(a0, a0, -4);                            // daddiu a0, a0, -4
  c->vmula_bc(DEST::xyzw, BC::w, vf18, vf0);        // vmulaw.xyzw acc, vf18, vf0
  bc = ((s64)c->sgpr64(a0)) <= 0;                   // blez a0, L292
  c->vmadda_bc(DEST::xyzw, BC::x, vf15, vf5);       // vmaddax.xyzw acc, vf15, vf5
  if (bc) {goto block_5;}                           // branch non-likely

  c->vmadda_bc(DEST::xyzw, BC::y, vf16, vf5);       // vmadday.xyzw acc, vf16, vf5
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf5, vf17, vf5);   // vmaddz.xyzw vf5, vf17, vf5
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf18, vf0);        // vmulaw.xyzw acc, vf18, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf15, vf6);       // vmaddax.xyzw acc, vf15, vf6
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf16, vf6);       // vmadday.xyzw acc, vf16, vf6
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf6, vf17, vf6);   // vmaddz.xyzw vf6, vf17, vf6
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf18, vf0);        // vmulaw.xyzw acc, vf18, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf15, vf7);       // vmaddax.xyzw acc, vf15, vf7
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf16, vf7);       // vmadday.xyzw acc, vf16, vf7
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf7, vf17, vf7);   // vmaddz.xyzw vf7, vf17, vf7
  c->daddiu(v1, v1, 256);                           // daddiu v1, v1, 256
  c->vmula_bc(DEST::xyzw, BC::w, vf18, vf0);        // vmulaw.xyzw acc, vf18, vf0
  // Unknown instr: vlqi.xyz vf1, vi1
  vlqi(c, vf1);
  c->vmadda_bc(DEST::xyzw, BC::x, vf15, vf8);       // vmaddax.xyzw acc, vf15, vf8
  // Unknown instr: vlqi.xyz vf2, vi1
  vlqi(c, vf2);
  c->vmadda_bc(DEST::xyzw, BC::y, vf16, vf8);       // vmadday.xyzw acc, vf16, vf8
  // Unknown instr: vlqi.xyz vf3, vi1
  vlqi(c, vf3);
  c->vmadd_bc(DEST::xyzw, BC::z, vf8, vf17, vf8);   // vmaddz.xyzw vf8, vf17, vf8
  // Unknown instr: vlqi.xyz vf4, vi1
  vlqi(c, vf4);
  c->vftoi0(DEST::xyzw, vf9, vf5);                  // vftoi0.xyzw vf9, vf5
  c->sqc2(vf5, -112, v1);                           // sqc2 vf5, -112(v1)
  c->vftoi0(DEST::xyzw, vf10, vf6);                 // vftoi0.xyzw vf10, vf6
  c->sqc2(vf6, -80, v1);                            // sqc2 vf6, -80(v1)
  c->vftoi0(DEST::xyzw, vf11, vf7);                 // vftoi0.xyzw vf11, vf7
  c->sqc2(vf7, -48, v1);                            // sqc2 vf7, -48(v1)
  c->vftoi0(DEST::xyzw, vf12, vf8);                 // vftoi0.xyzw vf12, vf8
  c->sqc2(vf8, -16, v1);                            // sqc2 vf8, -16(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf9, -128, v1);                           // sqc2 vf9, -128(v1)
  c->daddiu(a0, a0, -4);                            // daddiu a0, a0, -4
  c->sqc2(vf10, -96, v1);                           // sqc2 vf10, -96(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf11, -64, v1);                           // sqc2 vf11, -64(v1)
  bc = ((s64)c->sgpr64(a0)) > 0;                    // bgtz a0, L291
  c->sqc2(vf12, -32, v1);                           // sqc2 vf12, -32(v1)
  if (bc) {goto block_3;}                           // branch non-likely


  block_5:
  //beq r0, r0, L295                                // beq r0, r0, L295
  // nop                                            // sll r0, r0, 0
  goto block_8;                                     // branch always


  block_6:
  // c->lui(v1, 28672);                                // lui v1, 28672
  get_fake_spad_addr(v1, cache.fake_scratchpad_data, 0, c);

  c->lqc2(vf14, 12, a1);                            // lqc2 vf14, 12(a1)
  c->vmove(DEST::xyzw, vf1, vf0);                   // vmove.xyzw vf1, vf0
  // c->lqc2(vf13, 0, a0);                             // lqc2 vf13, 0(a0)
  c->vfs[vf13].du32[0] = 0x4d000000;
  c->vfs[vf13].du32[1] = 0x4d000000;
  c->vfs[vf13].du32[2] = 0x4d000000;
  c->vfs[vf13].du32[3] =  0;
  c->vmove(DEST::xyzw, vf2, vf0);                   // vmove.xyzw vf2, vf0
  c->lbu(a0, 24, a1);                               // lbu a0, 24(a1)
  // Unknown instr: ctc2.i vi1, a3
  assert(c->sgpr64(a3) == 0);
  vi1 = c->sgpr64(a3);
  c->vmove(DEST::xyzw, vf3, vf0);                   // vmove.xyzw vf3, vf0
  c->vitof0(DEST::xyzw, vf14, vf14);                // vitof0.xyzw vf14, vf14
  c->vmove(DEST::xyzw, vf4, vf0);                   // vmove.xyzw vf4, vf0
  // nop                                            // sll r0, r0, 0
  c->vmove(DEST::xyzw, vf5, vf0);                   // vmove.xyzw vf5, vf0
  // nop                                            // sll r0, r0, 0
  c->vmove(DEST::xyzw, vf6, vf0);                   // vmove.xyzw vf6, vf0
  // nop                                            // sll r0, r0, 0
  c->vmove(DEST::xyzw, vf7, vf0);                   // vmove.xyzw vf7, vf0
  c->vsub(DEST::xyzw, vf13, vf13, vf14);            // vsub.xyzw vf13, vf13, vf14
  c->vmove(DEST::xyzw, vf8, vf0);                   // vmove.xyzw vf8, vf0

  block_7:
  // nop                                            // sll r0, r0, 0
  // Unknown instr: vlqi.xyz vf1, vi1
  vlqi(c, vf1);
  // nop                                            // sll r0, r0, 0
  // Unknown instr: vlqi.xyz vf2, vi1
  vlqi(c, vf2);
  // nop                                            // sll r0, r0, 0
  // Unknown instr: vlqi.xyz vf3, vi1
  vlqi(c, vf3);
  c->daddiu(v1, v1, 256);                           // daddiu v1, v1, 256
  // Unknown instr: vlqi.xyz vf4, vi1
  vlqi(c, vf4);
  c->vsub(DEST::xyz, vf1, vf1, vf13);               // vsub.xyz vf1, vf1, vf13
  // Unknown instr: vlqi.xyz vf5, vi1
  vlqi(c, vf5);
  c->vsub(DEST::xyz, vf2, vf2, vf13);               // vsub.xyz vf2, vf2, vf13
  // Unknown instr: vlqi.xyz vf6, vi1
  vlqi(c, vf6);
  c->vsub(DEST::xyz, vf3, vf3, vf13);               // vsub.xyz vf3, vf3, vf13
  // Unknown instr: vlqi.xyz vf7, vi1
  vlqi(c, vf7);
  c->vsub(DEST::xyz, vf4, vf4, vf13);               // vsub.xyz vf4, vf4, vf13
  // Unknown instr: vlqi.xyz vf8, vi1
  vlqi(c, vf8);
  c->vftoi0(DEST::xyzw, vf9, vf1);                  // vftoi0.xyzw vf9, vf1
  c->sqc2(vf1, -240, v1);                           // sqc2 vf1, -240(v1)
  c->vftoi0(DEST::xyzw, vf10, vf2);                 // vftoi0.xyzw vf10, vf2
  c->sqc2(vf2, -208, v1);                           // sqc2 vf2, -208(v1)
  c->vftoi0(DEST::xyzw, vf11, vf3);                 // vftoi0.xyzw vf11, vf3
  c->sqc2(vf3, -176, v1);                           // sqc2 vf3, -176(v1)
  c->vftoi0(DEST::xyzw, vf12, vf4);                 // vftoi0.xyzw vf12, vf4
  c->sqc2(vf4, -144, v1);                           // sqc2 vf4, -144(v1)
  c->vsub(DEST::xyz, vf5, vf5, vf13);               // vsub.xyz vf5, vf5, vf13
  c->sqc2(vf9, -256, v1);                           // sqc2 vf9, -256(v1)
  c->vsub(DEST::xyz, vf6, vf6, vf13);               // vsub.xyz vf6, vf6, vf13
  c->sqc2(vf10, -224, v1);                          // sqc2 vf10, -224(v1)
  c->vsub(DEST::xyz, vf7, vf7, vf13);               // vsub.xyz vf7, vf7, vf13
  c->sqc2(vf11, -192, v1);                          // sqc2 vf11, -192(v1)
  c->vsub(DEST::xyz, vf8, vf8, vf13);               // vsub.xyz vf8, vf8, vf13
  c->sqc2(vf12, -160, v1);                          // sqc2 vf12, -160(v1)
  c->vftoi0(DEST::xyzw, vf9, vf5);                  // vftoi0.xyzw vf9, vf5
  c->sqc2(vf5, -112, v1);                           // sqc2 vf5, -112(v1)
  c->vftoi0(DEST::xyzw, vf10, vf6);                 // vftoi0.xyzw vf10, vf6
  c->sqc2(vf6, -80, v1);                            // sqc2 vf6, -80(v1)
  c->vftoi0(DEST::xyzw, vf11, vf7);                 // vftoi0.xyzw vf11, vf7
  c->sqc2(vf7, -48, v1);                            // sqc2 vf7, -48(v1)
  c->vftoi0(DEST::xyzw, vf12, vf8);                 // vftoi0.xyzw vf12, vf8
  c->sqc2(vf8, -16, v1);                            // sqc2 vf8, -16(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf9, -128, v1);                           // sqc2 vf9, -128(v1)
  c->daddiu(a0, a0, -8);                            // daddiu a0, a0, -8
  c->sqc2(vf10, -96, v1);                           // sqc2 vf10, -96(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf11, -64, v1);                           // sqc2 vf11, -64(v1)
  bc = ((s64)c->sgpr64(a0)) > 0;                    // bgtz a0, L294
  c->sqc2(vf12, -32, v1);                           // sqc2 vf12, -32(v1)
  if (bc) {goto block_7;}                           // branch non-likely


  block_8:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("(method 32 collide-cache)", execute, 128);
}

} // namespace method_32_collide_cache
} // namespace Mips2C


namespace Mips2C {

// clip by box
namespace method_26_collide_cache {
struct Cache {
  void* already_printed_exeeded_max_cache_tris; // *already-printed-exeeded-max-cache-tris*
  void* cheat_mode; // *cheat-mode*
  void* stdcon; // *stdcon*
  void* debug; // debug
  void* format; // format
  void* fake_scratchpad_data;  // *fake-scratchpad-data*
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
  // nop                                            // sll r0, r0, 0
  c->addiu(v1, r0, 460);                            // addiu v1, r0, 460
  c->lwu(a2, 0, a0);                                // lwu a2, 0(a0)
  c->dsubu(t0, v1, a2);                             // dsubu t0, v1, a2
  c->addiu(a3, r0, 64);                             // addiu a3, r0, 64
  c->mult3(a3, a2, a3);                             // mult3 a3, a2, a3
  bc = ((s64)c->sgpr64(t0)) < 0;                    // bltz t0, L249
  c->mov64(t0, a0);                                 // or t0, a0, r0
  if (bc) {goto block_17;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lbu(t2, 25, a1);                               // lbu t2, 25(a1)
  c->daddiu(t0, t0, 4908);                          // daddiu t0, t0, 4908
  c->lwu(t1, 0, a1);                                // lwu t1, 0(a1)
  c->dsll(t2, t2, 4);                               // dsll t2, t2, 4
  c->lhu(t3, 8, a1);                                // lhu t3, 8(a1)
  c->daddu(a3, t0, a3);                             // daddu a3, t0, a3
  c->lq(t0, 60, a0);                                // lq t0, 60(a0)
  c->daddu(t1, t1, t2);                             // daddu t1, t1, t2
  c->lq(t2, 76, a0);                                // lq t2, 76(a0)
  // nop                                            // sll r0, r0, 0
  c->lwu(a1, 4, a1);                                // lwu a1, 4(a1)
  c->daddu(t3, t1, t3);                             // daddu t3, t1, t3
  c->lwu(t4, 8, a0);                                // lwu t4, 8(a0)

  block_2:
  // c->lui(t5, 28672);                                // lui t5, 28672
  get_fake_spad_addr(t5, cache.fake_scratchpad_data, 0, c);
  c->lb(t7, 0, t1);                                 // lb t7, 0(t1)
  // nop                                            // sll r0, r0, 0
  c->lb(t8, 1, t1);                                 // lb t8, 1(t1)
  bc = ((s64)c->sgpr64(t7)) < 0;                    // bltz t7, L248
  c->lb(t6, 2, t1);                                 // lb t6, 2(t1)
  if (bc) {goto block_16;}                          // branch non-likely

  c->dsll(t7, t7, 5);                               // dsll t7, t7, 5
  c->dsll(t8, t8, 5);                               // dsll t8, t8, 5
  c->dsll(t9, t6, 5);                               // dsll t9, t6, 5
  c->daddu(t6, t7, t5);                             // daddu t6, t7, t5
  c->daddu(t7, t8, t5);                             // daddu t7, t8, t5
  c->lq(t8, 0, t6);                                 // lq t8, 0(t6)
  c->daddu(ra, t9, t5);                             // daddu ra, t9, t5
  c->lq(t9, 0, t7);                                 // lq t9, 0(t7)
  c->pminw(s4, t8, t9);                             // pminw s4, t8, t9
  c->lq(gp, 0, ra);                                 // lq gp, 0(ra)
  c->pmaxw(s5, t8, t9);                             // pmaxw s5, t8, t9
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pminw(s4, s4, gp);                             // pminw s4, s4, gp
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaxw(s5, s5, gp);                             // pmaxw s5, s5, gp
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(s4, s4, t2);                             // pcgtw s4, s4, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(s5, t0, s5);                             // pcgtw s5, t0, s5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->por(s4, s4, s5);                               // por s4, s4, s5
  c->lbu(s5, 0, t3);                                // lbu s5, 0(t3)
  c->ppach(s4, r0, s4);                             // ppach s4, r0, s4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsll(s4, s4, 16);                              // dsll s4, s4, 16
  c->dsll(s5, s5, 2);                               // dsll s5, s5, 2
  bc = c->sgpr64(s4) != 0;                          // bne s4, r0, L245
  c->daddu(s5, s5, a1);                             // daddu s5, s5, a1
  if (bc) {goto block_7;}                           // branch non-likely

  bc = c->sgpr64(a2) == c->sgpr64(v1);              // beq a2, v1, L249
  c->lwu(s5, 0, s5);                                // lwu s5, 0(s5)
  if (bc) {goto block_17;}                          // branch non-likely

  c->and_(s4, s5, t4);                              // and s4, s5, t4
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s4) != 0;                          // bne s4, r0, L245
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->sq(r0, 48, a3);                                // sq r0, 48(a3)
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  c->sw(s5, 48, a3);                                // sw s5, 48(a3)
  c->daddiu(a3, a3, 64);                            // daddiu a3, a3, 64
  c->lq(s5, 16, t6);                                // lq s5, 16(t6)
  // nop                                            // sll r0, r0, 0
  c->lq(s4, 16, t7);                                // lq s4, 16(t7)
  // nop                                            // sll r0, r0, 0
  c->lq(s3, 16, ra);                                // lq s3, 16(ra)
  // nop                                            // sll r0, r0, 0
  c->sq(s5, -64, a3);                               // sq s5, -64(a3)
  // nop                                            // sll r0, r0, 0
  c->sq(s4, -48, a3);                               // sq s4, -48(a3)
  // nop                                            // sll r0, r0, 0
  c->sq(s3, -32, a3);                               // sq s3, -32(a3)

  block_7:
  c->daddiu(t3, t3, 1);                             // daddiu t3, t3, 1
  c->daddiu(t1, t1, 3);                             // daddiu t1, t1, 3
  c->addiu(s5, r0, 16);                             // addiu s5, r0, 16
  // nop                                            // sll r0, r0, 0

  block_8:
  // nop                                            // sll r0, r0, 0
  c->lb(s4, 0, t1);                                 // lb s4, 0(t1)
  c->daddiu(t1, t1, 1);                             // daddiu t1, t1, 1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s4) == 0;                          // beq s4, r0, L244
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  if (((s64)c->sgpr64(s4)) < 0) {                   // bltzl s4, L247
    c->dsubu(s4, r0, s4);                           // dsubu s4, r0, s4
    goto block_12;
  }

  block_11:
  c->mov128_gpr_gpr(t8, t9);                        // por t8, t9, r0
  c->mov64(t6, t7);                                 // or t6, t7, r0
  c->dsubu(s5, r0, s5);                             // dsubu s5, r0, s5
  // nop                                            // sll r0, r0, 0

  block_12:
  c->daddiu(s4, s4, -1);                            // daddiu s4, s4, -1
  c->mov64(t7, ra);                                 // or t7, ra, r0
  c->mov128_gpr_gpr(t9, gp);                        // por t9, gp, r0
  c->dsll(ra, s4, 5);                               // dsll ra, s4, 5
  c->daddu(ra, ra, t5);                             // daddu ra, ra, t5
  c->lbu(s4, 0, t3);                                // lbu s4, 0(t3)
  c->pminw(s2, t8, t9);                             // pminw s2, t8, t9
  c->lq(gp, 0, ra);                                 // lq gp, 0(ra)
  c->pmaxw(s3, t8, t9);                             // pmaxw s3, t8, t9
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pminw(s2, s2, gp);                             // pminw s2, s2, gp
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaxw(s3, s3, gp);                             // pmaxw s3, s3, gp
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(s2, s2, t2);                             // pcgtw s2, s2, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(s3, t0, s3);                             // pcgtw s3, t0, s3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->por(s3, s2, s3);                               // por s3, s2, s3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppach(s3, r0, s3);                             // ppach s3, r0, s3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsll(s3, s3, 16);                              // dsll s3, s3, 16
  c->dsll(s4, s4, 2);                               // dsll s4, s4, 2
  bc = c->sgpr64(s3) != 0;                          // bne s3, r0, L246
  c->daddiu(t3, t3, 1);                             // daddiu t3, t3, 1
  if (bc) {goto block_8;}                           // branch non-likely

  c->daddu(s4, s4, a1);                             // daddu s4, s4, a1
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lwu(s4, 0, s4);                                // lwu s4, 0(s4)
  c->and_(s3, s4, t4);                              // and s3, s4, t4
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s3) != 0;                          // bne s3, r0, L246
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_8;}                           // branch non-likely

  bc = c->sgpr64(a2) == c->sgpr64(v1);              // beq a2, v1, L249
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_17;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->sq(r0, 48, a3);                                // sq r0, 48(a3)
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  c->sw(s4, 48, a3);                                // sw s4, 48(a3)
  c->daddiu(s2, a3, 16);                            // daddiu s2, a3, 16
  c->lq(s1, 16, t7);                                // lq s1, 16(t7)
  c->daddiu(a3, a3, 64);                            // daddiu a3, a3, 64
  c->lq(s4, 16, t6);                                // lq s4, 16(t6)
  // nop                                            // sll r0, r0, 0
  c->lq(s3, 16, ra);                                // lq s3, 16(ra)
  // nop                                            // sll r0, r0, 0
  c->sq(s1, 0, s2);                                 // sq s1, 0(s2)
  c->dsubu(s1, s2, s5);                             // dsubu s1, s2, s5
  c->daddu(s2, s2, s5);                             // daddu s2, s2, s5
  // nop                                            // sll r0, r0, 0
  c->sq(s4, 0, s1);                                 // sq s4, 0(s1)
  //beq r0, r0, L246                                // beq r0, r0, L246
  c->sq(s3, 0, s2);                                 // sq s3, 0(s2)
  goto block_8;                                     // branch always


  block_16:
  // nop                                            // sll r0, r0, 0
  c->sw(a2, 0, a0);                                 // sw a2, 0(a0)
  //beq r0, r0, L251                                // beq r0, r0, L251
  // nop                                            // sll r0, r0, 0
  goto block_21;                                    // branch always


  block_17:
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 0, a0);                                 // sw v1, 0(a0)
  c->load_symbol(v1, cache.already_printed_exeeded_max_cache_tris);// lw v1, *already-printed-exeeded-max-cache-tris*(s7)
  bc = c->sgpr64(s7) != c->sgpr64(v1);              // bne s7, v1, L250
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_20;}                          // branch non-likely

  c->daddiu(v1, s7, 8);                             // daddiu v1, s7, #t
  //c->store_symbol(v1, cache.already_printed_exeeded_max_cache_tris);// sw v1, *already-printed-exeeded-max-cache-tris*(s7)
  //c->load_symbol_addr(v1, cache.debug);             // daddiu v1, s7, debug
  c->load_symbol(a0, cache.cheat_mode);             // lw a0, *cheat-mode*(s7)
  bc = c->sgpr64(a0) != c->sgpr64(v1);              // bne a0, v1, L250
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_20;}                          // branch non-likely

  c->load_symbol(t9, cache.format);                 // lw t9, format(s7)
  c->load_symbol(a0, cache.stdcon);                 // lw a0, *stdcon*(s7)
  //daddiu a1, fp, L305                               // daddiu a1, fp, L305
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  //c->jalr(call_addr);                               // jalr ra, t9
  printf("exceeded maximum collide cache tris (should print on screen but too lazy for that now)\n");
  c->mov64(v1, v0);                                 // or v1, v0, r0

  block_20:
  //beq r0, r0, L251                                // beq r0, r0, L251
  // nop                                            // sll r0, r0, 0
  goto block_21;                                    // branch always


  block_21:
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
  cache.already_printed_exeeded_max_cache_tris = intern_from_c("*already-printed-exeeded-max-cache-tris*").c();
  cache.cheat_mode = intern_from_c("*cheat-mode*").c();
  cache.stdcon = intern_from_c("*stdcon*").c();
  cache.debug = intern_from_c("debug").c();
  cache.format = intern_from_c("format").c();
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("(method 26 collide-cache)", execute, 512);
}

} // namespace method_26_collide_cache
} // namespace Mips2C
