//--------------------------MIPS2C---------------------
#include "common/dma/gs.h"

#include "game/kernel/jak1/kscheme.h"
#include "game/mips2c/mips2c_private.h"
using namespace jak1;

const uint32_t* max_tri_count = nullptr;
namespace {
u32 vu0_buffer[1024];  // todo, maybe can be 512.
u32 vi1 = 0;

void vlqi(Mips2C::ExecutionContext* c, int reg) {
  memcpy(&c->vfs[reg].f[0], &vu0_buffer[vi1 * 4], 16);
  vi1++;
}
}  // namespace

namespace Mips2C::jak1 {

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

  // int qw_in_source = c->sgpr64(a1);
  int qw_to_write = c->sgpr64(a2);
  const u16* data_in = (const u16*)(g_ee_main_mem + c->sgpr64(a0));
  // I don't quite get why this is wrong sometimes.
  //  ASSERT(qw_to_write * 3 == qw_in_source * 8);
  ASSERT(qw_to_write <= 128);

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
  // u32 call_addr = 0;
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
  ASSERT(c->sgpr64(a3) == 0);
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
  ASSERT(c->sgpr64(a3) == 0);
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
  max_tri_count = intern_from_c("*collide-cache-max-tris*").cast<u32>().c();
}

} // namespace method_32_collide_cache
} // namespace Mips2C


namespace Mips2C::jak1 {

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
  [[maybe_unused]] u32 call_addr = 0;
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
  c->addiu(v1, r0, *max_tri_count);                 // addiu v1, r0, 460
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

  // block_11:
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

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace method_28_collide_cache {
struct Cache {
  void* already_printed_exeeded_max_cache_tris; // *already-printed-exeeded-max-cache-tris*
  void* cheat_mode; // *cheat-mode*
  void* stdcon; // *stdcon*
  void* debug; // debug
  void* format; // format
  void* fake_scratchpad_data;
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  [[maybe_unused]] u32 call_addr = 0;
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
  c->addiu(v1, r0, *max_tri_count);                 // addiu v1, r0, 460
  c->lwu(a2, 0, a0);                                // lwu a2, 0(a0)
  c->dsubu(t0, v1, a2);                             // dsubu t0, v1, a2
  c->addiu(a3, r0, 64);                             // addiu a3, r0, 64
  c->mult3(a3, a2, a3);                             // mult3 a3, a2, a3
  bc = ((s64)c->sgpr64(t0)) < 0;                    // bltz t0, L222
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
  bc = ((s64)c->sgpr64(t7)) < 0;                    // bltz t7, L221
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
  bc = c->sgpr64(s4) != 0;                          // bne s4, r0, L218
  c->daddu(s5, s5, a1);                             // daddu s5, s5, a1
  if (bc) {goto block_7;}                           // branch non-likely

  bc = c->sgpr64(a2) == c->sgpr64(v1);              // beq a2, v1, L222
  c->lwu(s5, 0, s5);                                // lwu s5, 0(s5)
  if (bc) {goto block_17;}                          // branch non-likely

  c->and_(s4, s5, t4);                              // and s4, s5, t4
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s4) != 0;                          // bne s4, r0, L218
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
  bc = c->sgpr64(s4) == 0;                          // beq s4, r0, L217
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  if (((s64)c->sgpr64(s4)) < 0) {                   // bltzl s4, L220
    c->dsubu(s4, r0, s4);                           // dsubu s4, r0, s4
    goto block_12;
  }

  // block_11:
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
  bc = c->sgpr64(s3) != 0;                          // bne s3, r0, L219
  c->daddiu(t3, t3, 1);                             // daddiu t3, t3, 1
  if (bc) {goto block_8;}                           // branch non-likely

  c->daddu(s4, s4, a1);                             // daddu s4, s4, a1
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lwu(s4, 0, s4);                                // lwu s4, 0(s4)
  c->and_(s3, s4, t4);                              // and s3, s4, t4
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s3) != 0;                          // bne s3, r0, L219
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_8;}                           // branch non-likely

  bc = c->sgpr64(a2) == c->sgpr64(v1);              // beq a2, v1, L222
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
  //beq r0, r0, L219                                // beq r0, r0, L219
  c->sq(s3, 0, s2);                                 // sq s3, 0(s2)
  goto block_8;                                     // branch always


  block_16:
  // nop                                            // sll r0, r0, 0
  c->sw(a2, 0, a0);                                 // sw a2, 0(a0)
  //beq r0, r0, L224                                // beq r0, r0, L224
  // nop                                            // sll r0, r0, 0
  goto block_21;                                    // branch always


  block_17:
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 0, a0);                                 // sw v1, 0(a0)
  c->load_symbol(v1, cache.already_printed_exeeded_max_cache_tris);// lw v1, *already-printed-exeeded-max-cache-tris*(s7)
  bc = c->sgpr64(s7) != c->sgpr64(v1);              // bne s7, v1, L223
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_20;}                          // branch non-likely

  c->daddiu(v1, s7, 8);                             // daddiu v1, s7, #t
  //c->store_symbol(v1, cache.already_printed_exeeded_max_cache_tris);// sw v1, *already-printed-exeeded-max-cache-tris*(s7)
  //c->load_symbol_addr(v1, cache.debug);             // daddiu v1, s7, debug
  c->load_symbol(a0, cache.cheat_mode);             // lw a0, *cheat-mode*(s7)
  bc = c->sgpr64(a0) != c->sgpr64(v1);              // bne a0, v1, L223
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
  //beq r0, r0, L224                                // beq r0, r0, L224
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
  gLinkedFunctionTable.reg("(method 28 collide-cache)", execute, 512);
}

} // namespace method_28_collide_cache
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace method_27_collide_cache {
struct Cache {
  void* already_printed_exeeded_max_cache_tris; // *already-printed-exeeded-max-cache-tris*
  void* cheat_mode; // *cheat-mode*
  void* collide_work; // *collide-work*
  void* stdcon; // *stdcon*
  void* debug; // debug
  void* format; // format
  void* fake_scratchpad_data;
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
  c->mov64(s5, a1);                                 // or s5, a1, r0
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 132, v1);                              // lwu t9, 132(v1)
  c->mov64(a1, s5);                                 // or a1, s5, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  // nop                                            // sll r0, r0, 0
  c->load_symbol(t0, cache.collide_work);           // lw t0, *collide-work*(s7)
  c->addiu(v1, r0, *max_tri_count);                 // addiu v1, r0, 460
  c->lwu(a0, 0, gp);                                // lwu a0, 0(gp)
  c->dsubu(a2, v1, a0);                             // dsubu a2, v1, a0
  c->dsll(a1, a0, 6);                               // dsll a1, a0, 6
  bc = ((s64)c->sgpr64(a2)) < 0;                    // bltz a2, L179
  c->mov64(a2, gp);                                 // or a2, gp, r0
  if (bc) {goto block_17;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lbu(t1, 25, s5);                               // lbu t1, 25(s5)
  c->daddiu(a2, a2, 4908);                          // daddiu a2, a2, 4908
  c->lwu(a3, 0, s5);                                // lwu a3, 0(s5)
  c->dsll(t1, t1, 4);                               // dsll t1, t1, 4
  c->lhu(t2, 8, s5);                                // lhu t2, 8(s5)
  c->daddu(a1, a2, a1);                             // daddu a1, a2, a1
  c->lq(a2, 16, t0);                                // lq a2, 16(t0)
  c->daddu(a3, a3, t1);                             // daddu a3, a3, t1
  c->lq(t0, 32, t0);                                // lq t0, 32(t0)
  // nop                                            // sll r0, r0, 0
  c->lwu(t1, 4, s5);                                // lwu t1, 4(s5)
  c->daddu(t2, a3, t2);                             // daddu t2, a3, t2
  c->lwu(t3, 8, gp);                                // lwu t3, 8(gp)

  block_2:
  // c->lui(t4, 28672);                                // lui t4, 28672
  get_fake_spad_addr(t4, cache.fake_scratchpad_data, 0, c);
  c->lb(t6, 0, a3);                                 // lb t6, 0(a3)
  // nop                                            // sll r0, r0, 0
  c->lb(t7, 1, a3);                                 // lb t7, 1(a3)
  bc = ((s64)c->sgpr64(t6)) < 0;                    // bltz t6, L178
  c->lb(t5, 2, a3);                                 // lb t5, 2(a3)
  if (bc) {goto block_16;}                          // branch non-likely

  c->dsll(t6, t6, 5);                               // dsll t6, t6, 5
  c->dsll(t7, t7, 5);                               // dsll t7, t7, 5
  c->dsll(t8, t5, 5);                               // dsll t8, t5, 5
  c->daddu(t5, t6, t4);                             // daddu t5, t6, t4
  c->daddu(t6, t7, t4);                             // daddu t6, t7, t4
  c->lq(t7, 4096, t5);                              // lq t7, 4096(t5)
  c->daddu(t9, t8, t4);                             // daddu t9, t8, t4
  c->lq(t8, 4096, t6);                              // lq t8, 4096(t6)
  c->pminw(s4, t7, t8);                             // pminw s4, t7, t8
  c->lq(ra, 4096, t9);                              // lq ra, 4096(t9)
  c->pmaxw(s5, t7, t8);                             // pmaxw s5, t7, t8
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pminw(s4, s4, ra);                             // pminw s4, s4, ra
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaxw(s5, s5, ra);                             // pmaxw s5, s5, ra
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(s4, s4, t0);                             // pcgtw s4, s4, t0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(s5, a2, s5);                             // pcgtw s5, a2, s5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->por(s4, s4, s5);                               // por s4, s4, s5
  c->lbu(s5, 0, t2);                                // lbu s5, 0(t2)
  c->ppach(s4, r0, s4);                             // ppach s4, r0, s4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsll(s4, s4, 16);                              // dsll s4, s4, 16
  c->dsll(s5, s5, 2);                               // dsll s5, s5, 2
  bc = c->sgpr64(s4) != 0;                          // bne s4, r0, L175
  c->daddu(s5, s5, t1);                             // daddu s5, s5, t1
  if (bc) {goto block_7;}                           // branch non-likely

  bc = c->sgpr64(a0) == c->sgpr64(v1);              // beq a0, v1, L179
  c->lwu(s5, 0, s5);                                // lwu s5, 0(s5)
  if (bc) {goto block_17;}                          // branch non-likely

  c->and_(s4, s5, t3);                              // and s4, s5, t3
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s4) != 0;                          // bne s4, r0, L175
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->sq(r0, 48, a1);                                // sq r0, 48(a1)
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  c->sw(s5, 48, a1);                                // sw s5, 48(a1)
  c->daddiu(a1, a1, 64);                            // daddiu a1, a1, 64
  c->lq(s5, 16, t5);                                // lq s5, 16(t5)
  // nop                                            // sll r0, r0, 0
  c->lq(s4, 16, t6);                                // lq s4, 16(t6)
  // nop                                            // sll r0, r0, 0
  c->lq(s3, 16, t9);                                // lq s3, 16(t9)
  // nop                                            // sll r0, r0, 0
  c->sq(s5, -64, a1);                               // sq s5, -64(a1)
  // nop                                            // sll r0, r0, 0
  c->sq(s4, -48, a1);                               // sq s4, -48(a1)
  // nop                                            // sll r0, r0, 0
  c->sq(s3, -32, a1);                               // sq s3, -32(a1)

  block_7:
  c->daddiu(t2, t2, 1);                             // daddiu t2, t2, 1
  c->daddiu(a3, a3, 3);                             // daddiu a3, a3, 3
  c->addiu(s5, r0, 16);                             // addiu s5, r0, 16
  // nop                                            // sll r0, r0, 0

  block_8:
  // nop                                            // sll r0, r0, 0
  c->lb(s4, 0, a3);                                 // lb s4, 0(a3)
  c->daddiu(a3, a3, 1);                             // daddiu a3, a3, 1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s4) == 0;                          // beq s4, r0, L174
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  if (((s64)c->sgpr64(s4)) < 0) {                   // bltzl s4, L177
    c->dsubu(s4, r0, s4);                           // dsubu s4, r0, s4
    goto block_12;
  }

  // block_11:
  c->mov128_gpr_gpr(t7, t8);                        // por t7, t8, r0
  c->mov64(t5, t6);                                 // or t5, t6, r0
  c->dsubu(s5, r0, s5);                             // dsubu s5, r0, s5
  // nop                                            // sll r0, r0, 0

  block_12:
  c->daddiu(s4, s4, -1);                            // daddiu s4, s4, -1
  c->mov64(t6, t9);                                 // or t6, t9, r0
  c->mov128_gpr_gpr(t8, ra);                        // por t8, ra, r0
  c->dsll(t9, s4, 5);                               // dsll t9, s4, 5
  c->daddu(t9, t9, t4);                             // daddu t9, t9, t4
  c->lbu(s4, 0, t2);                                // lbu s4, 0(t2)
  c->pminw(s2, t7, t8);                             // pminw s2, t7, t8
  c->lq(ra, 4096, t9);                              // lq ra, 4096(t9)
  c->pmaxw(s3, t7, t8);                             // pmaxw s3, t7, t8
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pminw(s2, s2, ra);                             // pminw s2, s2, ra
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaxw(s3, s3, ra);                             // pmaxw s3, s3, ra
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(s2, s2, t0);                             // pcgtw s2, s2, t0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(s3, a2, s3);                             // pcgtw s3, a2, s3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->por(s3, s2, s3);                               // por s3, s2, s3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppach(s3, r0, s3);                             // ppach s3, r0, s3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsll(s3, s3, 16);                              // dsll s3, s3, 16
  c->dsll(s4, s4, 2);                               // dsll s4, s4, 2
  bc = c->sgpr64(s3) != 0;                          // bne s3, r0, L176
  c->daddiu(t2, t2, 1);                             // daddiu t2, t2, 1
  if (bc) {goto block_8;}                           // branch non-likely

  c->daddu(s4, s4, t1);                             // daddu s4, s4, t1
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lwu(s4, 0, s4);                                // lwu s4, 0(s4)
  c->and_(s3, s4, t3);                              // and s3, s4, t3
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s3) != 0;                          // bne s3, r0, L176
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_8;}                           // branch non-likely

  bc = c->sgpr64(a0) == c->sgpr64(v1);              // beq a0, v1, L179
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_17;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->sq(r0, 48, a1);                                // sq r0, 48(a1)
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  c->sw(s4, 48, a1);                                // sw s4, 48(a1)
  c->daddiu(s2, a1, 16);                            // daddiu s2, a1, 16
  c->lq(s1, 16, t6);                                // lq s1, 16(t6)
  c->daddiu(a1, a1, 64);                            // daddiu a1, a1, 64
  c->lq(s4, 16, t5);                                // lq s4, 16(t5)
  // nop                                            // sll r0, r0, 0
  c->lq(s3, 16, t9);                                // lq s3, 16(t9)
  // nop                                            // sll r0, r0, 0
  c->sq(s1, 0, s2);                                 // sq s1, 0(s2)
  c->dsubu(s1, s2, s5);                             // dsubu s1, s2, s5
  c->daddu(s2, s2, s5);                             // daddu s2, s2, s5
  // nop                                            // sll r0, r0, 0
  c->sq(s4, 0, s1);                                 // sq s4, 0(s1)
  //beq r0, r0, L176                                // beq r0, r0, L176
  c->sq(s3, 0, s2);                                 // sq s3, 0(s2)
  goto block_8;                                     // branch always


  block_16:
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 0, gp);                                 // sw a0, 0(gp)
  //beq r0, r0, L181                                // beq r0, r0, L181
  // nop                                            // sll r0, r0, 0
  goto block_21;                                    // branch always


  block_17:
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 0, gp);                                 // sw v1, 0(gp)
  c->load_symbol(v1, cache.already_printed_exeeded_max_cache_tris);// lw v1, *already-printed-exeeded-max-cache-tris*(s7)
  bc = c->sgpr64(s7) != c->sgpr64(v1);              // bne s7, v1, L180
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_20;}                          // branch non-likely

  c->daddiu(v1, s7, 8);                             // daddiu v1, s7, #t
  // c->store_symbol(v1, cache.already_printed_exeeded_max_cache_tris);// sw v1, *already-printed-exeeded-max-cache-tris*(s7)
  // c->load_symbol_addr(v1, cache.debug);             // daddiu v1, s7, debug
  c->load_symbol(a0, cache.cheat_mode);             // lw a0, *cheat-mode*(s7)
  bc = c->sgpr64(a0) != c->sgpr64(v1);              // bne a0, v1, L180
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
  // nop                                            // sll r0, r0, 0

  block_20:
  //beq r0, r0, L181                                // beq r0, r0, L181
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
  cache.collide_work = intern_from_c("*collide-work*").c();
  cache.stdcon = intern_from_c("*stdcon*").c();
  cache.debug = intern_from_c("debug").c();
  cache.format = intern_from_c("format").c();
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("(method 27 collide-cache)", execute, 512);
}

} // namespace method_27_collide_cache
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace method_29_collide_cache {
struct Cache {
  void* collide_work; // *collide-work*
  void* fake_scratchpad_data;
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  // u32 call_addr = 0;
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->load_symbol(a0, cache.collide_work);           // lw a0, *collide-work*(s7)
  // c->lui(v1, 28672);                                // lui v1, 28672
  get_fake_spad_addr(v1, cache.fake_scratchpad_data, 0, c);
  c->lbu(a1, 24, a1);                               // lbu a1, 24(a1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf1, 48, a0);                             // lqc2 vf1, 48(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 64, a0);                             // lqc2 vf2, 64(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 80, a0);                             // lqc2 vf3, 80(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf4, 96, a0);                             // lqc2 vf4, 96(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf5, 16, v1);                             // lqc2 vf5, 16(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf6, 48, v1);                             // lqc2 vf6, 48(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf7, 80, v1);                             // lqc2 vf7, 80(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf8, 112, v1);                            // lqc2 vf8, 112(v1)
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  c->lqc2(vf9, 144, v1);                            // lqc2 vf9, 144(v1)
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf5);        // vmaddax.xyzw acc, vf1, vf5
  c->lqc2(vf10, 176, v1);                           // lqc2 vf10, 176(v1)
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf5);        // vmadday.xyzw acc, vf2, vf5
  c->lqc2(vf11, 208, v1);                           // lqc2 vf11, 208(v1)
  c->vmadd_bc(DEST::xyzw, BC::z, vf5, vf3, vf5);    // vmaddz.xyzw vf5, vf3, vf5
  c->lqc2(vf12, 240, v1);                           // lqc2 vf12, 240(v1)
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
  c->vftoi0(DEST::xyzw, vf5, vf5);                  // vftoi0.xyzw vf5, vf5
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf6, vf6);                  // vftoi0.xyzw vf6, vf6
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf7, vf7);                  // vftoi0.xyzw vf7, vf7
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf8, vf8);                  // vftoi0.xyzw vf8, vf8
  // nop                                            // sll r0, r0, 0

  block_1:
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  c->sqc2(vf5, 4096, v1);                           // sqc2 vf5, 4096(v1)
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf9);        // vmaddax.xyzw acc, vf1, vf9
  c->sqc2(vf6, 4128, v1);                           // sqc2 vf6, 4128(v1)
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf9);        // vmadday.xyzw acc, vf2, vf9
  c->sqc2(vf7, 4160, v1);                           // sqc2 vf7, 4160(v1)
  c->vmadd_bc(DEST::xyzw, BC::z, vf9, vf3, vf9);    // vmaddz.xyzw vf9, vf3, vf9
  c->sqc2(vf8, 4192, v1);                           // sqc2 vf8, 4192(v1)
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  c->lqc2(vf5, 272, v1);                            // lqc2 vf5, 272(v1)
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf10);       // vmaddax.xyzw acc, vf1, vf10
  c->lqc2(vf6, 304, v1);                            // lqc2 vf6, 304(v1)
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf10);       // vmadday.xyzw acc, vf2, vf10
  c->lqc2(vf7, 336, v1);                            // lqc2 vf7, 336(v1)
  c->vmadd_bc(DEST::xyzw, BC::z, vf10, vf3, vf10);  // vmaddz.xyzw vf10, vf3, vf10
  c->lqc2(vf8, 368, v1);                            // lqc2 vf8, 368(v1)
  c->daddiu(v1, v1, 256);                           // daddiu v1, v1, 256
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf11);       // vmaddax.xyzw acc, vf1, vf11
  c->daddiu(a0, a1, -4);                            // daddiu a0, a1, -4
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf11);       // vmadday.xyzw acc, vf2, vf11
  bc = ((s64)c->sgpr64(a0)) <= 0;                   // blez a0, L172
  c->vmadd_bc(DEST::xyzw, BC::z, vf11, vf3, vf11);  // vmaddz.xyzw vf11, vf3, vf11
  if (bc) {goto block_4;}                           // branch non-likely

  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf12);       // vmaddax.xyzw acc, vf1, vf12
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf12);       // vmadday.xyzw acc, vf2, vf12
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf12, vf3, vf12);  // vmaddz.xyzw vf12, vf3, vf12
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf9, vf9);                  // vftoi0.xyzw vf9, vf9
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf10, vf10);                // vftoi0.xyzw vf10, vf10
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf11, vf11);                // vftoi0.xyzw vf11, vf11
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf12, vf12);                // vftoi0.xyzw vf12, vf12
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  c->sqc2(vf9, 3968, v1);                           // sqc2 vf9, 3968(v1)
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf5);        // vmaddax.xyzw acc, vf1, vf5
  c->sqc2(vf10, 4000, v1);                          // sqc2 vf10, 4000(v1)
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf5);        // vmadday.xyzw acc, vf2, vf5
  c->sqc2(vf11, 4032, v1);                          // sqc2 vf11, 4032(v1)
  c->vmadd_bc(DEST::xyzw, BC::z, vf5, vf3, vf5);    // vmaddz.xyzw vf5, vf3, vf5
  c->sqc2(vf12, 4064, v1);                          // sqc2 vf12, 4064(v1)
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf6);        // vmaddax.xyzw acc, vf1, vf6
  c->daddiu(a1, a0, -4);                            // daddiu a1, a0, -4
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf6);        // vmadday.xyzw acc, vf2, vf6
  bc = ((s64)c->sgpr64(a1)) <= 0;                   // blez a1, L172
  c->vmadd_bc(DEST::xyzw, BC::z, vf6, vf3, vf6);    // vmaddz.xyzw vf6, vf3, vf6
  if (bc) {goto block_4;}                           // branch non-likely

  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  c->lqc2(vf9, 144, v1);                            // lqc2 vf9, 144(v1)
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf7);        // vmaddax.xyzw acc, vf1, vf7
  c->lqc2(vf10, 176, v1);                           // lqc2 vf10, 176(v1)
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf7);        // vmadday.xyzw acc, vf2, vf7
  c->lqc2(vf11, 208, v1);                           // lqc2 vf11, 208(v1)
  c->vmadd_bc(DEST::xyzw, BC::z, vf7, vf3, vf7);    // vmaddz.xyzw vf7, vf3, vf7
  c->lqc2(vf12, 240, v1);                           // lqc2 vf12, 240(v1)
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf8);        // vmaddax.xyzw acc, vf1, vf8
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf8);        // vmadday.xyzw acc, vf2, vf8
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf8, vf3, vf8);    // vmaddz.xyzw vf8, vf3, vf8
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf5, vf5);                  // vftoi0.xyzw vf5, vf5
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf6, vf6);                  // vftoi0.xyzw vf6, vf6
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf7, vf7);                  // vftoi0.xyzw vf7, vf7
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf8, vf8);                  // vftoi0.xyzw vf8, vf8
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L171                                // beq r0, r0, L171
  // nop                                            // sll r0, r0, 0
  goto block_1;                                     // branch always


  block_4:
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
  cache.collide_work = intern_from_c("*collide-work*").c();
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("(method 29 collide-cache)", execute, 128);
}

} // namespace method_29_collide_cache
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace method_12_collide_shape_prim_mesh {
struct Cache {
  void* already_printed_exeeded_max_cache_tris; // *already-printed-exeeded-max-cache-tris*
  void* cheat_mode; // *cheat-mode*
  void* stdcon; // *stdcon*
  void* debug; // debug
  void* format; // format
  void* fake_scratchpad_data;
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
  c->mov64(s5, a0);                                 // or s5, a0, r0
  c->mov64(gp, a1);                                 // or gp, a1, r0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lwu(s3, 68, s5);                               // lwu s3, 68(s5)
  c->daddiu(v1, gp, 108);                           // daddiu v1, gp, 108
  c->lwu(a0, 4, gp);                                // lwu a0, 4(gp)
  bc = c->sgpr64(s3) == c->sgpr64(s7);              // beq s3, s7, L147
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_15;}                          // branch non-likely

  c->addiu(a1, r0, 100);                            // addiu a1, r0, 100
  c->dsll(a2, a0, 1);                               // dsll a2, a0, 1
  bc = c->sgpr64(a0) == c->sgpr64(a1);              // beq a0, a1, L145
  c->daddu(a0, a2, a0);                             // daddu a0, a2, a0
  if (bc) {goto block_11;}                          // branch non-likely

  c->dsll(a0, a0, 4);                               // dsll a0, a0, 4
  c->daddu(s4, v1, a0);                             // daddu s4, v1, a0
  c->mov64(a0, s3);                                 // or a0, s3, r0
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 72, v1);                               // lwu t9, 72(v1)
  c->lwu(v1, 0, s5);                                // lwu v1, 0(s5)
  c->lwu(v1, 136, v1);                              // lwu v1, 136(v1)
  c->lwu(v1, 112, v1);                              // lwu v1, 112(v1)
  c->lb(a1, 8, s5);                                 // lb a1, 8(s5)
  c->dsll(a1, a1, 5);                               // dsll a1, a1, 5
  c->daddu(v1, v1, a1);                             // daddu v1, v1, a1
  c->lwu(v1, 28, v1);                               // lwu v1, 28(v1)
  c->daddu(a1, r0, v1);                             // daddu a1, r0, v1
  // c->lui(a2, 28672);                                // lui a2, 28672
  get_fake_spad_addr(a2, cache.fake_scratchpad_data, 0, c);
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lwu(a0, 4, gp);                                // lwu a0, 4(gp)
  c->addiu(a1, r0, *max_tri_count);                 // addiu a1, r0, 460
  c->lwu(v1, 0, gp);                                // lwu v1, 0(gp)
  c->dsll32(a0, a0, 0);                             // dsll32 a0, a0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf1, a0);                        // qmtc2.i vf1, a0
  // nop                                            // sll r0, r0, 0
  c->dsubu(a0, a1, v1);                             // dsubu a0, a1, v1
  c->dsll(t1, v1, 6);                               // dsll t1, v1, 6
  bc = ((s64)c->sgpr64(a0)) < 0;                    // bltz a0, L146
  c->daddiu(a2, s3, 28);                            // daddiu a2, s3, 28
  if (bc) {goto block_12;}                          // branch non-likely

  c->mov64(a0, v1);                                 // or a0, v1, r0
  c->lwu(a3, 4, s3);                                // lwu a3, 4(s3)
  c->daddiu(t2, gp, 4908);                          // daddiu t2, gp, 4908
  c->lq(t0, 60, gp);                                // lq t0, 60(gp)
  c->daddu(t1, t2, t1);                             // daddu t1, t2, t1
  c->lq(t2, 76, gp);                                // lq t2, 76(gp)
  // c->lui(t3, 28672);                                // lui t3, 28672
  get_fake_spad_addr(t3, cache.fake_scratchpad_data, 0, c);
  c->lwu(t4, 8, gp);                                // lwu t4, 8(gp)
  c->vsub(DEST::zw, vf1, vf0, vf0);                 // vsub.zw vf1, vf0, vf0
  // nop                                            // sll r0, r0, 0

  block_4:
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L144
  c->lbu(t5, 0, a2);                                // lbu t5, 0(a2)
  if (bc) {goto block_9;}                           // branch non-likely

  c->daddiu(a3, a3, -1);                            // daddiu a3, a3, -1
  c->lbu(t6, 1, a2);                                // lbu t6, 1(a2)
  c->dsll(t8, t5, 5);                               // dsll t8, t5, 5
  c->lbu(t5, 2, a2);                                // lbu t5, 2(a2)
  c->dsll(t7, t6, 5);                               // dsll t7, t6, 5
  c->daddu(t6, t8, t3);                             // daddu t6, t8, t3
  c->dsll(t5, t5, 5);                               // dsll t5, t5, 5
  // nop                                            // sll r0, r0, 0
  c->daddu(t7, t7, t3);                             // daddu t7, t7, t3
  c->lq(t9, 16, t6);                                // lq t9, 16(t6)
  c->daddu(t5, t5, t3);                             // daddu t5, t5, t3
  c->lq(s3, 16, t7);                                // lq s3, 16(t7)
  c->pminw(ra, t9, s3);                             // pminw ra, t9, s3
  c->lq(t8, 16, t5);                                // lq t8, 16(t5)
  c->pmaxw(t9, t9, s3);                             // pmaxw t9, t9, s3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pminw(ra, ra, t8);                             // pminw ra, ra, t8
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaxw(t8, t9, t8);                             // pmaxw t8, t9, t8
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(t9, ra, t2);                             // pcgtw t9, ra, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(ra, t0, t8);                             // pcgtw ra, t0, t8
  c->lwu(t8, 4, a2);                                // lwu t8, 4(a2)
  c->por(t9, t9, ra);                               // por t9, t9, ra
  c->lq(t6, 0, t6);                                 // lq t6, 0(t6)
  c->ppach(t9, r0, t9);                             // ppach t9, r0, t9
  c->lq(t7, 0, t7);                                 // lq t7, 0(t7)
  c->dsll(t9, t9, 16);                              // dsll t9, t9, 16
  c->lq(t5, 0, t5);                                 // lq t5, 0(t5)
  bc = c->sgpr64(t9) != 0;                          // bne t9, r0, L143
  c->daddiu(a2, a2, 8);                             // daddiu a2, a2, 8
  if (bc) {goto block_4;}                           // branch non-likely

  bc = c->sgpr64(v1) == c->sgpr64(a1);              // beq v1, a1, L146
  c->sqc2(vf1, 48, t1);                             // sqc2 vf1, 48(t1)
  if (bc) {goto block_12;}                          // branch non-likely

  c->and_(t9, t8, t4);                              // and t9, t8, t4
  c->sw(t8, 48, t1);                                // sw t8, 48(t1)
  bc = c->sgpr64(t9) != 0;                          // bne t9, r0, L143
  c->sq(t6, 0, t1);                                 // sq t6, 0(t1)
  if (bc) {goto block_4;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->sq(t7, 16, t1);                                // sq t7, 16(t1)
  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  c->sq(t5, 32, t1);                                // sq t5, 32(t1)
  //beq r0, r0, L143                                // beq r0, r0, L143
  c->daddiu(t1, t1, 64);                            // daddiu t1, t1, 64
  goto block_4;                                     // branch always


  block_9:
  c->dsubu(a3, v1, a0);                             // dsubu a3, v1, a0
  c->lwu(t0, 4, gp);                                // lwu t0, 4(gp)
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L147
  c->lq(a1, 12, s5);                                // lq a1, 12(s5)
  if (bc) {goto block_15;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lq(a2, 28, s5);                                // lq a2, 28(s5)
  // nop                                            // sll r0, r0, 0
  c->sq(r0, 32, s4);                                // sq r0, 32(s4)
  // nop                                            // sll r0, r0, 0
  c->sh(a3, 42, s4);                                // sh a3, 42(s4)
  // nop                                            // sll r0, r0, 0
  c->sw(gp, 32, s4);                                // sw gp, 32(s4)
  // nop                                            // sll r0, r0, 0
  c->sw(s5, 36, s4);                                // sw s5, 36(s4)
  // nop                                            // sll r0, r0, 0
  c->sh(a0, 40, s4);                                // sh a0, 40(s4)
  c->daddiu(a0, t0, 1);                             // daddiu a0, t0, 1
  c->sq(a1, 0, s4);                                 // sq a1, 0(s4)
  // nop                                            // sll r0, r0, 0
  c->sq(a2, 16, s4);                                // sq a2, 16(s4)
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 4, gp);                                 // sw a0, 4(gp)
  //beq r0, r0, L147                                // beq r0, r0, L147
  c->sw(v1, 0, gp);                                 // sw v1, 0(gp)
  goto block_15;                                    // branch always


  block_11:
  c->load_symbol(t9, cache.format);                 // lw t9, format(s7)
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  // daddiu a1, fp, L306                               // daddiu a1, fp, L306
  printf("too many prims\n");
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  //beq r0, r0, L147                                // beq r0, r0, L147
  // nop                                            // sll r0, r0, 0
  goto block_15;                                    // branch always


  block_12:
  c->load_symbol(v1, cache.already_printed_exeeded_max_cache_tris);// lw v1, *already-printed-exeeded-max-cache-tris*(s7)
  bc = c->sgpr64(s7) != c->sgpr64(v1);              // bne s7, v1, L147
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_15;}                          // branch non-likely

  c->daddiu(v1, s7, 8);                             // daddiu v1, s7, #t
  // c->store_symbol(v1, cache.already_printed_exeeded_max_cache_tris);// sw v1, *already-printed-exeeded-max-cache-tris*(s7)
  // c->load_symbol_addr(v1, cache.debug);             // daddiu v1, s7, debug
  c->load_symbol(a0, cache.cheat_mode);             // lw a0, *cheat-mode*(s7)
  bc = c->sgpr64(a0) != c->sgpr64(v1);              // bne a0, v1, L147
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_15;}                          // branch non-likely

  c->load_symbol(t9, cache.format);                 // lw t9, format(s7)
  c->load_symbol(a0, cache.stdcon);                 // lw a0, *stdcon*(s7)
  // daddiu a1, fp, L305                               // daddiu a1, fp, L305
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  printf("too many tris\n");
  c->mov64(v1, v0);                                 // or v1, v0, r0

  block_15:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
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
  gLinkedFunctionTable.reg("(method 12 collide-shape-prim-mesh)", execute, 128);
}

} // namespace method_12_collide_shape_prim_mesh
} // namespace Mips2C


//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace method_14_collide_shape_prim_mesh {
struct Cache {
  void* already_printed_exeeded_max_cache_tris; // *already-printed-exeeded-max-cache-tris*
  void* cheat_mode; // *cheat-mode*
  void* stdcon; // *stdcon*
  void* debug; // debug
  void* format; // format
  void* fake_scratchpad_data;
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
  c->mov64(s5, a0);                                 // or s5, a0, r0
  c->mov64(gp, a1);                                 // or gp, a1, r0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lwu(s3, 68, s5);                               // lwu s3, 68(s5)
  c->daddiu(v1, gp, 108);                           // daddiu v1, gp, 108
  c->lwu(a0, 4, gp);                                // lwu a0, 4(gp)
  bc = c->sgpr64(s3) == c->sgpr64(s7);              // beq s3, s7, L112
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_15;}                          // branch non-likely

  c->addiu(a1, r0, 100);                            // addiu a1, r0, 100
  c->dsll(a2, a0, 1);                               // dsll a2, a0, 1
  bc = c->sgpr64(a0) == c->sgpr64(a1);              // beq a0, a1, L110
  c->daddu(a0, a2, a0);                             // daddu a0, a2, a0
  if (bc) {goto block_11;}                          // branch non-likely

  c->dsll(a0, a0, 4);                               // dsll a0, a0, 4
  c->daddu(s4, v1, a0);                             // daddu s4, v1, a0
  c->mov64(a0, s3);                                 // or a0, s3, r0
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 72, v1);                               // lwu t9, 72(v1)
  c->lwu(v1, 0, s5);                                // lwu v1, 0(s5)
  c->lwu(v1, 136, v1);                              // lwu v1, 136(v1)
  c->lwu(v1, 112, v1);                              // lwu v1, 112(v1)
  c->lb(a1, 8, s5);                                 // lb a1, 8(s5)
  c->dsll(a1, a1, 5);                               // dsll a1, a1, 5
  c->daddu(v1, v1, a1);                             // daddu v1, v1, a1
  c->lwu(v1, 28, v1);                               // lwu v1, 28(v1)
  c->daddu(a1, r0, v1);                             // daddu a1, r0, v1
  // c->lui(a2, 28672);                                // lui a2, 28672
  get_fake_spad_addr(a2, cache.fake_scratchpad_data, 0, c);
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lwu(a0, 4, gp);                                // lwu a0, 4(gp)
  c->addiu(a1, r0, *max_tri_count);                 // addiu a1, r0, 460
  c->lwu(v1, 0, gp);                                // lwu v1, 0(gp)
  c->dsll32(a0, a0, 0);                             // dsll32 a0, a0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf1, a0);                        // qmtc2.i vf1, a0
  // nop                                            // sll r0, r0, 0
  c->dsubu(a0, a1, v1);                             // dsubu a0, a1, v1
  c->dsll(t1, v1, 6);                               // dsll t1, v1, 6
  bc = ((s64)c->sgpr64(a0)) < 0;                    // bltz a0, L111
  c->daddiu(a2, s3, 28);                            // daddiu a2, s3, 28
  if (bc) {goto block_12;}                          // branch non-likely

  c->mov64(a0, v1);                                 // or a0, v1, r0
  c->lwu(a3, 4, s3);                                // lwu a3, 4(s3)
  c->daddiu(t2, gp, 4908);                          // daddiu t2, gp, 4908
  c->lq(t0, 60, gp);                                // lq t0, 60(gp)
  c->daddu(t1, t2, t1);                             // daddu t1, t2, t1
  c->lq(t2, 76, gp);                                // lq t2, 76(gp)
  // c->lui(t3, 28672);                                // lui t3, 28672
  get_fake_spad_addr(t3, cache.fake_scratchpad_data, 0, c);
  c->lwu(t4, 8, gp);                                // lwu t4, 8(gp)
  c->vsub(DEST::zw, vf1, vf0, vf0);                 // vsub.zw vf1, vf0, vf0
  // nop                                            // sll r0, r0, 0

  block_4:
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L109
  c->lbu(t5, 0, a2);                                // lbu t5, 0(a2)
  if (bc) {goto block_9;}                           // branch non-likely

  c->daddiu(a3, a3, -1);                            // daddiu a3, a3, -1
  c->lbu(t6, 1, a2);                                // lbu t6, 1(a2)
  c->dsll(t8, t5, 5);                               // dsll t8, t5, 5
  c->lbu(t5, 2, a2);                                // lbu t5, 2(a2)
  c->dsll(t7, t6, 5);                               // dsll t7, t6, 5
  c->daddu(t6, t8, t3);                             // daddu t6, t8, t3
  c->dsll(t5, t5, 5);                               // dsll t5, t5, 5
  // nop                                            // sll r0, r0, 0
  c->daddu(t7, t7, t3);                             // daddu t7, t7, t3
  c->lq(t9, 16, t6);                                // lq t9, 16(t6)
  c->daddu(t5, t5, t3);                             // daddu t5, t5, t3
  c->lq(s3, 16, t7);                                // lq s3, 16(t7)
  c->pminw(ra, t9, s3);                             // pminw ra, t9, s3
  c->lq(t8, 16, t5);                                // lq t8, 16(t5)
  c->pmaxw(t9, t9, s3);                             // pmaxw t9, t9, s3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pminw(ra, ra, t8);                             // pminw ra, ra, t8
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaxw(t8, t9, t8);                             // pmaxw t8, t9, t8
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(t9, ra, t2);                             // pcgtw t9, ra, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(ra, t0, t8);                             // pcgtw ra, t0, t8
  c->lwu(t8, 4, a2);                                // lwu t8, 4(a2)
  c->por(t9, t9, ra);                               // por t9, t9, ra
  c->lq(t6, 0, t6);                                 // lq t6, 0(t6)
  c->ppach(t9, r0, t9);                             // ppach t9, r0, t9
  c->lq(t7, 0, t7);                                 // lq t7, 0(t7)
  c->dsll(t9, t9, 16);                              // dsll t9, t9, 16
  c->lq(t5, 0, t5);                                 // lq t5, 0(t5)
  bc = c->sgpr64(t9) != 0;                          // bne t9, r0, L108
  c->daddiu(a2, a2, 8);                             // daddiu a2, a2, 8
  if (bc) {goto block_4;}                           // branch non-likely

  bc = c->sgpr64(v1) == c->sgpr64(a1);              // beq v1, a1, L111
  c->sqc2(vf1, 48, t1);                             // sqc2 vf1, 48(t1)
  if (bc) {goto block_12;}                          // branch non-likely

  c->and_(t9, t8, t4);                              // and t9, t8, t4
  c->sw(t8, 48, t1);                                // sw t8, 48(t1)
  bc = c->sgpr64(t9) != 0;                          // bne t9, r0, L108
  c->sq(t6, 0, t1);                                 // sq t6, 0(t1)
  if (bc) {goto block_4;}                           // branch non-likely

  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  c->sq(t7, 16, t1);                                // sq t7, 16(t1)
  // nop                                            // sll r0, r0, 0
  c->sq(t5, 32, t1);                                // sq t5, 32(t1)
  //beq r0, r0, L108                                // beq r0, r0, L108
  c->daddiu(t1, t1, 64);                            // daddiu t1, t1, 64
  goto block_4;                                     // branch always


  block_9:
  c->dsubu(a3, v1, a0);                             // dsubu a3, v1, a0
  c->lwu(t0, 4, gp);                                // lwu t0, 4(gp)
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L112
  c->lq(a1, 12, s5);                                // lq a1, 12(s5)
  if (bc) {goto block_15;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lq(a2, 28, s5);                                // lq a2, 28(s5)
  // nop                                            // sll r0, r0, 0
  c->sq(r0, 32, s4);                                // sq r0, 32(s4)
  // nop                                            // sll r0, r0, 0
  c->sh(a3, 42, s4);                                // sh a3, 42(s4)
  // nop                                            // sll r0, r0, 0
  c->sw(gp, 32, s4);                                // sw gp, 32(s4)
  // nop                                            // sll r0, r0, 0
  c->sw(s5, 36, s4);                                // sw s5, 36(s4)
  // nop                                            // sll r0, r0, 0
  c->sh(a0, 40, s4);                                // sh a0, 40(s4)
  c->daddiu(a0, t0, 1);                             // daddiu a0, t0, 1
  c->sq(a1, 0, s4);                                 // sq a1, 0(s4)
  // nop                                            // sll r0, r0, 0
  c->sq(a2, 16, s4);                                // sq a2, 16(s4)
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 4, gp);                                 // sw a0, 4(gp)
  //beq r0, r0, L112                                // beq r0, r0, L112
  c->sw(v1, 0, gp);                                 // sw v1, 0(gp)
  goto block_15;                                    // branch always


  block_11:
  c->load_symbol(t9, cache.format);                 // lw t9, format(s7)
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  // daddiu a1, fp, L306                               // daddiu a1, fp, L306
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  printf("too many prims\n");
  //beq r0, r0, L112                                // beq r0, r0, L112
  // nop                                            // sll r0, r0, 0
  goto block_15;                                    // branch always


  block_12:
  c->load_symbol(v1, cache.already_printed_exeeded_max_cache_tris);// lw v1, *already-printed-exeeded-max-cache-tris*(s7)
  bc = c->sgpr64(s7) != c->sgpr64(v1);              // bne s7, v1, L112
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_15;}                          // branch non-likely

  c->daddiu(v1, s7, 8);                             // daddiu v1, s7, #t
  // c->store_symbol(v1, cache.already_printed_exeeded_max_cache_tris);// sw v1, *already-printed-exeeded-max-cache-tris*(s7)
  // c->load_symbol_addr(v1, cache.debug);             // daddiu v1, s7, debug
  c->load_symbol(a0, cache.cheat_mode);             // lw a0, *cheat-mode*(s7)
  bc = c->sgpr64(a0) != c->sgpr64(v1);              // bne a0, v1, L112
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_15;}                          // branch non-likely

  c->load_symbol(t9, cache.format);                 // lw t9, format(s7)
  c->load_symbol(a0, cache.stdcon);                 // lw a0, *stdcon*(s7)
  // daddiu a1, fp, L305                               // daddiu a1, fp, L305
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
   //c->jalr(call_addr);                               // jalr ra, t9
  printf("too many tris\n");
  c->mov64(v1, v0);                                 // or v1, v0, r0

  block_15:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
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
  gLinkedFunctionTable.reg("(method 14 collide-shape-prim-mesh)", execute, 256);
}

} // namespace method_14_collide_shape_prim_mesh
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace method_13_collide_shape_prim_mesh {
struct Cache {
  void* already_printed_exeeded_max_cache_tris; // *already-printed-exeeded-max-cache-tris*
  void* cheat_mode; // *cheat-mode*
  void* collide_work; // *collide-work*
  void* stdcon; // *stdcon*
  void* debug; // debug
  void* format; // format
  void* fake_scratchpad_data;
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
  c->mov64(s5, a0);                                 // or s5, a0, r0
  c->mov64(gp, a1);                                 // or gp, a1, r0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lwu(s3, 68, s5);                               // lwu s3, 68(s5)
  c->daddiu(v1, gp, 108);                           // daddiu v1, gp, 108
  c->lwu(a0, 4, gp);                                // lwu a0, 4(gp)
  bc = c->sgpr64(s3) == c->sgpr64(s7);              // beq s3, s7, L77
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_15;}                          // branch non-likely

  c->addiu(a1, r0, 100);                            // addiu a1, r0, 100
  c->dsll(a2, a0, 1);                               // dsll a2, a0, 1
  bc = c->sgpr64(a0) == c->sgpr64(a1);              // beq a0, a1, L75
  c->daddu(a0, a2, a0);                             // daddu a0, a2, a0
  if (bc) {goto block_11;}                          // branch non-likely

  c->dsll(a0, a0, 4);                               // dsll a0, a0, 4
  c->daddu(s4, v1, a0);                             // daddu s4, v1, a0
  c->mov64(a0, s3);                                 // or a0, s3, r0
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 76, v1);                               // lwu t9, 76(v1)
  c->lwu(v1, 0, s5);                                // lwu v1, 0(s5)
  c->lwu(v1, 136, v1);                              // lwu v1, 136(v1)
  c->lwu(v1, 112, v1);                              // lwu v1, 112(v1)
  c->lb(a1, 8, s5);                                 // lb a1, 8(s5)
  c->dsll(a1, a1, 5);                               // dsll a1, a1, 5
  c->daddu(v1, v1, a1);                             // daddu v1, v1, a1
  c->lwu(v1, 28, v1);                               // lwu v1, 28(v1)
  c->daddu(a1, r0, v1);                             // daddu a1, r0, v1
  c->load_symbol(v1, cache.collide_work);           // lw v1, *collide-work*(s7)
  c->daddiu(a2, v1, 48);                            // daddiu a2, v1, 48
  // c->lui(a3, 28672);                                // lui a3, 28672
  get_fake_spad_addr(a3, cache.fake_scratchpad_data, 0, c);
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->load_symbol(t2, cache.collide_work);           // lw t2, *collide-work*(s7)
  // nop                                            // sll r0, r0, 0
  c->lwu(a0, 4, gp);                                // lwu a0, 4(gp)
  c->addiu(a1, r0, *max_tri_count);                 // addiu a1, r0, 460
  c->lwu(v1, 0, gp);                                // lwu v1, 0(gp)
  c->dsll32(a0, a0, 0);                             // dsll32 a0, a0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf1, a0);                        // qmtc2.i vf1, a0
  // nop                                            // sll r0, r0, 0
  c->dsubu(a0, a1, v1);                             // dsubu a0, a1, v1
  c->dsll(t1, v1, 6);                               // dsll t1, v1, 6
  bc = ((s64)c->sgpr64(a0)) < 0;                    // bltz a0, L76
  c->daddiu(a2, s3, 28);                            // daddiu a2, s3, 28
  if (bc) {goto block_12;}                          // branch non-likely

  c->mov64(a0, v1);                                 // or a0, v1, r0
  c->lwu(a3, 4, s3);                                // lwu a3, 4(s3)
  c->daddiu(t3, gp, 4908);                          // daddiu t3, gp, 4908
  c->lq(t0, 16, t2);                                // lq t0, 16(t2)
  c->daddu(t1, t3, t1);                             // daddu t1, t3, t1
  c->lq(t2, 32, t2);                                // lq t2, 32(t2)
  // c->lui(t3, 28672);                                // lui t3, 28672
  get_fake_spad_addr(t3, cache.fake_scratchpad_data, 0, c);
  c->lwu(t4, 8, gp);                                // lwu t4, 8(gp)
  c->vsub(DEST::zw, vf1, vf0, vf0);                 // vsub.zw vf1, vf0, vf0
  // nop                                            // sll r0, r0, 0

  block_4:
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L74
  c->lbu(t5, 0, a2);                                // lbu t5, 0(a2)
  if (bc) {goto block_9;}                           // branch non-likely

  c->daddiu(a3, a3, -1);                            // daddiu a3, a3, -1
  c->lbu(t6, 1, a2);                                // lbu t6, 1(a2)
  c->dsll(t8, t5, 5);                               // dsll t8, t5, 5
  c->lbu(t5, 2, a2);                                // lbu t5, 2(a2)
  c->dsll(t7, t6, 5);                               // dsll t7, t6, 5
  c->daddu(t6, t8, t3);                             // daddu t6, t8, t3
  c->dsll(t5, t5, 5);                               // dsll t5, t5, 5
  // nop                                            // sll r0, r0, 0
  c->daddu(t7, t7, t3);                             // daddu t7, t7, t3
  c->lq(t9, 16, t6);                                // lq t9, 16(t6)
  c->daddu(t5, t5, t3);                             // daddu t5, t5, t3
  c->lq(s3, 16, t7);                                // lq s3, 16(t7)
  // nop                                            // sll r0, r0, 0
  c->lq(t8, 16, t5);                                // lq t8, 16(t5)
  c->pminw(ra, t9, s3);                             // pminw ra, t9, s3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaxw(t9, t9, s3);                             // pmaxw t9, t9, s3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pminw(ra, ra, t8);                             // pminw ra, ra, t8
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaxw(t8, t9, t8);                             // pmaxw t8, t9, t8
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(t9, ra, t2);                             // pcgtw t9, ra, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(ra, t0, t8);                             // pcgtw ra, t0, t8
  c->lwu(t8, 4, a2);                                // lwu t8, 4(a2)
  c->por(t9, t9, ra);                               // por t9, t9, ra
  c->lq(t6, 0, t6);                                 // lq t6, 0(t6)
  c->ppach(t9, r0, t9);                             // ppach t9, r0, t9
  c->lq(t7, 0, t7);                                 // lq t7, 0(t7)
  c->dsll(t9, t9, 16);                              // dsll t9, t9, 16
  c->lq(t5, 0, t5);                                 // lq t5, 0(t5)
  bc = c->sgpr64(t9) != 0;                          // bne t9, r0, L73
  c->daddiu(a2, a2, 8);                             // daddiu a2, a2, 8
  if (bc) {goto block_4;}                           // branch non-likely

  bc = c->sgpr64(v1) == c->sgpr64(a1);              // beq v1, a1, L76
  c->sqc2(vf1, 48, t1);                             // sqc2 vf1, 48(t1)
  if (bc) {goto block_12;}                          // branch non-likely

  c->and_(t9, t8, t4);                              // and t9, t8, t4
  c->sw(t8, 48, t1);                                // sw t8, 48(t1)
  bc = c->sgpr64(t9) != 0;                          // bne t9, r0, L73
  c->sq(t6, 0, t1);                                 // sq t6, 0(t1)
  if (bc) {goto block_4;}                           // branch non-likely

  c->daddiu(v1, v1, 1);                             // daddiu v1, v1, 1
  c->sq(t7, 16, t1);                                // sq t7, 16(t1)
  // nop                                            // sll r0, r0, 0
  c->sq(t5, 32, t1);                                // sq t5, 32(t1)
  //beq r0, r0, L73                                 // beq r0, r0, L73
  c->daddiu(t1, t1, 64);                            // daddiu t1, t1, 64
  goto block_4;                                     // branch always


  block_9:
  c->dsubu(a3, v1, a0);                             // dsubu a3, v1, a0
  c->lwu(t0, 4, gp);                                // lwu t0, 4(gp)
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L77
  c->lq(a1, 12, s5);                                // lq a1, 12(s5)
  if (bc) {goto block_15;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lq(a2, 28, s5);                                // lq a2, 28(s5)
  // nop                                            // sll r0, r0, 0
  c->sq(r0, 32, s4);                                // sq r0, 32(s4)
  // nop                                            // sll r0, r0, 0
  c->sh(a3, 42, s4);                                // sh a3, 42(s4)
  // nop                                            // sll r0, r0, 0
  c->sw(gp, 32, s4);                                // sw gp, 32(s4)
  // nop                                            // sll r0, r0, 0
  c->sw(s5, 36, s4);                                // sw s5, 36(s4)
  // nop                                            // sll r0, r0, 0
  c->sh(a0, 40, s4);                                // sh a0, 40(s4)
  c->daddiu(a0, t0, 1);                             // daddiu a0, t0, 1
  c->sq(a1, 0, s4);                                 // sq a1, 0(s4)
  // nop                                            // sll r0, r0, 0
  c->sq(a2, 16, s4);                                // sq a2, 16(s4)
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 4, gp);                                 // sw a0, 4(gp)
  //beq r0, r0, L77                                 // beq r0, r0, L77
  c->sw(v1, 0, gp);                                 // sw v1, 0(gp)
  goto block_15;                                    // branch always


  block_11:
  c->load_symbol(t9, cache.format);                 // lw t9, format(s7)
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  // daddiu a1, fp, L306                               // daddiu a1, fp, L306
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  printf("too many prims\n");
  //beq r0, r0, L77                                 // beq r0, r0, L77
  // nop                                            // sll r0, r0, 0
  goto block_15;                                    // branch always


  block_12:
  c->load_symbol(v1, cache.already_printed_exeeded_max_cache_tris);// lw v1, *already-printed-exeeded-max-cache-tris*(s7)
  bc = c->sgpr64(s7) != c->sgpr64(v1);              // bne s7, v1, L77
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_15;}                          // branch non-likely

  c->daddiu(v1, s7, 8);                             // daddiu v1, s7, #t
  // c->store_symbol(v1, cache.already_printed_exeeded_max_cache_tris);// sw v1, *already-printed-exeeded-max-cache-tris*(s7)
  // c->load_symbol_addr(v1, cache.debug);             // daddiu v1, s7, debug
  c->load_symbol(a0, cache.cheat_mode);             // lw a0, *cheat-mode*(s7)
  bc = c->sgpr64(a0) != c->sgpr64(v1);              // bne a0, v1, L77
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_15;}                          // branch non-likely

  c->load_symbol(t9, cache.format);                 // lw t9, format(s7)
  c->load_symbol(a0, cache.stdcon);                 // lw a0, *stdcon*(s7)
  // daddiu a1, fp, L305                               // daddiu a1, fp, L305
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  printf("too many tris\n");
  c->mov64(v1, v0);                                 // or v1, v0, r0

  block_15:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
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
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.already_printed_exeeded_max_cache_tris = intern_from_c("*already-printed-exeeded-max-cache-tris*").c();
  cache.cheat_mode = intern_from_c("*cheat-mode*").c();
  cache.collide_work = intern_from_c("*collide-work*").c();
  cache.stdcon = intern_from_c("*stdcon*").c();
  cache.debug = intern_from_c("debug").c();
  cache.format = intern_from_c("format").c();
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("(method 13 collide-shape-prim-mesh)", execute, 128);
}

} // namespace method_13_collide_shape_prim_mesh
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace method_30_collide_cache {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  // u32 call_addr = 0;
  bool cop1_bc = false;
  // nop                                            // sll r0, r0, 0
  c->daddiu(a0, a0, 4908);                          // daddiu a0, a0, 4908
  c->lhu(a3, 40, a2);                               // lhu a3, 40(a2)
  // nop                                            // sll r0, r0, 0
  c->lhu(v1, 42, a2);                               // lhu v1, 42(a2)
  c->dsll(a2, a3, 6);                               // dsll a2, a3, 6
  c->lwc1(f4, 0, a1);                               // lwc1 f4, 0(a1)
  c->daddu(a0, a0, a2);                             // daddu a0, a0, a2
  // nop                                            // sll r0, r0, 0

  block_1:
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L56
  c->lqc2(vf1, 0, a0);                              // lqc2 vf1, 0(a0)
  if (bc) {goto block_9;}                           // branch non-likely

  c->daddiu(v1, v1, -1);                            // daddiu v1, v1, -1
  c->lqc2(vf2, 16, a0);                             // lqc2 vf2, 16(a0)
  c->vsub(DEST::xyzw, vf5, vf2, vf1);               // vsub.xyzw vf5, vf2, vf1
  c->lqc2(vf3, 32, a0);                             // lqc2 vf3, 32(a0)

  c->vsub(DEST::xyzw, vf6, vf2, vf3);               // vsub.xyzw vf6, vf2, vf3
  c->lqc2(vf4, 16, a1);                             // lqc2 vf4, 16(a1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf20, 32, a1);                            // lqc2 vf20, 32(a1)
  c->vsub(DEST::xyzw, vf7, vf2, vf4);               // vsub.xyzw vf7, vf2, vf4
  c->lwu(a2, 48, a0);                               // lwu a2, 48(a0)
  c->vmul(DEST::xyzw, vf8, vf20, vf20);             // vmul.xyzw vf8, vf20, vf20
  c->lwu(a3, 4, a1);                                // lwu a3, 4(a1)
  c->vopmula(vf6, vf5);                             // vopmula.xyz acc, vf6, vf5
  c->daddiu(a0, a0, 64);                            // daddiu a0, a0, 64
  c->vopmsub(vf9, vf5, vf6);                        // vopmsub.xyz vf9, vf5, vf6
  c->and_(a2, a2, a3);                              // and a2, a2, a3
  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L55
  c->vadd_bc(DEST::x, BC::y, vf8, vf8, vf8);        // vaddy.x vf8, vf8, vf8
  if (bc) {goto block_1;}                           // branch non-likely

  c->vmul(DEST::xyzw, vf10, vf9, vf9);              // vmul.xyzw vf10, vf9, vf9
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf7, vf7, vf9);               // vmul.xyzw vf7, vf7, vf9
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf11, vf20, vf9);             // vmul.xyzw vf11, vf20, vf9

  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::z, vf8, vf8, vf8);        // vaddz.x vf8, vf8, vf8
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::y, vf10, vf10, vf10);     // vaddy.x vf10, vf10, vf10
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::y, vf7, vf7, vf7);        // vaddy.x vf7, vf7, vf7
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::y, vf11, vf11, vf11);     // vaddy.x vf11, vf11, vf11
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::z, vf10, vf10, vf10);     // vaddz.x vf10, vf10, vf10
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::z, vf7, vf7, vf7);        // vaddz.x vf7, vf7, vf7
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::z, vf11, vf11, vf11);     // vaddz.x vf11, vf11, vf11
  // nop                                            // sll r0, r0, 0
  c->vrsqrt(vf0, BC::w, vf10, BC::x);               // vrsqrt Q, vf0.w, vf10.x
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a2, vf11);                       // qmfc2.i a2, vf11
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a3, vf7);                        // qmfc2.i a3, vf7
  c->mtc1(f2, a2);                                  // mtc1 f2, a2
  c->mtc1(f1, a3);                                  // mtc1 f1, a3
  c->divs(f3, f1, f2);                              // div.s f3, f1, f2
  c->vmove(DEST::w, vf9, vf0);                      // vmove.w vf9, vf0
  c->sll(a2, a2, 1);                                // sll a2, a2, 1
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L55
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->vwaitq();                                      // vwaitq
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf9, vf9);                    // vmulq.xyz vf9, vf9, Q
  c->mfc1(a2, f3);                                  // mfc1 a2, f3
  bc = ((s64)c->sgpr64(a2)) < 0;                    // bltz a2, L55
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov128_vf_gpr(vf12, a2);                       // qmtc2.i vf12, a2
  // nop                                            // sll r0, r0, 0
  cop1_bc = c->fprs[f4] <= c->fprs[f3];             // c.le.s f4, f3
  // nop                                            // sll r0, r0, 0
  bc = cop1_bc;                                     // bc1t L55
  c->vmula_bc(DEST::xyzw, BC::x, vf20, vf12);       // vmulax.xyzw acc, vf20, vf12
  if (bc) {goto block_1;}                           // branch non-likely

  c->vmadd_bc(DEST::xyzw, BC::w, vf13, vf4, vf0);   // vmaddw.xyzw vf13, vf4, vf0
  // nop                                            // sll r0, r0, 0
  c->vsub(DEST::xyzw, vf14, vf2, vf13);             // vsub.xyzw vf14, vf2, vf13
  // nop                                            // sll r0, r0, 0
  c->vsub(DEST::xyzw, vf15, vf13, vf3);             // vsub.xyzw vf15, vf13, vf3
  // nop                                            // sll r0, r0, 0
  c->vsub(DEST::xyzw, vf16, vf13, vf1);             // vsub.xyzw vf16, vf13, vf1
  // nop                                            // sll r0, r0, 0
  c->vopmula(vf6, vf14);                            // vopmula.xyz acc, vf6, vf14
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf21, vf20, vf9);             // vmul.xyzw vf21, vf20, vf9
  // nop                                            // sll r0, r0, 0
  c->vopmsub(vf17, vf14, vf6);                      // vopmsub.xyz vf17, vf14, vf6
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::y, vf21, vf21, vf21);     // vaddy.x vf21, vf21, vf21
  // nop                                            // sll r0, r0, 0
  c->vopmula(vf14, vf5);                            // vopmula.xyz acc, vf14, vf5
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::z, vf21, vf21, vf21);     // vaddz.x vf21, vf21, vf21
  // nop                                            // sll r0, r0, 0
  c->mtc1(f6, r0);                                  // mtc1 f6, r0
  // nop                                            // sll r0, r0, 0
  c->vopmsub(vf18, vf5, vf14);                      // vopmsub.xyz vf18, vf5, vf14
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a2, vf21);                       // qmfc2.i a2, vf21
  // nop                                            // sll r0, r0, 0
  c->mtc1(f5, a2);                                  // mtc1 f5, a2
  // nop                                            // sll r0, r0, 0
  cop1_bc = c->fprs[f6] <= c->fprs[f5];             // c.le.s f6, f5
  // nop                                            // sll r0, r0, 0
  bc = cop1_bc;                                     // bc1t L55
  c->vopmula(vf15, vf16);                           // vopmula.xyz acc, vf15, vf16
  if (bc) {goto block_1;}                           // branch non-likely

  c->vopmsub(vf19, vf16, vf15);                     // vopmsub.xyz vf19, vf16, vf15
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyz, vf17, vf17, vf9);              // vmul.xyz vf17, vf17, vf9
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyz, vf18, vf18, vf9);              // vmul.xyz vf18, vf18, vf9
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyz, vf19, vf19, vf9);              // vmul.xyz vf19, vf19, vf9
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::y, BC::x, vf17, vf17, vf17);     // vaddx.y vf17, vf17, vf17
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::y, BC::x, vf18, vf18, vf18);     // vaddx.y vf18, vf18, vf18
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::y, BC::x, vf19, vf19, vf19);     // vaddx.y vf19, vf19, vf19
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::y, BC::z, vf17, vf17, vf17);     // vaddz.y vf17, vf17, vf17
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::y, BC::z, vf18, vf18, vf18);     // vaddz.y vf18, vf18, vf18
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::y, BC::z, vf19, vf19, vf19);     // vaddz.y vf19, vf19, vf19
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a3, vf17);                       // qmfc2.i a3, vf17
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t0, vf18);                       // qmfc2.i t0, vf18
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a2, vf19);                       // qmfc2.i a2, vf19
  // nop                                            // sll r0, r0, 0
  c->or_(a3, a3, t0);                               // or a3, a3, t0
  // nop                                            // sll r0, r0, 0
  c->or_(a3, a3, a2);                               // or a3, a3, a2
  c->lwu(a2, 8, a1);                                // lwu a2, 8(a1)
  bc = ((s64)c->sgpr64(a3)) < 0;                    // bltz a3, L55
  c->lwu(a3, -16, a0);                              // lwu a3, -16(a0)
  if (bc) {goto block_1;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->swc1(f3, 0, a1);                               // swc1 f3, 0(a1)
  c->movs(f4, f3);                                  // mov.s f4, f3
  c->sqc2(vf1, 0, a2);                              // sqc2 vf1, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf2, 16, a2);                             // sqc2 vf2, 16(a2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf3, 32, a2);                             // sqc2 vf3, 32(a2)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf9, 64, a2);                             // sqc2 vf9, 64(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(a3, 80, a2);                                // sw a3, 80(a2)
  //beq r0, r0, L55                                 // beq r0, r0, L55
  c->sqc2(vf13, 48, a2);                            // sqc2 vf13, 48(a2)
  goto block_1;                                     // branch always


  block_9:
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
  gLinkedFunctionTable.reg("(method 30 collide-cache)", execute, 128);
}

} // namespace method_30_collide_cache
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace method_9_collide_cache_prim {
struct Cache {
  void* moving_sphere_triangle_intersect; // moving-sphere-triangle-intersect
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -224);                          // daddiu sp, sp, -224
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  c->sq(s1, 128, sp);                               // sq s1, 128(sp)
  c->sq(s2, 144, sp);                               // sq s2, 144(sp)
  c->sq(s3, 160, sp);                               // sq s3, 160(sp)
  c->sq(s4, 176, sp);                               // sq s4, 176(sp)
  c->sq(s5, 192, sp);                               // sq s5, 192(sp)
  c->sq(gp, 208, sp);                               // sq gp, 208(sp)
  c->mov64(gp, a1);                                 // or gp, a1, r0
  c->mov64(s5, a2);                                 // or s5, a2, r0
  c->mov64(s4, a3);                                 // or s4, a3, r0
  c->daddiu(s3, sp, 16);                            // daddiu s3, sp, 16
  // nop                                            // sll r0, r0, 0
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->sw(t1, 8, s3);                                 // sw t1, 8(s3)
  c->mtc1(f1, t0);                                  // mtc1 f1, t0
  c->lhu(a1, 40, a0);                               // lhu a1, 40(a0)
  // nop                                            // sll r0, r0, 0
  c->lwu(v1, 32, a0);                               // lwu v1, 32(a0)
  cop1_bc = c->fprs[f0] <= c->fprs[f1];             // c.le.s f0, f1
  c->dsll(a1, a1, 6);                               // dsll a1, a1, 6
  bc = cop1_bc;                                     // bc1t L37
  c->daddiu(v1, v1, 4908);                          // daddiu v1, v1, 4908
  if (bc) {goto block_2;}                           // branch non-likely

  c->fprs[f1] = 2.0;                                // lwc1 f1, L310(fp)
  // nop                                            // sll r0, r0, 0

  block_2:
  c->daddu(s2, v1, a1);                             // daddu s2, v1, a1
  c->swc1(f1, 0, s3);                               // swc1 f1, 0(s3)
  // nop                                            // sll r0, r0, 0
  c->lhu(s1, 42, a0);                               // lhu s1, 42(a0)
  // nop                                            // sll r0, r0, 0
  c->swc1(f1, 4, s3);                               // swc1 f1, 4(s3)

  block_3:
  bc = c->sgpr64(s1) == 0;                          // beq s1, r0, L40
  c->daddiu(s1, s1, -1);                            // daddiu s1, s1, -1
  if (bc) {goto block_14;}                          // branch non-likely

  c->load_symbol(t9, cache.moving_sphere_triangle_intersect);// lw t9, moving-sphere-triangle-intersect(s7)
  c->mov64(a0, s5);                                 // or a0, s5, r0
  c->mov64(a1, s4);                                 // or a1, s4, r0
  c->lwc1(f0, 12, s5);                              // lwc1 f0, 12(s5)
  c->mfc1(a2, f0);                                  // mfc1 a2, f0
  c->daddu(a3, r0, s2);                             // daddu a3, r0, s2
  c->daddiu(t0, s3, 64);                            // daddiu t0, s3, 64
  c->daddiu(t1, s3, 80);                            // daddiu t1, s3, 80
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  // nop                                            // sll r0, r0, 0
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->lqc2(vf1, 0, s4);                              // lqc2 vf1, 0(s4)
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  c->lwc1(f1, 0, s3);                               // lwc1 f1, 0(s3)
  cop1_bc = c->fprs[f2] < c->fprs[f0];              // c.lt.s f2, f0
  c->lqc2(vf2, 80, s3);                             // lqc2 vf2, 80(s3)
  if (cop1_bc) {                                    // bc1tl L38
    c->daddiu(s2, s2, 64);                          // daddiu s2, s2, 64
    goto block_3;
  }

  // block_6:
  cop1_bc = c->fprs[f1] <= c->fprs[f2];             // c.le.s f1, f2
  c->lqc2(vf3, 64, s3);                             // lqc2 vf3, 64(s3)
  if (cop1_bc) {                                    // bc1tl L38
    c->daddiu(s2, s2, 64);                          // daddiu s2, s2, 64
    goto block_3;
  }

  // block_8:
  c->vmul(DEST::xyzw, vf5, vf1, vf2);               // vmul.xyzw vf5, vf1, vf2
  c->lqc2(vf4, 0, s5);                              // lqc2 vf4, 0(s5)
  // nop                                            // sll r0, r0, 0
  c->lwu(v1, 8, s3);                                // lwu v1, 8(s3)
  c->vsub(DEST::xyzw, vf7, vf4, vf3);               // vsub.xyzw vf7, vf4, vf3
  c->andi(v1, v1, 1);                               // andi v1, v1, 1
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L39
  c->vadd_bc(DEST::x, BC::y, vf5, vf5, vf5);        // vaddy.x vf5, vf5, vf5
  if (bc) {goto block_13;}                          // branch non-likely

  c->vmul(DEST::xyzw, vf6, vf7, vf2);               // vmul.xyzw vf6, vf7, vf2
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::z, vf5, vf5, vf5);        // vaddz.x vf5, vf5, vf5
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::y, vf6, vf6, vf6);        // vaddy.x vf6, vf6, vf6
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(v1, vf5);                        // qmfc2.i v1, vf5
  // nop                                            // sll r0, r0, 0
  c->mtc1(f3, v1);                                  // mtc1 f3, v1
  // nop                                            // sll r0, r0, 0
  cop1_bc = c->fprs[f0] <= c->fprs[f3];             // c.le.s f0, f3
  // nop                                            // sll r0, r0, 0
  if (cop1_bc) {                                    // bc1tl L38
    c->daddiu(s2, s2, 64);                          // daddiu s2, s2, 64
    goto block_3;
  }

  // block_11:
  c->vadd_bc(DEST::x, BC::z, vf6, vf6, vf6);        // vaddz.x vf6, vf6, vf6
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(v1, vf6);                        // qmfc2.i v1, vf6
  // nop                                            // sll r0, r0, 0
  c->mtc1(f4, v1);                                  // mtc1 f4, v1
  // nop                                            // sll r0, r0, 0
  cop1_bc = c->fprs[f4] < c->fprs[f0];              // c.lt.s f4, f0
  // nop                                            // sll r0, r0, 0
  if (cop1_bc) {                                    // bc1tl L38
    c->daddiu(s2, s2, 64);                          // daddiu s2, s2, 64
    goto block_3;
  }

  block_13:
  c->lqc2(vf8, 0, s2);                              // lqc2 vf8, 0(s2)
  c->lqc2(vf9, 16, s2);                             // lqc2 vf9, 16(s2)
  c->lqc2(vf10, 32, s2);                            // lqc2 vf10, 32(s2)
  c->lwu(v1, 48, s2);                               // lwu v1, 48(s2)
  c->swc1(f2, 0, s3);                               // swc1 f2, 0(s3)
  c->sqc2(vf3, 48, gp);                             // sqc2 vf3, 48(gp)
  c->sqc2(vf2, 64, gp);                             // sqc2 vf2, 64(gp)
  c->sqc2(vf8, 0, gp);                              // sqc2 vf8, 0(gp)
  c->sqc2(vf9, 16, gp);                             // sqc2 vf9, 16(gp)
  c->sqc2(vf10, 32, gp);                            // sqc2 vf10, 32(gp)
  c->sw(v1, 80, gp);                                // sw v1, 80(gp)
  //beq r0, r0, L38                                 // beq r0, r0, L38
  c->daddiu(s2, s2, 64);                            // daddiu s2, s2, 64
  goto block_3;                                     // branch always


  block_14:
  c->lwc1(f1, 0, s3);                               // lwc1 f1, 0(s3)
  c->lwc1(f5, 4, s3);                               // lwc1 f5, 4(s3)
  cop1_bc = c->fprs[f1] == c->fprs[f5];             // c.eq.s f1, f5
  if (!cop1_bc) {                                   // bc1fl L41
    c->mfc1(v0, f1);                                // mfc1 v0, f1
    goto block_17;
  }

  // block_16:
  c->lw_float_constant(v0, 0xccbebc20);             // lw v0, L313(fp) -100000000.0

  block_17:
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
  c->lq(gp, 208, sp);                               // lq gp, 208(sp)
  c->lq(s5, 192, sp);                               // lq s5, 192(sp)
  c->lq(s4, 176, sp);                               // lq s4, 176(sp)
  c->lq(s3, 160, sp);                               // lq s3, 160(sp)
  c->lq(s2, 144, sp);                               // lq s2, 144(sp)
  c->lq(s1, 128, sp);                               // lq s1, 128(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 224);                           // daddiu sp, sp, 224
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.moving_sphere_triangle_intersect = intern_from_c("moving-sphere-triangle-intersect").c();
  gLinkedFunctionTable.reg("(method 9 collide-cache-prim)", execute, 512);
}

} // namespace method_9_collide_cache_prim
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace method_10_collide_cache_prim {
struct Cache {
  void* moving_sphere_sphere_intersect; // moving-sphere-sphere-intersect
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -128);                          // daddiu sp, sp, -128
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  c->sq(s1, 32, sp);                                // sq s1, 32(sp)
  c->sq(s2, 48, sp);                                // sq s2, 48(sp)
  c->sq(s3, 64, sp);                                // sq s3, 64(sp)
  c->sq(s4, 80, sp);                                // sq s4, 80(sp)
  c->sq(s5, 96, sp);                                // sq s5, 96(sp)
  c->sq(gp, 112, sp);                               // sq gp, 112(sp)
  c->mov64(s3, a0);                                 // or s3, a0, r0
  c->mov64(gp, a1);                                 // or gp, a1, r0
  c->mov64(s5, a3);                                 // or s5, a3, r0
  c->mov64(s2, t0);                                 // or s2, t0, r0
  c->mov64(s4, t1);                                 // or s4, t1, r0
  c->daddiu(s1, sp, 16);                            // daddiu s1, sp, 16
  c->load_symbol(t9, cache.moving_sphere_sphere_intersect);// lw t9, moving-sphere-sphere-intersect(s7)
  c->mov64(a0, a2);                                 // or a0, a2, r0
  c->mov64(a1, s5);                                 // or a1, s5, r0
  c->daddu(a2, r0, s3);                             // daddu a2, r0, s3
  c->mov64(a3, s1);                                 // or a3, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mtc1(f3, v0);                                  // mtc1 f3, v0
  c->fprs[f4] = -100000000.0;                       // lwc1 f4, L313(fp)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->lqc2(vf4, 0, s1);                              // lqc2 vf4, 0(s1)
  c->movs(f1, f3);                                  // mov.s f1, f3
  c->lqc2(vf5, 0, s3);                              // lqc2 vf5, 0(s3)
  cop1_bc = c->fprs[f1] < c->fprs[f0];              // c.lt.s f1, f0
  c->vmove(DEST::xyzw, vf1, vf0);                   // vmove.xyzw vf1, vf0
  bc = cop1_bc;                                     // bc1t L35
  c->mtc1(f2, s2);                                  // mtc1 f2, s2
  if (bc) {goto block_11;}                          // branch non-likely

  cop1_bc = c->fprs[f2] < c->fprs[f0];              // c.lt.s f2, f0
  c->vsub(DEST::xyz, vf1, vf4, vf5);                // vsub.xyz vf1, vf4, vf5
  bc = cop1_bc;                                     // bc1t L32
  c->lwu(v1, 36, s3);                               // lwu v1, 36(s3)
  if (bc) {goto block_4;}                           // branch non-likely

  cop1_bc = c->fprs[f2] <= c->fprs[f1];             // c.le.s f2, f1
  c->andi(a0, s4, 1);                               // andi a0, s4, 1
  if (cop1_bc) {                                    // bc1tl L35
    c->movs(f3, f4);                                // mov.s f3, f4
    goto block_11;
  }

  block_4:
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L33
  c->lqc2(vf15, 0, s5);                             // lqc2 vf15, 0(s5)
  if (bc) {goto block_7;}                           // branch non-likely

  c->vmul(DEST::xyzw, vf16, vf15, vf1);             // vmul.xyzw vf16, vf15, vf1
  c->vadd_bc(DEST::y, BC::x, vf16, vf16, vf16);     // vaddx.y vf16, vf16, vf16
  c->vadd_bc(DEST::y, BC::z, vf16, vf16, vf16);     // vaddz.y vf16, vf16, vf16
  c->mov128_gpr_vf(a0, vf16);                       // qmfc2.i a0, vf16
  if (((s64)c->sgpr64(a0)) >= 0) {                  // bgezl a0, L35
    c->movs(f3, f4);                                // mov.s f3, f4
    goto block_11;
  }

  block_7:
  //daddiu a0, fp, L299                               // daddiu a0, fp, L299
  //L299:
  //    .word 0x0
  //    .word 0x45800000
  //    .word 0x0
  //    .word 0x3f800000

  //    .word 0x0
  //    .word 0xc5800000
  //    .word 0x45800000
  //    .word 0x3f800000

  //    .word 0x0
  //    .word 0xc5800000
  //    .word 0xc5800000
  //    .word 0x3f800000
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf12, vf1, vf1);              // vmul.xyzw vf12, vf1, vf1
  c->sqc2(vf4, 48, gp);                             // sqc2 vf4, 48(gp)
  c->vmula_bc(DEST::w, BC::x, vf0, vf12);           // vmulax.w acc, vf0, vf12
  //c->lqc2(vf9, 0, a0);                              // lqc2 vf9, 0(a0)
  c->vfs[vf9].du32[0] = 0;
  c->vfs[vf9].du32[1] = 0x45800000;
  c->vfs[vf9].du32[2] = 0;
  c->vfs[vf9].du32[3] = 0x3f800000;

  c->vmadda_bc(DEST::w, BC::y, vf0, vf12);          // vmadday.w acc, vf0, vf12
  // c->lqc2(vf10, 16, a0);                            // lqc2 vf10, 16(a0)
  c->vfs[vf10].du32[0] = 0;
  c->vfs[vf10].du32[1] = 0xc5800000;
  c->vfs[vf10].du32[2] = 0x45800000;
  c->vfs[vf10].du32[3] = 0x3f800000;
  c->vmadd_bc(DEST::w, BC::z, vf12, vf0, vf12);     // vmaddz.w vf12, vf0, vf12
 // c->lqc2(vf11, 32, a0);                            // lqc2 vf11, 32(a0)
  c->vfs[vf11].du32[0] = 0;
  c->vfs[vf11].du32[1] = 0xc5800000;
  c->vfs[vf11].du32[2] = 0xc5800000;
  c->vfs[vf11].du32[3] = 0x3f800000;
  c->vrsqrt(vf0, BC::w, vf12, BC::w);               // vrsqrt Q, vf0.w, vf12.w
  // nop                                            // sll r0, r0, 0
  c->vwaitq();                                      // vwaitq
  c->lwu(v1, 68, v1);                               // lwu v1, 68(v1)
  c->vmulq(DEST::xyz, vf1, vf1);                    // vmulq.xyz vf1, vf1, Q
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf14, vf1, vf1);              // vmul.xyzw vf14, vf1, vf1
  c->sqc2(vf1, 64, gp);                             // sqc2 vf1, 64(gp)
  c->vabs(DEST::xyzw, vf13, vf1);                   // vabs.xyzw vf13, vf1
  c->sw(v1, 80, gp);                                // sw v1, 80(gp)
  c->vmove(DEST::xyzw, vf2, vf0);                   // vmove.xyzw vf2, vf0
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::y, vf14, vf14, vf14);     // vaddy.x vf14, vf14, vf14
  c->mov128_gpr_vf(v1, vf13);                       // qmfc2.i v1, vf13
  if (((s64)c->sgpr64(v1)) == ((s64)0)) {           // beql v1, r0, L34
    c->vadd_bc(DEST::x, BC::z, vf2, vf0, vf1);      // vaddz.x vf2, vf0, vf1
    goto block_10;
  }

  // block_9:
  c->vsub_bc(DEST::x, BC::y, vf2, vf0, vf1);        // vsuby.x vf2, vf0, vf1
  // nop                                            // sll r0, r0, 0
  c->vrsqrt(vf0, BC::w, vf14, BC::x);               // vrsqrt Q, vf0.w, vf14.x
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::y, BC::x, vf2, vf0, vf1);        // vaddx.y vf2, vf0, vf1
  // nop                                            // sll r0, r0, 0
  c->vwaitq();                                      // vwaitq
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xy, vf2, vf2);                     // vmulq.xy vf2, vf2, Q
  // nop                                            // sll r0, r0, 0

  block_10:
  c->vopmula(vf1, vf2);                             // vopmula.xyz acc, vf1, vf2
  // nop                                            // sll r0, r0, 0
  c->vopmsub(vf3, vf2, vf1);                        // vopmsub.xyz vf3, vf2, vf1
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf9);        // vmaddax.xyzw acc, vf1, vf9
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf9);        // vmadday.xyzw acc, vf2, vf9
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyz, BC::z, vf9, vf3, vf9);     // vmaddz.xyz vf9, vf3, vf9
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf10);       // vmaddax.xyzw acc, vf1, vf10
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf10);       // vmadday.xyzw acc, vf2, vf10
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyz, BC::z, vf10, vf3, vf10);   // vmaddz.xyz vf10, vf3, vf10
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf11);       // vmaddax.xyzw acc, vf1, vf11
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf11);       // vmadday.xyzw acc, vf2, vf11
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyz, BC::z, vf11, vf3, vf11);   // vmaddz.xyz vf11, vf3, vf11
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf9, 0, gp);                              // sqc2 vf9, 0(gp)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf10, 16, gp);                            // sqc2 vf10, 16(gp)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf11, 32, gp);                            // sqc2 vf11, 32(gp)
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0

  block_11:
  c->mfc1(v0, f3);                                  // mfc1 v0, f3
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
  c->lq(gp, 112, sp);                               // lq gp, 112(sp)
  c->lq(s5, 96, sp);                                // lq s5, 96(sp)
  c->lq(s4, 80, sp);                                // lq s4, 80(sp)
  c->lq(s3, 64, sp);                                // lq s3, 64(sp)
  c->lq(s2, 48, sp);                                // lq s2, 48(sp)
  c->lq(s1, 32, sp);                                // lq s1, 32(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 128);                           // daddiu sp, sp, 128
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.moving_sphere_sphere_intersect = intern_from_c("moving-sphere-sphere-intersect").c();
  gLinkedFunctionTable.reg("(method 10 collide-cache-prim)", execute, 512);
}

} // namespace method_10_collide_cache_prim
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace method_10_collide_puss_work {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  // u32 call_addr = 0;
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lwu(v1, 4, a2);                                // lwu v1, 4(a2)
  c->daddiu(a2, a0, 96);                            // daddiu a2, a0, 96
  c->lqc2(vf1, 0, a1);                              // lqc2 vf1, 0(a1)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L10
  c->lq(a1, 64, a0);                                // lq a1, 64(a0)
  if (bc) {goto block_12;}                          // branch non-likely

  c->vmax_bc(DEST::xyzw, BC::w, vf4, vf0, vf0);     // vmaxw.xyzw vf4, vf0, vf0
  c->lq(a3, 80, a0);                                // lq a3, 80(a0)
  c->vsub_bc(DEST::xyz, BC::w, vf2, vf1, vf1);      // vsubw.xyz vf2, vf1, vf1
  c->gprs[a0].du64[0] = 0;                          // or a0, r0, r0
  c->vadd_bc(DEST::xyz, BC::w, vf3, vf1, vf1);      // vaddw.xyz vf3, vf1, vf1
  c->lqc2(vf5, 0, a2);                              // lqc2 vf5, 0(a2)
  c->vftoi0(DEST::xyzw, vf2, vf2);                  // vftoi0.xyzw vf2, vf2
  c->lqc2(vf6, 48, a2);                             // lqc2 vf6, 48(a2)
  c->vftoi0(DEST::xyzw, vf3, vf3);                  // vftoi0.xyzw vf3, vf3
  c->lqc2(vf7, 96, a2);                             // lqc2 vf7, 96(a2)
  // nop                                            // sll r0, r0, 0
  c->vsub_bc(DEST::w, BC::w, vf1, vf0, vf1);        // vsubw.w vf1, vf0, vf1
  c->mov128_gpr_vf(t0, vf2);                        // qmfc2.i t0, vf2
  c->lqc2(vf8, 144, a2);                            // lqc2 vf8, 144(a2)
  c->mov128_gpr_vf(t1, vf3);                        // qmfc2.i t1, vf3
  // nop                                            // sll r0, r0, 0
  c->pcgtw(a1, a1, t1);                             // pcgtw a1, a1, t1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(a3, t0, a3);                             // pcgtw a3, t0, a3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->por(a1, a1, a3);                               // por a1, a1, a3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppach(a1, r0, a1);                             // ppach a1, r0, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsll(a1, a1, 16);                              // dsll a1, a1, 16
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a1) != 0;                          // bne a1, r0, L10
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely


  block_2:
  c->vsub(DEST::xyzw, vf9, vf5, vf1);               // vsub.xyzw vf9, vf5, vf1
  c->daddiu(a2, a2, 192);                           // daddiu a2, a2, 192
  c->vsub(DEST::xyzw, vf10, vf6, vf1);              // vsub.xyzw vf10, vf6, vf1
  c->lqc2(vf5, 0, a2);                              // lqc2 vf5, 0(a2)
  c->vsub(DEST::xyzw, vf11, vf7, vf1);              // vsub.xyzw vf11, vf7, vf1
  c->lqc2(vf6, 48, a2);                             // lqc2 vf6, 48(a2)
  c->vsub(DEST::xyzw, vf12, vf8, vf1);              // vsub.xyzw vf12, vf8, vf1
  c->lqc2(vf7, 96, a2);                             // lqc2 vf7, 96(a2)
  c->vmul(DEST::xyzw, vf9, vf9, vf9);               // vmul.xyzw vf9, vf9, vf9
  c->lqc2(vf8, 144, a2);                            // lqc2 vf8, 144(a2)
  c->vmul(DEST::xyzw, vf10, vf10, vf10);            // vmul.xyzw vf10, vf10, vf10
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf11, vf11, vf11);            // vmul.xyzw vf11, vf11, vf11
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf12, vf12, vf12);            // vmul.xyzw vf12, vf12, vf12
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::x, vf4, vf9);         // vmulax.xyzw acc, vf4, vf9
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf4, vf9);        // vmadday.xyzw acc, vf4, vf9
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::z, vf4, vf9);        // vmaddaz.xyzw acc, vf4, vf9
  // nop                                            // sll r0, r0, 0
  c->vmsub_bc(DEST::xyzw, BC::w, vf9, vf4, vf9);    // vmsubw.xyzw vf9, vf4, vf9
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::x, vf4, vf10);        // vmulax.xyzw acc, vf4, vf10
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf4, vf10);       // vmadday.xyzw acc, vf4, vf10
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::z, vf4, vf10);       // vmaddaz.xyzw acc, vf4, vf10
  // nop                                            // sll r0, r0, 0
  c->vmsub_bc(DEST::xyzw, BC::w, vf10, vf4, vf10);  // vmsubw.xyzw vf10, vf4, vf10
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a1, vf9);                        // qmfc2.i a1, vf9
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  bc = ((s64)c->sgpr64(a1)) <= 0;                   // blez a1, L9
  c->vmula_bc(DEST::xyzw, BC::x, vf4, vf11);        // vmulax.xyzw acc, vf4, vf11
  if (bc) {goto block_11;}                          // branch non-likely

  bc = c->sgpr64(a0) == c->sgpr64(v1);              // beq a0, v1, L10
  c->vmadda_bc(DEST::xyzw, BC::y, vf4, vf11);       // vmadday.xyzw acc, vf4, vf11
  if (bc) {goto block_12;}                          // branch non-likely

  c->vmadda_bc(DEST::xyzw, BC::z, vf4, vf11);       // vmaddaz.xyzw acc, vf4, vf11
  // nop                                            // sll r0, r0, 0
  c->vmsub_bc(DEST::xyzw, BC::w, vf11, vf4, vf11);  // vmsubw.xyzw vf11, vf4, vf11
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a1, vf10);                       // qmfc2.i a1, vf10
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  bc = ((s64)c->sgpr64(a1)) <= 0;                   // blez a1, L9
  c->vmula_bc(DEST::xyzw, BC::x, vf4, vf12);        // vmulax.xyzw acc, vf4, vf12
  if (bc) {goto block_11;}                          // branch non-likely

  bc = c->sgpr64(a0) == c->sgpr64(v1);              // beq a0, v1, L10
  c->vmadda_bc(DEST::xyzw, BC::y, vf4, vf12);       // vmadday.xyzw acc, vf4, vf12
  if (bc) {goto block_12;}                          // branch non-likely

  c->vmadda_bc(DEST::xyzw, BC::z, vf4, vf12);       // vmaddaz.xyzw acc, vf4, vf12
  // nop                                            // sll r0, r0, 0
  c->vmsub_bc(DEST::xyzw, BC::w, vf12, vf4, vf12);  // vmsubw.xyzw vf12, vf4, vf12
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a1, vf11);                       // qmfc2.i a1, vf11
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  bc = ((s64)c->sgpr64(a1)) <= 0;                   // blez a1, L9
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_11;}                          // branch non-likely

  bc = c->sgpr64(a0) == c->sgpr64(v1);              // beq a0, v1, L10
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely

  c->mov128_gpr_vf(a1, vf12);                       // qmfc2.i a1, vf12
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  bc = ((s64)c->sgpr64(a1)) <= 0;                   // blez a1, L9
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_11;}                          // branch non-likely

  bc = c->sgpr64(a0) != c->sgpr64(v1);              // bne a0, v1, L8
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  //beq r0, r0, L10                                 // beq r0, r0, L10
  // nop                                            // sll r0, r0, 0
  goto block_12;                                    // branch always


  block_11:
  c->daddiu(v1, s7, 8);                             // daddiu v1, s7, #t
  c->mov64(v0, v1);                                 // or v0, v1, r0
  //beq r0, r0, L11                                 // beq r0, r0, L11
  // nop                                            // sll r0, r0, 0
  goto block_14;                                    // branch always


  block_12:
  c->mov64(v0, s7);                                 // or v0, s7, r0
  //beq r0, r0, L11                                 // beq r0, r0, L11
  // nop                                            // sll r0, r0, 0
  goto block_14;                                    // branch always

  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0

  block_14:
  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  gLinkedFunctionTable.reg("(method 10 collide-puss-work)", execute, 128);
}

} // namespace method_10_collide_puss_work
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace method_9_collide_puss_work {
struct Cache {
  void* closest_pt_in_triangle; // closest-pt-in-triangle
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -128);                          // daddiu sp, sp, -128
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s0, 16, sp);                                // sq s0, 16(sp)
  c->sq(s1, 32, sp);                                // sq s1, 32(sp)
  c->sq(s2, 48, sp);                                // sq s2, 48(sp)
  c->sq(s3, 64, sp);                                // sq s3, 64(sp)
  c->sq(s4, 80, sp);                                // sq s4, 80(sp)
  c->sq(s5, 96, sp);                                // sq s5, 96(sp)
  c->sq(gp, 112, sp);                               // sq gp, 112(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->mov64(s5, a2);                                 // or s5, a2, r0
  // nop                                            // sll r0, r0, 0
  c->lwu(a0, 32, a1);                               // lwu a0, 32(a1)
  // nop                                            // sll r0, r0, 0
  c->lhu(v1, 40, a1);                               // lhu v1, 40(a1)
  c->daddiu(a0, a0, 4908);                          // daddiu a0, a0, 4908
  c->lhu(s4, 42, a1);                               // lhu s4, 42(a1)
  c->dsll(v1, v1, 6);                               // dsll v1, v1, 6
  // nop                                            // sll r0, r0, 0
  c->daddu(s3, a0, v1);                             // daddu s3, a0, v1
  // nop                                            // sll r0, r0, 0

  block_1:
  bc = c->sgpr64(s4) == 0;                          // beq s4, r0, L15
  c->lqc2(vf1, 0, s3);                              // lqc2 vf1, 0(s3)
  if (bc) {goto block_12;}                          // branch non-likely

  c->daddiu(s4, s4, -1);                            // daddiu s4, s4, -1
  c->lqc2(vf2, 16, s3);                             // lqc2 vf2, 16(s3)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 32, s3);                             // lqc2 vf3, 32(s3)
  c->vsub(DEST::xyzw, vf4, vf2, vf1);               // vsub.xyzw vf4, vf2, vf1
  c->lq(a1, 64, gp);                                // lq a1, 64(gp)
  c->vsub(DEST::xyzw, vf5, vf3, vf1);               // vsub.xyzw vf5, vf3, vf1
  c->lq(v1, 80, gp);                                // lq v1, 80(gp)
  c->vmini(DEST::xyzw, vf6, vf1, vf2);              // vmini.xyzw vf6, vf1, vf2
  // nop                                            // sll r0, r0, 0
  c->vmax(DEST::xyzw, vf7, vf1, vf2);               // vmax.xyzw vf7, vf1, vf2
  // nop                                            // sll r0, r0, 0
  c->vopmula(vf4, vf5);                             // vopmula.xyz acc, vf4, vf5
  // nop                                            // sll r0, r0, 0
  c->vmove(DEST::xyzw, vf8, vf0);                   // vmove.xyzw vf8, vf0
  // nop                                            // sll r0, r0, 0
  c->vmini(DEST::xyzw, vf6, vf6, vf3);              // vmini.xyzw vf6, vf6, vf3
  // nop                                            // sll r0, r0, 0
  c->vmax(DEST::xyzw, vf7, vf7, vf3);               // vmax.xyzw vf7, vf7, vf3
  // nop                                            // sll r0, r0, 0
  c->vopmsub(vf8, vf5, vf4);                        // vopmsub.xyz vf8, vf5, vf4
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf6, vf6);                  // vftoi0.xyzw vf6, vf6
  // nop                                            // sll r0, r0, 0
  c->vftoi0(DEST::xyzw, vf7, vf7);                  // vftoi0.xyzw vf7, vf7
  // nop                                            // sll r0, r0, 0
  c->vmul(DEST::xyzw, vf9, vf8, vf8);               // vmul.xyzw vf9, vf8, vf8
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a0, vf6);                        // qmfc2.i a0, vf6
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a2, vf7);                        // qmfc2.i a2, vf7
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::w, BC::x, vf0, vf9);            // vmulax.w acc, vf0, vf9
  c->sqc2(vf6, 32, gp);                             // sqc2 vf6, 32(gp)
  c->vmadda_bc(DEST::w, BC::y, vf0, vf9);           // vmadday.w acc, vf0, vf9
  c->sqc2(vf7, 48, gp);                             // sqc2 vf7, 48(gp)
  c->vmadd_bc(DEST::w, BC::z, vf9, vf0, vf9);       // vmaddz.w vf9, vf0, vf9
  // nop                                            // sll r0, r0, 0
  c->pcgtw(a1, a1, a2);                             // pcgtw a1, a1, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(v1, a0, v1);                             // pcgtw v1, a0, v1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->por(v1, a1, v1);                               // por v1, a1, v1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->vrsqrt(vf0, BC::w, vf9, BC::w);                // vrsqrt Q, vf0.w, vf9.w
  // nop                                            // sll r0, r0, 0
  c->ppach(v1, r0, v1);                             // ppach v1, r0, v1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsll(v1, v1, 16);                              // dsll v1, v1, 16
  // nop                                            // sll r0, r0, 0
  if (((s64)c->sgpr64(v1)) != ((s64)0)) {           // bnel v1, r0, L13
    c->daddiu(s3, s3, 64);                          // daddiu s3, s3, 64
    goto block_1;
  }

  // block_4:
  c->vwaitq();                                      // vwaitq
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf8, vf8);                    // vmulq.xyz vf8, vf8, Q
  c->daddiu(s2, gp, 96);                            // daddiu s2, gp, 96
  // nop                                            // sll r0, r0, 0
  c->lwu(s1, 4, s5);                                // lwu s1, 4(s5)
  c->gprs[s0].du64[0] = 0;                          // or s0, r0, r0
  c->sqc2(vf8, 16, gp);                             // sqc2 vf8, 16(gp)

  block_5:
  if (((s64)c->sgpr64(s0)) == ((s64)c->sgpr64(s1))) {// beql s0, s1, L13
    c->daddiu(s3, s3, 64);                          // daddiu s3, s3, 64
    goto block_1;
  }

  // block_7:
  c->daddiu(s0, s0, 1);                             // daddiu s0, s0, 1
  c->lq(a1, 16, s2);                                // lq a1, 16(s2)
  // nop                                            // sll r0, r0, 0
  c->lq(a2, 48, gp);                                // lq a2, 48(gp)
  // nop                                            // sll r0, r0, 0
  c->lq(v1, 32, s2);                                // lq v1, 32(s2)
  // nop                                            // sll r0, r0, 0
  c->lq(a0, 32, gp);                                // lq a0, 32(gp)
  c->pcgtw(a1, a1, a2);                             // pcgtw a1, a1, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcgtw(v1, a0, v1);                             // pcgtw v1, a0, v1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->por(v1, a1, v1);                               // por v1, a1, v1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppach(v1, r0, v1);                             // ppach v1, r0, v1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsll(v1, v1, 16);                              // dsll v1, v1, 16
  // nop                                            // sll r0, r0, 0
  if (((s64)c->sgpr64(v1)) != ((s64)0)) {           // bnel v1, r0, L14
    c->daddiu(s2, s2, 48);                          // daddiu s2, s2, 48
    goto block_5;
  }

  // block_9:
  c->load_symbol(t9, cache.closest_pt_in_triangle); // lw t9, closest-pt-in-triangle(s7)
  c->daddu(a0, r0, gp);                             // daddu a0, r0, gp
  c->daddu(a1, r0, s2);                             // daddu a1, r0, s2
  c->daddu(a2, r0, s3);                             // daddu a2, r0, s3
  c->daddiu(a3, gp, 16);                            // daddiu a3, gp, 16
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lqc2(vf10, 0, gp);                             // lqc2 vf10, 0(gp)
  c->lqc2(vf11, 0, s2);                             // lqc2 vf11, 0(s2)
  c->daddiu(s2, s2, 48);                            // daddiu s2, s2, 48
  c->vsub(DEST::xyz, vf9, vf10, vf11);              // vsub.xyz vf9, vf10, vf11
  c->vmul(DEST::w, vf11, vf11, vf11);               // vmul.w vf11, vf11, vf11
  c->vmul(DEST::xyzw, vf9, vf9, vf9);               // vmul.xyzw vf9, vf9, vf9
  c->vmula_bc(DEST::w, BC::x, vf0, vf9);            // vmulax.w acc, vf0, vf9
  c->vmadda_bc(DEST::w, BC::y, vf0, vf9);           // vmadday.w acc, vf0, vf9
  c->vmadd_bc(DEST::w, BC::z, vf9, vf0, vf9);       // vmaddz.w vf9, vf0, vf9
  c->vsub(DEST::w, vf9, vf9, vf11);                 // vsub.w vf9, vf9, vf11
  c->mov128_gpr_vf(v1, vf9);                        // qmfc2.i v1, vf9
  c->pcpyud(v1, v1, v1);                            // pcpyud v1, v1, v1
  if (((s64)c->sgpr64(v1)) > 0) {                   // bgtzl v1, L14
    // nop                                          // sll r0, r0, 0
    goto block_5;
  }

  // block_11:
  c->daddiu(v1, s7, 8);                             // daddiu v1, s7, #t
  c->mov64(v0, v1);                                 // or v0, v1, r0
  //beq r0, r0, L16                                 // beq r0, r0, L16
  // nop                                            // sll r0, r0, 0
  goto block_14;                                    // branch always


  block_12:
  c->mov64(v0, s7);                                 // or v0, s7, r0
  //beq r0, r0, L16                                 // beq r0, r0, L16
  // nop                                            // sll r0, r0, 0
  goto block_14;                                    // branch always

  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0

  block_14:
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 112, sp);                               // lq gp, 112(sp)
  c->lq(s5, 96, sp);                                // lq s5, 96(sp)
  c->lq(s4, 80, sp);                                // lq s4, 80(sp)
  c->lq(s3, 64, sp);                                // lq s3, 64(sp)
  c->lq(s2, 48, sp);                                // lq s2, 48(sp)
  c->lq(s1, 32, sp);                                // lq s1, 32(sp)
  c->lq(s0, 16, sp);                                // lq s0, 16(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 128);                           // daddiu sp, sp, 128
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.closest_pt_in_triangle = intern_from_c("closest-pt-in-triangle").c();
  gLinkedFunctionTable.reg("(method 9 collide-puss-work)", execute, 256);
}

} // namespace method_9_collide_puss_work
} // namespace Mips2C
