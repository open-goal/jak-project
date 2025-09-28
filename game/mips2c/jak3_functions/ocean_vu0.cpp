//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace method_14_ocean {
struct Cache {
  void* ocean_wave_frames; // *ocean-wave-frames*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->mtc1(f0, a2);                                  // mtc1 f0, a2
  c->cvtws(f0, f0);                                 // cvt.w.s f0, f0
  c->mfc1(t1, f0);                                  // mfc1 t1, f0
  c->dsll(v1, t1, 10);                              // dsll v1, t1, 10
  c->daddu(v1, r0, v1);                             // daddu v1, r0, v1
  c->load_symbol2(t0, cache.ocean_wave_frames);     // lw t0, *ocean-wave-frames*(s7)
  c->daddu(v1, v1, t0);                             // daddu v1, v1, t0
  c->daddiu(t0, t1, 1);                             // daddiu t0, t1, 1
  c->andi(t0, t0, 63);                              // andi t0, t0, 63
  c->dsll(t0, t0, 10);                              // dsll t0, t0, 10
  c->daddu(t0, r0, t0);                             // daddu t0, r0, t0
  c->load_symbol2(t2, cache.ocean_wave_frames);     // lw t2, *ocean-wave-frames*(s7)
  c->daddu(t0, t0, t2);                             // daddu t0, t0, t2
  c->mtc1(f0, a2);                                  // mtc1 f0, a2
  c->mtc1(f1, t1);                                  // mtc1 f1, t1
  c->cvtsw(f1, f1);                                 // cvt.s.w f1, f1
  c->subs(f0, f0, f1);                              // sub.s f0, f0, f1
  c->swc1(f0, 148, a0);                             // swc1 f0, 148(a0)
  c->lui(a2, 16256);                                // lui a2, 16256
  c->mtc1(f0, a2);                                  // mtc1 f0, a2
  c->lwc1(f1, 148, a0);                             // lwc1 f1, 148(a0)
  c->subs(f0, f0, f1);                              // sub.s f0, f0, f1
  c->swc1(f0, 144, a0);                             // swc1 f0, 144(a0)
  c->lwc1(f0, 144, a0);                             // lwc1 f0, 144(a0)
  c->mtc1(f1, a3);                                  // mtc1 f1, a3
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->swc1(f0, 144, a0);                             // swc1 f0, 144(a0)
  c->lwc1(f0, 148, a0);                             // lwc1 f0, 148(a0)
  c->mtc1(f1, a3);                                  // mtc1 f1, a3
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->swc1(f0, 148, a0);                             // swc1 f0, 148(a0)
  c->addiu(a2, r0, 32);                             // addiu a2, r0, 32
  c->lqc2(vf1, 144, a0);                            // lqc2 vf1, 144(a0)
  // nop                                            // sll r0, r0, 0
  c->lw(a0, 0, v1);                                 // lw a0, 0(v1)
  // nop                                            // sll r0, r0, 0
  c->lw(a3, 0, t0);                                 // lw a3, 0(t0)

block_1:
  c->pextlb(t1, a0, r0);                            // pextlb t1, a0, r0
  c->lw(a0, 4, v1);                                 // lw a0, 4(v1)
  c->pextlb(t2, a3, r0);                            // pextlb t2, a3, r0
  c->lw(a3, 4, t0);                                 // lw a3, 4(t0)
  c->pextlb(t3, a0, r0);                            // pextlb t3, a0, r0
  c->lw(a0, 8, v1);                                 // lw a0, 8(v1)
  c->pextlb(t4, a3, r0);                            // pextlb t4, a3, r0
  c->lw(a3, 8, t0);                                 // lw a3, 8(t0)
  c->pextlh(t5, t1, r0);                            // pextlh t5, t1, r0
  c->lw(t1, 12, v1);                                // lw t1, 12(v1)
  c->pextlh(t6, t2, r0);                            // pextlh t6, t2, r0
  c->lw(t2, 12, t0);                                // lw t2, 12(t0)
  c->pextlh(t3, t3, r0);                            // pextlh t3, t3, r0
  c->mov128_vf_gpr(vf2, t5);                        // qmtc2.i vf2, t5
  c->pextlh(t4, t4, r0);                            // pextlh t4, t4, r0
  c->mov128_vf_gpr(vf10, t6);                       // qmtc2.i vf10, t6
  c->pextlb(a0, a0, r0);                            // pextlb a0, a0, r0
  c->mov128_vf_gpr(vf3, t3);                        // qmtc2.i vf3, t3
  c->pextlb(a3, a3, r0);                            // pextlb a3, a3, r0
  c->mov128_vf_gpr(vf11, t4);                       // qmtc2.i vf11, t4
  c->pextlb(t4, t1, r0);                            // pextlb t4, t1, r0
  c->vitof15(DEST::xyzw, vf2, vf2);                 // vitof15.xyzw vf2, vf2
  c->pextlb(t3, t2, r0);                            // pextlb t3, t2, r0
  c->vitof15(DEST::xyzw, vf10, vf10);               // vitof15.xyzw vf10, vf10
  c->pextlh(a0, a0, r0);                            // pextlh a0, a0, r0
  c->vitof15(DEST::xyzw, vf3, vf3);                 // vitof15.xyzw vf3, vf3
  c->pextlh(t1, a3, r0);                            // pextlh t1, a3, r0
  c->vitof15(DEST::xyzw, vf11, vf11);               // vitof15.xyzw vf11, vf11
  c->pextlh(t2, t4, r0);                            // pextlh t2, t4, r0
  c->lw(t5, 16, v1);                                // lw t5, 16(v1)
  c->pextlh(t3, t3, r0);                            // pextlh t3, t3, r0
  c->lw(t6, 16, t0);                                // lw t6, 16(t0)
  c->vmula_bc(DEST::xyzw, BC::x, vf2, vf1);         // vmulax.xyzw acc, vf2, vf1
  c->lw(t4, 20, v1);                                // lw t4, 20(v1)
  c->vmadd_bc(DEST::xyzw, BC::y, vf2, vf10, vf1);   // vmaddy.xyzw vf2, vf10, vf1
  c->lw(a3, 20, t0);                                // lw a3, 20(t0)
  c->vmula_bc(DEST::xyzw, BC::x, vf3, vf1);         // vmulax.xyzw acc, vf3, vf1
  c->mov128_vf_gpr(vf4, a0);                        // qmtc2.i vf4, a0
  c->vmadd_bc(DEST::xyzw, BC::y, vf3, vf11, vf1);   // vmaddy.xyzw vf3, vf11, vf1
  c->mov128_vf_gpr(vf12, t1);                       // qmtc2.i vf12, t1
  c->pextlb(a0, t5, r0);                            // pextlb a0, t5, r0
  c->mov128_vf_gpr(vf5, t2);                        // qmtc2.i vf5, t2
  c->pextlb(t1, t6, r0);                            // pextlb t1, t6, r0
  c->mov128_vf_gpr(vf13, t3);                       // qmtc2.i vf13, t3
  c->pextlb(t2, t4, r0);                            // pextlb t2, t4, r0
  c->vitof15(DEST::xyzw, vf4, vf4);                 // vitof15.xyzw vf4, vf4
  c->pextlb(a3, a3, r0);                            // pextlb a3, a3, r0
  c->vitof15(DEST::xyzw, vf12, vf12);               // vitof15.xyzw vf12, vf12
  c->pextlh(a0, a0, r0);                            // pextlh a0, a0, r0
  c->vitof15(DEST::xyzw, vf5, vf5);                 // vitof15.xyzw vf5, vf5
  c->pextlh(t1, t1, r0);                            // pextlh t1, t1, r0
  c->vitof15(DEST::xyzw, vf13, vf13);               // vitof15.xyzw vf13, vf13
  c->pextlh(t2, t2, r0);                            // pextlh t2, t2, r0
  c->lw(t5, 24, v1);                                // lw t5, 24(v1)
  c->pextlh(t3, a3, r0);                            // pextlh t3, a3, r0
  c->lw(t6, 24, t0);                                // lw t6, 24(t0)
  c->vmula_bc(DEST::xyzw, BC::x, vf4, vf1);         // vmulax.xyzw acc, vf4, vf1
  c->lw(t4, 28, v1);                                // lw t4, 28(v1)
  c->vmadd_bc(DEST::xyzw, BC::y, vf4, vf12, vf1);   // vmaddy.xyzw vf4, vf12, vf1
  c->lw(a3, 28, t0);                                // lw a3, 28(t0)
  c->vmula_bc(DEST::xyzw, BC::x, vf5, vf1);         // vmulax.xyzw acc, vf5, vf1
  c->mov128_vf_gpr(vf6, a0);                        // qmtc2.i vf6, a0
  c->vmadd_bc(DEST::xyzw, BC::y, vf5, vf13, vf1);   // vmaddy.xyzw vf5, vf13, vf1
  c->mov128_vf_gpr(vf14, t1);                       // qmtc2.i vf14, t1
  c->pextlb(a0, t5, r0);                            // pextlb a0, t5, r0
  c->mov128_vf_gpr(vf7, t2);                        // qmtc2.i vf7, t2
  c->pextlb(t1, t6, r0);                            // pextlb t1, t6, r0
  c->mov128_vf_gpr(vf15, t3);                       // qmtc2.i vf15, t3
  c->pextlb(t2, t4, r0);                            // pextlb t2, t4, r0
  c->vitof15(DEST::xyzw, vf6, vf6);                 // vitof15.xyzw vf6, vf6
  c->pextlb(a3, a3, r0);                            // pextlb a3, a3, r0
  c->vitof15(DEST::xyzw, vf14, vf14);               // vitof15.xyzw vf14, vf14
  c->pextlh(a0, a0, r0);                            // pextlh a0, a0, r0
  c->vitof15(DEST::xyzw, vf7, vf7);                 // vitof15.xyzw vf7, vf7
  c->pextlh(t1, t1, r0);                            // pextlh t1, t1, r0
  c->vitof15(DEST::xyzw, vf15, vf15);               // vitof15.xyzw vf15, vf15
  c->pextlh(t2, t2, r0);                            // pextlh t2, t2, r0
  c->sqc2(vf2, 0, a1);                              // sqc2 vf2, 0(a1)
  c->pextlh(a3, a3, r0);                            // pextlh a3, a3, r0
  c->sqc2(vf3, 16, a1);                             // sqc2 vf3, 16(a1)
  c->vmula_bc(DEST::xyzw, BC::x, vf6, vf1);         // vmulax.xyzw acc, vf6, vf1
  c->sqc2(vf4, 32, a1);                             // sqc2 vf4, 32(a1)
  c->vmadd_bc(DEST::xyzw, BC::y, vf6, vf14, vf1);   // vmaddy.xyzw vf6, vf14, vf1
  c->sqc2(vf5, 48, a1);                             // sqc2 vf5, 48(a1)
  c->vmula_bc(DEST::xyzw, BC::x, vf7, vf1);         // vmulax.xyzw acc, vf7, vf1
  c->mov128_vf_gpr(vf8, a0);                        // qmtc2.i vf8, a0
  c->vmadd_bc(DEST::xyzw, BC::y, vf7, vf15, vf1);   // vmaddy.xyzw vf7, vf15, vf1
  c->mov128_vf_gpr(vf16, t1);                       // qmtc2.i vf16, t1
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf9, t2);                        // qmtc2.i vf9, t2
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf17, a3);                       // qmtc2.i vf17, a3
  // nop                                            // sll r0, r0, 0
  c->vitof15(DEST::xyzw, vf8, vf8);                 // vitof15.xyzw vf8, vf8
  c->daddiu(a1, a1, 128);                           // daddiu a1, a1, 128
  c->vitof15(DEST::xyzw, vf16, vf16);               // vitof15.xyzw vf16, vf16
  c->lw(a0, 32, v1);                                // lw a0, 32(v1)
  c->vitof15(DEST::xyzw, vf9, vf9);                 // vitof15.xyzw vf9, vf9
  c->lw(a3, 32, t0);                                // lw a3, 32(t0)
  c->vitof15(DEST::xyzw, vf17, vf17);               // vitof15.xyzw vf17, vf17
  c->vmula_bc(DEST::xyzw, BC::x, vf8, vf1);         // vmulax.xyzw acc, vf8, vf1
  c->sqc2(vf6, -64, a1);                            // sqc2 vf6, -64(a1)
  c->vmadd_bc(DEST::xyzw, BC::y, vf8, vf16, vf1);   // vmaddy.xyzw vf8, vf16, vf1
  c->sqc2(vf7, -48, a1);                            // sqc2 vf7, -48(a1)
  c->vmula_bc(DEST::xyzw, BC::x, vf9, vf1);         // vmulax.xyzw acc, vf9, vf1
  c->daddiu(a2, a2, -1);                            // daddiu a2, a2, -1
  c->vmadd_bc(DEST::xyzw, BC::y, vf9, vf17, vf1);   // vmaddy.xyzw vf9, vf17, vf1
  c->daddiu(v1, v1, 32);                            // daddiu v1, v1, 32
  // nop                                            // sll r0, r0, 0
  c->daddiu(t0, t0, 32);                            // daddiu t0, t0, 32
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, -32, a1);                            // sqc2 vf8, -32(a1)
  bc = ((s64)c->sgpr64(a2)) > 0;                    // bgtz a2, L16
  c->sqc2(vf9, -16, a1);                            // sqc2 vf9, -16(a1)
  if (bc) {goto block_1;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.ocean_wave_frames = intern_from_c(-1, 0, "*ocean-wave-frames*").c();
  gLinkedFunctionTable.reg("(method 14 ocean)", execute, 256);
}

} // namespace method_14_ocean
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace method_15_ocean {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  // nop                                            // sll r0, r0, 0
  c->mov64(v1, a1);                                 // or v1, a1, r0
  c->mov64(a0, a2);                                 // or a0, a2, r0
  c->addiu(a1, r0, 64);                             // addiu a1, r0, 64
  // nop                                            // sll r0, r0, 0

block_1:
  c->lqc2(vf1, 0, v1);                              // lqc2 vf1, 0(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 16, v1);                             // lqc2 vf2, 16(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 32, v1);                             // lqc2 vf3, 32(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf4, 48, v1);                             // lqc2 vf4, 48(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf5, 0, a0);                              // lqc2 vf5, 0(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf6, 16, a0);                             // lqc2 vf6, 16(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf7, 32, a0);                             // lqc2 vf7, 32(a0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf8, 48, a0);                             // lqc2 vf8, 48(a0)
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyzw, vf1, vf1, vf5);               // vadd.xyzw vf1, vf1, vf5
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyzw, vf2, vf2, vf6);               // vadd.xyzw vf2, vf2, vf6
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyzw, vf3, vf3, vf7);               // vadd.xyzw vf3, vf3, vf7
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyzw, vf4, vf4, vf8);               // vadd.xyzw vf4, vf4, vf8
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf1, 0, v1);                              // sqc2 vf1, 0(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf2, 16, v1);                             // sqc2 vf2, 16(v1)
  c->daddiu(a1, a1, -1);                            // daddiu a1, a1, -1
  c->sqc2(vf3, 32, v1);                             // sqc2 vf3, 32(v1)
  c->daddiu(v1, v1, 64);                            // daddiu v1, v1, 64
  bc = ((s64)c->sgpr64(a1)) > 0;                    // bgtz a1, L14
  c->sqc2(vf4, -16, v1);                            // sqc2 vf4, -16(v1)
  if (bc) {goto block_1;}                           // branch non-likely

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
  gLinkedFunctionTable.reg("(method 15 ocean)", execute, 128);
}

} // namespace method_15_ocean
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace method_16_ocean {
struct Cache {
  void* level; // *level*
  void* time_of_day_context; // *time-of-day-context*
  void* ocean_vu0_block; // ocean-vu0-block
  void* sewerb; // sewerb
  void* upload_vu0_program; // upload-vu0-program
  void* vu_lights_light_group; // vu-lights<-light-group!
  void* ocean_generate_verts_vector;
} cache;

void vcallms0(ExecutionContext* c) {
  // nop                        |  mulay.x ACC, vf12, vf02        0
  c->acc.vf.mula(Mask::x, c->vf_src(vf12).vf, c->vf_src(vf02).vf.y());
  // nop                        |  mulax.z ACC, vf12, vf03        1
  c->acc.vf.mula(Mask::z, c->vf_src(vf12).vf, c->vf_src(vf03).vf.x());
  // nop                        |  msubx.xz vf24, vf12, vf02      2
  // ASSERT(false);
  c->acc.vf.msub(Mask::xz, c->vfs[vf24].vf, c->vf_src(vf12).vf, c->vf_src(vf02).vf.x());
  // nop                        |  mulaz.x ACC, vf12, vf02        3
  c->acc.vf.mula(Mask::x, c->vf_src(vf12).vf, c->vf_src(vf02).vf.z());
  // nop                        |  mulay.z ACC, vf12, vf03        4
  c->acc.vf.mula(Mask::z, c->vf_src(vf12).vf, c->vf_src(vf03).vf.y());
  // nop                        |  msuby.xz vf25, vf12, vf02      5
  // ASSERT(false);
  c->acc.vf.msub(Mask::xz, c->vfs[vf25].vf, c->vf_src(vf12).vf, c->vf_src(vf02).vf.y());
  // nop                        |  mulaw.x ACC, vf12, vf02        6
  c->acc.vf.mula(Mask::x, c->vf_src(vf12).vf, c->vf_src(vf02).vf.w());
  // nop                        |  mulaz.z ACC, vf12, vf03        7
  c->acc.vf.mula(Mask::z, c->vf_src(vf12).vf, c->vf_src(vf03).vf.z());
  // nop                        |  msubz.xz vf26, vf12, vf02      8
  // ASSERT(false);
  c->acc.vf.msub(Mask::xz, c->vfs[vf26].vf, c->vf_src(vf12).vf, c->vf_src(vf02).vf.z());
  // nop                        |  mulax.x ACC, vf12, vf04        9
  c->acc.vf.mula(Mask::x, c->vf_src(vf12).vf, c->vf_src(vf04).vf.x());
  // nop                        |  mulaw.z ACC, vf12, vf03        10
  c->acc.vf.mula(Mask::z, c->vf_src(vf12).vf, c->vf_src(vf03).vf.w());
  // nop                        |  msubw.xz vf27, vf12, vf02      11
  // ASSERT(false);
  c->acc.vf.msub(Mask::xz, c->vfs[vf27].vf, c->vf_src(vf12).vf, c->vf_src(vf02).vf.w());
  // nop                        |  mul.xz vf28, vf24, vf24        12
  c->vfs[vf28].vf.mul(Mask::xz, c->vf_src(vf24).vf, c->vf_src(vf24).vf);
  // nop                        |  mul.xz vf29, vf25, vf25        13
  c->vfs[vf29].vf.mul(Mask::xz, c->vf_src(vf25).vf, c->vf_src(vf25).vf);
  // nop                        |  mul.xz vf30, vf26, vf26        14
  c->vfs[vf30].vf.mul(Mask::xz, c->vf_src(vf26).vf, c->vf_src(vf26).vf);
  // nop                        |  mul.xz vf31, vf27, vf27        15
  c->vfs[vf31].vf.mul(Mask::xz, c->vf_src(vf27).vf, c->vf_src(vf27).vf);
  // nop                        |  subx.y vf24, vf01, vf28        16
  c->vfs[vf24].vf.sub(Mask::y, c->vf_src(vf01).vf, c->vf_src(vf28).vf.x());
  // nop                        |  subx.y vf25, vf01, vf29        17
  c->vfs[vf25].vf.sub(Mask::y, c->vf_src(vf01).vf, c->vf_src(vf29).vf.x());
  // nop                        |  subx.y vf26, vf01, vf30        18
  c->vfs[vf26].vf.sub(Mask::y, c->vf_src(vf01).vf, c->vf_src(vf30).vf.x());
  // nop                        |  subx.y vf27, vf01, vf31        19
  c->vfs[vf27].vf.sub(Mask::y, c->vf_src(vf01).vf, c->vf_src(vf31).vf.x());
  // nop                        |  subz.y vf24, vf24, vf28        20
  c->vfs[vf24].vf.sub(Mask::y, c->vf_src(vf24).vf, c->vf_src(vf28).vf.z());
  // nop                        |  subz.y vf25, vf25, vf29        21
  c->vfs[vf25].vf.sub(Mask::y, c->vf_src(vf25).vf, c->vf_src(vf29).vf.z());
  // nop                        |  subz.y vf26, vf26, vf30        22
  c->vfs[vf26].vf.sub(Mask::y, c->vf_src(vf26).vf, c->vf_src(vf30).vf.z());
  // nop                        |  subz.y vf27, vf27, vf31        23
  c->vfs[vf27].vf.sub(Mask::y, c->vf_src(vf27).vf, c->vf_src(vf31).vf.z());
  // nop                        |  mulx.w vf24, vf01, vf02        24
  c->vfs[vf24].vf.mul(Mask::w, c->vf_src(vf01).vf, c->vf_src(vf02).vf.x());
  // nop                        |  muly.w vf25, vf01, vf02        25
  c->vfs[vf25].vf.mul(Mask::w, c->vf_src(vf01).vf, c->vf_src(vf02).vf.y());
  // nop                        |  mulz.w vf26, vf01, vf02        26
  c->vfs[vf26].vf.mul(Mask::w, c->vf_src(vf01).vf, c->vf_src(vf02).vf.z());
  // nop                        |  mulw.w vf27, vf01, vf02        27
  c->vfs[vf27].vf.mul(Mask::w, c->vf_src(vf01).vf, c->vf_src(vf02).vf.w());
  // nop                        |  mulax.xyzw ACC, vf05, vf24     28
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf05).vf, c->vf_src(vf24).vf.x());
  // nop                        |  madday.xyzw ACC, vf06, vf24    29
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf06].vf, c->vfs[vf24].vf.y());
  // nop                        |  maddz.xyz vf16, vf07, vf24     30
  c->acc.vf.madd(Mask::xyz, c->vfs[vf16].vf, c->vf_src(vf07).vf, c->vf_src(vf24).vf.z());
  // nop                        |  mulax.xyzw ACC, vf05, vf25     31
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf05).vf, c->vf_src(vf25).vf.x());
  // nop                        |  madday.xyzw ACC, vf06, vf25    32
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf06].vf, c->vfs[vf25].vf.y());
  // nop                        |  maddz.xyz vf17, vf07, vf25     33
  c->acc.vf.madd(Mask::xyz, c->vfs[vf17].vf, c->vf_src(vf07).vf, c->vf_src(vf25).vf.z());
  // nop                        |  mulax.xyzw ACC, vf05, vf26     34
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf05).vf, c->vf_src(vf26).vf.x());
  // nop                        |  madday.xyzw ACC, vf06, vf26    35
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf06].vf, c->vfs[vf26].vf.y());
  // nop                        |  maddz.xyz vf18, vf07, vf26     36
  c->acc.vf.madd(Mask::xyz, c->vfs[vf18].vf, c->vf_src(vf07).vf, c->vf_src(vf26).vf.z());
  // nop                        |  mulax.xyzw ACC, vf05, vf27     37
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf05).vf, c->vf_src(vf27).vf.x());
  // nop                        |  madday.xyzw ACC, vf06, vf27    38
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf06].vf, c->vfs[vf27].vf.y());
  // nop                        |  maddz.xyz vf19, vf07, vf27     39
  c->acc.vf.madd(Mask::xyz, c->vfs[vf19].vf, c->vf_src(vf07).vf, c->vf_src(vf27).vf.z());
  // nop                        |  maxx.xyz vf16, vf16, vf00      40
  c->vfs[vf16].vf.max(Mask::xyz, c->vf_src(vf16).vf, c->vf_src(vf00).vf.x());
  // nop                        |  maxx.xyz vf17, vf17, vf00      41
  c->vfs[vf17].vf.max(Mask::xyz, c->vf_src(vf17).vf, c->vf_src(vf00).vf.x());
  // nop                        |  maxx.xyz vf18, vf18, vf00      42
  c->vfs[vf18].vf.max(Mask::xyz, c->vf_src(vf18).vf, c->vf_src(vf00).vf.x());
  // nop                        |  maxx.xyz vf19, vf19, vf00      43
  c->vfs[vf19].vf.max(Mask::xyz, c->vf_src(vf19).vf, c->vf_src(vf00).vf.x());
  // nop                        |  mula.xyzw ACC, vf01, vf11      44
  // vu.acc.mula(Mask::xyzw, vu.vf01, vu.vf11);
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf01).vf, c->vf_src(vf11).vf);
  // nop                        |  maddax.xyzw ACC, vf08, vf16     45
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf08].vf, c->vfs[vf16].vf.x());
  // nop                        |  madday.xyzw ACC, vf09, vf16     46
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf09].vf, c->vfs[vf16].vf.y());
  // nop                        |  maddz.xyzw vf20, vf10, vf16     47
  c->acc.vf.madd(Mask::xyzw, c->vfs[vf20].vf, c->vf_src(vf10).vf, c->vf_src(vf16).vf.z());
  // nop                        |  mula.xyzw ACC, vf01, vf11       48
  // vu.acc.mula(Mask::xyzw, vu.vf01, vu.vf11);
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf01).vf, c->vf_src(vf11).vf);
  // nop                        |  maddax.xyzw ACC, vf08, vf17     49
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf08].vf, c->vfs[vf17].vf.x());
  // nop                        |  madday.xyzw ACC, vf09, vf17     50
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf09].vf, c->vfs[vf17].vf.y());
  // nop                        |  maddz.xyzw vf21, vf10, vf17     51
  c->acc.vf.madd(Mask::xyzw, c->vfs[vf21].vf, c->vf_src(vf10).vf, c->vf_src(vf17).vf.z());
  // nop                        |  mula.xyzw ACC, vf01, vf11       52
  // vu.acc.mula(Mask::xyzw, vu.vf01, vu.vf11);
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf01).vf, c->vf_src(vf11).vf);
  // nop                        |  maddax.xyzw ACC, vf08, vf18     53
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf08].vf, c->vfs[vf18].vf.x());
  // nop                        |  madday.xyzw ACC, vf09, vf18     54
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf09].vf, c->vfs[vf18].vf.y());
  // nop                        |  maddz.xyzw vf22, vf10, vf18     55
  c->acc.vf.madd(Mask::xyzw, c->vfs[vf22].vf, c->vf_src(vf10).vf, c->vf_src(vf18).vf.z());
  // nop                        |  mula.xyzw ACC, vf01, vf11       56
  // vu.acc.mula(Mask::xyzw, vu.vf01, vu.vf11);
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf01).vf, c->vf_src(vf11).vf);
  // nop                        |  maddax.xyzw ACC, vf08, vf19     57
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf08].vf, c->vfs[vf19].vf.x());
  // nop                        |  madday.xyzw ACC, vf09, vf19     58
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf09].vf, c->vfs[vf19].vf.y());
  // nop                        |  maddz.xyzw vf23, vf10, vf19     59
  c->acc.vf.madd(Mask::xyzw, c->vfs[vf23].vf, c->vf_src(vf10).vf, c->vf_src(vf19).vf.z());
  // nop                        |  miniy.xyzw vf20, vf20, vf12     60
  c->vfs[vf20].vf.mini(Mask::xyzw, c->vf_src(vf20).vf, c->vf_src(vf12).vf.y());
  // nop                        |  miniy.xyzw vf21, vf21, vf12     61
  c->vfs[vf21].vf.mini(Mask::xyzw, c->vf_src(vf21).vf, c->vf_src(vf12).vf.y());
  // nop                        |  miniy.xyzw vf22, vf22, vf12 :e  62
  c->vfs[vf22].vf.mini(Mask::xyzw, c->vf_src(vf22).vf, c->vf_src(vf12).vf.y());
  // nop                        |  miniy.xyzw vf23, vf23, vf12     63
  c->vfs[vf23].vf.mini(Mask::xyzw, c->vf_src(vf23).vf, c->vf_src(vf12).vf.y());
}

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -80);                           // daddiu sp, sp, -80
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  c->sq(s4, 32, sp);                                // sq s4, 32(sp)
  c->sq(s5, 48, sp);                                // sq s5, 48(sp)
  c->sq(gp, 64, sp);                                // sq gp, 64(sp)
  c->mov64(s4, a0);                                 // or s4, a0, r0
  c->mov64(s5, a1);                                 // or s5, a1, r0
  c->mov64(gp, a2);                                 // or gp, a2, r0
  c->load_symbol2(t9, cache.upload_vu0_program);    // lw t9, upload-vu0-program(s7)
  c->load_symbol2(a0, cache.ocean_vu0_block);       // lw a0, ocean-vu0-block(s7)
  c->daddiu(a1, s4, 8296);                          // daddiu a1, s4, 8296
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->load_symbol2(a0, cache.level);                 // lw a0, *level*(s7)
  c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
  c->lwu(t9, 52, v1);                               // lwu t9, 52(v1)
  c->load_symbol_addr(a1, cache.sewerb);            // daddiu a1, s7, sewerb
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L2
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  c->load_symbol2(t9, cache.vu_lights_light_group);// lw t9, vu-lights<-light-group!(s7)
  c->daddiu(a0, s4, 8144);                          // daddiu a0, s4, 8144
  c->daddiu(a1, v1, 796);                           // daddiu a1, v1, 796
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  //beq r0, r0, L3                                  // beq r0, r0, L3
  // nop                                            // sll r0, r0, 0
  goto block_3;                                     // branch always


block_2:
  c->load_symbol2(t9, cache.vu_lights_light_group);// lw t9, vu-lights<-light-group!(s7)
  c->daddiu(a0, s4, 8144);                          // daddiu a0, s4, 8144
  c->load_symbol2(v1, cache.time_of_day_context);   // lw v1, *time-of-day-context*(s7)
  c->daddiu(a1, v1, 156);                           // daddiu a1, v1, 156
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0

block_3:
  c->daddiu(v1, sp, 16);                            // daddiu v1, sp, 16
  c->mov64(a2, v1);                                 // or a2, v1, r0
  c->daddiu(a0, s4, 8192);                          // daddiu a0, s4, 8192
  c->daddiu(a1, s4, 8208);                          // daddiu a1, s4, 8208
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->lqc2(vf4, 0, a0);                              // lqc2 vf4, 0(a0)
  c->lqc2(vf5, 0, a1);                              // lqc2 vf5, 0(a1)
  c->vadd(DEST::xyz, vf6, vf4, vf5);                // vadd.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, a2);                              // sqc2 vf6, 0(a2)
  c->mov64(a2, v1);                                 // or a2, v1, r0
  c->mov64(a0, v1);                                 // or a0, v1, r0
  c->daddiu(a1, s4, 8224);                          // daddiu a1, s4, 8224
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->lqc2(vf4, 0, a0);                              // lqc2 vf4, 0(a0)
  c->lqc2(vf5, 0, a1);                              // lqc2 vf5, 0(a1)
  c->vadd(DEST::xyz, vf6, vf4, vf5);                // vadd.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, a2);                              // sqc2 vf6, 0(a2)
  c->mov64(a2, v1);                                 // or a2, v1, r0
  c->mov64(a0, v1);                                 // or a0, v1, r0
  c->daddiu(a1, s4, 8240);                          // daddiu a1, s4, 8240
  c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
  c->lqc2(vf4, 0, a0);                              // lqc2 vf4, 0(a0)
  c->lqc2(vf5, 0, a1);                              // lqc2 vf5, 0(a1)
  c->vadd(DEST::xyz, vf6, vf4, vf5);                // vadd.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, a2);                              // sqc2 vf6, 0(a2)
  c->lui(a0, 16288);                                // lui a0, 16288
  c->mtc1(f0, a0);                                  // mtc1 f0, a0
  c->lwc1(f1, 0, v1);                               // lwc1 f1, 0(v1)
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = cop1_bc;                                     // bc1t L4
  c->daddiu(a0, s7, 4);                             // daddiu a0, s7, 4
  if (bc) {goto block_5;}                           // branch non-likely

  c->mov64(a0, s7);                                 // or a0, s7, r0

block_5:
  if (((s64)c->sgpr64(s7)) != ((s64)c->sgpr64(a0))) {// bnel s7, a0, L6
    c->mov64(a0, a0);                               // or a0, a0, r0
    goto block_13;
  }


  c->lui(a0, 16288);                                // lui a0, 16288
  c->mtc1(f0, a0);                                  // mtc1 f0, a0
  c->lwc1(f1, 4, v1);                               // lwc1 f1, 4(v1)
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = cop1_bc;                                     // bc1t L5
  c->daddiu(a0, s7, 4);                             // daddiu a0, s7, 4
  if (bc) {goto block_9;}                           // branch non-likely

  c->mov64(a0, s7);                                 // or a0, s7, r0

block_9:
  if (((s64)c->sgpr64(s7)) != ((s64)c->sgpr64(a0))) {// bnel s7, a0, L6
    c->mov64(a0, a0);                               // or a0, a0, r0
    goto block_13;
  }

  c->lui(a0, 16288);                                // lui a0, 16288
  c->mtc1(f0, a0);                                  // mtc1 f0, a0
  c->lwc1(f1, 8, v1);                               // lwc1 f1, 8(v1)
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = cop1_bc;                                     // bc1t L6
  c->daddiu(a0, s7, 4);                             // daddiu a0, s7, 4
  if (bc) {goto block_13;}                          // branch non-likely

  c->mov64(a0, s7);                                 // or a0, s7, r0

block_13:
  bc = c->sgpr64(s7) == c->sgpr64(a0);              // beq s7, a0, L10
  c->mov64(a0, s7);                                 // or a0, s7, r0
  if (bc) {goto block_20;}                          // branch non-likely

  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->lwc1(f0, 4, v1);                               // lwc1 f0, 4(v1)
  c->lwc1(f1, 0, v1);                               // lwc1 f1, 0(v1)
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = !cop1_bc;                                    // bc1f L7
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_16;}                          // branch non-likely

  c->lwc1(f0, 0, v1);                               // lwc1 f0, 0(v1)
  c->lwc1(f1, 8240, s4);                            // lwc1 f1, 8240(s4)
  c->mfc1(a0, f1);                                  // mfc1 a0, f1
  //beq r0, r0, L8                                  // beq r0, r0, L8
  // nop                                            // sll r0, r0, 0
  goto block_17;                                    // branch always


block_16:
  c->lwc1(f0, 4, v1);                               // lwc1 f0, 4(v1)
  c->lwc1(f1, 8244, s4);                            // lwc1 f1, 8244(s4)
  c->mfc1(a0, f1);                                  // mfc1 a0, f1

block_17:
  c->lwc1(f2, 8, v1);                               // lwc1 f2, 8(v1)
  cop1_bc = c->fprs[f0] < c->fprs[f2];              // c.lt.s f0, f2
  bc = !cop1_bc;                                    // bc1f L9
  c->mov64(a0, s7);                                 // or a0, s7, r0
  if (bc) {goto block_19;}                          // branch non-likely

  c->lwc1(f0, 8, v1);                               // lwc1 f0, 8(v1)
  c->lwc1(f1, 8248, s4);                            // lwc1 f1, 8248(s4)
  c->mfc1(v1, f1);                                  // mfc1 v1, f1

block_19:
  c->lui(v1, 16288);                                // lui v1, 16288
  c->mtc1(f2, v1);                                  // mtc1 f2, v1
  c->subs(f2, f2, f1);                              // sub.s f2, f2, f1
  c->subs(f0, f0, f1);                              // sub.s f0, f0, f1
  c->divs(f0, f2, f0);                              // div.s f0, f2, f0
  c->daddiu(v1, s4, 8192);                          // daddiu v1, s4, 8192
  c->daddiu(a0, s4, 8192);                          // daddiu a0, s4, 8192
  c->movs(f1, f0);                                  // mov.s f1, f0
  c->lqc2(vf1, 0, a0);                              // lqc2 vf1, 0(a0)
  c->mfc1(a0, f1);                                  // mfc1 a0, f1
  c->mov128_vf_gpr(vf2, a0);                        // qmtc2.i vf2, a0
  c->vadd_bc(DEST::w, BC::x, vf1, vf0, vf0);        // vaddx.w vf1, vf0, vf0
  c->vmul_bc(DEST::xyz, BC::x, vf1, vf1, vf2);      // vmulx.xyz vf1, vf1, vf2
  c->sqc2(vf1, 0, v1);                              // sqc2 vf1, 0(v1)
  c->daddiu(v1, s4, 8208);                          // daddiu v1, s4, 8208
  c->daddiu(a0, s4, 8208);                          // daddiu a0, s4, 8208
  c->movs(f1, f0);                                  // mov.s f1, f0
  c->lqc2(vf1, 0, a0);                              // lqc2 vf1, 0(a0)
  c->mfc1(a0, f1);                                  // mfc1 a0, f1
  c->mov128_vf_gpr(vf2, a0);                        // qmtc2.i vf2, a0
  c->vadd_bc(DEST::w, BC::x, vf1, vf0, vf0);        // vaddx.w vf1, vf0, vf0
  c->vmul_bc(DEST::xyz, BC::x, vf1, vf1, vf2);      // vmulx.xyz vf1, vf1, vf2
  c->sqc2(vf1, 0, v1);                              // sqc2 vf1, 0(v1)
  c->daddiu(a0, s4, 8224);                          // daddiu a0, s4, 8224
  c->daddiu(v1, s4, 8224);                          // daddiu v1, s4, 8224
  c->lqc2(vf1, 0, v1);                              // lqc2 vf1, 0(v1)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mov128_vf_gpr(vf2, v1);                        // qmtc2.i vf2, v1
  c->vadd_bc(DEST::w, BC::x, vf1, vf0, vf0);        // vaddx.w vf1, vf0, vf0
  c->vmul_bc(DEST::xyz, BC::x, vf1, vf1, vf2);      // vmulx.xyz vf1, vf1, vf2
  c->sqc2(vf1, 0, a0);                              // sqc2 vf1, 0(a0)

block_20:
  // daddiu v1, fp, L18                             // daddiu v1, fp, L18
  c->load_symbol2(v1, cache.ocean_generate_verts_vector); // HACK
  c->daddiu(a2, s4, 8192);                          // daddiu a2, s4, 8192
  c->daddiu(a0, s4, 8192);                          // daddiu a0, s4, 8192
  c->mov64(a1, v1);                                 // or a1, v1, r0
  c->lqc2(vf4, 0, a0);                              // lqc2 vf4, 0(a0)
  c->lqc2(vf5, 0, a1);                              // lqc2 vf5, 0(a1)
  c->vadd_bc(DEST::w, BC::x, vf6, vf0, vf0);        // vaddx.w vf6, vf0, vf0
  c->vmul(DEST::xyz, vf6, vf4, vf5);                // vmul.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, a2);                              // sqc2 vf6, 0(a2)
  c->daddiu(a2, s4, 8208);                          // daddiu a2, s4, 8208
  c->daddiu(a0, s4, 8208);                          // daddiu a0, s4, 8208
  c->mov64(a1, v1);                                 // or a1, v1, r0
  c->lqc2(vf4, 0, a0);                              // lqc2 vf4, 0(a0)
  c->lqc2(vf5, 0, a1);                              // lqc2 vf5, 0(a1)
  c->vadd_bc(DEST::w, BC::x, vf6, vf0, vf0);        // vaddx.w vf6, vf0, vf0
  c->vmul(DEST::xyz, vf6, vf4, vf5);                // vmul.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, a2);                              // sqc2 vf6, 0(a2)
  c->daddiu(a2, s4, 8224);                          // daddiu a2, s4, 8224
  c->daddiu(a0, s4, 8224);                          // daddiu a0, s4, 8224
  c->mov64(a1, v1);                                 // or a1, v1, r0
  c->lqc2(vf4, 0, a0);                              // lqc2 vf4, 0(a0)
  c->lqc2(vf5, 0, a1);                              // lqc2 vf5, 0(a1)
  c->vadd_bc(DEST::w, BC::x, vf6, vf0, vf0);        // vaddx.w vf6, vf0, vf0
  c->vmul(DEST::xyz, vf6, vf4, vf5);                // vmul.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, a2);                              // sqc2 vf6, 0(a2)
  c->daddiu(a1, s4, 8240);                          // daddiu a1, s4, 8240
  c->daddiu(a0, s4, 8240);                          // daddiu a0, s4, 8240
  c->lqc2(vf4, 0, a0);                              // lqc2 vf4, 0(a0)
  c->lqc2(vf5, 0, v1);                              // lqc2 vf5, 0(v1)
  c->vadd_bc(DEST::w, BC::x, vf6, vf0, vf0);        // vaddx.w vf6, vf0, vf0
  c->vmul(DEST::xyz, vf6, vf4, vf5);                // vmul.xyz vf6, vf4, vf5
  c->sqc2(vf6, 0, a1);                              // sqc2 vf6, 0(a1)
  c->lui(v1, 17152);                                // lui v1, 17152
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->swc1(f0, 8204, s4);                            // swc1 f0, 8204(s4)
  c->lui(v1, 17152);                                // lui v1, 17152
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->swc1(f0, 8220, s4);                            // swc1 f0, 8220(s4)
  c->lui(v1, 17152);                                // lui v1, 17152
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->swc1(f0, 8236, s4);                            // swc1 f0, 8236(s4)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 8252, s4);                            // swc1 f0, 8252(s4)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->vmax_bc(DEST::xyzw, BC::w, vf1, vf0, vf0);     // vmaxw.xyzw vf1, vf0, vf0
  c->lqc2(vf12, 8096, s4);                          // lqc2 vf12, 8096(s4)
  c->lqc2(vf5, 8144, s4);                           // lqc2 vf5, 8144(s4)
  c->lqc2(vf6, 8160, s4);                           // lqc2 vf6, 8160(s4)
  c->lqc2(vf7, 8176, s4);                           // lqc2 vf7, 8176(s4)
  c->lqc2(vf8, 8192, s4);                           // lqc2 vf8, 8192(s4)
  c->lqc2(vf9, 8208, s4);                           // lqc2 vf9, 8208(s4)
  c->lqc2(vf10, 8224, s4);                          // lqc2 vf10, 8224(s4)
  c->lqc2(vf11, 8240, s4);                          // lqc2 vf11, 8240(s4)
  c->addiu(v1, r0, 31);                             // addiu v1, r0, 31
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->daddiu(a1, gp, 128);                           // daddiu a1, gp, 128
  // nop                                            // sll r0, r0, 0

block_21:
  c->lqc2(vf2, 0, gp);                              // lqc2 vf2, 0(gp)
  c->addiu(t2, r0, 6);                              // addiu t2, r0, 6
  c->lqc2(vf3, 16, gp);                             // lqc2 vf3, 16(gp)
  c->daddiu(t5, gp, 16);                            // daddiu t5, gp, 16
  c->lqc2(vf4, 0, a1);                              // lqc2 vf4, 0(a1)
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  // Unknown instr: vcallms 0
  vcallms0(c);
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t1, vf20);                       // qmfc2.i t1, vf20
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t0, vf21);                       // qmfc2.i t0, vf21
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a3, vf22);                       // qmfc2.i a3, vf22
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a2, vf23);                       // qmfc2.i a2, vf23
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t6, vf24);                       // qmfc2.i t6, vf24
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t7, vf25);                       // qmfc2.i t7, vf25
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t3, vf26);                       // qmfc2.i t3, vf26
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t4, vf27);                       // qmfc2.i t4, vf27
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 0, t5);                              // lqc2 vf2, 0(t5)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 16, t5);                             // lqc2 vf3, 16(t5)
  c->daddiu(gp, t5, 16);                            // daddiu gp, t5, 16
  c->lqc2(vf4, 0, a1);                              // lqc2 vf4, 0(a1)
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16

block_22:
  // Unknown instr: vcallms 0
  vcallms0(c);
  // nop                                            // sll r0, r0, 0
  c->pextlw(t5, t7, t6);                            // pextlw t5, t7, t6
  // nop                                            // sll r0, r0, 0
  c->pextuw(t6, t7, t6);                            // pextuw t6, t7, t6
  // nop                                            // sll r0, r0, 0
  c->pextlw(t7, t4, t3);                            // pextlw t7, t4, t3
  // nop                                            // sll r0, r0, 0
  c->pextuw(t4, t4, t3);                            // pextuw t4, t4, t3
  // nop                                            // sll r0, r0, 0
  c->pcpyld(t3, t7, t5);                            // pcpyld t3, t7, t5
  c->sq(t1, 0, s5);                                 // sq t1, 0(s5)
  c->pcpyud(t1, t5, t7);                            // pcpyud t1, t5, t7
  c->sq(t0, 32, s5);                                // sq t0, 32(s5)
  c->pcpyld(t0, t4, t6);                            // pcpyld t0, t4, t6
  c->sq(a3, 64, s5);                                // sq a3, 64(s5)
  c->pcpyud(a3, t6, t4);                            // pcpyud a3, t6, t4
  c->sq(a2, 96, s5);                                // sq a2, 96(s5)
  c->sq(t3, 16, s5);                                // sq t3, 16(s5)
  // nop                                            // sll r0, r0, 0
  c->sq(t1, 48, s5);                                // sq t1, 48(s5)
  // nop                                            // sll r0, r0, 0
  c->sq(t0, 80, s5);                                // sq t0, 80(s5)
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 112, s5);                               // sq a3, 112(s5)
  c->daddiu(s5, s5, 128);                           // daddiu s5, s5, 128
  c->mov128_gpr_vf(t1, vf20);                       // qmfc2.i t1, vf20
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t0, vf21);                       // qmfc2.i t0, vf21
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a3, vf22);                       // qmfc2.i a3, vf22
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a2, vf23);                       // qmfc2.i a2, vf23
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t6, vf24);                       // qmfc2.i t6, vf24
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t7, vf25);                       // qmfc2.i t7, vf25
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t3, vf26);                       // qmfc2.i t3, vf26
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t4, vf27);                       // qmfc2.i t4, vf27
  c->daddiu(t2, t2, -1);                            // daddiu t2, t2, -1
  c->lqc2(vf2, 0, gp);                              // lqc2 vf2, 0(gp)
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->lqc2(vf3, 16, gp);                             // lqc2 vf3, 16(gp)
  c->daddiu(gp, gp, 16);                            // daddiu gp, gp, 16
  bc = ((s64)c->sgpr64(t2)) > 0;                    // bgtz t2, L12
  c->lqc2(vf4, -16, a1);                            // lqc2 vf4, -16(a1)
  if (bc) {goto block_22;}                          // branch non-likely

  c->lqc2(vf3, -128, gp);                           // lqc2 vf3, -128(gp)
  // nop                                            // sll r0, r0, 0
  // Unknown instr: vcallms 0
  vcallms0(c);
  // nop                                            // sll r0, r0, 0
  c->pextlw(t2, t7, t6);                            // pextlw t2, t7, t6
  // nop                                            // sll r0, r0, 0
  c->pextuw(t5, t7, t6);                            // pextuw t5, t7, t6
  // nop                                            // sll r0, r0, 0
  c->pextlw(t6, t4, t3);                            // pextlw t6, t4, t3
  // nop                                            // sll r0, r0, 0
  c->pextuw(t4, t4, t3);                            // pextuw t4, t4, t3
  // nop                                            // sll r0, r0, 0
  c->pcpyld(t3, t6, t2);                            // pcpyld t3, t6, t2
  c->sq(t1, 0, s5);                                 // sq t1, 0(s5)
  c->pcpyud(t1, t2, t6);                            // pcpyud t1, t2, t6
  c->sq(t0, 32, s5);                                // sq t0, 32(s5)
  c->pcpyld(t0, t4, t5);                            // pcpyld t0, t4, t5
  c->sq(a3, 64, s5);                                // sq a3, 64(s5)
  c->pcpyud(a3, t5, t4);                            // pcpyud a3, t5, t4
  c->sq(a2, 96, s5);                                // sq a2, 96(s5)
  c->sq(t3, 16, s5);                                // sq t3, 16(s5)
  // nop                                            // sll r0, r0, 0
  c->sq(t1, 48, s5);                                // sq t1, 48(s5)
  // nop                                            // sll r0, r0, 0
  c->sq(t0, 80, s5);                                // sq t0, 80(s5)
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 112, s5);                               // sq a3, 112(s5)
  c->daddiu(a2, s5, 128);                           // daddiu a2, s5, 128
  c->mov128_gpr_vf(t2, vf20);                       // qmfc2.i t2, vf20
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t1, vf21);                       // qmfc2.i t1, vf21
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t0, vf22);                       // qmfc2.i t0, vf22
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a3, vf23);                       // qmfc2.i a3, vf23
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t4, vf24);                       // qmfc2.i t4, vf24
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t7, vf25);                       // qmfc2.i t7, vf25
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t5, vf26);                       // qmfc2.i t5, vf26
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t6, vf27);                       // qmfc2.i t6, vf27
  c->daddiu(v1, v1, -1);                            // daddiu v1, v1, -1
  c->pextlw(t3, t7, t4);                            // pextlw t3, t7, t4
  // nop                                            // sll r0, r0, 0
  c->pextuw(t4, t7, t4);                            // pextuw t4, t7, t4
  // nop                                            // sll r0, r0, 0
  c->pextlw(t7, t6, t5);                            // pextlw t7, t6, t5
  // nop                                            // sll r0, r0, 0
  c->pextuw(t6, t6, t5);                            // pextuw t6, t6, t5
  // nop                                            // sll r0, r0, 0
  c->pcpyld(t5, t7, t3);                            // pcpyld t5, t7, t3
  c->sq(t2, 0, a2);                                 // sq t2, 0(a2)
  c->pcpyud(t2, t3, t7);                            // pcpyud t2, t3, t7
  c->sq(t1, 32, a2);                                // sq t1, 32(a2)
  c->pcpyld(t1, t6, t4);                            // pcpyld t1, t6, t4
  c->sq(t0, 64, a2);                                // sq t0, 64(a2)
  c->pcpyud(t0, t4, t6);                            // pcpyud t0, t4, t6
  c->sq(a3, 96, a2);                                // sq a3, 96(a2)
  c->sq(t5, 16, a2);                                // sq t5, 16(a2)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 48, a2);                                // sq t2, 48(a2)
  // nop                                            // sll r0, r0, 0
  c->sq(t1, 80, a2);                                // sq t1, 80(a2)
  // nop                                            // sll r0, r0, 0
  c->sq(t0, 112, a2);                               // sq t0, 112(a2)
  c->daddiu(s5, a2, 128);                           // daddiu s5, a2, 128
  bc = ((s64)c->sgpr64(v1)) > 0;                    // bgtz v1, L11
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_21;}                          // branch non-likely

  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L11
  c->mov64(a1, a0);                                 // or a1, a0, r0
  if (bc) {goto block_21;}                          // branch non-likely

  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
  c->lq(gp, 64, sp);                                // lq gp, 64(sp)
  c->lq(s5, 48, sp);                                // lq s5, 48(sp)
  c->lq(s4, 32, sp);                                // lq s4, 32(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 80);                            // daddiu sp, sp, 80
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.level = intern_from_c(-1, 0, "*level*").c();
  cache.time_of_day_context = intern_from_c(-1, 0, "*time-of-day-context*").c();
  cache.ocean_vu0_block = intern_from_c(-1, 0, "ocean-vu0-block").c();
  cache.sewerb = intern_from_c(-1, 0, "sewerb").c();
  cache.upload_vu0_program = intern_from_c(-1, 0, "upload-vu0-program").c();
  cache.vu_lights_light_group = intern_from_c(-1, 0, "vu-lights<-light-group!").c();
  cache.ocean_generate_verts_vector = intern_from_c(-1, 0, "*ocean-generate-verts-vector*").c();
  gLinkedFunctionTable.reg("(method 16 ocean)", execute, 128);
}

} // namespace method_16_ocean
} // namespace Mips2C