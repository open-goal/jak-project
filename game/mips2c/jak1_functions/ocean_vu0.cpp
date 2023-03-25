// clang-format off
//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak1/kscheme.h"
using namespace jak1;
namespace Mips2C::jak1 {
namespace ocean_interp_wave {
struct Cache {
  void* ocean_wave_frames; // *ocean-wave-frames*
  void* ocean_work; // *ocean-work*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  c->dsra(a2, a1, 5);                               // dsra a2, a1, 5
  c->andi(v1, a2, 63);                              // andi v1, a2, 63
  c->dsll(v1, v1, 10);                              // dsll v1, v1, 10
  c->daddu(v1, r0, v1);                             // daddu v1, r0, v1
  c->load_symbol(a3, cache.ocean_wave_frames);      // lw a3, *ocean-wave-frames*(s7)
  c->daddu(v1, v1, a3);                             // daddu v1, v1, a3
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  c->andi(a2, a2, 63);                              // andi a2, a2, 63
  c->dsll(a2, a2, 10);                              // dsll a2, a2, 10
  c->daddu(a2, r0, a2);                             // daddu a2, r0, a2
  c->load_symbol(a3, cache.ocean_wave_frames);      // lw a3, *ocean-wave-frames*(s7)
  c->daddu(a2, a2, a3);                             // daddu a2, a2, a3
  c->fprs[f0] = 0.03125;                            // lwc1 f0, L12(fp)
  c->andi(a1, a1, 31);                              // andi a1, a1, 31
  c->mtc1(f1, a1);                                  // mtc1 f1, a1
  c->cvtsw(f1, f1);                                 // cvt.s.w f1, f1
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->load_symbol(a1, cache.ocean_work);             // lw a1, *ocean-work*(s7)
  c->swc1(f0, 64, a1);                              // swc1 f0, 64(a1)
  c->fprs[f0] = 1.0;                                // lwc1 f0, L11(fp)
  c->load_symbol(a1, cache.ocean_work);             // lw a1, *ocean-work*(s7)
  c->lwc1(f1, 64, a1);                              // lwc1 f1, 64(a1)
  c->subs(f0, f0, f1);                              // sub.s f0, f0, f1
  c->load_symbol(a1, cache.ocean_work);             // lw a1, *ocean-work*(s7)
  c->swc1(f0, 60, a1);                              // swc1 f0, 60(a1)
  c->fprs[f0] = 0.333;                              // lwc1 f0, L10(fp)
  c->load_symbol(a1, cache.ocean_work);             // lw a1, *ocean-work*(s7)
  c->lwc1(f1, 60, a1);                              // lwc1 f1, 60(a1)
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->load_symbol(a1, cache.ocean_work);             // lw a1, *ocean-work*(s7)
  c->swc1(f0, 60, a1);                              // swc1 f0, 60(a1)
  c->fprs[f0] = 0.333;                              // lwc1 f0, L10(fp)
  c->load_symbol(a1, cache.ocean_work);             // lw a1, *ocean-work*(s7)
  c->lwc1(f1, 64, a1);                              // lwc1 f1, 64(a1)
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->load_symbol(a1, cache.ocean_work);             // lw a1, *ocean-work*(s7)
  c->swc1(f0, 64, a1);                              // swc1 f0, 64(a1)
  c->load_symbol(a3, cache.ocean_work);             // lw a3, *ocean-work*(s7)
  c->addiu(a1, r0, 32);                             // addiu a1, r0, 32
  c->lqc2(vf1, 60, a3);                             // lqc2 vf1, 60(a3)
  // nop                                            // sll r0, r0, 0
  c->lw(a3, 0, v1);                                 // lw a3, 0(v1)
  // nop                                            // sll r0, r0, 0
  c->lw(t0, 0, a2);                                 // lw t0, 0(a2)

  block_1:
  c->pextlb(t1, a3, r0);                            // pextlb t1, a3, r0
  c->lw(a3, 4, v1);                                 // lw a3, 4(v1)
  c->pextlb(t2, t0, r0);                            // pextlb t2, t0, r0
  c->lw(t0, 4, a2);                                 // lw t0, 4(a2)
  c->pextlb(t3, a3, r0);                            // pextlb t3, a3, r0
  c->lw(a3, 8, v1);                                 // lw a3, 8(v1)
  c->pextlb(t4, t0, r0);                            // pextlb t4, t0, r0
  c->lw(t0, 8, a2);                                 // lw t0, 8(a2)
  c->pextlh(t5, t1, r0);                            // pextlh t5, t1, r0
  c->lw(t1, 12, v1);                                // lw t1, 12(v1)
  c->pextlh(t6, t2, r0);                            // pextlh t6, t2, r0
  c->lw(t2, 12, a2);                                // lw t2, 12(a2)
  c->pextlh(t3, t3, r0);                            // pextlh t3, t3, r0
  c->mov128_vf_gpr(vf2, t5);                        // qmtc2.i vf2, t5
  c->pextlh(t4, t4, r0);                            // pextlh t4, t4, r0
  c->mov128_vf_gpr(vf10, t6);                       // qmtc2.i vf10, t6
  c->pextlb(a3, a3, r0);                            // pextlb a3, a3, r0
  c->mov128_vf_gpr(vf3, t3);                        // qmtc2.i vf3, t3
  c->pextlb(t0, t0, r0);                            // pextlb t0, t0, r0
  c->mov128_vf_gpr(vf11, t4);                       // qmtc2.i vf11, t4
  c->pextlb(t4, t1, r0);                            // pextlb t4, t1, r0
  c->vitof15(DEST::xyzw, vf2, vf2);                 // vitof15.xyzw vf2, vf2
  c->pextlb(t3, t2, r0);                            // pextlb t3, t2, r0
  c->vitof15(DEST::xyzw, vf10, vf10);               // vitof15.xyzw vf10, vf10
  c->pextlh(a3, a3, r0);                            // pextlh a3, a3, r0
  c->vitof15(DEST::xyzw, vf3, vf3);                 // vitof15.xyzw vf3, vf3
  c->pextlh(t1, t0, r0);                            // pextlh t1, t0, r0
  c->vitof15(DEST::xyzw, vf11, vf11);               // vitof15.xyzw vf11, vf11
  c->pextlh(t2, t4, r0);                            // pextlh t2, t4, r0
  c->lw(t5, 16, v1);                                // lw t5, 16(v1)
  c->pextlh(t3, t3, r0);                            // pextlh t3, t3, r0
  c->lw(t6, 16, a2);                                // lw t6, 16(a2)
  c->vmula_bc(DEST::xyzw, BC::x, vf2, vf1);         // vmulax.xyzw acc, vf2, vf1
  c->lw(t4, 20, v1);                                // lw t4, 20(v1)
  c->vmadd_bc(DEST::xyzw, BC::y, vf2, vf10, vf1);   // vmaddy.xyzw vf2, vf10, vf1
  c->lw(t0, 20, a2);                                // lw t0, 20(a2)
  c->vmula_bc(DEST::xyzw, BC::x, vf3, vf1);         // vmulax.xyzw acc, vf3, vf1
  c->mov128_vf_gpr(vf4, a3);                        // qmtc2.i vf4, a3
  c->vmadd_bc(DEST::xyzw, BC::y, vf3, vf11, vf1);   // vmaddy.xyzw vf3, vf11, vf1
  c->mov128_vf_gpr(vf12, t1);                       // qmtc2.i vf12, t1
  c->pextlb(a3, t5, r0);                            // pextlb a3, t5, r0
  c->mov128_vf_gpr(vf5, t2);                        // qmtc2.i vf5, t2
  c->pextlb(t1, t6, r0);                            // pextlb t1, t6, r0
  c->mov128_vf_gpr(vf13, t3);                       // qmtc2.i vf13, t3
  c->pextlb(t2, t4, r0);                            // pextlb t2, t4, r0
  c->vitof15(DEST::xyzw, vf4, vf4);                 // vitof15.xyzw vf4, vf4
  c->pextlb(t0, t0, r0);                            // pextlb t0, t0, r0
  c->vitof15(DEST::xyzw, vf12, vf12);               // vitof15.xyzw vf12, vf12
  c->pextlh(a3, a3, r0);                            // pextlh a3, a3, r0
  c->vitof15(DEST::xyzw, vf5, vf5);                 // vitof15.xyzw vf5, vf5
  c->pextlh(t1, t1, r0);                            // pextlh t1, t1, r0
  c->vitof15(DEST::xyzw, vf13, vf13);               // vitof15.xyzw vf13, vf13
  c->pextlh(t2, t2, r0);                            // pextlh t2, t2, r0
  c->lw(t5, 24, v1);                                // lw t5, 24(v1)
  c->pextlh(t3, t0, r0);                            // pextlh t3, t0, r0
  c->lw(t6, 24, a2);                                // lw t6, 24(a2)
  c->vmula_bc(DEST::xyzw, BC::x, vf4, vf1);         // vmulax.xyzw acc, vf4, vf1
  c->lw(t4, 28, v1);                                // lw t4, 28(v1)
  c->vmadd_bc(DEST::xyzw, BC::y, vf4, vf12, vf1);   // vmaddy.xyzw vf4, vf12, vf1
  c->lw(t0, 28, a2);                                // lw t0, 28(a2)
  c->vmula_bc(DEST::xyzw, BC::x, vf5, vf1);         // vmulax.xyzw acc, vf5, vf1
  c->mov128_vf_gpr(vf6, a3);                        // qmtc2.i vf6, a3
  c->vmadd_bc(DEST::xyzw, BC::y, vf5, vf13, vf1);   // vmaddy.xyzw vf5, vf13, vf1
  c->mov128_vf_gpr(vf14, t1);                       // qmtc2.i vf14, t1
  c->pextlb(a3, t5, r0);                            // pextlb a3, t5, r0
  c->mov128_vf_gpr(vf7, t2);                        // qmtc2.i vf7, t2
  c->pextlb(t1, t6, r0);                            // pextlb t1, t6, r0
  c->mov128_vf_gpr(vf15, t3);                       // qmtc2.i vf15, t3
  c->pextlb(t2, t4, r0);                            // pextlb t2, t4, r0
  c->vitof15(DEST::xyzw, vf6, vf6);                 // vitof15.xyzw vf6, vf6
  c->pextlb(t0, t0, r0);                            // pextlb t0, t0, r0
  c->vitof15(DEST::xyzw, vf14, vf14);               // vitof15.xyzw vf14, vf14
  c->pextlh(a3, a3, r0);                            // pextlh a3, a3, r0
  c->vitof15(DEST::xyzw, vf7, vf7);                 // vitof15.xyzw vf7, vf7
  c->pextlh(t1, t1, r0);                            // pextlh t1, t1, r0
  c->vitof15(DEST::xyzw, vf15, vf15);               // vitof15.xyzw vf15, vf15
  c->pextlh(t2, t2, r0);                            // pextlh t2, t2, r0
  c->sqc2(vf2, 0, a0);                              // sqc2 vf2, 0(a0)
  c->pextlh(t0, t0, r0);                            // pextlh t0, t0, r0
  c->sqc2(vf3, 16, a0);                             // sqc2 vf3, 16(a0)
  c->vmula_bc(DEST::xyzw, BC::x, vf6, vf1);         // vmulax.xyzw acc, vf6, vf1
  c->sqc2(vf4, 32, a0);                             // sqc2 vf4, 32(a0)
  c->vmadd_bc(DEST::xyzw, BC::y, vf6, vf14, vf1);   // vmaddy.xyzw vf6, vf14, vf1
  c->sqc2(vf5, 48, a0);                             // sqc2 vf5, 48(a0)
  c->vmula_bc(DEST::xyzw, BC::x, vf7, vf1);         // vmulax.xyzw acc, vf7, vf1
  c->mov128_vf_gpr(vf8, a3);                        // qmtc2.i vf8, a3
  c->vmadd_bc(DEST::xyzw, BC::y, vf7, vf15, vf1);   // vmaddy.xyzw vf7, vf15, vf1
  c->mov128_vf_gpr(vf16, t1);                       // qmtc2.i vf16, t1
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf9, t2);                        // qmtc2.i vf9, t2
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf17, t0);                       // qmtc2.i vf17, t0
  // nop                                            // sll r0, r0, 0
  c->vitof15(DEST::xyzw, vf8, vf8);                 // vitof15.xyzw vf8, vf8
  c->daddiu(a0, a0, 128);                           // daddiu a0, a0, 128
  c->vitof15(DEST::xyzw, vf16, vf16);               // vitof15.xyzw vf16, vf16
  c->lw(a3, 32, v1);                                // lw a3, 32(v1)
  c->vitof15(DEST::xyzw, vf9, vf9);                 // vitof15.xyzw vf9, vf9
  c->lw(t0, 32, a2);                                // lw t0, 32(a2)
  c->vitof15(DEST::xyzw, vf17, vf17);               // vitof15.xyzw vf17, vf17
  c->vmula_bc(DEST::xyzw, BC::x, vf8, vf1);         // vmulax.xyzw acc, vf8, vf1
  c->sqc2(vf6, -64, a0);                            // sqc2 vf6, -64(a0)
  c->vmadd_bc(DEST::xyzw, BC::y, vf8, vf16, vf1);   // vmaddy.xyzw vf8, vf16, vf1
  c->sqc2(vf7, -48, a0);                            // sqc2 vf7, -48(a0)
  c->vmula_bc(DEST::xyzw, BC::x, vf9, vf1);         // vmulax.xyzw acc, vf9, vf1
  c->daddiu(a1, a1, -1);                            // daddiu a1, a1, -1
  c->vmadd_bc(DEST::xyzw, BC::y, vf9, vf17, vf1);   // vmaddy.xyzw vf9, vf17, vf1
  c->daddiu(v1, v1, 32);                            // daddiu v1, v1, 32
  // nop                                            // sll r0, r0, 0
  c->daddiu(a2, a2, 32);                            // daddiu a2, a2, 32
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, -32, a0);                            // sqc2 vf8, -32(a0)
  bc = ((s64)c->sgpr64(a1)) > 0;                    // bgtz a1, L5
  c->sqc2(vf9, -16, a0);                            // sqc2 vf9, -16(a0)
  if (bc) {goto block_1;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
  goto end_of_function;                             // return

  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.ocean_wave_frames = intern_from_c("*ocean-wave-frames*").c();
  cache.ocean_work = intern_from_c("*ocean-work*").c();
  gLinkedFunctionTable.reg("ocean-interp-wave", execute, 256);
}

} // namespace ocean_interp_wave
} // namespace Mips2C


//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace ocean_generate_verts {
struct Cache {
  void* ocean_vu0_work; // *ocean-vu0-work*
  void* time_of_day_context; // *time-of-day-context*
  void* ocean_vu0_block; // ocean-vu0-block
  void* upload_vu0_program; // upload-vu0-program
  void* vector; // vector*!
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
  // nop                        |  subw.z vf24, vf24, vf00        31
  c->vfs[vf24].vf.sub(Mask::z, c->vf_src(vf24).vf, c->vf_src(vf00).vf.w());
  // nop                        |  mulax.xyzw ACC, vf05, vf25     32
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf05).vf, c->vf_src(vf25).vf.x());
  // nop                        |  madday.xyzw ACC, vf06, vf25    33
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf06].vf, c->vfs[vf25].vf.y());
  // nop                        |  maddz.xyz vf17, vf07, vf25     34
  c->acc.vf.madd(Mask::xyz, c->vfs[vf17].vf, c->vf_src(vf07).vf, c->vf_src(vf25).vf.z());
  // div Q, vf00.w, vf24.z      |  subw.z vf25, vf25, vf00        35
  c->vfs[vf25].vf.sub(Mask::z, c->vf_src(vf25).vf, c->vf_src(vf00).vf.w());   c->Q = 1.f / c->vf_src(vf24).vf.z();
  // nop                        |  mulax.xyzw ACC, vf05, vf26     36
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf05).vf, c->vf_src(vf26).vf.x());
  // nop                        |  madday.xyzw ACC, vf06, vf26    37
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf06].vf, c->vfs[vf26].vf.y());
  // nop                        |  maddz.xyz vf18, vf07, vf26     38
  c->acc.vf.madd(Mask::xyz, c->vfs[vf18].vf, c->vf_src(vf07).vf, c->vf_src(vf26).vf.z());
  // nop                        |  subw.z vf26, vf26, vf00        39
  c->vfs[vf26].vf.sub(Mask::z, c->vf_src(vf26).vf, c->vf_src(vf00).vf.w());
  // nop                        |  mulax.xyzw ACC, vf05, vf27     40
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf05).vf, c->vf_src(vf27).vf.x());
  // nop                        |  madday.xyzw ACC, vf06, vf27    41
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf06].vf, c->vfs[vf27].vf.y());
  // nop                        |  mul.w vf20, vf00, Q            42
  c->vfs[vf20].vf.mul(Mask::w, c->vf_src(vf00).vf, c->Q);
  // div Q, vf00.w, vf25.z      |  maddz.xyz vf19, vf07, vf27     43
  c->acc.vf.madd(Mask::xyz, c->vfs[vf19].vf, c->vf_src(vf07).vf, c->vf_src(vf27).vf.z());   c->Q = 1.f / c->vf_src(vf25).vf.z();
  // nop                        |  subw.z vf27, vf27, vf00        44
  c->vfs[vf27].vf.sub(Mask::z, c->vf_src(vf27).vf, c->vf_src(vf00).vf.w());
  // nop                        |  maxx.xyz vf16, vf16, vf00      45
  c->vfs[vf16].vf.max(Mask::xyz, c->vf_src(vf16).vf, c->vf_src(vf00).vf.x());
  // nop                        |  maxx.xyz vf17, vf17, vf00      46
  c->vfs[vf17].vf.max(Mask::xyz, c->vf_src(vf17).vf, c->vf_src(vf00).vf.x());
  // nop                        |  maxx.xyz vf18, vf18, vf00      47
  c->vfs[vf18].vf.max(Mask::xyz, c->vf_src(vf18).vf, c->vf_src(vf00).vf.x());
  // nop                        |  maxx.xyz vf19, vf19, vf00      48
  c->vfs[vf19].vf.max(Mask::xyz, c->vf_src(vf19).vf, c->vf_src(vf00).vf.x());
  // nop                        |  mul.w vf21, vf00, Q            49
  c->vfs[vf21].vf.mul(Mask::w, c->vf_src(vf00).vf, c->Q);
  // div Q, vf00.w, vf26.z      |  mula.xyzw ACC, vf01, vf11      50
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf01).vf, c->vf_src(vf11).vf);   c->Q = 1.f / c->vf_src(vf26).vf.z();
  // nop                        |  maddax.xyz ACC, vf08, vf16     51
  c->acc.vf.madda(Mask::xyz, c->vfs[vf08].vf, c->vfs[vf16].vf.x());
  // nop                        |  madday.xyz ACC, vf09, vf16     52
  c->acc.vf.madda(Mask::xyz, c->vfs[vf09].vf, c->vfs[vf16].vf.y());
  // nop                        |  maddz.xyz vf20, vf10, vf16     53
  c->acc.vf.madd(Mask::xyz, c->vfs[vf20].vf, c->vf_src(vf10).vf, c->vf_src(vf16).vf.z());
  // nop                        |  mula.xyzw ACC, vf01, vf11      54
  // vu.acc.mula(Mask::xyzw, vu.vf01, vu.vf11);
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf01).vf, c->vf_src(vf11).vf);
  // nop                        |  maddax.xyz ACC, vf08, vf17     55
  c->acc.vf.madda(Mask::xyz, c->vfs[vf08].vf, c->vfs[vf17].vf.x());
  // nop                        |  madday.xyz ACC, vf09, vf17     56
  c->acc.vf.madda(Mask::xyz, c->vfs[vf09].vf, c->vfs[vf17].vf.y());
  // nop                        |  mul.w vf22, vf00, Q            57
  c->vfs[vf22].vf.mul(Mask::w, c->vf_src(vf00).vf, c->Q);
  // nop                        |  maddz.xyz vf21, vf10, vf17     58
  c->acc.vf.madd(Mask::xyz, c->vfs[vf21].vf, c->vf_src(vf10).vf, c->vf_src(vf17).vf.z());
  // div Q, vf00.w, vf27.z      |  mula.xyzw ACC, vf01, vf11      59
  // vu.acc.mula(Mask::xyzw, vu.vf01, vu.vf11);
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf01).vf, c->vf_src(vf11).vf);
  c->Q = 1.f / c->vf_src(vf27).vf.z();
  // nop                        |  maddax.xyz ACC, vf08, vf18     60
  c->acc.vf.madda(Mask::xyz, c->vfs[vf08].vf, c->vfs[vf18].vf.x());
  // nop                        |  madday.xyz ACC, vf09, vf18     61
  c->acc.vf.madda(Mask::xyz, c->vfs[vf09].vf, c->vfs[vf18].vf.y());
  // nop                        |  maddz.xyz vf22, vf10, vf18     62
  c->acc.vf.madd(Mask::xyz, c->vfs[vf22].vf, c->vf_src(vf10).vf, c->vf_src(vf18).vf.z());
  // nop                        |  mula.xyzw ACC, vf01, vf11      63
  // vu.acc.mula(Mask::xyzw, vu.vf01, vu.vf11);
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf01).vf, c->vf_src(vf11).vf);
  // nop                        |  maddax.xyz ACC, vf08, vf19     64
  c->acc.vf.madda(Mask::xyz, c->vfs[vf08].vf, c->vfs[vf19].vf.x());
  // nop                        |  madday.xyz ACC, vf09, vf19     65
  c->acc.vf.madda(Mask::xyz, c->vfs[vf09].vf, c->vfs[vf19].vf.y());
  // nop                        |  maddz.xyz vf23, vf10, vf19     66
  c->acc.vf.madd(Mask::xyz, c->vfs[vf23].vf, c->vf_src(vf10).vf, c->vf_src(vf19).vf.z());
  // nop                        |  mul.w vf23, vf00, Q            67
  c->vfs[vf23].vf.mul(Mask::w, c->vf_src(vf00).vf, c->Q);
  // nop                        |  miniy.xyz vf20, vf20, vf12     68
  c->vfs[vf20].vf.mini(Mask::xyz, c->vf_src(vf20).vf, c->vf_src(vf12).vf.y());
  // nop                        |  miniy.xyz vf21, vf21, vf12     69
  c->vfs[vf21].vf.mini(Mask::xyz, c->vf_src(vf21).vf, c->vf_src(vf12).vf.y());
  // nop                        |  miniy.xyz vf22, vf22, vf12 :e  70
  c->vfs[vf22].vf.mini(Mask::xyz, c->vf_src(vf22).vf, c->vf_src(vf12).vf.y());
  // nop                        |  miniy.xyz vf23, vf23, vf12     71
  c->vfs[vf23].vf.mini(Mask::xyz, c->vf_src(vf23).vf, c->vf_src(vf12).vf.y());
}

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -64);                           // daddiu sp, sp, -64
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sd(fp, 8, sp);                                 // sd fp, 8(sp)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  c->sq(s4, 16, sp);                                // sq s4, 16(sp)
  c->sq(s5, 32, sp);                                // sq s5, 32(sp)
  c->sq(gp, 48, sp);                                // sq gp, 48(sp)
  c->mov64(s5, a0);                                 // or s5, a0, r0
  c->mov64(gp, a1);                                 // or gp, a1, r0
  c->load_symbol(t9, cache.upload_vu0_program);     // lw t9, upload-vu0-program(s7)
  c->load_symbol(a0, cache.ocean_vu0_block);        // lw a0, ocean-vu0-block(s7)
  c->load_symbol(v1, cache.ocean_vu0_work);         // lw v1, *ocean-vu0-work*(s7)
  c->daddiu(a1, v1, 160);                           // daddiu a1, v1, 160
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->load_symbol(t9, cache.vu_lights_light_group); // lw t9, vu-lights<-light-group!(s7)
  c->load_symbol(v1, cache.ocean_vu0_work);         // lw v1, *ocean-vu0-work*(s7)
  c->daddiu(a0, v1, 48);                            // daddiu a0, v1, 48
  c->load_symbol(v1, cache.time_of_day_context);    // lw v1, *time-of-day-context*(s7)
  c->daddiu(a1, v1, 156);                           // daddiu a1, v1, 156
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  // daddiu s4, fp, L7                                 // daddiu s4, fp, L7 TODO
  c->load_symbol(s4, cache.ocean_generate_verts_vector); // HACK
  c->load_symbol(t9, cache.vector);                 // lw t9, vector*!(s7)
  c->load_symbol(v1, cache.ocean_vu0_work);         // lw v1, *ocean-vu0-work*(s7)
  c->daddiu(a0, v1, 96);                            // daddiu a0, v1, 96
  c->load_symbol(v1, cache.ocean_vu0_work);         // lw v1, *ocean-vu0-work*(s7)
  c->daddiu(a1, v1, 96);                            // daddiu a1, v1, 96
  c->mov64(a2, s4);                                 // or a2, s4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->load_symbol(t9, cache.vector);                 // lw t9, vector*!(s7)
  c->load_symbol(v1, cache.ocean_vu0_work);         // lw v1, *ocean-vu0-work*(s7)
  c->daddiu(a0, v1, 112);                           // daddiu a0, v1, 112
  c->load_symbol(v1, cache.ocean_vu0_work);         // lw v1, *ocean-vu0-work*(s7)
  c->daddiu(a1, v1, 112);                           // daddiu a1, v1, 112
  c->mov64(a2, s4);                                 // or a2, s4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->load_symbol(t9, cache.vector);                 // lw t9, vector*!(s7)
  c->load_symbol(v1, cache.ocean_vu0_work);         // lw v1, *ocean-vu0-work*(s7)
  c->daddiu(a0, v1, 128);                           // daddiu a0, v1, 128
  c->load_symbol(v1, cache.ocean_vu0_work);         // lw v1, *ocean-vu0-work*(s7)
  c->daddiu(a1, v1, 128);                           // daddiu a1, v1, 128
  c->mov64(a2, s4);                                 // or a2, s4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->load_symbol(t9, cache.vector);                 // lw t9, vector*!(s7)
  c->load_symbol(v1, cache.ocean_vu0_work);         // lw v1, *ocean-vu0-work*(s7)
  c->daddiu(a0, v1, 144);                           // daddiu a0, v1, 144
  c->load_symbol(v1, cache.ocean_vu0_work);         // lw v1, *ocean-vu0-work*(s7)
  c->daddiu(a1, v1, 144);                           // daddiu a1, v1, 144
  c->mov64(a2, s4);                                 // or a2, s4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->load_symbol(v1, cache.ocean_vu0_work);         // lw v1, *ocean-vu0-work*(s7)
  c->vmax_bc(DEST::xyzw, BC::w, vf1, vf0, vf0);     // vmaxw.xyzw vf1, vf0, vf0
  c->lqc2(vf12, 0, v1);                             // lqc2 vf12, 0(v1)
  c->lqc2(vf5, 48, v1);                             // lqc2 vf5, 48(v1)
  c->lqc2(vf6, 64, v1);                             // lqc2 vf6, 64(v1)
  c->lqc2(vf7, 80, v1);                             // lqc2 vf7, 80(v1)
  c->lqc2(vf8, 96, v1);                             // lqc2 vf8, 96(v1)
  c->lqc2(vf9, 112, v1);                            // lqc2 vf9, 112(v1)
  c->lqc2(vf10, 128, v1);                           // lqc2 vf10, 128(v1)
  c->lqc2(vf11, 144, v1);                           // lqc2 vf11, 144(v1)
  c->addiu(v1, r0, 31);                             // addiu v1, r0, 31
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->daddiu(a1, gp, 128);                           // daddiu a1, gp, 128

  block_1:
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

  block_2:
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
  bc = ((s64)c->sgpr64(t2)) > 0;                    // bgtz t2, L3
  c->lqc2(vf4, -16, a1);                            // lqc2 vf4, -16(a1)
  if (bc) {goto block_2;}                           // branch non-likely

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
  bc = ((s64)c->sgpr64(v1)) > 0;                    // bgtz v1, L2
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L2
  c->mov64(a1, a0);                                 // or a1, a0, r0
  if (bc) {goto block_1;}                           // branch non-likely

  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->ld(fp, 8, sp);                                 // ld fp, 8(sp)
  c->lq(gp, 48, sp);                                // lq gp, 48(sp)
  c->lq(s5, 32, sp);                                // lq s5, 32(sp)
  c->lq(s4, 16, sp);                                // lq s4, 16(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 64);                            // daddiu sp, sp, 64
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.ocean_vu0_work = intern_from_c("*ocean-vu0-work*").c();
  cache.time_of_day_context = intern_from_c("*time-of-day-context*").c();
  cache.ocean_vu0_block = intern_from_c("ocean-vu0-block").c();
  cache.upload_vu0_program = intern_from_c("upload-vu0-program").c();
  cache.vector = intern_from_c("vector*!").c();
  cache.vu_lights_light_group = intern_from_c("vu-lights<-light-group!").c();
  cache.ocean_generate_verts_vector = intern_from_c("*ocean-generate-verts-vector*").c();
  gLinkedFunctionTable.reg("ocean-generate-verts", execute, 128);
}

} // namespace ocean_generate_verts
} // namespace Mips2C
