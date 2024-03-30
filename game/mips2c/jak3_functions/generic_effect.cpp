
//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace generic_light_proc {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;

void vcallms0(ExecutionContext* c) {
  // move.xyzw vf21, vf17       |  mulax.xyzw ACC, vf10, vf01
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf10).vf, c->vf_src(vf01).vf.x());   c->vfs[vf21].vf.move(Mask::xyzw, c->vf_src(vf17).vf);
  // move.xyzw vf22, vf18       |  madday.xyzw ACC, vf11, vf01
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf11].vf, c->vfs[vf01].vf.y());   c->vfs[vf22].vf.move(Mask::xyzw, c->vf_src(vf18).vf);
  // move.xyzw vf23, vf19       |  maddz.xyzw vf01, vf12, vf01
  c->acc.vf.madd(Mask::xyzw, c->vfs[vf01].vf, c->vf_src(vf12).vf, c->vf_src(vf01).vf.z());   c->vfs[vf23].vf.move(Mask::xyzw, c->vf_src(vf19).vf);
  // move.xyzw vf24, vf20       |  mulax.xyzw ACC, vf10, vf02
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf10).vf, c->vf_src(vf02).vf.x());   c->vfs[vf24].vf.move(Mask::xyzw, c->vf_src(vf20).vf);
  // nop                        |  itof0.xyzw vf17, vf05
  c->vfs[vf17].vf.itof0(Mask::xyzw, c->vf_src(vf05).vf);
  // nop                        |  itof0.xyzw vf18, vf06
  c->vfs[vf18].vf.itof0(Mask::xyzw, c->vf_src(vf06).vf);
  // nop                        |  itof0.xyzw vf19, vf07
  c->vfs[vf19].vf.itof0(Mask::xyzw, c->vf_src(vf07).vf);
  // nop                        |  itof0.xyzw vf20, vf08
  c->vfs[vf20].vf.itof0(Mask::xyzw, c->vf_src(vf08).vf);

  // nop                        |  madday.xyzw ACC, vf11, vf02
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf11].vf, c->vfs[vf02].vf.y());
  // nop                        |  maddz.xyzw vf02, vf12, vf02
  c->acc.vf.madd(Mask::xyzw, c->vfs[vf02].vf, c->vf_src(vf12).vf, c->vf_src(vf02).vf.z());
  // nop                        |  mulax.xyzw ACC, vf10, vf03
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf10).vf, c->vf_src(vf03).vf.x());
  // nop                        |  madday.xyzw ACC, vf11, vf03
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf11].vf, c->vfs[vf03].vf.y());
  // nop                        |  maddz.xyzw vf03, vf12, vf03
  c->acc.vf.madd(Mask::xyzw, c->vfs[vf03].vf, c->vf_src(vf12).vf, c->vf_src(vf03).vf.z());
  // nop                        |  mulax.xyzw ACC, vf10, vf04
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf10).vf, c->vf_src(vf04).vf.x());
  // nop                        |  madday.xyzw ACC, vf11, vf04
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf11].vf, c->vfs[vf04].vf.y());
  // nop                        |  maddz.xyzw vf04, vf12, vf04
  c->acc.vf.madd(Mask::xyzw, c->vfs[vf04].vf, c->vf_src(vf12).vf, c->vf_src(vf04).vf.z());
  // nop                        |  maxx.xyzw vf01, vf01, vf00
  c->vfs[vf01].vf.max(Mask::xyzw, c->vf_src(vf01).vf, c->vf_src(vf00).vf.x());
  // nop                        |  maxx.xyzw vf02, vf02, vf00
  c->vfs[vf02].vf.max(Mask::xyzw, c->vf_src(vf02).vf, c->vf_src(vf00).vf.x());
  // nop                        |  maxx.xyzw vf03, vf03, vf00
  c->vfs[vf03].vf.max(Mask::xyzw, c->vf_src(vf03).vf, c->vf_src(vf00).vf.x());
  // nop                        |  maxx.xyzw vf04, vf04, vf00
  c->vfs[vf04].vf.max(Mask::xyzw, c->vf_src(vf04).vf, c->vf_src(vf00).vf.x());
  // nop                        |  mulaw.xyzw ACC, vf13, vf00
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf13).vf, c->vf_src(vf00).vf.w());
  // nop                        |  maddax.xyzw ACC, vf14, vf01
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf14].vf, c->vfs[vf01].vf.x());
  // nop                        |  madday.xyzw ACC, vf15, vf01
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf15].vf, c->vfs[vf01].vf.y());
  // nop                        |  maddz.xyzw vf01, vf16, vf01
  c->acc.vf.madd(Mask::xyzw, c->vfs[vf01].vf, c->vf_src(vf16).vf, c->vf_src(vf01).vf.z());
  // nop                        |  mulaw.xyzw ACC, vf13, vf00
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf13).vf, c->vf_src(vf00).vf.w());
  // nop                        |  maddax.xyzw ACC, vf14, vf02
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf14].vf, c->vfs[vf02].vf.x());
  // nop                        |  madday.xyzw ACC, vf15, vf02
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf15].vf, c->vfs[vf02].vf.y());
  // nop                        |  maddz.xyzw vf02, vf16, vf02
  c->acc.vf.madd(Mask::xyzw, c->vfs[vf02].vf, c->vf_src(vf16).vf, c->vf_src(vf02).vf.z());
  // nop                        |  mulaw.xyzw ACC, vf13, vf00
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf13).vf, c->vf_src(vf00).vf.w());
  // nop                        |  maddax.xyzw ACC, vf14, vf03
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf14].vf, c->vfs[vf03].vf.x());
  // nop                        |  madday.xyzw ACC, vf15, vf03
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf15].vf, c->vfs[vf03].vf.y());
  // nop                        |  maddz.xyzw vf03, vf16, vf03
  c->acc.vf.madd(Mask::xyzw, c->vfs[vf03].vf, c->vf_src(vf16).vf, c->vf_src(vf03).vf.z());
  // nop                        |  mulaw.xyzw ACC, vf13, vf00
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf13).vf, c->vf_src(vf00).vf.w());
  // nop                        |  maddax.xyzw ACC, vf14, vf04
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf14].vf, c->vfs[vf04].vf.x());
  // nop                        |  madday.xyzw ACC, vf15, vf04
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf15].vf, c->vfs[vf04].vf.y());
  // nop                        |  maddz.xyzw vf04, vf16, vf04
  c->acc.vf.madd(Mask::xyzw, c->vfs[vf04].vf, c->vf_src(vf16).vf, c->vf_src(vf04).vf.z());
  // nop                        |  mul.xyzw vf17, vf17, vf01
  c->vfs[vf17].vf.mul(Mask::xyzw, c->vf_src(vf17).vf, c->vf_src(vf01).vf);
  // nop                        |  mul.xyzw vf18, vf18, vf02
  c->vfs[vf18].vf.mul(Mask::xyzw, c->vf_src(vf18).vf, c->vf_src(vf02).vf);
  // nop                        |  mul.xyzw vf19, vf19, vf03
  c->vfs[vf19].vf.mul(Mask::xyzw, c->vf_src(vf19).vf, c->vf_src(vf03).vf);
  // nop                        |  mul.xyzw vf20, vf20, vf04
  c->vfs[vf20].vf.mul(Mask::xyzw, c->vf_src(vf20).vf, c->vf_src(vf04).vf);
  // nop                        |  minix.xyzw vf17, vf17, vf09
  c->vfs[vf17].vf.mini(Mask::xyzw, c->vf_src(vf17).vf, c->vf_src(vf09).vf.x());
  // nop                        |  minix.xyzw vf18, vf18, vf09
  c->vfs[vf18].vf.mini(Mask::xyzw, c->vf_src(vf18).vf, c->vf_src(vf09).vf.x());
  // nop                        |  minix.xyzw vf19, vf19, vf09
  c->vfs[vf19].vf.mini(Mask::xyzw, c->vf_src(vf19).vf, c->vf_src(vf09).vf.x());
  // nop                        |  minix.xyzw vf20, vf20, vf09
  c->vfs[vf20].vf.mini(Mask::xyzw, c->vf_src(vf20).vf, c->vf_src(vf09).vf.x());
  //fmt::print("light:\n {}\n {}\n {}\n {}\n\n", c->vf_src(vf17).vf.print(), c->vf_src(vf18).vf.print(), c->vf_src(vf19).vf.print(), c->vf_src(vf20).vf.print());



  // nop                        |  ftoi0.xyzw vf17, vf17
  c->vfs[vf17].vf.ftoi0(Mask::xyzw, c->vf_src(vf17).vf);
  // nop                        |  ftoi0.xyzw vf18, vf18
  c->vfs[vf18].vf.ftoi0(Mask::xyzw, c->vf_src(vf18).vf);
  // nop                        |  ftoi0.xyzw vf19, vf19 :e
  c->vfs[vf19].vf.ftoi0(Mask::xyzw, c->vf_src(vf19).vf);
  // nop                        |  ftoi0.xyzw vf20, vf20
  c->vfs[vf20].vf.ftoi0(Mask::xyzw, c->vf_src(vf20).vf);
}


u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->daddiu(sp, sp, -96);                           // daddiu sp, sp, -96
  c->sd(ra, 12432, at);                             // sd ra, 12432(at)
  c->sq(s2, 12448, at);                             // sq s2, 12448(at)
  c->sq(s3, 12464, at);                             // sq s3, 12464(at)
  c->sq(s4, 12480, at);                             // sq s4, 12480(at)
  c->sq(s5, 12496, at);                             // sq s5, 12496(at)
  c->sq(gp, 12512, at);                             // sq gp, 12512(at)
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 44, at);                                // lw v1, 44(at)
  // nop                                            // sll r0, r0, 0
  c->lw(a1, 36, at);                                // lw a1, 36(at)
  // nop                                            // sll r0, r0, 0
  c->lw(a0, 4, v1);                                 // lw a0, 4(v1)
  // nop                                            // sll r0, r0, 0
  c->lw(t0, 0, v1);                                 // lw t0, 0(v1)
  // nop                                            // sll r0, r0, 0
  c->lw(t3, 4, at);                                 // lw t3, 4(at)
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 8, at);                                 // lw v1, 8(at)
  c->addiu(a3, r0, 255);                            // addiu a3, r0, 255
  c->lw(t2, 12, at);                                // lw t2, 12(at)
  c->addiu(a2, r0, 256);                            // addiu a2, r0, 256
  c->lui(t1, -2);                                   // lui t1, -2
  c->mov64(t4, a1);                                 // or t4, a1, r0
  c->lqc2(vf10, 12688, at);                         // lqc2 vf10, 12688(at)
  c->ori(a1, t1, 65534);                            // ori a1, t1, 65534
  c->lqc2(vf11, 12704, at);                         // lqc2 vf11, 12704(at)
  c->pextlw(a1, a1, a1);                            // pextlw a1, a1, a1
  c->lqc2(vf12, 12720, at);                         // lqc2 vf12, 12720(at)
  c->pextlw(t1, a0, a0);                            // pextlw t1, a0, a0
  c->lqc2(vf15, 12752, at);                         // lqc2 vf15, 12752(at)
  c->pextlw(a0, a1, a1);                            // pextlw a0, a1, a1
  c->lqc2(vf14, 12736, at);                         // lqc2 vf14, 12736(at)
  c->pextlw(a1, t1, t1);                            // pextlw a1, t1, t1
  c->lqc2(vf16, 12768, at);                         // lqc2 vf16, 12768(at)
  c->pcpyh(a3, a3);                                 // pcpyh a3, a3
  c->lqc2(vf13, 12784, at);                         // lqc2 vf13, 12784(at)
  c->pcpyh(t1, a2);                                 // pcpyh t1, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyld(a2, a3, a3);                            // pcpyld a2, a3, a3
  c->lqc2(vf9, 12144, at);                          // lqc2 vf9, 12144(at)
  c->pcpyld(a3, t1, t1);                            // pcpyld a3, t1, t1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->ldr(t1, 0, t0);                                // ldr t1, 0(t0)
  // nop                                            // sll r0, r0, 0
  c->ldl(t1, 7, t0);                                // ldl t1, 7(t0)
  // nop                                            // sll r0, r0, 0
  c->daddiu(t0, t0, 8);                             // daddiu t0, t0, 8
  c->pextlh(t1, r0, t1);                            // pextlh t1, r0, t1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pand(t5, t1, a2);                              // pand t5, t1, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psllw(t5, t5, 5);                              // psllw t5, t5, 5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->paddw(s5, t5, a1);                             // paddw s5, t5, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsrl32(t9, s5, 0);                             // dsrl32 t9, s5, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyud(s4, s5, r0);                            // pcpyud s4, s5, r0
  c->lq(t6, 0, s5);                                 // lq t6, 0(s5)
  c->dsrl32(ra, s4, 0);                             // dsrl32 ra, s4, 0
  c->lq(t7, 0, t9);                                 // lq t7, 0(t9)
  c->pand(t8, t1, a3);                              // pand t8, t1, a3
  c->lq(t5, 0, s4);                                 // lq t5, 0(s4)
  c->psraw(gp, t8, 8);                              // psraw gp, t8, 8
  c->lq(t8, 0, ra);                                 // lq t8, 0(ra)
  c->pextuw(s3, t7, t6);                            // pextuw s3, t7, t6
  c->lq(s5, 16, s5);                                // lq s5, 16(s5)
  c->pextuw(s2, t8, t5);                            // pextuw s2, t8, t5
  c->lq(t9, 16, t9);                                // lq t9, 16(t9)
  c->pcpyud(s3, s3, s2);                            // pcpyud s3, s3, s2
  c->lq(s4, 16, s4);                                // lq s4, 16(s4)
  c->pand(s3, s3, a0);                              // pand s3, s3, a0
  c->lq(ra, 16, ra);                                // lq ra, 16(ra)
  c->por(s3, s3, gp);                               // por s3, s3, gp
  c->mov128_vf_gpr(vf1, s5);                        // qmtc2.ni vf1, s5
  c->pextub(gp, r0, s5);                            // pextub gp, r0, s5
  c->sq(s3, 0, t2);                                 // sq s3, 0(t2)
  c->pextub(s5, r0, t9);                            // pextub s5, r0, t9
  c->mov128_vf_gpr(vf2, t9);                        // qmtc2.ni vf2, t9
  c->pextub(t9, r0, s4);                            // pextub t9, r0, s4
  c->mov128_vf_gpr(vf3, s4);                        // qmtc2.ni vf3, s4
  c->pextub(s4, r0, ra);                            // pextub s4, r0, ra
  c->mov128_vf_gpr(vf4, ra);                        // qmtc2.ni vf4, ra
  c->pextuh(ra, r0, gp);                            // pextuh ra, r0, gp
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextuh(gp, r0, s5);                            // pextuh gp, r0, s5
  c->mov128_vf_gpr(vf5, ra);                        // qmtc2.ni vf5, ra
  c->pextuh(t9, r0, t9);                            // pextuh t9, r0, t9
  c->mov128_vf_gpr(vf6, gp);                        // qmtc2.ni vf6, gp
  c->pextuh(ra, r0, s4);                            // pextuh ra, r0, s4
  c->mov128_vf_gpr(vf7, t9);                        // qmtc2.ni vf7, t9
  c->prot3w(t8, t8);                                // prot3w t8, t8
  c->mov128_vf_gpr(vf8, ra);                        // qmtc2.ni vf8, ra
  c->prot3w(t7, t7);                                // prot3w t7, t7
  // Unknown instr: vcallms 0
  vcallms0(c);
  c->pextuw(t9, t7, t6);                            // pextuw t9, t7, t6
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyld(t7, t5, t7);                            // pcpyld t7, t5, t7
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyld(t6, t9, t6);                            // pcpyld t6, t9, t6
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->daddiu(t2, t2, 16);                            // daddiu t2, t2, 16
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextuw(t5, t8, t5);                            // pextuw t5, t8, t5
  c->sq(t7, 16, t3);                                // sq t7, 16(t3)
  c->pcpyld(t5, t8, t5);                            // pcpyld t5, t8, t5
  c->sq(t6, 0, t3);                                 // sq t6, 0(t3)
  // nop                                            // sll r0, r0, 0
  c->sq(t5, 32, t3);                                // sq t5, 32(t3)
  c->daddiu(t3, t3, 48);                            // daddiu t3, t3, 48
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->daddiu(t4, t4, -4);                            // daddiu t4, t4, -4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  bc = ((s64)c->sgpr64(t4)) <= 0;                   // blez t4, L91
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  if (bc) {goto block_2;}                           // branch non-likely


  block_1:
  // nop                                            // sll r0, r0, 0
  c->ldr(t1, 0, t0);                                // ldr t1, 0(t0)
  // nop                                            // sll r0, r0, 0
  c->ldl(t1, 7, t0);                                // ldl t1, 7(t0)
  // nop                                            // sll r0, r0, 0
  c->daddiu(t0, t0, 8);                             // daddiu t0, t0, 8
  c->pextlh(t1, r0, t1);                            // pextlh t1, r0, t1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pand(t5, t1, a2);                              // pand t5, t1, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psllw(t5, t5, 5);                              // psllw t5, t5, 5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->paddw(s5, t5, a1);                             // paddw s5, t5, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsrl32(t9, s5, 0);                             // dsrl32 t9, s5, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyud(s4, s5, r0);                            // pcpyud s4, s5, r0
  c->lq(t6, 0, s5);                                 // lq t6, 0(s5)
  c->dsrl32(ra, s4, 0);                             // dsrl32 ra, s4, 0
  c->lq(t7, 0, t9);                                 // lq t7, 0(t9)
  c->pand(t8, t1, a3);                              // pand t8, t1, a3
  c->lq(t5, 0, s4);                                 // lq t5, 0(s4)
  c->psraw(gp, t8, 8);                              // psraw gp, t8, 8
  c->lq(t8, 0, ra);                                 // lq t8, 0(ra)
  c->pextuw(s3, t7, t6);                            // pextuw s3, t7, t6
  c->lq(s5, 16, s5);                                // lq s5, 16(s5)
  c->pextuw(s2, t8, t5);                            // pextuw s2, t8, t5
  c->lq(t9, 16, t9);                                // lq t9, 16(t9)
  c->pcpyud(s3, s3, s2);                            // pcpyud s3, s3, s2
  c->lq(s4, 16, s4);                                // lq s4, 16(s4)
  c->pand(s3, s3, a0);                              // pand s3, s3, a0
  c->lq(ra, 16, ra);                                // lq ra, 16(ra)
  c->por(s3, s3, gp);                               // por s3, s3, gp
  c->mov128_vf_gpr(vf1, s5);                        // qmtc2.ni vf1, s5
  c->pextub(gp, r0, s5);                            // pextub gp, r0, s5
  c->sq(s3, 0, t2);                                 // sq s3, 0(t2)
  c->pextub(s5, r0, t9);                            // pextub s5, r0, t9
  c->mov128_vf_gpr(vf2, t9);                        // qmtc2.ni vf2, t9
  c->pextub(t9, r0, s4);                            // pextub t9, r0, s4
  c->mov128_vf_gpr(vf3, s4);                        // qmtc2.ni vf3, s4
  c->pextub(s4, r0, ra);                            // pextub s4, r0, ra
  c->mov128_vf_gpr(vf4, ra);                        // qmtc2.ni vf4, ra
  c->pextuh(ra, r0, gp);                            // pextuh ra, r0, gp
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextuh(gp, r0, s5);                            // pextuh gp, r0, s5
  c->mov128_vf_gpr(vf5, ra);                        // qmtc2.ni vf5, ra
  c->pextuh(t9, r0, t9);                            // pextuh t9, r0, t9
  c->mov128_vf_gpr(vf6, gp);                        // qmtc2.ni vf6, gp
  c->pextuh(ra, r0, s4);                            // pextuh ra, r0, s4
  c->mov128_vf_gpr(vf7, t9);                        // qmtc2.ni vf7, t9
  c->prot3w(t8, t8);                                // prot3w t8, t8
  c->mov128_vf_gpr(vf8, ra);                        // qmtc2.ni vf8, ra
  c->prot3w(t7, t7);                                // prot3w t7, t7
  // Unknown instr: vcallms 0
  vcallms0(c);
  c->pextuw(t9, t7, t6);                            // pextuw t9, t7, t6
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyld(t7, t5, t7);                            // pcpyld t7, t5, t7
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyld(t6, t9, t6);                            // pcpyld t6, t9, t6
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextuw(t5, t8, t5);                            // pextuw t5, t8, t5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyld(t5, t8, t5);                            // pcpyld t5, t8, t5
  c->sq(t6, 0, t3);                                 // sq t6, 0(t3)
  // nop                                            // sll r0, r0, 0
  c->sq(t7, 16, t3);                                // sq t7, 16(t3)
  // nop                                            // sll r0, r0, 0
  c->sq(t5, 32, t3);                                // sq t5, 32(t3)
  c->daddiu(t2, t2, 16);                            // daddiu t2, t2, 16
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->daddiu(t3, t3, 48);                            // daddiu t3, t3, 48
  c->mov128_gpr_vf(t7, vf21);                       // qmfc2.ni t7, vf21
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t8, vf22);                       // qmfc2.ni t8, vf22
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t5, vf23);                       // qmfc2.ni t5, vf23
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t6, vf24);                       // qmfc2.ni t6, vf24
  c->ppach(t7, t8, t7);                             // ppach t7, t8, t7
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppach(t5, t6, t5);                             // ppach t5, t6, t5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppacb(t5, t5, t7);                             // ppacb t5, t5, t7
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->sq(t5, 0, v1);                                 // sq t5, 0(v1)
  c->daddiu(t4, t4, -4);                            // daddiu t4, t4, -4
  c->daddiu(v1, v1, 16);                            // daddiu v1, v1, 16
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  bc = ((s64)c->sgpr64(t4)) > 0;                    // bgtz t4, L90
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely


  block_2:
  // nop                                            // sll r0, r0, 0
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a2, vf17);                       // qmfc2.ni a2, vf17
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a3, vf18);                       // qmfc2.ni a3, vf18
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a0, vf19);                       // qmfc2.ni a0, vf19
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a1, vf20);                       // qmfc2.ni a1, vf20
  c->ppach(a2, a3, a2);                             // ppach a2, a3, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppach(a0, a1, a0);                             // ppach a0, a1, a0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppacb(a0, a0, a2);                             // ppacb a0, a0, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 12432, at);                             // ld ra, 12432(at)
  c->lq(gp, 12512, at);                             // lq gp, 12512(at)
  c->lq(s5, 12496, at);                             // lq s5, 12496(at)
  c->lq(s4, 12480, at);                             // lq s4, 12480(at)
  c->lq(s3, 12464, at);                             // lq s3, 12464(at)
  c->lq(s2, 12448, at);                             // lq s2, 12448(at)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 96);                            // daddiu sp, sp, 96
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c(-1, 0, "*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("generic-light-proc", execute, 128);
}

} // namespace generic_light_proc
} // namespace Mips2C
// add generic_light_proc::link to the link callback table for the object file.
// FWD DEC:

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace generic_envmap_proc {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;


void vcallms48(ExecutionContext* c) {
  // nop                        |  mulx.xyzw vf13, vf09, vf31
  c->vfs[vf13].vf.mul(Mask::xyzw, c->vf_src(vf09).vf, c->vf_src(vf31).vf.x());
  // nop                        |  subw.z vf21, vf21, vf00
  c->vfs[vf21].vf.sub(Mask::z, c->vf_src(vf21).vf, c->vf_src(vf00).vf.w());
  // nop                        |  addy.x vf29, vf29, vf29
  c->vfs[vf29].vf.add(Mask::x, c->vf_src(vf29).vf, c->vf_src(vf29).vf.y());
  // nop                        |  mulx.xyz vf08, vf08, vf30
  c->vfs[vf08].vf.mul(Mask::xyz, c->vf_src(vf08).vf, c->vf_src(vf30).vf.x());
  // nop                        |  addw.xy vf05, vf05, vf31
  c->vfs[vf05].vf.add(Mask::xy, c->vf_src(vf05).vf, c->vf_src(vf31).vf.w());
  // nop                        |  mul.xyz vf30, vf21, vf13
  c->vfs[vf30].vf.mul(Mask::xyz, c->vf_src(vf21).vf, c->vf_src(vf13).vf);
  // nop                        |  addz.x vf29, vf29, vf29
  c->vfs[vf29].vf.add(Mask::x, c->vf_src(vf29).vf, c->vf_src(vf29).vf.z());
  // nop                        |  add.xyz vf08, vf08, vf16
  c->vfs[vf08].vf.add(Mask::xyz, c->vf_src(vf08).vf, c->vf_src(vf16).vf);
  // move.xyzw vf28, vf27       |  ftoi12.xy vf17, vf05
  c->vfs[vf17].vf.ftoi12(Mask::xy, c->vf_src(vf05).vf);   c->vfs[vf28].vf.move(Mask::xyzw, c->vf_src(vf27).vf);
  // move.xyzw vf02, vf22       |  addy.x vf30, vf30, vf30
  c->vfs[vf30].vf.add(Mask::x, c->vf_src(vf30).vf, c->vf_src(vf30).vf.y());   c->vfs[vf02].vf.move(Mask::xyzw, c->vf_src(vf22).vf);
  // rsqrt Q, vf31.z, vf29.x    |  mul.xyz vf06, vf06, Q
  c->vfs[vf06].vf.mul(Mask::xyz, c->vf_src(vf06).vf, c->Q);   c->Q = c->vf_src(vf31).vf.z() / std::sqrt(c->vf_src(vf29).vf.x());
  // nop                        |  mul.xyz vf29, vf08, vf08
  c->vfs[vf29].vf.mul(Mask::xyz, c->vf_src(vf08).vf, c->vf_src(vf08).vf);
  // nop                        |  mulx.xyz vf01, vf21, vf28
  c->vfs[vf01].vf.mul(Mask::xyz, c->vf_src(vf21).vf, c->vf_src(vf28).vf.x());
  // nop                        |  addz.x vf30, vf30, vf30
  c->vfs[vf30].vf.add(Mask::x, c->vf_src(vf30).vf, c->vf_src(vf30).vf.z());
  // nop                        |  mulx.xyzw vf14, vf10, vf31
  c->vfs[vf14].vf.mul(Mask::xyzw, c->vf_src(vf10).vf, c->vf_src(vf31).vf.x());
  // nop                        |  subw.z vf02, vf02, vf00
  c->vfs[vf02].vf.sub(Mask::z, c->vf_src(vf02).vf, c->vf_src(vf00).vf.w());
  // nop                        |  addy.x vf29, vf29, vf29
  c->vfs[vf29].vf.add(Mask::x, c->vf_src(vf29).vf, c->vf_src(vf29).vf.y());
  // nop                        |  mulx.xyz vf01, vf01, vf30
  c->vfs[vf01].vf.mul(Mask::xyz, c->vf_src(vf01).vf, c->vf_src(vf30).vf.x());
  // nop                        |  addw.xy vf06, vf06, vf31
  c->vfs[vf06].vf.add(Mask::xy, c->vf_src(vf06).vf, c->vf_src(vf31).vf.w());
  // nop                        |  mul.xyz vf30, vf02, vf14
  c->vfs[vf30].vf.mul(Mask::xyz, c->vf_src(vf02).vf, c->vf_src(vf14).vf);
  // nop                        |  addz.x vf29, vf29, vf29
  c->vfs[vf29].vf.add(Mask::x, c->vf_src(vf29).vf, c->vf_src(vf29).vf.z());
  // nop                        |  add.xyz vf01, vf01, vf13
  c->vfs[vf01].vf.add(Mask::xyz, c->vf_src(vf01).vf, c->vf_src(vf13).vf);
  // nop                        |  ftoi12.xy vf18, vf06
  c->vfs[vf18].vf.ftoi12(Mask::xy, c->vf_src(vf06).vf);
  // nop                        |  addy.x vf30, vf30, vf30
  c->vfs[vf30].vf.add(Mask::x, c->vf_src(vf30).vf, c->vf_src(vf30).vf.y());
  // rsqrt Q, vf31.z, vf29.x    |  mul.xyz vf07, vf07, Q
  c->vfs[vf07].vf.mul(Mask::xyz, c->vf_src(vf07).vf, c->Q);   c->Q = c->vf_src(vf31).vf.z() / std::sqrt(c->vf_src(vf29).vf.x());
  // move.xyzw vf03, vf23       |  mul.xyz vf29, vf01, vf01
  c->vfs[vf29].vf.mul(Mask::xyz, c->vf_src(vf01).vf, c->vf_src(vf01).vf);   c->vfs[vf03].vf.move(Mask::xyzw, c->vf_src(vf23).vf);
  // nop                        |  muly.xyz vf02, vf02, vf28
  c->vfs[vf02].vf.mul(Mask::xyz, c->vf_src(vf02).vf, c->vf_src(vf28).vf.y());
  // nop                        |  addz.x vf30, vf30, vf30
  c->vfs[vf30].vf.add(Mask::x, c->vf_src(vf30).vf, c->vf_src(vf30).vf.z());
  // nop                        |  mulx.xyzw vf15, vf11, vf31
  c->vfs[vf15].vf.mul(Mask::xyzw, c->vf_src(vf11).vf, c->vf_src(vf31).vf.x());
  // nop                        |  subw.z vf03, vf03, vf00
  c->vfs[vf03].vf.sub(Mask::z, c->vf_src(vf03).vf, c->vf_src(vf00).vf.w());
  // nop                        |  addy.x vf29, vf29, vf29
  c->vfs[vf29].vf.add(Mask::x, c->vf_src(vf29).vf, c->vf_src(vf29).vf.y());
  // nop                        |  mulx.xyz vf02, vf02, vf30
  c->vfs[vf02].vf.mul(Mask::xyz, c->vf_src(vf02).vf, c->vf_src(vf30).vf.x());
  // nop                        |  addw.xy vf07, vf07, vf31
  c->vfs[vf07].vf.add(Mask::xy, c->vf_src(vf07).vf, c->vf_src(vf31).vf.w());
  // nop                        |  mul.xyz vf30, vf03, vf15
  c->vfs[vf30].vf.mul(Mask::xyz, c->vf_src(vf03).vf, c->vf_src(vf15).vf);
  // nop                        |  addz.x vf29, vf29, vf29
  c->vfs[vf29].vf.add(Mask::x, c->vf_src(vf29).vf, c->vf_src(vf29).vf.z());
  // nop                        |  add.xyz vf02, vf02, vf14
  c->vfs[vf02].vf.add(Mask::xyz, c->vf_src(vf02).vf, c->vf_src(vf14).vf);
  // nop                        |  ftoi12.xy vf19, vf07
  c->vfs[vf19].vf.ftoi12(Mask::xy, c->vf_src(vf07).vf);
  // nop                        |  addy.x vf30, vf30, vf30
  c->vfs[vf30].vf.add(Mask::x, c->vf_src(vf30).vf, c->vf_src(vf30).vf.y());
  // rsqrt Q, vf31.z, vf29.x    |  mul.xyz vf08, vf08, Q
  c->vfs[vf08].vf.mul(Mask::xyz, c->vf_src(vf08).vf, c->Q);   c->Q = c->vf_src(vf31).vf.z() / std::sqrt(c->vf_src(vf29).vf.x());
  // move.xyzw vf04, vf24       |  mul.xyz vf29, vf02, vf02
  c->vfs[vf29].vf.mul(Mask::xyz, c->vf_src(vf02).vf, c->vf_src(vf02).vf);   c->vfs[vf04].vf.move(Mask::xyzw, c->vf_src(vf24).vf);
  // nop                        |  mulz.xyz vf03, vf03, vf28
  c->vfs[vf03].vf.mul(Mask::xyz, c->vf_src(vf03).vf, c->vf_src(vf28).vf.z());
  // nop                        |  addz.x vf30, vf30, vf30
  c->vfs[vf30].vf.add(Mask::x, c->vf_src(vf30).vf, c->vf_src(vf30).vf.z());
  // nop                        |  mulx.xyzw vf16, vf12, vf31
  c->vfs[vf16].vf.mul(Mask::xyzw, c->vf_src(vf12).vf, c->vf_src(vf31).vf.x());
  // nop                        |  subw.z vf04, vf04, vf00
  c->vfs[vf04].vf.sub(Mask::z, c->vf_src(vf04).vf, c->vf_src(vf00).vf.w());
  // nop                        |  addy.x vf29, vf29, vf29
  c->vfs[vf29].vf.add(Mask::x, c->vf_src(vf29).vf, c->vf_src(vf29).vf.y());
  // nop                        |  mulx.xyz vf03, vf03, vf30
  c->vfs[vf03].vf.mul(Mask::xyz, c->vf_src(vf03).vf, c->vf_src(vf30).vf.x());
  // nop                        |  addw.xy vf08, vf08, vf31
  c->vfs[vf08].vf.add(Mask::xy, c->vf_src(vf08).vf, c->vf_src(vf31).vf.w());
  // nop                        |  mul.xyz vf30, vf04, vf16
  c->vfs[vf30].vf.mul(Mask::xyz, c->vf_src(vf04).vf, c->vf_src(vf16).vf);
  // nop                        |  addz.x vf29, vf29, vf29
  c->vfs[vf29].vf.add(Mask::x, c->vf_src(vf29).vf, c->vf_src(vf29).vf.z());
  // nop                        |  add.xyz vf03, vf03, vf15
  c->vfs[vf03].vf.add(Mask::xyz, c->vf_src(vf03).vf, c->vf_src(vf15).vf);
  // nop                        |  ftoi12.xy vf20, vf08
  c->vfs[vf20].vf.ftoi12(Mask::xy, c->vf_src(vf08).vf);
  // nop                        |  addy.x vf30, vf30, vf30
  c->vfs[vf30].vf.add(Mask::x, c->vf_src(vf30).vf, c->vf_src(vf30).vf.y());
  // rsqrt Q, vf31.z, vf29.x    |  mul.xyz vf05, vf01, Q
  c->vfs[vf05].vf.mul(Mask::xyz, c->vf_src(vf01).vf, c->Q);   c->Q = c->vf_src(vf31).vf.z() / std::sqrt(c->vf_src(vf29).vf.x());
  // move.xyzw vf06, vf02       |  mul.xyz vf29, vf03, vf03
  c->vfs[vf29].vf.mul(Mask::xyz, c->vf_src(vf03).vf, c->vf_src(vf03).vf);   c->vfs[vf06].vf.move(Mask::xyzw, c->vf_src(vf02).vf);
  // move.xyzw vf07, vf03       |  mulw.xyz vf08, vf04, vf28 :e
  c->vfs[vf08].vf.mul(Mask::xyz, c->vf_src(vf04).vf, c->vf_src(vf28).vf.w());   c->vfs[vf07].vf.move(Mask::xyzw, c->vf_src(vf03).vf);
  // nop                        |  addz.x vf30, vf30, vf30
  c->vfs[vf30].vf.add(Mask::x, c->vf_src(vf30).vf, c->vf_src(vf30).vf.z());

}

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->daddiu(sp, sp, -128);                          // daddiu sp, sp, -128
  c->sd(ra, 12432, at);                             // sd ra, 12432(at)
  c->sq(s0, 12448, at);                             // sq s0, 12448(at)
  c->sq(s1, 12464, at);                             // sq s1, 12464(at)
  c->sq(s2, 12480, at);                             // sq s2, 12480(at)
  c->sq(s3, 12496, at);                             // sq s3, 12496(at)
  c->sq(s4, 12512, at);                             // sq s4, 12512(at)
  c->sq(s5, 12528, at);                             // sq s5, 12528(at)
  c->sq(gp, 12544, at);                             // sq gp, 12544(at)
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 44, at);                                // lw v1, 44(at)
  // nop                                            // sll r0, r0, 0
  c->lw(a0, 36, at);                                // lw a0, 36(at)
  // nop                                            // sll r0, r0, 0
  c->lw(t0, 0, v1);                                 // lw t0, 0(v1)
  // nop                                            // sll r0, r0, 0
  c->lw(a2, 4, v1);                                 // lw a2, 4(v1)
  // nop                                            // sll r0, r0, 0
  c->lw(t3, 16, at);                                // lw t3, 16(at)
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 20, at);                                // lw v1, 20(at)
  // nop                                            // sll r0, r0, 0
  c->addiu(t1, r0, 255);                            // addiu t1, r0, 255
  c->addiu(a3, r0, 256);                            // addiu a3, r0, 256
  c->lui(a1, -2);                                   // lui a1, -2
  c->lui(t2, 16256);                                // lui t2, 16256
  c->ori(a1, a1, 65534);                            // ori a1, a1, 65534
  c->mtc1(f0, t2);                                  // mtc1 f0, t2
  c->daddiu(t2, a0, 3);                             // daddiu t2, a0, 3
  c->sra(t5, t2, 2);                                // sra t5, t2, 2
  c->lq(t2, 12048, at);                             // lq t2, 12048(at)
  c->sra(t4, t5, 2);                                // sra t4, t5, 2
  c->andi(t5, t5, 3);                               // andi t5, t5, 3
  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L85
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely


  block_1:
  c->daddiu(t3, t3, 64);                            // daddiu t3, t3, 64
  c->sq(t2, -64, t3);                               // sq t2, -64(t3)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, -48, t3);                               // sq t2, -48(t3)
  c->daddiu(t4, t4, -1);                            // daddiu t4, t4, -1
  c->sq(t2, -32, t3);                               // sq t2, -32(t3)
  bc = ((s64)c->sgpr64(t4)) > 0;                    // bgtz t4, L84
  c->sq(t2, -16, t3);                               // sq t2, -16(t3)
  if (bc) {goto block_1;}                           // branch non-likely


  block_2:
  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L86
  c->daddiu(t4, t5, -1);                            // daddiu t4, t5, -1
  if (bc) {goto block_6;}                           // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L86
  c->sq(t2, 0, t3);                                 // sq t2, 0(t3)
  if (bc) {goto block_6;}                           // branch non-likely

  c->daddiu(t3, t3, 16);                            // daddiu t3, t3, 16
  c->daddiu(t4, t4, -1);                            // daddiu t4, t4, -1
  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L86
  c->sq(t2, 0, t3);                                 // sq t2, 0(t3)
  if (bc) {goto block_6;}                           // branch non-likely

  c->daddiu(t3, t3, 16);                            // daddiu t3, t3, 16
  c->daddiu(t4, t4, -1);                            // daddiu t4, t4, -1
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 0, t3);                                 // sq t2, 0(t3)

  block_6:
  c->daddiu(a0, a0, -4);                            // daddiu a0, a0, -4
  c->lqc2(vf31, 12016, at);                         // lqc2 vf31, 12016(at)
  c->pextlw(a1, a1, a1);                            // pextlw a1, a1, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlw(a1, a1, a1);                            // pextlw a1, a1, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlw(a2, a2, a2);                            // pextlw a2, a2, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlw(a2, a2, a2);                            // pextlw a2, a2, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyh(t2, t1);                                 // pcpyh t2, t1
  c->ld(t1, 0, t0);                                 // ld t1, 0(t0)
  c->pcpyld(t2, t2, t2);                            // pcpyld t2, t2, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyh(a3, a3);                                 // pcpyh a3, a3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyld(a3, a3, a3);                            // pcpyld a3, a3, a3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->daddiu(t0, t0, 8);                             // daddiu t0, t0, 8
  c->sq(t2, 96, at);                                // sq t2, 96(at)
  c->pextlh(t1, r0, t1);                            // pextlh t1, r0, t1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pand(t2, t1, t2);                              // pand t2, t1, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psllw(t2, t2, 5);                              // psllw t2, t2, 5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->paddw(t5, t2, a2);                             // paddw t5, t2, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsrl32(t6, t5, 0);                             // dsrl32 t6, t5, 0
  c->lwc1(f4, 24, t5);                              // lwc1 f4, 24(t5)
  c->pcpyud(t7, t5, r0);                            // pcpyud t7, t5, r0
  c->lwc1(f3, 24, t6);                              // lwc1 f3, 24(t6)
  c->dsrl32(t8, t7, 0);                             // dsrl32 t8, t7, 0
  c->lwc1(f2, 24, t7);                              // lwc1 f2, 24(t7)
  c->pand(t1, t1, a3);                              // pand t1, t1, a3
  c->lwc1(f1, 24, t8);                              // lwc1 f1, 24(t8)
  c->psraw(t2, t1, 8);                              // psraw t2, t1, 8
  c->lq(t1, 16, t5);                                // lq t1, 16(t5)
  c->mov128_gpr_gpr(s0, t2);                        // por s0, t2, r0
  c->subs(f4, f4, f0);                              // sub.s f4, f4, f0
  c->divs(f4, f0, f4);                              // div.s f4, f0, f4
  c->lq(t2, 16, t6);                                // lq t2, 16(t6)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 16, t7);                                // lq t3, 16(t7)
  // nop                                            // sll r0, r0, 0
  c->lq(t4, 16, t8);                                // lq t4, 16(t8)
  // nop                                            // sll r0, r0, 0
  c->lq(t5, 0, t5);                                 // lq t5, 0(t5)
  // nop                                            // sll r0, r0, 0
  c->lq(t6, 0, t6);                                 // lq t6, 0(t6)
  // nop                                            // sll r0, r0, 0
  c->lq(t7, 0, t7);                                 // lq t7, 0(t7)
  // nop                                            // sll r0, r0, 0
  c->lq(t8, 0, t8);                                 // lq t8, 0(t8)
  c->muls(f4, f4, f0);                              // mul.s f4, f4, f0
  // nop                                            // sll r0, r0, 0
  c->subs(f3, f3, f0);                              // sub.s f3, f3, f0
  // nop                                            // sll r0, r0, 0
  c->divs(f3, f0, f3);                              // div.s f3, f0, f3
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(t9, f4);                                  // mfc1 t9, f4
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->muls(f3, f3, f0);                              // mul.s f3, f3, f0
  // nop                                            // sll r0, r0, 0
  c->subs(f2, f2, f0);                              // sub.s f2, f2, f0
  // nop                                            // sll r0, r0, 0
  c->divs(f2, f0, f2);                              // div.s f2, f0, f2
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(ra, f3);                                  // mfc1 ra, f3
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->muls(f2, f2, f0);                              // mul.s f2, f2, f0
  // nop                                            // sll r0, r0, 0
  c->subs(f1, f1, f0);                              // sub.s f1, f1, f0
  // nop                                            // sll r0, r0, 0
  c->divs(f1, f0, f1);                              // div.s f1, f0, f1
  // nop                                            // sll r0, r0, 0
  c->pextlw(t9, ra, t9);                            // pextlw t9, ra, t9
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(ra, f2);                                  // mfc1 ra, f2
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(gp, f1);                                  // mfc1 gp, f1
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->pextlw(ra, gp, ra);                            // pextlw ra, gp, ra
  // nop                                            // sll r0, r0, 0
  c->pcpyld(t9, ra, t9);                            // pcpyld t9, ra, t9
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf21, t1);                       // qmtc2.ni vf21, t1
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf22, t2);                       // qmtc2.ni vf22, t2
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf23, t3);                       // qmtc2.ni vf23, t3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf24, t4);                       // qmtc2.ni vf24, t4
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf9, t5);                        // qmtc2.ni vf9, t5
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf10, t6);                       // qmtc2.ni vf10, t6
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf11, t7);                       // qmtc2.ni vf11, t7
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf12, t8);                       // qmtc2.ni vf12, t8
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf27, t9);                       // qmtc2.ni vf27, t9
  c->lq(t2, 96, at);                                // lq t2, 96(at)
  // Unknown instr: vcallms 48
  vcallms48(c);
  // nop                                            // sll r0, r0, 0
  c->ld(t1, 0, t0);                                 // ld t1, 0(t0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(t0, t0, 8);                             // daddiu t0, t0, 8
  c->pextlh(t1, r0, t1);                            // pextlh t1, r0, t1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pand(t2, t1, t2);                              // pand t2, t1, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psllw(t2, t2, 5);                              // psllw t2, t2, 5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->paddw(t5, t2, a2);                             // paddw t5, t2, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsrl32(t6, t5, 0);                             // dsrl32 t6, t5, 0
  c->lwc1(f3, 24, t5);                              // lwc1 f3, 24(t5)
  c->pcpyud(t7, t5, r0);                            // pcpyud t7, t5, r0
  c->lwc1(f2, 24, t6);                              // lwc1 f2, 24(t6)
  c->dsrl32(t8, t7, 0);                             // dsrl32 t8, t7, 0
  c->lwc1(f1, 24, t7);                              // lwc1 f1, 24(t7)
  c->pand(t1, t1, a3);                              // pand t1, t1, a3
  c->lwc1(f4, 24, t8);                              // lwc1 f4, 24(t8)
  c->psraw(t2, t1, 8);                              // psraw t2, t1, 8
  c->lq(t1, 16, t5);                                // lq t1, 16(t5)
  c->mov128_gpr_gpr(s1, t2);                        // por s1, t2, r0
  c->subs(f5, f3, f0);                              // sub.s f5, f3, f0
  c->subs(f3, f2, f0);                              // sub.s f3, f2, f0
  // nop                                            // sll r0, r0, 0
  c->subs(f2, f1, f0);                              // sub.s f2, f1, f0
  // nop                                            // sll r0, r0, 0
  c->subs(f1, f4, f0);                              // sub.s f1, f4, f0
  // nop                                            // sll r0, r0, 0
  c->divs(f4, f0, f5);                              // div.s f4, f0, f5
  c->lq(t2, 16, t6);                                // lq t2, 16(t6)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 16, t7);                                // lq t3, 16(t7)
  // nop                                            // sll r0, r0, 0
  c->lq(t4, 16, t8);                                // lq t4, 16(t8)
  // nop                                            // sll r0, r0, 0
  c->lq(t5, 0, t5);                                 // lq t5, 0(t5)
  // nop                                            // sll r0, r0, 0
  c->lq(t6, 0, t6);                                 // lq t6, 0(t6)
  // nop                                            // sll r0, r0, 0
  c->lq(t7, 0, t7);                                 // lq t7, 0(t7)
  // nop                                            // sll r0, r0, 0
  c->lq(t8, 0, t8);                                 // lq t8, 0(t8)
  c->muls(f4, f4, f0);                              // mul.s f4, f4, f0
  // nop                                            // sll r0, r0, 0
  c->divs(f3, f0, f3);                              // div.s f3, f0, f3
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(t9, f4);                                  // mfc1 t9, f4
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->muls(f3, f3, f0);                              // mul.s f3, f3, f0
  // nop                                            // sll r0, r0, 0
  c->divs(f2, f0, f2);                              // div.s f2, f0, f2
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(ra, f3);                                  // mfc1 ra, f3
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->muls(f2, f2, f0);                              // mul.s f2, f2, f0
  // nop                                            // sll r0, r0, 0
  c->divs(f1, f0, f1);                              // div.s f1, f0, f1
  // nop                                            // sll r0, r0, 0
  c->pextlw(t9, ra, t9);                            // pextlw t9, ra, t9
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(ra, f2);                                  // mfc1 ra, f2
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(gp, f1);                                  // mfc1 gp, f1
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf21, t1);                       // qmtc2.ni vf21, t1
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf9, t5);                        // qmtc2.ni vf9, t5
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf10, t6);                       // qmtc2.ni vf10, t6
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf11, t7);                       // qmtc2.ni vf11, t7
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf12, t8);                       // qmtc2.ni vf12, t8
  c->pextlw(t1, gp, ra);                            // pextlw t1, gp, ra
  // nop                                            // sll r0, r0, 0
  c->pcpyld(t1, t1, t9);                            // pcpyld t1, t1, t9
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf22, t2);                       // qmtc2.ni vf22, t2
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf23, t3);                       // qmtc2.ni vf23, t3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf24, t4);                       // qmtc2.ni vf24, t4
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf27, t1);                       // qmtc2.ni vf27, t1
  c->lq(t2, 96, at);                                // lq t2, 96(at)
  // Unknown instr: vcallms 48
  vcallms48(c);
  // nop                                            // sll r0, r0, 0
  c->ld(t1, 0, t0);                                 // ld t1, 0(t0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(t0, t0, 8);                             // daddiu t0, t0, 8
  c->pextlh(t1, r0, t1);                            // pextlh t1, r0, t1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pand(t2, t1, t2);                              // pand t2, t1, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psllw(t2, t2, 5);                              // psllw t2, t2, 5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->paddw(t2, t2, a2);                             // paddw t2, t2, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsrl32(t3, t2, 0);                             // dsrl32 t3, t2, 0
  c->lwc1(f3, 24, t2);                              // lwc1 f3, 24(t2)
  c->pcpyud(t7, t2, r0);                            // pcpyud t7, t2, r0
  c->lwc1(f2, 24, t3);                              // lwc1 f2, 24(t3)
  c->dsrl32(t8, t7, 0);                             // dsrl32 t8, t7, 0
  c->lwc1(f1, 24, t7);                              // lwc1 f1, 24(t7)
  c->pand(t1, t1, a3);                              // pand t1, t1, a3
  c->lwc1(f4, 24, t8);                              // lwc1 f4, 24(t8)
  c->psraw(t4, t1, 8);                              // psraw t4, t1, 8
  c->lq(t1, 16, t2);                                // lq t1, 16(t2)
  c->mov128_gpr_gpr(s2, t4);                        // por s2, t4, r0
  c->subs(f5, f3, f0);                              // sub.s f5, f3, f0
  c->subs(f3, f2, f0);                              // sub.s f3, f2, f0
  // nop                                            // sll r0, r0, 0
  c->subs(f2, f1, f0);                              // sub.s f2, f1, f0
  // nop                                            // sll r0, r0, 0
  c->subs(f1, f4, f0);                              // sub.s f1, f4, f0
  // nop                                            // sll r0, r0, 0
  c->divs(f4, f0, f5);                              // div.s f4, f0, f5
  c->lq(t4, 16, t3);                                // lq t4, 16(t3)
  // nop                                            // sll r0, r0, 0
  c->lq(t5, 16, t7);                                // lq t5, 16(t7)
  // nop                                            // sll r0, r0, 0
  c->lq(t6, 16, t8);                                // lq t6, 16(t8)
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 0, t2);                                 // lq t2, 0(t2)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 0, t3);                                 // lq t3, 0(t3)
  // nop                                            // sll r0, r0, 0
  c->lq(t7, 0, t7);                                 // lq t7, 0(t7)
  // nop                                            // sll r0, r0, 0
  c->lq(t8, 0, t8);                                 // lq t8, 0(t8)
  c->muls(f4, f4, f0);                              // mul.s f4, f4, f0
  // nop                                            // sll r0, r0, 0
  c->divs(f3, f0, f3);                              // div.s f3, f0, f3
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(t9, f4);                                  // mfc1 t9, f4
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->muls(f3, f3, f0);                              // mul.s f3, f3, f0
  // nop                                            // sll r0, r0, 0
  c->divs(f2, f0, f2);                              // div.s f2, f0, f2
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(ra, f3);                                  // mfc1 ra, f3
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->muls(f2, f2, f0);                              // mul.s f2, f2, f0
  // nop                                            // sll r0, r0, 0
  c->divs(f1, f0, f1);                              // div.s f1, f0, f1
  // nop                                            // sll r0, r0, 0
  c->pextlw(t9, ra, t9);                            // pextlw t9, ra, t9
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->mfc1(ra, f2);                                  // mfc1 ra, f2
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf21, t1);                       // qmtc2.ni vf21, t1
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf22, t4);                       // qmtc2.ni vf22, t4
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf23, t5);                       // qmtc2.ni vf23, t5
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf24, t6);                       // qmtc2.ni vf24, t6
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(t1, f1);                                  // mfc1 t1, f1
  c->pextlw(t1, t1, ra);                            // pextlw t1, t1, ra
  // nop                                            // sll r0, r0, 0
  c->pcpyld(t1, t1, t9);                            // pcpyld t1, t1, t9
  c->mov128_vf_gpr(vf9, t2);                        // qmtc2.ni vf9, t2
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf10, t3);                       // qmtc2.ni vf10, t3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf11, t7);                       // qmtc2.ni vf11, t7
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf12, t8);                       // qmtc2.ni vf12, t8
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf27, t1);                       // qmtc2.ni vf27, t1
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t4, vf17);                       // qmfc2.ni t4, vf17
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t5, vf18);                       // qmfc2.ni t5, vf18
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t6, vf19);                       // qmfc2.ni t6, vf19
  bc = ((s64)c->sgpr64(a0)) <= 0;                   // blez a0, L88
  c->mov128_gpr_vf(t7, vf20);                       // qmfc2.ni t7, vf20
  if (bc) {goto block_8;}                           // branch non-likely


  block_7:
  c->lq(t2, 96, at);                                // lq t2, 96(at)
  // Unknown instr: vcallms 48
  vcallms48(c);
  c->daddiu(a0, a0, -4);                            // daddiu a0, a0, -4
  c->ld(t1, 0, t0);                                 // ld t1, 0(t0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(v1, v1, 16);                            // daddiu v1, v1, 16
  c->daddiu(t0, t0, 8);                             // daddiu t0, t0, 8
  c->pextlh(t1, r0, t1);                            // pextlh t1, r0, t1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pand(t2, t1, t2);                              // pand t2, t1, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psllw(t2, t2, 5);                              // psllw t2, t2, 5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->paddw(t9, t2, a2);                             // paddw t9, t2, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsrl32(ra, t9, 0);                             // dsrl32 ra, t9, 0
  c->lwc1(f1, 24, t9);                              // lwc1 f1, 24(t9)
  c->pcpyud(s4, t9, r0);                            // pcpyud s4, t9, r0
  c->lwc1(f4, 24, ra);                              // lwc1 f4, 24(ra)
  c->subs(f3, f1, f0);                              // sub.s f3, f1, f0
  // nop                                            // sll r0, r0, 0
  c->dsrl32(s3, s4, 0);                             // dsrl32 s3, s4, 0
  c->lwc1(f1, 24, s4);                              // lwc1 f1, 24(s4)
  c->pand(t1, t1, a3);                              // pand t1, t1, a3
  c->lwc1(f2, 24, s3);                              // lwc1 f2, 24(s3)
  c->psraw(s5, t1, 8);                              // psraw s5, t1, 8
  c->lq(t1, 16, t9);                                // lq t1, 16(t9)
  c->divs(f3, f0, f3);                              // div.s f3, f0, f3
  c->lq(t2, 16, ra);                                // lq t2, 16(ra)
  c->mov128_gpr_gpr(gp, s0);                        // por gp, s0, r0
  c->subs(f4, f4, f0);                              // sub.s f4, f4, f0
  c->ppach(t4, r0, t4);                             // ppach t4, r0, t4
  c->lq(t3, 16, s4);                                // lq t3, 16(s4)
  c->ppach(t5, r0, t5);                             // ppach t5, r0, t5
  c->lq(t8, 16, s3);                                // lq t8, 16(s3)
  c->ppach(t6, r0, t6);                             // ppach t6, r0, t6
  c->lq(t9, 0, t9);                                 // lq t9, 0(t9)
  c->ppach(t7, r0, t7);                             // ppach t7, r0, t7
  c->lq(ra, 0, ra);                                 // lq ra, 0(ra)
  c->pextlw(t4, t5, t4);                            // pextlw t4, t5, t4
  c->lq(t5, 0, s4);                                 // lq t5, 0(s4)
  c->pextlw(t6, t7, t6);                            // pextlw t6, t7, t6
  c->lq(t7, 0, s3);                                 // lq t7, 0(s3)
  c->mov128_gpr_gpr(s0, s1);                        // por s0, s1, r0
  c->muls(f5, f3, f0);                              // mul.s f5, f3, f0
  c->mov128_gpr_gpr(s1, s2);                        // por s1, s2, r0
  c->divs(f3, f0, f4);                              // div.s f3, f0, f4
  c->pcpyld(t4, t6, t4);                            // pcpyld t4, t6, t4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pand(t4, t4, a1);                              // pand t4, t4, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->mov128_gpr_gpr(s2, s5);                        // por s2, s5, r0
  c->mfc1(t6, f5);                                  // mfc1 t6, f5
  c->subs(f4, f1, f0);                              // sub.s f4, f1, f0
  // nop                                            // sll r0, r0, 0
  c->por(t4, t4, gp);                               // por t4, t4, gp
  // nop                                            // sll r0, r0, 0
  c->subs(f1, f2, f0);                              // sub.s f1, f2, f0
  // nop                                            // sll r0, r0, 0
  c->muls(f2, f3, f0);                              // mul.s f2, f3, f0
  c->sq(t4, -16, v1);                               // sq t4, -16(v1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->divs(f3, f0, f4);                              // div.s f3, f0, f4
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(t4, f2);                                  // mfc1 t4, f2
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->muls(f2, f3, f0);                              // mul.s f2, f3, f0
  // nop                                            // sll r0, r0, 0
  c->pextlw(t4, t4, t6);                            // pextlw t4, t4, t6
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->divs(f1, f0, f1);                              // div.s f1, f0, f1
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(t6, f2);                                  // mfc1 t6, f2
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf21, t1);                       // qmtc2.ni vf21, t1
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf22, t2);                       // qmtc2.ni vf22, t2
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf23, t3);                       // qmtc2.ni vf23, t3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf24, t8);                       // qmtc2.ni vf24, t8
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(t1, f1);                                  // mfc1 t1, f1
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf9, t9);                        // qmtc2.ni vf9, t9
  c->pextlw(t1, t1, t6);                            // pextlw t1, t1, t6
  c->mov128_vf_gpr(vf10, ra);                       // qmtc2.ni vf10, ra
  c->pcpyld(t1, t1, t4);                            // pcpyld t1, t1, t4
  c->mov128_vf_gpr(vf11, t5);                       // qmtc2.ni vf11, t5
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf12, t7);                       // qmtc2.ni vf12, t7
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf27, t1);                       // qmtc2.ni vf27, t1
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t4, vf17);                       // qmfc2.ni t4, vf17
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t5, vf18);                       // qmfc2.ni t5, vf18
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t6, vf19);                       // qmfc2.ni t6, vf19
  bc = ((s64)c->sgpr64(a0)) > 0;                    // bgtz a0, L87
  c->mov128_gpr_vf(t7, vf20);                       // qmfc2.ni t7, vf20
  if (bc) {goto block_7;}                           // branch non-likely


  block_8:
  c->daddiu(v1, v1, 16);                            // daddiu v1, v1, 16
  // nop                                            // sll r0, r0, 0
  c->ppach(t4, r0, t4);                             // ppach t4, r0, t4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppach(t5, r0, t5);                             // ppach t5, r0, t5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppach(t6, r0, t6);                             // ppach t6, r0, t6
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppach(t7, r0, t7);                             // ppach t7, r0, t7
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlw(t4, t5, t4);                            // pextlw t4, t5, t4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlw(t6, t7, t6);                            // pextlw t6, t7, t6
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyld(t4, t6, t4);                            // pcpyld t4, t6, t4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pand(t4, t4, a1);                              // pand t4, t4, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->por(t4, t4, s0);                               // por t4, t4, s0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->sq(t4, -16, v1);                               // sq t4, -16(v1)
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
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c(-1, 0, "*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("generic-envmap-proc", execute, 256);
}

} // namespace generic_envmap_proc
} // namespace Mips2C
// add generic_envmap_proc::link to the link callback table for the object file.
// FWD DEC:

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace generic_prepare_dma_double {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->daddiu(sp, sp, -128);                          // daddiu sp, sp, -128
  c->sd(ra, 12432, at);                             // sd ra, 12432(at)
  c->sq(s0, 12448, at);                             // sq s0, 12448(at)
  c->sq(s1, 12464, at);                             // sq s1, 12464(at)
  c->sq(s2, 12480, at);                             // sq s2, 12480(at)
  c->sq(s3, 12496, at);                             // sq s3, 12496(at)
  c->sq(s4, 12512, at);                             // sq s4, 12512(at)
  c->sq(s5, 12528, at);                             // sq s5, 12528(at)
  c->sq(gp, 12544, at);                             // sq gp, 12544(at)
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lw(a3, 44, at);                                // lw a3, 44(at)
  // nop                                            // sll r0, r0, 0
  c->lbu(v1, 16, a3);                               // lbu v1, 16(a3)
  // nop                                            // sll r0, r0, 0
  c->lh(a0, 18, a3);                                // lh a0, 18(a3)
  // nop                                            // sll r0, r0, 0
  c->lw(t7, 24, at);                                // lw t7, 24(at)
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
  c->dsll(t4, t0, 4);                               // dsll t4, t0, 4
  c->dsra(t9, a2, 4);                               // dsra t9, a2, 4
  c->dsll(t1, t9, 4);                               // dsll t1, t9, 4
  c->mov64(a2, t7);                                 // or a2, t7, r0
  c->dsra(t8, a1, 4);                               // dsra t8, a1, 4
  c->lq(t0, 11744, at);                             // lq t0, 11744(at)
  c->daddu(t2, a2, a1);                             // daddu t2, a2, a1
  c->lq(t5, 11760, at);                             // lq t5, 11760(at)
  c->daddu(t1, t2, t1);                             // daddu t1, t2, t1
  c->sq(t0, 0, a2);                                 // sq t0, 0(a2)
  c->daddiu(t0, t2, 32);                            // daddiu t0, t2, 32
  c->lq(ra, 11776, at);                             // lq ra, 11776(at)
  c->daddu(t2, t1, t4);                             // daddu t2, t1, t4
  c->sq(r0, -16, t0);                               // sq r0, -16(t0)
  c->daddiu(t1, t1, 64);                            // daddiu t1, t1, 64
  c->lq(gp, 11792, at);                             // lq gp, 11792(at)
  c->daddu(t3, t2, t4);                             // daddu t3, t2, t4
  c->sq(r0, -32, t1);                               // sq r0, -32(t1)
  c->daddiu(t2, t2, 80);                            // daddiu t2, t2, 80
  c->sq(r0, -16, t1);                               // sq r0, -16(t1)
  c->daddu(s5, t3, a1);                             // daddu s5, t3, a1
  c->sq(r0, -32, t2);                               // sq r0, -32(t2)
  c->daddiu(a1, t3, 80);                            // daddiu a1, t3, 80
  c->sq(r0, -16, t2);                               // sq r0, -16(t2)
  c->daddu(s4, s5, t4);                             // daddu s4, s5, t4
  c->sq(r0, -16, a1);                               // sq r0, -16(a1)
  c->daddiu(t3, s5, 112);                           // daddiu t3, s5, 112
  c->lw(s5, 11984, at);                             // lw s5, 11984(at)
  c->daddu(s3, s4, t4);                             // daddu s3, s4, t4
  c->sq(r0, -16, t3);                               // sq r0, -16(t3)
  c->daddiu(t4, s4, 128);                           // daddiu t4, s4, 128
  c->sq(t5, 0, a1);                                 // sq t5, 0(a1)
  c->daddiu(t5, s3, 128);                           // daddiu t5, s3, 128
  c->sq(r0, -32, t4);                               // sq r0, -32(t4)
  c->subu(s4, t5, t7);                              // subu s4, t5, t7
  c->sq(r0, -16, t4);                               // sq r0, -16(t4)
  c->sra(s4, s4, 4);                                // sra s4, s4, 4
  c->sq(r0, -16, t5);                               // sq r0, -16(t5)
  c->daddiu(s4, s4, -1);                            // daddiu s4, s4, -1
  c->sq(ra, 0, t5);                                 // sq ra, 0(t5)
  c->sh(t9, 0, t5);                                 // sh t9, 0(t5)
  c->sq(gp, 16, t5);                                // sq gp, 16(t5)
  c->sw(s5, 24, t5);                                // sw s5, 24(t5)
  c->sh(s4, 0, t7);                                 // sh s4, 0(t7)
  c->daddiu(t7, s4, 3);                             // daddiu t7, s4, 3
  c->sw(t7, 40, at);                                // sw t7, 40(at)
  c->lw(t7, 60, at);                                // lw t7, 60(at)
  c->dsubu(t9, t0, a2);                             // dsubu t9, t0, a2
  c->daddu(t7, t7, t9);                             // daddu t7, t7, t9
  c->lw(t9, 12, a2);                                // lw t9, 12(a2)
  c->sll(ra, t8, 16);                               // sll ra, t8, 16
  c->lw(t8, 68, at);                                // lw t8, 68(at)
  // nop                                            // sll r0, r0, 0
  c->lw(gp, 12, a1);                                // lw gp, 12(a1)
  c->or_(s4, t9, t8);                               // or s4, t9, t8
  c->lw(t9, 72, at);                                // lw t9, 72(at)
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
  bc = c->sgpr64(t9) != c->sgpr64(s4);              // bne t9, s4, L74
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
  bc = c->sgpr64(t9) != c->sgpr64(t6);              // bne t9, t6, L75
  c->daddiu(t5, t9, 279);                           // daddiu t5, t9, 279
  if (bc) {goto block_4;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->addiu(t5, r0, 9);                              // addiu t5, r0, 9

  block_4:
  // nop                                            // sll r0, r0, 0
  c->sw(t5, 72, at);                                // sw t5, 72(at)
  // nop                                            // sll r0, r0, 0
  c->sw(t0, 4, at);                                 // sw t0, 4(at)
  // nop                                            // sll r0, r0, 0
  c->sw(t1, 8, at);                                 // sw t1, 8(at)
  // nop                                            // sll r0, r0, 0
  c->sw(t2, 12, at);                                // sw t2, 12(at)
  // nop                                            // sll r0, r0, 0
  c->sw(t3, 16, at);                                // sw t3, 16(at)
  // nop                                            // sll r0, r0, 0
  c->sw(t4, 20, at);                                // sw t4, 20(at)
  // nop                                            // sll r0, r0, 0
  c->lw(t0, 48, at);                                // lw t0, 48(at)
  // nop                                            // sll r0, r0, 0
  c->lq(t1, 11808, at);                             // lq t1, 11808(at)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L80
  c->lq(t2, 11824, at);                             // lq t2, 11824(at)
  if (bc) {goto block_13;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lq(t3, 11840, at);                             // lq t3, 11840(at)
  // nop                                            // sll r0, r0, 0
  c->lq(t4, 11856, at);                             // lq t4, 11856(at)
  // nop                                            // sll r0, r0, 0
  c->lh(t5, 58, at);                                // lh t5, 58(at)
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
  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L76
  c->lq(t1, 11872, at);                             // lq t1, 11872(at)
  if (bc) {goto block_7;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lq(t2, 11904, at);                             // lq t2, 11904(at)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 11936, at);                             // lq t3, 11936(at)
  //beq r0, r0, L77                                 // beq r0, r0, L77
  c->lq(t4, 12032, at);                             // lq t4, 12032(at)
  goto block_8;                                     // branch always


  block_7:
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 11888, at);                             // lq t2, 11888(at)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 11936, at);                             // lq t3, 11936(at)
  // nop                                            // sll r0, r0, 0
  c->lq(t4, 12032, at);                             // lq t4, 12032(at)

  block_8:
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

  block_9:
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
  bc = ((s64)c->sgpr64(t4)) > 0;                    // bgtz t4, L78
  c->sq(ra, 64, t1);                                // sq ra, 64(t1)
  if (bc) {goto block_9;}                           // branch non-likely

  c->ori(t0, t5, 32768);                            // ori t0, t5, 32768
  c->sw(a0, 36, at);                                // sw a0, 36(at)
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
  c->lw(t6, 52, at);                                // lw t6, 52(at)
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

  block_11:
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
  bc = ((s64)c->sgpr64(t1)) > 0;                    // bgtz t1, L79
  c->sq(t6, 64, a2);                                // sq t6, 64(a2)
  if (bc) {goto block_11;}                          // branch non-likely

  c->ori(a3, t7, 32768);                            // ori a3, t7, 32768
  c->sw(a0, 36, at);                                // sw a0, 36(at)
  // nop                                            // sll r0, r0, 0
  c->sw(a3, 28, a2);                                // sw a3, 28(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 92, a1);                                // sw v1, 92(a1)
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 108, a1);                               // sw a0, 108(a1)
  //beq r0, r0, L82                                 // beq r0, r0, L82
  c->sw(r0, 124, a1);                               // sw r0, 124(a1)
  goto block_16;                                    // branch always


  block_13:
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
  c->lw(t5, 52, at);                                // lw t5, 52(at)
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

  block_14:
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
  bc = ((s64)c->sgpr64(a3)) > 0;                    // bgtz a3, L81
  c->sq(t5, 64, t0);                                // sq t5, 64(t0)
  if (bc) {goto block_14;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->sw(a0, 36, at);                                // sw a0, 36(at)
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 92, a1);                                // sw v1, 92(a1)
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 108, a1);                               // sw a0, 108(a1)
  // nop                                            // sll r0, r0, 0
  c->sw(r0, 124, a1);                               // sw r0, 124(a1)

  block_16:
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
  cache.fake_scratchpad_data = intern_from_c(-1, 0, "*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("generic-prepare-dma-double", execute, 256);
}

} // namespace generic_prepare_dma_double
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace generic_prepare_dma_single {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;

u64 execute(void*) {
  ASSERT(false);
  return 0;
}

u64 execute_real(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->daddiu(sp, sp, -32);                           // daddiu sp, sp, -32
  // get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c); // another guess
  c->sq(gp, 12448, at);                             // sq gp, 12448(at)
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  // nop                                            // sll r0, r0, 0
  c->lw(t1, 44, at);                                // lw t1, 44(at)
  // nop                                            // sll r0, r0, 0
  c->lw(t8, 48, at);                                // lw t8, 48(at)
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 24, at);                                // lw v1, 24(at)
  // nop                                            // sll r0, r0, 0
  c->lh(t3, 56, at);                                // lh t3, 56(at)
  // nop                                            // sll r0, r0, 0
  c->lh(a1, 18, t1);                                // lh a1, 18(t1)
  c->mov64(a0, v1);                                 // or a0, v1, r0
  c->lbu(a2, 16, t1);                               // lbu a2, 16(t1)
  bc = c->sgpr64(t8) == 0;                          // beq t8, r0, L70
  c->lq(t4, 11744, at);                             // lq t4, 11744(at)
  if (bc) {goto block_10;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lq(t9, 11808, at);                             // lq t9, 11808(at)
  // nop                                            // sll r0, r0, 0
  c->lq(gp, 11824, at);                             // lq gp, 11824(at)
  c->mov64(a3, a2);                                 // or a3, a2, r0
  c->lq(t7, 11840, at);                             // lq t7, 11840(at)
  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L64
  c->lq(t2, 11856, at);                             // lq t2, 11856(at)
  if (bc) {goto block_3;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lq(t0, 11872, at);                             // lq t0, 11872(at)
  // nop                                            // sll r0, r0, 0
  c->lq(t5, 11888, at);                             // lq t5, 11888(at)
  //beq r0, r0, L65                                 // beq r0, r0, L65
  c->lq(t6, 11936, at);                             // lq t6, 11936(at)
  goto block_4;                                     // branch always


  block_3:
  // nop                                            // sll r0, r0, 0
  c->lq(t0, 12032, at);                             // lq t0, 12032(at)
  // nop                                            // sll r0, r0, 0
  c->lq(t5, 11936, at);                             // lq t5, 11936(at)
  // nop                                            // sll r0, r0, 0
  c->lq(t6, 11936, at);                             // lq t6, 11936(at)

  block_4:
  // nop                                            // sll r0, r0, 0
  c->sq(t4, 0, a0);                                 // sq t4, 0(a0)
  // nop                                            // sll r0, r0, 0
  c->sq(t9, 16, a0);                                // sq t9, 16(a0)
  // nop                                            // sll r0, r0, 0
  c->sq(gp, 32, a0);                                // sq gp, 32(a0)
  c->mov64(t4, t8);                                 // or t4, t8, r0
  c->sq(t7, 48, a0);                                // sq t7, 48(a0)
  c->daddiu(t1, t1, 22);                            // daddiu t1, t1, 22
  c->sq(t2, 64, a0);                                // sq t2, 64(a0)
  c->addiu(t2, r0, 0);                              // addiu t2, r0, 0
  c->sq(t0, 80, a0);                                // sq t0, 80(a0)
  c->addiu(t0, r0, 128);                            // addiu t0, r0, 128
  c->sq(t5, 96, a0);                                // sq t5, 96(a0)
  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L67
  c->sq(t6, 112, a0);                               // sq t6, 112(a0)
  if (bc) {goto block_7;}                           // branch non-likely


  block_5:
  c->daddu(a0, a0, t0);                             // daddu a0, a0, t0
  c->lq(t0, 0, t4);                                 // lq t0, 0(t4)
  c->daddiu(a3, a3, -1);                            // daddiu a3, a3, -1
  c->lbu(t3, 0, t1);                                // lbu t3, 0(t1)
  // nop                                            // sll r0, r0, 0
  c->lq(t5, 16, t4);                                // lq t5, 16(t4)
  c->daddu(t7, t3, t3);                             // daddu t7, t3, t3
  c->lq(t6, 32, t4);                                // lq t6, 32(t4)
  c->daddu(t8, t7, t3);                             // daddu t8, t7, t3
  c->lq(t7, 48, t4);                                // lq t7, 48(t4)
  c->daddiu(t9, t8, 9);                             // daddiu t9, t8, 9
  c->lq(t8, 64, t4);                                // lq t8, 64(t4)
  c->daddiu(t4, t4, 80);                            // daddiu t4, t4, 80
  c->sq(t0, 0, a0);                                 // sq t0, 0(a0)
  // nop                                            // sll r0, r0, 0
  c->sw(t2, 12, a0);                                // sw t2, 12(a0)
  c->daddiu(t1, t1, 1);                             // daddiu t1, t1, 1
  c->sq(t5, 16, a0);                                // sq t5, 16(a0)
  c->daddu(t2, t2, t9);                             // daddu t2, t2, t9
  c->sw(t3, 28, a0);                                // sw t3, 28(a0)
  // nop                                            // sll r0, r0, 0
  c->sq(t6, 32, a0);                                // sq t6, 32(a0)
  c->addiu(t0, r0, 80);                             // addiu t0, r0, 80
  c->sq(t7, 48, a0);                                // sq t7, 48(a0)
  bc = ((s64)c->sgpr64(a3)) > 0;                    // bgtz a3, L66
  c->sq(t8, 64, a0);                                // sq t8, 64(a0)
  if (bc) {goto block_5;}                           // branch non-likely

  //beq r0, r0, L69                                 // beq r0, r0, L69
  // nop                                            // sll r0, r0, 0
  goto block_9;                                     // branch always


  block_7:
  // nop                                            // sll r0, r0, 0
  c->lw(t3, 52, at);                                // lw t3, 52(at)
  // nop                                            // sll r0, r0, 0
  c->lq(t4, 0, t3);                                 // lq t4, 0(t3)
  // nop                                            // sll r0, r0, 0
  c->lq(t5, 16, t3);                                // lq t5, 16(t3)
  // nop                                            // sll r0, r0, 0
  c->lq(t6, 32, t3);                                // lq t6, 32(t3)
  // nop                                            // sll r0, r0, 0
  c->lq(t7, 48, t3);                                // lq t7, 48(t3)
  // nop                                            // sll r0, r0, 0
  c->lq(t8, 64, t3);                                // lq t8, 64(t3)

  block_8:
  c->daddu(a0, a0, t0);                             // daddu a0, a0, t0
  c->lbu(t3, 0, t1);                                // lbu t3, 0(t1)
  c->daddiu(a3, a3, -1);                            // daddiu a3, a3, -1
  c->sq(t4, 0, a0);                                 // sq t4, 0(a0)
  c->daddu(t0, t3, t3);                             // daddu t0, t3, t3
  c->sw(t2, 12, a0);                                // sw t2, 12(a0)
  c->daddu(t0, t0, t3);                             // daddu t0, t0, t3
  c->sq(t5, 16, a0);                                // sq t5, 16(a0)
  c->daddiu(t0, t0, 9);                             // daddiu t0, t0, 9
  c->sw(t3, 28, a0);                                // sw t3, 28(a0)
  c->daddiu(t1, t1, 1);                             // daddiu t1, t1, 1
  // nop                                            // sll r0, r0, 0
  c->daddu(t2, t2, t0);                             // daddu t2, t2, t0
  c->sq(t6, 32, a0);                                // sq t6, 32(a0)
  c->addiu(t0, r0, 80);                             // addiu t0, r0, 80
  c->sq(t7, 48, a0);                                // sq t7, 48(a0)
  bc = ((s64)c->sgpr64(a3)) > 0;                    // bgtz a3, L68
  c->sq(t8, 64, a0);                                // sq t8, 64(a0)
  if (bc) {goto block_8;}                           // branch non-likely


  block_9:
  c->ori(a3, t3, 32768);                            // ori a3, t3, 32768
  c->sw(a1, 36, at);                                // sw a1, 36(at)
  // nop                                            // sll r0, r0, 0
  c->sw(a3, 28, a0);                                // sw a3, 28(a0)
  // nop                                            // sll r0, r0, 0
  c->sw(a2, 92, a0);                                // sw a2, 92(a0)
  // nop                                            // sll r0, r0, 0
  c->sw(a1, 108, v1);                               // sw a1, 108(v1)
  //beq r0, r0, L71                                 // beq r0, r0, L71
  c->sw(r0, 124, v1);                               // sw r0, 124(v1)
  goto block_11;                                    // branch always


  block_10:
  c->dsll(a3, a2, 2);                               // dsll a3, a2, 2
  c->sq(t4, 0, a0);                                 // sq t4, 0(a0)
  c->daddu(a3, a3, a2);                             // daddu a3, a3, a2
  c->sw(a1, 108, v1);                               // sw a1, 108(v1)
  c->dsll(a3, a3, 4);                               // dsll a3, a3, 4
  // nop                                            // sll r0, r0, 0
  c->daddiu(t0, a3, 128);                           // daddiu t0, a3, 128
  c->sw(a1, 36, at);                                // sw a1, 36(at)

  block_11:
  c->dsll(t1, a2, 2);                               // dsll t1, a2, 2
  c->lw(a3, 12, v1);                                // lw a3, 12(v1)
  c->daddu(a2, t1, a2);                             // daddu a2, t1, a2
  c->lw(t1, 68, at);                                // lw t1, 68(at)
  c->daddiu(a2, a2, 7);                             // daddiu a2, a2, 7
  // nop                                            // sll r0, r0, 0
  c->or_(t2, a3, t1);                               // or t2, a3, t1
  // nop                                            // sll r0, r0, 0
  c->sll(t3, a2, 16);                               // sll t3, a2, 16
  c->xori(a3, t1, 38);                              // xori a3, t1, 38
  c->or_(t1, t2, t3);                               // or t1, t2, t3
  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
  // nop                                            // sll r0, r0, 0
  c->sw(t1, 12, v1);                                // sw t1, 12(v1)
  // nop                                            // sll r0, r0, 0
  c->sw(a3, 68, at);                                // sw a3, 68(at)
  c->daddiu(a1, a1, 3);                             // daddiu a1, a1, 3
  c->daddu(a0, a0, t0);                             // daddu a0, a0, t0
  c->dsra(a1, a1, 2);                               // dsra a1, a1, 2
  c->daddiu(a2, a0, 32);                            // daddiu a2, a0, 32
  c->dsll(t0, a1, 2);                               // dsll t0, a1, 2
  // nop                                            // sll r0, r0, 0
  c->daddu(a3, t0, t0);                             // daddu a3, t0, t0
  c->dsll(a1, t0, 2);                               // dsll a1, t0, 2
  c->daddu(a3, a3, t0);                             // daddu a3, a3, t0
  c->daddiu(a1, a1, 15);                            // daddiu a1, a1, 15
  c->dsll(a3, a3, 2);                               // dsll a3, a3, 2
  c->dsra(a1, a1, 4);                               // dsra a1, a1, 4
  c->daddiu(a3, a3, 15);                            // daddiu a3, a3, 15
  c->dsll(t1, a1, 4);                               // dsll t1, a1, 4
  c->dsra(a3, a3, 4);                               // dsra a3, a3, 4
  c->lw(a1, 72, at);                                // lw a1, 72(at)
  c->dsll(a3, a3, 4);                               // dsll a3, a3, 4
  // nop                                            // sll r0, r0, 0
  c->daddu(a3, a2, a3);                             // daddu a3, a2, a3
  c->lw(t2, 11968, at);                             // lw t2, 11968(at)
  c->daddu(a2, a3, t1);                             // daddu a2, a3, t1
  c->sq(r0, 0, a3);                                 // sq r0, 0(a3)
  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->sq(r0, -16, a3);                               // sq r0, -16(a3)
  c->daddu(t1, a2, t1);                             // daddu t1, a2, t1
  c->sq(r0, -32, a3);                               // sq r0, -32(a3)
  c->daddiu(t1, t1, 16);                            // daddiu t1, t1, 16
  c->sq(r0, 0, a2);                                 // sq r0, 0(a2)
  c->subu(t3, t1, v1);                              // subu t3, t1, v1
  c->sq(r0, -16, a2);                               // sq r0, -16(a2)
  c->sra(t3, t3, 4);                                // sra t3, t3, 4
  c->sq(r0, 0, a0);                                 // sq r0, 0(a0)
  // nop                                            // sll r0, r0, 0
  c->sh(t3, 0, v1);                                 // sh t3, 0(v1)
  c->daddiu(v1, t3, 1);                             // daddiu v1, t3, 1
  c->sq(r0, 0, t1);                                 // sq r0, 0(t1)
  // nop                                            // sll r0, r0, 0
  c->sq(r0, -16, t1);                               // sq r0, -16(t1)
  // nop                                            // sll r0, r0, 0
  c->daddiu(t5, a1, 1);                             // daddiu t5, a1, 1
  c->daddiu(t4, a1, 2);                             // daddiu t4, a1, 2
  c->lw(t3, 11988, at);                             // lw t3, 11988(at)
  // nop                                            // sll r0, r0, 0
  c->lw(t7, 11972, at);                             // lw t7, 11972(at)
  c->dsll(t0, t0, 16);                              // dsll t0, t0, 16
  c->lw(t6, 11976, at);                             // lw t6, 11976(at)
  c->or_(t4, t7, t4);                               // or t4, t7, t4
  c->lw(t7, 11980, at);                             // lw t7, 11980(at)
  c->or_(t5, t6, t5);                               // or t5, t6, t5
  c->lw(t6, 11984, at);                             // lw t6, 11984(at)
  // nop                                            // sll r0, r0, 0
  c->lw(t8, 11992, at);                             // lw t8, 11992(at)
  c->mov64(t9, a1);                                 // or t9, a1, r0
  c->sw(t2, 8, a0);                                 // sw t2, 8(a0)
  c->or_(t2, t7, t9);                               // or t2, t7, t9
  c->sw(t8, 0, t1);                                 // sw t8, 0(t1)
  c->daddiu(a0, a0, 16);                            // daddiu a0, a0, 16
  c->sw(r0, 4, t1);                                 // sw r0, 4(t1)
  c->or_(t4, t4, t0);                               // or t4, t4, t0
  c->sw(t6, 8, t1);                                 // sw t6, 8(t1)
  c->daddiu(a3, a3, 16);                            // daddiu a3, a3, 16
  c->sw(t3, 12, t1);                                // sw t3, 12(t1)
  c->or_(t1, t5, t0);                               // or t1, t5, t0
  c->sw(t4, -4, a0);                                // sw t4, -4(a0)
  c->or_(t0, t2, t0);                               // or t0, t2, t0
  c->sw(t1, -4, a3);                                // sw t1, -4(a3)
  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->sw(t0, -4, a2);                                // sw t0, -4(a2)
  c->addiu(t0, r0, 567);                            // addiu t0, r0, 567
  c->sw(v1, 40, at);                                // sw v1, 40(at)
  bc = c->sgpr64(a1) != c->sgpr64(t0);              // bne a1, t0, L72
  c->daddiu(v1, a1, 279);                           // daddiu v1, a1, 279
  if (bc) {goto block_13;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->addiu(v1, r0, 9);                              // addiu v1, r0, 9

  block_13:
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 72, at);                                // sw v1, 72(at)
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 4, at);                                 // sw a0, 4(at)
  // nop                                            // sll r0, r0, 0
  c->sw(a3, 8, at);                                 // sw a3, 8(at)
  // nop                                            // sll r0, r0, 0
  c->sw(a2, 12, at);                                // sw a2, 12(at)
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->lq(gp, 12448, at);                             // lq gp, 12448(at)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 32);                            // daddiu sp, sp, 32
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c(-1, 0, "*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("generic-prepare-dma-single", execute, 128);
}

} // namespace generic_prepare_dma_single
} // namespace Mips2C
// add generic_prepare_dma_single::link to the link callback table for the object file.
// FWD DEC:

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace generic_warp_source_proc {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* math_camera; // *math-camera*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
// annoyingly, a aguess...
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);
  c->sd(fp, 12440, at);                             // sd fp, 12440(at)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->lw(a1, 44, at);                                // lw a1, 44(at)
  c->lw(v1, 8, a1);                                 // lw v1, 8(a1)
  c->lw(a0, 4, a1);                                 // lw a0, 4(a1)
  c->lh(a1, 20, a1);                                // lh a1, 20(a1)
  c->load_symbol2(t0, cache.math_camera);           // lw t0, *math-camera*(s7)
  c->lhu(a3, 6820, at);                             // lhu a3, 6820(at)
  c->lhu(a2, 6822, at);                             // lhu a2, 6822(at)
  c->dsll(t1, a3, 16);                              // dsll t1, a3, 16
  c->or_(a3, a3, t1);                               // or a3, a3, t1
  c->lqc2(vf5, 7520, at);                           // lqc2 vf5, 7520(at)
  c->lqc2(vf1, 7136, at);                           // lqc2 vf1, 7136(at)
  c->lqc2(vf2, 7152, at);                           // lqc2 vf2, 7152(at)
  c->lqc2(vf3, 7168, at);                           // lqc2 vf3, 7168(at)
  c->lqc2(vf4, 7184, at);                           // lqc2 vf4, 7184(at)
  c->lui(t1, -16966);                               // lui t1, -16966
  c->ori(t1, t1, 24117);                            // ori t1, t1, 24117
  c->lwc1(f0, 0, t0);                               // lwc1 f0, 0(t0)
  c->mfc1(t0, f0);                                  // mfc1 t0, f0
  c->dsll32(t1, t1, 0);                             // dsll32 t1, t1, 0
  c->or_(t0, t1, t0);                               // or t0, t1, t0
  c->mov128_vf_gpr(vf6, t0);                        // qmtc2.i vf6, t0
  // Unknown instr: ld t0, L111(fp)
  c->gprs[t0].du64[0] = 0xffff'ffff;
  // nop                                            // sll r0, r0, 0

  block_1:
  c->lqc2(vf9, 0, a0);                              // lqc2 vf9, 0(a0)
  c->lwu(t1, 12, a0);                               // lwu t1, 12(a0)
  c->vmax_bc(DEST::z, BC::x, vf9, vf9, vf6);        // vmaxx.z vf9, vf9, vf6
  // nop                                            // vnop
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf9);        // vmaddax.xyzw acc, vf1, vf9
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf9);        // vmadday.xyzw acc, vf2, vf9
  c->vmadd_bc(DEST::xyzw, BC::z, vf9, vf3, vf9);    // vmaddz.xyzw vf9, vf3, vf9
  // nop                                            // vnop
  c->vdiv(vf5, BC::w, vf9, BC::w);                  // vdiv Q, vf5.w, vf9.w
  // nop                                            // vnop
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyz, vf9, vf9);                    // vmulq.xyz vf9, vf9, Q
  c->vmul(DEST::xy, vf9, vf9, vf5);                 // vmul.xy vf9, vf9, vf5
  c->vadd_bc(DEST::xy, BC::z, vf9, vf9, vf5);       // vaddz.xy vf9, vf9, vf5
  c->vadd_bc(DEST::y, BC::y, vf9, vf9, vf6);        // vaddy.y vf9, vf9, vf6
  c->vftoi12(DEST::xyzw, vf10, vf9);                // vftoi12.xyzw vf10, vf9
  c->psubh(t1, t1, a3);
  c->dsrav(t1, t1, a2);                             // dsrav t1, t1, a2
  c->ppacb(t1, r0, t1);                             // ppacb t1, r0, t1
  c->and_(t1, t1, t0);                              // and t1, t1, t0
  bc = ((s64)c->sgpr64(t1)) < 0;                    // bltz t1, L20
  c->daddiu(t2, t1, -394);                          // daddiu t2, t1, -394
  if (bc) {goto block_3;}                           // branch non-likely

  bc = ((s64)c->sgpr64(t2)) < 0;                    // bltz t2, L21
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_4;}                           // branch non-likely


  block_3:
  // nop                                            // sll r0, r0, 0

  block_4:
  c->sll(t1, t1, 2);                                // sll t1, t1, 2
  c->daddu(t1, v1, t1);                             // daddu t1, v1, t1
  c->mov128_gpr_vf(t2, vf10);                       // qmfc2.i t2, vf10
  c->ppach(t2, r0, t2);                             // ppach t2, r0, t2
  c->daddiu(a1, a1, -1);                            // daddiu a1, a1, -1
  c->sw(t2, 0, t1);                                 // sw t2, 0(t1)
  bc = ((s64)c->sgpr64(a1)) > 0;                    // bgtz a1, L19
  c->daddiu(a0, a0, 32);                            // daddiu a0, a0, 32
  if (bc) {goto block_1;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(fp, 12440, at);                             // ld fp, 12440(at)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c(-1, 0, "*fake-scratchpad-data*").c();
  cache.math_camera = intern_from_c(-1, 0, "*math-camera*").c();
  gLinkedFunctionTable.reg("generic-warp-source-proc", execute, 128);
}

} // namespace generic_warp_source_proc
} // namespace Mips2C
// add generic_warp_source_proc::link to the link callback table for the object file.
// FWD DEC:

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace generic_warp_dest_proc {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
  c->sd(fp, 12440, at);                             // sd fp, 12440(at)
  c->mov64(fp, t9);                                 // or fp, t9, r0
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->lw(a1, 44, at);                                // lw a1, 44(at)
  c->lw(v1, 8, a1);                                 // lw v1, 8(a1)
  c->lw(a0, 4, a1);                                 // lw a0, 4(a1)
  c->lbu(a1, 17, a1);                               // lbu a1, 17(a1)
  c->lhu(a3, 6820, at);                             // lhu a3, 6820(at)
  c->lhu(a2, 6822, at);                             // lhu a2, 6822(at)
  c->dsll(t0, a3, 16);                              // dsll t0, a3, 16
  c->or_(a3, a3, t0);                               // or a3, a3, t0
  // Unknown instr: ld t0, L111(fp)
  c->gprs[t0].du64[0] = 0xffff'ffff;

  block_1:
  c->lwu(t1, 12, a0);                               // lwu t1, 12(a0)
  // Unknown instr: psubh t1, t1, a3
  c->psubh(t1, t1, a3);
  c->dsrav(t1, t1, a2);                             // dsrav t1, t1, a2
  c->ppacb(t1, r0, t1);                             // ppacb t1, r0, t1
  c->and_(t1, t1, t0);                              // and t1, t1, t0
  c->sll(t1, t1, 2);                                // sll t1, t1, 2
  c->daddu(t1, v1, t1);                             // daddu t1, v1, t1
  c->lw(t1, 0, t1);                                 // lw t1, 0(t1)
  c->sw(t1, 12, a0);                                // sw t1, 12(a0)
  c->daddiu(a1, a1, -1);                            // daddiu a1, a1, -1
  bc = ((s64)c->sgpr64(a1)) > 0;                    // bgtz a1, L16
  c->daddiu(a0, a0, 32);                            // daddiu a0, a0, 32
  if (bc) {goto block_1;}                           // branch non-likely

  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(fp, 12440, at);                             // ld fp, 12440(at)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c(-1, 0, "*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("generic-warp-dest-proc", execute, 128);
}

} // namespace generic_warp_dest_proc
} // namespace Mips2C
// add generic_warp_dest_proc::link to the link callback table for the object file.
// FWD DEC:

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace generic_no_light_proc {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  // u32 call_addr = 0;
  c->daddiu(sp, sp, -96);                           // daddiu sp, sp, -96
  c->sd(ra, 12432, at);                             // sd ra, 12432(at)
  c->sq(s2, 12448, at);                             // sq s2, 12448(at)
  c->sq(s3, 12464, at);                             // sq s3, 12464(at)
  c->sq(s4, 12480, at);                             // sq s4, 12480(at)
  c->sq(s5, 12496, at);                             // sq s5, 12496(at)
  c->sq(gp, 12512, at);                             // sq gp, 12512(at)
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  // nop                                            // sll r0, r0, 0
  c->addiu(t1, r0, 256);                            // addiu t1, r0, 256
  c->lw(v1, 36, at);                                // lw v1, 36(at)
  c->lui(a0, -2);                                   // lui a0, -2
  c->lw(a1, 44, at);                                // lw a1, 44(at)
  c->daddiu(v1, v1, 3);                             // daddiu v1, v1, 3
  c->addiu(a3, r0, 255);                            // addiu a3, r0, 255
  c->dsra(a2, v1, 2);                               // dsra a2, v1, 2
  c->lw(v1, 0, a1);                                 // lw v1, 0(a1)
  c->dsll(t0, a2, 3);                               // dsll t0, a2, 3
  c->lw(a2, 4, a1);                                 // lw a2, 4(a1)
  c->ori(a1, a0, 65534);                            // ori a1, a0, 65534
  c->daddu(a0, v1, t0);                             // daddu a0, v1, t0
  c->pextlw(a1, a1, a1);                            // pextlw a1, a1, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlw(a1, a1, a1);                            // pextlw a1, a1, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlw(a2, a2, a2);                            // pextlw a2, a2, a2
  c->lw(t2, 4, at);                                 // lw t2, 4(at)
  c->pextlw(a2, a2, a2);                            // pextlw a2, a2, a2
  c->lw(t3, 8, at);                                 // lw t3, 8(at)
  c->pcpyh(a3, a3);                                 // pcpyh a3, a3
  c->lw(t4, 12, at);                                // lw t4, 12(at)
  c->pcpyld(a3, a3, a3);                            // pcpyld a3, a3, a3
  c->lq(t0, 12160, at);                             // lq t0, 12160(at)
  c->pcpyh(t1, t1);                                 // pcpyh t1, t1
  c->ld(t5, 0, v1);                                 // ld t5, 0(v1)
  c->pcpyld(t1, t1, t1);                            // pcpyld t1, t1, t1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(t6, r0, t5);                            // pextlh t6, r0, t5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pand(t5, t6, a3);                              // pand t5, t6, a3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psllw(t5, t5, 5);                              // psllw t5, t5, 5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->daddiu(t2, t2, -48);                           // daddiu t2, t2, -48
  c->daddiu(t3, t3, -16);                           // daddiu t3, t3, -16
  //beq r0, r0, L55                                 // beq r0, r0, L55
  c->daddiu(t4, t4, -16);                           // daddiu t4, t4, -16
  goto block_2;                                     // branch always


  block_1:
  c->pextlh(t6, r0, ra);                            // pextlh t6, r0, ra
  c->sq(t5, 0, t2);                                 // sq t5, 0(t2)
  c->pand(t5, t6, a3);                              // pand t5, t6, a3
  c->sq(t7, 16, t2);                                // sq t7, 16(t2)
  c->psllw(t5, t5, 5);                              // psllw t5, t5, 5
  c->sq(t8, 32, t2);                                // sq t8, 32(t2)

  block_2:
  c->paddw(gp, t5, a2);                             // paddw gp, t5, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsrl32(s4, gp, 0);                             // dsrl32 s4, gp, 0
  c->daddiu(t2, t2, 48);                            // daddiu t2, t2, 48
  c->pcpyud(s5, gp, r0);                            // pcpyud s5, gp, r0
  c->lq(t5, 0, gp);                                 // lq t5, 0(gp)
  c->dsrl32(t8, s5, 0);                             // dsrl32 t8, s5, 0
  c->daddiu(t3, t3, 16);                            // daddiu t3, t3, 16
  c->pand(t6, t6, t1);                              // pand t6, t6, t1
  c->lq(t7, 0, s4);                                 // lq t7, 0(s4)
  c->psraw(ra, t6, 8);                              // psraw ra, t6, 8
  c->lq(t6, 0, s5);                                 // lq t6, 0(s5)
  c->pextuw(s3, t7, t5);                            // pextuw s3, t7, t5
  c->lq(t9, 0, t8);                                 // lq t9, 0(t8)
  c->daddiu(t4, t4, 16);                            // daddiu t4, t4, 16
  c->daddiu(v1, v1, 8);                             // daddiu v1, v1, 8
  c->pextuw(s2, t9, t6);                            // pextuw s2, t9, t6
  c->lw(gp, 28, gp);                                // lw gp, 28(gp)
  c->pcpyud(s3, s3, s2);                            // pcpyud s3, s3, s2
  c->lw(s4, 28, s4);                                // lw s4, 28(s4)
  c->paddh(s3, s3, t0);                             // paddh s3, s3, t0
  c->lw(s5, 28, s5);                                // lw s5, 28(s5)
  c->pand(s3, s3, a1);                              // pand s3, s3, a1
  c->lw(t8, 28, t8);                                // lw t8, 28(t8)
  c->por(ra, s3, ra);                               // por ra, s3, ra
  c->sw(gp, 0, t3);                                 // sw gp, 0(t3)
  c->prot3w(t9, t9);                                // prot3w t9, t9
  c->sq(ra, 0, t4);                                 // sq ra, 0(t4)
  c->prot3w(t7, t7);                                // prot3w t7, t7
  c->sw(s4, 4, t3);                                 // sw s4, 4(t3)
  c->pextuw(gp, t7, t5);                            // pextuw gp, t7, t5
  c->sw(s5, 8, t3);                                 // sw s5, 8(t3)
  c->pcpyld(t7, t6, t7);                            // pcpyld t7, t6, t7
  c->ld(ra, 0, v1);                                 // ld ra, 0(v1)
  c->pcpyld(t5, gp, t5);                            // pcpyld t5, gp, t5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextuw(t6, t9, t6);                            // pextuw t6, t9, t6
  c->sw(t8, 12, t3);                                // sw t8, 12(t3)
  bc = c->sgpr64(v1) != c->sgpr64(a0);              // bne v1, a0, L54
  c->pcpyld(t8, t9, t6);                            // pcpyld t8, t9, t6
  if (bc) {goto block_1;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->sq(t5, 0, t2);                                 // sq t5, 0(t2)
  // nop                                            // sll r0, r0, 0
  c->sq(t7, 16, t2);                                // sq t7, 16(t2)
  // nop                                            // sll r0, r0, 0
  c->sq(t8, 32, t2);                                // sq t8, 32(t2)
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 12432, at);                             // ld ra, 12432(at)
  c->lq(gp, 12512, at);                             // lq gp, 12512(at)
  c->lq(s5, 12496, at);                             // lq s5, 12496(at)
  c->lq(s4, 12480, at);                             // lq s4, 12480(at)
  c->lq(s3, 12464, at);                             // lq s3, 12464(at)
  c->lq(s2, 12448, at);                             // lq s2, 12448(at)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 96);                            // daddiu sp, sp, 96
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c(-1, 0, "*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("generic-no-light-proc", execute, 128);
}

} // namespace generic_no_light_proc
} // namespace Mips2C
// add generic_no_light_proc::link to the link callback table for the object file.
// FWD DEC:

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace generic_warp_dest {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* warp_shader; // *warp-shader*
  void* generic_no_light_proc; // generic-no-light-proc
  void* generic_warp_dest_proc; // generic-warp-dest-proc
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 madr, sadr, qwc;
  [[maybe_unused]] u32 call_addr = 0;
  c->daddiu(sp, sp, -32);                           // daddiu sp, sp, -32
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(gp, 16, sp);                                // sq gp, 16(sp)
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L14
  c->mov64(v0, s7);                                 // or v0, s7, r0
  if (bc) {goto block_8;}                           // branch non-likely

  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->mov64(v1, a0);                                 // or v1, a0, r0
  c->load_symbol2(gp, cache.warp_shader);           // lw gp, *warp-shader*(s7)
  c->addiu(a0, r0, 14);                             // addiu a0, r0, 14
  c->sh(a0, 11984, at);                             // sh a0, 11984(at)
  c->sw(r0, 48, at);                                // sw r0, 48(at)
  c->sw(v1, 44, at);                                // sw v1, 44(at)
  c->sh(r0, 56, at);                                // sh r0, 56(at)
  c->daddiu(v1, at, 12064);                         // daddiu v1, at, 12064
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->lbu(a0, 6842, a0);                             // lbu a0, 6842(a0)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L11
  c->mov64(a0, s7);                                 // or a0, s7, r0
  if (bc) {goto block_3;}                           // branch non-likely

  c->addiu(v1, r0, 6960);                           // addiu v1, r0, 6960
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  c->mov64(v1, v1);                                 // or v1, v1, r0
  c->mov64(a0, v1);                                 // or a0, v1, r0

  block_3:
  c->sw(v1, 52, at);                                // sw v1, 52(at)
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(t9, 7464, v1);                             // lwu t9, 7464(v1)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  generic_prepare_dma_single::execute_real(c);
  c->ld(v1, 0, gp);                                 // ld v1, 0(gp)
  c->ld(a0, 16, gp);                                // ld a0, 16(gp)
  c->ld(a1, 32, gp);                                // ld a1, 32(gp)
  c->ld(a2, 48, gp);                                // ld a2, 48(gp)
  c->ld(a3, 64, gp);                                // ld a3, 64(gp)
  c->ld(t0, 11936, at);                             // ld t0, 11936(at)
  c->lw(t1, 24, at);                                // lw t1, 24(at)
  c->sd(v1, 128, t1);                               // sd v1, 128(t1)
  c->sd(a0, 144, t1);                               // sd a0, 144(t1)
  c->sd(a1, 160, t1);                               // sd a1, 160(t1)
  c->sd(a2, 176, t1);                               // sd a2, 176(t1)
  c->sd(a3, 192, t1);                               // sd a3, 192(t1)
  c->sd(t0, 96, t1);                                // sd t0, 96(t1)
  c->load_symbol2(t9, cache.generic_warp_dest_proc);// lw t9, generic-warp-dest-proc(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  generic_warp_dest_proc::execute(c);
  c->load_symbol2(t9, cache.generic_no_light_proc); // lw t9, generic-no-light-proc(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  generic_no_light_proc::execute(c);
  c->lw(v1, 24, at);                                // lw v1, 24(at)
  c->lw(a0, 40, at);                                // lw a0, 40(at)
  c->mov64(a3, v1);                                 // or a3, v1, r0
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->lui(a2, 4096);                                 // lui a2, 4096
  c->lwu(a1, 60, at);                               // lwu a1, 60(at)
  c->ori(a2, a2, 53248);                            // ori a2, a2, 53248
  // c->lw(t1, 0, a2);                                 // lw t1, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->daddiu(t0, at, 92);                            // daddiu t0, at, 92
  c->andi(a3, a3, 16383);                           // andi a3, a3, 16383
  c->andi(t1, t1, 256);                             // andi t1, t1, 256
  // nop                                            // sll r0, r0, 0
//  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L13
//  // nop                                            // sll r0, r0, 0
//  if (bc) {goto block_7;}                           // branch non-likely

  c->mov64(t1, a2);                                 // or t1, a2, r0
  // nop                                            // sll r0, r0, 0

//  block_5:
//  c->lw(t2, 0, t0);                                 // lw t2, 0(t0)
//  // nop                                            // sll r0, r0, 0
//  c->lw(t3, 0, t1);                                 // lw t3, 0(t1)
//  // nop                                            // sll r0, r0, 0
//  c->andi(t3, t3, 256);                             // andi t3, t3, 256
//  c->daddiu(t2, t2, 1);                             // daddiu t2, t2, 1
//  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L12
//  c->sw(t2, 0, t0);                                 // sw t2, 0(t0)
//  if (bc) {goto block_5;}                           // branch non-likely
//
//  c->gprs[t0].du64[0] = 0;                          // or t0, r0, r0

  // block_7:
  c->dsll(t0, a0, 4);                               // dsll t0, a0, 4
  // c->sw(a3, 128, a2);                               // sw a3, 128(a2)
  sadr = c->sgpr64(a3);
  // nop                                            // sll r0, r0, 0
  // c->sw(a1, 16, a2);                                // sw a1, 16(a2)
  madr = c->sgpr64(a1);
  c->addiu(a3, r0, 256);                            // addiu a3, r0, 256
  // c->sw(a0, 32, a2);                                // sw a0, 32(a2)
  qwc = c->sgpr64(a0);
  c->daddu(a0, a1, t0);                             // daddu a0, a1, t0
  // c->sw(a3, 0, a2);                                 // sw a3, 0(a2)
  spad_from_dma_no_sadr_off(cache.fake_scratchpad_data, madr, sadr, qwc);
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 60, at);                                // sw a0, 60(at)
  c->gprs[a0].du64[0] = 0;                          // or a0, r0, r0
  c->xori(v0, v1, 4608);                            // xori v0, v1, 4608
  c->sw(v0, 24, at);                                // sw v0, 24(at)

  block_8:
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 16, sp);                                // lq gp, 16(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 32);                            // daddiu sp, sp, 32
  goto end_of_function;                             // return

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
  cache.fake_scratchpad_data = intern_from_c(-1, 0, "*fake-scratchpad-data*").c();
  cache.warp_shader = intern_from_c(-1, 0, "*warp-shader*").c();
  cache.generic_no_light_proc = intern_from_c(-1, 0, "generic-no-light-proc").c();
  cache.generic_warp_dest_proc = intern_from_c(-1, 0, "generic-warp-dest-proc").c();
  gLinkedFunctionTable.reg("generic-warp-dest", execute, 128);
}

} // namespace generic_warp_dest
} // namespace Mips2C
// add generic_warp_dest::link to the link callback table for the object file.
// FWD DEC:

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak3/kscheme.h"
using ::jak3::intern_from_c;
namespace Mips2C::jak3 {
namespace generic_warp_envmap_dest {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* warp_shader; // *warp-shader*
  void* generic_envmap_proc; // generic-envmap-proc
  void* generic_no_light_proc; // generic-no-light-proc
  void* generic_warp_dest_proc; // generic-warp-dest-proc
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  [[maybe_unused]] u32 call_addr = 0;
  u32 qwc = 0;
  u32 madr = 0;
  u32 sadr = 0;
  c->daddiu(sp, sp, -32);                           // daddiu sp, sp, -32
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(gp, 16, sp);                                // sq gp, 16(sp)
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L9
  c->mov64(v0, s7);                                 // or v0, s7, r0
  if (bc) {goto block_11;}                          // branch non-likely

  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->mov64(v1, a0);                                 // or v1, a0, r0
  c->load_symbol2(gp, cache.warp_shader);           // lw gp, *warp-shader*(s7)
  c->addiu(a0, r0, 14);                             // addiu a0, r0, 14
  c->sh(a0, 11984, at);                             // sh a0, 11984(at)
  c->sw(r0, 48, at);                                // sw r0, 48(at)
  c->sw(v1, 44, at);                                // sw v1, 44(at)
  c->addiu(v1, r0, 1);                              // addiu v1, r0, 1
  c->sh(v1, 56, at);                                // sh v1, 56(at)
  c->daddiu(v1, at, 12064);                         // daddiu v1, at, 12064
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->lbu(a0, 6842, a0);                             // lbu a0, 6842(a0)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L4
  c->mov64(a0, s7);                                 // or a0, s7, r0
  if (bc) {goto block_3;}                           // branch non-likely

  c->addiu(v1, r0, 6960);                           // addiu v1, r0, 6960
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  c->mov64(v1, v1);                                 // or v1, v1, r0
  c->mov64(a0, v1);                                 // or a0, v1, r0

  block_3:
  c->sw(v1, 52, at);                                // sw v1, 52(at)
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(t9, 7468, v1);                             // lwu t9, 7468(v1)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  generic_prepare_dma_double::execute(c);
  c->ld(v1, 0, gp);                                 // ld v1, 0(gp)
  c->ld(a0, 16, gp);                                // ld a0, 16(gp)
  c->ld(a1, 32, gp);                                // ld a1, 32(gp)
  c->ld(a2, 48, gp);                                // ld a2, 48(gp)
  c->ld(t0, 64, gp);                                // ld t0, 64(gp)
  c->ld(a3, 11936, at);                             // ld a3, 11936(at)
  get_fake_spad_addr2(t1, cache.fake_scratchpad_data, 0, c);// lui t1, 28672
  c->lbu(t1, 6856, t1);                             // lbu t1, 6856(t1)
  c->addiu(t2, r0, 128);                            // addiu t2, r0, 128
  c->sltu(t2, t2, t1);                              // sltu t2, t2, t1
  bc = c->sgpr64(t2) != 0;                          // bne t2, r0, L5
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_5;}                           // branch non-likely

  c->addiu(t2, r0, 128);                            // addiu t2, r0, 128
  c->dsubu(t1, t2, t1);                             // dsubu t1, t2, t1
  c->dsll32(t1, t1, 24);                            // dsll32 t1, t1, 24
  c->dsrl(t1, t1, 24);                              // dsrl t1, t1, 24
  c->ori(t1, t1, 100);                              // ori t1, t1, 100
  c->mov64(t1, t1);                                 // or t1, t1, r0
  c->mov64(t2, t1);                                 // or t2, t1, r0
  //beq r0, r0, L6                                  // beq r0, r0, L6
  // nop                                            // sll r0, r0, 0
  goto block_6;                                     // branch always


  block_5:
  c->ld(t1, 11904, at);                             // ld t1, 11904(at)
  c->mov64(t2, t1);                                 // or t2, t1, r0

  block_6:
  c->lw(t2, 24, at);                                // lw t2, 24(at)
  c->sd(v1, 128, t2);                               // sd v1, 128(t2)
  c->sd(a0, 144, t2);                               // sd a0, 144(t2)
  c->sd(a1, 160, t2);                               // sd a1, 160(t2)
  c->sd(a2, 176, t2);                               // sd a2, 176(t2)
  c->sd(t0, 192, t2);                               // sd t0, 192(t2)
  c->addiu(v1, r0, 71);                             // addiu v1, r0, 71
  c->sd(a3, 96, t2);                                // sd a3, 96(t2)
  c->addiu(a0, r0, 66);                             // addiu a0, r0, 66
  c->sd(t1, 112, t2);                               // sd t1, 112(t2)
  c->sw(v1, 104, t2);                               // sw v1, 104(t2)
  c->sw(a0, 120, t2);                               // sw a0, 120(t2)
  c->load_symbol2(t9, cache.generic_warp_dest_proc);// lw t9, generic-warp-dest-proc(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  generic_warp_dest_proc::execute(c);
  c->load_symbol2(t9, cache.generic_no_light_proc); // lw t9, generic-no-light-proc(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  generic_no_light_proc::execute(c);
  c->load_symbol2(t9, cache.generic_envmap_proc);   // lw t9, generic-envmap-proc(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  generic_envmap_proc::execute(c);
  c->lw(v1, 24, at);                                // lw v1, 24(at)
  c->lw(a0, 40, at);                                // lw a0, 40(at)
  c->mov64(a3, v1);                                 // or a3, v1, r0
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->lui(a2, 4096);                                 // lui a2, 4096
  c->lwu(a1, 60, at);                               // lwu a1, 60(at)
  c->ori(a2, a2, 53248);                            // ori a2, a2, 53248
  // c->lw(t1, 0, a2);                                 // lw t1, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->daddiu(t0, at, 92);                            // daddiu t0, at, 92
  c->andi(a3, a3, 16383);                           // andi a3, a3, 16383
  c->andi(t1, t1, 256);                             // andi t1, t1, 256
  // nop                                            // sll r0, r0, 0
//  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L8
//  // nop                                            // sll r0, r0, 0
//  if (bc) {goto block_10;}                          // branch non-likely
//
//  c->mov64(t1, a2);                                 // or t1, a2, r0
//  // nop                                            // sll r0, r0, 0
//
//  block_8:
//  c->lw(t2, 0, t0);                                 // lw t2, 0(t0)
//  // nop                                            // sll r0, r0, 0
//  c->lw(t3, 0, t1);                                 // lw t3, 0(t1)
//  // nop                                            // sll r0, r0, 0
//  c->andi(t3, t3, 256);                             // andi t3, t3, 256
//  c->daddiu(t2, t2, 1);                             // daddiu t2, t2, 1
//  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L7
//  c->sw(t2, 0, t0);                                 // sw t2, 0(t0)
//  if (bc) {goto block_8;}                           // branch non-likely

  // c->gprs[t0].du64[0] = 0;                          // or t0, r0, r0

  // block_10:
  c->dsll(t0, a0, 4);                               // dsll t0, a0, 4
  // c->sw(a3, 128, a2);                               // sw a3, 128(a2)
  sadr = c->sgpr64(a3);
  // nop                                            // sll r0, r0, 0
  // c->sw(a1, 16, a2);                                // sw a1, 16(a2)
  madr = c->sgpr64(a1);
  c->addiu(a3, r0, 256);                            // addiu a3, r0, 256
  // c->sw(a0, 32, a2);                                // sw a0, 32(a2)
  qwc = c->sgpr64(a0);
  c->daddu(a0, a1, t0);                             // daddu a0, a1, t0
  // c->sw(a3, 0, a2);                                 // sw a3, 0(a2)
  spad_from_dma_no_sadr_off(cache.fake_scratchpad_data, madr, sadr, qwc);
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 60, at);                                // sw a0, 60(at)
  c->gprs[a0].du64[0] = 0;                          // or a0, r0, r0
  c->xori(v0, v1, 4608);                            // xori v0, v1, 4608
  c->sw(v0, 24, at);                                // sw v0, 24(at)

  block_11:
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 16, sp);                                // lq gp, 16(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 32);                            // daddiu sp, sp, 32
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c(-1, 0, "*fake-scratchpad-data*").c();
  cache.warp_shader = intern_from_c(-1, 0, "*warp-shader*").c();
  cache.generic_envmap_proc = intern_from_c(-1, 0, "generic-envmap-proc").c();
  cache.generic_no_light_proc = intern_from_c(-1, 0, "generic-no-light-proc").c();
  cache.generic_warp_dest_proc = intern_from_c(-1, 0, "generic-warp-dest-proc").c();
  gLinkedFunctionTable.reg("generic-warp-envmap-dest", execute, 128);
}

} // namespace generic_warp_envmap_dest
} // namespace Mips2C
// add generic_warp_envmap_dest::link to the link callback table for the object file.
// FWD DEC:
