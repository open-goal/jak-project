//--------------------------MIPS2C---------------------

#include "game/kernel/jak1/kscheme.h"
#include "game/mips2c/mips2c_private.h"
using namespace jak1;
namespace Mips2C::jak1 {

// clang-format off
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

namespace generic_envmap_dproc {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  // nop                                            // sll r0, r0, 0
  get_fake_spad_addr(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->lui(v1, 16256);                                // lui v1, 16256
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lwu(a1, 60, at);                               // lwu a1, 60(at)
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 12048, at);                             // lw v1, 12048(at)
  // nop                                            // sll r0, r0, 0
  c->lw(a2, 4, a1);                                 // lw a2, 4(a1)
  c->mov64(a0, a2);                                 // or a0, a2, r0
  c->lhu(a1, 20, a1);                               // lhu a1, 20(a1)
  // nop                                            // sll r0, r0, 0
  c->lwc1(f4, 24, a2);                              // lwc1 f4, 24(a2)
  c->daddiu(a1, a1, -4);                            // daddiu a1, a1, -4
  c->lwc1(f3, 56, a2);                              // lwc1 f3, 56(a2)
  // nop                                            // sll r0, r0, 0
  c->lwc1(f2, 88, a2);                              // lwc1 f2, 88(a2)
  // nop                                            // sll r0, r0, 0
  c->lwc1(f1, 120, a2);                             // lwc1 f1, 120(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 16, a2);                                // lq t2, 16(a2)
  c->subs(f4, f4, f0);                              // sub.s f4, f4, f0
  c->lq(t3, 48, a2);                                // lq t3, 48(a2)
  c->divs(f4, f0, f4);                              // div.s f4, f0, f4
  c->lq(t4, 80, a2);                                // lq t4, 80(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t5, 112, a2);                               // lq t5, 112(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf31, 12016, at);                         // lqc2 vf31, 12016(at)
  // nop                                            // sll r0, r0, 0
  c->lq(t6, 0, a2);                                 // lq t6, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(a3, 32, a2);                                // lq a3, 32(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t0, 64, a2);                                // lq t0, 64(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t1, 96, a2);                                // lq t1, 96(a2)
  c->muls(f4, f4, f0);                              // mul.s f4, f4, f0
  c->mov128_vf_gpr(vf21, t2);                       // qmtc2.i vf21, t2
  c->subs(f3, f3, f0);                              // sub.s f3, f3, f0
  c->mov128_vf_gpr(vf22, t3);                       // qmtc2.ni vf22, t3
  c->divs(f3, f0, f3);                              // div.s f3, f0, f3
  c->mov128_vf_gpr(vf23, t4);                       // qmtc2.ni vf23, t4
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf24, t5);                       // qmtc2.ni vf24, t5
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf9, t6);                        // qmtc2.ni vf9, t6
  c->subs(f2, f2, f0);                              // sub.s f2, f2, f0
  c->mfc1(t2, f4);                                  // mfc1 t2, f4
  c->subs(f1, f1, f0);                              // sub.s f1, f1, f0
  c->mov128_vf_gpr(vf10, a3);                       // qmtc2.ni vf10, a3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf11, t0);                       // qmtc2.ni vf11, t0
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf12, t1);                       // qmtc2.ni vf12, t1
  c->muls(f3, f3, f0);                              // mul.s f3, f3, f0
  // nop                                            // sll r0, r0, 0
  c->divs(f2, f0, f2);                              // div.s f2, f0, f2
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(a3, f3);                                  // mfc1 a3, f3
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
  c->pextlw(a3, a3, t2);                            // pextlw a3, a3, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(t0, f2);                                  // mfc1 t0, f2
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(a2, a2, 128);                           // daddiu a2, a2, 128
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(t1, f1);                                  // mfc1 t1, f1
  c->pextlw(t0, t1, t0);                            // pextlw t0, t1, t0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyld(a3, t0, a3);                            // pcpyld a3, t0, a3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf27, a3);                       // qmtc2.ni vf27, a3
  // nop                                            // sll r0, r0, 0
  // Unknown instr: vcallms 48
  vcallms48(c);
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
  c->lwc1(f4, 24, a2);                              // lwc1 f4, 24(a2)
  // nop                                            // sll r0, r0, 0
  c->lwc1(f3, 56, a2);                              // lwc1 f3, 56(a2)
  // nop                                            // sll r0, r0, 0
  c->lwc1(f2, 88, a2);                              // lwc1 f2, 88(a2)
  // nop                                            // sll r0, r0, 0
  c->lwc1(f1, 120, a2);                             // lwc1 f1, 120(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(a3, 16, a2);                                // lq a3, 16(a2)
  c->subs(f4, f4, f0);                              // sub.s f4, f4, f0
  // nop                                            // sll r0, r0, 0
  c->subs(f3, f3, f0);                              // sub.s f3, f3, f0
  // nop                                            // sll r0, r0, 0
  c->subs(f2, f2, f0);                              // sub.s f2, f2, f0
  // nop                                            // sll r0, r0, 0
  c->subs(f1, f1, f0);                              // sub.s f1, f1, f0
  // nop                                            // sll r0, r0, 0
  c->divs(f4, f0, f4);                              // div.s f4, f0, f4
  c->lq(t0, 48, a2);                                // lq t0, 48(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t1, 80, a2);                                // lq t1, 80(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 112, a2);                               // lq t2, 112(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 0, a2);                                 // lq t3, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t4, 32, a2);                                // lq t4, 32(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t5, 64, a2);                                // lq t5, 64(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t6, 96, a2);                                // lq t6, 96(a2)
  c->muls(f4, f4, f0);                              // mul.s f4, f4, f0
  // nop                                            // sll r0, r0, 0
  c->divs(f3, f0, f3);                              // div.s f3, f0, f3
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(t7, f4);                                  // mfc1 t7, f4
  // nop                                            // sll r0, r0, 0
  c->daddiu(a2, a2, 128);                           // daddiu a2, a2, 128
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
  c->mfc1(t8, f3);                                  // mfc1 t8, f3
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
  c->pextlw(t7, t8, t7);                            // pextlw t7, t8, t7
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(t8, f2);                                  // mfc1 t8, f2
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
  c->mfc1(t9, f1);                                  // mfc1 t9, f1
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf21, a3);                       // qmtc2.ni vf21, a3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf9, t3);                        // qmtc2.ni vf9, t3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf10, t4);                       // qmtc2.ni vf10, t4
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf11, t5);                       // qmtc2.ni vf11, t5
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf12, t6);                       // qmtc2.ni vf12, t6
  c->pextlw(a3, t9, t8);                            // pextlw a3, t9, t8
  // nop                                            // sll r0, r0, 0
  c->pcpyld(a3, a3, t7);                            // pcpyld a3, a3, t7
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf22, t0);                       // qmtc2.ni vf22, t0
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf23, t1);                       // qmtc2.ni vf23, t1
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf24, t2);                       // qmtc2.ni vf24, t2
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf27, a3);                       // qmtc2.ni vf27, a3
  // nop                                            // sll r0, r0, 0
  // Unknown instr: vcallms 48
  vcallms48(c);
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
  c->lwc1(f1, 24, a2);                              // lwc1 f1, 24(a2)
  c->subs(f1, f1, f0);                              // sub.s f1, f1, f0
  c->lwc1(f2, 56, a2);                              // lwc1 f2, 56(a2)
  c->divs(f3, f0, f1);                              // div.s f3, f0, f1
  c->lwc1(f5, 88, a2);                              // lwc1 f5, 88(a2)
  // nop                                            // sll r0, r0, 0
  c->lwc1(f1, 120, a2);                             // lwc1 f1, 120(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(a3, 16, a2);                                // lq a3, 16(a2)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->subs(f4, f2, f0);                              // sub.s f4, f2, f0
  // nop                                            // sll r0, r0, 0
  c->subs(f2, f5, f0);                              // sub.s f2, f5, f0
  // nop                                            // sll r0, r0, 0
  c->subs(f1, f1, f0);                              // sub.s f1, f1, f0
  // nop                                            // sll r0, r0, 0
  c->muls(f3, f3, f0);                              // mul.s f3, f3, f0
  c->lq(t0, 48, a2);                                // lq t0, 48(a2)
  c->divs(f4, f0, f4);                              // div.s f4, f0, f4
  c->lq(t1, 80, a2);                                // lq t1, 80(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 112, a2);                               // lq t2, 112(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 0, a2);                                 // lq t3, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t4, 32, a2);                                // lq t4, 32(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t5, 64, a2);                                // lq t5, 64(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t6, 96, a2);                                // lq t6, 96(a2)
  // nop                                            // sll r0, r0, 0
  c->mfc1(t7, f3);                                  // mfc1 t7, f3
  c->muls(f3, f4, f0);                              // mul.s f3, f4, f0
  // nop                                            // sll r0, r0, 0
  c->divs(f2, f0, f2);                              // div.s f2, f0, f2
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(a2, a2, 128);                           // daddiu a2, a2, 128
  // nop                                            // sll r0, r0, 0
  c->mfc1(t8, f3);                                  // mfc1 t8, f3
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->muls(f2, f2, f0);                              // mul.s f2, f2, f0
  // nop                                            // sll r0, r0, 0
  c->divs(f1, f0, f1);                              // div.s f1, f0, f1
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(t9, f2);                                  // mfc1 t9, f2
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->pextlw(t7, t8, t7);                            // pextlw t7, t8, t7
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(t8, f1);                                  // mfc1 t8, f1
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->pextlw(t8, t8, t9);                            // pextlw t8, t8, t9
  // nop                                            // sll r0, r0, 0
  c->pcpyld(t7, t8, t7);                            // pcpyld t7, t8, t7
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf21, a3);                       // qmtc2.ni vf21, a3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf22, t0);                       // qmtc2.ni vf22, t0
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf23, t1);                       // qmtc2.ni vf23, t1
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf24, t2);                       // qmtc2.ni vf24, t2
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf9, t3);                        // qmtc2.ni vf9, t3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf10, t4);                       // qmtc2.ni vf10, t4
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf11, t5);                       // qmtc2.ni vf11, t5
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf12, t6);                       // qmtc2.ni vf12, t6
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf27, t7);                       // qmtc2.ni vf27, t7
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t1, vf17);                       // qmfc2.ni t1, vf17
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t2, vf18);                       // qmfc2.ni t2, vf18
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t0, vf19);                       // qmfc2.ni t0, vf19
  bc = ((s64)c->sgpr64(a1)) <= 0;                   // blez a1, L47
  c->mov128_gpr_vf(a3, vf20);                       // qmfc2.ni a3, vf20
  if (bc) {goto block_2;}                           // branch non-likely


  block_1:
  c->ppach(t1, r0, t1);                             // ppach t1, r0, t1
  // Unknown instr: vcallms 48
  vcallms48(c);
  c->ppach(t2, r0, t2);                             // ppach t2, r0, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppach(t0, r0, t0);                             // ppach t0, r0, t0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppach(a3, r0, a3);                             // ppach a3, r0, a3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->sw(t1, 16, a0);                                // sw t1, 16(a0)
  // nop                                            // sll r0, r0, 0
  c->sw(t2, 48, a0);                                // sw t2, 48(a0)
  // nop                                            // sll r0, r0, 0
  c->sw(t0, 80, a0);                                // sw t0, 80(a0)
  // nop                                            // sll r0, r0, 0
  c->sw(a3, 112, a0);                               // sw a3, 112(a0)
  // nop                                            // sll r0, r0, 0
  c->lwc1(f4, 24, a2);                              // lwc1 f4, 24(a2)
  // nop                                            // sll r0, r0, 0
  c->lwc1(f3, 56, a2);                              // lwc1 f3, 56(a2)
  // nop                                            // sll r0, r0, 0
  c->lwc1(f2, 88, a2);                              // lwc1 f2, 88(a2)
  // nop                                            // sll r0, r0, 0
  c->lwc1(f1, 120, a2);                             // lwc1 f1, 120(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(a3, 16, a2);                                // lq a3, 16(a2)
  c->subs(f4, f4, f0);                              // sub.s f4, f4, f0
  c->sw(v1, 20, a0);                                // sw v1, 20(a0)
  c->subs(f3, f3, f0);                              // sub.s f3, f3, f0
  c->sw(v1, 52, a0);                                // sw v1, 52(a0)
  c->subs(f2, f2, f0);                              // sub.s f2, f2, f0
  c->sw(v1, 84, a0);                                // sw v1, 84(a0)
  c->subs(f1, f1, f0);                              // sub.s f1, f1, f0
  c->sw(v1, 116, a0);                               // sw v1, 116(a0)
  c->divs(f4, f0, f4);                              // div.s f4, f0, f4
  c->lq(t3, 48, a2);                                // lq t3, 48(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t4, 80, a2);                                // lq t4, 80(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t5, 112, a2);                               // lq t5, 112(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t6, 0, a2);                                 // lq t6, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 32, a2);                                // lq t2, 32(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t0, 64, a2);                                // lq t0, 64(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t1, 96, a2);                                // lq t1, 96(a2)
  c->daddiu(a1, a1, -4);                            // daddiu a1, a1, -4
  c->daddiu(a0, a0, 128);                           // daddiu a0, a0, 128
  c->muls(f4, f4, f0);                              // mul.s f4, f4, f0
  // nop                                            // sll r0, r0, 0
  c->divs(f3, f0, f3);                              // div.s f3, f0, f3
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(t7, f4);                                  // mfc1 t7, f4
  // nop                                            // sll r0, r0, 0
  c->daddiu(a2, a2, 128);                           // daddiu a2, a2, 128
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
  c->mfc1(t8, f3);                                  // mfc1 t8, f3
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
  c->pextlw(t7, t8, t7);                            // pextlw t7, t8, t7
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(t8, f2);                                  // mfc1 t8, f2
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
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(t9, f1);                                  // mfc1 t9, f1
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf21, a3);                       // qmtc2.ni vf21, a3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf22, t3);                       // qmtc2.ni vf22, t3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf23, t4);                       // qmtc2.ni vf23, t4
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf24, t5);                       // qmtc2.ni vf24, t5
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf9, t6);                        // qmtc2.ni vf9, t6
  c->pextlw(a3, t9, t8);                            // pextlw a3, t9, t8
  c->mov128_vf_gpr(vf10, t2);                       // qmtc2.ni vf10, t2
  c->pcpyld(a3, a3, t7);                            // pcpyld a3, a3, t7
  c->mov128_vf_gpr(vf11, t0);                       // qmtc2.ni vf11, t0
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf12, t1);                       // qmtc2.ni vf12, t1
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf27, a3);                       // qmtc2.ni vf27, a3
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t1, vf17);                       // qmfc2.ni t1, vf17
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t2, vf18);                       // qmfc2.ni t2, vf18
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t0, vf19);                       // qmfc2.ni t0, vf19
  bc = ((s64)c->sgpr64(a1)) > 0;                    // bgtz a1, L46
  c->mov128_gpr_vf(a3, vf20);                       // qmfc2.ni a3, vf20
  if (bc) {goto block_1;}                           // branch non-likely


  block_2:
  c->ppach(a1, r0, t1);                             // ppach a1, r0, t1
  c->sw(v1, 20, a0);                                // sw v1, 20(a0)
  c->ppach(a2, r0, t2);                             // ppach a2, r0, t2
  c->sw(v1, 52, a0);                                // sw v1, 52(a0)
  c->ppach(t0, r0, t0);                             // ppach t0, r0, t0
  c->sw(a1, 16, a0);                                // sw a1, 16(a0)
  c->ppach(a1, r0, a3);                             // ppach a1, r0, a3
  c->sw(a2, 48, a0);                                // sw a2, 48(a0)
  // nop                                            // sll r0, r0, 0
  c->sw(t0, 80, a0);                                // sw t0, 80(a0)
  // nop                                            // sll r0, r0, 0
  c->sw(a1, 112, a0);                               // sw a1, 112(a0)
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 84, a0);                                // sw v1, 84(a0)
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 116, a0);                               // sw v1, 116(a0)
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
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("generic-envmap-dproc", execute, 256);
}

} // namespace generic_envmap_dproc
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace generic_interp_dproc {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  get_fake_spad_addr(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 80, at);                                // lw v1, 80(at)
  // nop                                            // sll r0, r0, 0
  c->lw(a0, 60, at);                                // lw a0, 60(at)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L44
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lw(t0, 8, v1);                                 // lw t0, 8(v1)
  // nop                                            // sll r0, r0, 0
  c->lw(a2, 4, a0);                                 // lw a2, 4(a0)
  // nop                                            // sll r0, r0, 0
  c->lh(a0, 0, v1);                                 // lh a0, 0(v1)
  // nop                                            // sll r0, r0, 0
  c->lh(a1, 2, v1);                                 // lh a1, 2(v1)
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L44
  c->lh(a0, 4, v1);                                 // lh a0, 4(v1)
  if (bc) {goto block_7;}                           // branch non-likely

  c->dsll(t1, a0, 5);                               // dsll t1, a0, 5
  c->lh(a0, 12, v1);                                // lh a0, 12(v1)
  c->daddiu(a3, a1, 7);                             // daddiu a3, a1, 7
  c->lh(a1, 14, v1);                                // lh a1, 14(v1)
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L44
  c->daddu(v1, t1, a2);                             // daddu v1, t1, a2
  if (bc) {goto block_7;}                           // branch non-likely

  c->pextlh(a0, a0, a0);                            // pextlh a0, a0, a0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlw(a0, a0, a0);                            // pextlw a0, a0, a0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyld(a0, a0, a0);                            // pcpyld a0, a0, a0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(a1, a1, a1);                            // pextlh a1, a1, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlw(a1, a1, a1);                            // pextlw a1, a1, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyld(a1, a1, a1);                            // pcpyld a1, a1, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlw(a2, a2, a2);                            // pextlw a2, a2, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyld(a2, a2, a2);                            // pcpyld a2, a2, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsra(a3, a3, 3);                               // dsra a3, a3, 3
  // nop                                            // sll r0, r0, 0
  c->dsll(a3, a3, 4);                               // dsll a3, a3, 4
  c->ld(t1, 0, t0);                                 // ld t1, 0(t0)
  c->daddu(a3, t0, a3);                             // daddu a3, t0, a3
  c->daddiu(t0, t0, 8);                             // daddiu t0, t0, 8
  c->pextlb(t1, r0, t1);                            // pextlb t1, r0, t1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psllh(t2, t1, 5);                              // psllh t2, t1, 5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextuh(t1, r0, t2);                            // pextuh t1, r0, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(t2, r0, t2);                            // pextlh t2, r0, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->paddw(t2, t2, a2);                             // paddw t2, t2, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  //beq r0, r0, L43                                 // beq r0, r0, L43
  c->pcpyud(t5, t2, r0);                            // pcpyud t5, t2, r0
  goto block_5;                                     // branch always


  block_4:
  c->dsrl32(t5, t6, 0);                             // dsrl32 t5, t6, 0
  c->dsrl32(t4, t3, 0);                             // dsrl32 t4, t3, 0
  c->pextuh(t1, r0, t2);                            // pextuh t1, r0, t2
  c->sw(t6, 16, v1);                                // sw t6, 16(v1)
  c->pextlh(t2, r0, t2);                            // pextlh t2, r0, t2
  c->sw(t5, 48, v1);                                // sw t5, 48(v1)
  c->paddw(t2, t2, a2);                             // paddw t2, t2, a2
  c->sw(t3, 80, v1);                                // sw t3, 80(v1)
  c->pcpyud(t5, t2, r0);                            // pcpyud t5, t2, r0
  c->sw(t4, 112, v1);                               // sw t4, 112(v1)
  c->daddiu(t0, t0, 8);                             // daddiu t0, t0, 8
  c->daddiu(v1, v1, 128);                           // daddiu v1, v1, 128

  block_5:
  c->paddw(t1, t1, a2);                             // paddw t1, t1, a2
  c->lwu(t3, 16, t2);                               // lwu t3, 16(t2)
  c->pcpyud(t6, t1, r0);                            // pcpyud t6, t1, r0
  c->lwu(t4, 16, t5);                               // lwu t4, 16(t5)
  c->dsrl32(t8, t2, 0);                             // dsrl32 t8, t2, 0
  c->lwu(t2, 16, t1);                               // lwu t2, 16(t1)
  c->dsrl32(t9, t5, 0);                             // dsrl32 t9, t5, 0
  c->lwu(t5, 16, t6);                               // lwu t5, 16(t6)
  c->dsrl32(t7, t1, 0);                             // dsrl32 t7, t1, 0
  c->lwu(t1, 16, t8);                               // lwu t1, 16(t8)
  c->dsrl32(t8, t6, 0);                             // dsrl32 t8, t6, 0
  c->lwu(t6, 16, t9);                               // lwu t6, 16(t9)
  c->pextlw(t4, t4, t3);                            // pextlw t4, t4, t3
  c->lwu(t3, 16, t7);                               // lwu t3, 16(t7)
  c->pextlw(t2, t5, t2);                            // pextlw t2, t5, t2
  c->lwu(t5, 16, t8);                               // lwu t5, 16(t8)
  c->pcpyld(t2, t2, t4);                            // pcpyld t2, t2, t4
  c->lwu(t4, 16, v1);                               // lwu t4, 16(v1)
  c->pextlw(t6, t6, t1);                            // pextlw t6, t6, t1
  c->lwu(t1, 48, v1);                               // lwu t1, 48(v1)
  c->pextlw(t3, t5, t3);                            // pextlw t3, t5, t3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyld(t3, t3, t6);                            // pcpyld t3, t3, t6
  c->lwu(t5, 80, v1);                               // lwu t5, 80(v1)
  c->pextlw(t1, t1, t4);                            // pextlw t1, t1, t4
  c->lwu(t4, 112, v1);                              // lwu t4, 112(v1)
  c->pmulth(r0, t2, a1);                            // pmulth r0, t2, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlw(t2, t4, t5);                            // pextlw t2, t4, t5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmaddh(r0, t3, a1);                            // pmaddh r0, t3, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyld(t1, t2, t1);                            // pcpyld t1, t2, t1
  c->ld(t2, 0, t0);                                 // ld t2, 0(t0)
  c->pmaddh(r0, t1, a0);                            // pmaddh r0, t1, a0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(t1, r0, t2);                            // pextlb t1, r0, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psllh(t2, t1, 5);                              // psllh t2, t1, 5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // Unknown instr: pmfhl.lw t3
  c->pmfhl_lw(t3);
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // Unknown instr: pmfhl.uw t1
  c->pmfhl_uw(t1);
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psraw(t3, t3, 8);                              // psraw t3, t3, 8
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psraw(t1, t1, 8);                              // psraw t1, t1, 8
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pinteh(t6, t1, t3);                            // pinteh t6, t1, t3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  bc = c->sgpr64(t0) != c->sgpr64(a3);              // bne t0, a3, L42
  c->pcpyud(t3, t6, r0);                            // pcpyud t3, t6, r0
  if (bc) {goto block_4;}                           // branch non-likely

  c->dsrl32(a0, t6, 0);                             // dsrl32 a0, t6, 0
  c->sw(t6, 16, v1);                                // sw t6, 16(v1)
  c->dsrl32(a1, t3, 0);                             // dsrl32 a1, t3, 0
  c->sw(a0, 48, v1);                                // sw a0, 48(v1)
  // nop                                            // sll r0, r0, 0
  c->sw(t3, 80, v1);                                // sw t3, 80(v1)
  // nop                                            // sll r0, r0, 0
  c->sw(a1, 112, v1);                               // sw a1, 112(v1)

  block_7:
  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("generic-interp-dproc", execute, 128);
}

} // namespace generic_interp_dproc
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {
namespace generic_no_light_dproc {
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
  get_fake_spad_addr(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  // nop                                            // sll r0, r0, 0
  c->lw(a1, 60, at);                                // lw a1, 60(at)
  // nop                                            // sll r0, r0, 0
  c->lw(a0, 52, at);                                // lw a0, 52(at)
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 0, a1);                                 // lw v1, 0(a1)
  // nop                                            // sll r0, r0, 0
  c->lw(a2, 4, a1);                                 // lw a2, 4(a1)
  c->daddiu(a0, a0, 3);                             // daddiu a0, a0, 3
  // nop                                            // sll r0, r0, 0
  c->dsra(a0, a0, 2);                               // dsra a0, a0, 2
  // nop                                            // sll r0, r0, 0
  c->dsll(a0, a0, 3);                               // dsll a0, a0, 3
  c->addiu(a3, r0, 255);                            // addiu a3, r0, 255
  c->lui(a1, -2);                                   // lui a1, -2
  c->addiu(t1, r0, 256);                            // addiu t1, r0, 256
  c->ori(a1, a1, 65534);                            // ori a1, a1, 65534
  c->daddu(a0, v1, a0);                             // daddu a0, v1, a0
  c->pextlw(a1, a1, a1);                            // pextlw a1, a1, a1
  c->lw(t2, 20, at);                                // lw t2, 20(at)
  c->pextlw(a1, a1, a1);                            // pextlw a1, a1, a1
  c->lw(t3, 24, at);                                // lw t3, 24(at)
  c->pextlw(a2, a2, a2);                            // pextlw a2, a2, a2
  c->lw(t5, 28, at);                                // lw t5, 28(at)
  c->pextlw(a2, a2, a2);                            // pextlw a2, a2, a2
  c->lw(t4, 32, at);                                // lw t4, 32(at)
  c->pcpyh(a3, a3);                                 // pcpyh a3, a3
  c->lw(t6, 36, at);                                // lw t6, 36(at)
  c->pcpyld(a3, a3, a3);                            // pcpyld a3, a3, a3
  c->lq(t0, 12160, at);                             // lq t0, 12160(at)
  c->pcpyh(t1, t1);                                 // pcpyh t1, t1
  c->ld(t7, 0, v1);                                 // ld t7, 0(v1)
  c->pcpyld(t1, t1, t1);                            // pcpyld t1, t1, t1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(t8, r0, t7);                            // pextlh t8, r0, t7
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pand(t7, t8, a3);                              // pand t7, t8, a3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psllw(t7, t7, 5);                              // psllw t7, t7, 5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->daddiu(t2, t2, -48);                           // daddiu t2, t2, -48
  c->daddiu(t3, t3, -16);                           // daddiu t3, t3, -16
  c->daddiu(t4, t4, -16);                           // daddiu t4, t4, -16
  c->daddiu(t5, t5, -16);                           // daddiu t5, t5, -16
  //beq r0, r0, L34                                 // beq r0, r0, L34
  c->daddiu(t6, t6, -16);                           // daddiu t6, t6, -16
  goto block_3;                                     // branch always

  // nop                                            // sll r0, r0, 0

  block_2:
  c->pextlh(t8, r0, gp);                            // pextlh t8, r0, gp
  c->sq(t7, 0, t2);                                 // sq t7, 0(t2)
  c->pand(t7, t8, a3);                              // pand t7, t8, a3
  c->sq(t9, 16, t2);                                // sq t9, 16(t2)
  c->psllw(t7, t7, 5);                              // psllw t7, t7, 5
  c->sq(ra, 32, t2);                                // sq ra, 32(t2)

  block_3:
  c->paddw(s3, t7, a2);                             // paddw s3, t7, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsrl32(s2, s3, 0);                             // dsrl32 s2, s3, 0
  c->daddiu(t2, t2, 48);                            // daddiu t2, t2, 48
  c->pcpyud(s5, s3, r0);                            // pcpyud s5, s3, r0
  c->lq(t7, 0, s3);                                 // lq t7, 0(s3)
  c->dsrl32(s4, s5, 0);                             // dsrl32 s4, s5, 0
  c->daddiu(t3, t3, 16);                            // daddiu t3, t3, 16
  c->pand(t8, t8, t1);                              // pand t8, t8, t1
  c->lq(t9, 0, s2);                                 // lq t9, 0(s2)
  c->psraw(gp, t8, 8);                              // psraw gp, t8, 8
  c->lq(t8, 0, s5);                                 // lq t8, 0(s5)
  c->pextuw(s1, t9, t7);                            // pextuw s1, t9, t7
  c->lq(ra, 0, s4);                                 // lq ra, 0(s4)
  c->daddiu(t5, t5, 16);                            // daddiu t5, t5, 16
  c->daddiu(v1, v1, 8);                             // daddiu v1, v1, 8
  c->daddiu(t4, t4, 16);                            // daddiu t4, t4, 16
  c->daddiu(t6, t6, 16);                            // daddiu t6, t6, 16
  c->pextuw(s0, ra, t8);                            // pextuw s0, ra, t8
  c->lq(s3, 16, s3);                                // lq s3, 16(s3)
  c->pcpyud(s1, s1, s0);                            // pcpyud s1, s1, s0
  c->lq(s2, 16, s2);                                // lq s2, 16(s2)
  c->paddh(s0, s1, t0);                             // paddh s0, s1, t0
  c->lq(s1, 16, s5);                                // lq s1, 16(s5)
  c->pand(s5, s0, a1);                              // pand s5, s0, a1
  c->lq(s0, 16, s4);                                // lq s0, 16(s4)
  c->pextlw(s4, s2, s3);                            // pextlw s4, s2, s3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextuw(s3, s2, s3);                            // pextuw s3, s2, s3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlw(s2, s0, s1);                            // pextlw s2, s0, s1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextuw(s0, s0, s1);                            // pextuw s0, s0, s1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyld(s1, s2, s4);                            // pcpyld s1, s2, s4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyud(s4, s4, s2);                            // pcpyud s4, s4, s2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyud(s3, s3, s0);                            // pcpyud s3, s3, s0
  c->sq(s4, 0, t4);                                 // sq s4, 0(t4)
  c->pand(s4, s1, a1);                              // pand s4, s1, a1
  c->sq(s3, 0, t3);                                 // sq s3, 0(t3)
  c->por(s4, s4, gp);                               // por s4, s4, gp
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->por(gp, s5, gp);                               // por gp, s5, gp
  c->sq(s4, 0, t6);                                 // sq s4, 0(t6)
  c->prot3w(ra, ra);                                // prot3w ra, ra
  c->sq(gp, 0, t5);                                 // sq gp, 0(t5)
  c->prot3w(t9, t9);                                // prot3w t9, t9
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextuw(s5, t9, t7);                            // pextuw s5, t9, t7
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyld(t9, t8, t9);                            // pcpyld t9, t8, t9
  c->ld(gp, 0, v1);                                 // ld gp, 0(v1)
  c->pcpyld(t7, s5, t7);                            // pcpyld t7, s5, t7
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextuw(t8, ra, t8);                            // pextuw t8, ra, t8
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  bc = c->sgpr64(v1) != c->sgpr64(a0);              // bne v1, a0, L33
  c->pcpyld(ra, ra, t8);                            // pcpyld ra, ra, t8
  if (bc) {goto block_2;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->sq(t7, 0, t2);                                 // sq t7, 0(t2)
  // nop                                            // sll r0, r0, 0
  c->sq(t9, 16, t2);                                // sq t9, 16(t2)
  // nop                                            // sll r0, r0, 0
  c->sq(ra, 32, t2);                                // sq ra, 32(t2)
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
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("generic-no-light-dproc", execute, 256);
}

} // namespace generic_no_light_dproc
} // namespace Mips2C


