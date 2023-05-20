//--------------------------MIPS2C---------------------
#include "game/kernel/jak2/kscheme.h"
#include "game/mips2c/mips2c_private.h"
namespace Mips2C::jak2 {
// clang-format off
namespace {
void exec_0(ExecutionContext* c) {
  // nop                        |  sub.xyzw vf05, vf03, vf02      0
  c->vfs[vf05].vf.sub(Mask::xyzw, c->vf_src(vf03).vf, c->vf_src(vf02).vf);
  // nop                        |  sub.xyzw vf06, vf04, vf02      1
  c->vfs[vf06].vf.sub(Mask::xyzw, c->vf_src(vf04).vf, c->vf_src(vf02).vf);
  // nop                        |  sub.xyzw vf10, vf08, vf07      2
  c->vfs[vf10].vf.sub(Mask::xyzw, c->vf_src(vf08).vf, c->vf_src(vf07).vf);
  // nop                        |  sub.xyzw vf11, vf09, vf07      3
  c->vfs[vf11].vf.sub(Mask::xyzw, c->vf_src(vf09).vf, c->vf_src(vf07).vf);
  // nop                        |  sub.xyzw vf15, vf13, vf12      4
  c->vfs[vf15].vf.sub(Mask::xyzw, c->vf_src(vf13).vf, c->vf_src(vf12).vf);
  // nop                        |  sub.xyzw vf16, vf14, vf12      5
  c->vfs[vf16].vf.sub(Mask::xyzw, c->vf_src(vf14).vf, c->vf_src(vf12).vf);
  // nop                        |  sub.xyzw vf20, vf18, vf17      6
  c->vfs[vf20].vf.sub(Mask::xyzw, c->vf_src(vf18).vf, c->vf_src(vf17).vf);
  // nop                        |  sub.xyzw vf21, vf19, vf17      7
  c->vfs[vf21].vf.sub(Mask::xyzw, c->vf_src(vf19).vf, c->vf_src(vf17).vf);
  // nop                        |  opmula.xyz ACC, vf05, vf06     8
  c->vopmula(vf05, vf06); // ASSERT(false);
  // nop                        |  opmsub.xyz vf05, vf06, vf05    9
  c->vopmsub(vf05, vf06, vf05); //ASSERT(false);
  // nop                        |  opmula.xyz ACC, vf10, vf11     10
  c->vopmula(vf10, vf11); // ASSERT(false);
  // nop                        |  opmsub.xyz vf10, vf11, vf10    11
  c->vopmsub(vf10, vf11, vf10); // ASSERT(false);
  // nop                        |  opmula.xyz ACC, vf15, vf16     12
  c->vopmula(vf15, vf16); // ASSERT(false);
  // nop                        |  opmsub.xyz vf15, vf16, vf15    13
  c->vopmsub(vf15, vf16, vf15); // ASSERT(false);
  // nop                        |  opmula.xyz ACC, vf20, vf21     14
  c->vopmula(vf20, vf21); // ASSERT(false);
  // nop                        |  opmsub.xyz vf20, vf21, vf20    15
  c->vopmsub(vf20, vf21, vf20); // ASSERT(false);
  // nop                        |  mul.xyz vf05, vf05, vf01       16
  c->vfs[vf05].vf.mul(Mask::xyz, c->vf_src(vf05).vf, c->vf_src(vf01).vf);
  // nop                        |  mul.xyz vf10, vf10, vf01       17
  c->vfs[vf10].vf.mul(Mask::xyz, c->vf_src(vf10).vf, c->vf_src(vf01).vf);
  // nop                        |  mul.xyz vf15, vf15, vf01       18
  c->vfs[vf15].vf.mul(Mask::xyz, c->vf_src(vf15).vf, c->vf_src(vf01).vf);
  // nop                        |  mul.xyz vf20, vf20, vf01       19
  c->vfs[vf20].vf.mul(Mask::xyz, c->vf_src(vf20).vf, c->vf_src(vf01).vf);
  // nop                        |  addx.y vf05, vf05, vf05        20
  c->vfs[vf05].vf.add(Mask::y, c->vf_src(vf05).vf, c->vf_src(vf05).vf.x());
  // nop                        |  addx.y vf10, vf10, vf10        21
  c->vfs[vf10].vf.add(Mask::y, c->vf_src(vf10).vf, c->vf_src(vf10).vf.x());
  // nop                        |  addx.y vf15, vf15, vf15        22
  c->vfs[vf15].vf.add(Mask::y, c->vf_src(vf15).vf, c->vf_src(vf15).vf.x());
  // nop                        |  addx.y vf20, vf20, vf20        23
  c->vfs[vf20].vf.add(Mask::y, c->vf_src(vf20).vf, c->vf_src(vf20).vf.x());
  // nop                        |  addz.y vf22, vf05, vf05        24
  c->vfs[vf22].vf.add(Mask::y, c->vf_src(vf05).vf, c->vf_src(vf05).vf.z());
  // nop                        |  addz.y vf23, vf10, vf10        25
  c->vfs[vf23].vf.add(Mask::y, c->vf_src(vf10).vf, c->vf_src(vf10).vf.z());
  // nop                        |  addz.y vf24, vf15, vf15 :e     26
  c->vfs[vf24].vf.add(Mask::y, c->vf_src(vf15).vf, c->vf_src(vf15).vf.z());
  // nop                        |  addz.y vf25, vf20, vf20        27
  c->vfs[vf25].vf.add(Mask::y, c->vf_src(vf20).vf, c->vf_src(vf20).vf.z());
}

void exec_28(ExecutionContext* c) {
// nop                        |  mul.xyzw vf27, vf20, Q         28
  c->vfs[vf27].vf.mul(Mask::xyzw, c->vf_src(vf20).vf, c->Q);
  // div Q, vf13.x, vf17.x      |  sub.xyzw vf19, vf01, vf03      29
  c->vfs[vf19].vf.sub(Mask::xyzw, c->vf_src(vf01).vf, c->vf_src(vf03).vf);   c->Q = c->vfs[vf13].vf.x() / c->vfs[vf17].vf.x();
  // move.xyzw vf23, vf07       |  sub.xyzw vf20, vf01, vf04      30
  c->vfs[vf20].vf.sub(Mask::xyzw, c->vf_src(vf01).vf, c->vf_src(vf04).vf);   c->vfs[vf23].vf.move(Mask::xyzw, c->vf_src(vf07).vf);
  // nop                        |  sub.xyzw vf21, vf01, vf05      31
  c->vfs[vf21].vf.sub(Mask::xyzw, c->vf_src(vf01).vf, c->vf_src(vf05).vf);
  // move.xyzw vf25, vf09       |  sub.xyzw vf22, vf01, vf06      32
  c->vfs[vf22].vf.sub(Mask::xyzw, c->vf_src(vf01).vf, c->vf_src(vf06).vf);   c->vfs[vf25].vf.move(Mask::xyzw, c->vf_src(vf09).vf);
  // move.xyzw vf26, vf10       |  sub.xyzw vf24, vf08, vf27      33
  c->vfs[vf24].vf.sub(Mask::xyzw, c->vf_src(vf08).vf, c->vf_src(vf27).vf);   c->vfs[vf26].vf.move(Mask::xyzw, c->vf_src(vf10).vf);
  // nop                        |  mul.xyzw vf11, vf03, vf02      34
  c->vfs[vf11].vf.mul(Mask::xyzw, c->vf_src(vf03).vf, c->vf_src(vf02).vf);
  // nop                        |  mul.xyz vf15, vf19, vf02       35
  c->vfs[vf15].vf.mul(Mask::xyz, c->vf_src(vf19).vf, c->vf_src(vf02).vf);
  // div Q, vf14.x, vf18.x      |  mul.xyzw vf12, vf04, vf02      36
  float oldQ = c->Q;
  c->vfs[vf12].vf.mul(Mask::xyzw, c->vf_src(vf04).vf, c->vf_src(vf02).vf);   c->Q = c->vfs[vf14].vf.x() / c->vfs[vf18].vf.x();
  // move.xyzw vf07, vf03       |  mul.xyzw vf28, vf28, Q         37
  c->vfs[vf28].vf.mul(Mask::xyzw, c->vf_src(vf28).vf, oldQ);   c->vfs[vf07].vf.move(Mask::xyzw, c->vf_src(vf03).vf);
  // move.xyzw vf08, vf04       |  mul.xyz vf16, vf20, vf02       38
  c->vfs[vf16].vf.mul(Mask::xyz, c->vf_src(vf20).vf, c->vf_src(vf02).vf);   c->vfs[vf08].vf.move(Mask::xyzw, c->vf_src(vf04).vf);
  // move.xyzw vf09, vf05       |  addy.x vf11, vf11, vf11        39
  c->vfs[vf11].vf.add(Mask::x, c->vf_src(vf11).vf, c->vf_src(vf11).vf.y());   c->vfs[vf09].vf.move(Mask::xyzw, c->vf_src(vf05).vf);
  // move.xyzw vf10, vf06       |  addy.x vf15, vf15, vf15        40
  c->vfs[vf15].vf.add(Mask::x, c->vf_src(vf15).vf, c->vf_src(vf15).vf.y());   c->vfs[vf10].vf.move(Mask::xyzw, c->vf_src(vf06).vf);
  // nop                        |  sub.xyzw vf25, vf25, vf28      41
  c->vfs[vf25].vf.sub(Mask::xyzw, c->vf_src(vf25).vf, c->vf_src(vf28).vf);
  // nop                        |  addy.x vf12, vf12, vf12        42
  c->vfs[vf12].vf.add(Mask::x, c->vf_src(vf12).vf, c->vf_src(vf12).vf.y());
  // nop                        |  mul.xyzw vf29, vf29, Q         43
  c->vfs[vf29].vf.mul(Mask::xyzw, c->vf_src(vf29).vf, c->Q);
  // nop                        |  addy.x vf16, vf16, vf16        44
  c->vfs[vf16].vf.add(Mask::x, c->vf_src(vf16).vf, c->vf_src(vf16).vf.y());
  // nop                        |  addz.x vf11, vf11, vf11        45
  c->vfs[vf11].vf.add(Mask::x, c->vf_src(vf11).vf, c->vf_src(vf11).vf.z());
  // nop                        |  addz.x vf15, vf15, vf15        46
  c->vfs[vf15].vf.add(Mask::x, c->vf_src(vf15).vf, c->vf_src(vf15).vf.z());
  // nop                        |  sub.xyzw vf26, vf26, vf29      47
  c->vfs[vf26].vf.sub(Mask::xyzw, c->vf_src(vf26).vf, c->vf_src(vf29).vf);
  // nop                        |  addz.x vf12, vf12, vf12        48
  c->vfs[vf12].vf.add(Mask::x, c->vf_src(vf12).vf, c->vf_src(vf12).vf.z());
  // nop                        |  addz.x vf16, vf16, vf16        49
  c->vfs[vf16].vf.add(Mask::x, c->vf_src(vf16).vf, c->vf_src(vf16).vf.z());
  // nop                        |  addw.x vf11, vf11, vf11        50
  c->vfs[vf11].vf.add(Mask::x, c->vf_src(vf11).vf, c->vf_src(vf11).vf.w());
  // nop                        |  mul.xyzw vf13, vf09, vf02      51

  c->vfs[vf13].vf.mul(Mask::xyzw, c->vf_src(vf09).vf, c->vf_src(vf02).vf);
  c->vfs[vf13].vf.saturate_infs();

  // nop                        |  addw.x vf12, vf12, vf12        52
  c->vfs[vf12].vf.add(Mask::x, c->vf_src(vf12).vf, c->vf_src(vf12).vf.w());
  // nop                        |  mul.xyz vf17, vf21, vf02       53
  c->vfs[vf17].vf.mul(Mask::xyz, c->vf_src(vf21).vf, c->vf_src(vf02).vf);
  // nop                        |  mul.xyzw vf14, vf10, vf02      54
  c->vfs[vf14].vf.mul(Mask::xyzw, c->vf_src(vf10).vf, c->vf_src(vf02).vf);
  c->vfs[vf14].vf.saturate_infs();

  // div Q, vf11.x, vf15.x      |  mul.xyz vf18, vf22, vf02       55
  c->vfs[vf18].vf.mul(Mask::xyz, c->vf_src(vf22).vf, c->vf_src(vf02).vf);   c->Q = c->vfs[vf11].vf.x() / c->vfs[vf15].vf.x();
  // nop                        |  addy.x vf13, vf13, vf13        56
  c->vfs[vf13].vf.add(Mask::x, c->vf_src(vf13).vf, c->vf_src(vf13).vf.y());

  // nop                        |  addy.x vf17, vf17, vf17        57
  c->vfs[vf17].vf.add(Mask::x, c->vf_src(vf17).vf, c->vf_src(vf17).vf.y());
  // nop                        |  addy.x vf14, vf14, vf14        58
  c->vfs[vf14].vf.add(Mask::x, c->vf_src(vf14).vf, c->vf_src(vf14).vf.y());

  // nop                        |  addy.x vf18, vf18, vf18        59
  c->vfs[vf18].vf.add(Mask::x, c->vf_src(vf18).vf, c->vf_src(vf18).vf.y());
  // nop                        |  addz.x vf13, vf13, vf13        60
  c->vfs[vf13].vf.add(Mask::x, c->vf_src(vf13).vf, c->vf_src(vf13).vf.z());

  // nop                        |  addz.x vf17, vf17, vf17        61
  c->vfs[vf17].vf.add(Mask::x, c->vf_src(vf17).vf, c->vf_src(vf17).vf.z());
  // div Q, vf12.x, vf16.x      |  addz.x vf14, vf14, vf14        62
  oldQ = c->Q;
  c->vfs[vf14].vf.add(Mask::x, c->vf_src(vf14).vf, c->vf_src(vf14).vf.z());   c->Q = c->vfs[vf12].vf.x() / c->vfs[vf16].vf.x();

  // nop                        |  mul.xyzw vf19, vf19, Q         63
  c->vfs[vf19].vf.mul(Mask::xyzw, c->vf_src(vf19).vf, oldQ);
  // move.xyzw vf28, vf21       |  addz.x vf18, vf18, vf18        64
  c->vfs[vf18].vf.add(Mask::x, c->vf_src(vf18).vf, c->vf_src(vf18).vf.z());   c->vfs[vf28].vf.move(Mask::xyzw, c->vf_src(vf21).vf);
  // move.xyzw vf29, vf22       |  addw.x vf13, vf13, vf13        65
  c->vfs[vf13].vf.add(Mask::x, c->vf_src(vf13).vf, c->vf_src(vf13).vf.w());   c->vfs[vf29].vf.move(Mask::xyzw, c->vf_src(vf22).vf);
  c->vfs[vf13].vf.saturate_infs();

  // nop                        |  addw.x vf14, vf14, vf14 :e     66
  c->vfs[vf14].vf.add(Mask::x, c->vf_src(vf14).vf, c->vf_src(vf14).vf.w());
  c->vfs[vf14].vf.saturate_infs();

  // nop                        |  sub.xyzw vf07, vf07, vf19      67
  c->vfs[vf07].vf.sub(Mask::xyzw, c->vf_src(vf07).vf, c->vf_src(vf19).vf);
}

void exec_68(ExecutionContext* c) {
  // nop                        |  mul.xyzw vf27, vf20, Q         68
  c->vfs[vf27].vf.mul(Mask::xyzw, c->vf_src(vf20).vf, c->Q);
  // div Q, vf13.x, vf17.x      |  nop                            69
  c->Q = c->vfs[vf13].vf.x() / c->vfs[vf17].vf.x();
  // move.xyzw vf23, vf07       |  nop                            70
  c->vfs[vf23].vf.move(Mask::xyzw, c->vf_src(vf07).vf);
  // nop                        |  nop                            71

  // move.xyzw vf25, vf09       |  nop                            72
  c->vfs[vf25].vf.move(Mask::xyzw, c->vf_src(vf09).vf);
  // move.xyzw vf26, vf10       |  sub.xyzw vf24, vf08, vf27      73
  c->vfs[vf24].vf.sub(Mask::xyzw, c->vf_src(vf08).vf, c->vf_src(vf27).vf);   c->vfs[vf26].vf.move(Mask::xyzw, c->vf_src(vf10).vf);
  // nop                        |  nop                            74

  // nop                        |  nop                            75

  // div Q, vf14.x, vf18.x      |  nop                            76
  float oldQ = c->Q;
  c->Q = c->vfs[vf14].vf.x() / c->vfs[vf18].vf.x();
  // nop                        |  mul.xyzw vf28, vf28, Q         77
  c->vfs[vf28].vf.mul(Mask::xyzw, c->vf_src(vf28).vf, oldQ);
  // nop                        |  nop                            78

  // nop                        |  nop                            79

  // nop                        |  nop                            80

  // nop                        |  sub.xyzw vf25, vf25, vf28      81
  c->vfs[vf25].vf.sub(Mask::xyzw, c->vf_src(vf25).vf, c->vf_src(vf28).vf); // was bad
  // nop                        |  nop                            82

  // nop                        |  mul.xyzw vf29, vf29, Q         83
  c->vfs[vf29].vf.mul(Mask::xyzw, c->vf_src(vf29).vf, c->Q);
  // nop                        |  nop                            84

  // nop                        |  nop                            85

  // nop                        |  nop :e                         86

  // nop                        |  sub.xyzw vf26, vf26, vf29      87
  c->vfs[vf26].vf.sub(Mask::xyzw, c->vf_src(vf26).vf, c->vf_src(vf29).vf);

}
} // namespace
} // namespace Mips2C::jak2

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace shadow_xform_verts {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 68, a1);                                // lw v1, 68(a1)
  // nop                                            // sll r0, r0, 0
  c->lw(a2, 20, a0);                                // lw a2, 20(a0)
  c->dsll(v1, v1, 4);                               // dsll v1, v1, 4
  c->lw(t0, 24, a0);                                // lw t0, 24(a0)
  c->daddu(a2, a2, a0);                             // daddu a2, a2, a0
  c->lh(a3, 8, a0);                                 // lh a3, 8(a0)
  c->daddu(t0, t0, a0);                             // daddu t0, t0, a0
  // nop                                            // sll r0, r0, 0
  c->daddu(v1, a0, v1);                             // daddu v1, a0, v1
  c->sw(a2, 0, a1);                                 // sw a2, 0(a1)
  c->daddiu(v1, v1, 144);                           // daddiu v1, v1, 144
  // nop                                            // sll r0, r0, 0
  c->mov64(a1, t0);                                 // or a1, t0, r0
  c->lh(t0, 10, a0);                                // lh t0, 10(a0)
  c->mov64(a2, a2);                                 // or a2, a2, r0
  // nop                                            // sll r0, r0, 0
  c->dsubu(a3, a3, t0);                             // dsubu a3, a3, t0
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L119
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely


block_1:
  c->daddiu(a3, a3, -1);                            // daddiu a3, a3, -1
  c->lbu(t0, 0, a1);                                // lbu t0, 0(a1)
  // nop                                            // sll r0, r0, 0
  c->lbu(t1, 1, a1);                                // lbu t1, 1(a1)
  // nop                                            // sll r0, r0, 0
  c->daddiu(a1, a1, 2);                             // daddiu a1, a1, 2
  c->dsll(t0, t0, 7);                               // dsll t0, t0, 7
  // nop                                            // sll r0, r0, 0
  c->daddu(t0, t0, v1);                             // daddu t0, t0, v1
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf1, 0, t0);                              // lqc2 vf1, 0(t0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 16, t0);                             // lqc2 vf2, 16(t0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 32, t0);                             // lqc2 vf3, 32(t0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf4, 48, t0);                             // lqc2 vf4, 48(t0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf9, 0, a2);                              // lqc2 vf9, 0(a2)
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf9);        // vmaddax.xyzw acc, vf1, vf9
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf9);        // vmadday.xyzw acc, vf2, vf9
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyz, BC::z, vf9, vf3, vf9);     // vmaddz.xyz vf9, vf3, vf9
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf9, 0, a2);                              // sqc2 vf9, 0(a2)
  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L118
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely


block_2:
  // nop                                            // sll r0, r0, 0
  c->lh(a0, 10, a0);                                // lh a0, 10(a0)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L121
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_5;}                           // branch non-likely


block_3:
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->lbu(t0, 0, a1);                                // lbu t0, 0(a1)
  // nop                                            // sll r0, r0, 0
  c->lbu(a3, 1, a1);                                // lbu a3, 1(a1)
  c->dsll(t0, t0, 7);                               // dsll t0, t0, 7
  c->daddiu(a1, a1, 2);                             // daddiu a1, a1, 2
  c->dsll(a3, a3, 7);                               // dsll a3, a3, 7
  c->daddu(t0, t0, v1);                             // daddu t0, t0, v1
  c->daddu(a3, a3, v1);                             // daddu a3, a3, v1
  c->lqc2(vf1, 0, t0);                              // lqc2 vf1, 0(t0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 16, t0);                             // lqc2 vf2, 16(t0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 32, t0);                             // lqc2 vf3, 32(t0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf4, 48, t0);                             // lqc2 vf4, 48(t0)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf9, 0, a2);                              // lqc2 vf9, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf5, 0, a3);                              // lqc2 vf5, 0(a3)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf6, 16, a3);                             // lqc2 vf6, 16(a3)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf7, 32, a3);                             // lqc2 vf7, 32(a3)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf8, 48, a3);                             // lqc2 vf8, 48(a3)
  c->vsub_bc(DEST::w, BC::w, vf10, vf0, vf9);       // vsubw.w vf10, vf0, vf9
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf4, vf0);         // vmulaw.xyzw acc, vf4, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf9);        // vmaddax.xyzw acc, vf1, vf9
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf9);        // vmadday.xyzw acc, vf2, vf9
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyz, BC::z, vf10, vf3, vf9);    // vmaddz.xyz vf10, vf3, vf9
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf8, vf0);         // vmulaw.xyzw acc, vf8, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf5, vf9);        // vmaddax.xyzw acc, vf5, vf9
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf6, vf9);        // vmadday.xyzw acc, vf6, vf9
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyz, BC::z, vf9, vf7, vf9);     // vmaddz.xyz vf9, vf7, vf9
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyz, BC::w, vf10, vf9);         // vmulaw.xyz acc, vf10, vf9
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyz, BC::w, vf9, vf9, vf10);    // vmaddw.xyz vf9, vf9, vf10
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::w, BC::x, vf9, vf0, vf0);        // vaddx.w vf9, vf0, vf0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf9, 0, a2);                              // sqc2 vf9, 0(a2)
  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L120
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_3;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0

block_5:
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
  gLinkedFunctionTable.reg("shadow-xform-verts", execute, 128);
}

} // namespace shadow_xform_verts
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace shadow_calc_dual_verts {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 16, a1);                                // lw v1, 16(a1)
  // nop                                            // sll r0, r0, 0
  c->lw(a2, 0, a1);                                 // lw a2, 0(a1)
  c->daddiu(v1, v1, 15);                            // daddiu v1, v1, 15
  c->lqc2(vf1, 80, a1);                             // lqc2 vf1, 80(a1)
  c->dsra(v1, v1, 4);                               // dsra v1, v1, 4
  c->lqc2(vf2, 96, a1);                             // lqc2 vf2, 96(a1)
  c->dsll(a3, v1, 4);                               // dsll a3, v1, 4
  c->lh(a0, 8, a0);                                 // lh a0, 8(a0)
  c->mov64(v1, a3);                                 // or v1, a3, r0
  c->sw(a3, 44, a1);                                // sw a3, 44(a1)
  c->mov64(a2, a2);                                 // or a2, a2, r0
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L116
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_9;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lq(a3, 0, a2);                                 // lq a3, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t0, 16, a2);                                // lq t0, 16(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t1, 32, a2);                                // lq t1, 32(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 48, a2);                                // lq t2, 48(a2)
  c->daddiu(a2, a2, 64);                            // daddiu a2, a2, 64
  c->mov128_vf_gpr(vf3, a3);                        // qmtc2.i vf3, a3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf4, t0);                        // qmtc2.ni vf4, t0
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf5, t1);                        // qmtc2.ni vf5, t1
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf6, t2);                        // qmtc2.ni vf6, t2
  exec_28(c);                                       // vcallms 28
  // nop                                            // sll r0, r0, 0
  c->daddiu(a0, a0, -4);                            // daddiu a0, a0, -4
  c->lq(a3, 0, a2);                                 // lq a3, 0(a2)
  bc = ((s64)c->sgpr64(a0)) <= 0;                   // blez a0, L114
  c->lq(t0, 16, a2);                                // lq t0, 16(a2)
  if (bc) {goto block_4;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lq(t1, 32, a2);                                // lq t1, 32(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 48, a2);                                // lq t2, 48(a2)
  c->daddiu(a2, a2, 64);                            // daddiu a2, a2, 64
  c->mov128_vf_gpr(vf3, a3);                        // qmtc2.i vf3, a3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf4, t0);                        // qmtc2.ni vf4, t0
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf5, t1);                        // qmtc2.ni vf5, t1
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf6, t2);                        // qmtc2.ni vf6, t2

block_3:
  exec_28(c);                                       // vcallms 28
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
  c->mov128_gpr_vf(a3, vf23);                       // qmfc2.i a3, vf23
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 0, v1);                                 // sq a3, 0(v1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a3, vf24);                       // qmfc2.ni a3, vf24
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 16, v1);                                // sq a3, 16(v1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a3, vf25);                       // qmfc2.ni a3, vf25
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 32, v1);                                // sq a3, 32(v1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a3, vf26);                       // qmfc2.ni a3, vf26
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 48, v1);                                // sq a3, 48(v1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lq(a3, 0, a2);                                 // lq a3, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t0, 16, a2);                                // lq t0, 16(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(t1, 32, a2);                                // lq t1, 32(a2)
  c->daddiu(a0, a0, -4);                            // daddiu a0, a0, -4
  c->lq(t2, 48, a2);                                // lq t2, 48(a2)
  c->daddiu(a2, a2, 64);                            // daddiu a2, a2, 64
  c->daddiu(v1, v1, 64);                            // daddiu v1, v1, 64
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf3, a3);                        // qmtc2.ni vf3, a3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf4, t0);                        // qmtc2.ni vf4, t0
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf5, t1);                        // qmtc2.ni vf5, t1
  bc = ((s64)c->sgpr64(a0)) > 0;                    // bgtz a0, L113
  c->mov128_vf_gpr(vf6, t2);                        // qmtc2.ni vf6, t2
  if (bc) {goto block_3;}                           // branch non-likely


block_4:
  exec_68(c);                                       // vcallms 68
  // nop                                            // sll r0, r0, 0
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  c->daddiu(a2, a0, 3);                             // daddiu a2, a0, 3
  c->mov128_gpr_vf(a3, vf23);                       // qmfc2.i a3, vf23
  c->daddiu(t0, a0, 2);                             // daddiu t0, a0, 2
  c->mov128_gpr_vf(t1, vf24);                       // qmfc2.i t1, vf24
  c->daddiu(t2, a0, 1);                             // daddiu t2, a0, 1
  c->mov128_gpr_vf(t3, vf25);                       // qmfc2.i t3, vf25
  c->daddiu(a0, a0, 4);                             // daddiu a0, a0, 4
  c->mov128_gpr_vf(t4, vf26);                       // qmfc2.i t4, vf26
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L115
  c->sq(a3, 0, v1);                                 // sq a3, 0(v1)
  if (bc) {goto block_8;}                           // branch non-likely

  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L115
  c->sq(t1, 16, v1);                                // sq t1, 16(v1)
  if (bc) {goto block_8;}                           // branch non-likely

  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L115
  c->sq(t3, 32, v1);                                // sq t3, 32(v1)
  if (bc) {goto block_8;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->sq(t4, 48, v1);                                // sq t4, 48(v1)

block_8:
  c->dsll(a0, a0, 4);                               // dsll a0, a0, 4
  // nop                                            // sll r0, r0, 0
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  // nop                                            // sll r0, r0, 0

block_9:
  c->sw(v1, 16, a1);                                // sw v1, 16(a1)
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
  gLinkedFunctionTable.reg("shadow-calc-dual-verts", execute, 128);
}

} // namespace shadow_calc_dual_verts
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace shadow_scissor_edges {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lw(a3, 44, a1);                                // lw a3, 44(a1)
  // nop                                            // sll r0, r0, 0
  c->lw(a2, 0, a1);                                 // lw a2, 0(a1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 128, a1);                            // lqc2 vf3, 128(a1)
  // nop                                            // sll r0, r0, 0
  c->lh(a0, 8, a0);                                 // lh a0, 8(a0)
  c->mov64(a1, a3);                                 // or a1, a3, r0
  c->lw(a3, 44, v1);                                // lw a3, 44(v1)
  c->mov64(a2, a2);                                 // or a2, a2, r0
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L111
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely


block_1:
  c->lqc2(vf1, 0, a2);                              // lqc2 vf1, 0(a2)
  c->lqc2(vf2, 0, a1);                              // lqc2 vf2, 0(a1)
  c->vadd_bc(DEST::z, BC::w, vf7, vf1, vf3);        // vaddw.z vf7, vf1, vf3
  c->vadd_bc(DEST::z, BC::w, vf8, vf2, vf3);        // vaddw.z vf8, vf2, vf3
  c->vsub_bc(DEST::z, BC::z, vf6, vf1, vf2);        // vsubz.z vf6, vf1, vf2
  c->vadd_bc(DEST::z, BC::w, vf5, vf1, vf3);        // vaddw.z vf5, vf1, vf3
  c->vadd_bc(DEST::y, BC::z, vf7, vf0, vf7);        // vaddz.y vf7, vf0, vf7
  c->vadd_bc(DEST::y, BC::z, vf8, vf0, vf8);        // vaddz.y vf8, vf0, vf8
  c->vsub(DEST::xyz, vf4, vf2, vf1);                // vsub.xyz vf4, vf2, vf1
  c->mov128_gpr_vf(t0, vf7);                        // qmfc2.i t0, vf7
  c->mov128_gpr_vf(t1, vf8);                        // qmfc2.i t1, vf8
  bc = ((s64)c->sgpr64(t0)) < 0;                    // bltz t0, L109
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_4;}                           // branch non-likely

  bc = ((s64)c->sgpr64(t1)) > 0;                    // bgtz t1, L110
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_5;}                           // branch non-likely

  //beq r0, r0, L110                                // beq r0, r0, L110
  c->daddiu(a3, a3, 1);                             // daddiu a3, a3, 1
  goto block_5;                                     // branch always


block_4:
  bc = ((s64)c->sgpr64(t1)) < 0;                    // bltz t1, L110
  c->daddiu(a3, a3, 1);                             // daddiu a3, a3, 1
  if (bc) {goto block_5;}                           // branch non-likely


block_5:
  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L108
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->sw(a3, 44, v1);                                // sw a3, 44(v1)

block_7:
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
  gLinkedFunctionTable.reg("shadow-scissor-edges", execute, 128);
}

} // namespace shadow_scissor_edges
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace shadow_scissor_top {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  // nop                                            // sll r0, r0, 0
  c->lw(a2, 44, a1);                                // lw a2, 44(a1)
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 0, a1);                                 // lw v1, 0(a1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf3, 112, a1);                            // lqc2 vf3, 112(a1)
  // nop                                            // sll r0, r0, 0
  c->lh(a0, 8, a0);                                 // lh a0, 8(a0)
  c->mov64(a1, a2);                                 // or a1, a2, r0
  // nop                                            // sll r0, r0, 0
  c->mov64(v1, v1);                                 // or v1, v1, r0
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L106
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_4;}                           // branch non-likely


block_1:
  c->lqc2(vf1, 0, v1);                              // lqc2 vf1, 0(v1)
  c->lqc2(vf2, 0, a1);                              // lqc2 vf2, 0(a1)
  c->vsub(DEST::xyzw, vf4, vf2, vf1);               // vsub.xyzw vf4, vf2, vf1
  c->vmul(DEST::xyzw, vf5, vf1, vf3);               // vmul.xyzw vf5, vf1, vf3
  c->vmul(DEST::xyz, vf6, vf4, vf3);                // vmul.xyz vf6, vf4, vf3
  c->vadd_bc(DEST::y, BC::x, vf5, vf5, vf5);        // vaddx.y vf5, vf5, vf5
  c->vadd_bc(DEST::x, BC::y, vf6, vf6, vf6);        // vaddy.x vf6, vf6, vf6
  c->vadd_bc(DEST::y, BC::z, vf5, vf5, vf5);        // vaddz.y vf5, vf5, vf5
  c->vadd_bc(DEST::x, BC::z, vf6, vf6, vf6);        // vaddz.x vf6, vf6, vf6
  c->vadd_bc(DEST::y, BC::w, vf5, vf5, vf5);        // vaddw.y vf5, vf5, vf5
  c->mov128_gpr_vf(a2, vf5);                        // qmfc2.i a2, vf5
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(a2)) < 0;                    // bltz a2, L105
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_3;}                           // branch non-likely

  c->vdiv(vf5, BC::y, vf6, BC::x);                  // vdiv Q, vf5.y, vf6.x
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyzw, vf4, vf4);                   // vmulq.xyzw vf4, vf4, Q
  c->vsub(DEST::xyzw, vf1, vf1, vf4);               // vsub.xyzw vf1, vf1, vf4
  c->sqc2(vf1, 0, v1);                              // sqc2 vf1, 0(v1)

block_3:
  c->daddiu(v1, v1, 16);                            // daddiu v1, v1, 16
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L104
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely


block_4:
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
  gLinkedFunctionTable.reg("shadow-scissor-top", execute, 128);
}

} // namespace shadow_scissor_top
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace shadow_init_vars {
struct Cache {
  void* math_camera; // *math-camera*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  c->load_symbol2(v1, cache.math_camera);           // lw v1, *math-camera*(s7)
  c->mov64(v1, v1);                                 // or v1, v1, r0
  c->lqc2(vf7, 364, v1);                            // lqc2 vf7, 364(v1)
  c->lqc2(vf8, 380, v1);                            // lqc2 vf8, 380(v1)
  c->lqc2(vf9, 396, v1);                            // lqc2 vf9, 396(v1)
  c->lqc2(vf10, 412, v1);                           // lqc2 vf10, 412(v1)
  c->lqc2(vf1, 144, a1);                            // lqc2 vf1, 144(a1)
  c->lqc2(vf11, 96, a1);                            // lqc2 vf11, 96(a1)
  c->lqc2(vf12, 112, a1);                           // lqc2 vf12, 112(a1)
  c->lqc2(vf2, 80, a1);                             // lqc2 vf2, 80(a1)
  c->vmula_bc(DEST::xyzw, BC::x, vf7, vf1);         // vmulax.xyzw acc, vf7, vf1
  c->vmadda_bc(DEST::xyzw, BC::y, vf8, vf1);        // vmadday.xyzw acc, vf8, vf1
  c->vmadd_bc(DEST::xyzw, BC::z, vf1, vf9, vf1);    // vmaddz.xyzw vf1, vf9, vf1
  c->vmula_bc(DEST::xyzw, BC::x, vf7, vf11);        // vmulax.xyzw acc, vf7, vf11
  c->vmadda_bc(DEST::xyzw, BC::y, vf8, vf11);       // vmadday.xyzw acc, vf8, vf11
  c->vmadd_bc(DEST::xyz, BC::z, vf11, vf9, vf11);   // vmaddz.xyz vf11, vf9, vf11
  c->vmula_bc(DEST::xyzw, BC::x, vf7, vf12);        // vmulax.xyzw acc, vf7, vf12
  c->vmadda_bc(DEST::xyzw, BC::y, vf8, vf12);       // vmadday.xyzw acc, vf8, vf12
  c->vmadd_bc(DEST::xyz, BC::z, vf12, vf9, vf12);   // vmaddz.xyz vf12, vf9, vf12
  c->vmul(DEST::xyzw, vf13, vf10, vf11);            // vmul.xyzw vf13, vf10, vf11
  c->vmula_bc(DEST::xyzw, BC::w, vf10, vf0);        // vmulaw.xyzw acc, vf10, vf0
  c->vmadda_bc(DEST::xyzw, BC::x, vf7, vf2);        // vmaddax.xyzw acc, vf7, vf2
  c->vmul(DEST::xyzw, vf14, vf10, vf12);            // vmul.xyzw vf14, vf10, vf12
  c->vsub_bc(DEST::w, BC::x, vf13, vf13, vf13);     // vsubx.w vf13, vf13, vf13
  c->vsub_bc(DEST::w, BC::x, vf14, vf14, vf14);     // vsubx.w vf14, vf14, vf14
  c->vmadda_bc(DEST::xyzw, BC::y, vf8, vf2);        // vmadday.xyzw acc, vf8, vf2
  c->vmadd_bc(DEST::xyzw, BC::z, vf2, vf9, vf2);    // vmaddz.xyzw vf2, vf9, vf2
  c->vsub_bc(DEST::w, BC::y, vf13, vf13, vf13);     // vsuby.w vf13, vf13, vf13
  c->vsub_bc(DEST::w, BC::y, vf14, vf14, vf14);     // vsuby.w vf14, vf14, vf14
  c->vsub_bc(DEST::w, BC::z, vf11, vf13, vf13);     // vsubz.w vf11, vf13, vf13
  c->vsub_bc(DEST::w, BC::z, vf12, vf14, vf14);     // vsubz.w vf12, vf14, vf14
  c->sqc2(vf2, 80, a1);                             // sqc2 vf2, 80(a1)
  c->sqc2(vf1, 144, a1);                            // sqc2 vf1, 144(a1)
  c->sqc2(vf11, 96, a1);                            // sqc2 vf11, 96(a1)
  c->sqc2(vf12, 112, a1);                           // sqc2 vf12, 112(a1)
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
  cache.math_camera = intern_from_c("*math-camera*").c();
  gLinkedFunctionTable.reg("shadow-init-vars", execute, 128);
}

} // namespace shadow_init_vars
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace shadow_find_facing_single_tris {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->daddiu(sp, sp, -64);                           // daddiu sp, sp, -64
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s4, 16, sp);                                // sq s4, 16(sp)
  c->sq(s5, 32, sp);                                // sq s5, 32(sp)
  c->sq(gp, 48, sp);                                // sq gp, 48(sp)
  c->lw(v1, 16, a1);                                // lw v1, 16(a1)
  c->lh(t0, 12, a0);                                // lh t0, 12(a0)
  c->mov64(a2, v1);                                 // or a2, v1, r0
  c->lw(a3, 28, a0);                                // lw a3, 28(a0)
  c->daddu(a0, a3, a0);                             // daddu a0, a3, a0
  c->mov64(a3, a0);                                 // or a3, a0, r0
  c->lqc2(vf2, 80, a1);                             // lqc2 vf2, 80(a1)
  c->lqc2(vf1, 144, a1);                            // lqc2 vf1, 144(a1)
  c->lqc2(vf11, 96, a1);                            // lqc2 vf11, 96(a1)
  c->lw(a0, 0, a1);                                 // lw a0, 0(a1)
  c->pextlw(a0, a0, a0);                            // pextlw a0, a0, a0
  c->pextlw(a0, a0, a0);                            // pextlw a0, a0, a0
  c->daddiu(t0, t0, -4);                            // daddiu t0, t0, -4
  c->addiu(t1, r0, 1);                              // addiu t1, r0, 1
  bc = ((s64)c->sgpr64(t0)) < 0;                    // bltz t0, L98
  c->daddiu(t0, t0, 4);                             // daddiu t0, t0, 4
  if (bc) {goto block_11;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lq(t3, 0, a3);                                 // lq t3, 0(a3)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->pextub(t2, r0, t3);                            // pextub t2, r0, t3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(t3, r0, t3);                            // pextlb t3, r0, t3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psllh(t2, t2, 4);                              // psllh t2, t2, 4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psllh(t4, t3, 4);                              // psllh t4, t3, 4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextuh(t3, r0, t4);                            // pextuh t3, r0, t4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(t4, r0, t4);                            // pextlh t4, r0, t4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextuh(t7, r0, t2);                            // pextuh t7, r0, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(t5, r0, t2);                            // pextlh t5, r0, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->paddw(t6, t4, a0);                             // paddw t6, t4, a0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyud(t4, t6, r0);                            // pcpyud t4, t6, r0
  c->lq(t2, 0, t6);                                 // lq t2, 0(t6)
  c->paddw(t8, t3, a0);                             // paddw t8, t3, a0
  c->lq(t3, 0, t4);                                 // lq t3, 0(t4)
  c->pcpyud(t9, t8, r0);                            // pcpyud t9, t8, r0
  c->lq(t4, 0, t8);                                 // lq t4, 0(t8)
  c->dsra32(t6, t6, 0);                             // dsra32 t6, t6, 0
  c->dsra32(t8, t8, 0);                             // dsra32 t8, t8, 0
  c->paddw(s5, t5, a0);                             // paddw s5, t5, a0
  c->lq(t5, 0, t9);                                 // lq t5, 0(t9)
  c->pcpyud(t9, s5, r0);                            // pcpyud t9, s5, r0
  c->lq(t6, 0, t6);                                 // lq t6, 0(t6)
  c->paddw(gp, t7, a0);                             // paddw gp, t7, a0
  c->lq(t7, 0, t8);                                 // lq t7, 0(t8)
  c->pcpyud(ra, gp, r0);                            // pcpyud ra, gp, r0
  c->lq(t8, 0, s5);                                 // lq t8, 0(s5)
  c->dsra32(s5, s5, 0);                             // dsra32 s5, s5, 0
  c->dsra32(s4, gp, 0);                             // dsra32 s4, gp, 0
  // nop                                            // sll r0, r0, 0
  c->lq(s5, 0, s5);                                 // lq s5, 0(s5)
  // nop                                            // sll r0, r0, 0
  c->lq(t9, 0, t9);                                 // lq t9, 0(t9)
  // nop                                            // sll r0, r0, 0
  c->lq(gp, 0, gp);                                 // lq gp, 0(gp)
  // nop                                            // sll r0, r0, 0
  c->lq(s4, 0, s4);                                 // lq s4, 0(s4)
  // nop                                            // sll r0, r0, 0
  c->lq(ra, 0, ra);                                 // lq ra, 0(ra)
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf2, t2);                        // qmtc2.ni vf2, t2
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf3, t6);                        // qmtc2.ni vf3, t6
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf4, t3);                        // qmtc2.ni vf4, t3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf7, t4);                        // qmtc2.ni vf7, t4
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf8, t7);                        // qmtc2.ni vf8, t7
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf9, t5);                        // qmtc2.ni vf9, t5
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf12, t8);                       // qmtc2.ni vf12, t8
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf13, s5);                       // qmtc2.ni vf13, s5
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf14, t9);                       // qmtc2.ni vf14, t9
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf17, gp);                       // qmtc2.ni vf17, gp
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf18, s4);                       // qmtc2.ni vf18, s4
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf19, ra);                       // qmtc2.ni vf19, ra

block_2:
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 16, a3);                                // lq t3, 16(a3)
  c->daddiu(t0, t0, -4);                            // daddiu t0, t0, -4
  exec_0(c);                                        // vcallms 0
  c->pextub(t2, r0, t3);                            // pextub t2, r0, t3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(t3, r0, t3);                            // pextlb t3, r0, t3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psllh(t2, t2, 4);                              // psllh t2, t2, 4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psllh(t4, t3, 4);                              // psllh t4, t3, 4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextuh(t3, r0, t4);                            // pextuh t3, r0, t4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(t4, r0, t4);                            // pextlh t4, r0, t4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextuh(t7, r0, t2);                            // pextuh t7, r0, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(t5, r0, t2);                            // pextlh t5, r0, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->paddw(t6, t4, a0);                             // paddw t6, t4, a0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyud(t4, t6, r0);                            // pcpyud t4, t6, r0
  c->lq(t2, 0, t6);                                 // lq t2, 0(t6)
  c->paddw(t8, t3, a0);                             // paddw t8, t3, a0
  c->lq(t3, 0, t4);                                 // lq t3, 0(t4)
  c->pcpyud(t9, t8, r0);                            // pcpyud t9, t8, r0
  c->lq(t4, 0, t8);                                 // lq t4, 0(t8)
  c->dsra32(t6, t6, 0);                             // dsra32 t6, t6, 0
  c->dsra32(t8, t8, 0);                             // dsra32 t8, t8, 0
  c->paddw(s5, t5, a0);                             // paddw s5, t5, a0
  c->lq(t5, 0, t9);                                 // lq t5, 0(t9)
  c->pcpyud(t9, s5, r0);                            // pcpyud t9, s5, r0
  c->lq(t6, 0, t6);                                 // lq t6, 0(t6)
  c->paddw(gp, t7, a0);                             // paddw gp, t7, a0
  c->lq(t7, 0, t8);                                 // lq t7, 0(t8)
  c->pcpyud(ra, gp, r0);                            // pcpyud ra, gp, r0
  c->lq(t8, 0, s5);                                 // lq t8, 0(s5)
  c->dsra32(s5, s5, 0);                             // dsra32 s5, s5, 0
  c->dsra32(s4, gp, 0);                             // dsra32 s4, gp, 0
  // nop                                            // sll r0, r0, 0
  c->lq(s5, 0, s5);                                 // lq s5, 0(s5)
  // nop                                            // sll r0, r0, 0
  c->lq(t9, 0, t9);                                 // lq t9, 0(t9)
  // nop                                            // sll r0, r0, 0
  c->lq(gp, 0, gp);                                 // lq gp, 0(gp)
  // nop                                            // sll r0, r0, 0
  c->lq(s4, 0, s4);                                 // lq s4, 0(s4)
  // nop                                            // sll r0, r0, 0
  c->lq(ra, 0, ra);                                 // lq ra, 0(ra)
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf2, t2);                        // qmtc2.ni vf2, t2
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf3, t6);                        // qmtc2.ni vf3, t6
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf4, t3);                        // qmtc2.ni vf4, t3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf7, t4);                        // qmtc2.ni vf7, t4
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf8, t7);                        // qmtc2.ni vf8, t7
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf9, t5);                        // qmtc2.ni vf9, t5
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf12, t8);                       // qmtc2.ni vf12, t8
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf13, s5);                       // qmtc2.ni vf13, s5
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf14, t9);                       // qmtc2.ni vf14, t9
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf17, gp);                       // qmtc2.ni vf17, gp
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf18, s4);                       // qmtc2.ni vf18, s4
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf19, ra);                       // qmtc2.ni vf19, ra
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t3, vf22);                       // qmfc2.ni t3, vf22
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t4, vf23);                       // qmfc2.ni t4, vf23
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t2, vf24);                       // qmfc2.ni t2, vf24
  bc = ((s64)c->sgpr64(t3)) >= 0;                   // bgez t3, L94
  c->mov128_gpr_vf(t3, vf25);                       // qmfc2.ni t3, vf25
  if (bc) {goto block_4;}                           // branch non-likely

  c->sb(t1, 3, a3);                                 // sb t1, 3(a3)
  // nop                                            // sll r0, r0, 0
  c->sw(a3, 0, a2);                                 // sw a3, 0(a2)
  c->daddiu(a2, a2, 4);                             // daddiu a2, a2, 4

block_4:
  bc = ((s64)c->sgpr64(t4)) >= 0;                   // bgez t4, L95
  c->daddiu(a3, a3, 4);                             // daddiu a3, a3, 4
  if (bc) {goto block_6;}                           // branch non-likely

  c->sb(t1, 3, a3);                                 // sb t1, 3(a3)
  // nop                                            // sll r0, r0, 0
  c->sw(a3, 0, a2);                                 // sw a3, 0(a2)
  c->daddiu(a2, a2, 4);                             // daddiu a2, a2, 4

block_6:
  bc = ((s64)c->sgpr64(t2)) >= 0;                   // bgez t2, L96
  c->daddiu(a3, a3, 4);                             // daddiu a3, a3, 4
  if (bc) {goto block_8;}                           // branch non-likely

  c->sb(t1, 3, a3);                                 // sb t1, 3(a3)
  // nop                                            // sll r0, r0, 0
  c->sw(a3, 0, a2);                                 // sw a3, 0(a2)
  c->daddiu(a2, a2, 4);                             // daddiu a2, a2, 4

block_8:
  bc = ((s64)c->sgpr64(t3)) >= 0;                   // bgez t3, L97
  c->daddiu(a3, a3, 4);                             // daddiu a3, a3, 4
  if (bc) {goto block_10;}                          // branch non-likely

  c->sb(t1, 3, a3);                                 // sb t1, 3(a3)
  // nop                                            // sll r0, r0, 0
  c->sw(a3, 0, a2);                                 // sw a3, 0(a2)
  c->daddiu(a2, a2, 4);                             // daddiu a2, a2, 4

block_10:
  bc = ((s64)c->sgpr64(t0)) > 0;                    // bgtz t0, L93
  c->daddiu(a3, a3, 4);                             // daddiu a3, a3, 4
  if (bc) {goto block_2;}                           // branch non-likely


block_11:
  bc = ((s64)c->sgpr64(t0)) <= 0;                   // blez t0, L101
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_15;}                          // branch non-likely


block_12:
  c->lbu(t2, 0, a3);                                // lbu t2, 0(a3)
  c->lbu(t3, 1, a3);                                // lbu t3, 1(a3)
  c->lbu(t1, 2, a3);                                // lbu t1, 2(a3)
  c->dsll(t2, t2, 4);                               // dsll t2, t2, 4
  c->dsll(t3, t3, 4);                               // dsll t3, t3, 4
  c->dsll(t1, t1, 4);                               // dsll t1, t1, 4
  c->daddu(t2, t2, a0);                             // daddu t2, t2, a0
  c->daddu(t3, t3, a0);                             // daddu t3, t3, a0
  c->daddu(t1, t1, a0);                             // daddu t1, t1, a0
  c->lqc2(vf2, 0, t2);                              // lqc2 vf2, 0(t2)
  c->lqc2(vf3, 0, t3);                              // lqc2 vf3, 0(t3)
  c->lqc2(vf4, 0, t1);                              // lqc2 vf4, 0(t1)
  c->vsub(DEST::xyzw, vf5, vf3, vf2);               // vsub.xyzw vf5, vf3, vf2
  c->vsub(DEST::xyzw, vf6, vf4, vf2);               // vsub.xyzw vf6, vf4, vf2
  c->vopmula(vf5, vf6);                             // vopmula.xyz acc, vf5, vf6
  c->vopmsub(vf5, vf6, vf5);                        // vopmsub.xyz vf5, vf6, vf5
  c->vmul(DEST::xyz, vf5, vf5, vf1);                // vmul.xyz vf5, vf5, vf1
  c->vadd_bc(DEST::y, BC::x, vf5, vf5, vf5);        // vaddx.y vf5, vf5, vf5
  c->vadd_bc(DEST::y, BC::z, vf5, vf5, vf5);        // vaddz.y vf5, vf5, vf5
  c->mov128_gpr_vf(t1, vf5);                        // qmfc2.i t1, vf5
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(t1)) >= 0;                   // bgez t1, L100
  c->addiu(t1, r0, 1);                              // addiu t1, r0, 1
  if (bc) {goto block_14;}                          // branch non-likely

  c->sw(a3, 0, a2);                                 // sw a3, 0(a2)
  c->daddiu(a2, a2, 4);                             // daddiu a2, a2, 4
  c->sb(t1, 3, a3);                                 // sb t1, 3(a3)

block_14:
  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  bc = c->sgpr64(t0) != 0;                          // bne t0, r0, L99
  c->daddiu(a3, a3, 4);                             // daddiu a3, a3, 4
  if (bc) {goto block_12;}                          // branch non-likely


block_15:
  c->dsubu(a0, a2, v1);                             // dsubu a0, a2, v1
  c->dsra(a0, a0, 2);                               // dsra a0, a0, 2
  c->sw(a0, 20, a1);                                // sw a0, 20(a1)
  c->sw(v1, 32, a1);                                // sw v1, 32(a1)
  c->sw(a2, 16, a1);                                // sw a2, 16(a1)
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 48, sp);                                // lq gp, 48(sp)
  c->lq(s5, 32, sp);                                // lq s5, 32(sp)
  c->lq(s4, 16, sp);                                // lq s4, 16(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 64);                            // daddiu sp, sp, 64
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  gLinkedFunctionTable.reg("shadow-find-facing-single-tris", execute, 128);
}

} // namespace shadow_find_facing_single_tris
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace shadow_find_single_edges {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->lw(a2, 16, a1);                                // lw a2, 16(a1)
  c->lh(a3, 14, a0);                                // lh a3, 14(a0)
  c->mov64(v1, a2);                                 // or v1, a2, r0
  c->lw(t0, 32, a0);                                // lw t0, 32(a0)
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L91
  c->lw(t1, 28, a0);                                // lw t1, 28(a0)
  if (bc) {goto block_8;}                           // branch non-likely

  c->daddu(t0, t0, a0);                             // daddu t0, t0, a0
  c->sw(a2, 36, a1);                                // sw a2, 36(a1)
  c->daddu(a0, t1, a0);                             // daddu a0, t1, a0
  c->sw(t0, 4, a1);                                 // sw t0, 4(a1)
  c->mov64(t1, t0);                                 // or t1, t0, r0
  c->addiu(t2, r0, 255);                            // addiu t2, r0, 255
  // nop                                            // sll r0, r0, 0

block_2:
  c->daddiu(a3, a3, -1);                            // daddiu a3, a3, -1
  c->lbu(t4, 3, t1);                                // lbu t4, 3(t1)
  // nop                                            // sll r0, r0, 0
  c->lbu(t5, 2, t1);                                // lbu t5, 2(t1)
  bc = c->sgpr64(t4) == c->sgpr64(t2);              // beq t4, t2, L88
  c->gprs[t3].du64[0] = 0;                          // or t3, r0, r0
  if (bc) {goto block_5;}                           // branch non-likely

  c->dsll(t3, t5, 2);                               // dsll t3, t5, 2
  c->dsll(t4, t4, 2);                               // dsll t4, t4, 2
  c->daddu(t3, t3, a0);                             // daddu t3, t3, a0
  c->daddu(t5, t4, a0);                             // daddu t5, t4, a0
  // nop                                            // sll r0, r0, 0
  c->lbu(t4, 3, t3);                                // lbu t4, 3(t3)
  // nop                                            // sll r0, r0, 0
  c->lbu(t5, 3, t5);                                // lbu t5, 3(t5)
  c->sltiu(t3, t4, 1);                              // sltiu t3, t4, 1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t4) == c->sgpr64(t5);              // beq t4, t5, L90
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  //beq r0, r0, L89                                 // beq r0, r0, L89
  // nop                                            // sll r0, r0, 0
  goto block_6;                                     // branch always


block_5:
  c->dsll(t4, t5, 2);                               // dsll t4, t5, 2
  // nop                                            // sll r0, r0, 0
  c->daddu(t4, t4, a0);                             // daddu t4, t4, a0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lbu(t4, 3, t4);                                // lbu t4, 3(t4)
  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L90
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely


block_6:
  c->dsubu(t4, t1, t0);                             // dsubu t4, t1, t0
  c->sh(t3, 2, v1);                                 // sh t3, 2(v1)
  c->sh(t4, 0, v1);                                 // sh t4, 0(v1)
  c->daddiu(v1, v1, 4);                             // daddiu v1, v1, 4

block_7:
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L87
  c->daddiu(t1, t1, 4);                             // daddiu t1, t1, 4
  if (bc) {goto block_2;}                           // branch non-likely


block_8:
  c->dsubu(a0, v1, a2);                             // dsubu a0, v1, a2
  c->dsra(a0, a0, 2);                               // dsra a0, a0, 2
  c->sw(a0, 24, a1);                                // sw a0, 24(a1)
  c->sw(v1, 16, a1);                                // sw v1, 16(a1)
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
  gLinkedFunctionTable.reg("shadow-find-single-edges", execute, 128);
}

} // namespace shadow_find_single_edges
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace shadow_find_facing_double_tris {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->lh(a2, 16, a0);                                // lh a2, 16(a0)
  c->lw(v1, 36, a0);                                // lw v1, 36(a0)
  c->daddu(a0, v1, a0);                             // daddu a0, v1, a0
  c->mov64(v1, a0);                                 // or v1, a0, r0
  c->sw(a0, 12, a1);                                // sw a0, 12(a1)
  c->lqc2(vf1, 144, a1);                            // lqc2 vf1, 144(a1)
  c->lqc2(vf2, 80, a1);                             // lqc2 vf2, 80(a1)
  c->lqc2(vf11, 96, a1);                            // lqc2 vf11, 96(a1)
  c->lw(a0, 0, a1);                                 // lw a0, 0(a1)
  c->pextlw(a0, a0, a0);                            // pextlw a0, a0, a0
  c->pextlw(a0, a0, a0);                            // pextlw a0, a0, a0
  c->daddiu(a1, a2, -4);                            // daddiu a1, a2, -4
  c->addiu(a2, r0, 1);                              // addiu a2, r0, 1
  bc = ((s64)c->sgpr64(a1)) < 0;                    // bltz a1, L82
  c->daddiu(a1, a1, 4);                             // daddiu a1, a1, 4
  if (bc) {goto block_11;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lq(t0, 0, v1);                                 // lq t0, 0(v1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->pextub(a3, r0, t0);                            // pextub a3, r0, t0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(t0, r0, t0);                            // pextlb t0, r0, t0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psllh(a3, a3, 4);                              // psllh a3, a3, 4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psllh(t1, t0, 4);                              // psllh t1, t0, 4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextuh(t0, r0, t1);                            // pextuh t0, r0, t1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(t1, r0, t1);                            // pextlh t1, r0, t1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextuh(t4, r0, a3);                            // pextuh t4, r0, a3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(t2, r0, a3);                            // pextlh t2, r0, a3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->paddw(t3, t1, a0);                             // paddw t3, t1, a0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyud(t1, t3, r0);                            // pcpyud t1, t3, r0
  c->lq(a3, 0, t3);                                 // lq a3, 0(t3)
  c->paddw(t5, t0, a0);                             // paddw t5, t0, a0
  c->lq(t0, 0, t1);                                 // lq t0, 0(t1)
  c->pcpyud(t6, t5, r0);                            // pcpyud t6, t5, r0
  c->lq(t1, 0, t5);                                 // lq t1, 0(t5)
  c->dsra32(t3, t3, 0);                             // dsra32 t3, t3, 0
  c->dsra32(t5, t5, 0);                             // dsra32 t5, t5, 0
  c->paddw(t9, t2, a0);                             // paddw t9, t2, a0
  c->lq(t2, 0, t6);                                 // lq t2, 0(t6)
  c->pcpyud(t6, t9, r0);                            // pcpyud t6, t9, r0
  c->lq(t3, 0, t3);                                 // lq t3, 0(t3)
  c->paddw(t8, t4, a0);                             // paddw t8, t4, a0
  c->lq(t4, 0, t5);                                 // lq t4, 0(t5)
  c->pcpyud(t7, t8, r0);                            // pcpyud t7, t8, r0
  c->lq(t5, 0, t9);                                 // lq t5, 0(t9)
  c->dsra32(t9, t9, 0);                             // dsra32 t9, t9, 0
  c->dsra32(ra, t8, 0);                             // dsra32 ra, t8, 0
  // nop                                            // sll r0, r0, 0
  c->lq(t9, 0, t9);                                 // lq t9, 0(t9)
  // nop                                            // sll r0, r0, 0
  c->lq(t6, 0, t6);                                 // lq t6, 0(t6)
  // nop                                            // sll r0, r0, 0
  c->lq(t8, 0, t8);                                 // lq t8, 0(t8)
  // nop                                            // sll r0, r0, 0
  c->lq(ra, 0, ra);                                 // lq ra, 0(ra)
  // nop                                            // sll r0, r0, 0
  c->lq(t7, 0, t7);                                 // lq t7, 0(t7)
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf2, a3);                        // qmtc2.ni vf2, a3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf3, t3);                        // qmtc2.ni vf3, t3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf4, t0);                        // qmtc2.ni vf4, t0
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf7, t1);                        // qmtc2.ni vf7, t1
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf8, t4);                        // qmtc2.ni vf8, t4
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf9, t2);                        // qmtc2.ni vf9, t2
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf12, t5);                       // qmtc2.ni vf12, t5
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf13, t9);                       // qmtc2.ni vf13, t9
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf14, t6);                       // qmtc2.ni vf14, t6
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf17, t8);                       // qmtc2.ni vf17, t8
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf18, ra);                       // qmtc2.ni vf18, ra
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf19, t7);                       // qmtc2.ni vf19, t7

block_2:
  // nop                                            // sll r0, r0, 0
  c->lq(t0, 16, v1);                                // lq t0, 16(v1)
  c->daddiu(a1, a1, -4);                            // daddiu a1, a1, -4
  exec_0(c);                                        // vcallms 0
  c->pextub(a3, r0, t0);                            // pextub a3, r0, t0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(t0, r0, t0);                            // pextlb t0, r0, t0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psllh(a3, a3, 4);                              // psllh a3, a3, 4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psllh(t1, t0, 4);                              // psllh t1, t0, 4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextuh(t0, r0, t1);                            // pextuh t0, r0, t1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(t1, r0, t1);                            // pextlh t1, r0, t1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextuh(t4, r0, a3);                            // pextuh t4, r0, a3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(t2, r0, a3);                            // pextlh t2, r0, a3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->paddw(t3, t1, a0);                             // paddw t3, t1, a0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyud(t1, t3, r0);                            // pcpyud t1, t3, r0
  c->lq(a3, 0, t3);                                 // lq a3, 0(t3)
  c->paddw(t5, t0, a0);                             // paddw t5, t0, a0
  c->lq(t0, 0, t1);                                 // lq t0, 0(t1)
  c->pcpyud(t6, t5, r0);                            // pcpyud t6, t5, r0
  c->lq(t1, 0, t5);                                 // lq t1, 0(t5)
  c->dsra32(t3, t3, 0);                             // dsra32 t3, t3, 0
  c->dsra32(t5, t5, 0);                             // dsra32 t5, t5, 0
  c->paddw(t9, t2, a0);                             // paddw t9, t2, a0
  c->lq(t2, 0, t6);                                 // lq t2, 0(t6)
  c->pcpyud(t6, t9, r0);                            // pcpyud t6, t9, r0
  c->lq(t3, 0, t3);                                 // lq t3, 0(t3)
  c->paddw(t8, t4, a0);                             // paddw t8, t4, a0
  c->lq(t4, 0, t5);                                 // lq t4, 0(t5)
  c->pcpyud(t7, t8, r0);                            // pcpyud t7, t8, r0
  c->lq(t5, 0, t9);                                 // lq t5, 0(t9)
  c->dsra32(t9, t9, 0);                             // dsra32 t9, t9, 0
  c->dsra32(ra, t8, 0);                             // dsra32 ra, t8, 0
  // nop                                            // sll r0, r0, 0
  c->lq(t9, 0, t9);                                 // lq t9, 0(t9)
  // nop                                            // sll r0, r0, 0
  c->lq(t6, 0, t6);                                 // lq t6, 0(t6)
  // nop                                            // sll r0, r0, 0
  c->lq(t8, 0, t8);                                 // lq t8, 0(t8)
  // nop                                            // sll r0, r0, 0
  c->lq(ra, 0, ra);                                 // lq ra, 0(ra)
  // nop                                            // sll r0, r0, 0
  c->lq(t7, 0, t7);                                 // lq t7, 0(t7)
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf2, a3);                        // qmtc2.ni vf2, a3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf3, t3);                        // qmtc2.ni vf3, t3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf4, t0);                        // qmtc2.ni vf4, t0
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf7, t1);                        // qmtc2.ni vf7, t1
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf8, t4);                        // qmtc2.ni vf8, t4
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf9, t2);                        // qmtc2.ni vf9, t2
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf12, t5);                       // qmtc2.ni vf12, t5
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf13, t9);                       // qmtc2.ni vf13, t9
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf14, t6);                       // qmtc2.ni vf14, t6
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf17, t8);                       // qmtc2.ni vf17, t8
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf18, ra);                       // qmtc2.ni vf18, ra
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf19, t7);                       // qmtc2.ni vf19, t7
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t0, vf22);                       // qmfc2.ni t0, vf22
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t1, vf23);                       // qmfc2.ni t1, vf23
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a3, vf24);                       // qmfc2.ni a3, vf24
  bc = ((s64)c->sgpr64(t0)) >= 0;                   // bgez t0, L78
  c->mov128_gpr_vf(t0, vf25);                       // qmfc2.ni t0, vf25
  if (bc) {goto block_4;}                           // branch non-likely

  c->sb(a2, 3, v1);                                 // sb a2, 3(v1)
  // nop                                            // sll r0, r0, 0

block_4:
  bc = ((s64)c->sgpr64(t1)) >= 0;                   // bgez t1, L79
  c->daddiu(v1, v1, 4);                             // daddiu v1, v1, 4
  if (bc) {goto block_6;}                           // branch non-likely

  c->sb(a2, 3, v1);                                 // sb a2, 3(v1)
  // nop                                            // sll r0, r0, 0

block_6:
  bc = ((s64)c->sgpr64(a3)) >= 0;                   // bgez a3, L80
  c->daddiu(v1, v1, 4);                             // daddiu v1, v1, 4
  if (bc) {goto block_8;}                           // branch non-likely

  c->sb(a2, 3, v1);                                 // sb a2, 3(v1)
  // nop                                            // sll r0, r0, 0

block_8:
  bc = ((s64)c->sgpr64(t0)) >= 0;                   // bgez t0, L81
  c->daddiu(v1, v1, 4);                             // daddiu v1, v1, 4
  if (bc) {goto block_10;}                          // branch non-likely

  c->sb(a2, 3, v1);                                 // sb a2, 3(v1)
  // nop                                            // sll r0, r0, 0

block_10:
  bc = ((s64)c->sgpr64(a1)) > 0;                    // bgtz a1, L77
  c->daddiu(v1, v1, 4);                             // daddiu v1, v1, 4
  if (bc) {goto block_2;}                           // branch non-likely


block_11:
  bc = ((s64)c->sgpr64(a1)) <= 0;                   // blez a1, L85
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_15;}                          // branch non-likely


block_12:
  c->lbu(a3, 0, v1);                                // lbu a3, 0(v1)
  c->lbu(t0, 1, v1);                                // lbu t0, 1(v1)
  c->lbu(a2, 2, v1);                                // lbu a2, 2(v1)
  c->dsll(a3, a3, 4);                               // dsll a3, a3, 4
  c->dsll(t0, t0, 4);                               // dsll t0, t0, 4
  c->dsll(a2, a2, 4);                               // dsll a2, a2, 4
  c->daddu(a3, a3, a0);                             // daddu a3, a3, a0
  c->daddu(t0, t0, a0);                             // daddu t0, t0, a0
  c->daddu(a2, a2, a0);                             // daddu a2, a2, a0
  c->lqc2(vf2, 0, a3);                              // lqc2 vf2, 0(a3)
  c->lqc2(vf3, 0, t0);                              // lqc2 vf3, 0(t0)
  c->lqc2(vf4, 0, a2);                              // lqc2 vf4, 0(a2)
  c->vsub(DEST::xyzw, vf5, vf3, vf2);               // vsub.xyzw vf5, vf3, vf2
  c->vsub(DEST::xyzw, vf6, vf4, vf2);               // vsub.xyzw vf6, vf4, vf2
  c->vopmula(vf5, vf6);                             // vopmula.xyz acc, vf5, vf6
  c->vopmsub(vf5, vf6, vf5);                        // vopmsub.xyz vf5, vf6, vf5
  c->vmul(DEST::xyz, vf5, vf5, vf1);                // vmul.xyz vf5, vf5, vf1
  c->vadd_bc(DEST::y, BC::x, vf5, vf5, vf5);        // vaddx.y vf5, vf5, vf5
  c->vadd_bc(DEST::y, BC::z, vf5, vf5, vf5);        // vaddz.y vf5, vf5, vf5
  c->mov128_gpr_vf(a2, vf5);                        // qmfc2.i a2, vf5
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(a2)) >= 0;                   // bgez a2, L84
  c->addiu(a2, r0, 1);                              // addiu a2, r0, 1
  if (bc) {goto block_14;}                          // branch non-likely

  c->sb(a2, 3, v1);                                 // sb a2, 3(v1)

block_14:
  c->daddiu(a1, a1, -1);                            // daddiu a1, a1, -1
  bc = c->sgpr64(a1) != 0;                          // bne a1, r0, L83
  c->daddiu(v1, v1, 4);                             // daddiu v1, v1, 4
  if (bc) {goto block_12;}                          // branch non-likely


block_15:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  gLinkedFunctionTable.reg("shadow-find-facing-double-tris", execute, 128);
}

} // namespace shadow_find_facing_double_tris
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace shadow_find_double_edges {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->lw(a2, 16, a1);                                // lw a2, 16(a1)
  c->lh(a3, 18, a0);                                // lh a3, 18(a0)
  c->mov64(v1, a2);                                 // or v1, a2, r0
  c->lw(t1, 40, a0);                                // lw t1, 40(a0)
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L75
  c->lw(t0, 12, a1);                                // lw t0, 12(a1)
  if (bc) {goto block_7;}                           // branch non-likely

  c->daddu(a0, t1, a0);                             // daddu a0, t1, a0
  c->sw(a2, 40, a1);                                // sw a2, 40(a1)
  c->sw(a0, 8, a1);                                 // sw a0, 8(a1)
  c->mov64(t1, a0);                                 // or t1, a0, r0
  c->addiu(t2, r0, 255);                            // addiu t2, r0, 255

block_2:
  c->daddiu(a3, a3, -1);                            // daddiu a3, a3, -1
  c->lbu(t3, 3, t1);                                // lbu t3, 3(t1)
  // nop                                            // sll r0, r0, 0
  c->lbu(t4, 2, t1);                                // lbu t4, 2(t1)
  bc = c->sgpr64(t3) == c->sgpr64(t2);              // beq t3, t2, L73
  c->gprs[t5].du64[0] = 0;                          // or t5, r0, r0
  if (bc) {goto block_5;}                           // branch non-likely

  c->dsll(t4, t4, 2);                               // dsll t4, t4, 2
  c->dsll(t3, t3, 2);                               // dsll t3, t3, 2
  c->daddu(t4, t4, t0);                             // daddu t4, t4, t0
  c->daddu(t3, t3, t0);                             // daddu t3, t3, t0
  // nop                                            // sll r0, r0, 0
  c->lbu(t4, 3, t4);                                // lbu t4, 3(t4)
  // nop                                            // sll r0, r0, 0
  c->lbu(t3, 3, t3);                                // lbu t3, 3(t3)
  bc = c->sgpr64(t4) == c->sgpr64(t3);              // beq t4, t3, L74
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_6;}                           // branch non-likely

  c->sltiu(t4, t4, 1);                              // sltiu t4, t4, 1
  // nop                                            // sll r0, r0, 0
  c->sltu(t3, r0, t3);                              // sltu t3, r0, t3
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sh(t4, 2, v1);                                 // sh t4, 2(v1)
  c->dsubu(t4, t1, a0);                             // dsubu t4, t1, a0
  c->sh(t3, 6, v1);                                 // sh t3, 6(v1)
  // nop                                            // sll r0, r0, 0
  c->sh(t4, 0, v1);                                 // sh t4, 0(v1)
  // nop                                            // sll r0, r0, 0
  c->sh(t4, 4, v1);                                 // sh t4, 4(v1)
  //beq r0, r0, L74                                 // beq r0, r0, L74
  c->daddiu(v1, v1, 8);                             // daddiu v1, v1, 8
  goto block_6;                                     // branch always


block_5:
  c->dsll(t3, t4, 2);                               // dsll t3, t4, 2
  // nop                                            // sll r0, r0, 0
  c->daddu(t3, t3, t0);                             // daddu t3, t3, t0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lbu(t3, 3, t3);                                // lbu t3, 3(t3)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sltiu(t3, t3, 1);                              // sltiu t3, t3, 1
  c->dsubu(t4, t1, a0);                             // dsubu t4, t1, a0
  c->sh(t3, 2, v1);                                 // sh t3, 2(v1)
  c->sh(t4, 0, v1);                                 // sh t4, 0(v1)
  c->daddiu(v1, v1, 4);                             // daddiu v1, v1, 4

block_6:
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L72
  c->daddiu(t1, t1, 4);                             // daddiu t1, t1, 4
  if (bc) {goto block_2;}                           // branch non-likely


block_7:
  c->dsubu(a0, v1, a2);                             // dsubu a0, v1, a2
  c->dsra(a0, a0, 2);                               // dsra a0, a0, 2
  c->sw(a0, 28, a1);                                // sw a0, 28(a1)
  c->sw(v1, 16, a1);                                // sw v1, 16(a1)
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
  gLinkedFunctionTable.reg("shadow-find-double-edges", execute, 128);
}

} // namespace shadow_find_double_edges
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace shadow_add_verts {
struct Cache {
  void* shadow_data; // *shadow-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->load_symbol2(v1, cache.shadow_data);           // lw v1, *shadow-data*(s7)
  c->mov64(v1, v1);                                 // or v1, v1, r0
  c->lh(a0, 8, a0);                                 // lh a0, 8(a0)
  c->addiu(t1, r0, 4);                              // addiu t1, r0, 4
  c->lq(t0, 0, v1);                                 // lq t0, 0(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t0, 0, a2);                                 // sq t0, 0(a2)
  c->sh(a0, 0, a2);                                 // sh a0, 0(a2)
  c->sb(a0, 14, a2);                                // sb a0, 14(a2)
  c->sh(t1, 12, a2);                                // sh t1, 12(a2)
  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->lw(t3, 0, a1);                                 // lw t3, 0(a1)
  c->sll(v1, a0, 4);                                // sll v1, a0, 4
  c->mov64(t2, a2);                                 // or t2, a2, r0
  c->mov64(t1, a0);                                 // or t1, a0, r0
  c->daddiu(t4, t1, -4);                            // daddiu t4, t1, -4
  c->mov64(t2, t2);                                 // or t2, t2, r0
  bc = ((s64)c->sgpr64(t4)) < 0;                    // bltz t4, L65
  c->mov64(t3, t3);                                 // or t3, t3, r0
  if (bc) {goto block_2;}                           // branch non-likely


block_1:
  // nop                                            // sll r0, r0, 0
  c->lq(t7, 0, t3);                                 // lq t7, 0(t3)
  // nop                                            // sll r0, r0, 0
  c->lq(t4, 16, t3);                                // lq t4, 16(t3)
  c->daddiu(t1, t1, -4);                            // daddiu t1, t1, -4
  c->lq(t5, 32, t3);                                // lq t5, 32(t3)
  c->daddiu(t2, t2, 64);                            // daddiu t2, t2, 64
  c->lq(t6, 48, t3);                                // lq t6, 48(t3)
  c->daddiu(t3, t3, 64);                            // daddiu t3, t3, 64
  c->sq(t7, -64, t2);                               // sq t7, -64(t2)
  c->daddiu(t7, t1, -4);                            // daddiu t7, t1, -4
  c->sq(t4, -48, t2);                               // sq t4, -48(t2)
  // nop                                            // sll r0, r0, 0
  c->sq(t5, -32, t2);                               // sq t5, -32(t2)
  bc = ((s64)c->sgpr64(t7)) >= 0;                   // bgez t7, L64
  c->sq(t6, -16, t2);                               // sq t6, -16(t2)
  if (bc) {goto block_1;}                           // branch non-likely


block_2:
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L66
  c->lq(t4, 0, t3);                                 // lq t4, 0(t3)
  if (bc) {goto block_7;}                           // branch non-likely

  c->daddiu(t3, t3, 16);                            // daddiu t3, t3, 16
  c->daddiu(t2, t2, 16);                            // daddiu t2, t2, 16
  c->daddiu(t1, t1, -1);                            // daddiu t1, t1, -1
  c->sq(t4, -16, t2);                               // sq t4, -16(t2)
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L66
  c->lq(t4, 0, t3);                                 // lq t4, 0(t3)
  if (bc) {goto block_7;}                           // branch non-likely

  c->daddiu(t3, t3, 16);                            // daddiu t3, t3, 16
  c->daddiu(t2, t2, 16);                            // daddiu t2, t2, 16
  c->daddiu(t1, t1, -1);                            // daddiu t1, t1, -1
  c->sq(t4, -16, t2);                               // sq t4, -16(t2)
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L66
  c->lq(t4, 0, t3);                                 // lq t4, 0(t3)
  if (bc) {goto block_7;}                           // branch non-likely

  c->daddiu(t3, t3, 16);                            // daddiu t3, t3, 16
  c->daddiu(t2, t2, 16);                            // daddiu t2, t2, 16
  c->daddiu(t1, t1, -1);                            // daddiu t1, t1, -1
  c->sq(t4, -16, t2);                               // sq t4, -16(t2)
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L66
  c->lq(t4, 0, t3);                                 // lq t4, 0(t3)
  if (bc) {goto block_7;}                           // branch non-likely

  c->daddiu(t3, t3, 16);                            // daddiu t3, t3, 16
  c->daddiu(t2, t2, 16);                            // daddiu t2, t2, 16
  c->daddiu(t1, t1, -1);                            // daddiu t1, t1, -1
  c->sq(t4, -16, t2);                               // sq t4, -16(t2)

block_7:
  c->gprs[t1].du64[0] = 0;                          // or t1, r0, r0
  c->daddu(v0, a2, v1);                             // daddu v0, a2, v1
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L70
  c->addiu(a2, r0, 174);                            // addiu a2, r0, 174
  if (bc) {goto block_16;}                          // branch non-likely

  c->sq(t0, 0, v0);                                 // sq t0, 0(v0)
  c->sh(a0, 0, v0);                                 // sh a0, 0(v0)
  c->sb(a0, 14, v0);                                // sb a0, 14(v0)
  c->sw(r0, 8, v0);                                 // sw r0, 8(v0)
  c->sh(a2, 12, v0);                                // sh a2, 12(v0)
  c->lw(a3, 44, a1);                                // lw a3, 44(a1)
  c->daddiu(a1, v0, 16);                            // daddiu a1, v0, 16
  c->mov64(a2, a1);                                 // or a2, a1, r0
  c->daddiu(t0, a0, -4);                            // daddiu t0, a0, -4
  c->mov64(a2, a2);                                 // or a2, a2, r0
  bc = ((s64)c->sgpr64(t0)) < 0;                    // bltz t0, L68
  c->mov64(a3, a3);                                 // or a3, a3, r0
  if (bc) {goto block_10;}                          // branch non-likely


block_9:
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 0, a3);                                 // lq t3, 0(a3)
  // nop                                            // sll r0, r0, 0
  c->lq(t0, 16, a3);                                // lq t0, 16(a3)
  c->daddiu(a0, a0, -4);                            // daddiu a0, a0, -4
  c->lq(t1, 32, a3);                                // lq t1, 32(a3)
  c->daddiu(a2, a2, 64);                            // daddiu a2, a2, 64
  c->lq(t2, 48, a3);                                // lq t2, 48(a3)
  c->daddiu(a3, a3, 64);                            // daddiu a3, a3, 64
  c->sq(t3, -64, a2);                               // sq t3, -64(a2)
  c->daddiu(t3, a0, -4);                            // daddiu t3, a0, -4
  c->sq(t0, -48, a2);                               // sq t0, -48(a2)
  // nop                                            // sll r0, r0, 0
  c->sq(t1, -32, a2);                               // sq t1, -32(a2)
  bc = ((s64)c->sgpr64(t3)) >= 0;                   // bgez t3, L67
  c->sq(t2, -16, a2);                               // sq t2, -16(a2)
  if (bc) {goto block_9;}                           // branch non-likely


block_10:
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L69
  c->lq(t0, 0, a3);                                 // lq t0, 0(a3)
  if (bc) {goto block_15;}                          // branch non-likely

  c->daddiu(a3, a3, 16);                            // daddiu a3, a3, 16
  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(t0, -16, a2);                               // sq t0, -16(a2)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L69
  c->lq(t0, 0, a3);                                 // lq t0, 0(a3)
  if (bc) {goto block_15;}                          // branch non-likely

  c->daddiu(a3, a3, 16);                            // daddiu a3, a3, 16
  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(t0, -16, a2);                               // sq t0, -16(a2)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L69
  c->lq(t0, 0, a3);                                 // lq t0, 0(a3)
  if (bc) {goto block_15;}                          // branch non-likely

  c->daddiu(a3, a3, 16);                            // daddiu a3, a3, 16
  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(t0, -16, a2);                               // sq t0, -16(a2)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L69
  c->lq(t0, 0, a3);                                 // lq t0, 0(a3)
  if (bc) {goto block_15;}                          // branch non-likely

  c->daddiu(a3, a3, 16);                            // daddiu a3, a3, 16
  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(t0, -16, a2);                               // sq t0, -16(a2)

block_15:
  c->gprs[a0].du64[0] = 0;                          // or a0, r0, r0
  c->daddu(v0, a1, v1);                             // daddu v0, a1, v1

block_16:
  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.shadow_data = intern_from_c("*shadow-data*").c();
  gLinkedFunctionTable.reg("shadow-add-verts", execute, 128);
}

} // namespace shadow_add_verts
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace shadow_add_facing_single_tris {
struct Cache {
  void* shadow_data; // *shadow-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->load_symbol2(v1, cache.shadow_data);           // lw v1, *shadow-data*(s7)
  c->mov64(a3, v1);                                 // or a3, v1, r0
  c->lw(v1, 20, a1);                                // lw v1, 20(a1)
  c->lw(a0, 32, a1);                                // lw a0, 32(a1)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L62
  c->daddiu(a1, v1, 1);                             // daddiu a1, v1, 1
  if (bc) {goto block_4;}                           // branch non-likely

  c->daddiu(a1, a1, 3);                             // daddiu a1, a1, 3
  c->dsra(t0, a1, 2);                               // dsra t0, a1, 2
  c->dsll(a1, t0, 2);                               // dsll a1, t0, 2
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  c->ld(t1, 16, a3);                                // ld t1, 16(a3)
  c->daddu(t0, t1, t0);                             // daddu t0, t1, t0
  c->lw(a3, 28, a3);                                // lw a3, 28(a3)
  c->sd(t0, 0, a2);                                 // sd t0, 0(a2)
  c->addiu(t0, r0, 16728);                          // addiu t0, r0, 16728
  c->sw(r0, 8, a2);                                 // sw r0, 8(a2)
  c->sw(a3, 12, a2);                                // sw a3, 12(a2)
  c->sb(a1, 14, a2);                                // sb a1, 14(a2)
  c->dsll(a1, a1, 2);                               // dsll a1, a1, 2
  c->sh(t0, 12, a2);                                // sh t0, 12(a2)
  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddu(a1, a2, a1);                             // daddu a1, a2, a1
  c->sq(r0, -16, a1);                               // sq r0, -16(a1)
  c->sw(v1, 0, a2);                                 // sw v1, 0(a2)
  c->daddiu(a2, a2, 4);                             // daddiu a2, a2, 4

block_2:
  c->lw(a3, 0, a0);                                 // lw a3, 0(a0)
  c->daddiu(a0, a0, 4);                             // daddiu a0, a0, 4
  c->lw(a3, 0, a3);                                 // lw a3, 0(a3)
  c->daddiu(v1, v1, -1);                            // daddiu v1, v1, -1
  c->sw(a3, 0, a2);                                 // sw a3, 0(a2)
  c->daddiu(a2, a2, 4);                             // daddiu a2, a2, 4
  bc = ((s64)c->sgpr64(v1)) > 0;                    // bgtz v1, L61
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  c->mov64(v1, a1);                                 // or v1, a1, r0
  c->lui(a0, 5376);                                 // lui a0, 5376
  c->ori(a0, a0, 2);                                // ori a0, a0, 2
  c->sq(r0, 0, v1);                                 // sq r0, 0(v1)
  c->sw(a0, 12, v1);                                // sw a0, 12(v1)
  c->daddiu(a2, v1, 16);                            // daddiu a2, v1, 16

block_4:
  c->mov64(v0, a2);                                 // or v0, a2, r0
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
  cache.shadow_data = intern_from_c("*shadow-data*").c();
  gLinkedFunctionTable.reg("shadow-add-facing-single-tris", execute, 128);
}

} // namespace shadow_add_facing_single_tris
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace shadow_add_single_edges {
struct Cache {
  void* shadow_data; // *shadow-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->load_symbol2(v1, cache.shadow_data);           // lw v1, *shadow-data*(s7)
  c->mov64(a3, v1);                                 // or a3, v1, r0
  c->lw(v1, 24, a1);                                // lw v1, 24(a1)
  c->lw(a0, 36, a1);                                // lw a0, 36(a1)
  c->lw(a1, 4, a1);                                 // lw a1, 4(a1)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L59
  c->daddiu(t0, v1, 1);                             // daddiu t0, v1, 1
  if (bc) {goto block_4;}                           // branch non-likely

  c->daddiu(t0, t0, 3);                             // daddiu t0, t0, 3
  c->dsra(t1, t0, 2);                               // dsra t1, t0, 2
  c->dsll(t0, t1, 2);                               // dsll t0, t1, 2
  c->daddiu(t1, t1, 1);                             // daddiu t1, t1, 1
  c->ld(t2, 16, a3);                                // ld t2, 16(a3)
  c->daddu(t1, t2, t1);                             // daddu t1, t2, t1
  c->lw(a3, 28, a3);                                // lw a3, 28(a3)
  c->sd(t1, 0, a2);                                 // sd t1, 0(a2)
  c->addiu(t1, r0, 16984);                          // addiu t1, r0, 16984
  c->sw(r0, 8, a2);                                 // sw r0, 8(a2)
  c->sw(a3, 12, a2);                                // sw a3, 12(a2)
  c->sb(t0, 14, a2);                                // sb t0, 14(a2)
  c->dsll(a3, t0, 2);                               // dsll a3, t0, 2
  c->sh(t1, 12, a2);                                // sh t1, 12(a2)
  c->daddiu(t0, a2, 16);                            // daddiu t0, a2, 16
  c->daddu(a2, t0, a3);                             // daddu a2, t0, a3
  c->sq(r0, -16, a2);                               // sq r0, -16(a2)
  c->sw(v1, 0, t0);                                 // sw v1, 0(t0)
  c->daddiu(a3, t0, 4);                             // daddiu a3, t0, 4

block_2:
  c->lh(t1, 0, a0);                                 // lh t1, 0(a0)
  c->lh(t0, 2, a0);                                 // lh t0, 2(a0)
  c->daddiu(a0, a0, 4);                             // daddiu a0, a0, 4
  c->daddu(t1, t1, a1);                             // daddu t1, t1, a1
  c->lhu(t1, 0, t1);                                // lhu t1, 0(t1)
  c->sh(t0, 2, a3);                                 // sh t0, 2(a3)
  c->sh(t1, 0, a3);                                 // sh t1, 0(a3)
  c->daddiu(v1, v1, -1);                            // daddiu v1, v1, -1
  c->daddiu(a3, a3, 4);                             // daddiu a3, a3, 4
  bc = ((s64)c->sgpr64(v1)) > 0;                    // bgtz v1, L58
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  c->mov64(v1, a2);                                 // or v1, a2, r0
  c->lui(a0, 5376);                                 // lui a0, 5376
  c->ori(a0, a0, 4);                                // ori a0, a0, 4
  c->sq(r0, 0, v1);                                 // sq r0, 0(v1)
  c->sw(a0, 12, v1);                                // sw a0, 12(v1)
  c->daddiu(a2, v1, 16);                            // daddiu a2, v1, 16

block_4:
  c->mov64(v0, a2);                                 // or v0, a2, r0
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
  cache.shadow_data = intern_from_c("*shadow-data*").c();
  gLinkedFunctionTable.reg("shadow-add-single-edges", execute, 128);
}

} // namespace shadow_add_single_edges
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace shadow_add_single_tris {
struct Cache {
  void* shadow_data; // *shadow-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->load_symbol2(v1, cache.shadow_data);           // lw v1, *shadow-data*(s7)
  c->mov64(a3, v1);                                 // or a3, v1, r0
  c->lh(a1, 12, a0);                                // lh a1, 12(a0)
  c->lw(v1, 28, a0);                                // lw v1, 28(a0)
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L56
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  if (bc) {goto block_4;}                           // branch non-likely

  c->daddiu(a0, a1, 1);                             // daddiu a0, a1, 1
  c->daddiu(a0, a0, 3);                             // daddiu a0, a0, 3
  c->dsra(t0, a0, 2);                               // dsra t0, a0, 2
  c->dsll(a0, t0, 2);                               // dsll a0, t0, 2
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  c->ld(t1, 16, a3);                                // ld t1, 16(a3)
  c->daddu(t0, t1, t0);                             // daddu t0, t1, t0
  c->lw(a3, 28, a3);                                // lw a3, 28(a3)
  c->sd(t0, 0, a2);                                 // sd t0, 0(a2)
  c->addiu(t0, r0, 16728);                          // addiu t0, r0, 16728
  c->sw(r0, 8, a2);                                 // sw r0, 8(a2)
  c->sw(a3, 12, a2);                                // sw a3, 12(a2)
  c->sb(a0, 14, a2);                                // sb a0, 14(a2)
  c->dsll(a0, a0, 2);                               // dsll a0, a0, 2
  c->sh(t0, 12, a2);                                // sh t0, 12(a2)
  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddu(a0, a2, a0);                             // daddu a0, a2, a0
  c->sq(r0, -16, a0);                               // sq r0, -16(a0)
  c->ori(a1, a1, 256);                              // ori a1, a1, 256
  c->sw(a1, 0, a2);                                 // sw a1, 0(a2)
  c->daddiu(a2, a2, 4);                             // daddiu a2, a2, 4

block_2:
  c->lw(a3, 0, v1);                                 // lw a3, 0(v1)
  c->daddiu(v1, v1, 4);                             // daddiu v1, v1, 4
  c->daddiu(a1, a1, -1);                            // daddiu a1, a1, -1
  c->sw(a3, 0, a2);                                 // sw a3, 0(a2)
  bc = ((s64)c->sgpr64(a1)) > 0;                    // bgtz a1, L55
  c->daddiu(a2, a2, 4);                             // daddiu a2, a2, 4
  if (bc) {goto block_2;}                           // branch non-likely

  c->mov64(v1, a0);                                 // or v1, a0, r0
  c->lui(a0, 5376);                                 // lui a0, 5376
  c->ori(a0, a0, 6);                                // ori a0, a0, 6
  c->sq(r0, 0, v1);                                 // sq r0, 0(v1)
  c->sw(a0, 12, v1);                                // sw a0, 12(v1)
  c->daddiu(a2, v1, 16);                            // daddiu a2, v1, 16

block_4:
  c->mov64(v0, a2);                                 // or v0, a2, r0
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
  cache.shadow_data = intern_from_c("*shadow-data*").c();
  gLinkedFunctionTable.reg("shadow-add-single-tris", execute, 128);
}

} // namespace shadow_add_single_tris
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace shadow_add_double_tris {
struct Cache {
  void* shadow_data; // *shadow-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->load_symbol2(v1, cache.shadow_data);           // lw v1, *shadow-data*(s7)
  c->mov64(a3, v1);                                 // or a3, v1, r0
  c->lh(v1, 16, a0);                                // lh v1, 16(a0)
  c->lw(a0, 12, a1);                                // lw a0, 12(a1)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L53
  c->daddiu(a1, v1, 1);                             // daddiu a1, v1, 1
  if (bc) {goto block_4;}                           // branch non-likely

  c->daddiu(a1, a1, 3);                             // daddiu a1, a1, 3
  c->dsra(t0, a1, 2);                               // dsra t0, a1, 2
  c->dsll(a1, t0, 2);                               // dsll a1, t0, 2
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  c->ld(t1, 16, a3);                                // ld t1, 16(a3)
  c->daddu(t0, t1, t0);                             // daddu t0, t1, t0
  c->lw(a3, 28, a3);                                // lw a3, 28(a3)
  c->sd(t0, 0, a2);                                 // sd t0, 0(a2)
  c->addiu(t0, r0, 16728);                          // addiu t0, r0, 16728
  c->sw(r0, 8, a2);                                 // sw r0, 8(a2)
  c->sw(a3, 12, a2);                                // sw a3, 12(a2)
  c->sb(a1, 14, a2);                                // sb a1, 14(a2)
  c->dsll(a1, a1, 2);                               // dsll a1, a1, 2
  c->sh(t0, 12, a2);                                // sh t0, 12(a2)
  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddu(a1, a2, a1);                             // daddu a1, a2, a1
  c->sq(r0, -16, a1);                               // sq r0, -16(a1)
  c->sw(v1, 0, a2);                                 // sw v1, 0(a2)
  c->daddiu(a2, a2, 4);                             // daddiu a2, a2, 4

block_2:
  c->lw(a3, 0, a0);                                 // lw a3, 0(a0)
  c->daddiu(a0, a0, 4);                             // daddiu a0, a0, 4
  c->daddiu(v1, v1, -1);                            // daddiu v1, v1, -1
  c->sw(a3, 0, a2);                                 // sw a3, 0(a2)
  c->daddiu(a2, a2, 4);                             // daddiu a2, a2, 4
  bc = ((s64)c->sgpr64(v1)) > 0;                    // bgtz v1, L52
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  c->mov64(v1, a1);                                 // or v1, a1, r0
  c->lui(a0, 5376);                                 // lui a0, 5376
  c->ori(a0, a0, 6);                                // ori a0, a0, 6
  c->sq(r0, 0, v1);                                 // sq r0, 0(v1)
  c->sw(a0, 12, v1);                                // sw a0, 12(v1)
  c->daddiu(a2, v1, 16);                            // daddiu a2, v1, 16

block_4:
  c->mov64(v0, a2);                                 // or v0, a2, r0
  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.shadow_data = intern_from_c("*shadow-data*").c();
  gLinkedFunctionTable.reg("shadow-add-double-tris", execute, 128);
}

} // namespace shadow_add_double_tris
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace shadow_add_double_edges {
struct Cache {
  void* shadow_data; // *shadow-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->load_symbol2(v1, cache.shadow_data);           // lw v1, *shadow-data*(s7)
  c->mov64(a3, v1);                                 // or a3, v1, r0
  c->lw(v1, 28, a1);                                // lw v1, 28(a1)
  c->lw(a0, 40, a1);                                // lw a0, 40(a1)
  c->lw(a1, 8, a1);                                 // lw a1, 8(a1)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L50
  c->daddiu(t0, v1, 1);                             // daddiu t0, v1, 1
  if (bc) {goto block_4;}                           // branch non-likely

  c->daddiu(t0, t0, 3);                             // daddiu t0, t0, 3
  c->dsra(t1, t0, 2);                               // dsra t1, t0, 2
  c->dsll(t0, t1, 2);                               // dsll t0, t1, 2
  c->daddiu(t1, t1, 1);                             // daddiu t1, t1, 1
  c->ld(t2, 16, a3);                                // ld t2, 16(a3)
  c->daddu(t1, t2, t1);                             // daddu t1, t2, t1
  c->lw(a3, 28, a3);                                // lw a3, 28(a3)
  c->sd(t1, 0, a2);                                 // sd t1, 0(a2)
  c->addiu(t1, r0, 16984);                          // addiu t1, r0, 16984
  c->sw(r0, 8, a2);                                 // sw r0, 8(a2)
  c->sw(a3, 12, a2);                                // sw a3, 12(a2)
  c->sb(t0, 14, a2);                                // sb t0, 14(a2)
  c->dsll(a3, t0, 2);                               // dsll a3, t0, 2
  c->sh(t1, 12, a2);                                // sh t1, 12(a2)
  c->daddiu(t0, a2, 16);                            // daddiu t0, a2, 16
  c->daddu(a2, t0, a3);                             // daddu a2, t0, a3
  c->sq(r0, -16, a2);                               // sq r0, -16(a2)
  c->sw(v1, 0, t0);                                 // sw v1, 0(t0)
  c->daddiu(a3, t0, 4);                             // daddiu a3, t0, 4

block_2:
  c->lh(t1, 0, a0);                                 // lh t1, 0(a0)
  c->lh(t0, 2, a0);                                 // lh t0, 2(a0)
  c->daddiu(a0, a0, 4);                             // daddiu a0, a0, 4
  c->daddu(t1, t1, a1);                             // daddu t1, t1, a1
  c->lhu(t1, 0, t1);                                // lhu t1, 0(t1)
  c->sh(t0, 2, a3);                                 // sh t0, 2(a3)
  c->sh(t1, 0, a3);                                 // sh t1, 0(a3)
  c->daddiu(v1, v1, -1);                            // daddiu v1, v1, -1
  c->daddiu(a3, a3, 4);                             // daddiu a3, a3, 4
  bc = ((s64)c->sgpr64(v1)) > 0;                    // bgtz v1, L49
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  c->mov64(v1, a2);                                 // or v1, a2, r0
  c->lui(a0, 5376);                                 // lui a0, 5376
  c->ori(a0, a0, 4);                                // ori a0, a0, 4
  c->sq(r0, 0, v1);                                 // sq r0, 0(v1)
  c->sw(a0, 12, v1);                                // sw a0, 12(v1)
  c->daddiu(a2, v1, 16);                            // daddiu a2, v1, 16

block_4:
  c->mov64(v0, a2);                                 // or v0, a2, r0
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
  cache.shadow_data = intern_from_c("*shadow-data*").c();
  gLinkedFunctionTable.reg("shadow-add-double-edges", execute, 128);
}

} // namespace shadow_add_double_edges
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace shadow_execute {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* gsf_buffer; // *gsf-buffer*
  void* math_camera; // *math-camera*
  void* shadow_debug; // *shadow-debug*
  void* camera_pos; // camera-pos
  void* debug_draw_settings; // debug-draw-settings
  void* flush_cache; // flush-cache
  void* shadow_add_double_edges; // shadow-add-double-edges
  void* shadow_add_double_tris; // shadow-add-double-tris
  void* shadow_add_facing_single_tris; // shadow-add-facing-single-tris
  void* shadow_add_single_edges; // shadow-add-single-edges
  void* shadow_add_single_tris; // shadow-add-single-tris
  void* shadow_add_verts; // shadow-add-verts
  void* shadow_calc_dual_verts; // shadow-calc-dual-verts
  void* shadow_find_double_edges; // shadow-find-double-edges
  void* shadow_find_facing_double_tris; // shadow-find-facing-double-tris
  void* shadow_find_facing_single_tris; // shadow-find-facing-single-tris
  void* shadow_find_single_edges; // shadow-find-single-edges
  void* shadow_init_vars; // shadow-init-vars
  void* shadow_scissor_edges; // shadow-scissor-edges
  void* shadow_scissor_top; // shadow-scissor-top
  void* shadow_xform_verts; // shadow-xform-verts
} cache;

u64 execute(void* ctxt) {
  u32 sadr, tadr;
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  bool cop1_bc = false;
  c->daddiu(sp, sp, -112);                          // daddiu sp, sp, -112
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s1, 16, sp);                                // sq s1, 16(sp)
  c->sq(s2, 32, sp);                                // sq s2, 32(sp)
  c->sq(s3, 48, sp);                                // sq s3, 48(sp)
  c->sq(s4, 64, sp);                                // sq s4, 64(sp)
  c->sq(s5, 80, sp);                                // sq s5, 80(sp)
  c->sq(gp, 96, sp);                                // sq gp, 96(sp)
  c->mov64(gp, a1);                                 // or gp, a1, r0
  c->mov64(s4, a0);                                 // or s4, a0, r0
  c->addiu(v1, r0, 0);                              // addiu v1, r0, 0
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->daddiu(s5, v1, 40);                            // daddiu s5, v1, 40
  c->load_symbol2(t9, cache.flush_cache);           // lw t9, flush-cache(s7)
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  //beq r0, r0, L42                                 // beq r0, r0, L42
  // nop                                            // sll r0, r0, 0
  goto block_37;                                    // branch always


block_1:
  c->lui(v1, 4096);                                 // lui v1, 4096
  c->ori(v1, v1, 54272);                            // ori v1, v1, 54272
  c->addiu(a0, r0, 48);                             // addiu a0, r0, 48
  c->mov64(a1, v1);                                 // or a1, v1, r0

/*
block_2:
  c->lw(a2, 0, a1);                                 // lw a2, 0(a1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(a2, a2, 256);                             // andi a2, a2, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L26
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_4;}                           // branch non-likely

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
  //beq r0, r0, L25                                 // beq r0, r0, L25
  // nop                                            // sll r0, r0, 0
  goto block_2;                                     // branch always


block_4:
  c->gprs[a1].du64[0] = 0;                          // or a1, r0, r0
  c->mov64(a1, v1);                                 // or a1, v1, r0
  c->mov64(a2, s5);                                 // or a2, s5, r0
  c->lw(a3, 0, a1);                                 // lw a3, 0(a1)
  c->andi(a3, a3, 256);                             // andi a3, a3, 256
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L28
  DANGER jump to delay slot, this MUST be fixed manually!
  c->lw(a3, 0, a2);                                 // lw a3, 0(a2)
  if (bc) {goto block_-1;}                          // branch non-likely


block_5:
  c->lw(a3, 0, a2);                                 // lw a3, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->lw(t0, 0, a1);                                 // lw t0, 0(a1)
  // nop                                            // sll r0, r0, 0
  c->andi(t0, t0, 256);                             // andi t0, t0, 256
  c->daddiu(a3, a3, 1);                             // daddiu a3, a3, 1
  bc = c->sgpr64(t0) != 0;                          // bne t0, r0, L27
  c->sw(a3, 0, a2);                                 // sw a3, 0(a2)
  if (bc) {goto block_5;}                           // branch non-likely
*/

  c->gprs[a1].du64[0] = 0;                          // or a1, r0, r0
  sadr = c->sgpr64(a0); // c->sw(a0, 128, v1);                               // sw a0, 128(v1)
  tadr = c->sgpr64(s4); // c->sw(s4, 48, v1);                                // sw s4, 48(v1)
  // c->sw(r0, 32, v1);                                // sw r0, 32(v1)
  // Unknown instr: sync.l
  c->addiu(a0, r0, 324);                            // addiu a0, r0, 324
  // c->sw(a0, 0, v1);                                 // sw a0, 0(v1)
  spad_to_dma_blerc_chain(cache.fake_scratchpad_data, sadr, tadr);
  // Unknown instr: sync.l
  c->mov64(a0, s5);                                 // or a0, s5, r0
/*
  c->lw(a1, 0, v1);                                 // lw a1, 0(v1)
  c->andi(a1, a1, 256);                             // andi a1, a1, 256
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L30
  DANGER jump to delay slot, this MUST be fixed manually!
  c->lw(a1, 0, a0);                                 // lw a1, 0(a0)
  if (bc) {goto block_-1;}                          // branch non-likely


block_8:
  c->lw(a1, 0, a0);                                 // lw a1, 0(a0)
  // nop                                            // sll r0, r0, 0
  c->lw(a2, 0, v1);                                 // lw a2, 0(v1)
  // nop                                            // sll r0, r0, 0
  c->andi(a2, a2, 256);                             // andi a2, a2, 256
  c->daddiu(a1, a1, 1);                             // daddiu a1, a1, 1
  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L29
  c->sw(a1, 0, a0);                                 // sw a1, 0(a0)
  if (bc) {goto block_8;}                           // branch non-likely
*/

  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  get_fake_spad_addr2(s4, cache.fake_scratchpad_data, 0, c);// lui s4, 28672
  c->addiu(v1, r0, 160);                            // addiu v1, r0, 160
  c->daddu(s2, v1, s4);                             // daddu s2, v1, s4
  c->lw(v1, 56, s4);                                // lw v1, 56(s4)
  c->daddiu(s1, s4, 64);                            // daddiu s1, s4, 64
  c->load_symbol2(s3, cache.gsf_buffer);            // lw s3, *gsf-buffer*(s7)
  c->sw(v1, 68, s3);                                // sw v1, 68(s3)
  c->lw(v1, 12, s1);                                // lw v1, 12(s1)
  c->andi(v1, v1, 32);                              // andi v1, v1, 32
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L41
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_36;}                          // branch non-likely

  c->load_symbol2(v1, cache.shadow_debug);          // lw v1, *shadow-debug*(s7)
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L31
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_13;}                          // branch non-likely

  c->load_symbol2(t9, cache.debug_draw_settings);   // lw t9, debug-draw-settings(s7)
  c->mov64(a0, s1);                                 // or a0, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0

block_13:
  c->daddiu(v1, s3, 80);                            // daddiu v1, s3, 80
  c->daddu(a0, r0, s1);                             // daddu a0, r0, s1
  c->daddiu(a1, s1, 16);                            // daddiu a1, s1, 16
  c->lwc1(f0, 28, s1);                              // lwc1 f0, 28(s1)
  c->lqc2(vf2, 0, a1);                              // lqc2 vf2, 0(a1)
  c->lqc2(vf1, 0, a0);                              // lqc2 vf1, 0(a0)
  c->mfc1(a0, f0);                                  // mfc1 a0, f0
  c->mov128_vf_gpr(vf3, a0);                        // qmtc2.i vf3, a0
  c->vadd_bc(DEST::w, BC::x, vf4, vf0, vf0);        // vaddx.w vf4, vf0, vf0
  c->vmula_bc(DEST::xyzw, BC::x, vf2, vf3);         // vmulax.xyzw acc, vf2, vf3
  c->vmadd_bc(DEST::xyz, BC::w, vf4, vf1, vf0);     // vmaddw.xyz vf4, vf1, vf0
  c->sqc2(vf4, 0, v1);                              // sqc2 vf4, 0(v1)
  c->lw(v1, 12, s1);                                // lw v1, 12(s1)
  c->andi(v1, v1, 4);                               // andi v1, v1, 4
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L32
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_15;}                          // branch non-likely

  c->lq(v1, 32, s1);                                // lq v1, 32(s1)
  c->sq(v1, 96, s3);                                // sq v1, 96(s3)
  c->lq(v1, 48, s1);                                // lq v1, 48(s1)
  c->sq(v1, 112, s3);                               // sq v1, 112(s3)
  //beq r0, r0, L33                                 // beq r0, r0, L33
  // nop                                            // sll r0, r0, 0
  goto block_16;                                    // branch always


block_15:
  c->daddiu(v1, s3, 96);                            // daddiu v1, s3, 96
  c->lwc1(f0, 32, s1);                              // lwc1 f0, 32(s1)
  c->swc1(f0, 0, v1);                               // swc1 f0, 0(v1)
  c->lwc1(f0, 36, s1);                              // lwc1 f0, 36(s1)
  c->swc1(f0, 4, v1);                               // swc1 f0, 4(v1)
  c->lwc1(f0, 40, s1);                              // lwc1 f0, 40(s1)
  c->swc1(f0, 8, v1);                               // swc1 f0, 8(v1)
  c->lwc1(f0, 44, s1);                              // lwc1 f0, 44(s1)
  c->lwc1(f1, 4, s1);                               // lwc1 f1, 4(s1)
  c->subs(f0, f0, f1);                              // sub.s f0, f0, f1
  c->swc1(f0, 12, v1);                              // swc1 f0, 12(v1)
  c->daddiu(v1, s3, 112);                           // daddiu v1, s3, 112
  c->lwc1(f0, 48, s1);                              // lwc1 f0, 48(s1)
  c->swc1(f0, 0, v1);                               // swc1 f0, 0(v1)
  c->lwc1(f0, 52, s1);                              // lwc1 f0, 52(s1)
  c->swc1(f0, 4, v1);                               // swc1 f0, 4(v1)
  c->lwc1(f0, 56, s1);                              // lwc1 f0, 56(s1)
  c->swc1(f0, 8, v1);                               // swc1 f0, 8(v1)
  c->lwc1(f0, 60, s1);                              // lwc1 f0, 60(s1)
  c->lwc1(f1, 4, s1);                               // lwc1 f1, 4(s1)
  c->subs(f0, f0, f1);                              // sub.s f0, f0, f1
  c->swc1(f0, 12, v1);                              // swc1 f0, 12(v1)

block_16:
  c->lw(v1, 12, s1);                                // lw v1, 12(s1)
  c->andi(v1, v1, 1);                               // andi v1, v1, 1
  if (((s64)c->sgpr64(v1)) == ((s64)0)) {           // beql v1, r0, L34
    c->daddiu(v1, s7, 4);                           // daddiu v1, s7, 4
    goto block_20;
  }

// block_18:
  c->load_symbol2(t9, cache.camera_pos);            // lw t9, camera-pos(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(a0, v0);                                 // or a0, v0, r0
  c->daddiu(v1, s3, 96);                            // daddiu v1, s3, 96
  c->lwc1(f0, 0, a0);                               // lwc1 f0, 0(a0)
  c->lwc1(f1, 4, a0);                               // lwc1 f1, 4(a0)
  c->lwc1(f2, 8, a0);                               // lwc1 f2, 8(a0)
  c->lwc1(f3, 0, v1);                               // lwc1 f3, 0(v1)
  c->lwc1(f4, 4, v1);                               // lwc1 f4, 4(v1)
  c->lwc1(f5, 8, v1);                               // lwc1 f5, 8(v1)

  // Unknown instr: mula.s f0, f3
  // Unknown instr: madda.s f1, f4
  // Unknown instr: madd.s f0, f2, f5
  c->fprs[f0] = (c->fprs[f2] * c->fprs[f5]) + (c->fprs[f1] * c->fprs[f4]) + (c->fprs[f0] * c->fprs[f3]);

  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lwc1(f1, 108, s3);                             // lwc1 f1, 108(s3)
  c->negs(f1, f1);                                  // neg.s f1, f1
  cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
  bc = !cop1_bc;                                    // bc1f L34
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  if (bc) {goto block_20;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0

block_20:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L41
  c->mov64(a0, s7);                                 // or a0, s7, r0
  if (bc) {goto block_36;}                          // branch non-likely

  c->daddiu(a0, s3, 80);                            // daddiu a0, s3, 80
  c->daddiu(v1, s3, 96);                            // daddiu v1, s3, 96
  c->lwc1(f0, 0, a0);                               // lwc1 f0, 0(a0)
  c->lwc1(f1, 4, a0);                               // lwc1 f1, 4(a0)
  c->lwc1(f2, 8, a0);                               // lwc1 f2, 8(a0)
  c->lwc1(f3, 0, v1);                               // lwc1 f3, 0(v1)
  c->lwc1(f4, 4, v1);                               // lwc1 f4, 4(v1)
  c->lwc1(f5, 8, v1);                               // lwc1 f5, 8(v1)
  // Unknown instr: mula.s f0, f3
  // Unknown instr: madda.s f1, f4
  // Unknown instr: madd.s f0, f2, f5
  c->fprs[f0] = (c->fprs[f2] * c->fprs[f5]) + (c->fprs[f1] * c->fprs[f4]) + (c->fprs[f0] * c->fprs[f3]);
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->daddiu(a0, s3, 80);                            // daddiu a0, s3, 80
  c->daddiu(v1, s3, 112);                           // daddiu v1, s3, 112
  c->lwc1(f1, 0, a0);                               // lwc1 f1, 0(a0)
  c->lwc1(f2, 4, a0);                               // lwc1 f2, 4(a0)
  c->lwc1(f3, 8, a0);                               // lwc1 f3, 8(a0)
  c->lwc1(f4, 0, v1);                               // lwc1 f4, 0(v1)
  c->lwc1(f5, 4, v1);                               // lwc1 f5, 4(v1)
  c->lwc1(f6, 8, v1);                               // lwc1 f6, 8(v1)
  // Unknown instr: mula.s f1, f4
  // Unknown instr: madda.s f2, f5
  // Unknown instr: madd.s f1, f3, f6
  c->fprs[f1] = (c->fprs[f3] * c->fprs[f6]) + (c->fprs[f2] * c->fprs[f5]) + (c->fprs[f1] * c->fprs[f4]);
  c->mfc1(v1, f1);                                  // mfc1 v1, f1
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->lwc1(f2, 108, s3);                             // lwc1 f2, 108(s3)
  cop1_bc = c->fprs[f2] < c->fprs[f0];              // c.lt.s f2, f0
  bc = cop1_bc;                                     // bc1t L35
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  if (bc) {goto block_23;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0

block_23:
  if (((s64)c->sgpr64(s7)) == ((s64)c->sgpr64(v1))) {// beql s7, v1, L36
    c->mov64(v1, v1);                               // or v1, v1, r0
    goto block_27;
  }

// block_25:
  c->lwc1(f2, 124, s3);                             // lwc1 f2, 124(s3)
  cop1_bc = c->fprs[f1] < c->fprs[f2];              // c.lt.s f1, f2
  bc = cop1_bc;                                     // bc1t L36
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  if (bc) {goto block_27;}                          // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0

block_27:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L38
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_31;}                          // branch non-likely

  c->mtc1(f2, r0);                                  // mtc1 f2, r0
  c->lwc1(f3, 28, s1);                              // lwc1 f3, 28(s1)
  cop1_bc = c->fprs[f2] < c->fprs[f3];              // c.lt.s f2, f3
  bc = !cop1_bc;                                    // bc1f L37
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_30;}                          // branch non-likely

  c->negs(f0, f0);                                  // neg.s f0, f0
  c->swc1(f0, 108, s3);                             // swc1 f0, 108(s3)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  //beq r0, r0, L38                                 // beq r0, r0, L38
  // nop                                            // sll r0, r0, 0
  goto block_31;                                    // branch always


block_30:
  c->negs(f0, f1);                                  // neg.s f0, f1
  c->swc1(f0, 124, s3);                             // swc1 f0, 124(s3)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0

block_31:
  c->lq(v1, 16, s1);                                // lq v1, 16(s1)
  c->sq(v1, 144, s3);                               // sq v1, 144(s3)
  c->daddiu(v1, s3, 128);                           // daddiu v1, s3, 128
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 0, v1);                               // swc1 f0, 0(v1)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 4, v1);                               // swc1 f0, 4(v1)
  c->lui(a0, 16256);                                // lui a0, 16256
  c->mtc1(f0, a0);                                  // mtc1 f0, a0
  c->swc1(f0, 8, v1);                               // swc1 f0, 8(v1)
  c->lui(a0, -16384);                               // lui a0, -16384
  c->mtc1(f0, a0);                                  // mtc1 f0, a0
  c->load_symbol2(a0, cache.math_camera);           // lw a0, *math-camera*(s7)
  c->lwc1(f1, 0, a0);                               // lwc1 f1, 0(a0)
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->swc1(f0, 12, v1);                              // swc1 f0, 12(v1)
  c->daddiu(v1, s3, 176);                           // daddiu v1, s3, 176
  c->sw(v1, 16, s3);                                // sw v1, 16(s3)
  c->daddiu(v1, s3, 48);                            // daddiu v1, s3, 48
  c->lhu(a0, 18, v1);                               // lhu a0, 18(v1)
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  c->sh(a0, 18, v1);                                // sh a0, 18(v1)
  c->lw(v1, 12, s1);                                // lw v1, 12(s1)
  c->andi(v1, v1, 64);                              // andi v1, v1, 64
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L39
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_33;}                          // branch non-likely

  c->load_symbol2(t9, cache.shadow_xform_verts);    // lw t9, shadow-xform-verts(s7)
  c->mov64(a0, s2);                                 // or a0, s2, r0
  c->mov64(a1, s3);                                 // or a1, s3, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->load_symbol2(t9, cache.shadow_init_vars);      // lw t9, shadow-init-vars(s7)
  c->mov64(a0, s2);                                 // or a0, s2, r0
  c->mov64(a1, s3);                                 // or a1, s3, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->load_symbol2(t9, cache.shadow_add_verts);      // lw t9, shadow-add-verts(s7)
  c->mov64(a0, s2);                                 // or a0, s2, r0
  c->mov64(a1, s3);                                 // or a1, s3, r0
  c->addiu(a3, r0, 1);                              // addiu a3, r0, 1
  c->mov64(a2, gp);                                 // or a2, gp, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(a2, v0);                                 // or a2, v0, r0
  c->load_symbol2(t9, cache.shadow_add_single_tris);// lw t9, shadow-add-single-tris(s7)
  c->mov64(a0, s2);                                 // or a0, s2, r0
  c->mov64(a1, s3);                                 // or a1, s3, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(gp, v0);                                 // or gp, v0, r0
  c->mov64(a0, gp);                                 // or a0, gp, r0
  //beq r0, r0, L41                                 // beq r0, r0, L41
  // nop                                            // sll r0, r0, 0
  goto block_36;                                    // branch always


block_33:
  c->load_symbol2(t9, cache.shadow_xform_verts);    // lw t9, shadow-xform-verts(s7)
  c->mov64(a0, s2);                                 // or a0, s2, r0
  c->mov64(a1, s3);                                 // or a1, s3, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->load_symbol2(t9, cache.shadow_init_vars);      // lw t9, shadow-init-vars(s7)
  c->mov64(a0, s2);                                 // or a0, s2, r0
  c->mov64(a1, s3);                                 // or a1, s3, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->load_symbol2(t9, cache.shadow_calc_dual_verts);// lw t9, shadow-calc-dual-verts(s7)
  c->mov64(a0, s2);                                 // or a0, s2, r0
  c->mov64(a1, s3);                                 // or a1, s3, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lw(v1, 12, s1);                                // lw v1, 12(s1)
  c->andi(v1, v1, 8);                               // andi v1, v1, 8
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L40
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_35;}                          // branch non-likely

  c->load_symbol2(t9, cache.shadow_scissor_top);    // lw t9, shadow-scissor-top(s7)
  c->mov64(a0, s2);                                 // or a0, s2, r0
  c->mov64(a1, s3);                                 // or a1, s3, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0

block_35:
  c->load_symbol2(t9, cache.shadow_scissor_edges);  // lw t9, shadow-scissor-edges(s7)
  c->mov64(a0, s2);                                 // or a0, s2, r0
  c->mov64(a1, s3);                                 // or a1, s3, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->load_symbol2(t9, cache.shadow_find_facing_single_tris);// lw t9, shadow-find-facing-single-tris(s7)
  c->mov64(a0, s2);                                 // or a0, s2, r0
  c->mov64(a1, s3);                                 // or a1, s3, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->load_symbol2(t9, cache.shadow_find_single_edges);// lw t9, shadow-find-single-edges(s7)
  c->mov64(a0, s2);                                 // or a0, s2, r0
  c->mov64(a1, s3);                                 // or a1, s3, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->load_symbol2(t9, cache.shadow_find_facing_double_tris);// lw t9, shadow-find-facing-double-tris(s7)
  c->mov64(a0, s2);                                 // or a0, s2, r0
  c->mov64(a1, s3);                                 // or a1, s3, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->load_symbol2(t9, cache.shadow_find_double_edges);// lw t9, shadow-find-double-edges(s7)
  c->mov64(a0, s2);                                 // or a0, s2, r0
  c->mov64(a1, s3);                                 // or a1, s3, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->load_symbol2(t9, cache.shadow_add_verts);      // lw t9, shadow-add-verts(s7)
  c->mov64(a0, s2);                                 // or a0, s2, r0
  c->mov64(a1, s3);                                 // or a1, s3, r0
  c->mov64(a2, gp);                                 // or a2, gp, r0
  c->addiu(a3, r0, 0);                              // addiu a3, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(a2, v0);                                 // or a2, v0, r0
  c->load_symbol2(t9, cache.shadow_add_facing_single_tris);// lw t9, shadow-add-facing-single-tris(s7)
  c->mov64(a0, s2);                                 // or a0, s2, r0
  c->mov64(a1, s3);                                 // or a1, s3, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(a2, v0);                                 // or a2, v0, r0
  c->load_symbol2(t9, cache.shadow_add_single_edges);// lw t9, shadow-add-single-edges(s7)
  c->mov64(a0, s2);                                 // or a0, s2, r0
  c->mov64(a1, s3);                                 // or a1, s3, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(a2, v0);                                 // or a2, v0, r0
  c->load_symbol2(t9, cache.shadow_add_double_tris);// lw t9, shadow-add-double-tris(s7)
  c->mov64(a0, s2);                                 // or a0, s2, r0
  c->mov64(a1, s3);                                 // or a1, s3, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(a2, v0);                                 // or a2, v0, r0
  c->load_symbol2(t9, cache.shadow_add_double_edges);// lw t9, shadow-add-double-edges(s7)
  c->mov64(a0, s2);                                 // or a0, s2, r0
  c->mov64(a1, s3);                                 // or a1, s3, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(gp, v0);                                 // or gp, v0, r0
  c->daddiu(v1, s3, 48);                            // daddiu v1, s3, 48
  c->lwu(a0, 0, v1);                                // lwu a0, 0(v1)
  c->lwu(a1, 20, s3);                               // lwu a1, 20(s3)
  c->daddu(a0, a0, a1);                             // daddu a0, a0, a1
  c->sw(a0, 0, v1);                                 // sw a0, 0(v1)
  c->lwu(a0, 4, v1);                                // lwu a0, 4(v1)
  c->lhu(a1, 16, s2);                               // lhu a1, 16(s2)
  c->daddu(a0, a0, a1);                             // daddu a0, a0, a1
  c->sw(a0, 4, v1);                                 // sw a0, 4(v1)
  c->lwu(a0, 8, v1);                                // lwu a0, 8(v1)
  c->lwu(a1, 24, s3);                               // lwu a1, 24(s3)
  c->daddu(a0, a0, a1);                             // daddu a0, a0, a1
  c->sw(a0, 8, v1);                                 // sw a0, 8(v1)
  c->lwu(a0, 12, v1);                               // lwu a0, 12(v1)
  c->lwu(a1, 28, s3);                               // lwu a1, 28(s3)
  c->daddu(a0, a0, a1);                             // daddu a0, a0, a1
  c->sw(a0, 12, v1);                                // sw a0, 12(v1)

block_36:
  c->lw(s4, 60, s4);                                // lw s4, 60(s4)

block_37:
  bc = c->sgpr64(s4) != 0;                          // bne s4, r0, L24
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v0, gp);                                 // or v0, gp, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
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
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  cache.gsf_buffer = intern_from_c("*gsf-buffer*").c();
  cache.math_camera = intern_from_c("*math-camera*").c();
  cache.shadow_debug = intern_from_c("*shadow-debug*").c();
  cache.camera_pos = intern_from_c("camera-pos").c();
  cache.debug_draw_settings = intern_from_c("debug-draw-settings").c();
  cache.flush_cache = intern_from_c("flush-cache").c();
  cache.shadow_add_double_edges = intern_from_c("shadow-add-double-edges").c();
  cache.shadow_add_double_tris = intern_from_c("shadow-add-double-tris").c();
  cache.shadow_add_facing_single_tris = intern_from_c("shadow-add-facing-single-tris").c();
  cache.shadow_add_single_edges = intern_from_c("shadow-add-single-edges").c();
  cache.shadow_add_single_tris = intern_from_c("shadow-add-single-tris").c();
  cache.shadow_add_verts = intern_from_c("shadow-add-verts").c();
  cache.shadow_calc_dual_verts = intern_from_c("shadow-calc-dual-verts").c();
  cache.shadow_find_double_edges = intern_from_c("shadow-find-double-edges").c();
  cache.shadow_find_facing_double_tris = intern_from_c("shadow-find-facing-double-tris").c();
  cache.shadow_find_facing_single_tris = intern_from_c("shadow-find-facing-single-tris").c();
  cache.shadow_find_single_edges = intern_from_c("shadow-find-single-edges").c();
  cache.shadow_init_vars = intern_from_c("shadow-init-vars").c();
  cache.shadow_scissor_edges = intern_from_c("shadow-scissor-edges").c();
  cache.shadow_scissor_top = intern_from_c("shadow-scissor-top").c();
  cache.shadow_xform_verts = intern_from_c("shadow-xform-verts").c();
  gLinkedFunctionTable.reg("shadow-execute", execute, 256);
}

} // namespace shadow_execute
} // namespace Mips2C