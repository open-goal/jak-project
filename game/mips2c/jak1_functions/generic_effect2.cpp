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

} // namespace Mips2C


