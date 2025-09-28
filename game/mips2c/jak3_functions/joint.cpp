
//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak3 {
// main function for (parent bone, transformq) -> child bone
// this is used to compute world-space bones, used for collision and similar.
// This includes the weird w divisor thing
// This does not take into account the bind pose for mesh drawing.
// (that's handled in bones.gc, which combines this with the bind pose to get the merc/pris matrix)
namespace cspace_parented_transformq_joint {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  // nop                                            // sll r0, r0, 0
  c->lw(a3, 0, a0);      // lw a3, 0(a0)
  c->lui(v1, 16256);     // lui v1, 16256
  c->lqc2(vf5, 16, a1);  // lqc2 vf5, 16(a1)
  c->mtc1(f0, v1);       // mtc1 f0, v1
  c->lw(t0, 16, a3);     // lw t0, 16(a3)
  // nop                                            // sll r0, r0, 0
  c->lw(a2, 16, a0);                          // lw a2, 16(a0)
  c->vadd(DEST::xyzw, vf6, vf5, vf5);         // vadd.xyzw vf6, vf5, vf5
  c->lwc1(f1, 64, t0);                        // lwc1 f1, 64(t0)
  c->vadd_bc(DEST::x, BC::w, vf2, vf0, vf5);  // vaddw.x vf2, vf0, vf5
  c->lqc2(vf15, 0, a1);                       // lqc2 vf15, 0(a1)
  c->vadd_bc(DEST::y, BC::z, vf2, vf0, vf5);  // vaddz.y vf2, vf0, vf5
  c->lqc2(vf1, 32, a1);                       // lqc2 vf1, 32(a1)
  c->divs_accurate(f4, f0, f1);               // div.s f4, f0, f1
  c->lqc2(vf7, 0, t0);                        // lqc2 vf7, 0(t0)
  c->vsub_bc(DEST::z, BC::y, vf2, vf0, vf5);  // vsuby.z vf2, vf0, vf5
  c->lqc2(vf8, 16, t0);                       // lqc2 vf8, 16(t0)
  // sets vf2.w to 0
  c->vsub_bc(DEST::w, BC::w, vf2, vf0, vf0);  // vsubw.w vf2, vf0, vf0
  c->lqc2(vf9, 32, t0);                       // lqc2 vf9, 32(t0)
  c->vsub_bc(DEST::x, BC::z, vf3, vf0, vf5);  // vsubz.x vf3, vf0, vf5
  c->lqc2(vf10, 48, t0);                      // lqc2 vf10, 48(t0)
  c->vadd_bc(DEST::y, BC::w, vf3, vf0, vf5);  // vaddw.y vf3, vf0, vf5
  c->lwc1(f2, 68, t0);                        // lwc1 f2, 68(t0)
  c->vadd_bc(DEST::z, BC::x, vf3, vf0, vf5);  // vaddx.z vf3, vf0, vf5
  c->sqc2(vf1, 64, a2);                       // sqc2 vf1, 64(a2)
  c->vsub_bc(DEST::w, BC::w, vf3, vf0, vf0);  // vsubw.w vf3, vf0, vf0
  c->lwc1(f3, 72, t0);                        // lwc1 f3, 72(t0)
  c->vadd_bc(DEST::x, BC::y, vf4, vf0, vf5);  // vaddy.x vf4, vf0, vf5
  c->lw(v1, 76, t0);                          // lw v1, 76(t0)
  c->vsub_bc(DEST::y, BC::x, vf4, vf0, vf5);  // vsubx.y vf4, vf0, vf5
  c->mfc1(t1, f4);                            // mfc1 t1, f4
  c->vadd_bc(DEST::z, BC::w, vf4, vf0, vf5);  // vaddw.z vf4, vf0, vf5
  c->divs_accurate(f4, f0, f2);               // div.s f4, f0, f2
  c->vsub_bc(DEST::w, BC::w, vf4, vf0, vf0);  // vsubw.w vf4, vf0, vf0
  c->vopmula(vf6, vf2);                       // vopmula.xyz acc, vf6, vf2
  c->vopmsub(vf2, vf2, vf6);                  // vopmsub.xyz vf2, vf2, vf6
  c->vopmula(vf6, vf3);                       // vopmula.xyz acc, vf6, vf3
  c->vopmsub(vf3, vf3, vf6);                  // vopmsub.xyz vf3, vf3, vf6
  c->vopmula(vf6, vf4);                       // vopmula.xyz acc, vf6, vf4
  c->vopmsub(vf4, vf4, vf6);                  // vopmsub.xyz vf4, vf4, vf6
  c->vadd_bc(DEST::x, BC::w, vf2, vf2, vf0);  // vaddw.x vf2, vf2, vf0
  c->vadd_bc(DEST::y, BC::w, vf3, vf3, vf0);  // vaddw.y vf3, vf3, vf0
  c->vadd_bc(DEST::z, BC::w, vf4, vf4, vf0);  // vaddw.z vf4, vf4, vf0
  c->mfc1(t2, f4);                            // mfc1 t2, f4
  bc = c->sgpr64(v1) != 0;                    // bne v1, r0, L50
  c->divs_accurate(f4, f0, f3);               // div.s f4, f0, f3
  if (bc) {
    goto block_2;
  }  // branch non-likely

  c->vmul_bc(DEST::xyzw, BC::x, vf2, vf2, vf1);     // vmulx.xyzw vf2, vf2, vf1
  c->vmul_bc(DEST::xyzw, BC::y, vf3, vf3, vf1);     // vmuly.xyzw vf3, vf3, vf1
  c->vmul_bc(DEST::xyzw, BC::z, vf4, vf4, vf1);     // vmulz.xyzw vf4, vf4, vf1
  c->vmula_bc(DEST::xyzw, BC::x, vf7, vf2);         // vmulax.xyzw acc, vf7, vf2
  c->vmadda_bc(DEST::xyzw, BC::y, vf8, vf2);        // vmadday.xyzw acc, vf8, vf2
  c->vmadda_bc(DEST::xyzw, BC::z, vf9, vf2);        // vmaddaz.xyzw acc, vf9, vf2
  c->vmadd_bc(DEST::xyzw, BC::w, vf11, vf10, vf2);  // vmaddw.xyzw vf11, vf10, vf2
  c->vmula_bc(DEST::xyzw, BC::x, vf7, vf3);         // vmulax.xyzw acc, vf7, vf3
  c->vmadda_bc(DEST::xyzw, BC::y, vf8, vf3);        // vmadday.xyzw acc, vf8, vf3
  c->vmadda_bc(DEST::xyzw, BC::z, vf9, vf3);        // vmaddaz.xyzw acc, vf9, vf3
  c->vmadd_bc(DEST::xyzw, BC::w, vf12, vf10, vf3);  // vmaddw.xyzw vf12, vf10, vf3
  c->vmula_bc(DEST::xyzw, BC::x, vf7, vf4);         // vmulax.xyzw acc, vf7, vf4
  c->vmadda_bc(DEST::xyzw, BC::y, vf8, vf4);        // vmadday.xyzw acc, vf8, vf4
  c->vmadda_bc(DEST::xyzw, BC::z, vf9, vf4);        // vmaddaz.xyzw acc, vf9, vf4
  c->vmadd_bc(DEST::xyzw, BC::w, vf13, vf10, vf4);  // vmaddw.xyzw vf13, vf10, vf4
  c->vmula_bc(DEST::xyzw, BC::x, vf7, vf15);        // vmulax.xyzw acc, vf7, vf15
  c->vmadda_bc(DEST::xyzw, BC::y, vf8, vf15);       // vmadday.xyzw acc, vf8, vf15
  c->vmadda_bc(DEST::xyzw, BC::z, vf9, vf15);       // vmaddaz.xyzw acc, vf9, vf15
  c->vmadd_bc(DEST::xyzw, BC::w, vf14, vf10, vf0);  // vmaddw.xyzw vf14, vf10, vf0
  c->sqc2(vf11, 0, a2);                             // sqc2 vf11, 0(a2)
  c->sqc2(vf12, 16, a2);                            // sqc2 vf12, 16(a2)
  c->sqc2(vf13, 32, a2);                            // sqc2 vf13, 32(a2)
  c->sqc2(vf14, 48, a2);                            // sqc2 vf14, 48(a2)
  // jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;  // return

block_2:
  c->pextlw(t1, t2, t1);                         // pextlw t1, t2, t1
  c->vmul_bc(DEST::xyzw, BC::x, vf2, vf2, vf1);  // vmulx.xyzw vf2, vf2, vf1
  c->vmul_bc(DEST::xyzw, BC::y, vf3, vf3, vf1);  // vmuly.xyzw vf3, vf3, vf1
  c->vmul_bc(DEST::xyzw, BC::z, vf4, vf4, vf1);  // vmulz.xyzw vf4, vf4, vf1
  // here, f4 is 1/scale. Sometimes the scale out of the joint compression code is slightly negative
  // this leads to mfc1 sign extending 1's into the upper 32 bits of t3 (this is weirdly how the ps2
  // does it).
  c->mfc1(t3, f4);  // mfc1 t3, f4
  // and this brings those ones into bits 96-128
  c->pcpyld(t1, t3, t1);  // pcpyld t1, t3, t1
  // so here, vf16.w is usually 0, except for when the scale is negative, then it's 0xffff'ffff
  // (NaN on x86, -BIG on PS2)
  c->mov128_vf_gpr(vf16, t1);  // qmtc2.i vf16, t1
  // here, vf2/3/4's w's are all 0. On PS2, this always keeps them as 0.
  // but on x86, this propagates NaNs: 0 * NaN = NaN.
  // so:
  c->vfs[vf16].vf.w() = 0;  // PATCH to clear invalid float that will be multiplied by 0 below
  // (this might seem weird because the multiplication sequence could have 3 instructions removed
  //  because we know that vf2/3/4.w are all 0. But maybe this is just copy-pasted, or it didn't
  //  really matter because it would have stalled in place of that 1 cycle instruction because
  //  multiplication latency is 4).

  c->vmul(DEST::xyzw, vf2, vf2, vf16);              // vmul.xyzw vf2, vf2, vf16
  c->vmul(DEST::xyzw, vf3, vf3, vf16);              // vmul.xyzw vf3, vf3, vf16
  c->vmul(DEST::xyzw, vf4, vf4, vf16);              // vmul.xyzw vf4, vf4, vf16
  c->vmula_bc(DEST::xyzw, BC::x, vf7, vf2);         // vmulax.xyzw acc, vf7, vf2
  c->vmadda_bc(DEST::xyzw, BC::y, vf8, vf2);        // vmadday.xyzw acc, vf8, vf2
  c->vmadda_bc(DEST::xyzw, BC::z, vf9, vf2);        // vmaddaz.xyzw acc, vf9, vf2
  c->vmadd_bc(DEST::xyzw, BC::w, vf11, vf10, vf2);  // vmaddw.xyzw vf11, vf10, vf2
  c->vmula_bc(DEST::xyzw, BC::x, vf7, vf3);         // vmulax.xyzw acc, vf7, vf3
  c->vmadda_bc(DEST::xyzw, BC::y, vf8, vf3);        // vmadday.xyzw acc, vf8, vf3
  c->vmadda_bc(DEST::xyzw, BC::z, vf9, vf3);        // vmaddaz.xyzw acc, vf9, vf3
  c->vmadd_bc(DEST::xyzw, BC::w, vf12, vf10, vf3);  // vmaddw.xyzw vf12, vf10, vf3
  c->vmula_bc(DEST::xyzw, BC::x, vf7, vf4);         // vmulax.xyzw acc, vf7, vf4
  c->vmadda_bc(DEST::xyzw, BC::y, vf8, vf4);        // vmadday.xyzw acc, vf8, vf4
  c->vmadda_bc(DEST::xyzw, BC::z, vf9, vf4);        // vmaddaz.xyzw acc, vf9, vf4
  c->vmadd_bc(DEST::xyzw, BC::w, vf13, vf10, vf4);  // vmaddw.xyzw vf13, vf10, vf4
  c->vmula_bc(DEST::xyzw, BC::x, vf7, vf15);        // vmulax.xyzw acc, vf7, vf15
  c->vmadda_bc(DEST::xyzw, BC::y, vf8, vf15);       // vmadday.xyzw acc, vf8, vf15
  c->vmadda_bc(DEST::xyzw, BC::z, vf9, vf15);       // vmaddaz.xyzw acc, vf9, vf15
  c->vmadd_bc(DEST::xyzw, BC::w, vf14, vf10, vf0);  // vmaddw.xyzw vf14, vf10, vf0
  c->sqc2(vf11, 0, a2);                             // sqc2 vf11, 0(a2)
  c->sqc2(vf12, 16, a2);                            // sqc2 vf12, 16(a2)
  c->sqc2(vf13, 32, a2);                            // sqc2 vf13, 32(a2)
  c->sqc2(vf14, 48, a2);                            // sqc2 vf14, 48(a2)
  // jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;  // return

  // jr ra                                           // jr ra
  c->daddu(sp, sp, r0);  // daddu sp, sp, r0
  goto end_of_function;  // return

// nop                                            // sll r0, r0, 0
// nop                                            // sll r0, r0, 0
// nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  gLinkedFunctionTable.reg("cspace<-parented-transformq-joint!", execute, 128);
}

}  // namespace cspace_parented_transformq_joint
}  // namespace Mips2C::jak3
   // add cspace<_parented_transformq_joint::link to the link callback table for the object file.
   // FWD DEC: