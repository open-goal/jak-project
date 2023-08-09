//--------------------------MIPS2C---------------------
#include "game/kernel/jak2/kscheme.h"
#include "game/mips2c/mips2c_private.h"
using namespace jak2;
// clang-format off

namespace {
struct Cache {
  void* clear_frame_accumulator; // clear-frame-accumulator
  void* decompress_fixed_data_to_accumulator; // decompress-fixed-data-to-accumulator
  void* decompress_frame_data_pair_to_accumulator; // decompress-frame-data-pair-to-accumulator
  void* decompress_frame_data_to_accumulator; // decompress-frame-data-to-accumulator
  void* normalize_frame_quaternions; // normalize-frame-quaternions
  void* fake_scratchpad_data;  // *fake-scratchpad-data*

} cache;
}

namespace Mips2C::jak2 {
namespace decompress_frame_data_pair_to_accumulator {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 next_block = 0;
  while(true) {
    switch(next_block) {

      case 0:
        next_block = 1;
        c->daddiu(sp, sp, -64);                           // daddiu sp, sp, -64
        c->sq(a0, 0, sp);                                 // sq a0, 0(sp)
        c->sq(t7, 16, sp);                                // sq t7, 16(sp)
        c->sq(s0, 32, sp);                                // sq s0, 32(sp)
        c->sq(s1, 48, sp);                                // sq s1, 48(sp)
        c->mov128_vf_gpr(vf11, t0);                       // qmtc2.i vf11, t0
        c->daddu(a3, a3, a1);                             // daddu a3, a3, a1
        c->mov128_vf_gpr(vf13, a2);                       // qmtc2.i vf13, a2
        //c->lui(t2, 28672);                                // lui t2, 28672
        get_fake_spad_addr2(t2, cache.fake_scratchpad_data, 0, c);
        c->lw(t4, 0, a1);                                 // lw t4, 0(a1)
        c->daddiu(v1, a1, 16);                            // daddiu v1, a1, 16
        c->lw(t5, 4, a1);                                 // lw t5, 4(a1)
        c->daddu(s5, t1, r0);                             // daddu s5, t1, r0
        c->lw(t6, 8, a1);                                 // lw t6, 8(a1)
        c->daddu(t4, t4, v1);                             // daddu t4, t4, v1
        c->vsub_bc(DEST::w, BC::x, vf11, vf0, vf11);      // vsubx.w vf11, vf0, vf11
        c->lw(t7, 0, a3);                                 // lw t7, 0(a3)
        c->vmul_bc(DEST::xyzw, BC::x, vf13, vf14, vf13);  // vmulx.xyzw vf13, vf14, vf13
        c->daddiu(t2, t2, 1744);                          // daddiu t2, t2, 1744 // JAK 2 CHANGE
        c->lw(s2, 56, t1);                                // lw s2, 56(t1)
        c->daddu(t5, t5, v1);                             // daddu t5, t5, v1
        c->lw(s4, 60, t1);                                // lw s4, 60(t1)
        c->daddu(t6, t6, v1);                             // daddu t6, t6, v1
        c->addiu(s3, r0, 8);                              // addiu s3, r0, 8
        c->daddiu(s5, s5, 4);                             // daddiu s5, s5, 4
        c->vmul_bc(DEST::xy, BC::w, vf13, vf13, vf11);    // vmulw.xy vf13, vf13, vf11
        c->lw(s0, 4, a3);                                 // lw s0, 4(a3)
        c->vmul_bc(DEST::zw, BC::x, vf13, vf13, vf11);    // vmulx.zw vf13, vf13, vf11
        c->lw(s1, 8, a3);                                 // lw s1, 8(a3)
        // nop                                            // sll r0, r0, 0
        c->daddiu(v1, a3, 16);                            // daddiu v1, a3, 16
        c->daddu(t7, t7, v1);                             // daddu t7, t7, v1
        c->daddu(s0, s0, v1);                             // daddu s0, s0, v1
        c->daddu(s1, s1, v1);                             // daddu s1, s1, v1
        // nop                                            // sll r0, r0, 0
        c->andi(t3, s4, 1);                               // andi t3, s4, 1
        // nop                                            // sll r0, r0, 0
        bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L10
        // nop                                            // sll r0, r0, 0
        if (bc) {next_block = 2;}                         // branch non-likely

        break;

      case 1:
        next_block = 2;
        c->lqc2(vf1, 0, t4);                              // lqc2 vf1, 0(t4)
        c->lqc2(vf2, 16, t4);                             // lqc2 vf2, 16(t4)
        c->lqc2(vf3, 32, t4);                             // lqc2 vf3, 32(t4)
        c->lqc2(vf4, 48, t4);                             // lqc2 vf4, 48(t4)
        c->lqc2(vf5, 0, t7);                              // lqc2 vf5, 0(t7)
        c->lqc2(vf6, 16, t7);                             // lqc2 vf6, 16(t7)
        c->lqc2(vf7, 32, t7);                             // lqc2 vf7, 32(t7)
        c->lqc2(vf8, 48, t7);                             // lqc2 vf8, 48(t7)
        c->lqc2(vf9, 0, a0);                              // lqc2 vf9, 0(a0)
        c->daddiu(t4, t4, 64);                            // daddiu t4, t4, 64
        c->lqc2(vf10, 16, a0);                            // lqc2 vf10, 16(a0)
        c->daddiu(t7, t7, 64);                            // daddiu t7, t7, 64
        c->lqc2(vf11, 32, a0);                            // lqc2 vf11, 32(a0)
        c->lqc2(vf12, 48, a0);                            // lqc2 vf12, 48(a0)
        c->vmula_bc(DEST::xyzw, BC::w, vf9, vf0);         // vmulaw.xyzw acc, vf9, vf0
        c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf13);       // vmaddax.xyzw acc, vf1, vf13
        c->vmadd_bc(DEST::xyzw, BC::w, vf9, vf5, vf13);   // vmaddw.xyzw vf9, vf5, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf10, vf0);        // vmulaw.xyzw acc, vf10, vf0
        c->vmadda_bc(DEST::xyzw, BC::x, vf2, vf13);       // vmaddax.xyzw acc, vf2, vf13
        c->vmadd_bc(DEST::xyzw, BC::w, vf10, vf6, vf13);  // vmaddw.xyzw vf10, vf6, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf11, vf0);        // vmulaw.xyzw acc, vf11, vf0
        c->vmadda_bc(DEST::xyzw, BC::x, vf3, vf13);       // vmaddax.xyzw acc, vf3, vf13
        c->vmadd_bc(DEST::xyzw, BC::w, vf11, vf7, vf13);  // vmaddw.xyzw vf11, vf7, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf12, vf0);        // vmulaw.xyzw acc, vf12, vf0
        c->vmadda_bc(DEST::xyzw, BC::x, vf4, vf13);       // vmaddax.xyzw acc, vf4, vf13
        c->vmadd_bc(DEST::xyzw, BC::w, vf12, vf8, vf13);  // vmaddw.xyzw vf12, vf8, vf13
        c->sqc2(vf9, 0, a0);                              // sqc2 vf9, 0(a0)
        c->sqc2(vf10, 16, a0);                            // sqc2 vf10, 16(a0)
        c->sqc2(vf11, 32, a0);                            // sqc2 vf11, 32(a0)
        c->sqc2(vf12, 48, a0);                            // sqc2 vf12, 48(a0)

      case 2:
        next_block = 3;
        c->andi(t3, s4, 2);                               // andi t3, s4, 2
        c->daddiu(a0, a0, 64);                            // daddiu a0, a0, 64
        bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L11
        // nop                                            // sll r0, r0, 0
        if (bc) {next_block = 4;}                         // branch non-likely

        break;

      case 3:
        next_block = 4;
        c->lqc2(vf1, 0, t4);                              // lqc2 vf1, 0(t4)
        c->lqc2(vf2, 16, t4);                             // lqc2 vf2, 16(t4)
        c->lqc2(vf3, 32, t4);                             // lqc2 vf3, 32(t4)
        c->lqc2(vf4, 48, t4);                             // lqc2 vf4, 48(t4)
        c->lqc2(vf5, 0, t7);                              // lqc2 vf5, 0(t7)
        c->lqc2(vf6, 16, t7);                             // lqc2 vf6, 16(t7)
        c->lqc2(vf7, 32, t7);                             // lqc2 vf7, 32(t7)
        c->lqc2(vf8, 48, t7);                             // lqc2 vf8, 48(t7)
        c->lqc2(vf9, 0, a0);                              // lqc2 vf9, 0(a0)
        c->daddiu(t4, t4, 64);                            // daddiu t4, t4, 64
        c->lqc2(vf10, 16, a0);                            // lqc2 vf10, 16(a0)
        c->daddiu(t7, t7, 64);                            // daddiu t7, t7, 64
        c->lqc2(vf11, 32, a0);                            // lqc2 vf11, 32(a0)
        c->lqc2(vf12, 48, a0);                            // lqc2 vf12, 48(a0)
        c->vmula_bc(DEST::xyzw, BC::w, vf9, vf0);         // vmulaw.xyzw acc, vf9, vf0
        c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf13);       // vmaddax.xyzw acc, vf1, vf13
        c->vmadd_bc(DEST::xyzw, BC::w, vf9, vf5, vf13);   // vmaddw.xyzw vf9, vf5, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf10, vf0);        // vmulaw.xyzw acc, vf10, vf0
        c->vmadda_bc(DEST::xyzw, BC::x, vf2, vf13);       // vmaddax.xyzw acc, vf2, vf13
        c->vmadd_bc(DEST::xyzw, BC::w, vf10, vf6, vf13);  // vmaddw.xyzw vf10, vf6, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf11, vf0);        // vmulaw.xyzw acc, vf11, vf0
        c->vmadda_bc(DEST::xyzw, BC::x, vf3, vf13);       // vmaddax.xyzw acc, vf3, vf13
        c->vmadd_bc(DEST::xyzw, BC::w, vf11, vf7, vf13);  // vmaddw.xyzw vf11, vf7, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf12, vf0);        // vmulaw.xyzw acc, vf12, vf0
        c->vmadda_bc(DEST::xyzw, BC::x, vf4, vf13);       // vmaddax.xyzw acc, vf4, vf13
        c->vmadd_bc(DEST::xyzw, BC::w, vf12, vf8, vf13);  // vmaddw.xyzw vf12, vf8, vf13
        c->sqc2(vf9, 0, a0);                              // sqc2 vf9, 0(a0)
        c->sqc2(vf10, 16, a0);                            // sqc2 vf10, 16(a0)
        c->sqc2(vf11, 32, a0);                            // sqc2 vf11, 32(a0)
        c->sqc2(vf12, 48, a0);                            // sqc2 vf12, 48(a0)

      case 4:
        next_block = 5;
        c->lw(s4, -4, s5);                                // lw s4, -4(s5)
        c->daddiu(a0, a0, 64);                            // daddiu a0, a0, 64

      case 5:
        next_block = 6;
        c->andi(t3, s4, 15);                              // andi t3, s4, 15
        c->sra(s4, s4, 4);                                // sra s4, s4, 4
        c->sll(t3, t3, 2);                                // sll t3, t3, 2
        c->daddiu(s3, s3, -1);                            // daddiu s3, s3, -1
        c->daddu(t3, t3, t2);                             // daddu t3, t3, t2
        c->daddiu(s2, s2, -1);                            // daddiu s2, s2, -1
        c->lw(t3, 0, t3);                                 // lw t3, 0(t3)
        // nop                                            // sll r0, r0, 0
        // nop                                            // sll r0, r0, 0
        // nop                                            // sll r0, r0, 0
        next_block = 0x4d7666d9 ^ c->gprs[t3].du32[0];    // jr t3
        ASSERT(next_block < 33);
        break;
        // nop                                            // sll r0, r0, 0

      case 6:
        next_block = 7;
        bc = c->sgpr64(s2) == 0;                          // beq s2, r0, L20
        c->daddiu(a0, a0, 48);                            // daddiu a0, a0, 48
        if (bc) {next_block = 32;}                        // branch non-likely

        break;

      case 7:
        next_block = 8;
        bc = c->sgpr64(s3) != 0;                          // bne s3, r0, L12
        // nop                                            // sll r0, r0, 0
        if (bc) {next_block = 5;}                         // branch non-likely

        break;

      case 8:
        next_block = 9;
        c->lw(s4, 0, s5);                                 // lw s4, 0(s5)
        c->daddiu(s5, s5, 4);                             // daddiu s5, s5, 4
        //beq r0, r0, L12                                 // beq r0, r0, L12
        c->addiu(s3, r0, 8);                              // addiu s3, r0, 8
        next_block = 5;                                   // branch always

        break;

      case 9:
        next_block = 10;
        c->lw(s6, 0, t5);                                 // lw s6, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(gp, 0, t6);                                 // lh gp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->lqc2(vf3, 0, a0);                              // lqc2 vf3, 0(a0)
        c->pextlw(s6, gp, s6);                            // pextlw s6, gp, s6
        c->pextlh(s6, s6, r0);                            // pextlh s6, s6, r0
        c->psraw(s6, s6, 16);                             // psraw s6, s6, 16
        c->mov128_vf_gpr(vf1, s6);                        // qmtc2.i vf1, s6
        c->lw(s6, 0, s0);                                 // lw s6, 0(s0)
        c->daddiu(s0, s0, 4);                             // daddiu s0, s0, 4
        c->lh(gp, 0, s1);                                 // lh gp, 0(s1)
        c->daddiu(s1, s1, 2);                             // daddiu s1, s1, 2
        c->pextlw(s6, gp, s6);                            // pextlw s6, gp, s6
        c->pextlh(s6, s6, r0);                            // pextlh s6, s6, r0
        c->psraw(s6, s6, 16);                             // psraw s6, s6, 16
        c->mov128_vf_gpr(vf2, s6);                        // qmtc2.i vf2, s6
        c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
        c->vitof0(DEST::xyzw, vf2, vf2);                  // vitof0.xyzw vf2, vf2
        c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
        c->vmadda_bc(DEST::xyzw, BC::y, vf1, vf13);       // vmadday.xyzw acc, vf1, vf13
        c->vmadd_bc(DEST::xyzw, BC::z, vf3, vf2, vf13);   // vmaddz.xyzw vf3, vf2, vf13
        c->sqc2(vf3, 0, a0);                              // sqc2 vf3, 0(a0)
        //beq r0, r0, L13                                 // beq r0, r0, L13
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 10:
        next_block = 11;
        c->ld(s6, 0, t4);                                 // ld s6, 0(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lw(gp, 0, t5);                                 // lw gp, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lqc2(vf3, 0, a0);                              // lqc2 vf3, 0(a0)
        c->pcpyld(s6, gp, s6);                            // pcpyld s6, gp, s6
        c->mov128_vf_gpr(vf1, s6);                        // qmtc2.i vf1, s6
        c->ld(s6, 0, t7);                                 // ld s6, 0(t7)
        c->daddiu(t7, t7, 8);                             // daddiu t7, t7, 8
        c->lw(gp, 0, s0);                                 // lw gp, 0(s0)
        c->daddiu(s0, s0, 4);                             // daddiu s0, s0, 4
        c->pcpyld(s6, gp, s6);                            // pcpyld s6, gp, s6
        c->mov128_vf_gpr(vf2, s6);                        // qmtc2.i vf2, s6
        c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
        c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf13);       // vmaddax.xyzw acc, vf1, vf13
        c->vmadd_bc(DEST::xyzw, BC::w, vf3, vf2, vf13);   // vmaddw.xyzw vf3, vf2, vf13
        c->sqc2(vf3, 0, a0);                              // sqc2 vf3, 0(a0)
        //beq r0, r0, L13                                 // beq r0, r0, L13
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 11:
        next_block = 12;
        c->ld(t9, 0, t4);                                 // ld t9, 0(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lqc2(vf6, 16, a0);                             // lqc2 vf6, 16(a0)
        c->pextlh(t9, t9, r0);                            // pextlh t9, t9, r0
        c->psraw(t9, t9, 16);                             // psraw t9, t9, 16
        c->mov128_vf_gpr(vf4, t9);                        // qmtc2.i vf4, t9
        c->ld(t9, 0, t7);                                 // ld t9, 0(t7)
        c->daddiu(t7, t7, 8);                             // daddiu t7, t7, 8
        c->pextlh(t9, t9, r0);                            // pextlh t9, t9, r0
        c->psraw(t9, t9, 16);                             // psraw t9, t9, 16
        c->mov128_vf_gpr(vf5, t9);                        // qmtc2.i vf5, t9
        c->vitof15(DEST::xyzw, vf4, vf4);                 // vitof15.xyzw vf4, vf4
        c->vitof15(DEST::xyzw, vf5, vf5);                 // vitof15.xyzw vf5, vf5
        c->vmula_bc(DEST::xyzw, BC::x, vf4, vf13);        // vmulax.xyzw acc, vf4, vf13
        c->vmadd_bc(DEST::xyzw, BC::w, vf4, vf5, vf13);   // vmaddw.xyzw vf4, vf5, vf13
        c->vmul(DEST::xyzw, vf10, vf4, vf6);              // vmul.xyzw vf10, vf4, vf6
        c->vmula_bc(DEST::xyzw, BC::w, vf10, vf0);        // vmulaw.xyzw acc, vf10, vf0
        c->vmadda_bc(DEST::xyzw, BC::z, vf0, vf10);       // vmaddaz.xyzw acc, vf0, vf10
        c->vmadda_bc(DEST::xyzw, BC::y, vf0, vf10);       // vmadday.xyzw acc, vf0, vf10
        c->vmadd_bc(DEST::xyzw, BC::x, vf10, vf0, vf10);  // vmaddx.xyzw vf10, vf0, vf10
        c->mov128_gpr_vf(t9, vf10);                       // qmfc2.i t9, vf10
        c->pcpyud(t9, t9, r0);                            // pcpyud t9, t9, r0
        if (((s64)c->sgpr64(t9)) < 0) {                   // bltzl t9, L14
          c->vsub(DEST::xyzw, vf4, vf15, vf4);            // vsub.xyzw vf4, vf15, vf4
          next_block = 13;
          break;
        }

      case 13:
        next_block = 14;
        c->vadd(DEST::xyzw, vf6, vf6, vf4);               // vadd.xyzw vf6, vf6, vf4
        c->sqc2(vf6, 16, a0);                             // sqc2 vf6, 16(a0)
        //beq r0, r0, L13                                 // beq r0, r0, L13
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 14:
        next_block = 15;
        c->ld(t9, 0, t4);                                 // ld t9, 0(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lqc2(vf6, 16, a0);                             // lqc2 vf6, 16(a0)
        c->pextlh(t9, t9, r0);                            // pextlh t9, t9, r0
        c->psraw(t9, t9, 16);                             // psraw t9, t9, 16
        c->mov128_vf_gpr(vf4, t9);                        // qmtc2.i vf4, t9
        c->ld(t9, 0, t7);                                 // ld t9, 0(t7)
        c->daddiu(t7, t7, 8);                             // daddiu t7, t7, 8
        c->pextlh(t9, t9, r0);                            // pextlh t9, t9, r0
        c->psraw(t9, t9, 16);                             // psraw t9, t9, 16
        c->mov128_vf_gpr(vf5, t9);                        // qmtc2.i vf5, t9
        c->vitof15(DEST::xyzw, vf4, vf4);                 // vitof15.xyzw vf4, vf4
        c->lw(s6, 0, t5);                                 // lw s6, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(gp, 0, t6);                                 // lh gp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->vitof15(DEST::xyzw, vf5, vf5);                 // vitof15.xyzw vf5, vf5
        c->lqc2(vf3, 0, a0);                              // lqc2 vf3, 0(a0)
        c->pextlw(s6, gp, s6);                            // pextlw s6, gp, s6
        c->vmula_bc(DEST::xyzw, BC::x, vf4, vf13);        // vmulax.xyzw acc, vf4, vf13
        c->vmadd_bc(DEST::xyzw, BC::w, vf4, vf5, vf13);   // vmaddw.xyzw vf4, vf5, vf13
        c->pextlh(s6, s6, r0);                            // pextlh s6, s6, r0
        c->psraw(s6, s6, 16);                             // psraw s6, s6, 16
        c->mov128_vf_gpr(vf1, s6);                        // qmtc2.i vf1, s6
        c->vmul(DEST::xyzw, vf10, vf4, vf6);              // vmul.xyzw vf10, vf4, vf6
        c->lw(s6, 0, s0);                                 // lw s6, 0(s0)
        c->daddiu(s0, s0, 4);                             // daddiu s0, s0, 4
        c->lh(gp, 0, s1);                                 // lh gp, 0(s1)
        c->daddiu(s1, s1, 2);                             // daddiu s1, s1, 2
        c->vmula_bc(DEST::xyzw, BC::w, vf10, vf0);        // vmulaw.xyzw acc, vf10, vf0
        c->vmadda_bc(DEST::xyzw, BC::z, vf0, vf10);       // vmaddaz.xyzw acc, vf0, vf10
        c->vmadda_bc(DEST::xyzw, BC::y, vf0, vf10);       // vmadday.xyzw acc, vf0, vf10
        c->vmadd_bc(DEST::xyzw, BC::x, vf10, vf0, vf10);  // vmaddx.xyzw vf10, vf0, vf10
        c->pextlw(s6, gp, s6);                            // pextlw s6, gp, s6
        c->pextlh(s6, s6, r0);                            // pextlh s6, s6, r0
        c->psraw(s6, s6, 16);                             // psraw s6, s6, 16
        c->mov128_gpr_vf(t9, vf10);                       // qmfc2.i t9, vf10
        c->mov128_vf_gpr(vf2, s6);                        // qmtc2.i vf2, s6
        c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
        c->pcpyud(t9, t9, r0);                            // pcpyud t9, t9, r0
        if (((s64)c->sgpr64(t9)) < 0) {                   // bltzl t9, L15
          c->vsub(DEST::xyzw, vf4, vf15, vf4);            // vsub.xyzw vf4, vf15, vf4
          next_block = 16;
          break;
        }

      case 16:
        next_block = 17;
        c->vitof0(DEST::xyzw, vf2, vf2);                  // vitof0.xyzw vf2, vf2
        c->vadd(DEST::xyzw, vf6, vf6, vf4);               // vadd.xyzw vf6, vf6, vf4
        c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
        c->vmadda_bc(DEST::xyzw, BC::y, vf1, vf13);       // vmadday.xyzw acc, vf1, vf13
        c->vmadd_bc(DEST::xyzw, BC::z, vf3, vf2, vf13);   // vmaddz.xyzw vf3, vf2, vf13
        c->sqc2(vf6, 16, a0);                             // sqc2 vf6, 16(a0)
        c->sqc2(vf3, 0, a0);                              // sqc2 vf3, 0(a0)
        //beq r0, r0, L13                                 // beq r0, r0, L13
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 17:
        next_block = 18;
        c->ld(t9, 8, t4);                                 // ld t9, 8(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lqc2(vf6, 16, a0);                             // lqc2 vf6, 16(a0)
        c->pextlh(t9, t9, r0);                            // pextlh t9, t9, r0
        c->psraw(t9, t9, 16);                             // psraw t9, t9, 16
        c->mov128_vf_gpr(vf4, t9);                        // qmtc2.i vf4, t9
        c->ld(t9, 8, t7);                                 // ld t9, 8(t7)
        c->daddiu(t7, t7, 8);                             // daddiu t7, t7, 8
        c->pextlh(t9, t9, r0);                            // pextlh t9, t9, r0
        c->psraw(t9, t9, 16);                             // psraw t9, t9, 16
        c->mov128_vf_gpr(vf5, t9);                        // qmtc2.i vf5, t9
        c->vitof15(DEST::xyzw, vf4, vf4);                 // vitof15.xyzw vf4, vf4
        c->ld(s6, -8, t4);                                // ld s6, -8(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lw(gp, 0, t5);                                 // lw gp, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->vitof15(DEST::xyzw, vf5, vf5);                 // vitof15.xyzw vf5, vf5
        c->lqc2(vf3, 0, a0);                              // lqc2 vf3, 0(a0)
        c->pcpyld(s6, gp, s6);                            // pcpyld s6, gp, s6
        c->vmula_bc(DEST::xyzw, BC::x, vf4, vf13);        // vmulax.xyzw acc, vf4, vf13
        c->vmadd_bc(DEST::xyzw, BC::w, vf4, vf5, vf13);   // vmaddw.xyzw vf4, vf5, vf13
        c->mov128_vf_gpr(vf1, s6);                        // qmtc2.i vf1, s6
        c->vmul(DEST::xyzw, vf10, vf4, vf6);              // vmul.xyzw vf10, vf4, vf6
        c->ld(s6, -8, t7);                                // ld s6, -8(t7)
        c->daddiu(t7, t7, 8);                             // daddiu t7, t7, 8
        c->lw(gp, 0, s0);                                 // lw gp, 0(s0)
        c->daddiu(s0, s0, 4);                             // daddiu s0, s0, 4
        c->vmula_bc(DEST::xyzw, BC::w, vf10, vf0);        // vmulaw.xyzw acc, vf10, vf0
        c->vmadda_bc(DEST::xyzw, BC::z, vf0, vf10);       // vmaddaz.xyzw acc, vf0, vf10
        c->vmadda_bc(DEST::xyzw, BC::y, vf0, vf10);       // vmadday.xyzw acc, vf0, vf10
        c->vmadd_bc(DEST::xyzw, BC::x, vf10, vf0, vf10);  // vmaddx.xyzw vf10, vf0, vf10
        c->pcpyld(s6, gp, s6);                            // pcpyld s6, gp, s6
        c->mov128_gpr_vf(t9, vf10);                       // qmfc2.i t9, vf10
        c->mov128_vf_gpr(vf2, s6);                        // qmtc2.i vf2, s6
        c->pcpyud(t9, t9, r0);                            // pcpyud t9, t9, r0
        if (((s64)c->sgpr64(t9)) < 0) {                   // bltzl t9, L16
          c->vsub(DEST::xyzw, vf4, vf15, vf4);            // vsub.xyzw vf4, vf15, vf4
          next_block = 19;
          break;
        }

      case 19:
        next_block = 20;
        c->vadd(DEST::xyzw, vf6, vf6, vf4);               // vadd.xyzw vf6, vf6, vf4
        c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
        c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf13);       // vmaddax.xyzw acc, vf1, vf13
        c->vmadd_bc(DEST::xyzw, BC::w, vf3, vf2, vf13);   // vmaddw.xyzw vf3, vf2, vf13
        c->sqc2(vf6, 16, a0);                             // sqc2 vf6, 16(a0)
        c->sqc2(vf3, 0, a0);                              // sqc2 vf3, 0(a0)
        //beq r0, r0, L13                                 // beq r0, r0, L13
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 20:
        next_block = 21;
        c->lw(t8, 0, t5);                                 // lw t8, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(fp, 0, t6);                                 // lh fp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->lqc2(vf9, 32, a0);                             // lqc2 vf9, 32(a0)
        c->pextlw(t8, fp, t8);                            // pextlw t8, fp, t8
        c->pextlh(t8, t8, r0);                            // pextlh t8, t8, r0
        c->psraw(t8, t8, 16);                             // psraw t8, t8, 16
        c->mov128_vf_gpr(vf7, t8);                        // qmtc2.i vf7, t8
        c->lw(t8, 0, s0);                                 // lw t8, 0(s0)
        c->daddiu(s0, s0, 4);                             // daddiu s0, s0, 4
        c->lh(fp, 0, s1);                                 // lh fp, 0(s1)
        c->daddiu(s1, s1, 2);                             // daddiu s1, s1, 2
        c->pextlw(t8, fp, t8);                            // pextlw t8, fp, t8
        c->pextlh(t8, t8, r0);                            // pextlh t8, t8, r0
        c->psraw(t8, t8, 16);                             // psraw t8, t8, 16
        c->mov128_vf_gpr(vf8, t8);                        // qmtc2.i vf8, t8
        c->vitof12(DEST::xyzw, vf7, vf7);                 // vitof12.xyzw vf7, vf7
        c->vitof12(DEST::xyzw, vf8, vf8);                 // vitof12.xyzw vf8, vf8
        c->vmula_bc(DEST::xyzw, BC::w, vf9, vf0);         // vmulaw.xyzw acc, vf9, vf0
        c->vmadda_bc(DEST::xyzw, BC::x, vf7, vf13);       // vmaddax.xyzw acc, vf7, vf13
        c->vmadd_bc(DEST::xyzw, BC::w, vf9, vf8, vf13);   // vmaddw.xyzw vf9, vf8, vf13
        c->sqc2(vf9, 32, a0);                             // sqc2 vf9, 32(a0)
        //beq r0, r0, L13                                 // beq r0, r0, L13
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 21:
        next_block = 22;
        c->lw(s6, 0, t5);                                 // lw s6, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(gp, 0, t6);                                 // lh gp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->lqc2(vf3, 0, a0);                              // lqc2 vf3, 0(a0)
        c->pextlw(s6, gp, s6);                            // pextlw s6, gp, s6
        c->pextlh(s6, s6, r0);                            // pextlh s6, s6, r0
        c->psraw(s6, s6, 16);                             // psraw s6, s6, 16
        c->mov128_vf_gpr(vf1, s6);                        // qmtc2.i vf1, s6
        c->lw(s6, 0, s0);                                 // lw s6, 0(s0)
        c->daddiu(s0, s0, 4);                             // daddiu s0, s0, 4
        c->lh(gp, 0, s1);                                 // lh gp, 0(s1)
        c->daddiu(s1, s1, 2);                             // daddiu s1, s1, 2
        c->pextlw(s6, gp, s6);                            // pextlw s6, gp, s6
        c->pextlh(s6, s6, r0);                            // pextlh s6, s6, r0
        c->psraw(s6, s6, 16);                             // psraw s6, s6, 16
        c->mov128_vf_gpr(vf2, s6);                        // qmtc2.i vf2, s6
        c->lw(t8, 0, t5);                                 // lw t8, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(fp, 0, t6);                                 // lh fp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
        c->vitof0(DEST::xyzw, vf2, vf2);                  // vitof0.xyzw vf2, vf2
        c->lqc2(vf9, 32, a0);                             // lqc2 vf9, 32(a0)
        c->pextlw(t8, fp, t8);                            // pextlw t8, fp, t8
        c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
        c->vmadda_bc(DEST::xyzw, BC::y, vf1, vf13);       // vmadday.xyzw acc, vf1, vf13
        c->vmadd_bc(DEST::xyzw, BC::z, vf3, vf2, vf13);   // vmaddz.xyzw vf3, vf2, vf13
        c->pextlh(t8, t8, r0);                            // pextlh t8, t8, r0
        c->psraw(t8, t8, 16);                             // psraw t8, t8, 16
        c->mov128_vf_gpr(vf7, t8);                        // qmtc2.i vf7, t8
        c->lw(t8, 0, s0);                                 // lw t8, 0(s0)
        c->daddiu(s0, s0, 4);                             // daddiu s0, s0, 4
        c->lh(fp, 0, s1);                                 // lh fp, 0(s1)
        c->daddiu(s1, s1, 2);                             // daddiu s1, s1, 2
        c->pextlw(t8, fp, t8);                            // pextlw t8, fp, t8
        c->pextlh(t8, t8, r0);                            // pextlh t8, t8, r0
        c->psraw(t8, t8, 16);                             // psraw t8, t8, 16
        c->mov128_vf_gpr(vf8, t8);                        // qmtc2.i vf8, t8
        c->vitof12(DEST::xyzw, vf7, vf7);                 // vitof12.xyzw vf7, vf7
        c->vitof12(DEST::xyzw, vf8, vf8);                 // vitof12.xyzw vf8, vf8
        c->vmula_bc(DEST::xyzw, BC::w, vf9, vf0);         // vmulaw.xyzw acc, vf9, vf0
        c->vmadda_bc(DEST::xyzw, BC::x, vf7, vf13);       // vmaddax.xyzw acc, vf7, vf13
        c->vmadd_bc(DEST::xyzw, BC::w, vf9, vf8, vf13);   // vmaddw.xyzw vf9, vf8, vf13
        c->sqc2(vf3, 0, a0);                              // sqc2 vf3, 0(a0)
        c->sqc2(vf9, 32, a0);                             // sqc2 vf9, 32(a0)
        //beq r0, r0, L13                                 // beq r0, r0, L13
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 22:
        next_block = 23;
        c->ld(s6, 0, t4);                                 // ld s6, 0(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lw(gp, 0, t5);                                 // lw gp, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lqc2(vf3, 0, a0);                              // lqc2 vf3, 0(a0)
        c->pcpyld(s6, gp, s6);                            // pcpyld s6, gp, s6
        c->mov128_vf_gpr(vf1, s6);                        // qmtc2.i vf1, s6
        c->ld(s6, 0, t7);                                 // ld s6, 0(t7)
        c->daddiu(t7, t7, 8);                             // daddiu t7, t7, 8
        c->lw(gp, 0, s0);                                 // lw gp, 0(s0)
        c->daddiu(s0, s0, 4);                             // daddiu s0, s0, 4
        c->pcpyld(s6, gp, s6);                            // pcpyld s6, gp, s6
        c->mov128_vf_gpr(vf2, s6);                        // qmtc2.i vf2, s6
        c->lw(t8, 0, t5);                                 // lw t8, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(fp, 0, t6);                                 // lh fp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->lqc2(vf9, 32, a0);                             // lqc2 vf9, 32(a0)
        c->pextlw(t8, fp, t8);                            // pextlw t8, fp, t8
        c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
        c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf13);       // vmaddax.xyzw acc, vf1, vf13
        c->vmadd_bc(DEST::xyzw, BC::w, vf3, vf2, vf13);   // vmaddw.xyzw vf3, vf2, vf13
        c->pextlh(t8, t8, r0);                            // pextlh t8, t8, r0
        c->psraw(t8, t8, 16);                             // psraw t8, t8, 16
        c->mov128_vf_gpr(vf7, t8);                        // qmtc2.i vf7, t8
        c->lw(t8, 0, s0);                                 // lw t8, 0(s0)
        c->daddiu(s0, s0, 4);                             // daddiu s0, s0, 4
        c->lh(fp, 0, s1);                                 // lh fp, 0(s1)
        c->daddiu(s1, s1, 2);                             // daddiu s1, s1, 2
        c->pextlw(t8, fp, t8);                            // pextlw t8, fp, t8
        c->pextlh(t8, t8, r0);                            // pextlh t8, t8, r0
        c->psraw(t8, t8, 16);                             // psraw t8, t8, 16
        c->mov128_vf_gpr(vf8, t8);                        // qmtc2.i vf8, t8
        c->vitof12(DEST::xyzw, vf7, vf7);                 // vitof12.xyzw vf7, vf7
        c->vitof12(DEST::xyzw, vf8, vf8);                 // vitof12.xyzw vf8, vf8
        c->vmula_bc(DEST::xyzw, BC::w, vf9, vf0);         // vmulaw.xyzw acc, vf9, vf0
        c->vmadda_bc(DEST::xyzw, BC::x, vf7, vf13);       // vmaddax.xyzw acc, vf7, vf13
        c->vmadd_bc(DEST::xyzw, BC::w, vf9, vf8, vf13);   // vmaddw.xyzw vf9, vf8, vf13
        c->sqc2(vf3, 0, a0);                              // sqc2 vf3, 0(a0)
        c->sqc2(vf9, 32, a0);                             // sqc2 vf9, 32(a0)
        //beq r0, r0, L13                                 // beq r0, r0, L13
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 23:
        next_block = 24;
        c->ld(t9, 0, t4);                                 // ld t9, 0(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lqc2(vf6, 16, a0);                             // lqc2 vf6, 16(a0)
        c->pextlh(t9, t9, r0);                            // pextlh t9, t9, r0
        c->psraw(t9, t9, 16);                             // psraw t9, t9, 16
        c->mov128_vf_gpr(vf4, t9);                        // qmtc2.i vf4, t9
        c->ld(t9, 0, t7);                                 // ld t9, 0(t7)
        c->daddiu(t7, t7, 8);                             // daddiu t7, t7, 8
        c->pextlh(t9, t9, r0);                            // pextlh t9, t9, r0
        c->psraw(t9, t9, 16);                             // psraw t9, t9, 16
        c->mov128_vf_gpr(vf5, t9);                        // qmtc2.i vf5, t9
        c->vitof15(DEST::xyzw, vf4, vf4);                 // vitof15.xyzw vf4, vf4
        c->lw(t8, 0, t5);                                 // lw t8, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(fp, 0, t6);                                 // lh fp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->vitof15(DEST::xyzw, vf5, vf5);                 // vitof15.xyzw vf5, vf5
        c->lqc2(vf9, 32, a0);                             // lqc2 vf9, 32(a0)
        c->pextlw(t8, fp, t8);                            // pextlw t8, fp, t8
        c->vmula_bc(DEST::xyzw, BC::x, vf4, vf13);        // vmulax.xyzw acc, vf4, vf13
        c->vmadd_bc(DEST::xyzw, BC::w, vf4, vf5, vf13);   // vmaddw.xyzw vf4, vf5, vf13
        c->pextlh(t8, t8, r0);                            // pextlh t8, t8, r0
        c->psraw(t8, t8, 16);                             // psraw t8, t8, 16
        c->mov128_vf_gpr(vf7, t8);                        // qmtc2.i vf7, t8
        c->vmul(DEST::xyzw, vf10, vf4, vf6);              // vmul.xyzw vf10, vf4, vf6
        c->lw(t8, 0, s0);                                 // lw t8, 0(s0)
        c->daddiu(s0, s0, 4);                             // daddiu s0, s0, 4
        c->lh(fp, 0, s1);                                 // lh fp, 0(s1)
        c->daddiu(s1, s1, 2);                             // daddiu s1, s1, 2
        c->pextlw(t8, fp, t8);                            // pextlw t8, fp, t8
        c->vmula_bc(DEST::xyzw, BC::w, vf10, vf0);        // vmulaw.xyzw acc, vf10, vf0
        c->vmadda_bc(DEST::xyzw, BC::z, vf0, vf10);       // vmaddaz.xyzw acc, vf0, vf10
        c->vmadda_bc(DEST::xyzw, BC::y, vf0, vf10);       // vmadday.xyzw acc, vf0, vf10
        c->vmadd_bc(DEST::xyzw, BC::x, vf10, vf0, vf10);  // vmaddx.xyzw vf10, vf0, vf10
        c->pextlh(t8, t8, r0);                            // pextlh t8, t8, r0
        c->psraw(t8, t8, 16);                             // psraw t8, t8, 16
        c->mov128_vf_gpr(vf8, t8);                        // qmtc2.i vf8, t8
        c->mov128_gpr_vf(t9, vf10);                       // qmfc2.i t9, vf10
        c->vitof12(DEST::xyzw, vf7, vf7);                 // vitof12.xyzw vf7, vf7
        c->vitof12(DEST::xyzw, vf8, vf8);                 // vitof12.xyzw vf8, vf8
        c->pcpyud(t9, t9, r0);                            // pcpyud t9, t9, r0
        if (((s64)c->sgpr64(t9)) < 0) {                   // bltzl t9, L17
          c->vsub(DEST::xyzw, vf4, vf15, vf4);            // vsub.xyzw vf4, vf15, vf4
          next_block = 25;
          break;
        }

      case 25:
        next_block = 26;
        c->vmula_bc(DEST::xyzw, BC::w, vf9, vf0);         // vmulaw.xyzw acc, vf9, vf0
        c->vmadda_bc(DEST::xyzw, BC::x, vf7, vf13);       // vmaddax.xyzw acc, vf7, vf13
        c->vmadd_bc(DEST::xyzw, BC::w, vf9, vf8, vf13);   // vmaddw.xyzw vf9, vf8, vf13
        c->vadd(DEST::xyzw, vf6, vf6, vf4);               // vadd.xyzw vf6, vf6, vf4
        c->sqc2(vf6, 16, a0);                             // sqc2 vf6, 16(a0)
        c->sqc2(vf9, 32, a0);                             // sqc2 vf9, 32(a0)
        //beq r0, r0, L13                                 // beq r0, r0, L13
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 26:
        next_block = 27;
        c->ld(t9, 0, t4);                                 // ld t9, 0(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lqc2(vf6, 16, a0);                             // lqc2 vf6, 16(a0)
        c->pextlh(t9, t9, r0);                            // pextlh t9, t9, r0
        c->psraw(t9, t9, 16);                             // psraw t9, t9, 16
        c->mov128_vf_gpr(vf4, t9);                        // qmtc2.i vf4, t9
        c->ld(t9, 0, t7);                                 // ld t9, 0(t7)
        c->daddiu(t7, t7, 8);                             // daddiu t7, t7, 8
        c->pextlh(t9, t9, r0);                            // pextlh t9, t9, r0
        c->psraw(t9, t9, 16);                             // psraw t9, t9, 16
        c->mov128_vf_gpr(vf5, t9);                        // qmtc2.i vf5, t9
        c->lw(s6, 0, t5);                                 // lw s6, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(gp, 0, t6);                                 // lh gp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->vitof15(DEST::xyzw, vf4, vf4);                 // vitof15.xyzw vf4, vf4
        c->vitof15(DEST::xyzw, vf5, vf5);                 // vitof15.xyzw vf5, vf5
        c->lqc2(vf3, 0, a0);                              // lqc2 vf3, 0(a0)
        c->pextlw(s6, gp, s6);                            // pextlw s6, gp, s6
        c->vmula_bc(DEST::xyzw, BC::x, vf4, vf13);        // vmulax.xyzw acc, vf4, vf13
        c->vmadd_bc(DEST::xyzw, BC::w, vf4, vf5, vf13);   // vmaddw.xyzw vf4, vf5, vf13
        c->pextlh(s6, s6, r0);                            // pextlh s6, s6, r0
        c->psraw(s6, s6, 16);                             // psraw s6, s6, 16
        c->mov128_vf_gpr(vf1, s6);                        // qmtc2.i vf1, s6
        c->vmul(DEST::xyzw, vf10, vf4, vf6);              // vmul.xyzw vf10, vf4, vf6
        c->lw(s6, 0, s0);                                 // lw s6, 0(s0)
        c->daddiu(s0, s0, 4);                             // daddiu s0, s0, 4
        c->lh(gp, 0, s1);                                 // lh gp, 0(s1)
        c->daddiu(s1, s1, 2);                             // daddiu s1, s1, 2
        c->pextlw(s6, gp, s6);                            // pextlw s6, gp, s6
        c->vmula_bc(DEST::xyzw, BC::w, vf10, vf0);        // vmulaw.xyzw acc, vf10, vf0
        c->vmadda_bc(DEST::xyzw, BC::z, vf0, vf10);       // vmaddaz.xyzw acc, vf0, vf10
        c->vmadda_bc(DEST::xyzw, BC::y, vf0, vf10);       // vmadday.xyzw acc, vf0, vf10
        c->vmadd_bc(DEST::xyzw, BC::x, vf10, vf0, vf10);  // vmaddx.xyzw vf10, vf0, vf10
        c->pextlh(s6, s6, r0);                            // pextlh s6, s6, r0
        c->psraw(s6, s6, 16);                             // psraw s6, s6, 16
        c->mov128_vf_gpr(vf2, s6);                        // qmtc2.i vf2, s6
        c->mov128_gpr_vf(t9, vf10);                       // qmfc2.i t9, vf10
        c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
        c->lw(t8, 0, t5);                                 // lw t8, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(fp, 0, t6);                                 // lh fp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->pcpyud(t9, t9, r0);                            // pcpyud t9, t9, r0
        if (((s64)c->sgpr64(t9)) < 0) {                   // bltzl t9, L18
          c->vsub(DEST::xyzw, vf4, vf15, vf4);            // vsub.xyzw vf4, vf15, vf4
          next_block = 28;
          break;
        }

      case 28:
        next_block = 29;
        c->vitof0(DEST::xyzw, vf2, vf2);                  // vitof0.xyzw vf2, vf2
        c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
        c->vmadda_bc(DEST::xyzw, BC::y, vf1, vf13);       // vmadday.xyzw acc, vf1, vf13
        c->vmadd_bc(DEST::xyzw, BC::z, vf3, vf2, vf13);   // vmaddz.xyzw vf3, vf2, vf13
        c->vadd(DEST::xyzw, vf6, vf6, vf4);               // vadd.xyzw vf6, vf6, vf4
        c->lqc2(vf9, 32, a0);                             // lqc2 vf9, 32(a0)
        c->pextlw(t8, fp, t8);                            // pextlw t8, fp, t8
        c->pextlh(t8, t8, r0);                            // pextlh t8, t8, r0
        c->psraw(t8, t8, 16);                             // psraw t8, t8, 16
        c->mov128_vf_gpr(vf7, t8);                        // qmtc2.i vf7, t8
        c->lw(t8, 0, s0);                                 // lw t8, 0(s0)
        c->daddiu(s0, s0, 4);                             // daddiu s0, s0, 4
        c->lh(fp, 0, s1);                                 // lh fp, 0(s1)
        c->daddiu(s1, s1, 2);                             // daddiu s1, s1, 2
        c->pextlw(t8, fp, t8);                            // pextlw t8, fp, t8
        c->pextlh(t8, t8, r0);                            // pextlh t8, t8, r0
        c->psraw(t8, t8, 16);                             // psraw t8, t8, 16
        c->mov128_vf_gpr(vf8, t8);                        // qmtc2.i vf8, t8
        c->vitof12(DEST::xyzw, vf7, vf7);                 // vitof12.xyzw vf7, vf7
        c->vitof12(DEST::xyzw, vf8, vf8);                 // vitof12.xyzw vf8, vf8
        c->vmula_bc(DEST::xyzw, BC::w, vf9, vf0);         // vmulaw.xyzw acc, vf9, vf0
        c->vmadda_bc(DEST::xyzw, BC::x, vf7, vf13);       // vmaddax.xyzw acc, vf7, vf13
        c->vmadd_bc(DEST::xyzw, BC::w, vf9, vf8, vf13);   // vmaddw.xyzw vf9, vf8, vf13
        c->sqc2(vf3, 0, a0);                              // sqc2 vf3, 0(a0)
        c->sqc2(vf6, 16, a0);                             // sqc2 vf6, 16(a0)
        c->sqc2(vf9, 32, a0);                             // sqc2 vf9, 32(a0)
        //beq r0, r0, L13                                 // beq r0, r0, L13
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 29:
        next_block = 30;
        c->ld(t9, 8, t4);                                 // ld t9, 8(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lqc2(vf6, 16, a0);                             // lqc2 vf6, 16(a0)
        c->pextlh(t9, t9, r0);                            // pextlh t9, t9, r0
        c->psraw(t9, t9, 16);                             // psraw t9, t9, 16
        c->mov128_vf_gpr(vf4, t9);                        // qmtc2.i vf4, t9
        c->ld(t9, 8, t7);                                 // ld t9, 8(t7)
        c->daddiu(t7, t7, 8);                             // daddiu t7, t7, 8
        c->pextlh(t9, t9, r0);                            // pextlh t9, t9, r0
        c->psraw(t9, t9, 16);                             // psraw t9, t9, 16
        c->mov128_vf_gpr(vf5, t9);                        // qmtc2.i vf5, t9
        c->ld(s6, -8, t4);                                // ld s6, -8(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lw(gp, 0, t5);                                 // lw gp, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->vitof15(DEST::xyzw, vf4, vf4);                 // vitof15.xyzw vf4, vf4
        c->vitof15(DEST::xyzw, vf5, vf5);                 // vitof15.xyzw vf5, vf5
        c->lqc2(vf3, 0, a0);                              // lqc2 vf3, 0(a0)
        c->pcpyld(s6, gp, s6);                            // pcpyld s6, gp, s6
        c->vmula_bc(DEST::xyzw, BC::x, vf4, vf13);        // vmulax.xyzw acc, vf4, vf13
        c->vmadd_bc(DEST::xyzw, BC::w, vf4, vf5, vf13);   // vmaddw.xyzw vf4, vf5, vf13
        c->mov128_vf_gpr(vf1, s6);                        // qmtc2.i vf1, s6
        c->vmul(DEST::xyzw, vf10, vf4, vf6);              // vmul.xyzw vf10, vf4, vf6
        c->ld(s6, -8, t7);                                // ld s6, -8(t7)
        c->daddiu(t7, t7, 8);                             // daddiu t7, t7, 8
        c->lw(gp, 0, s0);                                 // lw gp, 0(s0)
        c->daddiu(s0, s0, 4);                             // daddiu s0, s0, 4
        c->pcpyld(s6, gp, s6);                            // pcpyld s6, gp, s6
        c->vmula_bc(DEST::xyzw, BC::w, vf10, vf0);        // vmulaw.xyzw acc, vf10, vf0
        c->vmadda_bc(DEST::xyzw, BC::z, vf0, vf10);       // vmaddaz.xyzw acc, vf0, vf10
        c->vmadda_bc(DEST::xyzw, BC::y, vf0, vf10);       // vmadday.xyzw acc, vf0, vf10
        c->vmadd_bc(DEST::xyzw, BC::x, vf10, vf0, vf10);  // vmaddx.xyzw vf10, vf0, vf10
        c->mov128_vf_gpr(vf2, s6);                        // qmtc2.i vf2, s6
        c->mov128_gpr_vf(t9, vf10);                       // qmfc2.i t9, vf10
        c->lw(t8, 0, t5);                                 // lw t8, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(fp, 0, t6);                                 // lh fp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->pcpyud(t9, t9, r0);                            // pcpyud t9, t9, r0
        if (((s64)c->sgpr64(t9)) < 0) {                   // bltzl t9, L19
          c->vsub(DEST::xyzw, vf4, vf15, vf4);            // vsub.xyzw vf4, vf15, vf4
          next_block = 31;
          break;
        }

      case 31:
        next_block = 32;
        c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
        c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf13);       // vmaddax.xyzw acc, vf1, vf13
        c->vmadd_bc(DEST::xyzw, BC::w, vf3, vf2, vf13);   // vmaddw.xyzw vf3, vf2, vf13
        c->vadd(DEST::xyzw, vf6, vf6, vf4);               // vadd.xyzw vf6, vf6, vf4
        c->lqc2(vf9, 32, a0);                             // lqc2 vf9, 32(a0)
        c->pextlw(t8, fp, t8);                            // pextlw t8, fp, t8
        c->pextlh(t8, t8, r0);                            // pextlh t8, t8, r0
        c->psraw(t8, t8, 16);                             // psraw t8, t8, 16
        c->mov128_vf_gpr(vf7, t8);                        // qmtc2.i vf7, t8
        c->lw(t8, 0, s0);                                 // lw t8, 0(s0)
        c->daddiu(s0, s0, 4);                             // daddiu s0, s0, 4
        c->lh(fp, 0, s1);                                 // lh fp, 0(s1)
        c->daddiu(s1, s1, 2);                             // daddiu s1, s1, 2
        c->pextlw(t8, fp, t8);                            // pextlw t8, fp, t8
        c->pextlh(t8, t8, r0);                            // pextlh t8, t8, r0
        c->psraw(t8, t8, 16);                             // psraw t8, t8, 16
        c->mov128_vf_gpr(vf8, t8);                        // qmtc2.i vf8, t8
        c->vitof12(DEST::xyzw, vf7, vf7);                 // vitof12.xyzw vf7, vf7
        c->vitof12(DEST::xyzw, vf8, vf8);                 // vitof12.xyzw vf8, vf8
        c->vmula_bc(DEST::xyzw, BC::w, vf9, vf0);         // vmulaw.xyzw acc, vf9, vf0
        c->vmadda_bc(DEST::xyzw, BC::x, vf7, vf13);       // vmaddax.xyzw acc, vf7, vf13
        c->vmadd_bc(DEST::xyzw, BC::w, vf9, vf8, vf13);   // vmaddw.xyzw vf9, vf8, vf13
        c->sqc2(vf3, 0, a0);                              // sqc2 vf3, 0(a0)
        c->sqc2(vf6, 16, a0);                             // sqc2 vf6, 16(a0)
        c->sqc2(vf9, 32, a0);                             // sqc2 vf9, 32(a0)
        //beq r0, r0, L13                                 // beq r0, r0, L13
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 32:
        next_block = 33;
        c->lq(a0, 0, sp);                                 // lq a0, 0(sp)
        c->lq(t7, 16, sp);                                // lq t7, 16(sp)
        c->lq(s0, 32, sp);                                // lq s0, 32(sp)
        c->lq(s1, 48, sp);                                // lq s1, 48(sp)
        //jr ra                                           // jr ra
        c->daddiu(sp, sp, 64);                            // daddiu sp, sp, 64
        goto end_of_function;                             // return

        //jr ra                                           // jr ra
        c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
        goto end_of_function;                             // return

        // nop                                            // sll r0, r0, 0
        // nop                                            // sll r0, r0, 0
    }
  }
  end_of_function:
  return c->gprs[v0].du64[0];
}

u32 jump_table_vals[16] = {
    0x4d7666df, // = 6 ^ 1299605209
    0x4d7666d0, // = 9 ^ 1299605209
    0x4d7666d2, // = 11 ^ 1299605209
    0x4d7666d7, // = 14 ^ 1299605209
    0x4d7666cd, // = 20 ^ 1299605209
    0x4d7666cc, // = 21 ^ 1299605209
    0x4d7666ce, // = 23 ^ 1299605209
    0x4d7666c3, // = 26 ^ 1299605209
    0x4d7666df, // = 6 ^ 1299605209
    0x4d7666d3, // = 10 ^ 1299605209
    0x4d7666d2, // = 11 ^ 1299605209
    0x4d7666c8, // = 17 ^ 1299605209
    0x4d7666cd, // = 20 ^ 1299605209
    0x4d7666cf, // = 22 ^ 1299605209
    0x4d7666ce, // = 23 ^ 1299605209
    0x4d7666c4, // = 29 ^ 1299605209
};

} // namespace decompress_frame_data_pair_to_accumulator
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak2 {
namespace decompress_frame_data_to_accumulator {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 next_block = 0;
  while(true) {
    switch(next_block) {

      case 0:
        next_block = 1;
        c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
        c->mov128_vf_gpr(vf13, a2);                       // qmtc2.i vf13, a2
        c->sq(a0, 0, sp);                                 // sq a0, 0(sp)
        //c->lui(t2, 28672);                                // lui t2, 28672
        get_fake_spad_addr2(t2, cache.fake_scratchpad_data, 0, c);
        c->lw(t4, 0, a1);                                 // lw t4, 0(a1)
        c->daddiu(v1, a1, 16);                            // daddiu v1, a1, 16
        c->lw(t5, 4, a1);                                 // lw t5, 4(a1)
        c->daddu(s5, t1, r0);                             // daddu s5, t1, r0
        c->lw(t6, 8, a1);                                 // lw t6, 8(a1)
        c->daddu(t4, t4, v1);                             // daddu t4, t4, v1
        c->vmul_bc(DEST::xyzw, BC::x, vf13, vf14, vf13);  // vmulx.xyzw vf13, vf14, vf13
        c->daddiu(t2, t2, 1680);                          // daddiu t2, t2, 1680 JAK 2 CHANGE
        c->lw(s2, 56, t1);                                // lw s2, 56(t1)
        c->daddu(t5, t5, v1);                             // daddu t5, t5, v1
        c->lw(s4, 60, t1);                                // lw s4, 60(t1)
        c->daddu(t6, t6, v1);                             // daddu t6, t6, v1
        c->addiu(s3, r0, 8);                              // addiu s3, r0, 8
        c->daddiu(s5, s5, 4);                             // daddiu s5, s5, 4
        c->andi(t3, s4, 1);                               // andi t3, s4, 1
        // nop                                            // sll r0, r0, 0
        bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L22
        // nop                                            // sll r0, r0, 0
        if (bc) {next_block = 2;}                         // branch non-likely

        break;

      case 1:
        next_block = 2;
        c->lqc2(vf1, 0, t4);                              // lqc2 vf1, 0(t4)
        c->lqc2(vf2, 16, t4);                             // lqc2 vf2, 16(t4)
        c->lqc2(vf3, 32, t4);                             // lqc2 vf3, 32(t4)
        c->lqc2(vf4, 48, t4);                             // lqc2 vf4, 48(t4)
        c->lqc2(vf9, 0, a0);                              // lqc2 vf9, 0(a0)
        c->daddiu(t4, t4, 64);                            // daddiu t4, t4, 64
        c->lqc2(vf10, 16, a0);                            // lqc2 vf10, 16(a0)
        // nop                                            // sll r0, r0, 0
        c->lqc2(vf11, 32, a0);                            // lqc2 vf11, 32(a0)
        c->lqc2(vf12, 48, a0);                            // lqc2 vf12, 48(a0)
        c->vmula_bc(DEST::xyzw, BC::w, vf9, vf0);         // vmulaw.xyzw acc, vf9, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf1, vf13);   // vmaddx.xyzw vf9, vf1, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf10, vf0);        // vmulaw.xyzw acc, vf10, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf10, vf2, vf13);  // vmaddx.xyzw vf10, vf2, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf11, vf0);        // vmulaw.xyzw acc, vf11, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf11, vf3, vf13);  // vmaddx.xyzw vf11, vf3, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf12, vf0);        // vmulaw.xyzw acc, vf12, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf12, vf4, vf13);  // vmaddx.xyzw vf12, vf4, vf13
        c->sqc2(vf9, 0, a0);                              // sqc2 vf9, 0(a0)
        c->sqc2(vf10, 16, a0);                            // sqc2 vf10, 16(a0)
        c->sqc2(vf11, 32, a0);                            // sqc2 vf11, 32(a0)
        c->sqc2(vf12, 48, a0);                            // sqc2 vf12, 48(a0)

      case 2:
        next_block = 3;
        c->andi(t3, s4, 2);                               // andi t3, s4, 2
        c->daddiu(a0, a0, 64);                            // daddiu a0, a0, 64
        bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L23
        // nop                                            // sll r0, r0, 0
        if (bc) {next_block = 4;}                         // branch non-likely

        break;

      case 3:
        next_block = 4;
        c->lqc2(vf1, 0, t4);                              // lqc2 vf1, 0(t4)
        c->lqc2(vf2, 16, t4);                             // lqc2 vf2, 16(t4)
        c->lqc2(vf3, 32, t4);                             // lqc2 vf3, 32(t4)
        c->lqc2(vf4, 48, t4);                             // lqc2 vf4, 48(t4)
        c->lqc2(vf9, 0, a0);                              // lqc2 vf9, 0(a0)
        c->daddiu(t4, t4, 64);                            // daddiu t4, t4, 64
        c->lqc2(vf10, 16, a0);                            // lqc2 vf10, 16(a0)
        // nop                                            // sll r0, r0, 0
        c->lqc2(vf11, 32, a0);                            // lqc2 vf11, 32(a0)
        c->lqc2(vf12, 48, a0);                            // lqc2 vf12, 48(a0)
        c->vmula_bc(DEST::xyzw, BC::w, vf9, vf0);         // vmulaw.xyzw acc, vf9, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf1, vf13);   // vmaddx.xyzw vf9, vf1, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf10, vf0);        // vmulaw.xyzw acc, vf10, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf10, vf2, vf13);  // vmaddx.xyzw vf10, vf2, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf11, vf0);        // vmulaw.xyzw acc, vf11, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf11, vf3, vf13);  // vmaddx.xyzw vf11, vf3, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf12, vf0);        // vmulaw.xyzw acc, vf12, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf12, vf4, vf13);  // vmaddx.xyzw vf12, vf4, vf13
        c->sqc2(vf9, 0, a0);                              // sqc2 vf9, 0(a0)
        c->sqc2(vf10, 16, a0);                            // sqc2 vf10, 16(a0)
        c->sqc2(vf11, 32, a0);                            // sqc2 vf11, 32(a0)
        c->sqc2(vf12, 48, a0);                            // sqc2 vf12, 48(a0)

      case 4:
        next_block = 5;
        c->lw(s4, -4, s5);                                // lw s4, -4(s5)
        c->daddiu(a0, a0, 64);                            // daddiu a0, a0, 64

      case 5:
        next_block = 6;
        c->andi(t3, s4, 15);                              // andi t3, s4, 15
        c->sra(s4, s4, 4);                                // sra s4, s4, 4
        c->sll(t3, t3, 2);                                // sll t3, t3, 2
        c->daddiu(s3, s3, -1);                            // daddiu s3, s3, -1
        c->daddu(t3, t3, t2);                             // daddu t3, t3, t2
        c->daddiu(s2, s2, -1);                            // daddiu s2, s2, -1
        c->lw(t3, 0, t3);                                 // lw t3, 0(t3)
        // nop                                            // sll r0, r0, 0
        // nop                                            // sll r0, r0, 0
        // nop                                            // sll r0, r0, 0
        next_block = 0x7b2191d ^ c->gprs[t3].du32[0];     // jr t3
        ASSERT(next_block < 33);
        break;
        // nop                                            // sll r0, r0, 0

      case 6:
        next_block = 7;
        bc = c->sgpr64(s2) == 0;                          // beq s2, r0, L32
        c->daddiu(a0, a0, 48);                            // daddiu a0, a0, 48
        if (bc) {next_block = 32;}                        // branch non-likely

        break;

      case 7:
        next_block = 8;
        bc = c->sgpr64(s3) != 0;                          // bne s3, r0, L24
        // nop                                            // sll r0, r0, 0
        if (bc) {next_block = 5;}                         // branch non-likely

        break;

      case 8:
        next_block = 9;
        c->lw(s4, 0, s5);                                 // lw s4, 0(s5)
        c->daddiu(s5, s5, 4);                             // daddiu s5, s5, 4
        //beq r0, r0, L24                                 // beq r0, r0, L24
        c->addiu(s3, r0, 8);                              // addiu s3, r0, 8
        next_block = 5;                                   // branch always

        break;

      case 9:
        next_block = 10;
        c->lw(s6, 0, t5);                                 // lw s6, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(gp, 0, t6);                                 // lh gp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->lqc2(vf3, 0, a0);                              // lqc2 vf3, 0(a0)
        c->pextlw(s6, gp, s6);                            // pextlw s6, gp, s6
        c->pextlh(s6, s6, r0);                            // pextlh s6, s6, r0
        c->psraw(s6, s6, 16);                             // psraw s6, s6, 16
        c->mov128_vf_gpr(vf1, s6);                        // qmtc2.i vf1, s6
        c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
        c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
        c->vmadd_bc(DEST::xyzw, BC::y, vf3, vf1, vf13);   // vmaddy.xyzw vf3, vf1, vf13
        c->sqc2(vf3, 0, a0);                              // sqc2 vf3, 0(a0)
        //beq r0, r0, L25                                 // beq r0, r0, L25
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 10:
        next_block = 11;
        c->ld(s6, 0, t4);                                 // ld s6, 0(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lw(gp, 0, t5);                                 // lw gp, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lqc2(vf3, 0, a0);                              // lqc2 vf3, 0(a0)
        c->pcpyld(s6, gp, s6);                            // pcpyld s6, gp, s6
        c->mov128_vf_gpr(vf1, s6);                        // qmtc2.i vf1, s6
        c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf3, vf1, vf13);   // vmaddx.xyzw vf3, vf1, vf13
        c->sqc2(vf3, 0, a0);                              // sqc2 vf3, 0(a0)
        //beq r0, r0, L25                                 // beq r0, r0, L25
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 11:
        next_block = 12;
        c->ld(t9, 0, t4);                                 // ld t9, 0(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lqc2(vf6, 16, a0);                             // lqc2 vf6, 16(a0)
        c->pextlh(t9, t9, r0);                            // pextlh t9, t9, r0
        c->psraw(t9, t9, 16);                             // psraw t9, t9, 16
        c->mov128_vf_gpr(vf4, t9);                        // qmtc2.i vf4, t9
        c->vitof15(DEST::xyzw, vf4, vf4);                 // vitof15.xyzw vf4, vf4
        c->vmul(DEST::xyzw, vf10, vf4, vf6);              // vmul.xyzw vf10, vf4, vf6
        c->vmula_bc(DEST::xyzw, BC::w, vf10, vf0);        // vmulaw.xyzw acc, vf10, vf0
        c->vmadda_bc(DEST::xyzw, BC::z, vf0, vf10);       // vmaddaz.xyzw acc, vf0, vf10
        c->vmadda_bc(DEST::xyzw, BC::y, vf0, vf10);       // vmadday.xyzw acc, vf0, vf10
        c->vmadd_bc(DEST::xyzw, BC::x, vf10, vf0, vf10);  // vmaddx.xyzw vf10, vf0, vf10
        c->mov128_gpr_vf(t9, vf10);                       // qmfc2.i t9, vf10
        c->pcpyud(t9, t9, r0);                            // pcpyud t9, t9, r0
        if (((s64)c->sgpr64(t9)) < 0) {                   // bltzl t9, L26
          c->vsub(DEST::xyzw, vf4, vf15, vf4);            // vsub.xyzw vf4, vf15, vf4
          next_block = 13;
          break;
        }

      case 13:
        next_block = 14;
        c->vmula_bc(DEST::xyzw, BC::w, vf6, vf0);         // vmulaw.xyzw acc, vf6, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf6, vf4, vf13);   // vmaddx.xyzw vf6, vf4, vf13
        c->sqc2(vf6, 16, a0);                             // sqc2 vf6, 16(a0)
        //beq r0, r0, L25                                 // beq r0, r0, L25
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 14:
        next_block = 15;
        c->ld(t9, 0, t4);                                 // ld t9, 0(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lqc2(vf6, 16, a0);                             // lqc2 vf6, 16(a0)
        c->pextlh(t9, t9, r0);                            // pextlh t9, t9, r0
        c->psraw(t9, t9, 16);                             // psraw t9, t9, 16
        c->mov128_vf_gpr(vf4, t9);                        // qmtc2.i vf4, t9
        c->lw(s6, 0, t5);                                 // lw s6, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(gp, 0, t6);                                 // lh gp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->lqc2(vf3, 0, a0);                              // lqc2 vf3, 0(a0)
        c->vitof15(DEST::xyzw, vf4, vf4);                 // vitof15.xyzw vf4, vf4
        c->pextlw(s6, gp, s6);                            // pextlw s6, gp, s6
        c->pextlh(s6, s6, r0);                            // pextlh s6, s6, r0
        c->psraw(s6, s6, 16);                             // psraw s6, s6, 16
        c->vmul(DEST::xyzw, vf10, vf4, vf6);              // vmul.xyzw vf10, vf4, vf6
        c->mov128_vf_gpr(vf1, s6);                        // qmtc2.i vf1, s6
        c->vmula_bc(DEST::xyzw, BC::w, vf10, vf0);        // vmulaw.xyzw acc, vf10, vf0
        c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
        c->vmadda_bc(DEST::xyzw, BC::z, vf0, vf10);       // vmaddaz.xyzw acc, vf0, vf10
        c->vmadda_bc(DEST::xyzw, BC::y, vf0, vf10);       // vmadday.xyzw acc, vf0, vf10
        c->vmadd_bc(DEST::xyzw, BC::x, vf10, vf0, vf10);  // vmaddx.xyzw vf10, vf0, vf10
        c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
        c->vmadd_bc(DEST::xyzw, BC::y, vf3, vf1, vf13);   // vmaddy.xyzw vf3, vf1, vf13
        c->mov128_gpr_vf(t9, vf10);                       // qmfc2.i t9, vf10
        c->pcpyud(t9, t9, r0);                            // pcpyud t9, t9, r0
        if (((s64)c->sgpr64(t9)) < 0) {                   // bltzl t9, L27
          c->vsub(DEST::xyzw, vf4, vf15, vf4);            // vsub.xyzw vf4, vf15, vf4
          next_block = 16;
          break;
        }

      case 16:
        next_block = 17;
        c->vmula_bc(DEST::xyzw, BC::w, vf6, vf0);         // vmulaw.xyzw acc, vf6, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf6, vf4, vf13);   // vmaddx.xyzw vf6, vf4, vf13
        c->sqc2(vf3, 0, a0);                              // sqc2 vf3, 0(a0)
        c->sqc2(vf6, 16, a0);                             // sqc2 vf6, 16(a0)
        //beq r0, r0, L25                                 // beq r0, r0, L25
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 17:
        next_block = 18;
        c->ld(t9, 8, t4);                                 // ld t9, 8(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lqc2(vf6, 16, a0);                             // lqc2 vf6, 16(a0)
        c->pextlh(t9, t9, r0);                            // pextlh t9, t9, r0
        c->psraw(t9, t9, 16);                             // psraw t9, t9, 16
        c->mov128_vf_gpr(vf4, t9);                        // qmtc2.i vf4, t9
        c->ld(s6, -8, t4);                                // ld s6, -8(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lw(gp, 0, t5);                                 // lw gp, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lqc2(vf3, 0, a0);                              // lqc2 vf3, 0(a0)
        c->vitof15(DEST::xyzw, vf4, vf4);                 // vitof15.xyzw vf4, vf4
        c->pcpyld(s6, gp, s6);                            // pcpyld s6, gp, s6
        c->vmul(DEST::xyzw, vf10, vf4, vf6);              // vmul.xyzw vf10, vf4, vf6
        c->mov128_vf_gpr(vf1, s6);                        // qmtc2.i vf1, s6
        c->vmula_bc(DEST::xyzw, BC::w, vf10, vf0);        // vmulaw.xyzw acc, vf10, vf0
        c->vmadda_bc(DEST::xyzw, BC::z, vf0, vf10);       // vmaddaz.xyzw acc, vf0, vf10
        c->vmadda_bc(DEST::xyzw, BC::y, vf0, vf10);       // vmadday.xyzw acc, vf0, vf10
        c->vmadd_bc(DEST::xyzw, BC::x, vf10, vf0, vf10);  // vmaddx.xyzw vf10, vf0, vf10
        c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf3, vf1, vf13);   // vmaddx.xyzw vf3, vf1, vf13
        c->mov128_gpr_vf(t9, vf10);                       // qmfc2.i t9, vf10
        c->pcpyud(t9, t9, r0);                            // pcpyud t9, t9, r0
        if (((s64)c->sgpr64(t9)) < 0) {                   // bltzl t9, L28
          c->vsub(DEST::xyzw, vf4, vf15, vf4);            // vsub.xyzw vf4, vf15, vf4
          next_block = 19;
          break;
        }

      case 19:
        next_block = 20;
        c->vmula_bc(DEST::xyzw, BC::w, vf6, vf0);         // vmulaw.xyzw acc, vf6, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf6, vf4, vf13);   // vmaddx.xyzw vf6, vf4, vf13
        c->sqc2(vf3, 0, a0);                              // sqc2 vf3, 0(a0)
        c->sqc2(vf6, 16, a0);                             // sqc2 vf6, 16(a0)
        //beq r0, r0, L25                                 // beq r0, r0, L25
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 20:
        next_block = 21;
        c->lw(t8, 0, t5);                                 // lw t8, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(fp, 0, t6);                                 // lh fp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->lqc2(vf9, 32, a0);                             // lqc2 vf9, 32(a0)
        c->pextlw(t8, fp, t8);                            // pextlw t8, fp, t8
        c->pextlh(t8, t8, r0);                            // pextlh t8, t8, r0
        c->psraw(t8, t8, 16);                             // psraw t8, t8, 16
        c->mov128_vf_gpr(vf7, t8);                        // qmtc2.i vf7, t8
        c->vitof12(DEST::xyzw, vf7, vf7);                 // vitof12.xyzw vf7, vf7
        c->vmula_bc(DEST::xyzw, BC::w, vf9, vf0);         // vmulaw.xyzw acc, vf9, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf7, vf13);   // vmaddx.xyzw vf9, vf7, vf13
        c->sqc2(vf9, 32, a0);                             // sqc2 vf9, 32(a0)
        //beq r0, r0, L25                                 // beq r0, r0, L25
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 21:
        next_block = 22;
        c->lw(s6, 0, t5);                                 // lw s6, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(gp, 0, t6);                                 // lh gp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->lqc2(vf3, 0, a0);                              // lqc2 vf3, 0(a0)
        c->pextlw(s6, gp, s6);                            // pextlw s6, gp, s6
        c->pextlh(s6, s6, r0);                            // pextlh s6, s6, r0
        c->psraw(s6, s6, 16);                             // psraw s6, s6, 16
        c->mov128_vf_gpr(vf1, s6);                        // qmtc2.i vf1, s6
        c->lw(t8, 0, t5);                                 // lw t8, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(fp, 0, t6);                                 // lh fp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->lqc2(vf9, 32, a0);                             // lqc2 vf9, 32(a0)
        c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
        c->pextlw(t8, fp, t8);                            // pextlw t8, fp, t8
        c->pextlh(t8, t8, r0);                            // pextlh t8, t8, r0
        c->psraw(t8, t8, 16);                             // psraw t8, t8, 16
        c->mov128_vf_gpr(vf7, t8);                        // qmtc2.i vf7, t8
        c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
        c->vmadd_bc(DEST::xyzw, BC::y, vf3, vf1, vf13);   // vmaddy.xyzw vf3, vf1, vf13
        c->vitof12(DEST::xyzw, vf7, vf7);                 // vitof12.xyzw vf7, vf7
        c->vmula_bc(DEST::xyzw, BC::w, vf9, vf0);         // vmulaw.xyzw acc, vf9, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf7, vf13);   // vmaddx.xyzw vf9, vf7, vf13
        c->sqc2(vf3, 0, a0);                              // sqc2 vf3, 0(a0)
        c->sqc2(vf9, 32, a0);                             // sqc2 vf9, 32(a0)
        //beq r0, r0, L25                                 // beq r0, r0, L25
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 22:
        next_block = 23;
        c->ld(s6, 0, t4);                                 // ld s6, 0(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lw(gp, 0, t5);                                 // lw gp, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lqc2(vf3, 0, a0);                              // lqc2 vf3, 0(a0)
        c->pcpyld(s6, gp, s6);                            // pcpyld s6, gp, s6
        c->mov128_vf_gpr(vf1, s6);                        // qmtc2.i vf1, s6
        c->lw(t8, 0, t5);                                 // lw t8, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(fp, 0, t6);                                 // lh fp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->lqc2(vf9, 32, a0);                             // lqc2 vf9, 32(a0)
        c->pextlw(t8, fp, t8);                            // pextlw t8, fp, t8
        c->pextlh(t8, t8, r0);                            // pextlh t8, t8, r0
        c->psraw(t8, t8, 16);                             // psraw t8, t8, 16
        c->mov128_vf_gpr(vf7, t8);                        // qmtc2.i vf7, t8
        c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf3, vf1, vf13);   // vmaddx.xyzw vf3, vf1, vf13
        c->vitof12(DEST::xyzw, vf7, vf7);                 // vitof12.xyzw vf7, vf7
        c->vmula_bc(DEST::xyzw, BC::w, vf9, vf0);         // vmulaw.xyzw acc, vf9, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf7, vf13);   // vmaddx.xyzw vf9, vf7, vf13
        c->sqc2(vf3, 0, a0);                              // sqc2 vf3, 0(a0)
        c->sqc2(vf9, 32, a0);                             // sqc2 vf9, 32(a0)
        //beq r0, r0, L25                                 // beq r0, r0, L25
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 23:
        next_block = 24;
        c->ld(t9, 0, t4);                                 // ld t9, 0(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lqc2(vf6, 16, a0);                             // lqc2 vf6, 16(a0)
        c->pextlh(t9, t9, r0);                            // pextlh t9, t9, r0
        c->psraw(t9, t9, 16);                             // psraw t9, t9, 16
        c->mov128_vf_gpr(vf4, t9);                        // qmtc2.i vf4, t9
        c->lw(t8, 0, t5);                                 // lw t8, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(fp, 0, t6);                                 // lh fp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->lqc2(vf9, 32, a0);                             // lqc2 vf9, 32(a0)
        c->vitof15(DEST::xyzw, vf4, vf4);                 // vitof15.xyzw vf4, vf4
        c->pextlw(t8, fp, t8);                            // pextlw t8, fp, t8
        c->pextlh(t8, t8, r0);                            // pextlh t8, t8, r0
        c->psraw(t8, t8, 16);                             // psraw t8, t8, 16
        c->vmul(DEST::xyzw, vf10, vf4, vf6);              // vmul.xyzw vf10, vf4, vf6
        c->mov128_vf_gpr(vf7, t8);                        // qmtc2.i vf7, t8
        c->vmula_bc(DEST::xyzw, BC::w, vf10, vf0);        // vmulaw.xyzw acc, vf10, vf0
        c->vitof12(DEST::xyzw, vf7, vf7);                 // vitof12.xyzw vf7, vf7
        c->vmadda_bc(DEST::xyzw, BC::z, vf0, vf10);       // vmaddaz.xyzw acc, vf0, vf10
        c->vmadda_bc(DEST::xyzw, BC::y, vf0, vf10);       // vmadday.xyzw acc, vf0, vf10
        c->vmadd_bc(DEST::xyzw, BC::x, vf10, vf0, vf10);  // vmaddx.xyzw vf10, vf0, vf10
        c->vmula_bc(DEST::xyzw, BC::w, vf9, vf0);         // vmulaw.xyzw acc, vf9, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf7, vf13);   // vmaddx.xyzw vf9, vf7, vf13
        c->mov128_gpr_vf(t9, vf10);                       // qmfc2.i t9, vf10
        c->pcpyud(t9, t9, r0);                            // pcpyud t9, t9, r0
        if (((s64)c->sgpr64(t9)) < 0) {                   // bltzl t9, L29
          c->vsub(DEST::xyzw, vf4, vf15, vf4);            // vsub.xyzw vf4, vf15, vf4
          next_block = 25;
          break;
        }

      case 25:
        next_block = 26;
        c->vmula_bc(DEST::xyzw, BC::w, vf6, vf0);         // vmulaw.xyzw acc, vf6, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf6, vf4, vf13);   // vmaddx.xyzw vf6, vf4, vf13
        c->sqc2(vf6, 16, a0);                             // sqc2 vf6, 16(a0)
        c->sqc2(vf9, 32, a0);                             // sqc2 vf9, 32(a0)
        //beq r0, r0, L25                                 // beq r0, r0, L25
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 26:
        next_block = 27;
        c->ld(t9, 0, t4);                                 // ld t9, 0(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lqc2(vf6, 16, a0);                             // lqc2 vf6, 16(a0)
        c->pextlh(t9, t9, r0);                            // pextlh t9, t9, r0
        c->psraw(t9, t9, 16);                             // psraw t9, t9, 16
        c->mov128_vf_gpr(vf4, t9);                        // qmtc2.i vf4, t9
        c->lw(s6, 0, t5);                                 // lw s6, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(gp, 0, t6);                                 // lh gp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->lqc2(vf3, 0, a0);                              // lqc2 vf3, 0(a0)
        c->vitof15(DEST::xyzw, vf4, vf4);                 // vitof15.xyzw vf4, vf4
        c->pextlw(s6, gp, s6);                            // pextlw s6, gp, s6
        c->pextlh(s6, s6, r0);                            // pextlh s6, s6, r0
        c->psraw(s6, s6, 16);                             // psraw s6, s6, 16
        c->vmul(DEST::xyzw, vf10, vf4, vf6);              // vmul.xyzw vf10, vf4, vf6
        c->mov128_vf_gpr(vf1, s6);                        // qmtc2.i vf1, s6
        c->lw(t8, 0, t5);                                 // lw t8, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(fp, 0, t6);                                 // lh fp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->vmula_bc(DEST::xyzw, BC::w, vf10, vf0);        // vmulaw.xyzw acc, vf10, vf0
        c->vmadda_bc(DEST::xyzw, BC::z, vf0, vf10);       // vmaddaz.xyzw acc, vf0, vf10
        c->vmadda_bc(DEST::xyzw, BC::y, vf0, vf10);       // vmadday.xyzw acc, vf0, vf10
        c->vmadd_bc(DEST::xyzw, BC::x, vf10, vf0, vf10);  // vmaddx.xyzw vf10, vf0, vf10
        c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
        c->lqc2(vf9, 32, a0);                             // lqc2 vf9, 32(a0)
        c->pextlw(t8, fp, t8);                            // pextlw t8, fp, t8
        c->mov128_gpr_vf(t9, vf10);                       // qmfc2.i t9, vf10
        c->pextlh(t8, t8, r0);                            // pextlh t8, t8, r0
        c->psraw(t8, t8, 16);                             // psraw t8, t8, 16
        c->mov128_vf_gpr(vf7, t8);                        // qmtc2.i vf7, t8
        c->pcpyud(t9, t9, r0);                            // pcpyud t9, t9, r0
        if (((s64)c->sgpr64(t9)) < 0) {                   // bltzl t9, L30
          c->vsub(DEST::xyzw, vf4, vf15, vf4);            // vsub.xyzw vf4, vf15, vf4
          next_block = 28;
          break;
        }

      case 28:
        next_block = 29;
        c->vitof12(DEST::xyzw, vf7, vf7);                 // vitof12.xyzw vf7, vf7
        c->vmula_bc(DEST::xyzw, BC::w, vf6, vf0);         // vmulaw.xyzw acc, vf6, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf6, vf4, vf13);   // vmaddx.xyzw vf6, vf4, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
        c->vmadd_bc(DEST::xyzw, BC::y, vf3, vf1, vf13);   // vmaddy.xyzw vf3, vf1, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf9, vf0);         // vmulaw.xyzw acc, vf9, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf7, vf13);   // vmaddx.xyzw vf9, vf7, vf13
        c->sqc2(vf3, 0, a0);                              // sqc2 vf3, 0(a0)
        c->sqc2(vf6, 16, a0);                             // sqc2 vf6, 16(a0)
        c->sqc2(vf9, 32, a0);                             // sqc2 vf9, 32(a0)
        //beq r0, r0, L25                                 // beq r0, r0, L25
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 29:
        next_block = 30;
        c->ld(t9, 8, t4);                                 // ld t9, 8(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lqc2(vf6, 16, a0);                             // lqc2 vf6, 16(a0)
        c->pextlh(t9, t9, r0);                            // pextlh t9, t9, r0
        c->psraw(t9, t9, 16);                             // psraw t9, t9, 16
        c->mov128_vf_gpr(vf4, t9);                        // qmtc2.i vf4, t9
        c->ld(s6, -8, t4);                                // ld s6, -8(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lw(gp, 0, t5);                                 // lw gp, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lqc2(vf3, 0, a0);                              // lqc2 vf3, 0(a0)
        c->vitof15(DEST::xyzw, vf4, vf4);                 // vitof15.xyzw vf4, vf4
        c->pcpyld(s6, gp, s6);                            // pcpyld s6, gp, s6
        c->vmul(DEST::xyzw, vf10, vf4, vf6);              // vmul.xyzw vf10, vf4, vf6
        c->mov128_vf_gpr(vf1, s6);                        // qmtc2.i vf1, s6
        c->lw(t8, 0, t5);                                 // lw t8, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(fp, 0, t6);                                 // lh fp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->vmula_bc(DEST::xyzw, BC::w, vf10, vf0);        // vmulaw.xyzw acc, vf10, vf0
        c->vmadda_bc(DEST::xyzw, BC::z, vf0, vf10);       // vmaddaz.xyzw acc, vf0, vf10
        c->vmadda_bc(DEST::xyzw, BC::y, vf0, vf10);       // vmadday.xyzw acc, vf0, vf10
        c->vmadd_bc(DEST::xyzw, BC::x, vf10, vf0, vf10);  // vmaddx.xyzw vf10, vf0, vf10
        c->lqc2(vf9, 32, a0);                             // lqc2 vf9, 32(a0)
        c->pextlw(t8, fp, t8);                            // pextlw t8, fp, t8
        c->mov128_gpr_vf(t9, vf10);                       // qmfc2.i t9, vf10
        c->pextlh(t8, t8, r0);                            // pextlh t8, t8, r0
        c->psraw(t8, t8, 16);                             // psraw t8, t8, 16
        c->mov128_vf_gpr(vf7, t8);                        // qmtc2.i vf7, t8
        c->pcpyud(t9, t9, r0);                            // pcpyud t9, t9, r0
        if (((s64)c->sgpr64(t9)) < 0) {                   // bltzl t9, L31
          c->vsub(DEST::xyzw, vf4, vf15, vf4);            // vsub.xyzw vf4, vf15, vf4
          next_block = 31;
          break;
        }

      case 31:
        next_block = 32;
        c->vitof12(DEST::xyzw, vf7, vf7);                 // vitof12.xyzw vf7, vf7
        c->vmula_bc(DEST::xyzw, BC::w, vf6, vf0);         // vmulaw.xyzw acc, vf6, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf6, vf4, vf13);   // vmaddx.xyzw vf6, vf4, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf3, vf1, vf13);   // vmaddx.xyzw vf3, vf1, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf9, vf0);         // vmulaw.xyzw acc, vf9, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf7, vf13);   // vmaddx.xyzw vf9, vf7, vf13
        c->sqc2(vf3, 0, a0);                              // sqc2 vf3, 0(a0)
        c->sqc2(vf6, 16, a0);                             // sqc2 vf6, 16(a0)
        c->sqc2(vf9, 32, a0);                             // sqc2 vf9, 32(a0)
        //beq r0, r0, L25                                 // beq r0, r0, L25
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 32:
        next_block = 33;
        c->lq(a0, 0, sp);                                 // lq a0, 0(sp)
        // nop                                            // sll r0, r0, 0
        //jr ra                                           // jr ra
        c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
        goto end_of_function;                             // return

        //jr ra                                           // jr ra
        c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
        goto end_of_function;                             // return

        // nop                                            // sll r0, r0, 0
        // nop                                            // sll r0, r0, 0
        // nop                                            // sll r0, r0, 0
    }
  }
  end_of_function:
  return c->gprs[v0].du64[0];
}

u32 jump_table_vals[16] = {
    0x7b2191b, // = 6 ^ 129112349
    0x7b21914, // = 9 ^ 129112349
    0x7b21916, // = 11 ^ 129112349
    0x7b21913, // = 14 ^ 129112349
    0x7b21909, // = 20 ^ 129112349
    0x7b21908, // = 21 ^ 129112349
    0x7b2190a, // = 23 ^ 129112349
    0x7b21907, // = 26 ^ 129112349
    0x7b2191b, // = 6 ^ 129112349
    0x7b21917, // = 10 ^ 129112349
    0x7b21916, // = 11 ^ 129112349
    0x7b2190c, // = 17 ^ 129112349
    0x7b21909, // = 20 ^ 129112349
    0x7b2190b, // = 22 ^ 129112349
    0x7b2190a, // = 23 ^ 129112349
    0x7b21900, // = 29 ^ 129112349
};

} // namespace decompress_frame_data_to_accumulator
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak2 {
namespace decompress_fixed_data_to_accumulator {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 next_block = 0;
  while(true) {
    switch(next_block) {

      case 0:
        next_block = 1;
        c->lq(t4, 0, a1);                                 // lq t4, 0(a1)
        c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
        c->lq(t5, 16, a1);                                // lq t5, 16(a1)
        // nop                                            // sll r0, r0, 0
        c->sq(t4, 0, t1);                                 // sq t4, 0(t1)
        // nop                                            // sll r0, r0, 0
        c->sq(t5, 16, t1);                                // sq t5, 16(t1)
        // nop                                            // sll r0, r0, 0
        c->lq(t4, 32, a1);                                // lq t4, 32(a1)
        // nop                                            // sll r0, r0, 0
        c->lq(t5, 48, a1);                                // lq t5, 48(a1)
        // nop                                            // sll r0, r0, 0
        c->sq(t4, 32, t1);                                // sq t4, 32(t1)
        // nop                                            // sll r0, r0, 0
        c->sq(t5, 48, t1);                                // sq t5, 48(t1)
        // nop                                            // sll r0, r0, 0
        c->sq(a0, 0, sp);                                 // sq a0, 0(sp)
        // nop                                            // sll r0, r0, 0
        c->mov128_vf_gpr(vf13, a2);                       // qmtc2.i vf13, a2
        // c->lui(t2, 28672);                                // lui t2, 28672
        get_fake_spad_addr2(t2, cache.fake_scratchpad_data, 0, c);
        c->lw(t4, 64, a1);                                // lw t4, 64(a1)
        c->daddiu(v1, a1, 80);                            // daddiu v1, a1, 80
        c->lw(t5, 68, a1);                                // lw t5, 68(a1)
        c->daddu(s5, t1, r0);                             // daddu s5, t1, r0
        c->lw(t6, 72, a1);                                // lw t6, 72(a1)
        c->daddu(t4, t4, v1);                             // daddu t4, t4, v1
        c->vmul_bc(DEST::xyzw, BC::x, vf13, vf14, vf13);  // vmulx.xyzw vf13, vf14, vf13
        c->daddiu(t2, t2, 1616);                          // daddiu t2, t2, 1616 JAK 2 CHANGE
        c->lw(s2, 56, t1);                                // lw s2, 56(t1)
        c->daddu(t5, t5, v1);                             // daddu t5, t5, v1
        c->lw(s4, 60, t1);                                // lw s4, 60(t1)
        c->daddu(t6, t6, v1);                             // daddu t6, t6, v1
        c->addiu(s3, r0, 8);                              // addiu s3, r0, 8
        c->daddiu(s5, s5, 4);                             // daddiu s5, s5, 4
        c->andi(t3, s4, 1);                               // andi t3, s4, 1
        // nop                                            // sll r0, r0, 0
        bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L34
        // nop                                            // sll r0, r0, 0
        if (bc) {next_block = 2;}                         // branch non-likely

        break;

      case 1:
        next_block = 2;
        c->lqc2(vf1, 0, t4);                              // lqc2 vf1, 0(t4)
        c->lqc2(vf2, 16, t4);                             // lqc2 vf2, 16(t4)
        c->lqc2(vf3, 32, t4);                             // lqc2 vf3, 32(t4)
        c->lqc2(vf4, 48, t4);                             // lqc2 vf4, 48(t4)
        c->lqc2(vf9, 0, a0);                              // lqc2 vf9, 0(a0)
        c->daddiu(t4, t4, 64);                            // daddiu t4, t4, 64
        c->lqc2(vf10, 16, a0);                            // lqc2 vf10, 16(a0)
        // nop                                            // sll r0, r0, 0
        c->lqc2(vf11, 32, a0);                            // lqc2 vf11, 32(a0)
        c->lqc2(vf12, 48, a0);                            // lqc2 vf12, 48(a0)
        c->vmula_bc(DEST::xyzw, BC::w, vf9, vf0);         // vmulaw.xyzw acc, vf9, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf1, vf13);   // vmaddx.xyzw vf9, vf1, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf10, vf0);        // vmulaw.xyzw acc, vf10, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf10, vf2, vf13);  // vmaddx.xyzw vf10, vf2, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf11, vf0);        // vmulaw.xyzw acc, vf11, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf11, vf3, vf13);  // vmaddx.xyzw vf11, vf3, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf12, vf0);        // vmulaw.xyzw acc, vf12, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf12, vf4, vf13);  // vmaddx.xyzw vf12, vf4, vf13
        c->sqc2(vf9, 0, a0);                              // sqc2 vf9, 0(a0)
        c->sqc2(vf10, 16, a0);                            // sqc2 vf10, 16(a0)
        c->sqc2(vf11, 32, a0);                            // sqc2 vf11, 32(a0)
        c->sqc2(vf12, 48, a0);                            // sqc2 vf12, 48(a0)

      case 2:
        next_block = 3;
        c->andi(t3, s4, 2);                               // andi t3, s4, 2
        c->daddiu(a0, a0, 64);                            // daddiu a0, a0, 64
        bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L35
        // nop                                            // sll r0, r0, 0
        if (bc) {next_block = 4;}                         // branch non-likely

        break;

      case 3:
        next_block = 4;
        c->lqc2(vf1, 0, t4);                              // lqc2 vf1, 0(t4)
        c->lqc2(vf2, 16, t4);                             // lqc2 vf2, 16(t4)
        c->lqc2(vf3, 32, t4);                             // lqc2 vf3, 32(t4)
        c->lqc2(vf4, 48, t4);                             // lqc2 vf4, 48(t4)
        c->lqc2(vf9, 0, a0);                              // lqc2 vf9, 0(a0)
        c->daddiu(t4, t4, 64);                            // daddiu t4, t4, 64
        c->lqc2(vf10, 16, a0);                            // lqc2 vf10, 16(a0)
        // nop                                            // sll r0, r0, 0
        c->lqc2(vf11, 32, a0);                            // lqc2 vf11, 32(a0)
        c->lqc2(vf12, 48, a0);                            // lqc2 vf12, 48(a0)
        c->vmula_bc(DEST::xyzw, BC::w, vf9, vf0);         // vmulaw.xyzw acc, vf9, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf1, vf13);   // vmaddx.xyzw vf9, vf1, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf10, vf0);        // vmulaw.xyzw acc, vf10, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf10, vf2, vf13);  // vmaddx.xyzw vf10, vf2, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf11, vf0);        // vmulaw.xyzw acc, vf11, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf11, vf3, vf13);  // vmaddx.xyzw vf11, vf3, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf12, vf0);        // vmulaw.xyzw acc, vf12, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf12, vf4, vf13);  // vmaddx.xyzw vf12, vf4, vf13
        c->sqc2(vf9, 0, a0);                              // sqc2 vf9, 0(a0)
        c->sqc2(vf10, 16, a0);                            // sqc2 vf10, 16(a0)
        c->sqc2(vf11, 32, a0);                            // sqc2 vf11, 32(a0)
        c->sqc2(vf12, 48, a0);                            // sqc2 vf12, 48(a0)

      case 4:
        next_block = 5;
        c->lw(s4, -4, s5);                                // lw s4, -4(s5)
        c->daddiu(a0, a0, 64);                            // daddiu a0, a0, 64

      case 5:
        next_block = 6;
        c->andi(t3, s4, 15);                              // andi t3, s4, 15
        c->sra(s4, s4, 4);                                // sra s4, s4, 4
        c->sll(t3, t3, 2);                                // sll t3, t3, 2
        c->daddiu(s3, s3, -1);                            // daddiu s3, s3, -1
        c->daddu(t3, t3, t2);                             // daddu t3, t3, t2
        c->daddiu(s2, s2, -1);                            // daddiu s2, s2, -1
        c->lw(t3, 0, t3);                                 // lw t3, 0(t3)
        // nop                                            // sll r0, r0, 0
        // nop                                            // sll r0, r0, 0
        // nop                                            // sll r0, r0, 0
        next_block = 0x3ee6b6f0 ^ c->gprs[t3].du32[0];    // jr t3
        ASSERT(next_block < 33);
        break;
        // nop                                            // sll r0, r0, 0

      case 6:
        next_block = 7;
        bc = c->sgpr64(s2) == 0;                          // beq s2, r0, L44
        c->daddiu(a0, a0, 48);                            // daddiu a0, a0, 48
        if (bc) {next_block = 32;}                        // branch non-likely

        break;

      case 7:
        next_block = 8;
        bc = c->sgpr64(s3) != 0;                          // bne s3, r0, L36
        // nop                                            // sll r0, r0, 0
        if (bc) {next_block = 5;}                         // branch non-likely

        break;

      case 8:
        next_block = 9;
        c->lw(s4, 0, s5);                                 // lw s4, 0(s5)
        c->daddiu(s5, s5, 4);                             // daddiu s5, s5, 4
        //beq r0, r0, L36                                 // beq r0, r0, L36
        c->addiu(s3, r0, 8);                              // addiu s3, r0, 8
        next_block = 5;                                   // branch always

        break;

      case 9:
        next_block = 10;
        c->ld(t9, 0, t4);                                 // ld t9, 0(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lqc2(vf6, 16, a0);                             // lqc2 vf6, 16(a0)
        c->pextlh(t9, t9, r0);                            // pextlh t9, t9, r0
        c->psraw(t9, t9, 16);                             // psraw t9, t9, 16
        c->mov128_vf_gpr(vf4, t9);                        // qmtc2.i vf4, t9
        c->lw(s6, 0, t5);                                 // lw s6, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(gp, 0, t6);                                 // lh gp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->lqc2(vf3, 0, a0);                              // lqc2 vf3, 0(a0)
        c->vitof15(DEST::xyzw, vf4, vf4);                 // vitof15.xyzw vf4, vf4
        c->pextlw(s6, gp, s6);                            // pextlw s6, gp, s6
        c->pextlh(s6, s6, r0);                            // pextlh s6, s6, r0
        c->psraw(s6, s6, 16);                             // psraw s6, s6, 16
        c->vmul(DEST::xyzw, vf10, vf4, vf6);              // vmul.xyzw vf10, vf4, vf6
        c->mov128_vf_gpr(vf1, s6);                        // qmtc2.i vf1, s6
        c->lw(t8, 0, t5);                                 // lw t8, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(fp, 0, t6);                                 // lh fp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->vmula_bc(DEST::xyzw, BC::w, vf10, vf0);        // vmulaw.xyzw acc, vf10, vf0
        c->vmadda_bc(DEST::xyzw, BC::z, vf0, vf10);       // vmaddaz.xyzw acc, vf0, vf10
        c->vmadda_bc(DEST::xyzw, BC::y, vf0, vf10);       // vmadday.xyzw acc, vf0, vf10
        c->vmadd_bc(DEST::xyzw, BC::x, vf10, vf0, vf10);  // vmaddx.xyzw vf10, vf0, vf10
        c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
        c->lqc2(vf9, 32, a0);                             // lqc2 vf9, 32(a0)
        c->pextlw(t8, fp, t8);                            // pextlw t8, fp, t8
        c->mov128_gpr_vf(t9, vf10);                       // qmfc2.i t9, vf10
        c->pextlh(t8, t8, r0);                            // pextlh t8, t8, r0
        c->psraw(t8, t8, 16);                             // psraw t8, t8, 16
        c->mov128_vf_gpr(vf7, t8);                        // qmtc2.i vf7, t8
        c->pcpyud(t9, t9, r0);                            // pcpyud t9, t9, r0
        if (((s64)c->sgpr64(t9)) < 0) {                   // bltzl t9, L38
          c->vsub(DEST::xyzw, vf4, vf15, vf4);            // vsub.xyzw vf4, vf15, vf4
          next_block = 11;
          break;
        }

      case 11:
        next_block = 12;
        c->vitof12(DEST::xyzw, vf7, vf7);                 // vitof12.xyzw vf7, vf7
        c->vmula_bc(DEST::xyzw, BC::w, vf6, vf0);         // vmulaw.xyzw acc, vf6, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf6, vf4, vf13);   // vmaddx.xyzw vf6, vf4, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
        c->vmadd_bc(DEST::xyzw, BC::y, vf3, vf1, vf13);   // vmaddy.xyzw vf3, vf1, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf9, vf0);         // vmulaw.xyzw acc, vf9, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf7, vf13);   // vmaddx.xyzw vf9, vf7, vf13
        c->sqc2(vf3, 0, a0);                              // sqc2 vf3, 0(a0)
        c->sqc2(vf6, 16, a0);                             // sqc2 vf6, 16(a0)
        c->sqc2(vf9, 32, a0);                             // sqc2 vf9, 32(a0)
        //beq r0, r0, L37                                 // beq r0, r0, L37
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 12:
        next_block = 13;
        c->ld(t9, 8, t4);                                 // ld t9, 8(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lqc2(vf6, 16, a0);                             // lqc2 vf6, 16(a0)
        c->pextlh(t9, t9, r0);                            // pextlh t9, t9, r0
        c->psraw(t9, t9, 16);                             // psraw t9, t9, 16
        c->mov128_vf_gpr(vf4, t9);                        // qmtc2.i vf4, t9
        c->ld(s6, -8, t4);                                // ld s6, -8(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lw(gp, 0, t5);                                 // lw gp, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lqc2(vf3, 0, a0);                              // lqc2 vf3, 0(a0)
        c->vitof15(DEST::xyzw, vf4, vf4);                 // vitof15.xyzw vf4, vf4
        c->pcpyld(s6, gp, s6);                            // pcpyld s6, gp, s6
        c->vmul(DEST::xyzw, vf10, vf4, vf6);              // vmul.xyzw vf10, vf4, vf6
        c->mov128_vf_gpr(vf1, s6);                        // qmtc2.i vf1, s6
        c->lw(t8, 0, t5);                                 // lw t8, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(fp, 0, t6);                                 // lh fp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->vmula_bc(DEST::xyzw, BC::w, vf10, vf0);        // vmulaw.xyzw acc, vf10, vf0
        c->vmadda_bc(DEST::xyzw, BC::z, vf0, vf10);       // vmaddaz.xyzw acc, vf0, vf10
        c->vmadda_bc(DEST::xyzw, BC::y, vf0, vf10);       // vmadday.xyzw acc, vf0, vf10
        c->vmadd_bc(DEST::xyzw, BC::x, vf10, vf0, vf10);  // vmaddx.xyzw vf10, vf0, vf10
        c->lqc2(vf9, 32, a0);                             // lqc2 vf9, 32(a0)
        c->pextlw(t8, fp, t8);                            // pextlw t8, fp, t8
        c->mov128_gpr_vf(t9, vf10);                       // qmfc2.i t9, vf10
        c->pextlh(t8, t8, r0);                            // pextlh t8, t8, r0
        c->psraw(t8, t8, 16);                             // psraw t8, t8, 16
        c->mov128_vf_gpr(vf7, t8);                        // qmtc2.i vf7, t8
        c->pcpyud(t9, t9, r0);                            // pcpyud t9, t9, r0
        if (((s64)c->sgpr64(t9)) < 0) {                   // bltzl t9, L39
          c->vsub(DEST::xyzw, vf4, vf15, vf4);            // vsub.xyzw vf4, vf15, vf4
          next_block = 14;
          break;
        }

      case 14:
        next_block = 15;
        c->vitof12(DEST::xyzw, vf7, vf7);                 // vitof12.xyzw vf7, vf7
        c->vmula_bc(DEST::xyzw, BC::w, vf6, vf0);         // vmulaw.xyzw acc, vf6, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf6, vf4, vf13);   // vmaddx.xyzw vf6, vf4, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf3, vf1, vf13);   // vmaddx.xyzw vf3, vf1, vf13
        c->vmula_bc(DEST::xyzw, BC::w, vf9, vf0);         // vmulaw.xyzw acc, vf9, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf7, vf13);   // vmaddx.xyzw vf9, vf7, vf13
        c->sqc2(vf3, 0, a0);                              // sqc2 vf3, 0(a0)
        c->sqc2(vf6, 16, a0);                             // sqc2 vf6, 16(a0)
        c->sqc2(vf9, 32, a0);                             // sqc2 vf9, 32(a0)
        //beq r0, r0, L37                                 // beq r0, r0, L37
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 15:
        next_block = 16;
        c->ld(t9, 0, t4);                                 // ld t9, 0(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lqc2(vf6, 16, a0);                             // lqc2 vf6, 16(a0)
        c->pextlh(t9, t9, r0);                            // pextlh t9, t9, r0
        c->psraw(t9, t9, 16);                             // psraw t9, t9, 16
        c->mov128_vf_gpr(vf4, t9);                        // qmtc2.i vf4, t9
        c->lw(t8, 0, t5);                                 // lw t8, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(fp, 0, t6);                                 // lh fp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->lqc2(vf9, 32, a0);                             // lqc2 vf9, 32(a0)
        c->vitof15(DEST::xyzw, vf4, vf4);                 // vitof15.xyzw vf4, vf4
        c->pextlw(t8, fp, t8);                            // pextlw t8, fp, t8
        c->pextlh(t8, t8, r0);                            // pextlh t8, t8, r0
        c->psraw(t8, t8, 16);                             // psraw t8, t8, 16
        c->vmul(DEST::xyzw, vf10, vf4, vf6);              // vmul.xyzw vf10, vf4, vf6
        c->mov128_vf_gpr(vf7, t8);                        // qmtc2.i vf7, t8
        c->vmula_bc(DEST::xyzw, BC::w, vf10, vf0);        // vmulaw.xyzw acc, vf10, vf0
        c->vitof12(DEST::xyzw, vf7, vf7);                 // vitof12.xyzw vf7, vf7
        c->vmadda_bc(DEST::xyzw, BC::z, vf0, vf10);       // vmaddaz.xyzw acc, vf0, vf10
        c->vmadda_bc(DEST::xyzw, BC::y, vf0, vf10);       // vmadday.xyzw acc, vf0, vf10
        c->vmadd_bc(DEST::xyzw, BC::x, vf10, vf0, vf10);  // vmaddx.xyzw vf10, vf0, vf10
        c->vmula_bc(DEST::xyzw, BC::w, vf9, vf0);         // vmulaw.xyzw acc, vf9, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf7, vf13);   // vmaddx.xyzw vf9, vf7, vf13
        c->mov128_gpr_vf(t9, vf10);                       // qmfc2.i t9, vf10
        c->pcpyud(t9, t9, r0);                            // pcpyud t9, t9, r0
        if (((s64)c->sgpr64(t9)) < 0) {                   // bltzl t9, L40
          c->vsub(DEST::xyzw, vf4, vf15, vf4);            // vsub.xyzw vf4, vf15, vf4
          next_block = 17;
          break;
        }

      case 17:
        next_block = 18;
        c->vmula_bc(DEST::xyzw, BC::w, vf6, vf0);         // vmulaw.xyzw acc, vf6, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf6, vf4, vf13);   // vmaddx.xyzw vf6, vf4, vf13
        c->sqc2(vf6, 16, a0);                             // sqc2 vf6, 16(a0)
        c->sqc2(vf9, 32, a0);                             // sqc2 vf9, 32(a0)
        //beq r0, r0, L37                                 // beq r0, r0, L37
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 18:
        next_block = 19;
        c->lw(s6, 0, t5);                                 // lw s6, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(gp, 0, t6);                                 // lh gp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->lqc2(vf3, 0, a0);                              // lqc2 vf3, 0(a0)
        c->pextlw(s6, gp, s6);                            // pextlw s6, gp, s6
        c->pextlh(s6, s6, r0);                            // pextlh s6, s6, r0
        c->psraw(s6, s6, 16);                             // psraw s6, s6, 16
        c->mov128_vf_gpr(vf1, s6);                        // qmtc2.i vf1, s6
        c->lw(t8, 0, t5);                                 // lw t8, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(fp, 0, t6);                                 // lh fp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->lqc2(vf9, 32, a0);                             // lqc2 vf9, 32(a0)
        c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
        c->pextlw(t8, fp, t8);                            // pextlw t8, fp, t8
        c->pextlh(t8, t8, r0);                            // pextlh t8, t8, r0
        c->psraw(t8, t8, 16);                             // psraw t8, t8, 16
        c->mov128_vf_gpr(vf7, t8);                        // qmtc2.i vf7, t8
        c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
        c->vmadd_bc(DEST::xyzw, BC::y, vf3, vf1, vf13);   // vmaddy.xyzw vf3, vf1, vf13
        c->vitof12(DEST::xyzw, vf7, vf7);                 // vitof12.xyzw vf7, vf7
        c->vmula_bc(DEST::xyzw, BC::w, vf9, vf0);         // vmulaw.xyzw acc, vf9, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf7, vf13);   // vmaddx.xyzw vf9, vf7, vf13
        c->sqc2(vf3, 0, a0);                              // sqc2 vf3, 0(a0)
        c->sqc2(vf9, 32, a0);                             // sqc2 vf9, 32(a0)
        //beq r0, r0, L37                                 // beq r0, r0, L37
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 19:
        next_block = 20;
        c->ld(s6, 0, t4);                                 // ld s6, 0(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lw(gp, 0, t5);                                 // lw gp, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lqc2(vf3, 0, a0);                              // lqc2 vf3, 0(a0)
        c->pcpyld(s6, gp, s6);                            // pcpyld s6, gp, s6
        c->mov128_vf_gpr(vf1, s6);                        // qmtc2.i vf1, s6
        c->lw(t8, 0, t5);                                 // lw t8, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(fp, 0, t6);                                 // lh fp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->lqc2(vf9, 32, a0);                             // lqc2 vf9, 32(a0)
        c->pextlw(t8, fp, t8);                            // pextlw t8, fp, t8
        c->pextlh(t8, t8, r0);                            // pextlh t8, t8, r0
        c->psraw(t8, t8, 16);                             // psraw t8, t8, 16
        c->mov128_vf_gpr(vf7, t8);                        // qmtc2.i vf7, t8
        c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf3, vf1, vf13);   // vmaddx.xyzw vf3, vf1, vf13
        c->vitof12(DEST::xyzw, vf7, vf7);                 // vitof12.xyzw vf7, vf7
        c->vmula_bc(DEST::xyzw, BC::w, vf9, vf0);         // vmulaw.xyzw acc, vf9, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf7, vf13);   // vmaddx.xyzw vf9, vf7, vf13
        c->sqc2(vf3, 0, a0);                              // sqc2 vf3, 0(a0)
        c->sqc2(vf9, 32, a0);                             // sqc2 vf9, 32(a0)
        //beq r0, r0, L37                                 // beq r0, r0, L37
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 20:
        next_block = 21;
        c->lw(t8, 0, t5);                                 // lw t8, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(fp, 0, t6);                                 // lh fp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->lqc2(vf9, 32, a0);                             // lqc2 vf9, 32(a0)
        c->pextlw(t8, fp, t8);                            // pextlw t8, fp, t8
        c->pextlh(t8, t8, r0);                            // pextlh t8, t8, r0
        c->psraw(t8, t8, 16);                             // psraw t8, t8, 16
        c->mov128_vf_gpr(vf7, t8);                        // qmtc2.i vf7, t8
        c->vitof12(DEST::xyzw, vf7, vf7);                 // vitof12.xyzw vf7, vf7
        c->vmula_bc(DEST::xyzw, BC::w, vf9, vf0);         // vmulaw.xyzw acc, vf9, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf9, vf7, vf13);   // vmaddx.xyzw vf9, vf7, vf13
        c->sqc2(vf9, 32, a0);                             // sqc2 vf9, 32(a0)
        //beq r0, r0, L37                                 // beq r0, r0, L37
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 21:
        next_block = 22;
        c->ld(t9, 0, t4);                                 // ld t9, 0(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lqc2(vf6, 16, a0);                             // lqc2 vf6, 16(a0)
        c->pextlh(t9, t9, r0);                            // pextlh t9, t9, r0
        c->psraw(t9, t9, 16);                             // psraw t9, t9, 16
        c->mov128_vf_gpr(vf4, t9);                        // qmtc2.i vf4, t9
        c->lw(s6, 0, t5);                                 // lw s6, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(gp, 0, t6);                                 // lh gp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->lqc2(vf3, 0, a0);                              // lqc2 vf3, 0(a0)
        c->vitof15(DEST::xyzw, vf4, vf4);                 // vitof15.xyzw vf4, vf4
        c->pextlw(s6, gp, s6);                            // pextlw s6, gp, s6
        c->pextlh(s6, s6, r0);                            // pextlh s6, s6, r0
        c->psraw(s6, s6, 16);                             // psraw s6, s6, 16
        c->vmul(DEST::xyzw, vf10, vf4, vf6);              // vmul.xyzw vf10, vf4, vf6
        c->mov128_vf_gpr(vf1, s6);                        // qmtc2.i vf1, s6
        c->vmula_bc(DEST::xyzw, BC::w, vf10, vf0);        // vmulaw.xyzw acc, vf10, vf0
        c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
        c->vmadda_bc(DEST::xyzw, BC::z, vf0, vf10);       // vmaddaz.xyzw acc, vf0, vf10
        c->vmadda_bc(DEST::xyzw, BC::y, vf0, vf10);       // vmadday.xyzw acc, vf0, vf10
        c->vmadd_bc(DEST::xyzw, BC::x, vf10, vf0, vf10);  // vmaddx.xyzw vf10, vf0, vf10
        c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
        c->vmadd_bc(DEST::xyzw, BC::y, vf3, vf1, vf13);   // vmaddy.xyzw vf3, vf1, vf13
        c->mov128_gpr_vf(t9, vf10);                       // qmfc2.i t9, vf10
        c->pcpyud(t9, t9, r0);                            // pcpyud t9, t9, r0
        if (((s64)c->sgpr64(t9)) < 0) {                   // bltzl t9, L41
          c->vsub(DEST::xyzw, vf4, vf15, vf4);            // vsub.xyzw vf4, vf15, vf4
          next_block = 23;
          break;
        }

      case 23:
        next_block = 24;
        c->vmula_bc(DEST::xyzw, BC::w, vf6, vf0);         // vmulaw.xyzw acc, vf6, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf6, vf4, vf13);   // vmaddx.xyzw vf6, vf4, vf13
        c->sqc2(vf3, 0, a0);                              // sqc2 vf3, 0(a0)
        c->sqc2(vf6, 16, a0);                             // sqc2 vf6, 16(a0)
        //beq r0, r0, L37                                 // beq r0, r0, L37
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 24:
        next_block = 25;
        c->ld(t9, 8, t4);                                 // ld t9, 8(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lqc2(vf6, 16, a0);                             // lqc2 vf6, 16(a0)
        c->pextlh(t9, t9, r0);                            // pextlh t9, t9, r0
        c->psraw(t9, t9, 16);                             // psraw t9, t9, 16
        c->mov128_vf_gpr(vf4, t9);                        // qmtc2.i vf4, t9
        c->ld(s6, -8, t4);                                // ld s6, -8(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lw(gp, 0, t5);                                 // lw gp, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lqc2(vf3, 0, a0);                              // lqc2 vf3, 0(a0)
        c->vitof15(DEST::xyzw, vf4, vf4);                 // vitof15.xyzw vf4, vf4
        c->pcpyld(s6, gp, s6);                            // pcpyld s6, gp, s6
        c->vmul(DEST::xyzw, vf10, vf4, vf6);              // vmul.xyzw vf10, vf4, vf6
        c->mov128_vf_gpr(vf1, s6);                        // qmtc2.i vf1, s6
        c->vmula_bc(DEST::xyzw, BC::w, vf10, vf0);        // vmulaw.xyzw acc, vf10, vf0
        c->vmadda_bc(DEST::xyzw, BC::z, vf0, vf10);       // vmaddaz.xyzw acc, vf0, vf10
        c->vmadda_bc(DEST::xyzw, BC::y, vf0, vf10);       // vmadday.xyzw acc, vf0, vf10
        c->vmadd_bc(DEST::xyzw, BC::x, vf10, vf0, vf10);  // vmaddx.xyzw vf10, vf0, vf10
        c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf3, vf1, vf13);   // vmaddx.xyzw vf3, vf1, vf13
        c->mov128_gpr_vf(t9, vf10);                       // qmfc2.i t9, vf10
        c->pcpyud(t9, t9, r0);                            // pcpyud t9, t9, r0
        if (((s64)c->sgpr64(t9)) < 0) {                   // bltzl t9, L42
          c->vsub(DEST::xyzw, vf4, vf15, vf4);            // vsub.xyzw vf4, vf15, vf4
          next_block = 26;
          break;
        }

      case 26:
        next_block = 27;
        c->vmula_bc(DEST::xyzw, BC::w, vf6, vf0);         // vmulaw.xyzw acc, vf6, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf6, vf4, vf13);   // vmaddx.xyzw vf6, vf4, vf13
        c->sqc2(vf3, 0, a0);                              // sqc2 vf3, 0(a0)
        c->sqc2(vf6, 16, a0);                             // sqc2 vf6, 16(a0)
        //beq r0, r0, L37                                 // beq r0, r0, L37
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 27:
        next_block = 28;
        c->ld(t9, 0, t4);                                 // ld t9, 0(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lqc2(vf6, 16, a0);                             // lqc2 vf6, 16(a0)
        c->pextlh(t9, t9, r0);                            // pextlh t9, t9, r0
        c->psraw(t9, t9, 16);                             // psraw t9, t9, 16
        c->mov128_vf_gpr(vf4, t9);                        // qmtc2.i vf4, t9
        c->vitof15(DEST::xyzw, vf4, vf4);                 // vitof15.xyzw vf4, vf4
        c->vmul(DEST::xyzw, vf10, vf4, vf6);              // vmul.xyzw vf10, vf4, vf6
        c->vmula_bc(DEST::xyzw, BC::w, vf10, vf0);        // vmulaw.xyzw acc, vf10, vf0
        c->vmadda_bc(DEST::xyzw, BC::z, vf0, vf10);       // vmaddaz.xyzw acc, vf0, vf10
        c->vmadda_bc(DEST::xyzw, BC::y, vf0, vf10);       // vmadday.xyzw acc, vf0, vf10
        c->vmadd_bc(DEST::xyzw, BC::x, vf10, vf0, vf10);  // vmaddx.xyzw vf10, vf0, vf10
        c->mov128_gpr_vf(t9, vf10);                       // qmfc2.i t9, vf10
        c->pcpyud(t9, t9, r0);                            // pcpyud t9, t9, r0
        if (((s64)c->sgpr64(t9)) < 0) {                   // bltzl t9, L43
          c->vsub(DEST::xyzw, vf4, vf15, vf4);            // vsub.xyzw vf4, vf15, vf4
          next_block = 29;
          break;
        }

      case 29:
        next_block = 30;
        c->vmula_bc(DEST::xyzw, BC::w, vf6, vf0);         // vmulaw.xyzw acc, vf6, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf6, vf4, vf13);   // vmaddx.xyzw vf6, vf4, vf13
        c->sqc2(vf6, 16, a0);                             // sqc2 vf6, 16(a0)
        //beq r0, r0, L37                                 // beq r0, r0, L37
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 30:
        next_block = 31;
        c->lw(s6, 0, t5);                                 // lw s6, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lh(gp, 0, t6);                                 // lh gp, 0(t6)
        c->daddiu(t6, t6, 2);                             // daddiu t6, t6, 2
        c->lqc2(vf3, 0, a0);                              // lqc2 vf3, 0(a0)
        c->pextlw(s6, gp, s6);                            // pextlw s6, gp, s6
        c->pextlh(s6, s6, r0);                            // pextlh s6, s6, r0
        c->psraw(s6, s6, 16);                             // psraw s6, s6, 16
        c->mov128_vf_gpr(vf1, s6);                        // qmtc2.i vf1, s6
        c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
        c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
        c->vmadd_bc(DEST::xyzw, BC::y, vf3, vf1, vf13);   // vmaddy.xyzw vf3, vf1, vf13
        c->sqc2(vf3, 0, a0);                              // sqc2 vf3, 0(a0)
        //beq r0, r0, L37                                 // beq r0, r0, L37
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 31:
        next_block = 32;
        c->ld(s6, 0, t4);                                 // ld s6, 0(t4)
        c->daddiu(t4, t4, 8);                             // daddiu t4, t4, 8
        c->lw(gp, 0, t5);                                 // lw gp, 0(t5)
        c->daddiu(t5, t5, 4);                             // daddiu t5, t5, 4
        c->lqc2(vf3, 0, a0);                              // lqc2 vf3, 0(a0)
        c->pcpyld(s6, gp, s6);                            // pcpyld s6, gp, s6
        c->mov128_vf_gpr(vf1, s6);                        // qmtc2.i vf1, s6
        c->vmula_bc(DEST::xyzw, BC::w, vf3, vf0);         // vmulaw.xyzw acc, vf3, vf0
        c->vmadd_bc(DEST::xyzw, BC::x, vf3, vf1, vf13);   // vmaddx.xyzw vf3, vf1, vf13
        c->sqc2(vf3, 0, a0);                              // sqc2 vf3, 0(a0)
        //beq r0, r0, L37                                 // beq r0, r0, L37
        // nop                                            // sll r0, r0, 0
        next_block = 6;                                   // branch always

        break;

      case 32:
        next_block = 33;
        c->lq(a0, 0, sp);                                 // lq a0, 0(sp)
        // nop                                            // sll r0, r0, 0
        //jr ra                                           // jr ra
        c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
        goto end_of_function;                             // return

        //jr ra                                           // jr ra
        c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
        goto end_of_function;                             // return

        // nop                                            // sll r0, r0, 0
        // nop                                            // sll r0, r0, 0
        // nop                                            // sll r0, r0, 0
    }
  }
  end_of_function:
  return c->gprs[v0].du64[0];
}

u32 jump_table_vals[16] = {
    0x3ee6b6f9, // = 9 ^ 1055307504
    0x3ee6b6ff, // = 15 ^ 1055307504
    0x3ee6b6e2, // = 18 ^ 1055307504
    0x3ee6b6e4, // = 20 ^ 1055307504
    0x3ee6b6e5, // = 21 ^ 1055307504
    0x3ee6b6eb, // = 27 ^ 1055307504
    0x3ee6b6ee, // = 30 ^ 1055307504
    0x3ee6b6f6, // = 6 ^ 1055307504
    0x3ee6b6fc, // = 12 ^ 1055307504
    0x3ee6b6ff, // = 15 ^ 1055307504
    0x3ee6b6e3, // = 19 ^ 1055307504
    0x3ee6b6e4, // = 20 ^ 1055307504
    0x3ee6b6e8, // = 24 ^ 1055307504
    0x3ee6b6eb, // = 27 ^ 1055307504
    0x3ee6b6ef, // = 31 ^ 1055307504
    0x3ee6b6f6, // = 6 ^ 1055307504
};

} // namespace decompress_fixed_data_to_accumulator
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak2 {
namespace normalize_frame_quaternions {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
  c->daddiu(s2, s2, -2);                            // daddiu s2, s2, -2
  c->sw(a0, 0, sp);                                 // sw a0, 0(sp)
  c->daddiu(a0, a0, 128);                           // daddiu a0, a0, 128

  block_1:
  c->lqc2(vf4, 16, a0);                             // lqc2 vf4, 16(a0)
  c->lqc2(vf1, 0, a0);                              // lqc2 vf1, 0(a0)
  c->lqc2(vf7, 32, a0);                             // lqc2 vf7, 32(a0)
  c->vmul(DEST::xyzw, vf10, vf4, vf4);              // vmul.xyzw vf10, vf4, vf4
  c->vmove(DEST::w, vf1, vf0);                      // vmove.w vf1, vf0
  c->vmove(DEST::w, vf7, vf0);                      // vmove.w vf7, vf0
  c->vmula_bc(DEST::xyzw, BC::w, vf0, vf10);        // vmulaw.xyzw acc, vf0, vf10
  c->vmadda_bc(DEST::xyzw, BC::z, vf0, vf10);       // vmaddaz.xyzw acc, vf0, vf10
  c->vmadda_bc(DEST::xyzw, BC::y, vf0, vf10);       // vmadday.xyzw acc, vf0, vf10
  c->vmadd_bc(DEST::xyzw, BC::x, vf10, vf0, vf10);  // vmaddx.xyzw vf10, vf0, vf10
  c->sqc2(vf1, 0, a0);                              // sqc2 vf1, 0(a0)
  c->sqc2(vf7, 32, a0);                             // sqc2 vf7, 32(a0)
  c->daddiu(a0, a0, 48);                            // daddiu a0, a0, 48
  c->vrsqrt(vf0, BC::w, vf10, BC::w);               // vrsqrt Q, vf0.w, vf10.w
  c->vwaitq();                                      // vwaitq
  c->vmulq(DEST::xyzw, vf4, vf4);                   // vmulq.xyzw vf4, vf4, Q
  c->daddiu(s2, s2, -1);                            // daddiu s2, s2, -1
  bc = c->sgpr64(s2) != 0;                          // bne s2, r0, L46
  c->sqc2(vf4, -32, a0);                            // sqc2 vf4, -32(a0)
  if (bc) {goto block_1;}                           // branch non-likely

  c->lw(a0, 0, sp);                                 // lw a0, 0(sp)
  // nop                                            // sll r0, r0, 0
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
  goto end_of_function;                             // return

  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

} // namespace normalize_frame_quaternions
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak2 {
namespace clear_frame_accumulator {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 0, sp);                                 // sw a0, 0(sp)
  // nop                                            // sll r0, r0, 0
  c->sq(r0, 0, a0);                                 // sq r0, 0(a0)
  c->daddiu(s2, s2, -2);                            // daddiu s2, s2, -2
  c->sq(r0, 16, a0);                                // sq r0, 16(a0)
  // nop                                            // sll r0, r0, 0
  c->sq(r0, 32, a0);                                // sq r0, 32(a0)
  // nop                                            // sll r0, r0, 0
  c->sq(r0, 48, a0);                                // sq r0, 48(a0)
  // nop                                            // sll r0, r0, 0
  c->sq(r0, 64, a0);                                // sq r0, 64(a0)
  // nop                                            // sll r0, r0, 0
  c->sq(r0, 80, a0);                                // sq r0, 80(a0)
  // nop                                            // sll r0, r0, 0
  c->sq(r0, 96, a0);                                // sq r0, 96(a0)
  // nop                                            // sll r0, r0, 0
  c->sq(r0, 112, a0);                               // sq r0, 112(a0)
  c->daddiu(a0, a0, 128);                           // daddiu a0, a0, 128

  block_1:
  c->sq(r0, 0, a0);                                 // sq r0, 0(a0)
  c->daddiu(s2, s2, -1);                            // daddiu s2, s2, -1
  c->sq(r0, 16, a0);                                // sq r0, 16(a0)
  c->daddiu(a0, a0, 48);                            // daddiu a0, a0, 48
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(s2) != 0;                          // bne s2, r0, L48
  c->sq(r0, -16, a0);                               // sq r0, -16(a0)
  if (bc) {goto block_1;}                           // branch non-likely

  c->lw(a0, 0, sp);                                 // lw a0, 0(sp)
  // nop                                            // sll r0, r0, 0
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
  goto end_of_function;                             // return

  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}
} // namespace clear_frame_accumulator
} // namespace Mips2C


//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak2 {
namespace calc_animation_from_spr {


u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  [[maybe_unused]] u32 call_addr = 0;
  u32 madr, sadr, qwc;
  c->mov64(v1, a1);                                 // or v1, a1, r0
  c->daddiu(sp, sp, -192);                          // daddiu sp, sp, -192
  c->sq(s0, 0, sp);                                 // sq s0, 0(sp)
  c->sq(s1, 16, sp);                                // sq s1, 16(sp)
  c->sq(s2, 32, sp);                                // sq s2, 32(sp)
  c->sq(s3, 48, sp);                                // sq s3, 48(sp)
  c->sq(s4, 64, sp);                                // sq s4, 64(sp)
  c->sq(s5, 80, sp);                                // sq s5, 80(sp)
  c->sq(s6, 96, sp);                                // sq s6, 96(sp)
  c->sq(t8, 112, sp);                               // sq t8, 112(sp)
  c->sq(t9, 128, sp);                               // sq t9, 128(sp)
  c->sq(gp, 144, sp);                               // sq gp, 144(sp)
  c->sq(fp, 160, sp);                               // sq fp, 160(sp)
  c->sq(ra, 176, sp);                               // sq ra, 176(sp)
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
  c->mov128_vf_gpr(vf15, r0);                       // qmtc2.i vf15, r0
  c->sw(a1, 0, sp);                                 // sw a1, 0(sp)
  //c->lui(v1, 28672);                                // lui v1, 28672
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);
  c->lw(s1, 2384, v1);                              // lw s1, 2400(v1)
  c->daddiu(t7, v1, 1808);                          // daddiu t7, v1, 1824
  c->lui(s0, 4096);                                 // lui s0, 4096
  c->daddiu(t1, v1, 7328);                          // daddiu t1, v1, 7344
  bc = c->sgpr64(s1) == 0;                          // beq s1, r0, L7
  c->ori(s0, s0, 54272);                            // ori s0, s0, 54272 // spr to (d400)
  if (bc) {goto block_12;}                          // branch non-likely

  c->lw(t2, 0, t7);                                 // lw t2, 0(t7)
  c->addiu(t3, r0, 7392);                           // addiu t3, r0, 7408
  c->lw(t4, 4, t7);                                 // lw t4, 4(t7)
  c->addiu(v1, r0, 256);                            // addiu v1, r0, 256
  // c->sw(t2, 16, s0);                                // sw t2, 16(s0)
  madr = c->sgpr64(t2);
  c->vadd_bc(DEST::xyzw, BC::w, vf14, vf15, vf0);   // vaddw.xyzw vf14, vf15, vf0
  //c->sw(t3, 128, s0);                               // sw t3, 128(s0)
  sadr = c->sgpr64(t3);
  // nop                                            // sll r0, r0, 0
  //c->sw(t4, 32, s0);                                // sw t4, 32(s0)
  qwc = c->sgpr64(t4);
  // Unknown instr: sync.l
  //c->sw(v1, 0, s0);                                 // sw v1, 0(s0)
  spad_to_dma_no_sadr_off(cache.fake_scratchpad_data, madr, sadr, qwc);
  // Unknown instr: sync.l
  c->load_symbol2(t9, cache.clear_frame_accumulator);// lw t9, clear-frame-accumulator(s7)
  c->vadd(DEST::yz, vf14, vf14, vf14);              // vadd.yz vf14, vf14, vf14
  c->lw(s2, 0, sp);                                 // lw s2, 0(sp)
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->vadd(DEST::yz, vf14, vf14, vf14);              // vadd.yz vf14, vf14, vf14
  //c->jalr(call_addr);                               // jalr ra, t9
  clear_frame_accumulator::execute(c);

  block_2:
//  c->lw(v1, 0, s0);                                 // lw v1, 0(s0)
//  // nop                                            // sll r0, r0, 0
//  c->andi(v1, v1, 256);                             // andi v1, v1, 256
//  // nop                                            // sll r0, r0, 0
//  // nop                                            // sll r0, r0, 0
//  // nop                                            // sll r0, r0, 0
//  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L3
//  // nop                                            // sll r0, r0, 0
//  if (bc) {goto block_2;}                           // branch non-likely

  c->lw(t2, 8, t7);                                 // lw t2, 8(t7)
  c->addiu(t3, r0, 9600);                           // addiu t3, r0, 9616
  c->lw(t4, 12, t7);                                // lw t4, 12(t7)
  c->addiu(v1, r0, 256);                            // addiu v1, r0, 256
  //c->sw(t2, 16, s0);                                // sw t2, 16(s0)
  madr = c->sgpr64(t2);
  // nop                                            // sll r0, r0, 0
  //c->sw(t3, 128, s0);                               // sw t3, 128(s0)
  sadr = c->sgpr64(t3);
  // nop                                            // sll r0, r0, 0
  //c->sw(t4, 32, s0);                                // sw t4, 32(s0)
  qwc = c->sgpr64(t4);
  // Unknown instr: sync.l
  //c->sw(v1, 0, s0);                                 // sw v1, 0(s0)
  spad_to_dma_no_sadr_off(cache.fake_scratchpad_data, madr, sadr, qwc);
  // Unknown instr: sync.l
  c->lw(a2, 16, t7);                                // lw a2, 16(t7)
  //c->lui(a1, 28672);                                // lui a1, 28672
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);
  c->load_symbol2(t9, cache.decompress_fixed_data_to_accumulator);// lw t9, decompress-fixed-data-to-accumulator(s7)
  c->daddiu(a1, a1, 7392);                          // daddiu a1, a1, 7408
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->daddiu(s1, s1, -1);                            // daddiu s1, s1, -1
  //c->jalr(call_addr);                               // jalr ra, t9
  decompress_fixed_data_to_accumulator::execute(c);

//  block_4:
//  c->lw(v1, 0, s0);                                 // lw v1, 0(s0)
//  // nop                                            // sll r0, r0, 0
//  c->andi(v1, v1, 256);                             // andi v1, v1, 256
//  // nop                                            // sll r0, r0, 0
//  // nop                                            // sll r0, r0, 0
//  // nop                                            // sll r0, r0, 0
//  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L4
//  // nop                                            // sll r0, r0, 0
//  if (bc) {goto block_4;}                           // branch non-likely

  bc = c->sgpr64(s1) == 0;                          // beq s1, r0, L5
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_7;}                           // branch non-likely

  c->lw(t2, 24, t7);                                // lw t2, 24(t7)
  c->addiu(t3, r0, 7392);                           // addiu t3, r0, 7408
  c->lw(t4, 28, t7);                                // lw t4, 28(t7)
  c->addiu(v1, r0, 256);                            // addiu v1, r0, 256
  //c->sw(t2, 16, s0);                                // sw t2, 16(s0)
  madr = c->sgpr64(t2);
  // nop                                            // sll r0, r0, 0
  //c->sw(t3, 128, s0);                               // sw t3, 128(s0)
  sadr = c->sgpr64(t3);
  // nop                                            // sll r0, r0, 0
  //c->sw(t4, 32, s0);                                // sw t4, 32(s0)
  qwc = c->sgpr64(t4);
  // Unknown instr: sync.l
  //c->sw(v1, 0, s0);                                 // sw v1, 0(s0)
  spad_to_dma_no_sadr_off(cache.fake_scratchpad_data, madr, sadr, qwc);
  // Unknown instr: sync.l

  block_7:
  c->lw(t0, 20, t7);                                // lw t0, 20(t7)
  //c->lui(a1, 28672);                                // lui a1, 28672
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);
  c->lw(a2, 16, t7);                                // lw a2, 16(t7)
  c->daddiu(a1, a1, 9600);                          // daddiu a1, a1, 9616
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L6
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_10;}                          // branch non-likely

  c->lw(a3, 12, t7);                                // lw a3, 12(t7)
  // nop                                            // sll r0, r0, 0
  c->load_symbol2(t9, cache.decompress_frame_data_pair_to_accumulator);// lw t9, decompress-frame-data-pair-to-accumulator(s7)
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(a3, a3, 3);                                // sll a3, a3, 3
  //c->jalr(call_addr);                               // jalr ra, t9
  decompress_frame_data_pair_to_accumulator::execute(c);
  bc = c->sgpr64(s1) != 0;                          // bne s1, r0, L3
  c->daddiu(t7, t7, 24);                            // daddiu t7, t7, 24
  if (bc) {goto block_2;}                           // branch non-likely

  c->load_symbol2(t9, cache.normalize_frame_quaternions);// lw t9, normalize-frame-quaternions(s7)
  // nop                                            // sll r0, r0, 0
  c->lw(s2, 0, sp);                                 // lw s2, 0(sp)
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  //c->jalr(call_addr);                               // jalr ra, t9
  normalize_frame_quaternions::execute(c);
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
  // nop                                            // sll r0, r0, 0
  c->lq(s0, 0, sp);                                 // lq s0, 0(sp)
  c->lq(s1, 16, sp);                                // lq s1, 16(sp)
  c->lq(s2, 32, sp);                                // lq s2, 32(sp)
  c->lq(s3, 48, sp);                                // lq s3, 48(sp)
  c->lq(s4, 64, sp);                                // lq s4, 64(sp)
  c->lq(s5, 80, sp);                                // lq s5, 80(sp)
  c->lq(s6, 96, sp);                                // lq s6, 96(sp)
  c->lq(t8, 112, sp);                               // lq t8, 112(sp)
  c->lq(t9, 128, sp);                               // lq t9, 128(sp)
  c->lq(gp, 144, sp);                               // lq gp, 144(sp)
  c->lq(ra, 176, sp);                               // lq ra, 176(sp)
  c->lq(fp, 160, sp);                               // lq fp, 160(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 192);                           // daddiu sp, sp, 192
  goto end_of_function;                             // return


  block_10:
  c->load_symbol2(t9, cache.decompress_frame_data_to_accumulator);// lw t9, decompress-frame-data-to-accumulator(s7)
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  //c->jalr(call_addr);                               // jalr ra, t9
  decompress_frame_data_to_accumulator::execute(c);
  bc = c->sgpr64(s1) != 0;                          // bne s1, r0, L3
  c->daddiu(t7, t7, 24);                            // daddiu t7, t7, 24
  if (bc) {goto block_2;}                           // branch non-likely

  c->load_symbol2(t9, cache.normalize_frame_quaternions);// lw t9, normalize-frame-quaternions(s7)
  // nop                                            // sll r0, r0, 0
  c->lw(s2, 0, sp);                                 // lw s2, 0(sp)
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  //c->jalr(call_addr);                               // jalr ra, t9
  normalize_frame_quaternions::execute(c);

  block_12:
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
  // nop                                            // sll r0, r0, 0
  c->lq(s0, 0, sp);                                 // lq s0, 0(sp)
  c->lq(s1, 16, sp);                                // lq s1, 16(sp)
  c->lq(s2, 32, sp);                                // lq s2, 32(sp)
  c->lq(s3, 48, sp);                                // lq s3, 48(sp)
  c->lq(s4, 64, sp);                                // lq s4, 64(sp)
  c->lq(s5, 80, sp);                                // lq s5, 80(sp)
  c->lq(s6, 96, sp);                                // lq s6, 96(sp)
  c->lq(t8, 112, sp);                               // lq t8, 112(sp)
  c->lq(t9, 128, sp);                               // lq t9, 128(sp)
  c->lq(gp, 144, sp);                               // lq gp, 144(sp)
  c->lq(ra, 176, sp);                               // lq ra, 176(sp)
  c->lq(fp, 160, sp);                               // lq fp, 160(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 192);                           // daddiu sp, sp, 192
  goto end_of_function;                             // return

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
  cache.clear_frame_accumulator = intern_from_c("clear-frame-accumulator").c();
  cache.decompress_fixed_data_to_accumulator = intern_from_c("decompress-fixed-data-to-accumulator").c();
  cache.decompress_frame_data_pair_to_accumulator = intern_from_c("decompress-frame-data-pair-to-accumulator").c();
  cache.decompress_frame_data_to_accumulator = intern_from_c("decompress-frame-data-to-accumulator").c();
  cache.normalize_frame_quaternions = intern_from_c("normalize-frame-quaternions").c();
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();

  gLinkedFunctionTable.reg("calc-animation-from-spr", execute, 1024);
}

} // namespace calc_animation_from_spr
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak2 {
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
  c->lw(a3, 0, a0);                                 // lw a3, 0(a0)
  c->lui(v1, 16256);                                // lui v1, 16256
  c->lqc2(vf5, 16, a1);                             // lqc2 vf5, 16(a1)
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lw(t0, 16, a3);                                // lw t0, 16(a3)
  // nop                                            // sll r0, r0, 0
  c->lw(a2, 16, a0);                                // lw a2, 16(a0)
  c->vadd(DEST::xyzw, vf6, vf5, vf5);               // vadd.xyzw vf6, vf5, vf5
  c->lwc1(f1, 64, t0);                              // lwc1 f1, 64(t0)
  c->vadd_bc(DEST::x, BC::w, vf2, vf0, vf5);        // vaddw.x vf2, vf0, vf5
  c->lqc2(vf15, 0, a1);                             // lqc2 vf15, 0(a1)
  c->vadd_bc(DEST::y, BC::z, vf2, vf0, vf5);        // vaddz.y vf2, vf0, vf5
  c->lqc2(vf1, 32, a1);                             // lqc2 vf1, 32(a1)
  c->divs_accurate(f4, f0, f1);                              // div.s f4, f0, f1
  c->lqc2(vf7, 0, t0);                              // lqc2 vf7, 0(t0)
  c->vsub_bc(DEST::z, BC::y, vf2, vf0, vf5);        // vsuby.z vf2, vf0, vf5
  c->lqc2(vf8, 16, t0);                             // lqc2 vf8, 16(t0)
  // sets vf2.w to 0
  c->vsub_bc(DEST::w, BC::w, vf2, vf0, vf0);        // vsubw.w vf2, vf0, vf0
  c->lqc2(vf9, 32, t0);                             // lqc2 vf9, 32(t0)
  c->vsub_bc(DEST::x, BC::z, vf3, vf0, vf5);        // vsubz.x vf3, vf0, vf5
  c->lqc2(vf10, 48, t0);                            // lqc2 vf10, 48(t0)
  c->vadd_bc(DEST::y, BC::w, vf3, vf0, vf5);        // vaddw.y vf3, vf0, vf5
  c->lwc1(f2, 68, t0);                              // lwc1 f2, 68(t0)
  c->vadd_bc(DEST::z, BC::x, vf3, vf0, vf5);        // vaddx.z vf3, vf0, vf5
  c->sqc2(vf1, 64, a2);                             // sqc2 vf1, 64(a2)
  c->vsub_bc(DEST::w, BC::w, vf3, vf0, vf0);        // vsubw.w vf3, vf0, vf0
  c->lwc1(f3, 72, t0);                              // lwc1 f3, 72(t0)
  c->vadd_bc(DEST::x, BC::y, vf4, vf0, vf5);        // vaddy.x vf4, vf0, vf5
  c->lw(v1, 76, t0);                                // lw v1, 76(t0)
  c->vsub_bc(DEST::y, BC::x, vf4, vf0, vf5);        // vsubx.y vf4, vf0, vf5
  c->mfc1(t1, f4);                                  // mfc1 t1, f4
  c->vadd_bc(DEST::z, BC::w, vf4, vf0, vf5);        // vaddw.z vf4, vf0, vf5
  c->divs_accurate(f4, f0, f2);                              // div.s f4, f0, f2
  c->vsub_bc(DEST::w, BC::w, vf4, vf0, vf0);        // vsubw.w vf4, vf0, vf0
  c->vopmula(vf6, vf2);                             // vopmula.xyz acc, vf6, vf2
  c->vopmsub(vf2, vf2, vf6);                        // vopmsub.xyz vf2, vf2, vf6
  c->vopmula(vf6, vf3);                             // vopmula.xyz acc, vf6, vf3
  c->vopmsub(vf3, vf3, vf6);                        // vopmsub.xyz vf3, vf3, vf6
  c->vopmula(vf6, vf4);                             // vopmula.xyz acc, vf6, vf4
  c->vopmsub(vf4, vf4, vf6);                        // vopmsub.xyz vf4, vf4, vf6
  c->vadd_bc(DEST::x, BC::w, vf2, vf2, vf0);        // vaddw.x vf2, vf2, vf0
  c->vadd_bc(DEST::y, BC::w, vf3, vf3, vf0);        // vaddw.y vf3, vf3, vf0
  c->vadd_bc(DEST::z, BC::w, vf4, vf4, vf0);        // vaddw.z vf4, vf4, vf0
  c->mfc1(t2, f4);                                  // mfc1 t2, f4
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L50
  c->divs_accurate(f4, f0, f3);                              // div.s f4, f0, f3
  if (bc) {goto block_2;}                           // branch non-likely

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
  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return


  block_2:
  c->pextlw(t1, t2, t1);                            // pextlw t1, t2, t1
  c->vmul_bc(DEST::xyzw, BC::x, vf2, vf2, vf1);     // vmulx.xyzw vf2, vf2, vf1
  c->vmul_bc(DEST::xyzw, BC::y, vf3, vf3, vf1);     // vmuly.xyzw vf3, vf3, vf1
  c->vmul_bc(DEST::xyzw, BC::z, vf4, vf4, vf1);     // vmulz.xyzw vf4, vf4, vf1
  // here, f4 is 1/scale. Sometimes the scale out of the joint compression code is slightly negative
  // this leads to mfc1 sign extending 1's into the upper 32 bits of t3 (this is weirdly how the ps2
  // does it).
  c->mfc1(t3, f4);                                  // mfc1 t3, f4
  // and this brings those ones into bits 96-128
  c->pcpyld(t1, t3, t1);                            // pcpyld t1, t3, t1
  // so here, vf16.w is usually 0, except for when the scale is negative, then it's 0xffff'ffff
  // (NaN on x86, -BIG on PS2)
  c->mov128_vf_gpr(vf16, t1);                       // qmtc2.i vf16, t1
  // here, vf2/3/4's w's are all 0. On PS2, this always keeps them as 0.
  // but on x86, this propagates NaNs: 0 * NaN = NaN.
  // so:
  c->vfs[vf16].vf.w() = 0; // PATCH to clear invalid float that will be multiplied by 0 below
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
  //jr ra                                           // jr ra
  // nop                                            // sll r0, r0, 0
  goto end_of_function;                             // return

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
  gLinkedFunctionTable.reg("cspace<-parented-transformq-joint!", execute, 128);
}

} // namespace cspace<_parented_transformq_joint
} // namespace Mips2C
// add cspace<_parented_transformq_joint::link to the link callback table for the object file.
// FWD DEC:


namespace normalize_frame_quaternions { extern void link(); }

namespace decompress_fixed_data_to_accumulator { extern void link(); }
namespace decompress_frame_data_to_accumulator { extern void link(); }


namespace decompress_frame_data_pair_to_accumulator { extern void link(); }
