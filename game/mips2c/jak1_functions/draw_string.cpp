//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak1/kscheme.h"
using namespace jak1;
namespace Mips2C::jak1 {
namespace draw_string {
struct Cache {
  void* font_work; // *font-work*
  void* font12_table; // *font12-table*
  void* font24_table; // *font24-table*
  void* math_camera; // *math-camera*
  void* video_parms; // *video-parms*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;  c->load_symbol(v1, cache.math_camera);            // lw v1, *math-camera*(s7)
  c->lqc2(vf26, 732, v1);                           // lqc2 vf26, 732(v1)
  c->lqc2(vf27, 732, v1);                           // lqc2 vf27, 732(v1)
  c->vadd_bc(DEST::xy, BC::w, vf26, vf26, vf0);     // vaddw.xy vf26, vf26, vf0
  c->vadd_bc(DEST::x, BC::w, vf26, vf26, vf0);      // vaddw.x vf26, vf26, vf0
  c->lw(v1, 72, a2);                                // lw v1, 72(a2)
  c->lqc2(vf25, 44, a2);                            // lqc2 vf25, 44(a2)
  c->lqc2(vf23, 12, a2);                            // lqc2 vf23, 12(a2)
  c->lqc2(vf24, 12, a2);                            // lqc2 vf24, 12(a2)
  c->lqc2(vf28, 0, v1);                             // lqc2 vf28, 0(v1)
  c->lqc2(vf29, 16, v1);                            // lqc2 vf29, 16(v1)
  c->lqc2(vf30, 32, v1);                            // lqc2 vf30, 32(v1)
  c->lqc2(vf31, 48, v1);                            // lqc2 vf31, 48(v1)
  c->load_symbol(v1, cache.video_parms);            // lw v1, *video-parms*(s7)
  c->mov64(v1, v1);                                 // or v1, v1, r0
  c->lqc2(vf1, 64, v1);                             // lqc2 vf1, 64(v1)
  c->vmul(DEST::xy, vf25, vf25, vf1);               // vmul.xy vf25, vf25, vf1
  c->vmul(DEST::xy, vf23, vf23, vf1);               // vmul.xy vf23, vf23, vf1
  c->vmul(DEST::xy, vf24, vf24, vf1);               // vmul.xy vf24, vf24, vf1
  c->load_symbol(v1, cache.font_work);              // lw v1, *font-work*(s7)
  c->mov64(v1, v1);                                 // or v1, v1, r0
  c->sw(a1, 3024, v1);                              // sw a1, 3024(v1)
  c->lw(a1, 4, a1);                                 // lw a1, 4(a1)
  c->sw(a0, 3028, v1);                              // sw a0, 3028(v1)
  c->lw(t2, 68, a2);                                // lw t2, 68(a2)
  c->vmove(DEST::xyzw, vf1, vf0);                   // vmove.xyzw vf1, vf0
  c->vmove(DEST::xyzw, vf2, vf0);                   // vmove.xyzw vf2, vf0
  c->vmove(DEST::xyzw, vf3, vf0);                   // vmove.xyzw vf3, vf0
  c->vmove(DEST::xyzw, vf4, vf0);                   // vmove.xyzw vf4, vf0
  c->sw(t2, 3032, v1);                              // sw t2, 3032(v1)
  c->lqc2(vf16, 240, v1);                           // lqc2 vf16, 240(v1)
  c->lqc2(vf17, 256, v1);                           // lqc2 vf17, 256(v1)
  c->lqc2(vf18, 272, v1);                           // lqc2 vf18, 272(v1)
  c->andi(a3, t2, 32);                              // andi a3, t2, 32
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L22
  c->load_symbol(a3, cache.font12_table);           // lw a3, *font12-table*(s7)
  if (bc) {goto block_2;}                           // branch non-likely

  c->mov64(a3, a3);                                 // or a3, a3, r0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf13, 144, v1);                           // lqc2 vf13, 144(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 160, v1);                           // lqc2 vf14, 160(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf15, 176, v1);                           // lqc2 vf15, 176(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t0, 80, v1);                                // lq t0, 80(v1)
  //beq r0, r0, L23                                 // beq r0, r0, L23
  c->lq(t1, 96, v1);                                // lq t1, 96(v1)
  goto block_3;                                     // branch always


  block_2:
  // nop                                            // sll r0, r0, 0
  c->load_symbol(a3, cache.font24_table);           // lw a3, *font24-table*(s7)
  c->mov64(a3, a3);                                 // or a3, a3, r0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf13, 192, v1);                           // lqc2 vf13, 192(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 208, v1);                           // lqc2 vf14, 208(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf15, 224, v1);                           // lqc2 vf15, 224(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t0, 112, v1);                               // lq t0, 112(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t1, 128, v1);                               // lq t1, 128(v1)

  block_3:
  c->lw(t3, 60, a2);                                // lw t3, 60(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(t3, 3008, v1);                              // sw t3, 3008(v1)
  // nop                                            // sll r0, r0, 0
  c->sll(t3, t3, 4);                                // sll t3, t3, 4
  // nop                                            // sll r0, r0, 0
  c->daddu(t4, t3, v1);                             // daddu t4, t3, v1
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lwu(t3, 1984, t4);                             // lwu t3, 1984(t4)
  // nop                                            // sll r0, r0, 0
  c->lwu(t5, 1988, t4);                             // lwu t5, 1988(t4)
  c->pextlb(t6, r0, t3);                            // pextlb t6, r0, t3
  c->lwu(t3, 1992, t4);                             // lwu t3, 1992(t4)
  c->pextlh(t6, r0, t6);                            // pextlh t6, r0, t6
  c->lwu(t4, 1996, t4);                             // lwu t4, 1996(t4)
  c->pextlb(t5, r0, t5);                            // pextlb t5, r0, t5
  c->sq(t6, 432, v1);                               // sq t6, 432(v1)
  c->pextlh(t5, r0, t5);                            // pextlh t5, r0, t5
  c->sq(t6, 816, v1);                               // sq t6, 816(v1)
  c->pextlb(t3, r0, t3);                            // pextlb t3, r0, t3
  c->sq(t5, 448, v1);                               // sq t5, 448(v1)
  c->pextlh(t3, r0, t3);                            // pextlh t3, r0, t3
  c->sq(t5, 832, v1);                               // sq t5, 832(v1)
  c->pextlb(t4, r0, t4);                            // pextlb t4, r0, t4
  c->sq(t3, 464, v1);                               // sq t3, 464(v1)
  c->pextlh(t4, r0, t4);                            // pextlh t4, r0, t4
  c->sq(t3, 848, v1);                               // sq t3, 848(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t4, 480, v1);                               // sq t4, 480(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t4, 864, v1);                               // sq t4, 864(v1)
  c->mov64(t3, v1);                                 // or t3, v1, r0
  // nop                                            // sll r0, r0, 0

  block_4:
  c->lbu(t4, 4, a0);                                // lbu t4, 4(a0)
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L47
  c->daddiu(t5, t4, -1);                            // daddiu t5, t4, -1
  if (bc) {goto block_67;}                          // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L39
  c->daddiu(t5, t4, -126);                          // daddiu t5, t4, -126
  if (bc) {goto block_54;}                          // branch non-likely

  bc = c->sgpr64(t5) != 0;                          // bne t5, r0, L40
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_55;}                          // branch non-likely

  c->lbu(t4, 4, a0);                                // lbu t4, 4(a0)
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  c->addiu(t5, r0, 0);                              // addiu t5, r0, 0
  c->addiu(t6, r0, 0);                              // addiu t6, r0, 0
  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L47
  c->daddiu(t7, t4, -43);                           // daddiu t7, t4, -43
  if (bc) {goto block_67;}                          // branch non-likely

  c->movz(t5, t4, t7);                              // movz t5, t4, t7
  c->daddiu(t7, t4, -45);                           // daddiu t7, t4, -45
  c->movz(t5, t4, t7);                              // movz t5, t4, t7
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t5) != 0;                          // bne t5, r0, L25
  c->daddiu(t7, t4, -121);                          // daddiu t7, t4, -121
  if (bc) {goto block_15;}                          // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L37
  c->daddiu(t6, t4, -89);                           // daddiu t6, t4, -89
  if (bc) {goto block_52;}                          // branch non-likely

  bc = c->sgpr64(t6) == 0;                          // beq t6, r0, L37
  c->daddiu(t6, t4, -122);                          // daddiu t6, t4, -122
  if (bc) {goto block_52;}                          // branch non-likely

  bc = c->sgpr64(t6) == 0;                          // beq t6, r0, L38
  c->daddiu(t6, t4, -90);                           // daddiu t6, t4, -90
  if (bc) {goto block_53;}                          // branch non-likely

  bc = c->sgpr64(t6) == 0;                          // beq t6, r0, L38
  c->daddiu(t6, t4, -48);                           // daddiu t6, t4, -48
  if (bc) {goto block_53;}                          // branch non-likely

  bc = ((s64)c->sgpr64(t6)) < 0;                    // bltz t6, L40
  c->daddiu(t6, t4, -57);                           // daddiu t6, t4, -57
  if (bc) {goto block_55;}                          // branch non-likely

  bc = ((s64)c->sgpr64(t6)) > 0;                    // bgtz t6, L40
  c->daddiu(t6, t4, -48);                           // daddiu t6, t4, -48
  if (bc) {goto block_55;}                          // branch non-likely


  block_15:
  c->lbu(t4, 4, a0);                                // lbu t4, 4(a0)
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L47
  c->daddiu(t7, t4, -110);                          // daddiu t7, t4, -110
  if (bc) {goto block_67;}                          // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L26
  c->daddiu(t7, t4, -78);                           // daddiu t7, t4, -78
  if (bc) {goto block_33;}                          // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L26
  c->daddiu(t7, t4, -108);                          // daddiu t7, t4, -108
  if (bc) {goto block_33;}                          // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L24
  c->daddiu(t7, t4, -76);                           // daddiu t7, t4, -76
  if (bc) {goto block_4;}                           // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L24
  c->daddiu(t7, t4, -119);                          // daddiu t7, t4, -119
  if (bc) {goto block_4;}                           // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L24
  c->daddiu(t7, t4, -87);                           // daddiu t7, t4, -87
  if (bc) {goto block_4;}                           // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L24
  c->daddiu(t7, t4, -107);                          // daddiu t7, t4, -107
  if (bc) {goto block_4;}                           // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L28
  c->daddiu(t7, t4, -75);                           // daddiu t7, t4, -75
  if (bc) {goto block_36;}                          // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L28
  c->daddiu(t7, t4, -106);                          // daddiu t7, t4, -106
  if (bc) {goto block_36;}                          // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L29
  c->daddiu(t7, t4, -74);                           // daddiu t7, t4, -74
  if (bc) {goto block_38;}                          // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L29
  c->daddiu(t7, t4, -104);                          // daddiu t7, t4, -104
  if (bc) {goto block_38;}                          // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L31
  c->daddiu(t7, t4, -72);                           // daddiu t7, t4, -72
  if (bc) {goto block_42;}                          // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L31
  c->daddiu(t7, t4, -118);                          // daddiu t7, t4, -118
  if (bc) {goto block_42;}                          // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L34
  c->daddiu(t7, t4, -86);                           // daddiu t7, t4, -86
  if (bc) {goto block_47;}                          // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L34
  c->daddiu(t7, t4, -48);                           // daddiu t7, t4, -48
  if (bc) {goto block_47;}                          // branch non-likely

  bc = ((s64)c->sgpr64(t7)) < 0;                    // bltz t7, L40
  c->daddiu(t8, t4, -57);                           // daddiu t8, t4, -57
  if (bc) {goto block_55;}                          // branch non-likely

  bc = ((s64)c->sgpr64(t8)) > 0;                    // bgtz t8, L40
  c->sll(t8, t6, 2);                                // sll t8, t6, 2
  if (bc) {goto block_55;}                          // branch non-likely

  c->daddu(t4, t6, t8);                             // daddu t4, t6, t8
  // nop                                            // sll r0, r0, 0
  c->sll(t4, t4, 1);                                // sll t4, t4, 1
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L25                                 // beq r0, r0, L25
  c->daddu(t6, t4, t7);                             // daddu t6, t4, t7
  goto block_15;                                    // branch always


  block_33:
  bc = c->sgpr64(t6) != 0;                          // bne t6, r0, L27
  c->addiu(t0, r0, -33);                            // addiu t0, r0, -33
  if (bc) {goto block_35;}                          // branch non-likely

  c->load_symbol(a3, cache.font12_table);           // lw a3, *font12-table*(s7)
  c->mov64(a3, a3);                                 // or a3, a3, r0
  c->lqc2(vf13, 144, v1);                           // lqc2 vf13, 144(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 160, v1);                           // lqc2 vf14, 160(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf15, 176, v1);                           // lqc2 vf15, 176(v1)
  c->and_(t2, t2, t0);                              // and t2, t2, t0
  c->lq(t0, 80, v1);                                // lq t0, 80(v1)
  //beq r0, r0, L24                                 // beq r0, r0, L24
  c->lq(t1, 96, v1);                                // lq t1, 96(v1)
  goto block_4;                                     // branch always


  block_35:
  // nop                                            // sll r0, r0, 0
  c->load_symbol(a3, cache.font24_table);           // lw a3, *font24-table*(s7)
  c->mov64(a3, a3);                                 // or a3, a3, r0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf13, 192, v1);                           // lqc2 vf13, 192(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 208, v1);                           // lqc2 vf14, 208(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf15, 224, v1);                           // lqc2 vf15, 224(v1)
  c->ori(t2, t2, 32);                               // ori t2, t2, 32
  c->lq(t0, 112, v1);                               // lq t0, 112(v1)
  //beq r0, r0, L24                                 // beq r0, r0, L24
  c->lq(t1, 128, v1);                               // lq t1, 128(v1)
  goto block_4;                                     // branch always


  block_36:
  c->addiu(t4, r0, -3);                             // addiu t4, r0, -3
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t6) == 0;                          // beq t6, r0, L24
  c->and_(t2, t2, t4);                              // and t2, t2, t4
  if (bc) {goto block_4;}                           // branch non-likely

  //beq r0, r0, L24                                 // beq r0, r0, L24
  c->ori(t2, t2, 2);                                // ori t2, t2, 2
  goto block_4;                                     // branch always


  block_38:
  c->addiu(t4, r0, -21);                            // addiu t4, r0, -21
  c->daddiu(t5, t6, -2);                            // daddiu t5, t6, -2
  bc = c->sgpr64(t6) == 0;                          // beq t6, r0, L24
  c->and_(t2, t2, t4);                              // and t2, t2, t4
  if (bc) {goto block_4;}                           // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L30
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_41;}                          // branch non-likely

  //beq r0, r0, L24                                 // beq r0, r0, L24
  c->ori(t2, t2, 16);                               // ori t2, t2, 16
  goto block_4;                                     // branch always


  block_41:
  //beq r0, r0, L24                                 // beq r0, r0, L24
  c->ori(t2, t2, 4);                                // ori t2, t2, 4
  goto block_4;                                     // branch always


  block_42:
  c->mov128_vf_gpr(vf1, t6);                        // qmtc2.i vf1, t6
  c->daddiu(t4, t5, -45);                           // daddiu t4, t5, -45
  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L33
  c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
  if (bc) {goto block_46;}                          // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L32
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_45;}                          // branch non-likely

  //beq r0, r0, L24                                 // beq r0, r0, L24
  c->vadd_bc(DEST::x, BC::x, vf23, vf23, vf1);      // vaddx.x vf23, vf23, vf1
  goto block_4;                                     // branch always


  block_45:
  //beq r0, r0, L24                                 // beq r0, r0, L24
  c->vsub_bc(DEST::x, BC::x, vf23, vf23, vf1);      // vsubx.x vf23, vf23, vf1
  goto block_4;                                     // branch always


  block_46:
  //beq r0, r0, L24                                 // beq r0, r0, L24
  c->vadd_bc(DEST::x, BC::x, vf23, vf0, vf1);       // vaddx.x vf23, vf0, vf1
  goto block_4;                                     // branch always


  block_47:
  c->mov128_vf_gpr(vf1, t6);                        // qmtc2.i vf1, t6
  c->daddiu(t4, t5, -45);                           // daddiu t4, t5, -45
  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L36
  c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
  if (bc) {goto block_51;}                          // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L35
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_50;}                          // branch non-likely

  //beq r0, r0, L24                                 // beq r0, r0, L24
  c->vadd_bc(DEST::y, BC::x, vf23, vf23, vf1);      // vaddx.y vf23, vf23, vf1
  goto block_4;                                     // branch always


  block_50:
  //beq r0, r0, L24                                 // beq r0, r0, L24
  c->vsub_bc(DEST::y, BC::x, vf23, vf23, vf1);      // vsubx.y vf23, vf23, vf1
  goto block_4;                                     // branch always


  block_51:
  //beq r0, r0, L24                                 // beq r0, r0, L24
  c->vadd_bc(DEST::y, BC::x, vf23, vf0, vf1);       // vaddx.y vf23, vf0, vf1
  goto block_4;                                     // branch always


  block_52:
  //beq r0, r0, L24                                 // beq r0, r0, L24
  c->sqc2(vf23, 288, v1);                           // sqc2 vf23, 288(v1)
  goto block_4;                                     // branch always


  block_53:
  //beq r0, r0, L24                                 // beq r0, r0, L24
  c->lqc2(vf23, 288, v1);                           // lqc2 vf23, 288(v1)
  goto block_4;                                     // branch always


  block_54:
  c->lbu(t4, 4, a0);                                // lbu t4, 4(a0)
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
  c->vadd(DEST::xyz, vf4, vf23, vf15);              // vadd.xyz vf4, vf23, vf15
  c->andi(t4, t4, 127);                             // andi t4, t4, 127
  c->vsub(DEST::xyzw, vf1, vf25, vf23);             // vsub.xyzw vf1, vf25, vf23
  c->daddiu(t4, t4, 255);                           // daddiu t4, t4, 255
  //beq r0, r0, L44                                 // beq r0, r0, L44
  c->sll(t5, t4, 4);                                // sll t5, t4, 4
  goto block_62;                                    // branch always


  block_55:
  c->vadd(DEST::xyz, vf4, vf23, vf15);              // vadd.xyz vf4, vf23, vf15
  c->sll(t5, t4, 4);                                // sll t5, t4, 4
  c->vsub(DEST::xyzw, vf1, vf25, vf23);             // vsub.xyzw vf1, vf25, vf23
  c->daddiu(t6, t4, -10);                           // daddiu t6, t4, -10
  bc = c->sgpr64(t6) == 0;                          // beq t6, r0, L41
  c->daddiu(t4, t4, -13);                           // daddiu t4, t4, -13
  if (bc) {goto block_57;}                          // branch non-likely

  bc = c->sgpr64(t4) != 0;                          // bne t4, r0, L44
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_62;}                          // branch non-likely


  block_57:
  c->vsub(DEST::xyzw, vf1, vf23, vf24);             // vsub.xyzw vf1, vf23, vf24
  c->andi(t4, t2, 16);                              // andi t4, t2, 16
  bc = c->sgpr64(t4) != 0;                          // bne t4, r0, L42
  c->andi(t4, t2, 4);                               // andi t4, t2, 4
  if (bc) {goto block_60;}                          // branch non-likely

  bc = c->sgpr64(t4) != 0;                          // bne t4, r0, L43
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_61;}                          // branch non-likely

  c->vadd_bc(DEST::x, BC::x, vf23, vf0, vf24);      // vaddx.x vf23, vf0, vf24
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf23, 944, t3);                           // sqc2 vf23, 944(t3)
  c->vadd_bc(DEST::y, BC::w, vf23, vf23, vf15);     // vaddw.y vf23, vf23, vf15
  //beq r0, r0, L24                                 // beq r0, r0, L24
  c->daddiu(t3, t3, 16);                            // daddiu t3, t3, 16
  goto block_4;                                     // branch always


  block_60:
  c->vsub(DEST::x, vf23, vf24, vf1);                // vsub.x vf23, vf24, vf1
  c->sqc2(vf23, 944, t3);                           // sqc2 vf23, 944(t3)
  c->vadd_bc(DEST::x, BC::x, vf23, vf0, vf24);      // vaddx.x vf23, vf0, vf24
  c->vadd_bc(DEST::y, BC::w, vf23, vf23, vf15);     // vaddw.y vf23, vf23, vf15
  //beq r0, r0, L24                                 // beq r0, r0, L24
  c->daddiu(t3, t3, 16);                            // daddiu t3, t3, 16
  goto block_4;                                     // branch always


  block_61:
  c->vmul_bc(DEST::x, BC::w, vf1, vf1, vf16);       // vmulw.x vf1, vf1, vf16
  // nop                                            // sll r0, r0, 0
  c->vsub(DEST::x, vf23, vf24, vf1);                // vsub.x vf23, vf24, vf1
  c->sqc2(vf23, 944, t3);                           // sqc2 vf23, 944(t3)
  c->vadd_bc(DEST::x, BC::x, vf23, vf0, vf24);      // vaddx.x vf23, vf0, vf24
  c->vadd_bc(DEST::y, BC::w, vf23, vf23, vf15);     // vaddw.y vf23, vf23, vf15
  //beq r0, r0, L24                                 // beq r0, r0, L24
  c->daddiu(t3, t3, 16);                            // daddiu t3, t3, 16
  goto block_4;                                     // branch always


  block_62:
  c->addu(t4, t5, a3);                              // addu t4, t5, a3
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf5, -256, t4);                           // lqc2 vf5, -256(t4)
  c->mov128_gpr_vf(t4, vf1);                        // qmfc2.i t4, vf1
  bc = ((s64)c->sgpr64(t4)) < 0;                    // bltz t4, L47
  c->sra(t4, t4, 31);                               // sra t4, t4, 31
  if (bc) {goto block_67;}                          // branch non-likely

  c->vmul(DEST::xyzw, vf19, vf5, vf13);             // vmul.xyzw vf19, vf5, vf13
  c->andi(t4, t2, 2);                               // andi t4, t2, 2
  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L45
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_65;}                          // branch non-likely

  //beq r0, r0, L46                                 // beq r0, r0, L46
  c->vadd_bc(DEST::x, BC::w, vf23, vf23, vf19);     // vaddw.x vf23, vf23, vf19
  goto block_66;                                    // branch always


  block_65:
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::w, vf23, vf23, vf14);     // vaddw.x vf23, vf23, vf14

  block_66:
  //beq r0, r0, L24                                 // beq r0, r0, L24
  // nop                                            // sll r0, r0, 0
  goto block_4;                                     // branch always


  block_67:
  c->vsub(DEST::xyzw, vf1, vf23, vf24);             // vsub.xyzw vf1, vf23, vf24
  c->andi(a0, t2, 16);                              // andi a0, t2, 16
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L48
  c->andi(a0, t2, 4);                               // andi a0, t2, 4
  if (bc) {goto block_70;}                          // branch non-likely

  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L49
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_71;}                          // branch non-likely

  c->vadd_bc(DEST::x, BC::x, vf23, vf0, vf24);      // vaddx.x vf23, vf0, vf24
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L50                                 // beq r0, r0, L50
  c->sqc2(vf23, 944, t3);                           // sqc2 vf23, 944(t3)
  goto block_72;                                    // branch always


  block_70:
  c->vsub(DEST::x, vf23, vf24, vf1);                // vsub.x vf23, vf24, vf1
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L50                                 // beq r0, r0, L50
  c->sqc2(vf23, 944, t3);                           // sqc2 vf23, 944(t3)
  goto block_72;                                    // branch always


  block_71:
  c->vmul_bc(DEST::x, BC::w, vf1, vf1, vf16);       // vmulw.x vf1, vf1, vf16
  // nop                                            // sll r0, r0, 0
  c->vsub(DEST::x, vf23, vf24, vf1);                // vsub.x vf23, vf24, vf1
  c->sqc2(vf23, 944, t3);                           // sqc2 vf23, 944(t3)

  block_72:
  c->lw(a0, 3032, v1);                              // lw a0, 3032(v1)
  c->mov64(t2, v1);                                 // or t2, v1, r0
  c->lw(t3, 3028, v1);                              // lw t3, 3028(v1)
  c->lqc2(vf23, 944, t2);                           // lqc2 vf23, 944(t2)

  block_73:
  c->lbu(t4, 4, t3);                                // lbu t4, 4(t3)
  c->daddiu(t3, t3, 1);                             // daddiu t3, t3, 1
  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L71
  c->daddiu(t5, t4, -1);                            // daddiu t5, t4, -1
  if (bc) {goto block_131;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L65
  c->daddiu(t5, t4, -126);                          // daddiu t5, t4, -126
  if (bc) {goto block_121;}                         // branch non-likely

  bc = c->sgpr64(t5) != 0;                          // bne t5, r0, L66
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_122;}                         // branch non-likely

  c->lbu(t4, 4, t3);                                // lbu t4, 4(t3)
  c->daddiu(t3, t3, 1);                             // daddiu t3, t3, 1
  c->addiu(t5, r0, 0);                              // addiu t5, r0, 0
  c->addiu(t6, r0, 0);                              // addiu t6, r0, 0
  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L71
  c->daddiu(t7, t4, -43);                           // daddiu t7, t4, -43
  if (bc) {goto block_131;}                         // branch non-likely

  c->movz(t5, t4, t7);                              // movz t5, t4, t7
  c->daddiu(t7, t4, -45);                           // daddiu t7, t4, -45
  c->movz(t5, t4, t7);                              // movz t5, t4, t7
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t5) != 0;                          // bne t5, r0, L52
  c->daddiu(t7, t4, -121);                          // daddiu t7, t4, -121
  if (bc) {goto block_84;}                          // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L63
  c->daddiu(t6, t4, -89);                           // daddiu t6, t4, -89
  if (bc) {goto block_119;}                         // branch non-likely

  bc = c->sgpr64(t6) == 0;                          // beq t6, r0, L63
  c->daddiu(t6, t4, -122);                          // daddiu t6, t4, -122
  if (bc) {goto block_119;}                         // branch non-likely

  bc = c->sgpr64(t6) == 0;                          // beq t6, r0, L64
  c->daddiu(t6, t4, -90);                           // daddiu t6, t4, -90
  if (bc) {goto block_120;}                         // branch non-likely

  bc = c->sgpr64(t6) == 0;                          // beq t6, r0, L64
  c->daddiu(t6, t4, -48);                           // daddiu t6, t4, -48
  if (bc) {goto block_120;}                         // branch non-likely

  bc = ((s64)c->sgpr64(t6)) < 0;                    // bltz t6, L66
  c->daddiu(t6, t4, -57);                           // daddiu t6, t4, -57
  if (bc) {goto block_122;}                         // branch non-likely

  bc = ((s64)c->sgpr64(t6)) > 0;                    // bgtz t6, L66
  c->daddiu(t6, t4, -48);                           // daddiu t6, t4, -48
  if (bc) {goto block_122;}                         // branch non-likely


  block_84:
  c->lbu(t4, 4, t3);                                // lbu t4, 4(t3)
  c->daddiu(t3, t3, 1);                             // daddiu t3, t3, 1
  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L71
  c->daddiu(t7, t4, -110);                          // daddiu t7, t4, -110
  if (bc) {goto block_131;}                         // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L53
  c->daddiu(t7, t4, -78);                           // daddiu t7, t4, -78
  if (bc) {goto block_102;}                         // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L53
  c->daddiu(t7, t4, -108);                          // daddiu t7, t4, -108
  if (bc) {goto block_102;}                         // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L51
  c->daddiu(t7, t4, -76);                           // daddiu t7, t4, -76
  if (bc) {goto block_73;}                          // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L51
  c->daddiu(t7, t4, -119);                          // daddiu t7, t4, -119
  if (bc) {goto block_73;}                          // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L55
  c->daddiu(t7, t4, -87);                           // daddiu t7, t4, -87
  if (bc) {goto block_105;}                         // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L55
  c->daddiu(t7, t4, -107);                          // daddiu t7, t4, -107
  if (bc) {goto block_105;}                         // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L56
  c->daddiu(t7, t4, -75);                           // daddiu t7, t4, -75
  if (bc) {goto block_107;}                         // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L56
  c->daddiu(t7, t4, -106);                          // daddiu t7, t4, -106
  if (bc) {goto block_107;}                         // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L51
  c->daddiu(t7, t4, -74);                           // daddiu t7, t4, -74
  if (bc) {goto block_73;}                          // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L51
  c->daddiu(t7, t4, -104);                          // daddiu t7, t4, -104
  if (bc) {goto block_73;}                          // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L57
  c->daddiu(t7, t4, -72);                           // daddiu t7, t4, -72
  if (bc) {goto block_109;}                         // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L57
  c->daddiu(t7, t4, -118);                          // daddiu t7, t4, -118
  if (bc) {goto block_109;}                         // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L60
  c->daddiu(t7, t4, -86);                           // daddiu t7, t4, -86
  if (bc) {goto block_114;}                         // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L60
  c->daddiu(t7, t4, -48);                           // daddiu t7, t4, -48
  if (bc) {goto block_114;}                         // branch non-likely

  bc = ((s64)c->sgpr64(t7)) < 0;                    // bltz t7, L66
  c->daddiu(t8, t4, -57);                           // daddiu t8, t4, -57
  if (bc) {goto block_122;}                         // branch non-likely

  bc = ((s64)c->sgpr64(t8)) > 0;                    // bgtz t8, L66
  c->sll(t8, t6, 2);                                // sll t8, t6, 2
  if (bc) {goto block_122;}                         // branch non-likely

  c->daddu(t4, t6, t8);                             // daddu t4, t6, t8
  // nop                                            // sll r0, r0, 0
  c->sll(t4, t4, 1);                                // sll t4, t4, 1
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L52                                 // beq r0, r0, L52
  c->daddu(t6, t4, t7);                             // daddu t6, t4, t7
  goto block_84;                                    // branch always


  block_102:
  bc = c->sgpr64(t6) != 0;                          // bne t6, r0, L54
  c->load_symbol(a3, cache.font12_table);           // lw a3, *font12-table*(s7)
  if (bc) {goto block_104;}                         // branch non-likely

  c->mov64(a3, a3);                                 // or a3, a3, r0
  c->addiu(t0, r0, -33);                            // addiu t0, r0, -33
  c->lqc2(vf13, 144, v1);                           // lqc2 vf13, 144(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 160, v1);                           // lqc2 vf14, 160(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf15, 176, v1);                           // lqc2 vf15, 176(v1)
  c->and_(a0, a0, t0);                              // and a0, a0, t0
  c->lq(t0, 80, v1);                                // lq t0, 80(v1)
  //beq r0, r0, L51                                 // beq r0, r0, L51
  c->lq(t1, 96, v1);                                // lq t1, 96(v1)
  goto block_73;                                    // branch always


  block_104:
  // nop                                            // sll r0, r0, 0
  c->load_symbol(a3, cache.font24_table);           // lw a3, *font24-table*(s7)
  c->mov64(a3, a3);                                 // or a3, a3, r0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf13, 192, v1);                           // lqc2 vf13, 192(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 208, v1);                           // lqc2 vf14, 208(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf15, 224, v1);                           // lqc2 vf15, 224(v1)
  c->ori(a0, a0, 32);                               // ori a0, a0, 32
  c->lq(t0, 112, v1);                               // lq t0, 112(v1)
  //beq r0, r0, L51                                 // beq r0, r0, L51
  c->lq(t1, 128, v1);                               // lq t1, 128(v1)
  goto block_73;                                    // branch always


  block_105:
  c->addiu(t4, r0, -2);                             // addiu t4, r0, -2
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t6) == 0;                          // beq t6, r0, L51
  c->and_(a0, a0, t4);                              // and a0, a0, t4
  if (bc) {goto block_73;}                          // branch non-likely

  //beq r0, r0, L51                                 // beq r0, r0, L51
  c->ori(a0, a0, 1);                                // ori a0, a0, 1
  goto block_73;                                    // branch always


  block_107:
  c->addiu(t4, r0, -3);                             // addiu t4, r0, -3
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t6) == 0;                          // beq t6, r0, L51
  c->and_(a0, a0, t4);                              // and a0, a0, t4
  if (bc) {goto block_73;}                          // branch non-likely

  //beq r0, r0, L51                                 // beq r0, r0, L51
  c->ori(a0, a0, 2);                                // ori a0, a0, 2
  goto block_73;                                    // branch always


  block_109:
  c->mov128_vf_gpr(vf1, t6);                        // qmtc2.i vf1, t6
  c->daddiu(t4, t5, -45);                           // daddiu t4, t5, -45
  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L59
  c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
  if (bc) {goto block_113;}                         // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L58
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_112;}                         // branch non-likely

  //beq r0, r0, L51                                 // beq r0, r0, L51
  c->vadd_bc(DEST::x, BC::x, vf23, vf23, vf1);      // vaddx.x vf23, vf23, vf1
  goto block_73;                                    // branch always


  block_112:
  //beq r0, r0, L51                                 // beq r0, r0, L51
  c->vsub_bc(DEST::x, BC::x, vf23, vf23, vf1);      // vsubx.x vf23, vf23, vf1
  goto block_73;                                    // branch always


  block_113:
  //beq r0, r0, L51                                 // beq r0, r0, L51
  c->vadd_bc(DEST::x, BC::x, vf23, vf0, vf1);       // vaddx.x vf23, vf0, vf1
  goto block_73;                                    // branch always


  block_114:
  c->mov128_vf_gpr(vf1, t6);                        // qmtc2.i vf1, t6
  c->daddiu(t4, t5, -45);                           // daddiu t4, t5, -45
  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L62
  c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
  if (bc) {goto block_118;}                         // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L61
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_117;}                         // branch non-likely

  //beq r0, r0, L51                                 // beq r0, r0, L51
  c->vadd_bc(DEST::y, BC::x, vf23, vf23, vf1);      // vaddx.y vf23, vf23, vf1
  goto block_73;                                    // branch always


  block_117:
  //beq r0, r0, L51                                 // beq r0, r0, L51
  c->vsub_bc(DEST::y, BC::x, vf23, vf23, vf1);      // vsubx.y vf23, vf23, vf1
  goto block_73;                                    // branch always


  block_118:
  //beq r0, r0, L51                                 // beq r0, r0, L51
  c->vadd_bc(DEST::y, BC::x, vf23, vf0, vf1);       // vaddx.y vf23, vf0, vf1
  goto block_73;                                    // branch always


  block_119:
  //beq r0, r0, L51                                 // beq r0, r0, L51
  c->sqc2(vf23, 288, v1);                           // sqc2 vf23, 288(v1)
  goto block_73;                                    // branch always


  block_120:
  //beq r0, r0, L51                                 // beq r0, r0, L51
  c->lqc2(vf23, 288, v1);                           // lqc2 vf23, 288(v1)
  goto block_73;                                    // branch always


  block_121:
  c->lbu(t4, 4, t3);                                // lbu t4, 4(t3)
  c->daddiu(t3, t3, 1);                             // daddiu t3, t3, 1
  c->vadd(DEST::xyz, vf4, vf23, vf15);              // vadd.xyz vf4, vf23, vf15
  c->andi(t5, t4, 127);                             // andi t5, t4, 127
  c->vsub(DEST::xyzw, vf1, vf25, vf23);             // vsub.xyzw vf1, vf25, vf23
  c->daddiu(t5, t5, 255);                           // daddiu t5, t5, 255
  //beq r0, r0, L68                                 // beq r0, r0, L68
  c->sll(t5, t5, 4);                                // sll t5, t5, 4
  goto block_125;                                   // branch always


  block_122:
  c->vadd(DEST::xyz, vf4, vf23, vf15);              // vadd.xyz vf4, vf23, vf15
  c->sll(t5, t4, 4);                                // sll t5, t4, 4
  c->vsub(DEST::xyzw, vf1, vf25, vf23);             // vsub.xyzw vf1, vf25, vf23
  c->daddiu(t6, t4, -10);                           // daddiu t6, t4, -10
  bc = c->sgpr64(t6) == 0;                          // beq t6, r0, L67
  c->daddiu(t6, t4, -13);                           // daddiu t6, t4, -13
  if (bc) {goto block_124;}                         // branch non-likely

  bc = c->sgpr64(t6) != 0;                          // bne t6, r0, L68
  c->andi(t6, a0, 1);                               // andi t6, a0, 1
  if (bc) {goto block_125;}                         // branch non-likely


  block_124:
  c->daddiu(t2, t2, 16);                            // daddiu t2, t2, 16
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L51                                 // beq r0, r0, L51
  c->lqc2(vf23, 944, t2);                           // lqc2 vf23, 944(t2)
  goto block_73;                                    // branch always


  block_125:
  c->addu(t5, t5, a3);                              // addu t5, t5, a3
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf5, -256, t5);                           // lqc2 vf5, -256(t5)
  c->mov128_gpr_vf(t5, vf1);                        // qmfc2.i t5, vf1
  bc = ((s64)c->sgpr64(t5)) < 0;                    // bltz t5, L71
  c->vadd(DEST::xyz, vf6, vf5, vf16);               // vadd.xyz vf6, vf5, vf16
  if (bc) {goto block_131;}                         // branch non-likely

  c->sra(t5, t5, 31);                               // sra t5, t5, 31
  c->vadd(DEST::xyz, vf7, vf5, vf17);               // vadd.xyz vf7, vf5, vf17
  bc = ((s64)c->sgpr64(t5)) < 0;                    // bltz t5, L51
  c->vadd(DEST::xyz, vf8, vf5, vf18);               // vadd.xyz vf8, vf5, vf18
  if (bc) {goto block_73;}                          // branch non-likely

  c->vadd(DEST::xyz, vf1, vf23, vf0);               // vadd.xyz vf1, vf23, vf0
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyz, vf2, vf23, vf13);              // vadd.xyz vf2, vf23, vf13
  c->sqc2(vf5, 496, v1);                            // sqc2 vf5, 496(v1)
  c->vadd(DEST::xyz, vf3, vf23, vf14);              // vadd.xyz vf3, vf23, vf14
  c->sqc2(vf6, 512, v1);                            // sqc2 vf6, 512(v1)
  c->vmul(DEST::xyzw, vf19, vf5, vf13);             // vmul.xyzw vf19, vf5, vf13
  c->sqc2(vf7, 528, v1);                            // sqc2 vf7, 528(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, 544, v1);                            // sqc2 vf8, 544(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf1, 368, v1);                            // sqc2 vf1, 368(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf2, 384, v1);                            // sqc2 vf2, 384(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf3, 400, v1);                            // sqc2 vf3, 400(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf4, 416, v1);                            // sqc2 vf4, 416(v1)
  c->lqc2(vf1, 368, v1);                            // lqc2 vf1, 368(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 384, v1);                            // lqc2 vf2, 384(v1)
  c->vmula_bc(DEST::xyzw, BC::w, vf31, vf0);        // vmulaw.xyzw acc, vf31, vf0
  c->lqc2(vf3, 400, v1);                            // lqc2 vf3, 400(v1)
  c->vmadda_bc(DEST::xyzw, BC::x, vf28, vf1);       // vmaddax.xyzw acc, vf28, vf1
  c->lqc2(vf4, 416, v1);                            // lqc2 vf4, 416(v1)
  c->vmadda_bc(DEST::xyzw, BC::y, vf29, vf1);       // vmadday.xyzw acc, vf29, vf1
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf1, vf30, vf1);   // vmaddz.xyzw vf1, vf30, vf1
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf31, vf0);        // vmulaw.xyzw acc, vf31, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf28, vf2);       // vmaddax.xyzw acc, vf28, vf2
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf29, vf2);       // vmadday.xyzw acc, vf29, vf2
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf2, vf30, vf2);   // vmaddz.xyzw vf2, vf30, vf2
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf31, vf0);        // vmulaw.xyzw acc, vf31, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf28, vf3);       // vmaddax.xyzw acc, vf28, vf3
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf29, vf3);       // vmadday.xyzw acc, vf29, vf3
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf3, vf30, vf3);   // vmaddz.xyzw vf3, vf30, vf3
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf31, vf0);        // vmulaw.xyzw acc, vf31, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf28, vf4);       // vmaddax.xyzw acc, vf28, vf4
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf29, vf4);       // vmadday.xyzw acc, vf29, vf4
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf4, vf30, vf4);   // vmaddz.xyzw vf4, vf30, vf4
  c->vdiv(vf25, BC::z, vf1, BC::w);                 // vdiv Q, vf25.z, vf1.w
  c->lq(t5, 32, v1);                                // lq t5, 32(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t6, 48, v1);                                // lq t6, 48(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t5, 0, a1);                                 // sq t5, 0(a1)
  // nop                                            // sll r0, r0, 0
  c->sq(t6, 16, a1);                                // sq t6, 16(a1)
  c->lqc2(vf5, 496, v1);                            // lqc2 vf5, 496(v1)
  c->mov128_gpr_gpr(t5, t0);                        // por t5, t0, r0
  c->lqc2(vf6, 512, v1);                            // lqc2 vf6, 512(v1)
  c->andi(t4, t4, 128);                             // andi t4, t4, 128
  c->lqc2(vf7, 528, v1);                            // lqc2 vf7, 528(v1)
  c->movn(t5, t1, t4);                              // movn t5, t1, t4
  c->vmulq(DEST::xyz, vf1, vf1);                    // vmulq.xyz vf1, vf1, Q
  c->sq(t5, 32, a1);                                // sq t5, 32(a1)
  c->vmulq(DEST::xyz, vf5, vf5);                    // vmulq.xyz vf5, vf5, Q
  // nop                                            // sll r0, r0, 0
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  c->vdiv(vf25, BC::z, vf2, BC::w);                 // vdiv Q, vf25.z, vf2.w
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf8, 544, v1);                            // lqc2 vf8, 544(v1)
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyzw, vf1, vf1, vf26);              // vadd.xyzw vf1, vf1, vf26
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf9, 1968, v1);                           // lqc2 vf9, 1968(v1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->vftoi4(DEST::xyzw, vf1, vf1);                  // vftoi4.xyzw vf1, vf1
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf2, vf2);                    // vmulq.xyz vf2, vf2, Q
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf6, vf6);                    // vmulq.xyz vf6, vf6, Q
  c->sqc2(vf5, 48, a1);                             // sqc2 vf5, 48(a1)
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  c->vdiv(vf25, BC::z, vf3, BC::w);                 // vdiv Q, vf25.z, vf3.w
  c->sqc2(vf9, 64, a1);                             // sqc2 vf9, 64(a1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf1, 80, a1);                             // sqc2 vf1, 80(a1)
  c->vadd(DEST::xyzw, vf2, vf2, vf26);              // vadd.xyzw vf2, vf2, vf26
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->vftoi4(DEST::xyzw, vf2, vf2);                  // vftoi4.xyzw vf2, vf2
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf3, vf3);                    // vmulq.xyz vf3, vf3, Q
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf7, vf7);                    // vmulq.xyz vf7, vf7, Q
  c->sqc2(vf6, 96, a1);                             // sqc2 vf6, 96(a1)
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  c->vdiv(vf25, BC::z, vf4, BC::w);                 // vdiv Q, vf25.z, vf4.w
  c->sqc2(vf9, 112, a1);                            // sqc2 vf9, 112(a1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf2, 128, a1);                            // sqc2 vf2, 128(a1)
  c->vadd(DEST::xyzw, vf3, vf3, vf26);              // vadd.xyzw vf3, vf3, vf26
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->vftoi4(DEST::xyzw, vf3, vf3);                  // vftoi4.xyzw vf3, vf3
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf4, vf4);                    // vmulq.xyz vf4, vf4, Q
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf8, vf8);                    // vmulq.xyz vf8, vf8, Q
  c->sqc2(vf7, 144, a1);                            // sqc2 vf7, 144(a1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf9, 160, a1);                            // sqc2 vf9, 160(a1)
  c->vadd(DEST::xyzw, vf4, vf4, vf26);              // vadd.xyzw vf4, vf4, vf26
  c->sqc2(vf3, 176, a1);                            // sqc2 vf3, 176(a1)
  c->andi(t4, a0, 2);                               // andi t4, a0, 2
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L69
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_129;}                         // branch non-likely

  //beq r0, r0, L70                                 // beq r0, r0, L70
  c->vadd_bc(DEST::x, BC::w, vf23, vf23, vf19);     // vaddw.x vf23, vf23, vf19
  goto block_130;                                   // branch always


  block_129:
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::w, vf23, vf23, vf14);     // vaddw.x vf23, vf23, vf14

  block_130:
  c->vftoi4(DEST::xyzw, vf4, vf4);                  // vftoi4.xyzw vf4, vf4
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, 192, a1);                            // sqc2 vf8, 192(a1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf9, 208, a1);                            // sqc2 vf9, 208(a1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf4, 224, a1);                            // sqc2 vf4, 224(a1)
  //beq r0, r0, L51                                 // beq r0, r0, L51
  c->daddiu(a1, a1, 240);                           // daddiu a1, a1, 240
  goto block_73;                                    // branch always


  block_131:
  c->lw(a0, 3032, v1);                              // lw a0, 3032(v1)
  c->mov64(t2, v1);                                 // or t2, v1, r0
  c->lw(t3, 3028, v1);                              // lw t3, 3028(v1)
  c->lqc2(vf23, 944, t2);                           // lqc2 vf23, 944(t2)

  block_132:
  c->lbu(t4, 4, t3);                                // lbu t4, 4(t3)
  c->daddiu(t3, t3, 1);                             // daddiu t3, t3, 1
  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L92
  c->daddiu(t5, t4, -1);                            // daddiu t5, t4, -1
  if (bc) {goto block_189;}                         // branch non-likely

  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L86
  c->daddiu(t5, t4, -126);                          // daddiu t5, t4, -126
  if (bc) {goto block_179;}                         // branch non-likely

  bc = c->sgpr64(t5) != 0;                          // bne t5, r0, L87
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_180;}                         // branch non-likely

  c->lbu(t4, 4, t3);                                // lbu t4, 4(t3)
  c->daddiu(t3, t3, 1);                             // daddiu t3, t3, 1
  c->addiu(t5, r0, 0);                              // addiu t5, r0, 0
  c->addiu(t6, r0, 0);                              // addiu t6, r0, 0
  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L92
  c->daddiu(t7, t4, -43);                           // daddiu t7, t4, -43
  if (bc) {goto block_189;}                         // branch non-likely

  c->movz(t5, t4, t7);                              // movz t5, t4, t7
  c->daddiu(t7, t4, -45);                           // daddiu t7, t4, -45
  c->movz(t5, t4, t7);                              // movz t5, t4, t7
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t5) != 0;                          // bne t5, r0, L73
  c->daddiu(t7, t4, -121);                          // daddiu t7, t4, -121
  if (bc) {goto block_143;}                         // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L84
  c->daddiu(t6, t4, -89);                           // daddiu t6, t4, -89
  if (bc) {goto block_177;}                         // branch non-likely

  bc = c->sgpr64(t6) == 0;                          // beq t6, r0, L84
  c->daddiu(t6, t4, -122);                          // daddiu t6, t4, -122
  if (bc) {goto block_177;}                         // branch non-likely

  bc = c->sgpr64(t6) == 0;                          // beq t6, r0, L85
  c->daddiu(t6, t4, -90);                           // daddiu t6, t4, -90
  if (bc) {goto block_178;}                         // branch non-likely

  bc = c->sgpr64(t6) == 0;                          // beq t6, r0, L85
  c->daddiu(t6, t4, -48);                           // daddiu t6, t4, -48
  if (bc) {goto block_178;}                         // branch non-likely

  bc = ((s64)c->sgpr64(t6)) < 0;                    // bltz t6, L87
  c->daddiu(t6, t4, -57);                           // daddiu t6, t4, -57
  if (bc) {goto block_180;}                         // branch non-likely

  bc = ((s64)c->sgpr64(t6)) > 0;                    // bgtz t6, L87
  c->daddiu(t6, t4, -48);                           // daddiu t6, t4, -48
  if (bc) {goto block_180;}                         // branch non-likely


  block_143:
  c->lbu(t4, 4, t3);                                // lbu t4, 4(t3)
  c->daddiu(t3, t3, 1);                             // daddiu t3, t3, 1
  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L92
  c->daddiu(t7, t4, -110);                          // daddiu t7, t4, -110
  if (bc) {goto block_189;}                         // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L74
  c->daddiu(t7, t4, -78);                           // daddiu t7, t4, -78
  if (bc) {goto block_161;}                         // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L74
  c->daddiu(t7, t4, -108);                          // daddiu t7, t4, -108
  if (bc) {goto block_161;}                         // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L76
  c->daddiu(t7, t4, -76);                           // daddiu t7, t4, -76
  if (bc) {goto block_164;}                         // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L76
  c->daddiu(t7, t4, -119);                          // daddiu t7, t4, -119
  if (bc) {goto block_164;}                         // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L72
  c->daddiu(t7, t4, -87);                           // daddiu t7, t4, -87
  if (bc) {goto block_132;}                         // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L72
  c->daddiu(t7, t4, -107);                          // daddiu t7, t4, -107
  if (bc) {goto block_132;}                         // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L77
  c->daddiu(t7, t4, -75);                           // daddiu t7, t4, -75
  if (bc) {goto block_165;}                         // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L77
  c->daddiu(t7, t4, -106);                          // daddiu t7, t4, -106
  if (bc) {goto block_165;}                         // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L72
  c->daddiu(t7, t4, -74);                           // daddiu t7, t4, -74
  if (bc) {goto block_132;}                         // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L72
  c->daddiu(t7, t4, -104);                          // daddiu t7, t4, -104
  if (bc) {goto block_132;}                         // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L78
  c->daddiu(t7, t4, -72);                           // daddiu t7, t4, -72
  if (bc) {goto block_167;}                         // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L78
  c->daddiu(t7, t4, -118);                          // daddiu t7, t4, -118
  if (bc) {goto block_167;}                         // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L81
  c->daddiu(t7, t4, -86);                           // daddiu t7, t4, -86
  if (bc) {goto block_172;}                         // branch non-likely

  bc = c->sgpr64(t7) == 0;                          // beq t7, r0, L81
  c->daddiu(t7, t4, -48);                           // daddiu t7, t4, -48
  if (bc) {goto block_172;}                         // branch non-likely

  bc = ((s64)c->sgpr64(t7)) < 0;                    // bltz t7, L87
  c->daddiu(t8, t4, -57);                           // daddiu t8, t4, -57
  if (bc) {goto block_180;}                         // branch non-likely

  bc = ((s64)c->sgpr64(t8)) > 0;                    // bgtz t8, L87
  c->sll(t8, t6, 2);                                // sll t8, t6, 2
  if (bc) {goto block_180;}                         // branch non-likely

  c->daddu(t4, t6, t8);                             // daddu t4, t6, t8
  // nop                                            // sll r0, r0, 0
  c->sll(t4, t4, 1);                                // sll t4, t4, 1
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L73                                 // beq r0, r0, L73
  c->daddu(t6, t4, t7);                             // daddu t6, t4, t7
  goto block_143;                                   // branch always


  block_161:
  bc = c->sgpr64(t6) != 0;                          // bne t6, r0, L75
  c->load_symbol(a3, cache.font12_table);           // lw a3, *font12-table*(s7)
  if (bc) {goto block_163;}                         // branch non-likely

  c->mov64(a3, a3);                                 // or a3, a3, r0
  c->addiu(t0, r0, -33);                            // addiu t0, r0, -33
  c->lqc2(vf13, 144, v1);                           // lqc2 vf13, 144(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 160, v1);                           // lqc2 vf14, 160(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf15, 176, v1);                           // lqc2 vf15, 176(v1)
  c->and_(a0, a0, t0);                              // and a0, a0, t0
  c->lq(t0, 80, v1);                                // lq t0, 80(v1)
  //beq r0, r0, L72                                 // beq r0, r0, L72
  c->lq(t1, 96, v1);                                // lq t1, 96(v1)
  goto block_132;                                   // branch always


  block_163:
  // nop                                            // sll r0, r0, 0
  c->load_symbol(a3, cache.font24_table);           // lw a3, *font24-table*(s7)
  c->mov64(a3, a3);                                 // or a3, a3, r0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf13, 192, v1);                           // lqc2 vf13, 192(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf14, 208, v1);                           // lqc2 vf14, 208(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf15, 224, v1);                           // lqc2 vf15, 224(v1)
  c->ori(a0, a0, 32);                               // ori a0, a0, 32
  c->lq(t0, 112, v1);                               // lq t0, 112(v1)
  //beq r0, r0, L72                                 // beq r0, r0, L72
  c->lq(t1, 128, v1);                               // lq t1, 128(v1)
  goto block_132;                                   // branch always


  block_164:
  // nop                                            // sll r0, r0, 0
  c->sw(t6, 3008, v1);                              // sw t6, 3008(v1)
  c->sll(t4, t6, 4);                                // sll t4, t6, 4
  c->lq(t5, 432, v1);                               // lq t5, 432(v1)
  c->daddu(t4, t4, v1);                             // daddu t4, t4, v1
  c->lq(t6, 448, v1);                               // lq t6, 448(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t5, 624, v1);                               // sq t5, 624(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t6, 640, v1);                               // sq t6, 640(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t5, 464, v1);                               // lq t5, 464(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t6, 480, v1);                               // lq t6, 480(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t5, 656, v1);                               // sq t5, 656(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t6, 672, v1);                               // sq t6, 672(v1)
  // nop                                            // sll r0, r0, 0
  c->lwu(t6, 1984, t4);                             // lwu t6, 1984(t4)
  // nop                                            // sll r0, r0, 0
  c->lwu(t5, 1988, t4);                             // lwu t5, 1988(t4)
  c->pextlb(t6, r0, t6);                            // pextlb t6, r0, t6
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(t6, r0, t6);                            // pextlh t6, r0, t6
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(t5, r0, t5);                            // pextlb t5, r0, t5
  c->sq(t6, 432, v1);                               // sq t6, 432(v1)
  c->pextlh(t5, r0, t5);                            // pextlh t5, r0, t5
  c->sq(t6, 816, v1);                               // sq t6, 816(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t5, 448, v1);                               // sq t5, 448(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t5, 832, v1);                               // sq t5, 832(v1)
  // nop                                            // sll r0, r0, 0
  c->lwu(t5, 1992, t4);                             // lwu t5, 1992(t4)
  // nop                                            // sll r0, r0, 0
  c->lwu(t4, 1996, t4);                             // lwu t4, 1996(t4)
  c->pextlb(t5, r0, t5);                            // pextlb t5, r0, t5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(t5, r0, t5);                            // pextlh t5, r0, t5
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(t4, r0, t4);                            // pextlb t4, r0, t4
  c->sq(t5, 464, v1);                               // sq t5, 464(v1)
  c->pextlh(t4, r0, t4);                            // pextlh t4, r0, t4
  c->sq(t5, 848, v1);                               // sq t5, 848(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t4, 480, v1);                               // sq t4, 480(v1)
  //beq r0, r0, L72                                 // beq r0, r0, L72
  c->sq(t4, 864, v1);                               // sq t4, 864(v1)
  goto block_132;                                   // branch always


  block_165:
  c->addiu(t4, r0, -3);                             // addiu t4, r0, -3
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t6) == 0;                          // beq t6, r0, L72
  c->and_(a0, a0, t4);                              // and a0, a0, t4
  if (bc) {goto block_132;}                         // branch non-likely

  //beq r0, r0, L72                                 // beq r0, r0, L72
  c->ori(a0, a0, 2);                                // ori a0, a0, 2
  goto block_132;                                   // branch always


  block_167:
  c->mov128_vf_gpr(vf1, t6);                        // qmtc2.i vf1, t6
  c->daddiu(t4, t5, -45);                           // daddiu t4, t5, -45
  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L80
  c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
  if (bc) {goto block_171;}                         // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L79
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_170;}                         // branch non-likely

  //beq r0, r0, L72                                 // beq r0, r0, L72
  c->vadd_bc(DEST::x, BC::x, vf23, vf23, vf1);      // vaddx.x vf23, vf23, vf1
  goto block_132;                                   // branch always


  block_170:
  //beq r0, r0, L72                                 // beq r0, r0, L72
  c->vsub_bc(DEST::x, BC::x, vf23, vf23, vf1);      // vsubx.x vf23, vf23, vf1
  goto block_132;                                   // branch always


  block_171:
  //beq r0, r0, L72                                 // beq r0, r0, L72
  c->vadd_bc(DEST::x, BC::x, vf23, vf0, vf1);       // vaddx.x vf23, vf0, vf1
  goto block_132;                                   // branch always


  block_172:
  c->mov128_vf_gpr(vf1, t6);                        // qmtc2.i vf1, t6
  c->daddiu(t4, t5, -45);                           // daddiu t4, t5, -45
  bc = c->sgpr64(t5) == 0;                          // beq t5, r0, L83
  c->vitof0(DEST::xyzw, vf1, vf1);                  // vitof0.xyzw vf1, vf1
  if (bc) {goto block_176;}                         // branch non-likely

  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L82
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_175;}                         // branch non-likely

  //beq r0, r0, L72                                 // beq r0, r0, L72
  c->vadd_bc(DEST::y, BC::x, vf23, vf23, vf1);      // vaddx.y vf23, vf23, vf1
  goto block_132;                                   // branch always


  block_175:
  //beq r0, r0, L72                                 // beq r0, r0, L72
  c->vsub_bc(DEST::y, BC::x, vf23, vf23, vf1);      // vsubx.y vf23, vf23, vf1
  goto block_132;                                   // branch always


  block_176:
  //beq r0, r0, L72                                 // beq r0, r0, L72
  c->vadd_bc(DEST::y, BC::x, vf23, vf0, vf1);       // vaddx.y vf23, vf0, vf1
  goto block_132;                                   // branch always


  block_177:
  // nop                                            // sll r0, r0, 0
  c->lw(t4, 3008, v1);                              // lw t4, 3008(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf9, 432, v1);                            // lqc2 vf9, 432(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf10, 448, v1);                           // lqc2 vf10, 448(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf11, 464, v1);                           // lqc2 vf11, 464(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf12, 480, v1);                           // lqc2 vf12, 480(v1)
  // nop                                            // sll r0, r0, 0
  c->sw(t4, 3016, v1);                              // sw t4, 3016(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf9, 304, v1);                            // sqc2 vf9, 304(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf10, 320, v1);                           // sqc2 vf10, 320(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf11, 336, v1);                           // sqc2 vf11, 336(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf12, 352, v1);                           // sqc2 vf12, 352(v1)
  //beq r0, r0, L72                                 // beq r0, r0, L72
  c->sqc2(vf23, 288, v1);                           // sqc2 vf23, 288(v1)
  goto block_132;                                   // branch always


  block_178:
  // nop                                            // sll r0, r0, 0
  c->lw(t4, 3016, v1);                              // lw t4, 3016(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf9, 304, v1);                            // lqc2 vf9, 304(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf10, 320, v1);                           // lqc2 vf10, 320(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf11, 336, v1);                           // lqc2 vf11, 336(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf12, 352, v1);                           // lqc2 vf12, 352(v1)
  // nop                                            // sll r0, r0, 0
  c->sw(t4, 3008, v1);                              // sw t4, 3008(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf9, 432, v1);                            // sqc2 vf9, 432(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf9, 816, v1);                            // sqc2 vf9, 816(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf10, 448, v1);                           // sqc2 vf10, 448(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf10, 832, v1);                           // sqc2 vf10, 832(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf11, 464, v1);                           // sqc2 vf11, 464(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf11, 848, v1);                           // sqc2 vf11, 848(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf12, 480, v1);                           // sqc2 vf12, 480(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf12, 864, v1);                           // sqc2 vf12, 864(v1)
  //beq r0, r0, L72                                 // beq r0, r0, L72
  c->lqc2(vf23, 288, v1);                           // lqc2 vf23, 288(v1)
  goto block_132;                                   // branch always


  block_179:
  c->lbu(t4, 4, t3);                                // lbu t4, 4(t3)
  c->daddiu(t3, t3, 1);                             // daddiu t3, t3, 1
  c->vadd(DEST::xyz, vf4, vf23, vf15);              // vadd.xyz vf4, vf23, vf15
  c->andi(t5, t4, 127);                             // andi t5, t4, 127
  c->vsub(DEST::xyzw, vf1, vf25, vf23);             // vsub.xyzw vf1, vf25, vf23
  c->daddiu(t5, t5, 255);                           // daddiu t5, t5, 255
  //beq r0, r0, L89                                 // beq r0, r0, L89
  c->sll(t5, t5, 4);                                // sll t5, t5, 4
  goto block_183;                                   // branch always


  block_180:
  c->vadd(DEST::xyz, vf4, vf23, vf15);              // vadd.xyz vf4, vf23, vf15
  c->sll(t5, t4, 4);                                // sll t5, t4, 4
  c->vsub(DEST::xyzw, vf1, vf25, vf23);             // vsub.xyzw vf1, vf25, vf23
  c->daddiu(t6, t4, -10);                           // daddiu t6, t4, -10
  bc = c->sgpr64(t6) == 0;                          // beq t6, r0, L88
  c->daddiu(t6, t4, -13);                           // daddiu t6, t4, -13
  if (bc) {goto block_182;}                         // branch non-likely

  bc = c->sgpr64(t6) != 0;                          // bne t6, r0, L89
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_183;}                         // branch non-likely


  block_182:
  c->daddiu(t2, t2, 16);                            // daddiu t2, t2, 16
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L72                                 // beq r0, r0, L72
  c->lqc2(vf23, 944, t2);                           // lqc2 vf23, 944(t2)
  goto block_132;                                   // branch always


  block_183:
  c->addu(t5, t5, a3);                              // addu t5, t5, a3
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf5, -256, t5);                           // lqc2 vf5, -256(t5)
  c->mov128_gpr_vf(t5, vf1);                        // qmfc2.i t5, vf1
  bc = ((s64)c->sgpr64(t5)) < 0;                    // bltz t5, L92
  c->vadd(DEST::xyz, vf6, vf5, vf16);               // vadd.xyz vf6, vf5, vf16
  if (bc) {goto block_189;}                         // branch non-likely

  c->sra(t5, t5, 31);                               // sra t5, t5, 31
  c->vadd(DEST::xyz, vf7, vf5, vf17);               // vadd.xyz vf7, vf5, vf17
  bc = ((s64)c->sgpr64(t5)) < 0;                    // bltz t5, L72
  c->vadd(DEST::xyz, vf8, vf5, vf18);               // vadd.xyz vf8, vf5, vf18
  if (bc) {goto block_132;}                         // branch non-likely

  c->vadd(DEST::xyz, vf1, vf23, vf0);               // vadd.xyz vf1, vf23, vf0
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyz, vf2, vf23, vf13);              // vadd.xyz vf2, vf23, vf13
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyz, vf3, vf23, vf14);              // vadd.xyz vf3, vf23, vf14
  c->sqc2(vf5, 496, v1);                            // sqc2 vf5, 496(v1)
  c->vadd(DEST::xyz, vf4, vf23, vf15);              // vadd.xyz vf4, vf23, vf15
  c->sqc2(vf6, 512, v1);                            // sqc2 vf6, 512(v1)
  c->vmul(DEST::xyzw, vf19, vf5, vf13);             // vmul.xyzw vf19, vf5, vf13
  c->sqc2(vf7, 528, v1);                            // sqc2 vf7, 528(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, 544, v1);                            // sqc2 vf8, 544(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf1, 368, v1);                            // sqc2 vf1, 368(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf2, 384, v1);                            // sqc2 vf2, 384(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf3, 400, v1);                            // sqc2 vf3, 400(v1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf4, 416, v1);                            // sqc2 vf4, 416(v1)
  c->lqc2(vf1, 368, v1);                            // lqc2 vf1, 368(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf2, 384, v1);                            // lqc2 vf2, 384(v1)
  c->vmula_bc(DEST::xyzw, BC::w, vf31, vf0);        // vmulaw.xyzw acc, vf31, vf0
  c->lqc2(vf3, 400, v1);                            // lqc2 vf3, 400(v1)
  c->vmadda_bc(DEST::xyzw, BC::x, vf28, vf1);       // vmaddax.xyzw acc, vf28, vf1
  c->lqc2(vf4, 416, v1);                            // lqc2 vf4, 416(v1)
  c->vmadda_bc(DEST::xyzw, BC::y, vf29, vf1);       // vmadday.xyzw acc, vf29, vf1
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf1, vf30, vf1);   // vmaddz.xyzw vf1, vf30, vf1
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf31, vf0);        // vmulaw.xyzw acc, vf31, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf28, vf2);       // vmaddax.xyzw acc, vf28, vf2
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf29, vf2);       // vmadday.xyzw acc, vf29, vf2
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf2, vf30, vf2);   // vmaddz.xyzw vf2, vf30, vf2
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf31, vf0);        // vmulaw.xyzw acc, vf31, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf28, vf3);       // vmaddax.xyzw acc, vf28, vf3
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf29, vf3);       // vmadday.xyzw acc, vf29, vf3
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf3, vf30, vf3);   // vmaddz.xyzw vf3, vf30, vf3
  // nop                                            // sll r0, r0, 0
  c->vmula_bc(DEST::xyzw, BC::w, vf31, vf0);        // vmulaw.xyzw acc, vf31, vf0
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::x, vf28, vf4);       // vmaddax.xyzw acc, vf28, vf4
  // nop                                            // sll r0, r0, 0
  c->vmadda_bc(DEST::xyzw, BC::y, vf29, vf4);       // vmadday.xyzw acc, vf29, vf4
  // nop                                            // sll r0, r0, 0
  c->vmadd_bc(DEST::xyzw, BC::z, vf4, vf30, vf4);   // vmaddz.xyzw vf4, vf30, vf4
  c->vdiv(vf25, BC::z, vf1, BC::w);                 // vdiv Q, vf25.z, vf1.w
  c->lq(t5, 32, v1);                                // lq t5, 32(v1)
  // nop                                            // sll r0, r0, 0
  c->lq(t6, 48, v1);                                // lq t6, 48(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t5, 0, a1);                                 // sq t5, 0(a1)
  // nop                                            // sll r0, r0, 0
  c->sq(t6, 16, a1);                                // sq t6, 16(a1)
  c->lqc2(vf5, 496, v1);                            // lqc2 vf5, 496(v1)
  c->mov128_gpr_gpr(t5, t0);                        // por t5, t0, r0
  c->lqc2(vf6, 512, v1);                            // lqc2 vf6, 512(v1)
  c->andi(t4, t4, 128);                             // andi t4, t4, 128
  c->lqc2(vf7, 528, v1);                            // lqc2 vf7, 528(v1)
  c->movn(t5, t1, t4);                              // movn t5, t1, t4
  c->vmulq(DEST::xyz, vf1, vf1);                    // vmulq.xyz vf1, vf1, Q
  c->sq(t5, 32, a1);                                // sq t5, 32(a1)
  c->vmulq(DEST::xyz, vf5, vf5);                    // vmulq.xyz vf5, vf5, Q
  // nop                                            // sll r0, r0, 0
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  c->vdiv(vf25, BC::z, vf2, BC::w);                 // vdiv Q, vf25.z, vf2.w
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf8, 544, v1);                            // lqc2 vf8, 544(v1)
  // nop                                            // sll r0, r0, 0
  c->vadd(DEST::xyzw, vf1, vf1, vf27);              // vadd.xyzw vf1, vf1, vf27
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf9, 432, v1);                            // lqc2 vf9, 432(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf10, 448, v1);                           // lqc2 vf10, 448(v1)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf11, 464, v1);                           // lqc2 vf11, 464(v1)
  // nop                                            // sll r0, r0, 0
  c->vftoi4(DEST::xyzw, vf1, vf1);                  // vftoi4.xyzw vf1, vf1
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf2, vf2);                    // vmulq.xyz vf2, vf2, Q
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf6, vf6);                    // vmulq.xyz vf6, vf6, Q
  c->sqc2(vf5, 48, a1);                             // sqc2 vf5, 48(a1)
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  c->vdiv(vf25, BC::z, vf3, BC::w);                 // vdiv Q, vf25.z, vf3.w
  c->sqc2(vf9, 64, a1);                             // sqc2 vf9, 64(a1)
  c->lqc2(vf12, 480, v1);                           // lqc2 vf12, 480(v1)
  c->sqc2(vf1, 80, a1);                             // sqc2 vf1, 80(a1)
  c->vadd(DEST::xyzw, vf2, vf2, vf27);              // vadd.xyzw vf2, vf2, vf27
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->vftoi4(DEST::xyzw, vf2, vf2);                  // vftoi4.xyzw vf2, vf2
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf3, vf3);                    // vmulq.xyz vf3, vf3, Q
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf7, vf7);                    // vmulq.xyz vf7, vf7, Q
  c->sqc2(vf6, 96, a1);                             // sqc2 vf6, 96(a1)
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  c->vdiv(vf25, BC::z, vf4, BC::w);                 // vdiv Q, vf25.z, vf4.w
  c->sqc2(vf10, 112, a1);                           // sqc2 vf10, 112(a1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf2, 128, a1);                            // sqc2 vf2, 128(a1)
  c->vadd(DEST::xyzw, vf3, vf3, vf27);              // vadd.xyzw vf3, vf3, vf27
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->vftoi4(DEST::xyzw, vf3, vf3);                  // vftoi4.xyzw vf3, vf3
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf4, vf4);                    // vmulq.xyz vf4, vf4, Q
  // nop                                            // sll r0, r0, 0
  c->vmulq(DEST::xyz, vf8, vf8);                    // vmulq.xyz vf8, vf8, Q
  c->sqc2(vf7, 144, a1);                            // sqc2 vf7, 144(a1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf11, 160, a1);                           // sqc2 vf11, 160(a1)
  c->vadd(DEST::xyzw, vf4, vf4, vf27);              // vadd.xyzw vf4, vf4, vf27
  c->sqc2(vf3, 176, a1);                            // sqc2 vf3, 176(a1)
  c->andi(t4, a0, 2);                               // andi t4, a0, 2
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(t4) == 0;                          // beq t4, r0, L90
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_187;}                         // branch non-likely

  //beq r0, r0, L91                                 // beq r0, r0, L91
  c->vadd_bc(DEST::x, BC::w, vf23, vf23, vf19);     // vaddw.x vf23, vf23, vf19
  goto block_188;                                   // branch always


  block_187:
  // nop                                            // sll r0, r0, 0
  c->vadd_bc(DEST::x, BC::w, vf23, vf23, vf14);     // vaddw.x vf23, vf23, vf14

  block_188:
  c->vftoi4(DEST::xyzw, vf4, vf4);                  // vftoi4.xyzw vf4, vf4
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf8, 192, a1);                            // sqc2 vf8, 192(a1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf12, 208, a1);                           // sqc2 vf12, 208(a1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf4, 224, a1);                            // sqc2 vf4, 224(a1)
  //beq r0, r0, L72                                 // beq r0, r0, L72
  c->daddiu(a1, a1, 240);                           // daddiu a1, a1, 240
  goto block_132;                                   // branch always


  block_189:
  c->lw(v1, 3024, v1);                              // lw v1, 3024(v1)
  c->sw(a1, 4, v1);                                 // sw a1, 4(v1)
  c->lqc2(vf24, 12, a2);                            // lqc2 vf24, 12(a2)
  c->vsub(DEST::xyzw, vf23, vf23, vf24);            // vsub.xyzw vf23, vf23, vf24
  c->mov128_gpr_vf(v0, vf23);                       // qmfc2.i v0, vf23
  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.font_work = intern_from_c("*font-work*").c();
  cache.font12_table = intern_from_c("*font12-table*").c();
  cache.font24_table = intern_from_c("*font24-table*").c();
  cache.math_camera = intern_from_c("*math-camera*").c();
  cache.video_parms = intern_from_c("*video-parms*").c();
  gLinkedFunctionTable.reg("draw-string", execute, 0);
}

} // namespace draw_string
} // namespace Mips2C
// clang-format on