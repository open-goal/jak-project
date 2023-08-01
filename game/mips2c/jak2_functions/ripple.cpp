//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {

struct RippleVu0 {
  Vf data_mem[256];
  void sq(const Vf& reg, u16 addr) {
    ASSERT(addr < 256);
    data_mem[addr] = reg;
  }
  Vf lq(u16 addr) {
    ASSERT(addr < 256);
    return data_mem[addr];
  }
} gRippleVu0;

namespace ripple_execute_init {
struct Cache {
  void* cos_poly_vec; // *cos-poly-vec*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->load_symbol2(v1, cache.cos_poly_vec);          // lw v1, *cos-poly-vec*(s7)
  c->lqc2(vf7, 0, v1);                              // lqc2 vf7, 0(v1)
  c->lui(v1, 15561);                                // lui v1, 15561
  c->ori(v1, v1, 4058);                             // ori v1, v1, 4058
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mov128_vf_gpr(vf6, v1);                        // qmtc2.i vf6, v1
  c->mov128_gpr_vf(v1, vf6);                        // qmfc2.i v1, vf6
  c->addiu(v1, r0, 128);                            // addiu v1, r0, 128
  u16 vi2 = c->gpr_src(v1).du16[0];                 // ctc2.i vi2, v1
  c->gprs[v1].du64[0] = vi2;                        // cfc2.i v1, vi2
  c->lui(v1, 16256);                                // lui v1, 16256
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  c->mov128_vf_gpr(vf9, v1);                        // qmtc2.i vf9, v1
  c->mov128_gpr_vf(v1, vf9);                        // qmfc2.i v1, vf9
  u16 vi1 = 0;                                      // viaddi vi1, vi0, 0
  c->addiu(v1, r0, 128);                            // addiu v1, r0, 128
  c->vmove(DEST::xyzw, vf5, vf6);                   // vmove.xyzw vf5, vf6

block_1:
  c->vmul(DEST::xyzw, vf1, vf5, vf5);               // vmul.xyzw vf1, vf5, vf5
  c->vadda_bc(DEST::xyzw, BC::w, vf0, vf0);         // vaddaw.xyzw acc, vf0, vf0
  c->vadd(DEST::x, vf5, vf5, vf6);                  // vadd.x vf5, vf5, vf6
  c->vsub_bc(DEST::y, BC::x, vf9, vf0, vf9);        // vsubx.y vf9, vf0, vf9
  c->vmul(DEST::xyzw, vf2, vf1, vf1);               // vmul.xyzw vf2, vf1, vf1
  c->vmadda_bc(DEST::xyzw, BC::x, vf1, vf7);        // vmaddax.xyzw acc, vf1, vf7
  c->vmul(DEST::xyzw, vf3, vf2, vf1);               // vmul.xyzw vf3, vf2, vf1
  c->vmadda_bc(DEST::xyzw, BC::y, vf2, vf7);        // vmadday.xyzw acc, vf2, vf7
  c->vmul(DEST::xyzw, vf4, vf2, vf2);               // vmul.xyzw vf4, vf2, vf2
  c->vmadda_bc(DEST::xyzw, BC::z, vf3, vf7);        // vmaddaz.xyzw acc, vf3, vf7
  c->daddiu(v1, v1, -1);                            // daddiu v1, v1, -1
  c->vmadd_bc(DEST::xyzw, BC::w, vf8, vf4, vf7);    // vmaddw.xyzw vf8, vf4, vf7
  c->vadd_bc(DEST::y, BC::x, vf9, vf9, vf8);        // vaddx.y vf9, vf9, vf8
  c->vsub(DEST::xyzw, vf10, vf0, vf9);              // vsub.xyzw vf10, vf0, vf9
  gRippleVu0.sq(c->vfs[vf9].vf, vi1++);             // vsqi.xyzw vf9, vi1
  c->vmove(DEST::xyzw, vf9, vf8);                   // vmove.xyzw vf9, vf8
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L40
  gRippleVu0.sq(c->vfs[vf10].vf, vi2++);            // vsqi.xyzw vf10, vi2
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
  cache.cos_poly_vec = intern_from_c("*cos-poly-vec*").c();
  gLinkedFunctionTable.reg("ripple-execute-init", execute, 32);
}

} // namespace ripple_execute_init
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace ripple_create_wave_table {
struct Cache {
  void* display; // *display*
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* setting_control; // *setting-control*
  void* atan; // atan
  void* cos; // cos
  void* ntsc; // ntsc
  void* pal; // pal
  void* ripple_update_waveform_offs; // ripple-update-waveform-offs
  void* sin; // sin
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u16 vi1, vi2;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -80);                           // daddiu sp, sp, -80
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s4, 16, sp);                                // sq s4, 16(sp)
  c->sq(s5, 32, sp);                                // sq s5, 32(sp)
  c->sq(gp, 48, sp);                                // sq gp, 48(sp)
  c->swc1(f28, 64, sp);                             // swc1 f28, 64(sp)
  c->swc1(f30, 68, sp);                             // swc1 f30, 68(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  c->lwu(v1, 4, gp);                                // lwu v1, 4(gp)
  bc = c->sgpr64(s7) != c->sgpr64(v1);              // bne s7, v1, L31
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_9;}                           // branch non-likely

  c->addiu(s5, r0, 0);                              // addiu s5, r0, 0
  //beq r0, r0, L30                                 // beq r0, r0, L30
  // nop                                            // sll r0, r0, 0
  goto block_7;                                     // branch always


block_2:
  c->addiu(v1, r0, 28);                             // addiu v1, r0, 28
  c->mult3(v1, v1, s5);                             // mult3 v1, v1, s5
  c->daddiu(v1, v1, 12);                            // daddiu v1, v1, 12
  c->daddu(s4, v1, gp);                             // daddu s4, v1, gp
  c->load_symbol2(t9, cache.atan);                  // lw t9, atan(s7)
  c->lh(v1, 10, s4);                                // lh v1, 10(s4)
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->cvtsw(f0, f0);                                 // cvt.s.w f0, f0
  c->mfc1(a0, f0);                                  // mfc1 a0, f0
  c->lh(v1, 8, s4);                                 // lh v1, 8(s4)
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->cvtsw(f0, f0);                                 // cvt.s.w f0, f0
  c->mfc1(a1, f0);                                  // mfc1 a1, f0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mtc1(f28, v0);                                 // mtc1 f28, v0
  c->lui(v1, 16768);                                // lui v1, 16768
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lh(v1, 8, s4);                                 // lh v1, 8(s4)
  c->lh(a0, 8, s4);                                 // lh a0, 8(s4)
  c->mult3(v1, v1, a0);                             // mult3 v1, v1, a0
  c->lh(a0, 10, s4);                                // lh a0, 10(s4)
  c->lh(a1, 10, s4);                                // lh a1, 10(s4)
  c->mult3(a0, a0, a1);                             // mult3 a0, a0, a1
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->cvtsw(f1, f1);                                 // cvt.s.w f1, f1
  c->sqrts(f1, f1);                                 // sqrt.s f1, f1
  c->divs(f30, f0, f1);                             // div.s f30, f0, f1
  c->load_symbol2(t9, cache.cos);                   // lw t9, cos(s7)
  c->mfc1(a0, f28);                                 // mfc1 a0, f28
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mtc1(f0, v0);                                  // mtc1 f0, v0
  c->lui(v1, 18304);                                // lui v1, 18304
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->divs(f1, f1, f30);                             // div.s f1, f1, f30
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->swc1(f0, 16, s4);                              // swc1 f0, 16(s4)
  c->load_symbol2(t9, cache.sin);                   // lw t9, sin(s7)
  c->mfc1(a0, f28);                                 // mfc1 a0, f28
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mtc1(f0, v0);                                  // mtc1 f0, v0
  c->lui(v1, 18304);                                // lui v1, 18304
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->divs(f1, f1, f30);                             // div.s f1, f1, f30
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->swc1(f0, 20, s4);                              // swc1 f0, 20(s4)
  c->load_symbol2(v1, cache.setting_control);       // lw v1, *setting-control*(s7)
  c->lwu(v1, 72, v1);                               // lwu v1, 72(v1)
  c->load_symbol_addr(a0, cache.ntsc);              // daddiu a0, s7, ntsc
  bc = c->sgpr64(v1) != c->sgpr64(a0);              // bne v1, a0, L28
  c->mov64(a0, s7);                                 // or a0, s7, r0
  if (bc) {goto block_4;}                           // branch non-likely

  c->lui(v1, 15496);                                // lui v1, 15496
  c->ori(v1, v1, 34953);                            // ori v1, v1, 34953
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lui(v1, -14464);                               // lui v1, -14464
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->divs(f1, f1, f30);                             // div.s f1, f1, f30
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->lwc1(f1, 12, s4);                              // lwc1 f1, 12(s4)
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->swc1(f0, 24, s4);                              // swc1 f0, 24(s4)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0
  //beq r0, r0, L29                                 // beq r0, r0, L29
  // nop                                            // sll r0, r0, 0
  goto block_6;                                     // branch always


block_4:
  c->load_symbol_addr(a0, cache.pal);               // daddiu a0, s7, pal
  bc = c->sgpr64(v1) != c->sgpr64(a0);              // bne v1, a0, L29
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_6;}                           // branch non-likely

  c->lui(v1, 15523);                                // lui v1, 15523
  c->ori(v1, v1, 55050);                            // ori v1, v1, 55050
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->lui(v1, -14464);                               // lui v1, -14464
  c->mtc1(f1, v1);                                  // mtc1 f1, v1
  c->divs(f1, f1, f30);                             // div.s f1, f1, f30
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->lwc1(f1, 12, s4);                              // lwc1 f1, 12(s4)
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  c->swc1(f0, 24, s4);                              // swc1 f0, 24(s4)
  c->mfc1(v1, f0);                                  // mfc1 v1, f0

block_6:
  c->daddiu(s5, s5, 1);                             // daddiu s5, s5, 1

block_7:
  c->lw(v1, 0, gp);                                 // lw v1, 0(gp)
  c->slt(v1, s5, v1);                               // slt v1, s5, v1
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L27
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, #t
  c->sw(v1, 4, gp);                                 // sw v1, 4(gp)

block_9:
  c->load_symbol2(t9, cache.ripple_update_waveform_offs);// lw t9, ripple-update-waveform-offs(s7)
  c->mov64(a0, gp);                                 // or a0, gp, r0
  c->load_symbol2(v1, cache.display);               // lw v1, *display*(s7)
  c->lwu(a1, 72, v1);                               // lwu a1, 72(v1)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->addiu(a0, r0, 64);                             // addiu a0, r0, 64
  c->mov64(a1, v1);                                 // or a1, v1, r0

block_10:
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(r0, 0, a1);                                 // sq r0, 0(a1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L32
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  if (bc) {goto block_10;}                          // branch non-likely

  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  //beq r0, r0, L36                                 // beq r0, r0, L36
  // nop                                            // sll r0, r0, 0
  goto block_17;                                    // branch always


block_12:
  c->addiu(a1, r0, 28);                             // addiu a1, r0, 28
  c->mult3(a1, a1, a0);                             // mult3 a1, a1, a0
  c->daddiu(a1, a1, 12);                            // daddiu a1, a1, 12
  c->daddu(a1, a1, gp);                             // daddu a1, a1, gp
  c->lui(a2, 18048);                                // lui a2, 18048
  c->mtc1(f0, a2);                                  // mtc1 f0, a2
  c->lui(a2, 15232);                                // lui a2, 15232
  c->mtc1(f1, a2);                                  // mtc1 f1, a2
  c->lwc1(f2, 4, a1);                               // lwc1 f2, 4(a1)
  c->muls(f1, f1, f2);                              // mul.s f1, f1, f2
  c->adds(f0, f0, f1);                              // add.s f0, f0, f1
  c->lui(a2, 15232);                                // lui a2, 15232
  c->mtc1(f1, a2);                                  // mtc1 f1, a2
  c->lwc1(f2, 16, a1);                              // lwc1 f2, 16(a1)
  c->muls(f1, f1, f2);                              // mul.s f1, f1, f2
  c->lui(a2, 15232);                                // lui a2, 15232
  c->mtc1(f2, a2);                                  // mtc1 f2, a2
  c->lwc1(f3, 20, a1);                              // lwc1 f3, 20(a1)
  c->muls(f2, f2, f3);                              // mul.s f2, f2, f3
  c->lwc1(f3, 0, a1);                               // lwc1 f3, 0(a1)
  c->addiu(a1, r0, 255);                            // addiu a1, r0, 255
  c->mfc1(a2, f0);                                  // mfc1 a2, f0
  c->mov128_vf_gpr(vf1, a2);                        // qmtc2.i vf1, a2
  c->mfc1(a2, f1);                                  // mfc1 a2, f1
  c->mov128_vf_gpr(vf2, a2);                        // qmtc2.i vf2, a2
  c->mfc1(a2, f2);                                  // mfc1 a2, f2
  c->mov128_vf_gpr(vf3, a2);                        // qmtc2.i vf3, a2
  c->mfc1(a2, f3);                                  // mfc1 a2, f3
  c->mov128_vf_gpr(vf4, a2);                        // qmtc2.i vf4, a2
  vi1 = c->gpr_src(a1).du16[0];                     // ctc2.i vi1, a1
  c->mov64(a1, v1);                                 // or a1, v1, r0
  c->addiu(a2, r0, 16);                             // addiu a2, r0, 16
  c->vmove(DEST::xyzw, vf6, vf0);                   // vmove.xyzw vf6, vf0

block_13:
  c->addiu(a3, r0, 16);                             // addiu a3, r0, 16
  c->vmove(DEST::xyzw, vf5, vf0);                   // vmove.xyzw vf5, vf0

block_14:
  c->lw(at, 0, a1);                                 // lw at, 0(a1)
  c->vadda_bc(DEST::xyzw, BC::x, vf1, vf0);         // vaddax.xyzw acc, vf1, vf0
  c->vmadda(DEST::xyzw,  vf2, vf5);                 // vmadda.xyzw acc, vf2, vf5
  c->vmadd(DEST::xyzw, vf7, vf3, vf6);              // vmadd.xyzw vf7, vf3, vf6
  c->mov128_vf_gpr(vf10, at);                       // qmtc2.i vf10, at
  c->vadd_bc(DEST::xyzw, BC::w, vf5, vf5, vf0);     // vaddw.xyzw vf5, vf5, vf0
  c->vftoi0(DEST::xyzw, vf8, vf7);                  // vftoi0.xyzw vf8, vf7
  vi2 = c->vfs[vf8].vf.x_as_u16();                  // vmtirx vi2, vf8
  c->vitof0(DEST::xyzw, vf8, vf8);                  // vitof0.xyzw vf8, vf8
  vi2 &= vi1;                                       // viand vi2, vi2, vi1
  c->vfs[vf9].vf = gRippleVu0.lq(vi2++);            // vlqi.xyzw vf9, vi2
  c->vsub(DEST::xyzw, vf7, vf7, vf8);               // vsub.xyzw vf7, vf7, vf8
  c->vadda_bc(DEST::xyzw, BC::x, vf0, vf9);         // vaddax.xyzw acc, vf0, vf9
  c->vmadd_bc(DEST::xyzw, BC::y, vf9, vf7, vf9);    // vmaddy.xyzw vf9, vf7, vf9
  c->vadda_bc(DEST::xyzw, BC::x, vf10, vf0);        // vaddax.xyzw acc, vf10, vf0
  c->vmadd(DEST::xyzw, vf10, vf9, vf4);             // vmadd.xyzw vf10, vf9, vf4
  c->mov128_gpr_vf(at, vf10);                       // qmfc2.i at, vf10
  c->sw(at, 0, a1);                                 // sw at, 0(a1)
  c->daddiu(a3, a3, -1);                            // daddiu a3, a3, -1
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L35
  c->daddiu(a1, a1, 4);                             // daddiu a1, a1, 4
  if (bc) {goto block_14;}                          // branch non-likely

  c->daddiu(a2, a2, -1);                            // daddiu a2, a2, -1
  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L34
  c->vadd_bc(DEST::xyzw, BC::w, vf6, vf6, vf0);     // vaddw.xyzw vf6, vf6, vf0
  if (bc) {goto block_13;}                          // branch non-likely

  c->mov128_gpr_vf(a1, vf6);                        // qmfc2.i a1, vf6
  c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1

block_17:
  c->lw(a1, 0, gp);                                 // lw a1, 0(gp)
  c->slt(a1, a0, a1);                               // slt a1, a0, a1
  bc = c->sgpr64(a1) != 0;                          // bne a1, r0, L33
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely

  c->mov64(a0, s7);                                 // or a0, s7, r0
  c->mov64(a0, s7);                                 // or a0, s7, r0
  c->lui(a0, 17152);                                // lui a0, 17152
  c->mtc1(f0, a0);                                  // mtc1 f0, a0
  c->lui(a0, 17279);                                // lui a0, 17279
  c->mtc1(f1, a0);                                  // mtc1 f1, a0
  c->lwc1(f2, 8, gp);                               // lwc1 f2, 8(gp)
  c->mfc1(a0, f2);                                  // mfc1 a0, f2
  c->mov128_vf_gpr(vf16, a0);                       // qmtc2.i vf16, a0
  c->mfc1(a0, f0);                                  // mfc1 a0, f0
  c->mov128_vf_gpr(vf14, a0);                       // qmtc2.i vf14, a0
  c->mfc1(a0, f1);                                  // mfc1 a0, f1
  c->mov128_vf_gpr(vf15, a0);                       // qmtc2.i vf15, a0
  c->vmax_bc(DEST::xyzw, BC::x, vf16, vf0, vf16);   // vmaxx.xyzw vf16, vf0, vf16
  c->vmini_bc(DEST::w, BC::x, vf16, vf0, vf0);      // vminix.w vf16, vf0, vf0
  c->mov64(v1, v1);                                 // or v1, v1, r0
  c->addiu(v0, r0, 15);                             // addiu v0, r0, 15

block_19:
  c->addiu(a0, r0, 15);                             // addiu a0, r0, 15

block_20:
  c->addiu(a2, r0, 4);                              // addiu a2, r0, 4
  c->addiu(at, r0, -60);                            // addiu at, r0, -60
  c->movz(a2, at, a0);                              // movz a2, at, a0
  c->addiu(a1, r0, 64);                             // addiu a1, r0, 64
  c->addiu(at, r0, -960);                           // addiu at, r0, -960
  c->daddu(a2, a2, v1);                             // daddu a2, a2, v1
  c->movz(a1, at, v0);                              // movz a1, at, v0
  c->lw(at, 0, v1);                                 // lw at, 0(v1)
  c->lw(a2, 0, a2);                                 // lw a2, 0(a2)
  c->daddu(a1, a1, v1);                             // daddu a1, a1, v1
  c->lw(a1, 0, a1);                                 // lw a1, 0(a1)
  c->mov128_vf_gpr(vf11, at);                       // qmtc2.i vf11, at
  c->mov128_vf_gpr(vf12, a2);                       // qmtc2.i vf12, a2
  c->mov128_vf_gpr(vf13, a1);                       // qmtc2.i vf13, a1
  c->vadd_bc(DEST::xyzw, BC::x, vf11, vf0, vf11);   // vaddx.xyzw vf11, vf0, vf11
  c->vsub_bc(DEST::y, BC::x, vf11, vf11, vf12);     // vsubx.y vf11, vf11, vf12
  c->vsub_bc(DEST::z, BC::x, vf11, vf11, vf13);     // vsubx.z vf11, vf11, vf13
  c->vmul(DEST::yzw, vf11, vf11, vf16);             // vmul.yzw vf11, vf11, vf16
  c->vadd_bc(DEST::xyzw, BC::x, vf11, vf11, vf14);  // vaddx.xyzw vf11, vf11, vf14
  c->vmax_bc(DEST::xyzw, BC::x, vf11, vf11, vf0);   // vmaxx.xyzw vf11, vf11, vf0
  c->vmini_bc(DEST::xyzw, BC::x, vf11, vf11, vf15); // vminix.xyzw vf11, vf11, vf15
  c->vftoi0(DEST::xyzw, vf11, vf11);                // vftoi0.xyzw vf11, vf11
  c->mov128_gpr_vf(at, vf11);                       // qmfc2.i at, vf11
  c->ppach(at, at, at);                             // ppach at, at, at
  c->ppacb(at, at, at);                             // ppacb at, at, at
  c->sw(at, 1024, v1);                              // sw at, 1024(v1)
  c->daddiu(v1, v1, 4);                             // daddiu v1, v1, 4
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L38
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  if (bc) {goto block_20;}                          // branch non-likely

  bc = c->sgpr64(v0) != 0;                          // bne v0, r0, L37
  c->daddiu(v0, v0, -1);                            // daddiu v0, v0, -1
  if (bc) {goto block_19;}                          // branch non-likely

  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lwc1(f30, 68, sp);                             // lwc1 f30, 68(sp)
  c->lwc1(f28, 64, sp);                             // lwc1 f28, 64(sp)
  c->lq(gp, 48, sp);                                // lq gp, 48(sp)
  c->lq(s5, 32, sp);                                // lq s5, 32(sp)
  c->lq(s4, 16, sp);                                // lq s4, 16(sp)
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
  cache.display = intern_from_c("*display*").c();
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  cache.setting_control = intern_from_c("*setting-control*").c();
  cache.atan = intern_from_c("atan").c();
  cache.cos = intern_from_c("cos").c();
  cache.ntsc = intern_from_c("ntsc").c();
  cache.pal = intern_from_c("pal").c();
  cache.ripple_update_waveform_offs = intern_from_c("ripple-update-waveform-offs").c();
  cache.sin = intern_from_c("sin").c();
  gLinkedFunctionTable.reg("ripple-create-wave-table", execute, 128);
}

} // namespace ripple_create_wave_table
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace ripple_apply_wave_table {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->daddiu(v1, v1, 1024);                          // daddiu v1, v1, 1024
  c->lwu(a1, 4, a0);                                // lwu a1, 4(a0)
  c->lwu(t2, 0, a0);                                // lwu t2, 0(a0)
  c->lwu(a2, 8, a0);                                // lwu a2, 8(a0)
  c->lwu(a3, 12, a0);                               // lwu a3, 12(a0)
  c->lhu(a0, 18, a0);                               // lhu a0, 18(a0)
  c->addiu(t0, r0, 0);                              // addiu t0, r0, 0
  //beq r0, r0, L25                                 // beq r0, r0, L25
  // nop                                            // sll r0, r0, 0
  goto block_4;                                     // branch always


block_1:
  c->lbu(t1, 0, a3);                                // lbu t1, 0(a3)
  c->lbu(t3, 0, a1);                                // lbu t3, 0(a1)
  c->daddiu(t3, t3, 3);                             // daddiu t3, t3, 3
  c->dsrl(t3, t3, 2);                               // dsrl t3, t3, 2
  c->dsll(t3, t3, 4);                               // dsll t3, t3, 4
  c->daddu(t2, t2, t3);                             // daddu t2, t2, t3
  c->mov64(t4, t2);                                 // or t4, t2, r0
  c->mov64(t3, t1);                                 // or t3, t1, r0
  c->mov64(t5, t4);                                 // or t5, t4, r0
  c->mov64(t4, a2);                                 // or t4, a2, r0

block_2:
  c->lb(t6, 0, t4);                                 // lb t6, 0(t4)
  c->lb(t7, 1, t4);                                 // lb t7, 1(t4)
  c->andi(t6, t6, 15);                              // andi t6, t6, 15
  c->andi(t7, t7, 15);                              // andi t7, t7, 15
  c->sll(t7, t7, 4);                                // sll t7, t7, 4
  c->daddu(t6, t6, t7);                             // daddu t6, t6, t7
  c->sll(t6, t6, 2);                                // sll t6, t6, 2
  c->daddu(t8, t6, v1);                             // daddu t8, t6, v1
  c->lb(t6, 0, t8);                                 // lb t6, 0(t8)
  c->lb(t7, 1, t8);                                 // lb t7, 1(t8)
  c->lb(t8, 2, t8);                                 // lb t8, 2(t8)
  c->sb(t6, 7, t5);                                 // sb t6, 7(t5)
  c->sb(t7, 2, t5);                                 // sb t7, 2(t5)
  c->sb(t8, 10, t5);                                // sb t8, 10(t5)
  c->daddiu(t3, t3, -1);                            // daddiu t3, t3, -1
  c->daddiu(t4, t4, 2);                             // daddiu t4, t4, 2
  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L24
  c->daddiu(t5, t5, 12);                            // daddiu t5, t5, 12
  if (bc) {goto block_2;}                           // branch non-likely

  c->lbu(t3, 2, a1);                                // lbu t3, 2(a1)
  c->lbu(t4, 1, a1);                                // lbu t4, 1(a1)
  c->daddiu(t4, t4, 3);                             // daddiu t4, t4, 3
  c->dsrl(t4, t4, 2);                               // dsrl t4, t4, 2
  c->daddu(t3, t3, t4);                             // daddu t3, t3, t4
  c->dsll(t3, t3, 4);                               // dsll t3, t3, 4
  c->daddu(t2, t2, t3);                             // daddu t2, t2, t3
  c->lbu(t3, 3, a1);                                // lbu t3, 3(a1)
  c->dsll(t3, t3, 1);                               // dsll t3, t3, 1
  c->daddiu(t3, t3, 4);                             // daddiu t3, t3, 4
  c->daddu(a1, a1, t3);                             // daddu a1, a1, t3
  c->dsll(t1, t1, 1);                               // dsll t1, t1, 1
  c->daddiu(t1, t1, 15);                            // daddiu t1, t1, 15
  c->andi(t1, t1, 65520);                           // andi t1, t1, 65520
  c->daddu(a2, a2, t1);                             // daddu a2, a2, t1
  c->daddiu(a3, a3, 2);                             // daddiu a3, a3, 2
  c->mov64(t1, a3);                                 // or t1, a3, r0
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1

block_4:
  c->slt(t1, t0, a0);                               // slt t1, t0, a0
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L23
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->mov64(v0, s7);                                 // or v0, s7, r0
  //jr ra                                           // jr ra
  c->daddu(sp, sp, r0);                             // daddu sp, sp, r0
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("ripple-apply-wave-table", execute, 128);
}

} // namespace ripple_apply_wave_table
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace ripple_matrix_scale {
u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  c->lhu(v1, 2, a0);                                // lhu v1, 2(a0)
  c->lw(a1, 4, a0);                                 // lw a1, 4(a0)
  c->lw(a2, 28, a0);                                // lw a2, 28(a0)
  c->lqc2(vf1, 16, a0);                             // lqc2 vf1, 16(a0)

block_1:
  c->lqc2(vf5, 16, a1);                             // lqc2 vf5, 16(a1)
  c->lqc2(vf6, 48, a1);                             // lqc2 vf6, 48(a1)
  c->lqc2(vf7, 64, a1);                             // lqc2 vf7, 64(a1)
  c->lqc2(vf8, 96, a1);                             // lqc2 vf8, 96(a1)
  c->vmul_bc(DEST::xyzw, BC::x, vf4, vf5, vf1);     // vmulx.xyzw vf4, vf5, vf1
  c->vmul_bc(DEST::xyzw, BC::y, vf5, vf5, vf1);     // vmuly.xyzw vf5, vf5, vf1
  c->lq(a3, 0, a1);                                 // lq a3, 0(a1)
  c->vmul_bc(DEST::xyzw, BC::z, vf7, vf7, vf1);     // vmulz.xyzw vf7, vf7, vf1
  c->lq(a0, 32, a1);                                // lq a0, 32(a1)
  c->vmul_bc(DEST::xyzw, BC::z, vf8, vf8, vf1);     // vmulz.xyzw vf8, vf8, vf1
  c->sq(a3, 0, a2);                                 // sq a3, 0(a2)
  c->lq(a3, 80, a1);                                // lq a3, 80(a1)
  c->vsub(DEST::xyzw, vf6, vf6, vf4);               // vsub.xyzw vf6, vf6, vf4
  c->sq(a0, 32, a2);                                // sq a0, 32(a2)
  c->sqc2(vf5, 16, a2);                             // sqc2 vf5, 16(a2)
  c->sq(a3, 80, a2);                                // sq a3, 80(a2)
  c->daddiu(v1, v1, -1);                            // daddiu v1, v1, -1
  c->sqc2(vf7, 64, a2);                             // sqc2 vf7, 64(a2)
  c->addiu(a1, a1, 128);                            // addiu a1, a1, 128
  c->sqc2(vf6, 48, a2);                             // sqc2 vf6, 48(a2)
  c->addiu(a2, a2, 128);                            // addiu a2, a2, 128
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L13
  c->sqc2(vf8, -32, a2);                            // sqc2 vf8, -32(a2)
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
  gLinkedFunctionTable.reg("ripple-matrix-scale", execute, 128);
}

} // namespace ripple_matrix_scale
} // namespace Mips2C