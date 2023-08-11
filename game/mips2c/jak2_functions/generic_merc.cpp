
//--------------------------MIPS2C---------------------
// clang-format off

#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
namespace Mips2C::jak2 {
namespace generic_warp_dest {
u64 execute(void* c);
}
namespace generic_prepare_dma_single {
u64 execute_real(void* c);
}
namespace generic_prepare_dma_double {
u64 execute(void* c);
}

namespace generic_light_proc {
u64 execute(void* c);
}

namespace generic_envmap_proc {
u64 execute(void* c);
}
}

using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace generic_merc_init_asm {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* gsf_buffer; // *gsf-buffer*
  void* inv_init_table; // *inv-init-table*
  void* math_camera; // *math-camera*
  void* generic_envmap_proc; // generic-envmap-proc
  void* generic_light_proc; // generic-light-proc
  void* generic_prepare_dma_double; // generic-prepare-dma-double
  void* generic_prepare_dma_single; // generic-prepare-dma-single
  void* high_speed_reject; // high-speed-reject
  void* mercneric_convert; // mercneric-convert
  void* mercneric_vu0_block; // mercneric-vu0-block
  void* upload_vu0_program; // upload-vu0-program
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->load_symbol2(t9, cache.upload_vu0_program);    // lw t9, upload-vu0-program(s7)
  c->load_symbol2(a0, cache.mercneric_vu0_block);   // lw a0, mercneric-vu0-block(s7)
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->ori(a1, v1, 84);                               // ori a1, v1, 84
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->load_symbol2(v1, cache.math_camera);           // lw v1, *math-camera*(s7)
  c->addiu(a0, r0, 7136);                           // addiu a0, r0, 7136
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->daddu(a0, a0, a1);                             // daddu a0, a0, a1
  c->load_symbol2(a1, cache.mercneric_convert);     // lw a1, mercneric-convert(s7)
  c->sw(a1, 324, a0);                               // sw a1, 324(a0)
  c->load_symbol2(a1, cache.generic_prepare_dma_single);// lw a1, generic-prepare-dma-single(s7)
  c->sw(a1, 328, a0);                               // sw a1, 328(a0)
  c->load_symbol2(a1, cache.generic_prepare_dma_double);// lw a1, generic-prepare-dma-double(s7)
  c->sw(a1, 332, a0);                               // sw a1, 332(a0)
  c->load_symbol2(a1, cache.generic_light_proc);    // lw a1, generic-light-proc(s7)
  c->sw(a1, 336, a0);                               // sw a1, 336(a0)
  c->load_symbol2(a1, cache.generic_envmap_proc);   // lw a1, generic-envmap-proc(s7)
  c->sw(a1, 340, a0);                               // sw a1, 340(a0)
  c->load_symbol2(a1, cache.high_speed_reject);     // lw a1, high-speed-reject(s7)
  c->sw(a1, 344, a0);                               // sw a1, 344(a0)
  c->sw(r0, 320, a0);                               // sw r0, 320(a0)
  c->daddu(t0, r0, a0);                             // daddu t0, r0, a0
  c->daddiu(t1, v1, 156);                           // daddiu t1, v1, 156
  c->lq(a1, 0, t1);                                 // lq a1, 0(t1)
  c->lq(a2, 16, t1);                                // lq a2, 16(t1)
  c->lq(a3, 32, t1);                                // lq a3, 32(t1)
  c->lq(t1, 48, t1);                                // lq t1, 48(t1)
  c->sq(a1, 0, t0);                                 // sq a1, 0(t0)
  c->sq(a2, 16, t0);                                // sq a2, 16(t0)
  c->sq(a3, 32, t0);                                // sq a3, 32(t0)
  c->sq(t1, 48, t0);                                // sq t1, 48(t0)
  c->daddiu(t0, a0, 64);                            // daddiu t0, a0, 64
  c->daddiu(t1, v1, 220);                           // daddiu t1, v1, 220
  c->lq(a1, 0, t1);                                 // lq a1, 0(t1)
  c->lq(a2, 16, t1);                                // lq a2, 16(t1)
  c->lq(a3, 32, t1);                                // lq a3, 32(t1)
  c->lq(t1, 48, t1);                                // lq t1, 48(t1)
  c->sq(a1, 0, t0);                                 // sq a1, 0(t0)
  c->sq(a2, 16, t0);                                // sq a2, 16(t0)
  c->sq(a3, 32, t0);                                // sq a3, 32(t0)
  c->sq(t1, 48, t0);                                // sq t1, 48(t0)
  c->daddiu(t0, a0, 128);                           // daddiu t0, a0, 128
  c->daddiu(t1, v1, 428);                           // daddiu t1, v1, 428
  c->lq(a1, 0, t1);                                 // lq a1, 0(t1)
  c->lq(a2, 16, t1);                                // lq a2, 16(t1)
  c->lq(a3, 32, t1);                                // lq a3, 32(t1)
  c->lq(t1, 48, t1);                                // lq t1, 48(t1)
  c->sq(a1, 0, t0);                                 // sq a1, 0(t0)
  c->sq(a2, 16, t0);                                // sq a2, 16(t0)
  c->sq(a3, 32, t0);                                // sq a3, 32(t0)
  c->sq(t1, 48, t0);                                // sq t1, 48(t0)
  c->lui(a1, 16256);                                // lui a1, 16256
  c->mtc1(f0, a1);                                  // mtc1 f0, a1
  c->lwc1(f1, 12, v1);                              // lwc1 f1, 12(v1)
  c->divs(f1, f0, f1);                              // div.s f1, f0, f1
  c->lui(a1, 16256);                                // lui a1, 16256
  c->mtc1(f0, a1);                                  // mtc1 f0, a1
  c->lwc1(f2, 16, v1);                              // lwc1 f2, 16(v1)
  c->divs(f0, f0, f2);                              // div.s f0, f0, f2
  c->daddiu(a2, a0, 352);                           // daddiu a2, a0, 352
  c->daddiu(a1, a0, 368);                           // daddiu a1, a0, 368
  c->daddiu(a0, a0, 384);                           // daddiu a0, a0, 384
  c->swc1(f1, 0, a2);                               // swc1 f1, 0(a2)
  c->negs(f1, f1);                                  // neg.s f1, f1
  c->swc1(f1, 4, a2);                               // swc1 f1, 4(a2)
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  c->swc1(f1, 8, a2);                               // swc1 f1, 8(a2)
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  c->swc1(f1, 12, a2);                              // swc1 f1, 12(a2)
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  c->swc1(f1, 0, a1);                               // swc1 f1, 0(a1)
  c->mtc1(f1, r0);                                  // mtc1 f1, r0
  c->swc1(f1, 4, a1);                               // swc1 f1, 4(a1)
  c->swc1(f0, 8, a1);                               // swc1 f0, 8(a1)
  c->negs(f0, f0);                                  // neg.s f0, f0
  c->swc1(f0, 12, a1);                              // swc1 f0, 12(a1)
  c->lui(a1, 15104);                                // lui a1, 15104
  c->mtc1(f0, a1);                                  // mtc1 f0, a1
  c->swc1(f0, 0, a0);                               // swc1 f0, 0(a0)
  c->lui(a1, 15104);                                // lui a1, 15104
  c->mtc1(f0, a1);                                  // mtc1 f0, a1
  c->swc1(f0, 4, a0);                               // swc1 f0, 4(a0)
  c->lui(a1, 16128);                                // lui a1, 16128
  c->mtc1(f0, a1);                                  // mtc1 f0, a1
  c->swc1(f0, 8, a0);                               // swc1 f0, 8(a0)
  c->lwc1(f0, 908, v1);                             // lwc1 f0, 908(v1)
  c->swc1(f0, 12, a0);                              // swc1 f0, 12(a0)
  c->load_symbol2(v1, cache.gsf_buffer);            // lw v1, *gsf-buffer*(s7)
  c->daddiu(a0, v1, 32);                            // daddiu a0, v1, 32
  c->sw(a0, 0, v1);                                 // sw a0, 0(v1)
  c->daddiu(a0, v1, 448);                           // daddiu a0, v1, 448
  c->sw(a0, 4, v1);                                 // sw a0, 4(v1)
  c->daddiu(a0, v1, 6608);                          // daddiu a0, v1, 6608
  c->sw(a0, 8, v1);                                 // sw a0, 8(v1)
  c->daddiu(a0, v1, 2784);                          // daddiu a0, v1, 2784
  c->sw(a0, 2752, v1);                              // sw a0, 2752(v1)
  c->daddiu(a0, v1, 3200);                          // daddiu a0, v1, 3200
  c->sw(a0, 2756, v1);                              // sw a0, 2756(v1)
  c->daddiu(a0, v1, 6608);                          // daddiu a0, v1, 6608
  c->sw(a0, 2760, v1);                              // sw a0, 2760(v1)
  c->load_symbol2(v1, cache.gsf_buffer);            // lw v1, *gsf-buffer*(s7)
  c->daddiu(a0, v1, 5504);                          // daddiu a0, v1, 5504
  c->mov64(a0, a0);                                 // or a0, a0, r0
  c->load_symbol2(a1, cache.inv_init_table);        // lw a1, *inv-init-table*(s7)
  c->mov64(a1, a1);                                 // or a1, a1, r0
  c->sq(r0, 0, a0);                                 // sq r0, 0(a0)
  c->sq(r0, 544, a0);                               // sq r0, 544(a0)
  c->sq(r0, 16, a0);                                // sq r0, 16(a0)
  c->sq(r0, 560, a0);                               // sq r0, 560(a0)
  c->sq(r0, 208, a0);                               // sq r0, 208(a0)
  c->sq(r0, 752, a0);                               // sq r0, 752(a0)
  c->sq(r0, 224, a0);                               // sq r0, 224(a0)
  c->sq(r0, 768, a0);                               // sq r0, 768(a0)
  c->sq(r0, 240, a0);                               // sq r0, 240(a0)
  c->sq(r0, 784, a0);                               // sq r0, 784(a0)
  c->sq(r0, 384, a0);                               // sq r0, 384(a0)
  c->sq(r0, 800, a0);                               // sq r0, 800(a0)
  c->sq(r0, 400, a0);                               // sq r0, 400(a0)
  c->sq(r0, 944, a0);                               // sq r0, 944(a0)
  c->sq(r0, 416, a0);                               // sq r0, 416(a0)
  c->sq(r0, 960, a0);                               // sq r0, 960(a0)
  c->sq(r0, 432, a0);                               // sq r0, 432(a0)
  c->sq(r0, 976, a0);                               // sq r0, 976(a0)
  c->sq(r0, 448, a0);                               // sq r0, 448(a0)
  c->sq(r0, 992, a0);                               // sq r0, 992(a0)
  c->sq(r0, 464, a0);                               // sq r0, 464(a0)
  c->sq(r0, 1008, a0);                              // sq r0, 1008(a0)
  c->sq(r0, 480, a0);                               // sq r0, 480(a0)
  c->sq(r0, 1024, a0);                              // sq r0, 1024(a0)
  c->sq(r0, 496, a0);                               // sq r0, 496(a0)
  c->sq(r0, 1040, a0);                              // sq r0, 1040(a0)
  c->sq(r0, 512, a0);                               // sq r0, 512(a0)
  c->sq(r0, 1056, a0);                              // sq r0, 1056(a0)
  c->sq(r0, 528, a0);                               // sq r0, 528(a0)
  c->sq(r0, 1072, a0);                              // sq r0, 1072(a0)
  c->addiu(a2, r0, 8);                              // addiu a2, r0, 8
  // nop                                            // sll r0, r0, 0

block_1:
  c->lhu(t1, 2, a1);                                // lhu t1, 2(a1)
  c->daddiu(a2, a2, -1);                            // daddiu a2, a2, -1
  c->lbu(a3, 0, a1);                                // lbu a3, 0(a1)
  // nop                                            // sll r0, r0, 0
  c->lbu(t0, 1, a1);                                // lbu t0, 1(a1)
  // nop                                            // sll r0, r0, 0
  c->daddu(t1, t1, a0);                             // daddu t1, t1, a0
  c->daddiu(a1, a1, 4);                             // daddiu a1, a1, 4

block_2:
  c->sb(t0, 0, t1);                                 // sb t0, 0(t1)
  c->daddiu(a3, a3, -1);                            // daddiu a3, a3, -1
  c->sb(t0, 550, t1);                               // sb t0, 550(t1)
  c->daddiu(t0, t0, 2);                             // daddiu t0, t0, 2
  c->daddiu(t1, t1, 3);                             // daddiu t1, t1, 3
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L167
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L166
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->daddu(a0, r0, v1);                             // daddu a0, r0, v1
  c->mov64(a0, a0);                                 // or a0, a0, r0
  c->sq(r0, 416, a0);                               // sq r0, 416(a0)
  c->sq(r0, 432, a0);                               // sq r0, 432(a0)
  c->daddiu(v1, v1, 2752);                          // daddiu v1, v1, 2752
  c->mov64(v1, v1);                                 // or v1, v1, r0
  c->sq(r0, 416, v1);                               // sq r0, 416(v1)
  c->sq(r0, 432, v1);                               // sq r0, 432(v1)
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->lui(v1, 4096);                                 // lui v1, 4096
  c->ori(v1, v1, 32768);                            // ori v1, v1, 32768
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->ori(a0, a0, 84);                               // ori a0, a0, 84

// just a dma sync.
//  c->lw(a1, 0, v1);                                 // lw a1, 0(v1)
//  c->andi(a1, a1, 256);                             // andi a1, a1, 256
//  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L169
//  DANGER jump to delay slot, this MUST be fixed manually!
//  c->lw(a1, 0, a0);                                 // lw a1, 0(a0)
//  if (bc) {goto block_-1;}                          // branch non-likely
//
//
//block_5:
//  c->lw(a1, 0, a0);                                 // lw a1, 0(a0)
//  // nop                                            // sll r0, r0, 0
//  c->lw(a2, 0, v1);                                 // lw a2, 0(v1)
//  // nop                                            // sll r0, r0, 0
//  c->andi(a2, a2, 256);                             // andi a2, a2, 256
//  c->daddiu(a1, a1, 1);                             // daddiu a1, a1, 1
//  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L168
//  c->sw(a1, 0, a0);                                 // sw a1, 0(a0)
//  if (bc) {goto block_5;}                           // branch non-likely

  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
  goto end_of_function;                             // return

end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  cache.gsf_buffer = intern_from_c("*gsf-buffer*").c();
  cache.inv_init_table = intern_from_c("*inv-init-table*").c();
  cache.math_camera = intern_from_c("*math-camera*").c();
  cache.generic_envmap_proc = intern_from_c("generic-envmap-proc").c();
  cache.generic_light_proc = intern_from_c("generic-light-proc").c();
  cache.generic_prepare_dma_double = intern_from_c("generic-prepare-dma-double").c();
  cache.generic_prepare_dma_single = intern_from_c("generic-prepare-dma-single").c();
  cache.high_speed_reject = intern_from_c("high-speed-reject").c();
  cache.mercneric_convert = intern_from_c("mercneric-convert").c();
  cache.mercneric_vu0_block = intern_from_c("mercneric-vu0-block").c();
  cache.upload_vu0_program = intern_from_c("upload-vu0-program").c();
  gLinkedFunctionTable.reg("generic-merc-init-asm", execute, 128);
}

} // namespace generic_merc_init_asm
} // namespace Mips2C


//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace mercneric_convert {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;


u8 vu0_data_mem[1024 * 4];

void sq_buffer(Mask mask, const Vf& data, u32 qw) {
  ASSERT(qw * 16 < sizeof(vu0_data_mem));
  for (int i = 0; i < 4; i++) {
    if ((u64)mask & (1 << i)) {
      memcpy(vu0_data_mem + qw * 16 + i * 4, data.data + i, 4);
    }
  }
}

void sq_xyzw(const Vf& data, u32 qw) {
  memcpy(vu0_data_mem + qw * 16, data.data, 16);
}

void lq_buffer(Mask mask, Vf& data, u32 qw) {
  ASSERT(qw * 16 < sizeof(vu0_data_mem));
  for (int i = 0; i < 4; i++) {
    if ((u64)mask & (1 << i)) {
      memcpy(data.data + i, vu0_data_mem + qw * 16 + i * 4, 4);
    }
  }
}

void lq_xyzw(Vf& data, u32 qw) {
  memcpy(data.data, vu0_data_mem + qw * 16, 16);
}

void vcallms_280(ExecutionContext* c, u16* vis) {
  bool bc;
  // 0.003921569                |  maxw.x vf17, vf00, vf00 :i
  c->vfs[vf17].vf.max(Mask::x, c->vf_src(vf00).vf, c->vf_src(vf00).vf.w());   c->I = 0.003921569;
  // -65537.0                   |  maxi.y vf17, vf00, I :i
  c->vfs[vf17].vf.maxi(Mask::y, c->vf_src(vf00).vf, c->I);   c->I = -65537.0;
  // iaddiu vi10, vi10, 0x88    |  minii.z vf05, vf00, I
  c->vfs[vf05].vf.minii(Mask::z, c->vf_src(vf00).vf, c->I);   vis[vi10] = vis[vi10] + 0x88; /* 136 */
  // iaddiu vi08, vi00, 0x8c    |  minii.z vf06, vf00, I
  c->vfs[vf06].vf.minii(Mask::z, c->vf_src(vf00).vf, c->I);   vis[vi08] = 0x8c; /* 140 */
  // sqi.xyzw vf01, vi08        |  minii.z vf07, vf00, I

  c->vfs[vf07].vf.minii(Mask::z, c->vf_src(vf00).vf, c->I);   sq_xyzw(c->vf_src(vf01).vf, vis[vi08]++);
  // sqi.xyzw vf02, vi08        |  minix.w vf05, vf00, vf27
  c->vfs[vf05].vf.mini(Mask::w, c->vf_src(vf00).vf, c->vf_src(vf27).vf.x());   sq_xyzw(c->vf_src(vf02).vf, vis[vi08]++);
  // sqi.xyzw vf03, vi08        |  miniy.w vf06, vf00, vf27
  c->vfs[vf06].vf.mini(Mask::w, c->vf_src(vf00).vf, c->vf_src(vf27).vf.y());   sq_xyzw(c->vf_src(vf03).vf, vis[vi08]++);
  // sqi.xyzw vf04, vi08        |  miniz.w vf07, vf00, vf27
  c->vfs[vf07].vf.mini(Mask::w, c->vf_src(vf00).vf, c->vf_src(vf27).vf.z());   sq_xyzw(c->vf_src(vf04).vf, vis[vi08]++);

  // BRANCH!
  // ibne vi00, vi13, L1        |  nop
  bc = (vis[vi13] != 0);
  // iaddiu vi05, vi00, 0x182   |  nop
  vis[vi05] = 0x182; /* 386 */
  if (bc) { goto L1; }

  // iaddiu vi05, vi00, 0x1ab   |  nop
  vis[vi05] = 0x1ab; /* 427 */
  L1:
  // BRANCH!
  // ibne vi00, vi12, L2        |  nop
  bc = (vis[vi12] != 0);
  // iaddiu vi03, vi00, 0x146   |  nop
  vis[vi03] = 0x146; /* 326 */
  if (bc) { goto L2; }

  // ior vi03, vi05, vi00       |  nop
  vis[vi03] = vis[vi05];
  L2:
  // BRANCH!
  // ibne vi00, vi11, L3        |  nop
  bc = (vis[vi11] != 0);
  // iaddiu vi01, vi00, 0x13a   |  nop
  vis[vi01] = 0x13a; /* 314 */
  if (bc) { goto L3; }

  // ior vi01, vi03, vi00       |  nop
  vis[vi01] = vis[vi03];
  L3:
  // lqi.xyzw vf29, vi10        |  nop
  lq_xyzw(c->vfs[vf29].vf, vis[vi10]++);
  // iadd vi02, vi08, vi11      |  nop
  vis[vi02] = vis[vi08] + vis[vi11];
  // iadd vi04, vi02, vi12      |  nop
  vis[vi04] = vis[vi02] + vis[vi12];
  // iadd vi06, vi04, vi13      |  nop
  vis[vi06] = vis[vi04] + vis[vi13];
  // mtir vi13, vf29.w          |  itof0.xyzw vf18, vf29 :e
  c->vfs[vf18].vf.itof0(Mask::xyzw, c->vf_src(vf29).vf);   vis[vi13] = c->vf_src(vf29).vf.w_as_u16();
  // iaddiu vi09, vi00, 0x7f    |  nop
  vis[vi09] = 0x7f; /* 127 */


}

void vcallms_303(ExecutionContext* c, u16* vis) {
  // vf21.x coming into here is bad (should be < 1, I think)
  // sq.xyzw vf23, 3(vi14)      |  mulx.xyzw vf11, vf20, vf19
  c->vfs[vf11].vf.mul_xyzw(c->vf_src(vf20).vf, c->vf_src(vf19).vf.x());   sq_xyzw(c->vf_src(vf23).vf, vis[vi14] + 3);
  // sq.xyzw vf24, 4(vi14)      |  mulx.xyzw vf12, vf21, vf19
  c->vfs[vf12].vf.mul_xyzw(c->vf_src(vf21).vf, c->vf_src(vf19).vf.x());   sq_xyzw(c->vf_src(vf24).vf, vis[vi14] + 4);
  // sq.xyzw vf25, 5(vi14)      |  mulx.xyzw vf13, vf22, vf19
  c->vfs[vf13].vf.mul_xyzw(c->vf_src(vf22).vf, c->vf_src(vf19).vf.x());   sq_xyzw(c->vf_src(vf25).vf, vis[vi14] + 5);

  // sq.xyzw vf26, 6(vi14)      |  nop
  sq_xyzw(c->vf_src(vf26).vf, vis[vi14] + 6);
  // sq.xyzw vf11, 0(vi14)      |  nop
  sq_xyzw(c->vf_src(vf11).vf, vis[vi14]);
  // sq.xyzw vf12, 1(vi14)      |  nop
  sq_xyzw(c->vf_src(vf12).vf, vis[vi14] + 1);
  // sq.xyzw vf13, 2(vi14)      |  nop :e
  sq_xyzw(c->vf_src(vf13).vf, vis[vi14] + 2);
  // nop                        |  nop

}

void vcallms_311_case_314(ExecutionContext* c, u16* vis) {
  // this is one of the most expensive operations.

  // 00 mtir vi11, vf01.x          |  maddz.xyzw vf11, vf26, vf10
  // 01 jr vi01                    |  mul.xyzw vf14, vf13, Q
  // 02 sqi.xyzw vf04, vi08        |  mulaw.xyzw ACC, vf20, vf08
  // 03 rsqrt Q, vf00.w, vf16.x    |  maddaw.xyzw ACC, vf21, vf09
  // 04 lq.xyzw vf24, -124(vi11)   |  maddaw.xyzw ACC, vf22, vf10
  // 05 lq.xyzw vf25, -123(vi11)   |  maddw.xyzw vf15, vf23, vf00
  // 06 lq.xyzw vf26, -122(vi11)   |  mul.xyzw vf16, vf11, vf11
  // 07 lq.xyzw vf20, -128(vi11)   |  add.xyzw vf08, vf01, vf05
  // 08 lq.xyzw vf21, -127(vi11)   |  add.xyzw vf09, vf02, vf06
  // 09 lq.xyzw vf22, -126(vi11)   |  add.xyzw vf10, vf03, vf07
  // 10 ibne vi08, vi02, L4        |  adday.xyzw vf16, vf16
  // 11 lq.xyzw vf23, -125(vi11)   |  maddz.xyzw vf16, vf17, vf16
  // 12 ior vi01, vi03, vi00       |  nop
  // L4:
  // 13 move.xyzw vf13, vf12       |  mulaz.xyzw ACC, vf24, vf08 :e
  // 14 move.xyzw vf12, vf11       |  maddaz.xyzw ACC, vf25, vf09

  // we've got:
  // - mul by Q
  // - calculate next Q
  // - accumulated multiply part
  // - lots of loading
  // - adding part

  c->acc.vf.madd_xyzw(c->vfs[vf11].vf, c->vf_src(vf26).vf, c->vf_src(vf10).vf.z());
  vis[vi11] = c->vf_src(vf01).vf.x_as_u16();
  c->vfs[vf14].vf.mul_xyzw(c->vf_src(vf13).vf, c->Q);
  sq_xyzw(c->vf_src(vf04).vf, vis[vi08]++);
  c->Q = 1.f / std::sqrt(c->vf_src(vf16).vf.x());



  //  c->acc.vf.mula_xyzw(c->vf_src(vf20).vf, c->vf_src(vf08).vf.w());
  //  c->acc.vf.madda_xyzw(c->vfs[vf21].vf, c->vfs[vf09].vf.w());
  //  c->acc.vf.madda_xyzw(c->vfs[vf22].vf, c->vfs[vf10].vf.w());
  //  c->acc.vf.madd_xyzw(c->vfs[vf15].vf, c->vf_src(vf23).vf, c->vf_src(vf00).vf.w());

  __m128 acc = _mm_mul_ps(_mm_load_ps(c->vf_src(vf20).vf.data), _mm_set1_ps(c->vf_src(vf08).vf.w()));
  acc = _mm_add_ps(_mm_mul_ps(_mm_load_ps(c->vf_src(vf21).vf.data), _mm_set1_ps(c->vf_src(vf09).vf.w())), acc);
  acc = _mm_add_ps(_mm_mul_ps(_mm_load_ps(c->vf_src(vf22).vf.data), _mm_set1_ps(c->vf_src(vf10).vf.w())), acc);
  acc = _mm_add_ps(_mm_load_ps(c->vf_src(vf23).vf.data), acc);
  _mm_store_ps(c->vfs[vf15].vf.data, acc);


  //  lq_xyzw(c->vfs[vf20].vf, vis[vi11] + -128);
  //  lq_xyzw(c->vfs[vf21].vf, vis[vi11] + -127);
  //  lq_xyzw(c->vfs[vf22].vf, vis[vi11] + -126);
  //  lq_xyzw(c->vfs[vf23].vf, vis[vi11] + -125);
  //  lq_xyzw(c->vfs[vf24].vf, vis[vi11] + -124);
  //  lq_xyzw(c->vfs[vf25].vf, vis[vi11] + -123);
  //  lq_xyzw(c->vfs[vf26].vf, vis[vi11] + -122);
  memcpy(c->vfs[vf20].vf.data, vu0_data_mem + (vis[vi11] - 128) * 16, 7 * 16);


  c->vfs[vf16].vf.mul_xyzw(c->vf_src(vf11).vf, c->vf_src(vf11).vf);
  c->vfs[vf08].vf.add_xyzw(c->vf_src(vf01).vf, c->vf_src(vf05).vf);
  c->vfs[vf09].vf.add_xyzw(c->vf_src(vf02).vf, c->vf_src(vf06).vf);
  c->vfs[vf10].vf.add_xyzw(c->vf_src(vf03).vf, c->vf_src(vf07).vf);

  c->acc.vf.adda(Mask::xyzw, c->vfs[vf16].vf, c->vfs[vf16].vf.y());
  c->acc.vf.madd_xyzw(c->vfs[vf16].vf, c->vf_src(vf17).vf, c->vf_src(vf16).vf.z());
  c->acc.vf.mula_xyzw(c->vf_src(vf24).vf, c->vf_src(vf08).vf.z());
  c->acc.vf.madda_xyzw(c->vfs[vf25].vf, c->vfs[vf09].vf.z());

  c->vfs[vf13].vf.move(Mask::xyzw, c->vf_src(vf12).vf);
  c->vfs[vf12].vf.move(Mask::xyzw, c->vf_src(vf11).vf);

  // end integer thing.
  if (vis[vi08] == vis[vi02]) {
    vis[vi01] = vis[vi03];
  }
}

void vcallms_311_case_326(ExecutionContext* c, u16* vis) {
  bool bc;
  // mtir vi11, vf01.x          |  maddz.xyzw vf11, vf26, vf10
  c->acc.vf.madd_xyzw(c->vfs[vf11].vf, c->vf_src(vf26).vf, c->vf_src(vf10).vf.z());   vis[vi11] = c->vf_src(vf01).vf.x_as_u16();
  // jr vi01                    |  mul.xyzw vf14, vf13, Q
  c->vfs[vf14].vf.mul_xyzw(c->vf_src(vf13).vf, c->Q);
  // sqi.xyzw vf04, vi08        |  mulaw.xyzw ACC, vf20, vf08
  c->acc.vf.mula_xyzw(c->vf_src(vf20).vf, c->vf_src(vf08).vf.w());   sq_xyzw(c->vf_src(vf04).vf, vis[vi08]++);
// rsqrt Q, vf00.w, vf16.x    |  maddaw.xyzw ACC, vf21, vf09
  c->acc.vf.madda_xyzw(c->vfs[vf21].vf, c->vfs[vf09].vf.w());   c->Q = c->vf_src(vf00).vf.w() / std::sqrt(c->vf_src(vf16).vf.x());
  // mtir vi12, vf01.y          |  maddaw.xyzw ACC, vf22, vf10
  c->acc.vf.madda_xyzw(c->vfs[vf22].vf, c->vfs[vf10].vf.w());   vis[vi12] = c->vf_src(vf01).vf.y_as_u16();
  // iand vi11, vi11, vi09      |  maddw.xyzw vf15, vf23, vf00
  c->acc.vf.madd_xyzw(c->vfs[vf15].vf, c->vf_src(vf23).vf, c->vf_src(vf00).vf.w());   vis[vi11] = vis[vi11] & vis[vi09];
  // lq.xyzw vf19, 4(vi11)      |  mul.xyzw vf16, vf11, vf11
  c->vfs[vf16].vf.mul_xyzw(c->vf_src(vf11).vf, c->vf_src(vf11).vf);   lq_xyzw(c->vfs[vf19].vf, vis[vi11] + 4);
  // BRANCH!
  // ibeq vi09, vi11, L7        |  add.xyzw vf08, vf01, vf05
  c->vfs[vf08].vf.add_xyzw(c->vf_src(vf01).vf, c->vf_src(vf05).vf);   bc = (vis[vi09] == vis[vi11]);
  // iand vi12, vi12, vi09      |  add.xyzw vf09, vf02, vf06
  c->vfs[vf09].vf.add_xyzw(c->vf_src(vf02).vf, c->vf_src(vf06).vf);   vis[vi12] = vis[vi12] & vis[vi09];
  if (bc) { goto L7; }

  // nop                        |  muly.xyzw vf18, vf18, vf17
  c->vfs[vf18].vf.mul_xyzw(c->vf_src(vf18).vf, c->vf_src(vf17).vf.y());
  // lq.xyzw vf24, 4(vi12)      |  add.xyzw vf10, vf03, vf07
  c->vfs[vf10].vf.add_xyzw(c->vf_src(vf03).vf, c->vf_src(vf07).vf);   lq_xyzw(c->vfs[vf24].vf, vis[vi12] + 4);
  // lq.xyzw vf27, 5(vi11)      |  adday.xyzw vf16, vf16
  c->acc.vf.adda(Mask::xyzw, c->vfs[vf16].vf, c->vfs[vf16].vf.y());   lq_xyzw(c->vfs[vf27].vf, vis[vi11] + 5);
  // lq.xyzw vf25, 5(vi12)      |  maddz.xyzw vf16, vf17, vf16
  c->acc.vf.madd_xyzw(c->vfs[vf16].vf, c->vf_src(vf17).vf, c->vf_src(vf16).vf.z());   lq_xyzw(c->vfs[vf25].vf, vis[vi12] + 5);
  // lq.xyzw vf28, 6(vi11)      |  mulax.xyzw ACC, vf19, vf18
  c->acc.vf.mula_xyzw(c->vf_src(vf19).vf, c->vf_src(vf18).vf.x());   lq_xyzw(c->vfs[vf28].vf, vis[vi11] + 6);
  // lq.xyzw vf26, 6(vi12)      |  maddy.xyzw vf24, vf24, vf18
  c->acc.vf.madd_xyzw(c->vfs[vf24].vf, c->vf_src(vf24).vf, c->vf_src(vf18).vf.y());   lq_xyzw(c->vfs[vf26].vf, vis[vi12] + 6);
  // lq.xyzw vf29, 0(vi11)      |  mulax.xyzw ACC, vf27, vf18
  c->acc.vf.mula_xyzw(c->vf_src(vf27).vf, c->vf_src(vf18).vf.x());   lq_xyzw(c->vfs[vf29].vf, vis[vi11]);
  // lq.xyzw vf20, 0(vi12)      |  maddy.xyzw vf25, vf25, vf18
  c->acc.vf.madd_xyzw(c->vfs[vf25].vf, c->vf_src(vf25).vf, c->vf_src(vf18).vf.y());   lq_xyzw(c->vfs[vf20].vf, vis[vi12]);
  // lq.xyzw vf19, 1(vi11)      |  mulax.xyzw ACC, vf28, vf18
  c->acc.vf.mula_xyzw(c->vf_src(vf28).vf, c->vf_src(vf18).vf.x());   lq_xyzw(c->vfs[vf19].vf, vis[vi11] + 1);
  // lq.xyzw vf21, 1(vi12)      |  maddy.xyzw vf26, vf26, vf18
  c->acc.vf.madd_xyzw(c->vfs[vf26].vf, c->vf_src(vf26).vf, c->vf_src(vf18).vf.y());   lq_xyzw(c->vfs[vf21].vf, vis[vi12] + 1);
  // lq.xyzw vf27, 2(vi11)      |  mulax.xyzw ACC, vf29, vf18
  c->acc.vf.mula_xyzw(c->vf_src(vf29).vf, c->vf_src(vf18).vf.x());   lq_xyzw(c->vfs[vf27].vf, vis[vi11] + 2);
  // lq.xyzw vf22, 2(vi12)      |  maddy.xyzw vf20, vf20, vf18
  c->acc.vf.madd_xyzw(c->vfs[vf20].vf, c->vf_src(vf20).vf, c->vf_src(vf18).vf.y());   lq_xyzw(c->vfs[vf22].vf, vis[vi12] + 2);
  // lq.xyzw vf28, 3(vi11)      |  mulax.xyzw ACC, vf19, vf18
  c->acc.vf.mula_xyzw(c->vf_src(vf19).vf, c->vf_src(vf18).vf.x());   lq_xyzw(c->vfs[vf28].vf, vis[vi11] + 3);
  // lq.xyzw vf23, 3(vi12)      |  maddy.xyzw vf21, vf21, vf18
  c->acc.vf.madd_xyzw(c->vfs[vf21].vf, c->vf_src(vf21).vf, c->vf_src(vf18).vf.y());   lq_xyzw(c->vfs[vf23].vf, vis[vi12] + 3);
  // nop                        |  mulax.xyzw ACC, vf27, vf18
  c->acc.vf.mula_xyzw(c->vf_src(vf27).vf, c->vf_src(vf18).vf.x());
  // iaddiu vi01, vi00, 0x161   |  maddy.xyzw vf22, vf22, vf18
  c->acc.vf.madd_xyzw(c->vfs[vf22].vf, c->vf_src(vf22).vf, c->vf_src(vf18).vf.y());   vis[vi01] = 0x161; /* 353 */
  // BRANCH!
  // ibne vi08, vi04, L5        |  mulax.xyzw ACC, vf28, vf18
  c->acc.vf.mula_xyzw(c->vf_src(vf28).vf, c->vf_src(vf18).vf.x());   bc = (vis[vi08] != vis[vi04]);
  // nop                        |  maddy.xyzw vf23, vf23, vf18
  c->acc.vf.madd_xyzw(c->vfs[vf23].vf, c->vf_src(vf23).vf, c->vf_src(vf18).vf.y());
  if (bc) { goto L5; }

  // ior vi01, vi05, vi00       |  nop
  vis[vi01] = vis[vi05];
  L5:
  // move.xyzw vf13, vf12       |  mulaz.xyzw ACC, vf24, vf08 :e
  c->acc.vf.mula_xyzw(c->vf_src(vf24).vf, c->vf_src(vf08).vf.z());   c->vfs[vf13].vf.move(Mask::xyzw, c->vf_src(vf12).vf);
  // move.xyzw vf12, vf11       |  maddaz.xyzw ACC, vf25, vf09
  c->acc.vf.madda_xyzw(c->vfs[vf25].vf, c->vfs[vf09].vf.z());   c->vfs[vf12].vf.move(Mask::xyzw, c->vf_src(vf11).vf);
  return;
  L7:
  // nop                        |  add.xyzw vf10, vf03, vf07
  c->vfs[vf10].vf.add_xyzw(c->vf_src(vf03).vf, c->vf_src(vf07).vf);
  // BRANCH!
  // ibne vi08, vi04, L8        |  adday.xyzw vf16, vf16
  c->acc.vf.adda(Mask::xyzw, c->vfs[vf16].vf, c->vfs[vf16].vf.y());   bc = (vis[vi08] != vis[vi04]);
  // nop                        |  maddz.xyzw vf16, vf17, vf16
  c->acc.vf.madd_xyzw(c->vfs[vf16].vf, c->vf_src(vf17).vf, c->vf_src(vf16).vf.z());
  if (bc) { goto L8; }

  // ior vi01, vi05, vi00       |  nop
  vis[vi01] = vis[vi05];
  L8:
  // move.xyzw vf13, vf12       |  mulaz.xyzw ACC, vf24, vf08 :e
  c->acc.vf.mula_xyzw(c->vf_src(vf24).vf, c->vf_src(vf08).vf.z());   c->vfs[vf13].vf.move(Mask::xyzw, c->vf_src(vf12).vf);
  // move.xyzw vf12, vf11       |  maddaz.xyzw ACC, vf25, vf09
  c->acc.vf.madda_xyzw(c->vfs[vf25].vf, c->vfs[vf09].vf.z());   c->vfs[vf12].vf.move(Mask::xyzw, c->vf_src(vf11).vf);
  return;
}

void vcallms_311_case_353(ExecutionContext* c, u16* vis) {
  bool bc;
  // mtir vi11, vf01.x          |  maddz.xyzw vf11, vf26, vf10
  c->acc.vf.madd_xyzw(c->vfs[vf11].vf, c->vf_src(vf26).vf, c->vf_src(vf10).vf.z());   vis[vi11] = c->vf_src(vf01).vf.x_as_u16();
  // jr vi01                    |  mul.xyzw vf14, vf13, Q
  c->vfs[vf14].vf.mul_xyzw(c->vf_src(vf13).vf, c->Q);
  // sqi.xyzw vf04, vi08        |  mulaw.xyzw ACC, vf20, vf08
  c->acc.vf.mula_xyzw(c->vf_src(vf20).vf, c->vf_src(vf08).vf.w());   sq_xyzw(c->vf_src(vf04).vf, vis[vi08]++);
  // rsqrt Q, vf00.w, vf16.x    |  maddaw.xyzw ACC, vf21, vf09
  c->acc.vf.madda_xyzw(c->vfs[vf21].vf, c->vfs[vf09].vf.w());   c->Q = c->vf_src(vf00).vf.w() / std::sqrt(c->vf_src(vf16).vf.x());
  // mtir vi12, vf01.y          |  maddaw.xyzw ACC, vf22, vf10
  c->acc.vf.madda_xyzw(c->vfs[vf22].vf, c->vfs[vf10].vf.w());   vis[vi12] = c->vf_src(vf01).vf.y_as_u16();
  // iand vi11, vi11, vi09      |  maddw.xyzw vf15, vf23, vf00
  c->acc.vf.madd_xyzw(c->vfs[vf15].vf, c->vf_src(vf23).vf, c->vf_src(vf00).vf.w());   vis[vi11] = vis[vi11] & vis[vi09];
  // lq.xyzw vf19, 4(vi11)      |  mul.xyzw vf16, vf11, vf11
  c->vfs[vf16].vf.mul_xyzw(c->vf_src(vf11).vf, c->vf_src(vf11).vf);   lq_xyzw(c->vfs[vf19].vf, vis[vi11] + 4);
  // BRANCH!
  // ibeq vi09, vi11, L7        |  add.xyzw vf08, vf01, vf05
  c->vfs[vf08].vf.add_xyzw(c->vf_src(vf01).vf, c->vf_src(vf05).vf);   bc = (vis[vi09] == vis[vi11]);
  // iand vi12, vi12, vi09      |  add.xyzw vf09, vf02, vf06
  c->vfs[vf09].vf.add_xyzw(c->vf_src(vf02).vf, c->vf_src(vf06).vf);   vis[vi12] = vis[vi12] & vis[vi09];
  if (bc) { goto L7; }

  // lq.xyzw vf24, 4(vi12)      |  add.xyzw vf10, vf03, vf07
  c->vfs[vf10].vf.add_xyzw(c->vf_src(vf03).vf, c->vf_src(vf07).vf);   lq_xyzw(c->vfs[vf24].vf, vis[vi12] + 4);
  // lq.xyzw vf27, 5(vi11)      |  adday.xyzw vf16, vf16
  c->acc.vf.adda(Mask::xyzw, c->vfs[vf16].vf, c->vfs[vf16].vf.y());   lq_xyzw(c->vfs[vf27].vf, vis[vi11] + 5);
  // lq.xyzw vf25, 5(vi12)      |  maddz.xyzw vf16, vf17, vf16
  c->acc.vf.madd_xyzw(c->vfs[vf16].vf, c->vf_src(vf17).vf, c->vf_src(vf16).vf.z());   lq_xyzw(c->vfs[vf25].vf, vis[vi12] + 5);
  // lq.xyzw vf28, 6(vi11)      |  mulaz.xyzw ACC, vf19, vf18
  c->acc.vf.mula_xyzw(c->vf_src(vf19).vf, c->vf_src(vf18).vf.z());   lq_xyzw(c->vfs[vf28].vf, vis[vi11] + 6);
  // lq.xyzw vf26, 6(vi12)      |  maddw.xyzw vf24, vf24, vf18
  c->acc.vf.madd_xyzw(c->vfs[vf24].vf, c->vf_src(vf24).vf, c->vf_src(vf18).vf.w());   lq_xyzw(c->vfs[vf26].vf, vis[vi12] + 6);
  // lq.xyzw vf29, 0(vi11)      |  mulaz.xyzw ACC, vf27, vf18
  c->acc.vf.mula_xyzw(c->vf_src(vf27).vf, c->vf_src(vf18).vf.z());   lq_xyzw(c->vfs[vf29].vf, vis[vi11]);
  // lq.xyzw vf20, 0(vi12)      |  maddw.xyzw vf25, vf25, vf18
  c->acc.vf.madd_xyzw(c->vfs[vf25].vf, c->vf_src(vf25).vf, c->vf_src(vf18).vf.w());   lq_xyzw(c->vfs[vf20].vf, vis[vi12]);
  // lq.xyzw vf19, 1(vi11)      |  mulaz.xyzw ACC, vf28, vf18
  c->acc.vf.mula_xyzw(c->vf_src(vf28).vf, c->vf_src(vf18).vf.z());   lq_xyzw(c->vfs[vf19].vf, vis[vi11] + 1);
  // lq.xyzw vf21, 1(vi12)      |  maddw.xyzw vf26, vf26, vf18
  c->acc.vf.madd_xyzw(c->vfs[vf26].vf, c->vf_src(vf26).vf, c->vf_src(vf18).vf.w());   lq_xyzw(c->vfs[vf21].vf, vis[vi12] + 1);
  // lq.xyzw vf27, 2(vi11)      |  mulaz.xyzw ACC, vf29, vf18
  c->acc.vf.mula_xyzw(c->vf_src(vf29).vf, c->vf_src(vf18).vf.z());   lq_xyzw(c->vfs[vf27].vf, vis[vi11] + 2);
  // lq.xyzw vf22, 2(vi12)      |  maddw.xyzw vf20, vf20, vf18
  c->acc.vf.madd_xyzw(c->vfs[vf20].vf, c->vf_src(vf20).vf, c->vf_src(vf18).vf.w());   lq_xyzw(c->vfs[vf22].vf, vis[vi12] + 2);
  // lq.xyzw vf28, 3(vi11)      |  mulaz.xyzw ACC, vf19, vf18
  c->acc.vf.mula_xyzw(c->vf_src(vf19).vf, c->vf_src(vf18).vf.z());   lq_xyzw(c->vfs[vf28].vf, vis[vi11] + 3);
  // lq.xyzw vf23, 3(vi12)      |  maddw.xyzw vf21, vf21, vf18
  c->acc.vf.madd_xyzw(c->vfs[vf21].vf, c->vf_src(vf21).vf, c->vf_src(vf18).vf.w());   lq_xyzw(c->vfs[vf23].vf, vis[vi12] + 3);
  // lqi.xyzw vf29, vi10        |  mulaz.xyzw ACC, vf27, vf18
  c->acc.vf.mula_xyzw(c->vf_src(vf27).vf, c->vf_src(vf18).vf.z());   lq_xyzw(c->vfs[vf29].vf, vis[vi10]++);
  // iaddiu vi01, vi00, 0x146   |  maddw.xyzw vf22, vf22, vf18
  c->acc.vf.madd_xyzw(c->vfs[vf22].vf, c->vf_src(vf22).vf, c->vf_src(vf18).vf.w());   vis[vi01] = 0x146; /* 326 */
  // BRANCH!
  // ibne vi08, vi04, L6        |  mulaz.xyzw ACC, vf28, vf18
  c->acc.vf.mula_xyzw(c->vf_src(vf28).vf, c->vf_src(vf18).vf.z());   bc = (vis[vi08] != vis[vi04]);
  // nop                        |  maddw.xyzw vf23, vf23, vf18
  c->acc.vf.madd_xyzw(c->vfs[vf23].vf, c->vf_src(vf23).vf, c->vf_src(vf18).vf.w());
  if (bc) { goto L6; }

  // ior vi01, vi05, vi00       |  nop
  vis[vi01] = vis[vi05];
  L6:
  // mtir vi13, vf29.w          |  itof0.xyzw vf18, vf29
  c->vfs[vf18].vf.itof0(Mask::xyzw, c->vf_src(vf29).vf);   vis[vi13] = c->vf_src(vf29).vf.w_as_u16();
  // move.xyzw vf13, vf12       |  mulaz.xyzw ACC, vf24, vf08 :e
  c->acc.vf.mula_xyzw(c->vf_src(vf24).vf, c->vf_src(vf08).vf.z());   c->vfs[vf13].vf.move(Mask::xyzw, c->vf_src(vf12).vf);
  // move.xyzw vf12, vf11       |  maddaz.xyzw ACC, vf25, vf09
  c->acc.vf.madda_xyzw(c->vfs[vf25].vf, c->vfs[vf09].vf.z());   c->vfs[vf12].vf.move(Mask::xyzw, c->vf_src(vf11).vf);
  return;
  L7:
  // nop                        |  add.xyzw vf10, vf03, vf07
  c->vfs[vf10].vf.add_xyzw(c->vf_src(vf03).vf, c->vf_src(vf07).vf);
  // BRANCH!
  // ibne vi08, vi04, L8        |  adday.xyzw vf16, vf16
  c->acc.vf.adda(Mask::xyzw, c->vfs[vf16].vf, c->vfs[vf16].vf.y());   bc = (vis[vi08] != vis[vi04]);
  // nop                        |  maddz.xyzw vf16, vf17, vf16
  c->acc.vf.madd_xyzw(c->vfs[vf16].vf, c->vf_src(vf17).vf, c->vf_src(vf16).vf.z());
  if (bc) { goto L8; }

  // ior vi01, vi05, vi00       |  nop
  vis[vi01] = vis[vi05];
  L8:
  // move.xyzw vf13, vf12       |  mulaz.xyzw ACC, vf24, vf08 :e
  c->acc.vf.mula_xyzw(c->vf_src(vf24).vf, c->vf_src(vf08).vf.z());   c->vfs[vf13].vf.move(Mask::xyzw, c->vf_src(vf12).vf);
  // move.xyzw vf12, vf11       |  maddaz.xyzw ACC, vf25, vf09
  c->acc.vf.madda_xyzw(c->vfs[vf25].vf, c->vfs[vf09].vf.z());   c->vfs[vf12].vf.move(Mask::xyzw, c->vf_src(vf11).vf);
  return;
}

void vcallms_311_case_386(ExecutionContext* c, u16* vis) {
  bool bc;
  // mtir vi11, vf01.x          |  maddz.xyzw vf11, vf26, vf10
  c->acc.vf.madd_xyzw(c->vfs[vf11].vf, c->vf_src(vf26).vf, c->vf_src(vf10).vf.z());   vis[vi11] = c->vf_src(vf01).vf.x_as_u16();
  // jr vi01                    |  mul.xyzw vf14, vf13, Q
  c->vfs[vf14].vf.mul_xyzw(c->vf_src(vf13).vf, c->Q);
  // sqi.xyzw vf04, vi08        |  mulaw.xyzw ACC, vf20, vf08
  c->acc.vf.mula_xyzw(c->vf_src(vf20).vf, c->vf_src(vf08).vf.w());   sq_xyzw(c->vf_src(vf04).vf, vis[vi08]++);
  // rsqrt Q, vf00.w, vf16.x    |  maddaw.xyzw ACC, vf21, vf09
  c->acc.vf.madda_xyzw(c->vfs[vf21].vf, c->vfs[vf09].vf.w());   c->Q = c->vf_src(vf00).vf.w() / std::sqrt(c->vf_src(vf16).vf.x());
  // mtir vi12, vf01.y          |  maddaw.xyzw ACC, vf22, vf10
  c->acc.vf.madda_xyzw(c->vfs[vf22].vf, c->vfs[vf10].vf.w());   vis[vi12] = c->vf_src(vf01).vf.y_as_u16();
  // iand vi11, vi11, vi09      |  maddw.xyzw vf15, vf23, vf00
  c->acc.vf.madd_xyzw(c->vfs[vf15].vf, c->vf_src(vf23).vf, c->vf_src(vf00).vf.w());   vis[vi11] = vis[vi11] & vis[vi09];
  // lq.xyzw vf19, 4(vi11)      |  mul.xyzw vf16, vf11, vf11
  c->vfs[vf16].vf.mul_xyzw(c->vf_src(vf11).vf, c->vf_src(vf11).vf);   lq_xyzw(c->vfs[vf19].vf, vis[vi11] + 4);
  // BRANCH!
  // ibeq vi09, vi11, L10       |  add.xyzw vf08, vf01, vf05
  c->vfs[vf08].vf.add_xyzw(c->vf_src(vf01).vf, c->vf_src(vf05).vf);   bc = (vis[vi09] == vis[vi11]);
  // iand vi12, vi12, vi09      |  add.xyzw vf09, vf02, vf06
  c->vfs[vf09].vf.add_xyzw(c->vf_src(vf02).vf, c->vf_src(vf06).vf);   vis[vi12] = vis[vi12] & vis[vi09];
  if (bc) { goto L10; }

  // nop                        |  muly.xyzw vf18, vf18, vf17
  c->vfs[vf18].vf.mul_xyzw(c->vf_src(vf18).vf, c->vf_src(vf17).vf.y());
  // lq.xyzw vf27, 4(vi12)      |  add.xyzw vf10, vf03, vf07
  c->vfs[vf10].vf.add_xyzw(c->vf_src(vf03).vf, c->vf_src(vf07).vf);   lq_xyzw(c->vfs[vf27].vf, vis[vi12] + 4);
  // lq.xyzw vf24, 4(vi13)      |  adday.xyzw vf16, vf16
  c->acc.vf.adda(Mask::xyzw, c->vfs[vf16].vf, c->vfs[vf16].vf.y());   lq_xyzw(c->vfs[vf24].vf, vis[vi13] + 4);
  // lq.xyzw vf28, 5(vi11)      |  maddz.xyzw vf16, vf17, vf16
  c->acc.vf.madd_xyzw(c->vfs[vf16].vf, c->vf_src(vf17).vf, c->vf_src(vf16).vf.z());   lq_xyzw(c->vfs[vf28].vf, vis[vi11] + 5);
  // lq.xyzw vf19, 5(vi12)      |  mulax.xyzw ACC, vf19, vf18
  c->acc.vf.mula_xyzw(c->vf_src(vf19).vf, c->vf_src(vf18).vf.x());   lq_xyzw(c->vfs[vf19].vf, vis[vi12] + 5);
  // lq.xyzw vf25, 5(vi13)      |  madday.xyzw ACC, vf27, vf18
  c->acc.vf.madda_xyzw(c->vfs[vf27].vf, c->vfs[vf18].vf.y());   lq_xyzw(c->vfs[vf25].vf, vis[vi13] + 5);
  // lq.xyzw vf27, 6(vi11)      |  maddz.xyzw vf24, vf24, vf18
  c->acc.vf.madd_xyzw(c->vfs[vf24].vf, c->vf_src(vf24).vf, c->vf_src(vf18).vf.z());   lq_xyzw(c->vfs[vf27].vf, vis[vi11] + 6);
  // lq.xyzw vf28, 6(vi12)      |  mulax.xyzw ACC, vf28, vf18
  c->acc.vf.mula_xyzw(c->vf_src(vf28).vf, c->vf_src(vf18).vf.x());   lq_xyzw(c->vfs[vf28].vf, vis[vi12] + 6);
  // lq.xyzw vf26, 6(vi13)      |  madday.xyzw ACC, vf19, vf18
  c->acc.vf.madda_xyzw(c->vfs[vf19].vf, c->vfs[vf18].vf.y());   lq_xyzw(c->vfs[vf26].vf, vis[vi13] + 6);
  // lq.xyzw vf19, 0(vi11)      |  maddz.xyzw vf25, vf25, vf18
  c->acc.vf.madd_xyzw(c->vfs[vf25].vf, c->vf_src(vf25).vf, c->vf_src(vf18).vf.z());   lq_xyzw(c->vfs[vf19].vf, vis[vi11]);
  // lq.xyzw vf27, 0(vi12)      |  mulax.xyzw ACC, vf27, vf18
  c->acc.vf.mula_xyzw(c->vf_src(vf27).vf, c->vf_src(vf18).vf.x());   lq_xyzw(c->vfs[vf27].vf, vis[vi12]);
  // lq.xyzw vf20, 0(vi13)      |  madday.xyzw ACC, vf28, vf18
  c->acc.vf.madda_xyzw(c->vfs[vf28].vf, c->vfs[vf18].vf.y());   lq_xyzw(c->vfs[vf20].vf, vis[vi13]);
  // lq.xyzw vf28, 1(vi11)      |  maddz.xyzw vf26, vf26, vf18
  c->acc.vf.madd_xyzw(c->vfs[vf26].vf, c->vf_src(vf26).vf, c->vf_src(vf18).vf.z());   lq_xyzw(c->vfs[vf28].vf, vis[vi11] + 1);
  // lq.xyzw vf19, 1(vi12)      |  mulax.xyzw ACC, vf19, vf18
  c->acc.vf.mula_xyzw(c->vf_src(vf19).vf, c->vf_src(vf18).vf.x());   lq_xyzw(c->vfs[vf19].vf, vis[vi12] + 1);
  // lq.xyzw vf21, 1(vi13)      |  madday.xyzw ACC, vf27, vf18
  c->acc.vf.madda_xyzw(c->vfs[vf27].vf, c->vfs[vf18].vf.y());   lq_xyzw(c->vfs[vf21].vf, vis[vi13] + 1);
  // lq.xyzw vf27, 2(vi11)      |  maddz.xyzw vf20, vf20, vf18
  c->acc.vf.madd_xyzw(c->vfs[vf20].vf, c->vf_src(vf20).vf, c->vf_src(vf18).vf.z());   lq_xyzw(c->vfs[vf27].vf, vis[vi11] + 2);
  // lq.xyzw vf28, 2(vi12)      |  mulax.xyzw ACC, vf28, vf18
  c->acc.vf.mula_xyzw(c->vf_src(vf28).vf, c->vf_src(vf18).vf.x());   lq_xyzw(c->vfs[vf28].vf, vis[vi12] + 2);
  // lq.xyzw vf22, 2(vi13)      |  madday.xyzw ACC, vf19, vf18
  c->acc.vf.madda_xyzw(c->vfs[vf19].vf, c->vfs[vf18].vf.y());   lq_xyzw(c->vfs[vf22].vf, vis[vi13] + 2);
  // lq.xyzw vf19, 3(vi11)      |  maddz.xyzw vf21, vf21, vf18
  c->acc.vf.madd_xyzw(c->vfs[vf21].vf, c->vf_src(vf21).vf, c->vf_src(vf18).vf.z());   lq_xyzw(c->vfs[vf19].vf, vis[vi11] + 3);
  // lq.xyzw vf27, 3(vi12)      |  mulax.xyzw ACC, vf27, vf18
  c->acc.vf.mula_xyzw(c->vf_src(vf27).vf, c->vf_src(vf18).vf.x());   lq_xyzw(c->vfs[vf27].vf, vis[vi12] + 3);
  // lq.xyzw vf23, 3(vi13)      |  madday.xyzw ACC, vf28, vf18
  c->acc.vf.madda_xyzw(c->vfs[vf28].vf, c->vfs[vf18].vf.y());   lq_xyzw(c->vfs[vf23].vf, vis[vi13] + 3);
  // lqi.xyzw vf29, vi10        |  maddz.xyzw vf22, vf22, vf18
  c->acc.vf.madd_xyzw(c->vfs[vf22].vf, c->vf_src(vf22).vf, c->vf_src(vf18).vf.z());   lq_xyzw(c->vfs[vf29].vf, vis[vi10]++);
  // BRANCH!
  // ibne vi08, vi06, L9        |  mulax.xyzw ACC, vf19, vf18
  c->acc.vf.mula_xyzw(c->vf_src(vf19).vf, c->vf_src(vf18).vf.x());   bc = (vis[vi08] != vis[vi06]);
  // nop                        |  madday.xyzw ACC, vf27, vf18
  c->acc.vf.madda_xyzw(c->vfs[vf27].vf, c->vfs[vf18].vf.y());
  if (bc) { goto L9; }

  // iaddiu vi01, vi00, 0x1ab   |  nop
  vis[vi01] = 0x1ab; /* 427 */
  L9:
  // nop                        |  maddz.xyzw vf23, vf23, vf18
  c->acc.vf.madd_xyzw(c->vfs[vf23].vf, c->vf_src(vf23).vf, c->vf_src(vf18).vf.z());
  // mtir vi13, vf29.w          |  itof0.xyz vf18, vf29
  c->vfs[vf18].vf.itof0(Mask::xyz, c->vf_src(vf29).vf);   vis[vi13] = c->vf_src(vf29).vf.w_as_u16();
  // move.xyzw vf13, vf12       |  mulaz.xyzw ACC, vf24, vf08 :e
  c->acc.vf.mula_xyzw(c->vf_src(vf24).vf, c->vf_src(vf08).vf.z());   c->vfs[vf13].vf.move(Mask::xyzw, c->vf_src(vf12).vf);
  // move.xyzw vf12, vf11       |  maddaz.xyzw ACC, vf25, vf09
  c->acc.vf.madda_xyzw(c->vfs[vf25].vf, c->vfs[vf09].vf.z());   c->vfs[vf12].vf.move(Mask::xyzw, c->vf_src(vf11).vf);
  return;
  L10:
  // nop                        |  add.xyzw vf10, vf03, vf07
  c->vfs[vf10].vf.add_xyzw(c->vf_src(vf03).vf, c->vf_src(vf07).vf);
  // BRANCH!
  // ibne vi08, vi06, L11       |  adday.xyzw vf16, vf16
  c->acc.vf.adda(Mask::xyzw, c->vfs[vf16].vf, c->vfs[vf16].vf.y());   bc = (vis[vi08] != vis[vi06]);
  // nop                        |  maddz.xyzw vf16, vf17, vf16
  c->acc.vf.madd_xyzw(c->vfs[vf16].vf, c->vf_src(vf17).vf, c->vf_src(vf16).vf.z());
  if (bc) { goto L11; }

  // iaddiu vi01, vi00, 0x1ab   |  nop
  vis[vi01] = 0x1ab; /* 427 */
  L11:
  // move.xyzw vf13, vf12       |  mulaz.xyzw ACC, vf24, vf08 :e
  c->acc.vf.mula_xyzw(c->vf_src(vf24).vf, c->vf_src(vf08).vf.z());   c->vfs[vf13].vf.move(Mask::xyzw, c->vf_src(vf12).vf);
  // move.xyzw vf12, vf11       |  maddaz.xyzw ACC, vf25, vf09
  c->acc.vf.madda_xyzw(c->vfs[vf25].vf, c->vfs[vf09].vf.z());   c->vfs[vf12].vf.move(Mask::xyzw, c->vf_src(vf11).vf);
  return;
}

void vcallms_311_case_427(ExecutionContext* c, u16* vis) {
  // mtir vi11, vf01.x          |  maddz.xyzw vf11, vf26, vf10
  c->acc.vf.madd_xyzw(c->vfs[vf11].vf, c->vf_src(vf26).vf, c->vf_src(vf10).vf.z());   vis[vi11] = c->vf_src(vf01).vf.x_as_u16();
  // jr vi01                    |  mul.xyzw vf14, vf13, Q
  c->vfs[vf14].vf.mul_xyzw(c->vf_src(vf13).vf, c->Q);
  // sqi.xyzw vf04, vi08        |  mulaw.xyzw ACC, vf20, vf08
  c->acc.vf.mula_xyzw(c->vf_src(vf20).vf, c->vf_src(vf08).vf.w());   sq_xyzw(c->vf_src(vf04).vf, vis[vi08]++);
  c->acc.vf.madda_xyzw(c->vfs[vf21].vf, c->vfs[vf09].vf.w());   c->Q = c->vf_src(vf00).vf.w() / std::sqrt(c->vf_src(vf16).vf.x());
  // nop                        |  maddaw.xyzw ACC, vf22, vf10
  c->acc.vf.madda_xyzw(c->vfs[vf22].vf, c->vfs[vf10].vf.w());
  // nop                        |  maddw.xyzw vf15, vf23, vf00
  c->acc.vf.madd_xyzw(c->vfs[vf15].vf, c->vf_src(vf23).vf, c->vf_src(vf00).vf.w());
  // nop                        |  mul.xyzw vf16, vf11, vf11
  c->vfs[vf16].vf.mul_xyzw(c->vf_src(vf11).vf, c->vf_src(vf11).vf);
  // nop                        |  add.xyzw vf08, vf01, vf05
  c->vfs[vf08].vf.add_xyzw(c->vf_src(vf01).vf, c->vf_src(vf05).vf);
  // nop                        |  add.xyzw vf09, vf02, vf06
  c->vfs[vf09].vf.add_xyzw(c->vf_src(vf02).vf, c->vf_src(vf06).vf);
  // nop                        |  add.xyzw vf10, vf03, vf07
  c->vfs[vf10].vf.add_xyzw(c->vf_src(vf03).vf, c->vf_src(vf07).vf);
  // nop                        |  adday.xyzw vf16, vf16
  c->acc.vf.adda(Mask::xyzw, c->vfs[vf16].vf, c->vfs[vf16].vf.y());
  // nop                        |  maddz.xyzw vf16, vf17, vf16
  c->acc.vf.madd_xyzw(c->vfs[vf16].vf, c->vf_src(vf17).vf, c->vf_src(vf16).vf.z());
  // move.xyzw vf13, vf12       |  mulaz.xyzw ACC, vf24, vf08 :e
  c->acc.vf.mula_xyzw(c->vf_src(vf24).vf, c->vf_src(vf08).vf.z());   c->vfs[vf13].vf.move(Mask::xyzw, c->vf_src(vf12).vf);
  // move.xyzw vf12, vf11       |  maddaz.xyzw ACC, vf25, vf09
  c->acc.vf.madda_xyzw(c->vfs[vf25].vf, c->vfs[vf09].vf.z());   c->vfs[vf12].vf.move(Mask::xyzw, c->vf_src(vf11).vf);
  return;
}

void vcallms_311(ExecutionContext* c, u16* vis) {
  switch(vis[vi01]) {
    case 314:
      vcallms_311_case_314(c, vis);
      break;
    case 326:
      vcallms_311_case_326(c, vis);
      break;
    case 353:
      vcallms_311_case_353(c, vis);
      break;
    case 386:
      vcallms_311_case_386(c, vis);
      break;
    case 427:
      vcallms_311_case_427(c, vis);
      break;
    default:
      ASSERT_MSG(false, fmt::format("BAD JUMP {}", vis[vi01]));
  }
}

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u16 vis[16];
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->sq(s0, 7536, at);                              // sq s0, 7536(at)
  c->sq(s1, 7552, at);                              // sq s1, 7552(at)
  c->sq(s2, 7568, at);                              // sq s2, 7568(at)
  c->sq(s3, 7584, at);                              // sq s3, 7584(at)
  c->sq(s4, 7600, at);                              // sq s4, 7600(at)
  c->sq(s5, 7616, at);                              // sq s5, 7616(at)
  c->sq(s6, 7632, at);                              // sq s6, 7632(at)
  c->sq(t8, 7648, at);                              // sq t8, 7648(at)
  c->sq(t9, 7664, at);                              // sq t9, 7664(at)
  c->sq(gp, 7680, at);                              // sq gp, 7680(at)
  c->sq(sp, 7696, at);                              // sq sp, 7696(at)
  c->sq(fp, 7712, at);                              // sq fp, 7712(at)
  c->sq(ra, 7728, at);                              // sq ra, 7728(at)
  // nop                                            // sll r0, r0, 0
  get_fake_spad_addr2(t4, cache.fake_scratchpad_data, 0, c);// lui t4, 28672
  c->addiu(a0, r0, 0);                              // addiu a0, r0, 0
  c->lbu(a1, 6955, t4);                             // lbu a1, 6955(t4)
  c->addiu(a2, r0, 4);                              // addiu a2, r0, 4
  c->lw(t0, 7444, t4);                              // lw t0, 7444(t4)
  c->addiu(a3, r0, 1);                              // addiu a3, r0, 1
  c->lw(v1, 7448, t4);                              // lw v1, 7448(t4)
  c->and_(a3, a1, a3);                              // and a3, a1, a3
  c->lw(a1, 24, t4);                                // lw a1, 24(t4)
  c->movn(a0, a2, a3);                              // movn a0, a2, a3
  c->lbu(a3, 6855, t4);                             // lbu a3, 6855(t4)
  c->daddu(t1, a0, t4);                             // daddu t1, a0, t4
  c->lbu(t2, 6852, t4);                             // lbu t2, 6852(t4)
  c->daddiu(a2, a1, 128);                           // daddiu a2, a1, 128
  c->lw(a0, 7428, t4);                              // lw a0, 7428(t4)
  c->dsll(t3, a3, 3);                               // dsll t3, a3, 3
  c->lbu(a3, 10, t0);                               // lbu a3, 10(t0)
  c->dsll(t5, t2, 1);                               // dsll t5, t2, 1
  c->lbu(t2, 9, t0);                                // lbu t2, 9(t0)
  c->daddu(t1, t1, t5);                             // daddu t1, t1, t5
  // nop                                            // sll r0, r0, 0
  c->daddu(t3, t1, t3);                             // daddu t3, t1, t3
  c->lbu(t1, 11953, t3);                            // lbu t1, 11953(t3)
  // nop                                            // sll r0, r0, 0
  c->lbu(t5, 11952, t3);                            // lbu t5, 11952(t3)
  // nop                                            // sll r0, r0, 0
  c->lw(t3, 7432, t4);                              // lw t3, 7432(t4)
  c->sll(t6, a3, 4);                                // sll t6, a3, 4
  c->daddu(t5, t5, t4);                             // daddu t5, t5, t4
  c->daddu(t7, t1, t4);                             // daddu t7, t1, t4
  c->lw(a3, 7436, t4);                              // lw a3, 7436(t4)
  c->daddu(t1, t6, t0);                             // daddu t1, t6, t0
  c->sw(t1, 7452, t4);                              // sw t1, 7452(t4)
  // nop                                            // sll r0, r0, 0
  c->lbu(t0, 12, t1);                               // lbu t0, 12(t1)
  // nop                                            // sll r0, r0, 0
  c->lq(t7, 11888, t7);                             // lq t7, 11888(t7)
  // nop                                            // sll r0, r0, 0
  c->lq(s0, 11888, t5);                             // lq s0, 11888(t5)
  // nop                                            // sll r0, r0, 0
  c->sq(t7, 96, a1);                                // sq t7, 96(a1)
  c->daddiu(t5, v1, 5504);                          // daddiu t5, v1, 5504
  c->sq(s0, 112, a1);                               // sq s0, 112(a1)
  c->daddiu(v1, v1, 6048);                          // daddiu v1, v1, 6048
  c->lbu(ra, 6857, t4);                             // lbu ra, 6857(t4)
  c->movn(v1, t5, t2);                              // movn v1, t5, t2
  c->sw(r0, 124, a1);                               // sw r0, 124(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(t7, 11744, t4);                             // lq t7, 11744(t4)
  c->daddiu(t6, t1, 16);                            // daddiu t6, t1, 16
  c->lq(s0, 11808, t4);                             // lq s0, 11808(t4)
  c->addiu(t9, r0, 0);                              // addiu t9, r0, 0
  c->lq(s4, 11824, t4);                             // lq s4, 11824(t4)
  c->addiu(t5, r0, 0);                              // addiu t5, r0, 0
  c->lq(gp, 11840, t4);                             // lq gp, 11840(t4)
  c->addiu(t1, r0, -3);                             // addiu t1, r0, -3
  c->lq(s5, 11856, t4);                             // lq s5, 11856(t4)
  c->addiu(t8, r0, 6);                              // addiu t8, r0, 6
  c->lq(t4, 11872, t4);                             // lq t4, 11872(t4)
  c->andi(s3, ra, 4);                               // andi s3, ra, 4
  c->sq(t7, 0, a1);                                 // sq t7, 0(a1)
  c->movz(t8, r0, t2);                              // movz t8, r0, t2
  c->sq(s0, 16, a1);                                // sq s0, 16(a1)
  c->addiu(ra, r0, 0);                              // addiu ra, r0, 0
  c->sq(s4, 32, a1);                                // sq s4, 32(a1)
  c->addiu(t7, r0, -32768);                         // addiu t7, r0, -32768
  c->sq(gp, 48, a1);                                // sq gp, 48(a1)
  c->movz(ra, t7, t0);                              // movz ra, t7, t0
  c->sq(s5, 64, a1);                                // sq s5, 64(a1)
  c->dsll(t7, s3, 18);                              // dsll t7, s3, 18
  c->pextlw(t7, t7, t7);                            // pextlw t7, t7, t7
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // Unknown instr: pnor t7, r0, t7
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pand(t4, t4, t7);                              // pand t4, t4, t7
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  bc = c->sgpr64(t2) == 0;                          // beq t2, r0, L84
  c->sq(t4, 80, a1);                                // sq t4, 80(a1)
  if (bc) {goto block_4;}                           // branch non-likely

  c->lq(t7, 2944, t3);                              // lq t7, 2944(t3)
  c->daddu(ra, t2, ra);                             // daddu ra, t2, ra
  c->lq(s0, 2960, t3);                              // lq s0, 2960(t3)
  c->addiu(gp, r0, -6);                             // addiu gp, r0, -6
  c->lq(t4, 2976, t3);                              // lq t4, 2976(t3)
  c->daddiu(t0, t0, 1);                             // daddiu t0, t0, 1
  c->lq(t2, 2992, t3);                              // lq t2, 2992(t3)
  c->daddiu(s5, t0, -3);                            // daddiu s5, t0, -3
  bc = ((s64)c->sgpr64(s5)) <= 0;                   // blez s5, L86
  c->lq(t3, 3008, t3);                              // lq t3, 3008(t3)
  if (bc) {goto block_8;}                           // branch non-likely

  c->daddiu(t9, t0, -5);                            // daddiu t9, t0, -5
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(t9)) <= 0;                   // blez t9, L86
  c->lh(t9, 172, t6);                               // lh t9, 172(t6)
  if (bc) {goto block_8;}                           // branch non-likely

  //beq r0, r0, L86                                 // beq r0, r0, L86
  c->lh(t5, 332, t6);                               // lh t5, 332(t6)
  goto block_8;                                     // branch always


  block_4:
  c->daddiu(t2, t0, -3);                            // daddiu t2, t0, -3
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(t2)) <= 0;                   // blez t2, L85
  c->daddiu(t2, t0, -5);                            // daddiu t2, t0, -5
  if (bc) {goto block_7;}                           // branch non-likely

  bc = ((s64)c->sgpr64(t2)) <= 0;                   // blez t2, L85
  c->lh(t9, 252, t6);                               // lh t9, 252(t6)
  if (bc) {goto block_7;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->lh(t5, 412, t6);                               // lh t5, 412(t6)

  block_7:
  c->lh(gp, 12, t6);                                // lh gp, 12(t6)
  // nop                                            // sll r0, r0, 0
  c->lh(ra, 28, t6);                                // lh ra, 28(t6)
  // nop                                            // sll r0, r0, 0
  c->lq(t7, 0, t6);                                 // lq t7, 0(t6)
  // nop                                            // sll r0, r0, 0
  c->lq(s0, 16, t6);                                // lq s0, 16(t6)
  // nop                                            // sll r0, r0, 0
  c->lq(t4, 32, t6);                                // lq t4, 32(t6)
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 48, t6);                                // lq t2, 48(t6)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 64, t6);                                // lq t3, 64(t6)
  c->daddiu(t6, t6, 80);                            // daddiu t6, t6, 80

  block_8:
  c->sq(t7, 0, a2);                                 // sq t7, 0(a2)
  c->daddu(gp, gp, t8);                             // daddu gp, gp, t8
  c->sq(s0, 16, a2);                                // sq s0, 16(a2)
  c->daddiu(t8, t8, 2);                             // daddiu t8, t8, 2
  c->sq(t4, 32, a2);                                // sq t4, 32(a2)
  c->daddu(t1, t1, ra);                             // daddu t1, t1, ra
  c->sq(t2, 48, a2);                                // sq t2, 48(a2)
  c->daddiu(t1, t1, 3);                             // daddiu t1, t1, 3
  c->sq(t3, 64, a2);                                // sq t3, 64(a2)
  c->andi(t1, t1, 255);                             // andi t1, t1, 255
  c->sw(gp, 12, a2);                                // sw gp, 12(a2)
  c->daddiu(a2, a2, 80);                            // daddiu a2, a2, 80
  bc = ((s64)c->sgpr64(ra)) > 0;                    // bgtz ra, L85
  c->sw(ra, -52, a2);                               // sw ra, -52(a2)
  if (bc) {goto block_7;}                           // branch non-likely

  c->sw(t1, 108, a1);                               // sw t1, 108(a1)
  c->daddiu(a1, t9, 7);                             // daddiu a1, t9, 7
  c->sq(t7, 2944, a0);                              // sq t7, 2944(a0)
  c->daddiu(t5, t5, 7);                             // daddiu t5, t5, 7
  c->sq(s0, 2960, a0);                              // sq s0, 2960(a0)
  c->sra(a2, a1, 4);                                // sra a2, a1, 4
  c->sq(t4, 2976, a0);                              // sq t4, 2976(a0)
  c->sra(t4, t5, 4);                                // sra t4, t5, 4
  c->sq(t2, 2992, a0);                              // sq t2, 2992(a0)
  c->daddiu(a1, a3, 192);                           // daddiu a1, a3, 192
  c->sq(t3, 3008, a0);                              // sq t3, 3008(a0)
  c->dsll(a0, t0, 2);                               // dsll a0, t0, 2
  c->sh(t1, 18, a3);                                // sh t1, 18(a3)
  c->daddu(a0, a0, t0);                             // daddu a0, a0, t0
  c->sb(t0, 16, a3);                                // sb t0, 16(a3)
  c->daddiu(a0, a0, 7);                             // daddiu a0, a0, 7
  c->dsubu(a0, t4, a2);                             // dsubu a0, t4, a2
  c->lq(t0, 0, v1);                                 // lq t0, 0(v1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(t1, a2, -3);                            // daddiu t1, a2, -3
  c->lq(a3, 16, v1);                                // lq a3, 16(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(t0, 0, a1);                                 // sq t0, 0(a1)
  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L87
  c->lq(a2, 32, v1);                                // lq a2, 32(v1)
  if (bc) {goto block_21;}                          // branch non-likely

  c->daddiu(t0, t1, -1);                            // daddiu t0, t1, -1
  c->sq(a3, 16, a1);                                // sq a3, 16(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L88
  c->lq(a3, 48, v1);                                // lq a3, 48(v1)
  if (bc) {goto block_22;}                          // branch non-likely

  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  c->sq(a2, 32, a1);                                // sq a2, 32(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L89
  c->lq(a2, 64, v1);                                // lq a2, 64(v1)
  if (bc) {goto block_23;}                          // branch non-likely

  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  c->sq(a3, 48, a1);                                // sq a3, 48(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L90
  c->lq(a3, 80, v1);                                // lq a3, 80(v1)
  if (bc) {goto block_24;}                          // branch non-likely

  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  c->sq(a2, 64, a1);                                // sq a2, 64(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L91
  c->lq(a2, 96, v1);                                // lq a2, 96(v1)
  if (bc) {goto block_25;}                          // branch non-likely

  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  c->sq(a3, 80, a1);                                // sq a3, 80(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L92
  c->lq(a3, 112, v1);                               // lq a3, 112(v1)
  if (bc) {goto block_26;}                          // branch non-likely

  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  c->sq(a2, 96, a1);                                // sq a2, 96(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L93
  c->lq(a2, 128, v1);                               // lq a2, 128(v1)
  if (bc) {goto block_27;}                          // branch non-likely

  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  c->sq(a3, 112, a1);                               // sq a3, 112(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L94
  c->lq(a3, 144, v1);                               // lq a3, 144(v1)
  if (bc) {goto block_28;}                          // branch non-likely

  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  c->sq(a2, 128, a1);                               // sq a2, 128(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L95
  c->lq(a2, 160, v1);                               // lq a2, 160(v1)
  if (bc) {goto block_29;}                          // branch non-likely

  c->daddiu(t0, t0, -1);                            // daddiu t0, t0, -1
  c->sq(a3, 144, a1);                               // sq a3, 144(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L96
  c->lq(a3, 176, v1);                               // lq a3, 176(v1)
  if (bc) {goto block_30;}                          // branch non-likely

  c->daddiu(a0, t0, -1);                            // daddiu a0, t0, -1
  c->sq(a2, 160, a1);                               // sq a2, 160(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L97
  c->lq(a0, 192, v1);                               // lq a0, 192(v1)
  if (bc) {goto block_31;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->sq(a3, 176, a1);                               // sq a3, 176(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(v1, 208, v1);                               // lq v1, 208(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(a0, 192, a1);                               // sq a0, 192(a1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sq(v1, 208, a1);                               // sq v1, 208(a1)
  //beq r0, r0, L107                                // beq r0, r0, L107
  // nop                                            // sll r0, r0, 0
  goto block_41;                                    // branch always


  block_21:
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, 16, a1);                                // sq a3, 16(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(a3, 224, v1);                               // lq a3, 224(v1)

  block_22:
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a2, 32, a1);                                // sq a2, 32(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L98
  c->lq(a2, 240, v1);                               // lq a2, 240(v1)
  if (bc) {goto block_32;}                          // branch non-likely


  block_23:
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, 48, a1);                                // sq a3, 48(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L99
  c->lq(a3, 256, v1);                               // lq a3, 256(v1)
  if (bc) {goto block_33;}                          // branch non-likely


  block_24:
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a2, 64, a1);                                // sq a2, 64(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L100
  c->lq(a2, 272, v1);                               // lq a2, 272(v1)
  if (bc) {goto block_34;}                          // branch non-likely


  block_25:
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, 80, a1);                                // sq a3, 80(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L101
  c->lq(a3, 288, v1);                               // lq a3, 288(v1)
  if (bc) {goto block_35;}                          // branch non-likely


  block_26:
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a2, 96, a1);                                // sq a2, 96(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L102
  c->lq(a2, 304, v1);                               // lq a2, 304(v1)
  if (bc) {goto block_36;}                          // branch non-likely


  block_27:
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, 112, a1);                               // sq a3, 112(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L103
  c->lq(a3, 320, v1);                               // lq a3, 320(v1)
  if (bc) {goto block_37;}                          // branch non-likely


  block_28:
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a2, 128, a1);                               // sq a2, 128(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L104
  c->lq(a2, 336, v1);                               // lq a2, 336(v1)
  if (bc) {goto block_38;}                          // branch non-likely


  block_29:
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, 144, a1);                               // sq a3, 144(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L105
  c->lq(a3, 352, v1);                               // lq a3, 352(v1)
  if (bc) {goto block_39;}                          // branch non-likely


  block_30:
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a2, 160, a1);                               // sq a2, 160(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L106
  c->lq(a0, 368, v1);                               // lq a0, 368(v1)
  if (bc) {goto block_40;}                          // branch non-likely


  block_31:
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 176, a1);                               // sq a3, 176(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(v1, 384, v1);                               // lq v1, 384(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(a0, 192, a1);                               // sq a0, 192(a1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sq(v1, 208, a1);                               // sq v1, 208(a1)
  //beq r0, r0, L107                                // beq r0, r0, L107
  // nop                                            // sll r0, r0, 0
  goto block_41;                                    // branch always


  block_32:
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 48, a1);                                // sq a3, 48(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(a3, 400, v1);                               // lq a3, 400(v1)

  block_33:
  // nop                                            // sll r0, r0, 0
  c->sq(a2, 64, a1);                                // sq a2, 64(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(a2, 416, v1);                               // lq a2, 416(v1)

  block_34:
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 80, a1);                                // sq a3, 80(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(a3, 432, v1);                               // lq a3, 432(v1)

  block_35:
  // nop                                            // sll r0, r0, 0
  c->sq(a2, 96, a1);                                // sq a2, 96(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(a2, 448, v1);                               // lq a2, 448(v1)

  block_36:
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 112, a1);                               // sq a3, 112(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(a3, 464, v1);                               // lq a3, 464(v1)

  block_37:
  // nop                                            // sll r0, r0, 0
  c->sq(a2, 128, a1);                               // sq a2, 128(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(a2, 480, v1);                               // lq a2, 480(v1)

  block_38:
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 144, a1);                               // sq a3, 144(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(a3, 496, v1);                               // lq a3, 496(v1)

  block_39:
  // nop                                            // sll r0, r0, 0
  c->sq(a2, 160, a1);                               // sq a2, 160(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(a0, 512, v1);                               // lq a0, 512(v1)

  block_40:
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 176, a1);                               // sq a3, 176(a1)
  // nop                                            // sll r0, r0, 0
  c->lq(v1, 528, v1);                               // lq v1, 528(v1)
  // nop                                            // sll r0, r0, 0
  c->sq(a0, 192, a1);                               // sq a0, 192(a1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sq(v1, 208, a1);                               // sq v1, 208(a1)

  block_41:
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  // nop                                            // sll r0, r0, 0
  c->lw(a1, 7452, v1);                              // lw a1, 7452(v1)
  // nop                                            // sll r0, r0, 0
  c->lw(a0, 7436, v1);                              // lw a0, 7436(v1)
  // nop                                            // sll r0, r0, 0
  c->lbu(a2, 13, a1);                               // lbu a2, 13(a1)
  // nop                                            // sll r0, r0, 0
  c->lbu(v1, 14, a1);                               // lbu v1, 14(a1)
  c->daddiu(a0, a0, 32);                            // daddiu a0, a0, 32
  c->lbu(a3, 15, a1);                               // lbu a3, 15(a1)
  c->daddu(a1, a1, a2);                             // daddu a1, a1, a2
  c->mult3(a3, v1, a3);                             // mult3 a3, v1, a3
  c->mov64(a1, a1);                                 // or a1, a1, r0
  c->mov64(v1, v1);                                 // or v1, v1, r0
  c->mov64(a0, a0);                                 // or a0, a0, r0
  c->addiu(a2, r0, 513);                            // addiu a2, r0, 513
  c->addiu(t0, r0, 257);                            // addiu t0, r0, 257
  c->dsll(t2, a2, 18);                              // dsll t2, a2, 18
  c->dsll(t1, t0, 16);                              // dsll t1, t0, 16
  c->or_(a2, a2, t2);                               // or a2, a2, t2
  c->or_(t0, t0, t1);                               // or t0, t0, t1
  c->dsll32(t2, a2, 4);                             // dsll32 t2, a2, 4
  c->dsll32(t1, t0, 0);                             // dsll32 t1, t0, 0
  c->or_(a2, a2, t2);                               // or a2, a2, t2
  c->or_(t0, t0, t1);                               // or t0, t0, t1
  c->pcpyld(a2, a2, a2);                            // pcpyld a2, a2, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyld(t0, t0, t0);                            // pcpyld t0, t0, t0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->daddu(a3, a3, a1);                             // daddu a3, a3, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->lhu(t1, 0, a1);                                // lhu t1, 0(a1)
  c->daddu(a1, a1, v1);                             // daddu a1, a1, v1
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->pextlb(t1, t1, t1);                            // pextlb t1, t1, t1
  //beq r0, r0, L109                                // beq r0, r0, L109
  c->pextlb(t2, t1, t1);                            // pextlb t2, t1, t1
  goto block_43;                                    // branch always


  block_42:
  c->daddu(a1, a1, v1);                             // daddu a1, a1, v1
  c->daddiu(a0, a0, 32);                            // daddiu a0, a0, 32
  c->pextlb(t2, t2, t2);                            // pextlb t2, t2, t2
  c->sq(t1, -16, a0);                               // sq t1, -16(a0)

  block_43:
  c->pextlb(t1, t2, t2);                            // pextlb t1, t2, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pand(t1, t1, a2);                              // pand t1, t1, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pceqb(t1, t1, a2);                             // pceqb t1, t1, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pand(t1, t1, t0);                              // pand t1, t1, t0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(t2, t1, r0);                            // pextlb t2, t1, r0
  c->lhu(t3, 0, a1);                                // lhu t3, 0(a1)
  c->pextub(t1, t1, r0);                            // pextub t1, t1, r0
  c->sq(t2, 0, a0);                                 // sq t2, 0(a0)
  bc = c->sgpr64(a1) != c->sgpr64(a3);              // bne a1, a3, L108
  c->pextlb(t2, t3, t3);                            // pextlb t2, t3, t3
  if (bc) {goto block_42;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->sq(t1, 16, a0);                                // sq t1, 16(a0)
  get_fake_spad_addr2(s4, cache.fake_scratchpad_data, 0, c);// lui s4, 28672
  // nop                                            // sll r0, r0, 0
  c->lw(a0, 7444, s4);                              // lw a0, 7444(s4)
  // nop                                            // sll r0, r0, 0
  c->lw(a1, 7436, s4);                              // lw a1, 7436(s4)
  // nop                                            // sll r0, r0, 0
  c->lq(t8, 16, a0);                                // lq t8, 16(a0)
  c->daddiu(s6, a0, 48);                            // daddiu s6, a0, 48
  c->lbu(a3, 12, a0);                               // lbu a3, 12(a0)
  // nop                                            // sll r0, r0, 0
  c->lbu(t2, 10, a0);                               // lbu t2, 10(a0)
  c->pextlb(t9, r0, t8);                            // pextlb t9, r0, t8
  vis[10] = c->gpr_src(a3).du16[0];                    // ctc2.ni vi10, a3
  c->pextlh(gp, r0, t9);                            // pextlh gp, r0, t9
  c->mov128_vf_gpr(vf1, gp);                        // qmtc2.ni vf1, gp
  c->pextuh(t9, r0, t9);                            // pextuh t9, r0, t9
  c->mov128_vf_gpr(vf2, t9);                        // qmtc2.ni vf2, t9
  c->pextub(t9, r0, t8);                            // pextub t9, r0, t8
  c->lbu(a3, 4, a0);                                // lbu a3, 4(a0)
  c->pextlh(gp, r0, t9);                            // pextlh gp, r0, t9
  c->mov128_vf_gpr(vf3, gp);                        // qmtc2.ni vf3, gp
  c->pextuh(t9, r0, t9);                            // pextuh t9, r0, t9
  c->mov128_vf_gpr(vf4, t9);                        // qmtc2.ni vf4, t9
  c->sll(t2, t2, 4);                                // sll t2, t2, 4
  c->lbu(t0, 5, a0);                                // lbu t0, 5(a0)
  c->daddu(t2, t2, a0);                             // daddu t2, t2, a0
  c->lbu(t1, 6, a0);                                // lbu t1, 6(a0)
  c->daddiu(s5, a3, -1);                            // daddiu s5, a3, -1
  vis[11] = c->gpr_src(a3).du16[0];                    // ctc2.ni vi11, a3
  c->daddu(s5, s5, t0);                             // daddu s5, s5, t0
  vis[12] = c->gpr_src(t0).du16[0];                    // ctc2.ni vi12, t0
  c->daddu(s5, s5, t1);                             // daddu s5, s5, t1
  vis[13] = c->gpr_src(t1).du16[0];                    // ctc2.ni vi13, t1
  c->daddiu(t4, a1, 32);                            // daddiu t4, a1, 32
  c->lqc2(vf27, 0, t2);                             // lqc2 vf27, 0(t2)
  c->daddiu(t3, a1, 192);                           // daddiu t3, a1, 192
  // Unknown instr: vcallms 280
  vcallms_280(c, vis);
  c->daddiu(v1, a1, 448);                           // daddiu v1, a1, 448
  c->lhu(s1, 6820, s4);                             // lhu s1, 6820(s4)
  c->pextlw(t3, t3, t3);                            // pextlw t3, t3, t3
  c->lhu(s2, 6822, s4);                             // lhu s2, 6822(s4)
  c->pcpyld(t3, t3, t3);                            // pcpyld t3, t3, t3
  c->lqc2(vf19, 6800, s4);                          // lqc2 vf19, 6800(s4)
  c->pcpyh(s1, s1);                                 // pcpyh s1, s1
  c->lbu(v0, 2, a0);                                // lbu v0, 2(a0)
  c->addiu(t5, r0, 0);                              // addiu t5, r0, 0
  c->lbu(at, 1, a0);                                // lbu at, 1(a0)
  c->addiu(s4, r0, -1);                             // addiu s4, r0, -1
  c->lui(a1, 19201);                                // lui a1, 19201
  c->daddiu(v0, v0, 3);                             // daddiu v0, v0, 3
  c->daddiu(a1, a1, 18304);                         // daddiu a1, a1, 18304
  c->andi(v0, v0, 252);                             // andi v0, v0, 252
  c->sll(at, at, 2);                                // sll at, at, 2
  c->sll(v0, v0, 2);                                // sll v0, v0, 2
  c->lq(t8, 32, a0);                                // lq t8, 32(a0)
  c->daddu(v0, v0, a0);                             // daddu v0, v0, a0
  c->lbu(t0, 13, a0);                               // lbu t0, 13(a0)
  c->daddu(at, at, a0);                             // daddu at, at, a0
  c->lbu(t2, 11, a0);                               // lbu t2, 11(a0)
  c->pextlw(a1, a1, r0);                            // pextlw a1, a1, r0
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L112
  c->pcpyld(a1, a1, a1);                            // pcpyld a1, a1, a1
  if (bc) {goto block_49;}                          // branch non-likely

  c->sll(t2, t2, 4);                                // sll t2, t2, 4
  c->daddiu(a3, a0, 14);                            // daddiu a3, a0, 14
  //beq r0, r0, L111                                // beq r0, r0, L111
  c->daddu(t2, t2, a0);                             // daddu t2, t2, a0
  goto block_47;                                    // branch always


  block_46:
  // Unknown instr: vcallms 303
  vcallms_303(c, vis);
  c->mov64(t0, t1);                                 // or t0, t1, r0

  block_47:
  c->lbu(t1, 0, a3);                                // lbu t1, 0(a3)
  c->daddiu(a3, a3, 1);                             // daddiu a3, a3, 1
  c->lqc2(vf23, 64, t2);                            // lqc2 vf23, 64(t2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf24, 80, t2);                            // lqc2 vf24, 80(t2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf25, 96, t2);                            // lqc2 vf25, 96(t2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf26, 112, t2);                           // lqc2 vf26, 112(t2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf20, 16, t2);                            // lqc2 vf20, 16(t2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf21, 32, t2);                            // lqc2 vf21, 32(t2)
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf22, 48, t2);                            // lqc2 vf22, 48(t2)
  c->daddiu(t2, t2, 128);                           // daddiu t2, t2, 128
  bc = c->sgpr64(t1) != 0;                          // bne t1, r0, L110
  vis[14] = c->gpr_src(t0).du16[0];                    // ctc2.ni vi14, t0
  if (bc) {goto block_46;}                          // branch non-likely

  // Unknown instr: vcallms 303
  vcallms_303(c, vis);
  // nop                                            // sll r0, r0, 0

  block_49:
  c->lq(a2, 0, v0);                                 // lq a2, 0(v0)
  c->pextlb(t9, r0, t8);                            // pextlb t9, r0, t8
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(gp, r0, t9);                            // pextlh gp, r0, t9
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(a3, r0, a2);                            // pextlb a3, r0, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextub(a2, r0, a2);                            // pextub a2, r0, a2
  c->mov128_vf_gpr(vf4, gp);                        // qmtc2.ni vf4, gp
  c->pextlh(t0, a1, a3);                            // pextlh t0, a1, a3
  c->mov128_vf_gpr(vf1, t0);                        // qmtc2.ni vf1, t0
  c->pextuh(a3, a1, a3);                            // pextuh a3, a1, a3
  c->mov128_vf_gpr(vf2, a3);                        // qmtc2.ni vf2, a3
  c->pextlh(t1, a1, a2);                            // pextlh t1, a1, a2
  c->mov128_vf_gpr(vf3, t1);                        // qmtc2.ni vf3, t1
  c->pextuh(t9, r0, t9);                            // pextuh t9, r0, t9
  // Unknown instr: vcallms 311
  vcallms_311(c, vis);
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
  c->lq(t2, 16, v0);                                // lq t2, 16(v0)
  // nop                                            // sll r0, r0, 0
  c->daddiu(at, at, -16);                           // daddiu at, at, -16
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextuh(a2, a1, a2);                            // pextuh a2, a1, a2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(t0, r0, t2);                            // pextlb t0, r0, t2
  c->mov128_vf_gpr(vf4, t9);                        // qmtc2.ni vf4, t9
  c->pextlh(t1, a1, t0);                            // pextlh t1, a1, t0
  c->mov128_vf_gpr(vf1, a2);                        // qmtc2.ni vf1, a2
  c->pextuh(t0, a1, t0);                            // pextuh t0, a1, t0
  c->mov128_vf_gpr(vf2, t1);                        // qmtc2.ni vf2, t1
  c->pextub(t9, r0, t8);                            // pextub t9, r0, t8
  c->mov128_vf_gpr(vf3, t0);                        // qmtc2.ni vf3, t0
  c->pextlh(gp, r0, t9);                            // pextlh gp, r0, t9
  // nop                                            // sll r0, r0, 0
  c->daddiu(v0, v0, -48);                           // daddiu v0, v0, -48
  // Unknown instr: vcallms 311
  vcallms_311(c, vis);
  // nop                                            // sll r0, r0, 0
  c->addiu(t6, r0, 0);                              // addiu t6, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lwl(t6, 53, v0);                               // lwl t6, 53(v0)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(t6, r0, t6);                            // pextlb t6, r0, t6
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(t6, r0, t6);                            // pextlb t6, r0, t6
  c->lq(a3, 80, v0);                                // lq a3, 80(v0)
  c->paddw(t6, t6, t3);                             // paddw t6, t6, t3
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextub(t2, r0, t2);                            // pextub t2, r0, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(t0, a1, t2);                            // pextlh t0, a1, t2
  c->mov128_vf_gpr(vf4, gp);                        // qmtc2.ni vf4, gp
  c->pextuh(t2, a1, t2);                            // pextuh t2, a1, t2
  c->mov128_vf_gpr(vf1, t0);                        // qmtc2.ni vf1, t0
  c->pextlb(t1, r0, a3);                            // pextlb t1, r0, a3
  c->mov128_vf_gpr(vf2, t2);                        // qmtc2.ni vf2, t2
  c->pextlh(a2, a1, t1);                            // pextlh a2, a1, t1
  c->mov128_vf_gpr(vf3, a2);                        // qmtc2.ni vf3, a2
  c->pextuh(t9, r0, t9);                            // pextuh t9, r0, t9
  // nop                                            // sll r0, r0, 0
  c->daddiu(v1, v1, -64);                           // daddiu v1, v1, -64
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf15, 64, v1);                            // sqc2 vf15, 64(v1)
  // Unknown instr: vcallms 311
  vcallms_311(c, vis);

  // nop                                            // sll r0, r0, 0
  c->lwr(s3, 56, v0);                               // lwr s3, 56(v0)
  // nop                                            // sll r0, r0, 0
  c->lwl(s3, 69, v0);                               // lwl s3, 69(v0)
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyud(t6, t6, r0);                            // pcpyud t6, t6, r0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(s3, r0, s3);                            // pextlb s3, r0, s3
  // nop                                            // sll r0, r0, 0
  c->dsllv(s3, s3, s2);                             // dsllv s3, s3, s2
  c->lq(t8, 0, s6);                                 // lq t8, 0(s6)
  c->paddh(s3, s3, s1);                             // paddh s3, s3, s1
  c->lbu(t7, 0, t6);                                // lbu t7, 0(t6)
  c->dsrl32(t6, t6, 0);                             // dsrl32 t6, t6, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextuh(t0, a1, t1);                            // pextuh t0, a1, t1
  c->lbu(s0, 0, t6);                                // lbu s0, 0(t6)
  c->pextub(a3, r0, a3);                            // pextub a3, r0, a3
  c->mov128_vf_gpr(vf4, t9);                        // qmtc2.ni vf4, t9
  c->pextlh(t2, a1, a3);                            // pextlh t2, a1, a3
  c->mov128_vf_gpr(vf1, t0);                        // qmtc2.ni vf1, t0
  c->pextuh(a3, a1, a3);                            // pextuh a3, a1, a3
  c->mov128_vf_gpr(vf2, t2);                        // qmtc2.ni vf2, t2
  c->pextlb(t9, r0, t8);                            // pextlb t9, r0, t8
  c->mov128_vf_gpr(vf3, a3);                        // qmtc2.ni vf3, a3
  c->pextlh(gp, r0, t9);                            // pextlh gp, r0, t9
  // nop                                            // sll r0, r0, 0
  c->daddiu(s6, s6, 16);                            // daddiu s6, s6, 16
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf15, 96, v1);                            // sqc2 vf15, 96(v1)
  // Unknown instr: vcallms 311
  vcallms_311(c, vis);

  c->sw(s3, 76, v1);                                // sw s3, 76(v1)
  c->lwr(t6, 64, v0);                               // lwr t6, 64(v0)
  c->daddu(t7, t7, t4);                             // daddu t7, t7, t4
  c->lwl(t6, 77, v0);                               // lwl t6, 77(v0)
  c->daddu(s0, s0, t4);                             // daddu s0, s0, t4
  //beq r0, r0, L114                                // beq r0, r0, L114
  c->dsrl32(s3, s3, 0);                             // dsrl32 s3, s3, 0
  goto block_51;                                    // branch always


  block_50:
  // Unknown instr: vcallms 311
  vcallms_311(c, vis);

  c->sw(s3, 76, v1);                                // sw s3, 76(v1)
  c->lwr(t6, 64, v0);                               // lwr t6, 64(v0)
  c->daddu(t7, t7, t4);                             // daddu t7, t7, t4
  c->lwl(t6, 77, v0);                               // lwl t6, 77(v0)
  c->daddu(s0, s0, t4);                             // daddu s0, s0, t4
  c->sw(a0, 60, v1);                                // sw a0, 60(v1)
  c->dsrl32(s3, s3, 0);                             // dsrl32 s3, s3, 0

  block_51:
  c->sb(t5, 0, t7);                                 // sb t5, 0(t7)
  c->pextlb(t6, r0, t6);                            // pextlb t6, r0, t6
  c->sb(t5, 0, s0);                                 // sb t5, 0(s0)
  c->pextlb(t6, r0, t6);                            // pextlb t6, r0, t6
  c->lq(a2, 96, v0);                                // lq a2, 96(v0)
  c->paddw(t6, t6, t3);                             // paddw t6, t6, t3
  c->lbu(t7, 0, t6);                                // lbu t7, 0(t6)
  c->dsrl32(t6, t6, 0);                             // dsrl32 t6, t6, 0
  c->lw(a0, 16, at);                                // lw a0, 16(at)
  c->pextlb(a3, r0, a2);                            // pextlb a3, r0, a2
  c->lbu(s0, 0, t6);                                // lbu s0, 0(t6)
  c->pextub(a2, r0, a2);                            // pextub a2, r0, a2
  c->mov128_vf_gpr(vf4, gp);                        // qmtc2.ni vf4, gp
  c->pextlh(t0, a1, a3);                            // pextlh t0, a1, a3
  c->mov128_vf_gpr(vf1, t0);                        // qmtc2.ni vf1, t0
  c->pextuh(a3, a1, a3);                            // pextuh a3, a1, a3
  c->mov128_vf_gpr(vf2, a3);                        // qmtc2.ni vf2, a3
  c->pextlh(t1, a1, a2);                            // pextlh t1, a1, a2
  c->mov128_vf_gpr(vf3, t1);                        // qmtc2.ni vf3, t1
  c->pextuh(t9, r0, t9);                            // pextuh t9, r0, t9
  c->sqc2(vf14, 80, v1);                            // sqc2 vf14, 80(v1)
  c->daddiu(at, at, 16);                            // daddiu at, at, 16
  c->addiu(s4, s4, 2);                              // addiu s4, s4, 2
  c->sqc2(vf15, 128, v1);                           // sqc2 vf15, 128(v1)
  bc = c->sgpr64(t5) == c->sgpr64(s5);              // beq t5, s5, L115
  c->daddiu(t5, t5, 2);                             // daddiu t5, t5, 2
  if (bc) {goto block_56;}                          // branch non-likely

  // Unknown instr: vcallms 311
  vcallms_311(c, vis);

  c->sw(s3, 108, v1);                               // sw s3, 108(v1)
  c->lwr(s3, 80, v0);                               // lwr s3, 80(v0)
  c->daddu(t7, t7, t4);                             // daddu t7, t7, t4
  c->lwl(s3, 93, v0);                               // lwl s3, 93(v0)
  c->daddu(s0, s0, t4);                             // daddu s0, s0, t4
  c->sw(a0, 92, v1);                                // sw a0, 92(v1)
  c->pcpyud(t6, t6, r0);                            // pcpyud t6, t6, r0
  c->sb(s4, 0, t7);                                 // sb s4, 0(t7)
  c->pextlb(s3, r0, s3);                            // pextlb s3, r0, s3
  c->sb(s4, 0, s0);                                 // sb s4, 0(s0)
  c->dsllv(s3, s3, s2);                             // dsllv s3, s3, s2
  c->lq(t2, 112, v0);                               // lq t2, 112(v0)
  c->paddh(s3, s3, s1);                             // paddh s3, s3, s1
  c->lbu(t7, 0, t6);                                // lbu t7, 0(t6)
  c->dsrl32(t6, t6, 0);                             // dsrl32 t6, t6, 0
  c->lw(a0, 4, at);                                 // lw a0, 4(at)
  c->pextuh(a2, a1, a2);                            // pextuh a2, a1, a2
  c->lbu(s0, 0, t6);                                // lbu s0, 0(t6)
  c->pextlb(t0, r0, t2);                            // pextlb t0, r0, t2
  c->mov128_vf_gpr(vf4, t9);                        // qmtc2.ni vf4, t9
  c->pextlh(t1, a1, t0);                            // pextlh t1, a1, t0
  c->mov128_vf_gpr(vf1, a2);                        // qmtc2.ni vf1, a2
  c->pextuh(t0, a1, t0);                            // pextuh t0, a1, t0
  c->mov128_vf_gpr(vf2, t1);                        // qmtc2.ni vf2, t1
  c->pextub(t9, r0, t8);                            // pextub t9, r0, t8
  c->mov128_vf_gpr(vf3, t0);                        // qmtc2.ni vf3, t0
  c->pextlh(gp, r0, t9);                            // pextlh gp, r0, t9
  c->sqc2(vf14, 112, v1);                           // sqc2 vf14, 112(v1)
  c->daddiu(v0, v0, 48);                            // daddiu v0, v0, 48
  bc = c->sgpr64(s4) == c->sgpr64(s5);              // beq s4, s5, L116
  c->sqc2(vf15, 160, v1);                           // sqc2 vf15, 160(v1)
  if (bc) {goto block_57;}                          // branch non-likely

  // Unknown instr: vcallms 311
  vcallms_311(c, vis);

  c->sw(s3, 140, v1);                               // sw s3, 140(v1)
  c->lwr(t6, 40, v0);                               // lwr t6, 40(v0)
  c->daddu(t7, t7, t4);                             // daddu t7, t7, t4
  c->lwl(t6, 53, v0);                               // lwl t6, 53(v0)
  c->daddu(s0, s0, t4);                             // daddu s0, s0, t4
  c->sw(a0, 124, v1);                               // sw a0, 124(v1)
  c->dsrl32(s3, s3, 0);                             // dsrl32 s3, s3, 0
  c->sb(t5, 0, t7);                                 // sb t5, 0(t7)
  c->pextlb(t6, r0, t6);                            // pextlb t6, r0, t6
  c->sb(t5, 0, s0);                                 // sb t5, 0(s0)
  c->pextlb(t6, r0, t6);                            // pextlb t6, r0, t6
  c->lq(a3, 80, v0);                                // lq a3, 80(v0)
  c->paddw(t6, t6, t3);                             // paddw t6, t6, t3
  c->lbu(t7, 0, t6);                                // lbu t7, 0(t6)
  c->dsrl32(t6, t6, 0);                             // dsrl32 t6, t6, 0
  c->lw(a0, 8, at);                                 // lw a0, 8(at)
  c->pextub(t2, r0, t2);                            // pextub t2, r0, t2
  c->lbu(s0, 0, t6);                                // lbu s0, 0(t6)
  c->pextlh(t0, a1, t2);                            // pextlh t0, a1, t2
  c->mov128_vf_gpr(vf4, gp);                        // qmtc2.ni vf4, gp
  c->pextuh(t2, a1, t2);                            // pextuh t2, a1, t2
  c->mov128_vf_gpr(vf1, t0);                        // qmtc2.ni vf1, t0
  c->pextlb(t1, r0, a3);                            // pextlb t1, r0, a3
  c->mov128_vf_gpr(vf2, t2);                        // qmtc2.ni vf2, t2
  c->pextlh(a2, a1, t1);                            // pextlh a2, a1, t1
  c->mov128_vf_gpr(vf3, a2);                        // qmtc2.ni vf3, a2
  c->pextuh(t9, r0, t9);                            // pextuh t9, r0, t9
  c->sqc2(vf14, 144, v1);                           // sqc2 vf14, 144(v1)
  c->daddiu(v1, v1, 128);                           // daddiu v1, v1, 128
  c->addiu(s4, s4, 2);                              // addiu s4, s4, 2
  c->sqc2(vf15, 64, v1);                            // sqc2 vf15, 64(v1)
  bc = c->sgpr64(t5) == c->sgpr64(s5);              // beq t5, s5, L117
  c->daddiu(t5, t5, 2);                             // daddiu t5, t5, 2
  if (bc) {goto block_58;}                          // branch non-likely

  // Unknown instr: vcallms 311
  vcallms_311(c, vis);

  c->sw(s3, 44, v1);                                // sw s3, 44(v1)
  c->lwr(s3, 56, v0);                               // lwr s3, 56(v0)
  c->daddu(t7, t7, t4);                             // daddu t7, t7, t4
  c->lwl(s3, 69, v0);                               // lwl s3, 69(v0)
  c->daddu(s0, s0, t4);                             // daddu s0, s0, t4
  c->sw(a0, 28, v1);                                // sw a0, 28(v1)
  c->pcpyud(t6, t6, r0);                            // pcpyud t6, t6, r0
  c->sb(s4, 0, t7);                                 // sb s4, 0(t7)
  c->pextlb(s3, r0, s3);                            // pextlb s3, r0, s3
  c->sb(s4, 0, s0);                                 // sb s4, 0(s0)
  c->dsllv(s3, s3, s2);                             // dsllv s3, s3, s2
  c->lq(t8, 0, s6);                                 // lq t8, 0(s6)
  c->paddh(s3, s3, s1);                             // paddh s3, s3, s1
  c->lbu(t7, 0, t6);                                // lbu t7, 0(t6)
  c->dsrl32(t6, t6, 0);                             // dsrl32 t6, t6, 0
  c->lw(a0, 12, at);                                // lw a0, 12(at)
  c->pextuh(t0, a1, t1);                            // pextuh t0, a1, t1
  c->lbu(s0, 0, t6);                                // lbu s0, 0(t6)
  c->pextub(a3, r0, a3);                            // pextub a3, r0, a3
  c->mov128_vf_gpr(vf4, t9);                        // qmtc2.ni vf4, t9
  c->pextlh(t2, a1, a3);                            // pextlh t2, a1, a3
  c->mov128_vf_gpr(vf1, t0);                        // qmtc2.ni vf1, t0
  c->pextuh(a3, a1, a3);                            // pextuh a3, a1, a3
  c->mov128_vf_gpr(vf2, t2);                        // qmtc2.ni vf2, t2
  c->pextlb(t9, r0, t8);                            // pextlb t9, r0, t8
  c->mov128_vf_gpr(vf3, a3);                        // qmtc2.ni vf3, a3
  c->pextlh(gp, r0, t9);                            // pextlh gp, r0, t9
  c->sqc2(vf14, 48, v1);                            // sqc2 vf14, 48(v1)
  c->daddiu(s6, s6, 16);                            // daddiu s6, s6, 16
  bc = c->sgpr64(s4) != c->sgpr64(s5);              // bne s4, s5, L113
  c->sqc2(vf15, 96, v1);                            // sqc2 vf15, 96(v1)
  if (bc) {goto block_50;}                          // branch non-likely

  //beq r0, r0, L117                                // beq r0, r0, L117
  c->daddiu(v1, v1, 32);                            // daddiu v1, v1, 32
  goto block_58;                                    // branch always


  block_56:
  //beq r0, r0, L117                                // beq r0, r0, L117
  c->daddiu(v1, v1, 64);                            // daddiu v1, v1, 64
  goto block_58;                                    // branch always


  block_57:
  //beq r0, r0, L117                                // beq r0, r0, L117
  c->daddiu(v1, v1, 96);                            // daddiu v1, v1, 96
  goto block_58;                                    // branch always


  block_58:
  c->sw(a0, 28, v1);                                // sw a0, 28(v1)
  c->daddiu(s5, s5, 1);                             // daddiu s5, s5, 1
  c->daddiu(v1, v1, 32);                            // daddiu v1, v1, 32
  // nop                                            // sll r0, r0, 0
  get_fake_spad_addr2(a3, cache.fake_scratchpad_data, 0, c);// lui a3, 28672
  // nop                                            // sll r0, r0, 0
  c->lw(a0, 7444, a3);                              // lw a0, 7444(a3)
  // nop                                            // sll r0, r0, 0
  c->lw(a1, 7436, a3);                              // lw a1, 7436(a3)
  // nop                                            // sll r0, r0, 0
  c->lw(a3, 7440, a3);                              // lw a3, 7440(a3)
  // nop                                            // sll r0, r0, 0
  c->sb(s5, 17, a1);                                // sb s5, 17(a1)
  // nop                                            // sll r0, r0, 0
  c->lbu(s0, 0, a0);                                // lbu s0, 0(a0)
  c->addiu(s4, r0, 0);                              // addiu s4, r0, 0
  c->lbu(s1, 7, a0);                                // lbu s1, 7(a0)
  c->addiu(t8, r0, 0);                              // addiu t8, r0, 0
  c->lbu(s2, 8, a0);                                // lbu s2, 8(a0)
  c->sll(s0, s0, 2);                                // sll s0, s0, 2
  c->daddu(s0, s0, a0);                             // daddu s0, s0, a0
  c->sll(s1, s1, 2);                                // sll s1, s1, 2
  bc = c->sgpr64(s1) == 0;                          // beq s1, r0, L119
  c->daddu(s1, s1, s0);                             // daddu s1, s1, s0
  if (bc) {goto block_61;}                          // branch non-likely


  block_59:
  c->lbu(s3, 0, s0);                                // lbu s3, 0(s0)
  c->daddu(s4, s4, t4);                             // daddu s4, s4, t4
  c->lbu(s6, 1, s0);                                // lbu s6, 1(s0)
  c->daddu(t8, t8, t4);                             // daddu t8, t8, t4
  c->lbu(t9, 0, s4);                                // lbu t9, 0(s4)
  c->daddu(s3, s3, t3);                             // daddu s3, s3, t3
  c->lbu(s4, 0, s3);                                // lbu s4, 0(s3)
  c->daddu(s6, s6, t3);                             // daddu s6, s6, t3
  c->sb(t9, 0, t8);                                 // sb t9, 0(t8)
  c->daddiu(s0, s0, 4);                             // daddiu s0, s0, 4
  bc = c->sgpr64(s0) != c->sgpr64(s1);              // bne s0, s1, L118
  c->lbu(t8, 0, s6);                                // lbu t8, 0(s6)
  if (bc) {goto block_59;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->daddu(s4, s4, t4);                             // daddu s4, s4, t4
  c->lbu(t9, 0, s4);                                // lbu t9, 0(s4)
  c->daddu(t8, t8, t4);                             // daddu t8, t8, t4
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sb(t9, 0, t8);                                 // sb t9, 0(t8)
  // nop                                            // sll r0, r0, 0

  block_61:
  c->sll(s2, s2, 2);                                // sll s2, s2, 2
  c->daddiu(t5, a3, 32);                            // daddiu t5, a3, 32
  bc = c->sgpr64(s2) == 0;                          // beq s2, r0, L122
  c->daddu(s2, s2, s1);                             // daddu s2, s2, s1
  if (bc) {goto block_65;}                          // branch non-likely

  c->lbu(s3, 0, s0);                                // lbu s3, 0(s0)
  c->daddiu(t6, a3, 192);                           // daddiu t6, a3, 192
  c->lbu(s6, 1, s0);                                // lbu s6, 1(s0)
  c->daddiu(t7, a3, 448);                           // daddiu t7, a3, 448
  // nop                                            // sll r0, r0, 0
  c->daddu(s3, s3, t6);                             // daddu s3, s3, t6
  c->lbu(s3, 0, s3);                                // lbu s3, 0(s3)
  c->daddu(s6, s6, t3);                             // daddu s6, s6, t3
  c->lbu(s6, 0, s6);                                // lbu s6, 0(s6)
  c->daddiu(v1, v1, -32);                           // daddiu v1, v1, -32
  // nop                                            // sll r0, r0, 0
  c->daddu(s3, s3, t5);                             // daddu s3, s3, t5
  c->lbu(s4, 0, s3);                                // lbu s4, 0(s3)
  c->daddu(s6, s6, t4);                             // daddu s6, s6, t4
  // nop                                            // sll r0, r0, 0
  c->daddiu(s0, s0, 4);                             // daddiu s0, s0, 4
  c->sb(s5, 0, s6);                                 // sb s5, 0(s6)
  c->daddiu(s5, s5, 1);                             // daddiu s5, s5, 1
  bc = c->sgpr64(s0) == c->sgpr64(s2);              // beq s0, s2, L121
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_64;}                          // branch non-likely


  block_63:
  c->lbu(s3, 0, s0);                                // lbu s3, 0(s0)
  c->sll(s4, s4, 5);                                // sll s4, s4, 5
  c->lbu(s6, 1, s0);                                // lbu s6, 1(s0)
  c->daddu(s4, s4, t7);                             // daddu s4, s4, t7
  c->lq(t9, 0, s4);                                 // lq t9, 0(s4)
  c->daddu(s3, s3, t6);                             // daddu s3, s3, t6
  c->lbu(s3, 0, s3);                                // lbu s3, 0(s3)
  c->daddu(s6, s6, t3);                             // daddu s6, s6, t3
  c->lbu(s6, 0, s6);                                // lbu s6, 0(s6)
  c->daddiu(v1, v1, 32);                            // daddiu v1, v1, 32
  c->lq(gp, 16, s4);                                // lq gp, 16(s4)
  c->daddu(s3, s3, t5);                             // daddu s3, s3, t5
  c->lbu(s4, 0, s3);                                // lbu s4, 0(s3)
  c->daddu(s6, s6, t4);                             // daddu s6, s6, t4
  c->sq(t9, 0, v1);                                 // sq t9, 0(v1)
  c->daddiu(s0, s0, 4);                             // daddiu s0, s0, 4
  c->sb(s5, 0, s6);                                 // sb s5, 0(s6)
  c->daddiu(s5, s5, 1);                             // daddiu s5, s5, 1
  bc = c->sgpr64(s0) != c->sgpr64(s2);              // bne s0, s2, L120
  c->sq(gp, 16, v1);                                // sq gp, 16(v1)
  if (bc) {goto block_63;}                          // branch non-likely


  block_64:
  // nop                                            // sll r0, r0, 0
  c->sll(s4, s4, 5);                                // sll s4, s4, 5
  // nop                                            // sll r0, r0, 0
  c->daddu(s4, s4, t7);                             // daddu s4, s4, t7
  c->lq(t9, 0, s4);                                 // lq t9, 0(s4)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(v1, v1, 32);                            // daddiu v1, v1, 32
  c->lq(gp, 16, s4);                                // lq gp, 16(s4)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sq(t9, 0, v1);                                 // sq t9, 0(v1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sq(gp, 16, v1);                                // sq gp, 16(v1)

  block_65:
  c->sh(s5, 20, a1);                                // sh s5, 20(a1)
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->lq(s0, 7536, at);                              // lq s0, 7536(at)
  c->lq(s1, 7552, at);                              // lq s1, 7552(at)
  c->lq(s2, 7568, at);                              // lq s2, 7568(at)
  c->lq(s3, 7584, at);                              // lq s3, 7584(at)
  c->lq(s4, 7600, at);                              // lq s4, 7600(at)
  c->lq(s5, 7616, at);                              // lq s5, 7616(at)
  c->lq(s6, 7632, at);                              // lq s6, 7632(at)
  c->lq(t8, 7648, at);                              // lq t8, 7648(at)
  c->lq(t9, 7664, at);                              // lq t9, 7664(at)
  c->lq(gp, 7680, at);                              // lq gp, 7680(at)
  c->lq(ra, 7728, at);                              // lq ra, 7728(at)
  c->lq(sp, 7696, at);                              // lq sp, 7696(at)
  //jr ra                                           // jr ra
  c->lq(fp, 7712, at);                              // lq fp, 7712(at)
  goto end_of_function;                             // return

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
  gLinkedFunctionTable.reg("mercneric-convert", execute, 128);
}

} // namespace mercneric_convert
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace high_speed_reject {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;


void vcallms_438(ExecutionContext* c, u16* vis) {

  // nop                        |  mulaz.xyzw ACC, vf09, vf01     158
  c->acc.vf.mula_xyzw(c->vf_src(vf09).vf, c->vf_src(vf01).vf.z());
  // nop                        |  maddax.xyzw ACC, vf10, vf01    159
  c->acc.vf.madda_xyzw(c->vfs[vf10].vf, c->vfs[vf01].vf.x());
  // nop                        |  maddy.xyzw vf01, vf11, vf01    160
  u16 f1 = c->acc.vf.madd_flag(Mask::xyzw, c->vfs[vf01].vf, c->vf_src(vf11).vf, c->vf_src(vf01).vf.y());
  // nop                        |  mulaz.xyzw ACC, vf09, vf02     161
  c->acc.vf.mula_xyzw(c->vf_src(vf09).vf, c->vf_src(vf02).vf.z());
  // nop                        |  maddax.xyzw ACC, vf10, vf02    162
  c->acc.vf.madda_xyzw(c->vfs[vf10].vf, c->vfs[vf02].vf.x());
  // nop                        |  maddy.xyzw vf02, vf11, vf02    163
  u16 f2 = c->acc.vf.madd_flag(Mask::xyzw, c->vfs[vf02].vf, c->vf_src(vf11).vf, c->vf_src(vf02).vf.y());
  // fmand vi01, vi09           |  mulaz.xyzw ACC, vf09, vf03     164
  c->acc.vf.mula_xyzw(c->vf_src(vf09).vf, c->vf_src(vf03).vf.z());   vis[1] = vis[9] & f1;
  // nop                        |  maddax.xyzw ACC, vf10, vf03    165
  c->acc.vf.madda_xyzw(c->vfs[vf10].vf, c->vfs[vf03].vf.x());
  // nop                        |  maddy.xyzw vf03, vf11, vf03    166
  u16 f3 = c->acc.vf.madd_flag(Mask::xyzw, c->vfs[vf03].vf, c->vf_src(vf11).vf, c->vf_src(vf03).vf.y());
  // fmand vi02, vi09           |  mulaz.xyzw ACC, vf09, vf04     167
  c->acc.vf.mula_xyzw(c->vf_src(vf09).vf, c->vf_src(vf04).vf.z());   vis[2] = vis[9] & f2;
  // nop                        |  maddax.xyzw ACC, vf10, vf04    168
  c->acc.vf.madda_xyzw(c->vfs[vf10].vf, c->vfs[vf04].vf.x());
  // nop                        |  maddy.xyzw vf04, vf11, vf04    169
  c->acc.vf.madd_flag(Mask::xyzw, c->vfs[vf04].vf, c->vf_src(vf11).vf, c->vf_src(vf04).vf.y());
  // fmand vi03, vi09           |  nop                            170
  // ASSERT(false);
  vis[3] = vis[9] & f3;
  // nop                        |  nop                            171

  // nop                        |  nop :e                         172

  // fmand vi04, vi09           |  nop                            173
  // ASSERT(false);
  vis[4] = vis[9] & f4;

}

void vcallms_454(ExecutionContext* c, u16* vis) {
  // nop                        |  mulaz.xyzw ACC, vf09, vf05     174
  c->acc.vf.mula_xyzw(c->vf_src(vf09).vf, c->vf_src(vf05).vf.z());
  // nop                        |  maddax.xyzw ACC, vf10, vf05    175
  c->acc.vf.madda_xyzw(c->vfs[vf10].vf, c->vfs[vf05].vf.x());
  // nop                        |  maddy.xyzw vf05, vf11, vf05    176
  u16 f1 = c->acc.vf.madd_flag(Mask::xyzw, c->vfs[vf05].vf, c->vf_src(vf11).vf, c->vf_src(vf05).vf.y());
  // nop                        |  mulaz.xyzw ACC, vf09, vf06     177
  c->acc.vf.mula_xyzw(c->vf_src(vf09).vf, c->vf_src(vf06).vf.z());
  // nop                        |  maddax.xyzw ACC, vf10, vf06    178
  c->acc.vf.madda_xyzw(c->vfs[vf10].vf, c->vfs[vf06].vf.x());
  // nop                        |  maddy.xyzw vf06, vf11, vf06    179
  u16 f2 = c->acc.vf.madd_flag(Mask::xyzw, c->vfs[vf06].vf, c->vf_src(vf11).vf, c->vf_src(vf06).vf.y());
  // fmand vi05, vi09           |  mulaz.xyzw ACC, vf09, vf07     180
  c->acc.vf.mula_xyzw(c->vf_src(vf09).vf, c->vf_src(vf07).vf.z());   vis[5] = vis[9] & f1;
  // nop                        |  maddax.xyzw ACC, vf10, vf07    181
  c->acc.vf.madda_xyzw(c->vfs[vf10].vf, c->vfs[vf07].vf.x());
  // nop                        |  maddy.xyzw vf07, vf11, vf07    182
  u16 f3 = c->acc.vf.madd_flag(Mask::xyzw, c->vfs[vf07].vf, c->vf_src(vf11).vf, c->vf_src(vf07).vf.y());
  // fmand vi06, vi09           |  mulaz.xyzw ACC, vf09, vf08     183
  c->acc.vf.mula_xyzw(c->vf_src(vf09).vf, c->vf_src(vf08).vf.z());   vis[5] = vis[9] & f2;
  // nop                        |  maddax.xyzw ACC, vf10, vf08    184
  c->acc.vf.madda_xyzw(c->vfs[vf10].vf, c->vfs[vf08].vf.x());
  // nop                        |  maddy.xyzw vf08, vf11, vf08    185
  u16 f4 =c->acc.vf.madd_flag(Mask::xyzw, c->vfs[vf08].vf, c->vf_src(vf11).vf, c->vf_src(vf08).vf.y());
  // fmand vi07, vi09           |  nop                            186
  vis[7] = vis[9] & f3;
  // nop                        |  nop                            187

  // nop                        |  nop :e                         188

  // fmand vi08, vi09           |  nop                            189
  // ASSERT(false);
  vis[8] = vis[9] & f4;

}

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u16 vis[16];
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lw(a0, 7436, v1);                              // lw a0, 7436(v1)
  c->daddiu(a1, a0, 448);                           // daddiu a1, a0, 448
  c->lhu(a3, 20, a0);                               // lhu a3, 20(a0)
  c->vmax_bc(DEST::xyzw, BC::w, vf9, vf0, vf0);     // vmaxw.xyzw vf9, vf0, vf0
  c->lqc2(vf10, 7488, v1);                          // lqc2 vf10, 7488(v1)
  c->lqc2(vf11, 7504, v1);                          // lqc2 vf11, 7504(v1)
  c->lqc2(vf1, 0, a1);                              // lqc2 vf1, 0(a1)
  c->addiu(a0, r0, 240);                            // addiu a0, r0, 240
  c->lqc2(vf2, 32, a1);                             // lqc2 vf2, 32(a1)
  vis[9] = c->gpr_src(a0).du16[0];                     // ctc2.i vi9, a0
  c->lqc2(vf3, 64, a1);                             // lqc2 vf3, 64(a1)
  c->lqc2(vf4, 96, a1);                             // lqc2 vf4, 96(a1)
  c->daddiu(a0, r0, -1);                            // daddiu a0, r0, -1
  // nop                                            // sll r0, r0, 0
  // Unknown instr: vcallms 438
  vcallms_438(c, vis);
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lqc2(vf5, 128, a1);                            // lqc2 vf5, 128(a1)
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
  c->lqc2(vf6, 160, a1);                            // lqc2 vf6, 160(a1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lwu(t0, 8, a1);                                // lwu t0, 8(a1)
  c->lqc2(vf7, 192, a1);                            // lqc2 vf7, 192(a1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->gprs[a2].du64[0] = vis[1];                        // cfc2.ni a2, vi1
  c->lqc2(vf8, 224, a1);                            // lqc2 vf8, 224(a1)
  c->srl(t0, t0, 31);                               // srl t0, t0, 31
  c->or_(t0, a2, t0);                               // or t0, a2, t0
  c->lwu(a2, 40, a1);                               // lwu a2, 40(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L82
  c->and_(a0, a0, t0);                              // and a0, a0, t0
  if (bc) {goto block_20;}                          // branch non-likely

  c->daddiu(a1, a1, 128);                           // daddiu a1, a1, 128
  c->daddiu(a3, a3, -1);                            // daddiu a3, a3, -1
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L81
  c->gprs[t0].du64[0] = vis[2];                        // cfc2.ni t0, vi2
  if (bc) {goto block_18;}                          // branch non-likely


  block_2:
  // Unknown instr: vcallms 454
  vcallms_454(c, vis);
  c->srl(a2, a2, 31);                               // srl a2, a2, 31
  c->or_(a2, t0, a2);                               // or a2, t0, a2
  c->lwu(t0, -56, a1);                              // lwu t0, -56(a1)
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L82
  c->and_(a0, a0, a2);                              // and a0, a0, a2
  if (bc) {goto block_20;}                          // branch non-likely

  c->lqc2(vf1, 128, a1);                            // lqc2 vf1, 128(a1)
  c->daddiu(a2, a3, -1);                            // daddiu a2, a3, -1
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L81
  c->gprs[a3].du64[0] = vis[3];                        // cfc2.ni a3, vi3
  if (bc) {goto block_18;}                          // branch non-likely

  c->daddiu(a2, a2, -1);                            // daddiu a2, a2, -1
  c->srl(t0, t0, 31);                               // srl t0, t0, 31
  c->or_(t0, a3, t0);                               // or t0, a3, t0
  c->lwu(a3, -24, a1);                              // lwu a3, -24(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L82
  c->and_(a0, a0, t0);                              // and a0, a0, t0
  if (bc) {goto block_20;}                          // branch non-likely

  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L81
  c->gprs[t0].du64[0] = vis[4];                        // cfc2.ni t0, vi4
  if (bc) {goto block_18;}                          // branch non-likely

  c->lqc2(vf2, 160, a1);                            // lqc2 vf2, 160(a1)
  c->srl(a3, a3, 31);                               // srl a3, a3, 31
  c->or_(t0, t0, a3);                               // or t0, t0, a3
  c->lwu(a3, 8, a1);                                // lwu a3, 8(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L82
  c->and_(a0, a0, t0);                              // and a0, a0, t0
  if (bc) {goto block_20;}                          // branch non-likely

  c->lqc2(vf3, 192, a1);                            // lqc2 vf3, 192(a1)
  c->daddiu(a2, a2, -1);                            // daddiu a2, a2, -1
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L81
  c->gprs[t0].du64[0] = vis[5];                        // cfc2.ni t0, vi5
  if (bc) {goto block_18;}                          // branch non-likely

  c->lqc2(vf4, 224, a1);                            // lqc2 vf4, 224(a1)
  c->srl(a3, a3, 31);                               // srl a3, a3, 31
  c->or_(t0, t0, a3);                               // or t0, t0, a3
  c->lwu(a3, 40, a1);                               // lwu a3, 40(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L82
  c->and_(a0, a0, t0);                              // and a0, a0, t0
  if (bc) {goto block_20;}                          // branch non-likely

  c->daddiu(a1, a1, 128);                           // daddiu a1, a1, 128
  c->daddiu(a2, a2, -1);                            // daddiu a2, a2, -1
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L81
  c->gprs[t0].du64[0] = vis[6];                        // cfc2.ni t0, vi6
  if (bc) {goto block_18;}                          // branch non-likely

  // Unknown instr: vcallms 438
  c->srl(a3, a3, 31);                               // srl a3, a3, 31
  c->or_(t0, t0, a3);                               // or t0, t0, a3
  c->lwu(a3, -56, a1);                              // lwu a3, -56(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L82
  c->and_(a0, a0, t0);                              // and a0, a0, t0
  if (bc) {goto block_20;}                          // branch non-likely

  c->lqc2(vf5, 128, a1);                            // lqc2 vf5, 128(a1)
  c->daddiu(a2, a2, -1);                            // daddiu a2, a2, -1
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L81
  c->gprs[t0].du64[0] = vis[7];                        // cfc2.ni t0, vi7
  if (bc) {goto block_18;}                          // branch non-likely

  c->daddiu(a2, a2, -1);                            // daddiu a2, a2, -1
  c->srl(a3, a3, 31);                               // srl a3, a3, 31
  c->or_(t0, t0, a3);                               // or t0, t0, a3
  c->lwu(a3, -24, a1);                              // lwu a3, -24(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L82
  c->and_(a0, a0, t0);                              // and a0, a0, t0
  if (bc) {goto block_20;}                          // branch non-likely

  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L81
  c->gprs[t0].du64[0] = vis[8];                        // cfc2.ni t0, vi8
  if (bc) {goto block_18;}                          // branch non-likely

  c->lqc2(vf6, 160, a1);                            // lqc2 vf6, 160(a1)
  c->srl(a3, a3, 31);                               // srl a3, a3, 31
  c->or_(a3, t0, a3);                               // or a3, t0, a3
  c->lwu(t0, 8, a1);                                // lwu t0, 8(a1)
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L82
  c->and_(a0, a0, a3);                              // and a0, a0, a3
  if (bc) {goto block_20;}                          // branch non-likely

  c->lqc2(vf7, 192, a1);                            // lqc2 vf7, 192(a1)
  c->daddiu(a3, a2, -1);                            // daddiu a3, a2, -1
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L81
  c->gprs[a2].du64[0] = vis[1];                        // cfc2.ni a2, vi1
  if (bc) {goto block_18;}                          // branch non-likely

  c->lqc2(vf8, 224, a1);                            // lqc2 vf8, 224(a1)
  c->srl(t0, t0, 31);                               // srl t0, t0, 31
  c->or_(t0, a2, t0);                               // or t0, a2, t0
  c->lwu(a2, 40, a1);                               // lwu a2, 40(a1)
  bc = c->sgpr64(t0) == 0;                          // beq t0, r0, L82
  c->and_(a0, a0, t0);                              // and a0, a0, t0
  if (bc) {goto block_20;}                          // branch non-likely

  c->daddiu(a1, a1, 128);                           // daddiu a1, a1, 128
  c->daddiu(a3, a3, -1);                            // daddiu a3, a3, -1
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L80
  c->gprs[t0].du64[0] = vis[2];                        // cfc2.ni t0, vi2
  if (bc) {goto block_2;}                           // branch non-likely


  block_18:
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L82
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_20;}                          // branch non-likely

  c->sb(r0, 6853, v1);                              // sb r0, 6853(v1)

  block_20:
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
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("high-speed-reject", execute, 128);
}

} // namespace high_speed_reject
} // namespace Mips2C

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace generic_translucent {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;

u64 execute(void* ctxt) {
  u32 qwc = 0;
  u32 madr = 0;
  u32 sadr = 0;
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  [[maybe_unused]] u32 call_addr = 0;
  c->daddiu(sp, sp, -16);                           // daddiu sp, sp, -16
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->mov64(v1, a0);                                 // or v1, a0, r0
  c->daddiu(a0, at, 12064);                         // daddiu a0, at, 12064
  c->lbu(a1, 6842, at);                             // lbu a1, 6842(at)
  c->daddiu(a2, at, 6960);                          // daddiu a2, at, 6960
  c->mov64(a2, a2);                                 // or a2, a2, r0
  c->movn(a0, a2, a1);                              // movn a0, a2, a1
  c->lbu(a1, 6856, at);                             // lbu a1, 6856(at)
  c->addiu(a2, r0, 128);                            // addiu a2, r0, 128
  c->sltu(a2, a2, a1);                              // sltu a2, a2, a1
  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L75
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_2;}                           // branch non-likely

  c->addiu(a3, r0, 100);                            // addiu a3, r0, 100
  c->lw(a2, 24, at);                                // lw a2, 24(at)
  c->sw(a3, 112, a2);                               // sw a3, 112(a2)
  c->addiu(a3, r0, 66);                             // addiu a3, r0, 66
  c->sw(a3, 120, a2);                               // sw a3, 120(a2)
  c->sw(a1, 116, a2);                               // sw a1, 116(a2)
  //beq r0, r0, L76                                 // beq r0, r0, L76
  // nop                                            // sll r0, r0, 0
  goto block_3;                                     // branch always


  block_2:
  c->gprs[a1].du64[0] = 0;                          // or a1, r0, r0

  block_3:
  c->sw(r0, 48, at);                                // sw r0, 48(at)
  c->sw(a0, 52, at);                                // sw a0, 52(at)
  c->sw(v1, 44, at);                                // sw v1, 44(at)
  c->lwu(t9, 7468, at);                             // lwu t9, 7468(at)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  generic_prepare_dma_double::execute(c);
  c->lwu(t9, 7472, at);                             // lwu t9, 7472(at)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  generic_light_proc::execute(c);
  c->lwu(t9, 7476, at);                             // lwu t9, 7476(at)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  generic_envmap_proc::execute(c);
  c->lw(v1, 24, at);                                // lw v1, 24(at)
  c->lw(a0, 40, at);                                // lw a0, 40(at)
  c->mov64(a3, v1);                                 // or a3, v1, r0
  // nop                                            // sll r0, r0, 0
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->lui(a2, 4096);                                 // lui a2, 4096
  c->lwu(a1, 60, at);                               // lwu a1, 60(at)
  c->ori(a2, a2, 53248);                            // ori a2, a2, 53248
//   c->lw(t1, 0, a2);                                 // lw t1, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->daddiu(t0, at, 92);                            // daddiu t0, at, 92
  c->andi(a3, a3, 16383);                           // andi a3, a3, 16383
  c->andi(t1, t1, 256);                             // andi t1, t1, 256
  // nop                                            // sll r0, r0, 0
//  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L78
//  // nop                                            // sll r0, r0, 0
//  if (bc) {goto block_7;}                           // branch non-likely
//
//  c->mov64(t1, a2);                                 // or t1, a2, r0
//  // nop                                            // sll r0, r0, 0
//
//  block_5:
//  c->lw(t2, 0, t0);                                 // lw t2, 0(t0)
//  // nop                                            // sll r0, r0, 0
//  c->lw(t3, 0, t1);                                 // lw t3, 0(t1)
//  // nop                                            // sll r0, r0, 0
//  c->andi(t3, t3, 256);                             // andi t3, t3, 256
//  c->daddiu(t2, t2, 1);                             // daddiu t2, t2, 1
//  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L77
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
  c->xori(v1, v1, 4608);                            // xori v1, v1, 4608
  c->sw(v1, 24, at);                                // sw v1, 24(at)
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 16);                            // daddiu sp, sp, 16
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  gLinkedFunctionTable.reg("generic-translucent", execute, 128);
}

} // namespace generic_translucent
} // namespace Mips2C
// add generic_translucent::link to the link callback table for the object file.
// FWD DEC:
//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace generic_merc_query {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* vector_matrix; // vector-matrix*!
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -96);                           // daddiu sp, sp, -96
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s2, 16, sp);                                // sq s2, 16(sp)
  c->sq(s3, 32, sp);                                // sq s3, 32(sp)
  c->sq(s4, 48, sp);                                // sq s4, 48(sp)
  c->sq(s5, 64, sp);                                // sq s5, 64(sp)
  c->sq(gp, 80, sp);                                // sq gp, 80(sp)
  c->mov64(s4, a0);                                 // or s4, a0, r0
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(gp, 6836, v1);                             // lwu gp, 6836(v1)
  c->lhu(s3, 20, s4);                               // lhu s3, 20(s4)
  c->lw(s2, 24, gp);                                // lw s2, 24(gp)
  c->lw(s5, 20, gp);                                // lw s5, 20(gp)
  c->addiu(v1, r0, 7136);                           // addiu v1, r0, 7136
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  c->lwu(v1, 284, v1);                              // lwu v1, 284(v1)
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L70
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_2;}                           // branch non-likely

  c->addiu(s5, r0, 0);                              // addiu s5, r0, 0
  c->lw(s2, 12, gp);                                // lw s2, 12(gp)
  c->mov64(v1, s2);                                 // or v1, s2, r0

  block_2:
  //beq r0, r0, L73                                 // beq r0, r0, L73
  // nop                                            // sll r0, r0, 0
  goto block_6;                                     // branch always


  block_3:
  c->lw(v1, 0, gp);                                 // lw v1, 0(gp)
  c->slt(v1, s5, v1);                               // slt v1, s5, v1
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L72
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_5;}                           // branch non-likely

  c->dsll(v1, s5, 4);                               // dsll v1, s5, 4
  c->daddiu(v1, v1, 28);                            // daddiu v1, v1, 28
  c->daddu(a1, v1, gp);                             // daddu a1, v1, gp
  c->dsll(v1, s2, 5);                               // dsll v1, s2, 5
  c->daddu(v1, v1, s4);                             // daddu v1, v1, s4
  c->lwc1(f0, 448, v1);                             // lwc1 f0, 448(v1)
  c->swc1(f0, 0, a1);                               // swc1 f0, 0(a1)
  c->dsll(v1, s2, 5);                               // dsll v1, s2, 5
  c->daddu(v1, v1, s4);                             // daddu v1, v1, s4
  c->lwc1(f0, 452, v1);                             // lwc1 f0, 452(v1)
  c->swc1(f0, 4, a1);                               // swc1 f0, 4(a1)
  c->dsll(v1, s2, 5);                               // dsll v1, s2, 5
  c->daddu(v1, v1, s4);                             // daddu v1, v1, s4
  c->lwc1(f0, 456, v1);                             // lwc1 f0, 456(v1)
  c->swc1(f0, 8, a1);                               // swc1 f0, 8(a1)
  c->lui(v1, 16256);                                // lui v1, 16256
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->swc1(f0, 12, a1);                              // swc1 f0, 12(a1)
  c->load_symbol2(t9, cache.vector_matrix);         // lw t9, vector-matrix*!(s7)
  c->mov64(a0, a1);                                 // or a0, a1, r0
  c->addiu(v1, r0, 7264);                           // addiu v1, r0, 7264
  get_fake_spad_addr2(a2, cache.fake_scratchpad_data, 0, c);// lui a2, 28672
  c->daddu(a2, v1, a2);                             // daddu a2, v1, a2
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->daddiu(s5, s5, 1);                             // daddiu s5, s5, 1
  c->mov64(v1, s5);                                 // or v1, s5, r0

  block_5:
  c->lw(v1, 16, gp);                                // lw v1, 16(gp)
  c->daddu(s2, s2, v1);                             // daddu s2, s2, v1

  block_6:
  c->slt(v1, s2, s3);                               // slt v1, s2, s3
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L71
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_3;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->dsubu(v1, s2, s3);                             // dsubu v1, s2, s3
  c->sw(v1, 24, gp);                                // sw v1, 24(gp)
  c->sw(s5, 20, gp);                                // sw s5, 20(gp)
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 80, sp);                                // lq gp, 80(sp)
  c->lq(s5, 64, sp);                                // lq s5, 64(sp)
  c->lq(s4, 48, sp);                                // lq s4, 48(sp)
  c->lq(s3, 32, sp);                                // lq s3, 32(sp)
  c->lq(s2, 16, sp);                                // lq s2, 16(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 96);                            // daddiu sp, sp, 96
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  cache.vector_matrix = intern_from_c("vector-matrix*!").c();
  gLinkedFunctionTable.reg("generic-merc-query", execute, 128);
}

} // namespace generic_merc_query
} // namespace Mips2C
// add generic_merc_query::link to the link callback table for the object file.
// FWD DEC:

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace generic_merc_death {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* merc_death_spawn; // merc-death-spawn
  void* vector_matrix; // vector-matrix*!
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -144);                          // daddiu sp, sp, -144
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s1, 48, sp);                                // sq s1, 48(sp)
  c->sq(s2, 64, sp);                                // sq s2, 64(sp)
  c->sq(s3, 80, sp);                                // sq s3, 80(sp)
  c->sq(s4, 96, sp);                                // sq s4, 96(sp)
  c->sq(s5, 112, sp);                               // sq s5, 112(sp)
  c->sq(gp, 128, sp);                               // sq gp, 128(sp)
  c->mov64(gp, a0);                                 // or gp, a0, r0
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lhu(s5, 6846, v1);                             // lhu s5, 6846(v1)
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lhu(s3, 6844, v1);                             // lhu s3, 6844(v1)
  c->lhu(s4, 20, gp);                               // lhu s4, 20(gp)
  c->daddiu(s2, sp, 16);                            // daddiu s2, sp, 16
  c->sq(r0, 0, s2);                                 // sq r0, 0(s2)
  c->daddiu(s1, sp, 32);                            // daddiu s1, sp, 32
  c->sq(r0, 0, s1);                                 // sq r0, 0(s1)
  //beq r0, r0, L68                                 // beq r0, r0, L68
  // nop                                            // sll r0, r0, 0
  goto block_2;                                     // branch always


  block_1:
  c->dsll(v1, s5, 5);                               // dsll v1, s5, 5
  c->daddu(v1, v1, gp);                             // daddu v1, v1, gp
  c->lwc1(f0, 448, v1);                             // lwc1 f0, 448(v1)
  c->swc1(f0, 0, s2);                               // swc1 f0, 0(s2)
  c->dsll(v1, s5, 5);                               // dsll v1, s5, 5
  c->daddu(v1, v1, gp);                             // daddu v1, v1, gp
  c->lwc1(f0, 452, v1);                             // lwc1 f0, 452(v1)
  c->swc1(f0, 4, s2);                               // swc1 f0, 4(s2)
  c->dsll(v1, s5, 5);                               // dsll v1, s5, 5
  c->daddu(v1, v1, gp);                             // daddu v1, v1, gp
  c->lwc1(f0, 456, v1);                             // lwc1 f0, 456(v1)
  c->swc1(f0, 8, s2);                               // swc1 f0, 8(s2)
  c->lui(v1, 16256);                                // lui v1, 16256
  c->mtc1(f0, v1);                                  // mtc1 f0, v1
  c->swc1(f0, 12, s2);                              // swc1 f0, 12(s2)
  c->dsll(v1, s5, 5);                               // dsll v1, s5, 5
  c->daddu(v1, v1, gp);                             // daddu v1, v1, gp
  c->lwc1(f0, 464, v1);                             // lwc1 f0, 464(v1)
  c->swc1(f0, 0, s1);                               // swc1 f0, 0(s1)
  c->dsll(v1, s5, 5);                               // dsll v1, s5, 5
  c->daddu(v1, v1, gp);                             // daddu v1, v1, gp
  c->lwc1(f0, 468, v1);                             // lwc1 f0, 468(v1)
  c->swc1(f0, 4, s1);                               // swc1 f0, 4(s1)
  c->dsll(v1, s5, 5);                               // dsll v1, s5, 5
  c->daddu(v1, v1, gp);                             // daddu v1, v1, gp
  c->lwc1(f0, 472, v1);                             // lwc1 f0, 472(v1)
  c->swc1(f0, 8, s1);                               // swc1 f0, 8(s1)
  c->mtc1(f0, r0);                                  // mtc1 f0, r0
  c->swc1(f0, 12, s1);                              // swc1 f0, 12(s1)
  c->load_symbol2(t9, cache.vector_matrix);         // lw t9, vector-matrix*!(s7)
  c->mov64(a0, s2);                                 // or a0, s2, r0
  c->mov64(a1, s2);                                 // or a1, s2, r0
  c->addiu(v1, r0, 7264);                           // addiu v1, r0, 7264
  get_fake_spad_addr2(a2, cache.fake_scratchpad_data, 0, c);// lui a2, 28672
  c->daddu(a2, v1, a2);                             // daddu a2, v1, a2
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->load_symbol2(t9, cache.vector_matrix);         // lw t9, vector-matrix*!(s7)
  c->mov64(a0, s1);                                 // or a0, s1, r0
  c->mov64(a1, s1);                                 // or a1, s1, r0
  c->addiu(v1, r0, 7264);                           // addiu v1, r0, 7264
  get_fake_spad_addr2(a2, cache.fake_scratchpad_data, 0, c);// lui a2, 28672
  c->daddu(a2, v1, a2);                             // daddu a2, v1, a2
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->load_symbol2(t9, cache.merc_death_spawn);      // lw t9, merc-death-spawn(s7)
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(a0, 6848, v1);                             // lwu a0, 6848(v1)
  c->mov64(a1, s2);                                 // or a1, s2, r0
  c->mov64(a2, s1);                                 // or a2, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->daddu(s5, s5, s3);                             // daddu s5, s5, s3

  block_2:
  c->sltu(v1, s5, s4);                              // sltu v1, s5, s4
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L67
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  c->dsubu(a0, s5, s4);                             // dsubu a0, s5, s4
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->sh(a0, 6846, v1);                              // sh a0, 6846(v1)
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 128, sp);                               // lq gp, 128(sp)
  c->lq(s5, 112, sp);                               // lq s5, 112(sp)
  c->lq(s4, 96, sp);                                // lq s4, 96(sp)
  c->lq(s3, 80, sp);                                // lq s3, 80(sp)
  c->lq(s2, 64, sp);                                // lq s2, 64(sp)
  c->lq(s1, 48, sp);                                // lq s1, 48(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 144);                           // daddiu sp, sp, 144
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  cache.merc_death_spawn = intern_from_c("merc-death-spawn").c();
  cache.vector_matrix = intern_from_c("vector-matrix*!").c();
  gLinkedFunctionTable.reg("generic-merc-death", execute, 256);
}

} // namespace generic_merc_death
} // namespace Mips2C
// add generic_merc_death::link to the link callback table for the object file.
// FWD DEC:

//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace generic_merc_execute_asm {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* gsf_buffer; // *gsf-buffer*
  void* generic_merc_death; // generic-merc-death
  void* generic_merc_query; // generic-merc-query
  void* generic_translucent; // generic-translucent
  void* generic_warp_dest; // generic-warp-dest
  void* generic_warp_envmap_dest; // generic-warp-envmap-dest
  void* generic_warp_source; // generic-warp-source
} cache;

u64 execute(void* ctxt) {
  u32 madr, sadr, qwc;
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -112);                          // daddiu sp, sp, -112
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s1, 16, sp);                                // sq s1, 16(sp)
  c->sq(s2, 32, sp);                                // sq s2, 32(sp)
  c->sq(s3, 48, sp);                                // sq s3, 48(sp)
  c->sq(s4, 64, sp);                                // sq s4, 64(sp)
  c->sq(s5, 80, sp);                                // sq s5, 80(sp)
  c->sq(gp, 96, sp);                                // sq gp, 96(sp)
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(v1, 7456, v1);                             // lwu v1, 7456(v1)
  c->lwu(s2, 0, v1);                                // lwu s2, 0(v1)
  c->addiu(v1, r0, 0);                              // addiu v1, r0, 0
  c->addiu(gp, r0, 0);                              // addiu gp, r0, 0
  c->addiu(s5, r0, 0);                              // addiu s5, r0, 0
  c->load_symbol2(s4, cache.gsf_buffer);            // lw s4, *gsf-buffer*(s7)
  c->lui(v1, 4096);                                 // lui v1, 4096
  c->ori(s3, v1, 54272);                            // ori s3, v1, 54272
  c->addiu(v1, r0, 624);                            // addiu v1, r0, 624
  c->lui(a0, 4096);                                 // lui a0, 4096
  c->ori(a0, a0, 54272);                            // ori a0, a0, 54272
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->ori(a1, a1, 88);                               // ori a1, a1, 88
// dma
//  c->lw(a2, 0, a0);                                 // lw a2, 0(a0)
//  c->andi(a2, a2, 256);                             // andi a2, a2, 256
//  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L21
//  DANGER jump to delay slot, this MUST be fixed manually!
//      c->lw(a2, 0, a1);                                 // lw a2, 0(a1)
//  if (bc) {goto block_-1;}                          // branch non-likely
//
//
//  block_1:
//  c->lw(a2, 0, a1);                                 // lw a2, 0(a1)
//  // nop                                            // sll r0, r0, 0
//  c->lw(a3, 0, a0);                                 // lw a3, 0(a0)
//  // nop                                            // sll r0, r0, 0
//  c->andi(a3, a3, 256);                             // andi a3, a3, 256
//  c->daddiu(a2, a2, 1);                             // daddiu a2, a2, 1
//  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L20
//  c->sw(a2, 0, a1);                                 // sw a2, 0(a1)
//  if (bc) {goto block_1;}                           // branch non-likely

  c->gprs[a0].du64[0] = 0;                          // or a0, r0, r0
  // c->sw(v1, 128, s3);                               // sw v1, 128(s3)
  sadr = c->sgpr64(v1);

  // c->sw(s2, 48, s3);                                // sw s2, 48(s3)
  u32 tadr = c->sgpr64(s2);

  // c->sw(r0, 32, s3);                                // sw r0, 32(s3)
  // Unknown instr: sync.l
  c->addiu(v1, r0, 324);                            // addiu v1, r0, 324
  // c->sw(v1, 0, s3);                                 // sw v1, 0(s3)
  spad_to_dma_blerc_chain(cache.fake_scratchpad_data, sadr, tadr);
  // Unknown instr: sync.l
  //beq r0, r0, L65                                 // beq r0, r0, L65
  // nop                                            // sll r0, r0, 0
  goto block_85;                                    // branch always


  block_4:
  bc = c->sgpr64(gp) != 0;                          // bne gp, r0, L23
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_6;}                           // branch non-likely

  c->addiu(v1, r0, 624);                            // addiu v1, r0, 624
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  //beq r0, r0, L24                                 // beq r0, r0, L24
  // nop                                            // sll r0, r0, 0
  goto block_7;                                     // branch always


  block_6:
  c->addiu(v1, r0, 3648);                           // addiu v1, r0, 3648
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0

  block_7:
  bc = c->sgpr64(gp) != 0;                          // bne gp, r0, L25
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_9;}                           // branch non-likely

  c->addiu(a0, r0, 3648);                           // addiu a0, r0, 3648
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->daddu(a0, a0, a1);                             // daddu a0, a0, a1
  //beq r0, r0, L26                                 // beq r0, r0, L26
  // nop                                            // sll r0, r0, 0
  goto block_10;                                    // branch always


  block_9:
  c->addiu(a0, r0, 624);                            // addiu a0, r0, 624
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->daddu(a0, a0, a1);                             // daddu a0, a0, a1

  block_10:
  bc = c->sgpr64(gp) != 0;                          // bne gp, r0, L27
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_12;}                          // branch non-likely

  c->addiu(a1, r0, 3568);                           // addiu a1, r0, 3568
  get_fake_spad_addr2(a2, cache.fake_scratchpad_data, 0, c);// lui a2, 28672
  c->daddu(a1, a1, a2);                             // daddu a1, a1, a2
  //beq r0, r0, L28                                 // beq r0, r0, L28
  // nop                                            // sll r0, r0, 0
  goto block_13;                                    // branch always


  block_12:
  c->addiu(a1, r0, 6592);                           // addiu a1, r0, 6592
  get_fake_spad_addr2(a2, cache.fake_scratchpad_data, 0, c);// lui a2, 28672
  c->daddu(a1, a1, a2);                             // daddu a1, a1, a2

  block_13:
  bc = c->sgpr64(gp) != 0;                          // bne gp, r0, L29
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_15;}                          // branch non-likely

  c->addiu(a1, r0, 6592);                           // addiu a1, r0, 6592
  get_fake_spad_addr2(a2, cache.fake_scratchpad_data, 0, c);// lui a2, 28672
  c->daddu(a1, a1, a2);                             // daddu a1, a1, a2
  //beq r0, r0, L30                                 // beq r0, r0, L30
  // nop                                            // sll r0, r0, 0
  goto block_16;                                    // branch always


  block_15:
  c->addiu(a1, r0, 3568);                           // addiu a1, r0, 3568
  get_fake_spad_addr2(a2, cache.fake_scratchpad_data, 0, c);// lui a2, 28672
  c->daddu(a1, a1, a2);                             // daddu a1, a1, a2

  block_16:
  bc = c->sgpr64(gp) != 0;                          // bne gp, r0, L31
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_18;}                          // branch non-likely

  c->daddu(s1, r0, s4);                             // daddu s1, r0, s4
  //beq r0, r0, L32                                 // beq r0, r0, L32
  // nop                                            // sll r0, r0, 0
  goto block_19;                                    // branch always


  block_18:
  c->daddiu(s1, s4, 2752);                          // daddiu s1, s4, 2752

  block_19:
  bc = c->sgpr64(gp) != 0;                          // bne gp, r0, L33
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_21;}                          // branch non-likely

  c->daddiu(a1, s4, 2752);                          // daddiu a1, s4, 2752
  //beq r0, r0, L34                                 // beq r0, r0, L34
  // nop                                            // sll r0, r0, 0
  goto block_22;                                    // branch always


  block_21:
  c->daddu(a1, r0, s4);                             // daddu a1, r0, s4

  block_22:
  c->addiu(a2, r0, 7136);                           // addiu a2, r0, 7136
  get_fake_spad_addr2(a3, cache.fake_scratchpad_data, 0, c);// lui a3, 28672
  c->daddu(a2, a2, a3);                             // daddu a2, a2, a3
  c->sw(gp, 280, a2);                               // sw gp, 280(a2)
  c->sw(s5, 284, a2);                               // sw s5, 284(a2)
  c->sw(v1, 292, a2);                               // sw v1, 292(a2)
  c->sw(a0, 296, a2);                               // sw a0, 296(a2)
  c->sw(s1, 300, a2);                               // sw s1, 300(a2)
  c->sw(a1, 304, a2);                               // sw a1, 304(a2)
  c->sw(s4, 312, a2);                               // sw s4, 312(a2)
  c->mov64(a1, s4);                                 // or a1, s4, r0
  c->lui(a1, 4096);                                 // lui a1, 4096
  c->ori(a1, a1, 54272);                            // ori a1, a1, 54272
  get_fake_spad_addr2(a2, cache.fake_scratchpad_data, 0, c);// lui a2, 28672
  c->ori(a2, a2, 88);                               // ori a2, a2, 88
//  c->lw(a3, 0, a1);                                 // lw a3, 0(a1)
//  c->andi(a3, a3, 256);                             // andi a3, a3, 256
//  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L36
//  DANGER jump to delay slot, this MUST be fixed manually!
      c->lw(a3, 0, a2);                                 // lw a3, 0(a2)
//  if (bc) {goto block_-1;}                          // branch non-likely
//
//
//  block_23:
//  c->lw(a3, 0, a2);                                 // lw a3, 0(a2)
//  // nop                                            // sll r0, r0, 0
//  c->lw(t0, 0, a1);                                 // lw t0, 0(a1)
//  // nop                                            // sll r0, r0, 0
//  c->andi(t0, t0, 256);                             // andi t0, t0, 256
//  c->daddiu(a3, a3, 1);                             // daddiu a3, a3, 1
//  bc = c->sgpr64(t0) != 0;                          // bne t0, r0, L35
//  c->sw(a3, 0, a2);                                 // sw a3, 0(a2)
//  if (bc) {goto block_23;}                          // branch non-likely

  c->gprs[a1].du64[0] = 0;                          // or a1, r0, r0
  c->lwu(s2, 12, v1);                               // lwu s2, 12(v1)
  bc = c->sgpr64(s2) == 0;                          // beq s2, r0, L37
  c->mov64(a1, s7);                                 // or a1, s7, r0
  if (bc) {goto block_27;}                          // branch non-likely

  c->daddu(a0, r0, a0);                             // daddu a0, r0, a0
  c->andi(a0, a0, 65535);                           // andi a0, a0, 65535
  // c->sw(a0, 128, s3);                               // sw a0, 128(s3)
  sadr = c->sgpr64(a0);
  // c->sw(s2, 48, s3);                                // sw s2, 48(s3)
  tadr = c->sgpr64(s2);
  // c->sw(r0, 32, s3);                                // sw r0, 32(s3)
  // Unknown instr: sync.l
  c->addiu(a0, r0, 324);                            // addiu a0, r0, 324
  // c->sw(a0, 0, s3);                                 // sw a0, 0(s3)
  // hack - I don't really know why, but the andi above puts sadr into a 64 kb
  // range instead of a 16 kB range. I believe the hardware will just mask it.
  // or, maybe there's something else wrong? Not sure, but the transfer does seem to go
  // to the right spot when I use this.
  spad_to_dma_blerc_chain(cache.fake_scratchpad_data, sadr & 0x3fff, tadr);
  // Unknown instr: sync.l
  c->gprs[a0].du64[0] = 0;                          // or a0, r0, r0

  block_27:
  bc = c->sgpr64(s5) != 0;                          // bne s5, r0, L44
  c->mov64(a0, s7);                                 // or a0, s7, r0
  if (bc) {goto block_41;}                          // branch non-likely

  c->addiu(a0, r0, 6672);                           // addiu a0, r0, 6672
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->daddu(a1, a0, a1);                             // daddu a1, a0, a1
  c->mov64(a2, v1);                                 // or a2, v1, r0
  c->lwu(a0, 8, v1);                                // lwu a0, 8(v1)
  // nop                                            // sll r0, r0, 0
  c->daddiu(a3, a0, -4);                            // daddiu a3, a0, -4
  c->mov64(a1, a1);                                 // or a1, a1, r0
  bc = ((s64)c->sgpr64(a3)) < 0;                    // bltz a3, L39
  c->mov64(a2, a2);                                 // or a2, a2, r0
  if (bc) {goto block_30;}                          // branch non-likely


  block_29:
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 0, a2);                                 // lq t2, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->lq(a3, 16, a2);                                // lq a3, 16(a2)
  c->daddiu(a0, a0, -4);                            // daddiu a0, a0, -4
  c->lq(t0, 32, a2);                                // lq t0, 32(a2)
  c->daddiu(a1, a1, 64);                            // daddiu a1, a1, 64
  c->lq(t1, 48, a2);                                // lq t1, 48(a2)
  c->daddiu(a2, a2, 64);                            // daddiu a2, a2, 64
  c->sq(t2, -64, a1);                               // sq t2, -64(a1)
  c->daddiu(t2, a0, -4);                            // daddiu t2, a0, -4
  c->sq(a3, -48, a1);                               // sq a3, -48(a1)
  // nop                                            // sll r0, r0, 0
  c->sq(t0, -32, a1);                               // sq t0, -32(a1)
  bc = ((s64)c->sgpr64(t2)) >= 0;                   // bgez t2, L38
  c->sq(t1, -16, a1);                               // sq t1, -16(a1)
  if (bc) {goto block_29;}                          // branch non-likely


  block_30:
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L40
  c->lq(a3, 0, a2);                                 // lq a3, 0(a2)
  if (bc) {goto block_35;}                          // branch non-likely

  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, -16, a1);                               // sq a3, -16(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L40
  c->lq(a3, 0, a2);                                 // lq a3, 0(a2)
  if (bc) {goto block_35;}                          // branch non-likely

  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, -16, a1);                               // sq a3, -16(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L40
  c->lq(a3, 0, a2);                                 // lq a3, 0(a2)
  if (bc) {goto block_35;}                          // branch non-likely

  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, -16, a1);                               // sq a3, -16(a1)
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L40
  c->lq(a3, 0, a2);                                 // lq a3, 0(a2)
  if (bc) {goto block_35;}                          // branch non-likely

  c->daddiu(a2, a2, 16);                            // daddiu a2, a2, 16
  c->daddiu(a1, a1, 16);                            // daddiu a1, a1, 16
  c->daddiu(a0, a0, -1);                            // daddiu a0, a0, -1
  c->sq(a3, -16, a1);                               // sq a3, -16(a1)

  block_35:
  c->gprs[a0].du64[0] = 0;                          // or a0, r0, r0
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->lbu(a0, 6840, a0);                             // lbu a0, 6840(a0)
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L41
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_37;}                          // branch non-likely

  c->addiu(a0, r0, 8);                              // addiu a0, r0, 8
  //beq r0, r0, L42                                 // beq r0, r0, L42
  // nop                                            // sll r0, r0, 0
  goto block_38;                                    // branch always


  block_37:
  c->addiu(a0, r0, 6);                              // addiu a0, r0, 6

  block_38:
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->ori(a1, a1, 11984);                            // ori a1, a1, 11984
  c->sh(a0, 0, a1);                                 // sh a0, 0(a1)
  c->addiu(a0, r0, 12048);                          // addiu a0, r0, 12048
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->daddu(a0, a0, a1);                             // daddu a0, a0, a1
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->lwu(a1, 6832, a1);                             // lwu a1, 6832(a1)
  c->sw(a1, 0, a0);                                 // sw a1, 0(a0)
  c->sw(a1, 4, a0);                                 // sw a1, 4(a0)
  c->sw(a1, 8, a0);                                 // sw a1, 8(a0)
  c->sw(a1, 12, a0);                                // sw a1, 12(a0)
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->addiu(a0, a1, 7136);                           // addiu a0, a1, 7136
  c->lbu(a2, 6841, a1);                             // lbu a2, 6841(a1)
  c->addiu(a3, a1, 7200);                           // addiu a3, a1, 7200
  c->addiu(a1, a1, 11808);                          // addiu a1, a1, 11808
  c->movn(a0, a3, a2);                              // movn a0, a3, a2
  c->lq(a2, 0, a0);                                 // lq a2, 0(a0)
  c->lq(a3, 16, a0);                                // lq a3, 16(a0)
  c->lq(t0, 32, a0);                                // lq t0, 32(a0)
  c->lq(a0, 48, a0);                                // lq a0, 48(a0)
  c->sq(a2, 0, a1);                                 // sq a2, 0(a1)
  c->sq(a3, 16, a1);                                // sq a3, 16(a1)
  c->sq(t0, 32, a1);                                // sq t0, 32(a1)
  c->sq(a0, 48, a1);                                // sq a0, 48(a1)
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->lbu(a0, 6843, a0);                             // lbu a0, 6843(a0)
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->sb(a0, 6853, a1);                              // sb a0, 6853(a1)
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->lqc2(vf1, 6688, at);                           // lqc2 vf1, 6688(at)
  c->lqc2(vf2, 6704, at);                           // lqc2 vf2, 6704(at)
  c->lqc2(vf3, 6720, at);                           // lqc2 vf3, 6720(at)
  c->lqc2(vf4, 6736, at);                           // lqc2 vf4, 6736(at)
  c->lqc2(vf5, 6752, at);                           // lqc2 vf5, 6752(at)
  c->lqc2(vf6, 6768, at);                           // lqc2 vf6, 6768(at)
  c->lqc2(vf7, 6784, at);                           // lqc2 vf7, 6784(at)
  c->sqc2(vf1, 12688, at);                          // sqc2 vf1, 12688(at)
  c->sqc2(vf2, 12704, at);                          // sqc2 vf2, 12704(at)
  c->sqc2(vf3, 12720, at);                          // sqc2 vf3, 12720(at)
  c->sqc2(vf4, 12736, at);                          // sqc2 vf4, 12736(at)
  c->sqc2(vf5, 12752, at);                          // sqc2 vf5, 12752(at)
  c->sqc2(vf6, 12768, at);                          // sqc2 vf6, 12768(at)
  c->sqc2(vf7, 12784, at);                          // sqc2 vf7, 12784(at)
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->lbu(a0, 6856, a0);                             // lbu a0, 6856(a0)
  c->addiu(a1, r0, 128);                            // addiu a1, r0, 128
  c->sltu(a1, a1, a0);                              // sltu a1, a1, a0
  bc = c->sgpr64(a1) != 0;                          // bne a1, r0, L43
  c->mov64(a1, s7);                                 // or a1, s7, r0
  if (bc) {goto block_40;}                          // branch non-likely

  c->lui(a1, 15360);                                // lui a1, 15360
  c->mtc1(f0, a1);                                  // mtc1 f0, a1
  c->mtc1(f1, a0);                                  // mtc1 f1, a0
  c->cvtsw(f1, f1);                                 // cvt.s.w f1, f1
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->lwc1(f1, 12796, a0);                           // lwc1 f1, 12796(a0)
  c->muls(f0, f0, f1);                              // mul.s f0, f0, f1
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->swc1(f0, 12796, a0);                           // swc1 f0, 12796(a0)
  c->mfc1(a1, f0);                                  // mfc1 a1, f0

  block_40:
  c->lwu(a0, 8, v1);                                // lwu a0, 8(v1)
  c->dsll(a0, a0, 4);                               // dsll a0, a0, 4
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  c->mov64(a0, v1);                                 // or a0, v1, r0

  block_41:
  c->addiu(a0, r0, 7136);                           // addiu a0, r0, 7136
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->daddu(a0, a0, a1);                             // daddu a0, a0, a1
  c->daddiu(v1, v1, 16);                            // daddiu v1, v1, 16
  c->sw(v1, 308, a0);                               // sw v1, 308(a0)
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(t9, 7460, v1);                             // lwu t9, 7460(v1)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  mercneric_convert::execute(c);
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->addiu(a0, r0, 1);                              // addiu a0, r0, 1
  c->lbu(a1, 6840, v1);                             // lbu a1, 6840(v1)
  c->daddiu(a2, s1, 448);                           // daddiu a2, s1, 448
  c->lbu(a3, 6843, v1);                             // lbu a3, 6843(v1)
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L46
  c->lqc2(vf1, 0, a2);                              // lqc2 vf1, 0(a2)
  if (bc) {goto block_47;}                          // branch non-likely

  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L46
  c->vmax_bc(DEST::xyzw, BC::w, vf9, vf0, vf0);     // vmaxw.xyzw vf9, vf0, vf0
  if (bc) {goto block_47;}                          // branch non-likely

  c->sb(a0, 6853, v1);                              // sb a0, 6853(v1)
  c->lqc2(vf10, 7488, v1);                          // lqc2 vf10, 7488(v1)
  c->lqc2(vf11, 7504, v1);                          // lqc2 vf11, 7504(v1)
  c->vmula_bc(DEST::xyzw, BC::z, vf9, vf1);         // vmulaz.xyzw acc, vf9, vf1
  c->lw(a0, 8, a2);                                 // lw a0, 8(a2)
  c->vmadda_bc(DEST::xyzw, BC::x, vf10, vf1);       // vmaddax.xyzw acc, vf10, vf1
  bc = ((s64)c->sgpr64(a0)) < 0;                    // bltz a0, L45
  c->lw(v1, 7480, v1);                              // lw v1, 7480(v1)
  if (bc) {goto block_45;}                          // branch non-likely

  c->vmadd_bc(DEST::xyzw, BC::y, vf1, vf11, vf1);   // vmaddy.xyzw vf1, vf11, vf1
  c->mov128_gpr_vf(a0, vf1);                        // qmfc2.i a0, vf1
  c->pcgtw(a0, r0, a0);                             // pcgtw a0, r0, a0
  c->ppach(a0, r0, a0);                             // ppach a0, r0, a0
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L46
  // DANGER jump to delay slot, this MUST be fixed manually!
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_47;}                          // branch non-likely


  block_45:
  // nop                                            // sll r0, r0, 0
  call_addr = c->gprs[v1].du32[0];                  // function call:
  // nop                                            // sll r0, r0, 0
  c->jalr(call_addr);                               // jalr ra, v1
  // high_speed_reject::execute(c);

  block_47:
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(v1, 6836, v1);                             // lwu v1, 6836(v1)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L47
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_49;}                          // branch non-likely

  c->load_symbol2(t9, cache.generic_merc_query);    // lw t9, generic-merc-query(s7)
  c->mov64(a0, s1);                                 // or a0, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0

  block_49:
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lhu(v1, 6844, v1);                             // lhu v1, 6844(v1)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L48
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_51;}                          // branch non-likely

  c->load_symbol2(t9, cache.generic_merc_death);    // lw t9, generic-merc-death(s7)
  c->mov64(a0, s1);                                 // or a0, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0

  block_51:
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lw(v1, 7424, v1);                              // lw v1, 7424(v1)
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->lwu(a0, 60, a0);                               // lwu a0, 60(a0)
  c->dsubu(v1, v1, a0);                             // dsubu v1, v1, a0
  c->slt(v1, r0, v1);                               // slt v1, r0, v1
  bc = c->sgpr64(v1) != 0;                          // bne v1, r0, L49
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_53;}                          // branch non-likely

  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->sb(r0, 6853, v1);                              // sb r0, 6853(v1)
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->sb(r0, 6843, v1);                              // sb r0, 6843(v1)
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0

  block_53:
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lbu(v1, 6853, v1);                             // lbu v1, 6853(v1)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L63
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_82;}                          // branch non-likely

  c->addiu(v1, r0, 1);                              // addiu v1, r0, 1
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->lbu(a0, 6854, a0);                             // lbu a0, 6854(a0)
  bc = c->sgpr64(a0) != c->sgpr64(v1);              // bne a0, v1, L50
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_56;}                          // branch non-likely

  c->load_symbol2(t9, cache.generic_warp_source);   // lw t9, generic-warp-source(s7)
  c->mov64(a0, s1);                                 // or a0, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  //beq r0, r0, L63                                 // beq r0, r0, L63
  // nop                                            // sll r0, r0, 0
  goto block_82;                                    // branch always


  block_56:
  c->addiu(v1, r0, 2);                              // addiu v1, r0, 2
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->lbu(a0, 6854, a0);                             // lbu a0, 6854(a0)
  bc = c->sgpr64(a0) != c->sgpr64(v1);              // bne a0, v1, L53
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_61;}                          // branch non-likely

  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lbu(v1, 6955, v1);                             // lbu v1, 6955(v1)
  c->andi(v1, v1, 1);                               // andi v1, v1, 1
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L51
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_59;}                          // branch non-likely

  c->load_symbol2(t9, cache.generic_warp_envmap_dest);// lw t9, generic-warp-envmap-dest(s7)
  c->mov64(a0, s1);                                 // or a0, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  //beq r0, r0, L52                                 // beq r0, r0, L52
  // nop                                            // sll r0, r0, 0
  goto block_60;                                    // branch always


  block_59:
  c->load_symbol2(t9, cache.generic_warp_dest);     // lw t9, generic-warp-dest(s7)
  c->mov64(a0, s1);                                 // or a0, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  generic_warp_dest::execute(c);
  c->mov64(v1, v0);                                 // or v1, v0, r0

  block_60:
  //beq r0, r0, L63                                 // beq r0, r0, L63
  // nop                                            // sll r0, r0, 0
  goto block_82;                                    // branch always


  block_61:
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lbu(v1, 6955, v1);                             // lbu v1, 6955(v1)
  c->andi(v1, v1, 1);                               // andi v1, v1, 1
  if (((s64)c->sgpr64(v1)) == ((s64)0)) {           // beql v1, r0, L54
    c->mov64(v1, s7);                               // or v1, s7, r0
    goto block_64;
  }

  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  c->lbu(a0, 6852, a0);                             // lbu a0, 6852(a0)
  c->movz(v1, s7, a0);                              // movz v1, s7, a0

  block_64:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L55
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_66;}                          // branch non-likely

  c->load_symbol2(t9, cache.generic_translucent);   // lw t9, generic-translucent(s7)
  c->mov64(a0, s1);                                 // or a0, s1, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  //beq r0, r0, L63                                 // beq r0, r0, L63
  // nop                                            // sll r0, r0, 0
  goto block_82;                                    // branch always


  block_66:
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lbu(v1, 6955, v1);                             // lbu v1, 6955(v1)
  c->andi(v1, v1, 1);                               // andi v1, v1, 1
  if (((s64)c->sgpr64(v1)) == ((s64)0)) {           // beql v1, r0, L56
    c->mov64(v1, s7);                               // or v1, s7, r0
    goto block_69;
  }

  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lbu(v1, 6858, v1);                             // lbu v1, 6858(v1)
  c->daddiu(a0, v1, -1);                            // daddiu a0, v1, -1
  c->daddiu(v1, s7, 4);                             // daddiu v1, s7, 4
  c->movz(v1, s7, a0);                              // movz v1, s7, a0

  block_69:
  bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L60
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_77;}                          // branch non-likely

  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->mov64(v1, s1);                                 // or v1, s1, r0
  c->daddiu(a0, at, 12064);                         // daddiu a0, at, 12064
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->lbu(a1, 6842, a1);                             // lbu a1, 6842(a1)
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L57
  c->mov64(a1, s7);                                 // or a1, s7, r0
  if (bc) {goto block_72;}                          // branch non-likely

  c->addiu(a0, r0, 6960);                           // addiu a0, r0, 6960
  get_fake_spad_addr2(a1, cache.fake_scratchpad_data, 0, c);// lui a1, 28672
  c->daddu(a0, a0, a1);                             // daddu a0, a0, a1
  c->mov64(a0, a0);                                 // or a0, a0, r0
  c->mov64(a1, a0);                                 // or a1, a0, r0

  block_72:
  c->sw(r0, 48, at);                                // sw r0, 48(at)
  c->sw(a0, 52, at);                                // sw a0, 52(at)
  c->sw(v1, 44, at);                                // sw v1, 44(at)
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(t9, 7468, v1);                             // lwu t9, 7468(v1)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  generic_prepare_dma_double::execute(c);
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(t9, 7472, v1);                             // lwu t9, 7472(v1)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  generic_light_proc::execute(c);
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(t9, 7476, v1);                             // lwu t9, 7476(v1)
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
//  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L59
//  // nop                                            // sll r0, r0, 0
//  if (bc) {goto block_76;}                          // branch non-likely
//
//  c->mov64(t1, a2);                                 // or t1, a2, r0
  // nop                                            // sll r0, r0, 0

//  c->lw(t2, 0, t0);                                 // lw t2, 0(t0)
//  // nop                                            // sll r0, r0, 0
//  c->lw(t3, 0, t1);                                 // lw t3, 0(t1)
//  // nop                                            // sll r0, r0, 0
//  c->andi(t3, t3, 256);                             // andi t3, t3, 256
//  c->daddiu(t2, t2, 1);                             // daddiu t2, t2, 1
//  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L58
//  c->sw(t2, 0, t0);                                 // sw t2, 0(t0)
//  if (bc) {goto block_74;}                          // branch non-likely

  c->gprs[t0].du64[0] = 0;                          // or t0, r0, r0

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
  c->xori(v1, v1, 4608);                            // xori v1, v1, 4608
  c->sw(v1, 24, at);                                // sw v1, 24(at)
  //beq r0, r0, L63                                 // beq r0, r0, L63
  // nop                                            // sll r0, r0, 0
  goto block_82;                                    // branch always


  block_77:
  get_fake_spad_addr2(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->mov64(v1, s1);                                 // or v1, s1, r0
  c->sw(r0, 48, at);                                // sw r0, 48(at)
  c->sw(v1, 44, at);                                // sw v1, 44(at)
  c->sh(r0, 56, at);                                // sh r0, 56(at)
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(t9, 7464, v1);                             // lwu t9, 7464(v1)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  generic_prepare_dma_single::execute_real(c);
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(t9, 7472, v1);                             // lwu t9, 7472(v1)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  // c->jalr(call_addr);                               // jalr ra, t9
  generic_light_proc::execute(c);
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
//  bc = c->sgpr64(t1) == 0;                          // beq t1, r0, L62
//  // nop                                            // sll r0, r0, 0
//  if (bc) {goto block_81;}                          // branch non-likely
//
//  c->mov64(t1, a2);                                 // or t1, a2, r0
//  // nop                                            // sll r0, r0, 0
//
//  block_79:
//  c->lw(t2, 0, t0);                                 // lw t2, 0(t0)
//  // nop                                            // sll r0, r0, 0
//  c->lw(t3, 0, t1);                                 // lw t3, 0(t1)
//  // nop                                            // sll r0, r0, 0
//  c->andi(t3, t3, 256);                             // andi t3, t3, 256
//  c->daddiu(t2, t2, 1);                             // daddiu t2, t2, 1
//  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L61
//  c->sw(t2, 0, t0);                                 // sw t2, 0(t0)
//  if (bc) {goto block_79;}                          // branch non-likely
//
//  c->gprs[t0].du64[0] = 0;                          // or t0, r0, r0

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
  c->xori(v1, v1, 4608);                            // xori v1, v1, 4608
  c->sw(v1, 24, at);                                // sw v1, 24(at)

  block_82:
  c->addiu(v1, r0, 1);                              // addiu v1, r0, 1
  c->dsubu(gp, v1, gp);                             // dsubu gp, v1, gp
  c->daddiu(s5, s5, 1);                             // daddiu s5, s5, 1
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lhu(v1, 6946, v1);                             // lhu v1, 6946(v1)
  bc = c->sgpr64(s5) != c->sgpr64(v1);              // bne s5, v1, L64
  c->mov64(v1, s7);                                 // or v1, s7, r0
  if (bc) {goto block_84;}                          // branch non-likely

  c->addiu(s5, r0, 0);                              // addiu s5, r0, 0
  c->mov64(v1, s5);                                 // or v1, s5, r0

  block_84:
  c->mov64(v1, s2);                                 // or v1, s2, r0

  block_85:
  bc = c->sgpr64(s2) != 0;                          // bne s2, r0, L22
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_4;}                           // branch non-likely

  c->mov64(v1, s7);                                 // or v1, s7, r0
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(v1, 7456, v1);                             // lwu v1, 7456(v1)
  c->sw(r0, 0, v1);                                 // sw r0, 0(v1)
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
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
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  cache.gsf_buffer = intern_from_c("*gsf-buffer*").c();
  cache.generic_merc_death = intern_from_c("generic-merc-death").c();
  cache.generic_merc_query = intern_from_c("generic-merc-query").c();
  cache.generic_translucent = intern_from_c("generic-translucent").c();
  cache.generic_warp_dest = intern_from_c("generic-warp-dest").c();
  cache.generic_warp_envmap_dest = intern_from_c("generic-warp-envmap-dest").c();
  cache.generic_warp_source = intern_from_c("generic-warp-source").c();
  gLinkedFunctionTable.reg("generic-merc-execute-asm", execute, 256);
}

} // namespace generic_merc_execute_asm
} // namespace Mips2C
// add generic_merc_execute_asm::link to the link callback table for the object file.
// FWD DEC:
//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
namespace generic_merc_do_chain {
struct Cache {
  void* display; // *display*
  void* fake_scratchpad_data; // *fake-scratchpad-data*
  void* dma_bucket_insert_tag; // dma-bucket-insert-tag
  void* generic_merc_execute_asm; // generic-merc-execute-asm
  void* generic_work_init; // generic-work-init
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  u32 call_addr = 0;
  c->daddiu(sp, sp, -80);                           // daddiu sp, sp, -80
  c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
  c->sq(s3, 16, sp);                                // sq s3, 16(sp)
  c->sq(s4, 32, sp);                                // sq s4, 32(sp)
  c->sq(s5, 48, sp);                                // sq s5, 48(sp)
  c->sq(gp, 64, sp);                                // sq gp, 64(sp)
  c->mov64(s5, a0);                                 // or s5, a0, r0
  c->mov64(gp, a1);                                 // or gp, a1, r0
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->sw(s5, 7456, v1);                              // sw s5, 7456(v1)
  c->daddiu(a0, s5, 8);                             // daddiu a0, s5, 8
  c->lwu(v1, 0, s5);                                // lwu v1, 0(s5)
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L18
  c->mov64(v0, s7);                                 // or v0, s7, r0
  if (bc) {goto block_5;}                           // branch non-likely

  c->load_symbol2(v1, cache.display);               // lw v1, *display*(s7)
  c->lw(v1, 0, v1);                                 // lw v1, 0(v1)
  c->dsll(v1, v1, 2);                               // dsll v1, v1, 2
  c->load_symbol2(a1, cache.display);               // lw a1, *display*(s7)
  c->daddu(v1, v1, a1);                             // daddu v1, v1, a1
  c->lwu(v1, 8, v1);                                // lwu v1, 8(v1)
  c->lwu(s3, 36, v1);                               // lwu s3, 36(v1)
  c->lwu(s4, 4, s3);                                // lwu s4, 4(s3)
  c->load_symbol2(t9, cache.generic_work_init);     // lw t9, generic-work-init(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->lwu(v1, 4, s3);                                // lwu v1, 4(s3)
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
  // printf("watch *0x%lx\n", (uintptr_t)(g_ee_main_mem + c->sgpr64(a0) + 60));
  c->sw(v1, 60, a0);                                // sw v1, 60(a0)
  c->load_symbol2(t9, cache.generic_merc_execute_asm);// lw t9, generic-merc-execute-asm(s7)
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  get_fake_spad_addr2(v1, cache.fake_scratchpad_data, 0, c);// lui v1, 28672
  c->lwu(v1, 60, v1);                               // lwu v1, 60(v1)
  c->sw(v1, 4, s3);                                 // sw v1, 4(s3)
  c->lui(v1, 4096);                                 // lui v1, 4096
  c->ori(v1, v1, 53248);                            // ori v1, v1, 53248
  get_fake_spad_addr2(a0, cache.fake_scratchpad_data, 0, c);// lui a0, 28672
//  c->ori(a0, a0, 92);                               // ori a0, a0, 92
//  c->lw(a1, 0, v1);                                 // lw a1, 0(v1)
//  c->andi(a1, a1, 256);                             // andi a1, a1, 256
//  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L17
//  DANGER jump to delay slot, this MUST be fixed manually!
//      c->lw(a1, 0, a0);                                 // lw a1, 0(a0)
//  if (bc) {goto block_-1;}                          // branch non-likely
//
//
//  block_2:
//  c->lw(a1, 0, a0);                                 // lw a1, 0(a0)
//  // nop                                            // sll r0, r0, 0
//  c->lw(a2, 0, v1);                                 // lw a2, 0(v1)
//  // nop                                            // sll r0, r0, 0
//  c->andi(a2, a2, 256);                             // andi a2, a2, 256
//  c->daddiu(a1, a1, 1);                             // daddiu a1, a1, 1
//  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L16
//  c->sw(a1, 0, a0);                                 // sw a1, 0(a0)
//  if (bc) {goto block_2;}                           // branch non-likely

  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->lwu(a3, 4, s3);                                // lwu a3, 4(s3)
  c->lwu(v1, 4, s3);                                // lwu v1, 4(s3)
  c->lui(a0, 8192);                                 // lui a0, 8192
  c->sd(a0, 0, v1);                                 // sd a0, 0(v1)
  c->sw(r0, 8, v1);                                 // sw r0, 8(v1)
  c->sw(r0, 12, v1);                                // sw r0, 12(v1)
  c->daddiu(v1, v1, 16);                            // daddiu v1, v1, 16
  c->sw(v1, 4, s3);                                 // sw v1, 4(s3)
  c->load_symbol2(t9, cache.dma_bucket_insert_tag); // lw t9, dma-bucket-insert-tag(s7)
  c->load_symbol2(v1, cache.display);               // lw v1, *display*(s7)
  c->lw(v1, 0, v1);                                 // lw v1, 0(v1)
  c->dsll(v1, v1, 2);                               // dsll v1, v1, 2
  c->load_symbol2(a0, cache.display);               // lw a0, *display*(s7)
  c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
  c->lwu(v1, 8, v1);                                // lwu v1, 8(v1)
  c->lwu(a0, 40, v1);                               // lwu a0, 40(v1)
  c->lw(a1, 16, s5);                                // lw a1, 16(s5)
  c->mov64(a2, s4);                                 // or a2, s4, r0
  call_addr = c->gprs[t9].du32[0];                  // function call:
  c->sll(v0, ra, 0);                                // sll v0, ra, 0
  c->jalr(call_addr);                               // jalr ra, t9
  c->mov64(v1, v0);                                 // or v1, v0, r0
  c->lwu(v1, 4, gp);                                // lwu v1, 4(gp)
  // Unknown instr: sync.l
  // Unknown instr: cache dxwbin v1, 0
  // Unknown instr: sync.l
  // Unknown instr: cache dxwbin v1, 1
  // Unknown instr: sync.l
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0

  block_5:
  c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
  c->lq(gp, 64, sp);                                // lq gp, 64(sp)
  c->lq(s5, 48, sp);                                // lq s5, 48(sp)
  c->lq(s4, 32, sp);                                // lq s4, 32(sp)
  c->lq(s3, 16, sp);                                // lq s3, 16(sp)
  //jr ra                                           // jr ra
  c->daddiu(sp, sp, 80);                            // daddiu sp, sp, 80
  goto end_of_function;                             // return

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  end_of_function:
  return c->gprs[v0].du64[0];
}

void link() {
  cache.display = intern_from_c("*display*").c();
  cache.fake_scratchpad_data = intern_from_c("*fake-scratchpad-data*").c();
  cache.dma_bucket_insert_tag = intern_from_c("dma-bucket-insert-tag").c();
  cache.generic_merc_execute_asm = intern_from_c("generic-merc-execute-asm").c();
  cache.generic_work_init = intern_from_c("generic-work-init").c();
  gLinkedFunctionTable.reg("generic-merc-do-chain", execute, 128);
}

} // namespace generic_merc_do_chain
} // namespace Mips2C
// add generic_merc_do_chain::link to the link callback table for the object file.
// FWD DEC:
