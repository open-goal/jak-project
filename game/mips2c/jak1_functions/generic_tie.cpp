// clang-format off
//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak1/kscheme.h"
using namespace jak1;
namespace Mips2C::jak1 {
namespace generic_tie_dma_to_spad_sync {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  // nop                                            // sll r0, r0, 0
  get_fake_spad_addr(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->ori(a2, r0, 65535);                            // ori a2, r0, 65535
  c->lui(v1, 4096);                                 // lui v1, 4096
  // nop                                            // sll r0, r0, 0
  c->ori(v1, v1, 54272);                            // ori v1, v1, 54272 // SPR TO
  c->and_(a2, a1, a2);                              // and a2, a1, a2
  /*
  block_1:
  c->lw(a3, 0, v1);                                 // lw a3, 0(v1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(a3, a3, 256);                             // andi a3, a3, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L90
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_3;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(a1, a1, 1);                             // daddiu a1, a1, 1
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L89                                 // beq r0, r0, L89
  // nop                                            // sll r0, r0, 0
  goto block_1;                                     // branch always
  */


  c->addiu(a3, r0, 324);                            // addiu a3, r0, 324
  // c->sw(a2, 128, v1);                               // sw a2, 128(v1)
  u32 sadr = c->sgpr64(a2);
  // c->sw(a0, 48, v1);                                // sw a0, 48(v1)
  u32 tadr = c->sgpr64(a0);
  // c->sw(r0, 32, v1);                                // sw r0, 32(v1)
  // Unknown instr: sync.l
  // c->sw(a3, 0, v1);                                 // sw a3, 0(v1)
  // same and hack as generic merc.
  spad_to_dma_blerc_chain(cache.fake_scratchpad_data, sadr & 0x3fff, tadr);

  /*
  block_4:
  c->lw(a0, 0, v1);                                 // lw a0, 0(v1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(a0, a0, 256);                             // andi a0, a0, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L92
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_6;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(a1, a1, 1);                             // daddiu a1, a1, 1
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L91                                 // beq r0, r0, L91
  // nop                                            // sll r0, r0, 0
  goto block_4;                                     // branch always
   */


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
  gLinkedFunctionTable.reg("generic-tie-dma-to-spad-sync", execute, 128);
}

} // namespace generic_tie_dma_to_spad_sync
} // namespace Mips2C

//--------------------------MIPS2C---------------------
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak1 {

namespace generic_prepare_dma_double {
extern u64 execute(void* ctxt);
}

namespace generic_envmap_dproc {
extern u64 execute(void* ctxt);
}
namespace generic_interp_dproc {
extern u64 execute(void* ctxt);
}
namespace generic_no_light_dproc {
extern u64 execute(void* ctxt);
}

namespace generic_tie_convert {
struct Cache {
  void* fake_scratchpad_data; // *fake-scratchpad-data*
} cache;

u16 vis[16];

u8 vu0_data_mem[1024 * 4];

void sq_buffer(Mask mask, const Vf& data, u32 qw) {
  ASSERT(qw * 16 < sizeof(vu0_data_mem));
  for (int i = 0; i < 4; i++) {
    if ((u64)mask & (1 << i)) {
      memcpy(vu0_data_mem + qw * 16 + i * 4, data.data + i, 4);
    }
  }
}

void lq_buffer(Mask mask, Vf& data, u32 qw) {
  ASSERT(qw * 16 < sizeof(vu0_data_mem));
  for (int i = 0; i < 4; i++) {
    if ((u64)mask & (1 << i)) {
      memcpy(data.data + i, vu0_data_mem + qw * 16 + i * 4, 4);
    }
  }
}


void vcallms_104() {
  // iaddi vi02, vi00, 0x0      |  nop                            104
  vis[vi02] = 0;
  // iadd vi03, vi02, vi08      |  nop                            105
  vis[vi03] = vis[vi02] + vis[vi08];
  // iaddiu vi03, vi03, 0x10    |  nop                            106
  vis[vi03] = vis[vi03] + 0x10; /* 16 */
  // iadd vi01, vi03, vi09      |  nop                            107
  vis[vi01] = vis[vi03] + vis[vi09];
  // iadd vi01, vi01, vi09      |  nop                            108
  vis[vi01] = vis[vi01] + vis[vi09];
  // iaddiu vi01, vi01, 0x10    |  nop                            109
  vis[vi01] = vis[vi01] + 0x10; /* 16 */
  // iaddi vi10, vi00, 0x0      |  nop                            110
  vis[vi10] = 0;
  // ior vi11, vi03, vi00       |  nop                            111
  vis[vi11] = vis[vi03];
  // ior vi12, vi01, vi00       |  nop :e                         112
  vis[vi12] = vis[vi01];
  // iadd vi13, vi01, vi08      |  nop                            113
  vis[vi13] = vis[vi01] + vis[vi08];
}

void vcallms_114(ExecutionContext* c) {
  // nop                        |  itof12.xyzw vf09, vf05         114
  c->vfs[vf09].vf.itof12(Mask::xyzw, c->vf_src(vf05).vf);
  // nop                        |  itof12.xyzw vf10, vf06         115
  c->vfs[vf10].vf.itof12(Mask::xyzw, c->vf_src(vf06).vf);
  // nop                        |  itof12.xyzw vf11, vf07         116
  c->vfs[vf11].vf.itof12(Mask::xyzw, c->vf_src(vf07).vf);
  // nop                        |  itof12.xyzw vf12, vf08         117
  c->vfs[vf12].vf.itof12(Mask::xyzw, c->vf_src(vf08).vf);
  // sqi.xyzw vf09, vi01        |  nop                            118
  sq_buffer(Mask::xyzw, c->vf_src(vf09).vf, vis[vi01]++);
  // sqi.xyzw vf10, vi01        |  nop                            119
  sq_buffer(Mask::xyzw, c->vf_src(vf10).vf, vis[vi01]++);
  // sqi.xyzw vf11, vi01        |  nop :e                         120
  sq_buffer(Mask::xyzw, c->vf_src(vf11).vf, vis[vi01]++);
  // sqi.xyzw vf12, vi01        |  nop                            121
  sq_buffer(Mask::xyzw, c->vf_src(vf12).vf, vis[vi01]++);
}

void vcallms_122(ExecutionContext* c) {
  // mr32.w vf17, vf13          |  itof0.xyz vf09, vf01           122
  c->vfs[vf09].vf.itof0(Mask::xyz, c->vf_src(vf01).vf);   c->vfs[vf17].vf.mr32(Mask::w, c->vf_src(vf13).vf);
  // mr32.w vf18, vf14          |  itof0.xyz vf10, vf02           123
  c->vfs[vf10].vf.itof0(Mask::xyz, c->vf_src(vf02).vf);   c->vfs[vf18].vf.mr32(Mask::w, c->vf_src(vf14).vf);
  // mr32.w vf19, vf15          |  itof0.xyz vf11, vf03           124
  c->vfs[vf11].vf.itof0(Mask::xyz, c->vf_src(vf03).vf);   c->vfs[vf19].vf.mr32(Mask::w, c->vf_src(vf15).vf);
  // mr32.w vf20, vf16          |  itof0.xyz vf12, vf04           125
  c->vfs[vf12].vf.itof0(Mask::xyz, c->vf_src(vf04).vf);   c->vfs[vf20].vf.mr32(Mask::w, c->vf_src(vf16).vf);
  // move.w vf09, vf17          |  nop                            126
  c->vfs[vf09].vf.move(Mask::w, c->vf_src(vf17).vf);
  // move.w vf10, vf18          |  nop                            127
  c->vfs[vf10].vf.move(Mask::w, c->vf_src(vf18).vf);
  // move.w vf11, vf19          |  nop                            128
  c->vfs[vf11].vf.move(Mask::w, c->vf_src(vf19).vf);
  // move.w vf12, vf20          |  nop                            129
  c->vfs[vf12].vf.move(Mask::w, c->vf_src(vf20).vf);
  // sqi.xyzw vf09, vi02        |  nop                            130
  sq_buffer(Mask::xyzw, c->vf_src(vf09).vf, vis[vi02]++);
  // sqi.xyzw vf10, vi02        |  nop                            131
  sq_buffer(Mask::xyzw, c->vf_src(vf10).vf, vis[vi02]++);
  // sqi.xyzw vf11, vi02        |  nop :e                         132
  sq_buffer(Mask::xyzw, c->vf_src(vf11).vf, vis[vi02]++);
  // sqi.xyzw vf12, vi02        |  nop                            133
  sq_buffer(Mask::xyzw, c->vf_src(vf12).vf, vis[vi02]++);
}

void vcallms_134(ExecutionContext* c) {
  // mr32.w vf17, vf13          |  itof0.xyz vf09, vf01           134
  c->vfs[vf09].vf.itof0(Mask::xyz, c->vf_src(vf01).vf);   c->vfs[vf17].vf.mr32(Mask::w, c->vf_src(vf13).vf);
  // mr32.w vf18, vf14          |  itof0.xyz vf10, vf02           135
  c->vfs[vf10].vf.itof0(Mask::xyz, c->vf_src(vf02).vf);   c->vfs[vf18].vf.mr32(Mask::w, c->vf_src(vf14).vf);
  // nop                        |  itof0.xyz vf11, vf03           136
  c->vfs[vf11].vf.itof0(Mask::xyz, c->vf_src(vf03).vf);
  // nop                        |  nop                            137

  // move.w vf09, vf17          |  nop                            138
  c->vfs[vf09].vf.move(Mask::w, c->vf_src(vf17).vf);
  // move.w vf10, vf18          |  nop                            139
  c->vfs[vf10].vf.move(Mask::w, c->vf_src(vf18).vf);
  // nop                        |  nop                            140

  // iaddi vi03, vi03, 0x2      |  nop                            141
  vis[vi03] = vis[vi03] + 2;
  // sqi.xyzw vf09, vi02        |  nop                            142
  sq_buffer(Mask::xyzw, c->vf_src(vf09).vf, vis[vi02]++);
  // sq.xyzw vf10, -2(vi03)     |  nop :e                         143
  sq_buffer(Mask::xyzw, c->vf_src(vf10).vf, vis[vi03] + -2);
  // sq.xyzw vf11, -1(vi03)     |  nop                            144
  sq_buffer(Mask::xyzw, c->vf_src(vf11).vf, vis[vi03] + -1);
}

void vcallms_145(ExecutionContext* c) {
  // mr32.w vf17, vf13          |  itof0.xyz vf09, vf01           145
  c->vfs[vf09].vf.itof0(Mask::xyz, c->vf_src(vf01).vf);   c->vfs[vf17].vf.mr32(Mask::w, c->vf_src(vf13).vf);
  // mr32.w vf18, vf14          |  itof0.xyz vf10, vf02           146
  c->vfs[vf10].vf.itof0(Mask::xyz, c->vf_src(vf02).vf);   c->vfs[vf18].vf.mr32(Mask::w, c->vf_src(vf14).vf);
  // mr32.w vf19, vf15          |  itof0.xyz vf11, vf03           147
  c->vfs[vf11].vf.itof0(Mask::xyz, c->vf_src(vf03).vf);   c->vfs[vf19].vf.mr32(Mask::w, c->vf_src(vf15).vf);
  // nop                        |  itof0.xyz vf12, vf04           148
  c->vfs[vf12].vf.itof0(Mask::xyz, c->vf_src(vf04).vf);
  // move.w vf09, vf17          |  nop                            149
  c->vfs[vf09].vf.move(Mask::w, c->vf_src(vf17).vf);
  // move.w vf10, vf18          |  nop                            150
  c->vfs[vf10].vf.move(Mask::w, c->vf_src(vf18).vf);
  // move.w vf11, vf19          |  nop                            151
  c->vfs[vf11].vf.move(Mask::w, c->vf_src(vf19).vf);
  // iaddi vi03, vi03, 0x2      |  nop                            152
  vis[vi03] = vis[vi03] + 2;
  // sqi.xyzw vf09, vi02        |  nop                            153
  sq_buffer(Mask::xyzw, c->vf_src(vf09).vf, vis[vi02]++);
  // sqi.xyzw vf10, vi02        |  nop                            154
  sq_buffer(Mask::xyzw, c->vf_src(vf10).vf, vis[vi02]++);
  // sq.xyzw vf11, -2(vi03)     |  nop :e                         155
  sq_buffer(Mask::xyzw, c->vf_src(vf11).vf, vis[vi03] + -2);
  // sq.xyzw vf12, -1(vi03)     |  nop                            156
  sq_buffer(Mask::xyzw, c->vf_src(vf12).vf, vis[vi03] + -1);
}

void vcallms_157(ExecutionContext* c) {
  // mr32.w vf17, vf13          |  itof0.xyz vf09, vf01           157
  c->vfs[vf09].vf.itof0(Mask::xyz, c->vf_src(vf01).vf);   c->vfs[vf17].vf.mr32(Mask::w, c->vf_src(vf13).vf);
  // mr32.w vf18, vf14          |  itof0.xyz vf10, vf02           158
  c->vfs[vf10].vf.itof0(Mask::xyz, c->vf_src(vf02).vf);   c->vfs[vf18].vf.mr32(Mask::w, c->vf_src(vf14).vf);
  // mr32.w vf19, vf15          |  itof0.xyz vf11, vf03           159
  c->vfs[vf11].vf.itof0(Mask::xyz, c->vf_src(vf03).vf);   c->vfs[vf19].vf.mr32(Mask::w, c->vf_src(vf15).vf);
  // nop                        |  nop                            160

  // move.w vf09, vf17          |  nop                            161
  c->vfs[vf09].vf.move(Mask::w, c->vf_src(vf17).vf);
  // move.w vf10, vf18          |  nop                            162
  c->vfs[vf10].vf.move(Mask::w, c->vf_src(vf18).vf);
  // move.w vf11, vf19          |  nop                            163
  c->vfs[vf11].vf.move(Mask::w, c->vf_src(vf19).vf);
  // nop                        |  nop                            164

  // sqi.xyzw vf09, vi02        |  nop                            165
  sq_buffer(Mask::xyzw, c->vf_src(vf09).vf, vis[vi02]++);
  // sqi.xyzw vf10, vi02        |  nop :e                         166
  sq_buffer(Mask::xyzw, c->vf_src(vf10).vf, vis[vi02]++);
  // sqi.xyzw vf11, vi02        |  nop                            167
  sq_buffer(Mask::xyzw, c->vf_src(vf11).vf, vis[vi02]++);
}

void vcallms_168(ExecutionContext* c) {
  // mr32.w vf17, vf13          |  itof0.xyz vf09, vf01           168
  c->vfs[vf09].vf.itof0(Mask::xyz, c->vf_src(vf01).vf);   c->vfs[vf17].vf.mr32(Mask::w, c->vf_src(vf13).vf);
  // mr32.w vf19, vf14          |  itof0.xyz vf10, vf02           169
  c->vfs[vf10].vf.itof0(Mask::xyz, c->vf_src(vf02).vf);   c->vfs[vf19].vf.mr32(Mask::w, c->vf_src(vf14).vf);
  // nop                        |  itof0.xyz vf11, vf03           170
  c->vfs[vf11].vf.itof0(Mask::xyz, c->vf_src(vf03).vf);
  // iaddi vi03, vi03, 0x4      |  itof0.xyz vf12, vf04           171
  c->vfs[vf12].vf.itof0(Mask::xyz, c->vf_src(vf04).vf);   vis[vi03] = vis[vi03] + 4;
  // move.w vf09, vf17          |  nop                            172
  c->vfs[vf09].vf.move(Mask::w, c->vf_src(vf17).vf);
  // move.w vf11, vf19          |  nop                            173
  c->vfs[vf11].vf.move(Mask::w, c->vf_src(vf19).vf);
  // sq.xyz vf10, -3(vi03)      |  nop                            174
  sq_buffer(Mask::xyz, c->vf_src(vf10).vf, vis[vi03] + -3);
  // sq.xyz vf12, -1(vi03)      |  nop                            175
  sq_buffer(Mask::xyz, c->vf_src(vf12).vf, vis[vi03] + -1);
  // sq.xyzw vf09, -4(vi03)     |  nop :e                         176
  sq_buffer(Mask::xyzw, c->vf_src(vf09).vf, vis[vi03] + -4);
  // sq.xyzw vf11, -2(vi03)     |  nop                            177
  sq_buffer(Mask::xyzw, c->vf_src(vf11).vf, vis[vi03] + -2);
}

void vcallms_183(ExecutionContext* c);

void vcallms_178(ExecutionContext* c) {
  // ior vi02, vi10, vi00       |  nop                            178
  vis[vi02] = vis[vi10];
  // ior vi01, vi12, vi00       |  nop                            179
  vis[vi01] = vis[vi12];
  // lqi.xyzw vf05, vi02        |  nop                            180
  lq_buffer(Mask::xyzw, c->vfs[vf05].vf, vis[vi02]++);
  // nop                        |  nop                            181

  // nop                        |  nop                            182
  vcallms_183(c);
}

void vcallms_183(ExecutionContext* c) {
  // lqi.xyzw vf09, vi01        |  mulaw.xyzw ACC, vf04, vf00     183
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf04).vf, c->vf_src(vf00).vf.w());   lq_buffer(Mask::xyzw, c->vfs[vf09].vf, vis[vi01]++);
  // move.xyzw vf19, vf21       |  maddax.xyzw ACC, vf01, vf05    184
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf01].vf, c->vfs[vf05].vf.x());   c->vfs[vf19].vf.move(Mask::xyzw, c->vf_src(vf21).vf);
  // move.xyzw vf16, vf22       |  madday.xyzw ACC, vf02, vf05    185
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf02].vf, c->vfs[vf05].vf.y());   c->vfs[vf16].vf.move(Mask::xyzw, c->vf_src(vf22).vf);
  // move.xyzw vf20, vf23       |  maddz.xyz vf13, vf03, vf05     186
  c->acc.vf.madd(Mask::xyz, c->vfs[vf13].vf, c->vf_src(vf03).vf, c->vf_src(vf05).vf.z());   c->vfs[vf20].vf.move(Mask::xyzw, c->vf_src(vf23).vf);
  // lqi.xyzw vf06, vi02        |  mulax.xyzw ACC, vf01, vf09     187
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf01).vf, c->vf_src(vf09).vf.x());   lq_buffer(Mask::xyzw, c->vfs[vf06].vf, vis[vi02]++);
  // move.xyzw vf15, vf24       |  madday.xyzw ACC, vf02, vf09    188
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf02].vf, c->vfs[vf09].vf.y());   c->vfs[vf15].vf.move(Mask::xyzw, c->vf_src(vf24).vf);
  // nop                        |  maddz.xyzw vf17, vf03, vf09    189
  c->acc.vf.madd(Mask::xyzw, c->vfs[vf17].vf, c->vf_src(vf03).vf, c->vf_src(vf09).vf.z());
  // lqi.xyzw vf10, vi01        |  mulaw.xyzw ACC, vf04, vf00     190
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf04).vf, c->vf_src(vf00).vf.w());   lq_buffer(Mask::xyzw, c->vfs[vf10].vf, vis[vi01]++);
  // move.w vf13, vf05          |  maddax.xyzw ACC, vf01, vf06    191
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf01].vf, c->vfs[vf06].vf.x());   c->vfs[vf13].vf.move(Mask::w, c->vf_src(vf05).vf);
  // nop                        |  madday.xyzw ACC, vf02, vf06    192
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf02].vf, c->vfs[vf06].vf.y());
  // nop                        |  maddz.xyz vf14, vf03, vf06     193
  c->acc.vf.madd(Mask::xyz, c->vfs[vf14].vf, c->vf_src(vf03).vf, c->vf_src(vf06).vf.z());
  // lqi.xyzw vf07, vi02        |  mulax.xyzw ACC, vf01, vf10     194
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf01).vf, c->vf_src(vf10).vf.x());   lq_buffer(Mask::xyzw, c->vfs[vf07].vf, vis[vi02]++);
  // nop                        |  madday.xyzw ACC, vf02, vf10    195
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf02].vf, c->vfs[vf10].vf.y());
  // nop                        |  maddz.xyzw vf18, vf03, vf10    196
  c->acc.vf.madd(Mask::xyzw, c->vfs[vf18].vf, c->vf_src(vf03).vf, c->vf_src(vf10).vf.z());
  // lqi.xyzw vf11, vi01        |  mulaw.xyzw ACC, vf04, vf00     197
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf04).vf, c->vf_src(vf00).vf.w());   lq_buffer(Mask::xyzw, c->vfs[vf11].vf, vis[vi01]++);
  // move.w vf14, vf06          |  maddax.xyzw ACC, vf01, vf07    198
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf01].vf, c->vfs[vf07].vf.x());   c->vfs[vf14].vf.move(Mask::w, c->vf_src(vf06).vf);
  // nop                        |  madday.xyzw ACC, vf02, vf07    199
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf02].vf, c->vfs[vf07].vf.y());
  // nop                        |  maddz.xyz vf24, vf03, vf07     200
  c->acc.vf.madd(Mask::xyz, c->vfs[vf24].vf, c->vf_src(vf03).vf, c->vf_src(vf07).vf.z());
  // lqi.xyzw vf08, vi02        |  mulax.xyzw ACC, vf01, vf11     201
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf01).vf, c->vf_src(vf11).vf.x());   lq_buffer(Mask::xyzw, c->vfs[vf08].vf, vis[vi02]++);
  // nop                        |  madday.xyzw ACC, vf02, vf11    202
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf02].vf, c->vfs[vf11].vf.y());
  // nop                        |  maddz.xyzw vf21, vf03, vf11    203
  c->acc.vf.madd(Mask::xyzw, c->vfs[vf21].vf, c->vf_src(vf03).vf, c->vf_src(vf11).vf.z());
  // lqi.xyzw vf12, vi01        |  mulaw.xyzw ACC, vf04, vf00     204
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf04).vf, c->vf_src(vf00).vf.w());   lq_buffer(Mask::xyzw, c->vfs[vf12].vf, vis[vi01]++);
  // move.w vf24, vf07          |  maddax.xyzw ACC, vf01, vf08    205
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf01].vf, c->vfs[vf08].vf.x());   c->vfs[vf24].vf.move(Mask::w, c->vf_src(vf07).vf);
  // nop                        |  madday.xyzw ACC, vf02, vf08    206
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf02].vf, c->vfs[vf08].vf.y());
  // nop                        |  maddz.xyz vf22, vf03, vf08     207
  c->acc.vf.madd(Mask::xyz, c->vfs[vf22].vf, c->vf_src(vf03).vf, c->vf_src(vf08).vf.z());
  // lqi.xyzw vf05, vi02        |  mulax.xyzw ACC, vf01, vf12     208
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf01).vf, c->vf_src(vf12).vf.x());   lq_buffer(Mask::xyzw, c->vfs[vf05].vf, vis[vi02]++);
  // move.w vf22, vf08          |  madday.xyzw ACC, vf02, vf12 :e 209
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf02].vf, c->vfs[vf12].vf.y());   c->vfs[vf22].vf.move(Mask::w, c->vf_src(vf08).vf);
  // nop                        |  maddz.xyzw vf23, vf03, vf12    210
  c->acc.vf.madd(Mask::xyzw, c->vfs[vf23].vf, c->vf_src(vf03).vf, c->vf_src(vf12).vf.z());
}

void vcallms_211(ExecutionContext* c) {
  // move.xyzw vf19, vf21       |  nop                            211
  c->vfs[vf19].vf.move(Mask::xyzw, c->vf_src(vf21).vf);
  // move.xyzw vf16, vf22       |  nop                            212
  c->vfs[vf16].vf.move(Mask::xyzw, c->vf_src(vf22).vf);
  // move.xyzw vf20, vf23       |  nop :e                         213
  c->vfs[vf20].vf.move(Mask::xyzw, c->vf_src(vf23).vf);
  // move.xyzw vf15, vf24       |  nop                            214
  c->vfs[vf15].vf.move(Mask::xyzw, c->vf_src(vf24).vf);
}
void vcallms_221(ExecutionContext* c);

void vcallms_215(ExecutionContext* c) {
  // ior vi03, vi11, vi00       |  nop                            215
  vis[vi03] = vis[vi11];
  // ior vi01, vi13, vi00       |  nop                            216
  vis[vi01] = vis[vi13];
  // lqi.xyzw vf05, vi03        |  nop                            217
  lq_buffer(Mask::xyzw, c->vfs[vf05].vf, vis[vi03]++);
  // lqi.xyzw vf25, vi03        |  nop                            218
  lq_buffer(Mask::xyzw, c->vfs[vf25].vf, vis[vi03]++);
  // lqi.xyzw vf09, vi01        |  nop                            219
  lq_buffer(Mask::xyzw, c->vfs[vf09].vf, vis[vi01]++);
  // nop                        |  nop                            220
  vcallms_221(c);
}

void vcallms_221(ExecutionContext* c) {
  // move.xyzw vf16, vf21       |  mulax.xyz ACC, vf05, vf29      221
  c->acc.vf.mula(Mask::xyz, c->vf_src(vf05).vf, c->vf_src(vf29).vf.x());   c->vfs[vf16].vf.move(Mask::xyzw, c->vf_src(vf21).vf);
  // nop                        |  maddw.xyz vf30, vf25, vf00     222
  c->acc.vf.madd(Mask::xyz, c->vfs[vf30].vf, c->vf_src(vf25).vf, c->vf_src(vf00).vf.w());
  // nop                        |  mulax.xyzw ACC, vf01, vf09     223
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf01).vf, c->vf_src(vf09).vf.x());
  // nop                        |  madday.xyzw ACC, vf02, vf09    224
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf02].vf, c->vfs[vf09].vf.y());
  // move.w vf13, vf05          |  maddz.xyz vf17, vf03, vf09     225
  c->acc.vf.madd(Mask::xyz, c->vfs[vf17].vf, c->vf_src(vf03).vf, c->vf_src(vf09).vf.z());   c->vfs[vf13].vf.move(Mask::w, c->vf_src(vf05).vf);
  // lqi.xyzw vf06, vi03        |  mulaw.xyzw ACC, vf04, vf00     226
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf04).vf, c->vf_src(vf00).vf.w());   lq_buffer(Mask::xyzw, c->vfs[vf06].vf, vis[vi03]++);
  // lqi.xyzw vf26, vi03        |  maddax.xyzw ACC, vf01, vf30    227
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf01].vf, c->vfs[vf30].vf.x());   lq_buffer(Mask::xyzw, c->vfs[vf26].vf, vis[vi03]++);
  // lqi.xyzw vf10, vi01        |  madday.xyzw ACC, vf02, vf30    228
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf02].vf, c->vfs[vf30].vf.y());   lq_buffer(Mask::xyzw, c->vfs[vf10].vf, vis[vi01]++);
  // nop                        |  maddz.xyz vf13, vf03, vf30     229
  c->acc.vf.madd(Mask::xyz, c->vfs[vf13].vf, c->vf_src(vf03).vf, c->vf_src(vf30).vf.z());
  // nop                        |  mulax.xyz ACC, vf06, vf29      230
  c->acc.vf.mula(Mask::xyz, c->vf_src(vf06).vf, c->vf_src(vf29).vf.x());
  // nop                        |  maddw.xyz vf30, vf26, vf00     231
  c->acc.vf.madd(Mask::xyz, c->vfs[vf30].vf, c->vf_src(vf26).vf, c->vf_src(vf00).vf.w());
  // nop                        |  mulax.xyzw ACC, vf01, vf10     232
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf01).vf, c->vf_src(vf10).vf.x());
  // nop                        |  madday.xyzw ACC, vf02, vf10    233
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf02].vf, c->vfs[vf10].vf.y());
  // move.w vf14, vf06          |  maddz.xyz vf18, vf03, vf10     234
  c->acc.vf.madd(Mask::xyz, c->vfs[vf18].vf, c->vf_src(vf03).vf, c->vf_src(vf10).vf.z());   c->vfs[vf14].vf.move(Mask::w, c->vf_src(vf06).vf);
  // lqi.xyzw vf07, vi03        |  mulaw.xyzw ACC, vf04, vf00     235
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf04).vf, c->vf_src(vf00).vf.w());   lq_buffer(Mask::xyzw, c->vfs[vf07].vf, vis[vi03]++);
  // lqi.xyzw vf27, vi03        |  maddax.xyzw ACC, vf01, vf30    236
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf01].vf, c->vfs[vf30].vf.x());   lq_buffer(Mask::xyzw, c->vfs[vf27].vf, vis[vi03]++);
  // lqi.xyzw vf11, vi01        |  madday.xyzw ACC, vf02, vf30    237
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf02].vf, c->vfs[vf30].vf.y());   lq_buffer(Mask::xyzw, c->vfs[vf11].vf, vis[vi01]++);
  // nop                        |  maddz.xyz vf14, vf03, vf30     238
  c->acc.vf.madd(Mask::xyz, c->vfs[vf14].vf, c->vf_src(vf03).vf, c->vf_src(vf30).vf.z());
  // nop                        |  mulax.xyz ACC, vf07, vf29      239
  c->acc.vf.mula(Mask::xyz, c->vf_src(vf07).vf, c->vf_src(vf29).vf.x());
  // nop                        |  maddw.xyz vf30, vf27, vf00     240
  c->acc.vf.madd(Mask::xyz, c->vfs[vf30].vf, c->vf_src(vf27).vf, c->vf_src(vf00).vf.w());
  // nop                        |  mulax.xyzw ACC, vf01, vf11     241
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf01).vf, c->vf_src(vf11).vf.x());
  // nop                        |  madday.xyzw ACC, vf02, vf11    242
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf02].vf, c->vfs[vf11].vf.y());
  // move.w vf15, vf07          |  maddz.xyz vf19, vf03, vf11     243
  c->acc.vf.madd(Mask::xyz, c->vfs[vf19].vf, c->vf_src(vf03).vf, c->vf_src(vf11).vf.z());   c->vfs[vf15].vf.move(Mask::w, c->vf_src(vf07).vf);
  // lqi.xyzw vf08, vi03        |  mulaw.xyzw ACC, vf04, vf00     244
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf04).vf, c->vf_src(vf00).vf.w());   lq_buffer(Mask::xyzw, c->vfs[vf08].vf, vis[vi03]++);
  // lqi.xyzw vf28, vi03        |  maddax.xyzw ACC, vf01, vf30    245
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf01].vf, c->vfs[vf30].vf.x());   lq_buffer(Mask::xyzw, c->vfs[vf28].vf, vis[vi03]++);
  // lqi.xyzw vf12, vi01        |  madday.xyzw ACC, vf02, vf30    246
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf02].vf, c->vfs[vf30].vf.y());   lq_buffer(Mask::xyzw, c->vfs[vf12].vf, vis[vi01]++);
  // nop                        |  maddz.xyz vf15, vf03, vf30     247
  c->acc.vf.madd(Mask::xyz, c->vfs[vf15].vf, c->vf_src(vf03).vf, c->vf_src(vf30).vf.z());
  // nop                        |  mulax.xyz ACC, vf08, vf29      248
  c->acc.vf.mula(Mask::xyz, c->vf_src(vf08).vf, c->vf_src(vf29).vf.x());
  // nop                        |  maddw.xyz vf30, vf28, vf00     249
  c->acc.vf.madd(Mask::xyz, c->vfs[vf30].vf, c->vf_src(vf28).vf, c->vf_src(vf00).vf.w());
  // nop                        |  mulax.xyzw ACC, vf01, vf12     250
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf01).vf, c->vf_src(vf12).vf.x());
  // nop                        |  madday.xyzw ACC, vf02, vf12    251
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf02].vf, c->vfs[vf12].vf.y());
  // move.w vf21, vf08          |  maddz.xyz vf20, vf03, vf12     252
  c->acc.vf.madd(Mask::xyz, c->vfs[vf20].vf, c->vf_src(vf03).vf, c->vf_src(vf12).vf.z());   c->vfs[vf21].vf.move(Mask::w, c->vf_src(vf08).vf);
  // lqi.xyzw vf05, vi03        |  mulaw.xyzw ACC, vf04, vf00     253
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf04).vf, c->vf_src(vf00).vf.w());   lq_buffer(Mask::xyzw, c->vfs[vf05].vf, vis[vi03]++);
  // lqi.xyzw vf25, vi03        |  maddax.xyzw ACC, vf01, vf30    254
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf01].vf, c->vfs[vf30].vf.x());   lq_buffer(Mask::xyzw, c->vfs[vf25].vf, vis[vi03]++);
  // lqi.xyzw vf09, vi01        |  madday.xyzw ACC, vf02, vf30 :e 255
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf02].vf, c->vfs[vf30].vf.y());   lq_buffer(Mask::xyzw, c->vfs[vf09].vf, vis[vi01]++);
  // nop                        |  maddz.xyz vf21, vf03, vf30     256
  c->acc.vf.madd(Mask::xyz, c->vfs[vf21].vf, c->vf_src(vf03).vf, c->vf_src(vf30).vf.z());
}

void vcallms_257(ExecutionContext* c) {
  // move.xyzw vf16, vf21       |  nop :e                         257
  c->vfs[vf16].vf.move(Mask::xyzw, c->vf_src(vf21).vf);
  // nop                        |  nop                            258
}

void vcallms_264(ExecutionContext* c);
void vcallms_259(ExecutionContext* c) {
  // ior vi01, vi13, vi00       |  nop                            259
  vis[vi01] = vis[vi13];
  // lqi.xyzw vf09, vi01        |  nop                            260
  lq_buffer(Mask::xyzw, c->vfs[vf09].vf, vis[vi01]++);
  // ior vi03, vi11, vi00       |  nop                            261
  vis[vi03] = vis[vi11];
  // lq.xyzw vf05, 1(vi03)      |  nop                            262
  lq_buffer(Mask::xyzw, c->vfs[vf05].vf, vis[vi03] + 1);
  // iaddi vi03, vi03, 0x2      |  nop                            263
  vis[vi03] = vis[vi03] + 2;
  vcallms_264(c);
}

void vcallms_264(ExecutionContext* c) {
  // move.xyzw vf15, vf21       |  mulax.xyzw ACC, vf01, vf09     264
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf01).vf, c->vf_src(vf09).vf.x());   c->vfs[vf15].vf.move(Mask::xyzw, c->vf_src(vf21).vf);
  // move.xyzw vf20, vf22       |  madday.xyzw ACC, vf02, vf09    265
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf02].vf, c->vfs[vf09].vf.y());   c->vfs[vf20].vf.move(Mask::xyzw, c->vf_src(vf22).vf);
  // lq.w vf13, -2(vi03)        |  maddz.xyz vf17, vf03, vf09     266
  c->acc.vf.madd(Mask::xyz, c->vfs[vf17].vf, c->vf_src(vf03).vf, c->vf_src(vf09).vf.z());   lq_buffer(Mask::w, c->vfs[vf13].vf, vis[vi03] + -2);
  // lqi.xyzw vf10, vi01        |  mulaw.xyzw ACC, vf04, vf00     267
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf04).vf, c->vf_src(vf00).vf.w());   lq_buffer(Mask::xyzw, c->vfs[vf10].vf, vis[vi01]++);
  // move.xyzw vf16, vf23       |  maddax.xyzw ACC, vf01, vf05    268
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf01].vf, c->vfs[vf05].vf.x());   c->vfs[vf16].vf.move(Mask::xyzw, c->vf_src(vf23).vf);
  // lq.xyzw vf06, 1(vi03)      |  madday.xyzw ACC, vf02, vf05    269
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf02].vf, c->vfs[vf05].vf.y());   lq_buffer(Mask::xyzw, c->vfs[vf06].vf, vis[vi03] + 1);
  // iaddi vi03, vi03, 0x2      |  maddz.xyz vf13, vf03, vf05     270
  c->acc.vf.madd(Mask::xyz, c->vfs[vf13].vf, c->vf_src(vf03).vf, c->vf_src(vf05).vf.z());   vis[vi03] = vis[vi03] + 2;
  // nop                        |  mulax.xyzw ACC, vf01, vf10     271
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf01).vf, c->vf_src(vf10).vf.x());
  // nop                        |  madday.xyzw ACC, vf02, vf10    272
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf02].vf, c->vfs[vf10].vf.y());
  // lq.w vf14, -2(vi03)        |  maddz.xyz vf18, vf03, vf10     273
  c->acc.vf.madd(Mask::xyz, c->vfs[vf18].vf, c->vf_src(vf03).vf, c->vf_src(vf10).vf.z());   lq_buffer(Mask::w, c->vfs[vf14].vf, vis[vi03] + -2);
  // lqi.xyzw vf11, vi01        |  mulaw.xyzw ACC, vf04, vf00     274
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf04).vf, c->vf_src(vf00).vf.w());   lq_buffer(Mask::xyzw, c->vfs[vf11].vf, vis[vi01]++);
  // nop                        |  maddax.xyzw ACC, vf01, vf06    275
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf01].vf, c->vfs[vf06].vf.x());
  // lq.xyzw vf07, 1(vi03)      |  madday.xyzw ACC, vf02, vf06    276
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf02].vf, c->vfs[vf06].vf.y());   lq_buffer(Mask::xyzw, c->vfs[vf07].vf, vis[vi03] + 1);
  // iaddi vi03, vi03, 0x2      |  maddz.xyz vf14, vf03, vf06     277
  c->acc.vf.madd(Mask::xyz, c->vfs[vf14].vf, c->vf_src(vf03).vf, c->vf_src(vf06).vf.z());   vis[vi03] = vis[vi03] + 2;
  // nop                        |  mulax.xyzw ACC, vf01, vf11     278
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf01).vf, c->vf_src(vf11).vf.x());
  // nop                        |  madday.xyzw ACC, vf02, vf11    279
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf02].vf, c->vfs[vf11].vf.y());
  // lq.w vf21, -2(vi03)        |  maddz.xyz vf19, vf03, vf11     280
  c->acc.vf.madd(Mask::xyz, c->vfs[vf19].vf, c->vf_src(vf03).vf, c->vf_src(vf11).vf.z());   lq_buffer(Mask::w, c->vfs[vf21].vf, vis[vi03] + -2);
  // lqi.xyzw vf12, vi01        |  mulaw.xyzw ACC, vf04, vf00     281
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf04).vf, c->vf_src(vf00).vf.w());   lq_buffer(Mask::xyzw, c->vfs[vf12].vf, vis[vi01]++);
  // nop                        |  maddax.xyzw ACC, vf01, vf07    282
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf01].vf, c->vfs[vf07].vf.x());
  // lq.xyzw vf08, 1(vi03)      |  madday.xyzw ACC, vf02, vf07    283
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf02].vf, c->vfs[vf07].vf.y());   lq_buffer(Mask::xyzw, c->vfs[vf08].vf, vis[vi03] + 1);
  // iaddi vi03, vi03, 0x2      |  maddz.xyz vf21, vf03, vf07     284
  c->acc.vf.madd(Mask::xyz, c->vfs[vf21].vf, c->vf_src(vf03).vf, c->vf_src(vf07).vf.z());   vis[vi03] = vis[vi03] + 2;
  // nop                        |  mulax.xyzw ACC, vf01, vf12     285
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf01).vf, c->vf_src(vf12).vf.x());
  // nop                        |  madday.xyzw ACC, vf02, vf12    286
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf02].vf, c->vfs[vf12].vf.y());
  // lq.w vf23, -2(vi03)        |  maddz.xyz vf22, vf03, vf12     287
  c->acc.vf.madd(Mask::xyz, c->vfs[vf22].vf, c->vf_src(vf03).vf, c->vf_src(vf12).vf.z());   lq_buffer(Mask::w, c->vfs[vf23].vf, vis[vi03] + -2);
  // lqi.xyzw vf09, vi01        |  mulaw.xyzw ACC, vf04, vf00     288
  c->acc.vf.mula(Mask::xyzw, c->vf_src(vf04).vf, c->vf_src(vf00).vf.w());   lq_buffer(Mask::xyzw, c->vfs[vf09].vf, vis[vi01]++);
  // nop                        |  maddax.xyzw ACC, vf01, vf08    289
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf01].vf, c->vfs[vf08].vf.x());
  // lq.xyzw vf05, 1(vi03)      |  madday.xyzw ACC, vf02, vf08 :e 290
  c->acc.vf.madda(Mask::xyzw, c->vfs[vf02].vf, c->vfs[vf08].vf.y());   lq_buffer(Mask::xyzw, c->vfs[vf05].vf, vis[vi03] + 1);
  // iaddi vi03, vi03, 0x2      |  maddz.xyz vf23, vf03, vf08     291
  c->acc.vf.madd(Mask::xyz, c->vfs[vf23].vf, c->vf_src(vf03).vf, c->vf_src(vf08).vf.z());   vis[vi03] = vis[vi03] + 2;
}

void vcallms_292(ExecutionContext* c) {
  // move.xyzw vf15, vf21       |  nop                            292
  c->vfs[vf15].vf.move(Mask::xyzw, c->vf_src(vf21).vf);
  // move.xyzw vf20, vf22       |  nop :e                         293
  c->vfs[vf20].vf.move(Mask::xyzw, c->vf_src(vf22).vf);
  // move.xyzw vf16, vf23       |  nop                            294
  c->vfs[vf16].vf.move(Mask::xyzw, c->vf_src(vf23).vf);
}

u64 execute(void* ctxt) {
  auto* c = (ExecutionContext*)ctxt;
  bool bc = false;
  get_fake_spad_addr(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  [[maybe_unused]] u32 call_addr = 0;
  u32 madr, sadr, qwc;
  c->daddiu(sp, sp, -128);                          // daddiu sp, sp, -128
  c->sd(ra, 384, at);                               // sd ra, 384(at)
  c->sq(s0, 400, at);                               // sq s0, 400(at)
  c->sq(s1, 416, at);                               // sq s1, 416(at)
  c->sq(s2, 432, at);                               // sq s2, 432(at)
  c->sq(s3, 448, at);                               // sq s3, 448(at)
  c->sq(s4, 464, at);                               // sq s4, 464(at)
  c->sq(s5, 480, at);                               // sq s5, 480(at)
  c->sq(gp, 496, at);                               // sq gp, 496(at)

  block_1:
  c->lui(v1, 4096);                                 // lui v1, 4096
  get_fake_spad_addr(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->ori(v1, v1, 54272);                            // ori v1, v1, 54272
  // nop                                            // sll r0, r0, 0

  /*
  block_2:
  c->lw(a0, 0, v1);                                 // lw a0, 0(v1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->andi(a0, a0, 256);                             // andi a0, a0, 256
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a0) == 0;                          // beq a0, r0, L11
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_4;}                           // branch non-likely

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->daddiu(gp, gp, 1);                             // daddiu gp, gp, 1
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L10                                 // beq r0, r0, L10
  // nop                                            // sll r0, r0, 0
  goto block_2;                                     // branch always
   */


  // nop                                            // sll r0, r0, 0
  c->lw(v1, 724, at);                               // lw v1, 724(at)
  // nop                                            // sll r0, r0, 0
  c->lw(a2, 728, at);                               // lw a2, 728(at)
  c->dsubu(a0, v1, r0);                             // dsubu a0, v1, r0
  c->lw(a1, 732, at);                               // lw a1, 732(at)
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L15
  c->daddiu(a0, at, 640);                           // daddiu a0, at, 640
  if (bc) {goto block_13;}                          // branch non-likely

  c->addiu(t0, a1, 16);                             // addiu t0, a1, 16
  c->lhu(a3, 0, a1);                                // lhu a3, 0(a1)
  c->sll(t2, a3, 4);                                // sll t2, a3, 4
  c->lw(t1, 8, a1);                                 // lw t1, 8(a1)
  c->addiu(a2, at, 7632);                           // addiu a2, at, 7632
  c->addu(a1, t0, t2);                              // addu a1, t0, t2
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 0, t1);                                 // lq t2, 0(t1)
  // nop                                            // sll r0, r0, 0
  c->lq(t3, 16, t1);                                // lq t3, 16(t1)
  // nop                                            // sll r0, r0, 0
  c->lq(t4, 32, t1);                                // lq t4, 32(t1)
  // nop                                            // sll r0, r0, 0
  c->lq(t5, 48, t1);                                // lq t5, 48(t1)
  // nop                                            // sll r0, r0, 0
  c->lq(t1, 64, t1);                                // lq t1, 64(t1)
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 12064, at);                             // sq t2, 12064(at)
  // nop                                            // sll r0, r0, 0
  c->sq(t3, 12080, at);                             // sq t3, 12080(at)
  // nop                                            // sll r0, r0, 0
  c->sq(t4, 12096, at);                             // sq t4, 12096(at)
  // nop                                            // sll r0, r0, 0
  c->sq(t5, 12112, at);                             // sq t5, 12112(at)
  // nop                                            // sll r0, r0, 0
  c->sq(t1, 12128, at);                             // sq t1, 12128(at)
  c->mov64(t1, a2);                                 // or t1, a2, r0
  // nop                                            // sll r0, r0, 0
  c->daddiu(t2, a3, -4);                            // daddiu t2, a3, -4
  c->mov64(t1, t1);                                 // or t1, t1, r0
  bc = ((s64)c->sgpr64(t2)) < 0;                    // bltz t2, L13
  c->mov64(t0, t0);                                 // or t0, t0, r0
  if (bc) {goto block_7;}                           // branch non-likely


  block_6:
  // nop                                            // sll r0, r0, 0
  c->lq(t5, 0, t0);                                 // lq t5, 0(t0)
  // nop                                            // sll r0, r0, 0
  c->lq(t2, 16, t0);                                // lq t2, 16(t0)
  c->daddiu(a3, a3, -4);                            // daddiu a3, a3, -4
  c->lq(t3, 32, t0);                                // lq t3, 32(t0)
  c->daddiu(t1, t1, 64);                            // daddiu t1, t1, 64
  c->lq(t4, 48, t0);                                // lq t4, 48(t0)
  c->daddiu(t0, t0, 64);                            // daddiu t0, t0, 64
  c->sq(t5, -64, t1);                               // sq t5, -64(t1)
  c->daddiu(t5, a3, -4);                            // daddiu t5, a3, -4
  c->sq(t2, -48, t1);                               // sq t2, -48(t1)
  // nop                                            // sll r0, r0, 0
  c->sq(t3, -32, t1);                               // sq t3, -32(t1)
  bc = ((s64)c->sgpr64(t5)) >= 0;                   // bgez t5, L12
  c->sq(t4, -16, t1);                               // sq t4, -16(t1)
  if (bc) {goto block_6;}                           // branch non-likely


  block_7:
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L14
  c->lq(t2, 0, t0);                                 // lq t2, 0(t0)
  if (bc) {goto block_12;}                          // branch non-likely

  c->daddiu(t0, t0, 16);                            // daddiu t0, t0, 16
  c->daddiu(t1, t1, 16);                            // daddiu t1, t1, 16
  c->daddiu(a3, a3, -1);                            // daddiu a3, a3, -1
  c->sq(t2, -16, t1);                               // sq t2, -16(t1)
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L14
  c->lq(t2, 0, t0);                                 // lq t2, 0(t0)
  if (bc) {goto block_12;}                          // branch non-likely

  c->daddiu(t0, t0, 16);                            // daddiu t0, t0, 16
  c->daddiu(t1, t1, 16);                            // daddiu t1, t1, 16
  c->daddiu(a3, a3, -1);                            // daddiu a3, a3, -1
  c->sq(t2, -16, t1);                               // sq t2, -16(t1)
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L14
  c->lq(t2, 0, t0);                                 // lq t2, 0(t0)
  if (bc) {goto block_12;}                          // branch non-likely

  c->daddiu(t0, t0, 16);                            // daddiu t0, t0, 16
  c->daddiu(t1, t1, 16);                            // daddiu t1, t1, 16
  c->daddiu(a3, a3, -1);                            // daddiu a3, a3, -1
  c->sq(t2, -16, t1);                               // sq t2, -16(t1)
  bc = c->sgpr64(a3) == 0;                          // beq a3, r0, L14
  c->lq(t2, 0, t0);                                 // lq t2, 0(t0)
  if (bc) {goto block_12;}                          // branch non-likely

  c->daddiu(t0, t0, 16);                            // daddiu t0, t0, 16
  c->daddiu(t0, t1, 16);                            // daddiu t0, t1, 16
  c->daddiu(a3, a3, -1);                            // daddiu a3, a3, -1
  c->sq(t2, -16, t0);                               // sq t2, -16(t0)

  block_12:
  c->gprs[a3].du64[0] = 0;                          // or a3, r0, r0
  //beq r0, r0, L16                                 // beq r0, r0, L16
  c->sw(a2, 640, at);                               // sw a2, 640(at)
  goto block_15;                                    // branch always


  block_13:
  c->daddiu(a3, v1, -1);                            // daddiu a3, v1, -1
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(a3) != 0;                          // bne a3, r0, L17
  c->mov64(t2, a2);                                 // or t2, a2, r0
  if (bc) {goto block_16;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->daddiu(a1, a1, 528);                           // daddiu a1, a1, 528
  // nop                                            // sll r0, r0, 0

  block_15:
  c->addiu(a2, a1, 16);                             // addiu a2, a1, 16
  c->lhu(a3, 0, a1);                                // lhu a3, 0(a1)
  c->sll(t0, a3, 4);                                // sll t0, a3, 4
  c->lwu(t1, 28, a2);                               // lwu t1, 28(a2)
  c->sll(t2, t1, 2);                                // sll t2, t1, 2
  c->lwu(a3, 12, a1);                               // lwu a3, 12(a1)
  c->addu(t1, t2, t1);                              // addu t1, t2, t1
  c->lwu(a1, 8, a1);                                // lwu a1, 8(a1)
  c->addu(t0, a2, t0);                              // addu t0, a2, t0
  c->sw(a3, 48, a0);                                // sw a3, 48(a0)
  c->sll(a3, t1, 4);                                // sll a3, t1, 4
  c->sw(a1, 52, a0);                                // sw a1, 52(a0)
  c->daddiu(a1, t0, 16);                            // daddiu a1, t0, 16
  c->lhu(t0, 0, t0);                                // lhu t0, 0(t0)
  c->daddu(a3, a2, a3);                             // daddu a3, a2, a3
  c->sw(a2, 4, a0);                                 // sw a2, 4(a0)
  c->sll(a2, t0, 4);                                // sll a2, t0, 4
  c->sw(a3, 8, a0);                                 // sw a3, 8(a0)
  c->daddu(a2, a1, a2);                             // daddu a2, a1, a2
  c->sw(a1, 12, a0);                                // sw a1, 12(a0)
  c->daddiu(a1, a2, 16);                            // daddiu a1, a2, 16
  c->lhu(a2, 0, a2);                                // lhu a2, 0(a2)
  c->sll(t2, a2, 4);                                // sll t2, a2, 4
  c->lhu(a2, 14, a1);                               // lhu a2, 14(a1)
  c->daddiu(a3, a1, 32);                            // daddiu a3, a1, 32
  c->lhu(t0, 10, a1);                               // lhu t0, 10(a1)
  c->daddu(t0, t0, a1);                             // daddu t0, t0, a1
  c->lhu(t1, 12, a1);                               // lhu t1, 12(a1)
  c->daddu(t1, t1, a1);                             // daddu t1, t1, a1
  c->lhu(t3, 0, a1);                                // lhu t3, 0(a1)
  c->daddu(t2, a1, t2);                             // daddu t2, a1, t2
  c->sw(a1, 16, a0);                                // sw a1, 16(a0)
  c->daddiu(t2, t2, 16);                            // daddiu t2, t2, 16
  c->sw(a3, 20, a0);                                // sw a3, 20(a0)
  c->daddu(a1, a2, a1);                             // daddu a1, a2, a1
  c->sw(t1, 28, a0);                                // sw t1, 28(a0)
  // nop                                            // sll r0, r0, 0
  c->sw(a1, 32, a0);                                // sw a1, 32(a0)
  //beq r0, r0, L17                                 // beq r0, r0, L17
  c->sw(t0, 24, a0);                                // sw t0, 24(a0)
  goto block_16;                                    // branch always


  block_16:
  c->daddiu(a1, t2, 16);                            // daddiu a1, t2, 16
  c->lwu(a3, 12, t2);                               // lwu a3, 12(t2)
  c->daddiu(a2, a1, 112);                           // daddiu a2, a1, 112
  c->sw(a3, 44, a0);                                // sw a3, 44(a0)
  // nop                                            // sll r0, r0, 0
  c->lhu(a3, -16, a2);                              // lhu a3, -16(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(a1, 36, a0);                                // sw a1, 36(a0)
  // nop                                            // sll r0, r0, 0
  c->sw(a2, 40, a0);                                // sw a2, 40(a0)
  // nop                                            // sll r0, r0, 0
  c->sw(v1, 56, a0);                                // sw v1, 56(a0)
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 744, at);                               // lw v1, 744(at)
  // nop                                            // sll r0, r0, 0
  c->lw(a0, 76, at);                                // lw a0, 76(at)
  c->dsubu(v1, a0, v1);                             // dsubu v1, a0, v1
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(v1)) >= 0;                   // bgez v1, L51
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_70;}                          // branch non-likely

  get_fake_spad_addr(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 684, at);                               // lw v1, 684(at)
  c->lui(a1, 1);                                    // lui a1, 1
  c->lw(a0, 688, at);                               // lw a0, 688(at)
  c->dsubu(a1, v1, a1);                             // dsubu a1, v1, a1
  c->lw(a2, 692, at);                               // lw a2, 692(at)
  bc = ((s64)c->sgpr64(a1)) > 0;                    // bgtz a1, L19
  c->lw(a1, 728, at);                               // lw a1, 728(at)
  if (bc) {goto block_23;}                          // branch non-likely

  bc = c->sgpr64(a2) != 0;                          // bne a2, r0, L18
  c->lw(v1, 732, at);                               // lw v1, 732(at)
  if (bc) {goto block_21;}                          // branch non-likely

  c->xori(a1, v1, 3248);                            // xori a1, v1, 3248
  c->mov64(v1, a0);                                 // or v1, a0, r0
  c->daddiu(a3, a1, 2880);                          // daddiu a3, a1, 2880
  c->sw(a1, 732, at);                               // sw a1, 732(at)
  c->xori(a2, a3, 7264);                            // xori a2, a3, 7264
  c->sw(a3, 728, at);                               // sw a3, 728(at)
  c->addiu(a3, r0, 0);                              // addiu a3, r0, 0
  c->daddu(a1, a1, r0);                             // daddu a1, a1, r0
  c->andi(a2, a2, 65535);                           // andi a2, a2, 65535
  c->sw(a3, 724, at);                               // sw a3, 724(at)
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L20
  c->sw(a2, 736, at);                               // sw a2, 736(at)
  if (bc) {goto block_24;}                          // branch non-likely

  c->addiu(v1, r0, 1);                              // addiu v1, r0, 1
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L21                                 // beq r0, r0, L21
  c->sw(v1, 740, at);                               // sw v1, 740(at)
  goto block_25;                                    // branch always


  block_21:
  c->xori(a1, v1, 3248);                            // xori a1, v1, 3248
  c->mov64(v1, a0);                                 // or v1, a0, r0
  c->daddiu(a3, a1, 2880);                          // daddiu a3, a1, 2880
  c->sw(a1, 732, at);                               // sw a1, 732(at)
  c->xori(a2, a3, 7264);                            // xori a2, a3, 7264
  c->sw(a3, 728, at);                               // sw a3, 728(at)
  c->addiu(a3, r0, 1);                              // addiu a3, r0, 1
  c->daddiu(a1, a1, 528);                           // daddiu a1, a1, 528
  c->andi(a2, a2, 65535);                           // andi a2, a2, 65535
  c->sw(a3, 724, at);                               // sw a3, 724(at)
  bc = c->sgpr64(a0) != 0;                          // bne a0, r0, L20
  c->sw(a2, 736, at);                               // sw a2, 736(at)
  if (bc) {goto block_24;}                          // branch non-likely

  c->addiu(v1, r0, 1);                              // addiu v1, r0, 1
  // nop                                            // sll r0, r0, 0
  //beq r0, r0, L21                                 // beq r0, r0, L21
  c->sw(v1, 740, at);                               // sw v1, 740(at)
  goto block_25;                                    // branch always


  block_23:
  c->mov64(v1, v1);                                 // or v1, v1, r0
  c->lw(a2, 736, at);                               // lw a2, 736(at)
  c->addiu(a0, r0, 2);                              // addiu a0, r0, 2
  // nop                                            // sll r0, r0, 0
  c->xor_(a1, a1, a2);                              // xor a1, a1, a2
  c->sw(a0, 724, at);                               // sw a0, 724(at)
  //beq r0, r0, L20                                 // beq r0, r0, L20
  c->sw(a1, 728, at);                               // sw a1, 728(at)
  goto block_24;                                    // branch always


  block_24:
  get_fake_spad_addr(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->ori(a2, r0, 65535);                            // ori a2, r0, 65535
  c->lui(a0, 4096);                                 // lui a0, 4096
  // nop                                            // sll r0, r0, 0
  c->ori(a0, a0, 54272);                            // ori a0, a0, 54272
  c->and_(a1, a1, a2);                              // and a1, a1, a2
  c->addiu(a2, r0, 324);                            // addiu a2, r0, 324
  {
    // spr to
    sadr = c->sgpr64(a1);
    u32 tadr = c->sgpr64(v1);
    spad_to_dma_blerc_chain(cache.fake_scratchpad_data, sadr & 0x3fff, tadr);
  }
//  c->sw(a1, 128, a0);                               // sw a1, 128(a0)
//  c->sw(v1, 48, a0);                                // sw v1, 48(a0)
//  c->sw(r0, 32, a0);                                // sw r0, 32(a0)
//  c->sw(a2, 0, a0);                                 // sw a2, 0(a0)

  block_25:
  // nop                                            // sll r0, r0, 0
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  get_fake_spad_addr(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->daddiu(t2, at, 640);                           // daddiu t2, at, 640
  c->lw(a0, 716, at);                               // lw a0, 716(at)
  // nop                                            // sll r0, r0, 0
  c->lw(a1, 56, t2);                                // lw a1, 56(t2)
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 4, t2);                                 // lw v1, 4(t2)
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 8, t2);                                 // lw v1, 8(t2)
  // nop                                            // sll r0, r0, 0
  c->lw(t3, 12, t2);                                // lw t3, 12(t2)
  // nop                                            // sll r0, r0, 0
  c->lw(t1, 16, t2);                                // lw t1, 16(t2)
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 32, t2);                                // lw v1, 32(t2)
  // nop                                            // sll r0, r0, 0
  c->lw(t5, 20, t2);                                // lw t5, 20(t2)
  // nop                                            // sll r0, r0, 0
  c->lw(t6, 24, t2);                                // lw t6, 24(t2)
  // nop                                            // sll r0, r0, 0
  c->lw(t4, 28, t2);                                // lw t4, 28(t2)
  // nop                                            // sll r0, r0, 0
  c->lw(a3, 36, t2);                                // lw a3, 36(t2)
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 40, t2);                                // lw v1, 40(t2)
  // nop                                            // sll r0, r0, 0
  c->lw(t0, 0, t2);                                 // lw t0, 0(t2)
  // nop                                            // sll r0, r0, 0
  c->lhu(a2, 20, t1);                               // lhu a2, 20(t1)
  // nop                                            // sll r0, r0, 0
  c->lhu(t8, 18, t1);                               // lhu t8, 18(t1)
  c->daddiu(a2, a1, -2);                            // daddiu a2, a1, -2
  c->lbu(a1, 2, t1);                                // lbu a1, 2(t1)
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L38
  c->lbu(a2, 3, t1);                                // lbu a2, 3(t1)
  if (bc) {goto block_52;}                          // branch non-likely

  c->daddu(t7, t8, t8);                             // daddu t7, t8, t8
  c->addiu(t9, r0, -16);                            // addiu t9, r0, -16
  c->daddiu(ra, t7, 15);                            // daddiu ra, t7, 15
  c->daddiu(t7, a0, 32);                            // daddiu t7, a0, 32
  c->and_(t9, ra, t9);                              // and t9, ra, t9
  c->lq(ra, 16, t1);                                // lq ra, 16(t1)
  c->daddu(t9, t7, t9);                             // daddu t9, t7, t9
  c->sq(ra, 16, a0);                                // sq ra, 16(a0)
  c->dsll(ra, t8, 5);                               // dsll ra, t8, 5
  c->sw(t7, 0, a0);                                 // sw t7, 0(a0)
  c->daddu(ra, t9, ra);                             // daddu ra, t9, ra
  c->sw(t9, 4, a0);                                 // sw t9, 4(a0)
  c->daddiu(t8, t8, 7);                             // daddiu t8, t8, 7
  c->sw(ra, 8, a0);                                 // sw ra, 8(a0)
  c->dsra(t9, t8, 3);                               // dsra t9, t8, 3
  // nop                                            // sll r0, r0, 0
  c->addiu(t8, r0, 2);                              // addiu t8, r0, 2
  // nop                                            // sll r0, r0, 0
  c->mult3(s3, t8, t9);                             // mult3 s3, t8, t9
  c->mov64(ra, t6);                                 // or ra, t6, r0
  c->mov64(t6, t8);                                 // or t6, t8, r0
  c->mov64(t7, t7);                                 // or t7, t7, r0
  c->addiu(t8, r0, 513);                            // addiu t8, r0, 513
  c->addiu(t9, r0, 257);                            // addiu t9, r0, 257
  c->dsll(s1, t8, 18);                              // dsll s1, t8, 18
  c->dsll(s2, t9, 16);                              // dsll s2, t9, 16
  c->or_(t8, t8, s1);                               // or t8, t8, s1
  c->or_(t9, t9, s2);                               // or t9, t9, s2
  c->dsll32(s1, t8, 4);                             // dsll32 s1, t8, 4
  c->dsll32(s2, t9, 0);                             // dsll32 s2, t9, 0
  c->or_(t8, t8, s1);                               // or t8, t8, s1
  c->or_(t9, t9, s2);                               // or t9, t9, s2
  c->pcpyld(t8, t8, t8);                            // pcpyld t8, t8, t8
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyld(t9, t9, t9);                            // pcpyld t9, t9, t9
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->daddu(s3, s3, ra);                             // daddu s3, s3, ra
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->lhu(s2, 0, ra);                                // lhu s2, 0(ra)
  c->daddu(ra, ra, t6);                             // daddu ra, ra, t6
  // nop                                            // sll r0, r0, 0
  c->mov64(t5, t5);                                 // or t5, t5, r0
  // nop                                            // sll r0, r0, 0
  c->pextlb(s2, s2, s2);                            // pextlb s2, s2, s2
  //beq r0, r0, L23                                 // beq r0, r0, L23
  c->pextlb(s1, s2, s2);                            // pextlb s1, s2, s2
  goto block_28;                                    // branch always


  block_27:
  c->daddiu(t5, t5, 16);                            // daddiu t5, t5, 16
  // nop                                            // sll r0, r0, 0
  c->daddu(ra, ra, t6);                             // daddu ra, ra, t6
  c->daddiu(t7, t7, 32);                            // daddiu t7, t7, 32
  c->pextlb(s1, s1, s1);                            // pextlb s1, s1, s1
  c->sq(s2, -16, t7);                               // sq s2, -16(t7)

  block_28:
  c->pextlb(s2, s1, s1);                            // pextlb s2, s1, s1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pand(s2, s2, t8);                              // pand s2, s2, t8
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pceqb(s1, s2, t8);                             // pceqb s1, s2, t8
  c->lq(s2, 0, t5);                                 // lq s2, 0(t5)
  c->pand(v0, s1, t9);                              // pand v0, s1, t9
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(s1, v0, s2);                            // pextlb s1, v0, s2
  c->lhu(s0, 0, ra);                                // lhu s0, 0(ra)
  c->pextub(s2, v0, s2);                            // pextub s2, v0, s2
  c->sq(s1, 0, t7);                                 // sq s1, 0(t7)
  bc = c->sgpr64(ra) != c->sgpr64(s3);              // bne ra, s3, L22
  c->pextlb(s1, s0, s0);                            // pextlb s1, s0, s0
  if (bc) {goto block_27;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->sq(s2, 16, t7);                                // sq s2, 16(t7)
  c->gprs[t5].du64[0] = 0;                          // or t5, r0, r0
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L24
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_31;}                          // branch non-likely

  c->daddiu(t6, a2, 7);                             // daddiu t6, a2, 7
  c->daddiu(t5, at, 700);                           // daddiu t5, at, 700
  // nop                                            // sll r0, r0, 0
  c->lw(t2, 32, t2);                                // lw t2, 32(t2)
  c->sra(t6, t6, 3);                                // sra t6, t6, 3
  c->sh(r0, 0, t5);                                 // sh r0, 0(t5)
  // nop                                            // sll r0, r0, 0
  c->sh(a1, 4, t5);                                 // sh a1, 4(t5)
  // nop                                            // sll r0, r0, 0
  c->sh(a2, 2, t5);                                 // sh a2, 2(t5)
  // nop                                            // sll r0, r0, 0
  c->sw(t2, 8, t5);                                 // sw t2, 8(t5)
  // nop                                            // sll r0, r0, 0
  c->sw(t5, 80, at);                                // sw t5, 80(at)
  //beq r0, r0, L25                                 // beq r0, r0, L25
  // nop                                            // sll r0, r0, 0
  goto block_32;                                    // branch always


  block_31:
  // nop                                            // sll r0, r0, 0
  c->sw(r0, 80, at);                                // sw r0, 80(at)
  // nop                                            // sll r0, r0, 0

  block_32:
  c->daddu(t5, a1, a2);                             // daddu t5, a1, a2
  c->mov64(t2, t4);                                 // or t2, t4, r0
  c->daddiu(t4, t5, 3);                             // daddiu t4, t5, 3
  vis[vi09] = c->gpr_src(a2).du16[0];                     // ctc2.i vi9, a2
  c->sra(t5, t4, 2);                                // sra t5, t4, 2
  c->lq(t4, 0, t2);                                 // lq t4, 0(t2)
  c->sll(t5, t5, 4);                                // sll t5, t5, 4
  c->daddiu(t2, t2, 16);                            // daddiu t2, t2, 16
  c->pextub(t7, t4, r0);                            // pextub t7, t4, r0
  vis[vi08] = c->gpr_src(a1).du16[0];                     // ctc2.i vi8, a1
  c->pextlb(t6, t4, r0);                            // pextlb t6, t4, r0
  vis[vi09] = c->gpr_src(a2).du16[0];                     // ctc2.i vi9, a2
  c->daddu(t4, t2, t5);                             // daddu t4, t2, t5
  // Unknown instr: vcallms 104
  vcallms_104();

  block_33:
  c->pextuh(t5, t7, r0);                            // pextuh t5, t7, r0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(t7, t7, r0);                            // pextlh t7, t7, r0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextuh(t8, t6, r0);                            // pextuh t8, t6, r0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(t6, t6, r0);                            // pextlh t6, t6, r0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psraw(t9, t6, 19);                             // psraw t9, t6, 19
  c->lq(t6, 0, t2);                                 // lq t6, 0(t2)
  c->psraw(t8, t8, 19);                             // psraw t8, t8, 19
  c->mov128_vf_gpr(vf5, t9);                        // qmtc2.ni vf5, t9
  c->psraw(t7, t7, 19);                             // psraw t7, t7, 19
  c->mov128_vf_gpr(vf6, t8);                        // qmtc2.ni vf6, t8
  c->psraw(t5, t5, 19);                             // psraw t5, t5, 19
  c->mov128_vf_gpr(vf7, t7);                        // qmtc2.ni vf7, t7
  c->pextub(t7, t6, r0);                            // pextub t7, t6, r0
  c->mov128_vf_gpr(vf8, t5);                        // qmtc2.ni vf8, t5
  c->pextlb(t6, t6, r0);                            // pextlb t6, t6, r0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  // Unknown instr: vcallms 114
  vcallms_114(c);
  bc = c->sgpr64(t2) != c->sgpr64(t4);              // bne t2, t4, L26
  c->daddiu(t2, t2, 16);                            // daddiu t2, t2, 16
  if (bc) {goto block_33;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->mov64(t2, t3);                                 // or t2, t3, r0
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L34
  c->daddiu(t4, a1, -4);                            // daddiu t4, a1, -4
  if (bc) {goto block_48;}                          // branch non-likely

  bc = ((s64)c->sgpr64(t4)) <= 0;                   // blez t4, L29
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_39;}                          // branch non-likely

  c->mov64(t2, t3);                                 // or t2, t3, r0
  c->ld(t5, 0, t3);                                 // ld t5, 0(t3)
  c->daddiu(t4, a1, -8);                            // daddiu t4, a1, -8
  c->ld(t6, 16, t3);                                // ld t6, 16(t3)
  c->pextlh(t7, t5, r0);                            // pextlh t7, t5, r0
  c->ld(t5, 32, t3);                                // ld t5, 32(t3)
  c->pextlh(ra, t6, r0);                            // pextlh ra, t6, r0
  c->ld(t6, 48, t3);                                // ld t6, 48(t3)
  c->pextlh(t8, t5, r0);                            // pextlh t8, t5, r0
  c->lwu(t5, 8, t3);                                // lwu t5, 8(t3)
  c->pextlh(t9, t6, r0);                            // pextlh t9, t6, r0
  c->lwu(t6, 24, t3);                               // lwu t6, 24(t3)
  c->psraw(s3, t7, 10);                             // psraw s3, t7, 10
  c->lwu(t7, 40, t3);                               // lwu t7, 40(t3)
  c->psraw(ra, ra, 10);                             // psraw ra, ra, 10
  c->lwu(t3, 56, t3);                               // lwu t3, 56(t3)
  c->psraw(t8, t8, 10);                             // psraw t8, t8, 10
  c->mov128_vf_gpr(vf1, s3);                        // qmtc2.ni vf1, s3
  c->psraw(t9, t9, 10);                             // psraw t9, t9, 10
  c->mov128_vf_gpr(vf2, ra);                        // qmtc2.ni vf2, ra
  c->daddiu(t2, t2, 64);                            // daddiu t2, t2, 64
  c->mov128_vf_gpr(vf3, t8);                        // qmtc2.ni vf3, t8
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf4, t9);                        // qmtc2.ni vf4, t9
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf13, t5);                       // qmtc2.ni vf13, t5
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf14, t6);                       // qmtc2.ni vf14, t6
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf15, t7);                       // qmtc2.ni vf15, t7
  bc = ((s64)c->sgpr64(t4)) <= 0;                   // blez t4, L28
  c->mov128_vf_gpr(vf16, t3);                       // qmtc2.ni vf16, t3
  if (bc) {goto block_38;}                          // branch non-likely


  block_37:
  // Unknown instr: vcallms 122
  vcallms_122(c);
  c->ld(t3, 0, t2);                                 // ld t3, 0(t2)
  c->daddiu(t4, t4, -4);                            // daddiu t4, t4, -4
  c->ld(t5, 16, t2);                                // ld t5, 16(t2)
  c->pextlh(t8, t3, r0);                            // pextlh t8, t3, r0
  c->ld(t3, 32, t2);                                // ld t3, 32(t2)
  c->pextlh(t9, t5, r0);                            // pextlh t9, t5, r0
  c->ld(t5, 48, t2);                                // ld t5, 48(t2)
  c->pextlh(t6, t3, r0);                            // pextlh t6, t3, r0
  c->lwu(t3, 8, t2);                                // lwu t3, 8(t2)
  c->pextlh(t7, t5, r0);                            // pextlh t7, t5, r0
  c->lwu(t5, 24, t2);                               // lwu t5, 24(t2)
  c->psraw(ra, t8, 10);                             // psraw ra, t8, 10
  c->lwu(t8, 40, t2);                               // lwu t8, 40(t2)
  c->psraw(t9, t9, 10);                             // psraw t9, t9, 10
  c->lwu(s3, 56, t2);                               // lwu s3, 56(t2)
  c->psraw(t6, t6, 10);                             // psraw t6, t6, 10
  c->mov128_vf_gpr(vf1, ra);                        // qmtc2.ni vf1, ra
  c->psraw(t7, t7, 10);                             // psraw t7, t7, 10
  c->mov128_vf_gpr(vf2, t9);                        // qmtc2.ni vf2, t9
  c->daddiu(t2, t2, 64);                            // daddiu t2, t2, 64
  c->mov128_vf_gpr(vf3, t6);                        // qmtc2.ni vf3, t6
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf4, t7);                        // qmtc2.ni vf4, t7
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf13, t3);                       // qmtc2.ni vf13, t3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf14, t5);                       // qmtc2.ni vf14, t5
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf15, t8);                       // qmtc2.ni vf15, t8
  bc = ((s64)c->sgpr64(t4)) > 0;                    // bgtz t4, L27
  c->mov128_vf_gpr(vf16, s3);                       // qmtc2.ni vf16, s3
  if (bc) {goto block_37;}                          // branch non-likely


  block_38:
  // Unknown instr: vcallms 122
  vcallms_122(c);
  // nop                                            // sll r0, r0, 0

  block_39:
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L33
  c->andi(t3, a1, 3);                               // andi t3, a1, 3
  if (bc) {goto block_47;}                          // branch non-likely

  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L33
  c->daddiu(t3, t3, -1);                            // daddiu t3, t3, -1
  if (bc) {goto block_47;}                          // branch non-likely

  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L30
  c->daddiu(t3, t3, -1);                            // daddiu t3, t3, -1
  if (bc) {goto block_44;}                          // branch non-likely

  bc = c->sgpr64(t3) == 0;                          // beq t3, r0, L31
  c->daddiu(t3, t3, -1);                            // daddiu t3, t3, -1
  if (bc) {goto block_45;}                          // branch non-likely

  //beq r0, r0, L32                                 // beq r0, r0, L32
  // nop                                            // sll r0, r0, 0
  goto block_46;                                    // branch always


  block_44:
  // nop                                            // sll r0, r0, 0
  c->ld(t3, 0, t2);                                 // ld t3, 0(t2)
  // nop                                            // sll r0, r0, 0
  c->ld(t4, 16, t2);                                // ld t4, 16(t2)
  c->pextlh(t3, t3, r0);                            // pextlh t3, t3, r0
  c->ld(t6, 24, t2);                                // ld t6, 24(t2)
  c->pextlh(t4, t4, r0);                            // pextlh t4, t4, r0
  c->lwu(t5, 8, t2);                                // lwu t5, 8(t2)
  c->pextlh(t6, t6, r0);                            // pextlh t6, t6, r0
  c->lwu(t7, 32, t2);                               // lwu t7, 32(t2)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->psraw(t3, t3, 10);                             // psraw t3, t3, 10
  c->mov128_vf_gpr(vf13, t5);                       // qmtc2.ni vf13, t5
  c->psraw(t4, t4, 10);                             // psraw t4, t4, 10
  c->mov128_vf_gpr(vf14, t7);                       // qmtc2.ni vf14, t7
  c->psraw(t5, t6, 10);                             // psraw t5, t6, 10
  c->mov128_vf_gpr(vf1, t3);                        // qmtc2.ni vf1, t3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf2, t4);                        // qmtc2.ni vf2, t4
  c->daddiu(t2, t2, 40);                            // daddiu t2, t2, 40
  c->mov128_vf_gpr(vf3, t5);                        // qmtc2.ni vf3, t5
  //beq r0, r0, L34                                 // beq r0, r0, L34
  // Unknown instr: vcallms 134
  vcallms_134(c);
  goto block_48;                                    // branch always


  block_45:
  // nop                                            // sll r0, r0, 0
  c->ld(t4, 0, t2);                                 // ld t4, 0(t2)
  // nop                                            // sll r0, r0, 0
  c->ld(t3, 16, t2);                                // ld t3, 16(t2)
  c->pextlh(t8, t4, r0);                            // pextlh t8, t4, r0
  c->ld(t5, 32, t2);                                // ld t5, 32(t2)
  c->pextlh(t4, t3, r0);                            // pextlh t4, t3, r0
  c->ld(t3, 40, t2);                                // ld t3, 40(t2)
  c->pextlh(t5, t5, r0);                            // pextlh t5, t5, r0
  c->lwu(t6, 8, t2);                                // lwu t6, 8(t2)
  c->pextlh(t7, t3, r0);                            // pextlh t7, t3, r0
  c->lwu(t3, 24, t2);                               // lwu t3, 24(t2)
  c->psraw(t9, t8, 10);                             // psraw t9, t8, 10
  c->lwu(t8, 48, t2);                               // lwu t8, 48(t2)
  c->psraw(t4, t4, 10);                             // psraw t4, t4, 10
  c->mov128_vf_gpr(vf13, t6);                       // qmtc2.ni vf13, t6
  c->psraw(t5, t5, 10);                             // psraw t5, t5, 10
  c->mov128_vf_gpr(vf1, t9);                        // qmtc2.ni vf1, t9
  c->psraw(t6, t7, 10);                             // psraw t6, t7, 10
  c->mov128_vf_gpr(vf2, t4);                        // qmtc2.ni vf2, t4
  c->daddiu(t2, t2, 56);                            // daddiu t2, t2, 56
  c->mov128_vf_gpr(vf3, t5);                        // qmtc2.ni vf3, t5
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf4, t6);                        // qmtc2.ni vf4, t6
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf14, t3);                       // qmtc2.ni vf14, t3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf15, t8);                       // qmtc2.ni vf15, t8
  //beq r0, r0, L34                                 // beq r0, r0, L34
  // Unknown instr: vcallms 145
  vcallms_145(c);
  goto block_48;                                    // branch always


  block_46:
  // nop                                            // sll r0, r0, 0
  c->ld(t4, 0, t2);                                 // ld t4, 0(t2)
  // nop                                            // sll r0, r0, 0
  c->ld(t3, 16, t2);                                // ld t3, 16(t2)
  c->pextlh(t7, t4, r0);                            // pextlh t7, t4, r0
  c->ld(t4, 32, t2);                                // ld t4, 32(t2)
  c->pextlh(t5, t3, r0);                            // pextlh t5, t3, r0
  c->lwu(t3, 8, t2);                                // lwu t3, 8(t2)
  c->pextlh(t6, t4, r0);                            // pextlh t6, t4, r0
  c->lwu(t4, 24, t2);                               // lwu t4, 24(t2)
  c->psraw(t7, t7, 10);                             // psraw t7, t7, 10
  c->lwu(t8, 40, t2);                               // lwu t8, 40(t2)
  c->psraw(t5, t5, 10);                             // psraw t5, t5, 10
  c->mov128_vf_gpr(vf13, t3);                       // qmtc2.ni vf13, t3
  c->psraw(t3, t6, 10);                             // psraw t3, t6, 10
  c->mov128_vf_gpr(vf1, t7);                        // qmtc2.ni vf1, t7
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf2, t5);                        // qmtc2.ni vf2, t5
  c->daddiu(t2, t2, 48);                            // daddiu t2, t2, 48
  c->mov128_vf_gpr(vf3, t3);                        // qmtc2.ni vf3, t3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf14, t4);                       // qmtc2.ni vf14, t4
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf15, t8);                       // qmtc2.ni vf15, t8
  //beq r0, r0, L34                                 // beq r0, r0, L34
  // Unknown instr: vcallms 157
  vcallms_157(c);
  goto block_48;                                    // branch always


  block_47:
  // nop                                            // sll r0, r0, 0
  c->ld(t3, 0, t2);                                 // ld t3, 0(t2)
  // nop                                            // sll r0, r0, 0
  c->ld(t4, 16, t2);                                // ld t4, 16(t2)
  c->pextlh(t7, t3, r0);                            // pextlh t7, t3, r0
  c->ld(t3, 32, t2);                                // ld t3, 32(t2)
  c->pextlh(t8, t4, r0);                            // pextlh t8, t4, r0
  c->ld(t4, 48, t2);                                // ld t4, 48(t2)
  c->pextlh(t5, t3, r0);                            // pextlh t5, t3, r0
  c->lwu(t3, 8, t2);                                // lwu t3, 8(t2)
  c->pextlh(t6, t4, r0);                            // pextlh t6, t4, r0
  c->lwu(t4, 24, t2);                               // lwu t4, 24(t2)
  c->psraw(t9, t7, 10);                             // psraw t9, t7, 10
  c->lwu(t7, 40, t2);                               // lwu t7, 40(t2)
  c->psraw(t8, t8, 10);                             // psraw t8, t8, 10
  c->lwu(ra, 56, t2);                               // lwu ra, 56(t2)
  c->psraw(t5, t5, 10);                             // psraw t5, t5, 10
  c->mov128_vf_gpr(vf1, t9);                        // qmtc2.ni vf1, t9
  c->psraw(t6, t6, 10);                             // psraw t6, t6, 10
  c->mov128_vf_gpr(vf2, t8);                        // qmtc2.ni vf2, t8
  c->daddiu(t2, t2, 64);                            // daddiu t2, t2, 64
  c->mov128_vf_gpr(vf3, t5);                        // qmtc2.ni vf3, t5
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf4, t6);                        // qmtc2.ni vf4, t6
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf13, t3);                       // qmtc2.ni vf13, t3
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf14, t4);                       // qmtc2.ni vf14, t4
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf15, t7);                       // qmtc2.ni vf15, t7
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf16, ra);                       // qmtc2.ni vf16, ra
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L37
  // Unknown instr: vcallms 122
  vcallms_122(c);
  if (bc) {goto block_51;}                          // branch non-likely


  block_48:
  // nop                                            // sll r0, r0, 0
  c->ld(t4, 0, t2);                                 // ld t4, 0(t2)
  c->daddiu(t3, a2, -2);                            // daddiu t3, a2, -2
  c->ld(t5, 8, t2);                                 // ld t5, 8(t2)
  c->pextlh(t4, t4, r0);                            // pextlh t4, t4, r0
  c->ld(t6, 24, t2);                                // ld t6, 24(t2)
  c->pextlh(t5, t5, r0);                            // pextlh t5, t5, r0
  c->ld(t7, 32, t2);                                // ld t7, 32(t2)
  c->pextlh(t6, t6, r0);                            // pextlh t6, t6, r0
  c->lwu(t8, 16, t2);                               // lwu t8, 16(t2)
  c->pextlh(t7, t7, r0);                            // pextlh t7, t7, r0
  c->lwu(t9, 40, t2);                               // lwu t9, 40(t2)
  c->psraw(t4, t4, 10);                             // psraw t4, t4, 10
  c->mov128_vf_gpr(vf13, t8);                       // qmtc2.ni vf13, t8
  c->psraw(t5, t5, 10);                             // psraw t5, t5, 10
  c->mov128_vf_gpr(vf14, t9);                       // qmtc2.ni vf14, t9
  c->psraw(t6, t6, 10);                             // psraw t6, t6, 10
  c->mov128_vf_gpr(vf1, t4);                        // qmtc2.ni vf1, t4
  c->psraw(t4, t7, 10);                             // psraw t4, t7, 10
  c->mov128_vf_gpr(vf2, t5);                        // qmtc2.ni vf2, t5
  c->daddiu(t2, t2, 48);                            // daddiu t2, t2, 48
  c->mov128_vf_gpr(vf3, t6);                        // qmtc2.ni vf3, t6
  bc = ((s64)c->sgpr64(t3)) <= 0;                   // blez t3, L36
  c->mov128_vf_gpr(vf4, t4);                        // qmtc2.ni vf4, t4
  if (bc) {goto block_50;}                          // branch non-likely


  block_49:
  // Unknown instr: vcallms 168
  vcallms_168(c);
  c->ld(t4, 0, t2);                                 // ld t4, 0(t2)
  c->daddiu(t3, t3, -2);                            // daddiu t3, t3, -2
  c->ld(t5, 8, t2);                                 // ld t5, 8(t2)
  c->pextlh(t4, t4, r0);                            // pextlh t4, t4, r0
  c->ld(t6, 24, t2);                                // ld t6, 24(t2)
  c->pextlh(t5, t5, r0);                            // pextlh t5, t5, r0
  c->ld(t7, 32, t2);                                // ld t7, 32(t2)
  c->pextlh(t6, t6, r0);                            // pextlh t6, t6, r0
  c->lwu(t8, 16, t2);                               // lwu t8, 16(t2)
  c->pextlh(t7, t7, r0);                            // pextlh t7, t7, r0
  c->lwu(t9, 40, t2);                               // lwu t9, 40(t2)
  c->psraw(t4, t4, 10);                             // psraw t4, t4, 10
  c->mov128_vf_gpr(vf13, t8);                       // qmtc2.ni vf13, t8
  c->psraw(t5, t5, 10);                             // psraw t5, t5, 10
  c->mov128_vf_gpr(vf14, t9);                       // qmtc2.ni vf14, t9
  c->psraw(t6, t6, 10);                             // psraw t6, t6, 10
  c->mov128_vf_gpr(vf1, t4);                        // qmtc2.ni vf1, t4
  c->psraw(t4, t7, 10);                             // psraw t4, t7, 10
  c->mov128_vf_gpr(vf2, t5);                        // qmtc2.ni vf2, t5
  c->daddiu(t2, t2, 48);                            // daddiu t2, t2, 48
  c->mov128_vf_gpr(vf3, t6);                        // qmtc2.ni vf3, t6
  bc = ((s64)c->sgpr64(t3)) > 0;                    // bgtz t3, L35
  c->mov128_vf_gpr(vf4, t4);                        // qmtc2.ni vf4, t4
  if (bc) {goto block_49;}                          // branch non-likely


  block_50:
  // Unknown instr: vcallms 168
  vcallms_168(c);
  // nop                                            // sll r0, r0, 0

  block_51:
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0

  block_52:
  c->mov64(t3, a1);                                 // or t3, a1, r0
  c->lw(t6, 80, a3);                                // lw t6, 80(a3)
  c->mov64(t4, v1);                                 // or t4, v1, r0
  c->lw(t1, 4, t1);                                 // lw t1, 4(t1)
  c->daddiu(t5, at, 12048);                         // daddiu t5, at, 12048
  c->sw(t6, 92, at);                                // sw t6, 92(at)
  c->pextlb(t2, r0, t1);                            // pextlb t2, r0, t1
  c->lq(t1, 0, a3);                                 // lq t1, 0(a3)
  c->pextlh(t7, t6, t6);                            // pextlh t7, t6, t6
  c->lq(t6, 16, a3);                                // lq t6, 16(a3)
  c->pextlh(t8, t7, t7);                            // pextlh t8, t7, t7
  c->lq(t7, 32, a3);                                // lq t7, 32(a3)
  c->pmulth(r0, t2, t8);                            // pmulth r0, t2, t8
  c->lq(t2, 48, a3);                                // lq t2, 48(a3)
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf1, t1);                        // qmtc2.ni vf1, t1
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf2, t6);                        // qmtc2.ni vf2, t6
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf3, t7);                        // qmtc2.ni vf3, t7
  // nop                                            // sll r0, r0, 0
  c->mov128_vf_gpr(vf4, t2);                        // qmtc2.ni vf4, t2
  // Unknown instr: vcallms 178
  vcallms_178(c);
  c->mov64(t1, t0);                                 // or t1, t0, r0
  c->pextlw(t2, t1, t1);                            // pextlw t2, t1, t1
  c->lw(t1, 4, a0);                                 // lw t1, 4(a0)
  c->pcpyld(t2, t2, t2);                            // pcpyld t2, t2, t2
  c->lw(t8, 96, at);                                // lw t8, 96(at)
  c->pmfhl_lh(t7);                                  // pmfhl.lh t7
  c->lw(t6, 84, a3);                                // lw t6, 84(a3)
  c->pextlb(t8, r0, t8);                            // pextlb t8, r0, t8
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psrlh(t7, t7, 7);                              // psrlh t7, t7, 7
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmulth(r0, t7, t8);                            // pmulth r0, t7, t8
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  bc = c->sgpr64(t6) != 0;                          // bne t6, r0, L39
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  if (bc) {goto block_54;}                          // branch non-likely

  // nop                                            // sll r0, r0, 0
  c->addiu(t6, r0, 8);                              // addiu t6, r0, 8
  //beq r0, r0, L40                                 // beq r0, r0, L40
  c->sh(t6, 11984, at);                             // sh t6, 11984(at)
  goto block_55;                                    // branch always


  block_54:
  // nop                                            // sll r0, r0, 0
  c->addiu(t6, r0, 6);                              // addiu t6, r0, 6
  // nop                                            // sll r0, r0, 0
  c->sh(t6, 11984, at);                             // sh t6, 11984(at)

  block_55:
  c->pmfhl_lh(t6);                                  // pmfhl.lh t6
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psrlh(t6, t6, 7);                              // psrlh t6, t6, 7
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppacb(t6, r0, t6);                             // ppacb t6, r0, t6
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->lw(t7, 0, t4);                                 // lw t7, 0(t4)
  c->daddiu(t4, t4, 4);                             // daddiu t4, t4, 4
  c->sw(t6, 0, t5);                                 // sw t6, 0(t5)
  c->pextlb(t7, r0, t7);                            // pextlb t7, r0, t7
  c->sw(t6, 4, t5);                                 // sw t6, 4(t5)
  c->pextlh(t7, r0, t7);                            // pextlh t7, r0, t7
  c->sw(t6, 8, t5);                                 // sw t6, 8(t5)
  c->psllw(t7, t7, 2);                              // psllw t7, t7, 2
  c->sw(t6, 12, t5);                                // sw t6, 12(t5)
  c->paddw(t9, t7, t2);                             // paddw t9, t7, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsrl32(s3, t9, 0);                             // dsrl32 s3, t9, 0
  c->mov128_gpr_vf(t7, vf17);                       // qmfc2.ni t7, vf17
  c->pcpyud(t5, t9, r0);                            // pcpyud t5, t9, r0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsrl32(t6, t5, 0);                             // dsrl32 t6, t5, 0
  c->mov128_gpr_vf(ra, vf13);                       // qmfc2.ni ra, vf13
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
  c->mov128_gpr_vf(t8, vf14);                       // qmfc2.ni t8, vf14
  // nop                                            // sll r0, r0, 0
  c->lwu(t9, 0, t9);                                // lwu t9, 0(t9)
  // nop                                            // sll r0, r0, 0
  c->lwu(s3, 0, s3);                                // lwu s3, 0(s3)
  // nop                                            // sll r0, r0, 0
  c->lwu(t5, 0, t5);                                // lwu t5, 0(t5)
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(s0, vf18);                       // qmfc2.ni s0, vf18
  // nop                                            // sll r0, r0, 0
  c->lwu(t6, 0, t6);                                // lwu t6, 0(t6)
  // nop                                            // sll r0, r0, 0
  c->sq(ra, 0, t1);                                 // sq ra, 0(t1)
  c->daddiu(t3, t3, -4);                            // daddiu t3, t3, -4
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  bc = ((s64)c->sgpr64(t3)) <= 0;                   // blez t3, L42
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_57;}                          // branch non-likely


  block_56:
  // Unknown instr: vcallms 183
  vcallms_183(c);
  c->sq(t7, 16, t1);                                // sq t7, 16(t1)
  // nop                                            // sll r0, r0, 0
  c->sw(t9, 28, t1);                                // sw t9, 28(t1)
  // nop                                            // sll r0, r0, 0
  c->sq(t8, 32, t1);                                // sq t8, 32(t1)
  // nop                                            // sll r0, r0, 0
  c->sq(s0, 48, t1);                                // sq s0, 48(t1)
  // nop                                            // sll r0, r0, 0
  c->sw(s3, 60, t1);                                // sw s3, 60(t1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  // nop                                            // sll r0, r0, 0
  c->lw(t8, 0, t4);                                 // lw t8, 0(t4)
  c->daddiu(t4, t4, 4);                             // daddiu t4, t4, 4
  c->mov128_gpr_vf(t7, vf19);                       // qmfc2.ni t7, vf19
  c->pextlb(t9, r0, t8);                            // pextlb t9, r0, t8
  c->mov128_gpr_vf(t8, vf20);                       // qmfc2.ni t8, vf20
  c->pextlh(t9, r0, t9);                            // pextlh t9, r0, t9
  c->mov128_gpr_vf(s3, vf15);                       // qmfc2.ni s3, vf15
  c->psllw(ra, t9, 2);                              // psllw ra, t9, 2
  c->mov128_gpr_vf(t9, vf16);                       // qmfc2.ni t9, vf16
  c->paddw(s0, ra, t2);                             // paddw s0, ra, t2
  c->sq(t7, 80, t1);                                // sq t7, 80(t1)
  c->dsrl32(v0, s0, 0);                             // dsrl32 v0, s0, 0
  c->mov128_gpr_vf(t7, vf17);                       // qmfc2.ni t7, vf17
  c->pcpyud(ra, s0, r0);                            // pcpyud ra, s0, r0
  c->sq(s3, 64, t1);                                // sq s3, 64(t1)
  c->dsrl32(s2, ra, 0);                             // dsrl32 s2, ra, 0
  c->mov128_gpr_vf(s1, vf13);                       // qmfc2.ni s1, vf13
  // nop                                            // sll r0, r0, 0
  c->sq(t8, 112, t1);                               // sq t8, 112(t1)
  // nop                                            // sll r0, r0, 0
  c->sw(t5, 92, t1);                                // sw t5, 92(t1)
  // nop                                            // sll r0, r0, 0
  c->sw(t6, 124, t1);                               // sw t6, 124(t1)
  // nop                                            // sll r0, r0, 0
  c->sq(t9, 96, t1);                                // sq t9, 96(t1)
  // nop                                            // sll r0, r0, 0
  c->lwu(t9, 0, s0);                                // lwu t9, 0(s0)
  // nop                                            // sll r0, r0, 0
  c->lwu(s3, 0, v0);                                // lwu s3, 0(v0)
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t8, vf14);                       // qmfc2.ni t8, vf14
  c->daddiu(t1, t1, 128);                           // daddiu t1, t1, 128
  c->mov128_gpr_vf(s0, vf18);                       // qmfc2.ni s0, vf18
  c->daddiu(t3, t3, -4);                            // daddiu t3, t3, -4
  c->lwu(t5, 0, ra);                                // lwu t5, 0(ra)
  // nop                                            // sll r0, r0, 0
  c->sq(s1, 0, t1);                                 // sq s1, 0(t1)
  bc = ((s64)c->sgpr64(t3)) > 0;                    // bgtz t3, L41
  c->lwu(t6, 0, s2);                                // lwu t6, 0(s2)
  if (bc) {goto block_56;}                          // branch non-likely


  block_57:
  // Unknown instr: vcallms 211
  vcallms_211(c);
  c->sq(t7, 16, t1);                                // sq t7, 16(t1)
  // nop                                            // sll r0, r0, 0
  c->sw(t9, 28, t1);                                // sw t9, 28(t1)
  // nop                                            // sll r0, r0, 0
  c->sq(t8, 32, t1);                                // sq t8, 32(t1)
  // nop                                            // sll r0, r0, 0
  c->sq(s0, 48, t1);                                // sq s0, 48(t1)
  // nop                                            // sll r0, r0, 0
  c->sw(s3, 60, t1);                                // sw s3, 60(t1)
  // nop                                            // sll r0, r0, 0
  // nop                                            // vnop
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf19, 80, t1);                            // sqc2 vf19, 80(t1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf15, 64, t1);                            // sqc2 vf15, 64(t1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf16, 96, t1);                            // sqc2 vf16, 96(t1)
  // nop                                            // sll r0, r0, 0
  c->sqc2(vf20, 112, t1);                           // sqc2 vf20, 112(t1)
  // nop                                            // sll r0, r0, 0
  c->sw(t5, 92, t1);                                // sw t5, 92(t1)
  // nop                                            // sll r0, r0, 0
  c->sw(t6, 124, t1);                               // sw t6, 124(t1)
  bc = c->sgpr64(a2) == 0;                          // beq a2, r0, L48
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_65;}                          // branch non-likely

  c->mov64(t1, t0);                                 // or t1, t0, r0
  c->lqc2(vf29, 64, a3);                            // lqc2 vf29, 64(a3)
  c->mov64(t0, v1);                                 // or t0, v1, r0
  c->lwc1(f0, 72, a3);                              // lwc1 f0, 72(a3)
  c->daddiu(t0, a1, 3);                             // daddiu t0, a1, 3
  c->lwc1(f2, 76, a3);                              // lwc1 f2, 76(a3)
  c->addiu(a3, r0, -4);                             // addiu a3, r0, -4
  // nop                                            // sll r0, r0, 0
  c->cvtws(f0, f0);                                 // cvt.w.s f0, f0
  c->and_(a3, t0, a3);                              // and a3, t0, a3
  c->cvtws(f2, f2);                                 // cvt.w.s f2, f2
  c->lw(t2, 4, a0);                                 // lw t2, 4(a0)
  c->mfc1(a0, f0);                                  // mfc1 a0, f0
  c->mov64(t0, a2);                                 // or t0, a2, r0
  c->mfc1(t3, f2);                                  // mfc1 t3, f2
  c->dsll(a2, a1, 5);                               // dsll a2, a1, 5
  c->dsra(a1, t3, 1);                               // dsra a1, t3, 1
  c->daddu(a3, v1, a3);                             // daddu a3, v1, a3
  bc = c->sgpr64(a1) == 0;                          // beq a1, r0, L45
  c->daddu(a2, t2, a2);                             // daddu a2, t2, a2
  if (bc) {goto block_62;}                          // branch non-likely

  c->pcpyh(v1, a0);                                 // pcpyh v1, a0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pcpyld(v1, v1, v1);                            // pcpyld v1, v1, v1
  c->sh(v1, 712, at);                               // sh v1, 712(at)
  c->pcpyh(a0, a1);                                 // pcpyh a0, a1
  c->sh(a0, 714, at);                               // sh a0, 714(at)
  c->pcpyld(a0, a0, a0);                            // pcpyld a0, a0, a0
  c->ldr(s5, 0, a3);                                // ldr s5, 0(a3)
  c->pextlw(a1, t1, t1);                            // pextlw a1, t1, t1
  c->ldl(s5, 7, a3);                                // ldl s5, 7(a3)
  c->pcpyld(a1, a1, a1);                            // pcpyld a1, a1, a1
  c->ldr(s4, 8, a3);                                // ldr s4, 8(a3)
  c->pextlb(t2, r0, s5);                            // pextlb t2, r0, s5
  c->ldl(s4, 15, a3);                               // ldl s4, 15(a3)
  c->pextuh(t1, r0, t2);                            // pextuh t1, r0, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(t3, r0, t2);                            // pextlh t3, r0, t2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(t4, r0, s4);                            // pextlb t4, r0, s4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextuh(t2, r0, t4);                            // pextuh t2, r0, t4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(t5, r0, t4);                            // pextlh t5, r0, t4
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psllw(t3, t3, 2);                              // psllw t3, t3, 2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psllw(t4, t1, 2);                              // psllw t4, t1, 2
  // Unknown instr: vcallms 215
  vcallms_215(c);

  c->psllw(t6, t5, 2);                              // psllw t6, t5, 2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psllw(t5, t2, 2);                              // psllw t5, t2, 2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->paddw(t1, t3, a1);                             // paddw t1, t3, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->paddw(t4, t4, a1);                             // paddw t4, t4, a1
  c->lwu(t3, 0, t1);                                // lwu t3, 0(t1)
  c->paddw(t2, t6, a1);                             // paddw t2, t6, a1
  c->lwu(t9, 0, t4);                                // lwu t9, 0(t4)
  c->paddw(t5, t5, a1);                             // paddw t5, t5, a1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsrl32(s4, t1, 0);                             // dsrl32 s4, t1, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsrl32(s3, t4, 0);                             // dsrl32 s3, t4, 0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->dsrl32(ra, t2, 0);                             // dsrl32 ra, t2, 0
  c->daddiu(a2, a2, -128);                          // daddiu a2, a2, -128
  c->dsrl32(t8, t5, 0);                             // dsrl32 t8, t5, 0
  c->daddiu(a3, a3, 16);                            // daddiu a3, a3, 16
  c->pcpyud(t7, t1, r0);                            // pcpyud t7, t1, r0
  c->lwu(s5, 0, t2);                                // lwu s5, 0(t2)
  c->pcpyud(t6, t4, r0);                            // pcpyud t6, t4, r0
  c->lwu(s2, 0, t5);                                // lwu s2, 0(t5)
  c->pcpyud(t4, t2, r0);                            // pcpyud t4, t2, r0
  c->lwu(s4, 0, s4);                                // lwu s4, 0(s4)
  c->pcpyud(t5, t5, r0);                            // pcpyud t5, t5, r0
  c->lwu(s3, 0, s3);                                // lwu s3, 0(s3)
  c->pextlw(t9, t9, t3);                            // pextlw t9, t9, t3
  c->lwu(ra, 0, ra);                                // lwu ra, 0(ra)
  c->pextlw(t3, s2, s5);                            // pextlw t3, s2, s5
  c->lwu(s2, 0, t8);                                // lwu s2, 0(t8)
  c->pextlw(s5, s3, s4);                            // pextlw s5, s3, s4
  c->lwu(t8, 0, t7);                                // lwu t8, 0(t7)
  c->pextlw(t7, s2, ra);                            // pextlw t7, s2, ra
  c->lwu(ra, 0, t6);                                // lwu ra, 0(t6)
  c->pextlb(t6, r0, t9);                            // pextlb t6, r0, t9
  c->lwu(t9, 0, t4);                                // lwu t9, 0(t4)
  c->pextlb(t4, r0, s5);                            // pextlb t4, r0, s5
  c->lwu(s5, 0, t5);                                // lwu s5, 0(t5)
  c->pextlw(ra, ra, t8);                            // pextlw ra, ra, t8
  c->mov128_gpr_vf(t5, vf17);                       // qmfc2.ni t5, vf17
  c->pextlw(t8, s5, t9);                            // pextlw t8, s5, t9
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(t9, r0, ra);                            // pextlb t9, r0, ra
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->paddh(t9, t4, t9);                             // paddh t9, t4, t9
  c->mov128_gpr_vf(t4, vf13);                       // qmfc2.ni t4, vf13
  c->pmulth(r0, t9, a0);                            // pmulth r0, t9, a0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(t3, r0, t3);                            // pextlb t3, r0, t3
  c->sq(t5, 144, a2);                               // sq t5, 144(a2)
  c->pmaddh(r0, t6, v1);                            // pmaddh r0, t6, v1
  c->sq(t4, 128, a2);                               // sq t4, 128(a2)
  c->pextlb(t5, r0, t7);                            // pextlb t5, r0, t7
  c->ldr(t1, 0, a3);                                // ldr t1, 0(a3)
  c->pextlb(t7, r0, t8);                            // pextlb t7, r0, t8
  c->ldl(t1, 7, a3);                                // ldl t1, 7(a3)
  c->pmfhl_lh(t6);                                  // pmfhl.lh t6
  c->mov128_gpr_vf(t4, vf18);                       // qmfc2.ni t4, vf18
  c->paddh(t5, t5, t7);                             // paddh t5, t5, t7
  c->ldr(t2, 8, a3);                                // ldr t2, 8(a3)
  c->psrlh(t6, t6, 8);                              // psrlh t6, t6, 8
  c->ldl(t2, 15, a3);                               // ldl t2, 15(a3)
  c->ppacb(t6, r0, t6);                             // ppacb t6, r0, t6
  c->sq(t4, 176, a2);                               // sq t4, 176(a2)
  c->dsrl32(t4, t6, 0);                             // dsrl32 t4, t6, 0
  c->daddiu(t0, t0, -4);                            // daddiu t0, t0, -4
  c->pmulth(r0, t5, a0);                            // pmulth r0, t5, a0
  c->mov128_gpr_vf(t5, vf14);                       // qmfc2.ni t5, vf14
  c->pextlb(t1, r0, t1);                            // pextlb t1, r0, t1
  c->sw(t6, 156, a2);                               // sw t6, 156(a2)
  c->pmaddh(r0, t3, v1);                            // pmaddh r0, t3, v1
  c->sw(t4, 188, a2);                               // sw t4, 188(a2)
  c->pextlb(t3, r0, t2);                            // pextlb t3, r0, t2
  c->sq(t5, 160, a2);                               // sq t5, 160(a2)
  c->pextuh(t2, r0, t1);                            // pextuh t2, r0, t1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmfhl_lh(t6);                                  // pmfhl.lh t6
  c->mov128_gpr_vf(t5, vf19);                       // qmfc2.ni t5, vf19
  c->pextlh(t4, r0, t1);                            // pextlh t4, r0, t1
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psrlh(t1, t6, 8);                              // psrlh t1, t6, 8
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->ppacb(t6, r0, t1);                             // ppacb t6, r0, t1
  c->sq(t5, 208, a2);                               // sq t5, 208(a2)
  c->dsrl32(t1, t6, 0);                             // dsrl32 t1, t6, 0
  c->sw(t6, 220, a2);                               // sw t6, 220(a2)
  c->pextuh(t5, r0, t3);                            // pextuh t5, r0, t3
  c->mov128_gpr_vf(t6, vf15);                       // qmfc2.ni t6, vf15
  c->pextlh(t7, r0, t3);                            // pextlh t7, r0, t3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psllw(t3, t4, 2);                              // psllw t3, t4, 2
  c->sq(t6, 192, a2);                               // sq t6, 192(a2)
  c->psllw(t4, t2, 2);                              // psllw t4, t2, 2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psllw(t6, t7, 2);                              // psllw t6, t7, 2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psllw(t5, t5, 2);                              // psllw t5, t5, 2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->paddw(s5, t3, a1);                             // paddw s5, t3, a1
  c->mov128_gpr_vf(t2, vf20);                       // qmfc2.ni t2, vf20
  c->paddw(t4, t4, a1);                             // paddw t4, t4, a1
  c->lwu(t3, 0, s5);                                // lwu t3, 0(s5)
  c->paddw(s4, t6, a1);                             // paddw s4, t6, a1
  c->lwu(t9, 0, t4);                                // lwu t9, 0(t4)
  bc = ((s64)c->sgpr64(t0)) <= 0;                   // blez t0, L44
  c->paddw(t5, t5, a1);                             // paddw t5, t5, a1
  if (bc) {goto block_61;}                          // branch non-likely


  block_60:
  c->dsrl32(s3, s5, 0);                             // dsrl32 s3, s5, 0
  // Unknown instr: vcallms 221
  vcallms_221(c);

  c->dsrl32(s0, t4, 0);                             // dsrl32 s0, t4, 0
  c->dsrl32(s2, s4, 0);                             // dsrl32 s2, s4, 0
  c->dsrl32(t8, t5, 0);                             // dsrl32 t8, t5, 0
  c->daddiu(a2, a2, 128);                           // daddiu a2, a2, 128
  c->pcpyud(t7, s5, r0);                            // pcpyud t7, s5, r0
  c->lwu(ra, 0, s4);                                // lwu ra, 0(s4)
  c->pcpyud(t6, t4, r0);                            // pcpyud t6, t4, r0
  c->lwu(s1, 0, t5);                                // lwu s1, 0(t5)
  c->pcpyud(t4, s4, r0);                            // pcpyud t4, s4, r0
  c->lwu(s3, 0, s3);                                // lwu s3, 0(s3)
  c->pcpyud(t5, t5, r0);                            // pcpyud t5, t5, r0
  c->lwu(s0, 0, s0);                                // lwu s0, 0(s0)
  c->pextlw(t9, t9, t3);                            // pextlw t9, t9, t3
  c->lwu(s2, 0, s2);                                // lwu s2, 0(s2)
  c->pextlw(t3, s1, ra);                            // pextlw t3, s1, ra
  c->lwu(s1, 0, t8);                                // lwu s1, 0(t8)
  c->pextlw(ra, s0, s3);                            // pextlw ra, s0, s3
  c->lwu(t8, 0, t7);                                // lwu t8, 0(t7)
  c->pextlw(t7, s1, s2);                            // pextlw t7, s1, s2
  c->lwu(s3, 0, t6);                                // lwu s3, 0(t6)
  c->pextlb(t6, r0, t9);                            // pextlb t6, r0, t9
  c->lwu(t9, 0, t4);                                // lwu t9, 0(t4)
  c->pextlb(t4, r0, ra);                            // pextlb t4, r0, ra
  c->lwu(t5, 0, t5);                                // lwu t5, 0(t5)
  c->pextlw(t8, s3, t8);                            // pextlw t8, s3, t8
  c->sq(t2, 112, a2);                               // sq t2, 112(a2)
  c->pextlw(t2, t5, t9);                            // pextlw t2, t5, t9
  c->mov128_gpr_vf(t5, vf17);                       // qmfc2.ni t5, vf17
  c->pextlb(t8, r0, t8);                            // pextlb t8, r0, t8
  c->sw(t1, 124, a2);                               // sw t1, 124(a2)
  c->paddh(t1, t4, t8);                             // paddh t1, t4, t8
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pmulth(r0, t1, a0);                            // pmulth r0, t1, a0
  c->mov128_gpr_vf(t4, vf13);                       // qmfc2.ni t4, vf13
  c->pextlb(t1, r0, t3);                            // pextlb t1, r0, t3
  c->sq(t5, 144, a2);                               // sq t5, 144(a2)
  c->pmaddh(r0, t6, v1);                            // pmaddh r0, t6, v1
  c->ldr(s5, 16, a3);                               // ldr s5, 16(a3)
  c->pextlb(t3, r0, t7);                            // pextlb t3, r0, t7
  c->sq(t4, 128, a2);                               // sq t4, 128(a2)
  c->pextlb(t2, r0, t2);                            // pextlb t2, r0, t2
  c->ldl(s5, 23, a3);                               // ldl s5, 23(a3)
  c->pmfhl_lh(t4);                                  // pmfhl.lh t4
  c->ldr(s4, 24, a3);                               // ldr s4, 24(a3)
  c->paddh(t2, t3, t2);                             // paddh t2, t3, t2
  c->mov128_gpr_vf(t5, vf18);                       // qmfc2.ni t5, vf18
  c->psrlh(t3, t4, 8);                              // psrlh t3, t4, 8
  c->ldl(s4, 31, a3);                               // ldl s4, 31(a3)
  c->ppacb(t3, r0, t3);                             // ppacb t3, r0, t3
  c->sq(t5, 176, a2);                               // sq t5, 176(a2)
  c->dsrl32(t5, t3, 0);                             // dsrl32 t5, t3, 0
  c->daddiu(t0, t0, -4);                            // daddiu t0, t0, -4
  c->pmulth(r0, t2, a0);                            // pmulth r0, t2, a0
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlb(t4, r0, s5);                            // pextlb t4, r0, s5
  c->mov128_gpr_vf(t6, vf14);                       // qmfc2.ni t6, vf14
  c->pmaddh(r0, t1, v1);                            // pmaddh r0, t1, v1
  c->sw(t3, 156, a2);                               // sw t3, 156(a2)
  c->pextlb(t3, r0, s4);                            // pextlb t3, r0, s4
  c->sw(t5, 188, a2);                               // sw t5, 188(a2)
  c->pextuh(t2, r0, t4);                            // pextuh t2, r0, t4
  c->sq(t6, 160, a2);                               // sq t6, 160(a2)
  c->pmfhl_lh(t6);                                  // pmfhl.lh t6
  c->mov128_gpr_vf(t1, vf16);                       // qmfc2.ni t1, vf16
  c->pextlh(t4, r0, t4);                            // pextlh t4, r0, t4
  c->mov128_gpr_vf(t5, vf19);                       // qmfc2.ni t5, vf19
  c->psrlh(t6, t6, 8);                              // psrlh t6, t6, 8
  c->sq(t1, 96, a2);                                // sq t1, 96(a2)
  c->ppacb(t6, r0, t6);                             // ppacb t6, r0, t6
  c->sq(t5, 208, a2);                               // sq t5, 208(a2)
  c->dsrl32(t1, t6, 0);                             // dsrl32 t1, t6, 0
  c->daddiu(a3, a3, 16);                            // daddiu a3, a3, 16
  c->pextuh(t5, r0, t3);                            // pextuh t5, r0, t3
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->pextlh(t7, r0, t3);                            // pextlh t7, r0, t3
  c->mov128_gpr_vf(t8, vf15);                       // qmfc2.ni t8, vf15
  c->psllw(t3, t4, 2);                              // psllw t3, t4, 2
  c->sw(t6, 220, a2);                               // sw t6, 220(a2)
  c->psllw(t4, t2, 2);                              // psllw t4, t2, 2
  c->sq(t8, 192, a2);                               // sq t8, 192(a2)
  c->psllw(t6, t7, 2);                              // psllw t6, t7, 2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->psllw(t5, t5, 2);                              // psllw t5, t5, 2
  c->mfc1(r0, f31);                                 // mfc1 r0, f31
  c->paddw(s5, t3, a1);                             // paddw s5, t3, a1
  c->mov128_gpr_vf(t2, vf20);                       // qmfc2.ni t2, vf20
  c->paddw(t4, t4, a1);                             // paddw t4, t4, a1
  c->lwu(t3, 0, s5);                                // lwu t3, 0(s5)
  c->paddw(s4, t6, a1);                             // paddw s4, t6, a1
  c->lwu(t9, 0, t4);                                // lwu t9, 0(t4)
  bc = ((s64)c->sgpr64(t0)) > 0;                    // bgtz t0, L43
  c->paddw(t5, t5, a1);                             // paddw t5, t5, a1
  if (bc) {goto block_60;}                          // branch non-likely


  block_61:
  // nop                                            // sll r0, r0, 0
  // Unknown instr: vcallms 257
  vcallms_257(c);

  // nop                                            // sll r0, r0, 0
  c->sq(t2, 240, a2);                               // sq t2, 240(a2)
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(v1, vf16);                       // qmfc2.i v1, vf16
  // nop                                            // sll r0, r0, 0
  c->sw(t1, 252, a2);                               // sw t1, 252(a2)
  //beq r0, r0, L48                                 // beq r0, r0, L48
  c->sq(v1, 224, a2);                               // sq v1, 224(a2)
  goto block_65;                                    // branch always


  block_62:
  // Unknown instr: vcallms 259
  vcallms_259(c);

  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->sh(a0, 712, at);                               // sh a0, 712(at)
  // nop                                            // sll r0, r0, 0
  c->sh(a1, 714, at);                               // sh a1, 714(at)
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
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lbu(v1, 0, a3);                                // lbu v1, 0(a3)
  // nop                                            // sll r0, r0, 0
  c->lbu(a0, 4, a3);                                // lbu a0, 4(a3)
  // nop                                            // sll r0, r0, 0
  c->lbu(a1, 8, a3);                                // lbu a1, 8(a3)
  c->dsll(v1, v1, 2);                               // dsll v1, v1, 2
  c->lbu(t2, 12, a3);                               // lbu t2, 12(a3)
  c->dsll(a0, a0, 2);                               // dsll a0, a0, 2
  // nop                                            // sll r0, r0, 0
  c->dsll(t3, a1, 2);                               // dsll t3, a1, 2
  // nop                                            // sll r0, r0, 0
  c->dsll(t4, t2, 2);                               // dsll t4, t2, 2
  c->mov128_gpr_vf(t2, vf17);                       // qmfc2.i t2, vf17
  c->daddu(s5, v1, t1);                             // daddu s5, v1, t1
  c->daddiu(v1, a3, 16);                            // daddiu v1, a3, 16
  c->daddu(a1, a0, t1);                             // daddu a1, a0, t1
  // nop                                            // sll r0, r0, 0
  c->daddu(s4, t3, t1);                             // daddu s4, t3, t1
  // nop                                            // sll r0, r0, 0
  c->daddu(a0, t4, t1);                             // daddu a0, t4, t1
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t7, vf13);                       // qmfc2.ni t7, vf13
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a3, vf18);                       // qmfc2.ni a3, vf18
  // nop                                            // sll r0, r0, 0
  c->lwu(t6, 0, s5);                                // lwu t6, 0(s5)
  // nop                                            // sll r0, r0, 0
  c->lwu(t3, 0, a1);                                // lwu t3, 0(a1)
  // nop                                            // sll r0, r0, 0
  c->lwu(t4, 0, s4);                                // lwu t4, 0(s4)
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t5, vf14);                       // qmfc2.ni t5, vf14
  // nop                                            // sll r0, r0, 0
  c->lwu(a0, 0, a0);                                // lwu a0, 0(a0)
  c->daddiu(a1, t0, -4);                            // daddiu a1, t0, -4
  c->sq(t7, 0, a2);                                 // sq t7, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t0, vf19);                       // qmfc2.ni t0, vf19
  // nop                                            // sll r0, r0, 0
  c->sq(t2, 16, a2);                                // sq t2, 16(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(t6, 28, a2);                                // sw t6, 28(a2)
  bc = ((s64)c->sgpr64(a1)) <= 0;                   // blez a1, L47
  c->sq(a3, 48, a2);                                // sq a3, 48(a2)
  if (bc) {goto block_64;}                          // branch non-likely


  block_63:
  // Unknown instr: vcallms 264
  vcallms_264(c);

  c->sw(t3, 60, a2);                                // sw t3, 60(a2)
  // nop                                            // sll r0, r0, 0
  c->sq(t5, 32, a2);                                // sq t5, 32(a2)
  // nop                                            // sll r0, r0, 0
  c->sq(t0, 80, a2);                                // sq t0, 80(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(t4, 92, a2);                                // sw t4, 92(a2)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  c->lbu(a3, 0, v1);                                // lbu a3, 0(v1)
  // nop                                            // sll r0, r0, 0
  c->lbu(t0, 4, v1);                                // lbu t0, 4(v1)
  // nop                                            // sll r0, r0, 0
  c->lbu(t2, 8, v1);                                // lbu t2, 8(v1)
  c->dsll(t3, a3, 2);                               // dsll t3, a3, 2
  c->lbu(a3, 12, v1);                               // lbu a3, 12(v1)
  c->dsll(t6, t0, 2);                               // dsll t6, t0, 2
  c->mov128_gpr_vf(t0, vf15);                       // qmfc2.i t0, vf15
  c->dsll(t4, t2, 2);                               // dsll t4, t2, 2
  c->mov128_gpr_vf(t2, vf20);                       // qmfc2.ni t2, vf20
  c->dsll(t5, a3, 2);                               // dsll t5, a3, 2
  c->mov128_gpr_vf(a3, vf17);                       // qmfc2.ni a3, vf17
  c->daddu(s5, t3, t1);                             // daddu s5, t3, t1
  c->daddiu(v1, v1, 16);                            // daddiu v1, v1, 16
  c->daddu(t3, t6, t1);                             // daddu t3, t6, t1
  c->mov128_gpr_vf(t6, vf16);                       // qmfc2.ni t6, vf16
  c->daddu(s4, t4, t1);                             // daddu s4, t4, t1
  c->sq(t0, 64, a2);                                // sq t0, 64(a2)
  c->daddu(t0, t5, t1);                             // daddu t0, t5, t1
  c->sq(t2, 112, a2);                               // sq t2, 112(a2)
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t2, vf13);                       // qmfc2.ni t2, vf13
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 124, a2);                               // sw a0, 124(a2)
  // nop                                            // sll r0, r0, 0
  c->sq(t6, 96, a2);                                // sq t6, 96(a2)
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t6, vf18);                       // qmfc2.ni t6, vf18
  // nop                                            // sll r0, r0, 0
  c->lwu(t7, 0, s5);                                // lwu t7, 0(s5)
  // nop                                            // sll r0, r0, 0
  c->lwu(t3, 0, t3);                                // lwu t3, 0(t3)
  // nop                                            // sll r0, r0, 0
  c->lwu(t4, 0, s4);                                // lwu t4, 0(s4)
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t5, vf14);                       // qmfc2.ni t5, vf14
  c->daddiu(a2, a2, 128);                           // daddiu a2, a2, 128
  c->lwu(a0, 0, t0);                                // lwu a0, 0(t0)
  c->daddiu(a1, a1, -4);                            // daddiu a1, a1, -4
  c->sq(t2, 0, a2);                                 // sq t2, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(t0, vf19);                       // qmfc2.ni t0, vf19
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 16, a2);                                // sq a3, 16(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(t7, 28, a2);                                // sw t7, 28(a2)
  bc = ((s64)c->sgpr64(a1)) > 0;                    // bgtz a1, L46
  c->sq(t6, 48, a2);                                // sq t6, 48(a2)
  if (bc) {goto block_63;}                          // branch non-likely


  block_64:
  // nop                                            // sll r0, r0, 0
  // Unknown instr: vcallms 292
  vcallms_292(c);

  // nop                                            // sll r0, r0, 0
  c->sw(t3, 60, a2);                                // sw t3, 60(a2)
  // nop                                            // sll r0, r0, 0
  c->sq(t5, 32, a2);                                // sq t5, 32(a2)
  // nop                                            // sll r0, r0, 0
  c->sq(t0, 80, a2);                                // sq t0, 80(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(t4, 92, a2);                                // sw t4, 92(a2)
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(v1, vf15);                       // qmfc2.i v1, vf15
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a1, vf20);                       // qmfc2.ni a1, vf20
  // nop                                            // sll r0, r0, 0
  c->mov128_gpr_vf(a3, vf16);                       // qmfc2.ni a3, vf16
  // nop                                            // sll r0, r0, 0
  c->sq(v1, 64, a2);                                // sq v1, 64(a2)
  // nop                                            // sll r0, r0, 0
  c->sq(a1, 112, a2);                               // sq a1, 112(a2)
  // nop                                            // sll r0, r0, 0
  c->sw(a0, 124, a2);                               // sw a0, 124(a2)
  // nop                                            // sll r0, r0, 0
  c->sq(a3, 96, a2);                                // sq a3, 96(a2)

  block_65:
  c->gprs[v1].du64[0] = 0;                          // or v1, r0, r0
  c->lw(v1, 644, at);                               // lw v1, 644(at)
  c->lw(a0, 716, at);                               // lw a0, 716(at)
  c->daddiu(a1, at, 12064);                         // daddiu a1, at, 12064
  c->sw(v1, 64, at);                                // sw v1, 64(at)
  c->sw(a0, 60, at);                                // sw a0, 60(at)
  c->sw(a1, 68, at);                                // sw a1, 68(at)
  c->sw(r0, 72, at);                                // sw r0, 72(at)
  c->lw(v1, 748, at);                               // lw v1, 748(at)
  call_addr = c->gprs[v1].du32[0];                  // function call:
  // Unknown instr: sllv v0, ra, r0
  // c->jalr(call_addr);                               // jalr ra, v1
  generic_prepare_dma_double::execute(c);
  c->lw(v1, 752, at);                               // lw v1, 752(at)
  call_addr = c->gprs[v1].du32[0];                  // function call:
  // Unknown instr: sllv v0, ra, r0
  // c->jalr(call_addr);                               // jalr ra, v1
  generic_envmap_dproc::execute(c);
  c->lw(v1, 756, at);                               // lw v1, 756(at)
  call_addr = c->gprs[v1].du32[0];                  // function call:
  // Unknown instr: sllv v0, ra, r0
  // c->jalr(call_addr);                               // jalr ra, v1
  generic_interp_dproc::execute(c);
  c->lw(v1, 760, at);                               // lw v1, 760(at)
  call_addr = c->gprs[v1].du32[0];                  // function call:
  // Unknown instr: sllv v0, ra, r0
  // c->jalr(call_addr);                               // jalr ra, v1
  generic_no_light_dproc::execute(c);
  c->lw(v1, 40, at);                                // lw v1, 40(at)
  c->lw(a0, 56, at);                                // lw a0, 56(at)
  c->mov64(a3, v1);                                 // or a3, v1, r0
  // nop                                            // sll r0, r0, 0
  get_fake_spad_addr(at, cache.fake_scratchpad_data, 0, c);// lui at, 28672
  c->lui(a2, 4096);                                 // lui a2, 4096
  c->lwu(a1, 76, at);                               // lwu a1, 76(at)
  c->ori(a2, a2, 53248);                            // ori a2, a2, 53248
  // c->lw(t1, 0, a2);                                 // lw t1, 0(a2)
  // nop                                            // sll r0, r0, 0
  c->daddiu(t0, at, 108);                           // daddiu t0, at, 108
  c->andi(a3, a3, 16383);                           // andi a3, a3, 16383
  c->andi(t1, t1, 256);                             // andi t1, t1, 256
  // nop                                            // sll r0, r0, 0
  bc = true; // c->sgpr64(t1) == 0;                          // beq t1, r0, L50
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_69;}                          // branch non-likely

  c->mov64(t1, a2);                                 // or t1, a2, r0
  // nop                                            // sll r0, r0, 0

  block_67:
  c->lw(t2, 0, t0);                                 // lw t2, 0(t0)
  // nop                                            // sll r0, r0, 0
  c->lw(t3, 0, t1);                                 // lw t3, 0(t1)
  // nop                                            // sll r0, r0, 0
  c->andi(t3, t3, 256);                             // andi t3, t3, 256
  c->daddiu(t2, t2, 1);                             // daddiu t2, t2, 1
  bc = c->sgpr64(t3) != 0;                          // bne t3, r0, L49
  c->sw(t2, 0, t0);                                 // sw t2, 0(t0)
  if (bc) {goto block_67;}                          // branch non-likely

  c->gprs[t0].du64[0] = 0;                          // or t0, r0, r0

  block_69:
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
  c->sw(a0, 76, at);                                // sw a0, 76(at)
  c->gprs[a0].du64[0] = 0;                          // or a0, r0, r0
  c->xori(v1, v1, 4608);                            // xori v1, v1, 4608
  c->sw(v1, 40, at);                                // sw v1, 40(at)
  // nop                                            // sll r0, r0, 0
  c->lw(v1, 740, at);                               // lw v1, 740(at)
  // nop                                            // sll r0, r0, 0
  // nop                                            // sll r0, r0, 0
  bc = c->sgpr64(v1) == 0;                          // beq v1, r0, L9
  // nop                                            // sll r0, r0, 0
  if (bc) {goto block_1;}                           // branch non-likely


  block_70:
  c->gprs[v0].du64[0] = 0;                          // or v0, r0, r0
  c->ld(ra, 384, at);                               // ld ra, 384(at)
  c->lq(gp, 496, at);                               // lq gp, 496(at)
  c->lq(s5, 480, at);                               // lq s5, 480(at)
  c->lq(s4, 464, at);                               // lq s4, 464(at)
  c->lq(s3, 448, at);                               // lq s3, 448(at)
  c->lq(s2, 432, at);                               // lq s2, 432(at)
  c->lq(s1, 416, at);                               // lq s1, 416(at)
  c->lq(s0, 400, at);                               // lq s0, 400(at)
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
  gLinkedFunctionTable.reg("generic-tie-convert", execute, 256);
}

} // namespace generic_tie_convert
} // namespace Mips2C

