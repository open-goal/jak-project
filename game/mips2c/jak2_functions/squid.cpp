//--------------------------MIPS2C---------------------
// clang-format off
#include "game/mips2c/mips2c_private.h"
#include "game/kernel/jak2/kscheme.h"
using ::jak2::intern_from_c;
namespace Mips2C::jak2 {
    namespace method_53_squid {
        struct Cache {
            void* default_dead_pool; // *default-dead-pool*
            void* game_info; // *game-info*
            void* rand_vu_float_range; // rand-vu-float-range
            void* spawn_projectile; // spawn-projectile
            void* squid_increment_shield; // squid-increment-shield
            void* squid_shot; // squid-shot
            void* tan; // tan
            void* vector_flatten; // vector-flatten!
            void* vector_normalize; // vector-normalize!
            void* vector_cspace; // vector<-cspace!
        } cache;

        u64 execute(void* ctxt) {
            auto* c = (ExecutionContext*)ctxt;
            bool bc = false;
            u32 call_addr = 0;
            bool cop1_bc = false;
            c->daddiu(sp, sp, -336);                          // daddiu sp, sp, -336
            c->sd(ra, 0, sp);                                 // sd ra, 0(sp)
            c->sq(s1, 224, sp);                               // sq s1, 224(sp)
            c->sq(s2, 240, sp);                               // sq s2, 240(sp)
            c->sq(s3, 256, sp);                               // sq s3, 256(sp)
            c->sq(s4, 272, sp);                               // sq s4, 272(sp)
            c->sq(s5, 288, sp);                               // sq s5, 288(sp)
            c->sq(gp, 304, sp);                               // sq gp, 304(sp)
            c->swc1(f24, 320, sp);                            // swc1 f24, 320(sp)
            c->swc1(f26, 324, sp);                            // swc1 f26, 324(sp)
            c->swc1(f28, 328, sp);                            // swc1 f28, 328(sp)
            c->swc1(f30, 332, sp);                            // swc1 f30, 332(sp)
            c->mov64(gp, a0);                                 // or gp, a0, r0
            c->mov64(s4, a1);                                 // or s4, a1, r0
            c->load_symbol2(t9, cache.squid_increment_shield);// lw t9, squid-increment-shield(s7)
            c->lui(v1, -17393);                               // lui v1, -17393
            c->ori(a0, v1, 47140);                            // ori a0, v1, 47140
            call_addr = c->gprs[t9].du32[0];                  // function call:
            c->sll(v0, ra, 0);                                // sll v0, ra, 0
            c->jalr(call_addr);                               // jalr ra, t9
            c->load_symbol2(t9, cache.vector_cspace);        // lw t9, vector<-cspace!(s7)
            c->daddiu(a0, sp, 16);                            // daddiu a0, sp, 16
            c->dsll(v1, s4, 5);                               // dsll v1, s4, 5
            c->daddiu(v1, v1, 12);                            // daddiu v1, v1, 12
            c->lwu(a1, 128, gp);                              // lwu a1, 128(gp)
            c->daddu(a1, v1, a1);                             // daddu a1, v1, a1
            call_addr = c->gprs[t9].du32[0];                  // function call:
            c->sll(v0, ra, 0);                                // sll v0, ra, 0
            c->jalr(call_addr);                               // jalr ra, t9
            c->mov64(s2, v0);                                 // or s2, v0, r0
            c->daddiu(s5, sp, 32);                            // daddiu s5, sp, 32
            c->lwu(v1, 128, gp);                              // lwu v1, 128(gp)
            c->dsll(a0, s4, 5);                               // dsll a0, s4, 5
            c->daddu(v1, v1, a0);                             // daddu v1, v1, a0
            c->lwu(v1, 28, v1);                               // lwu v1, 28(v1)
            c->daddu(a2, r0, v1);                             // daddu a2, r0, v1
            c->lq(v1, 0, a2);                                 // lq v1, 0(a2)
            c->lq(a0, 16, a2);                                // lq a0, 16(a2)
            c->lq(a1, 32, a2);                                // lq a1, 32(a2)
            c->lq(a2, 48, a2);                                // lq a2, 48(a2)
            c->sq(v1, 0, s5);                                 // sq v1, 0(s5)
            c->sq(a0, 16, s5);                                // sq a0, 16(s5)
            c->sq(a1, 32, s5);                                // sq a1, 32(s5)
            c->sq(a2, 48, s5);                                // sq a2, 48(s5)
            c->mov64(a0, gp);                                 // or a0, gp, r0
            c->lwu(v1, -4, a0);                               // lwu v1, -4(a0)
            c->lwu(t9, 184, v1);                              // lwu t9, 184(v1)
            c->daddiu(a1, sp, 96);                            // daddiu a1, sp, 96
            call_addr = c->gprs[t9].du32[0];                  // function call:
            c->sll(v0, ra, 0);                                // sll v0, ra, 0
            c->jalr(call_addr);                               // jalr ra, t9
            c->mov64(s3, v0);                                 // or s3, v0, r0
            c->mov64(a1, s3);                                 // or a1, s3, r0
            c->mov64(v1, s3);                                 // or v1, s3, r0
            c->mov64(a0, s2);                                 // or a0, s2, r0
            c->lqc2(vf4, 0, v1);                              // lqc2 vf4, 0(v1)
            c->lqc2(vf5, 0, a0);                              // lqc2 vf5, 0(a0)
            c->vmove(DEST::w, vf6, vf0);                      // vmove.w vf6, vf0
            c->vsub(DEST::xyz, vf6, vf4, vf5);                // vsub.xyz vf6, vf4, vf5
            c->sqc2(vf6, 0, a1);                              // sqc2 vf6, 0(a1)
            c->load_symbol2(t9, cache.vector_normalize);      // lw t9, vector-normalize!(s7)
            c->daddu(a0, r0, s5);                             // daddu a0, r0, s5
            c->lui(a1, 16256);                                // lui a1, 16256
            call_addr = c->gprs[t9].du32[0];                  // function call:
            c->sll(v0, ra, 0);                                // sll v0, ra, 0
            c->jalr(call_addr);                               // jalr ra, t9
            c->load_symbol2(t9, cache.vector_normalize);      // lw t9, vector-normalize!(s7)
            c->daddiu(a0, s5, 16);                            // daddiu a0, s5, 16
            c->lui(a1, 16256);                                // lui a1, 16256
            call_addr = c->gprs[t9].du32[0];                  // function call:
            c->sll(v0, ra, 0);                                // sll v0, ra, 0
            c->jalr(call_addr);                               // jalr ra, t9
            c->load_symbol2(t9, cache.vector_normalize);      // lw t9, vector-normalize!(s7)
            c->daddiu(a0, s5, 32);                            // daddiu a0, s5, 32
            c->lui(a1, 16256);                                // lui a1, 16256
            call_addr = c->gprs[t9].du32[0];                  // function call:
            c->sll(v0, ra, 0);                                // sll v0, ra, 0
            c->jalr(call_addr);                               // jalr ra, t9
            c->lui(v1, 18800);                                // lui v1, 18800
            c->mtc1(f0, v1);                                  // mtc1 f0, v1
            c->lui(v1, 18176);                                // lui v1, 18176
            c->mtc1(f1, v1);                                  // mtc1 f1, v1
            c->mov64(a0, s3);                                 // or a0, s3, r0
            c->daddiu(v1, s5, 16);                            // daddiu v1, s5, 16
            c->lwc1(f2, 0, a0);                               // lwc1 f2, 0(a0)
            c->lwc1(f3, 4, a0);                               // lwc1 f3, 4(a0)
            c->lwc1(f4, 8, a0);                               // lwc1 f4, 8(a0)
            c->lwc1(f5, 0, v1);                               // lwc1 f5, 0(v1)
            c->lwc1(f6, 4, v1);                               // lwc1 f6, 4(v1)
            c->lwc1(f7, 8, v1);                               // lwc1 f7, 8(v1)
            // Unknown instr: mula.s f2, f5
            // Unknown instr: madda.s f3, f6
            // Unknown instr: madd.s f2, f4, f7
            c->fprs[f2] = (c->fprs[f4] * c->fprs[f7]) + (c->fprs[f3] * c->fprs[f6]) + (c->fprs[f2] * c->fprs[f5]);
            c->mfc1(v1, f2);                                  // mfc1 v1, f2
            c->mtc1(f2, v1);                                  // mtc1 f2, v1
            c->maxs(f1, f1, f2);                              // max.s f1, f1, f2
            c->divs(f30, f0, f1);                             // div.s f30, f0, f1
            c->daddiu(s4, sp, 112);                           // daddiu s4, sp, 112
            c->lwu(v1, 52, gp);                               // lwu v1, 52(gp)
            c->sw(v1, 0, s4);                                 // sw v1, 0(s4)
            c->lui(v1, 16256);                                // lui v1, 16256
            c->mtc1(f0, v1);                                  // mtc1 f0, v1
            c->swc1(f0, 4, s4);                               // swc1 f0, 4(s4)
            c->addiu(v1, r0, 8192);                           // addiu v1, r0, 8192
            c->sd(v1, 16, s4);                                // sd v1, 16(s4)
            c->daddiu(v1, s4, 48);                            // daddiu v1, s4, 48
            c->lq(a0, 0, s2);                                 // lq a0, 0(s2)
            c->sq(a0, 0, v1);                                 // sq a0, 0(v1)
            c->mov64(a0, gp);                                 // or a0, gp, r0
            bc = c->sgpr64(s7) == c->sgpr64(a0);              // beq s7, a0, L2
            c->mov64(v1, s7);                                 // or v1, s7, r0
            if (bc) {goto block_2;}                           // branch non-likely

            c->lwu(v1, 24, a0);                               // lwu v1, 24(a0)

            block_2:
            bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L3
            // nop                                            // sll r0, r0, 0
            if (bc) {goto block_4;}                           // branch non-likely

            c->lwu(a0, 0, v1);                                // lwu a0, 0(v1)
            c->lw(a0, 40, a0);                                // lw a0, 40(a0)
            c->dsll32(a0, a0, 0);                             // dsll32 a0, a0, 0
            //beq r0, r0, L4                                  // beq r0, r0, L4
            // nop                                            // sll r0, r0, 0
            goto block_5;                                     // branch always


            block_4:
            c->addiu(a0, r0, 0);                              // addiu a0, r0, 0

            block_5:
            c->sllv(v1, v1, r0);                              // sllv v1, v1, r0
            c->or_(v1, a0, v1);                               // or v1, a0, v1
            c->sd(v1, 24, s4);                                // sd v1, 24(s4)
            c->sd(s7, 32, s4);                                // sd s7, 32(s4)
            c->mov64(a0, gp);                                 // or a0, gp, r0
            bc = c->sgpr64(s7) == c->sgpr64(a0);              // beq s7, a0, L5
            c->mov64(v1, s7);                                 // or v1, s7, r0
            if (bc) {goto block_7;}                           // branch non-likely

            c->lwu(v1, 24, a0);                               // lwu v1, 24(a0)

            block_7:
            bc = c->sgpr64(s7) == c->sgpr64(v1);              // beq s7, v1, L6
            // nop                                            // sll r0, r0, 0
            if (bc) {goto block_9;}                           // branch non-likely

            c->lwu(a0, 0, v1);                                // lwu a0, 0(v1)
            c->lw(a0, 40, a0);                                // lw a0, 40(a0)
            c->dsll32(a0, a0, 0);                             // dsll32 a0, a0, 0
            //beq r0, r0, L7                                  // beq r0, r0, L7
            // nop                                            // sll r0, r0, 0
            goto block_10;                                    // branch always


            block_9:
            c->addiu(a0, r0, 0);                              // addiu a0, r0, 0

            block_10:
            c->sllv(v1, v1, r0);                              // sllv v1, v1, r0
            c->or_(v1, a0, v1);                               // or v1, a0, v1
            c->sd(v1, 40, s4);                                // sd v1, 40(s4)
            c->load_symbol2(v1, cache.game_info);             // lw v1, *game-info*(s7)
            c->lwu(a0, 224, v1);                              // lwu a0, 224(v1)
            c->daddiu(a0, a0, 1);                             // daddiu a0, a0, 1
            c->sw(a0, 224, v1);                               // sw a0, 224(v1)
            c->sw(a0, 8, s4);                                 // sw a0, 8(s4)
            c->addiu(v1, r0, 1200);                           // addiu v1, r0, 1200
            c->sd(v1, 80, s4);                                // sd v1, 80(s4)
            c->daddiu(v1, s4, 64);                            // daddiu v1, s4, 64
            c->daddiu(a0, s5, 16);                            // daddiu a0, s5, 16
            c->lui(a1, 18800);                                // lui a1, 18800
            c->mtc1(f0, a1);                                  // mtc1 f0, a1
            c->lqc2(vf1, 0, a0);                              // lqc2 vf1, 0(a0)
            c->mfc1(a0, f0);                                  // mfc1 a0, f0
            c->mov128_vf_gpr(vf2, a0);                        // qmtc2.i vf2, a0
            c->vadd_bc(DEST::w, BC::x, vf1, vf0, vf0);        // vaddx.w vf1, vf0, vf0
            c->vmul_bc(DEST::xyz, BC::x, vf1, vf1, vf2);      // vmulx.xyz vf1, vf1, vf2
            c->sqc2(vf1, 0, v1);                              // sqc2 vf1, 0(v1)
            c->load_symbol2(t9, cache.vector_flatten);        // lw t9, vector-flatten!(s7)
            c->daddiu(a0, sp, 208);                           // daddiu a0, sp, 208
            c->daddiu(a2, s5, 32);                            // daddiu a2, s5, 32
            c->mov64(a1, s3);                                 // or a1, s3, r0
            call_addr = c->gprs[t9].du32[0];                  // function call:
            c->sll(v0, ra, 0);                                // sll v0, ra, 0
            c->jalr(call_addr);                               // jalr ra, t9
            c->mov64(a0, v0);                                 // or a0, v0, r0
            c->daddu(v1, r0, s5);                             // daddu v1, r0, s5
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
            c->mtc1(f1, r0);                                  // mtc1 f1, r0
            cop1_bc = c->fprs[f1] < c->fprs[f0];              // c.lt.s f1, f0
            bc = !cop1_bc;                                    // bc1f L8
            c->mov64(v1, s7);                                 // or v1, s7, r0
            if (bc) {goto block_12;}                          // branch non-likely

            c->addiu(v1, r0, 0);                              // addiu v1, r0, 0
            c->lui(a0, -14976);                               // lui a0, -14976
            c->mtc1(f1, a0);                                  // mtc1 f1, a0
            c->adds(f0, f1, f0);                              // add.s f0, f1, f0
            c->mfc1(a0, f0);                                  // mfc1 a0, f0
            c->mtc1(f0, a0);                                  // mtc1 f0, a0
            c->cvtws(f0, f0);                                 // cvt.w.s f0, f0
            c->mfc1(a1, f0);                                  // mfc1 a1, f0
            c->slt(a1, v1, a1);                               // slt a1, v1, a1
            c->movn(v1, a0, a1);                              // movn v1, a0, a1
            c->mtc1(f0, v1);                                  // mtc1 f0, v1
            c->mfc1(v1, f0);                                  // mfc1 v1, f0
            //beq r0, r0, L9                                  // beq r0, r0, L9
            // nop                                            // sll r0, r0, 0
            goto block_14;                                    // branch always


            block_12:
            c->mtc1(f1, r0);                                  // mtc1 f1, r0
            cop1_bc = c->fprs[f0] < c->fprs[f1];              // c.lt.s f0, f1
            bc = !cop1_bc;                                    // bc1f L9
            c->mov64(v1, s7);                                 // or v1, s7, r0
            if (bc) {goto block_14;}                          // branch non-likely

            c->addiu(v1, r0, 0);                              // addiu v1, r0, 0
            c->lui(a0, 17792);                                // lui a0, 17792
            c->mtc1(f1, a0);                                  // mtc1 f1, a0
            c->adds(f0, f1, f0);                              // add.s f0, f1, f0
            c->mfc1(a0, f0);                                  // mfc1 a0, f0
            c->mtc1(f0, a0);                                  // mtc1 f0, a0
            c->cvtws(f0, f0);                                 // cvt.w.s f0, f0
            c->mfc1(a1, f0);                                  // mfc1 a1, f0
            c->slt(a1, v1, a1);                               // slt a1, v1, a1
            c->movz(v1, a0, a1);                              // movz v1, a0, a1
            c->mtc1(f0, v1);                                  // mtc1 f0, v1
            c->mfc1(v1, f0);                                  // mfc1 v1, f0

            block_14:
            c->muls(f28, f0, f30);                            // mul.s f28, f0, f30
            c->lui(v1, 18800);                                // lui v1, 18800
            c->mtc1(f26, v1);                                 // mtc1 f26, v1
            c->load_symbol2(t9, cache.tan);                   // lw t9, tan(s7)
            c->lui(v1, 17507);                                // lui v1, 17507
            c->ori(a0, v1, 36409);                            // ori a0, v1, 36409
            call_addr = c->gprs[t9].du32[0];                  // function call:
            c->sll(v0, ra, 0);                                // sll v0, ra, 0
            c->jalr(call_addr);                               // jalr ra, t9
            c->mtc1(f0, v0);                                  // mtc1 f0, v0
            c->muls(f26, f26, f0);                            // mul.s f26, f26, f0
            c->lui(v1, 18800);                                // lui v1, 18800
            c->mtc1(f24, v1);                                 // mtc1 f24, v1
            c->load_symbol2(t9, cache.tan);                   // lw t9, tan(s7)
            c->lui(v1, 17507);                                // lui v1, 17507
            c->ori(a0, v1, 36409);                            // ori a0, v1, 36409
            call_addr = c->gprs[t9].du32[0];                  // function call:
            c->sll(v0, ra, 0);                                // sll v0, ra, 0
            c->jalr(call_addr);                               // jalr ra, t9
            c->mtc1(f0, v0);                                  // mtc1 f0, v0
            c->muls(f0, f24, f0);                             // mul.s f0, f24, f0
            c->negs(f0, f0);                                  // neg.s f0, f0
            c->maxs(f0, f0, f28);                             // max.s f0, f0, f28
            c->mins(f0, f26, f0);                             // min.s f0, f26, f0
            c->daddiu(v1, s4, 64);                            // daddiu v1, s4, 64
            c->daddiu(a0, s4, 64);                            // daddiu a0, s4, 64
            c->daddu(a1, r0, s5);                             // daddu a1, r0, s5
            c->lqc2(vf2, 0, a1);                              // lqc2 vf2, 0(a1)
            c->lqc2(vf1, 0, a0);                              // lqc2 vf1, 0(a0)
            c->mfc1(a0, f0);                                  // mfc1 a0, f0
            c->mov128_vf_gpr(vf3, a0);                        // qmtc2.i vf3, a0
            c->vadd_bc(DEST::w, BC::x, vf4, vf0, vf0);        // vaddx.w vf4, vf0, vf0
            c->vmula_bc(DEST::xyzw, BC::x, vf2, vf3);         // vmulax.xyzw acc, vf2, vf3
            c->vmadd_bc(DEST::xyz, BC::w, vf4, vf1, vf0);     // vmaddw.xyz vf4, vf1, vf0
            c->sqc2(vf4, 0, v1);                              // sqc2 vf4, 0(v1)
            c->lui(v1, 17664);                                // lui v1, 17664
            c->mtc1(f0, v1);                                  // mtc1 f0, v1
            c->muls(f30, f0, f30);                            // mul.s f30, f0, f30
            c->daddiu(s3, s4, 64);                            // daddiu s3, s4, 64
            c->daddiu(s2, s4, 64);                            // daddiu s2, s4, 64
            c->daddu(s1, r0, s5);                             // daddu s1, r0, s5
            c->load_symbol2(t9, cache.rand_vu_float_range);   // lw t9, rand-vu-float-range(s7)
            c->negs(f0, f30);                                 // neg.s f0, f30
            c->mfc1(a0, f0);                                  // mfc1 a0, f0
            c->mfc1(a1, f30);                                 // mfc1 a1, f30
            call_addr = c->gprs[t9].du32[0];                  // function call:
            c->sll(v0, ra, 0);                                // sll v0, ra, 0
            c->jalr(call_addr);                               // jalr ra, t9
            c->mtc1(f0, v0);                                  // mtc1 f0, v0
            c->lqc2(vf2, 0, s1);                              // lqc2 vf2, 0(s1)
            c->lqc2(vf1, 0, s2);                              // lqc2 vf1, 0(s2)
            c->mfc1(v1, f0);                                  // mfc1 v1, f0
            c->mov128_vf_gpr(vf3, v1);                        // qmtc2.i vf3, v1
            c->vadd_bc(DEST::w, BC::x, vf4, vf0, vf0);        // vaddx.w vf4, vf0, vf0
            c->vmula_bc(DEST::xyzw, BC::x, vf2, vf3);         // vmulax.xyzw acc, vf2, vf3
            c->vmadd_bc(DEST::xyz, BC::w, vf4, vf1, vf0);     // vmaddw.xyz vf4, vf1, vf0
            c->sqc2(vf4, 0, s3);                              // sqc2 vf4, 0(s3)
            c->daddiu(s3, s4, 64);                            // daddiu s3, s4, 64
            c->daddiu(s2, s4, 64);                            // daddiu s2, s4, 64
            c->daddiu(s5, s5, 32);                            // daddiu s5, s5, 32
            c->load_symbol2(t9, cache.rand_vu_float_range);   // lw t9, rand-vu-float-range(s7)
            c->negs(f0, f30);                                 // neg.s f0, f30
            c->mfc1(a0, f0);                                  // mfc1 a0, f0
            c->mfc1(a1, f30);                                 // mfc1 a1, f30
            call_addr = c->gprs[t9].du32[0];                  // function call:
            c->sll(v0, ra, 0);                                // sll v0, ra, 0
            c->jalr(call_addr);                               // jalr ra, t9
            c->mtc1(f0, v0);                                  // mtc1 f0, v0
            c->lqc2(vf2, 0, s5);                              // lqc2 vf2, 0(s5)
            c->lqc2(vf1, 0, s2);                              // lqc2 vf1, 0(s2)
            c->mfc1(v1, f0);                                  // mfc1 v1, f0
            c->mov128_vf_gpr(vf3, v1);                        // qmtc2.i vf3, v1
            c->vadd_bc(DEST::w, BC::x, vf4, vf0, vf0);        // vaddx.w vf4, vf0, vf0
            c->vmula_bc(DEST::xyzw, BC::x, vf2, vf3);         // vmulax.xyzw acc, vf2, vf3
            c->vmadd_bc(DEST::xyz, BC::w, vf4, vf1, vf0);     // vmaddw.xyz vf4, vf1, vf0
            c->sqc2(vf4, 0, s3);                              // sqc2 vf4, 0(s3)
            c->load_symbol2(t9, cache.spawn_projectile);      // lw t9, spawn-projectile(s7)
            c->load_symbol2(a0, cache.squid_shot);            // lw a0, squid-shot(s7)
            c->load_symbol2(a3, cache.default_dead_pool);     // lw a3, *default-dead-pool*(s7)
            c->mov64(a1, s4);                                 // or a1, s4, r0
            c->mov64(a2, gp);                                 // or a2, gp, r0
            call_addr = c->gprs[t9].du32[0];                  // function call:
            c->sll(v0, ra, 0);                                // sll v0, ra, 0
            c->jalr(call_addr);                               // jalr ra, t9
            c->ld(ra, 0, sp);                                 // ld ra, 0(sp)
            c->lwc1(f30, 332, sp);                            // lwc1 f30, 332(sp)
            c->lwc1(f28, 328, sp);                            // lwc1 f28, 328(sp)
            c->lwc1(f26, 324, sp);                            // lwc1 f26, 324(sp)
            c->lwc1(f24, 320, sp);                            // lwc1 f24, 320(sp)
            c->lq(gp, 304, sp);                               // lq gp, 304(sp)
            c->lq(s5, 288, sp);                               // lq s5, 288(sp)
            c->lq(s4, 272, sp);                               // lq s4, 272(sp)
            c->lq(s3, 256, sp);                               // lq s3, 256(sp)
            c->lq(s2, 240, sp);                               // lq s2, 240(sp)
            c->lq(s1, 224, sp);                               // lq s1, 224(sp)
            //jr ra                                           // jr ra
            c->daddiu(sp, sp, 336);                           // daddiu sp, sp, 336
            goto end_of_function;                             // return

            // nop                                            // sll r0, r0, 0
            // nop                                            // sll r0, r0, 0
            end_of_function:
            return c->gprs[v0].du64[0];
        }

        void link() {
            cache.default_dead_pool = intern_from_c("*default-dead-pool*").c();
            cache.game_info = intern_from_c("*game-info*").c();
            cache.rand_vu_float_range = intern_from_c("rand-vu-float-range").c();
            cache.spawn_projectile = intern_from_c("spawn-projectile").c();
            cache.squid_increment_shield = intern_from_c("squid-increment-shield").c();
            cache.squid_shot = intern_from_c("squid-shot").c();
            cache.tan = intern_from_c("tan").c();
            cache.vector_flatten = intern_from_c("vector-flatten!").c();
            cache.vector_normalize = intern_from_c("vector-normalize!").c();
            cache.vector_cspace = intern_from_c("vector<-cspace!").c();
            gLinkedFunctionTable.reg("(method 53 squid)", execute, 512);
        }

    } // namespace method_53_squid
} // namespace Mips2C
