#include <cstring>
#include <vector>

#include "goalc/emitter/CodeTester.h"
#include "goalc/emitter/IGen.h"
#include "gtest/gtest.h"

using namespace emitter;

namespace {
const auto instr_set = InstructionSet::ARM64;

CodeTester create_tester(int code_capacity = 1024) {
  CodeTester tester(instr_set);
  tester.init_code_buffer(code_capacity);
  return tester;
}

}  // namespace

TEST(NEONEmitter, VF_NOP) {
  CodeTester tester = create_tester();
  tester.emit(IGen::nop_vf(tester.generator()));
  EXPECT_EQ(tester.dump_to_hex_string(true), "1F2003D5");
}

TEST(NEONEmitter, WAIT_VF) {
  CodeTester tester = create_tester();
  tester.emit(IGen::wait_vf(tester.generator()));
  EXPECT_EQ(tester.dump_to_hex_string(true), "1F2003D5");
}

// 128-bit loads and stores with a register offset. GOAL addresses vector memory as a base
// register plus an offset register, so every addressing form has to use both.
//
// These run the code they emit, so they only build on an arm64 host. emitter_util.cpp guards
// the arm64 tests that go through it the same way.
#ifdef __aarch64__

namespace {
constexpr int kGoalPtr = 4096;
constexpr int kBufSize = 16384;

struct VecMem {
  std::vector<u8> buf;
  VecMem() : buf(kBufSize, 0) {}
  u8* base() { return buf.data(); }
  u8* at(int off) { return buf.data() + off; }
};

// two 16 byte vector slots, back to back
constexpr int kA = kGoalPtr;
constexpr int kB = kGoalPtr + 16;
// far enough from kA that a displaced store cannot land on the markers around it
constexpr int kDst = kGoalPtr + 4096;

// x0 = base, x1 = source offset, x2 = second offset, x3 = destination offset
template <typename EmitOp>
void run_mem(CodeTester& tester, VecMem& mem, EmitOp emit_op) {
  tester.clear();
  tester.emit_push_all_gprs(true);
  emit_op(tester);
  tester.emit_pop_all_gprs(true);
  tester.emit_return();
  tester.execute_ret<u64>((u64)mem.base(), kA, kB, kDst);
}
}  // namespace

TEST(NEONEmitter, vector_load_store_register_offset_uses_both_registers) {
  CodeTester tester = create_tester(2048);
  VecMem mem;

  const u32 want[4] = {0x11112222u, 0x33334444u, 0x55556666u, 0x77778888u};
  const u32 decoy[4] = {0xdeadbeefu, 0xdeadbeefu, 0xdeadbeefu, 0xdeadbeefu};
  memcpy(mem.at(kA), want, 16);
  memcpy(mem.at(0), decoy, 16);  // what a dropped offset register would read instead
  memset(mem.at(kDst), 0, 16);

  run_mem(tester, mem, [](CodeTester& t) {
    t.emit(IGen::loadvf_gpr64_plus_gpr64(t.generator(), Register(V0), Register(X0), Register(X1)));
    t.emit(IGen::storevf_gpr64_plus_gpr64(t.generator(), Register(V0), Register(X0), Register(X3)));
  });

  u32 got[4];
  memcpy(got, mem.at(kDst), 16);
  for (int i = 0; i < 4; i++) {
    EXPECT_EQ(got[i], want[i]) << "reg+reg round trip lane " << i;
  }
  tester.clear();
}

TEST(NEONEmitter, vector_load_store_displaced_reads_base_plus_index_plus_offset) {
  CodeTester tester = create_tester(2048);
  VecMem mem;

  // put a marker 16 bytes past kA and another 16 before it, so a wrong displacement is visible
  const u32 lo[4] = {0xa000u, 0xa001u, 0xa002u, 0xa003u};
  const u32 mid[4] = {0xb000u, 0xb001u, 0xb002u, 0xb003u};
  const u32 hi[4] = {0xc000u, 0xc001u, 0xc002u, 0xc003u};
  memcpy(mem.at(kA - 16), lo, 16);
  memcpy(mem.at(kA), mid, 16);
  memcpy(mem.at(kA + 16), hi, 16);

  struct Case {
    int disp;
    const u32* want;
  };
  const Case cases[] = {{-16, lo}, {0, mid}, {16, hi}};

  for (auto& c : cases) {
    // s8 form
    memset(mem.at(kDst), 0, 16);
    run_mem(tester, mem, [&](CodeTester& t) {
      t.emit(IGen::loadvf_gpr64_plus_gpr64_plus_s8(t.generator(), Register(V0), Register(X0),
                                                   Register(X1), c.disp));
      t.emit(
          IGen::storevf_gpr64_plus_gpr64(t.generator(), Register(V0), Register(X0), Register(X3)));
    });
    u32 got[4];
    memcpy(got, mem.at(kDst), 16);
    for (int i = 0; i < 4; i++) {
      EXPECT_EQ(got[i], c.want[i]) << "load s8 disp " << c.disp << " lane " << i;
    }

    // s32 form, same answers
    memset(mem.at(kDst), 0, 16);
    run_mem(tester, mem, [&](CodeTester& t) {
      t.emit(IGen::loadvf_gpr64_plus_gpr64_plus_s32(t.generator(), Register(V0), Register(X0),
                                                    Register(X1), c.disp));
      t.emit(
          IGen::storevf_gpr64_plus_gpr64(t.generator(), Register(V0), Register(X0), Register(X3)));
    });
    memcpy(got, mem.at(kDst), 16);
    for (int i = 0; i < 4; i++) {
      EXPECT_EQ(got[i], c.want[i]) << "load s32 disp " << c.disp << " lane " << i;
    }

    // and the displaced store lands where it should
    memset(mem.at(kDst - 16), 0, 48);
    run_mem(tester, mem, [&](CodeTester& t) {
      t.emit(
          IGen::loadvf_gpr64_plus_gpr64(t.generator(), Register(V0), Register(X0), Register(X1)));
      t.emit(IGen::storevf_gpr64_plus_gpr64_plus_s8(t.generator(), Register(V0), Register(X0),
                                                    Register(X3), c.disp));
    });
    memcpy(got, mem.at(kDst + c.disp), 16);
    for (int i = 0; i < 4; i++) {
      EXPECT_EQ(got[i], mid[i]) << "store s8 disp " << c.disp << " lane " << i;
    }
  }
  tester.clear();
}

TEST(NEONEmitter, displaced_vector_access_leaves_the_address_registers_alone) {
  CodeTester tester = create_tester(2048);
  VecMem mem;

  const u32 v[4] = {1, 2, 3, 4};
  memcpy(mem.at(kA), v, 16);

  // load twice from the same place. a post indexed form would walk the base on the first one
  // and read somewhere else on the second.
  memset(mem.at(kDst), 0, 16);
  run_mem(tester, mem, [](CodeTester& t) {
    t.emit(IGen::loadvf_gpr64_plus_gpr64_plus_s8(t.generator(), Register(V0), Register(X0),
                                                 Register(X1), 0));
    t.emit(IGen::loadvf_gpr64_plus_gpr64_plus_s8(t.generator(), Register(V1), Register(X0),
                                                 Register(X1), 0));
    t.emit(IGen::storevf_gpr64_plus_gpr64(t.generator(), Register(V1), Register(X0), Register(X3)));
  });
  u32 got[4];
  memcpy(got, mem.at(kDst), 16);
  for (int i = 0; i < 4; i++) {
    EXPECT_EQ(got[i], v[i]) << "second load lane " << i;
  }
  tester.clear();
}

#endif  // __aarch64__
