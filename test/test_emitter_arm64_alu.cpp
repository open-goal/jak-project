#include <cmath>
#include <cstring>
#include <limits>
#include <vector>

#include "common/common_types.h"

#include "goalc/emitter/CodeTester.h"
#include "goalc/emitter/IGen.h"
#include "goalc/emitter/Register.h"
#include "gtest/gtest.h"

#if defined(__aarch64__)

using namespace emitter;

namespace {

// values that separate the logical operations
const std::vector<std::pair<u64, u64>> kPairs = {
    {0, 0},
    {0xffffffffffffffffull, 0xffffffffffffffffull},
    {0xf0f0f0f0f0f0f0f0ull, 0x0ff00ff00ff00ff0ull},
    {0x0000000000000100ull, 0x0000000000000100ull},  // and returns 0x100 while add returns 0x200
    {0x123456789abcdef0ull, 0x0fedcba987654321ull},
    {0x8000000000000000ull, 0x8000000000000001ull},
    {0x00000000ffffffffull, 0xffffffff00000000ull},
    {0xdeadbeefcafebabeull, 0x00000000000000ffull},
};

constexpr int kGoalPtr = 4096;
constexpr int kBufSize = 16384;

struct VecMem {
  std::vector<u8> buf;
  VecMem() : buf(kBufSize, 0) {}
  u8* base() { return buf.data(); }
  u8* at(int off) { return buf.data() + off; }
};

constexpr int kA = kGoalPtr;
constexpr int kB = kGoalPtr + 16;
constexpr int kOut = kGoalPtr + 32;

// run one vector op through memory
template <typename EmitOp>
void run_vec(CodeTester& tester, VecMem& mem, EmitOp emit_op) {
  tester.clear();
  tester.emit_push_all_gprs(true);
  tester.emit(
      IGen::load_goal_xmm128(tester.generator(), Register(V0), Register(X0), Register(X1), 0));
  tester.emit(
      IGen::load_goal_xmm128(tester.generator(), Register(V1), Register(X2), Register(X1), 0));
  emit_op(tester);
  tester.emit(IGen::store_goal_vf(tester.generator(), Register(X3), Register(V2), Register(X1), 0));
  tester.emit_pop_all_gprs(true);
  tester.emit_return();
  tester.execute_ret<u64>(kA, (u64)mem.base(), kB, kOut);
}

void put_floats(VecMem& mem, int off, const float (&v)[4]) {
  memcpy(mem.at(off), v, 16);
}

void get_floats(VecMem& mem, int off, float (&v)[4]) {
  memcpy(v, mem.at(off), 16);
}

}  // namespace

#ifdef __aarch64__  // runs the code it emits
TEST(ARM64Alu, gpr_logical_ops) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(1024);

  for (auto& p : kPairs) {
    tester.clear();
    tester.emit_push_all_gprs(true);
    tester.emit(IGen::and_gpr64_gpr64(tester.generator(), Register(X0), Register(X1)));
    tester.emit_pop_all_gprs(true);
    tester.emit_return();
    EXPECT_EQ(tester.execute_ret<u64>(p.first, p.second, 0, 0), p.first & p.second)
        << "and " << std::hex << p.first << " & " << p.second;

    tester.clear();
    tester.emit_push_all_gprs(true);
    tester.emit(IGen::or_gpr64_gpr64(tester.generator(), Register(X0), Register(X1)));
    tester.emit_pop_all_gprs(true);
    tester.emit_return();
    EXPECT_EQ(tester.execute_ret<u64>(p.first, p.second, 0, 0), p.first | p.second)
        << "or " << std::hex << p.first << " | " << p.second;

    tester.clear();
    tester.emit_push_all_gprs(true);
    tester.emit(IGen::xor_gpr64_gpr64(tester.generator(), Register(X0), Register(X1)));
    tester.emit_pop_all_gprs(true);
    tester.emit_return();
    EXPECT_EQ(tester.execute_ret<u64>(p.first, p.second, 0, 0), p.first ^ p.second)
        << "xor " << std::hex << p.first << " ^ " << p.second;

    tester.clear();
    tester.emit_push_all_gprs(true);
    tester.emit(IGen::not_gpr64(tester.generator(), Register(X0)));
    tester.emit_pop_all_gprs(true);
    tester.emit_return();
    EXPECT_EQ(tester.execute_ret<u64>(p.first, 0, 0, 0), ~p.first) << "not " << std::hex << p.first;
  }
  tester.clear();
}
#endif  // __aarch64__

#ifdef __aarch64__  // runs the code it emits
TEST(ARM64Alu, gpr_arithmetic_ops) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(1024);

  for (auto& p : kPairs) {
    tester.clear();
    tester.emit_push_all_gprs(true);
    tester.emit(IGen::add_gpr64_gpr64(tester.generator(), Register(X0), Register(X1)));
    tester.emit_pop_all_gprs(true);
    tester.emit_return();
    EXPECT_EQ(tester.execute_ret<u64>(p.first, p.second, 0, 0), u64(p.first + p.second))
        << "add " << std::hex << p.first << " + " << p.second;

    tester.clear();
    tester.emit_push_all_gprs(true);
    tester.emit(IGen::sub_gpr64_gpr64(tester.generator(), Register(X0), Register(X1)));
    tester.emit_pop_all_gprs(true);
    tester.emit_return();
    EXPECT_EQ(tester.execute_ret<u64>(p.first, p.second, 0, 0), u64(p.first - p.second))
        << "sub " << std::hex << p.first << " - " << p.second;

    tester.clear();
    tester.emit_push_all_gprs(true);
    tester.emit(IGen::imul_gpr64_gpr64(tester.generator(), Register(X0), Register(X1)));
    tester.emit_pop_all_gprs(true);
    tester.emit_return();
    EXPECT_EQ(tester.execute_ret<u64>(p.first, p.second, 0, 0), u64(p.first * p.second))
        << "imul " << std::hex << p.first << " * " << p.second;
  }
  tester.clear();
}
#endif  // __aarch64__

#ifdef __aarch64__  // runs the code it emits
TEST(ARM64Alu, gpr_shifts) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(1024);

  const u64 value = 0x8123456789abcdefull;
  for (int sa = 0; sa < 64; sa++) {
    tester.clear();
    tester.emit_push_all_gprs(true);
    tester.emit(IGen::shl_gpr64_u8(tester.generator(), Register(X0), u8(sa)));
    tester.emit_pop_all_gprs(true);
    tester.emit_return();
    EXPECT_EQ(tester.execute_ret<u64>(value, 0, 0, 0), value << sa) << "shl imm " << sa;

    tester.clear();
    tester.emit_push_all_gprs(true);
    tester.emit(IGen::shr_gpr64_u8(tester.generator(), Register(X0), u8(sa)));
    tester.emit_pop_all_gprs(true);
    tester.emit_return();
    EXPECT_EQ(tester.execute_ret<u64>(value, 0, 0, 0), value >> sa) << "shr imm " << sa;

    tester.clear();
    tester.emit_push_all_gprs(true);
    tester.emit(IGen::sar_gpr64_u8(tester.generator(), Register(X0), u8(sa)));
    tester.emit_pop_all_gprs(true);
    tester.emit_return();
    EXPECT_EQ(tester.execute_ret<s64>(value, 0, 0, 0), s64(value) >> sa) << "sar imm " << sa;

    tester.clear();
    tester.emit_push_all_gprs(true);
    tester.emit(IGen::shl_gpr64_reg(tester.generator(), Register(X0), Register(X1)));
    tester.emit_pop_all_gprs(true);
    tester.emit_return();
    EXPECT_EQ(tester.execute_ret<u64>(value, sa, 0, 0), value << sa) << "shl reg " << sa;

    tester.clear();
    tester.emit_push_all_gprs(true);
    tester.emit(IGen::shr_gpr64_reg(tester.generator(), Register(X0), Register(X1)));
    tester.emit_pop_all_gprs(true);
    tester.emit_return();
    EXPECT_EQ(tester.execute_ret<u64>(value, sa, 0, 0), value >> sa) << "shr reg " << sa;

    tester.clear();
    tester.emit_push_all_gprs(true);
    tester.emit(IGen::sar_gpr64_reg(tester.generator(), Register(X0), Register(X1)));
    tester.emit_pop_all_gprs(true);
    tester.emit_return();
    EXPECT_EQ(tester.execute_ret<s64>(value, sa, 0, 0), s64(value) >> sa) << "sar reg " << sa;
  }
  tester.clear();
}
#endif  // __aarch64__

TEST(ARM64Alu, vector_float_ops) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(1024);
  VecMem mem;

  const float a[4] = {1.5f, -2.25f, 1e20f, 0.125f};
  const float b[4] = {4.0f, 8.5f, -3.5f, 1e-20f};
  put_floats(mem, kA, a);
  put_floats(mem, kB, b);

  struct Case {
    const char* name;
    Instruction (*emit)(const ObjectGenerator&, Register, Register, Register);
    float (*expect)(float, float);
  };
  const Case cases[] = {
      {"add", IGen::add_vf, [](float x, float y) { return x + y; }},
      {"sub", IGen::sub_vf, [](float x, float y) { return x - y; }},
      {"mul", IGen::mul_vf, [](float x, float y) { return x * y; }},
      {"div", IGen::div_vf, [](float x, float y) { return x / y; }},
      {"max", IGen::max_vf, [](float x, float y) { return x > y ? x : y; }},
      {"min", IGen::min_vf, [](float x, float y) { return x < y ? x : y; }},
  };

  for (auto& c : cases) {
    memset(mem.at(kOut), 0, 16);
    run_vec(tester, mem, [&](CodeTester& t) {
      t.emit(c.emit(t.generator(), Register(V2), Register(V0), Register(V1)));
    });
    float got[4];
    get_floats(mem, kOut, got);
    for (int i = 0; i < 4; i++) {
      EXPECT_FLOAT_EQ(got[i], c.expect(a[i], b[i])) << c.name << " lane " << i;
    }
  }

  {
    const float s[4] = {4.0f, 9.0f, 2.0f, 1e10f};
    put_floats(mem, kA, s);
    memset(mem.at(kOut), 0, 16);
    run_vec(tester, mem, [](CodeTester& t) {
      t.emit(IGen::sqrt_vf(t.generator(), Register(V2), Register(V0)));
    });
    float got[4];
    get_floats(mem, kOut, got);
    for (int i = 0; i < 4; i++) {
      EXPECT_FLOAT_EQ(got[i], std::sqrt(s[i])) << "sqrt lane " << i;
    }
  }
  tester.clear();
}

TEST(ARM64Alu, vector_int_float_conversions) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(1024);
  VecMem mem;

  const s32 ints[4] = {0, -7, 12345, -2000000000};
  memcpy(mem.at(kA), ints, 16);
  memset(mem.at(kOut), 0, 16);
  run_vec(tester, mem,
          [](CodeTester& t) { t.emit(IGen::itof_vf(t.generator(), Register(V2), Register(V0))); });
  float asf[4];
  get_floats(mem, kOut, asf);
  for (int i = 0; i < 4; i++) {
    EXPECT_FLOAT_EQ(asf[i], float(ints[i])) << "itof lane " << i;
  }

  const float floats[4] = {0.0f, -7.9f, 12345.5f, 1e9f};
  put_floats(mem, kA, floats);
  memset(mem.at(kOut), 0, 16);
  run_vec(tester, mem,
          [](CodeTester& t) { t.emit(IGen::ftoi_vf(t.generator(), Register(V2), Register(V0))); });
  s32 asi[4];
  memcpy(asi, mem.at(kOut), 16);
  for (int i = 0; i < 4; i++) {
    EXPECT_EQ(asi[i], s32(floats[i])) << "ftoi lane " << i;  // truncates toward zero
  }
  tester.clear();
}

#ifdef __aarch64__  // runs the code it emits
TEST(ARM64Alu, scalar_int_float_conversions) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(1024);

  // different source and destination registers catch a bad Rn field
  for (s32 v : {0, 1, -1, 7, -7, 12345, -2000000000, 2000000000}) {
    tester.clear();
    tester.emit_push_all_gprs(true);
    // s0 = (float)w1, then return its bits in x0
    tester.emit(IGen::int32_to_f32(tester.generator(), Register(V0), Register(X1)));
    tester.emit(IGen::movd_gpr32_f32(tester.generator(), Register(X0), Register(V0)));
    tester.emit_pop_all_gprs(true);
    tester.emit_return();

    u32 got = (u32)tester.execute_ret<u64>(0, (u64)(u32)v, 0, 0);
    float want_f = float(v);
    u32 want;
    memcpy(&want, &want_f, 4);
    EXPECT_EQ(got, want) << "int32_to_f32 of " << v;
  }

  for (float f : {0.0f, 1.0f, -1.0f, 7.9f, -7.9f, 12345.5f, 1e9f}) {
    u32 bits;
    memcpy(&bits, &f, 4);
    tester.clear();
    tester.emit_push_all_gprs(true);
    // s1 = the bits, then w0 = (int)s1
    tester.emit(IGen::movd_f32_gpr32(tester.generator(), Register(V1), Register(X1)));
    tester.emit(IGen::f32_to_int32(tester.generator(), Register(X0), Register(V1)));
    tester.emit_pop_all_gprs(true);
    tester.emit_return();

    s32 got = (s32)(u32)tester.execute_ret<u64>(0, bits, 0, 0);
    EXPECT_EQ(got, s32(f)) << "f32_to_int32 of " << f;  // truncates toward zero
  }
  tester.clear();
}
#endif  // __aarch64__

#ifdef __aarch64__  // runs the code it emits
TEST(ARM64Alu, float_to_int_overflow) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(1024);

  auto convert = [&](float f) {
    u32 bits;
    memcpy(&bits, &f, 4);
    tester.clear();
    tester.emit_push_all_gprs(true);
    tester.emit(IGen::movd_f32_gpr32(tester.generator(), Register(V1), Register(X1)));
    tester.emit(IGen::f32_to_int32(tester.generator(), Register(X0), Register(V1)));
    tester.emit_pop_all_gprs(true);
    tester.emit_return();
    return (s32)(u32)tester.execute_ret<u64>(0, bits, 0, 0);
  };

  constexpr s32 kIntMax = 2147483647;
  constexpr s32 kIntMin = -2147483647 - 1;

  // 2147483520 is the largest exactly representable float below INT_MAX.
  EXPECT_EQ(convert(0.0f), 0);
  EXPECT_EQ(convert(7.9f), 7) << "round toward zero";
  EXPECT_EQ(convert(-7.9f), -7) << "round toward zero, not down";
  EXPECT_EQ(convert(2147483520.0f), 2147483520) << "largest exact in-range float";
  EXPECT_EQ(convert(-2147483648.0f), kIntMin) << "-2^31 is exactly representable and in range";

  // finite overflow clamps to the nearest bound
  EXPECT_EQ(convert(2147483648.0f), kIntMax) << "2^31, the first positive overflow";
  EXPECT_EQ(convert(3e9f), kIntMax) << "positive overflow";
  EXPECT_EQ(convert(26843545600.0f), kIntMax) << "the value tricky-floats.gc uses";
  EXPECT_EQ(convert(-2147483904.0f), kIntMin) << "the first negative overflow";
  EXPECT_EQ(convert(-3e9f), kIntMin) << "negative overflow";

  // ARM64 results for NaN and infinities
  EXPECT_EQ(convert(std::numeric_limits<float>::infinity()), kIntMax) << "+inf";
  EXPECT_EQ(convert(-std::numeric_limits<float>::infinity()), kIntMin) << "-inf";
  EXPECT_EQ(convert(std::numeric_limits<float>::quiet_NaN()), 0) << "NaN";
  tester.clear();
}
#endif  // __aarch64__

TEST(ARM64Alu, vector_integer_ops) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(1024);
  VecMem mem;

  const u32 a[4] = {0xf0f0f0f0u, 0x00000100u, 0xffffffffu, 0x12345678u};
  const u32 b[4] = {0x0ff00ff0u, 0x00000100u, 0x00000000u, 0x87654321u};
  memcpy(mem.at(kA), a, 16);
  memcpy(mem.at(kB), b, 16);

  struct Case {
    const char* name;
    Instruction (*emit)(const ObjectGenerator&, Register, Register, Register);
    u32 (*expect)(u32, u32);
  };
  const Case cases[] = {
      {"and", IGen::parallel_bitwise_and, [](u32 x, u32 y) { return x & y; }},
      {"or", IGen::parallel_bitwise_or, [](u32 x, u32 y) { return x | y; }},
      {"xor", IGen::parallel_bitwise_xor, [](u32 x, u32 y) { return x ^ y; }},
      {"xor_vf", IGen::xor_vf, [](u32 x, u32 y) { return x ^ y; }},
      {"psubd", IGen::vpsubd, [](u32 x, u32 y) { return u32(x - y); }},
  };

  for (auto& c : cases) {
    memset(mem.at(kOut), 0, 16);
    run_vec(tester, mem, [&](CodeTester& t) {
      t.emit(c.emit(t.generator(), Register(V2), Register(V0), Register(V1)));
    });
    u32 got[4];
    memcpy(got, mem.at(kOut), 16);
    for (int i = 0; i < 4; i++) {
      EXPECT_EQ(got[i], c.expect(a[i], b[i])) << c.name << " lane " << i;
    }
  }

  memset(mem.at(kOut), 0, 16);
  run_vec(tester, mem, [](CodeTester& t) {
    t.emit(IGen::parallel_add_byte(t.generator(), Register(V2), Register(V0), Register(V1)));
  });
  u8 ab[16], bb[16], gb[16];
  memcpy(ab, mem.at(kA), 16);
  memcpy(bb, mem.at(kB), 16);
  memcpy(gb, mem.at(kOut), 16);
  for (int i = 0; i < 16; i++) {
    EXPECT_EQ(gb[i], u8(ab[i] + bb[i])) << "padd byte " << i;
  }
  tester.clear();
}

TEST(ARM64Alu, pext_interleave) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(1024);
  VecMem mem;

  u8 a[16], b[16];
  for (int i = 0; i < 16; i++) {
    a[i] = u8(i);         // 00 01 02 .. 0f
    b[i] = u8(0x10 + i);  // 10 11 12 .. 1f
  }
  memcpy(mem.at(kA), a, 16);
  memcpy(mem.at(kB), b, 16);

  struct Case {
    const char* name;
    Instruction (*emit)(const ObjectGenerator&, Register, Register, Register);
    int elem;   // bytes per element
    bool high;  // low half or high half of the sources
  };
  const Case cases[] = {
      {"pextlb", IGen::pextlb_swapped, 1, false}, {"pextub", IGen::pextub_swapped, 1, true},
      {"pextlh", IGen::pextlh_swapped, 2, false}, {"pextuh", IGen::pextuh_swapped, 2, true},
      {"pextlw", IGen::pextlw_swapped, 4, false}, {"pextuw", IGen::pextuw_swapped, 4, true},
  };

  for (auto& c : cases) {
    memset(mem.at(kOut), 0, 16);
    run_vec(tester, mem, [&](CodeTester& t) {
      t.emit(c.emit(t.generator(), Register(V2), Register(V0), Register(V1)));
    });
    u8 got[16];
    memcpy(got, mem.at(kOut), 16);

    // alternate elements from the chosen half
    const int n = 16 / c.elem;  // elements per vector
    const int first = c.high ? n / 2 : 0;
    u8 want[16];
    for (int i = 0; i < n; i++) {
      const u8* srcv = (i % 2) ? b : a;
      const int src_elem = first + i / 2;
      memcpy(want + i * c.elem, srcv + src_elem * c.elem, c.elem);
    }
    EXPECT_EQ(memcmp(got, want, 16), 0) << c.name << " does not interleave";
  }
  tester.clear();
}

TEST(ARM64Alu, vector_compares) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(1024);
  VecMem mem;

  const s32 a[4] = {5, -3, 0, 100};
  const s32 b[4] = {5, 7, 0, -100};
  memcpy(mem.at(kA), a, 16);
  memcpy(mem.at(kB), b, 16);

  memset(mem.at(kOut), 0, 16);
  run_vec(tester, mem, [](CodeTester& t) {
    t.emit(IGen::parallel_compare_e_w(t.generator(), Register(V2), Register(V0), Register(V1)));
  });
  u32 got[4];
  memcpy(got, mem.at(kOut), 16);
  for (int i = 0; i < 4; i++) {
    EXPECT_EQ(got[i], a[i] == b[i] ? 0xffffffffu : 0u) << "cmpeq w lane " << i;
  }

  memset(mem.at(kOut), 0, 16);
  run_vec(tester, mem, [](CodeTester& t) {
    t.emit(IGen::parallel_compare_gt_w(t.generator(), Register(V2), Register(V0), Register(V1)));
  });
  memcpy(got, mem.at(kOut), 16);
  for (int i = 0; i < 4; i++) {
    EXPECT_EQ(got[i], a[i] > b[i] ? 0xffffffffu : 0u) << "cmpgt w lane " << i;
  }
  tester.clear();
}

#ifdef __aarch64__  // runs the code it emits
TEST(ARM64Alu, lane_shuffles) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(1024);
  VecMem mem;

  const u32 a[4] = {0xaaaa0000u, 0xbbbb1111u, 0xcccc2222u, 0xdddd3333u};
  memcpy(mem.at(kA), a, 16);

  // all 256 controls through both entry points
  for (int imm = 0; imm < 256; imm++) {
    const u8 dx = imm & 3, dy = (imm >> 2) & 3, dz = (imm >> 4) & 3, dw = (imm >> 6) & 3;

    memset(mem.at(kOut), 0, 16);
    run_vec(tester, mem, [&](CodeTester& t) {
      t.emit(IGen::swizzle_vf(t.generator(), Register(V2), Register(V0), u8(imm)));
    });
    u32 got[4];
    memcpy(got, mem.at(kOut), 16);
    EXPECT_EQ(got[0], a[dx]) << "swizzle 0x" << std::hex << imm << " lane 0";
    EXPECT_EQ(got[1], a[dy]) << "swizzle 0x" << std::hex << imm << " lane 1";
    EXPECT_EQ(got[2], a[dz]) << "swizzle 0x" << std::hex << imm << " lane 2";
    EXPECT_EQ(got[3], a[dw]) << "swizzle 0x" << std::hex << imm << " lane 3";

    // shuffle_vf uses the same lane picks
    memset(mem.at(kOut), 0, 16);
    run_vec(tester, mem, [&](CodeTester& t) {
      t.emit(IGen::shuffle_vf(t.generator(), Register(V2), Register(V0), dx, dy, dz, dw));
    });
    memcpy(got, mem.at(kOut), 16);
    EXPECT_EQ(got[0], a[dx]) << "shuffle 0x" << std::hex << imm << " lane 0";
    EXPECT_EQ(got[1], a[dy]) << "shuffle 0x" << std::hex << imm << " lane 1";
    EXPECT_EQ(got[2], a[dz]) << "shuffle 0x" << std::hex << imm << " lane 2";
    EXPECT_EQ(got[3], a[dw]) << "shuffle 0x" << std::hex << imm << " lane 3";
  }

  // destination aliases the source through v16
  memcpy(mem.at(kA), a, 16);
  tester.clear();
  tester.emit_push_all_gprs(true);
  tester.emit(
      IGen::load_goal_xmm128(tester.generator(), Register(V0), Register(X0), Register(X1), 0));
  tester.emit(IGen::swizzle_vf(tester.generator(), Register(V0), Register(V0), 0b00011011));
  tester.emit(IGen::store_goal_vf(tester.generator(), Register(X3), Register(V0), Register(X1), 0));
  tester.emit_pop_all_gprs(true);
  tester.emit_return();
  memset(mem.at(kOut), 0, 16);
  tester.execute_ret<u64>(kA, (u64)mem.base(), kB, kOut);
  u32 rev[4];
  memcpy(rev, mem.at(kOut), 16);
  for (int i = 0; i < 4; i++) {
    EXPECT_EQ(rev[i], a[3 - i]) << "in-place reverse lane " << i;
  }
  tester.clear();
}
#endif  // __aarch64__

TEST(ARM64Alu, splat) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(1024);
  VecMem mem;

  const u32 a[4] = {0xaaaa0000u, 0xbbbb1111u, 0xcccc2222u, 0xdddd3333u};
  memcpy(mem.at(kA), a, 16);

  const Register::VF_ELEMENT elems[4] = {Register::VF_ELEMENT::X, Register::VF_ELEMENT::Y,
                                         Register::VF_ELEMENT::Z, Register::VF_ELEMENT::W};
  for (int e = 0; e < 4; e++) {
    memset(mem.at(kOut), 0, 16);
    run_vec(tester, mem, [&](CodeTester& t) {
      t.emit(IGen::splat_vf(t.generator(), Register(V2), Register(V0), elems[e]));
    });
    u32 got[4];
    memcpy(got, mem.at(kOut), 16);
    for (int lane = 0; lane < 4; lane++) {
      EXPECT_EQ(got[lane], a[e]) << "splat elem " << e << " lane " << lane;
    }
  }
  tester.clear();
}

TEST(ARM64Alu, vector_shifts) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(1024);
  VecMem mem;

  const u32 w[4] = {0x80000000u, 0x12345678u, 0xffffffffu, 0x00000001u};
  memcpy(mem.at(kA), w, 16);

  // right shifts start at 1 on ARM64
  for (int sa = 1; sa <= 32; sa++) {
    memset(mem.at(kOut), 0, 16);
    run_vec(tester, mem, [&](CodeTester& t) {
      t.emit(IGen::pw_sra(t.generator(), Register(V2), Register(V0), u8(sa)));
    });
    s32 got[4];
    memcpy(got, mem.at(kOut), 16);
    for (int i = 0; i < 4; i++) {
      // a 32-bit right shift leaves only the sign bits
      const s32 want = sa == 32 ? (w[i] & 0x80000000u ? -1 : 0) : (s32(w[i]) >> sa);
      EXPECT_EQ(got[i], want) << "pw_sra " << sa << " lane " << i;
    }

    memset(mem.at(kOut), 0, 16);
    run_vec(tester, mem, [&](CodeTester& t) {
      t.emit(IGen::pw_srl(t.generator(), Register(V2), Register(V0), u8(sa)));
    });
    u32 gotu[4];
    memcpy(gotu, mem.at(kOut), 16);
    for (int i = 0; i < 4; i++) {
      EXPECT_EQ(gotu[i], sa == 32 ? 0u : w[i] >> sa) << "pw_srl " << sa << " lane " << i;
    }
  }

  for (int sa = 0; sa < 32; sa++) {
    memset(mem.at(kOut), 0, 16);
    run_vec(tester, mem, [&](CodeTester& t) {
      t.emit(IGen::pw_sll(t.generator(), Register(V2), Register(V0), u8(sa)));
    });
    u32 gotu[4];
    memcpy(gotu, mem.at(kOut), 16);
    for (int i = 0; i < 4; i++) {
      EXPECT_EQ(gotu[i], u32(w[i] << sa)) << "pw_sll " << sa << " lane " << i;
    }
  }

  const u16 h[8] = {0x8000, 0x1234, 0xffff, 0x0001, 0x00ff, 0xff00, 0x5555, 0xaaaa};
  memcpy(mem.at(kA), h, 16);

  for (int sa = 1; sa <= 16; sa++) {
    memset(mem.at(kOut), 0, 16);
    run_vec(tester, mem, [&](CodeTester& t) {
      t.emit(IGen::ph_srl(t.generator(), Register(V2), Register(V0), u8(sa)));
    });
    u16 got[8];
    memcpy(got, mem.at(kOut), 16);
    for (int i = 0; i < 8; i++) {
      EXPECT_EQ(got[i], sa == 16 ? 0 : u16(h[i] >> sa)) << "ph_srl " << sa << " lane " << i;
    }
  }

  for (int sa = 0; sa < 16; sa++) {
    memset(mem.at(kOut), 0, 16);
    run_vec(tester, mem, [&](CodeTester& t) {
      t.emit(IGen::ph_sll(t.generator(), Register(V2), Register(V0), u8(sa)));
    });
    u16 got[8];
    memcpy(got, mem.at(kOut), 16);
    for (int i = 0; i < 8; i++) {
      EXPECT_EQ(got[i], u16(h[i] << sa)) << "ph_sll " << sa << " lane " << i;
    }
  }
  tester.clear();
}

#endif  // __aarch64__

#ifdef __aarch64__  // runs the code it emits
TEST(ARM64Alu, byte_shifts) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(1024);
  VecMem mem;

  u8 a[16];
  for (int i = 0; i < 16; i++) {
    a[i] = u8(0x10 + i);
  }
  memcpy(mem.at(kA), a, 16);

  for (int sh = 0; sh < 16; sh++) {
    memset(mem.at(kOut), 0xcc, 16);
    run_vec(tester, mem, [&](CodeTester& t) {
      t.emit(IGen::vpsrldq(t.generator(), Register(V2), Register(V0), u8(sh)));
    });
    u8 got[16];
    memcpy(got, mem.at(kOut), 16);
    for (int i = 0; i < 16; i++) {
      EXPECT_EQ(got[i], (i + sh < 16) ? a[i + sh] : 0) << "vpsrldq " << sh << " byte " << i;
    }

    memset(mem.at(kOut), 0xcc, 16);
    run_vec(tester, mem, [&](CodeTester& t) {
      t.emit(IGen::vpslldq(t.generator(), Register(V2), Register(V0), u8(sh)));
    });
    memcpy(got, mem.at(kOut), 16);
    for (int i = 0; i < 16; i++) {
      EXPECT_EQ(got[i], (i >= sh) ? a[i - sh] : 0) << "vpslldq " << sh << " byte " << i;
    }
  }
  tester.clear();
}

TEST(ARM64Alu, byte_and_halfword_compares) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(1024);
  VecMem mem;

  const u8 a[16] = {0, 1, 2, 3, 0x7f, 0x80, 0xff, 5, 9, 9, 0, 0, 0x81, 0x7f, 3, 3};
  const u8 b[16] = {0, 2, 2, 4, 0x7f, 0x7f, 0xfe, 6, 9, 8, 1, 0, 0x80, 0x80, 3, 4};
  memcpy(mem.at(kA), a, 16);
  memcpy(mem.at(kB), b, 16);

  memset(mem.at(kOut), 0, 16);
  run_vec(tester, mem, [](CodeTester& t) {
    t.emit(IGen::parallel_compare_e_b(t.generator(), Register(V2), Register(V0), Register(V1)));
  });
  u8 gb[16];
  memcpy(gb, mem.at(kOut), 16);
  for (int i = 0; i < 16; i++) {
    EXPECT_EQ(gb[i], a[i] == b[i] ? 0xff : 0x00) << "cmpeq b byte " << i;
  }

  memset(mem.at(kOut), 0, 16);
  run_vec(tester, mem, [](CodeTester& t) {
    t.emit(IGen::parallel_compare_gt_b(t.generator(), Register(V2), Register(V0), Register(V1)));
  });
  memcpy(gb, mem.at(kOut), 16);
  for (int i = 0; i < 16; i++) {
    EXPECT_EQ(gb[i], (s8)a[i] > (s8)b[i] ? 0xff : 0x00) << "cmpgt b byte " << i;
  }

  u16 ah[8], bh[8];
  memcpy(ah, a, 16);
  memcpy(bh, b, 16);
  memset(mem.at(kOut), 0, 16);
  run_vec(tester, mem, [](CodeTester& t) {
    t.emit(IGen::parallel_compare_e_h(t.generator(), Register(V2), Register(V0), Register(V1)));
  });
  u16 gh[8];
  memcpy(gh, mem.at(kOut), 16);
  for (int i = 0; i < 8; i++) {
    EXPECT_EQ(gh[i], ah[i] == bh[i] ? 0xffff : 0x0000) << "cmpeq h lane " << i;
  }

  memset(mem.at(kOut), 0, 16);
  run_vec(tester, mem, [](CodeTester& t) {
    t.emit(IGen::parallel_compare_gt_h(t.generator(), Register(V2), Register(V0), Register(V1)));
  });
  memcpy(gh, mem.at(kOut), 16);
  for (int i = 0; i < 8; i++) {
    EXPECT_EQ(gh[i], (s16)ah[i] > (s16)bh[i] ? 0xffff : 0x0000) << "cmpgt h lane " << i;
  }
  tester.clear();
}

TEST(ARM64Alu, doubleword_copies) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(1024);
  VecMem mem;

  const u64 a[2] = {0x1111111100000000ull, 0x3333333322222222ull};
  const u64 b[2] = {0xaaaaaaaa99999999ull, 0xccccccccbbbbbbbbull};
  memcpy(mem.at(kA), a, 16);
  memcpy(mem.at(kB), b, 16);

  memset(mem.at(kOut), 0, 16);
  run_vec(tester, mem, [](CodeTester& t) {
    t.emit(IGen::pcpyld_swapped(t.generator(), Register(V2), Register(V0), Register(V1)));
  });
  u64 got[2];
  memcpy(got, mem.at(kOut), 16);
  EXPECT_EQ(got[0], a[0]) << "pcpyld_swapped low";
  EXPECT_EQ(got[1], b[0]) << "pcpyld_swapped high";

  memset(mem.at(kOut), 0, 16);
  run_vec(tester, mem, [](CodeTester& t) {
    t.emit(IGen::pcpyud(t.generator(), Register(V2), Register(V0), Register(V1)));
  });
  memcpy(got, mem.at(kOut), 16);
  EXPECT_EQ(got[0], a[1]) << "pcpyud low";
  EXPECT_EQ(got[1], b[1]) << "pcpyud high";
  tester.clear();
}

TEST(ARM64Alu, packuswb) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(1024);
  VecMem mem;

  const s16 a[8] = {0, 1, 127, 128, 255, 256, -1, -32768};
  const s16 b[8] = {32767, -2, 300, 5, 0, 254, 255, 256};
  memcpy(mem.at(kA), a, 16);
  memcpy(mem.at(kB), b, 16);

  memset(mem.at(kOut), 0, 16);
  run_vec(tester, mem, [](CodeTester& t) {
    t.emit(IGen::vpackuswb(t.generator(), Register(V2), Register(V0), Register(V1)));
  });
  u8 got[16];
  memcpy(got, mem.at(kOut), 16);
  auto sat = [](s16 v) -> u8 { return v < 0 ? 0 : (v > 255 ? 255 : u8(v)); };
  for (int i = 0; i < 8; i++) {
    EXPECT_EQ(got[i], sat(a[i])) << "packuswb low byte " << i;
    EXPECT_EQ(got[8 + i], sat(b[i])) << "packuswb high byte " << i;
  }
  tester.clear();
}

TEST(ARM64Alu, blend_and_move) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(1024);
  VecMem mem;

  const u32 a[4] = {0xa0a0a0a0u, 0xa1a1a1a1u, 0xa2a2a2a2u, 0xa3a3a3a3u};
  const u32 b[4] = {0xb0b0b0b0u, 0xb1b1b1b1u, 0xb2b2b2b2u, 0xb3b3b3b3u};
  memcpy(mem.at(kA), a, 16);
  memcpy(mem.at(kB), b, 16);

  for (u8 mask = 0; mask < 16; mask++) {
    memset(mem.at(kOut), 0, 16);
    run_vec(tester, mem, [&](CodeTester& t) {
      t.emit(IGen::blend_vf(t.generator(), Register(V2), Register(V0), Register(V1), mask));
    });
    u32 got[4];
    memcpy(got, mem.at(kOut), 16);
    for (int i = 0; i < 4; i++) {
      EXPECT_EQ(got[i], (mask & (1 << i)) ? b[i] : a[i])
          << "blend mask " << int(mask) << " lane " << i;
    }
  }

  memset(mem.at(kOut), 0, 16);
  run_vec(tester, mem, [](CodeTester& t) {
    t.emit(IGen::mov_vf_vf(t.generator(), Register(V2), Register(V0)));
  });
  u32 got[4];
  memcpy(got, mem.at(kOut), 16);
  for (int i = 0; i < 4; i++) {
    EXPECT_EQ(got[i], a[i]) << "mov_vf_vf lane " << i;
  }
  tester.clear();
}

TEST(ARM64Alu, halfword_shuffles) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(1024);
  VecMem mem;

  u16 a[8];
  for (int i = 0; i < 8; i++) {
    a[i] = u16(0x1100 + i);
  }
  memcpy(mem.at(kA), a, 16);

  for (int imm = 0; imm < 256; imm++) {
    const u8 s0 = imm & 3, s1 = (imm >> 2) & 3, s2 = (imm >> 4) & 3, s3 = (imm >> 6) & 3;

    // shuffle the low half and keep the high half
    memset(mem.at(kOut), 0, 16);
    run_vec(tester, mem, [&](CodeTester& t) {
      t.emit(IGen::vpshuflw(t.generator(), Register(V2), Register(V0), u8(imm)));
    });
    u16 got[8];
    memcpy(got, mem.at(kOut), 16);
    EXPECT_EQ(got[0], a[s0]) << "pshuflw 0x" << std::hex << imm << " lane 0";
    EXPECT_EQ(got[1], a[s1]) << "pshuflw 0x" << std::hex << imm << " lane 1";
    EXPECT_EQ(got[2], a[s2]) << "pshuflw 0x" << std::hex << imm << " lane 2";
    EXPECT_EQ(got[3], a[s3]) << "pshuflw 0x" << std::hex << imm << " lane 3";
    for (int i = 4; i < 8; i++) {
      EXPECT_EQ(got[i], a[i]) << "pshuflw 0x" << std::hex << imm << " kept lane " << i;
    }

    // shuffle the high half and keep the low half
    memset(mem.at(kOut), 0, 16);
    run_vec(tester, mem, [&](CodeTester& t) {
      t.emit(IGen::vpshufhw(t.generator(), Register(V2), Register(V0), u8(imm)));
    });
    memcpy(got, mem.at(kOut), 16);
    for (int i = 0; i < 4; i++) {
      EXPECT_EQ(got[i], a[i]) << "pshufhw 0x" << std::hex << imm << " kept lane " << i;
    }
    EXPECT_EQ(got[4], a[4 + s0]) << "pshufhw 0x" << std::hex << imm << " lane 4";
    EXPECT_EQ(got[5], a[4 + s1]) << "pshufhw 0x" << std::hex << imm << " lane 5";
    EXPECT_EQ(got[6], a[4 + s2]) << "pshufhw 0x" << std::hex << imm << " lane 6";
    EXPECT_EQ(got[7], a[4 + s3]) << "pshufhw 0x" << std::hex << imm << " lane 7";
  }
  tester.clear();
}

TEST(ARM64Alu, movq) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(1024);
  VecMem mem;

  const u64 a[2] = {0x0123456789abcdefull, 0xfedcba9876543210ull};
  memcpy(mem.at(kA), a, 16);
  memset(mem.at(kOut), 0xcc, 16);

  run_vec(tester, mem, [](CodeTester& t) {
    t.emit(IGen::movq_gpr64_f64(t.generator(), Register(X4), Register(V0)));
    t.emit(IGen::movq_f64_gpr64(t.generator(), Register(V2), Register(X4)));
  });

  u64 got[2];
  memcpy(got, mem.at(kOut), 16);
  EXPECT_EQ(got[0], a[0]) << "movq low half";
  // FMOV Dd, Xn clears the high 64 bits like x86 movq
  EXPECT_EQ(got[1], 0ull) << "movq high half is not zero";
  tester.clear();
}

#endif  // __aarch64__

#ifdef __aarch64__  // runs the code it emits
TEST(ARM64Alu, gpr_immediate_arithmetic) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(8192);

  // immediate boundaries and the multi-instruction path
  const s64 imms[] = {0,         1,         2,         4094,     4095,     4096,
                      4097,      8191,      8192,      0xfff000, 0xfff001, 0xffffff,
                      0x1000000, 0x1000001, 0x1001000, 123456,   1048576};
  const u64 bases[] = {0ull, 1ull, 0x1234ull, 0xdeadbeefull, 0xffffffffffffffffull};

  for (u64 base : bases) {
    for (s64 imm : imms) {
      tester.clear();
      tester.emit_push_all_gprs(true);
      tester.emit(IGen::add_gpr64_imm(tester.generator(), Register(X0), imm));
      tester.emit_pop_all_gprs(true);
      tester.emit_return();
      EXPECT_EQ(tester.execute_ret<u64>(base, 0, 0, 0), u64(base + u64(imm)))
          << "add " << std::hex << base << " + " << imm;

      tester.clear();
      tester.emit_push_all_gprs(true);
      tester.emit(IGen::sub_gpr64_imm(tester.generator(), Register(X0), imm));
      tester.emit_pop_all_gprs(true);
      tester.emit_return();
      EXPECT_EQ(tester.execute_ret<u64>(base, 0, 0, 0), u64(base - u64(imm)))
          << "sub " << std::hex << base << " - " << imm;
    }
  }

  // negative immediates use the opposite operation
  for (u64 base : bases) {
    tester.clear();
    tester.emit_push_all_gprs(true);
    tester.emit(IGen::add_gpr64_imm(tester.generator(), Register(X0), -4097));
    tester.emit_pop_all_gprs(true);
    tester.emit_return();
    EXPECT_EQ(tester.execute_ret<u64>(base, 0, 0, 0), u64(base - 4097ull))
        << "add negative " << std::hex << base;
  }
  tester.clear();
}
#endif  // __aarch64__

#ifdef __aarch64__  // runs the code it emits
TEST(ARM64Alu, scalar_float_min_max) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(1024);

  // mixed signs catch absolute value comparisons
  const float pairs[][2] = {{3.0f, 5.0f},   {5.0f, 3.0f},   {-5.0f, 3.0f}, {3.0f, -5.0f},
                            {-5.0f, -3.0f}, {-3.0f, -5.0f}, {0.0f, 0.0f},  {1.5f, 1.5f},
                            {-0.5f, 0.25f}, {1e9f, -1e9f}};

  for (auto& p : pairs) {
    u32 ab, bb;
    memcpy(&ab, &p[0], 4);
    memcpy(&bb, &p[1], 4);

    tester.clear();
    tester.emit_push_all_gprs(true);
    tester.emit(IGen::movd_f32_gpr32(tester.generator(), Register(V0), Register(X0)));
    tester.emit(IGen::movd_f32_gpr32(tester.generator(), Register(V1), Register(X1)));
    tester.emit(IGen::max_f32_f32(tester.generator(), Register(V0), Register(V1)));
    tester.emit(IGen::movd_gpr32_f32(tester.generator(), Register(X0), Register(V0)));
    tester.emit_pop_all_gprs(true);
    tester.emit_return();
    u32 got = (u32)tester.execute_ret<u64>(ab, bb, 0, 0);
    float gotf;
    memcpy(&gotf, &got, 4);
    EXPECT_EQ(gotf, std::max(p[0], p[1])) << "max " << p[0] << ", " << p[1];

    tester.clear();
    tester.emit_push_all_gprs(true);
    tester.emit(IGen::movd_f32_gpr32(tester.generator(), Register(V0), Register(X0)));
    tester.emit(IGen::movd_f32_gpr32(tester.generator(), Register(V1), Register(X1)));
    tester.emit(IGen::min_f32_f32(tester.generator(), Register(V0), Register(V1)));
    tester.emit(IGen::movd_gpr32_f32(tester.generator(), Register(X0), Register(V0)));
    tester.emit_pop_all_gprs(true);
    tester.emit_return();
    got = (u32)tester.execute_ret<u64>(ab, bb, 0, 0);
    memcpy(&gotf, &got, 4);
    EXPECT_EQ(gotf, std::min(p[0], p[1])) << "min " << p[0] << ", " << p[1];
  }
  tester.clear();
}
#endif  // __aarch64__

#ifdef __aarch64__  // runs the code it emits
TEST(ARM64Alu, gpr64_displaced_ops) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(8192);
  VecMem mem;

  // distinct values catch a bad displacement
  for (int i = 0; i < 16; i++) {
    u64 v = 0xAA00000000000000ull + u64(i);
    memcpy(mem.at(kGoalPtr + i * 8), &v, 8);
  }
  const u64 base = (u64)mem.base() + kGoalPtr + 8 * 8;  // centered so both offset signs fit

  for (int slot = -8; slot <= 7; slot++) {
    const s32 off = slot * 8;

    tester.clear();
    tester.emit_push_all_gprs(true);
    tester.emit(IGen::load64_gpr64_plus_s32(tester.generator(), Register(X0), off, Register(X1)));
    tester.emit_pop_all_gprs(true);
    tester.emit_return();
    u64 want = 0xAA00000000000000ull + u64(8 + slot);
    EXPECT_EQ(tester.execute_ret<u64>(0, base, 0, 0), want) << "load64 offset " << off;

    tester.clear();
    tester.emit_push_all_gprs(true);
    tester.emit(IGen::lea_reg_plus_off32(tester.generator(), Register(X0), Register(X1), off));
    tester.emit_pop_all_gprs(true);
    tester.emit_return();
    EXPECT_EQ(tester.execute_ret<u64>(0, base, 0, 0), base + u64(s64(off)))
        << "lea32 offset " << off;

    if (off >= INT8_MIN && off <= INT8_MAX) {
      tester.clear();
      tester.emit_push_all_gprs(true);
      tester.emit(IGen::lea_reg_plus_off8(tester.generator(), Register(X0), Register(X1), off));
      tester.emit_pop_all_gprs(true);
      tester.emit_return();
      EXPECT_EQ(tester.execute_ret<u64>(0, base, 0, 0), base + u64(s64(off)))
          << "lea8 offset " << off;
    }

    const u64 marker = 0x5555000000000000ull + u64(slot + 8);
    tester.clear();
    tester.emit_push_all_gprs(true);
    tester.emit(IGen::store64_gpr64_plus_s32(tester.generator(), Register(X1), off, Register(X2)));
    tester.emit_pop_all_gprs(true);
    tester.emit_return();
    tester.execute_ret<u64>(0, base, marker, 0);
    u64 got;
    memcpy(&got, mem.at(kGoalPtr + (8 + slot) * 8), 8);
    EXPECT_EQ(got, marker) << "store64 offset " << off;
  }
  tester.clear();
}
#endif  // __aarch64__
