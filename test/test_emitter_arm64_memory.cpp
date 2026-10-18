#include <vector>

#include "common/common_types.h"

#include "goalc/emitter/CodeTester.h"
#include "goalc/emitter/IGen.h"
#include "goalc/emitter/Register.h"
#include "gtest/gtest.h"

using namespace emitter;

namespace {

// signed and scaled offset boundaries
const std::vector<int> kOffsets = {0,   1,   2,   4,    8,    10,    12, 14, 16,   32,
                                   100, 127, 128, 4095, 8192, 0x140, -4, -8, -100, -128};

// keep the GOAL pointer aligned and away from buffer boundaries
constexpr int kGoalPtr = 4096;
constexpr int kBufSize = 32768;

struct Mem {
  std::vector<u8> buf;
  Mem() : buf(kBufSize, 0) {}
  void fill_pattern() {
    for (int i = 0; i < kBufSize; i++) {
      buf[i] = u8((i * 7 + 11) & 0xff);
    }
  }
  u8* base() { return buf.data(); }
  u8* at(int goal_off) { return buf.data() + goal_off; }
};

}  // namespace

// x0 has the GOAL pointer, x1 has the offset base
#ifdef __aarch64__  // runs the code it emits
TEST(ARM64Memory, gpr_loads) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(512);
  Mem mem;
  mem.fill_pattern();

  struct {
    int size;
    bool sign_extend;
  } forms[] = {{1, false}, {1, true}, {2, false}, {2, true}, {4, false}, {4, true}, {8, false}};

  for (auto f : forms) {
    for (int off : kOffsets) {
      if (kGoalPtr + off < 0 || kGoalPtr + off + f.size > kBufSize) {
        continue;
      }
      tester.clear();
      tester.emit_push_all_gprs(true);
      tester.emit(IGen::load_goal_gpr(tester.generator(), Register(X0), Register(X0), Register(X1),
                                      off, f.size, f.sign_extend));
      tester.emit_pop_all_gprs(true);
      tester.emit_return();

      u64 got = tester.execute_ret<u64>(kGoalPtr, (u64)mem.base(), 0, 0);

      u64 want = 0;
      memcpy(&want, mem.at(kGoalPtr + off), f.size);
      if (f.sign_extend) {
        switch (f.size) {
          case 1:
            want = u64(s64(s8(want)));
            break;
          case 2:
            want = u64(s64(s16(want)));
            break;
          case 4:
            want = u64(s64(s32(want)));
            break;
          default:
            break;
        }
      }
      EXPECT_EQ(got, want) << "size " << f.size << (f.sign_extend ? " signed" : " unsigned")
                           << " offset " << off;
    }
  }
  tester.clear();
}
#endif  // __aarch64__

#ifdef __aarch64__  // runs the code it emits
TEST(ARM64Memory, gpr_stores) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(512);

  for (int size : {1, 2, 4, 8}) {
    for (int off : kOffsets) {
      if (kGoalPtr + off < 0 || kGoalPtr + off + size > kBufSize) {
        continue;
      }
      Mem mem;
      tester.clear();
      tester.emit_push_all_gprs(true);
      tester.emit(IGen::store_goal_gpr(tester.generator(), Register(X0), Register(X2), Register(X1),
                                       off, size));
      tester.emit_pop_all_gprs(true);
      tester.emit_return();

      const u64 value = 0x1122334455667788ull;
      tester.execute_ret<u64>(kGoalPtr, (u64)mem.base(), value, 0);

      u64 want = 0;
      memcpy(&want, &value, size);
      u64 got = 0;
      memcpy(&got, mem.at(kGoalPtr + off), size);
      EXPECT_EQ(got, want) << "size " << size << " offset " << off;

      int changed_lo = -1, changed_hi = -1;
      for (int i = 0; i < kBufSize; i++) {
        if (mem.buf[i]) {
          if (changed_lo < 0) {
            changed_lo = i;
          }
          changed_hi = i;
        }
      }
      if (changed_lo >= 0) {
        EXPECT_GE(changed_lo, kGoalPtr + off)
            << "size " << size << " offset " << off << " writes before the field";
        EXPECT_LT(changed_hi, kGoalPtr + off + size)
            << "size " << size << " offset " << off << " writes past the field";
      }
    }
  }
  tester.clear();
}
#endif  // __aarch64__

#ifdef __aarch64__  // runs the code it emits
TEST(ARM64Memory, f32_loads_and_stores) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(512);

  for (int off : kOffsets) {
    if (kGoalPtr + off < 0 || kGoalPtr + off + 4 > kBufSize) {
      continue;
    }

    {
      Mem mem;
      mem.fill_pattern();
      const float f = 1234.5f;
      memcpy(mem.at(kGoalPtr + off), &f, 4);

      tester.clear();
      tester.emit_push_all_gprs(true);
      tester.emit(IGen::load_goal_simd32(tester.generator(), Register(V0), Register(X0),
                                         Register(X1), off));
      tester.emit(IGen::movd_gpr32_f32(tester.generator(), Register(X0), Register(V0)));
      tester.emit_pop_all_gprs(true);
      tester.emit_return();

      u32 got = (u32)tester.execute_ret<u64>(kGoalPtr, (u64)mem.base(), 0, 0);
      u32 want;
      memcpy(&want, &f, 4);
      EXPECT_EQ(got, want) << "f32 load at offset " << off;
    }

    {
      Mem mem;
      const float f = -8765.25f;
      u32 bits;
      memcpy(&bits, &f, 4);

      tester.clear();
      tester.emit_push_all_gprs(true);
      tester.emit(IGen::movd_f32_gpr32(tester.generator(), Register(V0), Register(X2)));
      tester.emit(IGen::store_goal_simd32(tester.generator(), Register(X0), Register(V0),
                                          Register(X1), off));
      tester.emit_pop_all_gprs(true);
      tester.emit_return();

      tester.execute_ret<u64>(kGoalPtr, (u64)mem.base(), bits, 0);

      u32 got;
      memcpy(&got, mem.at(kGoalPtr + off), 4);
      EXPECT_EQ(got, bits) << "f32 store at offset " << off;

      for (int i = 0; i < kBufSize; i++) {
        if (i >= kGoalPtr + off && i < kGoalPtr + off + 4) {
          continue;
        }
        ASSERT_EQ(mem.buf[i], 0) << "f32 store at offset " << off << " also writes byte " << i;
      }
    }
  }
  tester.clear();
}
#endif  // __aarch64__

#ifdef __aarch64__  // runs the code it emits
TEST(ARM64Memory, vf_loads_and_stores) {
  CodeTester tester(InstructionSet::ARM64);
  tester.init_code_buffer(512);

  // keep 128-bit accesses aligned
  for (int off : kOffsets) {
    if (off % 16 != 0 || kGoalPtr + off < 0 || kGoalPtr + off + 16 > kBufSize) {
      continue;
    }

    // round trip through scratch memory
    Mem mem;
    mem.fill_pattern();
    const int scratch = 256;  // keep scratch away from the test fields
    memset(mem.at(scratch), 0, 16);

    tester.clear();
    tester.emit_push_all_gprs(true);
    tester.emit(
        IGen::load_goal_simd128(tester.generator(), Register(V0), Register(X0), Register(X1), off));
    tester.emit(
        IGen::store_goal_vf(tester.generator(), Register(X2), Register(V0), Register(X1), 0));
    tester.emit_pop_all_gprs(true);
    tester.emit_return();

    tester.execute_ret<u64>(kGoalPtr, (u64)mem.base(), scratch, 0);

    EXPECT_EQ(memcmp(mem.at(scratch), mem.at(kGoalPtr + off), 16), 0)
        << "vf load/store at offset " << off;
  }
  tester.clear();
}
#endif  // __aarch64__
