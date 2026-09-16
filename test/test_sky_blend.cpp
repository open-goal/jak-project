// the blend maths, checked against a scalar reference. `out` starts filled with the wrong
// answer, so a function that writes nothing fails instead of quietly giving a black sky.

#include <cstring>
#include <vector>

#include "common/common_types.h"

#include "game/graphics/opengl_renderer/SkyBlendCPU.h"
#include "gtest/gtest.h"

namespace {
// what the x86 SSE/AVX2 paths compute, spelled out
u8 ref_initial(u8 in, u8 intensity) {
  u32 v = ((u32)in * intensity) >> 7;
  return v > 255 ? 255 : (u8)v;
}
u8 ref_blend(u8 out, u8 in, u8 intensity) {
  u32 v = ((u32)in * intensity) >> 7;
  if (v > 255) {
    v = 255;
  }
  u32 sum = out + v;
  return sum > 255 ? 255 : (u8)sum;
}
}  // namespace

TEST(SkyBlend, InitialMatchesReferenceAndActuallyWrites) {
  constexpr u32 kSize = 4096;  // the 32x32 sky buffer, 4 bytes per texel
  std::vector<u8> in(kSize), out(kSize);
  u32 st = 12345;
  for (u32 i = 0; i < kSize; i++) {
    st = st * 1664525u + 1013904223u;
    in[i] = u8(st >> 24);
  }

  for (int intensity : {0, 1, 64, 127, 128, 200, 255}) {
    // fill the destination with the wrong answer so a function that writes nothing fails
    std::memset(out.data(), 0xAB, kSize);
    blend_sky_initial_fast(u8(intensity), out.data(), in.data(), kSize);

    bool wrote_something = false;
    for (u32 i = 0; i < kSize; i++) {
      ASSERT_EQ(out[i], ref_initial(in[i], u8(intensity)))
          << "intensity " << intensity << " byte " << i;
      if (out[i] != 0xAB) {
        wrote_something = true;
      }
    }
    EXPECT_TRUE(wrote_something) << "blend_sky_initial_fast wrote nothing at intensity "
                                 << intensity << ". is it compiled out on this platform?";
  }
}

TEST(SkyBlend, BlendMatchesReferenceAndActuallyWrites) {
  constexpr u32 kSize = 16384;  // the 64x64 cloud buffer
  std::vector<u8> in(kSize), seed(kSize);
  u32 st = 999;
  for (u32 i = 0; i < kSize; i++) {
    st = st * 1664525u + 1013904223u;
    in[i] = u8(st >> 24);
    st = st * 1664525u + 1013904223u;
    seed[i] = u8(st >> 24);
  }

  for (int intensity : {0, 1, 64, 127, 128, 200, 255}) {
    auto out = seed;
    blend_sky_fast(u8(intensity), out.data(), in.data(), kSize);

    bool changed = false;
    for (u32 i = 0; i < kSize; i++) {
      ASSERT_EQ(out[i], ref_blend(seed[i], in[i], u8(intensity)))
          << "intensity " << intensity << " byte " << i;
      if (out[i] != seed[i]) {
        changed = true;
      }
    }
    if (intensity > 0) {
      EXPECT_TRUE(changed) << "blend_sky_fast changed nothing at intensity " << intensity
                           << ". is it compiled out on this platform?";
    }
  }
}

TEST(SkyBlend, SaturatesLikePackus) {
  // (255 * 255) >> 7 is 508, which has to clamp to 255 the way packus/vqmovn do
  std::vector<u8> in(16, 255), out(16, 0);
  blend_sky_initial_fast(255, out.data(), in.data(), 16);
  for (u8 v : out) {
    EXPECT_EQ(v, 255);
  }
  // and the add saturates too
  std::vector<u8> in2(16, 255), out2(16, 200);
  blend_sky_fast(255, out2.data(), in2.data(), 16);
  for (u8 v : out2) {
    EXPECT_EQ(v, 255);
  }
}
