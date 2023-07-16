#pragma once
#include <cfloat>

#ifdef __aarch64__
#include "third-party/sse2neon/sse2neon.h"
#else
#include <immintrin.h>
#endif

#include "common/common_types.h"
#include "common/math/Vector.h"
#include "common/util/Assert.h"

enum class Mask {
  NONE = 0,
  x = 1,
  y = 2,
  xy = 3,
  z = 4,
  xz = 5,
  yz = 6,
  xyz = 7,
  w = 8,
  xw = 9,
  yw = 10,
  xyw = 11,
  zw = 12,
  xzw = 13,
  yzw = 14,
  xyzw = 15
};

#ifdef __linux__
#define REALLY_INLINE __attribute__((always_inline))
#elif __APPLE__
#define REALLY_INLINE __attribute__((always_inline))
#else
#define REALLY_INLINE __forceinline
#endif

// note: must be aligned.
static inline REALLY_INLINE void copy_vector(void* dest, const void* src) {
  __m128 val = _mm_load_ps((const float*)src);
  _mm_store_ps((float*)dest, val);
}

inline float vu_max(float a, float b) {
  return std::max(b, a);
  //  s32 ai, bi;
  //  memcpy(&ai, &a, 4);
  //  memcpy(&bi, &b, 4);
  //  bool flip = ai < 0 && bi < 0;
  //  if (ai > bi) {
  //    return flip ? b : a;
  //  } else {
  //    return flip ? a : b;
  //  }
}

inline float vu_min(float a, float b) {
  s32 ai, bi;
  memcpy(&ai, &a, 4);
  memcpy(&bi, &b, 4);
  bool flip = ai < 0 && bi < 0;
  if (ai > bi) {
    return flip ? a : b;
  } else {
    return flip ? b : a;
  }
}

struct alignas(16) Vf {
  REALLY_INLINE __m128 load() const { return _mm_load_ps(data); }

  REALLY_INLINE void move_xyzw(const Vf& src) { copy_vector(data, src.data); }

  float data[4];
  float& x() { return data[0]; }
  float& y() { return data[1]; }
  float& z() { return data[2]; }
  float& w() { return data[3]; }

  const float& x() const { return data[0]; }
  const float& y() const { return data[1]; }
  const float& z() const { return data[2]; }
  const float& w() const { return data[3]; }

  std::string print() const { return fmt::format("{} {} {} {}", x(), y(), z(), w()); }

  float length_xyz() const { return std::sqrt(x() * x() + y() * y() + z() * z()); }

  std::string print_hex() const {
    return fmt::format("0x{:x} 0x{:x} 0x{:x} 0x{:x}", x_as_u32(), y_as_u32(), z_as_u32(),
                       w_as_u32());
  }

  void set_zero() {
    data[0] = 0;
    data[1] = 0;
    data[2] = 0;
    data[3] = 0;
  }

  void set_u32s(u32 xx, u32 yy, u32 zz, u32 ww) {
    memcpy(&data[0], &xx, 4);
    memcpy(&data[1], &yy, 4);
    memcpy(&data[2], &zz, 4);
    memcpy(&data[3], &ww, 4);
  }

  u16 x_as_u16() const {
    u16 result;
    memcpy(&result, &data[0], 2);
    return result;
  }

  u16 y_as_u16() const {
    u16 result;
    memcpy(&result, &data[1], 2);
    return result;
  }

  u16 z_as_u16() const {
    u16 result;
    memcpy(&result, &data[2], 2);
    return result;
  }

  u16 w_as_u16() const {
    u16 result;
    memcpy(&result, &data[3], 2);
    return result;
  }

  u32 x_as_u32() const {
    u32 result;
    memcpy(&result, &data[0], 4);
    return result;
  }

  u32 y_as_u32() const {
    u32 result;
    memcpy(&result, &data[1], 4);
    return result;
  }

  u32 z_as_u32() const {
    u32 result;
    memcpy(&result, &data[2], 4);
    return result;
  }

  u32 w_as_u32() const {
    u32 result;
    memcpy(&result, &data[3], 4);
    return result;
  }

  Vf() = default;
  Vf(const math::Vector4f& vec) { memcpy(data, vec.data(), 16); }
  Vf(float a, float b, float c, float d) {
    data[0] = a;
    data[1] = b;
    data[2] = c;
    data[3] = d;
  }

  std::string str_float() const { return fmt::format("{} {} {} {}", x(), y(), z(), w()); }

  float& operator[](int i) { return data[i]; }
  float operator[](int i) const { return data[i]; }

  void mr32(Mask mask, const Vf& other) {
    float temp[4];
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        temp[i] = other[(i + 1) % 4];
      }
    }

    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        data[i] = temp[i];
      }
    }
  }

  REALLY_INLINE void mr32_z(const Vf& other) { data[2] = other.data[3]; }

  void mfir(Mask mask, s16 in) {
    s32 sext = in;
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        memcpy(data + i, &sext, 4);
      }
    }
  }

  void maxi(Mask mask, const Vf& other, float I) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        data[i] = vu_max(other[i], I);
      }
    }
  }

  void max(Mask mask, const Vf& other, float I) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        data[i] = vu_max(other[i], I);
      }
    }
  }

  void max(Mask mask, const Vf& other, const Vf& b) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        data[i] = vu_max(other[i], b[i]);
      }
    }
  }

  REALLY_INLINE void max_xyzw(const Vf& a, const Vf& b) {
    _mm_store_ps(data, _mm_max_ps(_mm_load_ps(a.data), _mm_load_ps(b.data)));
  }

  REALLY_INLINE void max_xyzw(const Vf& a, float b) {
    _mm_store_ps(data, _mm_max_ps(_mm_load_ps(a.data), _mm_set1_ps(b)));
  }

  REALLY_INLINE void mini_xyzw(const Vf& a, const Vf& b) {
    _mm_store_ps(data, _mm_min_ps(_mm_load_ps(a.data), _mm_load_ps(b.data)));
  }

  REALLY_INLINE void mini_xyzw(const Vf& a, float b) {
    _mm_store_ps(data, _mm_min_ps(_mm_load_ps(a.data), _mm_set1_ps(b)));
  }

  void minii(Mask mask, const Vf& other, float I) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        data[i] = vu_min(other[i], I);
      }
    }
  }

  void mini(Mask mask, const Vf& other, float I) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        data[i] = vu_min(other[i], I);
      }
    }
  }

  void mini(Mask mask, const Vf& a, const Vf& b) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        data[i] = vu_min(a[i], b[i]);
      }
    }
  }

  void fill(float f) {
    for (auto& x : data) {
      x = f;
    }
  }

  void move(Mask mask, const Vf& other) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        data[i] = other[i];
      }
    }
  }

  void mfp(Mask mask, float other) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        data[i] = other;
      }
    }
  }

  void add(Mask mask, const Vf& a, const Vf& b) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        data[i] = a[i] + b[i];
      }
    }
  }

  REALLY_INLINE void add_xyzw(const Vf& a, const Vf& b) {
    _mm_store_ps(data, _mm_add_ps(_mm_load_ps(a.data), _mm_load_ps(b.data)));
  }

  void add(Mask mask, const Vf& a, float b) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        data[i] = a[i] + b;
      }
    }
  }

  u32 add_and_set_sf_s(Mask mask, const Vf& a, float b) {
    u32 flag = 0;
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        data[i] = a[i] + b;
        if (data[i] < 0) {
          flag = 0x2;
        }
      }
    }
    return flag;
  }

  u32 sub_and_set_sf_s(Mask mask, const Vf& a, float b) {
    u32 flag = 0;
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        data[i] = a[i] - b;
        if (data[i] < 0) {
          flag = 0x2;
        }
      }
    }
    return flag;
  }

  void sub(Mask mask, const Vf& a, float b) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        data[i] = a[i] - b;
      }
    }
  }

  void sub(Mask mask, const Vf& a, const Vf& b) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        data[i] = a[i] - b[i];
      }
    }
  }

  REALLY_INLINE void mul_xyzw(const Vf& a, const Vf& b) {
    _mm_store_ps(data, _mm_mul_ps(_mm_load_ps(a.data), _mm_load_ps(b.data)));
  }

  REALLY_INLINE void mul_xyzw(const Vf& a, float b) {
    _mm_store_ps(data, _mm_mul_ps(_mm_load_ps(a.data), _mm_set1_ps(b)));
  }

  void mul(Mask mask, const Vf& a, const Vf& b) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        data[i] = a[i] * b[i];
      }
    }
  }

  void saturate_infs() {
    for (auto& val : data) {
      if (std::isinf(val)) {
        if (val > 0) {
          val = FLT_MAX;
        } else {
          val = -FLT_MAX;
        }
      }
    }
  }

  void mul(Mask mask, const Vf& a, float b) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        data[i] = a[i] * b;
      }
    }
  }

  void itof0(Mask mask, const Vf& a) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        s32 val;
        memcpy(&val, &a.data[i], 4);
        data[i] = val;
      }
    }
  }

  void itof12(Mask mask, const Vf& a) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        s32 val;
        memcpy(&val, &a.data[i], 4);
        data[i] = ((float)val) * (1.f / 4096.f);
      }
    }
  }

  void itof15(Mask mask, const Vf& a) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        s32 val;
        memcpy(&val, &a.data[i], 4);
        data[i] = ((float)val) * (1.f / 32768.f);
      }
    }
  }

  void ftoi4(Mask mask, const Vf& a) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        s32 val = a.data[i] * 16.f;
        memcpy(&data[i], &val, 4);
      }
    }
  }

  void ftoi4_check(Mask /*mask*/, const Vf& a) {
    for (int i = 0; i < 3; i++) {
      data[i] = a.data[i];
    }

    for (int i = 3; i < 4; i++) {
      s32 val = a.data[i] * 16.f;
      memcpy(&data[i], &val, 4);
    }
  }

  void ftoi12(Mask mask, const Vf& a) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        //        if (std::isnan(a.data[i])) {
        //          ASSERT(false);
        //        }
        s32 val = a.data[i] * 4096.f;
        memcpy(&data[i], &val, 4);
      }
    }
  }

  void ftoi12_check(Mask mask, const Vf& a) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        if (std::isnan(a.data[i])) {
          ASSERT(false);
        }
        s32 val = a.data[i] * 4096.f;
        memcpy(&data[i], &val, 4);
      }
    }
  }

  void ftoi0(Mask mask, const Vf& a) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        s32 val = a.data[i];
        memcpy(&data[i], &val, 4);
      }
    }
  }
};

struct alignas(16) Accumulator {
  float data[4];

  std::string print() const {
    return fmt::format("{} {} {} {}", data[0], data[1], data[2], data[3]);
  }

  void adda(Mask mask, const Vf& a, float b) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        data[i] = a[i] + b;
      }
    }
  }

  void madda(Mask mask, const Vf& a, const Vf& b) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        data[i] += a[i] * b[i];
      }
    }
  }

  void madda(Mask mask, const Vf& a, float b) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        data[i] += a[i] * b;
      }
    }
  }

  REALLY_INLINE void madda_xyzw(const Vf& _a, float _b) {
    auto b = _mm_set1_ps(_b);
    auto a = _mm_load_ps(_a.data);
    auto acc = _mm_load_ps(data);
    _mm_store_ps(data, _mm_add_ps(_mm_mul_ps(a, b), acc));
  }

  REALLY_INLINE void madda_xyzw(const Vf& _a, const Vf& _b) {
    auto b = _mm_load_ps(_b.data);
    auto a = _mm_load_ps(_a.data);
    auto acc = _mm_load_ps(data);
    _mm_store_ps(data, _mm_add_ps(_mm_mul_ps(a, b), acc));
  }

  void madd(Mask mask, Vf& dest, const Vf& a, const Vf& b) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        dest[i] = data[i] + a[i] * b[i];
      }
    }
  }

  REALLY_INLINE void madd_xyzw(Vf& dest, const Vf& _a, float _b) {
    auto b = _mm_set1_ps(_b);
    auto a = _mm_load_ps(_a.data);
    auto acc = _mm_load_ps(data);
    _mm_store_ps(dest.data, _mm_add_ps(_mm_mul_ps(a, b), acc));
  }

  REALLY_INLINE void madd_xyz(Vf& dest, const Vf& _a, float _b) {
    auto b = _mm_set1_ps(_b);
    auto a = _mm_load_ps(_a.data);
    auto acc = _mm_load_ps(data);
    auto prod = _mm_add_ps(_mm_mul_ps(a, b), acc);
    prod = _mm_blend_ps(prod, _mm_load_ps(dest.data), 0b1000);
    _mm_store_ps(dest.data, prod);
  }

  void madd(Mask mask, Vf& dest, const Vf& a, float b) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        dest[i] = data[i] + a[i] * b;
      }
    }
  }

  void msub(Mask mask, Vf& dest, const Vf& a, float b) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        dest[i] = data[i] - a[i] * b;
      }
    }
  }

  void msuba(Mask mask, const Vf& a, float b) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        data[i] -= a[i] * b;
      }
    }
  }

  u16 madd_flag(Mask mask, Vf& dest, const Vf& a, float b) {
    u16 result = 0;
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        dest[i] = data[i] + a[i] * b;
        if (dest[i] < 0) {
          result |= (1 << (3 - i)) << 4;
        }
        if (dest[i] == 0) {
          result |= (1 << (3 - i));
        }
      }
    }
    return result;
  }

  void mula(Mask mask, const Vf& a, const Vf& b) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        data[i] = a[i] * b[i];
      }
    }
  }

  void mula(Mask mask, const Vf& a, float b) {
    for (int i = 0; i < 4; i++) {
      if ((u64)mask & (1 << i)) {
        data[i] = a[i] * b;
      }
    }
  }

  REALLY_INLINE void mula_xyzw(const Vf& _a, float _b) {
    auto b = _mm_set1_ps(_b);
    auto a = _mm_load_ps(_a.data);
    _mm_store_ps(data, _mm_mul_ps(a, b));
  }

  REALLY_INLINE void mula_xyzw(const Vf& _a, const Vf& _b) {
    auto b = _mm_load_ps(_b.data);
    auto a = _mm_load_ps(_a.data);
    _mm_store_ps(data, _mm_mul_ps(a, b));
  }

  void opmula(const Vf& a, const Vf& b) {
    data[0] = a.data[1] * b.data[2];
    data[1] = a.data[2] * b.data[0];
    data[2] = a.data[0] * b.data[1];
  }

  void opmsub(Vf& dst, Vf a, Vf b) {
    dst.data[0] = data[0] - a.data[1] * b.data[2];
    dst.data[1] = data[1] - a.data[2] * b.data[0];
    dst.data[2] = data[2] - a.data[0] * b.data[1];
  }
};
