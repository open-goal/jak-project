#pragma once

#include <optional>

#include "common/common_types.h"
#include "common/util/Assert.h"
#include "common/util/Range.h"

constexpr int BITS_PER_BYTE = 8;
template <typename T>
std::optional<Range<int>> get_bit_range(T value) {
  enum State { INITIAL_ZEROS, ONES, TRAILING_ZEROS } state = INITIAL_ZEROS;

  int start_bit = 0;
  int end_bit = 0;
  int max_bit = BITS_PER_BYTE * sizeof(T);

  for (int i = 0; i < max_bit; i++) {
    bool bit = (value) & (T(1) << i);
    if (state == INITIAL_ZEROS) {
      if (bit) {
        start_bit = i;
        state = ONES;
      }
    } else if (state == ONES) {
      if (!bit) {
        end_bit = i;
        state = TRAILING_ZEROS;
      }
    } else if (state == TRAILING_ZEROS) {
      if (bit) {
        return std::nullopt;
      }
    }
  }

  if (state == INITIAL_ZEROS) {
    return std::nullopt;
  } else if (state == ONES) {
    end_bit = max_bit;
  }

  return Range<int>{start_bit, end_bit};
}

/*!
 * Note: this only works on numbers that are greater than or equal to 0.
 */
template <typename T>
std::optional<int> get_power_of_two(T in) {
  if (in == 0) {
    return std::nullopt;
  }
  if ((in & (in - 1)) == 0) {
    for (int i = 0; i < 64; i++) {
      if (in == (T(1) << i)) {
        return i;
      }
    }
    ASSERT(false);
  } else {
    return std::nullopt;
  }
}

bool integer_fits(s64 in, int size, bool is_signed);
u32 float_as_u32(float x);

template <typename T>
T align64(T in) {
  return (in + 63) & (~T(63));
}

template <typename T>
T align32(T in) {
  return (in + 31) & (~T(31));
}

template <typename T>
T align16(T in) {
  return (in + 15) & (~T(15));
}

template <typename T>
T align8(T in) {
  return (in + 7) & (~T(7));
}

template <typename T>
T align4(T in) {
  return (in + 3) & (~T(3));
}

template <typename T>
T align2(T in) {
  return (in + 1) & (~T(1));
}

inline u32 count_leading_zeros_u32(u32 in) {
#if defined(__GNUC__) || defined(__clang__)
  return __builtin_clz(in);
#else
  unsigned long result;
  _BitScanReverse(&result, in);
  return result;
#endif
}
