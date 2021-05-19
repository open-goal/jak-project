#pragma once

#include <optional>
#include "common/util/assert.h"
#include "common/util/Range.h"
#include "common/common_types.h"

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
    assert(false);
  } else {
    return std::nullopt;
  }
}

bool integer_fits(s64 in, int size, bool is_signed);
u32 float_as_u32(float x);
