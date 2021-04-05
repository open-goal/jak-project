#pragma once

#include <optional>

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