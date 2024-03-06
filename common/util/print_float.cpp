#include "print_float.h"

#include <cmath>

#include "common/goal_constants.h"
#include "common/util/Assert.h"

#include "fmt/core.h"
#include "third-party/dragonbox.h"

/*!
 * Convert a float to a string. The string is _always_ in this format:
 * [negative_sign] [at least 1 digit] [decimal point] [at least 1 digit]
 * and, if you trust the dragonbox library, should be the shortest possible representation
 * that round-trips through a properly implemented string -> float conversion.
 */
std::string float_to_string(float value, bool append_trailing_decimal) {
  constexpr int buff_size = 128;
  char buff[buff_size];
  float_to_cstr(value, buff, append_trailing_decimal);
  return {buff};
}

/*!
 * Wrapper around float_to_string, for printing meters. Unlike float_to_string, it does not append
 * decimals by default.
 */
std::string meters_to_string(float value, bool append_trailing_decimal) {
  return float_to_string(value / METER_LENGTH, append_trailing_decimal);
}

/*!
 * Wrapper around float_to_string, for printing degrees. Unlike float_to_string, it does not append
 * decimals by default.
 */
std::string degrees_to_string(float value, bool append_trailing_decimal) {
  return float_to_string(value / DEGREES_LENGTH, append_trailing_decimal);
}

/*!
 * Convert a fixed point value to a float. Fixed point values usually end up with strange numbers
 * that were definitely not what was written when we do a naive conversion. This function
 * is a bit more clever.
 */
float fixed_point_to_float(s64 value, s64 scale) {
  float sign = value < 0 ? -1 : 1;
  value = std::abs(value);
  double naive = (double)value / scale;

  double flt_scale = 1;
  while (flt_scale < scale) {
    // 1 -> 0.1
    flt_scale *= 10;
  }

  // we have a scale with enough precision for our fixed point. now find a good decimal.

  // start at something resonable.
  s64 fixed_start = (s64)(naive * flt_scale);
  fixed_start = fixed_start - (fixed_start % 5);
  // add e.g. 0.005 in a 1/1000 scale
  s64 fixed_add = 0;
  while ((s64)((fixed_start + fixed_add) / flt_scale * scale) < value) {
    fixed_add += 5;
  }
  if ((s64)((fixed_start + fixed_add) / flt_scale * scale) == value) {
    return (fixed_start + fixed_add) / flt_scale * sign;
  }

  // add e.g. 0.001 in a 1/1000 scale
  fixed_add = 0;
  while ((s64)((fixed_start + fixed_add) / flt_scale * scale) < value) {
    fixed_add += 1;
  }
  if ((s64)((fixed_start + fixed_add) / flt_scale * scale) == value) {
    return (fixed_start + fixed_add) / flt_scale * sign;
  }
  // added too much!
  ASSERT_MSG(false, fmt::format("fixed_point_to_float failed hard. v: {} s: {}", value, scale));
  return 0;
}

/*!
 * Convert a fixed point value to a string. Wrapper around fixed_point_to_string
 */
std::string fixed_point_to_string(s64 value, s64 scale, bool append_trailing_decimal) {
  return float_to_string(fixed_point_to_float(value, scale), append_trailing_decimal);
}

/*!
 * Wrapper around fixed_point_to_string, for printing seconds.
 */
std::string seconds_to_string(s64 value, bool append_trailing_decimal) {
  return fixed_point_to_string(value, TICKS_PER_SECOND, append_trailing_decimal);
}

int float_to_cstr(float value, char* buffer, bool append_trailing_decimal) {
  ASSERT(std::isfinite(value));
  // dragonbox gives us:
  //  - an integer, representing the decimal value
  //  - sign
  //  - exponent

  int i = 0;

  // the exponent/significand representation of dragonbox is ambiguous with how it represents 0,
  // so just handle that as a special case
  if (value == 0) {
    buffer[i++] = '0';
    if (append_trailing_decimal) {
      buffer[i++] = '.';
      buffer[i++] = '0';
    }
    buffer[i++] = '\0';
  }

  auto decimal = jkj::dragonbox::to_decimal(value);

  // in all cases, we need to convert the decimal to characters.
  char digit_buff[64];
  int num_digits = 0;
  u64 significand = decimal.significand;
  while (significand) {
    digit_buff[num_digits++] = '0' + (significand % 10);
    significand /= 10;
  }

  if (decimal.exponent >= 0) {
    // needs 0 or more trailing zeros before decimal (no nonzeros after decimal).

    // print in four parts:
    // negative sign | digits | 000's | .0

    // part 1
    if (decimal.is_negative) {
      buffer[i++] = '-';
    }

    // part 2
    for (int digit = 0; digit < num_digits; digit++) {
      buffer[i++] = digit_buff[num_digits - (digit + 1)];
    }

    // part 3
    for (int j = 0; j < decimal.exponent; j++) {
      buffer[i++] = '0';
    }

    // part 4
    if (append_trailing_decimal) {
      buffer[i++] = '.';
      buffer[i++] = '0';
    }
    buffer[i++] = '\0';
  } else {
    // some nonzero digits after decimal.

    if (num_digits <= -decimal.exponent) {
      // all after the decimal
      // negative sign | 0. | 000's | digits

      // part 1
      if (decimal.is_negative) {
        buffer[i++] = '-';
      }

      // part 2
      buffer[i++] = '0';
      buffer[i++] = '.';

      // part 3
      int zeros = -decimal.exponent - num_digits;
      for (int j = 0; j < zeros; j++) {
        buffer[i++] = '0';
      }

      // part 4
      for (int digit = 0; digit < num_digits; digit++) {
        buffer[i++] = digit_buff[num_digits - (digit + 1)];
      }
      buffer[i++] = '\0';

    } else {
      // some before, some after.
      // negative sign | digits | . | digits

      // part 1
      if (decimal.is_negative) {
        buffer[i++] = '-';
      }

      // part 2
      int digits_before_decimal = num_digits + decimal.exponent;
      int digit = 0;
      for (; digit < digits_before_decimal; digit++) {
        buffer[i++] = digit_buff[num_digits - (digit + 1)];
      }

      // part 3
      buffer[i++] = '.';

      // part 4
      for (; digit < num_digits; digit++) {
        buffer[i++] = digit_buff[num_digits - (digit + 1)];
      }
      buffer[i++] = '\0';
    }
  }
  return i;
}

bool proper_float(float value) {
  u32 int_value;
  memcpy(&int_value, &value, 4);
  u8 exp = (int_value >> 23) & 0xff;
  u32 mant = int_value & 0x7fffff;
  if ((exp == 0 && mant != 0) || exp == 0xff || !std::isfinite(value)) {
    return false;
  } else {
    return true;
  }
}
