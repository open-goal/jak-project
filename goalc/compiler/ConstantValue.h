#pragma once

#include <string>

#include "common/common_types.h"
#include "common/util/Assert.h"
#include "common/util/BitUtils.h"

#include "third-party/fmt/core.h"

struct U128 {
  U128() = default;
  U128(u64 _lo, u64 _hi) : lo(_lo), hi(_hi) {}
  u64 lo = 0;
  u64 hi = 0;
};

class ConstantValue {
 public:
  ConstantValue(const void* data, int size) : m_size(size) {
    ASSERT(size == 8 || size == 16);
    memcpy(m_value, data, size);
  }

  std::string print() const {
    if (m_size == 8) {
      return std::to_string(value_64());
    } else {
      return fmt::format("0x{:08x}{:08x}", value_128_hi(), value_128_lo());
    }
  }

  s64 value_64() const {
    ASSERT(m_size == 8);
    s64 result;
    memcpy(&result, m_value, sizeof(s64));
    return result;
  }

  u64 value_128_lo() const {
    ASSERT(m_size == 16);
    s64 result;
    memcpy(&result, m_value, sizeof(s64));
    return result;
  }

  u64 value_128_hi() const {
    ASSERT(m_size == 16);
    s64 result;
    memcpy(&result, m_value + sizeof(s64), sizeof(s64));
    return result;
  }

  /*!
   * 8 or 16 byte constant?
   */
  int size() const { return m_size; }

  /*!
   * Okay to put this in a gpr?
   */
  bool uses_gpr() const { return m_size == 8; }

  /*!
   * Try to copy data to destination.
   */
  bool copy_to(void* destination, int size, bool is_signed) const {
    if (m_size == 8) {
      if (!integer_fits(value_64(), size, is_signed)) {
        return false;
      }
      memcpy(destination, m_value, size);
      return true;
    } else if (m_size == 16) {
      if (size != 16) {
        return false;
      }
      memcpy(destination, m_value, 16);
      return true;
    } else {
      ASSERT(false);
    }
    return false;
  }

  /*!
   * Is the value 0?
   */
  bool is_zero() const {
    if (m_size) {
      return value_64() == 0;
    } else {
      return (value_128_lo() == 0) && (value_128_hi() == 0);
    }
  }

 protected:
  u8 m_value[16] = {0};
  int m_size;
};
