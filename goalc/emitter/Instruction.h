#pragma once

#include "common/common_types.h"

namespace emitter {
/*!
 * A high-level description of a opcode.  It can emit itself.
 */
struct Instruction {
  /*!
   * Emit into a buffer and return how many bytes written (can be zero)
   */
  virtual u8 emit(u8* buffer) const = 0;
  virtual u8 length() const = 0;
};
}  // namespace emitter
