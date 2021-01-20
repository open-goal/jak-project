#pragma once
#include "common/common_types.h"
#include "decompiler/Disasm/Register.h"

namespace decompiler {
enum class VariableMode : u8 {
  READ,  // represents value of the variable at the beginning of the instruction
  WRITE  // represents value of the variable at the end of the instruction
};

/*!
 * A register plus an integer ID.
 */
struct RegId {
  Register reg;
  int id = -1;
  struct hash {
    auto operator()(const RegId& x) const {
      return Register::hash()(x.reg) ^ std::hash<int>()(x.id);
    }
  };

  bool operator==(const RegId& other) const { return reg == other.reg && id == other.id; }
  bool operator!=(const RegId& other) const { return !((*this) == other); }
};
}  // namespace decompiler
