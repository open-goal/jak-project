#pragma once
#include "common/common_types.h"

namespace decompiler {
enum class VariableMode : u8 {
  READ,  // represents value of the variable at the beginning of the instruction
  WRITE  // represents value of the variable at the end of the instruction
};
}
