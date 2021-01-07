#include <stdexcept>
#include "Env.h"

namespace decompiler {
std::string Env::get_variable_name(Register reg, int atomic_idx) const {
  (void)reg;
  (void)atomic_idx;
  throw std::runtime_error("Env::get_variable_name not yet implemented.");
}
}  // namespace decompiler