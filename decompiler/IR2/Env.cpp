#include <stdexcept>
#include "Env.h"

namespace decompiler {
std::string Env::get_variable_name(Register reg, int atomic_idx) const {
  (void)reg;
  (void)atomic_idx;
  throw std::runtime_error("Env::get_variable_name not yet implemented.");
}

/*!
 * Update the Env with the result of the type analysis pass.
 */
void Env::set_types(const std::vector<TypeState>& block_init_types,
                    const std::vector<TypeState>& op_end_types) {
  m_block_init_types = block_init_types;
  m_op_end_types = op_end_types;
  m_has_types = true;
}
}  // namespace decompiler