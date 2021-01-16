#include <stdexcept>
#include "Env.h"

namespace decompiler {
std::string Env::get_variable_name(Register reg, int atomic_idx, bool is_read) const {
  if (is_read) {
    return m_read_vars.at(reg).at(atomic_idx);
  } else {
    return m_write_vars.at(reg).at(atomic_idx);
  }
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