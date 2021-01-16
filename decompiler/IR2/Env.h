#pragma once

#include <string>
#include <vector>
#include <cassert>
#include "decompiler/util/TP_Type.h"
#include "decompiler/Disasm/Register.h"

namespace decompiler {
class LinkedObjectFile;

/*!
 * An "environment" for a single function.
 * This contains data for an entire function, like which registers are live when, the types of
 * values in registers, and local variable names.  This does not actually store IR itself, just
 * shared data that all IR can look at.  The concept is somewhat similar to Env in the compiler.
 */
class Env {
 public:
  bool has_local_vars() const { return m_has_local_vars; }
  bool has_type_analysis() const { return m_has_types; }
  std::string get_variable_name(Register reg, int atomic_idx, bool is_read) const;

  /*!
   * Get the types in registers _after_ the given operation has completed.
   */
  const TypeState& get_types_after_op(int atomic_op_id) const {
    assert(m_has_types);
    return m_op_end_types.at(atomic_op_id);
  }

  /*!
   * Get the types in registers at the beginning of this basic block, before any operations
   * have occurred.
   */
  const TypeState& get_types_at_block_entry(int block_id) const {
    assert(m_has_types);
    return m_block_init_types.at(block_id);
  }

  void set_types(const std::vector<TypeState>& block_init_types,
                 const std::vector<TypeState>& op_end_types);

  void set_local_vars(
      const std::unordered_map<Register, std::vector<std::string>, Register::hash>& reads,
      const std::unordered_map<Register, std::vector<std::string>, Register::hash>& writes) {
    m_read_vars = reads;
    m_write_vars = writes;
    m_has_local_vars = true;
  }
  LinkedObjectFile* file = nullptr;

 private:
  bool m_has_local_vars = false;
  bool m_has_types = false;
  std::vector<TypeState> m_block_init_types;
  std::vector<TypeState> m_op_end_types;
  std::unordered_map<Register, std::vector<std::string>, Register::hash> m_read_vars;
  std::unordered_map<Register, std::vector<std::string>, Register::hash> m_write_vars;
};
}  // namespace decompiler