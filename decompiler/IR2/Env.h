#pragma once

#include <string>
#include <vector>
#include <cassert>
#include "decompiler/util/TP_Type.h"
#include "decompiler/Disasm/Register.h"
#include "decompiler/IR2/IR2_common.h"

namespace decompiler {
class LinkedObjectFile;

struct VariableNames {
  struct VarInfo {
    VarInfo() = default;
    std::string name() const { return fmt::format("{}-{}", reg.to_charp(), id); }
    TP_Type type;
    Register reg;
    int id = -1;
    bool initialized = false;
  };

  std::unordered_map<Register, std::vector<VariableNames::VarInfo>, Register::hash> read_vars,
      write_vars;
  std::unordered_map<Register, std::vector<int>, Register::hash> read_opid_to_varid,
      write_opid_to_varid;

  const VarInfo& lookup(Register reg, int op_id, VariableMode mode) const {
    if (mode == VariableMode::READ) {
      return read_vars.at(reg).at(read_opid_to_varid.at(reg).at(op_id));
    } else {
      return write_vars.at(reg).at(write_opid_to_varid.at(reg).at(op_id));
    }
  }
};

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
  std::string get_variable_name(Register reg, int atomic_idx, VariableMode mode) const;

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

  void set_local_vars(const VariableNames& names) {
    m_var_names = names;
    m_has_local_vars = true;
  }

  std::string print_local_var_types() const;

  LinkedObjectFile* file = nullptr;

 private:
  bool m_has_local_vars = false;
  bool m_has_types = false;
  std::vector<TypeState> m_block_init_types;
  std::vector<TypeState> m_op_end_types;
  VariableNames m_var_names;
};
}  // namespace decompiler