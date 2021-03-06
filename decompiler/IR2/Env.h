#pragma once

#include <string>
#include <vector>
#include <cassert>
#include <common/goos/Object.h>
#include "decompiler/util/TP_Type.h"
#include "decompiler/Disasm/Register.h"
#include "decompiler/IR2/IR2_common.h"
#include "decompiler/analysis/reg_usage.h"
#include "decompiler/config.h"

namespace decompiler {
class LinkedObjectFile;
class Form;
class DecompilerTypeSystem;
struct FunctionAtomicOps;

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
  bool has_reg_use() const { return m_has_reg_use; }

  void set_reg_use(const RegUsageInfo& info) {
    m_reg_use = info;
    m_has_reg_use = true;
  }

  const RegUsageInfo& reg_use() const {
    assert(m_has_reg_use);
    return m_reg_use;
  }

  RegUsageInfo& reg_use() {
    assert(m_has_reg_use);
    return m_reg_use;
  }

  // TODO - remove this.
  goos::Object get_variable_name(Register reg, int atomic_idx, AccessMode mode) const;
  std::string get_variable_name(const RegisterAccess& access) const;

  /*!
   * Get the types in registers _after_ the given operation has completed.
   */
  const TypeState& get_types_after_op(int atomic_op_id) const {
    assert(m_has_types);
    return m_op_end_types.at(atomic_op_id);
  }

  const TypeState& get_types_before_op(int atomic_op_id) const {
    assert(m_has_types);
    return *m_op_init_types.at(atomic_op_id);
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
                 const std::vector<TypeState>& op_end_types,
                 const FunctionAtomicOps& atomic_ops);

  void set_local_vars(const VariableNames& names) {
    m_var_names = names;
    m_has_local_vars = true;
  }

  void set_end_var(RegisterAccess var) { m_end_var = var; }
  const RegisterAccess& end_var() const { return m_end_var; }

  std::vector<VariableNames::VarInfo> extract_visible_variables(const Form* top_level_form) const;
  std::string print_local_var_types(const Form* top_level_form) const;
  goos::Object local_var_type_list(const Form* top_level_form,
                                   int nargs_to_ignore,
                                   int* count_out) const;

  std::unordered_set<RegId, RegId::hash> get_ssa_var(const RegAccessSet& vars) const;
  RegId get_program_var_id(const RegisterAccess& var) const;

  bool allow_sloppy_pair_typing() const { return m_allow_sloppy_pair_typing; }
  void set_sloppy_pair_typing() { m_allow_sloppy_pair_typing = true; }
  void set_type_hints(const std::unordered_map<int, std::vector<TypeHint>>& hints) {
    m_typehints = hints;
  }

  void set_remap_for_function(int nargs);
  void set_remap_for_method(int nargs);
  void set_remap_for_new_method(int nargs);
  void map_args_from_config(const std::vector<std::string>& args_names,
                            const std::unordered_map<std::string, std::string>& var_names);

  const std::string& remapped_name(const std::string& name) const;

  bool op_id_is_eliminated_coloring_move(int op_id) const {
    assert(has_local_vars());
    return m_var_names.eliminated_move_op_ids.find(op_id) !=
           m_var_names.eliminated_move_op_ids.end();
  }
  const std::unordered_map<std::string, LabelType>& label_types() const { return m_label_types; }

  void set_label_types(const std::unordered_map<std::string, LabelType>& types) {
    m_label_types = types;
  }

  const UseDefInfo& get_use_def_info(const RegisterAccess& ra) const;
  void disable_use(const RegisterAccess& access) {
    if (has_local_vars()) {
      m_var_names.disable_use(access);
    }
  }

  void disable_def(const RegisterAccess& access) {
    if (has_local_vars()) {
      m_var_names.disable_def(access);
    }
  }

  void set_defined_in_let(const std::string& var) { m_vars_defined_in_let.insert(var); }

  LinkedObjectFile* file = nullptr;
  DecompilerTypeSystem* dts = nullptr;

 private:
  RegisterAccess m_end_var;

  bool m_has_reg_use = false;
  RegUsageInfo m_reg_use;

  bool m_has_local_vars = false;
  VariableNames m_var_names;

  bool m_has_types = false;
  std::vector<TypeState> m_block_init_types;
  std::vector<TypeState> m_op_end_types;
  std::vector<TypeState*> m_op_init_types;

  bool m_allow_sloppy_pair_typing = false;

  std::unordered_map<int, std::vector<TypeHint>> m_typehints;
  std::unordered_map<std::string, std::string> m_var_remap;
  std::unordered_map<std::string, LabelType> m_label_types;

  std::unordered_set<std::string> m_vars_defined_in_let;
};
}  // namespace decompiler