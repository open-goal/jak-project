#pragma once

#include <string>
#include <vector>
#include "common/util/assert.h"
#include <common/goos/Object.h>
#include "decompiler/util/TP_Type.h"
#include "decompiler/util/StackSpillMap.h"
#include "decompiler/Disasm/Register.h"
#include "decompiler/IR2/IR2_common.h"
#include "decompiler/analysis/reg_usage.h"
#include "decompiler/config.h"

namespace decompiler {
class LinkedObjectFile;
class Form;
class DecompilerTypeSystem;
struct FunctionAtomicOps;

struct VariableWithCast {
  std::string name;
  std::optional<TypeSpec> cast;
};

struct StackVarEntry {
  StackVariableHint hint;
  TypeSpec ref_type;  // the actual type of the address.
  int size = -1;
};

struct StackSpillEntry {
  TP_Type tp_type;
  TypeSpec typespec;
  int offset;
  std::optional<std::string> name_override;
  std::string name() const {
    if (name_override) {
      return *name_override;
    } else {
      return fmt::format("sv-{}", offset);
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
  bool types_succeeded = false;
  bool has_local_vars() const { return m_has_local_vars; }
  bool has_type_analysis() const { return m_has_types; }
  bool has_reg_use() const { return m_has_reg_use; }
  const RegUsageInfo& reg_use() const {
    assert(m_has_reg_use);
    return m_reg_use;
  }

  void set_reg_use(const RegUsageInfo& info) {
    m_reg_use = info;
    m_has_reg_use = true;
  }

  RegUsageInfo& reg_use() {
    assert(m_has_reg_use);
    return m_reg_use;
  }

  // TODO - remove this.
  goos::Object get_variable_name_with_cast(Register reg, int atomic_idx, AccessMode mode) const;
  goos::Object get_variable_name_with_cast(const RegisterAccess& access) const;
  std::string get_variable_name(const RegisterAccess& access) const;
  VariableWithCast get_variable_and_cast(const RegisterAccess& access) const;
  std::optional<TypeSpec> get_user_cast_for_access(const RegisterAccess& access) const;
  TypeSpec get_variable_type(const RegisterAccess& access, bool using_user_var_types) const;

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

  const TypeState& get_types_for_op_mode(int atomic_op_id, AccessMode mode) const {
    if (mode == AccessMode::READ) {
      return get_types_before_op(atomic_op_id);
    } else {
      return get_types_after_op(atomic_op_id);
    }
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
                 const FunctionAtomicOps& atomic_ops,
                 const TypeSpec& my_type);

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
  void set_type_casts(const std::unordered_map<int, std::vector<TypeCast>>& casts) {
    m_typecasts = casts;
  }

  const std::unordered_map<int, std::vector<TypeCast>>& casts() const { return m_typecasts; }

  void set_remap_for_function(int nargs);
  void set_remap_for_method(int nargs);
  void set_remap_for_new_method(int nargs);
  void map_args_from_config(const std::vector<std::string>& args_names,
                            const std::unordered_map<std::string, std::string>& var_names);
  void map_args_from_config(const std::vector<std::string>& args_names,
                            const std::unordered_map<std::string, LocalVarOverride>& var_overrides);

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

  void set_stack_var_hints(const std::vector<StackVariableHint>& hints);
  const std::vector<StackVarEntry>& stack_var_hints() const { return m_stack_vars; }

  const UseDefInfo& get_use_def_info(const RegisterAccess& ra) const;
  void disable_use(const RegisterAccess& access) {
    if (has_local_vars()) {
      m_var_names.disable_use(access);
    }
  }

  void disable_def(const RegisterAccess& access);

  void set_defined_in_let(const std::string& var) { m_vars_defined_in_let.insert(var); }

  void set_retype_map(const std::unordered_map<std::string, TypeSpec>& map) { m_var_retype = map; }

  void set_stack_spills(const StackSpillMap& map) { m_stack_spill_map = map; }
  const StackSpillMap& stack_spills() const { return m_stack_spill_map; }

  // todo - remove these hacks at some point.
  LinkedObjectFile* file = nullptr;
  DecompilerTypeSystem* dts = nullptr;
  std::unordered_map<int, StackSpillEntry> stack_slot_entries;

  std::string get_spill_slot_var_name(int offset) const {
    auto kv = stack_slot_entries.find(offset);
    if (kv == stack_slot_entries.end()) {
      return fmt::format("sv-{}", offset);
    } else {
      return kv->second.name();
    }
  }

  const std::unordered_map<std::string, std::string>& var_remap_map() const { return m_var_remap; }

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

  std::unordered_map<int, std::vector<TypeCast>> m_typecasts;
  std::vector<StackVarEntry> m_stack_vars;
  std::unordered_map<std::string, std::string> m_var_remap;
  std::unordered_map<std::string, TypeSpec> m_var_retype;
  std::unordered_map<std::string, LabelType> m_label_types;

  std::unordered_set<std::string> m_vars_defined_in_let;
  std::optional<TypeSpec> m_type_analysis_return_type;

  StackSpillMap m_stack_spill_map;
};
}  // namespace decompiler