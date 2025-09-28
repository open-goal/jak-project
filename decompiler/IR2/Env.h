#pragma once

#include <string>
#include <vector>

#include "common/goos/Object.h"
#include "common/util/Assert.h"

#include "decompiler/Disasm/Register.h"
#include "decompiler/IR2/IR2_common.h"
#include "decompiler/analysis/reg_usage.h"
#include "decompiler/config.h"
#include "decompiler/util/StackSpillMap.h"
#include "decompiler/util/TP_Type.h"

namespace decompiler {
class LinkedObjectFile;
class Form;
class DecompilerTypeSystem;
struct FunctionAtomicOps;

struct VariableWithCast {
  std::string name;
  std::optional<TypeSpec> cast;
};

struct StackStructureEntry {
  StackStructureHint hint;
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

struct FunctionVariableDefinitions {
  std::optional<goos::Object> local_vars;
  bool had_pp = false;
  int count = 0;
};

/*!
 * An "environment" for a single function.
 * This contains data for an entire function, like which registers are live when, the types of
 * values in registers, and local variable names.  This does not actually store IR itself, just
 * shared data that all IR can look at.  The concept is somewhat similar to Env in the compiler.
 */
class Env {
 public:
  GameVersion version = GameVersion::Jak1;
  bool types_succeeded = false;
  bool has_local_vars() const { return m_has_local_vars; }
  bool has_type_analysis() const { return m_has_types; }
  bool has_reg_use() const { return m_has_reg_use; }
  const RegUsageInfo& reg_use() const {
    ASSERT(m_has_reg_use);
    return m_reg_use;
  }

  void set_reg_use(const RegUsageInfo& info) {
    m_reg_use = info;
    m_has_reg_use = true;
  }

  RegUsageInfo& reg_use() {
    ASSERT(m_has_reg_use);
    return m_reg_use;
  }

  // TODO - remove this.
  goos::Object get_variable_name_with_cast(Register reg, int atomic_idx, AccessMode mode) const;
  goos::Object get_variable_name_with_cast(const RegisterAccess& access) const;
  std::string get_variable_name(const RegisterAccess& access) const;
  std::string get_variable_name_name_only(const RegisterAccess& access) const;
  VariableWithCast get_variable_and_cast(const RegisterAccess& access) const;
  std::optional<TypeSpec> get_user_cast_for_access(const RegisterAccess& access) const;
  TypeSpec get_variable_type(const RegisterAccess& access, bool using_user_var_types) const;
  TP_Type get_variable_tp_type(const RegisterAccess& access, bool using_user_var_types) const;

  /*!
   * Get the types in registers _after_ the given operation has completed.
   */
  const TypeState& get_types_after_op(int atomic_op_id) const {
    ASSERT(m_has_types);
    return m_op_end_types.at(atomic_op_id);
  }

  const TypeState& get_types_before_op(int atomic_op_id) const {
    ASSERT(m_has_types);
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
    ASSERT(m_has_types);
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
  FunctionVariableDefinitions local_var_type_list(const Form* top_level_form,
                                                  int nargs_to_ignore) const;

  std::unordered_set<RegId, RegId::hash> get_ssa_var(const RegAccessSet& vars) const;
  RegId get_program_var_id(const RegisterAccess& var) const;

  bool allow_sloppy_pair_typing() const { return m_allow_sloppy_pair_typing; }
  void set_sloppy_pair_typing() { m_allow_sloppy_pair_typing = true; }
  void set_type_casts(const std::unordered_map<int, std::vector<RegisterTypeCast>>& casts) {
    m_typecasts = casts;
  }
  const std::unordered_map<int, std::vector<RegisterTypeCast>>& casts() const {
    return m_typecasts;
  }

  void set_stack_casts(const std::unordered_map<int, StackTypeCast>& casts) {
    m_stack_typecasts = casts;
  }

  const std::unordered_map<int, StackTypeCast>& stack_casts() const { return m_stack_typecasts; }

  void set_art_group(const std::string& art_group) { m_art_group = art_group; }
  const std::string& art_group() const { return m_art_group; }
  std::optional<std::string> get_art_elt_name(int idx) const;
  void set_jg(const std::string& art_group) {
    if (art_group.substr(art_group.size() - 3) == "-ag") {
      m_joint_geo = art_group.substr(0, art_group.size() - 3) + "-lod0-jg";
    } else {
      m_joint_geo = art_group + "-lod0-jg";
    }
  }
  const std::string& joint_geo() const { return m_joint_geo; }
  std::optional<std::string> get_joint_node_name(int idx) const;

  void set_remap_for_function(const Function& func);
  void set_remap_for_method(const TypeSpec& ts);
  void set_remap_for_new_method(const TypeSpec& ts);
  void set_remap_for_relocate_method(const TypeSpec& ts);
  void set_remap_for_memusage_method(const TypeSpec& ts);
  void map_args_from_config(const std::vector<std::string>& args_names,
                            const std::unordered_map<std::string, std::string>& var_names);
  void map_args_from_config(const std::vector<std::string>& args_names,
                            const std::unordered_map<std::string, LocalVarOverride>& var_overrides);

  const std::string& remapped_name(const std::string& name) const;

  bool op_id_is_eliminated_coloring_move(int op_id) const {
    ASSERT(has_local_vars());
    return m_var_names.eliminated_move_op_ids.find(op_id) !=
           m_var_names.eliminated_move_op_ids.end();
  }

  void set_stack_structure_hints(const std::vector<StackStructureHint>& hints);
  void add_stack_structure_hint(const StackStructureHint& hint);
  const std::vector<StackStructureEntry>& stack_structure_hints() const {
    return m_stack_structures;
  }

  const UseDefInfo& get_use_def_info(const RegisterAccess& ra) const;
  void disable_use(const RegisterAccess& access);

  void disable_def(const RegisterAccess& access, DecompWarnings& warnings);

  void set_defined_in_let(const std::string& var) { m_vars_defined_in_let.insert(var); }

  void set_retype_map(const std::unordered_map<std::string, TypeSpec>& map) { m_var_retype = map; }

  void set_stack_spills(const StackSpillMap& map) { m_stack_spill_map = map; }
  const StackSpillMap& stack_spills() const { return m_stack_spill_map; }

  // todo - remove these hacks at some point.
  LinkedObjectFile* file = nullptr;
  DecompilerTypeSystem* dts = nullptr;
  Function* func = nullptr;

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

  // hacks:
  bool aggressively_reject_cond_to_value_rewrite = false;

  bool pp_mapped_by_behavior() const { return m_pp_mapped_by_behavior; }

 private:
  RegisterAccess m_end_var;

  bool m_has_reg_use = false;
  RegUsageInfo m_reg_use;

  bool m_has_local_vars = false;
  VariableNames m_var_names;

  bool m_has_types = false;
  bool m_pp_mapped_by_behavior = false;
  std::vector<TypeState> m_block_init_types;
  std::vector<TypeState> m_op_end_types;
  std::vector<TypeState*> m_op_init_types;

  bool m_allow_sloppy_pair_typing = false;

  std::unordered_map<int, std::vector<RegisterTypeCast>> m_typecasts;
  std::unordered_map<int, StackTypeCast> m_stack_typecasts;
  std::vector<StackStructureEntry> m_stack_structures;
  std::unordered_map<std::string, std::string> m_var_remap;
  std::unordered_map<std::string, TypeSpec> m_var_retype;

  std::unordered_set<std::string> m_vars_defined_in_let;
  std::optional<TypeSpec> m_type_analysis_return_type;

  StackSpillMap m_stack_spill_map;

  std::string m_art_group;
  std::string m_joint_geo;
};
}  // namespace decompiler
