#pragma once

#include "common/goos/Reader.h"
#include "common/goos/TextDB.h"
#include "common/type_system/TypeSystem.h"

#include "decompiler/Disasm/Register.h"

namespace decompiler {
class TP_Type;
class TypeState;

class DecompilerTypeSystem {
 public:
  DecompilerTypeSystem(GameVersion version);
  TypeSystem ts;

  std::unordered_map<std::string, TypeSpec> symbol_types;
  std::unordered_set<std::string> symbols;
  std::vector<std::string> symbol_add_order;

  // TODO - these are needed to propagate the info from the `Type` to the final result
  // as only the `TypeSpec` is available at that point
  std::unordered_map<std::string, DefinitionMetadata> symbol_metadata_map;
  // {type_name : {method_name : {handler : doc}}}
  std::unordered_map<
      std::string,
      std::unordered_map<std::string, std::unordered_map<std::string, DefinitionMetadata>>>
      virtual_state_metadata;
  // {state_name : {handler : doc}}
  std::unordered_map<std::string, std::unordered_map<std::string, DefinitionMetadata>>
      state_metadata;

  std::unordered_map<std::string, u64> type_flags;
  std::unordered_map<std::string, std::string> type_parents;
  std::unordered_map<std::string, int> bad_format_strings;
  std::unordered_map<std::string, std::vector<std::vector<int>>>
      format_ops_with_dynamic_string_by_func_name;
  std::unordered_map<std::string, std::unordered_map<int, std::string>> art_group_info;
  std::unordered_map<std::string, std::unordered_map<int, std::string>> jg_info;

  void add_symbol(const std::string& name) {
    if (symbols.find(name) == symbols.end()) {
      symbols.insert(name);
      symbol_add_order.push_back(name);
    }
  }

  void add_symbol(const std::string& name,
                  const std::string& base_type,
                  const DefinitionMetadata& symbol_metadata) {
    add_symbol(name, TypeSpec(base_type), symbol_metadata);
  }

  void add_symbol(const std::string& name,
                  const TypeSpec& type_spec,
                  const DefinitionMetadata& symbol_metadata);
  void parse_type_defs(const std::vector<std::string>& file_path);
  TypeSpec parse_type_spec(const std::string& str) const;
  void add_type_flags(const std::string& name, u64 flags);
  void add_type_parent(const std::string& child, const std::string& parent);
  std::string dump_symbol_types();
  std::string lookup_parent_from_inspects(const std::string& child) const;
  bool lookup_flags(const std::string& type, u64* dest) const;
  TP_Type tp_lca(const TP_Type& existing, const TP_Type& add, bool* changed) const;
  bool tp_lca(TypeState* combined, const TypeState& add);
  int get_format_arg_count(const std::string& str) const;
  int get_format_arg_count(const TP_Type& type) const;
  int get_dynamic_format_arg_count(const std::string& func_name, int op_idx) const;
  TypeSpec lookup_symbol_type(const std::string& name) const;
  bool should_attempt_cast_simplify(const TypeSpec& expected, const TypeSpec& actual) const;

  void add_art_group_elt(const std::string& ag_name, const std::string& elt_name, int elt_index) {
    if (art_group_info.count(ag_name) == 0) {
      art_group_info[ag_name] = {};
    }
    if (art_group_info.at(ag_name).count(elt_index) == 0) {
      art_group_info.at(ag_name)[elt_index] = elt_name;
    }
  }

  void add_joint_node(const std::string& jg_name, const std::string& joint_name, int joint_idx) {
    if (jg_info.count(jg_name) == 0) {
      jg_info[jg_name] = {};
    }
    if (jg_info.at(jg_name).count(joint_idx) == 0) {
      jg_info.at(jg_name)[joint_idx] = joint_name;
    }
  }

  // todo - totally eliminate this.
  struct {
    std::string current_method_type;
    void reset() { current_method_type.clear(); }
  } type_prop_settings;

 private:
  mutable goos::Reader m_reader;
};
}  // namespace decompiler
