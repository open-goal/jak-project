#ifndef JAK_DECOMPILERTYPESYSTEM_H
#define JAK_DECOMPILERTYPESYSTEM_H

#include "common/type_system/TypeSystem.h"
#include "decompiler/Disasm/Register.h"
#include "common/goos/Reader.h"

namespace decompiler {
class TP_Type;
struct TypeState;

class DecompilerTypeSystem {
 public:
  DecompilerTypeSystem();
  TypeSystem ts;
  std::unordered_map<std::string, TypeSpec> symbol_types;
  std::unordered_set<std::string> symbols;
  std::vector<std::string> symbol_add_order;
  std::unordered_map<std::string, u64> type_flags;
  std::unordered_map<std::string, std::string> type_parents;

  void add_symbol(const std::string& name) {
    if (symbols.find(name) == symbols.end()) {
      symbols.insert(name);
      symbol_add_order.push_back(name);
    }
  }

  void add_symbol(const std::string& name, const std::string& base_type) {
    add_symbol(name, TypeSpec(base_type));
  }

  void add_symbol(const std::string& name, const TypeSpec& type_spec);
  void parse_type_defs(const std::vector<std::string>& file_path);
  TypeSpec parse_type_spec(const std::string& str);
  void add_type_flags(const std::string& name, u64 flags);
  void add_type_parent(const std::string& child, const std::string& parent);
  std::string dump_symbol_types();
  std::string lookup_parent_from_inspects(const std::string& child) const;
  bool lookup_flags(const std::string& type, u64* dest) const;
  TP_Type tp_lca(const TP_Type& existing, const TP_Type& add, bool* changed);
  TP_Type tp_lca_no_simplify(const TP_Type& existing, const TP_Type& add, bool* changed);
  bool tp_lca(TypeState* combined, const TypeState& add);
  int get_format_arg_count(const std::string& str);
  int get_format_arg_count(const TP_Type& type);
  struct {
    bool allow_pair;
    std::string current_method_type;
    void reset() {
      allow_pair = false;
      current_method_type.clear();
    }
  } type_prop_settings;

 private:
  goos::Reader m_reader;
};
}  // namespace decompiler

#endif  // JAK_DECOMPILERTYPESYSTEM_H
