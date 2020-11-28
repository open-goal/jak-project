#ifndef JAK_DECOMPILERTYPESYSTEM_H
#define JAK_DECOMPILERTYPESYSTEM_H

#include "common/type_system/TypeSystem.h"
#include "decompiler/Disasm/Register.h"

struct TP_Type;
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
  void add_type_flags(const std::string& name, u64 flags);
  void add_type_parent(const std::string& child, const std::string& parent);
  std::string dump_symbol_types();
  std::string lookup_parent_from_inspects(const std::string& child) const;
  bool lookup_flags(const std::string& type, u64* dest) const;
  TP_Type tp_lca(const TP_Type& existing, const TP_Type& add, bool* changed);
  TP_Type tp_lca_no_simplify(const TP_Type& existing, const TP_Type& add, bool* changed);
  bool tp_lca(TypeState* combined, const TypeState& add);
  struct {
    bool allow_pair;
    std::string current_method_type;
    void reset() {
      allow_pair = false;
      current_method_type.clear();
    }
  } type_prop_settings;
};

#endif  // JAK_DECOMPILERTYPESYSTEM_H
