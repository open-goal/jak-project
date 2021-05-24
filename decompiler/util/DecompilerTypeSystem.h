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
  struct SymbolMetadata {
    std::string associated_file;

    SymbolMetadata() = default;
    SymbolMetadata(const std::string& file_name) : associated_file(file_name){};
  };

  DecompilerTypeSystem();
  TypeSystem ts;
  std::unordered_map<std::string, TypeSpec> symbol_types;
  std::unordered_set<std::string> symbols;
  std::vector<std::string> symbol_add_order;
  std::unordered_map<std::string, SymbolMetadata> symbol_metadata;
  std::unordered_map<std::string, u64> type_flags;
  std::unordered_map<std::string, std::string> type_parents;

  void add_symbol(const std::string& name, const SymbolMetadata& metadata = SymbolMetadata()) {
    if (!symbols.count(name)) {
      symbols.insert(name);
      symbol_add_order.push_back(name);
      symbol_metadata[name] = metadata;
    } else if (symbol_metadata[name].associated_file.empty() && !metadata.associated_file.empty()) {
      symbol_metadata[name] = metadata;
    }
  }

  void add_symbol(const std::string& name,
                  const std::string& base_type,
                  const std::string& file_name = "") {
    add_symbol(name, TypeSpec(base_type));
  }

  void add_symbol(const std::string& name, const TypeSpec& type_spec);
  void parse_type_defs(const std::vector<std::string>& file_path);
  TypeSpec parse_type_spec(const std::string& str) const;
  void add_type_flags(const std::string& name, u64 flags);
  void add_type_parent(const std::string& child, const std::string& parent);
  std::string dump_symbol_types();
  std::string dump_symbol_mapping();
  std::string lookup_parent_from_inspects(const std::string& child) const;
  bool lookup_flags(const std::string& type, u64* dest) const;
  TP_Type tp_lca(const TP_Type& existing, const TP_Type& add, bool* changed) const;
  bool tp_lca(TypeState* combined, const TypeState& add);
  int get_format_arg_count(const std::string& str) const;
  int get_format_arg_count(const TP_Type& type) const;
  TypeSpec lookup_symbol_type(const std::string& name) const;

  // todo - totally eliminate this.
  struct {
    std::string current_method_type;
    void reset() { current_method_type.clear(); }
  } type_prop_settings;

 private:
  mutable goos::Reader m_reader;
};
}  // namespace decompiler

#endif  // JAK_DECOMPILERTYPESYSTEM_H
