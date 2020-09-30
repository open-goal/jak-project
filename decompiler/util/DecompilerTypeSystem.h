#ifndef JAK_DECOMPILERTYPESYSTEM_H
#define JAK_DECOMPILERTYPESYSTEM_H

#include "common/type_system/TypeSystem.h"
#include "third-party/fmt/format.h"

class DecompilerTypeSystem {
 public:
  DecompilerTypeSystem();
  TypeSystem ts;
  std::unordered_map<std::string, TypeSpec> symbol_types;
  std::unordered_set<std::string> symbols;
  std::vector<std::string> symbol_add_order;

  void add_symbol(const std::string& name) {
    if (symbols.find(name) == symbols.end()) {
      symbols.insert(name);
      symbol_add_order.push_back(name);
    }
  }

  void add_symbol(const std::string& name, const std::string& base_type) {
    add_symbol(name, TypeSpec(base_type));
  }

  void add_symbol(const std::string& name, const TypeSpec& type_spec) {
    add_symbol(name);
    auto skv = symbol_types.find(name);
    if (skv == symbol_types.end() || skv->second == type_spec) {
      symbol_types[name] = type_spec;
    } else {
      if (ts.typecheck(type_spec, skv->second, "", false, false)) {
      } else {
        fmt::print("Attempting to redefine type of symbol {} from {} to {}\n", name,
                   skv->second.print(), type_spec.print());
        throw std::runtime_error("Type redefinition");
      }
    }
  }

  void parse_type_defs(const std::vector<std::string>& file_path);

  std::string dump_symbol_types();
};

#endif  // JAK_DECOMPILERTYPESYSTEM_H
