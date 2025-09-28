#pragma once

#include <optional>
#include <string>

#include "goalc/compiler/symbol_info.h"

#include "third-party/json.hpp"

using json = nlohmann::json;

// TODO - deprecate this file in factor of the now consolidated `SymbolInfo`
// which now contains comprehensive info on all forms of symbols

namespace Docs {

struct DefinitionLocation {
  std::string filename;
  int line_idx = -1;
  int char_idx = -1;
};
void to_json(json& j, const DefinitionLocation& obj);

struct BuiltinDocumentation {
  std::string name;
  std::string description = "";
};
void to_json(json& j, const BuiltinDocumentation& obj);

struct VariableDocumentation {
  std::string name;
  std::string description = "";
  std::string type;
  std::optional<DefinitionLocation> def_location;
  bool is_constant;
};
void to_json(json& j, const VariableDocumentation& obj);

struct ArgumentDocumentation {
  std::string name;
  std::string type;
  // Below is parsed from the functions docstring
  std::string description = "";
  bool is_mutated = false;
  bool is_optional = false;
  bool is_unused = false;
};
void to_json(json& j, const ArgumentDocumentation& obj);

struct FunctionDocumentation {
  std::string name;
  std::string description;
  std::optional<DefinitionLocation> def_location;
  // TODO - need to track function calls to determine this, obviously cant be determined from just
  // the definition
  //
  // If this is done, that also solves the issue of reference-finding
  bool is_unused = false;
  std::vector<ArgumentDocumentation> args;
  std::string return_type;
};
void to_json(json& j, const FunctionDocumentation& obj);

struct FieldDocumentation {
  std::string name;
  std::string description = "";
  std::string type;
  bool is_array = false;
  bool is_dynamic = false;
  bool is_inline = false;
};
void to_json(json& j, const FieldDocumentation& obj);

struct TypeMethodDocumentation {
  // TODO - relevant?
  int id;
  std::string name;
  bool is_override = false;
};
void to_json(json& j, const TypeMethodDocumentation& obj);

struct TypeStateDocumentation {
  bool is_virtual;
  std::optional<int> id;
  std::string name;
};
void to_json(json& j, const TypeStateDocumentation& obj);

struct TypeDocumentation {
  std::string name;
  std::string description = "";
  std::string parent_type;
  std::optional<DefinitionLocation> def_location;
  int size;
  std::vector<FieldDocumentation> fields = {};
  // TODO - who cares, remove this probably
  int method_count;
  std::vector<TypeMethodDocumentation> methods = {};
  std::vector<TypeStateDocumentation> states = {};
};
void to_json(json& j, const TypeDocumentation& obj);

struct MethodDocumentation {
  // TODO - relevant?
  int id;
  bool is_builtin;
  std::string name;
  std::string description = "";
  // TODO - this is `object` sometimes, for example `(defmethod print ((this light))`
  // i believe this is because we always grab the first symbol, but of course, overridden methods
  // dont work like that so things are likely working as intended
  std::string type;
  std::optional<DefinitionLocation> def_location;
  // TODO - need to track function calls to determine this, obviously cant be determined from just
  // the definition
  //
  // If this is done, that also solves the issue of reference-finding
  bool is_unused = false;
  bool is_override = false;
  std::vector<ArgumentDocumentation> args;
  std::string return_type;
};
void to_json(json& j, const MethodDocumentation& obj);

struct MacroDocumentation {
  std::string name;
  std::string description = "";
  std::vector<std::string> args;
  std::vector<std::pair<std::string, std::optional<std::string>>> kwargs;
  std::optional<std::string> variadic_arg;
  std::optional<DefinitionLocation> def_location = {};
};
void to_json(json& j, const MacroDocumentation& obj);

struct FileDocumentation {
  std::string description = "";
  std::vector<VariableDocumentation> global_vars;
  std::vector<FunctionDocumentation> functions;
  std::vector<TypeDocumentation> types;
  std::vector<VariableDocumentation> constants;
  std::vector<MacroDocumentation> macros;
  std::vector<MethodDocumentation> methods;
  // TODO - enums and states
};
void to_json(json& j, const FileDocumentation& obj);

struct SymbolDocumentation {
  // TODO - forward declared symbols
  std::string name;
  std::string description = "";
  symbol_info::Kind kind;
  std::optional<DefinitionLocation> def_location = {};
  std::vector<DefinitionLocation> forward_declared_in = {};
};
void to_json(json& j, const SymbolDocumentation& obj);

std::vector<ArgumentDocumentation> get_args_from_docstring(
    std::vector<symbol_info::ArgumentInfo> args,
    std::string docstring);

}  // namespace Docs
