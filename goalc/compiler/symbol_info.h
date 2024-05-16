#pragma once

#include <set>
#include <string>
#include <vector>

#include "common/goos/Object.h"
#include "common/util/Assert.h"
#include "common/util/trie_with_duplicates.h"

#include "goalc/compiler/Val.h"

namespace symbol_info {

// Require statement todo:
// - [] reduce empty fields in SymbolInfo
// - [] `bundle-list` command
// - [] get rid of old doc generation code
// - [] compilation speed report
// - [] potentially remove redundant map in compiler, try to get my symbol info trie more efficient
// (assuming its not).  Some sort of lookup cache, etc.
// - [] replace most unordered_maps with robin-maps atleast a 2x improvement (benchmark it)
// - [] some paths are stored differently for some reason, there's some inconsistent path separators
// somewhere, investigate

enum class Kind {
  GLOBAL_VAR,
  FWD_DECLARED_SYM,
  FUNCTION,
  TYPE,
  CONSTANT,
  MACRO,  // TODO - defining form for virtual state isnt tracked by reader
  LANGUAGE_BUILTIN,
  METHOD,
  STATE,  // TODO - defining form for virtual state isnt tracked by reader
  ENUM,
  INVALID
};

struct DefinitionLocation {
  std::string file_path;
  uint32_t line_idx;
  uint32_t char_idx;
  // TODO - store the extent of the symbol definition as well
};

struct ArgumentInfo {
  std::string name;
  std::string type;
  std::string description = "";
  // !var
  bool is_mutated = false;
  // ?var
  bool is_optional = false;
  // _var
  bool is_unused = false;
};

struct FieldInfo {
  std::string name;
  // TODO - DefinitionLocation def_location;
  std::string description = "";
  std::string type;
  // ?? TODO
  bool is_array = false;
  // :dynamic
  bool is_dynamic = false;
  // :inline
  bool is_inline = false;
};

struct TypeMethodInfo {
  int id;  // TODO - is this even relevant anymore?
  std::string name;
  // TODO - DefinitionLocation def_location;
  bool is_override = false;
};

struct TypeStateInfo {
  std::string name;
  // TODO - DefinitionLocation def_location;
  bool is_virtual = false;
  std::optional<int> id;  // TODO - is this even relevant anymore?
};

struct SymbolInfo {
  Kind m_kind = Kind::INVALID;
  std::string m_name;
  goos::Object m_def_form;
  std::optional<DefinitionLocation> m_def_location;
  std::string m_docstring = "";
  std::string m_type = "";
  // Method or Function Related
  std::vector<ArgumentInfo> m_args = {};
  std::string m_return_type = "";
  // Method Related
  MethodInfo m_method_info;
  bool m_method_builtin = false;
  // Type Related
  std::string m_parent_type = "";
  int m_type_size = -1;
  std::vector<FieldInfo> m_type_fields = {};
  std::vector<TypeMethodInfo> m_type_methods = {};
  std::vector<TypeStateInfo> m_type_states = {};
  // Macro Related
  std::vector<std::string> m_macro_args = {};
  std::vector<std::pair<std::string, std::optional<std::string>>> m_macro_kwargs = {};
  std::optional<std::string> m_variadic_arg = {};
  // State Related
  std::string m_state_related_type;
  bool m_state_virtual = false;
  std::unordered_map<std::string, std::string> m_state_handler_docstrings;
  std::vector<ArgumentInfo> m_state_enter_and_code_args = {};
  // Enum related
  EnumType* m_enum_info = nullptr;

  // TODO: need to track references for this, this is a TODO for LSP work
  // bool is_unused = false;

  void update_args_from_docstring();
  void set_definition_location(const goos::TextDb* textdb);
};

/*!
 * A map of symbol info. It internally stores the info in a prefix tree so you can quickly get
 * a list of all symbols starting with a given prefix.
 */
class SymbolInfoMap {
  goos::TextDb* m_textdb;
  TrieWithDuplicates<SymbolInfo> m_symbol_map;
  // Indexes references to symbols by the file they are defined within
  // This allows us to not only efficiently retrieve symbols by file, but also allows us to
  // cleanup symbols when files are re-compiled.
  std::unordered_map<std::string, std::vector<SymbolInfo*>> m_file_symbol_index;

  void add_symbol_to_file_index(const std::string& file_path, SymbolInfo* symbol);

 public:
  SymbolInfoMap(goos::TextDb* textdb) : m_textdb(textdb) {}
  void add_global(const std::string& name,
                  const std::string& type,
                  const goos::Object& defining_form,
                  const std::string& docstring = "");
  void add_fwd_dec(const std::string& name, const goos::Object& defining_form);
  void add_function(const std::string& name,
                    const std::string& return_type,
                    const std::vector<GoalArg>& args,
                    const goos::Object& defining_form,
                    const std::string& docstring = "");
  void add_type(const std::string& name,
                Type* type_info,
                const goos::Object& defining_form,
                const std::string& docstring = "");
  void add_constant(const std::string& name,
                    const goos::Object& defining_form,
                    const std::string& docstring = "");
  void add_macro(const std::string& name,
                 const goos::ArgumentSpec arg_spec,
                 const goos::Object& defining_form,
                 const std::string& docstring = "");
  void add_builtin(const std::string& name, const std::string& docstring = "");
  void add_method(const std::string& method_name,
                  const std::vector<GoalArg>& args,
                  const MethodInfo& method_info,
                  const goos::Object& defining_form);
  void add_state(const std::string& name,
                 const std::string& related_type,
                 const bool is_virtual,
                 const std::vector<ArgumentInfo>& code_args,
                 const goos::Object& defining_form,
                 const std::string& docstring);
  void add_enum(EnumType* enum_info,
                const goos::Object& defining_form,
                const std::string& docstring = "");
  std::vector<SymbolInfo*> lookup_symbols_by_file(const std::string& file_path) const;
  std::vector<SymbolInfo*> lookup_exact_name(const std::string& name) const;
  std::vector<SymbolInfo*> lookup_exact_name(const std::string& name, const Kind sym_kind) const;
  std::vector<SymbolInfo*> lookup_exact_method_name(const std::string& name,
                                                    const std::string& defining_type_name) const;
  std::vector<SymbolInfo*> lookup_exact_virtual_state_name(
      const std::string& name,
      const std::string& defining_type_name) const;
  std::vector<SymbolInfo*> lookup_symbols_starting_with(const std::string& prefix) const;
  std::set<std::string> lookup_names_starting_with(const std::string& prefix,
                                                   int max_count = -1) const;
  std::vector<SymbolInfo*> get_all_symbols() const;
  int symbol_count() const;
  // Uses the per-file index to find and evict symbols globally
  // This should be done before re-compiling a file, symbols will be re-added to the DB if they are
  // found again
  void evict_symbols_using_file_index(const std::string& file_path);
};

}  // namespace symbol_info
