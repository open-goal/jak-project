#pragma once

#include <set>
#include <string>
#include <vector>

#include "common/goos/Object.h"
#include "common/util/Assert.h"
#include "common/util/Trie.h"

#include "goalc/compiler/Val.h"

/*!
 * Info about a single symbol, representing one of:
 *  - Global variable
 *  - Global function
 *  - Type
 *  - Constant
 *  - Macro
 *  - Builtin keyword of the OpenGOAL language
 */
class SymbolInfo {
 public:
  struct Metadata {
    std::string docstring = "";
  };

  // TODO - states
  // TODO - enums
  enum class Kind {
    GLOBAL_VAR,
    FWD_DECLARED_SYM,
    FUNCTION,
    TYPE,
    CONSTANT,
    MACRO,
    LANGUAGE_BUILTIN,
    METHOD,
    INVALID
  };

  static SymbolInfo make_global(const std::string& name,
                                const goos::Object& defining_form,
                                const std::optional<Metadata> meta = {}) {
    SymbolInfo info;
    info.m_kind = Kind::GLOBAL_VAR;
    info.m_name = name;
    info.m_def_form = defining_form;
    if (meta) {
      info.m_meta = *meta;
    }
    return info;
  }

  static SymbolInfo make_fwd_declared_sym(const std::string& name,
                                          const goos::Object& defining_form) {
    SymbolInfo info;
    info.m_kind = Kind::FWD_DECLARED_SYM;
    info.m_name = name;
    info.m_def_form = defining_form;
    return info;
  }

  static SymbolInfo make_function(const std::string& name,
                                  const std::vector<GoalArg> args,
                                  const goos::Object& defining_form,
                                  const std::optional<Metadata> meta = {}) {
    SymbolInfo info;
    info.m_kind = Kind::FUNCTION;
    info.m_name = name;
    info.m_def_form = defining_form;
    if (meta) {
      info.m_meta = *meta;
    }
    info.m_args = args;
    return info;
  }

  static SymbolInfo make_type(const std::string& name,
                              const goos::Object& defining_form,
                              const std::optional<Metadata> meta = {}) {
    SymbolInfo info;
    info.m_kind = Kind::TYPE;
    info.m_name = name;
    info.m_def_form = defining_form;
    if (meta) {
      info.m_meta = *meta;
    }
    return info;
  }

  static SymbolInfo make_constant(const std::string& name,
                                  const goos::Object& defining_form,
                                  const std::optional<Metadata> meta = {}) {
    SymbolInfo info;
    info.m_kind = Kind::CONSTANT;
    info.m_name = name;
    info.m_def_form = defining_form;
    if (meta) {
      info.m_meta = *meta;
    }
    return info;
  }

  static SymbolInfo make_macro(const std::string& name,
                               const goos::Object& defining_form,
                               const std::optional<Metadata> meta = {}) {
    SymbolInfo info;
    info.m_kind = Kind::MACRO;
    info.m_name = name;
    info.m_def_form = defining_form;
    if (meta) {
      info.m_meta = *meta;
    }
    return info;
  }

  static SymbolInfo make_builtin(const std::string& name, const std::optional<Metadata> meta = {}) {
    SymbolInfo info;
    info.m_kind = Kind::LANGUAGE_BUILTIN;
    info.m_name = name;
    if (meta) {
      info.m_meta = *meta;
    }
    return info;
  }

  static SymbolInfo make_method(const std::string& method_name,
                                const std::vector<GoalArg> args,
                                const MethodInfo& method_info,
                                const goos::Object& defining_form) {
    SymbolInfo info;
    info.m_kind = Kind::METHOD;
    info.m_name = method_name;
    info.m_method_info = method_info;
    info.m_def_form = defining_form;
    info.m_meta.docstring =
        info.m_method_info.docstring.has_value() ? info.m_method_info.docstring.value() : "";
    info.m_args = args;
    return info;
  }

  const std::string& name() const { return m_name; }
  const MethodInfo& method_info() const { return m_method_info; }
  Kind kind() const { return m_kind; }
  const goos::Object& src_form() const { return m_def_form; }
  const Metadata& meta() const { return m_meta; }
  const std::vector<GoalArg>& args() const { return m_args; }

 private:
  Kind m_kind = Kind::INVALID;
  goos::Object m_def_form;
  std::string m_name;
  MethodInfo m_method_info;
  Metadata m_meta;
  std::vector<GoalArg> m_args;

  std::string m_return_type;
};

/*!
 * A map of symbol info. It internally stores the info in a prefix tree so you can quickly get
 * a list of all symbols starting with a given prefix.
 */
class SymbolInfoMap {
 public:
  SymbolInfoMap() = default;
  void add_global(const std::string& name,
                  const goos::Object& defining_form,
                  const std::optional<SymbolInfo::Metadata> meta = {}) {
    m_map[name]->push_back(SymbolInfo::make_global(name, defining_form, meta));
  }

  void add_fwd_dec(const std::string& name, const goos::Object& defining_form) {
    m_map[name]->push_back(SymbolInfo::make_fwd_declared_sym(name, defining_form));
  }

  // The m_symbol_types container stores TypeSpecs -- this does have argument information but not
  // the names, which is why they have to be explicitly provided
  void add_function(const std::string& name,
                    const std::vector<GoalArg> args,
                    const goos::Object& defining_form,
                    const std::optional<SymbolInfo::Metadata> meta = {}) {
    m_map[name]->push_back(SymbolInfo::make_function(name, args, defining_form, meta));
  }

  void add_type(const std::string& name,
                const goos::Object& defining_form,
                const std::optional<SymbolInfo::Metadata> meta = {}) {
    m_map[name]->push_back(SymbolInfo::make_type(name, defining_form, meta));
  }

  void add_constant(const std::string& name,
                    const goos::Object& defining_form,
                    const std::optional<SymbolInfo::Metadata> meta = {}) {
    m_map[name]->push_back(SymbolInfo::make_constant(name, defining_form, meta));
  }

  void add_macro(const std::string& name,
                 const goos::Object& defining_form,
                 const std::optional<SymbolInfo::Metadata> meta = {}) {
    m_map[name]->push_back(SymbolInfo::make_macro(name, defining_form, meta));
  }

  void add_builtin(const std::string& name, const std::optional<SymbolInfo::Metadata> meta = {}) {
    m_map[name]->push_back(SymbolInfo::make_builtin(name, meta));
  }

  // The m_symbol_types container stores TypeSpecs -- this does have argument information but not
  // the names, which is why they have to be explicitly provided
  void add_method(const std::string& method_name,
                  const std::vector<GoalArg> args,
                  const MethodInfo& method_info,
                  const goos::Object& defining_form) {
    m_map[method_name]->push_back(
        SymbolInfo::make_method(method_name, args, method_info, defining_form));
  }

  std::vector<SymbolInfo>* lookup_exact_name(const std::string& name) const {
    return m_map.lookup(name);
  }

  std::set<std::string> lookup_symbols_starting_with(const std::string& prefix) const {
    std::set<std::string> result;
    auto lookup = m_map.lookup_prefix(prefix);
    for (auto& x : lookup) {
      for (auto& y : *x) {
        result.insert(y.name());
      }
    }
    return result;
  }

  int symbol_count() const { return m_map.size(); }

  std::vector<SymbolInfo> get_all_symbols() const {
    std::vector<SymbolInfo> info;
    auto lookup = m_map.get_all_nodes();
    for (auto& x : lookup) {
      for (auto& y : *x) {
        info.push_back(y);
      }
    }
    return info;
  }

 private:
  Trie<std::vector<SymbolInfo>> m_map;
};
