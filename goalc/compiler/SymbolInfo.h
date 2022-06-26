#pragma once

#include <set>
#include <string>
#include <vector>

#include "common/goos/Object.h"
#include "common/util/Assert.h"
#include "common/util/Trie.h"

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

  static SymbolInfo make_global(const std::string& name, const goos::Object& defining_form) {
    SymbolInfo info;
    info.m_kind = Kind::GLOBAL_VAR;
    info.m_name = name;
    info.m_def_form = defining_form;
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

  static SymbolInfo make_function(const std::string& name, const goos::Object& defining_form) {
    SymbolInfo info;
    info.m_kind = Kind::FUNCTION;
    info.m_name = name;
    info.m_def_form = defining_form;
    return info;
  }

  static SymbolInfo make_type(const std::string& name, const goos::Object& defining_form) {
    SymbolInfo info;
    info.m_kind = Kind::TYPE;
    info.m_name = name;
    info.m_def_form = defining_form;
    return info;
  }

  static SymbolInfo make_constant(const std::string& name, const goos::Object& defining_form) {
    SymbolInfo info;
    info.m_kind = Kind::CONSTANT;
    info.m_name = name;
    info.m_def_form = defining_form;
    return info;
  }

  static SymbolInfo make_macro(const std::string& name, const goos::Object& defining_form) {
    SymbolInfo info;
    info.m_kind = Kind::MACRO;
    info.m_name = name;
    info.m_def_form = defining_form;
    return info;
  }

  static SymbolInfo make_builtin(const std::string& name) {
    SymbolInfo info;
    info.m_kind = Kind::LANGUAGE_BUILTIN;
    info.m_name = name;
    return info;
  }

  static SymbolInfo make_method(const std::string& method_name,
                                const std::string& type_name,
                                const goos::Object& defining_form) {
    SymbolInfo info;
    info.m_kind = Kind::METHOD;
    info.m_name = method_name;
    info.m_type_of_method = type_name;
    info.m_def_form = defining_form;
    return info;
  }

  const std::string& name() const { return m_name; }
  const std::string& type() const { return m_type_of_method; }
  Kind kind() const { return m_kind; }
  const goos::Object& src_form() const { return m_def_form; }

 private:
  Kind m_kind = Kind::INVALID;
  goos::Object m_def_form;
  std::string m_name;
  std::string m_type_of_method;
};

/*!
 * A map of symbol info. It internally stores the info in a prefix tree so you can quickly get
 * a list of all symbols starting with a given prefix.
 */
class SymbolInfoMap {
 public:
  SymbolInfoMap() = default;
  void add_global(const std::string& name, const goos::Object& defining_form) {
    m_map[name]->push_back(SymbolInfo::make_global(name, defining_form));
  }

  void add_fwd_dec(const std::string& name, const goos::Object& defining_form) {
    m_map[name]->push_back(SymbolInfo::make_fwd_declared_sym(name, defining_form));
  }

  void add_function(const std::string& name, const goos::Object& defining_form) {
    m_map[name]->push_back(SymbolInfo::make_function(name, defining_form));
  }

  void add_type(const std::string& name, const goos::Object& defining_form) {
    m_map[name]->push_back(SymbolInfo::make_type(name, defining_form));
  }

  void add_constant(const std::string& name, const goos::Object& defining_form) {
    m_map[name]->push_back(SymbolInfo::make_constant(name, defining_form));
  }

  void add_macro(const std::string& name, const goos::Object& defining_form) {
    m_map[name]->push_back(SymbolInfo::make_macro(name, defining_form));
  }

  void add_builtin(const std::string& name) {
    m_map[name]->push_back(SymbolInfo::make_builtin(name));
  }

  void add_method(const std::string& method_name,
                  const std::string& type_name,
                  const goos::Object& defining_form) {
    m_map[method_name]->push_back(SymbolInfo::make_method(method_name, type_name, defining_form));
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

 private:
  Trie<std::vector<SymbolInfo>> m_map;
};
