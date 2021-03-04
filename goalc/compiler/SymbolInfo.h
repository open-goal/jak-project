#pragma once

#include <cassert>
#include <vector>
#include <string>
#include <set>
#include "common/util/Trie.h"
#include "common/goos/Object.h"

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
  enum class Kind { GLOBAL_VAR, FUNCTION, TYPE, CONSTANT, MACRO, LANGUAGE_BUILTIN, INVALID };

  static SymbolInfo make_global(const std::string& name, const goos::Object& defining_form) {
    SymbolInfo info;
    info.m_kind = Kind::GLOBAL_VAR;
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

  const std::string& name() const { return m_name; }
  Kind kind() const { return m_kind; }
  const goos::Object& src_form() const { return m_def_form; }

 private:
  Kind m_kind = Kind::INVALID;
  goos::Object m_def_form;
  std::string m_name;
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

  void add_builtin(const std::string& name) {
    m_map[name]->push_back(SymbolInfo::make_builtin(name));
  }

  std::vector<SymbolInfo>* lookup_exact_name(const std::string& name) { return m_map.lookup(name); }

  std::set<std::string> lookup_symbols_starting_with(const std::string& prefix) {
    std::set<std::string> result;
    auto lookup = m_map.lookup_prefix(prefix);
    for (auto& x : lookup) {
      for (auto& y : *x) {
        result.insert(y.name());
      }
    }
    return result;
  }

 private:
  Trie<std::vector<SymbolInfo>> m_map;
};