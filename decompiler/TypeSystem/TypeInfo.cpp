#include "TypeInfo.h"

#include <utility>

namespace {
TypeInfo gTypeInfo;
}

TypeInfo::TypeInfo() {
  GoalType type_type("type");
  m_types["type"] = type_type;
  GoalSymbol type_symbol("type");
  m_symbols["type"] = type_symbol;
}

TypeInfo& get_type_info() {
  return gTypeInfo;
}

std::string TypeInfo::get_summary() {
  int total_symbols = 0;
  int syms_with_type_info = 0;
  for (const auto& kv : m_symbols) {
    total_symbols++;
    if (kv.second.has_type_info()) {
      syms_with_type_info++;
    }
  }

  int total_types = 0;
  int types_with_info = 0;
  int types_with_method_count = 0;
  for (const auto& kv : m_types) {
    total_types++;
    if (kv.second.has_info()) {
      types_with_info++;
    }
    if (kv.second.has_method_count()) {
      types_with_method_count++;
    }
  }

  char buffer[1024];
  sprintf(buffer,
          "TypeInfo Summary\n"
          " Total Symbols: %d\n"
          "   with type info: %d (%.2f%%)\n"
          " Total Types: %d\n"
          "   with info: %d (%.2f%%)\n"
          "   with method count: %d (%.2f%%)\n",
          total_symbols, syms_with_type_info,
          100.f * float(syms_with_type_info) / float(total_symbols), total_types, types_with_info,
          100.f * float(types_with_info) / float(total_types), types_with_method_count,
          100.f * float(types_with_method_count) / float(total_types));

  return {buffer};
}

/*!
 * inform TypeInfo that there is a symbol with this name.
 * Provides no type info - if some is already known there is no change.
 */
void TypeInfo::inform_symbol_with_no_type_info(const std::string& name) {
  if (m_symbols.find(name) == m_symbols.end()) {
    // only add it if we haven't seen this already.
    GoalSymbol sym(name);
    m_symbols[name] = sym;
  }
}

void TypeInfo::inform_symbol(const std::string& name, TypeSpec type) {
  inform_symbol_with_no_type_info(name);
  m_symbols.at(name).set_type(std::move(type));
}

void TypeInfo::inform_type(const std::string& name) {
  if (m_types.find(name) == m_types.end()) {
    GoalType typ(name);
    m_types[name] = typ;
  }
  inform_symbol(name, TypeSpec("type"));
}

void TypeInfo::inform_type_method_count(const std::string& name, int methods) {
  // create type and symbol
  inform_type(name);
  m_types.at(name).set_methods(methods);
}

std::string TypeInfo::get_all_symbols_debug() {
  std::string result = "const char* all_syms[" + std::to_string(m_symbols.size()) + "] = {";
  for (auto& x : m_symbols) {
    result += "\"" + x.first + "\",";
  }
  if (!result.empty()) {
    result.pop_back();
  }
  return result + "};";
}