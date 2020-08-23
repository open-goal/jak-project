#ifndef JAK_DISASSEMBLER_TYPEINFO_H
#define JAK_DISASSEMBLER_TYPEINFO_H

#include <unordered_map>
#include "GoalType.h"
#include "GoalFunction.h"
#include "GoalSymbol.h"

class TypeInfo {
 public:
  TypeInfo();

  void inform_symbol(const std::string& name, TypeSpec type);
  void inform_symbol_with_no_type_info(const std::string& name);
  void inform_type(const std::string& name);
  void inform_type_method_count(const std::string& name, int methods);

  std::string get_summary();
  std::string get_all_symbols_debug();

 private:
  std::unordered_map<std::string, GoalType> m_types;
  std::unordered_map<std::string, GoalFunction> m_global_functions;
  std::unordered_map<std::string, GoalSymbol> m_symbols;
};

TypeInfo& get_type_info();
void init_type_info();

#endif  // JAK_DISASSEMBLER_TYPEINFO_H
