#ifndef JAK_DISASSEMBLER_GOALSYMBOL_H
#define JAK_DISASSEMBLER_GOALSYMBOL_H

#include <cassert>
#include <string>
#include "TypeSpec.h"

class GoalSymbol {
 public:
  GoalSymbol() = default;
  explicit GoalSymbol(std::string name) : m_name(std::move(name)) {}
  GoalSymbol(std::string name, TypeSpec ts) : m_name(std::move(name)), m_type(std::move(ts)) {
    m_has_type_info = true;
  }

  bool has_type_info() const {
    return m_has_type_info;
  }

  void set_type(TypeSpec ts) {
    if(m_has_type_info) {
      if(ts != m_type) {
        printf("symbol %s %s -> %s", m_name.c_str(), m_type.to_string().c_str(), ts.to_string().c_str());
        assert(false);
      }
    }

    m_has_type_info = true;
    m_type = std::move(ts);
  }

 private:
  std::string m_name;
  TypeSpec m_type;
  bool m_has_type_info = false;
};

#endif  // JAK_DISASSEMBLER_GOALSYMBOL_H
