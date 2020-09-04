#ifndef JAK_DISASSEMBLER_GOALTYPE_H
#define JAK_DISASSEMBLER_GOALTYPE_H

#include <string>

class GoalType {
 public:
  GoalType() = default;
  GoalType(std::string name) : m_name(std::move(name)) {}
  bool has_info() const { return m_has_info; }

  bool has_method_count() const { return m_method_count_set; }

  void set_methods(int n);

 private:
  std::string m_name;
  bool m_has_info = false;
  bool m_method_count_set = false;
  int m_method_count = -1;
};

#endif  // JAK_DISASSEMBLER_GOALTYPE_H
