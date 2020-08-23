#include "GoalType.h"

void GoalType::set_methods(int n) {
  if (m_method_count_set) {
    if (m_method_count != n) {
      printf("Type %s had %d methods, set_methods tried to change it to %d\n", m_name.c_str(),
             m_method_count, n);
    }
  } else {
    m_method_count = n;
    m_method_count_set = true;
  }
}