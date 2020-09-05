#ifndef JAK_COMPILER_H
#define JAK_COMPILER_H

#include "common/type_system/TypeSystem.h"

class Compiler {
 public:
  Compiler();
  ~Compiler();
  void execute_repl();

 private:
  void init_logger();

  TypeSystem m_ts;
};

#endif  // JAK_COMPILER_H
