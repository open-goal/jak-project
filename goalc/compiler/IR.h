#ifndef JAK_IR_H
#define JAK_IR_H

#include <string>
#include "CodeGenerator.h"
#include "goalc/regalloc/allocate.h"

class IR {
 public:
  virtual std::string print() = 0;
  virtual RegAllocInstr to_rai() = 0;
  virtual void do_codegen(CodeGenerator* gen) = 0;
};

class IR_Set : public IR {
 public:
  std::string print() override;
  RegAllocInstr to_rai() override;
};

#endif  // JAK_IR_H
