#pragma once

#include <vector>
#include "decompiler/IR/IR.h"
#include "decompiler/Disasm/Register.h"
#include "decompiler/util/TP_Type.h"

class ExpressionStack {
 public:
  ExpressionStack() = default;
  void set(Register reg, std::shared_ptr<IR> value);
  std::shared_ptr<IR> get(Register reg);
  bool is_single_expression();
  std::string print(LinkedObjectFile& file);
  std::vector<std::shared_ptr<IR>> get_result();

 private:
  struct StackEntry {
    bool display = true;         // should this appear in the output?
    Register destination;        // what register we are setting
    std::shared_ptr<IR> source;  // the value we are setting the register to.
    // TP_Type type;
    std::string print(LinkedObjectFile& file);
  };
  std::vector<StackEntry> m_stack;

  bool display_stack_empty();
  StackEntry& get_display_stack_top();
};