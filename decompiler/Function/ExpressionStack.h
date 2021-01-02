#pragma once

#include <vector>
#include <optional>
#include "decompiler/IR/IR.h"
#include "decompiler/Disasm/Register.h"
#include "decompiler/util/TP_Type.h"

/*!
 * An ExpressionStack is used to track partial expressions when rebuilding the tree structure of
 * GOAL code. Linear sequences of operations are added onto the expression stack.
 */
class ExpressionStack {
 public:
  ExpressionStack() = default;
  void set(Register reg, std::shared_ptr<IR> value, bool sequence_point);
  void add_no_set(std::shared_ptr<IR> value, bool sequence_point);
  std::shared_ptr<IR> get(Register reg);
  bool is_single_expression();
  std::string print(LinkedObjectFile& file);
  std::vector<std::shared_ptr<IR>> get_result();

 private:
  struct StackEntry {
    bool display = true;                  // should this appear in the output?
    std::optional<Register> destination;  // what register we are setting (or nullopt if no dest.)
    std::shared_ptr<IR> source;           // the value we are setting the register to.
    bool sequence_point = false;
    // TP_Type type;
    std::string print(LinkedObjectFile& file);
  };
  std::vector<StackEntry> m_stack;

  bool display_stack_empty();
  StackEntry& get_display_stack_top();
};