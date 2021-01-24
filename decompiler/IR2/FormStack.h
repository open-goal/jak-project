#pragma once

#include <optional>
#include "decompiler/Disasm/Register.h"
#include "decompiler/IR2/AtomicOp.h"

namespace decompiler {
class Form;
/*!
 * A FormStack is used to track partial expressions when rebuilding the tree structure of
 * GOAL code. Linear sequences of operations are added onto the expression stack.
 */
class FormStack {
 public:
  FormStack() = default;
  void push_value_to_reg(Variable var, Form* value, bool sequence_point);
  void push_form_element(FormElement* elt, bool sequence_point);
  Form* pop_reg(const Variable& var);
  bool is_single_expression();
  std::vector<FormElement*> rewrite(FormPool& pool);
  std::vector<FormElement*> rewrite_to_get_reg(FormPool& pool, Register reg, const Env& env);
  std::string print(const Env& env);

 private:
  struct StackEntry {
    bool active = true;                   // should this appear in the output?
    std::optional<Variable> destination;  // what register we are setting (or nullopt if no dest.)
    Form* source = nullptr;               // the value we are setting the register to.

    FormElement* elt = nullptr;
    bool sequence_point = false;
    TP_Type type;
    std::string print(const Env& env) const;
  };
  std::vector<StackEntry> m_stack;
};
}  // namespace decompiler
