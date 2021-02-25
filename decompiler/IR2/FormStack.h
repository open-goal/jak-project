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
  explicit FormStack(bool is_root_stack) : m_is_root_stack(is_root_stack) {}
  void push_value_to_reg(Variable var,
                         Form* value,
                         bool sequence_point,
                         const SetVarInfo& info = {});
  void push_non_seq_reg_to_reg(const Variable& dst,
                               const Variable& src,
                               Form* src_as_form,
                               const SetVarInfo& info = {});
  void push_value_to_reg_dead(Variable var,
                              Form* value,
                              bool sequence_point,
                              const SetVarInfo& info = {});
  void push_form_element(FormElement* elt, bool sequence_point);
  Form* pop_reg(const Variable& var,
                const RegSet& barrier,
                const Env& env,
                bool allow_side_effects,
                int begin_idx = -1);
  Form* pop_reg(Register reg,
                const RegSet& barrier,
                const Env& env,
                bool allow_side_effects,
                int begin_idx = -1);
  FormElement* pop_back(FormPool& pool);
  Form* unsafe_peek(Register reg, const Env& env);
  bool is_single_expression();
  std::vector<FormElement*> rewrite(FormPool& pool, const Env& env);
  std::string print(const Env& env);
  bool is_root() const { return m_is_root_stack; }

 private:
  struct StackEntry {
    bool active = true;                   // should this appear in the output?
    std::optional<Variable> destination;  // what register we are setting (or nullopt if no dest.)
    std::optional<Variable> non_seq_source;  // source variable, if we are setting var to var.
    Form* source = nullptr;                  // the value we are setting the register to.

    FormElement* elt = nullptr;
    bool sequence_point = false;
    TP_Type type;
    bool is_compactable = false;

    SetVarInfo set_info;

    std::string print(const Env& env) const;
  };
  std::vector<StackEntry> m_stack;
  bool m_is_root_stack = false;
};

void rewrite_to_get_var(std::vector<FormElement*>& default_result,
                        FormPool& pool,
                        const Variable& var,
                        const Env& env);
std::vector<FormElement*> rewrite_to_get_var(FormStack& stack,
                                             FormPool& pool,
                                             const Variable& var,
                                             const Env& env);
}  // namespace decompiler
