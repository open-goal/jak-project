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
  void push_value_to_reg(RegisterAccess var,
                         Form* value,
                         bool sequence_point,
                         TypeSpec type,
                         const SetVarInfo& info = {});
  void push_non_seq_reg_to_reg(const RegisterAccess& dst,
                               const RegisterAccess& src,
                               Form* src_as_form,
                               TypeSpec type,
                               const SetVarInfo& info = {});
  void push_value_to_reg_dead(RegisterAccess var,
                              Form* value,
                              bool sequence_point,
                              TypeSpec type,
                              const SetVarInfo& info = {});

  void push_form_element(FormElement* elt, bool sequence_point);
  Form* pop_reg(const RegisterAccess& var,
                const RegSet& barrier,
                const Env& env,
                bool allow_side_effects,
                int begin_idx = -1);
  Form* pop_reg(Register reg,
                const RegSet& barrier,
                const Env& env,
                bool allow_side_effects,
                int begin_idx = -1,
                RegisterAccess* orig_out = nullptr,
                bool* found_orig_out = nullptr);
  FormElement* pop_back(FormPool& pool);
  bool is_single_expression();
  std::vector<FormElement*> rewrite(FormPool& pool, const Env& env) const;
  std::string print(const Env& env);
  bool is_root() const { return m_is_root_stack; }

  struct StackEntry {
    bool active = true;  // should this appear in the output?
    std::optional<RegisterAccess>
        destination;  // what register we are setting (or nullopt if no dest.)
    std::optional<RegisterAccess> non_seq_source;  // source variable, if we are setting var to var.
    Form* source = nullptr;                        // the value we are setting the register to.

    FormElement* elt = nullptr;
    bool sequence_point = false;
    // TP_Type type;
    bool is_compactable = false;

    SetVarInfo set_info;
    TypeSpec set_type;

    std::string print(const Env& env) const;
  };

  // requires consecutive active entries to succeed (can't skip over inactives).
  // it's safe to use pop with the same size or smaller to remove the entries you expect.
  std::optional<std::vector<StackEntry>> try_getting_active_stack_entries(
      const std::vector<bool>& is_set) const {
    if (is_set.size() > m_stack.size()) {
      return {};
    }

    std::vector<StackEntry> entries;
    size_t offset = m_stack.size() - is_set.size();
    for (size_t i = 0; i < is_set.size(); i++) {
      auto& my_entry = m_stack.at(i + offset);
      if (my_entry.active) {
        if (is_set.at(i)) {
          if (!my_entry.destination) {
            return {};
          }
          ASSERT(my_entry.source && !my_entry.elt);
        } else {
          if (my_entry.destination) {
            return {};
          }
          ASSERT(my_entry.elt && !my_entry.source);
        }
        entries.push_back(my_entry);
      } else {
        return {};
      }
    }
    return entries;
  }

  void pop(int count) {
    for (int i = 0; i < count; i++) {
      ASSERT(!m_stack.empty());
      m_stack.pop_back();
    }
  }

  // get the back, skipping inactives
  const StackEntry* active_back() const {
    for (size_t i = m_stack.size(); i-- > 0;) {
      auto& e = m_stack.at(i);
      if (e.active) {
        return &e;
      }
    }
    return nullptr;
  }

  // pop the back, skipping inactives
  void pop_active_back() {
    for (size_t i = m_stack.size(); i-- > 0;) {
      auto& e = m_stack.at(i);
      if (e.active) {
        m_stack.erase(m_stack.begin() + i);
        return;
      }
    }
    ASSERT(false);
  }

  int size() const { return m_stack.size(); }

 private:
  std::vector<StackEntry> m_stack;
  bool m_is_root_stack = false;
};

std::optional<RegisterAccess> rewrite_to_get_var(std::vector<FormElement*>& default_result,
                                                 FormPool& pool,
                                                 const RegisterAccess& var,
                                                 const Env& env);
std::vector<FormElement*> rewrite_to_get_var(FormStack& stack,
                                             FormPool& pool,
                                             const RegisterAccess& var,
                                             const Env& env,
                                             std::optional<RegisterAccess>* used_var = nullptr);
}  // namespace decompiler
