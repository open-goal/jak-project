#include "FormStack.h"
#include "Form.h"

namespace decompiler {
std::string FormStack::StackEntry::print(const Env& env) const {
  if (destination.has_value()) {
    assert(source && !elt);
    return fmt::format("d: {} s: {} | {} <- {}", active, sequence_point,
                       destination.value().reg().to_charp(), source->to_string(env));
  } else {
    assert(elt && !source);
    return fmt::format("d: {} s: {} | {}", active, sequence_point, elt->to_string(env));
  }
}

std::string FormStack::print(const Env& env) {
  std::string result;
  for (auto& x : m_stack) {
    result += x.print(env);
    result += '\n';
  }
  return result;
}

void FormStack::push_value_to_reg(Variable var, Form* value, bool sequence_point) {
  StackEntry entry;
  entry.active = true;  // by default, we should display everything!
  entry.sequence_point = sequence_point;
  entry.destination = var;
  entry.source = value;
  m_stack.push_back(entry);
}

bool FormStack::is_single_expression() {
  int count = 0;
  for (auto& e : m_stack) {
    if (e.active) {
      count++;
    }
  }
  return count == 1;
}

void FormStack::push_form_element(FormElement* elt, bool sequence_point) {
  StackEntry entry;
  entry.active = true;
  entry.elt = elt;
  entry.sequence_point = sequence_point;
  m_stack.push_back(entry);
}

Form* FormStack::pop_reg(Register reg) {
  for (size_t i = m_stack.size(); i-- > 0;) {
    auto& entry = m_stack.at(i);
    if (entry.active) {
      if (entry.destination->reg() == reg) {
        entry.active = false;
        assert(entry.source);
        return entry.source;
      } else {
        // we didn't match
        if (entry.sequence_point) {
          // and it's a sequence point! can't look any more back than this.
          return nullptr;
        }
      }
    }
  }
  // we didn't have it...
  return nullptr;
}

Form* FormStack::pop_reg(const Variable& var) {
  return pop_reg(var.reg());
}

std::vector<FormElement*> FormStack::rewrite(FormPool& pool) {
  std::vector<FormElement*> result;

  for (auto& e : m_stack) {
    if (!e.active) {
      continue;
    }

    if (e.destination.has_value()) {
      auto elt = pool.alloc_element<SetVarElement>(*e.destination, e.source, e.sequence_point);
      e.source->parent_element = elt;
      result.push_back(elt);
    } else {
      result.push_back(e.elt);
    }
  }
  return result;
}

std::vector<FormElement*> FormStack::rewrite_to_get_var(FormPool& pool,
                                                        const Variable& var,
                                                        const Env&) {
  // first, rewrite as normal.
  auto default_result = rewrite(pool);

  // try a few different ways to "naturally" rewrite this so the value of the form is the
  // value in the given register.

  auto last_op_as_set = dynamic_cast<SetVarElement*>(default_result.back());
  if (last_op_as_set && last_op_as_set->dst().reg() == var.reg()) {
    default_result.pop_back();
    for (auto form : last_op_as_set->src()->elts()) {
      form->parent_form = nullptr;  // will get set later, this makes it obvious if I forget.
      default_result.push_back(form);
    }
    return default_result;
  } else {
    default_result.push_back(pool.alloc_element<SimpleAtomElement>(SimpleAtom::make_var(var)));
    return default_result;
  }
}
}  // namespace decompiler