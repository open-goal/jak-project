#include "AtomicOp.h"
#include "Form.h"

namespace decompiler {

ConditionElement* IR2_Condition::get_as_form(FormPool& pool) const {
  Form* sources[2] = {nullptr, nullptr};
  int n_sources = get_condition_num_args(m_kind);
  for (int i = 0; i < n_sources; i++) {
    sources[i] = pool.alloc_single_element_form<SimpleAtomElement>(nullptr, m_src[i]);
  }

  return pool.alloc_element<ConditionElement>(m_kind, sources[0], sources[1]);
}

FormElement* SetVarOp::get_as_form(FormPool& pool) const {
  auto source = pool.alloc_single_element_form<SimpleExpressionElement>(nullptr, m_src);
  return pool.alloc_element<SetVarElement>(m_dst, source, is_sequence_point());
}

FormElement* AsmOp::get_as_form(FormPool& pool) const {
  return pool.alloc_element<AtomicOpElement>(this);
}

FormElement* SetVarConditionOp::get_as_form(FormPool& pool) const {
  return pool.alloc_element<SetVarElement>(
      m_dst, pool.alloc_single_form(nullptr, m_condition.get_as_form(pool)), is_sequence_point());
}

FormElement* StoreOp::get_as_form(FormPool& pool) const {
  return pool.alloc_element<StoreElement>(this);
}

FormElement* LoadVarOp::get_as_form(FormPool& pool) const {
  auto source = pool.alloc_single_element_form<SimpleExpressionElement>(nullptr, m_src);
  auto load = pool.alloc_single_element_form<LoadSourceElement>(nullptr, source, m_size, m_kind);
  return pool.alloc_element<SetVarElement>(m_dst, load, true);
}

FormElement* BranchOp::get_as_form(FormPool& pool) const {
  return pool.alloc_element<BranchElement>(this);
}

FormElement* SpecialOp::get_as_form(FormPool& pool) const {
  return pool.alloc_element<AtomicOpElement>(this);
}

FormElement* CallOp::get_as_form(FormPool& pool) const {
  auto call = pool.alloc_element<FunctionCallElement>(this);
  if (m_write_regs.empty()) {
    return call;
  } else if (m_write_regs.size() == 1) {
    throw std::runtime_error("CallOp::get_as_expr not yet implemented");
  } else {
    throw std::runtime_error("CallOp::get_as_expr not yet implemented");
  }
}

FormElement* ConditionalMoveFalseOp::get_as_form(FormPool& pool) const {
  throw std::runtime_error("ConditionalMoveFalseOp::get_as_expr is not yet implemented");
}

}  // namespace decompiler