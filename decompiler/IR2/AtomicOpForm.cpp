#include "AtomicOp.h"
#include "Form.h"

namespace decompiler {

ConditionElement* BranchOp::get_condition_as_form(FormPool& pool) const {
  return m_condition.get_as_form(pool);
}

ConditionElement* IR2_Condition::get_as_form(FormPool& pool) const {
  Form* sources[2] = {nullptr, nullptr};
  int n_sources = get_condition_num_args(m_kind);
  for (int i = 0; i < n_sources; i++) {
    sources[i] = pool.alloc_single_element_form<SimpleAtomElement>(nullptr, m_src[i]);
  }

  return pool.alloc_element<ConditionElement>(m_kind, sources[0], sources[1]);
}

FormElement* SetVarOp::get_as_form(FormPool& pool) const {
  auto source = pool.alloc_single_element_form<SimpleExpressionElement>(nullptr, m_src, m_my_idx);
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
  auto source = pool.alloc_single_element_form<SimpleExpressionElement>(nullptr, m_src, m_my_idx);
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
  if (m_write_regs.empty() && m_call_type_set == true) {
    return call;
  } else if (m_write_regs.size() == 1 || !m_call_type_set) {
    // this is a little scary in the case that type analysis doesn't run and relies on the fact
    // that CallOp falls back to writing v0 in the case where the function type isn't known.
    Variable out_var(VariableMode::WRITE, Register(Reg::GPR, Reg::V0), m_my_idx);
    return pool.alloc_element<SetVarElement>(out_var, pool.alloc_single_form(nullptr, call), true);
  } else {
    throw std::runtime_error("CallOp::get_as_expr not yet implemented");
  }
}

FormElement* ConditionalMoveFalseOp::get_as_form(FormPool& pool) const {
  auto source =
      pool.alloc_single_element_form<SimpleAtomElement>(nullptr, SimpleAtom::make_var(m_src));
  return pool.alloc_element<ConditionalMoveFalseElement>(m_dst, source, m_on_zero);
}

}  // namespace decompiler