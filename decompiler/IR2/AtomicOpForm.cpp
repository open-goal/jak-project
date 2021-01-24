#include "AtomicOp.h"
#include "Form.h"
#include "common/type_system/TypeSystem.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"

namespace decompiler {

namespace {
RegClass get_reg_kind(const Register& r) {
  switch (r.get_kind()) {
    case Reg::GPR:
      return RegClass::GPR_64;
    case Reg::FPR:
      return RegClass::FLOAT;
    default:
      assert(false);
  }
}

DerefToken to_token(FieldReverseLookupOutput::Token in) {
  switch (in.kind) {
    case FieldReverseLookupOutput::Token::Kind::FIELD:
      return DerefToken::make_field_name(in.name);
    case FieldReverseLookupOutput::Token::Kind::CONSTANT_IDX:
      return DerefToken::make_int_constant(in.idx);
    default:
      assert(false);
  }
}
}  // namespace

ConditionElement* BranchOp::get_condition_as_form(FormPool& pool, const Env& env) const {
  return m_condition.get_as_form(pool, env, m_my_idx);
}

ConditionElement* IR2_Condition::get_as_form(FormPool& pool, const Env& env, int my_idx) const {
  RegSet consumed;
  if (env.has_reg_use()) {
    consumed = env.reg_use().op.at(my_idx).consumes;
  }

  std::optional<SimpleAtom> vars[2];
  for (int i = 0; i < get_condition_num_args(m_kind); i++) {
    vars[i] = m_src[i];
  }
  return pool.alloc_element<ConditionElement>(m_kind, vars[0], vars[1], consumed);
}

FormElement* SetVarOp::get_as_form(FormPool& pool, const Env&) const {
  auto source = pool.alloc_single_element_form<SimpleExpressionElement>(nullptr, m_src, m_my_idx);
  return pool.alloc_element<SetVarElement>(m_dst, source, is_sequence_point());
}

FormElement* AsmOp::get_as_form(FormPool& pool, const Env&) const {
  return pool.alloc_element<AtomicOpElement>(this);
}

FormElement* SetVarConditionOp::get_as_form(FormPool& pool, const Env& env) const {
  return pool.alloc_element<SetVarElement>(
      m_dst, pool.alloc_single_form(nullptr, m_condition.get_as_form(pool, env, m_my_idx)),
      is_sequence_point());
}

FormElement* StoreOp::get_as_form(FormPool& pool, const Env&) const {
  return pool.alloc_element<StoreElement>(this);
}

FormElement* LoadVarOp::get_as_form(FormPool& pool, const Env& env) const {
  if (env.has_type_analysis()) {
    IR2_RegOffset ro;
    if (get_as_reg_offset(m_src, &ro)) {
      auto& input_type = env.get_types_before_op(m_my_idx).get(ro.reg);

      // todo basic method
      // todo structure method
      // todo pointer
      // todo product trick
      // todo type of basic fallback
      // todo dynamic method id access

      // Assume we're accessing a field of an object.
      FieldReverseLookupInput rd_in;
      DerefKind dk;
      dk.is_store = false;
      dk.reg_kind = get_reg_kind(ro.reg);
      dk.sign_extend = m_kind == Kind::SIGNED;
      dk.size = m_size;
      rd_in.deref = dk;
      rd_in.base_type = input_type.typespec();
      rd_in.stride = 0;
      rd_in.offset = ro.offset;
      auto rd = env.dts->ts.reverse_field_lookup(rd_in);

      // todo, error here?

      if (rd.success) {
        auto source = pool.alloc_single_element_form<SimpleExpressionElement>(
            nullptr, SimpleAtom::make_var(ro.var).as_expr(), m_my_idx);
        std::vector<DerefToken> tokens;
        for (auto& x : rd.tokens) {
          tokens.push_back(to_token(x));
        }
        auto load =
            pool.alloc_single_element_form<DerefElement>(nullptr, source, rd.addr_of, tokens);
        return pool.alloc_element<SetVarElement>(m_dst, load, true);
      }

      // todo, try as pair
    }
  }

  auto source = pool.alloc_single_element_form<SimpleExpressionElement>(nullptr, m_src, m_my_idx);
  auto load = pool.alloc_single_element_form<LoadSourceElement>(nullptr, source, m_size, m_kind);
  return pool.alloc_element<SetVarElement>(m_dst, load, true);
}

FormElement* BranchOp::get_as_form(FormPool& pool, const Env&) const {
  return pool.alloc_element<BranchElement>(this);
}

FormElement* SpecialOp::get_as_form(FormPool& pool, const Env&) const {
  return pool.alloc_element<AtomicOpElement>(this);
}

FormElement* CallOp::get_as_form(FormPool& pool, const Env& env) const {
  auto call = pool.alloc_element<FunctionCallElement>(this);
  if (m_write_regs.empty() && m_call_type_set == true) {
    return call;
  } else if (m_write_regs.size() == 1 || !m_call_type_set) {
    if (env.has_reg_use() && m_write_regs.size() == 1) {
      auto& written_and_unused = env.reg_use().op.at(m_my_idx).written_and_unused;
      if (written_and_unused.find(m_write_regs.front()) != written_and_unused.end()) {
        return call;
      }
    }

    // this is a little scary in the case that type analysis doesn't run and relies on the fact
    // that CallOp falls back to writing v0 in the case where the function type isn't known.
    Variable out_var(VariableMode::WRITE, Register(Reg::GPR, Reg::V0), m_my_idx);
    return pool.alloc_element<SetVarElement>(out_var, pool.alloc_single_form(nullptr, call), true);
  } else {
    throw std::runtime_error("CallOp::get_as_expr not yet implemented");
  }
}

FormElement* ConditionalMoveFalseOp::get_as_form(FormPool& pool, const Env&) const {
  auto source =
      pool.alloc_single_element_form<SimpleAtomElement>(nullptr, SimpleAtom::make_var(m_src));
  return pool.alloc_element<ConditionalMoveFalseElement>(m_dst, source, m_on_zero);
}

}  // namespace decompiler