#include "third-party/fmt/core.h"
#include "Val.h"
#include "Env.h"
#include "IR.h"

/*!
 * Fallback to_gpr if a more optimized one is not provided.
 */
RegVal* Val::to_gpr(Env* fe) {
  // TODO - handle 128-bit stuff here!
  auto rv = to_reg(fe);
  if (rv->ireg().reg_class == RegClass::GPR_64) {
    return rv;
  } else {
    auto re = fe->make_gpr(coerce_to_reg_type(m_ts));
    fe->emit(std::make_unique<IR_RegSet>(re, rv));
    return re;
  }
}

/*!
 * Fallback to_fpr if a more optimized one is not provided.
 */
RegVal* Val::to_fpr(Env* fe) {
  auto rv = to_reg(fe);
  if (rv->ireg().reg_class == RegClass::FLOAT) {
    return rv;
  } else {
    auto re = fe->make_fpr(coerce_to_reg_type(m_ts));
    fe->emit(std::make_unique<IR_RegSet>(re, rv));
    return re;
  }
}

/*!
 * Fallback to_xmm128 if a more optimized one is not provided.
 */
RegVal* Val::to_xmm128(Env* fe) {
  auto rv = to_reg(fe);
  if (rv->ireg().reg_class == RegClass::INT_128 || rv->ireg().reg_class == RegClass::VECTOR_FLOAT) {
    return rv;
  } else {
    auto re = fe->make_ireg(coerce_to_reg_type(m_ts), RegClass::INT_128);
    fe->emit(std::make_unique<IR_RegSet>(re, rv));
    return re;
  }
}

RegVal* RegVal::to_reg(Env* fe) {
  (void)fe;
  return this;
}

RegVal* RegVal::to_gpr(Env* fe) {
  if (m_ireg.reg_class == RegClass::GPR_64) {
    return this;
  } else {
    auto re = fe->make_gpr(coerce_to_reg_type(m_ts));
    fe->emit(std::make_unique<IR_RegSet>(re, this));
    return re;
  }
}

RegVal* RegVal::to_fpr(Env* fe) {
  if (m_ireg.reg_class == RegClass::FLOAT) {
    return this;
  } else {
    auto re = fe->make_fpr(coerce_to_reg_type(m_ts));
    fe->emit(std::make_unique<IR_RegSet>(re, this));
    return re;
  }
}

RegVal* RegVal::to_xmm128(Env* fe) {
  if (m_ireg.reg_class == RegClass::INT_128 || m_ireg.reg_class == RegClass::VECTOR_FLOAT) {
    return this;
  } else {
    auto re = fe->make_ireg(coerce_to_reg_type(m_ts), RegClass::INT_128);
    fe->emit(std::make_unique<IR_RegSet>(re, this));
    return re;
  }
}

void RegVal::set_rlet_constraint(emitter::Register reg) {
  m_rlet_constraint = reg;
}

const std::optional<emitter::Register>& RegVal::rlet_constraint() const {
  return m_rlet_constraint;
}

RegVal* IntegerConstantVal::to_reg(Env* fe) {
  auto rv = fe->make_gpr(coerce_to_reg_type(m_ts));
  fe->emit(std::make_unique<IR_LoadConstant64>(rv, m_value));
  return rv;
}

RegVal* SymbolVal::to_reg(Env* fe) {
  auto re = fe->make_gpr(coerce_to_reg_type(m_ts));
  fe->emit(std::make_unique<IR_LoadSymbolPointer>(re, m_name));
  return re;
}

RegVal* SymbolValueVal::to_reg(Env* fe) {
  auto re = fe->make_gpr(coerce_to_reg_type(m_ts));
  fe->emit(std::make_unique<IR_GetSymbolValue>(re, m_sym, m_sext));
  return re;
}

RegVal* StaticVal::to_reg(Env* fe) {
  auto re = fe->make_gpr(coerce_to_reg_type(m_ts));
  fe->emit(std::make_unique<IR_StaticVarAddr>(re, obj));
  return re;
}

RegVal* LambdaVal::to_reg(Env* fe) {
  auto re = fe->make_gpr(coerce_to_reg_type(m_ts));
  assert(func);
  fe->emit(std::make_unique<IR_FunctionAddr>(re, func));
  return re;
}

RegVal* InlinedLambdaVal::to_reg(Env* fe) {
  throw std::runtime_error("Cannot put InlinedLambdaVal in a register.");
  return lv->to_reg(fe);
}

RegVal* FloatConstantVal::to_reg(Env* fe) {
  auto re = fe->make_fpr(coerce_to_reg_type(m_ts));
  fe->emit(std::make_unique<IR_StaticVarLoad>(re, m_value));
  return re;
}

namespace {
/*!
 * Constant propagate nested MemoryOffsetConstantVal's to get a single base + offset.
 */
Val* get_constant_offset_and_base(MemoryOffsetConstantVal* in, int64_t* offset_out) {
  Val* next_base = in->base;
  s64 total_offset = in->offset;
  while (dynamic_cast<MemoryOffsetConstantVal*>(next_base)) {
    auto bac = dynamic_cast<MemoryOffsetConstantVal*>(next_base);
    total_offset += bac->offset;
    next_base = bac->base;
  }
  *offset_out = total_offset;
  return next_base;
}
}  // namespace

RegVal* MemoryOffsetConstantVal::to_reg(Env* fe) {
  auto re = fe->make_gpr(coerce_to_reg_type(m_ts));
  s64 final_offset;
  auto final_base = get_constant_offset_and_base(this, &final_offset);

  if (final_offset == 0) {
    fe->emit_ir<IR_RegSet>(re, final_base->to_gpr(fe));
  } else {
    fe->emit(std::make_unique<IR_LoadConstant64>(re, int64_t(final_offset)));
    fe->emit(std::make_unique<IR_IntegerMath>(IntegerMathKind::ADD_64, re, final_base->to_gpr(fe)));
  }

  return re;
}

RegVal* MemoryOffsetVal::to_reg(Env* fe) {
  auto re = fe->make_gpr(coerce_to_reg_type(m_ts));
  fe->emit(std::make_unique<IR_RegSet>(re, offset->to_gpr(fe)));
  fe->emit(std::make_unique<IR_IntegerMath>(IntegerMathKind::ADD_64, re, base->to_gpr(fe)));
  return re;
}

RegVal* MemoryDerefVal::to_reg(Env* fe) {
  auto re = fe->make_ireg(coerce_to_reg_type(m_ts), info.reg);
  auto base_as_co = dynamic_cast<MemoryOffsetConstantVal*>(base);
  if (base_as_co) {
    s64 offset;
    auto final_base = get_constant_offset_and_base(base_as_co, &offset);
    fe->emit_ir<IR_LoadConstOffset>(re, (int)offset, final_base->to_gpr(fe), info);
  } else {
    auto addr = base->to_gpr(fe);
    fe->emit(std::make_unique<IR_LoadConstOffset>(re, 0, addr, info));
  }
  return re;
}

RegVal* MemoryDerefVal::to_fpr(Env* fe) {
  auto base_as_co = dynamic_cast<MemoryOffsetConstantVal*>(base);
  auto re = fe->make_fpr(coerce_to_reg_type(m_ts));
  if (base_as_co) {
    s64 offset;
    auto final_base = get_constant_offset_and_base(base_as_co, &offset);
    fe->emit_ir<IR_LoadConstOffset>(re, offset, final_base->to_gpr(fe), info);
  } else {
    auto addr = base->to_gpr(fe);
    fe->emit(std::make_unique<IR_LoadConstOffset>(re, 0, addr, info));
  }
  return re;
}

RegVal* AliasVal::to_reg(Env* fe) {
  auto as_old_type = base->to_reg(fe);
  auto result = fe->make_ireg(m_ts, as_old_type->ireg().reg_class);
  fe->emit(std::make_unique<IR_RegSet>(result, as_old_type));
  return result;
}

std::string PairEntryVal::print() const {
  if (is_car) {
    return fmt::format("[car of {}]", base->print());
  } else {
    return fmt::format("[cdr of {}]", base->print());
  }
}

RegVal* PairEntryVal::to_reg(Env* fe) {
  int offset = is_car ? -2 : 2;
  auto re = fe->make_gpr(coerce_to_reg_type(m_ts));
  MemLoadInfo info;
  info.reg = RegClass::GPR_64;
  info.sign_extend = true;
  info.size = 4;
  fe->emit(std::make_unique<IR_LoadConstOffset>(re, offset, base->to_gpr(fe), info));
  return re;
}

RegVal* StackVarAddrVal::to_reg(Env* fe) {
  auto re = fe->make_gpr(coerce_to_reg_type(m_ts));
  fe->emit(std::make_unique<IR_GetStackAddr>(re, m_slot));
  return re;
}

std::string BitFieldVal::print() const {
  return fmt::format("[bitfield sz {} off {} sx {} of {}]", m_size, m_offset, m_sign_extend,
                     m_parent->print());
}

RegVal* BitFieldVal::to_reg(Env* env) {
  // first get the parent value
  auto parent_reg = m_parent->to_gpr(env);

  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  auto result = fe->make_ireg(coerce_to_reg_type(m_ts), RegClass::GPR_64);
  env->emit(std::make_unique<IR_RegSet>(result, parent_reg));

  int start_bit = m_offset;
  int end_bit = m_offset + m_size;
  int epad = 64 - end_bit;
  assert(epad >= 0);
  int spad = start_bit;

  // shift left as much as possible to kill upper bits
  if (epad > 0) {
    env->emit(std::make_unique<IR_IntegerMath>(IntegerMathKind::SHL_64, result, epad));
  }

  int next_shift = epad + spad;
  assert(next_shift + m_size == 64);
  assert(next_shift >= 0);

  if (next_shift > 0) {
    if (m_sign_extend) {
      env->emit(std::make_unique<IR_IntegerMath>(IntegerMathKind::SAR_64, result, next_shift));
    } else {
      env->emit(std::make_unique<IR_IntegerMath>(IntegerMathKind::SHR_64, result, next_shift));
    }
  }

  return result;
}