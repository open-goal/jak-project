#include "Val.h"

#include "Env.h"
#include "IR.h"

#include "third-party/fmt/core.h"

/*!
 * Fallback to_gpr if a more optimized one is not provided.
 */
RegVal* Val::to_gpr(const goos::Object& form, Env* fe) {
  auto rv = to_reg(form, fe);
  if (rv->ireg().reg_class == RegClass::GPR_64) {
    return rv;
  } else {
    auto re = fe->make_gpr(coerce_to_reg_type(m_ts));
    fe->emit(form, std::make_unique<IR_RegSet>(re, rv));
    return re;
  }
}

/*!
 * Fallback to_fpr if a more optimized one is not provided.
 */
RegVal* Val::to_fpr(const goos::Object& form, Env* fe) {
  auto rv = to_reg(form, fe);
  if (rv->ireg().reg_class == RegClass::FLOAT) {
    return rv;
  } else {
    auto re = fe->make_fpr(coerce_to_reg_type(m_ts));
    fe->emit(form, std::make_unique<IR_RegSet>(re, rv));
    return re;
  }
}

/*!
 * Fallback to_xmm128 if a more optimized one is not provided.
 */
RegVal* Val::to_xmm128(const goos::Object& form, Env* fe) {
  auto rv = to_reg(form, fe);
  if (rv->ireg().reg_class == RegClass::INT_128 || rv->ireg().reg_class == RegClass::VECTOR_FLOAT) {
    return rv;
  } else {
    auto re = fe->make_ireg(coerce_to_reg_type(m_ts), RegClass::INT_128);
    fe->emit(form, std::make_unique<IR_RegSet>(re, rv));
    return re;
  }
}

RegVal* RegVal::to_reg(const goos::Object& /*form*/, Env* fe) {
  (void)fe;
  return this;
}

RegVal* RegVal::to_gpr(const goos::Object& form, Env* fe) {
  if (m_ireg.reg_class == RegClass::GPR_64) {
    return this;
  } else {
    auto re = fe->make_gpr(coerce_to_reg_type(m_ts));
    fe->emit(form, std::make_unique<IR_RegSet>(re, this));
    return re;
  }
}

RegVal* RegVal::to_fpr(const goos::Object& form, Env* fe) {
  if (m_ireg.reg_class == RegClass::FLOAT) {
    return this;
  } else {
    auto re = fe->make_fpr(coerce_to_reg_type(m_ts));
    fe->emit(form, std::make_unique<IR_RegSet>(re, this));
    return re;
  }
}

RegVal* RegVal::to_xmm128(const goos::Object& form, Env* fe) {
  if (m_ireg.reg_class == RegClass::INT_128 || m_ireg.reg_class == RegClass::VECTOR_FLOAT) {
    return this;
  } else {
    auto re = fe->make_ireg(coerce_to_reg_type(m_ts), RegClass::INT_128);
    fe->emit(form, std::make_unique<IR_RegSet>(re, this));
    return re;
  }
}

void RegVal::set_rlet_constraint(emitter::Register reg) {
  m_rlet_constraint = reg;
}

const std::optional<emitter::Register>& RegVal::rlet_constraint() const {
  return m_rlet_constraint;
}

RegVal* IntegerConstantVal::to_reg(const goos::Object& form, Env* fe) {
  if (m_value.uses_gpr()) {
    auto rv = fe->make_gpr(coerce_to_reg_type(m_ts));
    fe->emit(form, std::make_unique<IR_LoadConstant64>(rv, m_value.value_64()));
    return rv;
  } else {
    auto rv = fe->make_ireg(m_ts, RegClass::INT_128);
    auto gpr = fe->make_gpr(TypeSpec("object"));
    auto xmm_temp = fe->make_ireg(TypeSpec("object"), RegClass::INT_128);

    fe->emit_ir<IR_LoadConstant64>(form, gpr, m_value.value_128_lo());
    fe->emit_ir<IR_RegSet>(form, xmm_temp, gpr);
    fe->emit_ir<IR_LoadConstant64>(form, gpr, m_value.value_128_hi());
    fe->emit_ir<IR_RegSet>(form, rv, gpr);
    fe->emit_ir<IR_Int128Math3Asm>(form, true, rv, rv, xmm_temp, IR_Int128Math3Asm::Kind::PCPYLD);

    return rv;
  }
}

RegVal* IntegerConstantVal::to_xmm128(const goos::Object& form, Env* fe) {
  if (m_value.is_zero()) {
    // if we are a constant 0, can use XOR
    auto rv = fe->make_ireg(m_ts, RegClass::INT_128);
    fe->emit_ir<IR_Int128Math3Asm>(form, true, rv, rv, rv, IR_Int128Math3Asm::Kind::PXOR);
    return rv;
  } else {
    // not zero. fall back to the normal implementation.
    auto rv = to_reg(form, fe);
    if (rv->ireg().reg_class == RegClass::INT_128 ||
        rv->ireg().reg_class == RegClass::VECTOR_FLOAT) {
      return rv;
    } else {
      // but we got only an integer, need to promote. we're a constant, so this is safe.
      auto re = fe->make_ireg(coerce_to_reg_type(m_ts), RegClass::INT_128);
      fe->emit(form, std::make_unique<IR_RegSet>(re, rv));
      return re;
    }
  }
}

RegVal* SymbolVal::to_reg(const goos::Object& form, Env* fe) {
  auto re = fe->make_gpr(coerce_to_reg_type(m_ts));
  fe->emit(form, std::make_unique<IR_LoadSymbolPointer>(re, m_name));
  return re;
}

RegVal* SymbolValueVal::to_reg(const goos::Object& form, Env* fe) {
  auto re = fe->make_gpr(coerce_to_reg_type(m_ts));
  fe->emit(form, std::make_unique<IR_GetSymbolValue>(re, m_sym, m_sext));
  return re;
}

RegVal* StaticVal::to_reg(const goos::Object& form, Env* fe) {
  auto re = fe->make_gpr(coerce_to_reg_type(m_ts));
  fe->emit(form, std::make_unique<IR_StaticVarAddr>(re, obj));
  return re;
}

RegVal* LambdaVal::to_reg(const goos::Object& form, Env* fe) {
  auto re = fe->make_gpr(coerce_to_reg_type(m_ts));
  ASSERT(func);
  fe->emit(form, std::make_unique<IR_FunctionAddr>(re, func));
  return re;
}

RegVal* InlinedLambdaVal::to_reg(const goos::Object&, Env*) {
  throw std::runtime_error("Cannot put InlinedLambdaVal in a register.");
  return nullptr;
}

RegVal* FloatConstantVal::to_reg(const goos::Object& form, Env* fe) {
  auto re = fe->make_fpr(coerce_to_reg_type(m_ts));
  fe->emit(form, std::make_unique<IR_StaticVarLoad>(re, m_value));
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

RegVal* MemoryOffsetConstantVal::to_reg(const goos::Object& form, Env* fe) {
  auto re = fe->make_gpr(coerce_to_reg_type(m_ts));
  s64 final_offset;
  auto final_base = get_constant_offset_and_base(this, &final_offset);

  if (final_offset == 0) {
    fe->emit_ir<IR_RegSet>(form, re, final_base->to_gpr(form, fe));
  } else {
    fe->emit(form, std::make_unique<IR_LoadConstant64>(re, int64_t(final_offset)));
    fe->emit(form, std::make_unique<IR_IntegerMath>(IntegerMathKind::ADD_64, re,
                                                    final_base->to_gpr(form, fe)));
  }

  return re;
}

RegVal* MemoryOffsetVal::to_reg(const goos::Object& form, Env* fe) {
  auto re = fe->make_gpr(coerce_to_reg_type(m_ts));
  fe->emit(form, std::make_unique<IR_RegSet>(re, offset->to_gpr(form, fe)));
  fe->emit(form,
           std::make_unique<IR_IntegerMath>(IntegerMathKind::ADD_64, re, base->to_gpr(form, fe)));
  return re;
}

RegVal* MemoryDerefVal::to_reg(const goos::Object& form, Env* fe) {
  auto re = fe->make_ireg(coerce_to_reg_type(m_ts), info.reg);
  auto base_as_co = dynamic_cast<MemoryOffsetConstantVal*>(base);
  if (base_as_co) {
    s64 offset;
    auto final_base = get_constant_offset_and_base(base_as_co, &offset);
    fe->emit_ir<IR_LoadConstOffset>(form, re, (int)offset, final_base->to_gpr(form, fe), info);
  } else {
    auto addr = base->to_gpr(form, fe);
    fe->emit(form, std::make_unique<IR_LoadConstOffset>(re, 0, addr, info));
  }
  return re;
}

RegVal* MemoryDerefVal::to_fpr(const goos::Object& form, Env* fe) {
  auto base_as_co = dynamic_cast<MemoryOffsetConstantVal*>(base);
  auto re = fe->make_fpr(coerce_to_reg_type(m_ts));
  if (base_as_co) {
    s64 offset;
    auto final_base = get_constant_offset_and_base(base_as_co, &offset);
    fe->emit_ir<IR_LoadConstOffset>(form, re, offset, final_base->to_gpr(form, fe), info);
  } else {
    auto addr = base->to_gpr(form, fe);
    fe->emit(form, std::make_unique<IR_LoadConstOffset>(re, 0, addr, info));
  }
  return re;
}

RegVal* AliasVal::to_reg(const goos::Object& form, Env* fe) {
  auto as_old_type = base->to_reg(form, fe);
  auto result = fe->make_ireg(m_ts, as_old_type->ireg().reg_class);
  fe->emit(form, std::make_unique<IR_RegSet>(result, as_old_type));
  return result;
}

RegVal* AliasVal::to_xmm128(const goos::Object& form, Env* fe) {
  auto as_old_type = base->to_xmm128(form, fe);
  auto result = fe->make_ireg(m_ts, as_old_type->ireg().reg_class);
  fe->emit(form, std::make_unique<IR_RegSet>(result, as_old_type));
  return result;
}

std::string PairEntryVal::print() const {
  if (is_car) {
    return fmt::format("[car of {}]", base->print());
  } else {
    return fmt::format("[cdr of {}]", base->print());
  }
}

RegVal* PairEntryVal::to_reg(const goos::Object& form, Env* fe) {
  int offset = is_car ? -2 : 2;
  auto re = fe->make_gpr(coerce_to_reg_type(m_ts));
  MemLoadInfo info;
  info.reg = RegClass::GPR_64;
  info.sign_extend = true;
  info.size = 4;
  fe->emit(form, std::make_unique<IR_LoadConstOffset>(re, offset, base->to_gpr(form, fe), info));
  return re;
}

RegVal* StackVarAddrVal::to_reg(const goos::Object& form, Env* fe) {
  auto re = fe->make_gpr(coerce_to_reg_type(m_ts));
  fe->emit(form, std::make_unique<IR_GetStackAddr>(re, m_slot));
  return re;
}

std::string BitFieldVal::print() const {
  return fmt::format("[bitfield sz {} off {} sx {} of {} 128? {}]", m_size, m_offset, m_sign_extend,
                     m_parent->print(), m_use_128);
}

RegVal* BitFieldVal::to_reg(const goos::Object& form, Env* env) {
  int start_bit = -1;
  auto fe = env->function_env();
  RegVal* result = fe->make_ireg(coerce_to_reg_type(m_ts), RegClass::GPR_64);

  // this first step gets the right 64-bits into a GPR that is also used as the result.
  if (m_offset < 64) {
    // accessing in the lower 64 bits, we can just get the value in a GPR.
    start_bit = m_offset;
    RegVal* gpr = m_parent->to_gpr(form, env);
    env->emit(form, std::make_unique<IR_RegSet>(result, gpr));
  } else {
    // we need to get the value as a 128-bit integer
    auto xmm = m_parent->to_reg(form, env);
    ASSERT(xmm->ireg().reg_class == RegClass::INT_128);
    auto xmm_temp = fe->make_ireg(TypeSpec("object"), RegClass::INT_128);
    env->emit_ir<IR_Int128Math3Asm>(form, true, xmm_temp, xmm, xmm,
                                    IR_Int128Math3Asm::Kind::PCPYUD);
    env->emit_ir<IR_RegSet>(form, result, xmm_temp);
    start_bit = m_offset - 64;
  }

  // this second step does up to 2 shifts to extract the bitfield and sign extend as needed.
  int end_bit = start_bit + m_size;
  ASSERT(end_bit <= 64);  // should be checked by the type system.
  int epad = 64 - end_bit;
  ASSERT(epad >= 0);
  int spad = start_bit;

  // shift left as much as possible to kill upper bits
  if (epad > 0) {
    env->emit(form, std::make_unique<IR_IntegerMath>(IntegerMathKind::SHL_64, result, epad));
  }

  int next_shift = epad + spad;
  ASSERT(next_shift + m_size == 64);
  ASSERT(next_shift >= 0);

  if (next_shift > 0) {
    if (m_sign_extend) {
      env->emit(form,
                std::make_unique<IR_IntegerMath>(IntegerMathKind::SAR_64, result, next_shift));
    } else {
      env->emit(form,
                std::make_unique<IR_IntegerMath>(IntegerMathKind::SHR_64, result, next_shift));
    }
  }

  return result;
}