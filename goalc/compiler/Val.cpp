#include "Val.h"
#include "Env.h"
#include "IR.h"

/*!
 * Fallback to_gpr if a more optimized one is not provided.
 */
RegVal* Val::to_gpr(Env* fe) {
  // TODO - handle 128-bit stuff here!
  auto rv = to_reg(fe);
  if (rv->ireg().kind == emitter::RegKind::GPR) {
    return rv;
  } else {
    auto re = fe->make_gpr(m_ts);
    fe->emit(std::make_unique<IR_RegSet>(re, rv));
    return re;
  }
}

/*!
 * Fallback to_xmm if a more optimized one is not provided.
 */
RegVal* Val::to_xmm(Env* fe) {
  auto rv = to_reg(fe);
  if (rv->ireg().kind == emitter::RegKind::XMM) {
    return rv;
  } else {
    throw std::runtime_error("Register is not an XMM[0-15] register.");
  }
}

RegVal* RegVal::to_reg(Env* fe) {
  (void)fe;
  return this;
}

RegVal* RegVal::to_gpr(Env* fe) {
  (void)fe;
  if (m_ireg.kind == emitter::RegKind::GPR) {
    return this;
  } else {
    auto re = fe->make_gpr(m_ts);
    fe->emit(std::make_unique<IR_RegSet>(re, this));
    return re;
  }
}

RegVal* RegVal::to_xmm(Env* fe) {
  (void)fe;
  if (m_ireg.kind == emitter::RegKind::XMM) {
    return this;
  } else {
    auto re = fe->make_xmm(m_ts);
    fe->emit(std::make_unique<IR_RegSet>(re, this));
    return re;
  }
}

RegVal* IntegerConstantVal::to_reg(Env* fe) {
  auto rv = fe->make_gpr(m_ts);
  fe->emit(std::make_unique<IR_LoadConstant64>(rv, m_value));
  return rv;
}

RegVal* SymbolVal::to_reg(Env* fe) {
  auto re = fe->make_gpr(m_ts);
  fe->emit(std::make_unique<IR_LoadSymbolPointer>(re, m_name));
  return re;
}

RegVal* SymbolValueVal::to_reg(Env* fe) {
  auto re = fe->make_gpr(m_ts);
  fe->emit(std::make_unique<IR_GetSymbolValue>(re, m_sym, m_sext));
  return re;
}

RegVal* StaticVal::to_reg(Env* fe) {
  auto re = fe->make_gpr(m_ts);
  fe->emit(std::make_unique<IR_StaticVarAddr>(re, obj));
  return re;
}

RegVal* LambdaVal::to_reg(Env* fe) {
  auto re = fe->make_gpr(m_ts);
  assert(func);
  fe->emit(std::make_unique<IR_FunctionAddr>(re, func));
  return re;
}

RegVal* FloatConstantVal::to_reg(Env* fe) {
  auto re = fe->make_xmm(m_ts);
  fe->emit(std::make_unique<IR_StaticVarLoad>(re, m_value));
  return re;
}

RegVal* MemoryOffsetConstantVal::to_reg(Env* fe) {
  auto re = fe->make_gpr(deref_type);
  fe->emit(std::make_unique<IR_LoadConstOffset>(re, offset, base, info));
  return re;
}