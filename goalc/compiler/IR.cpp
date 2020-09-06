#include "IR.h"

IR_Return::IR_Return(const RegVal* return_reg, const RegVal* value) : m_return_reg(return_reg), m_value(value) {}
std::string IR_Return::print() {
  return fmt::format("ret {} {}", m_return_reg->print(), m_value->print());
}

RegAllocInstr IR_Return::to_rai() {
  RegAllocInstr rai;
  rai.write.push_back(m_return_reg->ireg());
  rai.read.push_back(m_value->ireg());
  if(m_value->ireg().kind == m_return_reg->ireg().kind) {
    rai.is_move = true; // only true if we aren't moving from register kind to register kind
  }
  return rai;
}

void IR_Return::add_constraints(std::vector<IRegConstraint>* constraints, int my_id) {
  IRegConstraint c;
  if(dynamic_cast<const None*>(m_return_reg)){
    return;
  }

  c.ireg = m_return_reg->ireg();
  c.instr_idx = my_id;
  c.desired_register = emitter::RAX;
  constraints->push_back(c);
}

IR_LoadConstant64::IR_LoadConstant64(const RegVal* dest, u64 value) : m_dest(dest), m_value(value) {

}

std::string IR_LoadConstant64::print() {
  return fmt::format("mov-ic {}, {}", m_dest->print(), m_value);
}

RegAllocInstr IR_LoadConstant64::to_rai() {
  RegAllocInstr rai;
  rai.write.push_back(m_dest->ireg());
  return rai;
}