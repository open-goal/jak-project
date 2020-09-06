#include "IR.h"
#include "goalc/emitter/IGen.h"

using namespace emitter;

namespace {
Register get_reg(const RegVal* rv, const AllocationResult& allocs, emitter::IR_Record irec) {
  auto& ass = allocs.ass_as_ranges.at(rv->ireg().id).get(irec.ir_id);
  assert(ass.kind == Assignment::Kind::REGISTER);
  return ass.reg;
}
}  // namespace

IR_Return::IR_Return(const RegVal* return_reg, const RegVal* value)
    : m_return_reg(return_reg), m_value(value) {}
std::string IR_Return::print() {
  return fmt::format("ret {} {}", m_return_reg->print(), m_value->print());
}

RegAllocInstr IR_Return::to_rai() {
  RegAllocInstr rai;
  rai.write.push_back(m_return_reg->ireg());
  rai.read.push_back(m_value->ireg());
  if (m_value->ireg().kind == m_return_reg->ireg().kind) {
    rai.is_move = true;  // only true if we aren't moving from register kind to register kind
  }
  return rai;
}

void IR_Return::add_constraints(std::vector<IRegConstraint>* constraints, int my_id) {
  IRegConstraint c;
  if (dynamic_cast<const None*>(m_return_reg)) {
    return;
  }

  c.ireg = m_return_reg->ireg();
  c.instr_idx = my_id;
  c.desired_register = emitter::RAX;
  constraints->push_back(c);
}

void IR_Return::do_codegen(emitter::ObjectGenerator* gen,
                           const AllocationResult& allocs,
                           emitter::IR_Record irec) {
  auto val_reg = get_reg(m_value, allocs, irec);
  auto dest_reg = get_reg(m_return_reg, allocs, irec);

  if (val_reg == dest_reg) {
    gen->add_instr(IGen::null(), irec);
  } else {
    gen->add_instr(IGen::mov_gpr64_gpr64(dest_reg, val_reg), irec);
  }
}

IR_LoadConstant64::IR_LoadConstant64(const RegVal* dest, u64 value)
    : m_dest(dest), m_value(value) {}

std::string IR_LoadConstant64::print() {
  return fmt::format("mov-ic {}, {}", m_dest->print(), m_value);
}

RegAllocInstr IR_LoadConstant64::to_rai() {
  RegAllocInstr rai;
  rai.write.push_back(m_dest->ireg());
  return rai;
}

void IR_LoadConstant64::do_codegen(emitter::ObjectGenerator* gen,
                                   const AllocationResult& allocs,
                                   emitter::IR_Record irec) {
  auto dest_reg = get_reg(m_dest, allocs, irec);
  gen->add_instr(IGen::mov_gpr64_u64(dest_reg, m_value), irec);
}