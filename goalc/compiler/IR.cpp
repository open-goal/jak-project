#include "IR.h"

#include <utility>
#include "goalc/emitter/IGen.h"

using namespace emitter;

namespace {
Register get_reg(const RegVal* rv, const AllocationResult& allocs, emitter::IR_Record irec) {
  auto& ass = allocs.ass_as_ranges.at(rv->ireg().id).get(irec.ir_id);
  assert(ass.kind == Assignment::Kind::REGISTER);
  return ass.reg;
}
}  // namespace

///////////
// Return
///////////
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

/////////////////////
// LoadConstant64
/////////////////////
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

/////////////////////
// LoadSymbolPointer
/////////////////////
IR_LoadSymbolPointer::IR_LoadSymbolPointer(const RegVal* dest, std::string name)
    : m_dest(dest), m_name(std::move(name)) {}

std::string IR_LoadSymbolPointer::print() {
  return fmt::format("mov-symptr {}, '{}", m_dest->print(), m_name);
}

RegAllocInstr IR_LoadSymbolPointer::to_rai() {
  RegAllocInstr rai;
  rai.write.push_back(m_dest->ireg());
  return rai;
}

void IR_LoadSymbolPointer::do_codegen(emitter::ObjectGenerator* gen,
                                      const AllocationResult& allocs,
                                      emitter::IR_Record irec) {
  auto dest_reg = get_reg(m_dest, allocs, irec);
  // todo, could be single lea opcode
  gen->add_instr(IGen::mov_gpr64_gpr64(dest_reg, gRegInfo.get_st_reg()), irec);
  auto add = gen->add_instr(IGen::add_gpr64_imm32s(dest_reg, 0x0afecafe), irec);
  gen->link_instruction_symbol_ptr(add, m_name);
}

/////////////////////
// SetSymbolValue
/////////////////////

IR_SetSymbolValue::IR_SetSymbolValue(const SymbolVal* dest, const RegVal* src)
    : m_dest(dest), m_src(src) {}

std::string IR_SetSymbolValue::print() {
  return fmt::format("mov '{}, {}", m_dest->name(), m_src->print());
}

RegAllocInstr IR_SetSymbolValue::to_rai() {
  RegAllocInstr rai;
  rai.read.push_back(m_src->ireg());
  return rai;
}

void IR_SetSymbolValue::do_codegen(emitter::ObjectGenerator* gen,
                                   const AllocationResult& allocs,
                                   emitter::IR_Record irec) {
  auto src_reg = get_reg(m_src, allocs, irec);
  auto instr =
      gen->add_instr(IGen::store32_gpr64_gpr64_plus_gpr64_plus_s32(
                         gRegInfo.get_st_reg(), gRegInfo.get_offset_reg(), src_reg, 0x0badbeef),
                     irec);
  gen->link_instruction_symbol_mem(instr, m_dest->name());
}

/////////////////////
// GetSymbolValue
/////////////////////

IR_GetSymbolValue::IR_GetSymbolValue(const RegVal* dest, const SymbolVal* src, bool sext)
    : m_dest(dest), m_src(src), m_sext(sext) {}

std::string IR_GetSymbolValue::print() {
  return fmt::format("mov {}, '{}", m_dest->print(), m_src->name());
}

RegAllocInstr IR_GetSymbolValue::to_rai() {
  RegAllocInstr rai;
  rai.write.push_back(m_dest->ireg());
  return rai;
}

void IR_GetSymbolValue::do_codegen(emitter::ObjectGenerator* gen,
                                   const AllocationResult& allocs,
                                   emitter::IR_Record irec) {
  auto dst_reg = get_reg(m_dest, allocs, irec);
  if (m_sext) {
    auto instr =
        gen->add_instr(IGen::load32s_gpr64_gpr64_plus_gpr64_plus_s32(
                           dst_reg, gRegInfo.get_st_reg(), gRegInfo.get_offset_reg(), 0x0badbeef),
                       irec);
    gen->link_instruction_symbol_mem(instr, m_src->name());
  } else {
    auto instr =
        gen->add_instr(IGen::load32u_gpr64_gpr64_plus_gpr64_plus_s32(
                           dst_reg, gRegInfo.get_st_reg(), gRegInfo.get_offset_reg(), 0x0badbeef),
                       irec);
    gen->link_instruction_symbol_mem(instr, m_src->name());
  }
}