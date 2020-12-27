#include <utility>
#include "IR.h"
#include "goalc/emitter/IGen.h"
#include "third-party/fmt/core.h"

using namespace emitter;

namespace {
Register get_reg(const RegVal* rv, const AllocationResult& allocs, emitter::IR_Record irec) {
  if (rv->rlet_constraint().has_value()) {
    auto& range = allocs.ass_as_ranges;
    auto reg = rv->rlet_constraint().value();
    if (rv->ireg().id < int(range.size())) {
      auto& lr = range.at(rv->ireg().id);
      if (irec.ir_id >= lr.min && irec.ir_id <= lr.max) {
        auto ass_reg = range.at(rv->ireg().id).get(irec.ir_id);
        if (ass_reg.kind == Assignment::Kind::REGISTER) {
          assert(ass_reg.reg == reg);
        } else {
          assert(false);
        }
      } else {
        assert(false);
      }
    } else {
      assert(false);
    }
    return reg;
  } else {
    auto& ass = allocs.ass_as_ranges.at(rv->ireg().id).get(irec.ir_id);
    assert(ass.kind == Assignment::Kind::REGISTER);
    return ass.reg;
  }
}

Register get_no_color_reg(const RegVal* rv) {
  if (!rv->rlet_constraint().has_value()) {
    throw std::runtime_error(
        "Accessed a non-rlet constrained variable without the coloring system.");
  }
  return rv->rlet_constraint().value();
}

void load_constant(u64 value,
                   emitter::ObjectGenerator* gen,
                   emitter::IR_Record irec,
                   Register dest_reg) {
  s64 svalue = value;
  if (svalue == 0) {
    gen->add_instr(IGen::xor_gpr64_gpr64(dest_reg, dest_reg), irec);
  } else if (svalue > 0) {
    if (svalue < UINT32_MAX) {
      gen->add_instr(IGen::mov_gpr64_u32(dest_reg, value), irec);
    } else {
      // need a real 64 bit load
      gen->add_instr(IGen::mov_gpr64_u64(dest_reg, value), irec);
    }
  } else {
    if (svalue >= INT32_MIN) {
      gen->add_instr(IGen::mov_gpr64_s32(dest_reg, svalue), irec);
    } else {
      // need a real 64 bit load
      gen->add_instr(IGen::mov_gpr64_u64(dest_reg, value), irec);
    }
  }
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
  load_constant(m_value, gen, irec, dest_reg);
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

/////////////////////
// RegSet
/////////////////////

IR_RegSet::IR_RegSet(const RegVal* dest, const RegVal* src) : m_dest(dest), m_src(src) {}

RegAllocInstr IR_RegSet::to_rai() {
  RegAllocInstr rai;
  rai.write.push_back(m_dest->ireg());
  rai.read.push_back(m_src->ireg());
  if (m_dest->ireg().kind == m_src->ireg().kind) {
    rai.is_move = true;  // only true if we aren't moving from register kind to register kind
  }
  return rai;
}

void IR_RegSet::do_codegen(emitter::ObjectGenerator* gen,
                           const AllocationResult& allocs,
                           emitter::IR_Record irec) {
  auto val_reg = get_reg(m_src, allocs, irec);
  auto dest_reg = get_reg(m_dest, allocs, irec);

  if (val_reg == dest_reg) {
    gen->add_instr(IGen::null(), irec);
  } else if (val_reg.is_gpr() && dest_reg.is_gpr()) {
    gen->add_instr(IGen::mov_gpr64_gpr64(dest_reg, val_reg), irec);
  } else if (val_reg.is_xmm() && dest_reg.is_gpr()) {
    gen->add_instr(IGen::movd_gpr32_xmm32(dest_reg, val_reg), irec);
  } else if (val_reg.is_gpr() && dest_reg.is_xmm()) {
    gen->add_instr(IGen::movd_xmm32_gpr32(dest_reg, val_reg), irec);
  } else if (val_reg.is_xmm() && dest_reg.is_xmm()) {
    gen->add_instr(IGen::mov_xmm32_xmm32(dest_reg, val_reg), irec);
  } else {
    assert(false);
  }
}

std::string IR_RegSet::print() {
  return fmt::format("mov {}, {}", m_dest->print(), m_src->print());
}

/////////////////////
// GotoLabel
/////////////////////

IR_GotoLabel::IR_GotoLabel(const Label* dest) : m_dest(dest) {
  m_resolved = true;
}

IR_GotoLabel::IR_GotoLabel() {
  m_resolved = false;
}

std::string IR_GotoLabel::print() {
  return fmt::format("goto {}", m_dest->print());
}

RegAllocInstr IR_GotoLabel::to_rai() {
  assert(m_resolved);
  RegAllocInstr rai;
  rai.jumps.push_back(m_dest->idx);
  rai.fallthrough = false;
  return rai;
}

void IR_GotoLabel::do_codegen(emitter::ObjectGenerator* gen,
                              const AllocationResult& allocs,
                              emitter::IR_Record irec) {
  (void)allocs;
  auto instr = gen->add_instr(IGen::jmp_32(), irec);
  gen->link_instruction_jump(instr, gen->get_future_ir_record_in_same_func(irec, m_dest->idx));
}

void IR_GotoLabel::resolve(const Label* dest) {
  assert(!m_resolved);
  m_dest = dest;
  m_resolved = true;
}

/////////////////////
// FunctionCall
/////////////////////

IR_FunctionCall::IR_FunctionCall(const RegVal* func, const RegVal* ret, std::vector<RegVal*> args)
    : m_func(func), m_ret(ret), m_args(std::move(args)) {}

std::string IR_FunctionCall::print() {
  std::string result = fmt::format("call {} (ret {}) (args ", m_func->print(), m_ret->print());
  for (const auto& x : m_args) {
    result += fmt::format("{} ", x->print());
  }
  result.pop_back();
  result.push_back(')');
  return result;
}

RegAllocInstr IR_FunctionCall::to_rai() {
  RegAllocInstr rai;
  rai.read.push_back(m_func->ireg());
  rai.write.push_back(m_func->ireg());  // todo, can we avoid this?
  rai.write.push_back(m_ret->ireg());
  for (auto& arg : m_args) {
    rai.read.push_back(arg->ireg());
  }

  for (int i = 0; i < emitter::RegisterInfo::N_REGS; i++) {
    auto info = emitter::gRegInfo.get_info(i);
    if (info.temp()) {
      rai.clobber.emplace_back(i);
    }
  }

  return rai;
}

void IR_FunctionCall::add_constraints(std::vector<IRegConstraint>* constraints, int my_id) {
  for (size_t i = 0; i < m_args.size(); i++) {
    IRegConstraint c;
    c.ireg = m_args.at(i)->ireg();
    c.instr_idx = my_id;
    c.desired_register = emitter::gRegInfo.get_arg_reg(i);
    constraints->push_back(c);
  }

  IRegConstraint c;
  c.ireg = m_ret->ireg();
  c.desired_register = emitter::gRegInfo.get_ret_reg();
  c.instr_idx = my_id;
  constraints->push_back(c);
}

void IR_FunctionCall::do_codegen(emitter::ObjectGenerator* gen,
                                 const AllocationResult& allocs,
                                 emitter::IR_Record irec) {
  auto freg = get_reg(m_func, allocs, irec);
  gen->add_instr(IGen::add_gpr64_gpr64(freg, emitter::gRegInfo.get_offset_reg()), irec);
  gen->add_instr(IGen::call_r64(freg), irec);
  // todo, can we do a sub to undo the modification to the register? does that actually work?
}

/////////////////////
// StaticVarAddr
/////////////////////

IR_StaticVarAddr::IR_StaticVarAddr(const RegVal* dest, const StaticObject* src)
    : m_dest(dest), m_src(src) {}

std::string IR_StaticVarAddr::print() {
  return fmt::format("mov-sva {}, {}", m_dest->print(), m_src->print());
}

RegAllocInstr IR_StaticVarAddr::to_rai() {
  RegAllocInstr rai;
  rai.write.push_back(m_dest->ireg());
  return rai;
}

void IR_StaticVarAddr::do_codegen(emitter::ObjectGenerator* gen,
                                  const AllocationResult& allocs,
                                  emitter::IR_Record irec) {
  auto dr = get_reg(m_dest, allocs, irec);
  auto instr = gen->add_instr(IGen::static_addr(dr, 0), irec);
  gen->link_instruction_static(instr, m_src->rec, m_src->get_addr_offset());
  gen->add_instr(IGen::sub_gpr64_gpr64(dr, emitter::gRegInfo.get_offset_reg()), irec);
}

/////////////////////
// FunctionAddr
/////////////////////

IR_FunctionAddr::IR_FunctionAddr(const RegVal* dest, FunctionEnv* src) : m_dest(dest), m_src(src) {}

std::string IR_FunctionAddr::print() {
  return fmt::format("mov-fa {}, {}", m_dest->print(), m_src->print());
}

RegAllocInstr IR_FunctionAddr::to_rai() {
  RegAllocInstr rai;
  rai.write.push_back(m_dest->ireg());
  return rai;
}

void IR_FunctionAddr::do_codegen(emitter::ObjectGenerator* gen,
                                 const AllocationResult& allocs,
                                 emitter::IR_Record irec) {
  auto dr = get_reg(m_dest, allocs, irec);
  auto instr = gen->add_instr(IGen::static_addr(dr, 0), irec);
  gen->link_instruction_to_function(instr, gen->get_existing_function_record(m_src->idx_in_file));
  gen->add_instr(IGen::sub_gpr64_gpr64(dr, emitter::gRegInfo.get_offset_reg()), irec);
}

/////////////////////
// IntegerMath
/////////////////////

IR_IntegerMath::IR_IntegerMath(IntegerMathKind kind, RegVal* dest, RegVal* arg)
    : m_kind(kind), m_dest(dest), m_arg(arg) {}

IR_IntegerMath::IR_IntegerMath(IntegerMathKind kind, RegVal* dest, u8 shift_amount)
    : m_kind(kind), m_dest(dest), m_shift_amount(shift_amount) {}

std::string IR_IntegerMath::print() {
  switch (m_kind) {
    case IntegerMathKind::ADD_64:
      return fmt::format("addi {}, {}", m_dest->print(), m_arg->print());
    case IntegerMathKind::SUB_64:
      return fmt::format("subi {}, {}", m_dest->print(), m_arg->print());
    case IntegerMathKind::IMUL_32:
      return fmt::format("imul {}, {}", m_dest->print(), m_arg->print());
    case IntegerMathKind::IMUL_64:
      return fmt::format("imul64 {}, {}", m_dest->print(), m_arg->print());
    case IntegerMathKind::IDIV_32:
      return fmt::format("idiv {}, {}", m_dest->print(), m_arg->print());
    case IntegerMathKind::IMOD_32:
      return fmt::format("imod {}, {}", m_dest->print(), m_arg->print());
    case IntegerMathKind::SARV_64:
      return fmt::format("sarv {}, {}", m_dest->print(), m_arg->print());
    case IntegerMathKind::SHLV_64:
      return fmt::format("shlv {}, {}", m_dest->print(), m_arg->print());
    case IntegerMathKind::SHRV_64:
      return fmt::format("shrv {}, {}", m_dest->print(), m_arg->print());
    case IntegerMathKind::SAR_64:
      return fmt::format("sar {}, {}", m_dest->print(), m_shift_amount);
    case IntegerMathKind::SHL_64:
      return fmt::format("shl {}, {}", m_dest->print(), m_shift_amount);
    case IntegerMathKind::SHR_64:
      return fmt::format("shr {}, {}", m_dest->print(), m_shift_amount);
    case IntegerMathKind::AND_64:
      return fmt::format("and {}, {}", m_dest->print(), m_arg->print());
    case IntegerMathKind::OR_64:
      return fmt::format("or {}, {}", m_dest->print(), m_arg->print());
    case IntegerMathKind::XOR_64:
      return fmt::format("xor {}, {}", m_dest->print(), m_arg->print());
    case IntegerMathKind::NOT_64:
      return fmt::format("not {}", m_dest->print());
    default:
      throw std::runtime_error("Unsupported IntegerMathKind");
  }
}

RegAllocInstr IR_IntegerMath::to_rai() {
  RegAllocInstr rai;
  rai.write.push_back(m_dest->ireg());
  rai.read.push_back(m_dest->ireg());

  if (m_kind != IntegerMathKind::NOT_64 && m_kind != IntegerMathKind::SHL_64 &&
      m_kind != IntegerMathKind::SAR_64 && m_kind != IntegerMathKind::SHR_64) {
    rai.read.push_back(m_arg->ireg());
  }

  if (m_kind == IntegerMathKind::IDIV_32 || m_kind == IntegerMathKind::IMOD_32) {
    rai.exclude.emplace_back(emitter::RDX);
  }
  return rai;
}

void IR_IntegerMath::do_codegen(emitter::ObjectGenerator* gen,
                                const AllocationResult& allocs,
                                emitter::IR_Record irec) {
  switch (m_kind) {
    case IntegerMathKind::ADD_64:
      gen->add_instr(
          IGen::add_gpr64_gpr64(get_reg(m_dest, allocs, irec), get_reg(m_arg, allocs, irec)), irec);
      break;
    case IntegerMathKind::SUB_64:
      gen->add_instr(
          IGen::sub_gpr64_gpr64(get_reg(m_dest, allocs, irec), get_reg(m_arg, allocs, irec)), irec);
      break;
    case IntegerMathKind::AND_64:
      gen->add_instr(
          IGen::and_gpr64_gpr64(get_reg(m_dest, allocs, irec), get_reg(m_arg, allocs, irec)), irec);
      break;
    case IntegerMathKind::OR_64:
      gen->add_instr(
          IGen::or_gpr64_gpr64(get_reg(m_dest, allocs, irec), get_reg(m_arg, allocs, irec)), irec);
      break;
    case IntegerMathKind::XOR_64:
      gen->add_instr(
          IGen::xor_gpr64_gpr64(get_reg(m_dest, allocs, irec), get_reg(m_arg, allocs, irec)), irec);
      break;
    case IntegerMathKind::NOT_64:
      gen->add_instr(IGen::not_gpr64(get_reg(m_dest, allocs, irec)), irec);
      assert(!m_arg);
      break;
    case IntegerMathKind::SHLV_64:
      gen->add_instr(IGen::shl_gpr64_cl(get_reg(m_dest, allocs, irec)), irec);
      assert(get_reg(m_arg, allocs, irec) == emitter::RCX);
      break;
    case IntegerMathKind::SHRV_64:
      gen->add_instr(IGen::shr_gpr64_cl(get_reg(m_dest, allocs, irec)), irec);
      assert(get_reg(m_arg, allocs, irec) == emitter::RCX);
      break;
    case IntegerMathKind::SARV_64:
      gen->add_instr(IGen::sar_gpr64_cl(get_reg(m_dest, allocs, irec)), irec);
      assert(get_reg(m_arg, allocs, irec) == emitter::RCX);
      break;
    case IntegerMathKind::SHL_64:
      gen->add_instr(IGen::shl_gpr64_u8(get_reg(m_dest, allocs, irec), m_shift_amount), irec);
      break;
    case IntegerMathKind::SHR_64:
      gen->add_instr(IGen::shr_gpr64_u8(get_reg(m_dest, allocs, irec), m_shift_amount), irec);
      break;
    case IntegerMathKind::SAR_64:
      gen->add_instr(IGen::sar_gpr64_u8(get_reg(m_dest, allocs, irec), m_shift_amount), irec);
      break;
    case IntegerMathKind::IMUL_32: {
      auto dr = get_reg(m_dest, allocs, irec);
      gen->add_instr(IGen::imul_gpr32_gpr32(dr, get_reg(m_arg, allocs, irec)), irec);
      gen->add_instr(IGen::movsx_r64_r32(dr, dr), irec);
    } break;
    case IntegerMathKind::IMUL_64: {
      auto dr = get_reg(m_dest, allocs, irec);
      gen->add_instr(IGen::imul_gpr64_gpr64(dr, get_reg(m_arg, allocs, irec)), irec);
    } break;
    case IntegerMathKind::IDIV_32: {
      gen->add_instr(IGen::cdq(), irec);
      gen->add_instr(IGen::idiv_gpr32(get_reg(m_arg, allocs, irec)), irec);
      gen->add_instr(IGen::movsx_r64_r32(get_reg(m_dest, allocs, irec), emitter::RAX), irec);
    } break;
    case IntegerMathKind::IMOD_32: {
      gen->add_instr(IGen::cdq(), irec);
      gen->add_instr(IGen::idiv_gpr32(get_reg(m_arg, allocs, irec)), irec);
      gen->add_instr(IGen::movsx_r64_r32(get_reg(m_dest, allocs, irec), emitter::RDX), irec);
    } break;

    default:
      assert(false);
  }
}

/////////////////////
// FloatMath
/////////////////////

IR_FloatMath::IR_FloatMath(FloatMathKind kind, RegVal* dest, RegVal* arg)
    : m_kind(kind), m_dest(dest), m_arg(arg) {}

std::string IR_FloatMath::print() {
  switch (m_kind) {
    case FloatMathKind::DIV_SS:
      return fmt::format("divss {}, {}", m_dest->print(), m_arg->print());
    case FloatMathKind::MUL_SS:
      return fmt::format("mulss {}, {}", m_dest->print(), m_arg->print());
    case FloatMathKind::ADD_SS:
      return fmt::format("addss {}, {}", m_dest->print(), m_arg->print());
    case FloatMathKind::SUB_SS:
      return fmt::format("subss {}, {}", m_dest->print(), m_arg->print());
    case FloatMathKind::MAX_SS:
      return fmt::format("maxss {}, {}", m_dest->print(), m_arg->print());
    case FloatMathKind::MIN_SS:
      return fmt::format("minss {}, {}", m_dest->print(), m_arg->print());
    default:
      throw std::runtime_error("Unsupported FloatMathKind");
  }
}

RegAllocInstr IR_FloatMath::to_rai() {
  RegAllocInstr rai;
  rai.write.push_back(m_dest->ireg());
  rai.read.push_back(m_dest->ireg());
  rai.read.push_back(m_arg->ireg());
  return rai;
}

void IR_FloatMath::do_codegen(emitter::ObjectGenerator* gen,
                              const AllocationResult& allocs,
                              emitter::IR_Record irec) {
  switch (m_kind) {
    case FloatMathKind::DIV_SS:
      gen->add_instr(
          IGen::divss_xmm_xmm(get_reg(m_dest, allocs, irec), get_reg(m_arg, allocs, irec)), irec);
      break;
    case FloatMathKind::MUL_SS:
      gen->add_instr(
          IGen::mulss_xmm_xmm(get_reg(m_dest, allocs, irec), get_reg(m_arg, allocs, irec)), irec);
      break;
    case FloatMathKind::ADD_SS:
      gen->add_instr(
          IGen::addss_xmm_xmm(get_reg(m_dest, allocs, irec), get_reg(m_arg, allocs, irec)), irec);
      break;
    case FloatMathKind::SUB_SS:
      gen->add_instr(
          IGen::subss_xmm_xmm(get_reg(m_dest, allocs, irec), get_reg(m_arg, allocs, irec)), irec);
      break;
    case FloatMathKind::MAX_SS:
      gen->add_instr(
          IGen::maxss_xmm_xmm(get_reg(m_dest, allocs, irec), get_reg(m_arg, allocs, irec)), irec);
      break;
    case FloatMathKind::MIN_SS:
      gen->add_instr(
          IGen::minss_xmm_xmm(get_reg(m_dest, allocs, irec), get_reg(m_arg, allocs, irec)), irec);
      break;
    default:
      assert(false);
  }
}

/////////////////////
// StaticVarLoad
/////////////////////

IR_StaticVarLoad::IR_StaticVarLoad(const RegVal* dest, const StaticObject* src)
    : m_dest(dest), m_src(src) {}

std::string IR_StaticVarLoad::print() {
  return fmt::format("mov-svl {}, [{}]", m_dest->print(), m_src->print());
}

RegAllocInstr IR_StaticVarLoad::to_rai() {
  RegAllocInstr rai;
  rai.write.push_back(m_dest->ireg());
  return rai;
}

void IR_StaticVarLoad::do_codegen(emitter::ObjectGenerator* gen,
                                  const AllocationResult& allocs,
                                  emitter::IR_Record irec) {
  auto load_info = m_src->get_load_info();
  assert(m_src->get_addr_offset() == 0);

  if (m_dest->ireg().kind == emitter::RegKind::XMM) {
    assert(load_info.load_signed == false);
    assert(load_info.load_size == 4);
    assert(load_info.requires_load == true);

    auto instr = gen->add_instr(IGen::static_load_xmm32(get_reg(m_dest, allocs, irec), 0), irec);
    gen->link_instruction_static(instr, m_src->rec, 0);
  } else {
    assert(false);
  }
}

/////////////////////
// ConditionalBranch
/////////////////////

std::string Condition::print() const {
  switch (kind) {
    case ConditionKind::NOT_EQUAL:
      return a->print() + " != " + b->print();
    case ConditionKind::EQUAL:
      return a->print() + " == " + b->print();
    case ConditionKind::LEQ:
      return a->print() + " <= " + b->print();
    case ConditionKind::GEQ:
      return a->print() + " >= " + b->print();
    case ConditionKind::LT:
      return a->print() + " < " + b->print();
    case ConditionKind::GT:
      return a->print() + " > " + b->print();
    default:
      throw std::runtime_error("unknown condition type in GoalCondition::print()");
  }
}

RegAllocInstr Condition::to_rai() {
  RegAllocInstr rai;
  rai.read.push_back(a->ireg());
  rai.read.push_back(b->ireg());
  return rai;
}

IR_ConditionalBranch::IR_ConditionalBranch(const Condition& _condition, Label _label)
    : condition(_condition), label(_label) {}

std::string IR_ConditionalBranch::print() {
  // todo, float/signed info?
  return fmt::format("j({}) {}", condition.print(), label.print());
}

RegAllocInstr IR_ConditionalBranch::to_rai() {
  auto rai = condition.to_rai();
  assert(m_resolved);
  rai.jumps.push_back(label.idx);
  return rai;
}

void IR_ConditionalBranch::do_codegen(emitter::ObjectGenerator* gen,
                                      const AllocationResult& allocs,
                                      emitter::IR_Record irec) {
  Instruction jump_instr(0);
  assert(m_resolved);
  switch (condition.kind) {
    case ConditionKind::EQUAL:
      jump_instr = IGen::je_32();
      break;
    case ConditionKind::NOT_EQUAL:
      jump_instr = IGen::jne_32();
      break;
    case ConditionKind::LEQ:
      if (condition.is_signed) {
        jump_instr = IGen::jle_32();
      } else {
        jump_instr = IGen::jbe_32();
      }
      break;
    case ConditionKind::GEQ:
      if (condition.is_signed) {
        jump_instr = IGen::jge_32();
      } else {
        jump_instr = IGen::jae_32();
      }
      break;

    case ConditionKind::LT:
      if (condition.is_signed) {
        jump_instr = IGen::jl_32();
      } else {
        jump_instr = IGen::jb_32();
      }
      break;
    case ConditionKind::GT:
      if (condition.is_signed) {
        jump_instr = IGen::jg_32();
      } else {
        jump_instr = IGen::ja_32();
      }
      break;
    default:
      assert(false);
  }

  if (condition.is_float) {
    gen->add_instr(
        IGen::cmp_flt_flt(get_reg(condition.a, allocs, irec), get_reg(condition.b, allocs, irec)),
        irec);
  } else {
    gen->add_instr(IGen::cmp_gpr64_gpr64(get_reg(condition.a, allocs, irec),
                                         get_reg(condition.b, allocs, irec)),
                   irec);
  }

  auto jump_rec = gen->add_instr(jump_instr, irec);
  gen->link_instruction_jump(jump_rec, gen->get_future_ir_record_in_same_func(irec, label.idx));
}

/////////////////////
// LoadConstantOffset
/////////////////////

IR_LoadConstOffset::IR_LoadConstOffset(const RegVal* dest,
                                       int offset,
                                       const RegVal* base,
                                       MemLoadInfo info)
    : m_dest(dest), m_offset(offset), m_base(base), m_info(info) {}

std::string IR_LoadConstOffset::print() {
  return fmt::format("mov {}, [{} + {}]", m_dest->print(), m_base->print(), m_offset);
}

RegAllocInstr IR_LoadConstOffset::to_rai() {
  RegAllocInstr rai;
  rai.write.push_back(m_dest->ireg());
  rai.read.push_back(m_base->ireg());
  return rai;
}

void IR_LoadConstOffset::do_codegen(emitter::ObjectGenerator* gen,
                                    const AllocationResult& allocs,
                                    emitter::IR_Record irec) {
  if (m_dest->ireg().kind == emitter::RegKind::GPR) {
    gen->add_instr(IGen::load_goal_gpr(get_reg(m_dest, allocs, irec), get_reg(m_base, allocs, irec),
                                       emitter::gRegInfo.get_offset_reg(), m_offset, m_info.size,
                                       m_info.sign_extend),
                   irec);
  } else if (m_dest->ireg().kind == emitter::RegKind::XMM && m_info.size == 4 &&
             m_info.sign_extend == false && m_info.reg == ::RegKind::FLOAT) {
    gen->add_instr(
        IGen::load_goal_xmm32(get_reg(m_dest, allocs, irec), get_reg(m_base, allocs, irec),
                              emitter::gRegInfo.get_offset_reg(), m_offset),
        irec);
  } else {
    throw std::runtime_error("IR_LoadConstOffset::do_codegen not supported");
  }
}

///////////////////////
// StoreConstantOffset
///////////////////////
IR_StoreConstOffset::IR_StoreConstOffset(const RegVal* value,
                                         int offset,
                                         const RegVal* base,
                                         int size)
    : m_value(value), m_offset(offset), m_base(base), m_size(size) {}

std::string IR_StoreConstOffset::print() {
  return fmt::format("move [{} + {}], {}", m_base->print(), m_offset, m_value->print());
}

RegAllocInstr IR_StoreConstOffset::to_rai() {
  RegAllocInstr rai;
  rai.read.push_back(m_value->ireg());
  rai.read.push_back(m_base->ireg());
  return rai;
}

void IR_StoreConstOffset::do_codegen(emitter::ObjectGenerator* gen,
                                     const AllocationResult& allocs,
                                     emitter::IR_Record irec) {
  if (m_value->ireg().kind == emitter::RegKind::GPR) {
    gen->add_instr(
        IGen::store_goal_gpr(get_reg(m_base, allocs, irec), get_reg(m_value, allocs, irec),
                             emitter::gRegInfo.get_offset_reg(), m_offset, m_size),
        irec);
  } else if (m_value->ireg().kind == emitter::RegKind::XMM && m_size == 4) {
    gen->add_instr(
        IGen::store_goal_xmm32(get_reg(m_base, allocs, irec), get_reg(m_value, allocs, irec),
                               emitter::gRegInfo.get_offset_reg(), m_offset),
        irec);
  } else {
    throw std::runtime_error("IR_StoreConstOffset::do_codegen can't handle this");
  }
}

///////////////////////
// Null
///////////////////////
std::string IR_Null::print() {
  return "null";
}

RegAllocInstr IR_Null::to_rai() {
  return {};
}

void IR_Null::do_codegen(emitter::ObjectGenerator* gen,
                         const AllocationResult& allocs,
                         emitter::IR_Record irec) {
  (void)gen;
  (void)allocs;
  (void)irec;
}

///////////////////////
// ValueReset
///////////////////////
IR_ValueReset::IR_ValueReset(std::vector<RegVal*> args) : m_args(std::move(args)) {}

std::string IR_ValueReset::print() {
  return "value-reset";
}

RegAllocInstr IR_ValueReset::to_rai() {
  RegAllocInstr rai;
  for (auto& x : m_args) {
    rai.write.push_back(x->ireg());
  }
  return rai;
}

void IR_ValueReset::do_codegen(emitter::ObjectGenerator* gen,
                               const AllocationResult& allocs,
                               emitter::IR_Record irec) {
  (void)gen;
  (void)allocs;
  (void)irec;
}

///////////////////////
// FloatToInt
///////////////////////

IR_FloatToInt::IR_FloatToInt(const RegVal* dest, const RegVal* src) : m_dest(dest), m_src(src) {}

std::string IR_FloatToInt::print() {
  return fmt::format("f2i {}, {}", m_dest->print(), m_src->print());
}

RegAllocInstr IR_FloatToInt::to_rai() {
  RegAllocInstr rai;
  rai.read.push_back(m_src->ireg());
  rai.write.push_back(m_dest->ireg());
  return rai;
}

void IR_FloatToInt::do_codegen(emitter::ObjectGenerator* gen,
                               const AllocationResult& allocs,
                               emitter::IR_Record irec) {
  gen->add_instr(IGen::float_to_int32(get_reg(m_dest, allocs, irec), get_reg(m_src, allocs, irec)),
                 irec);
}

///////////////////////
// IntToFloat
///////////////////////

IR_IntToFloat::IR_IntToFloat(const RegVal* dest, const RegVal* src) : m_dest(dest), m_src(src) {}

std::string IR_IntToFloat::print() {
  return fmt::format("i2f {}, {}", m_dest->print(), m_src->print());
}

RegAllocInstr IR_IntToFloat::to_rai() {
  RegAllocInstr rai;
  rai.read.push_back(m_src->ireg());
  rai.write.push_back(m_dest->ireg());
  return rai;
}

void IR_IntToFloat::do_codegen(emitter::ObjectGenerator* gen,
                               const AllocationResult& allocs,
                               emitter::IR_Record irec) {
  gen->add_instr(IGen::int32_to_float(get_reg(m_dest, allocs, irec), get_reg(m_src, allocs, irec)),
                 irec);
}

///////////////////////
// GetStackAddr
///////////////////////

IR_GetStackAddr::IR_GetStackAddr(const RegVal* dest, int slot) : m_dest(dest), m_slot(slot) {}

std::string IR_GetStackAddr::print() {
  return fmt::format("mov {}, stack-slot-{}", m_dest->print(), m_slot);
}

RegAllocInstr IR_GetStackAddr::to_rai() {
  RegAllocInstr rai;
  rai.write.push_back(m_dest->ireg());
  return rai;
}

void IR_GetStackAddr::do_codegen(emitter::ObjectGenerator* gen,
                                 const AllocationResult& allocs,
                                 emitter::IR_Record irec) {
  auto dest_reg = get_reg(m_dest, allocs, irec);
  int offset = GPR_SIZE * allocs.get_slot_for_var(m_slot);

  // dest = offset
  load_constant(offset, gen, irec, dest_reg);
  // dest = offset + RSP
  gen->add_instr(IGen::add_gpr64_gpr64(dest_reg, RSP), irec);
  // dest = offset + RSP - offset
  gen->add_instr(IGen::sub_gpr64_gpr64(dest_reg, gRegInfo.get_offset_reg()), irec);
}

///////////////////////
// Asm
///////////////////////

IR_Asm::IR_Asm(bool use_coloring) : m_use_coloring(use_coloring) {}

std::string IR_Asm::get_color_suffix_string() {
  if (m_use_coloring) {
    return "";
  } else {
    return " :no-color";
  }
}

///////////////////////
// AsmRet
///////////////////////

IR_AsmRet::IR_AsmRet(bool use_coloring) : IR_Asm(use_coloring) {}

std::string IR_AsmRet::print() {
  return fmt::format(".ret{}", get_color_suffix_string());
}

RegAllocInstr IR_AsmRet::to_rai() {
  return {};
}

void IR_AsmRet::do_codegen(emitter::ObjectGenerator* gen,
                           const AllocationResult& allocs,
                           emitter::IR_Record irec) {
  (void)allocs;
  gen->add_instr(IGen::ret(), irec);
}

///////////////////////
// AsmPush
///////////////////////

IR_AsmPush::IR_AsmPush(bool use_coloring, const RegVal* src) : IR_Asm(use_coloring), m_src(src) {}

std::string IR_AsmPush::print() {
  return fmt::format(".push{} {}", get_color_suffix_string(), m_src->print());
}

RegAllocInstr IR_AsmPush::to_rai() {
  RegAllocInstr rai;
  if (m_use_coloring) {
    rai.read.push_back(m_src->ireg());
  }
  return rai;
}

void IR_AsmPush::do_codegen(emitter::ObjectGenerator* gen,
                            const AllocationResult& allocs,
                            emitter::IR_Record irec) {
  if (m_use_coloring) {
    gen->add_instr(IGen::push_gpr64(get_reg(m_src, allocs, irec)), irec);
  } else {
    gen->add_instr(IGen::push_gpr64(get_no_color_reg(m_src)), irec);
  }
}

///////////////////////
// AsmPop
///////////////////////

IR_AsmPop::IR_AsmPop(bool use_coloring, const RegVal* dst) : IR_Asm(use_coloring), m_dst(dst) {}

std::string IR_AsmPop::print() {
  return fmt::format(".pop{} {}", get_color_suffix_string(), m_dst->print());
}

RegAllocInstr IR_AsmPop::to_rai() {
  RegAllocInstr rai;
  if (m_use_coloring) {
    rai.write.push_back(m_dst->ireg());
  }
  return rai;
}

void IR_AsmPop::do_codegen(emitter::ObjectGenerator* gen,
                           const AllocationResult& allocs,
                           emitter::IR_Record irec) {
  if (m_use_coloring) {
    gen->add_instr(IGen::pop_gpr64(get_reg(m_dst, allocs, irec)), irec);
  } else {
    gen->add_instr(IGen::pop_gpr64(get_no_color_reg(m_dst)), irec);
  }
}

///////////////////////
// AsmSub
///////////////////////

IR_AsmSub::IR_AsmSub(bool use_coloring, const RegVal* dst, const RegVal* src)
    : IR_Asm(use_coloring), m_dst(dst), m_src(src) {}

std::string IR_AsmSub::print() {
  return fmt::format(".sub{} {}, {}", get_color_suffix_string(), m_dst->print(), m_src->print());
}

RegAllocInstr IR_AsmSub::to_rai() {
  RegAllocInstr rai;
  if (m_use_coloring) {
    rai.write.push_back(m_dst->ireg());
    rai.read.push_back(m_dst->ireg());
    rai.read.push_back(m_src->ireg());
  }
  return rai;
}

void IR_AsmSub::do_codegen(emitter::ObjectGenerator* gen,
                           const AllocationResult& allocs,
                           emitter::IR_Record irec) {
  if (m_use_coloring) {
    gen->add_instr(
        IGen::sub_gpr64_gpr64(get_reg(m_dst, allocs, irec), get_reg(m_src, allocs, irec)), irec);
  } else {
    gen->add_instr(IGen::sub_gpr64_gpr64(get_no_color_reg(m_dst), get_no_color_reg(m_src)), irec);
  }
}

///////////////////////
// AsmAdd
///////////////////////

IR_AsmAdd::IR_AsmAdd(bool use_coloring, const RegVal* dst, const RegVal* src)
    : IR_Asm(use_coloring), m_dst(dst), m_src(src) {}

std::string IR_AsmAdd::print() {
  return fmt::format(".add{} {}, {}", get_color_suffix_string(), m_dst->print(), m_src->print());
}

RegAllocInstr IR_AsmAdd::to_rai() {
  RegAllocInstr rai;
  if (m_use_coloring) {
    rai.write.push_back(m_dst->ireg());
    rai.read.push_back(m_dst->ireg());
    rai.read.push_back(m_src->ireg());
  }
  return rai;
}

void IR_AsmAdd::do_codegen(emitter::ObjectGenerator* gen,
                           const AllocationResult& allocs,
                           emitter::IR_Record irec) {
  if (m_use_coloring) {
    gen->add_instr(
        IGen::add_gpr64_gpr64(get_reg(m_dst, allocs, irec), get_reg(m_src, allocs, irec)), irec);
  } else {
    gen->add_instr(IGen::add_gpr64_gpr64(get_no_color_reg(m_dst), get_no_color_reg(m_src)), irec);
  }
}

///////////////////////
// AsmGetSymbolValue
///////////////////////

IR_GetSymbolValueAsm::IR_GetSymbolValueAsm(bool use_coloring,
                                           const RegVal* dest,
                                           std::string sym_name,
                                           bool sext)
    : IR_Asm(use_coloring), m_dest(dest), m_sym_name(std::move(sym_name)), m_sext(sext) {}

std::string IR_GetSymbolValueAsm::print() {
  return fmt::format(".load-sym{} {} [{}]", get_color_suffix_string(), m_dest->print(), m_sym_name);
}

RegAllocInstr IR_GetSymbolValueAsm::to_rai() {
  RegAllocInstr rai;
  if (m_use_coloring) {
    rai.write.push_back(m_dest->ireg());
  }
  return rai;
}

void IR_GetSymbolValueAsm::do_codegen(emitter::ObjectGenerator* gen,
                                      const AllocationResult& allocs,
                                      emitter::IR_Record irec) {
  auto dst_reg = m_use_coloring ? get_reg(m_dest, allocs, irec) : get_no_color_reg(m_dest);
  if (m_sext) {
    auto instr =
        gen->add_instr(IGen::load32s_gpr64_gpr64_plus_gpr64_plus_s32(
                           dst_reg, gRegInfo.get_st_reg(), gRegInfo.get_offset_reg(), 0x0badbeef),
                       irec);
    gen->link_instruction_symbol_mem(instr, m_sym_name);
  } else {
    auto instr =
        gen->add_instr(IGen::load32u_gpr64_gpr64_plus_gpr64_plus_s32(
                           dst_reg, gRegInfo.get_st_reg(), gRegInfo.get_offset_reg(), 0x0badbeef),
                       irec);
    gen->link_instruction_symbol_mem(instr, m_sym_name);
  }
}

///////////////////////
// AsmJumpReg
///////////////////////

IR_JumpReg::IR_JumpReg(bool use_coloring, const RegVal* src) : IR_Asm(use_coloring), m_src(src) {}

std::string IR_JumpReg::print() {
  return fmt::format(".jr{} {}", get_color_suffix_string(), m_src->print());
}

RegAllocInstr IR_JumpReg::to_rai() {
  RegAllocInstr rai;
  if (m_use_coloring) {
    rai.read.push_back(m_src->ireg());
  }
  return rai;
}

void IR_JumpReg::do_codegen(emitter::ObjectGenerator* gen,
                            const AllocationResult& allocs,
                            emitter::IR_Record irec) {
  auto src_reg = m_use_coloring ? get_reg(m_src, allocs, irec) : get_no_color_reg(m_src);
  gen->add_instr(IGen::jmp_r64(src_reg), irec);
}

///////////////////////
// AsmRegSet
///////////////////////

IR_RegSetAsm::IR_RegSetAsm(bool use_color, const RegVal* dst, const RegVal* src)
    : IR_Asm(use_color), m_dst(dst), m_src(src) {}

std::string IR_RegSetAsm::print() {
  return fmt::format(".mov{} {} {}", get_color_suffix_string(), m_dst->print(), m_src->print());
}

RegAllocInstr IR_RegSetAsm::to_rai() {
  RegAllocInstr rai;
  if (m_use_coloring) {
    rai.write.push_back(m_dst->ireg());
    rai.read.push_back(m_src->ireg());
  }
  return rai;
}

void IR_RegSetAsm::do_codegen(emitter::ObjectGenerator* gen,
                              const AllocationResult& allocs,
                              emitter::IR_Record irec) {
  auto val_reg = m_use_coloring ? get_reg(m_src, allocs, irec) : get_no_color_reg(m_src);
  auto dest_reg = m_use_coloring ? get_reg(m_dst, allocs, irec) : get_no_color_reg(m_dst);

  if (val_reg == dest_reg) {
    gen->add_instr(IGen::null(), irec);
  } else if (val_reg.is_gpr() && dest_reg.is_gpr()) {
    gen->add_instr(IGen::mov_gpr64_gpr64(dest_reg, val_reg), irec);
  } else if (val_reg.is_xmm() && dest_reg.is_gpr()) {
    gen->add_instr(IGen::movd_gpr32_xmm32(dest_reg, val_reg), irec);
  } else if (val_reg.is_gpr() && dest_reg.is_xmm()) {
    gen->add_instr(IGen::movd_xmm32_gpr32(dest_reg, val_reg), irec);
  } else if (val_reg.is_xmm() && dest_reg.is_xmm()) {
    gen->add_instr(IGen::mov_xmm32_xmm32(dest_reg, val_reg), irec);
  } else {
    assert(false);
  }
}