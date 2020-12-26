/*!
 * @file BasicOpBuilder.cpp
 * Convert a basic block into a sequence of IR operations.
 * Build up basic set instructions from GOAL code
 * Recognize common GOAL compiler idioms
 * Recognize branch delay slot use
 * Recognize assembly ops and pass them through as IR_Asm
 */

#include "BasicOpBuilder.h"
#include "decompiler/Function/Function.h"
#include "decompiler/Function/BasicBlocks.h"
#include "decompiler/Disasm/InstructionMatching.h"
#include "decompiler/IR/IR.h"
#include "common/symbols.h"

namespace {

///////////////////////////////
// Helpers
///////////////////////////////

/*!
 * Create a GOAL "set!" form.
 * These will later be compacted into more complicated nested expressions.
 */
std::shared_ptr<IR_Set_Atomic> make_set_atomic(IR_Set_Atomic::Kind kind,
                                               const std::shared_ptr<IR>& dst,
                                               const std::shared_ptr<IR>& src) {
  return std::make_shared<IR_Set_Atomic>(kind, dst, src);
}

/*!
 * Create an IR representing a register at a certain point.  Idx is the instruction index.
 */
std::shared_ptr<IR_Register> make_reg(Register reg, int idx) {
  return std::make_shared<IR_Register>(reg, idx);
}

/*!
 * Create an IR representing a symbol. The symbol itself ('thing), not the value.
 */
std::shared_ptr<IR_Symbol> make_sym(const std::string& name) {
  return std::make_shared<IR_Symbol>(name);
}

/*!
 * Create an IR representing the value of a symbol. Can be read/written.
 */
std::shared_ptr<IR_SymbolValue> make_sym_value(const std::string& name) {
  return std::make_shared<IR_SymbolValue>(name);
}

/*!
 * Create an integer constant.
 */
std::shared_ptr<IR_IntegerConstant> make_int(int64_t x) {
  return std::make_shared<IR_IntegerConstant>(x);
}

/*!
 * Create an assembly passthrough in the form op dst, src, src. Sets register info.
 */
std::shared_ptr<IR_Atomic> to_asm_reg_reg_reg(const std::string& str, Instruction& instr, int idx) {
  auto result = std::make_shared<IR_AsmOp_Atomic>(str);
  result->dst = make_reg(instr.get_dst(0).get_reg(), idx);
  result->src0 = make_reg(instr.get_src(0).get_reg(), idx);
  result->src1 = make_reg(instr.get_src(1).get_reg(), idx);
  result->set_reg_info();
  return result;
}

/*!
 * Create an assembly passthrough for op src. Sets register info.
 */
std::shared_ptr<IR_Atomic> to_asm_src_reg(const std::string& str, Instruction& instr, int idx) {
  auto result = std::make_shared<IR_AsmOp_Atomic>(str);
  result->src0 = make_reg(instr.get_src(0).get_reg(), idx);
  result->set_reg_info();
  return result;
}

/*!
 * Create an assembly passthrough for op dst src. Sets register info.
 */
std::shared_ptr<IR_Atomic> to_asm_dst_reg_src_reg(const std::string& str,
                                                  Instruction& instr,
                                                  int idx) {
  auto result = std::make_shared<IR_AsmOp_Atomic>(str);
  result->dst = make_reg(instr.get_dst(0).get_reg(), idx);
  result->src0 = make_reg(instr.get_src(0).get_reg(), idx);
  result->set_reg_info();
  return result;
}

/*!
 * Convert an instruction atom to IR.
 */
std::shared_ptr<IR> instr_atom_to_ir(const InstructionAtom& ia, int idx) {
  switch (ia.kind) {
    case InstructionAtom::REGISTER:
      return make_reg(ia.get_reg(), idx);
    case InstructionAtom::VU_Q:
      return std::make_shared<IR_AsmReg>(IR_AsmReg::VU_Q);
    case InstructionAtom::VU_ACC:
      return std::make_shared<IR_AsmReg>(IR_AsmReg::VU_ACC);
    case InstructionAtom::IMM:
      return make_int(ia.get_imm());
    default:
      assert(false);
  }
}

///////////////////////////////
// Assembly
///////////////////////////////

/*!
 * Convert an assembly operation to a IR. Sets register info.
 */
std::shared_ptr<IR_Atomic> to_asm_automatic(const std::string& str, Instruction& instr, int idx) {
  auto result = std::make_shared<IR_AsmOp_Atomic>(str);
  assert(instr.n_dst < 2);
  assert(instr.n_src < 4);
  if (instr.n_dst >= 1) {
    result->dst = instr_atom_to_ir(instr.get_dst(0), idx);
  }

  if (instr.n_src >= 1) {
    result->src0 = instr_atom_to_ir(instr.get_src(0), idx);
  }

  if (instr.n_src >= 2) {
    result->src1 = instr_atom_to_ir(instr.get_src(1), idx);
  }

  if (instr.n_src >= 3) {
    result->src1 = instr_atom_to_ir(instr.get_src(2), idx);
  }

  result->set_reg_info();
  return result;
}

/*!
 * Convert subu instruction to assembly op. GOAL doesn't generate subu's without inline assembly.
 */
std::shared_ptr<IR_Atomic> try_subu(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::SUBU, {}, {}, {})) {
    return to_asm_reg_reg_reg("subu", instr, idx);
  }
  return nullptr;
}

/*!
 * Convert sllv instruction to assembly op. GOAL doesn't generate sllv's without inline assembly.
 */
std::shared_ptr<IR_Atomic> try_sllv(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::SLLV, {}, {}, make_gpr(Reg::R0))) {
    return to_asm_reg_reg_reg("sllv", instr, idx);
  }
  return nullptr;
}

///////////////////////////////
// Logical
///////////////////////////////

/*!
 * OR (logical or of registers) is used three ways:
 * 1. set a register to #f
 * 2. set a register to the value of another register
 * 3. logical OR
 */
std::shared_ptr<IR_Atomic> try_or(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::OR, {}, make_gpr(Reg::S7), make_gpr(Reg::R0))) {
    // set value to #f : or dest, s7, r0
    auto dest = instr.get_dst(0).get_reg();
    auto op = make_set_atomic(IR_Set_Atomic::REG_64, make_reg(dest, idx), make_sym("#f"));
    op->write_regs.push_back(dest);
    op->reg_info_set = true;
    return op;
  } else if (is_gpr_3(instr, InstructionKind::OR, {}, make_gpr(Reg::R0), make_gpr(Reg::R0))) {
    auto dest = instr.get_dst(0).get_reg();
    auto op = make_set_atomic(IR_Set_Atomic::REG_64, make_reg(dest, idx),
                              std::make_shared<IR_IntegerConstant>(0));
    op->write_regs.push_back(dest);
    op->reg_info_set = true;
    return op;
  } else if (is_gpr_3(instr, InstructionKind::OR, {}, {}, make_gpr(Reg::R0))) {
    // set register from register : or dest, source, r0
    auto dest = instr.get_dst(0).get_reg();
    auto src = instr.get_src(0).get_reg();
    auto op = make_set_atomic(IR_Set_Atomic::REG_64, make_reg(dest, idx), make_reg(src, idx));
    op->write_regs.push_back(dest);
    op->read_regs.push_back(src);
    op->reg_info_set = true;
    return op;
  } else {
    // actually do a logical OR of two registers.
    auto op = make_set_atomic(
        IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_IntMath2>(IR_IntMath2::OR, make_reg(instr.get_src(0).get_reg(), idx),
                                      make_reg(instr.get_src(1).get_reg(), idx)));
    op->update_reginfo_self<IR_IntMath2>(1, 2, 0);
    return op;
  }
  return nullptr;
}

/*!
 * ORI (logical OR of register and 16-bit immediate) is used two ways:
 * 1. Set a register to a 16-bit constant
 * 2. logical OR with constant
 */
std::shared_ptr<IR_Atomic> try_ori(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::ORI && instr.get_src(0).is_reg(make_gpr(Reg::R0)) &&
      instr.get_src(1).is_imm()) {
    // load a constant.
    auto dst = instr.get_dst(0).get_reg();
    auto op = make_set_atomic(IR_Set_Atomic::REG_64, make_reg(dst, idx),
                              make_int(instr.get_src(1).get_imm()));
    op->write_regs.push_back(dst);
    op->reg_info_set = true;
    return op;
  } else if (instr.kind == InstructionKind::ORI && instr.get_src(1).is_imm()) {
    // do logical OR with a constant.
    auto dst = instr.get_dst(0).get_reg();
    auto op = make_set_atomic(
        IR_Set_Atomic::REG_64, make_reg(dst, idx),
        std::make_shared<IR_IntMath2>(IR_IntMath2::OR, make_reg(instr.get_src(0).get_reg(), idx),
                                      make_int(instr.get_src(1).get_imm())));
    op->write_regs.push_back(dst);
    op->reg_info_set = true;
    return op;
  }
  return nullptr;
}

/*!
 * POR - recognize POR as a move between 128-bit registers.
 */
std::shared_ptr<IR_Atomic> try_por(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::POR, {}, {}, make_gpr(Reg::R0))) {
    // move a 128-bit integer.
    auto dst = instr.get_dst(0).get_reg();
    auto src = instr.get_src(0).get_reg();
    auto op = make_set_atomic(IR_Set_Atomic::REG_I128, make_reg(dst, idx), make_reg(src, idx));
    op->write_regs.push_back(dst);
    op->read_regs.push_back(src);
    op->reg_info_set = true;
    return op;
  }
  return nullptr;
}

///////////////////////////////
// Moves
///////////////////////////////

/*!
 * MTC1 (move to coprocessor 1) move from GPR to FPR.
 */
std::shared_ptr<IR_Atomic> try_mtc1(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::MTC1) {
    auto op = make_set_atomic(IR_Set_Atomic::GPR_TO_FPR, make_reg(instr.get_dst(0).get_reg(), idx),
                              make_reg(instr.get_src(0).get_reg(), idx));
    op->update_reginfo_regreg();
    return op;
  }
  return nullptr;
}

/*!
 * MFC1 (move from coprocessor 1) move from FPR to GPR.
 */
std::shared_ptr<IR_Atomic> try_mfc1(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::MFC1) {
    auto op =
        make_set_atomic(IR_Set_Atomic::FPR_TO_GPR64, make_reg(instr.get_dst(0).get_reg(), idx),
                        make_reg(instr.get_src(0).get_reg(), idx));
    op->update_reginfo_regreg();
    return op;
  }
  return nullptr;
}

///////////////////////////////
// Loads
///////////////////////////////

/*!
 * LWC1 : load value into FPR.
 * 1. load static float (FP relative)
 * 2. load from address
 * 3. load at offset from address.
 */
std::shared_ptr<IR_Atomic> try_lwc1(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::LWC1 && instr.get_dst(0).is_reg() &&
      instr.get_src(0).is_link_or_label() && instr.get_src(1).is_reg(make_gpr(Reg::FP))) {
    // fp relative, use an IR_StaticAddress.
    auto dst = instr.get_dst(0).get_reg();
    auto op = make_set_atomic(
        IR_Set_Atomic::LOAD, make_reg(dst, idx),
        std::make_shared<IR_Load>(
            IR_Load::FLOAT, 4, std::make_shared<IR_StaticAddress>(instr.get_src(0).get_label())));
    op->write_regs.push_back(dst);
    op->reg_info_set = true;
    return op;
  } else if (instr.kind == InstructionKind::LWC1 && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm() && instr.get_src(0).get_imm() == 0) {
    // offset is zero, so eliminate it.
    auto dst = instr.get_dst(0).get_reg();
    auto src = instr.get_src(1).get_reg();
    auto op = make_set_atomic(IR_Set_Atomic::LOAD, make_reg(dst, idx),
                              std::make_shared<IR_Load>(IR_Load::FLOAT, 4, make_reg(src, idx)));
    op->write_regs.push_back(dst);
    op->read_regs.push_back(src);
    op->reg_info_set = true;
    return op;
  } else if (instr.kind == InstructionKind::LWC1 && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm()) {
    // nonzero offset, create compound expression to add the offset.
    auto dst = instr.get_dst(0).get_reg();
    auto src = instr.get_src(1).get_reg();
    auto op =
        make_set_atomic(IR_Set_Atomic::LOAD, make_reg(dst, idx),
                        std::make_shared<IR_Load>(
                            IR_Load::FLOAT, 4,
                            std::make_shared<IR_IntMath2>(
                                IR_IntMath2::ADD, make_reg(src, idx),
                                std::make_shared<IR_IntegerConstant>(instr.get_src(0).get_imm()))));
    op->write_regs.push_back(dst);
    op->read_regs.push_back(src);
    op->reg_info_set = true;
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_lhu(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::LHU && instr.get_dst(0).is_reg() &&
      instr.get_src(0).is_link_or_label() && instr.get_src(1).is_reg(make_gpr(Reg::FP))) {
    // static load
    auto op =
        make_set_atomic(IR_Set_Atomic::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_Load>(
                            IR_Load::UNSIGNED, 2,
                            std::make_shared<IR_StaticAddress>(instr.get_src(0).get_label())));
    op->update_reginfo_self<IR_Load>(1, 0, 0);
    return op;
  } else if (instr.kind == InstructionKind::LHU && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm() && instr.get_src(0).get_imm() == 0) {
    // no offset load
    auto op = make_set_atomic(
        IR_Set_Atomic::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_Load>(IR_Load::UNSIGNED, 2, make_reg(instr.get_src(1).get_reg(), idx)));
    op->update_reginfo_self<IR_Load>(1, 1, 0);
    return op;
  } else if (instr.kind == InstructionKind::LHU && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm()) {
    // load with offset
    auto op =
        make_set_atomic(IR_Set_Atomic::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_Load>(
                            IR_Load::UNSIGNED, 2,
                            std::make_shared<IR_IntMath2>(
                                IR_IntMath2::ADD, make_reg(instr.get_src(1).get_reg(), idx),
                                std::make_shared<IR_IntegerConstant>(instr.get_src(0).get_imm()))));
    op->update_reginfo_self<IR_Load>(1, 1, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_lh(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::LH && instr.get_dst(0).is_reg() &&
      instr.get_src(0).is_link_or_label() && instr.get_src(1).is_reg(make_gpr(Reg::FP))) {
    // static load
    auto op = make_set_atomic(
        IR_Set_Atomic::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_Load>(
            IR_Load::SIGNED, 2, std::make_shared<IR_StaticAddress>(instr.get_src(0).get_label())));
    op->update_reginfo_self<IR_Load>(1, 0, 0);
    return op;
  } else if (instr.kind == InstructionKind::LH && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm() && instr.get_src(0).get_imm() == 0) {
    // no offset load
    auto op = make_set_atomic(
        IR_Set_Atomic::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_Load>(IR_Load::SIGNED, 2, make_reg(instr.get_src(1).get_reg(), idx)));
    op->update_reginfo_self<IR_Load>(1, 1, 0);
    return op;
  } else if (instr.kind == InstructionKind::LH && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm()) {
    // offset load
    auto op =
        make_set_atomic(IR_Set_Atomic::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_Load>(
                            IR_Load::SIGNED, 2,
                            std::make_shared<IR_IntMath2>(
                                IR_IntMath2::ADD, make_reg(instr.get_src(1).get_reg(), idx),
                                std::make_shared<IR_IntegerConstant>(instr.get_src(0).get_imm()))));
    op->update_reginfo_self<IR_Load>(1, 1, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_lb(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::LB && instr.get_dst(0).is_reg() &&
      instr.get_src(0).is_link_or_label() && instr.get_src(1).is_reg(make_gpr(Reg::FP))) {
    // static load
    auto op = make_set_atomic(
        IR_Set_Atomic::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_Load>(
            IR_Load::SIGNED, 1, std::make_shared<IR_StaticAddress>(instr.get_src(0).get_label())));
    op->update_reginfo_self<IR_Load>(1, 0, 0);
    return op;
  } else if (instr.kind == InstructionKind::LB && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm() && instr.get_src(0).get_imm() == 0) {
    // no offset load
    auto op = make_set_atomic(
        IR_Set_Atomic::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_Load>(IR_Load::SIGNED, 1, make_reg(instr.get_src(1).get_reg(), idx)));
    op->update_reginfo_self<IR_Load>(1, 1, 0);
    return op;
  } else if (instr.kind == InstructionKind::LB && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm()) {
    // offset load
    auto op =
        make_set_atomic(IR_Set_Atomic::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_Load>(
                            IR_Load::SIGNED, 1,
                            std::make_shared<IR_IntMath2>(
                                IR_IntMath2::ADD, make_reg(instr.get_src(1).get_reg(), idx),
                                std::make_shared<IR_IntegerConstant>(instr.get_src(0).get_imm()))));
    op->update_reginfo_self<IR_Load>(1, 1, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_lbu(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::LBU && instr.get_dst(0).is_reg() &&
      instr.get_src(0).is_link_or_label() && instr.get_src(1).is_reg(make_gpr(Reg::FP))) {
    // static load
    auto op =
        make_set_atomic(IR_Set_Atomic::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_Load>(
                            IR_Load::UNSIGNED, 1,
                            std::make_shared<IR_StaticAddress>(instr.get_src(0).get_label())));
    op->update_reginfo_self<IR_Load>(1, 0, 0);
    return op;
  } else if (instr.kind == InstructionKind::LBU && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm() && instr.get_src(0).get_imm() == 0) {
    // no offset load
    auto op = make_set_atomic(
        IR_Set_Atomic::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_Load>(IR_Load::UNSIGNED, 1, make_reg(instr.get_src(1).get_reg(), idx)));
    op->update_reginfo_self<IR_Load>(1, 1, 0);
    return op;
  } else if (instr.kind == InstructionKind::LBU && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm()) {
    // offset load
    auto op =
        make_set_atomic(IR_Set_Atomic::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_Load>(
                            IR_Load::UNSIGNED, 1,
                            std::make_shared<IR_IntMath2>(
                                IR_IntMath2::ADD, make_reg(instr.get_src(1).get_reg(), idx),
                                std::make_shared<IR_IntegerConstant>(instr.get_src(0).get_imm()))));
    op->update_reginfo_self<IR_Load>(1, 1, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_ld(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::LD && instr.get_dst(0).is_reg() &&
      instr.get_src(0).is_link_or_label() && instr.get_src(1).is_reg(make_gpr(Reg::FP))) {
    // static load
    auto op =
        make_set_atomic(IR_Set_Atomic::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_Load>(
                            IR_Load::UNSIGNED, 8,
                            std::make_shared<IR_StaticAddress>(instr.get_src(0).get_label())));
    op->update_reginfo_self<IR_Load>(1, 0, 0);
    return op;
  } else if (instr.kind == InstructionKind::LD && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm() && instr.get_src(0).get_imm() == 0) {
    // no offset load
    auto op = make_set_atomic(
        IR_Set_Atomic::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_Load>(IR_Load::UNSIGNED, 8, make_reg(instr.get_src(1).get_reg(), idx)));
    op->update_reginfo_self<IR_Load>(1, 1, 0);
    return op;
  } else if (instr.kind == InstructionKind::LD && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm()) {
    // offset load
    auto op =
        make_set_atomic(IR_Set_Atomic::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_Load>(
                            IR_Load::UNSIGNED, 8,
                            std::make_shared<IR_IntMath2>(
                                IR_IntMath2::ADD, make_reg(instr.get_src(1).get_reg(), idx),
                                std::make_shared<IR_IntegerConstant>(instr.get_src(0).get_imm()))));
    op->update_reginfo_self<IR_Load>(1, 1, 0);
    return op;
  }
  return nullptr;
}

// TODO SPECIAL
std::shared_ptr<IR_Atomic> try_lw(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::LW && instr.get_dst(0).is_reg(make_gpr(Reg::R0)) &&
      instr.get_src(0).is_imm() && instr.get_src(0).get_imm() == 2 &&
      instr.get_src(1).is_reg(make_gpr(Reg::R0))) {
    auto op = std::make_shared<IR_Breakpoint_Atomic>();
    op->reg_info_set = true;
    return op;
  } else if (instr.kind == InstructionKind::LW && instr.get_src(1).is_reg(make_gpr(Reg::S7)) &&
             instr.get_src(0).kind == InstructionAtom::IMM_SYM) {
    // symbol load
    auto dst = instr.get_dst(0).get_reg();
    auto op = make_set_atomic(IR_Set_Atomic::SYM_LOAD, make_reg(dst, idx),
                              make_sym_value(instr.get_src(0).get_sym()));
    op->write_regs.push_back(dst);
    op->reg_info_set = true;
    return op;
  } else if (instr.kind == InstructionKind::LW && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_link_or_label() && instr.get_src(1).is_reg(make_gpr(Reg::FP))) {
    // static load
    auto op = make_set_atomic(
        IR_Set_Atomic::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_Load>(
            IR_Load::SIGNED, 4, std::make_shared<IR_StaticAddress>(instr.get_src(0).get_label())));
    op->update_reginfo_self<IR_Load>(1, 0, 0);
    return op;
  } else if (instr.kind == InstructionKind::LW && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm() && instr.get_src(0).get_imm() == 0) {
    // no offset load
    auto op = make_set_atomic(
        IR_Set_Atomic::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_Load>(IR_Load::SIGNED, 4, make_reg(instr.get_src(1).get_reg(), idx)));
    op->update_reginfo_self<IR_Load>(1, 1, 0);
    return op;
  } else if (instr.kind == InstructionKind::LW && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm()) {
    // offset load
    auto op =
        make_set_atomic(IR_Set_Atomic::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_Load>(
                            IR_Load::SIGNED, 4,
                            std::make_shared<IR_IntMath2>(
                                IR_IntMath2::ADD, make_reg(instr.get_src(1).get_reg(), idx),
                                std::make_shared<IR_IntegerConstant>(instr.get_src(0).get_imm()))));
    op->update_reginfo_self<IR_Load>(1, 1, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_lwu(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::LWU && instr.get_dst(0).is_reg() &&
      instr.get_src(0).is_link_or_label() && instr.get_src(1).is_reg(make_gpr(Reg::FP))) {
    // static load
    auto op =
        make_set_atomic(IR_Set_Atomic::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_Load>(
                            IR_Load::UNSIGNED, 4,
                            std::make_shared<IR_StaticAddress>(instr.get_src(0).get_label())));
    op->update_reginfo_self<IR_Load>(1, 0, 0);
    return op;
  } else if (instr.kind == InstructionKind::LWU && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm() && instr.get_src(0).get_imm() == 0) {
    // no offset load
    auto op = make_set_atomic(
        IR_Set_Atomic::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_Load>(IR_Load::UNSIGNED, 4, make_reg(instr.get_src(1).get_reg(), idx)));
    op->update_reginfo_self<IR_Load>(1, 1, 0);
    return op;
  } else if (instr.kind == InstructionKind::LWU && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm()) {
    // offset load
    auto op =
        make_set_atomic(IR_Set_Atomic::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_Load>(
                            IR_Load::UNSIGNED, 4,
                            std::make_shared<IR_IntMath2>(
                                IR_IntMath2::ADD, make_reg(instr.get_src(1).get_reg(), idx),
                                std::make_shared<IR_IntegerConstant>(instr.get_src(0).get_imm()))));
    op->update_reginfo_self<IR_Load>(1, 1, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_lq(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::LQ && instr.get_src(1).is_reg(make_gpr(Reg::S7)) &&
      instr.get_src(0).kind == InstructionAtom::IMM_SYM) {
    assert(false);
  } else if (instr.kind == InstructionKind::LQ && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_link_or_label() && instr.get_src(1).is_reg(make_gpr(Reg::FP))) {
    // static
    auto op =
        make_set_atomic(IR_Set_Atomic::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_Load>(
                            IR_Load::UNSIGNED, 16,
                            std::make_shared<IR_StaticAddress>(instr.get_src(0).get_label())));
    op->update_reginfo_self<IR_Load>(1, 0, 0);
    return op;
  } else if (instr.kind == InstructionKind::LQ && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm() && instr.get_src(0).get_imm() == 0) {
    // no offset
    auto op = make_set_atomic(IR_Set_Atomic::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                              std::make_shared<IR_Load>(IR_Load::UNSIGNED, 16,
                                                        make_reg(instr.get_src(1).get_reg(), idx)));
    op->update_reginfo_self<IR_Load>(1, 1, 0);
    return op;
  } else if (instr.kind == InstructionKind::LQ && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm()) {
    // offset
    auto op =
        make_set_atomic(IR_Set_Atomic::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_Load>(
                            IR_Load::UNSIGNED, 16,
                            std::make_shared<IR_IntMath2>(
                                IR_IntMath2::ADD, make_reg(instr.get_src(1).get_reg(), idx),
                                std::make_shared<IR_IntegerConstant>(instr.get_src(0).get_imm()))));
    op->update_reginfo_self<IR_Load>(1, 1, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_dsll(Instruction& instr, int idx) {
  if (is_gpr_2_imm_int(instr, InstructionKind::DSLL, {}, {}, {})) {
    auto op =
        make_set_atomic(IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_IntMath2>(IR_IntMath2::LEFT_SHIFT,
                                                      make_reg(instr.get_src(0).get_reg(), idx),
                                                      make_int(instr.get_src(1).get_imm())));
    op->update_reginfo_self<IR_IntMath2>(1, 1, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_dsll32(Instruction& instr, int idx) {
  if (is_gpr_2_imm_int(instr, InstructionKind::DSLL32, {}, {}, {})) {
    auto op =
        make_set_atomic(IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_IntMath2>(IR_IntMath2::LEFT_SHIFT,
                                                      make_reg(instr.get_src(0).get_reg(), idx),
                                                      make_int(32 + instr.get_src(1).get_imm())));
    op->update_reginfo_self<IR_IntMath2>(1, 1, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_dsra(Instruction& instr, int idx) {
  if (is_gpr_2_imm_int(instr, InstructionKind::DSRA, {}, {}, {})) {
    auto op =
        make_set_atomic(IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_IntMath2>(IR_IntMath2::RIGHT_SHIFT_ARITH,
                                                      make_reg(instr.get_src(0).get_reg(), idx),
                                                      make_int(instr.get_src(1).get_imm())));
    op->update_reginfo_self<IR_IntMath2>(1, 1, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_dsra32(Instruction& instr, int idx) {
  if (is_gpr_2_imm_int(instr, InstructionKind::DSRA32, {}, {}, {})) {
    auto op =
        make_set_atomic(IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_IntMath2>(IR_IntMath2::RIGHT_SHIFT_ARITH,
                                                      make_reg(instr.get_src(0).get_reg(), idx),
                                                      make_int(32 + instr.get_src(1).get_imm())));
    op->update_reginfo_self<IR_IntMath2>(1, 1, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_dsrl(Instruction& instr, int idx) {
  if (is_gpr_2_imm_int(instr, InstructionKind::DSRL, {}, {}, {})) {
    auto op =
        make_set_atomic(IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_IntMath2>(IR_IntMath2::RIGHT_SHIFT_LOGIC,
                                                      make_reg(instr.get_src(0).get_reg(), idx),
                                                      make_int(instr.get_src(1).get_imm())));
    op->update_reginfo_self<IR_IntMath2>(1, 1, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_dsrl32(Instruction& instr, int idx) {
  if (is_gpr_2_imm_int(instr, InstructionKind::DSRL32, {}, {}, {})) {
    auto op =
        make_set_atomic(IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_IntMath2>(IR_IntMath2::RIGHT_SHIFT_LOGIC,
                                                      make_reg(instr.get_src(0).get_reg(), idx),
                                                      make_int(32 + instr.get_src(1).get_imm())));
    op->update_reginfo_self<IR_IntMath2>(1, 1, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_float_math_2(Instruction& instr,
                                            int idx,
                                            InstructionKind instr_kind,
                                            IR_FloatMath2::Kind ir_kind) {
  if (is_gpr_3(instr, instr_kind, {}, {}, {})) {
    auto dst = instr.get_dst(0).get_reg();
    auto src0 = instr.get_src(0).get_reg();
    auto src1 = instr.get_src(1).get_reg();
    auto op = make_set_atomic(
        IR_Set_Atomic::REG_FLT, make_reg(dst, idx),
        std::make_shared<IR_FloatMath2>(ir_kind, make_reg(src0, idx), make_reg(src1, idx)));
    op->write_regs.push_back(dst);
    op->read_regs.push_back(src0);
    op->read_regs.push_back(src1);
    op->reg_info_set = true;
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_daddiu(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::DADDIU && instr.get_src(0).is_reg(make_gpr(Reg::S7)) &&
      instr.get_src(1).kind == InstructionAtom::IMM_SYM) {
    auto op = make_set_atomic(IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                              make_sym(instr.get_src(1).get_sym()));
    op->write_regs.push_back(instr.get_dst(0).get_reg());
    op->reg_info_set = true;
    return op;
  } else if (instr.kind == InstructionKind::DADDIU && instr.get_src(0).is_reg(make_gpr(Reg::S7)) &&
             instr.get_src(1).is_imm() && instr.get_src(1).get_imm() == -10) {
    auto op = make_set_atomic(IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                              std::make_shared<IR_EmptyPair>());
    op->write_regs.push_back(instr.get_dst(0).get_reg());
    op->reg_info_set = true;
    return op;
  } else if (instr.kind == InstructionKind::DADDIU && instr.get_src(0).is_reg(make_gpr(Reg::S7)) &&
             instr.get_src(1).is_imm() && instr.get_src(1).get_imm() == -32768) {
    auto op = make_set_atomic(IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                              std::make_shared<IR_SymbolValue>("__START-OF-TABLE__"));
    op->write_regs.push_back(instr.get_dst(0).get_reg());
    op->reg_info_set = true;
    return op;
  } else if (instr.kind == InstructionKind::DADDIU && instr.get_src(0).is_reg(make_gpr(Reg::S7)) &&
             instr.get_src(1).is_imm() && instr.get_src(1).get_imm() == FIX_SYM_TRUE) {
    auto op = make_set_atomic(IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                              std::make_shared<IR_Symbol>("#t"));
    op->write_regs.push_back(instr.get_dst(0).get_reg());
    op->reg_info_set = true;
    return op;
  } else if (instr.kind == InstructionKind::DADDIU && instr.get_src(0).is_reg(make_gpr(Reg::FP)) &&
             instr.get_src(1).kind == InstructionAtom::LABEL) {
    auto op = make_set_atomic(IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                              std::make_shared<IR_StaticAddress>(instr.get_src(1).get_label()));
    op->write_regs.push_back(instr.get_dst(0).get_reg());
    op->reg_info_set = true;
    return op;
  } else if (instr.kind == InstructionKind::DADDIU) {
    auto op = make_set_atomic(
        IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_IntMath2>(IR_IntMath2::ADD, make_reg(instr.get_src(0).get_reg(), idx),
                                      make_int(instr.get_src(1).get_imm())));
    op->update_reginfo_self<IR_IntMath2>(1, 1, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_daddu(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::DADDU, {}, make_gpr(Reg::R0), {})) {
    auto op = make_set_atomic(
        IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_IntMath2>(IR_IntMath2::ADD, make_reg(instr.get_src(1).get_reg(), idx),
                                      std::make_shared<IR_IntegerConstant>(0)));
    op->update_reginfo_self<IR_IntMath2>(1, 1, 0);
    return op;
  } else if (is_gpr_3(instr, InstructionKind::DADDU, {}, {}, {}) &&
             !instr.get_src(0).is_reg(make_gpr(Reg::S7)) &&
             !instr.get_src(1).is_reg(make_gpr(Reg::S7))) {
    auto op = make_set_atomic(
        IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_IntMath2>(IR_IntMath2::ADD, make_reg(instr.get_src(0).get_reg(), idx),
                                      make_reg(instr.get_src(1).get_reg(), idx)));
    op->update_reginfo_self<IR_IntMath2>(1, 2, 0);
    return op;
  }
  return to_asm_reg_reg_reg("daddu", instr, idx);
}

std::shared_ptr<IR_Atomic> try_dsubu(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::DSUBU, {}, make_gpr(Reg::R0), {}) &&
      !instr.get_src(0).is_reg(make_gpr(Reg::S7)) && !instr.get_src(1).is_reg(make_gpr(Reg::S7))) {
    auto op = make_set_atomic(
        IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_IntMath1>(IR_IntMath1::NEG, make_reg(instr.get_src(1).get_reg(), idx)));
    op->update_reginfo_self<IR_IntMath1>(1, 1, 0);
    return op;
  } else if (is_gpr_3(instr, InstructionKind::DSUBU, {}, {}, {}) &&
             !instr.get_src(0).is_reg(make_gpr(Reg::S7)) &&
             !instr.get_src(1).is_reg(make_gpr(Reg::S7))) {
    auto op = make_set_atomic(
        IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_IntMath2>(IR_IntMath2::SUB, make_reg(instr.get_src(0).get_reg(), idx),
                                      make_reg(instr.get_src(1).get_reg(), idx)));
    op->update_reginfo_self<IR_IntMath2>(1, 2, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_mult3(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::MULT3, {}, {}, {}) &&
      !instr.get_src(0).is_reg(make_gpr(Reg::S7)) && !instr.get_src(1).is_reg(make_gpr(Reg::S7))) {
    auto op =
        make_set_atomic(IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_IntMath2>(IR_IntMath2::MUL_SIGNED,
                                                      make_reg(instr.get_src(0).get_reg(), idx),
                                                      make_reg(instr.get_src(1).get_reg(), idx)));
    op->update_reginfo_self<IR_IntMath2>(1, 2, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_multu3(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::MULTU3, {}, {}, {}) &&
      !instr.get_src(0).is_reg(make_gpr(Reg::S7)) && !instr.get_src(1).is_reg(make_gpr(Reg::S7))) {
    auto op =
        make_set_atomic(IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_IntMath2>(IR_IntMath2::MUL_UNSIGNED,
                                                      make_reg(instr.get_src(0).get_reg(), idx),
                                                      make_reg(instr.get_src(1).get_reg(), idx)));
    op->update_reginfo_self<IR_IntMath2>(1, 2, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_and(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::AND, {}, {}, {}) &&
      !instr.get_src(0).is_reg(make_gpr(Reg::S7)) && !instr.get_src(1).is_reg(make_gpr(Reg::S7))) {
    auto op = make_set_atomic(
        IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_IntMath2>(IR_IntMath2::AND, make_reg(instr.get_src(0).get_reg(), idx),
                                      make_reg(instr.get_src(1).get_reg(), idx)));
    op->update_reginfo_self<IR_IntMath2>(1, 2, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_andi(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::ANDI) {
    auto op = make_set_atomic(
        IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_IntMath2>(IR_IntMath2::AND, make_reg(instr.get_src(0).get_reg(), idx),
                                      make_int(instr.get_src(1).get_imm())));
    op->update_reginfo_self<IR_IntMath2>(1, 1, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_xori(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::XORI) {
    auto op = make_set_atomic(
        IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_IntMath2>(IR_IntMath2::XOR, make_reg(instr.get_src(0).get_reg(), idx),
                                      make_int(instr.get_src(1).get_imm())));
    op->update_reginfo_self<IR_IntMath2>(1, 1, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_nor(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::NOR, {}, {}, {}) &&
      !instr.get_src(0).is_reg(make_gpr(Reg::S7)) && instr.get_src(1).is_reg(make_gpr(Reg::R0))) {
    auto op = make_set_atomic(
        IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_IntMath1>(IR_IntMath1::NOT, make_reg(instr.get_src(0).get_reg(), idx)));
    op->update_reginfo_self<IR_IntMath1>(1, 1, 0);
    return op;
  } else if (is_gpr_3(instr, InstructionKind::NOR, {}, {}, {}) &&
             !instr.get_src(0).is_reg(make_gpr(Reg::S7)) &&
             !instr.get_src(1).is_reg(make_gpr(Reg::S7))) {
    auto op = make_set_atomic(
        IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_IntMath2>(IR_IntMath2::NOR, make_reg(instr.get_src(0).get_reg(), idx),
                                      make_reg(instr.get_src(1).get_reg(), idx)));
    op->update_reginfo_self<IR_IntMath2>(1, 2, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_xor(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::XOR, {}, {}, {}) &&
      !instr.get_src(0).is_reg(make_gpr(Reg::S7)) && !instr.get_src(1).is_reg(make_gpr(Reg::S7))) {
    auto op = make_set_atomic(
        IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_IntMath2>(IR_IntMath2::XOR, make_reg(instr.get_src(0).get_reg(), idx),
                                      make_reg(instr.get_src(1).get_reg(), idx)));
    op->update_reginfo_self<IR_IntMath2>(1, 2, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_addiu(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::ADDIU && instr.get_src(0).is_reg(make_gpr(Reg::R0))) {
    auto dest = instr.get_dst(0).get_reg();
    auto op = make_set_atomic(IR_Set_Atomic::REG_64, make_reg(dest, idx),
                              make_int(instr.get_src(1).get_imm()));
    op->write_regs.push_back(dest);
    op->reg_info_set = true;
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_lui(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::LUI && instr.get_src(0).is_imm()) {
    auto dest = instr.get_dst(0).get_reg();
    auto op = make_set_atomic(IR_Set_Atomic::REG_64, make_reg(dest, idx),
                              make_int(instr.get_src(0).get_imm() << 16));
    op->write_regs.push_back(dest);
    op->reg_info_set = true;
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_sll(Instruction& instr, int idx) {
  (void)idx;
  if (is_nop(instr)) {
    auto op = std::make_shared<IR_Nop_Atomic>();
    op->reg_info_set = true;
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_dsrav(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::DSRAV, {}, {}, {}) &&
      !instr.get_src(0).is_reg(make_gpr(Reg::S7)) && !instr.get_src(1).is_reg(make_gpr(Reg::S7))) {
    auto op =
        make_set_atomic(IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_IntMath2>(IR_IntMath2::RIGHT_SHIFT_ARITH,
                                                      make_reg(instr.get_src(0).get_reg(), idx),
                                                      make_reg(instr.get_src(1).get_reg(), idx)));
    op->update_reginfo_self<IR_IntMath2>(1, 2, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_dsrlv(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::DSRLV, {}, {}, {}) &&
      !instr.get_src(0).is_reg(make_gpr(Reg::S7)) && !instr.get_src(1).is_reg(make_gpr(Reg::S7))) {
    auto op =
        make_set_atomic(IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_IntMath2>(IR_IntMath2::RIGHT_SHIFT_LOGIC,
                                                      make_reg(instr.get_src(0).get_reg(), idx),
                                                      make_reg(instr.get_src(1).get_reg(), idx)));
    op->update_reginfo_self<IR_IntMath2>(1, 2, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_dsllv(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::DSLLV, {}, {}, {}) &&
      !instr.get_src(0).is_reg(make_gpr(Reg::S7)) && !instr.get_src(1).is_reg(make_gpr(Reg::S7))) {
    auto op =
        make_set_atomic(IR_Set_Atomic::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_IntMath2>(IR_IntMath2::LEFT_SHIFT,
                                                      make_reg(instr.get_src(0).get_reg(), idx),
                                                      make_reg(instr.get_src(1).get_reg(), idx)));
    op->update_reginfo_self<IR_IntMath2>(1, 2, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_sw(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::SW && instr.get_src(1).is_sym() &&
      instr.get_src(2).is_reg(make_gpr(Reg::S7))) {
    auto src = instr.get_src(0).get_reg();
    auto op = std::make_shared<IR_Set_Atomic>(
        IR_Set_Atomic::SYM_STORE, make_sym_value(instr.get_src(1).get_sym()), make_reg(src, idx));
    op->read_regs.push_back(src);
    op->reg_info_set = true;
    return op;
  } else if (instr.kind == InstructionKind::SW && instr.get_src(1).is_imm()) {
    if (instr.get_src(1).get_imm() == 0) {
      auto op = std::make_shared<IR_Store_Atomic>(IR_Store_Atomic::INTEGER,
                                                  make_reg(instr.get_src(2).get_reg(), idx),
                                                  make_reg(instr.get_src(0).get_reg(), idx), 4);
      op->update_reginfo_self(0, 2, 0);
      return op;
    } else {
      if (instr.get_src(0).is_reg(make_gpr(Reg::S7))) {
        // store false
        auto op = std::make_shared<IR_Store_Atomic>(
            IR_Store_Atomic::Kind::INTEGER,
            std::make_shared<IR_IntMath2>(
                IR_IntMath2::ADD, make_reg(instr.get_src(2).get_reg(), idx),
                std::make_shared<IR_IntegerConstant>(instr.get_src(1).get_imm())),
            make_sym("#f"), 4);
        op->update_reginfo_self(0, 1, 0);
        return op;
      } else {
        auto op = std::make_shared<IR_Store_Atomic>(
            IR_Store_Atomic::INTEGER,
            std::make_shared<IR_IntMath2>(
                IR_IntMath2::ADD, make_reg(instr.get_src(2).get_reg(), idx),
                std::make_shared<IR_IntegerConstant>(instr.get_src(1).get_imm())),
            make_reg(instr.get_src(0).get_reg(), idx), 4);
        op->update_reginfo_self(0, 2, 0);
        return op;
      }
    }
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_sb(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::SB && instr.get_src(1).is_imm()) {
    if (instr.get_src(1).get_imm() == 0) {
      if (instr.get_src(0).is_reg(make_gpr(Reg::R0))) {
        auto op = std::make_shared<IR_Store_Atomic>(IR_Store_Atomic::INTEGER,
                                                    make_reg(instr.get_src(2).get_reg(), idx),
                                                    std::make_shared<IR_IntegerConstant>(0), 1);
        op->update_reginfo_self(0, 1, 0);
        return op;
      } else {
        auto op = std::make_shared<IR_Store_Atomic>(IR_Store_Atomic::INTEGER,
                                                    make_reg(instr.get_src(2).get_reg(), idx),
                                                    make_reg(instr.get_src(0).get_reg(), idx), 1);
        op->update_reginfo_self(0, 2, 0);
        return op;
      }

    } else {
      if (instr.get_src(0).is_reg(make_gpr(Reg::R0))) {
        auto op = std::make_shared<IR_Store_Atomic>(
            IR_Store_Atomic::INTEGER,
            std::make_shared<IR_IntMath2>(
                IR_IntMath2::ADD, make_reg(instr.get_src(2).get_reg(), idx),
                std::make_shared<IR_IntegerConstant>(instr.get_src(1).get_imm())),
            std::make_shared<IR_IntegerConstant>(0), 1);
        op->update_reginfo_self(0, 1, 0);
        return op;
      } else {
        auto op = std::make_shared<IR_Store_Atomic>(
            IR_Store_Atomic::INTEGER,
            std::make_shared<IR_IntMath2>(
                IR_IntMath2::ADD, make_reg(instr.get_src(2).get_reg(), idx),
                std::make_shared<IR_IntegerConstant>(instr.get_src(1).get_imm())),
            make_reg(instr.get_src(0).get_reg(), idx), 1);
        op->update_reginfo_self(0, 2, 0);
        return op;
      }
    }
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_sh(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::SH && instr.get_src(1).is_imm()) {
    auto op = std::make_shared<IR_Store_Atomic>(
        IR_Store_Atomic::INTEGER,
        std::make_shared<IR_IntMath2>(
            IR_IntMath2::ADD, make_reg(instr.get_src(2).get_reg(), idx),
            std::make_shared<IR_IntegerConstant>(instr.get_src(1).get_imm())),
        make_reg(instr.get_src(0).get_reg(), idx), 2);
    op->update_reginfo_self(0, 2, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_sd(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::SD && instr.get_src(1).is_imm()) {
    auto op = std::make_shared<IR_Store_Atomic>(
        IR_Store_Atomic::INTEGER,
        std::make_shared<IR_IntMath2>(
            IR_IntMath2::ADD, make_reg(instr.get_src(2).get_reg(), idx),
            std::make_shared<IR_IntegerConstant>(instr.get_src(1).get_imm())),
        make_reg(instr.get_src(0).get_reg(), idx), 8);
    op->update_reginfo_self(0, 2, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_sq(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::SQ && instr.get_src(1).is_imm()) {
    auto op = std::make_shared<IR_Store_Atomic>(
        IR_Store_Atomic::INTEGER,
        std::make_shared<IR_IntMath2>(
            IR_IntMath2::ADD, make_reg(instr.get_src(2).get_reg(), idx),
            std::make_shared<IR_IntegerConstant>(instr.get_src(1).get_imm())),
        make_reg(instr.get_src(0).get_reg(), idx), 16);
    op->update_reginfo_self(0, 2, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_swc1(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::SWC1 && instr.get_src(1).is_imm()) {
    auto op = std::make_shared<IR_Store_Atomic>(
        IR_Store_Atomic::FLOAT,
        std::make_shared<IR_IntMath2>(
            IR_IntMath2::ADD, make_reg(instr.get_src(2).get_reg(), idx),
            std::make_shared<IR_IntegerConstant>(instr.get_src(1).get_imm())),
        make_reg(instr.get_src(0).get_reg(), idx), 4);
    op->update_reginfo_self(0, 2, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_cvtws(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::CVTWS) {
    auto dst = instr.get_dst(0).get_reg();
    auto src = instr.get_src(0).get_reg();
    auto op = make_set_atomic(
        IR_Set_Atomic::REG_FLT, make_reg(dst, idx),
        std::make_shared<IR_FloatMath1>(IR_FloatMath1::FLOAT_TO_INT, make_reg(src, idx)));
    op->write_regs.push_back(dst);
    op->read_regs.push_back(src);
    op->reg_info_set = true;
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_cvtsw(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::CVTSW) {
    auto dst = instr.get_dst(0).get_reg();
    auto src = instr.get_src(0).get_reg();
    auto op = make_set_atomic(
        IR_Set_Atomic::REG_FLT, make_reg(dst, idx),
        std::make_shared<IR_FloatMath1>(IR_FloatMath1::INT_TO_FLOAT, make_reg(src, idx)));
    op->write_regs.push_back(dst);
    op->read_regs.push_back(src);
    op->reg_info_set = true;
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_float_math_1(Instruction& instr,
                                            int idx,
                                            InstructionKind ikind,
                                            IR_FloatMath1::Kind irkind) {
  if (instr.kind == ikind) {
    auto dst = instr.get_dst(0).get_reg();
    auto src = instr.get_src(0).get_reg();
    auto op = make_set_atomic(IR_Set_Atomic::REG_FLT, make_reg(dst, idx),
                              std::make_shared<IR_FloatMath1>(irkind, make_reg(src, idx)));
    op->write_regs.push_back(dst);
    op->read_regs.push_back(src);
    op->reg_info_set = true;
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_movs(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::MOVS) {
    auto dst = instr.get_dst(0).get_reg();
    auto src = instr.get_src(0).get_reg();
    auto op = make_set_atomic(IR_Set_Atomic::REG_FLT, make_reg(dst, idx), make_reg(src, idx));
    op->write_regs.push_back(dst);
    op->read_regs.push_back(src);
    op->reg_info_set = true;
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_movn(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::MOVN, {}, make_gpr(Reg::S7), {})) {
    auto dst = instr.get_dst(0).get_reg();
    auto src = instr.get_src(1).get_reg();
    auto op = make_set_atomic(IR_Set_Atomic::REG_64, make_reg(dst, idx),
                              std::make_shared<IR_CMoveF>(make_reg(src, idx), false));
    op->write_regs.push_back(dst);
    op->read_regs.push_back(src);
    op->reg_info_set = true;
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_movz(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::MOVZ, {}, make_gpr(Reg::S7), {})) {
    auto dst = instr.get_dst(0).get_reg();
    auto src = instr.get_src(1).get_reg();
    auto op = make_set_atomic(IR_Set_Atomic::REG_64, make_reg(dst, idx),
                              std::make_shared<IR_CMoveF>(make_reg(src, idx), true));
    op->write_regs.push_back(dst);
    op->read_regs.push_back(src);
    op->reg_info_set = true;
    return op;
  }
  return nullptr;
}

// TWO Instructions
std::shared_ptr<IR_Atomic> try_div(Instruction& instr, Instruction& next_instr, int idx) {
  if (instr.kind == InstructionKind::DIV && instr.get_src(0).is_reg() &&
      instr.get_src(1).is_reg() && next_instr.kind == InstructionKind::MFLO &&
      next_instr.get_dst(0).is_reg()) {
    auto op =
        make_set_atomic(IR_Set_Atomic::REG_64, make_reg(next_instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_IntMath2>(IR_IntMath2::DIV_SIGNED,
                                                      make_reg(instr.get_src(0).get_reg(), idx),
                                                      make_reg(instr.get_src(1).get_reg(), idx)));
    op->update_reginfo_self<IR_IntMath2>(1, 2, 0);
    return op;
  } else if (instr.kind == InstructionKind::DIV && instr.get_src(0).is_reg() &&
             instr.get_src(1).is_reg() && next_instr.kind == InstructionKind::MFHI &&
             next_instr.get_dst(0).is_reg()) {
    auto op =
        make_set_atomic(IR_Set_Atomic::REG_64, make_reg(next_instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_IntMath2>(IR_IntMath2::MOD_SIGNED,
                                                      make_reg(instr.get_src(0).get_reg(), idx),
                                                      make_reg(instr.get_src(1).get_reg(), idx)));
    op->update_reginfo_self<IR_IntMath2>(1, 2, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_divu(Instruction& instr, Instruction& next_instr, int idx) {
  if (instr.kind == InstructionKind::DIVU && instr.get_src(0).is_reg() &&
      instr.get_src(1).is_reg() && next_instr.kind == InstructionKind::MFLO &&
      next_instr.get_dst(0).is_reg()) {
    auto op =
        make_set_atomic(IR_Set_Atomic::REG_64, make_reg(next_instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_IntMath2>(IR_IntMath2::DIV_UNSIGNED,
                                                      make_reg(instr.get_src(0).get_reg(), idx),
                                                      make_reg(instr.get_src(1).get_reg(), idx)));
    op->update_reginfo_self<IR_IntMath2>(1, 2, 0);
    return op;
  } else if (instr.kind == InstructionKind::DIVU && instr.get_src(0).is_reg() &&
             instr.get_src(1).is_reg() && next_instr.kind == InstructionKind::MFHI &&
             next_instr.get_dst(0).is_reg()) {
    auto op =
        make_set_atomic(IR_Set_Atomic::REG_64, make_reg(next_instr.get_dst(0).get_reg(), idx),
                        std::make_shared<IR_IntMath2>(IR_IntMath2::MOD_UNSIGNED,
                                                      make_reg(instr.get_src(0).get_reg(), idx),
                                                      make_reg(instr.get_src(1).get_reg(), idx)));
    op->update_reginfo_self<IR_IntMath2>(1, 2, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_jalr(Instruction& instr, Instruction& next_instr, int idx) {
  (void)idx;
  if (instr.kind == InstructionKind::JALR && instr.get_dst(0).is_reg(make_gpr(Reg::RA)) &&
      instr.get_src(0).is_reg(make_gpr(Reg::T9)) &&
      is_gpr_2_imm_int(next_instr, InstructionKind::SLL, make_gpr(Reg::V0), make_gpr(Reg::RA), 0)) {
    auto op = std::make_shared<IR_Call_Atomic>();

    // for now, we assume no arguments, but a return.
    // todo - clobber fprs
    auto temps = {Reg::V1, Reg::A0, Reg::A1, Reg::A2, Reg::A3, Reg::T0, Reg::T1, Reg::T2,
                  Reg::T3, Reg::T4, Reg::T5, Reg::T6, Reg::T7, Reg::T8, Reg::T9};
    for (auto& r : temps) {
      op->clobber_regs.emplace_back(Reg::GPR, r);
    }

    op->read_regs.emplace_back(Reg::GPR, Reg::T9);
    op->write_regs.emplace_back(Reg::GPR, Reg::V0);  // may "write" a none.
    op->reg_info_set = true;
    return op;
  }
  return nullptr;
}

BranchDelay get_branch_delay(Instruction& i, int idx) {
  if (is_nop(i)) {
    // no read, write, clobber
    return BranchDelay(BranchDelay::NOP);
  } else if (is_gpr_3(i, InstructionKind::OR, {}, make_gpr(Reg::S7), make_gpr(Reg::R0))) {
    BranchDelay b(BranchDelay::SET_REG_FALSE);
    auto dst = i.get_dst(0).get_reg();
    b.destination = make_reg(dst, idx);
    b.write_regs.push_back(dst);
    return b;
  } else if (is_gpr_3(i, InstructionKind::OR, {}, {}, make_gpr(Reg::R0))) {
    BranchDelay b(BranchDelay::SET_REG_REG);
    auto dst = i.get_dst(0).get_reg();
    auto src = i.get_src(0).get_reg();
    b.destination = make_reg(dst, idx);
    b.source = make_reg(src, idx);
    b.write_regs.push_back(dst);
    b.read_regs.push_back(src);
    return b;
  } else if (i.kind == InstructionKind::DADDIU && i.get_src(0).is_reg(make_gpr(Reg::S7)) &&
             i.get_src(1).is_imm() && i.get_src(1).get_imm() == 8) {
    BranchDelay b(BranchDelay::SET_REG_TRUE);
    auto dst = i.get_dst(0).get_reg();
    b.destination = make_reg(dst, idx);
    b.write_regs.push_back(dst);
    return b;
  } else if (i.kind == InstructionKind::LW && i.get_src(1).is_reg(make_gpr(Reg::S7)) &&
             i.get_src(0).is_sym()) {
    if (i.get_src(0).get_sym() == "binteger") {
      BranchDelay b(BranchDelay::SET_BINTEGER);
      auto dst = i.get_dst(0).get_reg();
      b.destination = make_reg(dst, idx);
      b.write_regs.push_back(dst);
      return b;
    } else if (i.get_src(0).get_sym() == "pair") {
      BranchDelay b(BranchDelay::SET_PAIR);
      auto dst = i.get_dst(0).get_reg();
      b.destination = make_reg(dst, idx);
      b.write_regs.push_back(dst);
      return b;
    } else {
      assert(false);
    }
  } else if (i.kind == InstructionKind::DSLLV) {
    // this is used for ash?
    BranchDelay b(BranchDelay::DSLLV);
    auto dst = i.get_dst(0).get_reg();
    auto src0 = i.get_src(0).get_reg();
    auto src2 = i.get_src(1).get_reg();
    b.destination = make_reg(dst, idx);
    b.source = make_reg(src0, idx);
    b.source2 = make_reg(src2, idx);
    b.write_regs.push_back(dst);
    b.read_regs.push_back(src0);
    b.read_regs.push_back(src2);
    return b;
  } else if (is_gpr_3(i, InstructionKind::DSUBU, {}, make_gpr(Reg::R0), {})) {
    // this is used for abs?
    BranchDelay b(BranchDelay::NEGATE);
    auto dst = i.get_dst(0).get_reg();
    auto src = i.get_src(1).get_reg();
    b.destination = make_reg(dst, idx);
    b.source = make_reg(src, idx);
    b.write_regs.push_back(dst);
    b.read_regs.push_back(src);
    return b;
  }
  BranchDelay b(BranchDelay::UNKNOWN);
  return b;
}

std::shared_ptr<IR_Atomic> try_bne(Instruction& instr, Instruction& next_instr, int idx) {
  if (instr.kind == InstructionKind::BNE && instr.get_src(1).is_reg(make_gpr(Reg::R0))) {
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::NONZERO, make_reg(instr.get_src(0).get_reg(), idx), nullptr, nullptr),
        instr.get_src(2).get_label(), get_branch_delay(next_instr, idx), false);
    op->update_reginfo_self(0, 1, 0);
    return op;
  } else if (instr.kind == InstructionKind::BNE && instr.get_src(0).is_reg(make_gpr(Reg::S7))) {
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::TRUTHY, make_reg(instr.get_src(1).get_reg(), idx), nullptr, nullptr),
        instr.get_src(2).get_label(), get_branch_delay(next_instr, idx), false);
    op->update_reginfo_self(0, 1, 0);
    return op;
  } else if (instr.kind == InstructionKind::BNE) {
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::NOT_EQUAL, make_reg(instr.get_src(0).get_reg(), idx),
                  make_reg(instr.get_src(1).get_reg(), idx), nullptr),
        instr.get_src(2).get_label(), get_branch_delay(next_instr, idx), false);
    op->update_reginfo_self(0, 2, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_bnel(Instruction& instr, Instruction& next_instr, int idx) {
  if (instr.kind == InstructionKind::BNEL && instr.get_src(0).is_reg(make_gpr(Reg::S7))) {
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::TRUTHY, make_reg(instr.get_src(1).get_reg(), idx), nullptr, nullptr),
        instr.get_src(2).get_label(), get_branch_delay(next_instr, idx), true);
    op->update_reginfo_self(0, 1, 0);
    return op;
  } else if (instr.kind == InstructionKind::BNEL && instr.get_src(1).is_reg(make_gpr(Reg::R0))) {
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::NONZERO, make_reg(instr.get_src(0).get_reg(), idx), nullptr, nullptr),
        instr.get_src(2).get_label(), get_branch_delay(next_instr, idx), true);
    op->update_reginfo_self(0, 1, 0);
    return op;
  } else if (instr.kind == InstructionKind::BNEL) {
    //    return std::make_shared<IR_Branch2>(IR_Branch2::NOT_EQUAL, instr.get_src(2).get_label(),
    //                                        make_reg(instr.get_src(0).get_reg(), idx),
    //                                        make_reg(instr.get_src(1).get_reg(), idx),
    //                                        get_branch_delay(next_instr, idx), true);
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_beql(Instruction& instr, Instruction& next_instr, int idx) {
  if (instr.kind == InstructionKind::BEQL && instr.get_src(0).is_reg(make_gpr(Reg::S7))) {
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::FALSE, make_reg(instr.get_src(1).get_reg(), idx), nullptr, nullptr),
        instr.get_src(2).get_label(), get_branch_delay(next_instr, idx), true);
    op->update_reginfo_self(0, 1, 0);
    return op;
  } else if (instr.kind == InstructionKind::BEQL && instr.get_src(1).is_reg(make_gpr(Reg::R0))) {
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::ZERO, make_reg(instr.get_src(0).get_reg(), idx), nullptr, nullptr),
        instr.get_src(2).get_label(), get_branch_delay(next_instr, idx), true);
    op->update_reginfo_self(0, 1, 0);
    return op;
  } else if (instr.kind == InstructionKind::BEQL) {
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::EQUAL, make_reg(instr.get_src(0).get_reg(), idx),
                  make_reg(instr.get_src(1).get_reg(), idx), nullptr),
        instr.get_src(2).get_label(), get_branch_delay(next_instr, idx), true);
    op->update_reginfo_self(0, 2, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_beq(Instruction& instr, Instruction& next_instr, int idx) {
  if (instr.kind == InstructionKind::BEQ && instr.get_src(0).is_reg(make_gpr(Reg::R0)) &&
      instr.get_src(1).is_reg(make_gpr(Reg::R0))) {
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::ALWAYS, nullptr, nullptr, nullptr), instr.get_src(2).get_label(),
        get_branch_delay(next_instr, idx), false);
    op->update_reginfo_self(0, 0, 0);
    return op;
  } else if (instr.kind == InstructionKind::BEQ && instr.get_src(0).is_reg(make_gpr(Reg::S7))) {
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::FALSE, make_reg(instr.get_src(1).get_reg(), idx), nullptr, nullptr),
        instr.get_src(2).get_label(), get_branch_delay(next_instr, idx), false);
    op->update_reginfo_self(0, 1, 0);
    return op;
  } else if (instr.kind == InstructionKind::BEQ && instr.get_src(1).is_reg(make_gpr(Reg::R0))) {
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::ZERO, make_reg(instr.get_src(0).get_reg(), idx), nullptr, nullptr),
        instr.get_src(2).get_label(), get_branch_delay(next_instr, idx), false);
    op->update_reginfo_self(0, 1, 0);
    return op;
  } else if (instr.kind == InstructionKind::BEQ) {
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::EQUAL, make_reg(instr.get_src(0).get_reg(), idx),
                  make_reg(instr.get_src(1).get_reg(), idx), nullptr),
        instr.get_src(2).get_label(), get_branch_delay(next_instr, idx), false);
    op->update_reginfo_self(0, 2, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_bgtzl(Instruction& instr, Instruction& next_instr, int idx) {
  if (instr.kind == InstructionKind::BGTZL) {
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::GREATER_THAN_ZERO_SIGNED, make_reg(instr.get_src(0).get_reg(), idx),
                  nullptr, nullptr),
        instr.get_src(1).get_label(), get_branch_delay(next_instr, idx), true);
    op->update_reginfo_self(0, 1, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_bgezl(Instruction& instr, Instruction& next_instr, int idx) {
  if (instr.kind == InstructionKind::BGEZL) {
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::GEQ_ZERO_SIGNED, make_reg(instr.get_src(0).get_reg(), idx), nullptr,
                  nullptr),
        instr.get_src(1).get_label(), get_branch_delay(next_instr, idx), true);
    op->update_reginfo_self(0, 1, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_bltzl(Instruction& instr, Instruction& next_instr, int idx) {
  if (instr.kind == InstructionKind::BLTZL) {
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::LESS_THAN_ZERO, make_reg(instr.get_src(0).get_reg(), idx), nullptr,
                  nullptr),
        instr.get_src(1).get_label(), get_branch_delay(next_instr, idx), true);
    op->update_reginfo_self(0, 1, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_daddiu(Instruction& i0, Instruction& i1, int idx) {
  if (i0.kind == InstructionKind::DADDIU && i1.kind == InstructionKind::MOVN &&
      i0.get_src(0).get_reg() == make_gpr(Reg::S7)) {
    auto dst_reg = i0.get_dst(0).get_reg();
    auto src_reg = i1.get_src(1).get_reg();
    assert(i0.get_src(0).get_reg() == make_gpr(Reg::S7));
    assert(i0.get_src(1).get_imm() == 8);
    assert(i1.get_dst(0).get_reg() == dst_reg);
    assert(i1.get_src(0).get_reg() == make_gpr(Reg::S7));
    auto op = make_set_atomic(IR_Set_Atomic::REG_64, make_reg(dst_reg, idx),
                              std::make_shared<IR_Compare>(Condition(
                                  Condition::ZERO, make_reg(src_reg, idx), nullptr, nullptr)));
    op->write_regs.push_back(dst_reg);
    op->read_regs.push_back(src_reg);
    op->reg_info_set = true;
    return op;
  } else if (i0.kind == InstructionKind::DADDIU && i1.kind == InstructionKind::MOVZ &&
             i0.get_src(0).get_reg() == make_gpr(Reg::S7)) {
    auto dst_reg = i0.get_dst(0).get_reg();
    auto src_reg = i1.get_src(1).get_reg();
    assert(i0.get_src(0).get_reg() == make_gpr(Reg::S7));
    assert(i0.get_src(1).get_imm() == 8);
    assert(i1.get_dst(0).get_reg() == dst_reg);
    assert(i1.get_src(0).get_reg() == make_gpr(Reg::S7));
    auto op = make_set_atomic(IR_Set_Atomic::REG_64, make_reg(dst_reg, idx),
                              std::make_shared<IR_Compare>(Condition(
                                  Condition::NONZERO, make_reg(src_reg, idx), nullptr, nullptr)));
    op->write_regs.push_back(dst_reg);
    op->read_regs.push_back(src_reg);
    op->reg_info_set = true;
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_lui(Instruction& i0, Instruction& i1, int idx) {
  if (i0.kind == InstructionKind::LUI && i1.kind == InstructionKind::ORI &&
      i0.get_src(0).is_label() && i1.get_src(1).is_label()) {
    assert(i0.get_dst(0).get_reg() == i1.get_src(0).get_reg());
    assert(i0.get_src(0).get_label() == i1.get_src(1).get_label());
    auto op = make_set_atomic(IR_Set_Atomic::REG_64, make_reg(i1.get_dst(0).get_reg(), idx),
                              std::make_shared<IR_StaticAddress>(i0.get_src(0).get_label()));
    if (i0.get_dst(0).get_reg() != i1.get_dst(0).get_reg()) {
      op->clobber = make_reg(i0.get_dst(0).get_reg(), idx);
      op->clobber_regs.push_back(i0.get_dst(0).get_reg());
    }
    op->write_regs.push_back(i1.get_dst(0).get_reg());
    op->reg_info_set = true;
    return op;
  } else if (i0.kind == InstructionKind::LUI && i1.kind == InstructionKind::ORI &&
             i0.get_src(0).is_imm() && i1.get_src(1).is_imm() &&
             i0.get_dst(0).get_reg() == i1.get_src(0).get_reg()) {
    auto op = make_set_atomic(
        IR_Set_Atomic::REG_64, make_reg(i1.get_dst(0).get_reg(), idx),
        make_int((int64_t(i1.get_src(1).get_imm()) + int64_t(i0.get_src(0).get_imm() << 16))));
    if (i0.get_dst(0).get_reg() != i1.get_dst(0).get_reg()) {
      op->clobber = make_reg(i0.get_dst(0).get_reg(), idx);
      op->clobber_regs.push_back(i0.get_dst(0).get_reg());
    }
    op->write_regs.push_back(i1.get_dst(0).get_reg());
    op->reg_info_set = true;
    return op;
  }
  return nullptr;
}

// note - this one is a little bit strange in how it's colored.
std::shared_ptr<IR_Atomic> try_slt(Instruction& i0, Instruction& i1, int idx) {
  if (is_gpr_3(i0, InstructionKind::SLT, {}, {}, {})) {
    auto temp = i0.get_dst(0).get_reg();
    auto left = i0.get_src(0).get_reg();
    auto right = i0.get_src(1).get_reg();
    if (is_gpr_3(i1, InstructionKind::MOVZ, left, right, temp)) {
      // success!
      auto result =
          make_set_atomic(IR_Set_Atomic::REG_64, make_reg(left, idx),
                          std::make_shared<IR_IntMath2>(IR_IntMath2::MIN_SIGNED,
                                                        make_reg(left, idx), make_reg(right, idx)));
      result->clobber = make_reg(temp, idx);
      result->clobber_regs.push_back(temp);
      result->write_regs.push_back(left);
      result->read_regs.push_back(right);
      result->read_regs.push_back(left);
      result->reg_info_set = true;
      return result;
    }

    if (is_gpr_3(i1, InstructionKind::MOVN, left, right, temp)) {
      // success!
      auto result =
          make_set_atomic(IR_Set_Atomic::REG_64, make_reg(left, idx),
                          std::make_shared<IR_IntMath2>(IR_IntMath2::MAX_SIGNED,
                                                        make_reg(left, idx), make_reg(right, idx)));
      result->clobber = make_reg(temp, idx);
      result->clobber_regs.push_back(temp);
      result->write_regs.push_back(left);
      result->read_regs.push_back(right);
      result->read_regs.push_back(left);
      result->reg_info_set = true;
      return result;
    }
  }
  return nullptr;
}

// THREE OP
std::shared_ptr<IR_Atomic> try_lui(Instruction& i0, Instruction& i1, Instruction& i2, int idx) {
  if (i0.kind == InstructionKind::LUI && i1.kind == InstructionKind::ORI &&
      i0.get_src(0).is_label() && i1.get_src(1).is_label() &&
      is_gpr_3(i2, InstructionKind::ADDU, {}, make_gpr(Reg::FP), {})) {
    assert(i0.get_dst(0).get_reg() == i1.get_src(0).get_reg());
    assert(i0.get_src(0).get_label() == i1.get_src(1).get_label());
    assert(i2.get_dst(0).get_reg() == i2.get_src(1).get_reg());
    assert(i2.get_dst(0).get_reg() == i1.get_dst(0).get_reg());
    auto op = make_set_atomic(IR_Set_Atomic::REG_64, make_reg(i1.get_dst(0).get_reg(), idx),
                              std::make_shared<IR_StaticAddress>(i0.get_src(0).get_label()));
    if (i0.get_dst(0).get_reg() != i1.get_dst(0).get_reg()) {
      op->clobber = make_reg(i0.get_dst(0).get_reg(), idx);
      op->clobber_regs.push_back(i0.get_dst(0).get_reg());
    }
    op->write_regs.push_back(i1.get_dst(0).get_reg());
    op->reg_info_set = true;
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_dsubu(Instruction& i0, Instruction& i1, Instruction& i2, int idx) {
  if (i0.kind == InstructionKind::DSUBU && i1.kind == InstructionKind::DADDIU &&
      i2.kind == InstructionKind::MOVN) {
    // check for equality
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    auto src1_reg = i0.get_src(1).get_reg();
    auto dst_reg = i1.get_dst(0).get_reg();
    assert(i1.get_src(0).get_reg() == make_gpr(Reg::S7));
    assert(i1.get_src(1).get_imm() == 8);
    assert(i2.get_dst(0).get_reg() == dst_reg);
    assert(i2.get_src(0).get_reg() == make_gpr(Reg::S7));
    assert(i2.get_src(1).get_reg() == clobber_reg);
    auto op = make_set_atomic(IR_Set_Atomic::REG_64, make_reg(dst_reg, idx),
                              std::make_shared<IR_Compare>(
                                  Condition(Condition::EQUAL, make_reg(src0_reg, idx),
                                            make_reg(src1_reg, idx), make_reg(clobber_reg, idx))));
    op->update_reginfo_self<IR_Compare>(1, 2, 1);
    return op;
  } else if (i0.kind == InstructionKind::DSUBU && i1.kind == InstructionKind::DADDIU &&
             i2.kind == InstructionKind::MOVZ) {
    // check for equality
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    auto src1_reg = i0.get_src(1).get_reg();
    auto dst_reg = i1.get_dst(0).get_reg();
    assert(i1.get_src(0).get_reg() == make_gpr(Reg::S7));
    assert(i1.get_src(1).get_imm() == 8);
    assert(i2.get_dst(0).get_reg() == dst_reg);
    assert(i2.get_src(0).get_reg() == make_gpr(Reg::S7));
    if (i2.get_src(1).get_reg() != clobber_reg) {
      return nullptr;  // TODO!
    }
    auto op = make_set_atomic(IR_Set_Atomic::REG_64, make_reg(dst_reg, idx),
                              std::make_shared<IR_Compare>(
                                  Condition(Condition::NOT_EQUAL, make_reg(src0_reg, idx),
                                            make_reg(src1_reg, idx), make_reg(clobber_reg, idx))));
    op->update_reginfo_self<IR_Compare>(1, 2, 1);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_slt(Instruction& i0, Instruction& i1, Instruction& i2, int idx) {
  if (i0.kind == InstructionKind::SLT && i1.kind == InstructionKind::BNE) {
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    auto src1_reg = i0.get_src(1).get_reg();
    assert(i1.get_src(0).get_reg() == clobber_reg);
    assert(i1.get_src(1).get_reg() == make_gpr(Reg::R0));
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::LESS_THAN_SIGNED, make_reg(src0_reg, idx), make_reg(src1_reg, idx),
                  make_reg(clobber_reg, idx)),
        i1.get_src(2).get_label(), get_branch_delay(i2, idx), false);
    op->update_reginfo_self(0, 2, 1);
    return op;
  } else if (i0.kind == InstructionKind::SLT && i1.kind == InstructionKind::DADDIU &&
             i2.kind == InstructionKind::MOVZ) {
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    auto src1_reg = i0.get_src(1).get_reg();
    auto dst_reg = i1.get_dst(0).get_reg();
    assert(i1.get_src(0).is_reg(make_gpr(Reg::S7)));
    assert(i1.get_src(1).get_imm() == 8);
    assert(i2.get_dst(0).get_reg() == dst_reg);
    assert(i2.get_src(0).get_reg() == make_gpr(Reg::S7));
    if (i2.get_src(1).get_reg() != clobber_reg) {
      return nullptr;  // TODO!
    }
    if (src1_reg == make_gpr(Reg::R0)) {
      auto op = make_set_atomic(
          IR_Set_Atomic::REG_64, make_reg(dst_reg, idx),
          std::make_shared<IR_Compare>(Condition(Condition::LESS_THAN_ZERO, make_reg(src0_reg, idx),
                                                 nullptr, make_reg(clobber_reg, idx))));
      op->update_reginfo_self<IR_Compare>(1, 1, 1);
      return op;
    } else {
      auto op = make_set_atomic(IR_Set_Atomic::REG_64, make_reg(dst_reg, idx),
                                std::make_shared<IR_Compare>(Condition(
                                    Condition::LESS_THAN_SIGNED, make_reg(src0_reg, idx),
                                    make_reg(src1_reg, idx), make_reg(clobber_reg, idx))));
      op->update_reginfo_self<IR_Compare>(1, 2, 1);
      return op;
    }

  } else if (i0.kind == InstructionKind::SLT && i1.kind == InstructionKind::BEQ) {
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    auto src1_reg = i0.get_src(1).get_reg();
    assert(i1.get_src(0).get_reg() == clobber_reg);
    assert(i1.get_src(1).get_reg() == make_gpr(Reg::R0));
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::GEQ_SIGNED, make_reg(src0_reg, idx), make_reg(src1_reg, idx),
                  make_reg(clobber_reg, idx)),
        i1.get_src(2).get_label(), get_branch_delay(i2, idx), false);
    op->update_reginfo_self(0, 2, 1);
    return op;
  } else if (i0.kind == InstructionKind::SLT && i1.kind == InstructionKind::DADDIU &&
             i2.kind == InstructionKind::MOVN) {
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    auto src1_reg = i0.get_src(1).get_reg();
    auto dst_reg = i1.get_dst(0).get_reg();
    assert(i1.get_src(0).is_reg(make_gpr(Reg::S7)));
    assert(i1.get_src(1).get_imm() == 8);
    assert(i2.get_dst(0).get_reg() == dst_reg);
    assert(i2.get_src(0).get_reg() == make_gpr(Reg::S7));
    if (i2.get_src(1).get_reg() != clobber_reg) {
      return nullptr;  // TODO!
    }
    auto op = make_set_atomic(IR_Set_Atomic::REG_64, make_reg(dst_reg, idx),
                              std::make_shared<IR_Compare>(
                                  Condition(Condition::GEQ_SIGNED, make_reg(src0_reg, idx),
                                            make_reg(src1_reg, idx), make_reg(clobber_reg, idx))));
    op->update_reginfo_self<IR_Compare>(1, 2, 1);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_slti(Instruction& i0, Instruction& i1, Instruction& i2, int idx) {
  auto src1 = make_int(i0.get_src(1).get_imm());
  if (i0.kind == InstructionKind::SLTI && i1.kind == InstructionKind::BNE) {
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    assert(i1.get_src(0).get_reg() == clobber_reg);
    assert(i1.get_src(1).get_reg() == make_gpr(Reg::R0));
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::LESS_THAN_SIGNED, make_reg(src0_reg, idx), src1,
                  make_reg(clobber_reg, idx)),
        i1.get_src(2).get_label(), get_branch_delay(i2, idx), false);
    op->update_reginfo_self(0, 1, 1);
    return op;
  } else if (i0.kind == InstructionKind::SLTI && i1.kind == InstructionKind::DADDIU &&
             i2.kind == InstructionKind::MOVZ) {
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    auto dst_reg = i1.get_dst(0).get_reg();
    assert(i1.get_src(0).is_reg(make_gpr(Reg::S7)));
    assert(i1.get_src(1).get_imm() == 8);
    assert(i2.get_dst(0).get_reg() == dst_reg);
    assert(i2.get_src(0).get_reg() == make_gpr(Reg::S7));
    if (i2.get_src(1).get_reg() != clobber_reg) {
      return nullptr;  // TODO!
    }
    auto op = make_set_atomic(
        IR_Set_Atomic::REG_64, make_reg(dst_reg, idx),
        std::make_shared<IR_Compare>(Condition(Condition::LESS_THAN_SIGNED, make_reg(src0_reg, idx),
                                               src1, make_reg(clobber_reg, idx))));
    op->update_reginfo_self<IR_Compare>(1, 1, 1);
    return op;
  } else if (i0.kind == InstructionKind::SLTI && i1.kind == InstructionKind::BEQ) {
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    assert(i1.get_src(0).get_reg() == clobber_reg);
    assert(i1.get_src(1).get_reg() == make_gpr(Reg::R0));
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::GEQ_SIGNED, make_reg(src0_reg, idx), src1, make_reg(clobber_reg, idx)),
        i1.get_src(2).get_label(), get_branch_delay(i2, idx), false);
    op->update_reginfo_self(0, 1, 1);
    return op;
  } else if (i0.kind == InstructionKind::SLTI && i1.kind == InstructionKind::DADDIU &&
             i2.kind == InstructionKind::MOVN) {
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    auto dst_reg = i1.get_dst(0).get_reg();
    assert(i1.get_src(0).is_reg(make_gpr(Reg::S7)));
    assert(i1.get_src(1).get_imm() == 8);
    assert(i2.get_dst(0).get_reg() == dst_reg);
    assert(i2.get_src(0).get_reg() == make_gpr(Reg::S7));
    if (i2.get_src(1).get_reg() != clobber_reg) {
      return nullptr;  // TODO!
    }
    auto op = make_set_atomic(
        IR_Set_Atomic::REG_64, make_reg(dst_reg, idx),
        std::make_shared<IR_Compare>(Condition(Condition::GEQ_SIGNED, make_reg(src0_reg, idx), src1,
                                               make_reg(clobber_reg, idx))));
    op->update_reginfo_self<IR_Compare>(1, 1, 1);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_sltiu(Instruction& i0, Instruction& i1, Instruction& i2, int idx) {
  auto src1 = make_int(i0.get_src(1).get_imm());
  if (i0.kind == InstructionKind::SLTIU && i1.kind == InstructionKind::BNE) {
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    assert(i1.get_src(0).get_reg() == clobber_reg);
    assert(i1.get_src(1).get_reg() == make_gpr(Reg::R0));
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::LESS_THAN_UNSIGNED, make_reg(src0_reg, idx), src1,
                  make_reg(clobber_reg, idx)),
        i1.get_src(2).get_label(), get_branch_delay(i2, idx), false);
    op->update_reginfo_self(0, 1, 1);
    return op;
  } else if (i0.kind == InstructionKind::SLTIU && i1.kind == InstructionKind::DADDIU &&
             i2.kind == InstructionKind::MOVZ) {
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    auto dst_reg = i1.get_dst(0).get_reg();
    assert(i1.get_src(0).is_reg(make_gpr(Reg::S7)));
    assert(i1.get_src(1).get_imm() == 8);
    assert(i2.get_dst(0).get_reg() == dst_reg);
    assert(i2.get_src(0).get_reg() == make_gpr(Reg::S7));
    if (i2.get_src(1).get_reg() != clobber_reg) {
      return nullptr;  // TODO!
    }
    auto op = make_set_atomic(IR_Set_Atomic::REG_64, make_reg(dst_reg, idx),
                              std::make_shared<IR_Compare>(Condition(Condition::LESS_THAN_UNSIGNED,
                                                                     make_reg(src0_reg, idx), src1,
                                                                     make_reg(clobber_reg, idx))));
    op->update_reginfo_self<IR_Compare>(1, 1, 1);
    return op;
  } else if (i0.kind == InstructionKind::SLTIU && i1.kind == InstructionKind::BEQ) {
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    assert(i1.get_src(0).get_reg() == clobber_reg);
    assert(i1.get_src(1).get_reg() == make_gpr(Reg::R0));
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::GEQ_UNSIGNED, make_reg(src0_reg, idx), src1,
                  make_reg(clobber_reg, idx)),
        i1.get_src(2).get_label(), get_branch_delay(i2, idx), false);
    op->update_reginfo_self(0, 1, 1);
    return op;
  } else if (i0.kind == InstructionKind::SLTIU && i1.kind == InstructionKind::DADDIU &&
             i2.kind == InstructionKind::MOVN) {
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    auto dst_reg = i1.get_dst(0).get_reg();
    assert(i1.get_src(0).is_reg(make_gpr(Reg::S7)));
    assert(i1.get_src(1).get_imm() == 8);
    assert(i2.get_dst(0).get_reg() == dst_reg);
    assert(i2.get_src(0).get_reg() == make_gpr(Reg::S7));
    if (i2.get_src(1).get_reg() != clobber_reg) {
      return nullptr;  // TODO!
    }
    auto op = make_set_atomic(
        IR_Set_Atomic::REG_64, make_reg(dst_reg, idx),
        std::make_shared<IR_Compare>(Condition(Condition::GEQ_UNSIGNED, make_reg(src0_reg, idx),
                                               src1, make_reg(clobber_reg, idx))));
    op->update_reginfo_self<IR_Compare>(1, 1, 1);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_ceqs(Instruction& i0, Instruction& i1, Instruction& i2, int idx) {
  if (i0.kind == InstructionKind::CEQS && i1.kind == InstructionKind::BC1T) {
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::FLOAT_EQUAL, make_reg(i0.get_src(0).get_reg(), idx),
                  make_reg(i0.get_src(1).get_reg(), idx), nullptr),
        i1.get_src(0).get_label(), get_branch_delay(i2, idx), false);
    op->update_reginfo_self(0, 2, 0);
    return op;
  } else if (i0.kind == InstructionKind::CEQS && i1.kind == InstructionKind::BC1F) {
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::FLOAT_NOT_EQUAL, make_reg(i0.get_src(0).get_reg(), idx),
                  make_reg(i0.get_src(1).get_reg(), idx), nullptr),
        i1.get_src(0).get_label(), get_branch_delay(i2, idx), false);
    op->update_reginfo_self(0, 2, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_clts(Instruction& i0, Instruction& i1, Instruction& i2, int idx) {
  if (i0.kind == InstructionKind::CLTS && i1.kind == InstructionKind::BC1T) {
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::FLOAT_LESS_THAN, make_reg(i0.get_src(0).get_reg(), idx),
                  make_reg(i0.get_src(1).get_reg(), idx), nullptr),
        i1.get_src(0).get_label(), get_branch_delay(i2, idx), false);
    op->update_reginfo_self(0, 2, 0);
    return op;
  } else if (i0.kind == InstructionKind::CLTS && i1.kind == InstructionKind::BC1F) {
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::FLOAT_GEQ, make_reg(i0.get_src(0).get_reg(), idx),
                  make_reg(i0.get_src(1).get_reg(), idx), nullptr),
        i1.get_src(0).get_label(), get_branch_delay(i2, idx), false);
    op->update_reginfo_self(0, 2, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_cles(Instruction& i0, Instruction& i1, Instruction& i2, int idx) {
  if (i0.kind == InstructionKind::CLES && i1.kind == InstructionKind::BC1T) {
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::FLOAT_LEQ, make_reg(i0.get_src(0).get_reg(), idx),
                  make_reg(i0.get_src(1).get_reg(), idx), nullptr),
        i1.get_src(0).get_label(), get_branch_delay(i2, idx), false);
    op->update_reginfo_self(0, 2, 0);
    return op;
  } else if (i0.kind == InstructionKind::CLES && i1.kind == InstructionKind::BC1F) {
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::FLOAT_GREATER_THAN, make_reg(i0.get_src(0).get_reg(), idx),
                  make_reg(i0.get_src(1).get_reg(), idx), nullptr),
        i1.get_src(0).get_label(), get_branch_delay(i2, idx), false);
    op->update_reginfo_self(0, 2, 0);
    return op;
  }
  return nullptr;
}

std::shared_ptr<IR_Atomic> try_sltu(Instruction& i0, Instruction& i1, Instruction& i2, int idx) {
  if (i0.kind == InstructionKind::SLTU && i1.kind == InstructionKind::BNE) {
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    auto src1_reg = i0.get_src(1).get_reg();
    assert(i1.get_src(0).get_reg() == clobber_reg);
    assert(i1.get_src(1).get_reg() == make_gpr(Reg::R0));
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::LESS_THAN_UNSIGNED, make_reg(src0_reg, idx), make_reg(src1_reg, idx),
                  make_reg(clobber_reg, idx)),
        i1.get_src(2).get_label(), get_branch_delay(i2, idx), false);
    op->update_reginfo_self(0, 2, 1);
    return op;
  } else if (i0.kind == InstructionKind::SLTU && i1.kind == InstructionKind::DADDIU &&
             i2.kind == InstructionKind::MOVZ) {
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    auto src1_reg = i0.get_src(1).get_reg();
    auto dst_reg = i1.get_dst(0).get_reg();
    assert(i1.get_src(0).is_reg(make_gpr(Reg::S7)));
    assert(i1.get_src(1).get_imm() == 8);
    assert(i2.get_dst(0).get_reg() == dst_reg);
    assert(i2.get_src(0).get_reg() == make_gpr(Reg::S7));
    if (i2.get_src(1).get_reg() != clobber_reg) {
      return nullptr;  // TODO!
    }
    auto op = make_set_atomic(IR_Set_Atomic::REG_64, make_reg(dst_reg, idx),
                              std::make_shared<IR_Compare>(
                                  Condition(Condition::LESS_THAN_UNSIGNED, make_reg(src0_reg, idx),
                                            make_reg(src1_reg, idx), make_reg(clobber_reg, idx))));
    op->update_reginfo_self<IR_Compare>(1, 2, 1);
    return op;
  } else if (i0.kind == InstructionKind::SLTU && i1.kind == InstructionKind::BEQ) {
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    auto src1_reg = i0.get_src(1).get_reg();
    assert(i1.get_src(0).get_reg() == clobber_reg);
    assert(i1.get_src(1).get_reg() == make_gpr(Reg::R0));
    auto op = std::make_shared<IR_Branch_Atomic>(
        Condition(Condition::GEQ_UNSIGNED, make_reg(src0_reg, idx), make_reg(src1_reg, idx),
                  make_reg(clobber_reg, idx)),
        i1.get_src(2).get_label(), get_branch_delay(i2, idx), false);
    op->update_reginfo_self(0, 2, 1);
    return op;
  } else if (i0.kind == InstructionKind::SLTU && i1.kind == InstructionKind::DADDIU &&
             i2.kind == InstructionKind::MOVN) {
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    auto src1_reg = i0.get_src(1).get_reg();
    auto dst_reg = i1.get_dst(0).get_reg();
    assert(i1.get_src(0).is_reg(make_gpr(Reg::S7)));
    assert(i1.get_src(1).get_imm() == 8);
    assert(i2.get_dst(0).get_reg() == dst_reg);
    assert(i2.get_src(0).get_reg() == make_gpr(Reg::S7));
    if (i2.get_src(1).get_reg() != clobber_reg) {
      return nullptr;  // TODO!
    }
    auto op = make_set_atomic(IR_Set_Atomic::REG_64, make_reg(dst_reg, idx),
                              std::make_shared<IR_Compare>(
                                  Condition(Condition::GEQ_UNSIGNED, make_reg(src0_reg, idx),
                                            make_reg(src1_reg, idx), make_reg(clobber_reg, idx))));
    op->update_reginfo_self<IR_Compare>(1, 2, 1);
    return op;
  }
  return nullptr;
}

// five op
std::shared_ptr<IR_Atomic> try_lwu(Instruction& i0,
                                   Instruction& i1,
                                   Instruction& i2,
                                   Instruction& i3,
                                   Instruction& i4,
                                   int idx) {
  (void)idx;
  auto s6 = make_gpr(Reg::S6);
  if (i0.kind == InstructionKind::LWU && i0.get_dst(0).is_reg(s6) &&
      i0.get_src(0).get_imm() == 44 && i0.get_src(1).is_reg(s6) &&
      i1.kind == InstructionKind::MTLO1 && i1.get_src(0).is_reg(s6) &&
      i2.kind == InstructionKind::LWU && i2.get_dst(0).is_reg(s6) &&
      i2.get_src(0).get_imm() == 12 && i2.get_src(1).is_reg(s6) &&
      i3.kind == InstructionKind::JALR && i3.get_dst(0).is_reg(make_gpr(Reg::RA)) &&
      i3.get_src(0).is_reg(s6) && i4.kind == InstructionKind::MFLO1 && i4.get_dst(0).is_reg(s6)) {
    auto op = std::make_shared<IR_Suspend_Atomic>();
    op->reg_info_set = true;
    return op;
  }
  return nullptr;
}

}  // namespace

void add_basic_ops_to_block(Function* func, const BasicBlock& block, LinkedObjectFile* file) {
  (void)file;
  for (int instr = block.start_word; instr < block.end_word; instr++) {
    auto& i = func->instructions.at(instr);

    int length = 0;

    std::shared_ptr<IR_Atomic> result = nullptr;
    if (instr + 4 < block.end_word) {
      auto& i1 = func->instructions.at(instr + 1);
      auto& i2 = func->instructions.at(instr + 2);
      auto& i3 = func->instructions.at(instr + 3);
      auto& i4 = func->instructions.at(instr + 4);
      switch (i.kind) {
        case InstructionKind::LWU:
          result = try_lwu(i, i1, i2, i3, i4, instr);
          break;
        default:
          result = nullptr;
      }
      if (result) {
        length = 5;
      }
    }

    if (!result && instr + 2 < block.end_word) {
      auto& next = func->instructions.at(instr + 1);
      auto& next_next = func->instructions.at(instr + 2);
      switch (i.kind) {
        case InstructionKind::DSUBU:
          result = try_dsubu(i, next, next_next, instr);
          break;
        case InstructionKind::SLT:
          result = try_slt(i, next, next_next, instr);
          break;
        case InstructionKind::SLTI:
          result = try_slti(i, next, next_next, instr);
          break;
        case InstructionKind::SLTU:
          result = try_sltu(i, next, next_next, instr);
          break;
        case InstructionKind::SLTIU:
          result = try_sltiu(i, next, next_next, instr);
          break;
        case InstructionKind::CEQS:
          result = try_ceqs(i, next, next_next, instr);
          break;
        case InstructionKind::CLTS:
          result = try_clts(i, next, next_next, instr);
          break;
        case InstructionKind::CLES:
          result = try_cles(i, next, next_next, instr);
          break;
        case InstructionKind::LUI:
          result = try_lui(i, next, next_next, instr);
          break;
        default:
          result = nullptr;
      }

      if (result) {
        length = 3;
      }
    }

    if (!result && instr + 1 < block.end_word) {
      auto& next = func->instructions.at(instr + 1);
      // single op failed, try double
      switch (i.kind) {
        case InstructionKind::DIV:
          result = try_div(i, next, instr);
          break;
        case InstructionKind::DIVU:
          result = try_divu(i, next, instr);
          break;
        case InstructionKind::JALR:
          result = try_jalr(i, next, instr);
          break;
        case InstructionKind::BNE:
          result = try_bne(i, next, instr);
          break;
        case InstructionKind::BNEL:
          result = try_bnel(i, next, instr);
          break;
        case InstructionKind::BEQ:
          result = try_beq(i, next, instr);
          break;
        case InstructionKind::BGTZL:
          result = try_bgtzl(i, next, instr);
          break;
        case InstructionKind::BGEZL:
          result = try_bgezl(i, next, instr);
          break;
        case InstructionKind::BLTZL:
          result = try_bltzl(i, next, instr);
          break;
        case InstructionKind::BEQL:
          result = try_beql(i, next, instr);
          break;
        case InstructionKind::DADDIU:
          result = try_daddiu(i, next, instr);
          break;
        case InstructionKind::LUI:
          result = try_lui(i, next, instr);
          break;
        case InstructionKind::SLT:
          result = try_slt(i, next, instr);
          break;
        default:
          result = nullptr;
      }

      if (result) {
        length = 2;
      }
    }

    if (!result) {
      switch (i.kind) {
        case InstructionKind::OR:
          result = try_or(i, instr);
          break;
        case InstructionKind::ORI:
          result = try_ori(i, instr);
          break;
        case InstructionKind::DADDIU:
          result = try_daddiu(i, instr);
          break;
        case InstructionKind::AND:
          result = try_and(i, instr);
          break;
        case InstructionKind::ANDI:
          result = try_andi(i, instr);
          break;
        case InstructionKind::XORI:
          result = try_xori(i, instr);
          break;
        case InstructionKind::NOR:
          result = try_nor(i, instr);
          break;
        case InstructionKind::XOR:
          result = try_xor(i, instr);
          break;
        case InstructionKind::LWC1:
          result = try_lwc1(i, instr);
          break;
        case InstructionKind::MTC1:
          result = try_mtc1(i, instr);
          break;
        case InstructionKind::DIVS:
          result = try_float_math_2(i, instr, InstructionKind::DIVS, IR_FloatMath2::DIV);
          break;
        case InstructionKind::SUBS:
          result = try_float_math_2(i, instr, InstructionKind::SUBS, IR_FloatMath2::SUB);
          break;
        case InstructionKind::ADDS:
          result = try_float_math_2(i, instr, InstructionKind::ADDS, IR_FloatMath2::ADD);
          break;
        case InstructionKind::MULS:
          result = try_float_math_2(i, instr, InstructionKind::MULS, IR_FloatMath2::MUL);
          break;
        case InstructionKind::ABSS:
          result = try_float_math_1(i, instr, InstructionKind::ABSS, IR_FloatMath1::ABS);
          break;
        case InstructionKind::NEGS:
          result = try_float_math_1(i, instr, InstructionKind::NEGS, IR_FloatMath1::NEG);
          break;
        case InstructionKind::SQRTS:
          result = try_float_math_1(i, instr, InstructionKind::SQRTS, IR_FloatMath1::SQRT);
          break;
        case InstructionKind::MINS:
          result = try_float_math_2(i, instr, InstructionKind::MINS, IR_FloatMath2::MIN);
          break;
        case InstructionKind::MAXS:
          result = try_float_math_2(i, instr, InstructionKind::MAXS, IR_FloatMath2::MAX);
          break;
        case InstructionKind::MOVS:
          result = try_movs(i, instr);
          break;
        case InstructionKind::MFC1:
          result = try_mfc1(i, instr);
          break;
        case InstructionKind::DADDU:
          result = try_daddu(i, instr);
          break;
        case InstructionKind::DSUBU:
          result = try_dsubu(i, instr);
          if (!result) {
            // fails if it uses s7 register.
            result = to_asm_automatic(i.op_name_to_string(), i, instr);
          }
          break;
        case InstructionKind::MULT3:
          result = try_mult3(i, instr);
          break;
        case InstructionKind::MULTU3:
          result = try_multu3(i, instr);
          break;
        case InstructionKind::POR:
          result = try_por(i, instr);
          if (!result) {
            result = to_asm_automatic(i.op_name_to_string(), i, instr);
          }
          break;
        case InstructionKind::LBU:
          result = try_lbu(i, instr);
          break;
        case InstructionKind::LHU:
          result = try_lhu(i, instr);
          break;
        case InstructionKind::LB:
          result = try_lb(i, instr);
          break;
        case InstructionKind::LH:
          result = try_lh(i, instr);
          break;
        case InstructionKind::LW:
          result = try_lw(i, instr);
          break;
        case InstructionKind::LWU:
          result = try_lwu(i, instr);
          break;
        case InstructionKind::LD:
          result = try_ld(i, instr);
          break;
        case InstructionKind::LQ:
          result = try_lq(i, instr);
          break;
        case InstructionKind::DSRA:
          result = try_dsra(i, instr);
          break;
        case InstructionKind::DSRA32:
          result = try_dsra32(i, instr);
          break;
        case InstructionKind::DSRL:
          result = try_dsrl(i, instr);
          break;
        case InstructionKind::DSRL32:
          result = try_dsrl32(i, instr);
          break;
        case InstructionKind::DSLL:
          result = try_dsll(i, instr);
          break;
        case InstructionKind::DSLL32:
          result = try_dsll32(i, instr);
          break;
        case InstructionKind::ADDIU:
          result = try_addiu(i, instr);
          if (!result) {
            result = to_asm_automatic(i.op_name_to_string(), i, instr);
          }
          break;
        case InstructionKind::LUI:
          result = try_lui(i, instr);
          break;
        case InstructionKind::SLL:
          result = try_sll(i, instr);
          if (!result) {
            result = to_asm_automatic(i.op_name_to_string(), i, instr);
          }
          break;
        case InstructionKind::SB:
          result = try_sb(i, instr);
          break;
        case InstructionKind::SH:
          result = try_sh(i, instr);
          break;
        case InstructionKind::SW:
          result = try_sw(i, instr);
          break;
        case InstructionKind::SD:
          result = try_sd(i, instr);
          break;
        case InstructionKind::SQ:
          result = try_sq(i, instr);
          break;
        case InstructionKind::SWC1:
          result = try_swc1(i, instr);
          break;
        case InstructionKind::CVTWS:
          result = try_cvtws(i, instr);
          break;
        case InstructionKind::CVTSW:
          result = try_cvtsw(i, instr);
          break;
        case InstructionKind::DSRAV:
          result = try_dsrav(i, instr);
          break;
        case InstructionKind::DSRLV:
          result = try_dsrlv(i, instr);
          break;
        case InstructionKind::DSLLV:
          result = try_dsllv(i, instr);
          break;
        case InstructionKind::SUBU:
          result = try_subu(i, instr);
          break;
        case InstructionKind::SLLV:
          result = try_sllv(i, instr);
          break;
        case InstructionKind::MOVN:
          result = try_movn(i, instr);
          if (!result) {
            result = to_asm_automatic(i.op_name_to_string(), i, instr);
          }
          break;
        case InstructionKind::MOVZ:
          result = try_movz(i, instr);
          if (!result) {
            result = to_asm_automatic(i.op_name_to_string(), i, instr);
          }
          break;

          // Everything below here is an "asm passthrough".
        case InstructionKind::JR:
          result = to_asm_src_reg("jr", i, instr);
          break;

        // reg reg
        case InstructionKind::QMFC2:
          result = to_asm_dst_reg_src_reg(i.op_name_to_string(), i, instr);
          break;

        // VU/COP2
        case InstructionKind::VMOVE:
        case InstructionKind::VFTOI0:
        case InstructionKind::VFTOI4:
        case InstructionKind::VFTOI12:
        case InstructionKind::VITOF0:
        case InstructionKind::VITOF12:
        case InstructionKind::VITOF15:
        case InstructionKind::VABS:
        case InstructionKind::VADD:
        case InstructionKind::VSUB:
        case InstructionKind::VMUL:
        case InstructionKind::VMINI:
        case InstructionKind::VMAX:
        case InstructionKind::VOPMSUB:
        case InstructionKind::VMADD:
        case InstructionKind::VMSUB:
        case InstructionKind::VADD_BC:
        case InstructionKind::VSUB_BC:
        case InstructionKind::VMUL_BC:
        case InstructionKind::VMULA_BC:
        case InstructionKind::VMADD_BC:
        case InstructionKind::VADDA_BC:
        case InstructionKind::VMADDA_BC:
        case InstructionKind::VMSUBA_BC:
        case InstructionKind::VMSUB_BC:
        case InstructionKind::VMINI_BC:
        case InstructionKind::VMAX_BC:
        case InstructionKind::VADDQ:
        case InstructionKind::VSUBQ:
        case InstructionKind::VMULQ:
        case InstructionKind::VMSUBQ:
        case InstructionKind::VMULA:
        case InstructionKind::VADDA:
        case InstructionKind::VMADDA:
        case InstructionKind::VOPMULA:
        case InstructionKind::VDIV:
        case InstructionKind::VCLIP:
        case InstructionKind::VMULAQ:
        case InstructionKind::VMTIR:
        case InstructionKind::VIAND:
        case InstructionKind::VLQI:
        case InstructionKind::VIADDI:
        case InstructionKind::VSQI:
        case InstructionKind::VRGET:
        case InstructionKind::VSQRT:
        case InstructionKind::VRSQRT:
        case InstructionKind::VRXOR:
        case InstructionKind::VRNEXT:
        case InstructionKind::VNOP:
        case InstructionKind::VWAITQ:
        case InstructionKind::VCALLMS:

        // FPU/COP1
        case InstructionKind::MULAS:
        case InstructionKind::MADDAS:
        case InstructionKind::MADDS:
        case InstructionKind::ADDAS:

        // Moves / Loads / Stores
        case InstructionKind::CTC2:
        case InstructionKind::CFC2:
        case InstructionKind::SQC2:
        case InstructionKind::LQC2:
        case InstructionKind::LDR:
        case InstructionKind::LDL:
        case InstructionKind::QMTC2:
        case InstructionKind::MFC0:
        case InstructionKind::MTC0:
        case InstructionKind::SYNCL:
        case InstructionKind::SYNCP:
        case InstructionKind::SYSCALL:
        case InstructionKind::CACHE_DXWBIN:
        case InstructionKind::MTPC:
        case InstructionKind::MFPC:

        // random math
        case InstructionKind::ADDU:
        case InstructionKind::SRL:  // maybe bitfield ops use this?
        case InstructionKind::SRA:
        case InstructionKind::SLT:
        case InstructionKind::SLTI:

        // MMI
        case InstructionKind::PSLLW:
        case InstructionKind::PSRAW:
        case InstructionKind::PSRAH:
        case InstructionKind::PLZCW:
        case InstructionKind::PMFHL_UW:
        case InstructionKind::PMFHL_LW:
        case InstructionKind::PMFHL_LH:
        case InstructionKind::PSLLH:
        case InstructionKind::PSRLH:
        case InstructionKind::PEXTLW:
        case InstructionKind::PPACH:
        case InstructionKind::PSUBW:
        case InstructionKind::PCGTW:
        case InstructionKind::PEXTLH:
        case InstructionKind::PEXTLB:
        case InstructionKind::PMAXH:
        case InstructionKind::PPACB:
        case InstructionKind::PADDW:
        case InstructionKind::PADDH:
        case InstructionKind::PMAXW:
        case InstructionKind::PPACW:
        case InstructionKind::PCEQW:
        case InstructionKind::PEXTUW:
        case InstructionKind::PMINH:
        case InstructionKind::PEXTUH:
        case InstructionKind::PEXTUB:
        case InstructionKind::PCEQB:
        case InstructionKind::PMINW:
        case InstructionKind::PABSW:
        case InstructionKind::PCPYLD:
        case InstructionKind::PROT3W:
        case InstructionKind::PAND:
        case InstructionKind::PMADDH:
        case InstructionKind::PMULTH:
        case InstructionKind::PEXEW:
        case InstructionKind::PCPYUD:
        case InstructionKind::PNOR:
        case InstructionKind::PCPYH:
        case InstructionKind::PINTEH:

        case InstructionKind::MTDAB:
        case InstructionKind::MTDABM:

          // 128 bit integer
          //        case InstructionKind::LQ:
          //        case InstructionKind::SQ:

          result = to_asm_automatic(i.op_name_to_string(), i, instr);
          break;
        default:
          result = nullptr;
      }

      if (result) {
        length = 1;
      }
    }

    // everything failed
    if (!result) {
      // temp hack for debug:
      printf("Instruction -> BasicOp failed on %s\n", i.to_string(*file).c_str());
      func->add_basic_op(std::make_shared<IR_Failed_Atomic>(), instr, instr + 1);
    } else {
      if (!func->contains_asm_ops && dynamic_cast<IR_AsmOp*>(result.get())) {
        func->warnings += ";; Function contains asm op\n";
        func->contains_asm_ops = true;
      }

      if (!result->reg_info_set) {
        printf("Failed reg info %s\n", result->print(*file).c_str());
      }
      func->add_basic_op(result, instr, instr + length);
      instr += (length - 1);
    }
  }
}
