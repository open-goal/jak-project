#include "BasicOpBuilder.h"
#include "decompiler/Function/Function.h"
#include "decompiler/Function/BasicBlocks.h"
#include "decompiler/Disasm/InstructionMatching.h"

namespace {

struct ConvertState {
  void reset() {}
};

std::shared_ptr<IR_Set> make_set(IR_Set::Kind kind,
                                 const std::shared_ptr<IR>& dst,
                                 const std::shared_ptr<IR>& src) {
  return std::make_shared<IR_Set>(kind, dst, src);
}

std::shared_ptr<IR_Register> make_reg(Register reg, int idx) {
  return std::make_shared<IR_Register>(reg, idx);
}

std::shared_ptr<IR_Symbol> make_sym(const std::string& name) {
  return std::make_shared<IR_Symbol>(name);
}

std::shared_ptr<IR_SymbolValue> make_sym_value(const std::string& name) {
  return std::make_shared<IR_SymbolValue>(name);
}

std::shared_ptr<IR_IntegerConstant> make_int(int64_t x) {
  return std::make_shared<IR_IntegerConstant>(x);
}

std::shared_ptr<IR> try_or(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::OR, {}, make_gpr(Reg::S7), make_gpr(Reg::R0))) {
    return make_set(IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx), make_sym("#f"));
  } else if (is_gpr_3(instr, InstructionKind::OR, {}, {}, make_gpr(Reg::R0))) {
    return make_set(IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                    make_reg(instr.get_src(0).get_reg(), idx));
  } else {
    return make_set(
        IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_IntMath2>(IR_IntMath2::OR, make_reg(instr.get_src(0).get_reg(), idx),
                                      make_reg(instr.get_src(1).get_reg(), idx)));
  }
  return nullptr;
}

std::shared_ptr<IR> try_ori(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::ORI && instr.get_src(0).is_reg(make_gpr(Reg::R0)) &&
      instr.get_src(1).is_imm()) {
    return make_set(IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                    make_int(instr.get_src(1).get_imm()));
  } else if (instr.kind == InstructionKind::ORI && instr.get_src(1).is_imm()) {
    return make_set(
        IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_IntMath2>(IR_IntMath2::OR, make_reg(instr.get_src(0).get_reg(), idx),
                                      make_int(instr.get_src(1).get_imm())));
  }
  return nullptr;
}

std::shared_ptr<IR> try_por(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::POR, {}, {}, make_gpr(Reg::R0))) {
    return make_set(IR_Set::REG_I128, make_reg(instr.get_dst(0).get_reg(), idx),
                    make_reg(instr.get_src(0).get_reg(), idx));
  }
  return nullptr;
}

std::shared_ptr<IR> try_mtc1(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::MTC1) {
    return make_set(IR_Set::GPR_TO_FPR, make_reg(instr.get_dst(0).get_reg(), idx),
                    make_reg(instr.get_src(0).get_reg(), idx));
  }
  return nullptr;
}

std::shared_ptr<IR> try_mfc1(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::MFC1) {
    return make_set(IR_Set::FPR_TO_GPR64, make_reg(instr.get_dst(0).get_reg(), idx),
                    make_reg(instr.get_src(0).get_reg(), idx));
  }
  return nullptr;
}

std::shared_ptr<IR> try_lwc1(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::LWC1 && instr.get_dst(0).is_reg() &&
      instr.get_src(0).is_link_or_label() && instr.get_src(1).is_reg(make_gpr(Reg::FP))) {
    return make_set(
        IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_Load>(
            IR_Load::FLOAT, 4, std::make_shared<IR_StaticAddress>(instr.get_src(0).get_label())));
  } else if (instr.kind == InstructionKind::LWC1 && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm() && instr.get_src(0).get_imm() == 0) {
    return make_set(
        IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_Load>(IR_Load::FLOAT, 4, make_reg(instr.get_src(1).get_reg(), idx)));
  } else if (instr.kind == InstructionKind::LWC1 && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm()) {
    return make_set(IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_Load>(
                        IR_Load::FLOAT, 4,
                        std::make_shared<IR_IntMath2>(
                            IR_IntMath2::ADD, make_reg(instr.get_src(1).get_reg(), idx),
                            std::make_shared<IR_IntegerConstant>(instr.get_src(0).get_imm()))));
  }
  return nullptr;
}

std::shared_ptr<IR> try_lhu(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::LHU && instr.get_dst(0).is_reg() &&
      instr.get_src(0).is_link_or_label() && instr.get_src(1).is_reg(make_gpr(Reg::FP))) {
    return make_set(IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_Load>(
                        IR_Load::UNSIGNED, 2,
                        std::make_shared<IR_StaticAddress>(instr.get_src(0).get_label())));
  } else if (instr.kind == InstructionKind::LHU && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm() && instr.get_src(0).get_imm() == 0) {
    return make_set(
        IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_Load>(IR_Load::UNSIGNED, 2, make_reg(instr.get_src(1).get_reg(), idx)));
  } else if (instr.kind == InstructionKind::LHU && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm()) {
    return make_set(IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_Load>(
                        IR_Load::UNSIGNED, 2,
                        std::make_shared<IR_IntMath2>(
                            IR_IntMath2::ADD, make_reg(instr.get_src(1).get_reg(), idx),
                            std::make_shared<IR_IntegerConstant>(instr.get_src(0).get_imm()))));
  }
  return nullptr;
}

std::shared_ptr<IR> try_lh(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::LH && instr.get_dst(0).is_reg() &&
      instr.get_src(0).is_link_or_label() && instr.get_src(1).is_reg(make_gpr(Reg::FP))) {
    return make_set(
        IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_Load>(
            IR_Load::SIGNED, 2, std::make_shared<IR_StaticAddress>(instr.get_src(0).get_label())));
  } else if (instr.kind == InstructionKind::LH && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm() && instr.get_src(0).get_imm() == 0) {
    return make_set(
        IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_Load>(IR_Load::SIGNED, 2, make_reg(instr.get_src(1).get_reg(), idx)));
  } else if (instr.kind == InstructionKind::LH && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm()) {
    return make_set(IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_Load>(
                        IR_Load::SIGNED, 2,
                        std::make_shared<IR_IntMath2>(
                            IR_IntMath2::ADD, make_reg(instr.get_src(1).get_reg(), idx),
                            std::make_shared<IR_IntegerConstant>(instr.get_src(0).get_imm()))));
  }
  return nullptr;
}

std::shared_ptr<IR> try_lb(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::LB && instr.get_dst(0).is_reg() &&
      instr.get_src(0).is_link_or_label() && instr.get_src(1).is_reg(make_gpr(Reg::FP))) {
    return make_set(
        IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_Load>(
            IR_Load::SIGNED, 1, std::make_shared<IR_StaticAddress>(instr.get_src(0).get_label())));
  } else if (instr.kind == InstructionKind::LB && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm() && instr.get_src(0).get_imm() == 0) {
    return make_set(
        IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_Load>(IR_Load::SIGNED, 1, make_reg(instr.get_src(1).get_reg(), idx)));
  } else if (instr.kind == InstructionKind::LB && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm()) {
    return make_set(IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_Load>(
                        IR_Load::SIGNED, 1,
                        std::make_shared<IR_IntMath2>(
                            IR_IntMath2::ADD, make_reg(instr.get_src(1).get_reg(), idx),
                            std::make_shared<IR_IntegerConstant>(instr.get_src(0).get_imm()))));
  }
  return nullptr;
}

std::shared_ptr<IR> try_lbu(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::LBU && instr.get_dst(0).is_reg() &&
      instr.get_src(0).is_link_or_label() && instr.get_src(1).is_reg(make_gpr(Reg::FP))) {
    return make_set(IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_Load>(
                        IR_Load::UNSIGNED, 1,
                        std::make_shared<IR_StaticAddress>(instr.get_src(0).get_label())));
  } else if (instr.kind == InstructionKind::LBU && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm() && instr.get_src(0).get_imm() == 0) {
    return make_set(
        IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_Load>(IR_Load::UNSIGNED, 1, make_reg(instr.get_src(1).get_reg(), idx)));
  } else if (instr.kind == InstructionKind::LBU && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm()) {
    return make_set(IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_Load>(
                        IR_Load::UNSIGNED, 1,
                        std::make_shared<IR_IntMath2>(
                            IR_IntMath2::ADD, make_reg(instr.get_src(1).get_reg(), idx),
                            std::make_shared<IR_IntegerConstant>(instr.get_src(0).get_imm()))));
  }
  return nullptr;
}

std::shared_ptr<IR> try_lwu(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::LWU && instr.get_dst(0).is_reg() &&
      instr.get_src(0).is_link_or_label() && instr.get_src(1).is_reg(make_gpr(Reg::FP))) {
    return make_set(IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_Load>(
                        IR_Load::UNSIGNED, 4,
                        std::make_shared<IR_StaticAddress>(instr.get_src(0).get_label())));
  } else if (instr.kind == InstructionKind::LWU && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm() && instr.get_src(0).get_imm() == 0) {
    return make_set(
        IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_Load>(IR_Load::UNSIGNED, 4, make_reg(instr.get_src(1).get_reg(), idx)));
  } else if (instr.kind == InstructionKind::LWU && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm()) {
    return make_set(IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_Load>(
                        IR_Load::UNSIGNED, 4,
                        std::make_shared<IR_IntMath2>(
                            IR_IntMath2::ADD, make_reg(instr.get_src(1).get_reg(), idx),
                            std::make_shared<IR_IntegerConstant>(instr.get_src(0).get_imm()))));
  }
  return nullptr;
}

std::shared_ptr<IR> try_ld(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::LD && instr.get_dst(0).is_reg() &&
      instr.get_src(0).is_link_or_label() && instr.get_src(1).is_reg(make_gpr(Reg::FP))) {
    return make_set(IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_Load>(
                        IR_Load::UNSIGNED, 8,
                        std::make_shared<IR_StaticAddress>(instr.get_src(0).get_label())));
  } else if (instr.kind == InstructionKind::LD && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm() && instr.get_src(0).get_imm() == 0) {
    return make_set(
        IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_Load>(IR_Load::UNSIGNED, 8, make_reg(instr.get_src(1).get_reg(), idx)));
  } else if (instr.kind == InstructionKind::LD && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm()) {
    return make_set(IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_Load>(
                        IR_Load::UNSIGNED, 8,
                        std::make_shared<IR_IntMath2>(
                            IR_IntMath2::ADD, make_reg(instr.get_src(1).get_reg(), idx),
                            std::make_shared<IR_IntegerConstant>(instr.get_src(0).get_imm()))));
  }
  return nullptr;
}

std::shared_ptr<IR> try_dsll(Instruction& instr, int idx) {
  if (is_gpr_2_imm_int(instr, InstructionKind::DSLL, {}, {}, {})) {
    return make_set(IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_IntMath2>(IR_IntMath2::LEFT_SHIFT,
                                                  make_reg(instr.get_src(0).get_reg(), idx),
                                                  make_int(instr.get_src(1).get_imm())));
  }
  return nullptr;
}

std::shared_ptr<IR> try_dsll32(Instruction& instr, int idx) {
  if (is_gpr_2_imm_int(instr, InstructionKind::DSLL32, {}, {}, {})) {
    return make_set(IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_IntMath2>(IR_IntMath2::LEFT_SHIFT,
                                                  make_reg(instr.get_src(0).get_reg(), idx),
                                                  make_int(32 + instr.get_src(1).get_imm())));
  }
  return nullptr;
}

std::shared_ptr<IR> try_dsra(Instruction& instr, int idx) {
  if (is_gpr_2_imm_int(instr, InstructionKind::DSRA, {}, {}, {})) {
    return make_set(IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_IntMath2>(IR_IntMath2::RIGHT_SHIFT_ARITH,
                                                  make_reg(instr.get_src(0).get_reg(), idx),
                                                  make_int(instr.get_src(1).get_imm())));
  }
  return nullptr;
}

std::shared_ptr<IR> try_dsra32(Instruction& instr, int idx) {
  if (is_gpr_2_imm_int(instr, InstructionKind::DSRA32, {}, {}, {})) {
    return make_set(IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_IntMath2>(IR_IntMath2::RIGHT_SHIFT_ARITH,
                                                  make_reg(instr.get_src(0).get_reg(), idx),
                                                  make_int(32 + instr.get_src(1).get_imm())));
  }
  return nullptr;
}

std::shared_ptr<IR> try_dsrl(Instruction& instr, int idx) {
  if (is_gpr_2_imm_int(instr, InstructionKind::DSRL, {}, {}, {})) {
    return make_set(IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_IntMath2>(IR_IntMath2::RIGHT_SHIFT_LOGIC,
                                                  make_reg(instr.get_src(0).get_reg(), idx),
                                                  make_int(instr.get_src(1).get_imm())));
  }
  return nullptr;
}

std::shared_ptr<IR> try_dsrl32(Instruction& instr, int idx) {
  if (is_gpr_2_imm_int(instr, InstructionKind::DSRL32, {}, {}, {})) {
    return make_set(IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_IntMath2>(IR_IntMath2::RIGHT_SHIFT_LOGIC,
                                                  make_reg(instr.get_src(0).get_reg(), idx),
                                                  make_int(32 + instr.get_src(1).get_imm())));
  }
  return nullptr;
}

std::shared_ptr<IR> try_float_math_2(Instruction& instr,
                                     int idx,
                                     InstructionKind instr_kind,
                                     IR_FloatMath2::Kind ir_kind) {
  if (is_gpr_3(instr, instr_kind, {}, {}, {})) {
    return make_set(
        IR_Set::REG_FLT, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_FloatMath2>(ir_kind, make_reg(instr.get_src(0).get_reg(), idx),
                                        make_reg(instr.get_src(1).get_reg(), idx)));
  }
  return nullptr;
}

std::shared_ptr<IR> try_daddiu(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::DADDIU && instr.get_src(0).is_reg(make_gpr(Reg::S7)) &&
      instr.get_src(1).kind == InstructionAtom::IMM_SYM) {
    return make_set(IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                    make_sym(instr.get_src(1).get_sym()));
  } else if (instr.kind == InstructionKind::DADDIU && instr.get_src(0).is_reg(make_gpr(Reg::FP)) &&
             instr.get_src(1).kind == InstructionAtom::LABEL) {
    return make_set(IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_StaticAddress>(instr.get_src(1).get_label()));
  } else if (instr.kind == InstructionKind::DADDIU) {
    return make_set(
        IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_IntMath2>(IR_IntMath2::ADD, make_reg(instr.get_src(0).get_reg(), idx),
                                      make_int(instr.get_src(1).get_imm())));
  }
  return nullptr;
}

std::shared_ptr<IR> try_lw(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::LW && instr.get_src(1).is_reg(make_gpr(Reg::S7)) &&
      instr.get_src(0).kind == InstructionAtom::IMM_SYM) {
    return make_set(IR_Set::SYM_LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                    make_sym_value(instr.get_src(0).get_sym()));
  } else if (instr.kind == InstructionKind::LW && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_link_or_label() && instr.get_src(1).is_reg(make_gpr(Reg::FP))) {
    return make_set(
        IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_Load>(
            IR_Load::SIGNED, 4, std::make_shared<IR_StaticAddress>(instr.get_src(0).get_label())));
  } else if (instr.kind == InstructionKind::LW && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm() && instr.get_src(0).get_imm() == 0) {
    return make_set(
        IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_Load>(IR_Load::SIGNED, 4, make_reg(instr.get_src(1).get_reg(), idx)));
  } else if (instr.kind == InstructionKind::LW && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm()) {
    return make_set(IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_Load>(
                        IR_Load::SIGNED, 4,
                        std::make_shared<IR_IntMath2>(
                            IR_IntMath2::ADD, make_reg(instr.get_src(1).get_reg(), idx),
                            std::make_shared<IR_IntegerConstant>(instr.get_src(0).get_imm()))));
  }
  return nullptr;
}

std::shared_ptr<IR> try_lq(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::LQ && instr.get_src(1).is_reg(make_gpr(Reg::S7)) &&
      instr.get_src(0).kind == InstructionAtom::IMM_SYM) {
    assert(false);
  } else if (instr.kind == InstructionKind::LQ && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_link_or_label() && instr.get_src(1).is_reg(make_gpr(Reg::FP))) {
    return make_set(IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_Load>(
                        IR_Load::UNSIGNED, 16,
                        std::make_shared<IR_StaticAddress>(instr.get_src(0).get_label())));
  } else if (instr.kind == InstructionKind::LQ && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm() && instr.get_src(0).get_imm() == 0) {
    return make_set(IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_Load>(IR_Load::UNSIGNED, 16,
                                              make_reg(instr.get_src(1).get_reg(), idx)));
  } else if (instr.kind == InstructionKind::LQ && instr.get_dst(0).is_reg() &&
             instr.get_src(0).is_imm()) {
    return make_set(IR_Set::LOAD, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_Load>(
                        IR_Load::UNSIGNED, 16,
                        std::make_shared<IR_IntMath2>(
                            IR_IntMath2::ADD, make_reg(instr.get_src(1).get_reg(), idx),
                            std::make_shared<IR_IntegerConstant>(instr.get_src(0).get_imm()))));
  }
  return nullptr;
}

std::shared_ptr<IR> try_daddu(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::DADDU, {}, {}, {}) &&
      !instr.get_src(0).is_reg(make_gpr(Reg::S7)) && !instr.get_src(1).is_reg(make_gpr(Reg::S7))) {
    return make_set(
        IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_IntMath2>(IR_IntMath2::ADD, make_reg(instr.get_src(0).get_reg(), idx),
                                      make_reg(instr.get_src(1).get_reg(), idx)));
  }
  return nullptr;
}

std::shared_ptr<IR> try_dsubu(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::DSUBU, {}, {}, {}) &&
      !instr.get_src(0).is_reg(make_gpr(Reg::S7)) && !instr.get_src(1).is_reg(make_gpr(Reg::S7))) {
    return make_set(
        IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_IntMath2>(IR_IntMath2::SUB, make_reg(instr.get_src(0).get_reg(), idx),
                                      make_reg(instr.get_src(1).get_reg(), idx)));
  }
  return nullptr;
}

std::shared_ptr<IR> try_mult3(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::MULT3, {}, {}, {}) &&
      !instr.get_src(0).is_reg(make_gpr(Reg::S7)) && !instr.get_src(1).is_reg(make_gpr(Reg::S7))) {
    return make_set(IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_IntMath2>(IR_IntMath2::MUL_SIGNED,
                                                  make_reg(instr.get_src(0).get_reg(), idx),
                                                  make_reg(instr.get_src(1).get_reg(), idx)));
  }
  return nullptr;
}

std::shared_ptr<IR> try_multu3(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::MULTU3, {}, {}, {}) &&
      !instr.get_src(0).is_reg(make_gpr(Reg::S7)) && !instr.get_src(1).is_reg(make_gpr(Reg::S7))) {
    return make_set(IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_IntMath2>(IR_IntMath2::MUL_UNSIGNED,
                                                  make_reg(instr.get_src(0).get_reg(), idx),
                                                  make_reg(instr.get_src(1).get_reg(), idx)));
  }
  return nullptr;
}

std::shared_ptr<IR> try_and(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::AND, {}, {}, {}) &&
      !instr.get_src(0).is_reg(make_gpr(Reg::S7)) && !instr.get_src(1).is_reg(make_gpr(Reg::S7))) {
    return make_set(
        IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_IntMath2>(IR_IntMath2::AND, make_reg(instr.get_src(0).get_reg(), idx),
                                      make_reg(instr.get_src(1).get_reg(), idx)));
  }
  return nullptr;
}

std::shared_ptr<IR> try_andi(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::ANDI) {
    return make_set(
        IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_IntMath2>(IR_IntMath2::AND, make_reg(instr.get_src(0).get_reg(), idx),
                                      make_int(instr.get_src(1).get_imm())));
  }
  return nullptr;
}

std::shared_ptr<IR> try_nor(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::NOR, {}, {}, {}) &&
      !instr.get_src(0).is_reg(make_gpr(Reg::S7)) && instr.get_src(1).is_reg(make_gpr(Reg::R0))) {
    return make_set(
        IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_IntMath1>(IR_IntMath1::NOT, make_reg(instr.get_src(0).get_reg(), idx)));
  } else if (is_gpr_3(instr, InstructionKind::NOR, {}, {}, {}) &&
             !instr.get_src(0).is_reg(make_gpr(Reg::S7)) &&
             !instr.get_src(1).is_reg(make_gpr(Reg::S7))) {
    return make_set(
        IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_IntMath2>(IR_IntMath2::NOR, make_reg(instr.get_src(0).get_reg(), idx),
                                      make_reg(instr.get_src(1).get_reg(), idx)));
  }
  return nullptr;
}

std::shared_ptr<IR> try_xor(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::XOR, {}, {}, {}) &&
      !instr.get_src(0).is_reg(make_gpr(Reg::S7)) && !instr.get_src(1).is_reg(make_gpr(Reg::S7))) {
    return make_set(
        IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_IntMath2>(IR_IntMath2::XOR, make_reg(instr.get_src(0).get_reg(), idx),
                                      make_reg(instr.get_src(1).get_reg(), idx)));
  }
  return nullptr;
}

std::shared_ptr<IR> try_addiu(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::ADDIU && instr.get_src(0).is_reg(make_gpr(Reg::R0))) {
    return make_set(IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                    make_int(instr.get_src(1).get_imm()));
  }
  return nullptr;
}

std::shared_ptr<IR> try_lui(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::LUI && instr.get_src(0).is_imm()) {
    return make_set(IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                    make_int(instr.get_src(0).get_imm() << 16));
  }
  return nullptr;
}

std::shared_ptr<IR> try_sll(Instruction& instr, int idx) {
  (void)idx;
  if (is_nop(instr)) {
    return std::make_shared<IR_Nop>();
  }
  return nullptr;
}

std::shared_ptr<IR> try_sw(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::SW && instr.get_src(1).is_sym() &&
      instr.get_src(2).is_reg(make_gpr(Reg::S7))) {
    return std::make_shared<IR_Set>(IR_Set::SYM_STORE, make_sym_value(instr.get_src(1).get_sym()),
                                    make_reg(instr.get_src(0).get_reg(), idx));
  } else if (instr.kind == InstructionKind::SW && instr.get_src(1).is_imm()) {
    return std::make_shared<IR_Store>(
        IR_Store::INTEGER,
        std::make_shared<IR_IntMath2>(
            IR_IntMath2::ADD, make_reg(instr.get_src(2).get_reg(), idx),
            std::make_shared<IR_IntegerConstant>(instr.get_src(1).get_imm())),
        make_reg(instr.get_src(0).get_reg(), idx), 4);
  }
  return nullptr;
}

std::shared_ptr<IR> try_sb(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::SB && instr.get_src(1).is_imm()) {
    return std::make_shared<IR_Store>(
        IR_Store::INTEGER,
        std::make_shared<IR_IntMath2>(
            IR_IntMath2::ADD, make_reg(instr.get_src(2).get_reg(), idx),
            std::make_shared<IR_IntegerConstant>(instr.get_src(1).get_imm())),
        make_reg(instr.get_src(0).get_reg(), idx), 1);
  }
  return nullptr;
}

std::shared_ptr<IR> try_sh(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::SH && instr.get_src(1).is_imm()) {
    return std::make_shared<IR_Store>(
        IR_Store::INTEGER,
        std::make_shared<IR_IntMath2>(
            IR_IntMath2::ADD, make_reg(instr.get_src(2).get_reg(), idx),
            std::make_shared<IR_IntegerConstant>(instr.get_src(1).get_imm())),
        make_reg(instr.get_src(0).get_reg(), idx), 2);
  }
  return nullptr;
}

std::shared_ptr<IR> try_sd(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::SD && instr.get_src(1).is_imm()) {
    return std::make_shared<IR_Store>(
        IR_Store::INTEGER,
        std::make_shared<IR_IntMath2>(
            IR_IntMath2::ADD, make_reg(instr.get_src(2).get_reg(), idx),
            std::make_shared<IR_IntegerConstant>(instr.get_src(1).get_imm())),
        make_reg(instr.get_src(0).get_reg(), idx), 8);
  }
  return nullptr;
}

std::shared_ptr<IR> try_sq(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::SQ && instr.get_src(1).is_imm()) {
    return std::make_shared<IR_Store>(
        IR_Store::INTEGER,
        std::make_shared<IR_IntMath2>(
            IR_IntMath2::ADD, make_reg(instr.get_src(2).get_reg(), idx),
            std::make_shared<IR_IntegerConstant>(instr.get_src(1).get_imm())),
        make_reg(instr.get_src(0).get_reg(), idx), 16);
  }
  return nullptr;
}

std::shared_ptr<IR> try_swc1(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::SWC1 && instr.get_src(1).is_imm()) {
    return std::make_shared<IR_Store>(
        IR_Store::FLOAT,
        std::make_shared<IR_IntMath2>(
            IR_IntMath2::ADD, make_reg(instr.get_src(2).get_reg(), idx),
            std::make_shared<IR_IntegerConstant>(instr.get_src(1).get_imm())),
        make_reg(instr.get_src(0).get_reg(), idx), 4);
  }
  return nullptr;
}

std::shared_ptr<IR> try_cvtws(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::CVTWS) {
    return make_set(IR_Set::REG_FLT, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_FloatMath1>(IR_FloatMath1::FLOAT_TO_INT,
                                                    make_reg(instr.get_src(0).get_reg(), idx)));
  }
  return nullptr;
}

std::shared_ptr<IR> try_cvtsw(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::CVTSW) {
    return make_set(IR_Set::REG_FLT, make_reg(instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_FloatMath1>(IR_FloatMath1::INT_TO_FLOAT,
                                                    make_reg(instr.get_src(0).get_reg(), idx)));
  }
  return nullptr;
}

std::shared_ptr<IR> try_float_math_1(Instruction& instr,
                                     int idx,
                                     InstructionKind ikind,
                                     IR_FloatMath1::Kind irkind) {
  if (instr.kind == ikind) {
    return make_set(
        IR_Set::REG_FLT, make_reg(instr.get_dst(0).get_reg(), idx),
        std::make_shared<IR_FloatMath1>(irkind, make_reg(instr.get_src(0).get_reg(), idx)));
  }
  return nullptr;
}

std::shared_ptr<IR> try_movs(Instruction& instr, int idx) {
  if (instr.kind == InstructionKind::MOVS) {
    return make_set(IR_Set::REG_FLT, make_reg(instr.get_dst(0).get_reg(), idx),
                    make_reg(instr.get_src(0).get_reg(), idx));
  }
  return nullptr;
}

// TWO Instructions
std::shared_ptr<IR> try_div(Instruction& instr, Instruction& next_instr, int idx) {
  if (instr.kind == InstructionKind::DIV && instr.get_src(0).is_reg() &&
      instr.get_src(1).is_reg() && next_instr.kind == InstructionKind::MFLO &&
      next_instr.get_dst(0).is_reg()) {
    return make_set(IR_Set::REG_64, make_reg(next_instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_IntMath2>(IR_IntMath2::DIV_SIGNED,
                                                  make_reg(instr.get_src(0).get_reg(), idx),
                                                  make_reg(instr.get_src(1).get_reg(), idx)));
  } else if (instr.kind == InstructionKind::DIV && instr.get_src(0).is_reg() &&
             instr.get_src(1).is_reg() && next_instr.kind == InstructionKind::MFHI &&
             next_instr.get_dst(0).is_reg()) {
    return make_set(IR_Set::REG_64, make_reg(next_instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_IntMath2>(IR_IntMath2::MOD_SIGNED,
                                                  make_reg(instr.get_src(0).get_reg(), idx),
                                                  make_reg(instr.get_src(1).get_reg(), idx)));
  }
  return nullptr;
}

std::shared_ptr<IR> try_divu(Instruction& instr, Instruction& next_instr, int idx) {
  if (instr.kind == InstructionKind::DIVU && instr.get_src(0).is_reg() &&
      instr.get_src(1).is_reg() && next_instr.kind == InstructionKind::MFLO &&
      next_instr.get_dst(0).is_reg()) {
    return make_set(IR_Set::REG_64, make_reg(next_instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_IntMath2>(IR_IntMath2::DIV_UNSIGNED,
                                                  make_reg(instr.get_src(0).get_reg(), idx),
                                                  make_reg(instr.get_src(1).get_reg(), idx)));
  } else if (instr.kind == InstructionKind::DIVU && instr.get_src(0).is_reg() &&
             instr.get_src(1).is_reg() && next_instr.kind == InstructionKind::MFHI &&
             next_instr.get_dst(0).is_reg()) {
    return make_set(IR_Set::REG_64, make_reg(next_instr.get_dst(0).get_reg(), idx),
                    std::make_shared<IR_IntMath2>(IR_IntMath2::MOD_UNSIGNED,
                                                  make_reg(instr.get_src(0).get_reg(), idx),
                                                  make_reg(instr.get_src(1).get_reg(), idx)));
  }
  return nullptr;
}

std::shared_ptr<IR> try_jalr(Instruction& instr, Instruction& next_instr, int idx) {
  (void)idx;
  if (instr.kind == InstructionKind::JALR && instr.get_dst(0).is_reg(make_gpr(Reg::RA)) &&
      instr.get_src(0).is_reg(make_gpr(Reg::T9)) &&
      is_gpr_2_imm_int(next_instr, InstructionKind::SLL, make_gpr(Reg::V0), make_gpr(Reg::RA), 0)) {
    return std::make_shared<IR_Call>();
  }
  return nullptr;
}

BranchDelay get_branch_delay(Instruction& i, int idx) {
  if (is_nop(i)) {
    return BranchDelay(BranchDelay::NOP);
  } else if (is_gpr_3(i, InstructionKind::OR, {}, make_gpr(Reg::S7), make_gpr(Reg::R0))) {
    BranchDelay b(BranchDelay::SET_REG_FALSE);
    b.destination = make_reg(i.get_dst(0).get_reg(), idx);
    return b;
  } else if (is_gpr_3(i, InstructionKind::OR, {}, {}, make_gpr(Reg::R0))) {
    BranchDelay b(BranchDelay::SET_REG_REG);
    b.destination = make_reg(i.get_dst(0).get_reg(), idx);
    b.source = make_reg(i.get_src(0).get_reg(), idx);
    return b;
  } else if (i.kind == InstructionKind::DADDIU && i.get_src(0).is_reg(make_gpr(Reg::S7)) &&
             i.get_src(1).is_imm() && i.get_src(1).get_imm() == 8) {
    BranchDelay b(BranchDelay::SET_REG_TRUE);
    b.destination = make_reg(i.get_dst(0).get_reg(), idx);
    return b;
  }
  BranchDelay b(BranchDelay::UNKNOWN);
  return b;
}

std::shared_ptr<IR> try_bne(Instruction& instr, Instruction& next_instr, int idx) {
  if (instr.kind == InstructionKind::BNE && instr.get_src(1).is_reg(make_gpr(Reg::R0))) {
    return std::make_shared<IR_Branch>(
        Condition(Condition::NONZERO, make_reg(instr.get_src(0).get_reg(), idx), nullptr, nullptr),
        instr.get_src(2).get_label(), get_branch_delay(next_instr, idx), false);
  } else if (instr.kind == InstructionKind::BNE && instr.get_src(0).is_reg(make_gpr(Reg::S7))) {
    return std::make_shared<IR_Branch>(
        Condition(Condition::TRUTHY, make_reg(instr.get_src(1).get_reg(), idx), nullptr, nullptr),
        instr.get_src(2).get_label(), get_branch_delay(next_instr, idx), false);
  } else if (instr.kind == InstructionKind::BNE) {
    return std::make_shared<IR_Branch>(
        Condition(Condition::NOT_EQUAL, make_reg(instr.get_src(0).get_reg(), idx),
                  make_reg(instr.get_src(1).get_reg(), idx), nullptr),
        instr.get_src(2).get_label(), get_branch_delay(next_instr, idx), false);
  }
  return nullptr;
}

std::shared_ptr<IR> try_bnel(Instruction& instr, Instruction& next_instr, int idx) {
  if (instr.kind == InstructionKind::BNEL && instr.get_src(0).is_reg(make_gpr(Reg::S7))) {
    return std::make_shared<IR_Branch>(
        Condition(Condition::TRUTHY, make_reg(instr.get_src(1).get_reg(), idx), nullptr, nullptr),
        instr.get_src(2).get_label(), get_branch_delay(next_instr, idx), true);
  } else if (instr.kind == InstructionKind::BNEL) {
    //    return std::make_shared<IR_Branch2>(IR_Branch2::NOT_EQUAL, instr.get_src(2).get_label(),
    //                                        make_reg(instr.get_src(0).get_reg(), idx),
    //                                        make_reg(instr.get_src(1).get_reg(), idx),
    //                                        get_branch_delay(next_instr, idx), true);
  }
  return nullptr;
}

std::shared_ptr<IR> try_beql(Instruction& instr, Instruction& next_instr, int idx) {
  if (instr.kind == InstructionKind::BEQL && instr.get_src(0).is_reg(make_gpr(Reg::S7))) {
    return std::make_shared<IR_Branch>(
        Condition(Condition::FALSE, make_reg(instr.get_src(1).get_reg(), idx), nullptr, nullptr),
        instr.get_src(2).get_label(), get_branch_delay(next_instr, idx), true);
  } else if (instr.kind == InstructionKind::BEQL) {
    return std::make_shared<IR_Branch>(
        Condition(Condition::EQUAL, make_reg(instr.get_src(0).get_reg(), idx),
                  make_reg(instr.get_src(1).get_reg(), idx), nullptr),
        instr.get_src(2).get_label(), get_branch_delay(next_instr, idx), true);
  }
  return nullptr;
}

std::shared_ptr<IR> try_beq(Instruction& instr, Instruction& next_instr, int idx) {
  if (instr.kind == InstructionKind::BEQ && instr.get_src(0).is_reg(make_gpr(Reg::R0)) &&
      instr.get_src(1).is_reg(make_gpr(Reg::R0))) {
    return std::make_shared<IR_Branch>(Condition(Condition::ALWAYS, nullptr, nullptr, nullptr),
                                       instr.get_src(2).get_label(),
                                       get_branch_delay(next_instr, idx), false);
  } else if (instr.kind == InstructionKind::BEQ && instr.get_src(0).is_reg(make_gpr(Reg::S7))) {
    return std::make_shared<IR_Branch>(
        Condition(Condition::FALSE, make_reg(instr.get_src(1).get_reg(), idx), nullptr, nullptr),
        instr.get_src(2).get_label(), get_branch_delay(next_instr, idx), false);
  } else if (instr.kind == InstructionKind::BEQ) {
    return std::make_shared<IR_Branch>(
        Condition(Condition::EQUAL, make_reg(instr.get_src(0).get_reg(), idx),
                  make_reg(instr.get_src(1).get_reg(), idx), nullptr),
        instr.get_src(2).get_label(), get_branch_delay(next_instr, idx), false);
  }
  return nullptr;
}

std::shared_ptr<IR> try_daddiu(Instruction& i0, Instruction& i1, int idx) {
  if (i0.kind == InstructionKind::DADDIU && i1.kind == InstructionKind::MOVN &&
      i0.get_src(0).get_reg() == make_gpr(Reg::S7)) {
    auto dst_reg = i0.get_dst(0).get_reg();
    auto src_reg = i1.get_src(1).get_reg();
    assert(i0.get_src(0).get_reg() == make_gpr(Reg::S7));
    assert(i0.get_src(1).get_imm() == 8);
    assert(i1.get_dst(0).get_reg() == dst_reg);
    assert(i1.get_src(0).get_reg() == make_gpr(Reg::S7));
    return make_set(IR_Set::REG_64, make_reg(dst_reg, idx),
                    std::make_shared<IR_Compare>(
                        Condition(Condition::ZERO, make_reg(src_reg, idx), nullptr, nullptr)));
  } else if (i0.kind == InstructionKind::DADDIU && i1.kind == InstructionKind::MOVZ &&
             i0.get_src(0).get_reg() == make_gpr(Reg::S7)) {
    auto dst_reg = i0.get_dst(0).get_reg();
    auto src_reg = i1.get_src(1).get_reg();
    assert(i0.get_src(0).get_reg() == make_gpr(Reg::S7));
    assert(i0.get_src(1).get_imm() == 8);
    assert(i1.get_dst(0).get_reg() == dst_reg);
    assert(i1.get_src(0).get_reg() == make_gpr(Reg::S7));
    return make_set(IR_Set::REG_64, make_reg(dst_reg, idx),
                    std::make_shared<IR_Compare>(
                        Condition(Condition::NONZERO, make_reg(src_reg, idx), nullptr, nullptr)));
  }
  return nullptr;
}

std::shared_ptr<IR> try_lui(Instruction& i0, Instruction& i1, int idx) {
  if (i0.kind == InstructionKind::LUI && i1.kind == InstructionKind::ORI &&
      i0.get_src(0).is_label() && i1.get_src(1).is_label()) {
    assert(i0.get_dst(0).get_reg() == i1.get_src(0).get_reg());
    assert(i0.get_src(0).get_label() == i1.get_src(1).get_label());
    auto op = make_set(IR_Set::REG_64, make_reg(i1.get_dst(0).get_reg(), idx),
                       std::make_shared<IR_StaticAddress>(i0.get_src(0).get_label()));
    if (i0.get_dst(0).get_reg() != i1.get_dst(0).get_reg()) {
      op->clobber = make_reg(i0.get_dst(0).get_reg(), idx);
    }
    return op;
  } else if (i0.kind == InstructionKind::LUI && i1.kind == InstructionKind::ORI &&
             i0.get_src(0).is_imm() && i1.get_src(1).is_imm() &&
             i0.get_dst(0).get_reg() == i1.get_src(0).get_reg()) {
    auto op = make_set(
        IR_Set::REG_64, make_reg(i1.get_dst(0).get_reg(), idx),
        make_int((int64_t(i1.get_src(1).get_imm()) + int64_t(i0.get_src(0).get_imm() << 16))));
    if (i0.get_dst(0).get_reg() != i1.get_dst(0).get_reg()) {
      op->clobber = make_reg(i0.get_dst(0).get_reg(), idx);
    }
    return op;
  }
  return nullptr;
}

// THREE OP
std::shared_ptr<IR> try_dsubu(Instruction& i0, Instruction& i1, Instruction& i2, int idx) {
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
    return make_set(IR_Set::REG_64, make_reg(dst_reg, idx),
                    std::make_shared<IR_Compare>(
                        Condition(Condition::EQUAL, make_reg(src0_reg, idx),
                                  make_reg(src1_reg, idx), make_reg(clobber_reg, idx))));
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
    return make_set(IR_Set::REG_64, make_reg(dst_reg, idx),
                    std::make_shared<IR_Compare>(
                        Condition(Condition::NOT_EQUAL, make_reg(src0_reg, idx),
                                  make_reg(src1_reg, idx), make_reg(clobber_reg, idx))));
  }
  return nullptr;
}

std::shared_ptr<IR> try_slt(Instruction& i0, Instruction& i1, Instruction& i2, int idx) {
  if (i0.kind == InstructionKind::SLT && i1.kind == InstructionKind::BNE) {
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    auto src1_reg = i0.get_src(1).get_reg();
    assert(i1.get_src(0).get_reg() == clobber_reg);
    assert(i1.get_src(1).get_reg() == make_gpr(Reg::R0));
    return std::make_shared<IR_Branch>(
        Condition(Condition::LESS_THAN_SIGNED, make_reg(src0_reg, idx), make_reg(src1_reg, idx),
                  make_reg(clobber_reg, idx)),
        i1.get_src(2).get_label(), get_branch_delay(i2, idx), false);
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
    return make_set(IR_Set::REG_64, make_reg(dst_reg, idx),
                    std::make_shared<IR_Compare>(
                        Condition(Condition::LESS_THAN_SIGNED, make_reg(src0_reg, idx),
                                  make_reg(src1_reg, idx), make_reg(clobber_reg, idx))));
  } else if (i0.kind == InstructionKind::SLT && i1.kind == InstructionKind::BEQ) {
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    auto src1_reg = i0.get_src(1).get_reg();
    assert(i1.get_src(0).get_reg() == clobber_reg);
    assert(i1.get_src(1).get_reg() == make_gpr(Reg::R0));
    return std::make_shared<IR_Branch>(
        Condition(Condition::GEQ_SIGNED, make_reg(src0_reg, idx), make_reg(src1_reg, idx),
                  make_reg(clobber_reg, idx)),
        i1.get_src(2).get_label(), get_branch_delay(i2, idx), false);
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
    return make_set(IR_Set::REG_64, make_reg(dst_reg, idx),
                    std::make_shared<IR_Compare>(
                        Condition(Condition::GEQ_SIGNED, make_reg(src0_reg, idx),
                                  make_reg(src1_reg, idx), make_reg(clobber_reg, idx))));
  }
  return nullptr;
}

std::shared_ptr<IR> try_slti(Instruction& i0, Instruction& i1, Instruction& i2, int idx) {
  auto src1 = make_int(i0.get_src(1).get_imm());
  if (i0.kind == InstructionKind::SLTI && i1.kind == InstructionKind::BNE) {
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    assert(i1.get_src(0).get_reg() == clobber_reg);
    assert(i1.get_src(1).get_reg() == make_gpr(Reg::R0));
    return std::make_shared<IR_Branch>(
        Condition(Condition::LESS_THAN_SIGNED, make_reg(src0_reg, idx), src1,
                  make_reg(clobber_reg, idx)),
        i1.get_src(2).get_label(), get_branch_delay(i2, idx), false);
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
    return make_set(
        IR_Set::REG_64, make_reg(dst_reg, idx),
        std::make_shared<IR_Compare>(Condition(Condition::LESS_THAN_SIGNED, make_reg(src0_reg, idx),
                                               src1, make_reg(clobber_reg, idx))));
  } else if (i0.kind == InstructionKind::SLTI && i1.kind == InstructionKind::BEQ) {
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    assert(i1.get_src(0).get_reg() == clobber_reg);
    assert(i1.get_src(1).get_reg() == make_gpr(Reg::R0));
    return std::make_shared<IR_Branch>(
        Condition(Condition::GEQ_SIGNED, make_reg(src0_reg, idx), src1, make_reg(clobber_reg, idx)),
        i1.get_src(2).get_label(), get_branch_delay(i2, idx), false);
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
    return make_set(
        IR_Set::REG_64, make_reg(dst_reg, idx),
        std::make_shared<IR_Compare>(Condition(Condition::GEQ_SIGNED, make_reg(src0_reg, idx), src1,
                                               make_reg(clobber_reg, idx))));
  }
  return nullptr;
}

std::shared_ptr<IR> try_sltiu(Instruction& i0, Instruction& i1, Instruction& i2, int idx) {
  auto src1 = make_int(i0.get_src(1).get_imm());
  if (i0.kind == InstructionKind::SLTIU && i1.kind == InstructionKind::BNE) {
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    assert(i1.get_src(0).get_reg() == clobber_reg);
    assert(i1.get_src(1).get_reg() == make_gpr(Reg::R0));
    return std::make_shared<IR_Branch>(
        Condition(Condition::LESS_THAN_UNSIGNED, make_reg(src0_reg, idx), src1,
                  make_reg(clobber_reg, idx)),
        i1.get_src(2).get_label(), get_branch_delay(i2, idx), false);
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
    return make_set(IR_Set::REG_64, make_reg(dst_reg, idx),
                    std::make_shared<IR_Compare>(Condition(Condition::LESS_THAN_UNSIGNED,
                                                           make_reg(src0_reg, idx), src1,
                                                           make_reg(clobber_reg, idx))));
  } else if (i0.kind == InstructionKind::SLTIU && i1.kind == InstructionKind::BEQ) {
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    assert(i1.get_src(0).get_reg() == clobber_reg);
    assert(i1.get_src(1).get_reg() == make_gpr(Reg::R0));
    return std::make_shared<IR_Branch>(Condition(Condition::GEQ_UNSIGNED, make_reg(src0_reg, idx),
                                                 src1, make_reg(clobber_reg, idx)),
                                       i1.get_src(2).get_label(), get_branch_delay(i2, idx), false);
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
    return make_set(
        IR_Set::REG_64, make_reg(dst_reg, idx),
        std::make_shared<IR_Compare>(Condition(Condition::GEQ_UNSIGNED, make_reg(src0_reg, idx),
                                               src1, make_reg(clobber_reg, idx))));
  }
  return nullptr;
}

std::shared_ptr<IR> try_ceqs(Instruction& i0, Instruction& i1, Instruction& i2, int idx) {
  if (i0.kind == InstructionKind::CEQS && i1.kind == InstructionKind::BC1T) {
    return std::make_shared<IR_Branch>(
        Condition(Condition::FLOAT_EQUAL, make_reg(i0.get_src(0).get_reg(), idx),
                  make_reg(i0.get_src(1).get_reg(), idx), nullptr),
        i1.get_src(0).get_label(), get_branch_delay(i2, idx), false);
  } else if (i0.kind == InstructionKind::CEQS && i1.kind == InstructionKind::BC1F) {
    return std::make_shared<IR_Branch>(
        Condition(Condition::FLOAT_NOT_EQUAL, make_reg(i0.get_src(0).get_reg(), idx),
                  make_reg(i0.get_src(1).get_reg(), idx), nullptr),
        i1.get_src(0).get_label(), get_branch_delay(i2, idx), false);
  }
  return nullptr;
}

std::shared_ptr<IR> try_clts(Instruction& i0, Instruction& i1, Instruction& i2, int idx) {
  if (i0.kind == InstructionKind::CLTS && i1.kind == InstructionKind::BC1T) {
    return std::make_shared<IR_Branch>(
        Condition(Condition::FLOAT_LESS_THAN, make_reg(i0.get_src(0).get_reg(), idx),
                  make_reg(i0.get_src(1).get_reg(), idx), nullptr),
        i1.get_src(0).get_label(), get_branch_delay(i2, idx), false);
  } else if (i0.kind == InstructionKind::CLTS && i1.kind == InstructionKind::BC1F) {
    return std::make_shared<IR_Branch>(
        Condition(Condition::FLOAT_GEQ, make_reg(i0.get_src(0).get_reg(), idx),
                  make_reg(i0.get_src(1).get_reg(), idx), nullptr),
        i1.get_src(0).get_label(), get_branch_delay(i2, idx), false);
  }
  return nullptr;
}

std::shared_ptr<IR> try_sltu(Instruction& i0, Instruction& i1, Instruction& i2, int idx) {
  if (i0.kind == InstructionKind::SLTU && i1.kind == InstructionKind::BNE) {
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    auto src1_reg = i0.get_src(1).get_reg();
    assert(i1.get_src(0).get_reg() == clobber_reg);
    assert(i1.get_src(1).get_reg() == make_gpr(Reg::R0));
    return std::make_shared<IR_Branch>(
        Condition(Condition::LESS_THAN_UNSIGNED, make_reg(src0_reg, idx), make_reg(src1_reg, idx),
                  make_reg(clobber_reg, idx)),
        i1.get_src(2).get_label(), get_branch_delay(i2, idx), false);
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
    return make_set(IR_Set::REG_64, make_reg(dst_reg, idx),
                    std::make_shared<IR_Compare>(
                        Condition(Condition::LESS_THAN_UNSIGNED, make_reg(src0_reg, idx),
                                  make_reg(src1_reg, idx), make_reg(clobber_reg, idx))));
  } else if (i0.kind == InstructionKind::SLTU && i1.kind == InstructionKind::BEQ) {
    auto clobber_reg = i0.get_dst(0).get_reg();
    auto src0_reg = i0.get_src(0).get_reg();
    auto src1_reg = i0.get_src(1).get_reg();
    assert(i1.get_src(0).get_reg() == clobber_reg);
    assert(i1.get_src(1).get_reg() == make_gpr(Reg::R0));
    return std::make_shared<IR_Branch>(
        Condition(Condition::GEQ_UNSIGNED, make_reg(src0_reg, idx), make_reg(src1_reg, idx),
                  make_reg(clobber_reg, idx)),
        i1.get_src(2).get_label(), get_branch_delay(i2, idx), false);
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
    return make_set(IR_Set::REG_64, make_reg(dst_reg, idx),
                    std::make_shared<IR_Compare>(
                        Condition(Condition::GEQ_UNSIGNED, make_reg(src0_reg, idx),
                                  make_reg(src1_reg, idx), make_reg(clobber_reg, idx))));
  }
  return nullptr;
}

// five op
std::shared_ptr<IR> try_lwu(Instruction& i0,
                            Instruction& i1,
                            Instruction& i2,
                            Instruction& i3,
                            Instruction& i4,
                            int idx) {
  auto s6 = make_gpr(Reg::S6);
  if (i0.kind == InstructionKind::LWU && i0.get_dst(0).is_reg(s6) &&
      i0.get_src(0).get_imm() == 44 && i0.get_src(1).is_reg(s6) &&
      i1.kind == InstructionKind::MTLO1 && i1.get_src(0).is_reg(s6) &&
      i2.kind == InstructionKind::LWU && i2.get_dst(0).is_reg(s6) &&
      i2.get_src(0).get_imm() == 12 && i2.get_src(1).is_reg(s6) &&
      i3.kind == InstructionKind::JALR && i3.get_dst(0).is_reg(make_gpr(Reg::RA)) &&
      i3.get_src(0).is_reg(s6) && i4.kind == InstructionKind::MFLO1 && i4.get_dst(0).is_reg(s6)) {
    return std::make_shared<IR_Suspend>();
  }
  return nullptr;
}

}  // namespace

void add_basic_ops_to_block(Function* func, const BasicBlock& block, LinkedObjectFile* file) {
  ConvertState state;

  for (int instr = block.start_word; instr < block.end_word; instr++) {
    auto& i = func->instructions.at(instr);

    int length = 0;

    std::shared_ptr<IR> result = nullptr;
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
        case InstructionKind::BEQL:
          result = try_beql(i, next, instr);
          break;
        case InstructionKind::DADDIU:
          result = try_daddiu(i, next, instr);
          break;
        case InstructionKind::LUI:
          result = try_lui(i, next, instr);
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
          break;
        case InstructionKind::MULT3:
          result = try_mult3(i, instr);
          break;
        case InstructionKind::MULTU3:
          result = try_multu3(i, instr);
          break;
        case InstructionKind::POR:
          result = try_por(i, instr);
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
          break;
        case InstructionKind::LUI:
          result = try_lui(i, instr);
          break;
        case InstructionKind::SLL:
          result = try_sll(i, instr);
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
        default:
          result = nullptr;
      }

      if (result) {
        length = 1;
      }
    }

    // everything failed
    if (!result) {
      state.reset();
      // temp hack for debug:
      printf("Instruction -> BasicOp failed on %s\n", i.to_string(*file).c_str());
      func->add_basic_op(std::make_shared<IR_Failed>(), instr, instr + 1);
    } else {
      func->add_basic_op(result, instr, instr + length);
      instr += (length - 1);
    }
  }
}
