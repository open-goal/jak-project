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
  }
  BranchDelay b(BranchDelay::UNKNOWN);
  return b;
}

std::shared_ptr<IR> try_bne(Instruction& instr, Instruction& next_instr, int idx) {
  if (instr.kind == InstructionKind::BNE) {
    return std::make_shared<IR_Branch2>(IR_Branch2::NOT_EQUAL, instr.get_src(2).get_label(),
                                        make_reg(instr.get_src(0).get_reg(), idx),
                                        make_reg(instr.get_src(1).get_reg(), idx),
                                        get_branch_delay(next_instr, idx));
  }
  return nullptr;
}

std::shared_ptr<IR> try_beq(Instruction& instr, Instruction& next_instr, int idx) {
  if (instr.kind == InstructionKind::BEQ && instr.get_src(0).is_reg(make_gpr(Reg::R0)) &&
      instr.get_src(1).is_reg(make_gpr(Reg::R0))) {
    return std::make_shared<IR_BranchAlways>(instr.get_src(2).get_label(),
                                             get_branch_delay(next_instr, idx));
  }
  //  if (instr.kind == InstructionKind::BEQ) {
  //    return std::make_shared<IR_Branch2>(IR_Branch2::EQUAL, instr.get_src(2).get_label(),
  //                                        make_reg(instr.get_src(0).get_reg(), idx),
  //                                        make_reg(instr.get_src(1).get_reg(), idx),
  //                                        get_branch_delay(next_instr, idx));
  //  }
  return nullptr;
}

}  // namespace

void add_basic_ops_to_block(Function* func, const BasicBlock& block, LinkedObjectFile* file) {
  ConvertState state;

  for (int instr = block.start_word; instr < block.end_word; instr++) {
    auto& i = func->instructions.at(instr);

    int length = 0;

    std::shared_ptr<IR> result = nullptr;
    switch (i.kind) {
      case InstructionKind::OR:
        result = try_or(i, instr);
        break;
      case InstructionKind::DADDIU:
        result = try_daddiu(i, instr);
        break;
      case InstructionKind::AND:
        result = try_and(i, instr);
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
      case InstructionKind::POR:
        result = try_por(i, instr);
        break;
      case InstructionKind::LHU:
        result = try_lhu(i, instr);
        break;
      case InstructionKind::LW:
        result = try_lw(i, instr);
        break;
      case InstructionKind::LD:
        result = try_ld(i, instr);
        break;
      case InstructionKind::DSLL:
        result = try_dsll(i, instr);
        break;
      case InstructionKind::LWU:
        result = try_lwu(i, instr);
        break;
      default:
        result = nullptr;
    }

    if (result) {
      length = 1;
    }

    if (!result && instr + 1 < block.end_word) {
      auto& next = func->instructions.at(instr + 1);
      // single op failed, try double
      switch (i.kind) {
        case InstructionKind::DIV:
          result = try_div(i, next, instr);
          break;
        case InstructionKind::JALR:
          result = try_jalr(i, next, instr);
          break;
        case InstructionKind::BNE:
          result = try_bne(i, next, instr);
          break;
        case InstructionKind::BEQ:
          result = try_beq(i, next, instr);
          break;
        default:
          result = nullptr;
      }

      if (result) {
        length = 2;
      }
    }

    // everything failed
    if (!result) {
      state.reset();
      func->add_basic_op(std::make_shared<IR_Failed>(), instr, instr + 1);
    } else {
      func->add_basic_op(result, instr, instr + length);
      instr += (length - 1);
    }

    // temp hack for debug:
    // printf("Instruction -> BasicOp failed on %s\n", i.to_string(*file).c_str());
  }
}
