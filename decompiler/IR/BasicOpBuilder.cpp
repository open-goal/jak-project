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

std::shared_ptr<IR> try_or(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::OR, {}, make_gpr(Reg::S7), make_gpr(Reg::R0))) {
    return make_set(IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx), make_sym("#f"));
  } else if (is_gpr_3(instr, InstructionKind::OR, {}, {}, make_gpr(Reg::R0))) {
    return make_set(IR_Set::REG_64, make_reg(instr.get_dst(0).get_reg(), idx),
                    make_reg(instr.get_src(0).get_reg(), idx));
  }
  return nullptr;
}

// std::shared_ptr<BasicOp> try_por_reg_move(Instruction& instr, int idx) {
//  if (is_gpr_3(instr, InstructionKind::POR, {}, {}, make_gpr(Reg::R0))) {
//    return std::make_shared<RegRegMove>(BasicOpArg(instr.get_dst(0).get_reg(), idx),
//                                        BasicOpArg(instr.get_src(0).get_reg(), idx),
//                                        RegRegMove::GPR128_GPR128);
//  }
//  return nullptr;
//}
//
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
//
// std::shared_ptr<BasicOp> try_daddiu_get_sym(Instruction& instr, int idx) {
//  if (instr.kind == InstructionKind::DADDIU && instr.get_src(0).is_reg(make_gpr(Reg::S7)) &&
//      instr.get_src(1).kind == InstructionAtom::IMM_SYM) {
//    return std::make_shared<SetRegToSymbol>(BasicOpArg(instr.get_dst(0).get_reg(), idx),
//                                            instr.get_src(1).get_sym());
//  }
//  return nullptr;
//}
//
// std::shared_ptr<BasicOp> try_daddiu_get_static_addr(Instruction& instr, int idx) {
//  if (instr.kind == InstructionKind::DADDIU && instr.get_src(0).is_reg(make_gpr(Reg::FP)) &&
//      instr.get_src(1).kind == InstructionAtom::LABEL) {
//    return std::make_shared<StaticAddr>(BasicOpArg(instr.get_dst(0).get_reg(), idx),
//                                        instr.get_src(1).get_label());
//  }
//  return nullptr;
//}
//
// std::shared_ptr<BasicOp> try_lw_get_sym_value(Instruction& instr, int idx) {
//  if (instr.kind == InstructionKind::LW && instr.get_src(1).is_reg(make_gpr(Reg::S7)) &&
//      instr.get_src(0).kind == InstructionAtom::IMM_SYM) {
//    return std::make_shared<SetRegToSymbolValue>(BasicOpArg(instr.get_dst(0).get_reg(), idx),
//                                                 instr.get_src(0).get_sym());
//  }
//  return nullptr;
//}
//
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

// std::shared_ptr<BasicOp> try_logior(Instruction& instr, int idx) {
//  if (is_gpr_3(instr, InstructionKind::OR, {}, {}, {}) &&
//      !instr.get_src(0).is_reg(make_gpr(Reg::S7)) && !instr.get_src(1).is_reg(make_gpr(Reg::S7)))
//      {
//    return std::make_shared<IntMath3>(IntMath3::LOGIOR, BasicOpArg(instr.get_dst(0).get_reg(),
//    idx),
//                                      BasicOpArg(instr.get_src(0).get_reg(), idx),
//                                      BasicOpArg(instr.get_src(1).get_reg(), idx));
//  }
//  return nullptr;
//}
//
// std::shared_ptr<BasicOp> try_logand(Instruction& instr, int idx) {
//  if (is_gpr_3(instr, InstructionKind::AND, {}, {}, {}) &&
//      !instr.get_src(0).is_reg(make_gpr(Reg::S7)) && !instr.get_src(1).is_reg(make_gpr(Reg::S7)))
//      {
//    return std::make_shared<IntMath3>(IntMath3::LOGAND, BasicOpArg(instr.get_dst(0).get_reg(),
//    idx),
//                                      BasicOpArg(instr.get_src(0).get_reg(), idx),
//                                      BasicOpArg(instr.get_src(1).get_reg(), idx));
//  }
//  return nullptr;
//}
//
// std::shared_ptr<BasicOp> try_lognot(Instruction& instr, int idx) {
//  if (is_gpr_3(instr, InstructionKind::NOR, {}, {}, {}) &&
//      !instr.get_src(0).is_reg(make_gpr(Reg::S7)) && instr.get_src(1).is_reg(make_gpr(Reg::R0))) {
//    return std::make_shared<IntMath2>(IntMath2::LOGNOT, BasicOpArg(instr.get_dst(0).get_reg(),
//    idx),
//                                      BasicOpArg(instr.get_src(0).get_reg(), idx));
//  }
//  return nullptr;
//}
//
// std::shared_ptr<BasicOp> try_lognor(Instruction& instr, int idx) {
//  if (is_gpr_3(instr, InstructionKind::NOR, {}, {}, {}) &&
//      !instr.get_src(0).is_reg(make_gpr(Reg::S7)) && !instr.get_src(1).is_reg(make_gpr(Reg::S7)))
//      {
//    return std::make_shared<IntMath3>(IntMath3::LOGNOR, BasicOpArg(instr.get_dst(0).get_reg(),
//    idx),
//                                      BasicOpArg(instr.get_src(0).get_reg(), idx),
//                                      BasicOpArg(instr.get_src(1).get_reg(), idx));
//  }
//  return nullptr;
//}
//
// std::shared_ptr<BasicOp> try_logxor(Instruction& instr, int idx) {
//  if (is_gpr_3(instr, InstructionKind::XOR, {}, {}, {}) &&
//      !instr.get_src(0).is_reg(make_gpr(Reg::S7)) && !instr.get_src(1).is_reg(make_gpr(Reg::S7)))
//      {
//    return std::make_shared<IntMath3>(IntMath3::LOGXOR, BasicOpArg(instr.get_dst(0).get_reg(),
//    idx),
//                                      BasicOpArg(instr.get_src(0).get_reg(), idx),
//                                      BasicOpArg(instr.get_src(1).get_reg(), idx));
//  }
//  return nullptr;
//}

//
//// TWO Instructions
// std::shared_ptr<BasicOp> try_div(Instruction& instr, Instruction& next_instr, int idx) {
//  if (instr.kind == InstructionKind::DIV && instr.get_src(0).is_reg() &&
//      instr.get_src(1).is_reg() && next_instr.kind == InstructionKind::MFLO &&
//      next_instr.get_dst(0).is_reg()) {
//    return std::make_shared<IntMath3>(
//        IntMath3::DIV_SIGNED, BasicOpArg(next_instr.get_dst(0).get_reg(), idx),
//        BasicOpArg(instr.get_src(0).get_reg(), idx), BasicOpArg(instr.get_src(1).get_reg(), idx));
//  }
//  return nullptr;
//}
//
// std::shared_ptr<BasicOp> try_mod(Instruction& instr, Instruction& next_instr, int idx) {
//  if (instr.kind == InstructionKind::DIV && instr.get_src(0).is_reg() &&
//      instr.get_src(1).is_reg() && next_instr.kind == InstructionKind::MFHI &&
//      next_instr.get_dst(0).is_reg()) {
//    return std::make_shared<IntMath3>(
//        IntMath3::MOD_SIGNED, BasicOpArg(next_instr.get_dst(0).get_reg(), idx),
//        BasicOpArg(instr.get_src(0).get_reg(), idx), BasicOpArg(instr.get_src(1).get_reg(), idx));
//  }
//  return nullptr;
//}

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
      default:
        result = nullptr;
    }

    if (result) {
      length = 1;
    }

    //
    //    if (!result && instr + 1 < block.end_word) {
    //      // single op failed, try double
    //      for (auto x : {try_div, try_mod}) {
    //        result = x(i, func->instructions.at(instr + 1), instr);
    //        if (result) {
    //          length = 2;
    //          break;
    //        }
    //      }
    //    }
    //
    // everything failed
    if (!result) {
      state.reset();
      func->add_basic_op(std::make_shared<IR_Failed>(), instr, instr + 1);
    } else {
      func->add_basic_op(result, instr, instr + length);
      instr += (length - 1);
    }

    // temp hack for debug:
    printf("Instruction -> BasicOp failed on %s\n", i.to_string(*file).c_str());
  }
}
