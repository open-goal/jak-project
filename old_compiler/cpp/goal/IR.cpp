#include "IR.h"
#include "GoalEnv.h"

// std::string IR_Define::print() {
//  return "DEFINE-SYMBOL " + sym->print() + " " + value->print() + "\n";
//}

std::string IR_LoadInteger::print() {
  std::string result = "LOAD_INT ";
  if (is_signed) {
    result += std::to_string(s_value);
  } else {
    result += std::to_string(us_value);
  }

  result += " SIZE " + std::to_string((int)size);
  result += " INTO " + value->print();
  return result;
}

std::string IR_Return::print() {
  return "RETURN " + value->print() + " in register " + dest->print();
}

std::string IR_Goto_Label::print() {
  return "GOTO " + label->print();
}

// RegAllocInstr IR_Define::to_rai() {
//  RegAllocInstr instr;
//}

RegAllocInstr IR_Set::to_rai() {
  RegAllocInstr rai;

  rai.write.push_back(dest->get_assignment());
  rai.read.push_back(src->get_assignment());

  // the "move" flag is used so the coloring system can attempt to eliminate moves.
  // however, if the src and dst register kinds are different, it cannot be eliminated.
  // so we don't consider this case to be a "move".
  if (dest->get_assignment().kind == src->get_assignment().kind) {
    rai.is_move = true;
  }

  return rai;
}

std::string IR_GetSymbolValue::print() {
  return "GET-SYM-VAL " + symbol->print() + " IN " + dest->print();
}

RegAllocInstr IR_GetSymbolValue::to_rai() {
  RegAllocInstr rai;
  rai.write.push_back(dest->get_assignment());
  return rai;
}

RegAllocInstr IR_Goto_Label::to_rai() {
  RegAllocInstr rai;
  if (!resolved) {
    throw std::runtime_error("IR GOTO label has an unresolved label at coloring time!");
  }
  rai.jumps.push_back(label->idx);
  rai.fallthrough = false;
  return rai;
}

std::string IR_ConditionalBranch::print() {
  return "BR " + cond.print() + " " + label->print();
}

RegAllocInstr IR_ConditionalBranch::to_rai() {
  auto rai = cond.to_rai();
  rai.jumps.push_back(label->idx);
  if (!resolved) {
    throw std::runtime_error(
        "IR_ConditionalBranch label has an unresolved label at coloring time!");
  }

  return rai;
}

std::string GoalCondition::print() {
  switch (kind) {
    case NOT_EQUAL_64:
      return a->print() + " != " + b->print();
    case EQUAL_64:
      return a->print() + " == " + b->print();
    case LEQ_64:
      return a->print() + " <= " + b->print();
    case GEQ_64:
      return a->print() + " >= " + b->print();
    case LT_64:
      return a->print() + " < " + b->print();
    case GT_64:
      return a->print() + " > " + b->print();
    default:
      throw std::runtime_error("unknown condition type in GoalCondition::print()");
  }
}

RegAllocInstr GoalCondition::to_rai() {
  RegAllocInstr rai;
  switch (kind) {
    case NOT_EQUAL_64:
    case EQUAL_64:
    case LEQ_64:
    case GEQ_64:
    case LT_64:
    case GT_64:
      rai.read.push_back(a->get_assignment());
      rai.read.push_back(b->get_assignment());
      break;
    default:
      throw std::runtime_error("unknown condition type in GoalCondition::to_rai()");
  }

  return rai;
}

RegAllocInstr IR_LoadInteger::to_rai() {
  RegAllocInstr rai;
  if (value->is_register())
    rai.write.push_back(value->get_assignment());
  return rai;
}

RegAllocInstr IR_Return::to_rai() {
  RegAllocInstr rai;
  if (dest->is_register())
    rai.write.push_back(dest->get_assignment());
  if (value->is_register())
    rai.read.push_back(value->get_assignment());

  if (value->is_register() && (dest->get_assignment().kind == value->get_assignment().kind)) {
    rai.is_move = true;
  }

  return rai;
}

RegAllocInstr IR_SetSymbolValue::to_rai() {
  RegAllocInstr rai;
  if (value->is_register())
    rai.read.push_back(value->get_assignment());
  return rai;
}

std::string IR_FunctionCall::print() {
  std::string result = "CALL " + func_in->print() + " ARGS ";
  for (auto& arg : args) {
    result += arg->print() + " ";
  }
  result += "INTO " + dest->print();
  return result;
}

RegAllocInstr IR_FunctionCall::to_rai() {
  RegAllocInstr rai;
  rai.read.push_back(func_in->get_assignment());
  rai.write.push_back(func_call->get_assignment());
  for (auto& arg : args) {
    rai.read.push_back(arg->get_assignment());
  }

  rai.write.push_back(dest->get_assignment());

  // todo is this the right set of registers?
  for (int i = 0; i < 8; i++) {
    ColoringAssignment ass;
    ass.kind = REGISTER;
    ass.reg_id = ARG_REGS[i];
    rai.clobber.push_back(ass);
  }

  for (int i = XMM0; i < XMM15; i++) {
    ColoringAssignment ass;
    ass.kind = REGISTER;
    ass.reg_id = i;
    rai.clobber.push_back(ass);
  }

  // duh...
  ColoringAssignment ass;
  ass.kind = REGISTER;
  ass.reg_id = RAX;
  rai.clobber.push_back(ass);

  return rai;
}

void IR_FunctionCall::add_constraints_to_program(std::vector<RegConstraint>& constraints,
                                                 int my_id) {
  for (uint32_t i = 0; i < args.size(); i++) {
    RegConstraint c;
    c.var_id = args[i]->get_assignment().id;
    c.instr_id = my_id;
    c.ass.kind = REGISTER;
    c.ass.reg_id = ARG_REGS[i];
    constraints.push_back(c);
  }

  // function call reg
  RegConstraint fc;
  fc.var_id = func_call->get_assignment().id;
  fc.instr_id = my_id;
  fc.ass.kind = REGISTER;
  fc.ass.reg_id = T9_REG;
  constraints.push_back(fc);

  // funciton return
  RegConstraint rc;
  rc.var_id = dest->get_assignment().id;
  rc.instr_id = my_id;
  rc.ass.kind = REGISTER;
  rc.ass.reg_id = RET_REG;
  constraints.push_back(rc);
}

std::string IR_StaticVarAddr::print() {
  return "GET-STATIC& " + dest->print() + " FROM " + src->print();
}

RegAllocInstr IR_StaticVarAddr::to_rai() {
  RegAllocInstr rai;
  rai.write.push_back(dest->get_assignment());
  return rai;
}

std::string IR_StaticVar32::print() {
  return "GET-STATIC-32 " + dest->print() + " FROM " + src->print();
}

RegAllocInstr IR_StaticVar32::to_rai() {
  RegAllocInstr rai;
  rai.write.push_back(dest->get_assignment());
  return rai;
}

std::string IR_FunctionAddr::print() {
  return "GET-FNC " + dest->print() + " FRM " + src->print();
}

RegAllocInstr IR_FunctionAddr::to_rai() {
  RegAllocInstr rai;
  rai.write.push_back(dest->get_assignment());
  return rai;
}

std::string IR_FunctionBegin::print() {
  return "FUNCTION BEGIN " + std::to_string(nargs);
}

RegAllocInstr IR_FunctionBegin::to_rai() {
  RegAllocInstr rai;
  for (auto& var : lambda->func->params) {
    rai.write.push_back(var.second->get_assignment());
  }
  return rai;
}

std::string IR_IntegerMath::print() {
  std::string result;
  switch (math_kind) {
    case ADD_64:
      result = "ADD64 ";
      break;
    case SUB_64:
      result = "SUB64 ";
      break;
    case IMUL_32:
      result = "IMU32 ";
      break;
    case IDIV_32:
      result = "IDV32 ";
      break;
    case SHLV_64:
      result = "SHLV64 ";
      break;
    case SARV_64:
      result = "SARV64 ";
      break;
    case SHRV_64:
      result = "SHRV64 ";
      break;
    case SHL_64:
      result = "SHL64 ";
      break;
    case SAR_64:
      result = "SAR64 ";
      break;
    case SHR_64:
      result = "SHR64 ";
      break;
    case IMOD_32:
      result = "IMOD32 ";
      break;
    case OR_64:
      result = "OR64 ";
      break;
    case AND_64:
      result = "AND64 ";
      break;
    case XOR_64:
      result = "XOR64 ";
      break;
    case NOT_64:
      result = "NOT64 ";
      break;
    default:
      throw std::runtime_error("unknown kind of IntegerMath");
  }
  result += d->print() + " " + a0->print();
  return result;
}

RegAllocInstr IR_IntegerMath::to_rai() {
  RegAllocInstr rai;

  if (math_kind == SHL_64 || math_kind == SHR_64 || math_kind == SAR_64) {
    rai.write.push_back(d->get_assignment());
    rai.read.push_back(d->get_assignment());
    return rai;
  }

  if (math_kind == IDIV_32) {
    ColoringAssignment ca;
    ca.kind = AssignmentKind::REGISTER;
    ca.reg_id = RDX;
    rai.exclusive.push_back(ca);
  }

  if (math_kind == IMOD_32) {
    ColoringAssignment ca;
    ca.kind = AssignmentKind::REGISTER;
    ca.reg_id = RDX;
    rai.exclusive.push_back(ca);
  }

  rai.write.push_back(d->get_assignment());
  rai.read.push_back(d->get_assignment());

  if (math_kind != NOT_64) {
    rai.read.push_back(a0->get_assignment());
  }

  return rai;
}

std::string IR_FloatMath::print() {
  std::string result;
  switch (math_kind) {
    case MUL_SS:
      result = "MULSS ";
      break;
    case DIV_SS:
      result = "DIVSS ";
      break;
    case SUB_SS:
      result = "SUBSS ";
      break;
    case ADD_SS:
      result = "ADDSS ";
      break;
    default:
      throw std::runtime_error("unknown kind of FloatMath");
  }
  result += d->print() + " " + a0->print();
  return result;
}

RegAllocInstr IR_FloatMath::to_rai() {
  RegAllocInstr rai;
  rai.write.push_back(d->get_assignment());
  rai.read.push_back(a0->get_assignment());
  rai.read.push_back(d->get_assignment());
  return rai;
}

std::string IR_GetSymbolObj::print() {
  return "GET-SYM-OBJ " + sym->print() + " IN " + dest->print();
}

RegAllocInstr IR_GetSymbolObj::to_rai() {
  RegAllocInstr rai;
  rai.write.push_back(dest->get_assignment());
  return rai;
}

// std::string IR_Xmm2Gpr::print() {
//  return "MOVD " + dst->print() + " " + src->print();
//}
//
// RegAllocInstr IR_Xmm2Gpr::to_rai() {
//  RegAllocInstr rai;
//  rai.write.push_back(dst->get_assignment());
//  rai.read.push_back(src->get_assignment());
//  return rai;
//}

std::string IR_LoadConstOffset::print() {
  return "LOAD" + std::to_string(size) + " " + dst->print() + " (" + src->print() + " " +
         std::to_string(offset) + ")";
}

RegAllocInstr IR_LoadConstOffset::to_rai() {
  RegAllocInstr rai;
  rai.write.push_back(dst->get_assignment());
  rai.read.push_back(src->get_assignment());
  return rai;
}

std::string IR_StoreConstOffset::print() {
  return "STORE" + std::to_string(size) + " " + val->print() + " -> (" + mem->print() + " " +
         std::to_string(offset) + ")";
}

RegAllocInstr IR_StoreConstOffset::to_rai() {
  RegAllocInstr rai;
  rai.read.push_back(val->get_assignment());
  rai.read.push_back(mem->get_assignment());
  return rai;
}

RegAllocInstr IR_IntToFloat::to_rai() {
  RegAllocInstr rai;
  rai.read.push_back(src->get_assignment());
  rai.write.push_back(dest->get_assignment());
  return rai;
}

RegAllocInstr IR_FloatToInt::to_rai() {
  RegAllocInstr rai;
  rai.read.push_back(src->get_assignment());
  rai.write.push_back(dest->get_assignment());
  return rai;
}

std::string IR_GetReturnAddressPointer::print() {
  return "GET-RA-PTR " + dest->print();
}

RegAllocInstr IR_GetReturnAddressPointer::to_rai() {
  RegAllocInstr rai;
  rai.write.push_back(dest->get_assignment());
  return rai;
}

std::string IR_Asm::print() {
  return "ASM";
}

RegAllocInstr IR_Asm::to_rai() {
  RegAllocInstr rai;
  switch (asm_kind) {
    case IR_Asm::PUSH:
    case IR_Asm::POP:
      rai.read.push_back(args.at(0)->get_assignment());
      break;
    case IR_Asm::JMP:
      rai.read.push_back(args.at(0)->get_assignment());
      break;
    case IR_Asm::RET:
      break;
    case IR_Asm::RET_REGISTER:
      rai.read.push_back(args.at(0)->get_assignment());
      break;
    case IR_Asm::SUB:
      rai.read.push_back(args.at(0)->get_assignment());
      rai.read.push_back(args.at(1)->get_assignment());
      rai.write.push_back(args.at(0)->get_assignment());
      break;
    default:
      throw std::runtime_error("unknown asm mode in ir_asm to_rai");
  }
  return rai;
}