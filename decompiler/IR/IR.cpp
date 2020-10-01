#include "IR.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"

std::string IR::print(const LinkedObjectFile& file) const {
  return to_form(file)->toStringPretty();
}

std::shared_ptr<Form> IR_Register::to_form(const LinkedObjectFile& file) const {
  (void)file;
  return toForm(reg.to_charp());
}

std::shared_ptr<Form> IR_Set::to_form(const LinkedObjectFile& file) const {
  return buildList(toForm("set!"), dst->to_form(file), src->to_form(file));
}

std::shared_ptr<Form> IR_Store::to_form(const LinkedObjectFile& file) const {
  std::string store_operator;
  switch (kind) {
    case FLOAT:
      store_operator = "s.f";
      break;
    case INTEGER:
      switch (size) {
        case 1:
          store_operator = "s.b";
          break;
        case 2:
          store_operator = "s.h";
          break;
        case 4:
          store_operator = "s.w";
          break;
        case 8:
          store_operator = "s.d";
          break;
        case 16:
          store_operator = "s.q";
          break;
        default:
          assert(false);
      }
      break;
    default:
      assert(false);
  }

  return buildList(toForm(store_operator), dst->to_form(file), src->to_form(file));
}

std::shared_ptr<Form> IR_Failed::to_form(const LinkedObjectFile& file) const {
  (void)file;
  return buildList("INVALID-OPERATION");
}

std::shared_ptr<Form> IR_Symbol::to_form(const LinkedObjectFile& file) const {
  (void)file;
  return toForm("'" + name);
}

std::shared_ptr<Form> IR_SymbolValue::to_form(const LinkedObjectFile& file) const {
  (void)file;
  return toForm(name);
}

std::shared_ptr<Form> IR_StaticAddress::to_form(const LinkedObjectFile& file) const {
  // return buildList(toForm("&"), file.get_label_name(label_id));
  return toForm(file.get_label_name(label_id));
}

std::shared_ptr<Form> IR_Load::to_form(const LinkedObjectFile& file) const {
  std::string load_operator;
  switch (kind) {
    case FLOAT:
      load_operator = "l.f";
      break;
    case UNSIGNED:
      switch (size) {
        case 1:
          load_operator = "l.bu";
          break;
        case 2:
          load_operator = "l.hu";
          break;
        case 4:
          load_operator = "l.wu";
          break;
        case 8:
          load_operator = "l.d";
          break;
        case 16:
          load_operator = "l.q";
          break;
        default:
          assert(false);
      }
      break;
    case SIGNED:
      switch (size) {
        case 1:
          load_operator = "l.bs";
          break;
        case 2:
          load_operator = "l.hs";
          break;
        case 4:
          load_operator = "l.ws";
          break;
        default:
          assert(false);
      }
      break;
    default:
      assert(false);
  }
  return buildList(toForm(load_operator), location->to_form(file));
}

std::shared_ptr<Form> IR_FloatMath2::to_form(const LinkedObjectFile& file) const {
  std::string math_operator;
  switch (kind) {
    case DIV:
      math_operator = "/.f";
      break;
    case MUL:
      math_operator = "*.f";
      break;
    case ADD:
      math_operator = "+.f";
      break;
    case SUB:
      math_operator = "-.f";
      break;
    case MIN:
      math_operator = "min.f";
      break;
    case MAX:
      math_operator = "max.f";
      break;
    default:
      assert(false);
  }

  return buildList(toForm(math_operator), arg0->to_form(file), arg1->to_form(file));
}

std::shared_ptr<Form> IR_IntMath2::to_form(const LinkedObjectFile& file) const {
  std::string math_operator;
  switch (kind) {
    case ADD:
      math_operator = "+.i";
      break;
    case SUB:
      math_operator = "-.i";
      break;
    case MUL_SIGNED:
      math_operator = "*.si";
      break;
    case MUL_UNSIGNED:
      math_operator = "*.ui";
      break;
    case DIV_SIGNED:
      math_operator = "/.si";
      break;
    case MOD_SIGNED:
      math_operator = "mod.si";
      break;
    case DIV_UNSIGNED:
      math_operator = "/.ui";
      break;
    case MOD_UNSIGNED:
      math_operator = "mod.ui";
      break;
    case OR:
      math_operator = "logior";
      break;
    case AND:
      math_operator = "logand";
      break;
    case NOR:
      math_operator = "lognor";
      break;
    case XOR:
      math_operator = "logxor";
      break;
    case LEFT_SHIFT:
      math_operator = "shl";
      break;
    case RIGHT_SHIFT_ARITH:
      math_operator = "sar";
      break;
    case RIGHT_SHIFT_LOGIC:
      math_operator = "shr";
      break;
    default:
      assert(false);
  }
  return buildList(toForm(math_operator), arg0->to_form(file), arg1->to_form(file));
}

std::shared_ptr<Form> IR_IntMath1::to_form(const LinkedObjectFile& file) const {
  std::string math_operator;
  switch (kind) {
    case NOT:
      math_operator = "lognot";
      break;
    default:
      assert(false);
  }
  return buildList(toForm(math_operator), arg->to_form(file));
}

std::shared_ptr<Form> IR_FloatMath1::to_form(const LinkedObjectFile& file) const {
  std::string math_operator;
  switch (kind) {
    case FLOAT_TO_INT:
      math_operator = "int<-float";
      break;
    case INT_TO_FLOAT:
      math_operator = "float<-int";
      break;
    case ABS:
      math_operator = "abs.f";
      break;
    case NEG:
      math_operator = "neg.f";
      break;
    case SQRT:
      math_operator = "sqrt.f";
      break;
    default:
      assert(false);
  }
  return buildList(toForm(math_operator), arg->to_form(file));
}

std::shared_ptr<Form> IR_Call::to_form(const LinkedObjectFile& file) const {
  (void)file;
  return buildList("call!");
}

std::shared_ptr<Form> IR_IntegerConstant::to_form(const LinkedObjectFile& file) const {
  (void)file;
  return toForm(std::to_string(value));
}

std::shared_ptr<Form> BranchDelay::to_form(const LinkedObjectFile& file) const {
  (void)file;
  switch (kind) {
    case NOP:
      return buildList("nop");
    case SET_REG_FALSE:
      return buildList(toForm("set!"), destination->to_form(file), "'#f");
    case SET_REG_TRUE:
      return buildList(toForm("set!"), destination->to_form(file), "'#t");
    case SET_REG_REG:
      return buildList(toForm("set!"), destination->to_form(file), source->to_form(file));
    case UNKNOWN:
      return buildList("unknown-branch-delay");
    default:
      assert(false);
  }
}

std::shared_ptr<Form> IR_Nop::to_form(const LinkedObjectFile& file) const {
  (void)file;
  return buildList("nop!");
}

int Condition::num_args() const {
  switch (kind) {
    case NOT_EQUAL:
    case EQUAL:
    case LESS_THAN_SIGNED:
    case LESS_THAN_UNSIGNED:
    case GREATER_THAN_SIGNED:
    case GREATER_THAN_UNSIGNED:
    case LEQ_SIGNED:
    case GEQ_SIGNED:
    case LEQ_UNSIGNED:
    case GEQ_UNSIGNED:
    case FLOAT_EQUAL:
    case FLOAT_NOT_EQUAL:
    case FLOAT_LESS_THAN:
    case FLOAT_GEQ:
      return 2;
    case ZERO:
    case NONZERO:
    case FALSE:
    case TRUTHY:
      return 1;
    case ALWAYS:
      return 0;
    default:
      assert(false);
  }
}

std::shared_ptr<Form> Condition::to_form(const LinkedObjectFile& file) const {
  int nargs = num_args();
  std::string condtion_operator;
  switch (kind) {
    case NOT_EQUAL:
      condtion_operator = "!=";
      break;
    case EQUAL:
      condtion_operator = "=";
      break;
    case LESS_THAN_SIGNED:
      condtion_operator = "<.si";
      break;
    case LESS_THAN_UNSIGNED:
      condtion_operator = "<.ui";
      break;
    case GREATER_THAN_SIGNED:
      condtion_operator = ">.si";
      break;
    case GREATER_THAN_UNSIGNED:
      condtion_operator = ">.ui";
      break;
    case LEQ_SIGNED:
      condtion_operator = "<=.si";
      break;
    case GEQ_SIGNED:
      condtion_operator = ">=.si";
      break;
    case LEQ_UNSIGNED:
      condtion_operator = "<=.ui";
      break;
    case GEQ_UNSIGNED:
      condtion_operator = ">=.ui";
      break;
    case ZERO:
      condtion_operator = "zero?";
      break;
    case NONZERO:
      condtion_operator = "nonzero?";
      break;
    case FALSE:
      condtion_operator = "not";
      break;
    case TRUTHY:
      condtion_operator = "";
      break;
    case ALWAYS:
      condtion_operator = "'#t";
      break;
    case FLOAT_EQUAL:
      condtion_operator = "=.f";
      break;
    case FLOAT_NOT_EQUAL:
      condtion_operator = "!=.f";
      break;
    case FLOAT_LESS_THAN:
      condtion_operator = "<.f";
      break;
    case FLOAT_GEQ:
      condtion_operator = ">=.f";
      break;
    default:
      assert(false);
  }

  if (nargs == 2) {
    return buildList(toForm(condtion_operator), src0->to_form(file), src1->to_form(file));
  } else if (nargs == 1) {
    if (condtion_operator.empty()) {
      return src0->to_form(file);
    } else {
      return buildList(toForm(condtion_operator), src0->to_form(file));
    }
  } else if (nargs == 0) {
    return toForm(condtion_operator);
  } else {
    assert(false);
  }
}

std::shared_ptr<Form> IR_Branch::to_form(const LinkedObjectFile& file) const {
  return buildList(toForm(likely ? "bl!" : "b!"), condition.to_form(file),
                   toForm(file.get_label_name(dest_label_idx)), branch_delay.to_form(file));
}

std::shared_ptr<Form> IR_Compare::to_form(const LinkedObjectFile& file) const {
  return condition.to_form(file);
}

std::shared_ptr<Form> IR_Suspend::to_form(const LinkedObjectFile& file) const {
  (void)file;
  return buildList("suspend!");
}