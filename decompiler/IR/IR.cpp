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
        case 2:
          load_operator = "l.hu";
          break;
        case 4:
          load_operator = "l.wu";
          break;
        case 8:
          load_operator = "l.d";
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
    case DIV_SIGNED:
      math_operator = "/.si";
      break;
    case MOD_SIGNED:
      math_operator = "mod.si";
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
    case UNKNOWN:
      return buildList("unknown-branch-delay");
    default:
      assert(false);
  }
}

std::shared_ptr<Form> IR_Branch2::to_form(const LinkedObjectFile& file) const {
  std::string compare_operator;
  switch (kind) {
    case NOT_EQUAL:
      compare_operator = "!=";
      break;
    case EQUAL:
      compare_operator = "=";
      break;
    default:
      assert(false);
  }
  return buildList(toForm("b!"),
                   buildList(toForm(compare_operator), src0->to_form(file), src1->to_form(file)),
                   toForm(file.get_label_name(dest_label_idx)), branch_delay.to_form(file));
}

std::shared_ptr<Form> IR_BranchAlways::to_form(const LinkedObjectFile& file) const {
  return buildList(toForm("b!"), toForm("'#t"),
                   toForm(file.get_label_name(dest_label_idx)), branch_delay.to_form(file));
}