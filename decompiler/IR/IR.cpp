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
    default:
      assert(false);
  }
  return buildList(toForm(math_operator), arg0->to_form(file), arg1->to_form(file));
}