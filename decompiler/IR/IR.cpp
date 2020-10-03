#include "IR.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"

std::string IR::print(const LinkedObjectFile& file) const {
  return to_form(file)->toStringPretty();
}

std::shared_ptr<Form> IR_Failed::to_form(const LinkedObjectFile& file) const {
  (void)file;
  return buildList("INVALID-OPERATION");
}

void IR_Failed::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  (void)output;
}

std::shared_ptr<Form> IR_Register::to_form(const LinkedObjectFile& file) const {
  (void)file;
  return toForm(reg.to_charp());
}

void IR_Register::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  (void)output;
}

std::shared_ptr<Form> IR_Set::to_form(const LinkedObjectFile& file) const {
  return buildList(toForm("set!"), dst->to_form(file), src->to_form(file));
}

void IR_Set::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  // note that we are not returning clobber here because it shouldn't contain anything that
  // the IR simplification code should touch.
  output->push_back(dst);
  output->push_back(src);
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

std::shared_ptr<Form> IR_Symbol::to_form(const LinkedObjectFile& file) const {
  (void)file;
  return toForm("'" + name);
}

void IR_Symbol::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  (void)output;
}

std::shared_ptr<Form> IR_SymbolValue::to_form(const LinkedObjectFile& file) const {
  (void)file;
  return toForm(name);
}

void IR_SymbolValue::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  (void)output;
}

std::shared_ptr<Form> IR_StaticAddress::to_form(const LinkedObjectFile& file) const {
  // return buildList(toForm("&"), file.get_label_name(label_id));
  return toForm(file.get_label_name(label_id));
}

void IR_StaticAddress::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  (void)output;
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

void IR_Load::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  output->push_back(location);
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

void IR_FloatMath2::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  output->push_back(arg0);
  output->push_back(arg1);
}

void IR_FloatMath1::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  output->push_back(arg);
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

void IR_IntMath2::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  output->push_back(arg0);
  output->push_back(arg1);
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

void IR_IntMath1::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  output->push_back(arg);
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

void IR_Call::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  (void)output;
}

std::shared_ptr<Form> IR_IntegerConstant::to_form(const LinkedObjectFile& file) const {
  (void)file;
  return toForm(std::to_string(value));
}

void IR_IntegerConstant::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  (void)output;
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
    case SET_BINTEGER:
      return buildList(toForm("set!"), destination->to_form(file), "binteger");
    case SET_PAIR:
      return buildList(toForm("set!"), destination->to_form(file), "pair");
    case UNKNOWN:
      return buildList("unknown-branch-delay");
    default:
      assert(false);
  }
}

void BranchDelay::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  if (destination) {
    output->push_back(destination);
  }

  if (source) {
    output->push_back(source);
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
    case GREATER_THAN_ZERO_SIGNED:
      return 1;
    case ALWAYS:
      return 0;
    default:
      assert(false);
  }
}

void Condition::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  if (src0) {
    output->push_back(src0);
  }

  if (src1) {
    output->push_back(src1);
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
    case GREATER_THAN_ZERO_SIGNED:
      condtion_operator = ">0.s";
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

void IR_Branch::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  condition.get_children(output);
  branch_delay.get_children(output);
}

std::shared_ptr<Form> IR_Compare::to_form(const LinkedObjectFile& file) const {
  return condition.to_form(file);
}

void IR_Compare::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  condition.get_children(output);
}

std::shared_ptr<Form> IR_Suspend::to_form(const LinkedObjectFile& file) const {
  (void)file;
  return buildList("suspend!");
}

void IR_Nop::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  (void)output;
}

void IR_Suspend::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  (void)output;
}

std::shared_ptr<Form> IR_Begin::to_form(const LinkedObjectFile& file) const {
  std::vector<std::shared_ptr<Form>> list;
  list.push_back(toForm("begin"));
  for (auto& x : forms) {
    list.push_back(x->to_form(file));
  }
  return buildList(list);
}

void IR_Begin::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  for (auto& x : forms) {
    output->push_back(x);
  }
}

namespace {
void print_inlining_begin(std::vector<std::shared_ptr<Form>>* output,
                          IR* ir,
                          const LinkedObjectFile& file) {
  auto as_begin = dynamic_cast<IR_Begin*>(ir);
  if (as_begin) {
    for (auto& x : as_begin->forms) {
      output->push_back(x->to_form(file));
    }
  } else {
    output->push_back(ir->to_form(file));
  }
}

bool is_single_expression(IR* in) {
  return !dynamic_cast<IR_Begin*>(in);
}
}  // namespace

std::shared_ptr<Form> IR_WhileLoop::to_form(const LinkedObjectFile& file) const {
  std::vector<std::shared_ptr<Form>> list;
  list.push_back(toForm("while"));
  list.push_back(condition->to_form(file));
  print_inlining_begin(&list, body.get(), file);
  return buildList(list);
}

void IR_WhileLoop::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  output->push_back(condition);
  output->push_back(body);
}

std::shared_ptr<Form> IR_CondWithElse::to_form(const LinkedObjectFile& file) const {
  // for now we only turn it into an if statement if both cases won't require a begin at the top
  // level. I think it is more common to write these as a two-case cond instead of an if with begin.
  if (entries.size() == 1 && is_single_expression(entries.front().body.get()) &&
      is_single_expression(else_ir.get())) {
    std::vector<std::shared_ptr<Form>> list;
    list.push_back(toForm("if"));
    list.push_back(entries.front().condition->to_form(file));
    list.push_back(entries.front().body->to_form(file));
    list.push_back(else_ir->to_form(file));
    return buildList(list);
  } else {
    std::vector<std::shared_ptr<Form>> list;
    list.push_back(toForm("cond"));
    for (auto& e : entries) {
      std::vector<std::shared_ptr<Form>> entry;
      entry.push_back(e.condition->to_form(file));
      print_inlining_begin(&entry, e.body.get(), file);
      list.push_back(buildList(entry));
    }
    std::vector<std::shared_ptr<Form>> else_form;
    else_form.push_back(toForm("else"));
    print_inlining_begin(&else_form, else_ir.get(), file);
    list.push_back(buildList(else_form));
    return buildList(list);
  }
}

void IR_CondWithElse::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  for (auto& e : entries) {
    output->push_back(e.condition);
    output->push_back(e.body);
  }
  output->push_back(else_ir);
}

std::shared_ptr<Form> IR_GetRuntimeType::to_form(const LinkedObjectFile& file) const {
  std::vector<std::shared_ptr<Form>> list = {toForm("type-of"), object->to_form(file)};
  return buildList(list);
}

void IR_GetRuntimeType::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  output->push_back(object);
}