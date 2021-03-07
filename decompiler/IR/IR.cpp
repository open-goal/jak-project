#include "IR.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "common/goos/PrettyPrinter.h"
#include "third-party/fmt/core.h"

namespace decompiler {
// hack to print out reverse deref paths on loads to help with debugging load stuff.
bool enable_hack_load_path_print = false;
// hack to print (begin x) as x to make debug output easier to read.
bool inline_single_begins = true;

std::vector<std::shared_ptr<IR>> IR::get_all_ir(LinkedObjectFile& file) const {
  (void)file;
  std::vector<std::shared_ptr<IR>> result;
  get_children(&result);
  size_t last_checked = 0;
  size_t last_last_checked = -1;

  while (last_checked != last_last_checked) {
    last_last_checked = last_checked;
    auto end_of_check = result.size();
    for (size_t i = last_checked; i < end_of_check; i++) {
      auto it = result.at(i).get();
      assert(it);
      it->get_children(&result);
    }
    last_checked = end_of_check;
  }

  return result;
}

std::string IR::print(const LinkedObjectFile& file) const {
  return pretty_print::to_string(to_form(file));
}

namespace {
template <typename T>
void add_regs_to_str(const T& regs, std::string& str) {
  bool first = true;
  for (auto& reg : regs) {
    if (first) {
      first = false;
    } else {
      str.push_back(' ');
    }
    str.append(reg.to_charp());
  }
}
}  // namespace

goos::Object IR_Failed::to_form(const LinkedObjectFile& file) const {
  (void)file;
  return pretty_print::build_list("INVALID-OPERATION");
}

void IR_Failed::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  (void)output;
}

goos::Object IR_Register::to_form(const LinkedObjectFile& file) const {
  (void)file;
  return pretty_print::to_symbol(reg.to_charp());
}

void IR_Register::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  (void)output;
}

goos::Object IR_Set::to_form(const LinkedObjectFile& file) const {
  return pretty_print::build_list(pretty_print::to_symbol("set!"), dst->to_form(file),
                                  src->to_form(file));
}

void IR_Set::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  // note that we are not returning clobber here because it shouldn't contain anything that
  // the IR simplification code should touch.
  output->push_back(dst);
  output->push_back(src);
}

template <>
void IR_Set_Atomic::update_reginfo_self<IR_IntMath2>(int n_dest, int n_src, int n_clobber) {
  auto dst_as_reg = dynamic_cast<IR_Register*>(dst.get());
  if (dst_as_reg) {
    write_regs.push_back(dst_as_reg->reg);
  }

  auto src_as = dynamic_cast<IR_IntMath2*>(src.get());
  assert(src_as);

  for (auto& x : {src_as->arg0, src_as->arg1}) {
    auto reg = dynamic_cast<IR_Register*>(x.get());
    if (reg) {
      read_regs.push_back(reg->reg);
    }
  }

  assert(int(write_regs.size()) == n_dest);
  assert(int(read_regs.size()) == n_src);
  assert(int(clobber_regs.size()) == n_clobber);
  reg_info_set = true;
}

template <>
void IR_Set_Atomic::update_reginfo_self<IR_IntMath1>(int n_dest, int n_src, int n_clobber) {
  auto dst_as_reg = dynamic_cast<IR_Register*>(dst.get());
  if (dst_as_reg) {
    write_regs.push_back(dst_as_reg->reg);
  }

  auto src_as = dynamic_cast<IR_IntMath1*>(src.get());
  assert(src_as);

  auto reg = dynamic_cast<IR_Register*>(src_as->arg.get());
  if (reg) {
    read_regs.push_back(reg->reg);
  }

  assert(int(write_regs.size()) == n_dest);
  assert(int(read_regs.size()) == n_src);
  assert(int(clobber_regs.size()) == n_clobber);
  reg_info_set = true;
}

template <>
void IR_Set_Atomic::update_reginfo_self<IR_Load>(int n_dest, int n_src, int n_clobber) {
  auto dst_as_reg = dynamic_cast<IR_Register*>(dst.get());
  if (dst_as_reg) {
    write_regs.push_back(dst_as_reg->reg);
  }

  auto src_as = dynamic_cast<IR_Load*>(src.get());
  assert(src_as);

  // try to get the source as a register
  auto reg = dynamic_cast<IR_Register*>(src_as->location.get());
  if (reg) {
    read_regs.push_back(reg->reg);
  }

  // or as math with a register
  auto math = dynamic_cast<IR_IntMath2*>(src_as->location.get());
  if (math) {
    for (auto& x : {math->arg0, math->arg1}) {
      auto math_reg = dynamic_cast<IR_Register*>(x.get());
      if (math_reg) {
        read_regs.push_back(math_reg->reg);
      }
    }
  }

  assert(int(write_regs.size()) == n_dest);
  assert(int(read_regs.size()) == n_src);
  assert(int(clobber_regs.size()) == n_clobber);
  reg_info_set = true;
}

template <>
void IR_Set_Atomic::update_reginfo_self<IR_Compare>(int n_dest, int n_src, int n_clobber) {
  auto dst_as_reg = dynamic_cast<IR_Register*>(dst.get());
  if (dst_as_reg) {
    write_regs.push_back(dst_as_reg->reg);
  }

  auto src_as_cmp = dynamic_cast<IR_Compare*>(src.get());
  assert(src_as_cmp);
  for (auto& x : {src_as_cmp->condition.src0, src_as_cmp->condition.src1}) {
    auto as_reg = dynamic_cast<IR_Register*>(x.get());
    if (as_reg) {
      read_regs.push_back(as_reg->reg);
    }
  }

  auto as_reg = dynamic_cast<IR_Register*>(src_as_cmp->condition.clobber.get());
  if (as_reg) {
    clobber_regs.push_back(as_reg->reg);
  }

  assert(int(write_regs.size()) == n_dest);
  assert(int(read_regs.size()) == n_src);
  assert(int(clobber_regs.size()) == n_clobber);
  reg_info_set = true;
}

/*!
 * Set the register info, assuming this is a register to register set.
 */
void IR_Set_Atomic::update_reginfo_regreg() {
  auto dst_as_reg = dynamic_cast<IR_Register*>(dst.get());
  if (dst_as_reg) {
    write_regs.push_back(dst_as_reg->reg);
  }

  auto src_as = dynamic_cast<IR_Register*>(src.get());

  if (src_as) {
    read_regs.push_back(src_as->reg);
  }

  assert(int(write_regs.size()) == 1);
  assert(int(read_regs.size()) == 1);
  assert(int(clobber_regs.size()) == 0);
  reg_info_set = true;
}

goos::Object IR_Store::to_form(const LinkedObjectFile& file) const {
  std::string store_operator;
  switch (kind) {
    case Kind::FLOAT:
      store_operator = "s.f";
      break;
    case Kind::INTEGER:
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

  return pretty_print::build_list(pretty_print::to_symbol(store_operator), dst->to_form(file),
                                  src->to_form(file));
}

goos::Object IR_Store_Atomic::to_form(const LinkedObjectFile& file) const {
  std::string store_operator;
  switch (kind) {
    case Kind::FLOAT:
      store_operator = "s.f";
      break;
    case Kind::INTEGER:
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

  return pretty_print::build_list(pretty_print::to_symbol(store_operator), dst->to_form(file),
                                  src->to_form(file));
}

void IR_Store_Atomic::update_reginfo_self(int n_dest, int n_src, int n_clobber) {
  auto src_reg = dynamic_cast<IR_Register*>(src.get());
  if (src_reg) {
    read_regs.push_back(src_reg->reg);
  }

  auto dst_reg = dynamic_cast<IR_Register*>(dst.get());
  if (dst_reg) {
    read_regs.push_back(dst_reg->reg);
  }

  // or as math with a register
  auto math = dynamic_cast<IR_IntMath2*>(dst.get());
  if (math) {
    for (auto& x : {math->arg0, math->arg1}) {
      auto math_reg = dynamic_cast<IR_Register*>(x.get());
      if (math_reg) {
        read_regs.push_back(math_reg->reg);
      }
    }
  }

  assert(int(write_regs.size()) == n_dest);
  assert(int(read_regs.size()) == n_src);
  assert(int(clobber_regs.size()) == n_clobber);
  reg_info_set = true;
}

goos::Object IR_Symbol::to_form(const LinkedObjectFile& file) const {
  (void)file;
  return pretty_print::to_symbol("'" + name);
}

void IR_Symbol::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  (void)output;
}

goos::Object IR_SymbolValue::to_form(const LinkedObjectFile& file) const {
  (void)file;
  return pretty_print::to_symbol(name);
}

void IR_SymbolValue::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  (void)output;
}

goos::Object IR_EmptyPair::to_form(const LinkedObjectFile& file) const {
  (void)file;
  return pretty_print::to_symbol("'()");
}

void IR_EmptyPair::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  (void)output;
}

goos::Object IR_StaticAddress::to_form(const LinkedObjectFile& file) const {
  // return pretty_print::build_list(pretty_print::to_symbol("&"), file.get_label_name(label_id));
  return pretty_print::to_symbol(file.get_label_name(label_id));
}

void IR_StaticAddress::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  (void)output;
}

goos::Object IR_Load::to_form(const LinkedObjectFile& file) const {
  if (load_path_set && enable_hack_load_path_print) {
    std::vector<goos::Object> list;
    if (load_path_addr_of) {
      list.push_back(pretty_print::to_symbol("&->"));
    } else {
      list.push_back(pretty_print::to_symbol("->"));
    }
    list.push_back(load_path_base->to_form(file));
    for (auto& x : load_path) {
      list.push_back(pretty_print::to_symbol(x));
    }
    return pretty_print::build_list(list);
  }
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
  return pretty_print::build_list(pretty_print::to_symbol(load_operator), location->to_form(file));
}

void IR_Load::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  output->push_back(location);
}

goos::Object IR_FloatMath2::to_form(const LinkedObjectFile& file) const {
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

  return pretty_print::build_list(pretty_print::to_symbol(math_operator), arg0->to_form(file),
                                  arg1->to_form(file));
}

void IR_FloatMath2::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  output->push_back(arg0);
  output->push_back(arg1);
}

void IR_FloatMath1::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  output->push_back(arg);
}

goos::Object IR_IntMath2::to_form(const LinkedObjectFile& file) const {
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
    case MIN_SIGNED:
      math_operator = "min.si";
      break;
    case MAX_SIGNED:
      math_operator = "max.si";
      break;
    default:
      assert(false);
  }
  return pretty_print::build_list(pretty_print::to_symbol(math_operator), arg0->to_form(file),
                                  arg1->to_form(file));
}

void IR_IntMath2::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  output->push_back(arg0);
  output->push_back(arg1);
}

goos::Object IR_IntMath1::to_form(const LinkedObjectFile& file) const {
  std::string math_operator;
  switch (kind) {
    case NOT:
      math_operator = "lognot";
      break;
    case ABS:
      math_operator = "abs.si";
      break;
    case NEG:
      math_operator = "-.i";
      break;
    default:
      assert(false);
  }
  return pretty_print::build_list(pretty_print::to_symbol(math_operator), arg->to_form(file));
}

void IR_IntMath1::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  output->push_back(arg);
}

goos::Object IR_FloatMath1::to_form(const LinkedObjectFile& file) const {
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
  return pretty_print::build_list(pretty_print::to_symbol(math_operator), arg->to_form(file));
}

goos::Object IR_Call::to_form(const LinkedObjectFile& file) const {
  (void)file;
  std::vector<goos::Object> result;
  result.push_back(pretty_print::to_symbol("call!"));

  if (call_type_set) {
    result.push_back(pretty_print::to_symbol(":arg-count"));
    result.push_back(pretty_print::to_symbol(std::to_string(call_type.arg_count() - 1)));
  }

  for (auto& x : args) {
    result.push_back(x->to_form(file));
  }
  return pretty_print::build_list(result);
}

void IR_Call::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  (void)output;
}

goos::Object IR_IntegerConstant::to_form(const LinkedObjectFile& file) const {
  (void)file;
  return pretty_print::to_symbol(std::to_string(value));
}

void IR_IntegerConstant::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  (void)output;
}

goos::Object BranchDelay::to_form(const LinkedObjectFile& file) const {
  (void)file;
  switch (kind) {
    case NOP:
      return pretty_print::build_list("nop");
    case SET_REG_FALSE:
      return pretty_print::build_list(pretty_print::to_symbol("set!"), destination->to_form(file),
                                      "'#f");
    case SET_REG_TRUE:
      return pretty_print::build_list(pretty_print::to_symbol("set!"), destination->to_form(file),
                                      "'#t");
    case SET_REG_REG:
      return pretty_print::build_list(pretty_print::to_symbol("set!"), destination->to_form(file),
                                      source->to_form(file));
    case SET_BINTEGER:
      return pretty_print::build_list(pretty_print::to_symbol("set!"), destination->to_form(file),
                                      "binteger");
    case SET_PAIR:
      return pretty_print::build_list(pretty_print::to_symbol("set!"), destination->to_form(file),
                                      "pair");
    case DSLLV:
      return pretty_print::build_list(
          pretty_print::to_symbol("set!"), destination->to_form(file),
          pretty_print::build_list(pretty_print::to_symbol("shl"), source->to_form(file),
                                   source2->to_form(file)));
    case NEGATE:
      return pretty_print::build_list(pretty_print::to_symbol("set!"), destination->to_form(file),
                                      pretty_print::build_list("-", source->to_form(file)));
    case UNKNOWN:
      return pretty_print::build_list("unknown-branch-delay");
    default:
      assert(false);
      return {};
  }
}

void BranchDelay::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  if (destination) {
    output->push_back(destination);
  }

  if (source) {
    output->push_back(source);
  }

  if (source2) {
    output->push_back(source2);
  }
}

goos::Object IR_Nop::to_form(const LinkedObjectFile& file) const {
  (void)file;
  return pretty_print::build_list("nop!");
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
    case FLOAT_GREATER_THAN:
    case FLOAT_LEQ:
      return 2;
    case ZERO:
    case NONZERO:
    case FALSE:
    case TRUTHY:
    case GREATER_THAN_ZERO_SIGNED:
    case GEQ_ZERO_SIGNED:
    case LESS_THAN_ZERO:
    case LEQ_ZERO_SIGNED:
      return 1;
    case ALWAYS:
    case NEVER:
      return 0;
    default:
      assert(false);
      return -1;
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

void Condition::invert() {
  switch (kind) {
    case NOT_EQUAL:
      kind = EQUAL;
      break;
    case EQUAL:
      kind = NOT_EQUAL;
      break;
    case LESS_THAN_SIGNED:
      kind = GEQ_SIGNED;
      break;
    case GREATER_THAN_SIGNED:
      kind = LEQ_SIGNED;
      break;
    case LEQ_SIGNED:
      kind = GREATER_THAN_SIGNED;
      break;
    case GEQ_SIGNED:
      kind = LESS_THAN_SIGNED;
      break;
    case GREATER_THAN_ZERO_SIGNED:
      kind = LEQ_ZERO_SIGNED;
      break;
    case LEQ_ZERO_SIGNED:
      kind = GREATER_THAN_ZERO_SIGNED;
      break;
    case LESS_THAN_ZERO:
      kind = GEQ_ZERO_SIGNED;
      break;
    case GEQ_ZERO_SIGNED:
      kind = LESS_THAN_ZERO;
      break;
    case LESS_THAN_UNSIGNED:
      kind = GEQ_UNSIGNED;
      break;
    case GREATER_THAN_UNSIGNED:
      kind = LEQ_UNSIGNED;
      break;
    case LEQ_UNSIGNED:
      kind = GREATER_THAN_UNSIGNED;
      break;
    case GEQ_UNSIGNED:
      kind = LESS_THAN_UNSIGNED;
      break;
    case ZERO:
      kind = NONZERO;
      break;
    case NONZERO:
      kind = ZERO;
      break;
    case FALSE:
      kind = TRUTHY;
      break;
    case TRUTHY:
      kind = FALSE;
      break;
    case ALWAYS:
      kind = NEVER;
      break;
    case NEVER:
      kind = ALWAYS;
      break;
    case FLOAT_EQUAL:
      kind = FLOAT_NOT_EQUAL;
      break;
    case FLOAT_NOT_EQUAL:
      kind = FLOAT_EQUAL;
      break;
    case FLOAT_LESS_THAN:
      kind = FLOAT_GEQ;
      break;
    case FLOAT_GEQ:
      kind = FLOAT_LESS_THAN;
      break;
    case FLOAT_GREATER_THAN:
      kind = FLOAT_LEQ;
      break;
    case FLOAT_LEQ:
      kind = FLOAT_GREATER_THAN;
      break;
    default:
      assert(false);
  }
}

goos::Object Condition::to_form(const LinkedObjectFile& file) const {
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
    case NEVER:
      condtion_operator = "'#f";
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
    case FLOAT_GREATER_THAN:
      condtion_operator = ">.f";
      break;
    case FLOAT_LEQ:
      condtion_operator = "<=.f";
      break;
    case GREATER_THAN_ZERO_SIGNED:
      condtion_operator = ">0.si";
      break;
    case GEQ_ZERO_SIGNED:
      condtion_operator = ">=0.si";
      break;
    case LESS_THAN_ZERO:
      condtion_operator = "<0.si";
      break;
    case LEQ_ZERO_SIGNED:
      condtion_operator = "<=0.si";
      break;
    default:
      assert(false);
  }

  if (nargs == 2) {
    return pretty_print::build_list(pretty_print::to_symbol(condtion_operator), src0->to_form(file),
                                    src1->to_form(file));
  } else if (nargs == 1) {
    if (condtion_operator.empty()) {
      return src0->to_form(file);
    } else {
      return pretty_print::build_list(pretty_print::to_symbol(condtion_operator),
                                      src0->to_form(file));
    }
  } else if (nargs == 0) {
    return pretty_print::to_symbol(condtion_operator);
  } else {
    assert(false);
    return {};
  }
}

goos::Object IR_Branch::to_form(const LinkedObjectFile& file) const {
  return pretty_print::build_list(
      pretty_print::to_symbol(likely ? "bl!" : "b!"), condition.to_form(file),
      pretty_print::to_symbol(file.get_label_name(dest_label_idx)), branch_delay.to_form(file));
}

void IR_Branch::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  condition.get_children(output);
  branch_delay.get_children(output);
}

void IR_Branch_Atomic::update_reginfo_self(int n_dest, int n_src, int n_clobber) {
  // first, grab from condition
  for (auto& x : {condition.src0, condition.src1}) {
    auto as_reg = dynamic_cast<IR_Register*>(x.get());
    if (as_reg) {
      read_regs.push_back(as_reg->reg);
    }
  }

  auto as_reg = dynamic_cast<IR_Register*>(condition.clobber.get());
  if (as_reg) {
    clobber_regs.push_back(as_reg->reg);
  }
  assert(int(write_regs.size()) == n_dest);
  assert(int(read_regs.size()) == n_src);
  assert(int(clobber_regs.size()) == n_clobber);

  // copy from branch delay
  read_regs.insert(read_regs.end(), branch_delay.read_regs.begin(), branch_delay.read_regs.end());
  write_regs.insert(write_regs.end(), branch_delay.write_regs.begin(),
                    branch_delay.write_regs.end());
  clobber_regs.insert(clobber_regs.end(), branch_delay.clobber_regs.begin(),
                      branch_delay.clobber_regs.end());

  reg_info_set = true;
}

goos::Object IR_Compare::to_form(const LinkedObjectFile& file) const {
  return condition.to_form(file);
}

void IR_Compare::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  condition.get_children(output);
}

goos::Object IR_Suspend_Atomic::to_form(const LinkedObjectFile& file) const {
  (void)file;
  return pretty_print::build_list("suspend!");
}

void IR_Nop::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  (void)output;
}

void IR_Suspend_Atomic::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  (void)output;
}

goos::Object IR_Breakpoint_Atomic::to_form(const LinkedObjectFile& file) const {
  (void)file;
  return pretty_print::build_list("breakpoint!");
}

void IR_Breakpoint_Atomic::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  (void)output;
}

goos::Object IR_AsmOp::to_form(const LinkedObjectFile& file) const {
  std::vector<goos::Object> forms;
  forms.push_back(pretty_print::to_symbol(name));
  for (auto& x : {dst, src0, src1, src2}) {
    if (x) {
      forms.push_back(x->to_form(file));
    }
  }
  return pretty_print::build_list(forms);
}

void IR_AsmOp::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  for (auto& x : {dst, src0, src1}) {
    if (x) {
      output->push_back(x);
    }
  }
}

void IR_AsmOp_Atomic::set_reg_info() {
  auto dst_as_reg = dynamic_cast<IR_Register*>(dst.get());
  if (dst_as_reg) {
    write_regs.push_back(dst_as_reg->reg);
  }

  for (auto& x : {src0, src1, src2}) {
    auto src_as_reg = dynamic_cast<IR_Register*>(x.get());
    if (src_as_reg) {
      read_regs.push_back(src_as_reg->reg);
    }
  }

  reg_info_set = true;
}

goos::Object IR_CMoveF::to_form(const LinkedObjectFile& file) const {
  return pretty_print::build_list(
      pretty_print::to_symbol(on_zero ? "cmove-false-on-zero" : "cmove-false-on-nonzero"),
      src->to_form(file));
}

void IR_CMoveF::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  output->push_back(src);
}

goos::Object IR_AsmReg::to_form(const LinkedObjectFile& file) const {
  (void)file;
  switch (kind) {
    case VU_Q:
      return pretty_print::to_symbol("Q");
    case VU_ACC:
      return pretty_print::to_symbol("ACC");
    default:
      assert(false);
      return {};
  }
}

void IR_AsmReg::get_children(std::vector<std::shared_ptr<IR>>* output) const {
  (void)output;
}

}  // namespace decompiler