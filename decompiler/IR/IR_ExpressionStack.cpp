#include <algorithm>
#include "IR.h"
#include "decompiler/Function/ExpressionStack.h"

bool IR_Set_Atomic::expression_stack(ExpressionStack& stack, LinkedObjectFile& file) {
  // first determine the type of the set.
  switch (kind) {
    case IR_Set::REG_64:
    case IR_Set::LOAD:
    case IR_Set::GPR_TO_FPR:  // TODO - this should probably not be invisible.
    case IR_Set::FPR_TO_GPR64:
    case IR_Set::REG_FLT:
    case IR_Set::SYM_LOAD: {
      // normal 64-bit GPR set!
      // first, we update our source to substitute in more complicated expressions.
      auto src_as_reg = dynamic_cast<IR_Register*>(src.get());
      if (src_as_reg) {
        // an annoying special case.
        if (consumed.find(src_as_reg->reg) != consumed.end()) {
          // we consume it.
          src = stack.get(src_as_reg->reg);
        }
      } else {
        src->update_from_stack(consumed, stack, file);
      }

      // next, we tell the stack the value of the register we just set
      auto dest_reg = dynamic_cast<IR_Register*>(dst.get());
      assert(dest_reg);
      stack.set(dest_reg->reg, src);
      return true;
    }

    break;
    default:
      throw std::runtime_error("IR_Set_Atomic::expression_stack NYI for " + print(file));
  }
}

bool IR_Set::expression_stack(ExpressionStack& stack, LinkedObjectFile& file) {
  // first determine the type of the set.
  switch (kind) {
    case IR_Set::REG_64:
    case IR_Set::LOAD:
    case IR_Set::GPR_TO_FPR:  // TODO - this should probably not be invisible.
    case IR_Set::FPR_TO_GPR64:
    case IR_Set::REG_FLT: {
      // normal 64-bit GPR set!
      // first, we update our source to substitute in more complicated expressions.
      auto consumed = src->get_consumed(file);
      auto src_as_reg = dynamic_cast<IR_Register*>(src.get());
      if (src_as_reg) {
        // an annoying special case.
        if (consumed.find(src_as_reg->reg) != consumed.end()) {
          // we consume it.
          src = stack.get(src_as_reg->reg);
        }
      } else {
        src->update_from_stack(consumed, stack, file);
      }

      // next, we tell the stack the value of the register we just set
      auto dest_reg = dynamic_cast<IR_Register*>(dst.get());
      assert(dest_reg);
      stack.set(dest_reg->reg, src);
      return true;
    }

    break;
    default:
      throw std::runtime_error("IR_Set_Atomic::expression_stack NYI for " + print(file));
  }
}

bool IR_Call_Atomic::expression_stack(ExpressionStack& stack, LinkedObjectFile& file) {
  (void)file;
  if (!call_type_set) {
    throw std::runtime_error("Call type is unknown on an IR_Call_Atomic");
  }

  const Reg::Gpr arg_regs[8] = {Reg::A0, Reg::A1, Reg::A2, Reg::A3,
                                Reg::T0, Reg::T1, Reg::T2, Reg::T3};
  int nargs = int(call_type.arg_count()) - 1;
  // get all arguments.
  for (int i = nargs; i-- > 0;) {
    args.push_back(stack.get(Register(Reg::GPR, arg_regs[i])));
  }
  args.push_back(stack.get(Register(Reg::GPR, Reg::T9)));
  std::reverse(args.begin(), args.end());

  auto return_type = call_type.get_arg(call_type.arg_count() - 1);
  // bleh...
  stack.set(Register(Reg::GPR, Reg::V0), std::make_shared<IR_Call_Atomic>(*this));

  return true;
}

namespace {
void update_from_stack_helper(std::shared_ptr<IR>* ir,
                              const std::unordered_set<Register, Register::hash>& consume,
                              ExpressionStack& stack,
                              LinkedObjectFile& file) {
  auto as_reg = dynamic_cast<IR_Register*>(ir->get());
  if (as_reg) {
    if (consume.find(as_reg->reg) != consume.end()) {
      *ir = stack.get(as_reg->reg);
    }
  } else {
    (*ir)->update_from_stack(consume, stack, file);
  }
}
}  // namespace

bool IR_Load::update_from_stack(const std::unordered_set<Register, Register::hash>& consume,
                                ExpressionStack& stack,
                                LinkedObjectFile& file) {
  update_from_stack_helper(&location, consume, stack, file);
  return true;
}

bool IR_StaticAddress::update_from_stack(
    const std::unordered_set<Register, Register::hash>& consume,
    ExpressionStack& stack,
    LinkedObjectFile& file) {
  (void)consume;
  (void)stack;
  (void)file;
  return true;
}

bool IR_FloatMath2::update_from_stack(const std::unordered_set<Register, Register::hash>& consume,
                                      ExpressionStack& stack,
                                      LinkedObjectFile& file) {
  if (kind == DIV) {
    for (auto reg : {&arg1, &arg0}) {
      auto as_reg = dynamic_cast<IR_Register*>(reg->get());
      if (as_reg) {
        if (consume.find(as_reg->reg) != consume.end()) {
          *reg = stack.get(as_reg->reg);
        }
      } else {
        (*reg)->update_from_stack(consume, stack, file);
      }
    }
  } else {
    for (auto reg : {&arg0, &arg1}) {
      auto as_reg = dynamic_cast<IR_Register*>(reg->get());
      if (as_reg) {
        if (consume.find(as_reg->reg) != consume.end()) {
          *reg = stack.get(as_reg->reg);
        }
      } else {
        (*reg)->update_from_stack(consume, stack, file);
      }
    }
  }

  return true;
}

bool IR_IntMath2::update_from_stack(const std::unordered_set<Register, Register::hash>& consume,
                                    ExpressionStack& stack,
                                    LinkedObjectFile& file) {
  for (auto reg : {&arg1, &arg0}) {
    auto as_reg = dynamic_cast<IR_Register*>(reg->get());
    if (as_reg) {
      if (consume.find(as_reg->reg) != consume.end()) {
        *reg = stack.get(as_reg->reg);
      }
    } else {
      (*reg)->update_from_stack(consume, stack, file);
    }
  }
  return true;
}

std::unordered_set<Register, Register::hash> IR_Ash::get_consumed(LinkedObjectFile& file) {
  (void)file;
  // first get the set of read registers...
  auto value_as_reg = dynamic_cast<IR_Register*>(value.get());
  auto sa_as_reg = dynamic_cast<IR_Register*>(shift_amount.get());
  if (!sa_as_reg || !value_as_reg) {
    // consume nobody.
    // todo - is this actually right? If not, this is "safe", but might lead to ugly code.
    return {};
  }

  std::unordered_set<Register, Register::hash> result;

  for (auto& op : {branch_op, sub_op, shift_op}) {
    for (auto& reg : {value_as_reg->reg, sa_as_reg->reg}) {
      if (op->consumed.find(reg) != op->consumed.end()) {
        result.insert(reg);
      }
    }
  }

  return result;
}

bool IR_Ash::update_from_stack(const std::unordered_set<Register, Register::hash>& consume,
                               ExpressionStack& stack,
                               LinkedObjectFile& file) {
  for (auto x : {&value, &shift_amount}) {
    update_from_stack_helper(x, consume, stack, file);
  }
  return true;
}

std::unordered_set<Register, Register::hash> IR_IntMath1::get_consumed(LinkedObjectFile& file) {
  if (kind == ABS) {
    assert(abs_op);
    return abs_op->consumed;
  } else {
    throw std::runtime_error("IR_IntMath1::get_consumed NYI for " + print(file));
  }
}

bool IR_IntMath1::update_from_stack(const std::unordered_set<Register, Register::hash>& consume,
                                    ExpressionStack& stack,
                                    LinkedObjectFile& file) {
  update_from_stack_helper(&arg, consume, stack, file);
  return true;
}