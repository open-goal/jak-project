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
        // we're reading a register. Let's find out if it's safe to directly copy it's value.
        if (consumed.find(src_as_reg->reg) != consumed.end()) {
          // yep. Let's read it off of the stack.
          src = stack.get(src_as_reg->reg);
        }
      } else {
        // our source is some expression. we need to make sure the expression is up-to-date.
        src->update_from_stack(consumed, stack, file);
      }

      // next, we tell the stack the value of the register we just set
      auto dest_reg = dynamic_cast<IR_Register*>(dst.get());
      assert(dest_reg);
      // sequence point if not a register -> register set.
      stack.set(dest_reg->reg, src, !src_as_reg);
      return true;
    }

    case IR_Set::STORE:
    case IR_Set::SYM_STORE: {
      auto src_as_reg = dynamic_cast<IR_Register*>(src.get());
      if (src_as_reg) {
        // we're reading a register. Let's find out if it's safe to directly copy it's value.
        if (consumed.find(src_as_reg->reg) != consumed.end()) {
          // yep. Let's read it off of the stack.
          src = stack.get(src_as_reg->reg);
        }
      } else {
        // our source is some expression. we need to make sure the expression is up-to-date.
        src->update_from_stack(consumed, stack, file);
      }
      stack.add_no_set(std::make_shared<IR_Set_Atomic>(*this), true);
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
      stack.set(dest_reg->reg, src, !src_as_reg);
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
  //  printf("%s\n", stack.print(file).c_str());
  // get all arguments.
  for (int i = nargs; i-- > 0;) {
    args.push_back(stack.get(Register(Reg::GPR, arg_regs[i])));
  }
  args.push_back(stack.get(Register(Reg::GPR, Reg::T9)));
  std::reverse(args.begin(), args.end());

  auto return_type = call_type.get_arg(call_type.arg_count() - 1);
  // bleh...
  stack.set(Register(Reg::GPR, Reg::V0), std::make_shared<IR_Call_Atomic>(*this), true);

  return true;
}

bool IR_UntilLoop::expression_stack(ExpressionStack& stack, LinkedObjectFile& file) {
  (void)stack;
  (void)file;
  stack.add_no_set(std::make_shared<IR_UntilLoop>(*this), true);
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

bool IR_Compare::expression_stack(ExpressionStack& stack, LinkedObjectFile& file) {
  if (condition.kind != Condition::ALWAYS) {
    assert(root_op);
    //  auto consumed = root_op->get_consumed(file);
    auto& consumed = root_op->consumed;
    switch (condition.num_args()) {
      case 0:
        break;
      case 1:
        update_from_stack_helper(&condition.src0, consumed, stack, file);
        break;
      case 2:
        update_from_stack_helper(&condition.src1, consumed, stack, file);
        update_from_stack_helper(&condition.src0, consumed, stack, file);
        break;
      default:
        assert(false);
    }
  }

  stack.add_no_set(std::make_shared<IR_Compare>(*this), true);
  return true;
}

bool IR_Compare::update_from_stack(const std::unordered_set<Register, Register::hash>& consume,
                                   ExpressionStack& stack,
                                   LinkedObjectFile& file) {
  if (condition.kind != Condition::ALWAYS) {
    switch (condition.num_args()) {
      case 0:
        break;
      case 1:
        update_from_stack_helper(&condition.src0, consume, stack, file);
        break;
      case 2:
        update_from_stack_helper(&condition.src1, consume, stack, file);
        update_from_stack_helper(&condition.src0, consume, stack, file);
        break;
      default:
        assert(false);
    }
  }
  return true;
}

bool IR_ShortCircuit::expression_stack(ExpressionStack& stack, LinkedObjectFile& file) {
  (void)file;
  // this one is weird. All forms but the last implicitly set final_destination.
  // the last form should somewhere set final_destination, but due to tricky coloring we
  // can't identify this 100% of the time.
  // so we settle for something like:
  // (set! result (or <clause-a> ... (begin (blah) (set! result x) (blah))))
  // in the future, we may want to handle this a little bit better, at least in the obvious cases.

  assert(final_result);
  assert(used_as_value.has_value());

  if (used_as_value.value()) {
    auto dest_reg = dynamic_cast<IR_Register*>(final_result.get());

    // try as a set
    auto last_entry_as_set = dynamic_cast<IR_Set*>(entries.back().condition.get());
    if (last_entry_as_set) {
      auto sd = last_entry_as_set->dst;
      auto sd_as_reg = dynamic_cast<IR_Register*>(sd.get());
      if (sd_as_reg && sd_as_reg->reg == dest_reg->reg) {
        entries.back().condition = last_entry_as_set->src;
        stack.set(dest_reg->reg, std::make_shared<IR_ShortCircuit>(*this), true);
        return true;
      }
    }

    // try as the last thing in a begin.
    auto last_entry_as_begin = dynamic_cast<IR_Begin*>(entries.back().condition.get());
    if (last_entry_as_begin) {
      last_entry_as_set = dynamic_cast<IR_Set*>(last_entry_as_begin->forms.back().get());
      if (last_entry_as_set) {
        auto sd = last_entry_as_set->dst;
        auto sd_as_reg = dynamic_cast<IR_Register*>(sd.get());
        if (sd_as_reg && sd_as_reg->reg == dest_reg->reg) {
          entries.back().condition = last_entry_as_set->src;
          stack.set(dest_reg->reg, std::make_shared<IR_ShortCircuit>(*this), true);
          return true;
        }
      }
    }

    // nope. if we have something like (and x (if a b c)), we may need to explictly add an
    // evaluation of the if's result.
    auto new_last_entry = std::make_shared<IR_Begin>();
    new_last_entry->forms.push_back(entries.back().condition);
    new_last_entry->forms.push_back(std::make_shared<IR_Register>(dest_reg->reg, -1));
    entries.back().condition = new_last_entry;

    stack.set(dest_reg->reg, std::make_shared<IR_ShortCircuit>(*this), true);
    return true;

    //    throw std::runtime_error("Last entry in short circuit was bad: " +
    //                             entries.back().condition->print(file));
  } else {
    stack.add_no_set(std::make_shared<IR_ShortCircuit>(*this), true);
    return true;
  }
}

bool IR_Cond::expression_stack(ExpressionStack& stack, LinkedObjectFile& file) {
  if (used_as_value) {
    // we have to make sure that all of the bodies evaluate to the value stored in the
    // final_destination register.

    for (auto& entry : entries) {
      IR* current_ir = entry.body.get();
      while (dynamic_cast<IR_Begin*>(current_ir)) {
        current_ir = dynamic_cast<IR_Begin*>(current_ir)->forms.back().get();
      }
      auto as_set = dynamic_cast<IR_Set*>(current_ir);
      if (as_set) {
        auto sd = as_set->dst;
        auto sd_as_reg = dynamic_cast<IR_Register*>(sd.get());
        if (sd_as_reg && sd_as_reg->reg == final_destination) {
          // yep! it's okay. set!'s evaluate to the thing they are setting.
          continue;
        }
      }
      throw std::runtime_error("IR_Cond used as value didn't work for reg " +
                               final_destination.to_string() + "\n" + entry.body->print(file));
    }
    return true;
  } else {
    (void)file;
    stack.add_no_set(std::make_shared<IR_Cond>(*this), true);
    return true;
  }
}

bool IR_WhileLoop::expression_stack(ExpressionStack& stack, LinkedObjectFile& file) {
  (void)file;
  // while loops are never "used by value" yet, but this is okay because they don't
  // do any tricks in delay slots like IR_Cond's do.
  stack.add_no_set(std::make_shared<IR_WhileLoop>(*this), true);
  return true;
}

bool IR_AsmOp::expression_stack(ExpressionStack& stack, LinkedObjectFile& file) {
  (void)file;
  // we only fall back to asm ops if we don't understand the GOAL code, or if the original code
  // used inline assembly. In these cases, we create a sequence point here.
  stack.add_no_set(std::make_shared<IR_AsmOp>(*this), true);
  return true;
}

bool IR_CondWithElse::expression_stack(ExpressionStack& stack, LinkedObjectFile& file) {
  (void)file;
  // cond with else are never "used by value" yet, but this is okay because they don't
  // do any tricks in delay slots like IR_Cond's do.
  stack.add_no_set(std::make_shared<IR_CondWithElse>(*this), true);
  return true;
}

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

bool IR_GetRuntimeType::update_from_stack(
    const std::unordered_set<Register, Register::hash>& consume,
    ExpressionStack& stack,
    LinkedObjectFile& file) {
  update_from_stack_helper(&object, consume, stack, file);
  return true;
}

std::unordered_set<Register, Register::hash> IR_GetRuntimeType::get_consumed(
    LinkedObjectFile& file) {
  // todo, this can actually consume stuff.
  (void)file;
  return {};
}

std::unordered_set<Register, Register::hash> IR_Compare::get_consumed(LinkedObjectFile& file) {
  // todo, this can actually consume stuff.
  (void)file;
  return {};
}

bool IR_Nop::expression_stack(ExpressionStack& stack, LinkedObjectFile& file) {
  (void)stack;
  (void)file;
  return true;
}

bool IR_CMoveF::update_from_stack(const std::unordered_set<Register, Register::hash>& consume,
                                  ExpressionStack& stack,
                                  LinkedObjectFile& file) {
  update_from_stack_helper(&src, consume, stack, file);
  return true;
}

bool IR_FloatMath1::update_from_stack(const std::unordered_set<Register, Register::hash>& consume,
                                      ExpressionStack& stack,
                                      LinkedObjectFile& file) {
  update_from_stack_helper(&arg, consume, stack, file);
  return true;
}