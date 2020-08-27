/*!
 * @file GoalAsm.cpp
 * GOAL Assembly forms, used to include x86 instructions directly in the program.
 */

#include "Goal.h"
#include "util.h"

/*!
 * Helper to compile any of the assembly forms.
 */
std::shared_ptr<Place> Goal::compile_asm(const Object& form,
                                         Object rest,
                                         std::shared_ptr<GoalEnv> env) {
  // get op and args
  auto op = symbol_string(pair_car(form));
  std::vector<std::shared_ptr<Place>> args;
  for_each_in_list(rest, [&](Object o) {
    args.push_back(resolve_to_gpr_or_xmm(compile_error_guard(o, env), env));
  });

  auto check_arg_count = [&](size_t desired) {
    if (desired != args.size()) {
      throw_compile_error(form, "Assembly form got " + std::to_string(args.size()) +
                                    " arguments, but requires " + std::to_string(desired));
    }
  };

  // check argument count and emit the correct IR
  if (op == ".ret") {
    check_arg_count(0);
    env->emit(make_unique<IR_Asm>(IR_Asm::RET, args));
  } else if (op == ".ret-reg") {
    check_arg_count(1);
    env->emit(make_unique<IR_Asm>(IR_Asm::RET_REGISTER, args));
  } else if (op == ".push") {
    check_arg_count(1);
    env->emit(make_unique<IR_Asm>(IR_Asm::PUSH, args));
  } else if (op == ".jmp") {
    check_arg_count(1);
    env->emit(make_unique<IR_Asm>(IR_Asm::JMP, args));
  } else if (op == ".sub") {
    check_arg_count(2);
    env->emit(make_unique<IR_Asm>(IR_Asm::SUB, args));
  } else if (op == ".pop") {
    check_arg_count(1);
    env->emit(make_unique<IR_Asm>(IR_Asm::POP, args));
  }

  else {
    ice("Goal::compile_asm encountered an unknown asm operation: " + op);
  }

  return get_none();
}