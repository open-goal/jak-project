#include "Goal.h"

std::shared_ptr<Place> Goal::compile_print_type(const Object& form,
                                                Object rest,
                                                std::shared_ptr<GoalEnv> env) {
  auto first = rest.as_pair()->car;
  if (rest.as_pair()->cdr.type != EMPTY_LIST) {
    throw_compile_error(form, "invalid print-type");
  }

  auto result = compile_error_guard(first, env);

  printf("CODE: %s\nTYPE: %s\nPLACE: %s\n", first.print().c_str(), result->type.print().c_str(),
         result->print().c_str());
  return get_none();
}

std::shared_ptr<Place> Goal::compile_quote(const Object& form,
                                           Object rest,
                                           std::shared_ptr<GoalEnv> env) {
  auto first = rest.as_pair()->car;
  if (rest.as_pair()->cdr.type != EMPTY_LIST) {
    throw_compile_error(form, "invalid print-type");
  }

  switch (first.type) {
    case SYMBOL:
      return compile_get_sym_obj(first.as_symbol()->name, env);
      break;

    case EMPTY_LIST: {
      auto empty_pair = compile_get_sym_obj("_empty_", env);
      empty_pair->type = get_base_typespec("pair");
      return empty_pair;
    }

    default:
      throw_compile_error(rest, "cannot quote this yet");
  }

  return get_none();
}

std::shared_ptr<Place> Goal::compile_defconstant(const Object& form,
                                                 Object rest,
                                                 std::shared_ptr<GoalEnv> env) {
  (void)env;
  if (rest.type != PAIR) {
    throw_compile_error(form, "invalid defglobalconstant");
  }

  auto sym = rest.as_pair()->car;
  if (sym.type != SYMBOL) {
    throw_compile_error(form, "invalid defglobalconstant");
  }

  auto ssym = sym.as_symbol();

  rest = rest.as_pair()->cdr;
  if (rest.type != PAIR) {
    throw_compile_error(form, "invalid defglobalconstant");
  }

  auto value = rest.as_pair()->car;

  rest = rest.as_pair()->cdr;
  if (rest.type != EMPTY_LIST) {
    throw_compile_error(form, "invalid defglobalconstant");
  }

  // GOAL constant
  global_constants[ssym] = value;
  return get_none();
}

std::shared_ptr<Place> Goal::compile_current_method_type(const Object& form,
                                                         Object rest,
                                                         std::shared_ptr<GoalEnv> env) {
  if (rest.type != EMPTY_LIST) {
    throw_compile_error(form, "current-method-type accepts no arguments");
  }
  return compile_get_sym_obj(get_parent_env_of_type<FunctionEnv>(env)->method_of_type_name, env);
}

/*!
 * Try to find a macro with the given name in the goal_goos_env of GOOS.
 */
bool Goal::try_getting_macro_from_goos(Object macro_name, Object* dest) {
  Object macro_obj;
  bool got_macro = false;
  try {
    macro_obj = goos.eval_symbol(macro_name, goos.goal_goos_env.as_env());
    if (macro_obj.type == MACRO) {
      got_macro = true;
    }
  } catch (std::runtime_error& e) {
    got_macro = false;
  }

  if (got_macro) {
    *dest = macro_obj;
  }
  return got_macro;
}

std::shared_ptr<Place> Goal::compile_goos_macro(Object o,
                                                Object macro_obj,
                                                Object rest,
                                                std::shared_ptr<GoalEnv> env) {
  // we got a macro, so first set up for macro expansion by creating a GOOS environment where the
  // GOAL stuff is bound to the macro params:
  GoosArgs args = goos.get_macro_args(o, rest);
  // todo define keywords here
  auto macro = macro_obj.as_macro();
  auto mac_env_obj = EnvironmentObject::make_new();
  auto mac_env = mac_env_obj.as_env();

  mac_env->parent_env = goos.global_environment.as_env();

  // check argument count
  if (!macro->has_rest && args.unnamed_args.size() != macro->unnamed_args.size()) {
    throw_compile_error(o, "Macro " + macro->name + " requires " +
                               std::to_string(macro->unnamed_args.size()) + " but got " +
                               std::to_string(args.unnamed_args.size()));
  } else if (macro->has_rest && args.unnamed_args.size() < macro->unnamed_args.size()) {
    throw_compile_error(o, "Macro " + macro->name + " requires at least " +
                               std::to_string(macro->unnamed_args.size()) + " but only got " +
                               std::to_string(args.unnamed_args.size()));
  }

  // populate macro env.
  uint32_t i = 0;
  for (; i < macro->unnamed_args.size(); i++) {
    mac_env->vars[macro->unnamed_args.at(i).as_symbol()] = args.unnamed_args.at(i);
  }

  if (macro->has_rest) {
    if (i < args.unnamed_args.size()) {
      Object empty = EmptyListObject::make_new();
      Object rest_head = PairObject::make_new(args.unnamed_args[i], empty);
      Object last = rest_head;
      i++;
      for (; i < args.unnamed_args.size(); i++) {
        last.as_pair()->cdr = PairObject::make_new(args.unnamed_args[i], empty);
        last = last.as_pair()->cdr;
      }
      mac_env->vars[macro->rest_args.as_symbol()] = rest_head;
    } else {
      mac_env->vars[macro->rest_args.as_symbol()] = EmptyListObject::make_new();
    }
  }

  goos.goal_to_goos.enclosing_method_type =
      get_parent_env_of_type<FunctionEnv>(env)->method_of_type_name;
  auto goos_result = goos.eval_list_return_last(macro->body, macro->body, mac_env);
  goos.goal_to_goos.reset();
  // then compile the result of compiling the GOOS eval'd macro body.
  return compile_error_guard(goos_result, env);
}