/*!
 * @file Interpreter.cpp
 * The GOOS Interpreter and implementation of special forms.
 * Additional "built-in" forms are implemented in InterpreterEval
 */

#include <utility>
#include "Interpreter.h"

namespace goos {
Interpreter::Interpreter() {
  // Interpreter startup:
  goal_to_goos.reset();

  // create the GOOS global environment
  global_environment = EnvironmentObject::make_new("global");

  // create the environment which is be visible from GOAL
  goal_env = EnvironmentObject::make_new("goal");

  // make both environments available in both.
  define_var_in_env(global_environment, global_environment, "*global-env*");
  define_var_in_env(goal_env, goal_env, "*goal-env*");
  define_var_in_env(goal_env, global_environment, "*global-env*");
  define_var_in_env(global_environment, goal_env, "*goal-env*");

  // setup maps
  special_forms = {
      {"define", &Interpreter::eval_define},
      {"quote", &Interpreter::eval_quote},
      {"set!", &Interpreter::eval_set},
      {"lambda", &Interpreter::eval_lambda},
      {"cond", &Interpreter::eval_cond},
      {"or", &Interpreter::eval_or},
      {"and", &Interpreter::eval_and},
      {"macro", &Interpreter::eval_macro},
      {"quasiquote", &Interpreter::eval_quasiquote},
      {"while", &Interpreter::eval_while},
  };

  builtin_forms = {
      {"top-level", &Interpreter::eval_begin},
      {"begin", &Interpreter::eval_begin},
      {"exit", &Interpreter::eval_exit},
      {"read", &Interpreter::eval_read},
      {"read-file", &Interpreter::eval_read_file},
      {"print", &Interpreter::eval_print},
      {"inspect", &Interpreter::eval_inspect},
      {"load-file", &Interpreter::eval_load_file},
      {"eq?", &Interpreter::eval_equals},
      {"gensym", &Interpreter::eval_gensym},
      {"eval", &Interpreter::eval_eval},
      {"cons", &Interpreter::eval_cons},
      {"car", &Interpreter::eval_car},
      {"cdr", &Interpreter::eval_cdr},
      {"set-car!", &Interpreter::eval_set_car},
      {"set-cdr!", &Interpreter::eval_set_cdr},
      {"+", &Interpreter::eval_plus},
      {"-", &Interpreter::eval_minus},
      {"*", &Interpreter::eval_times},
      {"/", &Interpreter::eval_divide},
      {"=", &Interpreter::eval_numequals},
      {"<", &Interpreter::eval_lt},
      {">", &Interpreter::eval_gt},
      {"<=", &Interpreter::eval_leq},
      {">=", &Interpreter::eval_geq},
      {"null?", &Interpreter::eval_null},
      {"type?", &Interpreter::eval_type},
      {"current-method-type", &Interpreter::eval_current_method_type},
  };

  string_to_type = {{"empty-list", ObjectType::EMPTY_LIST},
                    {"integer", ObjectType::INTEGER},
                    {"float", ObjectType::FLOAT},
                    {"char", ObjectType::CHAR},
                    {"symbol", ObjectType::SYMBOL},
                    {"string", ObjectType::STRING},
                    {"pair", ObjectType::PAIR},
                    {"array", ObjectType::ARRAY},
                    {"lambda", ObjectType::LAMBDA},
                    {"macro", ObjectType::MACRO},
                    {"environment", ObjectType::ENVIRONMENT}};

  // load the standard library
  load_goos_library();
}

/*!
 * Disable printfs on errors, to make test output look less messy.
 */
void Interpreter::disable_printfs() {
  disable_printing = true;
}

/*!
 * Load the goos library, by interpreting (load-file "goal/gs/goos-lib.gs") in the global env.
 */
void Interpreter::load_goos_library() {
  auto cmd = "(load-file \"goalc/gs/goos-lib.gs\")";
  eval_with_rewind(reader.read_from_string(cmd), global_environment.as_env());
}

/*!
 * In env, set the variable named "name" to the value var.
 */
void Interpreter::define_var_in_env(Object& env, Object& var, const std::string& name) {
  env.as_env()->vars[intern(name).as_symbol()] = var;
}

/*!
 * Get a symbol with the given name, creating one if none exist.
 */
Object Interpreter::intern(const std::string& name) {
  return SymbolObject::make_new(reader.symbolTable, name);
}

/*!
 * Display the REPL, which will run until the user executes exit.
 */
void Interpreter::execute_repl() {
  while (!want_exit) {
    try {
      // read something from the user
      Object obj = reader.read_from_stdin("goos");
      // evaluate
      Object evald = eval_with_rewind(obj, global_environment.as_env());
      // print
      printf("%s\n", evald.print().c_str());
    } catch (std::exception& e) {
      printf("REPL Error: %s\n", e.what());
    }
  }
}

/*!
 * Signal an evaluation error. This throws an exception which will unwind the evaluation stack
 * for debugging.
 */
void Interpreter::throw_eval_error(const Object& o, const std::string& err) {
  throw std::runtime_error("[GOOS] Evaluation error on " + o.print() + ": " + err + "\n" +
                           reader.db.get_info_for(o));
}

/*!
 * Evaluate the given expression, with a "checkpoint" in the evaluation stack here.  If there is an
 * evaluation error, there will be a print indicating there was an error in the evaluation of "obj",
 * and if possible what file/line "obj" comes from.
 */
Object Interpreter::eval_with_rewind(const Object& obj,
                                     const std::shared_ptr<EnvironmentObject>& env) {
  Object result = EmptyListObject::make_new();
  try {
    result = eval(obj, env);
  } catch (std::runtime_error& e) {
    if (!disable_printing) {
      printf("-----------------------------------------\n");
      printf("From object %s\nat %s\n", obj.inspect().c_str(), reader.db.get_info_for(obj).c_str());
    }
    throw e;
  }
  return result;
}

/*!
 * Sets dest to the global variable with the given name, if the variable exists.
 * Returns if the variable was found.
 */
bool Interpreter::get_global_variable_by_name(const std::string& name, Object* dest) {
  auto kv = global_environment.as_env()->vars.find(
      SymbolObject::make_new(reader.symbolTable, name).as_symbol());
  if (kv != global_environment.as_env()->vars.end()) {
    *dest = kv->second;
    return true;
  }
  return false;
}

/*!
 * Get arguments being passed to a form. Don't evaluate them. There are two modes, "varargs" and
 * "not varargs".  With varargs enabled, any number of unnamed and named arguments can be given.
 * Without varags, the unnamed/named arguments must match the spec. By default specs are "not
 * vararg" - use make_varags() to get a varargs spec.  In general, macros/lambdas use specs, but
 * built-in forms use varargs.
 *
 * If form is "varargs", all arguments go to unnamed or named.
 *  Ex: (.... a b :key-1 c d) will put a, b, d in unnamed and d in key-1
 *
 * If form isn't "varargs", the expected number of unnamed arguments must match, unless "rest"
 * is specified, in which case the additional arguments are stored in rest.
 *
 * Also, if "varargs" isn't set, all keyword arguments must be defined. If the use doesn't provide
 * a value, the default value will be used instead.
 */
Arguments Interpreter::get_args(const Object& form, const Object& rest, const ArgumentSpec& spec) {
  Arguments args;

  // loop over forms in list
  Object current = rest;
  while (!current.is_empty_list()) {
    auto arg = current.as_pair()->car;

    // did we get a ":keyword"
    if (arg.is_symbol() && arg.as_symbol()->name.at(0) == ':') {
      auto key_name = arg.as_symbol()->name.substr(1);
      auto kv = spec.named.find(key_name);

      // check for unknown key name
      if (!spec.varargs && kv == spec.named.end()) {
        throw_eval_error(form, "Key argument " + key_name + " wasn't expected");
      }

      // check for multiple definition of key
      if (args.named.find(key_name) != args.named.end()) {
        throw_eval_error(form, "Key argument " + key_name + " multiply defined");
      }

      // check for well-formed :key value expression
      current = current.as_pair()->cdr;
      if (current.is_empty_list()) {
        throw_eval_error(form, "Key argument didn't have a value");
      }

      args.named[key_name] = current.as_pair()->car;
    } else {
      // not a keyword. Add to unnamed or rest, depending on what we expect
      if (spec.varargs || args.unnamed.size() < spec.unnamed.size()) {
        args.unnamed.push_back(arg);
      } else {
        args.rest.push_back(arg);
      }
    }
    current = current.as_pair()->cdr;
  }

  // Check expected key args and set default values on unset ones if possible
  for (auto& kv : spec.named) {
    auto defined_kv = args.named.find(kv.first);
    if (defined_kv == args.named.end()) {
      // key arg not given by user, try to use a default value.
      if (kv.second.has_default) {
        args.named[kv.first] = kv.second.default_value;
      } else {
        throw_eval_error(form,
                         "key argument \"" + kv.first + "\" wasn't given and has no default value");
      }
    }
  }

  // Check argument size, if spec defines it
  if (!spec.varargs) {
    if (args.unnamed.size() < spec.unnamed.size()) {
      throw_eval_error(form, "didn't get enough arguments");
    }
    assert(args.unnamed.size() == spec.unnamed.size());

    if (!args.rest.empty() && spec.rest.empty()) {
      throw_eval_error(form, "got too many arguments");
    }
  }

  return args;
}

/*!
 * Evaluate arguments in-place in the given environment.
 * Evaluation order is:
 *  - unnamed, in order of appearance
 *  - keyword, in alphabetical order
 *  - rest, in order of appearance
 *
 * Note that in varargs mode, all unnamed arguments are put in unnamed, not rest.
 */
void Interpreter::eval_args(Arguments* args, const std::shared_ptr<EnvironmentObject>& env) {
  for (auto& arg : args->unnamed) {
    arg = eval_with_rewind(arg, env);
  }

  for (auto& kv : args->named) {
    kv.second = eval_with_rewind(kv.second, env);
  }

  for (auto& arg : args->rest) {
    arg = eval_with_rewind(arg, env);
  }
}

/*!
 * Parse argument spec found in lambda/macro definition.
 * Like (x y &key z &key (w my-default-value) &rest body)
 */
ArgumentSpec Interpreter::parse_arg_spec(const Object& form, Object& rest) {
  ArgumentSpec spec;

  Object current = rest;
  while (!current.is_empty_list()) {
    auto arg = current.as_pair()->car;
    if (!arg.is_symbol()) {
      throw_eval_error(form, "args must be symbols");
    }

    if (arg.as_symbol()->name == "&rest") {
      // special case for &rest
      current = current.as_pair()->cdr;
      if (!current.is_pair()) {
        throw_eval_error(form, "rest arg must have a name");
      }
      auto rest_name = current.as_pair()->car;
      if (!rest_name.is_symbol()) {
        throw_eval_error(form, "rest name must be a symbol");
      }

      spec.rest = rest_name.as_symbol()->name;

      if (!current.as_pair()->cdr.is_empty_list()) {
        throw_eval_error(form, "rest must be the last argument");
      }
    } else if (arg.as_symbol()->name == "&key") {
      // special case for &key
      current = current.as_pair()->cdr;
      auto key_arg = current.as_pair()->car;
      if (key_arg.is_symbol()) {
        // form is &key name
        auto key_arg_name = key_arg.as_symbol()->name;
        if (spec.named.find(key_arg_name) != spec.named.end()) {
          throw_eval_error(form, "key argument " + key_arg_name + " multiply defined");
        }
        spec.named[key_arg_name] = NamedArg();
      } else if (key_arg.is_pair()) {
        // form is &key (name default-value)
        auto key_iter = key_arg;
        auto kn = key_iter.as_pair()->car;
        key_iter = key_iter.as_pair()->cdr;
        if (!kn.is_symbol()) {
          throw_eval_error(form, "key argument must have a symbol as a name");
        }
        auto key_arg_name = kn.as_symbol()->name;
        if (spec.named.find(key_arg_name) != spec.named.end()) {
          throw_eval_error(form, "key argument " + key_arg_name + " multiply defined");
        }
        NamedArg na;

        if (!key_iter.is_pair()) {
          throw_eval_error(form, "invalid keyword argument definition");
        }

        na.has_default = true;
        na.default_value = key_iter.as_pair()->car;

        if (!key_iter.as_pair()->cdr.is_empty_list()) {
          throw_eval_error(form, "invalid keyword argument definition");
        }

        spec.named[key_arg_name] = na;
      } else {
        throw_eval_error(form, "invalid key argument");
      }
    } else {
      spec.unnamed.push_back(arg.as_symbol()->name);
    }

    current = current.as_pair()->cdr;
  }
  return spec;
}

/*!
 * Argument check.
 * Must have the right number of unnamed arguments, with the right type.
 * Keyword arguments have a bool for "required" or not.
 * Extra keyword arguments are an error.
 */
void Interpreter::vararg_check(
    const Object& form,
    const Arguments& args,
    const std::vector<util::MatchParam<ObjectType>>& unnamed,
    const std::unordered_map<std::string, std::pair<bool, util::MatchParam<ObjectType>>>& named) {
  assert(args.rest.empty());
  if (unnamed.size() != args.unnamed.size()) {
    throw_eval_error(form, "Got " + std::to_string(args.unnamed.size()) +
                               " arguments, but expected " + std::to_string(unnamed.size()));
  }

  for (size_t i = 0; i < unnamed.size(); i++) {
    if (unnamed[i] != args.unnamed[i].type) {
      assert(!unnamed[i].is_wildcard);
      throw_eval_error(form, "Argument " + std::to_string(i) + " has type " +
                                 object_type_to_string(args.unnamed[i].type) + " but " +
                                 object_type_to_string(unnamed[i].value) + " was expected");
    }
  }

  for (const auto& kv : named) {
    auto kv2 = args.named.find(kv.first);
    if (kv2 == args.named.end()) {
      // argument not given.
      if (kv.second.first) {
        // but was required
        throw_eval_error(form, "Required named argument \"" + kv.first + "\" was not found");
      }
    } else {
      // argument given.
      if (kv.second.second != kv2->second.type) {
        // but is wrong type
        assert(!kv.second.second.is_wildcard);
        throw_eval_error(form, "Argument \"" + kv.first + "\" has type " +
                                   object_type_to_string(kv2->second.type) + " but " +
                                   object_type_to_string(kv.second.second.value) + " was expected");
      }
    }
  }

  for (const auto& kv : args.named) {
    if (named.find(kv.first) == named.end()) {
      throw_eval_error(form, "Got unrecognized keyword argument \"" + kv.first + "\"");
    }
  }
}

/*!
 * Evaluate a list and return the result of the last evaluation.
 */
Object Interpreter::eval_list_return_last(const Object& form,
                                          Object rest,
                                          const std::shared_ptr<EnvironmentObject>& env) {
  Object o = std::move(rest);
  Object rv = EmptyListObject::make_new();
  for (;;) {
    if (o.is_pair()) {
      auto op = o.as_pair();
      rv = eval_with_rewind(op->car, env);
      o = op->cdr;
    } else if (o.is_empty_list()) {
      return rv;
    } else {
      throw_eval_error(form, "malformed body to evaluate");
    }
  }
}

/*!
 * If o isn't an environment object, throws an evaluation error on form.
 */
void Interpreter::expect_env(const Object& form, const Object& o) {
  if (!o.is_env()) {
    throw_eval_error(form, "Object " + o.print() + " is a " + object_type_to_string(o.type) +
                               " but was expected to be an environment");
  }
}

/*!
 * Highest-level evaluation dispatch.
 */
Object Interpreter::eval(Object obj, const std::shared_ptr<EnvironmentObject>& env) {
  switch (obj.type) {
    case ObjectType::SYMBOL:
      return eval_symbol(obj, env);
    case ObjectType::PAIR:
      return eval_pair(obj, env);
    case ObjectType::INTEGER:
    case ObjectType::FLOAT:
    case ObjectType::STRING:
    case ObjectType::CHAR:
      return obj;
    default:
      throw_eval_error(obj, "cannot evaluate this object");
      return Object();
  }
}

namespace {

/*!
 * Try to find a symbol in an env or parent env. If successful, set dest and return true. Otherwise
 * return false.
 */
bool try_symbol_lookup(const Object& sym,
                       const std::shared_ptr<EnvironmentObject>& env,
                       Object* dest) {
  // booleans are hard-coded here
  if (sym.as_symbol()->name == "#t" || sym.as_symbol()->name == "#f") {
    *dest = sym;
    return true;
  }

  // loop up envs until we find it.
  std::shared_ptr<EnvironmentObject> search_env = env;
  for (;;) {
    auto kv = search_env->vars.find(sym.as_symbol());
    if (kv != search_env->vars.end()) {
      *dest = kv->second;
      return true;
    }

    auto pe = search_env->parent_env;
    if (pe) {
      search_env = pe;
    } else {
      return false;
    }
  }
}
}  // namespace

/*!
 * Evaluate a symbol by finding the closest scoped variable with matching name.
 */
Object Interpreter::eval_symbol(const Object& sym, const std::shared_ptr<EnvironmentObject>& env) {
  Object result;
  if (!try_symbol_lookup(sym, env, &result)) {
    throw_eval_error(sym, "symbol is not defined");
  }
  return result;
}

/*!
 * Evaluate a pair, either as special form, builtin form, macro application, or lambda application.
 */
Object Interpreter::eval_pair(const Object& obj, const std::shared_ptr<EnvironmentObject>& env) {
  auto pair = obj.as_pair();
  Object head = pair->car;
  Object rest = pair->cdr;

  // first see if we got a symbol:
  if (head.type == ObjectType::SYMBOL) {
    auto head_sym = head.as_symbol();

    // try a special form first
    auto kv_sf = special_forms.find(head_sym->name);
    if (kv_sf != special_forms.end()) {
      return ((*this).*(kv_sf->second))(obj, rest, env);
    }

    // try builtins next
    auto kv_b = builtin_forms.find(head_sym->name);
    if (kv_b != builtin_forms.end()) {
      Arguments args = get_args(obj, rest, make_varargs());
      // all "built-in" forms expect arguments to be evaluated (that's why they aren't special)
      eval_args(&args, env);
      return ((*this).*(kv_b->second))(obj, args, env);
    }

    // try macros next
    Object macro_obj;
    if (try_symbol_lookup(head, env, &macro_obj) && macro_obj.is_macro()) {
      auto macro = macro_obj.as_macro();
      Arguments args = get_args(obj, rest, macro->args);

      auto mac_env_obj = EnvironmentObject::make_new();
      auto mac_env = mac_env_obj.as_env();
      mac_env->parent_env = env;  // not 100% clear that this is right
      set_args_in_env(obj, args, macro->args, mac_env);
      // expand the macro!
      return eval_with_rewind(eval_list_return_last(macro->body, macro->body, mac_env), env);
    }
  }

  // eval the head and try it as a lambda
  Object eval_head = eval_with_rewind(head, env);
  if (eval_head.type != ObjectType::LAMBDA) {
    throw_eval_error(obj, "head of form didn't evaluate to lambda");
  }

  auto lam = eval_head.as_lambda();
  Arguments args = get_args(obj, rest, lam->args);
  eval_args(&args, env);
  auto lam_env_obj = EnvironmentObject::make_new();
  auto lam_env = lam_env_obj.as_env();
  lam_env->parent_env = lam->parent_env;
  set_args_in_env(obj, args, lam->args, lam_env);
  return eval_list_return_last(lam->body, lam->body, lam_env);
}

/*!
 * Given some arguments, an argument spec, and and environment, define the arguments are variables
 * in the environment.
 */
void Interpreter::set_args_in_env(const Object& form,
                                  const Arguments& args,
                                  const ArgumentSpec& arg_spec,
                                  const std::shared_ptr<EnvironmentObject>& env) {
  if (arg_spec.rest.empty() && args.unnamed.size() != arg_spec.unnamed.size()) {
    throw_eval_error(form, "did not get the expected number of unnamed arguments (got " +
                               std::to_string(args.unnamed.size()) + ", expected " +
                               std::to_string(arg_spec.unnamed.size()) + ")");
  } else if (!arg_spec.rest.empty() && args.unnamed.size() < arg_spec.unnamed.size()) {
    throw_eval_error(form, "args with rest didn't get enough arguments (got " +
                               std::to_string(args.unnamed.size()) + " but need at least " +
                               std::to_string(arg_spec.unnamed.size()) + ")");
  }

  // unnamed args
  for (size_t i = 0; i < arg_spec.unnamed.size(); i++) {
    env->vars[intern(arg_spec.unnamed.at(i)).as_symbol()] = args.unnamed.at(i);
  }

  // named args
  for (const auto& kv : arg_spec.named) {
    env->vars[intern(kv.first).as_symbol()] = args.named.at(kv.first);
  }

  // rest args
  if (!arg_spec.rest.empty()) {
    // will correctly handle the '() case
    env->vars[intern(arg_spec.rest).as_symbol()] = build_list(args.rest);
  } else {
    if (!args.rest.empty()) {
      throw_eval_error(form, "got too many arguments");
    }
  }
}

/*!
 * Define a variable in the current environment. The env can be overwritten with :env keyword arg
 */
Object Interpreter::eval_define(const Object& form,
                                const Object& rest,
                                const std::shared_ptr<EnvironmentObject>& env) {
  auto args = get_args(form, rest, make_varargs());
  vararg_check(form, args, {ObjectType::SYMBOL, {}}, {{"env", {false, {}}}});

  auto define_env = env;
  if (args.has_named("env")) {
    auto result = eval_with_rewind(args.get_named("env"), env);
    expect_env(form, result);
    define_env = result.as_env();
  }

  Object value = eval_with_rewind(args.unnamed[1], env);
  define_env->vars[args.unnamed[0].as_symbol()] = value;
  return value;
}

/*!
 * Set an existing variable. If there is no existing variable in the current environment, will
 * look at the parent environment.
 */
Object Interpreter::eval_set(const Object& form,
                             const Object& rest,
                             const std::shared_ptr<EnvironmentObject>& env) {
  auto args = get_args(form, rest, make_varargs());
  vararg_check(form, args, {ObjectType::SYMBOL, {}}, {});
  auto to_define = args.unnamed.at(0);
  Object to_set = eval_with_rewind(args.unnamed.at(1), env);

  std::shared_ptr<EnvironmentObject> search_env = env;
  for (;;) {
    auto kv = search_env->vars.find(to_define.as_symbol());
    if (kv != search_env->vars.end()) {
      kv->second = to_set;
      return kv->second;
    }

    auto pe = search_env->parent_env;
    if (pe) {
      search_env = pe;
    } else {
      throw_eval_error(to_define, "symbol is not defined");
    }
  }
}

/*!
 * Lambda definition special form.
 */
Object Interpreter::eval_lambda(const Object& form,
                                const Object& rest,
                                const std::shared_ptr<EnvironmentObject>& env) {
  if (!rest.is_pair()) {
    throw_eval_error(form, "lambda must receive two arguments");
  }

  Object arg_list = rest.as_pair()->car;
  if (!arg_list.is_pair() && !arg_list.is_empty_list()) {
    throw_eval_error(form, "lambda argument list must be a list");
  }

  Object new_lambda = LambdaObject::make_new();
  auto l = new_lambda.as_lambda();
  l->args = parse_arg_spec(form, arg_list);

  Object rrest = rest.as_pair()->cdr;
  if (!rrest.is_pair()) {
    throw_eval_error(form, "lambda body must be a list");
  }

  l->body = rrest;
  l->parent_env = env;
  return new_lambda;
}

/*!
 * Macro definition special form.
 */
Object Interpreter::eval_macro(const Object& form,
                               const Object& rest,
                               const std::shared_ptr<EnvironmentObject>& env) {
  if (!rest.is_pair()) {
    throw_eval_error(form, "macro must receive two arguments");
  }

  Object arg_list = rest.as_pair()->car;
  if (!arg_list.is_pair() && !arg_list.is_empty_list()) {
    throw_eval_error(form, "macro argument list must be a list");
  }

  Object new_macro = MacroObject::make_new();
  auto m = new_macro.as_macro();
  m->args = parse_arg_spec(form, arg_list);

  Object rrest = rest.as_pair()->cdr;
  if (!rrest.is_pair()) {
    throw_eval_error(form, "macro body must be a list");
  }

  m->body = rrest;
  m->parent_env = env;
  return new_macro;
}

/*!
 * Quote special form: (quote x) -> x
 */
Object Interpreter::eval_quote(const Object& form,
                               const Object& rest,
                               const std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  auto args = get_args(form, rest, make_varargs());
  vararg_check(form, args, {{}}, {});
  return args.unnamed.front();
}

/*!
 * Recursive quasi-quote evaluation
 */
Object Interpreter::quasiquote_helper(const Object& form,
                                      const std::shared_ptr<EnvironmentObject>& env) {
  Object lst = form;
  std::vector<Object> result;
  for (;;) {
    if (lst.type == ObjectType::PAIR) {
      Object item = lst.as_pair()->car;
      if (item.type == ObjectType::PAIR) {
        if (item.as_pair()->car.type == ObjectType::SYMBOL &&
            item.as_pair()->car.as_symbol()->name == "unquote") {
          Object unquote_arg = item.as_pair()->cdr;
          if (unquote_arg.type != ObjectType::PAIR ||
              unquote_arg.as_pair()->cdr.type != ObjectType::EMPTY_LIST) {
            throw_eval_error(form, "unquote must have exactly 1 arg");
          }
          item = eval_with_rewind(unquote_arg.as_pair()->car, env);
        } else if (item.as_pair()->car.type == ObjectType::SYMBOL &&
                   item.as_pair()->car.as_symbol()->name == "unquote-splicing") {
          Object unquote_arg = item.as_pair()->cdr;
          if (unquote_arg.type != ObjectType::PAIR ||
              unquote_arg.as_pair()->cdr.type != ObjectType::EMPTY_LIST) {
            throw_eval_error(form, "unquote must have exactly 1 arg");
          }
          item = eval_with_rewind(unquote_arg.as_pair()->car, env);

          // bypass normal addition:
          lst = lst.as_pair()->cdr;
          Object to_add = item;
          for (;;) {
            if (to_add.type == ObjectType::PAIR) {
              result.push_back(to_add.as_pair()->car);
              to_add = to_add.as_pair()->cdr;
            } else if (to_add.type == ObjectType::EMPTY_LIST) {
              break;
            } else {
              throw_eval_error(form, "malformed unquote-splicing result");
            }
          }
          continue;
        }

        else {
          item = quasiquote_helper(item, env);
        }
      }
      lst = lst.as_pair()->cdr;
      result.push_back(item);
    } else if (lst.type == ObjectType::EMPTY_LIST) {
      return build_list(result);
    } else {
      throw_eval_error(form, "malformed quasiquote");
    }
  }
}

/*!
 * Quasiquote (backtick) evaluation
 */
Object Interpreter::eval_quasiquote(const Object& form,
                                    const Object& rest,
                                    const std::shared_ptr<EnvironmentObject>& env) {
  if (rest.type != ObjectType::PAIR || rest.as_pair()->cdr.type != ObjectType::EMPTY_LIST)
    throw_eval_error(form, "quasiquote must have one argument!");
  return quasiquote_helper(rest.as_pair()->car, env);
}

namespace {
bool truthy(const Object& o) {
  return !(o.is_symbol() && o.as_symbol()->name == "#f");
}
}  // namespace

/*!
 * Scheme "cond" statement - tested by integrated tests only.
 */
Object Interpreter::eval_cond(const Object& form,
                              const Object& rest,
                              const std::shared_ptr<EnvironmentObject>& env) {
  if (rest.type != ObjectType::PAIR)
    throw_eval_error(form, "cond must have at least one clause, which must be a form");
  Object result;

  Object lst = rest;
  for (;;) {
    if (lst.type == ObjectType::PAIR) {
      Object current_case = lst.as_pair()->car;
      if (current_case.type != ObjectType::PAIR)
        throw_eval_error(lst, "bogus cond case");

      // check condition:
      Object condition_result = eval_with_rewind(current_case.as_pair()->car, env);
      if (truthy(condition_result)) {
        if (current_case.as_pair()->cdr.type == ObjectType::EMPTY_LIST) {
          return condition_result;
        }
        // got a match!
        return eval_list_return_last(current_case, current_case.as_pair()->cdr, env);
      } else {
        // no match, continue.
        lst = lst.as_pair()->cdr;
      }
    } else if (lst.type == ObjectType::EMPTY_LIST) {
      return SymbolObject::make_new(reader.symbolTable, "#f");
    } else {
      throw_eval_error(form, "malformed cond");
    }
  }
}

/*!
 * Short circuiting "or" statement
 */
Object Interpreter::eval_or(const Object& form,
                            const Object& rest,
                            const std::shared_ptr<EnvironmentObject>& env) {
  if (rest.type != ObjectType::PAIR) {
    throw_eval_error(form, "or must have at least one argument!");
  }

  Object lst = rest;
  for (;;) {
    if (lst.type == ObjectType::PAIR) {
      Object current = eval_with_rewind(lst.as_pair()->car, env);
      if (truthy(current)) {
        return current;
      }
      lst = lst.as_pair()->cdr;
    } else if (lst.type == ObjectType::EMPTY_LIST) {
      return SymbolObject::make_new(reader.symbolTable, "#f");
    } else {
      throw_eval_error(form, "invalid or form");
    }
  }
}

/*!
 * Short circuiting "and" statement
 */
Object Interpreter::eval_and(const Object& form,
                             const Object& rest,
                             const std::shared_ptr<EnvironmentObject>& env) {
  if (rest.type != ObjectType::PAIR) {
    throw_eval_error(form, "and must have at least one argument!");
  }

  Object lst = rest;
  Object current;
  for (;;) {
    if (lst.type == ObjectType::PAIR) {
      current = eval_with_rewind(lst.as_pair()->car, env);
      if (!truthy(current)) {
        return SymbolObject::make_new(reader.symbolTable, "#f");
      }
      lst = lst.as_pair()->cdr;
    } else if (lst.type == ObjectType::EMPTY_LIST) {
      return current;
    } else {
      throw_eval_error(form, "invalid and form");
    }
  }
}

/*!
 * Cheating "while loop" because we do not have tail recursion optimization yet.
 */
Object Interpreter::eval_while(const Object& form,
                               const Object& rest,
                               const std::shared_ptr<EnvironmentObject>& env) {
  if (rest.type != ObjectType::PAIR) {
    throw_eval_error(form, "while must have condition and body");
  }

  Object condition = rest.as_pair()->car;
  Object body = rest.as_pair()->cdr;
  if (body.type != ObjectType::PAIR) {
    throw_eval_error(form, "while must have condition and body");
  }

  Object rv = SymbolObject::make_new(reader.symbolTable, "#f");
  while (truthy(eval_with_rewind(condition, env))) {
    rv = eval_list_return_last(form, body, env);
  }

  return rv;
}

}  // namespace goos