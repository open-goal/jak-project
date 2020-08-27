#include "Goos.h"
#include "Object.h"

Goos::Goos() {
  goal_to_goos.reset();
  global_environment = EnvironmentObject::make_new();
  global_environment.as_env()->name = "global";
  global_environment.as_env()->parent_env = nullptr;
  // global_environment.as_env()->vars[SymbolObject::make_new(reader.symbolTable,
  // "*global-env*").as_symbol()] = global_environment;

  goal_goos_env = EnvironmentObject::make_new();
  goal_goos_env.as_env()->name = "goal";
  goal_goos_env.as_env()->parent_env = nullptr;

  // make both environments available in both.
  define_var_in_env(global_environment, global_environment, "*global-env*");
  define_var_in_env(goal_goos_env, goal_goos_env, "*goal-env*");
  define_var_in_env(goal_goos_env, global_environment, "*global-env*");
  define_var_in_env(global_environment, goal_goos_env, "*goal-env*");
  load_goos_library();
  // eval_with_rewind(reader.read_from_string("(load-file \"compiler-lib/goos-lib.gs\")"),
  // global_environment.as_env());
}

void Goos::load_goos_library() {
  auto next_dir = reader.get_next_dir();
  auto cmd = "(load-file \"old_compiler/gs/goos-lib.gs\")";
  eval_with_rewind(reader.read_from_string(cmd), global_environment.as_env());
}

void Goos::define_var_in_env(Object& env, Object& var, const std::string& name) {
  env.as_env()->vars[SymbolObject::make_new(reader.symbolTable, name).as_symbol()] = var;
}

void print_and_inspect(Object o) {
  printf("%s\n%s\n", o.print().c_str(), o.inspect().c_str());
}

void Goos::execute_repl() {
  // read, evaluate, print loop!
  while (!want_exit) {
    try {
      Object obj = reader.read_from_stdin("goos");
      // print_and_inspect(obj);
      Object evald = eval_with_rewind(obj, global_environment.as_env());
      printf("%s\n", evald.print().c_str());
      // print_and_inspect(evald);
    } catch (std::exception& e) {
      printf("REPL Error: %s\n", e.what());
    }
  }
}

Object Goos::eval_with_rewind(Object obj, std::shared_ptr<EnvironmentObject> env) {
  Object result = EmptyListObject::make_new();
  try {
    result = eval(obj, env);
  } catch (std::runtime_error& e) {
    printf("-----------------------------------------\n");
    printf("Eval error:\n%s\nobject %s\nat %s\n", e.what(), obj.inspect().c_str(),
           reader.db.get_info_for(obj).c_str());
    throw e;
  }
  return result;
}

Object Goos::eval(Object obj, std::shared_ptr<EnvironmentObject> env) {
  switch (obj.type) {
    case PAIR:
      return eval_pair(obj, env);
      break;

    case INTEGER:
    case FLOAT:
    case STRING:
      return obj;

    case SYMBOL:
      return eval_symbol(obj, env);

    default:
      throw_eval_error(obj, "cannot evaluate this object");
      return Object();
  }
}

Object Goos::eval_symbol(Object sym, std::shared_ptr<EnvironmentObject> env) {
  if (sym.as_symbol()->name == "#t")
    return sym;
  if (sym.as_symbol()->name == "#f")
    return sym;

  std::shared_ptr<EnvironmentObject> search_env = env;
  for (;;) {
    auto kv = search_env->vars.find(sym.as_symbol());
    if (kv != search_env->vars.end()) {
      return kv->second;
    }

    auto pe = search_env->parent_env;
    if (pe) {
      search_env = pe;
    } else {
      throw_eval_error(sym, "symbol is not defined");
    }
  }
}

void Goos::throw_eval_error(Object o, const std::string& err) {
  (void)o;
  throw std::runtime_error(err);
}

static const std::unordered_map<
    std::string,
    Object (Goos::*)(const Object& form, Object rest, std::shared_ptr<EnvironmentObject>& env)>
    special_forms = {
        {"define", &Goos::eval_define},
        {"quote", &Goos::eval_quote},
        {"set!", &Goos::eval_set},
        {"lambda", &Goos::eval_lambda},
        {"cond", &Goos::eval_cond},
        {"or", &Goos::eval_or},
        {"and", &Goos::eval_and},
        {"macro", &Goos::eval_macro},
        {"quasiquote", &Goos::eval_quasiquote},
        {"while", &Goos::eval_while},
};

static const std::unordered_map<
    std::string,
    Object (Goos::*)(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env)>
    builtins = {
        {"top-level", &Goos::eval_begin},
        {"begin", &Goos::eval_begin},
        {"exit", &Goos::eval_exit},
        {"read", &Goos::eval_read},
        {"read-file", &Goos::eval_read_file},
        {"print", &Goos::eval_print},
        {"inspect", &Goos::eval_inspect},
        {"load-file", &Goos::eval_load_file},
        {"eq?", &Goos::eval_equals},
        {"gensym", &Goos::eval_gensym},
        {"eval", &Goos::eval_eval},

        {"cons", &Goos::eval_cons},
        {"car", &Goos::eval_car},
        {"cdr", &Goos::eval_cdr},
        // set-car!
        // set-cdr!
        {"+", &Goos::eval_plus},
        {"-", &Goos::eval_minus},
        {"*", &Goos::eval_times},
        // divide
        {"=", &Goos::eval_numequals},
        {"<", &Goos::eval_lt},
        {">", &Goos::eval_gt},
        {"<=", &Goos::eval_leq},
        {">=", &Goos::eval_geq},
        // eval
        // not
        // xor
        // nor
        // nand
        // position
        // length
        {"null?", &Goos::eval_null},
        {"type?", &Goos::eval_type},
        {"current-method-type", &Goos::eval_current_method_type},
        // the float
        // the int
        // the char
        // is pair
        // is symbol
        // is integer
        // is char
        // is float
        // is null
        // is proc
        // is macro
        // is array
        // is string
        // string manip
        // array manip
        // lots more...
};

GoosArgs Goos::eval_args(const Object& form, Object rest, std::shared_ptr<EnvironmentObject>& env) {
  // todo, keyword args
  Object o = rest;
  GoosArgs args;
  for (;;) {
    if (o.type == PAIR) {
      auto op = o.as_pair();
      args.unnamed_args.push_back(eval_with_rewind(op->car, env));
      o = op->cdr;
    } else if (o.type == EMPTY_LIST) {
      return args;
    } else {
      throw_eval_error(form, "malformed argument list");
    }
  }
}

GoosArgs Goos::get_macro_args(const Object& form, Object rest) {
  // todo, keyword args
  Object o = rest;
  GoosArgs args;
  for (;;) {
    if (o.type == PAIR) {
      auto op = o.as_pair();
      args.unnamed_args.push_back(op->car);
      o = op->cdr;
    } else if (o.type == EMPTY_LIST) {
      return args;
    } else {
      throw_eval_error(form, "malformed argument list");
    }
  }
}

GoosArgs Goos::get_uneval_args(const Object& form, Object rest, int count) {
  GoosArgs args;
  args.rest = EmptyListObject::make_new();
  Object o = rest;
  auto next = [&]() {
    if (rest.type != PAIR) {
      throw_eval_error(form, "invalid arguments");
    }
    o = rest.as_pair()->car;
    rest = rest.as_pair()->cdr;
  };

  next();

  int got = 0;
  for (;;) {
    bool add_normal = true;

    if (o.type == SYMBOL) {
      auto osym = o.as_symbol();
      if (osym->name[0] == ':') {
        add_normal = false;
        std::string name = osym->name.substr(1);
        next();
        if (args.named_args.find(name) != args.named_args.end()) {
          throw_eval_error(rest, "multiply defined keyword argument");
        }
        args.named_args[name] = o;
      }
    }

    if (add_normal) {
      args.unnamed_args.push_back(o);
      got++;

      if (got == count) {
        args.rest = rest;
        args.has_rest = true;
        break;
      }
    }

    if (rest.type == PAIR) {
      next();
    } else {
      break;
    }
  }
  return args;
}

GoosArgs Goos::get_uneval_args_no_rest(const Object& form, Object rest, int count) {
  GoosArgs args;
  args.has_rest = false;
  args.rest = EmptyListObject::make_new();
  Object o = rest;
  auto next = [&]() {
    if (rest.type != PAIR) {
      throw_eval_error(form, "invalid arguments");
    }
    o = rest.as_pair()->car;
    rest = rest.as_pair()->cdr;
  };

  next();

  int got = 0;
  for (;;) {
    bool add_normal = true;

    if (o.type == SYMBOL) {
      auto osym = o.as_symbol();
      if (osym->name[0] == ':') {
        add_normal = false;
        std::string name = osym->name.substr(1);
        next();
        if (args.named_args.find(name) != args.named_args.end()) {
          throw_eval_error(rest, "multiply defined keyword argument");
        }
        args.named_args[name] = o;
      }
    }

    if (add_normal) {
      if (got == count) {
        throw_eval_error(rest, "too many arguments");
      }
      args.unnamed_args.push_back(o);
      got++;
    }

    if (rest.type == PAIR) {
      next();
    } else {
      break;
    }
  }
  return args;
}

std::string GoosArgs::print() {
  std::string result;
  for (auto& kv : named_args) {
    result += "[" + kv.first + "] " + kv.second.print() + "\n";
  }
  for (auto& a : unnamed_args) {
    result += a.print() + "\n";
  }
  result += "rest: " + rest.print();
  return result;
}

bool GoosArgs::check_count(int count) {
  return (int)unnamed_args.size() == count;
}

bool GoosArgs::check_keywords(std::unordered_set<std::string>& keywords) {
  for (auto& kv : named_args) {
    if (keywords.find(kv.first) == keywords.end()) {
      return false;
    }
  }

  return true;
}

Object Goos::eval_list_return_last(const Object& form,
                                   Object rest,
                                   std::shared_ptr<EnvironmentObject>& env) {
  Object o = rest;
  Object rv = EmptyListObject::make_new();
  for (;;) {
    if (o.type == PAIR) {
      auto op = o.as_pair();
      rv = eval_with_rewind(op->car, env);
      o = op->cdr;
    } else if (o.type == EMPTY_LIST) {
      return rv;
    } else {
      throw_eval_error(form, "malformed body to evaluate");
    }
  }
}

Object Goos::eval_pair(Object obj, std::shared_ptr<EnvironmentObject> env) {
  auto pair = obj.as_pair();
  Object head = pair->car;
  Object rest = pair->cdr;

  // first see if we got a symbol:
  if (head.type == SYMBOL) {
    auto head_sym = head.as_symbol();

    // try a special form first
    auto kv_sf = special_forms.find(head_sym->name);
    if (kv_sf != special_forms.end()) {
      return ((*this).*(kv_sf->second))(obj, rest, env);
    }

    // try builtins next
    auto kv_b = builtins.find(head_sym->name);
    if (kv_b != builtins.end()) {
      // eval:
      GoosArgs args = eval_args(obj, rest, env);
      return ((*this).*(kv_b->second))(obj, args, env);
    }

    // try macros next
    Object macro_obj;
    bool got_macro = false;
    try {
      macro_obj = eval_symbol(head, env);
      if (macro_obj.type == MACRO) {
        got_macro = true;
      }
    } catch (std::runtime_error& e) {
      got_macro = false;
    }

    if (got_macro) {
      GoosArgs args = get_macro_args(obj, rest);
      // todo define keywords here
      auto macro = macro_obj.as_macro();

      auto mac_env_obj = EnvironmentObject::make_new();
      auto mac_env = mac_env_obj.as_env();

      mac_env->parent_env = env;  // todo, is this how macros work?
      if (!macro->has_rest && args.unnamed_args.size() != macro->unnamed_args.size()) {
        throw_eval_error(obj, "macro didn't get expected argument count");
      } else if (macro->has_rest && args.unnamed_args.size() < macro->unnamed_args.size()) {
        throw_eval_error(obj, "macro (with rest args) didn't get enough arguments");
      }

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

      return eval_with_rewind(eval_list_return_last(macro->body, macro->body, mac_env), env);
    }
  }

  // eval the head and try it as a lambda
  Object eval_head = eval_with_rewind(head, env);
  if (eval_head.type != LAMBDA) {
    throw_eval_error(obj, "head of form didn't evaluate to lambda");
  }

  GoosArgs args = eval_args(obj, rest, env);
  // todo define keywords here
  auto lam = eval_head.as_lambda();

  auto lam_env_obj = EnvironmentObject::make_new();
  auto lam_env = lam_env_obj.as_env();

  lam_env->parent_env = lam->parent_env;
  if (!lam->has_rest && args.unnamed_args.size() != lam->unnamed_args.size()) {
    throw_eval_error(obj, "lambda didn't get expected argument count");
  } else if (lam->has_rest && args.unnamed_args.size() < lam->unnamed_args.size()) {
    throw_eval_error(obj, "lambda (with rest args) didn't get enough arguments");
  }

  uint32_t i = 0;
  for (; i < lam->unnamed_args.size(); i++) {
    lam_env->vars[lam->unnamed_args.at(i).as_symbol()] = args.unnamed_args.at(i);
  }

  if (lam->has_rest) {
    if (i < args.unnamed_args.size()) {
      Object empty = EmptyListObject::make_new();
      Object rest_head = PairObject::make_new(args.unnamed_args[i], empty);
      Object last = rest_head;
      i++;
      for (; i < args.unnamed_args.size(); i++) {
        last.as_pair()->cdr = PairObject::make_new(args.unnamed_args[i], empty);
        last = last.as_pair()->cdr;
      }
      lam_env->vars[lam->rest_args.as_symbol()] = rest_head;
    } else {
      lam_env->vars[lam->rest_args.as_symbol()] = EmptyListObject::make_new();
    }
  }

  return eval_list_return_last(lam->body, lam->body, lam_env);

  throw_eval_error(obj, "don't know how to evaluate this form");
  return EmptyListObject::make_new();
}

Object Goos::eval_define(const Object& form, Object rest, std::shared_ptr<EnvironmentObject>& env) {
  //  if(rest.type == PAIR) {
  //    Object to_define = rest.as_pair()->car;
  //    if(to_define.type != SYMBOL) {
  //      throw_eval_error(form, "define's first argument must be a symbol");
  //    }
  //    Object rrest = rest.as_pair()->cdr;
  //    if(rrest.type != PAIR) {
  //      throw_eval_error(form, "define must be given two arguments, you have not provided
  //      enough");
  //    }
  //    Object to_eval = rrest.as_pair()->car;
  //    if(rrest.as_pair()->cdr.type != EMPTY_LIST) {
  //      throw_eval_error(form, "define must be given two arguments, you have provided too many");
  //    }
  //    Object to_set = eval_with_rewind(to_eval, env);
  //    env->vars[to_define.as_symbol()] = to_set;
  //    return to_set;
  //  } else {
  //    throw_eval_error(form, "define must be given two arguments, you have not provided enough");
  //  }
  //
  //  return EmptyListObject::make_new();

  auto next = [&]() {
    if (rest.type != PAIR) {
      throw_eval_error(form, "invalid arguments");
    }
    auto rv = rest.as_pair()->car;
    rest = rest.as_pair()->cdr;
    return rv;
  };

  auto done = [&]() {
    if (rest.type != EMPTY_LIST) {
      throw_eval_error(form, "too many arguments");
    }
  };

  Object first_arg = next();
  if (first_arg.type != SYMBOL) {
    throw_eval_error(form, "define's first argument must be a symbol");
  }

  if (first_arg.as_symbol()->name == ":env") {
    // special
    Object define_env = eval_with_rewind(next(), env);
    if (define_env.type != ENVIRONMENT) {
      throw_eval_error(form, "define :env must give an environment");
    }
    Object sym_name = next();
    if (sym_name.type != SYMBOL) {
      throw_eval_error(form, "define's symbol argument must be a symbol");
    }
    Object to_set = eval_with_rewind(next(), env);
    define_env.as_env()->vars[sym_name.as_symbol()] = to_set;
    done();
    return to_set;
  } else {
    Object to_set = eval_with_rewind(next(), env);
    env->vars[first_arg.as_symbol()] = to_set;
    done();
    return to_set;
  }
}

Object Goos::eval_set(const Object& form, Object rest, std::shared_ptr<EnvironmentObject>& env) {
  if (rest.type == PAIR) {
    Object to_define = rest.as_pair()->car;
    if (to_define.type != SYMBOL) {
      throw_eval_error(form, "set!'s first argument must be a symbol");
    }
    Object rrest = rest.as_pair()->cdr;
    if (rrest.type != PAIR) {
      throw_eval_error(form, "set! must be given two arguments, you have not provided enough");
    }
    Object to_eval = rrest.as_pair()->car;
    if (rrest.as_pair()->cdr.type != EMPTY_LIST) {
      throw_eval_error(form, "set! must be given two arguments, you have provided too many");
    }
    Object to_set = eval_with_rewind(to_eval, env);

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
    return to_set;
  } else {
    throw_eval_error(form, "set! must be given two arguments, you have not provided enough");
  }

  return EmptyListObject::make_new();
}

// lambda special form.
Object Goos::eval_lambda(const Object& form, Object rest, std::shared_ptr<EnvironmentObject>& env) {
  if (rest.type != PAIR) {
    throw_eval_error(form, "lambda must receive two arguments");
  }

  Object arg_list = rest.as_pair()->car;
  if (arg_list.type != PAIR && arg_list.type != EMPTY_LIST) {
    throw_eval_error(form, "lambda argument list must be a list");
  }

  Object new_lambda = LambdaObject::make_new();
  auto l = new_lambda.as_lambda();

  Object arg_iter = arg_list;
  for (;;) {
    if (arg_iter.type == EMPTY_LIST) {
      break;
    } else if (arg_iter.type == PAIR) {
      // todo handle keyword arguments here
      Object arg_name = arg_iter.as_pair()->car;
      arg_iter = arg_iter.as_pair()->cdr;
      if (arg_name.type != SYMBOL) {
        throw_eval_error(form, "lambda args must be symbols");
      }

      if (arg_name.as_symbol()->name == "&rest") {
        // should have a name
        if (arg_iter.type != PAIR) {
          throw_eval_error(form, "rest argument must have a name");
        }
        if (arg_iter.as_pair()->car.type != SYMBOL) {
          throw_eval_error(form, "rest argument must be a symbol");
        }
        if (arg_iter.as_pair()->cdr.type != EMPTY_LIST) {
          throw_eval_error(form, "no arguments can follow a rest argument");
        }
        l->rest_args = arg_iter.as_pair()->car;
        l->has_rest = true;
        break;
      }

      l->unnamed_args.push_back(arg_name);
    } else {
      throw_eval_error(form, "lambda has invalid arg list");
    }
  }

  Object rrest = rest.as_pair()->cdr;
  if (rrest.type != PAIR) {
    throw_eval_error(form, "lamba body must be a list");
  }

  l->body = rrest;
  l->parent_env = env;

  return new_lambda;
}

Object Goos::eval_macro(const Object& form, Object rest, std::shared_ptr<EnvironmentObject>& env) {
  if (rest.type != PAIR) {
    throw_eval_error(form, "macro must receive two arguments");
  }

  Object arg_list = rest.as_pair()->car;
  if (arg_list.type != PAIR && arg_list.type != EMPTY_LIST) {
    throw_eval_error(form, "macro argument list must be a list");
  }

  Object new_macro = MacroObject::make_new();
  auto m = new_macro.as_macro();

  Object arg_iter = arg_list;
  for (;;) {
    if (arg_iter.type == EMPTY_LIST) {
      break;
    } else if (arg_iter.type == PAIR) {
      // todo handle keyword arguments here
      Object arg_name = arg_iter.as_pair()->car;
      arg_iter = arg_iter.as_pair()->cdr;
      if (arg_name.type != SYMBOL) {
        throw_eval_error(form, "macro args must be symbols");
      }

      if (arg_name.as_symbol()->name == "&rest") {
        // should have a name
        if (arg_iter.type != PAIR) {
          throw_eval_error(form, "rest argument must have a name");
        }
        if (arg_iter.as_pair()->car.type != SYMBOL) {
          throw_eval_error(form, "rest argument must be a symbol");
        }
        if (arg_iter.as_pair()->cdr.type != EMPTY_LIST) {
          throw_eval_error(form, "no arguments can follow a rest argument");
        }
        m->rest_args = arg_iter.as_pair()->car;
        m->has_rest = true;
        break;
      }

      m->unnamed_args.push_back(arg_name);
    } else {
      throw_eval_error(form, "macro has invalid arg list");
    }
  }

  Object rrest = rest.as_pair()->cdr;
  if (rrest.type != PAIR) {
    throw_eval_error(form, "macro body must be a list");
  }

  m->body = rrest;
  m->parent_env = env;

  return new_macro;
}

Object Goos::eval_quote(const Object& form, Object rest, std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  if (rest.type != PAIR) {
    throw_eval_error(form, "quote must be given exactly one argument");
  }

  Object result = rest.as_pair()->car;
  Object rrest = rest.as_pair()->cdr;
  if (rrest.type != EMPTY_LIST) {
    throw_eval_error(form, "quote must be given exactly one argument, you have given more");
  }

  return result;
}

bool Goos::get_object_by_name(const std::string& name, Object& dest) {
  auto kv = global_environment.as_env()->vars.find(
      SymbolObject::make_new(reader.symbolTable, name).as_symbol());
  if (kv != global_environment.as_env()->vars.end()) {
    dest = kv->second;
    return true;
  }
  return false;
}

static bool truthy(Object o) {
  if (o.type == SYMBOL && o.as_symbol()->name == "#f")
    return false;
  if (o.type == EMPTY_LIST)
    return false;  // debatable if this is ok.
  return true;
}

Object Goos::eval_cond(const Object& form, Object rest, std::shared_ptr<EnvironmentObject>& env) {
  if (rest.type != PAIR)
    throw_eval_error(form, "cond must have at least one clause, which must be a form");
  Object result;

  Object lst = rest;
  for (;;) {
    if (lst.type == PAIR) {
      Object current_case = lst.as_pair()->car;
      if (current_case.type != PAIR)
        throw_eval_error(lst, "bogus cond case");

      // check condition:
      Object condition_result = eval_with_rewind(current_case.as_pair()->car, env);
      if (truthy(condition_result)) {
        if (current_case.as_pair()->cdr.type == EMPTY_LIST) {
          return condition_result;
        }
        // got a match!
        return eval_list_return_last(current_case, current_case.as_pair()->cdr, env);
      } else {
        // no match, continue.
        lst = lst.as_pair()->cdr;
      }
    } else if (lst.type == EMPTY_LIST) {
      return SymbolObject::make_new(reader.symbolTable, "#f");
    } else {
      throw_eval_error(form, "malformed cond");
    }
  }
}

Object Goos::eval_or(const Object& form, Object rest, std::shared_ptr<EnvironmentObject>& env) {
  if (rest.type != PAIR)
    throw_eval_error(form, "or must have at least one argument!");

  Object lst = rest;
  for (;;) {
    if (lst.type == PAIR) {
      Object current = eval_with_rewind(lst.as_pair()->car, env);
      if (truthy(current)) {
        return current;
      }
      lst = lst.as_pair()->cdr;
    } else if (lst.type == EMPTY_LIST) {
      return SymbolObject::make_new(reader.symbolTable, "#f");
    } else {
      throw_eval_error(form, "invalid or form");
    }
  }
}

Object Goos::eval_and(const Object& form, Object rest, std::shared_ptr<EnvironmentObject>& env) {
  if (rest.type != PAIR)
    throw_eval_error(form, "and must have at least one argument!");

  Object lst = rest;
  Object current;
  for (;;) {
    if (lst.type == PAIR) {
      current = eval_with_rewind(lst.as_pair()->car, env);
      if (!truthy(current)) {
        return SymbolObject::make_new(reader.symbolTable, "#f");
      }
      lst = lst.as_pair()->cdr;
    } else if (lst.type == EMPTY_LIST) {
      return current;
    } else {
      throw_eval_error(form, "invalid and form");
    }
  }
}

Object Goos::quasiquote_helper(Object form, std::shared_ptr<EnvironmentObject>& env) {
  Object lst = form;
  std::vector<Object> result;
  for (;;) {
    if (lst.type == PAIR) {
      Object item = lst.as_pair()->car;
      if (item.type == PAIR) {
        if (item.as_pair()->car.type == SYMBOL &&
            item.as_pair()->car.as_symbol()->name == "unquote") {
          Object unquote_arg = item.as_pair()->cdr;
          if (unquote_arg.type != PAIR || unquote_arg.as_pair()->cdr.type != EMPTY_LIST) {
            throw_eval_error(form, "unquote must have exactly 1 arg");
          }
          item = eval_with_rewind(unquote_arg.as_pair()->car, env);
        } else if (item.as_pair()->car.type == SYMBOL &&
                   item.as_pair()->car.as_symbol()->name == "unquote-splicing") {
          Object unquote_arg = item.as_pair()->cdr;
          if (unquote_arg.type != PAIR || unquote_arg.as_pair()->cdr.type != EMPTY_LIST) {
            throw_eval_error(form, "unquote must have exactly 1 arg");
          }
          item = eval_with_rewind(unquote_arg.as_pair()->car, env);

          // bypass normal addition:
          lst = lst.as_pair()->cdr;
          Object to_add = item;
          for (;;) {
            if (to_add.type == PAIR) {
              result.push_back(to_add.as_pair()->car);
              to_add = to_add.as_pair()->cdr;
            } else if (to_add.type == EMPTY_LIST) {
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
    } else if (lst.type == EMPTY_LIST) {
      return build_list(result);
    } else {
      throw_eval_error(form, "malformed quasiquote");
    }
  }
}

Object Goos::eval_quasiquote(const Object& form,
                             Object rest,
                             std::shared_ptr<EnvironmentObject>& env) {
  if (rest.type != PAIR || rest.as_pair()->cdr.type != EMPTY_LIST)
    throw_eval_error(form, "quasiquote must have one argument!");
  return quasiquote_helper(rest.as_pair()->car, env);
}

Object Goos::eval_while(const Object& form, Object rest, std::shared_ptr<EnvironmentObject>& env) {
  if (rest.type != PAIR)
    throw_eval_error(form, "while must have condition and body");
  Object condition = rest.as_pair()->car;
  Object body = rest.as_pair()->cdr;
  if (body.type != PAIR)
    throw_eval_error(form, "while must have condition and body");

  Object rv = SymbolObject::make_new(reader.symbolTable, "#f");
  while (truthy(eval_with_rewind(condition, env))) {
    rv = eval_list_return_last(form, body, env);
  }

  return rv;
}