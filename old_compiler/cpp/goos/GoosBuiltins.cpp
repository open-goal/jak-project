#include "goos/Goos.h"

static int64_t gensym_id = 0;

Object Goos::eval_exit(const Object& form,
                       GoosArgs& args,
                       std::shared_ptr<EnvironmentObject>& env) {
  (void)form;
  (void)args;
  (void)env;
  want_exit = true;
  return EmptyListObject::make_new();
}

Object Goos::eval_begin(const Object& form,
                        GoosArgs& args,
                        std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  if (!args.named_args.empty()) {
    throw_eval_error(form, "begin form cannot have keyword arguments");
  }

  if (args.unnamed_args.empty()) {
    return EmptyListObject::make_new();
  } else {
    return args.unnamed_args.back();
  }
}

Object Goos::eval_read(const Object& form,
                       GoosArgs& args,
                       std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  if (args.unnamed_args.size() != 1 || !args.named_args.empty() ||
      args.unnamed_args.at(0).type != STRING) {
    throw_eval_error(form, "read must be given a single string argument");
  }

  try {
    return reader.read_from_string(args.unnamed_args.at(0).as_string()->data);
  } catch (std::runtime_error& e) {
    throw_eval_error(form, std::string("reader error inside of read:\n") + e.what());
  }

  return EmptyListObject::make_new();
}

Object Goos::eval_read_file(const Object& form,
                            GoosArgs& args,
                            std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  if (args.unnamed_args.size() != 1 || !args.named_args.empty() ||
      args.unnamed_args.at(0).type != STRING) {
    throw_eval_error(form, "read-file must be given a single string argument");
  }

  try {
    return reader.read_from_file(args.unnamed_args.at(0).as_string()->data);
  } catch (std::runtime_error& e) {
    throw_eval_error(form, std::string("reader error inside of read-file:\n") + e.what());
  }
  return EmptyListObject::make_new();
}

Object Goos::eval_load_file(const Object& form,
                            GoosArgs& args,
                            std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  if (args.unnamed_args.size() != 1 || !args.named_args.empty() ||
      args.unnamed_args.at(0).type != STRING) {
    throw_eval_error(form, "read-file must be given a single string argument");
  }

  Object o;
  try {
    o = reader.read_from_file(args.unnamed_args.at(0).as_string()->data);
  } catch (std::runtime_error& e) {
    throw_eval_error(form, std::string("reader error inside of load-file:\n") + e.what());
  }

  try {
    return eval_with_rewind(o, global_environment.as_env());
  } catch (std::runtime_error& e) {
    throw_eval_error(form, std::string("eval error inside of load-file:\n") + e.what());
  }
  return EmptyListObject::make_new();
}

Object Goos::eval_print(const Object& form,
                        GoosArgs& args,
                        std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  if (args.unnamed_args.size() != 1 || !args.named_args.empty()) {
    throw_eval_error(form, "print must be given a single argument");
  }

  printf("%s\n", args.unnamed_args.at(0).print().c_str());
  return EmptyListObject::make_new();
}

Object Goos::eval_inspect(const Object& form,
                          GoosArgs& args,
                          std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  if (args.unnamed_args.size() != 1 || !args.named_args.empty()) {
    throw_eval_error(form, "inspect must be given a single argument");
  }

  printf("%s\n", args.unnamed_args.at(0).inspect().c_str());
  return EmptyListObject::make_new();
}

Object Goos::eval_equals(const Object& form,
                         GoosArgs& args,
                         std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  if (args.unnamed_args.size() != 2 || !args.named_args.empty()) {
    throw_eval_error(form, "eq? must be given two unnamed arguments");
  }

  return SymbolObject::make_new(reader.symbolTable,
                                args.unnamed_args[0] == args.unnamed_args[1] ? "#t" : "#f");
}

int64_t Goos::number_to_integer(const Object& obj) {
  switch (obj.type) {
    case INTEGER:
      return obj.integer_obj.value;
    case FLOAT:
      return (int64_t)obj.float_obj.value;
    case CHAR:
      return obj.char_obj.value;
    default:
      throw_eval_error(obj, "object cannot be interpreted as a number!");
  }
  return 0;
}

double Goos::number_to_float(const Object& obj) {
  switch (obj.type) {
    case INTEGER:
      return obj.integer_obj.value;
    case FLOAT:
      return obj.float_obj.value;
    case CHAR:
      return obj.char_obj.value;
    default:
      throw_eval_error(obj, "object cannot be interpreted as a number!");
  }
  return 0;
}

char Goos::number_to_char(const Object& obj) {
  switch (obj.type) {
    case INTEGER:
      return obj.integer_obj.value;
    case FLOAT:
      return obj.float_obj.value;
    case CHAR:
      return obj.char_obj.value;
    default:
      throw_eval_error(obj, "object cannot be interpreted as a number!");
  }
  return 0;
}

template <>
double Goos::number(const Object& obj) {
  return number_to_float(obj);
}

template <>
int64_t Goos::number(const Object& obj) {
  return number_to_integer(obj);
}

template <>
char Goos::number(const Object& obj) {
  return number_to_char(obj);
}

template <typename T>
Object Goos::num_plus(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  (void)form;
  T result = 0;
  for (const auto& arg : args.unnamed_args) {
    result += number<T>(arg);
  }
  return Object::make_number<T>(result);
}

template <typename T>
Object Goos::num_times(const Object& form,
                       GoosArgs& args,
                       std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  (void)form;
  T result = 1;
  for (const auto& arg : args.unnamed_args) {
    result *= number<T>(arg);
  }
  return Object::make_number<T>(result);
}

template <typename T>
Object Goos::num_minus(const Object& form,
                       GoosArgs& args,
                       std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  (void)form;
  T result = 0;
  if (args.unnamed_args.size() > 1) {
    result = number<T>(args.unnamed_args[0]);
    for (uint32_t i = 1; i < args.unnamed_args.size(); i++) {
      result -= number<T>(args.unnamed_args[i]);
    }
  } else {
    result = -number<T>(args.unnamed_args[0]);
  }
  return Object::make_number<T>(result);
}

Object Goos::eval_plus(const Object& form,
                       GoosArgs& args,
                       std::shared_ptr<EnvironmentObject>& env) {
  if (!args.named_args.empty() || args.unnamed_args.empty())
    throw_eval_error(form, "+ must receive at least one unnamed argument!");
  switch (args.unnamed_args.front().type) {
    case INTEGER:
      return num_plus<int64_t>(form, args, env);

    case FLOAT:
      return num_plus<double>(form, args, env);

    case CHAR:
      return num_plus<char>(form, args, env);

    default:
      throw_eval_error(form, "+ must have a numeric argument");
      return EmptyListObject::make_new();
  }
}

Object Goos::eval_times(const Object& form,
                        GoosArgs& args,
                        std::shared_ptr<EnvironmentObject>& env) {
  if (!args.named_args.empty() || args.unnamed_args.empty())
    throw_eval_error(form, "* must receive at least one unnamed argument!");
  switch (args.unnamed_args.front().type) {
    case INTEGER:
      return num_times<int64_t>(form, args, env);

    case FLOAT:
      return num_times<double>(form, args, env);

    case CHAR:
      return num_times<char>(form, args, env);

    default:
      throw_eval_error(form, "* must have a numeric argument");
      return EmptyListObject::make_new();
  }
}

Object Goos::eval_minus(const Object& form,
                        GoosArgs& args,
                        std::shared_ptr<EnvironmentObject>& env) {
  if (!args.named_args.empty() || args.unnamed_args.empty())
    throw_eval_error(form, "- must receive at least one unnamed argument!");
  switch (args.unnamed_args.front().type) {
    case INTEGER:
      return num_minus<int64_t>(form, args, env);

    case FLOAT:
      return num_minus<double>(form, args, env);

    case CHAR:
      return num_minus<char>(form, args, env);

    default:
      throw_eval_error(form, "- must have a numeric argument");
      return EmptyListObject::make_new();
  }
}

Object Goos::eval_numequals(const Object& form,
                            GoosArgs& args,
                            std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  if (!args.named_args.empty() || args.unnamed_args.size() < 2)
    throw_eval_error(form, "= must receive at least two unnamed arguments!");
  bool result = true;
  switch (args.unnamed_args.front().type) {
    case INTEGER: {
      int64_t ref = number_to_integer(args.unnamed_args.front());
      for (uint32_t i = 1; i < args.unnamed_args.size(); i++) {
        if (ref != number_to_integer(args.unnamed_args[i])) {
          result = false;
          break;
        }
      }
    } break;

    case FLOAT: {
      double ref = number_to_float(args.unnamed_args.front());
      for (uint32_t i = 1; i < args.unnamed_args.size(); i++) {
        if (ref != number_to_float(args.unnamed_args[i])) {
          result = false;
          break;
        }
      }
    } break;

    case CHAR: {
      char ref = number_to_char(args.unnamed_args.front());
      for (uint32_t i = 1; i < args.unnamed_args.size(); i++) {
        if (ref != number_to_char(args.unnamed_args[i])) {
          result = false;
          break;
        }
      }
    } break;

    default:
      throw_eval_error(form, "+ must have a numeric argument");
      return EmptyListObject::make_new();
  }

  return SymbolObject::make_new(reader.symbolTable, result ? "#t" : "#f");
}

template <typename T>
Object Goos::num_lt(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env) {
  (void)form;
  (void)env;
  T a = number<T>(args.unnamed_args[0]);
  T b = number<T>(args.unnamed_args[1]);
  return SymbolObject::make_new(reader.symbolTable, (a < b) ? "#t" : "#f");
}

Object Goos::eval_lt(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env) {
  if (!args.named_args.empty() || args.unnamed_args.size() != 2)
    throw_eval_error(form, "< must receive two unnamed arguments!");
  switch (args.unnamed_args.front().type) {
    case INTEGER:
      return num_lt<int64_t>(form, args, env);

    case FLOAT:
      return num_lt<double>(form, args, env);

    case CHAR:
      return num_lt<char>(form, args, env);

    default:
      throw_eval_error(form, "< must have a numeric argument");
      return EmptyListObject::make_new();
  }
}

template <typename T>
Object Goos::num_gt(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env) {
  (void)form;
  (void)env;
  T a = number<T>(args.unnamed_args[0]);
  T b = number<T>(args.unnamed_args[1]);
  return SymbolObject::make_new(reader.symbolTable, (a > b) ? "#t" : "#f");
}

Object Goos::eval_gt(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env) {
  if (!args.named_args.empty() || args.unnamed_args.size() != 2)
    throw_eval_error(form, "> must receive two unnamed arguments!");
  switch (args.unnamed_args.front().type) {
    case INTEGER:
      return num_gt<int64_t>(form, args, env);

    case FLOAT:
      return num_gt<double>(form, args, env);

    case CHAR:
      return num_gt<char>(form, args, env);

    default:
      throw_eval_error(form, "> must have a numeric argument");
      return EmptyListObject::make_new();
  }
}

template <typename T>
Object Goos::num_leq(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env) {
  (void)form;
  (void)env;
  T a = number<T>(args.unnamed_args[0]);
  T b = number<T>(args.unnamed_args[1]);
  return SymbolObject::make_new(reader.symbolTable, (a <= b) ? "#t" : "#f");
}

Object Goos::eval_leq(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env) {
  if (!args.named_args.empty() || args.unnamed_args.size() != 2)
    throw_eval_error(form, "<= must receive two unnamed arguments!");
  switch (args.unnamed_args.front().type) {
    case INTEGER:
      return num_leq<int64_t>(form, args, env);

    case FLOAT:
      return num_leq<double>(form, args, env);

    case CHAR:
      return num_leq<char>(form, args, env);

    default:
      throw_eval_error(form, "<= must have a numeric argument");
      return EmptyListObject::make_new();
  }
}

template <typename T>
Object Goos::num_geq(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env) {
  (void)form;
  (void)env;
  T a = number<T>(args.unnamed_args[0]);
  T b = number<T>(args.unnamed_args[1]);
  return SymbolObject::make_new(reader.symbolTable, (a >= b) ? "#t" : "#f");
}

Object Goos::eval_geq(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env) {
  if (!args.named_args.empty() || args.unnamed_args.size() != 2)
    throw_eval_error(form, ">= must receive two unnamed arguments!");
  switch (args.unnamed_args.front().type) {
    case INTEGER:
      return num_geq<int64_t>(form, args, env);

    case FLOAT:
      return num_geq<double>(form, args, env);

    case CHAR:
      return num_geq<char>(form, args, env);

    default:
      throw_eval_error(form, ">= must have a numeric argument");
      return EmptyListObject::make_new();
  }
}

Object Goos::eval_eval(const Object& form,
                       GoosArgs& args,
                       std::shared_ptr<EnvironmentObject>& env) {
  if (!args.named_args.empty() || args.unnamed_args.size() != 1) {
    throw_eval_error(form, "eval must receive exactly one argument!");
  }
  return eval(args.unnamed_args[0], env);
}

Object Goos::eval_car(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  if (!args.named_args.empty() || args.unnamed_args.size() != 1 ||
      args.unnamed_args[0].type != PAIR)
    throw_eval_error(form, "car must receive a single pair argument");
  return args.unnamed_args[0].as_pair()->car;
}

Object Goos::eval_cdr(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  if (!args.named_args.empty() || args.unnamed_args.size() != 1 ||
      args.unnamed_args[0].type != PAIR)
    throw_eval_error(form, "cdr must receive a single pair argument");
  return args.unnamed_args[0].as_pair()->cdr;
}

Object Goos::eval_gensym(const Object& form,
                         GoosArgs& args,
                         std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  if (!args.named_args.empty())
    throw_eval_error(form, "gensym must take 1 or 0 arguments");
  if (args.unnamed_args.size() == 0) {
    return SymbolObject::make_new(reader.symbolTable, "gensym" + std::to_string(gensym_id++));
  } else if (args.unnamed_args.size() == 1 && args.unnamed_args[0].type == SYMBOL) {
    return SymbolObject::make_new(
        reader.symbolTable,
        "gs-" + args.unnamed_args[0].as_symbol()->name + std::to_string(gensym_id++));
  } else {
    throw_eval_error(form, "gensym error");
  }
  return EmptyListObject::make_new();
}

Object Goos::eval_cons(const Object& form,
                       GoosArgs& args,
                       std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  if (!args.named_args.empty() || args.unnamed_args.size() != 2)
    throw_eval_error(form, "cons must receive two unnamed arguments!");
  return PairObject::make_new(args.unnamed_args[0], args.unnamed_args[1]);
}

Object Goos::eval_null(const Object& form,
                       GoosArgs& args,
                       std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  if (!args.named_args.empty() || args.unnamed_args.size() != 1)
    throw_eval_error(form, "null? must receive one unnamed argument!");
  return SymbolObject::make_new(reader.symbolTable,
                                args.unnamed_args[0].type == EMPTY_LIST ? "#t" : "#f");
}

Object Goos::eval_type(const Object& form,
                       GoosArgs& args,
                       std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  if (!args.named_args.empty() || args.unnamed_args.size() != 2)
    throw_eval_error(form, "type? must receive two unnamed arguments!");

  auto type = args.unnamed_args.at(0);
  auto val = args.unnamed_args.at(1);

  if (type.type != SYMBOL) {
    throw_eval_error(form, "invalid type name");
  }

  auto tas = type.as_symbol();

  bool matches = true;

  if (tas->name == "string") {
    matches = (val.type == STRING);
  } else if (tas->name == "symbol") {
    matches = (val.type == SYMBOL);
  } else {
    throw_eval_error(form, "invalid type to type?");
  }

  if (matches) {
    return SymbolObject::make_new(reader.symbolTable, "#t");
  } else {
    return SymbolObject::make_new(reader.symbolTable, "#f");
  }
}

Object Goos::eval_current_method_type(const Object& form,
                                      GoosArgs& args,
                                      std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  if (!args.unnamed_args.empty() || !args.named_args.empty()) {
    throw_eval_error(form, "current-method-type accepts no arguments");
  }

  return SymbolObject::make_new(reader.symbolTable, goal_to_goos.enclosing_method_type);
}