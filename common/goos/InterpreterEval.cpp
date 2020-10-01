/*!
 * @file InterpreterEval.cpp
 * Implementation of built-in GOOS functions.
 */

#include <third-party/fmt/format.h>
#include "Interpreter.h"

namespace goos {

/*!
 * Exit GOOS. Accepts and ignores all arguments;
 */
Object Interpreter::eval_exit(const Object& form,
                              Arguments& args,
                              const std::shared_ptr<EnvironmentObject>& env) {
  (void)form;
  (void)args;
  (void)env;
  want_exit = true;
  return EmptyListObject::make_new();
}

/*!
 * Begin form
 */
Object Interpreter::eval_begin(const Object& form,
                               Arguments& args,
                               const std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  if (!args.named.empty()) {
    throw_eval_error(form, "begin form cannot have keyword arguments");
  }

  if (args.unnamed.empty()) {
    return EmptyListObject::make_new();
  } else {
    return args.unnamed.back();
  }
}

/*!
 * Read form, which runs the Reader on a string.
 */
Object Interpreter::eval_read(const Object& form,
                              Arguments& args,
                              const std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  vararg_check(form, args, {ObjectType::STRING}, {});

  try {
    return reader.read_from_string(args.unnamed.at(0).as_string()->data);
  } catch (std::runtime_error& e) {
    throw_eval_error(form, std::string("reader error inside of read:\n") + e.what());
  }

  return EmptyListObject::make_new();
}

/*!
 * Open and run the Reader on a text file.
 */
Object Interpreter::eval_read_file(const Object& form,
                                   Arguments& args,
                                   const std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  vararg_check(form, args, {ObjectType::STRING}, {});

  try {
    return reader.read_from_file({args.unnamed.at(0).as_string()->data});
  } catch (std::runtime_error& e) {
    throw_eval_error(form, std::string("reader error inside of read-file:\n") + e.what());
  }
  return EmptyListObject::make_new();
}

/*!
 * Combines read-file and eval to load in a file.
 */
Object Interpreter::eval_load_file(const Object& form,
                                   Arguments& args,
                                   const std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  vararg_check(form, args, {ObjectType::STRING}, {});

  Object o;
  try {
    o = reader.read_from_file({args.unnamed.at(0).as_string()->data});
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

/*!
 * Print the form to stdout, including a newline.
 * Returns ()
 */
Object Interpreter::eval_print(const Object& form,
                               Arguments& args,
                               const std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  vararg_check(form, args, {{}}, {});

  if (!disable_printing) {
    printf("%s\n", args.unnamed.at(0).print().c_str());
  }
  return EmptyListObject::make_new();
}

/*!
 * Print the inspection of a form to stdout, including a newline.
 * Returns ()
 */
Object Interpreter::eval_inspect(const Object& form,
                                 Arguments& args,
                                 const std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  vararg_check(form, args, {{}}, {});

  if (!disable_printing) {
    printf("%s\n", args.unnamed.at(0).inspect().c_str());
  }

  return EmptyListObject::make_new();
}

/*!
 * Fancy equality check (using Object::operator==)
 */
Object Interpreter::eval_equals(const Object& form,
                                Arguments& args,
                                const std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  vararg_check(form, args, {{}, {}}, {});
  return SymbolObject::make_new(reader.symbolTable,
                                args.unnamed[0] == args.unnamed[1] ? "#t" : "#f");
}

/*!
 * Convert a number to an integer
 */
IntType Interpreter::number_to_integer(const Object& obj) {
  switch (obj.type) {
    case ObjectType::INTEGER:
      return obj.integer_obj.value;
    case ObjectType::FLOAT:
      return (int64_t)obj.float_obj.value;
    default:
      throw_eval_error(obj, "object cannot be interpreted as a number!");
  }
  return 0;
}

/*!
 * Convert a number to floating point
 */
FloatType Interpreter::number_to_float(const Object& obj) {
  switch (obj.type) {
    case ObjectType::INTEGER:
      return obj.integer_obj.value;
    case ObjectType::FLOAT:
      return obj.float_obj.value;
    default:
      throw_eval_error(obj, "object cannot be interpreted as a number!");
  }
  return 0;
}

/*!
 * Convert number to template type.
 */
template <>
FloatType Interpreter::number(const Object& obj) {
  return number_to_float(obj);
}

/*!
 * Convert number to template type.
 */
template <>
IntType Interpreter::number(const Object& obj) {
  return number_to_integer(obj);
}

/*!
 * Template implementation of addition.
 */
template <typename T>
Object Interpreter::num_plus(const Object& form,
                             Arguments& args,
                             const std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  (void)form;
  T result = 0;
  for (const auto& arg : args.unnamed) {
    result += number<T>(arg);
  }
  return Object::make_number<T>(result);
}

/*!
 * Addition
 */
Object Interpreter::eval_plus(const Object& form,
                              Arguments& args,
                              const std::shared_ptr<EnvironmentObject>& env) {
  if (!args.named.empty() || args.unnamed.empty()) {
    throw_eval_error(form, "+ must receive at least one unnamed argument!");
  }

  switch (args.unnamed.front().type) {
    case ObjectType::INTEGER:
      return num_plus<int64_t>(form, args, env);

    case ObjectType::FLOAT:
      return num_plus<double>(form, args, env);

    default:
      throw_eval_error(form, "+ must have a numeric argument");
      return EmptyListObject::make_new();
  }
}

/*!
 * Template implementation of multiplication.
 */
template <typename T>
Object Interpreter::num_times(const Object& form,
                              Arguments& args,
                              const std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  (void)form;
  T result = 1;
  for (const auto& arg : args.unnamed) {
    result *= number<T>(arg);
  }
  return Object::make_number<T>(result);
}

/*!
 * Multiplication
 */
Object Interpreter::eval_times(const Object& form,
                               Arguments& args,
                               const std::shared_ptr<EnvironmentObject>& env) {
  if (!args.named.empty() || args.unnamed.empty()) {
    throw_eval_error(form, "* must receive at least one unnamed argument!");
  }

  switch (args.unnamed.front().type) {
    case ObjectType::INTEGER:
      return num_times<int64_t>(form, args, env);

    case ObjectType::FLOAT:
      return num_times<double>(form, args, env);

    default:
      throw_eval_error(form, "* must have a numeric argument");
      return EmptyListObject::make_new();
  }
}

/*!
 * Template implementation of subtraction.
 */
template <typename T>
Object Interpreter::num_minus(const Object& form,
                              Arguments& args,
                              const std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  (void)form;
  T result;
  if (args.unnamed.size() > 1) {
    result = number<T>(args.unnamed[0]);
    for (uint32_t i = 1; i < args.unnamed.size(); i++) {
      result -= number<T>(args.unnamed[i]);
    }
  } else {
    result = -number<T>(args.unnamed[0]);
  }
  return Object::make_number<T>(result);
}

/*!
 * Subtraction
 */
Object Interpreter::eval_minus(const Object& form,
                               Arguments& args,
                               const std::shared_ptr<EnvironmentObject>& env) {
  if (!args.named.empty() || args.unnamed.empty()) {
    throw_eval_error(form, "- must receive at least one unnamed argument!");
  }

  switch (args.unnamed.front().type) {
    case ObjectType::INTEGER:
      return num_minus<int64_t>(form, args, env);

    case ObjectType::FLOAT:
      return num_minus<double>(form, args, env);

    default:
      throw_eval_error(form, "- must have a numeric argument");
      return EmptyListObject::make_new();
  }
}

/*!
 * Template implementation of division.
 */
template <typename T>
Object Interpreter::num_divide(const Object& form,
                               Arguments& args,
                               const std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  (void)form;
  T result = number<T>(args.unnamed[0]) / number<T>(args.unnamed[1]);
  return Object::make_number<T>(result);
}

/*!
 * Division
 */
Object Interpreter::eval_divide(const Object& form,
                                Arguments& args,
                                const std::shared_ptr<EnvironmentObject>& env) {
  vararg_check(form, args, {{}, {}}, {});
  switch (args.unnamed.front().type) {
    case ObjectType::INTEGER:
      return num_divide<int64_t>(form, args, env);

    case ObjectType::FLOAT:
      return num_divide<double>(form, args, env);

    default:
      throw_eval_error(form, "/ must have a numeric argument");
      return EmptyListObject::make_new();
  }
}

/*!
 * Compare numbers for equality
 */
Object Interpreter::eval_numequals(const Object& form,
                                   Arguments& args,
                                   const std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  if (!args.named.empty() || args.unnamed.size() < 2) {
    throw_eval_error(form, "= must receive at least two unnamed arguments!");
  }

  bool result = true;
  switch (args.unnamed.front().type) {
    case ObjectType::INTEGER: {
      int64_t ref = number_to_integer(args.unnamed.front());
      for (uint32_t i = 1; i < args.unnamed.size(); i++) {
        if (ref != number_to_integer(args.unnamed[i])) {
          result = false;
          break;
        }
      }
    } break;

    case ObjectType::FLOAT: {
      double ref = number_to_float(args.unnamed.front());
      for (uint32_t i = 1; i < args.unnamed.size(); i++) {
        if (ref != number_to_float(args.unnamed[i])) {
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
Object Interpreter::num_lt(const Object& form,
                           Arguments& args,
                           const std::shared_ptr<EnvironmentObject>& env) {
  (void)form;
  (void)env;
  T a = number<T>(args.unnamed[0]);
  T b = number<T>(args.unnamed[1]);
  return SymbolObject::make_new(reader.symbolTable, (a < b) ? "#t" : "#f");
}

Object Interpreter::eval_lt(const Object& form,
                            Arguments& args,
                            const std::shared_ptr<EnvironmentObject>& env) {
  vararg_check(form, args, {{}, {}}, {});
  switch (args.unnamed.front().type) {
    case ObjectType::INTEGER:
      return num_lt<int64_t>(form, args, env);

    case ObjectType::FLOAT:
      return num_lt<double>(form, args, env);

    default:
      throw_eval_error(form, "< must have a numeric argument");
      return EmptyListObject::make_new();
  }
}

template <typename T>
Object Interpreter::num_gt(const Object& form,
                           Arguments& args,
                           const std::shared_ptr<EnvironmentObject>& env) {
  (void)form;
  (void)env;
  T a = number<T>(args.unnamed[0]);
  T b = number<T>(args.unnamed[1]);
  return SymbolObject::make_new(reader.symbolTable, (a > b) ? "#t" : "#f");
}

Object Interpreter::eval_gt(const Object& form,
                            Arguments& args,
                            const std::shared_ptr<EnvironmentObject>& env) {
  vararg_check(form, args, {{}, {}}, {});
  switch (args.unnamed.front().type) {
    case ObjectType::INTEGER:
      return num_gt<int64_t>(form, args, env);

    case ObjectType::FLOAT:
      return num_gt<double>(form, args, env);

    default:
      throw_eval_error(form, "> must have a numeric argument");
      return EmptyListObject::make_new();
  }
}

template <typename T>
Object Interpreter::num_leq(const Object& form,
                            Arguments& args,
                            const std::shared_ptr<EnvironmentObject>& env) {
  (void)form;
  (void)env;
  T a = number<T>(args.unnamed[0]);
  T b = number<T>(args.unnamed[1]);
  return SymbolObject::make_new(reader.symbolTable, (a <= b) ? "#t" : "#f");
}

Object Interpreter::eval_leq(const Object& form,
                             Arguments& args,
                             const std::shared_ptr<EnvironmentObject>& env) {
  vararg_check(form, args, {{}, {}}, {});
  switch (args.unnamed.front().type) {
    case ObjectType::INTEGER:
      return num_leq<int64_t>(form, args, env);

    case ObjectType::FLOAT:
      return num_leq<double>(form, args, env);

    default:
      throw_eval_error(form, "<= must have a numeric argument");
      return EmptyListObject::make_new();
  }
}

template <typename T>
Object Interpreter::num_geq(const Object& form,
                            Arguments& args,
                            const std::shared_ptr<EnvironmentObject>& env) {
  (void)form;
  (void)env;
  T a = number<T>(args.unnamed[0]);
  T b = number<T>(args.unnamed[1]);
  return SymbolObject::make_new(reader.symbolTable, (a >= b) ? "#t" : "#f");
}

Object Interpreter::eval_geq(const Object& form,
                             Arguments& args,
                             const std::shared_ptr<EnvironmentObject>& env) {
  vararg_check(form, args, {{}, {}}, {});
  switch (args.unnamed.front().type) {
    case ObjectType::INTEGER:
      return num_geq<int64_t>(form, args, env);

    case ObjectType::FLOAT:
      return num_geq<double>(form, args, env);

    default:
      throw_eval_error(form, ">= must have a numeric argument");
      return EmptyListObject::make_new();
  }
}

Object Interpreter::eval_eval(const Object& form,
                              Arguments& args,
                              const std::shared_ptr<EnvironmentObject>& env) {
  vararg_check(form, args, {{}}, {});
  return eval(args.unnamed[0], env);
}

Object Interpreter::eval_car(const Object& form,
                             Arguments& args,
                             const std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  vararg_check(form, args, {ObjectType::PAIR}, {});
  return args.unnamed[0].as_pair()->car;
}

Object Interpreter::eval_set_car(const Object& form,
                                 Arguments& args,
                                 const std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  vararg_check(form, args, {ObjectType::PAIR, {}}, {});
  args.unnamed[0].as_pair()->car = args.unnamed[1];
  return args.unnamed[0];
}

Object Interpreter::eval_set_cdr(const Object& form,
                                 Arguments& args,
                                 const std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  vararg_check(form, args, {ObjectType::PAIR, {}}, {});
  args.unnamed[0].as_pair()->cdr = args.unnamed[1];
  return args.unnamed[0];
}

Object Interpreter::eval_cdr(const Object& form,
                             Arguments& args,
                             const std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  vararg_check(form, args, {ObjectType::PAIR}, {});
  return args.unnamed[0].as_pair()->cdr;
}

Object Interpreter::eval_gensym(const Object& form,
                                Arguments& args,
                                const std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  vararg_check(form, args, {}, {});
  return SymbolObject::make_new(reader.symbolTable, "gensym" + std::to_string(gensym_id++));
}

Object Interpreter::eval_cons(const Object& form,
                              Arguments& args,
                              const std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  vararg_check(form, args, {{}, {}}, {});
  return PairObject::make_new(args.unnamed[0], args.unnamed[1]);
}

Object Interpreter::eval_null(const Object& form,
                              Arguments& args,
                              const std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  vararg_check(form, args, {{}}, {});
  return SymbolObject::make_new(reader.symbolTable, args.unnamed[0].is_empty_list() ? "#t" : "#f");
}

Object Interpreter::eval_type(const Object& form,
                              Arguments& args,
                              const std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  vararg_check(form, args, {{ObjectType::SYMBOL}, {}}, {});

  auto kv = string_to_type.find(args.unnamed[0].as_symbol()->name);
  if (kv == string_to_type.end()) {
    throw_eval_error(form, "invalid type given to type?");
  }

  if (args.unnamed[1].type == kv->second) {
    return SymbolObject::make_new(reader.symbolTable, "#t");
  } else {
    return SymbolObject::make_new(reader.symbolTable, "#f");
  }
}

Object Interpreter::eval_current_method_type(const Object& form,
                                             Arguments& args,
                                             const std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  vararg_check(form, args, {}, {});
  return SymbolObject::make_new(reader.symbolTable, goal_to_goos.enclosing_method_type);
}

Object Interpreter::eval_format(const Object& form,
                                Arguments& args,
                                const std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  if (args.unnamed.size() < 2) {
    throw_eval_error(form, "format must get at least two arguments");
  }

  auto dest = args.unnamed.at(0);
  auto format_str = args.unnamed.at(1);
  if (!format_str.is_string()) {
    throw_eval_error(form, "format string must be a string");
  }

  // Note: this might be relying on internal implementation details of libfmt to work properly
  // and isn't a great solution.
  std::vector<fmt::basic_format_arg<fmt::format_context>> args2;
  std::vector<std::string> strings;
  for (size_t i = 2; i < args.unnamed.size(); i++) {
    if (args.unnamed.at(i).is_string()) {
      strings.push_back(args.unnamed.at(i).as_string()->data);
    } else {
      strings.push_back(args.unnamed.at(i).print());
    }
  }

  for (auto& x : strings) {
    args2.push_back(fmt::detail::make_arg<fmt::format_context>(x));
  }

  auto formatted =
      fmt::vformat(format_str.as_string()->data,
                   fmt::format_args(args2.data(), static_cast<unsigned>(args2.size())));

  if (truthy(dest)) {
    printf("%s", formatted.c_str());
  }

  return StringObject::make_new(formatted);
}

Object Interpreter::eval_error(const Object& form,
                               Arguments& args,
                               const std::shared_ptr<EnvironmentObject>& env) {
  (void)env;
  vararg_check(form, args, {ObjectType::STRING}, {});
  throw_eval_error(form, "Error: " + args.unnamed.at(0).as_string()->data);
  return EmptyListObject::make_new();
}
}  // namespace goos
