#pragma once

/*!
 * @file Interpreter.h
 * The GOOS Interpreter and implementation of special and "built-in forms"
 */

#include <memory>
#include <optional>
#include "Object.h"
#include "Reader.h"

namespace goos {
class Interpreter {
 public:
  Interpreter();
  ~Interpreter();
  void execute_repl(ReplWrapper& repl);
  void throw_eval_error(const Object& o, const std::string& err);
  Object eval_with_rewind(const Object& obj, const std::shared_ptr<EnvironmentObject>& env);
  bool get_global_variable_by_name(const std::string& name, Object* dest);
  Object eval(Object obj, const std::shared_ptr<EnvironmentObject>& env);
  Object intern(const std::string& name);
  void disable_printfs();
  Object eval_symbol(const Object& sym, const std::shared_ptr<EnvironmentObject>& env);
  bool eval_symbol(const Object& sym,
                   const std::shared_ptr<EnvironmentObject>& env,
                   Object* result);
  Arguments get_args(const Object& form, const Object& rest, const ArgumentSpec& spec);
  void set_args_in_env(const Object& form,
                       const Arguments& args,
                       const ArgumentSpec& arg_spec,
                       const std::shared_ptr<EnvironmentObject>& env);
  Object eval_list_return_last(const Object& form,
                               Object rest,
                               const std::shared_ptr<EnvironmentObject>& env);
  bool truthy(const Object& o);

  Reader reader;
  Object global_environment;
  Object goal_env;

  // data passed from GOAL to GOOS available to any evaluation.
  struct GoalToGoosData {
    std::string enclosing_method_type;

    void reset() { enclosing_method_type = "#f"; }
  } goal_to_goos;

 private:
  friend class Goal;
  void load_goos_library();
  void define_var_in_env(Object& env, Object& var, const std::string& name);
  void expect_env(const Object& form, const Object& o);
  void vararg_check(
      const Object& form,
      const Arguments& args,
      const std::vector<std::optional<ObjectType>>& unnamed,
      const std::unordered_map<std::string, std::pair<bool, std::optional<ObjectType>>>& named);

  Object eval_pair(const Object& o, const std::shared_ptr<EnvironmentObject>& env);
  void eval_args(Arguments* args, const std::shared_ptr<EnvironmentObject>& env);
  ArgumentSpec parse_arg_spec(const Object& form, Object& rest);

  Object quasiquote_helper(const Object& form, const std::shared_ptr<EnvironmentObject>& env);

  IntType number_to_integer(const Object& obj);
  FloatType number_to_float(const Object& obj);

  template <typename T>
  T number(const Object& obj);

  template <typename T>
  Object num_lt(const Object& form, Arguments& args, const std::shared_ptr<EnvironmentObject>& env);
  template <typename T>
  Object num_gt(const Object& form, Arguments& args, const std::shared_ptr<EnvironmentObject>& env);
  template <typename T>
  Object num_leq(const Object& form,
                 Arguments& args,
                 const std::shared_ptr<EnvironmentObject>& env);
  template <typename T>
  Object num_geq(const Object& form,
                 Arguments& args,
                 const std::shared_ptr<EnvironmentObject>& env);
  template <typename T>
  Object num_plus(const Object& form,
                  Arguments& args,
                  const std::shared_ptr<EnvironmentObject>& env);
  template <typename T>
  Object num_minus(const Object& form,
                   Arguments& args,
                   const std::shared_ptr<EnvironmentObject>& env);
  template <typename T>
  Object num_divide(const Object& form,
                    Arguments& args,
                    const std::shared_ptr<EnvironmentObject>& env);
  template <typename T>
  Object num_times(const Object& form,
                   Arguments& args,
                   const std::shared_ptr<EnvironmentObject>& env);

  Object eval_eval(const Object& form,
                   Arguments& args,
                   const std::shared_ptr<EnvironmentObject>& env);
  Object eval_equals(const Object& form,
                     Arguments& args,
                     const std::shared_ptr<EnvironmentObject>& env);
  Object eval_exit(const Object& form,
                   Arguments& args,
                   const std::shared_ptr<EnvironmentObject>& env);
  Object eval_begin(const Object& form,
                    Arguments& args,
                    const std::shared_ptr<EnvironmentObject>& env);
  Object eval_read(const Object& form,
                   Arguments& args,
                   const std::shared_ptr<EnvironmentObject>& env);
  Object eval_read_file(const Object& form,
                        Arguments& args,
                        const std::shared_ptr<EnvironmentObject>& env);
  Object eval_load_file(const Object& form,
                        Arguments& args,
                        const std::shared_ptr<EnvironmentObject>& env);
  Object eval_print(const Object& form,
                    Arguments& args,
                    const std::shared_ptr<EnvironmentObject>& env);
  Object eval_inspect(const Object& form,
                      Arguments& args,
                      const std::shared_ptr<EnvironmentObject>& env);
  Object eval_plus(const Object& form,
                   Arguments& args,
                   const std::shared_ptr<EnvironmentObject>& env);
  Object eval_minus(const Object& form,
                    Arguments& args,
                    const std::shared_ptr<EnvironmentObject>& env);
  Object eval_times(const Object& form,
                    Arguments& args,
                    const std::shared_ptr<EnvironmentObject>& env);
  Object eval_divide(const Object& form,
                     Arguments& args,
                     const std::shared_ptr<EnvironmentObject>& env);
  Object eval_numequals(const Object& form,
                        Arguments& args,
                        const std::shared_ptr<EnvironmentObject>& env);
  Object eval_lt(const Object& form,
                 Arguments& args,
                 const std::shared_ptr<EnvironmentObject>& env);
  Object eval_gt(const Object& form,
                 Arguments& args,
                 const std::shared_ptr<EnvironmentObject>& env);
  Object eval_leq(const Object& form,
                  Arguments& args,
                  const std::shared_ptr<EnvironmentObject>& env);
  Object eval_geq(const Object& form,
                  Arguments& args,
                  const std::shared_ptr<EnvironmentObject>& env);
  Object eval_car(const Object& form,
                  Arguments& args,
                  const std::shared_ptr<EnvironmentObject>& env);
  Object eval_cdr(const Object& form,
                  Arguments& args,
                  const std::shared_ptr<EnvironmentObject>& env);
  Object eval_set_car(const Object& form,
                      Arguments& args,
                      const std::shared_ptr<EnvironmentObject>& env);
  Object eval_set_cdr(const Object& form,
                      Arguments& args,
                      const std::shared_ptr<EnvironmentObject>& env);
  Object eval_gensym(const Object& form,
                     Arguments& args,
                     const std::shared_ptr<EnvironmentObject>& env);
  Object eval_cons(const Object& form,
                   Arguments& args,
                   const std::shared_ptr<EnvironmentObject>& env);
  Object eval_null(const Object& form,
                   Arguments& args,
                   const std::shared_ptr<EnvironmentObject>& env);
  Object eval_type(const Object& form,
                   Arguments& args,
                   const std::shared_ptr<EnvironmentObject>& env);
  Object eval_current_method_type(const Object& form,
                                  Arguments& args,
                                  const std::shared_ptr<EnvironmentObject>& env);
  Object eval_format(const Object& form,
                     Arguments& args,
                     const std::shared_ptr<EnvironmentObject>& env);
  Object eval_error(const Object& form,
                    Arguments& args,
                    const std::shared_ptr<EnvironmentObject>& env);

  // specials
  Object eval_define(const Object& form,
                     const Object& rest,
                     const std::shared_ptr<EnvironmentObject>& env);
  Object eval_quote(const Object& form,
                    const Object& rest,
                    const std::shared_ptr<EnvironmentObject>& env);
  Object eval_set(const Object& form,
                  const Object& rest,
                  const std::shared_ptr<EnvironmentObject>& env);
  Object eval_lambda(const Object& form,
                     const Object& rest,
                     const std::shared_ptr<EnvironmentObject>& env);
  Object eval_cond(const Object& form,
                   const Object& rest,
                   const std::shared_ptr<EnvironmentObject>& env);
  Object eval_or(const Object& form,
                 const Object& rest,
                 const std::shared_ptr<EnvironmentObject>& env);
  Object eval_and(const Object& form,
                  const Object& rest,
                  const std::shared_ptr<EnvironmentObject>& env);
  Object eval_quasiquote(const Object& form,
                         const Object& rest,
                         const std::shared_ptr<EnvironmentObject>& env);
  Object eval_macro(const Object& form,
                    const Object& rest,
                    const std::shared_ptr<EnvironmentObject>& env);
  Object eval_while(const Object& form,
                    const Object& rest,
                    const std::shared_ptr<EnvironmentObject>& env);

  bool want_exit = false;
  bool disable_printing = false;

  std::unordered_map<std::string,
                     Object (Interpreter::*)(const Object& form,
                                             Arguments& args,
                                             const std::shared_ptr<EnvironmentObject>& env)>
      builtin_forms;
  std::unordered_map<std::string,
                     Object (Interpreter::*)(const Object& form,
                                             const Object& rest,
                                             const std::shared_ptr<EnvironmentObject>& env)>
      special_forms;
  int64_t gensym_id = 0;

  std::unordered_map<std::string, ObjectType> string_to_type;
};
}  // namespace goos
