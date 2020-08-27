#ifndef COMPILER_GOOS_H
#define COMPILER_GOOS_H

#include <unordered_set>
#include "reader/Reader.h"
#include "goos/Object.h"

struct GoosArgs {
  std::vector<Object> unnamed_args;
  std::unordered_map<std::string, Object> named_args;
  Object rest;
  bool has_rest = false;

  std::string print();

  bool check_count(int count);
  bool check_keywords(std::unordered_set<std::string>& keywords);
};

struct GoalToGoosData {
  std::string enclosing_method_type;

  void reset() { enclosing_method_type = "#f"; }
};

class Goos {
 public:
  Goos();
  void execute_repl();
  void throw_eval_error(Object o, const std::string& err);
  Object eval(Object obj, std::shared_ptr<EnvironmentObject> env);
  Object eval_with_rewind(Object obj, std::shared_ptr<EnvironmentObject> env);
  bool get_object_by_name(const std::string& name, Object& dest);
  Object get_object_by_name(const std::string& name) {
    Object o = EmptyListObject::make_new();
    get_object_by_name(name, o);
    return o;
  }
  Reader reader;
  Object global_environment;
  Object goal_goos_env;
  GoalToGoosData goal_to_goos;

  Object eval_eval(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  Object eval_equals(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  Object eval_exit(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  Object eval_begin(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  Object eval_read(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  Object eval_read_file(const Object& form,
                        GoosArgs& args,
                        std::shared_ptr<EnvironmentObject>& env);
  Object eval_load_file(const Object& form,
                        GoosArgs& args,
                        std::shared_ptr<EnvironmentObject>& env);
  Object eval_print(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  Object eval_inspect(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  Object eval_plus(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  Object eval_minus(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  Object eval_times(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  Object eval_numequals(const Object& form,
                        GoosArgs& args,
                        std::shared_ptr<EnvironmentObject>& env);
  Object eval_lt(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  Object eval_gt(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  Object eval_leq(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  Object eval_geq(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  Object eval_car(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  Object eval_cdr(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  Object eval_gensym(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  Object eval_cons(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  Object eval_null(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  Object eval_type(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  Object eval_current_method_type(const Object& form,
                                  GoosArgs& args,
                                  std::shared_ptr<EnvironmentObject>& env);

  // specials
  Object eval_define(const Object& form, Object rest, std::shared_ptr<EnvironmentObject>& env);
  Object eval_quote(const Object& form, Object rest, std::shared_ptr<EnvironmentObject>& env);
  Object eval_set(const Object& form, Object rest, std::shared_ptr<EnvironmentObject>& env);
  Object eval_lambda(const Object& form, Object rest, std::shared_ptr<EnvironmentObject>& env);
  Object eval_cond(const Object& form, Object rest, std::shared_ptr<EnvironmentObject>& env);
  Object eval_or(const Object& form, Object rest, std::shared_ptr<EnvironmentObject>& env);
  Object eval_and(const Object& form, Object rest, std::shared_ptr<EnvironmentObject>& env);
  Object eval_quasiquote(const Object& form, Object rest, std::shared_ptr<EnvironmentObject>& env);
  Object eval_macro(const Object& form, Object rest, std::shared_ptr<EnvironmentObject>& env);
  Object eval_while(const Object& form, Object rest, std::shared_ptr<EnvironmentObject>& env);

 private:
  friend class Goal;
  Object eval_pair(Object o, std::shared_ptr<EnvironmentObject> env);
  Object eval_symbol(Object sym, std::shared_ptr<EnvironmentObject> env);

  GoosArgs eval_args(const Object& form, Object rest, std::shared_ptr<EnvironmentObject>& env);
  GoosArgs get_macro_args(const Object& form, Object rest);
  GoosArgs get_uneval_args(const Object& form, Object rest, int count);
  GoosArgs get_uneval_args_no_rest(const Object& form, Object rest, int count);
  Object eval_list_return_last(const Object& form,
                               Object rest,
                               std::shared_ptr<EnvironmentObject>& env);
  Object quasiquote_helper(Object form, std::shared_ptr<EnvironmentObject>& env);

  int64_t number_to_integer(const Object& obj);
  char number_to_char(const Object& obj);
  double number_to_float(const Object& obj);

  void define_var_in_env(Object& env, Object& var, const std::string& name);

  void load_goos_library();

  template <typename T>
  T number(const Object& obj);

  template <typename T>
  Object num_lt(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  template <typename T>
  Object num_gt(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  template <typename T>
  Object num_leq(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  template <typename T>
  Object num_geq(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  template <typename T>
  Object num_plus(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  template <typename T>
  Object num_minus(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);
  template <typename T>
  Object num_times(const Object& form, GoosArgs& args, std::shared_ptr<EnvironmentObject>& env);

  bool want_exit = false;
};

#endif  // COMPILER_GOOS_H
