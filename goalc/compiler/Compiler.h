#ifndef JAK_COMPILER_H
#define JAK_COMPILER_H

#include "common/type_system/TypeSystem.h"
#include "Env.h"
#include "goalc/listener/Listener.h"
#include "goalc/goos/Interpreter.h"

class Compiler {
 public:
  Compiler();
  ~Compiler();
  void execute_repl();
  FileEnv* compile_object_file(const std::string& name, goos::Object code, bool allow_emit);
  std::unique_ptr<FunctionEnv> compile_top_level_function(const std::string& name,
                                                          const goos::Object& code,
                                                          Env* env);
  Val* compile(const goos::Object& code, Env* env);
  Val* compile_error_guard(const goos::Object& code, Env* env);
  void throw_compile_error(const goos::Object& o, const std::string& err);
  void ice(const std::string& err);
  None* get_none() { return m_none.get(); }

 private:
  void init_logger();
  Val* compile_pair(const goos::Object& code, Env* env);
  Val* compile_integer(const goos::Object& code, Env* env);
  Val* compile_integer(s64 value, Env* env);

  void for_each_in_list(const goos::Object& list, const std::function<void (const goos::Object&)>& f);


  goos::Arguments get_va(const goos::Object& form, const goos::Object& rest);
  void va_check(
      const goos::Object& form,
      const goos::Arguments& args,
      const std::vector<util::MatchParam<goos::ObjectType>>& unnamed,
      const std::unordered_map<std::string, std::pair<bool, util::MatchParam<goos::ObjectType>>>&
          named);

  TypeSystem m_ts;
  std::unique_ptr<GlobalEnv> m_global_env = nullptr;
  std::unique_ptr<None> m_none = nullptr;
  bool m_want_exit = false;
  listener::Listener m_listener;
  goos::Interpreter m_goos;

 public:
  Val* compile_exit(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_top_level(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_begin(const goos::Object& form, const goos::Object& rest, Env* env);
};

#endif  // JAK_COMPILER_H
