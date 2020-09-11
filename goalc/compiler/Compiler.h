#ifndef JAK_COMPILER_H
#define JAK_COMPILER_H

#include "common/type_system/TypeSystem.h"
#include "Env.h"
#include "goalc/listener/Listener.h"
#include "goalc/goos/Interpreter.h"
#include "goalc/compiler/IR.h"

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

  std::vector<std::string> run_test(const std::string& source_code);
  void shutdown_target();

 private:
  void init_logger();
  bool try_getting_macro_from_goos(const goos::Object& macro_name, goos::Object* dest);
  Val* compile_goos_macro(const goos::Object& o,
                          const goos::Object& macro_obj,
                          const goos::Object& rest,
                          Env* env);
  Val* compile_pair(const goos::Object& code, Env* env);
  Val* compile_integer(const goos::Object& code, Env* env);
  Val* compile_integer(s64 value, Env* env);
  Val* compile_symbol(const goos::Object& form, Env* env);
  Val* compile_get_symbol_value(const std::string& name, Env* env);
  SymbolVal* compile_get_sym_obj(const std::string& name, Env* env);
  void color_object_file(FileEnv* env);
  std::vector<u8> codegen_object_file(FileEnv* env);

  void for_each_in_list(const goos::Object& list,
                        const std::function<void(const goos::Object&)>& f);

  goos::Arguments get_va(const goos::Object& form, const goos::Object& rest);
  void va_check(
      const goos::Object& form,
      const goos::Arguments& args,
      const std::vector<MatchParam<goos::ObjectType>>& unnamed,
      const std::unordered_map<std::string, std::pair<bool, MatchParam<goos::ObjectType>>>& named);
  std::string as_string(const goos::Object& o);
  std::string symbol_string(const goos::Object& o);
  const goos::Object& pair_car(const goos::Object& o);
  const goos::Object& pair_cdr(const goos::Object& o);
  void expect_empty_list(const goos::Object& o);

  TypeSystem m_ts;
  std::unique_ptr<GlobalEnv> m_global_env = nullptr;
  std::unique_ptr<None> m_none = nullptr;
  bool m_want_exit = false;
  listener::Listener m_listener;
  goos::Interpreter m_goos;
  std::unordered_map<std::string, TypeSpec> m_symbol_types;
  std::unordered_map<std::shared_ptr<goos::SymbolObject>, goos::Object> m_global_constants;
  std::unordered_map<std::shared_ptr<goos::SymbolObject>, LambdaVal*> m_inlineable_functions;

  void typecheck(const goos::Object& form,
                 const TypeSpec& expected,
                 const TypeSpec& actual,
                 const std::string& error_message = "");

 public:
  // Atoms

  // Block
  Val* compile_begin(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_top_level(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_block(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_return_from(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_label(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_goto(const goos::Object& form, const goos::Object& rest, Env* env);

  // CompilerControl
  Val* compile_seval(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_exit(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_file(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_listen_to_target(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_reset_target(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_poke(const goos::Object& form, const goos::Object& rest, Env* env);

  // Define
  Val* compile_define(const goos::Object& form, const goos::Object& rest, Env* env);

  // Macro
  Val* compile_gscond(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_quote(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_defglobalconstant(const goos::Object& form, const goos::Object& rest, Env* env);
};

#endif  // JAK_COMPILER_H
