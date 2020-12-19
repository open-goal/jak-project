#pragma once

#ifndef JAK_COMPILER_H
#define JAK_COMPILER_H

#include <functional>
#include <optional>
#include "common/type_system/TypeSystem.h"
#include "Env.h"
#include "goalc/listener/Listener.h"
#include "common/goos/Interpreter.h"
#include "goalc/compiler/IR.h"
#include "goalc/debugger/Debugger.h"
#include "CompilerSettings.h"
#include "third-party/fmt/core.h"
#include "third-party/fmt/color.h"
#include "CompilerException.h"
#include "Enum.h"

enum MathMode { MATH_INT, MATH_BINT, MATH_FLOAT, MATH_INVALID };

class Compiler {
 public:
  Compiler();
  void execute_repl();
  goos::Interpreter& get_goos() { return m_goos; }
  FileEnv* compile_object_file(const std::string& name, goos::Object code, bool allow_emit);
  std::unique_ptr<FunctionEnv> compile_top_level_function(const std::string& name,
                                                          const goos::Object& code,
                                                          Env* env);
  Val* compile(const goos::Object& code, Env* env);
  Val* compile_error_guard(const goos::Object& code, Env* env);
  None* get_none() { return m_none.get(); }
  std::vector<std::string> run_test_from_file(const std::string& source_code);
  std::vector<std::string> run_test_from_string(const std::string& src,
                                                const std::string& obj_name = "*listener*");
  std::vector<std::string> run_test_no_load(const std::string& source_code);
  void compile_and_send_from_string(const std::string& source_code);
  void run_front_end_on_string(const std::string& src);
  void shutdown_target();
  void enable_throw_on_redefines() { m_throw_on_define_extern_redefinition = true; }
  Debugger& get_debugger() { return m_debugger; }
  listener::Listener& listener() { return m_listener; }
  void poke_target() { m_listener.send_poke(); }
  bool connect_to_target();

 private:
  bool get_true_or_false(const goos::Object& form, const goos::Object& boolean);
  bool try_getting_macro_from_goos(const goos::Object& macro_name, goos::Object* dest);
  void set_bitfield(const goos::Object& form, BitFieldVal* dst, RegVal* src, Env* env);
  Val* do_set(const goos::Object& form, Val* dst, RegVal* src_in_reg, Val* src, Env* env);
  Val* compile_goos_macro(const goos::Object& o,
                          const goos::Object& macro_obj,
                          const goos::Object& rest,
                          Env* env);
  Val* compile_pair(const goos::Object& code, Env* env);
  Val* compile_integer(const goos::Object& code, Env* env);
  Val* compile_integer(s64 value, Env* env);
  Val* compile_char(const goos::Object& code, Env* env);
  Val* compile_float(const goos::Object& code, Env* env);
  Val* compile_float(float value, Env* env, int seg);
  Val* compile_symbol(const goos::Object& form, Env* env);
  Val* compile_string(const goos::Object& form, Env* env);
  Val* compile_string(const std::string& str, Env* env, int seg = MAIN_SEGMENT);
  Val* compile_get_symbol_value(const goos::Object& form, const std::string& name, Env* env);
  Val* compile_function_or_method_call(const goos::Object& form, Env* env);

  Val* get_field_of_structure(const StructureType* type,
                              Val* object,
                              const std::string& field_name,
                              Env* env);

  SymbolVal* compile_get_sym_obj(const std::string& name, Env* env);
  void color_object_file(FileEnv* env);
  std::vector<u8> codegen_object_file(FileEnv* env);
  bool codegen_and_disassemble_object_file(FileEnv* env,
                                           std::vector<u8>* data_out,
                                           std::string* asm_out);

  void for_each_in_list(const goos::Object& list,
                        const std::function<void(const goos::Object&)>& f);

  goos::Arguments get_va(const goos::Object& form, const goos::Object& rest);
  void va_check(const goos::Object& form,
                const goos::Arguments& args,
                const std::vector<std::optional<goos::ObjectType>>& unnamed,
                const std::unordered_map<std::string,
                                         std::pair<bool, std::optional<goos::ObjectType>>>& named);
  std::string as_string(const goos::Object& o);
  std::string symbol_string(const goos::Object& o);
  std::string quoted_sym_as_string(const goos::Object& o);
  goos::Object unquote(const goos::Object& o);
  bool is_quoted_sym(const goos::Object& o);
  bool is_basic(const TypeSpec& ts);
  bool is_structure(const TypeSpec& ts);
  bool is_bitfield(const TypeSpec& ts);
  bool is_pair(const TypeSpec& ts);
  std::vector<goos::Object> get_list_as_vector(const goos::Object& o,
                                               goos::Object* rest_out = nullptr,
                                               int max_length = -1);
  const goos::Object& pair_car(const goos::Object& o);
  const goos::Object& pair_cdr(const goos::Object& o);
  void expect_empty_list(const goos::Object& o);
  void typecheck(const goos::Object& form,
                 const TypeSpec& expected,
                 const TypeSpec& actual,
                 const std::string& error_message = "");
  void typecheck_reg_type_allow_false(const goos::Object& form,
                                      const TypeSpec& expected,
                                      const Val* actual,
                                      const std::string& error_message = "");

  TypeSpec parse_typespec(const goos::Object& src);
  bool is_local_symbol(const goos::Object& obj, Env* env);
  emitter::RegKind get_preferred_reg_kind(const TypeSpec& ts);
  Val* compile_real_function_call(const goos::Object& form,
                                  RegVal* function,
                                  const std::vector<RegVal*>& args,
                                  Env* env,
                                  const std::string& method_type_name = "");

  bool try_getting_constant_integer(const goos::Object& in, int64_t* out, Env* env);
  float try_getting_constant_float(const goos::Object& in, float* out, Env* env);
  Val* compile_heap_new(const goos::Object& form,
                        const std::string& allocation,
                        const goos::Object& type,
                        const goos::Object* rest,
                        Env* env);
  Val* compile_static_new(const goos::Object& form,
                          const goos::Object& type,
                          const goos::Object* rest,
                          Env* env);
  Val* compile_stack_new(const goos::Object& form,
                         const goos::Object& type,
                         const goos::Object* rest,
                         Env* env);

  TypeSystem m_ts;
  std::unique_ptr<GlobalEnv> m_global_env = nullptr;
  std::unique_ptr<None> m_none = nullptr;
  bool m_want_exit = false;
  listener::Listener m_listener;
  Debugger m_debugger;
  goos::Interpreter m_goos;
  std::unordered_map<std::string, TypeSpec> m_symbol_types;
  std::unordered_map<std::string, GoalEnum> m_enums;
  std::unordered_map<std::shared_ptr<goos::SymbolObject>, goos::Object> m_global_constants;
  std::unordered_map<std::shared_ptr<goos::SymbolObject>, LambdaVal*> m_inlineable_functions;
  CompilerSettings m_settings;
  bool m_throw_on_define_extern_redefinition = false;
  MathMode get_math_mode(const TypeSpec& ts);
  bool is_number(const TypeSpec& ts);
  bool is_float(const TypeSpec& ts);
  bool is_integer(const TypeSpec& ts);
  bool is_binteger(const TypeSpec& ts);
  bool is_singed_integer_or_binteger(const TypeSpec& ts);
  Val* number_to_integer(const goos::Object& form, Val* in, Env* env);
  Val* number_to_float(const goos::Object& form, Val* in, Env* env);
  Val* number_to_binteger(const goos::Object& form, Val* in, Env* env);
  Val* to_math_type(const goos::Object& form, Val* in, MathMode mode, Env* env);
  bool is_none(Val* in);
  emitter::Register parse_register(const goos::Object& code);
  u64 enum_lookup(const goos::Object& form,
                  const GoalEnum& e,
                  const goos::Object& rest,
                  bool throw_on_error,
                  bool* success);
  Val* compile_enum_lookup(const goos::Object& form,
                           const GoalEnum& e,
                           const goos::Object& rest,
                           Env* env);

  Val* compile_variable_shift(const goos::Object& form,
                              const RegVal* in,
                              const RegVal* sa,
                              Env* env,
                              IntegerMathKind kind);
  Val* compile_fixed_shift(const goos::Object& form,
                           const RegVal* in,
                           u8 sa,
                           Env* env,
                           IntegerMathKind kind);

  Val* compile_format_string(const goos::Object& form,
                             Env* env,
                             const std::string& fmt_template,
                             std::vector<RegVal*> args,
                             const std::string& out_stream = "#t");
  void generate_field_description(const goos::Object& form,
                                  StructureType* type,
                                  Env* env,
                                  RegVal* reg,
                                  const Field& f);
  Val* generate_inspector_for_type(const goos::Object& form, Env* env, Type* type);
  RegVal* compile_get_method_of_type(const goos::Object& form,
                                     const TypeSpec& type,
                                     const std::string& method_name,
                                     Env* env);
  RegVal* compile_get_method_of_object(const goos::Object& form,
                                       RegVal* object,
                                       const std::string& method_name,
                                       Env* env);
  Val* compile_define_constant(const goos::Object& form,
                               const goos::Object& rest,
                               Env* env,
                               bool goos,
                               bool goal);

  Val* compile_new_static_structure_or_basic(const goos::Object& form,
                                             const TypeSpec& type,
                                             const goos::Object& field_defs,
                                             Env* env);
  Val* compile_new_static_bitfield(const goos::Object& form,
                                   const TypeSpec& type,
                                   const goos::Object& field_defs,
                                   Env* env);
  Val* compile_static_pair(const goos::Object& form, Env* env);
  StaticResult compile_static(const goos::Object& form, Env* env);
  StaticResult compile_static_no_eval_for_pairs(const goos::Object& form, Env* env);
  StaticResult compile_static_bitfield(const goos::Object& form,
                                       const TypeSpec& type,
                                       const goos::Object& _field_defs,
                                       Env* env);
  StaticResult compile_new_static_structure(const goos::Object& form,
                                            const TypeSpec& type,
                                            const goos::Object& _field_defs,
                                            Env* env);

  void compile_static_structure_inline(const goos::Object& form,
                                       const TypeSpec& type,
                                       const goos::Object& _field_defs,
                                       StaticStructure* structure,
                                       int offset,
                                       Env* env);

  template <typename... Args>
  void throw_compiler_error(const goos::Object& code, const std::string& str, Args&&... args) {
    fmt::print(fg(fmt::color::crimson) | fmt::emphasis::bold, "-- Compilation Error! --\n");
    if (!str.empty() && str.back() == '\n') {
      fmt::print(str, std::forward<Args>(args)...);
    } else {
      fmt::print(str + '\n', std::forward<Args>(args)...);
    }

    fmt::print(fg(fmt::color::yellow) | fmt::emphasis::bold, "Form:\n");
    fmt::print("{}\n", code.print());
    throw CompilerException("Compilation Error");
  }

  template <typename... Args>
  void throw_compiler_error_no_code(const std::string& str, Args&&... args) {
    fmt::print(fg(fmt::color::crimson) | fmt::emphasis::bold, "-- Compilation Error! --\n");
    if (!str.empty() && str.back() == '\n') {
      fmt::print(str, std::forward<Args>(args)...);
    } else {
      fmt::print(str + '\n', std::forward<Args>(args)...);
    }

    fmt::print(fg(fmt::color::yellow) | fmt::emphasis::bold, "Form:\n");
    throw CompilerException("Compilation Error");
  }

  template <typename... Args>
  void print_compiler_warning(const std::string& str, Args&&... args) {
    fmt::print(fg(fmt::color::yellow) | fmt::emphasis::bold, "[Warning] ");
    if (!str.empty() && str.back() == '\n') {
      fmt::print(str, std::forward<Args>(args)...);
    } else {
      fmt::print(str + '\n', std::forward<Args>(args)...);
    }
  }

 public:
  // Asm
  Val* compile_rlet(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_ret(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_push(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_pop(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_sub(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_add(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_load_sym(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_jr(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_mov(const goos::Object& form, const goos::Object& rest, Env* env);
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
  Val* compile_asm_data_file(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_listen_to_target(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_reset_target(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_poke(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_gs(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_set_config(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_in_package(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_build_dgo(const goos::Object& form, const goos::Object& rest, Env* env);

  // ControlFlow
  Condition compile_condition(const goos::Object& condition, Env* env, bool invert);
  Val* compile_condition_as_bool(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_when_goto(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_cond(const goos::Object& form, const goos::Object& rest, Env* env);

  // Define
  Val* compile_define(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_define_extern(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_set(const goos::Object& form, const goos::Object& rest, Env* env);

  // Debug
  Val* compile_dbg(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_dbs(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_break(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_cont(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_dump_all(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_pm(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_di(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_disasm(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_bp(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_ubp(const goos::Object& form, const goos::Object& rest, Env* env);
  u32 parse_address_spec(const goos::Object& form);

  // Macro
  Val* compile_gscond(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_quote(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_defglobalconstant(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_defconstant(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_mlet(const goos::Object& form, const goos::Object& rest, Env* env);

  // Math
  Val* compile_add(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_sub(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_mul(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_imul64(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_div(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_shlv(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_sarv(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_shrv(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_shl(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_sar(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_shr(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_mod(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_logxor(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_lognot(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_logand(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_logior(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_pointer_add(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_fmin(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_fmax(const goos::Object& form, const goos::Object& rest, Env* env);

  // Function
  Val* compile_lambda(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_inline(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_declare(const goos::Object& form, const goos::Object& rest, Env* env);

  // Type
  Val* compile_deftype(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_defmethod(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_deref(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_the_as(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_the(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_print_type(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_new(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_car(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_cdr(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_method(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_addr_of(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_declare_type(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_none(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_defenum(const goos::Object& form, const goos::Object& rest, Env* env);
};

#endif  // JAK_COMPILER_H
