#pragma once

#include <functional>
#include <mutex>
#include <optional>

#include "common/goos/Interpreter.h"
#include "common/repl/repl_wrapper.h"
#include "common/type_system/TypeSystem.h"

#include "goalc/compiler/CompilerException.h"
#include "goalc/compiler/CompilerSettings.h"
#include "goalc/compiler/Env.h"
#include "goalc/compiler/IR.h"
#include "goalc/compiler/docs/DocTypes.h"
#include "goalc/compiler/symbol_info.h"
#include "goalc/data_compiler/game_text_common.h"
#include "goalc/debugger/Debugger.h"
#include "goalc/emitter/Register.h"
#include "goalc/listener/Listener.h"
#include "goalc/make/MakeSystem.h"

#include "fmt/color.h"
#include "fmt/core.h"

enum MathMode { MATH_INT, MATH_BINT, MATH_FLOAT, MATH_INVALID };

enum class ReplStatus { OK, WANT_EXIT, WANT_RELOAD };

struct CompilationOptions {
  std::string filename;                 // input file
  std::string disassembly_output_file;  // file to write, containing x86 assembly output
  bool load = false;                    // send to target
  bool color = false;                   // do register allocation/code generation passes
  bool write = false;                   // write object file to out/obj
  bool no_code = false;                 // file shouldn't generate code, throw error if it does
  bool disassemble = false;             // either print disassembly to stdout or output_file
  bool disasm_code_only = false;        // if on, IR and source lines are not printed
  bool print_time = false;              // print timing statistics
};

struct GlobalConstantInfo {
  std::optional<goos::TextDb::ShortInfo> definition_info;
};

class Compiler {
 public:
  Compiler(GameVersion version,
           const std::optional<REPL::Config> repl_config = {},
           const std::string& user_profile = "#f",
           std::unique_ptr<REPL::Wrapper> repl = nullptr);
  ~Compiler();
  void asm_file(const CompilationOptions& options);

  void save_repl_history();
  void print_to_repl(const std::string& str);
  std::string get_prompt();
  std::string get_repl_input();
  ReplStatus handle_repl_string(const std::string& input);
  goos::Interpreter& get_goos() { return m_goos; }
  FileEnv* compile_object_file(const std::string& name, goos::Object code, bool allow_emit);
  std::unique_ptr<FunctionEnv> compile_top_level_function(const std::string& name,
                                                          const goos::Object& code,
                                                          Env* env);
  Val* compile(const goos::Object& code, Env* env);
  Val* compile_no_const_prop(const goos::Object& code, Env* env);

  Val* compile_error_guard(const goos::Object& code, Env* env);
  None* get_none() { return m_none.get(); }
  std::vector<std::string> run_test_from_file(const std::string& source_code);
  std::vector<std::string> run_test_from_string(const std::string& src,
                                                const std::string& obj_name = "*listener*");
  std::vector<std::string> run_test_no_load(const std::string& source_code);
  void compile_and_send_from_string(const std::string& source_code);
  void run_front_end_on_string(const std::string& src);
  void run_front_end_on_file(const std::vector<std::string>& path);
  void run_full_compiler_on_string_no_save(const std::string& src,
                                           const std::optional<std::string>& string_name);
  void shutdown_target();
  void enable_throw_on_redefines() { m_throw_on_define_extern_redefinition = true; }
  void add_ignored_define_extern_symbol(const std::string& name) {
    m_allow_inconsistent_definition_symbols.insert(name);
  }
  void add_ignored_type_definition(const std::string& type_name) {
    m_ts.add_type_to_allowed_redefinition_list(type_name);
  }
  Debugger& get_debugger() { return m_debugger; }
  listener::Listener& listener() { return m_listener; }
  void poke_target() { m_listener.send_poke(); }
  bool connect_to_target();
  replxx::Replxx::completions_t find_symbols_or_object_file_by_prefix(
      std::string const& context,
      int& contextLen,
      std::vector<std::string> const& user_data);
  replxx::Replxx::hints_t find_hints_by_prefix(std::string const& context,
                                               int& contextLen,
                                               replxx::Replxx::Color& color,
                                               std::vector<std::string> const& user_data);
  void repl_coloring(std::string const& str,
                     replxx::Replxx::colors_t& colors,
                     std::vector<std::pair<std::string, replxx::Replxx::Color>> const& user_data);
  bool knows_object_file(const std::string& name);
  MakeSystem& make_system() { return m_make; }
  std::vector<symbol_info::SymbolInfo*> lookup_symbol_info_by_file(
      const std::string& file_path) const;
  std::vector<symbol_info::SymbolInfo*> lookup_symbol_info_by_prefix(
      const std::string& prefix) const;
  std::set<std::string> lookup_symbol_names_starting_with(const std::string& prefix,
                                                          int max_count = -1) const;
  std::vector<symbol_info::SymbolInfo*> lookup_exact_name_info(const std::string& name) const;
  std::optional<TypeSpec> lookup_typespec(const std::string& symbol_name);
  TypeSystem& type_system() { return m_ts; };
  // TODO - rename these types / namespaces -- consolidate with SymbolInfo and whatever else tries
  // to also do this work
  std::tuple<std::unordered_map<std::string, Docs::SymbolDocumentation>,
             std::unordered_map<std::string, Docs::FileDocumentation>>
  generate_per_file_symbol_info();

 private:
  GameVersion m_version;
  TypeSystem m_ts;
  std::unique_ptr<GlobalEnv> m_global_env = nullptr;
  std::unique_ptr<None> m_none = nullptr;
  bool m_want_exit = false;
  bool m_want_reload = false;
  listener::Listener m_listener;
  goos::Interpreter m_goos;
  Debugger m_debugger;
  MakeSystem m_make;
  std::unique_ptr<REPL::Wrapper> m_repl;
  CompilerSettings m_settings;
  bool m_throw_on_define_extern_redefinition = false;  // TODO - move to settings

  // State Tracking
  symbol_info::SymbolInfoMap m_symbol_info;
  goos::InternedPtrMap<TypeSpec> m_symbol_types;
  goos::InternedPtrMap<goos::Object> m_global_constants;
  std::unordered_map<goos::InternedSymbolPtr, InlineableFunction, goos::InternedSymbolPtr::hash>
      m_inlineable_functions;

  // Overrides
  std::unordered_set<std::string> m_allow_inconsistent_definition_symbols;

  struct DebugStats {
    int num_spills = 0;
    int num_spills_v1 = 0;
    int num_moves_eliminated = 0;
    int total_funcs = 0;
    int funcs_requiring_v1_allocator = 0;
  } m_debug_stats;

  void setup_goos_forms();
  bool get_true_or_false(const goos::Object& form, const goos::Object& boolean);
  bool try_getting_macro_from_goos(const goos::Object& macro_name, goos::Object* dest);
  bool expand_macro_once(const goos::Object& src, goos::Object* out, Env* env);
  goos::Object expand_macro_completely(const goos::Object& src, Env* env);

  void set_bitfield(const goos::Object& form, BitFieldVal* dst, RegVal* src, Env* env);
  void set_bitfield_128(const goos::Object& form, BitFieldVal* dst, RegVal* src, Env* env);

  void set_bits_in_bitfield(const goos::Object& form,
                            int size,
                            int offset,
                            RegVal* dst,
                            RegVal* src,
                            FunctionEnv* fe,
                            Env* env);

  Val* do_set(const goos::Object& form, Val* dst, RegVal* src_in_reg, Val* src, Env* env);
  Val* compile_goos_macro(const goos::Object& o,
                          const goos::Object& macro_obj,
                          const goos::Object& rest,
                          const goos::Object& name_symbol,
                          Env* env);
  Val* compile_pair(const goos::Object& code, Env* env);
  Val* compile_integer(const goos::Object& code, Env* env);
  Val* compile_integer(const U128& value, Env* env);
  Val* compile_integer(s64 value, Env* env);
  Val* compile_char(const goos::Object& code, Env* env);
  Val* compile_float(const goos::Object& code, Env* env);
  Val* compile_float(float value, Env* env, int seg);
  Val* compile_symbol(const goos::Object& form, Env* env);
  Val* compile_string(const goos::Object& form, Env* env);
  Val* compile_string(const std::string& str, Env* env, int seg);
  Val* compile_get_symbol_value(const goos::Object& form, const std::string& name, Env* env);
  Val* compile_function_or_method_call(const goos::Object& form, Env* env);

  Val* compile_asm_vf_math3(const goos::Object& form,
                            const goos::Object& rest,
                            IR_VFMath3Asm::Kind kind,
                            emitter::Register::VF_ELEMENT broadcastElement,
                            Env* env);

  Val* compile_asm_int128_math3(const goos::Object& form,
                                const goos::Object& rest,
                                IR_Int128Math3Asm::Kind kind,
                                Env* env);

  Val* compile_asm_vf_math2(const goos::Object& form,
                            const goos::Object& rest,
                            IR_VFMath2Asm::Kind kind,
                            Env* env);

  Val* compile_asm_int128_math2_imm_u8(const goos::Object& form,
                                       const goos::Object& rest,
                                       IR_Int128Math2Asm::Kind kind,
                                       Env* env);

  Val* compile_asm_vf_math4_two_operation(const goos::Object& form,
                                          const goos::Object& rest,
                                          IR_VFMath3Asm::Kind first_op_kind,
                                          IR_VFMath3Asm::Kind second_op_kind,
                                          emitter::Register::VF_ELEMENT broadcastElement,
                                          Env* env);

  Val* get_field_of_structure(const StructureType* type,
                              Val* object,
                              const std::string& field_name,
                              Env* env);

  Val* get_field_of_bitfield(const BitFieldType* type,
                             Val* object,
                             const std::string& field_name,
                             Env* env);

  SymbolVal* compile_get_sym_obj(const std::string& name, Env* env);
  void color_object_file(FileEnv* env);
  std::vector<u8> codegen_object_file(FileEnv* env);
  bool codegen_and_disassemble_object_file(FileEnv* env,
                                           std::vector<u8>* data_out,
                                           std::string* asm_out,
                                           bool omit_ir);

  void for_each_in_list(const goos::Object& list,
                        const std::function<void(const goos::Object&)>& f);

  goos::Arguments get_va(const goos::Object& form, const goos::Object& rest);
  goos::Arguments get_va_no_named(const goos::Object& form, const goos::Object& rest);
  void va_check(const goos::Object& form,
                const goos::Arguments& args,
                const std::vector<std::optional<goos::ObjectType>>& unnamed,
                const std::unordered_map<std::string,
                                         std::pair<bool, std::optional<goos::ObjectType>>>& named);
  const std::string& as_string(const goos::Object& o);
  std::string symbol_string(const goos::Object& o);
  std::string quoted_sym_as_string(const goos::Object& o);
  goos::Object unquote(const goos::Object& o);
  bool is_quoted_sym(const goos::Object& o);
  bool is_basic(const TypeSpec& ts);
  bool is_structure(const TypeSpec& ts);
  bool is_bitfield(const TypeSpec& ts);
  bool is_pair(const TypeSpec& ts);
  bool is_symbol(const TypeSpec& ts);
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

  TypeSpec parse_typespec(const goos::Object& src, Env* env);
  bool is_local_symbol(const goos::Object& obj, Env* env);
  Val* compile_real_function_call(const goos::Object& form,
                                  RegVal* function,
                                  const std::vector<RegVal*>& args,
                                  Env* env,
                                  const std::string& method_type_name = "");

  s64 get_constant_integer_or_error(const goos::Object& in, Env* env);
  ValOrConstInt get_constant_integer_or_variable(const goos::Object& in, Env* env);
  ValOrConstFloat get_constant_float_or_variable(const goos::Object& in, Env* env);

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
                         Env* env,
                         bool call_constructor,
                         bool use_singleton);

  StaticResult fill_static_array(const goos::Object& form,
                                 const goos::Object& rest,
                                 Env* env,
                                 int seg);

  StaticResult fill_static_boxed_array(const goos::Object& form,
                                       const goos::Object& rest,
                                       Env* env,
                                       int seg,
                                       const std::string& array_type);

  StaticResult fill_static_inline_array(const goos::Object& form,
                                        const goos::Object& rest,
                                        Env* env,
                                        int seg);

  void fill_static_inline_array_inline(const goos::Object& form,
                                       const TypeSpec& content_type,
                                       const std::vector<goos::Object>& args,
                                       StaticStructure* structure,
                                       int offset,
                                       Env* env);
  void fill_static_array_inline(const goos::Object& form,
                                const TypeSpec& content_type,
                                goos::Object* args_array,
                                int args_array_length,
                                StaticStructure* structure,
                                int offset,
                                Env* env);

  std::string make_symbol_info_description(const symbol_info::SymbolInfo* info);

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
                  const EnumType* e,
                  const goos::Object& rest,
                  bool throw_on_error,
                  bool* success);
  Val* compile_enum_lookup(const goos::Object& form,
                           const EnumType* e,
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
  Val* compile_floating_point_division(const goos::Object& form,
                                       const TypeSpec& result_type,
                                       RegVal* a,
                                       RegVal* b,
                                       Env* env,
                                       bool imm_divisor);

  Val* compile_format_string(const goos::Object& form,
                             Env* env,
                             const std::string& fmt_template,
                             std::vector<RegVal*> args,
                             const std::string& out_stream = "#t");
  void generate_field_description(const goos::Object& form,
                                  StructureType* type,
                                  Env* env,
                                  RegVal* reg,
                                  const Field& f,
                                  int tab_count);
  Val* generate_inspector_for_structure_type(const goos::Object& form,
                                             Env* env,
                                             StructureType* structure_type);
  Val* generate_inspector_for_bitfield_type(const goos::Object& form, Env* env, BitFieldType* type);
  RegVal* compile_get_method_of_type(const goos::Object& form,
                                     const TypeSpec& compile_time_type,
                                     RegVal* type_object,
                                     const std::string& method_name,
                                     Env* env);
  RegVal* compile_get_method_of_type(const goos::Object& form,
                                     const TypeSpec& compile_time_type,
                                     const std::string& method_name,
                                     Env* env);
  RegVal* compile_get_method_of_object(const goos::Object& form,
                                       RegVal* object,
                                       const std::string& method_name,
                                       Env* env,
                                       bool error_message_function_or_method = false);
  Val* compile_define_constant(const goos::Object& form,
                               const goos::Object& rest,
                               Env* env,
                               bool goos,
                               bool goal);

  Val* compile_new_static_structure_or_basic(const goos::Object& form,
                                             const TypeSpec& type,
                                             const goos::Object& field_defs,
                                             Env* env,
                                             int seg);
  Val* compile_static_pair(const goos::Object& form, Env* env, int seg);
  StaticResult compile_static(const goos::Object& form, Env* env);
  StaticResult compile_static_no_eval_for_pairs(const goos::Object& form,
                                                Env* env,
                                                int seg,
                                                bool can_macro);

  Val* compile_bitfield_definition(const goos::Object& form,
                                   const TypeSpec& type,
                                   const goos::Object& _field_defs,
                                   bool allow_dynamic_construction,
                                   Env* env);
  StaticResult compile_new_static_structure(const goos::Object& form,
                                            const TypeSpec& type,
                                            const goos::Object& _field_defs,
                                            Env* env,
                                            int seg);

  void compile_static_structure_inline(const goos::Object& form,
                                       const TypeSpec& type,
                                       const goos::Object& _field_defs,
                                       StaticStructure* structure,
                                       int offset,
                                       Env* env);
  void compile_constant_product(const goos::Object& form,
                                RegVal* dest,
                                RegVal* src,
                                int stride,
                                Env* env);
  void check_vector_float_regs(const goos::Object& form,
                               Env* env,
                               std::vector<std::pair<std::string, RegVal*>> args);
  u8 ftf_fsf_to_blend_mask(u8 val);
  emitter::Register::VF_ELEMENT ftf_fsf_to_vector_element(u8 val);
  int get_size_for_size_of(const goos::Object& form, const goos::Object& rest);

  template <typename... Args>
  [[noreturn]] void throw_compiler_error(const goos::Object& code,
                                         const std::string& str,
                                         Args&&... args) {
    lg::print(fg(fmt::color::crimson) | fmt::emphasis::bold, "-- Compilation Error! --\n");
    if (!str.empty() && str.back() == '\n') {
      lg::print(fmt::emphasis::bold, str, std::forward<Args>(args)...);
    } else {
      lg::print(fmt::emphasis::bold, str + '\n', std::forward<Args>(args)...);
    }

    lg::print(fg(fmt::color::yellow) | fmt::emphasis::bold, "Form:\n");
    lg::print("{}\n", code.print());
    throw CompilerException("Compilation Error");
  }

  template <typename... Args>
  [[noreturn]] void throw_compiler_error_no_code(const std::string& str, Args&&... args) {
    lg::print(fg(fmt::color::crimson) | fmt::emphasis::bold, "-- Compilation Error! --\n");
    if (!str.empty() && str.back() == '\n') {
      lg::print(fmt::emphasis::bold, str, std::forward<Args>(args)...);
    } else {
      lg::print(fmt::emphasis::bold, str + '\n', std::forward<Args>(args)...);
    }

    lg::print(fg(fmt::color::yellow) | fmt::emphasis::bold, "Form:\n");
    throw CompilerException("Compilation Error");
  }

  template <typename... Args>
  void print_compiler_warning(const std::string& str, Args&&... args) {
    lg::print(fg(fmt::color::yellow) | fmt::emphasis::bold, "[Warning] ");
    if (!str.empty() && str.back() == '\n') {
      lg::print(str, std::forward<Args>(args)...);
    } else {
      lg::print(str + '\n', std::forward<Args>(args)...);
    }
  }

  void compile_state_handler_set(StructureType* state_type_info,
                                 RegVal* state_object,
                                 const std::string& name,
                                 goos::Arguments& args,
                                 const goos::Object& form,
                                 Env* env,
                                 Val*& code_val,
                                 Val*& enter_val);

 public:
  struct ConstPropResult {
    goos::Object value;
    bool has_side_effects = true;
  };
  ConstPropResult try_constant_propagation(const goos::Object& form, Env* env);
  ConstPropResult constant_propagation_dispatch(const goos::Object& form, Env* env);

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

  // Vector Float Operations
  Val* compile_asm_lvf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_svf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_mov_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_blend_vf(const goos::Object& form, const goos::Object& rest, Env* env);

  Val* compile_asm_wait_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_nop_vf(const goos::Object& form, const goos::Object& rest, Env* env);

  Val* compile_asm_xor_vf(const goos::Object& form, const goos::Object& rest, Env* env);

  Val* compile_asm_max_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_max_x_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_max_y_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_max_z_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_max_w_vf(const goos::Object& form, const goos::Object& rest, Env* env);

  Val* compile_asm_min_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_min_x_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_min_y_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_min_z_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_min_w_vf(const goos::Object& form, const goos::Object& rest, Env* env);

  Val* compile_asm_sub_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_sub_x_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_sub_y_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_sub_z_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_sub_w_vf(const goos::Object& form, const goos::Object& rest, Env* env);

  Val* compile_asm_add_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_add_x_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_add_y_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_add_z_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_add_w_vf(const goos::Object& form, const goos::Object& rest, Env* env);

  Val* compile_asm_mul_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_mul_x_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_mul_y_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_mul_z_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_mul_w_vf(const goos::Object& form, const goos::Object& rest, Env* env);

  Val* compile_asm_mul_add_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_mul_add_x_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_mul_add_y_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_mul_add_z_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_mul_add_w_vf(const goos::Object& form, const goos::Object& rest, Env* env);

  Val* compile_asm_mul_sub_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_mul_sub_x_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_mul_sub_y_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_mul_sub_z_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_mul_sub_w_vf(const goos::Object& form, const goos::Object& rest, Env* env);

  Val* compile_asm_abs_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_outer_product_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_outer_product_a_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_outer_product_b_vf(const goos::Object& form, const goos::Object& rest, Env* env);

  Val* compile_asm_div_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_sqrt_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_inv_sqrt_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_itof_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_ftoi_vf(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_pw_sll(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_pw_srl(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_pw_sra(const goos::Object& form, const goos::Object& rest, Env* env);

  Val* compile_asm_por(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_pxor(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_pnor(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_pand(const goos::Object& form, const goos::Object& rest, Env* env);

  Val* compile_asm_pceqb(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_pceqh(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_pceqw(const goos::Object& form, const goos::Object& rest, Env* env);

  Val* compile_asm_pcgtb(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_pcgth(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_pcgtw(const goos::Object& form, const goos::Object& rest, Env* env);

  Val* compile_asm_pextub(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_pextuh(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_pextuw(const goos::Object& form, const goos::Object& rest, Env* env);

  Val* compile_asm_pextlb(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_pextlh(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_pextlw(const goos::Object& form, const goos::Object& rest, Env* env);

  Val* compile_asm_paddb(const goos::Object& form, const goos::Object& rest, Env* env);

  Val* compile_asm_pcpyud(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_pcpyld(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_ppach(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_ppacb(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_psubw(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_xorp(const goos::Object& form, const goos::Object& rest, Env* env);

  // Atoms

  // Block
  Val* compile_begin(const goos::Object& form, const goos::Object& rest, Env* env);
  ConstPropResult const_prop_begin(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_top_level(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_block(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_return_from(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_label(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_goto(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_nop(const goos::Object& form, const goos::Object& rest, Env* env);

  // CompilerControl
  Val* compile_seval(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_exit(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_file(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_repl_clear_screen(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_data_file(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_asm_text_file(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_repl_help(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_repl_keybinds(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_listen_to_target(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_reset_target(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_poke(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_gs(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_set_config(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_in_package(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_bundles(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_require(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_build_dgo(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_reload(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_get_info(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_autocomplete(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_update_macro_metadata(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_load_project(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_make(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_print_debug_compiler_stats(const goos::Object& form,
                                          const goos::Object& rest,
                                          Env* env);
  Val* compile_gen_docs(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_export_requires(const goos::Object& form, const goos::Object& rest, Env*);

  // ControlFlow
  Condition compile_condition(const goos::Object& condition, Env* env, bool invert);
  Val* compile_condition_as_bool(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_when_goto(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_cond(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_and_or(const goos::Object& form, const goos::Object& rest, Env* env);

  // Define
  Val* compile_define(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_define_extern(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_set(const goos::Object& form, const goos::Object& rest, Env* env);

  // Debug
  Val* compile_dbg(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_dbg_and_continue(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_dbs(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_break(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_cont(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_stop(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_dump_all(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_pm(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_di(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_disasm(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_bp(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_ubp(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_d_sym_name(const goos::Object& form, const goos::Object& rest, Env* env);
  u32 parse_address_spec(const goos::Object& form);

  // Macro
  Val* compile_gscond(const goos::Object& form, const goos::Object& rest, Env* env);
  ConstPropResult const_prop_gscond(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_quote(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_defglobalconstant(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_defconstant(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_mlet(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_macro_expand(const goos::Object& form, const goos::Object& rest, Env* env);

  // Math
  Val* compile_add(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_sub(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_mul(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_imul64(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_div(const goos::Object& form, const goos::Object& rest, Env* env);
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
  Val* compile_sqrtf(const goos::Object& form, const goos::Object& rest, Env* env);

  // Function
  Val* compile_lambda(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_inline(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_declare(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_local_vars(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_declare_file(const goos::Object& form, const goos::Object& rest, Env* env);

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
  Val* compile_method_of_type(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_method_id_of_type(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_method_of_object(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_addr_of(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_declare_type(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_none(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_defenum(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_size_of(const goos::Object& form, const goos::Object& rest, Env* env);
  ConstPropResult const_prop_size_of(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_psize_of(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_current_method_id(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_current_method_type(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_cast_to_method_type(const goos::Object& form, const goos::Object& rest, Env* env);

  // State
  Val* compile_define_state_hook(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_define_virtual_state_hook(const goos::Object& form,
                                         const goos::Object& rest,
                                         Env* env);
  Val* compile_go_hook(const goos::Object& form, const goos::Object& rest, Env* env);
  Val* compile_gc_text(const goos::Object& form, const goos::Object& rest, Env* env);
};

extern const std::unordered_map<
    std::string,
    std::pair<std::string,
              Val* (Compiler::*)(const goos::Object& form, const goos::Object& rest, Env* env)>>
    g_goal_forms;
