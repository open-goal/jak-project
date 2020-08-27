#ifndef JAK_GOAL_H
#define JAK_GOAL_H

#include <functional>
#include "listener/Listener.h"
#include "goos/Goos.h"
#include "GoalEnv.h"
#include "TypeContainer.h"
#include "GoalEnum.h"

enum MathMode { MATH_INT, MATH_BINT, MATH_FLOAT, MATH_INVALID };

enum LogOpKind { LOGIOR, LOGAND, LOGXOR };

struct StructFieldDefinition {
  enum PrintType { DECIMAL, HEX, DONT_PRINT, DEFAULT };

  std::string name;  //, type;
  TypeSpec type;
  int array_size = 0;
  int offset_override = -1;
  int offset_assert = -1;
  bool is_inline = false;
  bool is_dynamic = false;
  PrintType printy_type = DEFAULT;
};

struct BitFieldDefinition {
  std::string name;
  TypeSpec type;
  int offset_override = -1;
  int offset_assert = -1;
  int size = -1;
};

struct CompilerConfigEntry {
  std::string name;
  Object value;
};

constexpr int BITS_PER_BYTE = 8;

class Goal {
 public:
  // GOAL
  Goal();
  void execute_repl();
  ~Goal();

  // ASM
  std::shared_ptr<Place> compile_asm(const Object& form, Object rest, std::shared_ptr<GoalEnv> env);

  // BLOCK
  std::shared_ptr<Place> compile_top_level(const Object& form,
                                           Object rest,
                                           std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_begin(const Object& form,
                                       Object rest,
                                       std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_block(const Object& form,
                                       Object rest,
                                       std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_return_from(const Object& form,
                                             Object rest,
                                             std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_label(const Object& form,
                                       Object rest,
                                       std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_goto(const Object& form,
                                      Object rest,
                                      std::shared_ptr<GoalEnv> env);

  // ATOMS
  bool try_getting_macro_from_goos(Object macro_name, Object* dest);
  std::shared_ptr<Place> compile_goos_macro(Object form,
                                            Object macro,
                                            Object rest,
                                            std::shared_ptr<GoalEnv> env);

  // compiler control
  std::shared_ptr<Place> compile_gs(const Object& form, Object rest, std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_exit(const Object& form,
                                      Object rest,
                                      std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_asm_file(const Object& form,
                                          Object rest,
                                          std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_test(const Object& form,
                                      Object rest,
                                      std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_in_package(const Object& form,
                                            Object rest,
                                            std::shared_ptr<GoalEnv> env);

  // conditional
  std::shared_ptr<Place> compile_gscond(const Object& form,
                                        Object rest,
                                        std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_seval(const Object& form,
                                       Object rest,
                                       std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_defglobalconstant(const Object& form,
                                                   Object rest,
                                                   std::shared_ptr<GoalEnv> env);

  // flow of control
  GoalCondition compile_condition(Object condition, std::shared_ptr<GoalEnv> env, bool invert);
  std::shared_ptr<Place> compile_condition_as_bool(const Object& form,
                                                   Object rest,
                                                   std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_cond(const Object& form,
                                      Object rest,
                                      std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_when_goto(const Object& form,
                                           Object rest,
                                           std::shared_ptr<GoalEnv> env);

  // define
  std::shared_ptr<Place> compile_define(const Object& form,
                                        Object rest,
                                        std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_define_extern(const Object& form,
                                               Object rest,
                                               std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_set(const Object& form, Object rest, std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_defun_extern(const Object& form,
                                              Object rest,
                                              std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_declare_method(const Object& form,
                                                Object rest,
                                                std::shared_ptr<GoalEnv> env);

  // deftype
  std::shared_ptr<Place> compile_deftype(const Object& form,
                                         Object rest,
                                         std::shared_ptr<GoalEnv> env);

  // ENUM
  std::shared_ptr<Place> compile_defenum(const Object& form,
                                         Object rest,
                                         std::shared_ptr<GoalEnv> env);

  // Field Access
  std::shared_ptr<Place> compile_deref(const Object& form,
                                       Object rest,
                                       std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_addr_of(const Object& form,
                                         Object rest,
                                         std::shared_ptr<GoalEnv> env);

  // function
  std::shared_ptr<Place> compile_inline(const Object& form,
                                        Object rest,
                                        std::shared_ptr<GoalEnv> env);

  std::shared_ptr<Place> compile_with_inline(const Object& form,
                                             Object rest,
                                             std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_rlet(const Object& form,
                                      Object rest,
                                      std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_declare(const Object& form,
                                         Object rest,
                                         std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_mlet(const Object& form,
                                      Object rest,
                                      std::shared_ptr<GoalEnv> env);

  std::shared_ptr<Place> compile_get_ra_ptr(const Object& form,
                                            Object rest,
                                            std::shared_ptr<GoalEnv> env);

  // pair

  std::shared_ptr<Place> compile_lambda(const Object& form,
                                        Object rest,
                                        std::shared_ptr<GoalEnv> env);

  std::shared_ptr<Place> compile_car(const Object& form, Object rest, std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_cdr(const Object& form, Object rest, std::shared_ptr<GoalEnv> env);

  // macro
  std::shared_ptr<Place> compile_print_type(const Object& form,
                                            Object rest,
                                            std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_quote(const Object& form,
                                       Object rest,
                                       std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_current_method_type(const Object& form,
                                                     Object rest,
                                                     std::shared_ptr<GoalEnv> env);

  std::shared_ptr<Place> compile_defconstant(const Object& form,
                                             Object rest,
                                             std::shared_ptr<GoalEnv> env);

  // compare

  // object

  std::shared_ptr<Place> compile_defmethod(const Object& form,
                                           Object rest,
                                           std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_new(const Object& form, Object rest, std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_make_static_object_of_type(const Object& form,
                                                            TypeSpec& type,
                                                            Object field_defs,
                                                            std::shared_ptr<GoalEnv> env);

  std::shared_ptr<Place> compile_method(const Object& form,
                                        Object rest,
                                        std::shared_ptr<GoalEnv> env);

  // integer math
  std::shared_ptr<Place> compile_add(const Object& form, Object rest, std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_sub(const Object& form, Object rest, std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_mult(const Object& form,
                                      Object rest,
                                      std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_divide(const Object& form,
                                        Object rest,
                                        std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_shlv(const Object& form,
                                      Object rest,
                                      std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_shrv(const Object& form,
                                      Object rest,
                                      std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_sarv(const Object& form,
                                      Object rest,
                                      std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_shl(const Object& form, Object rest, std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_shr(const Object& form, Object rest, std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_sar(const Object& form, Object rest, std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_mod(const Object& form, Object rest, std::shared_ptr<GoalEnv> env);

  std::shared_ptr<Place> compile_logior(const Object& form,
                                        Object rest,
                                        std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_logand(const Object& form,
                                        Object rest,
                                        std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_logxor(const Object& form,
                                        Object rest,
                                        std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_lognot(const Object& form,
                                        Object rest,
                                        std::shared_ptr<GoalEnv> env);

  std::shared_ptr<Place> compile_set_config(const Object& form,
                                            Object rest,
                                            std::shared_ptr<GoalEnv> env);
  // BUILDER
  std::shared_ptr<Place> compile_builder(const Object& form,
                                         Object rest,
                                         std::shared_ptr<GoalEnv> env);

  std::shared_ptr<Place> compile_the(const Object& form, Object rest, std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_the_as(const Object& form,
                                        Object rest,
                                        std::shared_ptr<GoalEnv> env);

  std::shared_ptr<Place> compile_listen_to_target(const Object& form,
                                                  Object rest,
                                                  std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_reset_target(const Object& form,
                                              Object rest,
                                              std::shared_ptr<GoalEnv> env);

  std::shared_ptr<Place> compile_poke(const Object& form,
                                      Object rest,
                                      std::shared_ptr<GoalEnv> env);

  std::shared_ptr<Place> compile_send_test_data(const Object& form,
                                                Object rest,
                                                std::shared_ptr<GoalEnv> env);

  std::shared_ptr<Place> compile_get_method_of_type(TypeSpec type,
                                                    const std::string& name,
                                                    std::shared_ptr<GoalEnv> env);

  std::shared_ptr<Place> compile_get_method_of_object(std::shared_ptr<Place> object,
                                                      const std::string& method_name,
                                                      std::shared_ptr<GoalEnv> env);

  // UTIL
  void for_each_in_list(Object list, const std::function<void(Object)>& f);
  int list_length(Object list);
  Object get_constant_or_error(Object error_form, const std::string& name);
  SymbolTable& get_symbol_table();
  std::shared_ptr<StringObject> as_string_obj(Object obj);
  std::string as_string(Object obj);
  std::string quoted_sym_as_string(Object obj);
  std::vector<std::string> as_string_list(Object obj);
  std::shared_ptr<PairObject> as_pair_obj(Object obj);
  Object pair_car(Object obj);
  Object pair_cdr(Object obj);
  void expect_empty_list(Object obj);
  std::shared_ptr<SymbolObject> as_symbol_obj(Object obj);
  std::string symbol_string(Object obj);

  bool write_to_binary_file(const std::string& name, void* data, uint32_t size);

 private:
  // GOAL
  static void init_logger();
  std::shared_ptr<ObjectFileEnv> compile_object_file(const std::string& name, const Object& code);
  std::shared_ptr<FunctionEnv> compile_top_level_function(const std::string& name,
                                                          const Object& code,
                                                          std::shared_ptr<ObjectFileEnv> object);
  void color_function(const std::shared_ptr<FunctionEnv>& func);
  void color_object_file(const std::shared_ptr<ObjectFileEnv>& obj);
  std::vector<uint8_t> codegen_object_file(const std::shared_ptr<ObjectFileEnv>& obj);
  std::shared_ptr<Place> compile_error_guard(Object obj, std::shared_ptr<GoalEnv> env);
  void throw_compile_error(Object o, const std::string& err);

  // COMPILE ATOMS
  std::shared_ptr<Place> compile(Object obj, std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_pair(Object o, std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_integer(const Object& form, std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_integer(int64_t value, std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_integer_to_gpr(int64_t value, std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_get_sym_val(const std::string& name, std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_get_sym_obj(const std::string& name, std::shared_ptr<GoalEnv> env);
  bool is_local_symbol(Object obj, std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_symbol(const Object& form, std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_string(const Object& form, std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_string(const std::string& str, std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_float(const Object& form, std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_float(float value, std::shared_ptr<GoalEnv> env);

  // CONST PROP
  std::shared_ptr<Place> resolve_bitfield_to_gpr(std::shared_ptr<BitfieldPlace> in,
                                                 std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> set_bitfield(std::shared_ptr<BitfieldPlace> dest,
                                      std::shared_ptr<Place> value,
                                      std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> try_resolve_bitfield_to_gpr(std::shared_ptr<Place> in,
                                                     std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> try_resolve_static_to_gpr(std::shared_ptr<Place> in,
                                                   std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> try_resolve_lambda_to_gpr(std::shared_ptr<Place> in,
                                                   std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> try_resolve_xmm_to_gpr(std::shared_ptr<Place> in,
                                                std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> try_resolve_mem_to_gpr(std::shared_ptr<Place> in,
                                                std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> try_resolve_static_to_xmm(std::shared_ptr<Place> in,
                                                   std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> try_resolve_gpr_to_xmm(std::shared_ptr<Place> in,
                                                std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> try_resolve_integer_to_gpr(std::shared_ptr<Place> in,
                                                    std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> resolve_to_gpr(std::shared_ptr<Place> in, std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> resolve_to_xmm(std::shared_ptr<Place> in, std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> resolve_to_gpr_or_xmm(std::shared_ptr<Place> in,
                                               std::shared_ptr<GoalEnv> env);
  int64_t compile_to_integer_constant(Object form, std::shared_ptr<GoalEnv> env);
  bool try_converting_to_integer_constant(Place& in, int64_t* out);
  std::shared_ptr<Place> addr_of(std::shared_ptr<Place> plc, std::shared_ptr<GoalEnv> env);

  // DEFTYPE
  StructFieldDefinition parse_struct_field_def(const Object& def);
  BitFieldDefinition parse_bit_field_def(const Object& def, std::shared_ptr<GoalEnv> env);
  int get_size_in_type(GoalField& f);
  std::shared_ptr<Place> deftype_structure(std::shared_ptr<StructureType> new_type,
                                           Object fields,
                                           Object options,
                                           std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> deftype_bitfield(std::shared_ptr<BitfieldType> new_type,
                                          Object fields,
                                          Object options,
                                          std::shared_ptr<GoalEnv> env);
  void deftype_methods_helper(Object form,
                              std::shared_ptr<GoalType> new_type,
                              std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> deftype_call_new_method_of_type(Object form,
                                                         std::shared_ptr<GoalType> new_type,
                                                         uint64_t flags,
                                                         std::shared_ptr<GoalEnv> env);

  // Enum
  std::shared_ptr<Place> compile_enum_lookup(GoalEnum& e,
                                             Object rest,
                                             std::shared_ptr<GoalEnv> env);

  // Function
  std::shared_ptr<Place> compile_real_function_call(const Object& form,
                                                    std::shared_ptr<Place> function,
                                                    std::vector<std::shared_ptr<Place>> args,
                                                    std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> compile_function_or_method_call(const Object& form,
                                                         std::shared_ptr<GoalEnv> env);

  // READER UTIL
  Object read_from_file(const std::string& file_name);
  Object read_from_stdin_prompt(const std::string& prompt_name);
  Object read_from_string(const std::string& str);

  // CONFIG UTIL
  Object get_config(const std::string& name);
  void set_config(const std::string& name, const Object& value);
  void setup_default_config();

  // ERROR UTIL
  void ice(const std::string& error);

  // MAIN DRIVERS

  // NUMERIC UTIL
  std::shared_ptr<Place> to_integer(std::shared_ptr<Place> in, std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> to_binteger(std::shared_ptr<Place> in, std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> to_float(std::shared_ptr<Place> in, std::shared_ptr<GoalEnv> env);
  bool is_signed_integer(TypeSpec& ts);
  bool is_integer(TypeSpec& ts);
  bool is_number(TypeSpec& ts);
  bool is_binteger(TypeSpec& ts);
  bool is_float(TypeSpec& ts);
  std::shared_ptr<Place> to_same_numeric_type(std::shared_ptr<Place> obj,
                                              TypeSpec numeric_type,
                                              std::shared_ptr<GoalEnv> env);

  // TYPE UTIL
  void typecheck_base_only(const Object& form,
                           TypeSpec& destination_type,
                           TypeSpec& source_type,
                           const std::string& error);

  void typecheck_for_set(const Object& form,
                         TypeSpec& destination_type,
                         TypeSpec& source_type,
                         const std::string& error);

  TypeSpec get_base_typespec(const std::string& name);
  TypeSpec compile_typespec(const Object& form);

  TypeSpec get_base_of_inline_array(TypeSpec ts);
  TypeSpec get_base_of_pointer(TypeSpec ts);
  std::shared_ptr<Place> compile_logop(const Object& form,
                                       Object rest,
                                       std::shared_ptr<GoalEnv> env,
                                       LogOpKind kind);

  bool is_basic(TypeSpec& ts);

  std::vector<std::shared_ptr<GoalType>> get_parents(std::shared_ptr<GoalType>);
  TypeSpec lowest_common_ancestor(TypeSpec a, TypeSpec b);
  TypeSpec lowest_common_ancestor(std::vector<TypeSpec> ts);

  MathMode get_math_mode(TypeSpec& ts);

  void typecheck_full(const Object& form,
                      TypeSpec& destination_type,
                      TypeSpec& source_type,
                      const std::string& error);

  ColoringAssignment reg_name_to_ca(Object& name);

  bool is_none(std::shared_ptr<Place> pl) {
    return std::dynamic_pointer_cast<NonePlace>(pl) != nullptr;
  }

  static bool truthy(Object o) {
    if (o.type == SYMBOL && o.as_symbol()->name == "#f")
      return false;
    if (o.type == EMPTY_LIST)
      return false;  // debatable if this is ok.
    return true;
  }

  std::shared_ptr<Place> compile_shift(const Object& form,
                                       Object rest,
                                       std::shared_ptr<GoalEnv> env,
                                       bool is_left,
                                       bool is_arith);
  std::shared_ptr<Place> compile_shift(std::shared_ptr<Place> in,
                                       std::shared_ptr<Place> sa,
                                       std::shared_ptr<GoalEnv> env,
                                       bool is_left,
                                       bool is_arith);

  std::shared_ptr<Place> compile_fixed_shift(const Object& form,
                                             Object rest,
                                             std::shared_ptr<GoalEnv> env,
                                             bool is_left,
                                             bool is_arith);
  std::shared_ptr<Place> compile_fixed_shift(std::shared_ptr<Place> in,
                                             uint8_t sa,
                                             std::shared_ptr<GoalEnv> env,
                                             bool is_left,
                                             bool is_arith);

  std::shared_ptr<LexicalEnv> make_rlet_env(Object defs, std::shared_ptr<GoalEnv> env);

  void set_symbol_type(const std::string& name, TypeSpec t) {
    symbol_types[SymbolObject::make_new(goos.reader.symbolTable, name).as_symbol()] = t;
  }

  void generate_inspector_format_call(const std::string& format,
                                      std::vector<std::shared_ptr<Place>> args,
                                      std::shared_ptr<GoalEnv> env);
  std::shared_ptr<Place> generate_inspector_for_type(std::shared_ptr<StructureType> type,
                                                     std::shared_ptr<GoalEnv> env);

  Goos goos;
  Listener listener;
  bool want_exit = false;
  std::shared_ptr<GoalEnv> global_env;

  TypeContainer types;

  std::unordered_map<std::shared_ptr<SymbolObject>, Object> global_constants;
  std::unordered_map<std::shared_ptr<SymbolObject>, TypeSpec> symbol_types;
  std::unordered_map<std::shared_ptr<SymbolObject>, std::shared_ptr<LambdaPlace>>
      inlineable_functions;
  std::unordered_map<std::string, GoalEnum> enums;
  std::unordered_map<std::string, CompilerConfigEntry> config_data;
};

#endif  // JAK_GOAL_H
