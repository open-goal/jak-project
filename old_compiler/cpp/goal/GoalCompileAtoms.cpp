/*!
 * @file GoalCompileAtoms.cpp
 * The top-level dispatch of the compilation process.
 */

#include <util.h>
#include "Goal.h"

/*!
 * Main table for compiler forms
 */
static const std::unordered_map<
    std::string,
    std::shared_ptr<Place> (Goal::*)(const Object& form, Object rest, std::shared_ptr<GoalEnv> env)>
    goal_forms = {
        // inline asm
        {".ret", &Goal::compile_asm},
        {".push", &Goal::compile_asm},
        {".pop", &Goal::compile_asm},
        {".jmp", &Goal::compile_asm},
        {".sub", &Goal::compile_asm},
        {".ret-reg", &Goal::compile_asm},

        // BLOCK FORMS
        {"top-level", &Goal::compile_top_level},
        {"begin", &Goal::compile_begin},
        {"block", &Goal::compile_block},
        {"return-from", &Goal::compile_return_from},
        {"label", &Goal::compile_label},
        {"goto", &Goal::compile_goto},

        // COMPILER CONTROL
        {"gs", &Goal::compile_gs},
        {":exit", &Goal::compile_exit},
        {"asm-file", &Goal::compile_asm_file},
        {"test", &Goal::compile_test},
        {"in-package", &Goal::compile_in_package},

        // CONDITIONAL COMPILATION
        {"#cond", &Goal::compile_gscond},
        {"defglobalconstant", &Goal::compile_defglobalconstant},
        {"seval", &Goal::compile_seval},

        // CONTROL FLOW
        {"cond", &Goal::compile_cond},
        {"when-goto", &Goal::compile_when_goto},

        // DEFINITION
        {"define", &Goal::compile_define},
        {"define-extern", &Goal::compile_define_extern},
        {"set!", &Goal::compile_set},
        {"defun-extern", &Goal::compile_defun_extern},
        {"declare-method", &Goal::compile_declare_method},

        // DEFTYPE
        {"deftype", &Goal::compile_deftype},

        // ENUM
        {"defenum", &Goal::compile_defenum},

        // Field Access
        {"->", &Goal::compile_deref},
        {"&", &Goal::compile_addr_of},

        // LAMBDA
        {"lambda", &Goal::compile_lambda},
        {"inline", &Goal::compile_inline},
        {"with-inline", &Goal::compile_with_inline},
        {"rlet", &Goal::compile_rlet},
        {"mlet", &Goal::compile_mlet},
        {"get-ra-ptr", &Goal::compile_get_ra_ptr},

        // MACRO
        {"print-type", &Goal::compile_print_type},
        {"quote", &Goal::compile_quote},
        {"defconstant", &Goal::compile_defconstant},

        {"declare", &Goal::compile_declare},

        // OBJECT

        {"the", &Goal::compile_the},
        {"the-as", &Goal::compile_the_as},

        {"defmethod", &Goal::compile_defmethod},

        {"current-method-type", &Goal::compile_current_method_type},
        {"new", &Goal::compile_new},
        {"method", &Goal::compile_method},

        // PAIR
        {"car", &Goal::compile_car},
        {"cdr", &Goal::compile_cdr},

        // IT IS MATH
        {"+", &Goal::compile_add},
        {"-", &Goal::compile_sub},
        {"*", &Goal::compile_mult},
        {"/", &Goal::compile_divide},
        {"shlv", &Goal::compile_shlv},
        {"shrv", &Goal::compile_shrv},
        {"sarv", &Goal::compile_sarv},
        {"shl", &Goal::compile_shl},
        {"shr", &Goal::compile_shr},
        {"sar", &Goal::compile_sar},
        {"mod", &Goal::compile_mod},
        {"logior", &Goal::compile_logior},
        {"logxor", &Goal::compile_logxor},
        {"logand", &Goal::compile_logand},
        {"lognot", &Goal::compile_lognot},
        {"=", &Goal::compile_condition_as_bool},
        {"!=", &Goal::compile_condition_as_bool},
        {"eq?", &Goal::compile_condition_as_bool},
        {"not", &Goal::compile_condition_as_bool},
        {"<=", &Goal::compile_condition_as_bool},
        {">=", &Goal::compile_condition_as_bool},
        {"<", &Goal::compile_condition_as_bool},
        {">", &Goal::compile_condition_as_bool},

        // BUILDER
        //    {"builder", &Goal::compile_builder},

        // UTIL
        {"set-config!", &Goal::compile_set_config},

        {"listen-to-target", &Goal::compile_listen_to_target},
        {"reset-target", &Goal::compile_reset_target},
        {":status", &Goal::compile_poke},

        // temporary testing hacks...
        {"send-test", &Goal::compile_send_test_data},
};

/*!
 * Top Level dispatch for compilation of code.
 */
std::shared_ptr<Place> Goal::compile(Object obj, std::shared_ptr<GoalEnv> env) {
  switch (obj.type) {
    case PAIR:
      return compile_pair(obj, env);
    case INTEGER:
      return compile_integer(obj, env);
    case SYMBOL:
      return compile_symbol(obj, env);
    case STRING:
      return compile_string(obj, env);
    case FLOAT:
      return compile_float(obj, env);

    default:
      ice("Goal::compile does not know how to compile " + obj.print());
      return get_none();
  }
}

/*!
 * Compile function for pair object
 */
std::shared_ptr<Place> Goal::compile_pair(Object o, std::shared_ptr<GoalEnv> env) {
  auto pair = o.as_pair();
  Object head = pair->car;
  Object rest = pair->cdr;

  if (head.type == SYMBOL) {
    // head is a symbol, so this may be a special form.
    auto head_sym = head.as_symbol();

    // first try as a goal compiler form
    auto kv_gfs = goal_forms.find(head_sym->name);
    if (kv_gfs != goal_forms.end()) {
      return ((*this).*(kv_gfs->second))(o, rest, env);
    }

    // next try to find a macro defined in the goal_goos_env (containing GOOS macros to be used from
    // GOAL)
    Object macro_obj;
    if (try_getting_macro_from_goos(head, &macro_obj)) {
      return compile_goos_macro(o, macro_obj, rest, env);
    }

    // next try as an enum
    auto enum_kv = enums.find(symbol_string(head));
    if (enum_kv != enums.end()) {
      return compile_enum_lookup(enum_kv->second, rest, env);
    }
  }

  // didn't recognize it as anything special, try it as a function or method call
  return compile_function_or_method_call(o, env);
}

/*!
 * Compile an integer.
 */
std::shared_ptr<Place> Goal::compile_integer(const Object& form, std::shared_ptr<GoalEnv> env) {
  assert(form.type == INTEGER);
  int64_t value = form.integer_obj.value;
  return compile_integer(value, env);
}

/*!
 * Compile an integer.
 */
std::shared_ptr<Place> Goal::compile_integer(int64_t value, std::shared_ptr<GoalEnv> env) {
  (void)env;
  // simply return an integer constant.
  return std::make_shared<IntegerConstantPlace>(get_base_typespec("integer"), value);
}

/*!
 * Compile an integer, but force it to be in a GPR instead of an integer constant.
 * This is can be used to save typing out resolve_to_gpr(compile_to_integer(...)) for getting
 * integers in gprs.
 */
std::shared_ptr<Place> Goal::compile_integer_to_gpr(int64_t value, std::shared_ptr<GoalEnv> env) {
  auto ir = make_unique<IR_LoadInteger>();

  ir->s_value = value;
  ir->is_signed = true;
  if (value >= INT8_MIN) {
    ir->size = 1;
  } else if (value >= INT16_MIN) {
    ir->size = 2;
  } else if (value >= INT32_MIN) {
    ir->size = 4;
  } else {
    ir->size = 8;
  }

  ir->value = env->alloc_reg(get_base_typespec("integer"));
  auto result = ir->value;
  env->emit(std::move(ir));
  return result;
}

/*!
 * Get the value of a global symbol by name.
 */
std::shared_ptr<Place> Goal::compile_get_sym_val(const std::string& name,
                                                 std::shared_ptr<GoalEnv> env) {
  auto existing_symbol =
      symbol_types.find(SymbolObject::make_new(goos.reader.symbolTable, name).as_symbol());
  if (existing_symbol == symbol_types.end()) {
    throw std::runtime_error("The symbol " + name + " was not defined");
  }
  auto ir = make_unique<IR_GetSymbolValue>();
  TypeSpec symbol_type = get_base_typespec("symbol");
  ir->symbol = std::make_shared<SymbolPlace>(name, symbol_type);
  // make sure to type the result correctly.
  ir->dest = env->alloc_reg(existing_symbol->second);
  if (is_signed_integer(existing_symbol->second)) {
    ir->sext = true;
  }
  auto result = ir->dest;
  env->emit(std::move(ir));
  return result;
}

/*!
 * Get a global symbol object by name.
 */
std::shared_ptr<Place> Goal::compile_get_sym_obj(const std::string& name,
                                                 std::shared_ptr<GoalEnv> env) {
  auto ir = make_unique<IR_GetSymbolObj>();
  TypeSpec symbol_type = get_base_typespec("symbol");
  ir->sym = std::make_shared<SymbolPlace>(name, symbol_type);
  ir->dest = env->alloc_reg(symbol_type);
  auto dest = ir->dest;
  env->emit(std::move(ir));
  return dest;
}

/*!
 * Does some local environment (mlet, lexical, or global constants) provide a value for this symbol?
 * This is used to determine if a function/method call should be tried as a method call.
 * Method names win over global symbols, but lose to everything else.
 */
bool Goal::is_local_symbol(Object obj, std::shared_ptr<GoalEnv> env) {
  // check in the symbol macro env.
  auto mlet_env = get_parent_env_of_type<SymbolMacroEnv>(env);
  while (mlet_env) {
    if (mlet_env->macros.find(as_symbol_obj(obj)) != mlet_env->macros.end()) {
      return true;
    }
    mlet_env = get_parent_env_of_type<SymbolMacroEnv>(mlet_env->parent);
  }

  // check lexical
  if (env->lexical_lookup(obj))
    return true;

  // check global constants
  if (global_constants.find(as_symbol_obj(obj)) != global_constants.end())
    return true;

  return false;
}

/*!
 * Compile a symbol
 */
std::shared_ptr<Place> Goal::compile_symbol(const Object& form, std::shared_ptr<GoalEnv> env) {
  // special case for the none symbol.
  if (form.as_symbol()->name == "none") {
    return get_none();
  }

  // first try as a constant defined in a macro env (mlet)
  auto mlet_env = get_parent_env_of_type<SymbolMacroEnv>(env);
  while (mlet_env) {
    auto mlkv = mlet_env->macros.find(form.as_symbol());
    if (mlkv != mlet_env->macros.end()) {
      return compile_error_guard(mlkv->second, env);
    }
    mlet_env = get_parent_env_of_type<SymbolMacroEnv>(mlet_env->parent);
  }

  // try lexical lookup
  auto lexical = env->lexical_lookup(form);
  if (lexical) {
    return lexical;
  }

  // try as a global constant (defglobalconstant), but make sure we don't have a symbol with the
  // same name
  auto global_constant = global_constants.find(form.as_symbol());
  auto existing_symbol = symbol_types.find(form.as_symbol());

  if (global_constant != global_constants.end()) {
    // check there is no symbol with the same name
    if (existing_symbol != symbol_types.end()) {
      throw_compile_error(form,
                          "symbol is both a runtime symbol and a global constant.  Something is "
                          "likely very wrong.");
    }

    // got a global constant
    return compile_error_guard(global_constant->second, env);
  }

  // no global constant, make sure we got a global symbol
  if (existing_symbol == symbol_types.end()) {
    throw_compile_error(form, "The symbol " + symbol_string(form) + " was not defined");
  }

  // finally as a global symbol
  return compile_get_sym_val(form.as_symbol()->name, env);
}

/*!
 * Compile a string
 */
std::shared_ptr<Place> Goal::compile_string(const Object& form, std::shared_ptr<GoalEnv> env) {
  return compile_string(as_string(form), env);
}

/*!
 * Compile a string
 */
std::shared_ptr<Place> Goal::compile_string(const std::string& in, std::shared_ptr<GoalEnv> env) {
  auto str = std::make_shared<StaticString>();
  str->data = in;
  str->segment = get_parent_env_of_type<FunctionEnv>(env)->segment;
  auto result = std::make_shared<StaticPlace>(get_base_typespec("string"), str);
  env->get_statics().push_back(result);
  return result;
}

/*!
 * Compile a float
 */
std::shared_ptr<Place> Goal::compile_float(float value, std::shared_ptr<GoalEnv> env) {
  auto flt = std::make_shared<StaticFloat>();
  flt->as_float = value;
  flt->segment = get_parent_env_of_type<FunctionEnv>(env)->segment;
  auto result = std::make_shared<StaticPlace>(get_base_typespec("float"), flt);
  env->get_statics().push_back(result);
  return result;
}

/*!
 * Compile a float
 */
std::shared_ptr<Place> Goal::compile_float(const Object& form, std::shared_ptr<GoalEnv> env) {
  return compile_float(form.float_obj.value, env);
}