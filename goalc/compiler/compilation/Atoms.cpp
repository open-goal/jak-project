#include "goalc/compiler/Compiler.h"
#include "goalc/compiler/IR.h"

/*!
 * Main table for compiler forms
 */
static const std::unordered_map<
    std::string,
    Val* (Compiler::*)(const goos::Object& form, const goos::Object& rest, Env* env)>
    goal_forms = {
        //        // inline asm
        //        {".ret", &Compiler::compile_asm},
        //        {".push", &Compiler::compile_asm},
        //        {".pop", &Compiler::compile_asm},
        //        {".jmp", &Compiler::compile_asm},
        //        {".sub", &Compiler::compile_asm},
        //        {".ret-reg", &Compiler::compile_asm},
        //
        //        // BLOCK FORMS
        {"top-level", &Compiler::compile_top_level},
        {"begin", &Compiler::compile_begin},
        //        {"block", &Compiler::compile_block},
        //        {"return-from", &Compiler::compile_return_from},
        //        {"label", &Compiler::compile_label},
        //        {"goto", &Compiler::compile_goto},
        //
        //        // COMPILER CONTROL
        //        {"gs", &Compiler::compile_gs},
        {":exit", &Compiler::compile_exit}
        //        {"asm-file", &Compiler::compile_asm_file},
        //        {"test", &Compiler::compile_test},
        //        {"in-package", &Compiler::compile_in_package},
        //
        //        // CONDITIONAL COMPILATION
        //        {"#cond", &Compiler::compile_gscond},
        //        {"defglobalconstant", &Compiler::compile_defglobalconstant},
        //        {"seval", &Compiler::compile_seval},
        //
        //        // CONTROL FLOW
        //        {"cond", &Compiler::compile_cond},
        //        {"when-goto", &Compiler::compile_when_goto},
        //
        //        // DEFINITION
        //        {"define", &Compiler::compile_define},
        //        {"define-extern", &Compiler::compile_define_extern},
        //        {"set!", &Compiler::compile_set},
        //        {"defun-extern", &Compiler::compile_defun_extern},
        //        {"declare-method", &Compiler::compile_declare_method},
        //
        //        // DEFTYPE
        //        {"deftype", &Compiler::compile_deftype},
        //
        //        // ENUM
        //        {"defenum", &Compiler::compile_defenum},
        //
        //        // Field Access
        //        {"->", &Compiler::compile_deref},
        //        {"&", &Compiler::compile_addr_of},
        //
        //
        //        // LAMBDA
        //        {"lambda", &Compiler::compile_lambda},
        //        {"inline", &Compiler::compile_inline},
        //        {"with-inline", &Compiler::compile_with_inline},
        //        {"rlet", &Compiler::compile_rlet},
        //        {"mlet", &Compiler::compile_mlet},
        //        {"get-ra-ptr", &Compiler::compile_get_ra_ptr},
        //
        //
        //
        //        // MACRO
        //        {"print-type", &Compiler::compile_print_type},
        //        {"quote", &Compiler::compile_quote},
        //        {"defconstant", &Compiler::compile_defconstant},
        //
        //        {"declare", &Compiler::compile_declare},
        //
        //
        //
        //
        //        // OBJECT
        //
        //        {"the", &Compiler::compile_the},
        //        {"the-as", &Compiler::compile_the_as},
        //
        //        {"defmethod", &Compiler::compile_defmethod},
        //
        //        {"current-method-type", &Compiler::compile_current_method_type},
        //        {"new", &Compiler::compile_new},
        //        {"method", &Compiler::compile_method},
        //
        //        // PAIR
        //        {"car", &Compiler::compile_car},
        //        {"cdr", &Compiler::compile_cdr},
        //
        //        // IT IS MATH
        //        {"+", &Compiler::compile_add},
        //        {"-", &Compiler::compile_sub},
        //        {"*", &Compiler::compile_mult},
        //        {"/", &Compiler::compile_divide},
        //        {"shlv", &Compiler::compile_shlv},
        //        {"shrv", &Compiler::compile_shrv},
        //        {"sarv", &Compiler::compile_sarv},
        //        {"shl", &Compiler::compile_shl},
        //        {"shr", &Compiler::compile_shr},
        //        {"sar", &Compiler::compile_sar},
        //        {"mod", &Compiler::compile_mod},
        //        {"logior", &Compiler::compile_logior},
        //        {"logxor", &Compiler::compile_logxor},
        //        {"logand", &Compiler::compile_logand},
        //        {"lognot", &Compiler::compile_lognot},
        //        {"=", &Compiler::compile_condition_as_bool},
        //        {"!=", &Compiler::compile_condition_as_bool},
        //        {"eq?", &Compiler::compile_condition_as_bool},
        //        {"not", &Compiler::compile_condition_as_bool},
        //        {"<=", &Compiler::compile_condition_as_bool},
        //        {">=", &Compiler::compile_condition_as_bool},
        //        {"<", &Compiler::compile_condition_as_bool},
        //        {">", &Compiler::compile_condition_as_bool},
        //
        //        // BUILDER
        //        {"builder", &Compiler::compile_builder},
        //
        //        // UTIL
        //        {"set-config!", &Compiler::compile_set_config},
        //
        //
        //
        //        {"listen-to-target", &Compiler::compile_listen_to_target},
        //        {"reset-target", &Compiler::compile_reset_target},
        //        {":status", &Compiler::compile_poke},
        //
        //        // temporary testing hacks...
        //        {"send-test", &Compiler::compile_send_test_data},
};

Val* Compiler::compile(const goos::Object& code, Env* env) {
  switch (code.type) {
    case goos::ObjectType::PAIR:
      return compile_pair(code, env);
    case goos::ObjectType::INTEGER:
      return compile_integer(code, env);
    default:
      ice("Don't know how to compile " + code.print());
  }
  return get_none();
}

Val* Compiler::compile_pair(const goos::Object& code, Env* env) {
  auto pair = code.as_pair();
  auto head = pair->car;
  auto rest = pair->cdr;

  if (head.is_symbol()) {
    auto head_sym = head.as_symbol();
    // first try as a goal compiler form
    auto kv_gfs = goal_forms.find(head_sym->name);
    if (kv_gfs != goal_forms.end()) {
      return ((*this).*(kv_gfs->second))(code, rest, env);
    }

    // todo macro
    // todo enum
  }

  // todo function or method call
  ice("unhandled compile_pair on " + code.print());
  return nullptr;
}

Val* Compiler::compile_integer(const goos::Object& code, Env* env) {
  return compile_integer(code.integer_obj.value, env);
}

Val* Compiler::compile_integer(s64 value, Env* env) {
  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  return fe->alloc_val<IntegerConstantVal>(m_ts.make_typespec("int"), value);
}
