/*!
 * @file Atoms.cpp
 * Top-level compilation forms for each of the GOOS object types.
 */

#include "common/util/string_util.h"

#include "goalc/compiler/Compiler.h"
#include "goalc/compiler/IR.h"

/*!
 * Main table for compiler forms
 */
const std::unordered_map<
    std::string,
    std::pair<std::string,
              Val* (Compiler::*)(const goos::Object& form, const goos::Object& rest, Env* env)>>
    g_goal_forms = {
        // INLINE ASM
        {".nop", {"A simple no-op, does nothing", &Compiler::compile_nop}},
        {".ret", {"", &Compiler::compile_asm_ret}},
        {".push", {"", &Compiler::compile_asm_push}},
        {".pop", {"", &Compiler::compile_asm_pop}},
        {"rlet", {"", &Compiler::compile_rlet}},
        {".jr", {"", &Compiler::compile_asm_jr}},
        {".sub", {"", &Compiler::compile_asm_sub}},
        {".add", {"", &Compiler::compile_asm_add}},
        {".load-sym", {"", &Compiler::compile_asm_load_sym}},
        {".mov", {"", &Compiler::compile_asm_mov}},

        // INLINE ASM - VECTOR FLOAT OPERATIONS
        {".lvf", {"", &Compiler::compile_asm_lvf}},
        {".svf", {"", &Compiler::compile_asm_svf}},
        {".mov.vf", {"", &Compiler::compile_asm_mov_vf}},
        {".blend.vf", {"", &Compiler::compile_asm_blend_vf}},

        {".nop.vf", {"", &Compiler::compile_asm_nop_vf}},
        {".wait.vf", {"", &Compiler::compile_asm_wait_vf}},

        {".xor.vf", {"", &Compiler::compile_asm_xor_vf}},
        {".xor.p", {"", &Compiler::compile_asm_xorp}},

        {".max.vf", {"", &Compiler::compile_asm_max_vf}},
        {".max.x.vf", {"", &Compiler::compile_asm_max_x_vf}},
        {".max.y.vf", {"", &Compiler::compile_asm_max_y_vf}},
        {".max.z.vf", {"", &Compiler::compile_asm_max_z_vf}},
        {".max.w.vf", {"", &Compiler::compile_asm_max_w_vf}},

        {".min.vf", {"", &Compiler::compile_asm_min_vf}},
        {".min.x.vf", {"", &Compiler::compile_asm_min_x_vf}},
        {".min.y.vf", {"", &Compiler::compile_asm_min_y_vf}},
        {".min.z.vf", {"", &Compiler::compile_asm_min_z_vf}},
        {".min.w.vf", {"", &Compiler::compile_asm_min_w_vf}},

        {".add.vf", {"", &Compiler::compile_asm_add_vf}},
        {".add.x.vf", {"", &Compiler::compile_asm_add_x_vf}},
        {".add.y.vf", {"", &Compiler::compile_asm_add_y_vf}},
        {".add.z.vf", {"", &Compiler::compile_asm_add_z_vf}},
        {".add.w.vf", {"", &Compiler::compile_asm_add_w_vf}},

        {".sub.vf", {"", &Compiler::compile_asm_sub_vf}},
        {".sub.x.vf", {"", &Compiler::compile_asm_sub_x_vf}},
        {".sub.y.vf", {"", &Compiler::compile_asm_sub_y_vf}},
        {".sub.z.vf", {"", &Compiler::compile_asm_sub_z_vf}},
        {".sub.w.vf", {"", &Compiler::compile_asm_sub_w_vf}},

        {".mul.vf", {"", &Compiler::compile_asm_mul_vf}},
        {".mul.x.vf", {"", &Compiler::compile_asm_mul_x_vf}},
        {".mul.y.vf", {"", &Compiler::compile_asm_mul_y_vf}},
        {".mul.z.vf", {"", &Compiler::compile_asm_mul_z_vf}},
        {".mul.w.vf", {"", &Compiler::compile_asm_mul_w_vf}},

        {".add.mul.vf", {"", &Compiler::compile_asm_mul_add_vf}},
        {".add.mul.x.vf", {"", &Compiler::compile_asm_mul_add_x_vf}},
        {".add.mul.y.vf", {"", &Compiler::compile_asm_mul_add_y_vf}},
        {".add.mul.z.vf", {"", &Compiler::compile_asm_mul_add_z_vf}},
        {".add.mul.w.vf", {"", &Compiler::compile_asm_mul_add_w_vf}},

        {".sub.mul.vf", {"", &Compiler::compile_asm_mul_sub_vf}},
        {".sub.mul.x.vf", {"", &Compiler::compile_asm_mul_sub_x_vf}},
        {".sub.mul.y.vf", {"", &Compiler::compile_asm_mul_sub_y_vf}},
        {".sub.mul.z.vf", {"", &Compiler::compile_asm_mul_sub_z_vf}},
        {".sub.mul.w.vf", {"", &Compiler::compile_asm_mul_sub_w_vf}},

        {".abs.vf", {"", &Compiler::compile_asm_abs_vf}},
        // NOTE - to compute the Outer Product with the VU, two back to back instructions were used
        // involving the ACC
        // However, we can be better than that and just provide a single instruction
        // BUT - if things used side effects of the modified ACC or benefited from only doing 1/2
        // operations, we'll need to implement them separately.
        //
        // ...and they did
        {".outer.product.vf", {"", &Compiler::compile_asm_outer_product_vf}},
        {".outer.product.a.vf", {"", &Compiler::compile_asm_outer_product_a_vf}},
        {".outer.product.b.vf", {"", &Compiler::compile_asm_outer_product_b_vf}},

        {".div.vf", {"", &Compiler::compile_asm_div_vf}},
        {".sqrt.vf", {"", &Compiler::compile_asm_sqrt_vf}},
        {".isqrt.vf", {"", &Compiler::compile_asm_inv_sqrt_vf}},
        {".itof.vf", {"", &Compiler::compile_asm_itof_vf}},
        {".ftoi.vf", {"", &Compiler::compile_asm_ftoi_vf}},

        {".pw.sll", {"", &Compiler::compile_asm_pw_sll}},
        {".pw.srl", {"", &Compiler::compile_asm_pw_srl}},
        {".pw.sra", {"", &Compiler::compile_asm_pw_sra}},

        {".por", {"", &Compiler::compile_asm_por}},
        {".pxor", {"", &Compiler::compile_asm_pxor}},
        {".pnor", {"", &Compiler::compile_asm_pnor}},
        {".pand", {"", &Compiler::compile_asm_pand}},

        {".pceqb", {"", &Compiler::compile_asm_pceqb}},
        {".pceqh", {"", &Compiler::compile_asm_pceqh}},
        {".pceqw", {"", &Compiler::compile_asm_pceqw}},

        {".pcgtb", {"", &Compiler::compile_asm_pcgtb}},
        {".pcgth", {"", &Compiler::compile_asm_pcgth}},
        {".pcgtw", {"", &Compiler::compile_asm_pcgtw}},

        {".pextlb", {"", &Compiler::compile_asm_pextlb}},
        {".pextlh", {"", &Compiler::compile_asm_pextlh}},
        {".pextlw", {"", &Compiler::compile_asm_pextlw}},

        {".pextub", {"", &Compiler::compile_asm_pextub}},
        {".pextuh", {"", &Compiler::compile_asm_pextuh}},
        {".pextuw", {"", &Compiler::compile_asm_pextuw}},

        {".pcpyld", {"", &Compiler::compile_asm_pcpyld}},
        {".pcpyud", {"", &Compiler::compile_asm_pcpyud}},
        {".pceqw", {"", &Compiler::compile_asm_pceqw}},
        {".ppach", {"", &Compiler::compile_asm_ppach}},
        {".ppacb", {"", &Compiler::compile_asm_ppacb}},
        {".psubw", {"", &Compiler::compile_asm_psubw}},
        {".paddb", {"", &Compiler::compile_asm_paddb}},

        // BLOCK FORMS
        {"top-level", {"", &Compiler::compile_top_level}},
        {"begin", {"", &Compiler::compile_begin}},
        {"block", {"", &Compiler::compile_block}},
        {"return-from", {"", &Compiler::compile_return_from}},
        {"label", {"", &Compiler::compile_label}},
        {"goto", {"", &Compiler::compile_goto}},
        {"nop!", {"", &Compiler::compile_nop}},

        // COMPILER CONTROL
        {"repl-help", {"", &Compiler::compile_repl_help}},
        {"repl-keybinds", {"", &Compiler::compile_repl_keybinds}},
        {":clear", {"", &Compiler::compile_repl_clear_screen}},
        {"gs", {"", &Compiler::compile_gs}},
        {":exit", {"", &Compiler::compile_exit}},
        {"asm-file", {"", &Compiler::compile_asm_file}},
        {"asm-data-file", {"", &Compiler::compile_asm_data_file}},
        {"asm-text-file", {"", &Compiler::compile_asm_text_file}},
        {"listen-to-target", {"", &Compiler::compile_listen_to_target}},
        {"reset-target", {"", &Compiler::compile_reset_target}},
        {":status", {"", &Compiler::compile_poke}},
        {"in-package", {"", &Compiler::compile_in_package}},
        {"bundles", {"", &Compiler::compile_bundles}},
        {"require", {"", &Compiler::compile_require}},
        {"reload", {"", &Compiler::compile_reload}},
        {"get-info", {"", &Compiler::compile_get_info}},
        {"autocomplete", {"", &Compiler::compile_autocomplete}},
        {"update-macro-metadata", {"", &Compiler::compile_update_macro_metadata}},
        {"load-project", {"", &Compiler::compile_load_project}},
        {"make", {"", &Compiler::compile_make}},
        {"print-debug-compiler-stats", {"", &Compiler::compile_print_debug_compiler_stats}},
        {"gen-docs", {"", &Compiler::compile_gen_docs}},
        {"export-requires", {"", &Compiler::compile_export_requires}},
        {"gc-text", {"", &Compiler::compile_gc_text}},

        // CONDITIONAL COMPILATION
        {"#cond", {"", &Compiler::compile_gscond}},
        {"defglobalconstant", {"", &Compiler::compile_defglobalconstant}},
        {"seval", {"", &Compiler::compile_seval}},

        // CONTROL FLOW
        {"cond", {"", &Compiler::compile_cond}},
        {"when-goto", {"", &Compiler::compile_when_goto}},
        {"and", {"", &Compiler::compile_and_or}},
        {"or", {"", &Compiler::compile_and_or}},

        // DEFINITION
        {"define", {"", &Compiler::compile_define}},
        {"define-extern", {"", &Compiler::compile_define_extern}},
        {"set!", {"", &Compiler::compile_set}},

        // DEBUGGING
        {"dbs", {"", &Compiler::compile_dbs}},
        {"dbg", {"", &Compiler::compile_dbg}},
        {"dbgc", {"", &Compiler::compile_dbg_and_continue}},
        {":cont", {"", &Compiler::compile_cont}},
        {":stop", {"", &Compiler::compile_stop}},
        {":break", {"", &Compiler::compile_break}},
        {":dump-all-mem", {"", &Compiler::compile_dump_all}},
        {":pm", {"", &Compiler::compile_pm}},
        {":di", {"", &Compiler::compile_di}},
        {":disasm", {"", &Compiler::compile_disasm}},
        {":bp", {"", &Compiler::compile_bp}},
        {":ubp", {"", &Compiler::compile_ubp}},
        {":sym-name", {"", &Compiler::compile_d_sym_name}},

        // TYPE
        {"deftype", {"", &Compiler::compile_deftype}},
        {"defmethod", {"", &Compiler::compile_defmethod}},
        {"defenum", {"", &Compiler::compile_defenum}},
        {"->", {"", &Compiler::compile_deref}},
        {"&", {"", &Compiler::compile_addr_of}},
        {"the-as", {"", &Compiler::compile_the_as}},
        {"the", {"", &Compiler::compile_the}},
        {"print-type", {"", &Compiler::compile_print_type}},
        {"new", {"", &Compiler::compile_new}},
        {"car", {"", &Compiler::compile_car}},
        {"cdr", {"", &Compiler::compile_cdr}},
        {"method-of-type", {"", &Compiler::compile_method_of_type}},
        {"method-id-of-type", {"", &Compiler::compile_method_id_of_type}},
        {"method-of-object", {"", &Compiler::compile_method_of_object}},
        {"declare-type", {"", &Compiler::compile_declare_type}},
        {"none", {"", &Compiler::compile_none}},
        {"size-of", {"", &Compiler::compile_size_of}},
        {"psize-of", {"", &Compiler::compile_psize_of}},
        {"current-method-id", {"", &Compiler::compile_current_method_id}},
        {"cast-to-method-type", {"", &Compiler::compile_cast_to_method_type}},

        // LAMBDA
        {"lambda", {"", &Compiler::compile_lambda}},
        {"declare", {"", &Compiler::compile_declare}},
        {"inline", {"", &Compiler::compile_inline}},
        {"local-vars", {"", &Compiler::compile_local_vars}},
        {"declare-file", {"", &Compiler::compile_declare_file}},
        //        {"with-inline", {"", &Compiler::compile_with_inline}},
        //        {"get-ra-ptr", {"", &Compiler::compile_get_ra_ptr}},

        // MACRO
        {"quote", {"", &Compiler::compile_quote}},
        {"mlet", {"", &Compiler::compile_mlet}},
        {"defconstant", {"", &Compiler::compile_defconstant}},
        {"macro-expand",
         {"Displays the expanded form of a macro without evaluating it.",
          &Compiler::compile_macro_expand}},

        // OBJECT
        {"current-method-type", {"", &Compiler::compile_current_method_type}},

        // MATH
        {"+", {"", &Compiler::compile_add}},
        {"-", {"", &Compiler::compile_sub}},
        {"*", {"", &Compiler::compile_mul}},
        {"imul64", {"", &Compiler::compile_imul64}},
        {"/", {"", &Compiler::compile_div}},
        {"shl", {"", &Compiler::compile_shl}},
        {"shr", {"", &Compiler::compile_shr}},
        {"sar", {"", &Compiler::compile_sar}},
        {"mod", {"", &Compiler::compile_mod}},
        {"logior", {"", &Compiler::compile_logior}},
        {"logxor", {"", &Compiler::compile_logxor}},
        {"logand", {"", &Compiler::compile_logand}},
        {"lognot", {"", &Compiler::compile_lognot}},
        {"=", {"", &Compiler::compile_condition_as_bool}},
        {"!=", {"", &Compiler::compile_condition_as_bool}},
        {"eq?", {"", &Compiler::compile_condition_as_bool}},
        {"neq?", {"", &Compiler::compile_condition_as_bool}},
        {"not", {"", &Compiler::compile_condition_as_bool}},
        {"<=", {"", &Compiler::compile_condition_as_bool}},
        {">=", {"", &Compiler::compile_condition_as_bool}},
        {"<", {"", &Compiler::compile_condition_as_bool}},
        {">", {"", &Compiler::compile_condition_as_bool}},
        {"&+", {"", &Compiler::compile_pointer_add}},
        {"fmax", {"", &Compiler::compile_fmax}},
        {"fmin", {"", &Compiler::compile_fmin}},
        {"sqrtf-no-fabs", {"", &Compiler::compile_sqrtf}},

        // BUILDER (build-dgo/build-cgo?)
        {"build-dgos", {"", &Compiler::compile_build_dgo}},

        // UTIL
        {"set-config!", {"", &Compiler::compile_set_config}},

        // STATE
        {"define-state-hook", {"", &Compiler::compile_define_state_hook}},
        {"go-hook", {"", &Compiler::compile_go_hook}},
        {"define-virtual-state-hook", {"", &Compiler::compile_define_virtual_state_hook}},
};

Val* Compiler::compile_no_const_prop(const goos::Object& code, Env* env) {
  switch (code.type) {
    case goos::ObjectType::PAIR:
      return compile_pair(code, env);
    case goos::ObjectType::INTEGER:
      return compile_integer(code, env);
    case goos::ObjectType::CHAR:
      return compile_char(code, env);
    case goos::ObjectType::SYMBOL:
      return compile_symbol(code, env);
    case goos::ObjectType::STRING:
      return compile_string(code, env);
    case goos::ObjectType::FLOAT:
      return compile_float(code, env);
    default:
      throw_compiler_error(code, "Cannot compile {}.", code.print());
  }
  return get_none();
}

/*!
 * Highest level compile function
 */
Val* Compiler::compile(const goos::Object& code, Env* env) {
  // auto propagated = try_constant_propagation(code, env);
  return compile_no_const_prop(code, env);
}

/*!
 * Compile a pair/list.
 * Can be a compiler form, function call (possibly inlined), method call, immediate application of a
 * lambda, or a goos macro.
 */
Val* Compiler::compile_pair(const goos::Object& code, Env* env) {
  auto pair = code.as_pair();
  auto& head = pair->car;

  if (head.is_symbol()) {
    auto head_sym = head.as_symbol();
    auto& rest = pair->cdr;
    // first try as a macro
    goos::Object macro_obj;
    if (try_getting_macro_from_goos(head, &macro_obj)) {
      return compile_goos_macro(code, macro_obj, rest, head, env);
    }

    // next try as a goal compiler form
    auto kv_gfs = g_goal_forms.find(head_sym.name_ptr);
    if (kv_gfs != g_goal_forms.end()) {
      auto& [docstring, func] = kv_gfs->second;
      return ((*this).*(func))(code, rest, env);
    }

    // next try as an enum
    auto enum_type = m_ts.try_enum_lookup(head_sym.name_ptr);
    if (enum_type) {
      return compile_enum_lookup(code, enum_type, rest, env);
    }
  }

  // if none of the above cases worked, then treat it like a function/method call.
  return compile_function_or_method_call(code, env);
}

/*!
 * Compile an integer constant. Returns an IntegerConstantVal and emits no code.
 * These integer constants do not generate static data and are stored directly in the code
 * which is generated with to_gpr.
 * The type is always int.
 */
Val* Compiler::compile_integer(const goos::Object& code, Env* env) {
  ASSERT(code.is_int());
  return compile_integer(code.integer_obj.value, env);
}

Val* Compiler::compile_char(const goos::Object& code, Env* env) {
  ASSERT(code.is_char());
  return compile_integer(uint8_t(code.char_obj.value), env);
}

/*!
 * Compile an integer constant. Returns an IntegerConstantVal and emits no code.
 * These integer constants do not generate static data and are stored directly in the code
 * which is generated with to_gpr.
 * The type is always int.
 */
Val* Compiler::compile_integer(s64 value, Env* env) {
  auto fe = env->function_env();
  return fe->alloc_val<IntegerConstantVal>(m_ts.make_typespec("int"), &value, 8);
}

Val* Compiler::compile_integer(const U128& value, Env* env) {
  auto fe = env->function_env();
  return fe->alloc_val<IntegerConstantVal>(m_ts.make_typespec("int"), &value, 16);
}

/*!
 * Get a SymbolVal representing a GOAL symbol object.
 */
SymbolVal* Compiler::compile_get_sym_obj(const std::string& name, Env* env) {
  auto fe = env->function_env();
  return fe->alloc_val<SymbolVal>(name, m_ts.make_typespec("symbol"));
}

/*!
 * Get a SymbolValueVal representing the value of a GOAL symbol.
 * Will throw a compilation error if the symbol wasn't previously defined.
 * TODO - determine sign extension behavior when loading symbol values.
 */
Val* Compiler::compile_get_symbol_value(const goos::Object& form,
                                        const std::string& name,
                                        Env* env) {
  auto existing_symbol = m_symbol_types.lookup(m_goos.intern_ptr(name));
  if (!existing_symbol) {
    throw_compiler_error(
        form, "The symbol {} was looked up as a global variable, but it does not exist.", name);
  }

  const auto& ts = *existing_symbol;
  const auto& full_type = m_ts.lookup_type_allow_partial_def(ts);
  if (m_settings.check_for_requires) {
    if (full_type->m_metadata.definition_info.has_value() &&
        !env->file_env()->m_missing_required_files.contains(
            full_type->m_metadata.definition_info->filename) &&
        env->file_env()->m_required_files.find(full_type->m_metadata.definition_info->filename) ==
            env->file_env()->m_required_files.end() &&
        !str_util::ends_with(full_type->m_metadata.definition_info->filename,
                             env->file_env()->name() + ".gc")) {
      lg::warn("Missing require in {} for {} over {}", env->file_env()->name(),
               full_type->m_metadata.definition_info->filename, name);
      env->file_env()->m_missing_required_files.insert(
          full_type->m_metadata.definition_info->filename);
    } else {
      // Try to lookup in symbol_info
      const auto& symbol_info = m_symbol_info.lookup_exact_name(name);
      if (!symbol_info.empty()) {
        const auto& result = symbol_info.at(0);
        if (result->m_def_location.has_value() &&
            !env->file_env()->m_missing_required_files.contains(
                result->m_def_location->file_path) &&
            env->file_env()->m_required_files.find(result->m_def_location->file_path) ==
                env->file_env()->m_required_files.end() &&
            !str_util::ends_with(result->m_def_location->file_path,
                                 env->file_env()->name() + ".gc")) {
          lg::warn("Missing require in {} for {} over {}", env->file_env()->name(),
                   result->m_def_location->file_path, name);
          env->file_env()->m_missing_required_files.insert(result->m_def_location->file_path);
        }
      }
    }
  }
  auto sext = full_type->get_load_signed();
  auto fe = env->function_env();
  auto sym = fe->alloc_val<SymbolVal>(name, m_ts.make_typespec("symbol"));
  auto re = fe->alloc_val<SymbolValueVal>(sym, ts, sext);
  return re;
}

/*!
 * Compile a symbol. Can get mlet macro symbols, local variables, constants, or symbols.
 * Note: order of checks here should match try_constant_propagation
 */
Val* Compiler::compile_symbol(const goos::Object& form, Env* env) {
  auto name = symbol_string(form);

  // optimization to look these up as symbol objects, not getting the value of a symbol.
  // so you don't have to type '#f, '#t everywhere to get the best performance.
  if (name == "#t" || name == "#f") {
    return compile_get_sym_obj(name, env);
  }

  // see if the symbol is defined in any enclosing symbol macro envs (mlet's).
  auto mlet_env = env->symbol_macro_env();
  while (mlet_env) {
    auto mlkv = mlet_env->macros.find(form.as_symbol());
    if (mlkv != mlet_env->macros.end()) {
      return compile_error_guard(mlkv->second, env);
    }
    mlet_env = mlet_env->parent()->symbol_macro_env();
  }

  // see if it's a local variable
  auto lexical = env->lexical_lookup(form);
  if (lexical) {
    return lexical;
  }

  auto global_constant = m_global_constants.lookup(form.as_symbol());

  // see if it's a constant
  if (global_constant) {
    auto existing_symbol = m_symbol_types.lookup(form.as_symbol());
    // check there is no symbol with the same name
    if (existing_symbol) {
      throw_compiler_error(form,
                           "Ambiguous symbol: {} is both a global variable and a constant and it "
                           "is not clear which should be used here.");
    }
    if (m_settings.check_for_requires) {
      // TODO - these file checks are gross, replace with something more robust long-term
      const auto& symbol_info =
          m_symbol_info.lookup_exact_name(name, symbol_info::Kind::GLOBAL_VAR);
      if (!symbol_info.empty()) {
        const auto& result = symbol_info.at(0);
        if (result->m_def_location.has_value() &&
            !env->file_env()->m_missing_required_files.contains(
                result->m_def_location->file_path) &&
            env->file_env()->m_required_files.find(result->m_def_location->file_path) ==
                env->file_env()->m_required_files.end() &&
            !str_util::ends_with(result->m_def_location->file_path,
                                 env->file_env()->name() + ".gc")) {
          lg::warn("Missing require in {} for {} over {}", env->file_env()->name(),
                   result->m_def_location->file_path, name);
          env->file_env()->m_missing_required_files.insert(result->m_def_location->file_path);
        }
      }
    }
    // got a global constant
    return compile_error_guard(*global_constant, env);
  }

  // none of those, so get a global symbol.
  return compile_get_symbol_value(form, name, env);
}

/*!
 * Compile a string constant. The constant is placed in the same segment as the parent function. For
 * top-level functions, the string is placed in the file's default segment.
 */
Val* Compiler::compile_string(const goos::Object& form, Env* env) {
  return compile_string(form.as_string()->data, env,
                        env->function_env()->segment_for_static_data());
}

/*!
 * Compile a string constant and place it in the given segment.
 */
Val* Compiler::compile_string(const std::string& str, Env* env, int seg) {
  auto obj = std::make_unique<StaticString>(str, seg);
  auto fe = env->function_env();
  auto result = fe->alloc_val<StaticVal>(obj.get(), m_ts.make_typespec("string"));
  auto fie = env->file_env();
  fie->add_static(std::move(obj));
  return result;
}

/*!
 * Compile a floating point constant and place it in the same segment as the containing function.
 * Unlike integers, all floating point constants are stored separately as static data outside
 * of the code, at least in Jak 1.
 */
Val* Compiler::compile_float(const goos::Object& code, Env* env) {
  ASSERT(code.is_float());
  // TODO: this will put top-level only floats in main. Which is conservative because I
  // don't think we can take the address of a float constant.
  return compile_float(code.float_obj.value, env, env->function_env()->segment_for_static_data());
}

/*!
 * Compile a floating point constant and place it in given segment.
 * Unlike integers, all floating point constants are stored separately as static data outside
 * of the code, at least in Jak 1.
 */
Val* Compiler::compile_float(float value, Env* env, int seg) {
  auto obj = std::make_unique<StaticFloat>(value, seg);
  auto fe = env->function_env();
  auto result = fe->alloc_val<FloatConstantVal>(m_ts.make_typespec("float"), obj.get());
  auto fie = env->file_env();
  fie->add_static(std::move(obj));
  return result;
}

Val* Compiler::compile_pointer_add(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va(form, rest);
  if (args.unnamed.size() < 2 || !args.named.empty()) {
    throw_compiler_error(form, "&+ must be used with at least two arguments.");
  }
  auto first = compile_error_guard(args.unnamed.at(0), env)->to_gpr(form, env);

  bool ok_type = false;
  for (auto& type : {"pointer", "structure", "inline-array"}) {
    if (m_ts.tc(m_ts.make_typespec(type), first->type())) {
      ok_type = true;
      break;
    }
  }

  if (!ok_type) {
    throw_compiler_error(
        form, "&+ was used with a {}, which is not a pointer, structure, or inline-array.",
        first->type().print());
  }

  auto result = env->make_gpr(first->type());
  env->emit_ir<IR_RegSet>(form, result, first);

  for (size_t i = 1; i < args.unnamed.size(); i++) {
    auto second = compile_error_guard(args.unnamed.at(i), env)->to_gpr(form, env);
    typecheck(form, m_ts.make_typespec("integer"), second->type(), "&+ second argument");
    env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::ADD_64, result, second);
  }

  return result;
}
