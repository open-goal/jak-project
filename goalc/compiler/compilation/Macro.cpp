#include "common/goos/PrettyPrinter.h"
#include "common/util/string_util.h"

#include "goalc/compiler/Compiler.h"

#include "fmt/core.h"

using namespace goos;

/*!
 * Try to find a macro with the given name in the GOOS "goal_env". Return if it succeeded.
 */
bool Compiler::try_getting_macro_from_goos(const goos::Object& macro_name, goos::Object* dest) {
  Object macro_obj;
  bool got_macro = false;
  if (m_goos.eval_symbol(macro_name, m_goos.goal_env.as_env_ptr(), &macro_obj) &&
      macro_obj.is_macro()) {
    got_macro = true;
  }

  if (got_macro && dest) {
    *dest = macro_obj;
  }
  return got_macro;
}

/*!
 * Expand a macro, then compile the result.
 */
Val* Compiler::compile_goos_macro(const goos::Object& o,
                                  const goos::Object& macro_obj,
                                  const goos::Object& rest,
                                  const goos::Object& name,
                                  Env* env) {
  if (m_settings.check_for_requires) {
    const auto& symbol_info =
        m_symbol_info.lookup_exact_name(name.print(), symbol_info::Kind::MACRO);
    if (!symbol_info.empty()) {
      const auto& result = symbol_info.at(0);
      if (result->m_def_location.has_value() &&
          !env->file_env()->m_missing_required_files.contains(result->m_def_location->file_path) &&
          env->file_env()->m_required_files.find(result->m_def_location->file_path) ==
              env->file_env()->m_required_files.end() &&
          !str_util::ends_with(result->m_def_location->file_path,
                               env->file_env()->name() + ".gc")) {
        lg::warn("Missing require in {} for {} over {}", env->file_env()->name(),
                 result->m_def_location->file_path, name.print());
        env->file_env()->m_missing_required_files.insert(result->m_def_location->file_path);
      }
    }
  }

  auto macro = macro_obj.as_macro();
  Arguments args = m_goos.get_args(o, rest, macro->args);
  auto mac_env_obj = EnvironmentObject::make_new();
  auto mac_env = mac_env_obj.as_env_ptr();
  mac_env->parent_env = m_goos.global_environment.as_env_ptr();
  m_goos.set_args_in_env(o, args, macro->args, mac_env);
  auto goos_result = m_goos.eval_list_return_last(macro->body, macro->body, mac_env);
  // make the macro expanded form point to the source where the macro was used for error messages.
  // m_goos.reader.db.inherit_info(o, goos_result);

  auto compile_env_for_macro =
      env->function_env()->alloc_env<MacroExpandEnv>(env, name.as_symbol(), macro->body, o);
  try {
    const auto& compile_result = compile(goos_result, compile_env_for_macro);
    return compile_result;
  } catch (CompilerException& ce) {
    if (ce.print_err_stack) {
      bool good_info = false;
      auto info = m_goos.reader.db.get_info_for(o, &good_info);
      lg::print(fg(fmt::color::yellow) | fmt::emphasis::bold, "Code:\n");
      auto code_str = pretty_print::to_string(goos_result, 120);
      if (code_str.size() > 120 * 30) {
        code_str = code_str.substr(0, 120 * 30) + "...";
      }
      lg::print("{}\n", code_str);
      lg::print(fg(fmt::color::yellow) | fmt::emphasis::bold, "From macro: ");
      lg::print(fg(fmt::color::orange), "{}\n", name.print());
      if (good_info) {
        lg::print(fg(fmt::color::yellow) | fmt::emphasis::bold, "Used in:\n");
        lg::print("{}\n", info);
        ce.print_err_stack = false;
      }
      std::string line(80, '-');
      line.push_back('\n');
      lg::print("{}", line);
    }

    throw;
  } catch (std::runtime_error& e) {
    bool good_info = false;
    auto info = m_goos.reader.db.get_info_for(o, &good_info);
    lg::print(fg(fmt::color::yellow) | fmt::emphasis::bold, "Code:\n");
    auto code_str = pretty_print::to_string(goos_result, 120);
    if (code_str.size() > 120 * 30) {
      code_str = code_str.substr(0, 120 * 30) + "...";
    }
    lg::print("{}\n", code_str);
    lg::print(fg(fmt::color::yellow) | fmt::emphasis::bold, "From macro: ");
    lg::print(fg(fmt::color::orange), "{}\n", name.print());
    if (good_info) {
      lg::print(fg(fmt::color::yellow) | fmt::emphasis::bold, "Used in:\n");
      lg::print("{}\n", info);
    }
    std::string line(80, '-');
    line.push_back('\n');
    lg::print("{}", line);

    throw;
  }
}

/*!
 * Compile the #cond form, which is a compile-time conditional statement.
 */
Val* Compiler::compile_gscond(const goos::Object& form, const goos::Object& rest, Env* env) {
  if (!rest.is_pair()) {
    throw_compiler_error(form, "#cond must have at least one clause, which must be a form");
  }
  Val* result = nullptr;

  Object lst = rest;
  for (;;) {
    if (lst.is_pair()) {
      Object current_case = lst.as_pair()->car;
      if (!current_case.is_pair()) {
        throw_compiler_error(lst, "Bad case in #cond");
      }

      // check condition:
      Object condition_result = m_goos.eval_with_rewind(current_case.as_pair()->car,
                                                        m_goos.global_environment.as_env_ptr());
      if (m_goos.truthy(condition_result)) {
        if (current_case.as_pair()->cdr.is_empty_list()) {
          return get_none();
        }
        // got a match!
        result = get_none();

        for_each_in_list(current_case.as_pair()->cdr, [&](const Object& o) {
          result = compile_error_guard(o, env);
          if (!dynamic_cast<None*>(result)) {
            result = result->to_reg(o, env);
          }
        });
        return result;
      } else {
        // no match, continue.
        lst = lst.as_pair()->cdr;
      }
    } else if (lst.is_empty_list()) {
      return get_none();
    } else {
      throw_compiler_error(form, "malformed #cond");
    }
  }
}

/*!
 * Compile (quote x) or 'x forms.
 * Current only supports 'thing or '(). Static lists/pairs should be added at some point.
 */
Val* Compiler::compile_quote(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto args = get_va_no_named(form, rest);
  if (args.unnamed.size() != 1) {
    throw_compiler_error(form, "invalid number of arguments to compile quote");
  }
  auto& thing = args.unnamed.front();
  switch (thing.type) {
    case goos::ObjectType::SYMBOL:
      return compile_get_sym_obj(thing.as_symbol().name_ptr, env);
    case goos::ObjectType::EMPTY_LIST: {
      auto empty_pair = compile_get_sym_obj("_empty_", env);
      empty_pair->set_type(m_ts.make_typespec("pair"));
      return empty_pair;
    }
    case goos::ObjectType::PAIR:
      return compile_static_pair(thing, env, env->function_env()->segment_for_static_data());
    default:
      throw_compiler_error(form, "Quote is not yet implemented for {}.", thing.print());
  }
  return get_none();
}

Val* Compiler::compile_define_constant(const goos::Object& form,
                                       const goos::Object& _rest,
                                       Env* env,
                                       bool goos,
                                       bool goal) {
  auto rest = &_rest;
  (void)env;
  if (!rest->is_pair()) {
    throw_compiler_error(form, "invalid constant definition");
  }

  auto sym = pair_car(*rest).as_symbol();
  rest = &pair_cdr(*rest);

  // check for potential docstring
  std::string docstring = "";
  if (rest->is_pair() && pair_car(*rest).is_string() && !pair_cdr(*rest).is_empty_list()) {
    docstring = pair_car(*rest).as_string()->data;
    rest = &pair_cdr(*rest);
  }

  auto& value = pair_car(*rest);

  rest = &rest->as_pair()->cdr;
  if (!rest->is_empty_list()) {
    throw_compiler_error(form, "invalid constant definition");
  }

  // GOAL constant
  if (goal) {
    if (m_symbol_types.lookup(sym)) {
      throw_compiler_error(form,
                           "Cannot define {} as a constant because "
                           "it is already the name of a symbol of type {}",
                           sym.name_ptr, m_symbol_types.lookup(sym)->print());
    }

    auto existing = m_global_constants.lookup(sym);
    if (existing && *existing != value) {
      print_compiler_warning("Constant {} has been redefined {} -> {}", sym.name_ptr,
                             existing->print(), value.print());
    }
    m_global_constants.set(sym, value);
  }

  // GOOS constant
  if (goos) {
    m_goos.global_environment.as_env()->vars.set(sym, value);
  }

  // TODO - eventually, it'd be nice if global constants were properly typed
  // and this information was propagated
  m_symbol_info.add_constant(sym.name_ptr, form, docstring);

  return get_none();
}

/*!
 * Compile defglobalconstant forms, which define a constant in both GOOS and GOAL.
 */
Val* Compiler::compile_defglobalconstant(const goos::Object& form,
                                         const goos::Object& rest,
                                         Env* env) {
  return compile_define_constant(form, rest, env, true, true);
}

/*!
 * Compile a defconstant form, which defines a constant that is only in GOAL.
 */
Val* Compiler::compile_defconstant(const goos::Object& form, const goos::Object& rest, Env* env) {
  return compile_define_constant(form, rest, env, false, true);
}

/*!
 * Compile an "mlet" scoped constant/symbol macro form
 */
Val* Compiler::compile_mlet(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto defs = pair_car(rest);
  auto body = pair_cdr(rest);

  auto fenv = env->function_env();
  auto menv = fenv->alloc_env<SymbolMacroEnv>(env);

  for_each_in_list(defs, [&](const goos::Object& o) {
    auto def_args = get_va(form, o);
    va_check(form, def_args, {goos::ObjectType::SYMBOL, {}}, {});
    menv->macros[def_args.unnamed.at(0).as_symbol()] = def_args.unnamed.at(1);
  });

  Val* result = get_none();
  for_each_in_list(body, [&](const goos::Object& o) {
    result = compile_error_guard(o, menv);
    if (!dynamic_cast<None*>(result)) {
      result = result->to_reg(o, menv);
    }
  });
  return result;
}

Val* Compiler::compile_macro_expand(const goos::Object& form, const goos::Object& rest, Env* env) {
  auto& macro = pair_car(rest);
  auto& macro_name = pair_car(macro);
  if (!try_getting_macro_from_goos(macro_name, nullptr)) {
    throw_compiler_error(form, "invalid argument to `macro-expand`: {} does not exist as a macro",
                         macro_name.print());
  }
  if (!pair_cdr(rest).is_empty_list()) {
    throw_compiler_error(form, "too many arguments to `macro-expand`");
  }
  auto result = expand_macro_completely(macro, env);
  auto code = pretty_print::to_string(result);
  lg::print("{}\n", code);
  return get_none();
}

bool Compiler::expand_macro_once(const goos::Object& src, goos::Object* out, Env*) {
  if (!src.is_pair()) {
    return false;
  }

  auto& first = src.as_pair()->car;
  auto& rest = src.as_pair()->cdr;
  if (!first.is_symbol()) {
    return false;
  }

  goos::Object macro_obj;
  if (!try_getting_macro_from_goos(first, &macro_obj)) {
    return false;
  }

  auto macro = macro_obj.as_macro();
  Arguments args = m_goos.get_args(src, rest, macro->args);
  auto mac_env_obj = EnvironmentObject::make_new();
  auto mac_env = mac_env_obj.as_env_ptr();
  mac_env->parent_env = m_goos.global_environment.as_env_ptr();
  m_goos.set_args_in_env(src, args, macro->args, mac_env);

  auto goos_result = m_goos.eval_list_return_last(macro->body, macro->body, mac_env);
  // make the macro expanded form point to the source where the macro was used for error messages.
  // m_goos.reader.db.inherit_info(src, goos_result);

  *out = goos_result;
  return true;
}

goos::Object Compiler::expand_macro_completely(const goos::Object& src, Env* env) {
  goos::Object result = src;
  while (expand_macro_once(result, &result, env)) {
  }
  return result;
}
