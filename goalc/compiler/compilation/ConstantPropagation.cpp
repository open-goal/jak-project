#include "common/util/string_util.h"

#include "goalc/compiler/Compiler.h"

/*!
 * Main table for compiler forms that can be constant propagated.
 */
const std::unordered_map<std::string,
                         Compiler::ConstPropResult (Compiler::*)(const goos::Object& form,
                                                                 const goos::Object& rest,
                                                                 Env* env)>
    g_const_prop_forms = {
        // INLINE ASM
        {"begin", &Compiler::const_prop_begin},
        {"size-of", &Compiler::const_prop_size_of},
        {"#cond", &Compiler::const_prop_gscond}};

// Note: writing const_prop functions is a bit tricky because you have to try expanding macros, but
// if you decide that you can't constant propagate, then there's no way to "undo" any side effects
// from the macro expansion. So the solution is to return a form with all macro expansions already
// applied.

// The result should be a goos Object that is either code to be compiled, or some constant
// integer/string/float/symbol. The const prop functions should emit no code.

/*!
 * Constant propagate a form like:
 * (begin a b c d ...)
 * The head doesn't have to be "begin" and it will still work (the form argument is ignored).
 *
 * If constant propagation fails, it will return a form like
 * (begin a c ..)
 * where the head is always begin (even if it wasn't originally), and some of a, b, c...
 * may be macro expanded, or omitted if they have no side effects.
 *
 * If constant propagation succeeds (the expression is a compile time constant with no side effects)
 * it will return a single value. The other values don't matter at all.
 *
 * This applies constant propagation recursively, so elements may be macro expanded and nested
 * begins can be eliminated.
 *
 * This function can generally be used to constant propagate any "body" of code.
 */
Compiler::ConstPropResult Compiler::const_prop_begin(const goos::Object& /*form*/,
                                                     const goos::Object& rest,
                                                     Env* env) {
  ConstPropResult result;
  result.has_side_effects = false;
  result.value = goos::PairObject::make_new({}, {});
  goos::Object* out_it = &result.value.as_pair()->cdr;
  const goos::Object* it = &rest;
  while (!it->is_empty_list()) {
    const goos::Object& obj = it->as_pair()->car;

    auto this_elt_prop =
        result.has_side_effects ? ConstPropResult{obj, true} : try_constant_propagation(obj, env);
    if (this_elt_prop.has_side_effects) {
      result.has_side_effects = true;
    }
    it = &it->as_pair()->cdr;
    if (it->is_empty_list() && !result.has_side_effects) {
      // can throw out the begin and replace it with the last thing
      return this_elt_prop;
    }

    if (this_elt_prop.has_side_effects) {
      *out_it = goos::PairObject::make_new(this_elt_prop.value, {});
      out_it = &out_it->as_pair()->cdr;
    }
  }

  result.value.as_pair()->car = m_goos.intern("begin");
  *out_it = goos::Object::make_empty_list();

  return result;
}

/*!
 * Constant propagate a #cond form.
 * This will always evaluate the actual conditions.
 * In cases where we return none, it gives up constant propagation, as we can't really do anything
 * with that.
 * In other cases, it tries to constant propagate the body of the matching case.
 */
Compiler::ConstPropResult Compiler::const_prop_gscond(const goos::Object& form,
                                                      const goos::Object& rest,
                                                      Env* env) {
  if (!rest.is_pair()) {
    throw_compiler_error(form, "#cond must have at least one clause, which must be a form");
  }

  goos::Object lst = rest;
  for (;;) {
    if (lst.is_pair()) {
      goos::Object current_case = lst.as_pair()->car;
      if (!current_case.is_pair()) {
        throw_compiler_error(lst, "Bad case in #cond");
      }

      // check condition:
      goos::Object condition_result = m_goos.eval_with_rewind(
          current_case.as_pair()->car, m_goos.global_environment.as_env_ptr());
      if (m_goos.truthy(condition_result)) {
        if (current_case.as_pair()->cdr.is_empty_list()) {
          // would return none, let's just return that this has side effects and let the compiler
          // handle it.
          return {form, true};
        }
        // got a match!
        return const_prop_begin(current_case, current_case.as_pair()->cdr, env);
      } else {
        // no match, continue.
        lst = lst.as_pair()->cdr;
      }
    } else if (lst.is_empty_list()) {
      return {form, true};
    } else {
      throw_compiler_error(form, "malformed #cond");
    }
  }
}

namespace {
size_t code_size(Env* e) {
  auto fe = e->function_env();
  if (fe) {
    return fe->code().size();
  } else {
    return 0;
  }
}
}  // namespace

Compiler::ConstPropResult Compiler::try_constant_propagation(const goos::Object& form, Env* env) {
  size_t start_size = code_size(env);
  auto ret = constant_propagation_dispatch(form, env);
  size_t end_size = code_size(env);
  if (start_size != end_size) {
    lg::print("Compiler bug in constant propagation. Code was generated: {} vs {}\n", start_size,
              end_size);
    ASSERT(false);
  }
  return ret;
}

/*!
 * Main constant propagation dispatch.
 * Note that there are some tricky orders to get right here - we don't want to use a global constant
 * when the normal compiler would use a lexical variable.
 */
Compiler::ConstPropResult Compiler::constant_propagation_dispatch(const goos::Object& code,
                                                                  Env* env) {
  // first, expand macros.
  // this only does something if code is a pair, and for pairs macros are the first check.
  auto expanded = expand_macro_completely(code, env);

  switch (expanded.type) {
    case goos::ObjectType::INTEGER:
    case goos::ObjectType::STRING:
    case goos::ObjectType::FLOAT:
      // we got a plain value, no code is needed to figure out the value and we can return it
      // directly.
      return {expanded, false};
    case goos::ObjectType::SYMBOL: {
      // NOTE: order must match compile_symbol
      // #t/#f
      // mlet
      // lexical
      // constant/symbol

      // first, try to resolve the symbol in an mlet environment.
      auto mlet_env = env->symbol_macro_env();
      while (mlet_env) {
        auto mlkv = mlet_env->macros.find(expanded.as_symbol());
        if (mlkv != mlet_env->macros.end()) {
          // we found a match, substitute and keep trying.
          return try_constant_propagation(mlkv->second, env);
        }
        mlet_env = mlet_env->parent()->symbol_macro_env();
      }

      // see if it's a local variable
      auto lexical = env->lexical_lookup(expanded);
      if (lexical) {
        // if so, that's what we should use, not a constant with the same name.
        // give up on constant propagation, we never do it for lexicals (can't really in a single
        // pass).
        return {expanded, true};
      }

      // it can either be a global or symbol
      const auto* global_constant = m_global_constants.lookup(expanded.as_symbol());
      const auto* existing_symbol = m_symbol_types.lookup(expanded.as_symbol());

      // see if it's a constant
      if (global_constant) {
        // check there is no symbol with the same name, this is likely a bug and complain.
        if (existing_symbol) {
          throw_compiler_error(
              code,
              "Ambiguous symbol: {} is both a global variable and a constant and it "
              "is not clear which should be used here.");
        }

        // got a global constant
        return try_constant_propagation(*global_constant, env);
      } else {
        // return to the compiler, we can't figure it out.
        return {expanded, true};
      }
    } break;

    case goos::ObjectType::PAIR: {
      auto pair = expanded.as_pair();
      auto head = pair->car;
      auto rest = pair->cdr;

      // in theory you could write code like:
      // ((#if PC_PORT foo bar) ...)
      // and you might want the compiler to constant propagate (foo ...)
      // but this is not implemented because the logic in compile_function_or_method_call
      // is quite complicated and this case seems unlikely to ever be used.

      if (head.is_symbol()) {
        auto head_sym = head.as_symbol();
        // the first thing tried should be macros, but we already did that. And it iterates until
        // all are expanded, so we don't need to do it again.

        // first try as a goal compiler form
        auto kv_gfs = g_const_prop_forms.find(head_sym.name_ptr);
        if (kv_gfs != g_const_prop_forms.end()) {
          return ((*this).*(kv_gfs->second))(expanded, rest, env);
        }

        const auto& kv_goal = g_goal_forms.find(head_sym.name_ptr);
        if (kv_goal != g_goal_forms.end()) {
          // it's a compiler form that we can't constant propagate.
          return {expanded, true};
        }
      }

      return {expanded, true};
    }
    default:
      return {expanded, true};
  }
}

s64 Compiler::get_constant_integer_or_error(const goos::Object& in, Env* env) {
  const auto prop = try_constant_propagation(in, env);
  if (prop.value.is_pair()) {
    const auto& head = prop.value.as_pair()->car;
    if (head.is_symbol()) {
      const auto& head_sym = head.as_symbol();
      const auto enum_type = m_ts.try_enum_lookup(head_sym.name_ptr);
      if (enum_type) {
        if (m_settings.check_for_requires) {
          // Check if the enum has been required or not
          if (enum_type->m_metadata.definition_info.has_value() &&
              !env->file_env()->m_missing_required_files.contains(
                  enum_type->m_metadata.definition_info->filename) &&
              env->file_env()->m_required_files.find(
                  enum_type->m_metadata.definition_info->filename) ==
                  env->file_env()->m_required_files.end() &&
              !str_util::ends_with(enum_type->m_metadata.definition_info->filename,
                                   env->file_env()->name() + ".gc")) {
            lg::warn("Missing require in {} for {} over {}", env->file_env()->name(),
                     enum_type->m_metadata.definition_info->filename, head_sym.name_ptr);
            env->file_env()->m_missing_required_files.insert(
                enum_type->m_metadata.definition_info->filename);
          }
        }
        bool success;
        // TODO - changed false to true so you'd actually once again get a useful error (something
        // wasn't found in the enum instead of just "cant be used as a constant") Ramifications of
        // this?
        const u64 as_enum =
            enum_lookup(prop.value, enum_type, prop.value.as_pair()->cdr, true, &success);
        if (success) {
          return as_enum;
        }
      } else {
        // TODO - provide a meaningful error to using an enum that wasn't defined
        // is this going to break other things, it looks like currently the `has_side_effects` is
        // defaulted to `true` so the error below is kinda...by accident?
        throw_compiler_error(in, "{} is not a defined enum.", head_sym.name_ptr);
      }
    }
  }

  if (prop.has_side_effects) {
    throw_compiler_error(in, "Value {} cannot be used as a constant.", in.print());
  } else {
    if (prop.value.is_int()) {
      return prop.value.as_int();
    } else {
      throw_compiler_error(
          in, "Value {} cannot be used as a constant integer - it has the wrong type.", in.print());
    }
  }
}

ValOrConstInt Compiler::get_constant_integer_or_variable(const goos::Object& in, Env* env) {
  auto prop = try_constant_propagation(in, env);

  if (prop.value.is_pair()) {
    auto head = prop.value.as_pair()->car;
    if (head.is_symbol()) {
      auto head_sym = head.as_symbol();
      auto enum_type = m_ts.try_enum_lookup(head_sym.name_ptr);
      if (enum_type) {
        bool success;
        u64 as_enum =
            enum_lookup(prop.value, enum_type, prop.value.as_pair()->cdr, false, &success);
        if (success) {
          return ValOrConstInt(as_enum);
        }
      }
    }
  }

  if (prop.has_side_effects) {
    return ValOrConstInt(compile_no_const_prop(prop.value, env));
  } else {
    if (prop.value.is_int()) {
      return ValOrConstInt(prop.value.as_int());
    } else {
      return ValOrConstInt(compile_no_const_prop(prop.value, env));
    }
  }
}

ValOrConstFloat Compiler::get_constant_float_or_variable(const goos::Object& in, Env* env) {
  auto prop = try_constant_propagation(in, env);
  if (prop.has_side_effects) {
    return ValOrConstFloat(compile_no_const_prop(prop.value, env));
  } else {
    if (prop.value.is_float()) {
      return ValOrConstFloat(prop.value.as_float());
    } else {
      return ValOrConstFloat(compile_no_const_prop(prop.value, env));
    }
  }
}
