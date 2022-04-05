#include "goalc/compiler/Compiler.h"

/*!
 * Main table for compiler forms
 */
const std::unordered_map<std::string,
                         Compiler::ConstPropResult (Compiler::*)(const goos::Object& form,
                                                                 const goos::Object& rest,
                                                                 Env* env)>
    g_const_prop_forms = {
        // INLINE ASM
        {"begin", &Compiler::const_prop_begin},
        {"size-of", &Compiler::const_prop_size_of}};

Compiler::ConstPropResult Compiler::const_prop_begin(const goos::Object& form,
                                                     const goos::Object& rest,
                                                     Env* env) {
  ConstPropResult result;
  result.has_side_effects = false;
  result.value = goos::PairObject::make_new(form.as_pair()->car, {});
  goos::Object* out_it = &result.value.as_pair()->cdr;
  const goos::Object* it = &rest;
  while (!it->is_empty_list()) {
    const goos::Object& obj = it->as_pair()->car;

    auto this_elt_prop =
        result.has_side_effects ? ConstPropResult{obj, true} : try_constant_propagation(obj, env);
    if (this_elt_prop.has_side_effects) {
      result.has_side_effects = true;
    }
    *out_it = goos::PairObject::make_new(this_elt_prop.value, {});
    out_it = &out_it->as_pair()->cdr;
    it = &it->as_pair()->cdr;
  }
  *out_it = goos::Object::make_empty_list();

  return result;
}

Compiler::ConstPropResult Compiler::try_constant_propagation(const goos::Object& code, Env* env) {
  auto expanded = expand_macro_completely(code, env);
  // auto expanded = code;

  switch (expanded.type) {
    case goos::ObjectType::INTEGER:
    case goos::ObjectType::STRING:
      return {expanded, false};
    case goos::ObjectType::SYMBOL: {
      const auto& global_constant = m_global_constants.find(expanded.as_symbol());
      if (global_constant == m_global_constants.end()) {
        return {expanded, false};
      } else {
        return try_constant_propagation(global_constant->second, env);
      }
    } break;

    case goos::ObjectType::PAIR: {
      auto pair = expanded.as_pair();
      auto head = pair->car;
      auto rest = pair->cdr;

      if (head.is_symbol()) {
        auto head_sym = head.as_symbol();
        // first try as a goal compiler form
        auto kv_gfs = g_const_prop_forms.find(head_sym->name);
        if (kv_gfs != g_const_prop_forms.end()) {
          return ((*this).*(kv_gfs->second))(expanded, rest, env);
        }
      }

      return {expanded, true};
    }
    default:
      return {expanded, true};
  }
}

s64 Compiler::get_constant_integer_or_error(const goos::Object& in, Env* env) {
  auto prop = try_constant_propagation(in, env);
  if (prop.has_side_effects) {
    throw_compiler_error(in, "Value cannot be used as a constant - it has side effects.");
  } else {
    if (prop.value.is_int()) {
      return prop.value.as_int();
    } else {
      throw_compiler_error(in,
                           "Value cannot be used as a constant integer - it has the wrong type.");
    }
  }
}