#include "common/goos/ParseHelpers.h"
#include "common/type_system/deftype.h"

#include "goalc/compiler/Compiler.h"
#include "goalc/compiler/IR.h"

/*!
 * Parse arguments into a goos::Arguments format.
 */
goos::Arguments Compiler::get_va(const goos::Object& form, const goos::Object& rest) {
  goos::Arguments args;

  std::string err;
  if (!goos::get_va(rest, &err, &args)) {
    throw_compiler_error(form, err);
  }
  return args;
}

/*!
 * Check arguments in a goos::Arguments format (named and unnamed) and throw a compiler error if it
 * fails.
 */
void Compiler::va_check(
    const goos::Object& form,
    const goos::Arguments& args,
    const std::vector<std::optional<goos::ObjectType>>& unnamed,
    const std::unordered_map<std::string, std::pair<bool, std::optional<goos::ObjectType>>>&
        named) {
  std::string err;
  if (!goos::va_check(args, unnamed, named, &err)) {
    throw_compiler_error(form, err);
  }
}

/*!
 * Iterate through elements of a goos list and apply the given function. Throw compiler error if the
 * list is invalid.
 */
void Compiler::for_each_in_list(const goos::Object& list,
                                const std::function<void(const goos::Object&)>& f) {
  const goos::Object* iter = &list;
  while (iter->is_pair()) {
    auto lap = iter->as_pair();
    f(lap->car);
    iter = &lap->cdr;
  }

  if (!iter->is_empty_list()) {
    throw_compiler_error(list, "Invalid list: {}", list.print());
  }
}

/*!
 * Convert a goos::Object that's a string to a std::string. Must be a string.
 */
std::string Compiler::as_string(const goos::Object& o) {
  return o.as_string()->data;
}

/*!
 * Convert a goos::Object that's a symbol to a std::string. Must be a string.
 */
std::string Compiler::symbol_string(const goos::Object& o) {
  return o.as_symbol()->name;
}

/*!
 * Convert a single quoted symbol into a std::string. Like 'hi -> "hi". Error if not a quoted
 * symbol.
 */
std::string Compiler::quoted_sym_as_string(const goos::Object& o) {
  auto args = get_va(o, o);
  va_check(o, args, {{goos::ObjectType::SYMBOL}, {goos::ObjectType::SYMBOL}}, {});
  if (symbol_string(args.unnamed.at(0)) != "quote") {
    throw_compiler_error(o, "Invalid quoted symbol: {}.", o.print());
  }
  return symbol_string(args.unnamed.at(1));
}

/*!
 * Get a thing that's quoted. Error if the thing isn't quoted.
 */
goos::Object Compiler::unquote(const goos::Object& o) {
  auto args = get_va(o, o);
  va_check(o, args, {{goos::ObjectType::SYMBOL}, {}}, {});
  if (symbol_string(args.unnamed.at(0)) != "quote") {
    throw_compiler_error(o, "Invalid quoted symbol: {}.", o.print());
  }
  return args.unnamed.at(1);
}

/*!
 * Determine if o is a quoted symbol like 'test.
 */
bool Compiler::is_quoted_sym(const goos::Object& o) {
  if (o.is_pair()) {
    auto car = pair_car(o);
    auto cdr = pair_cdr(o);
    if (car.is_symbol() && car.as_symbol()->name == "quote") {
      if (cdr.is_pair()) {
        auto thing = pair_car(cdr);
        if (thing.is_symbol()) {
          if (pair_cdr(cdr).is_empty_list()) {
            return true;
          }
        }
      }
    }
  }
  return false;
}

const goos::Object& Compiler::pair_car(const goos::Object& o) {
  return o.as_pair()->car;
}

const goos::Object& Compiler::pair_cdr(const goos::Object& o) {
  return o.as_pair()->cdr;
}

void Compiler::expect_empty_list(const goos::Object& o) {
  if (!o.is_empty_list()) {
    throw_compiler_error(o, "expected to be an empty list");
  }
}

TypeSpec Compiler::parse_typespec(const goos::Object& src, Env* env) {
  if (src.is_pair() && src.as_pair()->car.is_symbol("current-method-type") &&
      src.as_pair()->cdr.is_empty_list()) {
    return env->function_env()->method_of_type_name;
  }
  return ::parse_typespec(&m_ts, src);
}

bool Compiler::is_local_symbol(const goos::Object& obj, Env* env) {
  // check in the symbol macro env.
  auto mlet_env = env->symbol_macro_env();
  while (mlet_env) {
    if (mlet_env->macros.find(obj.as_symbol()) != mlet_env->macros.end()) {
      return true;
    }
    mlet_env = mlet_env->parent()->symbol_macro_env();
  }

  // check lexical
  if (env->lexical_lookup(obj)) {
    return true;
  }

  // check global constants
  if (m_global_constants.find(obj.as_symbol()) != m_global_constants.end()) {
    return true;
  }

  return false;
}

emitter::HWRegKind Compiler::get_preferred_reg_kind(const TypeSpec& ts) {
  switch (m_ts.lookup_type(ts)->get_preferred_reg_class()) {
    case RegClass::GPR_64:
      return emitter::HWRegKind::GPR;
    case RegClass::FLOAT:
      return emitter::HWRegKind::XMM;
    default:
      throw std::runtime_error("Unknown preferred register kind");
  }
}

bool Compiler::is_none(Val* in) {
  return dynamic_cast<None*>(in);
}

bool Compiler::is_basic(const TypeSpec& ts) {
  return m_ts.tc(m_ts.make_typespec("basic"), ts);
}

bool Compiler::is_structure(const TypeSpec& ts) {
  return m_ts.tc(m_ts.make_typespec("structure"), ts);
}

bool Compiler::is_bitfield(const TypeSpec& ts) {
  return m_ts.is_bitfield_type(ts.base_type());
}

bool Compiler::is_pair(const TypeSpec& ts) {
  return m_ts.tc(m_ts.make_typespec("pair"), ts);
}

bool Compiler::get_true_or_false(const goos::Object& form, const goos::Object& boolean) {
  // todo try other things.
  if (boolean.is_symbol()) {
    if (boolean.as_symbol()->name == "#t") {
      return true;
    }
    if (boolean.as_symbol()->name == "#f") {
      return false;
    }
  }
  throw_compiler_error(form, "The value {} cannot be used as a boolean.", boolean.print());
  return false;
}

std::vector<goos::Object> Compiler::get_list_as_vector(const goos::Object& o,
                                                       goos::Object* rest_out,
                                                       int max_length) {
  std::vector<goos::Object> result;

  auto* cur = &o;
  int n = 0;
  while (true) {
    if (max_length >= 0 && n >= max_length) {
      if (rest_out) {
        *rest_out = *cur;
      } else {
        throw std::runtime_error("get_list_as_vector would discard arguments");
      }
      return result;
    }

    if (cur->is_pair()) {
      result.push_back(cur->as_pair()->car);
      cur = &cur->as_pair()->cdr;
      n++;
    } else if (cur->is_empty_list()) {
      if (rest_out) {
        *rest_out = goos::Object::make_empty_list();
      }
      return result;
    }
  }
}

void Compiler::compile_constant_product(const goos::Object& form,
                                        RegVal* dest,
                                        RegVal* src,
                                        int stride,
                                        Env* env) {
  // todo - support imul with an imm.
  ASSERT(stride);

  bool is_power_of_two = (stride & (stride - 1)) == 0;
  if (stride == 1) {
    env->emit_ir<IR_RegSet>(form, dest, src);
  } else if (is_power_of_two) {
    for (int i = 0; i < 16; i++) {
      if (stride == (1 << i)) {
        env->emit_ir<IR_RegSet>(form, dest, src);
        env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::SHL_64, dest, i);
        return;
      }
    }
    ASSERT(false);
  } else {
    // get the multiplier
    env->emit_ir<IR_LoadConstant64>(form, dest, stride);
    env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::IMUL_32, dest, src);
  }
}
