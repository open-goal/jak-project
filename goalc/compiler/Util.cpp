#include "goalc/compiler/Compiler.h"
#include "goalc/compiler/IR.h"
#include "common/goos/ParseHelpers.h"
#include "common/type_system/deftype.h"

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

TypeSpec Compiler::parse_typespec(const goos::Object& src) {
  return ::parse_typespec(&m_ts, src);
}

bool Compiler::is_local_symbol(const goos::Object& obj, Env* env) {
  // check in the symbol macro env.
  auto mlet_env = get_parent_env_of_type<SymbolMacroEnv>(env);
  while (mlet_env) {
    if (mlet_env->macros.find(obj.as_symbol()) != mlet_env->macros.end()) {
      return true;
    }
    mlet_env = get_parent_env_of_type<SymbolMacroEnv>(mlet_env->parent());
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

bool Compiler::try_getting_constant_integer(const goos::Object& in, int64_t* out, Env* env) {
  (void)env;
  if (in.is_int()) {
    *out = in.as_int();
    return true;
  }

  if (in.is_pair()) {
    auto head = in.as_pair()->car;
    if (head.is_symbol()) {
      auto head_sym = head.as_symbol();
      auto enum_type = m_ts.try_enum_lookup(head_sym->name);
      if (enum_type) {
        bool success;
        u64 as_enum = enum_lookup(in, enum_type, in.as_pair()->cdr, false, &success);
        if (success) {
          *out = as_enum;
          return true;
        }
      }

      if (head_sym->name == "size-of") {
        *out = get_size_for_size_of(in, in.as_pair()->cdr);
        return true;
      }
    }
  }

  if (in.is_symbol()) {
    auto global_constant = m_global_constants.find(in.as_symbol());
    if (global_constant != m_global_constants.end()) {
      // recursively get constant integer, so we can have constants set to constants, etc.
      if (try_getting_constant_integer(global_constant->second, out, env)) {
        return true;
      }
    }
  }

  return false;
}

bool Compiler::try_getting_constant_float(const goos::Object& in, float* out, Env* env) {
  (void)env;
  if (in.is_float()) {
    *out = in.as_float();
    return true;
  }

  // todo, try more things like constants before giving up.
  return false;
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
        *rest_out = goos::EmptyListObject::make_new();
      }
      return result;
    }
  }
}

void Compiler::compile_constant_product(RegVal* dest, RegVal* src, int stride, Env* env) {
  // todo - support imul with an imm.
  assert(stride);

  bool is_power_of_two = (stride & (stride - 1)) == 0;
  if (stride == 1) {
    env->emit_ir<IR_RegSet>(dest, src);
  } else if (is_power_of_two) {
    for (int i = 0; i < 16; i++) {
      if (stride == (1 << i)) {
        env->emit_ir<IR_RegSet>(dest, src);
        env->emit_ir<IR_IntegerMath>(IntegerMathKind::SHL_64, dest, i);
        return;
      }
    }
    assert(false);
  } else {
    // get the multiplier
    env->emit_ir<IR_LoadConstant64>(dest, stride);
    env->emit_ir<IR_IntegerMath>(IntegerMathKind::IMUL_32, dest, src);
  }
}