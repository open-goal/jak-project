#include "goalc/compiler/Compiler.h"
#include "goalc/compiler/IR.h"

goos::Arguments Compiler::get_va(const goos::Object& form, const goos::Object& rest) {
  goos::Arguments args;
  // loop over forms in list
  goos::Object current = rest;
  while (!current.is_empty_list()) {
    auto arg = current.as_pair()->car;

    // did we get a ":keyword"
    if (arg.is_symbol() && arg.as_symbol()->name.at(0) == ':') {
      auto key_name = arg.as_symbol()->name.substr(1);

      // check for multiple definition of key
      if (args.named.find(key_name) != args.named.end()) {
        throw_compile_error(form, "Key argument " + key_name + " multiply defined");
      }

      // check for well-formed :key value expression
      current = current.as_pair()->cdr;
      if (current.is_empty_list()) {
        throw_compile_error(form, "Key argument didn't have a value");
      }

      args.named[key_name] = current.as_pair()->car;
    } else {
      // not a keyword. Add to unnamed or rest, depending on what we expect
      args.unnamed.push_back(arg);
    }
    current = current.as_pair()->cdr;
  }

  return args;
}

void Compiler::va_check(
    const goos::Object& form,
    const goos::Arguments& args,
    const std::vector<MatchParam<goos::ObjectType>>& unnamed,
    const std::unordered_map<std::string, std::pair<bool, MatchParam<goos::ObjectType>>>& named) {
  assert(args.rest.empty());
  if (unnamed.size() != args.unnamed.size()) {
    throw_compile_error(form, "Got " + std::to_string(args.unnamed.size()) +
                                  " arguments, but expected " + std::to_string(unnamed.size()));
  }

  for (size_t i = 0; i < unnamed.size(); i++) {
    if (unnamed[i] != args.unnamed[i].type) {
      assert(!unnamed[i].is_wildcard);
      throw_compile_error(form, "Argument " + std::to_string(i) + " has type " +
                                    object_type_to_string(args.unnamed[i].type) + " but " +
                                    object_type_to_string(unnamed[i].value) + " was expected");
    }
  }

  for (const auto& kv : named) {
    auto kv2 = args.named.find(kv.first);
    if (kv2 == args.named.end()) {
      // argument not given.
      if (kv.second.first) {
        // but was required
        throw_compile_error(form, "Required named argument \"" + kv.first + "\" was not found");
      }
    } else {
      // argument given.
      if (kv.second.second != kv2->second.type) {
        // but is wrong type
        assert(!kv.second.second.is_wildcard);
        throw_compile_error(form, "Argument \"" + kv.first + "\" has type " +
                                      object_type_to_string(kv2->second.type) + " but " +
                                      object_type_to_string(kv.second.second.value) +
                                      " was expected");
      }
    }
  }

  for (const auto& kv : args.named) {
    if (named.find(kv.first) == named.end()) {
      throw_compile_error(form, "Got unrecognized keyword argument \"" + kv.first + "\"");
    }
  }
}

void Compiler::for_each_in_list(const goos::Object& list,
                                const std::function<void(const goos::Object&)>& f) {
  const goos::Object* iter = &list;
  while (iter->is_pair()) {
    auto lap = iter->as_pair();
    f(lap->car);
    iter = &lap->cdr;
  }

  if (!iter->is_empty_list()) {
    throw_compile_error(list, "invalid list in for_each_in_list");
  }
}

std::string Compiler::as_string(const goos::Object& o) {
  return o.as_string()->data;
}

std::string Compiler::symbol_string(const goos::Object& o) {
  return o.as_symbol()->name;
}

std::string Compiler::quoted_sym_as_string(const goos::Object& o) {
  auto args = get_va(o, o);
  va_check(o, args, {{goos::ObjectType::SYMBOL}, {goos::ObjectType::SYMBOL}}, {});
  if (symbol_string(args.unnamed.at(0)) != "quote") {
    throw_compile_error(o, "invalid quoted symbol " + o.print());
  }
  return symbol_string(args.unnamed.at(1));
}

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
    throw_compile_error(o, "expected to be an empty list");
  }
}

TypeSpec Compiler::parse_typespec(const goos::Object& src) {
  if (src.is_symbol()) {
    return m_ts.make_typespec(symbol_string(src));
  } else if (src.is_pair()) {
    TypeSpec ts = m_ts.make_typespec(symbol_string(pair_car(src)));
    const auto& rest = pair_cdr(src);

    for_each_in_list(rest, [&](const goos::Object& o) { ts.add_arg(parse_typespec(o)); });

    return ts;
  } else {
    throw_compile_error(src, "invalid typespec");
  }
  assert(false);
  return {};
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

emitter::RegKind Compiler::get_preferred_reg_kind(const TypeSpec& ts) {
  switch (m_ts.lookup_type(ts)->get_preferred_reg_kind()) {
    case RegKind::GPR_64:
      return emitter::RegKind::GPR;
    case RegKind::FLOAT:
      return emitter::RegKind::XMM;
    default:
      throw std::runtime_error("Unknown preferred register kind");
  }
}

bool Compiler::is_none(Val* in) {
  return dynamic_cast<None*>(in);
}

bool Compiler::is_basic(const TypeSpec& ts) {
  return m_ts.typecheck(m_ts.make_typespec("basic"), ts, "", false, false);
}

bool Compiler::is_structure(const TypeSpec& ts) {
  return m_ts.typecheck(m_ts.make_typespec("structure"), ts, "", false, false);
}

bool Compiler::try_getting_constant_integer(const goos::Object& in, int64_t* out, Env* env) {
  (void)env;
  if (in.is_int()) {
    *out = in.as_int();
    return true;
  }

  // todo, try more things like constants before giving up.
  return false;
}

float Compiler::try_getting_constant_float(const goos::Object& in, float* out, Env* env) {
  (void)env;
  if (in.is_float()) {
    *out = in.as_float();
    return true;
  }

  // todo, try more things like constants before giving up.
  return false;
}