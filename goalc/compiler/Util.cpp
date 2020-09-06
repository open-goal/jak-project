#include "goalc/compiler/Compiler.h"

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
    const std::vector<util::MatchParam<goos::ObjectType>>& unnamed,
    const std::unordered_map<std::string, std::pair<bool, util::MatchParam<goos::ObjectType>>>&
        named) {
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

void Compiler::for_each_in_list(const goos::Object& list, const std::function<void(const goos::Object&)>& f) {
  const goos::Object* iter = &list;
  while(iter->is_pair()) {
    auto lap = iter->as_pair();
    f(lap->car);
    iter = &lap->cdr;
  }

  if(!iter->is_empty_list()) {
    throw_compile_error(list, "invalid list in for_each_in_list");
  }
}