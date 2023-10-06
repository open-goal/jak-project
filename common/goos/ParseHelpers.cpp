#include "ParseHelpers.h"

#include "third-party/fmt/core.h"

namespace goos {

bool get_va(const goos::Object& rest, std::string* err_string, goos::Arguments* result) {
  goos::Arguments args;
  // loop over forms in list
  goos::Object current = rest;
  while (!current.is_empty_list()) {
    auto arg = current.as_pair()->car;

    // did we get a ":keyword"
    if (arg.is_symbol() && arg.as_symbol().name_ptr[0] == ':') {
      auto key_name = arg.as_symbol().name_ptr + 1;

      // check for multiple definition of key
      if (args.named.find(key_name) != args.named.end()) {
        *err_string = fmt::format("Key argument {} multiply defined", key_name);
        return false;
      }

      // check for well-formed :key value expression
      current = current.as_pair()->cdr;
      if (current.is_empty_list()) {
        *err_string = "Key argument didn't have a value";
        return false;
      }

      args.named[key_name] = current.as_pair()->car;
    } else {
      // not a keyword. Add to unnamed or rest, depending on what we expect
      args.unnamed.push_back(arg);
    }
    current = current.as_pair()->cdr;
  }
  *result = args;
  return true;
}

void get_va_no_named(const goos::Object& rest, goos::Arguments* result) {
  goos::Arguments args;
  // loop over forms in list
  goos::Object current = rest;
  while (!current.is_empty_list()) {
    args.unnamed.push_back(current.as_pair()->car);
    current = current.as_pair()->cdr;
  }
  *result = args;
}

bool va_check(
    const goos::Arguments& args,
    const std::vector<std::optional<goos::ObjectType>>& unnamed,
    const std::unordered_map<std::string, std::pair<bool, std::optional<goos::ObjectType>>>& named,
    std::string* err_string) {
  ASSERT(args.rest.empty());
  if (unnamed.size() != args.unnamed.size()) {
    *err_string = fmt::format("Got {} arguments, but expected {}",
                              std::to_string(args.unnamed.size()), std::to_string(unnamed.size()));
    return false;
  }

  for (size_t i = 0; i < unnamed.size(); i++) {
    if (unnamed[i].has_value() && unnamed[i] != args.unnamed[i].type) {
      // special case -- an empty list is a valid pair
      *err_string = fmt::format("Argument {} has type {} but {} was expected\nArgument is: {}", i,
                                object_type_to_string(args.unnamed[i].type),
                                object_type_to_string(unnamed[i].value()), args.unnamed[i].print());
      return false;
    }
  }

  for (const auto& kv : named) {
    auto kv2 = args.named.find(kv.first);
    if (kv2 == args.named.end()) {
      // argument not given.
      if (kv.second.first) {
        // but was required
        *err_string = "Required named argument \"" + kv.first + "\" was not found";
        return false;
      }
    } else {
      // argument given.
      if (kv.second.second.has_value() && kv.second.second != kv2->second.type) {
        // but is wrong type
        *err_string = "Argument \"" + kv.first + "\" has type " +
                      object_type_to_string(kv2->second.type) + " but " +
                      object_type_to_string(kv.second.second.value()) + " was expected";
        return false;
      }
    }
  }

  for (const auto& kv : args.named) {
    if (named.find(kv.first) == named.end()) {
      *err_string = "Got unrecognized keyword argument \"" + kv.first + "\"";
      return false;
    }
  }

  return true;
}

int list_length(const goos::Object& list) {
  int len = 0;
  for_each_in_list(list, [&](const goos::Object& x) {
    (void)x;
    len++;
  });
  return len;
}

}  // namespace goos
