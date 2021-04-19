/*!
 * @file defenum.cpp
 * Parser for the GOAL "defenum" form.
 * This is used both in the compiler and in the decompiler for the type definition file.
 */

#include "Enum.h"
#include "defenum.h"
#include "deftype.h"
#include "third-party/fmt/core.h"

namespace {
const goos::Object& car(const goos::Object* x) {
  if (!x->is_pair()) {
    throw std::runtime_error("invalid defenum form");
  }

  return x->as_pair()->car;
}

const goos::Object* cdr(const goos::Object* x) {
  if (!x->is_pair()) {
    throw std::runtime_error("invalid defenum form");
  }

  return &x->as_pair()->cdr;
}

bool is_type(const std::string& expected, const TypeSpec& actual, const TypeSystem* ts) {
  return ts->tc(ts->make_typespec(expected), actual);
}

bool integer_fits(s64 in, int size, bool is_signed) {
  switch (size) {
    case 1:
      if (is_signed) {
        return in >= INT8_MIN && in <= INT8_MAX;
      } else {
        return in >= 0 && in <= UINT8_MAX;
      }
    case 2:
      if (is_signed) {
        return in >= INT16_MIN && in <= INT16_MAX;
      } else {
        return in >= 0 && in <= UINT16_MAX;
      }
    case 4:
      if (is_signed) {
        return in >= INT32_MIN && in <= INT32_MAX;
      } else {
        return in >= 0 && in <= UINT32_MAX;
      }
    case 8:
      return true;
    default:
      assert(false);
      return false;
  }
}

template <typename T>
void for_each_in_list(const goos::Object& list, T f) {
  const goos::Object* iter = &list;
  while (iter->is_pair()) {
    auto lap = iter->as_pair();
    f(lap->car);
    iter = &lap->cdr;
  }

  if (!iter->is_empty_list()) {
    throw std::runtime_error("invalid list in for_each_in_list: " + list.print());
  }
}

std::string symbol_string(const goos::Object& obj) {
  if (obj.is_symbol()) {
    return obj.as_symbol()->name;
  }
  throw std::runtime_error(obj.print() + " was supposed to be a symbol, but isn't");
}

}  // namespace

void parse_defenum(const goos::Object& defenum, TypeSystem* ts, GoalEnum& goalenum) {
  // default enum type will be int32.
  goalenum.base_type = ts->make_typespec("int32");
  goalenum.is_bitfield = false;

  auto iter = &defenum;

  auto& enum_name_obj = car(iter);
  iter = cdr(iter);

  if (!enum_name_obj.is_symbol()) {
    throw std::runtime_error("defenum must be given a symbol as its name");
  }
  goalenum.name = enum_name_obj.as_symbol()->name;

  auto current = car(iter);
  while (current.is_symbol() && symbol_string(current).at(0) == ':') {
    auto option_name = symbol_string(current);
    iter = cdr(iter);
    auto option_value = car(iter);
    iter = cdr(iter);
    current = car(iter);

    if (option_name == ":type") {
      goalenum.base_type = parse_typespec(ts, option_value);
    } else if (option_name == ":bitfield") {
      if (symbol_string(option_value) == "#t") {
        goalenum.is_bitfield = true;
      } else if (symbol_string(option_value) == "#f") {
        goalenum.is_bitfield = false;
      } else {
        fmt::print("Invalid option {} to :bitfield option.\n", option_value.print());
        throw std::runtime_error("invalid bitfield option");
      }
    } else {
      fmt::print("Unknown option {} for defenum.\n", option_name);
      throw std::runtime_error("unknown option for defenum");
    }
  }

  auto type = ts->lookup_type(goalenum.base_type);
  while (!iter->is_empty_list()) {
    auto field = car(iter);
    auto name = symbol_string(car(&field));
    auto rest = cdr(&field);
    auto& value = car(rest);
    if (!value.is_int()) {
      fmt::print("Expected integer for enum value, got {}\n", value.print());
    }

    auto entry_val = value.integer_obj.value;
    if (!integer_fits(entry_val, type->get_load_size(), type->get_load_signed())) {
      fmt::print("Integer {} does not fit inside a {}\n", entry_val, type->get_name());
    }

    rest = cdr(rest);
    if (!rest->is_empty_list()) {
      fmt::print("Got too many items in defenum {} entry {}\n", goalenum.name, name);
    }

    goalenum.entries[name] = entry_val;
    iter = cdr(iter);
  }

  if (is_type("integer", goalenum.base_type, ts)) {
    ts->add_enum_type(goalenum.name, goalenum.base_type.base_type());
  } else {
    throw std::runtime_error("Creating an enum with type " + goalenum.base_type.print() +
                             " is not allowed or not supported yet.");
  }
}
