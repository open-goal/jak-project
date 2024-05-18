/*!
 * @file defenum.cpp
 * Parser for the GOAL "defenum" form.
 * This is used both in the compiler and in the decompiler for the type definition file.
 */

#include "defenum.h"

#include "deftype.h"

#include "common/goos/ParseHelpers.h"
#include "common/log/log.h"
#include "common/util/BitUtils.h"
#include "common/util/string_util.h"

#include "fmt/core.h"

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

std::string symbol_string(const goos::Object& obj) {
  if (obj.is_symbol()) {
    return obj.as_symbol().name_ptr;
  }
  throw std::runtime_error(obj.print() + " was supposed to be a symbol, but isn't");
}

}  // namespace

EnumType* parse_defenum(const goos::Object& defenum,
                        TypeSystem* ts,
                        DefinitionMetadata* symbol_metadata) {
  // default enum type will be int64
  TypeSpec base_type = ts->make_typespec("int64");
  bool is_bitfield = false;
  std::unordered_map<std::string, s64> entries;

  auto iter = &defenum;

  auto& enum_name_obj = car(iter);
  iter = cdr(iter);
  // check for docstring
  std::optional<std::string> maybe_docstring;
  if (iter->is_pair() && car(iter).is_string()) {
    if (symbol_metadata) {
      maybe_docstring = str_util::trim_newline_indents(car(iter).as_string()->data);
      symbol_metadata->docstring = maybe_docstring.value();
    }
    iter = cdr(iter);
  }

  if (!enum_name_obj.is_symbol()) {
    throw std::runtime_error("defenum must be given a symbol as its name");
  }
  std::string name = enum_name_obj.as_symbol().name_ptr;

  auto current = car(iter);
  while (current.is_symbol() && symbol_string(current).at(0) == ':') {
    auto option_name = symbol_string(current);
    iter = cdr(iter);
    auto& option_value = car(iter);
    iter = cdr(iter);

    if (option_name == ":type") {
      base_type = parse_typespec(ts, option_value);
    } else if (option_name == ":bitfield") {
      if (symbol_string(option_value) == "#t") {
        is_bitfield = true;
      } else if (symbol_string(option_value) == "#f") {
        is_bitfield = false;
      } else {
        throw std::runtime_error(
            fmt::format("Invalid option {} to :bitfield option.\n", option_value.print()));
      }
    } else if (option_name == ":copy-entries") {
      auto other_info = ts->try_enum_lookup(parse_typespec(ts, option_value));
      if (!other_info) {
        throw std::runtime_error(fmt::format(
            "Cannot copy entries from {}, it is not a valid enum type", option_value.print()));
      }
      for (auto& e : other_info->entries()) {
        if (entries.find(e.first) != entries.end()) {
          throw std::runtime_error(fmt::format("Entry {} appears multiple times", e.first));
        }
        entries[e.first] = e.second;
      }
    } else {
      throw std::runtime_error(fmt::format("Unknown option {} for defenum.\n", option_name));
    }

    if (iter->is_pair()) {
      current = car(iter);
    } else {
      break;
    }
  }

  auto type = ts->lookup_type(base_type);
  s64 highest = -1;
  while (!iter->is_empty_list()) {
    auto field = car(iter);
    auto entry_name = symbol_string(car(&field));

    if (entries.find(entry_name) != entries.end()) {
      throw std::runtime_error(fmt::format("Entry {} appears multiple times.", entry_name));
    }

    auto rest = cdr(&field);
    if (!rest->is_empty_list()) {
      auto& value = car(rest);
      if (!value.is_int()) {
        throw std::runtime_error(
            fmt::format("Expected integer for enum value, got {}\n", value.print()));
      }

      auto entry_val = value.integer_obj.value;
      if (!integer_fits(entry_val, type->get_load_size(), type->get_load_signed())) {
        lg::warn("Integer {} does not fit inside a {}", entry_val, type->get_name());
      }

      if (!entries.size()) {
        highest = entry_val;
      }
      highest = std::max(highest, entry_val);

      rest = cdr(rest);
      if (!rest->is_empty_list()) {
        throw std::runtime_error(
            fmt::format("Got too many items in defenum {} entry {}\n", name, entry_name));
      }

      entries[entry_name] = entry_val;
    } else {
      entries[entry_name] = ++highest;
    }

    iter = cdr(iter);
  }

  if (is_type("integer", base_type, ts)) {
    auto parent = ts->get_type_of_type<ValueType>(base_type.base_type());
    auto new_type = std::make_unique<EnumType>(parent, name, is_bitfield, entries);
    new_type->m_metadata.docstring = maybe_docstring;
    new_type->set_runtime_type(parent->get_runtime_name());
    return dynamic_cast<EnumType*>(ts->add_type(name, std::move(new_type)));
  } else {
    throw std::runtime_error("Creating an enum with type " + base_type.print() +
                             " is not allowed or not supported yet.");
  }
}
