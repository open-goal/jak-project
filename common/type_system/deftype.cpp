#include "deftype.h"

/*!
 * Missing Features
 * - Bitfields
 * - Int128 children
 * - Refer to yourself (structure/basic only)
 * - Evaluate constants.
 *
 */

namespace {
const goos::Object& car(const goos::Object* x) {
  if (!x->is_pair()) {
    throw std::runtime_error("invalid deftype form");
  }

  return x->as_pair()->car;
}

const goos::Object* cdr(const goos::Object* x) {
  if (!x->is_pair()) {
    throw std::runtime_error("invalid deftype form");
  }

  return &x->as_pair()->cdr;
}

std::string deftype_parent_list(const goos::Object& list) {
  if (!list.is_pair()) {
    throw std::runtime_error("invalid parent list in deftype: " + list.print());
  }

  auto parent = list.as_pair()->car;
  auto rest = list.as_pair()->cdr;
  if (!rest.is_empty_list()) {
    throw std::runtime_error("invalid parent list in deftype - can only have one parent");
  }

  if (!parent.is_symbol()) {
    throw std::runtime_error("invalid parent in deftype parent list");
  }

  return parent.as_symbol()->name;
}

bool is_type(const std::string& expected, const TypeSpec& actual, const TypeSystem* ts) {
  return ts->typecheck(ts->make_typespec(expected), actual, "", false, false);
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
    throw std::runtime_error("invalid list in for_each_in_list");
  }
}

std::string symbol_string(const goos::Object& obj) {
  if (obj.is_symbol()) {
    return obj.as_symbol()->name;
  }
  throw std::runtime_error(obj.print() + " was supposed to be a symbol, but isn't");
}

int get_int(const goos::Object& obj) {
  if (obj.is_int()) {
    return obj.integer_obj.value;
  }
  throw std::runtime_error(obj.print() + " was supposed to be an integer, but isn't");
}

TypeSpec parse_typespec(TypeSystem* type_system, const goos::Object& src) {
  if (src.is_symbol()) {
    return type_system->make_typespec(symbol_string(src));
  } else if (src.is_pair()) {
    TypeSpec ts = type_system->make_typespec(symbol_string(car(&src)));
    const auto& rest = *cdr(&src);

    for_each_in_list(rest,
                     [&](const goos::Object& o) { ts.add_arg(parse_typespec(type_system, o)); });

    return ts;
  } else {
    throw std::runtime_error("invalid typespec: " + src.print());
  }
  assert(false);
  return {};
}

void add_field(StructureType* structure, TypeSystem* ts, const goos::Object& def) {
  auto rest = &def;

  auto name = symbol_string(car(rest));
  rest = cdr(rest);

  auto type = parse_typespec(ts, car(rest));
  rest = cdr(rest);

  int array_size = -1;
  bool is_inline = false;
  bool is_dynamic = false;
  int offset_override = -1;
  int offset_assert = -1;

  if (!rest->is_empty_list()) {
    if (car(rest).is_int()) {
      array_size = car(rest).integer_obj.value;
      rest = cdr(rest);
    }

    while (!rest->is_empty_list()) {
      auto opt_name = symbol_string(car(rest));
      rest = cdr(rest);

      if (opt_name == ":inline") {
        is_inline = true;
      } else if (opt_name == ":dynamic") {
        is_dynamic = true;
      } else if (opt_name == ":offset") {
        offset_override = get_int(car(rest));
        rest = cdr(rest);
      } else if (opt_name == ":offset-assert") {
        offset_assert = get_int(car(rest));
        if (offset_assert == -1) {
          throw std::runtime_error("Cannot use -1 as offset-assert");
        }
        rest = cdr(rest);
      } else {
        throw std::runtime_error("Invalid option in field specification: " + opt_name);
      }
    }
  }

  int actual_offset = ts->add_field_to_type(structure, name, type, is_inline, is_dynamic,
                                            array_size, offset_override);
  if (offset_assert != -1 && actual_offset != offset_assert) {
    throw std::runtime_error("Field " + name + " was placed at " + std::to_string(actual_offset) +
                             " but offset-assert was set to " + std::to_string(offset_assert));
  }
}

void parse_structure_def(StructureType* type,
                         TypeSystem* ts,
                         const goos::Object& fields,
                         const goos::Object& options) {
  (void)options;
  for_each_in_list(fields, [&](const goos::Object& o) { add_field(type, ts, o); });

  //  auto* rest = &options;
  //  while (!rest->is_empty_list()) {
  //    auto opt_name = symbol_string(car(rest));
  //    rest = cdr(rest);
  //
  //    if (opt_name == ":inline") {
  //      is_inline = true;
  //    } else if (opt_name == ":dynamic") {
  //      is_dynamic = true;
  //    } else if (opt_name == ":offset") {
  //      offset_override = get_int(car(rest));
  //      rest = cdr(rest);
  //    } else if (opt_name == ":offset-assert") {
  //      offset_assert = get_int(car(rest));
  //      if(offset_assert == -1) {
  //        throw std::runtime_error("Cannot use -1 as offset-assert");
  //      }
  //      rest = cdr(rest);
  //    } else {
  //      throw std::runtime_error("Invalid option in field specification: " + opt_name);
  //    }
  //  }
}

}  // namespace

TypeSpec parse_deftype(const goos::Object& deftype, TypeSystem* ts) {
  auto iter = &deftype;

  auto& type_name_obj = car(iter);
  iter = cdr(iter);
  auto& parent_list_obj = car(iter);
  iter = cdr(iter);
  auto& field_list_obj = car(iter);
  iter = cdr(iter);
  auto& options_obj = *iter;

  if (!type_name_obj.is_symbol()) {
    throw std::runtime_error("deftype must be given a symbol as the type name");
  }

  auto name = type_name_obj.as_symbol()->name;
  auto parent_type_name = deftype_parent_list(parent_list_obj);
  auto parent_type = ts->make_typespec(parent_type_name);

  if (is_type("basic", parent_type, ts)) {
    auto new_type = std::make_unique<BasicType>(parent_type_name, name);
    auto pto = dynamic_cast<BasicType*>(ts->lookup_type(parent_type));
    assert(pto);
    new_type->inherit(pto);
    parse_structure_def(new_type.get(), ts, field_list_obj, options_obj);
    ts->add_type(name, std::move(new_type));
  } else if (is_type("structure", parent_type, ts)) {
    auto new_type = std::make_unique<StructureType>(parent_type_name, name);
    auto pto = dynamic_cast<StructureType*>(ts->lookup_type(parent_type));
    assert(pto);
    new_type->inherit(pto);
    parse_structure_def(new_type.get(), ts, field_list_obj, options_obj);
    ts->add_type(name, std::move(new_type));
  } else if (is_type("integer", parent_type, ts)) {
    throw std::runtime_error("Creating a child type of integer is not supported yet.");
  } else {
    throw std::runtime_error("Creating a child type from " + parent_type.print() +
                             " is not allowed or not supported yet.");
  }

  return ts->make_typespec(name);
}
