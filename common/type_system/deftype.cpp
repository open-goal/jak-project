/*!
 * @file deftype.cpp
 * Parser for the GOAL "deftype" form.
 * This is used both in the compiler and in the decompiler for the type definition file.
 */

#include "common/goos/ParseHelpers.h"
#include "deftype.h"
#include "third-party/fmt/core.h"

/*!
 * Missing Features
 * - Bitfields
 * - Int128 children
 * - Refer to yourself (structure/basic only)
 * - Method List
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
  return ts->tc(ts->make_typespec(expected), actual);
}

std::string symbol_string(const goos::Object& obj) {
  if (obj.is_symbol()) {
    return obj.as_symbol()->name;
  }
  throw std::runtime_error(obj.print() + " was supposed to be a symbol, but isn't");
}

int64_t get_int(const goos::Object& obj) {
  if (obj.is_int()) {
    return obj.integer_obj.value;
  }
  throw std::runtime_error(obj.print() + " was supposed to be an integer, but isn't");
}

double get_float(const goos::Object& obj) {
  if (obj.is_int()) {
    return obj.integer_obj.value;
  } else if (obj.is_float()) {
    return obj.float_obj.value;
  }
  throw std::runtime_error(obj.print() + " was supposed to be an number, but isn't");
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
  double score = 0;
  bool skip_in_decomp = false;

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
      } else if (opt_name == ":score") {
        score = get_float(car(rest));
        rest = cdr(rest);
      } else if (opt_name == ":offset-assert") {
        offset_assert = get_int(car(rest));
        if (offset_assert == -1) {
          throw std::runtime_error("Cannot use -1 as offset-assert");
        }
        rest = cdr(rest);
      } else if (opt_name == ":do-not-decompile") {
        skip_in_decomp = true;
      } else {
        throw std::runtime_error("Invalid option in field specification: " + opt_name);
      }
    }
  }

  int actual_offset = ts->add_field_to_type(structure, name, type, is_inline, is_dynamic,
                                            array_size, offset_override, skip_in_decomp, score);
  if (offset_assert != -1 && actual_offset != offset_assert) {
    throw std::runtime_error("Field " + name + " was placed at " + std::to_string(actual_offset) +
                             " but offset-assert was set to " + std::to_string(offset_assert));
  }
}

void add_bitfield(BitFieldType* bitfield_type, TypeSystem* ts, const goos::Object& def) {
  auto rest = &def;

  auto name = symbol_string(car(rest));
  rest = cdr(rest);

  auto type = parse_typespec(ts, car(rest));
  rest = cdr(rest);

  int offset_override = -1;
  int size_override = -1;

  if (!rest->is_empty_list()) {
    while (!rest->is_empty_list()) {
      auto opt_name = symbol_string(car(rest));
      rest = cdr(rest);

      if (opt_name == ":offset") {
        offset_override = get_int(car(rest));
        rest = cdr(rest);
      } else if (opt_name == ":size") {
        size_override = get_int(car(rest));
        rest = cdr(rest);
      } else {
        throw std::runtime_error("Invalid option in field specification: " + opt_name);
      }
    }
  }

  if (offset_override == -1) {
    throw std::runtime_error("Bitfield type must manually specify offsets always");
  }

  // it's fine if the size is -1, that means it'll just use the type's size.
  ts->add_field_to_bitfield(bitfield_type, name, type, offset_override, size_override);
}

void declare_method(Type* type, TypeSystem* type_system, const goos::Object& def) {
  for_each_in_list(def, [&](const goos::Object& _obj) {
    auto obj = &_obj;
    // (name args return-type [:no-virtual] [:replace] [:state] [id])
    auto method_name = symbol_string(car(obj));
    obj = cdr(obj);
    auto& args = car(obj);
    obj = cdr(obj);
    auto& return_type = car(obj);
    obj = cdr(obj);

    bool no_virtual = false;
    bool replace_method = false;
    TypeSpec function_typespec("function");

    if (!obj->is_empty_list() && car(obj).is_symbol(":no-virtual")) {
      obj = cdr(obj);
      no_virtual = true;
    }

    if (!obj->is_empty_list() && car(obj).is_symbol(":replace")) {
      obj = cdr(obj);
      replace_method = true;
    }

    if (!obj->is_empty_list() && car(obj).is_symbol(":state")) {
      obj = cdr(obj);
      function_typespec = TypeSpec("state");
    }

    if (!obj->is_empty_list() && car(obj).is_symbol(":behavior")) {
      obj = cdr(obj);
      function_typespec.add_new_tag("behavior", symbol_string(obj->as_pair()->car));
      obj = cdr(obj);
    }

    int id = -1;
    if (!obj->is_empty_list() && car(obj).is_int()) {
      auto& id_obj = car(obj);
      id = get_int(id_obj);
      obj = cdr(obj);
    }

    if (!obj->is_empty_list()) {
      throw std::runtime_error("too many things in method def: " + def.print());
    }

    for_each_in_list(args, [&](const goos::Object& o) {
      function_typespec.add_arg(parse_typespec(type_system, o));
    });
    function_typespec.add_arg(parse_typespec(type_system, return_type));

    auto info = type_system->declare_method(type, method_name, no_virtual, function_typespec,
                                            replace_method);

    // check the method assert
    if (id != -1) {
      // method id assert!
      if (id != info.id) {
        printf("WARNING - ID assert failed on method %s of type %s (wanted %d got %d)\n",
               method_name.c_str(), type->get_name().c_str(), id, info.id);
        throw std::runtime_error("Method ID assert failed");
      }
    }
  });
}

void declare_state(Type* type, TypeSystem* type_system, const goos::Object& def) {
  for_each_in_list(def, [&](const goos::Object& _obj) {
    auto obj = &_obj;
    if (obj->is_list()) {
      // (name ,@args)
      auto state_name = symbol_string(car(obj));
      auto args = cdr(obj);

      TypeSpec state_typespec("state");

      for_each_in_list(*args, [&](const goos::Object& o) {
        state_typespec.add_arg(parse_typespec(type_system, o));
      });
      state_typespec.add_arg(TypeSpec(type->get_name()));

      type->add_state(state_name, state_typespec);
    } else {
      // name
      auto state_name = symbol_string(*obj);

      TypeSpec state_typespec("state");
      state_typespec.add_arg(TypeSpec(type->get_name()));

      type->add_state(state_name, state_typespec);
    }
  });
}

struct StructureDefResult {
  TypeFlags flags;
  bool generate_runtime_type = true;
  bool pack_me = false;
  bool allow_misaligned = false;
  bool final = false;
};

StructureDefResult parse_structure_def(StructureType* type,
                                       TypeSystem* ts,
                                       const goos::Object& fields,
                                       const goos::Object& options) {
  StructureDefResult result;
  for_each_in_list(fields, [&](const goos::Object& o) { add_field(type, ts, o); });
  TypeFlags flags;
  flags.heap_base = 0;

  flags.size = type->get_size_in_memory();
  flags.pad = 0;

  auto* rest = &options;
  int size_assert = -1;
  int method_count_assert = -1;
  uint64_t flag_assert = 0;
  bool flag_assert_set = false;
  while (!rest->is_empty_list()) {
    if (car(rest).is_pair()) {
      auto opt_list = &car(rest);
      auto& first = car(opt_list);
      opt_list = cdr(opt_list);

      auto list_name = symbol_string(first);
      if (list_name == ":methods") {
        declare_method(type, ts, *opt_list);
      } else if (list_name == ":states") {
        declare_state(type, ts, *opt_list);
      } else {
        throw std::runtime_error("Invalid option list in field specification: " +
                                 car(rest).print());
      }

      rest = cdr(rest);
    } else {
      auto opt_name = symbol_string(car(rest));
      rest = cdr(rest);

      if (opt_name == ":size-assert") {
        size_assert = get_int(car(rest));
        if (size_assert == -1) {
          throw std::runtime_error("Cannot use -1 as size-assert");
        }
        rest = cdr(rest);
      } else if (opt_name == ":method-count-assert") {
        method_count_assert = get_int(car(rest));
        if (method_count_assert == -1) {
          throw std::runtime_error("Cannot use -1 as method_count_assert");
        }
        rest = cdr(rest);
      } else if (opt_name == ":flag-assert") {
        flag_assert = get_int(car(rest));
        flag_assert_set = true;
        rest = cdr(rest);
      } else if (opt_name == ":no-runtime-type") {
        result.generate_runtime_type = false;
      } else if (opt_name == ":pack-me") {
        result.pack_me = true;
      } else if (opt_name == ":heap-base") {
        u16 hb = get_int(car(rest));
        rest = cdr(rest);
        flags.heap_base = hb;
      } else if (opt_name == ":allow-misaligned") {
        result.allow_misaligned = true;
      } else if (opt_name == ":final") {
        result.final = true;
      } else {
        throw std::runtime_error("Invalid option in field specification: " + opt_name);
      }
    }
  }

  if (size_assert != -1 && flags.size != u16(size_assert)) {
    throw std::runtime_error("Type " + type->get_name() + " came out to size " +
                             std::to_string(int(flags.size)) + " but size-assert was set to " +
                             std::to_string(size_assert));
  }

  flags.methods = ts->get_next_method_id(type);

  if (method_count_assert != -1 && flags.methods != u16(method_count_assert)) {
    throw std::runtime_error(
        "Type " + type->get_name() + " has " + std::to_string(int(flags.methods)) +
        " methods, but method-count-assert was set to " + std::to_string(method_count_assert));
  }

  if (flag_assert_set && (flags.flag != flag_assert)) {
    throw std::runtime_error(
        fmt::format("Type {} has flag 0x{:x} but flag-assert was set to 0x{:x}", type->get_name(),
                    flags.flag, flag_assert));
  }

  if (result.flags.heap_base) {
    int heap_start_1 = 112 + result.flags.heap_base;
    int heap_start_2 = (result.flags.size + 15) & (~15);
    if (heap_start_1 != heap_start_2) {
      throw std::runtime_error(fmt::format("Heap base bad on {}: {} vs {}\n", type->get_name(),
                                           heap_start_1, heap_start_2));
    }
  }

  result.flags = flags;
  return result;
}

struct BitFieldTypeDefResult {
  TypeFlags flags;
  bool generate_runtime_type = true;
};

BitFieldTypeDefResult parse_bitfield_type_def(BitFieldType* type,
                                              TypeSystem* ts,
                                              const goos::Object& fields,
                                              const goos::Object& options) {
  BitFieldTypeDefResult result;
  for_each_in_list(fields, [&](const goos::Object& o) { add_bitfield(type, ts, o); });
  TypeFlags flags;
  flags.heap_base = 0;
  flags.size = type->get_size_in_memory();
  flags.pad = 0;

  auto* rest = &options;
  int size_assert = -1;
  int method_count_assert = -1;
  uint64_t flag_assert = 0;
  bool flag_assert_set = false;
  while (!rest->is_empty_list()) {
    if (car(rest).is_pair()) {
      auto opt_list = &car(rest);
      auto& first = car(opt_list);
      opt_list = cdr(opt_list);

      if (symbol_string(first) == ":methods") {
        declare_method(type, ts, *opt_list);
      } else {
        throw std::runtime_error("Invalid option list in field specification: " +
                                 car(rest).print());
      }

      rest = cdr(rest);
    } else {
      auto opt_name = symbol_string(car(rest));
      rest = cdr(rest);

      if (opt_name == ":size-assert") {
        size_assert = get_int(car(rest));
        if (size_assert == -1) {
          throw std::runtime_error("Cannot use -1 as size-assert");
        }
        rest = cdr(rest);
      } else if (opt_name == ":method-count-assert") {
        method_count_assert = get_int(car(rest));
        if (method_count_assert == -1) {
          throw std::runtime_error("Cannot use -1 as method_count_assert");
        }
        rest = cdr(rest);
      } else if (opt_name == ":flag-assert") {
        flag_assert = get_int(car(rest));
        flag_assert_set = true;
        rest = cdr(rest);
      } else if (opt_name == ":no-runtime-type") {
        result.generate_runtime_type = false;
      } else if (opt_name == ":heap-base") {
        u16 hb = get_int(car(rest));
        rest = cdr(rest);
        flags.heap_base = hb;
      } else {
        throw std::runtime_error("Invalid option in field specification: " + opt_name);
      }
    }
  }

  if (size_assert != -1 && flags.size != u16(size_assert)) {
    throw std::runtime_error("Type " + type->get_name() + " came out to size " +
                             std::to_string(int(flags.size)) + " but size-assert was set to " +
                             std::to_string(size_assert));
  }

  flags.methods = ts->get_next_method_id(type);

  if (method_count_assert != -1 && flags.methods != u16(method_count_assert)) {
    throw std::runtime_error(
        "Type " + type->get_name() + " has " + std::to_string(int(flags.methods)) +
        " methods, but method-count-assert was set to " + std::to_string(method_count_assert));
  }

  if (flag_assert_set && (flags.flag != flag_assert)) {
    throw std::runtime_error(
        fmt::format("Type {} has flag 0x{:x} but flag-assert was set to 0x{:x}", type->get_name(),
                    flags.flag, flag_assert));
  }

  result.flags = flags;
  return result;
}

}  // namespace

TypeSpec parse_typespec(const TypeSystem* type_system, const goos::Object& src) {
  if (src.is_symbol()) {
    return type_system->make_typespec(symbol_string(src));
  } else if (src.is_pair()) {
    TypeSpec ts = type_system->make_typespec(symbol_string(car(&src)));
    const auto* rest = cdr(&src);

    while (rest->is_pair()) {
      auto& it = rest->as_pair()->car;

      if (it.is_symbol() && it.as_symbol()->name.at(0) == ':') {
        auto tag_name = it.as_symbol()->name.substr(1);
        rest = &rest->as_pair()->cdr;

        if (!rest->is_pair()) {
          throw std::runtime_error("TypeSpec missing tag value");
        }

        auto& tag_val = rest->as_pair()->car;

        if (tag_name == "behavior") {
          if (!type_system->fully_defined_type_exists(tag_val.as_symbol()->name) &&
              !type_system->partially_defined_type_exists(tag_val.as_symbol()->name)) {
            throw std::runtime_error(
                fmt::format("Behavior tag uses an unknown type {}", tag_val.as_symbol()->name));
          }
          ts.add_new_tag(tag_name, tag_val.as_symbol()->name);
        } else {
          throw std::runtime_error(fmt::format("Type tag {} is unknown", tag_name));
        }

      } else {
        // normal argument.
        ts.add_arg(parse_typespec(type_system, it));
      }

      rest = &rest->as_pair()->cdr;
    }

    return ts;
  } else {
    throw std::runtime_error("invalid typespec: " + src.print());
  }
  assert(false);
  return {};
}

DeftypeResult parse_deftype(const goos::Object& deftype, TypeSystem* ts) {
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
  DeftypeResult result;

  if (is_type("basic", parent_type, ts)) {
    auto new_type = std::make_unique<BasicType>(parent_type_name, name, false, 0);
    auto pto = dynamic_cast<BasicType*>(ts->lookup_type(parent_type));
    assert(pto);
    if (pto->final()) {
      throw std::runtime_error(
          fmt::format("[TypeSystem] Cannot make a child type {} of final basic type {}", name,
                      parent_type_name));
    }
    new_type->inherit(pto);
    ts->forward_declare_type_as(name, "basic");
    auto sr = parse_structure_def(new_type.get(), ts, field_list_obj, options_obj);
    result.flags = sr.flags;
    result.create_runtime_type = sr.generate_runtime_type;
    if (sr.pack_me) {
      new_type->set_pack(true);
    }
    if (sr.allow_misaligned) {
      fmt::print(
          "[TypeSystem] :allow-misaligned was set on {}, which is a basic and cannot "
          "be misaligned\n",
          name);
      throw std::runtime_error("invalid pack option on basic");
    }
    new_type->set_heap_base(result.flags.heap_base);
    if (sr.final) {
      new_type->set_final();
    }
    ts->add_type(name, std::move(new_type));
  } else if (is_type("structure", parent_type, ts)) {
    auto new_type = std::make_unique<StructureType>(parent_type_name, name, false, false, false, 0);
    auto pto = dynamic_cast<StructureType*>(ts->lookup_type(parent_type));
    assert(pto);
    new_type->inherit(pto);
    ts->forward_declare_type_as(name, "structure");
    auto sr = parse_structure_def(new_type.get(), ts, field_list_obj, options_obj);
    result.flags = sr.flags;
    result.create_runtime_type = sr.generate_runtime_type;
    if (sr.pack_me) {
      new_type->set_pack(true);
    }
    if (sr.allow_misaligned) {
      new_type->set_allow_misalign(true);
    }
    if (sr.final) {
      throw std::runtime_error(
          fmt::format("[TypeSystem] :final option cannot be used on structure type {}", name));
    }
    new_type->set_heap_base(result.flags.heap_base);
    ts->add_type(name, std::move(new_type));
  } else if (is_type("integer", parent_type, ts)) {
    auto pto = ts->lookup_type(parent_type);
    assert(pto);
    auto new_type = std::make_unique<BitFieldType>(
        parent_type_name, name, pto->get_size_in_memory(), pto->get_load_signed());
    auto parent_value = dynamic_cast<ValueType*>(pto);
    assert(parent_value);
    new_type->inherit(parent_value);
    new_type->set_runtime_type(pto->get_runtime_name());
    auto sr = parse_bitfield_type_def(new_type.get(), ts, field_list_obj, options_obj);
    result.flags = sr.flags;
    result.create_runtime_type = sr.generate_runtime_type;
    ts->add_type(name, std::move(new_type));
  } else {
    throw std::runtime_error("Creating a child type from " + parent_type.print() +
                             " is not allowed or not supported yet.");
  }

  result.type = ts->make_typespec(name);
  result.type_info = ts->lookup_type(result.type);
  return result;
}
