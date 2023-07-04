/*!
 * @file deftype.cpp
 * Parser for the GOAL "deftype" form.
 * This is used both in the compiler and in the decompiler for the type definition file.
 */

#include "deftype.h"

#include <unordered_map>

#include "common/goos/ParseHelpers.h"
#include "common/log/log.h"
#include "common/type_system/state.h"
#include "common/util/string_util.h"

#include "third-party/fmt/core.h"

/*!
 * Missing Features
 * - Int128 children
 * - Refer to yourself (structure/basic only)
 * - Method List
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

void add_field(StructureType* structure,
               TypeSystem* ts,
               const goos::Object& def,
               std::unordered_map<goos::HeapObject*, goos::Object>& constants) {
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
  std::optional<TypeSpec> decomp_as_ts = std::nullopt;
  Field override_field;
  bool override = false;

  if (!rest->is_empty_list()) {
    if (car(rest).is_int()) {
      array_size = car(rest).integer_obj.value;
      rest = cdr(rest);
    } else if (car(rest).is_symbol() &&
               constants.find((car(rest)).as_symbol()) != constants.end()) {
      array_size = get_int(constants[(car(rest)).as_symbol()]);
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
      } else if (opt_name == ":override") {
        override = true;
        if (!structure->lookup_field(name, &override_field)) {
          throw std::runtime_error(fmt::format("Field {} not found to override", name));
        }
      } else if (opt_name == ":overlay-at") {
        auto field_name = symbol_string(car(rest));
        Field overlay_field;
        if (!structure->lookup_field(field_name, &overlay_field)) {
          throw std::runtime_error(
              fmt::format("Field {} not found to overlay for {}", field_name, name));
        }
        offset_override = overlay_field.offset();
        rest = cdr(rest);
      } else if (opt_name == ":score") {
        score = get_float(car(rest));
        rest = cdr(rest);
      } else if (opt_name == ":decomp-as") {
        decomp_as_ts = TypeSpec(symbol_string(car(rest)));
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

  if (override && name == override_field.name()) {
    // override field, e.g. (root collide-shape :overlay-at root)
    // does not add new field, merely rewrites inherited one
    if (!ts->tc(override_field.type(), type)) {
      throw std::runtime_error(
          fmt::format("Wanted to override field {}, but type {} isn't child of {}",
                      override_field.name(), type.print(), override_field.type().print()));
    }
    if (override_field.is_inline() != is_inline || override_field.is_dynamic() != is_dynamic ||
        (array_size != -1 && override_field.array_size() != array_size)) {
      throw std::runtime_error(
          fmt::format("Wanted to override field {}, but some parameters were different",
                      override_field.name()));
    }
    structure->override_field_type(override_field.name(), type);
  } else {
    // new unique field
    int actual_offset =
        ts->add_field_to_type(structure, name, type, is_inline, is_dynamic, array_size,
                              offset_override, skip_in_decomp, score, decomp_as_ts);
    if (offset_assert != -1 && actual_offset != offset_assert) {
      throw std::runtime_error("Field " + name + " was placed at " + std::to_string(actual_offset) +
                               " but offset-assert was set to " + std::to_string(offset_assert));
    }
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
  bool skip_in_decomp = false;

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
      } else if (opt_name == ":do-not-decompile") {
        skip_in_decomp = true;
      } else {
        throw std::runtime_error("Invalid option in field specification: " + opt_name);
      }
    }
  }

  if (offset_override == -1) {
    throw std::runtime_error("Bitfield type must manually specify offsets always");
  }

  // it's fine if the size is -1, that means it'll just use the type's size.
  ts->add_field_to_bitfield(bitfield_type, name, type, offset_override, size_override,
                            skip_in_decomp);
}

struct StructureDefResult {
  TypeFlags flags;
  bool generate_runtime_type = true;
  bool pack_me = false;
  bool allow_misaligned = false;
  bool final = false;
  bool always_stack_singleton = false;

  std::unordered_map<std::string, std::unordered_map<std::string, DefinitionMetadata>>
      virtual_state_definitions;
  std::unordered_map<std::string, std::unordered_map<std::string, DefinitionMetadata>>
      state_definitions;

  void append_virtual_state_def(const std::string& state_name,
                                const StateHandler handler,
                                DefinitionMetadata data) {
    if (virtual_state_definitions.count(state_name) == 0) {
      virtual_state_definitions[state_name] = std::unordered_map<std::string, DefinitionMetadata>();
    }
    virtual_state_definitions[state_name][handler_kind_to_name(handler)] = data;
  }

  void append_state_def(const std::string& state_name,
                        const StateHandler handler,
                        DefinitionMetadata data) {
    if (state_definitions.count(state_name) == 0) {
      state_definitions[state_name] = std::unordered_map<std::string, DefinitionMetadata>();
    }
    state_definitions[state_name][handler_kind_to_name(handler)] = data;
  }
};

void declare_method(Type* type,
                    TypeSystem* type_system,
                    const goos::Object& def,
                    StructureDefResult& struct_def) {
  for_each_in_list(def, [&](const goos::Object& _obj) {
    auto obj = &_obj;
    // (name args return-type [:no-virtual] [:replace] [:state] [:behavior] [id])
    // or alternatively
    // (:override-doc "new-docstring" [id])
    // - this effectively does a :replace without having to re-define the name and signature and
    // keep that in-sync
    std::string method_name;
    TypeSpec function_typespec("function");
    std::optional<std::string> docstring;
    goos::Object args;
    goos::Object return_type;
    bool no_virtual = false;
    bool replace_method = false;
    bool overriding_doc = false;

    if (!obj->is_empty_list() && car(obj).is_symbol(":override-doc")) {
      obj = cdr(obj);
      if (car(obj).is_string()) {
        docstring = str_util::trim_newline_indents(car(obj).as_string()->data);
        overriding_doc = true;
        obj = cdr(obj);
      } else {
        throw std::runtime_error("Specified :override-doc with no docstring!");
      }
    }

    if (!overriding_doc) {
      // name
      method_name = symbol_string(car(obj));
      obj = cdr(obj);

      // docstring
      if (obj->is_pair() && car(obj).is_string()) {
        docstring = str_util::trim_newline_indents(car(obj).as_string()->data);
        obj = cdr(obj);
      }

      // args
      args = car(obj);
      obj = cdr(obj);

      // return type
      return_type = car(obj);
      obj = cdr(obj);

      // Iterate through the remainder of the form's supported keywords
      // this int is assumed to be the id, and always at the end!
      //
      // Doing it like this makes the ordering not critical
      while (!obj->is_empty_list() && car(obj).is_symbol()) {
        const auto& keyword = car(obj).as_symbol()->name;
        if (keyword == ":no-virtual") {
          no_virtual = true;
        } else if (keyword == ":replace") {
          replace_method = true;
        } else if (keyword == ":state") {
          auto behavior_tag = function_typespec.try_get_tag("behavior");
          function_typespec = TypeSpec("state");
          if (behavior_tag) {
            function_typespec.add_new_tag("behavior", behavior_tag.value());
          }
          // parse state docstrings if available
          if (car(cdr(obj)).is_list()) {
            obj = cdr(obj);
            auto docstring_list = &car(obj);
            auto elem = docstring_list;
            while (!elem->is_empty_list() && car(elem).is_symbol()) {
              const auto& handler = car(elem).as_symbol()->name;
              const auto handler_kind = handler_keyword_to_kind(handler);

              // Get the docstring
              elem = cdr(elem);
              if (!car(elem).is_string()) {
                throw std::runtime_error("Missing a docstring for a state handler!");
              }
              DefinitionMetadata def_meta;
              // TODO - definition location info
              def_meta.docstring = car(elem).as_string()->data;
              struct_def.append_virtual_state_def(method_name, handler_kind, def_meta);

              elem = cdr(elem);
            }
          }
        } else if (keyword == ":behavior") {
          obj = cdr(obj);
          if (!car(obj).is_symbol()) {
            lg::print(
                ":behavior tag used without providing the process type name in a method "
                "declaration. {}::{}\n",
                type->get_name(), method_name.c_str());
            throw std::runtime_error("Bad usage of :behavior in a method declaration");
          }
          function_typespec.add_new_tag("behavior", symbol_string(obj->as_pair()->car));
        }
        obj = cdr(obj);
      }

      // fill in args now that we've finalized the function spec
      for_each_in_list(args, [&](const goos::Object& o) {
        function_typespec.add_arg(parse_typespec(type_system, o));
      });
      function_typespec.add_arg(parse_typespec(type_system, return_type));
    }

    // determine the method id, it should be the last in the list
    int id = -1;
    if (!obj->is_empty_list() && car(obj).is_int()) {
      auto& id_obj = car(obj);
      id = get_int(id_obj);
      obj = cdr(obj);
    }

    if (!obj->is_empty_list()) {
      throw std::runtime_error("found symbols after the `id` in a method defintion: " +
                               def.print());
    }

    MethodInfo info;
    if (overriding_doc) {
      info = type_system->override_method(type, method_name, id, docstring);
    } else {
      info = type_system->declare_method(type, method_name, docstring, no_virtual,
                                         function_typespec, replace_method, id);
    }

    // check the method assert
    if (id != -1) {
      // method id assert!
      if (id != info.id) {
        lg::print("WARNING - ID assert failed on method {} of type {} (wanted {} got {})\n",
                  method_name.c_str(), type->get_name().c_str(), id, info.id);
        throw std::runtime_error("Method ID assert failed");
      }
    }
  });
}

void declare_state(Type* type,
                   TypeSystem* type_system,
                   const goos::Object& def,
                   StructureDefResult& struct_def) {
  for_each_in_list(def, [&](const goos::Object& _obj) {
    auto obj = &_obj;
    if (obj->is_list()) {
      // (name [(:event "docstring"...)] ,@args)
      auto state_name = symbol_string(car(obj));

      if (!cdr(obj)->is_empty_list() && car(cdr(obj)).is_list()) {
        obj = cdr(obj);
        auto docstring_list = &car(obj);
        auto elem = docstring_list;
        while (!elem->is_empty_list() && car(elem).is_symbol()) {
          const auto& handler = car(elem).as_symbol()->name;
          const auto handler_kind = handler_keyword_to_kind(handler);

          // Get the docstring
          elem = cdr(elem);
          if (!car(elem).is_string()) {
            throw std::runtime_error("Missing a docstring for a state handler!");
          }
          DefinitionMetadata def_meta;
          // TODO - definition location info
          def_meta.docstring = car(elem).as_string()->data;
          struct_def.append_state_def(state_name, handler_kind, def_meta);

          elem = cdr(elem);
        }
      }

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

StructureDefResult parse_structure_def(
    StructureType* type,
    TypeSystem* ts,
    const goos::Object& fields,
    const goos::Object& options,
    std::unordered_map<goos::HeapObject*, goos::Object>& constants) {
  StructureDefResult result;
  for_each_in_list(fields, [&](const goos::Object& o) { add_field(type, ts, o, constants); });
  TypeFlags flags;
  flags.heap_base = 0;

  flags.size = type->get_size_in_memory();
  flags.pad = 0;

  auto* rest = &options;
  int size_assert = -1;
  int method_count_assert = -1;
  uint64_t flag_assert = 0;
  bool flag_assert_set = false;
  bool set_heapbase = false;
  while (!rest->is_empty_list()) {
    if (car(rest).is_pair()) {
      auto opt_list = &car(rest);
      auto& first = car(opt_list);
      opt_list = cdr(opt_list);

      auto list_name = symbol_string(first);
      if (list_name == ":methods") {
        declare_method(type, ts, *opt_list, result);
      } else if (list_name == ":states") {
        declare_state(type, ts, *opt_list, result);
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
          throw std::runtime_error("Cannot use -1 as method-count-assert");
        }
        rest = cdr(rest);
      } else if (opt_name == ":flag-assert") {
        flag_assert = get_int(car(rest));
        flag_assert_set = true;
        rest = cdr(rest);
      } else if (opt_name == ":no-runtime-type") {
        result.generate_runtime_type = false;
      } else if (opt_name == ":no-inspect") {
        type->set_gen_inspect(false);
      } else if (opt_name == ":pack-me") {
        result.pack_me = true;
      } else if (opt_name == ":heap-base") {
        u16 hb = get_int(car(rest));
        if ((hb % 0x10) != 0) {
          throw std::runtime_error("heap-base is not 16-byte aligned");
        }
        rest = cdr(rest);
        flags.heap_base = hb;
        set_heapbase = true;
      } else if (opt_name == ":allow-misaligned") {
        result.allow_misaligned = true;
      } else if (opt_name == ":final") {
        result.final = true;
      } else if (opt_name == ":always-stack-singleton") {
        result.always_stack_singleton = true;
      } else {
        throw std::runtime_error("Invalid option in field specification: " + opt_name);
      }
    }
  }

  if (ts->fully_defined_type_exists(TypeSpec("process")) &&
      ts->tc(TypeSpec("process"), TypeSpec(type->get_parent()))) {
    // check heap-base if this is a child of process.
    auto process_type = ts->get_type_of_type<BasicType>("process");
    auto auto_hb = (flags.size - process_type->size() + 0xf) & ~0xf;
    if (!set_heapbase) {
      // wasnt set manually so set automatically.
      flags.heap_base = auto_hb;
    } else if (flags.heap_base < auto_hb) {
      // was set manually so verify if that's correct.
      throw std::runtime_error(
          fmt::format("Process heap underflow in type {}: heap-base is {} vs. auto-detected {}",
                      type->get_name(), flags.heap_base, auto_hb));
      //} else if (flags.heap_base != auto_hb) {
      //  lg::print("Type {} has manual heap-base ({} vs {}). This is fine. \n", type->get_name(),
      //             flags.heap_base, auto_hb);
    }
  }

  if (size_assert != -1 && flags.size != u16(size_assert)) {
    throw std::runtime_error(
        fmt::format("Type {} came out to size {}[{:#x}] but size-assert was set to {}",
                    type->get_name(), int(flags.size), int(flags.size), size_assert));
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
        auto dummy = StructureDefResult();
        declare_method(type, ts, *opt_list, dummy);
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
      } else if (opt_name == ":no-inspect") {
        type->set_gen_inspect(false);
      } else {
        throw std::runtime_error("Invalid option in field specification: " + opt_name);
      }
    }
  }

  if (size_assert != -1 && flags.size != u16(size_assert)) {
    throw std::runtime_error(
        fmt::format("Type {} came out to size {}[{:#x}] but size-assert was set to {}",
                    type->get_name(), int(flags.size), int(flags.size), size_assert));
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
  ASSERT(false);
  return {};
}

DeftypeResult parse_deftype(const goos::Object& deftype,
                            TypeSystem* ts,
                            std::unordered_map<goos::HeapObject*, goos::Object>* constants) {
  DefinitionMetadata symbol_metadata;
  std::unordered_map<goos::HeapObject*, goos::Object> no_consts;
  auto& constants_to_use = no_consts;
  if (constants != nullptr) {
    constants_to_use = *constants;
  }

  auto iter = &deftype;

  auto& type_name_obj = car(iter);
  iter = cdr(iter);
  auto& parent_list_obj = car(iter);
  iter = cdr(iter);
  // check for docstring
  if (iter->is_pair() && car(iter).is_string()) {
    symbol_metadata.docstring = str_util::trim_newline_indents(car(iter).as_string()->data);
    iter = cdr(iter);
  }
  auto& field_list_obj = car(iter);
  iter = cdr(iter);
  auto& options_obj = *iter;

  if (!type_name_obj.is_symbol()) {
    throw std::runtime_error("deftype must be given a symbol as the type name");
  }

  auto& name = type_name_obj.as_symbol()->name;
  auto parent_type_name = deftype_parent_list(parent_list_obj);
  auto parent_type = ts->make_typespec(parent_type_name);
  DeftypeResult result;
  std::optional<StructureDefResult> structure_result;

  if (is_type("basic", parent_type, ts)) {
    auto new_type = std::make_unique<BasicType>(parent_type_name, name, false, 0);
    new_type->m_metadata = symbol_metadata;
    auto pto = dynamic_cast<BasicType*>(ts->lookup_type(parent_type));
    ASSERT(pto);
    if (pto->final()) {
      throw std::runtime_error(
          fmt::format("[TypeSystem] Cannot make a child type {} of final basic type {}", name,
                      parent_type_name));
    }
    new_type->inherit(pto);
    ts->forward_declare_type_as(name, pto->get_name());
    auto sr =
        parse_structure_def(new_type.get(), ts, field_list_obj, options_obj, constants_to_use);
    result.flags = sr.flags;
    result.create_runtime_type = sr.generate_runtime_type;
    structure_result = sr;
    if (sr.pack_me) {
      new_type->set_pack(true);
    }
    if (sr.allow_misaligned) {
      new_type->set_allow_misalign(true);
    }
    if (sr.always_stack_singleton) {
      lg::print(
          "[TypeSystem] :always-stack-singleton was set on {}, which is a basic and cannot "
          "be a stack singleton\n",
          name);
      throw std::runtime_error("invalid stack singleton option on basic");
    }
    new_type->set_heap_base(result.flags.heap_base);
    if (sr.final) {
      new_type->set_final();
    }
    ts->add_type(name, std::move(new_type));
  } else if (is_type("structure", parent_type, ts)) {
    auto new_type = std::make_unique<StructureType>(parent_type_name, name, false, false, false, 0);
    new_type->m_metadata = symbol_metadata;
    auto pto = dynamic_cast<StructureType*>(ts->lookup_type(parent_type));
    ASSERT(pto);
    new_type->inherit(pto);
    ts->forward_declare_type_as(name, pto->get_name());
    auto sr =
        parse_structure_def(new_type.get(), ts, field_list_obj, options_obj, constants_to_use);
    result.flags = sr.flags;
    result.create_runtime_type = sr.generate_runtime_type;
    structure_result = sr;
    if (sr.pack_me) {
      new_type->set_pack(true);
    }
    if (sr.allow_misaligned) {
      new_type->set_allow_misalign(true);
    }
    if (sr.always_stack_singleton) {
      new_type->set_always_stack_singleton();
    }
    if (sr.final) {
      throw std::runtime_error(
          fmt::format("[TypeSystem] :final option cannot be used on structure type {}", name));
    }
    new_type->set_heap_base(result.flags.heap_base);
    ts->add_type(name, std::move(new_type));
  } else if (is_type("integer", parent_type, ts)) {
    auto pto = ts->lookup_type(parent_type);
    ASSERT(pto);
    auto new_type = std::make_unique<BitFieldType>(
        parent_type_name, name, pto->get_size_in_memory(), pto->get_load_signed());
    new_type->m_metadata = symbol_metadata;
    auto parent_value = dynamic_cast<ValueType*>(pto);
    ASSERT(parent_value);
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

  if (structure_result) {
    result.type_info->m_state_definition_meta = structure_result->state_definitions;
    result.type_info->m_virtual_state_definition_meta = structure_result->virtual_state_definitions;
  }

  return result;
}
