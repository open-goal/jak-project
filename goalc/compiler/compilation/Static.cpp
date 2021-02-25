/*!
 * @file Static.cpp
 * Compiler helper functions for creating static data.
 * This is the front end for things in StaticObject.cpp
 */

#include "goalc/compiler/Compiler.h"
#include "third-party/fmt/core.h"
#include "common/goos/ParseHelpers.h"

namespace {
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
  }
}

u32 float_as_u32(float x) {
  u32 result;
  memcpy(&result, &x, 4);
  return result;
}
}  // namespace

/*!
 * Compile the fields of a static structure into the given StaticStructure*, applying an offset.
 * This can be used to generate an entire structure (set offset to 0), or to fill out an inline
 * structure within an existing one (set the offset to the offset of the inline field)
 */
void Compiler::compile_static_structure_inline(const goos::Object& form,
                                               const TypeSpec& type,
                                               const goos::Object& _field_defs,
                                               StaticStructure* structure,
                                               int offset,
                                               Env* env) {
  auto type_info = dynamic_cast<StructureType*>(m_ts.lookup_type(type));
  assert(type_info);

  // make sure we have enough space
  if (int(structure->data.size()) < offset + type_info->get_size_in_memory()) {
    throw_compiler_error(form, "The structure does not fit in the type.");
  }

  auto* field_defs = &_field_defs;
  while (!field_defs->is_empty_list()) {
    auto field_name_def = symbol_string(pair_car(*field_defs));
    field_defs = &pair_cdr(*field_defs);

    auto field_value = pair_car(*field_defs);
    field_defs = &pair_cdr(*field_defs);

    if (field_name_def.at(0) != ':') {
      throw_compiler_error(
          form, "expected field def name to start with :, instead got " + field_name_def);
    }

    field_name_def = field_name_def.substr(1);
    auto field_info = m_ts.lookup_field_info(type_info->get_name(), field_name_def);

    if (field_info.field.is_dynamic() || field_info.field.is_array()) {
      throw_compiler_error(form, "Static objects not yet implemented for dynamic/inline/array");
    }

    auto field_offset = field_info.field.offset() + offset;

    if (is_integer(field_info.type)) {
      assert(field_info.needs_deref);  // for now...
      auto deref_info = m_ts.get_deref_info(m_ts.make_pointer_typespec(field_info.type));
      auto field_size = deref_info.load_size;
      assert(field_offset + field_size <= int(structure->data.size()));
      assert(!field_info.field.is_inline());
      s64 value = 0;
      auto sr = compile_static(field_value, env);
      if (!sr.is_constant_data()) {
        throw_compiler_error(form, "Could not use {} for an integer field", field_value.print());
      }
      // we are not strict with the type checking here, as long as you give an "integer" and it
      // ends up fitting, it's okay.
      typecheck(form, TypeSpec("integer"), sr.typespec());
      value = sr.constant_data();

      if (!integer_fits(value, deref_info.load_size, deref_info.sign_extend)) {
        throw_compiler_error(form,
                             "Field {} is set to a compile time integer value of {} which would "
                             "overflow (size {} signed {})",
                             field_name_def, value, deref_info.load_size, deref_info.sign_extend);
      }

      if (field_size == 1 || field_size == 2 || field_size == 4 || field_size == 8) {
        memcpy(structure->data.data() + field_offset, &value, field_size);
      } else {
        // not sure how we can create 128-bit integer constants at this point...
        assert(false);
      }
    } else if (is_structure(field_info.type) || is_pair(field_info.type)) {
      // todo - rewrite this to correctly handle structures within structures.
      if (is_pair(field_info.type)) {
        assert(!field_info.field.is_inline());
      }

      if (field_info.field.is_inline()) {
        // for an inline field, we only accept (new 'static '<type> ...)
        if (!field_value.is_list()) {
          throw_compiler_error(field_value, "Inline field was not properly specified");
        }

        goos::Object constructor_args;
        auto new_form = get_list_as_vector(field_value, &constructor_args, 3);
        if (new_form.size() != 3) {
          throw_compiler_error(field_value,
                               "Inline field must be defined with (new 'static 'type-name ...)");
        }

        if (!new_form.at(0).is_symbol() || new_form.at(0).as_symbol()->name != "new") {
          throw_compiler_error(field_value,
                               "Inline field must be defined with (new 'static 'type-name ...)");
        }

        if (!is_quoted_sym(new_form.at(1)) ||
            unquote(new_form.at(1)).as_symbol()->name != "static") {
          throw_compiler_error(field_value,
                               "Inline field must be defined with (new 'static 'type-name ...)");
        }

        auto inlined_type = parse_typespec(unquote(new_form.at(2)));
        if (inlined_type != field_info.type) {
          throw_compiler_error(field_value, "Cannot store a {} in an inline {}",
                               inlined_type.print(), field_info.type.print());
        }
        compile_static_structure_inline(field_value, inlined_type, constructor_args, structure,
                                        field_offset, env);

        if (is_basic(inlined_type)) {
          structure->add_type_record(inlined_type.base_type(), field_offset);
        }

      } else {
        assert(field_info.needs_deref);
        auto deref_info = m_ts.get_deref_info(m_ts.make_pointer_typespec(field_info.type));
        auto field_size = deref_info.load_size;
        assert(field_offset + field_size <= int(structure->data.size()));
        auto sr = compile_static(field_value, env);
        if (sr.is_symbol()) {
          if (sr.symbol_name() != "#f") {
            typecheck(form, field_info.type, sr.typespec());
          }
          structure->add_symbol_record(sr.symbol_name(), field_offset);
          assert(deref_info.mem_deref);
          assert(deref_info.can_deref);
          assert(deref_info.load_size == 4);
          // the linker needs to see a -1 in order to know to insert a symbol pointer
          // instead of just the symbol table offset.
          u32 linker_val = 0xffffffff;
          memcpy(structure->data.data() + field_offset, &linker_val, 4);
        } else if (sr.is_reference()) {
          typecheck(form, field_info.type, sr.typespec());
          structure->add_pointer_record(field_offset, sr.reference(),
                                        sr.reference()->get_addr_offset());
        } else {
          throw_compiler_error(form, "Unsupported field value {}.", field_value.print());
        }
      }
    } else if (is_float(field_info.type)) {
      assert(field_info.needs_deref);
      auto deref_info = m_ts.get_deref_info(m_ts.make_pointer_typespec(field_info.type));
      auto field_size = deref_info.load_size;
      assert(field_offset + field_size <= int(structure->data.size()));
      assert(!field_info.field.is_inline());
      auto sr = compile_static(field_value, env);
      if (!sr.is_constant_data()) {
        throw_compiler_error(form, "Could not use {} for a float field", field_value.print());
      }
      typecheck(form, TypeSpec("float"), sr.typespec());
      u64 value = sr.constant_data();
      memcpy(structure->data.data() + field_offset, &value, sizeof(float));
    }

    else {
      assert(false);  // for now
    }
  }
}

StaticResult Compiler::compile_new_static_structure(const goos::Object& form,
                                                    const TypeSpec& type,
                                                    const goos::Object& _field_defs,
                                                    Env* env) {
  std::unique_ptr<StaticStructure> obj;
  if (is_basic(type)) {
    obj = std::make_unique<StaticBasic>(MAIN_SEGMENT, type.base_type());
  } else {
    // if we ever find this type of static data outside of MAIN_SEGMENT, we can create an option
    // in the new form to pick the segment.
    obj = std::make_unique<StaticStructure>(MAIN_SEGMENT);
  }

  auto type_info = dynamic_cast<StructureType*>(m_ts.lookup_type(type));
  assert(type_info);

  obj->data.resize(type_info->get_size_in_memory());
  compile_static_structure_inline(form, type, _field_defs, obj.get(), 0, env);
  auto fie = get_parent_env_of_type<FileEnv>(env);
  auto result = StaticResult::make_structure_reference(obj.get(), type);
  fie->add_static(std::move(obj));
  return result;
}

StaticResult Compiler::compile_static_bitfield(const goos::Object& form,
                                               const TypeSpec& type,
                                               const goos::Object& _field_defs,
                                               Env* env) {
  u64 as_int = 0;

  auto type_info = dynamic_cast<BitFieldType*>(m_ts.lookup_type(type));
  assert(type_info);
  assert(type_info->get_load_size() <= 8);

  auto* field_defs = &_field_defs;
  while (!field_defs->is_empty_list()) {
    auto field_name_def = symbol_string(pair_car(*field_defs));
    field_defs = &pair_cdr(*field_defs);

    auto field_value = pair_car(*field_defs);
    field_defs = &pair_cdr(*field_defs);

    if (field_name_def.at(0) != ':') {
      throw_compiler_error(
          form, "expected field def name to start with :, instead got " + field_name_def);
    }

    field_name_def = field_name_def.substr(1);
    auto field_info = m_ts.lookup_bitfield_info(type_info->get_name(), field_name_def);

    auto field_offset = field_info.offset;
    auto field_size = field_info.size;
    assert(field_offset + field_size <= type_info->get_load_size() * 8);

    if (is_integer(field_info.result_type)) {
      s64 value = 0;
      if (!try_getting_constant_integer(field_value, &value, env)) {
        throw_compiler_error(form,
                             "Field {} is an integer, but the value given couldn't be "
                             "converted to an integer at compile time.",
                             field_name_def);
      }

      // todo, check the integer fits!
      u64 unsigned_value = value;
      u64 or_value = unsigned_value;
      // shift us all the way left to clear upper bits.
      or_value <<= (64 - field_size);
      // and back right.
      or_value >>= (64 - field_size);
      if (or_value != unsigned_value) {
        throw_compiler_error(form, "Field {}'s value doesn't fit.", field_name_def);
      }

      as_int |= (or_value << field_offset);
    } else if (is_float(field_info.result_type)) {
      if (field_size != 32) {
        throw_compiler_error(form,
                             "Tried to put a float into a float bitfield that's not 4 "
                             "bytes. This is probably not what you wanted to do.");
      }

      float value = 0.f;
      if (!try_getting_constant_float(field_value, &value, env)) {
        throw_compiler_error(form,
                             "Field {} is a float, but the value given couldn't "
                             "be converted to a float at compile time.",
                             field_name_def);
      }
      u64 float_value = float_as_u32(value);
      as_int |= (float_value << field_offset);
    }

    else {
      assert(false);  // for now
    }
  }

  return StaticResult::make_constant_data(as_int, type);
}

/*!
 * Handles stuff in static pairs. Integers must be s32's.
 * - Pairs
 * - Empty Lists
 * - Symbols
 * - Integers
 * - Strings
 */
StaticResult Compiler::compile_static_no_eval_for_pairs(const goos::Object& form, Env* env) {
  auto fie = get_parent_env_of_type<FileEnv>(env);
  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  auto segment = fe->segment;
  if (segment == TOP_LEVEL_SEGMENT) {
    segment = MAIN_SEGMENT;
  }
  if (form.is_pair()) {
    auto car = compile_static_no_eval_for_pairs(form.as_pair()->car, env);
    auto cdr = compile_static_no_eval_for_pairs(form.as_pair()->cdr, env);
    auto pair_structure = std::make_unique<StaticPair>(car, cdr, segment);
    auto result =
        StaticResult::make_structure_reference(pair_structure.get(), m_ts.make_typespec("pair"));
    fie->add_static(std::move(pair_structure));
    return result;
  } else if (form.is_int()) {
    if (!integer_fits(form.as_int(), 4, true)) {
      throw_compiler_error(
          form, "Cannot store {} (0x{:x}) in a pair because it overflows a signed 32-bit integer.",
          form.as_int(), form.as_int());
    }
    return StaticResult::make_constant_data(form.as_int(), TypeSpec("int32"));
  } else if (form.is_symbol()) {
    return StaticResult::make_symbol(form.as_symbol()->name);
  } else if (form.is_empty_list()) {
    return StaticResult::make_symbol("_empty_");
  } else if (form.is_string()) {
    // todo - this should eventually work with a string pool
    auto obj = std::make_unique<StaticString>(form.as_string()->data, segment);
    auto result = StaticResult::make_structure_reference(obj.get(), m_ts.make_typespec("string"));
    fie->add_static(std::move(obj));
    return result;
  } else {
    assert(false);  // not yet implemented
  }
}

/*!
 * Generic copmilation function to handle:
 *  - (new 'static <structure-or-basic>), a reference
 *
 *  - (new 'static '<bitfield>), an integer constant
 *  - (new 'static 'string), a string (not in the string pool, safe to modify)
 *  - '(...) a quoted pair
 *  - "a string" (goes in the string pool)
 *  - 'a-symbol
 *  - an integer
 *  - a float
 *  - a constant
 *  - #t or #f
 */
StaticResult Compiler::compile_static(const goos::Object& form, Env* env) {
  auto fie = get_parent_env_of_type<FileEnv>(env);
  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  auto segment = fe->segment;
  if (segment == TOP_LEVEL_SEGMENT) {
    segment = MAIN_SEGMENT;
  }

  if (form.is_symbol()) {
    // constant, #t, or #f
    auto& name = form.as_symbol()->name;
    if (name == "#t" || name == "#f") {
      return StaticResult::make_symbol(name);
    }

    // as a constant
    auto kv = m_global_constants.find(form.as_symbol());
    if (kv != m_global_constants.end()) {
      // expand constant and compile again.
      return compile_static(kv->second, env);
    } else {
      throw_compiler_error(form, "The symbol {} could not be evaluated at compile time",
                           form.print());
    }
  } else if (form.is_float()) {
    u64 value = float_as_u32(form.as_float());
    return StaticResult::make_constant_data(value, TypeSpec("float"));
  } else if (form.is_int()) {
    return StaticResult::make_constant_data(form.as_int(), TypeSpec("integer"));
  } else if (is_quoted_sym(form)) {
    return StaticResult::make_symbol(unquote(form).as_symbol()->name);
  } else if (form.is_string()) {
    // todo string pool
    auto obj = std::make_unique<StaticString>(form.as_string()->data, segment);
    auto result = StaticResult::make_structure_reference(obj.get(), m_ts.make_typespec("string"));
    fie->add_static(std::move(obj));
    return result;
  } else if (form.is_pair()) {
    auto first = form.as_pair()->car;
    auto rest = form.as_pair()->cdr;
    if (first.is_symbol() && first.as_symbol()->name == "quote") {
      if (rest.is_pair()) {
        auto second = rest.as_pair()->car;
        if (!rest.as_pair()->cdr.is_empty_list()) {
          throw_compiler_error(form, "The form {} is an invalid quoted form.", form.print());
        }
        if (second.is_pair() || second.is_empty_list()) {
          return compile_static_no_eval_for_pairs(second, env);
        } else {
          throw_compiler_error(form, "Could not evaluate the quoted form {} at compile time.",
                               second.print());
        }
      }
      throw_compiler_error(form, "The quoted form {} has no argument.", form.print());
    } else if (first.is_symbol() && first.as_symbol()->name == "new") {
      goos::Object constructor_args;
      auto args = get_list_as_vector(rest, &constructor_args, 2);
      if (args.size() < 2) {
        throw_compiler_error(form,
                             "New form evaluated at compile must specify (new 'static <type> ...)");
      }
      if (!is_quoted_sym(args.at(0)) || unquote(args.at(0)).as_symbol()->name != "static") {
        throw_compiler_error(form, "New form evaluated at compile time must use 'static. Got {}.",
                             args.at(0).print());
      }

      if (!is_quoted_sym(args.at(1))) {
        throw_compiler_error(form, "New form evaluated at compile got an invalid type: {}",
                             args.at(1).print());
      }

      if (unquote(args.at(1)).as_symbol()->name == "boxed-array") {
        return fill_static_array(form, rest, true, env);
      } else if (unquote(args.at(1)).as_symbol()->name == "array") {
        return fill_static_array(form, rest, false, env);
      } else {
        auto ts = parse_typespec(unquote(args.at(1)));
        if (ts == TypeSpec("string")) {
          // (new 'static 'string)
          if (rest.is_pair() && rest.as_pair()->cdr.is_empty_list() &&
              rest.as_pair()->car.is_string()) {
            auto obj =
                std::make_unique<StaticString>(rest.as_pair()->car.as_string()->data, segment);
            auto result =
                StaticResult::make_structure_reference(obj.get(), m_ts.make_typespec("string"));
            fie->add_static(std::move(obj));
            return result;
          } else {
            throw_compiler_error(form, "Invalid new static string");
          }
        } else if (is_bitfield(ts)) {
          return compile_static_bitfield(form, ts, constructor_args, env);
        } else if (is_structure(ts)) {
          return compile_new_static_structure(form, ts, constructor_args, env);
        } else {
          throw_compiler_error(form, "Cannot construct a static {}.", ts.print());
        }
      }
    } else if (first.is_symbol() && first.as_symbol()->name == "the-as") {
      auto args = get_va(form, rest);
      va_check(form, args, {{}, {}}, {});
      auto type = parse_typespec(args.unnamed.at(0));
      if (type == TypeSpec("float")) {
        s64 value;
        if (try_getting_constant_integer(args.unnamed.at(1), &value, env)) {
          if (integer_fits(value, 4, false)) {
            return StaticResult::make_constant_data(value, TypeSpec("float"));
          }
        }
      }
    } else {
      // maybe an enum
      s64 int_out;
      if (try_getting_constant_integer(form, &int_out, env)) {
        return StaticResult::make_constant_data(int_out, TypeSpec("int"));
      }
    }
  }

  throw_compiler_error(form, "Could not evaluate {} at compile time.", form.print());
  return {};
}

StaticResult Compiler::fill_static_array(const goos::Object& form,
                                         const goos::Object& rest,
                                         bool boxed,
                                         Env* env) {
  auto fie = get_parent_env_of_type<FileEnv>(env);
  // (new 'static 'boxed-array ...)
  // get all arguments now
  auto args = get_list_as_vector(rest);
  if (args.size() < 4) {
    throw_compiler_error(form, "new static boxed array must have type and min-size arguments");
  }
  auto content_type = parse_typespec(args.at(2));
  s64 min_size;
  if (!try_getting_constant_integer(args.at(3), &min_size, env)) {
    throw_compiler_error(form, "The length {} is not valid.", args.at(3).print());
  }
  s32 length = std::max(min_size, s64(args.size() - 4));
  // todo - generalize this array stuff if we ever need other types of static arrays.
  auto pointer_type = m_ts.make_pointer_typespec(content_type);
  auto deref_info = m_ts.get_deref_info(pointer_type);
  assert(deref_info.can_deref);
  assert(deref_info.mem_deref);
  auto array_data_size_bytes = length * deref_info.stride;
  // todo, segments
  std::unique_ptr<StaticStructure> obj;
  if (boxed) {
    obj = std::make_unique<StaticBasic>(MAIN_SEGMENT, "array");
  } else {
    obj = std::make_unique<StaticStructure>(MAIN_SEGMENT);
  }

  int array_header_size = boxed ? 16 : 0;
  obj->data.resize(array_header_size + array_data_size_bytes);

  if (boxed) {
    // 0 - 4 : type tag (set automatically)
    // 4 - 8 : length
    memcpy(obj->data.data() + 4, &length, 4);
    // 8 - 12 allocated length
    memcpy(obj->data.data() + 8, &length, 4);
    // 12 - 16 content type
    obj->add_type_record(content_type.base_type(), 12);
  }

  // now add arguments:
  for (size_t i = 4; i < args.size(); i++) {
    int arg_idx = i - 4;
    int elt_offset = array_header_size + arg_idx * deref_info.stride;
    auto sr = compile_static(args.at(i), env);
    if (is_integer(content_type)) {
      typecheck(form, TypeSpec("integer"), sr.typespec());
    } else {
      typecheck(form, content_type, sr.typespec());
    }
    if (sr.is_symbol()) {
      assert(deref_info.stride == 4);
      obj->add_symbol_record(sr.symbol_name(), elt_offset);
      u32 symbol_placeholder = 0xffffffff;
      memcpy(obj->data.data() + elt_offset, &symbol_placeholder, 4);
    } else if (sr.is_reference()) {
      assert(deref_info.stride == 4);
      obj->add_pointer_record(elt_offset, sr.reference(), sr.reference()->get_addr_offset());
    } else if (sr.is_constant_data()) {
      if (!integer_fits(sr.constant_data(), deref_info.load_size, deref_info.sign_extend)) {
        throw_compiler_error(form, "The integer {} doesn't fit in element {} of array of {}",
                             sr.constant_data(), arg_idx, content_type.print());
      }
      u64 data = sr.constant_data();
      memcpy(obj->data.data() + elt_offset, &data, deref_info.load_size);
    } else {
      assert(false);
    }
  }
  TypeSpec result_type;
  if (boxed) {
    result_type = m_ts.make_array_typespec(content_type);
  } else {
    result_type = m_ts.make_pointer_typespec(content_type);
  }
  auto result = StaticResult::make_structure_reference(obj.get(), result_type);
  fie->add_static(std::move(obj));
  return result;
}

Val* Compiler::compile_new_static_bitfield(const goos::Object& form,
                                           const TypeSpec& type,
                                           const goos::Object& _field_defs,
                                           Env* env) {
  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  auto sr = compile_static_bitfield(form, type, _field_defs, env);
  assert(sr.is_constant_data());
  return fe->alloc_val<IntegerConstantVal>(sr.typespec(), sr.constant_data());
}

Val* Compiler::compile_static_pair(const goos::Object& form, Env* env) {
  assert(form.is_pair());  // (quote PAIR)
  auto result = compile_static_no_eval_for_pairs(form, env);
  assert(result.is_reference());
  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  auto static_result = fe->alloc_val<StaticVal>(result.reference(), result.typespec());
  return static_result;
}

Val* Compiler::compile_new_static_structure_or_basic(const goos::Object& form,
                                                     const TypeSpec& type,
                                                     const goos::Object& field_defs,
                                                     Env* env) {
  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  auto sr = compile_new_static_structure(form, type, field_defs, env);
  auto result = fe->alloc_val<StaticVal>(sr.reference(), type);
  return result;
}