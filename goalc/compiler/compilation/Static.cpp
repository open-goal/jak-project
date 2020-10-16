/*!
 * @file Static.cpp
 * Compiler helper functions for creating static data.
 * This is the front end for things in StaticObject.cpp
 */

#include "goalc/compiler/Compiler.h"

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
}  // namespace

Val* Compiler::compile_new_static_structure_or_basic(const goos::Object& form,
                                                     const TypeSpec& type,
                                                     const goos::Object& _field_defs,
                                                     Env* env) {
  auto fe = get_parent_env_of_type<FunctionEnv>(env);
  std::unique_ptr<StaticStructure> obj;
  if (is_basic(type)) {
    obj = std::make_unique<StaticBasic>(MAIN_SEGMENT, type.base_type());
  } else {
    // if we ever find this type of static data outside of MAIN_SEGMENT, we can create an option
    // in the new form to pick the segment.
    obj = std::make_unique<StaticStructure>(MAIN_SEGMENT);
  }

  auto type_info = dynamic_cast<StructureType*>(m_ts.lookup_type(type));
  assert(type_info);  // should always be at least a structure.
  obj->data.resize(type_info->get_size_in_memory());
  // the file env will end up owning the obj.
  auto result = fe->alloc_val<StaticVal>(obj.get(), type);

  auto* field_defs = &_field_defs;
  while (!field_defs->is_empty_list()) {
    auto field_name_def = symbol_string(pair_car(*field_defs));
    field_defs = &pair_cdr(*field_defs);

    auto field_value = pair_car(*field_defs);
    field_defs = &pair_cdr(*field_defs);

    if (field_name_def.at(0) != ':') {
      throw_compile_error(form,
                          "expected field def name to start with :, instead got " + field_name_def);
    }

    field_name_def = field_name_def.substr(1);
    auto field_info = m_ts.lookup_field_info(type_info->get_name(), field_name_def);

    if (field_info.field.is_dynamic() || field_info.field.is_inline() ||
        field_info.field.is_array()) {
      throw_compile_error(form, "Static objects not yet implemented for dynamic/inline/array");
    }

    auto field_offset = field_info.field.offset();
    assert(field_info.needs_deref);  // for now...
    auto deref_info = m_ts.get_deref_info(m_ts.make_pointer_typespec(field_info.type));
    auto field_size = deref_info.load_size;
    assert(field_offset + field_size <= int(obj->data.size()));

    if (is_integer(field_info.type)) {
      s64 value = 0;
      if (!try_getting_constant_integer(field_value, &value, env)) {
        throw_compile_error(form,
                            fmt::format("Field {} is an integer, but the value given couldn't be "
                                        "converted to an integer at compile time.",
                                        field_name_def));
      }

      if (!integer_fits(value, deref_info.load_size, deref_info.sign_extend)) {
        throw_compile_error(
            form, fmt::format("Field {} is set to a compile time integer value of {} which would "
                              "overflow (size {} signed {})",
                              field_name_def, value, deref_info.load_size, deref_info.sign_extend));
      }

      if (field_size == 1 || field_size == 2 || field_size == 4 || field_size == 8) {
        memcpy(obj->data.data() + field_offset, &value, field_size);
      } else {
        // not sure how we can create 128-bit integer constants at this point...
        assert(false);
      }
    } else if (is_basic(field_info.type)) {
      if (is_quoted_sym(field_value)) {
        obj->add_symbol_record(quoted_sym_as_string(field_value), field_offset);
        assert(deref_info.mem_deref);
        assert(deref_info.can_deref);
        assert(deref_info.load_size == 4);

        // the linker needs to see a -1 in order to know to insert a symbol pointer
        // instead of just the symbol table offset.
        u32 linker_val = 0xffffffff;
        memcpy(obj->data.data() + field_offset, &linker_val, 4);
      } else {
        throw_compile_error(
            form, "Setting a basic field to anything other than a symbol is currently unsupported");
      }
    }

    else {
      assert(false);  // for now
    }
  }

  auto fie = get_parent_env_of_type<FileEnv>(env);
  fie->add_static(std::move(obj));
  return result;
}