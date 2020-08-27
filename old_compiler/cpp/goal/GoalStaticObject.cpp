#include <cstring>
#include "Goal.h"

std::shared_ptr<Place> Goal::compile_make_static_object_of_type(const Object& form,
                                                                TypeSpec& type,
                                                                Object field_defs,
                                                                std::shared_ptr<GoalEnv> env) {
  std::shared_ptr<StaticStructure> obj;
  if (is_basic(type)) {
    auto basic_obj = std::make_shared<StaticBasic>();
    basic_obj->type_name = type.type->get_name();
    obj = std::move(basic_obj);
  } else {
    obj = std::make_shared<StaticStructure>();
  }

  obj->segment = MAIN_SEGMENT;
  obj->data.resize(type.type->size);

  auto result = std::make_shared<StaticPlace>(type, obj);
  env->get_statics().push_back(result);

  auto struct_type = std::dynamic_pointer_cast<StructureType>(type.type);
  if (!struct_type) {
    throw_compile_error(form, "cannot static new a non-structure type!");  // ... for now
  }

  while (field_defs.type != EMPTY_LIST) {
    auto field_name_def = symbol_string(pair_car(field_defs));
    field_defs = pair_cdr(field_defs);

    auto field_value = pair_car(field_defs);
    field_defs = pair_cdr(field_defs);

    if (field_name_def.at(0) != ':') {
      throw_compile_error(form,
                          "expected field def name to start with :, instead got " + field_name_def);
    }

    field_name_def = field_name_def.substr(1);

    GoalField* d_field = nullptr;
    for (auto& field : struct_type->fields) {
      if (field.name == field_name_def) {
        d_field = &field;
        break;
      }
    }

    if (!d_field) {
      throw_compile_error(
          form, "type " + struct_type->name + " does not have a field named " + field_name_def);
    }

    if (d_field->is_dynamic || d_field->is_inline || d_field->is_array) {
      throw_compile_error(form, "in new, dynamic/inline/array fields are not yet supported");
    }

    // for now, no type-checking...
    auto field_offset = d_field->offset;
    auto field_size = d_field->type.type->load_size;

    assert(field_offset + field_size <= type.type->size);

    // TODO - warn on overflow?
    if (is_integer(d_field->type)) {
      auto value = compile_to_integer_constant(field_value, env);
      switch (field_size) {
        case 1:
        case 2:
        case 4:
        case 8:
          memcpy(obj->data.data() + field_offset, &value, field_size);
          break;
        default:
          throw_compile_error(form, "can't store an integer in this field: " + d_field->print());
      }

      //    if(field_value.type == INTEGER) {
      //      switch(field_size) {
      //        case 1:
      //        case 2:
      //        case 4:
      //        case 8:
      //          memcpy(obj->data.data() + field_offset, &field_value.integer_obj.value,
      //          field_size); break;
      //        default:
      //          throw_compile_error(form, "can't store an integer in this field: " +
      //          d_field->print());
      //      }
    } else if (field_value.type == SYMBOL &&
               (symbol_string(field_value) == "#t" || symbol_string(field_value) == "#f")) {
      if (field_size != 4) {
        throw_compile_error(form, "invalid set symbol on field " + d_field->print());
      }
      obj->symbol_ptr_recs[symbol_string(field_value)].push_back(field_offset);
      uint32_t value = 0xffffffff;
      memcpy(obj->data.data() + field_offset, &value, 4);
    } else {
      throw_compile_error(
          form, "can't use this object as a static field definition: " + field_value.print());
    }
  }

  return result;
}