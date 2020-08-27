/*!
 * @file GoalFieldAccess.cpp
 * Access field of types implementation.
 */

#include "Goal.h"
#include "util.h"

/*!
 * Compile the -> form
 */
std::shared_ptr<Place> Goal::compile_deref(const Object& form,
                                           Object rest,
                                           std::shared_ptr<GoalEnv> env) {
  if (rest.type == EMPTY_LIST) {
    throw_compile_error(form, "-> must get at least one argument!");
  }

  auto first_arg = rest.as_pair()->car;
  rest = rest.as_pair()->cdr;

  // start by evaluating the first thing.
  auto result = compile_error_guard(first_arg, env);

  if (rest.type == EMPTY_LIST) {
    // if there is only one argument, try to dereference it as a pointer.
    auto ptr_ts = get_base_typespec("pointer");
    typecheck_base_only(form, ptr_ts, result->type, "-> with one argument was not given a pointer");
    result = std::make_shared<MemoryDerefPlace>(get_base_of_pointer(result->type),
                                                result->type.type->load_size,
                                                result->type.type->load_signed, result);
    return result;
  }

  // loop through field names...
  while (rest.type != EMPTY_LIST) {
    // the current field to deref
    auto field_obj = rest.as_pair()->car;
    rest = rest.as_pair()->cdr;

    // if its a symbol, lets try it as a field name...
    // this may fail and fall through below to the integer array stuff.
    if (field_obj.type == SYMBOL) {
      auto field_name = field_obj.as_symbol()->name;
      auto struct_type = std::dynamic_pointer_cast<StructureType>(result->type.type);

      if (struct_type) {
        bool is_basic = (std::dynamic_pointer_cast<BasicType>(struct_type) != nullptr);

        bool found_field = false;
        int offset = is_basic ? -4 : 0;
        TypeSpec field_ts;
        GoalField t_field;
        // lookup!
        for (auto& field : struct_type->fields) {
          if (field.name == field_name) {
            // got it!
            found_field = true;
            offset += field.offset;
            field_ts = field.type;
            t_field = field;
            break;
          }
        }

        // note - so that we can later take the address of this field, we must express this field in
        // a way where we can "go one dereference back" to get where it's located.
        if (found_field) {
          auto result_type = field_ts;

          if (t_field.is_array || t_field.is_dynamic) {
            // array-like
            auto array_type = t_field.is_inline ? "inline-array" : "pointer";
            result_type = TypeSpec(get_base_typespec(array_type).type, {result_type});
            result = std::make_shared<MemoryOffsetConstPlace>(result_type, offset, result);
          } else {
            if (t_field.is_inline) {
              // inline field, so it's just an offset in memory from the base.
              result = std::make_shared<MemoryOffsetConstPlace>(result_type, offset, result);
            } else {
              // field is a reference.
              auto field_loc_type = TypeSpec(get_base_typespec("pointer").type, {result_type});
              auto field_loc =
                  std::make_shared<MemoryOffsetConstPlace>(field_loc_type, offset, result);
              result = std::make_shared<MemoryDerefPlace>(result_type, field_ts.type->load_size,
                                                          field_ts.type->load_signed, field_loc);
            }
          }

          // success, go on to the next field.
          continue;
        } else {
          // couldn't find the field.
          throw_compile_error(form, "invalid -> form - couldn't find the field named " +
                                        field_name + " in type " + struct_type->name);
        }
      }

      auto bitfield_type = std::dynamic_pointer_cast<BitfieldType>(result->type.type);
      if (bitfield_type) {
        GoalBitField t_field;
        if (bitfield_type->find_field(field_name, &t_field)) {
          result = std::make_shared<BitfieldPlace>(t_field, result);
          // success, go on to the next thing
          continue;
        }
      }
    }

    // try to get an integer:
    auto index_value = resolve_to_gpr(compile_error_guard(field_obj, env), env);
    if (!is_integer(index_value->type)) {
      throw_compile_error(form, "couldn't figure out how to -> with " + field_obj.print());
    }

    // array access!
    // todo - can remove the multiplication for constant integers!
    if (result->type.type->get_name() == "inline-array") {
      auto base_type = get_base_of_inline_array(result->type);

      // offset uses size_in_array because we are inline
      auto scaled_offset =
          compile_integer_to_gpr((base_type.type->get_size_in_inline_array()), env);
      env->emit(make_unique<IR_IntegerMath>(IMUL_32, scaled_offset, index_value));
      // because we are inline, we don't need to dereference
      auto loc = std::make_shared<MemoryOffsetVarPlace>(base_type, scaled_offset, result);
      result = loc;
    } else if (result->type.type->get_name() == "pointer") {
      auto base_type = get_base_of_pointer(result->type);
      auto scaled_offset =
          compile_integer_to_gpr(base_type.type->get_size_in_non_inline_array(), env);
      env->emit(make_unique<IR_IntegerMath>(IMUL_32, scaled_offset, index_value));
      auto loc = std::make_shared<MemoryOffsetVarPlace>(result->type, scaled_offset, result);
      result = std::make_shared<MemoryDerefPlace>(base_type, base_type.type->load_size,
                                                  base_type.type->load_signed, loc);
    } else {
      throw_compile_error(form, "can't access array of type " + result->type.type->get_name());
    }
  }

  return result;
}

/*!
 * Compile the address of form.
 */
std::shared_ptr<Place> Goal::compile_addr_of(const Object& form,
                                             Object rest,
                                             std::shared_ptr<GoalEnv> env) {
  auto arg = compile_error_guard(pair_car(rest), env);
  expect_empty_list(pair_cdr(rest));

  auto arg_as_mem_deref = std::dynamic_pointer_cast<MemoryDerefPlace>(arg);
  if (!arg_as_mem_deref) {
    throw_compile_error(form, "could not take the address of " + arg->print());
  }

  // todo - static and stack variables

  return arg_as_mem_deref->base;
}