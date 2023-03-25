/*!
 * @file Static.cpp
 * Compiler helper functions for creating static data.
 * This is the front end for things in StaticObject.cpp
 */

#include "common/goos/ParseHelpers.h"
#include "common/util/math_util.h"

#include "goalc/compiler/Compiler.h"

#include "third-party/fmt/core.h"

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
  ASSERT(type_info);

  // make sure we have enough space
  if (int(structure->data.size()) < offset + type_info->get_size_in_memory()) {
    throw_compiler_error(form, "The structure does not fit in the type.");
  }

  auto* field_defs = &_field_defs;
  while (!field_defs->is_empty_list()) {
    auto field_name_def = symbol_string(pair_car(*field_defs));
    field_defs = &pair_cdr(*field_defs);

    auto field_value = expand_macro_completely(pair_car(*field_defs), env);
    field_defs = &pair_cdr(*field_defs);

    if (field_name_def.at(0) != ':') {
      throw_compiler_error(
          form, "expected field def name to start with :, instead got " + field_name_def);
    }

    field_name_def = field_name_def.substr(1);
    auto field_info = m_ts.lookup_field_info(type_info->get_name(), field_name_def);

    auto field_offset = field_info.field.offset() + offset;

    if (field_info.field.is_array()) {
      bool is_inline = field_info.field.is_inline();

      // for an array field, we only accept (new 'static 'array <type> ...)
      if (!field_value.is_list()) {
        throw_compiler_error(field_value, "Array field was not properly specified");
      }

      goos::Object constructor_args;
      auto new_form = get_list_as_vector(field_value, &constructor_args, 5);
      if (new_form.size() != 5) {
        throw_compiler_error(
            field_value,
            "Array field must be defined with (new 'static ['array, 'inline-array] type-name ...)");
      }

      if (!new_form.at(0).is_symbol() || new_form.at(0).as_symbol()->name != "new") {
        throw_compiler_error(
            field_value,
            "Array field must be defined with (new 'static ['array, 'inline-array] type-name ...)");
      }

      if (!is_quoted_sym(new_form.at(1)) || unquote(new_form.at(1)).as_symbol()->name != "static") {
        throw_compiler_error(
            field_value,
            "Array field must be defined with (new 'static ['array, 'inline-array] type-name ...)");
      }

      if (unquote(new_form.at(2)).print() != (is_inline ? "inline-array" : "array")) {
        throw_compiler_error(
            field_value,
            "Array field must be defined with (new 'static ['array, 'inline-array] type-name ...)");
      }

      auto array_content_type = parse_typespec(new_form.at(3), env);

      if (is_inline) {
        if (field_info.field.type() != array_content_type) {
          throw_compiler_error(field_value, "Inline array field must have the correct type");
        }
      } else {
        // allow more specific types.
        // TODO make this better.
        m_ts.typecheck_and_throw(field_info.field.type(), array_content_type, "Array content type");
      }

      s64 elt_array_len = get_constant_integer_or_error(new_form.at(4), env);

      if (!field_info.field.is_dynamic() && elt_array_len != field_info.field.array_size()) {
        throw_compiler_error(field_value, "Array field had an expected size of {} but got {}",
                             field_info.field.array_size(), elt_array_len);
      }

      auto arg_list = get_list_as_vector(field_value.as_pair()->cdr);
      if (((int)arg_list.size() - 5) > elt_array_len) {
        throw_compiler_error(field_value, "Array field definition has too many values in it.");
      }

      if (is_inline) {
        if (field_info.field.is_dynamic()) {
          throw_compiler_error(form, "Dynamic fields are not supported for inline");
        }
        fill_static_inline_array_inline(field_value, field_info.field.type(), arg_list, structure,
                                        field_offset, env);
      } else {
        int num_elts = arg_list.size() - 4;
        if (field_info.field.is_dynamic()) {
          // need to resize data
          // first, expected original size
          int expected_data_size = type_info->get_size_in_memory();
          ASSERT(expected_data_size == (int)structure->data.size());
          int stride = align(type_info->get_size_in_memory(),
                             type_info->get_inline_array_stride_alignment());
          int increase_by = stride * num_elts;
          structure->data.resize(expected_data_size + increase_by);
        }
        fill_static_array_inline(field_value, field_info.field.type(), arg_list.data() + 4,
                                 num_elts, structure, field_offset, env);
      }

    } else if (is_integer(field_info.type)) {
      ASSERT(field_info.needs_deref);  // for now...
      auto deref_info = m_ts.get_deref_info(m_ts.make_pointer_typespec(field_info.type));
      ASSERT(field_offset + deref_info.load_size <= int(structure->data.size()));
      ASSERT(!field_info.field.is_inline());
      auto sr = compile_static(field_value, env);
      if (!sr.is_constant_data()) {
        throw_compiler_error(form, "Could not use {} for an integer field", field_value.print());
      }
      // we are not strict with the type checking here, as long as you give an "integer" and it
      // ends up fitting, it's okay.
      typecheck(form, TypeSpec("integer"), sr.typespec());

      if (!sr.constant().copy_to(structure->data.data() + field_offset, deref_info.load_size,
                                 deref_info.sign_extend)) {
        throw_compiler_error(form,
                             "Field {} is set to a compile time integer value of {} which would "
                             "overflow (size {} signed {})",
                             field_name_def, sr.constant().print(), deref_info.load_size,
                             deref_info.sign_extend);
      }

    } else if (is_structure(field_info.type) || is_pair(field_info.type) ||
               is_symbol(field_info.type) || field_info.type == TypeSpec("object")) {
      if (is_pair(field_info.type)) {
        ASSERT(!field_info.field.is_inline());
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

        auto inlined_type = parse_typespec(unquote(new_form.at(2)), env);
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
        ASSERT(field_info.needs_deref);
        auto deref_info = m_ts.get_deref_info(m_ts.make_pointer_typespec(field_info.type));
        auto field_size = deref_info.load_size;
        ASSERT(field_offset + field_size <= int(structure->data.size()));
        auto sr = compile_static(field_value, env);
        if (sr.is_symbol()) {
          if (sr.symbol_name() != "#f" && sr.symbol_name() != "_empty_") {
            typecheck(form, field_info.type, sr.typespec(),
                      fmt::format("Field {}, containing symbol {}", field_info.field.name(),
                                  sr.symbol_name()));
          }
          structure->add_symbol_record(sr.symbol_name(), field_offset);
          ASSERT(deref_info.mem_deref);
          ASSERT(deref_info.can_deref);
          ASSERT(deref_info.load_size == 4);
          // the linker needs to see a -1 in order to know to insert a symbol pointer
          // instead of just the symbol table offset.
          u32 linker_val = 0xffffffff;
          memcpy(structure->data.data() + field_offset, &linker_val, 4);
        } else if (sr.is_reference()) {
          typecheck(form, field_info.type, sr.typespec());
          structure->add_pointer_record(field_offset, sr.reference(),
                                        sr.reference()->get_addr_offset());
        } else if (sr.is_type()) {
          if (field_info.type != TypeSpec("type")) {
            throw_compiler_error(form, "Cannot put a type reference in a field with type {}",
                                 field_info.type.print());
          }
          structure->add_type_record(sr.symbol_name(), field_offset);
        } else if (sr.is_func()) {
          structure->add_function_record(sr.function(), field_offset);
        } else {
          throw_compiler_error(form, "Unsupported field value {}.", field_value.print());
        }
      }
    } else if (is_float(field_info.type)) {
      ASSERT(field_info.needs_deref);
      auto deref_info = m_ts.get_deref_info(m_ts.make_pointer_typespec(field_info.type));
      auto field_size = deref_info.load_size;
      ASSERT(field_offset + field_size <= int(structure->data.size()));
      ASSERT(!field_info.field.is_inline());
      auto sr = compile_static(field_value, env);
      if (!sr.is_constant_data()) {
        throw_compiler_error(form, "Could not use {} for a float field", field_value.print());
      }
      typecheck(form, TypeSpec("float"), sr.typespec());
      u64 value = sr.constant_u64();
      memcpy(structure->data.data() + field_offset, &value, sizeof(float));
    } else if (field_info.type.base_type() == "inline-array") {
      auto sr = compile_static(field_value, env);
      if (sr.is_symbol() && sr.symbol_name() == "#f") {
        // allow #f to be used for an inline-array
        structure->add_symbol_record(sr.symbol_name(), field_offset);
        auto deref_info = m_ts.get_deref_info(m_ts.make_pointer_typespec(field_info.type));
        ASSERT(deref_info.mem_deref);
        ASSERT(deref_info.can_deref);
        ASSERT(deref_info.load_size == 4);
        // the linker needs to see a -1 in order to know to insert a symbol pointer
        // instead of just the symbol table offset.
        u32 linker_val = 0xffffffff;
        memcpy(structure->data.data() + field_offset, &linker_val, 4);
      } else if (!sr.is_reference()) {
        throw_compiler_error(form, "Invalid definition of field {}", field_info.field.name());
      } else {
        typecheck(form, field_info.type, sr.typespec());
        ASSERT(sr.reference()->get_addr_offset() == 0);
        structure->add_pointer_record(field_offset, sr.reference(),
                                      sr.reference()->get_addr_offset());
      }
    } else if (field_info.type.base_type() == "pointer") {
      auto sr = compile_static(field_value, env);
      if (sr.is_symbol() && sr.symbol_name() == "#f") {
        // allow #f to be used for a pointer.
        structure->add_symbol_record(sr.symbol_name(), field_offset);
        auto deref_info = m_ts.get_deref_info(m_ts.make_pointer_typespec(field_info.type));
        ASSERT(deref_info.mem_deref);
        ASSERT(deref_info.can_deref);
        ASSERT(deref_info.load_size == 4);
        // the linker needs to see a -1 in order to know to insert a symbol pointer
        // instead of just the symbol table offset.
        u32 linker_val = 0xffffffff;
        memcpy(structure->data.data() + field_offset, &linker_val, 4);
      } else {
        if (!sr.is_reference()) {
          throw_compiler_error(form, "Invalid definition of field {}", field_info.field.name());
        }
        typecheck(form, field_info.type, sr.typespec());
        ASSERT(sr.reference()->get_addr_offset() == 0);
        structure->add_pointer_record(field_offset, sr.reference(),
                                      sr.reference()->get_addr_offset());
      }

    }

    else {
      ASSERT(false);  // for now
    }
  }
}

StaticResult Compiler::compile_new_static_structure(const goos::Object& form,
                                                    const TypeSpec& type,
                                                    const goos::Object& _field_defs,
                                                    Env* env,
                                                    int seg) {
  std::unique_ptr<StaticStructure> obj;
  if (is_basic(type)) {
    obj = std::make_unique<StaticBasic>(seg, type.base_type());
  } else {
    // if we ever find this type of static data outside of MAIN_SEGMENT, we can create an option
    // in the new form to pick the segment.
    obj = std::make_unique<StaticStructure>(seg);
  }

  auto type_info = dynamic_cast<StructureType*>(m_ts.lookup_type(type));
  ASSERT(type_info);

  obj->data.resize(type_info->get_size_in_memory());
  compile_static_structure_inline(form, type, _field_defs, obj.get(), 0, env);
  auto fie = env->file_env();
  auto result = StaticResult::make_structure_reference(obj.get(), type);
  fie->add_static(std::move(obj));
  return result;
}

/*!
 * Convert a bitfield definition to a StaticResult.
 * If allow_dynamic_construction is not set, this must happen entirely at compile time, and the
 * result will always be a constant integer.
 *
 * If allow_dynamic_construction is set, this may emit code to generate the value.
 */
Val* Compiler::compile_bitfield_definition(const goos::Object& form,
                                           const TypeSpec& type,
                                           const goos::Object& _field_defs,
                                           bool allow_dynamic_construction,
                                           Env* env) {
  // unset fields are 0, so initialize our constant value to 0 here.
  U128 constant_integer_part;

  // look up the bitfield type we're working with. For now, make sure it's under 8 bytes.
  auto type_info = dynamic_cast<BitFieldType*>(m_ts.lookup_type(type));
  ASSERT(type_info);
  bool use_128 = type_info->get_load_size() == 16;

  // We will construct this bitfield in two passes.
  // The first pass loops through definitions. If they can be evaluated at compile time, it adds
  // them to the constant. Definitions that must be evaluated at runtime are added to the
  // dynamic_defs list below. The second pass will combine the constant and dynamic defs to build
  // the final value.
  struct DynamicDef {
    // goos::Object definition;
    RegVal* value = nullptr;
    int field_offset, field_size;
    std::string field_name;  // for error message
    TypeSpec expected_type;
  };
  std::vector<DynamicDef> dynamic_defs;

  // iterate through definitions:
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
    ASSERT(field_offset + field_size <= type_info->get_load_size() * 8);

    if (is_integer(field_info.result_type) || field_info.result_type.base_type() == "pointer") {
      // first, try as a constant
      auto compiled_field_val = get_constant_integer_or_variable(field_value, env);
      bool got_constant = compiled_field_val.is_constant();
      if (!got_constant && is_bitfield(field_info.result_type) && !allow_dynamic_construction) {
        auto static_result = compile_static(field_value, env);
        if (static_result.is_constant_data()) {
          auto constant_data = static_result.constant();
          if (constant_data.size() == 8) {
            typecheck(field_value, field_info.result_type, static_result.typespec(),
                      "Type of static constant");
            got_constant = true;
            compiled_field_val.val = nullptr;
            compiled_field_val.constant = constant_data.value_64();
            // TODO: handle this in the constant propagation stuff
          }
        }
      }
      if (!got_constant) {
        // failed to get as constant, add to dynamic or error.
        if (allow_dynamic_construction) {
          DynamicDef dyn;
          dyn.value = compiled_field_val.val->to_gpr(field_value, env);
          dyn.field_offset = field_offset;
          dyn.field_size = field_size;
          dyn.field_name = field_name_def;
          dyn.expected_type = coerce_to_reg_type(field_info.result_type);
          // allow mismatched int/uint - there's too much code that gets this wrong already.
          if (m_ts.tc(TypeSpec("integer"), dyn.expected_type)) {
            dyn.expected_type = TypeSpec("integer");
          }
          dynamic_defs.push_back(dyn);
        } else {
          throw_compiler_error(form,
                               "Field {} is an integer, but the value given couldn't be "
                               "converted to an integer at compile time: {}",
                               field_name_def, field_value.print());
        }
      } else {
        u64 unsigned_value = compiled_field_val.constant;
        u64 or_value = unsigned_value;
        ASSERT(field_size <= 64);
        // shift us all the way left to clear upper bits.
        or_value <<= (64 - field_size);
        // and back right.
        or_value >>= (64 - field_size);
        if (or_value != unsigned_value) {
          throw_compiler_error(form, "Field {}'s value doesn't fit.", field_name_def);
        }

        bool start_lo = field_offset < 64;
        bool end_lo = (field_offset + field_size) <= 64;
        ASSERT(start_lo == end_lo);
        if (end_lo) {
          constant_integer_part.lo |= (or_value << field_offset);
        } else {
          constant_integer_part.hi |= (or_value << (field_offset - 64));
        }
      }

    } else if (is_float(field_info.result_type)) {
      if (field_size != 32) {
        throw_compiler_error(form,
                             "Tried to put a float into a float bitfield that's not 4 "
                             "bytes. This is probably not what you wanted to do.");
      }

      // float value = 0.f;
      auto float_value_or_const = get_constant_float_or_variable(field_value, env);
      if (float_value_or_const.is_variable()) {
        // failed to get as constant, add to dynamic or error.
        if (allow_dynamic_construction) {
          DynamicDef dyn;
          dyn.value = float_value_or_const.val->to_gpr(field_value, env);
          dyn.field_offset = field_offset;
          dyn.field_size = field_size;
          dyn.field_name = field_name_def;
          dyn.expected_type = TypeSpec("float");
          dynamic_defs.push_back(dyn);
        } else {
          throw_compiler_error(form,
                               "Field {} is a float, but the value given couldn't "
                               "be converted to a float at compile time.",
                               field_name_def);
        }
      } else {
        u64 float_value = float_as_u32(float_value_or_const.constant);
        bool start_lo = field_offset < 64;
        bool end_lo = (field_offset + field_size) <= 64;
        ASSERT(start_lo == end_lo);
        if (end_lo) {
          constant_integer_part.lo |= (float_value << field_offset);
        } else {
          constant_integer_part.hi |= (float_value << (field_offset - 64));
        }
      }
    } else if (field_info.result_type == TypeSpec("symbol")) {
      if (allow_dynamic_construction) {
        DynamicDef dyn;
        dyn.value = compile_error_guard(field_value, env)->to_gpr(field_value, env);
        dyn.field_offset = field_offset;
        dyn.field_size = field_size;
        dyn.field_name = field_name_def;
        dyn.expected_type = coerce_to_reg_type(field_info.result_type);
        dynamic_defs.push_back(dyn);
      } else {
        throw_compiler_error(
            form, "Field {} is a symbol, which cannot be constructed at compile time yet.",
            field_name_def);
      }
    } else if (m_ts.tc(TypeSpec("structure"), field_info.result_type)) {
      if (allow_dynamic_construction) {
        DynamicDef dyn;
        dyn.value = compile_error_guard(field_value, env)->to_gpr(field_value, env);
        dyn.field_offset = field_offset;
        dyn.field_size = field_size;
        dyn.field_name = field_name_def;
        dyn.expected_type = coerce_to_reg_type(field_info.result_type);
        dynamic_defs.push_back(dyn);
      } else {
        throw_compiler_error(
            form, "Field {} is a structure, which cannot be constructed at compile time yet.",
            field_name_def);
      }
    } else {
      throw_compiler_error(form, "Bitfield field {} with type {} cannot be set statically.",
                           field_name_def, field_info.result_type.print());
    }
  }

  Val* integer;
  if (use_128) {
    integer = compile_integer(constant_integer_part, env);
  } else {
    integer = compile_integer(constant_integer_part.lo, env);
    ASSERT(constant_integer_part.hi == 0);
  }

  integer->set_type(type);

  if (dynamic_defs.empty()) {
    return integer;
  } else {
    ASSERT(allow_dynamic_construction);

    if (use_128) {
      auto integer_lo = compile_integer(constant_integer_part.lo, env)->to_gpr(form, env);
      auto integer_hi = compile_integer(constant_integer_part.hi, env)->to_gpr(form, env);
      auto fe = env->function_env();
      auto rv = fe->make_ireg(type, RegClass::INT_128);
      auto xmm_temp = fe->make_ireg(TypeSpec("object"), RegClass::INT_128);

      for (auto& def : dynamic_defs) {
        auto field_val_in = def.value;
        auto field_val = env->make_gpr(field_val_in->type());
        env->emit_ir<IR_RegSet>(form, field_val, field_val_in);
        if (!m_ts.tc(def.expected_type, field_val->type())) {
          throw_compiler_error(form, "Typecheck failed for bitfield {}! Got a {} but expected a {}",
                               def.field_name, field_val->type().print(),
                               def.expected_type.print());
        }

        bool start_lo = def.field_offset < 64;
        bool end_lo = def.field_offset + def.field_size <= 64;
        ASSERT(start_lo == end_lo);
        ASSERT(def.field_size <= 64);

        int corrected_offset = def.field_offset;
        if (!start_lo) {
          corrected_offset -= 64;
        }

        int left_shift_amnt = 64 - def.field_size;
        int right_shift_amnt = (64 - def.field_size) - corrected_offset;
        ASSERT(right_shift_amnt >= 0);

        if (left_shift_amnt > 0) {
          env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::SHL_64, field_val, left_shift_amnt);
        }

        if (right_shift_amnt > 0) {
          env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::SHR_64, field_val, right_shift_amnt);
        }

        env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::OR_64,
                                     start_lo ? integer_lo : integer_hi, field_val);
      }

      fe->emit_ir<IR_RegSet>(form, xmm_temp, integer_lo);
      fe->emit_ir<IR_RegSet>(form, rv, integer_hi);
      fe->emit_ir<IR_Int128Math3Asm>(form, true, rv, rv, xmm_temp, IR_Int128Math3Asm::Kind::PCPYLD);
      return rv;
    } else {
      RegVal* integer_reg = integer->to_gpr(form, env);
      for (auto& def : dynamic_defs) {
        auto field_val_in = def.value;
        auto field_val = env->make_gpr(field_val_in->type());
        env->emit_ir<IR_RegSet>(form, field_val, field_val_in);
        if (!m_ts.tc(def.expected_type, field_val->type())) {
          throw_compiler_error(form, "Typecheck failed for bitfield {}! Got a {} but expected a {}",
                               def.field_name, field_val->type().print(),
                               def.expected_type.print());
        }
        int left_shift_amnt = 64 - def.field_size;
        int right_shift_amnt = (64 - def.field_size) - def.field_offset;
        ASSERT(right_shift_amnt >= 0);

        if (left_shift_amnt > 0) {
          env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::SHL_64, field_val, left_shift_amnt);
        }

        if (right_shift_amnt > 0) {
          env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::SHR_64, field_val, right_shift_amnt);
        }

        env->emit_ir<IR_IntegerMath>(form, IntegerMathKind::OR_64, integer_reg, field_val);
      }

      integer_reg->set_type(type);
      return integer_reg;
    }
  }
}

/*!
 * Handles stuff in static pairs. Integers must be s32's.
 * - Pairs
 * - Empty Lists
 * - Symbols
 * - Integers
 * - Strings
 */
StaticResult Compiler::compile_static_no_eval_for_pairs(const goos::Object& form,
                                                        Env* env,
                                                        int seg,
                                                        bool can_macro) {
  auto fie = env->file_env();
  if (form.is_pair()) {
    if (form.as_pair()->car.is_symbol("new") || form.as_pair()->car.is_symbol("the") ||
        (can_macro && form.as_pair()->car.is_symbol("lambda"))) {
      return compile_static(form, env);
    }
    if (form.as_pair()->car.is_symbol() && form.as_pair()->car.as_symbol()->name == "unquote") {
      // ,(macro-name args...) is actually (unquote (macro-name args...))
      // decompile the arg as macro if possible.
      // ,(lambda ...) is also a special case.
      auto& unq_arg_pair = form.as_pair()->cdr;
      if (unq_arg_pair.is_empty_list()) {
        throw_compiler_error(form, "Cannot unquote empty list");
      }
      auto& unq_arg = unq_arg_pair.as_pair()->car;
      if (!unq_arg.is_pair() || unq_arg.is_empty_list()) {
        throw_compiler_error(form, "Cannot unquote non-list");
      }
      goos::Object macro_obj;
      if (!unq_arg.as_pair()->car.is_symbol("lambda") &&
          !try_getting_macro_from_goos(unq_arg.as_pair()->car, &macro_obj)) {
        throw_compiler_error(form, "Macro {} not found", unq_arg.as_pair()->car.print());
      }
      return compile_static_no_eval_for_pairs(form.as_pair()->cdr.as_pair()->car, env, seg, true);
    }
    // try as a macro
    if (can_macro && form.as_pair()->car.is_symbol()) {
      goos::Object macro_obj;
      if (try_getting_macro_from_goos(form.as_pair()->car, &macro_obj)) {
        return compile_static_no_eval_for_pairs(expand_macro_completely(form, env), env, seg,
                                                false);
      }
    }
    auto car = compile_static_no_eval_for_pairs(form.as_pair()->car, env, seg, can_macro);
    auto cdr = compile_static_no_eval_for_pairs(form.as_pair()->cdr, env, seg, false);
    auto pair_structure = std::make_unique<StaticPair>(car, cdr, seg);
    auto result =
        StaticResult::make_structure_reference(pair_structure.get(), m_ts.make_typespec("pair"));
    fie->add_static(std::move(pair_structure));
    return result;
  } else if (form.is_int()) {
    s64 bint_val = form.as_int() << 3;
    if (!integer_fits(bint_val, 4, true)) {
      throw_compiler_error(
          form, "Cannot store {} (0x{:x}) in a pair because it overflows a signed 32-bit integer.",
          bint_val, bint_val);
    }
    return StaticResult::make_constant_data(bint_val, TypeSpec("int32"));
  } else if (form.is_symbol()) {
    return StaticResult::make_symbol(form.as_symbol()->name);
  } else if (form.is_empty_list()) {
    return StaticResult::make_symbol("_empty_");
  } else if (form.is_string()) {
    // todo - this should eventually work with a string pool
    auto obj = std::make_unique<StaticString>(form.as_string()->data, seg);
    auto result = StaticResult::make_structure_reference(obj.get(), m_ts.make_typespec("string"));
    fie->add_static(std::move(obj));
    return result;
  } else {
    throw_compiler_error(form, "Cannot put the following form in a static pair: {}", form.print());
    return {};
  }
}

/*!
 * Generic compilation function to handle:
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
StaticResult Compiler::compile_static(const goos::Object& form_before_macro, Env* env) {
  auto form = expand_macro_completely(form_before_macro, env);
  auto fie = env->file_env();
  auto fe = env->function_env();
  auto segment = fe->segment_for_static_data();

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
          return compile_static_no_eval_for_pairs(second, env, segment, false);
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

      auto unquoted_type = unquote(args.at(1));
      if (unquoted_type.as_symbol()->name == "boxed-array") {
        return fill_static_boxed_array(form, rest, env, segment, "array");
      } else if (unquoted_type.as_symbol()->name == "array") {
        return fill_static_array(form, rest, env, segment);
      } else if (unquoted_type.as_symbol()->name == "inline-array") {
        return fill_static_inline_array(form, rest, env, segment);
      } else {
        auto ts = parse_typespec(unquoted_type, env);
        bool is_array_subtype =
            m_ts.typecheck_and_throw(TypeSpec("array"), ts, "", false, false, false);
        if (is_array_subtype) {
          return fill_static_boxed_array(form, rest, env, segment, ts.base_type());
        } else if (ts == TypeSpec("string")) {
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
          auto val = dynamic_cast<const IntegerConstantVal*>(
              compile_bitfield_definition(form, ts, constructor_args, false, env));
          return StaticResult::make_constant_data(val->value(), val->type());
        } else if (is_structure(ts)) {
          return compile_new_static_structure(form, ts, constructor_args, env, segment);
        } else {
          throw_compiler_error(form, "Cannot construct a static {}.", ts.print());
        }
      }
    } else if (first.is_symbol() && first.as_symbol()->name == "the-as") {
      auto args = get_va(form, rest);
      va_check(form, args, {{}, {}}, {});
      auto type = parse_typespec(args.unnamed.at(0), env);
      if (type == TypeSpec("float")) {
        s64 value = get_constant_integer_or_error(args.unnamed.at(1), env);
        if (integer_fits(value, 4, false)) {
          return StaticResult::make_constant_data(value, TypeSpec("float"));
        }
      }
    } else if (first.is_symbol() && first.as_symbol()->name == "the") {
      auto args = get_va(form, rest);
      va_check(form, args, {{}, {}}, {});
      auto type = parse_typespec(args.unnamed.at(0), env);
      if (type == TypeSpec("binteger")) {
        s64 value = get_constant_integer_or_error(args.unnamed.at(1), env);
        if (integer_fits(value, 4, true)) {
          return StaticResult::make_constant_data(value << 3, TypeSpec("binteger"));
        }
      }
    } else if (first.is_symbol("type-ref")) {
      auto args = get_va(form, rest);
      va_check(form, args, {goos::ObjectType::SYMBOL},
               {{{"method-count", {false, goos::ObjectType::INTEGER}}}});

      auto type_name = args.unnamed.at(0).as_symbol()->name;

      std::optional<int> expected_method_count = m_ts.try_get_type_method_count(type_name);
      int method_count = -1;

      if (args.has_named("method-count")) {
        method_count = args.get_named("method-count").as_int();
        if (expected_method_count && (method_count != *expected_method_count)) {
          throw_compiler_error(
              form, "type-ref wanted {} methods for type {}, but the type system thinks it has {}",
              method_count, type_name, *expected_method_count);
        }
      } else {
        if (!expected_method_count) {
          throw_compiler_error(
              form,
              "Cannot create a static type reference for type {}. The type-ref form did not have a "
              ":method-count argument and the type system does not know how many methods it has.",
              type_name);
        }
        method_count = *expected_method_count;
      }

      m_ts.forward_declare_type_method_count(type_name, method_count);

      return StaticResult::make_type_ref(type_name, method_count);
    } else if (first.is_symbol("lambda")) {
      auto lambda = dynamic_cast<LambdaVal*>(compile_lambda(form, rest, env));
      ASSERT(lambda);
      if (!lambda->func) {
        throw_compiler_error(form, "Static lambda cannot be inline.");
      }
      return StaticResult::make_func_ref(lambda->func, lambda->type());
    } else {
      // maybe an enum
      s64 int_out = get_constant_integer_or_error(form, env);
      return StaticResult::make_constant_data(int_out, TypeSpec("int"));
    }
  }

  throw_compiler_error(form, "Could not evaluate {} at compile time.", form.print());
  return {};
}

void Compiler::fill_static_array_inline(const goos::Object& form,
                                        const TypeSpec& content_type,
                                        goos::Object* args_array,
                                        int args_array_length,
                                        StaticStructure* structure,
                                        int offset,
                                        Env* env) {
  auto pointer_type = m_ts.make_pointer_typespec(content_type);
  auto deref_info = m_ts.get_deref_info(pointer_type);
  ASSERT(deref_info.can_deref);
  ASSERT(deref_info.mem_deref);
  for (int arg_idx = 0; arg_idx < args_array_length; arg_idx++) {
    int elt_offset = offset + arg_idx * deref_info.stride;
    const auto& arg = args_array[arg_idx];
    // Special case for symbols that refer to types
    StaticResult sr;
    if (content_type == TypeSpec("type") && arg.is_symbol()) {
      const auto& type_name = arg.as_symbol()->name;
      std::optional<int> expected_method_count = m_ts.try_get_type_method_count(type_name);
      if (!expected_method_count) {
        throw_compiler_error(form, "Undeclared type used in inline-array - {}", type_name);
      }
      sr = StaticResult::make_type_ref(type_name, expected_method_count.value());
    } else {
      sr = compile_static(arg, env);
    }

    if (is_integer(content_type)) {
      typecheck(form, TypeSpec("integer"), sr.typespec());
    } else {
      if (sr.is_symbol() && sr.symbol_name() == "#f") {
        // allow #f for any structure, or symbol (no longer a structure in jak 2)
        if (content_type.base_type() != "symbol") {
          typecheck(form, TypeSpec("structure"), content_type);
        }
      } else {
        typecheck(form, content_type, sr.typespec());
      }
    }
    if (sr.is_symbol()) {
      ASSERT(deref_info.stride == 4);
      structure->add_symbol_record(sr.symbol_name(), elt_offset);
      u32 symbol_placeholder = 0xffffffff;
      memcpy(structure->data.data() + elt_offset, &symbol_placeholder, 4);
    } else if (sr.is_reference()) {
      ASSERT(deref_info.stride == 4);
      structure->add_pointer_record(elt_offset, sr.reference(), sr.reference()->get_addr_offset());
    } else if (sr.is_constant_data()) {
      if (!sr.constant().copy_to(structure->data.data() + elt_offset, deref_info.load_size,
                                 deref_info.sign_extend)) {
        throw_compiler_error(form, "The integer {} doesn't fit in element {} of array of {}",
                             sr.constant().print(), arg_idx, content_type.print());
      }
    } else if (sr.is_type()) {
      ASSERT(deref_info.stride == 4);
      structure->add_type_record(sr.symbol_name(), elt_offset);
    } else if (sr.is_func()) {
      ASSERT(deref_info.stride == 4);
      structure->add_function_record(sr.function(), elt_offset);
    } else {
      ASSERT(false);
    }
  }
}

StaticResult Compiler::fill_static_array(const goos::Object& form,
                                         const goos::Object& rest,
                                         Env* env,
                                         int seg) {
  auto fie = env->file_env();
  // (new 'static '[boxed-array|array-subtype] ...)
  // get all arguments now
  auto args = get_list_as_vector(rest);
  if (args.size() < 4) {
    throw_compiler_error(form, "new static array must have type and min-size arguments");
  }
  auto content_type = parse_typespec(args.at(2), env);
  s64 min_size = get_constant_integer_or_error(args.at(3), env);
  s32 length = std::max(min_size, s64(args.size() - 4));
  // todo - generalize this array stuff if we ever need other types of static arrays.
  auto pointer_type = m_ts.make_pointer_typespec(content_type);
  auto deref_info = m_ts.get_deref_info(pointer_type);
  ASSERT(deref_info.can_deref);
  ASSERT(deref_info.mem_deref);
  auto array_data_size_bytes = length * deref_info.stride;
  std::unique_ptr<StaticStructure> obj;

  obj = std::make_unique<StaticStructure>(seg);

  obj->data.resize(array_data_size_bytes);

  // now add arguments:
  fill_static_array_inline(form, content_type, args.data() + 4, args.size() - 4, obj.get(), 0, env);

  TypeSpec result_type;
  result_type = m_ts.make_pointer_typespec(content_type);
  auto result = StaticResult::make_structure_reference(obj.get(), result_type);
  fie->add_static(std::move(obj));
  return result;
}

StaticResult Compiler::fill_static_boxed_array(const goos::Object& form,
                                               const goos::Object& rest,
                                               Env* env,
                                               int seg,
                                               const std::string& array_type) {
  auto fie = env->file_env();
  // (new 'static 'boxed-array ...)
  // get all arguments now
  // auto args = get_list_as_vector(rest);
  auto args = get_va(form, rest);

  if (args.unnamed.size() < 2) {
    throw_compiler_error(form, "new static boxed array must have heap and type arguments");
  }

  if (!args.has_named("type")) {
    throw_compiler_error(form, "boxed array must have type");
  }
  auto content_type = parse_typespec(args.get_named("type"), env);

  s64 initialized_count = args.unnamed.size() - 2;

  s64 length;
  if (!args.has_named("length")) {
    length = initialized_count;
  } else {
    length = get_constant_integer_or_error(args.get_named("length"), env);
  }

  s64 allocated_length;
  if (args.has_named("allocated-length")) {
    if (!args.has_named("length")) {
      throw_compiler_error(form, "boxed array must length if it also has allocated-length");
    }
    allocated_length = get_constant_integer_or_error(args.get_named("allocated-length"), env);
  } else {
    allocated_length = length;
  }

  if (initialized_count > length) {
    throw_compiler_error(form, "Initialized {} elements, but length was {}", initialized_count,
                         length);
  }

  if (length > allocated_length) {
    throw_compiler_error(form, "Length {} is longer than the allocated-length {}", length,
                         allocated_length);
  }

  // todo - generalize this array stuff if we ever need other types of static arrays.
  auto pointer_type = m_ts.make_pointer_typespec(content_type);
  auto deref_info = m_ts.get_deref_info(pointer_type);
  ASSERT(deref_info.can_deref);
  ASSERT(deref_info.mem_deref);
  auto array_data_size_bytes = allocated_length * deref_info.stride;
  // todo, segments
  std::unique_ptr<StaticStructure> obj;

  // Determine if we are dealing with a subtype of an 'array' if so, use that type instead!
  const std::string type = array_type == "boxed-array" ? "array" : array_type;
  obj = std::make_unique<StaticBasic>(seg, type);

  int array_header_size = 16;
  obj->data.resize(array_header_size + array_data_size_bytes);

  // 0 - 4 : type tag (set automatically)
  // 4 - 8 : length
  memcpy(obj->data.data() + 4, &length, 4);
  // 8 - 12 allocated length
  memcpy(obj->data.data() + 8, &allocated_length, 4);
  // 12 - 16 content type
  auto runtime_type = m_ts.lookup_type(content_type.base_type())->get_runtime_name();
  obj->add_type_record(runtime_type, 12);

  // now add arguments:
  fill_static_array_inline(form, content_type, args.unnamed.data() + 2, args.unnamed.size() - 2,
                           obj.get(), array_header_size, env);

  TypeSpec result_type;

  result_type = m_ts.make_array_typespec(type, content_type);

  auto result = StaticResult::make_structure_reference(obj.get(), result_type);
  fie->add_static(std::move(obj));
  return result;
}

void Compiler::fill_static_inline_array_inline(const goos::Object& form,
                                               const TypeSpec& content_type,
                                               const std::vector<goos::Object>& args,
                                               StaticStructure* structure,
                                               int offset,
                                               Env* env) {
  auto inline_array_type = m_ts.make_inline_array_typespec(content_type);
  auto deref_info = m_ts.get_deref_info(inline_array_type);
  ASSERT(deref_info.can_deref);
  ASSERT(!deref_info.mem_deref);

  for (size_t i = 4; i < args.size(); i++) {
    auto arg_idx = i - 4;
    int elt_offset = arg_idx * deref_info.stride;
    auto elt_def = expand_macro_completely(args.at(i), env);
    if (!elt_def.is_list()) {
      throw_compiler_error(form, "Element in static inline-array must be a {}. Got {}",
                           content_type.print(), elt_def.print());
    }

    goos::Object ctor_args;
    auto new_form = get_list_as_vector(elt_def, &ctor_args, 3);
    if (new_form.size() != 3) {
      throw_compiler_error(
          elt_def, "Inline array element must be defined with (new 'static 'type-name ...)");
    }

    if (!new_form.at(0).is_symbol() || new_form.at(0).as_symbol()->name != "new") {
      throw_compiler_error(
          elt_def, "Inline array element must be defined with (new 'static 'type-name ...)");
    }

    if (!is_quoted_sym(new_form.at(1)) || unquote(new_form.at(1)).as_symbol()->name != "static") {
      throw_compiler_error(
          elt_def, "Inline array element must be defined with (new 'static 'type-name ...)");
    }

    auto inlined_type = parse_typespec(unquote(new_form.at(2)), env);
    if (inlined_type != content_type) {
      throw_compiler_error(elt_def, "Cannot store a {} in an inline array of {}",
                           inlined_type.print(), content_type.print());
    }
    compile_static_structure_inline(elt_def, content_type, ctor_args, structure,
                                    elt_offset + offset, env);
    if (is_basic(content_type)) {
      structure->add_type_record(content_type.base_type(), elt_offset + offset);
    }
  }
}

StaticResult Compiler::fill_static_inline_array(const goos::Object& form,
                                                const goos::Object& rest,
                                                Env* env,
                                                int seg) {
  auto fie = env->file_env();
  // (new 'static 'inline-array ...)
  // get all arguments now
  auto args = get_list_as_vector(rest);
  if (args.size() < 4) {
    throw_compiler_error(form, "new static inline array must have type and min-size arguments");
  }
  auto content_type = parse_typespec(args.at(2), env);
  s64 min_size = get_constant_integer_or_error(args.at(3), env);
  s32 length = std::max(min_size, s64(args.size() - 4));

  auto inline_array_type = m_ts.make_inline_array_typespec(content_type);
  auto deref_info = m_ts.get_deref_info(inline_array_type);
  if (!deref_info.can_deref || deref_info.mem_deref) {
    throw_compiler_error(form, "new static inline array of type {} is currently not supported",
                         content_type.print());
  }
  auto obj = std::make_unique<StaticStructure>(seg);
  obj->set_offset(is_basic(content_type) ? 4 : 0);
  obj->data.resize(length * deref_info.stride);

  // now add elements:
  fill_static_inline_array_inline(form, content_type, args, obj.get(), 0, env);

  TypeSpec result_type = m_ts.make_inline_array_typespec(content_type);
  auto result = StaticResult::make_structure_reference(obj.get(), result_type);
  fie->add_static(std::move(obj));
  return result;
}

Val* Compiler::compile_static_pair(const goos::Object& form, Env* env, int seg) {
  ASSERT(form.is_pair());  // (quote PAIR)
  auto result = compile_static_no_eval_for_pairs(form, env, seg, false);
  ASSERT(result.is_reference());
  auto fe = env->function_env();
  auto static_result = fe->alloc_val<StaticVal>(result.reference(), result.typespec());
  return static_result;
}

Val* Compiler::compile_new_static_structure_or_basic(const goos::Object& form,
                                                     const TypeSpec& type,
                                                     const goos::Object& field_defs,
                                                     Env* env,
                                                     int seg) {
  auto fe = env->function_env();
  auto sr = compile_new_static_structure(form, type, field_defs, env, seg);
  auto result = fe->alloc_val<StaticVal>(sr.reference(), type);
  return result;
}
