#include "logger/Logger.h"
#include "Goal.h"
#include "util.h"

/*!
 * Helper to get parent type.
 */
static std::string deftype_parent_list(Object& list) {
  if (list.type != PAIR) {
    throw std::runtime_error("invalid parent list in deftype");
  }

  auto parent = list.as_pair()->car;
  auto rest = list.as_pair()->cdr;
  if (rest.type != EMPTY_LIST) {
    throw std::runtime_error("invalid parent list in deftype - can only have one parent");
  }

  if (parent.type != SYMBOL) {
    throw std::runtime_error("invalid parent in deftype parent list");
  }

  return parent.as_symbol()->name;
}

/*!
 * Compile type definition.
 */
std::shared_ptr<Place> Goal::compile_deftype(const Object& form,
                                             Object rest,
                                             std::shared_ptr<GoalEnv> env) {
  auto args = goos.get_uneval_args(form, rest, 3);

  if (args.unnamed_args.size() != 3 || !args.named_args.empty()) {
    throw_compile_error(form, "invalid deftype");
  }

  auto type_name_obj = args.unnamed_args.at(0);
  auto parent_list_obj = args.unnamed_args.at(1);
  auto field_list_obj = args.unnamed_args.at(2);
  auto options_obj = args.rest;

  if (type_name_obj.type != SYMBOL) {
    throw_compile_error(form, "invalid deftype type name");
  }

  auto name = type_name_obj.as_symbol()->name;
  auto parent_type = get_base_typespec(deftype_parent_list(parent_list_obj));

  // first - determine if we are a structure, basic, or bitfield
  if (get_base_typespec("basic").typecheck_base_only(parent_type, types)) {
    ///////////////////////
    ///// BASIC
    //////////////////////
    std::shared_ptr<BasicType> new_type;

    // if we already have this type defined, get the old one and warn.
    auto existing_type_kv = types.types.find(name);
    if (existing_type_kv != types.types.end()) {
      if (name != "thread") {  ////////////////// HACK!
        // to silence the warning on the redefinition of the "thread" type, which is a "default"
        // type.
        gLogger.log(MSG_WARN,
                    "[Warning] type %s has been redefined and may have changed data layout\n",
                    name.c_str());
        // todo - check if a change actually occurs and silence the warning if no change.
      }

      new_type = std::dynamic_pointer_cast<BasicType>(existing_type_kv->second);
      assert(new_type);
    } else {
      new_type =
          std::make_shared<BasicType>(-1, parent_type.type, name, get_base_typespec("type").type);
    }

    auto parent_as_structure = std::dynamic_pointer_cast<StructureType>(parent_type.type);
    assert(parent_as_structure);
    new_type->inherit_fields(parent_as_structure);
    types.inherit_methods(parent_type.type->get_name(), name);
    return deftype_structure(new_type, field_list_obj, options_obj, env);

  } else if (get_base_typespec("structure").typecheck_base_only(parent_type, types)) {
    ///////////////////////
    ///// STRUCTURE
    //////////////////////
    std::shared_ptr<StructureType> new_type;

    auto existing_type_kv = types.types.find(name);
    if (existing_type_kv != types.types.end()) {
      new_type = std::dynamic_pointer_cast<StructureType>(existing_type_kv->second);
      assert(new_type);
      if (name != "connectable") {  //////// HACK!
        gLogger.log(MSG_WARN,
                    "[Warning] type %s has been redefined and may have changed data layout\n",
                    name.c_str());
        // todo - check if a change actually occurs and silence the warning if no change.
      }
    } else {
      new_type = std::make_shared<StructureType>(-1, parent_type.type, name);
    }

    auto parent_as_structure = std::dynamic_pointer_cast<StructureType>(parent_type.type);
    assert(parent_as_structure);
    new_type->inherit_fields(parent_as_structure);
    types.inherit_methods(parent_type.type->get_name(), name);
    return deftype_structure(new_type, field_list_obj, options_obj, env);

  } else if (get_base_typespec("integer").typecheck_base_only(parent_type, types)) {
    ///////////////////////
    ///// BITFIELD
    //////////////////////
    assert(parent_type.type->is_value_type);
    assert(parent_type.type->load_size == parent_type.type->size);
    if (parent_type.type->size <= 8) {
      std::shared_ptr<BitfieldType> new_type;
      auto existing_type_kv = types.types.find(name);
      if (existing_type_kv != types.types.end()) {
        printf("existing bitfield type\n");
        new_type = std::dynamic_pointer_cast<BitfieldType>(existing_type_kv->second);
        if (!new_type) {
          throw_compile_error(form,
                              "invalid attempt to change type " + name + " to a bitfield type!");
        }
      } else {
        new_type = std::make_shared<BitfieldType>(parent_type.type, name);
      }
      types.inherit_methods(parent_type.type->get_name(), name);
      return deftype_bitfield(new_type, field_list_obj, options_obj, env);
    } else {
      ice("large bitfields not supported yet");
    }

  } else {
    throw_compile_error(form, "unknown parent type kind " + parent_type.print());
  }

  return get_none();
}

/*!
 * Parse the definition of a field for a bit field.
 * NOTE: this needs the "env" in case the user sets the size of a bit field to a _CONSTANT_
 * (name type [:size sz] [:offset off] [:offset-assert assert])
 * @param def
 * @return
 */
BitFieldDefinition Goal::parse_bit_field_def(const Object& def, std::shared_ptr<GoalEnv> env) {
  BitFieldDefinition bfd;

  Object rest = def;

  // name
  bfd.name = symbol_string(pair_car(rest));
  rest = pair_cdr(rest);

  // type
  bfd.type = compile_typespec(pair_car(rest));
  rest = pair_cdr(rest);

  // options
  while (rest.type != EMPTY_LIST) {
    auto option_name = symbol_string(pair_car(rest));
    rest = pair_cdr(rest);

    if (option_name == ":offset") {
      bfd.offset_override = compile_to_integer_constant(pair_car(rest), env);
      rest = pair_cdr(rest);
    } else if (option_name == ":offset-assert") {
      bfd.offset_assert = compile_to_integer_constant(pair_car(rest), env);
      rest = pair_cdr(rest);
    } else if (option_name == ":size") {
      bfd.size = compile_to_integer_constant(pair_car(rest), env);
      rest = pair_cdr(rest);
    } else {
      throw_compile_error(def, "unknown option in bit field definition: " + option_name);
    }
  }

  // if user didn't set the size, we should set it ourself.
  if (bfd.size == -1) {
    bfd.size = bfd.type.type->get_size_in_non_inline_array() * BITS_PER_BYTE;
  }

  return bfd;
}

/*!
 * Parse the definition of a field for a struct/basic field.
 */
StructFieldDefinition Goal::parse_struct_field_def(const Object& def) {
  StructFieldDefinition sfd;

  Object rest = def;
  // first is name
  sfd.name = symbol_string(pair_car(rest));
  rest = pair_cdr(rest);

  // next is type
  sfd.type = compile_typespec(pair_car(rest));
  rest = pair_cdr(rest);

  if (rest.type == EMPTY_LIST)
    return sfd;

  // is it array size?
  if (pair_car(rest).type == INTEGER) {
    sfd.array_size = pair_car(rest).integer_obj.value;
    rest = pair_cdr(rest);
  }

  while (rest.type != EMPTY_LIST) {
    auto name = symbol_string(pair_car(rest));
    rest = pair_cdr(rest);
    if (name == ":inline") {
      auto val = symbol_string(pair_car(rest));
      rest = pair_cdr(rest);
      if (val == "#t") {
        sfd.is_inline = true;
      } else if (val == "#f") {
        sfd.is_inline = false;
      } else {
        throw_compile_error(def, "inline value must be #t or #f");
      }
    } else if (name == ":offset-assert") {
      if (pair_car(rest).type != INTEGER) {
        throw_compile_error(def, "offset assert must be an integer");
      }
      sfd.offset_assert = pair_car(rest).integer_obj.value;
      rest = pair_cdr(rest);
    } else if (name == ":offset") {
      if (pair_car(rest).type != INTEGER) {
        throw_compile_error(def, "offset must be an integer");
      }
      sfd.offset_override = pair_car(rest).integer_obj.value;
      rest = pair_cdr(rest);
    } else if (name == ":dynamic") {
      sfd.is_dynamic = true;
    }

    else {
      throw_compile_error(def, "unknown field spec " + name);
    }
  }

  return sfd;
}

/*!
 * Get the size of a field in a structure type.
 * For arrays, it will return the full size of the array, including padding in between elements.
 */
int Goal::get_size_in_type(GoalField& f) {
  if (f.is_dynamic) {
    return 0;
  }

  if (f.is_array) {
    if (f.is_inline) {
      // inline array
      assert(!f.type.type->is_value_type);  // not valid, arrays of value type are always "inline"
      return f.type.type->get_size_in_inline_array() * f.array_size;
    } else {
      return f.type.type->get_size_in_non_inline_array() * f.array_size;
    }
  } else {
    // not an array
    if (f.type.type->is_value_type) {
      // can't be inline
      assert(!f.is_inline);
      return f.type.type->size;
    } else {
      // is a reference type
      if (f.is_inline) {
        // but is inline
        return f.type.type->size;
      } else {
        // but is a reference
        return PTR_SIZE;
      }
    }
  }
}

/*!
 * Get the required alignment of a type as a field.
 */
static int get_alignment_in_type(TypeSpec& ts, bool in_inline) {
  if (in_inline || ts.type->is_value_type) {
    return ts.type->minimum_alignment;
  }

  // otherwise it's a reference.
  return PTR_SIZE;
}

// packed type flags for the runtime.
struct TypeFlags {
  union {
    uint64_t flag;
    struct {
      uint16_t size;
      uint16_t heap_base;
      uint16_t methods;
      uint16_t pad;
    };
  };
};

/*!
 * Helper to process the :method option to forward declare methods.
 * Shared between the bitfield and structure type definitions.
 */
void Goal::deftype_methods_helper(Object form,
                                  std::shared_ptr<GoalType> new_type,
                                  std::shared_ptr<GoalEnv> env) {
  for_each_in_list(form, [&](Object obj) {
    // (name args return-type [id])
    auto method_name = symbol_string(pair_car(obj));
    obj = pair_cdr(obj);
    auto args = pair_car(obj);
    obj = pair_cdr(obj);
    auto return_type = (pair_car(obj));
    obj = pair_cdr(obj);
    int id = -1;
    if (obj.type != EMPTY_LIST) {
      auto id_obj = pair_car(obj);
      id = compile_to_integer_constant(id_obj, env);
      expect_empty_list(pair_cdr(obj));
    }

    TypeSpec ts = get_base_typespec("function");
    ts.ts_args.emplace_back(return_type, types);
    for_each_in_list(args, [&](Object o) {
      if (o.type == SYMBOL) {
        ts.ts_args.push_back(get_base_typespec("object"));
      } else {
        auto param_args = goos.get_uneval_args(o, o, 3);
        if (param_args.unnamed_args.size() >= 3 || param_args.unnamed_args.size() < 1 ||
            param_args.has_rest || !param_args.named_args.empty()) {
          throw_compile_error(o, "invalid method parameter");
        }

        TypeSpec parm_type;

        if (param_args.unnamed_args.size() >= 2) {
          parm_type = TypeSpec(param_args.unnamed_args[1], types);  // todo improve
        } else {
          parm_type = get_base_typespec("object");
        }

        ts.ts_args.push_back(parm_type);
      }
    });

    // check that we don't modify an existing type definition
    MethodType existing;
    if (types.try_get_method_info(new_type->get_name(), method_name, &existing)) {
      if (existing.type != ts) {
        gLogger.log(MSG_WARN,
                    "deftype :methods section has redefined method %s %s\nold: %s\nnew: %s\n",
                    new_type->get_name().c_str(), method_name.c_str(),
                    existing.type.print().c_str(), ts.print().c_str());
      }
    }

    // declare the method
    int assigned_id = types.add_method(new_type->get_name(), method_name, ts);

    // check the method assert
    if (id != -1) {
      // method id assert!
      if (id != assigned_id) {
        printf("WARNING - ID assert failed on method %s of type %s (wanted %d got %d)\n",
               method_name.c_str(), new_type->get_name().c_str(), id, assigned_id);
        for (auto& method_info : types.type_method_types[new_type->get_name()]) {
          printf("  [%02d] is %s\n", method_info.id, method_info.method_name.c_str());
        }
        throw_compile_error(form, "method id assert failed");
      }
    }
  });
}

/*!
 * Helper to call the new method of type to generate the type object at runtime.
 */
std::shared_ptr<Place> Goal::deftype_call_new_method_of_type(Object form,
                                                             std::shared_ptr<GoalType> new_type,
                                                             uint64_t flags,
                                                             std::shared_ptr<GoalEnv> env) {
  auto ir = make_unique<IR_LoadInteger>();
  ir->is_signed = false;
  ir->size = 8;
  ir->us_value = flags;
  ir->value = env->alloc_reg(get_base_typespec("integer"));

  auto new_type_method = compile_get_method_of_type(get_base_typespec("type"), "new", env);
  auto new_type_symbol = compile_get_sym_obj(new_type->get_name(), env);
  auto parent_type = compile_get_sym_val(new_type->parent, env);
  auto new_type_flags = ir->value;
  env->emit(std::move(ir));

  // this new type method call will set a symbol with the name of a type. check that we don't
  // redefine an existing symbol...
  auto st_kv = symbol_types.find(
      SymbolObject::make_new(goos.reader.symbolTable, new_type->get_name()).as_symbol());
  if (st_kv != symbol_types.end() && st_kv->second != get_base_typespec("type")) {
    gLogger.log(MSG_WARN, "deftype redefines symbol %s from type %s to a type\n",
                new_type->get_name().c_str(), st_kv->second.print().c_str());
  }
  set_symbol_type(new_type->get_name(), get_base_typespec("type"));

  // do the function call!
  return compile_real_function_call(form, new_type_method,
                                    {new_type_symbol, parent_type, new_type_flags}, env);
}

/*!
 * Deftype for a structure.
 * The field placer is extremely basic - just places each field in order, putting it as close to the
 * end of the last placed field as possible. I think the real GOAL one is smarter, it'll reorder
 * your fields if it can sneak a small field into unused padding.  However, by ordering the fields
 * in the order they actually occur, this placer should have the same result.
 */
std::shared_ptr<Place> Goal::deftype_structure(std::shared_ptr<StructureType> new_type,
                                               Object fields,
                                               Object options,
                                               std::shared_ptr<GoalEnv> env) {
  // start by adding us to the types, so our fields can refer to our own type.
  types.types[new_type->name] = new_type;

  // parse all fields.
  std::vector<StructFieldDefinition> field_defs;
  for_each_in_list(fields, [&](Object o) { field_defs.emplace_back(parse_struct_field_def(o)); });

  // initialize the offset.  The offset is from the beginning of the type, and will _NOT_ include
  // the basic offset.
  int current_offset = 0;
  if (!new_type->fields.empty()) {
    // already have fields from parent, initialize to the back of that.
    current_offset = new_type->fields.back().offset + get_size_in_type(new_type->fields.back());
  }

  int basic_offset = 0;
  if (std::dynamic_pointer_cast<BasicType>(new_type))
    basic_offset = BASIC_OFFSET;

  // loop to place all fields
  for (auto& def : field_defs) {
    auto field_type = def.type;

    int offset_to_use = current_offset;
    if (def.offset_override == -1) {
      // auto place
      offset_to_use = align(current_offset, get_alignment_in_type(field_type, def.is_inline),
                            field_type.type->alignment_offset);
    } else {
      // the manual offset needs the basic offset applied, if its a basic.  So the first
      // user-defined field of a basic would go at a user offset of 0, but a real offset of 4.
      offset_to_use = def.offset_override + basic_offset;
    }

    // create the field at the calculated offset
    GoalField field(field_type, def.name, offset_to_use, def.is_inline, def.is_dynamic,
                    def.array_size);

    // if requested, check that the offset is correct
    if (def.offset_assert != -1) {
      if (def.offset_assert != offset_to_use - basic_offset) {
        throw_compile_error(fields, "offset assert failed on field " + def.name + ": got " +
                                        std::to_string(offset_to_use - basic_offset) +
                                        " expected " + std::to_string(def.offset_assert));
      }
    }

    // take the max, which will allow overlayed types to not screw up the auto-placer.
    current_offset = std::max(offset_to_use + get_size_in_type(field), current_offset);
    new_type->fields.push_back(field);
  }

  // size and padded size of the entire type.
  new_type->size = current_offset;

  // todo - heap-base assert
  // todo - size assert

  // process options for the entire type
  while (options.type != EMPTY_LIST) {
    auto current = pair_car(options);
    if (current.type == PAIR) {
      auto first = symbol_string(pair_car(current));
      if (first == ":methods") {
        deftype_methods_helper(pair_cdr(current), new_type, env);
      } else if (first == ":pack") {
        // todo - warn if packing violates minimum alignment.
        new_type->pack_structure_type = true;
      } else if (first == ":no-pack") {
        new_type->pack_structure_type = false;
      } else {
        throw_compile_error(current, "invalid option to deftype");
      }
    } else {
      throw_compile_error(current, "invalid option to deftype");
    }

    options = pair_cdr(options);
  }

  // generate code to call the (new) method of type
  TypeFlags flags;
  flags.size = new_type->size;
  flags.heap_base = 0;  // ??
  flags.methods = 64;   // todo - not this!
  if (new_type->name == "thread")
    flags.methods = 12;
  if (new_type->name == "connectable")
    flags.methods = 9;
  flags.pad = 0;
  auto new_type_obj = deftype_call_new_method_of_type(fields, new_type, flags.flag, env);

  // create an inspect function...
  auto inspector = generate_inspector_for_type(new_type, env);

  // and set it
  auto ir2 = make_unique<IR_LoadInteger>();
  ir2->is_signed = false;
  ir2->size = 8;
  ir2->us_value = 3;
  ir2->value = env->alloc_reg(get_base_typespec("integer"));
  auto value = ir2->value;
  env->emit(std::move(ir2));
  compile_real_function_call(fields, compile_get_sym_val("method-set!", env),
                             {new_type_obj, value, inspector}, env);

  return get_none();
}

/*!
 * Deftype to build a new bitfield type
 */
std::shared_ptr<Place> Goal::deftype_bitfield(std::shared_ptr<BitfieldType> new_type,
                                              Object fields,
                                              Object options,
                                              std::shared_ptr<GoalEnv> env) {
  new_type->fields.clear();

  // parse all fields.
  std::vector<BitFieldDefinition> field_defs;
  for_each_in_list(fields, [&](Object o) { field_defs.emplace_back(parse_bit_field_def(o, env)); });

  int current_offset = 0;

  // place all fields
  for (auto& def : field_defs) {
    int offset_to_use = current_offset;
    if (def.offset_override != -1) {
      offset_to_use = def.offset_override;
    }

    if (offset_to_use + def.size > new_type->size * BITS_PER_BYTE) {
      throw_compile_error(fields, "these field exceed the size of the bitfield type");
    }

    GoalBitField field(def.type, def.name, offset_to_use, def.size);
    if (def.offset_assert != -1) {
      if (def.offset_assert != offset_to_use) {
        throw_compile_error(fields, "offset assert failed on filed " + def.name + ": got " +
                                        std::to_string(offset_to_use) + "expected " +
                                        std::to_string(def.offset_assert));
      }
    }

    assert(def.size != -1);
    assert(def.size != 0);

    current_offset = std::max(offset_to_use + def.size, current_offset);
    new_type->fields.push_back(field);
  }

  // do this near the end so we can't refer to ourself in fields, but :methods can.
  types.types[new_type->name] = new_type;

  // process options for the entire type
  while (options.type != EMPTY_LIST) {
    auto current = pair_car(options);
    if (current.type == PAIR) {
      auto first = symbol_string(pair_car(current));
      if (first == ":methods") {
        deftype_methods_helper(pair_cdr(current), new_type, env);
      } else {
        throw_compile_error(current, "invalid option to deftype");
      }
    } else {
      throw_compile_error(current, "invalid option to deftype");
    }

    options = pair_cdr(options);
  }

  // generate code to call the (new) method of type
  TypeFlags flags;
  flags.size = new_type->size;
  flags.heap_base = 0;  // ??
  flags.methods = 64;   // todo - not this!
  if (new_type->name == "thread")
    flags.methods = 12;
  if (new_type->name == "connectable")
    flags.methods = 9;
  flags.pad = 0;
  auto new_type_obj = deftype_call_new_method_of_type(fields, new_type, flags.flag, env);

  //  printf("new bitfield type:\n%s\n", new_type->print().c_str());

  return get_none();
}