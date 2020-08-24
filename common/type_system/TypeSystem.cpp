#include <cassert>
#include <third-party/fmt/core.h>
#include "TypeSystem.h"
#include "type_util.h"
#include <cassert>

TypeSystem::TypeSystem() {
  // the "none" type is included by default.
  add_type("none", std::make_unique<NoneType>());
}

/*!
 * Specify a new type. If the type definition changes, it is an error if throw_on_redefine is set.
 */
Type* TypeSystem::add_type(const std::string& name, std::unique_ptr<Type> type) {
  auto kv = m_types.find(name);
  if (kv != m_types.end()) {
    // exists already

    if (*kv->second != *type) {
      // exists, and we are trying to change it!
      fmt::print("[TypeSystem] type {} was originally\n{}\nand is redefined as\n{}\n",
                 kv->second->get_name(), kv->second->print(), type->print());

      if (m_allow_redefinition) {
        // extra dangerous, we have allowed type redefinition!

        // keep the unique_ptr around, just in case somebody references this old type pointer.
        m_old_types.push_back(std::move(m_types[name]));

        // update the type
        m_types[name] = std::move(type);
      } else {
        throw std::runtime_error("Type was redefined with throw_on_redefine set.");
      }
    }
  } else {
    // newly defined!

    // none/object get to skip these checks because they are roots.
    if (name != "object" && name != "none") {
      if (m_forward_declared_types.find(type->get_parent()) != m_forward_declared_types.end()) {
        fmt::print("[TypeSystem] Type {} has incompletely defined parent {}\n", type->get_name(),
                   type->get_parent());
        throw std::runtime_error("add_type failed");
      }

      if (m_types.find(type->get_parent()) == m_types.end()) {
        fmt::print("[TypeSystem] Type {} has undefined parent {}\n", type->get_name(),
                   type->get_parent());
        throw std::runtime_error("add_type failed");
      }
    }

    m_types[name] = std::move(type);
    m_forward_declared_types.erase(name);
  }

  return m_types[name].get();
}

/*!
 * Inform the type system that there will eventually be a type named "name".
 * This will allow the type system to generate TypeSpecs for this type, but not access a Type*.
 */
void TypeSystem::forward_declare_type(std::string name) {
  m_forward_declared_types.insert(std::move(name));
}

/*!
 * Create a simple typespec.  The type must be defined or forward declared for this to work.
 */
TypeSpec TypeSystem::make_typespec(const std::string& name) {
  if (m_types.find(name) != m_types.end() ||
      m_forward_declared_types.find(name) != m_forward_declared_types.end()) {
    return TypeSpec(name);
  } else {
    fmt::print("[TypeSystem] The type {} is unknown.\n", name);
    throw std::runtime_error("make_typespec failed");
  }
}

/*!
 * Create a typespec for a function.
 */
TypeSpec TypeSystem::make_function_typespec(const std::vector<std::string>& arg_types,
                                            const std::string& return_type) {
  auto result = make_typespec("function");
  for (auto& x : arg_types) {
    result.add_arg(make_typespec(x));
  }
  result.add_arg(make_typespec(return_type));
  return result;
}

/*!
 * Get full type information. Throws if the type doesn't exist.
 */
Type* TypeSystem::lookup_type(const std::string& name) {
  auto kv = m_types.find(name);
  if (kv != m_types.end()) {
    return kv->second.get();
  }

  if (m_forward_declared_types.find(name) != m_forward_declared_types.end()) {
    fmt::print("[TypeSystem] The type {} is not fully defined.\n", name);
  } else {
    fmt::print("[TypeSystem] The type {} is not defined.\n", name);
  }

  throw std::runtime_error("lookup_type failed");
}

/*!
 * Get the method ID for the specified method.  If it doesn't exist, add it.
 * If it does exist, and newly specified method is different, throws an error.
 */
MethodInfo TypeSystem::add_method(Type* type, const std::string& method_name, const TypeSpec& ts) {
  if (method_name == "new") {
    return add_new_method(type, ts);
  }
  MethodInfo existing_info;
  bool got_existing = false;

  // first lookup the type

  auto* iter_type = type;

  // look up the method
  while (true) {
    if (iter_type->get_my_method(method_name, &existing_info)) {
      got_existing = true;
      break;
    }

    if (iter_type->has_parent()) {
      iter_type = lookup_type(iter_type->get_parent());
    } else {
      // couldn't find method.
      break;
    }
  }

  if (got_existing) {
    // make sure we aren't changing anything.
    if (ts != existing_info.type) {
      fmt::print(
          "[TypeSystem] The method {} of type {} was originally defined as {}, but has been "
          "redefined as {}\n",
          method_name, type->get_name(), existing_info.type.print(), ts.print());
      throw std::runtime_error("method redefinition");
    }

    return existing_info;
  } else {
    // add a new method!
    return type->add_method({get_next_method_id(type), method_name, ts, type->get_name()});
  }
}

/*!
 * Special case to add a new method, as new methods can specialize the arguments.
 */
MethodInfo TypeSystem::add_new_method(Type* type, const TypeSpec& ts) {
  MethodInfo existing;
  if (type->get_my_new_method(&existing)) {
    // it exists!
    if (existing.type != ts) {
      fmt::print(
          "[TypeSystem] The new method of {} was originally defined as {}, but has been redefined "
          "as {}\n",
          type->get_name(), existing.type.print(), ts.print());
      throw std::runtime_error("add_new_method failed");
    }

    return existing;
  } else {
    return type->add_new_method({0, "new", ts, type->get_name()});
  }
}

/*!
 * Lookup information on a method. Error if it can't be found.  Will check parent types if the
 * given type doesn't specialize the method.
 */
MethodInfo TypeSystem::lookup_method(const std::string& type_name, const std::string& method_name) {
  if (method_name == "new") {
    return lookup_new_method(type_name);
  }

  MethodInfo info;

  // first lookup the type
  auto* type = lookup_type(type_name);

  auto* iter_type = type;
  // look up the method
  while (true) {
    if (iter_type->get_my_method(method_name, &info)) {
      return info;
    }

    if (iter_type->has_parent()) {
      iter_type = lookup_type(iter_type->get_parent());
    } else {
      // couldn't find method.
      break;
    }
  }

  fmt::print("[TypeSystem] The method {} of type {} could not be found.\n", method_name, type_name);
  throw std::runtime_error("lookup_method failed");
}

/*!
 * Lookup information on a new method and get the most specialized version.
 */
MethodInfo TypeSystem::lookup_new_method(const std::string& type_name) {
  MethodInfo info;

  // first lookup the type
  auto* type = lookup_type(type_name);

  auto* iter_type = type;
  // look up the method
  while (true) {
    if (iter_type->get_my_new_method(&info)) {
      return info;
    }

    if (iter_type->has_parent()) {
      iter_type = lookup_type(iter_type->get_parent());
    } else {
      // couldn't find method.
      break;
    }
  }

  fmt::print("[TypeSystem] The new method of type {} could not be found.\n", type_name);
  throw std::runtime_error("lookup_new_method failed");
}

/*!
 * Get the next free method ID of a type.
 */
int TypeSystem::get_next_method_id(Type* type) {
  MethodInfo info;

  while (true) {
    if (type->get_my_last_method(&info)) {
      return info.id + 1;
    }

    if (type->has_parent()) {
      type = lookup_type(type->get_parent());
    } else {
      // nobody has defined any method yet. New is special and doens't use this, so we return
      // one after new.
      return 1;
    }
  }
}

Field TypeSystem::lookup_field(const std::string& type_name, const std::string& field_name) {
  auto type = get_type_of_type<StructureType>(type_name);
  Field field;
  if (!type->lookup_field(field_name, &field)) {
    fmt::print("[TypeSystem] Type {} has no field named {}\n", type_name, field_name);
    throw std::runtime_error("lookup_field failed");
  }
  return field;
}

void TypeSystem::assert_field_offset(const std::string& type_name,
                                     const std::string& field_name,
                                     int offset) {
  Field field = lookup_field(type_name, field_name);
  if (field.offset() != offset) {
    fmt::print("[TypeSystem] assert_field_offset({}, {}, {}) failed - got {}\n", type_name,
               field_name, offset);
    throw std::runtime_error("assert_field_offset failed");
  }
}

StructureType* TypeSystem::add_builtin_structure(const std::string& parent,
                                                 const std::string& type_name) {
  add_type(type_name, std::make_unique<StructureType>(parent, type_name));
  return get_type_of_type<StructureType>(type_name);
}

BasicType* TypeSystem::add_builtin_basic(const std::string& parent, const std::string& type_name) {
  add_type(type_name, std::make_unique<BasicType>(parent, type_name));
  return get_type_of_type<BasicType>(type_name);
}

ValueType* TypeSystem::add_builtin_value_type(const std::string& parent,
                                              const std::string& type_name,
                                              int size,
                                              bool boxed,
                                              bool sign_extend,
                                              RegKind reg) {
  add_type(type_name,
           std::make_unique<ValueType>(parent, type_name, boxed, size, sign_extend, reg));
  return get_type_of_type<ValueType>(type_name);
}

void TypeSystem::builtin_structure_inherit(StructureType* st) {
  st->inherit(get_type_of_type<StructureType>(st->get_parent()));
}

/*!
 * Add types which are built-in to GOAL.
 */
void TypeSystem::add_builtin_types() {
  // some of the basic types having confusing circular dependencies, so this is done manually.
  // there are no inlined things so its ok to do some things out of order because the actual size
  // doesn't really matter.

  // OBJECT
  auto obj_type = add_type(
      "object", std::make_unique<ValueType>("object", "object", false, 4, false, RegKind::GPR_64));

  auto structure_type = add_builtin_structure("object", "structure");
  auto basic_type = add_builtin_basic("structure", "basic");
  auto symbol_type = add_builtin_basic("basic", "symbol");
  auto type_type = add_builtin_basic("basic", "type");
  auto string_type = add_builtin_basic("basic", "string");
  auto function_type = add_builtin_basic("basic", "function");
  auto vu_function_type = add_builtin_structure("structure", "vu-function");
  auto link_block_type = add_builtin_basic("basic", "link-block");
  auto kheap_type = add_builtin_structure("structure", "kheap");
  auto array_type = add_builtin_basic("basic", "array");
  auto pair_type = add_builtin_structure("object", "pair");
  auto process_tree_type = add_builtin_basic("basic", "process-tree");
  auto process_type = add_builtin_basic("process-tree", "process");
  auto thread_type = add_builtin_basic("basic", "thread");
  auto connectable_type = add_builtin_structure("structure", "connectable");
  auto stack_frame_type = add_builtin_basic("basic", "stack-frame");
  auto file_stream_type = add_builtin_basic("basic", "file-stream");
  auto pointer_type = add_builtin_value_type("object", "pointer", 4);
  auto number_type = add_builtin_value_type("object", "number", 8);  // sign extend?
  auto float_type = add_builtin_value_type("number", "float", 4, false, false, RegKind::FLOAT);
  auto integer_type = add_builtin_value_type("number", "integer", 8, false, false);  // sign extend?
  auto binteger_type =
      add_builtin_value_type("integer", "binteger", 8, true, false);  // sign extend?
  auto sinteger_type = add_builtin_value_type("integer", "sinteger", 8, false, true);
  auto int8_type = add_builtin_value_type("sinteger", "int8", 1, false, true);
  auto int16_type = add_builtin_value_type("sinteger", "int16", 2, false, true);
  auto int32_type = add_builtin_value_type("sinteger", "int32", 4, false, true);
  auto int64_type = add_builtin_value_type("sinteger", "int64", 8, false, true);
  auto int128_type =
      add_builtin_value_type("sinteger", "int128", 16, false, true, RegKind::INT_128);
  auto uinteger_type = add_builtin_value_type("integer", "uinteger", 8);
  auto uint8_type = add_builtin_value_type("uinteger", "uint8", 1);
  auto uint16_type = add_builtin_value_type("uinteger", "uint16", 2);
  auto uint32_type = add_builtin_value_type("uinteger", "uint32", 4);
  auto uint64_type = add_builtin_value_type("uinteger", "uint64", 81);
  auto uint128_type =
      add_builtin_value_type("uinteger", "uint128", 16, false, false, RegKind::INT_128);

  // Methods and Fields

  // OBJECT
  add_method(obj_type, "new", make_function_typespec({"symbol", "type", "int32"}, "object"));
  add_method(obj_type, "delete", make_function_typespec({"object"}, "none"));
  add_method(obj_type, "print", make_function_typespec({"object"}, "object"));
  add_method(obj_type, "inspect", make_function_typespec({"object"}, "object"));
  add_method(obj_type, "length",
             make_function_typespec({"object"}, "int32"));  // todo - this integer type?
  add_method(obj_type, "asize-of", make_function_typespec({"object"}, "int32"));
  add_method(obj_type, "copy", make_function_typespec({"object", "symbol"}, "object"));
  add_method(obj_type, "relocate", make_function_typespec({"object", "int32"}, "object"));
  add_method(obj_type, "mem-usage",
             make_function_typespec({"object"}, "int32"));  // todo - this is a guess.

  // STRUCTURE
  // structure new doesn't support dynamic sizing, which is kinda weird - it grabs the size from
  // the type.  Dynamic structures use new-dynamic-structure, which is used exactly once ever.
  add_method(structure_type, "new", make_function_typespec({"symbol", "type"}, "structure"));
  // structure_type is a field-less StructureType, so we have to do this to match the runtime.
  structure_type->override_size_in_memory(4);

  // BASIC
  // we intentionally don't inherit from structure because structure's size is weird.
  add_field_to_type(basic_type, "type", make_typespec("type"));
  // the default new basic doesn't support dynamic sizing. anything dynamic will override this
  // and then call (method object new) to do the dynamically-sized allocation.
  add_method(basic_type, "new", make_function_typespec({"symbol", "type"}, "basic"));

  // SYMBOL
  builtin_structure_inherit(symbol_type);
  add_field_to_type(symbol_type, "value", make_typespec("object"));
  // a new method which returns type none means new is illegal.
  add_method(symbol_type, "new", make_function_typespec({}, "none"));

  // TYPE
  builtin_structure_inherit(type_type);
  add_field_to_type(type_type, "symbol", make_typespec("symbol"));
  add_field_to_type(type_type, "parent", make_typespec("type"));
  add_field_to_type(type_type, "allocated-size", make_typespec("uint16"));  // todo, u16 or s16?
  add_field_to_type(type_type, "psize",
                    make_typespec("uint16"));  // todo, u16 or s16. what really is this?
  add_field_to_type(type_type, "heap-base", make_typespec("uint16"));     // todo
  add_field_to_type(type_type, "method-count", make_typespec("uint16"));  // todo
  add_field_to_type(type_type, "vtable", make_typespec("function"), false, true);

  // STRING
  builtin_structure_inherit(string_type);
  add_field_to_type(string_type, "allocated-length", make_typespec("int32"));   // todo integer type
  add_field_to_type(string_type, "data", make_typespec("uint8"), false, true);  // todo integer type

  // FUNCTION
  builtin_structure_inherit(function_type);
  // ???

  // VU FUNCTION
  // don't inherit
}

int TypeSystem::manual_add_field_to_type(StructureType* type,
                                         const std::string& field_name,
                                         const TypeSpec& field_type,
                                         int offset,
                                         int size,
                                         int alignment) {
  Field field(field_name, field_type);
  field.set_alignment(alignment);
  field.set_offset(offset);
  int new_size = type->get_size_in_memory() + size;
  type->add_field(field, new_size);
  return offset;
}

int TypeSystem::add_field_to_type(StructureType* type,
                                  const std::string& field_name,
                                  const TypeSpec& field_type,
                                  bool is_inline,
                                  bool is_dynamic,
                                  int array_size,
                                  int offset_override) {
  if (type->lookup_field(field_name, nullptr)) {
    fmt::print("[TypeSystem] Type {} already has a field named {}\n", type->get_name(), field_name);
    throw std::runtime_error("add_field_to_type duplicate field names");
  }

  // first, construct the field
  Field field(field_name, field_type);
  if (is_inline) {
    field.set_inline();
  }

  if (is_dynamic) {
    field.set_dynamic();
    type->set_dynamic();
  }

  if (array_size != -1) {
    field.set_array(array_size);
  }

  int offset = offset_override;
  int field_alignment = get_alignment_in_type(field);

  if (offset == -1) {
    // we need to compute the offset ourself!
    offset = align(type->get_size_in_memory(), field_alignment);
  } else {
    int aligned_offset = align(type->get_size_in_memory(), field_alignment);
    if (offset != aligned_offset) {
      fmt::print(
          "[TypeSystem] Tried to overwrite offset of field to be {}, but it is not aligned "
          "correctly\n",
          offset);
      throw std::runtime_error("add_field_to_type bad offset_override");
    }
  }

  field.set_offset(offset);
  field.set_alignment(field_alignment);

  int after_field = offset + get_size_in_type(field);
  if (type->get_size_in_memory() < after_field) {
    type->override_size_in_memory(after_field);
  }
  type->add_field(field, type->get_size_in_memory());

  return offset;
}

std::string TypeSystem::print_all_type_information() const {
  std::string result;
  for (auto& kv : m_types) {
    result += kv.second->print() + "\n";
  }
  return result;
}

void TypeSystem::assert_method_id(const std::string& type_name,
                                  const std::string& method_name,
                                  int id) {
  auto info = lookup_method(type_name, method_name);
  if (info.id != id) {
    fmt::print(
        "[TypeSystem] Method ID assertion failed: type {}, method {} id was {}, expected {}\n",
        type_name, method_name, info.id, id);
  }
}

Type* TypeSystem::lookup_type(const TypeSpec& ts) {
  return lookup_type(ts.base_type());
}

int TypeSystem::get_alignment_in_type(const Field& field) {
  auto field_type = lookup_type(field.type());

  if (field.is_inline()) {
    if (field.is_array()) {
      return field_type->get_inline_array_alignment();
    } else {
      // it is an inlined field, so return the alignment in memory
      // TODO - for inline, but not inline array, do we use structure alignment always?
      return field_type->get_in_memory_alignment();
    }
  }

  if (!field_type->is_reference()) {
    // it is a value type, so it's stored in full:
    return field_type->get_in_memory_alignment();
  }

  // otherwise it's a reference
  return POINTER_SIZE;
}

int TypeSystem::get_size_in_type(const Field& field) {
  if (field.is_dynamic()) {
    return 0;
  }
  auto field_type = lookup_type(field.type());

  if (field.is_array()) {
    if (field.is_inline()) {
      assert(field_type->is_reference());
      return field.array_size() *
             align(field_type->get_size_in_memory(), field_type->get_inline_array_alignment());
    } else {
      if (field_type->is_reference()) {
        return field.array_size() * POINTER_SIZE;
      } else {
        return field.array_size() *
               align(field_type->get_size_in_memory(), field_type->get_in_memory_alignment());
      }
    }
  } else {
    // not an array
    if (field.is_inline()) {
      assert(field_type->is_reference());
      return align(field_type->get_size_in_memory(), field_type->get_in_memory_alignment());
    } else {
      if (field_type->is_reference()) {
        return POINTER_SIZE;
      } else {
        return align(field_type->get_size_in_memory(), field_type->get_in_memory_alignment());
      }
    }
  }
}