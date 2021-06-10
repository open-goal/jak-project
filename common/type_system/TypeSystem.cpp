/*!
 * @file TypeSystem.cpp
 * The GOAL Type System.
 * Stores types, symbol types, methods, etc, and does typechecking, lowest-common-ancestor, field
 * access types, and reverse type lookups.
 */

#include "common/util/assert.h"
#include <stdexcept>
#include <third-party/fmt/core.h>
#include "TypeSystem.h"
#include "common/util/math_util.h"
#include "third-party/fmt/color.h"

namespace {
template <typename... Args>
[[noreturn]] void throw_typesystem_error(const std::string& str, Args&&... args) {
  fmt::print(fg(fmt::color::crimson) | fmt::emphasis::bold, "-- Type Error! --\n");
  if (!str.empty() && str.back() == '\n') {
    fmt::print(fg(fmt::color::yellow), str, std::forward<Args>(args)...);
  } else {
    fmt::print(fg(fmt::color::yellow), str + '\n', std::forward<Args>(args)...);
  }

  throw std::runtime_error("Type Error");
}
}  // namespace

TypeSystem::TypeSystem() {
  // the "none" and "_type_" types are included by default.
  add_type("none", std::make_unique<NullType>("none"));
  add_type("_type_", std::make_unique<NullType>("_type_"));
  add_type("_varargs_", std::make_unique<NullType>("_varargs_"));
}

/*!
 * Add a new type. If the type exists, and this new type is different, it is an error if
 * throw_on_redefine is set. The type should be fully set up (fields, etc) before running this.
 */
Type* TypeSystem::add_type(const std::string& name, std::unique_ptr<Type> type) {
  auto kv = m_types.find(name);
  if (kv != m_types.end()) {
    // exists already

    if (*kv->second != *type) {
      // exists, and we are trying to change it!

      if (m_allow_redefinition) {
        fmt::print("[TypeSystem] Type {} was originally\n{}\nand is redefined as\n{}\n",
                   kv->second->get_name(), kv->second->print(), type->print());
        // extra dangerous, we have allowed type redefinition!

        // keep the unique_ptr around, just in case somebody references this old type pointer.
        m_old_types.push_back(std::move(m_types[name]));

        // update the type
        m_types[name] = std::move(type);
      } else {
        throw_typesystem_error(
            "Inconsistent type definition. Type {} was originally\n{}\nand is redefined as\n{}\n",
            kv->second->get_name(), kv->second->print(), type->print());
      }
    }
  } else {
    // newly defined!

    // none/object get to skip these checks because they are roots.
    if (name != "object" && name != "none" && name != "_type_" && name != "_varargs_") {
      if (m_forward_declared_types.find(type->get_parent()) != m_forward_declared_types.end()) {
        throw_typesystem_error(
            "Cannot create new type {}. The parent type {} is not fully defined.\n",
            type->get_name(), type->get_parent());
      }

      if (m_types.find(type->get_parent()) == m_types.end()) {
        throw_typesystem_error("Cannot create new type {}. The parent type {} is not defined.\n",
                               type->get_name(), type->get_parent());
      }
    }

    m_types[name] = std::move(type);
    m_forward_declared_types.erase(name);
  }

  return m_types[name].get();
}

/*!
 * Inform the type system that there will eventually be a type named "name".
 * This will allow the type system to generate TypeSpecs for this type, but not access detailed
 * information, or know the exact size.
 */
void TypeSystem::forward_declare_type(const std::string& name) {
  if (m_types.find(name) == m_types.end()) {
    m_forward_declared_types[name] = TYPE;
  }
}

/*!
 * Inform the type system that there will eventually be a type named "name" and that it's a basic.
 * This allows the type to be used in a few specific places. For instance a basic can have
 * a field where an element is the same type.
 */
void TypeSystem::forward_declare_type_as_basic(const std::string& name) {
  if (m_types.find(name) == m_types.end()) {
    m_forward_declared_types[name] = BASIC;
  }
}

/*!
 * Inform the type system that there will eventually be a type named "name" and that it's a
 * structure. This allows the type to be used in a few specific places. For instance a structure can
 * have a field where an element is the same type.
 */
void TypeSystem::forward_declare_type_as_structure(const std::string& name) {
  if (m_types.find(name) == m_types.end()) {
    m_forward_declared_types[name] = STRUCTURE;
  }
}

/*!
 * Get the runtime type (as a name string) of a TypeSpec.  Gets the runtime type of the primary
 * type of the TypeSpec.
 */
std::string TypeSystem::get_runtime_type(const TypeSpec& ts) {
  return lookup_type(ts)->get_runtime_name();
}

/*!
 * Get information about what happens if you dereference an object of given type
 */
DerefInfo TypeSystem::get_deref_info(const TypeSpec& ts) const {
  DerefInfo info;

  if (!ts.has_single_arg()) {
    // not enough info.
    info.can_deref = false;
    return info;
  }

  // default to GPR
  info.reg = RegClass::GPR_64;
  info.mem_deref = true;

  if (tc(TypeSpec("float"), ts)) {
    info.reg = RegClass::FLOAT;
  }

  if (ts.base_type() == "inline-array") {
    auto result_type = lookup_type(ts.get_single_arg());
    auto result_structure_type = dynamic_cast<StructureType*>(result_type);
    if (!result_structure_type || result_structure_type->is_dynamic()) {
      info.can_deref = false;
      return info;
    }

    // it's an inline array of structures. We can "dereference". But really we don't do a memory
    // dereference, we just add stride*idx to the pointer.
    info.can_deref = true;                   // deref operators should work...
    info.mem_deref = false;                  // but don't actually dereference a pointer
    info.result_type = ts.get_single_arg();  // what we're an inline-array of
    info.sign_extend = false;                // not applicable anyway

    if (result_type->is_reference()) {
      info.stride = align(result_type->get_size_in_memory(),
                          result_type->get_inline_array_stride_alignment());
    } else {
      // can't have an inline array of value types!
      assert(false);
    }
  } else if (ts.base_type() == "pointer") {
    info.can_deref = true;
    info.result_type = ts.get_single_arg();
    auto result_type = lookup_type_allow_partial_def(info.result_type);
    if (result_type->is_reference()) {
      // in memory, an array of pointers
      info.stride = POINTER_SIZE;
      info.sign_extend = false;
      info.load_size = POINTER_SIZE;
    } else {
      // an array of values, which should be loaded in the correct way to the correct register
      info.stride = result_type->get_size_in_memory();
      info.sign_extend = result_type->get_load_signed();
      info.reg = result_type->get_preferred_reg_class();
      info.load_size = result_type->get_load_size();
      assert(result_type->get_size_in_memory() == result_type->get_load_size());
    }
  } else {
    info.can_deref = false;
  }

  return info;
}

/*!
 * Create a simple typespec.  The type must be defined or forward declared for this to work.
 * If you really need a TypeSpec which refers to a non-existent type, just construct your own.
 */
TypeSpec TypeSystem::make_typespec(const std::string& name) const {
  if (m_types.find(name) != m_types.end() ||
      m_forward_declared_types.find(name) != m_forward_declared_types.end()) {
    return TypeSpec(name);
  } else {
    throw_typesystem_error("Type {} is unknown\n", name);
  }
}

bool TypeSystem::fully_defined_type_exists(const std::string& name) const {
  return m_types.find(name) != m_types.end();
}

bool TypeSystem::fully_defined_type_exists(const TypeSpec& type) const {
  return fully_defined_type_exists(type.base_type());
}

bool TypeSystem::partially_defined_type_exists(const std::string& name) const {
  return m_forward_declared_types.find(name) != m_forward_declared_types.end();
}

TypeSpec TypeSystem::make_array_typespec(const TypeSpec& element_type) const {
  return TypeSpec("array", {element_type});
}

/*!
 * Create a typespec for a function.  If the function doesn't return anything, use "none" as the
 * return type.
 */
TypeSpec TypeSystem::make_function_typespec(const std::vector<std::string>& arg_types,
                                            const std::string& return_type) const {
  auto result = make_typespec("function");
  for (auto& x : arg_types) {
    result.add_arg(make_typespec(x));
  }
  result.add_arg(make_typespec(return_type));
  return result;
}

/*!
 * Create a TypeSpec for a pointer to a type.
 */
TypeSpec TypeSystem::make_pointer_typespec(const std::string& type) const {
  return make_pointer_typespec(make_typespec(type));
}

/*!
 * Create a TypeSpec for a pointer to a type.
 */
TypeSpec TypeSystem::make_pointer_typespec(const TypeSpec& type) const {
  return TypeSpec("pointer", {type});
}

/*!
 * Create a TypeSpec for an inline-array of type
 */
TypeSpec TypeSystem::make_inline_array_typespec(const std::string& type) const {
  return make_inline_array_typespec(make_typespec(type));
}

/*!
 * Create a TypeSpec for an inline-array of type
 */
TypeSpec TypeSystem::make_inline_array_typespec(const TypeSpec& type) const {
  return TypeSpec("inline-array", {type});
}

/*!
 * Get full type information. Throws if the type doesn't exist. If the given type is redefined after
 * a call to lookup_type, the Type* will still be valid, but will point to the old data. Whenever
 * possible, don't store a Type* and store a TypeSpec instead.  The TypeSpec can then be used with
 * lookup_type to find the most up-to-date type information.
 */
Type* TypeSystem::lookup_type(const std::string& name) const {
  auto kv = m_types.find(name);
  if (kv != m_types.end()) {
    return kv->second.get();
  }

  if (m_forward_declared_types.find(name) != m_forward_declared_types.end()) {
    throw_typesystem_error("Type {} is not fully defined.\n", name);
  } else {
    throw_typesystem_error("Type {} is not defined.\n", name);
  }
  throw std::runtime_error("lookup_type failed");
}

/*!
 * Get full type information. Throws if the type doesn't exist. If the given type is redefined after
 * a call to lookup_type, the Type* will still be valid, but will point to the old data. Whenever
 * possible, don't store a Type* and store a TypeSpec instead.  The TypeSpec can then be used with
 * lookup_type to find the most up-to-date type information.
 */
Type* TypeSystem::lookup_type(const TypeSpec& ts) const {
  return lookup_type(ts.base_type());
}

/*!
 * Get type info. If the type is not fully defined (ie, we are parsing its deftype now) and its
 * forward defined as a basic or structure, just get basic/structure.
 */
Type* TypeSystem::lookup_type_allow_partial_def(const TypeSpec& ts) const {
  return lookup_type_allow_partial_def(ts.base_type());
}

/*!
 * Get type info. If the type is not fully defined (ie, we are parsing its deftype now) and its
 * forward defined as a basic or structure, just get basic/structure.
 */
Type* TypeSystem::lookup_type_allow_partial_def(const std::string& name) const {
  // look up fully defined types first:
  auto kv = m_types.find(name);
  if (kv != m_types.end()) {
    return kv->second.get();
  }

  auto fwd_dec = m_forward_declared_types.find(name);
  if (fwd_dec != m_forward_declared_types.end()) {
    if (fwd_dec->second == STRUCTURE) {
      return lookup_type("structure");
    } else if (fwd_dec->second == BASIC) {
      return lookup_type("basic");
    } else {
      throw_typesystem_error(
          "The type {} is known to be a type, but has not been defined with deftype "
          "or properly forward declared with declare-type\n",
          name);
    }
  }
  throw_typesystem_error("The type {} is unknown.\n", name);
}

MethodInfo TypeSystem::declare_method(const std::string& type_name,
                                      const std::string& method_name,
                                      bool no_virtual,
                                      const TypeSpec& ts) {
  return declare_method(lookup_type(make_typespec(type_name)), method_name, no_virtual, ts);
}

/*!
 * Add a method, if it doesn't exist. If the method already exists (possibly in a parent), checks to
 * see if this is an identical definition.  If not, it's an error, and if so, nothing happens.
 * Returns the info of either the existing or newly created method.
 *
 * This is not used to override methods, but instead to create truly new methods.  The one exception
 * is overriding the "new" method - the TypeSystem will track that because overridden new methods
 * may have different arguments.
 */
MethodInfo TypeSystem::declare_method(Type* type,
                                      const std::string& method_name,
                                      bool no_virtual,
                                      const TypeSpec& ts) {
  if (method_name == "new") {
    return add_new_method(type, ts);
  }

  // look up the method
  MethodInfo existing_info;
  bool got_existing = try_lookup_method(type, method_name, &existing_info);

  if (got_existing) {
    // make sure we aren't changing anything.
    if (!existing_info.type.is_compatible_child_method(ts, type->get_name())) {
      throw_typesystem_error(
          "The method {} of type {} was originally declared as {}, but has been "
          "redeclared as {}\n",
          method_name, type->get_name(), existing_info.type.print(), ts.print());
    }

    if ((existing_info.no_virtual || no_virtual) &&
        existing_info.defined_in_type != type->get_name()) {
      throw_typesystem_error(
          "Cannot define method {} in type {} when it was defined as no_virtual in parent type {}",
          method_name, type->get_name(), existing_info.defined_in_type);
    }

    if (no_virtual != existing_info.no_virtual) {
      throw_typesystem_error(
          "The method {} of type {} was originally declared with no_virtual = {}, but has been "
          "redeclared as {}",
          method_name, type->get_name(), existing_info.no_virtual, no_virtual);
    }

    return existing_info;
  } else {
    // add a new method!
    return type->add_method(
        {get_next_method_id(type), method_name, ts, type->get_name(), no_virtual});
  }
}

MethodInfo TypeSystem::define_method(const std::string& type_name,
                                     const std::string& method_name,
                                     const TypeSpec& ts) {
  return define_method(lookup_type(make_typespec(type_name)), method_name, ts);
}

/*!
 * Add a method, if it doesn't exist. If the method already exists (possibly in a parent), checks to
 * see if this is an identical definition.  If not, it's an error, and if so, nothing happens.
 * Returns the info of either the existing or newly created method.
 *
 * This is not used to override methods, but instead to create truly new methods.  The one exception
 * is overriding the "new" method - the TypeSystem will track that because overridden new methods
 * may have different arguments.
 */
MethodInfo TypeSystem::define_method(Type* type,
                                     const std::string& method_name,
                                     const TypeSpec& ts) {
  if (method_name == "new") {
    return add_new_method(type, ts);
  }

  // look up the method
  MethodInfo existing_info;
  bool got_existing = try_lookup_method(type, method_name, &existing_info);

  if (got_existing) {
    // make sure we aren't changing anything.
    if (!existing_info.type.is_compatible_child_method(ts, type->get_name())) {
      throw_typesystem_error(
          "The method {} of type {} was originally defined as {}, but has been "
          "redefined as {}\n",
          method_name, type->get_name(), existing_info.type.print(), ts.print());
    }

    return existing_info;
  } else {
    throw_typesystem_error("Cannot add method {} to type {} because it was not declared.\n",
                           method_name, type->get_name());
  }
}

/*!
 * Special case to add a new method, as new methods can specialize the arguments.
 * If it turns out that other child methods can specialize arguments (seems like a bad idea), this
 * may be generalized.
 */
MethodInfo TypeSystem::add_new_method(Type* type, const TypeSpec& ts) {
  MethodInfo existing;
  if (type->get_my_new_method(&existing)) {
    // it exists!
    if (!existing.type.is_compatible_child_method(ts, type->get_name())) {
      throw_typesystem_error(
          "Cannot add new method. Type does not match declaration. The new method of {} was "
          "originally defined as {}, but has been redefined as {}\n",
          type->get_name(), existing.type.print(), ts.print());
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
MethodInfo TypeSystem::lookup_method(const std::string& type_name,
                                     const std::string& method_name) const {
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

  throw_typesystem_error("The method {} of type {} could not be found.\n", method_name, type_name);
}

bool TypeSystem::try_lookup_method(const std::string& type_name,
                                   const std::string& method_name,
                                   MethodInfo* info) const {
  auto kv = m_types.find(type_name);
  if (kv == m_types.end()) {
    return false;
  }

  return try_lookup_method(kv->second.get(), method_name, info);
}

/*!
 * Like lookup_method, but won't throw or print an error when things go wrong.
 */
bool TypeSystem::try_lookup_method(const std::string& type_name,
                                   int method_id,
                                   MethodInfo* info) const {
  auto kv = m_types.find(type_name);
  if (kv == m_types.end()) {
    return false;
  }

  auto* iter_type = kv->second.get();
  // look up the method
  while (true) {
    if (method_id == GOAL_NEW_METHOD) {
      if (iter_type->get_my_new_method(info)) {
        return true;
      }
    } else {
      if (iter_type->get_my_method(method_id, info)) {
        return true;
      }
    }

    if (iter_type->has_parent()) {
      iter_type = lookup_type(iter_type->get_parent());
    } else {
      // couldn't find method.
      break;
    }
  }
  return false;
}

bool TypeSystem::try_lookup_method(const Type* type,
                                   const std::string& method_name,
                                   MethodInfo* info) const {
  // look up the method
  while (true) {
    if (method_name == "new") {
      if (type->get_my_new_method(info)) {
        return true;
      }
    } else {
      if (type->get_my_method(method_name, info)) {
        return true;
      }
    }

    if (type->has_parent()) {
      type = lookup_type(type->get_parent());
    } else {
      // couldn't find method.
      break;
    }
  }
  return false;
}

/*!
 * Lookup information on a method by ID number. Error if it can't be found.  Will check parent types
 * if the given type doesn't specialize the method.
 */
MethodInfo TypeSystem::lookup_method(const std::string& type_name, int method_id) const {
  if (method_id == GOAL_NEW_METHOD) {
    return lookup_new_method(type_name);
  }

  MethodInfo info;

  // first lookup the type
  auto* type = lookup_type(type_name);

  auto* iter_type = type;
  // look up the method
  while (true) {
    if (iter_type->get_my_method(method_id, &info)) {
      return info;
    }

    if (iter_type->has_parent()) {
      iter_type = lookup_type(iter_type->get_parent());
    } else {
      // couldn't find method.
      break;
    }
  }

  throw_typesystem_error("The method with id {} of type {} could not be found.\n", method_id,
                         type_name);
}

/*!
 * Lookup information on a new method and get the most specialized version.
 */
MethodInfo TypeSystem::lookup_new_method(const std::string& type_name) const {
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

  throw_typesystem_error("The new method of type {} could not be found.\n", type_name);
}

/*!
 * Makes sure a method exists at the given ID for the given type, possibly defined in a parent.
 */
void TypeSystem::assert_method_id(const std::string& type_name,
                                  const std::string& method_name,
                                  int id) {
  auto info = lookup_method(type_name, method_name);
  if (info.id != id) {
    throw_typesystem_error(
        "Method ID assertion failed: type {}, method {} id was {}, expected {}\n", type_name,
        method_name, info.id, id);
  }
}

/*!
 * Lookup detailed information about a field of a type by name, including type, offset,
 * and how to access it.
 */
FieldLookupInfo TypeSystem::lookup_field_info(const std::string& type_name,
                                              const std::string& field_name) const {
  FieldLookupInfo info;
  info.field = lookup_field(type_name, field_name);

  // get array size, for bounds checking (when possible)
  if (info.field.is_array() && !info.field.is_dynamic()) {
    info.array_size = info.field.array_size();
  }

  auto base_type = lookup_type_allow_partial_def(info.field.type());
  if (base_type->is_reference()) {
    if (info.field.is_inline()) {
      if (info.field.is_array()) {
        // inline array of reference types
        info.needs_deref = false;
        info.type = make_inline_array_typespec(info.field.type());
      } else {
        // inline object
        info.needs_deref = false;
        info.type = info.field.type();
      }
    } else {
      if (info.field.is_array()) {
        info.needs_deref = false;
        info.type = make_pointer_typespec(info.field.type());
      } else {
        info.needs_deref = true;
        info.type = info.field.type();
      }
    }
  } else {
    if (info.field.is_array()) {
      info.needs_deref = false;
      info.type = make_pointer_typespec(info.field.type());
    } else {
      // not array
      info.needs_deref = true;
      info.type = info.field.type();
    }
  }
  return info;
}

/*!
 * Make sure a field is located at the specified offset.
 */
void TypeSystem::assert_field_offset(const std::string& type_name,
                                     const std::string& field_name,
                                     int offset) {
  Field field = lookup_field(type_name, field_name);
  if (field.offset() != offset) {
    throw_typesystem_error("assert_field_offset({}, {}, {}) failed - got {}\n", type_name,
                           field_name, offset);
    throw std::runtime_error("assert_field_offset failed");
  }
}

/*!
 * Add a field to a type. If offset_override is -1 (the default), will place it automatically.
 */
int TypeSystem::add_field_to_type(StructureType* type,
                                  const std::string& field_name,
                                  const TypeSpec& field_type,
                                  bool is_inline,
                                  bool is_dynamic,
                                  int array_size,
                                  int offset_override,
                                  bool skip_in_static_decomp) {
  if (type->lookup_field(field_name, nullptr)) {
    throw_typesystem_error("Type {} already has a field named {}\n", type->get_name(), field_name);
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
    int aligned_offset = align(offset, field_alignment);
    field.mark_as_user_placed();
    if (offset != aligned_offset) {
      throw_typesystem_error("Tried to place field {} at {}, but it is not aligned correctly\n",
                             field_name, offset);
    }
  }

  field.set_offset(offset);
  field.set_alignment(field_alignment);
  if (skip_in_static_decomp) {
    field.set_skip_in_static_decomp();
  }

  int after_field = offset + get_size_in_type(field);
  if (type->get_size_in_memory() < after_field) {
    type->override_size_in_memory(after_field);
  }
  type->add_field(field, type->get_size_in_memory());

  return offset;
}

/*!
 * Add types which are built-in to GOAL.
 */
void TypeSystem::add_builtin_types() {
  // some of the basic types have confusing circular dependencies, so this is done manually.
  // there are no inlined things so its ok to do some things out of order because the actual size
  // doesn't really matter.

  // OBJECT
  auto obj_type = add_type(
      "object", std::make_unique<ValueType>("object", "object", false, 4, false, RegClass::GPR_64));

  auto structure_type = add_builtin_structure("object", "structure");
  auto basic_type = add_builtin_basic("structure", "basic");
  auto symbol_type = add_builtin_basic("basic", "symbol");
  auto type_type = add_builtin_basic("basic", "type");
  auto string_type = add_builtin_basic("basic", "string");
  string_type->set_final();  // no virtual calls used on string.
  auto function_type = add_builtin_basic("basic", "function");
  auto vu_function_type = add_builtin_structure("structure", "vu-function");
  auto link_block_type = add_builtin_basic("basic", "link-block");
  auto kheap_type = add_builtin_structure("structure", "kheap");
  auto array_type = add_builtin_basic("basic", "array");
  auto pair_type = add_builtin_structure("object", "pair", true);
  auto connectable_type = add_builtin_structure("structure", "connectable");
  auto file_stream_type = add_builtin_basic("basic", "file-stream");
  add_builtin_value_type("object", "pointer", 4);
  auto inline_array_type = add_builtin_value_type("object", "inline-array", 4);
  inline_array_type->set_runtime_type("pointer");

  add_builtin_value_type("object", "number", 8);  // sign extend?
  add_builtin_value_type("number", "float", 4, false, false, RegClass::FLOAT);
  add_builtin_value_type("number", "integer", 8, false, false);   // sign extend?
  add_builtin_value_type("integer", "binteger", 8, true, false);  // sign extend?
  add_builtin_value_type("integer", "sinteger", 8, false, true);
  add_builtin_value_type("sinteger", "int8", 1, false, true);
  add_builtin_value_type("sinteger", "int16", 2, false, true);
  add_builtin_value_type("sinteger", "int32", 4, false, true);
  add_builtin_value_type("sinteger", "int64", 8, false, true);
  add_builtin_value_type("sinteger", "int128", 16, false, true, RegClass::INT_128);
  add_builtin_value_type("integer", "uinteger", 8);
  add_builtin_value_type("uinteger", "uint8", 1);
  add_builtin_value_type("uinteger", "uint16", 2);
  add_builtin_value_type("uinteger", "uint32", 4);
  add_builtin_value_type("uinteger", "uint64", 8);
  add_builtin_value_type("uinteger", "uint128", 16, false, false, RegClass::INT_128);

  auto int_type = add_builtin_value_type("integer", "int", 8, false, true);
  int_type->disallow_in_runtime();
  auto uint_type = add_builtin_value_type("uinteger", "uint", 8, false, false);
  uint_type->disallow_in_runtime();

  // Methods and Fields
  forward_declare_type_as_structure("memory-usage-block");

  // OBJECT
  declare_method(obj_type, "new", false,
                 make_function_typespec({"symbol", "type", "int"}, "_type_"));
  declare_method(obj_type, "delete", false, make_function_typespec({"_type_"}, "none"));
  declare_method(obj_type, "print", false, make_function_typespec({"_type_"}, "_type_"));
  declare_method(obj_type, "inspect", false, make_function_typespec({"_type_"}, "_type_"));
  declare_method(obj_type, "length", false,
                 make_function_typespec({"_type_"}, "int"));  // todo - this integer type?
  declare_method(obj_type, "asize-of", false, make_function_typespec({"_type_"}, "int"));
  declare_method(obj_type, "copy", false, make_function_typespec({"_type_", "symbol"}, "_type_"));
  declare_method(obj_type, "relocate", false, make_function_typespec({"_type_", "int"}, "_type_"));
  declare_method(obj_type, "mem-usage", false,
                 make_function_typespec({"_type_", "memory-usage-block", "int"}, "_type_"));

  // STRUCTURE
  // structure new doesn't support dynamic sizing, which is kinda weird - it grabs the size from
  // the type.  Dynamic structures use new-dynamic-structure, which is used exactly once ever.
  declare_method(structure_type, "new", false,
                 make_function_typespec({"symbol", "type"}, "_type_"));
  // structure_type is a field-less StructureType, so we have to do this to match the runtime.
  //  structure_type->override_size_in_memory(4);

  // BASIC
  // we intentionally don't inherit from structure because structure's size is weird.
  add_field_to_type(basic_type, "type", make_typespec("type"));
  // the default new basic doesn't support dynamic sizing. anything dynamic will override this
  // and then call (method object new) to do the dynamically-sized allocation.
  declare_method(basic_type, "new", false, make_function_typespec({"symbol", "type"}, "_type_"));

  // SYMBOL
  builtin_structure_inherit(symbol_type);
  add_field_to_type(symbol_type, "value", make_typespec("object"));
  // a new method which returns type none means new is illegal.
  declare_method(symbol_type, "new", false, make_function_typespec({}, "none"));

  // TYPE
  builtin_structure_inherit(type_type);
  declare_method(type_type, "new", false,
                 make_function_typespec({"symbol", "type", "int"}, "_type_"));
  add_field_to_type(type_type, "symbol", make_typespec("symbol"));
  add_field_to_type(type_type, "parent", make_typespec("type"));
  add_field_to_type(type_type, "size", make_typespec("uint16"));  // actually u16
  add_field_to_type(type_type, "psize",
                    make_typespec("uint16"));  // todo, u16 or s16. what really is this?
  add_field_to_type(type_type, "heap-base", make_typespec("uint16"));         // todo
  add_field_to_type(type_type, "allocated-length", make_typespec("uint16"));  // todo
  add_field_to_type(type_type, "method-table", make_typespec("function"), false, true);

  // STRING
  builtin_structure_inherit(string_type);
  add_field_to_type(string_type, "allocated-length", make_typespec("int32"));   // todo integer type
  add_field_to_type(string_type, "data", make_typespec("uint8"), false, true);  // todo integer type
  // string is never deftype'd for the decompiler, so we need to manually give the constructor
  // type here.
  declare_method(string_type, "new", false,
                 make_function_typespec({"symbol", "type", "int", "string"}, "_type_"));

  // FUNCTION
  builtin_structure_inherit(function_type);
  // ???

  // VU FUNCTION
  // don't inherit
  add_field_to_type(vu_function_type, "length", make_typespec("int32"));   // todo integer type
  add_field_to_type(vu_function_type, "origin", make_typespec("int32"));   // todo sign extend?
  add_field_to_type(vu_function_type, "qlength", make_typespec("int32"));  // todo integer type
  add_field_to_type(vu_function_type, "data", make_typespec("uint8"), false, true);

  // link block
  builtin_structure_inherit(link_block_type);
  add_field_to_type(link_block_type, "allocated-length",
                    make_typespec("int32"));                              // todo integer type
  add_field_to_type(link_block_type, "version", make_typespec("int32"));  // todo integer type
  // there's probably some dynamically sized stuff after this...

  // kheap
  add_field_to_type(kheap_type, "base", make_typespec("pointer"));
  add_field_to_type(kheap_type, "top", make_typespec("pointer"));
  add_field_to_type(kheap_type, "current", make_typespec("pointer"));
  add_field_to_type(kheap_type, "top-base", make_typespec("pointer"));

  // todo
  builtin_structure_inherit(array_type);
  declare_method(array_type, "new", false,
                 make_function_typespec({"symbol", "type", "type", "int"}, "_type_"));
  // array has: number, number, type
  add_field_to_type(array_type, "length", make_typespec("int32"));
  add_field_to_type(array_type, "allocated-length", make_typespec("int32"));
  add_field_to_type(array_type, "content-type", make_typespec("type"));
  add_field_to_type(array_type, "data", make_typespec("uint8"), false, true);

  // pair
  pair_type->override_offset(2);
  declare_method(pair_type, "new", false,
                 make_function_typespec({"symbol", "type", "object", "object"}, "_type_"));
  add_field_to_type(pair_type, "car", make_typespec("object"));
  add_field_to_type(pair_type, "cdr", make_typespec("object"));

  // this type is very strange, as the compiler knows about it in gkernel-h, yet it is
  // defined inside of connect.
  add_field_to_type(connectable_type, "next0", make_typespec("connectable"));
  add_field_to_type(connectable_type, "prev0", make_typespec("connectable"));
  add_field_to_type(connectable_type, "next1", make_typespec("connectable"));
  add_field_to_type(connectable_type, "prev1", make_typespec("connectable"));

  // todo
  builtin_structure_inherit(file_stream_type);
  add_field_to_type(file_stream_type, "flags", make_typespec("uint32"));
  add_field_to_type(file_stream_type, "mode", make_typespec("basic"));
  add_field_to_type(file_stream_type, "name", make_typespec("string"));
  add_field_to_type(file_stream_type, "file", make_typespec("uint32"));
  declare_method(file_stream_type, "new", false,
                 make_function_typespec({"symbol", "type", "string", "basic"}, "_type_"));
}

/*!
 * Debugging function to print out all types, and their methods and fields.
 */
std::string TypeSystem::print_all_type_information() const {
  std::string result;
  for (auto& kv : m_types) {
    result += kv.second->print() + "\n";
  }
  return result;
}

/*!
 * Get the next free method ID of a type.
 */
int TypeSystem::get_next_method_id(const Type* type) const {
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

/*!
 * Lookup a field of a type by name
 */
Field TypeSystem::lookup_field(const std::string& type_name, const std::string& field_name) const {
  auto type = get_type_of_type<StructureType>(type_name);
  Field field;
  if (!type->lookup_field(field_name, &field)) {
    throw_typesystem_error("Type {} has no field named {}\n", type_name, field_name);
  }
  return field;
}

/*!
 * Get the minimum required aligment of a field.
 */
int TypeSystem::get_alignment_in_type(const Field& field) {
  auto field_type = lookup_type_allow_partial_def(field.type());

  if (field.is_inline()) {
    if (field.is_array()) {
      // TODO - is this actually correct? or do we use in_memory for the first element and
      // inline_array for the ones that follow?
      return field_type->get_inline_array_start_alignment();
    } else {
      // it is an inlined field, so return the alignment in memory
      // TODO - for inline, but not inline array, do we use structure alignment always?
      return field_type->get_inline_array_start_alignment();
    }
  }

  if (!field_type->is_reference()) {
    // it is a value type, so it's stored in full:
    return field_type->get_in_memory_alignment();
  }

  // otherwise it's a reference
  return POINTER_SIZE;
}

namespace {
bool allow_inline(const Type* type) {
  auto name = type->get_name();
  return name != "basic" && name != "structure";
}
}  // namespace

/*!
 * Get the size of a field in a type.  The array sizes should be consistent with get_deref_info's
 * stride.
 */
int TypeSystem::get_size_in_type(const Field& field) const {
  if (field.is_dynamic()) {
    return 0;
  }
  auto field_type = lookup_type_allow_partial_def(field.type());

  if (field.is_array()) {
    if (field.is_inline()) {
      if (!fully_defined_type_exists(field.type())) {
        throw_typesystem_error("Cannot use the forward-declared type {} in an inline array.\n",
                               field.type().print());
      }
      if (!allow_inline(field_type)) {
        throw_typesystem_error(
            "Attempted to use `{}` inline, this probably isn't what you wanted.\n",
            field_type->get_name());
      }
      assert(field_type->is_reference());
      return field.array_size() * align(field_type->get_size_in_memory(),
                                        field_type->get_inline_array_stride_alignment());
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
      if (!fully_defined_type_exists(field.type())) {
        throw_typesystem_error("Cannot use the forward-declared type {} inline.\n",
                               field.type().print());
      }
      if (!allow_inline(field_type)) {
        throw_typesystem_error(
            "Attempted to use `{}` inline, this probably isn't what you wanted. Type "
            "may not be defined fully.\n",
            field_type->get_name());
      }
      assert(field_type->is_reference());
      // return align(field_type->get_size_in_memory(), field_type->get_in_memory_alignment());
      // looking at dead-pool-heap we tightly pack in this case
      return field_type->get_size_in_memory();
    } else {
      if (field_type->is_reference()) {
        return POINTER_SIZE;
      } else {
        return align(field_type->get_size_in_memory(), field_type->get_in_memory_alignment());
      }
    }
  }
}

/*!
 * Add a simple structure type - don't use this outside of add_builtin_types as it forces you to do
 * things in the wrong order.
 */
StructureType* TypeSystem::add_builtin_structure(const std::string& parent,
                                                 const std::string& type_name,
                                                 bool boxed) {
  add_type(type_name, std::make_unique<StructureType>(parent, type_name, boxed, false, false, 0));
  return get_type_of_type<StructureType>(type_name);
}

/*!
 * Add a simple basic type - don't use this outside of add_builtin_types as it forces you to do
 * things in the wrong order.
 */
BasicType* TypeSystem::add_builtin_basic(const std::string& parent, const std::string& type_name) {
  add_type(type_name, std::make_unique<BasicType>(parent, type_name, false, 0));
  return get_type_of_type<BasicType>(type_name);
}

/*!
 * Add a simple value type - don't use this outside of add_builtin_types as it forces you to do
 * things in the wrong order.
 */
ValueType* TypeSystem::add_builtin_value_type(const std::string& parent,
                                              const std::string& type_name,
                                              int size,
                                              bool boxed,
                                              bool sign_extend,
                                              RegClass reg) {
  add_type(type_name,
           std::make_unique<ValueType>(parent, type_name, boxed, size, sign_extend, reg));
  return get_type_of_type<ValueType>(type_name);
}

/*!
 * Helper for inheritance of structure types when setting up builtin types.
 */
void TypeSystem::builtin_structure_inherit(StructureType* st) {
  st->inherit(get_type_of_type<StructureType>(st->get_parent()));
}

bool TypeSystem::tc(const TypeSpec& expected, const TypeSpec& actual) const {
  return typecheck_and_throw(expected, actual, "", false, false);
}

/*!
 * Main compile-time type check!
 * @param expected - the expected type
 * @param actual - the actual type (can be more specific)
 * @param error_source_name - optional, can provide a name for where the error comes from
 * @param print_on_error - print a message explaining the type error, if there is one
 * @param throw_on_error - throw a std::runtime_error on failure if set.
 * @return if the type check passes
 */
bool TypeSystem::typecheck_and_throw(const TypeSpec& expected,
                                     const TypeSpec& actual,
                                     const std::string& error_source_name,
                                     bool print_on_error,
                                     bool throw_on_error) const {
  bool success = true;
  // first, typecheck the base types:
  if (!typecheck_base_types(expected.base_type(), actual.base_type())) {
    success = false;
  }

  // next argument checks:
  if (expected.m_arguments.size() == actual.m_arguments.size()) {
    for (size_t i = 0; i < expected.m_arguments.size(); i++) {
      // don't print/throw because the error would be confusing. Better to fail only the
      // outer most check and print a single error message.
      if (!tc(expected.m_arguments[i], actual.m_arguments[i])) {
        success = false;
        break;
      }
    }
  } else {
    // different sizes of arguments.
    if (expected.m_arguments.empty()) {
      // we expect zero arguments, but got some. The actual type is more specific, so this is fine.
    } else {
      // different sizes, and we expected arguments. No good!
      success = false;
    }
  }

  if (!success) {
    if (print_on_error) {
      if (error_source_name.empty()) {
        fmt::print("[TypeSystem] Got type \"{}\" when expecting \"{}\"\n", actual.print(),
                   expected.print());
      } else {
        fmt::print("[TypeSystem] For {}, got type \"{}\" when expecting \"{}\"\n",
                   error_source_name, actual.print(), expected.print());
      }
    }

    if (throw_on_error) {
      throw std::runtime_error("typecheck failed");
    }
  }

  return success;
}

/*!
 * Is actual of type expected? For base types.
 */
bool TypeSystem::typecheck_base_types(const std::string& expected,
                                      const std::string& actual) const {
  // just to make sure it exists. (note - could there be a case when it just has to be forward
  // declared, but not defined?)
  lookup_type(expected);

  if (expected == actual || expected == lookup_type_allow_partial_def(actual)->get_name()) {
    lookup_type_allow_partial_def(actual);  // make sure it exists
    return true;
  }

  std::string actual_name = actual;
  auto actual_type = lookup_type_allow_partial_def(actual_name);
  while (actual_type->has_parent()) {
    actual_name = actual_type->get_parent();
    actual_type = lookup_type_allow_partial_def(actual_name);

    if (expected == actual_name) {
      return true;
    }
  }

  return false;
}

EnumType* TypeSystem::try_enum_lookup(const std::string& type_name) const {
  auto it = m_types.find(type_name);
  if (it != m_types.end()) {
    return dynamic_cast<EnumType*>(it->second.get());
  }
  return nullptr;
}

EnumType* TypeSystem::try_enum_lookup(const TypeSpec& type) const {
  return try_enum_lookup(type.base_type());
}

/*!
 * Get a path from type to object.
 */
std::vector<std::string> TypeSystem::get_path_up_tree(const std::string& type) const {
  auto parent = lookup_type(type)->get_parent();
  std::vector<std::string> path = {type};
  path.push_back(parent);
  auto parent_type = lookup_type(parent);

  while (parent_type->has_parent()) {
    parent = parent_type->get_parent();
    parent_type = lookup_type(parent);
    path.push_back(parent);
  }

  return path;
}

/*!
 * Lowest common ancestor of two base types.
 */
std::string TypeSystem::lca_base(const std::string& a, const std::string& b) const {
  if (a == b) {
    return a;
  }

  if (a == "none" || b == "none") {
    return "none";
  }

  auto a_up = get_path_up_tree(a);
  auto b_up = get_path_up_tree(b);

  int ai = a_up.size() - 1;
  int bi = b_up.size() - 1;

  std::string* result = nullptr;
  while (ai >= 0 && bi >= 0) {
    if (a_up.at(ai) == b_up.at(bi)) {
      result = &a_up.at(ai);
    } else {
      break;
    }
    ai--;
    bi--;
  }

  assert(result);
  return *result;
}

/*!
 * Lowest common ancestor of two typespecs.  Will recursively apply to arguments, if compatible.
 * Otherwise arguments are stripped off.
 * In a situation like lca("(a b)", "(c d)"), the result will be
 * (lca(a, b) lca(b, d)).
 */
TypeSpec TypeSystem::lowest_common_ancestor(const TypeSpec& a, const TypeSpec& b) const {
  auto result = make_typespec(lca_base(a.base_type(), b.base_type()));
  if (result == TypeSpec("function") && a.m_arguments.size() == 2 && b.m_arguments.size() == 2 &&
      (a.m_arguments.at(0) == TypeSpec("_varargs_") ||
       b.m_arguments.at(0) == TypeSpec("_varargs_"))) {
    return TypeSpec("function");
  }
  if (!a.m_arguments.empty() && !b.m_arguments.empty() &&
      a.m_arguments.size() == b.m_arguments.size()) {
    // recursively add arguments
    for (size_t i = 0; i < a.m_arguments.size(); i++) {
      result.add_arg(lowest_common_ancestor(a.m_arguments.at(i), b.m_arguments.at(i)));
    }
  }
  return result;
}

TypeSpec TypeSystem::lowest_common_ancestor_reg(const TypeSpec& a, const TypeSpec& b) const {
  return coerce_to_reg_type(lowest_common_ancestor(a, b));
}

/*!
 * Lowest common ancestor of multiple (or at least one) type.
 */
TypeSpec TypeSystem::lowest_common_ancestor(const std::vector<TypeSpec>& types) const {
  assert(!types.empty());
  if (types.size() == 1) {
    return types.front();
  }

  auto result = lowest_common_ancestor(types.at(0), types.at(1));
  for (size_t i = 2; i < types.size(); i++) {
    result = lowest_common_ancestor(result, types.at(i));
  }
  return result;
}

/*!
 * Converts a type in memory to the type you'll get in a register after loading it.
 */
TypeSpec coerce_to_reg_type(const TypeSpec& in) {
  if (in.arg_count() == 0) {
    if (in.base_type() == "int8" || in.base_type() == "int16" || in.base_type() == "int32" ||
        in.base_type() == "int64" || in.base_type() == "integer") {
      return TypeSpec("int");
    }

    if (in.base_type() == "uint8" || in.base_type() == "uint16" || in.base_type() == "uint32" ||
        in.base_type() == "uint64" || in.base_type() == "uinteger") {
      return TypeSpec("uint");
    }
  }

  return in;
}

/*!
 * Is the given type a bitfield type?
 */
bool TypeSystem::is_bitfield_type(const std::string& type_name) const {
  return dynamic_cast<BitFieldType*>(lookup_type(type_name));
}

/*!
 * Get information about a field within a bitfield type.
 */
BitfieldLookupInfo TypeSystem::lookup_bitfield_info(const std::string& type_name,
                                                    const std::string& field_name) const {
  auto type = get_type_of_type<BitFieldType>(type_name);
  BitField f;
  if (!type->lookup_field(field_name, &f)) {
    throw_typesystem_error("Type {} has no bitfield named {}\n", type_name, field_name);
  }

  BitfieldLookupInfo result;
  result.result_type = f.type();
  result.offset = f.offset();
  result.sign_extend = lookup_type(result.result_type)->get_load_signed();
  result.size = f.size();
  return result;
}

/*!
 * Add a new field to a bitfield type.
 * Set the field size to -1 if you want to just use the size of the type and not clip it.
 */
void TypeSystem::add_field_to_bitfield(BitFieldType* type,
                                       const std::string& field_name,
                                       const TypeSpec& field_type,
                                       int offset,
                                       int field_size) {
  // in bits
  auto load_size = lookup_type(field_type)->get_load_size() * 8;
  if (field_size == -1) {
    field_size = load_size;
  }

  if (field_size > load_size) {
    throw_typesystem_error(
        "Type {}'s bitfield {}'s set size is {}, which is larger than the actual "
        "type: {}\n",
        type->get_name(), field_name, field_size, load_size);
  }

  if (field_size + offset > type->get_load_size() * 8) {
    throw_typesystem_error(
        "Type {}'s bitfield {} will run off the end of the type (ends at {} bits, "
        "type is {} bits)\n",
        type->get_name(), field_name, field_size + offset, type->get_load_size() * 8);
  }

  // 128-bit bitfields have the limitation that fields cannot cross the 64-bit boundary.
  if (offset < 64 && offset + field_size > 64) {
    throw_typesystem_error(
        "Type {}'s bitfield {} will cross bit 64, which is not permitted. Range [{}, {})",
        type->get_name(), field_name, offset, offset + field_size);
  }

  BitField field(field_type, field_name, offset, field_size);
  type->m_fields.push_back(field);
}

/*!
 * Generate the part of a deftype for the flag asserts and methods.
 * Doesn't include the final close paren of the deftype
 * This should work for both structure/bitfield definitions.
 */
std::string TypeSystem::generate_deftype_footer(const Type* type) const {
  std::string result;

  auto as_structure = dynamic_cast<const StructureType*>(type);
  if (as_structure) {
    if (as_structure->is_packed()) {
      result.append("  :pack-me\n");
    }
    if (as_structure->is_allowed_misalign()) {
      result.append("  :allow-misaligned");
    }
  }

  if (type->heap_base()) {
    result.append(fmt::format("  :heap-base #x{:x}\n", type->heap_base()));
  }

  auto method_count = get_next_method_id(type);
  result.append(fmt::format("  :method-count-assert {}\n", get_next_method_id(type)));
  result.append(fmt::format("  :size-assert         #x{:x}\n", type->get_size_in_memory()));
  TypeFlags flags;
  flags.heap_base = type->heap_base();
  flags.size = type->get_size_in_memory();
  flags.pad = 0;
  flags.methods = method_count;

  result.append(fmt::format("  :flag-assert         #x{:x}\n  ", flags.flag));

  std::string methods_string;
  auto new_info = type->get_new_method_defined_for_type();
  if (new_info) {
    methods_string.append("(new (");
    for (size_t i = 0; i < new_info->type.arg_count() - 1; i++) {
      methods_string.append(new_info->type.get_arg(i).print());
      if (i != new_info->type.arg_count() - 2) {
        methods_string.push_back(' ');
      }
    }
    methods_string.append(fmt::format(
        ") {} {})\n    ", new_info->type.get_arg(new_info->type.arg_count() - 1).print(), 0));
  }

  for (auto& info : type->get_methods_defined_for_type()) {
    methods_string.append(fmt::format("({} (", info.name));
    for (size_t i = 0; i < info.type.arg_count() - 1; i++) {
      methods_string.append(info.type.get_arg(i).print());
      if (i != info.type.arg_count() - 2) {
        methods_string.push_back(' ');
      }
    }
    methods_string.append(fmt::format(
        ") {} {})\n    ", info.type.get_arg(info.type.arg_count() - 1).print(), info.id));
  }

  if (!methods_string.empty()) {
    result.append("(:methods\n    ");
    result.append(methods_string);
    result.append(")\n  ");
  }

  result.append(")\n");
  return result;
}

std::string TypeSystem::generate_deftype_for_structure(const StructureType* st) const {
  std::string result;
  result += fmt::format("(deftype {} ({})\n  (", st->get_name(), st->get_parent());

  int longest_field_name = 0;
  int longest_type_name = 0;
  int longest_mods = 0;

  const std::string inline_string = ":inline";
  const std::string dynamic_string = ":dynamic";
  const std::string user_offset_string = ":offset xxx";

  for (size_t i = st->first_unique_field_idx(); i < st->fields().size(); i++) {
    const auto& field = st->fields().at(i);
    longest_field_name = std::max(longest_field_name, int(field.name().size()));
    longest_type_name = std::max(longest_type_name, int(field.type().print().size()));

    int mods = 0;
    // mods are array size, :inline, :dynamic
    if (field.is_array() && !field.is_dynamic()) {
      mods += std::to_string(field.array_size()).size();
    }

    if (field.is_inline()) {
      if (mods) {
        mods++;  // space
      }
      mods += inline_string.size();
    }

    if (field.is_dynamic()) {
      if (mods) {
        mods++;  // space
      }
      mods += dynamic_string.size();
    }

    if (field.user_placed()) {
      if (mods) {
        mods++;
      }
      mods += user_offset_string.size();
    }
    longest_mods = std::max(longest_mods, mods);
  }

  for (size_t i = st->first_unique_field_idx(); i < st->fields().size(); i++) {
    const auto& field = st->fields().at(i);
    result += "(";
    result += field.name();
    result.append(1 + (longest_field_name - int(field.name().size())), ' ');
    result += field.type().print();
    result.append(1 + (longest_type_name - int(field.type().print().size())), ' ');

    std::string mods;
    if (field.is_array() && !field.is_dynamic()) {
      mods += std::to_string(field.array_size());
      mods += " ";
    }

    if (field.is_inline()) {
      mods += inline_string;
      mods += " ";
    }

    if (field.is_dynamic()) {
      mods += dynamic_string;
      mods += " ";
    }

    if (field.user_placed()) {
      mods += fmt::format(":offset {:3d}", field.offset());
    }
    result.append(mods);

    if (!field.user_placed()) {
      result.append(longest_mods - int(mods.size() - 1), ' ');
      result.append(":offset-assert ");
      result.append(std::to_string(field.offset()));
    }

    result.append(")\n   ");
  }

  result.append(")\n");
  result.append(generate_deftype_footer(st));

  return result;
}

std::string TypeSystem::generate_deftype_for_bitfield(const BitFieldType* type) const {
  std::string result;
  result += fmt::format("(deftype {} ({})\n  (", type->get_name(), type->get_parent());

  int longest_field_name = 0;
  int longest_type_name = 0;

  for (const auto& field : type->fields()) {
    longest_field_name = std::max(longest_field_name, int(field.name().size()));
    longest_type_name = std::max(longest_type_name, int(field.type().print().size()));
  }

  for (const auto& field : type->fields()) {
    result += "(";
    result += field.name();
    result.append(1 + (longest_field_name - int(field.name().size())), ' ');
    result += field.type().print();
    result.append(1 + (longest_type_name - int(field.type().print().size())), ' ');

    result.append(fmt::format(":offset {:3d} :size {:3d}", field.offset(), field.size()));
    result.append(")\n   ");
  }

  result.append(")\n");
  result.append(generate_deftype_footer(type));

  return result;
}

std::string TypeSystem::generate_deftype(const Type* type) const {
  std::string result;

  auto st = dynamic_cast<const StructureType*>(type);
  if (st) {
    return generate_deftype_for_structure(st);
  }

  auto bf = dynamic_cast<const BitFieldType*>(type);
  if (bf) {
    return generate_deftype_for_bitfield(bf);
  }

  return fmt::format(
      ";; cannot generate deftype for {}, it is not a structure, basic, or bitfield (parent {})\n",
      type->get_name(), type->get_parent());
}

bool TypeSystem::should_use_virtual_methods(const Type* type, int method_id) const {
  auto as_basic = dynamic_cast<const BasicType*>(type);
  if (as_basic && !as_basic->final() && !lookup_method(type->get_name(), method_id).no_virtual) {
    return true;
  } else {
    return false;
  }
}

bool TypeSystem::should_use_virtual_methods(const TypeSpec& type, int method_id) const {
  return should_use_virtual_methods(lookup_type(type), method_id);
}
