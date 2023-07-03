/*!
 * @file Type.cpp
 * Representation of a GOAL type in the type system.
 */

#include "Type.h"

#include <stdexcept>

#include "common/log/log.h"
#include "common/util/Assert.h"

#include "third-party/fmt/core.h"

namespace {
std::string reg_kind_to_string(RegClass kind) {
  switch (kind) {
    case RegClass::GPR_64:
      return "gpr64";
    case RegClass::INT_128:
      return "int128";
    case RegClass::FLOAT:
      return "float";
    case RegClass::VECTOR_FLOAT:
      return "float-4x";
    default:
      throw std::runtime_error("Unsupported HWRegKind");
  }
}

}  // namespace

/*!
 * Compare method for equality. This is used to determine if a new method declaration would
 * modify the existing type information.
 */
bool MethodInfo::operator==(const MethodInfo& other) const {
  return id == other.id && name == other.name && type == other.type &&
         defined_in_type == other.defined_in_type && other.no_virtual == no_virtual &&
         other.overrides_parent == overrides_parent &&
         only_overrides_docstring == other.only_overrides_docstring;
}

std::string MethodInfo::diff(const MethodInfo& other) const {
  std::string result;
  if (id != other.id) {
    result += fmt::format("id: {} vs. {}\n", id, other.id);
  }

  if (name != other.name) {
    result += fmt::format("name: {} vs. {}\n", name, other.name);
  }

  if (type != other.type) {
    result += fmt::format("type: {} vs. {}\n", type.print(), other.type.print());
  }

  if (defined_in_type != other.defined_in_type) {
    result += fmt::format("defined_in_type: {} vs. {}\n", defined_in_type, other.defined_in_type);
  }

  if (no_virtual != other.no_virtual) {
    result += fmt::format("no_virtual: {} vs. {}\n", no_virtual, other.no_virtual);
  }

  if (overrides_parent != other.overrides_parent) {
    result +=
        fmt::format("overrides_parent: {} vs. {}\n", overrides_parent, other.overrides_parent);
  }

  if (only_overrides_docstring != other.only_overrides_docstring) {
    result += fmt::format("only_overrides_docstring: {} vs. {}\n", only_overrides_docstring,
                          other.only_overrides_docstring);
  }

  return result;
}

/*!
 * Print a one-line description of a method (name and type)
 */
std::string MethodInfo::print_one_line() const {
  return fmt::format("Method {:3d}: {:20} {}", id, name, type.print());
}

Field::Field(std::string name, TypeSpec ts) : m_name(std::move(name)), m_type(std::move(ts)) {}
Field::Field(std::string name, TypeSpec ts, int offset)
    : m_name(std::move(name)), m_type(std::move(ts)), m_offset(offset) {}

/*!
 * Print a one line description of a field.
 */
std::string Field::print() const {
  return fmt::format(
      "Field: ({} {} :offset {}) inline: {:5}, dynamic: {:5}, array: {:5}, array size {:3}, align "
      "{:2}, skip {}",
      m_name, m_type.print(), m_offset, m_inline, m_dynamic, m_array, m_array_size, m_alignment,
      m_skip_in_static_decomp);
}

/*!
 * Mark a field as dynamic, indicating it has array semantics.
 */
void Field::set_dynamic() {
  m_dynamic = true;
  m_array = true;
}

/*!
 * Mark a field as a fixed size array.
 */
void Field::set_array(int size) {
  m_array_size = size;
  m_array = true;
}

/*!
 * Mark a field as inline. This has a different meaning depending on if the field is also an array
 */
void Field::set_inline() {
  m_inline = true;
}

/*!
 * Compare field definitions for equality. Used to determine if redefinition would change the type.
 */
bool Field::operator==(const Field& other) const {
  // we don't check the score and the do-not-decompile here.
  // clang-format off
  return m_name == other.m_name &&
         m_type == other.m_type &&
         m_offset == other.m_offset &&
         m_inline == other.m_inline &&
         m_dynamic == other.m_dynamic &&
         m_array == other.m_array &&
         m_array_size == other.m_array_size &&
         m_alignment == other.m_alignment;
  // clang-format on
}

std::string Field::diff(const Field& other) const {
  std::string result;
  if (m_name != other.m_name) {
    result += fmt::format("name: {} vs. {}\n", m_name, other.m_name);
  }

  if (m_type != other.m_type) {
    result += fmt::format("type: {} vs. {}\n", m_type.print(), other.m_type.print());
  }

  if (m_offset != other.m_offset) {
    result += fmt::format("offset: {} vs. {}\n", m_offset, other.m_offset);
  }

  if (m_inline != other.m_inline) {
    result += fmt::format("inline: {} vs. {}\n", m_inline, other.m_inline);
  }

  if (m_dynamic != other.m_dynamic) {
    result += fmt::format("dynamic: {} vs. {}\n", m_dynamic, other.m_dynamic);
  }

  if (m_array != other.m_array) {
    result += fmt::format("array: {} vs. {}\n", m_array, other.m_array);
  }

  if (m_array_size != other.m_array_size) {
    result += fmt::format("array_size: {} vs. {}\n", m_array_size, other.m_array_size);
  }

  if (m_alignment != other.m_alignment) {
    result += fmt::format("alignment: {} vs. {}\n", m_alignment, other.m_alignment);
  }

  if (m_skip_in_static_decomp != other.m_skip_in_static_decomp) {
    result += fmt::format("skip_in_static_decomp: {} vs. {}\n", m_skip_in_static_decomp,
                          other.m_skip_in_static_decomp);
  }

  return result;
}

/////////////
// Type
/////////////

// parent class of types, also has method logic.

Type::Type(std::string parent, std::string name, bool is_boxed, int heap_base)
    : m_parent(std::move(parent)),
      m_name(std::move(name)),
      m_is_boxed(is_boxed),
      m_heap_base(heap_base) {
  m_runtime_name = m_name;
}

/*!
 * Get the name of a type. This should be a unique identifier that can be used to find this
 * type from a TypeSystem.
 */
std::string Type::get_name() const {
  return m_name;
}

std::string Type::get_runtime_name() const {
  if (!m_allow_in_runtime) {
    lg::print("[TypeSystem] Tried to use type {} as a runtime type, which is not allowed.\n",
              get_name());
    throw std::runtime_error("get_runtime_name");
  }
  return m_runtime_name;
}

void Type::set_runtime_type(std::string name) {
  m_runtime_name = std::move(name);
}

/*!
 * Get the parent type's name.
 */
std::string Type::get_parent() const {
  return m_parent;
}

/*!
 * Compare the name/parent/method info for equality.  The more complete operator== should be used
 * to check if two types are really identical.
 */
bool Type::common_type_info_equal(const Type& other) const {
  // Check if methods only differ because of documentation overrides
  bool methods_the_same = true;
  for (const auto& method : m_methods) {
    if (method.only_overrides_docstring) {
      // skip these methods, as we _expect_ to not find them in one or the other!
      // this is a all-types vs normal code wrinkle
      continue;
    }
    // For each method, find it's matching id, it should only be allowed to be different
    // if its just the docstring
    bool found_method = false;
    for (const auto& _method : other.m_methods) {
      if (method.id == _method.id) {
        if (method == _method) {
          found_method = true;
          break;
        } else {
          methods_the_same = false;
          break;
        }
      }
    }
    if (!methods_the_same || !found_method) {
      methods_the_same = false;
      break;
    }
  }

  // clang-format off
  return methods_the_same &&
         m_states == other.m_states &&
         m_new_method_info == other.m_new_method_info &&
         m_new_method_info_defined == other.m_new_method_info_defined &&
         m_parent == other.m_parent &&
         m_name == other.m_name &&
         m_allow_in_runtime == other.m_allow_in_runtime &&
         m_runtime_name == other.m_runtime_name &&
         m_is_boxed == other.m_is_boxed &&
         m_generate_inspect == other.m_generate_inspect &&
         m_heap_base == other.m_heap_base;
  // clang-format on
}

std::string Type::common_type_info_diff(const Type& other) const {
  std::string result;
  if (m_methods != other.m_methods) {
    if (m_methods.size() != other.m_methods.size()) {
      result += fmt::format("Number of additional methods {} vs. {}\n", m_methods.size(),
                            other.m_methods.size());
    }

    for (size_t i = 0; i < std::min(m_methods.size(), other.m_methods.size()); i++) {
      if (m_methods.at(i) != other.m_methods.at(i)) {
        result += fmt::format("Method def {} ({}/{}):\n", i, m_methods.at(i).name,
                              other.m_methods.at(i).name);
        result += m_methods.at(i).diff(other.m_methods.at(i));
        result += "\n";
      }
    }
  }
  if (m_states != other.m_states) {
    result += "States are different:\n";
    for (auto& ours : m_states) {
      auto theirs = other.m_states.find(ours.first);
      if (theirs == other.m_states.end()) {
        result += fmt::format("  {} is in one, but not the other.\n", ours.first);
      } else if (ours.second != theirs->second) {
        result += fmt::format("  {} is defined differently: {} vs {}\n", ours.first,
                              ours.second.print(), theirs->second.print());
      }
    }

    for (auto& theirs : other.m_states) {
      auto ours = m_states.find(theirs.first);
      if (ours == m_states.end()) {
        result += fmt::format("  {} is in one, but not the other.\n", theirs.first);
      }
    }
  }
  if (m_new_method_info != other.m_new_method_info) {
    result += m_new_method_info.diff(other.m_new_method_info);
  }
  if (m_new_method_info_defined != other.m_new_method_info_defined) {
    result += fmt::format("new_method_info_defined: {} vs. {}\n", m_new_method_info_defined,
                          other.m_new_method_info_defined);
  }
  if (m_parent != other.m_parent) {
    result += fmt::format("parent: {} vs. {}\n", m_parent, other.m_parent);
  }
  if (m_name != other.m_name) {
    result += fmt::format("name: {} vs. {}\n", m_name, other.m_name);
  }
  if (m_allow_in_runtime != other.m_allow_in_runtime) {
    result +=
        fmt::format("allow_in_runtime: {} vs. {}\n", m_allow_in_runtime, other.m_allow_in_runtime);
  }
  if (m_runtime_name != other.m_runtime_name) {
    result += fmt::format("runtime_name: {} vs. {}\n", m_runtime_name, other.m_runtime_name);
  }
  if (m_is_boxed != other.m_is_boxed) {
    result += fmt::format("is_boxed: {} vs. {}\n", m_is_boxed, other.m_is_boxed);
  }
  if (m_heap_base != other.m_heap_base) {
    result += fmt::format("heap_base: {} vs. {}\n", m_heap_base, other.m_heap_base);
  }
  if (m_generate_inspect != other.m_generate_inspect) {
    result +=
        fmt::format("generate_inspect: {} vs. {}\n", m_generate_inspect, other.m_generate_inspect);
  }
  return result;
}

/*!
 * Does this type have a parent that makes sense to use? Object and none both don't have meaningful
 * parents.
 */
bool Type::has_parent() const {
  return m_name != "object" && !m_parent.empty();
}

/*!
 * Get a method that is defined specifically for this type. Returns if it was found or not.
 */
bool Type::get_my_method(const std::string& name, MethodInfo* out) const {
  for (auto& x : m_methods) {
    if (x.name == name) {
      *out = x;
      return true;
    }
  }

  return false;
}

/*!
 * Get a method that is defined specifically in this type by id. Returns if it was found or not.
 */
bool Type::get_my_method(int id, MethodInfo* out) const {
  ASSERT(id > 0);  // 0 is new, should use explicit new method functions instead.
  for (auto& x : m_methods) {
    if (x.id == id) {
      *out = x;
      return true;
    }
  }

  return false;
}

/*!
 * Get the last method defined specifically for this type. Returns if there were any methods
 * defined specifically for this type or not.
 */
bool Type::get_my_last_method(MethodInfo* out) const {
  for (auto it = m_methods.rbegin(); it != m_methods.rend(); it++) {
    if (!it->overrides_parent && !it->only_overrides_docstring) {
      *out = *it;
      return true;
    }
  }
  return false;
}

/*!
 * Get the new method defined specifically for this type. Returns if there is a new method specific
 * to this type or not.
 */
bool Type::get_my_new_method(MethodInfo* out) const {
  if (m_new_method_info_defined) {
    *out = m_new_method_info;
    return true;
  }
  return false;
}

/*!
 * Get the number of legitimate methods / overridden methods.  Ignore that which are inherited just
 * for documentation overrides
 */
int Type::get_num_methods() const {
  int num = 0;
  for (auto it = m_methods.rbegin(); it != m_methods.rend(); it++) {
    if (it->only_overrides_docstring) {
      continue;
    }
    num++;
  }
  return num;
}

/*!
 * Add a method defined specifically for this type.
 */
const MethodInfo& Type::add_method(const MethodInfo& info) {
  for (auto it = m_methods.rbegin(); it != m_methods.rend(); it++) {
    if (!it->overrides_parent && !it->only_overrides_docstring) {
      ASSERT(it->id + 1 == info.id);
      break;
    }
  }

  m_methods.push_back(info);
  return m_methods.back();
}

/*!
 * Add a NEW method defined specifically for this type. The name of this function is confusing -
 * this is specific to the method named NEW.
 */
const MethodInfo& Type::add_new_method(const MethodInfo& info) {
  ASSERT(info.name == "new");
  m_new_method_info_defined = true;
  m_new_method_info = info;
  return m_new_method_info;
}

/*!
 * Print information for all methods defined specifically for a type.
 * Does not print inherited methods.
 */
std::string Type::print_method_info() const {
  std::string result;
  if (m_new_method_info_defined) {
    result += "  " + m_new_method_info.print_one_line() + "\n";
  }

  for (const auto& x : m_methods) {
    result += "  " + x.print_one_line() + "\n";
  }

  return result;
}

void Type::add_state(const std::string& name, const TypeSpec& type) {
  if (!m_states.insert({name, type}).second) {
    throw std::runtime_error(fmt::format("State {} is already defined in type", name));
  }
}

std::string Type::incompatible_diff(const Type& other) const {
  return fmt::format("diff is not implemented between {} and {}\n", typeid((*this)).name(),
                     typeid(other).name());
}

std::string Type::diff(const Type& other) const {
  std::string result;
  result += common_type_info_diff(other);
  result += diff_impl(other);
  return result;
}

/////////////
// NullType
/////////////

// Special Type for both "none" and "_type_" types
// it's an error to try to do anything with Null.

NullType::NullType(std::string name) : Type("", std::move(name), false, 0) {}

bool NullType::is_reference() const {
  throw std::runtime_error("is_reference called on NullType");
}

int NullType::get_load_size() const {
  throw std::runtime_error("get_load_size called on NullType");
}

bool NullType::get_load_signed() const {
  throw std::runtime_error("get_load_size called on NullType");
}

int NullType::get_size_in_memory() const {
  throw std::runtime_error("get_size_in_memory called on NullType");
}

RegClass NullType::get_preferred_reg_class() const {
  throw std::runtime_error("get_preferred_reg_class called on NullType");
}

int NullType::get_offset() const {
  throw std::runtime_error("get_offset called on NoneType");
}

int NullType::get_in_memory_alignment() const {
  throw std::runtime_error("get_in_memory_alignment called on NullType");
}

int NullType::get_inline_array_start_alignment() const {
  throw std::runtime_error("get_inline_array_start_alignment called on NullType");
}

int NullType::get_inline_array_stride_alignment() const {
  throw std::runtime_error("get_inline_array_stride_alignment called on NullType");
}

std::string NullType::print() const {
  return m_name;
}

bool NullType::operator==(const Type& other) const {
  // any redefinition by the user should be invalid, so this will always return false unless
  // you're calling it on the same object.
  return this == &other;
}

std::string NullType::diff_impl(const Type& other) const {
  if ((*this) != other) {
    return "NullType error";
  } else {
    return "";
  }
}

/////////////
// ValueType
/////////////

// A value type is a type that fits entirely in a register, as opposed to types which stay in
// memory, and reference to them go in registers.

ValueType::ValueType(std::string parent,
                     std::string name,
                     bool is_boxed,
                     int size,
                     bool sign_extend,
                     RegClass reg)
    : Type(std::move(parent), std::move(name), is_boxed, 0),
      m_size(size),
      m_sign_extend(sign_extend),
      m_reg_kind(reg) {}

/*!
 * By definition, ValueType is not a reference.  Note that a type like (pointer x) is NOT a
 * reference type, as the pointer itself goes into memory. This can be confusing when compared to
 * C/C++.
 */
bool ValueType::is_reference() const {
  return false;
}

/*!
 * Get the size needed to load this value into a register.  Values fit entirely into a register,
 * so this is just the data size.
 */
int ValueType::get_load_size() const {
  return m_size;
}

/*!
 * The size in memory and size in register are the same.
 */
int ValueType::get_size_in_memory() const {
  return m_size;
}

/*!
 * The type of register that this value likes to be loaded into.
 */
RegClass ValueType::get_preferred_reg_class() const {
  return m_reg_kind;
}

/*!
 * Probably is zero always, but the pointer-offset trick for boxing could also be implemented for
 * value types, and it seems like a good idea to support this just in case.
 */
int ValueType::get_offset() const {
  return m_offset;
}

/*!
 * Should this value be sign extended when loading? This is required for signed integers.
 */
bool ValueType::get_load_signed() const {
  return m_sign_extend;
}

void ValueType::set_offset(int offset) {
  if (offset) {
    ASSERT(m_is_boxed);
  }
  m_offset = offset;
}

/*!
 * Alignment should be the size for MIPS.
 */
int ValueType::get_in_memory_alignment() const {
  return m_size;
}

int ValueType::get_inline_array_stride_alignment() const {
  return m_size;
}

int ValueType::get_inline_array_start_alignment() const {
  return m_size;
}

/*!
 * Inherit settings from a parent. User-defined types will pick the appropriate parent to get
 * the settings they want.
 */
void ValueType::inherit(const ValueType* parent) {
  m_sign_extend = parent->m_sign_extend;
  m_size = parent->m_size;
  m_offset = parent->m_offset;
  m_reg_kind = parent->m_reg_kind;
}

std::string ValueType::print() const {
  return fmt::format(
      "[ValueType] {}\n parent: {}\n boxed: {}\n size: {}\n sext: {}\n register: {}\n{}", m_name,
      m_parent, m_is_boxed, m_size, m_sign_extend, reg_kind_to_string(m_reg_kind),
      print_method_info());
}

/*!
 * Is other an identical ValueType?
 */
bool ValueType::operator==(const Type& other) const {
  if (typeid(ValueType) != typeid(other)) {
    return false;
  }

  auto* p_other = dynamic_cast<const ValueType*>(&other);
  // clang-format off
  return other.common_type_info_equal(*this) &&
         m_size == p_other->m_size &&
         m_sign_extend == p_other->m_sign_extend &&
         m_reg_kind == p_other->m_reg_kind &&
         m_offset == p_other->m_offset;
  // clang-format on
}

std::string ValueType::diff_impl(const Type& other_) const {
  if (typeid(ValueType) != typeid(other_)) {
    return Type::incompatible_diff(other_);
  }

  std::string result;
  auto& other = *dynamic_cast<const ValueType*>(&other_);

  if (m_size != other.m_size) {
    result += fmt::format("size: {} vs. {}\n", m_size, other.m_size);
  }
  if (m_offset != other.m_offset) {
    result += fmt::format("offset: {} vs. {}\n", m_offset, other.m_offset);
  }
  if (m_sign_extend != other.m_sign_extend) {
    result += fmt::format("sign_extend: {} vs. {}\n", m_sign_extend, other.m_sign_extend);
  }
  if (m_reg_kind != other.m_reg_kind) {
    result += fmt::format("reg_kind: {} vs. {}\n", (int)m_reg_kind, (int)other.m_reg_kind);
  }
  return result;
}

/////////////////
// ReferenceType
/////////////////

// ReferenceType is an abstract class that's a parent for everything that uses reference semantics.
// This means this type behaves like a C pointer - the thing that's passed around is a reference
// to some memory somewhere.

ReferenceType::ReferenceType(std::string parent, std::string name, bool is_boxed, int heap_base)
    : Type(std::move(parent), std::move(name), is_boxed, heap_base) {}

/*!
 * By definition, this is a reference!
 */
bool ReferenceType::is_reference() const {
  return true;
}

/*!
 * Pointers should not be sign extended
 */
bool ReferenceType::get_load_signed() const {
  return false;
}

/*!
 * Pointers are 4 bytes.
 */
int ReferenceType::get_load_size() const {
  return POINTER_SIZE;
}

/*!
 * Pointers go in GPRs
 */
RegClass ReferenceType::get_preferred_reg_class() const {
  return RegClass::GPR_64;
}

std::string ReferenceType::print() const {
  return fmt::format("[ReferenceType] {}\n parent: {}\n boxed: {}\n{}", m_name, m_parent,
                     m_is_boxed, print_method_info());
}

/////////////////
// StructureType
/////////////////

// StructureType is a ReferenceType which has fields.  It's also the parent of BasicType,
// which is a structure with runtime typing information.

StructureType::StructureType(std::string parent,
                             std::string name,
                             bool boxed,
                             bool dynamic,
                             bool pack,
                             int heap_base)
    : ReferenceType(std::move(parent), std::move(name), boxed, heap_base),
      m_dynamic(dynamic),
      m_pack(pack) {}

std::string StructureType::print() const {
  std::string result = fmt::format(
      "[StructureType] {}\n parent: {}\n boxed: {}\n dynamic: {}\n size: {}\n pack: {}\n misalign: "
      "{}\n heap-base: {}\n stack-singleton: {}\n fields:\n",
      m_name, m_parent, m_is_boxed, m_dynamic, m_size_in_mem, m_pack, m_allow_misalign, m_heap_base,
      m_always_stack_singleton);
  for (auto& x : m_fields) {
    result += "   " + x.print() + "\n";
  }
  result += " methods:\n" + print_method_info();
  return result;
}

void StructureType::inherit(StructureType* parent) {
  m_fields = parent->m_fields;
  m_dynamic = parent->m_dynamic;
  m_size_in_mem = parent->m_size_in_mem;
  m_idx_of_first_unique_field = m_fields.size();
}

bool StructureType::operator==(const Type& other) const {
  if (typeid(*this) != typeid(other)) {
    return false;
  }

  auto* p_other = dynamic_cast<const StructureType*>(&other);
  // clang-format off
  return other.common_type_info_equal(*this) &&
         m_fields == p_other->m_fields &&
         m_dynamic == p_other->m_dynamic &&
         m_size_in_mem == p_other->m_size_in_mem &&
         m_pack == p_other->m_pack &&
         m_allow_misalign == p_other->m_allow_misalign &&
         m_offset == p_other->m_offset &&
         m_idx_of_first_unique_field == p_other->m_idx_of_first_unique_field &&
         m_always_stack_singleton == p_other->m_always_stack_singleton;
  // clang-format on
}

std::string StructureType::diff_impl(const Type& other_) const {
  if (typeid(*this) != typeid(other_)) {
    return incompatible_diff(other_);
  }

  auto& other = *dynamic_cast<const StructureType*>(&other_);
  return diff_structure_common(other);
}

std::string StructureType::diff_structure_common(const StructureType& other) const {
  std::string result;
  if (m_fields != other.m_fields) {
    if (m_fields.size() != other.m_fields.size()) {
      result += fmt::format("Number of fields {} vs. {}\n", m_fields.size(), other.m_fields.size());
    }

    for (size_t i = 0; i < std::min(m_fields.size(), other.m_fields.size()); i++) {
      if (m_fields.at(i) != other.m_fields.at(i)) {
        result += fmt::format("field {} ({}/{}):\n", i, m_fields.at(i).name(),
                              other.m_fields.at(i).name());
        result += m_fields.at(i).diff(other.m_fields.at(i));
        result += "\n";
      }
    }
  }

  if (m_dynamic != other.m_dynamic) {
    result += fmt::format("dynamic: {} vs. {}\n", m_dynamic, other.m_dynamic);
  }

  if (m_size_in_mem != other.m_size_in_mem) {
    result += fmt::format("size_in_mem: {} vs. {}\n", m_size_in_mem, other.m_size_in_mem);
  }

  if (m_pack != other.m_pack) {
    result += fmt::format("pack: {} vs. {}\n", m_pack, other.m_pack);
  }

  if (m_allow_misalign != other.m_allow_misalign) {
    result += fmt::format("allow_misalign: {} vs. {}\n", m_allow_misalign, other.m_allow_misalign);
  }

  if (m_always_stack_singleton != other.m_always_stack_singleton) {
    result += fmt::format("always_stack_singleton: {} vs. {}\n", m_always_stack_singleton,
                          other.m_always_stack_singleton);
  }

  if (m_offset != other.m_offset) {
    result += fmt::format("offset: {} vs. {}\n", m_offset, other.m_offset);
  }

  if (m_idx_of_first_unique_field != other.m_idx_of_first_unique_field) {
    result += fmt::format("idx_of_first_unique_field: {} vs. {}\n", m_idx_of_first_unique_field,
                          other.m_idx_of_first_unique_field);
  }

  return result;
}

int StructureType::get_size_in_memory() const {
  return m_size_in_mem;
}

void StructureType::override_size_in_memory(int size) {
  m_size_in_mem = size;
}

int StructureType::get_offset() const {
  return m_offset;
}

int StructureType::get_in_memory_alignment() const {
  return STRUCTURE_ALIGNMENT;
}

// So the GOAL compiler was weird here.
// It seems like there were two states:
// - don't care about alignment of both the first element and the later
// - don't care about the alignment, but pad the stride.
// so you end up with a misaligned array of padded structures which seems very stupid.

int StructureType::get_inline_array_stride_alignment() const {
  if (m_pack) {
    // make elements of inline array the minimum allowable alignment.
    int alignment = 1;
    // TODO - I don't know if GOAL actually did this check, maybe packed inline arrays could
    // violate these?
    for (const auto& field : m_fields) {
      alignment = std::max(alignment, field.alignment());
    }
    return alignment;
  } else {
    // make elements of inline array properly aligned structures
    return STRUCTURE_ALIGNMENT;
  }
}

int StructureType::get_inline_array_start_alignment() const {
  if (m_pack || m_allow_misalign) {
    // make elements of inline array the minimum allowable alignment.
    int alignment = 1;
    // TODO - I don't know if GOAL actually did this check, maybe packed inline arrays could
    // violate these?
    for (const auto& field : m_fields) {
      alignment = std::max(alignment, field.alignment());
    }
    return alignment;
  } else {
    // make elements of inline array properly aligned structures
    return STRUCTURE_ALIGNMENT;
  }
}

bool StructureType::lookup_field(const std::string& name, Field* out) {
  for (auto& x : m_fields) {
    if (x.name() == name) {
      if (out) {
        *out = x;
      }

      return true;
    }
  }

  return false;
}

void StructureType::override_field_type(const std::string& field_name, const TypeSpec& new_type) {
  int i = 0;
  for (auto& x : m_fields) {
    if (x.name() == field_name) {
      x.set_override_type(new_type);
      m_overriden_fields.push_back(i);
    }
    ++i;
  }
}

/////////////////
// BasicType
/////////////////

BasicType::BasicType(std::string parent, std::string name, bool dynamic, int heap_base)
    : StructureType(std::move(parent), std::move(name), true, dynamic, false, heap_base) {}

std::string BasicType::print() const {
  std::string result = fmt::format(
      "[BasicType] {}\n parent: {}\n dynamic: {}\n size: {}\n heap-base: {}\n fields:\n", m_name,
      m_parent, m_dynamic, m_size_in_mem, m_heap_base);
  for (auto& x : m_fields) {
    result += "   " + x.print() + "\n";
  }
  result += " methods:\n" + print_method_info();
  return result;
}

int BasicType::get_offset() const {
  return BASIC_OFFSET;
}

int BasicType::get_inline_array_start_alignment() const {
  if (m_pack || m_allow_misalign) {
    // make elements of inline array the minimum allowable alignment.
    int alignment = m_allow_misalign ? 4 : 8;
    // TODO - I don't know if GOAL actually did this check, maybe packed inline arrays could
    // violate these?
    for (const auto& field : m_fields) {
      alignment = std::max(alignment, field.alignment());
    }
    return alignment;
  } else {
    // make elements of inline array properly aligned structures
    return STRUCTURE_ALIGNMENT;
  }
}

bool BasicType::operator==(const Type& other) const {
  if (typeid(*this) != typeid(other)) {
    return false;
  }

  auto* p_other = dynamic_cast<const BasicType*>(&other);
  // clang-format off
  return other.common_type_info_equal(*this) &&
         m_fields == p_other->m_fields &&
         m_dynamic == p_other->m_dynamic &&
         m_size_in_mem == p_other->m_size_in_mem &&
         m_pack == p_other->m_pack &&
         m_allow_misalign == p_other->m_allow_misalign &&
         m_offset == p_other->m_offset &&
         m_idx_of_first_unique_field == p_other->m_idx_of_first_unique_field &&
         m_final == p_other->m_final &&
         m_always_stack_singleton == p_other->m_always_stack_singleton;
  // clang-format on
}

std::string BasicType::diff_impl(const Type& other_) const {
  if (typeid(*this) != typeid(other_)) {
    return incompatible_diff(other_);
  }

  auto& other = *dynamic_cast<const BasicType*>(&other_);
  std::string result = diff_structure_common(other);

  if (m_final != other.m_final) {
    result += fmt::format("final: {} vs. {}\n", m_final, other.m_final);
  }

  return result;
}

/////////////////
// Bitfield
/////////////////

BitField::BitField(TypeSpec type, std::string name, int offset, int size, bool skip_in_decomp)
    : m_type(std::move(type)),
      m_name(std::move(name)),
      m_offset(offset),
      m_size(size),
      m_skip_in_static_decomp(skip_in_decomp) {}

bool BitField::operator==(const BitField& other) const {
  return m_type == other.m_type && m_name == other.m_name && m_offset == other.m_offset &&
         m_size == other.m_size;
}

std::string BitField::diff(const BitField& other) const {
  std::string result;

  if (m_type != other.m_type) {
    result += fmt::format("type: {} vs. {}\n", m_type.print(), other.m_type.print());
  }

  if (m_name != other.m_name) {
    result += fmt::format("name: {} vs. {}\n", m_name, other.m_name);
  }

  if (m_offset != other.m_offset) {
    result += fmt::format("offset: {} vs. {}\n", m_offset, other.m_offset);
  }

  if (m_size != other.m_size) {
    result += fmt::format("size: {} vs. {}\n", m_size, other.m_size);
  }

  if (m_skip_in_static_decomp != other.m_skip_in_static_decomp) {
    result += fmt::format("skip_in_static_decomp: {} vs. {}\n", m_skip_in_static_decomp,
                          other.m_skip_in_static_decomp);
  }

  return result;
}

BitFieldType::BitFieldType(std::string parent, std::string name, int size, bool sign_extend)
    : ValueType(std::move(parent), std::move(name), false, size, sign_extend, RegClass::GPR_64) {}

bool BitFieldType::lookup_field(const std::string& name, BitField* out) const {
  for (auto& field : m_fields) {
    if (field.name() == name) {
      if (out) {
        *out = field;
      }
      return true;
    }
  }
  return false;
}

std::string BitField::print() const {
  return fmt::format("[{} {}] sz {} off {}", name(), type().print(), size(), offset());
}

std::string BitFieldType::print() const {
  std::string result;
  result += fmt::format("Parent type: {}\nFields:\n", get_parent());
  for (auto& field : m_fields) {
    result += fmt::format("  {}\n", field.print());
  }
  result += fmt::format("Mem size: {}, load size: {}, signed {}, align {}\n", get_size_in_memory(),
                        get_load_size(), get_load_signed(), get_in_memory_alignment());
  return result;
}

bool BitFieldType::operator==(const Type& other) const {
  if (typeid(*this) != typeid(other)) {
    return false;
  }

  auto* p_other = dynamic_cast<const BitFieldType*>(&other);
  return other.common_type_info_equal(*this) && m_fields == p_other->m_fields;
}

std::string BitFieldType::diff_impl(const Type& other_) const {
  if (typeid(BitFieldType) != typeid(other_)) {
    return Type::incompatible_diff(other_);
  }

  std::string result;
  auto& other = *dynamic_cast<const BitFieldType*>(&other_);

  if (m_size != other.m_size) {
    result += fmt::format("size: {} vs. {}\n", m_size, other.m_size);
  }
  if (m_offset != other.m_offset) {
    result += fmt::format("offset: {} vs. {}\n", m_offset, other.m_offset);
  }
  if (m_sign_extend != other.m_sign_extend) {
    result += fmt::format("sign_extend: {} vs. {}\n", m_sign_extend, other.m_sign_extend);
  }
  if (m_reg_kind != other.m_reg_kind) {
    result += fmt::format("reg_kind: {} vs. {}\n", (int)m_reg_kind, (int)other.m_reg_kind);
  }

  if (m_fields != other.m_fields) {
    if (m_fields.size() != other.m_fields.size()) {
      result += fmt::format("Number of fields {} vs. {}\n", m_fields.size(), other.m_fields.size());
    }

    for (size_t i = 0; i < std::min(m_fields.size(), other.m_fields.size()); i++) {
      if (m_fields.at(i) != other.m_fields.at(i)) {
        result += fmt::format("field {} ({}/{}):\n", i, m_fields.at(i).name(),
                              other.m_fields.at(i).name());
        result += m_fields.at(i).diff(other.m_fields.at(i));
        result += "\n";
      }
    }
  }
  return result;
}

/////////////////
// Enum
/////////////////

EnumType::EnumType(const ValueType* parent,
                   std::string name,
                   bool is_bitfield,
                   const std::unordered_map<std::string, s64>& entries)
    : ValueType(parent->get_name(),
                std::move(name),
                parent->is_boxed(),
                parent->get_load_size(),
                parent->get_load_signed(),
                parent->get_preferred_reg_class()),
      m_is_bitfield(is_bitfield),
      m_entries(entries) {}

std::string EnumType::print() const {
  return fmt::format("Enum Type {}", m_name);
}

bool EnumType::operator==(const Type& other) const {
  if (typeid(*this) != typeid(other)) {
    return false;
  }

  auto* p_other = dynamic_cast<const EnumType*>(&other);
  return other.common_type_info_equal(*this) && (m_entries == p_other->m_entries) &&
         (m_is_bitfield == p_other->m_is_bitfield);
}

std::string EnumType::diff_impl(const Type& other_) const {
  if (typeid(EnumType) != typeid(other_)) {
    return Type::incompatible_diff(other_);
  }

  std::string result;
  auto& other = *dynamic_cast<const EnumType*>(&other_);

  if (m_is_bitfield != other.m_is_bitfield) {
    result += fmt::format("is_bitfield: {} vs. {}\n", m_is_bitfield, other.m_is_bitfield);
  }

  if (m_entries != other.m_entries) {
    result += "Entries are different:\n";
    for (auto& ours : m_entries) {
      auto theirs = other.m_entries.find(ours.first);
      if (theirs == other.m_entries.end()) {
        result += fmt::format("  {} is in one, but not the other.\n", ours.first);
      } else if (ours.second != theirs->second) {
        result += fmt::format("  {} is defined differently: {} vs {}\n", ours.first, ours.second,
                              theirs->second);
      }
    }

    for (auto& theirs : other.m_entries) {
      auto ours = m_entries.find(theirs.first);
      if (ours == m_entries.end()) {
        result += fmt::format("  {} is in one, but not the other.\n", theirs.first);
      }
    }
  }

  return result;
}
