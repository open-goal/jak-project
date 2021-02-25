/*!
 * @file Type.cpp
 * Representation of a GOAL type in the type system.
 */

#include <stdexcept>
#include <cassert>
#include <third-party/fmt/core.h>
#include "Type.h"

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
         defined_in_type == other.defined_in_type;
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
  // clang-format off
  return m_name == other.m_name &&
         m_type == other.m_type &&
         m_offset == other.m_offset &&
         m_inline == other.m_inline &&
         m_dynamic == other.m_dynamic &&
         m_array == other.m_array &&
         m_array_size == other.m_array_size &&
         m_alignment == other.m_alignment &&
         m_skip_in_static_decomp == other.m_skip_in_static_decomp;
  // clang-format on
}

/////////////
// Type
/////////////

// parent class of types, also has method logic.

Type::Type(std::string parent, std::string name, bool is_boxed)
    : m_parent(std::move(parent)), m_name(std::move(name)), m_is_boxed(is_boxed) {
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
    fmt::print("[TypeSystem] Tried to use type {} as a runtime type, which is not allowed.\n",
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
bool Type::is_equal(const Type& other) const {
  return m_parent == other.m_parent && m_name == other.m_name && m_is_boxed == other.m_is_boxed &&
         m_methods == other.m_methods && m_new_method_info == other.m_new_method_info &&
         m_new_method_info_defined == other.m_new_method_info_defined;
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
  assert(id > 0);  // 0 is new, should use explicit new method functions instead.
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
  if (!m_methods.empty()) {
    *out = m_methods.back();
    return true;
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
 * Add a method defined specifically for this type.
 */
const MethodInfo& Type::add_method(const MethodInfo& info) {
  if (!m_methods.empty()) {
    assert(m_methods.back().id + 1 == info.id);
  }
  m_methods.push_back(info);
  return m_methods.back();
}

/*!
 * Add a NEW method defined specifically for this type. The name of this function is confusing -
 * this is specific to the method named NEW.
 */
const MethodInfo& Type::add_new_method(const MethodInfo& info) {
  assert(info.name == "new");
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

/////////////
// NullType
/////////////

// Special Type for both "none" and "_type_" types
// it's an error to try to do anything with Null.

NullType::NullType(std::string name) : Type("", std::move(name), false) {}

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
    : Type(std::move(parent), std::move(name), is_boxed),
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
    assert(m_is_boxed);
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
  return other.is_equal(*this) &&
         m_size == p_other->m_size &&
         m_sign_extend == p_other->m_sign_extend &&
         m_reg_kind == p_other->m_reg_kind &&
         m_offset == p_other->m_offset;
  // clang-format on
}

/////////////////
// ReferenceType
/////////////////

// ReferenceType is an abstract class that's a parent for everything that uses reference semantics.
// This means this type behaves like a C pointer - the thing that's passed around is a reference
// to some memory somewhere.

ReferenceType::ReferenceType(std::string parent, std::string name, bool is_boxed)
    : Type(std::move(parent), std::move(name), is_boxed) {}

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
                             bool pack)
    : ReferenceType(std::move(parent), std::move(name), boxed), m_dynamic(dynamic), m_pack(pack) {}

std::string StructureType::print() const {
  std::string result = fmt::format(
      "[StructureType] {}\n parent: {}\n boxed: {}\n dynamic: {}\n size: {}\n pack: {}\n misalign: "
      "{}\n fields:\n",
      m_name, m_parent, m_is_boxed, m_dynamic, m_size_in_mem, m_pack, m_allow_misalign);
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
  return other.is_equal(*this) &&
         m_fields == p_other->m_fields &&
         m_dynamic == p_other->m_dynamic &&
         m_size_in_mem == p_other->m_size_in_mem &&
         m_pack == p_other->m_pack &&
         m_allow_misalign == p_other->m_allow_misalign;
  // clang-format on
}

bool BitFieldType::operator==(const Type& other) const {
  if (typeid(*this) != typeid(other)) {
    return false;
  }

  auto* p_other = dynamic_cast<const BitFieldType*>(&other);
  return other.is_equal(*this) && m_fields == p_other->m_fields;
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

/////////////////
// BasicType
/////////////////

BasicType::BasicType(std::string parent, std::string name, bool dynamic)
    : StructureType(std::move(parent), std::move(name), true, dynamic) {}

std::string BasicType::print() const {
  std::string result =
      fmt::format("[BasicType] {}\n parent: {}\n dynamic: {}\n size: {}\n fields:\n", m_name,
                  m_parent, m_dynamic, m_size_in_mem);
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
  if (m_pack) {
    // make elements of inline array the minimum allowable alignment.
    int alignment = 8;
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

/////////////////
// Bitfield
/////////////////

BitField::BitField(TypeSpec type, std::string name, int offset, int size)
    : m_type(std::move(type)), m_name(std::move(name)), m_offset(offset), m_size(size) {}

bool BitField::operator==(const BitField& other) const {
  return m_type == other.m_type && m_name == other.m_name && m_offset == other.m_offset &&
         other.m_size == m_size;
}

BitFieldType::BitFieldType(std::string parent, std::string name, int size, bool sign_extend)
    : ValueType(std::move(parent), std::move(name), false, size, sign_extend, RegClass::GPR_64) {}

bool BitFieldType::lookup_field(const std::string& name, BitField* out) const {
  for (auto& field : m_fields) {
    if (field.name() == name) {
      *out = field;
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