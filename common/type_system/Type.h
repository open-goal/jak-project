#pragma once

/*!
 * @file Type.h
 * Representation of a GOAL type in the type system.
 */

#pragma once

#include <string>
#include "common/util/assert.h"
#include <unordered_map>
#include "common/goal_constants.h"
#include "TypeSpec.h"

class TypeSystem;

struct MethodInfo {
  int id = -1;
  std::string name;
  TypeSpec type;
  std::string defined_in_type;

  bool operator==(const MethodInfo& other) const;
  std::string print_one_line() const;
};

/*!
 * Parent class of all Types.
 */
class Type {
 public:
  // is this type treated as a reference to data (pointer), or as data (fits in a register)?
  virtual bool is_reference() const = 0;

  // when loading data of this type into a register, how many bytes do we need?
  virtual int get_load_size() const = 0;

  // do we need to sign extend when loading?
  virtual bool get_load_signed() const = 0;

  // how much space does this use in memory?  For value types, this is the same as load size, as
  // value type data is loaded directly into registers.
  virtual int get_size_in_memory() const = 0;

  // if we have no other information, what kind of register should we load into?
  virtual RegClass get_preferred_reg_class() const = 0;

  // get the "offset" applied to boxed objects
  virtual int get_offset() const = 0;

  // get the alignment for the "size in memory" data.
  virtual int get_in_memory_alignment() const = 0;

  virtual int get_inline_array_stride_alignment() const = 0;
  virtual int get_inline_array_start_alignment() const = 0;

  virtual bool operator==(const Type& other) const = 0;

  // print some information for debugging
  virtual std::string print() const = 0;

  bool operator!=(const Type& other) const { return !(*this == other); }

  bool is_equal(const Type& other) const;
  bool has_parent() const;

  std::string get_name() const;
  std::string get_runtime_name() const;
  std::string get_parent() const;
  void set_runtime_type(std::string name);
  bool get_my_method(const std::string& name, MethodInfo* out) const;
  bool get_my_method(int id, MethodInfo* out) const;
  bool get_my_last_method(MethodInfo* out) const;
  bool get_my_new_method(MethodInfo* out) const;
  const MethodInfo& add_method(const MethodInfo& info);
  const MethodInfo& add_new_method(const MethodInfo& info);
  std::string print_method_info() const;

  void disallow_in_runtime() { m_allow_in_runtime = false; }

  virtual ~Type() = default;

  const std::vector<MethodInfo>& get_methods_defined_for_type() const { return m_methods; }

  const MethodInfo* get_new_method_defined_for_type() const {
    if (m_new_method_info_defined) {
      return &m_new_method_info;
    } else {
      return nullptr;
    }
  }

  bool is_boxed() const { return m_is_boxed; }

  int heap_base() const { return m_heap_base; }

 protected:
  Type(std::string parent, std::string name, bool is_boxed, int heap_base);

  std::vector<MethodInfo> m_methods;
  MethodInfo m_new_method_info;
  bool m_new_method_info_defined = false;

  std::string m_parent;  // the parent type (is empty for none and object)
  std::string m_name;
  bool m_allow_in_runtime = true;
  std::string m_runtime_name;
  bool m_is_boxed = false;  // does this have runtime type information?
  int m_heap_base = 0;
};

/*!
 * Used only for "none" - this is a type that the compiler can use for "this has no value".
 * Attempting to do anything with a NoneType is an error.
 */
class NullType : public Type {
 public:
  NullType(std::string name);
  bool is_reference() const override;
  int get_load_size() const override;
  bool get_load_signed() const override;
  int get_size_in_memory() const override;
  int get_inline_array_stride_alignment() const override;
  int get_inline_array_start_alignment() const override;
  RegClass get_preferred_reg_class() const override;
  int get_offset() const override;
  int get_in_memory_alignment() const override;
  std::string print() const override;
  bool operator==(const Type& other) const override;
  ~NullType() = default;
};

/*!
 * A type which is treated as a value, as opposed to a reference.  These types fit in a register
 * and are always passed by value in arguments/returns.
 */
class ValueType : public Type {
 public:
  ValueType(std::string parent,
            std::string name,
            bool is_boxed,
            int size,
            bool sign_extend,
            RegClass reg);
  bool is_reference() const override;
  int get_load_size() const override;
  bool get_load_signed() const override;
  int get_size_in_memory() const override;
  RegClass get_preferred_reg_class() const override;
  int get_offset() const override;
  int get_in_memory_alignment() const override;
  int get_inline_array_stride_alignment() const override;
  int get_inline_array_start_alignment() const override;
  std::string print() const override;
  bool operator==(const Type& other) const override;
  ~ValueType() = default;

  void inherit(const ValueType* parent);

 private:
  friend class TypeSystem;
  void set_offset(int offset);
  int m_size = -1;
  int m_offset = 0;
  bool m_sign_extend = false;
  RegClass m_reg_kind = RegClass::INVALID;
};

/*!
 * A type which is treated as a reference to data in memory somewhere. Internally, these are
 * treated as a pointer.
 */
class ReferenceType : public Type {
 public:
  ReferenceType(std::string parent, std::string name, bool is_boxed, int heap_base);
  bool is_reference() const override;
  int get_load_size() const override;
  bool get_load_signed() const override;
  RegClass get_preferred_reg_class() const override;
  std::string print() const override;
  ~ReferenceType() = default;
};

class Field {
 public:
  Field() = default;
  Field(std::string name, TypeSpec ts);
  Field(std::string name, TypeSpec ts, int offset);
  void set_dynamic();
  void set_array(int size);
  void set_inline();
  void mark_as_user_placed() { m_placed_by_user = true; }
  std::string print() const;
  const TypeSpec& type() const { return m_type; }
  bool is_inline() const { return m_inline; }
  bool is_array() const { return m_array; }
  bool is_dynamic() const { return m_dynamic; }
  const std::string& name() const { return m_name; }
  int offset() const { return m_offset; }
  bool skip_in_decomp() const { return m_skip_in_static_decomp; }
  bool user_placed() const { return m_placed_by_user; }
  bool operator==(const Field& other) const;

  int alignment() const {
    assert(m_alignment != -1);
    return m_alignment;
  }

  int array_size() const {
    assert(is_array() && !is_dynamic());
    return m_array_size;
  }

 private:
  friend class TypeSystem;
  void set_alignment(int alignment) { m_alignment = alignment; }
  void set_offset(int offset) { m_offset = offset; }
  void set_skip_in_static_decomp() { m_skip_in_static_decomp = true; }

  std::string m_name;
  TypeSpec m_type;
  int m_offset = -1;
  bool m_inline =
      false;  // does not make sense if m_type is value, and not an array and not dynamic
  bool m_dynamic = false;
  bool m_array = false;
  int m_array_size = 0;
  int m_alignment = -1;
  bool m_skip_in_static_decomp = false;
  bool m_placed_by_user = false;  // was this field placed manually by the user?
};

class StructureType : public ReferenceType {
 public:
  StructureType(std::string parent,
                std::string name,
                bool boxed,
                bool dynamic,
                bool pack,
                int heap_base);
  std::string print() const override;
  void inherit(StructureType* parent);
  const std::vector<Field>& fields() const { return m_fields; }
  bool operator==(const Type& other) const override;
  int get_size_in_memory() const override;
  int get_offset() const override;
  int get_in_memory_alignment() const override;
  int get_inline_array_stride_alignment() const override;
  int get_inline_array_start_alignment() const override;
  bool lookup_field(const std::string& name, Field* out);
  bool is_dynamic() const { return m_dynamic; }
  ~StructureType() = default;
  void set_pack(bool pack) { m_pack = pack; }
  void set_heap_base(int hb) { m_heap_base = hb; }
  bool is_packed() const { return m_pack; }
  bool is_allowed_misalign() const { return m_allow_misalign; };
  void set_allow_misalign(bool misalign) { m_allow_misalign = misalign; }

 protected:
  friend class TypeSystem;
  void override_offset(int offset) { m_offset = offset; }
  void override_size_in_memory(
      int size);  // only to be used for setting up weird types like "structure"
  void add_field(const Field& f, int new_size_in_mem) {
    m_fields.push_back(f);
    m_size_in_mem = new_size_in_mem;
  }

  void set_dynamic() { m_dynamic = true; }
  size_t first_unique_field_idx() const { return m_idx_of_first_unique_field; }

  std::vector<Field> m_fields;
  bool m_dynamic = false;
  int m_size_in_mem = 0;
  bool m_pack = false;
  bool m_allow_misalign = false;
  int m_offset = 0;
  size_t m_idx_of_first_unique_field = 0;
};

class BasicType : public StructureType {
 public:
  BasicType(std::string parent, std::string name, bool dynamic, int heap_base);
  int get_offset() const override;
  int get_inline_array_start_alignment() const override;
  std::string print() const override;
  bool final() const { return m_final; }
  void set_final() { m_final = true; }
  ~BasicType() = default;
  bool operator==(const Type& other) const override;

 protected:
  bool m_final = false;
};

class BitField {
 public:
  BitField() = default;
  BitField(TypeSpec type, std::string name, int offset, int size);
  const std::string name() const { return m_name; }
  int offset() const { return m_offset; }
  int size() const { return m_size; }
  const TypeSpec& type() const { return m_type; }
  bool operator==(const BitField& other) const;
  std::string print() const;

 private:
  TypeSpec m_type;
  std::string m_name;
  int m_offset = -1;  // in bits
  int m_size = -1;    // in bits.
};

class BitFieldType : public ValueType {
 public:
  BitFieldType(std::string parent, std::string name, int size, bool sign_extend);
  bool lookup_field(const std::string& name, BitField* out) const;
  std::string print() const override;
  bool operator==(const Type& other) const override;
  const std::vector<BitField>& fields() const { return m_fields; }

 private:
  friend class TypeSystem;
  std::vector<BitField> m_fields;
};

class EnumType : public ValueType {
 public:
  EnumType(const ValueType* parent,
           std::string name,
           bool is_bitfield,
           const std::unordered_map<std::string, s64>& entries);
  std::string print() const override;
  bool operator==(const Type& other) const override;
  const std::unordered_map<std::string, s64>& entries() const { return m_entries; }
  bool is_bitfield() const { return m_is_bitfield; }

 private:
  friend class TypeSystem;
  bool m_is_bitfield = false;
  std::unordered_map<std::string, s64> m_entries;
};
