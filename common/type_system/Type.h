#ifndef JAK_TYPE_H
#define JAK_TYPE_H

#include <string>
#include <cassert>
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
  virtual RegKind get_preferred_reg_kind() const = 0;

  // get the "offset" applied to boxed objects
  virtual int get_offset() const = 0;

  // get the alignment for the "size in memory" data.
  virtual int get_in_memory_alignment() const = 0;

  virtual int get_inline_array_alignment() const = 0;

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
  bool get_my_last_method(MethodInfo* out) const;
  bool get_my_new_method(MethodInfo* out) const;
  const MethodInfo& add_method(const MethodInfo& info);
  const MethodInfo& add_new_method(const MethodInfo& info);
  std::string print_method_info() const;

  void disallow_in_runtime() { m_allow_in_runtime = false; }

  virtual ~Type() = default;

 protected:
  Type(std::string parent, std::string name, bool is_boxed);

  std::vector<MethodInfo> m_methods;
  MethodInfo m_new_method_info;
  bool m_new_method_info_defined = false;

  std::string m_parent;  // the parent type (is empty for none and object)
  std::string m_name;
  bool m_allow_in_runtime = true;
  std::string m_runtime_name;
  bool m_is_boxed = false;  // does this have runtime type information?
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
  int get_inline_array_alignment() const override;
  RegKind get_preferred_reg_kind() const override;
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
            RegKind reg);
  bool is_reference() const override;
  int get_load_size() const override;
  bool get_load_signed() const override;
  int get_size_in_memory() const override;
  RegKind get_preferred_reg_kind() const override;
  int get_offset() const override;
  int get_in_memory_alignment() const override;
  int get_inline_array_alignment() const override;
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
  RegKind m_reg_kind = RegKind::INVALID;
};

/*!
 * A type which is treated as a reference to data in memory somewhere. Internally, these are
 * treated as a pointer.
 */
class ReferenceType : public Type {
 public:
  ReferenceType(std::string parent, std::string name, bool is_boxed);
  bool is_reference() const override;
  int get_load_size() const override;
  bool get_load_signed() const override;
  RegKind get_preferred_reg_kind() const override;
  std::string print() const override;
  ~ReferenceType() = default;
};

class Field {
 public:
  Field() = default;
  Field(std::string name, TypeSpec ts);
  void set_dynamic();
  void set_array(int size);
  void set_inline();
  std::string print() const;
  const TypeSpec& type() const { return m_type; }

  bool is_inline() const { return m_inline; }

  bool is_array() const { return m_array; }

  bool is_dynamic() const { return m_dynamic; }

  int alignment() const {
    assert(m_alignment != -1);
    return m_alignment;
  }

  int array_size() const {
    assert(is_array() && !is_dynamic());
    return m_array_size;
  }

  const std::string& name() const { return m_name; }

  int offset() const { return m_offset; }

  bool operator==(const Field& other) const;

 private:
  friend class TypeSystem;
  void set_alignment(int alignment) { m_alignment = alignment; }

  void set_offset(int offset) { m_offset = offset; }
  std::string m_name;
  TypeSpec m_type;
  int m_offset = -1;
  bool m_inline =
      false;  // does not make sense if m_type is value, and not an array and not dynamic
  bool m_dynamic = false;
  bool m_array = false;
  int m_array_size = 0;
  int m_alignment = -1;
};

class StructureType : public ReferenceType {
 public:
  StructureType(std::string parent,
                std::string name,
                bool boxed = false,
                bool dynamic = false,
                bool pack = false);
  std::string print() const override;
  void inherit(StructureType* parent);
  const std::vector<Field>& fields() { return m_fields; }
  bool operator==(const Type& other) const override;
  int get_size_in_memory() const override;
  int get_offset() const override;
  int get_in_memory_alignment() const override;
  int get_inline_array_alignment() const override;
  bool lookup_field(const std::string& name, Field* out);
  bool is_dynamic() const { return m_dynamic; }
  ~StructureType() = default;

 protected:
  friend class TypeSystem;
  void override_offset(int offset) {
    m_offset = offset;
  }
  void override_size_in_memory(
      int size);  // only to be used for setting up weird types like "structure"
  void add_field(const Field& f, int new_size_in_mem) {
    m_fields.push_back(f);
    m_size_in_mem = new_size_in_mem;
  }

  void set_dynamic() { m_dynamic = true; }

  std::vector<Field> m_fields;
  bool m_dynamic = false;
  int m_size_in_mem = 0;
  bool m_pack = false;
  int m_offset = 0;
};

class BasicType : public StructureType {
 public:
  BasicType(std::string parent, std::string name, bool dynamic = false);
  int get_offset() const override;
  std::string print() const override;
  ~BasicType() = default;
};

class BitField {};

class BitFieldType : ValueType {};

#endif  // JAK_TYPE_H
