#pragma once
#include <string>
#include <cassert>
#include "common/log/log.h"
#include "common/type_system/TypeSpec.h"
#include "common/common_types.h"
#include "decompiler/Disasm/Register.h"

namespace decompiler {
/*!
 * A TP_Type is a specialized typespec used in the type propagation algorithm.
 * It is basically a normal typespec plus some optional information.
 * It should always use register types.
 */
class TP_Type {
 public:
  enum class Kind {
    TYPESPEC,               // just a normal typespec
    TYPE_OF_TYPE_OR_CHILD,  // a type object, of the given type of a child type.
    TYPE_OF_TYPE_NO_VIRTUAL,
    FALSE_AS_NULL,                      // the GOAL "false" object, possibly used as a null.
    UNINITIALIZED,                      // representing data which is uninitialized.
    PRODUCT_WITH_CONSTANT,              // representing: (val * multiplier)
    OBJECT_PLUS_PRODUCT_WITH_CONSTANT,  // address: obj + (val * multiplier)
    OBJECT_NEW_METHOD,          // the method new of object, as used in an (object-new) or similar.
    STRING_CONSTANT,            // a string that's part of the string pool
    FORMAT_STRING,              // a string with a given number of format arguments
    INTEGER_CONSTANT,           // a constant integer.
    INTEGER_CONSTANT_PLUS_VAR,  // constant + variable. used in stuff like (&-> obj inline-val-arr
                                // x)
    DYNAMIC_METHOD_ACCESS,      // partial access into a
    METHOD,
    INVALID
  } kind = Kind::UNINITIALIZED;
  TP_Type() = default;
  std::string print() const;
  bool operator==(const TP_Type& other) const;
  bool operator!=(const TP_Type& other) const;
  TypeSpec typespec() const;

  bool is_constant_string() const { return kind == Kind::STRING_CONSTANT; }
  bool is_integer_constant() const { return kind == Kind::INTEGER_CONSTANT; }
  bool is_integer_constant(int64_t value) const { return is_integer_constant() && m_int == value; }
  bool is_product() const { return kind == Kind::PRODUCT_WITH_CONSTANT; }
  bool is_product_plus_obj() const { return kind == Kind::OBJECT_PLUS_PRODUCT_WITH_CONSTANT; }
  bool is_product_with(int64_t value) const {
    return kind == Kind::PRODUCT_WITH_CONSTANT && m_int == value;
  }
  bool is_format_string() const { return kind == Kind::FORMAT_STRING; }
  bool can_be_format_string() const { return is_format_string() || is_constant_string(); }

  int get_format_string_arg_count() const {
    assert(is_format_string());
    return m_int;
  }

  const std::string& get_string() const {
    assert(is_constant_string());
    return m_str;
  }

  static TP_Type make_from_format_string(int n_args) {
    TP_Type result;
    result.kind = Kind::FORMAT_STRING;
    result.m_int = n_args;
    return result;
  }

  static TP_Type make_from_ts(const TypeSpec& ts) {
    TP_Type result;
    result.kind = Kind::TYPESPEC;
    result.m_ts = ts;
    return result;
  }

  static TP_Type make_from_ts(const std::string& ts) { return make_from_ts(TypeSpec(ts)); }

  static TP_Type make_method(const TypeSpec& method_type) {
    TP_Type result;
    result.kind = Kind::METHOD;
    result.m_ts = method_type;
    return result;
  }

  static TP_Type make_from_string(const std::string& str) {
    TP_Type result;
    result.kind = Kind::STRING_CONSTANT;
    result.m_str = str;
    return result;
  }

  static TP_Type make_type_allow_virtual_object(const TypeSpec& type) {
    TP_Type result;
    result.kind = Kind::TYPE_OF_TYPE_OR_CHILD;
    result.m_ts = type;
    return result;
  }

  static TP_Type make_type_no_virtual_object(const TypeSpec& type) {
    TP_Type result;
    result.kind = Kind::TYPE_OF_TYPE_NO_VIRTUAL;
    result.m_ts = type;
    return result;
  }

  static TP_Type make_false() {
    TP_Type result;
    result.kind = Kind::FALSE_AS_NULL;
    return result;
  }

  static TP_Type make_uninitialized() {
    TP_Type result;
    result.kind = Kind::UNINITIALIZED;
    return result;
  }

  static TP_Type make_from_integer(int64_t value) {
    TP_Type result;
    result.kind = Kind::INTEGER_CONSTANT;
    result.m_int = value;
    return result;
  }

  static TP_Type make_from_integer_constant_plus_var(int64_t value, const TypeSpec& var_type) {
    TP_Type result;
    result.kind = Kind::INTEGER_CONSTANT_PLUS_VAR;
    result.m_int = value;
    result.m_ts = var_type;
    return result;
  }

  static TP_Type make_from_product(int64_t multiplier, bool is_signed) {
    TP_Type result;
    result.kind = Kind::PRODUCT_WITH_CONSTANT;
    result.m_int = multiplier;
    result.m_ts = is_signed ? TypeSpec("int") : TypeSpec("uint");
    return result;
  }

  static TP_Type make_partial_dyanmic_vtable_access() {
    TP_Type result;
    result.kind = Kind::DYNAMIC_METHOD_ACCESS;
    return result;
  }

  static TP_Type make_object_new(const TypeSpec& ts) {
    TP_Type result;
    result.kind = Kind::OBJECT_NEW_METHOD;
    result.m_ts = ts;
    return result;
  }

  static TP_Type make_object_plus_product(const TypeSpec& ts, int64_t multiplier) {
    TP_Type result;
    result.kind = Kind::OBJECT_PLUS_PRODUCT_WITH_CONSTANT;
    result.m_ts = ts;
    result.m_int = multiplier;
    return result;
  }

  const TypeSpec& get_objects_typespec() const {
    assert(kind == Kind::TYPESPEC || kind == Kind::INTEGER_CONSTANT_PLUS_VAR);
    return m_ts;
  }

  const TypeSpec& get_type_objects_typespec() const {
    assert(kind == Kind::TYPE_OF_TYPE_OR_CHILD || kind == Kind::TYPE_OF_TYPE_NO_VIRTUAL);
    return m_ts;
  }

  const TypeSpec& get_method_new_object_typespec() const {
    assert(kind == Kind::OBJECT_NEW_METHOD);
    return m_ts;
  }

  const TypeSpec& get_obj_plus_const_mult_typespec() const {
    assert(kind == Kind::OBJECT_PLUS_PRODUCT_WITH_CONSTANT);
    return m_ts;
  }

  uint64_t get_multiplier() const {
    assert(kind == Kind::PRODUCT_WITH_CONSTANT || kind == Kind::OBJECT_PLUS_PRODUCT_WITH_CONSTANT);
    return m_int;
  }

  uint64_t get_integer_constant() const {
    assert(kind == Kind::INTEGER_CONSTANT || kind == Kind::INTEGER_CONSTANT_PLUS_VAR);
    return m_int;
  }

 private:
  TypeSpec m_ts;
  std::string m_str;
  int64_t m_int = 0;
};

struct TypeState {
  TP_Type gpr_types[32];
  TP_Type fpr_types[32];

  std::string print_gpr_masked(u32 mask) const;
  TP_Type& get(const Register& r) {
    switch (r.get_kind()) {
      case Reg::GPR:
        return gpr_types[r.get_gpr()];
      case Reg::FPR:
        return fpr_types[r.get_fpr()];
      default:
        assert(false);
    }
  }

  const TP_Type& get(const Register& r) const {
    switch (r.get_kind()) {
      case Reg::GPR:
        return gpr_types[r.get_gpr()];
      case Reg::FPR:
        return fpr_types[r.get_fpr()];
      default:
        lg::die("Cannot use register {} with TypeState.", r.to_charp());
        assert(false);
    }
  }
};

u32 regs_to_gpr_mask(const std::vector<Register>& regs);
}  // namespace decompiler