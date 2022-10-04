#pragma once
#include <stdexcept>
#include <string>
#include <unordered_map>

#include "common/common_types.h"
#include "common/type_system/TypeSpec.h"
#include "common/util/Assert.h"

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
    NON_OBJECT_NEW_METHOD,      // the method new of some type that's not object.
    STRING_CONSTANT,            // a string that's part of the string pool
    FORMAT_STRING,              // a string with a given number of format arguments
    INTEGER_CONSTANT,           // a constant integer.
    INTEGER_CONSTANT_PLUS_VAR,  // constant + variable. for dynamic addr of
    INTEGER_CONSTANT_PLUS_VAR_MULT,  // like var + 100 + 12 * var2
    DYNAMIC_METHOD_ACCESS,           // partial access into a
    VIRTUAL_METHOD,
    NON_VIRTUAL_METHOD,
    PCPYUD_BITFIELD,
    PCPYUD_BITFIELD_AND,
    LEFT_SHIFTED_BITFIELD,  // (bitfield << some-constant)
    LABEL_ADDR,
    ENTER_STATE_FUNCTION,
    RUN_FUNCTION_IN_PROCESS_FUNCTION,
    SET_TO_RUN_FUNCTION,
    GET_ART_BY_NAME_METHOD,
    SYMBOL,
    INVALID
  } kind = Kind::UNINITIALIZED;
  TP_Type() = default;
  std::string print() const;
  bool operator==(const TP_Type& other) const;
  bool operator!=(const TP_Type& other) const;
  TypeSpec typespec() const;

  /*!
   * Returns true if the expression with this type should always be wrapped in a cast.
   */
  bool requires_cast() const {
    switch (kind) {
      case Kind::TYPESPEC:
      case Kind::TYPE_OF_TYPE_OR_CHILD:
      case Kind::TYPE_OF_TYPE_NO_VIRTUAL:
      case Kind::FALSE_AS_NULL:  // if we want all #f's for references to be cast, move this.
      case Kind::PRODUCT_WITH_CONSTANT:
      case Kind::STRING_CONSTANT:
      case Kind::FORMAT_STRING:
      case Kind::INTEGER_CONSTANT:
      case Kind::INTEGER_CONSTANT_PLUS_VAR:
      case Kind::INTEGER_CONSTANT_PLUS_VAR_MULT:
      case Kind::VIRTUAL_METHOD:
      case Kind::NON_VIRTUAL_METHOD:
      case Kind::LEFT_SHIFTED_BITFIELD:
      case Kind::PCPYUD_BITFIELD:
      case Kind::PCPYUD_BITFIELD_AND:
      case Kind::LABEL_ADDR:
      case Kind::ENTER_STATE_FUNCTION:
      case Kind::RUN_FUNCTION_IN_PROCESS_FUNCTION:
      case Kind::SET_TO_RUN_FUNCTION:
      case Kind::GET_ART_BY_NAME_METHOD:
      case Kind::NON_OBJECT_NEW_METHOD:
      case Kind::SYMBOL:
        return false;
      case Kind::UNINITIALIZED:
      case Kind::OBJECT_NEW_METHOD:
      case Kind::DYNAMIC_METHOD_ACCESS:
        return true;
      case Kind::OBJECT_PLUS_PRODUCT_WITH_CONSTANT:
        return m_ts.base_type() != "pointer";
      case Kind::INVALID:
      default:
        ASSERT(false);
    }
  }

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
  bool is_symbol() const { return kind == Kind::SYMBOL; }

  int get_format_string_arg_count() const {
    ASSERT(is_format_string());
    return m_int;
  }

  const std::string& get_string() const {
    ASSERT(is_constant_string());
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

  static TP_Type make_virtual_method(const TypeSpec& method_type,
                                     const TypeSpec& obj_type,
                                     int method_id) {
    TP_Type result;
    result.kind = Kind::VIRTUAL_METHOD;
    result.m_ts = method_type;
    result.m_method_from_type = obj_type;
    result.m_method_id = method_id;
    return result;
  }

  static TP_Type make_non_virtual_method(const TypeSpec& method_type,
                                         const TypeSpec& obj_type,
                                         int method_id) {
    TP_Type result;
    result.kind = Kind::NON_VIRTUAL_METHOD;
    result.m_method_from_type = obj_type;
    result.m_ts = method_type;
    result.m_method_id = method_id;
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

  static TP_Type make_symbol(const std::string& name) {
    TP_Type result;
    result.kind = Kind::SYMBOL;
    result.m_str = name;
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

  static TP_Type make_from_integer_constant_plus_var(int64_t value,
                                                     const TypeSpec& var_type,
                                                     const TypeSpec& sum_type) {
    TP_Type result;
    result.kind = Kind::INTEGER_CONSTANT_PLUS_VAR;
    result.m_int = value;
    result.m_ts = var_type;
    result.m_method_from_type = sum_type;
    return result;
  }

  static TP_Type make_from_integer_constant_plus_product(int64_t constant,
                                                         const TypeSpec& var_type,
                                                         int64_t multiplier) {
    TP_Type result;
    result.kind = Kind::INTEGER_CONSTANT_PLUS_VAR_MULT;
    result.m_int = constant;
    result.m_extra_multiplier = multiplier;
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

  static TP_Type make_non_object_new(const TypeSpec& function_type, const TypeSpec& object_type) {
    TP_Type result;
    result.kind = Kind::NON_OBJECT_NEW_METHOD;
    result.m_ts = function_type;
    result.m_method_from_type = object_type;
    return result;
  }

  /*!
   * flipped means it's int + obj.
   */
  static TP_Type make_object_plus_product(const TypeSpec& ts, int64_t multiplier, bool flipped) {
    TP_Type result;
    result.kind = Kind::OBJECT_PLUS_PRODUCT_WITH_CONSTANT;
    result.m_ts = ts;
    result.m_int = multiplier;
    result.m_flipped_order = flipped;
    return result;
  }

  static TP_Type make_from_left_shift_bitfield(const TypeSpec& ts, int amount, bool pcpyud) {
    TP_Type result;
    result.kind = Kind::LEFT_SHIFTED_BITFIELD;
    result.m_ts = ts;
    result.m_int = amount;
    result.m_pcpyud = pcpyud;
    return result;
  }

  static TP_Type make_from_pcpyud_bitfield(const TypeSpec& ts) {
    TP_Type result;
    result.kind = Kind::PCPYUD_BITFIELD;
    result.m_ts = ts;
    result.m_pcpyud = true;
    return result;
  }

  static TP_Type make_from_pcpyud_and_bitfield(const TypeSpec& ts) {
    TP_Type result;
    result.kind = Kind::PCPYUD_BITFIELD_AND;
    result.m_ts = ts;
    result.m_pcpyud = true;
    return result;
  }

  static TP_Type make_enter_state() {
    TP_Type result;
    result.kind = Kind::ENTER_STATE_FUNCTION;
    return result;
  }

  static TP_Type make_label_addr(int label_id) {
    TP_Type result;
    result.kind = Kind::LABEL_ADDR;
    result.m_int = label_id;
    return result;
  }

  static TP_Type make_run_function_in_process_function() {
    TP_Type result;
    result.kind = Kind::RUN_FUNCTION_IN_PROCESS_FUNCTION;
    return result;
  }

  static TP_Type make_set_to_run_function() {
    TP_Type result;
    result.kind = Kind::SET_TO_RUN_FUNCTION;
    return result;
  }

  static TP_Type make_get_art_by_name(const TypeSpec& method_ts,
                                      const TypeSpec& obj_ts,
                                      int method_id) {
    TP_Type result;
    result.kind = Kind::GET_ART_BY_NAME_METHOD;
    result.m_ts = method_ts;
    result.m_method_from_type = obj_ts;
    result.m_method_id = method_id;
    return result;
  }

  const TypeSpec& get_objects_typespec() const {
    ASSERT(kind == Kind::TYPESPEC || kind == Kind::INTEGER_CONSTANT_PLUS_VAR);
    return m_ts;
  }

  const TypeSpec& get_type_objects_typespec() const {
    ASSERT(kind == Kind::TYPE_OF_TYPE_OR_CHILD || kind == Kind::TYPE_OF_TYPE_NO_VIRTUAL);
    return m_ts;
  }

  const TypeSpec& get_method_new_object_typespec() const {
    ASSERT(kind == Kind::OBJECT_NEW_METHOD);
    return m_ts;
  }

  const TypeSpec& get_obj_plus_const_mult_typespec() const {
    ASSERT(kind == Kind::OBJECT_PLUS_PRODUCT_WITH_CONSTANT);
    return m_ts;
  }

  uint64_t get_multiplier() const {
    ASSERT(kind == Kind::PRODUCT_WITH_CONSTANT || kind == Kind::OBJECT_PLUS_PRODUCT_WITH_CONSTANT);
    return m_int;
  }

  uint64_t get_integer_constant() const {
    ASSERT(kind == Kind::INTEGER_CONSTANT || kind == Kind::INTEGER_CONSTANT_PLUS_VAR);
    return m_int;
  }

  u64 get_add_int_constant() const {
    ASSERT(kind == Kind::INTEGER_CONSTANT_PLUS_VAR_MULT);
    return m_int;
  }

  u64 get_mult_int_constant() const {
    ASSERT(kind == Kind::INTEGER_CONSTANT_PLUS_VAR_MULT);
    return m_extra_multiplier;
  }

  int get_left_shift() const {
    ASSERT(kind == Kind::LEFT_SHIFTED_BITFIELD);
    return m_int;
  }

  const TypeSpec& get_bitfield_type() const {
    ASSERT(kind == Kind::LEFT_SHIFTED_BITFIELD || kind == Kind::PCPYUD_BITFIELD ||
           kind == Kind::PCPYUD_BITFIELD_AND);
    return m_ts;
  }

  bool pcpyud() const {
    if (kind == Kind::LEFT_SHIFTED_BITFIELD) {
      return m_pcpyud;
    }
    ASSERT(false);
    return false;
  }

  const TypeSpec& method_from_type() const {
    ASSERT(kind == Kind::VIRTUAL_METHOD || kind == Kind::NON_VIRTUAL_METHOD ||
           kind == Kind::GET_ART_BY_NAME_METHOD || kind == Kind::NON_OBJECT_NEW_METHOD);
    return m_method_from_type;
  }

  int method_id() const {
    ASSERT(kind == Kind::VIRTUAL_METHOD || kind == Kind::NON_VIRTUAL_METHOD ||
           kind == Kind::GET_ART_BY_NAME_METHOD);
    return m_method_id;
  }

  bool flipped_add_order() const {
    ASSERT(kind == Kind::OBJECT_PLUS_PRODUCT_WITH_CONSTANT);
    return m_flipped_order;
  }

  int label_id() const {
    ASSERT(kind == Kind::LABEL_ADDR);
    return m_int;
  }

 private:
  TypeSpec m_ts;
  TypeSpec m_method_from_type;  // hack, also stores sum type.
  int m_method_id = -1;
  std::string m_str;
  int64_t m_int = 0;
  bool m_pcpyud = false;  // have we extracted the top doubleword of a bitfield?
  bool m_flipped_order = false;

  int64_t m_extra_multiplier = 0;
};

class TypeState {
 private:
 public:
  std::unordered_map<int, TP_Type> spill_slots;
  TP_Type gpr_types[32];
  TP_Type fpr_types[32];
  TP_Type next_state_type;
  std::string print_gpr_masked(u32 mask) const;
  TP_Type& get(const Register& r) {
    switch (r.get_kind()) {
      case Reg::GPR:
        return gpr_types[r.get_gpr()];
      case Reg::FPR:
        return fpr_types[r.get_fpr()];
      default:
        ASSERT(false);
        throw std::runtime_error("TP_Type::get failed");
    }
  }

  const TP_Type& get(const Register& r) const {
    switch (r.get_kind()) {
      case Reg::GPR:
        return gpr_types[r.get_gpr()];
      case Reg::FPR:
        return fpr_types[r.get_fpr()];
      default:
        ASSERT(false);
        throw std::runtime_error("TP_Type::get failed");
    }
  }

  const TP_Type& get_slot(int offset) const {
    auto result = spill_slots.find(offset);
    if (result == spill_slots.end()) {
      throw std::runtime_error("TP_Type::get_slot failed: " + std::to_string(offset));
    }
    return result->second;
  }

  TP_Type& get_slot(int offset) { return spill_slots[offset]; }
};

u32 regs_to_gpr_mask(const std::vector<Register>& regs);
}  // namespace decompiler
