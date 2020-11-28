#pragma once
#include <string>
#include <cassert>
#include "common/type_system/TypeSpec.h"
#include "common/common_types.h"
#include "decompiler/Disasm/Register.h"

struct TP_Type {
  enum Kind {
    OBJECT_OF_TYPE,
    TYPE_OBJECT,
    FALSE,
    NONE,
    PRODUCT,
    OBJ_PLUS_PRODUCT,
    PARTIAL_METHOD_TABLE_ACCESS,  // type + method_number * 4
    METHOD_NEW_OF_OBJECT
  } kind = NONE;
  // in the case that we are type_object, just store the type name in a single arg ts.
  TypeSpec ts;
  int multiplier;

  TP_Type() = default;
  explicit TP_Type(const TypeSpec& _ts) {
    kind = OBJECT_OF_TYPE;
    ts = _ts;
  }

  TP_Type simplify() const;
  std::string print() const;

  bool is_object_of_type() const { return kind == TYPE_OBJECT || ts == TypeSpec("type"); }

  TypeSpec as_typespec() const {
    switch (kind) {
      case OBJECT_OF_TYPE:
        return ts;
      case TYPE_OBJECT:
        return TypeSpec("type");
      case FALSE:
        return TypeSpec("symbol");
      case NONE:
        return TypeSpec("none");
      case PRODUCT:
      case METHOD_NEW_OF_OBJECT:
        return ts;
      default:
        assert(false);
    }
  }

  static TP_Type make_partial_method_table_access() {
    TP_Type result;
    result.kind = PARTIAL_METHOD_TABLE_ACCESS;
    return result;
  }

  static TP_Type make_type_object(const std::string& name) {
    TP_Type result;
    result.kind = TYPE_OBJECT;
    result.ts = TypeSpec(name);
    return result;
  }
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
        assert(false);
    }
  }
};