#include "TP_Type.h"
#include "third-party/fmt/core.h"

namespace decompiler {
u32 regs_to_gpr_mask(const std::vector<Register>& regs) {
  u32 result = 0;
  for (const auto& reg : regs) {
    if (reg.get_kind() == Reg::GPR) {
      result |= (1 << reg.get_gpr());
    }
  }
  return result;
}

std::string TypeState::print_gpr_masked(u32 mask) const {
  std::string result;
  for (int i = 0; i < 32; i++) {
    if (mask & (1 << i)) {
      result += Register(Reg::GPR, i).to_charp();
      result += ": ";
      result += gpr_types[i].print();
      result += " ";
    }
  }
  return result;
}

std::string TP_Type::print() const {
  switch (kind) {
    case Kind::TYPESPEC:
      return m_ts.print();
    case Kind::TYPE_OF_TYPE_OR_CHILD:
      return fmt::format("<the type {}>", m_ts.print());
    case Kind::FALSE_AS_NULL:
      return fmt::format("'#f");
    case Kind::UNINITIALIZED:
      return fmt::format("<uninitialized>");
    case Kind::PRODUCT_WITH_CONSTANT:
      return fmt::format("<value x {}>", m_int);
    case Kind::OBJECT_PLUS_PRODUCT_WITH_CONSTANT:
      return fmt::format("<{} + (value x {})>", m_ts.print(), m_int);
    case Kind::OBJECT_NEW_METHOD:
      return fmt::format("<(object-new) for {}>", m_ts.print());
    case Kind::STRING_CONSTANT:
      return fmt::format("<string \"{}\">", m_str);
    case Kind::FORMAT_STRING:
      return fmt::format("<string with {} args>", m_int);
    case Kind::INTEGER_CONSTANT:
      return fmt::format("<integer {}>", m_int);
    case Kind::DYNAMIC_METHOD_ACCESS:
      return fmt::format("<dynamic-method-access>");
    case Kind::INVALID:
    default:
      assert(false);
  }
}

bool TP_Type::operator==(const TP_Type& other) const {
  if (kind != other.kind) {
    return false;
  }

  switch (kind) {
    case Kind::TYPESPEC:
      return m_ts == other.m_ts;
    case Kind::TYPE_OF_TYPE_OR_CHILD:
      return m_ts == other.m_ts;
    case Kind::FALSE_AS_NULL:
      return true;
    case Kind::UNINITIALIZED:
      return true;
    case Kind::PRODUCT_WITH_CONSTANT:
      return m_int == other.m_int;
    case Kind::OBJECT_PLUS_PRODUCT_WITH_CONSTANT:
      return m_ts == other.m_ts && m_int == other.m_int;
    case Kind::OBJECT_NEW_METHOD:
      return m_ts == other.m_ts;
    case Kind::STRING_CONSTANT:
      return m_str == other.m_str;
    case Kind::INTEGER_CONSTANT:
      return m_int == other.m_int;
    case Kind::FORMAT_STRING:
      return m_int == other.m_int;
    case Kind::DYNAMIC_METHOD_ACCESS:
      return true;
    case Kind::INVALID:
    default:
      assert(false);
  }
}

bool TP_Type::operator!=(const TP_Type& other) const {
  return !((*this) == other);
}

TypeSpec TP_Type::typespec() const {
  switch (kind) {
    case Kind::TYPESPEC:
      return m_ts;
    case Kind::TYPE_OF_TYPE_OR_CHILD:
      return TypeSpec("type");
    case Kind::FALSE_AS_NULL:
      return TypeSpec("symbol");
    case Kind::UNINITIALIZED:
      return TypeSpec("none");
    case Kind::PRODUCT_WITH_CONSTANT:
      return TypeSpec("int");
    case Kind::OBJECT_PLUS_PRODUCT_WITH_CONSTANT:
      // this can be part of an array access, so we don't really know the type.
      // probably not a good idea to try to do anything with this as a typespec
      // so let's be very vague
      return TypeSpec("object");
    case Kind::OBJECT_NEW_METHOD:
      // similar to previous case, being more vague than we need to be because we don't
      // want to assume the return type incorrectly and you shouldn't try to do anything with
      // this as a typespec.
      return TypeSpec("function");
    case Kind::STRING_CONSTANT:
      return TypeSpec("string");
    case Kind::INTEGER_CONSTANT:
      return TypeSpec("int");
    case Kind::DYNAMIC_METHOD_ACCESS:
      return TypeSpec("object");
    case Kind::FORMAT_STRING:
      return TypeSpec("string");
    case Kind::INVALID:
    default:
      assert(false);
  }
}
}  // namespace decompiler