#include "TP_Type.h"
#include "third-party/fmt/core.h"

/*!
 * Takes the weird TP_Types and converts them to one of the main 4.
 * This is supposed to be used if the fancy type analysis steps are attempted but fail.
 */
TP_Type TP_Type::simplify() const {
  switch (kind) {
    case PRODUCT:
      return TP_Type(ts);
    case METHOD_NEW_OF_OBJECT:
      return TP_Type(ts);
    case OBJ_PLUS_PRODUCT:
      return TP_Type(TypeSpec("none"));
    default:
      return *this;
  }
}

std::string TP_Type::print() const {
  switch (kind) {
    case OBJECT_OF_TYPE:
      return ts.print();
    case TYPE_OBJECT:
      return fmt::format("[{}]", ts.print());
    case FALSE:
      return fmt::format("[#f]");
    case NONE:
      return fmt::format("[none]");
    case PRODUCT:
      return fmt::format("[{} x {}]", ts.print(), multiplier);
    case PARTIAL_METHOD_TABLE_ACCESS:
      return fmt::format("[[vtable-access]]");
    case METHOD_NEW_OF_OBJECT:
      return fmt::format("[(method object new)]");
    case OBJ_PLUS_PRODUCT:
      return fmt::format("[{} + int x {}]", ts.print(), multiplier);
    default:
      assert(false);
  }
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