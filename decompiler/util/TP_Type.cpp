#include "TP_Type.h"
#include "third-party/fmt/core.h"

/*!
 * Takes the weird TP_Types and converts them to one of the main 4.
 * This is supposed to be used if the fancy type analysis steps are attempted but fail.
 */
TP_Type TP_Type::simplify() {
  switch (kind) {
    case PRODUCT:
      return TP_Type(ts);
    default:
      assert(false);
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