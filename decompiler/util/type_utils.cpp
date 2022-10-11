#include "type_utils.h"

namespace decompiler {
bool allowable_base_type_for_symbol_to_string(const TypeSpec& ts) {
  auto& bt = ts.base_type();
  return bt == "symbol" || bt == "basic" || bt == "structure" || bt == "object";
}
}  // namespace decompiler
