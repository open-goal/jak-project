#include "Val.h"

/*!
 * Fallback to_gpr if a more optimized one is not provided.
 */
RegVal* Val::to_gpr(FunctionEnv* fe) const {
  (void)fe;
  throw std::runtime_error("Val::to_gpr NYI");
}

/*!
 * Fallback to_xmm if a more optimized one is not provided.
 */
RegVal* Val::to_xmm(FunctionEnv* fe) const {
  (void)fe;
  throw std::runtime_error("Val::to_xmm NYI");
}

RegVal* None::to_reg(FunctionEnv* fe) const {
  (void)fe;
  throw std::runtime_error("Cannot put None into a register.");
}
