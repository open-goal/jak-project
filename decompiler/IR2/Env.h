#pragma once

#include <string>
#include "decompiler/Disasm/Register.h"

namespace decompiler {
/*!
 * An "environment" for a single function.
 * This contains data for an entire function, like which registers are live when, the types of
 * values in registers, and local variable names.  This does not actually store IR itself, just
 * shared data that all IR can look at.  The concept is somewhat similar to Env in the compiler.
 */
class Env {
 public:
  bool has_local_vars() const { return m_has_local_vars; }
  std::string get_variable_name(Register reg, int atomic_idx) const;

 private:
  bool m_has_local_vars = false;
};
}  // namespace decompiler