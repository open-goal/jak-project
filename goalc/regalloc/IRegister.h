/*!
 * IRegister is the Register for the Intermediate Representation.
 */

#ifndef JAK_IREGISTER_H
#define JAK_IREGISTER_H

#include <string>
#include <vector>
#include "goalc/emitter/Register.h"

struct IRegister {
  emitter::RegKind kind = emitter::RegKind::INVALID;
  int id = -1;
  std::string to_string() const;
  struct hash {
    auto operator()(const IRegister& r) const { return std::hash<int>()(r.id); }
  };
};

struct IRegConstraint {
  IRegister ireg;
  int instr_idx = -1;
  emitter::Register desired_register;
  std::string to_string() const;
};

#endif  // JAK_IREGISTER_H
