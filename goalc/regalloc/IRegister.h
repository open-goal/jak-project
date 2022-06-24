#pragma once

/*!
 * IRegister is the Register for the Intermediate Representation.
 */

#include <string>
#include <vector>

#include "goalc/emitter/Register.h"

struct IRegister {
  RegClass reg_class = RegClass::INVALID;
  int id = -1;
  std::string to_string() const;
  struct hash {
    auto operator()(const IRegister& r) const { return std::hash<int>()(r.id); }
  };
};

struct IRegConstraint {
  IRegister ireg;
  int instr_idx = -1;
  bool contrain_everywhere = false;
  emitter::Register desired_register;
  std::string to_string() const;
};
