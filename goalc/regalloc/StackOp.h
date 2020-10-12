#pragma once

/*!
 * @file StackOp.h
 * An operation that's added to an Instruction so that it loads/stores things from the stack if
 * needed for spilling.
 */

#ifndef JAK_STACKOP_H
#define JAK_STACKOP_H

#include <vector>
#include "goalc/emitter/Register.h"

struct StackOp {
  struct Op {
    int slot = -1;
    emitter::Register reg;
    bool load = false;   // load from reg before instruction?
    bool store = false;  // store into reg after instruction?
  };

  std::vector<Op> ops;

  std::string print() const;
};

#endif  // JAK_STACKOP_H
