/*!
 * @file StackOp.h
 * An operation that's added to an Instruction so that it loads/stores things from the stack if
 * needed for spilling.
 */

#ifndef JAK_STACKOP_H
#define JAK_STACKOP_H

#include <vector>
#include "third-party/fmt/core.h"
#include "goalc/emitter/Register.h"

struct StackOp {
  struct Op {
    int slot = -1;
    emitter::Register reg;
    bool load = false;   // load from reg before instruction?
    bool store = false;  // store into reg after instruction?
  };

  std::vector<Op> ops;

  std::string print() const {
    std::string result;
    bool added = false;
    for (const auto& op : ops) {
      if (op.load) {
        result += fmt::format("{} <- [{:2d}], ", emitter::gRegInfo.get_info(op.reg).name, op.slot);
        added = true;
      }
      if (op.store) {
        result += fmt::format("{} -> [{:2d}], ", emitter::gRegInfo.get_info(op.reg).name, op.slot);
        added = true;
      }
    }

    if (added) {
      result.pop_back();
      result.pop_back();
    }

    return result;
  }
};

#endif  // JAK_STACKOP_H
