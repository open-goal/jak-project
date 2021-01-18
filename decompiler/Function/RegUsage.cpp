#include "Function.h"
#include "decompiler/IR/IR.h"

namespace decompiler {
namespace {
bool in_set(RegSet& set, const Register& obj) {
  return set.find(obj) != set.end();
}

void phase1(Function& f, BasicBlock& block) {
  for (int i = block.end_basic_op; i-- > block.start_basic_op;) {
    auto& instr = f.basic_ops.at(i);
    auto& lv = block.live.at(i - block.start_basic_op);
    auto& dd = block.dead.at(i - block.start_basic_op);

    // make all read live out
    auto read = instr->read_regs;
    lv.clear();
    for (auto& x : read) {
      lv.insert(x);
    }

    // kill things which are overwritten
    dd.clear();
    auto write = instr->write_regs;
    for (auto& x : write) {
      if (!in_set(lv, x)) {
        dd.insert(x);
      }
    }

    // b.use = i.liveout
    RegSet use_old = block.use;
    block.use.clear();
    for (auto& x : lv) {
      block.use.insert(x);
    }
    // | (bu.use & !i.dead)
    for (auto& x : use_old) {
      if (!in_set(dd, x)) {
        block.use.insert(x);
      }
    }

    // b.defs = i.dead
    RegSet defs_old = block.defs;
    block.defs.clear();
    for (auto& x : dd) {
      block.defs.insert(x);
    }
    // | b.defs & !i.lv
    for (auto& x : defs_old) {
      if (!in_set(lv, x)) {
        block.defs.insert(x);
      }
    }
  }
}

bool phase2(std::vector<BasicBlock>& blocks, BasicBlock& block) {
  bool changed = false;
  auto out = block.defs;

  for (auto s : {block.succ_branch, block.succ_ft}) {
    if (s == -1) {
      continue;
    }
    for (auto in : blocks.at(s).input) {
      out.insert(in);
    }
  }

  RegSet in = block.use;
  for (auto x : out) {
    if (!in_set(block.defs, x)) {
      in.insert(x);
    }
  }

  if (in != block.input || out != block.output) {
    changed = true;
    block.input = in;
    block.output = out;
  }

  return changed;
}

void phase3(std::vector<BasicBlock>& blocks, BasicBlock& block) {
  RegSet live_local;
  for (auto s : {block.succ_branch, block.succ_ft}) {
    if (s == -1) {
      continue;
    }
    for (auto i : blocks.at(s).input) {
      live_local.insert(i);
    }
  }

  for (int i = block.end_basic_op; i-- > block.start_basic_op;) {
    auto& lv = block.live.at(i - block.start_basic_op);
    auto& dd = block.dead.at(i - block.start_basic_op);

    RegSet new_live = lv;
    for (auto x : live_local) {
      if (!in_set(dd, x)) {
        new_live.insert(x);
      }
    }
    lv = live_local;
    live_local = new_live;
  }
}

}  // namespace
/*!
 * Analyze the function use of registers to determine which are live where.
 */
void Function::run_reg_usage() {
  // phase 1
  for (auto& block : basic_blocks) {
    block.live.resize(block.basic_op_size());
    block.dead.resize(block.basic_op_size());
    phase1(*this, block);
  }

  // phase 2
  bool changed = false;
  do {
    changed = false;
    for (auto& block : basic_blocks) {
      if (phase2(basic_blocks, block)) {
        changed = true;
      }
    }
  } while (changed);

  // phase 3
  for (auto& block : basic_blocks) {
    phase3(basic_blocks, block);
  }

  // we want to know if an op "consumes" a register.
  // this means that the value of the register coming in to the operation is:
  // A. read by the operation.
  // B. no longer read after the operation.
  for (auto& block : basic_blocks) {
    for (int i = block.start_basic_op; i < block.end_basic_op; i++) {
      auto& op = basic_ops.at(i);
      // look at each register that we read
      for (auto reg : op->read_regs) {
        if (!block.op_has_reg_live_out(i, reg)) {
          // if the register is not live out, we definitely consume it.
          op->consumed.insert(reg);
        } else {
          // it's live out... but it could be a new value.
          for (auto wr : op->write_regs) {
            if (wr == reg) {
              op->consumed.insert(reg);
            }
          }
        }
      }

      for (auto reg : op->write_regs) {
        if (!block.op_has_reg_live_out(i, reg)) {
          // we wrote it, but it is immediately dead. this is nice to know for things like
          // "is this if/and/or expression used as a value?"
          op->written_and_unused.insert(reg);
        }
      }
    }
  }
}
}  // namespace decompiler