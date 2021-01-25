#include "reg_usage.h"
#include "decompiler/Function/Function.h"

namespace decompiler {
RegUsageInfo::RegUsageInfo(int n_blocks, int n_ops) {
  block.resize(n_blocks);
  op.resize(n_ops);
}

namespace {
bool in_set(RegSet& set, const Register& obj) {
  return set.find(obj) != set.end();
}

void phase1(const FunctionAtomicOps& ops, int block_id, RegUsageInfo* out) {
  int end_op = ops.block_id_to_end_atomic_op.at(block_id);
  int start_op = ops.block_id_to_first_atomic_op.at(block_id);

  for (int i = end_op; i-- > start_op;) {
    const auto& instr = ops.ops.at(i);
    auto read = instr->read_regs();
    auto write = instr->write_regs();

    auto& lv = out->op.at(i).live;
    auto& dd = out->op.at(i).dead;
    auto& block = out->block.at(block_id);

    // make all read live out
    lv.clear();
    for (auto& x : read) {
      lv.insert(x);
    }

    // kill things which are overwritten
    dd.clear();
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

bool phase2(const std::vector<BasicBlock>& blocks, int block_id, RegUsageInfo* info) {
  bool changed = false;
  auto& block_info = info->block.at(block_id);
  const auto& block_obj = blocks.at(block_id);
  auto out = block_info.defs;  // copy

  for (auto s : {block_obj.succ_branch, block_obj.succ_ft}) {
    if (s == -1) {
      continue;
    }
    for (auto in : info->block.at(s).input) {
      out.insert(in);
    }
  }

  RegSet in = block_info.use;
  for (auto x : out) {
    if (!in_set(block_info.defs, x)) {
      in.insert(x);
    }
  }

  if (in != block_info.input || out != block_info.output) {
    changed = true;
    block_info.input = in;
    block_info.output = out;
  }

  return changed;
}

void phase3(const FunctionAtomicOps& ops,
            const std::vector<BasicBlock>& blocks,
            int block_id,
            RegUsageInfo* info) {
  RegSet live_local;
  const auto& block_obj = blocks.at(block_id);
  for (auto s : {block_obj.succ_branch, block_obj.succ_ft}) {
    if (s == -1) {
      continue;
    }
    for (auto i : info->block.at(s).input) {
      live_local.insert(i);
    }
  }

  int end_op = ops.block_id_to_end_atomic_op.at(block_id);
  int start_op = ops.block_id_to_first_atomic_op.at(block_id);

  for (int i = end_op; i-- > start_op;) {
    auto& lv = info->op.at(i).live;
    auto& dd = info->op.at(i).dead;

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

RegUsageInfo analyze_ir2_register_usage(const Function& function) {
  const auto& blocks = function.basic_blocks;
  const auto& ops = function.ir2.atomic_ops;
  RegUsageInfo result(blocks.size(), ops->ops.size() + 1);

  for (int i = 0; i < int(blocks.size()); i++) {
    phase1(*ops, i, &result);
  }

  bool changed = false;
  do {
    changed = false;
    for (int i = 0; i < int(blocks.size()); i++) {
      if (phase2(blocks, i, &result)) {
        changed = true;
      }
    }
  } while (changed);

  for (int i = 0; i < int(blocks.size()); i++) {
    phase3(*ops, blocks, i, &result);
  }

  // we want to know if an op "consumes" a register.
  // this means the value of the register coming in is:
  // A. read by the operation
  // B. dead after the operation.
  // loop over blocks, then
  for (int i = 0; i < int(ops->ops.size()); i++) {
    const auto& op = ops->ops.at(i);
    auto& op_info = result.op.at(i);

    // look at each register we read from:
    for (auto reg : op->read_regs()) {
      if (op_info.live.find(reg) == op_info.live.end()) {
        // not live out, this means we must consume it.
        op_info.consumes.insert(reg);
      } else {
        // the register has a live value, but is it a new value?
        for (auto wr : op->write_regs()) {
          if (wr == reg) {
            op_info.consumes.insert(reg);
          }
        }
      }
    }

    // also useful to know, written and unused.
    for (auto reg : op->write_regs()) {
      if (op_info.live.find(reg) == op_info.live.end()) {
        op_info.written_and_unused.insert(reg);
      }
    }
  }

  result.op.pop_back();
  assert(result.op.size() == ops->ops.size());
  return result;
}
}  // namespace decompiler