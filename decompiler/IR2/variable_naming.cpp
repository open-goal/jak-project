#include "variable_naming.h"
#include "reg_usage.h"
#include "decompiler/Function/Function.h"
#include "third-party/fmt/core.h"

namespace decompiler {

namespace {
template <typename T>
std::string reg_to_string(const T& regs) {
  std::string result;
  for (auto reg : regs) {
    result += reg.to_charp();
    result += ' ';
  }
  return result;
}
}  // namespace

VarMapSSA::VarMapSSA(int n_blocks) : m_block_count(n_blocks) {}

VarSSA VarMapSSA::allocate(Register reg) {
  Entry new_entry;
  new_entry.reg = reg;
  new_entry.entry_id = int(m_entries.size());
  new_entry.var_id = get_next_var_id(reg);
  VarSSA result(reg, new_entry.entry_id);
  m_entries.push_back(new_entry);
  return result;
}

VarSSA VarMapSSA::allocate_init_phi(Register reg, int block_id) {
  Entry new_entry;
  new_entry.reg = reg;
  new_entry.entry_id = int(m_entries.size());
  new_entry.var_id = -block_id;
  VarSSA result(reg, new_entry.entry_id);
  m_entries.push_back(new_entry);
  return result;
}

int VarMapSSA::get_next_var_id(Register reg) {
  return ++m_reg_next_id[reg];
}

void VarMapSSA::merge(const VarSSA& var_a, const VarSSA& var_b) {
  auto& a = m_entries.at(var_a.m_entry_id);
  auto& b = m_entries.at(var_b.m_entry_id);
  auto var_id = std::min(a.var_id, b.var_id);
  a.var_id = var_id;
  b.var_id = var_id;
}

std::string VarMapSSA::to_string(const VarSSA& var) const {
  auto var_id = m_entries.at(var.m_entry_id).var_id;
  if (var_id > 0) {
    return fmt::format("{}-{}", var.m_reg.to_charp(), var_id);
  } else {
    return fmt::format("{}-B{}", var.m_reg.to_charp(), -var_id);
  }
}

std::string SSA::Phi::print(const VarMapSSA& var_map) const {
  std::string result = var_map.to_string(dest);
  result += " <- phi(";
  for (auto& s : sources) {
    result += var_map.to_string(s);
    result += ' ';
  }

  if (!sources.empty()) {
    result.pop_back();
  }

  result += ')';
  return result;
}

std::string SSA::Ins::print(const VarMapSSA& var_map) const {
  std::string result;
  if (dst.has_value()) {
    result += var_map.to_string(*dst) + " <- (";
  } else {
    result += "read(";
  }

  for (auto& s : src) {
    result += var_map.to_string(s);
    result += ' ';
  }

  if (!src.empty()) {
    result.pop_back();
  }

  result += ')';
  return result;
}

std::string SSA::Block::print(const VarMapSSA& var_map) const {
  std::string result;
  for (auto& phi : phis) {
    result += "   ";
    result += phi.second.print(var_map);
    result += '\n';
  }
  for (auto& i : ins) {
    result += "   ";
    result += i.print(var_map);
    result += '\n';
  }
  return result;
}

SSA::Phi& SSA::get_phi(int block, Register dest_reg) {
  auto& phi_map = blocks.at(block).phis;
  auto kv = phi_map.find(dest_reg);
  if (kv == phi_map.end()) {
    auto dest_var = map.allocate_init_phi(dest_reg, block);
    phi_map.insert(std::make_pair(dest_reg, dest_var));
  }
  return phi_map.at(dest_reg);
}

VarSSA SSA::get_phi_dest(int block, Register dest_reg) {
  return get_phi(block, dest_reg).dest;
}

void SSA::add_phi(int block, Register dest_reg, const VarSSA& src_var) {
  auto& phi = get_phi(block, dest_reg);
  phi.sources.push_back(src_var);
}

SSA make_rc_ssa(const Function& function, const RegUsageInfo& rui, const FunctionAtomicOps& ops) {
  // Pass 1 - determine all registers which are read and written.
  std::unordered_set<Register, Register::hash> all_registers;
  for (auto& op : ops.ops) {
    for (auto& r : op->read_regs()) {
      all_registers.insert(r);
    }
    for (auto& w : op->write_regs()) {
      all_registers.insert(w);
    }
  }

  SSA ssa(rui.block_count());
  for (int block_id = 0; block_id < rui.block_count(); block_id++) {
    // loop over each block.
    //    auto& block_info = rui.block.at(block_id);
    const auto& block = function.basic_blocks.at(block_id);
    int start_op = ops.block_id_to_first_atomic_op.at(block_id);
    int end_op = ops.block_id_to_end_atomic_op.at(block_id);

    // local map: current register names.
    std::unordered_map<Register, VarSSA, Register::hash> current_regs;
    // initialize phis
    for (auto reg : all_registers) {
      //      current_regs[reg] = ssa.get_phi_dest(block_id, reg);
      auto it = current_regs.find(reg);
      if (it != current_regs.end()) {
        it->second = ssa.get_phi_dest(block_id, reg);
      } else {
        current_regs.insert(std::make_pair(reg, ssa.get_phi_dest(block_id, reg)));
      }
    }

    // loop over ops.
    for (int op_id = start_op; op_id < end_op; op_id++) {
      const auto& op = ops.ops.at(op_id);
      SSA::Ins ssa_i;
      // todo - verify no duplicates here?
      assert(op->write_regs().size() <= 1);
      // reads:
      for (auto r : op->read_regs()) {
        ssa_i.src.push_back(current_regs.at(r));
      }
      // writes:
      if (!op->write_regs().empty()) {
        auto w = op->write_regs().front();
        auto var = ssa.map.allocate(w);
        ssa_i.dst = var;
        //        current_regs[w] = var;
        auto it = current_regs.find(w);
        if (it != current_regs.end()) {
          it->second = var;
        } else {
          current_regs.insert(std::make_pair(w, var));
        }
      }

      ssa.blocks.at(block_id).ins.push_back(ssa_i);
    }

    // process succs:
    for (auto succ : {block.succ_branch, block.succ_ft}) {
      if (succ != -1) {
        for (auto reg : all_registers) {
          ssa.add_phi(succ, reg, current_regs.at(reg));
        }
      }
    }
  }
  return ssa;
}

std::string SSA::print() const {
  std::string result;
  for (int block_id = 0; block_id < int(blocks.size()); block_id++) {
    result += fmt::format("B-{}\n", block_id);
    result += blocks.at(block_id).print(map);
    result += "\n";
  }
  return result;
}

void run_variable_renaming(const Function& function,
                           const RegUsageInfo& rui,
                           const FunctionAtomicOps& ops) {
  std::string debug_in;
  for (int block_id = 0; block_id < rui.block_count(); block_id++) {
    auto& block_info = rui.block.at(block_id);
    //    const auto& block = function.basic_blocks.at(block_id);
    int start_op = ops.block_id_to_first_atomic_op.at(block_id);
    int end_op = ops.block_id_to_end_atomic_op.at(block_id);

    debug_in += fmt::format("Block {}\n", block_id);
    debug_in += fmt::format(" use: {}\n", reg_to_string(block_info.use));
    debug_in += fmt::format(" in : {}\n", reg_to_string(block_info.input));
    debug_in += "pred: ";
    for (auto p : function.basic_blocks.at(block_id).pred) {
      debug_in += std::to_string(p);
      debug_in += ' ';
    }
    debug_in += '\n';

    for (int op_id = start_op; op_id < end_op; op_id++) {
      debug_in += fmt::format("    {}\n", ops.ops.at(op_id)->to_string(function.ir2.env));
    }

    debug_in += fmt::format(" def: {}\n", reg_to_string(block_info.defs));
    debug_in += fmt::format(" out: {}\n\n", reg_to_string(block_info.output));
  }

  fmt::print("{}", debug_in);

  // Create and convert to SSA
  auto ssa = make_rc_ssa(function, rui, ops);
  fmt::print("{}", ssa.print());

  // eliminate PHIs

  // Merge bad phis

  // merge same vars

  // do rename
}
}  // namespace decompiler
