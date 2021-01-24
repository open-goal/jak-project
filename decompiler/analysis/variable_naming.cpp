#include <set>
#include "variable_naming.h"
#include "reg_usage.h"
#include "decompiler/Function/Function.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "decompiler/IR2/Env.h"
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

/*!
 * Allocate a new SSA variable for the given register.
 * This should only be used to allocate the result of a non-phi instruction.
 */
VarSSA VarMapSSA::allocate(Register reg) {
  Entry new_entry;
  new_entry.reg = reg;
  new_entry.entry_id = int(m_entries.size());
  new_entry.var_id = get_next_var_id(reg);
  VarSSA result(reg, new_entry.entry_id);
  m_entries.push_back(new_entry);
  return result;
}

/*!
 * Allocate a new SSA for the given register.
 * This should only be used to allocate the result of a phi-function.
 */
VarSSA VarMapSSA::allocate_init_phi(Register reg, int block_id) {
  Entry new_entry;
  new_entry.reg = reg;
  new_entry.entry_id = int(m_entries.size());
  new_entry.var_id = -block_id;
  VarSSA result(reg, new_entry.entry_id);
  m_entries.push_back(new_entry);
  return result;
}

/*!
 * Get the next unused variable id for the given register.
 */
int VarMapSSA::get_next_var_id(Register reg) {
  return ++m_reg_next_id[reg];
}

/*!
 * Combine the two variables into one. The final name is:
 * - B0, if either is B0
 * - otherwise b's name.
 */
void VarMapSSA::merge(const VarSSA& var_a, const VarSSA& var_b) {
  auto& a = m_entries.at(var_a.m_entry_id);
  auto b = m_entries.at(var_b.m_entry_id);
  assert(a.reg == b.reg);
  if (b.var_id == 0) {
    //    fmt::print("Merge {} <- {}\n", to_string(var_b), to_string(var_a));

    for (auto& entry : m_entries) {
      if (entry.var_id == a.var_id && entry.reg == a.reg) {
        entry.var_id = b.var_id;
      }
    }
    a.var_id = b.var_id;
  } else {
    //    fmt::print("Merge {} <- {}\n", to_string(var_a), to_string(var_b));

    for (auto& entry : m_entries) {
      if (entry.var_id == b.var_id && entry.reg == b.reg) {
        entry.var_id = a.var_id;
      }
    }
    b.var_id = a.var_id;
  }
}

/*!
 * Make all Bs A.
 */
void VarMapSSA::merge_to_first(const VarSSA& var_a, const VarSSA& var_b) {
  auto& a = m_entries.at(var_a.m_entry_id);
  auto b = m_entries.at(var_b.m_entry_id);

  //  fmt::print("Merge-to-first {} <- {}\n", to_string(var_a), to_string(var_b));
  assert(a.reg == b.reg);

  //  for (auto& entry : m_entries) {
  for (size_t i = 0; i < m_entries.size(); i++) {
    auto& entry = m_entries.at(i);
    if (entry.var_id == b.var_id && entry.reg == b.reg) {
      //      fmt::print("remap extra {} var_id from {} to {}\n", i, entry.var_id, a.var_id);
      entry.var_id = a.var_id;
    } else {
      //      fmt::print("no remap at {} (prev is {} {})\n", i, entry.reg.to_charp(), entry.var_id);
    }
  }
  b.var_id = a.var_id;
}

std::string VarMapSSA::to_string(const VarSSA& var) const {
  auto var_id = m_entries.at(var.m_entry_id).var_id;
  if (var_id > 0) {
    return fmt::format("{}-{}", var.m_reg.to_charp(), var_id);
  } else {
    return fmt::format("{}-B{}", var.m_reg.to_charp(), -var_id);
  }
}

/*!
 * Do these two SSA variables represent the same "program variable"
 */
bool VarMapSSA::same(const VarSSA& var_a, const VarSSA& var_b) const {
  return var_a.m_reg == var_b.m_reg &&
         m_entries.at(var_a.m_entry_id).var_id == m_entries.at(var_b.m_entry_id).var_id;
}

/*!
 * Get program variable ID from an SSA variable.
 */
int VarMapSSA::var_id(const VarSSA& var) {
  return m_entries.at(var.m_entry_id).var_id;
}

/*!
 * For a given register and map, remap using var_id = remap[var_id]
 * For variables not in the map, set ID to INT32_MIN.
 */
void VarMapSSA::remap_reg(Register reg, const std::unordered_map<int, int>& remap) {
  for (auto& entry : m_entries) {
    if (entry.reg == reg) {
      auto kv = remap.find(entry.var_id);
      if (kv == remap.end()) {
        entry.var_id = INT32_MIN;
      } else {
        entry.var_id = kv->second;
      }
    }
  }
}

void VarMapSSA::debug_print_map() const {
  for (auto& entry : m_entries) {
    fmt::print("[{:02d}] {} {}\n", entry.entry_id, entry.reg.to_charp(), entry.var_id);
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

/*!
 * Get the phi function that sets the initial value of the given register in this block
 * If no phi function exists, it will be created.
 */
SSA::Phi& SSA::get_phi(int block, Register dest_reg) {
  auto& phi_map = blocks.at(block).phis;
  auto kv = phi_map.find(dest_reg);
  if (kv == phi_map.end()) {
    //    printf("Allocate new get_phi for %s B%d\n", dest_reg.to_charp(), block);
    auto dest_var = map.allocate_init_phi(dest_reg, block);
    phi_map.insert(std::make_pair(dest_reg, dest_var));
  }
  return phi_map.at(dest_reg);
}

/*!
 * Get the result (SSA variable) of the phi function that sets the value of the given register in
 * this block. If there is no phi which sets this, creates one (empty)
 */
VarSSA SSA::get_phi_dest(int block, Register dest_reg) {
  return get_phi(block, dest_reg).dest;
}

/*!
 * Add a source SSA variable to the phi setting the initial value of dest reg at the top of the
 * given block. If there is no phi which sets dest_reg, creates one.
 */
void SSA::add_source_to_phi(int block, Register dest_reg, const VarSSA& src_var) {
  auto& phi = get_phi(block, dest_reg);
  phi.sources.push_back(src_var);
}

namespace {

/*!
 * Create a "really crude" SSA, as described in
 * "Aycock and Horspool Simple Generation of Static Single-Assignment Form"
 *
 * Note - we do a few tricks to make this more efficient, inspired by "improvement 1", but
 * implemented slightly differently. (I couldn't figure out how to efficiently implement their
 * improvement 1).  We also take advantage of precomputed register usage info to avoid creating
 * totally useless phis that propagate unused values through to the end of the function.
 */
SSA make_rc_ssa(const Function& function, const RegUsageInfo& rui, const FunctionAtomicOps& ops) {
  SSA ssa(rui.block_count());
  for (int block_id = 0; block_id < rui.block_count(); block_id++) {
    const auto& block = function.basic_blocks.at(block_id);
    int start_op = ops.block_id_to_first_atomic_op.at(block_id);
    int end_op = ops.block_id_to_end_atomic_op.at(block_id);
    if (start_op == end_op) {
      // later we rely on having > 0 ops in our block, so we must reject 0 size blocks.
      if (block_id + 1 == rui.block_count()) {
        // if it's the last block, just ignore it. The expression propagator will ignore it too,
        // so the return value will safely make it to the end.
        continue;
      }
      // otherwise give up. This is something that should be fixed upstream (#196).
      throw std::runtime_error("Zero size blocks not yet supported");
    }

    // local map: current register names at the current op.
    std::unordered_map<Register, VarSSA, Register::hash> current_regs;

    // initialize phis. this is only done on:
    //  - variables live out at the first op
    //  - variables read by the first op
    // which should contain at least all live variables at the beginning of the block.
    // this may accidentally add a phi for a variable that's dead at the block entry but is
    // defined by the first op. This is no big deal, as it will be trivially eliminated later on.
    const auto& start_op_info = rui.op.at(start_op);
    const auto& start_op_op = ops.ops.at(start_op);
    auto init_regs = start_op_info.live;
    for (auto reg : start_op_op->read_regs()) {
      init_regs.insert(reg);
    }

    for (auto reg : init_regs) {
      // to avoid operator[]
      auto it = current_regs.find(reg);
      if (it != current_regs.end()) {
        assert(false);
        it->second = ssa.get_phi_dest(block_id, reg);
      } else {
        current_regs.insert(std::make_pair(reg, ssa.get_phi_dest(block_id, reg)));
      }
    }

    // loop over ops, creating and reading from variables as needed.
    for (int op_id = start_op; op_id < end_op; op_id++) {
      const auto& op = ops.ops.at(op_id);
      SSA::Ins ssa_i(op_id);
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
        // avoid operator[] again
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
    auto& end_op_info = rui.op.at(end_op - 1);
    for (auto succ : {block.succ_branch, block.succ_ft}) {
      if (succ != -1) {
        for (auto reg : end_op_info.live) {
          // only update phis for variables that are actually live at the next block.
          ssa.add_source_to_phi(succ, reg, current_regs.at(reg));
        }
      }
    }
  }
  return ssa;
}
}  // namespace

std::string SSA::print() const {
  std::string result;
  for (int block_id = 0; block_id < int(blocks.size()); block_id++) {
    result += fmt::format("B-{}\n", block_id);
    result += blocks.at(block_id).print(map);
    result += "\n";
  }
  return result;
}

/*!
 * Simplify the SSA while still keeping it in SSA form.
 * This does only a single pass of simplifications and returns true if it made changes.
 */
bool SSA::simplify() {
  bool changed = false;
  for (auto& block : blocks) {
    auto it = block.phis.begin();
    while (it != block.phis.end()) {
      // first case: all sources are the same as the destination.
      // note - this will remove all phis with 1 or 0 arguments.
      bool remove = true;
      auto& dst = it->second.dest;
      for (auto& src : it->second.sources) {
        if (!map.same(src, dst)) {
          remove = false;
          break;
        }
      }

      if (!remove) {
        // second case. V_i = phi(combo of i, j)
        remove = true;
        auto v_i = it->second.dest;
        std::optional<VarSSA> v_j;
        for (auto& src : it->second.sources) {
          if (!map.same(v_i, src)) {
            // three cases:
            if (!v_j.has_value()) {
              // this is the first time we see j
              v_j = src;
            } else {
              // we know j...
              if (!map.same(*v_j, src)) {
                // but it's not a match. three different vars, so give up.
                remove = false;
                break;
              }
              // else, we know j and matched it, continue checking
            }
          }
        }

        if (remove) {
          assert(v_j.has_value());
          map.merge(*v_j, v_i);
        }
      }

      if (remove) {
        changed = true;
        it = block.phis.erase(it);
      } else {
        it++;
      }
    }
  }
  return changed;
}

/*!
 * Convert from SSA to a form without phis. This takes advantage of the following properties:
 * - All phis have all sources and dest in the same HW register.
 * - Merging variables in the same register is safe because they can't have overlapping use.
 * - As a bonus, this never merges variables that we _know_ are distinct GOAL variables.
 */
void SSA::merge_all_phis() {
  for (auto& block : blocks) {
    for (auto& phi : block.phis) {
      for (auto& src : phi.second.sources) {
        map.merge_to_first(phi.second.dest, src);
      }
    }
    block.phis.clear();
  }
}

void SSA::remap() {
  // this keeps the order of variable assignments in the instruction order, not var_id order.
  struct VarIdRecord {
    std::unordered_set<int> set;
    std::vector<int> order;
    void insert(int x) {
      if (set.find(x) == set.end()) {
        set.insert(x);
        order.push_back(x);
      }
    }
  };
  std::unordered_map<Register, VarIdRecord, Register::hash> used_vars;
  for (auto& block : blocks) {
    assert(block.phis.empty());
    for (auto& instr : block.ins) {
      if (instr.dst.has_value()) {
        used_vars[instr.dst->reg()].insert(map.var_id(*instr.dst));
      }
      for (auto& src : instr.src) {
        used_vars[src.reg()].insert(map.var_id(src));
      }
    }
  }

  for (auto& reg_vars : used_vars) {
    std::unordered_map<int, int> var_remap;
    int i = 0;
    for (auto var_id : reg_vars.second.order) {
      var_remap[var_id] = i++;
    }
    map.remap_reg(reg_vars.first, var_remap);
    program_read_vars[reg_vars.first].resize(i);
    program_write_vars[reg_vars.first].resize(i);
  }
}

namespace {
void update_var_info(VariableNames::VarInfo* info,
                     Register reg,
                     const TypeState& ts,
                     int var_id,
                     const DecompilerTypeSystem& dts) {
  if (info->initialized) {
    assert(info->reg_id.id == var_id);
    assert(info->reg_id.reg == reg);
    bool changed;
    info->type = dts.tp_lca(info->type, ts.get(reg), &changed);
  } else {
    info->reg_id.id = var_id;
    info->reg_id.reg = reg;
    info->type = ts.get(reg);
    info->initialized = true;
  }
}
}  // namespace

void SSA::make_vars(const Function& function, const DecompilerTypeSystem& dts) {
  for (int block_id = 0; block_id < int(blocks.size()); block_id++) {
    const auto& block = blocks.at(block_id);
    const TypeState* init_types = &function.ir2.env.get_types_at_block_entry(block_id);
    for (auto& instr : block.ins) {
      auto op_id = instr.op_id;
      const TypeState* end_types = &function.ir2.env.get_types_after_op(op_id);

      if (instr.dst.has_value()) {
        auto var_id = map.var_id(*instr.dst);
        auto* info = &program_write_vars[instr.dst->reg()].at(var_id);
        update_var_info(info, instr.dst->reg(), *end_types, var_id, dts);
      }

      for (auto& src : instr.src) {
        auto var_id = map.var_id(src);
        auto* info = &program_read_vars[src.reg()].at(var_id);
        update_var_info(info, src.reg(), *init_types, var_id, dts);
      }

      init_types = end_types;
    }
  }
}

VariableNames SSA::get_vars() {
  VariableNames result;
  result.read_vars = program_read_vars;
  result.write_vars = program_write_vars;

  for (int block_id = 0; block_id < int(blocks.size()); block_id++) {
    const auto& block = blocks.at(block_id);
    for (auto& instr : block.ins) {
      auto op_id = instr.op_id;
      if (instr.dst.has_value()) {
        auto& ids = result.write_opid_to_varid[instr.dst->reg()];
        if (int(ids.size()) <= op_id) {
          ids.resize(op_id + 1);
        }
        ids.at(op_id) = map.var_id(*instr.dst);
      }
    }
  }

  for (int block_id = 0; block_id < int(blocks.size()); block_id++) {
    const auto& block = blocks.at(block_id);
    for (auto& instr : block.ins) {
      auto op_id = instr.op_id;
      for (auto& src : instr.src) {
        auto& ids = result.read_opid_to_varid[src.reg()];
        if (int(ids.size()) <= op_id) {
          ids.resize(op_id + 1);
        }
        ids.at(op_id) = map.var_id(src);
      }
    }
  }

  return result;
}

std::optional<VariableNames> run_variable_renaming(const Function& function,
                                                   const RegUsageInfo& rui,
                                                   const FunctionAtomicOps& ops,
                                                   const DecompilerTypeSystem& dts,
                                                   bool debug_prints) {
  if (debug_prints) {
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
        debug_in +=
            fmt::format(" [{:03d}]   {} : ", op_id, ops.ops.at(op_id)->to_string(function.ir2.env));
        auto& op_info = rui.op.at(op_id);
        for (auto reg : op_info.live) {
          debug_in += reg.to_charp();
          debug_in += ' ';
        }
        debug_in += '\n';
      }

      debug_in += fmt::format(" def: {}\n", reg_to_string(block_info.defs));
      debug_in += fmt::format(" out: {}\n\n", reg_to_string(block_info.output));
    }

    fmt::print("Debug Input\n{}\n----------------------------------\n", debug_in);
  }

  // Create and convert to SSA
  auto ssa = make_rc_ssa(function, rui, ops);

  if (debug_prints) {
    fmt::print("Basic SSA\n{}\n------------------------------------\n", ssa.print());
  }

  // eliminate PHIs that are stupid.
  while (ssa.simplify()) {
  }
  if (debug_prints) {
    fmt::print("Simplified SSA\n{}-------------------------------\n", ssa.print());
  }

  // Merge phis to return to executable code.
  if (debug_prints) {
    ssa.map.debug_print_map();
  }

  ssa.merge_all_phis();
  if (debug_prints) {
    fmt::print("{}", ssa.print());
  }
  if (debug_prints) {
    ssa.map.debug_print_map();
  }

  // merge same vars (decided this made things worse)

  // do rename
  ssa.remap();
  if (debug_prints) {
    fmt::print("{}", ssa.print());
  }

  if (function.ir2.env.has_type_analysis()) {
    // make vars
    ssa.make_vars(function, dts);
    return ssa.get_vars();
  } else {
    return std::nullopt;
  }
}
}  // namespace decompiler
