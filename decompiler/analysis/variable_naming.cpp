#include <set>

#include "common/log/log.h"
#include "decompiler/Function/Function.h"
#include "decompiler/IR2/Env.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "reg_usage.h"
#include "third-party/fmt/core.h"
#include "variable_naming.h"

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
 * This should only be used to allocate the result of a non-phi instruction (a real instruction)
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
 * Allocate a new SSA variable for the given register as a result of a phi.
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
  ASSERT(a.reg == b.reg);
  if (b.var_id == 0) {
    //    lg::print("Merge {} <- {}\n", to_string(var_b), to_string(var_a));

    for (auto& entry : m_entries) {
      if (entry.var_id == a.var_id && entry.reg == a.reg) {
        entry.var_id = b.var_id;
      }
    }
    a.var_id = b.var_id;
  } else {
    //    lg::print("Merge {} <- {}\n", to_string(var_a), to_string(var_b));

    for (auto& entry : m_entries) {
      if (entry.var_id == b.var_id && entry.reg == b.reg) {
        entry.var_id = a.var_id;
      }
    }
    b.var_id = a.var_id;
  }
}

void VarMapSSA::merge_reg(Register reg) {
  for (auto& entry : m_entries) {
    if (entry.reg == reg) {
      entry.var_id = 0;
    }
  }
}

/*!
 * Make all Bs A.
 */
void VarMapSSA::merge_to_first(const VarSSA& var_a, const VarSSA& var_b) {
  auto& a = m_entries.at(var_a.m_entry_id);
  auto b = m_entries.at(var_b.m_entry_id);

  //  lg::print("Merge-to-first {} <- {}\n", to_string(var_a), to_string(var_b));
  ASSERT(a.reg == b.reg);

  //  for (auto& entry : m_entries) {
  for (size_t i = 0; i < m_entries.size(); i++) {
    auto& entry = m_entries.at(i);
    if (entry.var_id == b.var_id && entry.reg == b.reg) {
      //      lg::print("remap extra {} var_id from {} to {}\n", i, entry.var_id, a.var_id);
      entry.var_id = a.var_id;
    } else {
      //      lg::print("no remap at {} (prev is {} {})\n", i, entry.reg.to_charp(), entry.var_id);
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
int VarMapSSA::var_id(const VarSSA& var) const {
  return m_entries.at(var.m_entry_id).var_id;
}

/*!
 * For a given register and map, remap using var_id = remap[var_id]
 * For variables not in the map, set ID to INT32_MIN.
 *
 * This allows you to do a full remapping, without worrying new/old mappings aliasing part way
 * through the remapping.
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
    lg::print("[{:02d}] {} {}\n", entry.entry_id, entry.reg.to_charp(), entry.var_id);
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

bool is_arg_reg(Register r) {
  if (r.get_kind() == Reg::GPR) {
    return r.get_gpr() >= Reg::A0 && r.get_gpr() <= Reg::T3;
  } else {
    return false;
  }
}

int arg_reg_idx(Register r) {
  ASSERT(is_arg_reg(r));
  return (int)r.get_gpr() - (int)Reg::A0;
}

bool is_saved_reg(Register r) {
  if (r.get_kind() == Reg::GPR) {
    if (r.get_gpr() == Reg::GP) {
      return true;
    }
    return r.get_gpr() >= Reg::S0 && r.get_gpr() <= Reg::S6;
  } else {
    return false;
  }
}

bool is_possible_coloring_move(Register dst, Register src) {
  if (is_arg_reg(src) && is_saved_reg(dst)) {
    return true;
  }

  if (dst.get_kind() == Reg::FPR && dst.get_fpr() < 20 && is_arg_reg(src)) {
    return true;
  }
  return false;
}

namespace {
int arg_count(const Function& f) {
  if (f.type.arg_count() > 0) {
    return f.type.arg_count() - 1;
  } else {
    return 0;
  }
}
}  // namespace

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

  bool got_not_arg_coloring = false;
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

    // if we're block zero, write function arguments:

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
      if (reg.get_kind() == Reg::FPR || reg.get_kind() == Reg::GPR) {
        init_regs.insert(reg);
      }
    }

    for (auto reg : init_regs) {
      // to avoid operator[]
      auto it = current_regs.find(reg);
      if (it != current_regs.end()) {
        ASSERT(false);
        it->second = ssa.get_phi_dest(block_id, reg);
      } else {
        current_regs.insert(std::make_pair(reg, ssa.get_phi_dest(block_id, reg)));
      }
    }

    if (block_id == 0) {
      SSA::Ins ins(-1);
      for (int i = 0; i < arg_count(function); i++) {
        auto dest_reg = Register::get_arg_reg(i);
        auto it = current_regs.find(dest_reg);
        if (it == current_regs.end()) {
          current_regs.insert(std::make_pair(dest_reg, ssa.get_phi_dest(block_id, dest_reg)));
        }
        ins.src.push_back(current_regs.at(dest_reg));
      }
      ssa.blocks.at(block_id).ins.push_back(ins);
    }

    // loop over ops, creating and reading from variables as needed.
    for (int op_id = start_op; op_id < end_op; op_id++) {
      const auto& op = ops.ops.at(op_id);
      SSA::Ins ssa_i(op_id);

      if (block_id == 0 && !got_not_arg_coloring) {
        got_not_arg_coloring = true;
        auto as_set = dynamic_cast<const SetVarOp*>(op.get());
        if (as_set) {
          auto dst = as_set->dst().reg();

          if ((as_set->src().kind() == SimpleExpression::Kind::GPR_TO_FPR ||
               as_set->src().is_identity()) &&
              as_set->src().get_arg(0).is_var()) {
            auto src = as_set->src().get_arg(0).var().reg();

            if (is_possible_coloring_move(dst, src) &&
                rui.op.at(op_id).consumes.find(src) != rui.op.at(op_id).consumes.end()) {
              // an integer argument going into a fpr for int->float conversion shouldn't
              // be recognized as a coloring move.
              if (function.type.arg_count() > 0) {
                auto arg_idx = arg_reg_idx(src);
                if (dst.get_kind() != Reg::FPR ||
                    function.type.get_arg(arg_idx) == TypeSpec("float")) {
                  ssa_i.is_arg_coloring_move = true;
                  if (dst.get_kind() == Reg::FPR) {
                    ssa_i.is_gpr_fpr_coloring_move = true;
                  }
                  got_not_arg_coloring = false;
                }
              }
            }
          }
        }
      }

      auto as_set = dynamic_cast<const SetVarOp*>(op.get());
      if (as_set) {
        auto dst = as_set->dst().reg();
        if (as_set->src().is_var() ||
            (as_set->src().kind() == SimpleExpression::Kind::FPR_TO_GPR)) {
          auto src = as_set->src().get_arg(0).var().reg();
          auto& ri = rui.op.at(op_id);
          if (ri.consumes.find(src) != ri.consumes.end() &&
              ri.written_and_unused.find(dst) != ri.written_and_unused.end()) {
            ssa_i.is_dead_set = true;
          }
        }
      }

      // todo - verify no duplicates here?
      ASSERT(op->write_regs().size() <= 1);
      // reads:
      for (auto r : op->read_regs()) {
        if (r.get_kind() == Reg::FPR || r.get_kind() == Reg::GPR) {
          ssa_i.src.push_back(current_regs.at(r));
        }
      }
      // writes:
      if (!op->write_regs().empty()) {
        auto w = op->write_regs().front();
        if (w.get_kind() == Reg::FPR || w.get_kind() == Reg::GPR) {
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
      }

      ssa.blocks.at(block_id).ins.push_back(ssa_i);
    }

    // process succs:
    // auto& end_op_info = rui.op.at(end_op - 1);
    for (auto succ : {block.succ_branch, block.succ_ft}) {
      if (succ != -1) {
        auto first_op_in_succ_id = ops.block_id_to_first_atomic_op.at(succ);
        const auto& first_op_info = rui.op.at(first_op_in_succ_id);
        const auto& first_op = ops.ops.at(first_op_in_succ_id);
        // these are the registers that will actually be used in the successor block.
        RegSet regs;
        for (auto& reg : first_op->read_regs()) {
          regs.insert(reg);
        }

        for (auto& reg : first_op_info.live) {
          if (std::find(first_op->write_regs().begin(), first_op->write_regs().end(), reg) ==
              first_op->write_regs().end()) {
            regs.insert(reg);
          }
        }

        for (auto reg : regs) {
          // only update phis for variables that are actually live at the next block.
          if (reg.get_kind() == Reg::FPR || reg.get_kind() == Reg::GPR) {
            ssa.add_source_to_phi(succ, reg, current_regs.at(reg));
          }
        }

        /*
        for (auto reg : end_op_info.live) {
          // only update phis for variables that are actually live at the next block.
          if (reg.get_kind() == Reg::FPR || reg.get_kind() == Reg::GPR) {
            ssa.add_source_to_phi(succ, reg, current_regs.at(reg));
          }
        }
         */
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
 * Merge all variables in the same register to the given register.
 */
void SSA::merge_reg_to_single_variable(Register reg) {
  map.merge_reg(reg);
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
          ASSERT(v_j.has_value());
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

/*!
 * Remaps all SSA variable ids to final variable IDs.
 * This forces you to have all positive, consecutive IDs, with 0 being the entry value.
 */
void SSA::remap(int) {
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
  // we do this in two passes. the first pass collects only the B0 variables and adds those first,
  // so these remain index 0 (expected by later decompiler passes)
  for (auto& block : blocks) {
    ASSERT(block.phis.empty());
    for (auto& instr : block.ins) {
      if (instr.dst.has_value() && map.var_id(*instr.dst) == 0) {
        used_vars[instr.dst->reg()].insert(map.var_id(*instr.dst));
      }
      for (auto& src : instr.src) {
        if (map.var_id(src) == 0) {
          used_vars[src.reg()].insert(map.var_id(src));
        }
      }
    }
  }

  // and the second pass grabs all of them
  for (auto& block : blocks) {
    ASSERT(block.phis.empty());
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

    // paranoid
    ASSERT(var_remap.size() == reg_vars.second.order.size());
    std::unordered_set<int> check;
    for (auto kv : var_remap) {
      check.insert(kv.second);
    }
    ASSERT(check.size() == var_remap.size());

    map.remap_reg(reg_vars.first, var_remap);
    program_read_vars[reg_vars.first].resize(i);
    program_write_vars[reg_vars.first].resize(i);
  }
}

namespace {

TP_Type lca_for_var_types(const TP_Type& existing,
                          const TP_Type& add,
                          const DecompilerTypeSystem& dts,
                          bool event_handler_hack) {
  bool changed;
  auto normal = dts.tp_lca(existing, add, &changed);
  if (!event_handler_hack || normal.typespec().base_type() != "none") {
    return normal;
  }
  if (existing.typespec().base_type() == "none") {
    return add;
  } else if (add.typespec().base_type() == "none") {
    return existing;
  } else {
    return normal;
  }
}

void update_var_info(VariableNames::VarInfo* info,
                     Register reg,
                     const TypeState& ts,
                     int var_id,
                     const DecompilerTypeSystem& dts,
                     bool event_handler_hack) {
  auto& type = ts.get(reg);
  if (info->initialized) {
    ASSERT(info->reg_id.id == var_id);
    ASSERT(info->reg_id.reg == reg);

    info->type = lca_for_var_types(info->type, type, dts, event_handler_hack);

  } else {
    info->reg_id.id = var_id;
    info->reg_id.reg = reg;

    info->type = type;
    info->initialized = true;
  }
}

bool merge_infos(VariableNames::VarInfo* info1,
                 VariableNames::VarInfo* info2,
                 const DecompilerTypeSystem& dts,
                 bool event_handler_hack) {
  if (info1->initialized && info2->initialized) {
    auto new_type = lca_for_var_types(info1->type, info2->type, dts, event_handler_hack);

    info1->type = new_type;
    info2->type = new_type;
    return true;
  }
  return false;
}

void merge_infos(
    std::unordered_map<Register, std::vector<VariableNames::VarInfo>, Register::hash>& info1,
    std::unordered_map<Register, std::vector<VariableNames::VarInfo>, Register::hash>& info2,
    const DecompilerTypeSystem& dts,
    bool event_handler_hack) {
  for (auto& [reg, infos] : info1) {
    auto other = info2.find(reg);
    if (other != info2.end()) {
      for (size_t i = 0; i < std::min(other->second.size(), infos.size()); i++) {
        merge_infos(&infos.at(i), &other->second.at(i), dts, event_handler_hack);
      }
    }
  }
}
}  // namespace

/*!
 * Create variable info for each variable.
 * Note: the "event_handler_hack" is supposed to help with the use of "none" typed variables
 * that are actually used. It's a hack because we don't really have enough information to know if
 * the none variables
 */
void SSA::make_vars(const Function& function, const DecompilerTypeSystem& dts) {
  bool event_handler_hack = false;

  if (function.ir2.env.version == GameVersion::Jak2) {
    event_handler_hack = function.guessed_name.is_event_handler() ||
                         function.guessed_name.to_string() == "target-generic-event-handler" ||
                         function.guessed_name.to_string() == "target-standard-event-handler" ||
                         function.guessed_name.to_string() == "target-board-handler" ||
                         function.guessed_name.to_string() == "(method 74 pegasus)" ||
                         function.guessed_name.to_string() == "(method 74 crimson-guard-level)" ||
                         function.guessed_name.to_string() == "widow-handler" ||
                         function.guessed_name.to_string() == "(method 74 hal)" ||
                         function.guessed_name.to_string() == "water-anim-event-handler" ||
                         function.guessed_name.to_string() == "(method 74 civilian)" ||
                         function.guessed_name.to_string() == "(method 74 crimson-guard)";
  }

  if (function.ir2.env.version == GameVersion::Jak1) {
    event_handler_hack = function.guessed_name.is_event_handler() ||
                         function.guessed_name.to_string() == "target-generic-event-handler";
  }

  for (int block_id = 0; block_id < int(blocks.size()); block_id++) {
    const auto& block = blocks.at(block_id);
    const TypeState* init_types = &function.ir2.env.get_types_at_block_entry(block_id);
    for (auto& instr : block.ins) {
      auto op_id = instr.op_id;
      if (op_id < 0) {
        continue;
      }

      const TypeState* end_types = &function.ir2.env.get_types_after_op(op_id);

      if (instr.dst.has_value()) {
        auto var_id = map.var_id(*instr.dst);
        auto* info = &program_write_vars[instr.dst->reg()].at(var_id);
        update_var_info(info, instr.dst->reg(), *end_types, var_id, dts, event_handler_hack);
      }

      for (auto& src : instr.src) {
        auto var_id = map.var_id(src);
        auto* info = &program_read_vars[src.reg()].at(var_id);
        update_var_info(info, src.reg(), *init_types, var_id, dts, event_handler_hack);
      }

      init_types = end_types;
    }
  }

  // override the types of the variables for function arguments:
  ASSERT(function.type.arg_count() > 0);
  for (int arg_idx = 0; arg_idx < int(function.type.arg_count()) - 1; arg_idx++) {
    auto arg_reg = Register::get_arg_reg(arg_idx);
    if (!program_read_vars[arg_reg].empty()) {
      program_read_vars[arg_reg].at(0).type = TP_Type::make_from_ts(function.type.get_arg(arg_idx));
    }

    if (!program_write_vars[arg_reg].empty()) {
      program_write_vars[arg_reg].at(0).type =
          TP_Type::make_from_ts(function.type.get_arg(arg_idx));
    }
  }

  merge_infos(program_write_vars, program_read_vars, dts, event_handler_hack);

  // copy types from input argument coloring moves:
  for (auto& instr : blocks.at(0).ins) {
    if (instr.is_arg_coloring_move) {
      auto src_ssa = instr.src.at(0);
      for (int arg_idx = 0; arg_idx < int(function.type.arg_count()) - 1; arg_idx++) {
        if (Register::get_arg_reg(arg_idx) == src_ssa.reg()) {
          // copy the type from here.
          auto dst = instr.dst;
          ASSERT(dst);
          auto dst_reg = instr.dst->reg();
          auto dst_varid = map.var_id(*dst);
          if ((int)program_read_vars[dst_reg].size() > dst_varid) {
            program_read_vars[dst_reg].at(dst_varid).type =
                TP_Type::make_from_ts(function.type.get_arg(arg_idx));
          }

          if ((int)program_write_vars[dst_reg].size() > dst_varid) {
            program_write_vars[dst_reg].at(dst_varid).type =
                TP_Type::make_from_ts(function.type.get_arg(arg_idx));
          }
        }
      }
    }
  }
}

void remap_color_move(
    std::unordered_map<Register, std::vector<VariableNames::VarInfo>, Register::hash>& mapping,
    const RegId& old_var,
    const RegId& new_var) {
  auto old_kv = mapping.find(old_var.reg);
  if (old_kv == mapping.end()) {
    return;
  }

  if (int(old_kv->second.size()) <= old_var.id) {
    return;
  }

  old_kv->second.at(old_var.id).reg_id = new_var;
}

VariableNames SSA::get_vars() const {
  VariableNames result;
  result.read_vars = program_read_vars;
  result.write_vars = program_write_vars;

  for (int block_id = 0; block_id < int(blocks.size()); block_id++) {
    const auto& block = blocks.at(block_id);
    for (auto& instr : block.ins) {
      auto op_id = instr.op_id;
      if (op_id < 0) {
        continue;
      }
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
      if (op_id < 0) {
        continue;
      }
      for (auto& src : instr.src) {
        auto& ids = result.read_opid_to_varid[src.reg()];
        if (int(ids.size()) <= op_id) {
          ids.resize(op_id + 1);
        }
        ids.at(op_id) = map.var_id(src);
      }
    }
  }

  for (auto& instr : blocks.at(0).ins) {
    if (instr.is_arg_coloring_move) {
      result.eliminated_move_op_ids.insert(instr.op_id);
      ASSERT(instr.dst.has_value());
      ASSERT(instr.src.size() == 1);
      RegId new_regid, old_regid;
      new_regid.reg = instr.src.at(0).reg();
      new_regid.id = map.var_id(instr.src.at(0));
      old_regid.reg = instr.dst->reg();
      old_regid.id = map.var_id(*instr.dst);
      remap_color_move(result.read_vars, old_regid, new_regid);
      remap_color_move(result.write_vars, old_regid, new_regid);
    }
  }

  return result;
}

/*!
 * Get a map from access to SSA variable.
 */
RegAccessMap<int> SSA::get_ssa_mapping() {
  RegAccessMap<int> result;

  for (const auto& block : blocks) {
    for (const auto& instr : block.ins) {
      if (instr.dst.has_value()) {
        RegisterAccess access(AccessMode::WRITE, instr.dst->reg(), instr.op_id, true);
        result[access] = map.var_id(*instr.dst);
      }

      for (const auto& src : instr.src) {
        RegisterAccess access(AccessMode::READ, src.reg(), instr.op_id, true);
        result[access] = map.var_id(src);
      }
    }
  }

  return result;
}

/*!
 * Find Program Variables that can safely be propagated.
 */
std::unordered_map<RegId, UseDefInfo, RegId::hash> SSA::get_use_def_info(
    const RegAccessMap<int>& ssa_info) const {
  std::unordered_map<RegId, UseDefInfo, RegId::hash> result;

  // now, iterate through instruction
  // for (const auto& block : blocks) {
  for (size_t block_id = 0; block_id < blocks.size(); block_id++) {
    const auto& block = blocks[block_id];
    for (const auto& instr : block.ins) {
      if (instr.is_dead_set) {
        continue;
      }

      if (instr.dst.has_value()) {
        // get the SSA var:
        auto ssa_var_id =
            ssa_info.at(RegisterAccess(AccessMode::WRITE, instr.dst->reg(), instr.op_id, true));
        // get the info
        auto& info = result[RegId(instr.dst->reg(), map.var_id(*instr.dst))];
        // remember which SSA variable was in use here
        info.defs.push_back({instr.op_id, (int)block_id, AccessMode::WRITE});
        info.ssa_vars.insert(ssa_var_id);
      }

      for (const auto& src : instr.src) {
        // get the SSA var:
        auto ssa_var_id =
            ssa_info.at(RegisterAccess(AccessMode::READ, src.reg(), instr.op_id, true));
        // get the info
        auto& info = result[RegId(src.reg(), map.var_id(src))];
        // remember the variable
        info.ssa_vars.insert(ssa_var_id);
        info.uses.push_back({instr.op_id, (int)block_id, AccessMode::READ});
      }
    }
  }

  return result;
}

namespace {

VariableNames::VarInfo* try_lookup_read(VariableNames* in, RegId var_id) {
  auto kv = in->read_vars.find(var_id.reg);
  if (kv != in->read_vars.end()) {
    if ((int)kv->second.size() > var_id.id) {
      auto& entry = kv->second.at(var_id.id);
      if (entry.initialized) {
        return &entry;
      }
    }
  }
  return nullptr;
}

VariableNames::VarInfo* try_lookup_write(VariableNames* in, RegId var_id) {
  auto kv = in->write_vars.find(var_id.reg);
  if (kv != in->write_vars.end()) {
    if ((int)kv->second.size() > var_id.id) {
      auto& entry = kv->second.at(var_id.id);
      if (entry.initialized) {
        return &entry;
      }
    }
  }
  return nullptr;
}

bool is_128bit(const TP_Type& type, const DecompilerTypeSystem& dts) {
  if (dts.ts.tc(TypeSpec("uint128"), type.typespec())) {
    return true;
  }

  if (dts.ts.tc(TypeSpec("int128"), type.typespec())) {
    return true;
  }

  if (type.kind == TP_Type::Kind::PCPYUD_BITFIELD) {
    return true;
  }

  if (type.kind == TP_Type::Kind::PCPYUD_BITFIELD_AND) {
    return true;
  }

  return false;
}

void promote_register_class(const Function& func,
                            VariableNames* result,
                            const DecompilerTypeSystem& dts) {
  enum class PromotionType { PROMOTE_64, PROMOTE_128 };
  std::unordered_map<RegId, PromotionType, RegId::hash> promote_map;
  // here we loop through ops and find cases where we need to adjust types.

  auto& ao = func.ir2.atomic_ops;
  for (size_t op_idx = 0; op_idx < ao->ops.size() - 1; op_idx++) {
    auto* op = ao->ops.at(op_idx).get();
    auto op_as_asm = dynamic_cast<AsmOp*>(op);
    if (op_as_asm) {
      auto& instr = op_as_asm->instruction();
      if (gOpcodeInfo[(int)instr.kind].gpr_128) {
        for (auto& reg : op_as_asm->write_regs()) {
          if (reg.get_kind() == Reg::GPR) {
            auto& info = result->lookup(reg, op_idx, AccessMode::WRITE);
            promote_map[info.reg_id] = PromotionType::PROMOTE_128;
          }
        }

        for (auto& reg : op_as_asm->read_regs()) {
          if (reg.get_kind() == Reg::GPR) {
            auto& info = result->lookup(reg, op_idx, AccessMode::READ);
            promote_map[info.reg_id] = PromotionType::PROMOTE_128;
          }
        }
      }
    }
  }

  for (const auto& promotion : promote_map) {
    // lg::print("Promote {} to {}\n", promotion.first.print(), "uint128");

    // first reads:
    auto read_info = try_lookup_read(result, promotion.first);
    auto write_info = try_lookup_write(result, promotion.first);
    ASSERT(read_info || write_info);

    if (read_info && !is_128bit(read_info->type, dts)) {
      read_info->type = TP_Type::make_from_ts("uint128");
    }

    if (write_info && !is_128bit(write_info->type, dts)) {
      write_info->type = TP_Type::make_from_ts("uint128");
    }
  }
}
}  // namespace

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

    lg::print("Debug Input\n{}\n----------------------------------\n", debug_in);
  }

  // Create and convert to SSA
  auto ssa = make_rc_ssa(function, rui, ops);

  if (debug_prints) {
    lg::print("Basic SSA\n{}\n------------------------------------\n", ssa.print());
  }

  // eliminate PHIs that are not needed, still keeping us in SSA.
  while (ssa.simplify()) {
  }
  if (debug_prints) {
    lg::print("Simplified SSA\n{}-------------------------------\n", ssa.print());
  }

  // merge special registers
  ssa.merge_reg_to_single_variable(Register(Reg::GPR, Reg::SP));
  ssa.merge_reg_to_single_variable(Register(Reg::GPR, Reg::S6));

  // remember what the SSA mapping was:
  auto ssa_mapping = ssa.get_ssa_mapping();

  // Merge phis to return to executable code and exit SSA.
  if (debug_prints) {
    ssa.map.debug_print_map();
  }

  ssa.merge_all_phis();
  if (debug_prints) {
    lg::print("{}", ssa.print());
  }
  if (debug_prints) {
    ssa.map.debug_print_map();
  }

  // merge same vars (decided this made things worse)

  // do rename
  ssa.remap(arg_count(function));
  if (debug_prints) {
    lg::print("{}", ssa.print());
  }

  if (function.ir2.env.has_type_analysis()) {
    // make vars
    ssa.make_vars(function, dts);
    //
    auto result = ssa.get_vars();
    result.use_def_info = ssa.get_use_def_info(ssa_mapping);

    promote_register_class(function, &result, dts);
    return result;
  } else {
    return std::nullopt;
  }
}
}  // namespace decompiler
