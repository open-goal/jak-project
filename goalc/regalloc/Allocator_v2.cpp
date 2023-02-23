#include "Allocator_v2.h"

#include <algorithm>
#include <optional>
#include <unordered_map>

#include "common/log/log.h"
#include "common/util/Range.h"

#include "third-party/fmt/core.h"

/*!
 Documentation:

 Compared to the first version, Allocator2 has several changes.

 First, the data structures used don't support splitting variables within the allocator.
 Allocator1 never actually did any splitting but supported this.  The reasons for this are:
 - I can't figure out how the allocator can decide how to safely split, so any splitting would need
   to happen outside of the allocator.  Deciding when to insert splits seems like a pretty unsolved
   problem, and harder to do greedily than adding in too many splits and coalescing.
 - The OpenGOAL compiler doesn't reuse variables almost ever, meaning the only places where we
   could insert splits is with user variables.
 - Splitting user variables requires SSA, which would make sense to do outside the allocator.
 - Splitting is likely to be highly ineffective on our automatically decompiled code because it
   literally splits as much as safely possible as part of the register -> variable process.

 Second: We have a smarter approach for picking registers.
 We do a few different passes:
  - Do constrained allocations.
  - Allocate for variables which cross a function boundary, trying to move-eliminate with previous.
  - Allocate variables that could possibly be move eliminated with a previous allocation.
  - (repeat the above step until no more are found)
  - Allocate remaining variables

 Third: We have a new approach for spilling.
 In the old allocator we had to reserve registers for use only in spill loads/stores.
 In this design, we don't do this, but instead demote variables to the stack when we run out.
 This means that we must support changing the assignment of a variable.

 All the "fancy" features of Allocator v1 will be supported by default:
 - Fancy coloring (use interference graph, not live range overlap)
 - Move Eliminator (try to eliminate move instructions)
 - Allow Read Write Same Reg

 Future improvements:
 - Within a basic block, try to drop load/store instructions for stack ops.
 */

namespace {

// set this to restrict the allocator to a small subset of registers.
// this has the effect of adding many spills even to very simple functions.
// It can be used to test less common spilling situations.
constexpr bool torture_test_spills = false;

/*!
 * The VarAssignment is the per-variable information used by the allocator.
 * It "has_info" over a variable's live range.
 * It also tracks individual instructions within this range and can tell you if the variable
 * is actually alive at a given instruction within its live range.
 */
class VarAssignment {
 public:
  // The assignment state.
  // Register: permanently in a register.
  // Stack: by default in the stack, moved to temporary register as needed for use.
  enum class Kind { UNASSIGNED, STACK, REGISTER };

  // unassigned by default.
  // last_live is inclusive
  VarAssignment(int first_live, int last_live, int var) : m_first_live(first_live), m_var_idx(var) {
    m_live.resize(1 + last_live - first_live, false);
  }

  // Setup functions:

  // mark the given instruction as live.
  void mark_live(int instr) {
    m_live.at(instr - m_first_live) = true;
    m_seen = true;
  }

  // mark this variable as having a function call inside its live range.
  // this will make it prefer saved registers.
  void mark_crossing_function() { m_crosses_function_call = true; }

  // Get info functions

  // the number of instructions in the live range.
  int range_size() { return m_live.size(); }

  // get the index of the first instruction in the live range
  int first_live() const { return m_first_live; }

  // get the index of the last instruction in the live range (includes this instruction)
  int last_live() const { return m_first_live + m_live.size() - 1; }

  // is the given instruction within the live range?
  bool has_info_at(int instr) const { return instr >= first_live() && instr <= last_live(); }

  // is the variable "live" at the given instruction?
  // This is true if it is "liveout", or if the instruction reads/write the register.
  bool live(int instr) const { return has_info_at(instr) && m_live.at(instr - m_first_live); }

  // is the variable unassigned?
  bool unassigned() const { return m_kind == Kind::UNASSIGNED; }

  // is the variable assigned to a register or to a stack slot
  bool assigned() const { return m_kind != Kind::UNASSIGNED; }

  // is the variable assigned to any register?
  bool assigned_to_reg() const { return m_kind == Kind::REGISTER; }

  bool assigned_to_stack() const { return m_kind == Kind::STACK; }

  // is the variable assigned to the given register?
  bool assigned_to_reg(emitter::Register reg) const {
    return assigned_to_reg() && m_assigned_register == reg;
  }

  // get the register that this variable is assigned to. Must be assigned_to_reg()
  emitter::Register reg() const {
    ASSERT(assigned_to_reg());
    return m_assigned_register;
  }

  // get the variable index of the variable
  int var() const { return m_var_idx; }

  // have we "seen" this variable in the IR?
  bool seen() const { return m_seen; }

  // does the variable's live range cross a function call?
  bool crosses_function() const { return m_crosses_function_call; }

  bool locked() const { return m_locked; }

  // Assignment functions:

  // put this variable in the given register and constrain it there (prevent spilling)
  void constrain_to_register(const emitter::Register& reg) {
    ASSERT(unassigned());
    m_kind = Kind::REGISTER;
    m_locked = true;
    m_assigned_register = reg;
  }

  void assign_to_register(const emitter::Register& reg) {
    ASSERT(unassigned());
    m_kind = Kind::REGISTER;
    m_locked = false;
    m_assigned_register = reg;
  }

  void assign_to_stack(int slot) {
    ASSERT(unassigned());
    m_kind = Kind::STACK;
    m_locked = false;
    m_stack_temp_regs.resize(m_live.size());
    m_stack_slot = slot;
  }

  void demote_to_stack(int slot) {
    ASSERT(assigned_to_reg());
    m_kind = Kind::STACK;
    m_locked = false;
    m_stack_temp_regs.resize(m_live.size());
    m_stack_slot = slot;
  }

  void set_stack_slot_reg(const emitter::Register& reg, int instr_idx) {
    ASSERT(assigned_to_stack());
    ASSERT(!m_stack_temp_regs.at(instr_idx - first_live()));
    m_stack_temp_regs.at(instr_idx - first_live()) = reg;
  }

  void clear_stack_slot_regs() {
    for (auto& x : m_stack_temp_regs) {
      x = {};
    }
  }

  emitter::Register get_stack_slot_reg(int instr_idx) const {
    ASSERT(assigned_to_stack());
    ASSERT(m_stack_temp_regs.at(instr_idx - first_live()));
    return *m_stack_temp_regs.at(instr_idx - first_live());
  }

  bool stack_bonus_op_needs_reg(const emitter::Register& reg, int instr_idx) const {
    ASSERT(assigned_to_stack());
    auto& stack_reg = m_stack_temp_regs.at(instr_idx - m_first_live);
    return stack_reg && (*stack_reg == reg);
  }

  // result get
  std::vector<Assignment> make_assignment_vector() const {
    std::vector<Assignment> asses;
    asses.reserve(m_live.size());
    if (assigned_to_reg()) {
      for (int i = 0; i < (int)m_live.size(); i++) {
        Assignment a;
        a.kind = Assignment::Kind::REGISTER;
        a.reg = m_assigned_register;
        asses.push_back(a);
      }
      return asses;
    } else if (assigned_to_stack()) {
      for (int i = 0; i < (int)m_live.size(); i++) {
        Assignment a;
        a.kind = Assignment::Kind::STACK;
        a.stack_slot = m_stack_slot;
        auto& slot_reg = m_stack_temp_regs.at(i);
        if (slot_reg) {
          a.kind = Assignment::Kind::REGISTER;
          a.reg = *slot_reg;
        }
        asses.push_back(a);
      }
      return asses;
    } else {
      ASSERT(false);
    }
  }

  const std::vector<bool>& live_vector() const { return m_live; }

 private:
  // common info
  Kind m_kind = Kind::UNASSIGNED;
  int m_first_live = -1;     // index of instr where we are first alive
  int m_var_idx = -1;        // which variable we are
  std::vector<bool> m_live;  // where we are live, starting at m_first_live.

  // register assignment only
  emitter::Register m_assigned_register;  // if we are REGISTER assigned, our register.
  bool m_locked = false;  // if we are a REGISTER, means we must stay in the given register.
  bool m_crosses_function_call = false;
  bool m_seen = false;

  // stack assignment only
  int m_stack_slot = -1;
  // starting at m_first_live, the temporary register we're assigned for stack slots.
  std::vector<std::optional<emitter::Register>> m_stack_temp_regs;
};

struct RACache {
  ControlFlowAnalysisCache control_flow;
  std::vector<VarAssignment> vars;  // per var
  std::vector<bool> was_allocated;  // per var
  std::vector<bool> used_var;       // per var
  std::vector<IRegister> iregs;
  std::vector<StackOp> stack_ops;  // per instr.
  std::unordered_map<s32, s32> var_to_stack_slot;
  // list of live vars per instruction.
  std::vector<std::vector<s32>> live_per_instruction;

  std::vector<IRegSet> liveout_per_instr;
  int current_stack_slot = 0;
  bool used_stack = false;
  bool failed_alloc = false;

  struct Stats {
    int var_count = 0;
    int assign_passes = 0;
    int num_spilled_vars = 0;
    int num_spill_ops = 0;
  } stats;
};

struct AssignmentOrder {
  std::vector<emitter::Register> xmms, gprs;
};

AssignmentOrder REG_saved_first_order = {
    {emitter::XMM8, emitter::XMM9, emitter::XMM10, emitter::XMM11, emitter::XMM12, emitter::XMM13,
     emitter::XMM14, emitter::XMM15, emitter::XMM7, emitter::XMM6, emitter::XMM5, emitter::XMM4,
     emitter::XMM3, emitter::XMM2, emitter::XMM1, emitter::XMM0},
    {emitter::RBX, emitter::RBP, emitter::R12, emitter::R11, emitter::R10, emitter::R9, emitter::R8,
     emitter::RCX, emitter::RDX, emitter::RSI, emitter::RDI, emitter::RAX}};

AssignmentOrder REG_temp_first_order = {
    {emitter::XMM7, emitter::XMM6, emitter::XMM5, emitter::XMM4, emitter::XMM3, emitter::XMM2,
     emitter::XMM1, emitter::XMM0, emitter::XMM8, emitter::XMM9, emitter::XMM10, emitter::XMM11,
     emitter::XMM12, emitter::XMM13, emitter::XMM14, emitter::XMM15},
    {emitter::R9, emitter::R8, emitter::RCX, emitter::RDX, emitter::RSI, emitter::RDI, emitter::RAX,
     emitter::RBX, emitter::RBP, emitter::R12, emitter::R11, emitter::R10}};

AssignmentOrder REG_extra_hard_order = {
    {emitter::XMM7, emitter::XMM6, emitter::XMM5, emitter::XMM4, emitter::XMM3, emitter::XMM2,
     emitter::XMM1, emitter::XMM0, emitter::XMM8, emitter::XMM9},
    {emitter::R9, emitter::RSI, emitter::RDI, emitter::RAX, emitter::RBP, emitter::R12}};

AssignmentOrder REG_temp_only_order = {{emitter::XMM7, emitter::XMM6, emitter::XMM5, emitter::XMM4,
                                        emitter::XMM3, emitter::XMM2, emitter::XMM1, emitter::XMM0},
                                       {emitter::R9, emitter::R8, emitter::RCX, emitter::RDX,
                                        emitter::RSI, emitter::RDI, emitter::RAX}};
std::vector<emitter::Register> allowable_local_var_move_elim = {
    emitter::R9,    emitter::R8,    emitter::RCX,   emitter::RDX,  emitter::RSI,   emitter::RDI,
    emitter::RAX,   emitter::RBX,   emitter::RBP,   emitter::R12,  emitter::R11,   emitter::R10,
    emitter::XMM7,  emitter::XMM6,  emitter::XMM5,  emitter::XMM4, emitter::XMM3,  emitter::XMM2,
    emitter::XMM1,  emitter::XMM0,  emitter::XMM8,  emitter::XMM9, emitter::XMM10, emitter::XMM11,
    emitter::XMM12, emitter::XMM13, emitter::XMM14, emitter::XMM15};

const std::vector<emitter::Register>& get_alloc_order(int var_idx,
                                                      const AllocationInput& in,
                                                      const RACache& cache,
                                                      bool saved_first) {
  bool is_gpr =
      emitter::reg_class_to_hw(cache.iregs.at(var_idx).reg_class) == emitter::HWRegKind::GPR;
  if (in.is_asm_function) {
    if (is_gpr) {
      return REG_temp_only_order.gprs;
    } else {
      return REG_temp_only_order.xmms;
    }
  } else {
    if (torture_test_spills) {
      if (is_gpr) {
        return REG_extra_hard_order.gprs;
      } else {
        return REG_extra_hard_order.xmms;
      }
    }
    if (saved_first) {
      if (is_gpr) {
        return REG_saved_first_order.gprs;
      } else {
        return REG_saved_first_order.xmms;
      }
    } else {
      if (is_gpr) {
        return REG_temp_first_order.gprs;
      } else {
        return REG_temp_only_order.xmms;
      }
    }
  }
}

/*!
 * Determine the instruction where each variable first becomes live.
 * Return value is per-variable, the index of the instruction where it is first live.
 */
std::vector<Range<s32>> find_live_range_instr(const AllocationInput& input,
                                              ControlFlowAnalysisCache& cfa) {
  std::vector<Range<s32>> result;
  result.resize(input.max_vars, Range<s32>(INT32_MAX, INT32_MIN));

  for (auto& block : cfa.basic_blocks) {
    for (auto instr_idx : block.instr_idx) {
      const auto& inst = input.instructions.at(instr_idx);
      for (auto& rd : inst.read) {
        result.at(rd.id).first() = std::min(result.at(rd.id).first(), instr_idx);
        result.at(rd.id).last() = std::max(result.at(rd.id).last(), instr_idx);
      }
      for (auto& wr : inst.write) {
        result.at(wr.id).first() = std::min(result.at(wr.id).first(), instr_idx);
        result.at(wr.id).last() = std::max(result.at(wr.id).last(), instr_idx);
      }
    }

    ASSERT(block.live.size() == block.instr_idx.size());
    for (uint32_t i = 0; i < block.live.size(); i++) {
      for (int j = 0; j < block.live[i].size(); j++) {
        if (block.live[i][j]) {
          int instr_idx = block.instr_idx.at(i);
          result.at(j).first() = std::min(result.at(j).first(), instr_idx);
          result.at(j).last() = std::max(result.at(j).last(), instr_idx);
        }
      }
    }
  }

  // make us alive at any constrained instruction. todo, if this happens is this a sign of an issue
  for (auto& con : input.constraints) {
    if (!con.contrain_everywhere) {
      result.at(con.ireg.id).first() = std::min(result.at(con.ireg.id).first(), con.instr_idx);
      result.at(con.ireg.id).last() = std::max(result.at(con.ireg.id).last(), con.instr_idx);
    }
  }

  return result;
}

/*!
 * Initialize all VarAssignments as unassigned, with the appropriate live range.
 */
std::vector<VarAssignment> initialize_unassigned(const std::vector<Range<s32>>& live_ranges,
                                                 const AllocationInput& input,
                                                 ControlFlowAnalysisCache& cfa) {
  // allocate
  std::vector<VarAssignment> result;
  result.reserve(input.max_vars);
  ASSERT(input.max_vars == (int)live_ranges.size());
  int var_idx = 0;
  for (auto lr : live_ranges) {
    result.emplace_back(lr.first(), lr.last(), var_idx++);
  }

  // now compute the ranges
  for (auto& block : cfa.basic_blocks) {
    // from var use
    for (auto instr_id : block.instr_idx) {
      auto& inst = input.instructions.at(instr_id);
      for (auto& rd : inst.read) {
        result.at(rd.id).mark_live(instr_id);
      }
      for (auto& wr : inst.write) {
        result.at(wr.id).mark_live(instr_id);
      }
    }

    // and liveliness analysis
    ASSERT(block.live.size() == block.instr_idx.size());
    for (uint32_t instr = 0; instr < block.live.size(); instr++) {
      for (int var = 0; var < block.live[instr].size(); var++) {
        if (block.live[instr][var]) {
          result.at(var).mark_live(block.instr_idx.at(instr));
          auto& i = input.instructions.at(block.instr_idx.at(instr));
          if (!i.clobber.empty()) {
            result.at(var).mark_crossing_function();
          }
        }
      }
    }
  }

  // make us alive at any constrained instruction. todo, if this happens is this a sign of an issue
  for (auto& con : input.constraints) {
    if (!con.contrain_everywhere) {
      result.at(con.ireg.id).mark_live(con.instr_idx);
    }
  }

  return result;
}

/*!
 * Populates the control flow analysis cache and:
 * - iregs
 * - used_var
 * - initializes was allocated.
 */
void do_liveliness_analysis(const AllocationInput& input, RACache* cache) {
  find_basic_blocks(&cache->control_flow, input);
  cache->stats.var_count = input.max_vars;
  cache->was_allocated.resize(input.max_vars, false);
  cache->iregs.resize(input.max_vars);
  cache->used_var.resize(input.max_vars);
  cache->stack_ops.resize(input.instructions.size());

  for (auto& instr : input.instructions) {
    for (auto& wr : instr.write) {
      cache->iregs.at(wr.id) = wr;
      cache->used_var.at(wr.id) = true;
    }

    for (auto& rd : instr.read) {
      cache->iregs.at(rd.id) = rd;
      cache->used_var.at(rd.id) = true;
    }
  }

  // phase 1
  for (auto& block : cache->control_flow.basic_blocks) {
    block.live.resize(block.instr_idx.size());
    block.dead.resize(block.instr_idx.size());
    block.analyze_liveliness_phase1(input.instructions);
  }

  // phase 2
  bool changed = false;
  do {
    changed = false;
    for (auto& block : cache->control_flow.basic_blocks) {
      if (block.analyze_liveliness_phase2(cache->control_flow.basic_blocks, input.instructions)) {
        changed = true;
      }
    }
  } while (changed);

  // phase 3
  for (auto& block : cache->control_flow.basic_blocks) {
    block.analyze_liveliness_phase3(cache->control_flow.basic_blocks, input.instructions);
  }

  // phase 4
  auto live_ranges = find_live_range_instr(input, cache->control_flow);
  cache->vars = initialize_unassigned(live_ranges, input, cache->control_flow);

  // cache a list of live ranges which are live at each instruction.
  // filters out unseen lr's as well.
  // this makes instr * lr1 * lr2 loop much faster!
  cache->live_per_instruction.resize(input.instructions.size());
  for (u32 var_idx = 0; var_idx < cache->vars.size(); var_idx++) {
    auto& lr = cache->vars.at(var_idx);
    if (cache->used_var.at(var_idx)) {
      for (int i = lr.first_live(); i <= lr.last_live(); i++) {
        if (lr.live(i)) {
          cache->live_per_instruction.at(i).push_back(var_idx);
        }
      }
    }
  }
  if (input.debug_settings.print_analysis) {
    // print_analysis(input, &cache); TODO
  }

  cache->liveout_per_instr.resize(input.instructions.size());
  for (const auto& block : cache->control_flow.basic_blocks) {
    for (int idx_in_block = 0; idx_in_block < (int)block.instr_idx.size(); idx_in_block++) {
      int intsr_idx = block.instr_idx.at(idx_in_block);
      cache->liveout_per_instr.at(intsr_idx) = block.live.at(idx_in_block);
    }
  }
}

/*!
 * Assign variables with registers constraints to registers.
 */
void do_constrained_alloc(RACache* cache, const AllocationInput& input, bool trace_debug) {
  for (auto& constr : input.constraints) {
    auto var_id = constr.ireg.id;
    if (trace_debug) {
      lg::print("[RA] Apply constraint {}\n", constr.to_string());
    }
    cache->vars.at(var_id).constrain_to_register(constr.desired_register);
  }
}

/*!
 * Helper for safe_overlap.  Assumes a is the first one.
 */
bool safe_overlap_reads_first(const AllocationInput& in,
                              RACache& cache,
                              const VarAssignment& a,
                              const VarAssignment& b,
                              int instr_idx) {
  // it is safe to do something like
  // add var_a, var_b and put var_a and var_b in the same.
  auto& instr = in.instructions.at(instr_idx);
  // should read a, then a goes dead.
  if (!cache.liveout_per_instr.at(instr_idx)[a.var()] && instr.reads(a.var()) &&
      !instr.writes(a.var()) && instr.writes(b.var()) && !instr.reads(b.var())) {
    return true;
  }
  return false;
}

/*!
 * Is it safe for these two to overlap?
 * In the case where you have add var_a, var_b, you can have var_b
 */
bool safe_overlap(const AllocationInput& in,
                  RACache& cache,
                  const VarAssignment& a,
                  const VarAssignment& b,
                  int instr_idx) {
  return safe_overlap_reads_first(in, cache, a, b, instr_idx) ||
         safe_overlap_reads_first(in, cache, b, a, instr_idx);
}

/*!
 * After assigning constrained registers, check to see if all constraints can be satisfied.
 */
bool check_constrained_alloc(RACache* cache, const AllocationInput& in) {
  bool ok = true;

  // first, check that each constraint is actually satisfied.
  // if not, it means that there are two constraints on the same thing.
  for (auto& constr : in.constraints) {
    auto& lr = cache->vars.at(constr.ireg.id);
    for (int i = lr.first_live(); i <= lr.last_live(); i++) {
      if (lr.assigned()) {
        if (!lr.assigned_to_reg(constr.desired_register)) {
          lg::print("[RegAlloc Error] There are conflicting constraints on {}: {} and {}\n",
                    constr.ireg.to_string(), constr.desired_register.print(), "???");
          ok = false;
        }
      }
    }
  }

  // second, check that no constraints are overlapping.
  // this can occur in two cases:
  // - OpenGOAL IR generation is messed up.
  // - programmer actually asked for this.
  // In the second case, rlet will put both rletted variables into the same ireg, so we won't
  // see it from the register allocation.
  for (uint32_t i = 0; i < in.instructions.size(); i++) {
    for (auto idx1 : cache->live_per_instruction.at(i)) {
      auto& lr1 = cache->vars.at(idx1);
      for (auto idx2 : cache->live_per_instruction.at(i)) {
        if (idx1 == idx2) {
          continue;
        }
        auto& lr2 = cache->vars.at(idx2);
        if (lr1.assigned_to_reg() && lr2.assigned_to_reg()) {
          if (lr1.reg() == lr2.reg() && !safe_overlap(in, *cache, lr1, lr2, i)) {
            // todo, this error won't be helpful
            lg::print(
                "[RegAlloc Error] {} Cannot satisfy constraints at instruction {} due to "
                "constraints "
                "on {} and {}, both are assigned to register {}\n",
                in.function_name, i, lr1.var(), lr2.var(), lr1.reg().print());
            ok = false;
          }
        }
      }
    }
  }
  return ok;
}

std::vector<int> var_indices_of_function_crossers_large_to_small(const AllocationInput& input,
                                                                 RACache& cache) {
  std::vector<int> result;

  for (int var_idx = 0; var_idx < input.max_vars; var_idx++) {
    auto& info = cache.vars.at(var_idx);
    if (info.seen() && info.crosses_function()) {
      result.push_back(var_idx);
    }
  }

  std::sort(result.begin(), result.end(), [&](int a, int b) {
    return cache.vars.at(a).range_size() > cache.vars.at(b).range_size();
  });

  return result;
}

template <typename T>
bool vector_contains(const std::vector<T>& vec, const T& obj) {
  for (const auto& x : vec) {
    if (x == obj) {
      return true;
    }
  }
  return false;
}

/*!
 * Is it okay to assign the given variable to the register?
 */
bool check_register_assign_at(const AllocationInput& input,
                              RACache& cache,
                              int var_idx,
                              int instr_idx,
                              emitter::Register reg) {
  // Step 1: check other assignments

  // look at everybody else in the interference graph
  for (int other_idx : cache.live_per_instruction.at(instr_idx)) {
    // don't check ourselves
    if (other_idx == var_idx) {
      continue;
    }

    // we are both live here.
    const auto& other_var = cache.vars.at(other_idx);
    if (other_var.unassigned()) {
      // okay!
    } else if (other_var.assigned_to_reg()) {
      if (other_var.assigned_to_reg(reg)) {
        // assigned to the same register as us!
        if (!safe_overlap(input, cache, cache.vars.at(var_idx), other_var, instr_idx)) {
          return false;
        }
      }
    } else {
      // assigned to stack TODO
      if (other_var.stack_bonus_op_needs_reg(reg, instr_idx)) {
        return false;
      }
    }
  }

  // Step 2: check clobbers and excludes.
  // The model for clobber is that each instruction reads, clobbers, then writes.
  // so in some cases it's okay to clobber.

  // loop over our live range

  const auto& instr = input.instructions.at(instr_idx);

  if (vector_contains(instr.clobber, reg)) {
    // there's two cases where this is okay.
    // 1: if we aren't live-out. The clobber won't clobber anything.
    if (!cache.liveout_per_instr.at(instr_idx)[var_idx]) {
      // ok
    } else {
      // otherwise, we need to write it.
      //        if (!instr.writes(var_idx)) {
      return false;
      //        }
      // 2: we write it after the clobber.
    }
  }

  if (vector_contains(instr.exclude, reg)) {
    return false;
  }

  return true;
}

/*!
 * Is it okay to assign the given variable to the register?
 */
bool check_register_assign(const AllocationInput& input,
                           RACache& cache,
                           int var_idx,
                           emitter::Register reg) {
  auto& this_var = cache.vars.at(var_idx);

  // Step 1: check other assignments

  // loop over our live range
  for (int instr = this_var.first_live(); instr <= this_var.last_live(); instr++) {
    // and leave out the ones where we're dead
    if (!this_var.live(instr)) {
      continue;
    }

    // look at everybody else in the interference graph
    for (int other_idx : cache.live_per_instruction.at(instr)) {
      // don't check ourselves
      if (other_idx == var_idx) {
        continue;
      }

      // we are both live here.
      const auto& other_var = cache.vars.at(other_idx);
      if (other_var.unassigned()) {
        // skip unassigned.
        continue;
      } else if (other_var.assigned_to_reg()) {
        if (other_var.assigned_to_reg(reg)) {
          // assigned to the same register as us!
          if (!safe_overlap(input, cache, this_var, other_var, instr)) {
            ;
            return false;
          }
        }
      } else {
        // assigned to stack TODO
        if (other_var.stack_bonus_op_needs_reg(reg, instr)) {
          return false;
        }
      }
    }
  }

  // Step 2: check clobbers and excludes.
  // The model for clobber is that each instruction reads, clobbers, then writes.
  // so in some cases it's okay to clobber.

  // loop over our live range
  for (int instr_idx = this_var.first_live(); instr_idx <= this_var.last_live(); instr_idx++) {
    const auto& instr = input.instructions.at(instr_idx);
    if (vector_contains(instr.exclude, reg)) {
      return false;
    }

    // and leave out the ones where we're dead
    if (!this_var.live(instr_idx)) {
      continue;
    }

    if (vector_contains(instr.clobber, reg)) {
      // there's two cases where this is okay.
      // 1: if we aren't live-out. The clobber won't clobber anything.
      if (!cache.liveout_per_instr.at(instr_idx)[var_idx]) {
        continue;
      } else {
        // otherwise, we need to write it.
        if (!instr.writes(var_idx)) {
          return false;
        }
        // 2: we write it after the clobber.
      }
    }
  }

  return true;
}

int get_stack_slot_for_var(int var, RACache* cache) {
  int slot_size;
  auto& info = cache->iregs.at(var);
  switch (info.reg_class) {
    case RegClass::INT_128:
      slot_size = 2;
      break;
    case RegClass::VECTOR_FLOAT:
      slot_size = 2;
      break;
    case RegClass::FLOAT:
      slot_size = 1;  // todo - this wastes some space
      break;
    case RegClass::GPR_64:
      slot_size = 1;
      break;
    default:
      ASSERT(false);
  }
  auto kv = cache->var_to_stack_slot.find(var);
  if (kv == cache->var_to_stack_slot.end()) {
    if (slot_size == 2 && (cache->current_stack_slot & 1)) {
      cache->current_stack_slot++;
    }
    auto slot = cache->current_stack_slot;
    cache->current_stack_slot += slot_size;
    cache->var_to_stack_slot[var] = slot;
    return slot;
  } else {
    return kv->second;
  }
}

struct AssignmentSettings {
  bool trace_debug = false;
  bool prefer_saved = false;
  bool only_move_eliminate_assigns = false;
};

bool setup_stack_bonus_ops(const AllocationInput& input,
                           RACache* cache,
                           int var_idx,
                           const AssignmentSettings& settings,
                           int my_slot);

bool try_demote_stack(const AllocationInput& input,
                      RACache* cache,
                      int var_idx,
                      const AssignmentSettings& settings) {
  auto& var = cache->vars.at(var_idx);
  if (!var.assigned_to_reg() || var.locked()) {
    return false;  // not in a reg.
  }

  int my_slot = get_stack_slot_for_var(var_idx, cache);
  var.demote_to_stack(my_slot);
  setup_stack_bonus_ops(input, cache, var_idx, settings, my_slot);
  return true;
}

/*!
 * Stack "bonus" ops load and store arguments from the stack as needed.
 * This may require temporary registers, which are found here.
 */
bool setup_stack_bonus_ops(const AllocationInput& input,
                           RACache* cache,
                           int var_idx,
                           const AssignmentSettings& settings,
                           int my_slot) {
  auto& var = cache->vars.at(var_idx);
  // loop over all possible instruction that might use this var

  // we may retry, so don't add bonus ops until the end (
  struct BonusToAdd {
    StackOp::Op op;
    int instr_idx = -1;
  };
  std::vector<BonusToAdd> bonus_ops;

loop_top:
  bonus_ops.clear();

  for (int instr_idx = var.first_live(); instr_idx <= var.last_live(); instr_idx++) {
    // check out the instruction
    auto& op = input.instructions.at(instr_idx);
    bool is_read = op.reads(var_idx);
    bool is_written = op.writes(var_idx);
    //    lg::print("op {} {} {}\n", instr_idx, is_read, is_written);
    if (!is_read && !is_written) {
      continue;
    }
    // start setting up a bonus op.
    StackOp::Op bonus;
    bonus.reg_class = cache->iregs.at(var_idx).reg_class;
    const auto& order = get_alloc_order(var_idx, input, *cache, false);
    bool success = false;

    const auto& instr = input.instructions.at(instr_idx);
    if (instr.is_move) {
      int check_other_reg = is_written ? instr.read.front().id : instr.write.front().id;
      auto& check_other_var = cache->vars.at(check_other_reg);
      if (check_other_var.assigned_to_reg()) {
        auto reg = check_other_var.reg();
        if (vector_contains(allowable_local_var_move_elim, reg)) {
          if (check_register_assign_at(input, *cache, var_idx, instr_idx, reg)) {
            var.set_stack_slot_reg(reg, instr_idx);
            bonus.reg = reg;
            bonus.slot = my_slot;
            success = true;
            goto success_check;
          }
        }
      }
    }

    for (auto reg : order) {
      if (check_register_assign_at(input, *cache, var_idx, instr_idx, reg)) {
        var.set_stack_slot_reg(reg, instr_idx);
        bonus.reg = reg;
        bonus.slot = my_slot;
        success = true;
        break;
      }
    }

  success_check:
    if (!success) {
      for (auto other_var_idx : cache->live_per_instruction.at(instr_idx)) {
        if (other_var_idx == var_idx) {
          continue;
        }

        if (try_demote_stack(input, cache, other_var_idx, settings)) {
          var.clear_stack_slot_regs();
          goto loop_top;
        }
      }

      lg::print(
          "In function {}, register allocator fell back to a highly inefficient strategy to create "
          "a spill temporary register.\n",
          input.function_name);

      for (int other_var_idx = 0; other_var_idx < input.max_vars; other_var_idx++) {
        if (other_var_idx == var_idx) {
          continue;
        }

        auto& other_var = cache->vars.at(other_var_idx);

        if (other_var.seen() && (other_var.first_live() <= var.last_live()) &&
            (var.first_live() <= other_var.last_live())) {
          if (try_demote_stack(input, cache, other_var_idx, settings)) {
            var.clear_stack_slot_regs();
            goto loop_top;
          }
        }
      }

      return false;
    }

    bonus.load = is_read;
    bonus.store = is_written;

    if (bonus.load || bonus.store) {
      bonus_ops.push_back(BonusToAdd{bonus, instr_idx});
    }
  }

  //  }

  cache->stats.num_spilled_vars++;
  for (auto& op : bonus_ops) {
    if (op.op.load) {
      cache->stats.num_spill_ops++;
    }
    if (op.op.store) {
      cache->stats.num_spill_ops++;
    }
    cache->stack_ops.at(op.instr_idx).ops.push_back(op.op);
  }

  return true;
}

/*!
 * If we fail to put the variable in a register, this function will spill it to the
 stack.
 */
bool handle_failed_register_allocation(const AllocationInput& input,
                                       RACache* cache,
                                       int var_idx,
                                       const AssignmentSettings& settings) {
  // so we couldn't find a register and we need to put this on the stack.  Set on stack:
  auto& var = cache->vars.at(var_idx);
  int my_slot = get_stack_slot_for_var(var_idx, cache);
  var.assign_to_stack(my_slot);
  return setup_stack_bonus_ops(input, cache, var_idx, settings, my_slot);
}

/*!
 * Perform allocation for the given variable!
 */
bool run_assignment_on_var(const AllocationInput& input,
                           RACache* cache,
                           int var_idx,
                           const AssignmentSettings& settings) {
  bool trace = settings.trace_debug;
  auto& var = cache->vars.at(var_idx);
  bool can_be_in_register =
      input.force_on_stack_regs.find(var_idx) == input.force_on_stack_regs.end();
  if (var.unassigned() && var.seen()) {
    bool assigned_to_reg = false;

    // first try move eliminators
    auto& first_instr = input.instructions.at(var.first_live());
    auto& last_instr = input.instructions.at(var.last_live());

    if (first_instr.is_move && can_be_in_register) {
      int other_live_var_idx = first_instr.read.front().id;

      const auto& other_var = cache->vars.at(other_live_var_idx);
      if (other_var.assigned_to_reg() &&
          safe_overlap(input, *cache, var, other_var, var.first_live())) {
        if (vector_contains(allowable_local_var_move_elim, other_var.reg())) {
          bool worked = check_register_assign(input, *cache, var_idx, other_var.reg());
          if (trace) {
            lg::print("m0 trying var {} in {}: {}\n", cache->iregs.at(var_idx).to_string(),
                      other_var.reg().print(), worked);
          }

          if (worked) {
            var.assign_to_register(other_var.reg());
            assigned_to_reg = true;
          }
        }
      }
    }

    if (!assigned_to_reg && last_instr.is_move && can_be_in_register) {
      int other_live_var_idx = last_instr.write.front().id;

      const auto& other_var = cache->vars.at(other_live_var_idx);
      if (trace && var_idx == 5) {
        lg::print("  consider {} {} {} {} [{} {}]\n", other_live_var_idx,
                  other_var.assigned_to_reg(), var.last_live() == other_var.first_live(),
                  safe_overlap(input, *cache, var, other_var, var.last_live()), var.last_live(),
                  other_var.first_live());
      }

      if (other_var.assigned_to_reg() &&
          safe_overlap(input, *cache, var, other_var, var.last_live())) {
        if (vector_contains(allowable_local_var_move_elim, other_var.reg())) {
          bool worked = check_register_assign(input, *cache, var_idx, other_var.reg());
          if (trace) {
            lg::print("m1 trying var {} in {}: {}\n", cache->iregs.at(var_idx).to_string(),
                      other_var.reg().print(), worked);
          }

          if (worked) {
            var.assign_to_register(other_var.reg());
            assigned_to_reg = true;
          }
        }
      }
    }

    if (!assigned_to_reg && !settings.only_move_eliminate_assigns && can_be_in_register) {
      const auto& assign_order = get_alloc_order(var_idx, input, *cache, settings.prefer_saved);
      for (auto& reg : assign_order) {
        bool worked = check_register_assign(input, *cache, var_idx, reg);
        if (trace) {
          lg::print("m2 trying var {} in {}: {}\n", cache->iregs.at(var_idx).to_string(),
                    reg.print(), worked);
        }
        if (worked) {
          var.assign_to_register(reg);
          assigned_to_reg = true;
          break;
        }
      }
    }

    if (!assigned_to_reg && !settings.only_move_eliminate_assigns) {
      assigned_to_reg = handle_failed_register_allocation(input, cache, var_idx, settings);
      if (!assigned_to_reg) {
        cache->failed_alloc = true;
      }
    }
    return assigned_to_reg;
  } else {
    return false;
  }
}

int run_assignment_on_some_vars(const AllocationInput& input,
                                RACache* cache,
                                const std::vector<int>& vars_to_alloc,
                                const AssignmentSettings& settings) {
  cache->stats.assign_passes++;
  int assigned_count = 0;

  for (auto var_idx : vars_to_alloc) {
    if (run_assignment_on_var(input, cache, var_idx, settings)) {
      assigned_count++;
    }
  }
  return assigned_count;
}

int run_assignment_on_all_vars(const AllocationInput& input,
                               RACache* cache,
                               const AssignmentSettings& settings) {
  cache->stats.assign_passes++;
  int assigned_count = 0;

  for (int var_idx = 0; var_idx < input.max_vars; var_idx++) {
    if (run_assignment_on_var(input, cache, var_idx, settings)) {
      assigned_count++;
    }
  }
  return assigned_count;
}
}  // namespace

AllocationResult allocate_registers_v2(const AllocationInput& input) {
  AllocationResult result;

  // stores internal allocator state
  RACache cache;

  // debug print
  if (input.debug_settings.print_input) {
    print_allocate_input(input);
  }

  // STEP 1: Analysis:
  do_liveliness_analysis(input, &cache);

  // STEP 2: Constrained allocation.
  do_constrained_alloc(&cache, input, input.debug_settings.trace_debug_constraints);
  check_constrained_alloc(&cache, input);
  if (!check_constrained_alloc(&cache, input)) {
    result.ok = false;
    lg::print("[RegAlloc Error] Register allocation has failed due to bad constraints.\n");
    return result;
  }

  if (torture_test_spills) {
    AssignmentSettings pick_up_new_settings;
    run_assignment_on_all_vars(input, &cache, pick_up_new_settings);
  } else {
    // STEP 3: Function Crossing Allocation.
    AssignmentSettings function_cross_settings;
    function_cross_settings.only_move_eliminate_assigns = false;
    function_cross_settings.prefer_saved = true;
    auto func_cross_vars = var_indices_of_function_crossers_large_to_small(input, cache);
    run_assignment_on_some_vars(input, &cache, func_cross_vars, function_cross_settings);

    AssignmentSettings branch_out_settings;
    branch_out_settings.only_move_eliminate_assigns = true;

    AssignmentSettings pick_up_new_settings;

    int loop_count = 1;
    while (loop_count) {
      loop_count = run_assignment_on_all_vars(input, &cache, branch_out_settings);
    }

    run_assignment_on_all_vars(input, &cache, pick_up_new_settings);
  }

  result.ok = true;

  if (cache.failed_alloc) {
    result.ok = false;
    return result;
  }
  for (int var_idx = 0; var_idx < input.max_vars; var_idx++) {
    auto& var = cache.vars.at(var_idx);
    if (var.seen() && !var.assigned()) {
      //      lg::print("av2: {} failed\n", input.function_name);
      result.ok = false;
      return result;
    }
  }

  result.needs_aligned_stack_for_spills = cache.used_stack;
  result.stack_slots_for_spills = cache.current_stack_slot;
  result.stack_slots_for_vars = input.stack_slots_for_stack_vars;

  // check for use of saved registers
  for (auto sr : emitter::gRegInfo.get_all_saved()) {
    bool uses_sr = false;
    for (auto& lr : cache.vars) {
      for (int instr_idx = lr.first_live(); instr_idx <= lr.last_live(); instr_idx++) {
        if (lr.assigned_to_reg()) {
          if (lr.assigned_to_reg(sr)) {
            uses_sr = true;
            break;
          }
        } else if (lr.assigned_to_stack()) {
          if (lr.stack_bonus_op_needs_reg(sr, instr_idx)) {
            uses_sr = true;
            break;
          }
        }
      }
      if (uses_sr) {
        break;
      }
    }
    if (uses_sr) {
      result.used_saved_regs.push_back(sr);
    }
  }
  // result.ass_as_ranges = std::move(cache.live_ranges);
  for (auto& lr : cache.vars) {
    if (!lr.seen()) {
      result.ass_as_ranges.push_back(AssignmentRange(0, {}, {}));
    } else {
      result.ass_as_ranges.push_back(
          AssignmentRange(lr.first_live(), lr.live_vector(), lr.make_assignment_vector()));
    }
  }
  result.stack_ops = std::move(cache.stack_ops);

  // final result print
  if (input.debug_settings.print_result) {
    print_result(input, result);
  }

  result.num_spilled_vars = cache.stats.num_spilled_vars;
  result.num_spills = cache.stats.num_spill_ops;

  return result;
}
