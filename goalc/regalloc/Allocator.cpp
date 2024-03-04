/*!
 * @file Allocator.cpp
 * Implementation of register allocation algorithms
 */

#include "Allocator.h"

#include <stdexcept>

#include "common/log/log.h"

#include "goalc/regalloc/allocator_interface.h"

#include "fmt/core.h"

std::string LiveInfo::print_assignment() {
  std::string result = "Assignment for var " + std::to_string(var) + "\n";
  for (uint32_t i = 0; i < assignment.size(); i++) {
    result += fmt::format("i[{:3d}] {}\n", i + min, assignment.at(i).to_string());
  }
  return result;
}

namespace {

/*!
 * Setup live_ranges. Must have found where iregs are live first.
 */
void compute_live_ranges(RegAllocCache* cache, const AllocationInput& in) {
  // then resize live ranges to the correct size
  cache->live_ranges.resize(cache->max_var, LiveInfo(in.instructions.size(), 0));

  // now compute the ranges
  for (auto& block : cache->control_flow.basic_blocks) {
    // from var use
    for (auto instr_id : block.instr_idx) {
      auto& inst = in.instructions.at(instr_id);

      for (auto& lst : {inst.read, inst.write}) {
        for (auto& x : lst) {
          cache->live_ranges.at(x.id).add_live_instruction(instr_id);
        }
      }
    }

    // and liveliness analysis
    ASSERT(block.live.size() == block.instr_idx.size());
    for (uint32_t i = 0; i < block.live.size(); i++) {
      for (int j = 0; j < block.live[i].size(); j++) {
        if (block.live[i][j]) {
          cache->live_ranges.at(j).add_live_instruction(block.instr_idx.at(i));
        }
      }
    }
  }

  // make us alive at any constrained instruction. todo, if this happens is this a sign of an issue
  for (auto& con : in.constraints) {
    if (!con.contrain_everywhere) {
      cache->live_ranges.at(con.ireg.id).add_live_instruction(con.instr_idx);
    }
  }
}
}  // namespace

/*!
 * Analysis pass to find out where registers are live. Must have found basic blocks first.
 */
void analyze_liveliness(RegAllocCache* cache, const AllocationInput& in) {
  cache->max_var = in.max_vars;
  cache->was_colored.resize(cache->max_var, false);
  cache->iregs.resize(cache->max_var);

  for (auto& instr : in.instructions) {
    for (auto& wr : instr.write) {
      cache->iregs.at(wr.id) = wr;
    }

    for (auto& rd : instr.read) {
      cache->iregs.at(rd.id) = rd;
    }
  }

  // phase 1
  for (auto& block : cache->control_flow.basic_blocks) {
    block.live.resize(block.instr_idx.size());
    block.dead.resize(block.instr_idx.size());
    block.analyze_liveliness_phase1(in.instructions);
  }

  // phase 2
  bool changed = false;
  do {
    changed = false;
    for (auto& block : cache->control_flow.basic_blocks) {
      if (block.analyze_liveliness_phase2(cache->control_flow.basic_blocks, in.instructions)) {
        changed = true;
      }
    }
  } while (changed);

  // phase 3
  for (auto& block : cache->control_flow.basic_blocks) {
    block.analyze_liveliness_phase3(cache->control_flow.basic_blocks, in.instructions);
  }

  // phase 4
  compute_live_ranges(cache, in);

  // final setup!
  for (size_t i = 0; i < cache->live_ranges.size(); i++) {
    cache->live_ranges.at(i).prepare_for_allocation(i);
  }
  cache->stack_ops.resize(in.instructions.size());

  // cache a list of live ranges which are live at each instruction.
  // filters out unseen lr's as well.
  // this makes instr * lr1 * lr2 loop much faster!
  cache->live_ranges_by_instr.resize(in.instructions.size());
  for (u32 lr_idx = 0; lr_idx < cache->live_ranges.size(); lr_idx++) {
    auto& lr = cache->live_ranges.at(lr_idx);
    if (lr.seen) {
      for (int i = lr.min; i <= lr.max; i++) {
        if (lr.is_live_at_instr(i)) {
          cache->live_ranges_by_instr.at(i).push_back(lr_idx);
        }
      }
    }
  }
}

/*!
 * Assign registers which are constrained. If constraints are inconsistent, won't succeed in
 * satisfying them (of course), and won't error either. Use check_constrained_alloc to confirm
 * that all constraints are then satisfied.
 */
void do_constrained_alloc(RegAllocCache* cache, const AllocationInput& in, bool trace_debug) {
  for (auto& constr : in.constraints) {
    auto var_id = constr.ireg.id;
    if (trace_debug) {
      lg::print("[RA] Apply constraint {}\n", constr.to_string());
    }
    if (constr.contrain_everywhere) {
      cache->live_ranges.at(var_id).constrain_everywhere(constr.desired_register);
    } else {
      cache->live_ranges.at(var_id).constrain_at_one(constr.instr_idx, constr.desired_register);
    }
  }
}

/*!
 * Check to run after do_constrained_alloc to see if the constraints could actually be satisfied.
 */
bool check_constrained_alloc(RegAllocCache* cache, const AllocationInput& in) {
  bool ok = true;
  for (auto& constr : in.constraints) {
    if (constr.contrain_everywhere) {
      auto& lr = cache->live_ranges.at(constr.ireg.id);
      for (int i = lr.min; i <= lr.max; i++) {
        if (!lr.conflicts_at(i, constr.desired_register)) {
          lg::print("[RegAlloc Error] There are conflicting constraints on {}: {} and {}\n",
                    constr.ireg.to_string(), constr.desired_register.print(),
                    cache->live_ranges.at(constr.ireg.id).get(i).to_string());
          ok = false;
        }
      }
    } else {
      if (!cache->live_ranges.at(constr.ireg.id)
               .conflicts_at(constr.instr_idx, constr.desired_register)) {
        lg::print("[RegAlloc Error] There are conflicting constraints on {}: {} and {}\n",
                  constr.ireg.to_string(), constr.desired_register.print(),
                  cache->live_ranges.at(constr.ireg.id).get(constr.instr_idx).to_string());
        ok = false;
      }
    }
  }

  for (uint32_t i = 0; i < in.instructions.size(); i++) {
    for (auto idx1 : cache->live_ranges_by_instr.at(i)) {
      auto& lr1 = cache->live_ranges.at(idx1);
      for (auto idx2 : cache->live_ranges_by_instr.at(i)) {
        if (idx1 == idx2) {
          continue;
        }
        auto& lr2 = cache->live_ranges.at(idx2);
        // if lr1 is assigned...
        auto& ass1 = lr1.get(i);
        if (ass1.kind != Assignment::Kind::UNASSIGNED) {
          auto& ass2 = lr2.get(i);
          if (ass1.occupies_same_reg(ass2)) {
            // todo, this error won't be helpful
            lg::print(
                "[RegAlloc Error] Cannot satisfy constraints at instruction {} due to constraints "
                "on {} and {}\n",
                i, lr1.var, lr2.var);
            ok = false;
          }
        }
      }
    }
  }
  return ok;
}

namespace {

/*!
 * Assign variable to register. Don't check if its safe. If it's already assigned, and this would
 * change that assignment, throw.
 */
void assign_var_no_check(int var, Assignment ass, RegAllocCache* cache) {
  cache->live_ranges.at(var).assign_no_overwrite(ass);
}

/*!
 * Can var be assigned to ass?
 */
bool can_var_be_assigned(int var,
                         Assignment ass,
                         RegAllocCache* cache,
                         const AllocationInput& in,
                         int debug_trace) {
  // our live range:
  auto& lr = cache->live_ranges.at(var);
  // check against all other live ranges:
  for (int instr = lr.min; instr <= lr.max; instr++) {
    for (int other_idx : cache->live_ranges_by_instr.at(instr)) {
      auto& other_lr = cache->live_ranges.at(other_idx);
      if (other_lr.var == var) {
        continue;
      }
      // LR's overlap
      if (/*(instr != other_lr.max) && */ other_lr.conflicts_at(instr, ass)) {
        bool allowed_by_move_eliminator = false;
        if (move_eliminator) {
          if (enable_fancy_coloring) {
            if (lr.dies_next_at_instr(instr) && other_lr.becomes_live_at_instr(instr) &&
                (allow_read_write_same_reg || in.instructions.at(instr).is_move)) {
              allowed_by_move_eliminator = true;
            }

            if (lr.becomes_live_at_instr(instr) && other_lr.dies_next_at_instr(instr) &&
                (allow_read_write_same_reg || in.instructions.at(instr).is_move)) {
              allowed_by_move_eliminator = true;
            }
          } else {
            // case to allow rename (from us to them)
            if (instr == lr.max && instr == other_lr.min && in.instructions.at(instr).is_move) {
              allowed_by_move_eliminator = true;
            }

            if (instr == lr.min && instr == other_lr.min && in.instructions.at(instr).is_move) {
              allowed_by_move_eliminator = true;
            }
          }
        }

        if (!allowed_by_move_eliminator) {
          if (debug_trace >= 1) {
            printf("at idx %d, %s conflicts\n", instr, other_lr.print_assignment().c_str());
          }

          return false;
        }
      }
    }
  }

  // can clobber on the last one or first one - check that we don't interfere with a clobber
  for (int instr = lr.min + 1; instr <= lr.max - 1; instr++) {
    for (auto clobber : in.instructions.at(instr).clobber) {
      if (ass.occupies_reg(clobber)) {
        if (debug_trace >= 1) {
          printf("at idx %d clobber\n", instr);
        }

        return false;
      }
    }
  }

  for (int instr = lr.min; instr <= lr.max; instr++) {
    for (auto exclusive : in.instructions.at(instr).exclude) {
      if (ass.occupies_reg(exclusive)) {
        if (debug_trace >= 1) {
          printf("at idx %d exclusive conflict\n", instr);
        }

        return false;
      }
    }
  }

  // check we don't violate any others.
  for (int instr = lr.min; instr <= lr.max; instr++) {
    if (lr.has_constraint && lr.assignment.at(instr - lr.min).is_assigned()) {
      if (!(ass.occupies_same_reg(lr.assignment.at(instr - lr.min)))) {
        if (debug_trace >= 1) {
          printf("at idx %d self bad (%s) (%s)\n", instr,
                 lr.assignment.at(instr - lr.min).to_string().c_str(), ass.to_string().c_str());
        }

        return false;
      }
    }
  }

  return true;
}

bool assignment_ok_at(int var,
                      int idx,
                      Assignment ass,
                      RegAllocCache* cache,
                      const AllocationInput& in,
                      int debug_trace) {
  auto& lr = cache->live_ranges.at(var);
  for (auto other_idx : cache->live_ranges_by_instr.at(idx)) {
    auto& other_lr = cache->live_ranges.at(other_idx);
    if (other_lr.var == var) {
      continue;
    }

    if (/*(idx != other_lr.max) &&*/ other_lr.conflicts_at(idx, ass)) {
      bool allowed_by_move_eliminator = false;
      if (move_eliminator) {
        if (enable_fancy_coloring) {
          if (lr.dies_next_at_instr(idx) && other_lr.becomes_live_at_instr(idx) &&
              (allow_read_write_same_reg || in.instructions.at(idx).is_move)) {
            allowed_by_move_eliminator = true;
          }

          if (lr.becomes_live_at_instr(idx) && other_lr.dies_next_at_instr(idx) &&
              (allow_read_write_same_reg || in.instructions.at(idx).is_move)) {
            allowed_by_move_eliminator = true;
          }
        } else {
          // case to allow rename (from us to them)
          if (idx == lr.max && idx == other_lr.min && in.instructions.at(idx).is_move) {
            allowed_by_move_eliminator = true;
          }

          if (idx == lr.min && idx == other_lr.min && in.instructions.at(idx).is_move) {
            allowed_by_move_eliminator = true;
          }
        }
      }

      if (!allowed_by_move_eliminator) {
        if (debug_trace >= 2) {
          printf("at idx %d, %s conflicts\n", idx, other_lr.print_assignment().c_str());
        }
        return false;
      }
    }
  }

  // check we aren't violating a clobber
  if (idx != lr.min && idx != lr.max) {
    for (auto clobber : in.instructions.at(idx).clobber) {
      if (ass.occupies_reg(clobber)) {
        if (debug_trace >= 2) {
          printf("at idx %d clobber\n", idx);
        }

        return false;
      }
    }
  }

  for (auto exclusive : in.instructions.at(idx).exclude) {
    if (ass.occupies_reg(exclusive)) {
      if (debug_trace >= 2) {
        printf("at idx %d exclusive conflict\n", idx);
      }

      return false;
    }
  }

  // check we aren't violating ourselves
  if (lr.assignment.at(idx - lr.min).is_assigned()) {
    if (!(ass.occupies_same_reg(lr.assignment.at(idx - lr.min)))) {
      if (debug_trace >= 2) {
        printf("at idx %d self bad\n", idx);
      }

      return false;
    }
  }

  return true;
}

bool try_assignment_for_var(int var,
                            Assignment ass,
                            RegAllocCache* cache,
                            const AllocationInput& in,
                            int debug_trace) {
  if (can_var_be_assigned(var, ass, cache, in, debug_trace)) {
    assign_var_no_check(var, ass, cache);
    return true;
  }
  return false;
}

int get_stack_slot_for_var(int var, RegAllocCache* cache) {
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

const std::vector<emitter::Register>& get_default_alloc_order_for_var_spill(int v,
                                                                            RegAllocCache* cache) {
  auto& info = cache->iregs.at(v);
  ASSERT(info.reg_class != RegClass::INVALID);
  auto hw_kind = emitter::reg_class_to_hw(info.reg_class);
  if (hw_kind == emitter::HWRegKind::GPR) {
    return emitter::gRegInfo.get_gpr_spill_alloc_order();
  } else if (hw_kind == emitter::HWRegKind::XMM) {
    return emitter::gRegInfo.get_xmm_spill_alloc_order();
  } else {
    throw std::runtime_error("Unsupported HWRegKind");
  }
}

const std::vector<emitter::Register>& get_default_alloc_order_for_var(int v,
                                                                      RegAllocCache* cache,
                                                                      bool get_all) {
  auto& info = cache->iregs.at(v);
  ASSERT(info.reg_class != RegClass::INVALID);
  auto hw_kind = emitter::reg_class_to_hw(info.reg_class);
  if (hw_kind == emitter::HWRegKind::GPR || hw_kind == emitter::HWRegKind::INVALID) {
    if (!get_all && cache->is_asm_func) {
      return emitter::gRegInfo.get_gpr_temp_alloc_order();
    } else {
      return emitter::gRegInfo.get_gpr_alloc_order();
    }
  } else if (hw_kind == emitter::HWRegKind::XMM) {
    if (!get_all && cache->is_asm_func) {
      return emitter::gRegInfo.get_xmm_temp_alloc_order();
    } else {
      return emitter::gRegInfo.get_xmm_alloc_order();
    }
  } else {
    throw std::runtime_error("Unsupported HWRegKind");
  }
}

bool try_spill_coloring(int var, RegAllocCache* cache, const AllocationInput& in, int debug_trace) {
  // todo, reject flagged "unspillables"
  if (debug_trace >= 1) {
    printf("---- SPILL VAR %d ----\n", var);
  }

  auto& lr = cache->live_ranges.at(var);

  // possibly get a hint assignment
  Assignment hint_assignment;
  hint_assignment.kind = Assignment::Kind::UNASSIGNED;

  // loop over live range
  for (int instr = lr.min; instr <= lr.max; instr++) {
    //    bonus_instructions.at(instr).clear();
    StackOp::Op bonus;
    bonus.reg_class = cache->iregs.at(var).reg_class;

    // we may have a constraint in here
    auto& current_assignment = lr.assignment.at(instr - lr.min);

    auto& op = in.instructions.at(instr);
    bool is_read = op.reads(var);
    bool is_written = op.writes(var);

    // we have a constraint!
    if (current_assignment.is_assigned()) {
      if (debug_trace >= 2) {
        printf("  [%02d] already assigned %s\n", instr, current_assignment.to_string().c_str());
      }

      // remember this assignment as a hint for later
      hint_assignment = current_assignment;
      // check that this assignment is ok
      if (!assignment_ok_at(var, instr, current_assignment, cache, in, debug_trace)) {
        // this shouldn't be possible with feasible constraints
        printf("-- SPILL FAILED -- IMPOSSIBLE CONSTRAINT @ %d %s. This is likely a RegAlloc bug!\n",
               instr, current_assignment.to_string().c_str());
        ASSERT(false);
        return false;
      }

      // flag it as spilled, but currently in a GPR.
      current_assignment.spilled = true;
      current_assignment.stack_slot = get_stack_slot_for_var(var, cache);
      bonus.reg = current_assignment.reg;
    } else {
      // not assigned.
      if (debug_trace >= 1) {
        printf("  [%02d] nya rd? %d wr? %d\n", instr, is_read, is_written);
      }

      // We'd like to keep it on the stack if possible
      Assignment spill_assignment;
      spill_assignment.spilled = true;
      spill_assignment.kind = Assignment::Kind::STACK;
      spill_assignment.reg = -1;  // for now

      // needs a temp register
      if (is_read || is_written) {
        // we need to put it in a register here!
        // first check if the hint works?
        // todo floats?
        if (hint_assignment.kind == Assignment::Kind::REGISTER) {
          if (debug_trace >= 2) {
            printf("   try hint %s\n", hint_assignment.to_string().c_str());
          }

          if (assignment_ok_at(var, instr, hint_assignment, cache, in, debug_trace)) {
            // it's ok!
            if (debug_trace >= 2) {
              printf("   it worked!\n");
            }
            spill_assignment.reg = hint_assignment.reg;
          }
        }

        // hint didn't work
        // auto reg_order = get_default_reg_alloc_order();
        auto reg_order = get_default_alloc_order_for_var_spill(var, cache);
        if (spill_assignment.reg == -1) {
          for (auto reg : reg_order) {
            Assignment ass;
            ass.kind = Assignment::Kind::REGISTER;
            ass.reg = reg;
            if (debug_trace >= 2) {
              printf("  try %s\n", ass.to_string().c_str());
            }

            if (assignment_ok_at(var, instr, ass, cache, in, debug_trace)) {
              if (debug_trace >= 2) {
                printf("  it worked!\n");
              }
              spill_assignment.reg = ass.reg;
              break;
            }
          }
        }

        if (spill_assignment.reg == -1) {
          ASSERT_MSG(false, "SPILLING FAILED BECAUSE WE COULDN'T FIND A TEMP REGISTER!");
          return false;
        }

        // mark that it's in a GPR!
        spill_assignment.kind = Assignment::Kind::REGISTER;
      }  // end need temp reg
      spill_assignment.stack_slot = get_stack_slot_for_var(var, cache);
      lr.assignment.at(instr - lr.min) = spill_assignment;
      bonus.reg = spill_assignment.reg;
      bonus.slot = spill_assignment.stack_slot;
    }  // end not constrained

    bonus.slot = get_stack_slot_for_var(var, cache);
    bonus.load = is_read;
    bonus.store = is_written;

    if (bonus.load || bonus.store) {
      cache->stack_ops.at(instr).ops.push_back(bonus);
      if (bonus.load) {
        cache->stats.num_spill_ops++;
      }
      if (bonus.store) {
        cache->stats.num_spill_ops++;
      }
    }
  }
  return true;
}

template <typename T>
bool in_vec(const std::vector<T>& vec, const T& obj) {
  for (const auto& x : vec) {
    if (x == obj)
      return true;
  }
  return false;
}

bool do_allocation_for_var(int var,
                           RegAllocCache* cache,
                           const AllocationInput& in,
                           int debug_trace) {
  bool can_be_in_register = in.force_on_stack_regs.find(var) == in.force_on_stack_regs.end();
  bool colored = false;

  if (can_be_in_register) {
    // first, let's see if there's a hint...
    auto& lr = cache->live_ranges.at(var);
    if (lr.best_hint.is_assigned()) {
      colored = try_assignment_for_var(var, lr.best_hint, cache, in, debug_trace);
      if (debug_trace >= 2) {
        printf("var %d reg %s ? %d\n", var, lr.best_hint.to_string().c_str(), colored);
      }
    }

    auto reg_order = get_default_alloc_order_for_var(var, cache, false);
    auto& all_reg_order = get_default_alloc_order_for_var(var, cache, true);

    // todo, try other regs..
    if (!colored && move_eliminator) {
      auto& first_instr = in.instructions.at(lr.min);
      auto& last_instr = in.instructions.at(lr.max);

      if (!colored && last_instr.is_move) {
        auto& possible_coloring = cache->live_ranges.at(last_instr.write.front().id).get(lr.max);
        if (possible_coloring.is_assigned() && in_vec(all_reg_order, possible_coloring.reg)) {
          colored = try_assignment_for_var(var, possible_coloring, cache, in, debug_trace);
        }
      }

      if (!colored && first_instr.is_move) {
        auto& possible_coloring = cache->live_ranges.at(first_instr.read.front().id).get(lr.min);
        if (possible_coloring.is_assigned() && in_vec(all_reg_order, possible_coloring.reg)) {
          colored = try_assignment_for_var(var, possible_coloring, cache, in, debug_trace);
        }
      }
    }

    // auto reg_order = get_default_reg_alloc_order();

    for (auto reg : reg_order) {
      if (colored)
        break;
      Assignment ass;
      ass.kind = Assignment::Kind::REGISTER;
      ass.reg = reg;
      colored = try_assignment_for_var(var, ass, cache, in, debug_trace);
      if (debug_trace >= 1) {
        printf("var %d reg %s ? %d\n", var, ass.to_string().c_str(), colored);
      }
    }
  }

  if (!colored) {
    colored = try_spill_coloring(var, cache, in, debug_trace);
    if (colored) {
      cache->used_stack = true;
    }
  }

  // todo, try spilling
  if (!colored) {
    printf("[ERROR] var %d could not be colored:\n%s\n", var,
           cache->live_ranges.at(var).print_assignment().c_str());

    return false;
  } else {
    if (debug_trace >= 2) {
      printf("Colored var %d\n", var);
    }

    cache->was_colored.at(var) = true;
    return true;
  }
}

}  // namespace

bool run_allocator(RegAllocCache* cache, const AllocationInput& in, int debug_trace) {
  // here we allocate
  std::vector<int> allocation_order;
  for (uint32_t i = 0; i < cache->live_ranges.size(); i++) {
    if (cache->live_ranges.at(i).seen && cache->live_ranges.at(i).has_constraint) {
      allocation_order.push_back(i);
    }
  }

  for (uint32_t i = 0; i < cache->live_ranges.size(); i++) {
    if (cache->live_ranges.at(i).seen && !cache->live_ranges.at(i).has_constraint) {
      allocation_order.push_back(i);
    }
  }

  for (int var : allocation_order) {
    if (!do_allocation_for_var(var, cache, in, debug_trace)) {
      return false;
    }
  }
  return true;
}

namespace {
/*!
 * Print out the state of the RegAllocCache after doing analysis.
 */
void print_analysis(const AllocationInput& in, RegAllocCache* cache) {
  lg::print("[RegAlloc] Basic Blocks\n");
  lg::print("-----------------------------------------------------------------\n");
  for (auto& b : cache->control_flow.basic_blocks) {
    lg::print("{}\n", b.print(in.instructions));
  }

  printf("[RegAlloc] Alive Info\n");
  printf("-----------------------------------------------------------------\n");
  // align to where we start putting live stuff
  printf("      %30s    ", "");
  for (int i = 0; i < cache->max_var; i++) {
    printf("%2d ", i);
  }
  printf("\n");
  printf("_________________________________________________________________\n");
  for (uint32_t i = 0; i < in.instructions.size(); i++) {
    std::vector<bool> ids_live;
    std::string lives;

    ids_live.resize(cache->max_var, false);

    for (int j = 0; j < cache->max_var; j++) {
      if (cache->live_ranges.at(j).is_live_at_instr(i)) {
        ids_live.at(j) = true;
      }
    }

    for (uint32_t j = 0; j < ids_live.size(); j++) {
      if (ids_live[j]) {
        char buff[256];
        sprintf(buff, "%2d ", (int)j);
        lives.append(buff);
      } else {
        lives.append(".. ");
      }
    }

    if (in.debug_instruction_names.size() == in.instructions.size()) {
      std::string code_str = in.debug_instruction_names.at(i);
      if (code_str.length() >= 50) {
        code_str = code_str.substr(0, 48);
        code_str.push_back('~');
      }
      printf("[%03d] %30s -> %s\n", (int)i, code_str.c_str(), lives.c_str());
    } else {
      printf("[%03d] %30s -> %s\n", (int)i, "???", lives.c_str());
    }
  }
}
}  // namespace

/*!
 * The top-level register allocation algorithm!
 */
AllocationResult allocate_registers(const AllocationInput& input) {
  AllocationResult result;
  RegAllocCache cache;
  cache.is_asm_func = input.is_asm_function;

  // if desired, print input for debugging.
  if (input.debug_settings.print_input) {
    print_allocate_input(input);
  }

  // first step is analysis
  find_basic_blocks(&cache.control_flow, input);
  analyze_liveliness(&cache, input);
  if (input.debug_settings.print_analysis) {
    print_analysis(input, &cache);
  }

  // do constraints first, to get them out of the way
  do_constrained_alloc(&cache, input, input.debug_settings.trace_debug_constraints);
  // the user may have specified invalid constraints, so we should attempt to find conflicts now
  // rather than having the register allocation mysteriously fail later on or silently ignore a
  // constraint.
  if (!check_constrained_alloc(&cache, input)) {
    result.ok = false;
    lg::print("[RegAlloc Error] Register allocation has failed due to bad constraints.\n");
    return result;
  }

  // do the allocations!
  if (!run_allocator(&cache, input, input.debug_settings.allocate_log_level)) {
    result.ok = false;
    lg::print("[RegAlloc Error] Register allocation has failed.\n");
    return result;
  }

  // prepare the result
  result.ok = true;
  result.needs_aligned_stack_for_spills = cache.used_stack;
  result.stack_slots_for_spills = cache.current_stack_slot;
  result.stack_slots_for_vars = input.stack_slots_for_stack_vars;

  // check for use of saved registers
  for (auto sr : emitter::gRegInfo.get_all_saved()) {
    bool uses_sr = false;
    for (auto& lr : cache.live_ranges) {
      for (int instr_idx = lr.min; instr_idx <= lr.max; instr_idx++) {
        if (lr.get(instr_idx).reg == sr) {
          uses_sr = true;
          break;
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
  for (auto& lr : cache.live_ranges) {
    result.ass_as_ranges.push_back(AssignmentRange(lr.min, lr.is_alive, lr.assignment));
  }
  result.stack_ops = std::move(cache.stack_ops);

  // final result print
  if (input.debug_settings.print_result) {
    print_result(input, result);
  }

  result.num_spills = cache.stats.num_spill_ops;

  return result;
}
