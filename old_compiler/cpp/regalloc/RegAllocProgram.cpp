#include <algorithm>
#include <cassert>
#include "RegAllocProgram.h"
#include "logger/Logger.h"
#include "codegen/x86.h"

//#define LOG(...) gLogger.log(MSG_WARN, __VA_ARGS__)
#define LOG(...) \
  do {           \
  } while (0)

void RegAllocProgram::find_basic_blocks() {
  std::vector<int> dividers;

  dividers.push_back(0);
  dividers.push_back(instructions.size());

  // loop over instructions, finding jump targets
  for (uint32_t i = 0; i < instructions.size(); i++) {
    auto& instr = instructions[i];
    if (!instr.jumps.empty()) {
      dividers.push_back(i + 1);
      for (auto dest : instr.jumps) {
        dividers.push_back(dest);
      }
    }
  }

  // sort dividers, and make blocks
  std::sort(dividers.begin(), dividers.end(), [](int a, int b) { return a < b; });

  for (uint32_t i = 0; i < dividers.size() - 1; i++) {
    if (dividers[i] != dividers[i + 1]) {
      // new basic block!
      RegAllocBasicBlock block;
      for (int j = dividers[i]; j < dividers[i + 1]; j++) {
        block.instr_idx.push_back(j);
      }

      block.idx = basic_blocks.size();
      basic_blocks.push_back(block);
    }
  }

  if (!basic_blocks.empty()) {
    basic_blocks.front().is_entry = true;
    basic_blocks.back().is_exit = true;
  }

  auto find_basic_block_to_target = [&](int instr) {
    bool found = false;
    uint32_t result = -1;
    for (uint32_t i = 0; i < basic_blocks.size(); i++) {
      if (!basic_blocks[i].instr_idx.empty() && basic_blocks[i].instr_idx.front() == instr) {
        assert(!found);
        found = true;
        result = i;
      }
    }
    if (!found) {
      printf("couldn't find baisc block beginning with instr %d of %ld\n", instr,
             instructions.size());
    }
    assert(found);
    return result;
  };

  // link blocks
  for (auto& block : basic_blocks) {
    assert(!block.instr_idx.empty());
    auto& last_instr = instructions.at(block.instr_idx.back());
    if (last_instr.fallthrough) {
      // try to link to next block:
      int next_idx = block.idx + 1;
      if (next_idx < (int)basic_blocks.size()) {
        basic_blocks.at(next_idx).pred.push_back(block.idx);
        block.succ.push_back(next_idx);
      }
    }
    for (auto target : last_instr.jumps) {
      basic_blocks.at(find_basic_block_to_target(target)).pred.push_back(block.idx);
      block.succ.push_back(find_basic_block_to_target(target));
    }
  }
}

void RegAllocProgram::analyze_block_liveliness(int n_vars) {
  max_var = n_vars;
  was_colored.resize(n_vars, false);
  coloring_input.resize(n_vars);

  for (auto& instr : instructions) {
    for (auto& wr : instr.write) {
      coloring_input.at(wr.id) = wr;
    }

    for (auto& rd : instr.read) {
      coloring_input.at(rd.id) = rd;
    }
  }

  // phase 1
  for (auto& block : basic_blocks) {
    block.live.resize(block.instr_idx.size());
    block.dead.resize(block.instr_idx.size());
    block.analyze_liveliness_phase1(instructions);
  }

  // phase 2
  bool changed = false;
  do {
    changed = false;
    for (auto& block : basic_blocks) {
      if (block.analyze_liveliness_phase2(basic_blocks, instructions)) {
        changed = true;
      }
    }
  } while (changed);

  // phase 3
  for (auto& block : basic_blocks) {
    block.analyze_liveliness_phase3(basic_blocks, instructions);
  }

  // phase 4
  compute_live_ranges();
}

template <typename T>
bool in_set(std::set<T>& set, const T& obj) {
  return set.find(obj) != set.end();
}

template <typename T>
bool in_vec(const std::vector<T>& vec, const T& obj) {
  for (const auto& x : vec) {
    if (x == obj)
      return true;
  }
  return false;
}

template <typename T>
void print_set(std::set<T>& set) {
  for (auto x : set) {
    LOG("%s ", std::to_string(x).c_str());
  }
}

void RegAllocBasicBlock::analyze_liveliness_phase1(std::vector<RegAllocInstr>& instructions) {
  for (int i = instr_idx.size(); i-- > 0;) {
    auto ii = instr_idx.at(i);
    auto& instr = instructions.at(ii);
    auto& lv = live.at(i);
    auto& dd = dead.at(i);

    // make all read live out
    lv.clear();
    for (auto& x : instr.read) {
      lv.insert(x.id);
    }

    // kill things which are overwritten
    dd.clear();
    for (auto& x : instr.write) {
      if (!in_set(lv, x.id)) {
        dd.insert(x.id);
      }
    }

    // b.use = i.liveout
    std::set<int> use_old = use;
    use.clear();
    for (auto& x : lv) {
      use.insert(x);
    }
    // | (bu.use & !i.dead)
    for (auto& x : use_old) {
      if (!in_set(dd, x)) {
        use.insert(x);
      }
    }

    // b.defs = i.dead
    std::set<int> defs_old = defs;
    defs.clear();
    for (auto& x : dd) {
      defs.insert(x);
    }
    // | b.defs & !i.lv
    for (auto& x : defs_old) {
      if (!in_set(lv, x)) {
        defs.insert(x);
      }
    }
  }
}

bool RegAllocBasicBlock::analyze_liveliness_phase2(std::vector<RegAllocBasicBlock>& blocks,
                                                   std::vector<RegAllocInstr>& instructions) {
  (void)instructions;
  bool changed = false;
  auto out = defs;

  for (auto s : succ) {
    for (auto in : blocks.at(s).input) {
      out.insert(in);
    }
  }

  std::set<int> in = use;
  for (auto x : out) {
    if (!in_set(defs, x)) {
      in.insert(x);
    }
  }

  if (in != input || out != output) {
    changed = true;
    input = in;
    output = out;
  }

  return changed;
}

void RegAllocBasicBlock::analyze_liveliness_phase3(std::vector<RegAllocBasicBlock>& blocks,
                                                   std::vector<RegAllocInstr>& instructions) {
  (void)instructions;
  std::set<int> live_local;
  for (auto s : succ) {
    for (auto i : blocks.at(s).input) {
      live_local.insert(i);
    }
  }

  for (int i = instr_idx.size(); i-- > 0;) {
    auto& lv = live.at(i);
    auto& dd = dead.at(i);

    std::set<int> new_live = lv;
    for (auto x : live_local) {
      if (!in_set(dd, x)) {
        new_live.insert(x);
      }
    }
    lv = live_local;
    live_local = new_live;
  }
}

void RegAllocProgram::compute_live_ranges() {
  // then resize live ranges to the correct size
  live_ranges.resize(max_var, LiveRange(instructions.size(), 0));

  // now compute the ranges
  for (auto& block : basic_blocks) {
    // from var use
    for (auto instr_id : block.instr_idx) {
      auto& inst = instructions.at(instr_id);

      for (auto& lst : {inst.read, inst.write}) {
        for (auto& x : lst) {
          live_ranges.at(x.id).add_live_instruction(instr_id);
        }
      }
    }

    // and liveliness analysis
    assert(block.live.size() == block.instr_idx.size());
    for (uint32_t i = 0; i < block.live.size(); i++) {
      for (auto& x : block.live[i]) {
        live_ranges.at(x).add_live_instruction(block.instr_idx.at(i));
      }
    }
  }

  for (auto& con : constraints) {
    live_ranges.at(con.var_id).add_live_instruction(con.instr_id);
  }
}

void RegAllocProgram::do_constrained_allocations() {
  for (auto& constr : constraints) {
    auto var_id = constr.var_id;
    LOG("DO CONSTRAINED ALLOC VAR %d ASS %s\n", constr.var_id, constr.ass.print().c_str());
    LOG(" var %d, instr %d\n", var_id, constr.instr_id);
    live_ranges.at(var_id).constrain_at_one(constr.instr_id, constr.ass);
  }
}

void RegAllocProgram::check_constrained_allocations() {
  for (auto& constr : constraints) {
    if (!live_ranges.at(constr.var_id).conflicts_at(constr.instr_id, constr.ass)) {
      LOG("[ERROR] There are multiple conflicting coloring restraints on variable %d\n",
          constr.var_id);
      coloring_error = true;
    }
  }

  for (uint32_t i = 0; i < instructions.size(); i++) {
    for (auto& lr1 : live_ranges) {
      if (!lr1.seen || !lr1.is_live_at_instr(i))
        continue;
      for (auto& lr2 : live_ranges) {
        if (!lr2.seen || !lr2.is_live_at_instr(i) || (&lr1 == &lr2))
          continue;
        // if lr1 is assigned...
        auto& ass1 = lr1.get(i);
        if (ass1.kind != UNASSIGNED) {
          auto& ass2 = lr2.get(i);
          if (ass1.occupies_same_reg(ass2)) {
            LOG("[ERROR] There is an impossible constraint at instruction %d between var %d and "
                "%d\n",
                i, lr1.var, lr2.var);
            coloring_error = true;
          }
        }
      }
    }
  }
}

void RegAllocProgram::allocate() {
  // here we allocate
  std::vector<int> allocation_order;
  for (uint32_t i = 0; i < live_ranges.size(); i++) {
    if (live_ranges.at(i).seen && live_ranges.at(i).has_constraint) {
      allocation_order.push_back(i);
    }
  }

  for (uint32_t i = 0; i < live_ranges.size(); i++) {
    if (live_ranges.at(i).seen && !live_ranges.at(i).has_constraint) {
      allocation_order.push_back(i);
    }
  }

  for (int var : allocation_order) {
    do_allocation_for_var(var);
  }
}

//// todo consider adding r13
// std::vector<int> RegAllocProgram::get_default_reg_alloc_order() {
//  return {RAX, RCX, RDX, RSI, RDI, R8, R9, R10, R11, R12, RBX};
//}

std::vector<int> RegAllocProgram::get_default_alloc_order_for_var_spill(int v) {
  auto& info = coloring_input.at(v);
  assert(info.kind != UNASSIGNED_REG);

  if (info.kind == REG_GPR) {
    return {RAX, RCX, RDX, RSI, RDI, R8, R9, R10, R11, /*R12,*/ RBX};
  } else if (info.kind == REG_XMM_FLOAT) {
    // return {XMM0, XMM1, XMM2};
    return {XMM0, XMM1, XMM2,  XMM3,  XMM4,  XMM5,  XMM6,  XMM7,
            XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15};
  } else {
    throw std::runtime_error("unknown reg kind in get_default_alloc_order_for_var");
  }
}

std::vector<int> RegAllocProgram::get_default_alloc_order_for_var(int v) {
  auto& info = coloring_input.at(v);
  assert(info.kind != UNASSIGNED_REG);

  if (info.kind == REG_GPR) {
    return {RAX, RCX, RDX, RSI, RDI, R8, R9, R10, /*R11,*/ RBX};
  } else if (info.kind == REG_XMM_FLOAT) {
    // return {XMM0, XMM1, XMM2};
    return {XMM0, XMM1, XMM2,  XMM3,  XMM4,  XMM5,  XMM6, XMM7,
            XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14};
  } else {
    throw std::runtime_error("unknown reg kind in get_default_alloc_order_for_var");
  }
}

void RegAllocProgram::do_allocation_for_var(int var) {
  // first, let's see if there's a hint...
  auto& lr = live_ranges.at(var);
  bool colored = false;
  if (lr.best_hint.is_assigned()) {
    colored = try_assignment_for_var(var, lr.best_hint);
    LOG("var %d reg %s ? %d\n", var, lr.best_hint.print().c_str(), colored);
  }

  auto reg_order = get_default_alloc_order_for_var(var);

  // todo, try other regs..
  if (!colored && move_eliminator) {
    auto& first_instr = instructions.at(lr.min);
    auto& last_instr = instructions.at(lr.max);

    if (first_instr.is_move) {
      auto& possible_coloring = live_ranges.at(first_instr.read.front().id).get(lr.min);
      if (possible_coloring.is_assigned() && in_vec(reg_order, possible_coloring.reg_id)) {
        colored = try_assignment_for_var(var, possible_coloring);
      }
    }

    if (!colored && last_instr.is_move) {
      auto& possible_coloring = live_ranges.at(last_instr.write.front().id).get(lr.max);
      if (possible_coloring.is_assigned() && in_vec(reg_order, possible_coloring.reg_id)) {
        colored = try_assignment_for_var(var, possible_coloring);
      }
    }
  }

  // auto reg_order = get_default_reg_alloc_order();

  for (auto reg : reg_order) {
    if (colored)
      break;
    ColoringAssignment ass;
    ass.kind = REGISTER;
    ass.reg_id = reg;
    colored = try_assignment_for_var(var, ass);
    LOG("var %d reg %s ? %d\n", var, ass.print().c_str(), colored);
  }

  if (!colored) {
    colored = try_spill_coloring(var);
    if (colored)
      used_stack = true;
  }

  // todo, try spilling
  if (!colored) {
    LOG("[ERROR] var %d could not be colored:\n%s\n", var, live_ranges.at(var).print().c_str());

    coloring_error = true;
  } else {
    LOG("Colored var %d\n", var);
    was_colored.at(var) = true;
  }
}

int RegAllocProgram::get_stack_slot_for_var(int var) {
  auto kv = var_to_stack_slot.find(var);
  if (kv == var_to_stack_slot.end()) {
    auto slot = current_stack_slot++;
    var_to_stack_slot[var] = slot;
    return slot;
  } else {
    return kv->second;
  }
}

bool RegAllocProgram::try_spill_coloring(int var) {
  LOG("---- SPILL VAR %d ----\n", var);
  auto& lr = live_ranges.at(var);

  // possibly get a hint assignment
  ColoringAssignment hint_assignment;
  hint_assignment.kind = UNASSIGNED;

  // loop over live range
  for (int instr = lr.min; instr <= lr.max; instr++) {
    //    bonus_instructions.at(instr).clear();
    BonusOp bonus;

    // we may have a constaint in here
    auto& current_assignment = lr.assignment.at(instr - lr.min);

    auto& op = instructions.at(instr);
    bool is_read = op.reads(var);
    bool is_written = op.writes(var);

    // we have a constraint!
    if (current_assignment.is_assigned()) {
      LOG("  [%02d] already assigned %s\n", instr, current_assignment.print().c_str());

      // remember this assignment as a hint for later
      hint_assignment = current_assignment;
      // check that this assignment is ok
      if (!assignment_ok_at(var, instr, current_assignment)) {
        // this shouldn't be possible with feasible constraints
        printf("-- SPILL FAILED -- IMPOSSIBLE CONSTRAINT @ %d %s\n", instr,
               current_assignment.print().c_str());
        assert(false);
        return false;
      }

      // flag it as spilled, but currently in a GPR.
      current_assignment.spilled = true;
      bonus.ass = current_assignment;
    } else {
      // not assigned.
      LOG("  [%02d] nya rd? %d wr? %d\n", instr, is_read, is_written);

      // We'd like to keep it on the stack if possible
      ColoringAssignment spill_assignment;
      spill_assignment.spilled = true;
      spill_assignment.kind = STACK;
      spill_assignment.reg_id = -1;  // for now

      // needs a temp register
      if (is_read || is_written) {
        // we need to put it in a register here!
        // first check if the hint works?
        // todo floats?
        if (hint_assignment.kind == AssignmentKind::REGISTER) {
          LOG("   try hint %s\n", hint_assignment.print().c_str());
          if (assignment_ok_at(var, instr, hint_assignment)) {
            // it's ok!
            LOG("   it worked!\n");
            spill_assignment.reg_id = hint_assignment.reg_id;
          }
        }

        // hint didn't work
        // auto reg_order = get_default_reg_alloc_order();
        auto reg_order = get_default_alloc_order_for_var_spill(var);
        if (spill_assignment.reg_id == -1) {
          for (auto reg : reg_order) {
            ColoringAssignment ass;
            ass.kind = REGISTER;
            ass.reg_id = reg;
            LOG("  try %s\n", ass.print().c_str());

            if (assignment_ok_at(var, instr, ass)) {
              LOG("  it worked!\n");
              spill_assignment.reg_id = ass.reg_id;
              break;
            }
          }
        }

        if (spill_assignment.reg_id == -1) {
          LOG("SPILLING FAILED BECAUSE WE COULDN'T FIND A TEMP REGISTER!\n");
          assert(false);
          //          std::vector<bool> can_try_spilling;
          //          for(uint32_t other_spill = 0; other_spill < was_colored.size(); other_spill++)
          //          {
          //            if((int)other_spill != var && was_colored.at(other_spill)) {
          //              LOG("TRY SPILL %d?\n", other_spill);
          //              if(try_spill_coloring(other_spill)) {
          //                LOG("SPILL OK.\n");
          //                if(try_spill_coloring(var)) {
          //                  return true;
          //                }
          //              } else {
          //                LOG("SPILL %d failed.\n", other_spill);
          //              }
          //            }
          //          }
          return false;
        }

        // mark that it's in a GPR!
        spill_assignment.kind = REGISTER;
      }  // end need temp reg
      spill_assignment.stack_slot = get_stack_slot_for_var(var);
      lr.assignment.at(instr - lr.min) = spill_assignment;
      bonus.ass = spill_assignment;
    }  // end not constrained

    bonus.stack_slot = get_stack_slot_for_var(var);
    bonus.load_from_stack = is_read;
    bonus.store_into_stack = is_written;
    bonus_instructions.at(instr).ops.push_back(bonus);
  }
  return true;
}

bool RegAllocProgram::try_assignment_for_var(int var, ColoringAssignment ass) {
  if (can_var_be_assigned(var, ass)) {
    assign_var_no_check(var, ass);
    return true;
  }
  return false;
}

bool RegAllocProgram::assignment_ok_at(int var, int idx, ColoringAssignment ass) {
  auto& lr = live_ranges.at(var);
  for (auto& other_lr : live_ranges) {
    if (other_lr.var == var /*|| !other_lr.seen*/)
      continue;
    if (other_lr.is_live_at_instr(idx)) {
      if (/*(idx != other_lr.max) &&*/ other_lr.conflicts_at(idx, ass)) {
        bool allowed_by_move_eliminator = false;
        if (move_eliminator) {
          if (enable_fancy_coloring) {
            if (lr.dies_next_at_instr(idx) && other_lr.becomes_live_at_instr(idx) &&
                instructions.at(idx).is_move) {
              allowed_by_move_eliminator = true;
            }

            if (lr.becomes_live_at_instr(idx) && other_lr.dies_next_at_instr(idx) &&
                instructions.at(idx).is_move) {
              allowed_by_move_eliminator = true;
            }
          } else {
            // case to allow rename (from us to them)
            if (idx == lr.max && idx == other_lr.min && instructions.at(idx).is_move) {
              allowed_by_move_eliminator = true;
            }

            if (idx == lr.min && idx == other_lr.min && instructions.at(idx).is_move) {
              allowed_by_move_eliminator = true;
            }
          }
        }

        if (!allowed_by_move_eliminator) {
          LOG("at idx %d, %s conflicts\n", idx, other_lr.print().c_str());
          return false;
        }
      }
    }
  }

  // check we aren't violating a clobber
  if (idx != lr.min && idx != lr.max) {
    for (auto clobber : instructions.at(idx).clobber) {
      if (clobber.occupies_same_reg(ass)) {
        LOG("at idx %d clobber\n", idx);
        return false;
      }
    }
  }

  for (auto exclusive : instructions.at(idx).exclusive) {
    if (exclusive.occupies_same_reg(ass)) {
      LOG("at idx %d exclusive conflict\n", idx);
      return false;
    }
  }

  // check we aren't violating ourselves
  if (lr.assignment.at(idx - lr.min).is_assigned()) {
    if (!(ass.occupies_same_reg(lr.assignment.at(idx - lr.min)))) {
      LOG("at idx %d self bad\n", idx);
      return false;
    }
  }

  return true;
}

bool RegAllocProgram::can_var_be_assigned(int var, ColoringAssignment ass) {
  // our live range:
  auto& lr = live_ranges.at(var);
  // check against all other live ranges:
  for (auto& other_lr : live_ranges) {
    if (other_lr.var == var /*|| !other_lr.seen*/)
      continue;  // but not us!
    for (int instr = lr.min; instr <= lr.max; instr++) {
      if (other_lr.is_live_at_instr(instr)) {
        // LR's overlap
        if (/*(instr != other_lr.max) && */ other_lr.conflicts_at(instr, ass)) {
          bool allowed_by_move_eliminator = false;
          if (move_eliminator) {
            if (enable_fancy_coloring) {
              if (lr.dies_next_at_instr(instr) && other_lr.becomes_live_at_instr(instr) &&
                  instructions.at(instr).is_move) {
                allowed_by_move_eliminator = true;
              }

              if (lr.becomes_live_at_instr(instr) && other_lr.dies_next_at_instr(instr) &&
                  instructions.at(instr).is_move) {
                allowed_by_move_eliminator = true;
              }
            } else {
              // case to allow rename (from us to them)
              if (instr == lr.max && instr == other_lr.min && instructions.at(instr).is_move) {
                allowed_by_move_eliminator = true;
              }

              if (instr == lr.min && instr == other_lr.min && instructions.at(instr).is_move) {
                allowed_by_move_eliminator = true;
              }
            }
          }

          if (!allowed_by_move_eliminator) {
            LOG("at idx %d, %s conflicts\n", instr, other_lr.print().c_str());
            return false;
          }
        }
      }
    }
  }

  // can clobber on the last one or first one - check that we don't interfere with a clobber
  for (int instr = lr.min + 1; instr <= lr.max - 1; instr++) {
    for (auto clobber : instructions.at(instr).clobber) {
      if (clobber.occupies_same_reg(ass)) {
        LOG("at idx %d clobber\n", instr);
        return false;
      }
    }
  }

  for (int instr = lr.min; instr <= lr.max; instr++) {
    for (auto exclusive : instructions.at(instr).exclusive) {
      if (exclusive.occupies_same_reg(ass)) {
        LOG("at idx %d exclusive conflict\n", instr);
        return false;
      }
    }
  }

  // check we don't violate any others.
  for (int instr = lr.min; instr <= lr.max; instr++) {
    if (lr.has_constraint && lr.assignment.at(instr - lr.min).is_assigned()) {
      if (!(ass.occupies_same_reg(lr.assignment.at(instr - lr.min)))) {
        LOG("at idx %d self bad\n", instr);
        return false;
      }
    }
  }

  return true;
}

void RegAllocProgram::assign_var_no_check(int var, ColoringAssignment ass) {
  live_ranges.at(var).assign_no_overwrite(ass);
}

std::pair<int, int> RegAllocProgram::get_move_stats() {
  int total_moves = 0;
  int eliminated_moves = 0;

  for (size_t i = 0; i < instructions.size(); i++) {
    auto& instr = instructions[i];
    if (instr.is_move) {
      total_moves++;
      auto dst = live_ranges.at(instr.write.front().id).get(i);
      auto src = live_ranges.at(instr.read.front().id).get(i);
      if (dst.occupies_same_reg(src)) {
        eliminated_moves++;
      }
    }
  }
  return std::make_pair(eliminated_moves, total_moves);
}

int RegAllocProgram::get_spill_count() {
  int count = 0;
  for (auto& x : bonus_instructions) {
    for (auto& y : x.ops) {
      if (y.load_from_stack || y.store_into_stack) {
        count++;
      }
    }
  }
  return count;
}
