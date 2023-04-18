/*!
 * @file cfg_builder.cpp
 * Initial conversion from Control Flow Graph to IR2 Form.
 */

#include "cfg_builder.h"
#include "common/log/log.h"
#include "decompiler/Function/Function.h"
#include "decompiler/IR2/Form.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "decompiler/util/MatchParam.h"

namespace decompiler {
namespace {

Form* cfg_to_ir(FormPool& pool, Function& f, const CfgVtx* vtx);

/*!
 * If it's a form containing multiple elements, return a pointer to the branch element and the end
 * and also a pointer to the Form containing the branch element.
 * Otherwise returns nullptr.  Useful to modify or remove branches found at the end of blocks,
 * and inline things into the begin they were found in.
 */
std::pair<BranchElement*, Form*> get_condition_branch_as_vector(Form* in) {
  // With the current Form setup, we'll never have to dig deper to find the branch.
  // so we can just return the input as the Form*.
  //  If this changes, this can be fixed here, rather than refactoring the whole thing.
  if (in->size() > 1) {
    auto irb = dynamic_cast<BranchElement*>(in->back());
    ASSERT(irb);
    return std::make_pair(irb, in);
  }
  return std::make_pair(nullptr, nullptr);
}

/*!
 * Given an IR, find a branch IR at the end, and also the location of it so it can be patched.
 * Returns nullptr as the first item in the pair if it didn't work.
 * Use this to inspect a sequence ending in branch and have to ability to replace the branch with
 * something else if needed.
 */
std::pair<BranchElement*, FormElement**> get_condition_branch(Form* in) {
  BranchElement* condition_branch = dynamic_cast<BranchElement*>(in->back());
  FormElement** condition_branch_location = in->back_ref();

  if (!condition_branch) {
    auto as_return = dynamic_cast<ReturnElement*>(in->back());
    if (as_return) {
      return get_condition_branch(as_return->dead_code);
    }
  }

  if (!condition_branch) {
    auto as_break = dynamic_cast<BreakElement*>(in->back());
    if (as_break) {
      return get_condition_branch(as_break->dead_code);
    }
  }
  return std::make_pair(condition_branch, condition_branch_location);
}

/*!
 * Given a CondWithElse IR, remove the internal branches and set the condition to be an actual
 * compare IR instead of a branch.
 * Doesn't "rebalance" the leading condition because this runs way before expression compaction.
 */
void clean_up_cond_with_else(FormPool& pool, FormElement* ir, const Env& env) {
  auto cwe = dynamic_cast<CondWithElseElement*>(ir);
  ASSERT(cwe);
  for (auto& e : cwe->entries) {
    // don't reclean already cleaned things.
    if (e.cleaned) {
      continue;
    }
    auto jump_to_next = get_condition_branch(e.condition);
    ASSERT(jump_to_next.first);
    ASSERT(jump_to_next.first->op()->branch_delay().kind() == IR2_BranchDelay::Kind::NOP);
    // patch the branch to next with a condition.
    auto replacement = jump_to_next.first->op()->get_condition_as_form(pool, env);
    replacement->invert();
    *(jump_to_next.second) = replacement;

    // check the jump at the end of a block.
    auto jump_to_end = get_condition_branch(e.body);
    ASSERT(jump_to_end.first);
    ASSERT(jump_to_end.first->op()->branch_delay().kind() == IR2_BranchDelay::Kind::NOP);
    ASSERT(jump_to_end.first->op()->condition().kind() == IR2_Condition::Kind::ALWAYS);

    // if possible, we just want to remove this from the sequence its in.
    // but sometimes there's a case with nothing in it so there is no sequence.
    // in this case, we can just replace the branch with a NOP IR to indicate that nothing
    // happens in this case, but there was still GOAL code to test for it.
    // this happens rarely, as you would expect.
    auto as_end_of_sequence = get_condition_branch_as_vector(e.body);
    if (as_end_of_sequence.first) {
      ASSERT(as_end_of_sequence.second->size() > 1);
      as_end_of_sequence.second->pop_back();
    } else {
      // we need to have _something_ as the body, so we just put an (empty).
      *(jump_to_end.second) = pool.alloc_element<EmptyElement>();
    }
    e.cleaned = true;
  }
}

/*!
 * Replace the branch at the end of an until loop's condition with a condition.
 */
void clean_up_until_loop(FormPool& pool, UntilElement* ir, const Env& env) {
  auto condition_branch = get_condition_branch(ir->condition);
  ASSERT(condition_branch.first);
  if (condition_branch.first->op()->branch_delay().kind() != IR2_BranchDelay::Kind::NOP) {
    ASSERT_MSG(
        condition_branch.first->op()->branch_delay().kind() == IR2_BranchDelay::Kind::SET_REG_FALSE,
        fmt::format(
            "bad delay slot in until loop: {} in {}\n", env.func->name(),
            condition_branch.first->op()->branch_delay().to_form(env.file->labels, env).print()));
    ir->false_destination = condition_branch.first->op()->branch_delay().var(0);
  }
  auto replacement = condition_branch.first->op()->get_condition_as_form(pool, env);
  replacement->invert();
  *(condition_branch.second) = replacement;
}

/*!
 * Remove the true branch at the end of an infinite while loop.
 */
void clean_up_infinite_while_loop(FormPool& pool, WhileElement* ir) {
  auto jump = get_condition_branch(ir->body);
  ASSERT(jump.first);
  ASSERT(jump.first->op()->branch_delay().kind() == IR2_BranchDelay::Kind::NOP);
  ASSERT(jump.first->op()->condition().kind() == IR2_Condition::Kind::ALWAYS);
  auto as_end_of_sequence = get_condition_branch_as_vector(ir->body);
  if (as_end_of_sequence.first) {
    // there's more in the sequence, just remove the last thing.
    ASSERT(as_end_of_sequence.second->size() > 1);
    as_end_of_sequence.second->pop_back();
  } else {
    // Nothing else in the sequence, just replace the jump with an (empty)
    *(jump.second) = pool.alloc_element<EmptyElement>();
  }
  ir->cleaned = true;  // so we don't try this later...
}

/*!
 * Remove the branch in a return statement
 */
void clean_up_return(FormPool& pool, ReturnElement* ir) {
  auto jump_to_end = get_condition_branch(ir->return_code);
  ASSERT(jump_to_end.first);
  ASSERT(jump_to_end.first->op()->branch_delay().kind() == IR2_BranchDelay::Kind::NOP);
  ASSERT(jump_to_end.first->op()->condition().kind() == IR2_Condition::Kind::ALWAYS);
  auto as_end_of_sequence = get_condition_branch_as_vector(ir->return_code);
  if (as_end_of_sequence.first) {
    ASSERT(as_end_of_sequence.second->size() > 1);
    as_end_of_sequence.second->pop_back();
  } else {
    *(jump_to_end.second) = pool.alloc_element<EmptyElement>();
  }
}

void clean_up_return_final(const Function& f, ReturnElement* ir) {
  SetVarElement* dead = dynamic_cast<SetVarElement*>(ir->dead_code->try_as_single_element());
  if (!dead) {
    dead = dynamic_cast<SetVarElement*>(ir->dead_code->elts().front());
    for (int i = 1; i < ir->dead_code->size(); i++) {
      if (!dynamic_cast<EmptyElement*>(ir->dead_code->at(i))) {
        dead = nullptr;
        break;
      }
    }
  }

  if (!dead) {
    throw std::runtime_error(fmt::format("failed to recognize dead code after return, got {}",
                                         ir->dead_code->to_string(f.ir2.env)));
  }
  ASSERT(dead);
  auto src = dynamic_cast<SimpleExpressionElement*>(dead->src()->try_as_single_element());
  ASSERT(src);
  ASSERT(src->expr().is_identity() && src->expr().get_arg(0).is_int() &&
         src->expr().get_arg(0).get_int() == 0);
  ir->dead_code = nullptr;
}

/*!
 * Remove the branch in a break (really return-from nonfunction scope)
 */
void clean_up_break(FormPool& pool, BreakElement* ir, const Env&) {
  auto jump_to_end = get_condition_branch(ir->return_code);
  ASSERT(jump_to_end.first);
  ASSERT(jump_to_end.first->op()->branch_delay().kind() == IR2_BranchDelay::Kind::NOP);
  ASSERT(jump_to_end.first->op()->condition().kind() == IR2_Condition::Kind::ALWAYS);
  auto as_end_of_sequence = get_condition_branch_as_vector(ir->return_code);
  if (as_end_of_sequence.first) {
    ASSERT(as_end_of_sequence.second->size() > 1);
    as_end_of_sequence.second->pop_back();
  } else {
    *(jump_to_end.second) = pool.alloc_element<EmptyElement>();
  }
}

void clean_up_break_final(const Function& f, BreakElement* ir, const Env& env) {
  EmptyElement* dead_empty = dynamic_cast<EmptyElement*>(ir->dead_code->try_as_single_element());
  if (dead_empty) {
    ir->dead_code = nullptr;
    return;
  }

  SetVarElement* dead = dynamic_cast<SetVarElement*>(ir->dead_code->try_as_single_element());
  if (!dead) {
    dead = dynamic_cast<SetVarElement*>(ir->dead_code->elts().front());
    for (int i = 1; i < ir->dead_code->size(); i++) {
      if (!dynamic_cast<EmptyElement*>(ir->dead_code->at(i))) {
        dead = nullptr;
        break;
      }
    }
  }

  if (!dead) {
    if (ir->dead_code->to_string(env) == "(nop!)") {
      ir->dead_code = nullptr;
      return;
    }
  }

  if (!dead) {
    throw std::runtime_error(fmt::format("failed to recognize dead code after break, got {}",
                                         ir->dead_code->to_string(f.ir2.env)));
  }
  ASSERT(dead);
  auto src = dynamic_cast<SimpleExpressionElement*>(dead->src()->try_as_single_element());
  ASSERT(src);
  if (src->expr().is_identity() && src->expr().get_arg(0).is_int() &&
      src->expr().get_arg(0).get_int() == 0) {
    ir->dead_code = nullptr;
  }
}

/*!
 * Does the instruction in the delay slot set a register to false?
 * Note. a beql s7, x followed by a or y, x, r0 will count as this. I don't know why but
 * GOAL does this on comparisons to false.
 */
bool delay_slot_sets_false(BranchElement* branch, SetVarOp& delay) {
  ASSERT(branch->op()->likely());
  ASSERT(branch->op()->branch_delay().kind() == IR2_BranchDelay::Kind::NO_DELAY);

  if (delay.src().is_identity() && delay.src().get_arg(0).is_sym_val() &&
      delay.src().get_arg(0).get_str() == "#f") {
    return true;
  }

  if (branch->op()->condition().kind() == IR2_Condition::Kind::FALSE) {
    if (delay.src().is_identity() && delay.src().get_arg(0).is_var()) {
      auto src_var = delay.src().get_arg(0).var();
      auto& cond = branch->op()->condition();
      auto cond_reg = cond.src(0).var().reg();
      return cond_reg == src_var.reg();
    }
  }

  return false;

  //  if (branch->op()->branch_delay().kind() == IR2_BranchDelay::Kind::SET_REG_FALSE) {
  //    return true;
  //  }
  //
  //  if (branch->op()->condition().kind() == IR2_Condition::Kind::FALSE &&
  //      branch->op()->branch_delay().kind() == IR2_BranchDelay::Kind::SET_REG_REG) {
  //    auto& cond = branch->op()->condition();
  //    auto& delay = branch->op()->branch_delay();
  //    auto cond_reg = cond.src(0).var().reg();
  //    auto src_reg = delay.var(1).reg();
  //    return cond_reg == src_reg;
  //  }
  //
  //  return false;
}

/*!
 * Does the instruction in the delay slot set a register to a truthy value, like in a GOAL
 * or form branch?  Either it explicitly sets #t, or it tests the value for being not false,
 * then uses that
 */
bool delay_slot_sets_truthy(BranchElement* branch, SetVarOp& delay) {
  ASSERT(branch->op()->likely());
  ASSERT(branch->op()->branch_delay().kind() == IR2_BranchDelay::Kind::NO_DELAY);

  if (delay.src().is_identity() && delay.src().get_arg(0).is_sym_ptr() &&
      delay.src().get_arg(0).get_str() == "#t") {
    return true;
  }

  if (branch->op()->condition().kind() == IR2_Condition::Kind::TRUTHY) {
    if (delay.src().is_identity() && delay.src().get_arg(0).is_var()) {
      auto src_var = delay.src().get_arg(0).var();
      auto& cond = branch->op()->condition();
      auto cond_reg = cond.src(0).var().reg();
      return cond_reg == src_var.reg();
    }
  }

  //  if (branch->op()->branch_delay().kind() == IR2_BranchDelay::Kind::SET_REG_TRUE) {
  //    return true;
  //  }
  //
  //  if (branch->op()->condition().kind() == IR2_Condition::Kind::TRUTHY &&
  //      branch->op()->branch_delay().kind() == IR2_BranchDelay::Kind::SET_REG_REG) {
  //    auto& cond = branch->op()->condition();
  //    auto& delay = branch->op()->branch_delay();
  //    auto cond_reg = cond.src(0).var().reg();
  //    auto src_reg = delay.var(1).reg();
  //    return cond_reg == src_reg;
  //  }

  return false;
}

/*!
 * Try to convert a short circuit to an and.
 */
bool try_clean_up_sc_as_and(FormPool& pool, Function& func, ShortCircuitElement* ir) {
  Register destination;
  RegisterAccess ir_dest;
  for (int i = 0; i < int(ir->entries.size()) - 1; i++) {
    auto branch = get_condition_branch(ir->entries.at(i).condition);
    ASSERT(branch.first);
    ASSERT(ir->entries.at(i).branch_delay.has_value());
    if (!delay_slot_sets_false(branch.first, *ir->entries.at(i).branch_delay)) {
      return false;
    }

    if (i == 0) {
      // first case, remember the destination
      ir_dest = ir->entries.at(i).branch_delay->dst();
      destination = ir_dest.reg();
    } else {
      // check destination against the first case.
      if (destination != ir->entries.at(i).branch_delay->dst().reg()) {
        return false;
      }
    }
  }

  ir->kind = ShortCircuitElement::AND;
  ir->final_result = ir_dest;

  bool live_out_result = false;

  // now get rid of the branches
  for (int i = 0; i < int(ir->entries.size()) - 1; i++) {
    auto branch = get_condition_branch(ir->entries.at(i).condition);
    ASSERT(branch.first);

    if (func.ir2.env.has_reg_use()) {
      auto delay_id = ir->entries.at(i).branch_delay->dst().idx();
      auto& delay_info = func.ir2.env.reg_use().op.at(delay_id);

      auto branch_id = branch.first->op()->op_id();
      auto& branch_info = func.ir2.env.reg_use().op.at(branch_id);
      for (auto x : delay_info.consumes) {
        branch_info.consumes.insert(x);
      }

      auto delay_op = func.ir2.atomic_ops->ops.at(delay_id).get();
      auto as_set = dynamic_cast<SetVarOp*>(delay_op);
      ASSERT(as_set);
      if (as_set->src().is_var()) {
        // must be the case where the src should have truthy in it.
        // lg::warn("Disabling use of {} in or delay slot", as_set->to_string(func.ir2.env));
        func.ir2.env.disable_use(as_set->src().var());
      }

      // we also want to fix up the use/def info for the result.
      // it's somewhat arbitrary, but we use the convention that the short-circuit defs
      // are eliminated:
      if (func.ir2.env.has_local_vars()) {
        auto& ud_info = func.ir2.env.get_use_def_info(as_set->dst());
        if (i == int(ir->entries.size()) - 2) {
          if (ud_info.def_count() == 1) {
            // the final case of the or doesn't explicitly set the destination register.
            // this can happen if the move is eliminated during coloring.
            // for now, let's leave this last def here, just so it looks like _something_ sets it.

          } else {
            // lg::warn("Disabling def of {} in final or delay slot",
            // as_set->to_string(func.ir2.env));
            func.ir2.env.disable_def(as_set->dst(), func.warnings);
          }
        } else {
          // lg::warn("Disabling def of {} in or delay slot", as_set->to_string(func.ir2.env));
          func.ir2.env.disable_def(as_set->dst(), func.warnings);
        }
      }

      if (i == 0) {
        live_out_result = (branch_info.written_and_unused.find(ir_dest.reg()) ==
                           branch_info.written_and_unused.end());
      } else {
        bool this_live_out = (branch_info.written_and_unused.find(ir_dest.reg()) ==
                              branch_info.written_and_unused.end());
        if (live_out_result != this_live_out) {
          lg::error("Bad live out result on {}. At 0 was {} now at {} is {}", func.name(),
                    live_out_result, i, this_live_out);
        }
        ASSERT(live_out_result == this_live_out);
      }
    }

    auto replacement = branch.first->op()->get_condition_as_form(pool, func.ir2.env);
    replacement->invert();
    *(branch.second) = replacement;
  }

  ir->used_as_value = live_out_result;
  return true;
}

/*!
 * Try to convert a short circuit to an or.
 * Note - this will convert an and to a very strange or, so always use the try as and first.
 */
bool try_clean_up_sc_as_or(FormPool& pool, Function& func, ShortCircuitElement* ir) {
  // all cases of an or, excluding the last one, should move the value "true" into the result reg
  // in case the short circuit is taken.  The final case should just write the result reg.

  Register destination;    // destination register
  RegisterAccess ir_dest;  // destination access (the first one)

  // all but the last one (these should all do the delay slot trick)
  // this first pass is where we can reject this as an or.
  for (int i = 0; i < int(ir->entries.size()) - 1; i++) {
    // short circuit branch
    auto branch = get_condition_branch(ir->entries.at(i).condition);
    ASSERT(branch.first);
    ASSERT(ir->entries.at(i).branch_delay.has_value());
    // the branch should write true (there's two ways this can happen)
    if (!delay_slot_sets_truthy(branch.first, *ir->entries.at(i).branch_delay)) {
      return false;
    }
    if (i == 0) {
      // first case, remember the destination
      ir_dest = ir->entries.at(i).branch_delay->dst();
      destination = ir_dest.reg();
    } else {
      // check destination against the first case.
      if (destination != ir->entries.at(i).branch_delay->dst().reg()) {
        return false;
      }
    }
  }

  // at this point, we know that all non-last cases write the destination, and what
  // the destination is.
  // so we commit to rewriting this thing as an or, and any errors from here on are fatal.

  ir->kind = ShortCircuitElement::OR;
  ir->final_result = ir_dest;

  // we also would like to know if the result of this OR is used or not.
  // there's also a sanity check that:
  //  if the result is used - all writes to the result reg _should_ be live
  //  if the result is unused - all writes to the result reg should be dead.
  //  otherwise it means that our control flow graph is messed up, and we abort.

  bool live_out_result = false;

  for (int i = 0; i < int(ir->entries.size()) - 1; i++) {
    auto branch = get_condition_branch(ir->entries.at(i).condition);
    ASSERT(branch.first);

    if (func.ir2.env.has_reg_use()) {
      auto delay_id = ir->entries.at(i).branch_delay->dst().idx();
      auto& delay_info = func.ir2.env.reg_use().op.at(delay_id);

      // cheat for the old method
      auto branch_id = branch.first->op()->op_id();
      auto& branch_info = func.ir2.env.reg_use().op.at(branch_id);
      for (auto x : delay_info.consumes) {
        branch_info.consumes.insert(x);
      }

      // the branch may look like this:
      // bnel s7, a3, L283
      // or a2, a3, r0
      // which reads a3 twice.  But we want it to count as only once, as the second read
      // is inserted by the GOAL compiler, not by putting a var twice in the source code.
      auto delay_op = func.ir2.atomic_ops->ops.at(delay_id).get();
      auto as_set = dynamic_cast<SetVarOp*>(delay_op);
      ASSERT(as_set);
      if (as_set->src().is_var()) {
        // must be the case where the src should have truthy in it.
        // lg::warn("Disabling use of {} in or delay slot", as_set->to_string(func.ir2.env));
        func.ir2.env.disable_use(as_set->src().var());
      }

      // we also want to fix up the use/def info for the result.
      // it's somewhat arbitrary, but we use the convention that the short-circuit defs
      // are eliminated:
      if (func.ir2.env.has_local_vars()) {
        auto& ud_info = func.ir2.env.get_use_def_info(as_set->dst());
        if (i == int(ir->entries.size()) - 2) {
          if (ud_info.def_count() == 1) {
            // the final case of the or doesn't explicitly set the destination register.
            // this can happen if the move is eliminated during coloring.
            // for now, let's leave this last def here, just so it looks like _something_ sets it.
            // TODO - what if this isn't a def in the last slot? Does it matter?
          } else {
            // lg::warn("Disabling def of {} in final or delay slot",
            // as_set->to_string(func.ir2.env));
            func.ir2.env.disable_def(as_set->dst(), func.warnings);
          }

        } else {
          // lg::warn("Disabling def of {} in or delay slot", as_set->to_string(func.ir2.env));
          func.ir2.env.disable_def(as_set->dst(), func.warnings);
        }
      }

      if (i == 0) {
        live_out_result = (delay_info.written_and_unused.find(ir_dest.reg()) ==
                           delay_info.written_and_unused.end());
      } else {
        bool this_live_out = (delay_info.written_and_unused.find(ir_dest.reg()) ==
                              delay_info.written_and_unused.end());
        ASSERT(live_out_result == this_live_out);
      }
    }

    auto replacement = branch.first->op()->get_condition_as_form(pool, func.ir2.env);
    *(branch.second) = replacement;
  }

  // TODO - check the one remaining def location?

  ir->used_as_value = live_out_result;

  return true;
}

void clean_up_sc(FormPool& pool, Function& func, ShortCircuitElement* ir);

/*!
 * A form like (and x (or y z)) will be recognized as a single SC Vertex by the CFG pass.
 * In the case where we fail to clean it up as an AND or an OR, we should attempt splitting.
 * Part of the complexity here is that we want to clean up the split recursively so things like
 * (and x (or y (and a b)))
 * or
 * (and x (or y (and a b)) c d (or z))
 * will work correctly.  This may require doing more splitting on both sections!
 */
bool try_splitting_nested_sc(FormPool& pool, Function& func, ShortCircuitElement* ir) {
  auto first_branch = get_condition_branch(ir->entries.front().condition);
  ASSERT(first_branch.first);
  ASSERT(ir->entries.front().branch_delay.has_value());
  bool first_is_and = delay_slot_sets_false(first_branch.first, *ir->entries.front().branch_delay);
  bool first_is_or = delay_slot_sets_truthy(first_branch.first, *ir->entries.front().branch_delay);

  if (first_is_and == first_is_or) {
    throw std::runtime_error(fmt::format(
        "Failed to split nested sc.  This may mean that abs/ash/type-of was misrecognized as "
        "and/or:\n{}",
        ir->to_string(func.ir2.env)));
  }
  ASSERT(first_is_and != first_is_or);  // one or the other but not both!

  int first_different = -1;  // the index of the first one that's different.

  for (int i = 1; i < int(ir->entries.size()) - 1; i++) {
    auto branch = get_condition_branch(ir->entries.at(i).condition);
    ASSERT(branch.first);
    ASSERT(ir->entries.at(i).branch_delay.has_value());
    bool is_and = delay_slot_sets_false(branch.first, *ir->entries.at(i).branch_delay);
    bool is_or = delay_slot_sets_truthy(branch.first, *ir->entries.at(i).branch_delay);
    ASSERT_MSG(is_and != is_or,
               fmt::format("bad nested sc in {}: {}", func.name(), ir->to_string(func.ir2.env)));

    if (first_different == -1) {
      // haven't seen a change yet.
      if (first_is_and != is_and) {
        // change!
        first_different = i;
        break;
      }
    }
  }

  ASSERT(first_different != -1);

  std::vector<ShortCircuitElement::Entry> nested_ir;
  for (int i = first_different; i < int(ir->entries.size()); i++) {
    nested_ir.push_back(ir->entries.at(i));
  }

  auto s = int(ir->entries.size());
  for (int i = first_different; i < s; i++) {
    ir->entries.pop_back();
  }

  // nested_sc has no parent yet.
  auto nested_sc = pool.alloc_element<ShortCircuitElement>(nested_ir);
  clean_up_sc(pool, func, nested_sc);

  // the real trick
  ShortCircuitElement::Entry nested_entry;
  // sets both parents
  nested_entry.condition = pool.alloc_single_form(ir, nested_sc);
  ir->entries.push_back(nested_entry);

  clean_up_sc(pool, func, ir);

  return true;
}

/*!
 * Try to clean up a single short circuit IR. It may get split up into nested IR_ShortCircuits
 * if there is a case like (and a (or b c))
 */
void clean_up_sc(FormPool& pool, Function& func, ShortCircuitElement* ir) {
  ASSERT(ir->entries.size() > 0);
  if (ir->entries.size() == 1) {
    // need to fake the final entry.
    ShortCircuitElement::Entry empty_final;
    empty_final.condition = pool.alloc_single_element_form<EmptyElement>(ir);
    ir->entries.push_back(empty_final);
  }

  if (!try_clean_up_sc_as_and(pool, func, ir)) {
    if (!try_clean_up_sc_as_or(pool, func, ir)) {
      if (!try_splitting_nested_sc(pool, func, ir)) {
        ASSERT(false);
      }
    }
  }
}

const SimpleAtom* get_atom_src(const Form* form) {
  auto* elt = form->try_as_single_element();
  if (elt) {
    auto* as_expr = dynamic_cast<SimpleExpressionElement*>(elt);
    if (as_expr) {
      if (as_expr->expr().is_identity()) {
        return &as_expr->expr().get_arg(0);
      }
    }
  }
  return nullptr;
}

/*!
 * A GOAL comparison which produces a boolean is recognized as a cond-no-else by the CFG analysis.
 * But it should not be decompiled as a branching statement.
 * This either succeeds or asserts and must be called with with something that can be converted
 * successfully
 */
void convert_cond_no_else_to_compare(FormPool& pool,
                                     Function& f,
                                     FormElement** ir_loc,
                                     Form* parent_form) {
  CondNoElseElement* cne = dynamic_cast<CondNoElseElement*>(*ir_loc);
  ASSERT(cne);
  auto condition = get_condition_branch(cne->entries.front().condition);
  ASSERT(condition.first);
  auto body = dynamic_cast<SetVarElement*>(cne->entries.front().body->try_as_single_element());
  ASSERT(body);
  auto dst = body->dst();
  auto src_atom = get_atom_src(body->src());
  ASSERT(src_atom);
  ASSERT(src_atom->is_sym_val());
  ASSERT(src_atom->get_str() == "#f");
  ASSERT(cne->entries.size() == 1);

  // safe to do this here because we never give up on this.
  f.ir2.env.disable_def(condition.first->op()->branch_delay().var(0), f.warnings);

  auto condition_as_single =
      dynamic_cast<BranchElement*>(cne->entries.front().condition->try_as_single_element());
  auto condition_replacement = condition.first->op()->get_condition_as_form(pool, f.ir2.env);
  auto crf = pool.alloc_single_form(nullptr, condition_replacement);
  auto replacement = pool.alloc_element<SetVarElement>(dst, crf, true, TypeSpec("symbol"));
  replacement->parent_form = cne->parent_form;

  if (condition_as_single) {
    *ir_loc = replacement;
  } else {
    //    lg::error("Weird case in {}", f.name());
    (void)f;
    auto seq = cne->entries.front().condition;
    seq->pop_back();
    seq->push_back(replacement);

    parent_form->pop_back();
    for (auto& x : seq->elts()) {
      parent_form->push_back(x);
    }
    //        auto condition_as_seq = dynamic_cast<IR_Begin*>(cne->entries.front().condition.get());
    //        ASSERT(condition_as_seq);
    //        if (condition_as_seq) {
    //          auto replacement = std::make_shared<IR_Begin>();
    //          replacement->forms = condition_as_seq->forms;
    //          ASSERT(condition.second == &condition_as_seq->forms.back());
    //          replacement->forms.pop_back();
    //          replacement->forms.push_back(std::make_shared<IR_Set>(
    //              IR_Set::REG_64, dst,
    //              std::make_shared<IR_Compare>(condition.first->condition, condition.first)));
    //          *ir = replacement;
    //        }
  }
}

void clean_up_cond_no_else_final(Function& func, CondNoElseElement* cne) {
  for (size_t idx = 0; idx < cne->entries.size(); idx++) {
    auto& entry = cne->entries.at(idx);
    if (entry.false_destination.has_value()) {
      auto fr = entry.false_destination;
      ASSERT(fr.has_value());
      cne->final_destination = *fr;
    } else {
      lg::print("failed to clean up cond_no_else_final: {}\n", func.name());
      ASSERT(false);
    }
  }

  auto last_branch = dynamic_cast<BranchElement*>(cne->entries.back().original_condition_branch);
  ASSERT(last_branch);

  if (func.ir2.env.has_reg_use()) {
    auto& last_branch_info = func.ir2.env.reg_use().op.at(last_branch->op()->op_id());
    cne->used_as_value = last_branch_info.written_and_unused.find(cne->final_destination.reg()) ==
                         last_branch_info.written_and_unused.end();
  }

  // check that all other delay slot writes are unused.
  for (size_t i = 0; i < cne->entries.size() - 1; i++) {
    if (func.ir2.env.has_reg_use()) {
      auto branch = dynamic_cast<BranchElement*>(cne->entries.at(i).original_condition_branch);
      auto& branch_info_i = func.ir2.env.reg_use().op.at(branch->op()->op_id());
      auto reg = cne->entries.at(i).false_destination;
      ASSERT(reg.has_value());
      ASSERT(branch);
      if (branch_info_i.written_and_unused.find(reg->reg()) ==
          branch_info_i.written_and_unused.end()) {
        lg::error("Branch delay register used improperly: {}", reg->to_string(func.ir2.env));
        throw std::runtime_error("Bad delay slot in clean_up_cond_no_else_final: OP " +
                                 std::to_string(branch->op()->op_id()));
      }
      // ASSERT(branch_info_i.written_and_unused.find(reg->reg()) !=
      //       branch_info_i.written_and_unused.end());
    }
  }

  if (func.ir2.env.has_local_vars()) {
    for (size_t i = 0; i < cne->entries.size(); i++) {
      if (func.ir2.env.has_reg_use()) {
        auto reg = cne->entries.at(i).false_destination;
        // lg::warn("Disable def of {} at {}", reg->to_string(func.ir2.env), reg->idx());
        func.ir2.env.disable_def(*reg, func.warnings);
      }
    }
  }
}

/*!
 * Replace internal branches inside a CondNoElse IR.
 * If possible will simplify the entire expression into a comparison operation if possible.
 * Will record which registers are set to false in branch delay slots.
 * The exact behavior here isn't really clear to me. It's possible that these delay set false
 * were disabled in cases where the result of the cond was none, or was a number or something.
 * But it generally seems inconsistent.  The expression propagation step will have to deal with
 * this.
 */
void clean_up_cond_no_else(FormPool& pool, Function& f, FormElement** ir_loc, Form* parent_form) {
  auto cne = dynamic_cast<CondNoElseElement*>(*ir_loc);
  ASSERT(cne);
  for (size_t idx = 0; idx < cne->entries.size(); idx++) {
    auto& e = cne->entries.at(idx);
    if (e.cleaned) {
      continue;
    }

    auto jump_to_next = get_condition_branch(e.condition);
    ASSERT(jump_to_next.first);

    if (jump_to_next.first->op()->branch_delay().kind() == IR2_BranchDelay::Kind::SET_REG_TRUE &&
        cne->entries.size() == 1) {
      convert_cond_no_else_to_compare(pool, f, ir_loc, parent_form);
      return;
    } else {
      ASSERT(jump_to_next.first->op()->branch_delay().kind() ==
                 IR2_BranchDelay::Kind::SET_REG_FALSE ||
             jump_to_next.first->op()->branch_delay().kind() == IR2_BranchDelay::Kind::NOP);
      ASSERT(jump_to_next.first->op()->condition().kind() != IR2_Condition::Kind::ALWAYS);

      if (jump_to_next.first->op()->branch_delay().kind() == IR2_BranchDelay::Kind::SET_REG_FALSE) {
        ASSERT(!e.false_destination);
        e.false_destination = jump_to_next.first->op()->branch_delay().var(0);
        ASSERT(e.false_destination);
      }

      e.original_condition_branch = *jump_to_next.second;

      auto replacement = jump_to_next.first->op()->get_condition_as_form(pool, f.ir2.env);
      replacement->invert();
      *(jump_to_next.second) = replacement;
      e.cleaned = true;

      if (idx != cne->entries.size() - 1) {
        auto jump_to_end = get_condition_branch(e.body);
        ASSERT(jump_to_end.first);
        ASSERT(jump_to_end.first->op()->branch_delay().kind() == IR2_BranchDelay::Kind::NOP);
        ASSERT(jump_to_end.first->op()->condition().kind() == IR2_Condition::Kind::ALWAYS);
        auto as_end_of_sequence = get_condition_branch_as_vector(e.body);
        if (as_end_of_sequence.first) {
          ASSERT(as_end_of_sequence.second->size() > 1);
          as_end_of_sequence.second->pop_back();
        } else {
          *(jump_to_end.second) = pool.alloc_element<EmptyElement>();
        }
      }
    }
  }
}

/*!
 * Match for a (set! reg (math reg reg)) form
 */
bool is_op_3(FormElement* ir,
             MatchParam<SimpleExpression::Kind> kind,
             MatchParam<Register> dst,
             MatchParam<Register> src0,
             MatchParam<Register> src1,
             Register* dst_out = nullptr,
             Register* src0_out = nullptr,
             Register* src1_out = nullptr) {
  // should be a set reg to int math 2 ir
  auto set = dynamic_cast<SetVarElement*>(ir);
  if (!set) {
    return false;
  }

  // destination should be a register
  auto dest = set->dst();
  if (dst != dest.reg()) {
    return false;
  }

  auto math = dynamic_cast<const SimpleExpressionElement*>(set->src()->try_as_single_element());
  if (!math || kind != math->expr().kind()) {
    return false;
  }

  if (get_simple_expression_arg_count(math->expr().kind()) != 2) {
    return false;
  }

  auto arg0 = math->expr().get_arg(0);
  auto arg1 = math->expr().get_arg(1);

  if (!arg0.is_var() || src0 != arg0.var().reg() || !arg1.is_var() || src1 != arg1.var().reg()) {
    return false;
  }

  // it's a match!
  if (dst_out) {
    *dst_out = dest.reg();
  }

  if (src0_out) {
    *src0_out = arg0.var().reg();
  }

  if (src1_out) {
    *src1_out = arg1.var().reg();
  }
  return true;
}

bool is_op_3(AtomicOp* op,
             MatchParam<SimpleExpression::Kind> kind,
             MatchParam<Register> dst,
             MatchParam<Register> src0,
             MatchParam<Register> src1,
             Register* dst_out = nullptr,
             Register* src0_out = nullptr,
             Register* src1_out = nullptr) {
  // should be a set reg to int math 2 ir
  auto set = dynamic_cast<SetVarOp*>(op);
  if (!set) {
    return false;
  }

  // destination should be a register
  auto dest = set->dst();
  if (dst != dest.reg()) {
    return false;
  }

  auto math = set->src();
  if (kind != math.kind()) {
    return false;
  }

  if (get_simple_expression_arg_count(math.kind()) != 2) {
    return false;
  }

  auto arg0 = math.get_arg(0);
  auto arg1 = math.get_arg(1);

  if (!arg0.is_var() || src0 != arg0.var().reg() || !arg1.is_var() || src1 != arg1.var().reg()) {
    return false;
  }

  // it's a match!
  if (dst_out) {
    *dst_out = dest.reg();
  }

  if (src0_out) {
    *src0_out = arg0.var().reg();
  }

  if (src1_out) {
    *src1_out = arg1.var().reg();
  }
  return true;
}

bool is_op_2(FormElement* ir,
             MatchParam<SimpleExpression::Kind> kind,
             MatchParam<Register> dst,
             MatchParam<Register> src0,
             Register* dst_out = nullptr,
             Register* src0_out = nullptr) {
  // should be a set reg to int math 2 ir
  auto set = dynamic_cast<SetVarElement*>(ir);
  if (!set) {
    return false;
  }

  // destination should be a register
  auto dest = set->dst();
  if (dst != dest.reg()) {
    return false;
  }

  auto math = dynamic_cast<const SimpleExpressionElement*>(set->src()->try_as_single_element());
  if (!math || kind != math->expr().kind()) {
    return false;
  }

  auto arg = math->expr().get_arg(0);

  if (!arg.is_var() || src0 != arg.var().reg()) {
    return false;
  }

  // it's a match!
  if (dst_out) {
    *dst_out = dest.reg();
  }

  if (src0_out) {
    *src0_out = arg.var().reg();
  }

  return true;
}

/*!
 * Try to convert this SC Vertex into an abs (integer).
 * Will return a converted abs IR if successful, or nullptr if its not possible
 */
Form* try_sc_as_abs(FormPool& pool, Function& f, const ShortCircuit* vtx) {
  if (vtx->entries.size() != 1) {
    return nullptr;
  }

  auto b0_c = vtx->entries.at(0).condition;
  auto b0_d = dynamic_cast<BlockVtx*>(vtx->entries.at(0).likely_delay);
  if (!b0_c || !b0_d) {
    return nullptr;
  }

  auto b0_ptr = cfg_to_ir(pool, f, b0_c);
  //  auto b0_ir = dynamic_cast<IR_Begin*>(b0_ptr.get());

  BranchElement* branch = dynamic_cast<BranchElement*>(b0_ptr->back());

  if (!branch) {
    return nullptr;
  }

  auto delay_start = f.ir2.atomic_ops->block_id_to_first_atomic_op.at(b0_d->block_id);
  auto delay_end = f.ir2.atomic_ops->block_id_to_end_atomic_op.at(b0_d->block_id);
  if (delay_end - delay_start != 1) {
    return nullptr;
  }
  auto& delay_op = f.ir2.atomic_ops->ops.at(delay_start);
  auto* delay = dynamic_cast<SetVarOp*>(delay_op.get());

  // check the branch instruction
  if (!branch->op()->likely() ||
      branch->op()->condition().kind() != IR2_Condition::Kind::LESS_THAN_ZERO_SIGNED ||
      !is_op_2(delay, SimpleExpression::Kind::NEG, {}, {})) {
    // todo - if there was an abs(unsigned), it would be missed here.
    return nullptr;
  }

  auto input = branch->op()->condition().src(0);
  auto output = delay->dst();

  ASSERT(input.is_var());
  ASSERT(input.var().reg() == delay->src().get_arg(0).var().reg());

  // remove the branch
  b0_ptr->pop_back();
  // add the ash
  auto& info = f.ir2.env.reg_use();
  auto final_op_idx = input.var().idx();
  RegSet consumed = info.op.at(final_op_idx).consumes;

  if (output.reg() == input.var().reg()) {
    consumed.insert(output.reg());
  }

  auto src_abs = pool.alloc_single_element_form<AbsElement>(nullptr, input.var(), consumed);
  auto replacement = pool.alloc_element<SetVarElement>(output, src_abs, true, TypeSpec("int"));
  b0_ptr->push_back(replacement);

  return b0_ptr;
}

/*!
 * Attempt to convert a short circuit expression into an arithmetic shift.
 * GOAL's shift function accepts positive/negative numbers to determine the direction
 * of the shift.
 */
Form* try_sc_as_ash(FormPool& pool, Function& f, const ShortCircuit* vtx) {
  if (vtx->entries.size() != 2) {
    return nullptr;
  }

  // todo, I think b0 could possibly be something more complicated, depending on how we order.
  auto b0_c = vtx->entries.at(0).condition;
  auto b0_d = dynamic_cast<BlockVtx*>(vtx->entries.at(0).likely_delay);
  auto b1 = dynamic_cast<BlockVtx*>(vtx->entries.at(1).condition);

  if (!b0_c || !b0_d || !b1 || vtx->entries.at(1).likely_delay) {
    return nullptr;
  }

  auto b0_c_ptr = cfg_to_ir(pool, f, b0_c);
  auto b1_ptr = cfg_to_ir(pool, f, b1);

  auto delay_start = f.ir2.atomic_ops->block_id_to_first_atomic_op.at(b0_d->block_id);
  auto delay_end = f.ir2.atomic_ops->block_id_to_end_atomic_op.at(b0_d->block_id);
  if (delay_end - delay_start != 1) {
    return nullptr;
  }
  auto& delay_op = f.ir2.atomic_ops->ops.at(delay_start);
  auto* delay = dynamic_cast<SetVarOp*>(delay_op.get());

  auto branch = dynamic_cast<BranchElement*>(b0_c_ptr->back());
  if (!branch || b1_ptr->size() != 2) {
    return nullptr;
  }

  // check the branch instruction
  if (!branch->op()->likely() ||
      branch->op()->condition().kind() != IR2_Condition::Kind::GEQ_ZERO_SIGNED ||
      !is_op_3(delay, SimpleExpression::Kind::LEFT_SHIFT, {}, {}, {})) {
    return nullptr;
  }

  /*
   *  bgezl s5, L109    ; s5 is the shift amount
      dsllv a0, a0, s5  ; a0 is both input and output here

      dsubu a1, r0, s5  ; a1 is a temp here
      dsrav a0, a0, a1  ; a0 is both input and output here
   */

  auto sa_in = branch->op()->condition().src(0);
  ASSERT(sa_in.is_var());
  auto result = delay->dst();
  auto value_in = delay->src().get_arg(0).var();
  auto sa_in2 = delay->src().get_arg(1).var();
  ASSERT(sa_in.var().reg() == sa_in2.reg());

  auto dsubu_candidate = b1_ptr->at(0);
  auto dsrav_candidate = b1_ptr->at(1);

  Register clobber;
  if (!is_op_2(dsubu_candidate, SimpleExpression::Kind::NEG, {}, sa_in.var().reg(), &clobber)) {
    return nullptr;
  }

  bool is_arith = is_op_3(dsrav_candidate, SimpleExpression::Kind::RIGHT_SHIFT_ARITH, result.reg(),
                          value_in.reg(), clobber);
  bool is_logical = is_op_3(dsrav_candidate, SimpleExpression::Kind::RIGHT_SHIFT_LOGIC,
                            result.reg(), value_in.reg(), clobber);

  if (!is_arith && !is_logical) {
    return nullptr;
  }

  std::optional<RegisterAccess> clobber_ir;
  auto dsubu_set = dynamic_cast<SetVarElement*>(dsubu_candidate);
  auto dsrav_set = dynamic_cast<SetVarElement*>(dsrav_candidate);
  ASSERT(dsubu_set && dsrav_set);
  if (clobber != result.reg()) {
    clobber_ir = dsubu_set->dst();
  }

  RegisterAccess dest_ir = result;
  SimpleAtom shift_ir = branch->op()->condition().src(0);
  auto value_ir =
      dynamic_cast<const SimpleExpressionElement*>(dsrav_set->src()->try_as_single_element())
          ->expr()
          .get_arg(0);

  // remove the branch
  b0_c_ptr->pop_back();

  auto& info = f.ir2.env.reg_use();
  auto final_op_idx = value_ir.var().idx();
  RegSet consumed = info.op.at(final_op_idx).consumes;
  for (auto var : {shift_ir.var(), value_ir.var()}) {
    if (var.reg() == clobber) {
      consumed.insert(var.reg());
    }
    if (var.reg() == dest_ir.reg()) {
      consumed.insert(var.reg());
    }
  }

  // setup
  auto ash_form = pool.alloc_single_element_form<AshElement>(
      nullptr, shift_ir.var(), value_ir.var(), clobber_ir, is_arith, consumed);
  auto set_form = pool.alloc_element<SetVarElement>(dest_ir, ash_form, true, TypeSpec("int"));
  b0_c_ptr->push_back(set_form);

  // fix up reg info
  f.ir2.env.disable_use(delay->src().get_arg(0).var());

  // fix up the other ones:
  f.ir2.env.disable_use(branch->op()->condition().src(0).var());  // bgezl X, L
  auto dsubu_var = dynamic_cast<SimpleExpressionElement*>(dsubu_set->src()->try_as_single_element())
                       ->expr()
                       .get_arg(0)
                       .var();
  f.ir2.env.disable_use(dsubu_var);

  // and the def too
  f.ir2.env.disable_def(dsrav_set->dst(), f.warnings);

  return b0_c_ptr;
}

bool is_set_symbol_value(SetVarOp& op, const std::string& name) {
  return op.src().is_identity() && op.src().get_arg(0).is_sym_val() &&
         op.src().get_arg(0).get_str() == name;
}

bool is_set_symbol_value(SetVarElement* op, const std::string& name) {
  auto src = op->src()->try_as_element<SimpleExpressionElement>();
  if (!src) {
    return false;
  }
  return src->expr().is_identity() && src->expr().get_arg(0).is_sym_val() &&
         src->expr().get_arg(0).get_str() == name;
}

SetVarOp get_delay_op(const Function& f, const BlockVtx* vtx) {
  auto delay_start = f.ir2.atomic_ops->block_id_to_first_atomic_op.at(vtx->block_id);
  auto delay_end = f.ir2.atomic_ops->block_id_to_end_atomic_op.at(vtx->block_id);
  if (delay_end - delay_start != 1) {
    ASSERT(false);
  }
  auto& delay_op = f.ir2.atomic_ops->ops.at(delay_start);
  auto* delay = dynamic_cast<SetVarOp*>(delay_op.get());
  if (!delay) {
    lg::print("bad delay: {}\n", delay_op->to_string(f.ir2.env));
    ASSERT(false);
  }
  return *delay;
}

LoadVarOp get_delay_load_op(const Function& f, const BlockVtx* vtx) {
  auto delay_start = f.ir2.atomic_ops->block_id_to_first_atomic_op.at(vtx->block_id);
  auto delay_end = f.ir2.atomic_ops->block_id_to_end_atomic_op.at(vtx->block_id);
  if (delay_end - delay_start != 1) {
    ASSERT(false);
  }
  auto& delay_op = f.ir2.atomic_ops->ops.at(delay_start);
  auto* delay = dynamic_cast<LoadVarOp*>(delay_op.get());
  if (!delay) {
    lg::print("bad delay: {}\n", delay_op->to_string(f.ir2.env));
    ASSERT(false);
  }
  return *delay;
}

Form* try_sc_as_type_of_jak2(FormPool& pool, Function& f, const ShortCircuit* vtx) {
  /*
    dsll32 a2, a0, 29     * temp_reg0, src
    dsrl32 a2, a2, 29     * temp_reg0, temp_reg0
    beql a2, r0, L289     branch0
  B1:
    lw a0, binteger(s7)

  B2:
    addiu a3, r0, 4
    beql a2, a3, L289     branch1
  B3:
    lwu a0, -4(a0)

  B4:
    addiu a0, r0, 2
    beql a2, a0, L289     branch2
  B5:
    lw a0, pair(s7)

  B6:
    lw a0, symbol(s7)
  B7:
  L289:
 */

  if (vtx->entries.size() != 4) {
    return nullptr;
  }

  auto b0_c = dynamic_cast<CfgVtx*>(vtx->entries.at(0).condition);
  auto b0_d = dynamic_cast<BlockVtx*>(vtx->entries.at(0).likely_delay);
  auto b1_c = dynamic_cast<BlockVtx*>(vtx->entries.at(1).condition);
  auto b1_d = dynamic_cast<BlockVtx*>(vtx->entries.at(1).likely_delay);
  auto b2_c = dynamic_cast<BlockVtx*>(vtx->entries.at(2).condition);
  auto b2_d = dynamic_cast<BlockVtx*>(vtx->entries.at(2).likely_delay);
  auto b3_c = dynamic_cast<BlockVtx*>(vtx->entries.at(3).condition);

  if (!b0_c || !b0_d || !b1_c || !b1_d || !b2_c || !b2_d || !b3_c ||
      vtx->entries.at(3).likely_delay) {
    return nullptr;
  }

  // get the branch ir's
  auto b0_ptr = cfg_to_ir(pool, f, b0_c);  // should be begin.
  if (b0_ptr->size() <= 2) {
    return nullptr;
  }
  auto b1_ptr = cfg_to_ir(pool, f, b1_c);
  if (b1_ptr->size() <= 1) {
    return nullptr;
  }
  auto b2_ptr = cfg_to_ir(pool, f, b2_c);
  if (b2_ptr->size() <= 1) {
    return nullptr;
  }
  auto b3_ptr = cfg_to_ir(pool, f, b3_c);
  auto b3_ir = dynamic_cast<SetVarElement*>(b3_ptr->try_as_single_element());
  if (!b3_ir) {
    return nullptr;
  }

  // identify the left shift
  auto set_shift_left = dynamic_cast<SetVarElement*>(b0_ptr->at(b0_ptr->size() - 3));
  if (!set_shift_left) {
    return nullptr;
  }
  auto temp_reg0 = set_shift_left->dst();
  auto shift_left =
      dynamic_cast<SimpleExpressionElement*>(set_shift_left->src()->try_as_single_element());
  if (!shift_left || shift_left->expr().kind() != SimpleExpression::Kind::LEFT_SHIFT) {
    return nullptr;
  }
  auto src_reg = shift_left->expr().get_arg(0).var();
  auto sa_left = shift_left->expr().get_arg(1);
  if (!sa_left.is_int() || sa_left.get_int() != 61) {
    return nullptr;
  }

  // identify the right shift
  auto set_shift_right = dynamic_cast<SetVarElement*>(b0_ptr->at(b0_ptr->size() - 2));
  if (!set_shift_right) {
    return nullptr;
  }
  if (set_shift_right->dst().reg() != set_shift_left->dst().reg()) {
    return nullptr;
  }
  auto shift_right =
      dynamic_cast<SimpleExpressionElement*>(set_shift_right->src()->try_as_single_element());
  if (!shift_right || shift_right->expr().kind() != SimpleExpression::Kind::RIGHT_SHIFT_LOGIC) {
    return nullptr;
  }
  if (temp_reg0.reg() != shift_right->expr().get_arg(0).var().reg()) {
    return nullptr;
  }
  auto sa_right = shift_right->expr().get_arg(1);
  if (!sa_right.is_int() || sa_right.get_int() != 61) {
    return nullptr;
  }

  // branch 0
  auto first_branch = dynamic_cast<BranchElement*>(b0_ptr->back());
  auto b0_delay_op = get_delay_op(f, b0_d);
  if (!first_branch || !is_set_symbol_value(b0_delay_op, "binteger") ||
      first_branch->op()->condition().kind() != IR2_Condition::Kind::ZERO ||
      !first_branch->op()->likely()) {
    return nullptr;
  }
  auto temp_reg = first_branch->op()->condition().src(0).var();
  ASSERT(temp_reg.reg() == temp_reg0.reg());
  auto dst_reg = b0_delay_op.dst();

  // branch 1
  if (b1_ptr->size() != 2) {
    return nullptr;
  }
  auto second_branch_pre_op = dynamic_cast<SetVarElement*>(b1_ptr->at(0));
  if (!second_branch_pre_op) {
    return nullptr;
  }
  {
    auto pos = second_branch_pre_op->src();
    auto pos_as_se = pos->try_as_element<SimpleExpressionElement>();
    if (!pos_as_se || !pos_as_se->expr().is_identity() || !pos_as_se->expr().get_arg(0).is_int(4)) {
      return nullptr;
    }
  }
  auto temp_reg1 = second_branch_pre_op->dst();
  auto second_branch = dynamic_cast<BranchElement*>(b1_ptr->at(1));
  /*
    beql a2, a3, L289     branch1
  B3:
    lwu a0, -4(a0)
   */
  if (!second_branch || second_branch->op()->condition().kind() != IR2_Condition::Kind::EQUAL ||
      !second_branch->op()->likely() || !second_branch->op()->condition().src(0).is_var() ||
      second_branch->op()->condition().src(0).var().reg() != temp_reg0.reg() ||
      !second_branch->op()->condition().src(1).is_var() ||
      second_branch->op()->condition().src(1).var().reg() != temp_reg1.reg()) {
    return nullptr;
  }

  if (!b1_d) {
    return nullptr;
  }
  auto b1_delay_op = get_delay_load_op(f, b1_d);
  if (b1_delay_op.kind() != LoadVarOp::Kind::UNSIGNED || b1_delay_op.size() != 4) {
    return nullptr;
  }
  IR2_RegOffset ro;
  if (!get_as_reg_offset(b1_delay_op.src(), &ro)) {
    return nullptr;
  }
  if (ro.offset != -4) {
    return nullptr;
  }
  if (ro.reg != src_reg.reg() || b1_delay_op.get_set_destination().reg() != dst_reg.reg()) {
    return nullptr;
  }

  /////////////////////////////////
  // branch2
  /*
   *   B4:
    addiu a0, r0, 2
    beql a2, a0, L289     branch2
  B5:
    lw a0, pair(s7)
   */
  if (b2_ptr->size() != 2) {
    return nullptr;
  }
  auto third_branch_pre_op = dynamic_cast<SetVarElement*>(b2_ptr->at(0));
  if (!third_branch_pre_op) {
    return nullptr;
  }
  {
    auto pos = third_branch_pre_op->src();
    auto pos_as_se = pos->try_as_element<SimpleExpressionElement>();
    if (!pos_as_se || !pos_as_se->expr().is_identity() || !pos_as_se->expr().get_arg(0).is_int(2)) {
      return nullptr;
    }
  }
  auto temp_reg2 = third_branch_pre_op->dst();
  auto third_branch = dynamic_cast<BranchElement*>(b2_ptr->at(1));
  if (!third_branch || third_branch->op()->condition().kind() != IR2_Condition::Kind::EQUAL ||
      !third_branch->op()->likely() || !third_branch->op()->condition().src(0).is_var() ||
      third_branch->op()->condition().src(0).var().reg() != temp_reg0.reg() ||
      !third_branch->op()->condition().src(1).is_var() ||
      third_branch->op()->condition().src(1).var().reg() != temp_reg2.reg()) {
    return nullptr;
  }

  if (!b2_d) {
    return nullptr;
  }
  auto b2_delay_op = get_delay_op(f, b2_d);
  if (!is_set_symbol_value(b2_delay_op, "pair") || b2_delay_op.dst().reg() != dst_reg.reg()) {
    return nullptr;
  }

  if (!is_set_symbol_value(b3_ir, "symbol")) {
    return nullptr;
  }

  // we passed, time to clean things up...

  // remove the stuff from the back of block 0.
  b0_ptr->pop_back();
  b0_ptr->pop_back();
  b0_ptr->pop_back();

  auto obj = pool.alloc_single_element_form<SimpleExpressionElement>(
      nullptr, shift_left->expr().get_arg(0).as_expr(), set_shift_right->dst().idx());
  auto type_op = pool.alloc_single_element_form<TypeOfElement>(nullptr, obj);
  auto op = pool.alloc_element<SetVarElement>(dst_reg, type_op, true, TypeSpec("type"));
  b0_ptr->push_back(op);
  // if (b1_delay_op.src().)

  f.ir2.env.disable_def(b0_delay_op.dst(), f.warnings);
  f.ir2.env.disable_def(b1_delay_op.get_set_destination(), f.warnings);
  f.ir2.env.disable_def(b2_delay_op.dst(), f.warnings);
  f.ir2.env.disable_use(shift_left->expr().get_arg(0).var());

  f.warnings.warning("Using new Jak 2 rtype-of");
  return b0_ptr;
}

/*!
 * Try to convert a short circuiting expression into a "type-of" expression.
 * We do this before attempting the normal and/or expressions.
 */
Form* try_sc_as_type_of_jak1(FormPool& pool, Function& f, const ShortCircuit* vtx) {
  // the assembly looks like this:
  /*
         dsll32 v1, a0, 29                   ;; (set! v1 (shl a0 61))
         beql v1, r0, L60                    ;; (bl! (= v1 r0) L60 (unknown-branch-delay))
         lw v1, binteger(s7)

         bgtzl v1, L60                       ;; (bl! (>0.s v1) L60 (unknown-branch-delay))
         lw v1, pair(s7)

         lwu v1, -4(a0)                      ;; (set! v1 (l.wu (+.i a0 -4)))
     L60:
   */

  // some of these checks may be a little bit overkill but it's a nice way to sanity check that
  // we have actually decoded everything correctly.
  if (vtx->entries.size() != 3) {
    return nullptr;
  }

  auto b0_c = dynamic_cast<CfgVtx*>(vtx->entries.at(0).condition);
  auto b0_d = dynamic_cast<BlockVtx*>(vtx->entries.at(0).likely_delay);
  auto b1_c = dynamic_cast<BlockVtx*>(vtx->entries.at(1).condition);
  auto b1_d = dynamic_cast<BlockVtx*>(vtx->entries.at(1).likely_delay);
  auto b2_c = dynamic_cast<BlockVtx*>(vtx->entries.at(2).condition);

  if (!b0_c || !b0_d || !b1_c || !b1_d || !b2_c || vtx->entries.at(2).likely_delay) {
    return nullptr;
  }

  auto b0_ptr = cfg_to_ir(pool, f, b0_c);  // should be begin.
  if (b0_ptr->size() <= 1) {
    return nullptr;
  }

  auto b1_ptr = cfg_to_ir(pool, f, b1_c);
  auto b1_ir = dynamic_cast<BranchElement*>(b1_ptr->try_as_single_element());

  auto b2_ptr = cfg_to_ir(pool, f, b2_c);
  auto b2_ir = dynamic_cast<SetVarElement*>(b2_ptr->try_as_single_element());
  if (!b1_ir || !b2_ir) {
    return nullptr;
  }

  auto set_shift = dynamic_cast<SetVarElement*>(b0_ptr->at(b0_ptr->size() - 2));
  if (!set_shift) {
    return nullptr;
  }

  auto temp_reg0 = set_shift->dst();

  auto shift = dynamic_cast<SimpleExpressionElement*>(set_shift->src()->try_as_single_element());
  if (!shift || shift->expr().kind() != SimpleExpression::Kind::LEFT_SHIFT) {
    return nullptr;
  }
  auto src_reg = shift->expr().get_arg(0).var();
  auto sa = shift->expr().get_arg(1);
  if (!sa.is_int() || sa.get_int() != 61) {
    return nullptr;
  }

  auto first_branch = dynamic_cast<BranchElement*>(b0_ptr->back());
  auto second_branch = b1_ir;
  auto else_case = b2_ir;

  auto b0_delay_op = get_delay_op(f, b0_d);
  if (!first_branch || !is_set_symbol_value(b0_delay_op, "binteger") ||
      first_branch->op()->condition().kind() != IR2_Condition::Kind::ZERO ||
      !first_branch->op()->likely()) {
    return nullptr;
  }
  auto temp_reg = first_branch->op()->condition().src(0).var();
  ASSERT(temp_reg.reg() == temp_reg0.reg());
  auto dst_reg = b0_delay_op.dst();

  auto b1_delay_op = get_delay_op(f, b1_d);
  if (!second_branch || !is_set_symbol_value(b1_delay_op, "pair") ||
      second_branch->op()->condition().kind() != IR2_Condition::Kind::GREATER_THAN_ZERO_SIGNED ||
      !second_branch->op()->likely()) {
    return nullptr;
  }

  // check we agree on destination register.
  auto dst_reg2 = b1_delay_op.dst();
  ASSERT(dst_reg2.reg() == dst_reg.reg());

  // else case is a lwu to grab the type from a basic
  ASSERT(else_case);
  auto dst_reg3 = else_case->dst();
  ASSERT(dst_reg3.reg() == dst_reg.reg());
  auto load_op = dynamic_cast<LoadSourceElement*>(else_case->src()->try_as_single_element());
  if (!load_op || load_op->kind() != LoadVarOp::Kind::UNSIGNED || load_op->size() != 4) {
    return nullptr;
  }
  auto load_loc =
      dynamic_cast<SimpleExpressionElement*>(load_op->location()->try_as_single_element());
  if (!load_loc || load_loc->expr().kind() != SimpleExpression::Kind::ADD) {
    return nullptr;
  }
  auto src_reg3 = load_loc->expr().get_arg(0);
  auto offset = load_loc->expr().get_arg(1);
  if (!src_reg3.is_var() || !offset.is_int()) {
    return nullptr;
  }

  ASSERT(src_reg3.var().reg() == src_reg.reg());
  ASSERT(offset.get_int() == -4);

  // remove the branch
  b0_ptr->pop_back();
  // remove the shift
  b0_ptr->pop_back();

  // add the type-of
  auto obj = pool.alloc_single_element_form<SimpleExpressionElement>(
      nullptr, shift->expr().get_arg(0).as_expr(), set_shift->dst().idx());
  auto type_op = pool.alloc_single_element_form<TypeOfElement>(nullptr, obj);
  auto op = pool.alloc_element<SetVarElement>(else_case->dst(), type_op, true, TypeSpec("type"));
  b0_ptr->push_back(op);

  // fix register info
  f.ir2.env.disable_def(b0_delay_op.dst(), f.warnings);
  f.ir2.env.disable_def(b1_delay_op.dst(), f.warnings);
  f.ir2.env.disable_use(shift->expr().get_arg(0).var());

  return b0_ptr;
}

Form* try_sc_as_type_of(FormPool& pool, Function& f, const ShortCircuit* vtx, GameVersion version) {
  switch (version) {
    case GameVersion::Jak1:
      return try_sc_as_type_of_jak1(pool, f, vtx);
    case GameVersion::Jak2:
    case GameVersion::Jak3:
      return try_sc_as_type_of_jak2(pool, f, vtx);
    default:
      ASSERT(false);
      return nullptr;
  }
}

Form* merge_cond_else_with_sc_cond(FormPool& pool,
                                   Function& f,
                                   const CondWithElse* cwe,
                                   Form* else_ir) {
  if (else_ir->size() != 2) {
    return nullptr;
  }

  auto first = dynamic_cast<ShortCircuitElement*>(else_ir->at(0));
  auto second = dynamic_cast<CondNoElseElement*>(else_ir->at(1));
  if (!first || !second) {
    return nullptr;
  }

  std::vector<CondNoElseElement::Entry> entries;
  for (auto& x : cwe->entries) {
    CondNoElseElement::Entry e;
    e.condition = cfg_to_ir(pool, f, x.condition);
    e.body = cfg_to_ir(pool, f, x.body);
    entries.push_back(std::move(e));
  }

  auto first_condition = pool.alloc_empty_form();
  first_condition->push_back(else_ir->at(0));
  for (auto& x : second->entries.front().condition->elts()) {
    first_condition->push_back(x);
  }

  second->entries.front().condition = first_condition;

  for (auto& x : second->entries) {
    entries.push_back(x);
  }
  auto result = pool.alloc_single_element_form<CondNoElseElement>(nullptr, entries);
  clean_up_cond_no_else(pool, f, result->back_ref(), result);
  return result;
}

namespace {
template <typename T>
bool contains(const std::vector<T>& vec, const T& val) {
  for (auto& x : vec) {
    if (val == x) {
      return true;
    }
  }
  return false;
}
}  // namespace

/*!
 * Push x to output (can be a Form or std::vector<FormElement>).
 * Will take of grouping the delay slots for likely asm branches into a single operation.
 */
template <typename T>
void push_back_form_regroup_asm_likely_branches(T* output, FormElement* x, Function& f) {
  std::vector<FormElement*> hack_temp;

  if (output->size() > 0) {
    auto back_as_asm = dynamic_cast<AtomicOpElement*>(output->back());
    if (back_as_asm) {
      auto back_as_branch = dynamic_cast<AsmBranchOp*>(back_as_asm->op());
      if (back_as_branch && back_as_branch->is_likely()) {
        auto& pool = *f.ir2.form_pool;
        auto elt = pool.alloc_element<AsmBranchElement>(back_as_branch,
                                                        pool.alloc_single_form(nullptr, x), true);
        output->pop_back();
        output->push_back(elt);
        return;
      }
    }
  }
  output->push_back(x);
}

template <typename T>
void convert_and_inline(FormPool& pool, Function& f, const BlockVtx* as_block, T* output) {
  auto start_op = f.ir2.atomic_ops->block_id_to_first_atomic_op.at(as_block->block_id);
  auto end_op = f.ir2.atomic_ops->block_id_to_end_atomic_op.at(as_block->block_id);
  std::vector<int> add_map;
  for (auto i = start_op; i < end_op; i++) {
    // convert to a form.
    auto op = f.ir2.atomic_ops->ops.at(i)->get_as_form(pool, f.ir2.env);
    bool add = true;

    // check if we got a set, which we might want to eliminate
    auto op_as_set = dynamic_cast<SetVarElement*>(op);
    if (op_as_set) {
      // check for deadness

      if (op_as_set->is_dead_set()) {
        // we want to eliminate, but we should we fix up the register info.
        // now adding.
        // add = false;
        auto consumed_expr =
            dynamic_cast<SimpleExpressionElement*>(op_as_set->src()->try_as_single_element());
        ASSERT(consumed_expr);
        auto& consumed = consumed_expr->expr().get_arg(0).var();
        auto& ri_outer = f.ir2.env.reg_use().op.at(consumed.idx());  // meh
        if (ri_outer.consumes.find(consumed.reg()) != ri_outer.consumes.end()) {
          for (int j = i; j-- > start_op;) {
            auto& ri = f.ir2.env.reg_use().op.at(j);
            auto& ao = f.ir2.atomic_ops->ops.at(j);
            if (contains(ao->write_regs(), consumed.reg())) {
              ri.written_and_unused.insert(consumed.reg());
              //            lg::print("GOT 3, making {} wau by {}\n", consumed.reg().to_charp(),
              //                       ao->to_string(f.ir2.env));
              // HACK - regenerate:
              if (add_map.at(j - start_op) != -1) {
                //              lg::print("regenerating {} to ", output->at(add_map.at(j -
                //              start_op))->to_string(f.ir2.env));
                output->at(add_map.at(j - start_op)) =
                    f.ir2.atomic_ops->ops.at(j)->get_as_form(pool, f.ir2.env);
                //              lg::print("{}\n", output->at(add_map.at(j -
                //              start_op))->to_string(f.ir2.env));
              }
              break;
            }

            if (contains(ao->read_regs(), consumed.reg())) {
              //            lg::print("GOT 2, making {} consumed by {}\n",
              //            consumed.reg().to_charp(),
              //                       ao->to_string(f.ir2.env));
              ri.consumes.insert(consumed.reg());
              break;
            }
          }
        }
      }
    }
    if (add) {
      add_map.push_back(output->size());
      // output->push_back(op);
      push_back_form_regroup_asm_likely_branches(output, op, f);
    } else {
      add_map.push_back(-1);
    }
  }
}

void insert_cfg_into_list(FormPool& pool,
                          Function& f,
                          const CfgVtx* vtx,
                          std::vector<FormElement*>* output) {
  auto as_sequence = dynamic_cast<const SequenceVtx*>(vtx);
  auto as_block = dynamic_cast<const BlockVtx*>(vtx);
  if (as_sequence) {
    // inline the sequence.
    if (as_sequence->needs_label) {
      output->push_back(pool.alloc_element<LabelElement>(vtx->get_first_block_id()));
    }
    for (auto& x : as_sequence->seq) {
      insert_cfg_into_list(pool, f, x, output);
    }
  } else if (as_block) {
    if (as_block->needs_label) {
      output->push_back(pool.alloc_element<LabelElement>(vtx->get_first_block_id()));
    }
    convert_and_inline(pool, f, as_block, output);
  } else {
    auto ir = cfg_to_ir(pool, f, vtx);
    for (auto x : ir->elts()) {
      push_back_form_regroup_asm_likely_branches(output, x, f);
      // output->push_back(x);
    }
  }
}

Form* cfg_to_ir_allow_null(FormPool& pool, Function& f, const CfgVtx* vtx) {
  if (vtx) {
    return cfg_to_ir(pool, f, vtx);
  } else {
    return pool.alloc_single_element_form<EmptyElement>(nullptr);
  }
}

Form* cfg_to_ir_helper(FormPool& pool, Function& f, const CfgVtx* vtx) {
  if (dynamic_cast<const BlockVtx*>(vtx)) {
    auto* bv = dynamic_cast<const BlockVtx*>(vtx);
    Form* output = pool.alloc_empty_form();
    convert_and_inline(pool, f, bv, output);
    return output;
  } else if (dynamic_cast<const SequenceVtx*>(vtx)) {
    auto* sv = dynamic_cast<const SequenceVtx*>(vtx);
    Form* output = pool.alloc_empty_form();
    insert_cfg_into_list(pool, f, sv, &output->elts());

    return output;
  } else if (dynamic_cast<const WhileLoop*>(vtx)) {
    auto wvtx = dynamic_cast<const WhileLoop*>(vtx);

    return pool.alloc_single_element_form<WhileElement>(
        nullptr, cfg_to_ir(pool, f, wvtx->condition), cfg_to_ir(pool, f, wvtx->body));
  } else if (dynamic_cast<const UntilLoop*>(vtx)) {
    auto wvtx = dynamic_cast<const UntilLoop*>(vtx);
    auto result = pool.alloc_single_element_form<UntilElement>(
        nullptr, cfg_to_ir(pool, f, wvtx->condition), cfg_to_ir(pool, f, wvtx->body));
    clean_up_until_loop(pool, dynamic_cast<UntilElement*>(result->try_as_single_element()),
                        f.ir2.env);
    return result;
  } else if (dynamic_cast<const UntilLoop_single*>(vtx)) {
    auto wvtx = dynamic_cast<const UntilLoop_single*>(vtx);
    auto empty = pool.alloc_single_element_form<EmptyElement>(nullptr);
    auto result = pool.alloc_single_element_form<UntilElement>(
        nullptr, cfg_to_ir(pool, f, wvtx->block), empty);
    clean_up_until_loop(pool, dynamic_cast<UntilElement*>(result->try_as_single_element()),
                        f.ir2.env);
    return result;
  } else if (dynamic_cast<const InfiniteLoopBlock*>(vtx)) {
    auto wvtx = dynamic_cast<const InfiniteLoopBlock*>(vtx);
    auto condition = pool.alloc_single_element_form<ConditionElement>(
        nullptr, IR2_Condition::Kind::ALWAYS, std::nullopt, std::nullopt, RegSet(), false);
    auto result = pool.alloc_single_element_form<WhileElement>(nullptr, condition,
                                                               cfg_to_ir(pool, f, wvtx->block));
    clean_up_infinite_while_loop(pool,
                                 dynamic_cast<WhileElement*>(result->try_as_single_element()));
    return result;
  } else if (dynamic_cast<const CondWithElse*>(vtx)) {
    auto* cvtx = dynamic_cast<const CondWithElse*>(vtx);

    // the cfg analysis pass may recognize some things out of order, which can cause
    // fake nesting. This is actually a problem at this point because it can turn a normal
    // cond into a cond with else, which emits different instructions.  This attempts to recognize
    // an else which is actually more cases and compacts it into a single statement.  At this point
    // I don't know if this is sufficient to catch all cases.  it may even recognize the wrong
    // thing in some cases... maybe we should check the delay slot instead?
    auto else_ir = cfg_to_ir(pool, f, cvtx->else_vtx);
    auto fancy_compact_result = merge_cond_else_with_sc_cond(pool, f, cvtx, else_ir);
    if (fancy_compact_result) {
      return fancy_compact_result;
    }

    // this case is disabled because I _think_ it is now properly handled elsewhere.
    if (false /*&& dynamic_cast<IR_Cond*>(else_ir.get())*/) {
      //      auto extra_cond = dynamic_cast<IR_Cond*>(else_ir.get());
      //      std::vector<IR_Cond::Entry> entries;
      //      for (auto& x : cvtx->entries) {
      //        IR_Cond::Entry e;
      //        e.condition = cfg_to_ir(f, file, x.condition);
      //        e.body = cfg_to_ir(f, file, x.body);
      //        entries.push_back(std::move(e));
      //      }
      //      for (auto& x : extra_cond->entries) {
      //        entries.push_back(x);
      //      }
      //      std::shared_ptr<IR> result = std::make_shared<IR_Cond>(entries);
      //      clean_up_cond_no_else(&result, file);
      //      return result;
    } else {
      std::vector<CondWithElseElement::Entry> entries;
      for (auto& x : cvtx->entries) {
        CondWithElseElement::Entry e;
        e.condition = cfg_to_ir(pool, f, x.condition);
        e.body = cfg_to_ir(pool, f, x.body);
        entries.push_back(std::move(e));
      }
      auto result = pool.alloc_single_element_form<CondWithElseElement>(nullptr, entries, else_ir);
      clean_up_cond_with_else(
          pool, dynamic_cast<CondWithElseElement*>(result->try_as_single_element()), f.ir2.env);
      return result;
    }
  } else if (dynamic_cast<const ShortCircuit*>(vtx)) {
    auto* svtx = dynamic_cast<const ShortCircuit*>(vtx);
    // try as a type of expression first
    auto as_type_of = try_sc_as_type_of(pool, f, svtx, f.ir2.env.version);
    if (as_type_of) {
      return as_type_of;
    }

    auto as_ash = try_sc_as_ash(pool, f, svtx);
    if (as_ash) {
      return as_ash;
    }

    auto as_abs = try_sc_as_abs(pool, f, svtx);
    if (as_abs) {
      return as_abs;
    }

    // now try as a normal and/or
    std::vector<ShortCircuitElement::Entry> entries;
    for (auto& x : svtx->entries) {
      ShortCircuitElement::Entry e;
      e.condition = cfg_to_ir(pool, f, x.condition);
      if (x.likely_delay) {
        auto delay = dynamic_cast<BlockVtx*>(x.likely_delay);
        ASSERT(delay);
        auto delay_start = f.ir2.atomic_ops->block_id_to_first_atomic_op.at(delay->block_id);
        auto delay_end = f.ir2.atomic_ops->block_id_to_end_atomic_op.at(delay->block_id);
        ASSERT(delay_end - delay_start == 1);
        auto& op = f.ir2.atomic_ops->ops.at(delay_start);
        auto op_as_expr = dynamic_cast<SetVarOp*>(op.get());
        if (!op_as_expr) {
          lg::print("bad in {}\n", f.name());
          lg::print("{}\n", op->to_string(f.ir2.env));
        }
        ASSERT(op_as_expr);
        e.branch_delay = *op_as_expr;
      }

      entries.push_back(e);
    }
    auto result = pool.alloc_single_element_form<ShortCircuitElement>(nullptr, entries);
    clean_up_sc(pool, f, dynamic_cast<ShortCircuitElement*>(result->try_as_single_element()));
    return result;
  } else if (dynamic_cast<const CondNoElse*>(vtx)) {
    auto* cvtx = dynamic_cast<const CondNoElse*>(vtx);
    std::vector<CondNoElseElement::Entry> entries;
    for (auto& x : cvtx->entries) {
      CondNoElseElement::Entry e;
      e.condition = cfg_to_ir(pool, f, x.condition);
      e.body = cfg_to_ir(pool, f, x.body);
      entries.push_back(std::move(e));
    }
    auto result = pool.alloc_single_element_form<CondNoElseElement>(nullptr, entries);
    clean_up_cond_no_else(pool, f, result->back_ref(), result);
    return result;
  } else if (dynamic_cast<const GotoEnd*>(vtx)) {
    auto* cvtx = dynamic_cast<const GotoEnd*>(vtx);

    // dead code should always be (set! var 0)
    auto dead_code = cfg_to_ir(pool, f, cvtx->unreachable_block);
    //    auto dead = dynamic_cast<SetVarElement*>(dead_code->try_as_single_element());
    //    if (!dead) {
    //      lg::error("failed to recognize dead code after return, got {}",
    //      dead_code->to_string(f.ir2.env));
    //    }
    //    ASSERT(dead);
    //    auto src = dynamic_cast<SimpleExpressionElement*>(dead->src()->try_as_single_element());
    //    ASSERT(src);
    //    ASSERT(src->expr().is_identity() && src->expr().get_arg(0).is_int() &&
    //           src->expr().get_arg(0).get_int() == 0);

    auto result = pool.alloc_single_element_form<ReturnElement>(
        nullptr, cfg_to_ir(pool, f, cvtx->body), dead_code);
    clean_up_return(pool, dynamic_cast<ReturnElement*>(result->try_as_single_element()));
    return result;
  } else if (dynamic_cast<const Break*>(vtx)) {
    auto* cvtx = dynamic_cast<const Break*>(vtx);
    auto result = pool.alloc_single_element_form<BreakElement>(
        nullptr, cfg_to_ir(pool, f, cvtx->body),
        cfg_to_ir_allow_null(pool, f, cvtx->unreachable_block), cvtx->dest_block_id);
    clean_up_break(pool, dynamic_cast<BreakElement*>(result->try_as_single_element()), f.ir2.env);
    return result;
  } else if (dynamic_cast<const EmptyVtx*>(vtx)) {
    return pool.alloc_single_element_form<EmptyElement>(nullptr);
  }

  return nullptr;
}

Form* cfg_to_ir(FormPool& pool, Function& f, const CfgVtx* vtx) {
  // we cache these because some functions will do a conversion, give up, and throw away the result.
  // converting multiple times means that env-modifications will happen multiple times.
  auto cached = pool.lookup_cached_conversion(vtx);
  if (cached) {
    return cached;
  }
  Form* result = cfg_to_ir_helper(pool, f, vtx);
  if (vtx->needs_label) {
    result->elts().insert(result->elts().begin(),
                          pool.alloc_element<LabelElement>(vtx->get_first_block_id()));
  }

  pool.cache_conversion(vtx, result);
  return result;
}

/*!
 * Post processing pass to clean up while loops - annoyingly the block before a while loop
 * has a jump to the condition branch that we need to remove.  This currently happens after all
 * conversion but this may need to be revisited depending on the final order of simplifications.
 */
void clean_up_while_loops(FormPool& pool, Form* sequence, const Env& env) {
  std::vector<size_t> to_remove;  // the list of branches to remove by index in this sequence
  for (int i = 0; i < sequence->size(); i++) {
    auto* form_as_while = dynamic_cast<WhileElement*>(sequence->at(i));
    if (form_as_while && !form_as_while->cleaned) {
      ASSERT(i != 0);
      auto prev_as_branch = dynamic_cast<BranchElement*>(sequence->at(i - 1));
      ASSERT(prev_as_branch);
      // printf("got while intro branch %s\n", prev_as_branch->print(file).c_str());
      // this should be an always jump. We'll assume that the CFG builder successfully checked
      // the brach destination, but we will check the condition.
      ASSERT(prev_as_branch->op()->condition().kind() == IR2_Condition::Kind::ALWAYS);
      ASSERT(prev_as_branch->op()->branch_delay().kind() == IR2_BranchDelay::Kind::NOP);
      to_remove.push_back(i - 1);

      // now we should try to find the condition branch:

      auto condition_branch = get_condition_branch(form_as_while->condition);

      ASSERT(condition_branch.first);
      ASSERT(condition_branch.first->op()->branch_delay().kind() == IR2_BranchDelay::Kind::NOP);
      // printf("got while condition branch %s\n", condition_branch.first->print(file).c_str());
      auto replacement = condition_branch.first->op()->get_condition_as_form(pool, env);

      *(condition_branch.second) = replacement;
    }
  }

  // remove the implied forward always branches.
  for (int i = int(to_remove.size()); i-- > 0;) {
    auto idx = to_remove.at(i);
    ASSERT(dynamic_cast<BranchElement*>(sequence->at(idx)));
    sequence->elts().erase(sequence->elts().begin() + idx);
  }
}
}  // namespace

void build_initial_forms(Function& function) {
  auto& cfg = function.cfg;
  if (!cfg->is_fully_resolved()) {
    return;
  }

  try {
    auto& pool = function.ir2.form_pool;
    auto top_level = function.cfg->get_single_top_level();
    std::vector<FormElement*> top_level_elts;
    insert_cfg_into_list(*pool, function, top_level, &top_level_elts);
    auto result = pool->alloc_sequence_form(nullptr, top_level_elts);

    result->apply_form([&](Form* form) { clean_up_while_loops(*pool, form, function.ir2.env); });

    result->apply([&](FormElement* form) {
      auto as_cne = dynamic_cast<CondNoElseElement*>(form);
      if (as_cne) {
        clean_up_cond_no_else_final(function, as_cne);
      }

      auto as_return = dynamic_cast<ReturnElement*>(form);
      if (as_return) {
        clean_up_return_final(function, as_return);
      }

      auto as_break = dynamic_cast<BreakElement*>(form);
      if (as_break) {
        clean_up_break_final(function, as_break, function.ir2.env);
      }
    });

    function.ir2.top_form = result;
  } catch (std::runtime_error& e) {
    function.warnings.error(e.what());
    lg::warn("Failed to build initial forms in {}: {}", function.name(), e.what());
  }
}
}  // namespace decompiler
