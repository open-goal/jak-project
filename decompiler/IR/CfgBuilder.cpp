#include "third-party/fmt/core.h"
#include <unordered_set>
#include "decompiler/util/MatchParam.h"
#include "CfgBuilder.h"
#include "decompiler/Function/CfgVtx.h"
#include "decompiler/Function/Function.h"
#include "decompiler/Disasm/InstructionMatching.h"
#include "decompiler/IR/IR.h"

namespace decompiler {
namespace {

std::shared_ptr<IR> cfg_to_ir(Function& f, LinkedObjectFile& file, CfgVtx* vtx);

/*!
 * This adds a single CfgVtx* to a list of IR's by converting it with cfg to IR.
 * The trick here is that it will recursively inline anything which would generate an IR begin.
 * This avoids the case where Begin's are nested excessively.
 */
void insert_cfg_into_list(Function& f,
                          LinkedObjectFile& file,
                          std::vector<std::shared_ptr<IR>>* output,
                          CfgVtx* vtx) {
  auto as_sequence = dynamic_cast<SequenceVtx*>(vtx);
  auto as_block = dynamic_cast<BlockVtx*>(vtx);
  if (as_sequence) {
    for (auto& x : as_sequence->seq) {
      insert_cfg_into_list(f, file, output, x);
    }
  } else if (as_block) {
    auto& block = f.basic_blocks.at(as_block->block_id);
    IR* last = nullptr;
    for (int instr = block.start_word; instr < block.end_word; instr++) {
      auto got = f.get_basic_op_at_instr(instr);
      if (got.get() == last) {
        continue;
      }
      last = got.get();
      output->push_back(got);
    }
  } else {
    // doesn't look like we're going to get something that can be inlined, so try as usual
    auto ir = cfg_to_ir(f, file, vtx);
    auto ir_as_begin = dynamic_cast<IR_Begin*>(ir.get());
    if (ir_as_begin) {
      // we unexpectedly got a begin, even though we didn't think we would.  This is okay, but we
      // should inline this begin to avoid nested begins.  This happens in the case where an entire
      // control flow pattern is turned into a single op (like type-of) and includes some ops at
      // the beginning. We don't have a good way of knowing this will happen until we try it.
      for (auto& x : ir_as_begin->forms) {
        output->push_back(x);
      }
    } else {
      output->push_back(ir);
    }
  }
}

/*!
 * If it's a begin with a branch as the last operation, returns a pointer to the branch IR
 * and also a pointer to the vector which holds the branch operation in its last slot.
 * Otherwise returns nullptr.  Useful to modify or remove branches found at the end of blocks,
 * and inline things into the begin they were found in.
 */
std::pair<IR_Branch*, std::vector<std::shared_ptr<IR>>*> get_condition_branch_as_vector(IR* in) {
  auto as_seq = dynamic_cast<IR_Begin*>(in);
  if (as_seq) {
    auto irb = dynamic_cast<IR_Branch*>(as_seq->forms.back().get());
    auto loc = &as_seq->forms;
    assert(irb);
    return std::make_pair(irb, loc);
  }
  return std::make_pair(nullptr, nullptr);
}

/*!
 * Given an IR, find a branch IR at the end, and also the location of it so it can be patched.
 * Returns nullptr as the first item in the pair if it didn't work.
 */
std::pair<IR_Branch_Atomic*, std::shared_ptr<IR>*> get_condition_branch(std::shared_ptr<IR>* in) {
  IR_Branch_Atomic* condition_branch = dynamic_cast<IR_Branch_Atomic*>(in->get());
  std::shared_ptr<IR>* condition_branch_location = in;
  if (!condition_branch) {
    // not 100% sure this will always work
    auto as_seq = dynamic_cast<IR_Begin*>(in->get());
    if (as_seq) {
      condition_branch = dynamic_cast<IR_Branch_Atomic*>(as_seq->forms.back().get());
      condition_branch_location = &as_seq->forms.back();
    }
  }

  if (!condition_branch) {
    auto as_return = dynamic_cast<IR_Return*>(in->get());
    if (as_return) {
      return get_condition_branch(&as_return->dead_code);
    }
  }

  if (!condition_branch) {
    auto as_break = dynamic_cast<IR_Break*>(in->get());
    if (as_break) {
      return get_condition_branch(&as_break->dead_code);
    }
  }
  return std::make_pair(condition_branch, condition_branch_location);
}

/*!
 * Given a CondWithElse IR, remove the internal branches and set the condition to be an actual
 * compare IR instead of a branch.
 * Doesn't "rebalance" the leading condition because this runs way before expression compaction.
 */
void clean_up_cond_with_else(std::shared_ptr<IR>* ir, LinkedObjectFile& file) {
  (void)file;
  auto cwe = dynamic_cast<IR_CondWithElse*>(ir->get());
  assert(cwe);
  for (auto& e : cwe->entries) {
    if (e.cleaned) {
      continue;
    }
    auto jump_to_next = get_condition_branch(&e.condition);
    assert(jump_to_next.first);
    assert(jump_to_next.first->branch_delay.kind == BranchDelay::NOP);
    // patch the jump to next with a condition.
    auto replacement =
        std::make_shared<IR_Compare>(jump_to_next.first->condition, jump_to_next.first);
    replacement->condition.invert();
    *(jump_to_next.second) = replacement;

    // patch the jump at the end of a block.
    auto jump_to_end = get_condition_branch(&e.body);
    assert(jump_to_end.first);
    assert(jump_to_end.first->branch_delay.kind == BranchDelay::NOP);
    assert(jump_to_end.first->condition.kind == Condition::ALWAYS);

    // if possible, we just want to remove this from the sequence its in.
    // but sometimes there's a case with nothing in it so there is no sequence.
    // in this case, we can just replace the branch with a NOP IR to indicate that nothing
    // happens in this case, but there was still GOAL code to test for it.
    // this happens rarely, as you would expect.
    auto as_end_of_sequence = get_condition_branch_as_vector(e.body.get());
    if (as_end_of_sequence.first) {
      assert(as_end_of_sequence.second->size() > 1);
      as_end_of_sequence.second->pop_back();
    } else {
      // In the future we could consider having a more explicit "this case is empty" operator so
      // this doesn't get confused with an actual MIPS nop.
      *(jump_to_end.second) = std::make_shared<IR_Nop>();
    }
    e.cleaned = true;
  }
}

void clean_up_until_loop(IR_UntilLoop* ir) {
  auto condition_branch = get_condition_branch(&ir->condition);
  assert(condition_branch.first);
  assert(condition_branch.first->branch_delay.kind == BranchDelay::NOP);
  auto replacement =
      std::make_shared<IR_Compare>(condition_branch.first->condition, condition_branch.first);
  replacement->condition.invert();
  *(condition_branch.second) = replacement;
}

void clean_up_infinite_while_loop(IR_WhileLoop* ir) {
  auto jump = get_condition_branch(&ir->body);
  assert(jump.first);
  assert(jump.first->branch_delay.kind == BranchDelay::NOP);
  assert(jump.first->condition.kind == Condition::ALWAYS);
  auto as_end_of_sequence = get_condition_branch_as_vector(ir->body.get());
  if (as_end_of_sequence.first) {
    assert(as_end_of_sequence.second->size() > 1);
    as_end_of_sequence.second->pop_back();
  } else {
    // In the future we could consider having a more explicit "this case is empty" operator so
    // this doesn't get confused with an actual MIPS nop.
    *(jump.second) = std::make_shared<IR_Nop>();
  }
  ir->cleaned = true;  // so we don't try this later...
}

void clean_up_return(IR_Return* ir) {
  auto jump_to_end = get_condition_branch(&ir->return_code);
  assert(jump_to_end.first);
  assert(jump_to_end.first->branch_delay.kind == BranchDelay::NOP);
  assert(jump_to_end.first->condition.kind == Condition::ALWAYS);
  auto as_end_of_sequence = get_condition_branch_as_vector(ir->return_code.get());
  if (as_end_of_sequence.first) {
    assert(as_end_of_sequence.second->size() > 1);
    as_end_of_sequence.second->pop_back();
  } else {
    // In the future we could consider having a more explicit "this case is empty" operator so
    // this doesn't get confused with an actual MIPS nop.
    *(jump_to_end.second) = std::make_shared<IR_Nop>();
  }
}

void clean_up_break(IR_Break* ir) {
  auto jump_to_end = get_condition_branch(&ir->return_code);
  assert(jump_to_end.first);
  assert(jump_to_end.first->branch_delay.kind == BranchDelay::NOP);
  assert(jump_to_end.first->condition.kind == Condition::ALWAYS);
  auto as_end_of_sequence = get_condition_branch_as_vector(ir->return_code.get());
  if (as_end_of_sequence.first) {
    assert(as_end_of_sequence.second->size() > 1);
    as_end_of_sequence.second->pop_back();
  } else {
    // In the future we could consider having a more explicit "this case is empty" operator so
    // this doesn't get confused with an actual MIPS nop.
    *(jump_to_end.second) = std::make_shared<IR_Nop>();
  }
}

/*!
 * Does the instruction in the delay slot set a register to false?
 * Note. a beql s7, x followed by a or y, x, r0 will count as this. I don't know why but
 * GOAL does this on comparisons to false.
 */
bool delay_slot_sets_false(IR_Branch* branch) {
  if (branch->branch_delay.kind == BranchDelay::SET_REG_FALSE) {
    return true;
  }

  if (branch->condition.kind == Condition::FALSE &&
      branch->branch_delay.kind == BranchDelay::SET_REG_REG) {
    auto reg_check = dynamic_cast<IR_Register*>(branch->condition.src0.get());
    assert(reg_check);
    auto reg_read = dynamic_cast<IR_Register*>(branch->branch_delay.source.get());
    assert(reg_read);
    return reg_check->reg == reg_read->reg;
  }

  return false;
}

/*!
 * Does the instruction in the delay slot set a register to a truthy value, like in a GOAL
 * or form branch?  Either it explicitly sets #t, or it tests the value for being not false,
 * then uses that
 */
bool delay_slot_sets_truthy(IR_Branch* branch) {
  if (branch->branch_delay.kind == BranchDelay::SET_REG_TRUE) {
    return true;
  }

  if (branch->condition.kind == Condition::TRUTHY &&
      branch->branch_delay.kind == BranchDelay::SET_REG_REG) {
    auto reg_check = dynamic_cast<IR_Register*>(branch->condition.src0.get());
    assert(reg_check);
    auto reg_read = dynamic_cast<IR_Register*>(branch->branch_delay.source.get());
    assert(reg_read);
    return reg_check->reg == reg_read->reg;
  }

  return false;
}

/*!
 * Try to convert a short circuit to an and.
 */
bool try_clean_up_sc_as_and(std::shared_ptr<IR_ShortCircuit>& ir, LinkedObjectFile& file) {
  (void)file;
  Register destination;
  std::shared_ptr<IR> ir_dest = nullptr;
  for (int i = 0; i < int(ir->entries.size()) - 1; i++) {
    auto branch = get_condition_branch(&ir->entries.at(i).condition);
    assert(branch.first);
    if (!delay_slot_sets_false(branch.first)) {
      return false;
    }

    if (i == 0) {
      ir_dest = branch.first->branch_delay.destination;
      destination = dynamic_cast<IR_Register*>(branch.first->branch_delay.destination.get())->reg;
    } else {
      if (destination !=
          dynamic_cast<IR_Register*>(branch.first->branch_delay.destination.get())->reg) {
        return false;
      }
    }
  }

  ir->kind = IR_ShortCircuit::AND;
  ir->final_result = ir_dest;
  auto* dest_reg = dynamic_cast<IR_Register*>(ir_dest.get());
  assert(dest_reg);

  bool live_out_result = false;

  // now get rid of the branches
  for (int i = 0; i < int(ir->entries.size()) - 1; i++) {
    auto branch = get_condition_branch(&ir->entries.at(i).condition);
    assert(branch.first);

    if (i == 0) {
      live_out_result = (branch.first->written_and_unused.find(dest_reg->reg) ==
                         branch.first->written_and_unused.end());
    } else {
      bool this_live_out = (branch.first->written_and_unused.find(dest_reg->reg) ==
                            branch.first->written_and_unused.end());
      assert(live_out_result == this_live_out);
    }

    auto replacement = std::make_shared<IR_Compare>(branch.first->condition, branch.first);
    replacement->condition.invert();
    *(branch.second) = replacement;
  }

  ir->used_as_value = live_out_result;
  return true;
}

/*!
 * Try to convert a short circuit to an or.
 * Note - this will convert an and to a very strange or, so always use the try as and first.
 */
bool try_clean_up_sc_as_or(std::shared_ptr<IR_ShortCircuit>& ir, LinkedObjectFile& file) {
  (void)file;
  Register destination;
  std::shared_ptr<IR> ir_dest = nullptr;
  for (int i = 0; i < int(ir->entries.size()) - 1; i++) {
    auto branch = get_condition_branch(&ir->entries.at(i).condition);
    assert(branch.first);
    if (!delay_slot_sets_truthy(branch.first)) {
      return false;
    }
    assert(dynamic_cast<IR_Register*>(branch.first->branch_delay.destination.get()));

    if (i == 0) {
      ir_dest = branch.first->branch_delay.destination;
      destination = dynamic_cast<IR_Register*>(branch.first->branch_delay.destination.get())->reg;
    } else {
      if (destination !=
          dynamic_cast<IR_Register*>(branch.first->branch_delay.destination.get())->reg) {
        return false;
      }
    }
  }

  ir->kind = IR_ShortCircuit::OR;
  ir->final_result = ir_dest;
  auto* dest_reg = dynamic_cast<IR_Register*>(ir_dest.get());
  assert(dest_reg);

  bool live_out_result = false;

  for (int i = 0; i < int(ir->entries.size()) - 1; i++) {
    auto branch = get_condition_branch(&ir->entries.at(i).condition);
    assert(branch.first);
    if (i == 0) {
      live_out_result = (branch.first->written_and_unused.find(dest_reg->reg) ==
                         branch.first->written_and_unused.end());
    } else {
      bool this_live_out = (branch.first->written_and_unused.find(dest_reg->reg) ==
                            branch.first->written_and_unused.end());
      assert(live_out_result == this_live_out);
    }
    auto replacement = std::make_shared<IR_Compare>(branch.first->condition, branch.first);
    *(branch.second) = replacement;
  }

  ir->used_as_value = live_out_result;
  return true;
}

void clean_up_sc(std::shared_ptr<IR_ShortCircuit>& ir, LinkedObjectFile& file);

/*!
 * A form like (and x (or y z)) will be recognized as a single SC Vertex by the CFG pass.
 * In the case where we fail to clean it up as an AND or an OR, we should attempt splitting.
 * Part of the complexity here is that we want to clean up the split recursively so things like
 * (and x (or y (and a b)))
 * or
 * (and x (or y (and a b)) c d (or z))
 * will work correctly.  This may require doing more splitting on both sections!
 */
bool try_splitting_nested_sc(std::shared_ptr<IR_ShortCircuit>& ir, LinkedObjectFile& file) {
  auto first_branch = get_condition_branch(&ir->entries.front().condition);
  assert(first_branch.first);
  bool first_is_and = delay_slot_sets_false(first_branch.first);
  bool first_is_or = delay_slot_sets_truthy(first_branch.first);
  assert(first_is_and != first_is_or);  // one or the other but not both!

  int first_different = -1;  // the index of the first one that's different.

  for (int i = 1; i < int(ir->entries.size()) - 1; i++) {
    auto branch = get_condition_branch(&ir->entries.at(i).condition);
    assert(branch.first);
    bool is_and = delay_slot_sets_false(branch.first);
    bool is_or = delay_slot_sets_truthy(branch.first);
    assert(is_and != is_or);

    if (first_different == -1) {
      // haven't seen a change yet.
      if (first_is_and != is_and) {
        // change!
        first_different = i;
        break;
      }
    }
  }

  assert(first_different != -1);

  std::vector<IR_ShortCircuit::Entry> nested_ir;
  for (int i = first_different; i < int(ir->entries.size()); i++) {
    nested_ir.push_back(ir->entries.at(i));
  }

  auto s = int(ir->entries.size());
  for (int i = first_different; i < s; i++) {
    ir->entries.pop_back();
  }

  auto nested_sc = std::make_shared<IR_ShortCircuit>(nested_ir);
  clean_up_sc(nested_sc, file);

  // the real trick
  IR_ShortCircuit::Entry nested_entry;
  nested_entry.condition = nested_sc;
  ir->entries.push_back(nested_entry);

  clean_up_sc(ir, file);

  return true;
}

/*!
 * Try to clean up a single short circuit IR. It may get split up into nested IR_ShortCircuits
 * if there is a case like (and a (or b c))
 */
void clean_up_sc(std::shared_ptr<IR_ShortCircuit>& ir, LinkedObjectFile& file) {
  (void)file;
  assert(ir->entries.size() > 1);
  if (!try_clean_up_sc_as_and(ir, file)) {
    if (!try_clean_up_sc_as_or(ir, file)) {
      if (!try_splitting_nested_sc(ir, file)) {
        assert(false);
      }
    }
  }
}

/*!
 * A GOAL comparison which produces a boolean is recognized as a cond-no-else by the CFG analysis.
 * But it should not be decompiled as a branching statement.
 * This either succeeds or asserts and must be called with with something that can be converted
 * successfully
 */
void convert_cond_no_else_to_compare(std::shared_ptr<IR>* ir) {
  auto cne = dynamic_cast<IR_Cond*>(ir->get());
  assert(cne);
  auto condition = get_condition_branch(&cne->entries.front().condition);
  assert(condition.first);
  auto body = dynamic_cast<IR_Set*>(cne->entries.front().body.get());
  assert(body);
  auto dst = body->dst;
  auto src = dynamic_cast<IR_Symbol*>(body->src.get());
  assert(src->name == "#f");
  assert(cne->entries.size() == 1);

  auto condition_as_single = dynamic_cast<IR_Branch*>(cne->entries.front().condition.get());
  if (condition_as_single) {
    auto replacement = std::make_shared<IR_Set>(
        IR_Set::REG_64, dst,
        std::make_shared<IR_Compare>(condition.first->condition, condition.first));
    *ir = replacement;
  } else {
    auto condition_as_seq = dynamic_cast<IR_Begin*>(cne->entries.front().condition.get());
    assert(condition_as_seq);
    if (condition_as_seq) {
      auto replacement = std::make_shared<IR_Begin>();
      replacement->forms = condition_as_seq->forms;
      assert(condition.second == &condition_as_seq->forms.back());
      replacement->forms.pop_back();
      replacement->forms.push_back(std::make_shared<IR_Set>(
          IR_Set::REG_64, dst,
          std::make_shared<IR_Compare>(condition.first->condition, condition.first)));
      *ir = replacement;
    }
  }
}

void clean_up_cond_no_else_final(IR_Cond* cne, LinkedObjectFile& file) {
  (void)cne;
  (void)file;
  for (size_t idx = 0; idx < cne->entries.size(); idx++) {
    auto& entry = cne->entries.at(idx);
    if (entry.false_destination != nullptr) {
      auto* fr = dynamic_cast<IR_Register*>(entry.false_destination.get());
      assert(fr);
      cne->final_destination = fr->reg;
    } else {
      assert(false);
    }
  }

  auto last_branch =
      dynamic_cast<IR_Branch_Atomic*>(cne->entries.back().original_condition_branch.get());
  assert(last_branch);
  cne->used_as_value = last_branch->written_and_unused.find(cne->final_destination) ==
                       last_branch->written_and_unused.end();

  // check that all other delay slot writes are unused.
  for (size_t i = 0; i < cne->entries.size() - 1; i++) {
    auto branch =
        dynamic_cast<IR_Branch_Atomic*>(cne->entries.at(i).original_condition_branch.get());
    auto reg = dynamic_cast<IR_Register*>(cne->entries.at(i).false_destination.get());
    assert(reg);
    assert(branch);
    assert(branch->written_and_unused.find(reg->reg) != branch->written_and_unused.end());
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
void clean_up_cond_no_else(std::shared_ptr<IR>* ir, LinkedObjectFile& file) {
  (void)file;
  auto cne = dynamic_cast<IR_Cond*>(ir->get());
  assert(cne);
  for (size_t idx = 0; idx < cne->entries.size(); idx++) {
    auto& e = cne->entries.at(idx);
    if (e.cleaned) {
      continue;
    }

    auto jump_to_next = get_condition_branch(&e.condition);
    assert(jump_to_next.first);

    if (jump_to_next.first->branch_delay.kind == BranchDelay::SET_REG_TRUE &&
        cne->entries.size() == 1) {
      convert_cond_no_else_to_compare(ir);
      return;
    } else {
      assert(jump_to_next.first->branch_delay.kind == BranchDelay::SET_REG_FALSE ||
             jump_to_next.first->branch_delay.kind == BranchDelay::NOP);
      assert(jump_to_next.first->condition.kind != Condition::ALWAYS);

      if (jump_to_next.first->branch_delay.kind == BranchDelay::SET_REG_FALSE) {
        assert(!e.false_destination);
        e.false_destination = jump_to_next.first->branch_delay.destination;
        assert(e.false_destination);
      }

      e.original_condition_branch = *jump_to_next.second;

      auto replacement =
          std::make_shared<IR_Compare>(jump_to_next.first->condition, jump_to_next.first);
      replacement->condition.invert();
      *(jump_to_next.second) = replacement;
      e.cleaned = true;

      if (idx != cne->entries.size() - 1) {
        auto jump_to_end = get_condition_branch(&e.body);
        assert(jump_to_end.first);
        assert(jump_to_end.first->branch_delay.kind == BranchDelay::NOP);
        assert(jump_to_end.first->condition.kind == Condition::ALWAYS);
        auto as_end_of_sequence = get_condition_branch_as_vector(e.body.get());
        if (as_end_of_sequence.first) {
          assert(as_end_of_sequence.second->size() > 1);
          as_end_of_sequence.second->pop_back();
        } else {
          *(jump_to_end.second) = std::make_shared<IR_Nop>();
        }
      }
    }
  }

  //    bool has_any_falses = false;
  //    Register false_reg;
  //    for (size_t idx = 0; idx < cne->entries.size(); idx++) {
  //      auto& entry = cne->entries.at(idx);
  //      if (idx == 0) {
  //        has_any_falses = entry.false_destination != nullptr;
  //        if (has_any_falses) {
  //          auto* as_reg = dynamic_cast<IR_Register*>(entry.false_destination.get());
  //          assert(as_reg);
  //          false_reg = as_reg->reg;
  //        }
  //      } else {
  //        if (has_any_falses) {
  //          if (idx == cne->entries.size() - 1) {
  //            assert(entry.false_destination == nullptr);
  //          } else {
  //            auto* as_reg = dynamic_cast<IR_Register*>(entry.false_destination.get());
  //            assert(as_reg);
  //            assert(as_reg->reg == false_reg);
  //          }
  //        } else {
  //          if (entry.false_destination != nullptr) {
  //            printf("BAD set of %s\n", entry.false_destination->print(file).c_str());
  //            printf("%s\n", entry.condition->print(file).c_str());
  //          }
  //          assert(entry.false_destination == nullptr);
  //        }
  //      }
  //    }
}

/*!
 * Match for a (set! reg (math reg reg)) form
 */
bool is_int_math_3(IR* ir,
                   MatchParam<IR_IntMath2::Kind> kind,
                   MatchParam<Register> dst,
                   MatchParam<Register> src0,
                   MatchParam<Register> src1,
                   Register* dst_out = nullptr,
                   Register* src0_out = nullptr,
                   Register* src1_out = nullptr) {
  // should be a set reg to int math 2 ir
  auto set = dynamic_cast<IR_Set*>(ir);
  if (!set) {
    return false;
  }

  // destination should be a register
  auto dest = dynamic_cast<IR_Register*>(set->dst.get());
  if (!dest || dst != dest->reg) {
    return false;
  }

  auto math = dynamic_cast<IR_IntMath2*>(set->src.get());
  if (!math || kind != math->kind) {
    return false;
  }

  auto arg0 = dynamic_cast<IR_Register*>(math->arg0.get());
  auto arg1 = dynamic_cast<IR_Register*>(math->arg1.get());

  if (!arg0 || src0 != arg0->reg || !arg1 || src1 != arg1->reg) {
    return false;
  }

  // it's a match!
  if (dst_out) {
    *dst_out = dest->reg;
  }

  if (src0_out) {
    *src0_out = arg0->reg;
  }

  if (src1_out) {
    *src1_out = arg1->reg;
  }
  return true;
}

bool is_int_math_2(IR* ir,
                   MatchParam<IR_IntMath1::Kind> kind,
                   MatchParam<Register> dst,
                   MatchParam<Register> src0,
                   Register* dst_out = nullptr,
                   Register* src0_out = nullptr) {
  // should be a set reg to int math 2 ir
  auto set = dynamic_cast<IR_Set*>(ir);
  if (!set) {
    return false;
  }

  // destination should be a register
  auto dest = dynamic_cast<IR_Register*>(set->dst.get());
  if (!dest || dst != dest->reg) {
    return false;
  }

  auto math = dynamic_cast<IR_IntMath1*>(set->src.get());
  if (!math || kind != math->kind) {
    return false;
  }

  auto arg = dynamic_cast<IR_Register*>(math->arg.get());

  if (!arg || src0 != arg->reg) {
    return false;
  }

  // it's a match!
  if (dst_out) {
    *dst_out = dest->reg;
  }

  if (src0_out) {
    *src0_out = arg->reg;
  }

  return true;
}

/*!
 * Are these IR's both the same register? False if either is not a register.
 */
bool is_same_reg(IR* a, IR* b) {
  auto ar = dynamic_cast<IR_Register*>(a);
  auto br = dynamic_cast<IR_Register*>(b);
  return ar && br && ar->reg == br->reg;
}

/*!
 * Try to convert this SC Vertex into an abs (integer).
 * Will return a converted abs IR if successful, or nullptr if its not possible
 */
std::shared_ptr<IR> try_sc_as_abs(Function& f, LinkedObjectFile& file, ShortCircuit* vtx) {
  if (vtx->entries.size() != 1) {
    return nullptr;
  }

  auto b0 = dynamic_cast<BlockVtx*>(vtx->entries.at(0));
  if (!b0) {
    return nullptr;
  }

  auto b0_ptr = cfg_to_ir(f, file, b0);
  auto b0_ir = dynamic_cast<IR_Begin*>(b0_ptr.get());

  IR_Branch* branch = nullptr;
  std::shared_ptr<IR> branch_sp = nullptr;
  if (b0_ir) {
    branch_sp = b0_ir->forms.back();
  } else {
    branch_sp = b0_ptr;
  }
  branch = dynamic_cast<IR_Branch*>(branch_sp.get());

  if (!branch) {
    return nullptr;
  }

  // check the branch instruction
  if (!branch->likely || branch->condition.kind != Condition::LESS_THAN_ZERO ||
      branch->branch_delay.kind != BranchDelay::NEGATE) {
    return nullptr;
  }

  auto input = branch->condition.src0;
  auto output = branch->branch_delay.destination;

  assert(is_same_reg(input.get(), branch->branch_delay.source.get()));

  if (b0_ir->forms.size() == 1) {
    // this is probably fine but happens to not occur in anything we try yet.
    assert(false);
  } else {
    // remove the branch
    b0_ir->forms.pop_back();
    // add the ash
    b0_ir->forms.push_back(std::make_shared<IR_Set>(
        IR_Set::REG_64, output,
        std::make_shared<IR_IntMath1>(IR_IntMath1::ABS, input,
                                      std::dynamic_pointer_cast<IR_Atomic>(branch_sp))));

    return b0_ptr;
  }

  return nullptr;
}

/*!
 * Attempt to convert a short circuit expression into an arithmetic shift.
 * GOAL's shift function accepts positive/negative numbers to determine the direction
 * of the shift.
 */
std::shared_ptr<IR> try_sc_as_ash(Function& f, LinkedObjectFile& file, ShortCircuit* vtx) {
  if (vtx->entries.size() != 2) {
    return nullptr;
  }

  // todo, I think b0 could possibly be something more complicated, depending on how we order.
  auto b0 = dynamic_cast<CfgVtx*>(vtx->entries.at(0));
  auto b1 = dynamic_cast<BlockVtx*>(vtx->entries.at(1));
  if (!b0 || !b1) {
    return nullptr;
  }

  // todo, seems possible to be a single op instead of a begin...
  auto b0_ptr = cfg_to_ir(f, file, b0);
  auto b0_ir = dynamic_cast<IR_Begin*>(b0_ptr.get());

  auto b1_ptr = cfg_to_ir(f, file, b1);
  auto b1_ir = dynamic_cast<IR_Begin*>(b1_ptr.get());

  if (!b0_ir || !b1_ir) {
    return nullptr;
  }

  auto branch_sp = b0_ir->forms.back();
  auto branch = dynamic_cast<IR_Branch*>(branch_sp.get());
  if (!branch || b1_ir->forms.size() != 2) {
    return nullptr;
  }

  // check the branch instruction
  if (!branch->likely || branch->condition.kind != Condition::GEQ_ZERO_SIGNED ||
      branch->branch_delay.kind != BranchDelay::DSLLV) {
    return nullptr;
  }

  /*
   *  bgezl s5, L109    ; s5 is the shift amount
      dsllv a0, a0, s5  ; a0 is both input and output here

      dsubu a1, r0, s5  ; a1 is a temp here
      dsrav a0, a0, a1  ; a0 is both input and output here
   */

  auto sa_in = dynamic_cast<IR_Register*>(branch->condition.src0.get());
  assert(sa_in);
  auto result = dynamic_cast<IR_Register*>(branch->branch_delay.destination.get());
  auto value_in = dynamic_cast<IR_Register*>(branch->branch_delay.source.get());
  auto sa_in2 = dynamic_cast<IR_Register*>(branch->branch_delay.source2.get());
  assert(result && value_in && sa_in2);
  assert(sa_in->reg == sa_in2->reg);

  auto dsubu_candidate = b1_ir->forms.at(0);
  auto dsrav_candidate = b1_ir->forms.at(1);

  Register clobber;
  //  if (!is_int_math_3(dsubu_candidate.get(), IR_IntMath2::SUB, {}, make_gpr(Reg::R0), sa_in->reg,
  //                     &clobber)) {
  //    return nullptr;
  //  }
  if (!is_int_math_2(dsubu_candidate.get(), IR_IntMath1::NEG, {}, sa_in->reg, &clobber)) {
    return nullptr;
  }

  assert(result);
  assert(value_in);

  bool is_arith = is_int_math_3(dsrav_candidate.get(), IR_IntMath2::RIGHT_SHIFT_ARITH, result->reg,
                                value_in->reg, clobber);
  bool is_logical = is_int_math_3(dsrav_candidate.get(), IR_IntMath2::RIGHT_SHIFT_LOGIC,
                                  result->reg, value_in->reg, clobber);

  if (!is_arith && !is_logical) {
    return nullptr;
  }

  std::shared_ptr<IR> clobber_ir = nullptr;
  auto dsubu_set = dynamic_cast<IR_Set*>(dsubu_candidate.get());
  auto dsrav_set = dynamic_cast<IR_Set*>(dsrav_candidate.get());
  if (clobber != result->reg) {
    clobber_ir = dsubu_set->dst;
  }

  std::shared_ptr<IR> dest_ir = branch->branch_delay.destination;
  std::shared_ptr<IR> shift_ir = branch->condition.src0;
  std::shared_ptr<IR> value_ir = dynamic_cast<IR_IntMath2*>(dsrav_set->src.get())->arg0;
  if (b0_ir->forms.size() == 1) {
    // this is probably fine but happens to not occur in anything we try yet.
    assert(false);
  } else {
    // remove the branch
    b0_ir->forms.pop_back();
    // add the ash
    b0_ir->forms.push_back(std::make_shared<IR_Set>(
        IR_Set::REG_64, dest_ir,
        std::make_shared<IR_Ash>(shift_ir, value_ir, clobber_ir,
                                 std::dynamic_pointer_cast<IR_Branch_Atomic>(branch_sp),
                                 std::dynamic_pointer_cast<IR_Atomic>(dsubu_candidate),
                                 std::dynamic_pointer_cast<IR_Atomic>(dsrav_candidate), is_arith)));
    return b0_ptr;
  }

  return nullptr;
}

/*!
 * Try to convert a short circuiting expression into a "type-of" expression.
 * We do this before attempting the normal and/or expressions.
 */
std::shared_ptr<IR> try_sc_as_type_of(Function& f, LinkedObjectFile& file, ShortCircuit* vtx) {
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

  auto b0 = dynamic_cast<CfgVtx*>(vtx->entries.at(0));
  auto b1 = dynamic_cast<BlockVtx*>(vtx->entries.at(1));
  auto b2 = dynamic_cast<BlockVtx*>(vtx->entries.at(2));

  if (!b0 || !b1 || !b2) {
    return nullptr;
  }

  auto b0_ptr = cfg_to_ir(f, file, b0);
  auto b0_ir = dynamic_cast<IR_Begin*>(b0_ptr.get());

  auto b1_ptr = cfg_to_ir(f, file, b1);
  auto b1_ir = dynamic_cast<IR_Branch*>(b1_ptr.get());

  auto b2_ptr = cfg_to_ir(f, file, b2);
  auto b2_ir = dynamic_cast<IR_Set*>(b2_ptr.get());
  if (!b0_ir || !b1_ir || !b2_ir) {
    return nullptr;
  }

  auto set_shift = dynamic_cast<IR_Set*>(b0_ir->forms.at(b0_ir->forms.size() - 2).get());
  if (!set_shift) {
    return nullptr;
  }

  auto temp_reg0 = dynamic_cast<IR_Register*>(set_shift->dst.get());
  if (!temp_reg0) {
    return nullptr;
  }

  auto shift = dynamic_cast<IR_IntMath2*>(set_shift->src.get());
  if (!shift || shift->kind != IR_IntMath2::LEFT_SHIFT) {
    return nullptr;
  }
  auto src_reg = dynamic_cast<IR_Register*>(shift->arg0.get());
  auto sa = dynamic_cast<IR_IntegerConstant*>(shift->arg1.get());
  if (!src_reg || !sa || sa->value != 61) {
    return nullptr;
  }

  auto first_branch = dynamic_cast<IR_Branch*>(b0_ir->forms.back().get());
  auto second_branch = b1_ir;
  auto else_case = b2_ir;

  if (!first_branch || first_branch->branch_delay.kind != BranchDelay::SET_BINTEGER ||
      first_branch->condition.kind != Condition::ZERO || !first_branch->likely) {
    return nullptr;
  }
  auto temp_reg = dynamic_cast<IR_Register*>(first_branch->condition.src0.get());
  assert(temp_reg);
  assert(temp_reg->reg == temp_reg0->reg);
  auto dst_reg = dynamic_cast<IR_Register*>(first_branch->branch_delay.destination.get());
  assert(dst_reg);

  if (!second_branch || second_branch->branch_delay.kind != BranchDelay::SET_PAIR ||
      second_branch->condition.kind != Condition::GREATER_THAN_ZERO_SIGNED ||
      !second_branch->likely) {
    return nullptr;
  }

  // check we agree on destination register.
  auto dst_reg2 = dynamic_cast<IR_Register*>(second_branch->branch_delay.destination.get());
  assert(dst_reg2->reg == dst_reg->reg);

  // else case is a lwu to grab the type from a basic
  assert(else_case);
  auto dst_reg3 = dynamic_cast<IR_Register*>(else_case->dst.get());
  assert(dst_reg3);
  assert(dst_reg3->reg == dst_reg->reg);
  auto load_op = dynamic_cast<IR_Load*>(else_case->src.get());
  if (!load_op || load_op->kind != IR_Load::UNSIGNED || load_op->size != 4) {
    return nullptr;
  }
  auto load_loc = dynamic_cast<IR_IntMath2*>(load_op->location.get());
  if (!load_loc || load_loc->kind != IR_IntMath2::ADD) {
    return nullptr;
  }
  auto src_reg3 = dynamic_cast<IR_Register*>(load_loc->arg0.get());
  auto offset = dynamic_cast<IR_IntegerConstant*>(load_loc->arg1.get());
  if (!src_reg3 || !offset) {
    return nullptr;
  }

  assert(src_reg3->reg == src_reg->reg);
  assert(offset->value == -4);

  std::shared_ptr<IR> clobber = nullptr;
  if (temp_reg->reg != dst_reg->reg) {
    clobber = first_branch->condition.src0;
  }
  if (b0_ir->forms.size() == 2) {
    return std::make_shared<IR_Set>(IR_Set::REG_64, else_case->dst,
                                    std::make_shared<IR_GetRuntimeType>(shift->arg0, clobber));
  } else {
    // remove the branch
    b0_ir->forms.pop_back();
    // remove the shift
    b0_ir->forms.pop_back();
    // add the type-of
    b0_ir->forms.push_back(std::make_shared<IR_Set>(
        IR_Set::REG_64, else_case->dst, std::make_shared<IR_GetRuntimeType>(shift->arg0, clobber)));
    return b0_ptr;
  }
}

std::shared_ptr<IR> merge_cond_else_with_sc_cond(CondWithElse* cwe,
                                                 const std::shared_ptr<IR>& else_ir,
                                                 Function& f,
                                                 LinkedObjectFile& file) {
  auto as_seq = dynamic_cast<IR_Begin*>(else_ir.get());
  if (!as_seq || as_seq->forms.size() != 2) {
    return nullptr;
  }

  auto first = dynamic_cast<IR_ShortCircuit*>(as_seq->forms.at(0).get());
  auto second = dynamic_cast<IR_Cond*>(as_seq->forms.at(1).get());
  if (!first || !second) {
    return nullptr;
  }

  std::vector<IR_Cond::Entry> entries;
  for (auto& x : cwe->entries) {
    IR_Cond::Entry e;
    e.condition = cfg_to_ir(f, file, x.condition);
    e.body = cfg_to_ir(f, file, x.body);
    entries.push_back(std::move(e));
  }

  auto first_condition = std::make_shared<IR_Begin>();
  first_condition->forms.push_back(as_seq->forms.at(0));
  first_condition->forms.push_back(second->entries.front().condition);

  second->entries.front().condition = first_condition;

  for (auto& x : second->entries) {
    entries.push_back(x);
  }
  std::shared_ptr<IR> result = std::make_shared<IR_Cond>(entries);
  clean_up_cond_no_else(&result, file);
  return result;
}

/*!
 * Main CFG vertex to IR conversion.  Will pull basic IR ops from the provided function as needed.
 */
std::shared_ptr<IR> cfg_to_ir(Function& f, LinkedObjectFile& file, CfgVtx* vtx) {
  if (dynamic_cast<BlockVtx*>(vtx)) {
    auto* bv = dynamic_cast<BlockVtx*>(vtx);
    auto& block = f.basic_blocks.at(bv->block_id);
    std::vector<std::shared_ptr<IR>> irs;
    IR* last = nullptr;
    for (int instr = block.start_word; instr < block.end_word; instr++) {
      auto got = f.get_basic_op_at_instr(instr);
      if (got.get() == last) {
        continue;
      }
      last = got.get();
      irs.push_back(got);
    }

    if (irs.size() == 1) {
      return irs.front();
    } else {
      return std::make_shared<IR_Begin>(irs);
    }

  } else if (dynamic_cast<SequenceVtx*>(vtx)) {
    auto* sv = dynamic_cast<SequenceVtx*>(vtx);

    std::vector<std::shared_ptr<IR>> irs;
    insert_cfg_into_list(f, file, &irs, sv);

    return std::make_shared<IR_Begin>(irs);
  } else if (dynamic_cast<WhileLoop*>(vtx)) {
    auto wvtx = dynamic_cast<WhileLoop*>(vtx);
    auto result = std::make_shared<IR_WhileLoop>(cfg_to_ir(f, file, wvtx->condition),
                                                 cfg_to_ir(f, file, wvtx->body));
    return result;
  } else if (dynamic_cast<UntilLoop*>(vtx)) {
    auto wvtx = dynamic_cast<UntilLoop*>(vtx);
    auto result = std::make_shared<IR_UntilLoop>(cfg_to_ir(f, file, wvtx->condition),
                                                 cfg_to_ir(f, file, wvtx->body));
    clean_up_until_loop(result.get());
    return result;
  } else if (dynamic_cast<UntilLoop_single*>(vtx)) {
    auto wvtx = dynamic_cast<UntilLoop_single*>(vtx);
    auto result =
        std::make_shared<IR_UntilLoop>(cfg_to_ir(f, file, wvtx->block), std::make_shared<IR_Nop>());
    clean_up_until_loop(result.get());
    return result;
  } else if (dynamic_cast<InfiniteLoopBlock*>(vtx)) {
    auto wvtx = dynamic_cast<InfiniteLoopBlock*>(vtx);
    auto result = std::make_shared<IR_WhileLoop>(
        std::make_shared<IR_Compare>(Condition(Condition::ALWAYS, nullptr, nullptr, nullptr),
                                     nullptr),
        cfg_to_ir(f, file, wvtx->block));
    clean_up_infinite_while_loop(result.get());
    return result;
  } else if (dynamic_cast<CondWithElse*>(vtx)) {
    auto* cvtx = dynamic_cast<CondWithElse*>(vtx);

    // the cfg analysis pass may recognize some things out of order, which can cause
    // fake nesting. This is actually a problem at this point because it can turn a normal
    // cond into a cond with else, which emits different instructions.  This attempts to recognize
    // an else which is actually more cases and compacts it into a single statement.  At this point
    // I don't know if this is sufficient to catch all cases.  it may even recognize the wrong
    // thing in some cases... maybe we should check the delay slot instead?
    auto else_ir = cfg_to_ir(f, file, cvtx->else_vtx);
    auto fancy_compact_result = merge_cond_else_with_sc_cond(cvtx, else_ir, f, file);
    if (fancy_compact_result) {
      return fancy_compact_result;
    }

    // this case is disabled because I _think_ it is now properly handled elsewhere.
    if (false && dynamic_cast<IR_Cond*>(else_ir.get())) {
      auto extra_cond = dynamic_cast<IR_Cond*>(else_ir.get());
      std::vector<IR_Cond::Entry> entries;
      for (auto& x : cvtx->entries) {
        IR_Cond::Entry e;
        e.condition = cfg_to_ir(f, file, x.condition);
        e.body = cfg_to_ir(f, file, x.body);
        entries.push_back(std::move(e));
      }
      for (auto& x : extra_cond->entries) {
        entries.push_back(x);
      }
      std::shared_ptr<IR> result = std::make_shared<IR_Cond>(entries);
      clean_up_cond_no_else(&result, file);
      return result;
    } else {
      std::vector<IR_CondWithElse::Entry> entries;
      for (auto& x : cvtx->entries) {
        IR_CondWithElse::Entry e;
        e.condition = cfg_to_ir(f, file, x.condition);
        e.body = cfg_to_ir(f, file, x.body);
        entries.push_back(std::move(e));
      }
      std::shared_ptr<IR> result = std::make_shared<IR_CondWithElse>(entries, else_ir);
      clean_up_cond_with_else(&result, file);
      return result;
    }
  } else if (dynamic_cast<ShortCircuit*>(vtx)) {
    auto* svtx = dynamic_cast<ShortCircuit*>(vtx);
    // try as a type of expression first
    auto as_type_of = try_sc_as_type_of(f, file, svtx);
    if (as_type_of) {
      return as_type_of;
    }

    auto as_ash = try_sc_as_ash(f, file, svtx);
    if (as_ash) {
      return as_ash;
    }

    auto as_abs = try_sc_as_abs(f, file, svtx);
    if (as_abs) {
      return as_abs;
    }

    if (svtx->entries.size() == 1) {
      throw std::runtime_error("Weird short circuit form.");
    }
    // now try as a normal and/or
    std::vector<IR_ShortCircuit::Entry> entries;
    for (auto& x : svtx->entries) {
      IR_ShortCircuit::Entry e;
      e.condition = cfg_to_ir(f, file, x);
      entries.push_back(e);
    }
    auto result = std::make_shared<IR_ShortCircuit>(entries);
    clean_up_sc(result, file);
    return result;
  } else if (dynamic_cast<CondNoElse*>(vtx)) {
    auto* cvtx = dynamic_cast<CondNoElse*>(vtx);
    std::vector<IR_Cond::Entry> entries;
    for (auto& x : cvtx->entries) {
      IR_Cond::Entry e;
      e.condition = cfg_to_ir(f, file, x.condition);
      e.body = cfg_to_ir(f, file, x.body);
      entries.push_back(std::move(e));
    }
    std::shared_ptr<IR> result = std::make_shared<IR_Cond>(entries);
    clean_up_cond_no_else(&result, file);
    return result;
  } else if (dynamic_cast<GotoEnd*>(vtx)) {
    auto* cvtx = dynamic_cast<GotoEnd*>(vtx);
    auto result = std::make_shared<IR_Return>(cfg_to_ir(f, file, cvtx->body),
                                              cfg_to_ir(f, file, cvtx->unreachable_block));
    clean_up_return(result.get());
    return result;
  } else if (dynamic_cast<Break*>(vtx)) {
    auto* cvtx = dynamic_cast<Break*>(vtx);
    auto result = std::make_shared<IR_Break>(cfg_to_ir(f, file, cvtx->body),
                                             cfg_to_ir(f, file, cvtx->unreachable_block));
    clean_up_break(result.get());
    return result;
  }

  throw std::runtime_error("not yet implemented IR conversion.");
  return nullptr;
}

/*!
 * Post processing pass to clean up while loops - annoyingly the block before a while loop
 * has a jump to the condition branch that we need to remove.  This currently happens after all
 * conversion but this may need to be revisited depending on the final order of simplifications.
 */
void clean_up_while_loops(IR_Begin* sequence, LinkedObjectFile& file) {
  (void)file;
  std::vector<size_t> to_remove;  // the list of branches to remove by index in this sequence
  for (size_t i = 0; i < sequence->forms.size(); i++) {
    auto* form_as_while = dynamic_cast<IR_WhileLoop*>(sequence->forms.at(i).get());
    if (form_as_while && !form_as_while->cleaned) {
      assert(i != 0);
      auto prev_as_branch = dynamic_cast<IR_Branch*>(sequence->forms.at(i - 1).get());
      assert(prev_as_branch);
      // printf("got while intro branch %s\n", prev_as_branch->print(file).c_str());
      // this should be an always jump. We'll assume that the CFG builder successfully checked
      // the brach destination, but we will check the condition.
      assert(prev_as_branch->condition.kind == Condition::ALWAYS);
      assert(prev_as_branch->branch_delay.kind == BranchDelay::NOP);
      to_remove.push_back(i - 1);

      // now we should try to find the condition branch:

      auto condition_branch = get_condition_branch(&form_as_while->condition);

      assert(condition_branch.first);
      assert(condition_branch.first->branch_delay.kind == BranchDelay::NOP);
      // printf("got while condition branch %s\n", condition_branch.first->print(file).c_str());
      auto replacement =
          std::make_shared<IR_Compare>(condition_branch.first->condition, condition_branch.first);
      *(condition_branch.second) = replacement;
    }
  }

  // remove the implied forward always branches.
  for (int i = int(to_remove.size()); i-- > 0;) {
    auto idx = to_remove.at(i);
    assert(dynamic_cast<IR_Branch*>(sequence->forms.at(idx).get()));
    sequence->forms.erase(sequence->forms.begin() + idx);
  }
}
}  // namespace

/*!
 * Use a control flow graph to build a single IR representing a function.
 * This should be done after basic ops are added and before typing, variable splitting, and
 * expression compaction.
 */
std::shared_ptr<IR> build_cfg_ir(Function& function,
                                 ControlFlowGraph& cfg,
                                 LinkedObjectFile& file) {
  //  printf("build cfg ir\n");
  if (!cfg.is_fully_resolved()) {
    return nullptr;
  }

  try {
    auto top_level = cfg.get_single_top_level();
    // and possibly annotate the IR control flow structure so that we can determine if its and/or
    // or whatever. This may require rejecting a huge number of inline assembly functions, and
    // possibly resolving the min/max/ash issue.
    // auto ir = cfg_to_ir(function, file, top_level);
    auto ir = std::make_shared<IR_Begin>();
    insert_cfg_into_list(function, file, &ir->forms, top_level);
    auto all_children = ir->get_all_ir(file);
    all_children.push_back(ir);
    for (auto& child : all_children) {
      auto as_begin = dynamic_cast<IR_Begin*>(child.get());
      if (as_begin) {
        clean_up_while_loops(as_begin, file);
      }

      auto as_cond_no_else = dynamic_cast<IR_Cond*>(child.get());
      if (as_cond_no_else) {
        clean_up_cond_no_else_final(as_cond_no_else, file);
      }
    }
    return ir;
  } catch (std::runtime_error& e) {
    return nullptr;
  }
}
}  // namespace decompiler