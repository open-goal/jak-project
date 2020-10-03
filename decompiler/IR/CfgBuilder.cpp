#include <unordered_set>
#include "CfgBuilder.h"
#include "decompiler/Function/CfgVtx.h"
#include "decompiler/Function/Function.h"

std::vector<std::shared_ptr<IR>> IR::get_all_ir(LinkedObjectFile& file) const {
  std::vector<std::shared_ptr<IR>> result;
  get_children(&result);
  size_t last_checked = 0;
  size_t last_last_checked = -1;

  while (last_checked != last_last_checked) {
    last_last_checked = last_checked;
    auto end_of_check = result.size();
    for (size_t i = last_checked; i < end_of_check; i++) {
      auto it = result.at(i).get();
      assert(it);
      it->get_children(&result);
    }
    last_checked = end_of_check;
  }

  // Todo, remove this check which is just for debugging.
  std::unordered_set<std::shared_ptr<IR>> unique_ir;
  for (auto& x : result) {
    unique_ir.insert(x);
  }
  assert(unique_ir.size() == result.size());
  return result;
}

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
    output->push_back(cfg_to_ir(f, file, vtx));
  }
}

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

std::pair<IR_Branch*, std::shared_ptr<IR>*> get_condition_branch(std::shared_ptr<IR>* in) {
  IR_Branch* condition_branch = dynamic_cast<IR_Branch*>(in->get());
  std::shared_ptr<IR>* condition_branch_location = in;
  if (!condition_branch) {
    // not 100% sure this will always work
    auto as_seq = dynamic_cast<IR_Begin*>(in->get());
    if (as_seq) {
      condition_branch = dynamic_cast<IR_Branch*>(as_seq->forms.back().get());
      condition_branch_location = &as_seq->forms.back();
    }
  }
  return std::make_pair(condition_branch, condition_branch_location);
}

void clean_up_cond_with_else(IR_CondWithElse* cwe, LinkedObjectFile& file) {
  for (auto& e : cwe->entries) {
    auto jump_to_next = get_condition_branch(&e.condition);
    assert(jump_to_next.first);
    assert(jump_to_next.first->branch_delay.kind == BranchDelay::NOP);
    printf("got cond condition %s\n", jump_to_next.first->print(file).c_str());
    auto replacement = std::make_shared<IR_Compare>(jump_to_next.first->condition);
    *(jump_to_next.second) = replacement;

    auto jump_to_end = get_condition_branch(&e.body);
    assert(jump_to_end.first);
    assert(jump_to_end.first->branch_delay.kind == BranchDelay::NOP);
    assert(jump_to_end.first->condition.kind == Condition::ALWAYS);
    auto as_end_of_sequence = get_condition_branch_as_vector(e.body.get());
    if (as_end_of_sequence.first) {
      assert(as_end_of_sequence.second->size() > 1);
      as_end_of_sequence.second->pop_back();
    } else {
      // this means the case is empty, which is a little bit weird but does actually appear to
      // happen in a few places. so we just replace the jump with a nop.  In the future we could
      // consider having a more explicit "this case is empty" operator so this doesn't get confused
      // with an actual MIPS nop.
      *(jump_to_end.second) = std::make_shared<IR_Nop>();
    }
  }
}

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
  } else if (dynamic_cast<CondWithElse*>(vtx)) {
    auto* cvtx = dynamic_cast<CondWithElse*>(vtx);
    std::vector<IR_CondWithElse::Entry> entries;
    for (auto& x : cvtx->entries) {
      IR_CondWithElse::Entry e;
      e.condition = cfg_to_ir(f, file, x.condition);
      e.body = cfg_to_ir(f, file, x.body);
      entries.push_back(std::move(e));
    }
    auto else_ir = cfg_to_ir(f, file, cvtx->else_vtx);
    auto result = std::make_shared<IR_CondWithElse>(entries, else_ir);
    clean_up_cond_with_else(result.get(), file);
    return result;
  }

  else {
    throw std::runtime_error("not yet implemented IR conversion.");
    return nullptr;
  }
}

void clean_up_while_loops(IR_Begin* sequence, LinkedObjectFile& file) {
  std::vector<size_t> to_remove;  // the list of branches to remove by index in this sequence
  for (size_t i = 0; i < sequence->forms.size(); i++) {
    auto* form_as_while = dynamic_cast<IR_WhileLoop*>(sequence->forms.at(i).get());
    if (form_as_while) {
      assert(i != 0);
      auto prev_as_branch = dynamic_cast<IR_Branch*>(sequence->forms.at(i - 1).get());
      assert(prev_as_branch);
      printf("got while intro branch %s\n", prev_as_branch->print(file).c_str());
      // this should be an always jump. We'll assume that the CFG builder successfully checked
      // the brach destination, but we will check the condition.
      assert(prev_as_branch->condition.kind == Condition::ALWAYS);
      assert(prev_as_branch->branch_delay.kind == BranchDelay::NOP);
      to_remove.push_back(i - 1);

      // now we should try to find the condition branch:

      auto condition_branch = get_condition_branch(&form_as_while->condition);

      assert(condition_branch.first);
      assert(condition_branch.first->branch_delay.kind == BranchDelay::NOP);
      printf("got while condition branch %s\n", condition_branch.first->print(file).c_str());
      auto replacement = std::make_shared<IR_Compare>(condition_branch.first->condition);
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

std::shared_ptr<IR> build_cfg_ir(Function& function,
                                 ControlFlowGraph& cfg,
                                 LinkedObjectFile& file) {
  //  printf("build cfg ir\n");
  if (!cfg.is_fully_resolved()) {
    return nullptr;
  }

  try {
    auto top_level = cfg.get_single_top_level();
    // todo, we should apply transformations for fixing up branch instructions for each IR.
    // and possibly annotate the IR control flow structure so that we can determine if its and/or
    // or whatever. This may require rejecting a huge number of inline assembly functions, and
    // possibly resolving the min/max/ash issue.
    auto ir = cfg_to_ir(function, file, top_level);
    auto all_children = ir->get_all_ir(file);
    all_children.push_back(ir);
    for (auto& child : all_children) {
      //      printf("child is %s\n", child->print(file).c_str());
      auto as_begin = dynamic_cast<IR_Begin*>(child.get());
      if (as_begin) {
        clean_up_while_loops(as_begin, file);
      }
    }
    return ir;
  } catch (std::runtime_error& e) {
    return nullptr;
  }
}