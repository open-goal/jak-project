#include "cfg_builder.h"

namespace decompiler {
namespace {

/*!
 * If it's a sequence with a branch as the last thing, return a pointer to the branch element
 * and also a pointer to the vector which holds the branch operation in its last slot.
 * Otherwise returns nullptr.  Useful to modify or remove branches found at the end of blocks,
 * and inline things into the begin they were found in.
 */
std::pair<BranchElement*, Form*> get_condition_branch_as_vector(Form* in) {
  // I am pretty sure that we'll never have to "dig" deeper to find the branch.
  // but in case I'm wrong, we explictly return the Form* we're found in. If I'm wrong,
  // this can be fixed here, rather than refactoring the whole thing.
  if (in->size() > 1) {
    auto irb = dynamic_cast<BranchElement*>(in->back());
    assert(irb);
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
void clean_up_cond_with_else(FormPool& pool, FormElement* ir, LinkedObjectFile& file) {
  (void)file;
  auto cwe = dynamic_cast<CondWithElseElement*>(ir);
  assert(cwe);
  for (auto& e : cwe->entries) {
    // don't reclean already cleaned things.
    if (e.cleaned) {
      continue;
    }
    auto jump_to_next = get_condition_branch(e.condition);
    assert(jump_to_next.first);
    assert(jump_to_next.first->op()->branch_delay().kind() == IR2_BranchDelay::Kind::NOP);
    // patch the branch to next with a condition.
    auto replacement = jump_to_next.first->op()->condition().get_as_form(pool);
    replacement->invert();
    *(jump_to_next.second) = replacement;

    // check the jump at the end of a block.
    auto jump_to_end = get_condition_branch(e.body);
    assert(jump_to_end.first);
    assert(jump_to_end.first->op()->branch_delay().kind() == IR2_BranchDelay::Kind::NOP);
    assert(jump_to_end.first->op()->condition().kind() == IR2_Condition::Kind::ALWAYS);

    // if possible, we just want to remove this from the sequence its in.
    // but sometimes there's a case with nothing in it so there is no sequence.
    // in this case, we can just replace the branch with a NOP IR to indicate that nothing
    // happens in this case, but there was still GOAL code to test for it.
    // this happens rarely, as you would expect.
    auto as_end_of_sequence = get_condition_branch_as_vector(e.body);
    if (as_end_of_sequence.first) {
      assert(as_end_of_sequence.second->size() > 1);
      as_end_of_sequence.second->pop_back();
    } else {
      // we need to have _something_ as the body, so we just put an (empty).
      *(jump_to_end.second) = pool.alloc_element<EmptyElement>();
    }
    e.cleaned = true;
  }
}

void insert_cfg_into_list(FormPool& pool,
                          const Function& f,
                          const CfgVtx* vtx,
                          std::vector<FormElement*>* output) {
  auto as_sequence = dynamic_cast<const SequenceVtx*>(vtx);
  auto as_block = dynamic_cast<const BlockVtx*>(vtx);
  if (as_sequence) {
    // inline the sequence.
    for (auto& x : as_sequence->seq) {
      insert_cfg_into_list(pool, f, x, output);
    }
  } else if (as_block) {
    // inline the ops.
    auto start_op = f.ir2.atomic_ops->block_id_to_first_atomic_op.at(as_block->block_id);
    auto end_op = f.ir2.atomic_ops->block_id_to_end_atomic_op.at(as_block->block_id);
    for (auto i = start_op; i < end_op; i++) {
      output->push_back(f.ir2.atomic_ops->ops.at(i)->get_as_form(pool));
    }
  } else {
    throw std::runtime_error(fmt::format("Unhandled cfg vtx: {}", vtx->to_string()));
  }
}
}  // namespace

void build_initial_forms(Function& function) {
  auto& cfg = function.cfg;
  auto& pool = function.ir2.form_pool;

  if (!cfg->is_fully_resolved()) {
    return;
  }

  try {
    auto top_level = function.cfg->get_single_top_level();
    std::vector<FormElement*> top_level_elts;
    insert_cfg_into_list(pool, function, top_level, &top_level_elts);
    // todo
    auto result = pool.alloc_sequence_form(nullptr, top_level_elts);

    // todo cleanup.

    function.ir2.top_form = result;
  } catch (std::runtime_error& e) {
    lg::warn("Failed to build initial forms in {}: {}", function.guessed_name.to_string(),
             e.what());
  }
}
}  // namespace decompiler