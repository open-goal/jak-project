#include <cassert>
#include "common/goos/PrettyPrinter.h"
#include "decompiler/Disasm/InstructionMatching.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "CfgVtx.h"
#include "Function.h"

namespace decompiler {
/////////////////////////////////////////
/// CfgVtx
/////////////////////////////////////////

/*!
 * Make this vertex a child vertex of new_parent!
 */
void CfgVtx::parent_claim(CfgVtx* new_parent) {
  parent = new_parent;

  // clear out all this junk - we don't need it now that we are a part of the "real" CFG!
  next = nullptr;
  prev = nullptr;
  pred.clear();
  succ_ft = nullptr;
  succ_branch = nullptr;
}

/*!
 * Replace reference to old_pred as a predecessor with new_pred.
 * Errors if old_pred wasn't referenced.
 */
void CfgVtx::replace_pred_and_check(CfgVtx* old_pred, CfgVtx* new_pred) {
  bool replaced = false;
  for (auto& x : pred) {
    if (x == old_pred) {
      assert(!replaced);
      x = new_pred;
      replaced = true;
    }
  }
  assert(replaced);
}

/*!
 * Replace references to old_succ with new_succ in the successors.
 * Errors if old_succ wasn't replaced.
 */
void CfgVtx::replace_succ_and_check(CfgVtx* old_succ, CfgVtx* new_succ) {
  bool replaced = false;
  if (succ_branch == old_succ) {
    succ_branch = new_succ;
    replaced = true;
  }

  if (succ_ft == old_succ) {
    succ_ft = new_succ;
    replaced = true;
  }

  assert(replaced);
}

/*!
 * Replace references to old_preds with a single new_pred.
 * Doesn't insert duplicates.
 * Error if all old preds aren't found.
 * If new_pred is nullptr, just removes the old preds without adding a new.
 */
void CfgVtx::replace_preds_with_and_check(std::vector<CfgVtx*> old_preds, CfgVtx* new_pred) {
  std::vector<bool> found(old_preds.size(), false);

  std::vector<CfgVtx*> new_pred_list;

  for (auto* existing_pred : pred) {
    bool match = false;
    size_t idx = -1;
    for (size_t i = 0; i < old_preds.size(); i++) {
      if (existing_pred == old_preds[i]) {
        assert(!match);
        idx = i;
        match = true;
      }
    }

    if (match) {
      found.at(idx) = true;
    } else {
      new_pred_list.push_back(existing_pred);
    }
  }

  if (new_pred) {
    new_pred_list.push_back(new_pred);
  }

  pred = new_pred_list;

  for (auto x : found) {
    assert(x);
  }
}

std::string CfgVtx::links_to_string() {
  std::string result;
  if (parent) {
    result += "  parent: " + parent->to_string() + "\n";
  }

  if (succ_branch) {
    result += "  succ_branch: " + succ_branch->to_string() + "\n";
  }

  if (succ_ft) {
    result += "  succ_ft: " + succ_ft->to_string() + "\n";
  }

  if (next) {
    result += "  next: " + next->to_string() + "\n";
  }

  if (prev) {
    result += "  prev: " + prev->to_string() + "\n";
  }

  if (!pred.empty()) {
    result += "  preds:\n";
    for (auto* x : pred) {
      result += "    " + x->to_string() + "\n";
    }
  }
  return result;
}

/////////////////////////////////////////
/// VERTICES
/////////////////////////////////////////

std::string BlockVtx::to_string() {
  if (is_early_exit_block) {
    return "Block (EA) " + std::to_string(block_id);
  } else {
    return "Block " + std::to_string(block_id);
  }
}

goos::Object BlockVtx::to_form() {
  return pretty_print::to_symbol("b" + std::to_string(block_id));
}

std::string SequenceVtx::to_string() {
  assert(!seq.empty());
  // todo - this is not a great way to print it. Maybe sequences should have an ID or name?
  std::string result =
      "Seq " + seq.front()->to_string() + " ... " + seq.back()->to_string() + std::to_string(uid);
  return result;
}

goos::Object SequenceVtx::to_form() {
  std::vector<goos::Object> forms;
  forms.push_back(pretty_print::to_symbol("seq"));
  for (auto* x : seq) {
    forms.push_back(x->to_form());
  }
  return pretty_print::build_list(forms);
}

std::string EntryVtx::to_string() {
  return "ENTRY";
}

goos::Object EntryVtx::to_form() {
  return pretty_print::to_symbol("entry");
}

std::string ExitVtx::to_string() {
  return "EXIT";
}

goos::Object ExitVtx::to_form() {
  return pretty_print::to_symbol("exit");
}

std::string CondWithElse::to_string() {
  return "CONDWE" + std::to_string(uid);
}

goos::Object CondWithElse::to_form() {
  std::vector<goos::Object> forms;
  forms.push_back(pretty_print::to_symbol("cond"));
  for (const auto& x : entries) {
    std::vector<goos::Object> e = {x.condition->to_form(), x.body->to_form()};
    forms.push_back(pretty_print::build_list(e));
  }
  std::vector<goos::Object> e = {pretty_print::to_symbol("else"), else_vtx->to_form()};
  forms.push_back(pretty_print::build_list(e));
  return pretty_print::build_list(forms);
}

std::string CondNoElse::to_string() {
  return "CONDNE" + std::to_string(uid);
}

goos::Object CondNoElse::to_form() {
  std::vector<goos::Object> forms;
  forms.push_back(pretty_print::to_symbol("cond"));
  for (const auto& x : entries) {
    std::vector<goos::Object> e = {x.condition->to_form(), x.body->to_form()};
    forms.push_back(pretty_print::build_list(e));
  }
  return pretty_print::build_list(forms);
}

std::string WhileLoop::to_string() {
  return "WHL" + std::to_string(uid);
}

goos::Object WhileLoop::to_form() {
  std::vector<goos::Object> forms = {pretty_print::to_symbol("while"), condition->to_form(),
                                     body->to_form()};
  return pretty_print::build_list(forms);
}

std::string UntilLoop::to_string() {
  return "UNTL" + std::to_string(uid);
}

goos::Object UntilLoop::to_form() {
  std::vector<goos::Object> forms = {pretty_print::to_symbol("until"), condition->to_form(),
                                     body->to_form()};
  return pretty_print::build_list(forms);
}

std::string UntilLoop_single::to_string() {
  return "UNTLS" + std::to_string(uid);
}

goos::Object UntilLoop_single::to_form() {
  std::vector<goos::Object> forms = {pretty_print::to_symbol("until1"), block->to_form()};
  return pretty_print::build_list(forms);
}

std::string InfiniteLoopBlock::to_string() {
  return "INFL" + std::to_string(uid);
}

goos::Object InfiniteLoopBlock::to_form() {
  std::vector<goos::Object> forms = {pretty_print::to_symbol("inf-loop"), block->to_form()};
  return pretty_print::build_list(forms);
}

std::string ShortCircuit::to_string() {
  return "SC" + std::to_string(uid);
}

goos::Object ShortCircuit::to_form() {
  std::vector<goos::Object> forms;
  forms.push_back(pretty_print::to_symbol("sc"));
  for (const auto& x : entries) {
    forms.push_back(x->to_form());
  }
  return pretty_print::build_list(forms);
}

std::string GotoEnd::to_string() {
  return "goto_end" + std::to_string(uid);
}

goos::Object GotoEnd::to_form() {
  std::vector<goos::Object> forms = {pretty_print::to_symbol("return-from-function"),
                                     body->to_form(), unreachable_block->to_form()};
  return pretty_print::build_list(forms);
}

std::string Break::to_string() {
  return "goto" + std::to_string(uid);
}

goos::Object Break::to_form() {
  std::vector<goos::Object> forms = {pretty_print::to_symbol("break"),
                                     pretty_print::to_symbol(std::to_string(dest_block)),
                                     body->to_form(), unreachable_block->to_form()};
  return pretty_print::build_list(forms);
}

ControlFlowGraph::ControlFlowGraph() {
  // allocate the entry and exit vertices.
  m_entry = alloc<EntryVtx>();
  m_exit = alloc<ExitVtx>();
}

ControlFlowGraph::~ControlFlowGraph() {
  for (auto* x : m_node_pool) {
    delete x;
  }
}

/*!
 * Convert unresolved portion of CFG into a format that can be read by dot, a graph layout tool.
 * This is intended to help with debugging why a cfg couldn't be resolved.
 */
std::string ControlFlowGraph::to_dot() {
  std::string result = "digraph G {\n";
  std::string invis;
  for (auto* node : m_node_pool) {
    if (!node->parent) {
      auto me = "\"" + node->to_string() + "\"";
      if (!invis.empty()) {
        invis += " -> ";
      }
      invis += me;
      result += me + ";\n";

      // it's a top level node
      for (auto* s : node->succs()) {
        result += me + " -> \"" + s->to_string() + "\";\n";
      }
    }
  }
  result += "\n" + invis + " [style=invis];\n}\n";
  result += "\n\n";
  for_each_top_level_vtx([&](CfgVtx* vtx) {
    result += "VTX: " + vtx->to_string() + "\n" + vtx->links_to_string() + "\n";
    return true;
  });
  return result;
}

/*!
 * Is this CFG fully resolved?  Did we succeed in decoding the control flow?
 */
bool ControlFlowGraph::is_fully_resolved() {
  return get_top_level_vertices_count() == 1;
}

/*!
 * How many top level vertices are there?  Doesn't count entry and exit.
 */
int ControlFlowGraph::get_top_level_vertices_count() {
  int count = 0;
  for (auto* x : m_node_pool) {
    if (!x->parent && x != entry() && x != exit()) {
      count++;
    }
  }
  return count;
}

/*!
 * Get the top level vertex. Only safe to call if we are fully resolved.
 */
CfgVtx* ControlFlowGraph::get_single_top_level() {
  assert(get_top_level_vertices_count() == 1);
  for (auto* x : m_node_pool) {
    if (!x->parent && x != entry() && x != exit()) {
      return x;
    }
  }
  assert(false);
  return nullptr;
}

/*!
 * Turn into a form. If fully resolved, prints the nested control flow. Otherwise puts all the
 * ungrouped stuff into an "(ungrouped ...)" form and prints that.
 */
goos::Object ControlFlowGraph::to_form() {
  if (get_top_level_vertices_count() == 1) {
    return get_single_top_level()->to_form();
  } else {
    std::vector<goos::Object> forms = {pretty_print::to_symbol("ungrouped")};
    for (auto* x : m_node_pool) {
      if (!x->parent && x != entry() && x != exit()) {
        forms.push_back(x->to_form());
      }
    }
    return pretty_print::build_list(forms);
  }
}

/*!
 * Turn into a string. If fully resolved, prints the nested control flow. Otherwise puts all the
 * ungrouped stuff into an "(ungrouped ...)" form and prints that.
 */
std::string ControlFlowGraph::to_form_string() {
  return pretty_print::to_string(to_form());
}

// bool ControlFlowGraph::compact_top_level() {
//  int compact_count = 0;
//
//  std::string orig = to_dot();
//
//  while (compact_one_in_top_level()) {
//    compact_count++;
//  }
//
//  if (compact_count) {
//    printf("%s\nCHANGED TO\n%s\n", orig.c_str(), to_dot().c_str());
//    return true;
//  }
//
//  return false;
//}
//
// bool ControlFlowGraph::compact_one_in_top_level() {
//  for (auto* node : m_node_pool) {
//    if (node->parent_container) {
//      continue;
//    }
//
//    if (node != entry() && node->succ.size() == 1 && !node->has_succ(exit()) &&
//        node->succ.front()->pred.size() == 1 && !node->succ.front()->has_succ(node)) {
//      // can compact!
//      auto first = node;
//      auto second = node->succ.front();
//      assert(second->has_pred(first));
//
//      make_sequence(first, second);
//      return true;
//    }
//  }
//
//  return false;
//}
//

// bool ControlFlowGraph::is_if_else(CfgVtx* b0, CfgVtx* b1, CfgVtx* b2, CfgVtx* b3) {
//  // check existance
//  if (!b0 || !b1 || !b2 || !b3)
//    return false;
//
//  // check verts
//  if (b0->next != b1)
//    return false;
//  if (b0->succ_ft != b1)
//    return false;
//  if (b0->succ_branch != b2)
//    return false;
//  if (b0->end_branch.branch_always)
//    return false;
//  if (b0->end_branch.branch_likely)
//    return false;
//  assert(b0->end_branch.has_branch);
//  // b0 prev, pred don't care
//
//  if (b1->prev != b0)
//    return false;
//  if (!b1->has_pred(b0))
//    return false;
//  if (b1->pred.size() != 1)
//    return false;
//  if (b1->next != b2)
//    return false;
//  if (b1->succ_ft)
//    return false;
//  if (b1->succ_branch != b3)
//    return false;
//  assert(b1->end_branch.branch_always);
//  assert(b1->end_branch.has_branch);
//  if (b1->end_branch.branch_likely)
//    return false;
//
//  if (b2->prev != b1)
//    return false;
//  if (!b2->has_pred(b0))
//    return false;
//  if (b2->pred.size() != 1)
//    return false;
//  if (b2->next != b3)
//    return false;
//  if (b2->succ_branch)
//    return false;
//  assert(!b2->end_branch.has_branch);
//  if (b2->succ_ft != b3)
//    return false;
//
//  if (b3->prev != b2)
//    return false;
//  if (!b3->has_pred(b2))
//    return false;
//  if (!b3->has_pred(b1))
//    return false;
//
//  return true;
//}
//
bool ControlFlowGraph::is_while_loop(CfgVtx* b0, CfgVtx* b1, CfgVtx* b2) {
  // todo - check delay slots!
  if (!b0 || !b1 || !b2)
    return false;

  // check next and prev
  if (b0->next != b1)
    return false;
  if (b1->next != b2)
    return false;
  if (b2->prev != b1)
    return false;
  if (b1->prev != b0)
    return false;

  //  // check branch to condition at the beginning
  if (b0->succ_ft)
    return false;
  if (b0->succ_branch != b2)
    return false;
  assert(b0->end_branch.has_branch);
  assert(b0->end_branch.branch_always);
  if (b0->end_branch.branch_likely)
    return false;

  // check b1 -> b2 fallthrough
  if (b1->succ_ft != b2)
    return false;
  if (b1->succ_branch)
    return false;
  assert(!b1->end_branch.has_branch);
  if (!b2->has_pred(b0)) {
    printf("expect b2 (%s) to have pred b0 (%s)\n", b2->to_string().c_str(),
           b0->to_string().c_str());
    printf("but it doesn't! instead it has:\n");
    for (auto* x : b2->pred) {
      printf(" %s\n", x->to_string().c_str());
    }
    if (b0->succ_ft) {
      printf("b0's succ_ft: %s\n", b0->succ_ft->to_string().c_str());
    }
    if (b0->succ_branch) {
      printf("b0's succ_branch: %s\n", b0->succ_branch->to_string().c_str());
    }
  }
  assert(b2->has_pred(b0));
  assert(b2->has_pred(b1));
  if (b2->pred.size() != 2)
    return false;

  // check b2's branch back
  if (b2->succ_branch != b1)
    return false;
  if (b2->end_branch.branch_likely)
    return false;
  if (b2->end_branch.branch_always)
    return false;

  return true;
}

bool ControlFlowGraph::is_until_loop(CfgVtx* b1, CfgVtx* b2) {
  // todo - check delay slots!
  if (!b1 || !b2)
    return false;

  // check next and prev
  if (b1->next != b2)
    return false;
  if (b2->prev != b1)
    return false;

  // check b1 -> b2 fallthrough
  if (b1->succ_ft != b2)
    return false;
  if (b1->succ_branch)
    return false;
  assert(!b1->end_branch.has_branch);

  assert(b2->has_pred(b1));
  if (b2->pred.size() != 1)
    return false;

  // check b2's branch back
  if (b2->succ_branch != b1)
    return false;
  if (b2->end_branch.branch_likely)
    return false;
  if (b2->end_branch.branch_always)
    return false;

  return true;
}

bool ControlFlowGraph::is_goto_not_end_and_unreachable(CfgVtx* b0, CfgVtx* b1) {
  if (!b0 || !b1) {
    return false;
  }

  // b0 should be an always branch, not likely.
  if (!b0->end_branch.has_branch || !b0->end_branch.branch_always || b0->end_branch.branch_likely) {
    return false;
  }

  // b0 should be next to b1
  if (b0->next != b1) {
    return false;
  }

  assert(b1->prev == b0);

  // b1 should have no preds and be unreachable.
  if (!b1->pred.empty()) {
    return false;
  }

  return true;  // match!
}

bool ControlFlowGraph::is_goto_end_and_unreachable(CfgVtx* b0, CfgVtx* b1) {
  if (!b0 || !b1) {
    return false;
  }

  // b0 should branch to the and end vertex always
  if (!b0->end_branch.has_branch)
    return false;
  if (!b0->end_branch.branch_always)
    return false;
  if (b0->end_branch.branch_likely)
    return false;

  // b0 should next to b1
  if (b0->next != b1)
    return false;
  assert(b1->prev == b0);

  // b1 should have no preds
  if (!b1->pred.empty())
    return false;

  auto dest = b0->succ_branch;
  auto dest_as_block = dynamic_cast<BlockVtx*>(dest);
  //  printf("DAB: %s\n", dest_as_block->to_string().c_str());
  if (!dest_as_block)
    return false;
  if (!dest_as_block->is_early_exit_block)
    return false;

  return true;  // match!
}

/*
bool ControlFlowGraph::find_if_else_top_level() {
  // todo check delay slots
  // Example:
  // B0:
  //  beq s7, v1, B2  ;; inverted branch condition (branch on condition not met)
  //  sll r0, r0, 0   ;; nop in delay slot
  // B1:
  //  true case!
  //  beq r0, r0, B3  ;; unconditional branch
  //  sll r0, r0, 0   ;; nop in delay slot
  // B2:
  //  false case!     ;; fall through
  // B3:
  //  rest of code
  bool found_one = false;
  bool needs_work = true;
  while (needs_work) {
    needs_work = false;  // until we change something, assume we're done.

    for_each_top_level_vtx([&](CfgVtx* vtx) {
      // return true means "try again with a different vertex"
      // return false means "I changed something, bail out so we can start from the beginning
      // again."

      // attempt to match b0, b1, b2, b3
      auto* b0 = vtx;
      auto* b1 = vtx->succ_ft;
      auto* b2 = vtx->succ_branch;
      auto* b3 = b2 ? b2->succ_ft : nullptr;

      if (is_if_else(b0, b1, b2, b3)) {
        needs_work = true;

        // create the new vertex!
        auto* new_vtx = alloc<IfElseVtx>();
        new_vtx->condition = b0;
        new_vtx->true_case = b1;
        new_vtx->false_case = b2;

        // link new vertex pred
        for (auto* new_pred : b0->pred) {
          new_pred->replace_succ_and_check(b0, new_vtx);
        }
        new_vtx->pred = b0->pred;

        // link new vertex succ
        b3->replace_preds_with_and_check({b1, b2}, new_vtx);
        new_vtx->succ_ft = b3;

        // setup next/prev
        new_vtx->prev = b0->prev;
        if (new_vtx->prev) {
          new_vtx->prev->next = new_vtx;
        }
        new_vtx->next = b3;
        b3->prev = new_vtx;

        b0->parent_claim(new_vtx);
        b1->parent_claim(new_vtx);
        b2->parent_claim(new_vtx);
        found_one = true;
        return false;
      } else {
        return true;  // try again!
      }
    });
  }
  return found_one;
}
 */

bool ControlFlowGraph::find_while_loop_top_level() {
  // B0 can start with whatever
  // B0 ends in unconditional branch to B2 (condition).
  // B2 has conditional non-likely branch to B1
  // B1 falls through to B2 and nowhere else
  // B2 can end with whatever
  bool found_one = false;
  bool needs_work = true;
  while (needs_work) {
    needs_work = false;
    for_each_top_level_vtx([&](CfgVtx* vtx) {
      auto* b0 = vtx;
      auto* b1 = vtx->next;
      auto* b2 = b1 ? b1->next : nullptr;

      if (is_while_loop(b0, b1, b2)) {
        needs_work = true;

        auto* new_vtx = alloc<WhileLoop>();
        new_vtx->body = b1;
        new_vtx->condition = b2;

        b0->replace_succ_and_check(b2, new_vtx);
        new_vtx->pred = {b0};

        assert(b2->succ_ft);
        b2->succ_ft->replace_pred_and_check(b2, new_vtx);
        new_vtx->succ_ft = b2->succ_ft;
        // succ_branch is going back into the loop

        new_vtx->prev = b0;
        b0->next = new_vtx;

        new_vtx->next = b2->next;
        if (new_vtx->next) {
          new_vtx->next->prev = new_vtx;
        }

        b1->parent_claim(new_vtx);
        b2->parent_claim(new_vtx);
        found_one = true;
        return false;
      } else {
        return true;
      }
    });
  }
  return found_one;
}

bool ControlFlowGraph::find_until_loop() {
  // B2 has conditional non-likely branch to B1
  // B1 falls through to B2 and nowhere else
  // B2 can end with whatever
  bool found_one = false;
  bool needs_work = true;
  while (needs_work) {
    needs_work = false;
    for_each_top_level_vtx([&](CfgVtx* vtx) {
      auto* b1 = vtx;
      auto* b2 = b1 ? b1->next : nullptr;

      if (is_until_loop(b1, b2)) {
        needs_work = true;

        auto* new_vtx = alloc<UntilLoop>();
        new_vtx->body = b1;
        new_vtx->condition = b2;

        for (auto* b0 : b1->pred) {
          b0->replace_succ_and_check(b1, new_vtx);
        }

        new_vtx->pred = b1->pred;
        new_vtx->replace_preds_with_and_check({b2}, nullptr);

        assert(b2->succ_ft);
        b2->succ_ft->replace_pred_and_check(b2, new_vtx);
        new_vtx->succ_ft = b2->succ_ft;
        // succ_branch is going back into the loop

        new_vtx->prev = b1->prev;
        if (new_vtx->prev) {
          new_vtx->prev->next = new_vtx;
        }

        new_vtx->next = b2->next;
        if (new_vtx->next) {
          new_vtx->next->prev = new_vtx;
        }

        b1->parent_claim(new_vtx);
        b2->parent_claim(new_vtx);
        found_one = true;
        return false;
      } else {
        return true;
      }
    });
  }
  return found_one;
}

bool ControlFlowGraph::find_infinite_loop() {
  bool found = false;

  for_each_top_level_vtx([&](CfgVtx* vtx) {
    if (vtx->succ_branch == vtx && !vtx->succ_ft) {
      auto inf = alloc<InfiniteLoopBlock>();
      inf->block = vtx;
      inf->pred = vtx->pred;
      inf->replace_preds_with_and_check({vtx}, nullptr);
      for (auto* x : inf->pred) {
        x->replace_succ_and_check(vtx, inf);
      }
      inf->prev = vtx->prev;
      if (inf->prev) {
        inf->prev->next = inf;
      }

      inf->next = vtx->next;
      if (inf->next) {
        inf->succ_ft = inf->next;
        inf->next->prev = inf;
        inf->succ_ft->pred.push_back(inf);
      }

      inf->succ_branch = nullptr;
      vtx->parent_claim(inf);

      found = true;
      return false;
    }

    return true;
  });

  return found;
}

bool ControlFlowGraph::find_until1_loop() {
  bool found = false;

  for_each_top_level_vtx([&](CfgVtx* vtx) {
    if (vtx->succ_branch == vtx && vtx->succ_ft) {
      auto loop = alloc<UntilLoop_single>();
      loop->block = vtx;
      loop->pred = vtx->pred;
      loop->replace_preds_with_and_check({vtx}, nullptr);
      for (auto* x : loop->pred) {
        x->replace_succ_and_check(vtx, loop);
      }
      loop->prev = vtx->prev;
      if (loop->prev) {
        loop->prev->next = loop;
      }

      loop->next = vtx->next;
      if (loop->next) {
        loop->next->prev = loop;
      }

      loop->succ_ft = vtx->succ_ft;
      loop->succ_ft->replace_pred_and_check(vtx, loop);

      vtx->parent_claim(loop);

      found = true;
      return false;
    }

    return true;
  });

  return found;
}

bool ControlFlowGraph::find_goto_end() {
  bool replaced = false;

  for_each_top_level_vtx([&](CfgVtx* vtx) {
    auto* b0 = vtx;
    auto* b1 = vtx->next;
    if (is_goto_end_and_unreachable(b0, b1)) {
      replaced = true;

      auto* new_goto = alloc<GotoEnd>();
      new_goto->body = b0;
      new_goto->unreachable_block = b1;

      for (auto* new_pred : b0->pred) {
        //        printf("fix up pred %s of %s\n", new_pred->to_string().c_str(),
        //        b0->to_string().c_str());
        new_pred->replace_succ_and_check(b0, new_goto);
      }
      new_goto->pred = b0->pred;

      for (auto* new_succ : b1->succs()) {
        //        new_succ->replace_preds_with_and_check({b1}, nullptr);
        new_succ->replace_pred_and_check(b1, new_goto);
      }
      // this is a lie, but ok

      new_goto->succ_ft = b1->succ_ft;
      new_goto->succ_branch = b1->succ_branch;
      new_goto->end_branch = b1->end_branch;

      //      if(b1->next) {
      //        b1->next->pred.push_back(new_goto);
      //      }
      //      new_goto->succ_branch = b1->succ_branch;
      //      new_goto->end_branch = b1->end_branch;

      new_goto->prev = b0->prev;
      if (new_goto->prev) {
        new_goto->prev->next = new_goto;
      }

      new_goto->next = b1->next;
      if (new_goto->next) {
        new_goto->next->prev = new_goto;
      }

      b0->succ_branch->replace_preds_with_and_check({b0}, nullptr);

      b0->parent_claim(new_goto);
      b1->parent_claim(new_goto);

      return false;
    }

    // keep looking
    return true;
  });

  return replaced;
}

bool ControlFlowGraph::find_goto_not_end() {
  bool replaced = false;

  for_each_top_level_vtx([&](CfgVtx* vtx) {
    auto* b0 = vtx;
    auto* b1 = vtx->next;
    if (is_goto_not_end_and_unreachable(b0, b1)) {
      replaced = true;

      auto* new_goto = alloc<Break>();
      new_goto->body = b0;
      new_goto->unreachable_block = b1;
      // todo set block number

      for (auto* new_pred : b0->pred) {
        //        printf("fix up pred %s of %s\n", new_pred->to_string().c_str(),
        //        b0->to_string().c_str());
        new_pred->replace_succ_and_check(b0, new_goto);
      }
      new_goto->pred = b0->pred;

      for (auto* new_succ : b1->succs()) {
        //        new_succ->replace_preds_with_and_check({b1}, nullptr);
        new_succ->replace_pred_and_check(b1, new_goto);
      }
      // this is a lie, but ok

      new_goto->succ_ft = b1->succ_ft;
      new_goto->succ_branch = b1->succ_branch;
      new_goto->end_branch = b1->end_branch;

      //      if(b1->next) {
      //        b1->next->pred.push_back(new_goto);
      //      }
      //      new_goto->succ_branch = b1->succ_branch;
      //      new_goto->end_branch = b1->end_branch;

      new_goto->prev = b0->prev;
      if (new_goto->prev) {
        new_goto->prev->next = new_goto;
      }

      new_goto->next = b1->next;
      if (new_goto->next) {
        new_goto->next->prev = new_goto;
      }

      b0->succ_branch->replace_preds_with_and_check({b0}, nullptr);

      b0->parent_claim(new_goto);
      b1->parent_claim(new_goto);

      return false;
    }

    // keep looking
    return true;
  });

  return replaced;
}

bool ControlFlowGraph::is_sequence(CfgVtx* b0, CfgVtx* b1) {
  if (!b0 || !b1)
    return false;
  if (b0->next != b1)
    return false;
  if (b0->succ_ft != b1) {
    // may unconditionally branch to get to a loop.
    if (b0->succ_branch != b1)
      return false;
    if (b0->succ_ft)
      return false;
    assert(b0->end_branch.branch_always);
  } else {
    // falls through
    if (b0->succ_branch)
      return false;
    assert(!b0->end_branch.has_branch);
  }

  if (b1->prev != b0)
    return false;
  if (b1->pred.size() != 1)
    return false;
  if (!b1->has_pred(b0))
    return false;
  if (b1->succ_branch == b0)
    return false;

  return true;
}

bool ControlFlowGraph::is_sequence_of_non_sequences(CfgVtx* b0, CfgVtx* b1) {
  if (!b0 || !b1)
    return false;
  if (dynamic_cast<SequenceVtx*>(b0) || dynamic_cast<SequenceVtx*>(b1))
    return false;
  return is_sequence(b0, b1);
}

bool ControlFlowGraph::is_sequence_of_sequence_and_non_sequence(CfgVtx* b0, CfgVtx* b1) {
  if (!b0 || !b1)
    return false;
  if (!dynamic_cast<SequenceVtx*>(b0))
    return false;
  if (dynamic_cast<SequenceVtx*>(b1))
    return false;
  return is_sequence(b0, b1);
}

bool ControlFlowGraph::is_sequence_of_sequence_and_sequence(CfgVtx* b0, CfgVtx* b1) {
  if (!b0 || !b1)
    return false;
  if (!dynamic_cast<SequenceVtx*>(b0))
    return false;
  if (!dynamic_cast<SequenceVtx*>(b1))
    return false;
  return is_sequence(b0, b1);
}

bool ControlFlowGraph::is_sequence_of_non_sequence_and_sequence(CfgVtx* b0, CfgVtx* b1) {
  if (!b0 || !b1) {
    return false;
  }

  if (dynamic_cast<SequenceVtx*>(b0))
    return false;
  if (!dynamic_cast<SequenceVtx*>(b1))
    return false;
  return is_sequence(b0, b1);
}

/*!
 * Find and insert at most one sequence. Return true if sequence is inserted.
 * To generate more readable debug output, we should aim to run this as infrequent and as
 * late as possible, to avoid condition vertices with tons of extra junk packed in.
 */
bool ControlFlowGraph::find_seq_top_level() {
  bool replaced = false;
  for_each_top_level_vtx([&](CfgVtx* vtx) {
    auto* b0 = vtx;
    auto* b1 = vtx->next;

    //    if (b0 && b1) {
    //      printf("try seq %s %s\n", b0->to_string().c_str(), b1->to_string().c_str());
    //    }

    if (is_sequence_of_non_sequences(b0, b1)) {  // todo, avoid nesting sequences.
      //      printf("make seq type 1 %s %s\n", b0->to_string().c_str(), b1->to_string().c_str());
      replaced = true;

      auto* new_seq = alloc<SequenceVtx>();
      new_seq->seq.push_back(b0);
      new_seq->seq.push_back(b1);

      for (auto* new_pred : b0->pred) {
        new_pred->replace_succ_and_check(b0, new_seq);
      }
      new_seq->pred = b0->pred;

      for (auto* new_succ : b1->succs()) {
        new_succ->replace_pred_and_check(b1, new_seq);
      }
      new_seq->succ_ft = b1->succ_ft;
      new_seq->succ_branch = b1->succ_branch;

      new_seq->prev = b0->prev;
      if (new_seq->prev) {
        new_seq->prev->next = new_seq;
      }
      new_seq->next = b1->next;
      if (new_seq->next) {
        new_seq->next->prev = new_seq;
      }

      b0->parent_claim(new_seq);
      b1->parent_claim(new_seq);
      new_seq->end_branch = b1->end_branch;
      return false;
    }

    if (is_sequence_of_sequence_and_non_sequence(b0, b1)) {
      //      printf("make seq type 2 %s %s\n", b0->to_string().c_str(), b1->to_string().c_str());
      replaced = true;
      auto* seq = dynamic_cast<SequenceVtx*>(b0);
      assert(seq);

      seq->seq.push_back(b1);

      for (auto* new_succ : b1->succs()) {
        new_succ->replace_pred_and_check(b1, b0);
      }
      seq->succ_ft = b1->succ_ft;
      seq->succ_branch = b1->succ_branch;
      seq->next = b1->next;
      if (seq->next) {
        seq->next->prev = seq;
      }

      b1->parent_claim(seq);
      seq->end_branch = b1->end_branch;
      return false;
    }

    if (is_sequence_of_non_sequence_and_sequence(b0, b1)) {
      replaced = true;
      auto* seq = dynamic_cast<SequenceVtx*>(b1);
      assert(seq);
      seq->seq.insert(seq->seq.begin(), b0);

      for (auto* p : b0->pred) {
        p->replace_succ_and_check(b0, seq);
      }
      seq->pred = b0->pred;
      seq->prev = b0->prev;
      if (seq->prev) {
        seq->prev->next = seq;
      }

      b0->parent_claim(seq);
      return false;
    }

    if (is_sequence_of_sequence_and_sequence(b0, b1)) {
      //      printf("make seq type 3 %s %s\n", b0->to_string().c_str(), b1->to_string().c_str());
      replaced = true;
      auto* seq = dynamic_cast<SequenceVtx*>(b0);
      assert(seq);

      auto* old_seq = dynamic_cast<SequenceVtx*>(b1);
      assert(old_seq);

      for (auto* x : old_seq->seq) {
        x->parent_claim(seq);
        seq->seq.push_back(x);
      }

      for (auto* x : old_seq->succs()) {
        //        printf("fix preds of %s\n", x->to_string().c_str());
        x->replace_pred_and_check(old_seq, seq);
      }
      seq->succ_branch = old_seq->succ_branch;
      seq->succ_ft = old_seq->succ_ft;
      seq->end_branch = old_seq->end_branch;
      seq->next = old_seq->next;
      if (seq->next) {
        seq->next->prev = seq;
      }

      // todo - proper trash?
      old_seq->parent_claim(seq);

      return false;
    }

    return true;  // keep looking
  });

  return replaced;
}

namespace {

// is a found after b?
bool is_found_after(CfgVtx* a, CfgVtx* b) {
  b = b->next;
  while (b) {
    if (a == b) {
      return true;
    }
    b = b->next;
  }
  return false;
}

}  // namespace

bool ControlFlowGraph::find_cond_w_else() {
  bool found = false;

  for_each_top_level_vtx([&](CfgVtx* vtx) {
    // determine where the "else" block would be
    auto* c0 = vtx;       // first condition
    auto* b0 = c0->next;  // first body
    if (!b0) {
      return true;
    }

    //        printf("cwe try %s %s\n", c0->to_string().c_str(), b0->to_string().c_str());

    // first condition should have the _option_ to fall through to first body
    if (c0->succ_ft != b0 || c0->end_branch.kind != CfgVtx::DelaySlotKind::NOP) {
      return true;
    }

    // first body MUST unconditionally jump to else
    if (b0->succ_ft || b0->end_branch.branch_likely ||
        b0->end_branch.kind != CfgVtx::DelaySlotKind::NOP) {
      return true;
    }

    if (b0->pred.size() != 1) {
      return true;
    }

    assert(b0->end_branch.has_branch);
    assert(b0->end_branch.branch_always);
    assert(b0->succ_branch);

    // TODO - check what's in the delay slot!
    auto* end_block = b0->succ_branch;
    if (!end_block) {
      return true;
    }

    if (!is_found_after(end_block, b0)) {
      return true;
    }

    auto* else_block = end_block->prev;
    if (!else_block) {
      return true;
    }

    if (!is_found_after(else_block, b0)) {
      return true;
    }

    if (else_block->succ_branch) {
      return true;
    }

    if (else_block->succ_ft != end_block) {
      return true;
    }
    assert(!else_block->end_branch.has_branch);

    std::vector<CondWithElse::Entry> entries = {{c0, b0}};
    auto* prev_condition = c0;
    auto* prev_body = b0;

    // loop to try to grab all the cases up to the else, or reject if the inside is not sufficiently
    // compact or if this is not actually a cond with else Note, we are responsible for checking the
    // branch of prev_condition, but not the fallthrough
    while (true) {
      auto* next = prev_body->next;
      if (next == else_block) {
        // TODO - check what's in the delay slot!
        // we're done!
        // check the prev_condition, prev_body blocks properly go to the else/end_block
        // prev_condition should jump to else:
        if (prev_condition->succ_branch != else_block || prev_condition->end_branch.branch_likely ||
            prev_condition->end_branch.kind != CfgVtx::DelaySlotKind::NOP) {
          return true;
        }

        // prev_body should jump to end
        if (prev_body->succ_branch != end_block ||
            prev_body->end_branch.kind != CfgVtx::DelaySlotKind::NOP) {
          return true;
        }

        break;
      } else {
        auto* c = next;
        auto* b = c->next;
        if (!c || !b) {
          ;
          return true;
        };
        // attempt to add another

        if (c->pred.size() != 1) {
          return true;
        }

        if (b->pred.size() != 1) {
          return true;
        }

        // how to get to cond
        if (prev_condition->succ_branch != c || prev_condition->end_branch.branch_likely ||
            prev_condition->end_branch.kind != CfgVtx::DelaySlotKind::NOP) {
          return true;
        }

        if (prev_body->end_branch.kind != CfgVtx::DelaySlotKind::NOP) {
          return true;
        }

        if (c->succ_ft != b) {
          return true;  // condition should have the option to fall through if matched
        }

        // TODO - check what's in the delay slot!
        if (c->end_branch.branch_likely) {
          return true;  // otherwise should go to next with a non-likely branch
        }

        if (b->succ_ft || b->end_branch.branch_likely) {
          return true;  // body should go straight to else
        }

        if (b->succ_branch != end_block) {
          return true;
        }

        entries.emplace_back(c, b);
        prev_body = b;
        prev_condition = c;
      }
    }

    // now we need to add it
    //    printf("got cwe\n");
    auto new_cwe = alloc<CondWithElse>();

    // link x <-> new_cwe
    for (auto* npred : c0->pred) {
      npred->replace_succ_and_check(c0, new_cwe);
    }
    new_cwe->pred = c0->pred;
    new_cwe->prev = c0->prev;
    if (new_cwe->prev) {
      new_cwe->prev->next = new_cwe;
    }

    // link new_cwe <-> end
    std::vector<CfgVtx*> to_replace;
    to_replace.push_back(else_block);
    for (const auto& x : entries) {
      to_replace.push_back(x.body);
    }
    end_block->replace_preds_with_and_check(to_replace, new_cwe);
    new_cwe->succ_ft = end_block;
    new_cwe->next = end_block;
    end_block->prev = new_cwe;

    new_cwe->else_vtx = else_block;
    new_cwe->entries = std::move(entries);

    else_block->parent_claim(new_cwe);
    for (const auto& x : new_cwe->entries) {
      x.body->parent_claim(new_cwe);
      x.condition->parent_claim(new_cwe);
    }
    found = true;
    return false;
  });

  return found;
}

#define printf(format, ...) ;

bool ControlFlowGraph::find_cond_n_else() {
  bool found = false;

  for_each_top_level_vtx([&](CfgVtx* vtx) {
    printf("Try CNE on %s\n", vtx->to_string().c_str());
    auto* c0 = vtx;       // first condition
    auto* b0 = c0->next;  // first body
    if (!b0) {
      printf("reject 0\n");
      return true;
    }

    //            printf("cne: c0 %s b0 %s\n", c0->to_string().c_str(), b0->to_string().c_str());

    // first condition should have the _option_ to fall through to first body
    if (c0->succ_ft != b0) {
      printf("reject 1\n");
      return true;
    }

    // first body MUST unconditionally jump to end
    bool single_case = false;
    if (b0->end_branch.has_branch) {
      if (b0->succ_ft || b0->end_branch.branch_likely ||
          b0->end_branch.kind != CfgVtx::DelaySlotKind::NOP) {
        printf("reject 2A\n");
        return true;
      }
      assert(b0->end_branch.has_branch);
      assert(b0->end_branch.branch_always);
      assert(b0->succ_branch);
    } else {
      single_case = true;
    }

    if (b0->pred.size() != 1) {
      printf("reject 3\n");
      return true;
    }

    // TODO - check what's in the delay slot!
    auto* end_block = single_case ? b0->succ_ft : b0->succ_branch;
    if (!end_block) {
      printf("reject 4");
      return true;
    }

    if (!is_found_after(end_block, b0)) {
      printf("reject 5");
      return true;
    }

    std::vector<CondNoElse::Entry> entries = {{c0, b0}};
    auto* prev_condition = c0;
    auto* prev_body = b0;
    printf("add default entry %s %s\n", c0->to_string().c_str(), b0->to_string().c_str());
    printf("end_block = %s\n", end_block->to_string().c_str());

    // loop to try to grab all the cases up to the else, or reject if the inside is not sufficiently
    // compact or if this is not actually a cond with else Note, we are responsible for checking the
    // branch of prev_condition, but not the fallthrough
    while (true) {
      auto* next = prev_body->next;
      if (next == end_block) {
        // TODO - check what's in the delay slot!
        // we're done!
        // check the prev_condition, prev_body blocks properly go to the else/end_block
        // prev_condition should jump to else:
        // note - a GOAL branching NOT will be recognized as a single case COND with no else.
        // but the branch will be a register set true
        if (prev_condition->succ_branch != end_block || prev_condition->end_branch.branch_likely ||
            (prev_condition->end_branch.kind != CfgVtx::DelaySlotKind::SET_REG_FALSE &&
             prev_condition->end_branch.kind != CfgVtx::DelaySlotKind::SET_REG_TRUE)) {
          printf("reject 6\n");
          return true;
        }

        // if we are a not, we can have only one case. (I think).
        if (prev_condition->end_branch.kind == CfgVtx::DelaySlotKind::SET_REG_TRUE &&
            entries.size() > 1) {
          return true;
        }

        // prev_body should fall through to end todo - this was wrong?
        if (prev_body->succ_ft != end_block) {
          printf("reject 7\n");
          return true;
        }

        break;
      } else {
        // need to check pc->c
        // need to check pb->e
        // need to check c->b
        auto* c = next;
        auto* b = c->next;
        printf("add next entry %s %s\n", c->to_string().c_str(), b->to_string().c_str());
        if (!c || !b) {
          printf("reject 8\n");
          return true;
        };
        // attempt to add another
        //        printf("  e %s %s\n", c->to_string().c_str(), b->to_string().c_str());

        if (c->pred.size() != 1) {
          printf("reject 9\n");
          return true;
        }

        if (b->pred.size() != 1) {
          printf("reject 10 body %s\n", b->to_string().c_str());
          return true;
        }

        // how to get to cond (pc->c)
        if (prev_condition->succ_branch != c || prev_condition->end_branch.branch_likely ||
            prev_condition->end_branch.kind != CfgVtx::DelaySlotKind::SET_REG_FALSE) {
          printf("reject 11\n");
          return true;
        }

        // (c->b)
        if (c->succ_ft != b) {
          printf("reject 12\n");
          return true;  // condition should have the option to fall through if matched
        }

        if (c->end_branch.branch_likely ||
            c->end_branch.kind != CfgVtx::DelaySlotKind::SET_REG_FALSE) {
          printf("reject 13\n");
          return true;  // otherwise should go to next with a non-likely branch
        }

        if (prev_body->succ_ft || prev_body->end_branch.branch_likely ||
            prev_body->end_branch.kind != CfgVtx::DelaySlotKind::NOP) {
          printf("reject 14 on b %s %d %d %d\n", prev_body->to_string().c_str(),
                 !!prev_body->succ_ft, prev_body->end_branch.branch_likely,
                 prev_body->end_branch.kind != CfgVtx::DelaySlotKind::NOP);
          return true;  // body should go straight to else
        }

        if (prev_body->succ_branch != end_block) {
          printf("reject 15\n");
          return true;
        }

        entries.emplace_back(c, b);
        prev_body = b;
        prev_condition = c;
      }
    }

    // let's try to detect if this is an incomplete one.
    if (c0->prev) {
      if (c0->prev->succ_ft == nullptr && c0->prev->succ_branch == end_block &&
          c0->prev->end_branch.kind == CfgVtx::DelaySlotKind::NOP &&
          !c0->prev->end_branch.branch_likely) {
        // the previous body looks suspicious.
        for (auto pred : c0->pred) {
          // also check that we have the body skip to avoid false positives when the entire body of
          // a while loop is wrapped in a CNE with a single case.
          if (pred->succ_branch == c0 &&
              pred->end_branch.kind == CfgVtx::DelaySlotKind::SET_REG_FALSE) {
            printf("Suspisious reject\n");
            return true;
          }
        }
      }
    }

    // now we need to add it
    //    printf("got cne\n");
    auto new_cwe = alloc<CondNoElse>();

    // link x <-> new_cwe
    for (auto* npred : c0->pred) {
      //      printf("in %s, replace succ %s with %s\n", npred->to_string().c_str(),
      //      c0->to_string().c_str(), new_cwe->to_string().c_str());
      npred->replace_succ_and_check(c0, new_cwe);
    }
    new_cwe->pred = c0->pred;
    new_cwe->prev = c0->prev;
    if (new_cwe->prev) {
      new_cwe->prev->next = new_cwe;
    }

    // link new_cwe <-> end
    std::vector<CfgVtx*> to_replace;
    for (const auto& x : entries) {
      to_replace.push_back(x.body);
    }
    to_replace.push_back(entries.back().condition);
    //    if(single_case) {
    //      to_replace.push_back(c0);
    //    }
    end_block->replace_preds_with_and_check(to_replace, new_cwe);
    new_cwe->succ_ft = end_block;
    new_cwe->next = end_block;
    end_block->prev = new_cwe;

    new_cwe->entries = std::move(entries);

    for (const auto& x : new_cwe->entries) {
      x.body->parent_claim(new_cwe);
      x.condition->parent_claim(new_cwe);
    }
    found = true;

    //    printf("now %s\n", new_cwe->to_form()->toStringSimple().c_str());
    //    printf("%s\n", to_dot().c_str());
    return false;
  });

  return found;
}
#undef printf

bool ControlFlowGraph::find_short_circuits() {
  bool found = false;

  for_each_top_level_vtx([&](CfgVtx* vtx) {
    std::vector<CfgVtx*> entries = {vtx};
    auto* end = vtx->succ_branch;
    auto* next = vtx->next;

    //    printf("try sc @ %s\n", vtx->to_string().c_str());
    if (!end || !vtx->end_branch.branch_likely || next != vtx->succ_ft) {
      //      printf("reject 1\n");
      return true;
    }

    while (true) {
      //      printf("loop sc %s, end %s\n", vtx->to_string().c_str(), end->to_string().c_str());
      if (next == end) {
        // one entry sc!
        break;
      }

      if (next->next == end) {
        // check 1 pred
        if (next->pred.size() != 1) {
          //          printf("reject 2\n");
          return true;
        }
        entries.push_back(next);

        // done!
        break;
      }

      // check 1 pred
      if (next->pred.size() != 1) {
        //        printf("reject 3\n");
        return true;
      }

      // check branch to end
      if (next->succ_branch != end || !next->end_branch.branch_likely) {
        //        printf("reject 4\n");
        return true;
      }

      // check fallthrough to next
      if (!next->succ_ft) {
        //        printf("reject 5\n");
        return true;
      }

      assert(next->succ_ft == next->next);  // bonus check
      entries.push_back(next);
      next = next->succ_ft;
    }

    //    printf("got sc: \n");
    //    for (auto* x : entries) {
    //      printf("  %s\n", x->to_string().c_str());
    //    }

    auto new_sc = alloc<ShortCircuit>();

    for (auto* npred : vtx->pred) {
      npred->replace_succ_and_check(vtx, new_sc);
    }
    new_sc->pred = vtx->pred;
    new_sc->prev = vtx->prev;
    if (new_sc->prev) {
      new_sc->prev->next = new_sc;
    }

    end->replace_preds_with_and_check(entries, new_sc);
    new_sc->succ_ft = end;
    new_sc->next = end;
    end->prev = new_sc;
    new_sc->entries = std::move(entries);
    for (auto* x : new_sc->entries) {
      x->parent_claim(new_sc);
    }
    found = true;

    return false;
  });

  return found;
}

/*!
 * Create vertices for basic blocks.  Should only be called once to create all blocks at once.
 * Will set up the next/prev relation for all of them, but not the pred/succ.
 * The returned vector will have blocks in ordered, so the i-th entry is for the i-th block.
 */
const std::vector<BlockVtx*>& ControlFlowGraph::create_blocks(int count) {
  assert(m_blocks.empty());
  BlockVtx* prev = nullptr;  // for linking next/prev

  for (int i = 0; i < count; i++) {
    auto* new_block = alloc<BlockVtx>(i);

    // link next/prev
    new_block->prev = prev;
    if (prev) {
      prev->next = new_block;
    }
    prev = new_block;

    m_blocks.push_back(new_block);
  }

  return m_blocks;
}

/*!
 * Setup pred/succ for a block which falls through to the next.
 */
void ControlFlowGraph::link_fall_through(BlockVtx* first,
                                         BlockVtx* second,
                                         std::vector<BasicBlock>& blocks) {
  assert(!first->succ_ft);  // don't want to overwrite something by accident.
  // can only fall through to the next code in memory.
  assert(first->next == second);
  assert(second->prev == first);
  first->succ_ft = second;
  assert(blocks.at(first->block_id).succ_ft == -1);
  blocks.at(first->block_id).succ_ft = second->block_id;

  if (!second->has_pred(first)) {
    // if a block can block fall through and branch to the same block, we want to avoid adding
    // it as a pred twice. This is rare, but does happen and makes sense with likely branches
    // which only run the delay slot when taken.
    second->pred.push_back(first);
    blocks.at(second->block_id).pred.push_back(first->block_id);
  }
}

/*!
 * Setup pred/succ for a block which branches to second.
 */
void ControlFlowGraph::link_branch(BlockVtx* first,
                                   BlockVtx* second,
                                   std::vector<BasicBlock>& blocks) {
  assert(!first->succ_branch);
  first->succ_branch = second;
  assert(blocks.at(first->block_id).succ_branch == -1);
  blocks.at(first->block_id).succ_branch = second->block_id;

  if (!second->has_pred(first)) {
    // see comment in link_fall_through
    second->pred.push_back(first);
    blocks.at(second->block_id).pred.push_back(first->block_id);
  }
}

void ControlFlowGraph::flag_early_exit(const std::vector<BasicBlock>& blocks) {
  auto* b = m_blocks.back();
  const auto& block = blocks.at(b->block_id);

  if (block.start_word == block.end_word) {
    b->is_early_exit_block = true;
    assert(!b->end_branch.has_branch);
  }
}

CfgVtx::DelaySlotKind get_delay_slot(const Instruction& i) {
  if (is_nop(i)) {
    return CfgVtx::DelaySlotKind::NOP;
  } else if (is_gpr_3(i, InstructionKind::OR, {}, Register(Reg::GPR, Reg::S7),
                      Register(Reg::GPR, Reg::R0))) {
    return CfgVtx::DelaySlotKind::SET_REG_FALSE;
  } else if (is_gpr_2_imm_int(i, InstructionKind::DADDIU, {}, Register(Reg::GPR, Reg::S7), 8)) {
    return CfgVtx::DelaySlotKind::SET_REG_TRUE;
  } else {
    return CfgVtx::DelaySlotKind::OTHER;
  }
}

/*!
 * Build and resolve a Control Flow Graph as much as possible.
 */
std::shared_ptr<ControlFlowGraph> build_cfg(const LinkedObjectFile& file, int seg, Function& func) {
  auto cfg = std::make_shared<ControlFlowGraph>();

  const auto& blocks = cfg->create_blocks(func.basic_blocks.size());

  // add entry block
  cfg->entry()->succ_ft = blocks.front();
  blocks.front()->pred.push_back(cfg->entry());

  // add exit block
  cfg->exit()->pred.push_back(blocks.back());
  blocks.back()->succ_ft = cfg->exit();

  // todo - early returns!

  // set up succ / pred
  for (int i = 0; i < int(func.basic_blocks.size()); i++) {
    auto& b = func.basic_blocks[i];
    bool not_last = (i + 1) < int(func.basic_blocks.size());

    if (b.end_word - b.start_word < 2) {
      // there's no room for a branch here, fall through to the end
      if (not_last) {
        cfg->link_fall_through(blocks.at(i), blocks.at(i + 1), func.basic_blocks);
      }
    } else {
      // might be a branch
      int idx = b.end_word - 2;
      assert(idx >= b.start_word);
      auto& branch_candidate = func.instructions.at(idx);
      auto& delay_slot_candidate = func.instructions.at(idx + 1);

      if (is_branch(branch_candidate, {})) {
        blocks.at(i)->end_branch.has_branch = true;
        blocks.at(i)->end_branch.branch_likely = is_branch(branch_candidate, true);
        blocks.at(i)->end_branch.kind = get_delay_slot(delay_slot_candidate);
        bool branch_always = is_always_branch(branch_candidate);

        // need to find block target
        int block_target = -1;
        int label_target = branch_candidate.get_label_target();
        assert(label_target != -1);
        const auto& label = file.labels.at(label_target);
        assert(label.target_segment == seg);
        assert((label.offset % 4) == 0);
        int offset = label.offset / 4 - func.start_word;
        assert(offset >= 0);

        // the order here matters when there are zero size blocks. Unclear what the best answer is.
        //  i think in end it doesn't actually matter??
        //        for (int j = 0; j < int(func.basic_blocks.size()); j++) {
        for (int j = int(func.basic_blocks.size()); j-- > 0;) {
          if (func.basic_blocks[j].start_word == offset) {
            block_target = j;
            break;
          }
        }

        assert(block_target != -1);
        cfg->link_branch(blocks.at(i), blocks.at(block_target), func.basic_blocks);

        if (branch_always) {
          // don't continue to the next one
          blocks.at(i)->end_branch.branch_always = true;
        } else {
          // not an always branch
          if (not_last) {
            cfg->link_fall_through(blocks.at(i), blocks.at(i + 1), func.basic_blocks);
          }
        }
      } else {
        // not a branch at all
        if (not_last) {
          cfg->link_fall_through(blocks.at(i), blocks.at(i + 1), func.basic_blocks);
        }
      }
    }
  }

  cfg->flag_early_exit(func.basic_blocks);

  bool changed = true;
  while (changed) {
    changed = false;
    // note - we should prioritize finding short-circuiting expressions.
    //        printf("%s\n", cfg->to_dot().c_str());
    //    printf("%s\n", cfg->to_form().print().c_str());

    // todo - should we lower the priority of the conds?

    changed = changed || cfg->find_cond_w_else();

    changed = changed || cfg->find_while_loop_top_level();
    changed = changed || cfg->find_seq_top_level();
    changed = changed || cfg->find_short_circuits();
    changed = changed || cfg->find_cond_n_else();

    if (!changed) {
      changed = changed || cfg->find_goto_end();
      changed = changed || cfg->find_until_loop();
      changed = changed || cfg->find_until1_loop();
      changed = changed || cfg->find_infinite_loop();
    };

    if (!changed) {
      changed = changed || cfg->find_goto_not_end();
    }
  }

  if (!cfg->is_fully_resolved()) {
    func.warnings += ";; Failed to fully resolve CFG\n";
  }

  return cfg;
}
}  // namespace decompiler