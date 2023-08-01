#include "CfgVtx.h"

#include "Function.h"

#include "common/goos/PrettyPrinter.h"
#include "common/log/log.h"
#include "common/symbols.h"
#include "common/util/Assert.h"

#include "decompiler/Disasm/InstructionMatching.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"

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
      ASSERT(!replaced);
      x = new_pred;
      replaced = true;
    }
  }
  ASSERT(replaced);
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

  ASSERT(replaced);
}

void CfgVtx::remove_pred(CfgVtx* to_remove) {
  bool found = false;
  for (auto it = pred.begin(); it != pred.end(); it++) {
    if (*it == to_remove) {
      pred.erase(it);
      found = true;
      break;
    }
  }
  ASSERT(found);
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
        ASSERT(!match);
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
    ASSERT(x);
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

  result += "  start: " + std::to_string(get_first_block_id()) + "\n";

  if (end_branch.asm_branch) {
    result += "  ASM BRANCH\n";
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

std::string BlockVtx::to_string() const {
  if (is_early_exit_block) {
    return "Block (EA) " + std::to_string(block_id);
  } else {
    return "Block " + std::to_string(block_id);
  }
}

goos::Object BlockVtx::to_form() const {
  return pretty_print::to_symbol("b" + std::to_string(block_id));
}

int BlockVtx::get_first_block_id() const {
  return block_id;
}

std::string SequenceVtx::to_string() const {
  ASSERT(!seq.empty());
  // todo - this is not a great way to print it. Maybe sequences should have an ID or name?
  std::string result =
      "Seq " + seq.front()->to_string() + " ... " + seq.back()->to_string() + std::to_string(uid);
  return result;
}

goos::Object SequenceVtx::to_form() const {
  std::vector<goos::Object> forms;
  forms.push_back(pretty_print::to_symbol("seq"));
  for (auto* x : seq) {
    forms.push_back(x->to_form());
  }
  return pretty_print::build_list(forms);
}

int SequenceVtx::get_first_block_id() const {
  return seq.at(0)->get_first_block_id();
}

std::string EntryVtx::to_string() const {
  return "ENTRY";
}

goos::Object EntryVtx::to_form() const {
  return pretty_print::to_symbol("entry");
}

int EntryVtx::get_first_block_id() const {
  ASSERT(false);
  return -1;
}

std::string ExitVtx::to_string() const {
  return "EXIT";
}

goos::Object ExitVtx::to_form() const {
  return pretty_print::to_symbol("exit");
}

int ExitVtx::get_first_block_id() const {
  ASSERT(false);
  return -1;
}

std::string CondWithElse::to_string() const {
  return "CONDWE" + std::to_string(uid);
}

goos::Object CondWithElse::to_form() const {
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

int CondWithElse::get_first_block_id() const {
  return entries.at(0).condition->get_first_block_id();
}

std::string CondNoElse::to_string() const {
  return "CONDNE" + std::to_string(uid);
}

goos::Object CondNoElse::to_form() const {
  std::vector<goos::Object> forms;
  forms.push_back(pretty_print::to_symbol("cond"));
  for (const auto& x : entries) {
    std::vector<goos::Object> e = {x.condition->to_form(), x.body->to_form()};
    forms.push_back(pretty_print::build_list(e));
  }
  return pretty_print::build_list(forms);
}

int CondNoElse::get_first_block_id() const {
  return entries.at(0).condition->get_first_block_id();
}

std::string WhileLoop::to_string() const {
  return "WHL" + std::to_string(uid);
}

goos::Object WhileLoop::to_form() const {
  std::vector<goos::Object> forms = {pretty_print::to_symbol("while"), condition->to_form(),
                                     body->to_form()};
  return pretty_print::build_list(forms);
}

int WhileLoop::get_first_block_id() const {
  ASSERT(false);
  return -1;
}

std::string UntilLoop::to_string() const {
  return "UNTL" + std::to_string(uid);
}

goos::Object UntilLoop::to_form() const {
  std::vector<goos::Object> forms = {pretty_print::to_symbol("until"), condition->to_form(),
                                     body->to_form()};
  return pretty_print::build_list(forms);
}

int UntilLoop::get_first_block_id() const {
  return condition->get_first_block_id();
}

std::string UntilLoop_single::to_string() const {
  return "UNTLS" + std::to_string(uid);
}

goos::Object UntilLoop_single::to_form() const {
  std::vector<goos::Object> forms = {pretty_print::to_symbol("until1"), block->to_form()};
  return pretty_print::build_list(forms);
}

int UntilLoop_single::get_first_block_id() const {
  return block->get_first_block_id();
  ASSERT(false);
  return -1;
}

std::string InfiniteLoopBlock::to_string() const {
  return "INFL" + std::to_string(uid);
}

goos::Object InfiniteLoopBlock::to_form() const {
  std::vector<goos::Object> forms = {pretty_print::to_symbol("inf-loop"), block->to_form()};
  return pretty_print::build_list(forms);
}

int InfiniteLoopBlock::get_first_block_id() const {
  ASSERT(false);
  return -1;
}

std::string ShortCircuit::to_string() const {
  return "SC" + std::to_string(uid);
}

goos::Object ShortCircuit::to_form() const {
  std::vector<goos::Object> forms;
  forms.push_back(pretty_print::to_symbol("sc"));
  for (const auto& x : entries) {
    if (x.likely_delay) {
      forms.push_back(pretty_print::build_list(x.condition->to_form(), x.likely_delay->to_form()));
    } else {
      forms.push_back(x.condition->to_form());
    }
  }
  return pretty_print::build_list(forms);
}

int ShortCircuit::get_first_block_id() const {
  return entries.at(0).condition->get_first_block_id();
}

std::string GotoEnd::to_string() const {
  return "goto_end" + std::to_string(uid);
}

goos::Object GotoEnd::to_form() const {
  std::vector<goos::Object> forms = {pretty_print::to_symbol("return-from-function"),
                                     body->to_form(), unreachable_block->to_form()};
  return pretty_print::build_list(forms);
}

int GotoEnd::get_first_block_id() const {
  return body->get_first_block_id();
}

std::string Break::to_string() const {
  return "goto" + std::to_string(uid);
}

goos::Object Break::to_form() const {
  if (unreachable_block) {
    std::vector<goos::Object> forms = {pretty_print::to_symbol("break"),
                                       pretty_print::to_symbol(std::to_string(dest_block_id)),
                                       body->to_form(), unreachable_block->to_form()};
    return pretty_print::build_list(forms);
  } else {
    std::vector<goos::Object> forms = {pretty_print::to_symbol("break"),
                                       pretty_print::to_symbol(std::to_string(dest_block_id)),
                                       body->to_form(), pretty_print::to_symbol("no-unreachable")};
    return pretty_print::build_list(forms);
  }
}

int Break::get_first_block_id() const {
  return body->get_first_block_id();
}

std::string EmptyVtx::to_string() const {
  return "empty";
}

goos::Object EmptyVtx::to_form() const {
  return pretty_print::build_list("empty");
}

int EmptyVtx::get_first_block_id() const {
  ASSERT(false);
  return -1;
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
  ASSERT(get_top_level_vertices_count() == 1);
  for (auto* x : m_node_pool) {
    if (!x->parent && x != entry() && x != exit()) {
      return x;
    }
  }
  ASSERT(false);
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

bool ControlFlowGraph::is_while_loop(CfgVtx* b0, CfgVtx* b1, CfgVtx* b2) {
  // todo - check delay slots!
  if (!b0 || !b1 || !b2)
    return false;

  bool debug = b0->to_string() == "Seq CONDNE104 ... Block 18100";

  if (debug) {
    lg::debug("try while: {} | {} | {}", b0->to_string(), b1->to_string(), b2->to_string());
  }

  if (b0->end_branch.asm_branch || b1->end_branch.asm_branch) {
    if (debug)
      lg::debug("reject 1 {} {}", b0->end_branch.asm_branch, b1->end_branch.asm_branch);
    return false;
  }

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
  ASSERT(b0->end_branch.has_branch);
  ASSERT(b0->end_branch.branch_always);
  if (b0->end_branch.branch_likely)
    return false;

  // check b1 -> b2 fallthrough
  if (b1->succ_ft != b2)
    return false;
  if (b1->succ_branch)
    return false;
  ASSERT(!b1->end_branch.has_branch);
  if (!b2->has_pred(b0)) {
    lg::debug("expect b2 ({}) to have pred b0 ({})", b2->to_string().c_str(),
              b0->to_string().c_str());
    lg::debug("but it doesn't! instead it has:");
    for (auto* x : b2->pred) {
      lg::debug(" {}", x->to_string().c_str());
    }
    if (b0->succ_ft) {
      lg::debug("b0's succ_ft: {}", b0->succ_ft->to_string().c_str());
    }
    if (b0->succ_branch) {
      lg::debug("b0's succ_branch: {}", b0->succ_branch->to_string().c_str());
    }
  }
  ASSERT(b2->has_pred(b0));
  ASSERT(b2->has_pred(b1));
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

  if (b2->end_branch.asm_branch) {
    return false;
  }

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
  ASSERT(!b1->end_branch.has_branch);

  ASSERT_MSG(b2->has_pred(b1),
             fmt::format("Graph error {} (s {}) should have pred {} (s {})\n", b2->to_string(),
                         b2->get_first_block_id(), b1->to_string(), b1->get_first_block_id()));
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

  if (b0->end_branch.asm_branch || b1->end_branch.asm_branch) {
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

  ASSERT(b1->prev == b0);

  // b1 should have no preds and be unreachable.
  if (!b1->pred.empty()) {
    return false;
  }

  return true;  // match!
}

bool ControlFlowGraph::is_infinite_continue(CfgVtx* b0) {
  if (!b0) {
    return false;
  }

  // end branch always.
  if (!b0->end_branch.has_branch || !b0->end_branch.branch_always || b0->end_branch.branch_likely) {
    return false;
  }

  return true;
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
  ASSERT(b1->prev == b0);

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

        ASSERT(b2->succ_ft);
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

        ASSERT(b2->succ_ft);
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
    if (vtx->succ_branch == vtx && !vtx->succ_ft && !vtx->end_branch.asm_branch) {
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
    if (vtx->succ_branch == vtx && vtx->succ_ft && !vtx->end_branch.asm_branch) {
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
    if (is_goto_end_and_unreachable(b0, b1) && !b0->end_branch.asm_branch) {
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

int get_prev_count(CfgVtx* start, CfgVtx* to_find) {
  int result = 0;
  while (start && start != to_find) {
    result++;
    start = start->prev;
  }

  if (start == to_find) {
    return result;
  }
  return -1;
}

bool ControlFlowGraph::find_infinite_continue() {
  bool replaced = false;

  for_each_top_level_vtx([&](CfgVtx* vtx) {
    auto* b0 = vtx;
    if (is_infinite_continue(b0)) {
      int my_block = b0->get_first_block_id();
      int dest_block = b0->succ_branch->get_first_block_id();

      if (b0->end_branch.asm_branch) {
        return true;
      }
      if (dest_block >= my_block) {
        return true;
      }

      int prev_count = get_prev_count(b0, b0->succ_branch);
      if (prev_count == -1) {
        return true;
      }
      replaced = true;

      auto* new_goto = alloc<Break>();
      m_has_break = true;
      new_goto->body = b0;
      new_goto->unreachable_block = nullptr;
      new_goto->dest_block_id = b0->succ_branch->get_first_block_id();
      m_blocks.at(new_goto->dest_block_id)->needs_label = true;

      // patch up thing -> goto branches
      for (auto* new_pred : b0->pred) {
        new_pred->replace_succ_and_check(b0, new_goto);
      }
      new_goto->pred = b0->pred;

      ASSERT(b0->succs().size() == 1 && b0->succs().front() == b0->succ_branch);

      // patch up next and prev.
      new_goto->next = b0->next;
      if (new_goto->next) {
        ASSERT(new_goto->next->prev == b0);
        new_goto->next->prev = new_goto;
      }
      new_goto->prev = b0->prev;
      if (new_goto->prev) {
        ASSERT(new_goto->prev->next == b0);
        new_goto->prev->next = new_goto;
      }

      // now we want to make it look like the goto will fall through to next.
      if (new_goto->next) {
        // now we will fall through
        new_goto->succ_ft = b0->next;
        ASSERT(!new_goto->succ_ft->has_pred(new_goto));
        new_goto->succ_ft->pred.push_back(new_goto);
        ASSERT(!new_goto->succ_branch);
      }

      // break goto preds.
      b0->succ_branch->replace_preds_with_and_check({b0}, nullptr);

      b0->parent_claim(new_goto);
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

    // this can't work on asm branches because the structuring will fail.
    if (is_goto_not_end_and_unreachable(b0, b1) && b0 && !b0->end_branch.asm_branch) {
      replaced = true;

      auto* new_goto = alloc<Break>();
      m_has_break = true;
      new_goto->body = b0;
      new_goto->unreachable_block = b1;
      new_goto->dest_block_id = b0->succ_branch->get_first_block_id();
      m_blocks.at(new_goto->dest_block_id)->needs_label = true;

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

bool ControlFlowGraph::is_sequence(CfgVtx* b0, CfgVtx* b1, bool allow_self_loops) {
  if (!b0 || !b1)
    return false;

  //  if (b0->end_branch.asm_branch || b1->end_branch.asm_branch) {
  //    return false;
  //  }

  if (b0->next != b1) {
    return false;
  }
  if (b0->succ_ft != b1) {
    // may unconditionally branch to get to a loop.
    if (b0->succ_branch != b1)
      return false;
    if (b0->succ_ft)
      return false;
    ASSERT(b0->end_branch.branch_always);
  } else {
    // falls through
    if (b0->succ_branch)
      return false;
    ASSERT(!b0->end_branch.has_branch);
  }

  if (b1->prev != b0)
    return false;
  if (b1->pred.size() != 1)
    return false;
  if (!b1->has_pred(b0))
    return false;

  if (!allow_self_loops && b1->succ_branch == b0)
    return false;

  return true;
}

bool debug_asm_branch = false;
/*!
 * This is a weird and special pass that takes something like:
 * B0
 *  asm branch
 * B1
 *
 * and merges B0B1 into a single block with a
 */
bool ControlFlowGraph::clean_up_asm_branches() {
  bool replaced = false;
  for_each_top_level_vtx([&](CfgVtx* vtx) {
    auto* b0 = vtx;
    auto* b1 = vtx->next;
    if (!b1) {
      return true;
    }

    if (!b0->end_branch.asm_branch || !b0->end_branch.has_branch) {
      return true;
    }

    if (b1->succ_branch == b1) {
      // asm branch to yourself. just remove it.
      b1->succ_branch = nullptr;
      b1->end_branch.has_branch = false;
      b1->remove_pred(b1);
      replaced = true;
      return false;
    }

    // don't want to combine two with an incoming edge in between.
    if (b1->pred.size() > 1) {
      return true;
    } else {
      if (b1->pred.size() == 1 && !b1->has_pred(b1->prev)) {
        return true;
      }
    }

    if (b0->end_branch.branch_likely) {
      auto* bds = b1;
      b1 = bds->next;
      if (!b1) {
        return true;
      }

      if (debug_asm_branch) {
        lg::debug("Looks like asm likely branch: {} {} to {}", b0->to_string(), bds->to_string(),
                  b1->to_string());
      }

      auto* b0_seq = dynamic_cast<SequenceVtx*>(b0);
      auto* b1_seq = dynamic_cast<SequenceVtx*>(b1);
      if (!b0_seq && !b1_seq) {
        // build new sequence
        replaced = true;

        m_blocks.at(bds->succ_branch->get_first_block_id())->needs_label = true;

        auto* new_seq = alloc<SequenceVtx>();
        new_seq->seq.push_back(b0);
        new_seq->seq.push_back(bds);
        new_seq->seq.push_back(b1);

        for (auto* new_pred : b0->pred) {
          if (debug_asm_branch) {
            lg::debug("  pred {}", new_pred->to_string());
          }
          new_pred->replace_succ_and_check(b0, new_seq);
        }
        new_seq->pred = b0->pred;

        if (b0->succ_branch) {
          b0->succ_branch->replace_preds_with_and_check({b0}, nullptr);
        }

        if (bds->succ_branch) {
          // likely delay slots "branch" in this graph.
          bds->succ_branch->replace_preds_with_and_check({bds}, nullptr);
        }

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
        bds->parent_claim(new_seq);
        b1->parent_claim(new_seq);
        new_seq->end_branch = b1->end_branch;

        return false;
      } else if (b0_seq && b1_seq) {
        replaced = true;
        m_blocks.at(bds->succ_branch->get_first_block_id())->needs_label = true;
        auto* seq = dynamic_cast<SequenceVtx*>(b0);
        ASSERT(seq);

        auto* old_seq = dynamic_cast<SequenceVtx*>(b1);
        ASSERT(old_seq);

        if (b0->succ_branch) {
          b0->succ_branch->replace_preds_with_and_check({b0}, nullptr);
        }

        if (bds->succ_branch) {
          // likely delay slots "branch" in this graph.
          bds->succ_branch->replace_preds_with_and_check({bds}, nullptr);
        }

        seq->seq.push_back(bds);

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
        bds->parent_claim(seq);

        return false;
      } else if (b0_seq && !b1_seq) {
        replaced = true;
        m_blocks.at(bds->succ_branch->get_first_block_id())->needs_label = true;

        auto* seq = dynamic_cast<SequenceVtx*>(b0);
        ASSERT(seq);

        if (b0->succ_branch) {
          b0->succ_branch->replace_preds_with_and_check({b0}, nullptr);
        }

        if (bds->succ_branch) {
          // likely delay slots "branch" in this graph.
          bds->succ_branch->replace_preds_with_and_check({bds}, nullptr);
        }

        seq->seq.push_back(bds);

        seq->seq.push_back(b1);

        for (auto* x : b1->succs()) {
          //        printf("fix preds of %s\n", x->to_string().c_str());
          x->replace_pred_and_check(b1, seq);
        }
        seq->succ_branch = b1->succ_branch;
        seq->succ_ft = b1->succ_ft;
        seq->end_branch = b1->end_branch;
        seq->next = b1->next;
        if (seq->next) {
          seq->next->prev = seq;
        }

        // todo - proper trash?
        b1->parent_claim(seq);
        bds->parent_claim(seq);

        return false;
      }

      else {
        lg::error("unhandled sequences in clean_up_asm_branches likely seq: {} {}", !!b0_seq,
                  !!b1_seq);
        lg::error("{} {}", b0->get_first_block_id(), b1->get_first_block_id());
      }

    } else {
      if (debug_asm_branch) {
        lg::debug("Looks like asm normal branch: {} to {}", b0->to_string(), b1->to_string());
      }
      auto* b0_seq = dynamic_cast<SequenceVtx*>(b0);
      auto* b1_seq = dynamic_cast<SequenceVtx*>(b1);

      if (!b0_seq && !b1_seq) {
        if (debug_asm_branch) {
          lg::debug("[combo nn] {} and {}", b0->get_first_block_id(), b1->get_first_block_id());
        }
        // build new sequence
        replaced = true;
        if (!b0->succ_branch) {
          ASSERT_MSG(false, fmt::format("asm missing branch in block {}", b0->to_string()));
        }
        m_blocks.at(b0->succ_branch->get_first_block_id())->needs_label = true;

        auto* new_seq = alloc<SequenceVtx>();
        new_seq->seq.push_back(b0);
        new_seq->seq.push_back(b1);

        for (auto* new_pred : b0->pred) {
          new_pred->replace_succ_and_check(b0, new_seq);
        }
        new_seq->pred = b0->pred;

        if (b0->succ_branch) {
          b0->succ_branch->replace_preds_with_and_check({b0}, nullptr);
        }

        for (auto* new_succ : b1->succs()) {
          if (debug_asm_branch) {
            lg::debug("changing {}'s pred {} to seq. bc: {}", new_succ->to_string(),
                      b1->to_string(), new_succ->pred.size());
          }
          new_succ->replace_pred_and_check(b1, new_seq);
          if (debug_asm_branch) {
            lg::debug(" ac: {}", new_succ->pred.size());
          }
        }
        new_seq->succ_ft = b1->succ_ft;

        if (b1->succ_branch && debug_asm_branch) {
          lg::debug("combining {} and {} into a sequence, succ {}", b0->get_first_block_id(),
                    b1->get_first_block_id(), b1->succ_branch->get_first_block_id());
        }

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

        if (new_seq->succ_branch) {
          ASSERT(!new_seq->succ_branch->parent);
        }

        new_seq->end_branch = b1->end_branch;
        return false;
      } else if (b0_seq && !b1_seq) {
        if (debug_asm_branch) {
          lg::debug("[combo sn] {} and {}", b0->get_first_block_id(), b1->get_first_block_id());
          lg::debug("expanding sequence: {} (s {}) to include {}", b0_seq->to_string(),
                    b0_seq->get_first_block_id(), b1->get_first_block_id());
        }
        if (b1->succ_ft) {
          if (debug_asm_branch) {
            lg::debug("  b1 succ_ft is {}", b1->succ_ft->to_string());
          }
          ASSERT(b1->succ_ft->has_pred(b1));
        }
        replaced = true;
        m_blocks.at(b0->succ_branch->get_first_block_id())->needs_label = true;
        auto* seq = dynamic_cast<SequenceVtx*>(b0);
        ASSERT(seq);

        seq->seq.push_back(b1);

        if (b0->succ_branch) {
          if (debug_asm_branch) {
            lg::debug("succ {} has {} preds parent: {}", b0->succ_branch->get_first_block_id(),
                      b0->succ_branch->pred.size(), !!b0->succ_branch->parent);
          }
          b0->succ_branch->replace_preds_with_and_check({b0}, nullptr);
          if (debug_asm_branch) {
            lg::debug("OKOK");
          }
        }

        for (auto* new_succ : b1->succs()) {
          if (debug_asm_branch) {
            lg::debug("fixing up succ {}", new_succ->to_string());
          }
          new_succ->replace_pred_and_check(b1, b0);
        }

        if (b1->succ_ft) {
          ASSERT(b1->succ_ft->has_pred(b0));
        }

        if (b1->succ_ft) {
          ASSERT(b1->succ_ft->has_pred(b0));
        }

        // try

        seq->succ_ft = b1->succ_ft;
        seq->succ_branch = b1->succ_branch;
        if (b1->succ_branch) {
          ASSERT(!b1->succ_branch->parent);
        }

        if (seq->succ_branch && debug_asm_branch) {
          lg::debug("  new sb: {}", seq->succ_branch->get_first_block_id());
        }
        seq->next = b1->next;
        if (seq->next) {
          seq->next->prev = seq;
        }

        b1->parent_claim(seq);
        if (seq->succ_branch) {
          ASSERT(!seq->succ_branch->parent);
        }
        seq->end_branch = b1->end_branch;
        return false;
      } else if (b0_seq && b1_seq) {
        if (debug_asm_branch) {
          lg::debug("[combo ss] {} and {}", b0->get_first_block_id(), b1->get_first_block_id());
          lg::debug(" {} and {}", b0->to_string(), b1->to_string());
        }

        //      printf("make seq type 3 %s %s\n", b0->to_string().c_str(), b1->to_string().c_str());
        replaced = true;
        m_blocks.at(b0->succ_branch->get_first_block_id())->needs_label = true;
        auto* seq = dynamic_cast<SequenceVtx*>(b0);
        ASSERT(seq);

        auto* old_seq = dynamic_cast<SequenceVtx*>(b1);
        ASSERT(old_seq);

        if (b0->succ_branch) {
          if (debug_asm_branch) {
            lg::debug("  sbp: {}", !!b0->succ_branch->parent);
            lg::debug("  sb: {}", b0->succ_branch->to_string());
          }
          b0->succ_branch->replace_preds_with_and_check({b0}, nullptr);
        }

        for (auto* x : old_seq->seq) {
          x->parent_claim(seq);
          seq->seq.push_back(x);
        }

        for (auto* x : old_seq->succs()) {
          //        printf("fix preds of %s\n", x->to_string().c_str());
          x->replace_pred_and_check(old_seq, seq);
        }
        seq->succ_branch = old_seq->succ_branch;
        seq->succ_branch = b1->succ_branch;
        if (seq->succ_branch && debug_asm_branch) {
          lg::debug("  DS new sb: {}", seq->succ_branch->get_first_block_id());
        }
        seq->succ_ft = old_seq->succ_ft;
        seq->end_branch = old_seq->end_branch;
        seq->next = old_seq->next;
        if (seq->next) {
          seq->next->prev = seq;
        }

        // todo - proper trash?
        old_seq->parent_claim(seq);

        return false;
      } else if (!b0_seq && b1_seq) {
        replaced = true;
        if (!b0->succ_branch) {
          lg::debug("bad: {}", b0->to_string());
        }
        m_blocks.at(b0->succ_branch->get_first_block_id())->needs_label = true;
        auto* old_seq = dynamic_cast<SequenceVtx*>(b1);
        ASSERT(old_seq);
        if (b0->succ_branch) {
          if (debug_asm_branch) {
            lg::debug("  sbp: {}", !!b0->succ_branch->parent);
            lg::debug("  sb: {}", b0->succ_branch->to_string());
          }
          b0->succ_branch->replace_preds_with_and_check({b0}, nullptr);
        }

        for (auto* p : b0->pred) {
          p->replace_succ_and_check(b0, old_seq);
        }
        old_seq->pred = b0->pred;
        old_seq->prev = b0->prev;
        if (old_seq->prev) {
          old_seq->prev->next = old_seq;
        }

        old_seq->seq.insert(old_seq->seq.begin(), b0);
        b0->parent_claim(old_seq);
      }

      else {
        lg::error("unhandled sequences in clean_up_asm_branches seq: {} {}", !!b0_seq, !!b1_seq);
      }
    }

    return true;  // keep looking
  });
  return replaced;
}

bool ControlFlowGraph::is_sequence_of_non_sequences(CfgVtx* b0, CfgVtx* b1, bool allow_self_loops) {
  if (!b0 || !b1)
    return false;
  if (dynamic_cast<SequenceVtx*>(b0) || dynamic_cast<SequenceVtx*>(b1))
    return false;

  return is_sequence(b0, b1, allow_self_loops);
}

bool ControlFlowGraph::is_sequence_of_sequence_and_non_sequence(CfgVtx* b0,
                                                                CfgVtx* b1,
                                                                bool allow_self_loops) {
  if (!b0 || !b1)
    return false;
  if (!dynamic_cast<SequenceVtx*>(b0))
    return false;
  if (dynamic_cast<SequenceVtx*>(b1))
    return false;
  return is_sequence(b0, b1, allow_self_loops);
}

bool ControlFlowGraph::is_sequence_of_sequence_and_sequence(CfgVtx* b0,
                                                            CfgVtx* b1,
                                                            bool allow_self_loops) {
  if (!b0 || !b1)
    return false;
  if (!dynamic_cast<SequenceVtx*>(b0))
    return false;
  if (!dynamic_cast<SequenceVtx*>(b1))
    return false;
  return is_sequence(b0, b1, allow_self_loops);
}

bool ControlFlowGraph::is_sequence_of_non_sequence_and_sequence(CfgVtx* b0,
                                                                CfgVtx* b1,
                                                                bool allow_self_loops) {
  if (!b0 || !b1) {
    return false;
  }

  if (dynamic_cast<SequenceVtx*>(b0))
    return false;
  if (!dynamic_cast<SequenceVtx*>(b1))
    return false;
  return is_sequence(b0, b1, allow_self_loops);
}

/*!
 * Find and insert at most one sequence. Return true if sequence is inserted.
 * To generate more readable debug output, we should aim to run this as infrequent and as
 * late as possible, to avoid condition vertices with tons of extra junk packed in.
 */
bool ControlFlowGraph::find_seq_top_level(bool allow_self_loops) {
  bool replaced = false;
  for_each_top_level_vtx([&](CfgVtx* vtx) {
    auto* b0 = vtx;
    auto* b1 = vtx->next;

    //    if (b1 && b1->end_branch.asm_branch) {
    //      return true;
    //    }

    if (is_sequence_of_non_sequences(b0, b1, allow_self_loops)) {  // todo, avoid nesting sequences.
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

    if (is_sequence_of_sequence_and_non_sequence(b0, b1, allow_self_loops)) {
      //      printf("make seq type 2 %s %s\n", b0->to_string().c_str(), b1->to_string().c_str());
      replaced = true;
      auto* seq = dynamic_cast<SequenceVtx*>(b0);
      ASSERT(seq);

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

    if (is_sequence_of_non_sequence_and_sequence(b0, b1, allow_self_loops)) {
      replaced = true;
      auto* seq = dynamic_cast<SequenceVtx*>(b1);
      ASSERT(seq);
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

    if (is_sequence_of_sequence_and_sequence(b0, b1, allow_self_loops)) {
      //      printf("make seq type 3 %s %s\n", b0->to_string().c_str(), b1->to_string().c_str());
      replaced = true;
      auto* seq = dynamic_cast<SequenceVtx*>(b0);
      ASSERT(seq);

      auto* old_seq = dynamic_cast<SequenceVtx*>(b1);
      ASSERT(old_seq);

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

bool ControlFlowGraph::find_cond_w_else(const CondWithElseLengthHack& hack) {
  bool found = false;

  for_each_top_level_vtx([&](CfgVtx* vtx) {
    // determine where the "else" block would be
    auto* c0 = vtx;       // first condition
    auto* b0 = c0->next;  // first body
    if (!b0) {
      return true;
    }

    if (b0->end_branch.asm_branch) {
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

    ASSERT(b0->end_branch.has_branch);
    ASSERT(b0->end_branch.branch_always);
    ASSERT(b0->succ_branch);

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
      ;
      return true;
    }

    if (else_block->succ_branch) {
      return true;
    }

    if (else_block->succ_ft != end_block) {
      return true;
    }
    ASSERT(!else_block->end_branch.has_branch);

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

    auto hack_lookup = hack.max_length_by_start_block.find(c0->to_form().print());
    if (hack_lookup != hack.max_length_by_start_block.end()) {
      if ((int)entries.size() > hack_lookup->second) {
        return true;
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

bool ControlFlowGraph::find_cond_w_empty_else() {
  bool found = false;

  for_each_top_level_vtx([&](CfgVtx* vtx) {
    // determine where the "else" block would be
    auto* c0 = vtx;       // first condition
    auto* b0 = c0->next;  // first body
    if (!b0) {
      return true;
    }

    // printf("cwe try %s %s\n", c0->to_string().c_str(), b0->to_string().c_str());

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

    ASSERT(b0->end_branch.has_branch);
    ASSERT(b0->end_branch.branch_always);
    ASSERT(b0->succ_branch);

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

    if (else_block != b0) {
      return true;
    } else {
      else_block = end_block;
    }

    // the else code is empty, so the else block is the same as the end block.
    // in this empty else case, we don't care what the "else" block end conditions are.
    // else must fall through to end with no possible branch.
    // it's not our problem to deal with the end block's branching.;

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
    // to_replace.push_back(else_block);
    to_replace.push_back(entries.back().condition);
    for (const auto& x : entries) {
      to_replace.push_back(x.body);
    }
    end_block->replace_preds_with_and_check(to_replace, new_cwe);
    new_cwe->succ_ft = end_block;
    new_cwe->next = end_block;
    end_block->prev = new_cwe;

    // new_cwe->else_vtx = else_block;
    new_cwe->else_vtx = alloc<EmptyVtx>();
    new_cwe->entries = std::move(entries);

    new_cwe->else_vtx->parent_claim(new_cwe);
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

    if (b0->end_branch.asm_branch) {
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
      ASSERT(b0->end_branch.has_branch);
      ASSERT(b0->end_branch.branch_always);
      ASSERT(b0->succ_branch);
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

        if (prev_condition->end_branch.asm_branch) {
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
    std::vector<ShortCircuit::Entry> entries;
    if (!vtx->next) {
      return true;
    }

    if (vtx->end_branch.asm_branch) {
      return true;
    }

    // set up the first entry:
    ShortCircuit::Entry candidate = {vtx, vtx->next};
    CfgVtx* end = vtx->next->succ_branch;

    //    lg::print("Starting loop\n");

    while (true) {
      // check candidate:
      if (!candidate.condition || !candidate.likely_delay || !end) {
        //        lg::print("reject begin {} {} {}\n", !!candidate.condition,
        //        !!candidate.likely_delay,
        //                   !!end);
        return true;
      }

      //      lg::print(" try {} and {} end {}\n", candidate.condition->to_string(),
      //                 candidate.likely_delay->to_string(), end->to_string());

      if (candidate.condition->next == end) {
        if (!entries.empty()) {
          if (candidate.condition->end_branch.has_branch) {
            // should fall through to the end.
            return true;
          }
          candidate.likely_delay = nullptr;
          entries.push_back(candidate);

          //          lg::print("all done!");
          break;
        }
      }

      if (!candidate.condition->next || !candidate.condition->succ_branch) {
        //        lg::print(" fail 0 {}, {}\n", !!candidate.condition->next,
        //                   !!candidate.condition->succ_branch);
        return true;
      }

      // root -> slot
      if (!candidate.condition->next ||
          candidate.condition->next != candidate.condition->succ_branch ||
          !candidate.condition->end_branch.branch_likely ||
          candidate.condition->end_branch.kind != CfgVtx::DelaySlotKind::NO_DELAY) {
        //        lg::print("  fail 1 {} {} {} {}\n", !candidate.condition->next,
        //                   candidate.condition->next != candidate.condition->succ_branch,
        //                   !candidate.condition->end_branch.branch_likely,
        //                   candidate.condition->end_branch.kind !=
        //                   CfgVtx::DelaySlotKind::NO_DELAY);
        //        lg::print(" fail 1 condition->next {}, condition->succ_branch {}\n",
        //                   candidate.condition->next->to_string(),
        //                   candidate.condition->succ_branch->to_string());
        return true;
      }

      if (entries.empty() && candidate.likely_delay->next == end) {
        entries.push_back(candidate);

        //        lg::print("all don2!");
        break;
      }

      if (candidate.likely_delay->pred.size() != 1 || candidate.likely_delay->succ_ft ||
          !candidate.likely_delay->succ_branch || !candidate.likely_delay->end_branch.has_branch ||
          !candidate.likely_delay->end_branch.branch_always ||
          candidate.likely_delay->end_branch.branch_likely ||
          candidate.likely_delay->end_branch.kind != CfgVtx::DelaySlotKind::NO_DELAY) {
        //        lg::print("  fail 2 {} {} {} {} {} {} {}\n", candidate.likely_delay->pred.size()
        //        != 1,
        //                   !!candidate.likely_delay->succ_ft,
        //                   !candidate.likely_delay->succ_branch,
        //                   !candidate.likely_delay->end_branch.has_branch,
        //                   !candidate.likely_delay->end_branch.branch_always,
        //                   !!candidate.likely_delay->end_branch.branch_likely,
        //                   candidate.likely_delay->end_branch.kind !=
        //                   CfgVtx::DelaySlotKind::NO_DELAY);
        //        lg::print("delay {} has ft {}\n", candidate.likely_delay->to_string(),
        //                   candidate.likely_delay->succ_ft->to_string());
        return true;
      }

      // slot -> end
      if (candidate.likely_delay->succ_branch != end) {
        //        lg::print("  fail 3\n");
        return true;
      }

      // root -> next root
      if (!candidate.condition->next->next ||
          candidate.condition->next->next != candidate.condition->succ_ft) {
        //        lg::print("  fail 4\n");
        return true;
      }

      // add candidate:
      entries.push_back(candidate);
      auto next_root = candidate.condition->next->next;
      auto next_slot = next_root->next;
      candidate = {next_root, next_slot};

      // pre next root check
      if (next_root->pred.size() != 1) {
        //        lg::print("  fail 5\n");
        return true;
      }
      //      lg::print("on to next!\n");
    }

    auto new_sc = alloc<ShortCircuit>();

    for (auto* npred : vtx->pred) {
      npred->replace_succ_and_check(vtx, new_sc);
    }
    new_sc->pred = vtx->pred;
    new_sc->prev = vtx->prev;
    if (new_sc->prev) {
      new_sc->prev->next = new_sc;
    }

    std::vector<CfgVtx*> end_preds;
    for (auto& e : entries) {
      if (e.likely_delay) {
        end_preds.push_back(e.likely_delay);
      } else {
        end_preds.push_back(e.condition);
      }
    }

    if (entries.size() == 1) {
      end_preds.push_back(entries.front().condition);
    }

    end->replace_preds_with_and_check(end_preds, new_sc);
    new_sc->succ_ft = end;
    new_sc->next = end;
    end->prev = new_sc;
    new_sc->entries = std::move(entries);
    for (auto& x : new_sc->entries) {
      x.condition->parent_claim(new_sc);
      if (x.likely_delay) {
        x.likely_delay->parent_claim(new_sc);
      }
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
  ASSERT(m_blocks.empty());
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
  ASSERT(!first->succ_ft);  // don't want to overwrite something by accident.
  // can only fall through to the next code in memory.
  ASSERT(first->next == second);
  ASSERT(second->prev == first);
  first->succ_ft = second;
  ASSERT(blocks.at(first->block_id).succ_ft == -1);
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
  ASSERT(!first->succ_branch);
  first->succ_branch = second;
  ASSERT(blocks.at(first->block_id).succ_branch == -1);
  blocks.at(first->block_id).succ_branch = second->block_id;

  if (!second->has_pred(first)) {
    // see comment in link_fall_through
    second->pred.push_back(first);
    blocks.at(second->block_id).pred.push_back(first->block_id);
  }
}

void ControlFlowGraph::link_fall_through_likely(BlockVtx* first,
                                                BlockVtx* second,
                                                std::vector<BasicBlock>& blocks) {
  ASSERT(!first->succ_ft);  // don't want to overwrite something by accident.
  // can only fall through to the next code in memory.
  ASSERT(first->next->next == second);
  ASSERT(second->prev->prev == first);
  first->succ_ft = second;
  ASSERT(blocks.at(first->block_id).succ_ft == -1);
  blocks.at(first->block_id).succ_ft = second->block_id;

  if (!second->has_pred(first)) {
    // if a block can block fall through and branch to the same block, we want to avoid adding
    // it as a pred twice. This is rare, but does happen and makes sense with likely branches
    // which only run the delay slot when taken.
    second->pred.push_back(first);
    blocks.at(second->block_id).pred.push_back(first->block_id);
  }
}

void ControlFlowGraph::flag_early_exit(const std::vector<BasicBlock>& blocks) {
  auto* b = m_blocks.back();
  const auto& block = blocks.at(b->block_id);

  if (block.start_word == block.end_word) {
    b->is_early_exit_block = true;
    ASSERT(!b->end_branch.has_branch);
  }
}

CfgVtx::DelaySlotKind get_delay_slot(const Instruction& i, GameVersion version) {
  if (is_nop(i)) {
    return CfgVtx::DelaySlotKind::NOP;
  } else if (is_gpr_3(i, InstructionKind::OR, {}, Register(Reg::GPR, Reg::S7),
                      Register(Reg::GPR, Reg::R0))) {
    return CfgVtx::DelaySlotKind::SET_REG_FALSE;
  } else if (is_gpr_2_imm_int(i, InstructionKind::DADDIU, {}, Register(Reg::GPR, Reg::S7),
                              true_symbol_offset(version))) {
    return CfgVtx::DelaySlotKind::SET_REG_TRUE;
  } else {
    return CfgVtx::DelaySlotKind::OTHER;
  }
}

namespace {
/*!
 * Is this instruction possible in the delay slot, without using inline assembly?
 */
bool branch_delay_asm(const Instruction& i, GameVersion version) {
  if (is_nop(i)) {
    // nop can be used as a delay
    return false;
  } else if (is_gpr_3(i, InstructionKind::OR, {}, Register(Reg::GPR, Reg::S7),
                      Register(Reg::GPR, Reg::R0))) {
    // set false is used in ifs, etc
    return false;
  } else if (is_gpr_2_imm_int(i, InstructionKind::DADDIU, {}, Register(Reg::GPR, Reg::S7),
                              true_symbol_offset(version))) {
    // set true is used in sc
    return false;
  } else if (is_gpr_3(i, InstructionKind::OR, {}, {}, Register(Reg::GPR, Reg::R0))) {
    // set var to var
    return false;
  } else if (is_gpr_3(i, InstructionKind::DSLLV, {}, {}, {})) {
    // shift trick
    return false;
  } else if (is_gpr_3(i, InstructionKind::DSUBU, {}, Register(Reg::GPR, Reg::R0), {})) {
    // abs trick
    return false;
  } else if (i.kind == InstructionKind::LW &&
             (i.get_src(0).is_sym("binteger") || i.get_src(0).is_sym("pair"))) {
    // rtype trick
    return false;
  } else {
    return true;
  }
}
}  // namespace

/*!
 * Build and resolve a Control Flow Graph as much as possible.
 */
std::shared_ptr<ControlFlowGraph> build_cfg(const LinkedObjectFile& file,
                                            int seg,
                                            Function& func,
                                            const CondWithElseLengthHack& cond_with_else_hack,
                                            const std::unordered_set<int>& blocks_ending_in_asm_br,
                                            GameVersion version) {
  // lg::print("START {}\n", func.guessed_name.to_string());
  auto cfg = std::make_shared<ControlFlowGraph>();

  const auto& blocks = cfg->create_blocks(func.basic_blocks.size());

  // add entry block
  cfg->entry()->succ_ft = blocks.front();
  blocks.front()->pred.push_back(cfg->entry());

  // add exit block
  cfg->exit()->pred.push_back(blocks.back());
  blocks.back()->succ_ft = cfg->exit();

  // set up succ / pred
  for (int i = 0; i < int(func.basic_blocks.size()); i++) {
    auto& b = func.basic_blocks[i];
    if (blocks.at(i)->end_branch.branch_always) {
      // already set.
      continue;
    }
    bool not_last = (i + 1) < int(func.basic_blocks.size());

    if (b.end_word == b.start_word) {
      // there's no room for a branch here, fall through to the end
      if (not_last) {
        cfg->link_fall_through(blocks.at(i), blocks.at(i + 1), func.basic_blocks);
      }
    } else {
      // room for at least a likely branch, try that first.
      int likely_branch_idx = b.end_word - 1;
      ASSERT(likely_branch_idx >= b.start_word);
      auto& likely_branch_candidate = func.instructions.at(likely_branch_idx);

      if (is_branch(likely_branch_candidate, true)) {
        // is a likely branch
        blocks.at(i)->end_branch.has_branch = true;
        blocks.at(i)->end_branch.branch_likely = true;
        blocks.at(i)->end_branch.kind = CfgVtx::DelaySlotKind::NO_DELAY;
        bool branch_always = is_always_branch(likely_branch_candidate);

        // need to find block target
        int block_target = -1;
        int label_target = likely_branch_candidate.get_label_target();
        ASSERT(label_target != -1);
        const auto& label = file.labels.at(label_target);
        ASSERT(label.target_segment == seg);
        ASSERT((label.offset % 4) == 0);
        int offset = label.offset / 4 - func.start_word;
        ASSERT(offset >= 0);
        for (int j = int(func.basic_blocks.size()); j-- > 0;) {
          if (func.basic_blocks[j].start_word == offset) {
            block_target = j;
            break;
          }
        }

        ASSERT(block_target != -1);
        // branch to delay slot
        cfg->link_branch(blocks.at(i), blocks.at(i + 1), func.basic_blocks);

        if (branch_always) {
          // don't continue to the next one
          blocks.at(i)->end_branch.branch_always = true;
        } else {
          // not an always branch
          if (not_last) {
            // don't take the delay slot.
            cfg->link_fall_through_likely(blocks.at(i), blocks.at(i + 2), func.basic_blocks);
          }
        }

        auto& delay_block = blocks.at(i + 1);
        delay_block->end_branch.branch_likely = false;
        delay_block->end_branch.branch_always = true;
        delay_block->end_branch.has_branch = true;
        delay_block->end_branch.kind = CfgVtx::DelaySlotKind::NO_DELAY;
        cfg->link_branch(blocks.at(i + 1), blocks.at(block_target), func.basic_blocks);
        //        printf("SC block target is %d\n", block_target);
      } else {
        if (b.end_word - b.start_word < 2) {
          // no room for a branch, just fall through
          if (not_last) {
            cfg->link_fall_through(blocks.at(i), blocks.at(i + 1), func.basic_blocks);
          }
        } else {
          // try as a normal branch.
          int idx = b.end_word - 2;
          ASSERT(idx >= b.start_word);
          auto& branch_candidate = func.instructions.at(idx);
          auto& delay_slot_candidate = func.instructions.at(idx + 1);
          if (is_branch(branch_candidate, false)) {
            blocks.at(i)->end_branch.has_branch = true;
            blocks.at(i)->end_branch.branch_likely = false;
            blocks.at(i)->end_branch.kind = get_delay_slot(delay_slot_candidate, version);
            bool branch_always = is_always_branch(branch_candidate);

            // need to find block target
            int block_target = -1;
            int label_target = branch_candidate.get_label_target();
            ASSERT(label_target != -1);
            const auto& label = file.labels.at(label_target);
            ASSERT(label.target_segment == seg);
            ASSERT((label.offset % 4) == 0);
            int offset = label.offset / 4 - func.start_word;
            ASSERT(offset >= 0);

            // the order here matters when there are zero size blocks. Unclear what the best answer
            // is.
            //  i think in end it doesn't actually matter??
            //        for (int j = 0; j < int(func.basic_blocks.size()); j++) {
            for (int j = int(func.basic_blocks.size()); j-- > 0;) {
              if (func.basic_blocks[j].start_word == offset) {
                block_target = j;
                break;
              }
            }

            ASSERT(block_target != -1);
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
            // not a branch.
            if (not_last) {
              cfg->link_fall_through(blocks.at(i), blocks.at(i + 1), func.basic_blocks);
            }
          }
        }
      }
    }
  }

  for (int i = 0; i < int(func.basic_blocks.size()); i++) {
    auto& bb = func.basic_blocks[i];
    auto& b = blocks.at(i);

    if (bb.end_word == bb.start_word) {
      continue;  // zero sized block, there is no branch here.
    }

    if (blocks_ending_in_asm_br.find(i) != blocks_ending_in_asm_br.end()) {
      b->end_branch.asm_branch = true;
      if (debug_asm_branch) {
        lg::debug("OVERRIDE asm branch at block {}", i);
      }
      continue;
    }

    // room for at least a likely branch, try that first.
    int likely_branch_idx = bb.end_word - 1;
    ASSERT(likely_branch_idx >= bb.start_word);
    auto& likely_branch_candidate = func.instructions.at(likely_branch_idx);

    if (is_branch(likely_branch_candidate, true)) {
      // likely branch!
      auto following = func.instructions.at(likely_branch_idx + 1);
      if (branch_delay_asm(following, version)) {
        b->end_branch.asm_branch = true;
        if (debug_asm_branch) {
          lg::debug("LIKELY ASM BRANCH: {} and {}", likely_branch_candidate.to_string(file.labels),
                    following.to_string(file.labels));
        }
      }
    }

    if (bb.end_word - bb.start_word >= 2) {
      int idx = bb.end_word - 2;
      ASSERT(idx >= bb.start_word);
      auto& branch_candidate = func.instructions.at(idx);
      auto& delay_slot_candidate = func.instructions.at(idx + 1);
      if (is_branch(branch_candidate, false)) {
        if (branch_delay_asm(delay_slot_candidate, version)) {
          b->end_branch.asm_branch = true;
          if (debug_asm_branch) {
            lg::debug("NORMAL ASM BRANCH: {} and {}", branch_candidate.to_string(file.labels),
                      delay_slot_candidate.to_string(file.labels));
          }
        }
      }
    }
  }

  cfg->flag_early_exit(func.basic_blocks);

  bool changed = true;
  bool complained_about_weird_gotos = false;
  while (changed) {
    changed = false;
    // note - we should prioritize finding short-circuiting expressions.
    //        printf("%s\n", cfg->to_dot().c_str());
    //    printf("%s\n", cfg->to_form().print().c_str());

    // todo - should we lower the priority of the conds?

    changed = changed || cfg->find_cond_w_else(cond_with_else_hack);

    changed = changed || cfg->find_while_loop_top_level();
    changed = changed || cfg->find_seq_top_level(false);
    changed = changed || cfg->find_short_circuits();
    changed = changed || cfg->find_cond_n_else();

    if (!changed) {
      changed = changed || cfg->find_goto_end();
      changed = changed || cfg->find_until_loop();
      changed = changed || cfg->find_until1_loop();
      changed = changed || cfg->find_infinite_loop();
    };

    if (!changed) {
      changed = changed || cfg->find_seq_top_level(true);
    }

    if (!changed) {
      changed = changed || cfg->find_cond_w_empty_else();
    }

    if (!changed) {
      changed = changed || cfg->find_goto_not_end();
    }

    if (!changed) {
      changed = changed || cfg->clean_up_asm_branches();
    }

    if (!changed) {
      changed = changed || cfg->find_infinite_continue();
      if (changed && !complained_about_weird_gotos) {
        complained_about_weird_gotos = true;
        func.warnings.warning(
            "Found some very strange gotos. Check result carefully, this is not well tested.");
      }
    }
  }

  if (!cfg->is_fully_resolved()) {
    func.warnings.error("CFG building failed: Could not fully resolve CFG");
  }

  return cfg;
}
}  // namespace decompiler
