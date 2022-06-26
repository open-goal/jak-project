#pragma once

#include <memory>
#include <string>
#include <vector>

#include "common/util/Assert.h"

#include "decompiler/config.h"

namespace goos {
class Object;
}

namespace decompiler {
/*!
 * In v, find an item equal to old, and replace it with replace.
 * Will throw an error is there is not exactly one thing equal to old.
 */
template <typename T>
void replace_exactly_one_in(std::vector<T>& v, T old, T replace) {
  bool replaced = false;
  for (auto& x : v) {
    if (x == old) {
      ASSERT(!replaced);
      x = replace;
      replaced = true;
    }
  }
  ASSERT(replaced);
}

/*!
 * Representation of a vertex in the control flow graph.
 *
 * The desired state of the control flow graph is to have a single "top-level" node, with NULL as
 * its parent. This top level node can then be viewed as the entire control flow for the function.
 * When the graph is fully understood, the only relation between vertices should be parent-child.
 * For example, an "if_else" vertex will have a "condition" vertex, "true_case" vertex, and "false
 * case" vertex as children.
 *
 * However, the initial state of the CFG is to have all the vertices be in the top level. When there
 * are multiple top level vertices, the graph is considered to be "unresolved", as there are
 * relations between these that are not explained by parent-child control structuring. These
 * relations are either pred/succ, indicating program control flow, and next/prev indicating code
 * layout order.  These are undesirable because these do not map to high-level program structure.
 *
 * The graph attempts to "resolve" itself, meaning these pred/succ relations are destroyed and
 * replaced with nested control flow. The pred/succ and next/prev relations should only exist at the
 * top level.
 *
 * Once resolved, there will be a single "top level" node containing the entire control flow
 * structure.
 *
 * All CfgVtxs should be created from the ControlFlowGraph::alloc function, which allocates them
 * from a pool and cleans them up when the ControlFlowGraph is destroyed.  This approach avoids
 * circular reference issues from a referencing counting approach, but does mean that temporary
 * allocations aren't cleaned up until the entire graph is deleted, but this is probably fine.
 *
 * Note - there are two special "top-level" vertices that are always present, called Entry and Exit.
 * These always exist and don't count toward making the graph unresolved.
 * These vertices won't be counted in the get_top_level_vertices_count.
 *
 * Desired end state of the graph:
 *       Entry -> some-top-level-control-flow-structure -> Exit
 */
class CfgVtx {
 public:
  virtual std::string to_string() const = 0;  // convert to a single line string for debugging
  virtual goos::Object to_form() const = 0;   // recursive print as LISP form.
  virtual int get_first_block_id() const = 0;
  virtual ~CfgVtx() = default;

  CfgVtx* parent = nullptr;       // parent structure, or nullptr if top level
  CfgVtx* succ_branch = nullptr;  // possible successor from branching, or NULL if no branch
  CfgVtx* succ_ft = nullptr;      // possible successor from falling through, or NULL if impossible
  CfgVtx* next = nullptr;         // next code in memory
  CfgVtx* prev = nullptr;         // previous code in memory
  std::vector<CfgVtx*> pred;      // all vertices which have us as succ_branch or succ_ft
  int uid = -1;

  bool needs_label = false;

  enum class DelaySlotKind { NO_BRANCH, SET_REG_FALSE, SET_REG_TRUE, NOP, OTHER, NO_DELAY };

  struct {
    bool has_branch = false;     // does the block end in a branch (any kind)?
    bool branch_likely = false;  // does the block end in a likely branch?
    bool branch_always = false;  // does the branch always get taken?
    bool asm_branch = false;     // is this an inline assembly branch?
    DelaySlotKind kind = DelaySlotKind::NO_BRANCH;
  } end_branch;

  // each child class of CfgVtx will define its own children.

  /*!
   * Do we have s as a successor?
   */
  bool has_succ(CfgVtx* s) const { return succ_branch == s || succ_ft == s; }

  /*!
   * Do we have p as a predecessor?
   */
  bool has_pred(CfgVtx* p) const {
    for (auto* x : pred) {
      if (x == p)
        return true;
    }
    return false;
  }

  /*!
   * Lazy function for getting all non-null succesors
   */
  std::vector<CfgVtx*> succs() const {
    std::vector<CfgVtx*> result;
    if (succ_branch) {
      result.push_back(succ_branch);
    }
    if (succ_ft && succ_ft != succ_branch) {
      result.push_back(succ_ft);
    }
    return result;
  }

  void parent_claim(CfgVtx* new_parent);
  void replace_pred_and_check(CfgVtx* old_pred, CfgVtx* new_pred);
  void replace_succ_and_check(CfgVtx* old_succ, CfgVtx* new_succ);
  void replace_preds_with_and_check(std::vector<CfgVtx*> old_preds, CfgVtx* new_pred);

  void remove_pred(CfgVtx* to_remove);

  std::string links_to_string();
};

/*!
 * Special Entry vertex representing the beginning of the function
 */
class EntryVtx : public CfgVtx {
 public:
  EntryVtx() = default;
  goos::Object to_form() const override;
  std::string to_string() const override;
  int get_first_block_id() const override;
};

/*!
 * Special Exit vertex representing the end of the function
 */
class ExitVtx : public CfgVtx {
 public:
  std::string to_string() const override;
  goos::Object to_form() const override;
  int get_first_block_id() const override;
};

/*!
 * A vertex which represents a single basic block. It has no children.
 */
class BlockVtx : public CfgVtx {
 public:
  explicit BlockVtx(int id) : block_id(id) {}
  std::string to_string() const override;
  goos::Object to_form() const override;
  int get_first_block_id() const override;
  int block_id = -1;                 // which block are we?
  bool is_early_exit_block = false;  // are we an empty block at the end for early exits to jump to?
};

/*!
 * A vertex representing a sequence of child vertices which are always represented in order.
 * Child vertices in here don't set their next/prev pred/succ pointers as this counts as resolved.
 */
class SequenceVtx : public CfgVtx {
 public:
  std::string to_string() const override;
  goos::Object to_form() const override;
  int get_first_block_id() const override;
  std::vector<CfgVtx*> seq;
};

/*!
 * Representing a (cond ((a b) (c d) ... (else z))) structure.
 * Note that the first condition ("a" in the above example) may "steal" instructions belonging
 * to an outer scope and these may eventually need to be "unstolen"
 */
class CondWithElse : public CfgVtx {
 public:
  std::string to_string() const override;
  goos::Object to_form() const override;
  int get_first_block_id() const override;

  struct Entry {
    Entry() = default;
    Entry(CfgVtx* _c, CfgVtx* _b) : condition(_c), body(_b) {}
    CfgVtx* condition = nullptr;
    CfgVtx* body = nullptr;
  };

  std::vector<Entry> entries;
  CfgVtx* else_vtx = nullptr;
};

/*!
 * Representing a (cond ((a b) (c d) ... )) structure.
 * Note that the first condition ("a" in the above example) may "steal" instructions belonging
 * to an outer scope and these may eventually need to be "unstolen"
 */
class CondNoElse : public CfgVtx {
 public:
  std::string to_string() const override;
  goos::Object to_form() const override;
  int get_first_block_id() const override;

  struct Entry {
    Entry() = default;
    Entry(CfgVtx* _c, CfgVtx* _b) : condition(_c), body(_b) {}
    CfgVtx* condition = nullptr;
    CfgVtx* body = nullptr;
  };

  std::vector<Entry> entries;
};

class WhileLoop : public CfgVtx {
 public:
  std::string to_string() const override;
  goos::Object to_form() const override;
  int get_first_block_id() const override;

  CfgVtx* condition = nullptr;
  CfgVtx* body = nullptr;
};

class UntilLoop : public CfgVtx {
 public:
  std::string to_string() const override;
  goos::Object to_form() const override;
  int get_first_block_id() const override;

  CfgVtx* condition = nullptr;
  CfgVtx* body = nullptr;
};

class UntilLoop_single : public CfgVtx {
 public:
  std::string to_string() const override;
  goos::Object to_form() const override;
  int get_first_block_id() const override;

  CfgVtx* block = nullptr;
};

class ShortCircuit : public CfgVtx {
 public:
  std::string to_string() const override;
  goos::Object to_form() const override;
  int get_first_block_id() const override;
  struct Entry {
    CfgVtx* condition = nullptr;
    CfgVtx* likely_delay = nullptr;  // will be nullptr on last case
  };
  std::vector<Entry> entries;
};

class InfiniteLoopBlock : public CfgVtx {
 public:
  std::string to_string() const override;
  goos::Object to_form() const override;
  int get_first_block_id() const override;
  CfgVtx* block;
};

class GotoEnd : public CfgVtx {
 public:
  std::string to_string() const override;
  goos::Object to_form() const override;
  int get_first_block_id() const override;
  CfgVtx* body = nullptr;
  CfgVtx* unreachable_block = nullptr;
};

class Break : public CfgVtx {
 public:
  std::string to_string() const override;
  goos::Object to_form() const override;
  int get_first_block_id() const override;
  CfgVtx* body = nullptr;
  CfgVtx* unreachable_block = nullptr;
  int dest_block_id = -1;
};

class EmptyVtx : public CfgVtx {
 public:
  std::string to_string() const override;
  goos::Object to_form() const override;
  int get_first_block_id() const override;
};

struct BasicBlock;

/*!
 * The actual CFG class, which owns all the vertices.
 */
class ControlFlowGraph {
 public:
  ControlFlowGraph();
  ~ControlFlowGraph();

  goos::Object to_form();
  std::string to_form_string();
  std::string to_dot();
  int get_top_level_vertices_count();
  bool is_fully_resolved();
  CfgVtx* get_single_top_level();
  bool contains_break() const { return m_has_break; }

  void flag_early_exit(const std::vector<BasicBlock>& blocks);

  const std::vector<BlockVtx*>& create_blocks(int count);
  void link_fall_through(BlockVtx* first, BlockVtx* second, std::vector<BasicBlock>& blocks);
  void link_fall_through_likely(BlockVtx* first, BlockVtx* second, std::vector<BasicBlock>& blocks);
  void link_branch(BlockVtx* first, BlockVtx* second, std::vector<BasicBlock>& blocks);
  bool find_cond_w_else(const CondWithElseLengthHack& hacks);
  bool find_cond_w_empty_else();
  bool find_cond_n_else();
  bool find_infinite_continue();

  //  bool find_if_else_top_level();
  bool find_seq_top_level(bool allow_self_loops);
  bool find_while_loop_top_level();
  bool find_until_loop();
  bool find_until1_loop();
  bool find_short_circuits();
  bool find_goto_end();
  bool find_infinite_loop();
  bool find_goto_not_end();
  bool clean_up_asm_branches();

  /*!
   * Apply a function f to each top-level vertex.
   * If f returns false, stops.
   */
  template <typename Func>
  void for_each_top_level_vtx(Func f) {
    for (auto* x : m_node_pool) {
      if (!x->parent && x != entry() && x != exit()) {
        if (!f(x)) {
          return;
        }
      }
    }
  }

  EntryVtx* entry() { return m_entry; }
  ExitVtx* exit() { return m_exit; }

  /*!
   * Allocate and construct a node of the specified type.
   */
  template <typename T, class... Args>
  T* alloc(Args&&... args) {
    T* new_obj = new T(std::forward<Args>(args)...);
    m_node_pool.push_back(new_obj);
    new_obj->uid = m_uid++;
    return new_obj;
  }

 private:
  //  bool compact_one_in_top_level();
  //  bool is_if_else(CfgVtx* b0, CfgVtx* b1, CfgVtx* b2, CfgVtx* b3);
  bool is_sequence(CfgVtx* b0, CfgVtx* b1, bool allow_self_loops);
  bool is_sequence_of_non_sequences(CfgVtx* b0, CfgVtx* b1, bool allow_self_loops);
  bool is_sequence_of_sequence_and_non_sequence(CfgVtx* b0, CfgVtx* b1, bool allow_self_loops);
  bool is_sequence_of_sequence_and_sequence(CfgVtx* b0, CfgVtx* b1, bool allow_self_loops);
  bool is_sequence_of_non_sequence_and_sequence(CfgVtx* b0, CfgVtx* b1, bool allow_self_loops);
  bool is_while_loop(CfgVtx* b0, CfgVtx* b1, CfgVtx* b2);
  bool is_until_loop(CfgVtx* b1, CfgVtx* b2);
  bool is_goto_end_and_unreachable(CfgVtx* b0, CfgVtx* b1);
  bool is_goto_not_end_and_unreachable(CfgVtx* b0, CfgVtx* b1);
  bool is_infinite_continue(CfgVtx* b0);
  std::vector<BlockVtx*> m_blocks;   // all block nodes, in order.
  std::vector<CfgVtx*> m_node_pool;  // all nodes allocated
  EntryVtx* m_entry;                 // the entry vertex
  ExitVtx* m_exit;                   // the exit vertex
  int m_uid = 0;
  bool m_has_break = false;
};

class LinkedObjectFile;
class Function;
std::shared_ptr<ControlFlowGraph> build_cfg(const LinkedObjectFile& file,
                                            int seg,
                                            Function& func,
                                            const CondWithElseLengthHack& cond_with_else_hack,
                                            const std::unordered_set<int>& blocks_ending_in_asm_br,
                                            GameVersion version);
}  // namespace decompiler
