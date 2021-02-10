/*!
 * @file variable_naming.h
 * This implements the variable renaming algorithm that splits registers into variables.
 * Note - this doesn't "merge" in cases where a variable lives in multiple registers.
 *   That will be handled at expression building, as those cases are extremely specific.
 *
 * This algorithm has three phases:
 *  1). Convert to Static Single Assignment (SSA) form.
 *  2). Merge variables to eliminate phi functions
 *  3). Perform final variable naming and typing.
 *
 * In the future it may be possible to insert a step between 2 and 3 that merges incorrectly
 * separated variables based on heuristics.
 */

#pragma once

#include <string>
#include <vector>
#include <optional>
#include <unordered_map>
#include <cassert>
#include "decompiler/Disasm/Register.h"
#include "decompiler/util/TP_Type.h"
#include "decompiler/IR2/IR2_common.h"

namespace decompiler {

class Function;
class DecompilerTypeSystem;
struct RegUsageInfo;
struct FunctionAtomicOps;

/*!
 * An SSA variable in the variable analysis pass.  Can be converted into a register again.
 * These must be created from a VarMapSSA, which can then remap and merge these.
 * This remapping/merging functionality is used in the initial conversion to SSA,
 * the simplification of the SSA, and the merging of variables.
 */
class VarSSA {
 public:
  Register reg() const { return m_reg; }

 private:
  friend class VarMapSSA;
  VarSSA(Register reg, int entry_id) : m_reg(reg), m_entry_id(entry_id) {}
  VarSSA() = default;
  Register m_reg;
  int m_entry_id = -1;
};

/*!
 * A map of VarSSA's to ID's.  The ID represents a program variable.
 * The VarSSA represents an SSA variable during the "rough" SSA phase.
 * As the algorithm runs, it reduces the number of program variables my merging.
 *
 * ID's are given out in order per register in the order of allocation.
 * All ID's for normal variables are > 0.
 * Negative/0 ID's correspond to block ending variables (set with remap_to_final_for_block).
 * The ID is -block_id. It is printed as B{ID}.
 * Use merge(var, var) to make two variables have the same ID. A wins, unless B is zero, in which
 * case B wins. This approach is chosen because it
 *   - making A win makes the names match the block for intermediate results
 *   - makes the B0 version of the variable represent the initial value of the variable on function
 *    entry
 */
class VarMapSSA {
 public:
  VarSSA allocate(Register reg);
  VarSSA allocate_init_phi(Register reg, int block_id);
  void merge(const VarSSA& var_a, const VarSSA& var_b);
  void merge_to_first(const VarSSA& var_a, const VarSSA& var_b);
  std::string to_string(const VarSSA& var) const;
  bool same(const VarSSA& var_a, const VarSSA& var_b) const;
  int var_id(const VarSSA& var);
  void remap_reg(Register reg, const std::unordered_map<int, int>& remap);
  void debug_print_map() const;

 private:
  int get_next_var_id(Register reg);

  struct Entry {
    int var_id = -1;
    int entry_id = -1;
    Register reg;
  };

  std::vector<Entry> m_entries;
  std::unordered_map<Register, int, Register::hash> m_reg_next_id;
};

/*!
 * Representation of a program used in the variable renaming algorithm.
 */
struct SSA {
  struct Phi {
    // represents a phi node placed at the top of a block.
    VarSSA dest;
    std::vector<VarSSA> sources;

    explicit Phi(const VarSSA& d) : dest(d) {}
    std::string print(const VarMapSSA& var_map) const;
  };

  struct Ins {
    explicit Ins(int id) : op_id(id) {}
    // represents an instruction.
    std::optional<VarSSA> dst;
    std::vector<VarSSA> src;
    int op_id = -1;
    bool is_arg_coloring_move = false;

    std::string print(const VarMapSSA& var_map) const;
  };

  struct Block {
    std::unordered_map<Register, Phi, Register::hash> phis;  // stored per dest reg.
    std::vector<Ins> ins;

    std::string print(const VarMapSSA& var_map) const;
  };

  explicit SSA(int n_blocks) { blocks.resize(n_blocks); }
  VarMapSSA map;
  std::vector<Block> blocks;

  // in terms of reg, var_id
  std::unordered_map<Register, std::vector<VariableNames::VarInfo>, Register::hash>
      program_read_vars;
  std::unordered_map<Register, std::vector<VariableNames::VarInfo>, Register::hash>
      program_write_vars;

  Phi& get_phi(int block, Register dest_reg);
  VarSSA get_phi_dest(int block, Register dest_reg);
  void add_source_to_phi(int block, Register dest_reg, const VarSSA& src_var);

  bool simplify();
  void merge_all_phis();
  void remap();
  void make_vars(const Function& function, const DecompilerTypeSystem& dts);
  VariableNames get_vars();
  std::string print() const;
};

std::optional<VariableNames> run_variable_renaming(const Function& function,
                                                   const RegUsageInfo& rui,
                                                   const FunctionAtomicOps& ops,
                                                   const DecompilerTypeSystem& dts,
                                                   bool debug_prints = false);

}  // namespace decompiler
