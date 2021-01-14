#pragma once

#include <string>
#include <vector>
#include <optional>
#include <unordered_map>
#include <cassert>
#include "decompiler/Disasm/Register.h"

namespace decompiler {

class Function;
class RegUsageInfo;
class FunctionAtomicOps;

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
 * A map of VarSSA's to ID's.
 * ID's are given out in order per register in the order of allocation.
 * All ID's for normal variables are > 0.
 * Negative/0 ID's correspond to block ending variables (set with remap_to_final_for_block).
 * The ID is -block_id. It is printed as B{ID}.
 * Use merge(var, var) to make two variables have the same ID. The one with the lower ID wins.
 */
class VarMapSSA {
 public:
  explicit VarMapSSA(int n_blocks);
  VarSSA allocate(Register reg);
  VarSSA allocate_init_phi(Register reg, int block_id);
  void merge(const VarSSA& var_a, const VarSSA& var_b);
  std::string to_string(const VarSSA& var) const;

 private:
  int get_next_var_id(Register reg);

  int m_block_count = 0;

  struct Entry {
    int var_id = -1;
    int entry_id = -1;
    Register reg;
  };

  std::vector<Entry> m_entries;
  std::unordered_map<Register, int, Register::hash> m_reg_next_id;
};

struct SSA {
  struct Phi {
    // represents a phi node
    VarSSA dest;
    std::vector<VarSSA> sources;

    explicit Phi(const VarSSA& d) : dest(d) {}
    std::string print(const VarMapSSA& var_map) const;
  };

  struct Ins {
    // represents an instruction.
    std::optional<VarSSA> dst;
    std::vector<VarSSA> src;

    std::string print(const VarMapSSA& var_map) const;
  };

  struct Block {
    std::unordered_map<Register, Phi, Register::hash> phis;  // stored per dest reg.
    std::vector<Ins> ins;

    std::string print(const VarMapSSA& var_map) const;
  };

  explicit SSA(int n_blocks) : map(n_blocks) { blocks.resize(n_blocks); }
  VarMapSSA map;
  std::vector<Block> blocks;

  Phi& get_phi(int block, Register dest_reg);
  VarSSA get_phi_dest(int block, Register dest_reg);
  void add_phi(int block, Register dest_reg, const VarSSA& src_var);

  std::string print() const;
};

void run_variable_renaming(const Function& function,
                           const RegUsageInfo& rui,
                           const FunctionAtomicOps& ops);

}  // namespace decompiler
