/*!
 * @file TypeAnalysis.cpp
 * This is the first attempt to do GOAL type analysis.
 *
 * This approach is based purely on registers/basic blocks, and not GOAL variables or expressions.
 * This was chosen because it should (maybe) be more successful at things like
 * (format #t "blah" (if a b c) (and b c))
 * where there is branching in between the load of format and the actual function call.
 *
 * We use IR Basic Ops instead of MIPS instructions to do the type propagation because there are
 * often weird intermediate results in between instructions within the same IR basic op
 * that we don't care about.
 *
 * The basic idea is to "keep propagating types until nothing changes."
 * When there are two ways to get to the same spot, and the types there are different, we take
 * the lowest common ancestor of the types.
 */

#include <set>
#include "Function.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "decompiler/Disasm/InstructionMatching.h"

namespace {
/*!
 * Modify the combined type map to be the lowest common ancestor of combined and add for shared
 * regs. Currently combined will also be updated to contain the union of unshared registers.
 *
 * Returns if combined was changed.
 */
bool lca_tm(TypeMap& combined, const TypeMap& add, DecompilerTypeSystem& dts) {
  bool changed = false;
  for (auto& kv : add) {
    auto existing = combined.find(kv.first);
    if (existing == combined.end()) {
      changed = true;
      combined[kv.first] = kv.second;
    } else {
      auto candidate = dts.ts.lowest_common_ancestor(kv.second, existing->second);
      if (candidate != existing->second) {
        changed = true;
        combined[kv.first] = candidate;
      }
    }
  }
  return changed;
}

/*!
 * Debug print a TypeMap.
 */
void print_tm(const TypeMap& tm) {
  for (int i = 0; i < 32; i++) {
    auto gpr = Register(Reg::RegisterKind::GPR, i);
    auto kv = tm.find(gpr);
    if (kv != tm.end()) {
      fmt::print("{}: {}, ", gpr.to_charp(), kv->second.print());
    }
  }
  fmt::print("\n");
}
}  // namespace

/*!
 * Main Type Analysis Algorithm.
 */
void Function::run_type_analysis(const TypeSpec& my_type,
                                 DecompilerTypeSystem& dts,
                                 LinkedObjectFile& file) {
  if (!has_basic_ops()) {
    fmt::print("run_type_analysis failed because function {} has no basic ops\n",
               guessed_name.to_string());
    return;
  }
  std::vector<TypeMap> typemap_out;
  typemap_out.resize(basic_ops.size());

  // can only run if our type makes sense and has arguments.
  assert(my_type.base_type() == "function");
  assert(my_type.arg_count() > 0);

  int n_args = int(my_type.arg_count()) - 1;
  // auto& return_type = my_type.get_arg(int(my_type.arg_count()) - 1);

  // all types at the entrance of each basic block.
  std::vector<TypeMap> bb_entry_types;
  bb_entry_types.resize(basic_blocks.size());

  // We run the algorithm in rounds. If nothing changes after running a round, we are done.
  // In each round, we only visit each block once.
  // It's not clear if this is the most efficient approach, but it is an easy way to be sure to
  // hit everything.

  // the list of blocks that should be visited in this round.
  std::vector<int> to_visit;

  // this list of blocks we have already visited, and should not visit again until the next round.
  std::set<int> visited;

  // Initialize for the first round.
  // start by visiting the first block
  to_visit.push_back(0);

  // the argument registers for GOAL (todo, common register utils for GOAL)
  std::vector<Register> arg_regs = {make_gpr(Reg::A0), make_gpr(Reg::A1), make_gpr(Reg::A2),
                                    make_gpr(Reg::A3), make_gpr(Reg::T0), make_gpr(Reg::T1),
                                    make_gpr(Reg::T2), make_gpr(Reg::T3)};

  // set up entry types for the first block
  for (int i = 0; i < n_args; i++) {
    bb_entry_types.at(0)[arg_regs.at(i)] = my_type.get_arg(i);
  }
  //  print_tm(bb_entry_types.at(0));

  bool changed = true;  // did we change anything in this round?
  int round = 0;        // what round are we currently running
  while (changed) {
    changed = false;
    fmt::print("--Starting round {}\n", round);
    while (!to_visit.empty()) {
      int block_id = to_visit.back();
      visited.insert(block_id);
      auto& block = basic_blocks.at(block_id);
      to_visit.pop_back();
      fmt::print("-Visit {}\n", block_id);

      TypeMap current_types = bb_entry_types.at(block_id);

      // basic blocks are in terms of instructions, but we want to do our logic on basic ops
      for (int i = block.start_word; i < block.end_word; i++) {
        if (instr_starts_basic_op(i)) {
          auto basic_op = get_basic_op_at_instr(i);
          fmt::print("-Attempt prop on {}\n", basic_op->print(file));
          auto basic_idx = instruction_to_basic_op.at(i);
          typemap_out.at(basic_idx) = current_types;
          if (!basic_op->update_types(typemap_out.at(basic_idx), dts, file)) {
            fmt::print("ERROR: Giving up on type analysis, could not prop types on {}\n",
                       basic_op->print(file));
            return;
          }
          current_types = typemap_out.at(basic_idx);
        }
      }

      // prop to succ blocks
      for (auto succ : {block.succ_branch, block.succ_ft}) {
        if (succ != -1) {
          if (lca_tm(bb_entry_types.at(succ), current_types, dts)) {
            changed = true;  // need another round
            fmt::print("Block {} entry types are now ", succ);
            print_tm(bb_entry_types.at(succ));

            if (visited.find(succ) == visited.end()) {
              to_visit.push_back(succ);
            }
          }
        }
      }
    }

    round++;
  }

  basic_op_typemaps = std::move(typemap_out);
}
