#pragma once
#include <vector>
#include "AtomicOp.h"

namespace decompiler {
class Function;
struct BasicBlock;
class LinkedObjectFile;

/*!
 * A collection of Atomic Ops in a function
 */
struct FunctionAtomicOps {
  // the actual ops, store in the correct order
  std::vector<std::unique_ptr<AtomicOp>> ops;

  // mappings from instructions to atomic ops and back
  std::unordered_map<int, int> instruction_to_basic_op;
  std::unordered_map<int, int> atomic_op_to_instruction;

  // map from basic block to the index of the first op
  std::vector<int> block_id_to_first_atomic_op;
  // map from basic block to the index of the last op + 1
  std::vector<int> block_id_to_end_atomic_op;
};

/*!
 * Convert an entire basic block and add the results to a FunctionAtomicOps.
 * Updates the mapping between blocks, instructions, and atomic ops as needed
 * @param begin idx : the index of the first instruction for the block
 * @param begin     : the start of the instructions for the block
 * @param end       : the end of the instructions for the block
 * @param labels    : label names for the function, used for error prints on failed conversions
 * @param container : the container to add to
 */
void convert_block_to_atomic_ops(int begin_idx,
                                 std::vector<Instruction>::const_iterator begin,
                                 std::vector<Instruction>::const_iterator end,
                                 const std::vector<DecompilerLabel>& labels,
                                 FunctionAtomicOps* container);

/*!
 * Convert an entire function to AtomicOps
 */
FunctionAtomicOps convert_function_to_atomic_ops(const Function& func,
                                                 const std::vector<DecompilerLabel>& labels);
}  // namespace decompiler