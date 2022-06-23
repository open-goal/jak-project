#include "BasicBlocks.h"

#include <algorithm>

#include "common/util/Assert.h"

#include "decompiler/Disasm/InstructionMatching.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"

namespace decompiler {
/*!
 * Find all basic blocks in a function.
 * All delay slot instructions are grouped with the branch instruction.
 * This is done by finding all "dividers", which are after branch delay instructions and before
 * branch destinations, then sorting them, ignoring duplicates, and creating the blocks.
 */
std::vector<BasicBlock> find_blocks_in_function(const LinkedObjectFile& file,
                                                int seg,
                                                const Function& func) {
  std::vector<BasicBlock> basic_blocks;

  // note - the first word of a function is the "function" type and should go in any basic block
  std::vector<int> dividers = {0, int(func.instructions.size())};

  for (int i = 0; i < int(func.instructions.size()); i++) {
    const auto& instr = func.instructions.at(i);
    const auto& instr_info = instr.get_info();

    if (instr_info.is_branch && !instr_info.is_branch_likely) {
      // make sure the delay slot of this branch is included in the function
      ASSERT(i + func.start_word < func.end_word - 1);
      // divider after delay slot
      dividers.push_back(i + 2);
      auto label_id = instr.get_label_target();
      ASSERT(label_id != -1);
      const auto& label = file.labels.at(label_id);
      // should only jump to within our own function
      ASSERT(label.target_segment == seg);
      ASSERT(label.offset / 4 > func.start_word);
      ASSERT(label.offset / 4 < func.end_word - 1);
      dividers.push_back(label.offset / 4 - func.start_word);
    }

    // for branch likely, we treat the likely instruction as a separate block.
    if (instr_info.is_branch_likely) {
      ASSERT(i + func.start_word < func.end_word - 1);
      // divider after branch instruction
      dividers.push_back(i + 1);
      // divider after likely delay slot.
      dividers.push_back(i + 2);
      // divider at the destination.
      auto label_id = instr.get_label_target();
      ASSERT(label_id != -1);
      const auto& label = file.labels.at(label_id);
      // should only jump to within our own function
      ASSERT(label.target_segment == seg);
      ASSERT(label.offset / 4 > func.start_word);
      ASSERT(label.offset / 4 < func.end_word - 1);
      dividers.push_back(label.offset / 4 - func.start_word);
    }
  }

  std::sort(dividers.begin(), dividers.end());

  for (size_t i = 0; i < dividers.size() - 1; i++) {
    if (dividers[i] != dividers[i + 1]) {
      basic_blocks.emplace_back(dividers[i], dividers[i + 1]);
      ASSERT(dividers[i] >= 0);
    }
  }

  return basic_blocks;
}
}  // namespace decompiler
