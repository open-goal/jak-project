#pragma once

#include <unordered_map>
#include <vector>
#include <string>
#include "decompiler/Disasm/Instruction.h"
#include "common/util/Range.h"
#include "decompiler/util/StackSpillMap.h"

namespace decompiler {

/*!
 * Given the instructions for a function, build a StackSpillMap containing all memory used to
 * spill register variables. The range should be the non-prologue/non-epilogue instruction range.
 */
StackSpillMap build_spill_map(const std::vector<Instruction>& instructions, Range<int> range);

}  // namespace decompiler