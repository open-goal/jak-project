/*!
 * @file allocate.cpp
 * Runs the register allocator.
 */

#include "allocator_interface.h"

#include <algorithm>

#include "common/log/log.h"

#include "fmt/core.h"
/*!
 * Print out the input data for debugging.
 */
void print_allocate_input(const AllocationInput& in) {
  lg::print("[RegAlloc] Debug Input Program:\n");
  if (in.instructions.size() == in.debug_instruction_names.size()) {
    for (size_t i = 0; i < in.instructions.size(); i++) {
      //      lg::print(" [{}] {} -> {}\n", in.debug_instruction_names.at(i),
      //                 in.instructions.at(i).print());
      lg::print(" [{:3d}] {:30} -> {:30}\n", i, in.debug_instruction_names.at(i),
                in.instructions.at(i).print());
    }
  } else {
    for (const auto& instruction : in.instructions) {
      lg::print(" {}\n", instruction.print());
    }
  }
  lg::print("[RegAlloc] Debug Input Constraints:\n");
  for (const auto& c : in.constraints) {
    lg::print(" {}\n", c.to_string());
  }

  lg::print("\n");
}

/*!
 * Print the result of register allocation for debugging.
 */
void print_result(const AllocationInput& in, const AllocationResult& result) {
  printf("[RegAlloc] result:\n");
  printf("-----------------------------------------------------------------\n");
  for (uint32_t i = 0; i < in.instructions.size(); i++) {
    std::vector<bool> ids_live;
    std::string lives;

    ids_live.resize(in.max_vars, false);

    for (int j = 0; j < in.max_vars; j++) {
      if (result.ass_as_ranges.at(j).is_live_at_instr(i)) {
        lives += std::to_string(j) + " " + result.ass_as_ranges.at(j).get(i).to_string() + "  ";
      }
    }

    std::string code_str;
    if (in.debug_instruction_names.size() == in.instructions.size()) {
      code_str = in.debug_instruction_names.at(i);
    }

    if (code_str.length() >= 50) {
      code_str = code_str.substr(0, 48);
      code_str.push_back('~');
    }
    printf("[%03d] %30s | %30s | %30s\n", (int)i, code_str.c_str(), lives.c_str(),
           result.stack_ops.at(i).print().c_str());
  }
}

std::string Assignment::to_string() const {
  std::string result;
  if (spilled) {
    result += "*";
  }
  switch (kind) {
    case Kind::STACK:
      result += fmt::format("s[{:2d}]", stack_slot);
      break;
    case Kind::REGISTER:
      result += emitter::gRegInfo.get_info(reg).name;
      break;
    case Kind::UNASSIGNED:
      result += "unassigned";
      break;
    default:
      ASSERT(false);
  }

  return result;
}

/*!
 * Find basic blocks and add block link info.
 */
void find_basic_blocks(ControlFlowAnalysisCache* cache, const AllocationInput& in) {
  std::vector<int> dividers;

  dividers.push_back(0);
  dividers.push_back(in.instructions.size());

  // loop over instructions, finding jump targets
  for (uint32_t i = 0; i < in.instructions.size(); i++) {
    const auto& instr = in.instructions[i];
    if (!instr.jumps.empty()) {
      dividers.push_back(i + 1);
      for (auto dest : instr.jumps) {
        dividers.push_back(dest);
      }
    }
  }

  // sort dividers, and make blocks
  std::sort(dividers.begin(), dividers.end(), [](int a, int b) { return a < b; });

  for (uint32_t i = 0; i < dividers.size() - 1; i++) {
    if (dividers[i] != dividers[i + 1]) {
      // new basic block!
      RegAllocBasicBlock block;
      for (int j = dividers[i]; j < dividers[i + 1]; j++) {
        block.instr_idx.push_back(j);
      }

      block.idx = cache->basic_blocks.size();
      cache->basic_blocks.push_back(block);
    }
  }

  if (!cache->basic_blocks.empty()) {
    cache->basic_blocks.front().is_entry = true;
    cache->basic_blocks.back().is_exit = true;
  }

  auto find_basic_block_to_target = [&](int instr) {
    bool found = false;
    uint32_t result = -1;
    for (uint32_t i = 0; i < cache->basic_blocks.size(); i++) {
      if (!cache->basic_blocks[i].instr_idx.empty() &&
          cache->basic_blocks[i].instr_idx.front() == instr) {
        ASSERT(!found);
        found = true;
        result = i;
      }
    }
    if (!found) {
      printf("[RegAlloc Error] couldn't find basic block beginning with instr %d of %d\n", instr,
             int(in.instructions.size()));
    }
    ASSERT(found);
    return result;
  };

  // link blocks
  for (auto& block : cache->basic_blocks) {
    ASSERT(!block.instr_idx.empty());
    auto& last_instr = in.instructions.at(block.instr_idx.back());
    if (last_instr.fallthrough) {
      // try to link to next block:
      int next_idx = block.idx + 1;
      if (next_idx < (int)cache->basic_blocks.size()) {
        cache->basic_blocks.at(next_idx).pred.push_back(block.idx);
        block.succ.push_back(next_idx);
      }
    }
    for (auto target : last_instr.jumps) {
      cache->basic_blocks.at(find_basic_block_to_target(target)).pred.push_back(block.idx);
      block.succ.push_back(find_basic_block_to_target(target));
    }
  }
}

void RegAllocBasicBlock::analyze_liveliness_phase1(const std::vector<RegAllocInstr>& instructions) {
  for (int i = instr_idx.size(); i-- > 0;) {
    auto ii = instr_idx.at(i);
    auto& instr = instructions.at(ii);
    auto& lv = live.at(i);
    auto& dd = dead.at(i);

    // make all read live out
    lv.clear();
    for (auto& x : instr.read) {
      lv.insert(x.id);
    }

    // kill things which are overwritten
    dd.clear();
    for (auto& x : instr.write) {
      if (!lv[x.id]) {
        dd.insert(x.id);
      }
    }

    use.bitwise_and_not(dd);
    use.bitwise_or(lv);

    defs.bitwise_and_not(lv);
    defs.bitwise_or(dd);
  }
}

bool RegAllocBasicBlock::analyze_liveliness_phase2(std::vector<RegAllocBasicBlock>& blocks,
                                                   const std::vector<RegAllocInstr>& instructions) {
  (void)instructions;
  bool changed = false;
  auto out = defs;

  for (auto s : succ) {
    out.bitwise_or(blocks.at(s).input);
  }

  IRegSet in = use;
  IRegSet temp = out;
  temp.bitwise_and_not(defs);
  in.bitwise_or(temp);

  if (in != input || out != output) {
    changed = true;
    input = in;
    output = out;
  }

  return changed;
}

void RegAllocBasicBlock::analyze_liveliness_phase3(std::vector<RegAllocBasicBlock>& blocks,
                                                   const std::vector<RegAllocInstr>& instructions) {
  (void)instructions;
  IRegSet live_local;
  for (auto s : succ) {
    live_local.bitwise_or(blocks.at(s).input);
  }

  for (int i = instr_idx.size(); i-- > 0;) {
    auto& lv = live.at(i);
    auto& dd = dead.at(i);

    IRegSet new_live = live_local;
    new_live.bitwise_and_not(dd);
    new_live.bitwise_or(lv);

    lv = live_local;
    live_local = new_live;
  }
}

std::string RegAllocBasicBlock::print_summary() {
  std::string result = "block " + std::to_string(idx) + "\nsucc: ";
  for (auto s : succ) {
    result += std::to_string(s) + " ";
  }
  result += "\npred: ";
  for (auto p : pred) {
    result += std::to_string(p) + " ";
  }
  result += "\nuse: ";
  for (int x = 0; x < use.size(); x++) {
    if (use[x]) {
      result += std::to_string(x) + " ";
    }
  }
  result += "\ndef: ";
  for (int x = 0; x < defs.size(); x++) {
    if (defs[x]) {
      result += std::to_string(x) + " ";
    }
  }
  result += "\ninput: ";
  for (int x = 0; x < input.size(); x++) {
    if (input[x]) {
      result += std::to_string(x) + " ";
    }
  }
  result += "\noutput: ";
  for (int x = 0; x < output.size(); x++) {
    if (output[x]) {
      result += std::to_string(x) + " ";
    }
  }

  return result;
}

std::string RegAllocBasicBlock::print(const std::vector<RegAllocInstr>& insts) {
  std::string result = print_summary() + "\n";
  int k = 0;
  for (auto instr : instr_idx) {
    std::string line = insts.at(instr).print();
    constexpr int pad_len = 30;
    if (line.length() < pad_len) {
      // line.insert(line.begin(), pad_len - line.length(), ' ');
      line.append(pad_len - line.length(), ' ');
    }

    result += "  " + line + " live: ";
    for (int x = 0; x < live.at(k).size(); x++) {
      if (live.at(k)[x]) {
        result += std::to_string(x) + " ";
      }
    }
    result += "\n";

    k++;
  }
  return result;
}

/*!
 * Print for debugging
 */
std::string RegAllocInstr::print() const {
  bool first = true;
  std::string result = "(";

  if (!write.empty()) {
    first = false;
    result += "(write";
    for (auto& i : write) {
      result += " " + i.to_string();
    }
    result += ")";
  }

  if (!read.empty()) {
    if (!first) {
      result += " ";
    }
    first = false;
    result += "(read";
    for (auto& i : read) {
      result += " " + i.to_string();
    }
    result += ")";
  }

  if (!clobber.empty()) {
    if (!first) {
      result += " ";
    }
    first = false;
    result += "(clobber";
    for (auto& i : clobber) {
      result += " " + i.print();
    }
    result += ")";
  }

  if (!jumps.empty()) {
    if (!first) {
      result += " ";
    }
    first = false;
    result += "(jumps";
    for (auto& i : jumps) {
      result += " " + std::to_string(i);
    }
    result += ")";
  }
  result += ")";
  return result;
}

std::string StackOp::print() const {
  std::string result;
  bool added = false;
  for (const auto& op : ops) {
    if (op.load) {
      result += fmt::format("{} <- [{:2d}], ", emitter::gRegInfo.get_info(op.reg).name, op.slot);
      added = true;
    }
    if (op.store) {
      result += fmt::format("{} -> [{:2d}], ", emitter::gRegInfo.get_info(op.reg).name, op.slot);
      added = true;
    }
  }

  if (added) {
    result.pop_back();
    result.pop_back();
  }

  return result;
}
