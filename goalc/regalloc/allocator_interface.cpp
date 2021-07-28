/*!
 * @file allocate.cpp
 * Runs the register allocator.
 */

#include "third-party/fmt/core.h"
#include "allocator_interface.h"
#include "Allocator.h"

/*!
 * Print out the input data for debugging.
 */
void print_allocate_input(const AllocationInput& in) {
  fmt::print("[RegAlloc] Debug Input Program:\n");
  if (in.instructions.size() == in.debug_instruction_names.size()) {
    for (size_t i = 0; i < in.instructions.size(); i++) {
      //      fmt::print(" [{}] {} -> {}\n", in.debug_instruction_names.at(i),
      //                 in.instructions.at(i).print());
      fmt::print(" [{:3d}] {:30} -> {:30}\n", i, in.debug_instruction_names.at(i),
                 in.instructions.at(i).print());
    }
  } else {
    for (const auto& instruction : in.instructions) {
      fmt::print(" [{:3d}] {}\n", instruction.print());
    }
  }
  fmt::print("[RegAlloc] Debug Input Constraints:\n");
  for (const auto& c : in.constraints) {
    fmt::print(" {}\n", c.to_string());
  }

  fmt::print("\n");
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
    printf("[%03d] %30s | %30s | %30s\n", i, code_str.c_str(), lives.c_str(),
           result.stack_ops.at(i).print().c_str());
  }
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