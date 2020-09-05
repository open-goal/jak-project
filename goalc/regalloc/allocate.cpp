/*!
 * @file allocate.cpp
 * Runs the register allocator.
 */

#include "third-party/fmt/core.h"
#include "allocate.h"
#include "Allocator.h"

namespace {
/*!
 * Print out the input data for debugging.
 */
void print_allocate_input(const AllocationInput& in) {
  fmt::print("[RegAlloc] Debug Input:\n");
  if (in.instructions.size() == in.debug_instruction_names.size()) {
    for (size_t i = 0; i < in.instructions.size(); i++) {
      fmt::print(" [{:3d}] {:30} -> {:30}\n", in.debug_instruction_names.at(i),
                 in.instructions.at(i).print());
    }
  } else {
    for (const auto& instruction : in.instructions) {
      fmt::print(" [{:3d}] {}\n", instruction.print());
    }
  }
  for (const auto& c : in.constraints) {
    fmt::print(" {}\n", c.to_string());
  }

  fmt::print("\n");
}

/*!
 * Print out the state of the RegAllocCache after doing analysis.
 */
void print_analysis(const AllocationInput& in, RegAllocCache* cache) {
  fmt::print("[RegAlloc] Basic Blocks\n");
  fmt::print("-----------------------------------------------------------------\n");
  for (auto& b : cache->basic_blocks) {
    fmt::print("{}\n", b.print(in.instructions));
  }

  printf("[RegAlloc] Alive Info\n");
  printf("-----------------------------------------------------------------\n");
  // align to where we start putting live stuff
  printf("      %30s    ", "");
  for (int i = 0; i < cache->max_var; i++) {
    printf("%2d ", i);
  }
  printf("\n");
  printf("_________________________________________________________________\n");
  for (uint32_t i = 0; i < in.instructions.size(); i++) {
    std::vector<bool> ids_live;
    std::string lives;

    ids_live.resize(cache->max_var, false);

    for (int j = 0; j < cache->max_var; j++) {
      if (cache->live_ranges.at(j).is_live_at_instr(i)) {
        ids_live.at(j) = true;
      }
    }

    for (uint32_t j = 0; j < ids_live.size(); j++) {
      if (ids_live[j]) {
        char buff[256];
        sprintf(buff, "%2d ", j);
        lives.append(buff);
      } else {
        lives.append(".. ");
      }
    }

    if (in.debug_instruction_names.size() == in.instructions.size()) {
      std::string code_str = in.debug_instruction_names.at(i);
      if (code_str.length() >= 50) {
        code_str = code_str.substr(0, 48);
        code_str.push_back('~');
      }
      printf("[%03d] %30s -> %s\n", i, code_str.c_str(), lives.c_str());
    } else {
      printf("[%03d] %30s -> %s\n", i, "???", lives.c_str());
    }
  }
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
}  // namespace

/*!
 * The top-level register allocation algorithm!
 */
AllocationResult allocate_registers(const AllocationInput& input) {
  AllocationResult result;
  RegAllocCache cache;

  // if desired, print input for debugging.
  if (input.debug_settings.print_input) {
    print_allocate_input(input);
  }

  // first step is analysis
  find_basic_blocks(&cache, input);
  analyze_liveliness(&cache, input);
  if (input.debug_settings.print_analysis) {
    print_analysis(input, &cache);
  }

  // do constraints first, to get them out of the way
  do_constrained_alloc(&cache, input, input.debug_settings.trace_debug_constraints);
  // the user may have specified invalid constraints, so we should attempt to find conflicts now
  // rather than having the register allocation mysteriously fail later on or silently ignore a
  // constraint.
  if (!check_constrained_alloc(&cache, input)) {
    result.ok = false;
    fmt::print("[RegAlloc Error] Register allocation has failed due to bad constraints.\n");
    return result;
  }

  // do the allocations!
  if (!run_allocator(&cache, input, input.debug_settings.allocate_log_level)) {
    result.ok = false;
    fmt::print("[RegAlloc Error] Register allocation has failed.\n");
    return result;
  }

  // prepare the result
  result.ok = true;
  result.needs_aligned_stack_for_spills = cache.used_stack;
  result.stack_slots = cache.current_stack_slot;

  // copy over the assignment result
  result.assignment.resize(cache.max_var);
  for (size_t i = 0; i < result.assignment.size(); i++) {
    auto& x = result.assignment[i];
    x.resize(input.instructions.size());
    const auto& lr = cache.live_ranges.at(i);
    for (int j = lr.min; j <= lr.max; j++) {
      x.at(j) = lr.get(j);
    }
  }

  // check for use of saved registers
  for (auto sr : emitter::gRegInfo.get_all_saved()) {
    bool uses_sr = false;
    for (auto& lr : cache.live_ranges) {
      for (int instr_idx = lr.min; instr_idx <= lr.max; instr_idx++) {
        if (lr.get(instr_idx).reg == sr) {
          uses_sr = true;
          break;
        }
      }
      if (uses_sr) {
        break;
      }
    }
    if (uses_sr) {
      result.used_saved_regs.push_back(sr);
    }
  }
  result.ass_as_ranges = std::move(cache.live_ranges);
  result.stack_ops = std::move(cache.stack_ops);

  // final result print
  if (input.debug_settings.print_result) {
    print_result(input, result);
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