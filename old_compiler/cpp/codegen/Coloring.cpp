/*!
 * @file Coloring.cpp
 * High Level Interface for register coloring.
 */

#include "logger/Logger.h"
#include "goal/GoalEnv.h"
#include "regalloc/RegAllocProgram.h"
#include "Coloring.h"

#define LOG(...) gLogger.log(MSG_WARN, __VA_ARGS__)

bool debug_linear_scan = false;

/*!
 * Print each instruction, both as an IR instruction and a RegAllocInstruction.
 */
static void debug_print_register_use(FunctionEnv& f, RegAllocProgram& program) {
  printf("IR Register Use Analysis\n");
  printf("-----------------------------------------------------------------\n");
  for (uint32_t i = 0; i < f.code.size(); i++) {
    printf("[%03d] %30s -> %30s\n", i, f.code.at(i)->print().c_str(),
           program.instructions.at(i).print().c_str());
  }
}

/*!
 * Print the basic blocks and live ranges of a RegAllocProgram and a Function.
 */
static void debug_print_basic_blocks_and_live_ranges(FunctionEnv& f, RegAllocProgram& program) {
  LOG("\nBasic Blocks\n");
  LOG("-----------------------------------------------------------------\n");
  LOG("%s\n", program.print_block_detailed().c_str());

  LOG("\nLive Ranges (no alloc)\n");
  LOG("-----------------------------------------------------------------\n");
  // align to where we start putting live stuff
  LOG("      %30s    ", "");
  for (int i = 0; i < program.max_var; i++) {
    LOG("%2d ", i);
  }
  LOG("\n");
  LOG("_________________________________________________________________\n");
  for (uint32_t i = 0; i < f.code.size(); i++) {
    std::vector<bool> ids_live;
    std::string lives;

    ids_live.resize(program.max_var, false);

    for (int j = 0; j < program.max_var; j++) {
      if (program.live_ranges.at(j).is_live_at_instr(i)) {
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

    std::string code_str = f.code.at(i)->print();
    if (code_str.length() >= 50) {
      code_str = code_str.substr(0, 48);
      code_str.push_back('~');
    }
    LOG("[%03d] %30s -> %s\n", i, code_str.c_str(), lives.c_str());
  }
}

/*!
 * Print the result of coloring.
 */
static void debug_print_coloring(FunctionEnv& f, RegAllocProgram& program) {
  LOG("\nLive Ranges (after alloc)\n");
  LOG("-----------------------------------------------------------------\n");
  for (uint32_t i = 0; i < f.code.size(); i++) {
    std::vector<bool> ids_live;
    std::string lives;

    ids_live.resize(program.max_var, false);

    for (int j = 0; j < program.max_var; j++) {
      if (program.live_ranges.at(j).is_live_at_instr(i)) {
        lives += std::to_string(j) + " " + program.live_ranges.at(j).get(i).print() + "  ";
      }
    }

    std::string code_str = f.code.at(i)->print();
    if (code_str.length() >= 50) {
      code_str = code_str.substr(0, 48);
      code_str.push_back('~');
    }
    LOG("[%03d] %30s | %30s | %30s\n", i, code_str.c_str(), lives.c_str(),
        program.bonus_instructions.at(i).print().c_str());
  }
}

/*!
 * Attempt linear scan coloring algorithm on the given function.  Return true if it succeeds.
 */
bool do_linear_scan_coloring(FunctionEnv& f) {
  // first we translate to a RegAllocProgram
  RegAllocProgram program;
  for (auto& ir : f.code) {
    // convert to instruction
    auto inst = ir->to_rai();
    auto id = program.add_instruction(inst);
    // add more complicated constraints for the IR into the program, if the IR needs it.
    ir->add_constraints_to_program(program.constraints, id);

    // check if we need the RBP register
    if (ir->kind == STATIC_VAR_32 || ir->kind == STATIC_VAR_ADDR || ir->kind == FUNC_ADDR) {
      f.uses_rbp = true;
    }

    // if we have a function call, we should align our stack
    if (ir->kind == FUNCTION_CALL) {
      f.requires_aligned_stack = true;
    }
  }

  // add constraints contained in the function definition too.
  for (auto& c : f.register_constraints) {
    program.constraints.push_back(c);
  }

  // at this point, we can print which instruction reads/writes each register.
  if (debug_linear_scan) {
    debug_print_register_use(f, program);
  }

  // use analysis functions to find basic blocks and register liveliness (including ranges)
  program.find_basic_blocks();
  program.analyze_block_liveliness(f.vars.size());

  // prepare!
  program.prepare_for_allocation(f.code.size());

  // print basic blocks and live ranges
  if (debug_linear_scan) {
    debug_print_basic_blocks_and_live_ranges(f, program);
  }

  // constrained alloc
  program.do_constrained_allocations();
  program.check_constrained_allocations();

  // do other allocs
  program.allocate();

  if (debug_linear_scan) {
    debug_print_coloring(f, program);
  }

  // TODO - final check?

  // check if the coloring needed to use any saved registers
  for (int sr_id = 0; sr_id < SAVED_REG_COUNT; sr_id++) {
    auto sr = SAVED_REGS[sr_id];
    for (auto& lr : program.live_ranges) {
      for (int instr_idx = lr.min; instr_idx < lr.max; instr_idx++) {
        if (lr.get(instr_idx).reg_id == sr) {
          f.uses_saved_reg[sr_id] = true;
        }
      }
    }
    //    for(auto& instr : program.instructions) {
    //    for(size_t i = 0; i < program.instructions.size(); i++) {
    //      auto& instr = program.instructions.at(i);
    //
    //      auto sr_color = f.coloring.at(sr).get(i).reg_id;
    //      if(instr.reads(sr_color) || instr.writes(sr_color)) {
    //        printf("instruction %s uses sr id %d\n", instr.print().c_str(), sr_id);
    //        f.uses_saved_reg[sr_id] = true;
    //        break;
    //      }
    //    }
  }

  if (program.coloring_error) {
    LOG("Coloring was unsuccessful. Please try harder next time.\n");
    return false;
  } else {
    f.coloring = program.live_ranges;
    f.bonus_instructions = program.bonus_instructions;
    f.stack_slots = program.get_stack_slot_count();
    f.coloring_done = true;
    if (program.used_stack && f.is_asm_func) {
      printf("-- WARNING -- asm func %s used the stack!\n", f.name.c_str());
    }
    f.requires_aligned_stack = f.requires_aligned_stack || program.used_stack;
    //    auto move_stats = program.get_move_stats();
    //    auto spill_stats = program.get_spill_count();
    //    printf("%d/%d moves eliminated, %d spill moves\n", move_stats.first, move_stats.second,
    //    spill_stats);
    return true;
  }
}
