/*!
 * @file x86_Emitter.cpp
 * Emitter for converting IR and static objects into GOAL object files for x86.
 */

#include <algorithm>
#include "x86_Emitter.h"
#include "x86.h"
#include "shared_config.h"
#include "IGen.h"
#include "codegen_utils.h"
#include "goal/GoalEnv.h"

/*!
 * Add link data to insert a pointer to the function type at the given offset into the given
 * segment.
 */
void x86_Emitter::link_function_type_ptr(int segment, int offset) {
  function_type_ptr_recs[segment].push_back(offset);
}

/*!
 * Write a CodegenOutput from everything in the emitter.
 */
CodegenOutput x86_Emitter::write() {
  CodegenOutput out;

  // first pass over segments to emit code
  for (int seg = N_SEG; seg-- > 0;) {
    // loop over instructions
    for (auto& i : instructions[seg]) {
      // do alignment/typing if it's the beginning of a function
      if (i.is_function_start) {
        // align function (todo consider aligning more for good cache performance)
        while (out.code[seg].size() & 7)
          out.code[seg].push_back(0);

        // functions should start with a function type tag
        link_function_type_ptr(seg, out.code[seg].size());

        // add padding for that type tag
        for (int j = 0; j < PTR_SIZE; j++) {
          out.code[seg].push_back(0xae);
        }
      }

      // emit instruction
      uint8_t temp[128];
      auto count = i.emit(temp);

      // remember where this instruction is in the output
      out.instr_offsets[seg].push_back(out.code[seg].size());

      // add instruction to output
      for (int j = 0; j < count; j++) {
        out.code[seg].push_back(temp[j]);
      }
    }
  }

  // second pass over segments to emit static objects
  for (int seg = N_SEG; seg-- > 0;) {
    emit_static_objects(out, seg);
  }

  // third pass over segments to fix jumps and emit link tables
  for (int seg = N_SEG; seg-- > 0;) {
    // fix up jumps
    patch_jumps_and_recs(out, out.instr_offsets[seg], seg);

    // link table can now be emitted
    emit_link_table_data(out, out.instr_offsets[seg], seg);
  }

  // now we are all done, we know enough to set up the header
  emit_link_table_header(out);
  return out;
}

/*!
 * Moves static objects from the temporary static object holding vector to the code.
 * Also updates the type_ptr_recs to be relative to the start of the code, not the start of the
 * statics.
 */
void x86_Emitter::emit_static_objects(CodegenOutput& out, int segment) {
  auto& code = out.code.at(segment);
  auto& statics = static_objects.at(segment);

  // 16 byte align the statics section
  while (code.size() & 15) {
    code.push_back(0);
  }

  // remember the start location
  auto static_start = code.size();
  out.static_start[segment] = static_start;

  // apply offset to recs to include the size of the code segment
  for (auto& rec : type_ptr_recs_in_statics.at(segment)) {
    for (auto& v : rec.second) {
      v.offset += static_start;
    }
  }

  // add to output
  code.insert(code.end(), statics.begin(), statics.end());
}

/*!
 * Do patching that can only be done after all instruction lengths are known
 * Patch jumps to go the right spot. Forward jumps can't know how long the instructions will actully
 * be, so this has to be done after.
 *
 * Also computes the full offset of symbol mem access recs, which also need to know how long
 * instructions are
 */
void x86_Emitter::patch_jumps_and_recs(CodegenOutput& out, std::vector<int>& offsets, int seg) {
  for (auto& jmp : static_jumps[seg]) {
    switch (jmp.type) {
      case SIGNED_32_RIP: {
        // calculate the location of the offset
        int32_t* slot =
            (int32_t*)(out.code[seg].data() + offsets.at(jmp.instr_idx) + jmp.offset_into_instr);

        // find the target instruction (must be forward jump)
        auto instr =
            std::find_if(instructions[seg].begin() + jmp.function_offset, instructions[seg].end(),
                         [&](const Instruction& i) { return i.ir_index == jmp.target_ir_idx; });

        if (instr == instructions[seg].end()) {
          throw std::runtime_error("couldn't find instruction matching IR idx in patch jumps!");
        }

        int target_instr_idx = std::distance(instructions[seg].begin(), instr);

        // store the correct offset.
        *slot = offsets.at(target_instr_idx) - offsets.at(jmp.instr_idx + 1);
      } break;
      default:
        throw std::runtime_error("unknown jump kind in x86_Emitter::patch_jumps_and_recs");
    }
  }

  // patch symbol access as well.
  for (auto& sym_recs_map : symbol_mem_access_recs) {
    for (auto& sym_recs : sym_recs_map) {
      for (auto& rec : sym_recs.second) {
        if (rec.seg == seg) {
          rec.total_offset = rec.offset + offsets.at(rec.instr_idx);
        }
      }
    }
  }
}

/*!
 * Emit the function prologue
 */
void x86_Emitter::emit_prologue() {
  stack_offset = 0;

  // currently we back up all the registers always.
  for (int i = 0; i < SAVED_REG_COUNT; i++) {
    if (f->uses_saved_reg[i]) {
      push(ColoringAssignment(REGISTER, SAVED_REGS[i]));
      stack_offset += GPR_SIZE;
    }
  }

  if (f->uses_rbp) {
    // Setup function registers
    ColoringAssignment rbp(REGISTER, BP_REG);  // base register
    ColoringAssignment r13(REGISTER, T9_REG);  // function call register

    // push old base register
    push(rbp);
    stack_offset += GPR_SIZE;

    // set base register to call register
    mov(rbp, r13);
  }

  // compute total required stack offset of this function, including stack variable
  stack_offset += GPR_SIZE * f->stack_slots;

  // the portion of the stack offset which must be added manually (not by push instructions)
  additional_stack_offset = f->stack_slots * GPR_SIZE;

  extra_push_sr0 = false;

  if (additional_stack_offset || f->requires_aligned_stack) {
    // check that the total is aligned correctly
    if (!(stack_offset & 15)) {
      // if not, add some additional offset to make it correct
      if (additional_stack_offset) {
        additional_stack_offset += 8;
      } else {
        extra_push_sr0 = true;
        push(ColoringAssignment(REGISTER, SAVED_REGS[0]));
      }

      stack_offset += 8;
    }

    // ok, stack should be aligned now
    assert((stack_offset & 15));

    // move RSP manually, if we need to.
    if (additional_stack_offset) {
      if (additional_stack_offset < 126) {
        instructions[current_seg].push_back(IGen::add_gpr64_imm8s(RSP, -additional_stack_offset));
      } else {
        instructions[current_seg].push_back(IGen::add_gpr64_imm32s(RSP, -additional_stack_offset));
      }
    }
  }
}

/*!
 * Emit the function epilogue
 */
void x86_Emitter::emit_epilogue() {
  // reset RSP if needed.

  if (additional_stack_offset || f->requires_aligned_stack) {
    if (additional_stack_offset) {
      if (additional_stack_offset < 126) {
        instructions[current_seg].push_back(IGen::add_gpr64_imm8s(RSP, additional_stack_offset));
      } else {
        instructions[current_seg].push_back(IGen::add_gpr64_imm32s(RSP, additional_stack_offset));
      }
    }

    if (extra_push_sr0) {
      assert(!additional_stack_offset);
      pop(ColoringAssignment(REGISTER, SAVED_REGS[0]));
    }
  }

  // reset RBP
  if (f->uses_rbp) {
    pop(ColoringAssignment(REGISTER, BP_REG));
  }

  for (int i = SAVED_REG_COUNT; i-- > 0;) {
    if (f->uses_saved_reg[i]) {
      pop(ColoringAssignment(REGISTER, SAVED_REGS[i]));
    }
  }

  //
  //  // reset all registers
  //  pop(ColoringAssignment(REGISTER, RBX));
  //  for(int i = (R15 + 1); i-- > R12;) {
  //    pop(ColoringAssignment(REGISTER, i));
  //  }

  // return!
  instructions[current_seg].push_back(IGen::ret());
}

/*!
 * Process a function.
 */
int x86_Emitter::run(FunctionEnv* func, int target_segment) {
  // insert a function start instruction
  instructions[target_segment].push_back(IGen::function_start());

  // set up
  function_offset = instructions[target_segment].size();
  f = func;
  current_rbp_instr_idx = instructions[target_segment].size();
  current_seg = target_segment;

  // add the function prologue
  if (!f->is_asm_func) {
    emit_prologue();
  } else {
    stack_offset = 0;
    additional_stack_offset = 0;
  }

  // add all the instructions
  for (ir_idx = 0; ir_idx < (int)f->code.size(); ir_idx++) {
    auto& x = f->code.at(ir_idx);

    // load anything off the stack needed for this instruction
    auto& bonus = f->bonus_instructions.at(ir_idx);
    for (auto& op : bonus.ops) {
      if (op.load_from_stack) {
        emit_instr(IGen::load64_gpr64_r64off32s(op.ass.reg_id, op.stack_slot * GPR_SIZE, RSP));
      }
    }

    // ugly switch to dispatch the right function to turn the IR into x86 instructions
    switch (x->kind) {
      case RETURN:
        do_return(*dynamic_cast<IR_Return*>(x.get()));
        break;
      case LOAD_INTEGER:
        do_constvar(*dynamic_cast<IR_LoadInteger*>(x.get()));
        break;
      case SET:
        do_set(*dynamic_cast<IR_Set*>(x.get()));
        break;
      case GOTO_LABEL:
        do_goto_label(*dynamic_cast<IR_Goto_Label*>(x.get()));
        break;
      case SET_SYMBOL_VALUE:
        do_set_symbol(*dynamic_cast<IR_SetSymbolValue*>(x.get()));
        break;
      case GET_SYMBOL_VALUE:
        do_get_symbol(*dynamic_cast<IR_GetSymbolValue*>(x.get()));
        break;
      case FUNCTION_CALL:
        do_function_call(*dynamic_cast<IR_FunctionCall*>(x.get()));
        break;
      case STATIC_VAR_ADDR:
        do_static_var_addr(*dynamic_cast<IR_StaticVarAddr*>(x.get()));
        break;
      case FUNC_ADDR:
        do_function_addr(*dynamic_cast<IR_FunctionAddr*>(x.get()));
        break;
      case IR_NULL:
        emit_instr(IGen::null());
        break;
      case FUNCTION_BEGIN:
        // do nothing!
        break;
      case INTEGER_MATH:
        do_integer_math(*dynamic_cast<IR_IntegerMath*>(x.get()));
        break;
      case GET_SYMBOL_OBJ:
        do_get_symbol_object(*dynamic_cast<IR_GetSymbolObj*>(x.get()));
        break;
      case CONDITIONAL_BRANCH:
        do_cond_branch(*dynamic_cast<IR_ConditionalBranch*>(x.get()));
        break;
      case STATIC_VAR_32:
        do_static_var_32(*dynamic_cast<IR_StaticVar32*>(x.get()));
        break;
      case FLOAT_MATH:
        do_float_math(*dynamic_cast<IR_FloatMath*>(x.get()));
        break;
      case LOAD_CONST_OFFSET:
        do_load_const_offset(*dynamic_cast<IR_LoadConstOffset*>(x.get()));
        break;
      case STORE_CONST_OFFSET:
        do_store_const_offset(*dynamic_cast<IR_StoreConstOffset*>(x.get()));
        break;
      case FLOAT_TO_INT:
        do_float_to_int(*dynamic_cast<IR_FloatToInt*>(x.get()));
        break;
      case INT_TO_FLOAT:
        do_int_to_float(*dynamic_cast<IR_IntToFloat*>(x.get()));
        break;
      case GET_RETURN_ADDRESS_POINTER:
        do_get_ra_ptr(*dynamic_cast<IR_GetReturnAddressPointer*>(x.get()));
        break;
      case ASM:
        do_asm(*dynamic_cast<IR_Asm*>(x.get()));
        break;
      default:
        throw std::runtime_error("unknown IR in emitter: " + x->print());
    }

    // store anything onto the stack that is requested.
    for (auto& op : bonus.ops) {
      if (op.store_into_stack) {
        emit_instr(IGen::store64_r64off32s_gpr64(RSP, op.stack_slot * 8, op.ass.reg_id));
      }
    }
  }

  // function epilogue
  if (!f->is_asm_func) {
    emit_epilogue();
  }

  // clean up
  f = nullptr;
  current_seg = -1;
  return function_offset;
}

/*!
 * Insert a static object
 */
void x86_Emitter::run(StaticObject* obj, int target_segment) {
  // this goes in temp storage because we want all functions before any static objects
  // TODO - support for static object with symbols.
  obj->emit_into(static_objects.at(target_segment), type_ptr_recs_in_statics.at(target_segment));
}

/*!
 * Utility function to generate moves for function prologues/epilogues.
 * The newer stuff is preferred for IR translation.
 */
void x86_Emitter::mov(ColoringAssignment dst, ColoringAssignment src) {
  switch (dst.kind) {
    case REGISTER:
      switch (src.kind) {
        case REGISTER:
          instructions[current_seg].push_back(IGen::mov_gpr64_gpr64(dst.reg_id, src.reg_id));
          break;
        default:
          throw std::runtime_error("can't move from this place");
      }
      break;
    default:
      throw std::runtime_error("can't move to this place");
  }
}

/*!
 * Utility function to generate pushes for function prologues/epilogues
 * The newer stuff is preferred for IR translation.
 */
void x86_Emitter::push(ColoringAssignment src) {
  switch (src.kind) {
    case REGISTER:
      instructions[current_seg].push_back(IGen::push_gpr64(src.reg_id));
      break;
    default:
      throw std::runtime_error("can't push this");
  }
}

/*!
 * Utility function to generate pops for function prologues/epilogues
 * The newer stuff is preferred for IR translation.
 */
void x86_Emitter::pop(ColoringAssignment src) {
  switch (src.kind) {
    case REGISTER:
      instructions[current_seg].push_back(IGen::pop_gpr64(src.reg_id));
      break;
    default:
      throw std::runtime_error("can't pop this");
  }
}

/*!
 * Get the coloring assignment of a given place.
 */
ColoringAssignment x86_Emitter::get_ca(Place& var) {
  return f->coloring.at(var.get_assignment().id).get(ir_idx);
}

/*!
 * Get the gpr id number of the current variable, at the current IR.
 */
uint8_t x86_Emitter::gpr_id(Place& var) {
  if (var.get_assignment().kind != REG_GPR) {
    throw std::runtime_error("looked up coloring as gpr something which isn't a colored as a gpr " +
                             var.print() + " ass " + var.get_assignment().print());
  }
  const auto& result = get_ca(var);
  assert(result.is_assigned());
  assert(result.kind == REGISTER);
  return result.reg_id;
}

/*!
 * Get the xmm id number of the current variable, at the current IR.
 */
uint8_t x86_Emitter::xmm_id(Place& var) {
  if (var.get_assignment().kind != REG_XMM_FLOAT) {
    throw std::runtime_error("looked up coloring as xmm something which isn't a colored as a xmm" +
                             var.print() + " ass " + var.get_assignment().print());
  }
  const auto& result = get_ca(var);
  assert(result.is_assigned());
  assert(result.kind == REGISTER);
  return result.reg_id - 16;
}
