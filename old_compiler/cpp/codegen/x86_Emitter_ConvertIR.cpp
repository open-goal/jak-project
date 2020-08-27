/*!
 * @file x86_Emitter_Code.cpp
 * Emitter for converting IR and static objects into GOAL object files for x86 - Code Generation
 * from IR
 */

#include "x86_Emitter.h"
#include "IGen.h"

//;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//  IR TRANSLATION
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;

/*!
 * Move a constant into a register
 */
void x86_Emitter::do_constvar(IR_LoadInteger& cv) {
  auto gpr = gpr_id(*cv.value);

  // todo zero

  if (cv.s_value == 0) {
    emit_instr(IGen::xor_zero_gpr(gpr));
  } else if (cv.s_value > 0) {
    if (cv.us_value < UINT32_MAX) {
      emit_instr(IGen::mov_gpr64_u32(gpr, cv.us_value));
    } else {
      // need a real 64 bit load
      emit_instr(IGen::mov_gpr64_u64(gpr, cv.us_value));
    }
  } else {
    if (cv.s_value >= INT32_MIN) {
      emit_instr(IGen::mov_gpr64_s32(gpr, cv.s_value));
    } else {
      // need a real 64 bit load
      emit_instr(IGen::mov_gpr64_u64(gpr, cv.us_value));
    }
  }
}

/*!
 * Return a variable
 */
void x86_Emitter::do_return(IR_Return& ret) {
  // we need to insert a null instruction here so we can have a jump target to here, even if we
  // don't emit any real instructions
  emit_instr(IGen::null());

  // if we aren't None, we should actually return something
  if (!std::dynamic_pointer_cast<NonePlace>(ret.value)) {
    emit_mov_gpr64_gpr64_or_null(ret.dest, ret.value);
  }
}

/*!
 * Set one reg equal to another. Can handle XMM/GPR sets.
 * Currently all XMM sets are treated as floats.
 */
void x86_Emitter::do_set(IR_Set& set) {
  auto dreg = get_ca(*set.dest).reg_id;
  auto sreg = get_ca(*set.src).reg_id;

  if (dreg < 16 && sreg < 16) {
    emit_mov_gpr64_gpr64_or_null(set.dest, set.src);
  } else if (dreg >= 16 && sreg >= 16) {
    //    emit_instr(IGen::mov_xmm32_xmm32(xmm_id(*set.dest), xmm_id(*set.src)));
    emit_mov_xmm32_xmm32_or_null(set.dest, set.src);
  } else if (dreg < 16 && sreg >= 16) {
    emit_instr(IGen::movd_gpr32_xmm32(gpr_id(*set.dest), xmm_id(*set.src)));
  } else if (dreg >= 16 && sreg < 16) {
    emit_instr(IGen::movd_xmm32_gpr32(xmm_id(*set.dest), gpr_id(*set.src)));
  } else {
    throw std::runtime_error("invalid set - mixed operands");
  }
}

/*!
 * A jump to a label. Can be forward or backward.
 */
void x86_Emitter::do_goto_label(IR_Goto_Label& go_to) {
  assert(go_to.resolved);  // make sure the label is actually valid...
  emit_instr(IGen::jmp_32());

  // create a record.
  StaticJumpRecord rec;
  rec.resolved = false;
  rec.instr_idx = instructions[current_seg].size() - 1;
  rec.offset_into_instr = 1;
  rec.type = SIGNED_32_RIP;
  rec.target_ir_idx = go_to.label->idx;
  rec.function_offset = function_offset;
  static_jumps[current_seg].push_back(rec);
}

/*!
 * The various kinds of conditional branches
 */
void x86_Emitter::do_cond_branch(IR_ConditionalBranch& br) {
  switch (br.cond.kind) {
    case EQUAL_64:
      do_cmp_branch(br, IGen::je_32());
      break;
    case NOT_EQUAL_64:
      do_cmp_branch(br, IGen::jne_32());
      break;
    case LEQ_64:
      if (br.cond.is_signed) {
        do_cmp_branch(br, IGen::jle_32());
      } else {
        do_cmp_branch(br, IGen::jbe_32());
      }
      break;
    case GEQ_64:
      if (br.cond.is_signed) {
        do_cmp_branch(br, IGen::jge_32());
      } else {
        do_cmp_branch(br, IGen::jae_32());
      }
      break;
    case LT_64:
      if (br.cond.is_signed) {
        do_cmp_branch(br, IGen::jl_32());
      } else {
        do_cmp_branch(br, IGen::jb_32());
      }
      break;
    case GT_64:
      if (br.cond.is_signed) {
        do_cmp_branch(br, IGen::jg_32());
      } else {
        do_cmp_branch(br, IGen::ja_32());
      }
      break;
    default:
      throw std::runtime_error("unknown branch type in do_cond_branch");
  }
}

/*!
 * Set the value of a symbol
 */
void x86_Emitter::do_set_symbol(IR_SetSymbolValue& set_symbol) {
  auto dst_as_sym = std::dynamic_pointer_cast<SymbolPlace>(set_symbol.dest);
  assert(dst_as_sym);
  emit_instr(IGen::store32_r64off32s_gpr32(ST_REG, 0x0badbeef, gpr_id(*set_symbol.value)));
  SymbolMemAccessRec rec;
  rec.offset = instructions[current_seg].back().offset_of_disp();
  rec.instr_idx = instructions[current_seg].size() - 1;
  rec.seg = current_seg;
  symbol_mem_access_recs[current_seg][dst_as_sym->name].push_back(rec);
}

/*!
 * Get the value of a symbol
 */
void x86_Emitter::do_get_symbol(IR_GetSymbolValue& get_symbol) {
  emit_instr(IGen::load32_gpr32sz_r64off32s(gpr_id(*get_symbol.dest), 0xbad0beef, ST_REG,
                                            get_symbol.sext));
  SymbolMemAccessRec rec;
  rec.offset = instructions[current_seg].back().offset_of_disp();
  rec.instr_idx = instructions[current_seg].size() - 1;
  rec.seg = current_seg;
  symbol_mem_access_recs[current_seg][get_symbol.symbol->name].push_back(rec);
}

/*!
 * Call a function
 */
void x86_Emitter::do_function_call(IR_FunctionCall& fcall) {
  // currently the function call pointer has the GOAL offset address.
  // we need to change this to actually do a function call.

  emit_mov_gpr64_gpr64_or_null(fcall.func_call, fcall.func_in);

  // make sure the function address reg is correct
  auto freg = get_ca(*fcall.func_call);
  assert(freg.kind == REGISTER);
  assert(freg.reg_id == T9_REG);

  // do the add
  emit_instr(IGen::add_gpr64_gpr64(freg.reg_id, OFF_REG));

  // now do the call
  emit_instr(IGen::call_r64(freg.reg_id));
}

/*!
 * Get the address of a static variable
 * TODO - can the size of this be decreased?
 */
void x86_Emitter::do_static_var_addr(IR_StaticVarAddr& var_addr) {
  // get the offset:
  StaticVarAddrRecord rec;
  load_u64_to_gpr(0xbadcafebadcafe, var_addr.dest);
  rec.instr_idx = instructions[current_seg].size() - 1;
  rec.offset_into_instr = instructions[current_seg].back().offset_of_imm();
  rec.resolved = false;
  rec.place = std::dynamic_pointer_cast<StaticPlace>(var_addr.src);
  rec.current_rbp_instr_idx = current_rbp_instr_idx;
  rec.size = 8;
  assert(rec.place);
  static_var_addr_recs[current_seg].push_back(rec);

  // and add the base
  emit_instr(IGen::add_gpr64_gpr64(gpr_id(*var_addr.dest), RBP));
  emit_instr(IGen::sub_gpr64_gpr64(gpr_id(*var_addr.dest), OFF_REG));
}

/*!
 * Load a static variable (32 bits)
 * TODO - generalize this (and maybe the IR) for other sizes
 */
void x86_Emitter::do_static_var_32(IR_StaticVar32& var_addr) {
  auto ca = get_ca(*var_addr.dest);
  if (ca.reg_id >= 16) {
    emit_instr(IGen::load32_xmm32_r64off32s(xmm_id(*var_addr.dest), RBP, 0xcafecafe));
    StaticVarAddrRecord rec;
    rec.instr_idx = instructions[current_seg].size() - 1;
    rec.offset_into_instr = instructions[current_seg].back().offset_of_disp();
    rec.resolved = false;
    rec.place = std::dynamic_pointer_cast<StaticPlace>(var_addr.src);
    rec.current_rbp_instr_idx = current_rbp_instr_idx;
    rec.size = 4;
    assert(rec.place);
    static_var_addr_recs[current_seg].push_back(rec);
  } else {
    emit_instr(IGen::load32_gpr32sz_r64off32s(gpr_id(*var_addr.dest), 0xcafecafe, RBP,
                                              var_addr.src->type.type->load_signed));
    StaticVarAddrRecord rec;
    rec.instr_idx = instructions[current_seg].size() - 1;
    rec.offset_into_instr = instructions[current_seg].back().offset_of_disp();
    rec.resolved = false;
    rec.place = std::dynamic_pointer_cast<StaticPlace>(var_addr.src);
    rec.current_rbp_instr_idx = current_rbp_instr_idx;
    rec.size = 4;
    assert(rec.place);
    static_var_addr_recs[current_seg].push_back(rec);
  }
}

/*!
 * Get the address of a function
 */
void x86_Emitter::do_function_addr(IR_FunctionAddr& func_addr) {
  FuncAddrRecord rec;
  load_u64_to_gpr(0xcafebadcafebad, func_addr.dest);
  rec.instr_idx = instructions[current_seg].size() - 1;
  rec.offset_into_instr = instructions[current_seg].back().offset_of_imm();
  rec.place = std::dynamic_pointer_cast<LambdaPlace>(func_addr.src);
  rec.resolve = false;
  rec.current_rbp_instr_idx = current_rbp_instr_idx;
  assert(rec.place);
  func_addr_recs[current_seg].push_back(rec);
  emit_instr(IGen::add_gpr64_gpr64(gpr_id(*func_addr.dest), RBP));
  emit_instr(IGen::sub_gpr64_gpr64(gpr_id(*func_addr.dest), OFF_REG));
}

/*!
 * Do math on integers
 */
void x86_Emitter::do_integer_math(IR_IntegerMath& math) {
  switch (math.math_kind) {
    case ADD_64:
      emit_instr(IGen::add_gpr64_gpr64(gpr_id(*math.d), gpr_id(*math.a0)));
      break;

    case SUB_64:
      emit_instr(IGen::sub_gpr64_gpr64(gpr_id(*math.d), gpr_id(*math.a0)));
      break;

    case IMUL_32:
      emit_instr(IGen::imul_gpr32_gpr32(gpr_id(*math.d), gpr_id(*math.a0)));
      emit_instr(IGen::movsx_r64_r32(gpr_id(*math.d), gpr_id(*math.d)));
      break;

    case IDIV_32:
      emit_instr(IGen::cdq());
      emit_instr(IGen::idiv_gpr32(gpr_id(*math.a0)));
      emit_instr(IGen::movsx_r64_r32(gpr_id(*math.d), RAX));
      break;

    case IMOD_32:
      emit_instr(IGen::cdq());
      emit_instr(IGen::idiv_gpr32(gpr_id(*math.a0)));
      emit_instr(IGen::movsx_r64_r32(gpr_id(*math.d), RDX));
      break;

    case SHLV_64:
      emit_instr(IGen::shl_gpr64_cl(gpr_id(*math.d)));
      break;
    case SHRV_64:
      emit_instr(IGen::shr_gpr64_cl(gpr_id(*math.d)));
      break;
    case SARV_64:
      emit_instr(IGen::sar_gpr64_cl(gpr_id(*math.d)));
      break;

    case SHL_64:
      emit_instr(IGen::shl_gpr64_u8(gpr_id(*math.d), math.sa));
      break;
    case SHR_64:
      emit_instr(IGen::shr_gpr64_u8(gpr_id(*math.d), math.sa));
      break;
    case SAR_64:
      emit_instr(IGen::sar_gpr64_u8(gpr_id(*math.d), math.sa));
      break;

    case OR_64:
      emit_instr(IGen::or_gpr64_gpr64(gpr_id(*math.d), gpr_id(*math.a0)));
      break;
    case AND_64:
      emit_instr(IGen::and_gpr64_gpr64(gpr_id(*math.d), gpr_id(*math.a0)));
      break;
    case XOR_64:
      emit_instr(IGen::xor_gpr64_gpr64(gpr_id(*math.d), gpr_id(*math.a0)));
      break;
    case NOT_64:
      emit_instr(IGen::not_gpr64(gpr_id(*math.d)));
      break;

    default:
      throw std::runtime_error("unknown integer math kind in emitter");
  }
}

/*!
 * Do Math on Floats
 */
void x86_Emitter::do_float_math(IR_FloatMath& fl) {
  switch (fl.math_kind) {
    case MUL_SS:
      emit_instr(IGen::mulss_xmm_xmm(xmm_id(*fl.d), xmm_id(*fl.a0)));
      break;
    case DIV_SS:
      emit_instr(IGen::divss_xmm_xmm(xmm_id(*fl.d), xmm_id(*fl.a0)));
      break;
    case SUB_SS:
      emit_instr(IGen::subss_xmm_xmm(xmm_id(*fl.d), xmm_id(*fl.a0)));
      break;
    case ADD_SS:
      emit_instr(IGen::addss_xmm_xmm(xmm_id(*fl.d), xmm_id(*fl.a0)));
      break;
    default:
      throw std::runtime_error("unknown float math kind in emitter");
  }
}

void x86_Emitter::do_int_to_float(IR_IntToFloat& i2f) {
  emit_instr(IGen::int32_to_float(xmm_id(*i2f.dest), gpr_id(*i2f.src)));
}

void x86_Emitter::do_float_to_int(IR_FloatToInt& f2i) {
  emit_instr(IGen::float_to_int64(gpr_id(*f2i.dest), xmm_id(*f2i.src)));
}

/*!
 * Get a pointer to a symbol.
 */
void x86_Emitter::do_get_symbol_object(IR_GetSymbolObj& get_sym) {
  auto dest_id = get_ca(*get_sym.dest).reg_id;
  emit_instr(IGen::mov_gpr64_gpr64(dest_id, ST_REG));       // s7
  emit_instr(IGen::add_gpr64_imm32s(dest_id, 0xcafecafe));  // + offset
  SymbolMemAccessRec rec;
  rec.offset = instructions[current_seg].back().offset_of_imm();
  rec.instr_idx = instructions[current_seg].size() - 1;
  rec.seg = current_seg;
  symbol_mem_access_recs[current_seg][get_sym.sym->name].push_back(rec);
  emit_instr(IGen::sub_gpr64_gpr64(dest_id, OFF_REG));
}

/*!
 * Load from memory!
 */
void x86_Emitter::do_load_const_offset(IR_LoadConstOffset& load) {
  auto dst_reg = gpr_id(*load.dst);
  auto src_reg = gpr_id(*load.src);
  emit_instr(IGen::mov_gpr64_gpr64(dst_reg, src_reg));
  emit_instr(IGen::add_gpr64_gpr64(dst_reg, OFF_REG));
  if (load.size == 8) {
    emit_instr(IGen::load64_gpr64_r64off32s(dst_reg, load.offset, dst_reg));
  } else if (load.size == 4) {
    emit_instr(IGen::load32_gpr32sz_r64off32s(dst_reg, load.offset, dst_reg, load.is_signed));
  } else if (load.size == 2 && !load.is_signed) {
    emit_instr(IGen::load16_gpr16z_r64off32s(dst_reg, dst_reg, load.offset));
  } else if (load.size == 2 && load.is_signed) {
    emit_instr(IGen::load16_gpr16s_r64off32s(dst_reg, dst_reg, load.offset));
  } else if (load.size == 1 && !load.is_signed) {
    emit_instr(IGen::load16_gpr8z_r64off32s(dst_reg, dst_reg, load.offset));
  } else {
    throw std::runtime_error("unsupported load size in do_load_const_offset " + load.print());
  }
}

/*!
 * Store into memory!
 */
void x86_Emitter::do_store_const_offset(IR_StoreConstOffset& store) {
  // this sucks
  auto mem_reg = gpr_id(*store.mem);
  auto val_reg = gpr_id(*store.val);

  if (mem_reg == val_reg) {
    assert(mem_reg != BP_REG);
    assert(val_reg != BP_REG);
    emit_instr(IGen::push_gpr64(BP_REG));
    emit_instr(IGen::mov_gpr64_gpr64(BP_REG, OFF_REG));
    emit_instr(IGen::add_gpr64_gpr64(BP_REG, mem_reg));
    if (store.size == 8) {
      emit_instr(IGen::store64_r64off32s_gpr64(BP_REG, store.offset, val_reg));
    } else if (store.size == 4) {
      emit_instr(IGen::store32_r64off32s_gpr32(BP_REG, store.offset, val_reg));
    } else if (store.size == 2) {
      emit_instr(IGen::store16_r64off32s_gpr16(BP_REG, store.offset, val_reg));
    } else if (store.size == 1) {
      emit_instr(IGen::store8_r64off32s_gpr8(BP_REG, store.offset, val_reg));
    } else {
      throw std::runtime_error("unsupported load size in do_store_const_offset");
    }
    emit_instr(IGen::pop_gpr64(BP_REG));
  } else {
    emit_instr(IGen::add_gpr64_gpr64(mem_reg, OFF_REG));
    if (store.size == 8) {
      emit_instr(IGen::store64_r64off32s_gpr64(mem_reg, store.offset, val_reg));
    } else if (store.size == 4) {
      emit_instr(IGen::store32_r64off32s_gpr32(mem_reg, store.offset, val_reg));
    } else if (store.size == 2) {
      emit_instr(IGen::store16_r64off32s_gpr16(mem_reg, store.offset, val_reg));
    } else if (store.size == 1) {
      emit_instr(IGen::store8_r64off32s_gpr8(mem_reg, store.offset, val_reg));
    } else {
      throw std::runtime_error("unsupported load size in do_store_const_offset");
    }
    emit_instr(IGen::sub_gpr64_gpr64(mem_reg, OFF_REG));
  }
}

void x86_Emitter::do_get_ra_ptr(IR_GetReturnAddressPointer& get_ra) {
  auto dst_reg = gpr_id(*get_ra.dest);
  uint64_t offset = (int64_t)(stack_offset + 0);
  emit_instr(IGen::mov_gpr64_u64(dst_reg, offset));
  emit_instr(IGen::add_gpr64_gpr64(dst_reg, RSP));
  emit_instr(IGen::sub_gpr64_gpr64(dst_reg, OFF_REG));
}
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;
//  CODEGEN UTILITIES
//;;;;;;;;;;;;;;;;;;;;;;;;;;;;

/*!
 * Load a uint64_t into a gpr.
 */
void x86_Emitter::load_u64_to_gpr(uint64_t value, std::shared_ptr<Place> var) {
  auto as_reg = std::dynamic_pointer_cast<GprPlace>(var);
  if (as_reg) {
    emit_instr(IGen::mov_gpr64_u64(gpr_id(*var), value));
  } else {
    throw std::runtime_error("don't know how to load constant to this variable");
  }
}

/*!
 * Emit a move between two gpr64's if the two given gpr's aren't the same.
 * If a move isn't emitted, a null instruction is emitted instead.
 */
void x86_Emitter::emit_mov_gpr64_gpr64_or_null(uint8_t dst, uint8_t src) {
  if (dst == src) {
    emit_instr(IGen::null());
  } else {
    emit_instr(IGen::mov_gpr64_gpr64(dst, src));
  }
}

/*!
 * Emit a move between two gpr64's if the two given gpr's aren't the same.
 * If a move isn't emitted, a null instruction is emitted instead.
 */
void x86_Emitter::emit_mov_gpr64_gpr64_or_null(std::shared_ptr<Place> dst,
                                               std::shared_ptr<Place> src) {
  emit_mov_gpr64_gpr64_or_null(gpr_id(*dst), gpr_id(*src));
}

void x86_Emitter::emit_mov_xmm32_xmm32_or_null(std::shared_ptr<Place> dst,
                                               std::shared_ptr<Place> src) {
  auto dst_id = xmm_id(*dst);
  auto src_id = xmm_id(*src);
  if (dst_id == src_id) {
    emit_instr(IGen::null());
  } else {
    emit_instr(IGen::mov_xmm32_xmm32(dst_id, src_id));
  }
}

/*!
 * Given the jump instruction, create the cmp/jmp sequence.
 */
void x86_Emitter::do_cmp_branch(IR_ConditionalBranch& br, Instruction jump_instr) {
  assert(br.resolved);
  if (br.cond.is_float) {
    emit_instr(IGen::cmp_flt_flt(xmm_id(*br.cond.a), xmm_id(*br.cond.b)));
  } else {
    emit_instr(IGen::cmp_gpr64_gpr64(gpr_id(*br.cond.a), gpr_id(*br.cond.b)));
  }

  emit_instr(jump_instr);
  StaticJumpRecord rec;
  rec.resolved = false;
  rec.instr_idx = instructions[current_seg].size() - 1;
  rec.offset_into_instr = instructions[current_seg].back().offset_of_imm();
  rec.type = SIGNED_32_RIP;
  rec.target_ir_idx = br.label->idx;
  rec.function_offset = function_offset;
  static_jumps[current_seg].push_back(rec);
}

void x86_Emitter::do_asm(IR_Asm& asm_op) {
  switch (asm_op.asm_kind) {
    case IR_Asm::RET:
      assert(asm_op.args.empty());
      emit_instr(IGen::ret());
      break;
    case IR_Asm::RET_REGISTER:
      emit_instr(IGen::ret());
      break;
    case IR_Asm::PUSH:
      assert(asm_op.args.size() == 1);
      emit_instr(IGen::push_gpr64(gpr_id(*asm_op.args.at(0))));
      break;
    case IR_Asm::POP:
      assert(asm_op.args.size() == 1);
      emit_instr(IGen::pop_gpr64(gpr_id(*asm_op.args.at(0))));
      break;
    case IR_Asm::JMP:
      assert(asm_op.args.size() == 1);
      emit_instr(IGen::jmp_r64(gpr_id(*asm_op.args.at(0))));
      break;
    case IR_Asm::SUB:
      assert(asm_op.args.size() == 2);
      emit_instr(IGen::sub_gpr64_gpr64(gpr_id(*asm_op.args.at(0)), gpr_id(*asm_op.args.at(2))));
      break;
    default:
      throw std::runtime_error("unknown asm op in emitter");
  }
}