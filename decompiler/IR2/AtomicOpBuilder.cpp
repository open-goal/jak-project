#include "AtomicOpBuilder.h"

#include <memory>
#include "common/log/log.h"
#include "common/symbols.h"
#include "decompiler/Function/BasicBlocks.h"
#include "decompiler/Function/Function.h"
#include "decompiler/Disasm/InstructionMatching.h"

namespace decompiler {

namespace {

//////////////////////
// Register Helpers
//////////////////////

Register rs7() {
  return make_gpr(Reg::S7);
}

Register rr0() {
  return make_gpr(Reg::R0);
}

Register rfp() {
  return make_gpr(Reg::FP);
}

/////////////////////////
// Variable Helpers
/////////////////////////

Variable make_dst_var(Register reg, int idx) {
  return Variable(Variable::Mode::WRITE, reg, idx);
}

Variable make_src_var(Register reg, int idx) {
  return Variable(Variable::Mode::READ, reg, idx);
}

Variable make_dst_var(const Instruction& i, int idx) {
  assert(i.n_dst == 1);
  return make_dst_var(i.get_dst(0).get_reg(), idx);
}

////////////////////////
// Atom Helpers
////////////////////////

SimpleAtom make_src_atom(Register reg, int idx) {
  return SimpleAtom::make_var(make_src_var(reg, idx));
}

SimpleAtom false_sym() {
  return SimpleAtom::make_sym_ptr("#f");
}

////////////////////////
// Expression Helpers
////////////////////////

SimpleExpression make_2reg_expr(const Instruction& instr, SimpleExpression::Kind kind, int idx) {
  auto src0 = make_src_atom(instr.get_src(0).get_reg(), idx);
  auto src1 = make_src_atom(instr.get_src(1).get_reg(), idx);
  return SimpleExpression(kind, src0, src1);
}

SimpleExpression make_1reg_1imm_expr(const Instruction& instr,
                                     SimpleExpression::Kind kind,
                                     int idx,
                                     int imm_offset = 0) {
  auto src0 = make_src_atom(instr.get_src(0).get_reg(), idx);
  auto src1 = SimpleAtom::make_int_constant(instr.get_src(1).get_imm() + imm_offset);
  return SimpleExpression(kind, src0, src1);
}

SimpleExpression make_1reg_expr(const Instruction& instr, SimpleExpression::Kind kind, int idx) {
  auto src = make_src_atom(instr.get_src(0).get_reg(), idx);
  return SimpleExpression(kind, src);
}

SimpleExpression make_reg_plus_int(Register reg, int integer, int idx) {
  return SimpleExpression(SimpleExpression::Kind::ADD, make_src_atom(reg, idx),
                          SimpleAtom::make_int_constant(integer));
}

////////////////////////
// AtmoicOp Helpers
////////////////////////

/*!
 * Convert a single instruction in the form instr dest_reg, src_reg, src_reg
 * to an atomic op of (set! dst_reg (op src_reg src_reg))
 * Like daddu a0, a1, a2
 */
std::unique_ptr<AtomicOp> make_3reg_op(const Instruction& instr,
                                       SimpleExpression::Kind kind,
                                       int idx) {
  auto dst = make_dst_var(instr.get_dst(0).get_reg(), idx);
  return std::make_unique<SetVarOp>(dst, make_2reg_expr(instr, kind, idx), idx);
}

std::unique_ptr<AtomicOp> make_2reg_1imm_op(const Instruction& instr,
                                            SimpleExpression::Kind kind,
                                            int idx,
                                            int imm_offset = 0) {
  auto dst = make_dst_var(instr.get_dst(0).get_reg(), idx);
  return std::make_unique<SetVarOp>(dst, make_1reg_1imm_expr(instr, kind, idx, imm_offset), idx);
}

/*!
 * Convert a single instruction in the form instr dest_reg, src_reg
 * to an atomic op of (set! dest_reg (op src_reg))
 */
std::unique_ptr<AtomicOp> make_2reg_op(const Instruction& instr,
                                       SimpleExpression::Kind kind,
                                       int idx) {
  auto dst = make_dst_var(instr.get_dst(0).get_reg(), idx);
  return std::make_unique<SetVarOp>(dst, make_1reg_expr(instr, kind, idx), idx);
}

/*!
 * Common load helper. Supports fp relative, 0 offset, or integer constant offset
 */
std::unique_ptr<AtomicOp> make_standard_load(const Instruction& i0,
                                             int idx,
                                             int load_size,
                                             LoadVarOp::Kind kind) {
  auto dst = make_dst_var(i0, idx);
  SimpleExpression src;
  if (i0.get_src(0).is_label() && i0.get_src(1).is_reg(rfp())) {
    // it's an FP relative load.
    src = SimpleAtom::make_static_address(i0.get_src(0).get_label()).as_expr();
  } else if (i0.get_src(0).is_imm() && i0.get_src(0).get_imm() == 0) {
    // the offset is 0
    src = make_src_atom(i0.get_src(1).get_reg(), idx).as_expr();
  } else if (i0.get_src(0).is_imm()) {
    // the offset is not 0
    src = make_reg_plus_int(i0.get_src(1).get_reg(), i0.get_src(0).get_imm(), idx);
  } else {
    assert(false);
  }
  return std::make_unique<LoadVarOp>(kind, load_size, dst, src, idx);
}

std::unique_ptr<AtomicOp> make_standard_store(const Instruction& i0,
                                              int idx,
                                              int store_size,
                                              bool is_float) {
  SimpleAtom val;
  SimpleExpression dst;
  if (i0.get_src(0).is_reg(rs7())) {
    assert(!is_float);
    val = SimpleAtom::make_sym_ptr("#f");
  } else {
    val = make_src_atom(i0.get_src(0).get_reg(), idx);
  }

  auto base_reg = make_src_atom(i0.get_src(2).get_reg(), idx);
  auto offset = i0.get_src(1).get_imm();
  if (offset == 0) {
    dst = base_reg.as_expr();
  } else {
    dst = SimpleExpression(SimpleExpression::Kind::ADD, base_reg,
                           SimpleAtom::make_int_constant(offset));
  }

  return std::make_unique<StoreOp>(store_size, is_float, dst, val, idx);
}

///////////////////////
// OP 1 Conversions
//////////////////////

std::unique_ptr<AtomicOp> convert_or_1(const Instruction& i0, int idx) {
  auto dest = make_dst_var(i0, idx);
  SimpleExpression src;

  if (is_gpr_3(i0, InstructionKind::OR, {}, rs7(), rr0())) {
    // set reg_dest to #f : or reg_dest, s7, r0
    src = false_sym().as_expr();
  } else if (is_gpr_3(i0, InstructionKind::OR, {}, rr0(), rr0())) {
    // set reg_dest to 0 : or reg_dest, r0, r0
    src = SimpleAtom::make_int_constant(0).as_expr();
  } else if (is_gpr_3(i0, InstructionKind::OR, {}, {}, rr0())) {
    // set dst to src : or dst, src, r0
    src = make_src_atom(i0.get_src(0).get_reg(), idx).as_expr();
  } else {
    // actually do a logical OR of two registers: or a0, a1, a2
    src = make_2reg_expr(i0, SimpleExpression::Kind::OR, idx);
  }
  return std::make_unique<SetVarOp>(dest, src, idx);
}

std::unique_ptr<AtomicOp> convert_ori_1(const Instruction& i0, int idx) {
  auto dest = make_dst_var(i0, idx);
  SimpleExpression src;
  if (i0.get_src(0).is_reg(rr0()) && i0.get_src(1).is_imm()) {
    // load a 16-bit integer constant
    // ori reg, r0, 1234
    src = SimpleAtom::make_int_constant(i0.get_src(1).get_imm()).as_expr();
  } else if (i0.get_src(1).is_imm()) {
    // logical or with constant integer
    // ori dst, a0, 1234
    return make_2reg_1imm_op(i0, SimpleExpression::Kind::OR, idx);
  } else {
    assert(false);
  }
  return std::make_unique<SetVarOp>(dest, src, idx);
}

std::unique_ptr<AtomicOp> convert_lw_1(const Instruction& i0, int idx) {
  if (i0.get_dst(0).is_reg(rr0()) && i0.get_src(0).is_imm(2) && i0.get_src(1).is_reg(rr0())) {
    // lw r0, 2(r0), used to trigger an exception on purpose.
    return std::make_unique<SpecialOp>(SpecialOp::Kind::BREAK, idx);
  } else if (i0.get_src(1).is_reg(rs7()) && i0.get_src(0).is_sym()) {
    // symbol load.
    return std::make_unique<SetVarOp>(
        make_dst_var(i0, idx), SimpleAtom::make_sym_val(i0.get_src(0).get_sym()).as_expr(), idx);
  } else {
    // fall back to standard loads
    return make_standard_load(i0, idx, 4, LoadVarOp::Kind::SIGNED);
  }
}

std::unique_ptr<AtomicOp> convert_daddiu_1(const Instruction& i0, int idx) {
  if (i0.get_src(0).is_reg(rs7()) && i0.get_src(1).is_sym()) {
    // get symbol pointer
    return std::make_unique<SetVarOp>(
        make_dst_var(i0, idx), SimpleAtom::make_sym_ptr(i0.get_src(1).get_sym()).as_expr(), idx);
  } else if (i0.get_src(0).is_reg(rs7()) && i0.get_src(1).is_imm(FIX_SYM_EMPTY_PAIR)) {
    // get empty pair
    return std::make_unique<SetVarOp>(make_dst_var(i0, idx),
                                      SimpleAtom::make_empty_list().as_expr(), idx);
  } else if (i0.get_src(0).is_reg(rs7()) && i0.get_src(1).is_imm(-32768)) {
    // get pointer to beginning of symbol table (this is a bit of a hack)
    return std::make_unique<SetVarOp>(
        make_dst_var(i0, idx), SimpleAtom::make_sym_val("__START-OF-TABLE__").as_expr(), idx);
  } else if (i0.get_src(0).is_reg(rs7()) && i0.get_src(1).is_imm(FIX_SYM_TRUE)) {
    // get pointer to beginning of symbol table (this is a bit of a hack)
    return std::make_unique<SetVarOp>(make_dst_var(i0, idx),
                                      SimpleAtom::make_sym_ptr("#t").as_expr(), idx);
  } else if (i0.get_src(0).is_reg(rfp()) && i0.get_src(1).is_label()) {
    // get address of static
    return std::make_unique<SetVarOp>(
        make_dst_var(i0, idx), SimpleAtom::make_static_address(i0.get_src(1).get_label()).as_expr(),
        idx);
  } else {
    // fall back to normal add.
    return make_2reg_1imm_op(i0, SimpleExpression::Kind::ADD, idx);
  }
}

std::unique_ptr<AtomicOp> convert_dsubu_1(const Instruction& i0, int idx) {
  if (i0.get_src(0).is_reg(rr0())) {
    return std::make_unique<SetVarOp>(
        make_dst_var(i0, idx),
        SimpleExpression(SimpleExpression::Kind::NEG, make_src_atom(i0.get_src(1).get_reg(), idx)),
        idx);
  } else {
    // fall back
    return make_3reg_op(i0, SimpleExpression::Kind::SUB, idx);
  }
}

std::unique_ptr<AtomicOp> convert_nor_1(const Instruction& i0, int idx) {
  if (i0.get_src(1).is_reg(rr0())) {
    return std::make_unique<SetVarOp>(make_dst_var(i0, idx),
                                      SimpleExpression(SimpleExpression::Kind::LOGNOT,
                                                       make_src_atom(i0.get_src(0).get_reg(), idx)),
                                      idx);
  } else {
    // fall back
    return make_3reg_op(i0, SimpleExpression::Kind::NOR, idx);
  }
}

std::unique_ptr<AtomicOp> convert_addiu_1(const Instruction& i0, int idx) {
  // addiu is used to load a constant. sometimes.
  if (i0.get_src(0).is_reg(rr0())) {
    return std::make_unique<SetVarOp>(
        make_dst_var(i0, idx), SimpleAtom::make_int_constant(i0.get_src(1).get_imm()).as_expr(),
        idx);
  } else {
    // may be assembly
    return nullptr;
  }
}

std::unique_ptr<AtomicOp> convert_lui_1(const Instruction& i0, int idx) {
  if (i0.get_src(0).is_imm()) {
    return std::make_unique<SetVarOp>(
        make_dst_var(i0, idx),
        SimpleAtom::make_int_constant(i0.get_src(0).get_imm() << 16).as_expr(), idx);
  }
  return nullptr;
}

std::unique_ptr<AtomicOp> convert_sll_1(const Instruction& i0, int idx) {
  if (is_nop(i0)) {
    return std::make_unique<SpecialOp>(SpecialOp::Kind::NOP, idx);
  }
  return nullptr;
}

std::unique_ptr<AtomicOp> convert_sw_1(const Instruction& i0, int idx) {
  if (i0.get_src(1).is_sym() && i0.get_src(2).is_reg(rs7())) {
    auto name = i0.get_src(1).get_sym();
    // store into symbol table!
    SimpleAtom val;
    if (i0.get_src(0).is_reg(rs7())) {
      // store a false
      val = SimpleAtom::make_sym_ptr("#f");
    } else {
      // store a register.
      val = make_src_atom(i0.get_src(0).get_reg(), idx);
    }
    return std::make_unique<StoreOp>(4, false, SimpleAtom::make_sym_val(name).as_expr(), val, idx);
  } else {
    return make_standard_store(i0, idx, 4, false);
  }
}

// movn or movz
std::unique_ptr<AtomicOp> convert_cmov_1(const Instruction& i0, int idx) {
  if (i0.get_src(0).is_reg(rs7())) {
    return std::make_unique<ConditionalMoveFalseOp>(make_dst_var(i0, idx),
                                                    make_src_var(i0.get_src(1).get_reg(), idx),
                                                    i0.kind == InstructionKind::MOVZ, idx);
  } else {
    return nullptr;
  }
}

std::unique_ptr<AtomicOp> convert_1(const Instruction& i0, int idx) {
  switch (i0.kind) {
    case InstructionKind::OR:
      return convert_or_1(i0, idx);
    case InstructionKind::ORI:
      return convert_ori_1(i0, idx);
    case InstructionKind::AND:
      return make_3reg_op(i0, SimpleExpression::Kind::AND, idx);
    case InstructionKind::MTC1:
      return make_2reg_op(i0, SimpleExpression::Kind::GPR_TO_FPR, idx);
    case InstructionKind::MFC1:
      return make_2reg_op(i0, SimpleExpression::Kind::FPR_TO_GPR, idx);
    case InstructionKind::LWC1:
      return make_standard_load(i0, idx, 4, LoadVarOp::Kind::FLOAT);
    case InstructionKind::LB:
      return make_standard_load(i0, idx, 1, LoadVarOp::Kind::SIGNED);
    case InstructionKind::LBU:
      return make_standard_load(i0, idx, 1, LoadVarOp::Kind::UNSIGNED);
    case InstructionKind::LHU:
      return make_standard_load(i0, idx, 2, LoadVarOp::Kind::UNSIGNED);
    case InstructionKind::LH:
      return make_standard_load(i0, idx, 2, LoadVarOp::Kind::SIGNED);
    case InstructionKind::LWU:
      return make_standard_load(i0, idx, 4, LoadVarOp::Kind::UNSIGNED);
    case InstructionKind::LW:
      return convert_lw_1(i0, idx);
    case InstructionKind::LD:
      return make_standard_load(i0, idx, 8, LoadVarOp::Kind::UNSIGNED);
    case InstructionKind::DSLL:
      return make_2reg_1imm_op(i0, SimpleExpression::Kind::LEFT_SHIFT, idx);
    case InstructionKind::DSLL32:
      return make_2reg_1imm_op(i0, SimpleExpression::Kind::LEFT_SHIFT, idx, 32);
    case InstructionKind::DSRA:
      return make_2reg_1imm_op(i0, SimpleExpression::Kind::RIGHT_SHIFT_ARITH, idx);
    case InstructionKind::DSRA32:
      return make_2reg_1imm_op(i0, SimpleExpression::Kind::RIGHT_SHIFT_ARITH, idx, 32);
    case InstructionKind::DSRL:
      return make_2reg_1imm_op(i0, SimpleExpression::Kind::RIGHT_SHIFT_LOGIC, idx);
    case InstructionKind::DSRL32:
      return make_2reg_1imm_op(i0, SimpleExpression::Kind::RIGHT_SHIFT_LOGIC, idx, 32);
    case InstructionKind::DIVS:
      return make_3reg_op(i0, SimpleExpression::Kind::DIV_S, idx);
    case InstructionKind::SUBS:
      return make_3reg_op(i0, SimpleExpression::Kind::SUB_S, idx);
    case InstructionKind::ADDS:
      return make_3reg_op(i0, SimpleExpression::Kind::ADD_S, idx);
    case InstructionKind::MULS:
      return make_3reg_op(i0, SimpleExpression::Kind::MUL_S, idx);
    case InstructionKind::MINS:
      return make_3reg_op(i0, SimpleExpression::Kind::MIN_S, idx);
    case InstructionKind::MAXS:
      return make_3reg_op(i0, SimpleExpression::Kind::MAX_S, idx);
    case InstructionKind::DADDIU:
      return convert_daddiu_1(i0, idx);
    case InstructionKind::DADDU:
      return make_3reg_op(i0, SimpleExpression::Kind::ADD, idx);
    case InstructionKind::DSUBU:
      return convert_dsubu_1(i0, idx);
    case InstructionKind::MULT3:
      return make_3reg_op(i0, SimpleExpression::Kind::MUL_SIGNED, idx);
    case InstructionKind::MULTU3:
      return make_3reg_op(i0, SimpleExpression::Kind::MUL_UNSIGNED, idx);
    case InstructionKind::ANDI:
      return make_2reg_1imm_op(i0, SimpleExpression::Kind::AND, idx);
    case InstructionKind::XORI:
      return make_2reg_1imm_op(i0, SimpleExpression::Kind::XOR, idx);
    case InstructionKind::NOR:
      return convert_nor_1(i0, idx);
    case InstructionKind::XOR:
      return make_3reg_op(i0, SimpleExpression::Kind::XOR, idx);
    case InstructionKind::ADDIU:
      return convert_addiu_1(i0, idx);
    case InstructionKind::LUI:
      return convert_lui_1(i0, idx);
    case InstructionKind::SLL:
      return convert_sll_1(i0, idx);
    case InstructionKind::DSRAV:
      return make_3reg_op(i0, SimpleExpression::Kind::RIGHT_SHIFT_ARITH, idx);
    case InstructionKind::DSRLV:
      return make_3reg_op(i0, SimpleExpression::Kind::RIGHT_SHIFT_LOGIC, idx);
    case InstructionKind::DSLLV:
      return make_3reg_op(i0, SimpleExpression::Kind::LEFT_SHIFT, idx);
    case InstructionKind::SB:
      return make_standard_store(i0, idx, 1, false);
    case InstructionKind::SH:
      return make_standard_store(i0, idx, 2, false);
    case InstructionKind::SW:
      return convert_sw_1(i0, idx);
    case InstructionKind::SD:
      return make_standard_store(i0, idx, 8, false);
    case InstructionKind::SWC1:
      return make_standard_store(i0, idx, 4, true);
    case InstructionKind::CVTWS:  // float to int
      return make_2reg_op(i0, SimpleExpression::Kind::FLOAT_TO_INT, idx);
    case InstructionKind::CVTSW:  // int to float
      return make_2reg_op(i0, SimpleExpression::Kind::INT_TO_FLOAT, idx);
    case InstructionKind::ABSS:
      return make_2reg_op(i0, SimpleExpression::Kind::ABS_S, idx);
    case InstructionKind::NEGS:
      return make_2reg_op(i0, SimpleExpression::Kind::NEG_S, idx);
    case InstructionKind::SQRTS:
      return make_2reg_op(i0, SimpleExpression::Kind::SQRT_S, idx);
    case InstructionKind::MOVS:
      return make_2reg_op(i0, SimpleExpression::Kind::IDENTITY, idx);
    case InstructionKind::MOVN:
    case InstructionKind::MOVZ:
      return convert_cmov_1(i0, idx);
    default:
      return nullptr;
  }
}

}  // namespace

/*!
 * Convert an entire basic block and add the results to a FunctionAtomicOps
 * @param block_id  : the index of the block
 * @param begin     : the start of the instructions for the block
 * @param end       : the end of the instructions for the block
 * @param container : the container to add to
 */
void convert_block_to_atomic_ops(int begin_idx,
                                 std::vector<Instruction>::const_iterator begin,
                                 std::vector<Instruction>::const_iterator end,
                                 const std::vector<DecompilerLabel>& labels,
                                 FunctionAtomicOps* container) {
  container->block_id_to_first_atomic_op.push_back(container->ops.size());
  for (auto& instr = begin; instr < end;) {
    // how many instructions can we look at, at most?
    int n_instr = end - instr;
    // how many instructions did we use?
    int length = 0;
    // what is the index of the atomic op we would add
    int op_idx = int(container->ops.size());

    bool converted = false;
    std::unique_ptr<AtomicOp> op;

    if (n_instr >= 4) {
      // try 4 instructions
    }

    if (!converted && n_instr >= 3) {
      // try 3 instructions
    }

    if (!converted && n_instr >= 2) {
      // try 2 instructions
    }

    if (!converted) {
      // try 1 instruction
      op = convert_1(*instr, op_idx);
      if (op) {
        converted = true;
        length = 1;
      }
    }

    if (!converted) {
      // try assembly fallback.
    }

    if (!converted) {
      // failed!
      throw std::runtime_error("Failed to convert " + instr->to_string(labels));
      //      lg::die("Failed to convert instruction {} to an atomic op", instr->to_string(labels));
    }

    assert(converted && length && op);
    // add mappings:
    container->atomic_op_to_instruction[container->ops.size()] = begin_idx;
    for (int i = 0; i < length; i++) {
      container->instruction_to_atomic_op[begin_idx + i] = container->ops.size();
    }
    // add
    op->update_register_info();
    container->ops.emplace_back(std::move(op));
    instr += length;
  }
  container->block_id_to_end_atomic_op.push_back(container->ops.size());
}

FunctionAtomicOps convert_function_to_atomic_ops(const Function& func,
                                                 const std::vector<DecompilerLabel>& labels) {
  FunctionAtomicOps result;

  for (const auto& block : func.basic_blocks) {
    // we should only consider the blocks which actually have instructions:
    if (block.end_word > block.start_word) {
      auto begin = func.instructions.begin() + block.start_word;
      auto end = func.instructions.begin() + block.end_word;
      convert_block_to_atomic_ops(block.start_word, begin, end, labels, &result);
    } else {
      result.block_id_to_first_atomic_op.push_back(-1);
      result.block_id_to_end_atomic_op.push_back(-1);
    }
  }

  assert(func.basic_blocks.size() == result.block_id_to_end_atomic_op.size());
  assert(func.basic_blocks.size() == result.block_id_to_first_atomic_op.size());
  return result;
}
}  // namespace decompiler