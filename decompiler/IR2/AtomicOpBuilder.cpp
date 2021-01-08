#include "AtomicOpBuilder.h"

#include <memory>
#include "common/log/log.h"
#include "decompiler/Function/BasicBlocks.h"
#include "decompiler/Function/Function.h"
#include "decompiler/Disasm/InstructionMatching.h"

namespace decompiler {

namespace {

Variable make_dst_var(Register reg, int idx) {
  return Variable(Variable::Mode::WRITE, reg, idx);
}

Variable make_src_var(Register reg, int idx) {
  return Variable(Variable::Mode::READ, reg, idx);
}

SimpleAtom make_src_atom(Register reg, int idx) {
  return SimpleAtom::make_var(make_src_var(reg, idx));
}

Register rs7() {
  return make_gpr(Reg::S7);
}

Register rr0() {
  return make_gpr(Reg::R0);
}

SimpleAtom false_sym() {
  return SimpleAtom::make_sym_ptr("#f");
}

Variable make_dst_var(const Instruction& i, int idx) {
  return make_dst_var(i.get_dst(0).get_reg(), idx);
}

SimpleExpression make_2reg_expr(const Instruction& instr, SimpleExpression::Kind kind, int idx) {
  auto src0 = make_src_atom(instr.get_src(0).get_reg(), idx);
  auto src1 = make_src_atom(instr.get_src(1).get_reg(), idx);
  return SimpleExpression(kind, src0, src1);
}

SimpleExpression make_1reg_expr(const Instruction& instr, SimpleExpression::Kind kind, int idx) {
  auto src = make_src_atom(instr.get_src(0).get_reg(), idx);
  return SimpleExpression(kind, src);
}

/*!
 * Convert a single instruction in the form instr dest_reg, src_reg, src_reg
 * to an atomic op of (set! dst_reg (op src_reg src_reg))
 * Like daddu a0, a1, a2
 */
void make_3reg_op(const Instruction& instr,
                  SimpleExpression::Kind kind,
                  int idx,
                  std::unique_ptr<AtomicOp>& result) {
  auto dst = make_dst_var(instr.get_dst(0).get_reg(), idx);
  result = std::make_unique<SetVarOp>(dst, make_2reg_expr(instr, kind, idx), idx);
}

void make_2reg_op(const Instruction& instr,
                  SimpleExpression::Kind kind,
                  int idx,
                  std::unique_ptr<AtomicOp>& result) {
  auto dst = make_dst_var(instr.get_dst(0).get_reg(), idx);
  result = std::make_unique<SetVarOp>(dst, make_1reg_expr(instr, kind, idx), idx);
}

bool convert_or_1(const Instruction& i0, int idx, std::unique_ptr<AtomicOp>& result) {
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
  result = std::make_unique<SetVarOp>(dest, src, idx);
  return true;
}

bool convert_ori_1(const Instruction& i0, int idx, std::unique_ptr<AtomicOp>& result) {
  auto dest = make_dst_var(i0, idx);
  SimpleExpression src;
  if (i0.get_src(0).is_reg(rr0()) && i0.get_src(1).is_imm()) {
    // load a 16-bit integer constant
    // ori reg, r0, 1234
    src = SimpleAtom::make_int_constant(i0.get_src(1).get_imm()).as_expr();
  } else if (i0.get_src(1).is_imm()) {
    // logical or with constant integer
    // ori dst, a0, 1234
    src = SimpleExpression(SimpleExpression::Kind::OR, make_src_atom(i0.get_src(0).get_reg(), idx),
                           SimpleAtom::make_int_constant(i0.get_src(1).get_imm()));
  } else {
    return false;
  }
  result = std::make_unique<SetVarOp>(dest, src, idx);
  return true;
}

bool convert_mtc1_1(const Instruction& i0, int idx, std::unique_ptr<AtomicOp>& result) {
  // move from gpr to fpr
  make_2reg_op(i0, SimpleExpression::Kind::GPR_TO_FPR, idx, result);
  return true;
}

bool convert_mfc1_1(const Instruction& i0, int idx, std::unique_ptr<AtomicOp>& result) {
  // move from fpr to gpr
  make_2reg_op(i0, SimpleExpression::Kind::FPR_TO_GPR, idx, result);
  return true;
}

bool convert_and_1(const Instruction& i0, int idx, std::unique_ptr<AtomicOp>& result) {
  // and a0, a1, a2
  make_3reg_op(i0, SimpleExpression::Kind::AND, idx, result);
  return true;
}

bool convert_1(const Instruction& i0, int idx, std::unique_ptr<AtomicOp>& result) {
  switch (i0.kind) {
    case InstructionKind::OR:
      return convert_or_1(i0, idx, result);
    case InstructionKind::ORI:
      return convert_ori_1(i0, idx, result);
    case InstructionKind::AND:
      return convert_and_1(i0, idx, result);
    case InstructionKind::MTC1:
      return convert_mtc1_1(i0, idx, result);
    case InstructionKind::MFC1:
      return convert_mfc1_1(i0, idx, result);
    default:
      return false;
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
      if (convert_1(*instr, op_idx, op)) {
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