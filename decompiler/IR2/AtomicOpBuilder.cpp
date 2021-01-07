#include "AtomicOpBuilder.h"
#include "common/log/log.h"
#include "decompiler/Function/BasicBlocks.h"
#include "decompiler/Function/Function.h"

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
  auto src0 = make_src_atom(instr.get_src(0).get_reg(), idx);
  auto src1 = make_src_atom(instr.get_src(1).get_reg(), idx);
  result = std::make_unique<SetVarOp>(dst, SimpleExpression(kind, src0, src1), idx);
}

bool convert_and_1(const Instruction& i0, int idx, std::unique_ptr<AtomicOp>& result) {
  // or reg, reg, reg:
  make_3reg_op(i0, SimpleExpression::Kind::AND, idx, result);
  return true;
}

bool convert_1(const Instruction& i0, int idx, std::unique_ptr<AtomicOp>& result) {
  switch (i0.kind) {
    case InstructionKind::AND:
      return convert_and_1(i0, idx, result);
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
      lg::die("Failed to convert instruction {} to an atomic op", instr->to_string(labels));
    }

    assert(converted && length && op);
    // add mappings:
    container->atomic_op_to_instruction[container->ops.size()] = begin_idx;
    for (int i = 0; i < length; i++) {
      container->instruction_to_basic_op[begin_idx + i] = container->ops.size();
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