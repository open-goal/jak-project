#include "BasicOpBuilder.h"
#include "decompiler/Function/Function.h"
#include "decompiler/Function/BasicBlocks.h"
#include "decompiler/Disasm/InstructionMatching.h"

namespace {

struct ConvertState {
  void reset() {}
};

std::shared_ptr<BasicOp> try_or_reg_move(Instruction& instr, int idx) {
  if (is_gpr_3(instr, InstructionKind::OR, {}, {}, make_gpr(Reg::R0))) {
    return std::make_shared<RegRegMove>(BasicOpArg(instr.get_dst(0).get_reg(), idx),
                                        BasicOpArg(instr.get_src(0).get_reg(), idx),
                                        RegRegMove::GPR64_GPR64);
  }
  return nullptr;
}

}  // namespace

void add_basic_ops_to_block(Function* func, const BasicBlock& block, LinkedObjectFile* file) {
  ConvertState state;

  for (int instr = block.start_word; instr < block.end_word; instr++) {
    auto& i = func->instructions.at(instr);

    std::shared_ptr<BasicOp> result = nullptr;
    for (auto x : {try_or_reg_move}) {
      result = x(i, instr);
      if (result) {
        break;
      }
    }

    // everything failed
    if (!result) {
      state.reset();
      func->add_basic_op(std::make_shared<FailedBasicOp>(), instr, instr + 1);
    } else {
      func->add_basic_op(result, instr, instr + 1);
    }

    // temp hack for debug:
    printf("Instruction -> BasicOp failed on %s\n", i.to_string(*file).c_str());
  }
}
