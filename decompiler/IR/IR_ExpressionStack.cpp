#include "IR.h"
#include "decompiler/Function/ExpressionStack.h"

bool IR_Set_Atomic::expression_stack(ExpressionStack& stack, LinkedObjectFile& file) {
  // first determine the type of the set.
  switch (kind) {
    case IR_Set::REG_64:
    case IR_Set::LOAD:
    case IR_Set::GPR_TO_FPR:  // TODO - this should probably not be invisible.
    case IR_Set::FPR_TO_GPR64:
    case IR_Set::REG_FLT: {
      // normal 64-bit GPR set!
      // first, we update our source to substitute in more complicated expressions.
      auto src_as_reg = dynamic_cast<IR_Register*>(src.get());
      if (src_as_reg) {
        // an annoying special case.
        if (consumed.find(src_as_reg->reg) != consumed.end()) {
          // we consume it.
          src = stack.get(src_as_reg->reg);
        }
      } else {
        src->update_from_stack(consumed, stack, file);
      }

      // next, we tell the stack the value of the register we just set
      auto dest_reg = dynamic_cast<IR_Register*>(dst.get());
      assert(dest_reg);
      stack.set(dest_reg->reg, src);
      return true;
    }

    break;
    default:
      throw std::runtime_error("IR_Set_Atomic::expression_stack NYI for " + print(file));
  }
}

bool IR_Load::update_from_stack(const std::unordered_set<Register, Register::hash>& consume,
                                ExpressionStack& stack,
                                LinkedObjectFile& file) {
  return location->update_from_stack(consume, stack, file);
}

bool IR_StaticAddress::update_from_stack(
    const std::unordered_set<Register, Register::hash>& consume,
    ExpressionStack& stack,
    LinkedObjectFile& file) {
  (void)consume;
  (void)stack;
  (void)file;
  return true;
}

bool IR_FloatMath2::update_from_stack(const std::unordered_set<Register, Register::hash>& consume,
                                      ExpressionStack& stack,
                                      LinkedObjectFile& file) {
  if (kind == DIV) {
    for (auto reg : {&arg1, &arg0}) {
      auto as_reg = dynamic_cast<IR_Register*>(reg->get());
      if (as_reg) {
        if (consume.find(as_reg->reg) != consume.end()) {
          *reg = stack.get(as_reg->reg);
        }
      } else {
        (*reg)->update_from_stack(consume, stack, file);
      }
    }
  } else {
    for (auto reg : {&arg0, &arg1}) {
      auto as_reg = dynamic_cast<IR_Register*>(reg->get());
      if (as_reg) {
        if (consume.find(as_reg->reg) != consume.end()) {
          *reg = stack.get(as_reg->reg);
        }
      } else {
        (*reg)->update_from_stack(consume, stack, file);
      }
    }
  }

  return true;
}