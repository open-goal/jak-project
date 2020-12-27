#include "Function.h"
#include "decompiler/IR/IR.h"
#include "ExpressionStack.h"

namespace {
bool expressionize_begin(IR_Begin* begin, LinkedObjectFile& file) {
  ExpressionStack stack;
  // todo - this might need to run multiple times?
  for (auto& op : begin->forms) {
    op->expression_stack(stack, file);
  }
  begin->forms = stack.get_result();
  return true;
}
}  // namespace

bool Function::build_expression(LinkedObjectFile& file) {
  if (!ir) {
    return false;
  }

  try {
    // first we get a list of begins, which are where we can build up expressions.
    // we want to start with innermost begins because we'll probably need to do some fixing up
    // or more complicated analysis to do as good as possible on outer begins.
    auto all_children = ir->get_all_ir(file);
    std::vector<IR_Begin*> all_begins;

    // the top level may also be a begin
    auto as_begin = dynamic_cast<IR_Begin*>(ir.get());
    if (as_begin) {
      all_begins.push_back(as_begin);
    }

    for (auto& i : all_children) {
      auto child_as_begin = dynamic_cast<IR_Begin*>(i.get());
      if (child_as_begin) {
        all_begins.push_back(child_as_begin);
      }
    }

    // turn each begin into an expression
    for (auto b : all_begins) {
      if (!expressionize_begin(b, file)) {
        return false;
      }
    }
  } catch (std::exception& e) {
    printf("build_expression failed on %s due to %s\n", guessed_name.to_string().c_str(), e.what());
    return false;
  }

  return true;
}