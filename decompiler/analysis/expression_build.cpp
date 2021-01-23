#include "expression_build.h"
#include "decompiler/Function/Function.h"
#include "decompiler/IR2/Form.h"
#include "decompiler/IR2/FormStack.h"

namespace decompiler {
bool convert_to_expressions(Form* top_level_form, FormPool& pool, const Function& f) {
  assert(top_level_form);

  try {
    top_level_form->apply_form([&](Form* form) {
      FormStack stack;
      for (auto& entry : form->elts()) {
        entry->push_to_stack(f.ir2.env, stack);
      }
      auto new_entries = stack.rewrite(pool);
      form->clear();
      for (auto x : new_entries) {
        form->push_back(x);
      }
    });
  } catch (std::exception& e) {
    return false;
  }
  return true;
}
}  // namespace decompiler
