#include "expression_build.h"
#include "decompiler/Function/Function.h"
#include "decompiler/IR2/Form.h"
#include "decompiler/IR2/FormStack.h"

namespace decompiler {
bool convert_to_expressions(Form* top_level_form, FormPool& pool, const Function& f) {
  assert(top_level_form);

  try {
    //    top_level_form->apply_form([&](Form* form) {
    //      if (form == top_level_form || !form->is_single_element()) {
    //        FormStack stack;
    //        for (auto& entry : form->elts()) {
    //          fmt::print("push {} to stack\n", entry->to_form(f.ir2.env).print());
    //          entry->push_to_stack(f.ir2.env, pool, stack);
    //        }
    //        std::vector<FormElement*> new_entries;
    //        if (form == top_level_form && f.type.last_arg() != TypeSpec("none")) {
    //          new_entries = stack.rewrite_to_get_reg(pool, Register(Reg::GPR, Reg::V0));
    //        } else {
    //          new_entries = stack.rewrite(pool);
    //        }
    //        assert(!new_entries.empty());
    //        form->clear();
    //        for (auto x : new_entries) {
    //          form->push_back(x);
    //        }
    //      }
    //    });

    FormStack stack;
    for (auto& entry : top_level_form->elts()) {
      fmt::print("push {} to stack\n", entry->to_form(f.ir2.env).print());
      entry->push_to_stack(f.ir2.env, pool, stack);
    }
    std::vector<FormElement*> new_entries;
    if (f.type.last_arg() != TypeSpec("none")) {
      new_entries = stack.rewrite_to_get_reg(pool, Register(Reg::GPR, Reg::V0));
    } else {
      new_entries = stack.rewrite(pool);
    }
    assert(!new_entries.empty());
    top_level_form->clear();
    for (auto x : new_entries) {
      top_level_form->push_back(x);
    }
  } catch (std::exception& e) {
    lg::warn("Expression building failed: {}", e.what());
    return false;
  }
  return true;
}
}  // namespace decompiler
