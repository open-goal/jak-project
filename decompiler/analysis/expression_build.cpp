#include "expression_build.h"
#include "decompiler/Function/Function.h"
#include "decompiler/IR2/Form.h"
#include "decompiler/IR2/FormStack.h"
#include "decompiler/util/DecompilerTypeSystem.h"

namespace decompiler {
bool convert_to_expressions(Form* top_level_form,
                            FormPool& pool,
                            const Function& f,
                            const DecompilerTypeSystem& dts) {
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
      //      fmt::print("push {} to stack\n", entry->to_form(f.ir2.env).print());
      entry->push_to_stack(f.ir2.env, pool, stack);
    }
    std::vector<FormElement*> new_entries;
    if (f.type.last_arg() != TypeSpec("none")) {
      auto v0 = Register(Reg::GPR, Reg::V0);
      new_entries = stack.rewrite_to_get_reg(pool, v0, f.ir2.env);
      auto reg_return_type = f.ir2.env.get_types_after_op(f.ir2.atomic_ops->ops.size() - 1).get(v0);
      if (!dts.ts.typecheck(f.type.last_arg(), reg_return_type.typespec(), "", false, false)) {
        // we need to cast the final value.
        auto to_cast = new_entries.back();
        new_entries.pop_back();
        auto cast = pool.alloc_element<CastElement>(f.type.last_arg(),
                                                    pool.alloc_single_form(nullptr, to_cast));
        new_entries.push_back(cast);
      }
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
