#include "expression_build.h"
#include "common/goos/PrettyPrinter.h"
#include "common/log/log.h"
#include "decompiler/Function/Function.h"
#include "decompiler/IR2/Form.h"
#include "decompiler/IR2/FormStack.h"
#include "decompiler/util/DecompilerTypeSystem.h"

namespace decompiler {

/*!
 * The main expression building pass.
 */
bool convert_to_expressions(
    Form* top_level_form,
    FormPool& pool,
    Function& f,
    const std::vector<std::string>& arg_names,
    const std::unordered_map<std::string, LocalVarOverride>& var_override_map,
    const DecompilerTypeSystem& dts) {
  ASSERT(top_level_form);

  // set argument names to some reasonable defaults. these will be used if the user doesn't
  // give us anything more specific.
  if (f.guessed_name.kind == FunctionName::FunctionKind::GLOBAL ||
      f.guessed_name.kind == FunctionName::FunctionKind::UNIDENTIFIED ||
      f.guessed_name.kind == FunctionName::FunctionKind::NV_STATE ||
      f.guessed_name.kind == FunctionName::FunctionKind::V_STATE) {
    f.ir2.env.set_remap_for_function(f);
  } else if (f.guessed_name.kind == FunctionName::FunctionKind::METHOD) {
    auto method_type =
        dts.ts.lookup_method(f.guessed_name.type_name, f.guessed_name.method_id).type;
    if (f.guessed_name.method_id == GOAL_NEW_METHOD) {
      f.ir2.env.set_remap_for_new_method(method_type);
    } else {
      f.ir2.env.set_remap_for_method(method_type);
    }
  }

  // get variable names from the user.
  f.ir2.env.map_args_from_config(arg_names, var_override_map);

  // convert to typespec
  for (auto& info : f.ir2.env.stack_slot_entries) {
    auto rename = f.ir2.env.var_remap_map().find(info.second.name());
    if (rename != f.ir2.env.var_remap_map().end()) {
      info.second.name_override = rename->second;
    }
    //     debug
    // lg::print("STACK {} : {} ({})\n", info.first, info.second.typespec.print(),
    //         info.second.tp_type.print());
  }

  // override variable types from the user.
  std::unordered_map<std::string, TypeSpec> retype;
  for (auto& remap : var_override_map) {
    if (remap.second.type) {
      retype[remap.first] = dts.parse_type_spec(*remap.second.type);
    }
  }
  f.ir2.env.set_retype_map(retype);

  try {
    // create the root expression stack for the function
    FormStack stack(true);
    // and add all entries
    for (auto& entry : top_level_form->elts()) {
      entry->push_to_stack(f.ir2.env, pool, stack);
    }

    // rewrite the stack to get the correct final value
    std::vector<FormElement*> new_entries;
    if (f.type.last_arg() != TypeSpec("none")) {
      auto return_var = f.ir2.atomic_ops->end_op().return_var();
      new_entries = rewrite_to_get_var(stack, pool, return_var, f.ir2.env);
      TypeSpec return_type = f.ir2.env.get_types_after_op(f.ir2.atomic_ops->ops.size() - 1)
                                 .get(return_var.reg())
                                 .typespec();
      auto back_as_atom = form_element_as_atom(new_entries.back());
      if (back_as_atom && back_as_atom->is_var()) {
        return_type = f.ir2.env.get_variable_type(back_as_atom->var(), true);
        auto var_cast = f.ir2.env.get_variable_and_cast(back_as_atom->var());
        if (var_cast.cast) {
          return_type = *var_cast.cast;
        }
      }

      bool needs_cast = false;
      if (!dts.ts.tc(f.type.last_arg(), return_type)) {
        // we need to cast the final value.
        needs_cast = true;

      } else {
        bool found_early_return = false;
        for (auto e : new_entries) {
          e->apply([&](FormElement* elt) {
            auto as_ret = dynamic_cast<ReturnElement*>(elt);
            if (as_ret) {
              found_early_return = true;
            }
          });
          if (found_early_return) {
            break;
          }
        }

        if (!found_early_return && f.type.last_arg() != return_type) {
          needs_cast = true;
        }
      }

      if (needs_cast) {
        auto to_cast = new_entries.back();
        auto as_cast = dynamic_cast<CastElement*>(to_cast);
        if (as_cast) {
          as_cast->set_type(f.type.last_arg());
        } else {
          new_entries.pop_back();
          auto cast = pool.alloc_element<CastElement>(f.type.last_arg(),
                                                      pool.alloc_single_form(nullptr, to_cast));
          new_entries.push_back(cast);
        }
      }
    } else {
      // or just get all the expressions
      new_entries = stack.rewrite(pool, f.ir2.env);
      new_entries.push_back(
          pool.alloc_element<GenericElement>(GenericOperator::make_fixed(FixedOperatorKind::NONE)));
    }

    // if we are a totally empty function, insert a placeholder so we don't have to handle
    // the zero element case ever.
    if (new_entries.empty()) {
      new_entries.push_back(pool.alloc_element<EmptyElement>());
    }

    // turn us back into a form.
    top_level_form->clear();
    for (auto x : new_entries) {
      top_level_form->push_back(x);
    }

    // and sanity check for tree errors.
    for (auto x : top_level_form->elts()) {
      ASSERT(x->parent_form == top_level_form);
    }

    // if we were don't return, make sure we didn't find a return form.
    if (f.type.last_arg() == TypeSpec("none")) {
      bool found_return = false;
      top_level_form->apply([&](FormElement* elt) {
        if (dynamic_cast<ReturnElement*>(elt)) {
          found_return = true;
        }
      });

      if (found_return) {
        auto warn = fmt::format(
            "Function {} has a return type of none, but the expression builder found a return "
            "statement.",
            f.name());
        f.warnings.warning(warn);
        lg::warn(warn);
      }
    }

  } catch (std::exception& e) {
    f.warnings.error("Expression building failed: In {}: {}", f.name(), e.what());
    lg::warn("In {}: {}", f.name(), e.what());
    return false;
  }

  return true;
}
}  // namespace decompiler
