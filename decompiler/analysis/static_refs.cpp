#include "static_refs.h"
#include "common/goos/PrettyPrinter.h"
#include "decompiler/Function/Function.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "decompiler/analysis/final_output.h"

namespace decompiler {

namespace {
bool try_convert_lambda(const Function& parent_function,
                        FormPool& pool,
                        Form* f,
                        bool defstate_behavior) {
  auto atom = form_as_atom(f);
  if (atom && atom->is_static_addr()) {
    auto lab = parent_function.ir2.env.file->labels.at(atom->label());
    auto& env = parent_function.ir2.env;
    auto label_kv = env.label_types().find(lab.name);
    if (label_kv != env.label_types().end()) {
      if (label_kv->second.type_name == "_lambda_") {
        auto& file = env.file;
        auto other_func = file->try_get_function_at_label(atom->label());
        if (other_func) {
          goos::Object result;
          if (defstate_behavior) {
            result = final_output_defstate_anonymous_behavior(*other_func);
          } else {
            result = final_output_lambda(*other_func);
          }

          f->clear();
          f->push_back(pool.alloc_element<LambdaDefinitionElement>(result));
          return true;
        }
      }
    }
  }
  return false;
}
}  // namespace

int insert_static_refs(Form* top_level_form,
                       FormPool& pool,
                       const Function& function,
                       const DecompilerTypeSystem&) {
  int replaced = 0;

  // first, look for defstates and lambdas to behaviors.
  top_level_form->apply([&](FormElement* fe) {
    auto as_defstate = dynamic_cast<DefstateElement*>(fe);
    if (as_defstate) {
      for (auto& e : as_defstate->entries()) {
        if (e.is_behavior) {
          if (try_convert_lambda(function, pool, e.val, true)) {
            replaced++;
          }
        }
      }
    }
  });

  // next, all the rest.
  top_level_form->apply_form([&](Form* f) {
    if (try_convert_lambda(function, pool, f, false)) {
      replaced++;
    }
  });

  top_level_form->apply([&](FormElement* fe) {
    auto as_static_data = dynamic_cast<DecompiledDataElement*>(fe);
    if (as_static_data) {
      as_static_data->do_decomp(function.ir2.env, function.ir2.env.file);
    }
  });
  return replaced;
}
}  // namespace decompiler
