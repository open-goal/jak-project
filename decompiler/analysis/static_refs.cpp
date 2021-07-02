#include "static_refs.h"
#include "common/goos/PrettyPrinter.h"
#include "decompiler/Function/Function.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "decompiler/analysis/final_output.h"

namespace decompiler {
int insert_static_refs(Form* top_level_form,
                       FormPool& pool,
                       const Function& function,
                       const DecompilerTypeSystem&) {
  int replaced = 0;
  top_level_form->apply_form([&](Form* f) {
    auto atom = form_as_atom(f);
    if (atom && atom->is_static_addr()) {
      auto lab = function.ir2.env.file->labels.at(atom->label());
      auto& env = function.ir2.env;
      auto label_kv = env.label_types().find(lab.name);
      if (label_kv != env.label_types().end()) {
        if (label_kv->second.type_name == "_lambda_") {
          auto& file = env.file;
          auto other_func = file->try_get_function_at_label(atom->label());
          if (other_func) {
            std::vector<goos::Object> inline_body;
            other_func->ir2.top_form->inline_forms(inline_body, other_func->ir2.env);
            auto result = pretty_print::build_list(
                "lambda", get_arg_list_for_function(*other_func, other_func->ir2.env));
            pretty_print::append(result, pretty_print::build_list(inline_body));

            f->clear();
            f->push_back(pool.alloc_element<LambdaDefinitionElement>(result));
            replaced++;
          }
        }
      }
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
