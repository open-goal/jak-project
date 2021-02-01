#include "final_output.h"
#include "decompiler/IR2/Form.h"
#include "common/goos/PrettyPrinter.h"
#include "decompiler/util/DecompilerTypeSystem.h"

namespace decompiler {

namespace {
void append(goos::Object& _in, const goos::Object& add) {
  auto* in = &_in;
  while (in->is_pair() && !in->as_pair()->cdr.is_empty_list()) {
    in = &in->as_pair()->cdr;
  }

  if (!in->is_pair()) {
    assert(false);  // invalid list
  }
  in->as_pair()->cdr = add;
}
}  // namespace

std::string final_defun_out(const Function& func, const Env& env, const DecompilerTypeSystem& dts) {
  auto code_body = func.ir2.top_form->to_form(env);

  int var_count = 0;
  auto var_dec = env.local_var_type_list(func.ir2.top_form, func.type.arg_count() - 1, &var_count);

  std::vector<goos::Object> argument_elts;
  assert(func.type.arg_count() >= 1);
  for (size_t i = 0; i < func.type.arg_count() - 1; i++) {
    argument_elts.push_back(
        pretty_print::build_list(fmt::format("a{}-0", i), func.type.get_arg(i).print()));
  }
  auto arguments = pretty_print::build_list(argument_elts);

  if (func.guessed_name.kind == FunctionName::FunctionKind::GLOBAL) {
    std::vector<goos::Object> top;
    top.push_back(pretty_print::to_symbol("defun"));
    top.push_back(pretty_print::to_symbol(func.guessed_name.to_string()));
    top.push_back(arguments);
    auto top_form = pretty_print::build_list(top);

    if (var_count > 0) {
      append(top_form, pretty_print::build_list(var_dec));
    }

    append(top_form, pretty_print::build_list(code_body));
    return pretty_print::to_string(top_form);
  }

  if (func.guessed_name.kind == FunctionName::FunctionKind::METHOD) {
    std::vector<goos::Object> top;
    top.push_back(pretty_print::to_symbol("defmethod"));
    auto method_info =
        dts.ts.lookup_method(func.guessed_name.type_name, func.guessed_name.method_id);
    top.push_back(pretty_print::to_symbol(method_info.name));
    top.push_back(pretty_print::to_symbol(func.guessed_name.type_name));
    top.push_back(arguments);
    auto top_form = pretty_print::build_list(top);

    if (var_count > 0) {
      append(top_form, pretty_print::build_list(var_dec));
    }

    append(top_form, pretty_print::build_list(code_body));
    return pretty_print::to_string(top_form);
  }

  if (func.guessed_name.kind == FunctionName::FunctionKind::TOP_LEVEL_INIT) {
    std::vector<goos::Object> top;
    top.push_back(pretty_print::to_symbol("top-level-function"));
    top.push_back(arguments);
    auto top_form = pretty_print::build_list(top);

    if (var_count > 0) {
      append(top_form, pretty_print::build_list(var_dec));
    }

    append(top_form, pretty_print::build_list(code_body));
    return pretty_print::to_string(top_form);
  }
  return "nyi";
}
}  // namespace decompiler