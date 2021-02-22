#include "decompiler/IR2/GenericElementMatcher.h"
#include "final_output.h"
#include "decompiler/IR2/Form.h"
#include "common/goos/PrettyPrinter.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"

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

std::string final_defun_out(const Function& func,
                            const Env& env,
                            const DecompilerTypeSystem& dts,
                            FunctionDefSpecials special_mode) {
  std::vector<goos::Object> inline_body;
  func.ir2.top_form->inline_forms(inline_body, env);

  int var_count = 0;
  auto var_dec = env.local_var_type_list(func.ir2.top_form, func.type.arg_count() - 1, &var_count);

  std::vector<goos::Object> argument_elts;
  assert(func.type.arg_count() >= 1);
  for (size_t i = 0; i < func.type.arg_count() - 1; i++) {
    auto reg = Register(Reg::GPR, Reg::A0 + i);
    auto name = fmt::format("{}-0", reg.to_charp());
    argument_elts.push_back(
        pretty_print::build_list(env.remapped_name(name), func.type.get_arg(i).print()));
  }
  auto arguments = pretty_print::build_list(argument_elts);

  if (func.guessed_name.kind == FunctionName::FunctionKind::GLOBAL) {
    std::string def_name = "defun";
    if (special_mode == FunctionDefSpecials::DEFUN_DEBUG) {
      def_name = "defun-debug";
    } else {
      assert(special_mode == FunctionDefSpecials::NONE);
    }
    std::vector<goos::Object> top;
    top.push_back(pretty_print::to_symbol(def_name));
    top.push_back(pretty_print::to_symbol(func.guessed_name.to_string()));
    top.push_back(arguments);
    auto top_form = pretty_print::build_list(top);

    if (var_count > 0) {
      append(top_form, pretty_print::build_list(var_dec));
    }

    append(top_form, pretty_print::build_list(inline_body));
    return pretty_print::to_string(top_form);
  }

  if (func.guessed_name.kind == FunctionName::FunctionKind::METHOD) {
    assert(special_mode == FunctionDefSpecials::NONE);
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

    append(top_form, pretty_print::build_list(inline_body));
    return pretty_print::to_string(top_form);
  }

  if (func.guessed_name.kind == FunctionName::FunctionKind::TOP_LEVEL_INIT) {
    assert(special_mode == FunctionDefSpecials::NONE);
    std::vector<goos::Object> top;
    top.push_back(pretty_print::to_symbol("top-level-function"));
    top.push_back(arguments);
    auto top_form = pretty_print::build_list(top);

    if (var_count > 0) {
      append(top_form, pretty_print::build_list(var_dec));
    }

    append(top_form, pretty_print::build_list(inline_body));
    return pretty_print::to_string(top_form);
  }

  if (func.guessed_name.kind == FunctionName::FunctionKind::UNIDENTIFIED) {
    std::string def_name = "defun-anon";
    assert(special_mode == FunctionDefSpecials::NONE);
    std::vector<goos::Object> top;
    top.push_back(pretty_print::to_symbol(def_name));
    top.push_back(pretty_print::to_symbol(func.guessed_name.to_string()));
    top.push_back(arguments);
    auto top_form = pretty_print::build_list(top);

    if (var_count > 0) {
      append(top_form, pretty_print::build_list(var_dec));
    }

    append(top_form, pretty_print::build_list(inline_body));
    return pretty_print::to_string(top_form);
  }
  return "nyi";
}

namespace {
std::string careful_function_to_string(
    const Function* func,
    const DecompilerTypeSystem& dts,
    FunctionDefSpecials special_mode = FunctionDefSpecials::NONE) {
  auto& env = func->ir2.env;

  std::string result;
  if (func->warnings.has_warnings()) {
    result += func->warnings.get_warning_text(true);
  }

  if (!func->ir2.top_form) {
    return ";; ERROR: function was not converted to expressions. Cannot decompile.\n\n";
  }
  if (!env.has_type_analysis()) {
    return ";; ERROR: function has no type analysis. Cannot decompile.\n\n";
  }

  if (!env.has_local_vars()) {
    return ";; ERROR: function has no local vars. Cannot decompile.\n\n";
  }

  if (!env.has_reg_use()) {
    return ";; ERROR: function has no register use analysis. Cannot decompile.\n\n";
  }

  result += final_defun_out(*func, func->ir2.env, dts, special_mode) + "\n\n";
  return result;
}
}  // namespace

std::string write_from_top_level(const Function& top_level,
                                 const DecompilerTypeSystem& dts,
                                 const LinkedObjectFile& file) {
  auto top_form = top_level.ir2.top_form;
  if (!top_form) {
    return ";; ERROR: top level function was not converted to expressions. Cannot decompile.\n\n";
  }

  auto& env = top_level.ir2.env;
  if (!env.has_type_analysis()) {
    return ";; ERROR: top level has no type analysis. Cannot decompile.\n\n";
  }

  if (!env.has_local_vars()) {
    return ";; ERROR: top level has no local vars. Cannot decompile.\n\n";
  }

  if (!env.has_reg_use()) {
    return ";; ERROR: top level has no register use analysis. Cannot decompile.\n\n";
  }

  std::string result;

  // (set! identity L312)
  constexpr int func_name = 1;
  constexpr int label = 2;
  Matcher function_def_matcher =
      Matcher::set(Matcher::any_symbol(func_name), Matcher::any_label(label));

  // (method-set! vec4s 3 L352)
  constexpr int type_name = 1;
  //  constexpr int method_id = 2;
  constexpr int method_label = 3;
  Matcher method_def_matcher = Matcher::op(
      GenericOpMatcher::func(Matcher::symbol("method-set!")),
      {Matcher::any_symbol(type_name), Matcher::integer({}), Matcher::any_label(method_label)});

  // (type-new 'vec4s uint128 (the-as int (l.d L366)))
  Matcher deftype_matcher =
      Matcher::op_with_rest(GenericOpMatcher::fixed(FixedOperatorKind::TYPE_NEW),
                            {Matcher::any_quoted_symbol(type_name)});

  // (if *debug-segment* (set! mem-print L347) (set! mem-print nothing))
  auto debug_seg_matcher = Matcher::op(GenericOpMatcher::condition(IR2_Condition::Kind::TRUTHY),
                                       {Matcher::symbol("*debug-segment*")});
  auto debug_def_matcher = Matcher::set(Matcher::any_symbol(0), Matcher::any_label(1));
  auto non_debug_def_matcher = Matcher::set(Matcher::any_symbol(2), Matcher::symbol("nothing"));
  auto defun_debug_matcher =
      Matcher::if_with_else(debug_seg_matcher, debug_def_matcher, non_debug_def_matcher);

  for (auto& x : top_form->elts()) {
    bool something_matched = false;
    Form f;
    f.elts().push_back(x);
    auto global_match_result = match(function_def_matcher, &f);
    if (global_match_result.matched) {
      auto func = file.try_get_function_at_label(global_match_result.maps.label.at(label));
      if (func) {
        something_matched = true;
        result += fmt::format(";; definition for function {}\n",
                              global_match_result.maps.strings.at(func_name));
        result += careful_function_to_string(func, dts);
      }
    }

    if (!something_matched) {
      auto method_match_result = match(method_def_matcher, &f);
      if (method_match_result.matched) {
        auto func = file.try_get_function_at_label(method_match_result.maps.label.at(method_label));
        if (func) {
          something_matched = true;
          result += fmt::format(";; definition for method of type {}\n",
                                method_match_result.maps.strings.at(type_name));
          result += careful_function_to_string(func, dts);
        }
      }
    }

    if (!something_matched) {
      auto deftype_match_result = match(deftype_matcher, &f);
      if (deftype_match_result.matched) {
        auto& name = deftype_match_result.maps.strings.at(type_name);
        if (dts.ts.fully_defined_type_exists(name)) {
          result += fmt::format(";; definition of type {}\n", name);
          result += dts.ts.generate_deftype(dts.ts.lookup_type(name));
          result += "\n\n";
        } else {
          result +=
              fmt::format(";; type {} defintion, but it is unknown to the decompiler\n\n", name);
        }
        something_matched = true;
      }
    }

    if (!something_matched) {
      auto debug_match_result = match(defun_debug_matcher, &f);
      if (debug_match_result.matched) {
        auto first_name = debug_match_result.maps.strings.at(0);
        auto second_name = debug_match_result.maps.strings.at(2);
        if (first_name == second_name) {
          auto func = file.try_get_function_at_label(debug_match_result.maps.label.at(1));
          if (func) {
            something_matched = true;
            result += fmt::format(";; definition (debug) for function {}\n",
                                  debug_match_result.maps.strings.at(0));
            result += careful_function_to_string(func, dts, FunctionDefSpecials::DEFUN_DEBUG);
          }
        }
      }
    }

    if (!something_matched) {
      result += ";; failed to figure out what this is:\n";
      result += pretty_print::to_string(x->to_form(env));
      result += "\n\n";
    }
  }

  return result;
}
}  // namespace decompiler