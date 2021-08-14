#include "decompiler/IR2/GenericElementMatcher.h"
#include "final_output.h"
#include "decompiler/IR2/Form.h"
#include "common/goos/PrettyPrinter.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"

namespace decompiler {

goos::Object get_arg_list_for_function(const Function& func, const Env& env) {
  std::vector<goos::Object> argument_elts;
  if (func.type.arg_count() < 1) {
    throw std::runtime_error(
        fmt::format("Function {} has unknown type.\n", func.guessed_name.to_string()));
  }
  assert(func.type.arg_count() >= 1);
  for (size_t i = 0; i < func.type.arg_count() - 1; i++) {
    auto reg = Register(Reg::GPR, Reg::A0 + i);
    auto name = fmt::format("{}-0", reg.to_charp());
    argument_elts.push_back(
        pretty_print::build_list(env.remapped_name(name), func.type.get_arg(i).print()));
  }
  return pretty_print::build_list(argument_elts);
}

namespace {
void append_body_to_function_definition(goos::Object* top_form,
                                        const std::vector<goos::Object>& inline_body,
                                        const FunctionVariableDefinitions& var_dec,
                                        const TypeSpec& ts) {
  if (var_dec.local_vars) {
    pretty_print::append(*top_form, pretty_print::build_list(*var_dec.local_vars));
  }

  if (var_dec.had_pp && !ts.try_get_tag("behavior")) {
    std::vector<goos::Object> body_with_pp;
    body_with_pp.push_back(pretty_print::to_symbol("with-pp"));
    body_with_pp.insert(body_with_pp.end(), inline_body.begin(), inline_body.end());
    pretty_print::append(*top_form,
                         pretty_print::build_list(pretty_print::build_list(body_with_pp)));
  } else {
    pretty_print::append(*top_form, pretty_print::build_list(inline_body));
  }
}
}  // namespace

goos::Object final_output_lambda(const Function& func) {
  std::vector<goos::Object> inline_body;
  func.ir2.top_form->inline_forms(inline_body, func.ir2.env);
  auto var_dec = func.ir2.env.local_var_type_list(func.ir2.top_form, func.type.arg_count() - 1);

  auto behavior = func.type.try_get_tag("behavior");
  if (behavior) {
    auto result = pretty_print::build_list(fmt::format("lambda :behavior {}", *behavior),
                                           get_arg_list_for_function(func, func.ir2.env));
    append_body_to_function_definition(&result, inline_body, var_dec, func.type);
    return result;
  } else {
    auto result = pretty_print::build_list("lambda", get_arg_list_for_function(func, func.ir2.env));
    append_body_to_function_definition(&result, inline_body, var_dec, func.type);
    return result;
  }
}

std::string final_defun_out(const Function& func,
                            const Env& env,
                            const DecompilerTypeSystem& dts,
                            FunctionDefSpecials special_mode) {
  using pretty_print::append;
  std::vector<goos::Object> inline_body;
  try {
    func.ir2.top_form->inline_forms(inline_body, env);
  } catch (std::exception& e) {
    return e.what();
  }

  // int var_count = 0;
  auto var_dec = env.local_var_type_list(func.ir2.top_form, func.type.arg_count() - 1);
  auto arguments = get_arg_list_for_function(func, env);

  if (func.guessed_name.kind == FunctionName::FunctionKind::GLOBAL) {
    std::string def_name = "defun";
    if (special_mode == FunctionDefSpecials::DEFUN_DEBUG) {
      def_name = "defun-debug";
    } else {
      assert(special_mode == FunctionDefSpecials::NONE);
    }

    auto behavior = func.type.try_get_tag("behavior");
    if (behavior) {
      def_name = "defbehavior";
    }

    std::vector<goos::Object> top;
    top.push_back(pretty_print::to_symbol(def_name));

    if (behavior) {
      top.push_back(pretty_print::to_symbol(func.guessed_name.to_string() + " " + *behavior));
    } else {
      top.push_back(pretty_print::to_symbol(func.guessed_name.to_string()));
    }
    top.push_back(arguments);
    auto top_form = pretty_print::build_list(top);

    append_body_to_function_definition(&top_form, inline_body, var_dec, func.type);
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

    append_body_to_function_definition(&top_form, inline_body, var_dec, method_info.type);
    return pretty_print::to_string(top_form);
  }

  if (func.guessed_name.kind == FunctionName::FunctionKind::TOP_LEVEL_INIT) {
    assert(special_mode == FunctionDefSpecials::NONE);
    std::vector<goos::Object> top;
    top.push_back(pretty_print::to_symbol("top-level-function"));
    top.push_back(arguments);
    auto top_form = pretty_print::build_list(top);

    append_body_to_function_definition(&top_form, inline_body, var_dec, func.type);
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

    append_body_to_function_definition(&top_form, inline_body, var_dec, func.type);
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

std::string add_indent(const std::string& in, int indent, bool indent_first_line) {
  if (in.empty()) {
    return in;
  }

  std::string indent_str(indent, ' ');
  std::string result;

  char prev_char = indent_first_line ? '\n' : ' ';
  for (char c : in) {
    if (prev_char == '\n') {
      result += indent_str;
    }
    result += c;
    prev_char = c;
  }

  return result;
}

std::string write_from_top_level_form(Form* top_form,
                                      const DecompilerTypeSystem& dts,
                                      const LinkedObjectFile& file,
                                      const std::unordered_set<std::string>& skip_functions,
                                      const Env& env) {
  std::vector<FormElement*> forms = top_form->elts();
  assert(!forms.empty());

  // remove a (none) from the end, if it exists.
  auto back_as_generic_op = dynamic_cast<GenericElement*>(forms.back());
  if (back_as_generic_op && back_as_generic_op->op().is_fixed(FixedOperatorKind::NONE)) {
    forms.pop_back();
  }

  std::string result;
  // local vars:
  auto var_dec = env.local_var_type_list(top_form, 0);
  if (var_dec.local_vars) {
    result += pretty_print::to_string(*var_dec.local_vars);
    result += '\n';
    result += '\n';
  }

  // look for the whole thing being in a (when *debug-segment* ....)
  bool in_debug_only_file = false;
  if (forms.size() == 1) {
    auto as_cne = dynamic_cast<CondNoElseElement*>(forms.at(0));
    if (as_cne && as_cne->entries.size() == 1) {
      auto& entry = as_cne->entries.at(0);
      // a bit gross...
      if (entry.condition->to_string(env) == "*debug-segment*") {
        forms = entry.body->elts();
        result += ";; this file is debug only\n";
        result += "(when *debug-segment*\n";
        in_debug_only_file = true;
      }
    }
  }

  // look for the whole thing being in an rlet
  bool in_rlet = false;
  if (forms.size() == 1) {
    auto as_rlet = dynamic_cast<RLetElement*>(forms.at(0));
    if (as_rlet) {
      forms = as_rlet->body->elts();
      in_rlet = true;
      result += "(rlet ";
      result += add_indent(pretty_print::to_string(as_rlet->reg_list()), 6, false);
      result += '\n';
      if (as_rlet->needs_vf0_init()) {
        result += "(init-vf0-vector)\n";
      }
      result += '\n';
    }
  }

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

  // (set! sym-val <expr>)
  auto define_symbol_matcher = Matcher::set(Matcher::any_symbol(0), Matcher::any(1));

  // define-perm
  // (if (or (not <sym>) (zero? <sym>))
  //  (set! <sym> <init-val>)
  //  )
  auto define_perm_matcher = Matcher::if_no_else(
      Matcher::op(GenericOpMatcher::condition(IR2_Condition::Kind::TRUTHY),
                  {Matcher::or_expression(
                      {Matcher::op(GenericOpMatcher::condition(IR2_Condition::Kind::FALSE),
                                   {Matcher::any_symbol(0)}),
                       Matcher::op(GenericOpMatcher::condition(IR2_Condition::Kind::ZERO),
                                   {Matcher::any_symbol(1)})})}),
      Matcher::set(Matcher::any_symbol(2), Matcher::any(3)));

  for (auto& x : forms) {
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
        if (skip_functions.find(func->guessed_name.to_string()) == skip_functions.end()) {
          result += careful_function_to_string(func, dts);
        } else {
          result += ";; skipped.\n\n";
        }
      }
    }

    if (!something_matched) {
      auto method_match_result = match(method_def_matcher, &f);
      if (method_match_result.matched) {
        auto func = file.try_get_function_at_label(method_match_result.maps.label.at(method_label));
        if (func && func->guessed_name.kind == FunctionName::FunctionKind::METHOD) {
          something_matched = true;
          result +=
              fmt::format(";; definition for method {} of type {}\n", func->guessed_name.method_id,
                          method_match_result.maps.strings.at(type_name));
          if (skip_functions.find(func->guessed_name.to_string()) == skip_functions.end()) {
            result += careful_function_to_string(func, dts);
          } else {
            result += ";; skipped.\n\n";
          }
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
          result += "\n";
        } else {
          result += fmt::format(
              ";; type {} is defined here, but it is unknown to the decompiler\n\n", name);
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
            if (skip_functions.find(func->guessed_name.to_string()) == skip_functions.end()) {
              result += careful_function_to_string(func, dts, FunctionDefSpecials::DEFUN_DEBUG);
            } else {
              result += ";; skipped.\n\n";
            }
          }
        }
      }
    }

    if (!something_matched) {
      auto define_match_result = match(define_symbol_matcher, &f);
      if (define_match_result.matched) {
        something_matched = true;
        auto sym_name = define_match_result.maps.strings.at(0);
        auto symbol_type = dts.lookup_symbol_type(sym_name);
        result +=
            fmt::format(";; definition for symbol {}, type {}\n", sym_name, symbol_type.print());
        auto setset = dynamic_cast<SetFormFormElement*>(f.try_as_single_element());
        assert(setset);
        result += pretty_print::to_string(setset->to_form_for_define(env));
        result += "\n\n";
      }
    }

    if (!something_matched) {
      auto define_perm_match_result = match(define_perm_matcher, &f);
      if (define_perm_match_result.matched &&
          define_perm_match_result.maps.strings.at(0) ==
              define_perm_match_result.maps.strings.at(1) &&
          define_perm_match_result.maps.strings.at(0) ==
              define_perm_match_result.maps.strings.at(2)) {
        something_matched = true;
        auto sym_name = define_perm_match_result.maps.strings.at(0);
        auto symbol_type = dts.lookup_symbol_type(sym_name);

        result += fmt::format(";; definition (perm) for symbol {}, type {}\n", sym_name,
                              symbol_type.print());
        result += pretty_print::to_string(pretty_print::build_list(
            fmt::format("define-perm {} {}", sym_name, symbol_type.print()),
            define_perm_match_result.maps.forms.at(3)->to_form(env)));
        result += "\n\n";
      }
    }

    if (!something_matched) {
      auto as_cne = f.try_as_element<CondNoElseElement>();
      if (as_cne && as_cne->entries.size() == 1) {
        auto& entry = as_cne->entries.at(0);
        // a bit gross...
        if (entry.condition->to_string(env) == "*debug-segment*") {
          something_matched = true;
          // forms = entry.body->elts();
          result += ";; this part is debug only\n";
          result += "(when *debug-segment*\n";

          result += write_from_top_level_form(entry.body, dts, file, skip_functions, env);

          result += ")\n";
        }
      }
    }

    if (!something_matched) {
      auto empty = dynamic_cast<EmptyElement*>(x);
      if (empty) {
        something_matched = true;
      } else if (!x->active()) {
        something_matched = true;
      }
    }

    if (!something_matched) {
      result += ";; failed to figure out what this is:\n";
      result += pretty_print::to_string(x->to_form(env));
      result += "\n\n";
    }
  }

  if (in_debug_only_file) {
    result += ")\n";
  }

  if (in_rlet) {
    result += ")\n";
  }

  return result;
}

std::string write_from_top_level(const Function& top_level,
                                 const DecompilerTypeSystem& dts,
                                 const LinkedObjectFile& file,
                                 const std::unordered_set<std::string>& skip_functions) {
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

  return write_from_top_level_form(top_form, dts, file, skip_functions, env);
}
}  // namespace decompiler