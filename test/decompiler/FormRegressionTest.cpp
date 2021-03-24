#include "FormRegressionTest.h"

#include "decompiler/analysis/variable_naming.h"
#include "decompiler/analysis/reg_usage.h"
#include "decompiler/analysis/cfg_builder.h"
#include "decompiler/analysis/expression_build.h"
#include "decompiler/analysis/final_output.h"
#include "decompiler/analysis/insert_lets.h"
#include "common/goos/PrettyPrinter.h"
#include "common/util/json_util.h"
#include "decompiler/IR2/Form.h"
#include "third-party/json.hpp"

using namespace decompiler;

void FormRegressionTest::SetUpTestCase() {
  parser = std::make_unique<InstructionParser>();
  dts = std::make_unique<DecompilerTypeSystem>();
  dts->parse_type_defs({"decompiler", "config", "all-types.gc"});
}

void FormRegressionTest::TearDownTestCase() {
  parser.reset();
  dts.reset();
  parser.reset();
}

void FormRegressionTest::TestData::add_string_at_label(const std::string& label_name,
                                                       const std::string& data) {
  // first, align segment 1:
  while (file.words_by_seg.at(1).size() % 4) {
    file.words_by_seg.at(1).push_back(LinkedWord(0));
  }

  // add string type tag:
  LinkedWord type_tag(0);
  type_tag.kind = LinkedWord::Kind::TYPE_PTR;
  type_tag.symbol_name = "string";
  file.words_by_seg.at(1).push_back(type_tag);
  int string_start = 4 * int(file.words_by_seg.at(1).size());

  // add size
  file.words_by_seg.at(1).push_back(LinkedWord(int(data.length())));

  // add string:
  std::vector<char> bytes;
  bytes.resize(((data.size() + 1 + 3) / 4) * 4);
  for (size_t i = 0; i < data.size(); i++) {
    bytes[i] = data[i];
  }
  for (size_t i = 0; i < bytes.size() / 4; i++) {
    auto word = ((uint32_t*)bytes.data())[i];
    file.words_by_seg.at(1).push_back(LinkedWord(word));
  }
  for (int i = 0; i < 3; i++) {
    file.words_by_seg.at(1).push_back(LinkedWord(0));
  }
  // will be already null terminated.

  for (auto& label : file.labels) {
    if (label.name == label_name) {
      label.target_segment = 1;
      label.offset = string_start;
      return;
    }
  }

  EXPECT_TRUE(false);
}

std::pair<std::vector<std::string>, std::unordered_map<std::string, LocalVarOverride>>
parse_var_json(const std::string& str) {
  if (str.empty()) {
    return {};
  }

  std::vector<std::string> args;
  std::unordered_map<std::string, LocalVarOverride> var_overrides;

  auto j = parse_commented_json(str);

  auto arg = j.find("args");
  if (arg != j.end()) {
    for (auto& x : arg.value()) {
      args.push_back(x);
    }
  }

  auto var = j.find("vars");
  if (var != j.end()) {
    for (auto& vkv : var->get<std::unordered_map<std::string, nlohmann::json>>()) {
      LocalVarOverride override;
      if (vkv.second.is_string()) {
        override.name = vkv.second.get<std::string>();
      } else if (vkv.second.is_array()) {
        override.name = vkv.second[0].get<std::string>();
        override.type = vkv.second[1].get<std::string>();
      } else {
        throw std::runtime_error("Invalid function var override.");
      }
      var_overrides[vkv.first] = override;
    }
  }

  return {args, var_overrides};
}

std::unique_ptr<FormRegressionTest::TestData> FormRegressionTest::make_function(
    const std::string& code,
    const TypeSpec& function_type,
    bool do_expressions,
    bool allow_pairs,
    const std::string& method_name,
    const std::vector<std::pair<std::string, std::string>>& strings,
    const std::unordered_map<int, std::vector<TypeCast>>& casts,
    const std::string& var_map_json) {
  dts->type_prop_settings.locked = true;
  dts->type_prop_settings.reset();
  dts->type_prop_settings.allow_pair = allow_pairs;
  dts->type_prop_settings.current_method_type = method_name;

  std::vector<std::string> string_label_names;
  for (auto& x : strings) {
    string_label_names.push_back(x.first);
  }
  auto program = parser->parse_program(code, string_label_names);
  //  printf("prg:\n%s\n\n", program.print().c_str());
  auto test = std::make_unique<TestData>(program.instructions.size());
  test->file.words_by_seg.resize(3);
  test->file.labels = program.labels;
  test->func.ir2.env.file = &test->file;
  test->func.ir2.env.dts = dts.get();
  test->func.instructions = program.instructions;
  test->func.guessed_name.set_as_global("test-function");
  test->func.type = function_type;

  for (auto& str : strings) {
    test->add_string_at_label(str.first, str.second);
  }

  test->func.basic_blocks = find_blocks_in_function(test->file, 0, test->func);
  test->func.analyze_prologue(test->file);
  test->func.cfg = build_cfg(test->file, 0, test->func);
  EXPECT_TRUE(test->func.cfg->is_fully_resolved());
  if (!test->func.cfg->is_fully_resolved()) {
    fmt::print("CFG:\n{}\n", test->func.cfg->to_dot());
  }

  auto ops = convert_function_to_atomic_ops(test->func, program.labels);
  test->func.ir2.atomic_ops = std::make_shared<FunctionAtomicOps>(std::move(ops));
  test->func.ir2.atomic_ops_succeeded = true;
  test->func.ir2.env.set_end_var(test->func.ir2.atomic_ops->end_op().return_var());

  EXPECT_TRUE(test->func.run_type_analysis_ir2(function_type, *dts, test->file, casts, {}));
  test->func.ir2.env.types_succeeded = true;

  test->func.ir2.env.set_reg_use(analyze_ir2_register_usage(test->func));

  auto result = run_variable_renaming(test->func, test->func.ir2.env.reg_use(),
                                      *test->func.ir2.atomic_ops, *dts);
  if (result.has_value()) {
    test->func.ir2.env.set_local_vars(*result);
  } else {
    EXPECT_TRUE(false);
  }

  build_initial_forms(test->func);
  EXPECT_TRUE(test->func.ir2.top_form);

  // for now, just test that this can at least be called.
  if (test->func.ir2.top_form) {
    RegAccessSet vars;
    test->func.ir2.top_form->collect_vars(vars, true);

    if (do_expressions) {
      auto config = parse_var_json(var_map_json);
      bool success = convert_to_expressions(test->func.ir2.top_form, *test->func.ir2.form_pool,
                                            test->func, config.first, config.second, *dts);

      EXPECT_TRUE(success);
      if (!success) {
        return nullptr;
      }
      insert_lets(test->func, test->func.ir2.env, *test->func.ir2.form_pool,
                  test->func.ir2.top_form);
    }
  }

  //  for (int i = 0; i < int(test->func.ir2.atomic_ops->ops.size()); i++) {
  //    auto& op = test->func.ir2.atomic_ops->ops.at(i);
  //    auto& info = test->func.ir2.env.reg_use().op.at(i);
  //    fmt::print("{} - {}:  ", op->to_string(test->func.ir2.env),
  //               test->func.ir2.env.get_types_after_op(i).print_gpr_masked(
  //                   regs_to_gpr_mask({Register(Reg::GPR, Reg::V0)})));
  //    for (auto live : info.live) {
  //      fmt::print("{} ", live.to_charp());
  //    }
  //    fmt::print("\n");
  //  }

  return test;
}

void FormRegressionTest::test(const std::string& code,
                              const std::string& type,
                              const std::string& expected,
                              bool do_expressions,
                              bool allow_pairs,
                              const std::string& method_name,
                              const std::vector<std::pair<std::string, std::string>>& strings,
                              const std::unordered_map<int, std::vector<TypeCast>>& casts,
                              const std::string& var_map_json) {
  auto ts = dts->parse_type_spec(type);
  auto test = make_function(code, ts, do_expressions, allow_pairs, method_name, strings, casts,
                            var_map_json);
  ASSERT_TRUE(test);
  auto expected_form =
      pretty_print::get_pretty_printer_reader().read_from_string(expected, false).as_pair()->car;
  ASSERT_TRUE(test->func.ir2.top_form);
  auto actual_form =
      pretty_print::get_pretty_printer_reader()
          .read_from_string(test->func.ir2.top_form->to_form(test->func.ir2.env).print(), false)
          .as_pair()
          ->car;
  if (expected_form != actual_form) {
    printf("Got:\n%s\n\nExpected\n%s\n", pretty_print::to_string(actual_form).c_str(),
           pretty_print::to_string(expected_form).c_str());
  }

  EXPECT_TRUE(expected_form == actual_form);
}

void FormRegressionTest::test_final_function(
    const std::string& code,
    const std::string& type,
    const std::string& expected,
    bool allow_pairs,
    const std::vector<std::pair<std::string, std::string>>& strings,
    const std::unordered_map<int, std::vector<decompiler::TypeCast>>& casts,
    const std::string& var_map_json) {
  auto ts = dts->parse_type_spec(type);
  auto test = make_function(code, ts, true, allow_pairs, "", strings, casts, var_map_json);
  ASSERT_TRUE(test);
  auto expected_form =
      pretty_print::get_pretty_printer_reader().read_from_string(expected, false).as_pair()->car;
  ASSERT_TRUE(test->func.ir2.top_form);
  auto final = final_defun_out(test->func, test->func.ir2.env, *dts);
  auto actual_form =
      pretty_print::get_pretty_printer_reader().read_from_string(final, false).as_pair()->car;
  if (expected_form != actual_form) {
    printf("Got:\n%s\n\nExpected\n%s\n", pretty_print::to_string(actual_form).c_str(),
           pretty_print::to_string(expected_form).c_str());
  }

  EXPECT_TRUE(expected_form == actual_form);
}

std::unordered_map<int, std::vector<decompiler::TypeCast>> FormRegressionTest::parse_cast_json(
    const std::string& in) {
  std::unordered_map<int, std::vector<decompiler::TypeCast>> out;
  auto casts = nlohmann::json::parse(in);

  for (auto& cast : casts) {
    auto idx_range = parse_json_optional_integer_range(cast.at(0));
    for (auto idx : idx_range) {
      TypeCast type_cast;
      type_cast.atomic_op_idx = idx;
      type_cast.reg = Register(cast.at(1));
      type_cast.type_name = cast.at(2).get<std::string>();
      out[idx].push_back(type_cast);
    }
  }

  return out;
}

std::unique_ptr<InstructionParser> FormRegressionTest::parser;
std::unique_ptr<DecompilerTypeSystem> FormRegressionTest::dts;