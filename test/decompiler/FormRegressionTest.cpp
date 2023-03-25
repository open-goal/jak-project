#include "FormRegressionTest.h"

#include "common/goos/PrettyPrinter.h"
#include "common/util/json_util.h"

#include "decompiler/IR2/Form.h"
#include "decompiler/analysis/cfg_builder.h"
#include "decompiler/analysis/expression_build.h"
#include "decompiler/analysis/final_output.h"
#include "decompiler/analysis/inline_asm_rewrite.h"
#include "decompiler/analysis/insert_lets.h"
#include "decompiler/analysis/reg_usage.h"
#include "decompiler/analysis/stack_spill.h"
#include "decompiler/analysis/type_analysis.h"
#include "decompiler/analysis/variable_naming.h"
#include "decompiler/util/config_parsers.h"

#include "third-party/json.hpp"

using namespace decompiler;

void FormRegressionTestJak1::SetUpTestCase() {
  parser = std::make_unique<InstructionParser>();
  dts = std::make_unique<DecompilerTypeSystem>(GameVersion::Jak1);
  dts->parse_type_defs({"decompiler", "config", "jak1", "all-types.gc"});
}

void FormRegressionTestJak2::SetUpTestCase() {
  parser = std::make_unique<InstructionParser>();
  dts = std::make_unique<DecompilerTypeSystem>(GameVersion::Jak2);
  dts->parse_type_defs({"decompiler", "config", "jak2", "all-types.gc"});
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
  type_tag.set_to_symbol(decompiler::LinkedWord::TYPE_PTR, "string");
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

  auto j = parse_commented_json(str, "Regression Test variable map");

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
    const TestSettings& settings) {
  // Set up decompiler type system
  dts->type_prop_settings.reset();
  dts->type_prop_settings.current_method_type = settings.method_name;

  // set up label names for string constants
  std::vector<std::string> string_label_names;
  for (auto& x : settings.strings) {
    string_label_names.push_back(x.first);
  }

  // parse the assembly
  auto program = parser->parse_program(code, string_label_names);

  // create the test data collection
  auto test = std::make_unique<TestData>(program.instructions.size(), settings.version);
  // populate the LinkedObjectFile
  test->file.words_by_seg.resize(3);
  test->file.labels = program.labels;
  // Set up the environment
  test->func.ir2.env.file = &test->file;
  test->func.ir2.env.dts = dts.get();
  test->func.ir2.env.func = &test->func;
  // Set up the function
  test->func.instructions = program.instructions;
  test->func.guessed_name.set_as_global("test-function");
  test->func.type = function_type;

  // set up string constants in the data
  for (auto& str : settings.strings) {
    test->add_string_at_label(str.first, str.second);
  }

  // set up LabelDB:
  test->file.label_db = std::make_unique<LabelDB>(
      std::unordered_map<std::string, LabelConfigInfo>{}, test->file.labels, *dts);

  for (auto& str : settings.strings) {
    test->file.label_db->set_and_get_previous(test->file.label_db->get_index_by_name(str.first),
                                              TypeSpec("string"), false, {});
  }

  // find basic blocks
  test->func.basic_blocks = find_blocks_in_function(test->file, 0, test->func);
  // analyze function prologue/epilogue
  test->func.analyze_prologue(test->file);
  // build control flow graph
  test->func.cfg = build_cfg(test->file, 0, test->func, {}, {}, settings.version);
  EXPECT_TRUE(test->func.cfg->is_fully_resolved());
  if (!test->func.cfg->is_fully_resolved()) {
    fmt::print("CFG:\n{}\n", test->func.cfg->to_dot());
  } else {
    test->func.cfg_ok = true;
  }

  // find stack spill slots
  auto spill_map = build_spill_map(test->func.instructions,
                                   {test->func.prologue_end, test->func.epilogue_start});
  test->func.ir2.env.set_stack_spills(spill_map);

  // convert instruction to atomic ops
  DecompWarnings warnings;
  auto ops = convert_function_to_atomic_ops(test->func, program.labels, warnings, false, {},
                                            settings.version);
  test->func.ir2.atomic_ops = std::make_shared<FunctionAtomicOps>(std::move(ops));
  test->func.ir2.atomic_ops_succeeded = true;
  test->func.ir2.env.set_end_var(test->func.ir2.atomic_ops->end_op().return_var());

  // set up type settings
  if (!settings.casts_json.empty()) {
    test->func.ir2.env.set_type_casts(parse_cast_hints(nlohmann::json::parse(settings.casts_json)));
  }

  if (settings.allow_pairs) {
    test->func.ir2.env.set_sloppy_pair_typing();
  }

  if (!settings.stack_structure_json.empty()) {
    auto stack_hints =
        parse_stack_structure_hints(nlohmann::json::parse(settings.stack_structure_json));
    test->func.ir2.env.set_stack_structure_hints(stack_hints);
  }

  // analyze types
  EXPECT_TRUE(run_type_analysis_ir2(function_type, *dts, test->func));
  test->func.ir2.env.types_succeeded = true;

  // analyze registers
  test->func.ir2.env.set_reg_use(analyze_ir2_register_usage(test->func));

  // split to variables
  auto result = run_variable_renaming(test->func, test->func.ir2.env.reg_use(),
                                      *test->func.ir2.atomic_ops, *dts);
  if (result.has_value()) {
    test->func.ir2.env.set_local_vars(*result);
  } else {
    EXPECT_TRUE(false);
  }

  // structure
  build_initial_forms(test->func);
  EXPECT_TRUE(test->func.ir2.top_form);

  if (test->func.ir2.top_form) {
    // just make sure this doesn't crash
    RegAccessSet vars;
    test->func.ir2.top_form->collect_vars(vars, true);

    if (settings.do_expressions) {
      auto config = parse_var_json(settings.var_map_json);
      // build expressions (most of the fancy decompilation happens here)
      bool success = convert_to_expressions(test->func.ir2.top_form, *test->func.ir2.form_pool,
                                            test->func, config.first, config.second, *dts);

      EXPECT_TRUE(success);
      if (!success) {
        return nullptr;
      }

      rewrite_inline_asm_instructions(test->func.ir2.top_form, *test->func.ir2.form_pool,
                                      test->func, *dts);

      // move variables into lets.
      LetRewriteStats dummy;
      insert_lets(test->func, test->func.ir2.env, *test->func.ir2.form_pool,
                  test->func.ir2.top_form, dummy);
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
                              const TestSettings& settings) {
  auto ts = dts->parse_type_spec(type);
  auto test = make_function(code, ts, settings);
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
    const std::string& cast_json,
    const std::string& var_map_json) {
  auto ts = dts->parse_type_spec(type);
  TestSettings settings;
  settings.allow_pairs = allow_pairs;
  settings.strings = strings;
  settings.casts_json = cast_json;
  settings.var_map_json = var_map_json;
  settings.do_expressions = true;
  auto test = make_function(code, ts, settings);
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

void FormRegressionTest::test_with_stack_structures(const std::string& code,
                                                    const std::string& type,
                                                    const std::string& expected,
                                                    const std::string& stack_map_json,
                                                    const std::string& cast_json,
                                                    const std::string& var_map_json) {
  TestSettings settings;
  settings.do_expressions = true;
  settings.stack_structure_json = stack_map_json;
  settings.var_map_json = var_map_json;
  settings.casts_json = cast_json;
  test(code, type, expected, settings);
}

std::unique_ptr<InstructionParser> FormRegressionTest::parser;
std::unique_ptr<DecompilerTypeSystem> FormRegressionTest::dts;
