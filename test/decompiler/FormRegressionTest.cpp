#include "FormRegressionTest.h"

#include "decompiler/analysis/variable_naming.h"
#include "decompiler/analysis/reg_usage.h"
#include "decompiler/analysis/cfg_builder.h"
#include "decompiler/analysis/expression_build.h"
#include "common/goos/PrettyPrinter.h"
#include "decompiler/IR2/Form.h"

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

std::unique_ptr<FormRegressionTest::TestData> FormRegressionTest::make_function(
    const std::string& code,
    const TypeSpec& function_type,
    bool do_expressions,
    bool allow_pairs,
    const std::string& method_name,
    const std::vector<std::pair<std::string, std::string>>& strings) {
  dts->type_prop_settings.locked = true;
  dts->type_prop_settings.reset();
  dts->type_prop_settings.allow_pair = allow_pairs;
  dts->type_prop_settings.current_method_type = method_name;
  auto program = parser->parse_program(code);
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

  auto ops = convert_function_to_atomic_ops(test->func, program.labels);
  test->func.ir2.atomic_ops = std::make_shared<FunctionAtomicOps>(std::move(ops));
  test->func.ir2.atomic_ops_succeeded = true;

  EXPECT_TRUE(test->func.run_type_analysis_ir2(function_type, *dts, test->file, {}));

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
    VariableSet vars;
    test->func.ir2.top_form->collect_vars(vars);

    if (do_expressions) {
      bool success = convert_to_expressions(test->func.ir2.top_form, *test->func.ir2.form_pool,
                                            test->func, *dts);

      EXPECT_TRUE(success);
      if (!success) {
        return nullptr;
      }
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
                              const std::vector<std::pair<std::string, std::string>>& strings) {
  auto ts = dts->parse_type_spec(type);
  auto test = make_function(code, ts, do_expressions, allow_pairs, method_name, strings);
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

std::unique_ptr<InstructionParser> FormRegressionTest::parser;
std::unique_ptr<DecompilerTypeSystem> FormRegressionTest::dts;