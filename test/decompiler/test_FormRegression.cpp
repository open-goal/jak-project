#include <memory>
#include "gtest/gtest.h"
#include "decompiler/Disasm/InstructionParser.h"
#include "decompiler/Disasm/DecompilerLabel.h"
#include "decompiler/Function/Function.h"
#include "decompiler/ObjectFile/ObjectFileDB.h"
#include "decompiler/IR2/variable_naming.h"
#include "decompiler/IR2/cfg_builder.h"
#include "common/goos/PrettyPrinter.h"

using namespace decompiler;

class DecompilerRegressionTest : public ::testing::Test {
 protected:
  std::unique_ptr<InstructionParser> parser;
  std::unique_ptr<DecompilerTypeSystem> dts;

  void SetUp() override {
    parser = std::make_unique<InstructionParser>();
    dts = std::make_unique<DecompilerTypeSystem>();
  }

  void TearDown() override {
    parser.reset();
    dts.reset();
    parser.reset();
  }

  Function make_function(const std::string& code, const TypeSpec& function_type) {
    // todo load all types.
    auto program = parser->parse_program(code);
    //  printf("prg:\n%s\n\n", program.print().c_str());
    LinkedObjectFile file;
    file.labels = program.labels;
    Function function(0, int(program.instructions.size()));
    function.instructions = program.instructions;
    function.guessed_name.set_as_global("test-function");
    function.basic_blocks = find_blocks_in_function(file, 0, function);
    function.analyze_prologue(file);
    function.cfg = build_cfg(file, 0, function);
    EXPECT_TRUE(function.cfg->is_fully_resolved());

    auto ops = convert_function_to_atomic_ops(function, program.labels);
    function.ir2.atomic_ops = std::make_shared<FunctionAtomicOps>(std::move(ops));
    function.ir2.atomic_ops_succeeded = true;

    if (function.run_type_analysis_ir2(function_type, *dts, file, {})) {
      function.ir2.has_type_info = true;
    } else {
      EXPECT_TRUE(false);
    }

    function.ir2.reg_use = analyze_ir2_register_usage(function);
    function.ir2.has_reg_use = true;

    auto result =
        run_variable_renaming(function, function.ir2.reg_use, *function.ir2.atomic_ops, *dts);
    if (result.has_value()) {
      function.ir2.env.set_local_vars(*result);
    } else {
      EXPECT_TRUE(false);
    }

    build_initial_forms(function);
    EXPECT_TRUE(function.ir2.top_form);
    return function;
  }

  void test(const std::string& code, const std::string& type, const std::string& expected) {
    auto ts = dts->parse_type_spec(type);
    auto func = make_function(code, ts);
    auto expected_form =
        pretty_print::get_pretty_printer_reader().read_from_string(expected, false).as_pair()->car;
    auto actual_form = func.ir2.top_form->to_form(func.ir2.env);
    if (expected_form != actual_form) {
      printf("Got:\n%s\n\nExpected\n%s\n", actual_form.print().c_str(),
             expected_form.print().c_str());
    }

    EXPECT_TRUE(expected_form == actual_form);
  }
};

TEST_F(DecompilerRegressionTest, SimplestTest) {
  std::string func =
      "    sll r0, r0, 0\n"
      "    or v0, a0, r0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function object object)";
  std::string expected = "(set! v0-0 a0-0)";
  test(func, type, expected);
}

TEST_F(DecompilerRegressionTest, Op3) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L308:\n"
      "    mult3 v0, a0, a1\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(set! v0-0 (*.si a0-0 a1-0))";
  test(func, type, expected);
}

TEST_F(DecompilerRegressionTest, Division) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L307:\n"
      "    div a0, a1\n"
      "    mflo v0\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int int)";
  std::string expected = "(set! v0-0 (/.si a0-0 a1-0))";
  test(func, type, expected);
}

TEST_F(DecompilerRegressionTest, Ash) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L305:\n"
      "    or v1, a0, r0\n"
      "    bgezl a1, L306\n"
      "    dsllv v0, v1, a1\n"

      "    dsubu a0, r0, a1\n"
      "    dsrav v0, v1, a0\n"
      "L306:\n"
      "    jr ra\n"
      "    daddu sp, sp, r0\n"
      "    sll r0, r0, 0\n"
      "    sll r0, r0, 0";
  std::string type = "(function int int int)";
  std::string expected = "(begin (set! v1-0 a0-0) (set! v0-0 (ash.si v1-0 a1-0)))";
  test(func, type, expected);
}

TEST_F(DecompilerRegressionTest, Abs) {
  std::string func =
      "    sll r0, r0, 0\n"
      "L301:\n"
      "    or v0, a0, r0\n"
      "    bltzl v0, L302\n"
      "    dsubu v0, r0, v0\n"

      "L302:\n"
      "    jr ra\n"
      "    daddu sp, sp, r0";
  std::string type = "(function int int)";
  std::string expected = "(begin (set! v0-0 a0-0) (set! v0-1 (abs v0-0)))";
  test(func, type, expected);
}