#include <string>
#include <thread>

#include "game/runtime.h"
#include "goalc/compiler/Compiler.h"
#include "gtest/gtest.h"
#include "test/goalc/framework/test_runner.h"

struct ControlStatementParam {
  // TODO - Not Needed Yet
};

class ControlStatementTests : public testing::TestWithParam<ControlStatementParam> {
 public:
  static void SetUpTestSuite() {
    runtime_thread = std::make_unique<std::thread>(std::thread(GoalTest::runtime_no_kernel_jak1));
    compiler = std::make_unique<Compiler>(GameVersion::Jak1);
    runner = std::make_unique<GoalTest::CompilerTestRunner>();
    runner->c = compiler.get();
  }

  static void TearDownTestSuite() {
    compiler->shutdown_target();
    runtime_thread->join();

    runtime_thread.reset();
    compiler.reset();
    runner.reset();
  }

  void SetUp() {
    GoalTest::createDirIfAbsent(GoalTest::getTemplateDir(testCategory));
    GoalTest::createDirIfAbsent(GoalTest::getGeneratedDir(testCategory));
  }

  void TearDown() {}

  static std::unique_ptr<std::thread> runtime_thread;
  static std::unique_ptr<Compiler> compiler;
  static std::unique_ptr<GoalTest::CompilerTestRunner> runner;

  std::string testCategory = "control-statements";
};

std::unique_ptr<std::thread> ControlStatementTests::runtime_thread;
std::unique_ptr<Compiler> ControlStatementTests::compiler;
std::unique_ptr<GoalTest::CompilerTestRunner> ControlStatementTests::runner;

TEST_F(ControlStatementTests, ConditionalCompilation) {
  runner->run_static_test(testCategory, "conditional-compilation.static.gc", {"3\n"});
}

TEST_F(ControlStatementTests, Blocks) {
  runner->run_static_test(testCategory, "nested-blocks-1.static.gc", {"7\n"});
  runner->run_static_test(testCategory, "nested-blocks-2.static.gc", {"8\n"});
  runner->run_static_test(testCategory, "nested-blocks-3.static.gc", {"7\n"});
}

TEST_F(ControlStatementTests, GoTo) {
  runner->run_static_test(testCategory, "goto.static.gc", {"3\n"});
}

TEST_F(ControlStatementTests, Branch) {
  runner->run_static_test(testCategory, "return-value-of-if.static.gc", {"123\n"});
}

TEST_F(ControlStatementTests, DoTimes) {
  runner->run_static_test(testCategory, "dotimes.static.gc", {"4950\n"});
}

TEST_F(ControlStatementTests, Factorial) {
  runner->run_static_test(testCategory, "factorial-recursive.static.gc", {"3628800\n"});
  runner->run_static_test(testCategory, "factorial-iterative.static.gc", {"3628800\n"});
}

TEST_F(ControlStatementTests, Definitions) {
  runner->run_static_test(testCategory, "defun-return-constant.static.gc", {"12\n"});
  runner->run_static_test(testCategory, "defun-return-symbol.static.gc", {"42\n"});
}

TEST_F(ControlStatementTests, ReturnValue) {
  runner->run_static_test(testCategory, "return.static.gc", {"77\n"});
  runner->run_static_test(testCategory, "return-arg.static.gc", {"23\n"});
  runner->run_static_test(testCategory, "return-colors.static.gc", {"77\n"});
}

TEST_F(ControlStatementTests, Calling) {
  runner->run_static_test(testCategory, "nested-call.static.gc", {"2\n"});
  runner->run_static_test(testCategory, "simple-call.static.gc", {"30\n"});
}

TEST_F(ControlStatementTests, Anonymous) {
  runner->run_static_test(testCategory, "lambda-1.static.gc", {"2\n"});
}

TEST_F(ControlStatementTests, InlineIsInline) {
  auto code = compiler->get_goos().reader.read_from_file(
      {"test/goalc/source_templates/control-statements/declare-inline.static.gc"});
  auto compiled = compiler->compile_object_file("test-code", code, true);
  EXPECT_EQ(compiled->functions().size(), 2);
  auto& ir = compiled->top_level_function().code();
  bool got_mult = false;
  for (auto& x : ir) {
    EXPECT_EQ(dynamic_cast<IR_FunctionCall*>(x.get()), nullptr);
    auto as_im = dynamic_cast<IR_IntegerMath*>(x.get());
    if (as_im) {
      EXPECT_EQ(as_im->get_kind(), IntegerMathKind::IMUL_32);
      got_mult = true;
    }
  }
  EXPECT_TRUE(got_mult);
  runner->run_static_test(testCategory, "declare-inline.static.gc", {"32\n"});
}

TEST_F(ControlStatementTests, AllowInline) {
  auto code = compiler->get_goos().reader.read_from_file(
      {"test/goalc/source_templates/control-statements/inline-call.static.gc"});
  auto compiled = compiler->compile_object_file("test-code", code, true);
  EXPECT_EQ(compiled->functions().size(), 2);
  auto& ir = compiled->top_level_function().code();
  int got_mult = 0;
  int got_call = 0;
  for (auto& x : ir) {
    if (dynamic_cast<IR_FunctionCall*>(x.get())) {
      got_call++;
    }
    auto as_im = dynamic_cast<IR_IntegerMath*>(x.get());
    if (as_im && as_im->get_kind() == IntegerMathKind::IMUL_32) {
      got_mult++;
    }
  }
  EXPECT_EQ(got_mult, 1);
  EXPECT_EQ(got_call, 1);
  runner->run_static_test(testCategory, "inline-call.static.gc", {"44\n"});
}

TEST_F(ControlStatementTests, ReturnNone) {
  runner->run_static_test(testCategory, "function-returning-none.static.gc", {"1\n"});
}

TEST_F(ControlStatementTests, InlineBlock1) {
  runner->run_static_test(testCategory, "inline-with-block-1.static.gc", {"1\n"});
}

TEST_F(ControlStatementTests, InlineBlock2) {
  runner->run_static_test(testCategory, "inline-with-block-2.static.gc", {"3\n"});
}

TEST_F(ControlStatementTests, InlineBlock3) {
  runner->run_static_test(testCategory, "inline-with-block-3.static.gc", {"4\n"});
}

TEST_F(ControlStatementTests, InlineBlock4) {
  runner->run_static_test(testCategory, "inline-with-block-4.static.gc", {"3.0000\n0\n"});
}

TEST_F(ControlStatementTests, ReturnFromTrick) {
  runner->run_static_test(testCategory, "return-from-trick.static.gc", {"1\n"});
}

TEST_F(ControlStatementTests, Set) {
  runner->run_static_test(testCategory, "set-symbol.static.gc", {"22\n"});
}

TEST_F(ControlStatementTests, Protect) {
  runner->run_static_test(testCategory, "protect.static.gc", {"33\n"});
}

TEST_F(ControlStatementTests, Align) {
  runner->run_static_test(testCategory, "align16-1.static.gc", {"80\n"});
  runner->run_static_test(testCategory, "align16-2.static.gc", {"64\n"});
}

TEST_F(ControlStatementTests, Defsmacro) {
  runner->run_static_test(testCategory, "defsmacro-defgmacro.static.gc", {"20\n"});
}

TEST_F(ControlStatementTests, Desfun) {
  runner->run_static_test(testCategory, "desfun.static.gc", {"4\n"});
}

TEST_F(ControlStatementTests, DeReference) {
  runner->run_static_test(testCategory, "methods.static.gc", {"#t#t\n0\n"});
}
