#include <thread>
#include <chrono>

#include "gtest/gtest.h"
#include "game/runtime.h"
#include "goalc/listener/Listener.h"
#include "goalc/compiler/Compiler.h"

#include "inja.hpp"
#include "third-party/json.hpp"
#include <test/goalc/framework/test_runner.h>

#include <iostream>
#include <string>
#include <cstdio>
#include <sstream>
#include <iostream>
#include <random>
#include <filesystem>

struct FunctionParam {
  // TODO - Not Needed Yet
};

class FunctionTests : public testing::TestWithParam<FunctionParam> {
 public:
  static void SetUpTestSuite() {
    runtime_thread = std::thread((GoalTest::runtime_no_kernel));
    runner.c = &compiler;
  }

  static void TearDownTestSuite() {
    compiler.shutdown_target();
    runtime_thread.join();
  }

  void SetUp() {
    GoalTest::createDirIfAbsent(GoalTest::getTemplateDir(testCategory));
    GoalTest::createDirIfAbsent(GoalTest::getGeneratedDir(testCategory));
  }

  void TearDown() {}

  static std::thread runtime_thread;
  static Compiler compiler;
  static GoalTest::CompilerTestRunner runner;

  std::string testCategory = "functions";
  inja::Environment env{GoalTest::getTemplateDir(testCategory),
                        GoalTest::getGeneratedDir(testCategory)};
};

std::thread FunctionTests::runtime_thread;
Compiler FunctionTests::compiler;
GoalTest::CompilerTestRunner FunctionTests::runner;

TEST_F(FunctionTests, Definitions) {
  runner.run_static_test(env, testCategory, "defun-return-constant.static.gc", {"12\n"});
  runner.run_static_test(env, testCategory, "defun-return-symbol.static.gc", {"42\n"});
}

TEST_F(FunctionTests, ReturnValue) {
  runner.run_static_test(env, testCategory, "return.static.gc", {"77\n"});
  runner.run_static_test(env, testCategory, "return-arg.static.gc", {"23\n"});
  runner.run_static_test(env, testCategory, "return-colors.static.gc", {"77\n"});
}

TEST_F(FunctionTests, Calling) {
  runner.run_static_test(env, testCategory, "nested-call.static.gc", {"2\n"});
  runner.run_static_test(env, testCategory, "inline-call.static.gc", {"44\n"});
  runner.run_static_test(env, testCategory, "simple-call.static.gc", {"30\n"});
}

TEST_F(FunctionTests, Anonymous) {
  runner.run_static_test(env, testCategory, "declare-inline.static.gc", {"32\n"});
  runner.run_static_test(env, testCategory, "lambda-1.static.gc", {"2\n"});
}

TEST_F(FunctionTests, InlineIsInline) {
  auto code = compiler.get_goos().reader.read_from_file(
      {"test/goalc/source_templates/functions/declare-inline.static.gc"});
  auto compiled = compiler.compile_object_file("test-code", code, true);
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
}

TEST_F(FunctionTests, AllowInline) {
  auto code = compiler.get_goos().reader.read_from_file(
      {"test/goalc/source_templates/functions/inline-call.static.gc"});
  auto compiled = compiler.compile_object_file("test-code", code, true);
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
}

TEST_F(FunctionTests, ReturnNone) {
  runner.run_static_test(env, testCategory, "function-returning-none.static.gc", {"1\n"});
}

TEST_F(FunctionTests, InlineBlock1) {
  runner.run_static_test(env, testCategory, "inline-with-block-1.static.gc", {"1\n"});
}

TEST_F(FunctionTests, InlineBlock2) {
  runner.run_static_test(env, testCategory, "inline-with-block-2.static.gc", {"3\n"});
}

TEST_F(FunctionTests, InlineBlock3) {
  runner.run_static_test(env, testCategory, "inline-with-block-3.static.gc", {"4\n"});
}

TEST_F(FunctionTests, InlineBlock4) {
  runner.run_static_test(env, testCategory, "inline-with-block-4.static.gc", {"3.0000\n0\n"});
}

TEST_F(FunctionTests, ReturnFromTrick) {
  runner.run_static_test(env, testCategory, "return-from-trick.static.gc", {"1\n"});
}