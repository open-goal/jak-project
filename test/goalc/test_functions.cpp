#include <thread>
#include <chrono>

#include "gtest/gtest.h"
#include "game/runtime.h"
#include "goalc/listener/Listener.h"
#include "goalc/compiler/Compiler.h"

#include "third-party/inja.hpp"
#include "third-party/json.hpp"
#include <common\util\FileUtil.h>

#include <test\goalc\framework\test_runner.h>

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
