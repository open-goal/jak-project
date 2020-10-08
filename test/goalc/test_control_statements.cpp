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

struct ControlStatementParam {
  // TODO - Not Needed Yet
};

class ControlStatementTests : public testing::TestWithParam<ControlStatementParam> {
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

  std::string testCategory = "control-statements";
  inja::Environment env{GoalTest::getTemplateDir(testCategory),
                        GoalTest::getGeneratedDir(testCategory)};
};

std::thread ControlStatementTests::runtime_thread;
Compiler ControlStatementTests::compiler;
GoalTest::CompilerTestRunner ControlStatementTests::runner;

TEST_F(ControlStatementTests, ConditionalCompilation) {
  runner.run_static_test(env, testCategory, "conditional-compilation.static.gc", {"3\n"});
  // TODO - test-conditional-compilation-2.gc
  // these numbers match the game's memory layout for where the symbol table lives.
  // it's probably not 100% needed to get this exactly, but it's a good sign that the global
  // heap lives in the right spot because there should be no divergence in memory layout when its
  // built.  This also checks that #t, #f get "hashed" to the correct spot.
}

TEST_F(ControlStatementTests, Blocks) {
  runner.run_static_test(env, testCategory, "nested-blocks-1.static.gc", {"7\n"});
	runner.run_static_test(env, testCategory, "nested-blocks-2.static.gc", {"8\n"});
	runner.run_static_test(env, testCategory, "nested-blocks-3.static.gc", {"7\n"});
}

TEST_F(ControlStatementTests, GoTo) {
  runner.run_static_test(env, testCategory, "goto.static.gc", {"3\n"});
}

TEST_F(ControlStatementTests, Branch) {
  runner.run_static_test(env, testCategory, "return-value-of-if.static.gc", {"123\n"});
}
