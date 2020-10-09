#include <thread>
#include <chrono>

#include "gtest/gtest.h"
#include "game/runtime.h"
#include "goalc/listener/Listener.h"
#include "goalc/compiler/Compiler.h"

#include "third-party/inja.hpp"
#include "third-party/json.hpp"
#include <test/goalc/framework/test_runner.h>

#include <iostream>
#include <string>
#include <cstdio>
#include <sstream>
#include <iostream>
#include <random>
#include <filesystem>

struct LoopRecurParam {
  // TODO - Not Needed Yet
};

class LoopRecurTests : public testing::TestWithParam<LoopRecurParam> {
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

  std::string testCategory = "loop_recur";
  inja::Environment env{GoalTest::getTemplateDir(testCategory),
                        GoalTest::getGeneratedDir(testCategory)};
};

std::thread LoopRecurTests::runtime_thread;
Compiler LoopRecurTests::compiler;
GoalTest::CompilerTestRunner LoopRecurTests::runner;

TEST_F(LoopRecurTests, DoTimes) {
  runner.run_static_test(env, testCategory, "dotimes.static.gc", {"4950\n"});
}

TEST_F(LoopRecurTests, Factorial) {
  runner.run_static_test(env, testCategory, "factorial-recursive.static.gc", {"3628800\n"});
  runner.run_static_test(env, testCategory, "factorial-iterative.static.gc", {"3628800\n"});
}
