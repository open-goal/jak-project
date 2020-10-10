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

struct LogicParam {
  // TODO - Not Needed Yet
};

class LogicTests : public testing::TestWithParam<LogicParam> {
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

  std::string testCategory = "logic";
  inja::Environment env{GoalTest::getTemplateDir(testCategory),
                        GoalTest::getGeneratedDir(testCategory)};
};

std::thread LogicTests::runtime_thread;
Compiler LogicTests::compiler;
GoalTest::CompilerTestRunner LogicTests::runner;

TEST_F(LogicTests, LogicalOperators) {
  runner.run_static_test(env, testCategory, "logand.static.gc", {"4\n"});
  runner.run_static_test(env, testCategory, "logior.static.gc", {"60\n"});
  runner.run_static_test(env, testCategory, "logxor.static.gc", {"56\n"});
}

TEST_F(LogicTests, Comparison) {
  runner.run_static_test(env, testCategory, "signed-int-compare.static.gc", {"12\n"});
}
