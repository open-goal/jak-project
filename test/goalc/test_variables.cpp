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

struct VariableParam {
  // TODO - Not Needed Yet
};

class VariableTests : public testing::TestWithParam<VariableParam> {
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

  std::string testCategory = "variables";
  inja::Environment env{GoalTest::getTemplateDir(testCategory),
                        GoalTest::getGeneratedDir(testCategory)};
};

std::thread VariableTests::runtime_thread;
Compiler VariableTests::compiler;
GoalTest::CompilerTestRunner VariableTests::runner;

TEST_F(VariableTests, Globals) {
  runner.run_static_test(env, testCategory, "defglobalconstant-1.static.gc", {"17\n"});
  runner.run_static_test(env, testCategory, "defglobalconstant-2.static.gc", {"18\n"});
}

TEST_F(VariableTests, Definitions) {
  runner.run_static_test(env, testCategory, "define.static.gc", {"17\n"});
}

TEST_F(VariableTests, Let) {
  runner.run_static_test(env, testCategory, "let.static.gc", {"30\n"});
  runner.run_static_test(env, testCategory, "let-star.static.gc", {"30\n"});
  runner.run_static_test(env, testCategory, "mlet.static.gc", {"10\n"});
}
