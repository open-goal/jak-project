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

struct FloatParam {
  // TODO - Not Needed Yet
};

class FloatTests : public testing::TestWithParam<FloatParam> {
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

  std::string testCategory = "float";
  inja::Environment env{GoalTest::getTemplateDir(testCategory),
                        GoalTest::getGeneratedDir(testCategory)};
};

std::thread FloatTests::runtime_thread;
Compiler FloatTests::compiler;
GoalTest::CompilerTestRunner FloatTests::runner;

TEST_F(FloatTests, Constants) {
  runner.run_static_test(env, testCategory, "float.static.gc", {"1067316150\n"});
  runner.run_static_test(env, testCategory, "function-return-float-constant.static.gc",
                         {"3.14149\n0\n"});
}

TEST_F(FloatTests, Operations) {
  runner.run_static_test(env, testCategory, "float-pow.static.gc", {"256\n0\n"});
  runner.run_static_test(env, testCategory, "float-product.static.gc", {"120.0000\n0\n"});
}

TEST_F(FloatTests, Symbols) {
  runner.run_static_test(env, testCategory, "float-in-symbol.static.gc", {"2345.6000\n0\n"});
}

TEST_F(FloatTests, Functions) {
  runner.run_static_test(env, testCategory, "float-function.static.gc", {"10.152\n0\n"});
  runner.run_static_test(
      env, testCategory, "nested-float-functions.static.gc",
      {"i 1.4400 3.4000\nr 10.1523\ni 1.2000 10.1523\nr 17.5432\n17.543 10.152\n0\n"});
}
