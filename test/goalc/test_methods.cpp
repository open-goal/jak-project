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

struct MethodParam {
  // TODO - Not Needed Yet
};

class MethodTests : public testing::TestWithParam<MethodParam> {
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

  std::string testCategory = "methods";
  inja::Environment env{GoalTest::getTemplateDir(testCategory),
                        GoalTest::getGeneratedDir(testCategory)};
};

std::thread MethodTests::runtime_thread;
Compiler MethodTests::compiler;
GoalTest::CompilerTestRunner MethodTests::runner;

TEST_F(MethodTests, DeReference) {
  runner.run_static_test(env, testCategory, "methods.static.gc", {"#t#t\n0\n"});
}