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

struct PointerParam {
  // TODO - Not Needed Yet
};

class PointerTests : public testing::TestWithParam<PointerParam> {
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

  std::string testCategory = "pointers";
  inja::Environment env{GoalTest::getTemplateDir(testCategory),
                        GoalTest::getGeneratedDir(testCategory)};
};

std::thread PointerTests::runtime_thread;
Compiler PointerTests::compiler;
GoalTest::CompilerTestRunner PointerTests::runner;

TEST_F(PointerTests, DeReference) {
  runner.run_static_test(env, testCategory, "deref-simple.static.gc", {"structure\n0\n"});
}

TEST_F(PointerTests, Pointers) {
  runner.run_static_test(env, testCategory, "pointers.static.gc", {"13\n"});
}
