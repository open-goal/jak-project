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

struct MacroParam {
  // TODO - Not Needed Yet
};

class MacroTests : public testing::TestWithParam<MacroParam> {
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

  std::string testCategory = "macros";
  inja::Environment env{GoalTest::getTemplateDir(testCategory),
                        GoalTest::getGeneratedDir(testCategory)};
};

std::thread MacroTests::runtime_thread;
Compiler MacroTests::compiler;
GoalTest::CompilerTestRunner MacroTests::runner;

TEST_F(MacroTests, Defsmacro) {
  runner.run_static_test(env, testCategory, "defsmacro-defgmacro.static.gc", {"20\n"});
}

TEST_F(MacroTests, Desfun) {
  runner.run_static_test(env, testCategory, "desfun.static.gc", {"4\n"});
}
