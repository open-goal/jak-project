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

struct SymbolParam {
  // TODO - Not Needed Yet
};

class SymbolTests : public testing::TestWithParam<SymbolParam> {
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

  std::string testCategory = "symbols";
  inja::Environment env{GoalTest::getTemplateDir(testCategory),
                        GoalTest::getGeneratedDir(testCategory)};
};

std::thread SymbolTests::runtime_thread;
Compiler SymbolTests::compiler;
GoalTest::CompilerTestRunner SymbolTests::runner;

TEST_F(SymbolTests, GetSymbol) {
  runner.run_static_test(env, testCategory, "get-symbol-1.static.gc",
                         {"1342756\n"});  // 0x147d24 in hex
  runner.run_static_test(env, testCategory, "get-symbol-2.static.gc",
                         {"1342764\n"});  // 0x147d2c in hex
}
