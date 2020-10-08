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

struct LibraryParam {
  // TODO - Not Needed Yet
};

class LibraryTests : public testing::TestWithParam<LibraryParam> {
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

  std::string testCategory = "library";
  inja::Environment env{GoalTest::getTemplateDir(testCategory),
                        GoalTest::getGeneratedDir(testCategory)};
};

std::thread LibraryTests::runtime_thread;
Compiler LibraryTests::compiler;
GoalTest::CompilerTestRunner LibraryTests::runner;

TEST_F(LibraryTests, Set) {
  runner.run_static_test(env, testCategory, "set-symbol.static.gc", {"22\n"});
}

TEST_F(LibraryTests, Protect) {
  runner.run_static_test(env, testCategory, "protect.static.gc", {"33\n"});
}

TEST_F(LibraryTests, Align) {
  runner.run_static_test(env, testCategory, "align16-1.static.gc", {"80\n"});
	runner.run_static_test(env, testCategory, "align16-2.static.gc", {"64\n"});
}
