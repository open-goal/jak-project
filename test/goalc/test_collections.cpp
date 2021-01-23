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

struct CollectionParam {
  // TODO - Not Needed Yet
};

class CollectionTests : public testing::TestWithParam<CollectionParam> {
 public:
  static void SetUpTestSuite() {
    runtime_thread = std::make_unique<std::thread>(std::thread((GoalTest::runtime_no_kernel)));
    compiler = std::make_unique<Compiler>();
    runner = std::make_unique<GoalTest::CompilerTestRunner>();
    runner->c = compiler.get();
  }

  static void TearDownTestSuite() {
    compiler->shutdown_target();
    runtime_thread->join();

    runtime_thread.reset();
    compiler.reset();
    runner.reset();
  }

  void SetUp() {
    GoalTest::createDirIfAbsent(GoalTest::getTemplateDir(testCategory));
    GoalTest::createDirIfAbsent(GoalTest::getGeneratedDir(testCategory));
  }

  void TearDown() {}

  static std::unique_ptr<std::thread> runtime_thread;
  static std::unique_ptr<Compiler> compiler;
  static std::unique_ptr<GoalTest::CompilerTestRunner> runner;

  std::string testCategory = "collections";
  inja::Environment env{GoalTest::getTemplateDir(testCategory),
                        GoalTest::getGeneratedDir(testCategory)};
};

std::unique_ptr<std::thread> CollectionTests::runtime_thread;
std::unique_ptr<Compiler> CollectionTests::compiler;
std::unique_ptr<GoalTest::CompilerTestRunner> CollectionTests::runner;

TEST_F(CollectionTests, Pairs) {
  runner->run_static_test(env, testCategory, "empty-pair.static.gc", {"()\n0\n"});
  runner->run_static_test(env, testCategory, "pair-check.static.gc", {"#t#f\n0\n"});
}

TEST_F(CollectionTests, Lists) {
  runner->run_static_test(env, testCategory, "list.static.gc", {"(a b c d)\n0\n"});
}

TEST_F(CollectionTests, InlineArray) {
  runner->run_static_test(env, testCategory, "inline-array-field.static.gc", {"16\n"});
}

TEST_F(CollectionTests, Operations) {
  runner->run_static_test(env, testCategory, "cons.static.gc", {"(a . b)\n0\n"});
  runner->run_static_test(env, testCategory, "car-cdr-get.static.gc", {"ab\n0\n"});
  runner->run_static_test(env, testCategory, "car-cdr-set.static.gc", {"(c . d)\n0\n"});
  runner->run_static_test(env, testCategory, "nested-car-cdr-set.static.gc",
                          {"efgh\n((e . g) f . h)\n0\n"});
}
