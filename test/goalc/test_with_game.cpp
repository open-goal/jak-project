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

struct WithGameParam {
  // TODO - Not Needed Yet
};

class WithGameTests : public testing::TestWithParam<WithGameParam> {
 public:
  static void SetUpTestSuite() {
    runtime_thread = std::thread((GoalTest::runtime_with_kernel));
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

  std::string testCategory = "with_game";
  inja::Environment env{GoalTest::getTemplateDir(testCategory),
                        GoalTest::getGeneratedDir(testCategory)};
};

std::thread WithGameTests::runtime_thread;
Compiler WithGameTests::compiler;
GoalTest::CompilerTestRunner WithGameTests::runner;

// TODO - havn't done anything with these really yet
TEST_F(WithGameTests, All) {
  runner.run_static_test(env, testCategory, "defun-return-constant.static.gc", {"12\n"});
  runner.run_static_test(env, testCategory, "defun-return-symbol.static.gc", {"42\n"});
  runner.run_static_test(env, testCategory, "test-min-max.gc", {"10\n"});
  runner.run_static_test(env, testCategory, "test-bfloat.gc",
                         {"data 1.2330 print 1.2330 type bfloat\n0\n"});
  runner.run_static_test(env, testCategory, "test-basic-type-check.gc", {"#f#t#t#f#t#f#t#t\n0\n"});
  runner.run_static_test(env, testCategory, "test-condition-boolean.gc", {"4\n"});
  runner.run_static_test(env, testCategory, "test-type-type.gc", {"#t#f\n0\n"});
  runner.run_static_test(env, testCategory, "test-access-inline-array.gc", {"1.2345\n0\n"});
  runner.run_static_test(env, testCategory, "test-find-parent-method.gc", {"\"test pass!\"\n0\n"});
  runner.run_static_test(env, testCategory, "test-ref.gc", {"83\n"});
  runner.run_static_test(env, testCategory, "test-pair-asize.gc", {"8\n"});
  runner.run_static_test(env, testCategory, "test-last.gc", {"d\n0\n"});
  runner.run_static_test(
      env, testCategory, "test-sort.gc",
      {"(24 16 32 56 72 1234 -34 25 654)\n(1234 654 72 56 32 25 24 16 -34)\n0\n"});
  runner.run_static_test(
      env, testCategory, "test-sort-2.gc",
      {"(24 16 32 56 72 1234 -34 25 654)\n(-34 16 24 25 32 56 72 654 1234)\n0\n"});
  runner.run_static_test(
      env, testCategory, "test-sort-3.gc",
      {"(24 16 32 56 72 1234 -34 25 654)\n(-34 16 24 25 32 56 72 654 1234)\n0\n"});
  runner.run_static_test(env, testCategory, "test-pair-length.gc", {"6\n"});
  runner.run_static_test(env, testCategory, "test-member-1.gc", {"(c d)\n0\n"});
  runner.run_static_test(env, testCategory, "test-member-2.gc", {"#f\n0\n"});
  runner.run_static_test(env, testCategory, "test-assoc-1.gc", {"w\n0\n"});
  runner.run_static_test(env, testCategory, "test-assoc-2.gc", {"#f\n0\n"});
  runner.run_static_test(env, testCategory, "test-assoce-1.gc", {"x\n0\n"});
  runner.run_static_test(env, testCategory, "test-assoce-2.gc", {"x\n0\n"});
  runner.run_static_test(env, testCategory, "test-append.gc", {"(a b c d e)\n0\n"});
  runner.run_static_test(env, testCategory, "test-delete-list.gc", {"(a b d e)\n0\n"});
  runner.run_static_test(env, testCategory, "test-delete-car.gc", {"((a . b) (e . f))\n#f\n0\n"});
  runner.run_static_test(env, testCategory, "test-insert-cons.gc",
                         {"((c . w) (a . b) (e . f))\n0\n"});
  runner.run_static_test(env, testCategory, "test-new-inline-array-class.gc", {"2820\n"});
  runner.run_static_test(env, testCategory, "test-memcpy.gc", {"13\n"});
  runner.run_static_test(env, testCategory, "test-memset.gc", {"11\n"});
  runner.run_static_test(env, testCategory, "test-binteger-print.gc", {"-17\n0\n"});
  runner.run_static_test(env, testCategory, "test-tests.gc",
                         {"Test Failed On Test 0: \"unknown\"\nTest Failed On Test 0: "
                          "\"test\"\nTest \"test-of-test\": 1 Passes\n0\n"});
  runner.run_static_test(env, testCategory, "test-type-arrays.gc",
                         {"Test \"test-type-arrays\": 3 Passes\n0\n"});
  runner.run_static_test(env, testCategory, "test-number-comparison.gc",
                         {"Test \"number-comparison\": 14 Passes\n0\n"});
  /*runner.run_static_test(env, testCategory, "test-approx-pi.gc",
                         get_test_pass_string("approx-pi", 4));
  runner.run_static_test(env, testCategory, "test-dynamic-type.gc",
                         get_test_pass_string("dynamic-type", 4));
  runner.run_static_test(env, testCategory, "test-string-type.gc",
                         get_test_pass_string("string-type", 4));
  runner.run_static_test(env, testCategory, "test-new-string.gc",
                         get_test_pass_string("new-string", 5));*/
  // runner.run_static_test(env, testCategory, "test-addr-of.gc", get_test_pass_string("addr-of",
  // 2));
  runner.run_static_test(env, testCategory, "test-set-self.gc", {"#t\n0\n"});
}
