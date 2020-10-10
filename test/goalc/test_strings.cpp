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

struct StringParam {
  // TODO - Not Needed Yet
};

class StringTests : public testing::TestWithParam<StringParam> {
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

  std::string testCategory = "strings";
  inja::Environment env{GoalTest::getTemplateDir(testCategory),
                        GoalTest::getGeneratedDir(testCategory)};
};

std::thread StringTests::runtime_thread;
Compiler StringTests::compiler;
GoalTest::CompilerTestRunner StringTests::runner;

TEST_F(StringTests, Constants) {
  // TODO - runner.run_static_test(env, testCategory, "string-constant-1.static.gc");
  std::string expected = "\"test string!\"";
  runner.run_static_test(env, testCategory, "string-constant-2.static.gc", {expected},
                         expected.size());
}

TEST_F(StringTests, Symbols) {
  runner.run_static_test(env, testCategory, "quote-symbol.static.gc", {"banana\n0\n"});
  std::string expected = "test-string";
  runner.run_static_test(env, testCategory, "string-symbol.static.gc", {expected}, expected.size());
}

TEST_F(StringTests, Formatting) {
  runner.run_static_test(env, testCategory, "format-reg-order.static.gc",
                         {"test 1 2 3 4 5 6\n0\n"});
}

//  expected =
//      "test newline\nnewline\ntest tilde ~ \ntest A print boxed-string: \"boxed string!\"\ntest
//      A " "print symbol: a-symbol\ntest A make boxed object longer:             \"srt\"!\ntest A
//      " "non-default pad: zzzzzzpad-me\ntest A shorten(4): a23~\ntest A don'tchange(4):
//      a234\ntest A " "shorten with pad(4): sho~\ntest A a few things \"one thing\" a-second
//      integer #<compiled " "function @ #x161544>\n";
//
//  expected += "test S a string a-symbol another string!\n";
//  expected += "test C ) ]\n";
//  expected += "test P (no type) #<compiled function @ #x161544>\n";
//  expected += "test P (with type) 1447236\n";
//
//  // todo, finish format testing.
//  runner.run_test("test-format.gc", {expected}, expected.size());
