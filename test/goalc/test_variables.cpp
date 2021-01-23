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

TEST_F(VariableTests, StackVars) {
  runner.run_static_test(env, testCategory, "stack-ints.gc", {"12\n"});
  runner.run_static_test(env, testCategory, "stack-ints-2.gc", {"1\n"});
}

TEST_F(VariableTests, Bitfields) {
  runner.run_static_test(env, testCategory, "bitfield-enums.gc", {"5\n"});
  runner.run_static_test(env, testCategory, "integer-enums.gc", {"11\n"});
}

TEST_F(VariableTests, InlineAsm) {
  runner.run_static_test(env, testCategory, "inline-asm.static.gc", {"1\n"});
}

TEST_F(VariableTests, StaticBitfieldField) {
  runner.run_static_test(env, testCategory, "static-bitfield-field.gc", {"22\n"});
}

TEST_F(VariableTests, StackArrayAlignment) {
  runner.run_static_test(env, testCategory, "stack-array-align.gc", {"3\n"});
}

TEST_F(VariableTests, StackStructureAlignment) {
  runner.run_static_test(env, testCategory, "stack-structure-align.gc", {"1234\n"});
}

TEST_F(VariableTests, GetSymbol) {
  runner.run_static_test(env, testCategory, "get-symbol-1.static.gc",
                         {"1342756\n"});  // 0x147d24 in hex
  runner.run_static_test(env, testCategory, "get-symbol-2.static.gc",
                         {"1342764\n"});  // 0x147d2c in hex
}

TEST_F(VariableTests, Constants) {
  // TODO - runner.run_static_test(env, testCategory, "string-constant-1.static.gc");
  std::string expected = "\"test string!\"";
  runner.run_static_test(env, testCategory, "string-constant-2.static.gc", {expected},
                         expected.size());
}

TEST_F(VariableTests, Symbols) {
  runner.run_static_test(env, testCategory, "quote-symbol.static.gc", {"banana\n0\n"});
  std::string expected = "test-string";
  runner.run_static_test(env, testCategory, "string-symbol.static.gc", {expected}, expected.size());
}

TEST_F(VariableTests, Formatting) {
  runner.run_static_test(env, testCategory, "format-reg-order.static.gc",
                         {"test 1 2 3 4 5 6\n0\n"});
}

TEST_F(VariableTests, DeReference) {
  runner.run_static_test(env, testCategory, "deref-simple.static.gc", {"structure\n0\n"});
}

TEST_F(VariableTests, Pointers) {
  runner.run_static_test(env, testCategory, "pointers.static.gc", {"13\n"});
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
//  runner.run_test_from_file("test-format.gc", {expected}, expected.size());
