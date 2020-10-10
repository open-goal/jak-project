// https://github.com/google/googletest/blob/master/googletest/docs/advanced.md#value-parameterized-tests

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

// --------
// This is a very over-engineered integer test, but it serves as a decent example of how to use the
// test+template framework
// I've heavily annotated it with comments and links to docs to help
// --------

// We are using Google Test's paramaterized test feature
// This allows us to define a single generic test, and pass in a whole bunch of values
// See -
// https://github.com/google/googletest/blob/master/googletest/docs/advanced.md#value-parameterized-tests
struct IntegerParam {
  // An index is needed to be explicitly set because I couldn't find a way to pull the test-index
  // number from google's API
  // TODO - if you can find a way, please improve!
  // But this is needed so we can uniquely save the template files, especially if they error out
  // Why? - since you may choose to generate random values, it's nice for them to be stored after
  // the tests complete.  Some tests may be complex as well
  int index;
  // Each integer test has a signed value, and can be represented as hex or an integral
  s64 val;
  bool hex;

  IntegerParam(s64 val, bool hex = false, int index = 0) : val(val), hex(hex), index(index) {}

  // This is used to generate the value that is passed into the template engine
  // and injected into the file of lisp code.
  // In most cases this will probably be a string but look into inja's capabilities
  // - https://github.com/pantor/inja
  std::string toLisp() {
    // Append hex reader macro '#x'
    if (hex) {
      return std::string("#x") + std::string(std::to_string(val));
    }
    return std::to_string(val);
  }

  // This is used by the test runner code to know what the expected value is
  // For a simple example like this, a single eval is all that's required, but for
  // more complex tests, this may not be the case.
  std::string eval() {
    if (hex) {
      int64_t hexVal;
      std::stringstream ss;
      ss << std::hex << std::to_string(val);
      ss >> hexVal;
      return std::string(std::to_string(hexVal)) + "\n";
    }
    return std::to_string(val) + "\n";
  }
};

// This is a helper function that is used to generate a bunch of tests
// once again, very over-engineered for just testing engineers, but you might imagine
// a more complex test template that has several conditionals / loops / etc
std::vector<IntegerParam> genIntegerTests(int numTests,
                                          std::vector<IntegerParam> additionalTests = {}) {
  std::vector<IntegerParam> tests;
  std::random_device dev;
  std::mt19937 rng(dev());
  std::uniform_int_distribution<std::mt19937::result_type> dist6(0, UINT32_MAX);
  int testCases = 3;
  for (int i = 0; i < numTests; i++) {
    switch (i % testCases) {
      case 0:
        tests.push_back(IntegerParam(dist6(rng), false, i));
        break;
      case 1:
        tests.push_back(IntegerParam(dist6(rng) * -1, false, i));
        break;
      case 2:
        tests.push_back(IntegerParam(dist6(rng), true, i));
        break;
    }
  }

  for (int i = 0; i < additionalTests.size(); i++) {
    IntegerParam test = additionalTests.at(i);
    test.index = i + numTests - 1;
    tests.push_back(test);
  }

  return tests;
}

// In the interest of speed, we want to share the same thread/compiler across
// all the tests in this suite, so we have to over-ride this.
class ArithmeticTests : public testing::TestWithParam<IntegerParam> {
 public:
  // Per-test-suite set-up.
  // Called before the first test in this test suite.
  static void SetUpTestSuite() {
    runtime_thread = std::thread((GoalTest::runtime_no_kernel));
    runner.c = &compiler;
  }

  // Per-test-suite tear-down.
  // Called after the last test in this test suite.
  static void TearDownTestSuite() {
    compiler.shutdown_target();
    runtime_thread.join();
  }

  // You can define per-test set-up logic as usual.
  void SetUp() {
    GoalTest::createDirIfAbsent(GoalTest::getTemplateDir(testCategory));
    GoalTest::createDirIfAbsent(GoalTest::getGeneratedDir(testCategory));
  }

  // You can define per-test tear-down logic as usual.
  void TearDown() {}

  // Common Resources Across all Tests in the Suite
  static std::thread runtime_thread;
  static Compiler compiler;
  static GoalTest::CompilerTestRunner runner;

  // Just to promote better test organization, supports nesting the test files 1 directory deep
  std::string testCategory = "arithmetic";
  inja::Environment env{GoalTest::getTemplateDir(testCategory),
                        GoalTest::getGeneratedDir(testCategory)};
};

// You must initialize the static variables outside of the declaration, or you'll run into
// unresolved external errors
std::thread ArithmeticTests::runtime_thread;
Compiler ArithmeticTests::compiler;
GoalTest::CompilerTestRunner ArithmeticTests::runner;

// Finally, we define our generic test, given our custom class that represents our test inputs
// we can generate the lisp file, and pass along the path to the test runner
// If the test fails, the test runner will save the template file, with the expected/actual results
// into the `failed/` directory
TEST_P(ArithmeticTests, EvalIntegers) {
  IntegerParam param = GetParam();
  nlohmann::json data;
  data["integer"] = param.toLisp();

  std::string testFile = "eval-integer-" + std::to_string(param.index) + ".generated.gc";
  env.write("eval-integer.template.gc", data, testFile);
  runner.run_test(testCategory, testFile, {param.eval()});
}

// ValuesIn, is not the only way to use a parameterized test, but the most applicable for this
// example You can actually get googletest to compute the permutations for you, which may be useful.
// Consult their docs.
// -
// https://github.com/google/googletest/blob/master/googletest/docs/advanced.md#value-parameterized-tests
INSTANTIATE_TEST_SUITE_P(EvalIntegers,
                         ArithmeticTests,
                         testing::ValuesIn(genIntegerTests(3,
                                                           {IntegerParam(-2147483648),
                                                            IntegerParam(0), IntegerParam(-0)})));

TEST_F(ArithmeticTests, Addition) {
  runner.run_static_test(env, testCategory, "add-int-literals.static.gc", {"13\n"});
  runner.run_static_test(env, testCategory, "add-let.static.gc", {"7\n"});
}

TEST_F(ArithmeticTests, AddIntegerFunction) {
  runner.run_static_test(env, testCategory, "add-function.static.gc", {"21\n"});
}

TEST_F(ArithmeticTests, AddIntegerMultiple) {
  runner.run_static_test(env, testCategory, "add-int-multiple.static.gc", {"15\n"});
  runner.run_static_test(env, testCategory, "add-int-multiple-2.static.gc", {"15\n"});
}

TEST_F(ArithmeticTests, AddIntegerVariables) {
  runner.run_static_test(env, testCategory, "add-int-vars.static.gc", {"7\n"});
}

TEST_F(ArithmeticTests, AshFunction) {
  runner.run_static_test(env, testCategory, "ash.static.gc", {"18\n"});
}

TEST_F(ArithmeticTests, Division) {
  runner.run_static_test(env, testCategory, "divide-1.static.gc", {"6\n"});
  runner.run_static_test(env, testCategory, "divide-2.static.gc", {"7\n"});
}

TEST_F(ArithmeticTests, IntegerSymbol) {
  runner.run_static_test(env, testCategory, "negative-int-symbol.static.gc", {"-123\n"});
}

TEST_F(ArithmeticTests, Modulus) {
  runner.run_static_test(env, testCategory, "mod.static.gc", {"7\n"});
}

TEST_F(ArithmeticTests, Multiplication) {
  runner.run_static_test(env, testCategory, "multiply.static.gc", {"-12\n"});
  runner.run_static_test(env, testCategory, "multiply-let.static.gc", {"3\n"});
}

TEST_F(ArithmeticTests, NestedFunctionCall) {
  runner.run_static_test(env, testCategory, "nested-function.static.gc", {"10\n"});
}

TEST_F(ArithmeticTests, ShiftOperations) {
  runner.run_static_test(env, testCategory, "shiftvs.static.gc", {"11\n"});
}

TEST_F(ArithmeticTests, Subtraction) {
  runner.run_static_test(env, testCategory, "subtract-1.static.gc", {"4\n"});
  runner.run_static_test(env, testCategory, "subtract-2.static.gc", {"4\n"});
  runner.run_static_test(env, testCategory, "subtract-let.static.gc", {"3\n"});
}
