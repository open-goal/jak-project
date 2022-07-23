// https://github.com/google/googletest/blob/master/googletest/docs/advanced.md#value-parameterized-tests

#include <chrono>
#include <iostream>
#include <random>
#include <sstream>
#include <string>
#include <thread>

#include "inja.hpp"

#include "game/runtime.h"
#include "goalc/compiler/Compiler.h"
#include "goalc/listener/Listener.h"
#include "gtest/gtest.h"
#include "test/goalc/framework/test_runner.h"

#include "third-party/json.hpp"

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
  // Each integer test has a signed value, and can be represented as hex or an integral
  s64 val;
  bool hex;

  // An index is needed to be explicitly set because I couldn't find a way to pull the test-index
  // number from google's API
  // TODO - if you can find a way, please improve!
  // But this is needed so we can uniquely save the template files, especially if they error out
  // Why? - since you may choose to generate random values, it's nice for them to be stored after
  // the tests complete.  Some tests may be complex as well
  int index;

  IntegerParam(s64 _val, bool _hex = false, int _index = 0) : val(_val), hex(_hex), index(_index) {}

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
  int test_index = 0;
  for (; test_index < numTests; test_index++) {
    switch (test_index % testCases) {
      case 0:
        tests.push_back(IntegerParam(dist6(rng), false, test_index));
        break;
      case 1:
        tests.push_back(IntegerParam((s64)dist6(rng) * -1, false, test_index));
        break;
      case 2:
        tests.push_back(IntegerParam(dist6(rng), true, test_index));
        break;
    }
  }

  for (int i = 0; i < int(additionalTests.size()); i++) {
    IntegerParam test = additionalTests.at(i);
    test.index = i + numTests - 1;
    tests.push_back(test);
  }

  for (auto x :
       {s64(UINT32_MAX), s64(INT32_MIN), s64(INT32_MAX), s64(0), s64(INT64_MAX), s64(INT64_MIN)}) {
    for (auto y : {-1, 0, 1}) {
      s64 value = x + s64(y);
      tests.emplace_back(value, false, test_index++);
    }
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
    runtime_thread = std::make_unique<std::thread>(std::thread(GoalTest::runtime_no_kernel_jak1));
    compiler = std::make_unique<Compiler>(GameVersion::Jak1);
    runner = std::make_unique<GoalTest::CompilerTestRunner>();
    runner->c = compiler.get();
  }

  // Per-test-suite tear-down.
  // Called after the last test in this test suite.
  static void TearDownTestSuite() {
    compiler->shutdown_target();
    runtime_thread->join();

    runtime_thread.reset();
    compiler.reset();
    runner.reset();
  }

  // You can define per-test set-up logic as usual.
  void SetUp() {
    GoalTest::createDirIfAbsent(GoalTest::getTemplateDir(testCategory));
    GoalTest::createDirIfAbsent(GoalTest::getGeneratedDir(testCategory));
  }

  // You can define per-test tear-down logic as usual.
  void TearDown() {}

  // Common Resources Across all Tests in the Suite
  static std::unique_ptr<std::thread> runtime_thread;
  static std::unique_ptr<Compiler> compiler;
  static std::unique_ptr<GoalTest::CompilerTestRunner> runner;

  // Just to promote better test organization, supports nesting the test files 1 directory deep
  std::string testCategory = "arithmetic";
  inja::Environment env{GoalTest::getTemplateDir(testCategory),
                        GoalTest::getGeneratedDir(testCategory)};
};

// You must initialize the static variables outside of the declaration, or you'll run into
// unresolved external errors
std::unique_ptr<std::thread> ArithmeticTests::runtime_thread;
std::unique_ptr<Compiler> ArithmeticTests::compiler;
std::unique_ptr<GoalTest::CompilerTestRunner> ArithmeticTests::runner;

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
  runner->run_test(testCategory, testFile, {param.eval()});
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
  runner->run_static_test(env, testCategory, "add-int-literals.static.gc", {"13\n"});
  runner->run_static_test(env, testCategory, "add-let.static.gc", {"7\n"});
}

TEST_F(ArithmeticTests, AddIntegerFunction) {
  runner->run_static_test(env, testCategory, "add-function.static.gc", {"21\n"});
}

TEST_F(ArithmeticTests, AddIntegerMultiple) {
  runner->run_static_test(env, testCategory, "add-int-multiple.static.gc", {"15\n"});
  runner->run_static_test(env, testCategory, "add-int-multiple-2.static.gc", {"15\n"});
}

TEST_F(ArithmeticTests, AddIntegerVariables) {
  runner->run_static_test(env, testCategory, "add-int-vars.static.gc", {"7\n"});
}

TEST_F(ArithmeticTests, AshFunction) {
  runner->run_static_test(env, testCategory, "ash.static.gc", {"18\n"});
}

TEST_F(ArithmeticTests, Division) {
  runner->run_static_test(env, testCategory, "divide-1.static.gc", {"6\n"});
  runner->run_static_test(env, testCategory, "divide-2.static.gc", {"7\n"});
}

TEST_F(ArithmeticTests, IntegerSymbol) {
  runner->run_static_test(env, testCategory, "negative-int-symbol.static.gc", {"-123\n"});
}

TEST_F(ArithmeticTests, Modulus) {
  runner->run_static_test(env, testCategory, "mod.static.gc", {"7\n"});
}

TEST_F(ArithmeticTests, Multiplication) {
  runner->run_static_test(env, testCategory, "multiply.static.gc", {"-12\n"});
  runner->run_static_test(env, testCategory, "multiply-let.static.gc", {"3\n"});
}

TEST_F(ArithmeticTests, NestedFunctionCall) {
  runner->run_static_test(env, testCategory, "nested-function.static.gc", {"10\n"});
}

TEST_F(ArithmeticTests, VariableShift) {
  runner->run_static_test(env, testCategory, "shiftvs.static.gc", {"11\n"});
}

TEST_F(ArithmeticTests, FixedShift) {
  // same math as the variable shift test, just using the fixed shift operators.
  runner->run_static_test(env, testCategory, "shift-fixed.static.gc", {"11\n"});
}

TEST_F(ArithmeticTests, Subtraction) {
  runner->run_static_test(env, testCategory, "subtract-1.static.gc", {"4\n"});
  runner->run_static_test(env, testCategory, "subtract-2.static.gc", {"4\n"});
  runner->run_static_test(env, testCategory, "subtract-let.static.gc", {"3\n"});
}

TEST_F(ArithmeticTests, Multiplication2) {
  runner->run_static_test(env, testCategory, "multiply32.static.gc", {"-1234478448\n"});
  runner->run_static_test(env, testCategory, "multiply64.static.gc", {"93270638141856400\n"});
}

TEST_F(ArithmeticTests, Constants) {
  runner->run_static_test(env, testCategory, "float.static.gc", {"1067316150\n"});
  runner->run_static_test(env, testCategory, "function-return-float-constant.static.gc",
                          {"3.14149\n0\n"});
}

TEST_F(ArithmeticTests, Operations) {
  runner->run_static_test(env, testCategory, "float-pow.static.gc", {"256\n0\n"});
  runner->run_static_test(env, testCategory, "float-product.static.gc", {"120.0000\n0\n"});
}

TEST_F(ArithmeticTests, Symbols) {
  runner->run_static_test(env, testCategory, "float-in-symbol.static.gc", {"2345.6000\n0\n"});
}

TEST_F(ArithmeticTests, Functions) {
  runner->run_static_test(env, testCategory, "float-function.static.gc", {"10.152\n0\n"});
  runner->run_static_test(
      env, testCategory, "nested-float-functions.static.gc",
      {"i 1.4400 3.4000\nr 10.1523\ni 1.2000 10.1523\nr 17.5432\n17.543 10.152\n0\n"});
}

TEST_F(ArithmeticTests, MinMax) {
  runner->run_static_test(env, testCategory, "float-max.static.gc", {"3.70\n0\n"});
  runner->run_static_test(env, testCategory, "float-min.static.gc", {"-1.20\n0\n"});
}

TEST_F(ArithmeticTests, LogicalOperators) {
  runner->run_static_test(env, testCategory, "logand.static.gc", {"4\n"});
  runner->run_static_test(env, testCategory, "logior.static.gc", {"60\n"});
  runner->run_static_test(env, testCategory, "logxor.static.gc", {"56\n"});
}

TEST_F(ArithmeticTests, Comparison) {
  runner->run_static_test(env, testCategory, "signed-int-compare.static.gc", {"12\n"});
}

TEST_F(ArithmeticTests, DivideSigns) {
  runner->run_static_test(env, testCategory, "divide-signs.static.gc",
                          {"fffffffffffffffb 7ffffffffffffffb fffffffffffffffd 55555552\n0\n"});
}

TEST_F(ArithmeticTests, ModUnsigned) {
  runner->run_static_test(env, testCategory, "mod-unsigned.static.gc", {"ffffffffffffffff 5\n0\n"});
}