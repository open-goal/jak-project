// https://github.com/google/googletest/blob/master/googletest/docs/advanced.md#value-parameterized-tests

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

// TODO - put into the framework

#include <iostream>
#include <string>
#include <cstdio>
#include <sstream>
#include <iostream>
#include <random>

struct IntegerParam {
  s64 val;
  bool hex;

  IntegerParam(s64 val, bool hex = false) : val(val), hex(hex) {}

  std::string toLisp() {
    // Append hex reader macro '#x'
    if (hex) {
      return std::string("#x") + std::string(std::to_string(val));
    }
    return std::to_string(val);
  }

  std::string eval() {
    if (hex) {
      int64_t hexVal;
      std::stringstream ss;
      ss << std::hex << std::to_string(val);
      ss >> hexVal;
      return std::string(std::to_string(hexVal)) + "\n";
    }
    if (val == 123)
			return std::to_string(val);
    return std::to_string(val) + "\n";
  }
};

// TODO - make sure i log the input/output if there is a failure
// - maybe i don't have to, the last test may exit and the file would remain?

class IntegerTests : public testing::TestWithParam<IntegerParam> {};

TEST_P(IntegerTests, IntegerTests) {
  // TODO - might be slow if we open / close the thread for each test.
  // we might want to persist the compiler/test runner instance long term...shouldn't be that
  // difficult, this is C++ right..no rules! pointers pointers pointers.
  std::thread runtime_thread(GoalTest::runtime_no_kernel);
  Compiler compiler;
  GoalTest::CompilerTestRunner runner;
  runner.c = &compiler;

  // With separate input and output path
  std::string templateDir = file_util::get_file_path({"test/goalc/source_templates/"});
  std::string generatedDir = file_util::get_file_path({"test/goalc/source_generated/"});
  inja::Environment env{templateDir, generatedDir};

  IntegerParam param = GetParam();

  nlohmann::json data;
  data["integer"] = param.toLisp();

  env.write("integer-test.template.gc", data, "integer-test.generated.gc");

  runner.run_test("integer-test.generated.gc", {param.eval()});

  compiler.shutdown_target();
  runtime_thread.join();
  runner.print_summary();
}

// Generates a collection of evenly distributed tests
std::vector<IntegerParam> genIntegerTests(int numTests, bool includeHex, bool includeNegative) {
  std::vector<IntegerParam> tests;
	std::random_device dev;
  std::mt19937 rng(dev());
  std::uniform_int_distribution<std::mt19937::result_type> dist6(0, UINT32_MAX);
  int testCases = includeNegative ? 2 : 1;
  if (includeHex) {
    testCases *= 2;
  }
  for (int i = 0; i < numTests; i++) {
    switch (i % testCases) {
			case 0:
				tests.push_back(IntegerParam(dist6(rng)));
				break;
			case 1:
				tests.push_back(IntegerParam(dist6(rng) * -1));
				break;
			case 2:
				tests.push_back(IntegerParam(dist6(rng), true));
				tests.push_back(IntegerParam(123));
				break;
			case 3:
				tests.push_back(IntegerParam(dist6(rng) * -1, true));
				break;
		}
  }
	return tests;
}

// TODO - don't really need generated tests here, proof of concept
// specific examples for integers is more than enough
INSTANTIATE_TEST_SUITE_P(InstantiationName,
                         IntegerTests,
                         testing::ValuesIn(genIntegerTests(10, true, true)));
