// TODO - eventually replace our `goalc` tests with this setup
// A simple test runner framework for debugging / iterating on the formatter
// Tests are defined in files as such:

/*
===
TEST NAME
===

INPUT

---

EXPECTED OUTPUT

*/

// Test files can contain multiple tests, upon running we will recurse a directory
// looking for any `.test` files and run them through the framework
//
// Any differences will be diff'd and displayed

#include "common/formatter/formatter.h"
#include "common/util/FileUtil.h"
#include "common/util/string_util.h"

#include "gtest/gtest.h"

#include "third-party/fmt/core.h"

struct TestDefinition {
  std::string name;
  std::string input;
  std::string output;
};

bool run_tests(fs::path file_path) {
  // Read in the file, and run the test
  const auto contents = str_util::split(file_util::read_text_file(file_path));
  std::vector<TestDefinition> tests;
  TestDefinition curr_test;
  size_t i = 0;
  while (i < contents.size()) {
    const auto& line = contents.at(i);
    if (line == "===") {
      curr_test = TestDefinition();
      curr_test.name = contents.at(i + 1);
      i += 3;
      continue;
    }
    // Parse the input and output
    if (!curr_test.name.empty() && line.empty()) {
      i++;
      while (true) {
        if (contents.at(i) == "---") {
          i++;
          curr_test.input = str_util::trim(curr_test.input);
          break;
        }
        curr_test.input += contents.at(i) + "\n";
        i++;
      }
      i++;
      while (true) {
        if (i == contents.size() || contents.at(i) == "===") {
          curr_test.output = str_util::trim(curr_test.output);
          tests.push_back(curr_test);
          break;
        }
        curr_test.output += contents.at(i) + "\n";
        i++;
      }
      continue;
    }
  }
  // Run the tests, report successes and failures
  bool test_failed = false;
  fmt::print("{}:\n", file_util::base_name(file_path.string()));
  for (const auto& test : tests) {
    const auto formatted_result = formatter::format_code(test.input);
    if (formatted_result != test.output) {
      fmt::print("  ❌ - {}\n", test.name);
      fmt::print("{}\n", str_util::diff(test.output, formatted_result));
      test_failed = true;
    } else {
      fmt::print("  ✅ - {}\n", test.name);
    }
  }
  return test_failed;
}

bool find_and_run_tests() {
  // Enumerate test files
  const auto test_files = file_util::find_files_recursively(
      file_util::get_file_path({"test/common/formatter/corpus"}), std::regex("^.*\.test$"));
  bool failed = false;
  for (const auto& file : test_files) {
    failed = run_tests(file);
  }
  return !failed;
}

TEST(Formatter, FormatterTests) {
  EXPECT_TRUE(find_and_run_tests());
}
