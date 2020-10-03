
#include "test_runner.h"

#include <string>

#include "gtest/gtest.h"
#include "third-party/inja.hpp"
#include "third-party/json.hpp"

#include "game/runtime.h"
#include "goalc/listener/Listener.h"
#include "goalc/compiler/Compiler.h"

#include "common\util\FileUtil.h"

namespace GoalTest {

std::string escaped_string(const std::string& in) {
  std::string result;
  for (auto x : in) {
    switch (x) {
      case '\n':
        result.append("\\n");
        break;
      case '\t':
        result.append("\\t");
        break;
      default:
        result.push_back(x);
    }
  }
  return result;
}

void CompilerTestRunner::run_test(const std::string& test_file,
                                  const std::vector<std::string>& expected,
                                  MatchParam<int> truncate) {
  fprintf(stderr, "Testing %s\n", test_file.c_str());
  auto result = c->run_test("test/goalc/source_generated/" + test_file);
  if (!truncate.is_wildcard) {
    for (auto& x : result) {
      x = x.substr(0, truncate.value);
    }
  }

  EXPECT_EQ(result, expected);

  if (testing::Test::HasFailure()) {
    std::string testFile = file_util::get_file_path({"test/goalc/source_generated/" + test_file});
    // TODO - put the expected and unexpected values as comments in the file as well
    std::string failedFile =
        file_util::get_file_path({"test/goalc/source_generated/failed/" + test_file});

    std::ifstream src(testFile, std::ios::binary);
    std::ofstream dst(failedFile, std::ios::binary);

    std::string testOutput = "\n\n;------TEST OUTPUT------\n;-------Expected-------\n";

    for (auto& x : expected) {
      testOutput += fmt::format("; \"{}\"\n", escaped_string(x));
    }
    testOutput += "\n;--------Actual--------\n";
    for (auto& x : result) {
      testOutput += fmt::format("; \"{}\"\n", escaped_string(x));
    }

    dst << src.rdbuf() << testOutput;
  }

  tests.push_back({expected, result, test_file, false});
}

void CompilerTestRunner::run_always_pass(const std::string& test_file) {
  c->run_test("test/goalc/source_generated/" + test_file);
  tests.push_back({{}, {}, test_file, true});
}

// TODO - This might not be necessary with the switch to parameterized tests
void CompilerTestRunner::print_summary() {
  fmt::print("~~ Compiler Test Summary for {} tests... ~~\n", tests.size());
  int passed = 0;
  int passable = 0;
  int auto_pass = 0;
  for (auto& test : tests) {
    if (test.auto_pass) {
      auto_pass++;
      fmt::print("[{:40}] AUTO-PASS!\n", test.test_name);
    } else {
      passable++;
      if (test.expected == test.actual) {
        fmt::print("[{:40}] PASS!\n", test.test_name);
        passed++;
      } else {
        fmt::print("[{:40}] FAIL!\n", test.test_name);
        fmt::print("expected:\n");
        for (auto& x : test.expected) {
          fmt::print(" \"{}\"\n", escaped_string(x));
        }
        fmt::print("result:\n");
        for (auto& x : test.actual) {
          fmt::print(" \"{}\"\n", escaped_string(x));
        }
      }
    }
  }
  fmt::print("Total: passed {}/{} passable tests, {} auto-passed\n", passed, passable, auto_pass);
}

std::vector<std::string> get_test_pass_string(const std::string& name, int n_tests) {
  return {fmt::format("Test \"{}\": {} Passes\n0\n", name, n_tests)};
}

void runtime_no_kernel() {
  constexpr int argc = 4;
  const char* argv[argc] = {"", "-fakeiso", "-debug", "-nokernel"};
  exec_runtime(argc, const_cast<char**>(argv));
}

void runtime_with_kernel() {
  constexpr int argc = 3;
  const char* argv[argc] = {"", "-fakeiso", "-debug"};
  exec_runtime(argc, const_cast<char**>(argv));
}
}  // namespace GoalTest
