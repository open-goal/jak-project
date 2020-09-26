#pragma once

#include <string>
#include <vector>

#include "goalc/compiler/Compiler.h"
#include "common\util\FileUtil.h"

namespace GoalTest {

std::string escaped_string(const std::string& in);

struct CompilerTestRunner {
 public:
  Compiler* c = nullptr;

  struct Test {
    std::vector<std::string> expected, actual;
    std::string test_name;
    bool auto_pass = false;
  };

  std::vector<Test> tests;

  void run_test(const std::string& test_file,
                const std::vector<std::string>& expected,
                MatchParam<int> truncate = {});

  void run_always_pass(const std::string& test_file);

  void print_summary();
};

std::vector<std::string> get_test_pass_string(const std::string& name, int n_tests);

void runtime_no_kernel();

void runtime_with_kernel();
}  // namespace GoalTest
