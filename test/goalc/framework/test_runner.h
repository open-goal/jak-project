#pragma once

#include <string>
#include <vector>

#include "inja.hpp"
#include "goalc/compiler/Compiler.h"

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

  void run_static_test(inja::Environment& env,
                       std::string& testCategory,
                       const std::string& test_file,
                       const std::vector<std::string>& expected,
                       MatchParam<int> truncate = {});

  void run_test(const std::string& test_category,
                const std::string& test_file,
                const std::vector<std::string>& expected,
                MatchParam<int> truncate = {});

  void run_always_pass(const std::string& test_category, const std::string& test_file);

  void print_summary();
};

void runtime_no_kernel();
void runtime_with_kernel();

void createDirIfAbsent(const std::string& path);
std::string getTemplateDir(const std::string& category);
std::string getGeneratedDir(const std::string& category);
std::string getFailedDir(const std::string& category);

}  // namespace GoalTest
