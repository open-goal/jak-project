#pragma once

#include <optional>
#include <string>
#include <vector>

#include "goalc/compiler/Compiler.h"

namespace inja {
class Environment;
}

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

  std::string test_file_name(std::string templateStr);

  void run_static_test(inja::Environment& env,
                       std::string& testCategory,
                       const std::string& test_file,
                       const std::vector<std::string>& expected,
                       std::optional<int> truncate = {});

  void run_static_test(std::string& testCategory,
                       const std::string& test_file,
                       const std::vector<std::string>& expected,
                       std::optional<int> truncate = {});

  void run_test(const std::string& test_category,
                const std::string& test_file,
                const std::vector<std::string>& expected,
                std::optional<int> truncate = {});

  void run_always_pass(const std::string& test_category, const std::string& test_file);
};

void runtime_no_kernel_jak1();
void runtime_no_kernel_jak2();
void runtime_with_kernel_jak1();
void runtime_with_kernel_jak2();
void runtime_with_kernel_no_debug_segment();

void createDirIfAbsent(const std::string& path);
std::string getTemplateDir(const std::string& category);
std::string getGeneratedDir(const std::string& category);
std::string getFailedDir(const std::string& category);

inja::Environment getInjaEnvironment(const std::string& category);

}  // namespace GoalTest
