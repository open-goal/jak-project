
#include "test_runner.h"
#include "third-party/fmt/core.h"

#include <string>

#include "gtest/gtest.h"
#include "inja.hpp"
#include "third-party/json.hpp"

#include "game/runtime.h"
#include "goalc/listener/Listener.h"
#include "goalc/compiler/Compiler.h"

#include "common/util/FileUtil.h"
#include <filesystem>

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

void CompilerTestRunner::run_static_test(inja::Environment& env,
                                         std::string& testCategory,
                                         const std::string& test_file,
                                         const std::vector<std::string>& expected,
                                         std::optional<int> truncate) {
  env.write(test_file, {}, test_file);
  run_test(testCategory, test_file, expected, truncate);
}

void CompilerTestRunner::run_test(const std::string& test_category,
                                  const std::string& test_file,
                                  const std::vector<std::string>& expected,
                                  std::optional<int> truncate) {
  fprintf(stderr, "Testing %s\n", test_file.c_str());
  auto result =
      c->run_test_from_file("test/goalc/source_generated/" + test_category + "/" + test_file);
  if (truncate.has_value()) {
    for (auto& x : result) {
      x = x.substr(0, truncate.value());
    }
  }

  bool assertionFailed = false;
  EXPECT_EQ(result, expected) << (assertionFailed = true);

  if (assertionFailed) {
    std::string testFile = GoalTest::getGeneratedDir(test_category) + test_file;
    std::string failedFile = GoalTest::getFailedDir(test_category) + test_file;

    GoalTest::createDirIfAbsent(GoalTest::getFailedDir(test_category));

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

void CompilerTestRunner::run_always_pass(const std::string& test_category,
                                         const std::string& test_file) {
  c->run_test_from_file("test/goalc/source_generated/" + test_category + "/" + test_file);
  tests.push_back({{}, {}, test_file, true});
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

void runtime_with_kernel_no_debug_segment() {
  constexpr int argc = 3;
  const char* argv[argc] = {"", "-fakeiso", "-debug-mem"};
  exec_runtime(argc, const_cast<char**>(argv));
}

void createDirIfAbsent(const std::string& path) {
  if (!std::filesystem::is_directory(path) || !std::filesystem::exists(path)) {
    std::filesystem::create_directory(path);
  }
}
std::string getTemplateDir(const std::string& category) {
  return file_util::get_file_path({"test/goalc/source_templates", category + "/"});
}
std::string getGeneratedDir(const std::string& category) {
  return file_util::get_file_path({"test/goalc/source_generated", category + "/"});
}
std::string getFailedDir(const std::string& category) {
  return file_util::get_file_path({"test/goalc/source_generated/failed", category + "/"});
}
}  // namespace GoalTest
