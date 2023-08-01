
#include "test_runner.h"

#include <string>

#include "inja.hpp"

#include "common/util/FileUtil.h"

#include "game/runtime.h"
#include "goalc/compiler/Compiler.h"
#include "goalc/listener/Listener.h"
#include "gtest/gtest.h"

#include "third-party/fmt/core.h"
#include "third-party/json.hpp"

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

std::string CompilerTestRunner::test_file_name(std::string templateStr) {
  const ::testing::TestInfo* const test_info =
      ::testing::UnitTest::GetInstance()->current_test_info();
  std::string outFile = fmt::format(templateStr, test_info->name());
  std::replace(outFile.begin(), outFile.end(), '/', '_');
  return outFile;
}

void CompilerTestRunner::run_static_test(inja::Environment& env,
                                         std::string& testCategory,
                                         const std::string& test_file,
                                         const std::vector<std::string>& expected,
                                         std::optional<int> truncate) {
  env.write(test_file, {}, test_file);
  run_test(testCategory, test_file, expected, truncate);
}

void CompilerTestRunner::run_static_test(std::string& testCategory,
                                         const std::string& test_file,
                                         const std::vector<std::string>& expected,
                                         std::optional<int> truncate) {
  auto env = getInjaEnvironment(testCategory);
  run_static_test(env, testCategory, test_file, expected, truncate);
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

void runtime_no_kernel_jak1() {
  constexpr int argc = 5;
  const char* argv[argc] = {"", "-fakeiso", "-debug", "-nokernel", "-nosound"};
  GameLaunchOptions game_options;
  game_options.disable_display = true;
  exec_runtime(game_options, argc, argv);
}

void runtime_no_kernel_jak2() {
  constexpr int argc = 5;
  const char* argv[argc] = {"", "-fakeiso", "-debug", "-nokernel", "-nosound"};
  GameLaunchOptions game_options;
  game_options.disable_display = true;
  game_options.game_version = GameVersion::Jak2;
  exec_runtime(game_options, argc, argv);
}

void runtime_with_kernel_jak1() {
  constexpr int argc = 4;
  const char* argv[argc] = {"", "-fakeiso", "-debug", "-nosound"};
  GameLaunchOptions game_options;
  game_options.disable_display = true;
  exec_runtime(game_options, argc, argv);
}

void runtime_with_kernel_jak2() {
  constexpr int argc = 4;
  const char* argv[argc] = {"", "-fakeiso", "-debug", "-nosound"};
  GameLaunchOptions game_options;
  game_options.disable_display = true;
  game_options.game_version = GameVersion::Jak2;
  exec_runtime(game_options, argc, argv);
}

void runtime_with_kernel_no_debug_segment() {
  constexpr int argc = 4;
  const char* argv[argc] = {"", "-fakeiso", "-debug-mem", "-nosound"};
  GameLaunchOptions game_options;
  game_options.disable_display = true;
  exec_runtime(game_options, argc, argv);
}

void createDirIfAbsent(const std::string& path) {
  if (!fs::is_directory(path) || !fs::exists(path)) {
    fs::create_directory(path);
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

inja::Environment getInjaEnvironment(const std::string& category) {
  return inja::Environment(getTemplateDir(category), getGeneratedDir(category));
}

}  // namespace GoalTest
