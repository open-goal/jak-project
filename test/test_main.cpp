#include "gtest/gtest.h"

#include "common/util/FileUtil.h"

#include <filesystem>
#ifdef _WIN32
#include <Windows.h>
#endif

// Running subsets of tests, see:
// -
// https://github.com/google/googletest/blob/620659ed92829a88ee34134c782bf5b5aa5a0a0c/googletest/docs/advanced.md#running-a-subset-of-the-tests
// This can set via:
// - --gtest_filter="" CLI arg
// - 'GTEST_FILTER' environment variable,
// - or below in code by adding `::testing::GTEST_FLAG(filter) = "Test_Cases1*";` below
//
// I've set things up so VS has a run configuration that runs all tests with "Draft" in the name
// to make it easier to test a subset of tests

int main(int argc, char** argv) {
#ifdef _WIN32
  // Always enable VIRTUAL_TERMINAL_PROCESSING, this console mode allows the console (stdout) to
  // support ANSI colors in the outputted text, which are used by the logging tool.
  // This mode may not be enabled by default, and changing that involves modifying the registry,
  // so it seems like a better solution would be enabling it ourselves.

  // Get handle to stdout
  HANDLE hStdOut = GetStdHandle(STD_OUTPUT_HANDLE);
  // get current stdout mode
  DWORD modeStdOut;
  GetConsoleMode(hStdOut, &modeStdOut);
  // enable VIRTUAL_TERMINAL_PROCESSING. As a bitwise OR it will not do anything if it is
  // already set
  modeStdOut |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
  SetConsoleMode(hStdOut, modeStdOut);
#endif
  ::testing::InitGoogleTest(&argc, argv);

  // Re-init failed folder
  std::string failedFolder = file_util::get_file_path({"test/goalc/source_generated/failed/"});
  if (std::filesystem::exists(failedFolder)) {
    std::filesystem::remove_all(failedFolder);
  }
  std::filesystem::create_directory(failedFolder);

  return RUN_ALL_TESTS();
}
