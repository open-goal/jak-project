

#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/os.h"
#include "common/util/unicode_util.h"

#include "gtest/gtest.h"

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
  ArgumentGuard u8_guard(argc, argv);

  // hopefully get a debug print on github actions
  setup_cpu_info();
  file_util::setup_project_path(std::nullopt);
  lg::initialize();

  ::testing::InitGoogleTest(&argc, argv);

  // Re-init failed folder
  std::string failedFolder = file_util::get_file_path({"test/goalc/source_generated/failed/"});
  if (fs::exists(failedFolder)) {
    fs::remove_all(failedFolder);
  }
  fs::create_directory(failedFolder);

  return RUN_ALL_TESTS();
}
