#include "gtest/gtest.h"

#include "common/util/FileUtil.h"
#include "spdlog/spdlog.h"

#include <filesystem>

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);

  // Re-init failed folder
  std::string failedFolder = file_util::get_file_path({"test/goalc/source_generated/failed/"});
  if (std::filesystem::exists(failedFolder)) {
    std::filesystem::remove_all(failedFolder);
  }
  std::filesystem::create_directory(failedFolder);

  // verbose logging, for debug:
  spdlog::set_level(spdlog::level::debug);

  return RUN_ALL_TESTS();
}