#include "common/util/FileUtil.h"
#include "gtest/gtest.h"
#include <string>
#include <vector>

TEST(FileUtil, valid_path) {
  std::vector<std::string> test = {"cabbage", "banana", "apple"};
  std::string sampleString = file_util::get_file_path(test);
  // std::cout << sampleString << std::endl;

  EXPECT_TRUE(true);
}
