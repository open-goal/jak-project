#include "common/util/FileUtil.h"
#include <iostream>
#include "gtest/gtest.h"

TEST(test, test) {
  
  std::string test[] = {"cabbage", "banana", "apple"};
  std::cout << FileUtil::get_file_path(test) << std::endl;

  EXPECT_TRUE(true);
}