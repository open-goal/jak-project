#include "common/util/FileUtil.h"
#include "common/util/Trie.h"
#include "gtest/gtest.h"
#include "test/all_jak1_symbols.h"
#include <string>
#include <vector>

TEST(CommonUtil, get_file_path) {
  std::vector<std::string> test = {"cabbage", "banana", "apple"};
  std::string sampleString = file_util::get_file_path(test);
  // std::cout << sampleString << std::endl;

  EXPECT_TRUE(true);
}

TEST(CommonUtil, Trie) {
  Trie<std::string> test;

  std::vector<std::string> strings;

  for (auto x : all_syms) {
    strings.push_back(x);
    test.insert(strings.back(), strings.back());
  }

  auto cam_prefix = test.lookup_prefix("cam");
  EXPECT_EQ(cam_prefix.size(), 184);
  EXPECT_EQ(test.lookup("not-in-the-list"), nullptr);
  EXPECT_EQ(test.lookup("cam"), nullptr);
  EXPECT_NE(test.lookup("energydoor-closed-till-near"), nullptr);
  EXPECT_EQ(7941, test.lookup_prefix("").size());

  EXPECT_TRUE(test.lookup("") == nullptr);
  EXPECT_TRUE(test.lookup("p") == nullptr);
  EXPECT_TRUE(test.lookup("pa") == nullptr);
  EXPECT_TRUE(test.lookup("pat") == nullptr);
  EXPECT_FALSE(test.lookup("path") == nullptr);
  EXPECT_FALSE(test.lookup("path1") == nullptr);
  EXPECT_TRUE(test.lookup("path-") == nullptr);
  EXPECT_FALSE(test.lookup("path1-k") == nullptr);
}