#include <string>
#include <vector>

#include "common/util/FileUtil.h"
#include "common/util/Trie.h"
#include "common/util/BitUtils.h"
#include "gtest/gtest.h"
#include "test/all_jak1_symbols.h"
#include "common/util/json_util.h"
#include "common/util/Range.h"
#include "third-party/fmt/core.h"

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

TEST(CommonUtil, StripComments) {
  std::string test_input =
      R"(
test "asdf /* y */ /////a\"bcd"
///////// commented out!
// /*  also commented out

/* this is a block comment "with an unterminated string.
*/ and its done
)";

  std::string test_expected =
      R"(
test "asdf /* y */ /////a\"bcd"



 and its done
)";

  EXPECT_EQ(strip_cpp_style_comments(test_input), test_expected);
}

TEST(CommonUtil, RangeIterator) {
  std::vector<int> result = {}, expected_result = {4, 5, 6, 7};

  for (auto x : Range<int>(4, 8)) {
    result.push_back(x);
  }

  EXPECT_EQ(result, expected_result);
  EXPECT_TRUE(Range<int>().empty());
  EXPECT_FALSE(Range<int>(3, 4).empty());
  EXPECT_EQ(1, Range<int>(3, 4).size());
  EXPECT_EQ(4, Range<int>(4, 8).size());
}

TEST(CommonUtil, BitRange) {
  for (int x : {0, 0b1001, 0b1010, 0b01110001, 0b000100110}) {
    EXPECT_EQ(get_bit_range(x), std::nullopt);  // invalids
  }

  EXPECT_EQ(get_bit_range(0b1), Range<int>(0, 1));
  EXPECT_EQ(get_bit_range(0b10), Range<int>(1, 2));
  EXPECT_EQ(get_bit_range(0b11), Range<int>(0, 2));
  EXPECT_EQ(get_bit_range(0b110), Range<int>(1, 3));
  EXPECT_EQ(get_bit_range(UINT64_MAX), Range<int>(0, 64));
  EXPECT_EQ(get_bit_range(UINT64_MAX - 1), Range<int>(1, 64));
  EXPECT_EQ(get_bit_range(UINT64_MAX / 2), Range<int>(0, 63));
}