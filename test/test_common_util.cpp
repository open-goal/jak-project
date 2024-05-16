#include <limits>
#include <string>
#include <unordered_set>
#include <vector>

#include "common/util/Assert.h"
#include "common/util/BitUtils.h"
#include "common/util/CopyOnWrite.h"
#include "common/util/FileUtil.h"
#include "common/util/Range.h"
#include "common/util/SmallVector.h"
#include "common/util/Trie.h"
#include "common/util/crc32.h"
#include "common/util/json_util.h"
#include "common/util/os.h"
#include "common/util/print_float.h"

#include "gtest/gtest.h"
#include "test/all_jak1_symbols.h"

#include "fmt/core.h"

TEST(CommonUtil, CpuInfo) {
  setup_cpu_info();
}

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

TEST(CommonUtil, FloatToString) {
  float test_floats[] = {0.f,
                         1.f,
                         -1.f,
                         0.1f,
                         -0.1f,
                         1234,
                         12340,
                         123400,
                         -1234000,
                         0.00342f,
                         -0.003423f,
                         std::numeric_limits<float>::min(),
                         std::numeric_limits<float>::max(),
                         std::numeric_limits<float>::lowest(),
                         std::numeric_limits<float>::epsilon(),
                         std::numeric_limits<float>::denorm_min(),
                         -std::numeric_limits<float>::min(),
                         -std::numeric_limits<float>::max(),
                         -std::numeric_limits<float>::lowest(),
                         -std::numeric_limits<float>::epsilon(),
                         -std::numeric_limits<float>::denorm_min()};

  for (auto x : test_floats) {
    EXPECT_TRUE(x == (float)std::stod(float_to_string(x)));
  }

  // all three of these constants should become _exactly_ 1460961.25 when converted to a float.
  // to break a tie, dragonbox defaults to round to even, which is nice because that's the
  // default rounding mode.
  EXPECT_EQ("1460961.2", float_to_string(1460961.25));
  EXPECT_EQ("1460961.2", float_to_string(1460961.20));
  EXPECT_EQ("1460961.2", float_to_string(1460961.30));
}

TEST(CommonUtil, PowerOfTwo) {
  EXPECT_EQ(get_power_of_two(0), std::nullopt);
  EXPECT_EQ(get_power_of_two(1), 0);
  EXPECT_EQ(get_power_of_two(2), 1);
  EXPECT_EQ(get_power_of_two(3), std::nullopt);
  EXPECT_EQ(get_power_of_two(4), 2);
  EXPECT_EQ(get_power_of_two(u64(1) << 63), 63);
}

TEST(CommonUtil, CopyOnWrite) {
  CopyOnWrite<int> x(2);

  EXPECT_EQ(*x, 2);
  *x.mut() = 3;
  EXPECT_EQ(*x, 3);

  CopyOnWrite<int> y = x;
  EXPECT_EQ(*x, 3);
  EXPECT_EQ(*y, 3);
  EXPECT_EQ(x.get(), y.get());

  *x.mut() = 12;
  EXPECT_EQ(*x, 12);
  EXPECT_EQ(*y, 3);

  x = y;
  EXPECT_EQ(*x, 3);
  EXPECT_EQ(*y, 3);
  EXPECT_EQ(x.get(), y.get());

  y = x;
  EXPECT_EQ(*x, 3);
  EXPECT_EQ(*y, 3);
  EXPECT_EQ(x.get(), y.get());

  EXPECT_TRUE(x);
  EXPECT_TRUE(y);

  CopyOnWrite<int> z;
  EXPECT_FALSE(z);

  z = x;
  EXPECT_TRUE(z);
  EXPECT_EQ(x.get(), z.get());
  *z.mut() = 15;
  EXPECT_EQ(*x, 3);
  EXPECT_EQ(*y, 3);
  EXPECT_EQ(*z, 15);
}

namespace cu {
namespace test {

class ThrowOnDefaultConstruct {
 public:
  ThrowOnDefaultConstruct() {
    throw std::runtime_error("ThrowOnDefaultConstruct was default constructed.");
  }
};

class ThrowOnDestruct {
 public:
  ~ThrowOnDestruct() {
    // not a good idea to throw.
    exit(-1);
  }
};

struct RuleOfFiveExample {
  // if we fail to call the destructor we'll leak memory, which will get caught with valgrind.
  RuleOfFiveExample() { mem = new int; }
  RuleOfFiveExample(const RuleOfFiveExample& other) {
    if (&other != this) {
      mem = new int;
    }
  }
  RuleOfFiveExample(RuleOfFiveExample&& other) noexcept {
    if (&other != this) {
      mem = other.mem;
      other.mem = nullptr;
    }
  }

  RuleOfFiveExample& operator=(const RuleOfFiveExample& other) {
    if (&other != this) {
      delete mem;
      mem = new int;
    }
    return *this;
  }

  RuleOfFiveExample& operator=(RuleOfFiveExample&& other) noexcept {
    if (&other != this) {
      mem = other.mem;
      other.mem = nullptr;
    }
    return *this;
  }

  ~RuleOfFiveExample() { delete mem; }
  int value = 12;
  int* mem;
};

TEST(SmallVector, NoConstruction) {
  // Confirm that an empty vector constructs nothing.
  SmallVector<ThrowOnDefaultConstruct, 128> empty;
  EXPECT_EQ(empty.size(), 0);
  EXPECT_TRUE(empty.empty());

  // should also destroy nothing
  SmallVector<ThrowOnDestruct> empty2;
}

TEST(SmallVector, ConstructWithSize) {
  // Test construction calls default constructors.
  SmallVector<RuleOfFiveExample, 1> heap_no_stack(12);
  SmallVector<RuleOfFiveExample, 12> full_stack(12);
  SmallVector<RuleOfFiveExample, 12> not_full_stack(11);
  SmallVector<RuleOfFiveExample, 12> overflow_to_heap(13);

  // size
  EXPECT_EQ(heap_no_stack.size(), 12);
  EXPECT_EQ(full_stack.size(), 12);
  EXPECT_EQ(not_full_stack.size(), 11);
  EXPECT_EQ(overflow_to_heap.size(), 13);

  // capacity
  EXPECT_EQ(heap_no_stack.capacity(), 12);
  EXPECT_EQ(full_stack.capacity(), 12);
  EXPECT_EQ(not_full_stack.capacity(), 12);
  EXPECT_EQ(overflow_to_heap.capacity(), 13);

  // were they constructed?
  int i = 0;
  for (auto& obj : heap_no_stack) {
    EXPECT_EQ(obj.value, 12);
    i++;
  }
  EXPECT_EQ(i, 12);

  for (auto vec : {full_stack, not_full_stack, overflow_to_heap}) {
    int j = 0;
    for (auto& obj : heap_no_stack) {
      EXPECT_EQ(obj.value, 12);
      j++;
    }
    EXPECT_EQ(j, 12);
  }
}

// small std::string's aren't heap allocated.
constexpr const char* long_string_1 = "this-is-a-string-thats-long-enough-to-go-on-the-heap!";
constexpr const char* long_string_2 = "another-string-thats-long-enough-to-go-on-the-heap!";
constexpr const char* long_string_3 = "also-long-enough-to-go-on-the-heap!";

TEST(SmallVector, ConstructByCopying) {
  // test that we copy the input properly.
  SmallVector<std::string> strings(20, long_string_1);
  EXPECT_EQ(strings[0], long_string_1);
  strings[0] = long_string_2;
  EXPECT_EQ(strings[1], long_string_1);
  EXPECT_EQ(strings.size(), 20);
}

TEST(SmallVector, ConstructFromIterator) {
  std::unordered_set<std::string> stuff;
  for (auto x : Range(10, 20)) {
    stuff.insert(long_string_1 + std::to_string(x));
  }

  // iterators into unordered set can't be subtracted, but this should still work.
  SmallVector<std::string> strings(stuff.begin(), stuff.end());
  EXPECT_EQ(strings.size(), 10);
  std::unordered_set<std::string> stuff2(strings.begin(), strings.end());
  EXPECT_EQ(stuff, stuff2);

  // these can be subtracted.
  SmallVector<std::string> strings2(strings.begin(), strings.end());
  EXPECT_EQ(strings, strings2);
  EXPECT_EQ(strings.at(1), strings2.at(1));
  strings.at(1) = long_string_2;
  EXPECT_TRUE(strings.at(1) != strings2.at(1));
}

TEST(SmallVector, ConstructFromCopy) {
  SmallVector<std::string> one = {long_string_1, long_string_2, long_string_3};
  SmallVector<std::string> two(one);
  EXPECT_EQ(two.at(2), long_string_3);
  two.at(2) = "four";
  EXPECT_EQ(one.at(2), long_string_3);
}

TEST(SmallVector, ConstructFromMoveInline) {
  // stack move
  SmallVector<std::string, 20> one = {long_string_1, long_string_2, long_string_3};
  SmallVector<std::string, 20> two(std::move(one));
  EXPECT_TRUE(one.empty());  // this is the convention of SmallVector.
  EXPECT_EQ(two.at(2), long_string_3);
}

TEST(SmallVector, ConstructFromMoveHeap) {
  // heap move
  SmallVector<std::string, 1> one = {long_string_1, long_string_2, long_string_3};
  SmallVector<std::string, 1> two(std::move(one));
  EXPECT_TRUE(one.empty());  // this is the convention of SmallVector.
  EXPECT_EQ(two.at(2), long_string_3);
  EXPECT_EQ(two.size(), 3);
}

TEST(SmallVector, ConstructFromInitList) {
  SmallVector<std::string, 1> one({long_string_1, long_string_2, long_string_3});
  EXPECT_EQ(one.at(2), long_string_3);
  EXPECT_EQ(one.size(), 3);
}

/*
TEST(SmallVector, SelfCopyAndMoveAssignment) {
  SmallVector<std::string, 0> one({long_string_1, long_string_2, long_string_3});
  one = one;
  one = std::move(one);
  EXPECT_EQ(one.at(2), long_string_3);
  EXPECT_EQ(one.size(), 3);
}
*/

TEST(SmallVector, CopyAssign) {
  // heap -> heap
  SmallVector<std::string, 1> heap_one({long_string_1, long_string_2, long_string_3}),
      heap_two({"a", "b"});
  heap_one = heap_two;
  EXPECT_TRUE(heap_one.size() == 2);
  EXPECT_TRUE(heap_one[0] == "a");
  EXPECT_TRUE(heap_one[1] == "b");
}

TEST(SmallVector, Construction) {
  SmallVector<ThrowOnDefaultConstruct, 128> empty;
  EXPECT_EQ(empty.size(), 0);
  EXPECT_TRUE(empty.empty());
  empty.reserve(256);
  EXPECT_EQ(empty.size(), 0);
  EXPECT_TRUE(empty.empty());
  empty.shrink_to_fit();
  EXPECT_EQ(empty.capacity(), 128);

  SmallVector<int, 2> one(1);
  EXPECT_EQ(one.size(), 1);
  EXPECT_FALSE(one.empty());
}

#ifndef NO_ASSERT
TEST(Assert, Death) {
  EXPECT_DEATH(private_assert_failed("foo", "bar", 12, "aaa"), "");
}
#endif

uint32_t crc_reference(const u8* data, size_t size) {
  u32 crc = 0xffffffff;
  while (size--) {
    crc ^= *data++;
    for (int k = 0; k < 8; k++)
      crc = crc & 1 ? (crc >> 1) ^ 0x82f63b78 : crc >> 1;
  }
  return ~crc;
}

TEST(CRC32, Reference) {
  for (u32 so = 0; so < 7; so++) {
    std::vector<u8> test_data;
    for (u32 i = 0; i < 1024 + so; i++) {
      test_data.push_back(i & 0xff);
    }
    EXPECT_EQ(crc_reference(test_data.data(), test_data.size()),
              crc32(test_data.data(), test_data.size()));
  }
}

}  // namespace test
}  // namespace cu
