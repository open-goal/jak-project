#include <cstdio>
#include <string>
#include <vector>

#include "common/common_types.h"
#include "common/util/compress.h"

#include "gtest/gtest.h"
#include "test/all_jak1_symbols.h"

#include "third-party/zstd/lib/zstd.h"

TEST(ZSTD, Basic) {
  std::string all;
  for (auto& x : all_syms) {
    all.append(x);
    all.append("\n");
  }

  size_t in_size = all.size();
  printf("size: %d\n", (int)in_size);

  size_t max_out_size = ZSTD_compressBound(in_size);

  std::vector<u8> out(max_out_size);
  auto result =
      ZSTD_compress(out.data(), out.size(), all.data(), all.length(), ZSTD_CLEVEL_DEFAULT);
  ASSERT_FALSE(ZSTD_isError(result));
  printf("compressed: %d\n", (int)result);

  std::string uncompressed;
  uncompressed.resize(in_size);

  auto result2 = ZSTD_decompress(uncompressed.data(), uncompressed.size(), out.data(), result);
  if (ZSTD_isError(result2)) {
    printf("err: %s\n", ZSTD_getErrorName(result2));
  }
  ASSERT_FALSE(ZSTD_isError(result2));
  EXPECT_EQ(result2, in_size);
  EXPECT_EQ(all, uncompressed);
}

TEST(ZSTD, CommonLibrary) {
  std::string all;
  for (auto& x : all_syms) {
    all.append(x);
    all.append("\n");
  }

  auto compressed = compression::compress_zstd(all.data(), all.size());
  auto decompressed = compression::decompress_zstd(compressed.data(), compressed.size());

  ASSERT_EQ(decompressed.size(), all.size());
  for (size_t i = 0; i < all.length(); i++) {
    EXPECT_EQ(decompressed[i], all[i]);
  }

  EXPECT_TRUE(compressed.size() < 0.5 * all.size());
}