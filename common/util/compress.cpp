#include <cstring>
#include <cstdio>

#include "compress.h"
#include "third-party/zstd/lib/zstd.h"
#include "common/util/assert.h"

namespace compression {
std::vector<u8> compress_zstd(const void* data, size_t size) {
  auto max_compressed = ZSTD_compressBound(size);
  std::vector<u8> result(sizeof(size_t) + max_compressed);
  memcpy(result.data(), &size, sizeof(size_t));
  auto compressed_size = ZSTD_compress(result.data() + sizeof(size_t), max_compressed, data, size,
                                       ZSTD_CLEVEL_DEFAULT);
  if (ZSTD_isError(compressed_size)) {
    printf("ZSTD error: %s\n", ZSTD_getErrorName(compressed_size));
    assert(false);
  }
  result.resize(sizeof(size_t) + compressed_size);
  return result;
}

std::vector<u8> decompress_zstd(const void* data, size_t size) {
  assert(size >= sizeof(size_t));
  size_t decompressed_size;
  memcpy(&decompressed_size, data, sizeof(size_t));
  size_t compressed_size = size - sizeof(size_t);

  std::vector<u8> result(decompressed_size);
  auto decomp_size = ZSTD_decompress(result.data(), decompressed_size,
                                     (const u8*)data + sizeof(size_t), compressed_size);
  if (ZSTD_isError(decomp_size)) {
    printf("ZSTD error: %s\n", ZSTD_getErrorName(compressed_size));
    assert(false);
  }

  assert(decomp_size == decompressed_size);
  return result;
}
}  // namespace compression