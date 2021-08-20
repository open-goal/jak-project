#include "lzokay.hpp"
#include <cstring>

int compress_and_decompress(const uint8_t* data, std::size_t length) {
  lzokay::EResult error;

  /* This variable and 5th parameter of compress() is optional, but may
   * be reused across multiple compression runs; avoiding repeat
   * allocation/deallocation of the work memory used by the compressor.
   */
  lzokay::Dict<> dict;

  std::size_t compressed_size = lzokay::compress_worst_size(length);
  std::unique_ptr<uint8_t[]> compressed(new uint8_t[compressed_size]);
  error = lzokay::compress(data, length, compressed.get(), compressed_size,
                           compressed_size, dict);
  if (error < lzokay::EResult::Success)
    return 1;

  std::unique_ptr<uint8_t[]> decompressed(new uint8_t[length]);
  std::size_t decompressed_size;
  error = lzokay::decompress(compressed.get(), compressed_size,
                             decompressed.get(), length, decompressed_size);
  if (error < lzokay::EResult::Success)
    return 1;

  if (std::memcmp(data, decompressed.get(), decompressed_size) != 0)
    return 1;

  return 0;
}

int main(int argc, char** argv) {
  const char* testdata = "Hello World!";
  int ret = compress_and_decompress(reinterpret_cast<const uint8_t*>(testdata), 12);
  return ret;
}
