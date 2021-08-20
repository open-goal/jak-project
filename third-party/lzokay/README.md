LZ👌
===

A minimal, C++14 implementation of the
[LZO compression format](http://www.oberhumer.com/opensource/lzo/).

Objective
---------

The implementation provides compression behavior similar to the
`lzo1x_999_compress` function in `lzo2` (i.e. higher compression, lower speed).
The implementation is fixed to the default parameters of the original and
provides no facilities for various compression "levels" or an initialization
dictionary.

The decompressor is compatible with data compressed by other LZO1X
implementations.

Usage
-----

```cpp
#include <lzokay.hpp>
#include <cstring>

int compress_and_decompress(const uint8_t* data, std::size_t length) {
  lzokay::EResult error;

  /* This variable and 6th parameter of compress() is optional, but may
   * be reused across multiple compression runs; avoiding repeat
   * allocation/deallocation of the work memory used by the compressor.
   */
  lzokay::Dict<> dict;

  std::size_t estimated_size = lzokay::compress_worst_size(length);
  std::unique_ptr<uint8_t[]> compressed(new uint8_t[estimated_size]);
  std::size_t compressed_size;
  error = lzokay::compress(data, length, compressed.get(), estimated_size,
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
```

License
-------

LZ👌 is available under the
[MIT License](https://github.com/jackoalan/lzokay/blob/master/LICENSE)
and has no external dependencies.
