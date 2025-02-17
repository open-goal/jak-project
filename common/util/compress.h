#pragma once

#include <cstddef>
#include <vector>

#include "common/common_types.h"
namespace compression {
// compress and decompress data with zstd
std::vector<u8> compress_zstd(const void* data, size_t size);
std::vector<u8> decompress_zstd(const void* data, size_t size);
std::vector<u8> compress_zstd_no_header(const void* data, size_t size);
}  // namespace compression
