#pragma once

/*!
 * @file StrFileReader.h
 * Utility class to read a .STR file and extract the full file name.
 */

#include <string>
#include <vector>

#include "common/common_types.h"
#include "common/util/FileUtil.h"

namespace decompiler {
class StrFileReader {
 public:
  explicit StrFileReader(const fs::path& file_path);
  int chunk_count() const;
  const std::vector<u8>& get_chunk(int idx) const;
  std::string get_full_name(const std::string& short_name) const;

 private:
  std::vector<std::vector<u8>> m_chunks;
};
}  // namespace decompiler
