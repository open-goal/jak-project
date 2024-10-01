#pragma once

/*!
 * @file StrFileReader.h
 * Utility class to read a .STR file and extract the full file name.
 */

#include <string>
#include <vector>

#include "common/common_types.h"
#include "common/util/Assert.h"
#include "common/util/FileUtil.h"
#include "common/versions/versions.h"

namespace decompiler {
class StrFileReader {
 public:
  explicit StrFileReader(const fs::path& file_path, GameVersion version);
  int chunk_count() const;
  const std::vector<u8>& get_chunk(int idx) const;
  std::string get_chunk_art_name(int idx) const;

  std::string get_full_name(const std::string& short_name) const;
  std::string get_chunk_texture_name(int idx) const;

 private:
  void init_jak1(const fs::path& file_path);
  void init_jak2(const fs::path& file_path);

  GameVersion m_version;
  std::string get_art_group_file_info_string() const {
    switch (m_version) {
      case GameVersion::Jak1:
        return "/src/next/data/art-group6/";
      case GameVersion::Jak2:
        return "/src/jak2/final/art-group7/";
      case GameVersion::Jak3:
        return "/src/jak3/final/art-group8/";
      default:
        ASSERT_MSG(false, "NYI get_file_info_string version");
        break;
    }
  }

  std::string get_texture_page_file_info_string() const {
    switch (m_version) {
      case GameVersion::Jak2:
        return "/src/jak2/final/texture-page8/";
      case GameVersion::Jak3:
        return "/src/jak3/final/texture-page8/";
      default:
        ASSERT_MSG(false, "NYI get_file_info_string version");
        break;
    }
  }

  std::vector<std::vector<u8>> m_chunks;
};
}  // namespace decompiler
