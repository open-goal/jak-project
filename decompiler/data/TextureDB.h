#pragma once

#include <set>
#include <string>
#include <unordered_map>
#include <vector>

#include "common/common_types.h"
#include "common/util/FileUtil.h"

namespace decompiler {
struct TextureDB {
  struct TextureData {
    u16 w, h;
    std::string name;
    u32 page;
    std::vector<u32> rgba_bytes;
    u32 num_mips = -1;
  };

  std::unordered_map<u32, TextureData> textures;
  std::unordered_map<u32, std::string> tpage_names;
  std::unordered_map<std::string, std::set<u32>> texture_ids_per_level;

  void add_texture(u32 tpage,
                   u32 texid,
                   const std::vector<u32>& data,
                   u16 w,
                   u16 h,
                   const std::string& tex_name,
                   const std::string& tpage_name,
                   const std::vector<std::string>& level_names,
                   u32 num_mips);

  void replace_textures(const fs::path& path);
};
}  // namespace decompiler
