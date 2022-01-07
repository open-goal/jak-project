#pragma once

#include <vector>
#include <unordered_map>
#include <string>
#include "common/common_types.h"

namespace decompiler {
struct TextureDB {
  struct TextureData {
    u16 w, h;
    std::string name;
    u32 page;
    std::vector<u32> rgba_bytes;
  };

  std::unordered_map<u32, TextureData> textures;
  std::unordered_map<u32, std::string> tpage_names;

  void add_texture(u32 tpage,
                   u32 texid,
                   const std::vector<u32>& data,
                   u16 w,
                   u16 h,
                   const std::string& tex_name,
                   const std::string& tpage_name);
};
}  // namespace decompiler
