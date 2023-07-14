#pragma once

#include <map>
#include <set>
#include <string>
#include <unordered_map>
#include <vector>

#include "common/common_types.h"
#include "common/custom_data/Tfrag3Data.h"
#include "common/util/FileUtil.h"

namespace decompiler {
struct TextureDB {
  struct TextureData {
    u16 w, h;
    std::string name;
    u32 page;
    u32 dest = -1;
    std::vector<u32> rgba_bytes;
    u32 num_mips = -1;
  };

  std::map<u32, TextureData> textures;
  std::unordered_map<u32, std::string> tpage_names;
  std::unordered_map<std::string, std::set<u32>> texture_ids_per_level;

  // special textures for animation.
  std::map<u32, tfrag3::IndexTexture> index_textures_by_combo_id;

  std::unordered_map<std::string, u32> animated_tex_output_to_anim_slot;

  void add_texture(u32 tpage,
                   u32 texid,
                   const std::vector<u32>& data,
                   u16 w,
                   u16 h,
                   const std::string& tex_name,
                   const std::string& tpage_name,
                   const std::vector<std::string>& level_names,
                   u32 num_mips,
                   u32 dest);

  void add_index_texture(u32 tpage,
                         u32 texid,
                         const std::vector<u8>& index_data,
                         const std::array<math::Vector4<u8>, 256>& clut,
                         u16 w,
                         u16 h,
                         const std::string& tex_name,
                         const std::string& tpage_name,
                         const std::vector<std::string>& level_names);

  void replace_textures(const fs::path& path);

  std::string generate_texture_dest_adjustment_table() const;
};
}  // namespace decompiler
