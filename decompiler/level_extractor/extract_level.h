#pragma once

#include <vector>

#include "common_formats.h"

#include "common/math/Vector.h"

#include "decompiler/ObjectFile/ObjectFileDB.h"

namespace decompiler {

// info about what models have been replaced/added per level
struct MercSwapInfo {
  std::map<std::string, std::vector<std::string>> per_level_merc_swaps;
  std::map<std::string, std::vector<std::string>> per_level_custom_mdls;

  bool already_swapped(const std::string& model, const std::string& level) {
    auto mdls_it = per_level_merc_swaps.find(level);
    if (mdls_it != per_level_merc_swaps.end()) {
      auto& mdls = mdls_it->second;
      auto mdl = std::find(mdls.begin(), mdls.end(), model);
      return mdl != mdls.end();
    }
    return false;
  }

  bool already_added(const std::string& model, const std::string& level) {
    auto mdls_it = per_level_custom_mdls.find(level);
    if (mdls_it != per_level_custom_mdls.end()) {
      auto& mdls = mdls_it->second;
      auto mdl = std::find(mdls.begin(), mdls.end(), model);
      return mdl != mdls.end();
    }
    return false;
  }

  void add_to_swapped_list(const std::string& model, const std::string& level) {
    per_level_merc_swaps[level].push_back(model);
  }

  void add_to_custom_list(const std::string& model, const std::string& level) {
    per_level_custom_mdls[level].push_back(model);
  }
};

// extract everything
void extract_all_levels(const ObjectFileDB& db,
                        const TextureDB& tex_db,
                        const std::vector<std::string>& dgo_names,
                        const std::string& common_name,
                        const Config& config,
                        const fs::path& path);
void add_all_textures_from_level(tfrag3::Level& lev,
                                 const std::string& level_name,
                                 const TextureDB& tex_db);
tfrag3::Texture make_texture(u32 id,
                             const TextureDB::TextureData& tex,
                             const std::string& tpage_name,
                             bool pool_load);
std::vector<level_tools::TextureRemap> extract_tex_remap(const ObjectFileDB& db,
                                                         const std::string& dgo_name);
std::optional<ObjectFileRecord> get_bsp_file(const std::vector<ObjectFileRecord>& records,
                                             const std::string& dgo_name);
bool is_valid_bsp(const LinkedObjectFile& file);
}  // namespace decompiler
