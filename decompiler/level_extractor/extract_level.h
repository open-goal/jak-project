#pragma once

#include <vector>

#include "common_formats.h"

#include "common/math/Vector.h"

#include "decompiler/ObjectFile/ObjectFileDB.h"

namespace decompiler {

// extract everything
void extract_all_levels(const ObjectFileDB& db,
                        const TextureDB& tex_db,
                        const std::vector<std::string>& dgo_names,
                        const std::string& common_name,
                        const Config& config,
                        bool debug_dump_level,
                        bool extract_collision,
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
}  // namespace decompiler
