#pragma once

#include "common/custom_data/Tfrag3Data.h"
#include "common/math/Vector.h"

#include "decompiler/data/TextureDB.h"
#include "decompiler/level_extractor/BspHeader.h"

namespace decompiler {

/// <summary>
/// Extract shrubs from the level
/// </summary>
/// <param name="tree"></param>
/// <param name="debug_name"></param>
/// <param name="map"></param>
/// <param name="tex_db"></param>
/// <param name="expected_missing_textures"></param>
/// <param name="out"></param>
/// <param name="dump_level"></param>
void extract_shrub(const level_tools::shrub_types::DrawableTreeInstanceShrub* tree,
                   const std::string& debug_name,
                   const std::vector<level_tools::TextureRemap>& map,
                   const TextureDB& tex_db,
                   const std::vector<std::pair<int, int>>& expected_missing_textures,
                   tfrag3::Level& out,
                   bool dump_level,
                   GameVersion version);

}  // namespace decompiler
