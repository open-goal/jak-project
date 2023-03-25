#pragma once

#include "extract_tie.h"

#include "common/custom_data/Tfrag3Data.h"

#include "decompiler/data/TextureDB.h"
#include "decompiler/level_extractor/BspHeader.h"

namespace decompiler {

void extract_tie(const level_tools::DrawableTreeInstanceTie* tree,
                 const std::string& debug_name,
                 const std::vector<level_tools::TextureRemap>& tex_map,
                 const TextureDB& tex_db,
                 tfrag3::Level& out,
                 bool dump_level,
                 GameVersion version);
}
