#pragma once

#include "common/custom_data/Tfrag3Data.h"

#include "decompiler/level_extractor/BspHeader.h"

namespace decompiler {

void extract_hfrag(const level_tools::HFragment& hfrag,
                   const std::string& debug_name,
                   const std::vector<level_tools::TextureRemap>& map,
                   const TextureDB& tex_db,
                   tfrag3::Level* out);
}