#pragma once

#include "common/custom_data/Tfrag3Data.h"

#include "decompiler/level_extractor/BspHeader.h"

namespace decompiler {

void extract_hfrag(const level_tools::BspHeader& bsp, const TextureDB& tex_db, tfrag3::Level* out);
}