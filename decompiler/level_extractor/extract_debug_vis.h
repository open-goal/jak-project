#pragma once

#include "common/custom_data/Tfrag3Data.h"

#include "decompiler/level_extractor/BspHeader.h"

namespace decompiler {

void extract_debug_vis(const level_tools::BspHeader& bsp, tfrag3::Level* out);
}