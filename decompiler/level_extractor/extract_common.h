#pragma once
#include <vector>

#include "common/common_types.h"
#include "common/custom_data/Tfrag3Data.h"

#include "decompiler/level_extractor/BspHeader.h"

namespace decompiler {
u32 clean_up_vertex_indices(std::vector<u32>& idx);
tfrag3::PackedTimeOfDay pack_colors(const level_tools::TimeOfDayPalette& in);
}  // namespace decompiler
