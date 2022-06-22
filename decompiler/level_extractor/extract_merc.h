#pragma once

#include "common/custom_data/Tfrag3Data.h"

#include "decompiler/data/TextureDB.h"
#include "decompiler/level_extractor/common_formats.h"
#include "decompiler/ObjectFile/ObjectFileDB.h"

namespace decompiler {

void extract_merc(const ObjectFileData& ag_data,
                  const TextureDB& tex_db,
                  const DecompilerTypeSystem& dts,
                  const std::vector<level_tools::TextureRemap>& map,
                  tfrag3::Level& out,
                  bool dump_level);
}
