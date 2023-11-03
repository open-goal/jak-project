#pragma once

#include "common/custom_data/Tfrag3Data.h"

#include "decompiler/ObjectFile/ObjectFileDB.h"
#include "decompiler/data/TextureDB.h"
#include "decompiler/level_extractor/common_formats.h"

namespace decompiler {
void extract_joint_group(const ObjectFileData& ag_data,
                         const DecompilerTypeSystem& dts,
                         GameVersion version,
                         std::map<std::string, level_tools::ArtData>& out);
}