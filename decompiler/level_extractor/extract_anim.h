#pragma once
#include "common/custom_data/Tfrag3Data.h"

#include "decompiler/ObjectFile/ObjectFileDB.h"
#include "decompiler/level_extractor/common_formats.h"
#include "goalc/build_actor/common/build_actor.h"

namespace decompiler {
void extract_animations(const ObjectFileData& ag_data,
                        const DecompilerTypeSystem& dts,
                        GameVersion version,
                        std::map<std::string, level_tools::ArtData>& out);
}  // namespace decompiler