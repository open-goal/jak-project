#pragma once

#include "common/custom_data/Tfrag3Data.h"
#include "decompiler/ObjectFile/ObjectFileDB.h"

namespace decompiler {

void extract_shadow(const ObjectFileData& ag_data,
                    const DecompilerTypeSystem& dts,
                    tfrag3::Level& out,
                    bool dump_level,
                    GameVersion version);
}  // namespace decompiler