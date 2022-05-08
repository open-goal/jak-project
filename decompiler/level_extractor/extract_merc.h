#pragma once

#include "decompiler/data/TextureDB.h"
#include "common/custom_data/Tfrag3Data.h"
#include "decompiler/ObjectFile/ObjectFileDB.h"

namespace decompiler {

void extract_merc(const ObjectFileData& ag_data,
                  const TextureDB& tex_db,
                  const DecompilerTypeSystem& dts,
                  tfrag3::Level& out,
                  bool dump_level);
}
