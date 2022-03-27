#pragma once

#include <vector>

#include "common/math/Vector.h"
#include "decompiler/ObjectFile/ObjectFileDB.h"

namespace decompiler {
void extract_from_level(ObjectFileDB& db,
                        TextureDB& tex_db,
                        const std::string& dgo_name,
                        const DecompileHacks& hacks,
                        bool dump_level);
void extract_common(ObjectFileDB& db, TextureDB& tex_db, const std::string& dgo_name);
}  // namespace decompiler
