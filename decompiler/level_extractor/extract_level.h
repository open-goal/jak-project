#pragma once

#include <vector>

#include "common/math/Vector.h"
#include "decompiler/ObjectFile/ObjectFileDB.h"

namespace decompiler {
void extract_from_level(const ObjectFileDB& db,
                        const TextureDB& tex_db,
                        const std::string& dgo_name,
                        const DecompileHacks& hacks,
                        bool dump_level);
void extract_common(const ObjectFileDB& db, const TextureDB& tex_db, const std::string& dgo_name);

// extract everything
void extract_all_levels(const ObjectFileDB& db,
                        const TextureDB& tex_db,
                        const std::vector<std::string>& dgo_names,
                        const std::string& common_name,
                        const DecompileHacks& hacks,
                        bool debug_dump_level);
}  // namespace decompiler
