#pragma once

#include <vector>

#include "common/math/Vector.h"

#include "decompiler/ObjectFile/ObjectFileDB.h"

namespace decompiler {

// extract everything
void extract_all_levels(const ObjectFileDB& db,
                        const TextureDB& tex_db,
                        const std::vector<std::string>& dgo_names,
                        const std::string& common_name,
                        const Config& config,
                        bool debug_dump_level,
                        bool extract_collision,
                        const fs::path& path);
}  // namespace decompiler
