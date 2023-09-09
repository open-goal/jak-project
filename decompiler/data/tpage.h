#pragma once
#include <string>
#include <unordered_set>

#include "decompiler/data/TextureDB.h"

namespace decompiler {
struct ObjectFileData;

struct TPageResultStats {
  int total_textures = 0;
  int successful_textures = 0;
  int num_px = 0;
};

TPageResultStats process_tpage(ObjectFileData& data,
                               TextureDB& texture_db,
                               const fs::path& output_path,
                               const std::unordered_set<std::string>& animated_textures,
                               bool save_pngs);
}  // namespace decompiler
