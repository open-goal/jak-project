#pragma once

#include "decompiler/data/TextureDB.h"

namespace decompiler {
struct ObjectFileData;

struct TPageResultStats {
  int total_textures = 0;
  int successful_textures = 0;
  int num_px = 0;
};

TPageResultStats process_tpage(ObjectFileData& data, TextureDB& texture_db);
}  // namespace decompiler