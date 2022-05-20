#pragma once
#include "common/common_types.h"

namespace level_tools {

// levels may remap textures if they provide one that should be shared
struct TextureRemap {
  u32 original_texid;
  u32 new_texid;
};
}  // namespace level_tools