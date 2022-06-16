#pragma once

#include <vector>
#include "common/common_types.h"
#include "common/math/Vector.h"

struct QuantizedColors {
  std::vector<math::Vector<u8, 4>> final_colors;
  std::vector<u32> vtx_to_color;
};

QuantizedColors quantize_colors_dumb(const std::vector<math::Vector<u8, 4>>& in);

QuantizedColors quantize_colors_octree(const std::vector<math::Vector<u8, 4>>& in,
                                       u32 target_count);