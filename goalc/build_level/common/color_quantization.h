#pragma once

#include <vector>

#include "common/common_types.h"
#include "common/math/Vector.h"

// TODO: come up with something for time of day colors.
// time of day colors make the colors effectively 24-channel instead of just 3.
// the octree approach doesn't work too well here (we'd need a 16777216-tree)
// but a k-d tree seems like the right approach.

struct QuantizedColors {
  std::vector<math::Vector<u8, 4>> final_colors;
  std::vector<u32> vtx_to_color;
};

QuantizedColors quantize_colors_dumb(const std::vector<math::Vector<u8, 4>>& in);

QuantizedColors quantize_colors_octree(const std::vector<math::Vector<u8, 4>>& in,
                                       u32 target_count);

QuantizedColors quantize_colors_kd_tree(const std::vector<math::Vector<u8, 4>>& in,
                                        u32 target_depth);