#include "extract_common.h"

#include <cstddef>

namespace decompiler {
u32 clean_up_vertex_indices(std::vector<u32>& idx) {
  std::vector<u32> fixed;
  u32 num_tris = 0;

  bool looking_for_start = true;
  size_t i_of_start;
  for (size_t i = 0; i < idx.size(); i++) {
    if (looking_for_start) {
      if (idx[i] != UINT32_MAX) {
        looking_for_start = false;
        i_of_start = i;
      }
    } else {
      if (idx[i] == UINT32_MAX) {
        looking_for_start = true;
        size_t num_verts = i - i_of_start;
        if (num_verts >= 3) {
          if (!fixed.empty()) {
            fixed.push_back(UINT32_MAX);
          }
          fixed.insert(fixed.end(), idx.begin() + i_of_start, idx.begin() + i);
          num_tris += (num_verts - 2);
        }
      }
    }
  }

  if (!looking_for_start) {
    size_t num_verts = idx.size() - i_of_start;
    if (num_verts >= 3) {
      if (!fixed.empty()) {
        fixed.push_back(UINT32_MAX);
      }
      fixed.insert(fixed.end(), idx.begin() + i_of_start, idx.begin() + idx.size());
      num_tris += (num_verts - 2);
    }
  }

  idx = std::move(fixed);

  return num_tris;
}

// we want to absolutely minimize the number of time we have to "cross lanes" in AVX (meaning X
// component of one vector interacts with Y component of another).  We can make this a lot better by
// taking groups of 4 time of day colors (each containing 8x RGBAs) and rearranging them with this
// pattern.  We want to compute:
// [rgba][0][0] * weights[0] + [rgba][0][1] * weights[1] + [rgba][0][2]... + rgba[0][7] * weights[7]
// RGBA is already a vector of 4 components, but with AVX we have vectors with 32 bytes which fit
// 16 colors in them.

// This makes each vector have:
// colors0 = [rgba][0][0], [rgba][1][0], [rgba][2][0], [rgba][3][0]
// colors1 = [rgba][0][1], [rgba][1][1], [rgba][2][1], [rgba][3][1]
// ...
// so we can basically add up the columns (multiplying by weights in between)
// and we'll end up with [final0, final1, final2, final3, final4]
tfrag3::PackedTimeOfDay pack_colors(const level_tools::TimeOfDayPalette& in) {
  tfrag3::PackedTimeOfDay out;
  out.color_count = (in.height + 3) & (~3);
  out.data.resize(out.color_count * 8 * 4);

  // we're rearranging per 4 colors (groups of 32 * 4 = 128)
  // color (lots of these)
  // component (8 of these)
  // channel (4 of these, rgba)

  for (u32 color = 0; color < in.height; color++) {
    for (u32 palette = 0; palette < 8; palette++) {
      for (u32 channel = 0; channel < 4; channel++) {
        out.read(color, palette, channel) =
            (in.colors[color * 8 + palette] >> (8 * channel)) & 0xff;
      }
    }
  }

  return out;
}

}  // namespace decompiler