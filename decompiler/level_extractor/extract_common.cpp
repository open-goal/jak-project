#include "extract_common.h"

#include <cstddef>

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