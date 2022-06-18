#pragma once

#include "tools/build_level/collide_bvh.h"

struct CollideFragMeshData {
  math::Vector4f bsphere;  // not part of the collide frag, but is part of the drawable wrapping it
  std::vector<u8> packed_data, pat_array;
  u16 strip_data_len;
  u16 poly_count;
  math::Vector3f base_trans_xyz;
  u8 vertex_count;
  u8 vertex_data_qwc;
  u8 total_qwc;
};

struct CollideFragMeshDataArray {
  std::vector<CollideFragMeshData> packed_frag_data;
  std::vector<PatSurface> pats;
};

CollideFragMeshDataArray pack_collide_frags(const std::vector<collide::CollideFrag>& frag_data);