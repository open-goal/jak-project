#pragma once

#include "tools/build_level/collide_common.h"
#include <vector>

// requirements:
// max depth of 3 (maybe?)
// max face per frag = 90
// max vert per frag = 110
// branching factor of 8 everywhere.
namespace collide {

struct MeshFrag {
  std::vector<CollideFace> faces;
  math::Vector3f origin;
  math::Vector4f bsphere;
};

struct MeshFragArray {
  MeshFrag data[8];
};

struct BvhNodeArray;

struct BvhNode {
  math::Vector4f bsphere;
  std::unique_ptr<MeshFragArray> frag_children;
  std::unique_ptr<BvhNodeArray> node_children;
};

struct BvhNodeArray {
  BvhNode data[8];
};

void construct_collide_bvh(const std::vector<CollideFace>& tris);
}
