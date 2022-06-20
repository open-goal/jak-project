#pragma once

#include <vector>
#include "goalc/build_level/collide_common.h"

// requirements:
// max depth of 3 (maybe?)
// max face per frag = 90
// max vert per frag = 110
// branching factor of 8 everywhere.
namespace collide {

struct DrawNode {
  s32 children[8] = {-1, -1, -1, -1, -1, -1, -1, -1};
  math::Vector4f bsphere;
};

struct CollideFrag {
  math::Vector4f bsphere;
  std::vector<CollideFace> faces;
};

struct DrawableInlineArrayNode {
  std::vector<DrawNode> nodes;
};

struct DrawableInlineArrayCollideFrag {
  std::vector<CollideFrag> frags;
};

struct CollideTree {
  std::vector<DrawableInlineArrayNode> node_arrays;
  DrawableInlineArrayCollideFrag frags;
};

CollideTree construct_collide_bvh(const std::vector<CollideFace>& tris);
}  // namespace collide
