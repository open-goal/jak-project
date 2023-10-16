#pragma once

#include <vector>

#include "goalc/build_level/collide/common/collide_common.h"

// requirements:
// max depth of 3 (maybe?)
// max face per frag = 90
// max vert per frag = 110
// branching factor of 8 everywhere.
namespace collide {

struct DrawNode {
  std::vector<DrawNode> draw_node_children;
  std::vector<int> frag_children;
  math::Vector4f bsphere;
};

struct CollideFrag {
  math::Vector4f bsphere;
  std::vector<jak1::CollideFace> faces;
};

struct DrawableInlineArrayNode {
  std::vector<DrawNode> nodes;
};

struct DrawableInlineArrayCollideFrag {
  std::vector<CollideFrag> frags;
};

struct CollideTree {
  //  std::vector<DrawableInlineArrayNode> node_arrays;
  DrawNode fake_root_node;  // the children of this are the ones that go in the top level.
  DrawableInlineArrayCollideFrag frags;
};

CollideTree construct_collide_bvh(const std::vector<jak1::CollideFace>& tris);
}  // namespace collide
