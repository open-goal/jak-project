#include "collide_bvh.h"
#include "common/util/Assert.h"
#include "common/log/log.h"
#include "common/util/Timer.h"

// Collision BVH algorithm
// We start with all the points in a single node, then recursively split nodes in 8 until no nodes
// have too many faces.
// The splitting is done by doing median cuts along the x, y, or z axis.

// The bspheres are built at the end.

namespace collide {

namespace {

constexpr int MAX_FACES_IN_FRAG = 100;

/*!
 * The Collide node.
 * Has either children collide node or children faces, but not both
 * The size of child_nodes is either 0 or 8 at all times.
 */
struct CNode {
  std::vector<CNode> child_nodes;
  std::vector<CollideFace> faces;
  math::Vector4f bsphere;
};

struct BBox {
  math::Vector3f mins, maxs;
  std::string sz_to_string() const {
    return fmt::format("({})", ((maxs - mins) / 4096.f).to_string_aligned());
  }
};

/*!
 * Make the bounding box hold this node and all its children.
 */
void add_to_bbox_recursive(const CNode& node, BBox& bbox) {
  if (node.faces.empty()) {
    ASSERT(node.child_nodes.size() == 8);
    for (auto& child : node.child_nodes) {
      add_to_bbox_recursive(child, bbox);
    }
  } else {
    for (auto& face : node.faces) {
      for (auto& vert : face.v) {
        bbox.mins.min_in_place(vert);
        bbox.maxs.max_in_place(vert);
      }
    }
  }
}

BBox bbox_of_node(const CNode& node) {
  BBox bbox;
  bbox.mins.fill(std::numeric_limits<float>::max());
  bbox.maxs.fill(-std::numeric_limits<float>::max());
  add_to_bbox_recursive(node, bbox);
  return bbox;
}

/*!
 * Make the bsphere hold this node and all its children.
 */
void update_bsphere_recursive(const CNode& node, const math::Vector3f& origin, float& r_squared) {
  if (node.faces.empty()) {
    ASSERT(node.child_nodes.size() == 8);
    for (auto& child : node.child_nodes) {
      update_bsphere_recursive(child, origin, r_squared);
    }
  } else {
    for (auto& face : node.faces) {
      for (auto& vert : face.v) {
        r_squared = std::max(r_squared, (vert - origin).squared_length());
      }
    }
  }
}

/*!
 * Compute the bsphere of a single node.
 */
void compute_my_bsphere(CNode& node) {
  // first compute bbox.
  BBox bbox = bbox_of_node(node);
  float r = 0;
  math::Vector3f origin = (bbox.maxs + bbox.mins) * 0.5;
  update_bsphere_recursive(node, origin, r);
  node.bsphere.x() = origin.x();
  node.bsphere.y() = origin.y();
  node.bsphere.z() = origin.z();
  node.bsphere.w() = std::sqrt(r);
}

/*!
 * Pick a dimension (x, y, or z) to split along.
 * The heuristic is just to pick the direction with the largest span.
 */
int pick_dim_for_split(const std::vector<CollideFace>& faces) {
  math::Vector3f maxs, mins;
  maxs.fill(-std::numeric_limits<float>::max());
  mins.fill(std::numeric_limits<float>::min());

  for (const auto& face : faces) {
    for (const auto& vert : face.v) {
      maxs.max_in_place(vert);
      mins.min_in_place(vert);
    }
  }

  float best_range = 0;
  int best_dim = 0;
  math::Vector3f range = maxs - mins;
  for (int i = 0; i < 3; i++) {
    if (range[i] > best_range) {
      best_range = range[i];
      best_dim = i;
    }
  }

  return best_dim;
}

/*!
 * Split faces in two along a coordinate plane.
 * Will clear the input faces
 */
void split_along_dim(std::vector<CollideFace>& faces,
                     int dim,
                     std::vector<CollideFace>* out0,
                     std::vector<CollideFace>* out1) {
  std::sort(faces.begin(), faces.end(), [=](const CollideFace& a, const CollideFace& b) {
    return a.bsphere[dim] < b.bsphere[dim];
    //return a.v[0][dim] < b.v[0][dim];
  });
  size_t split_idx = faces.size() / 2;
  out0->insert(out0->end(), faces.begin(), faces.begin() + split_idx);
  out1->insert(out1->end(), faces.begin() + split_idx, faces.end());
  faces.clear();
}

/*!
 * Split a node into two nodes. The outputs should be uninitialized nodes
 */
void split_node_once(CNode& node, CNode* out0, CNode* out1) {
  split_along_dim(node.faces, pick_dim_for_split(node.faces), &out0->faces, &out1->faces);
}

/*!
 * Split a node into 8 children and store these in the given node.
 */
void split_node_to_8_children(CNode& node) {
  ASSERT(node.child_nodes.empty());
  node.child_nodes.resize(8);
  // level 0
  CNode level0[2];
  split_node_once(node, &level0[0], &level0[1]);
  // level 1
  CNode level1[4];
  split_node_once(level0[0], &level1[0], &level1[1]);
  split_node_once(level0[1], &level1[2], &level1[3]);
  // level 2
  split_node_once(level1[0], &node.child_nodes[0], &node.child_nodes[1]);
  split_node_once(level1[1], &node.child_nodes[2], &node.child_nodes[3]);
  split_node_once(level1[2], &node.child_nodes[4], &node.child_nodes[5]);
  split_node_once(level1[3], &node.child_nodes[6], &node.child_nodes[7]);
}

/*!
 * Split all leaf nodes. Returns the number of faces in the leaf with the most faces after
 * splitting.
 * This slightly unusual recursion pattern is to make sure we split everything to same depth,
 * which we believe might be a requirement of the collision system.
 */
size_t split_all_leaves(CNode& node) {
  size_t worst_leaf_face_count = 0;
  if (node.child_nodes.empty()) {
    // we're a leaf!
    // split us:
    split_node_to_8_children(node);
    for (auto& child : node.child_nodes) {
      worst_leaf_face_count = std::max(worst_leaf_face_count, child.faces.size());
    }
    return worst_leaf_face_count;
  } else {
    // not a leaf, recurse
    for (auto& child : node.child_nodes) {
      worst_leaf_face_count = std::max(worst_leaf_face_count, split_all_leaves(child));
    }
    return worst_leaf_face_count;
  }
}

/*!
 * Main BVH construction function. Splits leaves until it is no longer needed.
 */
void split_as_needed(CNode& root) {
  int initial_tri_count = root.faces.size();
  int num_leaves = 1;
  bool need_to_split = true;
  while (need_to_split) {
    int faces_in_worst = split_all_leaves(root);
    num_leaves *= 8;
    lg::info("after splitting, the worst leaf has {} tris", faces_in_worst);
    if (faces_in_worst < MAX_FACES_IN_FRAG) {
      need_to_split = false;
    }
  }
  lg::info("average triangles per leaf: {}", initial_tri_count / num_leaves);
  lg::info("leaf count: {}", num_leaves);
}



/*!
 * Recursively compute bspheres of all children
 * (note that we don't do bspheres of bspheres... I think this is better?)
 */
void bsphere_recursive(CNode& node) {
  compute_my_bsphere(node);
  for (auto& child : node.child_nodes) {
    bsphere_recursive(child);
  }
}

}  // namespace

void construct_collide_bvh(const std::vector<CollideFace>& tris) {
  Timer bvh_timer;
  lg::info("Building collide bvh from {} triangles", tris.size());
  CNode root;
  root.faces = tris;
  split_as_needed(root);
  bsphere_recursive(root);
  lg::info("Collide bvh took {:.2f} ms\n", bvh_timer.getMs());
}

}  // namespace collide