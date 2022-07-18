#include "collide_bvh.h"

#include <algorithm>

#include "common/log/log.h"
#include "common/util/Assert.h"
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
    return fmt::format("{} {} ({})", (mins / 4096.f).to_string_aligned(),
                       (maxs / 4096.f).to_string_aligned(),
                       ((maxs - mins) / 4096.f).to_string_aligned());
  }
};

void collect_vertices(const CNode& node, std::vector<math::Vector3f>& verts) {
  for (auto& child : node.child_nodes) {
    collect_vertices(child, verts);
  }
  for (auto& face : node.faces) {
    verts.push_back(face.v[0]);
    verts.push_back(face.v[1]);
    verts.push_back(face.v[2]);
  }
}

size_t find_most_distant(math::Vector3f pt, const std::vector<math::Vector3f>& verts) {
  float max_dist_squared = 0;
  size_t idx_of_best = 0;
  for (size_t i = 0; i < verts.size(); i++) {
    float dist = (pt - verts[i]).squared_length();
    if (dist > max_dist_squared) {
      max_dist_squared = dist;
      idx_of_best = i;
    }
  }
  return idx_of_best;
}

void compute_my_bsphere_ritters(CNode& node) {
  std::vector<math::Vector3f> verts;
  collect_vertices(node, verts);
  ASSERT(verts.size() > 0);
  auto px = verts[0];
  auto py = verts[find_most_distant(px, verts)];
  auto pz = verts[find_most_distant(py, verts)];

  auto origin = (pz + py) / 2.f;
  node.bsphere.x() = origin.x();
  node.bsphere.y() = origin.y();
  node.bsphere.z() = origin.z();

  float max_squared = 0;
  for (auto& pt : verts) {
    max_squared = std::max(max_squared, (pt - origin).squared_length());
  }
  node.bsphere.w() = std::sqrt(max_squared);
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
  });
  size_t split_idx = faces.size() / 2;
  out0->insert(out0->end(), faces.begin(), faces.begin() + split_idx);
  out1->insert(out1->end(), faces.begin() + split_idx, faces.end());
}

/*!
 * Split a node into two nodes. The outputs should be uninitialized nodes
 */
void split_node_once(CNode& node, CNode* out0, CNode* out1) {
  compute_my_bsphere_ritters(node);
  CNode temps[6];
  // split_along_dim(node.faces, pick_dim_for_split(node.faces), &out0->faces, &out1->faces);
  split_along_dim(node.faces, 0, &temps[0].faces, &temps[1].faces);
  split_along_dim(node.faces, 1, &temps[2].faces, &temps[3].faces);
  split_along_dim(node.faces, 2, &temps[4].faces, &temps[5].faces);
  node.faces.clear();
  for (auto& t : temps) {
    compute_my_bsphere_ritters(t);
  }

  float max_bspheres[3] = {0, 0, 0};

  for (int i = 0; i < 3; i++) {
    max_bspheres[i] = std::max(temps[i * 2].bsphere.w(), temps[i * 2 + 1].bsphere.w());
  }

  int best_dim = 0;
  float best_w = max_bspheres[0];
  for (int i = 0; i < 3; i++) {
    if (max_bspheres[i] < best_w) {
      best_dim = i;
      best_w = max_bspheres[i];
    }
  }

  *out0 = temps[best_dim * 2];
  *out1 = temps[best_dim * 2 + 1];
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

struct SplitResult {
  size_t max_leaf_count = 0;
  float max_bsphere_w = 0;
};
/*!
 * Split all leaf nodes. Returns the number of faces in the leaf with the most faces after
 * splitting.
 * This slightly unusual recursion pattern is to make sure we split everything to same depth,
 * which we believe might be a requirement of the collision system.
 */
SplitResult split_all_leaves(CNode& node) {
  SplitResult result;
  if (node.child_nodes.empty()) {
    // we're a leaf!
    // split us:
    split_node_to_8_children(node);
    for (auto& child : node.child_nodes) {
      result.max_leaf_count = std::max(result.max_leaf_count, child.faces.size());
      result.max_bsphere_w = std::max(result.max_bsphere_w, child.bsphere.w());
    }
    return result;
  } else {
    // not a leaf, recurse
    for (auto& child : node.child_nodes) {
      auto cret = split_all_leaves(child);
      result.max_bsphere_w = std::max(result.max_bsphere_w, cret.max_bsphere_w);
      result.max_leaf_count = std::max(result.max_leaf_count, cret.max_leaf_count);
    }
    return result;
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
    SplitResult worst = split_all_leaves(root);
    num_leaves *= 8;
    lg::info("after splitting, the worst leaf has {} tris, {} radius", worst.max_leaf_count,
             worst.max_bsphere_w / 4096.f);
    if (worst.max_leaf_count < MAX_FACES_IN_FRAG && worst.max_bsphere_w < (125.f * 4096.f)) {
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
  compute_my_bsphere_ritters(node);
  for (auto& child : node.child_nodes) {
    bsphere_recursive(child);
  }
}

void drawable_layout_helper(CNode& node, int depth, CollideTree& tree_out, size_t my_idx_check) {
  if (node.child_nodes.empty()) {
    // we're a leaf! add us to the frags
    auto& frag = tree_out.frags.frags.emplace_back();
    frag.bsphere = node.bsphere;
    frag.faces = node.faces;
  } else {
    // not a leaf
    if ((int)tree_out.node_arrays.size() <= depth) {
      tree_out.node_arrays.resize(depth + 1);
    }
    ASSERT(my_idx_check == tree_out.node_arrays.at(depth).nodes.size());
    auto& draw_node = tree_out.node_arrays[depth].nodes.emplace_back();
    draw_node.bsphere = node.bsphere;
    for (int i = 0; i < 8; i++) {
      draw_node.children[i] = my_idx_check * 8 + i;
      drawable_layout_helper(node.child_nodes.at(i), depth + 1, tree_out, draw_node.children[i]);
    }
  }
}

CollideTree build_collide_tree(CNode& root) {
  CollideTree tree;
  drawable_layout_helper(root, 0, tree, 0);
  return tree;
}

void debug_stats(const CollideTree& tree) {
  lg::info("Tree build: {} draw node layers", tree.node_arrays.size());
  float sum_w = 0, max_w = 0;
  for (auto& frag : tree.frags.frags) {
    sum_w += frag.bsphere.w();
    max_w = std::max(frag.bsphere.w(), max_w);
  }
  lg::info("Max bsphere radius: {:.2f}m, average {:.2f} (aiming for around 20-30m avg)",
           max_w / 4096, sum_w / (4096 * tree.frags.frags.size()));
}

}  // namespace

CollideTree construct_collide_bvh(const std::vector<CollideFace>& tris) {
  // part 1: build the tree
  Timer bvh_timer;
  lg::info("Building collide bvh from {} triangles", tris.size());
  CNode root;
  root.faces = tris;
  split_as_needed(root);
  lg::info("BVH tree constructed in {:.2f} ms", bvh_timer.getMs());

  // part 2: compute bspheres
  bvh_timer.start();
  bsphere_recursive(root);
  lg::info("Found bspheres in {:.2f} ms", bvh_timer.getMs());

  // part 3: layout tree
  bvh_timer.start();
  auto tree = build_collide_tree(root);
  debug_stats(tree);
  lg::info("Tree layout done in {:.2f} ms", bvh_timer.getMs());
  return tree;
}

}  // namespace collide