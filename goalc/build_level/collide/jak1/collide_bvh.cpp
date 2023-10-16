#include "collide_bvh.h"

#include <algorithm>
#include <map>
#include <unordered_set>

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
constexpr int MAX_UNIQUE_VERTS_IN_FRAG = 128;

/*!
 * The Collide node.
 * If it's a leaf, it has faces
 * Otherwise it has 2, 4, or 8 children nodes.
 */
struct CNode {
  std::vector<CNode> child_nodes;
  std::vector<jak1::CollideFace> faces;
  math::Vector4f bsphere;
};

struct VectorHash {
  size_t operator()(const math::Vector3f& in) const {
    return std::hash<float>()(in.x()) ^ std::hash<float>()(in.y()) ^ std::hash<float>()(in.z());
  }
};

/*!
 * Recursively get a list of vertices.
 */
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

/*!
 * Find the vertex in verts that is most distant from pt.
 */
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

/*!
 * Compute a bounding sphere for a node and its children.
 */
void compute_my_bsphere_ritters(CNode& node, const std::vector<math::Vector3f>& verts) {
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
 * Compute a bounding sphere for a node and its children.
 */
void compute_my_bsphere_ritters(CNode& node) {
  std::vector<math::Vector3f> verts;
  collect_vertices(node, verts);
  compute_my_bsphere_ritters(node, verts);
}

/*!
 * Split faces in two along a coordinate plane.
 * Will clear the input faces
 */
void split_along_dim(std::vector<jak1::CollideFace>& faces,
                     int dim,
                     std::vector<jak1::CollideFace>* out0,
                     std::vector<jak1::CollideFace>* out1) {
  std::sort(faces.begin(), faces.end(),
            [=](const jak1::CollideFace& a, const jak1::CollideFace& b) {
              return a.bsphere[dim] < b.bsphere[dim];
            });
  lg::print("splitting with size: {}\n", faces.size());
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

bool needs_split(const CNode& node) {
  // quick reject.
  if (node.faces.size() > 100) {
    return true;
  }

  if (node.bsphere.w() > (125.f * 4096.f)) {
    return true;
  }

  ASSERT(node.child_nodes.empty());
  std::unordered_set<math::Vector3f, VectorHash> unique_verts;
  for (auto& f : node.faces) {
    for (auto& v : f.v) {
      unique_verts.insert(v);
    }
  }

  return unique_verts.size() >= MAX_UNIQUE_VERTS_IN_FRAG;
}

void split_recursive(CNode& to_split) {
  ASSERT(to_split.child_nodes.empty());
  ASSERT(!to_split.faces.empty());

  CNode level0[2];
  split_node_once(to_split, &level0[0], &level0[1]);
  for (int i = 0; i < 2; i++) {
    if (needs_split(level0[i])) {
      CNode level1[2];
      split_node_once(level0[i], &level1[0], &level1[1]);
      for (int j = 0; j < 2; j++) {
        if (needs_split(level1[j])) {
          CNode level2[2];
          split_node_once(level1[j], &level2[0], &level2[1]);
          for (int k = 0; k < 2; k++) {
            if (needs_split(level2[k])) {
              to_split.child_nodes.push_back(std::move(level2[k]));
              split_recursive(to_split.child_nodes.back());
            } else {
              to_split.child_nodes.push_back(std::move(level2[k]));
            }
          }
        } else {
          to_split.child_nodes.push_back(std::move(level1[j]));
        }
      }
    } else {
      to_split.child_nodes.push_back(std::move(level0[i]));
    }
  }

  ASSERT(to_split.child_nodes.size() <= 8);

  bool has_leaves = false;
  bool has_not_leaves = false;
  for (auto& child : to_split.child_nodes) {
    if (!child.faces.empty()) {
      has_leaves = true;
    }
    if (!child.child_nodes.empty()) {
      has_not_leaves = true;
    }
  }

  if (has_leaves && has_not_leaves) {
    std::vector<CNode> temp_children = std::move(to_split.child_nodes);
    to_split.child_nodes = {};
    for (auto& c : temp_children) {
      if (!c.faces.empty()) {
        to_split.child_nodes.emplace_back();
        to_split.child_nodes.emplace_back();
        split_node_once(c, &to_split.child_nodes[to_split.child_nodes.size() - 1],
                        &to_split.child_nodes[to_split.child_nodes.size() - 2]);
      } else {
        to_split.child_nodes.push_back(std::move(c));
      }
    }
  }
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

void drawable_layout_helper(const CNode& node_in,
                            CollideTree& tree_out,
                            DrawNode& parent_to_add_to) {
  if (node_in.faces.empty()) {
    ASSERT(!node_in.child_nodes.empty());
    auto& next = parent_to_add_to.draw_node_children.emplace_back();
    next.bsphere = node_in.bsphere;
    for (auto& c : node_in.child_nodes) {
      drawable_layout_helper(c, tree_out, next);
    }

  } else {
    ASSERT(node_in.child_nodes.empty());
    size_t frag_idx = tree_out.frags.frags.size();
    auto& frag_out = tree_out.frags.frags.emplace_back();
    frag_out.faces = node_in.faces;
    frag_out.bsphere = node_in.bsphere;
    parent_to_add_to.frag_children.push_back((int)frag_idx);
  }
}

CollideTree build_collide_tree(CNode& root) {
  CollideTree tree;
  drawable_layout_helper(root, tree, tree.fake_root_node);
  return tree;
}

void debug_stats(const CollideTree& tree) {
  float sum_w = 0, max_w = 0;
  for (auto& frag : tree.frags.frags) {
    sum_w += frag.bsphere.w();
    max_w = std::max(frag.bsphere.w(), max_w);
  }
  lg::info("Max bsphere radius: {:.2f}m, average {:.2f} (aiming for around 20-30m avg)",
           max_w / 4096, sum_w / (4096 * tree.frags.frags.size()));
}

}  // namespace

CollideTree construct_collide_bvh(const std::vector<jak1::CollideFace>& tris) {
  // part 1: build the tree
  Timer bvh_timer;
  lg::info("Building collide bvh from {} triangles", tris.size());
  CNode root;
  root.faces = tris;
  split_recursive(root);
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
  std::map<int, int> size_histogram;
  for (const auto& f : tree.frags.frags) {
    size_histogram[f.faces.size()]++;
  }

  for (auto [size, count] : size_histogram) {
    lg::print(" [{:3d}] {:3d} ({})\n", size, count, size * count);
  }

  return tree;
}

}  // namespace collide
