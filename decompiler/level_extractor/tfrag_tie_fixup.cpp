#include "tfrag_tie_fixup.h"

#include <algorithm>
#include <set>
#include <unordered_map>

#include "common/log/log.h"
#include "common/math/Vector.h"
#include "common/util/Assert.h"

// Approach:
// 1: un-strip vertices and make individual strips consistent. Group triangles by strip.
// 2: build a graph of neighboring groups
// 3: find connected components
// 4: for each connected component, order strips in bfs order
// 5: iterate through the strips. If flipping the strip will result in fewer inconsistencies with
//    lowered-ordered neighbors, then flip it.
// 6: build final index buffer.

// vertex indices for a single triangle
struct Tri {
  u32 idx[3];
};

// a list of triangles, assumed to have consistent orientation.
struct TriGroup {
  std::vector<Tri> tris;
};

/*!
 * Step 1: convert strips to groups of un-stripped triangles, determine the old->new mapping.
 * Note: for the old->new mapping, we don't have to give the right answer for triangles inside of a
 * strip, or for degenerate strips. This mapping is just convenient for users of this data.
 */
void unstrip(const std::vector<u32>& stripped_indices,
             std::vector<TriGroup>& groups,
             std::vector<u32>& old_to_new_start) {
  // first triangle is the first triangle
  old_to_new_start.push_back(0);
  // doesn't matter, in the middle of a strip
  old_to_new_start.push_back(0);

  // the tfrag output flips every other triangle, we'll need to unflip that.
  bool toggle = false;

  // total number of indices created in the output.
  size_t num_unstripped_idx = 0;

  // the current strip
  TriGroup building_group;

  // loop over all groups of 3 indices...
  for (size_t i = 2; i < stripped_indices.size(); i++) {
    u32 a = stripped_indices[i];
    u32 b = stripped_indices[i - 1];
    u32 c = stripped_indices[i - 2];
    old_to_new_start.push_back(num_unstripped_idx);
    if (a == UINT32_MAX || b == UINT32_MAX || c == UINT32_MAX) {
      toggle = false;
      if (!building_group.tris.empty()) {
        groups.push_back(building_group);
        building_group.tris.clear();
      }
      continue;
    } else {
      num_unstripped_idx += 3;
      auto& tri = building_group.tris.emplace_back();
      tri.idx[0] = a;
      tri.idx[1] = toggle ? b : c;
      tri.idx[2] = toggle ? c : b;
      toggle = !toggle;
    }
  }
  old_to_new_start.push_back(num_unstripped_idx);
}

// A Node in the graph of groups.
// The self and and neighbors fields refers to group indices.
struct Node {
  int self_idx = -1;
  std::set<int> neighbors;
};

u64 group_pair_to_neighbor_key(u64 a, u64 b) {
  if (a < b) {
    std::swap(a, b);
  }
  ASSERT(a < UINT32_MAX && b < UINT32_MAX);
  return a | (b << 32);
}

// For each pair of groups that are neighbors:
struct GroupPairInfo {
  // which groups are neighbors
  int idx[2] = {-1, -1};

  // per-triangle-sharing-an-edge
  std::vector<bool> matched_edge_twists;  // true if twisted
};

/*!
 * Arbitrary ordering of vector3's.
 * We can use this to make sure that Edge(a, b) and Edge(b, a) are considered the same edge
 * by always storing an edge as [min(a,b), max(a,b)].
 */
bool greater_than(const math::Vector3f& a, const math::Vector3f& b) {
  for (int dim = 0; dim < 3; dim++) {
    if (a[dim] > b[dim]) {
      return true;
    } else if (a[dim] < b[dim]) {
      return false;
    }
  }
  // ASSERT(false);
  return false;
}

math::Vector3f round_vector(const math::Vector3f& in) {
  math::Vector3f rounded;
  for (int i = 0; i < 3; i++) {
    s64 x = in[i];
    rounded[i] = x;
  }
  return rounded;
}
/*!
 * Edge that can be hashed, and Edge(a, b) == Edge(b, a), using the trick described above.
 */
class HashableEdge {
 public:
  HashableEdge(const math::Vector3f& a, const math::Vector3f& b) {
    if (greater_than(a, b)) {
      m_greater_pt = a;
      m_lesser_pt = b;
    } else {
      m_greater_pt = b;
      m_lesser_pt = a;
    }
  }

  bool operator==(const HashableEdge& other) const {
    return m_lesser_pt == other.m_lesser_pt && m_greater_pt == other.m_greater_pt;
  }

  struct hash {
    std::size_t operator()(const HashableEdge& in) const {
      std::size_t result = 0;
      for (int i = 0; i < 3; i++) {
        result ^= std::hash<float>()(in.m_lesser_pt[i]) ^ std::hash<float>()(in.m_greater_pt[i]);
      }
      return result;
    }
  };

 private:
  math::Vector3f m_lesser_pt, m_greater_pt;
};

math::Vector3f triangle_normal(const Tri& tri, const std::vector<math::Vector3f>& positions) {
  const auto& a = positions.at(tri.idx[0]);
  const auto& b = positions.at(tri.idx[1]);
  const auto& c = positions.at(tri.idx[2]);
  return (b - a).cross(c - a);
}

float triangle_normal_dot(const Tri& a,
                          const Tri& b,
                          const std::vector<math::Vector3f>& positions) {
  return triangle_normal(a, positions).dot(triangle_normal(b, positions));
}

struct PerEdgeInfo {
  int source_group = -1;
  int tri_idx = -1;  // in group
  int edge_idx = -1;
  bool change_order = false;
};

/*!
 * Step 2: build a graph of connected groups. Groups are considered connected if they share an edge.
 * Also collect information about pairs of connected groups.
 * @param nodes: the node for each group.
 * @param neighbor_info: info for each pair of connected group, keyed on group_pair_to_neighbor_key
 * @param groups: input groups
 * @param positions: vertex position input
 */
void build_graph(std::vector<Node>& nodes,
                 std::unordered_map<u64, GroupPairInfo>& neighbor_info,
                 const std::vector<TriGroup>& groups,
                 const std::vector<math::Vector3f>& positions) {
  nodes.reserve(groups.size());

  // to avoid slow O(n^2) edge checks, we'll add all edges to a hash table, recording which
  // groups they appear in.
  std::unordered_map<HashableEdge, std::vector<PerEdgeInfo>, HashableEdge::hash> edge_info_map;

  // first pass: set up nodes and build hash table
  for (int group_idx = 0; group_idx < (int)groups.size(); group_idx++) {
    const auto& group = groups[group_idx];
    // add the node
    auto& node = nodes.emplace_back();
    node.self_idx = group_idx;

    for (int tri_idx = 0; tri_idx < (int)group.tris.size(); tri_idx++) {
      const auto& tri = group.tris[tri_idx];
      for (int edge_idx = 0; edge_idx < 3; edge_idx++) {
        u32 edge_indices[2] = {
            tri.idx[(edge_idx + 0) % 3],
            tri.idx[(edge_idx + 1) % 3],
        };
        HashableEdge edge(round_vector(positions.at(edge_indices[0])),
                          round_vector(positions.at(edge_indices[1])));
        auto& info_list = edge_info_map[edge];
        bool found = false;
        for (auto& x : info_list) {
          if (x.source_group == group_idx) {
            found = true;
            break;
          }
        }
        if (found) {
          continue;
        }
        auto& info = edge_info_map[edge].emplace_back();
        info.source_group = group_idx;
        info.tri_idx = tri_idx;
        info.edge_idx = edge_idx;
        info.change_order =
            greater_than(positions.at(edge_indices[0]), positions.at(edge_indices[1]));
      }
    }
  }

  // second pass: loop over shared edges
  [[maybe_unused]] int shared_edge_count = 0;
  for (const auto& [edge, infos] : edge_info_map) {
    // skip any edge that only shows up once.
    if (infos.size() < 2) {
      continue;
    }
    shared_edge_count++;

    for (int i = 0; i < (int)infos.size(); i++) {
      for (int j = 0; j < i; j++) {
        const auto& info_a = infos.at(i);
        const auto& info_b = infos.at(j);
        int group_a = info_a.source_group;
        int group_b = info_b.source_group;
        if (info_a.source_group == info_b.source_group) {
          lg::print("duplicate edge in group!\n");  // ??
          continue;
        }

        // link neighbors
        nodes.at(group_a).neighbors.insert(group_b);
        nodes.at(group_b).neighbors.insert(group_a);

        // make neighbor info (or append to existing)
        auto neighbor_key = group_pair_to_neighbor_key(group_a, group_b);
        auto& info = neighbor_info[neighbor_key];
        int a_idx = group_a > group_b ? 0 : 1;
        int b_idx = 1 - a_idx;
        info.idx[a_idx] = group_a;
        info.idx[b_idx] = group_b;
        info.matched_edge_twists.push_back(
            info_a.change_order == info_b.change_order  // should be opposite dirs.
            //            triangle_normal_dot(groups.at(group_a).tris.at(info_a.tri_idx),
            //                                groups.at(group_b).tris.at(info_b.tri_idx), positions)
        );
      }
    }
  }
}

struct ConnectedComponent {
  std::vector<int> groups;
};

/*!
 * Step 3: find connected components
 */
std::vector<ConnectedComponent> find_connected_components(const std::vector<Node>& nodes) {
  std::vector<ConnectedComponent> result;
  std::set<int> added;

  // loop over each node
  for (int node_idx = 0; node_idx < (int)nodes.size(); node_idx++) {
    // skip nodes already part of a connected component.
    if (added.find(node_idx) != added.end()) {
      continue;
    }

    // new node, create a new component and find everything connected.
    auto& component = result.emplace_back();
    // added this first node
    component.groups.push_back(node_idx);
    added.insert(node_idx);

    // initialize the search with neighbors of the first node
    std::vector<int> to_visit;
    for (auto x : nodes.at(node_idx).neighbors) {
      to_visit.push_back(x);
    }

    // find all connected!
    while (!to_visit.empty()) {
      int next_idx = to_visit.back();
      to_visit.pop_back();
      if (added.find(next_idx) != added.end()) {
        // already seen it, skip!
        continue;
      }
      // new node, add it
      added.insert(next_idx);
      component.groups.push_back(next_idx);
      // also look at neighbors
      for (auto x : nodes.at(next_idx).neighbors) {
        if (added.find(x) == added.end()) {
          to_visit.push_back(x);
        }
      }
    }
  }
  return result;
}

/*!
 * Step 4: order components in bfs order
 */
ConnectedComponent bfs_order_connected_component(const ConnectedComponent& in,
                                                 const std::vector<Node>& graph) {
  ConnectedComponent out;

  // add the first one
  std::set<int> added;
  std::vector<int> frontier = {in.groups.at(0)};

  // go!
  while (!frontier.empty()) {
    std::stable_sort(frontier.begin(), frontier.end(), [&graph](int a, int b) {
      return graph.at(a).neighbors.size() < graph.at(b).neighbors.size();
    });
    std::vector<int> next_frontier;

    for (auto x : frontier) {
      if (added.find(x) != added.end()) {
        continue;
      }
      added.insert(x);
      out.groups.push_back(x);

      auto& node = graph.at(x);
      for (auto& neighbor : node.neighbors) {
        if (added.find(neighbor) == added.end()) {
          next_frontier.push_back(neighbor);
        }
      }
    }

    frontier = std::move(next_frontier);
  }

  return out;
}

std::vector<bool> compute_flips(const std::vector<Node>& graph,
                                const std::vector<ConnectedComponent>& components,
                                const std::unordered_map<u64, GroupPairInfo>& neighbor_info) {
  std::vector<bool> flips;
  flips.resize(graph.size(), false);

  for (const auto& component : components) {
    std::set<int> decided;
    for (int node_idx : component.groups) {
      const auto& group = graph.at(node_idx);
      float flip_sum = 0;
      for (int neighbor_idx : group.neighbors) {
        if (decided.find(neighbor_idx) == decided.end()) {
          continue;  // skip, don't know this ones orientation
        }
        const auto& info = neighbor_info.at(group_pair_to_neighbor_key(node_idx, neighbor_idx));
        for (auto originally_twisted : info.matched_edge_twists) {
          bool twisted = originally_twisted;
          if (flips[neighbor_idx]) {
            twisted = !twisted;
          }
          if (twisted) {
            flip_sum--;
          } else {
            flip_sum++;
          }
        }
      }

      if (flip_sum < 0) {
        flips[node_idx] = true;
      }

      decided.insert(node_idx);
    }
  }

  return flips;
}

void apply_flips(const std::vector<bool>& flips, std::vector<TriGroup>& groups) {
  ASSERT(flips.size() == groups.size());
  for (size_t i = 0; i < groups.size(); i++) {
    if (flips[i]) {
      for (auto& tri : groups[i].tris) {
        std::swap(tri.idx[0], tri.idx[1]);
      }
    }
  }
}

void make_final_indices(const std::vector<TriGroup>& groups, std::vector<u32>& idx) {
  for (auto& g : groups) {
    for (auto& t : g.tris) {
      for (auto i : t.idx) {
        idx.push_back(i);
      }
    }
  }
}

void fixup_and_unstrip_tfrag_tie(const std::vector<u32>& stripped_indices,
                                 const std::vector<math::Vector3f>& positions,
                                 std::vector<u32>& unstripped,
                                 std::vector<u32>& old_to_new_start) {
  // Part 1
  std::vector<TriGroup> groups;
  unstrip(stripped_indices, groups, old_to_new_start);

  // Part 2
  std::vector<Node> nodes;
  std::unordered_map<u64, GroupPairInfo> neighbor_info;
  build_graph(nodes, neighbor_info, groups, positions);

  // Part 3
  auto connected_components = find_connected_components(nodes);

  // Part 4
  std::vector<ConnectedComponent> ordered_components;
  for (auto& c : connected_components) {
    ordered_components.push_back(bfs_order_connected_component(c, nodes));
  }

  // Part 5
  auto flips = compute_flips(nodes, ordered_components, neighbor_info);

  // Part 6
  apply_flips(flips, groups);

  // Part 7
  make_final_indices(groups, unstripped);
}
