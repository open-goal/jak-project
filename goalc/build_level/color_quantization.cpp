#include "color_quantization.h"

#include <algorithm>
#include <unordered_map>

#include "common/log/log.h"
#include "common/util/Assert.h"

/*!
 * Just removes duplicate colors, which can work if there are only a few unique colors.
 */
QuantizedColors quantize_colors_dumb(const std::vector<math::Vector<u8, 4>>& in) {
  QuantizedColors result;
  std::unordered_map<u64, u32> color_to_slot;
  for (auto& vtx : in) {
    u64 key;
    memcpy(&key, vtx.data(), sizeof(u64));
    const auto& existing = color_to_slot.find(key);
    if (existing == color_to_slot.end()) {
      auto cidx = result.final_colors.size();
      result.vtx_to_color.push_back(cidx);
      color_to_slot[key] = cidx;
      result.final_colors.push_back(vtx);
    } else {
      result.vtx_to_color.push_back(existing->second);
    }
  }
  lg::print("quantize_colors_dumb: {} -> {}\n", in.size(), result.final_colors.size());
  ASSERT(result.final_colors.size() < 8192);
  return result;
}

namespace {

using Color = math::Vector<u8, 4>;

// An octree node.
// Represents a color in the output if rgb_sum_count > 0.
// Otherwise, just organizational.
struct Node {
  u32 r_sum = 0;
  u32 g_sum = 0;
  u32 b_sum = 0;
  u32 rgb_sum_count = 0;
  u8 depth = 0xff;  // 0 for root, 7 deepest

  // children stuff
  u32 leaves_under_me = 0;
  std::vector<Node> children;
  Node* parent = nullptr;

  u32 final_idx = UINT32_MAX;
};

u8 child_index(Color color, u8 depth) {
  u8 r_bit = (color.x() >> (7 - depth)) & 1;
  u8 g_bit = (color.y() >> (7 - depth)) & 1;
  u8 b_bit = (color.z() >> (7 - depth)) & 1;
  return (r_bit) + (g_bit * 2) + (b_bit * 4);
}

void insert(Node& root, Color color, u8 current_depth) {
  if (current_depth == 7) {
    root.r_sum += color.x();
    root.g_sum += color.y();
    root.b_sum += color.z();
    if (root.rgb_sum_count == 0) {
      for (auto* up = root.parent; up; up = up->parent) {
        up->leaves_under_me++;
      }
    }
    root.rgb_sum_count++;
  } else {
    if (root.children.empty()) {
      root.children.resize(8);
    }
    auto& next_node = root.children[child_index(color, current_depth)];
    if (next_node.depth == 0xff) {
      next_node.depth = current_depth + 1;
      next_node.parent = &root;
    }
    insert(next_node, color, current_depth + 1);
  }
}

template <typename T>
void for_each_node(Node& root, T&& func) {
  func(root);
  for (auto& child : root.children) {
    for_each_node(child, func);
  }
}

u32 count_leaves(Node& root) {
  u32 result = 0;
  for_each_node(root, [&](Node& n) {
    if (n.rgb_sum_count) {
      ASSERT(n.children.empty());
      result++;
    }
  });
  return result;
}

void collapse1(Node& root) {
  ASSERT(!root.children.empty());
  u32 total_children_removed = 0;
  [[maybe_unused]] u32 total_rgb_sum_moved_up = 0;
  bool started_as_leaf = root.rgb_sum_count;
  for (auto& child : root.children) {
    if (child.depth != 0xff) {
      ASSERT(child.children.empty());
      ASSERT(child.rgb_sum_count);
      total_children_removed++;
      root.r_sum += child.r_sum;
      root.g_sum += child.g_sum;
      root.b_sum += child.b_sum;
      root.rgb_sum_count += child.rgb_sum_count;
      total_rgb_sum_moved_up += child.rgb_sum_count;
    }
  }
  ASSERT(total_children_removed == root.leaves_under_me);

  if (!started_as_leaf && root.rgb_sum_count) {
    total_children_removed--;
  }
  root.children.clear();
  root.leaves_under_me = 0;
  if (total_children_removed) {
    for (auto* up = root.parent; up; up = up->parent) {
      up->leaves_under_me -= total_children_removed;
    }
  }

  ASSERT(count_leaves(root) == 1);
}

void find_nodes_at_level(Node& n, std::vector<Node*>& out, u8 level) {
  if (n.depth == level) {
    out.push_back(&n);
  } else if (n.depth < level) {
    for (auto& child : n.children) {
      find_nodes_at_level(child, out, level);
    }
  }
}

void collapse_at_level(Node& root, u8 level, u32 target_leaf_count) {
  std::vector<Node*> nodes_at_level;
  find_nodes_at_level(root, nodes_at_level, level);
  std::stable_sort(nodes_at_level.begin(), nodes_at_level.end(),
                   [](Node* a, Node* b) { return a->leaves_under_me < b->leaves_under_me; });

  size_t at_level_to_try = 0;
  while (root.leaves_under_me > target_leaf_count && at_level_to_try < nodes_at_level.size()) {
    collapse1(*nodes_at_level[at_level_to_try++]);
  }
}

void collapse_as_needed(Node& root, u32 target_leaf_count) {
  u32 level_to_reduce = 6;
  while (root.leaves_under_me > target_leaf_count) {
    collapse_at_level(root, level_to_reduce--, target_leaf_count);
  }
}

void assign_colors(Node& root, std::vector<Color>& palette_out) {
  u32 idx = 0;
  for_each_node(root, [&](Node& n) {
    if (n.rgb_sum_count) {
      n.final_idx = idx++;
      palette_out.emplace_back(n.r_sum / n.rgb_sum_count, n.g_sum / n.rgb_sum_count,
                               n.b_sum / n.rgb_sum_count);
    }
  });
}

u32 lookup_node_for_color(Node& root, Color c, u8 depth) {
  if (root.children.empty()) {
    return root.final_idx;
  } else {
    return lookup_node_for_color(root.children[child_index(c, depth)], c, depth + 1);
  }
}

}  // namespace

/*!
 * Quantize colors using an octree for clustering.
 */
QuantizedColors quantize_colors_octree(const std::vector<math::Vector<u8, 4>>& in,
                                       u32 target_count) {
  Node root;
  root.depth = 0;
  for (auto& color : in) {
    insert(root, color, 0);
  }

  collapse_as_needed(root, target_count);

  QuantizedColors out;
  assign_colors(root, out.final_colors);
  for (auto& color : in) {
    out.vtx_to_color.push_back(lookup_node_for_color(root, color, 0));
  }

  float total_error[3] = {0, 0, 0};
  for (size_t i = 0; i < in.size(); i++) {
    // lg::print(" {} -> {}\n", in[i].to_string_hex_byte(),
    // out.final_colors[out.vtx_to_color[i]].to_string_hex_byte());
    auto diff = in[i].cast<int>() - out.final_colors[out.vtx_to_color[i]].cast<int>();

    for (int j = 0; j < 3; j++) {
      total_error[j] += std::abs(diff[j]);
    }
  }

  lg::info("Octree quantize average error (as 8-bit ints): r: {}, g: {} b: {}",
           total_error[0] / in.size(), total_error[1] / in.size(), total_error[2] / in.size());
  lg::info("Final palette size: {}", out.final_colors.size());

  return out;
}
