#include "color_quantization.h"

#include <algorithm>
#include <array>
#include <set>
#include <unordered_map>

#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/util/Timer.h"

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
                               n.b_sum / n.rgb_sum_count, 0);
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

struct KdNode {
  // if not a leaf
  std::unique_ptr<KdNode> left, right;

  // if leaf
  std::vector<Color> colors;
};

size_t pick_split_point(const std::vector<Color>& colors, int dim) {
  if (colors.empty()) {
    return 0;
  }
  double mean = 0;
  int mean_count = 0;
  std::array<bool, 256> seen;
  seen.fill(false);

  for (const auto& color : colors) {
    const u8 val = color[dim];
    if (!seen[val]) {
      mean += val;
      mean_count++;
      seen[val] = true;
    }
  }
  mean /= mean_count;

  for (size_t i = 0; i < colors.size(); i++) {
    if (colors.at(i)[dim] >= mean) {
      return i;
    }
  }

  return colors.size() - 1;
}

int pick_split_dim_final_splits(const std::vector<Color>& colors) {
  int mins[4] = {255, 255, 255, 255};
  int maxs[4] = {0, 0, 0, 0};
  for (const auto& color : colors) {
    for (int i = 0; i < 4; i++) {
      mins[i] = std::min(mins[i], (int)color[i]);
      maxs[i] = std::max(maxs[i], (int)color[i]);
    }
  }

  int best_dim = 0;
  int best_diff = 0;
  for (int i = 0; i < 4; i++) {
    const int diff = maxs[i] - mins[i];
    if (diff > best_diff) {
      best_diff = diff;
      best_dim = i;
    }
  }

  return best_dim;
}

void split_kd(KdNode* in, u32 depth, int next_split_dim) {
  if (!depth) {
    return;
  }

  if (!in->colors.empty()) {
    for (int i = 0; i < 4; i++) {
      bool all_same = true;
      u8 same = in->colors[0][next_split_dim];
      for (auto& color : in->colors) {
        if (color[next_split_dim] != same) {
          all_same = false;
          break;
        }
      }
      if (all_same) {
        next_split_dim = (next_split_dim + 1) % 4;
      } else {
        break;
      }
    }
  }

  // sort by split dimension
  std::stable_sort(in->colors.begin(), in->colors.end(), [=](const Color& a, const Color& b) {
    return a[next_split_dim] < b[next_split_dim];
  });

  in->left = std::make_unique<KdNode>();
  in->right = std::make_unique<KdNode>();

  size_t i = 0;
  // size_t mid = in->colors.size() / 2;
  size_t mid = pick_split_point(in->colors, next_split_dim);
  if (depth & 1) {
    while (mid > 1 && in->colors[mid][next_split_dim] == in->colors[mid - 1][next_split_dim]) {
      mid--;
    }
  } else {
    while (mid + 2 < in->colors.size() &&
           in->colors[mid][next_split_dim] == in->colors[mid + 1][next_split_dim]) {
      mid++;
    }
  }

  for (; i < mid; i++) {
    in->left->colors.push_back(in->colors[i]);
  }

  for (; i < in->colors.size(); i++) {
    in->right->colors.push_back(in->colors[i]);
  }

  /*
  if (debug) {
    if (in->left->colors.empty()) {
      printf(" LEFT empty\n");
    } else {
      printf(" LEFT, has %ld, %d to %d\n", in->left->colors.size(),
             in->left->colors.front()[next_split_dim], in->left->colors.back()[next_split_dim]);
    }

    if (in->right->colors.empty()) {
      printf(" RIGHT empty\n");
    } else {
      printf(" RIGHT, has %ld, %d to %d\n", in->right->colors.size(),
             in->right->colors.front()[next_split_dim], in->right->colors.back()[next_split_dim]);
    }
  }
  */

  split_kd(in->left.get(), depth - 1, (next_split_dim + 1) % 4);
  split_kd(in->right.get(), depth - 1, (next_split_dim + 1) % 4);
}

template <typename Func>
void for_each_child(KdNode* node, Func&& f) {
  if (node->left) {
    for_each_child(node->left.get(), f);
    for_each_child(node->right.get(), f);
  } else {
    f(node);
  }
}

u32 color_as_u32(const Color& color) {
  u32 ret = 0;
  memcpy(&ret, color.data(), 4);
  return ret;
}

Color u32_as_color(u32 in) {
  Color ret;
  memcpy(ret.data(), &in, 4);
  return ret;
}

std::vector<Color> deduplicated_colors(const std::vector<Color>& in) {
  std::set<u32> unique;
  for (auto& x : in) {
    unique.insert(color_as_u32(x));
  }
  std::vector<Color> out;
  for (auto& x : unique) {
    out.push_back(u32_as_color(x));
  }
  return out;
}

u8 saturate_to_u8(u32 in) {
  if (in >= UINT8_MAX) {
    return UINT8_MAX;
  } else {
    return in;
  }
}

s32 color_difference(const Color& c1, const Color& c2) {
  s32 ret = 0;
  for (int i = 0; i < 4; i++) {
    const int diff = (int)c1[i] - (int)c2[i];
    ret += diff * diff;
  }
  return ret;
}

int total_color_count(const KdNode* node) {
  int ret = 0;
  if (node->left && node->right) {
    ret += total_color_count(node->left.get());
    ret += total_color_count(node->right.get());
  } else {
    if (!node->colors.empty()) {
      ret += 1;
    }
  }
  return ret;
}

void get_splittable(KdNode* node, std::vector<KdNode*>* out) {
  if (node->left && node->right) {
    get_splittable(node->left.get(), out);
    get_splittable(node->right.get(), out);
  } else {
    if (node->colors.size() > 1 && node->colors.front() != node->colors.back()) {
      out->push_back(node);
    }
  }
}

QuantizedColors quantize_colors_kd_tree(const std::vector<math::Vector<u8, 4>>& in,
                                        u32 target_depth) {
  Timer timer;
  // Build root node:
  KdNode root;
  root.colors = deduplicated_colors(in);
  const int num_unique_colors = root.colors.size();

  // Split tree:
  split_kd(&root, target_depth, 0);

  // keep splitting!
  int color_count = total_color_count(&root);
  printf("color count %d / %d\n", color_count, (1 << target_depth));
  while (color_count < (1 << target_depth)) {
    printf("extra split iteration - have %d / %d\n", color_count, (1 << target_depth));
    std::vector<KdNode*> extra_splits;
    get_splittable(&root, &extra_splits);
    printf(" found %ld splittable nodes\n", extra_splits.size());
    if (extra_splits.empty()) {
      break;
    }

    int i = 0;
    while (color_count < (1 << target_depth)) {
      if (i >= extra_splits.size()) {
        break;
      }
      split_kd(extra_splits[i], 1, pick_split_dim_final_splits(extra_splits[i]->colors));
      i++;
      color_count++;
    }

    color_count = total_color_count(&root);
  }

  // Get final colors:
  std::unordered_map<u32, u32> color_value_to_color_idx;
  QuantizedColors result;
  for_each_child(&root, [&](KdNode* node) {
    if (node->colors.empty()) {
      return;
    }

    const u32 slot = result.final_colors.size();
    u32 totals[4] = {0, 0, 0, 0};
    const u32 n = node->colors.size();
    for (auto& color : node->colors) {
      color_value_to_color_idx[color_as_u32(color)] = slot;
      for (int i = 0; i < 4; i++) {
        totals[i] += color[i];
      }
    }
    result.final_colors.emplace_back(saturate_to_u8(totals[0] / n), saturate_to_u8(totals[1] / n),
                                     saturate_to_u8(totals[2] / n),
                                     saturate_to_u8(totals[3] / (2 * n)));
  });

  for (auto& color : in) {
    result.vtx_to_color.push_back(color_value_to_color_idx.at(color_as_u32(color)));
  }

  lg::warn("Quantize colors: {} input colors ({} unique) -> {} output in {:.3f} ms\n", in.size(),
           num_unique_colors, result.final_colors.size(), timer.getMs());

  // debug:
  if (!result.vtx_to_color.empty()) {
    Color worst_in;
    Color worst_out;
    s32 worst_diff = -1;

    for (size_t i = 0; i < result.vtx_to_color.size(); i++) {
      Color input = in.at(i);
      input.w() /= 2;
      const Color output = result.final_colors.at(result.vtx_to_color.at(i));
      const s32 diff = color_difference(input, output);
      if (diff > worst_diff) {
        worst_diff = diff;
        worst_in = input;
        worst_out = output;
      }
    }

    lg::error("Worst diff {} between {} {}", std::sqrt((float)worst_diff),
              worst_in.to_string_hex_byte(), worst_out.to_string_hex_byte());
  }

  return result;
}
