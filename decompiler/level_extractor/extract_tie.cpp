#include "extract_tie.h"

#include "decompiler/ObjectFile/LinkedObjectFile.h"

namespace decompiler {

/*!
 * Get the index of the first draw node in an array. Works for node or tfrag.
 */
u16 get_first_idx(const level_tools::DrawableInlineArray* array) {
  auto as_tie_instances = dynamic_cast<const level_tools::DrawableInlineArrayInstanceTie*>(array);
  auto as_nodes = dynamic_cast<const level_tools::DrawableInlineArrayNode*>(array);
  if (as_tie_instances) {
    return as_tie_instances->instances.at(0).id;
  } else if (as_nodes) {
    return as_nodes->draw_nodes.at(0).id;
  } else {
    assert(false);
  }
}

/*!
 * Verify node indices follow the patterns we expect. Takes start as the expected first,
 * writes the end.
 */
bool verify_node_indices_from_array(const level_tools::DrawableInlineArray* array,
                                    u16 start,
                                    u16* end) {
  auto as_tie_instances = dynamic_cast<const level_tools::DrawableInlineArrayInstanceTie*>(array);
  auto as_nodes = dynamic_cast<const level_tools::DrawableInlineArrayNode*>(array);

  if (as_tie_instances) {
    for (auto& elt : as_tie_instances->instances) {
      if (elt.id != start) {
        fmt::print("bad inst: exp {} got {}\n", start, elt.id);
        return false;
      }
      start++;
    }
    *end = start;
    return true;
  } else if (as_nodes) {
    for (auto& elt : as_nodes->draw_nodes) {
      if (elt.id != start) {
        fmt::print("bad node: exp {} got {}\n", start, elt.id);
        return false;
      }
      start++;
    }
    *end = start;
    return true;
  } else {
    fmt::print("bad node array type: {}\n", array->my_type());
    return false;
  }
}

/*!
 * Verify all node indices in a tree.
 */
bool verify_node_indices(const level_tools::DrawableTreeInstanceTie* tree) {
  u16 start = get_first_idx(tree->arrays.at(0).get());
  for (auto& array : tree->arrays) {
    if (!verify_node_indices_from_array(array.get(), start, &start)) {
      return false;
    }
    start = (start + 31) & ~(31);
  }
  return true;
}

/*!
 * Extract the visibility tree.
 * This does not insert nodes for the bottom level.
 */
void extract_vis_data(const level_tools::DrawableTreeInstanceTie* tree,
                      u16 first_child,
                      tfrag3::TieTree& out) {
  out.first_leaf_node = first_child;
  out.last_leaf_node = first_child;

  if (tree->arrays.size() == 0) {
    // shouldn't hit this?
  } else if (tree->arrays.size() == 1) {
    auto array =
        dynamic_cast<const level_tools::DrawableInlineArrayInstanceTie*>(tree->arrays.at(0).get());
    assert(array);
    out.first_root = array->instances.at(0).id;
    out.num_roots = array->instances.size();
    out.only_children = true;
  } else {
    auto array =
        dynamic_cast<const level_tools::DrawableInlineArrayNode*>(tree->arrays.at(0).get());
    assert(array);
    out.first_root = array->draw_nodes.at(0).id;
    out.num_roots = array->draw_nodes.size();
    out.only_children = false;
  }

  out.vis_nodes.resize(first_child - out.first_root);

  // may run 0 times, if there are only children.
  for (int i = 0; i < ((int)tree->arrays.size()) - 1; i++) {
    bool expecting_leaves = i == ((int)tree->arrays.size()) - 2;

    auto array =
        dynamic_cast<const level_tools::DrawableInlineArrayNode*>(tree->arrays.at(i).get());
    assert(array);
    u16 idx = first_child;
    for (auto& elt : array->draw_nodes) {
      auto& vis = out.vis_nodes.at(elt.id - out.first_root);
      assert(vis.num_kids == 0xff);
      for (int j = 0; j < 4; j++) {
        vis.bsphere[j] = elt.bsphere.data[j];
      }
      vis.num_kids = elt.child_count;
      vis.flags = elt.flags;
      assert(vis.flags == expecting_leaves ? 0 : 1);
      assert(vis.num_kids > 0);
      assert(vis.num_kids <= 8);
      assert(elt.children.size() == vis.num_kids);
      if (expecting_leaves) {
        for (int leaf = 0; leaf < (int)vis.num_kids; leaf++) {
          auto l = dynamic_cast<level_tools::InstanceTie*>(elt.children.at(leaf).get());
          assert(l);

          assert(idx == l->id);

          assert(l->id >= out.first_leaf_node);
          if (leaf == 0) {
            vis.child_id = l->id;
          }
          out.last_leaf_node = std::max((u16)l->id, out.last_leaf_node);
          idx++;
        }

      } else {
        u16 arr_idx = 0;
        for (int child = 0; child < (int)vis.num_kids; child++) {
          auto l = dynamic_cast<level_tools::DrawNode*>(elt.children.at(child).get());
          assert(l);
          if (child == 0) {
            arr_idx = l->id;
          } else {
            assert(arr_idx < l->id);
            arr_idx = l->id;
          }
          if (child == 0) {
            vis.child_id = l->id;
          }

          assert(l->id < out.first_leaf_node);
        }
      }
    }
  }
}

struct TieInstanceFragInfo {
  std::vector<u8> color_indices;
};

struct TieInstanceInfo {
  // The index of the prototype (the geometry) that is used by this instance
  // note: we're going to trust that this lines up with bucket.
  // if this assumption is wrong, we'll be drawing with the wrong model and it will be super
  // obvious.
  u16 prototype_idx = 0;

  // our bsphere's index in the BVH tree
  u16 vis_id = 0;

  // not totally sure if we'll use this (currently unused in tfrag, but probably worth if we
  // actually cull using the tree)
  math::Vector4f bsphere;

  std::array<math::Vector4f, 4> mat;

  u16 wind_index = 0;
  float unknown_wind_related_value = 0.f;  // w of the first mat vec.

  std::vector<TieInstanceFragInfo> frags;  // per-instance per-fragment info
};

struct TieFrag {
  u32 combo_tex;
  u64 alpha_val;
  u64 clamp_val;
  bool has_magic_tex0_bit = false;

  std::vector<u8> other_gif_data;
  std::vector<u8> points_data;
};

struct TieProtoInfo {
  std::string name;
  std::vector<TieInstanceInfo> instances;
  bool uses_generic = false;
  float stiffness = 0;
  std::vector<tfrag3::TimeOfDayColor> time_of_day_colors;
  std::vector<TieFrag> frags;
};

std::array<math::Vector4f, 4> extract_tie_matrix(const u16* data) {
  std::array<math::Vector4f, 4> result;
  for (int i = 0; i < 4; i++) {
    s32 x = data[12 + i];
    x <<= 16;
    x >>= 10;
    result[3][i] = x;
  }

  for (int vec = 0; vec < 3; vec++) {
    for (int i = 0; i < 4; i++) {
      s32 x = data[vec * 4 + i];
      x <<= 16;
      x >>= 16;
      result[vec][i] = (float)x / 4096.f;
    }
  }

  //  if (data[15]) {
  //    fmt::print("--- {}\n", data[15]);
  //  }

  //  for (int vec = 0; vec < 4; vec++) {
  //    fmt::print("{} {}\n", vec, result[vec].to_string_aligned());
  //  }

  return result;
}

constexpr int GEOM_IDX = 1;

std::vector<TieProtoInfo> collect_instance_info(
    const level_tools::DrawableInlineArrayInstanceTie* instances,
    const std::vector<level_tools::PrototypeBucketTie>* protos) {
  std::vector<TieProtoInfo> result;
  for (auto& instance : instances->instances) {
    TieInstanceInfo info;
    info.prototype_idx = instance.bucket_index;
    info.vis_id = instance.id;
    for (int i = 0; i < 4; i++) {
      info.bsphere[i] = instance.bsphere.data[i];
    }
    info.mat = extract_tie_matrix(instance.origin.data);
    info.wind_index = instance.wind_index;
    // there's a value stashed here that we can get rid of
    // it is related to wind.
    info.unknown_wind_related_value = info.mat[0][3];
    info.mat[0][3] = 0.f;

    // todo get colors.
    // each fragment has its own color data (3 dmatags)

    // the number of colors (qwc) is stored in the prototype, in the color-index-qwc array of bytes.
    // at an offset of index-start[geom] + frag_idx.

    // the actual data is located at the instance's color-indices + (proto.base-qw[geom] * 16)

    // and this is only the indices.... there's yet another lookup on the VU
    auto& proto = protos->at(info.prototype_idx);
    u32 offset_bytes = proto.base_qw[GEOM_IDX] * 16;
    for (int frag_idx = 0; frag_idx < proto.frag_count[GEOM_IDX]; frag_idx++) {
      TieInstanceFragInfo frag_info;
      u32 num_color_qwc = proto.color_index_qwc.at(proto.index_start[GEOM_IDX] + frag_idx);
      // fmt::print("frag: {}, qwc: {}, off: {}\n", frag_idx, num_color_qwc, offset_bytes);
      for (u32 i = 0; i < num_color_qwc / 4; i++) {
        for (u32 j = 0; j < 4; j++) {
          frag_info.color_indices.push_back(
              instance.color_indices.data->words_by_seg.at(instance.color_indices.seg)
                  .at(((offset_bytes + instance.color_indices.byte_offset) / 4) + i)
                  .get_byte(j));
        }
      }
      info.frags.push_back(std::move(frag_info));
      offset_bytes += num_color_qwc * 16;
    }

    if (result.size() <= info.prototype_idx) {
      result.resize(info.prototype_idx + 1);
    }
    result[info.prototype_idx].instances.push_back(info);
  }

  return result;
}

u32 remap_texture(u32 original, const std::vector<level_tools::TextureRemap>& map) {
  auto masked = original & 0xffffff00;
  for (auto& t : map) {
    if (t.original_texid == masked) {
      fmt::print("OKAY! remapped!\n");
      assert(false);
      return t.new_texid | 20;
    }
  }
  return original;
}

void update_proto_info(std::vector<TieProtoInfo>* out,
                       const std::vector<level_tools::TextureRemap>& map,
                       const TextureDB& tdb,
                       const std::vector<level_tools::PrototypeBucketTie>& protos) {
  out->resize(std::max(out->size(), protos.size()));
  for (size_t i = 0; i < protos.size(); i++) {
    const auto& proto = protos[i];
    auto& info = out->at(i);
    assert(proto.flags == 0 || proto.flags == 2);
    info.uses_generic = (proto.flags == 2);
    info.name = proto.name;
    info.stiffness = proto.stiffness;

    info.time_of_day_colors.resize(proto.time_of_day.height);
    for (int k = 0; k < (int)proto.time_of_day.height; k++) {
      for (int j = 0; j < 8; j++) {
        memcpy(info.time_of_day_colors[k].rgba[j].data(), &proto.time_of_day.colors[k * 8 + j], 4);
      }
    }

    for (int frag_idx = 0; frag_idx < proto.frag_count[GEOM_IDX]; frag_idx++) {
      TieFrag frag_info;
      for (int tex_idx = 0;
           tex_idx < proto.geometry[GEOM_IDX].tie_fragments.at(frag_idx).tex_count / 5; tex_idx++) {
        auto& gif_data = proto.geometry[GEOM_IDX].tie_fragments[frag_idx].gif_data;
        u8 ra_tex0 = gif_data.at(16 * (tex_idx * 5 + 0) + 8);
        u64 ra_tex0_val;
        memcpy(&ra_tex0_val, &gif_data.at(16 * (tex_idx * 5 + 0)), 8);
        assert(ra_tex0 == (u8)GsRegisterAddress::TEX0_1);
        assert(ra_tex0_val == 0 || ra_tex0_val == 0x800000000);  // todo: what does this mean
        frag_info.has_magic_tex0_bit = ra_tex0_val == 0x800000000;

        u8 ra_tex1 = gif_data.at(16 * (tex_idx * 5 + 1) + 8);
        u64 ra_tex1_val;
        memcpy(&ra_tex1_val, &gif_data.at(16 * (tex_idx * 5 + 1)), 8);
        assert(ra_tex1 == (u8)GsRegisterAddress::TEX1_1);
        assert(ra_tex1_val == 0x120);  // some flag
        u32 original_tex;
        memcpy(&original_tex, &gif_data.at(16 * (tex_idx * 5 + 1) + 8), 4);
        u32 new_tex = remap_texture(original_tex, map);
        if (original_tex != new_tex) {
          fmt::print("map from 0x{:x} to 0x{:x}\n", original_tex, new_tex);
        }
        u32 tpage = new_tex >> 20;
        u32 tidx = (new_tex >> 8) & 0b1111'1111'1111;
        u32 tex_combo = (((u32)tpage) << 16) | tidx;
        auto tex = tdb.textures.find(tex_combo);
        assert(tex != tdb.textures.end());
        frag_info.combo_tex = tex_combo;

        if (ra_tex0_val == 0x800000000) {
          fmt::print("texture {} in {} has weird tex setting\n", tex->second.name, proto.name);
        }

        u8 ra_mip = gif_data.at(16 * (tex_idx * 5 + 2) + 8);
        assert(ra_mip == (u8)GsRegisterAddress::MIPTBP1_1);
        // who cares about the value

        u8 ra_clamp = gif_data.at(16 * (tex_idx * 5 + 3) + 8);
        assert(ra_clamp == (u8)GsRegisterAddress::CLAMP_1);
        u64 clamp;
        memcpy(&clamp, &gif_data.at(16 * (tex_idx * 5 + 3)), 8);
        frag_info.clamp_val = clamp;

        u8 ra_alpha = gif_data.at(16 * (tex_idx * 5 + 4) + 8);
        assert(ra_alpha == (u8)GsRegisterAddress::ALPHA_1);
        u64 alpha;
        memcpy(&alpha, &gif_data.at(16 * (tex_idx * 5 + 4)), 8);
        frag_info.alpha_val = alpha;
      }
      int tex_qwc = proto.geometry[GEOM_IDX].tie_fragments.at(frag_idx).tex_count;
      int other_qwc = proto.geometry[GEOM_IDX].tie_fragments.at(frag_idx).gif_count;
      frag_info.other_gif_data.resize(16 * other_qwc);
      memcpy(frag_info.other_gif_data.data(),
             proto.geometry[GEOM_IDX].tie_fragments[frag_idx].gif_data.data() + (16 * tex_qwc),
             16 * other_qwc);

      // todo other tags

      info.frags.push_back(std::move(frag_info));
    }
  }
}

void debug_print_info(const std::vector<TieProtoInfo>& out) {
  for (auto& proto : out) {
    fmt::print("[{:40}]\n", proto.name);
    fmt::print("  generic: {}\n", proto.uses_generic);
    fmt::print("  use count: {}\n", proto.instances.size());
    fmt::print("  stiffness: {}\n", proto.stiffness);
  }
}

void extract_tie(const level_tools::DrawableTreeInstanceTie* tree,
                 const std::string& debug_name,
                 const std::vector<level_tools::TextureRemap>& tex_map,
                 const TextureDB& tex_db,
                 tfrag3::Level& out) {
  tfrag3::TieTree this_tree;

  // sanity check the vis tree (not a perfect check, but this is used in game and should be right)
  assert(tree->length == (int)tree->arrays.size());
  assert(tree->length > 0);
  auto last_array = tree->arrays.back().get();
  auto as_instance_array = dynamic_cast<level_tools::DrawableInlineArrayInstanceTie*>(last_array);
  assert(as_instance_array);
  assert(as_instance_array->length == (int)as_instance_array->instances.size());
  assert(as_instance_array->length > 0);
  u16 idx = as_instance_array->instances.front().id;
  for (auto& elt : as_instance_array->instances) {
    assert(elt.id == idx);
    idx++;
  }
  bool ok = verify_node_indices(tree);
  assert(ok);
  fmt::print("    tree has {} arrays and {} instances\n", tree->length, as_instance_array->length);

  // extract the vis tree. Note that this extracts the tree only down to the last draw node, a
  // parent of between 1 and 8 instances.
  extract_vis_data(tree, as_instance_array->instances.front().id, this_tree);

  // map of instance ID to its parent. We'll need this later.
  std::unordered_map<int, int> instance_parents;
  for (size_t node_idx = 0; node_idx < this_tree.vis_nodes.size(); node_idx++) {
    const auto& node = this_tree.vis_nodes[node_idx];
    if (node.flags == 0) {
      for (int i = 0; i < node.num_kids; i++) {
        instance_parents[node.child_id + i] = node_idx;
      }
    }
  }

  auto info = collect_instance_info(as_instance_array, &tree->prototypes.prototype_array_tie.data);
  update_proto_info(&info, tex_map, tex_db, tree->prototypes.prototype_array_tie.data);
  debug_print_info(info);

  // todo handle prototypes
  // todo handle vu1

  // todo time of day
  // todo tree parents
}
}  // namespace decompiler