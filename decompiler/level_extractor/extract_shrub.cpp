#include <array>

#include "extract_shrub.h"

#include "decompiler/ObjectFile/LinkedObjectFile.h"

#include "common/util/FileUtil.h"

namespace decompiler {

/// <summary>
/// Get the index of the first draw node in an array. Works for node or tfrag.
/// </summary>
/// <param name="array"></param>
/// <returns></returns>
u16 get_first_idx_shrub(const level_tools::DrawableInlineArray* array) {
  auto as_tie_instances = dynamic_cast<const shrub_types::DrawableInlineArrayInstanceShrub*>(array);
  auto as_nodes = dynamic_cast<const level_tools::DrawableInlineArrayNode*>(array);
  if (as_tie_instances) {
    return as_tie_instances->instances.at(0).id;
  } else if (as_nodes) {
    return as_nodes->draw_nodes.at(0).id;
  } else {
    assert(false);
  }
}

/// <summary>
/// Verify node indices follow the patterns we expect. Takes start as the expected first, writes the
/// end.
/// </summary>
/// <param name="array"></param>
/// <param name="start"></param>
/// <param name="end"></param>
/// <returns></returns>
bool verify_node_indices_from_array_shrub(const level_tools::DrawableInlineArray* array,
                                    u16 start,
                                    u16* end) {
  auto as_shrub_instances =
      dynamic_cast<const shrub_types::DrawableInlineArrayInstanceShrub*>(array);
  auto as_nodes = dynamic_cast<const level_tools::DrawableInlineArrayNode*>(array);

  if (as_shrub_instances) {
    for (auto& elt : as_shrub_instances->instances) {
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

/// <summary>
/// Verify all node indices in a tree.
/// </summary>
/// <param name="tree"></param>
/// <returns>If valid or not</returns>
bool verify_node_indices(const shrub_types::DrawableTreeInstanceShrub* tree) {
  u16 start = get_first_idx_shrub(tree->arrays.at(0).get());
  for (auto& array : tree->arrays) {
    if (!verify_node_indices_from_array_shrub(array.get(), start, &start)) {
      return false;
    }
    start =
        (start + 31) & ~(31);  // TODO - is this wrong for shrub (just copy pasted from tie for now)
  }
  return true;
}

/// <summary>
/// Extract the visibility tree. This does not insert nodes for the bottom level.
/// </summary>
/// <param name="tree"></param>
/// <param name="first_child"></param>
/// <param name="out"></param>
void extract_vis_data(const shrub_types::DrawableTreeInstanceShrub* tree,
                      u16 first_child,
                      tfrag3::ShrubTree& out) {
  out.bvh.first_leaf_node = first_child;
  out.bvh.last_leaf_node = first_child;

  if (tree->arrays.size() == 0) {
    // shouldn't hit this?
  } else if (tree->arrays.size() == 1) {
    auto array =
        dynamic_cast<const level_tools::DrawableInlineArrayInstanceTie*>(tree->arrays.at(0).get());
    assert(array);
    out.bvh.first_root = array->instances.at(0).id;
    out.bvh.num_roots = array->instances.size();
    out.bvh.only_children = true;
  } else {
    auto array =
        dynamic_cast<const level_tools::DrawableInlineArrayNode*>(tree->arrays.at(0).get());
    assert(array);
    out.bvh.first_root = array->draw_nodes.at(0).id;
    out.bvh.num_roots = array->draw_nodes.size();
    out.bvh.only_children = false;
  }

  out.bvh.vis_nodes.resize(first_child - out.bvh.first_root);

  // may run 0 times, if there are only children.
  for (int i = 0; i < ((int)tree->arrays.size()) - 1; i++) {
    bool expecting_leaves = i == ((int)tree->arrays.size()) - 2;

    auto array =
        dynamic_cast<const level_tools::DrawableInlineArrayNode*>(tree->arrays.at(i).get());
    assert(array);
    u16 idx = first_child;
    for (auto& elt : array->draw_nodes) {
      auto& vis = out.bvh.vis_nodes.at(elt.id - out.bvh.first_root);
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

          assert(l->id >= out.bvh.first_leaf_node);
          if (leaf == 0) {
            vis.child_id = l->id;
          }
          out.bvh.last_leaf_node = std::max((u16)l->id, out.bvh.last_leaf_node);
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

          assert(l->id < out.bvh.first_leaf_node);
        }
      }
    }
  }
}

struct ShrubInstanceFragInfo {
  // the color index table uploaded to VU.
  // this contains indices into the shared palette.
  std::vector<u8> color_indices;

  u16 color_index_offset_in_big_palette = -1;

  math::Vector<u32, 4> lq_colors_ui(u32 qw) const {
    // note: this includes the unpack
    assert(qw >= 204);
    qw -= 204;
    qw *= 4;
    assert(qw + 4 <= color_indices.size());
    math::Vector<u32, 4> result;
    for (int i = 0; i < 4; i++) {
      result[i] = color_indices.at(qw + i);
    }
    return result;
  }
};

struct ShrubInstanceInfo {
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

  std::vector<ShrubInstanceFragInfo> frags;  // per-instance per-fragment info
};

std::array<math::Vector4f, 4> extract_shrub_matrix(const u16* data) {
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

  return result;
}

struct ShrubProtoInfo {
  std::string name;
  std::vector<ShrubInstanceInfo> instances;
  bool uses_generic = false;
  float stiffness = 0;
  u32 generic_flag;
  std::vector<tfrag3::TimeOfDayColor> time_of_day_colors;
};

constexpr int GEOM_IDX = 1;  // todo 0 or 1??

std::vector<ShrubProtoInfo> collect_instance_info(
    const shrub_types::DrawableInlineArrayInstanceShrub* instances,
    const std::vector<shrub_types::PrototypeBucketShrub>* protos) {
  std::vector<ShrubProtoInfo> result;
  for (auto& instance : instances->instances) {
    ShrubInstanceInfo info;

    info.prototype_idx = instance.bucket_index;
    info.vis_id = instance.id;
    for (int i = 0; i < 4; i++) {
      info.bsphere[i] = instance.bsphere.data[i];
    }
    info.mat = extract_shrub_matrix(instance.origin.data);
    info.mat[3][0] += info.bsphere[0];
    info.mat[3][1] += info.bsphere[1];
    info.mat[3][2] += info.bsphere[2];
    info.wind_index = instance.wind_index;
    // there's a value stashed here that we can get rid of
    // it is related to wind.
    // TODO - is this the same for shrubs?
    info.unknown_wind_related_value = info.mat[0][3];
    info.mat[0][3] = 0.f;

    // TODO - from a type-level, the `instance-shrubbery` type also seems to store colors in the
    // same way
    //
    // However, there is no `base_qw` field, `prototype-bucket-shrub` is MUCH simpler
    //
    // For shrub, perhaps this is stored in the shrub's header? which is a `shrubbery`?
    /*auto& proto = protos->at(info.prototype_idx);
    u32 offset_bytes = proto.base_qw[GEOM_IDX] * 16;
    for (int frag_idx = 0; frag_idx < proto.frag_count[GEOM_IDX]; frag_idx++) {
      TieInstanceFragInfo frag_info;
      u32 num_color_qwc = proto.color_index_qwc.at(proto.index_start[GEOM_IDX] + frag_idx);
      for (u32 i = 0; i < num_color_qwc * 4; i++) {
        for (u32 j = 0; j < 4; j++) {
          frag_info.color_indices.push_back(
              instance.color_indices.data->words_by_seg.at(instance.color_indices.seg)
                  .at(((offset_bytes + instance.color_indices.byte_offset) / 4) + i)
                  .get_byte(j));
        }
      }
      info.frags.push_back(std::move(frag_info));
      assert(info.frags.back().color_indices.size() > 0);
      offset_bytes += num_color_qwc * 16;
    }*/

    if (result.size() <= info.prototype_idx) {
      result.resize(info.prototype_idx + 1);
    }
    result[info.prototype_idx].instances.push_back(info);
  }

  return result;
}

void update_proto_info(std::vector<ShrubProtoInfo>* out,
                       const std::vector<level_tools::TextureRemap>& map,
                       const TextureDB& tdb,
                       const std::vector<shrub_types::PrototypeBucketShrub>& protos) {
  out->resize(std::max(out->size(), protos.size()));
  for (size_t i = 0; i < protos.size(); i++) {
    const auto& proto = protos[i];
    auto& info = out->at(i);
    assert(proto.flags == 0 || proto.flags == 2);
    info.uses_generic = (proto.flags == 2);
    info.name = proto.name;
    info.stiffness = proto.stiffness;
    info.generic_flag = proto.flags & 2;

    // TODO - shrubbery is much simpler to this, it doens't have fragments (i think)

    // for (int frag_idx = 0; frag_idx < proto.frag_count[GEOM_IDX]; frag_idx++) {
    //  TieFrag frag_info;
    //  for (int tex_idx = 0;
    //       tex_idx < proto.geometry[GEOM_IDX].tie_fragments.at(frag_idx).tex_count / 5; tex_idx++)
    //       {
    //    AdgifInfo adgif;
    //    auto& gif_data = proto.geometry[GEOM_IDX].tie_fragments[frag_idx].gif_data;
    //    u8 ra_tex0 = gif_data.at(16 * (tex_idx * 5 + 0) + 8);
    //    u64 ra_tex0_val;
    //    memcpy(&ra_tex0_val, &gif_data.at(16 * (tex_idx * 5 + 0)), 8);
    //    assert(ra_tex0 == (u8)GsRegisterAddress::TEX0_1);
    //    assert(ra_tex0_val == 0 || ra_tex0_val == 0x800000000);  // note: decal
    //    frag_info.has_magic_tex0_bit = ra_tex0_val == 0x800000000;
    //    memcpy(&adgif.first_w, &gif_data.at(16 * (tex_idx * 5 + 0) + 12), 4);

    //    u8 ra_tex1 = gif_data.at(16 * (tex_idx * 5 + 1) + 8);
    //    u64 ra_tex1_val;
    //    memcpy(&ra_tex1_val, &gif_data.at(16 * (tex_idx * 5 + 1)), 8);
    //    assert(ra_tex1 == (u8)GsRegisterAddress::TEX1_1);
    //    assert(ra_tex1_val == 0x120);  // some flag
    //    u32 original_tex;
    //    memcpy(&original_tex, &gif_data.at(16 * (tex_idx * 5 + 1) + 8), 4);
    //    u32 new_tex = remap_texture(original_tex, map);
    //    if (original_tex != new_tex) {
    //      fmt::print("map from 0x{:x} to 0x{:x}\n", original_tex, new_tex);
    //    }
    //    u32 tpage = new_tex >> 20;
    //    u32 tidx = (new_tex >> 8) & 0b1111'1111'1111;
    //    u32 tex_combo = (((u32)tpage) << 16) | tidx;
    //    auto tex = tdb.textures.find(tex_combo);
    //    assert(tex != tdb.textures.end());
    //    adgif.combo_tex = tex_combo;
    //    memcpy(&adgif.second_w, &gif_data.at(16 * (tex_idx * 5 + 1) + 12), 4);

    //    if (ra_tex0_val == 0x800000000) {
    //      fmt::print("texture {} in {} has weird tex setting\n", tex->second.name, proto.name);
    //    }

    //    u8 ra_mip = gif_data.at(16 * (tex_idx * 5 + 2) + 8);
    //    assert(ra_mip == (u8)GsRegisterAddress::MIPTBP1_1);
    //    memcpy(&adgif.third_w, &gif_data.at(16 * (tex_idx * 5 + 2) + 12), 4);

    //    // who cares about the value

    //    u8 ra_clamp = gif_data.at(16 * (tex_idx * 5 + 3) + 8);
    //    assert(ra_clamp == (u8)GsRegisterAddress::CLAMP_1);
    //    u64 clamp;
    //    memcpy(&clamp, &gif_data.at(16 * (tex_idx * 5 + 3)), 8);
    //    adgif.clamp_val = clamp;

    //    u8 ra_alpha = gif_data.at(16 * (tex_idx * 5 + 4) + 8);
    //    assert(ra_alpha == (u8)GsRegisterAddress::ALPHA_1);
    //    u64 alpha;
    //    memcpy(&alpha, &gif_data.at(16 * (tex_idx * 5 + 4)), 8);
    //    adgif.alpha_val = alpha;
    //    frag_info.adgifs.push_back(adgif);
    //  }
    //  frag_info.expected_dverts = proto.geometry[GEOM_IDX].tie_fragments[frag_idx].num_dverts;
    //  int tex_qwc = proto.geometry[GEOM_IDX].tie_fragments.at(frag_idx).tex_count;
    //  int other_qwc = proto.geometry[GEOM_IDX].tie_fragments.at(frag_idx).gif_count;
    //  frag_info.other_gif_data.resize(16 * other_qwc);
    //  memcpy(frag_info.other_gif_data.data(),
    //         proto.geometry[GEOM_IDX].tie_fragments[frag_idx].gif_data.data() + (16 * tex_qwc),
    //         16 * other_qwc);

    //  const auto& pr = proto.geometry[GEOM_IDX].tie_fragments[frag_idx].point_ref;
    //  int in_qw = pr.size() / 16;
    //  int out_qw = in_qw * 2;
    //  frag_info.points_data.resize(out_qw * 16);
    //  {
    //    const s16* in_ptr = (const s16*)pr.data();
    //    s32* out_ptr = (s32*)frag_info.points_data.data();
    //    for (int ii = 0; ii < out_qw * 4; ii++) {
    //      out_ptr[ii] = in_ptr[ii];
    //    }
    //  }

    //  // just for debug
    //  for (int g = 0; g < 4; g++) {
    //    frag_info.point_sizes.push_back(proto.geometry[g].tie_fragments[frag_idx].point_ref.size());
    //  }

    //  info.frags.push_back(std::move(frag_info));
    //}
  }
}

void extract_shrub(const shrub_types::DrawableTreeInstanceShrub* tree,
                   const std::string& debug_name,
                   const std::vector<level_tools::TextureRemap>& map,
                   const TextureDB& tex_db,
                   const std::vector<std::pair<int, int>>& expected_missing_textures,
                   tfrag3::Level& out,
                   bool dump_level) {
  tfrag3::ShrubTree this_tree;

  // sanity check the vis tree (not a perfect check, but this is used in game and should be right)
  assert(tree->length == (int)tree->arrays.size());
  assert(tree->length > 0);
  auto last_array = tree->arrays.back().get();
  auto as_instance_array = dynamic_cast<shrub_types::DrawableInlineArrayInstanceShrub*>(last_array);
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
  for (size_t node_idx = 0; node_idx < this_tree.bvh.vis_nodes.size(); node_idx++) {
    const auto& node = this_tree.bvh.vis_nodes[node_idx];
    if (node.flags == 0) {
      for (int i = 0; i < node.num_kids; i++) {
        instance_parents[node.child_id + i] = node_idx;
      }
    }
  }

  auto info = collect_instance_info(as_instance_array, &tree->info.prototype_array_shrub.data);
  update_proto_info(&info, map, tex_db, tree->info.prototype_array_shrub.data);

  // AHHHH - likely stuck

  // debug_print_info(info);
  // emulate_tie_prototype_program(info);
  // emulate_tie_instance_program(info);
  // emulate_kicks(info);

  // if (dump_level) {
  //  auto dir = file_util::get_file_path({fmt::format("debug_out/tie-{}/", debug_name)});
  //  file_util::create_dir_if_needed(dir);
  //  for (auto& proto : info) {
  //    auto data = debug_dump_proto_to_obj(proto);
  //    file_util::write_text_file(fmt::format("{}/{}.obj", dir, proto.name), data);
  //    // file_util::create_dir_if_needed()
  //  }

  //  auto full = dump_full_to_obj(info);
  //  file_util::write_text_file(fmt::format("{}/ALL.obj", dir), full);
  //}

  // auto full_palette = make_big_palette(info);
  // add_vertices_and_static_draw(this_tree, out, tex_db, info);

  // for (auto& draw : this_tree.static_draws) {
  //  for (auto& str : draw.vis_groups) {
  //    auto it = instance_parents.find(str.vis_idx);
  //    if (it == instance_parents.end()) {
  //      str.vis_idx = UINT32_MAX;
  //    } else {
  //      str.vis_idx = it->second;
  //    }
  //  }
  //}

  // this_tree.colors = full_palette.colors;
  // fmt::print("TIE tree has {} draws\n", this_tree.static_draws.size());
  // out.tie_trees.push_back(std::move(this_tree));
}
}  // namespace decompiler
