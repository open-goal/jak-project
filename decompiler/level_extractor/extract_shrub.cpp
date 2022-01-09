#include <array>

#include "extract_common.h"
#include "extract_shrub.h"

#include "decompiler/ObjectFile/LinkedObjectFile.h"

#include "common/util/FileUtil.h"

namespace decompiler {

/// <summary>
/// Verify all node indices in a tree.
/// </summary>
/// <param name="tree"></param>
/// <returns>If valid or not</returns>
bool verify_node_indices(const shrub_types::DrawableTreeInstanceShrub* tree) {
  u16 start = get_first_idx(tree->arrays.at(0).get());
  for (auto& array : tree->arrays) {
    if (!verify_node_indices_from_array(array.get(), start, &start)) {
      return false;
    }
    start = (start + 31) & ~(31);  // TODO - is this wrong for shrub (just copy pasted from tie for now)
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
  fmt::print("shrub::extract_vis_data not yet implemeneted!");
}

std::vector<shrub_types::PrototypeArrayShrubInfo> collect_shrub_info(
    const shrub_types::DrawableInlineArrayInstanceShrub* instances,
    const std::vector<shrub_types::PrototypeBucketShrub>* protos) {
  std::vector<shrub_types::PrototypeArrayShrubInfo> result;
  for (auto& instance : instances->instances) {
    // todo left off
    shrub_types::Shrub info;

    info.prototype_idx = instance.bucket_index;
    info.vis_id = instance.id;
    for (int i = 0; i < 4; i++) {
      info.bsphere[i] = instance.bsphere.data[i];
    }
    info.mat = extract_tie_matrix(instance.origin.data);
    info.mat[3][0] += info.bsphere[0];
    info.mat[3][1] += info.bsphere[1];
    info.mat[3][2] += info.bsphere[2];
    info.wind_index = instance.wind_index;
    // there's a value stashed here that we can get rid of
    // it is related to wind.
    info.unknown_wind_related_value = info.mat[0][3];
    info.mat[0][3] = 0.f;

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
    }

    if (result.size() <= info.prototype_idx) {
      result.resize(info.prototype_idx + 1);
    }
    result[info.prototype_idx].instances.push_back(info);
  }

  return result;
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
  auto last_array = tree->arrays.at(6).get();  // shrub is the second last tree in `background-work`
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
  for (size_t node_idx = 0; node_idx < this_tree.bvh.vis_nodes.size(); node_idx++) {
    const auto& node = this_tree.bvh.vis_nodes[node_idx];
    if (node.flags == 0) {
      for (int i = 0; i < node.num_kids; i++) {
        instance_parents[node.child_id + i] = node_idx;
      }
    }
  }

  auto info = collect_instance_info(as_instance_array, &tree->prototypes.prototype_array_tie.data);
  update_proto_info(&info, tex_map, tex_db, tree->prototypes.prototype_array_tie.data);

  // AHHHH - likely stuck

  // debug_print_info(info);
  emulate_tie_prototype_program(info);
  emulate_tie_instance_program(info);
  emulate_kicks(info);

  if (dump_level) {
    auto dir = file_util::get_file_path({fmt::format("debug_out/tie-{}/", debug_name)});
    file_util::create_dir_if_needed(dir);
    for (auto& proto : info) {
      auto data = debug_dump_proto_to_obj(proto);
      file_util::write_text_file(fmt::format("{}/{}.obj", dir, proto.name), data);
      // file_util::create_dir_if_needed()
    }

    auto full = dump_full_to_obj(info);
    file_util::write_text_file(fmt::format("{}/ALL.obj", dir), full);
  }

  auto full_palette = make_big_palette(info);
  add_vertices_and_static_draw(this_tree, out, tex_db, info);

  for (auto& draw : this_tree.static_draws) {
    for (auto& str : draw.vis_groups) {
      auto it = instance_parents.find(str.vis_idx);
      if (it == instance_parents.end()) {
        str.vis_idx = UINT32_MAX;
      } else {
        str.vis_idx = it->second;
      }
    }
  }

  this_tree.colors = full_palette.colors;
  fmt::print("TIE tree has {} draws\n", this_tree.static_draws.size());
  out.tie_trees.push_back(std::move(this_tree));
}
}  // namespace decompiler
