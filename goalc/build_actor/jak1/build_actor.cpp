#include "build_actor.h"

#include "common/log/log.h"
#include "common/util/gltf_util.h"

#include "third-party/tiny_gltf/tiny_gltf.h"

using namespace gltf_util;
namespace jak1 {

struct MercExtractData {
  TexturePool tex_pool;
  std::vector<u32> new_indices;
  std::vector<tfrag3::PreloadedVertex> new_vertices;
  std::vector<math::Vector<u8, 4>> new_colors;

  tfrag3::MercModel new_model;
};

void extract(MercExtractData& out,
             const tinygltf::Model& model,
             const std::vector<NodeWithTransform>& all_nodes,
             u32 index_offset,
             u32 vertex_offset,
             u32 tex_offset) {
  ASSERT(out.new_vertices.empty());
  std::map<int, tfrag3::MercDraw> draw_by_material;
  int mesh_count = 0;
  int prim_count = 0;

  for (const auto& n : all_nodes) {
    const auto& node = model.nodes[n.node_idx];
    if (node.mesh >= 0) {
      const auto& mesh = model.meshes[node.mesh];
      mesh_count++;
      for (const auto& prim : mesh.primitives) {
        prim_count++;
        // extract index buffer
        std::vector<u32> prim_indices =
            gltf_index_buffer(model, prim.indices, out.new_vertices.size() + vertex_offset);
        ASSERT_MSG(prim.mode == TINYGLTF_MODE_TRIANGLES, "Unsupported triangle mode");
        // extract vertices
        auto verts = gltf_vertices(model, prim.attributes, n.w_T_node, true, true, mesh.name);
        out.new_vertices.insert(out.new_vertices.end(), verts.vtx.begin(), verts.vtx.end());
        out.new_colors.insert(out.new_colors.end(), verts.vtx_colors.begin(),
                              verts.vtx_colors.end());
        ASSERT(out.new_colors.size() == out.new_vertices.size());

        // TODO: just putting it all in one material
        auto& draw = draw_by_material[prim.material];
        draw.mode = make_default_draw_mode();  // todo rm
        draw.tree_tex_id = 0;                  // todo rm
        draw.num_triangles += prim_indices.size() / 3;
        //        if (draw.vis_groups.empty()) {
        //          auto& grp = draw.vis_groups.emplace_back();
        //          grp.num_inds += prim_indices.size();
        //          grp.num_tris += draw.num_triangles;
        //          grp.vis_idx_in_pc_bvh = UINT32_MAX;
        //        } else {
        //          auto& grp = draw.vis_groups.back();
        //          grp.num_inds += prim_indices.size();
        //          grp.num_tris += draw.num_triangles;
        //          grp.vis_idx_in_pc_bvh = UINT32_MAX;
        //        }
        draw.index_count = prim_indices.size();
        draw.first_index = index_offset + out.new_indices.size();

        out.new_indices.insert(out.new_indices.end(), prim_indices.begin(), prim_indices.end());
      }
    }
  }

  tfrag3::MercEffect e;
  out.new_model.max_bones = 120;  // idk
  out.new_model.max_draws = 200;
  for (const auto& [mat_idx, d_] : draw_by_material) {
    e.all_draws.push_back(d_);
    auto& draw = e.all_draws.back();
    draw.mode = make_default_draw_mode();

    if (mat_idx == -1) {
      lg::warn("Draw had a material index of -1, using default texture.");
      draw.tree_tex_id = 0;
      continue;
    }
    const auto& mat = model.materials[mat_idx];
    int tex_idx = mat.pbrMetallicRoughness.baseColorTexture.index;
    if (tex_idx == -1) {
      lg::warn("Material {} has no texture, using default texture.", mat.name);
      draw.tree_tex_id = 0;
      continue;
    }

    const auto& tex = model.textures[tex_idx];
    ASSERT(tex.sampler >= 0);
    ASSERT(tex.source >= 0);
    draw.mode = draw_mode_from_sampler(model.samplers.at(tex.sampler));

    const auto& img = model.images[tex.source];
    draw.tree_tex_id = tex_offset + texture_pool_add_texture(&out.tex_pool, img);
  }
  lg::info("total of {} unique materials", e.all_draws.size());
  out.new_model.effects.push_back(e);
  out.new_model.effects.push_back(e);
  out.new_model.effects.push_back(e);
  out.new_model.effects.push_back(e);

  lg::info("Merged {} meshes and {} prims into {} vertices", mesh_count, prim_count,
           out.new_vertices.size());
}

MercSwapData load_replacement_merc_model(u32 current_idx_count,
                                         u32 current_vtx_count,
                                         u32 current_tex_count,
                                         const std::string& path,
                                         const std::vector<tfrag3::MercVertex>& old_verts) {
  MercSwapData result;
  lg::info("Reading gltf mesh: {}", path);
  tinygltf::TinyGLTF loader;
  tinygltf::Model model;
  std::string err, warn;
  bool res = loader.LoadBinaryFromFile(&model, &err, &warn, path);
  ASSERT_MSG(warn.empty(), warn.c_str());
  ASSERT_MSG(err.empty(), err.c_str());
  ASSERT_MSG(res, "Failed to load GLTF file!");
  auto all_nodes = flatten_nodes_from_all_scenes(model);

  MercExtractData extract_data;
  extract(extract_data, model, all_nodes, current_idx_count, current_vtx_count, current_tex_count);
  // merc_convert(result, extract_data, old_verts);

  return result;
}

void run_build_actor(const std::string& input_model,
                     const std::string& ag_out,
                     const std::string& output_prefix) {}
}  // namespace jak1