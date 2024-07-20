#include "merc_replacement.h"

using namespace gltf_util;

namespace decompiler {
void extract(const std::string& name,
             MercExtractData& out,
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
    if (node.extras.Has("set_invisible") && node.extras.Get("set_invisible").Get<int>()) {
      continue;
    }
    if (node.mesh >= 0) {
      const auto& mesh = model.meshes[node.mesh];
      mesh_count++;
      for (const auto& prim : mesh.primitives) {
        prim_count++;
        // extract index buffer
        std::vector<u32> prim_indices = gltf_util::gltf_index_buffer(
            model, prim.indices, out.new_vertices.size() + vertex_offset);
        ASSERT_MSG(prim.mode == TINYGLTF_MODE_TRIANGLES, "Unsupported triangle mode");
        // extract vertices
        auto verts =
            gltf_util::gltf_vertices(model, prim.attributes, n.w_T_node, true, true, mesh.name);
        out.new_vertices.insert(out.new_vertices.end(), verts.vtx.begin(), verts.vtx.end());
        out.new_colors.insert(out.new_colors.end(), verts.vtx_colors.begin(),
                              verts.vtx_colors.end());
        out.normals.insert(out.normals.end(), verts.normals.begin(), verts.normals.end());
        ASSERT(out.new_colors.size() == out.new_vertices.size());

        // TODO: just putting it all in one material
        auto& draw = draw_by_material[prim.material];
        draw.mode = gltf_util::make_default_draw_mode();  // todo rm
        draw.tree_tex_id = 0;                             // todo rm
        draw.num_triangles += prim_indices.size() / 3;
        draw.no_strip = true;
        draw.index_count = prim_indices.size();
        draw.first_index = index_offset + out.new_indices.size();

        out.new_indices.insert(out.new_indices.end(), prim_indices.begin(), prim_indices.end());
      }
    }
  }

  tfrag3::MercEffect e;
  out.new_model.name = name;
  out.new_model.max_bones = 120;
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
  e.has_mod_draw = false;
  out.new_model.effects.push_back(e);
  out.new_model.effects.push_back(e);
  out.new_model.effects.push_back(e);
  out.new_model.effects.push_back(e);

  lg::info("Merged {} meshes and {} prims into {} vertices", mesh_count, prim_count,
           out.new_vertices.size());
}

const tfrag3::MercVertex& find_closest(const std::vector<tfrag3::MercVertex>& old,
                                       float x,
                                       float y,
                                       float z) {
  float best_dist = 1e10;
  int best_idx = 0;

  for (int i = 0; i < old.size(); i++) {
    auto& v = old[i];
    float dx = v.pos[0] - x;
    float dy = v.pos[1] - y;
    float dz = v.pos[2] - z;
    float dist = (dx * dx) + (dy * dy) + (dz * dz);
    if (dist < best_dist) {
      best_dist = dist;
      best_idx = i;
    }
  }

  return old[best_idx];
}

void merc_convert_replacement(MercSwapData& out,
                              const MercExtractData& in,
                              const std::vector<tfrag3::MercVertex>& old_verts) {
  out.new_model = in.new_model;
  out.new_indices = in.new_indices;
  out.new_textures = in.tex_pool.textures_by_idx;

  // convert vertices
  for (size_t i = 0; i < in.new_vertices.size(); i++) {
    const auto& y = in.new_vertices[i];
    const auto& copy_from = find_closest(old_verts, y.x, y.y, y.z);
    auto& x = out.new_vertices.emplace_back();
    x.pos[0] = y.x;
    x.pos[1] = y.y;
    x.pos[2] = y.z;
    x.normal[0] = copy_from.normal[0];
    x.normal[1] = copy_from.normal[1];
    x.normal[2] = copy_from.normal[2];
    x.weights[0] = copy_from.weights[0];
    x.weights[1] = copy_from.weights[1];
    x.weights[2] = copy_from.weights[2];
    x.st[0] = y.s;
    x.st[1] = y.t;
    x.rgba[0] = in.new_colors[i][0];
    x.rgba[1] = in.new_colors[i][1];
    x.rgba[2] = in.new_colors[i][2];
    x.rgba[3] = in.new_colors[i][3];
    x.mats[0] = copy_from.mats[0];
    x.mats[1] = copy_from.mats[1];
    x.mats[2] = copy_from.mats[2];
  }
}

void merc_convert_custom(MercSwapData& out, const MercExtractData& in) {
  out.new_model = in.new_model;
  out.new_indices = in.new_indices;
  out.new_textures = in.tex_pool.textures_by_idx;

  // convert vertices
  for (size_t i = 0; i < in.new_vertices.size(); i++) {
    const auto& y = in.new_vertices[i];
    auto& x = out.new_vertices.emplace_back();
    x.pos[0] = y.x;
    x.pos[1] = y.y;
    x.pos[2] = y.z;
    x.normal[0] = in.normals.at(i).x();
    x.normal[1] = in.normals.at(i).y();
    x.normal[2] = in.normals.at(i).z();
    x.weights[0] = 1.0f;
    x.weights[1] = 0.0f;
    x.weights[2] = 0.0f;
    x.st[0] = y.s;
    x.st[1] = y.t;
    x.rgba[0] = in.new_colors[i][0];
    x.rgba[1] = in.new_colors[i][1];
    x.rgba[2] = in.new_colors[i][2];
    x.rgba[3] = in.new_colors[i][3];
    x.mats[0] = 3;
    x.mats[1] = 0;
    x.mats[2] = 0;
  }
}

MercSwapData load_replacement_merc_model(const std::string& name,
                                         u32 current_idx_count,
                                         u32 current_vtx_count,
                                         u32 current_tex_count,
                                         const std::string& path,
                                         const std::vector<tfrag3::MercVertex>& old_verts,
                                         bool custom_mdl) {
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
  extract(name, extract_data, model, all_nodes, current_idx_count, current_vtx_count,
          current_tex_count);
  if (custom_mdl) {
    merc_convert_custom(result, extract_data);
  } else {
    merc_convert_replacement(result, extract_data, old_verts);
  }

  return result;
}
}  // namespace decompiler