#include "merc_replacement.h"

using namespace gltf_util;

namespace decompiler {

void extract(tfrag3::MercModel& mdl,
             MercExtractData& out,
             const tinygltf::Model& model,
             const std::vector<NodeWithTransform>& all_nodes,
             u32 index_offset,
             u32 vertex_offset,
             u32 tex_offset,
             bool& has_custom_weights) {
  ASSERT(out.new_vertices.empty());

  std::map<int, tfrag3::MercDraw> draw_by_material;
  int mesh_count = 0;
  int prim_count = 0;
  int joints = 3;
  auto skin_idx = find_single_skin(model, all_nodes);
  if (skin_idx) {
    joints += get_joint_count(model, *skin_idx);
  }
  bool copy_eye_draws = false;
  bool copy_mod_draws = false;

  for (const auto& n : all_nodes) {
    const auto& node = model.nodes[n.node_idx];
    if (node.extras.Has("set_invisible") && node.extras.Get("set_invisible").Get<int>()) {
      continue;
    }
    if (node.mesh >= 0) {
      const auto& mesh = model.meshes[node.mesh];
      mesh_count++;
      has_custom_weights = node.extras.Has("enable_custom_weights") &&
                           node.extras.Get("enable_custom_weights").Get<int>();
      copy_eye_draws =
          node.extras.Has("copy_eye_draws") && node.extras.Get("copy_eye_draws").Get<int>();
      copy_mod_draws =
          node.extras.Has("copy_mod_draws") && node.extras.Get("copy_mod_draws").Get<int>();
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

        if (prim.attributes.count("JOINTS_0") && prim.attributes.count("WEIGHTS_0")) {
          auto joints_and_weights = gltf_util::extract_and_flatten_joints_and_weights(model, prim);
          ASSERT(joints_and_weights.size() == verts.vtx.size());
          out.joints_and_weights.insert(out.joints_and_weights.end(), joints_and_weights.begin(),
                                        joints_and_weights.end());
        } else {
          // add fake data for vertices without this data
          gltf_util::JointsAndWeights dummy;
          dummy.joints[0] = 3;
          dummy.weights[0] = 1.f;
          for (size_t i = 0; i < out.new_vertices.size(); i++) {
            out.joints_and_weights.push_back(dummy);
          }
        }

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
  tfrag3::MercEffect envmap_eff;
  envmap_eff.has_envmap = false;
  out.new_model.name = mdl.name;
  // if we have a skeleton, use that joint count, otherwise use a high default value since the model
  // we replace can have more
  out.new_model.max_bones = joints != 3 ? joints : 100;
  out.new_model.max_draws = 0;

  for (const auto& [mat_idx, d_] : draw_by_material) {
    const auto& mat = model.materials[mat_idx];
    if (mat_idx < 0 || !gltf_util::material_has_envmap(model.materials[mat_idx]) ||
        !gltf_util::envmap_is_valid(model.materials[mat_idx])) {
      gltf_util::process_normal_merc_draw(model, out, tex_offset, e, mat_idx, d_);
    } else {
      envmap_eff.has_envmap = true;
      gltf_util::process_envmap_merc_draw(model, out, tex_offset, envmap_eff, mat_idx, d_);
    }
  }

  // in case a model only has envmap draws, we don't push the normal merc effect
  if (!e.all_draws.empty()) {
    out.new_model.effects.push_back(e);
  }
  if (envmap_eff.has_envmap) {
    out.new_model.effects.push_back(envmap_eff);
  }

  // copy any effects from the old model that used mod or eye draws
  if (copy_eye_draws || copy_mod_draws) {
    for (auto& old_eff : mdl.effects) {
      tfrag3::MercEffect eff;
      bool has_eye_draw = false;
      if (copy_eye_draws) {
        for (auto& draw : old_eff.all_draws) {
          if (draw.eye_id != 0xff) {
            has_eye_draw = true;
            eff.all_draws.push_back(draw);
          }
        }
      }
      if (copy_mod_draws && old_eff.has_mod_draw) {
        eff.has_mod_draw = true;
        eff.mod = old_eff.mod;
        eff.mod.fix_draw.clear();
      }
      if ((copy_eye_draws || copy_mod_draws) && (eff.has_mod_draw || has_eye_draw)) {
        lg::info("adding old effect for {} (mod draw {}, eye draw {})", mdl.name,
                 old_eff.has_mod_draw, has_eye_draw);
        out.new_model.effects.push_back(eff);
      }
    }
  }

  for (auto& effect : out.new_model.effects) {
    out.new_model.max_draws += effect.all_draws.size();
  }

  lg::info("total of {} unique materials ({} normal, {} envmap)", out.new_model.max_draws,
           e.all_draws.size(), envmap_eff.all_draws.size());
  lg::info("Merged {} meshes and {} prims into {} vertices", mesh_count, prim_count,
           out.new_vertices.size());
}

void extract(const std::string& name,
             MercExtractData& out,
             const tinygltf::Model& model,
             const std::vector<NodeWithTransform>& all_nodes,
             u32 index_offset,
             u32 vertex_offset,
             u32 tex_offset,
             bool& has_custom_weights) {
  ASSERT(out.new_vertices.empty());

  std::map<int, tfrag3::MercDraw> draw_by_material;
  int mesh_count = 0;
  int prim_count = 0;
  int joints = 3;
  auto skin_idx = find_single_skin(model, all_nodes);
  if (skin_idx) {
    joints += get_joint_count(model, *skin_idx);
  }

  for (const auto& n : all_nodes) {
    const auto& node = model.nodes[n.node_idx];
    if (node.extras.Has("set_invisible") && node.extras.Get("set_invisible").Get<int>()) {
      continue;
    }
    if (node.mesh >= 0) {
      const auto& mesh = model.meshes[node.mesh];
      mesh_count++;
      has_custom_weights = node.extras.Has("enable_custom_weights") &&
                           node.extras.Get("enable_custom_weights").Get<int>();
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

        if (prim.attributes.count("JOINTS_0") && prim.attributes.count("WEIGHTS_0")) {
          auto joints_and_weights = gltf_util::extract_and_flatten_joints_and_weights(model, prim);
          ASSERT(joints_and_weights.size() == verts.vtx.size());
          out.joints_and_weights.insert(out.joints_and_weights.end(), joints_and_weights.begin(),
                                        joints_and_weights.end());
        } else {
          // add fake data for vertices without this data
          gltf_util::JointsAndWeights dummy;
          dummy.joints[0] = 3;
          dummy.weights[0] = 1.f;
          for (size_t i = 0; i < out.new_vertices.size(); i++) {
            out.joints_and_weights.push_back(dummy);
          }
        }

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
  tfrag3::MercEffect envmap_eff;
  envmap_eff.has_envmap = false;
  out.new_model.name = name;
  // if we have a skeleton, use that joint count, otherwise use a high default value since the model
  // we replace can have more
  out.new_model.max_bones = joints != 3 ? joints : 100;
  out.new_model.max_draws = 0;

  for (const auto& [mat_idx, d_] : draw_by_material) {
    const auto& mat = model.materials[mat_idx];
    if (mat_idx < 0 || !gltf_util::material_has_envmap(model.materials[mat_idx]) ||
        !gltf_util::envmap_is_valid(model.materials[mat_idx])) {
      gltf_util::process_normal_merc_draw(model, out, tex_offset, e, mat_idx, d_);
    } else {
      envmap_eff.has_envmap = true;
      gltf_util::process_envmap_merc_draw(model, out, tex_offset, envmap_eff, mat_idx, d_);
    }
  }

  // in case a model only has envmap draws, we don't push the normal merc effect
  if (!e.all_draws.empty()) {
    out.new_model.effects.push_back(e);
  }
  if (envmap_eff.has_envmap) {
    out.new_model.effects.push_back(envmap_eff);
  }

  for (auto& effect : out.new_model.effects) {
    out.new_model.max_draws += effect.all_draws.size();
  }

  lg::info("total of {} unique materials ({} normal, {} envmap)", out.new_model.max_draws,
           e.all_draws.size(), envmap_eff.all_draws.size());
  lg::info("Merged {} meshes and {} prims into {} vertices", mesh_count, prim_count,
           out.new_vertices.size());
}

const tfrag3::MercVertex& find_closest(const std::vector<tfrag3::MercVertex>& old,
                                       float x,
                                       float y,
                                       float z) {
  float best_dist = 1e10;
  size_t best_idx = 0;

  for (size_t i = 0; i < old.size(); i++) {
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
                              const std::vector<tfrag3::MercVertex>& old_verts,
                              bool use_custom_weights) {
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
    x.normal[0] = in.normals.at(i).x();
    x.normal[1] = in.normals.at(i).y();
    x.normal[2] = in.normals.at(i).z();
    if (use_custom_weights) {
      x.weights[0] = in.joints_and_weights.at(i).weights[0];
      x.weights[1] = in.joints_and_weights.at(i).weights[1];
      x.weights[2] = in.joints_and_weights.at(i).weights[2];
      x.mats[0] = in.joints_and_weights.at(i).joints[0];
      x.mats[1] = in.joints_and_weights.at(i).joints[1];
      x.mats[2] = in.joints_and_weights.at(i).joints[2];
    } else {
      x.weights[0] = copy_from.weights[0];
      x.weights[1] = copy_from.weights[1];
      x.weights[2] = copy_from.weights[2];
      x.mats[0] = copy_from.mats[0];
      x.mats[1] = copy_from.mats[1];
      x.mats[2] = copy_from.mats[2];
    }
    x.st[0] = y.s;
    x.st[1] = y.t;
    x.rgba[0] = in.new_colors[i][0];
    x.rgba[1] = in.new_colors[i][1];
    x.rgba[2] = in.new_colors[i][2];
    x.rgba[3] = in.new_colors[i][3];
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
    x.weights[0] = in.joints_and_weights.at(i).weights[0];
    x.weights[1] = in.joints_and_weights.at(i).weights[1];
    x.weights[2] = in.joints_and_weights.at(i).weights[2];
    x.st[0] = y.s;
    x.st[1] = y.t;
    x.rgba[0] = in.new_colors[i][0];
    x.rgba[1] = in.new_colors[i][1];
    x.rgba[2] = in.new_colors[i][2];
    x.rgba[3] = in.new_colors[i][3];
    x.mats[0] = in.joints_and_weights.at(i).joints[0];
    x.mats[1] = in.joints_and_weights.at(i).joints[1];
    x.mats[2] = in.joints_and_weights.at(i).joints[2];
  }
}

MercSwapData load_replacement_merc_model(tfrag3::MercModel& mdl,
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
  auto has_custom_weights = false;
  extract(mdl, extract_data, model, all_nodes, current_idx_count, current_vtx_count,
          current_tex_count, has_custom_weights);
  if (custom_mdl) {
    merc_convert_custom(result, extract_data);
  } else {
    merc_convert_replacement(result, extract_data, old_verts, has_custom_weights);
  }

  return result;
}

MercSwapData load_custom_merc_model(const std::string& name,
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
  auto has_custom_weights = false;
  extract(name, extract_data, model, all_nodes, current_idx_count, current_vtx_count,
          current_tex_count, has_custom_weights);
  if (custom_mdl) {
    merc_convert_custom(result, extract_data);
  } else {
    merc_convert_replacement(result, extract_data, old_verts, has_custom_weights);
  }

  return result;
}
}  // namespace decompiler